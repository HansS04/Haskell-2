--1. Úloha 
data Attribute = Attribute {name :: String, value :: String} deriving (Show)
-- definujeme data type Attribute s jednym constructorom Attribute
-- atribute ma teda dva fieldy name ktory je definovany ako string a value ktora je definovana takisto ako string 
-- deriving ( Show ) umoznuje aby inštanciu Attribute aby bola printnuta pri vyuziti show napr ku nijakym debuging purposes
data Tag = Tag {tagName :: String, attributes :: [Attribute], tags :: [Tag]} deriving (Show)
-- definujeme datovy typ tag so jednym konštruktorom Tag 
-- Tag ma 3 fieldny tagname ( definovany ako string ), attributes ( list atributov ) tags ( list vnurenych / nested Tagov)
-- deriving show ( umoznuje nam inštanciu Tagu printovať s pouzitim show funkcie)
data HTMLDocument = HTMLDocument {documentTags :: [Tag]} deriving (Show)
-- opätovne definicia data typu s jednym konštruktorom HTMLDocument
-- HTMLDocument ma jeden fiield documentTags ( list tagov) 
-- opätovne pre deriving(show) umotnuje HTMLDocument vyprintovať na debuging purposes
test :: HTMLDocument
test = HTMLDocument [Tag "name" [Attribute "name" "haha"] []]
-- vytvarame inštanciu HTMLDocument s menom test 
-- HTMLDocument obsahuje list s jednym tagom 
-- Tag ma tagName name teda list Atributov s menom - "name" a a valuom "haha" - a empty list tagu 

-- definica potrebnych štruktur pre realizaciu druhej a tretej ulohy 
data Component
  = TextBox {textBoxName :: String, text :: String}
  | Button {buttonName :: String, buttonValue :: String}
  | Container {containerName :: String, children :: [Component]}
  deriving (Show)

--Component datovy typ definovany tromi konštruktormy TextBox Button Container
-- textbox ma dva fieldy textboxname ktory je defli ako string a text ktory je opatovne defli ako string
-- button  ma opätovne dva fieldy name ktory definujeme ako string a value ktory je opatovne defli ako string 
-- container  ma dva fieldy containerName a children teda list componentov 
-- deriving show uz opat sluzi len na na allownutie componentu na vyprintovani eza pouzitia show funkcie 

gui :: Component
gui =
  Container "My App"
    [ Container "Menu" [Button "btn_new" "New", Button "btn_open" "Open", Button "btn_close" "Close"],
      Container "Body" [TextBox "textbox1" "Some text goes here"],
      Container "Footer" []
    ]
-- gui je inštancia componentu reprezentujuca naše guiko 
-- container nesuci meno My app s tromi child komponentami 
-- container s menom Menu obsahuju tri button komponenty  "New," "Open," and "Close."
-- container body obsahujuce dva textbox komponenty extbox1" a text "dnes je štvrtok"
-- container s menom footer s ziadnym child komponentom inak povedane jednoducho empty list 

-- 2 úloha 
-- listButtonNames hladiac na hlavičku funkcie sa jedna o funkciu ktora berie component ako input a vracia list stringov 
-- mozeme sledovať klasicke vyuzitie pattern matchingu na zralitovanie troch casov zalozene na type inputu componentu
listButtonNames :: Component -> [String]
listButtonNames (Button name _) = [name]
-- pokial je input button compoment, extraktujeme name field a vraciame "singleton" list obsahujuci meno 
listButtonNames (TextBox _ _) = []
-- pokial je list texbox komponent vraciame empty list to nam teda indikuje te TextBox komponenty nie su brane v uvahu pri prechadzani button namov
listButtonNames (Container _ children) = concatMap listButtonNames children
-- pokial je input container komponent, rekurzivne applajne listButtonNames na kazdy child komponent s pouzitim concatMap
-- vysledkom je teda list listov 
-- načo pouzivame concatList ? pouzivame na zlučenie dvoch listov do jedneho ktory kombinuje nazvy všetkych buttnov z child komponentov 

-- priklady inputu 
-- listButtonNames (Button "btn1" "Click me")
-- listButtonNames (TextBox "text1" "Some text")
-- listButtonNames (Container "container1" [Button "btn2" "Press", TextBox "text2" "More text"])
-- listButtonNames (Container "app" [Button "btn3" "OK", Container "nested" [Button "btn4" "Cancel"]])

-- 3 úloha  
-- pokial sledujeme hlavičku našej funckie vieme ze 
-- funkcia berie component tzv target sring a string ktory bude nahradeny za target string a vracia nam modifnuty component 
changeText :: Component -> String -> String -> Component
changeText (TextBox name t) str1 str2 -- case 1 -- checkne ci inpuit je TextBox componet checkuje ci name textboxu matchuje target string 
-- ak tu je match tak create novy textbox s menom name ale s novym textom sttr2 
  | name == str1 = TextBox name str2
  | otherwise = TextBox name t
changeText (Container name children) str1 str2 = Container name [changeText c str1 str2 | c <- children]
-- naš case 2 pokial je input container component rekurzivne applajne changeText pre kazdy child komponent za pouzitia krasneho list komprhenču 
-- children of the container su modifajnute changeText funkciou 
changeText x _ _ = x -- naš default case 
-- a naš case pokial niput nie je textbox ani container returneme originalny komponent ! nezmeneny ! 
-- defaultny case nam zabezeči ze ine komponenty ako textbox a container nie su zmenen