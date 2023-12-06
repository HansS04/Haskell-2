data Element = Button' {name'::String, text'::String} |Text {name'::String,text'::String} | Panel [Element] deriving (Show)
-- toto definuje datovy typ Element s tromi konštruktormi button text a panel
-- button a text reprezentuju individualne elementy buttom a text a kazdy ma dva fieldy name - string a text - sttring 

prikladelement :: Element
prikladelement = Panel [(Text "Uvod" "Toto je uvod"),(Button' "Odist" "zo stranky"),(Panel [(Text "text sem" "text tu")])]
-- jedna sa o inštanciu Elementu typ reprezentujuci hierarchicku štrukturu 
-- je to Panel obsahujuci tri komponenty Text element Button element a Panel s vnorenym Text elementom 

--priklad inputu pre 1. ulohu 
--Panel [(Text "Uvod" "Toto je uvod"), (Button' "Odist" "zo stranky"), (Panel [(Text "text sem" "text tu")])]

data Component = 
    TextBox {name::String,text::String}
    |Button {name::String, value::String}
    |Container {name::String, children ::[Component]} deriving(Show)
-- deklarujeme data type component 
-- data type komponent ma tri konštruktory textBox button a container 
-- textbox ma dva fieldy name - string a text - string 
-- button konštruktor ma takisto dva fields name - string a value čo je takisto streing 
-- conatiner constructor ma dva fieldy name - string a children co je len list componentov 
-- deriving ( show ) -- umoznuje nam inštanciu component printnut za pouzitia show funkcie - na debug učely 

gui::Component
gui = Container "My App" [
        Container "Menu" [
            Button "btn_new" "New",
            Button "btn_open" "Open",
            Button "btn_close" "Close"],
        
        Container "Body" [TextBox "textbox_1" "Some text goes here"],
        Container "Footer" []] 
-- top level container - My app
-- menu container - first child z nasho top level containeru je dalši container s menom Menu 
-- obsahuje tri button komponenty New," "Open," and "Close."
-- 2 child komponent je container s menom body 
-- v body containeri je single textbox 1 a text Some text goes here."
--3 child je container s menom footer nema child komponent inak povedane empty list 

-- uloha 2 

-- priklady inputu 
-- listAllButtons (Button "btn1" "Click me")
--listAllButtons (TextBox "text1" "Some text")
--listAllButtons (Container "container1" [Button "btn2" "Press", TextBox "text2" "More text"])
--listAllButtons (Container "app" [Button "btn3" "OK", Container "nested" [Button "btn4" "Cancel"]])

listAllButtons ::Component -> [Component] -- jedna sa o funckiu ktora berie component ako input a vracia list komponentov 
listAllButtons (Button name value) = [(Button name value)]
listAllButtons (TextBox name value) = []
listAllButtons (Container _ children) = concatMap (listAllButtons) (children)

-- case 1 pokial je input button componet vrati singleton list obsahujuci ten button
-- case 2 pokial je input textBox komponent return empty list pretoze textbox kompontn nepovazujeme za buttons 
-- case 3 pokial je input container rekurzivne applajne listAllButtons pre kazdy child komponent za pouzitia concatMap
-- načo je concatMap na to aby skombinovala n počet listov do jedneho 

isButton::Component -> Bool  -- header = takuje component ako input a returne boolean value ci sa teda jedna o button 
isButton (Button _ _) = True -- ak je input button ( nezalezi na na nejme ani value) vrati true
-- klasicke vyuzitie anonymus znaku kedy nam nezalezi co je na tej pozicii
isButton _ = False -- pokial input nie je button ( hocijaky iny type componentu ) funkcia returne false 
-- klasicke vyuzitie anonymus znaku kedy nam nezalezi co je na tej pozicii

-- priklad inputu 
--removeAllButtons gui

removeAllButtons :: Component -> Component -- funkcia ktora takne component a vrati modifnuty komponent 
removeAllButtons (Button name value) = Button name value
removeAllButtons (TextBox name value) = TextBox name value
removeAllButtons (Container name children) = Container name (filter (\x->not(isButton x)) (map (removeAllButtons) children))
-- tri case 
-- case 1 pokial je input button jednoducho vrati rovnaky button ostava nezmeneny 
-- case 2 pokial je input textbox component takisto vrati rovnaky rovnaky textbox, komponent je nezmeneny 
-- pokial je input container rekurzivne applajne removeAllButtons pre kazdy child komponent za pouzitia map
-- filter filtruje komponenty ktore su button za pouzitia pomocnej funckie isbutton 
-- cize keepne componenty ktore nie su button 
-- filter children su potom len pozite na vycreatovanie nove contanere s rovnakym menom len bez buttonu