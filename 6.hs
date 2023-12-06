-- definicia data typu component 
data Component = TextBox {name :: String, text :: String} -- prvy constructor pre component reprezentuje textBox ma dva fieldy name - string a text - string 
                | Button {name :: String, value :: String} -- druhy constructor reprezentujucci button ma dva fieldy name - string a value tiez string 
                | Container {name :: String, children :: [Component]}deriving(Show) -- treti konstruktor datovy typ reprezentujuci container ma dva fielndy a tak isto list componentov 
-- hovorime o jednoduchom priklade 
gui :: Component -- data type component 
gui = Container "My App" [
        Container "Menu" [
            Button "btn_new" "New", 
            Button "btn_open" "Open", 
            Button "btn_close" "Close"],
        Container "Body" [TextBox "textbox_1" "Some text goes here"],
        Container "Footer" []]

-- definicia nove algebrickeho datoveho typu article ktora ma dva construktory text a section 
data Article = Text String | Section String [Article] deriving (Show)

-- jednoduchy priklad 
clanek :: Article
clanek = Section "clanek1" [Text "text_text_text_text_text"]

-- Uloha 2 

-- priklad inputu listAllNames gui
-- podla hlavičky našej funckie vieme ze listAllNames ze funkcia berie Component ako input a returne list stringov 
listAllNames :: Component -> [String] 
listAllNames (TextBox name _) = [name]
listAllNames (Button name _) = [name]
-- sledujeme dva priklady pattern matchingu pre textbox a pre button funckcia vrati list containing name vyčleneny z korešpodnujuceho construcotru
listAllNames (Container name children ) = name : concatMap listAllNames children
-- tu realizujeme odstranenuie name a children kedy input je container meno je adnute do listu 
--oncatMap listAllNames children je pouzita rekurzivne na listnames na kazdy child component v children liste 
-- concatMap je higher order funkcia v haskeli ktora aplajuje funkciu na kazdy elelement listu a potom concatne lists spoji 

-- uloha 3 

isTarget' :: Component -> [String] -> Bool -- berie component a list stringov ako input a vracia boolean 
isTarget' (Container name _) x = if elem name x then True else False -- elem name checkne či name of the container je present v liste x 
isTarget' (TextBox name _) x = if elem name x then True else False  -- simularne pattern matchuju clauses pre textbox a button konštruktroy 
isTarget' (Button name _) x = if elem name x then True else False  -- same logic ako u containeru ale ore textbox a button 

-- prikald inputu pre remove alll elements removeAllElements gui ["btn_open", "textbox_1"]
removeAllElements :: Component -> [String] -> Component
removeAllElements gui [] = gui -- base case kedy je list target names prazdny -- len prosto returne originalne gui 
removeAllElements (TextBox name value) target -- handlnovanie kedy je input textbox beu ohladu na nazov returne 
    | elem name target = TextBox name value
    | otherwise = TextBox name value 
removeAllElements (Button name value) target -- simultativne k handlvoanie casu 1 
    | elem name target = Button name value
    | otherwise = Button name value
removeAllElements (Container name children) target = -- handlovanie kedy input je container pouziva filter removnutie elementov ktorych mena su v target liste a rekurzivne applajne removeelements na kazde dieta za pouzia map fun
    Container name (filter (\x -> not (elem (nameOf x) target)) (map (`removeAllElements` target) children)) -- filterne mena ktore su v target liste , funkcia map rekurzivne applajne removeAllElements na kazdy child component v children liste 
    where -- where kondicia defajnuje pomocnu funkciu ktora extractuje name z componentu pouziva filter na sledovanie či name childu je v target liste 
        nameOf (TextBox n _) = n
        nameOf (Button n _) = n
        nameOf (Container n _) = n
