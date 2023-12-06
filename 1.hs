-- Uloha 1 
-- Definujeme datovy typ "Copmapny", ktory ma tri fieldny 
--name of company string reprezentujuci meno company 
--numberOfEmployess je int reprezentujuci počet employes v company 
--ownerOf je list [Company] reprezentujuci firmy ktore su vlastnene current company 
data Company = Company {
  nameOfCompany :: String,
  numberOfEmployees :: Int,
  ownerOf :: [Company]
} deriving Show

-- creatujeme instanciu Company type named company1 ma mene copmany1 420 zamestnancov a empty list ([]) teda neownuje ziadne ine companies
company1 :: Company
company1 = Company "Company1" 420 []

-- rovnako creatujeme aj company2 -- ani ta inštancia nevlastni ziadne ine companies 
company2 :: Company
company2 = Company "Company2" 69000 []

-- tu vytvarame inštanciu s menom parentcompany - 10,000,000 employees a list companies, ktore vlastni ten indikuje nam to ze vlastni obe vyššie dekladovane inšancie 
parentCompany :: Company
parentCompany = Company "Parent to all companies" 10000000 [company1, company2]

-- Implementačne nutnosti pre ulohu 2 a 3 

-- definujeme data type Component ktory sa sklada z troch konštruktorov 

data Component = TextBox {name :: String, text :: String} -- reprezentuje text box s name a textom oba su stringove formaty 
               | Button {name :: String, value :: String} -- button je reprezentovany namom a valuom 
               | Container {name :: String, children :: [Component]} deriving Show -- reprezentuje container s namom a listom children komponentov 

gui :: Component
gui = Container "My App" [
    Container "Menu" [
        Button "btn_new" "New",
        Button "btn_open" "Open",
        Button "btn_close" "Close"],
    Container "Body" [TextBox "textbox_1" "Some text goes here"],
    Container "Footer" []]

-- vytvarame inštaciu Compontnt s nazvom gui
-- reprezentuje gui štruktúru s top-level container s menom "My App" obsahujucu tri sub-containers ("Menu," "Body," and "Footer")
-- kazdy sub container ma svoje vlastne komponenty buttons / text boxes 

-- Uloha 2 

-- jedna sa o funkciu ktora ako mozeme sledovať podla headru zoberie ako vstup compenent a vystup je nijaka celočiselna hodnota počtu vyskytu 
countAllComponents :: Component -> Int -- vyuzivame realizaciu prostrednictvom pattern matchingu 
countAllComponents (TextBox _ _) = 1 -- sledujeme či teda naš input je textbox alebo buton v oboch pripadoch bude count inkriminovany o 1 nakolko pretoze textBox alebo button itself su brane ako komponenty 
countAllComponents (Button _ _) = 1 -- rovnaka logika ako na riadku 50 
countAllComponents (Container _ children) = 1 + sum (map countAllComponents children)
-- klasicke vyuzitie rekurzie na countovanie komponentov v ramci children listu 
--mapuje countAllComponents pre kazdy element v liste produkuje list of counts 
-- nasledne len sum funkcia adds upne count 
-- +1 znamene len addnutie countu pre current container ktory je countovany ako component 
-- priklady vstupu 

-- countAllComponents (TextBox "exampleTextBox" "Some text")
-- countAllComponents (Button "exampleButton" "Click me")
-- countAllComponents (Container "exampleContainer" [TextBox "text1" "Text", Button "btn1" "Click"])


-- Ukol 3

-- priklady inputy pre funckiu removeEmptyContainers

--removeEmptyContainers (TextBox "textName" "textValue")
--removeEmptyContainers (Button "buttonName" "buttonValue")
--removeEmptyContainers (Container "containerName" [TextBox "child1" "value1", Button "child2" "value2"])
--removeEmptyContainers (Container "emptyContainer" [])

-- tato funkcia odstranuje prazdne containeri z hierarchie componentov 
-- podla headeru ako input je brany container a vystupom je modifnuty container
-- pre textbox a button funkcia vrati rovnaky komponent pretoze su to tzv leaf komponenty ktore nemoze byt prazdne 
-- pre container sleduje cu containerr nie je przadny cez isEmpty function
-- pokial nie je prazdna reekurzivne vola removeEmptyContainers na kazdy child component, filtruje prazdne 
-- vytvara nove Container pre non empty child 
removeEmptyContainers :: Component -> Component
removeEmptyContainers(TextBox name value) = TextBox name value
removeEmptyContainers(Button name value) = Button name value
removeEmptyContainers (Container name children) = 
    if not (isEmpty (Container name children)) 
    then Container name (filter (not . isEmpty) (map removeEmptyContainers children))
    else Container name []

-- pomocna funkcia 
--pokial je input Container s empty listom of children 
-- povazujeme ho za empty a funkcia returnuje True 
-- pre kazdy iny type of component funkcia returnuje false indikuje none empty 
isEmpty :: Component -> Bool
isEmpty (Container _ []) = True
isEmpty _ = False