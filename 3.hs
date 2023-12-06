-- uloha 1

-- definujeme parametricky datový typ, TernaryTreem ktory dokaže holdovať hodnoty typu a
data TernaryTree a
  = Leaf a
  | Node (TernaryTree a) (TernaryTree a) (TernaryTree a)
  deriving (Show)

-- ma dve "construtor" listy ( leafs ) - ktore su reprezentovane leaf node s hodnotami
-- a nodou ktora reprezentuje "internal" nodu s tromi substromami

-- naš example tree creatne inštanciu TernaryTree s int hodnotami
-- z toho nam vyplyva že štruktura je hierarchicka s vnútornými ("Nodes") a samozrejme s leaf nodami ktore teda formuje naš terarny strom
-- priklad použitia je prosto len exampleTree
exampleTree :: TernaryTree Int
exampleTree = Node (Node (Leaf 1) (Leaf 2) (Leaf 3)) (Leaf 4) (Node (Leaf 5) (Leaf 6) (Leaf 7))

-- Štrukturove rozloženie veci pre naše úlohy 2 a 3
-- tu definujeme datovy typ komponent pre buildenie naš GUI komponentu
-- ma tri konštrukty prvym je textbook - pre stringovu reprzentaciu teda textbox
-- button pre buttony
-- a containerm ktory dokaze holdiť list child componentov
data Component
  = TextBox {name :: String, text :: String}
  | Button {name :: String, value :: String}
  | Container {name :: String, children :: [Component]}
  deriving (Show)

-- creatujeme examplne našeh GUI štruktúry (gui) s vyuzitim component data typu
-- naše gui sa sklada z z top level kontajneru pomenovaneho "My app" s troma child kontajnermi - menu body a footer
-- a v neposlednom rade menu container obsahuje tri butny - New, open, close
-- kde body contaiener obsahuje ešte tex box a footer container je empty
gui :: Component
gui =
  Container
    "My App"
    [ Container
        "Menu"
        [ Button "btn_new" "New",
          Button "btn_open" "Open",
          Button "btn_close" "Close"
        ],
      Container "Body" [TextBox "textbox_1" "Some text goes here"],
      Container "Footer" []
    ]

-- Druha Uloha - input = countEmptyContainers gui
-- sledujeme že naš header berie ako vstup určity nami realizovany priklad gui struktury a vystupom je teda nija celočiselna hodnota
-- hovorime tu o rekurzivnej funkci, ktora traverzuje GUI štruktúru reprezentovanu ako Component data type
countEmptyContainers :: Component -> Int
countEmptyContainers (TextBox _ _) = 0 -- jednoducha realizacia base casu kedy component je TextBox alebo Buttom tak nebude zaratany do countu prazdnych containerov takze count bude 0
countEmptyContainers (Button _ _) = 0 -- rovnake pravidlo countovania ako pri textboxee
countEmptyContainers (Container _ []) = 1 -- pokial je inputovy component container a ma prazdny list children povazujeme ho za prazdny container tym padom bude kontribuovať ako count 1
countEmptyContainers (Container _ children) = sum (map countEmptyContainers children)

-- v neposlednom rade pokial je input komponent non empty cantainer tak je jednoducho count kalkulovany súčtom childrenom prostrednictvom našej map funkcie

-- úloha 3
-- ako mozeme sledovať podla headru funkcia takne component a string ( targetName) ako input a returnuje modifikovany component
-- rekurzivne prechadza cez component a odstranuje prvu zmienku buttonu so špecifickou zmienkou mena teda targetNamu
-- pokial je componmet TextBox ostava nezmeneny
-- pokial je component Button a jeho meno matchuje targetname replacuje Button s empty containerom
-- pokial je component kontainer rekurzivne applajne removebutton pre svoje "chidrens" filtrovanim target Buttonu
-- input pre funckiu ma vyzerať nasledovne : removeButton gui "btn_open"
removeButton :: Component -> String -> Component
removeButton (TextBox name text) _ = TextBox name text
removeButton button@(Button name _) targetName
  | name == targetName = Container "" []
  | otherwise = button
removeButton (Container name children) targetName =
  Container name (map (`removeButton` targetName) (filter (not . isTargetButton targetName) children))

-- co hovori naša funkcia isTargetButton -- je to tzv helper funkcia
-- helper funkcia pouzivana removebuttonom na checknutie či sa jedna ci component je button s špecifickym target namom

isTargetButton :: String -> Component -> Bool
isTargetButton targetName (Button name _) = name == targetName -- pokial teda component je button s specifickym targetnamom a component je button a matchuje v tagetname tak return true
isTargetButton _ _ = False -- inak false