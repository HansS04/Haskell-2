-- Zadani cislo 2

-- ukol 1 vytvoreni datoveho typu Entity
data Entity
  = Point {x :: Double, y :: Double}
  | Circle {stred_x :: Double, stred_y :: Double, polomer :: Int}
  | Box {boxChildren :: [Entity]}
  deriving (Show)

-- ukol 1.2 vytvoreni instance pro Entity (Priklad pro Entity)
entita :: Entity
entita =
  Box
    [ Point 7.2 6.5,
      Box
        [ Point 8.1 8.2,
          Circle 3.12 5.1 4,
          Circle 3.5 6.12 7
        ],
      Box
        [ Circle 3.65 8.1 8
        ]
    ]

-- Vzor pro dalsi ukoly 2 - 3
data Component
  = TextBox {name :: String, text :: String}
  | Button {name :: String, value :: String}
  | Container {name :: String, children :: [Component]}
  deriving (Show)

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

-- Ukol 2 spocitat buttons

-- Pokud bude constructor typu Button, tak se vlozi do pole 1
-- Pokud bude constructor typu TextBox, tak se vlozi do pole 0
-- Nakonci pokud to bude Container , tak se zbytek stromu vlozi do pole za pomoci map a secte sa pomoci sum

countButtons :: Component -> Int
countButtons (Button _ _) = 1
countButtons (TextBox _ _) = 0
countButtons (Container _ children) = sum (map countButtons children)


-- Funkce copyElement vezme Component, název tlačítka a jeho hodnotu a vrátí upravenou Component.
-- Pokud v seznamu children najde tlačítko se shodným názvem, přidá nové tlačítko pod existující.
-- Jinak rekurzivně prochází a upravuje děti kontejneru.
 -- Kontroluje, zda se v seznamu nachází tlačítko s daným názvem
 -- Přidá nové tlačítko pod existující, pokud bylo nalezeno
 -- Rekurzivně prochází a upravuje děti kontejneru 
 -- Pokud se jedná o tlačítko (a ne kontejner), vrátí se beze změn
copyElement :: Component -> String -> String -> Component
copyElement (Container name children) buttonName value
  | any (`hasButton` buttonName) children = 
      Container name (addNewButton buttonName value children)  
  | otherwise =
      Container name (map (\c -> copyElement c buttonName value) children)  
copyElement x _ _ = x 

-- Funkce hasButton kontroluje, zda daná komponenta odpovídá zadanému názvu tlačítka.
-- Kontroluje shodu názvu tlačítka
-- Vrací False pro jakoukoli jinou komponentu než tlačítko
hasButton :: Component -> String -> Bool
hasButton (Button name _) btn = name == btn  
hasButton _ _ = False  

-- Funkce addNewButton přidá nové tlačítko pod existující tlačítko se shodným jménem v seznamu children.
-- Pokud je seznam prázdný, přidá nové tlačítko na začátek
-- Pokud je nalezeno tlačítko se shodným jménem, přidá nové tlačítko pod něj
-- Jinak pokračuje rekurzivně prohledávat zbytek seznamu
addNewButton :: String -> String -> [Component] -> [Component]
addNewButton btn value [] = [Button (btn ++ "_copy") value]  
addNewButton btn value (c : cs) =
  if hasButton c btn
    then c : Button (btn ++ "_copy") value : cs  
    else c : addNewButton btn value cs  

