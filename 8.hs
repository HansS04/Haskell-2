-- 1 ukol datovy typ
data HTMLDocument
  = Attribute {attributeName :: String, attributeValue :: String}
  | Tag {tagName :: String, tagChildren :: [HTMLDocument]}
  deriving (Show)

-- ukol 1.2 vytvoreni instance pro HTMLDocument (Priklad pro HTMLDocument)
htmlDocument :: HTMLDocument
htmlDocument =
  Tag
    "html"
    [ Tag
        "head"
        [ Tag "title" [Attribute "text" "Hello, World!"]
        ],
      Tag
        "body"
        [ Tag "h1" [Attribute "class" "heading", Attribute "id" "main-heading", Attribute "style" "color:blue;"],
          Tag "p" [Attribute "id" "paragraph", Attribute "style" "font-size:16px;", Attribute "text" "This is a sample paragraph."],
          Tag
            "div"
            [ Attribute "class" "container",
              Tag
                "ul"
                [ Tag "li" [Attribute "text" "Item 1"],
                  Tag "li" [Attribute "text" "Item 2"],
                  Tag "li" [Attribute "text" "Item 3"]
                ]
            ]
        ]
    ]

-- Tohle je potreba pro dalsi ukoly
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

-- ukol 2

-- Funkce printPaths prochází strukturou Component a vytváří řetězec reprezentující cestu ke komponentám se zadaným jménem.
-- Vytvoření cesty pro Button, pokud jméno odpovídá zadanému řetězci.
-- Vrací prázdný řetězec, pokud jméno není shodné.
-- Vytvoření cesty pro TextBox, pokud jméno odpovídá zadanému řetězci.
-- Vrací prázdný řetězec, pokud jméno není shodné.
-- Pokud nějaká dětská komponenta obsahuje hledané jméno,
-- vytvoří cestu a prochází dál strukturou.
-- Vrací prázdný řetězec, pokud žádné jméno není shodné.
printPaths :: Component -> String -> String
printPaths (Button name _) str
  | name == str = "/" ++ name 
  | otherwise = "" 
printPaths (TextBox name _) str
  | name == str = "/" ++ name 
  | otherwise = "" 
printPaths (Container name children) str
  | any (`containsComponent` str) children 
    =
      "/" ++ name ++ concat [printPaths c str | c <- children] 
  | otherwise = "" 

-- Funkce containsComponent kontroluje, zda je v komponentě obsaženo zadané jméno.
-- Kontroluje, zda jméno Buttonu odpovídá zadanému řetězci.
-- Kontroluje, zda jméno TextBoxu odpovídá zadanému řetězci.  
-- Rekurzivně kontroluje dětské komponenty, zda obsahují zadané jméno.
containsComponent :: Component -> String -> Bool
containsComponent (Button name _) porov = name == porov 
containsComponent (TextBox name _) porov = name == porov 
containsComponent (Container _ children) porov =

  any (`containsComponent` porov) children

-- Celý výraz any (containsComponent porov) children  prochází každou dětskou komponentu
-- v children a aplikuje funkci containsComponent s argumentem porov.
-- Výsledkem je True, pokud alespoň jedna z dětských komponent obsahuje zadané jméno porov, jinak False.

-- Funkce removeComponentFromContainerAtIndex odstraňuje komponenty z kontejneru s daným jménem na zadaném indexu.
-- Pokud je jméno kontejneru shodné, odstraní komponentu na daném indexu.
-- Rekurzivně prochází děti, pokud jméno není shodné.
-- Vrací původní komponentu, pokud není typu Container.
removeComponentFromContainerAtIndex :: Component -> String -> Int -> Component
removeComponentFromContainerAtIndex (Container name children) containerName index
  | name == containerName = Container name (removeAtIndex children index) 
  | otherwise = Container name [removeComponentFromContainerAtIndex c containerName index | c <- children] 
removeComponentFromContainerAtIndex x _ _ = x 

-- Funkce removeAtIndex odstraňuje prvek z daného seznamu na zadaném indexu.
-- Pokud je seznam prázdný, vrátí prázdný seznam.
-- Pokud dosáhneme indexu 0, odstraníme prvek.
-- Jinak rekurzivně pokračujeme přes zbytek seznamu.
removeAtIndex :: [a] -> Int -> [a]
removeAtIndex [] _ = [] 
removeAtIndex (x : xs) n
  | n == 0 = xs 
  | otherwise = x : removeAtIndex xs (n - 1)

-- Funkce trm2 kontroluje, zda je daný kontejner se zadaným jménem.
-- Porovnává jméno kontejneru se zadaným jménem.
-- Vrací False pro všechny ostatní případy.
trm2 :: Component -> String -> Int -> Bool
trm2 (Container name _) containerName _ = name == containerName 
trm2 _ _ _ = False 
