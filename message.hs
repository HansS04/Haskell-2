data Article
  = Text {jmeno :: String}
  | Sec {nadpis :: String, podkapitola :: [Article]}

test1 :: Article
test1 = Sec "Nazev" [Text "Text", Sec "12" []]

data Point = Point {column :: Int, row :: Int} deriving (Show)

data Position = Position {leftTopCorner :: Point, width :: Int, height :: Int} deriving (Show)

data Component
  = TextBox {name :: String, position :: Position, text :: String}
  | Button {name :: String, position :: Position, text :: String}
  | Container {name :: String, children :: [Component]}
  deriving (Show)

gui :: Component
gui =
  Container
    "My App"
    [ Container
        "Menu"
        [ Button "btn_new" (Position (Point 0 0) 100 20) "New",
          Button "btn_open" (Position (Point 100 0) 100 20) "Open",
          Button "btn_close" (Position (Point 200 0) 100 20) "Close"
        ],
      Container "Body" [TextBox "textbox_1" (Position (Point 0 20) 300 500) "Some text goes here"],
      Container "Footer" []
    ]

-- countEachComp :: Component -> (Int, Int, Int)
-- countEachComp (TextBox _ _ _) = (1, 0, 0)
-- countEachComp (Button _ _ _) = (0, 1, 0)
-- countEachComp (Container name children) = (0, 0, 1) + sum (map countEachComp children)

listAllButton :: Component -> [String]
listAllButton (Container _ children) = concatMap listAllButton children
listAllButton (Button name _ _) = [name]
listAllButton x = []

listAllNames :: Component -> [String]
listAllNames (Button name _ _) = [name]
listAllNames (TextBox name _ _) = [name]
listAllNames (Container name children) = [name] ++ concat (map listAllNames children)

allBuuton :: Component -> [Component]
allBuuton (Button name position text) = [Button name position text]
allBuuton (Container _ children) = concatMap allBuuton children
allBuuton (TextBox _ _ _) = []

countButton :: Component -> Int
countButton (Button _ _ _) = 1
countButton (TextBox _ _ _) = 0
countButton (Container _ children) = sum (map countButton children)

allComponent :: Component -> Int
allComponent (Button _ _ _) = 1
allComponent (TextBox _ _ _) = 1
allComponent (Container _ children) = 1 + sum (map allComponent children)

guiDepth :: Component -> Int
guiDepth (Container _ []) = 0
guiDepth (Container _ children) = 1 + maximum (map guiDepth children)
guiDepth x = 0

printPath :: Component -> String -> String
printPath (Button name _ _) str
  | name == str = name
  | otherwise = []
printPath (TextBox name _ _) str
  | name == str = name
  | otherwise = []
printPath (Container name children) str = name ++ "/" ++ concatMap (\x -> printPath x (str)) children

-- odstanění, vytvoření, přepsání
tmr :: Component -> Bool
tmr (Button _ _ _) = False
tmr (TextBox _ _ _) = False
tmr x = True

justContainer :: Component -> Component
justContainer (Container name children) = Container name [justContainer c | c <- children, tmr c]
justContainer x = x

-- removeComp :: Component -> String -> Int -> Component
-- removeComp
trm2 :: Component -> [String] -> Bool
trm2 x [] = True
trm2 (Button name a b) (x : xs)
  | name == x = False
  | otherwise = trm2 (Button name a b) xs
trm2 (TextBox name a b) (x : xs)
  | name == x = False
  | otherwise = trm2 (TextBox name a b) xs
trm2 (Container _ _) str = True

removeElements :: Component -> [String] -> Component
removeElements (Container name children) str = Container name [removeElements c str | c <- children, trm2 c str]
removeElements x str = x

changeText :: Component -> String -> String -> Component
changeText (TextBox name a text) porov new
  | name == porov = TextBox name a new
  | otherwise = TextBox name a text
changeText (Button name a text) porov new
  | name == porov = Button name a new
  | otherwise = Button name a text
changeText (Container name children) porov new = Container name [changeText c porov new | c <- children]

addElement :: Component -> Component -> String -> Component
addElement (Container name children) x str
  | name == str = Container name (children ++ [x])
  | otherwise = Container name [addElement c x str | c <- children]
addElement x y str = x



