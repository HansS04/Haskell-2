-- Importy různých knihoven a modulů
import Data.Text (Text)
import GHC.Float (int2Double)
import Distribution.Simple.Utils (xargs)
import Data.Time.Format.ISO8601 (yearFormat)
import Control.Monad.RWS.Class (MonadState(get))
import Control.Exception (mask)
import Data.Char
import Data.Array (Ix(index))
import Data.ByteString.Builder (FloatFormat)

-- Definice datové struktury trojcestného stromu
data TernaryTree a = Leaf a | Branch (TernaryTree a) (TernaryTree a) (TernaryTree a)

-- Příklad trojcestného stromu
strom :: TernaryTree Integer
strom = Branch (Leaf 11) (Branch (Leaf 3) (Leaf 8) (Leaf 1)) (Leaf 3)

-- Definice různých komponent uživatelského rozhraní: TextBox, Button, Container
data Component = TextBox {name :: String, text :: String}
               | Button {name :: String, value :: String}
               | Container {name :: String, children :: [Component]}
               deriving(Show)

-- Příklad struktury uživatelského rozhraní
gui :: Component
gui = Container "Moje Aplikace" [
    Container "Menu" [
        Button "btn_novy" "Nový",
        Button "btn_otevrit" "Otevřít",
        Button "btn_zavrit" "Zavřít"],
    Container "Tělo" [TextBox "textbox1" "Zde je nějaký text"],
    Container "Patička" []]

-- Funkce pro počítání výskytů různých komponent v uživatelském rozhraní
pocetVyskytu :: Component -> (Int, Int, Int)
pocetVyskytu (TextBox _ _) = (1, 0, 0)
pocetVyskytu (Button _ _) = (0, 1, 0)
pocetVyskytu (Container _ deti) =
    let vysledky = foldr (\dite (tb, btn, cont) ->
                        let (tb', btn', cont') = pocetVyskytu dite
                        in (tb + tb', btn + btn', cont + cont')
                      ) (0, 0, 1) deti
    in vysledky

-- Příklad trojcestného stromu
stromek :: TernaryTree Integer
stromek = Branch (Leaf 10) (Branch (Leaf 5) (Leaf 1) (Leaf 1)) (Leaf 5)

-- Funkce pro přidání komponenty do konkrétního kontejneru na daném indexu
pridejKomponentuDoKontejneruNaIndexu :: Component -> Component -> String -> Int -> Component
pridejKomponentuDoKontejneruNaIndexu komponenta novaKomponenta nazevKontejneru index
    | odpovidajiciKontejner komponenta nazevKontejneru = vlozKomponentu komponenta novaKomponenta index
    | otherwise = komponenta

-- Funkce pro vložení komponenty na konkrétní index do kontejneru
vlozKomponentu :: Component -> Component -> Int -> Component
vlozKomponentu (Container nazev deti) novaKomponenta index =
    Container nazev (vlozNaIndex deti novaKomponenta index)
vlozKomponentu jinaKomponenta novaKomponenta _ = novaKomponenta

-- Funkce pro vložení prvku na konkrétní index do seznamu
vlozNaIndex :: [a] -> a -> Int -> [a]
vlozNaIndex xs x index = take index xs ++ [x] ++ drop index xs

-- Funkce pro ověření, zda komponenta odpovídá zadanému názvu kontejneru
odpovidajiciKontejner :: Component -> String -> Bool
odpovidajiciKontejner (Container nazev _) nazevKontejneru = nazev == nazevKontejneru
odpovidajiciKontejner _ _ = False
