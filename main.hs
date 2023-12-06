evens :: String -> String
evens xs = pomocna xs 0

pomocna :: String -> Int -> String
pomocna [] _ = []
pomocna (x : xs) a
  | even a = x : pomocna xs (a + 1)
  | otherwise = pomocna xs (a + 1)

swap :: String -> String
swap xs = reverse (swapPomoc (reverse (xs)))

swapPomoc :: String -> String
swapPomoc (x : xs) = "!" ++ xs

swap2 :: String -> String
swap2 xs = init xs ++ "!"

makeGirl :: String -> String
makeGirl xs = xs ++ "ova"

nahrad :: String -> Char -> Char -> String
nahrad [] _ _ = []
nahrad (x : xs) a b
  | x == a = b : nahrad xs a b
  | otherwise = x : nahrad xs a b

odds :: String -> String
odds [] = []
odds (x : y : xs) = y : odds xs

interval :: [Int] -> (Int, Int)
interval xs = (minimum xs, maximum xs)

nejmensi :: [Int] -> Int
nejmensi [x, y]
  | x < y = x
  | otherwise = y
nejmensi (x : xs)
  | x < nejmensi xs = x
  | otherwise = nejmensi xs

nej :: [Int] -> Int
nej [x] = x
nej (x : y : xs)
  | x < y = nej (x : xs)
  | otherwise = nej (y : xs)

average :: [Double] -> Double
average xs = sum xs / fromIntegral (length (xs))

check :: String -> Bool
check xs
  | length (filter (== ')') xs) == length (filter (== '(') xs) = True
  | otherwise = False

prunik :: (Eq a) => [a] -> [a] -> [a] -> [a]
prunik xs ys zs = [(x) | x <- xs, x `elem` ys, x `elem` zs]

powers :: Int -> Int -> [Int]
powers x a = reverse (pomocnaPowers x a)

pomocnaPowers :: Int -> Int -> [Int]
pomocnaPowers _ 0 = []
pomocnaPowers x a = x ^ (a - 1) : pomocnaPowers x (a - 1)

mostFrequent :: [Int] -> Int
mostFrequent (x : y : xs)
  | length (x : y : xs) == length (filter (== x) (x : y : xs)) = x
  | y == x = mostFrequent (y : xs ++ [x])
  | length (filter (== x) xs) > length (filter (== y) xs) = mostFrequent (x : xs)
  | otherwise = mostFrequent (y : xs)

names :: [(String, String)] -> String -> [String]
names [] _ = []
names ((x, y) : xs) a
  | isSubString y a = x : names xs a
  | otherwise = names xs a

isSubString :: String -> String -> Bool
isSubString _ [] = True
isSubString ys (a : b : as)
  | a `elem` ys && b `elem` ys = isSubString ys as
  | otherwise = False

filter' :: [(String, Int)] -> Int -> [String]
filter' [] _ = []
filter' ((x, y) : xs) a
  | jeMensi y a = x : filter' xs a
  | otherwise = filter' xs a

jeMensi :: Int -> Int -> Bool
jeMensi y a
  | y < a = True
  | otherwise = False
