import Test.QuickCheck
import Data.Char
import Data.Ord

rot13 :: String -> String
rot13 = map $ \x -> 
          case lookup x table of
            Just y  -> y 
            Nothing -> x
  where
    table = table' 'A' 'Z' ++ table' 'a' 'z'
    table' a z = zip [a..z] (drop 13 (cycle [a..z]))
    
prop1 x = length x == length (rot13 x)
prop2 x = rot13 (map toUpper x) == map toUpper (rot13 x)
prop4 x = all (not . isAlpha) x ==> rot13 x == x
prop5 a b = rot13 (a ++ b) == rot13 a ++ rot13 b
prop6 x = not (null x) ==> ord (head x) + 13 == ord (head (rot13 x)) 
prop7 x = rot13 (rot13 x) == x


toBinary :: Int -> String
toBinary 0 = ""
toBinary n = let (d,r) = n `divMod` 2
              in toBinary d 
                   ++ if r == 0 then "0"
                                else "1"

fromBinary :: String -> Int
fromBinary = fst . foldr eachChar (0,1)
  where
    eachChar '1' (sum, m) = (sum + m, m*2)
    eachChar _   (sum, m) = (sum    , m*2)
    
b1 i = i >= 0 ==> fromBinary (toBinary i) == i
b2 s = all (`elem` "01") s ==> toBinary (fromBinary s) == s
b3 s = all (`elem` "01") s ==> read s >= fromBinary s 
b4 i = i > 0 ==> length (toBinary i) >= length (show i) 
b5 s = all (`elem` "01") s ==> fromBinary s == fromBinary ('0':s)

dedup :: (Eq a) => [a] -> [a]
dedup (x:y:xs) | x == y = dedup (y:xs)
               | otherwise = x : dedup (y:xs)
dedup xs = xs

sorted :: (Ord a) => [a] -> Bool
sorted (x:y:xs) = x <= y && sorted (y:xs)
sorted xs = True

s1 xs = sorted xs ==> sorted (dedup xs)
s3 xs = sorted xs ==> dedup (dedup xs) == dedup xs
s4 xs ys = sorted xs && sorted ys ==> dedup xs ++ dedup ys == dedup (xs ++ ys)
s5 xs = sorted xs ==> length (dedup xs) < length xs
s6 x xs = (x `elem` xs) == (x `elem` dedup xs)

fun :: [Integer] -> [Integer]
fun []       = []
fun [x]      = []
fun (x:y:xs) = (y-x):fun (y:xs)

nuf :: [Integer] -> Integer -> [Integer]
nuf xs i = scanl (\v x -> v + x) i xs 

prop_1 :: [Integer] -> Integer -> Bool
prop_1 xs x = nuf (fun (x:xs)) x == (x:xs)

prop_2 :: [Integer] -> Integer -> Bool
prop_2 xs x = fun (nuf xs x) == xs