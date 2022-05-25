{-# LANGUAGE FlexibleContexts #-}
module Ex04 where
import Text.Read (readMaybe)
import System.IO
import Data.Char
import System.Environment
import Control.Monad.State
import System.Random
import Test.QuickCheck

capitalise :: FilePath -> FilePath -> IO ()
capitalise i o = 
  do
    str <- readFile i    
    writeFile o (cap str) 
    where
      cap :: String -> String
      cap [] = []
      --cap x = (toUpper x)
      cap (x:xs) = (toUpper x):(cap xs)
  --openFile :: FilePath -> IOMode -> IO Handle
  --readFile :: FilePath -> IO String
  --toUpper :: Char -> Char

-- str is a string of numbers on newlines
-- put each number in a separate list
-- put each list into a single list
-- sum the list
sumFile :: IO ()
sumFile = 
  do
    args <- getArgs
    str <- readFile (args !! 0)
    writeFile (args !! 1) (show(sum(map fix (map readMaybe(lines (str))))))
    where
      fix :: Maybe Int -> Int
      fix (Just a) = a
      fix Nothing = 0

data Player m = Player { guess :: m Int
                       , wrong :: Answer -> m ()
                       }
data Answer = Lower | Higher

guessingGame :: (Monad m) => Int -> Int -> Player m -> m Bool
guessingGame x n p = go n
  where
   go 0 = pure False
   go n = do
     x' <- guess p
     case compare x x' of
       LT -> wrong p Lower  >> go (n-1)
       GT -> wrong p Higher >> go (n-1)
       EQ -> pure True

human :: Player IO
human = Player { guess = guess, wrong = wrong }
  where
    guess = do
      putStrLn "Enter a number (1-100):"
      x <- getLine
      case readMaybe x of
        Nothing -> guess
        Just i  -> pure i

    wrong Lower  = putStrLn "Lower!"
    wrong Higher = putStrLn "Higher!"

play :: IO ()
play = do
  x <- randomRIO (1,100)
  b <- guessingGame x 5 human
  putStrLn (if b then "You got it!" else "You ran out of guesses!")


midpoint :: Int -> Int -> Int
midpoint lo hi | lo <= hi  = lo + div (hi - lo) 2
               | otherwise = midpoint hi lo

ai :: Player (State (Int,Int))
ai = Player { guess = guess, wrong = wrong }
  guess :: State(Int,Int) Int
  guess = do
    s <- get
    case wrong of
      Lower  -> put ((midpoint 0 guess), _)
      Higher -> midpoint guess 100
      _      -> midpoint 0 100
  
  wrong :: Answer




prop_basic (Positive n) = forAll (choose (1,n)) $ \x -> evalState (guessingGame x n ai) (1,n)

prop_optimality (Positive n) = forAll (choose (1,n)) $ \x -> evalState (guessingGame x (bound n) ai) (1,n)
  where bound n = ceiling (logBase 2 (fromIntegral n)) + 1


