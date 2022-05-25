module Ex05 where

import Text.Read (readMaybe)

data Token = Number Int | Operator (Int -> Int -> Int)

parseToken :: String -> Maybe Token
parseToken "+" = Just (Operator (+))
parseToken "-" = Just (Operator (-))
parseToken "/" = Just (Operator div)
parseToken "*" = Just (Operator (*))
parseToken str = fmap Number (readMaybe str)

tokenise :: String -> Maybe [Token]
tokenise str = sequence (parseToken <$> words(str))
{-}
tokenise :: String -> Maybe [Token]
tokenise str = case (go (words str)) of
                [] -> Nothing
                x -> Just (x)
                x:xs -> Just (x:xs)
  where
    go :: [String] -> [Token]
    go [] = []
    go (x:xs) = case (parseToken x) of
                  Just a -> (a:(go xs))
                  Nothing -> []
-}
newtype Calc a = C ([Int] -> Maybe ([Int], a))


pop :: Calc Int
pop = C pop2
  where
    pop2 :: [Int] -> Maybe ([Int], Int)
    pop2 [] = Nothing
    pop2 (x:xs) = Just (xs, x)


push :: Int -> Calc ()
push i = C push2
  where 
    push2 :: [Int] -> Maybe ([Int], ())
    push2 [] = Just([i], ())
    push2 (xs) = Just((xs ++ [i]), ())




instance Functor Calc where
  fmap f (C sa) = C $ \s ->
      case sa s of 
        Nothing      -> Nothing
        Just (s', a) -> Just (s', f a)

instance Applicative Calc where
  pure x = C (\s -> Just (s,x))
  C sf <*> C sx = C $ \s -> 
      case sf s of 
          Nothing     -> Nothing
          Just (s',f) -> case sx s' of
              Nothing      -> Nothing
              Just (s'',x) -> Just (s'', f x)

instance Monad Calc where
  return = pure
  C sa >>= f = C $ \s -> 
      case sa s of 
          Nothing     -> Nothing
          Just (s',a) -> unwrapCalc (f a) s'
    where unwrapCalc (C a) = a

evaluate :: [Token] -> Calc Int
evaluate = error "e"
--evaluate [] = pop
--evaluate ((Number n):[]) = push n

calculate :: String -> Maybe Int
calculate s = error "calculate unimplemented"--undefined --evaluate <$> tokenise s

