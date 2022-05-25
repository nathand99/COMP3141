{-# LANGUAGE GADTs, DataKinds, KindSignatures, TupleSections, PolyKinds, TypeOperators, TypeFamilies, PartialTypeSignatures #-}
module Hare where
import Control.Monad
import Control.Applicative 
import HareMonad 

data RE :: * -> * where 
  Empty :: RE ()
  Fail :: RE a
  Char :: [Char] -> RE Char
  Seq :: RE a -> RE b -> RE (a,b)
  Choose :: RE a -> RE a -> RE a
  Star :: RE a -> RE [a]
  Action :: (a -> b) -> RE a -> RE b

match :: (Alternative f, Monad f) => RE a -> Hare f a
match Empty = pure ()
match Fail = failure
match (Char cs) = do 
    x <- readCharacter
    guard (x `elem` cs)
    pure (x)
match (Seq a b) = do 
    ra <- match a 
    rb <- match b  
    pure (ra, rb)
match (Choose a b) = 
        match a
    <|> match b
match (Star a) =
        addFront <$> match a <*> match (Star a)
    <|> pure ([])
  where 
    addFront x xs = x:xs
match (Action f a) = f <$> match a

matchAnywhere :: (Alternative f, Monad f) => RE a -> Hare f a
matchAnywhere re = match re <|> (readCharacter >> matchAnywhere re)

(=~) :: (Alternative f, Monad f) => String -> RE a -> f a 
(=~) = flip (hare . matchAnywhere)

infixr `cons`  
cons :: RE a -> RE [a] -> RE [a]
cons x xs = Action cons' (Seq x xs)
  where
    cons' :: (a,[a]) -> [a]
    cons' (x,xs) = x:xs

string :: String -> RE String
--string [] = Action f (Empty)
--  where
--    f :: RE () -> RE String
--    f Empty = pure []
string xs = error "'string' unimplemented"

rpt :: Int -> RE a -> RE [a]
--rpt 0 re = re
rpt n re = Action cons' (Seq re (rpt (n - 1) re))
  where
    cons' :: (a,[a]) -> [a]
    cons' (x,xs) = x:xs

rptRange :: (Int, Int) -> RE a -> RE [a]
rptRange (x,y) re = error "'rptRange' unimplemented"

option :: RE a -> RE (Maybe a)
option re = error "'option' unimplemented"

plus :: RE a -> RE [a]
plus re = Action f (Seq re (Star re))
  where
    f :: (a,[a]) -> [a]
    f (a,as) = a:as

choose :: [RE a] -> RE a
choose [] = Fail
choose (r:rs) = Choose r (choose rs)

