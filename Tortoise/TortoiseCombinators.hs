module TortoiseCombinators
       ( andThen 
       , loop 
       , invisibly 
       , retrace 
       , overlay 
       ) where

import Tortoise

-- recursively descends through the instructions, replacing the Stop instruction with the second argument to andThen.

andThen :: Instructions -> Instructions -> Instructions
andThen (Move a i1) i2 = (Move a) (andThen i1 i2)
andThen (Turn a i1) i2  = (Turn a) (andThen i1 i2)
andThen (SetStyle a i1) i2 = (SetStyle a) (andThen i1 i2)
andThen (SetColour a i1) i2  = (SetColour a) (andThen i1 i2)
andThen (PenDown i1) i2  = (PenDown) (andThen i1 i2)
andThen (PenUp i1) i2 = (PenUp) (andThen i1 i2) 
andThen (Stop) i2 = i2

loop :: Int -> Instructions -> Instructions
loop n i = case (compare n 1) of
              LT -> (Stop)
              EQ -> i
              GT -> i `andThen` loop (n - 1) i

invisibly :: Instructions -> Instructions
invisibly i = (PenUp) (invisibly' i True)

-- takes in instructions, keeps track of if pen is down, strips out PenDown, return new instructions
invisibly' :: Instructions -> Bool -> Instructions
invisibly' (Move a i1) b = (Move a) (invisibly' i1 b)
invisibly' (Turn a i1) b = (Turn a) (invisibly' i1 b)
invisibly' (SetStyle a i1) b = (SetStyle a) (invisibly' i1 b)
invisibly' (SetColour a i1) b  = (SetColour a) (invisibly' i1 b)
invisibly' (PenDown i1) b = (invisibly' i1 True) -- PenDown: remove instruction, set PenState bool to True
invisibly' (PenUp i1) b = (invisibly' i1 False) -- PenUp: remove instruction, set PenState to False
invisibly' (Stop) b = case b of 
                            True -> (PenDown) (Stop) -- if true, pen should be down
                            False -> (Stop)

retrace :: Instructions -> Instructions
retrace i = (retrace'' i)

{-
retrace' :: Instructions -> (Instructions -> Instructions -> Instructions)
retrace' (Move a i1) = ((retrace' i1) `andThen` (Move a))
retrace' (Turn a i1) = ((retrace' i1) `andThen`  (Turn a))
retrace' (SetStyle a i1) = ((retrace' i1) `andThen`  (SetStyle a))
retrace' (SetColour a i1)  = ((retrace' i1) `andThen`  (SetColour a))
retrace' (PenDown i1) = ((retrace' i1) `andThen` (PenDown))
retrace' (PenUp i1) = ((retrace' i1) `andThen` PenUp)
retrace' (Stop) = (PenDown)
-}
retrace'' :: Instructions -> Instructions
retrace'' i = go i (Stop) (collectC i [white]) (collectL i [(Solid 1)])
       where
              -- takes in instructions and accumulator and returns reversed insturctions
              go :: Instructions -> Instructions -> [Colour] -> [LineStyle] -> Instructions
              go (Stop) a c l = a
              --go (x:xs) a = go xs (x:a)
              
              go (Move arg i1) a c l = go i1 (Move (-arg) a) c l
              go (Turn arg i1) a c l = go i1 (Turn (-arg) a) c l
              go (SetStyle arg i1) a c l = go i1 (SetStyle (head l) a) c (drop 1 l)
              go (SetColour arg i1) a c l = go i1 (SetColour (head c) a) (drop 1 c) l
              go (PenDown i1) a c l = go i1 (PenDown a) c l
              go (PenUp i1) a c l = go i1 (PenDown a) c l
              {-}
              go :: Instructions -> Instructions -> Instructions
              go (Stop) a = a
              --go (x:xs) a = go xs (x:a)
              go (Move arg i1) a = go i1 (Move (-arg) a)
              go (Turn arg i1) a = go i1 (Turn (-arg) a)
              go (SetStyle arg i1) a = go i1 (SetStyle arg a)
              go (SetColour arg i1) a = go i1 (SetColour arg a)
              go (PenDown i1) a = go i1 (PenUp a)
              go (PenUp i1) a = go i1 (PenDown a)
              -}
              collectC :: Instructions -> [Colour] -> [Colour]
              collectC (SetColour a i1) cs = collectC i1 (cs++[a])
              collectC (Stop) cs = cs
              collectC (Move _ i1) cs = collectC i1 cs
              collectC (Turn _ i1) cs = collectC i1 cs
              collectC (SetStyle _ i1) cs = collectC i1 cs
              collectC (PenUp i1) cs = collectC i1 cs
              collectC (PenDown i1) cs = collectC i1 cs

              collectL :: Instructions -> [LineStyle] -> [LineStyle]
              collectL (SetStyle a i1) cs = collectL i1 (cs++[a])
              collectL (Stop) cs = cs
              collectL (Move _ i1) cs = collectL i1 cs
              collectL (Turn _ i1) cs = collectL i1 cs
              collectL (SetColour _ i1) cs = collectL i1 cs
              collectL (PenUp i1) cs = collectL i1 cs
              collectL (PenDown i1) cs = collectL i1 cs
{-}
              collectB :: Instructions -> [LineStyle] -> [LineStyle]
              collectB (SetStyle _ i1) cs = collectB i1 cs
              collectB (Stop) cs = cs
              collectB (Move _ i1) cs = collectB i1 cs
              collectB (Turn _ i1) cs = collectB i1 cs
              collectB (SetColour _ i1) cs = collectB i1 cs
              collectB (PenUp i1) cs = collectB i1 (cs++[False])
              collectB (PenDown i1) cs = collectLB i1 (cs++[True])
-}
-- go through reversed instructions and reverse the colours (to put them back the right way)


overlay :: [Instructions] -> Instructions
overlay [] = Stop
overlay (x:xs) = x `andThen` goBack `andThen` overlay xs
                     where
                            goBack = invisibly (retrace x) 

