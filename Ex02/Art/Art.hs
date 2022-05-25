module Art where  

import ShapeGraphics
import Codec.Picture

art :: Picture
art = tree 10 (Point 400 800) (Vector 0 (-100)) red' 10 Dashed
  where
    red'        = Colour 255    0   0 10
tree :: Int -> Point -> Vector -> Colour -> Int -> LineStyle -> Picture
tree depth base direction colour opacity linestyle
  | depth == 0 = drawLine line linestyle
  | otherwise
    =  drawLine line linestyle
    ++ tree (depth - 1) nextBase left nextColour (opacity + 10) linestyle-- left tree
    ++ tree (depth - 1) nextBase right nextColour (opacity + 10) linestyle -- right tree
  where
    drawLine :: Line -> LineStyle -> Picture
    drawLine (Line start end) linestyle =
      [ Path [start, end] colour linestyle ]

    line = Line base nextBase
    nextBase = offset direction base

    left = rotate (-pi /22) $ scale 0.9 $ direction
    right = rotate (pi /22) $ scale 0.9 $ direction

    nextColour =
      colour { redC = (redC colour) - 24, blueC = (blueC colour) + 25, opacityC = opacity + 10}

-- Offset a point by a vector
offset :: Vector -> Point -> Point
offset (Vector vx vy) (Point px py)
  = Point (px + vx) (py + vy)

-- Scale a vector
scale :: Float -> Vector -> Vector
scale factor (Vector x y) = Vector (factor * x) (factor * y)

-- Rotate a vector (in radians)
rotate :: Float -> Vector -> Vector
rotate alpha (Vector vx vy)
  = Vector (cos alpha * vx - sin alpha * vy)
           (sin alpha * vx + cos alpha * vy)

-- use 'writeToFile' to write a picture to file "ex01.png" to test your
-- program if you are not using Haskell for Mac
-- e.g., call
-- writeToFile [house, door]

writeToFile pic
  = writePng "art.png" (drawPicture 3 art)
