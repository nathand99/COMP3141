module Ex01 where

-- needed to display the picture in the playground
import Codec.Picture

-- our line graphics programming interface
import ShapeGraphics
import Test.QuickCheck
-- Part 1
-- picture of a house
housePic :: Picture
housePic = [door, house]
  where
    house :: PictureObject
    house = Path (map convert houseCOs) green Solid
    door :: PictureObject
    door =  Path (map convert doorCOs) red Solid
    
-- function to convert co-ordinates to Point
convert :: (Float, Float) -> Point
convert (x,y) = Point x y
-- chimneyCOs added in
houseCOs :: [(Float, Float)]
houseCOs = [(300, 750), (300, 450), (270, 450), (500, 200), (615, 325), (615, 250), (650, 250), (650, 363),
         (730, 450), (700, 450), (700, 750)]

doorCOs :: [(Float, Float)]
doorCOs = [(420, 750), (420, 550), (580, 550), (580, 750)]

grey :: Colour
grey = Colour 255 255 255 128

smoke :: PictureObject
smoke = Path  (map convert smokeCOs)
              grey
              Solid
-- chimneyHouse uses our house from above and adds smoke to it (order important in definition)
chimneyHouse :: Picture
chimneyHouse = smoke : housePic

smokeCOs :: [(Float, Float)]
smokeCOs = [(635, 240), (625, 230), (635, 220), (625, 210)]

-- Part 2
movePoint :: Point -> Vector -> Point
movePoint (Point x y) (Vector xv yv)
  = Point (x + xv) (y + yv)

movePictureObject :: Vector -> PictureObject -> PictureObject
-- circle
movePictureObject vec (Circle centrePO radiusPO colourPO linestylePO fillstylePO) 
  = Circle (movePoint centrePO vec) radiusPO colourPO linestylePO fillstylePO
-- ellipse
movePictureObject vec (Ellipse centrePO widthPO heightPO rotationPO colourPO linestylePO fillstylePO) 
  = Ellipse (movePoint centrePO vec) widthPO heightPO rotationPO colourPO linestylePO fillstylePO
-- path
movePictureObject vec (Path points colour lineStyle) 
  = Path (map (\p -> movePoint p vec) points) colour lineStyle
-- polygon
movePictureObject vec (Polygon points colour lineStyle fillStyle) 
  = Polygon (map ((flip movePoint) vec) points) colour lineStyle fillStyle

{- it would make more sense to have movePoint :: Vector -> (Point -> Point)
this way messes up the argument functions. In path and polygon - there are 2 ways around this -
both mean the same thing
better definition: 
movePoint' :: Vector -> Point -> Point
movePoint' (Vector xv yv) (Point x y)
  = Point (x + xv) (y + yv)
  or
  Path (map moved points)
  where
    moved point = movePoint point vec
-}

-- Part 3

-- generate the picture consisting of circles:
-- [Circle (Point 400 400) (400/n) col Solid SolidFill,
--  Circle (Point 400 400) 2 * (400/n) col Solid SolidFill,
--  ....
--  Circle (Point 400 400) 400 col Solid SolidFill]
-- = map circle [1, 2, 3 ...
-- use EnumFromThenTo (1, 2, n)
simpleCirclePic :: Colour -> Float -> Picture
simpleCirclePic col n = map circle $ enumFromThenTo 1 2 n
  where
    circle level = Circle (Point 400 400) (level * (400/n)) col Solid SolidFill
-- its clearer passing down level to go into (level * (400/n)) than putting it all on the same line

-- use 'writeToFile' to write a picture to file "ex01.png" to test your
-- program if you are not using Haskell for Mac
-- e.g., call
-- writeToFile [house, door]
writeToFile pic
  = writePng "ex01.png" (drawPicture 3 pic)



-- QUIZ



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
prop3 f x = rot13 (map f x) == map f (rot13 x)
prop4 x = all (not . isAlpha) x ==> rot13 x == x
prop5 a b = rot13 (a ++ b) == rot13 a ++ rot13 b
prop6 x = not (null x) ==> ord (head x) + 13 == ord (head (rot13 x)) 
prop7 x = rot13 (rot13 x) == x