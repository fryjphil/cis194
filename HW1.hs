{-# OPTIONS_GHC -fno-warn-warnings-deprecations -fno-warn-unused-binds #-}
import CodeWorld

main :: IO ()
main = exercise3


-- Fill in the blanks! (When I say blanks, I mean undefineds)

-- Exercise 1

botCircle, topCircle, midCircle:: Color -> Picture
botCircle c = colored c (translated 0 (-2.5) (solidCircle 1))
midCircle c = colored c (translated 0   0 (solidCircle 1))
topCircle c = colored c (translated 0   2.5  (solidCircle 1))

frame :: Picture
frame = rectangle 2.5 7.5

-- trafficLight :: Bool -> Picture
-- trafficLight True  = botCircle green & midCircle yellow & topCircle black & frame
-- trafficLight False = botCircle black & midCircle yellow & topCircle red   & frame

trafficLight :: Char -> Picture
trafficLight c
  | c == 'R' = topCircle red & midCircle black & botCircle black & frame
  | c == 'Y' = topCircle black & midCircle yellow & botCircle black & frame
  | c == 'G' = topCircle black & midCircle black & botCircle green & frame
  | c == 'C' = topCircle red & midCircle yellow & botCircle black & frame


trafficController :: Double -> Picture
trafficController t
  | round (t) `mod` 4 == 0 = trafficLight 'R'
  | round (t) `mod` 4 == 1 = trafficLight 'C'
  | round (t) `mod` 4 == 2 = trafficLight 'G'
  | round (t) `mod` 4 == 3 = trafficLight 'Y'

trafficLightAnimation :: Double -> Picture
trafficLightAnimation = trafficController

exercise1 :: IO ()
exercise1 = animationOf trafficLightAnimation

-- Exercise 2

tree :: Integer -> Picture
tree 0 = blank
tree n = polyline [(0,0),(0,1)] & translated 0 1 (
  rotated (pi/10) (tree (n-1)) & rotated (- pi/10) (tree (n-1)))

growingCircle :: Double -> Picture
growingCircle t = colored yellow (solidCircle ((min t 2)/7))

bloomingTree :: Picture -> Integer -> Picture
bloomingTree b 0 = b
bloomingTree b n = polyline [(0,0),(0,1)] & translated 0 1 (
  rotated (pi/10) (bloomingTree b (n-1)) & rotated (- pi/10) (bloomingTree b (n-1)))


myAnimation:: Double -> Picture
myAnimation t = bloomingTree (growingCircle t) 8

exercise2 :: IO ()
exercise2 = animationOf(myAnimation)

-- Exercise 3

wall, ground, storage, box :: Picture
wall =    drawTile (grey 0.5) 1
ground =  drawTile yellow 1
storage = drawCircle black 0.3 & drawTile yellow 1
box =     drawTile brown 1


drawTile :: Color -> Double -> Picture
drawCircle :: Color -> Double -> Picture

drawTile c t = colored c (solidRectangle t t)
drawCircle c t = colored c (solidCircle t)

drawWall::Double -> Picture
drawGround::Double -> Picture
drawStorage::Double -> Picture
drawBox::Double -> Picture

drawWall t = drawTile yellow t
drawGround t = drawTile black t
drawStorage t = drawTile orange t
drawBox t = drawTile red t

drawElement::Integer->Picture

drawElement t
  | t == 1 = wall
  | t == 2 = ground
  | t == 3 = storage
  | t == 4 = box
  | otherwise = blank

pictureOfMaze :: Picture
drawRows:: Integer -> Picture
drawColumns:: Integer -> Integer -> Picture
drawTileAt:: Integer -> Integer -> Picture

drawRows 11 = blank
drawRows r = drawColumns r (-10) & drawRows (r+1)

drawColumns _ 11 = blank
drawColumns r c = drawTileAt r c  & drawColumns r (c+1)

drawTileAt r c = translated (fromIntegral r) (fromIntegral c) (drawElement (maze r c))


pictureOfMaze = drawRows(-11)

exercise3 :: IO ()
exercise3 = drawingOf pictureOfMaze

maze :: Integer -> Integer -> Integer
maze x y
  | abs x > 4  || abs y > 4  = 0
  | abs x == 4 || abs y == 4 = 1
  | x ==  2 && y <= 0        = 1
  | x ==  3 && y <= 0        = 3
  | x >= -2 && y == 0        = 4
  | otherwise                = 2
