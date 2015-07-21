module Lib (grid20) where

import           Data.Array

data Direction
  = Horizontal
  | Vertical
  | Diagonal
  deriving (Eq,Ord,Show)

type Grid = Array (Int, Int) Int

initialGrid :: Grid
initialGrid =
  array ((0,0),(19,19))
        [((x,y),x + y) | x <- [0 .. 19]
                       , y <- [0 .. 19]]

next :: Direction -> (Int,Int) -> (Int,Int)
next Horizontal (x,y) = (x + 1,y)
next Vertical (x,y) = (x,y + 1)
next Diagonal (x,y) = (x + 1,y + 1)

slice :: Grid -> Int -> (Int,Int,Direction) -> [Int]
slice _ 0 _ = []
slice grid count (x,y,direction) =
  (grid !
   (x,y)) :
  slice grid
        (count - 1)
        (x',y',direction)
  where (x',y') = next direction (x,y)

possibleValues :: [(Int, Int, Direction)]
possibleValues =
  [(x,y,Horizontal) | x <- [0 .. 16]
                    , y <- [0 .. 19]] ++
  [(x,y,Vertical) | x <- [0 .. 19]
                  , y <- [0 .. 16]] ++
  [(x,y,Diagonal) | x <- [0 .. 16]
                  , y <- [0 .. 16]]

solution :: Int
solution =
  maximum $
  fmap (product .
        slice initialGrid 4)
       possibleValues

grid20 :: IO ()
grid20 = print solution
