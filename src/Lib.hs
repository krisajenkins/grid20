module Lib (grid20) where

import           Data.Array
import           System.Random

data Direction
  = Horizontal
  | Vertical
  | Diagonal
  deriving (Eq,Ord,Show)

type Grid = Array (Integer, Integer) Integer

makeGrid :: StdGen -> Grid
makeGrid g = array ((0,0),(19,19)) (zip positions values)
  where positions =
          [(x,y) | x <- [0 .. 19]
                 , y <- [0 .. 19]]
        values = randomRs (0,100) g

nextPosition :: Direction -> (Integer,Integer) -> (Integer,Integer)
nextPosition Horizontal (x,y) = (x + 1,y)
nextPosition Vertical (x,y) = (x,y + 1)
nextPosition Diagonal (x,y) = (x + 1,y + 1)

slice :: Grid -> Integer -> (Integer,Integer,Direction) -> [Integer]
slice _ 0 _ = []
slice grid count (x,y,direction) =
  (grid !
   (x,y)) :
  slice grid
        (count - 1)
        (x',y',direction)
  where (x',y') =
          nextPosition direction
                       (x,y)

possibleValues :: [(Integer, Integer, Direction)]
possibleValues =
  [(x,y,Horizontal) | x <- [0 .. 16]
                    , y <- [0 .. 19]] ++
  [(x,y,Vertical) | x <- [0 .. 19]
                  , y <- [0 .. 16]] ++
  [(x,y,Diagonal) | x <- [0 .. 16]
                  , y <- [0 .. 16]]

solution :: Grid -> Integer
solution grid =
  maximum $
  fmap (product .
        slice grid 4)
       possibleValues

grid20 :: IO ()
grid20 = do g <- newStdGen
            let grid = makeGrid g
            print $ solution grid
