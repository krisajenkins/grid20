module Lib (grid20) where

import           Data.Array
import           Data.Function
import           Data.List
import           Data.Set      (Set)
import qualified Data.Set      as Set
import           System.Random
import           Text.Printf

data Direction
  = Horizontal
  | Vertical
  | Diagonal
  deriving (Eq,Ord,Show)

type Grid = Array (Int, Int) Integer
type Signpost = (Int,Int,Direction)

makeGrid :: StdGen -> Grid
makeGrid stdGen =
  array ((0,0),(19,19))
        (zip positions values)
  where positions =
          [(x,y) | x <- [0 .. 19]
                 , y <- [0 .. 19]]
        values = randomRs (0,100) stdGen

prettyRow :: Grid -> Set (Int,Int) -> Int -> String
prettyRow grid highlighted row =
  concat [printf (format col) $
          grid !
          (col,row) | col <- [0 .. 19]]
  where format col =
          if (Set.member (row,col) highlighted)
             then "\ESC[41m%4d\ESC[40m"
             else "%4d"

prettyGrid :: Grid -> Set (Int,Int) -> String
prettyGrid grid highlighted =
  unlines [prettyRow grid highlighted row | row <-
                                             [0 .. 19]]

nextPosition :: Signpost -> Signpost
nextPosition (x,y,d@Horizontal) = (x + 1,y,d)
nextPosition (x,y,d@Vertical) = (x,y + 1,d)
nextPosition (x,y,d@Diagonal) = (x + 1,y + 1,d)

slice :: Grid -> Int -> Signpost -> [Integer]
slice _ 0 _ = []
slice grid count signpost@(x,y,_) =
  (grid ! (x,y)) :
  slice grid
        (count - 1)
        (nextPosition signpost)

possibleValues :: [Signpost]
possibleValues =
  [(x,y,Horizontal) | x <- [0 .. 16]
                    , y <- [0 .. 19]] ++
  [(x,y,Vertical) | x <- [0 .. 19]
                  , y <- [0 .. 16]] ++
  [(x,y,Diagonal) | x <- [0 .. 16]
                  , y <- [0 .. 16]]

findSolution :: Grid -> (Signpost, Integer)
findSolution grid =
  maximumBy (compare `on` snd) $
  fmap resultAt possibleValues
  where resultAt s = (s, product $ slice grid 4 s)

grid20 :: IO ()
grid20 =
  do stdGen <- newStdGen
     let grid = makeGrid stdGen
         solution = findSolution grid
     print solution
     putStrLn $ prettyGrid grid Set.empty
