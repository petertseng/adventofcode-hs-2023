import AdventOfCode (readInputFile)

import Data.Array.Unboxed ((!), (//), UArray, array, assocs)
import Data.Function (on)
import Data.List (groupBy, mapAccumL)
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set

type Pos = (Int, Int)
newtype Dir = Dir (Int, Int)
type Maze = UArray Pos Char

path :: Maze -> Pos -> Dir -> [Pos]
path m pos0 dir0 = pos0 : takeWhile (/= pos0) (map fst (drop 1 (iterate (step m) (pos0, dir0))))

step :: Maze -> (Pos, Dir) -> (Pos, Dir)
step m ((y, x), dir@(Dir (dy, dx))) = (pos', dir')
  where pos' = (y + dy, x + dx)
        c = m ! pos'
        dir' = case (c, dy, dx) of
          ('|', _, 0) -> dir
          ('|', _, _) -> error ("| while not vertical" ++ show (y, x, dy, dx))
          ('-', 0, _) -> dir
          ('-', _, _) -> error ("- while not horizontal" ++ show (y, x, dy, dx))
          ('L', 1, 0) -> Dir (0, 1)
          ('L', 0, -1) -> Dir (-1, 0)
          ('L', _, _) -> error ("L from wrong dir" ++ show (y, x, dy, dx))
          ('F', -1, 0) -> Dir (0, 1)
          ('F', 0, -1) -> Dir (1, 0)
          ('F', _, _) -> error ("F from wrong dir" ++ show (y, x, dy, dx))
          ('J', 1, 0) -> Dir (0, -1)
          ('J', 0, 1) -> Dir (-1, 0)
          ('J', _, _) -> error ("J from wrong dir" ++ show (y, x, dy, dx))
          ('7', -1, 0) -> Dir (0, -1)
          ('7', 0, 1) -> Dir (1, 0)
          ('7', _, _) -> error ("7 from wrong dir" ++ show (y, x, dy, dx))
          ('S', _, _) -> dir
          (_, _, _) -> error ("bad " ++ show (c, y, x))

inside :: Set Pos -> [(Pos, Char)] -> [Bool]
inside mainLoop = snd . mapAccumL inside' True
  where inside' :: Bool -> (Pos, Char) -> (Bool, Bool)
        inside' out (pos, c)
          -- Checking either |LJ or |F7 is sufficient
          -- (checking the top half or bottom half of every tile)
          | pos `Set.member` mainLoop = (out /= (c `elem` "|LJ"), False)
          | otherwise = (out, not out)

enumGrid :: [[a]] -> [(Pos, a)]
enumGrid = concat . zipWith enumRow [1..]
  where enumRow y = zipWith (\x cell -> ((y, x), cell)) [1..]

one :: [a] -> a
one [x] = x
one [] = error "none"
one (_:_) = error "too many"

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

main :: IO ()
main = do
  s <- readInputFile
  let grid = enumGrid (lines s)
      height = maximum (map (fst . fst) grid)
      width = maximum (map (snd . fst) grid)
      maze = array ((1, 1), (height, width)) grid
      coord letter = fst (one (filter ((== letter) . snd) grid))
      (y0, x0) = coord 'S'
      startConns = catMaybes [
          if y0 > 1      && maze ! (y0 - 1, x0) `elem` "|F7" then Just (-1, 0) else Nothing
        , if x0 > 1      && maze ! (y0, x0 - 1) `elem` "-FL" then Just (0, -1) else Nothing
        , if x0 < width  && maze ! (y0, x0 + 1) `elem` "-7J" then Just (0, 1) else Nothing
        , if y0 < height && maze ! (y0 + 1, x0) `elem` "|LJ" then Just (1, 0) else Nothing
        ]
      dir0 = Dir (head startConns)
      mainLoop = path maze (y0, x0) dir0
  print (length mainLoop `quot` 2)

  let startActsAs = case startConns of
        [(-1, 0), (0, -1)] -> 'J'
        [(-1, 0), (0, 1)] -> 'L'
        [(-1, 0), (1, 0)] -> '|'
        [(0, -1), (0, 1)] -> '-'
        [(0, -1), (1, 0)] -> '7'
        [(0, 1), (1, 0)] -> 'F'
        _ -> error ("bad start conns " ++ show startConns)
      maze' = maze // [((y0, x0), startActsAs)]
      rows = groupBy ((==) `on` (fst . fst)) (assocs maze')
      insides = map (inside (Set.fromList mainLoop)) rows
  print (count id (concat insides))
