import AdventOfCode (readInputFile)
import AdventOfCode.Search (bfs)

import Control.Arrow (second)
import Control.Monad.State (State, execState, get, modify)
import Control.Monad (when)
import Data.Array.Unboxed ((!), (!?), UArray, array)
import Data.Containers.ListUtils (nubOrdOn)
import Data.Either (rights)
import Data.Foldable (for_)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import Data.List (sortOn)
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set

type Pos = (Int, Int)

hike :: IntMap [(Int, Int)] -> Int -> Int -> Int -> Int
hike interNeigh adjust start goal = execState (hike' start adjust totalDist IntSet.empty) 0
  where hike' :: Int -> Int -> Int -> IntSet -> State Int ()
        hike' pos dist _ _ | pos == goal = modify (max dist)
        hike' pos _ _ seen | pos `IntSet.member` seen = return ()
        hike' pos dist remain seen = do
          best <- get
          when (dist + remain > best) $
            for_ (interNeigh IntMap.! pos) (\(d, n) -> hike' n (dist + d) remain' seen')
          where seen' = IntSet.insert pos seen
                remain' = remain - sum (map fst (filter ((`IntSet.notMember` seen) . snd) (interNeigh IntMap.! pos)))
        totalDist = sum [dist | (_, _, dist) <- edges] - adjust
        edges = nubOrdOn (\(u, v, _) -> (min u v, max u v)) [(u, v, d) | (u, vs) <- IntMap.assocs interNeigh, (d, v) <- vs]

neigh :: UArray Pos Char -> Bool -> Pos -> [Pos]
neigh trails slippery (y, x) = [(y + dy, x + dx) | (dy, dx) <- [(-1, 0), (0, -1), (0, 1), (1, 0)]
  , maybe False (/= '#') (trails !? (y + dy, x + dx))
  , not slippery || slipOK (dy, dx) (trails ! (y, x))
  ]
  where slipOK dir '^' = dir == (-1, 0)
        slipOK dir 'v' = dir == (1, 0)
        slipOK dir '<' = dir == (0, -1)
        slipOK dir '>' = dir == (0, 1)
        slipOK _ '.' = True
        slipOK dir c = error ("slipOK will not be called on " ++ show (dir, c))

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

uniform :: Eq b => (a -> b) -> [a] -> b
uniform _ [] = error "empty uniform"
uniform f (x:xs) | any ((/= f x) . f) xs = error "inconsistent uniform"
uniform f (x:_) = f x

enumGrid :: [[a]] -> [(Pos, a)]
enumGrid = concat . zipWith enumRow [0..]
  where enumRow y = zipWith (\x cell -> ((y, x), cell)) [0..]

main :: IO ()
main = do
  s <- readInputFile
  let g = enumGrid (lines s)
      height = length (lines s)
      width = uniform length (lines s)
      compress (y, x) = y * width + x
      trails = array ((0, 0), (height - 1, width - 1)) g
      intersections = [(y, x) | y <- [0 .. height - 1], x <- [0 .. width - 1]
                     , trails ! (y, x) /= '#'
                     , count ((/= '#') . (trails !)) (neigh trails False (y, x)) >= 3
                     ]
      goals = Set.fromAscList (intersections ++ [(height - 1, width - 2)])
      neigh' _ start pos | pos /= start && pos `Set.member` goals = []
      neigh' slippery _ pos = neigh trails slippery pos
      bfsFrom slippery start = rights (bfs (neigh' slippery start) (\pos -> pos /= start && pos `Set.member` goals) start)
      interNeigh slippery = IntMap.fromAscList [(compress start, sortOn (negate . fst) (map (second compress) (bfsFrom slippery start))) | start <- (0, 1) : intersections]
      (adjust, goal) = case bfsFrom False (height - 1, width - 2) of
        [v] -> v
        (_:_) -> (0, (height - 1, width - 2))
        [] -> error "no way to the goal"
  for_ [True, False] $ \slip -> print (hike (interNeigh slip) adjust (compress (0, 1)) (compress goal))
