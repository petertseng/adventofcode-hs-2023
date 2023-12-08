import AdventOfCode (readInputFile)

import Control.Arrow (second)
import Data.List (findIndices, foldl', isSuffixOf)
import Data.Map (Map)
import qualified Data.Map as Map

type Node = String

-- The infinite list of nodes visited
path :: [Map Node Node] -> Node -> [Node]
path dirs start = scanl (flip (Map.!)) start (cycle dirs)

-- The infinite list of step counts at which an end node is visited
pathLens :: [Map Node Node] -> (Node -> Bool) -> Node -> [Int]
pathLens dirs end start = findIndices end (path dirs start)

periodAndMin :: [Int] -> (Int, Int)
periodAndMin lens
  | any (/= diff) diffs = error ("unequal diffs " ++ show diffs)
  | head lens `rem` diff /= 0 = error ("nonzero offset " ++ show (head lens, diff))
  | otherwise = (diff, head lens)
  where diffs = zipWith (-) (drop 1 lens) (take 5 lens)
        diff = head diffs

dir :: Char -> (a, a) -> a
dir 'L' = fst
dir 'R' = snd
dir c = error ("bad dir " ++ [c])

node :: String -> (Node, (Node, Node))
node [n1, n2, n3, ' ', '=', ' ', '(', l1, l2, l3, ',', ' ', r1, r2, r3, ')'] = ([n1, n2, n3], ([l1, l2, l3], [r1, r2, r3]))
node s = error ("bad node " ++ s)

ceilDiv :: Int -> Int -> Int
a `ceilDiv` b = -(a `div` (-b))

main :: IO ()
main = do
  s <- readInputFile
  let (dirs, nodes) = case lines s of
        (ds:"":n) -> (map dir ds, map node n)
        _ -> error ("bad map " ++ s)
      network = (Map.fromList (map (second fst) nodes), Map.fromList (map (second snd) nodes))
      dirMaps = map ($ network) dirs
  print (head (pathLens dirMaps (== "ZZZ") "AAA"))

  let as = filter ("A" `isSuffixOf`) (map fst nodes)
      (periods, mins) = unzip (map (periodAndMin . pathLens dirMaps ("Z" `isSuffixOf`)) as)
      period = foldl' lcm 1 periods
      min' = maximum mins
      t = period + 0 -- would be some other number if there were offsets
  print (if t >= min' then t else t + ((min' - t) `ceilDiv` period) * period)
