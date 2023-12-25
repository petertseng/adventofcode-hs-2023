import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOnOne)

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.List (minimumBy, partition)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as Set

type Component = String

tryPartition :: IntMap [Int] -> Int -> Maybe (IntSet, Set (Int, Int))
tryPartition neigh seed = try (IntSet.singleton seed) (Set.fromList [(seed, v) | v <- neigh IntMap.! seed])
  where try left bridge | Set.size bridge == 3 = Just (left, bridge)
        try _ bridge | Set.size bridge < 3 = Nothing
        try left bridge = try (IntSet.insert add left) (Set.union (bridge Set.\\ dels) adds)
          where add = minimumBy (comparing (\u -> sum [if v `IntSet.member` left then -1 else 1 | v <- neigh IntMap.! u] :: Int)) (Set.toList (Set.map snd bridge))
                (inLeft, notInLeft) = partition (`IntSet.member` left) (neigh IntMap.! add)
                adds = Set.fromList ([(add, v) | v <- notInLeft])
                dels = Set.fromList ([(v, add) | v <- inLeft])

comp :: String -> [(Component, Component)]
comp s = [(l, r) | r <- words rs]
  where (l, rs) = splitOnOne ':' s

assignIds :: Ord a => Map a [a] -> IntMap [Int]
assignIds adj = IntMap.fromAscList [(ids Map.! k, map (ids Map.!) vs) | (k, vs) <- Map.assocs adj]
  where ids = Map.fromAscList (zip (Map.keys adj) [0..])

bidir :: Ord a => [(a, a)] -> Map a [a]
bidir xs = Map.fromListWith (++) (concat [[(a, [b]), (b, [a])] | (a, b) <- xs])

main :: IO ()
main = do
  s <- readInputFile
  let neigh = assignIds (bidir (concatMap comp (lines s)))
      nodes = IntMap.keys neigh
      solns = mapMaybe (tryPartition neigh) nodes
      (left, _bridge) = head solns
  print (IntSet.size left * (length nodes - IntSet.size left))
