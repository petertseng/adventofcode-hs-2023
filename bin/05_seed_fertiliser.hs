import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOn)

import Control.Arrow (first)
import Data.List (foldl', isSuffixOf, mapAccumL, sort)

type Interval = (Int, Int)

loc :: [[(Interval, Int)]] -> Interval -> [Interval]
loc = flip (foldl' stepMap . (: []))

stepMap :: [Interval] -> [(Interval, Int)] -> [Interval]
stepMap currents ranges = concatMap (step1 ranges) currents

step1 :: [(Interval, Int)] -> Interval -> [Interval]
--step1 ranges current = map shift intersected ++ unintersected
step1 ranges current = mergeSorted (sort (map shift intersected ++ unintersected))
  where intersected :: [(Interval, Int)]
        intersected = filter (not . interEmpty . fst) (map (first (intersect current)) ranges)
        unintersected = current `intervalMinus` map fst intersected
        shift ((a, b), d) = (a + d, b + d)

pairs :: [a] -> [(a, a)]
pairs (x:y:xs) = (x, y) : pairs xs
pairs [_] = error "odd elements"
pairs [] = []

seedLine :: String -> [Int]
seedLine s = case words s of
  "seeds:":xs -> map read xs
  _ -> error ("bad seeds" ++ s)

aToBMap :: [String] -> [(Interval, Int)]
aToBMap ls = case ls of
  (l:ls') | "map:" `isSuffixOf` l -> map mapLine ls'
    where mapLine :: String -> (Interval, Int)
          mapLine s = case words s of
            [a, b, c] -> let b' = read b in ((b', b' + read c - 1), read a - b')
            _ -> error ("bad map " ++ show ls)
  _ -> error ("bad map " ++ show ls)

mergeSorted :: [Interval] -> [Interval]
mergeSorted [] = []
mergeSorted ((min1, max1):(min2, max2):xs) | succ max1 >= min2 = mergeSorted ((min1, max max1 max2) : xs)
mergeSorted (x:xs) = x : mergeSorted xs

intervalMinus :: Interval -> [Interval] -> [Interval]
intervalMinus (ll, lr) rs = addLast (filter (not . interEmpty) maybeUnintersected)
  where (lastUnintersected, maybeUnintersected) = mapAccumL advanceUnintersected ll (sort rs)
        advanceUnintersected :: Int -> Interval -> (Int, Interval)
        advanceUnintersected ul (l, r) = (max (r + 1) ul, (ul, min lr (l - 1)))
        addLast = if lastUnintersected <= lr then ((lastUnintersected, lr) :) else id

intersect :: Interval -> Interval -> Interval
intersect (a, b) (c, d) = (max a c, min b d)

interEmpty :: Interval -> Bool
interEmpty (a, b) = a > b

main :: IO ()
main = do
  s <- readInputFile
  let (seeds, maps) = case splitOn "" (lines s) of
        [s']:m -> (seedLine s', map aToBMap m)
        _ -> error ("bad seeds/maps " ++ s)
      bestLoc f = fst . minimum . concatMap (loc maps . f)
  print (bestLoc (\v -> (v, v)) seeds)
  print (bestLoc (\(a, b) -> (a, a + b - 1)) (pairs seeds))
