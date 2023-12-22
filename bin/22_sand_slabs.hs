import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOn, splitOnOne)

import Data.Containers.ListUtils (nubOrd)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (foldl', sort)
import Data.Maybe (fromMaybe)

type Interval = (Int, Int)
type Box = (Interval, Interval, Interval)

fall :: Int -> Int -> [Box] -> IntMap [Int]
fall width xmin = snd . foldl' fall' (IntMap.empty, IntMap.empty) . zip [1..] . sort
  where fall' (highestAt, below) (i, ((z1, z2), (y1, y2), (x1, x2))) = (highestAt', below')
          where highestAt' = foldl' (\ha xy -> IntMap.insert xy (z2', i) ha) highestAt xys
                z1' = 1 + maximum (0 : [hgt | Just (hgt, _) <- map (`IntMap.lookup` highestAt) xys])
                z2' = z1' + z2 - z1
                belows = nubOrd [bel | Just (hgt, bel) <- map (`IntMap.lookup` highestAt) xys, hgt == z1' - 1]
                -- if adding (i, []) into below, then howManyFall needs to special-case [],
                -- since all undefined [] is True (empty conjunction)
                below' = if null belows then below else IntMap.insertWith (++) i belows below
                xys = [y * width + x - xmin | y <- [y1 .. y2], x <- [x1 .. x2]]

-- This checks all elements in fallen.
-- You'd think it'd be possible to improve by only checking bricks above seed,
-- but it didn't improve runtime.
-- Note that a transitive closure is needed to calculate all bricks above seed,
-- not just immediately above.
-- Can be implemeneted with State, any other way?
howManyFall :: IntMap [Int] -> Int -> Int
howManyFall below seed = count id (IntMap.elems fallen) - 1
  where fallen = IntMap.insert seed True (IntMap.map belowsGone below)
        -- for some reason, findWithDefault is 20% slower???
        --belowsGone = all (\x -> IntMap.findWithDefault False x fallen)
        belowsGone = all (fromMaybe False . (`IntMap.lookup` fallen))

-- z first so that I can use natural sort order
sand :: String -> Box
sand s = ((z1, z2), (y1, y2), (x1, x2))
  where (l, r) = splitOnOne '~' s
        (x1, y1, z1) = coord l
        (x2, y2, z2) = coord r
        coord s' = case splitOn ',' s' of
          [a, b, c] -> (read a, read b, read c)
          _ -> error ("bad coord " ++ s')

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

main :: IO ()
main = do
  s <- readInputFile
  let bricks = map sand (lines s)
      xs = concatMap (\(_, _, (x1, x2)) -> [x1, x2]) bricks
      xmin = minimum xs
      width = maximum xs - xmin + 1
      below = fall width xmin bricks
      fallIfDisinteg = map (howManyFall below) [1 .. length bricks]
  print (count (== 0) fallIfDisinteg)
  print (sum fallIfDisinteg)
