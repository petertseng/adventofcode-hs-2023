import AdventOfCode (readInputFileAndFlags)

import Control.Arrow ((***))
import Data.Array.Unboxed ((!), (!?), UArray, assocs, listArray)
import Data.Bits (shiftL)
import qualified Data.IntSet as IntSet

reaches :: UArray Int Char -> Int -> Int -> [Int]
reaches garden width pos0 = reaches' IntSet.empty (IntSet.singleton pos0)
  where reaches' seen poses = IntSet.size poses' : reaches' seen' poses'
          where seen' = IntSet.union seen poses'
                poses' = IntSet.fromList ([npos
                  | pos <- IntSet.toList poses
                  , dpos <- [-width, -1, 1, width]
                  , let npos = pos + dpos
                  , abs dpos /= 1 || npos `quot` width == pos `quot` width
                  , npos `IntSet.notMember` seen
                  , maybe False (/= '#') (garden !? npos)
                  ])

reachesWrap :: UArray Int Char -> Int -> Int -> Int -> [Int]
reachesWrap garden height width0 pos0 = reaches' IntSet.empty (IntSet.singleton (y0 * width + x0 + (width0 `shiftL` 29)))
  where reaches' seen poses = IntSet.size poses' : reaches' seen' poses'
          where seen' = IntSet.union seen poses'
                poses' = IntSet.fromList ([npos
                  | pos <- IntSet.toList poses
                  , dpos <- [-width, -1, 1, width]
                  , let npos = pos + dpos
                  , let (ny, nx) = npos `divMod` width
                  , npos `IntSet.notMember` seen
                  , garden ! (ny `mod` height * width0 + nx `mod` width0) /= '#'
                  ])
        width = width0 `shiftL` 30
        (y0, x0) = pos0 `divMod` width0

one :: [a] -> a
one [x] = x
one [] = error "none"
one (_:_) = error "too many"

uniform :: Eq b => (a -> b) -> [a] -> b
uniform _ [] = error "empty uniform"
uniform f (x:xs) | any ((/= f x) . f) xs = error "inconsistent uniform"
uniform f (x:_) = f x

everyOther :: [a] -> [a]
everyOther [] = []
everyOther [x] = [x]
everyOther (x:_:xs) = x : everyOther xs

main :: IO ()
main = do
  (s, flags) <- readInputFileAndFlags
  let height = length (lines s)
      width = uniform length (lines s)
      garden = listArray (0, height * width - 1) (concat (lines s))
      start = fst (one (filter ((== 'S') . snd) (assocs garden)))
      steps1 = maybe 64 read (lookup 'n' flags)
      steps2 = maybe 26501365 read (lookup 'n' flags)
      odds = everyOther
      evens = everyOther . drop 1
      reach f n = sum ((if odd n then odds else evens) (take n (f start)))
  print (reach (reaches garden width) steps1)
  if steps2 <= width then
    print (reach (reachesWrap garden width height) steps2)
  else do
    let sidelen = if height == width then width else error ("non-square " ++ show (height, width))
        (t, x0) = (if sidelen < 100 then subtract 4 *** (+ (sidelen * 4)) else id) (steps2 `quotRem` sidelen)
        rw = reach (reachesWrap garden width height)
        y0 = rw x0
        y1 = rw (x0 + sidelen)
        y2 = rw (x0 + sidelen * 2)
        vel = y1 - y0
        accel = y2 - 2 * y1 + y0
    print (y0 + vel * t + accel * t * (t - 1) `quot` 2)
