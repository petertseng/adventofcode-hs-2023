import AdventOfCode (readInputFile)

import Data.Array.Unboxed ((!), UArray, bounds, inRange, listArray)
import Data.Bits ((.|.), shiftL, shiftR)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

-- TODO: Have not implemented the loop compression and caching in Haskell.
-- It's a lot of work that I don't have the time for right now.

type Pos = (Int, Int)
newtype Dir = Dir (Int, Int) deriving (Eq, Ord)
type Contraption = UArray Pos Char

energised :: Contraption -> Pos -> Dir -> Int
energised cont p0 d0 = stepBeams IntSet.empty [(p0, d0)]
  where ((_, _), (_, maxX)) = bounds cont
        stepBeams :: IntSet -> [(Pos, Dir)] -> Int
        stepBeams seen [] = IntSet.size (IntSet.map (`shiftR` 4) seen)
        stepBeams seen beams = stepBeams seen' beams'
          where beams' = concatMap (filter (inRange (bounds cont) . fst) . stepBeam) beams
                seen' = IntSet.union seen (IntSet.fromList (map cacheKey beams))
                -- only need to filter on notSeen on splitters
                notSeen = (`IntSet.notMember` seen') . cacheKey
                stepBeam :: (Pos, Dir) -> [(Pos, Dir)]
                stepBeam (p, d@(Dir (dy, dx))) = case cont ! p of
                  '.' -> [step p d]
                  '|' | dx == 0 -> filter notSeen [step p d]
                  '|' -> filter notSeen [step p (Dir (-1, 0)), step p (Dir (1, 0))]
                  '-' | dy == 0 -> filter notSeen [step p d]
                  '-' -> filter notSeen [step p (Dir (0, -1)), step p (Dir (0, 1))]
                  -- -1 0 <-> 0 1, 1 0 <-> 0 -1
                  '/' -> [step p (Dir (-dx, -dy))]
                  -- -1 0 <-> 0 -1, 1 0 <-> 0 1
                  '\\' -> [step p (Dir (dx, dy))]
                  c -> error ("bad " ++ [c])
        -- Compressing to Int and using IntSet is about a 3x speedup over Set (Pos, Dir)
        cacheKey ((y, x), Dir (dy, dx)) = (y * (maxX + 1) + x) `shiftL` 4 .|. (dy + 1) `shiftL` 2 .|. (dx + 1)

step :: Pos -> Dir -> (Pos, Dir)
step (y, x) d@(Dir (dy, dx)) = ((y + dy, x + dx), d)

uniform :: Eq b => (a -> b) -> [a] -> b
uniform _ [] = error "empty uniform"
uniform f (x:xs) | any ((/= f x) . f) xs = error "inconsistent uniform"
uniform f (x:_) = f x

grid :: String -> Contraption
grid s = listArray ((1, 1), (h, w)) (concat ls)
  where ls = lines s
        h = length ls
        w = uniform length ls

main :: IO ()
main = do
  s <- readInputFile
  let contraption = grid s
      (_, (height, width)) = bounds contraption
  print (energised contraption (1, 1) (Dir (0, 1)))

  let up    = [((height, x), Dir (-1, 0)) | x <- [1 .. width]]
      down  = [((1,      x), Dir ( 1, 0)) | x <- [1 .. width]]
      left  = [((y,  width), Dir (0, -1)) | y <- [1 .. height]]
      right = [((y,      1), Dir (0,  1)) | y <- [1 .. height]]
  print (maximum (map (uncurry (energised contraption)) (up ++ down ++ left ++ right)))
