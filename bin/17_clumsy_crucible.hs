import AdventOfCode (readInputFile)
import AdventOfCode.Search (astarInt)

import Data.Array.Unboxed ((!), UArray, bounds, inRange, listArray)
import Data.Bits ((.|.), shiftL)
import Data.Char (digitToInt)
import Data.Foldable (for_)
import Data.Maybe (fromJust)

type Pos = (Int, Int)
type Heat = UArray Pos Int

neigh :: Heat -> Int -> Int -> (Int, Int, Bool) -> [(Int, (Int, Int, Bool))]
neigh _ _ _ (0, 0, _) = [(0, (1, 1, True)), (0, (1, 1, False))] -- TODO: Maybe one day I make astar support multiple starts???
neigh g maxStraight minTurn (y0, x0, horiz)
  | horiz     = movesFor (0, -1) ++ movesFor (0, 1)
  | otherwise = movesFor (-1, 0) ++ movesFor (1, 0)
  where movesFor (dy, dx) = map turn (drop minTurn (takeWhile (inRange (bounds g) . snd) (take (maxStraight + 1) (iterate step (0, (y0, x0))))))
          where step (c, (y, x)) = let pos' = (y + dy, x + dx) in (c + g ! pos', pos')
        turn (c, (y, x)) = (c, (y, x, not horiz))

uniform :: Eq b => (a -> b) -> [a] -> b
uniform _ [] = error "empty uniform"
uniform f (x:xs) | any ((/= f x) . f) xs = error "inconsistent uniform"
uniform f (x:_) = f x

grid :: String -> Heat
grid s = listArray ((1, 1), (h, w)) (concat g)
  where g = map (map digitToInt) (lines s)
        h = length g
        w = uniform length g

main :: IO ()
main = do
  s <- readInputFile
  let g = grid s
      (gy, gx) = snd (bounds g)
      -- Manhattan has no benefit
      --heur (y, x, _) = gy - y + gx - x
      heur = const 0
      goal (y, x, _) = y == gy && x == gx
      compress (y, x, horiz) = (y * (gx + 1) + x) `shiftL` 1 .|. fromEnum horiz
  for_ [(3, 1), (10, 4)] $ \(a, b) -> print (fromJust (astarInt compress (neigh g a b) heur goal (0, 0, True)))
