import AdventOfCode (readInputFile)

import Data.Foldable (for_)
import Data.List (scanl')
import Numeric (readHex)

area :: [(Int, Int)] -> Int
area digs = (internalArea ys xs + perimeter ys + perimeter xs) `quot` 2 + 1
  where (ys, xs) = unzip (scanl' step (0, 0) digs)

step :: (Int, Int) -> (Int, Int) -> (Int, Int)
step (a, b) (c, d) = (a + c, b + d)

internalArea :: [Int] -> [Int] -> Int
internalArea xs ys = abs (sumRot xs ys - sumRot ys xs)
  where sumRot xs' ys' = sum (zipWith (*) xs' (drop 1 (cycle ys')))

perimeter :: [Int] -> Int
perimeter xs = sum (map abs (zipWith (-) xs (drop 1 xs)))

dig :: String -> (Int, Int)
dig s = case words s of
  ["R", n, _] -> (0, read n)
  ["L", n, _] -> (0, -read n)
  ["U", n, _] -> (-read n, 0)
  ["D", n, _] -> (read n, 0)
  _ -> error ("bad dig " ++ show s)

dig2 :: String -> (Int, Int)
dig2 s = case words s of
  [_, _, ['(', '#', a, b, c, d, e, f, ')']] -> case (readHex [a, b, c, d, e], f) of
    ([(x, "")], '0') -> (0, x)
    ([(x, "")], '1') -> (x, 0)
    ([(x, "")], '2') -> (0, -x)
    ([(x, "")], '3') -> (-x, 0)
    ([(_, "")], _) -> error ("bad hexdir " ++ [c])
    ([(_, s')], _) -> error ("unparsed " ++ s')
    ([], _) -> error "no parse"
    (_:_:_, _) -> error "too many parses"
  _ -> error ("bad dig2 " ++ show s)

main :: IO ()
main = do
  s <- readInputFile
  for_ [dig, dig2] (\f -> print (area (map f (lines s))))
