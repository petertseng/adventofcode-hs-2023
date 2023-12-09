import AdventOfCode (readInputFile)

import Data.Foldable (for_)

extrap :: [Int] -> Int
extrap xs = sumBy head (takeWhile (any (/= 0)) (iterate diff xs))

diff :: [Int] -> [Int]
diff xs = zipWith (-) xs (drop 1 xs)

sumBy :: (a -> Int) -> [a] -> Int
sumBy f = sum . map f

main :: IO ()
main = do
  s <- readInputFile
  let hists = map (map read . words) (lines s)
  for_ [reverse, id] $ \f -> print (sumBy (extrap . f) hists)
