import AdventOfCode (readInputFile)

import Data.Char (isDigit)
import Data.List (foldl')

waysToWin :: Int -> Int -> Int
waysToWin t d = r - l
  where l = bsearch win 1 m
        r = bsearch (not . win) m t
        m = t `quot` 2
        win h = h * (t - h) > d

bsearch :: (Int -> Bool) -> Int -> Int -> Int
bsearch f low high
  | low > high = high
  | f mid = bsearch f low (mid - 1)
  | otherwise = bsearch f (mid + 1) high
  where mid = low + ((high - low) `div` 2)

nums :: String -> String -> ([Int], Int)
nums label s = case words s of
  l:ws | l == label -> (map read ws, read (filter isDigit (unwords ws)))
  _ -> error ("bad nums " ++ s)

main :: IO ()
main = do
  s <- readInputFile
  let ((ts, t), (ds, d)) = case lines s of
        [l1, l2] -> (nums "Time:" l1, nums "Distance:" l2)
        _ -> error "bad time/dists"
  let races = zip ts ds
  print (foldl' (*) 1 (map (uncurry waysToWin) races))
  print (waysToWin t d)
