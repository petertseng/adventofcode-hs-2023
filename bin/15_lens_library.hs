import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOn, splitOnOne)

import Control.Arrow (second)
import Data.Array (accumArray, assocs)
import Data.Char (ord)
import Data.List (dropWhileEnd, foldl', isSuffixOf)
import Data.Word (Word8)

data Op = Minus | Equal Int

moveLens :: [(String, Int)] -> (String, Op) -> [(String, Int)]
moveLens lenses (label, Minus) = filter ((/= label) . fst) lenses
moveLens lenses (label, Equal foc) = assoc label foc lenses

focal :: (Word8, [(String, Int)]) -> Int
focal (boxno, lenses) = (fromIntegral boxno + 1) * sum (zipWith focal' [1..] (map snd lenses))
  where focal' i foc = i * foc

-- Word8 means no need to `rem` 256.
hash :: String -> Word8
hash = foldl' h 0
  where h v c = (v + fromIntegral (ord c)) * 17

op :: String -> (String, Op)
op s | "-" `isSuffixOf` s = (init s, Minus)
op s = second (Equal . read) (splitOnOne '=' s)

assoc :: Eq a => a -> b -> [(a, b)] -> [(a, b)]
assoc k v = assoc'
  where assoc' [] = [(k, v)]
        assoc' ((k', _):kvs) | k == k' = (k, v) : kvs
        assoc' (kv:kvs) = kv : assoc' kvs

sumBy :: (a -> Int) -> [a] -> Int
sumBy f = sum . map f

main :: IO ()
main = do
  s <- readInputFile
  let ops = splitOn ',' (dropWhileEnd (== '\n') s)
  print (sumBy (fromIntegral . hash) ops)

  let boxes = accumArray moveLens [] (0, 255) [(hash label, (label, o)) | (label, o) <- map op ops]
  print (sumBy focal (assocs boxes))
