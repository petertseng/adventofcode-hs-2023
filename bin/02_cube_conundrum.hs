import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOn, splitOnOne)

import Data.List (foldl')

type Game = (Int, Int, Int, Int)

possible :: Game -> Bool
possible (_, r, g, b) = r <= 12 && g <= 13 && b <= 14

gameId :: Game -> Int
gameId (i, _, _, _) = i

power :: Game -> Int
power (_, r, g, b) = r * g * b

game :: String -> Game
game s = (i, red, green, blue)
  where (l, r) = splitOnOne ':' s
        i = case words l of
                   ["Game", g] -> read g
                   _ -> error ("bad game " ++ l)
        handfuls = concatMap (splitOn ',') (splitOn ';' r)
        (red, green, blue) = foldl' cubes (0, 0, 0) handfuls

cubes :: (Int, Int, Int) -> String -> (Int, Int, Int)
cubes (r, g, b) s = case words s of
  [n, "red"] -> (max r (read n), g, b)
  [n, "green"] -> (r, max g (read n), b)
  [n, "blue"] -> (r, g, max b (read n))
  _ -> error ("bad cubes " ++ s)

sumBy :: (a -> Int) -> [a] -> Int
sumBy f = sum . map f

main :: IO ()
main = do
  s <- readInputFile
  let games = map game (lines s)
  print (sumBy gameId (filter possible games))
  print (sumBy power games)
