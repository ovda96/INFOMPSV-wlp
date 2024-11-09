-- | Contains experiments for gathering data
module Experiments (runExpirements) where

import Data.Bifunctor (second)
import Data.List (intercalate)
import Parse
import Text.Printf
import System.Timeout

experimentFiles :: [String]
experimentFiles =
  map
    ("examples/" ++)
    [ "E.gcl",
      "S1.gcl",
      "memberOf.gcl",
      -- "min.gcl",
      -- "minind.gcl",
      -- "reverse.gcl",
      -- "swap.gcl",
      "test.gcl"
    ]

timeoutMS :: Int
timeoutMS = 40*10^(6 :: Int)

runExpirements :: IO ()
runExpirements = do
  putStrLn "hello experiments"
  let lengths = [10, 30 .. 100]
  runtimes <- experimentRunTimes experimentFiles lengths
  let r = map (second $ map showTime) runtimes
  toCsvFile "out.csv" (("lengths", map show lengths) : r)

experimentRunTimes :: [String] -> [Int] -> IO [(String, [Maybe Double])]
experimentRunTimes files pathLengths =
  sequence
    [ do
        let name = file ++ if noHeur then " no heuristics" else ""
        print name
        lst <- sequence [timeout timeoutMS (run noHeur len False file) | len <- pathLengths]
        return (name, map (fmap runtime) lst)
      | file <- files,
        noHeur <- [True, False]
    ]

showTime :: Maybe Double -> String
showTime (Just t) = printf "%f" t
showTime Nothing = printf "%d" timeoutMS

toCsvFile :: String -> [(String, [String])] -> IO ()
toCsvFile filename dat = writeFile filename (toCsv dat)

toCsv :: [(String, [String])] -> String
toCsv xs = csvMkLine labels ++ concat (combineLists dat csvMkLine)
  where
    (labels, dat) = unzip xs

csvMkLine :: [String] -> String
csvMkLine xs = intercalate "," xs ++ "\n"

combineLists :: [[a]] -> ([a] -> b) -> [b]
-- weird function that combines the i'th elements of each list
-- for example: combineLists ["abcd", "abcd","abcd","abcd"] id == ["aaaa","bbbb","cccc","dddd"]
combineLists xs f
  | not (any null xs) = heads : tails
  | otherwise = []
  where
    tails = combineLists (map tail xs) f
    heads = f $ map head xs
