-- | Contains experiments for gathering data
module Experiments (runExpirements) where

import Data.Bifunctor (second)
import Data.List (intercalate)
import Parse
import System.Timeout
import Text.Printf
import System.Random

experimentFiles :: [String]
experimentFiles =
  map
    ("examples/" ++)
    [ "E.gcl",
      "S1.gcl",
      "min.gcl",
      "minind.gcl",
      "reverse.gcl",
      "swap.gcl",
      "test.gcl"
    ]

benchmarkFiles :: [String]
benchmarkFiles =
  map
    ("examples/benchmark/" ++)
    [ "divByN.gcl",
      "memberOf.gcl",
      "pullUp.gcl"
    ]

functions :: [(String, Int -> Int -> IO Bool)]
functions =
  [ (" always f ", \_ _ -> return True),
    (" random",
      \c m -> do
        number <- randomRIO (1, m)
        return $ number > c
    ),
    (" half",
      \_ _ -> do
        number <- randomRIO (1:: Int, 2)
        return $ number == 1
    ),
    (" quarter",
      \_ _ -> do
        number <- randomRIO (1:: Int, 4)
        return $ number == 1
    ),
    (" eight",
      \_ _ -> do
        number <- randomRIO (1:: Int, 8)
        return $ number == 1
    )
  ]

timeoutMS :: Int
timeoutMS = 40 * 10 ^ (6 :: Int)

runExpirements :: IO ()
runExpirements = do
  putStrLn "hello experiments"
  let lengths = [30, 40, 45]
  runtimes <- experimentRunTimes functions benchmarkFiles lengths
  let r = map (second $ map showTime) runtimes
  toCsvFile "out.csv" (("lengths", map show lengths) : r)

experimentRunTimes :: [(String, Int -> Int -> IO Bool)] -> [String] -> [Int] -> IO [(String, [Maybe Double])]
experimentRunTimes fs files pathLengths =
  sequence
    [ do
        let name = file ++ (if noHeur then " no heuristics" else "") ++ label
        print name
        lst <- sequence [timeout timeoutMS (run f noHeur len False file) | len <- pathLengths]
        return (name, map (fmap runtime) lst)
      | file <- files,
        noHeur <- [True, False],
        (label, f) <- if noHeur then [("", \_ _ -> return True)] else fs
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
