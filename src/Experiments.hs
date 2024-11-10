-- | Experiments.hs: Contains experiments for gathering data
module Experiments
  ( runExpirements,
  )
where

--
import Data.Bifunctor (second)
import Data.List (intercalate)
import GCLParser.GCLDatatype
import GCLUtils (parseGCLfile)
import Parse
import PathTree
import System.Random (randomRIO)
import System.Timeout (timeout)
import Text.Printf (printf)

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

-- different feasibility checking strategys to expirement with
functions :: [(String, Int -> Int -> IO Bool)]
functions =
  [ (" always check ", \_ _ -> return True),
    ( " chance (m-c)/m",
      \c m -> do
        number <- randomRIO (1, m)
        return $ number > c
    ),
    ( " 1/2 chance",
      \_ _ -> do
        number <- randomRIO (1 :: Int, 2)
        return $ number == 1
    ),
    ( " 1/4 chance",
      \_ _ -> do
        number <- randomRIO (1 :: Int, 4)
        return $ number == 1
    ),
    ( " 1/8 chance",
      \_ _ -> do
        number <- randomRIO (1 :: Int, 8)
        return $ number == 1
    )
  ]

-- time out before we kill a run
timeoutMS :: Int
timeoutMS = 2 * 60 * 10 ^ (6 :: Int)

runExpirements :: IO ()
runExpirements = do
  putStrLn "hello experiments"
  -- experimentRunTimes "feasibility_strategys.csv" functions benchmarkFiles [30, 40, 45]
  -- experimentRunTimes "length_time.csv" [("", randomChooseCheck)] benchmarkFiles [30, 60, 90]
  -- experimentPaths "paths_time.csv" benchmarkFiles [30, 60, 90]
  -- experimentBenchmarks "examples/benchmark/memberOf.gcl" [40, 55, 65] "bench_member.csv"
  experimentBenchmarks "examples/benchmark/divByN.gcl" [100, 150, 200] "bench_div.csv"
  -- experimentBenchmarks "examples/benchmark/pullUp.gcl" [100] "bench_pull.csv"

experimentBenchmarks :: String -> [Int] -> String -> IO ()
experimentBenchmarks file pathLengths filename =
  do
    program <- parseGCLfile file
    let prg' = fromRight program
    print prg'
    results <-
      sequence
        [ do
            print nam
            lst <- sequence [timeout timeoutMS (runWithProgram randomChooseCheck False len False prg) | len <- pathLengths]
            return (nam, lst)
          | (nam, prg) <- generateExperiments prg'
        ]
    let runtimes = map (second $ map (showTime . fmap runtime)) results
    toCsvFile filename (("lengths", map show pathLengths) : runtimes)
  where
    generateExperiments :: Program -> [(String, Program)]
    generateExperiments
      Program
        { name = n,
          input = i,
          output = out,
          stmt = Seq (Assign "N" _) xs
        } =
        [ ( show ns,
            Program
              { name = n,
                input = i,
                output = out,
                stmt = Seq (Assign "N" (LitI ns)) xs
              }
          )
          | ns <- [2 .. 10]
        ]
    generateExperiments _ = error "idk"

    fromRight :: Either a b -> b
    fromRight (Right x) = x
    fromRight (Left _) = error "parsing error"

experimentRunTimes :: String -> [(String, Int -> Int -> IO Bool)] -> [String] -> [Int] -> IO ()
experimentRunTimes filename fs files pathLengths =
  do
    res <- experiment fs files pathLengths
    let runtimes = map (second $ map (showTime . fmap runtime)) res
    toCsvFile filename (("lengths", map show pathLengths) : runtimes)

experimentPaths :: String -> [String] -> [Int] -> IO ()
experimentPaths filename files pathLengths =
  do
    res <- experiment [("", randomChooseCheck)] files pathLengths
    let extraData = addColums res
    toCsvFile filename (("lengths", map show pathLengths) : extraData)
  where
    addColums :: [(String, [Maybe RunResult])] -> [(String, [String])]
    addColums =
      concatMap
        ( \(column, results) ->
            [ (column ++ " runtime", map (showTime . fmap runtime) results),
              (column ++ " generated paths", map (show . fmap pathsGenerated) results),
              (column ++ " pruned paths", map (show . fmap pathsPruned) results)
            ]
        )

experiment :: [(String, Int -> Int -> IO Bool)] -> [String] -> [Int] -> IO [(String, [Maybe RunResult])]
experiment fs files pathLengths =
  sequence
    [ do
        let name = file ++ (if noHeur then " no heuristics" else "") ++ label
        print name
        lst <- sequence [timeout timeoutMS (run f noHeur len False file) | len <- pathLengths]
        return (name, lst)
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
-- Weird function that combines the i'th elements of each list
--  For example: combineLists ["abcd", "abcd","abcd","abcd"] id == ["aaaa","bbbb","cccc","dddd"]
combineLists xs f
  | not (any null xs) = heads : tails
  | otherwise = []
  where
    tails = combineLists (map tail xs) f
    heads = f $ map head xs
