-- | Parse.hs: Parses the supplied path, pretty prints the results and echoes them back to the console
module Parse (
  run,
  validate,
  RunResult,
  pathsGenerated,
  pathsPruned,
  result,
  runtime
) where
--
import GCLUtils( parseGCLfile )
import PathTree( generate, generatePaths )
import Wlp( calculate )
import Control.Monad ( when, unless )
import GCLParser.GCLDatatype ( Program, Stmt )
import InterfaceZ3 ( isValid )
import Data.List (sortOn)
import System.TimeIt

data RunResult = RunResult {
  pathsGenerated :: Int,
  pathsPruned :: Int,
  result :: Bool,
  runtime :: Double
}

run :: Bool -> Int -> Bool -> String -> IO RunResult
run noHeur k v path = do
  (t, (p, pruned, res)) <- timeItT $ do
    -- 1) Parse file...
    gcl <- parseGCLfile path
    let (Right prg) = gcl
    
    -- 2) Construct tree and valid paths of max.length k
    let tree = PathTree.generate prg
    (paths, noPruned) <- PathTree.generatePaths noHeur k prg tree
    print ("[INFO] No. paths generated: " ++ show (length paths))

    -- We sort on the path lengths to make evaluate the paths from shortest -> longest, which improves performance for large k's significantly.
    --   I.e., on my machine: test.gcl, k = 2000, without pruning
    --        With sorting: 0.17s
    --        Without sorting: 16.48s
    --   and: test.gcl, k = 500, with pruning
    --        With sorting: 24.80s
    --        Without sorting: 24.92s
    --   Since pruning is expensive (for now, since we need to call z3 a lot), the difference is significantly less visible. Pruning also tends
    --      to eliminate quite a number of long paths, which further decreases the impact of the sorting.
    let sortedPaths = sortOn length paths

    when v $ print "[PATHS]"
    when v $ print sortedPaths

    -- We print the number of pruned branches unless heuristics was turned off (since we didn't prune anything if that was the case).
    unless noHeur $ print ("[INFO] No. branches pruned: " ++ show noPruned)

    -- 3) Validate complete paths
    res <- validate v prg sortedPaths
    when res $ print "accept"
    return (length paths, noPruned, res)
  return $ RunResult p pruned res t

validate :: Bool -> Program -> [[Stmt]] -> IO Bool
-- Validates the supplied paths.
validate _ _ []     = return True
validate v p (x : xs) = do
  when v $ print ("[PATH] " ++ show x)
  let wlp = Wlp.calculate x
  when v $ print ("[WLP] " ++ show wlp)

  valid <- isValid p wlp
  if valid
    then validate v p xs
    -- If we encounter a faulty path, we reject and print it
    else do
      print "reject"
      print $ show x
      return False
