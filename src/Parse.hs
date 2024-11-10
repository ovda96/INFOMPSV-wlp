-- | Parse.hs: Parses the supplied path, pretty prints the results and echoes them back to the console
module Parse (
  run,
  validate,
  RunResult,
  pathsGenerated,
  pathsPruned,
  formulaSize,
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
import Z3.Monad ( Z3Env, newEnv, stdOpts )
import Data.List ( sortOn )
import System.TimeIt
import Utils( count )

data RunResult = RunResult {
  pathsGenerated :: Int,
  pathsPruned :: Int,
  formulaSize :: Int,
  result :: Bool,
  runtime :: Double
}

run :: (Int -> Int -> IO Bool) -> Bool -> Int -> Bool -> String -> IO RunResult
run f noHeur k v path = do
  (t, (p, pruned, size, res)) <- timeItT $ do
    -- 1) Parse file...
    gcl <- parseGCLfile path
    let (Right prg) = gcl
    
    -- 2) Construct tree and valid paths of max.length k
    env <- newEnv Nothing stdOpts -- Reusing the Z3 environment significally increases performance.
    let tree = PathTree.generate prg
    (paths, noPruned) <- PathTree.generatePaths env f noHeur k prg tree
    print ("[INFO] No. paths generated: " ++ show (length paths))

    -- Sorting the paths on length from short to long is very benificial if a short path is faulty.
    let sortedPaths = sortOn length paths

    when v $ print "[PATHS]"
    when v $ print sortedPaths

    -- We print the number of pruned branches unless heuristics was turned off (since we didn't prune anything if that was the case).
    unless noHeur $ print ("[INFO] No. branches pruned: " ++ show noPruned)

    -- 3) Validate complete paths
    (res, tot) <- validate v env 0 prg sortedPaths
    print ("[INFO] Total investigated formula size: " ++ show tot)
    when res $ print "accept"

    return (length paths, noPruned, tot, res)

  return $ RunResult p pruned size res t

validate :: Bool -> Z3Env -> Int -> Program -> [[Stmt]] -> IO (Bool, Int)
-- Validates the supplied paths; also returns total formula size.
validate _ _ t _ []     = return (True, t)
validate v env t p (x : xs) = do
  when v $ print ("[PATH] " ++ show x)
  let wlp = Wlp.calculate x
  when v $ print ("[WLP] " ++ show wlp)
  let t' = t + Utils.count wlp

  valid <- isValid env p wlp
  if valid
    then validate v env t' p xs
    -- If we encounter a faulty path, we reject and print it
    else do
      print "reject"
      print $ show x
      return (False, t')
