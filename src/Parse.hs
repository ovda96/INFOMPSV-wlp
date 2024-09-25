-- | Parse.hs: Parses the supplied path, pretty prints the results and echoes them back to the console
module Parse (
  run
) where
--
import GCLUtils( parseGCLfile )
import PathTree( generate, generatePaths )
import Wlp( calculate )
import Control.Monad ( when, unless )
import GCLParser.GCLDatatype ( Program, Stmt )
import InterfaceZ3 ( isValid )

run :: Bool -> Int -> Bool -> String -> IO ()
run noHeur k v path = do
  -- 1) Parse file...
  gcl <- parseGCLfile path
  let (Right prg) = gcl
  
  -- 2) Construct tree and valid paths of max.length k
  let tree = PathTree.generate prg
  (paths, noPruned) <- PathTree.generatePaths noHeur k prg tree
  print ("[INFO] No. paths generated: " ++ show (length paths))

  when v $ print "[PATHS]"
  when v $ print paths

  -- We print the number of pruned branches unless heuristics was turned off (since we didn't prune anything if that was the case)
  unless noHeur $ print ("[INFO] No. branches pruned: " ++ show noPruned)

  -- 3) Validate complete paths
  result <- validate v prg paths
  when result $ print "accept"

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
