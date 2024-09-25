-- | Parse.hs: Parses the supplied path, pretty prints the results and echoes them back to the console
module Parse (
  run
) where
--
import GCLUtils( parseGCLfile )
import PathTree( generate, generatePaths )
import Wlp( calculate )
import Control.Monad ( when )
import GCLParser.GCLDatatype ( Program, Stmt )
import InterfaceZ3 ( isValid )

run :: Bool -> Int -> Bool -> String -> IO ()
run heur k v path = do
  -- 1) Parse file...
  gcl <- parseGCLfile path
  let (Right prg) = gcl
  
  -- 2) Construct tree and valid paths of max.length k
  let tree = PathTree.generate prg
  paths <- PathTree.generatePaths k prg tree
  when v $ print "[PATHS]"
  when v $ print $ length paths
  when v $ print paths

  let (feasiblePaths, unfeasCount) = paths  

  print $ "Number of unfeasible paths: " ++ show unfeasCount

  -- 3) Validate complete paths
  result <- validate v prg (feasiblePaths, unfeasCount)
  when result $ print "accept"

validate :: Bool -> Program -> ([[Stmt]],Int) -> IO Bool
-- Validates the supplied paths.
validate _ _ ([],_)     = return True
validate v p ((x:xs), unfeascount) = do
  when v $ print ("[PATH] " ++ show x)
  let wlp = Wlp.calculate x
  when v $ print ("[WLP] " ++ show wlp)

  valid <- isValid p wlp
  if valid
    then validate v p (xs,unfeascount)
    -- If we encounter a faulty path, we reject and print it
    else do
      print "reject"
      print $ show x
      return False
