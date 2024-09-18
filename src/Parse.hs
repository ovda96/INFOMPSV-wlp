-- | Parse.hs: Parses the supplied path, pretty prints the results and echoes them back to the console
module Parse (
  run
) where
--
import GCLUtils( parseGCLfile )
import PathTree( generate, generatePaths )
import Wlp( calculate )
import Control.Monad ( when )
import GCLParser.GCLDatatype ( Program, Expr )
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

  -- 3) Calculate WLPs
  let wlps = map Wlp.calculate paths
  when v $ print "[WLPs]"
  when v $ print wlps

  -- 4) Validate WLPs of complete paths
  result <- validate v prg wlps
  when result $ print "accept"

validate :: Bool -> Program -> [Expr] -> IO Bool
-- Validates the supplied WLPs.
validate _ _ []     = return True
validate v p (x:xs) = do
  when v $ print "[EVAL]"
  when v $ print x

  valid <- isValid p x
  if valid
    then validate v p xs
    -- If we encounter a faulty path, we reject and print it
    else do
      print "reject"
      print $ show x
      return False
