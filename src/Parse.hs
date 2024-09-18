-- | Parse.hs: Parses the supplied path, pretty prints the results and echoes them back to the console
module Parse (
  run
) where
--
import GCLUtils(parseGCLfile)
import PathTree(generate, generatePaths)
import Wlp(calculate)

run :: Bool -> Int -> String -> IO ()
run heur k path = do

  -- 1) Parse file...
  gcl <- parseGCLfile path
  let (Right prg) = gcl
  print "PROGRAM -----------------------"
  print prg
  
  -- 2) Construct tree and valid paths of length.
  let tree = PathTree.generate prg
  paths <- PathTree.generatePaths k prg tree
  print "PATHS -------------------------"
  print $ "found " ++ show (length paths) ++ " paths"
  print paths

  let wlps = map Wlp.calculate paths
  print "WLPS --------------------------"
  print wlps
