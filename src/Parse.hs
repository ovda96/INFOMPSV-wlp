-- | Parse.hs: Parses the supplied path, pretty prints the results and echoes them back to the console.
module Parse (
  run
) where
--
import GCLUtils(parseGCLfile)
import PathTree(generate, generatePaths)
import Wlp(calculate)

run :: Bool -> Int -> String -> IO ()
run heuristics k path = do
  putStrLn $ "Heuristics: " ++ if heuristics then "true" else "false"
  putStrLn $ "K: " ++ show k
  putStrLn "---"

  gcl <- parseGCLfile path
  let (Right program) = gcl
  print program
  
  let tree = PathTree.generate program
  let paths = PathTree.generatePaths k tree
  print paths

  let wlps = map Wlp.calculate paths
  print wlps
