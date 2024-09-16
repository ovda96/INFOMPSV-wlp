-- | Parse.hs: Parses the supplied path, pretty prints the results and echoes them back to the console.
module Parse (run) where
--
import GCLUtils(parseGCLfile)
import PathTree(generate, generatePaths)

run :: Bool -> Int -> String -> IO ()
run heuristics k path = do
  putStrLn $ "Heuristics: " ++ if heuristics then "true" else "false"
  putStrLn $ "K: " ++ show k
  putStrLn "---"
  gcl <- parseGCLfile path
  let (Right prg) = gcl
  print prg
  let ptree = PathTree.generate prg
  print $ PathTree.generatePaths k ptree