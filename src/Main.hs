module Main (main) where
-- 
import Options

import Parse (run)
import Example (run)

-- Option/arg parsing
data MainOptions = MainOptions
  { optHeuristics :: Bool
  , optLength :: Int
  }

instance Options MainOptions where
  defineOptions = MainOptions
    <$> simpleOption "heuristics" False
        "Turn on heuristics"
    <*> simpleOption "k" 0
         "The inclusive maximum path length to be evaluated (k)"

-- MAIN
-- When using cabal run including arguments/options, use:
--    cabal run exe:wlp-verifier -- {ARGS} {OPTS}
--  I.e.: cabal run exe:wlp-verifier -- "./examples/test.gcl" --heuristics
main :: IO ()
main = runCommand process
  where 
    process :: MainOptions -> [String] -> IO()
    process _ []          = putStrLn "Please provide path to .gcl-file"
    
    -- Note that we only parse the first supplied file path.
    process opts (p : _)  = Parse.run (optHeuristics opts) (optLength opts) p

-- To test whether Z3 works correctly, use below.
-- main :: IO () 
-- main = Example.run