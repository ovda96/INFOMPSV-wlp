{-# LANGUAGE InstanceSigs #-}
-- | Main.hs: Entry point, defines CLI-options

module Main (main) where
-- 
import Options
import Parse (run)
import System.TimeIt ( timeIt )
import Experiments
import Control.Monad (void)

-- Option/arg parsing
data MainOptions = MainOptions
  { optHeuristics :: Bool
  , optLength :: Int
  , optVerbose :: Bool
  , experiments :: Bool
  }

instance Options MainOptions where
  defineOptions :: DefineOptions MainOptions
  defineOptions = MainOptions
    <$> defineOption optionType_bool (\o -> o
      { optionLongFlags = ["noHeuristics"]
      , optionShortFlags = ['h']
      , optionDefault = False
      , optionDescription = "Turns off heuristics"
      })
    <*> defineOption optionType_int (\o -> o
      { optionLongFlags = ["length"]
      , optionShortFlags = ['k']
      , optionDefault = 0
      , optionDescription = "The inclusive maximum path length to be evaluated"
      })
    <*> defineOption optionType_bool (\o -> o
      { optionLongFlags = ["verbose"]
      , optionShortFlags = ['v']
      , optionDefault = False
      , optionDescription = "Print detailed information to console"
      })
    <*> simpleOption "experiments" False
          "Run the experiments"

-- MAIN
-- When using cabal run including arguments/options, use:
--    cabal run exe:wlp-verifier -- {ARGS} {OPTS}
--  I.e.: cabal run exe:wlp-verifier -- "./examples/test.gcl" -k 12
main :: IO ()
main = runCommand process
  where 
    process :: MainOptions -> [String] -> IO()
    process (MainOptions _ _ _ True) _ = runExpirements
    process _ []          = putStrLn "Please provide path to .gcl-file"
    
    -- Note that we only parse the first supplied file path.
    process opts (p : _)  = timeIt $ void $ Parse.run (optHeuristics opts) (optLength opts) (optVerbose opts) p
