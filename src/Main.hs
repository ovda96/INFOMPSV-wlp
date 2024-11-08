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
  , experiments :: Bool
  , optLength :: Int
  , optVerbose :: Bool
  }

instance Options MainOptions where
  defineOptions :: DefineOptions MainOptions
  defineOptions = MainOptions
    <$> simpleOption "noHeuristics" False
          "Disable heuristics"
    <*> simpleOption "experiments" False
          "Disable heuristics"
    <*> simpleOption "length" 0     -- TODO: Seems a silly default value.
          "The inclusive maximum path length to be evaluated"
    <*> simpleOption "verbose" False
          "Turn on detailed information printing"

-- MAIN
-- When using cabal run including arguments/options, use:
--    cabal run exe:wlp-verifier -- {ARGS} {OPTS}
--  I.e.: cabal run exe:wlp-verifier -- "./examples/test.gcl" --heuristics --length=12
main :: IO ()
main = runCommand process
  where 
    process :: MainOptions -> [String] -> IO()
    process (MainOptions _ True _ _) _ = runExpirements
    process _ []          = putStrLn "Please provide path to .gcl-file"
    
    -- Note that we only parse the first supplied file path.
    process opts (p : _)  = timeIt $ void $ Parse.run (optHeuristics opts) (optLength opts) (optVerbose opts) p
