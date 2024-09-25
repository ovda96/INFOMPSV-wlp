{-# LANGUAGE NamedFieldPuns #-}
-- | PathTree.hs: Defines the path tree datatype, relating funcs

module PathTree (
  PathTree(Node, CondNode, Leaf),
  generate,
  generatePaths
  ) where
--
import GCLParser.GCLDatatype
import Wlp ( translate, simplify )
import InterfaceZ3( isSatisfiable )

data PathTree = Node Stmt PathTree
              -- Cond(itional) Nodes contain the expression of the condition: if true, the left PathTree is evaluated; otherwise, the right one.
              | CondNode Expr PathTree PathTree
              | Leaf
  deriving (Show)

-- Functions 
generate :: Program -> PathTree
generate (Program { stmt })  = process Leaf stmt
  where
    process :: PathTree -> Stmt -> PathTree
    process pt (Seq s1 s2)            = process (process pt s2) s1
    process pt (IfThenElse g s1 s2)   = CondNode g (process pt s1) (process pt s2)
    process pt (Block _ s)            = process pt s

    -- For a while loop, we create an infinite sequence of (if g) -> do the action, recheck guard, or
    --    else, continue to statement after while loop.
    process pt (While g s)            = CondNode g (process pt (Seq s (While g s))) pt

    -- TODO
    process pt e@(TryCatch msg s1 s2) = error $ "Unimplemented PathTree conversion from Stmt TryCatch: " ++ show e

    process pt s                      = Node s pt -- the rest

generatePaths :: Bool -> Int -> Program -> PathTree -> IO ([[Stmt]],Int)
-- Generates a list of program excecutions of max. length n.
--    This unfortunately needs to be in an IO-block since we need to be able to evaluate the 
--    feasibility of a path using Z3.
generatePaths noHeur n p = travel 0 []
  where
    travel :: Int -> [Stmt] -> PathTree -> IO ([[Stmt]],Int)
    travel _ xs Leaf           = return ([xs], 0)

    travel c xs (Node stmt pt)  | c < n     = travel (c + 1) (xs ++ [stmt]) pt
                                -- If we have reached the max. path length but the tree hasn't been traversed correctly,
                                --    discard the entire path.
                                | otherwise = return ([], 0)

    -- For conditions:
    travel c xs (CondNode g pt1 pt2) = do
      -- We check the feasibility of path xs by calculating the wlp using g as postcond., AND using ¬g as pondcond.
      --    Note that we do not check the path feasibility when heuristics are turned off.
      feasibleG <- if noHeur then return True else isFeasible p g xs
      feasibleNegG <- if noHeur then return True else isFeasible p (OpNeg g) xs

      (b1, noPruned1) <- if feasibleG
        -- If path to g is feasible, we explore; else discard.
        then travel (c + 1) (xs ++ [Assume g]) pt1
        else return ([], 1)
      
      (b2, noPruned2) <- if feasibleNegG
        -- If path to ¬g is feasible, we explore; else discard.
        then travel (c + 1) (xs ++ [Assume (OpNeg g)]) pt2
        else return ([], 1)
 
      return (b1 ++ b2, noPruned1 + noPruned2)

isFeasible :: Program -> Expr -> [Stmt] -> IO Bool
-- Checks the feasibility of branch condition g, given program path xs.
-- TODO: Optimize performance, prevent many z3 calls.
isFeasible p g xs = do -- TODO revert
  let wlp = calculateWlp
  print wlp
  isSatisfiable p calculateWlp
  where
    calculateWlp :: Expr
    -- Calculates the wlp of all statements so far in path, with g as postcondition.
    calculateWlp = simplify $ foldr helper g xs

    helper :: Stmt -> Expr -> Expr
    -- In feasibility checking, we replace assumes with assert (or, rather, its calculated wlp)...
    helper (Assume e) acc = opAnd e acc
    -- ... and we discard the pre-existing asserts
    helper (Assert _) acc = acc
    helper x acc          = translate x acc
