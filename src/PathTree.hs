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
-- Note: TODO Discarding unfeasible paths
generate (Program { stmt })  = process Leaf stmt
  where
    process :: PathTree -> Stmt -> PathTree
    process pt (Seq s1 s2)          = process (process pt s2) s1
    process pt (IfThenElse g s1 s2) = CondNode g (process pt s1) (process pt s2)

    -- For a while loop, we create an infinite sequence of (if g) -> do the action, recheck guard, or
    --    else, continue to statement after while loop.
    process pt (While g s)          = CondNode g (process pt (Seq s (While g s))) pt

    -- TODO
    process pt (Block vd s)         = undefined
    process pt (TryCatch msg s1 s2) = undefined

    process pt s                    = Node s pt -- the rest

generatePaths :: Int -> Program -> PathTree -> IO [[Stmt]]
-- Generates a list of program excecutions of max. length n.
--    This unfortunately needs to be in an IO-block since we need to be able to evaluate the 
--    feasibility of a path using Z3.
generatePaths n p = travel 0 []
  where
    travel :: Int -> [Stmt] -> PathTree -> IO [[Stmt]]
    travel _ xs Leaf           = return [xs]

    travel c xs (Node stmt pt)  | c < n     = travel (c + 1) (xs ++ [stmt]) pt
                                -- If we have reached the max. path length but the tree hasn't been traversed correctly,
                                --    discard the entire path.
                                | otherwise = return []

    -- For conditions:
    travel c xs (CondNode g pt1 pt2) = do
      -- TODO: We should be able to count how many paths are marked as unfeasible.
      -- We check the feasibility of path xs by calculating the wlp using g as postcond., AND using ¬g as pondcond.
      feasibleG <- isFeasible p g xs
      feasibleNegG <- isFeasible p (OpNeg g) xs

      b1 <- if feasibleG
        -- If path to g is feasible, we explore; else discard.
        then travel (c + 1) (xs ++ [Assume g]) pt1
        else return []
      
      b2 <- if feasibleNegG
        -- If path to ¬g is feasible, we explore; else discard.
        then travel (c + 1) (xs ++ [Assume (OpNeg g)]) pt2
        else return []
 
      return $ b1 ++ b2

isFeasible :: Program -> Expr -> [Stmt] -> IO Bool
-- Checks the feasibility of branch condition g, given program path xs.
isFeasible p g xs = isSatisfiable p calculateWlp
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
