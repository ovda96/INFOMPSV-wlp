{-# LANGUAGE NamedFieldPuns #-}

module PathTree (
  PathTree(Node, CondNode, Leaf),
  generate,
  generatePaths
  ) where
--
import GCLParser.GCLDatatype
import Wlp

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

generatePaths :: Int -> PathTree -> [[Stmt]]
-- Generates a list of program excecutions of max. length n
generatePaths n = travel 0 []
  where
    travel :: Int -> [Stmt] -> PathTree -> [[Stmt]]
    travel _ xs Leaf           = [xs]

    travel c xs (Node stmt pt)  | c < n     = travel (c + 1) (xs ++ [stmt]) pt
                                -- If we have reached the max. path length but the tree hasn't been traversed correctly,
                                --    discard the entire path.
                                | otherwise = []

    -- For conditions, we traverse the path assuming the guard is true, and we traverse the alternative path assuming the guard is false.
    travel c xs (CondNode g pt1 pt2) = travel (c + 1) (xs ++ [Assume g]) pt1 ++ travel (c + 1) (xs ++ [Assume (OpNeg g)]) pt2
      -- if isFeasible g then travel (c + 1) (xs ++ [Assume g]) pt1 ++ travel (c + 1) (xs ++ [Assume (OpNeg g)]) pt2
      --                 else []

isFeasible :: Expr -> [Stmt] -> Expr
-- isFeasible :: Expr -> [Stmt] -> Bool
-- Checks the feasibility of branch condition g, given program path xs.
isFeasible g xs = simplify $ foldr helper g xs
  where
    helper :: Stmt -> Expr -> Expr
    -- In feasibility checking, we replace assumes with assert (or, rather, its calculated wlp)...
    helper (Assume e) acc = opAnd e acc
    -- ... and we discard the pre-existing asserts
    helper (Assert _) acc = acc
    helper x acc          = translate x acc
