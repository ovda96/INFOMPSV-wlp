{-# LANGUAGE NamedFieldPuns #-}

module PathTree (
  PathTree(Node, CondNode, Leaf),
  generate,
  generatePaths
  ) where
--
import GCLParser.GCLDatatype

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
    process pt (Seq s1 s2)          = process (process pt s2) s1
    process pt (IfThenElse g s1 s2) = CondNode g (process pt s1) (process pt s2)
    -- process pt (While g s)          = CondNode g (process pt s) pt -- Note; in the generated tree, we do not know where the condition ends.
    --    For now: a while-loop creates a conditional node with guard g, that in turn either executes statement s and subsequently checks the guard g (to do so again), OR else goes to the remainder of the code.
    process pt (While g s)          = CondNode g (process pt (Seq s (While g s))) pt
    process pt (Block vd s)         = undefined
    process pt (TryCatch msg s1 s2) = undefined
    process pt s                    = Node s pt -- the rest

generatePaths :: Int -> PathTree -> [[Stmt]]
generatePaths n = travel 0 []
  where
    travel :: Int -> [Stmt] -> PathTree -> [[Stmt]]
    travel _ xs Leaf           = [xs]

    travel c xs (Node stmt pt)  | c < n     = travel (c + 1) (stmt:xs) pt
                                | otherwise = []

    travel c xs (CondNode g pt1 pt2) = travel (c + 1) (Assume g:xs) pt1 ++ travel (c + 1) (Assume (OpNeg g):xs) pt2
