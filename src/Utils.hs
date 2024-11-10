-- | Utils.hs: Utility funcs

module Utils (
  apply,
  count
) where
--
import GCLParser.GCLDatatype

apply :: (Expr -> Expr) -> Expr -> Expr
-- Since catamorphism is scary, we'll just write a utility function to apply a function 
--    f to all further expressions within the recursive datatype.
apply f (Parens e)            = f (Parens (apply f e))
apply f (ArrayElem e1 e2)     = f (ArrayElem (apply f e1) (apply f e2))
apply f (OpNeg e)             = f (OpNeg (apply f e))
apply f (BinopExpr op e1 e2)  = f (BinopExpr op (apply f e1) (apply f e2))
apply f (Forall s e)          = f (Forall s (apply f e))
apply f (Exists s e)          = f (Exists s (apply f e))
apply f (RepBy e1 e2 e3)      = f (RepBy (apply f e1) (apply f e2) (apply f e3))
apply f (NewStore e)          = f (NewStore (apply f e))
apply f e                     = f e

count :: Expr -> Int
-- Counts the total number of literals and variables in an expression.
count (LitB _)            = 1
count (LitI _)            = 1
count (Var _)             = 1
count (Parens e)          = count e
count (ArrayElem e1 e2)   = count e1 + count e2
count (OpNeg e)           = count e
count (BinopExpr _ e1 e2) = count e1 + count e2
count (Forall _ e)        = count e
count (Exists _ e)        = count e
count (RepBy e1 e2 e3)    = count e1 + count e2 + count e3
count (NewStore e)        = count e
count _                   = 0