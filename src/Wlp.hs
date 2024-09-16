module Wlp (
  calculate
) where
--
import GCLParser.GCLDatatype

calculate :: [Stmt] -> Expr
-- We fold using "true" as our starting value
calculate = foldr translate (LitB True)

translate :: Stmt -> Expr -> Expr
-- TODO Unfinished.
translate Skip q               = q                      -- wlp skip Q = Q
translate (Assert e) q         = opAnd e q              -- wlp (assert e) Q = e /\ Q
translate (Assume e) q         = opImplication e q      -- wlp (assume e) Q = e => Q
translate (Assign var e) q     = substitute var e q     -- wlp (x:=e) Q = Q[e/x]
translate (AAssign var i e) q  = undefined
translate (DrefAssign var e) q = undefined

-- The rest should not appear in our generated path (for now).
translate _ _                  = undefined

substitute :: String -> Expr -> Expr -> Expr
-- Substitutes all occurrences of varName x in expression q by e.
substitute x e q@(Var s)            | x == s = e
                                    | otherwise = q
substitute x e (Parens e1) = Parens (substitute x e e1)
substitute x e (ArrayElem e1 e2)    = ArrayElem (substitute x e e1) (substitute x e e2)
substitute x e (OpNeg e1)           = OpNeg (substitute x e e1)
substitute x e (BinopExpr op e1 e2) = BinopExpr op (substitute x e e1) (substitute x e e2)
-- We do not need to replace the variable in the quantifier, since after the quantifier the expression will change due to the substitution.
--    I.e.: For all x: x > 0 --> x := x - 1 --> For all x: x - 1 > 0 (I think!).
substitute x e (Forall s e1)        = Forall s (substitute x e e1) 
substitute x e (Exists s e1)        = Exists s (substitute x e e1)
substitute x e (RepBy e1 e2 e3)     = RepBy (substitute x e e1) (substitute x e e2) (substitute x e e3)
substitute x e (NewStore e1)        = NewStore (substitute x e e1)

-- All others do not include expressions of their own.
substitute _ _ q                     = q
