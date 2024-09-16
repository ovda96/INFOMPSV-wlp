module Wlp (
  calculate
) where
--
import GCLParser.GCLDatatype
import Utils (apply)

calculate :: [Stmt] -> Expr
-- We fold using "true" as our starting value
calculate xs = simplify $ foldr translate (LitB True) xs

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
substitute x e = Utils.apply helper
  where
      helper :: Expr -> Expr
      -- We only need to substitute the variable name with the expression (if they match). Not:
          -- Quantifiers: since after the quantifier the expression will change due to the substitution.
                -- I.e.: For all x: x > 0 --> x := x - 1 --> For all x: x - 1 > 0 (I think!).
          -- Dereference: since we do not want to dereference (x - 1) after substituting x := x - 1.
      helper q@(Var s)  | x == s    = e
                        | otherwise = q
      helper q          = q

simplify :: Expr -> Expr
-- Simplifies out some common things in the resulting WLP.
simplify = Utils.apply helper
  where
    helper :: Expr -> Expr
    -- True && ... or ... && True == ...
    helper (BinopExpr And (LitB True) e) = e
    helper (BinopExpr And e (LitB True)) = e

    -- False && ... or ... && False == False
    helper (BinopExpr And (LitB False) _) = LitB False
    helper (BinopExpr And _ (LitB False)) = LitB False

    -- True || ... or ... || True == True
    helper (BinopExpr Or (LitB True) _) = LitB True
    helper (BinopExpr Or _ (LitB True)) = LitB True

    -- Self-implications
      -- p => p == True
      -- (¬(p => p) == False
      -- p => ¬p == False
      -- ¬p => p == False
    helper e@(BinopExpr Implication e1 e2)        | e1 == e2  = LitB True
                                                  | otherwise = e
    helper e@(OpNeg(BinopExpr Implication e1 e2)) | e1 == e2  = LitB False
                                                  | otherwise = e
    helper e@((BinopExpr Implication e1 e2))      | e1 == OpNeg e2 = LitB False
                                                  | OpNeg e1 == e2 = LitB False   -- Redundant?
                                                  | otherwise = e

    -- False self-comparisons
      -- x < x == False
      -- x > x == False
      -- ¬(x == x) == False
    helper e@(BinopExpr LessThan e1 e2)       | e1 == e2  = LitB False
                                              | otherwise = e
    helper e@(BinopExpr GreaterThan e1 e2)    | e1 == e2  = LitB False
                                              | otherwise = e
    helper e@(OpNeg(BinopExpr Equal e1 e2))   | e1 == e2  = LitB False
                                              | otherwise = e

    -- True self-comparisons
      -- x =< x == True
      -- x >= x == True
      -- x == x == True
    helper e@(BinopExpr LessThanEqual e1 e2)    | e1 == e2  = LitB True
                                                | otherwise = e
    helper e@(BinopExpr GreaterThanEqual e1 e2) | e1 == e2  = LitB True
                                                | otherwise = e
    helper e@(BinopExpr Equal e1 e2)            | e1 == e2  = LitB True
                                                | otherwise = e
    
    -- Non-recursive expressions (literals) in parentheses
    helper (Parens e@(Var _))         = e
    helper (Parens e@(LitI _))        = e
    helper (Parens e@(LitB _))        = e
    helper (Parens LitNull)           = LitNull
    helper (Parens e@(Dereference _)) = e
    
    -- All else
    helper e = e
