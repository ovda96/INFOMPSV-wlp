-- | Wlp.hs: Handles Expr to WLP conversion (in Expr-format)

module Wlp (
  calculate,
  simplify, 
  translate
) where
--
import GCLParser.GCLDatatype
import Utils ( apply )

calculate :: [Stmt] -> Expr
-- We fold using "true" as our starting value
calculate xs = simplify $ foldr translate (LitB True) xs

translate :: Stmt -> Expr -> Expr
translate Skip q                    = q                          -- wlp skip Q = Q
translate (Assert e) q              = opAnd e q                  -- wlp (assert e) Q = e /\ Q
translate (Assume e) q              = opImplication e q          -- wlp (assume e) Q = e => Q
translate (Assign var e) q          = substitute var e q         -- wlp (x:=e) Q = Q[e/x]
translate (AAssign var i e) q       = substituteArray var i e q  -- ...
translate s@(DrefAssign var e) q    = error $ "Unimplemented WLP-conversion from DrefAssign: " ++ show s
translate _ s                       = error $ "Unimplemented WLP-conversion from Stmt: " ++ show s

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

substituteArray :: String -> Expr -> Expr -> Expr -> Expr
-- Substitutes array_name using repby
substituteArray arrayName index assignedValue = Utils.apply helper
  where
    helper :: Expr -> Expr
    helper e@(Var var)  | var == arrayName  = RepBy e index assignedValue
                        | otherwise         = e
    helper e            = e

simplify :: Expr -> Expr
-- Simplifies out some common things in the resulting WLP (probably Z3 does this itself, but well).
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
    helper e@(BinopExpr Implication e1 e2)            | e1 == e2  = LitB True
                                                      | otherwise = e
    helper e@(OpNeg(BinopExpr Implication e1 e2))     | e1 == e2  = LitB False
                                                      | otherwise = e
    helper e@((BinopExpr Implication e1 (OpNeg e2)))  | e1 == e2  = LitB False
                                                      | otherwise = e
    helper e@((BinopExpr Implication (OpNeg e1) e2))  | e1 == e2  = LitB False
                                                      | otherwise = e

    -- False self-comparisons
      -- x < x == False
      -- x > x == False
      -- ¬(x == x) == False
      -- x == ¬x == False
      -- ¬x == x == False
    helper e@(BinopExpr LessThan e1 e2)       | e1 == e2  = LitB False
                                              | otherwise = e
    helper e@(BinopExpr GreaterThan e1 e2)    | e1 == e2  = LitB False
                                              | otherwise = e
    helper e@(OpNeg(BinopExpr Equal e1 e2))   | e1 == e2  = LitB False
                                              | otherwise = e
    helper e@(BinopExpr Equal e1 (OpNeg e2))  | e1 == e2  = LitB False
                                              | otherwise = e
    helper e@(BinopExpr Equal (OpNeg e1) e2)  | e1 == e2  = LitB False
                                              | otherwise = e

    -- True self-comparisons
      -- x =< x == True
      -- x >= x == True
      -- x == x == True
      -- x || ¬x == True
      -- ¬x || x == True
      -- x && x == True
    helper e@(BinopExpr LessThanEqual e1 e2)    | e1 == e2  = LitB True
                                                | otherwise = e
    helper e@(BinopExpr GreaterThanEqual e1 e2) | e1 == e2  = LitB True
                                                | otherwise = e
    helper e@(BinopExpr Equal e1 e2)            | e1 == e2  = LitB True
                                                | otherwise = e
    helper e@(BinopExpr Or e1 (OpNeg e2))       | e1 == e2  = LitB True
                                                | otherwise = e
    helper e@(BinopExpr Or (OpNeg e1) e2)       | e1 == e2  = LitB True
                                                | otherwise = e
    helper e@(BinopExpr And e1 e2)              | e1 == e2  = LitB True
                                                | otherwise = e

    -- Non-recursive expressions (literals) in parentheses
    helper (Parens e@(Var _))         = e
    helper (Parens e@(LitI _))        = e
    helper (Parens e@(LitB _))        = e
    helper (Parens LitNull)           = LitNull
    helper (Parens e@(Dereference _)) = e

    -- All else
    helper e = e
