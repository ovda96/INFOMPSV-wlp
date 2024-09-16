module Wlp (
  calculate
) where
--
import GCLParser.GCLDatatype as GCL

calculate :: [Stmt] -> Expr
-- We fold using "true" as our starting value
calculate = foldr translate (LitB True)

translate :: Stmt -> Expr -> Expr
-- TODO Unfinished.
translate Skip q               = q                      -- wlp skip Q = Q
translate (Assert e) q         = GCL.opAnd e q          -- wlp (assert e) Q = e /\ Q
translate (Assume e) q         = GCL.opImplication e q  -- wlp (assume e) Q = e => Q
translate (Assign var e) q     = undefined              -- How to replace all occurrences of x with e in q?
translate (AAssign var i e) q  = undefined
translate (DrefAssign var e) q = undefined

-- The rest should not appear in our generated path (for now).
translate _ _                  = undefined
