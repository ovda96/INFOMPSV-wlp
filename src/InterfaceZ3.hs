{-# LANGUAGE NamedFieldPuns #-}

module InterfaceZ3 (
  generate
) where

--
import Z3.Monad
import GCLParser.GCLDatatype
import Control.Monad
import Control.Applicative

generate :: Expr -> Z3 AST
generate (Var s)    = mkIntVar =<< mkStringSymbol s -- TODO look-up based on type

generate (LitI i)   = mkInteger (fromIntegral i)
generate (LitB b)   = if b then mkTrue else mkFalse
generate (Parens e) = generate e
generate (OpNeg e)  = mkNot =<< generate e

generate (BinopExpr And e1 e2)              = sequence [generate e1, generate e2] >>= mkAnd
generate (BinopExpr Or e1 e2)               = sequence [generate e1, generate e2] >>= mkOr
generate (BinopExpr Implication e1 e2)      = join (liftA2 mkImplies (generate e1) (generate e2))
generate (BinopExpr LessThan e1 e2)         = join (liftA2 mkLt (generate e1) (generate e2))
generate (BinopExpr LessThanEqual e1 e2)    = join (liftA2 mkLe (generate e1) (generate e2))
generate (BinopExpr GreaterThan e1 e2)      = join (liftA2 mkGt (generate e1) (generate e2))
generate (BinopExpr GreaterThanEqual e1 e2) = join (liftA2 mkGe (generate e1) (generate e2))
generate (BinopExpr Minus e1 e2)            = sequence [generate e1, generate e2] >>= mkSub
generate (BinopExpr Plus e1 e2)             = sequence [generate e1, generate e2] >>= mkAdd
generate (BinopExpr Multiply e1 e2)         = sequence [generate e1, generate e2] >>= mkMul
generate (BinopExpr Divide e1 e2)           = join (liftA2 mkDiv (generate e1) (generate e2))
generate (BinopExpr Alias e1 e2)            = undefined

generate _ = undefined

isSatisfiable :: Z3 AST -> Z3 Result
isSatisfiable ast = do
    _ast <- ast
    assert _ast
    (verdict, _) <- getModel
    return verdict