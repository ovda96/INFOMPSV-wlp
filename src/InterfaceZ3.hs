{-# LANGUAGE NamedFieldPuns #-}
-- | InterfaceZ3.hs: Handles Expr to Z3 conversion

module InterfaceZ3 (
  isSatisfiable,
  isValid
) where

--
import Z3.Monad
import GCLParser.GCLDatatype
import Control.Monad ( join )
import Control.Applicative ( Applicative(liftA2) )
import Data.Maybe ( fromJust )

initialize :: Program -> [(String, Z3 AST)]
-- Generates a list of variable instantiations from the program spec.
initialize (Program {input, output}) = map helper (input ++ output)
  where 
    helper :: VarDeclaration -> (String, Z3 AST)
    helper (VarDeclaration s (PType PTInt))   = (s, mkIntVar =<< mkStringSymbol s)
    helper (VarDeclaration s (PType PTBool))  = (s, mkBoolVar =<< mkStringSymbol s)
    helper decl                               = error $ "Unimplemented VarDeclaration " ++ show decl

generate :: Program -> Expr -> Z3 AST
-- Turns an expression into a Z3 AST.
generate p (Var s)    = fromJust $ lookup s vars
  -- In case of a variable, we look up the instantiation in our list to make sure we are using the
  --    correct type.(*)
  where 
    vars :: [(String, Z3 AST)]
    vars = initialize p

generate _ (LitI i)   = mkIntNum i
generate _ (LitB b)   = if b then mkTrue else mkFalse
generate p (Parens e) = generate p e
generate p (OpNeg e)  = mkNot =<< generate p e

-- Some shenanigans are needed to deal with monads/applicatives outside of do-blocks. Essentially,
--    there are two possible patterns involved:
--    1) (i.e.) liftA2 mkGt (... e1) (... e2)
--        do
--            _e1 <- e1
--            _e2 <- e2
--            _e1 `mkGt` _e2
--        (Note that we need to include a join to prevent a return type of Z3 (Z3 AST))
--    2) (i.e.) sequence [... e1, ... e2] >>= mkAnd
--        do
--          _e1 <- e1
--          _e2 <- e2
--          mkAnd [e1, e2]
generate p (BinopExpr And e1 e2)              = sequence [generate p e1, generate p e2] >>= mkAnd
generate p (BinopExpr Or e1 e2)               = sequence [generate p e1, generate p e2] >>= mkOr
generate p (BinopExpr Implication e1 e2)      = join (liftA2 mkImplies (generate p e1) (generate p e2))
generate p (BinopExpr LessThan e1 e2)         = join (liftA2 mkLt (generate p e1) (generate p e2))
generate p (BinopExpr LessThanEqual e1 e2)    = join (liftA2 mkLe (generate p e1) (generate p e2))
generate p (BinopExpr GreaterThan e1 e2)      = join (liftA2 mkGt (generate p e1) (generate p e2))
generate p (BinopExpr GreaterThanEqual e1 e2) = join (liftA2 mkGe (generate p e1) (generate p e2))
generate p (BinopExpr Minus e1 e2)            = sequence [generate p e1, generate p e2] >>= mkSub
generate p (BinopExpr Plus e1 e2)             = sequence [generate p e1, generate p e2] >>= mkAdd
generate p (BinopExpr Multiply e1 e2)         = sequence [generate p e1, generate p e2] >>= mkMul
generate p (BinopExpr Divide e1 e2)           = join (liftA2 mkDiv (generate p e1) (generate p e2))
generate p (BinopExpr Equal e1 e2)            = join (liftA2 mkEq (generate p e1) (generate p e2))
generate p exp@(BinopExpr Alias e1 e2)        = error $ "Unimplemented Z3 conversion from BinopExpr Alias: " ++ show exp

generate p exp@(Forall s e) = error $ "Unimplemented Z3 conversion from Forall: " ++ show exp     -- TODO This definitely still needs to be done
generate p exp@(Exists s e) = error $ "Unimplemented Z3 conversion from Exists: " ++ show exp     -- TODO This definitely still needs to be done

generate _ exp = error $ "Unimplemented Z3 conversion from Expr: " ++ show exp

isSatisfiable :: Program -> Expr -> IO Bool
-- Checks whether an expression is satisfiable.
-- src: https://github.com/wooshrow/gclparser/blob/master/examples/examplesHaskellZ3/Z3ProverExample.hs
isSatisfiable p e = do
  conclusion <- evalZ3 $ checker (generate p e)
  return $ conclusion == Sat
  where
    checker :: Z3 AST -> Z3 Result
    checker ast = do
      _ast <- ast
      assert _ast
      (verdict, _) <- getModel
      return verdict


isValid :: Program -> Expr -> IO Bool
-- Checks whether an expression is valid.
-- src: https://github.com/wooshrow/gclparser/blob/master/examples/examplesHaskellZ3/Z3ProverExample.hs
isValid p e = do
  conclusion <- evalZ3 $ checker (generate p e)
  return $ conclusion == Sat
  where
    checker :: Z3 AST -> Z3 Result
    checker ast = do
      _ast <- ast
      f <- mkNot _ast
      assert f
      (verdict, _) <- getModel
      return verdict
  