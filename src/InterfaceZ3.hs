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
import Data.Maybe ( fromJust )
import qualified Data.Map as Map

initialize :: Program -> Map.Map String (Z3 AST)
-- Generates a list of variable instantiations from the program spec.
initialize (Program {input, output, stmt}) = Map.fromList $ concatMap helper (input ++ output ++ blockDeclarations stmt)
  where
    -- Type is list because array creates two variables: the length and the
    --    array itself.
    helper :: VarDeclaration -> [(String, Z3 AST)]
    helper (VarDeclaration s t@(AType pt)) =
      ( s,
          do
            sort <- createSort t
            symbol <- mkStringSymbol s
            mkVar symbol sort
        ) : helper (VarDeclaration ('#' : s) (PType pt))
    helper (VarDeclaration s t) =
      [ ( s,
          do
            sort <- createSort t
            symbol <- mkStringSymbol s
            mkVar symbol sort
        )
      ]

    createSort :: Type -> Z3 Sort
    createSort (AType t) = do
      intSort <- mkIntSort
      tSort <- createSort $ PType t
      mkArraySort intSort tSort
      
    createSort (PType PTInt)  = mkIntSort
    createSort (PType PTBool) = mkIntSort

    createSort t = error $ "Unimplemented type conversion to z3" ++ show t

-- TODO: currently this is treating block declarations as global variables;
--    this is wrong, but easier this way.
blockDeclarations :: Stmt -> [VarDeclaration]
blockDeclarations (Seq s1 s2)           = blockDeclarations s1 ++ blockDeclarations s2
blockDeclarations (IfThenElse _ s1 s2)  = blockDeclarations s1 ++ blockDeclarations s2
blockDeclarations (While _ s1)          = blockDeclarations s1
blockDeclarations (Block ds s1)         = ds ++ blockDeclarations s1
blockDeclarations (TryCatch _ s1 s2)    = blockDeclarations s1 ++ blockDeclarations s2
blockDeclarations _                     = []

generate :: Map.Map String (Z3 AST) -> Expr -> Z3 AST
-- Turns an expression into a Z3 AST.
generate env (Var s)    = fromJust $ Map.lookup s env -- Lookup variable in environment
generate _ (LitI i)     = mkIntNum i
generate _ (LitB b)     = if b then mkTrue else mkFalse
generate env (Parens e) = generate env e
generate env (OpNeg e)  = mkNot =<< generate env e

-- Some shenanigans are needed to deal with monads/applicatives outside of do-blocks (although we could have just used them, oops). Essentially,
--    there are two possible patterns involved:
--    1) (i.e.) mkGt <$> (... e1) <*> (... e2)
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
generate env (BinopExpr And e1 e2)              = sequence [generate env e1, generate env e2] >>= mkAnd
generate env (BinopExpr Or e1 e2)               = sequence [generate env e1, generate env e2] >>= mkOr
generate env (BinopExpr Implication e1 e2)      = join (mkImplies <$> generate env e1 <*> generate env e2)
generate env (BinopExpr LessThan e1 e2)         = join (mkLt <$> generate env e1 <*> generate env e2)
generate env (BinopExpr LessThanEqual e1 e2)    = join (mkLe <$> generate env e1 <*> generate env e2)
generate env (BinopExpr GreaterThan e1 e2)      = join (mkGt <$> generate env e1 <*> generate env e2)
generate env (BinopExpr GreaterThanEqual e1 e2) = join (mkGe <$> generate env e1 <*> generate env e2)
generate env (BinopExpr Minus e1 e2)            = sequence [generate env e1, generate env e2] >>= mkSub
generate env (BinopExpr Plus e1 e2)             = sequence [generate env e1, generate env e2] >>= mkAdd
generate env (BinopExpr Multiply e1 e2)         = sequence [generate env e1, generate env e2] >>= mkMul
generate env (BinopExpr Divide e1 e2)           = join (mkDiv <$> generate env e1 <*> generate env e2)
generate env (BinopExpr Equal e1 e2)            = join (mkEq <$> generate env e1 <*> generate env e2)
generate env expr@(BinopExpr Alias e1 e2)       = error $ "Unimplemented Z3 conversion from BinopExpr Alias: " ++ show expr

generate env (Forall s e) = do
  quantifiedVar <- mkStringSymbol s >>= mkIntVar
  let env' = Map.insert s (return quantifiedVar) env
  expr <- generate env' e
  app <- toApp quantifiedVar
  mkForallConst [] [app] expr

generate env (Exists s e) = do
  quantifiedVar <- mkStringSymbol s >>= mkIntVar
  let env' = Map.insert s (return quantifiedVar) env
  expr <- generate env' e
  app <- toApp quantifiedVar
  mkExistsConst [] [app] expr

-- Thought this was needed for arrays, but we used repby instead.
generate env (Cond cond t f)   = join $ mkIte <$> generate env cond <*> generate env t <*> generate env f
generate env (RepBy var i val) = join $ mkStore <$> generate env var <*> generate env i <*> generate env val
generate env (ArrayElem var i) = join $ mkSelect <$> generate env var <*> generate env i
generate env (SizeOf v)        = fromJust $ Map.lookup ('#':fromJust (getArrayName v)) env
generate _ expr                = error $ "Unimplemented Z3 conversion from Expr: " ++ show expr


getArrayName :: Expr -> Maybe String
-- Given a repby-expression, returns the array name.
getArrayName (RepBy e1 _ _) = getArrayName e1
getArrayName (Var e)        = Just e
getArrayName _              = Nothing

createZ3AST :: Program -> Expr -> Z3 AST
createZ3AST p = generate (initialize p)

isSatisfiable :: Program -> Expr -> IO Bool
-- Checks whether an expression is satisfiable.
-- src: https://github.com/wooshrow/gclparser/blob/master/examples/examplesHaskellZ3/Z3ProverExample.hs
isSatisfiable p e = do
  conclusion <- evalZ3 $ checker $ createZ3AST p e
  return $ conclusion == Sat
  where
    checker :: Z3 AST -> Z3 Result
    checker ast = do
      _ast <- ast
      assert _ast
      (verdict, _) <- getModel
      return verdict

isValid :: Z3Env -> Program -> Expr -> IO Bool
-- Checks whether an expression is valid.
-- src: https://github.com/wooshrow/gclparser/blob/master/examples/examplesHaskellZ3/Z3ProverExample.hs
isValid env p e = do
  let ast = createZ3AST p e
  (conclusion, _) <- evalZ3WithEnv (local $ checker ast) env

  return $ conclusion == Unsat -- Note: this should be unsat, and cost us 20 years to spot
  where
    checker :: Z3 AST -> Z3 (Result, Maybe Model)
    checker ast = do
      _ast <- ast
      f <- mkNot _ast
      assert f
      getModel
      