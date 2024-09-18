module TestZ3 where

import Z3.Monad
-----------------------------------------------------------------------

-- (1 < x) ==> ((0 < x) ==> ((0 < (x - 1)) ==> (~(0 < ((x - 1) - 1)) ==> (((x - 1) - 1) = 1))))
-- f0 ==> (f1 ==> (f2 ==> (f3 ==> f4)))

f0 :: Z3 AST
f0 = do
  _1 <- mkIntNum 1
  _x <- mkIntVar =<< mkStringSymbol "x"
  _1 `mkLt` _x

f1 :: Z3 AST
f1 = do
  _0 <- mkIntNum 0
  _x <- mkIntVar =<< mkStringSymbol "x"
  _0 `mkLt` _x

f2 :: Z3 AST
f2 = do
  _0 <- mkIntNum 0
  _x <- mkIntVar =<< mkStringSymbol "x"
  _1 <- mkIntNum 1
  _minus1 <- mkSub [_x, _1]
  _0 `mkLt` _minus1

f3 :: Z3 AST
f3 = do
  _0 <- mkIntNum 0
  _x <- mkIntVar =<< mkStringSymbol "x"
  _1 <- mkIntNum 1
  _minus1 <- mkSub [_x, _1]
  _minus1' <- mkSub [_minus1, _1]
  mkNot =<< _0 `mkLt` _minus1'

f4 :: Z3 AST
f4 = do 
  _x <- mkIntVar =<< mkStringSymbol "x"
  _1 <- mkIntNum 1
  _minus1 <- mkSub [_x, _1]
  _minus1' <- mkSub [_minus1, _1]
  _minus1' `mkEq` _1

f5 :: Z3 AST
f5 = do
  f0_ <- f0
  f1_ <- f1
  f2_ <- f2
  f3_ <- f3
  f4_ <- f4
  r <- f3_ `mkImplies` f4_
  m <- f2_ `mkImplies` r
  l <- f1_ `mkImplies` m
  f0_ `mkImplies` l

-- An example of checking if a formula is satisfiable.
-- We will use the formula f1 (x>1) as an example. This
-- formula is satisfiable, so Z3 should come with the same
-- conclusion:
testSAT :: IO()
testSAT =
  let
  -- Construct a Z3 checker. It will "assert" the formula f1, then
  -- check if it is satisfiable. The checker returns (inside Z3-monad)
  -- either Sat or Unsat.
  checker :: Z3 Result
  checker = do
        f5_ <- f5
        assert f5_                   -- asserting the formula to check
        (verdict,model) <- getModel  -- checking if the formula is satisfiable
        return verdict               -- returning the verdict Sat or Unsat
  in
  -- Next, we run the checker. The we check if the returned verdict is Sat,
  -- and if so we print some Yay-message.
  do verdict <- evalZ3 checker
     if verdict == Sat
        then print "The formula f1 is satisfiable."
        else print "The formula f1 is UNsatisfiable."


-- An example of checking if a formula is valid.
-- We will use the formula f2 (x>1 ==> x>0) as an example. This
-- formula is valid, so Z3 should come with the same
-- conclusion. Because the API provided by Z3-binding can only
-- check satisfiability, we will instead check if "not f2" is
-- satisfiable. If it is, then f2 is not valid. If "not f2" is
-- unsatisfiable, then f2 is valid.
testVALID :: IO()
testVALID =
  let
  -- Construct a Z3 checker. It will "assert" the formula not-f2, then
  -- check if it is satisfiable. The checker returns (inside Z3-monad)
  -- either Sat or Unsat.
  checker :: Z3 (Result, String)
  checker = do
    f5_ <- f5
    f <- mkNot f5_
    --  _simp <- simplify _ast
    setASTPrintMode Z3_PRINT_SMTLIB_FULL
    str <- astToString _f5
    assert f                         -- asserting the formula to check
    (verdict, _) <- getModel  -- checking if the formula is satisfiable
    return (verdict, str)              -- returning the verdict Sat or Unsat
  in
  -- Next, we run the checker. The we check if the returned verdict is Unsat,
  -- and if so we print some Yay-message.
  do (verdict, str) <- evalZ3 checker
    print str
     if verdict == Unsat
        then print "The formula f2 is valid."
        else print "The formula f2 is invalid."