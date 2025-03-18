module AntiUnification where

import Data.Data (toConstr)
import Data.Function (on)
import Data.List (nub)

import Prelude hiding (exp)

import Syntax

type Exps = [Exp]
type Subs = [(Exps, Exp)]

-- Instead of returning fst of antiunify' like in Fig. 2 (6), Østvold (2004), 
-- a function is constructed with the created substitutions using phi.
antiunify :: Exps -> (Exp, Subs)
antiunify exps = (\(a, b, _) -> (a, b)) $ antiunify' exps [] 0

-- Adapted from Fig. 2 of "A function reconstruction of anti-unification", Østvold (2004).
antiunify' :: Exps -> Subs -> Int -> (Exp, Subs, Int)
antiunify' [] _ _ = error "An empty sequence of expressions isn't allowed."
antiunify' exps subs n =
    let exp = head exps in case exps of
        -- (7)
        _ |  all (== exp) exps -> (exp, subs, n)
        -- (8) with generalization over constructor arguments.
        _ |  all (((==) `on` toConstr) exp) exps -> antiunifyG exps subs n
        -- (9)
        _ |  Just expM <- lkSub exps subs -> (expM, subs, n)
        -- (10)
        _ -> mkSub exps subs n

-- Anti-unification over constructor arguments with the guarantee that `exps` contains only one constructor type.
antiunifyG :: Exps -> Subs -> Int -> (Exp, Subs, Int)
antiunifyG [] _ _ = error "An empty sequence of expressions isn't allowed."
antiunifyG exps subs n
    = case head exps of
        EExp (EEVar x _) -> if all (x ==) [x' | EExp (EEVar x' _) <- exps] then mkSub exps subs n else error "Unable to anti-unify on mismatched ellipsis variables."
        EApp {} -> let (e1, subs1, n1) = antiunify' [e1 | EApp e1 _ <- exps] subs n
                       (e2, subs2, n2) = antiunify' [e2 | EApp _ e2 <- exps] subs1 n1 in
                   (EApp e1 e2, subs2, n2)
        EFix {} -> let (e1, subs1, n1) = antiunify' [e1 | EFix e1 _ <- exps] subs n
                       (e2, subs2, n2) = antiunify' [e2 | EFix _ e2 <- exps] subs1 n1 in
                   (EFix e1 e2, subs2, n2)
        EBin {} -> let o' = case nub [op | EBin op _ _ <- exps] of [op] -> op; _ -> error "Unable to anti-unify on mismatched binary operations."
                       (e1, subs1, n1) = antiunify' [e1 | EBin _ e1 _ <- exps] subs n
                       (e2, subs2, n2) = antiunify' [e2 | EBin _ _ e2 <- exps] subs1 n1 in
                   (EBin o' e1 e2, subs2, n2)
        _ -> error "Unable to anti-unify on mismatched expressions that aren't ellipsis variables, function applications, or binary operations."

lkSub :: Exps -> Subs -> Maybe Exp
lkSub _ [] = Nothing
lkSub exps' ((exps, sub):subs) = if exps' == exps then Just sub else lkSub exps' subs

mkSub :: Exps -> Subs -> Int -> (Exp, Subs, Int)
mkSub exps subs n = let v = EVar $ mkVar n in (v, (exps, v) : subs, n + 1)
    where mkVar x = ['v', toEnum x]

phi :: Exps -> Exp
phi [] = error "An empty sequence of expressions isn't allowed."
phi exps = let au = antiunify exps in phi' (snd au) (fst au)

phi' :: Subs -> Exp -> Exp
phi' [] e = e
phi' ((_, EVar v):vs) e = phi' vs (EAbs v e)
phi' _ _ = error "An non-variable was declared as a substitution marker."