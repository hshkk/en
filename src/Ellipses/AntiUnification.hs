module Ellipses.AntiUnification (antiunify, Sub) where

import Data.Data (toConstr)
import Data.Function (on)
import Data.List (nub)

import Ellipses.Syntax

type Sub = ([Exp], Exp)

-- Instead of returning fst of antiunify' like in Fig. 2 (6), Østvold (2004), 
-- a function (φ) is constructed with the created substitutions.
antiunify :: [Exp] -> (Exp, [Sub])
antiunify exps = (\(a, b, _) -> (a, b)) $ antiunify' exps [] 1

-- Adapted from Fig. 2 of "A function reconstruction of anti-unification", Østvold (2004).
antiunify' :: [Exp] -> [Sub] -> Int -> (Exp, [Sub], Int)
antiunify' [] _ _ = error "An empty sequence of expressions isn't allowed."
antiunify' exps subs n =
    let e = head exps in case exps of
        -- (7)
        _ |  all (== e) exps -> (e, subs, n)
        -- (8) with generalization over constructor arguments.
        _ |  all (on (==) toConstr e) exps -> antifold exps subs n
        -- (9)
        _ |  Just expM <- lkSub exps subs -> (expM, subs, n)
        -- (10)
        _ -> mkSub exps subs n

-- Anti-unification over constructor arguments, assuming constructor homogeneity.
antifold :: [Exp] -> [Sub] -> Int -> (Exp, [Sub], Int)
antifold [] _ _ = error "An empty sequence of expressions isn't allowed."
antifold exps subs n
    = case head exps of
        EExp (EEVar x _) -> if all (x ==) [x' | EExp (EEVar x' _) <- exps] then mkSub exps subs n else error "Unable to anti-unify on mismatched ellipsis variables."
        -- TODO: Consolidating the similar logic below may involve generically folding over constructor arguments.
        EApp {} -> let (e1, subs1, n1) = antiunify' [e1 | EApp e1 _ <- exps] subs n
                       (e2, subs2, n2) = antiunify' [e2 | EApp _ e2 <- exps] subs1 n1 in
                   (EApp e1 e2, subs2, n2)
        EFix {} -> let (e1, subs1, n1) = antiunify' [e1 | EFix e1 _ <- exps] subs n
                       (e2, subs2, n2) = antiunify' [e2 | EFix _ e2 <- exps] subs1 n1 in
                   (EFix e1 e2, subs2, n2)
        EBin {} -> let op = case nub [op | EBin op _ _ <- exps] of [op] -> op; _ -> error "Unable to anti-unify on mismatched binary operations."
                       (e1, subs1, n1) = antiunify' [e1 | EBin _ e1 _ <- exps] subs n
                       (e2, subs2, n2) = antiunify' [e2 | EBin _ _ e2 <- exps] subs1 n1 in
                   (EBin op e1 e2, subs2, n2)
        _ -> error "Unable to anti-unify on mismatched expressions that aren't ellipsis variables, function applications, or binary operations."

lkSub :: [Exp] -> [Sub] -> Maybe Exp
lkSub _ [] = Nothing
lkSub exps' ((exps, sub):subs) = if exps' == exps then Just sub else lkSub exps' subs

mkSub :: [Exp] -> [Sub] -> Int -> (Exp, [Sub], Int)
mkSub exps subs n = let v = EVar $ mkVar n in (v, (exps, v) : subs, n + 1)
    where mkVar n = 'v' : show n