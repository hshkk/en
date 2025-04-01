module Ellipses.AntiUnification (antiunify, Sub) where

import Data.Data (toConstr)
import Data.Function (on)
import Data.List (nub, transpose)

import Ellipses.Syntax

type Sub = ([Exp], Exp)
-- The number of parameters φ has for a given set of expressions, 
-- other than when anti-unifying over a set made up of the same expression, in which case it's 0.
type PhiVars = Int

-- Instead of returning fst of antiunify' like in Fig. 2 (6), Østvold (2004), 
-- a function (φ) is constructed with the created substitutions.
antiunify :: [Exp] -> (Exp, [Sub])
antiunify exps = (\(a, b, _) -> (a, b)) $ antiunify' exps [] 1

-- Adapted from Fig. 2 of "A function reconstruction of anti-unification", Østvold (2004).
antiunify' :: [Exp] -> [Sub] -> PhiVars -> (Exp, [Sub], PhiVars)
antiunify' [] _ _ = error "An empty sequence of expressions isn't allowed."
antiunify' exps subs n =
    let e = head exps in case exps of
        -- (7)
        _ |  all (== e) exps -> (e, subs, n)
        -- (8) with generalization over constructor arguments.
        -- Variables are processed by searching for a previous substitution 
        -- that match the current set of expressions, where a substitution is created on failure.
        _ |  toConstr e /= toConstr (EVar "x") && 
             all (on (==) toConstr e) exps -> antifold exps subs n
        -- (9)
        _ | Just expM <- lkSub exps subs -> (expM, subs, n)
        -- (10)
        _ -> mkSub exps subs n

-- Anti-unification over expressions, assuming constructor homogeneity.
antifold :: [Exp] -> [Sub] -> PhiVars -> (Exp, [Sub], PhiVars)
antifold [] _ _ = error "An empty sequence of expressions isn't allowed."
antifold exps subs n =
    let inner = [e | EExp e <- exps] in case head exps of
        EExp e | all (on (==) toConstr e) inner -> antifold' inner subs n
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
        _ -> error "Unable to anti-unify on mismatched expressions that aren't ellipsis expressions, function applications, or binary operations."

-- Anti-unification over ellipsis expressions, assuming constructor homogeneity.
antifold' :: [EExp] -> [Sub] -> PhiVars -> (Exp, [Sub], PhiVars)
antifold' [] _ _ = error "An empty sequence of ellipsis expressions isn't allowed."
antifold' eexps subs n =
    let inner = [ss | EESeg ss <- eexps]; exps = map EExp eexps in case head eexps of
        -- To anti-unify on a set of  lists, they should be equal length and have the same segment types at the same indices.
        EESeg  {} | all ((== length (head inner)) . length) inner && 
                    all ((and . zipWith (==) (map toConstr $ head inner)) . map toConstr) inner ->
            let (ssk, subsk, nk) = foldl step ([], subs, n) (transpose inner) in
                (EExp $ EESeg ssk, subsk, nk)
                where
                    -- Defines the logic of folding anti-unification over an indeterminate number of list segments.
                    -- TODO: It would be nice to generalize this logic and use it for antifold!
                    step :: ([Seg], [Sub], PhiVars) -> [Seg] -> ([Seg], [Sub], PhiVars)
                    step (ssi, subsi, ni) ssi' = let (ssj, subsj, nj) = antifolds ssi' subsi ni in (ssi ++ [ssj], subsj, nj)
        EEFold {} -> let op = case nub [op | EEFold op _ _ <- eexps] of [op] -> op; _ -> error "Unable to anti-unify on mismatched binary operations."
                         (e1, subs1, n1) = antiunify' [e1 | EEFold _ e1 _ <- eexps] subs n
                         (e2, subs2, n2) = antiunify' [e2 | EEFold _ _ e2 <- eexps] subs1 n1 in
                     (EExp $ EEFold op e1 e2, subs2, n2)
        EEVar x _ -> if all (x ==) [x' | EEVar x' _ <- eexps] then mkSub exps subs n else error "Unable to anti-unify on mismatched ellipsis variables."
        _ -> error "Unable to anti-unify on mismatched ellipsis expressions."

-- Anti-unification over list segments, assuming constructor homogeneity.
antifolds :: [Seg] -> [Sub] -> PhiVars -> (Seg, [Sub], PhiVars)
antifolds [] _ _ = error "An empty sequence of list segments isn't allowed."
antifolds ss subs n =
    case head ss of
        SSng {} -> let (e1, subs1, n1) = antiunify' [e | SSng e <- ss] subs n in (SSng e1, subs1, n1)
        SEll {} -> let (e1, subs1, n1) = antiunify' [e1 | SEll e1 _ <- ss] subs n
                       (e2, subs2, n2) = antiunify' [e2 | SEll _ e2 <- ss] subs1 n1 in
                   (SEll e1 e2, subs2, n2)

lkSub :: [Exp] -> [Sub] -> Maybe Exp
lkSub _ [] = Nothing
lkSub exps' ((exps, sub):subs) = if exps' == exps then Just sub else lkSub exps' subs

mkSub :: [Exp] -> [Sub] -> PhiVars -> (Exp, [Sub], PhiVars)
mkSub exps subs n = let v = EVar $ mkVar n in (v, (exps, v) : subs, n + 1)
    where mkVar n = 'v' : show n
