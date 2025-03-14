module AntiUnification where

import Syntax hiding (VCon, VApp)

import Data.Char (ord, chr)
import Data.Data (toConstr)
import Data.Function (on)
import Data.List (nub)

import Prelude hiding (exp)

type Exps = [Exp]
type Subs = [(Exps, Exp)]

-- Instead of returning fst of antiunify' like in Fig. 2 (6), Østvold (2004), 
-- a function is constructed with the created substitutions using phi.
antiunify :: Exps -> (Exp, Subs)
-- (6) with a minor revision.
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
        -- (9) on variables.
        _ |  Just expM <- lkSub exps subs -> (expM, subs, n)
        -- (10)
        _ -> mkSub exps subs n

-- Anti-unification over constructor arguments with the guarantee that `exps` contains only one constructor type.
antiunifyG :: Exps -> Subs -> Int -> (Exp, Subs, Int)
antiunifyG [] _ _ = error "An empty sequence of expressions isn't allowed."
antiunifyG exps subs n
    = case head exps of
        EVar {}     -> mkSub exps subs n
        EVal {}     -> mkSub exps subs n
        EApp {}     -> let (e', subs', n')    = antiunify' [e1 | EApp e1 _ <- exps] subs n
                           (e'', subs'', n'') = antiunify' [e2 | EApp _ e2 <- exps] subs' n' in
                       (EApp e' e'', subs'', n'')
        EBinOp {}   -> let (e', subs', n') = antiunify' [e1 | EBinOp e1 _ _ <- exps] subs n
                           o' = case nub [op | EBinOp _ op _ <- exps] of [op] -> op; _ -> error "Unable to anti-unify on mismatched binary operations."
                           (e'', subs'', n'') = antiunify' [e2 | EBinOp _ _ e2 <- exps] subs' n' in
                       (EBinOp e' o' e'', subs'', n'')
        _ -> error "Unable to anti-unify on expressions that aren't variables, values, function applications, or binary operations."

-- A helper function for (9).
lkSub :: Exps -> Subs -> Maybe Exp
lkSub _ [] = Nothing
lkSub e ((k, v):ks) = if e == k then Just v else lkSub e ks

-- A helper function for (10).
mkSub :: Exps -> Subs -> Int -> (Exp, Subs, Int)
mkSub exps subs n = let v = EVar $ mkVar n in (v, (exps, v) : subs, n + 1)
    where mkVar x = 'v' : [chr $ ord '1' + x]

phi :: Exps -> Exp
phi [] = error "An empty sequence of expressions isn't allowed."
phi exps = let au = antiunify exps in phi' (snd au) (fst au)

phi' :: Subs -> Exp -> Exp
phi' [] e = e
phi' ((_, EVar v):vs) e = phi' vs (EAbs v e)
phi' _ _ = error "An non-variable was declared as a substitution marker."