module AntiUnification where

import Syntax

import Data.Char (ord, chr)

type Exps = [Exp]
type Subst a b = [(a, b)]

lsub :: Exps -> Subst Exps Exp -> Maybe Exp
lsub _ [] = Nothing
lsub e ((k, v):ks) = if e == k then Just v else lsub e ks

-- Adapted from Fig. 2 of "A function reconstruction of anti-unification", Østvold (2004).
antiunify' :: Exps -> Subst Exps Exp -> Int -> (Exp, Subst Exps Exp, Int)
antiunify' [] _ _ = error "An empty sequence of expressions isn't allowed."
antiunify' exps subs n = case exps of
    -- (7)
    (e:es)  | all (== e) es -> (e, subs, n)
    -- (8) TODO: Implement this!
    -- (9)
    es      | Just e <- lsub es subs -> (e, subs, n)
    -- (10)
    es -> 
        let v = EVar (fresh 'v' n)
            subs' = (es, v) : subs in 
            (v, subs', n + 1)
        where fresh c n = c : [chr $ ord '1' + n]

-- Instead of returning fst of antiunify' like in Fig. 2 (6), 
-- a function is constructed with the created substitutions using phi.
antiunify :: Exps -> (Exp, Subst Exps Exp)
antiunify exps = (\(a, b, _) -> (a, b)) $ antiunify' exps [] 0

phi' :: Subst Exps Exp -> Exp -> Exp
phi' [] e = e
phi' ((_, EVar v):vs) e = phi' vs (EAbs v e)
phi' ((_, _):_) _ = error "An non-variable was declared as a substitution marker."

phi :: Exps -> Exp
phi [] = error "An empty sequence of expressions isn't allowed."
phi exps = let au = antiunify exps in phi' (snd au) (fst au)