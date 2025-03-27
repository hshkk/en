{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Ellipses.SyntaxPatterns (
    pattern (:+:),
    pattern (:-:),
    pattern (:*:),
    pattern (:::),
    pattern ECons, 
    pattern EList,
    pattern VList
) where

import Ellipses.Syntax

-- Shorthand patterns for binary operations.

pattern (:+:) :: Exp -> Exp -> Exp
pattern x :+: y = EBin Add x y

pattern (:-:) :: Exp -> Exp -> Exp
pattern x :-: y = EBin Sub x y

pattern (:*:) :: Exp -> Exp -> Exp
pattern x :*: y = EBin Mul x y

pattern (:::) :: Exp -> Exp -> Exp
pattern x ::: y = EBin Cons x y

-- Shorthand patterns for constructors as expressions.

pattern ECons :: String -> [Exp] -> Exp
pattern ECons c exps <- (tuplify -> Just (c, exps))
    where ECons c exps = foldl EApp (ECon c) exps

-- Flattens a constructor into a tuple of its name and arguments.
tuplify :: Exp -> Maybe (String, [Exp])
tuplify (ECon c) = Just (c, [])
tuplify (EApp v1 v2) = (\(c, vs) -> (c, vs ++ [v2])) <$> tuplify v1
tuplify _ = Nothing

-- Shorthand patterns for lists as either expressions or values.

pattern EListNil :: Exp
pattern EListNil = ECons "Nil" []

pattern EListCons :: Exp -> Exp -> Exp
pattern EListCons x xs = ECons "Cons" [x, xs]

pattern EList :: [Exp] -> Exp
pattern EList exps <- (listify -> Just exps)
    where EList exps = foldr EListCons EListNil exps

pattern VListNil :: Val
pattern VListNil = VCons "Nil" []

pattern VListCons :: Val -> Val -> Val
pattern VListCons x xs = VCons "Cons" [x, xs]

pattern VList :: [Val] -> Val
pattern VList vs <- (listify' -> Just vs)
    where VList vs = foldr VListCons VListNil vs

-- Flattens a list constructor into a list of its arguments.
listify :: Exp -> Maybe [Exp]
listify EListNil = Just []
listify (EListCons x xs) = (x:) <$> listify xs
listify _ = Nothing

listify' :: Val -> Maybe [Val]
listify' VListNil = Just []
listify' (VListCons x xs) = (x:) <$> listify' xs
listify' _ = Nothing