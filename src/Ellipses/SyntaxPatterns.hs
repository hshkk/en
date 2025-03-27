{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Ellipses.SyntaxPatterns (
    pattern (:+:),
    pattern (:-:),
    pattern (:*:),
    pattern (:::),
    pattern VNum,
    pattern ECons,
    pattern EList,
    pattern VList
) where

import Data.Maybe (listToMaybe)

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

pattern VNum :: Int -> Val
pattern VNum n <- (intify -> Just n)
    where VNum n = peanoify n

intify :: Val -> Maybe Int
intify (VCons "Zero" []) = Just 0
intify (VCons "Succ" n)  = (intify =<< listToMaybe n) >>= \k -> return $  1 + k
intify (VCons "Pred" n)  = (intify =<< listToMaybe n) >>= \k -> return $ -1 + k
intify _ = Nothing

peanoify :: Int -> Val
peanoify 0         = VCons "Zero" []
peanoify n | n > 0 = VCons "Succ" [peanoify $ n - 1]
peanoify n         = VCons "Pred" [peanoify $ n + 1]

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

pattern VListNil :: Val
pattern VListNil = VCons "Nil" []

pattern VListCons :: Val -> Val -> Val
pattern VListCons x xs = VCons "Cons" [x, xs]

pattern VList :: [Val] -> Val
pattern VList vs <- (listify -> Just vs)
    where VList vs = foldr VListCons VListNil vs

-- Flattens a list constructor into a list of its arguments.
listify :: Val -> Maybe [Val]
listify VListNil = Just []
listify (VListCons x xs) = (x:) <$> listify xs
listify _ = Nothing

pattern EList :: [Exp] -> Exp
pattern EList exps <- (expify -> Just exps)
    where EList exps = EExp (EESeg $ map SSng exps)

-- Identical to listify other than restricting convertible list segments to singular expressions,
-- as the companion pattern synonym doesn't produce ellipsis expressions.
expify :: Exp -> Maybe [Exp]
expify (EExp (EESeg [])) = Just []
expify (EExp (EESeg ((SSng x):xs))) = (x:) <$> expify (EExp $ EESeg xs)
expify _ = Nothing