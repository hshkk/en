{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module SyntaxExtras where

import Syntax

-- Shorthand patterns for binary operations:

pattern (:+:) :: Exp -> Exp -> Exp
pattern x :+: y = EBinOp x Add y

pattern (:-:) :: Exp -> Exp -> Exp
pattern x :-: y = EBinOp x Sub y

pattern (:*:) :: Exp -> Exp -> Exp
pattern x :*: y = EBinOp x Mul y

pattern (:::) :: Exp -> Exp -> Exp
pattern x ::: y = EBinOp x Cons y

-- Shorthand patterns for constructors:

-- AppC
pattern VCons :: String -> [Val] -> Val
pattern VCons c vs <- (tuplify -> Just (c, vs))
    where VCons c vs = foldl VApp (VCon c) vs

-- Flattens a constructor into a tuple of its name and arguments.
tuplify :: Val -> Maybe (String, [Val])
tuplify (VCon c) = Just (c, [])
tuplify (VApp v1 v2) = (\(c, vs) -> (c, vs ++ [v2])) <$> tuplify v1
tuplify _ = Nothing

-- Shorthand patterns for lists:

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