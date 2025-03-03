{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Patterns where
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

-- Shorthand patterns for lists:

pattern VListNil :: Val
pattern VListNil = VCon "Nil" []

pattern VListCons :: Val -> Val -> Val
pattern VListCons x xs = VCon "Cons" [x, xs]

pattern VList :: [Val] -> Val
pattern VList vs <- (listify -> Just vs)
    where VList vs = foldr VListCons VListNil vs

listify :: Val -> Maybe [Val]
listify VListNil = Just []
listify (VListCons x x') | Just xs <- listify x' = Just (x:xs) 
listify _ = Nothing