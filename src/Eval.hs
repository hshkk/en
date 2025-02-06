module Eval where

import Syntax

import Data.Maybe

eval :: Env -> Exp -> Val
-- Con
eval _ (EVal v) = v
-- Var
eval env (EVar x) = case lookup x env of
    Just v -> v
    Nothing -> error $ x ++ " isn't binded in the current environment."
-- BinOp
eval env (EBinOp e1 op e2) =
    let v1 = eval env e1
        v2 = eval env e2 in
        case (v1, v2) of
            (VNum n1, VNum n2) -> VNum (bop op (n1, n2))
            _ -> error "No binary operations exist for the given values."
-- Abs
eval env xe@(EAbs {}) = VCls env xe
-- AppF
eval env (EApp e1 e2) = 
    let v = eval env e1 in
        case v of
            VCls env' (EAbs x e') -> 
                let v' = eval env e2 in
                    eval ((x, v') : env') e'
            _ -> error $ show e1 ++ " isn't a closure."
-- AppFix
eval env (EFix e1) = case eval env e1 of
    -- TODO: Rewrite this!
    VCls env' (EAbs f (EAbs x e')) -> eval ((f, eval env' (EAbs x e')) : env) (EApp fix e1)
    _ -> error $ show e1 ++ " isn't a closure."
-- Let
eval env (ELet x e1 e2) = let v' = eval env e1 in eval ((x, v') : env) e2
-- Case
eval _ (ECase _ []) = error "There weren't any successful pattern matches."
eval env (ECase e ((p, e'):as)) =
    let v' = eval env e 
        envs = pmatch v' p in
        if isJust envs then
            eval (fromJust envs ++ env) e'
        else
            eval env (ECase e as)
-- Ellipsis expressions!
eval env (EExp ee) = case ee of
    EESeg s -> VNum 0           -- TODO: Implement this!
    EEFold e1 o e2 -> VNum 0    -- TODO: Implement this!
    EEVar x e -> VNum 0         -- TODO: Implement this!

bop :: BinOp -> (Int, Int) -> Int
bop op (v1, v2) = case op of
    Add -> v1 + v2
    Sub -> v1 - v2
    Mul -> v1 * v2

pre :: Val -> Pat -> Maybe Env
pre v p = case p of
    -- PVar
    PVar x -> Just [(x, v)]
    _ -> Just []

-- Pattern matches a value against a pattern.
pmatch :: Val -> Pat -> Maybe Env
pmatch v p = pre v p >>= \r -> case v of   -- Check PVar/PAny, which are applied the same way for all values.
    VNum n -> case p of
        PVal v' -> case v' of
            VNum n' -> if n == n' then Just [] else Nothing
            _ -> Nothing
        PCon x vs -> Nothing
        PEll x i -> Nothing
        _ -> Just r
    VCon x vs -> case p of
        PVal v' -> case v' of
            VCon x' v's -> if x == x' && vs == v's then Just [] else Nothing
            _ -> Nothing
        -- PCon
        PCon x' ps -> if x == x' then concat <$> pmatchall vs ps else Nothing
        PEll x' i -> Nothing
        _ -> Just r
    _ -> error "The given value isn't matchable to a pattern."

-- Pattern matches every argument of a constructor against the arguments of a constructor pattern.
pmatchall :: [Val] -> [Pat] -> Maybe [Env]
pmatchall [] [] = Just []
pmatchall (v:vs) (p:ps) = (:) <$> pmatch v p <*> pmatchall vs ps
pmatchall _ _ = Nothing

fix :: Exp
fix = EAbs "f" (EApp (EAbs "x" (EApp (EVar "f") (EApp (EVar "x") (EVar "x")))) (EAbs "x" (EApp (EVar "f") (EApp (EVar "x") (EVar "x")))))