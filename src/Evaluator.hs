module Evaluator where

import Data.Maybe (isJust, fromJust)

import AntiUnification (phi)
import Operators
import Syntax
import SyntaxPatterns

eval :: Env -> Exp -> Val
-- Con
eval _ (EVal v) = v
eval _ (ECon c) = VCons c []
-- Var
eval env (EVar x) = case lookup x env of
    Just v  -> v
    Nothing -> error $ x ++ " isn't binded in the current environment."
-- BinOp
eval env (EBin op e1 e2) =
    let v1 = eval env e1
        v2 = eval env e2 in
        case op of
            Add  -> v1 >+< v2
            Sub  -> v1 >-< v2
            Mul  -> v1 >*< v2
            Cons -> v1 >:< v2
            _ -> error $ "The binary operation " ++ show op ++ " lacks a definition."
-- Abs
eval env xe@(EAbs {}) = VCls env xe
-- App
eval env (EApp e1 e2) =
    let v  = eval env e1
        v' = eval env e2 in
        case v of
            -- AppC
            VCons c vs -> VCons c (vs ++ [v'])
            -- AppF
            VCls env' (EAbs x e') -> eval ((x, v') : env') e'
            _ -> error $ show e1 ++ " is unable to be applied to " ++ show e2 ++ "."
-- AppFix
eval env (EFix e1 e2) = case eval env e1 of
    -- TODO: Rewrite this!
    VCls _ (EAbs _ (EAbs _ _)) -> eval env (EApp (EApp fix e1) e2)
        where fix = EAbs "f" (EApp (EAbs "x" (EApp (EVar "f") (EApp (EVar "x") (EVar "x")))) (EAbs "x" (EApp (EVar "f") (EApp (EVar "x") (EVar "x")))))
    _ -> error $ show e1 ++ " isn't a closure."
-- Let
eval env (ELet x e1 e2) = let v' = eval env e1 in eval ((x, v') : env) e2
-- Case
eval _ (ECase _ []) = error "There weren't any successful pattern matches."
eval env (ECase e ((p, ek):alts)) =
    let v = eval env e
        envs = pmatch v p in
        if isJust envs then eval (fromJust envs ++ env) ek else eval env (ECase e alts)
-- Ellipsis expressions!
eval env (EExp e) = case e of
    EESeg ss       -> VList (map (evalseg env) ss)
    EEFold e1 o ek -> let p = phi [e1, ek] in eval env p -- TODO: Implement this!
    EEVar x e      -> case lookup x env of
        Just (VList vs) -> let n = arith $ eval env e in vs !! (n - 1)
            where arith (VNum n) = n
                  arith v = error $ show v ++ " isn't an arithmetic expression."
        Just _          -> error $ x ++ " isn't binded to a list."
        Nothing         -> error $ x ++ " isn't binded in the current environment."

evalseg :: Env -> Seg -> Val
evalseg env (SSng e)     = eval env e
evalseg env (SEll e1 ek) = VNum 0 -- TODO: Implement this!

try :: Val -> Pat -> Maybe Env
try v p = case p of
    -- PVar
    PVar x -> Just [(x, v)]
    _      -> Just []

-- Matches a value against a pattern.
pmatch :: Val -> Pat -> Maybe Env
pmatch v p = try v p >>= \r -> case v of
    VNum n -> case p of
        PVal v' -> case v' of
            VNum n' -> if n == n' then Just [] else Nothing
            _ -> Nothing
        PCon    {} -> Nothing
        PCons   {} -> Nothing
        PEll    {} -> Nothing
        _ -> Just r
    VCons c vs -> case p of
        PVal v' -> case v' of
            VCons c' vs' -> if c == c' && vs == vs' then Just [] else Nothing
            _ -> Nothing
        PCon c' ps -> if c == c' then concat <$> pmatchall vs ps else Nothing
        PCons x xs -> case v of
            VList vs -> if null vs then Nothing else ((<*>) . fmap (++)) (pmatch (head vs) x) (pmatch (VList $ tail vs) xs)
            _ -> Nothing
        PEll    {} -> Nothing   -- TODO: Implement this!
        _ -> Just r
    _ -> error "The given value isn't matchable to a pattern."

-- Matches every argument of a constructor against the arguments of a constructor pattern.
pmatchall :: [Val] -> [Pat] -> Maybe [Env]
pmatchall [] [] = Just []
pmatchall (v:vs) (p:ps) = (:) <$> pmatch v p <*> pmatchall vs ps
pmatchall _ _ = Nothing