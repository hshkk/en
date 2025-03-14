module Eval where

import AntiUnification (phi)
import Syntax hiding (VCon, VApp)
import SyntaxExtras hiding (listify, tuplify)

import Data.Maybe

eval :: Env -> Exp -> Val
-- Con
eval _ (EVal v) = v
-- Var
eval env (EVar x) = case lookup x env of
    Just v  -> v
    Nothing -> error $ x ++ " isn't binded in the current environment."
-- BinOp
eval env (EBinOp e1 op e2) =
    let v1 = eval env e1
        v2 = eval env e2 in
        case (v1, v2) of
            (VNum n1, VNum n2) -> VNum (bopnum op (n1, n2))
            (v, VList vs)      -> VList (boplist op (v, vs))
            _ -> error "No binary operations exist for the given values."
-- Abs
eval env xe@(EAbs {}) = VCls env xe
-- AppF
eval env (EApp e1 e2) =
    let v = eval env e1 in
        case v of
            VCls env' (EAbs x e') -> let v' = eval env e2 in eval ((x, v') : env') e'
            _ -> error $ show e1 ++ " isn't a closure."
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
        Just _          -> error $ x ++ " isn't binded to a list."
        Nothing         -> error $ x ++ " isn't binded in the current environment."

evalseg :: Env -> Seg -> Val
evalseg env (SSng e)     = eval env e
evalseg env (SEll e1 ek) = VNum 0 -- TODO: Implement this!

arith :: Val -> Int
arith v = case v of
    VNum n -> n
    _      -> error $ show v ++ " isn't an arithmetic expression."

bopnum :: BinOp -> (Int, Int) -> Int
bopnum op (v1, v2) = case op of
    Add -> v1 + v2
    Sub -> v1 - v2
    Mul -> v1 * v2
    _   -> error $ "You can't use " ++ show op ++ " here."

boplist :: BinOp -> (a, [a]) -> [a]
boplist op (v1, v2) = case op of
    Cons -> v1 : v2
    _    -> error $ "You can't use " ++ show op ++ " here."

pre :: Val -> Pat -> Maybe Env
pre v p = case p of
    -- PVar
    PVar x -> Just [(x, v)]
    _      -> Just []

-- Matches a value against a pattern.
pmatch :: Val -> Pat -> Maybe Env
pmatch v p = pre v p >>= \r -> case v of   -- Check PVar/PAny, which are applied the same way for all values.
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
            VList vs -> if null vs then Nothing else Just [(x, a), (xs, VList b)]
                where
                    a = head vs
                    b = tail vs
            _ -> Nothing
        PEll    {} -> Nothing   -- TODO: Implement this!
        _ -> Just r
    _ -> error $ "The given value isn't matchable to a pattern."

-- Matches every argument of a constructor against the arguments of a constructor pattern.
pmatchall :: [Val] -> [Pat] -> Maybe [Env]
pmatchall [] [] = Just []
pmatchall (v:vs) (p:ps) = (:) <$> pmatch v p <*> pmatchall vs ps
pmatchall _ _ = Nothing
