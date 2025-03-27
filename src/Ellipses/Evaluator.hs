module Ellipses.Evaluator where

import Control.Applicative (liftA2)
import Data.List (transpose)
import Data.Maybe (isJust, fromJust)

import Ellipses.Bins
import Ellipses.PatternInference
import Ellipses.Syntax
import Ellipses.SyntaxPatterns

-- Evaluation rules:

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
eval env (ECase e ((pk, ek):alts)) =
    let v = eval env e
        envs = pmatch v pk in
        if isJust envs then eval (fromJust envs ++ env) ek else eval env (ECase e alts)
-- Ellipsis expressions!
eval env (EExp e) = case e of
    EESeg ss        -> VList (map (evalseg env) ss)
    EEFold op e1 ek ->
        let inf = cphi [e1, ek]
            phi = fst inf
            slc = map (slcToList env) (snd inf) in eval env $ foldWithK op phi slc
    EEVar x e       -> case lookup x env of
        Just (VList vs) -> let n = arith $ eval env e in vs !! (n - 1)
        Just _          -> error $ x ++ " isn't binded to a list."
        Nothing         -> error $ x ++ " isn't binded in the current environment."

evalseg :: Env -> Seg -> Val
evalseg env (SSng e)     = eval env e
evalseg env (SEll e1 ek) = VNum 0 -- TODO: Implement this!

-- Maps a given φ to a list of list slices, then collapses it into a single expression.
phifold :: ([Exp] -> Exp) -> Exp -> [[Val]] -> Exp
phifold f phi vss = f $ foldl EApp phi . map EVal <$> (transpose . cut) vss
    where cut xss = map (take . minimum $ map length xss) xss

foldWithK :: Bin -> Exp -> [[Val]] -> Exp
foldWithK op = phifold (foldl1 $ EBin op)

zipWithK :: Exp -> [[Val]] -> Exp
zipWithK = phifold EList

slcToList :: Env -> (Exp, Exp, Var) -> [Val]
slcToList env (a, b, x) = case eval env (EVar x) of
    VList vs -> slice (arith $ eval env a) (arith $ eval env b) vs
    _ -> error $ x ++ " isn't binded to a list."

arith :: Val -> Int
arith (VNum n) = n
arith v = error $ show v ++ " isn't an arithmetic expression."

-- Pattern matching rules:

pmatch :: Val -> Pat -> Maybe Env
pmatch v p = try v p >>= \var -> case v of
    VNum n -> case p of
        PVal v' -> case v' of
            VNum n' -> if n == n' then Just [] else Nothing
            _ -> Nothing
        PCon    {} -> Nothing
        PCons   {} -> Nothing
        PEll    {} -> Nothing
        _ -> Just var
    VCons c vs -> case p of
        PVal v' -> case v' of
            VCons c' vs' -> if c == c' && vs == vs' then Just [] else Nothing
            _ -> Nothing
        PCon c' ps -> if c == c' then concat <$> pmatfold vs ps else Nothing
        PCons x xs -> case v of
            VList [] -> Nothing
            VList vs -> liftA2 (++) (pmatch (head vs) x) (pmatch (VList $ tail vs) xs)
            _ -> Nothing
        PEll    {} -> Nothing   -- TODO: Implement this!
        _ -> Just var
    _ -> error "The given value isn't matchable to a pattern."
    where
        try :: Val -> Pat -> Maybe Env
        try v (PVar x) = Just [(x, v)]
        try _ _        = Just []

-- Matches every argument of a constructor against the arguments of a constructor pattern.
pmatfold :: [Val] -> [Pat] -> Maybe [Env]
pmatfold [] [] = Just []
pmatfold (v:vs) (p:ps) = (:) <$> pmatch v p <*> pmatfold vs ps
pmatfold _ _ = Nothing
