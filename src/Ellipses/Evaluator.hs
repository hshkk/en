module Ellipses.Evaluator where

import Control.Applicative (liftA2)
import Data.Maybe (isJust, fromJust)

import Ellipses.Bins
import Ellipses.PatternInference
import Ellipses.Pretty (prettys)
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
            Cat  -> v1 >#< v2
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
            _ -> error $ prettys e1 ++ " is unable to be applied to " ++ prettys e2 ++ "."
-- AppFix
eval env (EFix e1 e2) = case eval env e1 of
    -- TODO: Rewrite this!
    VCls _ (EAbs _ (EAbs _ _)) -> eval env (EApp (EApp fix e1) e2)
        where
            fix :: Exp
            fix = EAbs "f" (EApp (EAbs "x" (EApp (EVar "f") (EApp (EVar "x") (EVar "x")))) (EAbs "x" (EApp (EVar "f") (EApp (EVar "x") (EVar "x")))))
    _ -> error $ prettys e1 ++ " isn't a closure."
-- Let
eval env (ELet x e1 e2) = let v' = eval env e1 in eval ((x, v') : env) e2
-- Case
eval _ (ECase _ []) = error "There weren't any successful pattern matches."
eval env (ECase e ((pk, ek):alts)) =
    let v = eval env e
        env' = pmatch v pk in
        if isJust env' then eval (fromJust env' ++ env) ek else eval env (ECase e alts)
-- Ellipsis expressions!
eval env (EExp e) = case e of
    EESeg ss -> let vs = map (evals env) ss in VList . cat $ vs
        where
            cat :: [Val] -> [Val]
            cat [] = []
            cat ((VList vs):vls) = vs ++ cat vls
            cat v = v
    EEFold op e1 ek -> case eval env (EExp $ EESeg [SEll e1 ek]) of
        VList vs -> eval env $ foldl1 (EBin op) (map EVal vs)
        _ -> error "This shouldn't happen!"
    EEVar x e -> case lookup x env of
        Just (VList vs) -> let n = num $ eval env e in vs !! (n - 1)
        Just _          -> error $ x ++ " isn't binded to a list."
        Nothing         -> error $ x ++ " isn't binded in the current environment."

-- Evaluation rules for list segments:

evals :: Env -> Seg -> Val
evals env (SSng e)     = VList [eval env e]
evals env (SEll e1 ek) = 
    let inf = cphi [e1, ek]
        phi = fst inf
        -- slc is [] if e1 and ek are the same expression, in which case φ will be a constant.
        -- This edge case is accounted for in the definition of zipWithK.
        slc = map (render env) (snd inf) in eval env $ zipWithK phi slc

-- Processes an a/b-slice, yielding the corresponding sublist.
render :: Env -> ABSlice -> Slice
-- Integer interpolation:
-- (Rendering an a/b-slice referencing "_" involves performing a range interpolation.)
render env (a, b, "_") = 
    let m = num $ eval env a
        n = num $ eval env b in map VNum $ range m n
-- List slicing:
render env (a, b, x) = case eval env (EVar x) of
    VList vs -> slice (num $ eval env a) (num $ eval env b) vs
    _ -> error $ x ++ " isn't binded to a list."

range :: Int -> Int -> [Int]
range m n = if m == n then [m] else [m, m + signum (n - m)..n]

slice :: Int -> Int -> [a] -> [a]
slice a b | all (> 0) [a, b] = if a > b then reverse . slice' b a else slice' a b
    where slice' a b = take (b - a + 1) . drop (a - 1)
slice _ _ = error "a >= 1, b >= 1 must hold for a list slice (a, b, x)."

num :: Val -> Int
num (VNum n) = n
num v = error $ prettys v ++ " isn't an arithmetic expression."

-- Pattern matching rules:

pmatch :: Val -> Pat -> Maybe Env
pmatch v p = try v p >>= \var -> case v of
    VCons c vs -> case p of
        PVal v' -> case v' of
            VCons c' vs' -> if c == c' && vs == vs' then Just [] else Nothing
            _ -> Nothing
        PCon c' ps -> if c == c' then concat <$> pmatfold vs ps else Nothing
        PCons x xs -> case v of
            VList [] -> Nothing
            VList vs -> liftA2 (++) (pmatch (head vs) x) (pmatch (VList $ tail vs) xs)
            _ -> Nothing
        PEll x n -> case v of
            VList [] -> Nothing -- TODO: Should ellipsis expressions match with empty lists?
            l@(VList vs) -> Just [(x, l), (n, VNum $ length vs)]
            _ -> Nothing
        _ -> Just var
    _ -> Just var
    where
        try :: Val -> Pat -> Maybe Env
        try v (PVar x) = Just [(x, v)]
        try _ _        = Just []

-- Matches every argument of a constructor against the arguments of a constructor pattern.
pmatfold :: [Val] -> [Pat] -> Maybe [Env]
pmatfold [] [] = Just []
pmatfold (v:vs) (p:ps) = (:) <$> pmatch v p <*> pmatfold vs ps
pmatfold _ _ = Nothing
