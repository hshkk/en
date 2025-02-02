module En where

import Synt

eval :: Env -> Exp -> Val
-- Con
eval _ (EVal v) = v
-- Var
eval env (EVar x) = case lookup x env of
    Just v -> v
    Nothing -> error $ x ++ " is not binded. "
-- BinOp
eval env (EBinOp e1 op e2) =
    let v1 = eval env e1
        v2 = eval env e2 in
        case (v1, v2) of
            (VNum n1, VNum n2) -> VNum (binop op (n1, n2))
            _ -> error "An illegal constructor was received."
-- Abs
eval env xe@(EAbs {}) = VCls env xe
-- AppF
eval env (EApp e1 e2) = 
    let c = eval env e1 in
        case c of
        VCls env' (EAbs x e') -> 
            let v' = eval env e2 in
                eval ((x, v') : env') e'
        _ -> error $ show e1 ++ " is not a closure."
-- AppFix
eval env (EFix e1) = case eval env e1 of
    VCls env' (EAbs f (EAbs x e')) -> eval ((f, eval env' (EAbs x e')) : env) (EApp fix e1)
    _ -> error $ show e1 ++ " is not a closure."
-- Let
eval env (ELet x e1 e2) = 
    let v' = eval env e1 in
        eval ((x, v') : env) e2
-- Case
eval _ (ECase _ []) = error "No matching patterns were found."
eval env (ECase e (a:as)) = 
    let v' = eval env e in
        -- v' | pi -> 𝜌i
        case v' of
            VNum v -> case a of
                (PVar x, e') -> eval ((x, VNum v) : env) e'
                (PVal (VNum v'), e') -> if v == v' then eval env e' else eval env (ECase e as)
                (PCon _ _, _) -> eval env (ECase e as)
                (PAny, e') -> eval env e'
                _ -> error "An illegal pattern was received."
            _ -> error $ "An illegal expression " ++ show (eval env e) ++ " was received."
-- Ellipsis expressions!
eval env (EExp ee) = case ee of
    EESeg s -> VNum 0
    EEBinOp e1 o e2 -> VNum 0
    EEVar x e -> VNum 0

binop :: BinOp -> (Int, Int) -> Int
binop op (v1, v2) = case op of
    Add -> v1 + v2
    Sub -> v1 - v2
    Mul -> v1 * v2

fix :: Exp
fix = EAbs "f" (EApp (EAbs "x" (EApp (EVar "f") (EApp (EVar "x") (EVar "x")))) (EAbs "x" (EApp (EVar "f") (EApp (EVar "x") (EVar "x")))))