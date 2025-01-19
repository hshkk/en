module En where

type Name = String

type Env = [Bind]
type Bind = (Name, Val)

data Exp
    = EVar      Name
    | ECon      Val
    | EBinOp    Exp BinOp Exp
    | EAbs      Name Exp
    | EApp      Exp Exp
    | EFix      Exp Exp
    | ELet      Name Exp Exp
    | ECase     Exp [Alt]
    deriving (Show, Eq)

data Val
    = VNum      Int
    | VCon      Val
    | VCls      Env Exp
    deriving (Show, Eq)

data BinOp
    = Add
    | Sub
    | Mul
    deriving (Show, Eq)

type Alt = (Pat, Exp)

data Pat =
    PAny
    | PVar      Name
    | PCon      Val
    deriving (Show, Eq)

eval :: Env -> Exp -> Val
eval _ (ECon c) = c

eval env (EVar x) = case lookup x env of
    Just v -> v
    Nothing -> error $ x ++ " is not binded." ++ show env

eval env (EBinOp e1 op e2) = case (eval env e1, eval env e2) of
    (VNum v1, VNum v2) -> VNum (binop op (v1, v2))
    _ -> error "An illegal constructor was received."

eval env (EAbs x e) = VCls env (EAbs x e)

eval env (EApp e1 e2) = case eval env e1 of
    VCls env' (EAbs x e') -> eval ((x, eval env e2) : env') e'
    _ -> error $ show e1 ++ " is not a closure."

-- TODO: Rewrite AppFix to remove dependency on EApp.
eval env (EFix e1 e2) = case eval env e1 of
    VCls _ (EAbs _ _) -> eval env (EApp (EApp fix e1) e2)
    _ -> error $ show e1 ++ " is not a closure."

eval env (ELet x e1 e2) = eval ((x, eval env e1) : env) e2

eval _ (ECase _ []) = error "No matching patterns were found."

eval env (ECase e (a:as)) = case eval env e of
    VNum v -> case a of
        (PVar x, e') -> eval ((x, VNum v) : env) e'
        (PCon (VNum v'), e') -> if v == v' then eval env e' else eval env (ECase e as)
        (PAny, e') -> eval env e'
        _ -> error "An illegal pattern was received."
    _ -> error $ "An illegal expression " ++ show (eval env e) ++ " was received."

binop :: BinOp -> (Int, Int) -> Int
binop op (v1, v2) = case op of
    Add -> v1 + v2
    Sub -> v1 - v2
    Mul -> v1 * v2

fix :: Exp
fix = EAbs "f" (EApp (EAbs "x" (EApp (EVar "f") (EApp (EVar "x") (EVar "x")))) (EAbs "x" (EApp (EVar "f") (EApp (EVar "x") (EVar "x")))))