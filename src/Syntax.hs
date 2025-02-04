module Syntax where

type Name = String

type Env = [Bind]               -- 𝜌 {𝛽,...,𝛽}
type Bind = (Name, Val)         -- 𝛽 x=v

data Exp                        -- e
    = EVar      Name            -- x
    | EVal      Val             -- v
    | EBinOp    Exp BinOp Exp   -- e 𝜔 e
    | EAbs      Name Exp        -- \x -> e
    | EApp      Exp Exp         -- e e
    | EFix      Exp
    | ELet      Name Exp Exp    -- let x=e in e 
    | ECase     Exp [Alt]       -- case e of {p->e;...;p->e}
    | EExp      EExp            -- ...e
    deriving (Show, Eq)

data EExp                       -- ...e
    = EESeg     [Seg]           -- [𝜎, ..., 𝜎]
    | EEFold    Exp BinOp Exp   -- e 𝜔 ... 𝜔 e [𝜔 e]
    | EEVar     Name Exp        -- ...x{e}
    deriving (Show, Eq)

data Seg                        -- 𝜎
    = SSng      Exp             -- e
    | SEll      Exp Exp         -- e,...,e
    deriving (Show, Eq)

data Val
    = VNum      Int             -- n
    | VCon      Name [Val]      -- c v...v
    | VCls      Env Exp         -- (𝜌,e)
    deriving (Show, Eq)

data BinOp
    = Add
    | Sub
    | Mul
--  | Cons
    deriving (Show, Eq)

type Alt = (Pat, Exp)

data Pat =
    PAny
    | PVar      Name
    | PVal      Val
    | PCon      Name [Val]
    deriving (Show, Eq)