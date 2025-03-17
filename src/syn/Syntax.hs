{-# LANGUAGE DeriveDataTypeable #-}

module Syntax where

import Data.Data (Data)

type Var = String
type CVar = String
-- "Ellipsis variables"
type EVar = String
type IVar = String

type Env = [Bind]               -- 𝜌 {𝛽,...,𝛽}
type Bind = (Var, Val)          -- 𝛽 x=v

data Exp                        -- e
    = EVar      Var             -- x
    | ECon      CVar            -- c
    | EVal      Val             -- v
    | EBin      Bin Exp Exp     -- e 𝜔 e
    | EAbs      Var Exp         -- \x -> e
    | EApp      Exp Exp         -- e e
    | EFix      Exp Exp
    | ELet      Var Exp Exp     -- let x=e in e 
    | ECase     Exp [Alt]       -- case e of {p->e;...;p->e}
    | EExp      EExp            -- ...e
    deriving (Show, Eq, Data)

data EExp                       -- ...e
    = EESeg     [Seg]           -- [𝜎, ..., 𝜎]
    | EEFold    Exp Bin Exp     -- e 𝜔 ... 𝜔 e [𝜔 e]
    | EEVar     EVar Exp        -- ẍ{e}
    deriving (Show, Eq, Data)

data Seg                        -- 𝜎
    = SSng      Exp             -- e
    | SEll      Exp Exp         -- e,...,e
    deriving (Show, Eq, Data)

data Val
    = VNum      Int             -- n
    | VCons     CVar [Val]      -- c v...v
    | VCls      Env Exp         -- (𝜌,e)
    deriving (Show, Eq, Data)

data Bin
    = Add
    | Sub
    | Mul
    | Cons
    deriving (Show, Eq, Data)

type Alt = (Pat, Exp)

data Pat =
    PAny
    | PVar      Var             -- x
    | PVal      Val             -- n
    | PCon      CVar [Pat]      -- c p...p
    | PCons     Var Var         -- x:xs
    | PEll      EVar IVar       -- [ẍ{1}, ..., ẍ{ï}]
    deriving (Show, Eq, Data)