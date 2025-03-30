{-# LANGUAGE DeriveDataTypeable #-}

module Ellipses.Syntax where

import Data.Data (Data)

-- Standard type synonyms:
-- x
type Var  = String
-- c
type CVar = String

type Env  = [Bind]              -- 𝜌, {𝛽,...,𝛽}
type Bind = (Var, Val)          -- 𝛽, x=v

type Alt = (Pat, Exp)           -- p->e

-- Ellipsis type synonyms:
-- ẍ{n̈}
type EVar = String
type IVar = String

type Phi = Exp
type ABSlice = (Exp, Exp, EVar)
type Slice = [Val]

data Exp                        -- e
    = EVar      Var             -- x
    | ECon      CVar            -- c
    | EVal      Val             -- v
    | EBin      Bin Exp Exp     -- e 𝜔 e
    | EAbs      Var Exp         -- \x -> e
    | EApp      Exp Exp         -- e e
    | EFix      Exp Exp         -- fix e e
    | ELet      Var Exp Exp     -- let x=e in e 
    | ECase     Exp [Alt]       -- case e of {p->e;...;p->e}
    | EExp      EExp            -- ë
    deriving (Show, Eq, Data)

data EExp                       -- ë
    = EESeg     [Seg]           -- [𝜎, ..., 𝜎]
    | EEFold    Bin Exp Exp     -- e 𝜔 ... 𝜔 e [𝜔 e]
    | EEVar     EVar Exp        -- ẍ{e}
    deriving (Show, Eq, Data)

data Seg                        -- 𝜎
    = SSng      Exp             -- e
    | SEll      Exp Exp         -- e,...,e
    deriving (Show, Eq, Data)

data Val
    = VCons     CVar [Val]      -- c v...v
    | VCls      Env Exp         -- (𝜌,e)
    deriving (Show, Eq, Data)

data Bin
    = Add
    | Sub
    | Mul
    | Cons
    | Cat
    deriving (Show, Eq, Data)

data Pat 
    = PAny                      -- _
    | PVar      Var             -- x
    | PVal      Val             -- n
    | PCon      CVar [Pat]      -- c p...p
    | PCons     Pat Pat         -- x:xs
    | PEll      EVar IVar       -- [ẍ{1}, ..., ẍ{ï}]
    deriving (Show, Eq, Data)