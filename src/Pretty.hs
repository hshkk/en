{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Pretty where

import Syntax

import Prettyprinter
import qualified Data.Text as T

instance Pretty Exp where
    pretty (EVar x)                 = pretty (T.pack x)
    pretty (EVal v)                 = pretty v
    pretty (EBinOp e1 op e2)        = pretty e1 <+> pretty op <+> pretty e2
    pretty (EAbs x e)               = backslash <> pretty x <+> "->" <+> pretty e
    pretty (EApp e1 e2@(EBinOp {})) = pretty e1 <+> parens (pretty e2)
    pretty (EApp e1 (EApp e2 e3))   = pretty e1 <+> parens (pretty $ EApp e2 e3)
    pretty (EApp e1 e2)             = pretty e1 <+> pretty e2
    pretty (EFix e1)                = "fix" <+> parens (pretty e1)
    pretty (ELet x e1 e2)           = "let" <+> pretty x <+> equals <+> pretty e1 <+> "in" <+> pretty e2
    pretty (ECase e as)             = "case" <+> pretty e <+> "of" <+> braces (prettyAlts as)
    pretty (EExp e)                 = pretty e

instance Pretty EExp where
    pretty (EESeg s)                = brackets $ hsep $ punctuate comma $ map pretty s
    pretty (EEFold e1 op e2)        = pretty e1 <+> pretty op <+> "..." <+> pretty op <+> pretty e2
    pretty (EEVar x e)              = pretty (T.pack x) <> braces (pretty e)

instance Pretty Seg where
    pretty (SSng e)                 = pretty e
    pretty (SEll e1 e2)             = hsep $ punctuate comma [pretty e1, "...", pretty e2]

instance Pretty Val where
    pretty (VNum n)                 = pretty n
    pretty (VCon x vs)              = pretty x <+> prettyVals vs
    pretty (VCls env e)             = parens $ pretty env <> comma <+> pretty e

prettyVals :: [Val] -> Doc ann
prettyVals vs                       = hsep $ map pretty vs

instance Pretty BinOp where
    pretty Add                      = pretty (T.pack "+")
    pretty Sub                      = pretty (T.pack "-")
    pretty Mul                      = pretty (T.pack "*")

prettyAlt :: Alt -> Doc ann
prettyAlt (p, e)                    = pretty p <+> "->" <+> pretty e

prettyAlts :: [Alt] -> Doc ann
prettyAlts as                       = hsep $ punctuate semi (map prettyAlt as)

instance Pretty Pat where
    pretty PAny                     = pretty (T.pack "_")
    pretty (PVar x)                 = pretty (T.pack x)
    pretty (PVal v)                 = pretty v
    pretty (PCon x vs)              = pretty x <+> prettyVals vs