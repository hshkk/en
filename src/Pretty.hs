{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Pretty where

import En

import Prettyprinter
import qualified Data.Text as T

instance Pretty Exp where
    pretty (EVar x)                 = pretty (T.pack x)
    pretty (ECon v)                 = pretty v
    pretty (EBinOp e1 op e2)        = pretty e1 <+> pretty op <+> pretty e2
    pretty (EAbs x e)               = "\\" <> pretty x <+> "->" <+> pretty e 
    pretty (EApp e1 e2@(EBinOp {})) = pretty e1 <+> "(" <> pretty e2 <> ")"
    pretty (EApp e1 (EApp e2 e3))   = pretty e1 <+> "(" <> pretty (EApp e2 e3) <> ")"
    pretty (EApp e1 e2)             = pretty e1 <+> pretty e2
    pretty (EFix e1 e2)             = "fix" <+> "(" <> pretty e1 <> ")" <+> pretty e2
    pretty (ELet x e1 e2)           = "let" <+> pretty x <+> "=" <+> pretty e1 <+> "in" <+> pretty e2
    pretty (ECase e as)             = "case" <+> pretty e <+> "of" <+> "{" <+> prettyAlts as <+> "}"

instance Pretty Val where
    pretty (VNum n)                 = pretty n
    pretty (VCon v)                 = pretty v
    pretty (VCls env e)             = "(" <> pretty env <> "," <+> pretty e <> ")"

instance Pretty BinOp where
    pretty Add                      = pretty (T.pack "+")
    pretty Sub                      = pretty (T.pack "-")
    pretty Mul                      = pretty (T.pack "*")

prettyAlt :: Alt -> Doc ann
prettyAlt (p, e)                    = pretty p <+> "->" <+> pretty e

prettyAlts :: [Alt] -> Doc ann
prettyAlts as                        = hsep $ punctuate comma (map prettyAlt as)

instance Pretty Pat where
    pretty PAny                     = pretty (T.pack "_")
    pretty (PVar x)                 = pretty (T.pack x)
    pretty (PCon v)                 = pretty v