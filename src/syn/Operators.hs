module Operators where

import Syntax
import SyntaxPatterns

(>+<) :: Val -> Val -> Val
(>+<) (VNum m) (VNum n) = VNum $ m + n
(>+<) _ _ = error "No binary operations exist for the given values."

(>-<) :: Val -> Val -> Val
(>-<) (VNum m) (VNum n) = VNum $ m - n
(>-<) _ _ = error "No binary operations exist for the given values."

(>*<) :: Val -> Val -> Val
(>*<) (VNum m) (VNum n) = VNum $ m * n
(>*<) _ _ = error "No binary operations exist for the given values."

(>:<) :: Val -> Val -> Val
(>:<) v (VList vs) = VList $ v : vs
(>:<) _ _ = error "No binary operations exist for the given values."