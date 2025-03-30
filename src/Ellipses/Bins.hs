module Ellipses.Bins where

import Ellipses.Syntax
import Ellipses.SyntaxPatterns

(>+<) :: Val -> Val -> Val
(>+<) (VNum m) (VNum n) = VNum $ m + n
(>+<) _ _ = error "(+) is undefined for the given values."

(>-<) :: Val -> Val -> Val
(>-<) (VNum m) (VNum n) = VNum $ m - n
(>-<) _ _ = error "(-) is undefined for the given values."

(>*<) :: Val -> Val -> Val
(>*<) (VNum m) (VNum n) = VNum $ m * n
(>*<) _ _ = error "(*) is undefined for the given values."

(>:<) :: Val -> Val -> Val
(>:<) v (VList vs) = VList $ v : vs
(>:<) _ _ = error "(:) is undefined for the given values."

(>#<) :: Val -> Val -> Val
(>#<) (VList vs) (VList vs') = VList $ vs ++ vs'
(>#<) _ _ = error "(++) is undefined for the given values." 