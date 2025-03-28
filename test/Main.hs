module Main (main) where

import Data.List (intercalate)
import Prettyprinter (Pretty(..))
import System.Exit (exitFailure, exitSuccess)
import Test.HUnit

import Ellipses

tests :: Test
tests =
    TestList [
        maptests,
        ziptests
    ]
    
testm :: String -> Exp -> [String] -> [Exp] -> String
testm f x pars exps =
    "Testing " ++ f ++ " = " ++ (show . pretty) x ++ " where " ++
    intercalate ", " (zipWith (\p e -> p ++ " = " ++ (show . pretty) e) pars exps) ++ "."

main :: IO ()
main = do
    r <- runTestTT tests
    if failures r > 0 then exitFailure else exitSuccess

-- Example expressions written with ellipsis notation:

-- l1 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
l1 :: Exp
l1 =
    EList [
        EVal (VNum 1),
        EVal (VNum 2),
        EVal (VNum 3),
        EVal (VNum 4),
        EVal (VNum 5),
        EVal (VNum 6),
        EVal (VNum 7),
        EVal (VNum 8),
        EVal (VNum 9),
        EVal (VNum 10)
    ]

-- l2 = [(1, 2, 3), (2, 4, 6), (3, 6, 9), (4, 8, 12), (5, 10, 15)]
l2 :: Exp
l2 =
    EList [
        ECons "Triple" [EVal (VNum 1), EVal (VNum 2), EVal (VNum 3)],
        ECons "Triple" [EVal (VNum 2), EVal (VNum 4), EVal (VNum 6)],
        ECons "Triple" [EVal (VNum 3), EVal (VNum 6), EVal (VNum 9)],
        ECons "Triple" [EVal (VNum 4), EVal (VNum 8), EVal (VNum 12)],
        ECons "Triple" [EVal (VNum 5), EVal (VNum 10), EVal (VNum 15)]
    ]

-- list3 = [[10, 8, 6, 4, 2], [1, 3, 5, 7, 9], [10, 9, 8, 7, 6], [5, 4, 3, 2, 1]]
l3 :: Exp
l3 =
    EList [
        EVal (VList [VNum 10, VNum 8, VNum 6, VNum 4, VNum 2]),
        EVal (VList [VNum 1, VNum 3, VNum 5, VNum 7, VNum 9]),
        EVal (VList [VNum 10, VNum 9, VNum 8, VNum 7, VNum 6]),
        EVal (VList [VNum 5, VNum 4, VNum 3, VNum 2, VNum 1])
    ]

-- map f [] = []
-- map f [x1, ..., xn] = [f x1, ..., f xn]
emap :: Exp
emap =
    EAbs "f" $ EAbs "x" (ECase (EVar "x") [
        (PVal $ VList [], EVal $ VList []),
        (PEll "x" "n", EExp $ EESeg [
            SEll
                (EApp (EVar "f") (EExp $ EEVar "x" (EVal $ VNum 1)))
                (EApp (EVar "f") (EExp $ EEVar "x" (EVar "n")))
        ])
    ])

-- emapl1 x = x + 1
emapl1 :: Exp
emapl1 = EAbs "x" (EVar "x" :+: EVal (VNum 1))

-- emapl1r [2, 3, 4, 5, 6, 7, 8, 9, 10, 11]
emapl1r :: Exp
emapl1r =
    EList [
        EVal (VNum 2),
        EVal (VNum 3),
        EVal (VNum 4),
        EVal (VNum 5),
        EVal (VNum 6),
        EVal (VNum 7),
        EVal (VNum 8),
        EVal (VNum 9),
        EVal (VNum 10),
        EVal (VNum 11)
    ]

-- emapl2 x = case x of {(a, b, c) -> a * b * c; _ -> 0}
emapl2 :: Exp
emapl2 =
    EAbs "x" (ECase (EVar "x") [
        (PCon "Triple" [PVar "a", PVar "b", PVar "c"], EVar "a" :*: EVar "b" :*: EVar "c"),
        (PAny, EVal $ VNum 0)
    ])

-- emapl2r = [6, 48, 162, 384, 750]
emapl2r :: Exp
emapl2r =
    EList [
        EVal $ VNum 6,
        EVal $ VNum 48,
        EVal $ VNum 162,
        EVal $ VNum 384,
        EVal $ VNum 750
    ]

-- emapl3 [] = []
-- emapl3 [x1, ..., xn] = [xn, ..., x1]
emapl3 :: Exp
emapl3 =
    EAbs "x" (ECase (EVar "x") [
        (PVal (VList []), EVal (VList [])),
        (PEll "x" "n", EExp (EESeg [
            SEll
                (EExp (EEVar "x" (EVar "n")))
                (EExp (EEVar "x" (EVal (VNum 1))))
        ]))
    ])

-- emapl3r = [[2, 4, 6, 8, 10], [9, 7, 5, 3, 1], [6, 7, 8, 9, 10], [1, 2, 3, 4, 5]]
emapl3r :: Exp
emapl3r =
    EList [
        EVal (VList [VNum 2, VNum 4, VNum 6, VNum 8, VNum 10]),
        EVal (VList [VNum 9, VNum 7, VNum 5, VNum 3, VNum 1]),
        EVal (VList [VNum 6, VNum 7, VNum 8, VNum 9, VNum 10]),
        EVal (VList [VNum 1, VNum 2, VNum 3, VNum 4, VNum 5])
    ]

maptests :: Test
maptests =
    TestList [
        TestCase (assertEqual (testm "map" emap ["f", "x"] [emapl1, l1]) (eval [] emapl1r) (eval [] (EApp (EApp emap emapl1) l1))),
        TestCase (assertEqual (testm "map" emap ["f", "x"] [emapl2, l2]) (eval [] emapl2r) (eval [] (EApp (EApp emap emapl2) l2))),
        TestCase (assertEqual (testm "map" emap ["f", "x"] [emapl3, l3]) (eval [] emapl3r) (eval [] (EApp (EApp emap emapl3) l3)))
    ]

-- zip [] _ = []
-- zip _ [] = []
-- zip [x1, ..., xn] [y1, ..., ym] = [(x1, y1), ..., (xn, ym)]
ezip :: Exp
ezip =
    EAbs "x" $ EAbs "y" (ECase (EVar "x") [
        (PVal $ VList [], EVal $ VList []),
        (PEll "x" "n", ECase (EVar "y") [
            (PVal $ VList [], EVal $ VList []),
            (PEll "y" "m", EExp $ EESeg [
                SEll
                    (ECons "Pair" [
                        EExp $ EEVar "x" (EVal $ VNum 1),
                        EExp $ EEVar "y" (EVal $ VNum 1)
                    ])
                    (ECons "Pair" [
                        EExp $ EEVar "x" (EVar "n"),
                        EExp $ EEVar "y" (EVar "m")
                    ])
            ])
        ])
    ])

-- ezipl1r = [(1, 1), (2, 2), (3, 3), (4, 4), (5, 5), (6, 6), (7, 7), (8, 8), (9, 9), (10, 10)]
ezipl1r :: Exp
ezipl1r =
    EList [
        ECons "Pair" [EVal (VNum 1), EVal (VNum 1)],
        ECons "Pair" [EVal (VNum 2), EVal (VNum 2)],
        ECons "Pair" [EVal (VNum 3), EVal (VNum 3)],
        ECons "Pair" [EVal (VNum 4), EVal (VNum 4)],
        ECons "Pair" [EVal (VNum 5), EVal (VNum 5)],
        ECons "Pair" [EVal (VNum 6), EVal (VNum 6)],
        ECons "Pair" [EVal (VNum 7), EVal (VNum 7)],
        ECons "Pair" [EVal (VNum 8), EVal (VNum 8)],
        ECons "Pair" [EVal (VNum 9), EVal (VNum 9)],
        ECons "Pair" [EVal (VNum 10), EVal (VNum 10)]
    ]

ziptests :: Test
ziptests =
    TestList [
        TestCase (assertEqual (testm "zip" emap ["x", "y"] [l1, l1]) (eval [] ezipl1r) (eval [] (EApp (EApp ezip l1) l1)))
    ]
