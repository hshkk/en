module Ellipses.Parser (repl, replWith, Repl (..)) where

import Data.Char (isUpper)
import Data.Data (Data(toConstr))
import Data.Functor.Identity (Identity)
import Prettyprinter (Pretty(..))
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.String (Parser)

import Ellipses.Evaluator
import Ellipses.Lexer hiding (lexer)
import Ellipses.Pretty ()
import Ellipses.Syntax
import Ellipses.SyntaxPatterns

type Decl = (Maybe Var, Exp)
data Repl = EVTO | AST

parseExp :: Parser Exp
parseExp = choice [parseBinExp]
    where parseBinExp = buildExpressionParser tableBin $ choice [parseApplike, parseTermlike]

parseApplike :: Parser Exp
parseApplike = choice [parseFix, parseApp]

parseTermlike :: Parser Exp
parseTermlike = choice [parseAbs, parseLet, parseCase, parseVal]

parseVal :: Parser Exp
parseVal = try parseLit <|> parseVar

parseLit :: Parser Exp
parseLit = choice
    [ EVal . VNum . read <$> many1 digit <* spaces
    , brackets parseList]

parseList :: Parser Exp
parseList = do
    exps <- sepBy parseExp (symbol ",")
    return $ EList exps

parseVar :: Parser Exp
parseVar = do
    x <- idn
    if not (null x) && isUpper (head x) then return $ ECon x else return $ EVar x

parseAbs :: Parser Exp
parseAbs = do
    _ <- symbol "\\"
    x <- idn
    reservedOp "->"
    e <- parseExp
    return $ EAbs x e

parseApp :: Parser Exp
parseApp = chainl1 (choice [parens parseExp, parseTermlike]) (EApp <$ spaces)

parseFix :: Parser Exp
parseFix = do
    reserved "fix"
    e1 <- choice $ map parens [parseExp, parseTermlike]
    e2 <- choice [parens parseExp, parseTermlike]
    return $ EFix e1 e2

tableBin :: OperatorTable String () Identity Exp
tableBin = [[ifxl $ parseBin "+" Add
           , ifxl $ parseBin "-" Sub
           , ifxl $ parseBin "*" Mul
           , ifxr $ parseBin ":" Cons]]
    where ifxl x = Infix x AssocLeft
          ifxr x = Infix x AssocRight

parseBin :: String -> Bin -> Parser (Exp -> Exp -> Exp)
parseBin rep op = (\_ -> EBin op) <$> reservedOp rep

parseLet :: Parser Exp
parseLet = do
    reserved "let"
    x <- idn
    reservedOp "="
    a <- parseExp
    reserved "in"
    b <- parseExp
    return $ ELet x a b

parseCase :: Parser Exp
parseCase = do
    reserved "case"
    e    <- parseExp
    reserved "of"
    alts <- braces $ sepBy parseAlternate (symbol ";")
    return $ ECase e alts

parseAlternate :: Parser Alt
parseAlternate = do
    p <- parsePattern
    reservedOp "->"
    e <- parseExp
    return (p, e)

parsePattern :: Parser Pat
parsePattern = choice
    [ symbol "_" >> return PAny
    , PVal . VNum . read <$> many1 digit <* spaces
    -- Temporarily, only list literals are allowed.
    , PVal <$> brackets (sepBy parseExp (symbol ",") >>= \exps -> return $ VList $ map (eval []) exps)
    , idn >>= \x ->
        if not (null x) && isUpper (head x)
        then many parsePattern >>= \ps -> return $ PCon x ps
        else return $ PVar x
    , parens $ parsePattern >>= \x -> reservedOp ":" >> parsePattern >>= \xs -> return $ PCons x xs
    , parens parsePattern]

parseDecl :: Parser Decl
parseDecl = try parseDecl' <|> (parseExp >>= \e -> return (Nothing, e)) <* eof

parseDecl' :: Parser Decl
parseDecl' = do
    x <- idn
    reservedOp "="
    e <- parseExp
    return (Just x, e)

parseLn :: String -> Either String Decl
parseLn ln = case parse parseDecl ln ln of
    Left  err  -> Left $ show err
    Right decl -> Right decl

-- Runs the REPL in the EVTO state.
repl :: IO ()
repl = replWith EVTO

replWith :: Repl -> IO ()
replWith = repl' []

repl' :: [Decl] -> Repl -> IO ()
repl' decls st = do
    ln <- getLine
    case ln of
        -- Displays the AST of the parsed expression.
        ":ast"  -> repl' decls AST
        -- Displays the outermost exp./val. constructor of the parsed/evaluated expression respectively.
        ":evto" -> repl' decls EVTO
        -- Displays the list of local declarations made within the current REPL session.
        ":d"    -> print (pretty decls) >> repl' decls st
        -- Ends the current REPL session.
        ":q"    -> return ()
        -- Restarts the REPL with an empty list of local declarations.
        ":r"    -> repl' [] st
        _ -> case parseLn ln of
                Left  err  -> putStrLn err >> repl' decls st
                Right decl -> do
                    let env = [(x, eval env e) | (Just x, e) <- decls]
                    let e = snd decl
                    let v = eval env e
                    case st of
                        EVTO -> putStrLn $ show (pretty v) ++ " : " ++ show (toConstr e) ++ " => " ++ show (toConstr v)
                        AST  -> putStrLn $ show (pretty v) ++ " <~ " ++ show e
                    let decls' = maybe decls (\x -> (decl :) $ filter ((/= Just x) . fst) decls) (fst decl)
                    repl' decls' st