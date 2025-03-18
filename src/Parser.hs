module Parser (repl, replWith, Repl (..)) where

import Data.Char (isUpper)
import Data.Data (Data(toConstr))
import Data.Functor.Identity (Identity)
import Prettyprinter (Pretty(..))
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.String (Parser)

import Evaluator (eval)
import Lexer hiding (lexer)
import Pretty ()
import Syntax
import SyntaxPatterns

type Decl = (Maybe Var, Exp)
data Repl = EVTO | AST

parseExp :: Parser Exp
parseExp = buildExpressionParser tableBin $ 
    choice [parens parseExp, parseApp, parseExp']

parseExp' :: Parser Exp
parseExp' = choice [parseAbs, parseLet, parseVal]

parseVal :: Parser Exp
parseVal = try parseLit <|> parseVar

parseLit :: Parser Exp
parseLit = choice
    [ EVal . VNum . read <$> many1 digit <* spaces
    , EVal (VCons "True"  []) <$ reserved "True"
    , EVal (VCons "False" []) <$ reserved "False"
    , brackets parseList]

parseList :: Parser Exp
parseList = do
    vs <- sepBy parseExp (symbol ",")
    return $ EVal $ VList (map (eval []) vs)

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
parseApp = do
    f    <- parseExp'
    exps <- many $ choice [parens parseExp, parseExp']
    return $ foldl EApp f exps

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