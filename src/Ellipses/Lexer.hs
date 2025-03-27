module Ellipses.Lexer where

import Text.Parsec.Language (haskell)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Token

lexer :: Token.TokenParser a
lexer = haskell

lexeme :: Parser a -> Parser a
lexeme = Token.lexeme lexer

idn :: Parser String
idn = Token.identifier lexer

symbol :: String -> Parser String
symbol = Token.symbol lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

braces :: Parser a -> Parser a 
braces = Token.braces lexer

brackets :: Parser a -> Parser a
brackets = Token.brackets lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer
