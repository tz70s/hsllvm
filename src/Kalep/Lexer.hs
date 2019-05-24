module Kalep.Lexer
  ( lexer
  , integer
  , double
  , string
  , identifier
  , parens
  , commaSep
  , reserved
  , binary
  , whiteSpace
  )
where

import Text.Parsec hiding (string)
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import Kalep.Syntax

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Token

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser style
 where
  style = emptyDef
    { Token.commentLine     = "#"
    , Token.reservedNames   = ["def", "extern"]
    , Token.reservedOpNames = ["+", "-", "*", "/", ";"]
    }

integer :: Parser Integer
integer = Token.integer lexer

double :: Parser Double
double = Token.float lexer

string :: Parser String
string = Token.stringLiteral lexer

identifier :: Parser Name
identifier = Token.identifier lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

commaSep :: Parser a -> Parser [a]
commaSep = Token.commaSep lexer

reserved :: Name -> Parser ()
reserved = Token.reserved lexer

-- | Refer to document, it's clear.
-- http://hackage.haskell.org/package/parsec-3.1.13.0/docs/Text-Parsec-Expr.html
-- In case the type signature is a bit boilerplate, 
-- that we omit here.
binary s f = Ex.Infix (Token.reservedOp lexer s >> return (BinExpr f))

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer
