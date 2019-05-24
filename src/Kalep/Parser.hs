module Kalep.Parser
  ( parseToplevel
  , parseExpr
  )
where

import Text.Parsec
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

table =
  [ [binary "*" Mult Ex.AssocLeft, binary "/" Divide Ex.AssocLeft]
  , [binary "+" Plus Ex.AssocLeft, binary "-" Minus Ex.AssocLeft]
  ]
  where binary s f = Ex.Infix (Token.reservedOp lexer s >> return (BinExpr f))

integerL :: Parser Expr
integerL = do
  n <- Token.integer lexer
  return $ LitExpr (IntL n)

doubleL :: Parser Expr
doubleL = do
  n <- Token.float lexer
  return $ LitExpr (DoubleL n)

stringL :: Parser Expr
stringL = do
  s <- Token.stringLiteral lexer
  return $ LitExpr (StringL s)

variable :: Parser Expr
variable = do
  s <- Token.identifier lexer
  return $ Var s

expr :: Parser Expr
expr = Ex.buildExpressionParser table primaryExpr

primaryExpr :: Parser Expr
primaryExpr =
  try doubleL
    <|> try integerL
    <|> try stringL
    <|> try extern
    <|> try function
    <|> try call
    <|> variable
    <|> Token.parens lexer expr

call :: Parser Expr
call = do
  name <- Token.identifier lexer
  args <- Token.parens lexer $ Token.commaSep lexer expr
  return $ Call name args

function :: Parser Expr
function = do
  Token.reserved lexer "def"
  name <- Token.identifier lexer
  args <- Token.parens lexer $ Token.commaSep lexer $ variable
  body <- expr
  return $ Statement (Function name args body)

extern :: Parser Expr
extern = do
  Token.reserved lexer "extern"
  name <- Token.identifier lexer
  args <- Token.parens lexer $ Token.commaSep lexer variable
  return $ Statement (Extern name args)

defn :: Parser Expr
defn = try extern <|> try function <|> expr

contents :: Parser a -> Parser a
contents p = do
  Token.whiteSpace lexer
  r <- p
  eof
  return r

toplevel :: Parser [Expr]
toplevel = many defn

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (contents expr) "<stdin>"

parseToplevel :: String -> Either ParseError [Expr]
parseToplevel = parse (contents toplevel) "<stdin>"
