module Kalep.Parser
  ( parseToplevel
  , parseExpr
  )
where

import Text.Parsec hiding (string)
import Text.Parsec.String (Parser)

import Kalep.Syntax
import Kalep.Lexer

import qualified Text.Parsec.Expr as Ex

table =
  [ [binary "*" Mult Ex.AssocLeft, binary "/" Divide Ex.AssocLeft]
  , [binary "+" Plus Ex.AssocLeft, binary "-" Minus Ex.AssocLeft]
  ]

litInteger :: Parser Expr
litInteger = LitExpr . IntL <$> integer

litDouble :: Parser Expr
litDouble = LitExpr . DoubleL <$> double

litString :: Parser Expr
litString = LitExpr . StringL <$> string

variable :: Parser Expr
variable = Var <$> identifier

expr :: Parser Expr
expr = Ex.buildExpressionParser table primaryExpr

primaryExpr :: Parser Expr
primaryExpr =
  try litDouble
    <|> try litInteger
    <|> try litString
    <|> try extern
    <|> try function
    <|> try call
    <|> variable
    <|> parens expr

call :: Parser Expr
call = do
  name <- identifier
  args <- parens $ commaSep expr
  return $ Call name args

function :: Parser Expr
function = do
  reserved "def"
  name <- identifier
  args <- parens $ commaSep variable
  Statement . Function name args <$> expr

extern :: Parser Expr
extern = do
  reserved "extern"
  name <- identifier
  args <- parens $ commaSep variable
  return $ Statement (Extern name args)

defn :: Parser Expr
defn = try extern <|> try function <|> expr

contents :: Parser a -> Parser a
contents p = do
  whiteSpace
  r <- p
  eof
  return r

toplevel :: Parser [Expr]
toplevel = many defn

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (contents expr) "<stdin>"

parseToplevel :: String -> Either ParseError [Expr]
parseToplevel = parse (contents toplevel) "<stdin>"
