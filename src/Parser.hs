module Parser (parseString, ParseError, Statement (..), Expression (..)) where

import Codec.Picture (DynamicImage)
import Control.Monad (void)
import Data.List (intercalate)
import Text.ParserCombinators.Parsec

parseString :: String -> Either ParseError [Statement]
parseString = parse (whitespace *> program <* eof) ""

data Statement
  = Assignment String Expression
  | MethodCallStmt Expression
  | FunctionCallStmt Expression
  | ForLoop String Expression [Statement]
  deriving (Show, Eq)

data Expression
  = StringLit String
  | NumberLit Int
  | Identifier String
  | FunctionCall String [Expression]
  | ListExpression [Expression]
  | MethodCall Expression String [Expression]
  | ImageExpression (Maybe FilePath) DynamicImage
  | Add Expression Expression
  | TupleExpression [Expression]

instance Show Expression where
  show (StringLit s) = "\"" ++ s ++ "\""
  show (NumberLit n) = show n
  show (Identifier ident) = ident
  show (FunctionCall name args) = name ++ "(" ++ intercalate ", " (map show args) ++ ")"
  show (ListExpression elements) = "[" ++ intercalate ", " (map show elements) ++ "]"
  show (MethodCall obj method args) = show obj ++ "." ++ method ++ "(" ++ intercalate ", " (map show args) ++ ")"
  show (ImageExpression (Just imgPath) _) = show imgPath
  show (Add e1 e2) = show e1 ++ " + " ++ show e2
  show (ImageExpression _ _) = "Image"
  show (TupleExpression t) = "(" ++ intercalate ", " (map show t) ++ ")"

instance Eq Expression where
  (StringLit s1) == (StringLit s2) = s1 == s2
  (NumberLit n1) == (NumberLit n2) = n1 == n2
  (Identifier i1) == (Identifier i2) = i1 == i2
  (FunctionCall n1 a1) == (FunctionCall n2 a2) = n1 == n2 && a1 == a2
  (ListExpression e1) == (ListExpression e2) = e1 == e2
  (MethodCall o1 m1 a1) == (MethodCall o2 m2 a2) = o1 == o2 && m1 == m2 && a1 == a2
  (ImageExpression p1 _) == (ImageExpression p2 _) = p1 == p2
  (Add e1a e1b) == (Add e2a e2b) = e1a == e2a && e1b == e2b
  (TupleExpression t1) == (TupleExpression t2) = t1 == t2
  _ == _ = False

whitespace :: Parser ()
whitespace = void $ many $ void space

lexeme :: Parser a -> Parser a
lexeme = (<* whitespace)

symbol :: String -> Parser ()
symbol = lexeme . void . try . string

identifier :: Parser String
identifier = lexeme $ (:) <$> letter <*> many (letter <|> digit <|> char '_')

stringLiteral :: Parser Expression
stringLiteral = StringLit <$> between (char '"') (char '"') (many (noneOf "\""))

numberLiteral :: Parser Expression
numberLiteral = NumberLit . read <$> lexeme (many1 digit)

add :: Parser Expression
add = do
  left <- term
  whitespace
  void $ lexeme $ char '+'
  whitespace
  Add left <$> term

expression :: Parser Expression
expression = try add <|> term

term :: Parser Expression
term =
  try stringLiteral
    <|> try numberLiteral
    <|> try listExpression
    <|> try methodCall
    <|> try tupleExpression
    <|> Identifier <$> identifier

listExpression :: Parser Expression
listExpression = ListExpression <$> between (symbol "[") (symbol "]") listElements

listElements :: Parser [Expression]
listElements = expression `sepBy` symbol ","

tupleExpression :: Parser Expression
tupleExpression = TupleExpression <$> between (symbol "(") (symbol ")") listElements

assignment :: Parser Statement
assignment = do
  ident <- identifier
  symbol "="
  expr <- try functionCall <|> expression
  return $ Assignment ident expr

functionCall :: Parser Expression
functionCall = do
  func <- identifier
  args <- between (symbol "(") (symbol ")") (expression `sepBy` symbol ",")
  return $ FunctionCall func args

forLoop :: Parser Statement
forLoop = do
  symbol "for"
  var <- identifier
  symbol "in"
  expr <- expression
  symbol "{"
  stmts <- many statement
  symbol "}"
  return $ ForLoop var expr stmts

statement :: Parser Statement
statement = do
  stmt <-
    try assignment
      <|> try (MethodCallStmt <$> methodCall)
      <|> try (FunctionCallStmt <$> functionCall)
      <|> forLoop
  symbol ";"
  return stmt

methodCall :: Parser Expression
methodCall = do
  obj <- try functionCall <|> Identifier <$> identifier
  symbol "."
  method <- identifier
  args <- between (symbol "(") (symbol ")") (expression `sepBy` symbol ",")
  return $ MethodCall obj method args

program :: Parser [Statement]
program = many statement
