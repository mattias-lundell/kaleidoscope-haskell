module Parser where

import Ast
import Text.ParserCombinators.Parsec hiding (Parser)
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Control.Monad
import Data.List (sort)

-- State is a list of tuples containing precedence and operatorname.
type St = [(Int, String)]

type Parser a = GenParser Char St a

languageDef =
    emptyDef { Token.commentLine     = "#"
             , Token.identStart      = letter
             , Token.identLetter     = alphaNum
             , Token.reservedNames   = [ "def"
                                       , "extern"
                                       , "binary"
                                       , "unary"
                                       , "if"
                                       , "then"
                                       , "else"
                                       , "for"
                                       , "in"
                                       ]
             }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding parenthesis:
                                    -- parens p
                                    -- takes care of the parenthesis and
                                    -- uses p to parse what's inside them
integer    = Token.integer    lexer -- parses an integer
semi       = Token.semi       lexer -- parses a semicolon
whiteSpace = Token.whiteSpace lexer -- parses whitespace
commaSep   = Token.commaSep   lexer

kaleidoscopeParser :: Parser [Top]
kaleidoscopeParser = whiteSpace >> parseTops

parseTops :: Parser [Top]
parseTops = endBy (parseTopDef <|> parseTopExpr <|> parseTopExtern) semi

parseTopExpr :: Parser Top
parseTopExpr = liftM TExpr parseExpr

parseTopExtern :: Parser Top
parseTopExtern = do
  reserved "extern"
  liftM (TExtern . Extern) parseProto

parseTopDef :: Parser Top
parseTopDef = do
  reserved "def"
  p <- parseBinOpProto <|> parseUnaryOpProto <|> parseProto
  b <- parseExpr
  return $ TDef (Def p b)

parseExpr :: Parser Expr
parseExpr = parseOp <|> parseNum <|> parseVar <|> parseIfThenElse <|> parseFor

parseIfThenElse :: Parser Expr
parseIfThenElse = do
  reserved "if"
  c <- parseExpr
  reserved "then"
  e1 <- parseExpr
  reserved "else"
  e2 <- parseExpr
  return $ IfThenElse c e1 e2

parseFor :: Parser Expr
parseFor = do
  reserved "for"
  lv <- identifier
  string "="
  whiteSpace
  le <- parseExpr
  string ","
  whiteSpace
  lc <- parseExpr
  string ","
  ls <- parseExpr
  reserved "in"
  e1 <- parseExpr
  return $ For (Var lv) le lc ls e1

parseBinOpProto :: Parser Proto
parseBinOpProto = do
  reserved "binary"
  ident <- anyChar
  p <- integer
  whiteSpace
  args <- parens $ commaSep identifier
  s <- getState
  setState (((fromIntegral p), [ident]):s)
  return $ BinOpProto [ident] args (fromIntegral p)

parseUnaryOpProto :: Parser Proto
parseUnaryOpProto = do
  reserved "unary"
  ident <- anyChar
  whiteSpace
  args <- parens $ commaSep identifier
  return $ Proto [ident] args

parseProto :: Parser Proto
parseProto = do
  ident <- identifier
  args <- parens $ commaSep identifier
  return $ Proto ident args

parseOp :: Parser Expr
parseOp = do
  s <- getState
  buildExpressionParser (operators s) parseTerm

parseTerm :: Parser Expr
parseTerm = parens parseOp
       <|> parseVar
       <|> parseNum

parseNum :: Parser Expr
parseNum = liftM (Num . fromInteger) integer

parseVar :: Parser Expr
parseVar = do
  id <- identifier
  (string "(" >> (parseCall id)) <|> (return $ Var id)

parseCall :: String -> Parser Expr
parseCall id = do
  args <- commaSep parseExpr
  string ")"
  return $ Call id args

parseString :: String -> [Top]
parseString str =
    case runParser kaleidoscopeParser ops "" str of
      Left e  -> error $ show e
      Right r -> r

ops :: St
ops = [(0, "*"),
       (1, "+"),
       (2, "-"),
       (3, "<")]

operators :: St -> [[Operator Char st Expr]]
operators ops = map f (snd . unzip . sort $ ops)
    where f c = [Infix ((string c) >> return (BinOp c)) AssocLeft]

test_a = parseString "1+2+3+4*(91+1);\ndef g(d,g) g+d;"
test_b = parseString "def f() 1;1+2;"
