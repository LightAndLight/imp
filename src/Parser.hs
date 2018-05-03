{-# language RecursiveDo #-}
module Parser where

import Control.Applicative ((<|>), some, many, optional)
import Data.Functor (($>), (<$))
import Text.Trifecta (CharParsing, char, digit, letter, oneOf, string, try)

import Syntax (Expr(..), Statement(..), Type(..))

{-|

digit ::= ['0'..'9']
identifier ::= [a-zA-Z_-']

expr ::= expr [':' type]

simple_expr ::=
  '(' expr ')' |
  digit+ |
  'True' |
  'False' |
  'not' expr |
  'add' expr expr |
  'intEq' expr expr |
  identifier

statement ::= single_statement | single_statement ';' statement

single_statement ::=
  if_statement |
  while_statement |
  assign_statement |
  'newref' expr |
  'read' expr |
  expr ':=' expr |
  'pass' |
  expr

block_or_single_statement ::= block | single_statement

if_statement ::= 'if' block 'then' block 'else' block
while_statement ::= 'while' block block
assign_statement ::= identifier '<-' block_or_single_statement

block ::= '{' statement '}'
-}

token :: CharParsing m => m a -> m a
token m = m <* many (oneOf "\n\r\t ")

identifier :: CharParsing m => m String
identifier =
  token $
  some (letter <|> oneOf "_-\'")

type_ :: CharParsing m => m Type
type_ =
  token $
  string "Bool" $> TyBool <|>
  string "Int" $> TyInt

simple_expr :: CharParsing m => m Expr
simple_expr =
  token (char '(') *> expr <* token (char ')') <|>
  Not <$ token (string "not") <*> expr <|>
  Add <$ token (string "add") <*> expr <*> expr <|>
  IntEq <$ token (string "intEq") <*> expr <*> expr <|>
  Int . read <$> token (some digit) <|>
  token (string "True") $> Bool True <|>
  token (string "False") $> Bool False <|>
  Var <$> identifier

expr :: CharParsing m => m Expr
expr =
  (\a -> maybe a (Ann a)) <$>
  simple_expr <*>
  optional (token (char ':') *> type_)

single_statement :: CharParsing m => m Statement
single_statement =
  if_statement <|>
  while_statement <|>
  assign_statement <|>
  NewRef <$ token (string "newref") <*> expr <|>
  Read <$ token (string "read") <*> expr <|>
  token (string "pass") $> Pass <|>
  Write <$ token (string "write") <*> expr <*> expr <|>
  Expr <$> expr

while_statement :: CharParsing m => m Statement
while_statement =
  token $
  While <$ token (string "while") <*> block <*> block

if_statement :: CharParsing m => m Statement
if_statement =
  If <$ token (string "if") <*> block <*
  token (string "then") <*> block <*
  token (string "else") <*> block

assign_statement :: CharParsing m => m Statement
assign_statement =
  Assign <$> try (identifier <* token (string "<-")) <*> block_or_single_statement

block :: CharParsing m => m Statement
block = token (char '{') *> statement <* token (char '}')

block_or_single_statement :: CharParsing m => m Statement
block_or_single_statement = block <|> single_statement

statement :: CharParsing m => m Statement
statement =
  (\a -> maybe a (Seq a)) <$>
  single_statement <*>
  optional (token (char ';') *> statement)
