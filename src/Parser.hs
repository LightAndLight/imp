{-# language RecursiveDo #-}
module Parser where

import Control.Applicative ((<|>), some, many)
import Data.Functor (($>), (<$))
import Text.Trifecta (CharParsing, char, digit, oneOf, string)

import Syntax (Expr(..), Statement(..), Type(..))

{-|

digit ::= ['0'..'9']
expr ::= digit+ | 'True' | 'False'
statement ::= single_statement | single_statement ';' statement
single_statement ::= if_statement | while_statement | 'pass'
if_statement ::= 'if' expr block 'else' block
block ::= '{' statement '}'

-}

token :: CharParsing m => m a -> m a
token m = m <* many (oneOf "\n\r\t ")

type_ :: CharParsing m => m Type
type_ =
  token $
  string "Bool" $> TyBool <|>
  string "Int" $> TyInt

expr :: CharParsing m => m Expr
expr =
  token $
  Int . read <$> some digit <|>
  string "True" $> Bool True <|>
  string "False" $> Bool False <|>
  Ann <$> expr <* token (char ':') <*> type_

single_statement :: CharParsing m => m Statement
single_statement =
  if_statement <|>
  while_statement <|>
  token (string "pass") $> Pass

while_statement :: CharParsing m => m Statement
while_statement =
  token $
  While <$ token (string "while") <*> expr <*> block

if_statement :: CharParsing m => m Statement
if_statement =
  If <$ token (string "if") <*> expr <*> block <* token (string "else") <*> block

block :: CharParsing m => m Statement
block = token (char '{') *> statement <* token (char '}')

statement :: CharParsing m => m Statement
statement =
  single_statement <|>
  Seq <$> single_statement <* token (char ';') <*> statement
