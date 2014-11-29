module Funk.Parsing.Common where

import Control.Applicative hiding (many, (<|>))
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Combinator
import qualified Text.Parsec.Token as T
import Text.Parsec.Language

type Name = String

funkDef :: T.LanguageDef st
funkDef = emptyDef {
  T.commentStart    = "/*",
  T.commentEnd      = "*/",
  T.commentLine     = "//",
  T.reservedNames   = [
    "open",
    "typeclass",
    "given",
    "type",
    "instance"
  ],
  T.reservedOpNames = [
    ":=",
    "->",
    ":"
  ]
}

lexer :: T.TokenParser ()
lexer = T.makeTokenParser funkDef

moduleNamePiece :: Parser Name
moduleNamePiece = (:) <$> letter <*> many alphaNum
              <?> "module name piece"

orEOF :: Parser a -> Parser ()
orEOF p = void p <|> eof

moduleName :: Parser [Name]
moduleName = many newline
             *> sepBy1 moduleNamePiece (char '.') <*
             (newline >> (many1 (char '=') <?> "module name underline") >> orEOF (many newline))
         <?> "module name"

reserved :: String -> Parser ()
reserved = T.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = T.reservedOp lexer

parens :: Parser a -> Parser a
parens = T.parens lexer

commaSep1 :: Parser a -> Parser [a]
commaSep1 = T.commaSep1 lexer

identifier :: Parser Name
identifier = T.identifier lexer

angles :: Parser a -> Parser a
angles = T.angles lexer
