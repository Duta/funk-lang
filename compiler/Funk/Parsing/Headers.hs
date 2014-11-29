{-# LANGUAGE Rank2Types #-}
module Funk.Parsing.Headers where

import Control.Applicative hiding (many, (<|>))
import Control.Lens hiding (noneOf)
import Control.Monad
import Data.Char
import Data.List
import Funk.IR.ParsedAST
import Funk.Parsing.Common
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char

headerFile :: Parser ParsedFile
headerFile = Header <$> moduleName <*> headerFileContents

headerFileContents :: Parser HeaderFile
headerFileContents = typeDef   `parseInto` definedTypes
                 <|> classDef  `parseInto` definedClasses
                 <|> instAssum `parseInto` assumedInstances
                 <|> funcSig   `parseInto` functionSigs
                 <|> (eof >> return (HeaderFile [] [] [] []))
  where
    parseInto :: Parser a -> Lens' HeaderFile [a] -> Parser HeaderFile
    p `parseInto` l = p >>= (`liftM` headerFileContents) . over l . (:)

typeDef :: Parser TypeDefinition
typeDef = do
  reserved "type"
  defType <- genType
  cons <- option [] $ do
    reservedOp ":="
    con `sepBy1` reservedOp "|"
  info <- liftM (intercalate "\n") . many . liftM (dropWhile isSpace) $ reservedOp "->" *> many (noneOf ".")
  return $ TypeDefinition info defType cons

params :: Parser [Parameter]
params = option [] . parens $ commaSep1 param

param :: Parser Parameter
param = Parameter <$> identifier <*> (reservedOp ":" *> parType)

con :: Parser Constructor
con = Constructor <$> identifier <*> params

classDef :: Parser ClassDefinition
classDef = fail "" --error "TODO: classDef"

instAssum :: Parser AssumedInstance
instAssum = reserved "instance" *> (AssumedInstance <$> identifier <*> angles genType) <* char '.'

funcSig :: Parser FunctionSignature
funcSig = fail "" --error "TODO: funcSig"

genType :: Parser Type
genType = Type <$> identifier <*> return [] --fail "" --error "TODO: genType"

parType :: Parser ParamType
parType = error "TODO: parType"
