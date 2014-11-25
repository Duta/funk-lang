module Funk.IR.ParsedAST where

data HeaderFile = {
  definedTypes     :: [TypeDefinition],
  assumedInstances :: [AssumedInstance],
  functionSigs     :: [FunctionSignature]
} deriving (Show, Eq)

data SourceFile = {
  instances :: [InstanceDefinition],
  functions :: [FunctionDefinition]
} deriving (Show, Eq)

data TypeDefinition = {
  typeInfo     :: String,
  type         :: Type
  constructors :: [Constructor]
} deriving (Show, Eq)

data AssumedInstance = {
  assumedClass :: Name,
  assumedType  :: Name
} deriving (Show, Eq)

data FunctionSignature = {
  funcInfo   :: String,
  funcName   :: Name,
  funcGens   :: [Name],
  funcParams :: [Parameter],
  funcRet    :: Type
} deriving (Show, Eq)

data Constructor = {
  conName   :: Name,
  conParams :: [Parameter]
} deriving (Show, Eq)

data Parameter = {
  paramName :: Name,
  paramType :: ParamType
} deriving (Show, Eq)

data ParamType
  = FuncType FunctionType
  | RegType Type
  deriving (Show, Eq)

data Type = {
  typeName :: Name,
  typeGens :: [Name]
} deriving (Show, Eq)

data InstanceDefinition = {
  -- TODO
} deriving (Show, Eq)

data FunctionDefinition = {
  -- TODO
} deriving (Show, Eq)
