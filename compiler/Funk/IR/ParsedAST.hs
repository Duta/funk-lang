module Funk.IR.ParsedAST where

type Name = String

data HeaderFile = HeaderFile {
  definedTypes     :: [TypeDefinition],
  definedClasses   :: [ClassDefinition],
  assumedInstances :: [AssumedInstance],
  functionSigs     :: [FunctionSignature]
} deriving (Show, Eq)

data SourceFile = SourceFile {
  instances :: [InstanceDefinition],
  functions :: [FunctionDefinition]
} deriving (Show, Eq)

data TypeDefinition = TypeDefinition {
  typeInfo     :: String,
  definedType  :: Type,
  constructors :: [Constructor]
} deriving (Show, Eq)

data ClassDefinition = ClassDefinition {
  definedClass     :: Typeclass,
  classConstraints :: [Typeclass],
  classFunctions   :: [FunctionSignature]
} deriving (Show, Eq)

data Typeclass = Typeclass {
  className :: Name,
  classType :: Name
} deriving (Show, Eq)

data AssumedInstance = AssumedInstance {
  assumedClass :: Name,
  assumedType  :: Name
} deriving (Show, Eq)

data FunctionSignature = FunctionSignature {
  funcInfo   :: String,
  funcName   :: Name,
  funcGens   :: [Name],
  funcParams :: [Parameter],
  funcRet    :: Type
} deriving (Show, Eq)

data Constructor = Constructor {
  conName   :: Name,
  conParams :: [Parameter]
} deriving (Show, Eq)

data Parameter = Parameter {
  paramName :: Name,
  paramType :: ParamType
} deriving (Show, Eq)

data ParamType
  = FuncType FunctionType
  | RegType Type
  deriving (Show, Eq)

data FunctionType = FunctionType {
  paramTypes :: [Type],
  retType    :: Type
} deriving (Show, Eq)

data Type = Type {
  typeName :: Name,
  typeGens :: [Name]
} deriving (Show, Eq)

data InstanceDefinition = InstanceDefinition {
  instClass :: Typeclass,
  instFuncs :: [FunctionDefinition]
} deriving (Show, Eq)

data FunctionDefinition = FunctionDefinition {
  defFuncName :: Name
} deriving (Show, Eq)
