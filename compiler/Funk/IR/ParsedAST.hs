{-# LANGUAGE TemplateHaskell #-}
module Funk.IR.ParsedAST where

import Control.Lens

type Name = String

data ParsedFile
  = Header [Name] HeaderFile
  | Source [Name] SourceFile
  deriving (Show, Eq)

data HeaderFile = HeaderFile {
  _definedTypes     :: [TypeDefinition],
  _definedClasses   :: [ClassDefinition],
  _assumedInstances :: [AssumedInstance],
  _functionSigs     :: [FunctionSignature]
} deriving (Show, Eq)

data SourceFile = SourceFile {
  _instances :: [InstanceDefinition],
  _functions :: [FunctionDefinition]
} deriving (Show, Eq)

data TypeDefinition = TypeDefinition {
  _typeInfo     :: String,
  _definedType  :: Type,
  _constructors :: [Constructor]
} deriving (Show, Eq)

data ClassDefinition = ClassDefinition {
  _definedClass     :: Typeclass,
  _classConstraints :: [Typeclass],
  _classFunctions   :: [FunctionSignature]
} deriving (Show, Eq)

data Typeclass = Typeclass {
  _className :: Name,
  _classType :: Name
} deriving (Show, Eq)

data AssumedInstance = AssumedInstance {
  _assumedClass :: Name,
  _assumedType  :: Type
} deriving (Show, Eq)

data FunctionSignature = FunctionSignature {
  _funcInfo   :: String,
  _funcName   :: Name,
  _funcGens   :: [Name],
  _funcParams :: [Parameter],
  _funcRet    :: Type
} deriving (Show, Eq)

data Constructor = Constructor {
  _conName   :: Name,
  _conParams :: [Parameter]
} deriving (Show, Eq)

data Parameter = Parameter {
  _paramName :: Name,
  _paramType :: ParamType
} deriving (Show, Eq)

data ParamType
  = FuncType FunctionType
  | RegType Type
  deriving (Show, Eq)

data FunctionType = FunctionType {
  _paramTypes :: [Type],
  _retType    :: Type
} deriving (Show, Eq)

data Type = Type {
  _typeName :: Name,
  _typeGens :: [Name]
} deriving (Show, Eq)

data InstanceDefinition = InstanceDefinition {
  _instClass :: Typeclass,
  _instFuncs :: [FunctionDefinition]
} deriving (Show, Eq)

data FunctionDefinition = FunctionDefinition {
  _defFuncName :: Name
} deriving (Show, Eq)

makeLenses ''HeaderFile
makeLenses ''SourceFile
makeLenses ''TypeDefinition
makeLenses ''ClassDefinition
makeLenses ''Typeclass
makeLenses ''AssumedInstance
makeLenses ''FunctionSignature
makeLenses ''Constructor
makeLenses ''Parameter
makeLenses ''FunctionType
makeLenses ''Type
makeLenses ''InstanceDefinition
makeLenses ''FunctionDefinition
