{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Types where

import Control.DeepSeq.TH (deriveNFData)

import Data.Aeson (ToJSON (toJSON), Value (String), object, (.=))
import Data.Aeson.TH (deriveToJSON, defaultOptions, Options(..))
import Data.Char (toLower)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)

-- | Kinds of errors that might occur during interpretation of Haskell code.
data ErrorKind = Compilation | Restriction | Ghc | Runtime | Unknown deriving (Show)

instance ToJSON ErrorKind where
  toJSON Compilation = String "compilation"
  toJSON Restriction = String "restriction"
  toJSON Ghc         = String "ghc"
  toJSON Runtime     = String "runtime"
  toJSON Unknown     = String "unknown"

deriveNFData ''ErrorKind

data CodePosition =
  CodePosition
  { codePositionRow :: Int
  , codePositionCol :: Int
  } deriving (Eq, Show)

deriveToJSON defaultOptions { fieldLabelModifier = map toLower . drop 12 } ''CodePosition
deriveNFData ''CodePosition

data CodeSpan =
  CodeSpan
  { codeSpanStart :: CodePosition
  , codeSpanEnd :: CodePosition
  } deriving (Eq, Show)

deriveToJSON defaultOptions { fieldLabelModifier = map toLower . drop 8 } ''CodeSpan
deriveNFData ''CodeSpan

-- | An error fragment that binds an error message to a source code
-- location.
data ErrorFragment =
  ErrorFragment
  { errorFragmentPosition :: Maybe CodeSpan
  , errorFragmentMessage  :: String
  } deriving (Eq, Show)

deriveToJSON defaultOptions { fieldLabelModifier = map toLower . drop 13 } ''ErrorFragment
deriveNFData ''ErrorFragment

-- | Some variable that is in scope in some module.
data ScopeVar =
  ScopeVar
  { scopeVarModule   :: String
  , scopeVarVariable :: String
  } deriving (Show)

deriveToJSON defaultOptions { fieldLabelModifier = map toLower . drop 8 } ''ScopeVar
deriveNFData ''ScopeVar

-- | An error that occurs during interpretation.
data Error =
  Error
  { errorKind      :: ErrorKind
  , errorFragments :: [ErrorFragment]
  }
  deriving (Show)

deriveToJSON defaultOptions { fieldLabelModifier = map toLower . drop 5 } ''Error
deriveNFData ''Error

-- | The result of an expression evaluation in the context of a file.
data Evaluation =
  EvaluationSuccess
  { evaluationValue :: String
  , evaluationValueType :: String
  } |
  EvaluationError
  { evaluationError :: Error
  }
  deriving (Show)

instance ToJSON Evaluation where
  toJSON EvaluationSuccess {..} =
    object
    [ "type"  .= ("success" :: Text)
    , "value" .= evaluationValue
    , "valueType" .= evaluationValueType
    ]
  toJSON EvaluationError {..} =
    object
    [ "type"  .= ("error" :: Text)
    , "error" .= evaluationError
    ]

deriveNFData ''Evaluation

-- | The result of a file load + optional expression evaluation.
data Result =
  ResultSuccess
  { resultEvaluations :: HashMap String Evaluation
  , resultScope       :: [ScopeVar]
  } |
  ResultError
  { resultError :: Error
  } deriving (Show)

instance ToJSON Result where
  toJSON ResultSuccess {..} =
    object
    [ "type"        .= ("success" :: Text)
    , "evaluations" .= resultEvaluations
    , "scope"       .= resultScope
    ]
  toJSON ResultError {..} =
    object
    [ "type"  .= ("error" :: Text)
    , "error" .= resultError
    ]

deriveNFData ''Result
