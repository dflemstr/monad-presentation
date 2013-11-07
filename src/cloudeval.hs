{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main (main) where

import Control.Applicative ((<*>))
import Control.Arrow (first)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.DeepSeq (NFData)
import Control.DeepSeq.TH (deriveNFData)
import Control.Exception
  (AsyncException (..), catch, catches, Exception, SomeException)
import qualified Control.Exception as Exception (Handler (..))
import Control.Monad (forM)
import Control.Monad.Error.Class (catchError)

import Data.Functor ((<$>))
import qualified Data.HashMap.Strict as HashMap
import Data.List (isInfixOf, nub)
import Data.Text (pack, unpack)

import Language.Haskell.Interpreter
import Language.Haskell.Interpreter.Unsafe

import Network.Wai (requestBody)
import Network.Wai.Application.Static
  (defaultFileServerSettings, StaticSettings(..))

-- Internal GHC module; might break this code in the future!
import qualified SrcLoc

import WaiAppStatic.Types (unsafeToPiece)

import Yesod
import Yesod.Static

-- Our own modules:
import Limits (limitTime, TimeoutException (..))
import TempFile (withSourceAsTempFile)
import Types

data CloudEval =
  CloudEval
  { getInterpreterLock :: MVar ()
  , getStatic :: Static
  }

instance Yesod CloudEval

deriveNFData ''InterpreterError

-- Just to make some instance declarations work
instance NFData GhcError where

mkYesod "CloudEval" [parseRoutesNoCheck|
/evaluate   EvaluateR   POST
/           StaticR     Static getStatic
|]

-- | Catch all exceptions thrown by the specified action and convert
-- them to an 'Error'.
catchExceptions :: IO a -> IO (Either Error a)
catchExceptions action =
  (Right <$> action) `catches`
  [ Exception.Handler $ \ e ->
     case e of
       StackOverflow ->
         throwRestrictionError "Stack overflow"
       HeapOverflow ->
         throwRestrictionError "Heap overflow"
       UserInterrupt ->
         throwRestrictionError "User interrupted"
       ThreadKilled ->
         throwRuntimeError "Thread killed"
  , Exception.Handler $ \ e ->
     case e of
       TimeoutException -> throwRestrictionError "Timed out"
  , Exception.Handler $ \ e ->
     throwRuntimeError . show $ (e :: SomeException)
  ]
  where
    throwErrorKind kind =
      return . Left . Error kind . (:[]) . ErrorFragment Nothing
    throwRestrictionError = throwErrorKind Restriction
    throwRuntimeError = throwErrorKind Runtime

-- | A POST handler that parses a Haskell module, and either returns
-- compiler errors or the variables in scope of the module, and
-- optionally evaluates and shows the given expression(s) in the
-- context of the module.
postEvaluateR :: Handler TypedContent
postEvaluateR = do
  request <- getRequest
  lock <- getInterpreterLock <$> getYesod

  -- This is a Conduit source, so the request body is actually never
  -- loaded into memory (Depends on what WAI does).
  let body = requestBody . reqWaiRequest $ request
      exprs = [unpack expr | ("expression", expr) <- reqGetParams request]

  interpreterResult <-
    liftIO .
    -- Save the POST body to a temp file and use it
    withSourceAsTempFile "cloudeval.hs" body $ \ moduleFile ->
    -- Only one user can use the interpreter at once
    withMVar lock $ const .
    -- Catch all exceptions, and convert them to error messages
    catchExceptions .
    -- You get max 1s
    limitTime 1000000 .
    -- This happens sometimes... basically use a spinlock to wait
    -- until ready. TODO: figure out why this exception is thrown
    retryOn MultipleInstancesNotAllowed .
    -- Start the hint interpreter
    unsafeRunInterpreterWithArgs ["-XSafe"] $ do
      loadModules [moduleFile]
      loadedModules <- getLoadedModules
      setTopLevelModules loadedModules

      moduleScopes <- forM loadedModules $ \ m -> do
        exports <- getModuleExports m
        return [ScopeVar m var | Fun var <- exports]

      -- TODO: apply timeout per expression
      evals <- forM exprs $ \ expr ->
        (expr,) <$>
        (EvaluationSuccess <$> eval expr <*> typeOf expr)
        `catchError` (return . EvaluationError . parseError)

      return (evals, concat moduleScopes)

  let result =
        either ResultError
         (either
          (ResultError . parseError)
          (uncurry ResultSuccess . first HashMap.fromList))
        interpreterResult

  selectRep $ do
    provideRep . return . pack . show $ result
    provideJson result

retryOn :: (Exception e) => e -> IO a -> IO a
retryOn e action =
  action `catch`
  (\ e' -> let _ = e' `asTypeOf` e in retryOn e action)

-- | Converts an interpreter error to our own error type.
parseError :: InterpreterError -> Error
parseError (UnknownError err) =
  Error Unknown [ErrorFragment Nothing err]
parseError (WontCompile ghcErrors) =
  Error Compilation . nub . map (parseFragment <$> errMsg <*> errSpan) $ ghcErrors
  where
    parseFragment e s =
      let msg = if ".hs:" `isInfixOf` e then tail . dropWhile (/= ' ') $ e else e
      in  ErrorFragment (convertSpan s) msg

    convertSpan (SrcLoc.RealSrcSpan s) =
      Just $ CodeSpan
      (CodePosition (SrcLoc.srcSpanStartLine s) (SrcLoc.srcSpanStartCol s))
      (CodePosition (SrcLoc.srcSpanEndLine s) (SrcLoc.srcSpanEndCol s))
    convertSpan _ = Nothing
parseError (NotAllowed err) =
  Error Restriction [ErrorFragment Nothing err]
parseError (GhcException err) =
  Error Ghc [ErrorFragment Nothing err]

main :: IO ()
main = do
  lock <- newMVar ()
  let staticSettings =
        (defaultFileServerSettings "static") { ssListing = Nothing }
  warp 3000 . CloudEval lock . Static $ staticSettings
