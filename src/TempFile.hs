{-# LANGUAGE FlexibleContexts #-}
module TempFile (withSourceAsTempFile) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Resource
  (MonadBaseControl, MonadThrow, MonadUnsafeIO, ResourceT, allocate,
   runResourceT)

import Data.ByteString (ByteString)
import Data.Conduit (Source, ($$))
import Data.Conduit.Binary (sinkFile)

import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (hClose, openBinaryTempFile)

-- | Saves a 'Source' to a temporary file, and runs an action with
-- that file, after which the file will be deleted.
withSourceAsTempFile ::
  (MonadIO m, MonadThrow m, MonadUnsafeIO m,
   MonadBaseControl IO m)
  => String
  -- ^ A file name that the temporary file name
  -- should look similar to.
  -> Source (ResourceT m) ByteString
  -- ^ The 'Source' to save.
  -> (FilePath -> m a)
  -- ^ The action using the file.
  -> m a
withSourceAsTempFile pattern source action = runResourceT $ do
  tempDir <- liftIO getTemporaryDirectory
  (_, filePath) <-
    allocate
    (openBinaryTempFile tempDir pattern >>= \ (fp, h) ->
      hClose h >> return fp)
    removeFile
  source $$ sinkFile filePath
  lift $ action filePath
