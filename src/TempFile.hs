{-# LANGUAGE FlexibleContexts #-}
module TempFile (withSourceAsTempFile) where

import Control.Exception (bracket)

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString

import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (hClose, openBinaryTempFile)

-- | Saves a 'Source' to a temporary file, and runs an action with
-- that file, after which the file will be deleted.
withSourceAsTempFile
  :: String
  -- ^ A file name that the temporary file name
  -- should look similar to.
  -> ByteString
  -- ^ The 'Source' to save.
  -> (FilePath -> IO a)
  -- ^ The action using the file.
  -> IO a
withSourceAsTempFile pattern source action = do
  tempDir <- getTemporaryDirectory
  bracket
    (openBinaryTempFile tempDir pattern >>= \ (fp, h) ->
      hClose h >> ByteString.writeFile fp source >> return fp)
    removeFile
    action
