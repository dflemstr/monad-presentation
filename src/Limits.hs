{-# LANGUAGE DeriveDataTypeable #-}
{-|
An implementation of various limits that can be applied to arbitrary
code blocks.
-}
module Limits (limitTime, TimeoutException(..)) where

import Control.Concurrent (forkIO, killThread, myThreadId, threadDelay, throwTo)
import Control.DeepSeq (deepseq, NFData (..))
import Control.Concurrent.MVar
  (isEmptyMVar, newEmptyMVar, putMVar, takeMVar)
import Control.Exception
  (AsyncException (ThreadKilled), Handler (Handler), Exception, SomeException,
   catches, finally)
import Control.Monad (when)

import Data.Typeable (Typeable)

-- | Exception that is thrown when a computation times out
data TimeoutException = TimeoutException deriving (Show, Typeable)

instance Exception TimeoutException

-- | Limit an action so that it max may execute for the specified
-- number of microseconds.  The 'NFData' constraint is used to fully
-- evaluate the result of the action before it is returned, so that
-- evaluation is part of the time constraint.
limitTime :: NFData a => Int -> IO a -> IO a
limitTime timeout action = do
  -- Who am I?
  mainId <- myThreadId

  -- Variable that stores the fully evaluated result of the action
  resultVar <- newEmptyMVar

  -- Variable that is non-empty once the action is done executing
  doneVar <- newEmptyMVar

  -- The asynchronous action that is going to be performed on the
  -- child thread
  let asyncAction = do
        -- Perform the actual action
        r <- action

        -- Fully evaluate the result of the action on this thread
        -- before storing it in the var
        r `deepseq` putMVar resultVar r

  -- Spawn thread to apply time limit to
  childId <-
    forkIO $ asyncAction `catches`
    [ Handler $
      -- Handle AsyncException
      \ e -> case e of
        -- Was this thread killed? Then it was probably because of the
        -- timeout, so swallow the exception.
        ThreadKilled -> return ()
        -- Otherwise, forward exception to main thread
        _ -> throwTo mainId e
    , Handler $
      -- Handle all other exceptions
      \ e -> throwTo mainId (e :: SomeException)
    ]

  -- Spawn thread to kill the other thread after timeout
  _ <- forkIO $ do
    -- Wait for the allowed delay
    threadDelay timeout

    -- Check if done
    notDone <- isEmptyMVar doneVar

    -- Were we not done? Then report timeout to main thread...
    when notDone $ throwTo mainId TimeoutException

    -- ...and kill the child thread
    killThread childId

  -- Block until resultVar contains something or we get an exception
  result <-
    takeMVar resultVar `finally` putMVar doneVar ()

  -- Return the normal result
  return result
