module Scrz.Signal (setupSignalHandlers) where

import Data.Maybe

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TQueue

import System.Posix.Signals

import Scrz.Data
import Scrz.Log


signalQueueProcessor :: TVar State -> TQueue Int -> IO ()
signalQueueProcessor sharedState sharedSignalQueue = do
    (atomically $ readTQueue sharedSignalQueue) >>= handleSignal >> repeat

  where

    handleSignal 2 = do
        atomically $ do
            state <- readTVar sharedState
            writeTVar sharedState $ state { running = False }
        logger "\nReceived SIGINT" >> return ()
    handleSignal _ = do return ()

    repeat = signalQueueProcessor sharedState sharedSignalQueue


setupSignalHandlers :: TVar State -> IO (ThreadId)
setupSignalHandlers sharedState = do
    sharedSignalQueue <- newTQueueIO :: IO (TQueue Int)
    installHandler sigINT (Catch $ signalHandler sharedSignalQueue (fromIntegral sigINT)) Nothing
    forkIO $ signalQueueProcessor sharedState sharedSignalQueue

  where

    signalHandler :: TQueue Int -> Int -> IO ()
    signalHandler sharedSignalQueue signal = atomically $ writeTQueue sharedSignalQueue signal
