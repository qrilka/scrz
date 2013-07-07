module Scrz.Signal (setupSignalHandlers) where

import Control.Monad

import Control.Concurrent
import Control.Concurrent.STM

import System.Posix.Signals

import Scrz.Log
import Foreign.C


signalQueueProcessor :: ThreadId -> TQueue CInt -> IO ()
signalQueueProcessor controlThread signalQueue = do

    forever $ (atomically $ readTQueue signalQueue) >>= handleSignal

  where

    handleSignal signal
        | signal == sigINT = do
            logger "Received SIGINT"
            killThread controlThread

        | signal == sigTERM = do
            logger "Received SIGTERM"
            killThread controlThread

        | otherwise = return ()



setupSignalHandlers :: ThreadId -> IO ()
setupSignalHandlers controlThread = do
    signalQueue <- newTQueueIO :: IO (TQueue CInt)
    mapM_ (setupHandler signalQueue) [ sigINT, sigTERM ]
    void $ forkIO $ signalQueueProcessor controlThread signalQueue

  where

    setupHandler signalQueue sign =
        installHandler sign (Catch $ signalHandler signalQueue sign) Nothing

    signalHandler :: TQueue CInt -> CInt -> IO ()
    signalHandler signalQueue signal = atomically $ writeTQueue signalQueue signal
