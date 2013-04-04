module Scrz.Signal (setupSignalHandlers) where

import Data.Maybe
import Control.Monad

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TQueue

import System.Posix.Signals

import Network.Socket (close)

import Scrz.Log
import Scrz.Types
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



setupSignalHandlers :: ThreadId -> IO (ThreadId)
setupSignalHandlers controlThread = do
    signalQueue <- newTQueueIO :: IO (TQueue CInt)
    mapM (setupHandler signalQueue) [ sigINT, sigTERM ]
    forkIO $ signalQueueProcessor controlThread signalQueue

  where

    setupHandler signalQueue sign =
        installHandler sign (Catch $ signalHandler signalQueue sign) Nothing

    signalHandler :: TQueue CInt -> CInt -> IO ()
    signalHandler signalQueue signal = atomically $ writeTQueue signalQueue signal
