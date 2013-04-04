module Main where

import qualified Data.Map as M

import Control.Applicative
import Control.Monad
import System.Environment
import System.Directory
import System.Posix.Process

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar

import Network.Socket

import Scrz.Log
import Scrz.Types
import Scrz.Network
import Scrz.Container
import Scrz.Image
import Scrz.Service
import Scrz.Socket
import Scrz.Signal
import Scrz.Commands


createControlThread :: TMVar () -> TVar Runtime -> IO ThreadId
createControlThread mvar runtime = do
    socket <- serverSocket
    forkFinally (forever $ handleClient runtime socket) (cleanup socket)

  where

    cleanup socket = const $ do
        close socket
        removeFile controlSocketPath
        atomically $ putTMVar mvar ()


initializeRuntime :: IO Runtime
initializeRuntime = do
    (bridgeAddress, networkAddresses, networkPorts) <- initializeNetwork "scrz"
    images <- loadImages
    services <- loadServices

    return $ Runtime
      { bridgeAddress    = bridgeAddress
      , networkAddresses = networkAddresses
      , networkPorts     = networkPorts
      , images           = images
      , services         = services
      , containers       = M.empty
      }



run :: [ String ] -> IO ()
run [ "supervisor" ] = do
    runtime <- newTVarIO =<< initializeRuntime

    mvar <- newEmptyTMVarIO
    controlThread <- createControlThread mvar runtime
    setupSignalHandlers controlThread

    -- Wait for the control thread to finish.
    atomically $ takeTMVar mvar

    rt <- atomically $ readTVar runtime
    mapM_ (stopContainer runtime) $ M.elems (containers rt)

    cleanupNetwork

run [ "create-container", name, image ] = do
    sendCommand $ CreateContainer name image

run [ "list-containers" ] = do
    sendCommand $ ListContainers

run [ "stop-container", id ] = do
    sendCommand $ StopContainer id

run [ "destroy-container", id ] = do
    sendCommand $ DestroyContainer id

run [ "start", id ] = do
    sendCommand $ Start id

run [ "snapshot", container, image ] = do
    sendCommand $ Snapshot container image

run [ "quit" ] = do
    sendCommand $ Quit

run [ "console", id ] = do
    executeFile "lxc-console" True [ "-n", id ] Nothing

run args = do
    logger $ "Unknown arguments: " ++ (show args)


main = getArgs >>= run
