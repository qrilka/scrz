module Main where

import qualified Data.Map as M
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS

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
    --socket <- serverSocket
    --forkFinally (forever $ handleClient runtime socket) (cleanup socket)

    forkFinally (forever $ loadLocalConfig) (const $ atomically $ putTMVar mvar ())

  where

    cleanup socket = const $ do
        close socket
        removeFile controlSocketPath
        atomically $ putTMVar mvar ()

    hasService runtime service = elem service (localServices runtime)

    addService :: Service -> IO ()
    addService service = do
        rt <- atomically $ readTVar runtime
        if hasService rt service
            then return ()
            else do
                atomically $ modifyTVar runtime $ \x ->
                    x { localServices = service : (localServices x) }

                container <- createContainer runtime service
                startContainer runtime container
                id <- atomically $ containerId <$> readTVar container
                putStrLn $ show id

    loadLocalConfig = do
        putStrLn "Loading config file"
        conf <- LBS.readFile "conf.json"

        case A.decode conf :: Maybe Config of
            Nothing -> do
                putStrLn "unable to decode"
                threadDelay 10000000
            Just conf -> do
                putStrLn "decoded"
                forM (configServices conf) addService
                threadDelay 10000000


initializeRuntime :: IO Runtime
initializeRuntime = do
    (bridgeAddress, networkAddresses, networkPorts) <- initializeNetwork "scrz"

    return $ Runtime
      { bridgeAddress     = bridgeAddress
      , networkAddresses  = networkAddresses
      , networkPorts      = networkPorts
      , backingVolumes    = M.empty
      , containers        = M.empty
      , authorityServices = []
      , localServices     = []
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

run [ "create-container", name, image ] = undefined
    --sendCommand $ CreateContainer name image

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
