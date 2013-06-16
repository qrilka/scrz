module Main where

import Data.Maybe (isJust)
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS

import Control.Applicative
import Control.Monad
import System.Environment
import System.Directory
import System.Posix.Process
import System.Posix.Files
import System.Posix.IO
import System.Posix.Terminal (openPseudoTerminal, getSlaveTerminalName)
import System.IO

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
    forkIO $ forever $ loadLocalConfig >> threadDelay delay

    socket <- serverSocket
    forkFinally (forever $ handleClient runtime socket) (cleanup socket)


  where

    delay = 10 * 1000 * 1000

    cleanup socket = const $ do
        close socket
        removeFile controlSocketPath
        atomically $ putTMVar mvar ()

    addService :: Service -> IO ()
    addService service = do
        rt <- atomically $ readTVar runtime
        exists <- hasContainer rt Local service

        if exists
            then return ()
            else do
                container <- createContainer runtime Local service
                startContainer runtime container Nothing
                id <- atomically $ containerId <$> readTVar container
                logger $ show id

    loadLocalConfig = do
        conf <- LBS.readFile "conf.json"

        case A.decode conf :: Maybe Config of
            Nothing -> do
                logger "Unable to decode local config file"
                threadDelay 10000000
            Just conf -> do
                logger "Loading from local config file"
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
    mapM_ (\x -> stopContainer runtime x >> destroyContainer runtime x) $ M.elems (containers rt)

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

run ("run":image:command) = do
    (ptm, pts) <- openPseudoTerminal
    slaveName <- getSlaveTerminalName ptm
    setFdMode pts 0666
    sendCommand $ Run image command slaveName

    -- TODO: Handle ^C or when the slave exits (fdRead ptm should throw an
    -- exception in that case).

    mvar <- newEmptyTMVarIO
    let pump src dst = fdRead src 999 >>= \(x, _) -> fdWrite dst x

    forkFinally (forever $ pump ptm stdOutput) (const $ atomically $ putTMVar mvar ())
    forkFinally (forever $ pump stdInput ptm)  (const $ atomically $ putTMVar mvar ())

    putStrLn "Waiting on exit"
    atomically $ takeTMVar mvar
    putStrLn "exited"
    return ()


run args = do
    logger $ "Unknown arguments: " ++ (show args)


main :: IO ()
main = getArgs >>= run
