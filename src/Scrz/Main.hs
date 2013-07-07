module Main where

import qualified Data.Map as M
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS

import Control.Applicative
import Control.Monad
import System.Environment
import System.Directory
import System.Posix.Process
import System.Posix.IO
import System.Posix.Terminal (openPseudoTerminal, getSlaveTerminalName)

import Control.Exception
import Control.Concurrent
import Control.Concurrent.STM

import Network.Socket

import Scrz.Log
import Scrz.Types
import Scrz.Network
import Scrz.Container
import Scrz.Image
import Scrz.Socket
import Scrz.Signal
import Scrz.Commands
import Scrz.Terminal
import Scrz.Utils


createControlThread :: TMVar () -> TVar Runtime -> IO ThreadId
createControlThread mvar runtime = do
    forkIO $ forever $ loadLocalConfig >> threadDelay delay

    socket <- serverSocket
    forkFinally (forever $ handleClient runtime socket) (cleanup socket)


  where

    delay = 10 * 1000 * 1000

    cleanup :: Socket -> Either SomeException () -> IO ()
    cleanup socket ex = do
        logger $ show ex
        close socket
        removeFile controlSocketPath
        atomically $ putTMVar mvar ()

    addService :: Service -> IO ()
    addService service = do
        rt <- atomically $ readTVar runtime
        exists <- hasContainer rt Local service

        unless exists $ do
            container <- createContainer runtime Local service
            startContainer runtime container Nothing
            id <- atomically $ containerId <$> readTVar container
            logger $ show id

    loadLocalConfig = do
        conf <- LBS.readFile "/etc/scrz/config.json"

        case A.decode conf :: Maybe Config of
            Nothing -> do
                putStrLn "Could not decode config"
                threadDelay 10000000
            Just conf -> do
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
    sendCommand ListContainers >>= printResponse
    return ()

run [ "ps" ] = do
    sendCommand ListContainers >>= printResponse
    return ()

run [ "stop-container", id ] = do
    sendCommand $ StopContainer id
    return ()

run [ "destroy-container", id ] = do
    sendCommand $ DestroyContainer id
    return ()

run [ "start", id ] = do
    sendCommand $ Start id
    return ()

run [ "snapshot", container, image ] = do
    sendCommand $ Snapshot container image
    return ()

run [ "quit" ] = do
    sendCommand $ Quit
    return ()

run [ "console", id ] = do
    executeFile "lxc-console" True [ "-n", id ] Nothing

run [ "list-images" ] = do
    images <- loadImages
    forM_ (M.toList images) $ \(id, image) -> do
        when ('.' /= head (imageId image)) $ do
            putStrLn $ imageId image

run [ "pack-image", id ] = do
    image <- getImage id
    packImage image

run ("run":args) = do
    let ra = parseRunArguments (RunArgs "" [] [] Nothing) args

    (ptm, pts) <- openPseudoTerminal
    slaveName <- getSlaveTerminalName ptm
    response <- sendCommand $ Run (runArgsImage ra) (runArgsCommand ra) slaveName (runArgsMounts ra)

    attr1 <- setRawModeFd stdInput

    let pump src dst = fdRead src 999 >>= \(x, _) -> fdWrite dst x

    forkFinally (forever $ pump ptm stdOutput) (const $ return ())
    forkFinally (forever $ pump stdInput ptm)  (const $ return ())

    case response of
        CreateContainerResponse id -> do
            sendCommand $ Wait id
            resetModeFd stdInput attr1
            imageId <- maybe newId return (runArgsSaveAs ra)
            logger $ "Saving image under id " ++ imageId
            sendCommand $ Snapshot id imageId
            return ()

        _ -> return ()



    case response of
        CreateContainerResponse id -> do
            sendCommand $ DestroyContainer id
            return ()

        _ -> return ()


run args = do
    logger $ "Unknown arguments: " ++ (show args)


data RunArgs = RunArgs
  { runArgsImage :: String
  , runArgsCommand :: [String]
  , runArgsMounts :: [(String,String)]
  , runArgsSaveAs :: Maybe String
  } deriving (Show)

parseRunArguments :: RunArgs -> [String] -> RunArgs
parseRunArguments ra [] = ra

parseRunArguments ra ("--save-as" : id : args) =
    let pra = ra { runArgsSaveAs = Just id }
    in parseRunArguments pra args

parseRunArguments ra ("--mount" : bv : mp : args) =
    let pra = ra { runArgsMounts = (bv,mp) : runArgsMounts ra }
    in parseRunArguments pra args

parseRunArguments ra (image : command) = ra { runArgsImage = image, runArgsCommand = command }

parseRunArguments _ _ = error "Unable to parse arguments"

main :: IO ()
main = getArgs >>= run
