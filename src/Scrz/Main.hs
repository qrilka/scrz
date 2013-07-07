module Main where

import qualified Data.List as L
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
import Scrz.Http

withMaybe :: Maybe a -> (a -> IO ()) -> IO ()
withMaybe Nothing  _ = return ()
withMaybe (Just a) f = f a

createControlThread :: TMVar () -> TVar Runtime -> Maybe String -> IO ThreadId
createControlThread mvar runtime remoteAuthorityUrl = do
    void $ forkIO $ forever $ loadLocalConfig  >> threadDelay delay

    withMaybe remoteAuthorityUrl $ \url -> do
        void $ forkIO $ forever $ do
            syncRemoteConfig url
            threadDelay delay

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

    loadLocalConfig = do
        conf <- LBS.readFile "/etc/scrz/config.json"

        case A.decode conf :: Maybe Config of
            Nothing -> putStrLn "Could not decode config"
            Just config -> mergeConfig runtime Local config

    syncRemoteConfig url = do
        let authority = Remote url
        config <- getJSON $ url ++ "/api/conf?host=host.domain.tld"
        case config of
            Nothing -> return ()
            Just x -> mergeConfig runtime authority x


mergeConfig :: TVar Runtime -> Authority -> Config -> IO ()
mergeConfig runtime authority config = do
    -- Remove old services from the local runtime.
    rt <- atomically $ readTVar runtime
    forM_ (M.elems $ containers rt) $ \container -> do
        c <- atomically $ readTVar container

        let matchAuthority = authority == containerAuthority c
        let hasService = L.elem (containerService c) (configServices config)

        when (matchAuthority && not hasService) $ do
            logger $ "Service removed from the configuration, stopping container"
            stopContainer container
            destroyContainer runtime container

    -- Add new services from the authority.
    forM_ (configServices config) addService

  where

    addService :: Service -> IO ()
    addService service = do
        rt <- atomically $ readTVar runtime
        exists <- hasContainer rt authority service

        unless exists $ do
            container <- createContainer runtime authority service
            startContainer container Nothing
            id <- atomically $ containerId <$> readTVar container
            logger $ show id


initializeRuntime :: IO Runtime
initializeRuntime = do
    (bridgeAddress, networkAddresses, networkPorts) <- initializeNetwork

    return $ Runtime
      { bridgeAddress     = bridgeAddress
      , networkAddresses  = networkAddresses
      , networkPorts      = networkPorts
      , backingVolumes    = M.empty
      , containers        = M.empty
      }


startSupervisor :: Maybe String -> IO ()
startSupervisor remoteAuthorityUrl = do
    runtime <- newTVarIO =<< initializeRuntime

    mvar <- newEmptyTMVarIO
    controlThread <- createControlThread mvar runtime remoteAuthorityUrl
    setupSignalHandlers controlThread

    -- Wait for the control thread to finish.
    atomically $ takeTMVar mvar

    rt <- atomically $ readTVar runtime
    mapM_ (\x -> stopContainer x >> destroyContainer runtime x) $ M.elems (containers rt)

    cleanupNetwork


run :: [ String ] -> IO ()
run [ "supervisor" ] = startSupervisor Nothing
run [ "supervisor", remoteAuthorityUrl ] = startSupervisor $ Just remoteAuthorityUrl

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
parseRunArguments ra ("--save-as" : id : args) =
    let pra = ra { runArgsSaveAs = Just id }
    in parseRunArguments pra args

parseRunArguments ra ("--mount" : bv : mp : args) =
    let pra = ra { runArgsMounts = (bv,mp) : runArgsMounts ra }
    in parseRunArguments pra args

parseRunArguments ra (image : command) =
    ra { runArgsImage = image, runArgsCommand = command }

parseRunArguments ra [] = ra
parseRunArguments _ _ = error "Unable to parse arguments"

main :: IO ()
main = getArgs >>= run
