module Scrz.Container where

import Data.Maybe
import Data.List (intercalate)
import qualified Data.Map as M

import System.Directory
import System.Environment

import Control.Applicative
import Control.Monad

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

import Network.FQDN

import Scrz.Log
import Scrz.Types
import Scrz.Utils
import Scrz.Image
import Scrz.LXC
import Scrz.Network
import Scrz.Volume


createContainer :: TVar Runtime -> Authority -> Service -> IO (TVar Container)
createContainer runtime authority service = do
    id <- newId


 -- Allocate runtime resources (address, ports, volumes etc).
    addr <- allocateAddress runtime
    externalPorts <- forM (servicePorts service) (allocatePort runtime)
    mapPorts addr $ zip externalPorts $ servicePorts service
    backingVolumes <- allocateVolumes runtime service


 -- Prepare the filesystem (clone image, write LXC config file).
    let containerPath  = "/srv/scrz/containers/" ++ id
    let rootfsPath     = containerPath ++ "/rootfs"
    let lxcConfigPath  = containerPath ++ "/config"
    gatewayAddress <- atomically $ bridgeAddress <$> readTVar runtime

    createDirectoryIfMissing True containerPath
    cloneImage (serviceImage service) rootfsPath
    writeFile lxcConfigPath $ lxcConfig id addr gatewayAddress rootfsPath


 -- Register the container in the runtime.
    container <- newTVarIO $ Container id authority service addr externalPorts backingVolumes Nothing
    atomically $ modifyTVar runtime $ \x ->
        x { containers = M.insert id container (containers x) }

    return container


startContainer :: TVar Runtime -> TVar Container -> IO ()
startContainer runtime container = do
    c <- atomically $ readTVar container

    let id            = containerId c
    let containerPath = "/srv/scrz/containers/" ++ id
    let lxcConfigPath = containerPath ++ "/config"
    let service       = containerService c

    if isJust $ containerProcess c
        then return ()
        else do
            let args = ([ "-n", id, "-f", lxcConfigPath, "-c", "/dev/null", "/sbin/scrz-init" ]) ++ (serviceCommand service)

            env0 <- getEnvironment
            fqdn <- fullyQualifiedDomainName
            let env1 = maybe env0 (\x -> ( "FQDN", x ) : env0) fqdn

            let sports  = servicePorts $ containerService c
            let cports  = containerPorts c
            let ports   = zip sports cports
            let portmap = map (\(int, ext) -> (show $ internalPort int) ++ "=" ++ (show ext)) ports
            let env2    = ( "PORTMAP", intercalate " " portmap ) : env1


            p <- execEnv "lxc-start" args env2
            forkFinally (wait p) clearContainerProcess

            atomically $ modifyTVar container $ \x ->
                x { containerProcess = Just p }

  where

    clearContainerProcess = const $ atomically $ modifyTVar container $ \x ->
            x { containerProcess = Nothing }


stopContainer :: TVar Runtime -> TVar Container -> IO ()
stopContainer runtime container = do
    c <- atomically $ readTVar container

    when (isJust (containerProcess c)) $ do
        exec "lxc-stop" [ "-n", containerId c ] >>= wait
        wait (fromJust $ containerProcess c)

        atomically $ modifyTVar container $ \x ->
            x { containerProcess = Nothing }

    return ()


-- | Release all resources (IP addresses, mapped, rootfs clone etc) used by
--   the container and remove it form the runtime.
destroyContainer :: TVar Runtime -> TVar Container -> IO ()
destroyContainer runtime container0 = do
    container <- atomically $ readTVar container0

 -- Release runtime resources.
    let service = containerService container
    let ports   = containerPorts container
    let addr    = containerAddress container

    unmapPorts addr $ zip ports $ servicePorts service
    mapM_ (releasePort runtime) ports
    releaseAddress runtime addr

    releaseVolumes runtime (containerVolumes container)


 -- Delete resources from the filesystem.
    let id            = containerId container
    let containerPath = "/srv/scrz/containers/" ++ id
    let rootfsPath    = containerPath ++ "/rootfs"

    deleteImageClone rootfsPath
    removeDirectoryRecursive containerPath


 -- Unregister the container from the runtime.
    atomically $ modifyTVar runtime $ \x ->
        x { containers = M.delete id (containers x) }
