module Scrz.Container where

import Data.Maybe
import Data.List (intercalate)
import qualified Data.Map as M

import System.Directory
import System.IO

import Control.Monad

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM

import Scrz.Types
import Scrz.Utils
import Scrz.Image
import Scrz.LXC
import Scrz.Network
import Scrz.Volume
import Scrz.Log

createContainer :: TVar Runtime -> Authority -> Service -> IO (TVar Container)
createContainer runtime authority service = do
    id <- newId


 -- Make sure the image is downloaded and ready to use.
    ensureImage authority (serviceImage service)


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

    let volumes = zip backingVolumes (serviceVolumes service)
    writeFile lxcConfigPath $ lxcConfig id addr gatewayAddress rootfsPath volumes


 -- Create directories for the mount points
    forM_ (serviceVolumes service) $ \volume -> do
        let mountPoint = rootfsPath ++ (volumePath volume)
        createDirectoryIfMissing True mountPoint


 -- Update container config files (/etc/hosts, /etc/portmap, ...)
    let hostsLine = ["127.0.0.1", id, "localhost" ]
    writeFile (rootfsPath ++ "/etc/hosts") $ intercalate " " hostsLine

    let sports  = servicePorts service
    let cports  = externalPorts
    let ports   = zip sports cports
    let portmap = map (\(int, ext) -> (show $ internalPort int) ++ "=" ++ (show ext)) ports
    writeFile (rootfsPath ++ "/etc/portmap") $ intercalate " " portmap

    -- TODO: Do something sensible when the FQDN isn't known.
    fqdn <- fullyQualifiedDomainName
    writeFile (rootfsPath ++ "/etc/scrz-host-domain") $ fromMaybe "" fqdn


 -- Register the container in the runtime.
    container <- newTVarIO $ Container id authority service addr externalPorts backingVolumes Nothing
    atomically $ modifyTVar runtime $ \x ->
        x { containers = M.insert id container (containers x) }

    return container


startContainer :: TVar Container -> Maybe Handle -> IO ()
startContainer container mbHandle = do
    c <- atomically $ readTVar container

    let id            = containerId c
    let containerPath = "/srv/scrz/containers/" ++ id
    let lxcConfigPath = containerPath ++ "/config"
    let service       = containerService c

    if isJust $ containerProcess c
        then return ()
        else do
            let args = ([ "-n", id, "-f", lxcConfigPath, "-c", "/dev/null", "/sbin/scrz-init" ]) ++ (serviceCommand service)

            p <- execEnv "lxc-start" args [] mbHandle
            void $ forkFinally (wait p) clearContainerProcess

            atomically $ modifyTVar container $ \x ->
                x { containerProcess = Just p }

  where

    clearContainerProcess = const $ do
        logger "lxc-start exited"
        when (isJust mbHandle) $ do
            logger "Closing slave handle"
            hClose (fromJust mbHandle)
        atomically $ modifyTVar container $ \x ->
            x { containerProcess = Nothing }


stopContainer :: TVar Container -> IO ()
stopContainer container = do
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
