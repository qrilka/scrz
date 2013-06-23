module Scrz.Volume where

import Data.Maybe
import Data.List (intercalate)
import qualified Data.Map as M
import qualified Data.Traversable as T

import System.Directory
import System.Environment

import Control.Applicative
import Control.Monad

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

import Scrz.Log
import Scrz.Types
import Scrz.Utils
import Scrz.Image
import Scrz.LXC
import Scrz.Network


baseVolumeDirectory = "/srv/scrz/volumes"

allocateVolumes :: TVar Runtime -> Service -> IO [ BackingVolume ]
allocateVolumes runtime service = do
    forM (serviceVolumes service) (allocateVolume runtime)


releaseVolumes :: TVar Runtime -> [ BackingVolume ] -> IO ()
releaseVolumes runtime bv = forM_ bv (releaseVolume runtime)


releaseVolume :: TVar Runtime -> BackingVolume -> IO ()
releaseVolume runtime (AdHocVolume _) = return ()
releaseVolume runtime (ManagedVolume id) = do
    atomically $ modifyTVar runtime $ \x ->
        x { backingVolumes = M.delete id (backingVolumes x) }

    let path = baseVolumeDirectory ++ "/" ++ id
    p <- exec "btrfs" [ "subvolume", "delete", path ]
    wait p


allocateVolume :: TVar Runtime -> Volume -> IO BackingVolume
allocateVolume runtime volume = do
    case (volumeBacking volume) of
        Nothing -> createBackingVolume runtime
        Just x -> do
            if '/' == head x
                then return $ AdHocVolume x
                else do
                    rt <- atomically $ readTVar runtime
                    case M.lookup x (backingVolumes rt) of
                        Nothing -> error $ "Backing volume not available"
                        Just b -> return b


createBackingVolume :: TVar Runtime -> IO BackingVolume
createBackingVolume runtime = do
    createDirectoryIfMissing True baseVolumeDirectory

    id <- newId
    let backingVolumePath = baseVolumeDirectory ++ "/" ++ id

    p <- exec "btrfs" [ "subvolume", "create", backingVolumePath ]
    fatal p

    let ret = ManagedVolume id
    atomically $ modifyTVar runtime $ \x ->
        x { backingVolumes = M.insert id ret (backingVolumes x) }

    return ret
