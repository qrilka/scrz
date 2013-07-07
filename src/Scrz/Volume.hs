module Scrz.Volume where

import qualified Data.Map as M

import System.Directory

import Control.Monad
import Control.Concurrent.STM

import Scrz.Types
import Scrz.Utils


baseVolumeDirectory :: String
baseVolumeDirectory = "/srv/scrz/volumes"

allocateVolumes :: TVar Runtime -> Service -> IO [ BackingVolume ]
allocateVolumes runtime service = do
    forM (serviceVolumes service) (allocateVolume runtime)


releaseVolumes :: TVar Runtime -> [ BackingVolume ] -> IO ()
releaseVolumes runtime bv = forM_ bv (releaseVolume runtime)


releaseVolume :: TVar Runtime -> BackingVolume -> IO ()
releaseVolume _       (AdHocVolume _) = return ()
releaseVolume runtime (ManagedVolume id') = do
    atomically $ modifyTVar runtime $ \x ->
        x { backingVolumes = M.delete id' (backingVolumes x) }

    let path = baseVolumeDirectory ++ "/" ++ id'
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

    id' <- newId
    let backingVolumePath' = baseVolumeDirectory ++ "/" ++ id'

    p <- exec "btrfs" [ "subvolume", "create", backingVolumePath' ]
    fatal p

    let ret = ManagedVolume id'
    atomically $ modifyTVar runtime $ \x ->
        x { backingVolumes = M.insert id' ret (backingVolumes x) }

    return ret
