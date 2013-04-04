module Scrz.Image where

import           Data.Map (Map)
import qualified Data.Map as M

import System.Directory
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

import Scrz.Types
import Scrz.Utils


baseImageDirectory = "/srv/scrz/images"


loadImages :: IO (Map String Image)
loadImages = do
    images <- getDirectoryContents baseImageDirectory
    return $ M.fromList $ map (\id -> (id, Image id)) images


cloneImage :: Image -> String -> IO ()
cloneImage image path = do

    p <- exec "btrfs" [ "subvolume", "snapshot", imagePath, path ]
    fatal p

  where

    imagePath = "/srv/scrz/images/" ++ (imageId image) ++ "/volume"

deleteImageClone :: String -> IO ()
deleteImageClone path = do
    p <- exec "btrfs" [ "subvolume", "delete", path ]
    wait p


snapshotContainerImage :: TVar Container -> String -> IO ()
snapshotContainerImage container image = do
    id <- atomically $ containerId <$> readTVar container
    let rootfsPath = "/srv/scrz/containers/" ++ id ++ "/rootfs"

    createDirectoryIfMissing True imagePath
    p <- exec "btrfs" [ "subvolume", "snapshot", rootfsPath, volumePath]
    wait p

  where

    imagePath  = "/srv/scrz/images/" ++ image
    volumePath = imagePath ++ "/volume"
