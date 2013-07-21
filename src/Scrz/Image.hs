module Scrz.Image where

import           Data.Map (Map)
import qualified Data.Map as M

import System.Directory
import System.FilePath

import Control.Monad
import Control.Applicative
import Control.Concurrent.STM

import Scrz.Types
import Scrz.Utils
import Scrz.Http


baseImageDirectory :: String
baseImageDirectory = "/srv/scrz/images"

imageBasePath :: Image -> String
imageBasePath image = baseImageDirectory </> imageId image

imageContentPath :: Image -> String
imageContentPath image = imageBasePath image </> "content"

imageVolumePath :: Image -> String
imageVolumePath image = imageBasePath image </> "volume"


getImage :: String -> IO Image
getImage id' = do
    images <- loadImages
    case M.lookup id' images of
        Nothing -> error "Image not found"
        Just x -> return x


loadImages :: IO (Map String Image)
loadImages = do
    images <- getDirectoryContents baseImageDirectory
    return $ M.fromList $ map (\id' -> (id', Image id' "" 0)) images


cloneImage :: Image -> String -> IO ()
cloneImage image path = do

    p <- exec "btrfs" [ "subvolume", "snapshot", imageVolumePath image, path ]
    fatal p


deleteImageClone :: String -> IO ()
deleteImageClone path = do
    p <- exec "btrfs" [ "subvolume", "delete", path ]
    wait p


snapshotContainerImage :: TVar Container -> String -> IO ()
snapshotContainerImage container image = do
    id' <- atomically $ containerId <$> readTVar container
    let rootfsPath = "/srv/scrz/containers/" ++ id' ++ "/rootfs"

    createDirectoryIfMissing True imagePath
    p <- exec "btrfs" [ "subvolume", "snapshot", rootfsPath, volumePath']
    wait p

  where

    imagePath   = "/srv/scrz/images/" ++ image
    volumePath' = imagePath ++ "/volume"


imageUrl :: Authority -> Image -> String
imageUrl Socket _ = error "Can not construct image url for socket authority"
imageUrl Local  _ = error "Can not construct image url for local authority"
imageUrl (Remote host) image =
    host ++ "/api/images/" ++ (imageId image) ++ "/content"


ensureImage :: Authority -> Image -> IO ()
ensureImage Local image = do
    imageVolumeExists <- doesDirectoryExist $ imageVolumePath image
    unless imageVolumeExists $
        error $ "Volume for image " ++ imageId image ++ " does not exist"

ensureImage authority image = do
    imageContentExists <- doesFileExist $ imageContentPath image
    unless imageContentExists $ downloadImage authority image

    imageVolumeExists <- doesDirectoryExist $ imageVolumePath image
    unless imageVolumeExists $ unpackImage image


downloadImage :: Authority -> Image -> IO ()
downloadImage authority image = do
    createDirectoryIfMissing True $ imageBasePath image
    downloadBinary (imageUrl authority image) $ imageContentPath image


unpackImage :: Image -> IO ()
unpackImage image = do
    fatal =<< exec "btrfs" [ "subvolume", "create", imageVolumePath image ]
    fatal =<< exec "tar" [ "-xf", imageContentPath image, "-C", imageVolumePath image ]


packImage :: Image -> IO ()
packImage image = do
    fatal =<< exec "tar" [ "-cJf", imageContentPath image, "-C", imageVolumePath image, "." ]
