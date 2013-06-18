module Scrz.Image where

import qualified Data.ByteString.Lazy as BS
import           Data.Map (Map)
import qualified Data.Map as M

import System.Directory

import Control.Monad
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

import           Network.Curl

import Scrz.Types
import Scrz.Utils
import Scrz.Log


baseImageDirectory = "/srv/scrz/images"


getImage :: String -> IO Image
getImage id = do
    images <- loadImages
    case M.lookup id images of
        Nothing -> error "Image not found"
        Just x -> return x


loadImages :: IO (Map String Image)
loadImages = do
    images <- getDirectoryContents baseImageDirectory
    return $ M.fromList $ map (\id -> (id, Image id "" 0)) images


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


imageUrl authority image =
    "http://localhost:3000/api/images/" ++ (imageId image) ++ "/content"


ensureImage :: Authority -> Image -> IO ()
ensureImage authority image = do
    let imageFileName = "/srv/scrz/images/" ++ (imageId image) ++ "/content"
    exists <- doesFileExist imageFileName
    unless exists (downloadImage authority image)


downloadImage :: Authority -> Image -> IO ()
downloadImage authority image = do
    let url = imageUrl authority image
    logger $ "Downloading slug from " ++ url

    (code, stream) <- curlGetString_ url curlOptions
    case code of
        CurlOK -> do
            let imageFileName = "/srv/scrz/images/" ++ (imageId image) ++ "/content"
            createDirectoryIfMissing True $ "/srv/scrz/images/" ++ (imageId image)
            BS.writeFile imageFileName stream
            unpackImage image

        otherwise -> do
            logger $ "Failed to download the slug: " ++ (show code)
            return ()

  where

    curlOptions = [ CurlPost False, CurlNoBody False, CurlFollowLocation True ]

unpackImage :: Image -> IO ()
unpackImage image = do
    fatal =<< exec "btrfs" [ "subvolume", "create", imageVolumePath ]
    fatal =<< exec "tar" [ "-xf", imageFileName, "-C", imageVolumePath ]

  where

    imageVolumePath = "/srv/scrz/images/" ++ (imageId image) ++ "/volume"
    imageFileName   = "/srv/scrz/images/" ++ (imageId image) ++ "/content"


packImage :: Image -> IO ()
packImage image = do
    fatal =<< exec "tar" [ "-cJf", imageFileName, "-C", imageVolumePath, "." ]

  where

    imageVolumePath = "/srv/scrz/images/" ++ (imageId image) ++ "/volume"
    imageFileName   = "/srv/scrz/images/" ++ (imageId image) ++ "/content"
