module Scrz.Service where

import           Data.Map (Map)
import qualified Data.Map as M

import Data.Aeson
import qualified Data.ByteString.Lazy as L

import System.Directory
import Control.Applicative
import Control.Monad

import Scrz.Types
import Scrz.Utils


baseServicesDirectory = "/srv/scrz/services"


instance FromJSON Service where
    parseJSON (Object o) = Service
        <$> (o .: "id")
        <*> (o .: "command")
        <*> (o .: "ports")


loadServices :: IO (Map String Service)
loadServices = do
    services <- getDirectoryContents baseServicesDirectory
    foldM parseService M.empty services

  where

    parseService :: Map String Service -> String -> IO (Map String Service)
    parseService map id = do
        let filePath = baseServicesDirectory ++ "/" ++ id
        isFile <- doesFileExist filePath
        if not isFile
            then return map
            else do
                json <- L.readFile filePath
                case decode json of
                    Nothing -> return map
                    Just s -> return $ M.insert id s map
