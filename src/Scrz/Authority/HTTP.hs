module Scrz.Authority.HTTP (syncState) where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map as M
import           Data.Maybe

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad

import           Network.BSD
import           Network.Curl
import           Network.URI

import           Scrz.Data
import           Scrz.Log
import           Scrz.State

data Scrape = Scrape
  { scrapeId :: Int
  , scrapeService :: String
  , scrapeSlug :: String
  , scrapeHash :: String
  , scrapeInit :: String
  , scrapeEnv :: M.Map String String
  }

instance A.FromJSON Scrape where
    parseJSON (A.Object x) = Scrape
        <$> x A..: "id"
        <*> x A..: "service"
        <*> x A..: "slug"
        <*> x A..: "hash"
        <*> x A..: "init"
        <*> x A..: "env"


scrape :: String -> IO (Maybe [Scrape])
scrape url = do
    (code, stream :: LB.ByteString) <- curlGetString_ url curlOptions
    case code of
        CurlOK    -> return $ A.decode stream
        otherwise -> return Nothing

  where

    curlOptions = [ CurlPost False, CurlNoBody False, CurlFollowLocation True ]


syncState :: TVar State -> URI -> IO ()
syncState sharedState uri = do
    hostName <- getHostName
    logger $ "Scrape URL " ++ (scrapeUrl hostName)
    loop


  where
    auth = uriRegName $ fromJust $ uriAuthority uri
    port = uriPort $ fromJust $ uriAuthority uri

    scrapeUrl hostName = "http://" ++ auth ++ port ++ "/api/scrape?host=" ++ hostName

    loop = do
        state <- atomically $ readTVar sharedState
        if not (running state)
        then return ()
        else do
            hostName <- getHostName
            download <- scrape (scrapeUrl hostName)
            case download of
                Nothing -> logger "failed to get download" >> repeat
                Just scrapes -> do
                    mapM_ (activate uri) scrapes
                    repeat

    sleep x = threadDelay $ x * 1000000
    repeat = sleep 9 >> loop

    activate :: URI -> Scrape -> IO ()
    activate uri (Scrape id service slug hash init env) = do
        let slugUrl = "http://" ++ auth ++ port ++  "/api/slugs/" ++ slug
        let release = Release (service ++ "@" ++ (show id)) slugUrl hash env init
        activateService sharedState service release
