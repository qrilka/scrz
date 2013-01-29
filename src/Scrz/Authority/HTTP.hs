module Scrz.Authority.HTTP (syncState) where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map as M
import           Data.Maybe

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad

import           Network.Curl
import           Network.URI

import           Scrz.Data
import           Scrz.Log
import           Scrz.State

data Scrape = Scrape
  { scrapeId :: Int
  , scrapeSlug :: String
  , scrapeHash :: String
  , scrapeInit :: String
  , scrapeEnv :: M.Map String String
  }

instance A.FromJSON Scrape where
    parseJSON (A.Object x) = Scrape <$> x A..: "id" <*> x A..: "slug" <*> x A..: "hash" <*> x A..: "init" <*> x A..: "env"

scrape :: URI -> IO (Maybe Scrape)
scrape uri = do
    let auth = uriRegName $ fromJust $ uriAuthority uri
    let path = uriPath uri
    let url  = "http://" ++ auth ++ "/svc" ++ path ++ "/scrape"
    logger $ "Scrape URL " ++ url
    (code, stream :: LB.ByteString) <- curlGetString_ url curlOptions
    case code of
        CurlOK    -> return $ A.decode stream
        otherwise -> return Nothing

  where

    curlOptions = [ CurlPost False, CurlNoBody False, CurlFollowLocation True ]

sleep x = threadDelay $ x * 1000000

syncState sharedState uri = do
    let auth = uriRegName $ fromJust $ uriAuthority uri

    state <- atomically $ readTVar sharedState
    if not (running state)
    then return ()
    else do
        download <- scrape uri
        case download of
            Nothing -> logger "failed to get download" >> repeat
            Just (Scrape id slug hash init env) -> do
                let slugUrl = "http://" ++ auth ++ "/slug/" ++ slug
                let release = Release (slug ++ "@" ++ (show id)) slugUrl hash env init
                activateService sharedState (tail $ uriPath uri) release
                repeat

  where

    repeat = sleep 9 >> syncState sharedState uri
