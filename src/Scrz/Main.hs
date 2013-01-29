module Main where

import qualified Data.ByteString.Lazy as BS
import           Data.Foldable           (foldrM)
import qualified Data.List            as L
import qualified Data.Map             as M
import           Data.Maybe

import           System.IO
import           System.Directory
import           System.Process (terminateProcess)
import           System.Environment
import           System.Exit

import           Control.Monad
import           Control.Exception

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TVar
import           Control.Concurrent.STM.TQueue

import           Network.URI    ( parseURI )
import           Network.Curl

import           Scrz.Log
import           Scrz.Data
import           Scrz.Signal
import           Scrz.Supervisor
import           Scrz.State
import qualified Scrz.Authority.HTTP    as HTTP



data Sync = Sync { toStart, toKill :: [TVar Service] }
emptySyncData = Sync [] []

syncSharedState :: TVar State -> STM Sync
syncSharedState sharedState = do
    state <- readTVar sharedState
    foldrM (updateService state) emptySyncData $ M.assocs $ services state

  where

    updateService state (name, sharedService) syncData = do
        service <- readTVar sharedService
        if not (running state) && isJust (serviceProcess service)
        then return $ syncData { toKill = (toKill syncData) ++ [ sharedService ] }
        else
            if (serviceActive service) && (isNothing $ serviceSupervisor service)
            then do
                return $ syncData { toStart = (toStart syncData) ++ [ sharedService ] }
            else
                if False == (serviceActive service) && isJust (serviceProcess service)
                then return $ syncData { toKill = (toKill syncData) ++ [ sharedService ] }
                else do
                    let proc = serviceProcess service
                    case proc of
                        Nothing -> return syncData
                        Just (Process release _) ->
                            if release /= (serviceRelease service)
                            then return $ syncData { toKill = (toKill syncData) ++ [ sharedService ] }
                            else return syncData


exitLoop :: TVar State -> IO ()
exitLoop sharedState = do
    state <- atomically $ readTVar sharedState
    let rS = services state
    killProcesses $ map snd $ M.assocs rS
    threadDelay 5000000
    return ()
  where
    killProcesses procs = mapM_ killProcess procs
        where
            killProcess sharedService = do
                service <- atomically $ readTVar sharedService
                case (serviceProcess service) of
                    Nothing -> return ()
                    Just (Process _ x)  -> terminateProcess x >> return ()


mainLoop :: TVar State -> IO ()
mainLoop sharedState = do
    sync <- atomically $ syncSharedState sharedState
    startProcesses sharedState (toStart sync) >> killProcesses (toKill sync)

    (State running authority _) <- atomically $ readTVar sharedState
    if running
    then threadDelay 2000000 >> mainLoop sharedState
    else logger "Exiting..." >> exitLoop sharedState

  where

    startProcesses :: TVar State -> [TVar Service] -> IO ()
    startProcesses sharedState starts = mapM_ spawnWatcher starts
        where
            spawnWatcher s = do
                threadId <- forkIO $ supervise s
                atomically $ modifyTVar s $ \x -> x { serviceSupervisor = Just threadId }

    killProcesses :: [TVar Service] -> IO ()
    killProcesses procs = mapM_ killProcess procs
        where
            killProcess sharedService = do
                service <- atomically $ readTVar sharedService
                case (serviceProcess service) of
                    Nothing -> return ()
                    Just (Process _ x)  -> terminateProcess x >> return ()


parseAuthority (parseURI -> Just x) = return $ Just $ HTTP x
parseAuthority _ = return Nothing

parse [ auth ] = parseAuthority auth
parse _        = return Nothing

usage = do
    progName <- getProgName
    logger $ "Usage: " ++ progName ++ " <authority>"


syncStateThread sharedState (HTTP url) = HTTP.syncState sharedState url

run Nothing  = usage >> exitWith ExitSuccess
run (Just x) = do
    sharedState <- newTVarIO $ State True x M.empty
    forkIO $ syncStateThread sharedState x

    setupSignalHandlers sharedState
    mainLoop sharedState


main = getArgs >>= parse >>= run
