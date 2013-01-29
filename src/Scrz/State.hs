module Scrz.State where

import qualified Data.Map as M
import Control.Monad
import Data.Maybe
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Scrz.Data

activateWithRelease release service =
    service { serviceRelease = release, serviceActive = True }

deactivate service =
    service { serviceActive = False }

activateService :: TVar State -> String -> Release -> IO ()
activateService sharedState name release = atomically $ do
    state <- readTVar sharedState
    case M.lookup name (services state) of
        Nothing -> do
            service <- newTVar $ Service name release True Nothing Nothing
            writeTVar sharedState $ state { services = M.insert name service (services state) }
        Just x -> modifyTVar x $ activateWithRelease release


deactivateService :: TVar State -> String -> IO ()
deactivateService sharedState name = atomically $ do
    state <- readTVar sharedState
    case M.lookup name (services state) of
        Nothing -> return ()
        Just x  -> modifyTVar x deactivate
