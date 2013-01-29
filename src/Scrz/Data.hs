module Scrz.Data where

import qualified Data.Map as M
import Control.Concurrent.STM.TVar (TVar)
import System.Process (ProcessHandle)
import Control.Concurrent
import Network.URI


type Environment = M.Map String String


data Release = Release {
    releaseIdentifier :: String
    -- ^ Each release has a unique identifier (scoped by the service).

  , releaseSlugURL :: String
    -- ^ URL where the release slug is located. The supervisor will download
    --   it, mount it and then execute the entry point in a subprocess.

  , releaseSlugHash :: String
    -- ^ The SHA1 hash of the slug. Used to verify that the slug was
    --   downloaded properly before using it.

  , releaseEnvironment :: Environment
    -- ^ The environment is set just before the superviser forks the
    --   subprocess.

  , releaseEntryPoint :: String
    -- ^ Path to the script within the slug which will be executed to start
    --   the service.
} deriving (Eq, Show)


data Process = Process Release ProcessHandle
instance Show Process where
    show (Process release _) = "Process " ++ (releaseIdentifier release)

data Service = Service {
    serviceName :: String
    -- ^ The service name, unique across the whole network.

  , serviceRelease :: Release
    -- ^ The desired release that is supposed to run.

  , serviceActive :: Bool
    -- ^ True if the service is supposed to run. As long as this is true, the
    --   service is kept running and restarted if it crashes/exits.

  , serviceSupervisor :: Maybe ThreadId
    -- ^ The ThreadId of the process supervisor. It is set by the main loop to
    --   indicate that the supervisor is active. It is cleared by the
    --   supervisor just before the thread exits.

  , serviceProcess :: Maybe Process
    -- ^ Set by the supervisor if the service subprocess is running.
} deriving (Show)


data Authority = HTTP { uri :: URI }

data State = State {
    running :: Bool
  , authority :: Authority
  , services :: M.Map String (TVar Service)
}
