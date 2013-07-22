module Scrz.Types where


import Data.Maybe (isJust)
import Data.List as L
import Data.Set (Set)
import Data.Map as M
import System.Process
import Data.Word
import Control.Concurrent.STM


data Image = Image
  { imageId :: String
  , imageChecksum :: String
  , imageSize :: Int
  } deriving (Show, Eq)


data Port = Port
  { internalPort :: Int
  -- ^ The container port that is to be exposed to the external network.

  , externalPort :: Maybe Int
  -- ^ If set, the container port is mapped to this specific external port.
  --   Since external ports have to be unique, only a single container can map
  --   to a specific port.
  } deriving (Show, Eq)


data Volume = Volume
  { volumePath :: String
  -- ^ Path inside the container to mount the volume to.

  , volumeBacking :: Maybe String
  -- ^ ID of the backing volume if one should be reused. If Nothing, then the
  --   supervisor creates a new backing volume.
  } deriving (Show, Eq)


data Service = Service
  { serviceId :: Int
  , serviceRevision :: Int
  , serviceImage :: Image
  , serviceCommand :: [ String ]
    -- ^ Command and arguments that are executed to start this service.

  , serviceEnvironment :: [ (String,String) ]

  , servicePorts :: [ Port ]
    -- ^ Network ports that the service requires. When a container starts,
    --   a mapping is created for these ports so that they are exposed to the
    --   external network.

  , serviceVolumes :: [ Volume ]
  } deriving (Show, Eq)

data Config = Config
  { configServices :: [ Service ]
  } deriving (Show, Eq)

data BackingVolume = AdHocVolume String | ManagedVolume
  { backingVolumeId :: String
  }

backingVolumePath :: BackingVolume -> String
backingVolumePath (AdHocVolume path) = path
backingVolumePath (ManagedVolume vid) = "/srv/scrz/volumes/" ++ vid


data Authority = Local | Socket | Remote String
    deriving (Eq)

data Container = Container
  { containerId :: String

  , containerAuthority :: Authority
  , containerService :: Service
    -- ^ The service description as received from the authority server.

  , containerAddress :: IPv4
  , containerPorts :: [ Int ]
    -- ^ External ports mapped for the service. Each port in the service has
    --   a corresponding port here.

  , containerVolumes :: [ BackingVolume ]

  , containerProcess :: Maybe ProcessHandle
    -- ^ The process (lxc-start) which runs the container.
  }

implementsService :: Authority -> Service -> Container -> Bool
implementsService authority service container =
    (containerAuthority container) == authority && (containerService container) == service


data IPv4 = IPv4 Word32 deriving (Eq, Ord)

data Runtime = Runtime
  { bridgeAddress :: IPv4
  -- ^ Bridge to which all containers are connected to.

  , networkAddresses :: Set IPv4
  -- ^ Unallocated network addresses in the same subnet as the bridge.

  , networkPorts :: Set Int
  -- ^ Unallocated external ports.

  , backingVolumes :: Map String BackingVolume
  , containers :: Map String (TVar Container)
  }


-- | Return true if the runtime has a container running that implements the
--   service.
hasContainer :: Runtime -> Authority -> Service -> IO Bool
hasContainer runtime authority service = do
    cs <- mapM (atomically . readTVar) (M.elems $ containers runtime)
    return $ isJust $ L.find (implementsService authority service) cs
