module Scrz.Types where


import Data.Set (Set)
import Data.Map (Map)
import System.Process
import Data.Word
import Network.Socket
import Control.Concurrent
import Control.Concurrent.STM.TVar


data Image = Image
  { imageId :: String
  , imageChecksum :: String
  , imageSize :: Int
  }


data Port = Port
  { internalPort :: Int
  -- ^ The container port that is to be exposed to the external network.

  , externalPort :: Maybe Int
  -- ^ If set, the container port is mapped to this specific external port.
  --   Since external ports have to be unique, only a single container can map
  --   to a specific port.
  }


data Volume = Volume
  { volumePath :: String
  -- ^ Path inside the container to mount the volume to.

  , volumeBacking :: Maybe String
  -- ^ ID of the backing volume if one should be reused. If Nothing, then the
  --   supervisor creates a new backing volume.
  }


data Service = Service
  { serviceRevision :: Int
  , serviceImage :: Image
  , serviceCommand :: [ String ]
    -- ^ Command and arguments that are executed to start this service.

  , serviceEnvironment :: [ (String,String) ]

  , servicePorts :: [ Port ]
    -- ^ Network ports that the service requires. When a container starts,
    --   a mapping is created for these ports so that they are exposed to the
    --   external network.

  , serviceVolumes :: [ Volume ]
  }

data Config = Config
  { configServices :: [ Service ]
  }

data BackingVolume = BackingVolume
  { backingVolumeId :: String
  }


data Container = Container
  { containerId :: String

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
