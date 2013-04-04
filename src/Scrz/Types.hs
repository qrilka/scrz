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
  }


data Service = Service
  { serviceId :: String
  , serviceCommand :: [ String ]
    -- ^ Command and arguments that are executed to start this service.

  , servicePorts :: [ Int ]
    -- ^ Network ports that the service requires. When a container starts,
    --   a mapping is created for these ports so that they are exposed to the
    --   external network.
  }


data Container = Container
  { containerId :: String
  , containerImage :: Image
    -- ^ The image from which the rootfs for this container was derived from.

  , containerService :: Service
  , containerAddress :: IPv4
  , containerPorts :: [ Int ]
    -- ^ External ports mapped for the service. Each port in the service has
    --   a corresponding port here.

  , containerProcess :: Maybe ProcessHandle
    -- ^ The process (lxc-start) which runs the container.
  }


data IPv4 = IPv4 Word32 deriving (Eq, Ord)

data Runtime = Runtime
  { bridgeAddress :: IPv4

  , networkAddresses :: Set IPv4
  , networkPorts :: Set Int

  , images :: Map String Image
  , services :: Map String Service

  , containers :: Map String (TVar Container)
  }
