module Scrz.Network where

import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as S

import Control.Monad
import Control.Concurrent.STM

import System.Process

import Scrz.Log
import Scrz.Types
import Scrz.Utils
import Scrz.Network.IPv4


iptables :: (ProcessHandle -> IO ()) -> [String] -> IO ()
iptables f args = void $ exec "iptables" args >>= f

cleanupNetwork :: IO ()
cleanupNetwork = do
    logger "Cleaning up iptables configuration"

    iptables wait [ "-t", "nat", "-D", "PREROUTING", "-m", "addrtype", "--dst-type", "LOCAL", "-j", "SCRZ" ]
    iptables wait [ "-t", "nat", "-D", "OUTPUT", "-j", "SCRZ" ]

    iptables wait [ "-t", "nat", "-F", "SCRZ" ]
    iptables wait [ "-t", "nat", "-X", "SCRZ" ]


initializeNetwork :: IO (IPv4, Set IPv4, Set Int)
initializeNetwork = do

    cleanupNetwork

    logger "Initializing iptables"

    iptables fatal [ "-t", "nat", "-N", "SCRZ" ]
    iptables fatal [ "-t", "nat", "-A", "OUTPUT", "-j", "SCRZ" ]
    iptables fatal [ "-t", "nat", "-A", "PREROUTING", "-m", "addrtype", "--dst-type", "LOCAL", "-j", "SCRZ" ]

    return $ ( toIPv4 [10,1,0,1], addresses,  ports )

  where

    list      = take 100 $ iterate (1+) 2
    addresses = S.fromList $ map (\x -> toIPv4 [10,1,0,x]) list
    ports     = S.fromDistinctAscList [ 50000 .. 59999 ]


allocateAddress :: TVar Runtime -> IO IPv4
allocateAddress runtime = atomically $ do
    rt <- readTVar runtime
    let addresses = networkAddresses rt
    let ret = head $ S.toList addresses
    writeTVar runtime $ rt { networkAddresses = S.delete ret addresses }
    return ret

releaseAddress :: TVar Runtime -> IPv4 -> IO ()
releaseAddress runtime addr = atomically $ do
    modifyTVar runtime $ \x -> x { networkAddresses = S.insert addr (networkAddresses x)}

allocatePort :: TVar Runtime -> Port -> IO Int
allocatePort runtime port = atomically $ do
    maybe randomFromSet return (externalPort port)

  where
    randomFromSet = do
        rt <- readTVar runtime
        let ports = networkPorts rt
        let ext = fromMaybe (head $ S.toList ports) (externalPort port)
        if S.member ext ports
            then do
                writeTVar runtime $ rt { networkPorts = S.delete ext ports }
                return ext
            else
                error $ "Can not allocate external port " ++ (show $ externalPort port)

releasePort :: TVar Runtime -> Int -> IO ()
releasePort runtime port = atomically $ do
    modifyTVar runtime $ \x -> x { networkPorts = S.insert port (networkPorts x)}

mapPorts :: IPv4 -> [ (Int,Port) ] -> IO ()
mapPorts addr ports = do
    mapM_ (\x -> updateForwardRule "-A" addr (fst x) (internalPort $ snd x)) ports


unmapPorts :: IPv4 -> [ (Int,Port) ] -> IO ()
unmapPorts addr ports = do
    mapM_ (\x -> updateForwardRule "-D" addr (fst x) (internalPort $ snd x)) ports


updateForwardRule :: String -> IPv4 -> Int -> Int -> IO ()
updateForwardRule rule addr src dst = iptables wait
    [ "-t", "nat", rule, "SCRZ", "-p", "tcp", "--dport", show src
    , "-j", "DNAT", "--to-destination", (show addr) ++ ":" ++ (show dst)
    ]
