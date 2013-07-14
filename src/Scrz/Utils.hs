module Scrz.Utils where

import Control.Monad.Random

import System.Process
import System.IO
import System.Exit
import Network.BSD
import Network.Socket
import Control.Applicative


newId :: IO String
newId = evalRandIO (sequence (replicate 10 rnd))
  where
    rnd = getRandomR ('a','z')


exec :: String -> [ String ] -> IO ProcessHandle
exec cmd args = do
    (_, _, _, p) <- createProcess (proc cmd args)
    return p

execEnv :: String -> [ String ] -> [ (String,String) ] -> Maybe Handle -> IO ProcessHandle
execEnv cmd args environment mbHandle = do
    let stream = maybe Inherit UseHandle mbHandle
    (_, _, _, p) <- createProcess $ (proc cmd args) { env = Just environment, std_in = stream, std_out = stream, std_err = stream }
    return p

wait :: ProcessHandle -> IO ()
wait p = waitForProcess p >> return ()

fatal :: ProcessHandle -> IO ()
fatal p = do
    exitCode <- waitForProcess p
    case exitCode of
        ExitSuccess -> return ()
        _ -> error $ "Exited with " ++ (show exitCode)

kill :: ProcessHandle -> IO ()
kill = terminateProcess

fullyQualifiedDomainName :: IO (Maybe String)
fullyQualifiedDomainName = do
    hostName <- Just <$> getHostName
    addrInfo <- head <$> getAddrInfo Nothing hostName Nothing
    fst <$> getNameInfo [] True False (addrAddress addrInfo)
