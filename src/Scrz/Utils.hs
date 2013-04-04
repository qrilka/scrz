module Scrz.Utils where

import Data.Char
import Control.Monad.Random

import System.Process
import System.Exit

newId :: IO String
newId = evalRandIO (sequence (replicate 10 rnd))
  where
    rnd = getRandomR ('a','z')


exec :: String -> [ String ] -> IO ProcessHandle
exec cmd args = do
    (_, _, _, p) <- createProcess (proc cmd args)
    return p

execEnv :: String -> [ String ] -> [ (String,String) ] -> IO ProcessHandle
execEnv cmd args env = do
    (_, _, _, p) <- createProcess $ (proc cmd args) { env = Just env }
    return p

wait :: ProcessHandle -> IO ()
wait p = waitForProcess p >> return ()

fatal :: ProcessHandle -> IO ()
fatal p = do
    exitCode <- waitForProcess p
    case exitCode of
        ExitSuccess -> return ()
        otherwise   -> error $ "Exited with " ++ (show exitCode)

kill :: ProcessHandle -> IO ()
kill = terminateProcess
