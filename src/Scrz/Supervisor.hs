module Scrz.Supervisor (supervise) where

import qualified Data.ByteString.Lazy as BS
import           Data.Char
import           Data.Digest.Pure.SHA
import           Data.Foldable (foldrM)
import qualified Data.Map as M
import           Data.Maybe

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TQueue
import           Control.Concurrent.STM.TVar
import           Control.Exception
import           Control.Monad
import           Control.Monad.Maybe
import           Control.Monad.Trans

import           System.Directory
import           System.Exit
import           System.IO
import           System.IO.Error
import           System.Process (createProcess, proc, waitForProcess)
import           System.Process (CreateProcess(..), StdStream(..))
import           System.Random

import           Network.Curl

import           Scrz.Log
import           Scrz.Data


type MaybeIO = MaybeT IO

readLoop :: Handle -> IO ()
readLoop h = readLine >>= either (const $ return ()) printAndContinue
  where
    readLine = tryIOError $ hGetLine h
    printAndContinue x = logger x >> readLoop h

slugContents release = MaybeT $ do
    let url = releaseSlugURL release
    logger $ "Downloading slug from " ++ url

    (code, stream) <- curlGetString_ url curlOptions
    case code of
        CurlOK    -> return $ Just stream
        otherwise -> logger ("Failed to download the slug: " ++ (show code)) >> return Nothing
  where
    curlOptions = [ CurlPost False, CurlNoBody False, CurlFollowLocation True ]

verifyHash release byteString = MaybeT $ do
    logger "Verifying slug hash..."
    if (showDigest $ sha1 byteString) == (releaseSlugHash release)
    then return $ Just byteString
    else logger "Slug checksum is wrong..." >> (return Nothing) >> (return $ Just byteString)

writeToFile release slugFileName byteString = MaybeT $ do
    BS.writeFile slugFileName byteString
    return $ Just slugFileName


mountSlug release sourceFile = MaybeT $ do
    cwd <- getCurrentDirectory
    let mountPoint = cwd ++ "/" ++ (releaseIdentifier release)
    createDirectoryIfMissing False mountPoint
    (_, _, _, p) <- createProcess (proc "mount" [ sourceFile, mountPoint ])
    exitCode <- waitForProcess p
    case exitCode of
        ExitSuccess -> return $ Just mountPoint
        otherwise   -> return Nothing

runService release sharedService mountPoint = MaybeT $ do
    logger "Mounted slug.. starting..."

    let env = M.toList $ releaseEnvironment release
    let cmd = words $ releaseEntryPoint release
    let entryPoint = mountPoint ++ "/" ++ (head cmd)
    logger entryPoint
    (_, Just hOut, _, p) <- createProcess (proc entryPoint (tail cmd)) { std_out = CreatePipe, cwd = Just mountPoint, env = Just env, std_in = CreatePipe }
    updateProcessHandle $ Just $ Process release p
    readLoop hOut

    exitCode <- waitForProcess p
    logger $ show exitCode

    logger "Finished..."
    updateProcessHandle Nothing

    logger "Unmounting"
    (_, _, _, p) <- createProcess (proc "umount" [ mountPoint ])
    umountExitCode <- waitForProcess p
    logger $ show umountExitCode

    return $ Just exitCode

  where

    updateProcessHandle x = atomically $ do
        service <- readTVar sharedService
        writeTVar sharedService $ service { serviceProcess = x }

supervise :: TVar Service -> IO ()
supervise sharedService = do
    service <- atomically $ readTVar sharedService
    let rel = serviceRelease service

    logger $ "Starting service " ++ (serviceName service)
    (runMaybeT $ download rel >>= mount rel >>= run rel sharedService) >>= finally

  where

    (<?>) a b x = if x then a else b

    download :: Release -> MaybeIO String
    download rel = MaybeT $ isCached >>= fromCache <?> downloadAndCache
      where
        slugFileName = (releaseIdentifier rel) ++ ".slug"
        isCached = doesFileExist slugFileName
        fromCache = return $ Just slugFileName
        downloadAndCache = runMaybeT $ slugContents rel >>= verifyHash rel >>= writeToFile rel slugFileName

    mount :: Release -> String -> MaybeIO String
    mount = mountSlug

    run :: Release -> TVar Service -> String -> MaybeIO ExitCode
    run release sharedService = runService release sharedService

    finally :: Maybe ExitCode -> IO ()
    finally x = do
        service <- atomically $ readTVar sharedService
        if serviceActive service
        then do logger "Restarting..." >> repeat
        else do logger "Quit" >> return ()

    sleep x = threadDelay $ x * 1000000
    repeat = sleep 9 >> supervise sharedService
