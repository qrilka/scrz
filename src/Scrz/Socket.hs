module Scrz.Socket where

import Data.Aeson

import Control.Monad
import Control.Applicative
import Control.Concurrent

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString.Lazy
import Prelude hiding (getContents)

import Control.Concurrent.STM.TVar

import Scrz.Log
import Scrz.Types
import Scrz.Container
import Scrz.Commands


controlSocketPath    = "/var/run/scrz.sock"
controlSocketAddress = SockAddrUnix controlSocketPath
createControlSocket  = socket AF_UNIX Stream 0

serverSocket :: IO Socket
serverSocket = do
    sock <- createControlSocket
    bind sock controlSocketAddress
    listen sock 10

    return sock


handleClient :: TVar Runtime -> Socket -> IO ()
handleClient runtime socket = do
    (clientSocket, addr) <- accept socket
    logger "Accepted connection"

    json <- recv clientSocket 99999
    case decode json of
        Nothing -> do
            logger "Could not decode command"
            close clientSocket
            return ()

        Just cmd -> do
            response <- processCommand runtime cmd
            sendAll clientSocket $ encode response
            close clientSocket

clientSocket :: IO Socket
clientSocket = do
    sock <- createControlSocket
    connect sock controlSocketAddress

    return sock

sendCommand :: Command -> IO (Maybe Response)
sendCommand command = do
    socket <- clientSocket

    sendAll socket (encode command)
    response <- getContents socket
    case decode response of
        Nothing -> return Nothing
        Just response -> do
            printResponse response
            return $ Just response
