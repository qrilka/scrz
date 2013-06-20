module Scrz.Terminal where

import Data.Maybe
import Control.Monad
import System.IO
import System.Posix.Terminal
import System.Posix.IO
import System.Posix.Types


setRawModeFd :: Fd -> IO (Maybe TerminalAttributes)
setRawModeFd fd = do
    isatty <- queryTerminal fd
    if not isatty
        then return Nothing
        else do
            attr <- getTerminalAttributes fd
            setTerminalAttributes fd (makeRaw attr) Immediately
            return $ Just attr

setRawMode :: Handle -> IO (Maybe TerminalAttributes, Handle)
setRawMode handle = do
    fd <- handleToFd handle
    attr <- setRawModeFd fd
    handle1 <- fdToHandle fd
    return (attr, handle1)

resetModeFd :: Fd -> Maybe TerminalAttributes -> IO ()
resetModeFd fd Nothing = return ()
resetModeFd fd (Just attrs) = setTerminalAttributes fd attrs Immediately

resetMode :: Handle -> Maybe TerminalAttributes -> IO Handle
resetMode handle attrs = do
    fd <- handleToFd handle
    resetModeFd fd attrs
    fdToHandle fd

-- cfmakeraw() clears these flags:
-- IGNBRK | BRKINT | PARMRK | ISTRIP | INLCR | IGNCR | ICRNL | IXON
-- OPOST
-- ECHO | ECHONL | ICANON | ISIG | IEXTEN
-- CSIZE | PARENB
--
-- termios_p->c_cflag |= CS8

-- Missing: CSIZE, CS8
makeRaw :: TerminalAttributes -> TerminalAttributes
makeRaw attrs = foldl withoutMode attrs
    [ IgnoreBreak        -- IGNBRK
    , InterruptOnBreak   -- BRKINT
    , MarkParityErrors   -- PARMRK
    , StripHighBit       -- ISTRIP
    , MapLFtoCR          -- INLCR
    , IgnoreCR           -- IGNCR
    , MapCRtoLF          -- ICRNL
    , StartStopOutput    -- IXON
    , ProcessOutput      -- OPOST
    , EnableEcho         -- ECHO
    , EchoLF             -- ECHONL
    , ProcessInput       -- ICANON
    , KeyboardInterrupts -- ISIG
    , ExtendedFunctions  -- IEXTEN
    , EnableParity       -- PARENB
    ]
