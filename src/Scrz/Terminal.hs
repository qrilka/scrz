module Scrz.Terminal where

import System.IO
import System.Posix.Terminal
import System.Posix.IO
import System.Posix.Types


setRawModeFd :: Fd -> IO TerminalAttributes
setRawModeFd fd = do
    attr <- getTerminalAttributes fd
    let rawAttr = makeRaw attr
    setTerminalAttributes fd rawAttr Immediately
    return attr

setRawMode :: Handle -> IO (TerminalAttributes, Handle)
setRawMode handle = do
    fd <- handleToFd handle
    isatty <- queryTerminal fd
    putStrLn $ "isatty = " ++ (show isatty)
    attr <- setRawModeFd fd

    handle1 <- fdToHandle fd
    return (attr, handle1)

resetModeFd :: Fd -> TerminalAttributes -> IO ()
resetModeFd fd attrs = setTerminalAttributes fd attrs Immediately

resetMode :: Handle -> TerminalAttributes -> IO Handle
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
