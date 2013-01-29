module Scrz.Log (logger) where

import Data.Time.LocalTime (ZonedTime(..), getZonedTime)
import Data.Time.Format    (formatTime)

import Text.Printf         (printf)
import System.Locale       (defaultTimeLocale)


cleanCalendar :: ZonedTime -> String
cleanCalendar = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"

logger :: String -> IO ()
logger msg = do
    zt <- getZonedTime
    printf "[%s] %s\n" (cleanCalendar zt) msg
