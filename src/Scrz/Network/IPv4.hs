module Scrz.Network.IPv4 where

import Data.Bits
import Scrz.Types
import Text.Printf


toIPv4 :: [ Int ] -> IPv4
toIPv4 [a1,a2,a3,a4] = IPv4 $ fromIntegral $ shift a1 24 + shift a2 16 + shift a3 8 + a4
toIPv4 _             = error "toIPv4: wrong number of octets"


instance Show IPv4 where
    show (IPv4 a) = show4 a
      where
        remQuo x = (x `mod` 256, x `div` 256)
        show4 q = printf "%d.%d.%d.%d" a1 a2 a3 a4
          where
            (a4,q4) = remQuo q
            (a3,q3) = remQuo q4
            (a2,q2) = remQuo q3
            (a1, _) = remQuo q2
