module Actions where

import           Data.Aeson
import           Data.Word
import           Protocol

invokeMethod :: String -> Maybe [Value] -> Word32 -> ClientMessage
invokeMethod name = ClientMessage Method (Just name)

initConn :: Word32 -> ClientMessage
initConn = ClientMessage Connect Nothing $ Just [Number 0]

auth :: String -> Word32 -> ClientMessage
auth token = undefined