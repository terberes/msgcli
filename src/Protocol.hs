{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Protocol where

import           Data.Aeson
import qualified Data.Text as T
import           Data.Word
import           GHC.Generics

data ClientMessageType =
    Connect
  | Auth
  | Method
  | Subscribe
  | Unsubscribe
  | Response
  deriving (Show, Generic, ToJSON, FromJSON, Eq)

data ServerMessageType = ServerResponse
                       | Event
  deriving (Show, Eq)

instance FromJSON ServerMessageType where
  parseJSON = withText "ServerMessageType"
    $ \v -> case v of
      "Response" -> return ServerResponse
      "Event"    -> return Event
      _          -> fail $ "No such variant " ++ T.unpack v

instance ToJSON ServerMessageType where
  toJSON ServerResponse = "Response"
  toJSON Event = "Event"

data Message = Client ClientMessage
             | Server ServerMessage

data ClientMessage = ClientMessage { cMsgType :: ClientMessageType
                                   , method :: Maybe String
                                   , params :: Maybe [Value]
                                   , cmid :: Word32
                                   }
  deriving (Show)

instance FromJSON ClientMessage where
  parseJSON = withObject "ClientMessage"
    $ \v -> ClientMessage <$> v .: "Type"
    <*> v .:? "Method"
    <*> v .:? "Params"
    <*> v .: "ID"

instance ToJSON ClientMessage where
  toJSON (ClientMessage mt mm mp mid) =
    object ["Type" .= mt, "ID" .= mid, "Method" .= mm, "Params" .= mp]

data ServerMessage = ServerMessage { sMsgType :: ServerMessageType
                                   , smid :: Word32
                                   , isSuccess :: Bool
                                   , msgData :: Maybe Object
                                   }
  deriving (Show)

instance ToJSON ServerMessage where
  toJSON (ServerMessage mt mid iss md) =
    object ["Type" .= mt, "ID" .= mid, "IsSuccess" .= iss, "Data" .= md]

instance FromJSON ServerMessage where
  parseJSON = withObject "ServerMessage"
    $ \v -> ServerMessage <$> v .: "Type"
    <*> v .: "ID"
    <*> v .: "IsSuccess"
    <*> v .:? "Data"
