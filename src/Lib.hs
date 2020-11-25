{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           Actions
import           Control.Concurrent       (forkIO)
import           Control.Concurrent.Timer
import           Control.Monad            (forever, unless)
import           Data.Aeson
import           Data.Aeson.Text          (encodeToLazyText)
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Text.Encoding       (decodeUtf8)
import qualified Data.Text.IO             as T
import           Data.Text.Lazy           (toStrict)
import           Data.Word
import qualified Network.WebSockets       as WS
import           Protocol
import           System.IO
import           System.Random

prompt' :: Text -> IO Text
prompt' msg = do
  T.putStr msg
  hFlush stdout
  T.getLine

getInput :: IO (Either ClientMessage Text)
getInput = do
  input <- prompt' "==> "
  if T.null input
    then getInput
    else do
      completedInput <- processInput input
      mid <- randomIO
      return
        $ case completedInput of
          Left f  -> Left $ f mid
          Right t -> Right t

addData :: Text -> Maybe Object -> Text
addData a (Just b) = a `T.append` T.pack (show b)
addData _ Nothing  = "No data available"

processInput :: Text -> IO (Either (Word32 -> ClientMessage) Text)
processInput "echo" = do
  text <- prompt' "Echo message: "
  return $ Left $ ClientMessage Method (Just "Echo") (Just [toJSON text])
processInput x = return $ Right $ "Invalid command: " `T.append` x

checkSuccess :: Maybe ServerMessage -> Bool
checkSuccess (Just a) = isSuccess a
checkSuccess Nothing  = False

displayMessage :: ServerMessage -> Text
displayMessage (ServerMessage ServerResponse mid succ mdata) = "Operation "
  `T.append` T.pack (show mid)
  `T.append` (if succ
              then " succeeded: "
              else " failed: ")
  `T.append` toStrict (encodeToLazyText mdata)
displayMessage (ServerMessage Event _ _ mdata) =
  "New event received: " `addData` mdata

app :: WS.ClientApp ()
app conn = do
  mid <- randomIO
  WS.sendTextData conn $ encode $ initConn mid
  connRes <- WS.receiveData conn
  let decoded = decode connRes
  unless (checkSuccess decoded)
    $ do
      putStrLn "Error while connecting!"
      print connRes
      error $ show decoded
  putStrLn "## Connected!\n"
  -- Fork a thread that writes WS data to stdout
  forkIO
    $ forever
    $ do
      -- TODO message handling
      msg <- WS.receiveData conn
      T.putStrLn $ maybe (decodeUtf8 msg) displayMessage $ decodeStrict msg
  let loop = do
        msg <- getInput
        case msg of
          Right e  -> T.putStrLn $ "!! " `T.append` e
          Left msg -> do
            WS.sendTextData conn $ encode msg
        loop
  loop
  WS.sendClose conn ("Bye!" :: Text)
