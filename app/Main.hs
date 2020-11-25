module Main where

import Lib
import Network.Socket (withSocketsDo)
import qualified Network.WebSockets as WS
import System.IO

main :: IO ()
main = do
  addr <- prompt "Address [localhost]: "
  port <- prompt "Port [5000]: "
  withSocketsDo $ WS.runClient (addr `stror` "localhost") (read $ port `stror` "5000") "/ws" app

stror :: String -> String -> String
stror "" x = x
stror x _ = x

prompt :: String -> IO String
prompt msg = do
  putStr msg
  hFlush stdout
  getLine

