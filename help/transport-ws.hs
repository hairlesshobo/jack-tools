{-

- read /time OSC messages sent by jack-osc
- write /text OSC-JSON messages to websocket

-}


import Control.Monad {- base -}

import qualified Network.WebSockets as W {- websockets -}

import qualified Music.Theory.Time.Notation as T {- hmt -}

import Sound.OSC.FD {- hosc -}

import Sound.OSC.Type.JSON {- hosc-json -}

work :: UDP -> W.Connection -> IO ()
work u c = do
  m <- recvMessage u
  case m of
    Just (Message "/time" [Double x]) -> do
      let msg = message "/text" [string "txt",string (T.minsec_pp (T.fsec_to_minsec x))]
          js = json_encode_value (encode_message msg)
      W.sendTextData c js
    _ -> return ()

app :: UDP -> W.PendingConnection -> IO ()
app u rq = do
  c <- W.acceptRequest rq
  putStrLn "CONNECT"
  forever (work u c)

main :: IO ()
main = do
  let h = "192.168.1.7" -- "192.168.1.7" -- "127.0.0.1"
      w = 9160
      p = 57130 -- jack-osc
  withTransport
    (openUDP h p)
    (\u -> do
        sendMessage u (message "/receive" [int32 0x10])
        putStrLn "WAIT"
        W.runServer h w (app u))
