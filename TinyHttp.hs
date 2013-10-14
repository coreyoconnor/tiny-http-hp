{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad

import qualified Data.ByteString as BS
import Data.Either
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)

import Network (listenOn, PortID(..), PortNumber)
import Network.HTTP
import Network.Socket
import Network.Stream

main = withSocketsDo $ do
    http_socket <- listenOn $ PortNumber 9090
    dispatch_on_accept http_socket $ \byte_stream ->
        either (\failure -> print failure)
               (\r -> do
                    in_body <- evaluate $! rqBody r
                    respondHTTP byte_stream $ handle_request in_body
               )
    sClose http_socket

handle_request in_body = Response (2,0,0) "OK" [mkHeader HdrConnection "close"] $
    encodeUtf8 $ pack $ show in_body

dispatch_on_accept :: Socket -> (HandleStream BS.ByteString -> Result (Request BS.ByteString) -> IO ()) -> IO ()
dispatch_on_accept http_socket handler = forever $ accept http_socket >>= forkIO . httpHandler . fst
    where
        httpHandler client_socket = bracket (socketConnection "client" 0 client_socket)
                                            Network.HTTP.close
                                            (\byte_stream -> receiveHTTP byte_stream >>= handler byte_stream)
