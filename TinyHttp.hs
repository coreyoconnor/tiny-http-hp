module Main where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad

import qualified Data.ByteString as BS
import Data.Either
import Data.Maybe
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)

import Network (listenOn, PortID(..), PortNumber)
import Network.HTTP
import Network.Socket
import Network.Stream

import System.Timeout

main = withSocketsDo $ do
    http_socket <- listenOn $ PortNumber 9090
    dispatch_on_accept http_socket $ either handle_failed_request handle_valid_request
    sClose http_socket

handle_failed_request failure = do
    let response = Response (4,0,0) "Bad Request" [mkHeader HdrConnection "close"]
                            (encodeUtf8 $ pack $ show failure)
    return (response, True)

handle_valid_request request = do
    let request_body = rqBody request
    let done = Just "close" == findHeader HdrConnection request
        response = Response (2,0,0) "OK" [] (encodeUtf8 $ pack $ show request_body)
        response' = if done then insertHeader HdrConnection "close" response else response
    return (response', done)

dispatch_on_accept http_socket handler = forever $ accept http_socket >>= forkIO . httpHandler . fst
    where
        httpHandler client_socket = bracket (socketConnection "client" 0 client_socket)
                                            Network.HTTP.close
                                            client_interact
        client_interact :: HandleStream BS.ByteString -> IO ()
        client_interact byte_stream = loop
            where loop = do
                    mbrequest <- timeout (15 * 1000000) $ receiveHTTP byte_stream
                    case mbrequest of
                        Nothing -> return ()
                        Just request -> do
                            (response, done) <- handler request
                            respondHTTP byte_stream response
                            unless done loop

