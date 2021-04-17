import qualified Data.ByteString.Char8 as BC
import           Network.HTTP.Simple


-- {-# LANGUAGE OverloadedStrings -#}

-- getResponseHeaders <$> response

-- Q39.1 Build a function buildRequestNOSSL that works
-- exactly like buildRequest, only it doesn’t support SSL.

buildRequestNOSSL :: BC.ByteString -> BC.ByteString -> BC.ByteString
                  -> BC.ByteString -> Request
buildRequestNOSSL token host method path = setRequestMethod method
                                    $ setRequestHost host
                                    $ setRequestHeader "token" [token]
                                    $ setRequestPath path
                                    $ defaultRequest

-- Q39.2 Improve the output of your code when something
-- goes wrong. getResponseStatus will give you a data
-- type including both the statusCode and the statusMessage.
-- Fix main so that if you do get a non-200 statusCode,
-- you print out the appropriate error.
