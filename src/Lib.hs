{-# LANGUAGE OverloadedStrings #-}

module Lib ( run ) where

import Control.Concurrent (threadDelay)
import Control.Exception (catch)
import Control.Lens ((^.), (&), (?~))
import Control.Monad (forever, when)

import Data.Aeson.Lens (_String, key, _Array)
import Data.Aeson.Types (Value(String, Number, Object))
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import Data.List (intercalate)
import Data.Scientific (coefficient)
import Data.Text (unpack)
import Data.ByteString.Char8 (pack)
import Data.Vector (toList)

import Network.HTTP.Client (HttpException)
import Network.Wreq
import Network.Wreq.Lens (proxy)
import Network.Wreq.Session as S

import System.Environment (getArgs)
import System.Hclip (setClipboard)

initialSkipIds = ["1"]

seconds = (*1000000)

url = ("https://api.guerrillamail.com/ajax.php?f=" ++)

s (String s) = do unpack s
s (Number s) = do show $ coefficient s

at field (Object obj) = do s $ fromMaybe "NA" (HM.lookup field obj)

mailId  = at "mail_id"
from    = at "mail_from"
subject = at "mail_subject"
excerpt = at "mail_excerpt"
body    = at "mail_excerpt"

skip skipIds email =
  not $ any (== mailId email) skipIds

displayEmail email = intercalate "\n" [
  "** From:    " ++ from email,
  "** Subject: " ++ subject email,
  "** Body:    " ++ if _body == "NA" then excerpt email else _body
  ]
  where _body = body email

toEmailList response = toList (response ^. responseBody . key "list" . _Array)

fetchNewEmails get skipIds = do
  threadDelay (seconds 5)
  response <- get "get_email_list&offset=0"
  return $ filter (skip skipIds) (toEmailList response)

returnEmpty :: HttpException -> IO [a]
returnEmpty _ = do
  putStrLn "Momentary error connecting to the server... retrying."
  return []

pollNewEmails get skipIds = do
  newEmails <- catch (fetchNewEmails get skipIds) returnEmpty
  when (length newEmails > 0)
    (putStrLn $ intercalate "\n\n\n---------\n\n\n" (map displayEmail newEmails))
  pollNewEmails get (skipIds ++ map mailId newEmails)

fetchEmailAddress get = do
  response <- get "get_email_address"
  return $ unpack (response ^. responseBody . key "email_addr" . _String)

setupGet sess [host, port] =
  S.getWith (defaults & proxy ?~ httpProxy (pack host) (read port)) sess . url
setupGet sess [] =
  S.get sess . url
setupGet sess _ =
  error "Misconfigured proxy. Use `tmpemail HOST PORT`."

run :: IO ()
run = S.withSession $ \sess -> do
  args <- getArgs
  let get = setupGet sess args in do
    email <- fetchEmailAddress get
    setClipboard email
    putStrLn $ "The following email is copyied to your clipboard: " ++ email
    putStrLn "New emails are being polled, press Ctrl+C to quit..."
    pollNewEmails get initialSkipIds
