{-# LANGUAGE OverloadedStrings #-}
module Main where

import Snap.Core
import Snap.Http.Server
import Snap.Util.FileServe
import Control.Applicative

import Data.Int

import qualified Data.ByteString.Lazy.Char8 as C
import JsonInput

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site = route [ ("hs/rest/prediction", echoHandler)
             ] <|>
       dir "site" (serveDirectory "site")

maxBodySize :: Int64
maxBodySize = 2048

echoHandler :: Snap ()
echoHandler = do
  body <- readRequestBody maxBodySize
  let prediction = jsonPrediction $ C.unpack body
  writeLBS $ C.pack prediction
