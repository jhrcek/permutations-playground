{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.IO.Class
import qualified Data.Array as Array
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import Data.Proxy
import Math.Combinat.Permutations
import Network.HTTP.Media
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.API
import System.Random
import Turtle
import qualified Turtle.Bytes

main :: IO ()
main = do 
  putStrLn "Try http://localhost:8080/rp/10"
  run 8080 app

app :: Application
app = serve (Proxy @RandomPermAPI) server

type RandomPermAPI = "rp" :> Capture "n" Int :> Get '[SVG] LBS.ByteString

server :: Server RandomPermAPI
server = randomPermutationH
  where
    randomPermutationH :: Int -> Handler LBS.ByteString
    randomPermutationH n = do
      gen <- liftIO newStdGen
      let dotLines =
            fmap
              ( \(i, j) ->
                  Builder.toLazyByteString $
                    Builder.intDec i
                      <> Builder.char8 '-'
                      <> Builder.char8 '>'
                      <> Builder.intDec j
              )
              . Array.assocs
              $ permutationArray
              $ fst
              $ randomPermutation n gen
      liftIO $ LBS.writeFile "tmp.dot" $ LBS.intercalate "\n" $ "digraph {" : "node[shape=circle]" : dotLines <> ["}"]
      (_, graphSVG) <- Turtle.Bytes.shellStrict "circo tmp.dot -Tsvg" empty
      pure $ LBS.fromStrict graphSVG

data SVG

instance Servant.API.Accept SVG where
  contentType _ = "image" // "svg+xml"

instance MimeRender SVG LBS.ByteString where
  mimeRender _ = id
