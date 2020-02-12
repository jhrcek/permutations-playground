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
import Data.Char (digitToInt)
import Data.Proxy
import Math.Combinat.Permutations
import Network.HTTP.Media
import Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import Servant
import Servant.API
import System.Random
import Turtle
import qualified Turtle.Bytes

main :: IO ()
main = do
  putStrLn "Try http://localhost:8080/permutation/random/10"
  Warp.runSettings settings app

settings :: Warp.Settings
settings =
  Warp.defaultSettings
    & Warp.setTimeout 5
    & Warp.setPort 8080

app :: Application
app = serve (Proxy @RandomPermAPI) server

type RandomPermAPI =
  "permutation" :> "random" :> Capture "n" Int :> Get '[SVG] LBS.ByteString
    :<|> "permutation" :> Capture "perm" Int :> Get '[SVG] LBS.ByteString
    :<|> "permutation" :> Capture "perm" Int :> Capture "exponent" Int :> Get '[SVG] LBS.ByteString

server :: Server RandomPermAPI
server =
  randomPermutationH
    :<|> permutationH
    :<|> permutationPowerH
  where
    randomPermutationH :: Int -> Handler LBS.ByteString
    randomPermutationH n = do
      gen <- liftIO newStdGen
      let perm = fst $ randomPermutation n gen
      servePermutation perm
    permutationH :: Int -> Handler LBS.ByteString
    permutationH permAsInt = do
      let digits = map digitToInt $ show permAsInt
      case maybePermutation digits of
        Just perm -> servePermutation perm
        Nothing -> pure $ "Not a permutation"
    permutationPowerH :: Int -> Int -> Handler LBS.ByteString
    permutationPowerH permAsInt power = do
      let digits = map digitToInt $ show permAsInt
      case maybePermutation digits of
        Just perm -> servePermutation $ multiplyMany $ replicate power perm
        Nothing -> pure $ "Not a permutation"

data SVG

instance Servant.API.Accept SVG where
  contentType _ = "image" // "svg+xml"

instance MimeRender SVG LBS.ByteString where
  mimeRender _ = id

servePermutation :: Permutation -> Handler LBS.ByteString
servePermutation perm = do
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
          $ permutationArray perm
  liftIO $ LBS.writeFile "tmp.dot" $ LBS.intercalate ";" $ "digraph {node[shape=circle]" : dotLines <> ["}"]
  (_, graphSVG) <- Turtle.Bytes.shellStrict "circo tmp.dot -Tsvg" empty
  pure $ LBS.fromStrict graphSVG
