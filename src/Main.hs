{-# LANGUAGE DataKinds #-}
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
import qualified Data.Foldable as Fold
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import Data.Proxy
import Math.Combinat.Permutations
import Network.HTTP.Media
import Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import Servant
import Servant.API
import System.Random
import Text.Read (readMaybe)
import Turtle
import qualified Turtle.Bytes

main :: IO ()
main = do
  putStrLn $ unlines $
    "Endpoints"
      : fmap
        ("http://localhost:8080/" <>)
        [ "permutation/random/10",
          "permutation/2341",
          "permutation/2341/power/2",
          "permtree/3"
        ]
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
    :<|> "permutation" :> Capture "perm" Int :> "power" :> Capture "exponent" Int :> Get '[SVG] LBS.ByteString
    :<|> "permtree" :> Capture "n" Int :> Get '[SVG] LBS.ByteString

server :: Server RandomPermAPI
server =
  randomPermutationH
    :<|> permutationH
    :<|> permutationPowerH
    :<|> permTreeH
  where
    randomPermutationH :: Int -> Handler LBS.ByteString
    randomPermutationH n = do
      gen <- liftIO newStdGen
      let perm = fst $ randomPermutation n gen
      servePermutation perm
    -- TODO change Int to Permutation
    permutationH :: Int -> Handler LBS.ByteString
    permutationH permAsInt =
      withPermutation permAsInt servePermutation
    permutationPowerH :: Int -> Int -> Handler LBS.ByteString
    permutationPowerH permAsInt power =
      withPermutation permAsInt (servePermutation . multiplyMany . replicate power)
    permTreeH :: Int -> Handler LBS.ByteString
    permTreeH n
      | 1 <= n && n <= 4 = do
        let allPerms = concatMap show <$> List.permutations [1 .. n]
            edges = List.nub . List.sort $ Fold.concatMap permStrToEdges allPerms
        renderGraphFromEdges Dot edges
      | otherwise = throwError $ err400 {errBody = "Out of range [1 .. 4]"}

permStrToEdges :: String -> [(Int, Int)]
permStrToEdges str =
  zip xs (tail xs)
  where
    xs = rd <$> List.inits str
    rd = fromMaybe 0 . readMaybe

withPermutation :: Int -> (Permutation -> Handler LBS.ByteString) -> Handler LBS.ByteString
withPermutation permAsInt respond = do
  let digits = map digitToInt $ show permAsInt
  case maybePermutation digits of
    Just perm -> respond perm
    Nothing -> throwError $ err400 {errBody = "Not a permutation"}

servePermutation :: Permutation -> Handler LBS.ByteString
servePermutation =
  renderGraphFromEdges Circo . Array.assocs . permutationArray

renderGraphFromEdges :: GraphvizEngine -> [(Int, Int)] -> Handler LBS.ByteString
renderGraphFromEdges engine edges = do
  let dotLines = fmap renderEdge edges
  liftIO $ LBS.writeFile "tmp.dot" $ LBS.intercalate ";" $ "digraph {node[shape=circle]" : dotLines <> ["}"]
  (_, graphSVG) <- Turtle.Bytes.shellStrict (showEngine engine <> " tmp.dot -Tsvg") empty
  pure $ LBS.fromStrict graphSVG

renderEdge :: (Int, Int) -> LBS.ByteString
renderEdge (i, j) =
  Builder.toLazyByteString $
    Builder.intDec i
      <> Builder.char8 '-'
      <> Builder.char8 '>'
      <> Builder.intDec j

data GraphvizEngine = Dot | Circo

showEngine :: GraphvizEngine -> Text
showEngine Dot = "dot"
showEngine Circo = "circo"

data SVG

instance Servant.API.Accept SVG where
  contentType _ = "image" // "svg+xml"

instance MimeRender SVG LBS.ByteString where
  mimeRender _ = id
