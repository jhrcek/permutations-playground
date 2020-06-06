{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.IO.Class
import qualified Data.Array as Array
import Data.ByteString (ByteString)
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
  "permutation" :> "random" :> Capture "n" Int :> Get '[SVG] SvgGraph
    :<|> "permutation" :> Capture "perm" Int :> Get '[SVG] SvgGraph
    :<|> "permutation" :> Capture "perm" Int :> "power" :> Capture "exponent" Int :> Get '[SVG] SvgGraph
    :<|> "permtree" :> Capture "n" Int :> Get '[SVG] SvgGraph

newtype SvgGraph = SvgGraph ByteString

server :: Server RandomPermAPI
server =
  randomPermutationH
    :<|> permutationH
    :<|> permutationPowerH
    :<|> permTreeH
  where
    randomPermutationH :: Int -> Handler SvgGraph
    randomPermutationH n = do
      gen <- liftIO newStdGen
      let perm = fst $ randomPermutation n gen
      servePermutation perm
    -- TODO change Int to Permutation
    permutationH :: Int -> Handler SvgGraph
    permutationH permAsInt =
      withPermutation permAsInt servePermutation
    permutationPowerH :: Int -> Int -> Handler SvgGraph
    permutationPowerH permAsInt power =
      withPermutation permAsInt (servePermutation . multiplyMany . replicate power)
    permTreeH :: Int -> Handler SvgGraph
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

withPermutation :: Int -> (Permutation -> Handler SvgGraph) -> Handler SvgGraph
withPermutation permAsInt respond = do
  let digits = map digitToInt $ show permAsInt
  case maybePermutation digits of
    Just perm -> respond perm
    Nothing -> throwError $ err400 {errBody = "Not a permutation"}

servePermutation :: Permutation -> Handler SvgGraph
servePermutation =
  renderGraphFromEdges Circo . Array.assocs . permutationArray

renderGraphFromEdges :: GraphvizEngine -> [(Int, Int)] -> Handler SvgGraph
renderGraphFromEdges engine edges = do
  let dotLines = foldMap renderEdge edges
      dotContent =
        LBS.toStrict
          $ Builder.toLazyByteString
          $ Builder.string7 "digraph{node[shape=circle];" <> dotLines <> Builder.char8 '}'
  (exitCode, graphSVG) <- Turtle.Bytes.procStrict (engineCommand engine) ["-Tsvg"] (pure dotContent)
  case exitCode of
    ExitSuccess -> pure $ SvgGraph graphSVG
    ExitFailure _ -> throwError $ err500 {errBody = "Failed to generate svg image."}

renderEdge :: (Int, Int) -> Builder.Builder
renderEdge (i, j) =
  Builder.intDec i
    <> Builder.char8 '-'
    <> Builder.char8 '>'
    <> Builder.intDec j
    <> Builder.char8 ';'

data GraphvizEngine = Dot | Circo

engineCommand :: GraphvizEngine -> Text
engineCommand Dot = "dot"
engineCommand Circo = "circo"

data SVG

instance Servant.API.Accept SVG where
  contentType _ = "image" // "svg+xml"

instance MimeRender SVG SvgGraph where
  mimeRender _ (SvgGraph bs) = LBS.fromStrict bs
