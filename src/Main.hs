{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import qualified Data.Array as Array
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as List
import qualified Network.Wai.Handler.Warp as Warp
import qualified Turtle.Bytes

import Data.ByteString (ByteString)
import Data.Char (digitToInt)
import Data.Containers.ListUtils (nubOrd)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Math.Combinat.Permutations (Permutation, maybePermutation, permutationArray, productOfPermutations, randomPermutation)
import Network.HTTP.Media ((//))
import Network.Wai.Handler.Warp (Port)
import Servant (Accept, Application, Capture, Get, Handler, MimeRender, Server, contentType, err400, err500, errBody, mimeRender, serve, throwError, (:<|>) (..), (:>))
import System.Random (newStdGen)
import Text.Read (readMaybe)
import Turtle hiding (d, x)


main :: IO ()
main = do
    putStrLn $
        unlines $
            "Endpoints" :
            fmap
                (\path -> "http://localhost:" <> show appPort <> "/" <> path)
                [ "permutation/random/10"
                , "permutation/2341"
                , "permutation/2341/power/2"
                , "permtree/3"
                , "euclid-algo/168/188"
                ]
    Warp.runSettings settings app


settings :: Warp.Settings
settings =
    Warp.defaultSettings
        & Warp.setTimeout 5
        & Warp.setPort appPort


appPort :: Port
appPort = 8080


app :: Application
app = serve (Proxy @RandomPermAPI) server


type RandomPermAPI =
    "permutation" :> "random" :> Capture "n" Int :> Get '[SVG] SvgGraph
        :<|> "permutation" :> Capture "perm" Int :> Get '[SVG] SvgGraph
        :<|> "permutation" :> Capture "perm" Int :> "power" :> Capture "exponent" Int :> Get '[SVG] SvgGraph
        :<|> "permtree" :> Capture "n" Int :> Get '[SVG] SvgGraph
        :<|> "euclid-algo" :> Capture "a" Int :> Capture "b" Int :> Get '[SVG] SvgGraph


newtype SvgGraph = SvgGraph ByteString


server :: Server RandomPermAPI
server =
    randomPermutationH
        :<|> permutationH
        :<|> permutationPowerH
        :<|> permTreeH
        :<|> euclidAlgoH
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
        withPermutation permAsInt (servePermutation . productOfPermutations . replicate power)

    permTreeH :: Int -> Handler SvgGraph
    permTreeH n
        | 1 <= n && n <= 4 = do
            let allPerms = concatMap show <$> List.permutations [1 .. n]
                edges = nubOrd . List.sort $ concatMap permStrToEdges allPerms
                dotSrc = renderPermutation edges
            serveDot Dot ["-Tsvg", "-Nshape=circle", "-Nfixedsize=true"] dotSrc
        | otherwise = throwError $ err400{errBody = "Out of range [1 .. 4]"}

    euclidAlgoH :: Int -> Int -> Handler SvgGraph
    euclidAlgoH a b =
        let dotSrc = renderEuclidStepsGraph a b
         in serveDot Dot ["-Tsvg", "-Nshape=circle", "-Nfixedsize=true", "-Grankdir=BT"] dotSrc


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
        Nothing -> throwError $ err400{errBody = "Not a permutation"}


servePermutation :: Permutation -> Handler SvgGraph
servePermutation perm = do
    let dotSrc = renderPermutation $ Array.assocs $ permutationArray perm
    serveDot Circo ["-Tsvg", "-Nshape=circle", "-Nfixedsize=true"] dotSrc


serveDot :: GraphvizEngine -> [Text] -> DotSource -> Handler SvgGraph
serveDot engine gvParams (DotSource dotSrc) = do
    (exitCode, graphSVG) <- Turtle.Bytes.procStrict (engineCommand engine) gvParams $ pure dotSrc
    case exitCode of
        ExitSuccess -> pure $ SvgGraph graphSVG
        ExitFailure _ -> throwError $ err500{errBody = "Failed to generate svg image."}


renderPermutation :: [(Int, Int)] -> DotSource
renderPermutation edges =
    let dotLines = foldMap renderEdge edges
     in DotSource $
            LBS.toStrict $
                Builder.toLazyByteString $
                    Builder.string7 "digraph{" <> dotLines <> Builder.char8 '}'


renderEdge :: (Int, Int) -> Builder.Builder
renderEdge (i, j) =
    Builder.intDec i
        <> Builder.char8 '-'
        <> Builder.char8 '>'
        <> Builder.intDec j
        <> Builder.char8 ';'


renderEdgeNoConstraint :: (Int, Int) -> Builder.Builder
renderEdgeNoConstraint (i, j) =
    Builder.intDec i
        <> Builder.char8 '-'
        <> Builder.char8 '>'
        <> Builder.intDec j
        <> Builder.string8 "[constraint=false,color=red];"


euclidSteps :: Int -> Int -> [Int]
euclidSteps a b
    | a >= b = a : b : go a b
    | otherwise = b : a : go b a
  where
    go x y
        | r == 0 = []
        | otherwise = r : go y r
      where
        r = mod x y


renderEuclidStepsGraph :: Int -> Int -> DotSource
renderEuclidStepsGraph a b =
    let steps = euclidSteps a b
        allDivisors = nubOrd $ List.sort $ concatMap (\x -> filter (\d -> mod x d == 0) [1 .. x]) steps
        edges =
            [ (x, y)
            | x <- allDivisors
            , y <- allDivisors
            , x < y
            , mod y x == 0
            , -- TODO this is naive transitive reduction.
            -- But doing it by graphviz's `tread`, we run into
            -- cycles generated by renderEdgeNoConstraint below
            null [z | z <- allDivisors, x < z, z < y, mod y z == 0, mod z x == 0]
            ]
        dotLines =
            foldMap renderEdge edges
                <> foldMap renderEdgeNoConstraint (zip steps (tail steps))
     in DotSource $
            LBS.toStrict $
                Builder.toLazyByteString $
                    Builder.string7 "digraph{" <> dotLines <> Builder.char8 '}'


newtype DotSource = DotSource ByteString


data GraphvizEngine = Dot | Circo


engineCommand :: GraphvizEngine -> Text
engineCommand Dot = "dot"
engineCommand Circo = "circo"


data SVG


instance Accept SVG where
    contentType _ = "image" // "svg+xml"


instance MimeRender SVG SvgGraph where
    mimeRender _ (SvgGraph bs) = LBS.fromStrict bs


-- Steinhaus' algorithm from "Adventures in Group Theory: Rubik's Cube, Merlin's Machine and Other Mathematical Toys"
-- Generates list of all permutations for given n.
-- Identity is first and each of the next permutations can be optained from previous by composing with a transposition.
steinhaus :: Int -> [[Int]]
steinhaus n
    | n <= 0 = [[]]
    | otherwise = inserts False n $ steinhaus (n - 1)
  where
    inserts :: Bool -> Int -> [[Int]] -> [[Int]]
    inserts _ _ [] = []
    inserts True x (p : ps) = ins x p <> inserts False x ps
    inserts False x (p : ps) = reverse (ins x p) <> inserts True x ps

    ins :: Int -> [Int] -> [[Int]]
    ins x [] = [[x]]
    ins x (y : ys) = (x : y : ys) : fmap (y :) (ins x ys)
