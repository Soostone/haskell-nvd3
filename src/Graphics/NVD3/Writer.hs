{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

module Graphics.NVD3.Writer where

import           Data.Aeson

import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Data
import qualified Data.HashMap.Strict        as H
import           Data.List
import           Data.Monoid
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Lazy.Builder     as B
import qualified Data.Text.Lazy.Builder.Int as B
import           Data.Text.Lazy.Encoding
import           Data.Vector.Unboxed        (Vector)
import qualified Data.Vector.Unboxed        as V
import           Graphics.NVD3.NVTypes

buildJS :: Text -> [Series] -> ChartOptions -> B.Builder
buildJS chart ss options = B.fromText "function graphFunction () {\n" <>
                           (tab 1) <> B.fromText "var chart = nv.models." <>
                           B.fromText chart <> B.fromText "()\n" <>
                           (tab 2) <> B.fromText ".transitionDuration(350)\n" <>
                           (tab 2) <> B.fromText ".showLegend(true)\n" <>
                           (tab 2) <> B.fromText ".useInteractiveGuideline(true)\n" <>
                           (tab 1) <> B.fromText ";\n\n" <>
                           B.fromText "nv.addGraph(graphFunction);\n"

tab :: Int -> B.Builder
tab n = B.fromText $ T.replicate (4 * n) " "

buildChartOption :: Object -> B.Builder
-- buildChartOption options = map generate $ H.toList options
                           -- where generate k v = B.fromText "." <> B.fromText k <> B.fromText "("  <> v  <> B.fromText ");"
buildChartOption options = undefined

-- | Returns only the fields changed from the defaults
changedFields :: Object -> Object -> Object
changedFields def new = H.filterWithKey changed new
                        where changed k v = (def H.! k) == v
