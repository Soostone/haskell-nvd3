{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Graphics.NVD3.Writer where

import           Data.Aeson
import qualified Data.ByteString.Char8      as B
import           Data.Data
import qualified Data.HashMap.Strict        as H
import qualified Data.List                  as L
import           Data.Monoid
import           Data.String
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as LT
import qualified Data.Text.Lazy.Builder     as B
import qualified Data.Text.Lazy.Builder.Int as B
import           Data.Text.Lazy.Encoding
import           Data.Vector                (Vector)
import qualified Data.Vector                as V
import           Graphics.NVD3.Types

buildJS :: Text -> [Series] -> ChartOptions -> B.Builder
buildJS chart ss options = b "function graphFunction () {\n" <>
                           b "var chart = nv.models." <>
                           b chart <> b "();\n" <>
                           buildChartOptions optionsObj <>
                           (dumpMargins $ margins options) <>
                           buildAxisOptions options <>
                           (if (resize options == Just True)
                            then b "nv.utils.windowResize(function() { chart.update() });\n"
                            else mempty) <>
                           b "var myData = " <> buildSeries ss
                           <> b ";\n" <> maybe mempty b (d3Extra options) <> "\n" <>
                           b "d3.select('" <> b (cssSelector options)
                           <> b "').datum(myData).call(chart);\n" <>
                           b "return chart;\n" <>
                           b "}\nnv.addGraph(graphFunction);\n"
                           where 
                                 optionsObj = mkObject' options
                                 dumpMargins (Just m) = b "chart.margin(" <> (B.fromLazyText $
                                                        decodeUtf8 $ encode m) <> b ");\n"
                                 dumpMargins Nothing = mempty

tab :: Int -> B.Builder
tab n = B.fromText $ T.replicate (4 * n) " "

b :: Text -> B.Builder
b = B.fromText
    
buildChartOptions :: Object -> B.Builder
buildChartOptions options = generateOpts (b "chart") $ filterIrrelevant irrelevantFields options
  where
        filterIrrelevant :: [Text] -> Object -> Object
        filterIrrelevant (x:xs) o = H.delete x $ filterIrrelevant xs o
        filterIrrelevant _ o = o
        irrelevantFields = ["xAxis", "yAxis", "margins", "resize", "cssSelector"]

generateOpts :: B.Builder -> Object -> B.Builder
generateOpts var o = L.foldl1' (<>) $ map snd $ H.toList $
                 H.mapWithKey gen (encodeMap $ filterEmpty o)
  where gen k v = var <> b "." <> b k
               <> b "("  <> b v <> b ");\n"

encodeMap :: Object -> H.HashMap Text Text
encodeMap o = H.map (LT.toStrict . decodeUtf8 . encode) o

buildSeries :: [Series] -> B.Builder
buildSeries ss = B.fromLazyText $ decodeUtf8 $ encode $ toJSON'' ss

buildAxisOptions :: ChartOptions -> B.Builder
buildAxisOptions opts = maybe mempty (buildAxis "x") (xAxis opts) <>
                        maybe mempty (buildAxis "y") (yAxis opts)
  where buildAxis :: Text -> Axis -> B.Builder
        buildAxis xy axis = b "chart.show" <> (b $ T.toUpper xy)
                            <> b "Axis(" <> (if displayed axis then b "true" else b "false")
                            <> b ");\n"
                            <> generateOpts (b "chart." <> b xy <> b "Axis") (H.delete "displayed" $ mkObject $ toJSON $ axis)
                            
swapKeys :: [(Text,Text)] -> H.HashMap Text v -> H.HashMap Text v
swapKeys (t:ts) hm = swapKey t $ swapKeys ts hm
  where swapKey (old,new) o = let val = o H.! old in (H.delete old) $ H.insert new val o
swapKeys _ hm = hm

-- buildChartOptions options = optionsMap' H.tra
--   where optionsMap = undefined
--         optionsMap' = undefined


-- | Returns only the fields which have non-empty values
changedFields :: Object -> Object -> Object
changedFields def new = H.filterWithKey changed new
                        where changed k v = (def H.! k) == v

testChartOptions :: ChartOptions -> LT.Text
testChartOptions (toJSON' -> Object o) = B.toLazyText $ buildChartOptions o

testChartOptionsDef = testChartOptions defChartOptions
