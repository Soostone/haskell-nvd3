{-# LANGUAGE OverloadedStrings #-}

module Graphics.NVD3.Writer where

import           Data.Aeson
import qualified Data.ByteString.Char8   as B
import           Data.Data
import qualified Data.HashMap.Strict     as H
import qualified Data.List               as L
import           Data.Monoid
import           Data.String
import qualified Data.Text               as T
import           Data.Text.Lazy          (Text)
import qualified Data.Text.Lazy          as LT
import qualified Data.Text.Lazy.Builder  as B
import           Data.Text.Lazy.Encoding
import           Data.Vector             (Vector)
import qualified Data.Vector             as V
import           Graphics.NVD3.Types

buildJS :: Text -> [Series] -> ChartOptions -> B.Builder
buildJS chart ss options = b "function graphFunction () {\n" <>
                           b "var chart = nv.models." <>
                           b chart <> b "();\n" <>
                           buildChartOptions options <>
                           (dumpMargins $ margins options) <>
                           buildAxisOptions options chart <>
                           (if (resize options == Just True)
                            then b "nv.utils.windowResize(chart.update);\n"
                            else mempty) <>
                           b "var myData = " <>
                           (case chart of
                               "pieChart" -> b $ encodeText $ head ss
                               "bulletChart" -> L.foldl1' (<>) $ map (\bval -> b (encodeText' bval)) ss
                               _ -> buildSeries ss)
                           <> b ";\n" <> maybe mempty b (d3Extra options) <> "\n" <>
                           b "d3.select('" <> b (cssSelector options)
                           <> b "').datum(myData).call(chart);\n" <>
                           b "return chart;\n" <>
                           b "}\nnv.addGraph(graphFunction);\n"
                           where
                                 dumpMargins (Just m) = b "chart.margin(" <> (b $ encodeText m) <> b ");\n"
                                 dumpMargins Nothing = mempty

b :: Text -> B.Builder
b = B.fromLazyText

buildChartOptions :: ChartOptions -> B.Builder
buildChartOptions options = (generateOpts (b "chart") $ filterIrrelevant irrelevantFields opts)
                            <> maybe mempty (\cc -> b "chart.color(d3.scale." <> (b $ LT.toLower . LT.pack . show $ cc) <> b "().range());\n") (colorCategory options)
                            <> maybe mempty (\oc -> b "chart.scatter.onlyCircles" <> (b $ encodeText oc) <> b ");\n") (onlyCircles options)
  where
      irrelevantFields = ["xAxis", "yAxis", "margins", "resize", "cssSelector","colorCategory", "d3Extra"]
      opts = mkObject' options

filterIrrelevant :: [T.Text] -> Object -> Object
filterIrrelevant (x:xs) o = H.delete x $ filterIrrelevant xs o
filterIrrelevant _ o = o

generateOpts :: B.Builder -> Object -> B.Builder
generateOpts var o = L.foldl' (<>) mempty $ map snd $ H.toList $
                 H.mapWithKey gen (encodeMap $ filterEmpty o)
  where gen k v = var <> b "." <> B.fromText k
               <> b "("  <> b v <> b ");\n"

encodeMap :: Object -> H.HashMap T.Text Text
encodeMap o = H.map encodeText o

encodeText :: (ToJSON a) => a -> Text
encodeText = decodeUtf8 . encode

encodeText' :: (ToJSON a) => a -> Text
encodeText' = decodeUtf8 . encode . toJSON'

buildSeries :: [Series] -> B.Builder
buildSeries ss = B.fromLazyText $ decodeUtf8 $ encode $ toJSON'' ss

buildAxisOptions :: ChartOptions -> Text -> B.Builder
buildAxisOptions options chart = buildIfExists "x" (xAxis options) <>
                        case chart of "linePlusBarChart" -> buildIfExists "y1" (yAxis options) <>
                                                            buildIfExists "y2" (y2Axis options)
                                      "lineWithFocusChart" -> buildIfExists "y" (yAxis options) <>
                                                              buildIfExists "y2" (y2Axis options)
                                      _ -> buildIfExists "y" (yAxis options)

  where buildAxis :: Text -> Axis -> B.Builder
        buildAxis xy axis =
          (case encodeText (showAxis axis) of
              "null" -> mempty
              val -> b "chart.show" <> (b $ LT.toUpper xy)
                     <> b "Axis(" <> b val
                     <> b ");\n"
          )
          <> generateOpts (prefix xy)
          (filterIrrelevant irrelevantFields $ mkObject $ toJSON $ axis)
          <> maybe mempty
          (\tf -> prefix xy <> b ".tickFormat(" <> buildFormat tf <> b "'));\n")
          (tickFormat axis)
        irrelevantFields = ["displayed","tickFormat"]
        prefix xy = b "chart." <> b xy <> b "Axis"
        buildFormat :: TickFormat -> B.Builder
        buildFormat (TickFormat t) = b "d3.format('" <> b t <> "');\n"
        buildFormat (DateFormat t) = b "function(d) {\nreturn d3.time.format('"
                                     <> b t <> b "')(new Date(d))\n});\n"
        buildIfExists :: Text -> Maybe Axis -> B.Builder
        buildIfExists xy (Just ax) = buildAxis xy ax
        buildIfExists xy Nothing = mempty

swapKeys :: [(Text,Text)] -> H.HashMap Text v -> H.HashMap Text v
swapKeys (t:ts) hm = swapKey t $ swapKeys ts hm
  where swapKey (old,new) o = let val = o H.! old in (H.delete old) $ H.insert new val o
swapKeys _ hm = hm

-- | Returns only the fields which have non-empty values
changedFields :: Object -> Object -> Object
changedFields def new = H.filterWithKey changed new
                        where changed k v = (def H.! k) == v

-- testChartOptions :: ChartOptions -> T.Text
-- testChartOptions (toJSON' -> Object o) = B.toLazyText $ buildChartOptions o

-- testChartOptionsDef = testChartOptions defChartOptions
