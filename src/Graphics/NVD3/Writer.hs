{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Graphics.NVD3.Writer where

import           Data.Aeson
import qualified Data.ByteString.Char8      as B
import           Data.Data
import qualified Data.HashMap.Strict        as H
import           Data.List                  as L
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
                           -- buildAxisOptions optionsObj <>
                           (if (resize options == Just True)
                            then b "nv.utils.windowResize(function() { chart.update() });\n"
                            else mempty) <>
                           b "var myData = " <> dumpSeries ss
                           <> b ";\n" <>
                           b "d3.select('" <> b (cssSelector options)
                           <> b "').datum(myData).call(chart);\n" <>
                           b "return chart;\n" <>
                           b "}\nnv.addGraph(graphFunction);\n"
                           where mkObject (toJSON -> Object o) = o
                                 mkObject _ = error "Something that should not happen happened"
                                 optionsObj = mkObject options
                                 dumpMargins (Just m) = b "chart.margin(" <> (B.fromLazyText $
                                                        decodeUtf8 $ encode m) <> b ");\n"
                                 dumpMargins Nothing = mempty
                                 dumpSeries (toJSON -> Array arr) = B.fromLazyText $ decodeUtf8 $
                                                                  encode $ V.map adjust arr
                                 adjust (Object o) = let val = o H.! "type'" in
                                    filterEmpty $ (H.delete "type'") $ (H.insert "type" val) o

tab :: Int -> B.Builder
tab n = B.fromText $ T.replicate (4 * n) " "

b = B.fromText

buildChartOptions :: Object -> B.Builder
buildChartOptions options = L.foldl1' (<>) $  map snd $ H.toList $ H.mapWithKey generate (filterEmpty $ encodeMap $ filterIrrelevant options)
  where generate k v = B.fromText "chart." <> B.fromText k
                       <> B.fromText "("  <> B.fromText v <> B.fromText ");\n"
        filterIrrelevant :: Object -> Object
        filterIrrelevant o = L.foldl1' H.intersection $ map ((flip H.delete) o) ["xAxis", "yAxis", "margins", "resize", "cssSelector"]
        encodeMap o = H.map (LT.toStrict . decodeUtf8 . encode) o

filterEmpty :: (Eq v, IsString v) => H.HashMap k v -> H.HashMap k v
filterEmpty o = H.filter (not . (== "null")) o

buildAxisOptions :: Object -> B.Builder
buildAxisOptions = undefined

-- buildChartOptions options = optionsMap' H.tra
--   where optionsMap = undefined
--         optionsMap' = undefined


-- | Returns only the fields which have non-empty values
changedFields :: Object -> Object -> Object
changedFields def new = H.filterWithKey changed new
                        where changed k v = (def H.! k) == v

testChartOptions :: ChartOptions -> LT.Text
testChartOptions (toJSON -> Object o) = B.toLazyText $ buildChartOptions o

testChartOptionsDef = testChartOptions defChartOptions
