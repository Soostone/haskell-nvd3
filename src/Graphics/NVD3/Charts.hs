{-# LANGUAGE OverloadedStrings #-}

module Graphics.NVD3.Charts where

import           Data.Aeson
import           Data.Text.Lazy             (Text)
import qualified Data.Text.Lazy             as T
import qualified Data.Text.Lazy.Builder     as B
import qualified Data.Text.Lazy.Builder.Int as B
import           Data.Vector.Unboxed        (Vector)
import qualified Data.Vector.Unboxed        as V

import           Graphics.NVD3.Types
import           Graphics.NVD3.Writer


lineChart :: [Series] -> ChartOptions -> B.Builder
lineChart ss options = if null ss
                       then ""
                       else let a = buildJS "lineChart" ss options in a

vals :: Vector Values
vals = V.unfoldr (\n -> )

lineCOpts = defChartOptions{xAxis = Just defAxis{ axisLabel = Just "My X Axis"}, yAxis = Just defAxis{ axisLabel = Just "My X Axis"}}

ss = [defSeries {values = NumVals vals vals, key = "First Series", color = Just "#ff7f0e"}
     , defSeries {values = NumVals vals vals2, key = "Squared Series", color = Just "#2ca02c"}, defSeries {values = NumVals vals vals3, key = "Cubed Series", color = Just "#7777ff"}]

testLine = T.unpack $ B.toLazyText $ buildJS "lineChart" ss lineCOpts
