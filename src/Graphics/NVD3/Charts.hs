{-# LANGUAGE OverloadedStrings #-}

module Graphics.NVD3.Charts where

import           Data.Aeson
import           Data.Text.Lazy             (Text)
import qualified Data.Text.Lazy             as T
import qualified Data.Text.Lazy.Builder     as B
import qualified Data.Text.Lazy.Builder.Int as B
import           Data.Vector        (Vector)
import qualified Data.Vector        as V

import           Graphics.NVD3.Types
import           Graphics.NVD3.Writer


lineChart :: [Series] -> ChartOptions -> B.Builder
lineChart ss options = if null ss
                       then ""
                       else let a = buildJS "lineChart" ss options in a

vals :: Vector Values
vals = V.unfoldr (\n -> if n==21 then Nothing else Just (NumVals {numX = n, numY = n}, n+1)) 1

vals2 :: Vector Values
vals2 = V.unfoldr (\n -> if n==21 then Nothing else Just (NumVals {numX = n, numY = n*2}, n+1)) 1

vals3 :: Vector Values
vals3 = V.unfoldr (\n -> if n==21 then Nothing else Just (NumVals {numX = n, numY = n*3+n}, n+1)) 1

lineCOpts :: ChartOptions
lineCOpts = defChartOptions{xAxis = Just defAxis{ axisLabel = Just "My X Axis"}, yAxis = Just defAxis{ axisLabel = Just "My X Axis"}}

ss :: [Series]
ss = [defSeries {values = vals, key = "First Series", color = Just "#ff7f0e"}
     , defSeries {values = vals2, key = "Squared Series", color = Just "#2ca02c"}, defSeries {values = vals3, key = "Cubed Series", color = Just "#7777ff"}]

testLine = T.unpack $ B.toLazyText $ buildJS "lineChart" ss lineCOpts
