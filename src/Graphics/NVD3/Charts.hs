{-# LANGUAGE OverloadedStrings #-}

module Graphics.NVD3.Charts where

import           Data.Text.Lazy         (Text)
import qualified Data.Text.Lazy.Builder as B

import           Graphics.NVD3.Types
import           Graphics.NVD3.Writer

line :: [Series] -> ChartOptions -> Text
line ss options = if null ss
                  then ""
                  else B.toLazyText $ buildJS "lineChart" ss options

scatter :: [Series] -> ChartOptions -> Text
scatter ss options = if null ss
                     then ""
                     else B.toLazyText $ buildJS "scatterChart" ss options

stackedArea :: [Series] -> ChartOptions -> Text
stackedArea ss options = if null ss
                         then ""
                         else B.toLazyText $ buildJS "stackedAreaChart" ss options

bar :: [Series] -> ChartOptions -> Text
bar ss options = if null ss
                 then ""
                 else B.toLazyText $ buildJS "discreteBarChart" ss options

multiBar :: [Series] -> ChartOptions -> Text
multiBar ss options = if null ss
                      then ""
                      else B.toLazyText $ buildJS "multiBarChart" ss options

multiBarH :: [Series] -> ChartOptions -> Text
multiBarH ss options = if null ss
                       then ""
                       else B.toLazyText $ buildJS "multiBarHorizontalChart" ss options

lineBar :: [Series] -> ChartOptions -> Text
lineBar ss options = if null ss
                     then ""
                     else B.toLazyText $ buildJS "linePlusBarChart" ss options

cumLine :: [Series] -> ChartOptions -> Text
cumLine ss options = if null ss
                     then ""
                     else B.toLazyText $ buildJS "cumulativeLineChart" ss options

lineFocus :: [Series] -> ChartOptions -> Text
lineFocus ss options = if null ss
                       then ""
                       else B.toLazyText $ buildJS "lineWithFocusChart" ss options

pie :: [Series] -> ChartOptions -> Text
pie ss options = if null ss
                 then ""
                 else B.toLazyText $ buildJS "pieChart" ss options

bullet :: [Series] -> ChartOptions -> Text
bullet ss options = if null ss
                    then ""
                    else B.toLazyText $ buildJS "bulletChart" ss options
