{-# LANGUAGE OverloadedStrings #-}

module Graphics.NVD3.Charts where

import           Data.Aeson
import           Data.Text.Lazy         (Text)
import qualified Data.Text.Lazy         as T
import qualified Data.Text.Lazy.Builder as B
import           Data.Vector            (Vector)
import qualified Data.Vector            as V

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

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--- tests

s1 = defSeries
s2 = defSeries {values = mkNumVals (V.enumFromN 1 20) (V.map (*3) $ V.enumFromN 1 20), key = "Second Series"}

d1 = defSeries {values = mkDiscVals (V.fromList ["Group 1", "Group 2", "Group 3", "Group 4", "Group 5"]) (V.fromList [3,7,2,4,8])}
d2 = defSeries {values = mkDiscVals (V.fromList ["Group 1", "Group 2", "Group 3", "Group 4", "Group 5"]) (V.fromList [8,3,6,3,4]), key = "Second Series"}

p1 = PieSeries (discToPie $ values d1)

-- line chart

testLine = line [s1, s2] defChartOptions {useInteractiveGuideline = Just True, cssSelector = "#lineChart svg"}

-- scatter plot

testScatter = scatter [s1 {size = Just 0.45}, s2 {size = Just 0.9}] defChartOptions {showDistX = Just True, showDistY = Just True, cssSelector = "#scatterChart svg"}

-- stacked area

testStackedArea = stackedArea [s1, s2] defChartOptions {useInteractiveGuideline = Just True, cssSelector = "#stackedAreaChart svg", colorCategory = Just Category10}

-- bar chart

testBar = bar [d1] defChartOptions {showValues = Just True, cssSelector = "#barChart svg"}

-- multi-bar chart

testMultiBar = multiBar [d1, d2] defChartOptions {reduceXTicks = Just True, cssSelector = "#multiBarChart svg", groupSpacing = Just 0.1, colorCategory = Just Category10}

-- horizontal multi-bar chart

testMultiBarH = multiBarH [d1, d2] defChartOptions {cssSelector = "#multiBarHChart svg", showValues = Just True, margins = Just defMargins, colorCategory = Just Category10}

-- line plus bar chart

testLineBar = lineBar [s1{bar' = Just True},s2] defChartOptions {cssSelector = "#lineBarChart svg", margins = Just defMargins, transitionDuration = Nothing}

-- cumulative line chart

testCumLine = cumLine [s1,s2] defChartOptions {useInteractiveGuideline = Just True, cssSelector = "#cumLineChart svg"}

-- line chart with focus

testLineFocus = lineFocus [s1,s2] defChartOptions {cssSelector = "#lineFocusChart svg", xAxis = Just defAxis {axisLabel = Nothing}, colorCategory = Just Category10}

-- pie chart

testPieChart = pie [p1] defChartOptions {cssSelector = "#pieChart svg", showLabels = Just True, labelType = Just LabelPercent, xAxis = Nothing, yAxis = Nothing, transitionDuration = Nothing}

testDonutChart = pie [p1] defChartOptions {cssSelector = "#donutChart svg", showLabels = Just True, labelType = Just LabelPercent, xAxis = Nothing, yAxis = Nothing, transitionDuration = Nothing, donut = Just True, donutRatio = Just 0.35}

-- bullet chart

testBullet = bullet [BulletSeries (BulletVal "Revenues" Nothing [150,225,300] [220] [250])] defChartOptions {cssSelector = "#bulletChart svg", xAxis = Nothing, yAxis = Nothing, transitionDuration = Nothing}


