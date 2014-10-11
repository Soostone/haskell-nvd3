{-# LANGUAGE OverloadedStrings #-}

module Graphics.NVD3.Line where

import           Data.Aeson
import           Data.Text.Lazy             (Text)
import qualified Data.Text.Lazy             as T
import qualified Data.Text.Lazy.Builder     as B
import qualified Data.Text.Lazy.Builder.Int as B
import           Data.Vector.Unboxed        (Vector)
import qualified Data.Vector.Unboxed        as V

import           Graphics.NVD3.NVTypes
import           Graphics.NVD3.Writer

-- import qualified Data.Text.Lazy.Builder.RealFloat as F

-- data NVD3Options = NVD3Options
--                    { type :: String

-- , }

lineChart :: [Series] -> ChartOptions -> B.Builder
lineChart ss options = if null ss
                       then ""
                       else let a = buildJS "lineChart" ss options in a

-- put general options to ChartOptions and chart-specific functionality into the series themselves
-- can't import series to Writer.hs due to cyclic imports so send the JSON dump of the series - consider dumping the data into a separate JSON in the future
