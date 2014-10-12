{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module Graphics.NVD3.Types where

import           Data.Aeson
import           Data.Data
import qualified Data.HashMap.Strict        as H
import           Data.Monoid
import           Data.Text                  (Text)
import qualified Data.Text.Lazy             as T
import qualified Data.Text.Lazy.Builder     as B
import qualified Data.Text.Lazy.Builder.Int as B
import           Data.Typeable
import           Data.Vector.Unboxed        (Vector)
import qualified Data.Vector.Unboxed        as V
import           GHC.Generics

data Axis = Axis
            { displayed  :: Bool
            , axisLabel  :: Maybe Text
            , tickFormat :: Maybe Text
            , date       :: Maybe Bool
            } deriving (Generic,Show)

instance ToJSON Axis

defAxis :: Axis
defAxis = Axis
              { displayed = True
              , axisLabel = Just "Default Axis"
              , tickFormat = Nothing
              , date = Nothing
              }

-- | Field names should correspond directly to Javascript parameters
data ChartOptions = ChartOptions
                    { useInteractiveGuideline :: Bool
                    , transitionDuration      :: Int
                    , colorCategory           :: Maybe ColorCategory
                    , colorList               :: Maybe [Text]
                    , stacked                 :: Maybe Bool
                    , resize                  :: Maybe Bool
                    , showLegend              :: Maybe Bool
                    , showLabels              :: Maybe Bool
                    , dateFormat              :: Maybe Text
                    , xAxis                   :: Maybe Axis
                    , yAxis                   :: Maybe Axis
                    , margins                 :: Maybe Margins
                    , cssSelector             :: Text
                    , d3Extra                 :: Maybe Text
                    } deriving (Generic,Show)

instance ToJSON ChartOptions

defChartOptions :: ChartOptions
defChartOptions = ChartOptions
                        { useInteractiveGuideline = True
                        , transitionDuration = 360
                        , colorCategory = Nothing
                        , colorList = Nothing
                        , stacked = Nothing
                        , resize = Just True
                        , showLegend = Just True
                        , showLabels = Just True
                        , dateFormat = Just "%x"
                        , xAxis = Just defAxis
                        , yAxis = Just defAxis
                        , margins = Just defMargins
                        , cssSelector = "#chart svg"
                        , d3Extra = Nothing
                        }

data Series = Series
              { values :: Vector (Float,Float)
              , key    :: Text
              , color  :: Maybe Text
              , area   :: Maybe Bool
              , shape  :: Maybe Text
              , size   :: Maybe Int
              , type'  :: Maybe Text
              , bar    :: Maybe Bool
              } deriving (Generic,Show)

instance ToJSON Series

defSeries :: Series
defSeries = Series
            { values = V.zip (V.fromList [1..10]) (V.fromList [1..10])
            , key = "Default Series"
            , color = Nothing
            , area = Nothing
            , shape = Nothing
            , size = Nothing
            , type' = Nothing
            , bar = Nothing
            }

data Margins = Margins
               { left   :: Int
               , right  :: Int
               , top    :: Int
               , bottom :: Int
               } deriving (Generic,Show)

instance ToJSON Margins

defMargins :: Margins
defMargins = Margins { left = 60, right = 60, top = 30, bottom = 20}

data ColorCategory = Category10 | Category20 | Category20B | Category20C deriving (Generic,Show)

instance ToJSON ColorCategory

-- test = ChartOptions {interactiveGuideline = True, transitionDuration = 350, xAxis = Axis {displayed = True, label = "sup", tickFormat = "bros"}, yAxis = Axis {displayed = True, label = "sup", tickFormat = "bros"}, d3Extra = []}

-- test2 = toJSON test

-- test3 :: Value -> Value
-- test3 (Object obj123) = obj123 H.! "transitionDuration"
