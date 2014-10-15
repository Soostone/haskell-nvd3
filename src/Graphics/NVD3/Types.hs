{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE ViewPatterns       #-}

module Graphics.NVD3.Types where

import           Data.Aeson
import           Data.Data
import qualified Data.HashMap.Strict        as H
import           Data.Monoid
import           Data.String
import qualified Data.Text                  as T
import           Data.Text.Lazy             (Text)
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Builder     as B
import qualified Data.Text.Lazy.Builder.Int as B
import           Data.Typeable
import qualified Data.Vector                as V
import qualified Data.Vector.Generic        as VG
import           Data.Vector.Unboxed        (Vector)
import qualified Data.Vector.Unboxed        as VU
import           GHC.Generics

data Axis = Axis
            { showAxis   :: Maybe Bool -- set Nothing for multi-bar horizontal
            , axisLabel  :: Maybe Text
            , tickFormat :: Maybe TickFormat
            } deriving (Generic,Show)

instance ToJSON Axis

defAxis :: Axis
defAxis = Axis
              { showAxis = Nothing
              , axisLabel = Just "Default Axis"
              , tickFormat = Nothing
              }

-- | DateFormat assumes that the corresponding data is in milliseconds since epoch
data TickFormat = TickFormat Text | DateFormat Text deriving (Show,Generic)

instance ToJSON TickFormat

data ChartOptions = ChartOptions
                    { cssSelector             :: Text
                    , clipEdge                :: Maybe Bool
                    , colorCategory           :: Maybe ColorCategory
                    , colors                  :: Maybe [Text]
                    , donut                   :: Maybe Bool
                    , donutRatio              :: Maybe Float
                    , groupSpacing            :: Maybe Float
                    , labelThreshold          :: Maybe Float
                    , labelType               :: Maybe LabelType
                    , margins                 :: Maybe Margins
                    , onlyCircles             :: Maybe Bool
                    , reduceXTicks            :: Maybe Bool
                    , resize                  :: Maybe Bool
                    , rightAlignYAxis         :: Maybe Bool
                    , rotateLabels            :: Maybe Float
                    , showControls            :: Maybe Bool
                    , showDistX               :: Maybe Bool
                    , showDistY               :: Maybe Bool
                    , showLabels              :: Maybe Bool
                    , showLegend              :: Maybe Bool
                    , showValues              :: Maybe Bool
                    , staggerLabels           :: Maybe Bool
                    , tooltips                :: Maybe Bool
                    , transitionDuration      :: Maybe Int
                    , useInteractiveGuideline :: Maybe Bool
                    , xAxis                   :: Maybe Axis
                    , yAxis                   :: Maybe Axis
                    , y2Axis                  :: Maybe Axis
                    , d3Extra                 :: Maybe Text
                    } deriving (Generic,Show)

instance ToJSON ChartOptions

defChartOptions :: ChartOptions
defChartOptions = ChartOptions
                    { cssSelector = "#chart svg"
                    , clipEdge = Nothing
                    , colors = Nothing
                    , colorCategory = Nothing
                    , donut = Nothing
                    , donutRatio = Nothing
                    , groupSpacing = Nothing
                    , labelThreshold = Nothing
                    , labelType = Nothing
                    , margins = Nothing
                    , onlyCircles = Nothing
                    , reduceXTicks = Nothing
                    , resize = Just True
                    , rightAlignYAxis = Nothing
                    , rotateLabels = Nothing
                    , showControls = Nothing
                    , showDistX = Nothing
                    , showDistY = Nothing
                    , showLabels = Nothing
                    , showLegend = Nothing
                    , showValues = Nothing
                    , staggerLabels = Nothing
                    , tooltips = Nothing
                    , transitionDuration = Just 350
                    , useInteractiveGuideline = Nothing
                    , xAxis = Just defAxis
                    , yAxis = Just defAxis
                    , y2Axis = Nothing
                    , d3Extra = Nothing
                    }

data LabelType = LabelKey | LabelValue | LabelPercent deriving (Show,Eq)

instance ToJSON LabelType where
  toJSON LabelKey = "key"
  toJSON LabelValue = "value"
  toJSON LabelPercent = "percent"

data Series = Series
              { values :: V.Vector Values
              , key    :: Text -- don't name it null
              , color  :: Maybe Text
              , area   :: Maybe Bool
              , shape  :: Maybe Text
              , size   :: Maybe Float
              , type'  :: Maybe Text
              , bar'   :: Maybe Bool
              }
            | PieSeries (V.Vector PieVal)
            | BulletSeries BulletVal
            deriving (Show,Eq)

instance ToJSON Series where
  toJSON Series {..} =
    object [ "values" .= values
           , "key"    .= key
           , "color"  .= color
           , "area"   .= area
           , "shape"  .= shape
           , "size"   .= size
           , "type"   .= type'
           , "bar"    .= bar'
           ]
  toJSON (PieSeries vec) = Array $ V.map toJSON vec
  toJSON (BulletSeries bval) = toJSON bval

defSeries :: Series
defSeries = Series
            { values = mkNumVals (V.enumFromN 1 20)
                       (V.map (*2) $ V.enumFromN 1 20)
            , key = "Default Series"
            , color = Nothing
            , area = Nothing
            , shape = Nothing
            , size = Nothing
            , type' = Nothing
            , bar' = Nothing
            }

data PieVal = PieVal
               { pieLabel :: Text
               , pieVal   :: Float
               } deriving (Show,Eq)

instance ToJSON PieVal where
  toJSON PieVal {..} =
    object [ "x" .= pieLabel
           , "y" .= pieVal
           ]

data Values = NumVal
              { numX :: Float
              , numY :: Float
              }
            | DiscreteVal
              { dvLabel :: Text
              , dvY     :: Float
              } deriving (Show,Eq)

instance ToJSON Values where
  toJSON NumVal {..}  =
    object [ "x" .= numX
           , "y" .= numY
           ]
  toJSON DiscreteVal {..} =
    object [ "x" .= dvLabel
           , "y" .= dvY
           ]

data BulletVal = BulletVal
              { bTitle    :: Text
              , bSubtitle :: Maybe Text
              , bRanges   :: [Float]
              , bMeasures :: [Float]
              , bMarkers  :: [Float]
              } deriving (Show,Eq)

instance ToJSON BulletVal where
  toJSON BulletVal {..} =
    object [ "title" .= bTitle
           , "subtitle" .= bSubtitle
           , "ranges" .= bRanges
           , "measures" .= bMeasures
           , "markers" .= bMarkers
           ]

mkNumVals :: V.Vector Float -> V.Vector Float -> V.Vector Values
mkNumVals v1 v2 = V.map (uncurry NumVal) (V.zip v1 v2)

mkDiscVals :: V.Vector Text -> V.Vector Float -> V.Vector Values
mkDiscVals v1 v2 = V.map (uncurry DiscreteVal) (V.zip v1 v2)

discToPie :: V.Vector Values -> V.Vector PieVal
discToPie vec = V.map (\val -> PieVal (dvLabel val) (dvY val)) vec

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

instance ToJSON ColorCategory where
  toJSON = String . T.toLower . T.pack . show

filterEmpty :: Object -> Object
filterEmpty o = H.filter (/= Null) o

-- | Filter out the Null values if Value is an Object
toJSON' :: (ToJSON a) => a -> Value
toJSON' (toJSON -> Object o) = Object (filterEmpty o)
toJSON' a = toJSON a

-- | Filter out the Null values if Value is an Array of Objects
toJSON'' :: (ToJSON a) => [a] -> Value
toJSON'' xs = Array $ V.map (Object . filterEmpty . mkObject) vec
  where vec = mkArray xs

mkObject :: Value -> Object
mkObject (Object o) = o

mkObject' :: (ToJSON a) => a -> Object
mkObject' (toJSON' -> Object o) = o

mkArray :: (ToJSON a) => a -> Array
mkArray (toJSON -> Array a) = a
