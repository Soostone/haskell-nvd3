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
import           Data.Text                  (Text)
import qualified Data.Text.Lazy             as T
import qualified Data.Text.Lazy.Builder     as B
import qualified Data.Text.Lazy.Builder.Int as B
import           Data.Typeable
import qualified Data.Vector                as V
import qualified Data.Vector.Generic        as VG
import           Data.Vector.Unboxed        (Vector)
import qualified Data.Vector.Unboxed        as VU
import           GHC.Generics

data Axis = Axis
            { displayed  :: Bool
            , axisLabel  :: Maybe Text
            , tickFormat :: Maybe Text
            } deriving (Generic,Show)

instance ToJSON Axis

defAxis :: Axis
defAxis = Axis
              { displayed = True
              , axisLabel = Just "Default Axis"
              , tickFormat = Nothing
              }

data ChartOptions = ChartOptions
                    { useInteractiveGuideline :: Bool
                    , transitionDuration      :: Int
                    , cssSelector             :: Text
                    , colorCategory           :: Maybe ColorCategory
                    , colorList               :: Maybe [Text]
                    , stacked                 :: Maybe Bool
                    , resize                  :: Maybe Bool
                    , showLegend              :: Maybe Bool
                    , xAxis                   :: Maybe Axis
                    , yAxis                   :: Maybe Axis
                    , margins                 :: Maybe Margins
                    , d3Extra                 :: Maybe Text
                    } deriving (Generic,Show)

instance ToJSON ChartOptions

defChartOptions :: ChartOptions
defChartOptions = ChartOptions
                        { useInteractiveGuideline = True
                        , transitionDuration = 360
                        , cssSelector = "#chart svg"
                        , colorCategory = Nothing
                        , colorList = Nothing
                        , stacked = Nothing
                        , resize = Just True
                        , showLegend = Just True
                        , xAxis = Just defAxis
                        , yAxis = Just defAxis
                        , margins = Just defMargins
                        , d3Extra = Nothing
                        }

data Series = Series
              { values :: V.Vector Values
              , key    :: Text -- don't name it null
              , color  :: Maybe Text
              , area   :: Maybe Bool
              , shape  :: Maybe Text
              , size   :: Maybe Int
              , type'  :: Maybe Text
              , bar    :: Maybe Bool
              } deriving (Generic)

instance ToJSON Series where
  toJSON Series {..} =
    object [ "values" .= (toJSON values)
           , "key"    .= (toJSON key)
           , "color"  .= (toJSON color)
           , "area"   .= (toJSON area)
           , "shape"  .= (toJSON shape)
           , "size"   .= (toJSON size)
           , "type"   .= (toJSON type')
           , "bar"    .= (toJSON bar)
           ]

data Values = NumVals
              { numX :: Float
              , numY :: Float
              } deriving (Generic,Show) -- DateVals (Vector Date) (Vector Float)

instance ToJSON Values where
  toJSON NumVals {..}  =
    object [ "x" .= toJSON numX
           , "y" .= toJSON numY
           ]

defSeries :: Series
defSeries = Series
            { values = V.unfoldr (\n -> if n==21 then Nothing else Just (NumVals {numX = n, numY = n}, n+1)) 1
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

-- test = ChartOptions {interactiveGuideline = True, transitionDuration = 350, xAxis = Axis {displayed = True, label = "sup", tickFormat = "bros"}, yAxis = Axis {displayed = True, label = "sup", tickFormat = "bros"}, d3Extra = []}

-- test2 = toJSON test

-- test3 :: Value -> Value
-- test3 (Object obj123) = obj123 H.! "transitionDuration"
