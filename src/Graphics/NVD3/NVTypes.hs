{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module Graphics.NVD3.NVTypes where

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
            , automatic  :: Bool
            , label      :: Text
            , tickFormat :: Text
            } deriving (Generic,Show)

instance ToJSON Axis

data ChartOptions = ChartOptions
                    { interactiveGuideline :: Bool
                    , transitionDuration   :: Int
                    , xAxis                :: Axis
                    , yAxis                :: Axis
                    } deriving (Generic,Show)

instance ToJSON ChartOptions

data Series = FloatSeries
              { values :: Vector Float
              , name   :: Text
              , color  :: Text
              }

test = ChartOptions {interactiveGuideline = True, transitionDuration = 350, xAxis = Axis {displayed = True, automatic = False, label = "sup", tickFormat = "bros"}, yAxis = Axis {displayed = True, automatic = False, label = "sup", tickFormat = "bros"}}

test2 = toJSON test

test3 :: Value -> Value
test3 (Object obj) = obj H.! "transitionDuration"
test3 _ = error "wooo"


