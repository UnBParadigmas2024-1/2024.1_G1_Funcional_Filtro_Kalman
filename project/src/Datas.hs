{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Datas where

import Data.Csv as Cassava

data Parameters = Parameters{
    x_real :: Double,
    q :: Double,
    r1 :: Double,
    r2 :: Double,
    n :: Int
}

data Item =
  Item
    { itemPrediction :: Double
    , itemMeasure :: Double
    }
  deriving (Eq, Show)

type CsvRow = (Double, Double)

instance FromNamedRecord Item where
  parseNamedRecord m =
    Item
      <$> m .: "Tempo"
      <*> m .: "Valor"

instance ToNamedRecord Item where
  toNamedRecord Item{..} =
    Cassava.namedRecord
      [ "Tempo" .= itemPrediction
      , "Valor" .= itemMeasure
      ]


instance DefaultOrdered Item where
  headerOrder _ =
    Cassava.header
      [ "Tempo"
      , "Valor"
      ]
