{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module CSVConverter
  ( Item(..)
  , encodeItems
  , encodeItemsToFile
  , convertItems
  , itemHeader
  )
  where

-- base
import Control.Exception (IOException)
import qualified Control.Exception as Exception
import qualified Data.Foldable as Foldable

-- bytestring
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString

-- cassava
import Data.Csv
  ( DefaultOrdered(headerOrder)
  , FromField(parseField)
  , FromNamedRecord(parseNamedRecord)
  , Header
  , ToField(toField)
  , ToNamedRecord(toNamedRecord)
  , (.:)
  , (.=)
  )
import qualified Data.Csv as Cassava

-- text
import Data.Text (Text)
import qualified Data.Text.Encoding as Text

-- vector
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Csv as Cassava
import Data.Int (Int)

-- Definição de dados
data Item =
  Item
    { itemPrediction :: Int
    , itemMeasure :: Int
    }
  deriving (Eq, Show)


-- Instâncias para conversão CSV
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

-- Dados de exemplo


itemHeader :: Header
itemHeader =
  Vector.fromList
    [ "Tempo"
    , "Valor"
    ]

-- Funções
catchShowIO:: IO a -> IO (Either String a)
catchShowIO action =
  fmap Right action
    `Exception.catch` handleIOException
  where
    handleIOException
      :: IOException
      -> IO (Either String a)
    handleIOException =
      return . Left . show

convertItems :: [Int] -> [Int] -> [Item]
convertItems = zipWith Item

encodeItems :: Vector Item -> ByteString
encodeItems = Cassava.encodeDefaultOrderedByName . Foldable.toList

encodeItemsToFile :: FilePath -> Vector Item -> IO ()
encodeItemsToFile filePath = ByteString.writeFile filePath . encodeItems