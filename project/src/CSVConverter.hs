{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module CSVConverter
  ( Item(..)
  , ItemType(..)
  , encodeItems
  , encodeItemsToFile
  , filterCountryItems
  , itemHeader
  , japanItem
  , japanRecord
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

-- Definição de dados
data Item =
  Item
    { itemName :: Text
    , itemLink :: Text
    , itemType :: ItemType
    }
  deriving (Eq, Show)

data ItemType
  = Country
  | Other Text
  deriving (Eq, Show)

-- Instâncias para conversão CSV
instance FromNamedRecord Item where
  parseNamedRecord m =
    Item
      <$> m .: "Item"
      <*> m .: "Link"
      <*> m .: "Type"

instance ToNamedRecord Item where
  toNamedRecord Item{..} =
    Cassava.namedRecord
      [ "Item" .= itemName
      , "Link" .= itemLink
      , "Type" .= itemType
      ]

instance FromField ItemType where
  parseField "International Country" =
    pure Country

  parseField otherType =
    Other <$> parseField otherType

instance ToField ItemType where
  toField Country =
    "International Country"

  toField (Other otherType) =
    toField otherType

instance DefaultOrdered Item where
  headerOrder _ =
    Cassava.header
      [ "Item"
      , "Link"
      , "Type"
      ]

-- Dados de exemplo
japanItem :: Item
japanItem =
  Item
    { itemName = "Japan"
    , itemLink = "http://www.data.go.jp/"
    , itemType = Country
    }

japanRecord :: ByteString
japanRecord =
  "Japan,http://www.data.go.jp/,International Country"

itemHeader :: Header
itemHeader =
  Vector.fromList
    [ "Item"
    , "Link"
    , "Type"
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

filterCountryItems :: Vector Item -> Vector Item
filterCountryItems = Vector.filter isCountryItem

isCountryItem:: Item -> Bool
isCountryItem = (==) Country . itemType

encodeItems :: Vector Item -> ByteString
encodeItems = Cassava.encodeDefaultOrderedByName . Foldable.toList

encodeItemsToFile :: FilePath -> Vector Item -> IO ()
encodeItemsToFile filePath = ByteString.writeFile filePath . encodeItems