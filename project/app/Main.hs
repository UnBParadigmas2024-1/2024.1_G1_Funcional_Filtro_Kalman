module Main (main) where

import Lib
import CSVConverter
import Data.Text (pack)
import qualified Data.Vector as Vector

main :: IO ()
main = do
    let items = Vector.fromList [Item { itemName = pack "Item 1", itemLink = pack "link1", itemType = Country }, Item { itemName = pack "Item 2", itemLink = pack "link2", itemType = Other (pack "Other Type") }]
    let filePath = "items.csv"

    encodeItemsToFile filePath items

