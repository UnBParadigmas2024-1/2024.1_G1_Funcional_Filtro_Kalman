module Main (main) where

import Lib
import CSVConverter
import Data.Text (pack)
import qualified Data.Vector as Vector

main :: IO ()
main = do
    let items = Vector.fromList [Item { itemPrediction = 0, itemMeasure = 1 }, Item { itemPrediction = 1, itemMeasure = 0 }]
    let filePath = "kalman_filter.csv"

    encodeItemsToFile filePath items

