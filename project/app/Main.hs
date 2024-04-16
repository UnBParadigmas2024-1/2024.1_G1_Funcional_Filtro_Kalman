module Main (main) where

import Lib
import CSVConverter
import Data.Text (pack)
import qualified Data.Vector as Vector
import Parser

main :: IO ()
main = do
    let tempo = [0, 1, 2, 3, 4]
    let valor = [4, 3, 2, 1, 0]

    let elementos = convertItems tempo valor
    let items = Vector.fromList elementos
    let filePath = "kalman_filter.csv"

    encodeItemsToFile filePath items

    let filePath = "src/shunt.csv"
    (list1, list2) <- parserCsv filePath

    mapM_ print list1

    putStrLn "List 2:"
    mapM_ print list2

    {--
    let filePath = "src/shunt.csv"
    list1 <- parserCsv filePath
    print list1
    --}

