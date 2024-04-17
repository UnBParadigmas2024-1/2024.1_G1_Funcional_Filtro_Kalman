module Main (main) where

import Utils
import Modules
import Lib
import Data.Text (pack)
import qualified Data.Vector as Vector

main :: IO ()
main = do
    let tempo = [0, 1, 2, 3, 4]
    let valor = [4, 3, 2, 1, 0]

    let elementos = convertItems tempo valor
    let items = Vector.fromList elementos
    let filePath = "kalman_filter.csv"

    encodeItemsToFile filePath items
    let filePath = "src/shunt.csv"
    -- Retorna os valores agrupados em uma lista de listas, com os valores das colunas agrupados em 32 litas de 1000 valores cada
    (list1, list2) <- parserCsvGrouped filePath

    putStrLn "List 2:"
    mapM_ print list1

    putStrLn "List 2:"
    mapM_ print list2

    -- Retorna uma lista de listas, onde cada lista menor contém o tempo a medição correspondende
    list1 <- parserCsv filePath
    print list1

    -- Retorna duas listas contendo os valores da primeira e segunda colunas do CSV, respectivamente.
    (list1, list2) <- parserCsvNotGrouped filePath
    print list1
    print list2

    -- Retorna uma lista contendo os valores da primeira e segunda colunas do CSV, respectivamente.
    list1 <- parserCsvNotGroupedOne filePath
    print list1
    

