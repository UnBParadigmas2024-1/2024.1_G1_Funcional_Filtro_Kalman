module Parser
    ( parserCsv
    ) where

import Data.Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V


parserCsv :: IO ()
parserCsv = do
    csvData <- BL.readFile "src/shunt.csv"
    case decode NoHeader csvData of
        Left err -> putStrLn err
        Right v -> V.forM_ v $ \(x, y) ->
            putStrLn $ show (x :: Float) ++ " // " ++ show (y :: Float)