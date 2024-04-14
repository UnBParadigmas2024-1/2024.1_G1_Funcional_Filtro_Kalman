module Parser
    ( parserCsv
    ) where

import Data.Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Control.Monad (forM_)

groupAt :: Int -> [a] -> [[a]]
groupAt n = go
  where go [] = []
        go xs = ys : go zs
            where ~(ys, zs) = splitAt n xs

parserCsv :: IO ([[Float]], [[Float]])
parserCsv = do
    csvData <- BL.readFile "src/shunt.csv"
    case decode NoHeader csvData of
        Left err -> error err
        --Right v -> V.forM_ v $ \(x, y) ->
        --    putStrLn $ show (y :: Float) ++ " // " ++ show (y :: Float)
        ---Right v -> pure (groupAt 1000 (V.toList (fmap fst v)), groupAt 1000 (V.toList (fmap snd v))) 
        Right v -> do
            let list1 = groupAt 1000 (V.toList (fmap fst v))
                list2 = groupAt 1000 (V.toList (fmap snd v))
            forM_ list1 $ \row ->
                putStrLn $ "List 1: " ++ show row
            --forM_ list2 $ \row ->
            --    putStrLn $ "List 2: " ++ show row
            pure (list1, list2)