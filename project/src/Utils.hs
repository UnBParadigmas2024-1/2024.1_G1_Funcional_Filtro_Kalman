{-# LANGUAGE OverloadedStrings #-}

module Utils where
import Datas
import System.Random

import Control.Exception (IOException)
import qualified Control.Exception as Exception
import qualified Data.Foldable as Foldable

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString

import Data.Text (Text)
import qualified Data.Text.Encoding as Text

import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Csv as Cassava

import Data.Int (Int)
import Data.Csv
import Data.Csv
  ( Header )

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
import Control.Monad (void)

itemHeader :: Header
itemHeader =
  Vector.fromList
    [ "Tempo"
    , "Valor"
    ]

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

groupAt :: Int -> [a] -> [[a]]
groupAt n = go
  where go [] = []
        go xs = ys : go zs
            where ~(ys, zs) = splitAt n xs


parserCsvGrouped :: FilePath -> IO ([[Double]], [[Double]])
parserCsvGrouped filePath = do
    csvData <- ByteString.readFile filePath
    case decode NoHeader csvData of
        Left err -> error err
        Right v ->
            let xs = Vector.toList (Vector.map fst v)
                ys = Vector.toList (Vector.map snd v)
                groupedXs = groupAt 1000 xs
                groupedYs = groupAt 1000 ys
            in return (groupedXs, groupedYs)

parserCsv :: FilePath -> IO [[Double]]
parserCsv filePath = do
    csvData <- ByteString.readFile filePath
    case decode NoHeader csvData :: Either String (Vector.Vector CsvRow) of
        Left err -> error err
        Right v -> return $ map (\(x, y) -> [x, y]) (Vector.toList v)


parserCsvNotGrouped :: FilePath -> IO ([Double], [Double])
parserCsvNotGrouped filePath = do
    csvData <- ByteString.readFile filePath
    case decode NoHeader csvData :: Either String (Vector.Vector CsvRow) of
        Left err -> error err
        Right v -> do
            let (xs, ys) = Vector.foldr' (\(x, y) (accX, accY) -> (x : accX, y : accY)) ([], []) v
            return (xs, ys)


parserCsvNotGroupedOne :: FilePath -> IO [[Double]]
parserCsvNotGroupedOne filePath = do
    csvData <- ByteString.readFile filePath
    case decode NoHeader csvData :: Either String (Vector.Vector CsvRow) of
        Left err -> error err
        Right v -> do
            let (xs, ys) = Vector.foldr' (\(x, y) (accX, accY) -> (x : accX, y : accY)) ([], []) v
            return [reverse xs, reverse ys]

convertItems :: [Double] -> [Double] -> [Item]
convertItems = zipWith Item

encodeItems :: Vector Item -> ByteString
encodeItems = Cassava.encodeDefaultOrderedByName . Foldable.toList

encodeItemsToFile :: FilePath -> Vector Item -> IO ()
encodeItemsToFile filePath = ByteString.writeFile filePath . encodeItems

parseToInt :: String -> Int
parseToInt str = read str

parseToDouble :: String -> Double
parseToDouble str = read str

initialization :: Int -> (Double, Double, Double, Double, Int)
initialization _ = (x_real, r1, r2, q, n)
  where
    x_real = 10
    r1 = 0.04
    r2 = 0.09
    q = 0.01
    n = 1000

generateMeasurements :: Double -> Double -> Int -> IO [Double]
generateMeasurements x_real r n = do
  gen <- newStdGen
  let randomValues = take n $ randomRs (-r, r) gen
  return $ map (+ x_real) randomValues

transposeMatrix :: [[Double]] -> [[Double]]
transposeMatrix ([]:_) = []
transposeMatrix x = (map head x) : transposeMatrix (map tail x)

addMatrices :: [[Double]] -> [[Double]] -> [[Double]]
addMatrices [] [] = []
addMatrices (x:xs) (y:ys) = zipWith (+) x y : addMatrices xs ys

subMatrices :: [[Double]] -> [[Double]] -> [[Double]]
subMatrices [] [] = []
subMatrices (x:xs) (y:ys) = zipWith (-) x y : subMatrices xs ys

multiplyMatrices :: [[Double]] -> [[Double]] -> [[Double]]
multiplyMatrices m1 m2 = [[sum $ zipWith (*) r1 c2 | c2 <- transposeMatrix m2] | r1 <- m1]

cofactorMatrix :: Int -> [[Double]] -> [[Double]]
cofactorMatrix n m = [ [(-1)^(i+j) * determinantMatrix (minorMatrix i j m) | j <- [0..(n-1)]] | i <- [0..(n-1)] ]

determinantMatrix :: [[Double]] -> Double
determinantMatrix [[x]] = x
determinantMatrix m = sum [ (-1)^i * (head m)!!i * determinantMatrix (minorMatrix 0 i m) | i <- [0..(n-1)] ]
  where n = length m

minorMatrix :: Int -> Int -> [[Double]] -> [[Double]]
minorMatrix i j m = removeAtIndex j (map (removeAtIndex i) (transposeMatrix m))

removeAtIndex :: Int -> [a] -> [a]
removeAtIndex n xs = take n xs ++ drop (n+1) xs

inverseMatrix :: [[Double]] -> [[Double]]
inverseMatrix m
  | n == 1 = [[1 / (head (head m))]]
  | determinant == 0 = error "A matriz não tem inversa."
  | otherwise = map (map (/ determinant)) (transposeMatrix cofactor)
  where
    n = length m
    determinant = determinantMatrix m
    cofactor = cofactorMatrix n m 

multiplyMatrixByConstant :: Double -> [[Double]] -> [[Double]]
multiplyMatrixByConstant c mat = map (map (* c)) mat

extractDouble :: [[Double]] -> Double
extractDouble [[x]] = x
extractDouble _ = error "Lista não contém um único elemento"

kalmanFilter :: [Double] -> [Double] -> Double -> [[Double]] -> [[Double]] -> Double -> Double -> IO [Double]
kalmanFilter [] [] _ _ _ _ _ = return []
kalmanFilter (z1:zs1) (z2:zs2) q r h x0 p0 = do
  let
      p=p0+q
      z = [[z1],[z2]]
      k = multiplyMatrixByConstant p (multiplyMatrices (transposeMatrix h) (inverseMatrix (addMatrices (multiplyMatrices (multiplyMatrixByConstant p h) (transposeMatrix h)) r)))
      x_new= x0 + extractDouble (multiplyMatrices k (subMatrices z (multiplyMatrixByConstant x0 h)))
      p_new = (1-extractDouble (multiplyMatrices k h)) * p
  rest <- kalmanFilter zs1 zs2 q r h x_new p_new
  let result = x_new : rest
  return result

mediaAgrupada :: [[Double]] -> IO (Double)
mediaAgrupada [[]] = return 0.0
mediaAgrupada dados = do 
      return (sum (map sum dados) / (fromIntegral (length dados) * fromIntegral (length (head dados))))

mediaAritmetica :: [Double] -> Double
mediaAritmetica [] = 0.0
mediaAritmetica conjunto = sum conjunto / fromIntegral (length conjunto)

desvioPadraoReal :: [[Double]] -> IO (Double)
desvioPadraoReal [[]] = return 0.0
desvioPadraoReal dados = do
    return (let desvios = map desvioPadraoAgrupamento dados 
        in sum desvios / fromIntegral (length dados))

desvioPadraoAgrupamento :: [Double] -> Double
desvioPadraoAgrupamento [] = 0
desvioPadraoAgrupamento lista = sqrt (sum (map (\x -> (x - mediaAritmetica lista) ^ 2) lista) / fromIntegral (length lista))

varianciaReal :: Double -> IO (Double)
varianciaReal variancia = do 
    return (variancia ^ 2)
  
renderGraf :: [Double] -> [Double] -> IO(Renderable ())
renderGraf z1 z2 = toRenderable layout
  where
    val = plot_lines_style . line_color .~ opaque blue
          $ plot_lines_values .~ [ zip z1 z2 ]
          $ plot_lines_title .~ "valores"
          $ def

    return (layout_title .~ "Resultados"
      $ layout_grid_last .~ False
      $ layout_plots .~ [toPlot val]
      $ def)

saveGraf :: Renderable a -> Maybe String -> Maybe String -> IO () 
saveGraf graf nm fmt = do
  namedGraf (maybe "grafico" id nm) (maybe "svg" id fmt) graf
  where
    namedGraf :: String -> String -> Renderable a -> IO ()
    namedGraf sNm sFmt grf = void $ renderableToFile def ("img/" ++ sNm ++ "." ++ sFmt) grf