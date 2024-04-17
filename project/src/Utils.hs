module Utils where

import System.Random
import Data.List (concatMap)
import Data.Maybe (fromJust)

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

mediaAgrupada :: [[Double]] -> Double
mediaAgrupada [] = 0
mediaAgrupada dados = sum (map sum dados) / (fromIntegral (length dados) * fromIntegral (length (head dados)))

mediaAritmetica :: [Double] -> Double
mediaAritmetica [] = 0
mediaAritmetica conjunto = sum conjunto / fromIntegral (length conjunto)

desvioPadraoReal :: [[Double]] -> Double
desvioPadraoReal [] = 0
desvioPadraoReal dados =
    let desvios = map desvioPadraoAgrupamento dados
    in sum desvios / fromIntegral (length dados)

desvioPadraoAgrupamento :: [Double] -> Double
desvioPadraoAgrupamento [] = 0
desvioPadraoAgrupamento lista = sqrt (sum (map (\x -> (x - mediaAritmetica lista) ^ 2) lista) / fromIntegral (length lista))

varianciaAgrupamento :: [Double] -> Double
varianciaAgrupamento [] = 0
varianciaAgrupamento lista = desvioPadraoAgrupamento lista ^ 2

varianciaDesvioPadraoReal :: [[Double]] -> Double
varianciaDesvioPadraoReal [] = 0
varianciaDesvioPadraoReal lista = desvioPadraoReal lista ^ 2

varianciaReal :: [[Double]] -> Double
varianciaReal [] = 0
varianciaReal dados =
    let desvios = map varianciaAgrupamento dados
    in sum desvios / fromIntegral (length dados)

erroMediaAgrupada :: [[Double]] -> Double
erroMediaAgrupada [] = 0
erroMediaAgrupada dados =
    let dadosConcatenados = concat dados
        gruposIndividuais = map (:[]) dadosConcatenados
        mediaAgrupadaGrupos = mediaAgrupada gruposIndividuais
        mediaTotal = mediaAritmetica dadosConcatenados
    in abs (mediaAgrupadaGrupos - mediaTotal)

erroDesvioPadraoReal :: [[Double]] -> Double
erroDesvioPadraoReal [] = 0
erroDesvioPadraoReal dados = abs (desvioPadraoReal (map (: []) (concat dados)) - desvioPadraoReal dados)

erroPadrao :: [[Double]] -> Double
erroPadrao [] = 0
erroPadrao dados = desvioPadraoReal dados / (sqrt (fromIntegral (length dados)) * fromIntegral (length (head dados)))