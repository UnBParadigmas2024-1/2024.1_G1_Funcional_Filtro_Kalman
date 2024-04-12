module Utils where

import System.Random

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
    n = 100

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

multiplyMatrices :: [[Double]] -> [[Double]] -> [[Double]]
multiplyMatrices m1 m2 = [[sum $ zipWith (*) r1 c2 | c2 <- transposeMatrix m2] | r1 <- m1]

cofactorMatrix :: [[Double]] -> [[Double]]
cofactorMatrix [[]] = [[]]
cofactorMatrix m = [ [(-1)^(i+j) * determinantMatrix (minorMatrix i j m) | j <- [0..(n-1)]] | i <- [0..(n-1)] ]

determinantMatrix :: [[Double]] -> Double
determinantMatrix [[x]] = x
determinantMatrix m = sum [ (-1)^i * (head m)!!i * determinantMatrix (minorMatrix 0 i m) | i <- [0..(n-1)] ]
  where n = length m

minorMatrix :: Int -> Int -> [[a]] -> [[a]]
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
    cofactor = cofactorMatrix m