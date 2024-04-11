module Utils where

import Numeric.LinearAlgebra
import System.Random

parseToInt :: String -> Int
parseToInt str = read str

generateMeasurements :: Double -> Double -> Double -> Int -> IO (Vector Double, Vector Double)
generateMeasurements xReal r n' = do
  let noise1 = randn n' * (sqrt r)
      noise2 = randn n' * (sqrt r)
      z1 = xReal + noise1
      z2 = xReal + noise2
  return (z1, z2)

initialization :: Int -> (Vector Double, Vector Double)
initialization n' =
  let xHat0 = konst 0 n'
      p0 = konst 1 n'
  in (xHat0, p0)

h :: Matrix Double
h = (2><1) [1, 1]

r :: Matrix Double
r = (2><2) [r1, 0, 0, r2]
  where
    r1 = 0.04  -- Variância do ruído de medição do sensor shunt
    r2 = 0.09  -- Variância do ruído de medição do sensor Hall

kalmanIteration :: (Vector Double, Vector Double) -> (Vector Double, Vector Double) -> Int -> (Vector Double, Vector Double)
kalmanIteration (xHatPrev, pPrev) (z1, z2) k =
  let xHatPred = xHatPrev
      pPred = pPrev + (konst q n)
      z = (2><1) [z1 @> k, z2 @> k]
      k' = inv (h <> pPred <> trans h + r)
      xHat = xHatPred + k' #> (z - h <> xHatPred)
      p = (ident n - k' <> h) <> pPred
      n = length xHatPrev
      q = 0.01  -- Variância do ruído do processo
  in (xHat, p)
