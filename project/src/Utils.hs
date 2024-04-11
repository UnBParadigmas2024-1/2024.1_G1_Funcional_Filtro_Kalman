module Utils where

import System.Random

parseToInt :: String -> Int
parseToInt str = read str

generateMeasurements :: Double -> [[Double]] -> Int -> IO (Double, Double)
generateMeasurements xReal r n' = do
  let r1 = sqrt (r !! 0 !! 0)
      r2 = sqrt (r !! 1 !! 1)
  noise1 <- randomRIO (-r1, r1)
  noise2 <- randomRIO (-r2, r2)
  let z1 = xReal + noise1
      z2 = xReal + noise2
  return (z1, z2)

initialization :: Int -> (Double, Double)
initialization _ = (0, 1)

h :: [[Double]]
h = [[1, 1]]

r :: [[Double]]
r = [[0.04, 0], [0, 0.09]] 

kalmanIteration :: (Double, Double) -> (Double, Double) -> Int -> (Double, Double)
kalmanIteration (xHatPrev, pPrev) (z1, z2) _ =
  let xHatPred = xHatPrev
      pPred = pPrev + q
      z = [z1, z2]
      k' = map (\row -> map (\val -> val / (r1 + r2)) row) h
      xHat = xHatPred + sum (zipWith (*) (zipWith (-) z (map (* xHatPred) (head h))) (map (* xHatPred) (head k')))
      p = (1 - sum (map (sum . map (* xHatPrev)) k')) * pPred
      q = 0.01 
      (r1:r2:_) = map sum r
  in (xHat, p)
