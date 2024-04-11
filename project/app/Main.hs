module Main (main) where

import Utils
import Modules
import Lib

main :: IO ()
main = do
  (z1, z2) <- generateMeasurements x_real r1 n

  let (xHat0, p0) = initialization n
      measurements = zip (toList z1) (toList z2)
      (xHatFinal, _) = foldl (\(xHat, pPrev) (z1', z2') -> kalmanIteration (xHat, pPrev) (fromList [z1', z2']) 1) (xHat0, p0) measurements

  putStrLn "Medições do sensor shunt:"
  print z1

  putStrLn "Medições do sensor Hall:"
  print z2

  putStrLn "Estimativas da corrente elétrica:"
  print xHatFinal
  
  putStrLn "Covariâncias do erro de estimativa:"
  print p0
  