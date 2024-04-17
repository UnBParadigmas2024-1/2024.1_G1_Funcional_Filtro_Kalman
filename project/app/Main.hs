module Main (main) where

import Utils
import Modules
import Lib

main :: IO ()
main =  do
  let (x_real', r1', r2', q', n') = initialization 0
      r = [[1, 0], [0, 1]] -- Matriz identidade 2x2 como exemplo
      h = [[1], [1]]
      x0 = 10.0
      p0 = 1.0
  putStrLn $ "Valor de x_real: " ++ show x_real'
  putStrLn $ "Valor de r1: " ++ show r1'
  putStrLn $ "Valor de r2: " ++ show r2'
  putStrLn $ "Valor de q: " ++ show q'
  putStrLn $ "Valor de n: " ++ show n'

  z1 <- generateMeasurements x_real' r1' n' 
  z2 <- generateMeasurements x_real' r2' n'   
  
  putStrLn $ "Medições geradas para z1: " ++ show z1
  putStrLn $ "Medições geradas para z2: " ++ show z2

  result <- kalmanFilter z1 z2 q' r h x0 p0
  print(result)

