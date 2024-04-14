module Main (main) where

import EstatisticasAgrupamento

main :: IO ()
main = do
  let dados = [[1.0, 2.0, 3.0], [4.0, 5.0, 6.0], [7.0, 8.0, 9.0]]
  putStrLn $ "Média Agrupada: " ++ show (mediaAgrupada dados)
  putStrLn $ "Desvio Padrão Real: " ++ show (desvioPadraoReal dados)
  putStrLn $ "Erro Média Agrupada: " ++ show (erroMediaAgrupada dados)
  putStrLn $ "Erro Desvio Padrão Real: " ++ show (erroDesvioPadraoReal dados)