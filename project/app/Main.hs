module Main (main) where

import EstatisticasAgrupamento

main :: IO ()
main = do
  let dados = [[0.01, 0.040756], [0.02, -0.017995], [0.03, 0.005326]]
  putStrLn $ "Média Agrupada: " ++ show (mediaAgrupada dados)
  putStrLn $ "Desvio Padrão Real: " ++ show (desvioPadraoReal dados)
  putStrLn $ "Erro Média Agrupada: " ++ show (erroMediaAgrupada dados)
  putStrLn $ "Erro Desvio Padrão Real: " ++ show (erroDesvioPadraoReal dados)
  putStrLn $ "Erro Padrão: " ++ show (erroPadrao dados)