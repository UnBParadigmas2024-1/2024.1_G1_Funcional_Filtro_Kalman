-- Calcula media real (Calcula media normal), Calcula desvio padrao real
-- (Calcula desvio padrao real normal) e Estimar valores de erros (Quarteto B)


module EstatisticasAgrupamento
( mediaAgrupada
, desvioPadraoReal
, desvioPadraoAgrupamento
, mediaAritmetica
, erroMediaAgrupada
, erroDesvioPadraoReal
, erroPadrao
) where

import Data.List (concatMap)
import Data.Maybe (fromJust)

-- mediaReal :: [[Double]] -> Double
-- mediaReal [[]] = 0
-- mediaReal (xs:xss) = sum (concatMap sum (xs:xss))

mediaAgrupada :: [[Double]] -> Double
mediaAgrupada [] = 0
mediaAgrupada dados = sum (map sum dados) / (fromIntegral (length dados) * fromIntegral (length (head dados)))

--desvioPadraoReal :: [[Double]] -> Double
--desvioPadraoReal [[]] = 0 
--desvioPadraoReal dados = sqrt((sum (concat (map (map (\x -> (x - mediaAgrupada dados) ^ 2)) dados))) / (fromIntegral (length dados)))

desvioPadraoReal :: [[Double]] -> Double
desvioPadraoReal [] = 0
desvioPadraoReal dados = sqrt ((sum (concat (map (\conjunto -> (map (\x -> (x - mediaAritmetica conjunto) ^ 2) conjunto)) dados))) / fromIntegral (length dados))

desvioPadraoAgrupamento :: [Double] -> Double
desvioPadraoAgrupamento [] = 0
desvioPadraoAgrupamento lista = sqrt (sum (map (\x -> (x - mediaAritmetica lista) ^ 2) lista) / fromIntegral (length lista))

mediaAritmetica :: [Double] -> Double
mediaAritmetica [] = 0
mediaAritmetica conjunto = sum conjunto / fromIntegral (length conjunto)

erroMediaAgrupada :: [[Double]] -> Double
erroMediaAgrupada [] = 0
erroMediaAgrupada dados = abs (mediaAgrupada (map (\x -> [x]) (concat dados)) - mediaAritmetica (concat dados))

erroDesvioPadraoReal :: [[Double]] -> Double
erroDesvioPadraoReal [] = 0
erroDesvioPadraoReal dados = abs (desvioPadraoReal (map (\x -> [x]) (concat dados)) - desvioPadraoReal dados)

erroPadrao :: [[Double]] -> Double
erroPadrao [] = 0
erroPadrao dados = desvioPadraoReal dados / (sqrt (fromIntegral (length dados)) * fromIntegral (length (head dados)))