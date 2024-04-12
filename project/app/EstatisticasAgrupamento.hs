-- Calcula media real (Calcula media normal), Calcula desvio padrao real
-- (Calcula desvio padrao real normal) e Estimar valores de erros (Quarteto B)


module EstatisticasAgrupamento
( mediaAgrupada
, desvioPadraoReal
, mediaAritmetica
) where

-- mediaReal :: [[Double]] -> Double
-- mediaReal [[]] = 0
-- mediaReal (xs:xss) = sum (concatMap sum (xs:xss))

mediaAgrupada :: [[Double]] -> Double
mediaAgrupada dados = sum (map sum dados) / (fromIntegral (length dados) * fromIntegral (length (head dados)))

desvioPadraoReal :: [[Double]] -> Double
desvioPadraoReal [[]] = 0 
desvioPadraoReal dados = sqrt((sum (concat (map (map (\x -> (x - mediaAgrupada dados) ^ 2)) dados))) / (fromIntegral (length dados)))

-- desvioPadraoAgrupamento :: [Double] -> Double
-- desvioPadraoAgrupamento [] = 0
-- desvioPadraoAgrupamento lista = sqrt (sum (map (\x -> (x - media lista) ^ 2) lista) / fromIntegral (length lista))

mediaAritmetica :: [Double] -> Double
mediaAritmetica [] = 0
mediaAritmetica conjunto = sum conjunto / fromIntegral (length conjunto)