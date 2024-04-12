-- Calcula media real (Calcula media normal), Calcula desvio padrao real
-- (Calcula desvio padrao real normal) e Estimar valores de erros (Quarteto B)


module EstatisticasAgrupamento
( mediaAgrupada
, desvioPadraoReal
, desvioPadraoAgrupamento
, media
) where

-- mediaReal :: [[Double]] -> Double
-- mediaReal [[]] = 0
-- mediaReal (xs:xss) = sum (concatMap sum (xs:xss))

mediaAgrupada :: [[Double]] -> Double
mediaAgrupada dados = sum (map sum dados) / (fromIntegral (length dados) * fromIntegral (length (head dados)))

desvioPadraoReal :: [[Double]] -> Double
desvioPadraoReal [[]] = 0 

desvioPadraoAgrupamento :: [Double] -> Double
desvioPadraoAgrupamento [] = 0
desvioPadraoAgrupamento lista = sqrt (sum (map (\x -> (x - media lista) ^ 2) lista) / fromIntegral (length lista))

media :: [Double] -> Double
media [] = 0
media conjunto = sum conjunto / fromIntegral (length conjunto)