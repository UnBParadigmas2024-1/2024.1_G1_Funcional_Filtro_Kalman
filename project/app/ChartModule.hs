{-# LANGUAGE OverloadedStrings #-}
module ChartModule where

import Graphics.Vega.VegaLite

generateChart :: [Double] -> [Double] -> IO ()
generateChart z1 z2 = do
    let minValz2 = minimum z2 
        maxValz2 = maximum z2
        
    -- Definindo os dados
    let dados = dataFromColumns []
          . dataColumn "time" (Numbers z1)
          . dataColumn "valuesOut" (Numbers z2)

        encOut = encoding
            . position X [PName "time", PmType Quantitative]
            . position Y [PName "valuesOut", PmType Quantitative, PScale [SDomain (DNumbers [minValz2, maxValz2]), SClamp True]]
            . stroke [MString "red"] -- Definindo a cor da linha de valores pos filtragem como vermelha

    toHtmlFile "grafico.html" $ toVegaLite
      [ dados []
      , vConcat [asSpec [mark Line [], encOut []]]
      , height 500
      , width 1000
      ]
