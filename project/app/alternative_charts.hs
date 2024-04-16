{-# LANGUAGE OverloadedStrings #-}

import Graphics.Vega.VegaLite

main :: IO ()
main = do
    let z1 = [1,2,3,4,5,6,7,8,9,10] -- Entrada dos valores de saida tempo
        z2 = [10.0511, 9.9858, 10.1017, 9.8966, 10.0792, 9.9726, 10.0042 , 9.8847, 10.0458 , 9.9352] -- entrada dos valores de saida dados
        minValz2 = minimum z2 
        maxValz2 = maximum z2
        
        --inpz1 = [1,2,4,5,6,7,8,9,10] -- entrada dos valores de entrada do programa sem filtragem se necessario
        --inpz2 = [1.1, 2.4, 3.7, 4.3, 5.6, 6.1, 7.3, 8.6, 9.4, 10.2] -- entrada dos dados sem filtragem, se aplicavel
        --mininpz2 = minimum inpz2
        --maxinpz2 = maximum inpz2

    -- Definindo os dados
    let dados = dataFromColumns []
          . dataColumn "time" (Numbers z1)
          . dataColumn "valuesOut" (Numbers z2)
         -- . dataColumn "valuesInp" (Numbers inpz2)

        --encInp = encoding
        --    . position X [PName "time", PmType Quantitative]
        --    . position Y [PName "valuesInp", PmType Quantitative, PScale [SDomain (DNumbers [mininpz2, maxinpz2]), SClamp True]]
        --    . stroke [MString "blue"] -- Definindo a cor da linha dos valores de entrada como azul

        encOut = encoding
            . position X [PName "time", PmType Quantitative]
            . position Y [PName "valuesOut", PmType Quantitative, PScale [SDomain (DNumbers [minValz2, maxValz2]), SClamp True]]
            . stroke [MString "red"] -- Definindo a cor da linha de valores pos filtragem como vermelha

    toHtmlFile "grafico.html" $ toVegaLite
      [ dados []
      , vConcat [ --asSpec [mark Line [], encInp []],
                  asSpec [mark Line [], encOut []]]
      , height 500
      , width 1000
      ]
