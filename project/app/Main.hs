{-# LANGUAGE OverloadedStrings #-}

import Graphics.Vega.VegaLite

main :: IO ()
main = do
  let n = 10
      real = replicate n 10
      z1 = [9.9388, 9.9902, 9.8624, 9.9488,  9.8977, 10.0547, 9.9719, 9.9351, 10.0237, 9.9385]
      z2 = [10.0511, 9.9858, 10.1017, 9.8966, 10.0792, 9.9726, 10.0042 , 9.8847, 10.0458 , 9.9352]

      -- Definindo os dados
      dados = dataFromColumns []
        . dataColumn "posicaoVetor" (Numbers [1..10])
        . dataColumn "erro" (Numbers z1)
        . dataColumn "desvioPadrao" (Numbers z2)
        . dataColumn "real" (Numbers real)

      encErro = encoding
            . position X [PName "posicaoVetor", PmType Quantitative]
            . position Y [PName "erro", PmType Quantitative, PScale [SDomain (DNumbers [9.6, 10.4]), SClamp True]]
            . stroke [MString "red"] -- Definindo a cor da linha de erro como vermelha

      encDesvioPadrao = encoding
            . position X [PName "posicaoVetor", PmType Quantitative]
            . position Y [PName "desvioPadrao", PmType Quantitative, PScale [SDomain (DNumbers [9.6, 10.4]), SClamp True]]
            . stroke [MString "blue"] -- Definindo a cor da linha de desvio padrão como azul
        
      encReal = encoding
            . position X [PName "posicaoVetor", PmType Quantitative]
            . position Y [PName "real", PmType Quantitative, PScale [SDomain (DNumbers [9.6, 10.4]), SClamp True]]
            . stroke [MString "green"] -- Definindo a cor da linha de desvio padrão como azul  

  toHtmlFile "grafico.html" $ toVegaLite
   [ dados []
   , layer [asSpec [mark Line [], encErro []], asSpec [mark Line [], encDesvioPadrao []], asSpec [mark Line [], encReal []]]
   , height 300
   , width 400
   ]
