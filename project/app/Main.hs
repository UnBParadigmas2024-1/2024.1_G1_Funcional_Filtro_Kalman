{-# LANGUAGE OverloadedStrings #-}

import Graphics.Vega.VegaLite

main :: IO ()
main = do
  let dataVals = dataFromColumns []
        . dataColumn "Gols feitos" (Numbers [3, 2, 4, 5, 6])
        . dataColumn "Gols sofridos" (Numbers [1, 2, 3, 0, 1])
      
      enc = encoding
        . position X [PName "Gols feitos", PmType Quantitative]
        . position Y [PName "Gols sofridos", PmType Quantitative]
         
  toHtmlFile "jogos.html" $ toVegaLite [width 800, height 800, dataVals [], mark Point [MFilled True], enc [] ]

