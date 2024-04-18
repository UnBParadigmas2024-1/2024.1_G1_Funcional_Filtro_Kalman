module Submenu (submenu) where

import System.IO

submenu :: IO ()
submenu = do
    putStr "\ESC[2J\ESC[H" 
    putStrLn "Submenu - Selecione uma opcao:"
    putStrLn "1. Plotar grafico do sensor 1"
    putStrLn "2. Plotar grafico do sensor 2"
    putStrLn "3. Plotar graficos da saida filtrada"
    putStrLn "4. Voltar ao menu principal"

    putStr "Opcao: "
    hFlush stdout
    option <- getLine
    case option of
        "1" -> do
            submenu
        "2" -> do
            submenu
        "3" -> do
            submenu
        "4" -> do 
            putStrLn "Voltando ao menu principal..."

        _ -> do
            putStrLn "Opção inválida!"
            submenu