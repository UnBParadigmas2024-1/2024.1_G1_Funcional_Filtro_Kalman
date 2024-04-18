module Menu (mainMenu) where

import System.IO
import Submenu

mainMenu :: IO ()
mainMenu = do    
    
    putStr "\ESC[2J\ESC[H" 
    putStrLn "Selecione uma opcao:"
    putStrLn "1. Setar valores estatísticos do sensor 1"
    putStrLn "2. Setar valores estatísticos do sensor 2"
    putStrLn "3. Inserir valores do sensor 1"
    putStrLn "4. Inserir valores do sensor 2"
    putStrLn "5. Exibir estatísticas gerais"
    putStrLn "6. Plotar o gráfico dos sensores 1 e 2 e da saída filtrada"
    putStrLn "7. Exportar o CSV da saída filtrada"
    putStrLn "8. Sair"
    putStr "Opcao: "
    hFlush stdout
    option <- getLine
    case option of
        "1" -> do
            mainMenu
        "2" -> do
            mainMenu
        "3" -> do
            mainMenu
        "4" -> do
            mainMenu
        "5" -> do
            mainMenu
        "6" -> do
            submenu
            mainMenu
        "7" -> do
            mainMenu
        "8" -> putStrLn "Saindo..."
        _ -> do
            putStrLn "Opção inválida!"
            mainMenu
