
module MenuModules
    ( submenu, showSubMenu, getFluxSubMenu, mainMenu, getFluxMainMenu, showMenu
    ) where

submenu :: IO ()
submenu = do
    showSubMenu
    -- hFlush stdout
    option <- getLine
    getFluxSubMenu option
    
showSubMenu :: IO()
showSubMenu = do
    putStrLn "Submenu - Selecione uma opcao:"
    putStrLn "1. Plotar grafico do sensor 1"
    putStrLn "2. Plotar grafico do sensor 2"
    putStrLn "3. Plotar graficos da saida filtrada"
    putStrLn "4. Voltar ao menu principal"
    putStr "Opcao: "

getFluxSubMenu :: String -> IO()
getFluxSubMenu option = do
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

mainMenu :: IO ()
mainMenu = do    
    showMenu
    -- hFlush stdout
    option <- getLine
    getFluxMainMenu option

getFluxMainMenu :: String -> IO()
getFluxMainMenu option = do
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

showMenu :: IO()
showMenu = do
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