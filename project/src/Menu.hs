
module Menu where

import Utils

import System.IO
import Data.Vector (Vector)
import qualified Data.Vector as Vector

mainMenu ::  Double -> Double -> Double -> Double -> Double -> Double -> [Double] -> [Double] -> [Double] -> IO ()
mainMenu media1 desvio1 variancia1 media2 desvio2 variancia2 tempo sensor1 sensor2 = do     
    showMenu
    hFlush stdout
    option <- getLine
    getFluxMainMenu option media1 desvio1 variancia1 media2 desvio2 variancia2 tempo sensor1 sensor2

submenu :: [Double] -> [Double] -> [Double] -> [Double] -> IO ()
submenu tempo sensor1 sensor2 signal =  do
    showSubMenu
    hFlush stdout
    option <- getLine
    getFluxSubMenu option tempo sensor1 sensor2 signal


showSubMenu :: IO()
showSubMenu = do
    putStr "\ESC[2J\ESC[H" 
    putStrLn "Submenu - Selecione uma opcao:"
    putStrLn "1. Plotar grafico do sensor 1"
    putStrLn "2. Plotar grafico do sensor 2"
    putStrLn "3. Plotar graficos da saida filtrada"
    putStrLn "4. Exportar o CSV da saída filtrada"
    putStrLn "5. Voltar ao menu principal"
    putStr "Opcao: "

getFluxSubMenu :: String -> [Double] -> [Double] -> [Double] -> [Double] -> IO ()
getFluxSubMenu option tempo sensor1 sensor2 signal =  do
    case option of
        "1" -> do
            let grafico = renderGraf tempo sensor1
            saveGraf grafico (Just "grafico1") (Just "png")
            submenu tempo sensor1 sensor2 signal
        "2" -> do
            let grafico = renderGraf tempo sensor2
            saveGraf grafico (Just "grafico2") (Just "png")
            submenu tempo sensor1 sensor2 signal
        "3" -> do
            let grafico = renderGraf tempo signal
            saveGraf grafico (Just "grafico1") (Just "png")
            submenu tempo sensor1 sensor2 signal
        "4" -> do 
            items <- return (Vector.fromList(convertItems tempo signal))
            filePath <- return ("kalman_filter.csv")
            encodeItemsToFile filePath items
            submenu tempo sensor1 sensor2 signal
        "5" -> do 
            putStrLn "Voltando ao menu principal..."
        _ -> do
            putStrLn "Opção inválida!"
            submenu tempo sensor1 sensor2 signal
            
showMenu :: IO()
showMenu = do
    putStr "\ESC[2J\ESC[H" 
    putStrLn "Selecione uma opcao:"
    putStrLn "1. Setar valores estatísticos do sensor 1"
    putStrLn "2. Setar valores estatísticos do sensor 2"
    putStrLn "3. Inserir valores do sensor 1"
    putStrLn "4. Inserir valores do sensor 2"
    putStrLn "5. Exibir estatísticas gerais"
    putStrLn "6. Plotar ou Exportar o gráfico dos sensores 1 e 2 e da saída filtrada"
    putStrLn "7. Sair"
    putStr "Opcao: "
    



getFluxMainMenu :: String -> Double -> Double -> Double -> Double -> Double -> Double -> [Double] -> [Double] -> [Double] -> IO ()
getFluxMainMenu option media1 desvio1 variancia1 media2 desvio2 variancia2 tempo sensor1 sensor2 = do    
    case option of
        "1" -> do
            putStr "Digite o caminho do seu arquivo:"
            hFlush stdout
            path <- getLine
            (list1, list2) <- parserCsvGrouped path
            media1 <- mediaAgrupada list2
            desvio1 <- desvioPadraoReal list2
            variancia1 <- varianciaReal desvio1
            mainMenu media1 desvio1 variancia1 media2 desvio2 variancia2 tempo sensor1 sensor2
        "2" -> do
            putStr "Digite o caminho do seu arquivo:"
            hFlush stdout
            path <- getLine
            (list1, list2) <- parserCsvGrouped path
            media2 <- mediaAgrupada list2
            desvio2 <- desvioPadraoReal list2
            variancia2 <- varianciaReal desvio2
            mainMenu media1 desvio1 variancia1 media2 desvio2 variancia2 tempo sensor1 sensor2
        "3" -> do
            putStr "Digite o caminho do seu arquivo:"
            hFlush stdout
            path <- getLine
            (tempo, sensor1) <- parserCsvNotGrouped path
            mainMenu media1 desvio1 variancia1 media2 desvio2 variancia2 tempo sensor1 sensor2
        "4" -> do
            putStr "Digite o caminho do seu arquivo:"
            hFlush stdout
            path <- getLine
            (tempo, sensor2) <- parserCsvNotGrouped path
            mainMenu media1 desvio1 variancia1 media2 desvio2 variancia2 tempo sensor1 sensor2
        "5" -> do
            showStatistics media1 desvio1 variancia1 media2 desvio2 variancia2
            mainMenu media1 desvio1 variancia1 media2 desvio2 variancia2 tempo sensor1 sensor2
        "6" -> do
            r <- return([[variancia1, 0], [0, variancia2]]) 
            h <- return ([[1], [1]])
            x0 <- return (0.0)
            p0 <- return (1.0)
            q <- return (0.01)
            signal <- kalmanFilter sensor1 sensor2 q r h x0 p0
            submenu tempo sensor1 sensor2 signal
            mainMenu media1 desvio1 variancia1 media2 desvio2 variancia2 tempo sensor1 sensor2
        "7" -> putStrLn "Saindo..."
        _ -> do
            putStrLn "Opção inválida!"
            mainMenu media1 desvio1 variancia1 media2 desvio2 variancia2 tempo sensor1 sensor2



showStatistics :: Double -> Double -> Double -> Double -> Double -> Double -> IO()   
showStatistics media1 desvio1 variancia1 media2 desvio2 variancia2 = do
      putStr "\ESC[2J\ESC[H"
    
      putStrLn "Estatísticas Gerais:"
      putStrLn "Sensor 1:"
      putStrLn $ "Média: " ++ show (media1)
      putStrLn $ "Desvio Padrão: " ++ show (desvio1)
      putStrLn $ "Variância: " ++ show (variancia1)
      putStrLn "Sensor 2:"
      putStrLn $ "Média: " ++ show (media2)
      putStrLn $ "Desvio Padrão: " ++ show (desvio2)
      putStrLn $ "Variância: " ++ show (variancia2)
      putStr "\n"
      
      putStrLn "Selecione uma opcao:"
      putStrLn "1. Voltar ao menu principal"
      putStr "Opcao: "
      hFlush stdout
      option <- getLine
      case option of
          "1" -> do
              putStrLn "Voltando ao menu principal..."
          _ -> do
              putStrLn "Opção inválida!"
              showStatistics media1 desvio1 variancia1 media2 desvio2 variancia2

