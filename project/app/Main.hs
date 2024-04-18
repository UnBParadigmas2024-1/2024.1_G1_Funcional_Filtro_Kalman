import System.IO
import Menu 

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- Para evitar atrasos na exibição do menu
    mainMenu
