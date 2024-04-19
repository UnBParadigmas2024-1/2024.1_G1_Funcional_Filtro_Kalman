import System.IO
import Menu

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering 
    mainMenu 0.0 0.0 0.0 0.0 0.0 0.0 [0.0] [0.0] [0.0]
