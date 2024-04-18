import System.IO
import Utils 

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering 
    mainMenu 0.0 0.0 0.0
