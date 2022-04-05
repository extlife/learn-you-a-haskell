import Control.Monad ( forever )
import Data.Char ( toUpper )

main :: IO ()
main = forever $ do
    putStrLn "Give me some input: "
    line <- getLine 
    putStrLn $ map toUpper line
