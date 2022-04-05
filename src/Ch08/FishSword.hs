import Control.Monad ( when )

main :: IO ()
main = do
    input <- getLine 
    when (input == "FISH-SWORD") $ do
        putStrLn input
