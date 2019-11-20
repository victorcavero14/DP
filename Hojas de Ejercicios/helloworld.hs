{-

    Funciones impuras: lo que nos devuelven puede cambiar para los mismos valores de entrada (getLine)
    Funciones puras: los valores devueltos son iguales para los mismos valores insertados.

-}

import Data.Char

main = do
    putStrLn "What's your first name?"
    firstName <- getLine
    putStrLn "What's your last name?"
    lastName <- getLine
    let {bigFirstName = map toUpper firstName; bigLastName = map toUpper lastName} in putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"



main' = do
    line <- getLine
    if null line
        then return ()
        else do
            putStrLn $ reverseWords line
            main'

reverseWords :: String -> String
reverseWords = unwords . map reverse . words