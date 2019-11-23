-- Alumno: Víctor Manuel Cavero Gracia

import Data.List
import Data.Char
import Data.List.Split
import System.IO

--Ejercicio 1: 

adivina :: Int -> IO()
adivina n = do
    putStrLn "Guess: "
    number <- getLine
    if ((read number :: Int) == n) then putStrLn "Congrats!!! You guessed!"
    else
        if ((read number :: Int) > n) then do
            putStrLn "Oh... that's higher"
            adivina n
        else do 
            putStrLn "Oh... that's lower"
            adivina n

--Ejercicio 2:

contarpalabras :: IO() 
contarpalabras = do
    putStrLn "Insert a line:" 
    line <- getLine
    let {x = length (wordsBy (== ' ') line)} in putStrLn ("Size: " ++ (show x))

--Ejercicio 3:

--a:
palabras:: String -> IO () 
palabras path = do
    handle <- openFile path ReadMode
    contents <- hGetContents handle
    (let {x = length (wordsBy (== ' ') contents)} in putStrLn ("Size: " ++ (show x)) )
    hClose handle

--b

--c

--d

--e