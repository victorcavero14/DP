-- Alumno: Víctor Manuel Cavero Gracia 3ºA
-- Asignatura: Programación Declarativa

import Data.List
import Data.Char
import Data.List.Split
import System.IO

--Ejercicio 1: 

adivina :: Int -> IO()
adivina n = do
    putStr "Guess: "
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
    putStr "Insert a line: " 
    line <- getLine
    let {x = length (words line)} in putStrLn ("Size: " ++ (show x))

--Ejercicio 3:

--a:

palabras:: String -> IO Int 
palabras path = do
    handle <- openFile path ReadMode
    contents <- hGetContents handle
    let x = length (words contents)
    putStrLn ("Size: " ++ (show x))
    hClose handle
    return x

--b

palabras' :: IO()
palabras' = do
    putStr "Insert a line: " 
    line <- getLine
    handle <- openFile line ReadMode
    contents <- hGetContents handle
    let x = length (words contents)
    putStrLn ("El fichero " ++ line ++ " tiene " ++ (show x) ++ " palabras.")
    hClose handle


--c

promedia:: IO()
promedia = do 
    putStr "Insert a number: "
    line <- getLine
    let num = (read line :: Int)
    promedia_aux num num 1
    
promedia_aux:: Int -> Int -> Int -> IO()
promedia_aux num suma n = do
    if (num /= -1 && num >= 0) then do
        putStrLn ("Sum: " ++ show suma)
        putStrLn ("Average: " ++ (show (div suma n)))
        putStr "Insert a number: "
        line <- getLine
        let numero = (read line :: Int)
        promedia_aux numero (suma+numero) (n+1)
    else return ()
    
    
--d

--formatea:: String -> String -> Int -> IO()
--formatea fileIn fileOut n =

--e

--calculadora :: IO()
--calculadora =
