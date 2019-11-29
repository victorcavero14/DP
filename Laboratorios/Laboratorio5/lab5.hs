-- Alumno: Víctor Manuel Cavero Gracia 3ºA
-- Asignatura: Programación Declarativa

import System.IO

--Ejercicio 1: 

adivina :: Int -> IO()
adivina n = do
    putStr "Guess: "
    number <- getLine
    let num = (read number :: Int)
    if (num == n) then putStrLn "Congrats!!! You guessed!"
    else
        if (num > n) then do
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
    contents <- readFile path
    let x = length (words contents)
    return x
  

--b

palabras' :: IO()
palabras' = do
    putStr "Insert a line: " 
    line <- getLine
    contents <- readFile line
    let x = length (words contents)
    putStrLn ("El fichero " ++ line ++ " tiene " ++ (show x) ++ " palabras.")

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
