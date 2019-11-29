--year = truncate((((10^10 / 60 ) / 60) / 24) / 365)

main = do  
    putStrLn "What's your first name?"  
    firstName <- getLine  
    putStrLn "What's your last name?"  
    lastName <- getLine  
    putStrLn $ "hey " ++ firstName ++ " " ++ lastName ++ ", how are you?"  

date_secs secs = do
    let years = (((secs / 60 ) / 60) / 24) / 365
    putStrLn $ "Years: " ++ years