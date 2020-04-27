-- Chatterbot module

chatterbot :: String -> [(String, String)] -> IO()
chatterbot botName botRules = do
    putStrLn ("\n\nHi! I am " ++ botName ++ ". How are you?")
    botLoop

    where
        Bbrain = rulesCompile botRules
        botLoop = do
            putStr "\n: "
            question <- getLine
            answer   <- stateOfMind brain
            putStrLn (botName ++ ": " ++ (present . answer . prepare) question)
            
            if(not . endOfDialog) question
                then botLoop
            else
                return ()
    
