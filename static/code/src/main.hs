

main :: IO ()
main = execMany [putStrLn "10", putStrLn "20", putStrLn "30"]


helper :: Int -> IO ()
helper n = do
  putStrLn (show n ++ " What is ur name?")
  name <- getInputFromUser
  if name /= "quit"
    then do { putStrLn ("Hello, " ++ name ++ "!") ; helper (n+1)}
    else putStrLn "Goodbye!"


execMany :: [IO ()] -> IO ()
execMany []     = return ()
execMany (r:rs) = r >> execMany rs



-- a_then_b :: IO ()
-- a_then_b = a >> b

getInputFromUser :: IO String
getInputFromUser = getLine

greeting :: String -> IO ()
greeting name = putStrLn ("hello " ++ name)

askName :: IO ()
askName = putStrLn "what is your name?"
