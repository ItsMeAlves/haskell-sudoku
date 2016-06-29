input :: (String -> b) -> IO ()
input f = do
    line <- getLine
    f line
    return ()


