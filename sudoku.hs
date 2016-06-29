-- Haskell Sudoku Solver
-- Gabriel Alves de Lima

-- It requests a table, where the user enter each line.
-- Blank spaces should be '0'

-- Divider is a string to print between every table solution
divider = "-----------------"

-- a helper function to replace first given char in a string with another given char
replaceFirst :: Char -> String -> Char -> String
replaceFirst c (head:tail) x 
    | c == head = x:tail
    | otherwise = head : replaceFirst c tail x

-- helper function to apply a function to each element in a list
forEach [] _ = return ()
forEach (h:t) f = do
    f h
    forEach t f

-- pad is a helper function to fulfill a string with a given char until a desired size
pad :: String -> Char -> Int -> String
pad s c x 
    | length s < x = pad (s ++ [c]) c x
    | otherwise = s  

-- it receives a table and returns all of its lines in a list
cutLine :: String -> [String]
cutLine [] = []
cutLine s = (take 9 s):(cutLine $ drop 9 s)

-- it receives a table and returns all of its columns in a list
cutColumn :: String -> [String]
cutColumn s = cutLine $ transposed s
    where transposed list = [list !! (l+x) | l <- [0..8], x <- [0, 9, 18, 27, 36, 45, 54, 63, 72]]

-- it receives a table and returns all of its squares in a list
cutSquare :: String -> [String]
cutSquare s = (cutLine $ upper s) ++ (cutLine $ middle s) ++ (cutLine $ bottom s)
    where upper list = [list !! (l+x) | l <- [0,3,6], x <- [0, 1, 2, 9, 10, 11, 18, 19, 20]]
          middle list = [list !! (l+x) | l <- [0,3,6], x <- [27, 28, 29, 36, 37, 38, 45, 46, 47]]
          bottom list = [list !! (l+x) | l <- [0,3,6], x <- [54, 55, 56, 63, 64, 65, 72, 73, 74]] 

-- it prints a table and a divider
printTable :: [String] -> IO ()
printTable [] = putStrLn divider
printTable (h:t) = do
    putStrLn h
    printTable t

-- it requests all lines of a table and return an IO containing that
-- it also fills strings smaller than 9 with blank spaces
input :: String -> Int -> IO String
input s 0 = return s
input s x = do
    line <- getLine
    input (s ++ pad line '0' 9) (x - 1)

-- check if a table has 0
-- Without 0, it is done
done :: String -> Bool
done t = not $ '0' `elem` t


-- checks if a table is correct, following sudoku's rules
check :: String -> Bool
check t = (inspect t cutLine) && (inspect t cutColumn) && (inspect t cutSquare)
    where verify [a] = True
          verify (head:tail)
              | head == '0' = True && verify tail
              | head `elem` tail = False
              | otherwise = True && verify tail
          inspect x f = foldl (\w y -> w && y) True $ map verify (f x)
          
-- it generates all next valid moves
nextMoves t = filter check [replaceFirst '0' t x | x <- ['1'..'9']]

-- it receives a table and outputs every solution to that table
solve t 
    | done t = printTable $ cutLine t
    | otherwise = do
        forEach (nextMoves t) solve
        return ()

-- it's the main code stream
-- it starts by getting the table to solve and solves it
main = do
    startingTable <- input "" 9
    putStrLn divider
    solve startingTable
