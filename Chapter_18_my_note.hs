module Chapter_18_my_note where

readAndWrite :: IO ()
readAndWrite    = do line <- getLine
                     putStrLn "Get one line"
                     putStrLn line

readEcho :: IO ()
readEcho    = do putStrLn "Give me a line"
                 line <- getLine
                 putStrLn ("the line we get is: " ++ line)

sumInts :: Integer -> IO Integer
sumInts s    = do temp <- getLine
                  let n = read temp :: Integer
                  if n == 0
                  then return s
                  else sumInts (s + n)

sumInteract :: IO ()
sumInteract    = do putStrLn "Enter Integers one per line"
                    putStrLn "These will be summed until zero occurred"
                    sum_val <- sumInts 0
                    putStrLn ("The final sum is " ++ show sum_val)

fmap :: (a -> b) -> IO a -> IO b
fmap f origio    = do val <- origio
                      return (f val)

repeater :: IO Bool -> IO () -> IO ()
repeater test oper    = do checker <- test
                           if checker
                           then return ()
                           else do oper
                                   repeater test oper
