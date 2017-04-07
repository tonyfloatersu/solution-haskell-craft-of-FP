module Chapter_18_my_note where

import           System.IO.Unsafe (unsafePerformIO)

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

whileG :: (a -> IO Bool) -> (a -> IO a) -> (a -> IO a)
whileG test oper val    = do goOrStop <- test val
                             if goOrStop
                             then return val
                             else do operated <- oper val
                                     whileG test oper operated

improvedSumer :: (Integer, Integer) -> IO (Integer, Integer)
improvedSumer (times, n)    = do putStrLn "give me a number: "
                                 temp <- getLine
                                 return (times - 1, n + read temp :: Integer)

controller :: (Integer, Integer) -> IO Bool
controller (val, _)    = return (val <= 0)

getAverage :: IO ()
getAverage    = do putStrLn "Give me a integer for average number: "
                   tempAvgNum <- getLine
                   let avgNum = read tempAvgNum :: Integer
                   (_, sumVal) <- whileG controller improvedSumer (avgNum, 0)
                   let sumValFloat = fromInteger sumVal :: Float
                   let avgNumFloat = fromInteger avgNum :: Float
                   let average = sumValFloat / avgNumFloat
                   putStrLn ("the average you want is: " ++ show average)

accumulate :: [IO a] -> IO [a]
accumulate    = return . map unsafePerformIO

sequence :: [IO a] -> IO ()
sequence iolist    = do _ <- accumulate iolist
                        return ()

seqFuns :: [a -> IO a] -> (a -> a)
seqFuns funs iniv       = foldr subfunc iniv funs
  where subfunc :: (a -> IO a) -> a -> a
        subfunc func    = unsafePerformIO . func

seqList :: [a -> IO a] -> a -> IO a
seqList funcs val    = return (seqFuns funcs val)
