{-# LANGUAGE FlexibleInstances #-}

module Chapter_18_my_note where

import           System.IO
import           System.IO.Unsafe (unsafePerformIO)
import           Chapter_17_my_note ( Parse (..)
                                    , none
                                    , succeed )
import qualified Control.Monad.Identity as I
import qualified Data.List as L

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

_fmap :: (a -> b) -> IO a -> IO b
_fmap f origio    = do val <- origio
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

copyInteract :: IO ()
copyInteract    = do hSetBuffering stdin LineBuffering
                     copyEOF
                     hSetBuffering stdin NoBuffering

copyEOF :: IO ()
copyEOF    = do eof <- isEOF
                if eof
                then return ()
                else do line <- getLine
                        putStrLn line
                        copyEOF

combination :: [a -> a] -> (a -> a)
combination funcs valfeed    = foldr (\f x -> f x) valfeed funcs

promptReadFile :: IO ()
promptReadFile    = do loca <- getLine
                       (putStrLn . unsafePerformIO . readFile) loca

listIOprog :: String -> String
listIOprog    = unlines . map reverse . lines

sumIntsFile :: IO ()
sumIntsFile    = do loca <- getLine
                    let contents = (lines . unsafePerformIO . readFile) loca
                    let values = map (\x -> read x :: Int) contents
                    let result = sum values
                    print result

promptReadFileBind :: IO ()
promptReadFileBind    = getLine >>= (putStrLn . unsafePerformIO . readFile)

sumIntsFileBind :: IO ()
sumIntsFileBind      = getLine >>= process
    where process :: String -> IO ()
          process    = print . sum . map (\x -> read x :: Int)
                       . lines . unsafePerformIO . readFile

addOneLineParse :: IO ()
addOneLineParse    = getLine >>= \line -> (putStrLn . show) (1 + read line :: Int)

(>@>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
f >@> g    = \x -> f x >>= g

newtype MP a b = MP { mp :: Parse a b }

phoneBook :: [(String, String)]
phoneBook    = [ ("betty", "number1")
               , ("bonnie", "number2")
               , ("pasty", "number3")
               , ("lucille", "number4")
               , ("wendy", "number5")
               , ("penny", "number6")
               , ("wendy", "number7") ]

findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey key    = foldr (\(ck, cv) t -> if ck == key then Just cv else t) Nothing

findKey_ :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey_ key    = foldl (\t (ck, cv) -> if ck == key then Just cv else t) Nothing
