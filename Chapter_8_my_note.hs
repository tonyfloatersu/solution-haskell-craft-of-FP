module Chapter_8_my_note where

import           Chapter_7_my_note (isPalin, splitWord, qSort)
import           Data.Time.Clock
import           Data.Time.Format
import           System.IO.Unsafe
import           Test.QuickCheck

data Move = Rock | Paper | Scissors
            deriving Eq

instance Show Move where
         show Rock        = "r"
         show Paper       = "p"
         show Scissors    = "s"

convertMove :: Char -> Move
convertMove 'r'    = Rock
convertMove 'R'    = Rock
convertMove 'p'    = Paper
convertMove 'P'    = Paper
convertMove 's'    = Scissors
convertMove 'S'    = Scissors
convertMove _      = error "do not match"

type Tournament = ([Move], [Move])

outcome :: Move -> Move -> Integer
outcome Rock Paper        = -1
outcome Paper Rock        = 1
outcome Paper Scissors    = -1
outcome Rock Scissors     = 1
outcome Scissors Paper    = 1
outcome Scissors Rock     = -1
outcome _ _               = 0

tournamentOutcome :: Tournament -> Integer
tournamentOutcome tour    = sum [outcome (fst tp) (snd tp)
                                 | tp <- (zip (fst tour) (snd tour))]

type Strategy    = [Move] -> Move

rock, paper, scissors :: Strategy
rock _        = Rock
paper _       = Paper
scissors _    = Scissors

cycle' :: Strategy
cycle' moves    = case length moves `rem` 3 of
                       0 -> Rock
                       1 -> Paper
                       2 -> Scissors

echo :: Strategy
echo (latest : _) = latest
echo []           = Rock

data Result = Win | Lose | Draw
              deriving (Eq, Show)

beat :: Move -> Move
beat Rock  = Paper
beat Paper = Scissors
beat _     = Rock

lose :: Move -> Move
lose Rock  = Scissors
lose Paper = Rock
lose _     = Paper

draw :: Move -> Move
draw a    = a

winLast :: Strategy
winLast (latest : _) = beat latest
winLast []           = Rock

loseLast :: Strategy
loseLast (latest : _) = lose latest
loseLast []           = Scissors

randomInt :: Integer -> IO Integer
randomInt n    = do
    time <- getCurrentTime
    return ((`rem` n) $ read $ take 6 $ formatTime defaultTimeLocale "%q" time)

randInt :: Integer -> Integer
randInt    = unsafePerformIO . randomInt

convertToMove :: Integer -> Move
convertToMove num    = case num of
                            0 -> Rock
                            1 -> Scissors
                            _ -> Paper

randomStrategy :: Strategy
randomStrategy _    = convertToMove (randInt 3)

randomStrategyOptional :: Strategy
randomStrategyOptional []        = randomStrategy []
randomStrategyOptional [step]    = randomStrategy [step]
randomStrategyOptional (mv1 : mv2 : remain)
    | mv1 /= mv2                 = randomStrategy (mv1 : mv2 : remain)
    | otherwise                  = case mv1 of
                                        Rock     -> Scissors
                                        Scissors -> Paper
                                        _        -> Rock

moveIndex :: [Move] -> Move -> Int
moveIndex moves target    = length [mv | mv <- moves, mv == target]

movesNameIndex :: [Move] -> [(Move, Int)]
movesNameIndex moves    = [ (Rock, moveIndex moves Rock)
                          , (Paper, moveIndex moves Paper)
                          , (Scissors, moveIndex moves Scissors)]

findMinNum :: [(a, Int)] -> Int
findMinNum    = minimum . snd . unzip

minMoveName :: [(Move, Int)] -> [Move]
minMoveName _list    = [fst frag | frag <- _list, snd frag == findMinNum _list]

bestStrategyPredict :: Move -> Move -> Move
bestStrategyPredict mv1 mv2    = if mv1 == mv2
                                 then error "the moves are same, error"
                                 else case (mv1, mv2) of
                                           (Paper, Scissors) -> Scissors
                                           (Scissors, Paper) -> Scissors
                                           (Rock, Paper)     -> Paper
                                           (Paper, Rock)     -> Paper
                                           (Scissors, Rock)  -> Rock
                                           _                 -> Rock

findLeast :: [Move] -> Move
findLeast []    = randomStrategy []
findLeast _ls     = if length leastMoves == 2
                    then bestStrategyPredict (head leastMoves) (leastMoves !! 1)
                    else beat (head leastMoves)
  where
    leastMoves :: [Move]
    leastMoves    = (minMoveName . movesNameIndex) _ls

-- if we do not have the prev, then the function do not produce a certain solution
-- which goes against the pure function, this is not correct
-- so I have to add a prev move, or I just use a random choice based on 18.2
-- or I have to use a monad here.
-- alternate :: Strategy -> Strategy -> Strategy

-- got beaten in 8.7 - 8.10 have no clue about it....

reverseLine :: IO ()
reverseLine    = do
    line1 <- getLine
    (putStrLn . reverse) line1

getInt :: IO Integer
getInt    = do
    line <- getLine
    return (read line :: Integer)

ioIsPatin :: IO ()
ioIsPatin    = do
    line <- getLine
    if isPalin line
        then putStrLn ("``" ++ line ++ "`` " ++ "is palin")
        else putStrLn ("``" ++ line ++ "`` " ++ "is not Palin")

ioSumTwo :: IO ()
ioSumTwo    = do
    putStrLn "Give me two numbers: "
    num1 <- getInt
    num2 <- getInt
    putStrLn ("Here is the result " ++ show (num1 + num2))

putNtimes :: Integer -> String -> IO ()
putNtimes num str    = if num == 1
                       then putStrLn str
                       else do
                           putStrLn str
                           putNtimes (num - 1) str

ioPutNtimes :: IO ()
ioPutNtimes    = do
    times <- getInt
    str <- getLine
    putNtimes times str

getAllInt :: Integer -> IO [Integer]
getAllInt total    = if total == 1
                     then do
                         temp <- getInt
                         return [temp]
                     else do
                         temp <- getInt
                         _temp <- getAllInt (total - 1)
                         return (temp : _temp)

ioSumList :: IO ()
ioSumList    = do
    putStrLn "Give me a number: "
    quantity <- getInt
    putStrLn ("Give me " ++ show quantity ++ " number(s): ")
    list <- getAllInt quantity
    let listsum = sum list
    putStrLn ("Here is the sum of " ++ show quantity
              ++ "number(s): " ++ show listsum)

copyEmpty :: IO ()
copyEmpty    = do
    line <- getLine
    if line == ""
        then return ()
        else do
            putStrLn line
            copyEmpty

tripadd :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
tripadd (p1pt1, p1pt2, p1pt3) (p2pt1, p2pt2, p2pt3)
    = (p1pt1 + p2pt1, p1pt2 + p2pt2, p1pt3 + p2pt3)

lineConcat :: IO [String]
lineConcat    = do
    line <- getLine
    if line /= ""
        then do
            _rest <- lineConcat
            return (line : _rest)
        else return []

unix_wc :: IO ()
unix_wc    = do
    linestore <- lineConcat
    let lineNum = length linestore
    let symbNum = sum [length len | len <- linestore]
    let wordNum = sum [(length . splitWord) len | len <- linestore]
    putStrLn ("line: " ++ show lineNum
              ++ ", word: " ++ show wordNum
              ++ ", symbol: " ++ show symbNum)

serialPalin :: [String] -> IO ()
serialPalin strset    = if length strset == 0
                        then return ()
                        else do
                            palinJdg (head strset)
                            serialPalin (tail strset)
  where palinJdg :: String -> IO ()
        palinJdg line    = if isPalin line
                           then putStrLn ("``" ++ line ++ "`` " ++ "is palin")
                           else putStrLn ("``" ++ line ++ "`` " ++ "is not Palin")
combinedPalin :: IO ()
combinedPalin    = do
    storage <- lineConcat
    serialPalin storage

sumButZero :: IO Integer
sumButZero    = do
    test <- getInt
    if test == 0
        then return 0
        else do
            _rem <- sumButZero
            return (_rem + test)

ioSumButZero :: IO ()
ioSumButZero    = do
    putStrLn "Give me integers but zerooooooo:"
    res <- sumButZero
    putStrLn ("Result is: " ++ show res)

listButZero :: IO [Integer]
listButZero    = do
    test <- getInt
    if test == 0
        then return []
        else do
            _rem <- listButZero
            return (test : _rem)

printListNum :: [Integer] -> IO ()
printListNum ls    = if length ls /= 0
                     then do
                         putStrLn (show (head ls))
                         printListNum (tail ls)
                     else return ()

ioListSortedButZero :: IO ()
ioListSortedButZero    = do
    storage <- listButZero
    let sorted   = qSort storage
    printListNum sorted

copy :: IO ()
copy =    do
    line <- getLine
    let whileCopy = do
        if line == ""
            then return ()
            else do
                putStrLn line
                line <- getLine
                whileCopy
    whileCopy

-- the function do not stop if you do not input "" at first time
-- the immitation to while is wrong here
-- since line is predefined for the whileCopy,
-- so it will never change variable line twice

showResults :: Tournament -> IO ()
showResults tour    = do
    let res    = tournamentOutcome tour
    putStrLn (case compare res 0 of
                   GT -> "I won!"
                   EQ -> "Draw!"
                   LT -> "You won!")

playInteractive :: Strategy -> Tournament -> IO ()
playInteractive strategy tnmt@ (mine, yours)    = do
    ch <- getChar
    if not (ch `elem` "rpsRPS")
        then showResults tnmt
        else do
            let next    = strategy yours
            putStrLn ("\nI play: " ++ show next ++ " you play: " ++ [ch])
            let yourmove    = convertMove ch
            playInteractive strategy (next : mine, yourmove : yours)

play :: Strategy -> IO ()
play strategy    = playInteractive strategy ([], [])

randomPlay :: IO ()
randomPlay    = do
    rand <- randomInt 10
    play (case rand of
               0 -> echo
               1 -> rock
               3 -> randomStrategy
               4 -> randomStrategyOptional
               5 -> findLeast
               6 -> paper
               7 -> scissors
               8 -> winLast
               9 -> loseLast)

step :: Strategy -> Strategy -> Tournament -> Tournament
step strategyA strategyB ( moveA, moveB )    =
    (strategyA moveB : moveA, strategyB moveA : moveB)

playSvsS :: Strategy -> Strategy -> Integer -> Tournament
playSvsS strategyA strategyB _round    = if _round == 1
                                         then step strategyA strategyB ([], [])
                                         else step strategyA strategyB
                                               (playSvsS strategyA strategyB (_round - 1))
