module Chapter_8_my_note where

import           Chapter_7_my_note (isPalin)

data Move = Rock | Paper | Scissors
            deriving (Show, Eq)

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

type Strategy = [Move] -> Move

rock, paper, scissors :: Strategy
rock _        = Rock
paper _       = Paper
scissors _    = Scissors

cycle' :: Strategy
cycle' moves    = case length moves `rem` 3 of
                       0    -> Rock
                       1    -> Paper
                       2    -> Scissors

echo :: Strategy
echo (latest : _)    = latest
echo []              = Rock

data Result = Win | Lose | Draw
              deriving (Eq, Show)

beat :: Move -> Move
beat Rock     = Paper
beat Paper    = Scissors
beat _        = Rock

lose :: Move -> Move
lose Rock     = Scissors
lose Paper    = Rock
lose _        = Paper

draw :: Move -> Move
draw a    = a

winLast :: Strategy
winLast (latest : _)    = beat latest
winLast []              = Rock

loseLast :: Strategy
loseLast (latest : _)    = lose latest
loseLast []              = Scissors

-- leave this part to 18.2
randomStrategy :: Strategy
randomStrategy    = undefined

randomStrategyOptional :: Strategy
randomStrategyOptional []        = randomStrategy []
randomStrategyOptional [step]    = randomStrategy [step]
randomStrategyOptional (mv1 : mv2 : remain)
    | mv1 /= mv2                 = randomStrategy (mv1 : mv2 : remain)
    | otherwise                  = case mv1 of
                                        Rock        -> Scissors
                                        Scissors    -> Paper
                                        _           -> Rock

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
                                           (Paper, Scissors)    -> Scissors
                                           (Scissors, Paper)    -> Scissors
                                           (Rock, Paper)        -> Paper
                                           (Paper, Rock)        -> Paper
                                           (Scissors, Rock)     -> Rock
                                           _                    -> Rock

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
    putStrLn ("Here is the sum of " ++ show quantity ++ "number(s): " ++ show listsum)
