module Chapter_8_my_note where

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
