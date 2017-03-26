module LibraryADT ( Database
                  , initDB
                  , books
                  , borrowers
                  , borrowed
                  , numborrowed
                  , makeLoan
                  , returnLoan ) where

type Person = String
type Book = String
type Loan = [Book]
type Borrowers = [Person]

newtype DBLoans = DBLoans (Person -> Loan)
newtype DBBrows = DBBrows (Book -> Borrowers)

type Database = (DBLoans, DBBrows)

initDB :: Database
initDB    = (DBLoans (\_ -> []), DBBrows (\_ -> []))

books :: Database -> Person -> Loan
books (DBLoans func, _)    = func

borrowers :: Database -> Book -> Borrowers
borrowers (_, DBBrows func)    = func

borrowed :: Database -> Book -> Bool
borrowed db bk    = (not . null) (borrowers db bk)

numborrowed :: Database -> Person -> Int
numborrowed db    = length . books db

makeLoan :: Database -> Person -> Book -> Database
makeLoan (DBLoans funl, DBBrows funb) pers bk
    = ( DBLoans (\x -> if x == pers then bk : funl x else funl x)
      , DBBrows (\x -> if x == bk then pers : funb x else funb x) )

returnLoan :: Database -> Person -> Book -> Database
returnLoan db pers bk    = maybe db id (returnLoanM db pers bk)

returnLoanM :: Database -> Person -> Book -> Maybe Database
returnLoanM db@ (DBLoans funl, DBBrows funb) pers bk
    = if (pers `elem` borrowers db bk) && (bk `elem` books db pers)
      then Just ( DBLoans (\x -> if x == pers then eliminate (funl x) pers else funl x)
                , DBBrows (\x -> if x == bk then eliminate (funb x) bk else funb x) )
      else Nothing

eliminate :: Eq a => [a] -> a -> [a]
eliminate [] _           = []
eliminate (x : xs) fd    = if x == fd then xs else x : eliminate xs fd
