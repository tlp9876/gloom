import Data.List
import Control.Monad
import Control.Monad.Random
import System.Random.Shuffle

type Card = Maybe Int
type Deck = [Card]

mean :: (Real a, Fractional b) => [a]-> b
mean x = realToFrac (sum x) / (genericLength x)


baseDeck :: [Int]
baseDeck = [-2,-1,-1,-1,-1,-1,0,0,0,0,0,1,1,1,1,1,2]

fullDeck :: Deck
fullDeck = map Just baseDeck ++ [Nothing, Nothing]

plus3 :: Deck -> Deck
plus3 x = x ++ [Just 3]


removecard :: Card -> Int -> Deck -> Deck
removecard c 0 xs = xs
removecard c n [] = error "not enough"
removecard c n (x:xs) = if c == x
  then removecard c (n-1) xs
  else [x] ++ removecard c n xs


removeminus1s :: Int -> Deck -> Deck
removeminus1s = removecard (Just (-1))

replaceminus1plus1 :: Deck -> Deck
replaceminus1plus1 xs = (removeminus1s 1 xs) ++ [Just 1]


remove3minus1s :: Deck -> Deck
remove3minus1s = removeminus1s 3

manyDecks :: (MonadRandom m) => Int -> Deck -> m [Deck]
manyDecks n deck = sequence (map shuffleM (replicate n deck))

playRounds2 :: Int -> Deck -> [Deck] -> (Int, Int)
playRounds2 0 deck decks = (0,0)
playRounds2 rounds [] decks = error "empty deck"
playRounds2 rounds (Nothing:xs) decks = (score,shuffs+1)
    where (score,shuffs) = playRounds2 (rounds-1) (head decks) (tail decks)
playRounds2 rounds (Just x:xs) decks = (score+x, shuffs)
    where (score,shuffs) = playRounds2 (rounds-1) xs decks

playRounds :: Int -> [Deck] -> (Int,Int)
playRounds rounds decks = playRounds2 rounds (head decks) (tail decks)

playRoundsM :: (MonadRandom m) => Int -> Deck -> m (Int,Int)
playRoundsM rounds deck = do {decks <- manyDecks rounds deck; return (playRounds rounds decks)} 


sample :: (MonadRandom m) => Int -> Int -> Deck -> m [Int]
sample 0 rounds deck = return []
sample size rounds deck = do {
  (x,dontcare) <- (playRoundsM rounds deck);
  xs <- (sample (size-1) rounds deck);
  return ([x] ++ xs)
}

sampleMean :: (MonadRandom m, Fractional b) => Int -> Int -> Deck -> m b
sampleMean size rounds deck = (liftM mean) (sample size rounds deck)

