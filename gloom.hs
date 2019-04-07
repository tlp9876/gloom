import Data.List
import Control.Monad
import Control.Monad.Random
import System.Random.Shuffle

-- deck defs
type Card = Maybe Int
type Deck = [Card]
type Perk = (String, Deck -> Deck) -- name, and transform

baseDeck :: [Int]
baseDeck = [-2,-1,-1,-1,-1,-1,0,0,0,0,0,1,1,1,1,1,2]

fullDeck :: Deck
fullDeck = map Just baseDeck ++ [Nothing, Nothing]

-- helpers
mean :: (Real a, Fractional b) => [a]-> b
mean x = realToFrac (sum x) / (genericLength x)

-- perks
addCard :: Card -> Int -> Deck -> Deck
addCard c 0 deck = deck
addCard c n deck = [c] ++ addCard c (n-1) deck

addNumberCard :: Int -> Int -> Deck -> Deck
addNumberCard c n = addCard (Just c) n 

add2NumberCards c1 n1 c2 n2 deck = addNumberCard c1 n2 (addNumberCard c2 n2 deck)

removeCard :: Card -> Int -> Deck -> Deck
removeCard c 0 xs = xs
removeCard c n [] = error "not enough"
removeCard c n (x:xs) = if c == x
  then removeCard c (n-1) xs
  else [x] ++ removeCard c n xs

removeNumberCard :: Int -> Int -> Deck -> Deck
removeNumberCard c n = removeCard (Just c) n

replaceCard :: Card -> Int -> Card -> Int -> Deck -> Deck
replaceCard c1 n1 c2 n2 deck = addCard c2 n2 (removeCard c1 n1 deck)

replaceNumberCard :: Int -> Int -> Int -> Int -> Deck -> Deck
replaceNumberCard c1 n1 c2 n2 = replaceCard (Just c1) n1 (Just c2) n2

perk_R1M1_A1P1 = ("Remove 1 -1, add 1 +1", replaceNumberCard (-1) 1 1 1)
perk_R1Z_A1P2 = ("Remove 1 zero, add 1 +1", replaceNumberCard 0 1 2 1)
perk_R1Z_A2P1 = ("Remove 1 zero, add 2 +1s", replaceNumberCard 0 2 1 2)
perk_R2P1_A2P2 = ("Remove 2 +1s, add 2 +2s", replaceNumberCard 1 2 2 2)
perk_R1M2_A1Z = ("Remove 2 -1s, add 1 zero", replaceNumberCard (-2) 1 0 1)
perk_R2M1 = ("Remove 2 -1s", removeNumberCard (-1) 2)
perk_R4Z = ("Remove 4 zeros", removeNumberCard 0 4)
perk_A2P1 = ("Add 2 +1s", addNumberCard 1 2)
perk_A1P3 = ("Add 1 +3", addNumberCard 3 1)
perk_A1M2_A2P2 = ("Add 1 -2 and 2 +2s", add2NumberCards (-2) 1 2 2)

perks_brute = [
    perk_R2M1,
    perk_R1M1_A1P1,
    perk_A2P1, perk_A2P1,
    perk_A1P3
  ]

perks_tinkerer = [
    perk_R2M1, perk_R2M1,
    perk_R1M2_A1Z,
    perk_A2P1,
    perk_A1P3
  ]

perks_spellweaver = [
    perk_R4Z,
    perk_R1M1_A1P1, perk_R1M1_A1P1,
    perk_A2P1, perk_A2P1
  ]

perks_scoundrel = [
    perk_R2M1, perk_R2M1,
    perk_R4Z,
    perk_R1M2_A1Z,
    perk_R1M1_A1P1,
    perk_R1Z_A1P2, perk_R1Z_A1P2
  ]

perks_cragheart = [
    perk_R4Z,
    perk_R1M1_A1P1, perk_R1M1_A1P1, perk_R1M1_A1P1,
    perk_A1M2_A2P2
  ]

perks_mindthief = [
    perk_R2M1,
    perk_R4Z,
    perk_R2P1_A2P2,
    perk_R1M2_A1Z
  ]

-- calculation

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




