------------
-- Belote --
------------

-- Data type for Ranks in a card game
data Rank = Jack | King | Queen | Ace | Ten | Nine | Eight | Seven 
    deriving (Show)

-- Data type for Suits in a card game
data Suit = Heart | Diamond | Spade | Club 
  deriving (Show)

-- TASK
-- Check if two suits are equal
suitEquals :: Suit -> Suit -> Bool
suitEquals Heart Heart = True
suitEquals Spade Spade = True
suitEquals Club Club = True
suitEquals Diamond Diamond = True
suitEquals _ _ = False

-- Record syntax for representing a Card
data Card = Card{suit :: Suit, rank :: Rank}
  deriving (Show)

-- Data type for Contracts in the Belote game
data Contract = TrumpSpade | TrumpDiamond | TrumpHeart | TrumpClub | NoTrumps | AllTrumps
  deriving (Show)

-- TASK
-- Check if a card is of a trump suit based on a given contract
isTrump :: Contract -> Card -> Bool
isTrump TrumpSpade (Card s1 r1) = suitEquals Spade s1
isTrump TrumpDiamond (Card s1 r1) = suitEquals Diamond s1
isTrump TrumpHeart (Card s1 r1) = suitEquals Heart s1
isTrump TrumpClub (Card s1 r1) = suitEquals Club s1
isTrump NoTrumps _ = False
isTrump AllTrumps _ = True

-- TASK
-- Assign a numerical power value to a card based on the contract
-- Ensure that higher power values represent stronger cards
cardPower :: Contract -> Card -> Integer
cardPower _ (Card s1 Ace) = 11
cardPower _ (Card s1 Ten) = 10
cardPower _ (Card s1 King) = 4
cardPower _ (Card s1 Queen) = 3
cardPower _ (Card s1 Seven) = 0
cardPower _ (Card s1 Eight) = 0
cardPower t (Card s1 Jack) = if isTrump t (Card s1 Jack) then 20 else 2
cardPower t (Card s1 Nine) = if isTrump t (Card s1 Jack) then 14 else 0

-- TASK
-- | A data type to describe the different ways two cards can relate, given a contract
-- See the 'sameSuit' and 'relateCards' functions below to get a better sense of how
-- you'll be producing this data type, and hence what constructors it should have
--
-- This data type exists mainly because it's useful as a tool to implement the 'fight' function
-- As such, it might be the case that your version of CardRelation is different from what I intended
--
-- The way to think about this is as follows:
-- Imagine you're in a situation where someone has played a card, and you've just played a card
-- You now need to decide which card would beat the other one
-- 'CardRelation' expresses the first thing you need to calculate in regards to the two cards
-- *before* you can start checking their 'cardPower's, e.g. is one of them a trump and so on
--
-- HINT:
-- The intended solution has 4 constructors
data CardRelation = SameSuit | TrumpBeatsNonTrump |NonTrumpSecondTrump | DifferentSuits
  deriving (Show)

-- TASK
-- Calculate whether two cards have the same suit
-- Use the CardRelation type to express the result
sameSuit :: Card -> Card -> CardRelation
sameSuit  (Card s1 _) (Card s2 _) = if suitEquals s1 s2 then SameSuit else DifferentSuits

-- TASK
-- Given a contract, calculate how two cards relate
relateCards :: Contract -> Card -> Card -> CardRelation
relateCards c1 card1 card2 
  |isTrump c1 card1 && not (isTrump c1 card2) = TrumpBeatsNonTrump
  |not (isTrump c1 card1) && isTrump c1 card2 = NonTrumpSecondTrump
  |otherwise = sameSuit card1 card2

eqCardRelation :: CardRelation -> CardRelation -> Bool
eqCardRelation TrumpBeatsNonTrump TrumpBeatsNonTrump = True
eqCardRelation SameSuit SameSuit = True
eqCardRelation DifferentSuits DifferentSuits = True
eqCardRelation NonTrumpSecondTrump NonTrumpSecondTrump = True
eqCardRelation _ _ = False

-- TASK
-- Given a contract and two cards, return the winning card
-- Assume the first card is played first
fight :: Contract -> Card -> Card -> Card
fight c1 card1 card2 =
  case relateCards c1 card1 card2 of
  SameSuit -> if cardPower c1 card1 > cardPower c1 card2 then card1 else card2
  TrumpBeatsNonTrump -> card1
  NonTrumpSecondTrump -> card2
  DifferentSuits -> card1

-- Data type for a trick, consisting of four cards
data Trick = Trick{card1 :: Card, card2 :: Card, card3 :: Card, card4 :: Card}
  deriving (Show)

-- TASK
-- Given a contract and a Trick, determine the winning card
-- Remember that the leftmost card was played first
winner :: Contract -> Trick -> Card
winner c1 (Trick card1 card2 card3 card4) = fight c1 (fight c1 card1 card2) (fight c1 card3 card3) 

-- TASK
-- Check if a Trick could have been played according to the rules of Belote
isValid :: Contract -> Trick -> Bool
isValid = undefined
