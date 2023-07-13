-- Author Name: Patricia Widjojo <pwidjojo@student.unimelb.edu.au>
-- Author Student Id: 913557

-- A two-player card guessing game implementing feedback written in Haskell.

-- Detailed Summary: 
-- Given a combination of 2-4 cards from a standard card deck without Joker, 
-- the program will create guesses and feedback until this combination
-- (target) is guessed. Feedback is the format of a tuple of 5 integers 
-- covering how many correct card are also in guess, how many target cards 
-- have rank lower than the lowest rank in guess, how many cards have the same
-- ranks (each card in guess only counted once), how many cards in target have 
-- higher rank than highest rank in guess, and how many cards have the same 
-- suit (each card in guess only counted once). Completed as part of COMP90048 
-- 2023 Sem 1 Project 2 Submission. A sample call would be test "3S 4H" if we 
-- want the program to guess 3 Spade and 4 Heart.

-- Approach:
-- The initial guess will be such that the n guesses are with suits and ranks 
-- about equally distance from each other and from the top and bottom ranks. 
-- n is the size of the target. Next, combinations without replacement of n 
-- cards are created, this is the initial GameState. Since the next guesses 
-- would have feedback, from the combination list, those with different 
-- feedback to what is received would be removed and a guess taken as the 
-- middle of remaining eligible guesses. The remaining eligible guesses is the
-- updated GameState. This process would repeat until the target is guessed.

module Proj2 (feedback, initialGuess, nextGuess, GameState) where

import Card
import Data.List

-- define kind equivalents to make type declaration more readable
type Feedback = (Int, Int, Int, Int, Int)

-- gameState to hold possible combinations of size n of card left. n is the 
-- number of cards in target to be guessed
type GameState = [[Card]]

-- a list of all possible cards in order from lowest to highest
allCards = [minBound .. maxBound] :: [Card]

-- A. returns feedback as a tuple of the five feedback functions. More details
-- on the individual feedback functions are under A1-5
feedback :: [Card] -> [Card] -> Feedback
feedback target guess = (correctCards target guess, lowerRanks target guess, 
                        correctRanks target guess, higherRanks target guess,
                        correctSuits target guess)

-- A1. returns how many of the cards in target are in guess. 
correctCards :: [Card] -> [Card] -> Int
correctCards target guess = length (intersect target guess)

-- A2. returns how many cards in the answer have rank lower than the lowest
-- rank in the guess
lowerRanks :: [Card] -> [Card] -> Int
lowerRanks [] guess = 0
lowerRanks (t:ts) guess
    |rank t < gMinRank = 1 + lowerRanks ts guess
    |otherwise = lowerRanks ts guess
    where gMinRank = minimum (map rank guess)

-- A3. returns how many of the cards in the target have the same rank as a
-- card in the guess. Each card in the guess is only counted once
correctRanks :: [Card] -> [Card] -> Int
correctRanks target guess = length (tRanks \\ (tRanks \\ gRanks))
    where tRanks = map rank target
          gRanks = map rank guess

-- A4. returns how many cards in target have rank higher than the highest rank
-- in the guess
higherRanks :: [Card] -> [Card] -> Int
higherRanks [] guess = 0
higherRanks (t:ts) guess 
    |rank t > gMaxRank = 1 + higherRanks ts guess
    |otherwise = higherRanks ts guess
    where gMaxRank = maximum (map rank guess)

-- A5. returns how many of the cards in the target have the same suit as a card
-- in the guess. Each card in guess is only counted once
correctSuits :: [Card] -> [Card] -> Int
correctSuits target guess = length (tSuits \\ (tSuits \\ gSuits))
    where tSuits = map suit target
          gSuits = map suit guess

-- B. returns initial guess of size n where n is the size of target and initial 
-- GameState. Initial GameState is simply all possible combinations of size n 
-- from the full deck of cards without replacement. Helper functions are 
-- described in B1-3
initialGuess :: Int -> ([Card], GameState)
initialGuess n = (getStartCards n, getCombinations n allCards)

-- B1. returns the initial guess of size n through calculating step size such 
-- that there can be equal distance between the n cards and top and bottom 
-- ranks of allCards (full deck)
getStartCards :: Int -> [Card]
getStartCards n = getStartCardsHelper step step n
    where step = div (length allCards) (n+1)

-- B2. Helper function to B1 that gets the n cards whose index are step 
-- distance from bottom and each other
getStartCardsHelper :: Int -> Int -> Int -> [Card]
getStartCardsHelper _ _ 0 = []
getStartCardsHelper index step n = (allCards !! index) : 
                        getStartCardsHelper (index + step) step (n-1)

-- B3. returns all possible combinations of size n given a list of cards.
getCombinations :: Int -> [Card] -> [[Card]]
getCombinations 0 _ = [[]]
getCombinations _ [] = []
getCombinations n (x:xs) = map (x:) (getCombinations (n-1) xs) ++ 
                            (getCombinations n xs)

-- C. returns the next guess and updated GameState (remaining possible cards to 
-- guess). Helper functions are as described in C1-2
nextGuess :: ([Card], GameState) -> Feedback -> ([Card], GameState)
nextGuess (oldGuess, oldState) fb = (newGuess, newState)
    where newState = filter (\guess -> sameFeedback guess oldGuess fb) oldState
          newGuess = getMid newState

-- C1. returns a boolean whether the given card combination will return the 
-- same feedback to the previous guess.
sameFeedback :: [Card] -> [Card] -> Feedback -> Bool
sameFeedback target guess fb 
    | feedback target guess == fb = True
    | otherwise = False

-- C2. given a list of GameState / card combinations, returns the one in the 
-- middle.
getMid :: [[Card]] -> [Card]
getMid gs = gs !! mid
    where mid = div (length gs) 2