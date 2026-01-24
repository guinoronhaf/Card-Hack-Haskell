module Components.Deck (generateDeck, removeCards, howManyCards, totalCardsInDeck) where 

import qualified Data.Map as Map
import Data.Maybe (isJust, fromJust) 

cardsList :: [String]
cardsList = (map show [2..10]) ++ ["J", "K", "Q", "A"]

generateDeck :: Map.Map String Int 
generateDeck = Map.fromList (map (\x -> (x, 4)) cardsList)

removeCards :: [String] -> Map.Map String Int -> Map.Map String Int 
removeCards [] deckMap = deckMap
removeCards (x:xs) deckMap = do
	let f k v = if k == x && v /= 0 then
				   v - 1
				else
				   v
	let newDeck = Map.adjustWithKey f x deckMap
	removeCards xs newDeck

getByKey :: String -> Map.Map String Int -> Int 
getByKey key deckMap = do
	let value = Map.lookup key deckMap
	if isJust value then
		fromJust value
	else
		-1

howManyCards :: [String] -> Map.Map String Int -> Int
howManyCards [] _ = 0
howManyCards (x:xs) deckMap = (getByKey x deckMap) + howManyCards xs deckMap

totalCardsInDeck :: Map.Map String Int -> Int
totalCardsInDeck deckMap = howManyCards cardsList deckMap
