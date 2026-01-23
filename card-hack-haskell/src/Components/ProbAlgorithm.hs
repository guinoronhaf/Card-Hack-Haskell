module Components.ProbAlgorithm where

import qualified Data.Map as Map -- trabalhar com mapas
import Data.Ratio as Ratio -- trabalhar precisamente com números em ponto flutuante
import Components.Deck as Deck -- instanciar o mapa que representa o baralho
import Util.AuxiliarFunctions as Aux -- utilizar funções auxiliares para compor o jogo
import Data.Maybe (isJust, fromJust) -- trabalhar com Just nos mapas

totalCardsInDeck :: Map.Map String Int -> Int
totalCardsInDeck deck = Deck.howManyCards ((map (\x -> show x) [2..10]) ++ ["J", "K", "Q", "A"]) deck

updateDeck :: [String] -> Map.Map String Int -> Map.Map String Int
updateDeck cards deck = Deck.removeCards cards deck

isBlackjack :: [String] -> Bool
isBlackjack cards = (Aux.sumCards cards) == 21

isOverflow :: [String] -> Bool
isOverflow cards = (Aux.sumCards cards) > 21

underflowBlackjackProb :: [String] -> Map.Map String Int -> Double
underflowBlackjackProb cards deck = underflowProb cards 21 deck

overflowBlackjackProb :: [String] -> Map.Map String Int -> Double
overflowBlackjackProb cards deck = overflowProb cards 21 deck

overflowProb :: [String] -> Int -> Map.Map String Int -> Double
overflowProb cards limit deck = do
	let edge = limit - (Aux.sumCards cards)
	if edge <= 0 then
		1.0
	else do
		let minValue = edge + 1
		let matchingCards = (map (\x -> show x) [edge..10]) ++ ["J", "K", "Q"]
		fromIntegral (Deck.howManyCards matchingCards deck) / fromIntegral (totalCardsInDeck deck)

underflowProb :: [String] -> Int -> Map.Map String Int -> Double
underflowProb cards limit deck = do
	let edge = limit - (Aux.sumCards cards)
	if edge <= 0 then
		0.0
	else do
		let maxValue = edge - 1
		let matchingCards = if maxValue >= 10 then
								["J", "K", "Q"] ++ (map (\x -> show x) [10,9..2])
							else
								(map (\x -> show x) [maxValue,maxValue-1..2])
		fromIntegral (Deck.howManyCards matchingCards deck) / fromIntegral (totalCardsInDeck deck)

blackjackProb :: [String] -> Map.Map String Int -> Double 
blackjackProb cards deck = do
	let blackjackEdge = 21 - (Aux.sumCards cards)
	let matchingCards = if blackjackEdge `elem` [2..9] then
		                    [show blackjackEdge]
						else if blackjackEdge `elem` [1, 11] then
						    ["A"]
						else if blackjackEdge == 10 then
						    ["J", "K", "Q", "10"]
						else []
	fromIntegral (Deck.howManyCards matchingCards deck) / fromIntegral (totalCardsInDeck deck)

calculateProbs :: Map.Map String [String] -> (Double, Double)
calculateProbs cardsMap = do
	let userCards = fromJust (Map.lookup "user" cardsMap)
	let dealerCards = fromJust (Map.lookup "dealer" cardsMap)
	if isBlackjack userCards then
		(0.0, 1.0)
	else if isOverflow userCards then
		(0.0, 0.0)
	else (1, 2)
