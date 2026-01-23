module Components.ProbAlgorithm (calculateProbs) where

import qualified Data.Map as Map -- trabalhar com mapas
import Data.Ratio as Ratio -- trabalhar precisamente com números em ponto flutuante
import Components.Deck as Deck -- instanciar o mapa que representa o baralho
import Util.AuxiliarFunctions as Aux -- utilizar funções auxiliares para compor o jogo
import Data.Maybe (isJust, fromJust) -- trabalhar com Just nos mapas
import System.Random (randomRIO)
import System.IO.Unsafe (unsafePerformIO)

randomInt :: Int -> Int -> Int
randomInt low high = unsafePerformIO $ randomRIO (low, high)

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
								(map (\x -> show x) [maxValue,maxValue-1..2]) ++ ["A"]
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
	let starterDeck = Deck.generateDeck
	let userCards = fromJust (Map.lookup "user" cardsMap)
	let dealerCards = fromJust (Map.lookup "dealer" cardsMap)
	if isBlackjack userCards then
		(0.0, 1.0)
	else if isOverflow userCards then
		(0.0, 0.0)
	else do
		let diff = 21 - (Aux.sumCards userCards) - 1
		let possibilities = if diff >= 10 then
								["J", "K", "Q", "A"] ++ (map (\x -> show x) [2..10])
							else
								["A"] ++ (map (\x -> show x) [2..diff])
		let randomIdx = randomInt 0 ((length possibilities) - 1)
		let randomElement = possibilities !! randomIdx
		let newUserCards = userCards ++ [randomElement]
		let newUserSum = Aux.sumCards newUserCards
		let updatedDeck = Deck.removeCards (userCards ++ dealerCards) starterDeck
		let probGanharSemPuxar = underflowProb dealerCards (Aux.sumCards userCards) updatedDeck
		let probGanharPuxando = blackjackProb userCards updatedDeck + (underflowBlackjackProb userCards updatedDeck * underflowProb dealerCards newUserSum (Deck.removeCards [randomElement] updatedDeck))
		(probGanharPuxando, probGanharSemPuxar)
