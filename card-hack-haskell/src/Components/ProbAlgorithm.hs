module Components.ProbAlgorithm (blackjackProb) where

import qualified Data.Map as Map -- trabalhar com mapas
import Data.Ratio as Ratio -- trabalhar precisamente com números em ponto flutuante
import Components.Deck as Deck -- instanciar o mapa que representa o baralho
import Util.AuxiliarFunctions as Aux -- utilizar funções auxiliares para compor o jogo

totalCardsInDeck :: Map.Map String Int -> Int
totalCardsInDeck deck = Deck.howManyCards ((map (\x -> show x) [2..9]) ++ ["J", "K", "Q", "A"]) deck

isBlackjack :: [String] -> Bool
isBlackjack cards = (Aux.sumCards cards) == 21

isOverflow :: [String] -> Bool
isOverflow cards = (Aux.sumCards cards) > 21

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

underflowProb :: [String] -> Map.Map String Int -> Double
underflowProb cards deck = do
	let underflowEdge = 20 - (Aux.sumCards cards)
	let total = totalCardsInDeck deck
	let qtdeMatchingCards = if underflowEdge <= 0 then
					0
				   else if underflowEdge == 11 then
				   	Deck.howManyCards ["A"] deck
				   else if underflowEdge `elem` [2..9] then
					Deck.howManyCards ((map (\x -> (show x)) [2..underflowEdge]) ++ ["A"]) deck
				   else total
	fromIntegral qtdeMatchingCards / fromIntegral total


overflowProb :: [String] -> Map.Map String Int -> Double
overflowProb cards deck = do
	let overflowEdge = 22 - (Aux.sumCards cards)
	let total =  Deck.howManyCards ((map (\x -> (show x)) [2..10]) ++ ["J", "Q", "K", "A"]) deck
	let qtdeMatchingCards = if overflowEdge > 10 then
					0
				   else if overflowEdge `elem` [2..10] then
					Deck.howManyCards ((map (\x -> (show x)) [overflowEdge..10]) ++ ["J", "Q", "K"]) deck
				   else total
	fromIntegral qtdeMatchingCards / fromIntegral total
