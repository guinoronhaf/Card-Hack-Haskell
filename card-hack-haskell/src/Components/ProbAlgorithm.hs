module Components.ProbAlgorithm (blackjackProb) where

import qualified Data.Map as Map -- trabalhar com mapas
import Data.Ratio as Ratio -- trabalhar precisamente com números em ponto flutuante
import Components.Deck as Deck -- instanciar o mapa que representa o baralho
import Util.AuxiliarFunctions as Aux -- utilizar funções auxiliares para compor o jogo

isBlackjack :: [String] -> Bool
isBlackjack cards = (Aux.sumCards cards) == 21

isOverflow :: [String] -> Bool
isOverflow cards = (Aux.sumCards cards) > 21

blackjackProb :: [String] -> Map.Map String Int -> Double 
blackjackProb cards deck = do
	let sum = Aux.sumCards cards
	let diff = 21 - sum
	let total =  Deck.howManyCards ((map (\x -> (show x)) [2..10]) ++ ["J", "Q", "K", "A"]) deck
	let qtdeDiff = if diff `elem` [2..9] then
					Deck.getValue (show diff) deck
				   else if diff `elem` [1, 11] then
				   	Deck.getValue "A" deck
				   else if diff == 10 then
				    Deck.howManyCards ["J", "K", "Q", "10"] deck
				   else 0
	fromIntegral qtdeDiff / fromIntegral total

-- underflowProb :: [String] -> Map.Map String Int -> Ratio
-- underflowProb cards deck = do
-- 	let sum = Aux.sumCards cards
-- 	let diff = 20 - sum
-- 	let qtdeDiff = if diff `elem` [2..9] then
-- 					Deck.getValue (show diff) deck
-- 				   else if diff `elem` [1, 11] then
-- 				   	Deck.getValue "A" deck
-- 				   else
-- 				    Deck.getValue "J" deck + Deck.getValue "Q" deck + Deck.getValue "K" deck
--
-- overflowProb :: [String] -> Map.Map String Int -> Ratio
-- overflowProb cards deck = do
-- 	let sum = Aux.sumCards cards

-- getProb

testFunc :: String
testFunc = "oi"
