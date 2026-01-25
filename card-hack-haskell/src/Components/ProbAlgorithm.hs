{-|
Module		: ProbAlgorithm
Description : Algoritmo principal para cálculo de probabilidades
-}
module Components.ProbAlgorithm (calculateProbs) where

import qualified Data.Map as Map 
import Data.Maybe (isJust, fromJust)
import Components.Deck as Deck
import Util.AuxiliaryFunctions as Aux

-- * Funções que retornam booleanos
-- | Verifica se uma lista de cardas configura um Blackjack.
-- Recebe uma lista de Strings @cards@ e retorna um Bool que indica ou não o Blackjack.
isBlackjack :: [String] -> Bool
isBlackjack cards = (Aux.sumCards cards) == 21

-- | Verifica se uma lista de cardas configura um estouro de Blackjack (overflow).
-- Recebe uma lista de Strings @cards@ e retorna um Bool que indica ou não o estouro.
isOverflow :: [String] -> Bool
isOverflow cards = (Aux.sumCards cards) > 21

-- * Funções que calculam probabilidades com relação ao "valor alvo" do Blackjack: 21
-- | Calcula a probabilidade de retirar uma carta do baralho e ficar abaixo dos 21 pontos.
-- Recebe uma lista de cartas @cards@ e um mapa de String para Inteiros @deck@ e calcula a probabilidade.
underflowBlackjackProb :: [String] -> Map.Map String Int -> Double
underflowBlackjackProb cards deck = underflowProb cards 21 deck

-- | Calcula a probabilidade de retirar uma carta do baralho e ficar acima dos 21 pontos.
-- Recebe uma lista de cartas @cards@ e um mapa de String para Inteiros @deck@ e calcula a probabilidade.
overflowBlackjackProb :: [String] -> Map.Map String Int -> Double
overflowBlackjackProb cards deck = overflowProb cards 21 deck

-- * Funções para cálculo de probabilidade geral
-- | Determina a probabilidade de retirar uma carta do baralho e ficar acima de um valor específico em pontos.
-- Recebe uma lista de cartas @cards@, um inteiro @limit@ e um mapa de String para Inteiros @deck@ e calcula a probabilidade.
overflowProb :: [String] -> Int -> Map.Map String Int -> Double
overflowProb cards limit deck =
	let edge = limit - (Aux.sumCards cards)
	in if edge <= 0 then
		1.0
	else
		let matchingCards = (map show [(edge + 1)..10]) ++ ["J", "K", "Q"]
		in fromIntegral (Deck.howManyCards matchingCards deck) / fromIntegral (Deck.totalCardsInDeck deck)

-- | Determina a probabilidade de retirar uma carta do baralho e ficar abaixo de um valor específico em pontos.
-- Recebe uma lista de cartas @cards@, um inteiro @limit@ e um mapa de String para Inteiros @deck@ e calcula a probabilidade.
underflowProb :: [String] -> Int -> Map.Map String Int -> Double
underflowProb cards limit deck =
	let edge = limit - (Aux.sumCards cards)
	in if edge <= 0 then
		0.0
	else
        let maxValue = edge - 1
            matchingCards = if maxValue >= 10
			                then ["J", "K", "Q", "A"] ++ map show [10,9..2]
							else map show [maxValue, maxValue-1..2] ++ ["A"]
            probCount = fromIntegral (Deck.howManyCards matchingCards deck)
            total     = fromIntegral (Deck.totalCardsInDeck deck)
        in probCount / total

-- | Determina a probabilidade de retirar uma carta do baralho e ficar exatamente com 21 pontos, configurando Blackjack.
-- Recebe uma lista de cartas @cards@ e um mapa de Strings para Inteiros @deck@ e calcula a probabilidade.
blackjackProb :: [String] -> Map.Map String Int -> Double 
blackjackProb cards deck = 
    let blackjackEdge = 21 - (Aux.sumCards cards)
        matchingCards = case blackjackEdge of
			n | n `elem` [2..9]    -> [show n]
			n | n `elem` [1, 11]   -> ["A"]
			10                     -> ["J", "K", "Q", "10"]
			_                      -> []
        probCount = fromIntegral (Deck.howManyCards matchingCards deck)
        total = fromIntegral (Deck.totalCardsInDeck deck)
    in probCount / total

avarageProbUserAboveDealerInterface :: ([String], [String]) -> (Double, Double)
avarageProbUserAboveDealerInterface tuple = avaregeProbUserAboveDealer tuple 50.0 1.0 (probUserAboveDealer tuple)

avaregeProbUserAboveDealer :: ([String], [String]) -> Double -> Double -> (Double, Double) -> (Double, Double)
avaregeProbUserAboveDealer tuple timesLimit times (getCardProb, stayProb)
    | times == timesLimit   = (Aux.truncateAt (getCardProb / timesLimit) 2, Aux.truncateAt (stayProb / timesLimit) 2)
    | otherwise             = avaregeProbUserAboveDealer tuple timesLimit (times + 1) ((getCardProb + newGetCardProb), (stayProb + newStayProb))
    where
        newProb        = probUserAboveDealer tuple
        newGetCardProb = fst newProb
        newStayProb    = snd newProb
                                

probUserAboveDealer :: ([String], [String]) -> (Double, Double)
probUserAboveDealer (userCards, dealerCards) =
    let starterDeck = Deck.generateDeck
        updatedDeck = Deck.removeCards (userCards ++ dealerCards) starterDeck
        underflowEdge = 21 - (Aux.sumCards userCards) - 1
        underflowEdgeCards = if underflowEdge >= 10 
							    then ["J", "K", "Q", "A"] ++ map show [2..10]
							    else ["A"] ++ map show [2..underflowEdge]
        randomElement = underflowEdgeCards !! (Aux.randomInt 0 ((length underflowEdgeCards) - 1))
        userCardsAfterPossiblePick = userCards ++ [randomElement]
        sumUserCardsAfterPick = Aux.sumCards userCardsAfterPossiblePick
        deckAfterUserPick = Deck.removeCards [randomElement] updatedDeck
        noPickVictoryProb = underflowProb dealerCards (Aux.sumCards userCards) updatedDeck * 100
        pickVictoryProb = if (Aux.sumCards userCards > Aux.sumCards dealerCards)
                            then (underflowBlackjackProb userCards updatedDeck * underflowProb dealerCards sumUserCardsAfterPick deckAfterUserPick + blackjackProb userCards updatedDeck) * 100
							else (overflowProb userCards (Aux.sumCards dealerCards) updatedDeck * underflowBlackjackProb userCardsAfterPossiblePick deckAfterUserPick * underflowProb dealerCards sumUserCardsAfterPick deckAfterUserPick + blackjackProb userCards updatedDeck) * 100
	in (Aux.truncateAt pickVictoryProb 2, Aux.truncateAt noPickVictoryProb 2)

-- * Função principal para cálculo de probabilidade
-- | Calcula as probabilidades de o usuário vencer a rodada pegando, ou não, uma nova carta.
-- Recebe um mapa de Strings para lista de Strings @cardsMap@ (que representa as cartas do `user` e do `dealer`) e calcula as probabilidades, retornando uma tupla de elementos do tipo Double.
calculateProbs :: Map.Map String [String] -> (Double, Double)
calculateProbs cardsMap =
    let userCards = fromJust (Map.lookup "user" cardsMap)
        dealerCards = fromJust (Map.lookup "dealer" cardsMap)
	in if isBlackjack userCards then
		(0.00, 100.00)
	else if isOverflow userCards then
		(0.00, 0.00)
	else
        avarageProbUserAboveDealerInterface (userCards, dealerCards)
