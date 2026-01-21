module Components.ProbAlgorithm (testFunc) where

import qualified Data.Map as Map -- trabalhar com mapas
import Data.Ratio as Ratio -- trabalhar precisamente com números em ponto flutuante
import Components.Deck as Deck -- instanciar o mapa que representa o baralho
import Util.AuxiliarFunctions as Aux -- utilizar funções auxiliares para compor o jogo

isBlackJack :: [String] -> Bool
isBlackJack cards = (Aux.sumCards cards) == 21

isOverflow :: [String] -> Bool
isOverflow cards = (Aux.sumCards cards) > 21

-- blackjackProb :: [String] -> Map.Map String Int -> Ratio

-- underflowProb :: [String] -> Map.Map String Int -> Ratio

-- overFlowProb :: [String] -> Map.Map String Int -> Ratio

testFunc :: String
testFunc = "oi"
