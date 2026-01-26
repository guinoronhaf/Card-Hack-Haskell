{- HLINT ignore "Redundant bracket" -}
module Components.Deck (generateDeck, removeCards, howManyCards, totalCardsInDeck) where 

import qualified Data.Map as Map
import Data.Maybe (isJust, fromJust) 

-- Lista de cartas do baralho:
-- números de 2 a 10 convertidos para String
-- mais as cartas de figura e o Ás
cardsList :: [String]
cardsList = (map show [2..10]) ++ ["J", "K", "Q", "A"]

-- Gera um baralho inicial representado por um Map
-- cada carta começa com quantidade 4
generateDeck :: Map.Map String Int 
generateDeck = Map.fromList (map (\x -> (x, 4)) cardsList)

-- Remove cartas do baralho com base em uma lista de cartas
-- decrementa 1 da quantidade da carta, se ela existir e não for zero
removeCards :: [String] -> Map.Map String Int -> Map.Map String Int 
removeCards [] deckMap = deckMap
removeCards (x:xs) deckMap = do
    -- Função auxiliar que ajusta o valor da carta no Map
    let f k v =
            if k == x && v /= 0 then
                v - 1
            else
                v

    -- Atualiza o baralho removendo uma ocorrência da carta x
    let newDeck = Map.adjustWithKey f x deckMap

    -- Continua removendo as próximas cartas da lista
    removeCards xs newDeck

-- Obtém o valor associado a uma chave no Map
-- retorna -1 caso a carta não exista
getByKey :: String -> Map.Map String Int -> Int 
getByKey key deckMap = do
    let value = Map.lookup key deckMap
    if isJust value then
        fromJust value
    else
        -1

-- Soma a quantidade de cartas restantes no baralho
-- com base em uma lista de cartas
howManyCards :: [String] -> Map.Map String Int -> Int
howManyCards [] _ = 0
howManyCards (x:xs) deckMap =
    getByKey x deckMap + howManyCards xs deckMap

-- Calcula o total de cartas restantes no baralho
-- usando todas as cartas possíveis (cardsList)
totalCardsInDeck :: Map.Map String Int -> Int
totalCardsInDeck deckMap =
    howManyCards cardsList deckMap