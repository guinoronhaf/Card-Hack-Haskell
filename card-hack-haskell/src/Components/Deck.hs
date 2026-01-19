module Components.Deck (generateDeck, removeCard, getValue) where -- exportando as funções que devem ser utilizadas no projeto

import qualified Data.Map as Map -- importando Data.Map
import Data.Maybe (isJust, fromJust) -- importando funções para trabalhar com Maybe, Just e Nothing

cardsList :: [String] -- lista com todos os ssímbolos de cartas presentes em um baralho convencional
cardsList = (map (\n -> show n) [2..10]) ++ ["J", "K", "Q", "A"]

generateDeck :: Map.Map String Int -- geração de um mapa que determina a existência de 4 cartas para cada símbolo
generateDeck = Map.fromList (map (\x -> (x, 4)) cardsList)

removeCard :: String -> Map.Map String Int -> Map.Map String Int -- remove uma carta específica do baralho, atualizando o mapa
removeCard key deckMap = do
	let f k v = if k == key && v /= 0 then
				   v - 1
				else
				   v
	Map.adjustWithKey f key deckMap

getValue :: String -> Map.Map String Int -> Int -- retorna a quantidade de cartas dado um símbolo específico
getValue key deckMap = do
	let value = Map.lookup key deckMap
	if isJust value then
		fromJust value
	else
		-1
