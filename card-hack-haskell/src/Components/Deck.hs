-- exportando as funções que serão utilizadas no projeto
module Components.Deck (generateDeck, removeCard, getValue) where 

-- importando Data.Map para trabalhar com mapas
import qualified Data.Map as Map
-- importanto Data.Maybe para trabalhar com os tipos Maybe, Just e Nothing
import Data.Maybe (isJust, fromJust) 

-- lista com todas as cartas (símbolos) presentes em um baralho convencional
cardsList :: [String]
cardsList = (map (\n -> show n) [2..10]) ++ ["J", "K", "Q", "A"]

-- geração que uma lista de tuplas que associam cada carta à sua quantidade inicial no baralho (4)
generateDeck :: Map.Map String Int 
generateDeck = Map.fromList (map (\x -> (x, 4)) cardsList)

-- remolção de um carta específica do baralho, atualizando o mapa
removeCard :: String -> Map.Map String Int -> Map.Map String Int 
removeCard key deckMap = do
	let f k v = if k == key && v /= 0 then
				   v - 1
				else
				   v
	Map.adjustWithKey f key deckMap

-- retorna a quantidade de cartas de um determinado símbolo no baralho
getValue :: String -> Map.Map String Int -> Int 
getValue key deckMap = do
	let value = Map.lookup key deckMap
	if isJust value then
		fromJust value
	else
		-1
