module Components.Deck (generateDeck) where

import qualified Data.Map as Map

cardsList :: [String]
cardsList = (map (\n -> show n) [2..10]) ++ ["J", "K", "Q", "A"]

generateDeck :: Map.Map String Int
generateDeck = Map.fromList (map (\x -> (x, 4)) cardsList)
