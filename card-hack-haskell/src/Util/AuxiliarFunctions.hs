module Util.AuxiliarFunctions (getStrValue, sumCards, adjustAces, setUpCards, verifyQauntityCards) where

import Text.Read (readMaybe)
import Control.Monad (unless)
import Data.Maybe (fromJust)
import qualified Data.Map as Map

-- função split
split :: Char -> String -> [String]
split _ [] = [""]
split sep (c:cs)
    | c == sep = "" : rest
    | otherwise = (c : head rest) : tail rest
    where
        rest = split sep cs

-- pega os valores das cartas,  em que caso seja 'A' = 11, numero = seu oroprio valor; 'J', 'Q', 'K' = 10.
getStrValue :: String -> Int
getStrValue "A" = 11
getStrValue s = case (readMaybe s :: Maybe Int) of
    Just n -> n
    Nothing -> 10 -- se for uma letra, retorna 10 -> ps: o "A" deve ser tratado de maneira especial no algoritmo 

-- realiza a soma dos valores das cartas e retornando o valor correto e ajustado, caso necessário, do usuário
sumCards :: [String] -> Int
sumCards cards =
    let values = map getStrValue cards -- guarda a lista de valores correspondente as cartas do usuario
        total = sum values -- soma os valores da lista de valores 
        aces = length (filter (== "A") cards) -- filtra os Aces e pega a sua quantidade 
    in adjustAces total aces

-- função que realiza a ultima 'varredura' para caso tenha algum 'As' e o somatorio ter passado de 21 ele reajusra o valor total (- 10).
adjustAces :: Int -> Int -> Int
adjustAces total aces
  | total <= 21 || aces == 0 = total -- ajusta o valor total, enquanto o "A" valer 11 
  | otherwise = adjustAces (total - 10) (aces - 1)  -- caso não entre na primeira condição, ele chama recursivamente subtraindo o valor total por 10 e diminuindo um 'As' na quantidade

-- enviar o input do usuario primeiro no [String]
setUpCards :: [String] -> Map.Map String [String]
setUpCards inputs =
    let cardsToArray = map (split ' ') inputs
        usersTuple = head cardsToArray
        dealersTuple = cardsToArray !! 1
    in Map.fromList [("user", usersTuple), ("dealer", dealersTuple)]

verifyQauntityCards :: Map.Map String [String] -> Bool
verifyQauntityCards m = validateQuantityTuple (fromJust (Map.lookup "user" m)) (head (fromJust (Map.lookup "dealer" m))) 
        
validateQuantityTuple :: [String] -> String -> Bool
validateQuantityTuple usersTuple dealerCard =
    let mapInput    = createMapCardsInput usersTuple
        mapAdjusted = Map.adjust (+1) dealerCard mapInput
    in filterQuantityAboveLimitCards mapAdjusted

filterQuantityAboveLimitCards :: Map.Map String Int -> Bool
filterQuantityAboveLimitCards m = Map.null (Map.filter (> 4) m)

createMapCardsInput :: [String] -> Map.Map String Int
createMapCardsInput usersTuple = Map.fromList (map (\x -> (x, (getCardQuantity usersTuple x))) usersTuple)


getCardQuantity :: [String] -> String -> Int
getCardQuantity (c:cs) x 
    | c == x    = (getCardQuantity cs x) + 1
    | cs == []  = 0  
    | otherwise = (getCardQuantity cs x) + 0
    