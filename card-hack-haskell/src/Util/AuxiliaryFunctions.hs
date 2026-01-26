{-|
Module		: AuxiliaryFunctions
Description : Conjunto de funções auxiliares de uso do sistema
-}
module Util.AuxiliaryFunctions (getStrValue, sumCards, adjustAces, setUpCards, verifyQauntityCards, truncateAt, randomInt) where

import Text.Read (readMaybe)
import Data.Maybe (fromJust)
import qualified Data.Map as Map
import System.Random (randomRIO)
import System.IO.Unsafe (unsafePerformIO)

-- * Funções meramente utilitárias, sem relação com os símbolos das cartas do baralho.
-- | Trunca valores decimais a partir de um número específico de casas decimais.
-- Retorna um valor decimal @x@ devidamente truncado em @n@ casas decimais.
truncateAt :: Double -> Int -> Double
truncateAt x n = fromIntegral (floor (x * (10 ^ n))) / (10 ^ n)

-- | Gera um valor pseudoaleatório do tipo Inteiro.
-- Retorna um Inteiro a partir de dois valores que norteiam o intervalo: @low@ e @high@
randomInt :: Int -> Int -> Int
randomInt low high = unsafePerformIO $ randomRIO (low, high)

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

-- Processa os inputs do usuário e do dealer, separando as cartas
-- e organizando-as em um Map com as chaves "user" e "dealer"
setUpCards :: [String] -> Map.Map String [String]
setUpCards inputs =
    let cardsToArray = map (split ' ') inputs
        usersTuple = head cardsToArray
        dealersTuple = cardsToArray !! 1
    in Map.fromList [("user", usersTuple), ("dealer", dealersTuple)]

-- Verifica se a quantidade total de cartas do usuário e do dealer
-- não ultrapassa o limite permitido no baralho
verifyQauntityCards :: Map.Map String [String] -> Bool
verifyQauntityCards m =
    validateQuantityTuple
        (fromJust (Map.lookup "user" m))
        (head (fromJust (Map.lookup "dealer" m))) 

-- Valida se a combinação das cartas do usuário com a carta do dealer
-- respeita o limite máximo de ocorrências por carta
validateQuantityTuple :: [String] -> String -> Bool
validateQuantityTuple usersTuple dealerCard =
    let mapInput    = createMapCardsInput usersTuple
        mapAdjusted = Map.adjust (+1) dealerCard mapInput
    in filterQuantityAboveLimitCards mapAdjusted

-- Verifica se nenhuma carta ultrapassa o limite máximo permitido (4)
-- retornando True caso esteja tudo válido
filterQuantityAboveLimitCards :: Map.Map String Int -> Bool
filterQuantityAboveLimitCards m =
    Map.null (Map.filter (> 4) m)

-- Cria um Map onde cada carta do usuário é associada
-- à quantidade de vezes que ela aparece na mão
createMapCardsInput :: [String] -> Map.Map String Int
createMapCardsInput usersTuple =
    Map.fromList (map (\x -> (x, getCardQuantity usersTuple x)) usersTuple)

-- Conta quantas vezes uma carta específica aparece na lista de cartas
getCardQuantity :: [String] -> String -> Int
getCardQuantity cards x =
    length (filter (== x) cards)

