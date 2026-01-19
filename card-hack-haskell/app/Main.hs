module Main (main) where

import Components.Deck
--
-- import Text.Read (readMaybe)
-- import Control.Exception
-- import Control.Monad (unless)
--
-- -- função split
-- split :: Char -> String -> [String]
-- split _ [] = [""]
-- split sep (c:cs)
--     | c == sep = "" : rest
--     | otherwise = (c : head rest) : tail rest
--     where
--         rest = split sep cs
--
-- -- pega os valores das cartas,  em que caso seja 'A' = 11, numero = seu oroprio valor; 'J', 'Q', 'K' = 10.
-- getStrValue :: String -> Int
-- getStrValue "A" = 11
-- getStrValue s = case (readMaybe s :: Maybe Int) of
--     Just n -> n
--     Nothing -> 10 -- se for uma letra, retorna 10 -> ps: o "A" deve ser tratado de maneira especial no algoritmo 
--
-- -- realiza a soma dos valores das cartas e retornando o valor correto e ajustado, caso necessário, do usuário
-- sumCards :: [String] -> Int
-- sumCards cards =
--     let values = map getStrValue cards -- guarda a lista de valores correspondente as cartas do usuario
--         total = sum values -- soma os valores da lista de valores 
--         aces = length (filter (== "A") cards) -- filtra os Aces e pega a sua quantidade 
--     in adjustAces total aces
--
-- -- função que realiza a ultima 'varredura' para caso tenha algum 'As' e o somatorio ter passado de 21 ele reajusra o valor total (- 10).
-- adjustAces :: Int -> Int -> Int
-- adjustAces total aces
--   | total <= 21 || aces == 0 = total -- ajusta o valor total, enquanto o "A" valer 11 
--   | otherwise = adjustAces (total - 10) (aces - 1)  -- caso não entre na primeira condição, ele chama recursivamente subtraindo o valor total por 10 e diminuindo um 'As' na quantidade
--
-- validateSumValue :: Int -> Bool
-- validateSumValue value = value <= 21
--
-- main :: IO ()
-- main = do
--     putStrLn "Insira as cartas do Jogador: (separe os valores por ' ')"
--     input <- getLine
--
--     let cards      = split ' ' input
--         usersValue = sumCards cards
--         isValid    = validateSumValue usersValue
--
--     unless isValid $ throwIO (userError "Soma de valores acima de 21, envie um conjunto de cartas válido.")
--
--     putStrLn "Insira a carta do Dealer: (apenas uma e não a separe por espaços)"
--     input <- getLine
--
--     let dealersValue = getStrValue input
--
--     putStrLn ("Users value = " ++ show usersValue)
--     putStrLn ("Dealers value = " ++ show dealersValue)

main :: IO ()
main = do
	let m = generateDeck
	let m1 = removeCard "J" m
	print(getValue "X" m1)
