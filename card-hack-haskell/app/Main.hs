-- import Util.AuxiliarFunctions (setUpCards, sumCards)
--
-- import qualified Data.Map as Map
-- import Data.Char (toUpper)
--
-- main :: IO ()
-- main = do
--     gameLoop
--
-- gameLoop :: IO ()
-- gameLoop = do
--     putStrLn "Digite as suas cartas (separadas por ''):"
--     usersCard <- getLine
--
--     putStrLn "Digite a carta do Dealer:"
--     dealersCard <- getLine
--
--     let mapCards = setUpCards [usersCard, dealersCard]
--
--     -- call do algoritmo (Funcionalidade 2)
--
--     -- mostrar as possibilidades (Funcionalidade 4)
--     
--     -- solicitair reinicio de programa (Fucnionalidade 5)
--     restartButton
--
-- restartButton :: IO ()
-- restartButton = do
--     putStrLn "Deseja reiniciar o programa? (Y / N)"
--     input <- getLine
--
--     case restartCases input of 
--         'Y' -> gameLoop
--         'R' -> restartButton
--         'N' -> putStrLn "Fim do programa, obrigado pela preferência. Ass: CARD-HACK"
--
--
-- restartCases :: String -> Char
-- restartCases input = 
--     case (map toUpper input) of 
--         "Y" -> 'Y' 
--         "N" -> 'N'
--         _   -> 'R' -- input invalido, refazer

import Components.Deck as Deck
import Components.ProbAlgorithm as Prob
import qualified Data.Map as Map
import Data.Maybe (fromJust)

main :: IO ()
main = do
	let m = Map.fromList [("user", ["J", "J", "2"]), ("dealer", ["A"])]
	print(Prob.calculateProbs m)
