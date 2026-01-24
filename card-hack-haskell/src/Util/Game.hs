module Util.Game (gameLoop) where 

import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Char (toLower)
import Util.AuxiliaryFunctions (setUpCards, sumCards, verifyQauntityCards)
import Components.Button (buttonUnicode)
import Components.ProbAlgorithm as Prob

showResult :: (Float, Float) -> IO ()
showResult possibilites = do
    putStrLn $ "Chance de vencer ao puxar mais uma carta: " ++ show (fst possibilites)
    putStrLn $ "Chance de vencer ao não puxar nehuma carta"  ++ show (snd possibilites)

userOption :: IO ()
userOption = do
    userInput <- getLine

    case (map toLower userInput) of
        "s" -> putStrLn "Fim do programa, obrigado pela preferência. Ass: CARD-HACK"
        "j" -> do
            putStrLn "Iniciando novo jogo"
            gameLoop
        _ -> do
            putStrLn "Opção inválida"
            userOption
            
nextGame :: IO ()
nextGame = do 
    buttonUnicode "Jogar novamente (J)"
    buttonUnicode "Sair (S)"

    userOption

gameLoop :: IO ()
gameLoop = do
    buttonUnicode "Digite as suas cartas (separadas por ''):"
    usersCard <- getLine

    buttonUnicode "Digite a carta do Dealer:"
    dealersCard <- getLine

    let mapCards = setUpCards [usersCard, dealersCard]

    if verifyQauntityCards mapCards 
        then putStrLn "Quantidade Ok de cartas"
        else putStrLn "Quantidade de cartas acima de 4" 

    -- call do algoritmo (Funcionalidade 2)
    let probTuple = Prob.calculateProbs mapCards

    -- mostrar as possibilidades (Funcionalidade 4)
    -- showResult possibilidades

    -- solicitair reinicio de programa (Fucnionalidade 5)
    print(probTuple)
    nextGame
