module Util.Game (gameLoop) where 

import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Char (toLower)
import Util.AuxiliaryFunctions (setUpCards, sumCards, verifyQauntityCards)
import Components.Button (buttonUnicode)
import Components.ProbAlgorithm as Prob
import Components.ProbInterpreter as Interpreter

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
        then do 
            putStrLn ""
            putStrLn "Calculando as probabilidades..."
            putStrLn "Analizando todas as combinações..."
            putStrLn "Análise concluída: "
            putStrLn ""
        
            -- call do algoritmo (Funcionalidade 2)
            let probTuple = Prob.calculateProbs mapCards

            -- mostrar as possibilidades (Funcionalidade 4)
            putStrLn $ Interpreter.probAnalise probTuple
        else do
            putStrLn "ERRO: quantidade de um unico numero de carta acima de 4"

    nextGame
