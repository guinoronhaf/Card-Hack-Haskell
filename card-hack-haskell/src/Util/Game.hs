module Util.Game (gameLoop) where 

import Data.Char (toLower)
import Util.AuxiliaryFunctions (setUpCards, verifyQauntityCards)
import Components.Button (buttonUnicode)
import Components.ProbAlgorithm as Prob
import Components.ProbInterpreter as Interpreter
import Control.Concurrent (threadDelay)

-- Lê a opção do usuário após o término de uma rodada
-- permitindo iniciar um novo jogo ou encerrar o programa
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

-- Exibe as opções disponíveis ao usuário após o fim de uma rodada
nextGame :: IO ()
nextGame = do 
    buttonUnicode "Jogar novamente (J)"
    buttonUnicode "Sair (S)"

    userOption

-- Controla o fluxo principal do jogo, realizando a leitura das cartas,
-- validações, cálculo das probabilidades e exibição do resultado
gameLoop :: IO ()
gameLoop = do
    putStrLn ""
    buttonUnicode "Digite as suas cartas (separadas por ''):"
    usersCard <- getLine

    buttonUnicode "Digite a carta do Dealer:"
    dealersCard <- getLine

    let mapCards = setUpCards [usersCard, dealersCard]

    if verifyQauntityCards mapCards
        then do 
            putStrLn ""
            putStrLn "Calculando as probabilidades..."
            
            threadDelay 2000000
            
            putStrLn "Analisando todas as combinações..."
            
            threadDelay 2000000

            putStrLn "Análise concluída: "
            putStrLn ""
        
            let probTuple = Prob.calculateProbs mapCards
            putStrLn $ Interpreter.probAnalise probTuple
        else do
            putStrLn "ERRO: quantidade de um unico numero de carta acima de 4"

    nextGame

