import Util.AuxiliarFunctions (setUpCards, sumCards)
import Components.Button (buttonUnicode)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Char (toLower)

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

    -- call do algoritmo (Funcionalidade 2)

    -- mostrar as possibilidades (Funcionalidade 4)
    -- showResult possibilidades

    -- solicitair reinicio de programa (Fucnionalidade 5)
    nextGame

-- restartButton :: IO ()
-- restartButton = do
--     putStrLn "Deseja reiniciar o programa? (Y / N)"
--     input <- getLine

--     case restartCases input of 
--         'Y' -> gameLoop
--         'R' -> restartButton
--         'N' -> putStrLn "Fim do programa, obrigado pela preferência. Ass: CARD-HACK"


-- restartCases :: String -> Char
-- restartCases input = 
--     case (map toUpper input) of 
--         "Y" -> 'Y' 
--         "N" -> 'N'
--         _   -> 'R' -- input invalido, refazer

main = do
    gameLoop

