import Util.AuxiliarFunctions (setUpCards, sumCards)
import Components.Button (buttonUnicode)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Char (toLower)

showResult :: [Float] -> ()
showResult possibilites = do
    putStrLn $ "Chance de vencer ao puxar mais uma carta: " ++ show (fst possibilites)
    putStrLn $ "Chance de vencer ao não puxar nehuma carta"  ++ show (snd possibilites)

userOption :: IO ()
userOption = do
    userInput <- getLine

    case (map toLower userInput) of
        "s" -> putStrLn "Encerrando o jogo..."
        "j" -> do
            putStrLn "Iniciando novo jogo"
            setUpCards
        _ -> do
            putStrLn "Opção inválida"
            userOption
            
nextGame :: IO ()
nextGame = do 
    buttonUnicode "Jogar novamente (J)"
    buttonUnicode "Sair (S)"

    userOption

main = do
    putStrLn "Digite as suas cartas (separadas por ''):"
    usersCard <- getLine

    putStrLn "Digite a carta do Dealer:"
    dealersCard <- getLine

    let mapCards = setUpCards [usersCard, dealersCard]
        arrayUsersCards = fromJust (Map.lookup "user" mapCards)
        arrayDealersCard = fromJust (Map.lookup "dealer" mapCards)


    putStrLn $ "Cartas do usuario: " ++ show arrayUsersCards
    putStrLn $ "Cartas do dealer "  ++ show arrayDealersCard

    putStrLn $ "A soma do usuario: " ++ show (sumCards arrayUsersCards)
    putStrLn $ "A soma do dealer: " ++ show (sumCards arrayDealersCard)




