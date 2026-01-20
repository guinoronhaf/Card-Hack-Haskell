import Util.AuxiliarFunctions (setUpCards, sumCards)

import qualified Data.Map as Map
import Data.Maybe (fromJust)

main :: IO ()
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


