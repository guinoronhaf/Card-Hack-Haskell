import qualified Data.Map.Strict as HM
import Text.Read (readMaybe)
import Data.IntMap (adjust)

split :: Char -> String -> [String]
split _ [] = [""]
split sep (c:cs) 
    | c == sep = "" : rest
    | otherwise = (c : head rest) : tail rest
    where
        rest = split sep cs 

getStrValue :: String -> Int
getStrValue "A" = 11 
getStrValue s = case (readMaybe s :: Maybe Int) of
    Just n -> n
    Nothing -> 10 -- se for uma letra, retorna 10 -> ps: o "A" deve ser tratado de maneira especial no algoritmo 

sumCards :: [String] -> Int
sumCards cards = 
    let values = map getStrValue cards
        total = sum values 
        aces = length (filter (== "A") cards)
    in adjustAces total aces 

adjustAces :: Int -> Int -> Int
adjustAces total aces
  | total <= 21 || aces == 0 = total -- ajusta o valor total, enquanto o "A" valer 11 
  | otherwise = adjustAces (total - 10) (aces - 1)

main :: IO()
main = do 
    putStrLn "Insira as cartas do Jogador: (separe os valores por ' ')"
    usersCardsInput <- getLine
    let usersCards = split ' ' usersCardsInput -- arrays de valores
    -- print apenas para não ter erro de compilação
    let sumValues = sumCards usersCards 
    print sumValues