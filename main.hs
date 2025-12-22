split :: Char -> String -> [String]
split _ [] = [""]
split sep (c:cs) 
    | c == sep = "" : rest
    | otherwise = (c : head rest) : tail rest
    where 
        rest = split sep cs

main :: IO()
main = do 
    putStrLn "Insira as cartas do Jogador: (separe os valores por ' ')"
    usersCardsInput <- getLine
    let usersCards = split ' ' usersCardsInput -- arrays de valores 
    putStrLn "Insira a carta do Dealer: "
    dealersCard <- getLine -- string unica (apenas um valor)
    -- print apenas para não ter erro de compilação 
    print usersCards
    print dealersCard