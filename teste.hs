-- Função que compara dois Strings
compString :: String -> String -> Int
compString [] [] = 0  -- As duas strings estão vazias
compString (x:xs) (y:ys)  -- Separa o primeiro caractere da string
    | x == y    = compString xs ys  -- Se caracteres forem iguais, continua com o resto da string
    | otherwise = 1 + compString xs ys  -- Se caracteres forem diferentes, soma 1 e continua
compString _ [] = 1 + compString [] ys -- Se a primeira string acabar e a segunda ainda tiver caracteres
compString [] _ = 1 + compString xs [] --Se a segunda string acabar e a primeira ainda tiver caracteres

main = do
    
    let teste = compString "passaro" "passarinho"
    print teste