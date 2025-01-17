-- Função que le linhas de um arquivo
lerLinhas :: String -> IO [String] -- Entra uma o caminho do arquivo e retorna uma lista de strings
lerLinhas arquivo = do
    conteudo <- readFile arquivo -- Lê o conteúdo do arquivo
    pure (lines conteudo) -- Encapsula o resultado em um IO


-- Função que compara dois Strings
compString :: String -> String -> Int
compString [] [] = 0  -- As duas strings estão vazias
compString (x:xs) (y:ys)  -- Separa o primeiro caractere da string
    | x == y    = compString xs ys  -- Se caracteres forem iguais, continua com o resto da string
    | otherwise = 1 + compString xs ys  -- Se caracteres forem diferentes, soma 1 e continua
compString [] ys = length ys -- Se a primeira string acabar e a segunda ainda tiver caracteres
compString xs [] = length xs -- Se a segunda string acabar e a primeira ainda tiver caracteres


-- Função que compara as linhas
compLinhas :: [String] -> [String] -> [Int]
compLinhas [] [] = [] -- As duas listas estão vazias
compLinhas (x:xs) (y:ys) =
    let resultado = compString x y -- Compara as duas strings
    in resultado : compLinhas xs ys -- Recursivamente compara o resto das strings e adiciona na lista
compLinhas _ _ = [] -- Se uma lista acabar retorna uma lista vazia


-- Lista acumulada de denominadores
acumularDenominadores :: [Int] -> [Int]
acumularDenominadores xs = acumular xs 0 -- Chama a função acumular com o acumulador inicial 0
  where
    acumular [] _ = [] -- Caso base: lista vazia
    acumular (x:xs) acc -- Separa o primeiro elemento da lista 
        | x > 0     = (acc + 1) : acumular xs (acc + 1) -- Se o elemento for maior que 0, soma 1 ao acumulador e faz chamada recursiva
        | otherwise = acc : acumular xs acc -- Se o elemento for 0, faz chamada recursiva



-- Função de saída
resultados :: [Int] -> [Int] -> String
resultados [] [] = ""
resultados (x:xs) (y:ys)
    | x == 0 = "0\n" ++ resultados xs ys
    | otherwise = show (media x y) ++ "\n" ++ resultados xs ys
    where media a b = fromIntegral a / fromIntegral b


-- Função principal
main = do
    -- Entrada dos arquivos
    arq1 <- lerLinhas "arquivo1.txt"
    arq2 <- lerLinhas "arquivo2.txt"


    print arq1
    print arq2

    let nlinhas = compLinhas arq1 arq2
    print nlinhas


    -- Criação da lista de denominadores
    let denominadores = acumularDenominadores nlinhas


    -- Saída
    let saida = resultados nlinhas denominadores
    print saida