-- Função que le linhas de um arquivo
lerLinhas :: String -> IO [String] -- Entra uma o caminho do arquivo e retorna uma lista de strings
lerLinhas arquivo = do
    conteudo <- readFile arquivo -- Lê o conteúdo do arquivo
    pure (lines conteudo) -- Encapsula o resultado em um IO


-- Função que calcula a distância de Levenshtein entre duas strings
levenshtein :: String -> String -> Int
levenshtein xs ys = levMemo (length xs) (length ys)  -- Chama a função auxiliar com os comprimentos das strings
  where
    levMemo i j = table !! i !! j  -- Acessa o valor na posição (i, j) da tabela de distâncias
    
    table = [[levCell i j | j <- [0..length ys]] | i <- [0..length xs]] -- Cria uma tabela bidimensional onde cada célula calcula a distância de edição até aquela posição

    levCell 0 j = j -- Caso base: se a primeira string for vazia, a distância é o tamanho da segunda string

    levCell i 0 = i -- Caso base: se a segunda string for vazia, a distância é o tamanho da primeira string

    levCell i j -- Caso geral: compara os últimos caracteres das substrings atuais
      | xs !! (i - 1) == ys !! (j - 1) = table !! (i - 1) !! (j - 1)  -- Se forem iguais, usa o valor da célula anterior
      | otherwise = 1 + minimum [table !! (i - 1) !! j,    -- Se forem diferentes, calcula a menor distância com:
                                 table !! i !! (j - 1),    -- Deleção
                                 table !! (i - 1) !! (j - 1)]  -- Inserção



-- -- Função que compara dois Strings
-- compString :: String -> String -> Int
-- compString [] [] = 0  -- As duas strings estão vazias
-- compString (x:xs) (y:ys)  -- Separa o primeiro caractere da string
--     | x == y    = compString xs ys  -- Se caracteres forem iguais, continua com o resto da string
--     | otherwise = 1 + compString xs ys  -- Se caracteres forem diferentes, soma 1 e continua
-- compString [] ys = length ys -- Se a primeira string acabar e a segunda ainda tiver caracteres
-- compString xs [] = length xs -- Se a segunda string acabar e a primeira ainda tiver caracteres


-- Função que compara as linhas
compLinhas :: [String] -> [String] -> [Int]
compLinhas [] [] = [] -- As duas listas estão vazias
compLinhas (x:xs) (y:ys) =
    let resultado = levenshtein x y -- Compara as duas strings
    in resultado : compLinhas xs ys -- Recursivamente compara o resto das strings e adiciona na lista
compLinhas _ _ = [] -- Se uma lista acabar retorna uma lista vazia


-- Lista acumulada de erros
acumular :: [Int] -> [Int]
acumular = scanl (+) 0


-- Função de saída que retorna uma lista de floats
resultados :: [Int] -> [Int] -> [Float]
resultados [] [] = []
resultados (x:xs) (y:ys)
    | x == 0    = 0.0 : resultados xs ys  -- Se x for 0, adiciona 0.0 na lista
    | otherwise = media x y : resultados xs ys  -- Caso contrário, calcula a média e adiciona na lista
  where
    media a b = fromIntegral a / fromIntegral b  -- Converte os valores para Float e calcula a média



-- Função principal
main = do

    -- Entrada dos arquivos
    arq1 <- lerLinhas "arquivo1.txt"
    arq2 <- lerLinhas "arquivo2.txt"

    print arq1
    print arq2


    -- Compara as linhas dos arquivos
    let nlinhas = compLinhas arq1 arq2
    print nlinhas


    -- Criação da lista acumulada
    let listaAcumulada = acumular nlinhas
    let totalErros = drop 1 listaAcumulada
    print totalErros


    -- Saída
    let saida = resultados nlinhas totalErros
    mapM_ print saida