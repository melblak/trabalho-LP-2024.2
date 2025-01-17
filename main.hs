-- Função que le linhas de um arquivo
lerLinhas :: String -> IO [String] -- Entra uma o caminho do arquivo e retorna uma lista de strings
lerLinhas arquivo = do
    conteudo <- readFile arquivo -- Lê o conteúdo do arquivo
    pure (lines conteudo) -- Encapsula o resultado em um IO


-- Função que calcula a distância de Levenshtein entre duas strings
levenshtein :: String -> String -> Int
levenshtein xs ys = memoria (length xs) (length ys)  -- Chama a função auxiliar com os comprimentos das strings
  where
    memoria i j = matriz !! i !! j  -- Acessa o valor na posição (i, j) da tabela de distâncias
    matriz = [[celula i j | j <- [0..length ys]] | i <- [0..length xs]] -- Cria uma tabela bidimensional onde cada célula calcula a distância de edição até aquela posição
    celula 0 j = j -- Caso base: se a primeira string for vazia, a distância é o tamanho da segunda string
    celula i 0 = i -- Caso base: se a segunda string for vazia, a distância é o tamanho da primeira string
    celula i j -- Caso geral: compara os últimos caracteres das substrings atuais
      | xs !! (i - 1) == ys !! (j - 1) = matriz !! (i - 1) !! (j - 1)  -- Se forem iguais, usa o valor da célula anterior
      | otherwise = 1 + minimum [matriz !! (i - 1) !! j,    -- Se forem diferentes, calcula a menor distância com:
                                 matriz !! i !! (j - 1),    -- Deleção
                                 matriz !! (i - 1) !! (j - 1)]  -- Inserção


compLinhas :: [String] -> [String] -> [(Int, String)]
compLinhas [] [] = []  -- Caso base: ambas listas vazias
compLinhas xs [] = map (\x -> (length x, "Delecao")) xs  -- Linhas restantes de `xs` são deleções
compLinhas [] ys = map (\y -> (length y, "Insercao")) ys  -- Linhas restantes de `ys` são inserções
compLinhas (x:xs) (y:ys)
    | x == y = (0, "Igual") : compLinhas xs ys  -- Linhas iguais
    | otherwise =
        let distAtual = levenshtein x y  -- Calcula a distância de edição entre as linhas atuais
            proximoXs = if null xs then maxBound else levenshtein (head xs) y  -- Distância da próxima linha de `xs` com a linha atual de `ys`
            proximoYs = if null ys then maxBound else levenshtein x (head ys)  -- Distância da linha atual de `xs` com a próxima linha de `ys`
        in if distAtual <= proximoXs && distAtual <= proximoYs
           then (distAtual, "Alteracao") : compLinhas xs ys  -- Alteração na linha atual
           else if proximoXs < proximoYs
           then (length x, "Delecao") : compLinhas xs (y:ys)  -- Linha de `xs` deletada
           else (length y, "Insercao") : compLinhas (x:xs) ys  -- Linha de `ys` inserida



-- Lista acumulada de erros
acumular :: [Int] -> [Int]
acumular = scanl (+) 0

-- Função que extrai os primeiros elementos das tuplas
extrairPrimeiros :: [(Int, String)] -> [Int]
extrairPrimeiros = map fst

-- Função que extrai os segundos elementos das tuplas
extrairSegundos :: [(Int, String)] -> [String]
extrairSegundos = map snd


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
    let distancias = extrairPrimeiros nlinhas
    let mudancas = extrairSegundos nlinhas
    print nlinhas
    print mudancas


    -- Criação da lista acumulada
    let listaAcumulada = acumular distancias
    let totalErros = drop 1 listaAcumulada
    print totalErros


    -- Saída
    let saida = resultados distancias totalErros
    mapM_ print saida
