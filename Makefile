# Variáveis
SRC = main.hs  # Lista de arquivos fonte
OUT = main     # Nome do arquivo de saída

# Regra padrão: compilar o projeto
all: $(OUT)

# Regra para compilar o executável
$(OUT): $(SRC)
	ghc -o $(OUT) $(SRC)

# Regra para limpar os arquivos gerados
clean:
	rm -f $(OUT) *.hi *.o

# Regra para executar o programa
run: $(OUT)
	./$(OUT)

