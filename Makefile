# Variáveis
arquivo = main.hs
target = main

# Regra padrão: compilar o projeto
all: $(target)

$(target): $(arquivo)
	ghc -o $(target) $(arquivo)

# Regra para limpar os arquivos gerados
clean:
	rm -f $(target) *.hi *.o

# Regra para executar o programa
run: $(target)
	./$(target)

