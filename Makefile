# Variáveis
arquivo = main.hs
target = main

# Compila o projeto
all: $(target)

$(target): $(arquivo)
	ghc -o $(target) $(arquivo)

# Limpa os arquivos gerados
clean:
	rm -f $(target) *.hi *.o

# Executa o programa
run: $(target)
	./$(target)

