# compila todos os modulos do sistema e joga o resultado para a pasta build
cd ..

# criando diretorio de execucao caso nao exista
mkdir -p build

echo "COMPILANDO PROGRAMA DO CABECALHO..."
cobc -free -o    build/CABECALHO CABECALHO.cbl

echo "COMPILANDO PROGRAMA DE INCLUSAO..."
cobc -free -o    build/INCLUSAO INCLUSAO.cbl

echo "COMPILANDO PROGRAMA DE LEITURA..."
cobc -free -o 	 build/LEITURA LEITURA.cbl

echo "COMPILANDO PROGRAMA DE ALTERACAO..."
cobc -free -o   build/ALTERAR ALTERAR.cbl

echo "COMPILANDO PROGRAMA DE EXCLUSAO..."
cobc -free -o   build/EXCLUSAO EXCLUSAO.cbl

echo "COMPILANDO PROGRAMA PRINCIPAL..."
cobc -free -x -o build/MENU_INICIAL MENU_INICIAL.cbl

echo "Espero que nao tenha dado nenhum erro :b"

cd scripts/