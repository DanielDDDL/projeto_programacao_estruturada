# compila todos os modulos do sistema e joga o resultado para a pasta build
cd ..

echo "COMPILANDO PROGRAMA DE INCLUSAO..."
cobc -free -o    build/INCLUSAO INCLUSAO.cbl

echo "COMPILANDO PROGRAMA PRINCIPAL..."
cobc -free -x -o build/MENU_INICIAL MENU_INICIAL.cbl

cd scripts/