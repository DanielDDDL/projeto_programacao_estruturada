# checa se a pasta de build for criada
# executa menu principal caso tenha sido

cd ..

if [ -d build ]; then
	cd build/
	./MENU_INICIAL

	cd..
	cd scripts/
else
	echo "Diretório de execução não encontrado. Tente primeiro compilar o programa."
fi

