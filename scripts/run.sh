# executa o main do programa 
cd ..

if [ ! -d build ]; then
  echo "Diretorio de compilacao nao encontrado. Por favor, execute o comando './compile' para que ele seja criado."
else
	cd build/

	./MENU_INICIAL

	cd ..
	cd scripts/
fi


