       
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXCLUSAO.
       AUTHOR. DANIEL.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

            SPECIAL-NAMES.      
                 DECIMAL-POINT IS COMMA.

             INPUT-OUTPUT SECTION.
               FILE-CONTROL.
                   SELECT OPTIONAL CAD-PRODUTO
                   ASSIGN TO "livros.dat"
                   ORGANIZATION INDEXED
                   RECORD KEY IS CODPROD
                   ACCESS RANDOM
                   FILE STATUS IS CODERRO.

       DATA DIVISION.
         
         FILE  SECTION.
         FD  CAD-PRODUTO
             LABEL  RECORD  STANDARD.
         01  REG-PRODUTO.
             02  CODPROD        PIC  9(3).
             02  DESCRI         PIC  X(20).
             02  PRECO          PIC  9(4)V99.
             02  FILLER         PIC  X(41).

         WORKING-STORAGE SECTION.
         77  CODERRO            PIC X(2)     VALUE SPACES.
         77  OPC                PIC X        VALUE SPACE.
             88  OPC-OK                      VALUE "S" "N".
         77  W-CODPROD-PESQUISA PIC 9(3)     VALUE ZEROS.
         77  CODPROD-ED         PIC ZZ9      VALUE ZEROS.
         77  OPC-EXCL           PIC X        VALUE SPACE.
         77  PRECO-ED           PIC Z.ZZ9,99 VALUE ZEROS.
  	
         01  DATA-SIS.
             02  ANO  PIC  9999.
             02  MES  PIC  99.
             02  DIA  PIC  99.
         01  DATA-DIA.
             02  DIA  PIC  99/.
             02  MES  PIC  99/.
             02  ANO  PIC  9999.
         01  DATA-COM-BARRA  REDEFINES  DATA-DIA  PIC X(10).

         SCREEN SECTION.
            01 CLEAR-SCREEN.
               05 BLANK SCREEN BACKGROUND-COLOR 0 FOREGROUND-COLOR 0.
       
       PROCEDURE DIVISION.

       INICIO.
           PERFORM  INICIALIZACAO.
           PERFORM  PROCESSAMENTO UNTIL OPC = "N".
	         PERFORM  FINALIZACAO.
           EXIT PROGRAM.

       INICIALIZACAO.
           INITIALIZE DATA-SIS.
           ACCEPT  DATA-SIS FROM DATE YYYYMMDD.
           PERFORM ABRIR-ARQUIVO.

       PROCESSAMENTO.
           PERFORM   FORMATAR-TELA.
           PERFORM   ROTINA-LEITURA
           PERFORM   ROTINA-DELECAO.
           PERFORM   RECEBER-OPCAO-CONTINUIDADE.

       FORMATAR-TELA.
           INITIALIZE DATA-DIA CODERRO REG-PRODUTO W-CODPROD-PESQUISA.
           MOVE ZEROS TO CODPROD-ED   PRECO-ED.
           MOVE CORR DATA-SIS TO DATA-DIA.
           DISPLAY  CLEAR-SCREEN.
	         DISPLAY  "EXCLUSAO DE CADASTRO DE PRODUTOS" AT 0515.
           DISPLAY   DATA-DIA AT 0722.
	         DISPLAY  "CODIGO:" AT 1010.
           DISPLAY  "DESCRICAO:" AT 1210.
           DISPLAY  "PRECO UNIT.:" AT 1410.
           DISPLAY  "OUTRO REGISTRO?(S/N):" AT 2010.

       ROTINA-LEITURA.
      
           DISPLAY  "DIGITE O CODIGO DO PRODUTO A EXLUIR" AT 0910
           ACCEPT CODPROD-ED  AT 1022.
           MOVE CODPROD-ED  TO  CODPROD    
           READ CAD-PRODUTO
           IF  CODERRO NOT = "00"
               DISPLAY "PRODUTO NAO FOI ENCONTRADO" AT 1040 WITH BLINK
           ELSE
               DISPLAY DESCRI AT 1222
               MOVE  PRECO   TO   PRECO-ED
               DISPLAY  PRECO-ED   AT  1422
           END-IF.
       ROTINA-DELECAO.
            IF  CODERRO = "00"
                DISPLAY  "CONFIRMA A EXCLUSAO?(S/N):"  AT  1810
                ACCEPT   OPC-EXCL AT 1840
		            IF  OPC-EXCL  =  "S"
  			           DELETE  CAD-PRODUTO      
                ELSE
			             DISPLAY  "EXCLUSAO NAO EFETIVADA"  AT 1844
                   STOP  "<ENTER> PARA CONTINUAR"
                END-IF
             ELSE
                NEXT  SENTENCE
             END-IF.
      
       RECEBER-OPCAO-CONTINUIDADE.
           PERFORM WITH TEST AFTER UNTIL OPC-OK
               ACCEPT OPC  AT  2035 WITH AUTO
               MOVE FUNCTION UPPER-CASE (OPC) TO OPC
               IF  OPC-OK
                   DISPLAY " " AT 2040
               ELSE
                   DISPLAY "DIGITE S OU N" AT 2040
               END-IF
           END-PERFORM.
       
       ABRIR-ARQUIVO.
           *> SE ARQUIVO NAO FOR ENCONTRADO, UM NOVO SERA CRIADO
           OPEN  I-O  CAD-PRODUTO.

       FINALIZACAO.
           CLOSE  CAD-PRODUTO
           DISPLAY "FIM DE PROCESSAMENTO" AT 2455.
           STOP  " ".

       FIM-ULTIMA-LINHA.