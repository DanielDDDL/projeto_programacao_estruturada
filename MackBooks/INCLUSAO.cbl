       IDENTIFICATION DIVISION.
       PROGRAM-ID. INCLUSAO.
       AUTHOR. DANIEL.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           SPECIAL-NAMES.      
               DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQ-PROD ASSIGN TO "C:\TEMP\LIVROS.DAT"
               ORGANIZATION INDEXED
               RECORD KEY COD-PROD
               ACCESS RANDOM
               FILE STATUS IS W-COD-ERRO.
       DATA DIVISION.
       FILE SECTION.
       FD  ARQ-PROD
           LABEL RECORD STANDARD.
       01  REG-PROD.
           02  COD-PROD    PIC 9(3).
           02  DESCRI-PROD PIC X(20).
           02  PRECO-PROD  PIC 9(4)V99.
           02  FILLER      PIC X(41).
       
       WORKING-STORAGE SECTION.
       01  W-COD-ERRO          PIC XX VALUE SPACES.
       01  W-OPCAO             PIC X  VALUE SPACE.
       01  W-INCLUI            PIC X  VALUE SPACE.
       01  W-BRANCO            PIC X(50) VALUE SPACE.
       01	 MASCARAS.
            02   COD-ED	PIC ZZ9  VALUE ZEROS.
            02   PRECO-ED    PIC  ZZ.ZZ9,99   VALUE  ZEROS.

      SCREEN SECTION.
        01 CLEAR-SCREEN.
             05 BLANK SCREEN BACKGROUND-COLOR 0 FOREGROUND-COLOR 0.

       PROCEDURE   DIVISION.
       
       INICIO.
           PERFORM INICIALIZACAO.
           PERFORM PROCESSAMENTO UNTIL W-OPCAO = "N".
           PERFORM FINALIZACAO.
           EXIT PROGRAM.
       
       INICIALIZACAO.
           PERFORM   LIMPAR-VARIAVEIS.
           OPEN  I-O  ARQ-PROD.
      *>
      *>    Se  W-COD-ERRO   retornar codigo 05 significa que
      *>    o arquivo nao existe e portanto esta criando um novo arquivo
      *>

       PROCESSAMENTO.
           PERFORM FORMATAR-TELA.
           PERFORM RECEBER-DADOS.
           PERFORM GRAVAR-DADOS.
           PERFORM OPCAO-CONTINUIDADE.
       
       FORMATAR-TELA.
          DISPLAY CLEAR-SCREEN.
	        DISPLAY  "INCLUSAO DE PRODUTOS"  AT 0520.
	        DISPLAY  "CODIGO:"   AT  1010.
          DISPLAY  "DESCRICAO:"   AT  1210.
          DISPLAY  "PRECO UNIT.:"   AT  1410.
          DISPLAY  "CONFIRMA A INCLUSAO?(S/N):"   AT  1810.
	        DISPLAY  "MENSAGEM: "   AT  2410.
       
       RECEBER-DADOS.
           PERFORM   LIMPAR-VARIAVEIS.
           PERFORM  WITH TEST AFTER UNTIL    *> VALIDA COD-PROD
                    COD-PROD > 100 AND < 500
               ACCEPT COD-ED  AT  1030
               MOVE  COD-ED  TO  COD-PROD
               IF  COD-PROD <= 100 OR >= 500
                   DISPLAY "CODIGO DEVERA SER > 100 E < 500"
                           AT 2421
               ELSE
                   DISPLAY W-BRANCO AT 2421
               END-IF
           END-PERFORM.
           PERFORM WITH TEST AFTER UNTIL     *> VALIDA DESCRI-PROD
                   DESCRI-PROD NOT = SPACES
               ACCEPT DESCRI-PROD  AT  1230
               IF  DESCRI-PROD = SPACES
                   DISPLAY "DESCRICAO - CAMPO OBRIGATORIO    " AT 2421
               ELSE
                   DISPLAY W-BRANCO AT 2421
               END-IF
           END-PERFORM.
           
	       PERFORM WITH TEST AFTER UNTIL    *> VALIDA PRECO-PROD
                   PRECO-PROD > 0 AND <= 10000,00
               ACCEPT PRECO-ED  AT  1430
               MOVE   PRECO-ED  TO  PRECO-PROD
               IF  PRECO-PROD = 0 OR > 10000,00
                   DISPLAY "PRECO UNIT. DEVERA SER > 0 E <= 10.000,00"
                           AT 2421
               ELSE
                   DISPLAY W-BRANCO AT 2421
               END-IF
           END-PERFORM.
       
       GRAVAR-DADOS.
           PERFORM WITH TEST AFTER UNTIL  *> VALIDA W-INCLUI
                   W-INCLUI = "S" OR "N"
               ACCEPT W-INCLUI AT  1845
               IF  W-INCLUI NOT = "S" AND "N"
                   DISPLAY "DIGITAR S PARA GRAVAR E N PARA DESITIR"
                           AT 2421
               ELSE
                   DISPLAY W-BRANCO AT 2421
               END-IF
           END-PERFORM.
           IF  W-INCLUI = "S"    
               WRITE  REG-PROD  
               IF W-COD-ERRO NOT = "00"
                  DISPLAY "REGISTRO DUPLICADO" AT 2421 WITH FOREGROUND-COLOR 4
               ELSE
                  DISPLAY "                   " AT 2421
               END-IF
           ELSE
               DISPLAY "REGISTRO DESCARTADO   " AT 2421
               STOP  "    <ENTER>  PARA  CONTINUAR" 
               DISPLAY W-BRANCO AT       2421
           END-IF.
       
       OPCAO-CONTINUIDADE.
           DISPLAY "DESEJA INCLUIR OUTRO REGISTRO?(S/N):" AT 2220
           PERFORM WITH TEST AFTER UNTIL
                   W-OPCAO = "S" OR "N"
               ACCEPT W-OPCAO AT  2265
               IF  W-OPCAO NOT = "S" AND "N"
                   DISPLAY "DIGITAR S PARA INCLUIR OUTRO REGISTRO E N PARA TERMINAR"  AT 2421
               ELSE
                   DISPLAY W-BRANCO AT 2421
               END-IF
           END-PERFORM.
       
       LIMPAR-VARIAVEIS.
           INITIALIZE   REG-PROD.
           MOVE ZEROS TO COD-ED  PRECO-ED.
           MOVE SPACES TO W-INCLUI  W-OPCAO.
       
       FINALIZACAO.
           CLOSE  ARQ-PROD.
           DISPLAY "TERMINO DO PROCESSAMENTO" AT 2421.
  		STOP   "   ".
       FIM.
