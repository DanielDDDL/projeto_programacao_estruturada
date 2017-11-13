       
       IDENTIFICATION DIVISION.
       
       PROGRAM-ID. INCLUSAO.
       AUTHOR. DANIEL.
       DATE-WRITTEN. 06/11/2017.

       ENVIRONMENT DIVISION.
         CONFIGURATION SECTION.

             SPECIAL-NAMES.      
                 DECIMAL-POINT IS COMMA.

             INPUT-OUTPUT SECTION.
               FILE-CONTROL.
                   SELECT OPTIONAL ARQ-LIVRO 
                   ASSIGN TO "livros.dat"
                   ORGANIZATION INDEXED
                   RECORD KEY IS COD-LIVRO
                   ACCESS RANDOM
                   FILE STATUS IS W-COD-ERRO.

       DATA DIVISION.
         FILE SECTION.
         FD  ARQ-LIVRO
             LABEL RECORD STANDARD.
         01  REG-LIVRO.
             02  COD-LIVRO    PIC 9(3).
             02  TITULO-LIVRO PIC X(40).
             02  AUTOR-LIVRO  PIC X(40).
             02  FILLER       PIC X(41).

       
         WORKING-STORAGE SECTION.
         77  W-COD-ERRO     PIC XX         VALUE SPACES.
         77  W-OPCAO        PIC X          VALUE SPACES.
             88 OPC-OK                     VALUE "S" "N".
         77  W-INCLUI       PIC X          VALUE SPACES.
         77  W-BRANCO       PIC X(50)      VALUE SPACES.
         77  COD-ED	        PIC ZZ9        VALUE ZEROS.

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
           PERFORM LIMPAR-VARIAVEIS.
           OPEN I-O ARQ-LIVRO.

       PROCESSAMENTO.
           PERFORM FORMATAR-TELA.
           PERFORM RECEBER-DADOS.
           PERFORM GRAVAR-DADOS.
           PERFORM OPCAO-CONTINUIDADE.
       
       FORMATAR-TELA.

          *> LIMPANDO TELA
          DISPLAY CLEAR-SCREEN.

          CALL 'CABECALHO'.

          *> TITULO DO PROGRAMA
	        DISPLAY  "INCLUSAO DE LIVROS"           AT 1310.
	        
          *> CAMPOS
          DISPLAY  "CODIGO:"                      AT 1502.
          DISPLAY  "TITULO:"                      AT 1702.
          DISPLAY  "AUTOR:"                       AT 1902.
          DISPLAY  "CONFIRMA A INCLUSAO? (S/N):"  AT 2102.
	        DISPLAY  "MENSAGEM: "                   AT 2302.
          DISPLAY  "OUTRO REGISTRO? (S/N):"       AT 2502.
       
       RECEBER-DADOS.

           PERFORM LIMPAR-VARIAVEIS.

           *> CODIGO
           PERFORM WITH TEST AFTER UNTIL COD-LIVRO > 100 AND < 500
               ACCEPT COD-ED  AT  1511
               MOVE COD-ED  TO  COD-LIVRO
               IF COD-LIVRO <= 100 OR >= 500
                   DISPLAY "CODIGO DEVERA SER > 100 E < 500" AT 2312
               ELSE
                   PERFORM LIMPAR-ESPACO-MENSAGEM
               END-IF
           END-PERFORM.

           *> TITULO DE LIVRO
           PERFORM WITH TEST AFTER UNTIL TITULO-LIVRO NOT = SPACES
               ACCEPT TITULO-LIVRO  AT  1711
               IF  TITULO-LIVRO = SPACES
                  DISPLAY "O TITULO DO LIVRO E OBRIGATORIO" AT 2312
               ELSE
                  PERFORM LIMPAR-ESPACO-MENSAGEM
               END-IF
           END-PERFORM.
           
           *> AUTOR DO LIVRO
  	       PERFORM WITH TEST AFTER UNTIL AUTOR-LIVRO NOT = SPACES
               ACCEPT AUTOR-LIVRO  AT  1911
               IF AUTOR-LIVRO = SPACES
                  DISPLAY "O AUTOR DO LIVRO E OBRIGATORIO" AT 2312
                ELSE
                  PERFORM LIMPAR-ESPACO-MENSAGEM
                END-IF
           END-PERFORM.
       
       GRAVAR-DADOS.

           *> VALIDACAO DO CAMPO DE CONFIRMACAO
           PERFORM WITH TEST AFTER UNTIL W-INCLUI = "S" OR "N"
              ACCEPT W-INCLUI AT  2130 WITH AUTO
              MOVE FUNCTION UPPER-CASE (W-INCLUI) TO W-INCLUI
              IF W-INCLUI NOT = "S" AND "N"
                  DISPLAY "DIGITAR 'S' PARA GRAVAR E 'N' PARA DESITIR" AT 2312
              ELSE
                  PERFORM LIMPAR-ESPACO-MENSAGEM
              END-IF
           END-PERFORM.

           IF  W-INCLUI = "S"  
               WRITE REG-LIVRO  
               IF W-COD-ERRO NOT = "00"
                  DISPLAY "REGISTRO DUPLICADO" AT 2312
               ELSE
                  PERFORM LIMPAR-ESPACO-MENSAGEM
               END-IF
           ELSE
               DISPLAY "REGISTRO DESCARTADO" AT 2312
           END-IF.
       
       OPCAO-CONTINUIDADE.
           
           *> VALIDACAO DA OPCAO DE CONTINUEDADE
           PERFORM WITH TEST AFTER UNTIL OPC-OK
               ACCEPT W-OPCAO AT 2525
               MOVE FUNCTION UPPER-CASE (W-OPCAO) TO W-OPCAO
               IF W-OPCAO NOT = "S" AND "N"
                  DISPLAY "DIGITE 'S' PARA OUTRO REGISTRO 'N' PARA VOLTAR" AT 2312
               ELSE
                  PERFORM LIMPAR-ESPACO-MENSAGEM
               END-IF
           END-PERFORM.
       
       LIMPAR-VARIAVEIS.
           INITIALIZE REG-LIVRO.
           MOVE ZEROS TO COD-ED.
           MOVE SPACES TO W-INCLUI W-OPCAO.
       
       LIMPAR-ESPACO-MENSAGEM.
           DISPLAY W-BRANCO AT 2312.

       FINALIZACAO.
           CLOSE ARQ-LIVRO.
           DISPLAY "TERMINO DO PROCESSAMENTO" AT 2421.
