       
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
                   RECORD KEY IS COD-LIVRO
                   ACCESS RANDOM
                   FILE STATUS IS CODERRO.

       DATA DIVISION.
         
         FILE  SECTION.
         FD  CAD-PRODUTO
             LABEL  RECORD  STANDARD.
         01  REG-LIVRO.
             02  COD-LIVRO     PIC  9(3).
             02  TITULO-LIVRO  PIC  X(40).
             02  AUTOR-LIVRO   PIC  X(40).
             02  FILLER        PIC  X(41).

         WORKING-STORAGE SECTION.
         77  CODERRO              PIC X(2)  VALUE SPACES.
         77  OPC                  PIC X     VALUE SPACE.
             88  OPC-OK                     VALUE "S" "N".
         77  W-COD-LIVRO-PESQUISA PIC 9(3)  VALUE ZEROS.
         77  COD-LIVRO-ED         PIC ZZ9   VALUE ZEROS.
         77  OPC-EXCL             PIC X     VALUE SPACE.
         77  W-BRANCO             PIC X(50) VALUE SPACES.
  	
         SCREEN SECTION.
            01 CLEAR-SCREEN.
               05 BLANK SCREEN BACKGROUND-COLOR 0 FOREGROUND-COLOR 0.
       
       PROCEDURE DIVISION.

       INICIO.
           PERFORM INICIALIZACAO.
           PERFORM PROCESSAMENTO UNTIL OPC = "N".
	         PERFORM FINALIZACAO.
           EXIT PROGRAM.

       INICIALIZACAO.
           MOVE "S" TO OPC.
           PERFORM ABRIR-ARQUIVO.

       PROCESSAMENTO.
           PERFORM FORMATAR-TELA.
           PERFORM ROTINA-LEITURA
           PERFORM EXIBIR-DADOS-LIDOS.
           PERFORM ROTINA-DELECAO.
           PERFORM RECEBER-OPCAO-CONTINUIDADE.

       FORMATAR-TELA.
           MOVE ZEROS TO COD-LIVRO-ED.

           *> LIMPANDO TELA
           DISPLAY CLEAR-SCREEN.

           CALL "CABECALHO"

	         DISPLAY "EXCLUSAO DE CADASTRO DE PRODUTOS" AT 0515.
	         DISPLAY "CODIGO:" AT 1010.
           DISPLAY "TITULO-LIVROCAO:" AT 1210.
           DISPLAY "AUTOR-LIVRO UNIT.:" AT 1410.
           DISPLAY "OUTRO REGISTRO?(S/N):" AT 2010.



           MOVE ZEROS TO COD-LIVRO-ED.

           *> LIMPANDO TELA
           DISPLAY  CLEAR-SCREEN.
           
           CALL "CABECALHO".

           *> TITULO DO PROGRAMA
           DISPLAY "LEITURA DE LIVROS" AT 1311.

           *> CAMPOS
           DISPLAY "CODIGO:"                   AT 1502.
           DISPLAY "TITULO:"                   AT 1702.
           DISPLAY "AUTOR:"                    AT 1902.
           DISPLAY "CONFIRMAR REMOCAO? (S/N): " AT 2102.
           DISPLAY "OUTRO REGISTRO? (S/N): "   AT 2302.
           DISPLAY "MENSAGEM:"                 AT 2502.

       ROTINA-LEITURA.
          ACCEPT COD-LIVRO-ED AT 1511.
          MOVE COD-LIVRO-ED TO  COD-LIVRO.    
          READ CAD-PRODUTO.
           
       EXIBIR-DADOS-LIDOS.
          IF  CODERRO NOT = "00"
              DISPLAY "LIVRO NAO ENCONTRADO" AT 2512
          ELSE
              DISPLAY TITULO-LIVRO   AT 1711
              DISPLAY AUTOR-LIVRO    AT 1911
          END-IF. 
                 
       ROTINA-DELECAO.
            IF  CODERRO = "00"
                ACCEPT   OPC-EXCL AT 2129
		            IF  OPC-EXCL  =  "S"
  			           DELETE  CAD-PRODUTO      
                   DISPLAY "EXCLUSIVA REALIZADA COM SUCESSO" AT 2512
                ELSE
			             DISPLAY "EXCLUSAO NAO EFETIVADA" AT 2512
                END-IF
             END-IF.
      
       RECEBER-OPCAO-CONTINUIDADE.
           PERFORM WITH TEST AFTER UNTIL OPC-OK
               ACCEPT OPC AT 2325 WITH AUTO
               MOVE FUNCTION UPPER-CASE (OPC) TO OPC
               PERFORM LIMPAR-ESPACO-MENSAGEM
               IF NOT OPC-OK
                   DISPLAY "DIGITE 'S' OU 'N'" AT 2512
               END-IF
           END-PERFORM.
       
       ABRIR-ARQUIVO.
           *> SE ARQUIVO NAO FOR ENCONTRADO, UM NOVO SERA CRIADO
           OPEN  I-O  CAD-PRODUTO.

       FINALIZACAO.
           CLOSE  CAD-PRODUTO
           DISPLAY "FIM DE PROCESSAMENTO" AT 2455.
           STOP  " ".

       LIMPAR-ESPACO-MENSAGEM.
          DISPLAY W-BRANCO AT 2512.

       FIM-ULTIMA-LINHA.