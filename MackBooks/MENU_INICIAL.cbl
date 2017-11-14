
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MENU_INICIAL INITIAL.

       ENVIRONMENT DIVISION.
           CONFIGURATION SECTION.
               SPECIAL-NAMES.
                   DECIMAL-POINT IS COMMA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       77 OPC PIC 9 VALUE ZEROS.
          88 OPC-VALIDA VALUE 1 2 3 4 9.

       77 BRANCO PIC X(20) VALUE SPACES.

       SCREEN SECTION.
          01 CLEAR-SCREEN.
             05 BLANK SCREEN BACKGROUND-COLOR 0 FOREGROUND-COLOR 0.

       PROCEDURE DIVISION.

           PERFORM INICIO.
           PERFORM MENUPRINCIPAL UNTIL OPC = 9.
           STOP RUN.

       INICIO.
          DISPLAY CLEAR-SCREEN.
          CALL "CABECALHO". *> EXIBICAO DO CABECALHO PADRAO
          PERFORM DISPLAY-MENU-PRINCIPAL.

       DISPLAY-MENU-PRINCIPAL.
           DISPLAY "(1)  ADICIONAR LIVRO" AT 1307.
           DISPLAY "(2)  REMOVER LIVRO"   AT 1507.
           DISPLAY "(3)  ALTERAR LIVRO"   AT 1707.
           DISPLAY "(4)  CONSULTAR LIVRO" AT 1907.
           DISPLAY "(9)  ENCERRAR"        AT 2107.
           DISPLAY "ESCOLHA A OPCAO:"     AT 2307.

       MENUPRINCIPAL.

           PERFORM INICIO.
           MOVE ZEROS TO OPC.

           PERFORM WITH TEST AFTER UNTIL OPC-VALIDA
               ACCEPT OPC AT 2325
               IF OPC-VALIDA
                   *> LIMPANDO MESANGEM DE ERRO
                   DISPLAY BRANCO AT 2407
               ELSE
                   *> MENSAGEM DE ERRO
                   DISPLAY "DIGITE 1, 2, 3, 4 OU 9" AT 2407
               END-IF

               EVALUATE OPC
                   WHEN 1
                       *> CHAMADO DO MODULO DE INCLUSAO
                       CALL "INCLUSAO"
                   WHEN 2
                       *> CHAMADO DE REMOCAO
                       CALL "EXCLUSAO"
                   WHEN 3
                       *> CHAMADO DE ALTERACAO
                       CALL "ALTERAR"
                   WHEN 4
                       *> CHAMADO DE CONSULTA
                       CALL "LEITURA"
               END-EVALUATE
           END-PERFORM.
