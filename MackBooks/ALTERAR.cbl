
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ALTERAR.

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
           FILE SECTION.
           FD CAD-PRODUTO.
           01 REG-PRODUTO.
              02 COD-LIVRO     PIC 9(3).
              02 TITULO-LIVRO  PIC X(40).
              02 AUTOR-LIVRO   PIC X(40).
              02 FILLER        PIC X(41).

           WORKING-STORAGE SECTION.
           77 CODERRO             PIC X(2)  VALUE SPACES.
           77 OPC                 PIC X     VALUE SPACE.
              88 OPC-OK                     VALUE "S" "N".
           77 W-CODLIVRO-PESQUISA PIC 9(3)  VALUE ZEROS.
           77 COD-LIVRO-ED        PIC ZZ9   VALUE ZEROS.
           77 OPC-ALT             PIC X     VALUE SPACE.
           77 W-BRANCO            PIC X(50) VALUE SPACES.

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
           PERFORM ROTINA-LEITURA.
           PERFORM EXIBIR-DADOS-LIDOS.
           PERFORM ROTINA-ALTERACAO.
           PERFORM RECEBER-OPCAO-CONTINUIDADE.

       FORMATAR-TELA.

           MOVE ZEROS TO COD-LIVRO-ED.

           *> LIMPANDO TELA
           DISPLAY CLEAR-SCREEN.

           CALL "CABECALHO"
           DISPLAY "ALTERACAO DE LIVROS"          AT 1309.

           DISPLAY "CODIGO:"                      AT 1502.
           DISPLAY "TITULO:"                      AT 1702.
           DISPLAY "AUTOR:"                       AT 1902.
           DISPLAY "CONFIRMAR ALTERACAO? (S/N): " AT 2102.
           DISPLAY "OUTRO REGISTRO? (S/N): "      AT 2302.
           DISPLAY "MENSAGEM:"                    AT 2502.

       ROTINA-LEITURA.
           ACCEPT COD-LIVRO-ED AT 1511.
           MOVE COD-LIVRO-ED TO COD-LIVRO.
           READ CAD-PRODUTO.

       EXIBIR-DADOS-LIDOS.
           IF CODERRO NOT = "00"
               DISPLAY "LIVRO NAO ENCONTRADO" AT 2512
           ELSE
               DISPLAY TITULO-LIVRO AT 1711
               DISPLAY AUTOR-LIVRO  AT 1911
           END-IF.

       ROTINA-ALTERACAO.

           IF CODERRO = "00"
               PERFORM RECEBER-NOVA-TITULO-LIVRO
               PERFORM RECEBER-NOVO-AUTOR-LIVRO

               ACCEPT OPC-ALT AT 2130 WITH AUTO
               MOVE FUNCTION UPPER-CASE (OPC-ALT) TO OPC-ALT

               IF OPC-ALT = "S"
                   REWRITE  REG-PRODUTO
                   DISPLAY "ALTERACAO REALIZADA COM SUCESSO" AT 2512
               ELSE
                   DISPLAY "ALTERACAO NAO EFETIVADA" AT 2512
               END-IF
           END-IF.

       RECEBER-NOVA-TITULO-LIVRO.
           PERFORM WITH TEST AFTER UNTIL TITULO-LIVRO NOT = SPACES
               ACCEPT TITULO-LIVRO  AT  1711
               IF  TITULO-LIVRO = SPACES
                   DISPLAY "O TITULO DO LIVRO E OBRIGATORIO" AT 2312
               ELSE
                   PERFORM LIMPAR-ESPACO-MENSAGEM
               END-IF
           END-PERFORM.

       RECEBER-NOVO-AUTOR-LIVRO.
           PERFORM WITH TEST AFTER UNTIL AUTOR-LIVRO NOT = SPACES
               ACCEPT AUTOR-LIVRO AT 1911
               IF AUTOR-LIVRO = SPACES
                   DISPLAY "O AUTOR DO LIVRO E OBRIGATORIO" AT 2312
               ELSE
                   PERFORM LIMPAR-ESPACO-MENSAGEM
               END-IF
           END-PERFORM.

       RECEBER-OPCAO-CONTINUIDADE.
           PERFORM WITH TEST AFTER UNTIL OPC-OK
               ACCEPT OPC AT 2325 WITH AUTO
               MOVE FUNCTION UPPER-CASE (OPC) TO OPC
               IF NOT OPC-OK
                   DISPLAY "DIGITE 'S' OU 'N'" AT 2312
               ELSE
                   PERFORM LIMPAR-ESPACO-MENSAGEM
               END-IF
           END-PERFORM.

       ABRIR-ARQUIVO.
           OPEN I-O CAD-PRODUTO.

       FINALIZACAO.
           CLOSE CAD-PRODUTO.

       LIMPAR-ESPACO-MENSAGEM.
           DISPLAY W-BRANCO AT 2512.
