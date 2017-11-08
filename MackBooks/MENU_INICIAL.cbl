       IDENTIFICATION DIVISION.
       
       PROGRAM-ID. MENU_INICIAL INITIAL.
       AUTHOR. DANIEL.
       DATE-WRITTEN. 06/11/2017.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           SPECIAL-NAMES.
               DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       
       WORKING-STORAGE SECTION.
       
       01 DATA-SIST.
           02 ANO-SIST PIC 9(4).
           02 MES-SIST PIC 9(2).
           02 DIA-SIST PIC 9(2).
       01 DATA-DIA.
           02 DIA PIC 99/.
           02 MES PIC 99/.
           02 ANO PIC 9(4).
       
       77 OPC PIC 9 VALUE ZEROS.
           88 OPC-VALIDA VALUE 1 2 3 4 9.
           
       77 BRANCO PIC X(20) VALUE SPACES.
       77 LINHA  PIC X(32) VALUE ALL '='.                       

       PROCEDURE DIVISION.
       
           PERFORM CABECALHO.
       
           PERFORM MENUPRINCIPAL UNTIL OPC = 9.
           STOP " ".
           STOP RUN.
       
       CABECALHO.
       
           *> DIVISOES
           DISPLAY LINHA AT 0302.
           DISPLAY LINHA AT 1102.
       
           *> PEGANDO DATA DO SISTEMA
           ACCEPT DATA-SIST FROM DATE YYYYMMDD.
           MOVE ANO-SIST TO ANO.
           MOVE MES-SIST TO MES.
           MOVE DIA-SIST TO DIA.
       
           DISPLAY "BOOK'S CORP" AT 0503.
           DISPLAY DATA-DIA AT 0523.
           DISPLAY "MACKBOOKS" AT 0714.
           DISPLAY "DEVS: DANIEL, RONIFER E HENRIQUE " AT 0902.
       
       MENUPRINCIPAL.
          
           MOVE ZEROS TO OPC.
           
   	       DISPLAY "(1)  ADICIONAR LIVRO" AT 1307.     
   	       DISPLAY "(2)  REMOVER LIVRO"   AT 1507.    
   	       DISPLAY "(3)  ALTERAR LIVRO"   AT 1707.
           DISPLAY "(4)  CONSULTAR LIVRO" AT 1907.
           DISPLAY "(9)  ENCERRAR"        AT 2107.                      
   	       DISPLAY "ESCOLHA A OPCAO:"     AT 2307.
           
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
                       DISPLAY "SOMETHING" AT 1010
                   WHEN 2
                       *> CHAMADO DE REMOCAO
                       DISPLAY "SOMETHING" AT 1010
                   WHEN 3
                       *> CHAMADO DE ALTERACAO
                       DISPLAY "SOMETHING" AT 1010
                   WHEN 4 
                       *> CHAMADO DE CONSULTA
                       DISPLAY "SOMETHING" AT 1010
               END-EVALUATE
           END-PERFORM.
           
      . *> FIM DO PROGRAMA PRINCIPAL