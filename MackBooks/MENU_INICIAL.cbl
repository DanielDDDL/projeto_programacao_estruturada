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

       PROCEDURE DIVISION.
           ACCEPT DATA-SIST FROM DATE YYYYMMDD.
           MOVE ANO-SIST TO ANO.
           MOVE MES-SIST TO MES.
           MOVE DIA-SIST TO DIA.
       
           *> A MOSTRAR A DATA DO SISTEMA AQUI
       
           PERFORM MENUPRINCIPAL UNTIL OPC = 9.
           STOP " ".
           STOP RUN.
       
       MENUPRINCIPAL.
           
           DISPLAY ERASE.
           MOVE ZEROS TO OPC.
           
   	       DISPLAY "(1)  ADICIONAR LIVRO".     
   	       DISPLAY "(2)  REMOVER LIVRO".    
   	       DISPLAY "(3)  ALTERAR LIVRO".
           DISPLAY "(4)  CONSULTAR LIVRO".
           DISPLAY "(9)  ENCERRAR".                                 
   	       DISPLAY "ESCOLHA A OPCAO:".
           
           PERFORM WITH TEST AFTER UNTIL OPC-VALIDA
           ACCEPT OPC WITH AUTO
           IF OPC-VALIDA
               *> LIMPANDO MESANGEM DE ERRO
               DISPLAY BRANCO
           ELSE 
               *> MENSAGEM DE ERRO
               DISPLAY "DIGITE 1, 2 OU 9"
           END-IF.
           
           EVALUATE OPC
               WHEN 1
                   *> CHAMADO DO MODULO DE INCLUSAO
               WHEN 2
                   *> CHAMADO DE REMOCAO
               WHEN 3
                   *> CHAMADO DE ALTERACAO
               WHEN 4 
                   *> CHAMADO DE CONSULTA
           END-EVALUATE.