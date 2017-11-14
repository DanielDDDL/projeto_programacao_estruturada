
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABECALHO.

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

           77 LINHA  PIC X(32) VALUE ALL '='.

           SCREEN SECTION.
           01 CLEAR-SCREEN.
               05 BLANK SCREEN BACKGROUND-COLOR 0 FOREGROUND-COLOR 0.

       PROCEDURE DIVISION.

           PERFORM IMPRIMIR.
           EXIT PROGRAM.

       IMPRIMIR.

           *> DIVISOES
           DISPLAY LINHA AT 0302.
           DISPLAY LINHA AT 1102.

           *> PEGANDO DATA DO SISTEMA
           ACCEPT DATA-SIST FROM DATE YYYYMMDD.
           MOVE ANO-SIST TO ANO.
           MOVE MES-SIST TO MES.
           MOVE DIA-SIST TO DIA.

           *> INFORMACOES DE CABECALHO
           DISPLAY "BOOK'S CORP" AT 0503.
           DISPLAY DATA-DIA AT 0523.
           DISPLAY "MACKBOOKS" AT 0714.
           DISPLAY "DEVS: DANIEL, RONIFER E HENRIQUE " AT 0902.
