       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG00.
       AUTHOR. DIEGO H.
       DATE-WRITTEN. 24/06/2019.
      *              ******** MENU PRINCIPAL ********

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 DATA-SISTEMA         PIC 9(06) VALUE ZEROS.
       01 FILLER REDEFINES DATA-SISTEMA.
          02 ANO-SISTEMA       PIC 9(02).
          02 MES-SISTEMA       PIC 9(02).
          02 DIA-SISTEMA       PIC 9(02).
       77 LINHA-TRACO          PIC X(80) VALUE ALL '-'.
       01 CAB-DATA             PIC 99/99/99.
       01 OPCAO                PIC 9(2) VALUE 1.

       SCREEN SECTION.
       01 LIMPA-TELA   BLANK SCREEN
                       BACKGROUND-COLOR 1
                       FOREGROUND-COLOR 7.

       01  MENU BLANK SCREEN
                BACKGROUND-COLOR 1
                FOREGROUND-COLOR 7.
           02 LINE 01 COLUMN 01 PIC X(80) FROM LINHA-TRACO.
           02 LINE 02 COLUMN 01 PIC X(08) FROM CAB-DATA.
           02 LINE 02 COLUMN 15 VALUE
              "HBSIS - Sistema Gerenciador de Carteiras de Clientes".
           02 LINE 02 COLUMN 73 VALUE "MENU".
           02 LINE 03 COLUMN 01 PIC X(80) FROM LINHA-TRACO.
           02 LINE 04 COLUMN 01 VALUE
              "01 - MANUTENCAO DE CLIENTES".
           02 LINE 05 COLUMN 01 VALUE
              "02 - MANUTENCAO DE VENDEDORES".
           02 LINE 06 COLUMN 01 VALUE
              "03 - RELATORIO DE CLIENTES".
           02 LINE 07 COLUMN 01 VALUE
              "04 - RELATORIO DE VENDEDORES".
           02 LINE 08 COLUMN 01 VALUE
              "05 - EXECUTAR DISTRIBUICAO DE CLIENTES".
           02 LINE 24 COLUMN 01
              "Informe sua opcao. Para encerrar digite 00: ".

       PROCEDURE DIVISION.
       INICIO.
           ACCEPT DATA-SISTEMA FROM DATE
           MOVE DIA-SISTEMA TO CAB-DATA (1:2)
           MOVE "/" TO CAB-DATA (3:1)
           MOVE MES-SISTEMA TO CAB-DATA (4:2)
           MOVE "/" TO CAB-DATA (6:1)
           MOVE ANO-SISTEMA TO CAB-DATA (7:2)
           PERFORM UNTIL OPCAO EQUAL 00
               DISPLAY MENU
               ACCEPT OPCAO
               EVALUATE OPCAO
                   WHEN 01
                       DISPLAY LIMPA-TELA
                       CALL "PROG01" USING CAB-DATA
                       CANCEL "PROG01"
                   WHEN 02
                       DISPLAY LIMPA-TELA
                       CALL "PROG02" USING CAB-DATA
                       CANCEL "PROG02"
                   WHEN 03
                       DISPLAY LIMPA-TELA
                       CALL "PROG03" USING CAB-DATA
                       CANCEL "PROG03"
                   WHEN 04
                       DISPLAY LIMPA-TELA
                       CALL "PROG04" USING CAB-DATA
                       CANCEL "PROG04"
                   END-EVALUATE
           END-PERFORM
           .
       FIM.
           STOP RUN.
