       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG03.
       AUTHOR. DIEGO H.
       DATE-WRITTEN. 25/06/2019.
      * ------------ LISTAGEM DE VENDEDOR --------------

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       FILE-CONTROL.
           SELECT ARQ-CLIENTE ASSIGN TO DISK WID-ARQ-CLIENTE
                  ORGANIZATION     IS INDEXED
                  RECORD KEY       IS CLI-CODIGO
                  ACCESS MODE      IS DYNAMIC
                  LOCK MODE        IS MANUAL
                  FILE STATUS      IS WS-RESULTADO-ACESSO.


           SELECT ARQ-SAI ASSIGN TO "ARQSAI".

           SELECT RELATORIO ASSIGN TO "RCLIENTE.TXT"
                            ORGANIZATION IS LINE SEQUENTIAL.

           SELECT ARQ-SORT ASSIGN TO "SORT".


       DATA DIVISION.
       FILE SECTION.

       COPY "ARQ-CLIENTE.FD".
       FD RELATORIO.
       01 LINHA        PIC X(132).

       FD ARQ-SAI.

       01 REG-SAI.
          02 CLI-CODIGO-SAI            PIC 9(07).
          02 CLI-CNPJ-SAI               PIC 9(14).
          02 CLI-NOME-SAI              PIC X(40).
          02 CLI-LATITUDE-SAI          PIC s9(03)V9(08).
          02 CLI-LONGITUDE-SAI         PIC s9(03)V9(08).


       SD ARQ-SORT.

       01 REG-SORT.
          05 CLI-CODIGO-SORT PIC 9(07).
          05 FILLER          PIC 9(14).
          05 CLI-NOME-SORT   PIC X(40).
          05 FILLER          PIC X(22).

       WORKING-STORAGE SECTION.
      *VARIAVEIS DA TELA
       77 WS-ORDEM             PIC X(01) VALUE SPACES.
       77 WS-CLASSIFICA        PIC X(01) VALUE SPACES.
       77 WS-CODIGO-CLIENTE    PIC 9(07) VALUE ZEROS.
       77 WS-NOME-CLIENTE      PIC X(40) VALUE SPACES.
      *-----------------------------------------------------------------
       77 LINHA-TRACO          PIC X(80) VALUE ALL '-'.
       77 LIMPA-LINHA          PIC X(80) VALUE SPACES.
       77 CONTADOR-LINHA       PIC 9(02) VALUE ZERO.
       77 CONTADOR-PAGINA      PIC 9(03) VALUE ZERO.
       77 CONTROLE-FIM         PIC 9(01) VALUE ZEROS.
       77 PAUSA                PIC X(01).
       77 WS-RESULTADO-ACESSO       PIC 9(02) VALUE ZEROS.
       01 CABECALHO-1.
          02 FILLER PIC X(06) VALUE "CODIGO".
          02 FILLER PIC X(03) VALUE SPACES.
          02 FILLER PIC X(30) VALUE " NOME CLIENTE".
          02 FILLER PIC X(03) VALUE SPACES.
          02 FILLER PIC X(18) VALUE "           C.N.P.J".
          02 FILLER PIC X(03) VALUE SPACES.
          02 FILLER PIC X(15) VALUE "       LATITUDE".
          02 FILLER PIC X(03) VALUE SPACES.
          02 FILLER PIC X(12) VALUE "   LONGITUDE".

       01 CABECALHO-2.
          02 FILLER PIC X(132) VALUES ALL "-".

       01 DETALHE.
          02 DET-CLI-CODIGO        PIC 9(03).
          02 FILLER                PIC X(03) VALUE SPACES.
          02 DET-CLI-RAZAO-SOCIAL  PIC X(40).
          02 FILLER                PIC X(07) VALUE SPACES.
          02 DET-CLI-CNPJ          PIC 9(14).
          02 FILLER                PIC X(03) VALUE SPACES.
          02 DET-CLI-LATITUDE      PIC s9(03)V9(08).
          02 FILLER                PIC X(03) VALUE SPACES.
          02 DET-CLI-LONGITUDE     PIC s9(03)V9(08).

       01 CABECALHO-TITULO.
          02 CAB-DATA     PIC X(08).
          02 FILLER       PIC X(41) VALUE SPACES.
          02 FILLER       PIC X(21) VALUE "RELACAO DE CLIENTES".
          02 FILLER       PIC X(51) VALUES SPACES.
          02 FILLER       PIC X(09) VALUE "PAGINA: ".
          02 CAB-PAGINA   PIC ZZ9.

       LINKAGE SECTION.
       77 DATA-DE-HOJE    PIC 99/99/99.

       SCREEN SECTION.
       01 LIMPA-TELA BLANK SCREEN
                     BACKGROUND-COLOR 1
                     FOREGROUND-COLOR 7.


       01 TELA-RELATORIO BLANK SCREEN
                         BACKGROUND-COLOR 1
                         FOREGROUND-COLOR 7.
          02 LINE 01 COLUMN 01 PIC X(80) FROM LINHA-TRACO.
          02 LINE 02 COLUMN 01 PIC X(08) FROM DATA-DE-HOJE.
          02 LINE 02 COLUMN 25 VALUE
             "     Relatorio de Vendedores     ".
          02 LINE 02 COLUMN 73 VALUE "PROG04".
          02 LINE 03 COLUMN 01 PIC X(80) FROM LINHA-TRACO.
          02 LINE 04 COLUMN 01 VALUE "ORDENACAO ASCENDENTE (A) OU DESCEN
      -"DENTE (D)?....... ".
          02 LINE 06 COLUMN 01 VALUE "CLASSIFICACAO POR CODIGO (C) OU RA
      -"ZAO SICIAL (R)?.. ".
          02 LINE 08 COLUMN 01 VALUE
       "********************************FILTROS*************************
      -"****************".
          02 LINE 10 COLUMN 01 VALUE "CODIGO DO CLIENTE...".
          02 LINE 12 COLUMN 01 VALUE "RAZAO SOCIAL........".



       PROCEDURE DIVISION USING DATA-DE-HOJE.

       INICIO.
           MOVE "CLIENTE.DAT"   TO WID-ARQ-CLIENTE
      *    OPEN INPUT ARQ-CLIENTE
           OPEN OUTPUT RELATORIO
           MOVE DATA-DE-HOJE TO CAB-DATA
           PERFORM IMPRIMIR-CABECALHO
           DISPLAY TELA-RELATORIO.

       ACCEPT-TELA.
           DISPLAY "SELECIONE A FORMA DE ORDENACAO DO RELATORIO" AT 2401
           PERFORM UNTIL WS-ORDEM = "A" OR = "D"
              ACCEPT WS-ORDEM AT 0453
           END-PERFORM
           DISPLAY LIMPA-LINHA AT 2401

          DISPLAY
          "SELECIONE A FORMA DE CLASSIFICACAO DO RELATORIO" AT 2401
           PERFORM UNTIL WS-CLASSIFICA = "C" OR = "R"
              ACCEPT WS-CLASSIFICA AT 0653
           END-PERFORM
           DISPLAY LIMPA-LINHA AT 2401

           DISPLAY
           "CASO QUEIRA FILTRAR POR NOME, DEIXAR O CAMPO ZERADO" AT 2401
           ACCEPT WS-CODIGO-CLIENTE  AT 1022
           DISPLAY LIMPA-LINHA AT 2401

           DISPLAY
           "CASO QUEIRA TRAZER O RELATORIO COM TODOS OS REGISTROS, DEIXE
      -" EM BRANCO" AT 2401
           ACCEPT WS-NOME-CLIENTE  AT 1222
           DISPLAY LIMPA-LINHA AT 2401.

       REL-GERAL.
           EVALUATE WS-ORDEM
              WHEN "A"
               PERFORM ASCENDENTE
              WHEN "D"
               PERFORM DESCENDENTE
           END-EVALUATE
           PERFORM IMPRIME-RELATORIO
              THRU F-IMPRIME-RELATORIO
           .
       F-REL-GERAL. EXIT.

           DISPLAY LIMPA-TELA
           DISPLAY "RELATORIO GERADO COM SUCESSO TECLE ENTER PARA RETORN
      -    "AR AO MENU INICIAL" AT 1503
           ACCEPT  PAUSA               AT 2478.
           CLOSE ARQ-CLIENTE
           CLOSE RELATORIO

           EXIT PROGRAM.

       ASCENDENTE.
         IF WS-CLASSIFICA ="C"
            SORT ARQ-SORT  ON ASCENDING KEY CLI-CODIGO-SORT
               USING  ARQ-CLIENTE
               GIVING ARQ-SAI
         ELSE
            SORT ARQ-SORT  ON ASCENDING KEY CLI-NOME-SORT
               USING  ARQ-CLIENTE
              GIVING ARQ-SAI

         END-IF.

       DESCENDENTE.
         IF WS-CLASSIFICA ="C"
            SORT ARQ-SORT ON DESCENDING KEY CLI-CODIGO-SORT
               USING  ARQ-CLIENTE
               GIVING ARQ-SAI

         ELSE
            SORT ARQ-SORT ON DESCENDING KEY CLI-NOME-SORT
               USING  ARQ-CLIENTE
               GIVING ARQ-SAI
         END-IF.

       IMPRIMIR-CABECALHO.
           ADD 01 TO CONTADOR-PAGINA
           MOVE CONTADOR-PAGINA TO CAB-PAGINA
           WRITE LINHA FROM CABECALHO-TITULO AFTER PAGE
           WRITE LINHA FROM CABECALHO-2      AFTER 1 LINE
           WRITE LINHA FROM CABECALHO-1      AFTER 1 LINE
           WRITE LINHA FROM CABECALHO-2      AFTER 1 LINE
           MOVE  04 TO CONTADOR-LINHA.

       IMPRIME-RELATORIO.
           MOVE ZEROS TO CONTROLE-FIM
           OPEN INPUT ARQ-SAI
           PERFORM UNTIL CONTROLE-FIM = 1
              READ ARQ-SAI NEXT
                 AT END
                    MOVE 1 TO CONTROLE-FIM
                    EXIT PERFORM
              END-READ
              IF WS-CODIGO-CLIENTE <> ZEROS
                 IF CLI-CODIGO-SAI = WS-CODIGO-CLIENTE
                    MOVE CLI-CODIGO-SAI         TO DET-CLI-CODIGO
                    MOVE CLI-CNPJ-SAI           TO DET-CLI-CNPJ
                    MOVE CLI-NOME-SAI           TO DET-CLI-RAZAO-SOCIAL
                    MOVE CLI-LATITUDE-SAI       TO DET-CLI-LATITUDE
                    MOVE CLI-LONGITUDE-SAI      TO DET-CLI-LONGITUDE
                    WRITE LINHA FROM DETALHE AFTER 1 LINES
                 END-IF
              END-IF
              IF WS-NOME-CLIENTE <> SPACES
                 IF WS-NOME-CLIENTE = CLI-NOME-SAI
                    MOVE CLI-CODIGO-SAI      TO DET-CLI-CODIGO
                    MOVE CLI-CNPJ-SAI        TO DET-CLI-CNPJ
                    MOVE CLI-NOME-SAI        TO DET-CLI-RAZAO-SOCIAL
                    MOVE CLI-LATITUDE-SAI    TO DET-CLI-LATITUDE
                    MOVE CLI-LONGITUDE-SAI   TO DET-CLI-LONGITUDE
                    WRITE LINHA FROM DETALHE AFTER 1 LINES
                 END-IF
              END-IF
             IF WS-NOME-CLIENTE = SPACES AND WS-CODIGO-CLIENTE = ZEROS
                MOVE CLI-CODIGO-SAI         TO DET-CLI-CODIGO
                MOVE CLI-CNPJ-SAI           TO DET-CLI-CNPJ
                MOVE CLI-NOME-SAI   TO DET-CLI-RAZAO-SOCIAL
                MOVE CLI-LATITUDE-SAI       TO DET-CLI-LATITUDE
                MOVE CLI-LONGITUDE-SAI      TO DET-CLI-LONGITUDE
                WRITE LINHA FROM DETALHE AFTER 1 LINES
             END-IF

            END-PERFORM
            CLOSE ARQ-SAI.

       F-IMPRIME-RELATORIO. EXIT.


