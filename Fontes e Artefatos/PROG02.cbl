       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG02.
       AUTHOR. DIEGO H.
       DATE-WRITTEN. 24/06/2019.
      *      ******** PROGRAMA MANUTEN��O VENDEDORES *********          *

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       FILE-CONTROL.
           SELECT ARQ-VENDEDOR ASSIGN TO DISK WID-ARQ-VENDEDOR
                  ORGANIZATION     IS INDEXED
                  RECORD KEY       IS VEN-CODIGO
                  ACCESS MODE      IS DYNAMIC
                  LOCK MODE        IS MANUAL
                  FILE STATUS      IS WS-RESULTADO-ACESSO.

       DATA DIVISION.
       FILE SECTION.


       COPY "ARQ-VENDEDOR.FD".

       WORKING-STORAGE SECTION.

       COPY "AREA-CPF.CPY".

       01 AUX-REGISTRO-VENDEDOR.
          02 AUX-CODIGO            PIC 9(03).
          02 AUX-CPF               PIC 9(11).
          02 AUX-NOME              PIC X(40).
          02 AUX-LATITUDE          PIC S9(03)V9(08).
          02 AUX-LONGITUDE         PIC S9(03)V9(08).

       77 LINHA-TRACO           PIC X(80) VALUE ALL '-'.
       77 LIMPA-LINHA           PIC X(80) VALUE SPACES.
       77 WID-ARQ-VENDEDOR      PIC X(50) VALUE SPACES.
       77 WS-RESULTADO-ACESSO   PIC 9(02) VALUES ZEROS.
       77 TIPO-LEITURA          PIC X(02) VALUES SPACES.
       77 CONTROLE-FIM          PIC 9(02) VALUES ZEROS.
       77 OPCAO                 PIC A(01) VALUES SPACES.
       77 PAUSA                 PIC X(02) VALUES SPACES.
       77 MASCARA-DATA-CADASTRO PIC 99/99/99.
       77 WS-RESPOSTA           PIC X(01) VALUE SPACES.
       77 LK-TIPO-DADO          PIC 9(01). *> 01 - CLIENTE 02 - VENDEDOR

       LINKAGE SECTION.
       77 DATA-DE-HOJE          PIC 99/99/99.

       SCREEN SECTION.
       01 LIMPA-TELA   BLANK SCREEN
                       BACKGROUND-COLOR 1
                       FOREGROUND-COLOR 7.

       01 TELA-VENDEDOR BLANK SCREEN
                       BACKGROUND-COLOR 1
                       FOREGROUND-COLOR 7.
          02 LINE 01 COLUMN 01 PIC X(80) FROM LINHA-TRACO.
          02 LINE 02 COLUMN 01 PIC X(08) FROM DATA-DE-HOJE.
          02 LINE 02 COLUMN 25 VALUE
             "     CADASTRO DE VENDEDORES     ".
          02 LINE 02 COLUMN 73 VALUE "PROG02".
          02 LINE 03 COLUMN 01 PIC X(80) FROM LINHA-TRACO.
          02 LINE 04 COLUMN 01 VALUE "CODIGO DO VENDEDOR....".
          02 LINE 05 COLUMN 01 VALUE "CPF...................".
          02 LINE 06 COLUMN 01 VALUE "NOME..................".
          02 LINE 07 COLUMN 01 VALUE "LATITUDE..............".
          02 LINE 08 COLUMN 01 VALUE "LONGITUDE.............".
          02 LINE 09 COLUMN 01 PIC X(80) FROM LINHA-TRACO.

       PROCEDURE DIVISION USING DATA-DE-HOJE.

       INICIO.
      *----------VERIFICA SE O USUARIO QUER IMPORTAR O ARQUIVO----------*
           DISPLAY "DESEJA IMPORTAR UM ARQUIVO? S/N " AT 2401
           PERFORM UNTIL WS-RESPOSTA = "S" OR = "N" OR = "N" OR = "S"
              ACCEPT WS-RESPOSTA AT 2433
           END-PERFORM
           IF WS-RESPOSTA = "S"
              PERFORM TRATA-IMPORTA
                 THRU F-TRATA-IMPORTA
              EXIT PROGRAM
           END-IF

      *------ VERIFICA��O PARA INCLUSAO OU ALTERA�AO NO CADASTRO -------*
           MOVE "VENDEDOR.DAT"   TO WID-ARQ-VENDEDOR
           OPEN I-O ARQ-VENDEDOR
           IF WS-RESULTADO-ACESSO NOT = 00
              OPEN OUTPUT ARQ-VENDEDOR
              CLOSE ARQ-VENDEDOR
              OPEN I-O ARQ-VENDEDOR
           END-IF
      *------ VERIFICA��O DO PROXIMO CODIGO VVENDEDOR LAN�ADO ------    *
           MOVE 999 TO VEN-CODIGO
           PERFORM UNTIL VEN-CODIGO = ZEROS
              MOVE 999 TO VEN-CODIGO
              START ARQ-VENDEDOR KEY LESS VEN-CODIGO
              IF WS-RESULTADO-ACESSO NOT = 00
                 DISPLAY "ERRO NO POSICIONAMENTO DA CHAVE - VENDEDOR: "
                      AT 2401
                 DISPLAY WS-RESULTADO-ACESSO AT 2440
                 ACCEPT  PAUSA               AT 2478
                 DISPLAY LIMPA-TELA          AT 2401
              END-IF
              READ ARQ-VENDEDOR NEXT AT END
                MOVE ZEROS TO VEN-CODIGO
              END-READ
              ADD 1 TO VEN-CODIGO
              MOVE VEN-CODIGO TO AUX-CODIGO
      *------ INICIO DO PROCESSO ---------------------------------------*
              MOVE 1 TO VEN-CODIGO
              PERFORM MOSTRAR-TELA
              ACCEPT AUX-CODIGO AT 0424
              MOVE AUX-CODIGO TO VEN-CODIGO
              IF VEN-CODIGO NOT EQUAL ZEROS THEN
                 MOVE "I" TO TIPO-LEITURA
                 PERFORM LER-ARQUIVO THRU FIM-LER-ARQUIVO
                    IF WS-RESULTADO-ACESSO = 23
                       PERFORM INCLUIR
                    ELSE
                       PERFORM CONSULTAR
                       PERFORM EXECUTAR-OPCAO
                    END-IF
              END-IF
           END-PERFORM
           .
       FIM.
           CLOSE ARQ-VENDEDOR
           IF WS-RESULTADO-ACESSO NOT = 0
              DISPLAY "ERRO NO FECHAMENTO;" AT 2401
              DISPLAY WS-RESULTADO-ACESSO        AT 2421
           END-IF
           EXIT PROGRAM
           .
       MOSTRAR-TELA.
           DISPLAY TELA-VENDEDOR AT 0101
           .
       LER-ARQUIVO.
           MOVE 99 TO WS-RESULTADO-ACESSO
           PERFORM UNTIL WS-RESULTADO-ACESSO NOT = 99
              IF TIPO-LEITURA = "I"
                 READ ARQ-VENDEDOR
                 IF WS-RESULTADO-ACESSO NOT = 00
                    MOVE 1 TO CONTROLE-FIM
                 END-IF
              ELSE
                 READ ARQ-VENDEDOR NEXT AT END
                      MOVE 1 TO CONTROLE-FIM
                 END-READ
              END-IF
              IF WS-RESULTADO-ACESSO = 68
                 DISPLAY
                 "REGISTRO BLOQUEADO POR OUTRO USUARIO. AGUARDE..."
                 AT 2401
                 ACCEPT PAUSA AT 2478
              END-IF
           END-PERFORM
           IF WS-RESULTADO-ACESSO NOT = 00 AND 02 AND 23 AND 10
              DISPLAY "ERRO NA LEITURA - ALUNOS:" AT 2401
              DISPLAY WS-RESULTADO-ACESSO         AT 2440
              ACCEPT PAUSA                        AT 2478
              DISPLAY LIMPA-TELA
           END-IF
           .
       FIM-LER-ARQUIVO.
           EXIT
           .
       INCLUIR.
           INITIALIZE AUX-REGISTRO-VENDEDOR
           IF VEN-CODIGO NOT EQUAL ZEROS
              PERFORM UNTIL AUX-CPF NOT = ZEROS
                 ACCEPT AUX-CPF AT 0524
                 MOVE AUX-CPF TO VEN-CPF
                 IF AUX-CPF = ZEROS
                    DISPLAY "O CPF E OBRIGATORIO!" AT 2401
                 END-IF
              END-PERFORM
              MOVE AUX-CPF TO AREA-CPF
              PERFORM VALIDA-CPF
                 THRU F-VALIDA-CPF
              IF WS-ERRO-CPF = "S"
                 PERFORM UNTIL WS-ERRO-CPF = "N"
                    ACCEPT AUX-CPF AT 0524
                    PERFORM VALIDA-CPF
                       THRU F-VALIDA-CPF
                    MOVE AUX-CPF TO VEN-CPF
                 END-PERFORM
              END-IF

              DISPLAY LIMPA-LINHA               AT 2401
              ACCEPT AUX-NOME           AT 0624
              MOVE AUX-NOME TO VEN-NOME
              ACCEPT AUX-LATITUDE             AT 0724
              MOVE AUX-LATITUDE TO VEN-LATITUDE
              ACCEPT AUX-LONGITUDE             AT 0824
              MOVE AUX-LONGITUDE TO VEN-LONGITUDE
              PERFORM GRAVAR
           ELSE
              DISPLAY LIMPA-TELA
              DISPLAY "FIM DE CADASTRO" AT 1010
              ACCEPT PAUSA
           END-IF
           DISPLAY LIMPA-TELA
           .
       GRAVAR.
           WRITE REGISTRO-VENDEDOR
           IF WS-RESULTADO-ACESSO NOT = 00
              DISPLAY "ERRO NO FECHAMENTO:" AT 2401
              DISPLAY WS-RESULTADO-ACESSO   AT 2440
              ACCEPT PAUSA
           END-IF
           DISPLAY LIMPA-TELA               AT 2401
           .
       CONSULTAR.
           PERFORM MOSTRAR-TELA
           MOVE VEN-CODIGO TO AUX-CODIGO
           DISPLAY AUX-CODIGO                     AT 0424
           MOVE VEN-CPF TO AUX-CPF
           DISPLAY AUX-CPF                       AT 0524
           MOVE VEN-NOME TO AUX-NOME
           DISPLAY AUX-NOME               AT 0624
           MOVE VEN-LATITUDE TO AUX-LATITUDE
           DISPLAY AUX-LATITUDE                   AT 0724
           MOVE VEN-LONGITUDE TO AUX-LONGITUDE
           DISPLAY AUX-LONGITUDE                  AT 0824
           DISPLAY
           "INFORME: (A)LTERAR (E)XCLUIR (P)ROXIMO ENTER(CONTINUAR)"
                                                   AT 2401
           INITIALIZE OPCAO
           ACCEPT OPCAO AT 2478
           .
       EXECUTAR-OPCAO.
           EVALUATE OPCAO
               WHEN "A"
                   PERFORM ALTERAR
               WHEN "E"
                   PERFORM EXCLUIR
               WHEN "P"
                   PERFORM LER-PROXIMO
      *        WHEN "I"
      *            PERFORM INCLUIR
           END-EVALUATE
           .
       LER-PROXIMO.
           INITIALIZE CONTROLE-FIM
           PERFORM UNTIL CONTROLE-FIM = 1
               MOVE "S" TO TIPO-LEITURA
               PERFORM LER-ARQUIVO
               IF CONTROLE-FIM NOT = 1
                   PERFORM CONSULTAR
                   IF OPCAO NOT = "P"
                       MOVE 1 TO CONTROLE-FIM
                   END-IF
               END-IF
           END-PERFORM
           .
       ALTERAR.
           PERFORM MOSTRAR-TELA
           ACCEPT AUX-CODIGO                           AT 0424
           MOVE AUX-CODIGO TO VEN-CODIGO
           ACCEPT AUX-CPF                             AT 0524
           PERFORM UNTIL WS-ERRO-CPF = "N"
              MOVE AUX-CPF     TO AREA-CPF
              PERFORM VALIDA-CPF
                 THRU F-VALIDA-CPF
              IF WS-ERRO-CPF = "S"
                 ACCEPT AUX-CPF AT 0524
              END-IF
           END-PERFORM


           MOVE AUX-CPF TO VEN-CPF
           ACCEPT AUX-NOME                     AT 0624
           MOVE AUX-NOME TO VEN-NOME
           ACCEPT AUX-LATITUDE                         AT 0724
           MOVE AUX-LATITUDE TO VEN-LATITUDE
           ACCEPT AUX-LONGITUDE                        AT 0824
           MOVE AUX-LONGITUDE TO VEN-LONGITUDE
           REWRITE REGISTRO-VENDEDOR
           IF WS-RESULTADO-ACESSO NOT = 00 AND 02 THEN
              DISPLAY "ERRO NA ATUALIZACAO - VENDEDORES:" AT 2401
              DISPLAY WS-RESULTADO-ACESSO               AT 2440
              ACCEPT PAUSA                              AT 2478
              DISPLAY LIMPA-TELA                        AT 2401
           END-IF
           .
       EXCLUIR.
           DELETE ARQ-VENDEDOR
           IF WS-RESULTADO-ACESSO NOT = 00
              DISPLAY "ERRO NA EXCLUSAO - VENDEDORES:" AT 2401
              DISPLAY WS-RESULTADO-ACESSO            AT 2440
              ACCEPT PAUSA                           AT 2478
              DISPLAY LIMPA-TELA                     AT 2401
           END-IF
           .
       TRATA-IMPORTA.
           MOVE 2 TO  LK-TIPO-DADO
           DISPLAY LIMPA-TELA
           CALL "IMPORTACAO" USING DATA-DE-HOJE
                                   LK-TIPO-DADO
           CANCEL "IMPORTACAO".


       F-TRATA-IMPORTA. EXIT.

       COPY "VALIDA-CPF.cpy".


