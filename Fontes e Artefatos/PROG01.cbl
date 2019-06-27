       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG01.
       AUTHOR. DIEGO H.
       DATE-WRITTEN. 24/06/2019.
      *      ******** PROGRAMA MANUTENÇÃO CLIENTE *********             *

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

       DATA DIVISION.
       FILE SECTION.
       COPY "ARQ-CLIENTE.FD".

       WORKING-STORAGE SECTION.

       copy "AREA-CNPJ.cpy".

       01 AUX-REGISTRO-CLIENTE.
          02 AUX-CODIGO            PIC 9(07).
          02 AUX-CNPJ              PIC 9(14).
          02 AUX-RAZAO-SOCIAL      PIC X(40).
          02 AUX-LATITUDE          PIC s9(03)V9(08).
          02 AUX-LONGITUDE         PIC s9(03)V9(08).

       77 LINHA-TRACO           PIC X(80) VALUE ALL '-'.
       77 LIMPA-LINHA           PIC X(80) VALUE SPACES.
       77 WID-ARQ-CLIENTE       PIC X(50) VALUE SPACES.
       77 WS-RESULTADO-ACESSO   PIC 9(02) VALUES ZEROS.
       77 TIPO-LEITURA          PIC X(02) VALUES SPACES.
       77 CONTROLE-FIM          PIC 9(02) VALUES ZEROS.
       77 OPCAO                 PIC A(01) VALUES SPACES.
       77 PAUSA                 PIC X(02) VALUES SPACES.
       77 LK-TIPO-DADO          PIC 9(01). *> 01 - CLIENTE 02 - VENDEDOR
       77 WS-RESPOSTA           PIC X(01) VALUE SPACES.
       77 MASCARA-DATA-CADASTRO PIC 99/99/99.
       77 WS-CNPJ-DUPLICADO     PIC X(01) VALUE SPACES.
       77 TIPO-VALIDACAO PIC 9(01).
       77 NUMERO-CPF     PIC 9(11).
       77 NUMERO-CNPJ    PIC 9(14).
       77 RESPOSTA       PIC 9(01).

       LINKAGE SECTION.
       77 DATA-DE-HOJE          PIC 99/99/99.

       SCREEN SECTION.
       01 LIMPA-TELA   BLANK SCREEN
                       BACKGROUND-COLOR 1
                       FOREGROUND-COLOR 7.

       01 TELA-CLIENTE BLANK SCREEN
                       BACKGROUND-COLOR 1
                       FOREGROUND-COLOR 7.
          02 LINE 01 COLUMN 01 PIC X(80) FROM LINHA-TRACO.
          02 LINE 02 COLUMN 01 PIC X(08) FROM DATA-DE-HOJE.
          02 LINE 02 COLUMN 25 VALUE
             "     Cadastro de Clientes     ".
          02 LINE 02 COLUMN 73 VALUE "PROG01".
          02 LINE 03 COLUMN 01 PIC X(80) FROM LINHA-TRACO.
          02 LINE 04 COLUMN 01 VALUE "CODIGO DO CLIENTE.....".
          02 LINE 05 COLUMN 01 VALUE "CNPJ .................".
          02 LINE 06 COLUMN 01 VALUE "RAZAO SOCIAL..........".
          02 LINE 07 COLUMN 01 VALUE "LATITUDE..............".
          02 LINE 08 COLUMN 01 VALUE "LONGITUDE.............".
          02 LINE 09 COLUMN 01 PIC X(80) FROM LINHA-TRACO.

       PROCEDURE DIVISION USING DATA-DE-HOJE.

       INICIO.

      *----------VERIFICA SE O USUARIO QUER IMPORTAR O ARQUIVO----------*
           DISPLAY "DESEJA IMPORTAR UM ARQUIVO? S/N " AT 2401
           PERFORM UNTIL WS-RESPOSTA = "S" OR = "N" or = "n" or = "s"
              ACCEPT WS-RESPOSTA AT 2433
           END-PERFORM
           IF WS-RESPOSTA = "S"
              PERFORM TRATA-IMPORTA
                 THRU F-TRATA-IMPORTA
              EXIT PROGRAM
           END-IF
      *------ VERIFICAÇÃO PARA INCLUSAO OU ALTERAÇAO NO CADASTRO -------*
           MOVE "CLIENTE.DAT"   TO WID-ARQ-CLIENTE
           OPEN I-O ARQ-CLIENTE
           IF WS-RESULTADO-ACESSO NOT = 00
              OPEN OUTPUT ARQ-CLIENTE
              CLOSE ARQ-CLIENTE
              OPEN I-O ARQ-CLIENTE
           END-IF
      *------ VERIFICAÇÃO DO PROXIMO CODIGO CLIENTE A SER LANÇADO ------*
           MOVE 9999999 TO CLI-CODIGO
           PERFORM UNTIL CLI-CODIGO = ZEROS
              MOVE 9999999 TO CLI-CODIGO
              START ARQ-CLIENTE KEY LESS CLI-CODIGO
              IF WS-RESULTADO-ACESSO NOT = 00
                 DISPLAY "ERRO NO POSICIONAMENTO DA CHAVE - CLIENTE: "
                      AT 2401
                 DISPLAY WS-RESULTADO-ACESSO AT 2440
                 ACCEPT  PAUSA               AT 2478
                 DISPLAY LIMPA-TELA          AT 2401
              END-IF
              READ ARQ-CLIENTE NEXT AT END
                MOVE ZEROS TO CLI-CODIGO
              END-READ
              ADD 1 TO CLI-CODIGO
              MOVE CLI-CODIGO TO AUX-CODIGO
      *------ INICIO DO PROCESSO ---------------------------------------*
              MOVE 1 TO CLI-CODIGO
              PERFORM MOSTRAR-TELA
              ACCEPT AUX-CODIGO AT 0424
              MOVE AUX-CODIGO TO CLI-CODIGO
              IF CLI-CODIGO NOT EQUAL ZEROS THEN
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
           CLOSE ARQ-CLIENTE
           IF WS-RESULTADO-ACESSO NOT = 0
              DISPLAY "ERRO NO FECHAMENTO;" AT 2401
              DISPLAY WS-RESULTADO-ACESSO        AT 2421
           END-IF
           EXIT PROGRAM
           .
       MOSTRAR-TELA.
           DISPLAY TELA-CLIENTE AT 0101
           .
       LER-ARQUIVO.
           MOVE 99 TO WS-RESULTADO-ACESSO
           PERFORM UNTIL WS-RESULTADO-ACESSO NOT = 99
              IF TIPO-LEITURA = "I"
                 READ ARQ-CLIENTE
                 IF WS-RESULTADO-ACESSO NOT = 00
                    MOVE 1 TO CONTROLE-FIM
                 END-IF
              ELSE
                 READ ARQ-CLIENTE NEXT AT END
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
           INITIALIZE AUX-REGISTRO-CLIENTE
           IF CLI-CODIGO NOT EQUAL ZEROS
              PERFORM UNTIL AUX-CNPJ NOT = ZEROS
                 ACCEPT AUX-CNPJ AT 0524
                 IF AUX-CNPJ = ZEROS
                    DISPLAY "O CNPJ E OBRIGATORIO!" AT 2401
                 END-IF
              END-PERFORM
              MOVE AUX-CNPJ TO WS-CGC-R
              PERFORM VALIDA-CNPJ
                 THRU F-VALIDA-CNPJ
              IF WS-ERRO-CNPJ = "S"
                 PERFORM UNTIL WS-ERRO-CNPJ = "N"
                    ACCEPT AUX-CNPJ AT 0524
                    MOVE AUX-CNPJ TO WS-CGC-R
                    PERFORM VALIDA-CNPJ
                       THRU F-VALIDA-CNPJ
                 END-PERFORM
              END-IF
              DISPLAY LIMPA-LINHA               AT 2401

              MOVE AUX-CNPJ TO CLI-CNPJ
              ACCEPT AUX-RAZAO-SOCIAL           AT 0624
              MOVE AUX-RAZAO-SOCIAL TO CLI-RAZAO-SOCIAL
              ACCEPT AUX-LATITUDE             AT 0724
              MOVE AUX-LATITUDE TO CLI-LATITUDE
              ACCEPT AUX-LONGITUDE             AT 0824
              MOVE AUX-LONGITUDE TO CLI-LONGITUDE
              PERFORM GRAVAR
           ELSE
              DISPLAY LIMPA-TELA
              DISPLAY "FIM DE CADASTRO" AT 1010
              ACCEPT PAUSA
           END-IF
           DISPLAY LIMPA-TELA
           .
       GRAVAR.
           WRITE REGISTRO-CLIENTE
           IF WS-RESULTADO-ACESSO NOT = 00
              DISPLAY "ERRO NO FECHAMENTO:" AT 2401
              DISPLAY WS-RESULTADO-ACESSO   AT 2440
              ACCEPT PAUSA
           END-IF
           DISPLAY LIMPA-TELA               AT 2401
           .
       CONSULTAR.
           PERFORM MOSTRAR-TELA
           MOVE CLI-CODIGO TO AUX-CODIGO
           DISPLAY AUX-CODIGO                     AT 0424
           MOVE CLI-CNPJ TO AUX-CNPJ
           DISPLAY AUX-CNPJ                       AT 0524
           MOVE CLI-RAZAO-SOCIAL TO AUX-RAZAO-SOCIAL
           DISPLAY AUX-RAZAO-SOCIAL               AT 0624
           MOVE CLI-LATITUDE TO AUX-LATITUDE
           DISPLAY AUX-LATITUDE                   AT 0724
           MOVE CLI-LONGITUDE TO AUX-LONGITUDE
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
           MOVE AUX-CODIGO TO CLI-CODIGO
           ACCEPT AUX-CNPJ                             AT 0524
           PERFORM UNTIL WS-ERRO-CNPJ = "N"
           move AUX-CNPJ to WS-CGC-R
              PERFORM VALIDA-CNPJ
                 THRU F-VALIDA-CNPJ
              IF WS-ERRO-CNPJ = "S"
                 ACCEPT AUX-CNPJ AT 0524
              END-IF
           END-PERFORM
           MOVE AUX-CNPJ TO CLI-CNPJ
           ACCEPT AUX-RAZAO-SOCIAL                     AT 0624
           MOVE AUX-RAZAO-SOCIAL TO CLI-RAZAO-SOCIAL
           ACCEPT AUX-LATITUDE                         AT 0724
           MOVE AUX-LATITUDE TO CLI-LATITUDE
           ACCEPT AUX-LONGITUDE                        AT 0824
           MOVE AUX-LONGITUDE TO CLI-LONGITUDE
           REWRITE REGISTRO-CLIENTE
           IF WS-RESULTADO-ACESSO NOT = 00 AND 02 THEN
              DISPLAY "ERRO NA ATUALIZACAO - CLIENTES:" AT 2401
              DISPLAY WS-RESULTADO-ACESSO               AT 2440
              ACCEPT PAUSA                              AT 2478
              DISPLAY LIMPA-TELA                        AT 2401
           END-IF
           .
       EXCLUIR.
           DELETE ARQ-CLIENTE
           IF WS-RESULTADO-ACESSO NOT = 00
              DISPLAY "ERRO NA EXCLUSAO - CLIENTES:" AT 2401
              DISPLAY WS-RESULTADO-ACESSO            AT 2440
              ACCEPT PAUSA                           AT 2478
              DISPLAY LIMPA-TELA                     AT 2401
           END-IF
           .
       TRATA-IMPORTA.
           MOVE 1 TO  LK-TIPO-DADO
           DISPLAY LIMPA-TELA
           CALL "IMPORTACAO" USING DATA-DE-HOJE
                                   LK-TIPO-DADO
           CANCEL "IMPORTACAO".


       F-TRATA-IMPORTA. EXIT.
       copy "VALIDA-CNPJ.cpy".
