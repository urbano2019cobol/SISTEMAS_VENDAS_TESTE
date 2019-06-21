       IDENTIFICATION DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID.    VENDEDORES.
      * AUTHOR.        ALBERI NUNES.
      * DATE-WRITTEN.  21/06/2019.
      * REMARKS.
      *----------------------------------------------------------------*
      * SISTEMA:      VENDAS
      * PROGRAMA:     MANUTENÇÃO DE VENDEDORES
      *
      * OBJETIVO:     MANUTENÇÃO DE VENDEDORES
      *
      * VERSOES:      DATA        DESCRICAO
      *               ----------  --------------------------------------
      *               21/06/2019  MANTER VENDEDORES
      *



      *===============================================================*
       ENVIRONMENT DIVISION.
      *----------------------------------------------------------------*
       CONFIGURATION SECTION.
        SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FILE1 ASSIGN TO DISK
                 "vendedores.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               FILE STATUS IS FS-STAT
               RECORD KEY IS FS-KEY
               ALTERNATE RECORD KEY FS-CPF.

           SELECT FILE2 ASSIGN TO DISK WID-ARQUIVO-IMP
      *          "vendedores_imp.txt"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-STAT.

       DATA DIVISION.
       FILE SECTION.
       FD FILE1.
       01 FILE1-REC.
           05 FS-KEY.
               10 FS-CODIGO PIC 9(003).
           05 FS-NOME       PIC X(040).
           05 FS-CPF        PIC 9(011).
           05 FS-LATITUDE   PIC s9(003)v9(008).
           05 FS-LONGITUDE  PIC s9(003)v9(008).
           05 FILLER        PIC X(20).

       FD FILE2.
       01 FILE2-REC.
           05 FS2-KEY.
               10 FS2-CODIGO PIC 9(003).
           05 FS2-NOME       PIC X(040).
           05 FS2-CPF        PIC 9(011).
           05 FS2-LATITUDE   PIC s9(003)v9(008).
           05 FS2-LONGITUDE  PIC s9(003)v9(008).
           05 FILLER        PIC X(20).

       WORKING-STORAGE SECTION.
       01  VARIAVEIS-SISTEMA.
           03  WS-LIMPA       PIC X(100) VALUE SPACES.
           03  WS-CONT        PIC 9(03) VALUE ZEROS.
           03  ED-CONT        PIC ZZ9.
       01  WS-DATA.
           03 WS-ANO          PIC 9(02) VALUE ZEROS.
           03 WS-MES          PIC 9(02) VALUE ZEROS.
           03 WS-DIA          PIC 9(02) VALUE ZEROS.
       01 WS-HORA.
           03  WS-HOR         PIC 9(02) VALUE ZEROS.
           03  WS-MIN         PIC 9(02) VALUE ZEROS.
           03  WS-SEG         PIC 9(02) VALUE ZEROS.
           03  WS-CSE         PIC 9(02) VALUE ZEROS.
       01 WS-MODULO.
           03 FILLER PIC X(13) VALUE "VENDEDORES - ".
           03 WS-OP PIC  X(20) VALUE SPACES.

       01 WS-CPF-MS.
           03 WS-CPF-MS1  PIC X(03).
           03 FILLER      PIC X(01) VALUE ".".
           03 WS-CPF-MS2  PIC X(03).
           03 FILLER      PIC X(01) VALUE ".".
           03 WS-CPF-MS3  PIC X(03).
           03 FILLER      PIC X(01) VALUE "-".
           03 WS-CPF-MS4  PIC X(02).

       01 WS-CPF-TT PIC 9(03) VALUE ZEROS.
       01 WS-CPF-QC PIC 9(03) VALUE ZEROS.
       01 WS-CPF-RS PIC 9(02) VALUE ZEROS.
       01 WS-CPF-D1 PIC 9(01) VALUE ZEROS.
       01 WS-CPF-D2 PIC 9(01) VALUE ZEROS.
       01 WS-CPF-ORI PIC 9(11) VALUE ZEROS.
       01 WS-CPF    PIC 9(11) VALUE ZEROS.
       01 FILLER REDEFINES WS-CPF.
           03 WS-CPF-P01.
                05 WS-CPF-01 PIC 9(01).
                05 WS-CPF-02 PIC 9(01).
                05 WS-CPF-03 PIC 9(01).
           03 WS-CPF-P02.
                05 WS-CPF-04 PIC 9(01).
                05 WS-CPF-05 PIC 9(01).
                05 WS-CPF-06 PIC 9(01).
           03 WS-CPF-P03.
                05 WS-CPF-07 PIC 9(01).
                05 WS-CPF-08 PIC 9(01).
                05 WS-CPF-09 PIC 9(01).
           03 WS-CPF-P04.
                05 WS-CPF-10 PIC 9(01).
                05 WS-CPF-11 PIC 9(01).

       01  WS-ARQIMP PIC X(60) VALUE SPACES.

       77 ST-ERRO PIC X(02) VALUE "00".
       77 MENS1   PIC X(01).
       77 WS-OPCAO PIC X.
           88 E-INCLUIR   VALUE IS "1".
           88 E-CONSULTAR VALUE IS "2".
           88 E-ALTERAR   VALUE IS "3".
           88 E-EXCLUIR   VALUE IS "4".
           88 E-IMPORTAR  VALUE IS "5".
           88 E-ENCERRAR  VALUE IS "X" "x".
       77 FS-STAT PIC 9(02).
           88 FS-OK         VALUE ZEROS.
           88 FS-CANCELA    VALUE 99.
           88 FS-NAO-EXISTE VALUE 35.
       77 WS-ERRO PIC X.
           88 E-SIM VALUES ARE "S" "s".

       77 FS-EXIT PIC 9(02) VALUE ZEROS.
           88 FS-PROCESSA   VALUE 0.
           88 FS-TERMINA    VALUE 99.

       77 WS-NUML PIC 999.
       77 WS-NUMC PIC 999.
       77 COR-FUNDO PIC 9 VALUE 1.
       77 COR-FRENTE PIC 9 VALUE 6.

       77 WS-STATUS PIC X(30).
       77 WS-MSGERRO PIC X(100).

       COPY screenio.

       SCREEN SECTION.
       01 SS-CLS.
           05 SS-FILLER.
               10 BLANK SCREEN.
               10 LINE 01 COLUMN 01 ERASE EOL
                  BACKGROUND-COLOR COR-FUNDO.
               10 LINE WS-NUML COLUMN 01 ERASE EOL
                  BACKGROUND-COLOR COR-FUNDO.
           05 SS-CABECALHO.
               10 LINE 01 COLUMN 02 PIC X(31) FROM WS-MODULO
                  HIGHLIGHT FOREGROUND-COLOR COR-FRENTE
                  BACKGROUND-COLOR COR-FUNDO.
           05 SS-STATUS.
               10 LINE WS-NUML COLUMN 2 ERASE EOL PIC X(30)
                  FROM WS-STATUS HIGHLIGHT
                  FOREGROUND-COLOR COR-FRENTE
                  BACKGROUND-COLOR COR-FUNDO.
       01 SS-MENU FOREGROUND-COLOR 6.
           05 LINE 04 COLUMN 10 VALUE
                "CADASTRO DE VENDEDORES HBSIS"
                HIGHLIGHT .
           05 LINE 05 COLUMN 10 VALUE
                "============================ "
                HIGHLIGHT .
           05 LINE 07 COLUMN 15 VALUE "1 - INCLUIR".
           05 LINE 08 COLUMN 15 VALUE "2 - CONSULTAR".
           05 LINE 09 COLUMN 15 VALUE "3 - ALTERAR".
           05 LINE 10 COLUMN 15 VALUE "4 - EXCLUIR".
           05 LINE 11 COLUMN 15 VALUE "5 - IMPORTAR".
           05 LINE 12 COLUMN 15 VALUE "X - ENCERRAR".
           05 LINE 14 COLUMN 15 VALUE "OPCAO: ".
           05 LINE 14 COL PLUS 1 USING WS-OPCAO AUTO.

       01  SS-TELA-REGISTRO.
           02  BLANK SCREEN.
           02  LINE  01  COLUMN   01  VALUE "DATA:".
           02  LINE  01  COLUMN  PLUS 2 USING  WS-DIA.
           02  LINE  01  COLUMN  PLUS 1  VALUE "/".
           02  LINE  01  COLUMN  PLUS 1 USING  WS-MES.
           02  LINE  01  COLUMN  PLUS 1  VALUE "/".
           02  LINE  01  COLUMN  PLUS 1 USING  WS-ANO.
           02  LINE  01 COLUMN   29  VALUE
                "Cadastro de Vendedores HBSIS".

           01  SS-CHAVE.
               05  LINE  05 COLUMN 07  VALUE
                "Codigo.................:".
               05  T-CODIGO  LINE  05  COLUMN 34 PIC 9(03)
                USING  FS-CODIGO    HIGHLIGHT .

           01  SS-DADOS.
               05  LINE  07 COLUMN 07  VALUE
                    "Nome Vendedor..........:".
               05  LINE  09 COLUMN 07  VALUE
                    "CPF....................:".
               05  LINE  11 COLUMN 07 VALUE
                    "Latitude...............:".
               05  LINE  13 COLUMN 07  VALUE
                    "Longitude.............:".
               05  LINE 21 COLUMN  07  VALUE
                    "MENSAGEM: ".

               05  T-NOME LINE  07  COLUMN 34  PIC X(40)
                            USING FS-NOME    HIGHLIGHT.
               05  T-CPF  LINE  09  COLUMN 34  PIC 9(11)
                            USING FS-CPF    HIGHLIGHT.
               05  T-LATID LINE 11  COLUMN 34  PIC ZZ9,99999999
                            USING FS-LATITUDE   HIGHLIGHT.
               05  T-LONGI LINE 13  COLUMN 34  PIC ZZ9,99999999
                            USING FS-LONGITUDE   HIGHLIGHT.

           01  SS-IMPORTACAO.
               05  LINE  05 COLUMN 07  VALUE
                "Arquivo para importar ...:".
               05  T-ARQUIVO  LINE  05  COLUMN 34 PIC X(60)
                USING  WS-ARQIMP    HIGHLIGHT .
               05  LINE 21 COLUMN  07  VALUE
                     "MENSAGEM: ".

       01 SS-ERRO.
           05 FILLER FOREGROUND-COLOR 4 BACKGROUND-COLOR 1 HIGHLIGHT.
               10 LINE WS-NUML COLUMN 2 PIC X(80) FROM WS-MSGERRO BELL.
               10 COLUMN PLUS 2 TO WS-ERRO.

       PROCEDURE DIVISION.
       0000-CONTROLE SECTION.
       0000.
           PERFORM 1000-INICIO.
           PERFORM 2000-PROCESSO UNTIL E-ENCERRAR.
           PERFORM 8000-FINALIZA THRU 8000-FINALIZA-FIM.
      *    STOP RUN.
           GOBACK.

       0000-EXIT.
           EXIT.

       1000-INICIO SECTION.
       1000.
           SET ENVIRONMENT 'COB_SCREEN_EXCEPTIONS' TO 'Y'.
           SET ENVIRONMENT 'COB_SCREEN_ESC' TO 'Y'.
           SET ENVIRONMENT 'ESCDELAY' TO '25'.
           ACCEPT    WS-NUML FROM LINES
           ACCEPT    WS-NUMC FROM COLUMNS
           DISPLAY   SS-TELA-REGISTRO
           PERFORM  9000-ABRIR-ARQUIVOS
             THRU   9000-ABRIR-ARQUVOS-FIM.

       1000-EXIT.
           EXIT.

       2000-PROCESSO SECTION.
       2000.
           MOVE "MENU" TO WS-OP
           MOVE "ESCOLHA A OPCAO" TO WS-STATUS
           MOVE SPACES TO WS-OPCAO
           DISPLAY SS-TELA-REGISTRO
           DISPLAY SS-CLS
           ACCEPT SS-MENU
           ACCEPT WS-HORA FROM TIME
           ACCEPT WS-DATA FROM DATE
           MOVE "INCLUSÃO" TO WS-OP
           MOVE "ESC PARA ENCERRAR" TO WS-STATUS
           DISPLAY SS-CLS
           MOVE ZEROS TO FS-EXIT.
           DISPLAY SS-TELA-REGISTRO
           DISPLAY SS-CHAVE
           DISPLAY SS-DADOS
           EVALUATE TRUE
                WHEN E-INCLUIR
                     PERFORM 2100-INCLUIR THRU 2100-INCLUIR-FIM
                        UNTIL COB-CRT-STATUS = COB-SCR-ESC

                WHEN E-CONSULTAR
                     PERFORM 3000-CONSULTA THRU 3000-CONSULTA-FIM
                       UNTIL COB-CRT-STATUS = COB-SCR-ESC

                WHEN E-ALTERAR
                     PERFORM 4000-ALTERAR THRU 4000-ALTERAR-FIM
                       UNTIL COB-CRT-STATUS = COB-SCR-ESC

                WHEN E-EXCLUIR
                     PERFORM 5000-EXCLUIR THRU 5000-EXCLUIR-FIM
                       UNTIL COB-CRT-STATUS = COB-SCR-ESC

                WHEN E-IMPORTAR
                     PERFORM 6000-IMPORTAR THRU 6000-IMPORTAR-FIM
                       UNTIL COB-CRT-STATUS = COB-SCR-ESC

           END-EVALUATE.

       2000-PROCESSO-FIM.
           EXIT.

      * -----------------------------------
       2100-INCLUIR SECTION.
       2100.
           MOVE "INCLUSAO" TO WS-OP.
           MOVE "ESC PARA ENCERRAR" TO WS-STATUS.
           DISPLAY SS-CLS.
           DISPLAY SS-TELA-REGISTRO.
           DISPLAY SS-CHAVE.
           DISPLAY SS-DADOS.

           INITIALIZE FILE1-REC.

       2100-CODIGO.
           MOVE ZEROS TO FS-EXIT.
           PERFORM UNTIL FS-TERMINA
                   OR COB-CRT-STATUS = COB-SCR-ESC

           ACCEPT T-CODIGO
           IF FS-CODIGO EQUAL ZEROS
               MOVE "FAVOR INFORMAR CODIGO" TO WS-MSGERRO
               DISPLAY WS-MSGERRO at 2118
           ELSE
               READ FILE1 KEY IS FS-CODIGO
               IF FS-STAT = "00"
                  INITIALIZE FILE1-REC
                  MOVE "VENDEDOR JA EXISTE. INFORME NOVO CODIGO"
                   TO WS-MSGERRO
                  DISPLAY WS-MSGERRO at 2118
               ELSE
                  MOVE 99 TO FS-EXIT
               END-IF
           END-IF
           END-PERFORM.

       2100-NOME.
           PERFORM UNTIL FS-NOME NOT EQUAL SPACES
                   OR COB-CRT-STATUS = COB-SCR-ESC
              ACCEPT T-NOME
              IF FS-NOME EQUAL SPACES
                  MOVE "FAVOR INFORMAR NOME VENDEDOR " TO WS-MSGERRO
                  DISPLAY WS-MSGERRO at 2118
              END-IF
           END-PERFORM.

       2100-CPF.
           MOVE ZEROS TO FS-EXIT.
           PERFORM UNTIL FS-TERMINA
                   OR COB-CRT-STATUS = COB-SCR-ESC
           DISPLAY WS-LIMPA AT 0934
           DISPLAY FS-CPF AT 0934
           ACCEPT T-CPF
           IF FS-CPF EQUAL ZEROS OR ALL "1" OR ALL "2" OR ALL "3"
                     OR ALL "4" OR ALL "5" OR ALL "6" OR ALL "7"
                     OR ALL "8" OR ALL"9"
               MOVE "CPF INVALIDO. FAVOR INFORMAR CPF" TO WS-MSGERRO
               DISPLAY WS-MSGERRO at 2118
               MOVE ZEROS TO FS-EXIT
           ELSE
               DISPLAY WS-LIMPA AT 2118
               PERFORM 2200-CALCULA-CPF
               IF FS-CPF NOT EQUAL WS-CPF
                  MOVE "CPF INVALIDO. FAVOR INFORMAR CPF" TO
                       WS-MSGERRO
                  PERFORM 9900-MOSTRA-ERRO
                     THRU 9900-MOSTRA-ERRO-FIM
                  MOVE ZEROS TO FS-EXIT
               ELSE
                  MOVE FS-CPF (1:3)  TO WS-CPF-MS1
                  MOVE FS-CPF (4:3)  TO WS-CPF-MS2
                  MOVE FS-CPF (7:3)  TO WS-CPF-MS3
                  MOVE FS-CPF (10:2) TO WS-CPF-MS4

                  DISPLAY WS-CPF-MS AT 0934

                  READ FILE1 KEY IS FS-CPF
                  IF FS-STAT = "00"
                     MOVE "VENDEDOR JA EXISTE PARA CPF INFORMADO" TO
                          WS-MSGERRO
                     DISPLAY WS-MSGERRO at 2118
                     MOVE ZEROS TO FS-EXIT
                  ELSE
                     MOVE 99 TO FS-EXIT
                  END-IF
               END-IF
           END-IF
           END-PERFORM.

           MOVE ZEROS TO FS-EXIT.
           PERFORM UNTIL FS-LATITUDE NOT EQUAL ZEROS
                   OR COB-CRT-STATUS = COB-SCR-ESC
              ACCEPT T-LATID
              IF FS-LATITUDE EQUAL SPACES OR ZEROS
                 MOVE "FAVOR INFORMAR LATITUDE " TO WS-MSGERRO
                 DISPLAY WS-MSGERRO at 2118
              END-IF
           END-PERFORM.

           PERFORM UNTIL FS-LONGITUDE NOT EQUAL ZEROS
                   OR COB-CRT-STATUS = COB-SCR-ESC
              ACCEPT T-LONGI
              IF FS-LONGITUDE EQUAL SPACES OR ZEROS
                 MOVE "FAVOR INFORMAR LONGITUDE " TO WS-MSGERRO
                 DISPLAY WS-MSGERRO at 2118
              END-IF
           END-PERFORM.

           IF COB-CRT-STATUS = COB-SCR-ESC
              MOVE 99 TO FS-EXIT
              PERFORM 2100-INCLUIR-FIM
           END-IF.

           IF FS-PROCESSA
              PERFORM 2300-GRAVAR
              PERFORM 2400-LIMPA-DADOS
              MOVE "VENDEDOR INCLUIDO COM SUCESSO" TO WS-MSGERRO
              DISPLAY WS-MSGERRO at 2118
              MOVE ZEROS TO FS-EXIT
           END-IF.

       2100-INCLUIR-FIM.
           EXIT.

       2200-CALCULA-CPF SECTION.
       2200.
           MOVE FS-CPF TO WS-CPF.
           COMPUTE WS-CPF-TT = (WS-CPF-01 * 10) +
                               (WS-CPF-02 * 9) +
                               (WS-CPF-03 * 8) +
                               (WS-CPF-04 * 7) +
                               (WS-CPF-05 * 6) +
                               (WS-CPF-06 * 5) +
                               (WS-CPF-07 * 4) +
                               (WS-CPF-08 * 3) +
                               (WS-CPF-09 * 2).
           DIVIDE WS-CPF-TT BY 11 GIVING WS-CPF-QC
                                  REMAINDER WS-CPF-RS.
           IF WS-CPF-RS < 2
              MOVE ZEROS TO WS-CPF-10
           ELSE
              COMPUTE WS-CPF-10 =  11 - WS-CPF-RS
           END-IF.

      *    CALCULAR SEGUNDO DIGITO
           COMPUTE WS-CPF-TT = (WS-CPF-01 * 11) +
                               (WS-CPF-02 * 10) +
                               (WS-CPF-03 * 9) +
                               (WS-CPF-04 * 8) +
                               (WS-CPF-05 * 7) +
                               (WS-CPF-06 * 6) +
                               (WS-CPF-07 * 5) +
                               (WS-CPF-08 * 4) +
                               (WS-CPF-09 * 3) +
                               (WS-CPF-10 * 2) .
           DIVIDE WS-CPF-TT BY 11 GIVING WS-CPF-QC
                                   REMAINDER WS-CPF-RS.
           IF WS-CPF-RS < 2
              MOVE ZEROS TO WS-CPF-11
           ELSE
              COMPUTE WS-CPF-11 = 11 - WS-CPF-RS
           END-IF.

       2200-CALCULA-CPF-FIM.
           EXIT.

       2300-GRAVAR SECTION.
       2300.
           WRITE FILE1-REC
           INVALID KEY
               MOVE "VENDEDOR JÁ EXISTE" TO WS-MSGERRO
               PERFORM 9900-MOSTRA-ERRO
                       THRU 9900-MOSTRA-ERRO-FIM
               MOVE ZEROS TO FS-KEY
           NOT INVALID KEY
               INITIALIZE FILE1-REC
               MOVE "VENDEDOR INCLUIDO COM SUCESSO" TO WS-MSGERRO
               PERFORM 9900-MOSTRA-ERRO
                       THRU 9900-MOSTRA-ERRO-FIM
           END-WRITE.

       2300-GRAVAR-FIM.
           EXIT.

       2400-LIMPA-DADOS SECTION.
       2400.
           DISPLAY WS-LIMPA at 0534.
           DISPLAY WS-LIMPA at 0734.
           DISPLAY WS-LIMPA at 0934.
           DISPLAY WS-LIMPA at 1134.
           DISPLAY WS-LIMPA at 1334.
           DISPLAY WS-LIMPA at 2118.
           DISPLAY WS-LIMPA at 2340.

       2400-LIMPA-DADOS-FIM.
           EXIT.

      * -----------------------------------
       3000-CONSULTA SECTION.
       3000.
           MOVE "CONSULTA" TO WS-OP.
           MOVE "ESC PARA ENCERRAR" TO WS-STATUS.
           DISPLAY SS-CLS.
           INITIALIZE FILE1-REC.
           DISPLAY SS-TELA-REGISTRO.
           DISPLAY SS-CHAVE.
           DISPLAY SS-DADOS.

       3000-CODIGO.
           MOVE ZEROS TO FS-EXIT.
           PERFORM UNTIL FS-TERMINA
                   OR COB-CRT-STATUS = COB-SCR-ESC

           ACCEPT T-CODIGO
           IF FS-CODIGO EQUAL ZEROS
               MOVE "FAVOR INFORMAR CODIGO" TO WS-MSGERRO
               DISPLAY WS-MSGERRO at 2118
           ELSE
               READ FILE1 KEY IS FS-CODIGO
               IF FS-STAT = "23"
                  INITIALIZE FILE1-REC
                  MOVE "VENDEDOR NAO CADASTRADO. INFORME NOVO CODIGO"
                       TO WS-MSGERRO
                  DISPLAY WS-MSGERRO at 2118
               ELSE
                  DISPLAY WS-LIMPA at 2118
                  DISPLAY SS-DADOS
                  MOVE FS-CPF (1:3)  TO WS-CPF-MS1
                  MOVE FS-CPF (4:3)  TO WS-CPF-MS2
                  MOVE FS-CPF (7:3)  TO WS-CPF-MS3
                  MOVE FS-CPF (10:2) TO WS-CPF-MS4

                  DISPLAY WS-CPF-MS AT 0934
                  MOVE FS-CPF TO WS-CPF-ORI

                  MOVE "S" TO WS-ERRO
                  MOVE "PRESSIONE ENTER PARA NOVA CONSULTA" TO
                       WS-MSGERRO
                  ACCEPT SS-ERRO
                  IF E-SIM
                     PERFORM 2400-LIMPA-DADOS
                     MOVE SPACES TO WS-MSGERRO
                     DISPLAY WS-MSGERRO at 2118
                     MOVE ZEROS TO FS-EXIT
                  ELSE
                     MOVE 99 TO FS-EXIT
                  END-IF
               END-IF
           END-IF
           END-PERFORM.

       3000-CONSULTA-FIM.
           EXIT.

      * -----------------------------------
       4000-ALTERAR SECTION.
       4000.
           MOVE "ALTERACAO" TO WS-OP.
           MOVE "ESC PARA ENCERRAR" TO WS-STATUS.
           DISPLAY SS-CLS.
           DISPLAY SS-TELA-REGISTRO.
           DISPLAY SS-CHAVE.
           DISPLAY SS-DADOS.

           INITIALIZE FILE1-REC.

       4000-CODIGO.
           MOVE ZEROS TO FS-EXIT.
           PERFORM UNTIL FS-TERMINA
                   OR COB-CRT-STATUS = COB-SCR-ESC

           ACCEPT T-CODIGO
           IF FS-CODIGO EQUAL ZEROS
               MOVE "FAVOR INFORMAR CODIGO" TO WS-MSGERRO
               DISPLAY WS-MSGERRO at 2118
           ELSE
               READ FILE1 KEY IS FS-CODIGO
               IF FS-STAT = "23"
                  INITIALIZE FILE1-REC
                  MOVE "VENDEDOR NAO CADASTRADO. INFORME NOVO CODIGO"
                   TO WS-MSGERRO
                  DISPLAY WS-MSGERRO at 2118
               ELSE
                  DISPLAY WS-LIMPA at 2118
                  DISPLAY SS-DADOS
                  MOVE FS-CPF (1:3)  TO WS-CPF-MS1
                  MOVE FS-CPF (4:3)  TO WS-CPF-MS2
                  MOVE FS-CPF (7:3)  TO WS-CPF-MS3
                  MOVE FS-CPF (10:2) TO WS-CPF-MS4

                  DISPLAY WS-CPF-MS AT 0934
                  MOVE FS-CPF TO WS-CPF-ORI
                  MOVE 99 TO FS-EXIT
               END-IF
           END-IF
           END-PERFORM.

       4000-NOME.
           MOVE ZEROS TO FS-EXIT.
           PERFORM UNTIL FS-TERMINA
                   OR COB-CRT-STATUS = COB-SCR-ESC
              ACCEPT T-NOME
              IF FS-NOME EQUAL SPACES
                  MOVE "FAVOR INFORMAR NOME " TO WS-MSGERRO
                  DISPLAY WS-MSGERRO at 2118
              ELSE
                  MOVE 99 TO FS-EXIT
              END-IF
           END-PERFORM.

       4000-CNPJ.
           MOVE ZEROS TO FS-EXIT.
           PERFORM UNTIL FS-TERMINA
                   OR COB-CRT-STATUS = COB-SCR-ESC
           DISPLAY WS-LIMPA AT 0934
           DISPLAY FS-CPF AT 0934
           ACCEPT T-CPF
           IF FS-CPF EQUAL ZEROS OR ALL "1" OR ALL "2" OR ALL "3"
                     OR ALL "4" OR ALL "5" OR ALL "6" OR ALL "7"
                     OR ALL "8" OR ALL"9"
               MOVE "CPF INVALIDO. FAVOR INFORMAR CPF" TO WS-MSGERRO
               DISPLAY WS-MSGERRO at 2118
               MOVE ZEROS TO FS-EXIT
           ELSE
               DISPLAY WS-LIMPA AT 2118
               PERFORM 2200-CALCULA-CPF
               IF FS-CPF NOT EQUAL WS-CPF
                  MOVE "CPF INVALIDO. FAVOR INFORMAR CPF" TO
                       WS-MSGERRO
                  PERFORM 9900-MOSTRA-ERRO
                     THRU 9900-MOSTRA-ERRO-FIM
                  MOVE ZEROS TO FS-EXIT
               ELSE
                  MOVE FS-CPF (1:3)  TO WS-CPF-MS1
                  MOVE FS-CPF (4:3)  TO WS-CPF-MS2
                  MOVE FS-CPF (7:3)  TO WS-CPF-MS3
                  MOVE FS-CPF (10:2)  TO WS-CPF-MS4

                  DISPLAY WS-CPF-MS AT 0934
                  IF FS-CPF NOT EQUAL WS-CPF-ORI
                     READ FILE1 KEY IS FS-CPF
                     IF FS-STAT = "00"
                        MOVE "VENDEDOR JA EXISTE PARA CPF INFORMADO" TO
                             WS-MSGERRO
                        DISPLAY WS-MSGERRO at 2118
                        MOVE ZEROS TO FS-EXIT
                     ELSE
                        MOVE 99 TO FS-EXIT
                     END-IF
                  ELSE
                     MOVE 99 TO FS-EXIT
                  END-IF
               END-IF
           END-IF
           END-PERFORM.

           MOVE ZEROS TO FS-EXIT.
           PERFORM UNTIL FS-TERMINA
                   OR COB-CRT-STATUS = COB-SCR-ESC
              ACCEPT T-LATID
              IF FS-LATITUDE EQUAL SPACES OR ZEROS
                 MOVE "FAVOR INFORMAR LATITUDE " TO WS-MSGERRO
                 DISPLAY WS-MSGERRO at 2118
              ELSE
                 MOVE 99 TO FS-EXIT
              END-IF
           END-PERFORM.

           MOVE ZEROS TO FS-EXIT.
           PERFORM UNTIL FS-TERMINA
                   OR COB-CRT-STATUS = COB-SCR-ESC
              ACCEPT T-LONGI
              IF FS-LONGITUDE EQUAL SPACES OR ZEROS
                 MOVE "FAVOR INFORMAR LONGITUDE " TO WS-MSGERRO
                 DISPLAY WS-MSGERRO at 2118
              ELSE
                  MOVE 99 TO FS-EXIT
              END-IF
           END-PERFORM.

           MOVE ZEROS TO FS-EXIT.
           IF COB-CRT-STATUS = COB-SCR-ESC
              MOVE 99 TO FS-EXIT
              PERFORM 4000-ALTERAR-FIM
           END-IF.

           IF FS-PROCESSA
              PERFORM 4100-REGRAVAR
              PERFORM 2400-LIMPA-DADOS
              MOVE "VENDEDOR ALTERADO COM SUCESSO" TO WS-MSGERRO
              DISPLAY WS-MSGERRO at 2118
              MOVE ZEROS TO FS-EXIT
           END-IF.

       4000-ALTERAR-FIM.
           EXIT.

       4100-REGRAVAR SECTION.
       4100.
           REWRITE FILE1-REC
                INVALID KEY
                    MOVE "ERRO AO REGRAVAR REGISTRO" TO WS-MSGERRO
                    PERFORM 9900-MOSTRA-ERRO
                       THRU 9900-MOSTRA-ERRO-FIM
                NOT INVALID KEY
                    INITIALIZE FILE1-REC
                    MOVE "VENDEDOR ALTERADO COM SUCESSO" TO WS-MSGERRO
                    PERFORM 9900-MOSTRA-ERRO
                       THRU 9900-MOSTRA-ERRO-FIM
           END-REWRITE.

       4100-REGRAVAR-FIM.
           EXIT.

      * -----------------------------------
       5000-EXCLUIR SECTION.
       5000.
           MOVE "EXCLUSÃO" TO WS-OP.
           MOVE "ESC PARA ENCERRAR" TO WS-STATUS.
           DISPLAY SS-CLS.
           INITIALIZE FILE1-REC.
           DISPLAY SS-TELA-REGISTRO.
           DISPLAY SS-CHAVE.
           DISPLAY SS-DADOS.

       5000-CODIGO.
           MOVE ZEROS TO FS-EXIT.
           PERFORM UNTIL FS-TERMINA
                   OR COB-CRT-STATUS = COB-SCR-ESC

           ACCEPT T-CODIGO
           IF FS-CODIGO EQUAL ZEROS
               MOVE "FAVOR INFORMAR CODIGO" TO WS-MSGERRO
               DISPLAY WS-MSGERRO at 2118
           ELSE
               READ FILE1 KEY IS FS-CODIGO
               IF FS-STAT = "23"
                  INITIALIZE FILE1-REC
                  MOVE "VENDEDOR NAO CADASTRADO. INFORME NOVO CODIGO"
                       TO WS-MSGERRO
                  DISPLAY WS-MSGERRO at 2118
               ELSE
                  DISPLAY WS-LIMPA at 2118
                  DISPLAY SS-DADOS
                  MOVE FS-CPF (1:3)  TO WS-CPF-MS1
                  MOVE FS-CPF (4:3)  TO WS-CPF-MS2
                  MOVE FS-CPF (7:3)  TO WS-CPF-MS3
                  MOVE FS-CPF (10:2) TO WS-CPF-MS4

                  DISPLAY WS-CPF-MS AT 0934
                  MOVE FS-CPF TO WS-CPF-ORI

                  MOVE "N" TO WS-ERRO
                  MOVE "CONFIRMA A EXCLUSÃO DO VENDEDOR (S/N)?" TO
                       WS-MSGERRO
                  ACCEPT SS-ERRO
                  IF E-SIM
                     PERFORM 5100-DELETAR
                     PERFORM 2400-LIMPA-DADOS
                     MOVE "VENDEDOR EXCLUIDO COM SUCESSO" TO WS-MSGERRO
                     DISPLAY WS-MSGERRO at 2118
                     MOVE ZEROS TO FS-EXIT
                  ELSE
                     MOVE 99 TO FS-EXIT
                  END-IF
               END-IF
           END-IF
           END-PERFORM.

       5000-EXCLUIR-FIM.
           EXIT.

       5100-DELETAR SECTION.
       5100.
           DELETE FILE1
               INVALID KEY
                   MOVE "ERRO AO EXCLUIR REGISTRO" TO WS-MSGERRO
                   PERFORM 9900-MOSTRA-ERRO
                      THRU 9900-MOSTRA-ERRO-FIM
               NOT INVALID KEY
                   INITIALIZE FILE1-REC
                   MOVE "VENDEDOR EXCLUIDO COM SUCESSO" TO WS-MSGERRO
                   PERFORM 9900-MOSTRA-ERRO
                      THRU 9900-MOSTRA-ERRO-FIM
           END-DELETE.

       5100-DELETAR-FIM.
           EXIT.

       6000-IMPORTAR SECTION.
       6000.
           MOVE "IMPORTACAO" TO WS-OP.
           MOVE "ESC PARA ENCERRAR" TO WS-STATUS.
           INITIALIZE FILE2-REC.
      *     MOVE SPACES TO SS-CHAVE SS-DADOS.
           DISPLAY SS-TELA-REGISTRO.
           DISPLAY SS-IMPORTACAO.

       5000-CODIGO.
           MOVE ZEROS TO FS-EXIT.
           PERFORM UNTIL FS-TERMINA
                   OR COB-CRT-STATUS = COB-SCR-ESC

           ACCEPT T-ARQUIVO
           IF WS-ARQIMP EQUAL SPACES
               MOVE "FAVOR INFORMAR O CAMINHO E NOME DO ARQUIVO" TO
                    WS-MSGERRO
               DISPLAY WS-MSGERRO at 2118
           ELSE
               MOVE WS-ARQIMP TO WID-ARQUIVO-IMP
               PERFORM 9050-ABRIR-ARQUIVOS
               READ FILE2
               IF NOT FS-OK
                  STRING "ERRO DE LEITURA NO ARQUIVO DE IMPORTACAO: "
                       FS-STAT INTO WS-MSGERRO
                  DISPLAY WS-MSGERRO at 2118
               ELSE
                  DISPLAY WS-LIMPA at 2118

                  MOVE "N" TO WS-ERRO
                  MOVE "CONFIRMA A IMPORTACAO DO ARQUIVO (S/N)?" TO
                       WS-MSGERRO
                  ACCEPT SS-ERRO
                  IF E-SIM
                     PERFORM 6100-IMPORTACAO
                     PERFORM 2400-LIMPA-DADOS
                     MOVE "ARQUIVO IMPORTADO COM SUCESSO" TO WS-MSGERRO
                     DISPLAY WS-MSGERRO at 2118
                     MOVE ZEROS TO FS-EXIT
                  ELSE
                     MOVE SPACES TO WS-MSGERRO
                     MOVE 99 TO FS-EXIT
                  END-IF
               END-IF
           END-IF
           END-PERFORM.

           CLOSE FILE2.

       6000-IMPORTAR-FIM.
           EXIT.

       6100-IMPORTACAO SECTION.
       6100.
           INITIALIZE FILE1-REC

              STRING "CODIGO: " FS2-CODIGO INTO WS-MSGERRO
              PERFORM 9900-MOSTRA-ERRO
                 THRU 9900-MOSTRA-ERRO-FIM
           IF FS2-CODIGO EQUAL ZEROS
              MOVE 99 TO FS-EXIT
           END-IF.

              STRING "CODIGO: " FS2-NOME INTO WS-MSGERRO
              PERFORM 9900-MOSTRA-ERRO
                 THRU 9900-MOSTRA-ERRO-FIM
           IF FS2-NOME EQUAL SPACES
              MOVE 99 TO FS-EXIT
           END-IF.

              STRING "CODIGO: " FS2-LATITUDE INTO WS-MSGERRO
              PERFORM 9900-MOSTRA-ERRO
                 THRU 9900-MOSTRA-ERRO-FIM
           IF FS2-LATITUDE EQUAL ZEROS
              MOVE 99 TO FS-EXIT
           END-IF.

              STRING "CODIGO: " FS2-LONGITUDE INTO WS-MSGERRO
              PERFORM 9900-MOSTRA-ERRO
                 THRU 9900-MOSTRA-ERRO-FIM
           IF FS2-LONGITUDE EQUAL ZEROS
              MOVE 99 TO FS-EXIT
           END-IF.

              STRING "CODIGO: " FS2-CPF INTO WS-MSGERRO
              PERFORM 9900-MOSTRA-ERRO
                 THRU 9900-MOSTRA-ERRO-FIM
           IF FS2-CPF EQUAL ZEROS OR ALL "1" OR ALL "2" OR ALL "3"
                      OR ALL "4" OR ALL "5" OR ALL "6" OR ALL "7"
                      OR ALL "8" OR ALL"9"
              MOVE 99 TO FS-EXIT
           ELSE
               DISPLAY WS-LIMPA AT 2118
               MOVE FS2-CPF TO FS-CPF
               PERFORM 2200-CALCULA-CPF
               IF FS2-CPF NOT EQUAL WS-CPF
                  MOVE 99 TO FS-EXIT
               ELSE
                  MOVE FS2-CPF TO FS-CPF
                  READ FILE1 KEY IS FS-CPF
                  IF FS-STAT = "00"
                     MOVE 99 TO FS-EXIT
                  END-IF
               END-IF
           END-IF

           IF FS-PROCESSA
              PERFORM 6200-GRAVAR
              MOVE "ARQUIVO IMPORTADO COM SUCESSO" TO WS-MSGERRO
              PERFORM 9900-MOSTRA-ERRO
                 THRU 9900-MOSTRA-ERRO-FIM
           END-IF.

       6100-IMPORTACAO-FIM.
           EXIT.

       6200-GRAVAR SECTION.
       6200.
           INITIALIZE FILE1-REC WS-MSGERRO.
           MOVE FS2-CODIGO TO FS-CODIGO
           MOVE FS2-NOME TO FS-NOME
           MOVE FS2-CPF TO FS-CPF
      *     MOVE FS2-LATITUDE TO FS-LATITUDE
      *     MOVE FS2-LONGITUDE TO FS-LONGITUDE
           move zeros to FS-LATITUDE FS-LONGITUDE

           WRITE FILE1-REC
           INVALID KEY
               STRING "ERRO IMPORTACAO REGISTRO: " FS2-CODIGO
                      INTO WS-MSGERRO
           NOT INVALID KEY
               STRING "REGISTRO IMPORTADO: " FS2-CODIGO
                      INTO WS-MSGERRO
           END-WRITE.

           PERFORM 9900-MOSTRA-ERRO
              THRU 9900-MOSTRA-ERRO-FIM.

       6200-GRAVAR-FIM.
           EXIT.

       8000-FINALIZA SECTION.
           CLOSE FILE1.

       8000-FINALIZA-FIM.
           EXIT.

      * -----------------------------------
       9000-ABRIR-ARQUIVOS.
      * -----------------------------------
      * ABRE ARQUIVOS PARA ENTRADA E SAÍDA
      * -----------------------------------
           OPEN I-O FILE1
           IF FS-NAO-EXISTE THEN
               OPEN OUTPUT FILE1
               CLOSE FILE1
               OPEN I-O FILE1
           END-IF.

       9000-ABRIR-ARQUVOS-FIM.
           EXIT.

      * -----------------------------------
       9050-ABRIR-ARQUIVOS.
      * -----------------------------------
      * ABRE ARQUIVOS PARA ENTRADA E SAÍDA
      * -----------------------------------
           OPEN INPUT FILE2
           IF FS-NAO-EXISTE THEN
              STRING "ARQUIVO PARA IMPORTACAO NÃO ENCONTRADO: "
                     WID-ARQUIVO-IMP INTO WS-MSGERRO
              PERFORM 9900-MOSTRA-ERRO
                 THRU 9900-MOSTRA-ERRO-FIM
           END-IF.

       9050-ABRIR-ARQUVOS-FIM.
           EXIT.

      * -----------------------------------
      * LE VENDEDOR PROXIMO CODIGO
       9100-LEITURA-PREV-VENDEDOR SECTION.
       9100.
           MOVE 999 TO FS-CODIGO
           START FILE1 LAST END-START.

           READ FILE1 PREVIOUS END-READ.
           IF FS-STAT NOT EQUAL "00"
              MOVE 1                       TO FS-CODIGO
           ELSE
              ADD 1                        TO FS-CODIGO
           END-IF.

       9100-EXIT.
           EXIT.

      * LE CLIENTE E MOSTRA MENSAGEM SE CHAVE NÃO EXISTE
       9200-LE-VENDEDOR SECTION.
       9200.
           ACCEPT SS-CHAVE.
           IF NOT COB-CRT-STATUS = COB-SCR-ESC
               READ FILE1
                   INVALID KEY
                       MOVE "VENDEDOR NÃO ENCONTRADO" TO
                           WS-MSGERRO
                       PERFORM  9900-MOSTRA-ERRO
                       THRU 9900-MOSTRA-ERRO-FIM

               END-READ
           ELSE
               MOVE 99 to FS-STAT
           END-IF.

       9200-LE-VENDEDOR-FIM.
           EXIT.

       9300-LE-VENDEDOR-EXISTE.
           ACCEPT SS-CHAVE.
           READ FILE1
                   IF FS-STAT = "00"
                       MOVE "VENDEDOR JA EXISTE" TO
                           WS-MSGERRO
                       PERFORM   9900-MOSTRA-ERRO
                       THRU 9900-MOSTRA-ERRO-FIM
                       MOVE 99 to FS-STAT
                  END-IF.


       9300-LE-VENDEDOR-EXISTE-FIM.
           EXIT.

      * -----------------------------------
      * MOSTRA MENSAGEM, ESPERA ENTER, ATUALIZA BARRA STATUS
       9900-MOSTRA-ERRO SECTION.
       9900.
           DISPLAY SS-ERRO
           ACCEPT SS-ERRO

           DISPLAY SS-STATUS.

       9900-MOSTRA-ERRO-FIM.
           EXIT.
