       IDENTIFICATION DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID.     MENU.
      * AUTHOR.        ALBERI NUNES.
      * DATE-WRITTEN.  21/06/2019.
      * REMARKS.
      *----------------------------------------------------------------*
      * SISTEMA:      VENDAS
      * PROGRAMA:     MANUTENÇÃO DE MENU
      *
      * OBJETIVO:     MANUTENÇÃO DE MENU
      *
      * VERSOES:      DATA        DESCRICAO
      *               ----------  --------------------------------------
      *               21/06/2019  MANTER MENU
      *



      *===============================================================*
       ENVIRONMENT DIVISION.
      *----------------------------------------------------------------*
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.


       DATA DIVISION.
       FILE SECTION.


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
           03 FILLER          PIC X(07) VALUE "MENU - ".
           03 WS-OP           PIC  X(20) VALUE SPACES.

       01 WS-PROGRAMA         PIC X(30) VALUE SPACES.

       77 ST-ERRO             PIC X(02) VALUE "00".
       77 MENS1          PIC X(01).
       77 WS-OPCAO       PIC X(04).
           88 E-CLIENTE   VALUE IS "0101".
           88 E-VENDEDOR  VALUE IS "0102".
           88 E-RELCLI    VALUE IS "0201".
           88 E-RELVEN    VALUE IS "0202".
           88 E-DISTRIBUI VALUE IS "0301".
           88 E-ENCERRAR  VALUE IS "X" "x".

       77 FS-STAT        PIC 9(02).
           88 FS-OK         VALUE ZEROS.
           88 FS-CANCELA    VALUE 99.
           88 FS-NAO-EXISTE VALUE 35.
       77 WS-ERRO        PIC X.
           88 E-SIM VALUES ARE "S" "s".

       77 FS-EXIT        PIC 9(02) VALUE ZEROS.
           88 FS-PROCESSA   VALUE 0.
           88 FS-TERMINA    VALUE 99.

       77 WS-NUML        PIC 999.
       77 WS-NUMC        PIC 999.
       77 COR-FUNDO      PIC 9 VALUE 1.
       77 COR-FRENTE     PIC 9 VALUE 6.

       77 WS-STATUS      PIC X(30).
       77 WS-MSGERRO     PIC X(100).

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
                "MENU HBSIS"
                HIGHLIGHT .
           05 LINE 05 COLUMN 10 VALUE
                "========== "
                HIGHLIGHT .
           05 LINE 07 COLUMN 15 VALUE "01.00 - Cadastros".
           05 LINE 08 COLUMN 23 VALUE "01.01 - Cadastro de Cliente".
           05 LINE 09 COLUMN 23 VALUE "01.02 - Cadastro de Vendedor".
           05 LINE 10 COLUMN 15 VALUE "02.00 - Relatorios".
           05 LINE 11 COLUMN 23 VALUE "02.01 - Relatorio de Cliente".
           05 LINE 12 COLUMN 23 VALUE "02.02 - Relatorio de Vendedores".
           05 LINE 13 COLUMN 15 VALUE "03.00 - Executar".
           05 LINE 14 COLUMN 23 VALUE
              "03.01 - Executar Distribuicao de Clientes".
           05 LINE 16 COLUMN 15 VALUE "X - ENCERRAR".
           05 LINE 18 COLUMN 15 VALUE "OPCAO: ".
           05 LINE 18 COL PLUS 1 USING WS-OPCAO AUTO.

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
           STOP RUN.

       0000-EXIT.
           EXIT.

       1000-INICIO SECTION.
       1000.
           SET ENVIRONMENT 'COB_SCREEN_EXCEPTIONS' TO 'Y'.
           SET ENVIRONMENT 'COB_SCREEN_ESC' TO 'Y'.
           SET ENVIRONMENT 'ESCDELAY' TO '25'.
           ACCEPT    WS-NUML FROM LINES
           ACCEPT    WS-NUMC FROM COLUMNS.

       1000-EXIT.
           EXIT.

       2000-PROCESSO SECTION.
       2000.
           MOVE "MENU" TO WS-OP
           MOVE "ESCOLHA A OPCAO" TO WS-STATUS
           MOVE SPACES TO WS-OPCAO
           DISPLAY SS-CLS
           ACCEPT SS-MENU
           ACCEPT WS-HORA FROM TIME
           ACCEPT WS-DATA FROM DATE
           MOVE "MENU" TO WS-OP
           MOVE "ESC PARA ENCERRAR" TO WS-STATUS
           DISPLAY SS-CLS
           MOVE ZEROS TO FS-EXIT.
           DISPLAY SS-MENU
           EVALUATE TRUE
                WHEN E-CLIENTE
                     CALL "CLIENTES-CADASTRO.exe"

                WHEN E-VENDEDOR
                     CALL "VENDEDORES-CADASTRO.exe"

                WHEN E-RELCLI
                      CALL "RELATO01-CLIENTES.exe"

                WHEN E-RELVEN
                      CALL "RELATO01-VENDEDORES.exe"

                WHEN E-DISTRIBUI
                      CALL "DISTRIBUICAO_CSV.exe"

           END-EVALUATE.



       2000-PROCESSO-FIM.
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


       3000-CODIGO.
           MOVE ZEROS TO FS-EXIT.

       3000-CONSULTA-FIM.
           EXIT.






       8000-FINALIZA SECTION.
           MOVE ZEROS TO FS-EXIT.

       8000-FINALIZA-FIM.
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
