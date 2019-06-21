       IDENTIFICATION DIVISION.
       PROGRAM-ID.    DISTRIBUICAO.
      * AUTHOR.       ALBERI NUNES.
      * DATE-WRITTEN.  21/06/2019..
      * REMARKS.
      *----------------------------------------------------------------*
      * SISTEMA:      VENDAS
      * PROGRAMA:     DISTRIBUIR CLIENTE PARA VENDEDORES
      *
      * OBJETIVO:     DISTRIBUIR CLIENTE PARA VENDEDORES
      *               CONFORME A DISTANCIA
      * VERSOES:      DATA        DESCRICAO
      *               ----------  --------------------------------------
      *               21/06/2019  CARTEIRA DE CLIENTES
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FILE1 ASSIGN TO DISK "clientes.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               FILE STATUS IS FS-STAT
               RECORD KEY IS FS-KEY
               ALTERNATE RECORD KEY FS-CNPJ.
           SELECT FILE2 ASSIGN TO DISK "vendedores.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               FILE STATUS IS FS-STAT
               RECORD KEY IS FS2-KEY
               ALTERNATE RECORD KEY FS2-CPF.
           SELECT FILE3 ASSIGN TO DISK WID-ARQUIVO-GER
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-STAT.
       DATA DIVISION.
       FILE SECTION.
       COPY arqclientes.
       COPY arqvendedor.
       FD FILE3.
       01 FILE3-REC.
           03 FS3-CODCLI     PIC 9(007).
           03 FS3-PTO1       PIC X(001).
           03 FS3-RAZSOC     PIC X(040).
           03 FS3-PTO2       PIC X(001).
           03 FS3-CODVEN     PIC 9(014).
           03 FS3-PTO3       PIC X(001).
           03 FS3-NOMVEN     PIC X(040).
           03 FS3-PTO4       PIC X(001).
           03 FS3-DISTANCIA  PIC S9(003).
           03 FS3-PTO5       PIC X(001).
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
           03 FILLER          PIC X(11) VALUE "CLIENTES -".
           03 WS-OP           PIC  X(20) VALUE SPACES.
       01 WS-CNPJ-MS.
           03 WS-CNPJ-MS1 PIC X(02).
           03 FILLER      PIC X(01) VALUE ".".
           03 WS-CNPJ-MS2 PIC X(03).
           03 FILLER      PIC X(01) VALUE ".".
           03 WS-CNPJ-MS3 PIC X(03).
           03 FILLER      PIC X(01) VALUE "/".
           03 WS-CNPJ-MS4 PIC X(04).
           03 FILLER      PIC X(01) VALUE "-".
           03 WS-CNPJ-MS5 PIC X(02).
       01 WS-CNPJ-TT     PIC 9(03) VALUE ZEROS.
       01 WS-CNPJ-QC     PIC 9(03) VALUE ZEROS.
       01 WS-CNPJ-RS     PIC 9(02) VALUE ZEROS.
       01 WS-CNPJ-D1     PIC 9(01) VALUE ZEROS.
       01 WS-CNPJ-D2     PIC 9(01) VALUE ZEROS.
       01 WS-CNPJ-ORI    PIC 9(14) VALUE ZEROS.
       01 WS-CNPJ        PIC 9(14) VALUE ZEROS.
       01 FILLER REDEFINES WS-CNPJ.
           03 WS-CNPJ-P01.
                05 WS-CNPJ-01 PIC 9(01).
                05 WS-CNPJ-02 PIC 9(01).
           03 WS-CNPJ-P02.
                05 WS-CNPJ-03 PIC 9(01).
                05 WS-CNPJ-04 PIC 9(01).
                05 WS-CNPJ-05 PIC 9(01).
           03 WS-CNPJ-P03.
                05 WS-CNPJ-06 PIC 9(01).
                05 WS-CNPJ-07 PIC 9(01).
                05 WS-CNPJ-08 PIC 9(01).
           03 WS-CNPJ-P04.
                05 WS-CNPJ-09 PIC 9(01).
                05 WS-CNPJ-10 PIC 9(01).
                05 WS-CNPJ-11 PIC 9(01).
                05 WS-CNPJ-12 PIC 9(01).
           03 WS-CNPJ-P05.
                05 WS-CNPJ-13 PIC 9(01).
                05 WS-CNPJ-14 PIC 9(01).
       01  WS-ARQIMP     PIC X(60) VALUE SPACES.
       77 ST-ERRO        PIC X(02) VALUE "00".
       77 MENS1          PIC X(01).
       77 WS-OPCAO       PIC X.
           88 E-INCLUIR   VALUE IS "1".
           88 E-CONSULTAR VALUE IS "2".
           88 E-ALTERAR   VALUE IS "3".
           88 E-EXCLUIR   VALUE IS "4".
           88 E-IMPORTAR  VALUE IS "5".
           88 E-ENCERRAR  VALUE IS "X" "x".
       77 FS-STAT        PIC 9(02).
           88 FS-OK         VALUE ZEROS.
           88 FS-CANCELA    VALUE 99.
           88 FS-NAO-EXISTE VALUE 35.
       77 WS-ERRO        PIC X.
           88 E-SIM VALUES ARE "S" "s".
       77 FS-EXIT        PIC 9(02) VALUE ZEROS.
           88 FS-PROCESSA    VALUE 0.
           88 FS-TERMINA-VEN VALUE 88.
           88 FS-TERMINA     VALUE 99.
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
       01  SS-TELA-REGISTRO.
           02  BLANK SCREEN.
           02  LINE  01  COLUMN   01  VALUE "DATA:".
           02  LINE  01  COLUMN  PLUS 2 USING  WS-DIA.
           02  LINE  01  COLUMN  PLUS 1  VALUE "/".
           02  LINE  01  COLUMN  PLUS 1 USING  WS-MES.
           02  LINE  01  COLUMN  PLUS 1  VALUE "/".
           02  LINE  01  COLUMN  PLUS 1 USING  WS-ANO.
           02  LINE  01 COLUMN   29  VALUE
                "Carteira de Clientes por Vendedores HBSIS".
           01  SS-GERACAO.
               05  LINE  07 COLUMN 07  VALUE "Nome do arquivo ...:".
               05  T-ARQUIVO  LINE  07  COLUMN 28 PIC X(40)
                   USING  WS-ARQIMP    HIGHLIGHT .
               05  LINE 21 COLUMN  07  VALUE "MENSAGEM: ".
       01 SS-ERRO.
           05 FILLER FOREGROUND-COLOR 4 BACKGROUND-COLOR 1 HIGHLIGHT.
               10 LINE WS-NUML COLUMN 2 PIC X(80) FROM WS-MSGERRO BELL.
               10 COLUMN PLUS 2 TO WS-ERRO.
       PROCEDURE DIVISION.
       0000-CONTROLE SECTION.
       0000.
           PERFORM 1000-INICIO THRU 1000-INICIO-FIM.
           PERFORM 2000-PROCESSO UNTIL COB-CRT-STATUS = COB-SCR-ESC.
           PERFORM 8000-FINALIZA THRU 8000-FINALIZA-FIM.
           GOBACK.
       0000-CONTROLE-FIM.
           EXIT.
       1000-INICIO SECTION.
       1000.
           SET ENVIRONMENT 'COB_SCREEN_EXCEPTIONS' TO 'Y'.
           SET ENVIRONMENT 'COB_SCREEN_ESC' TO 'Y'.
           SET ENVIRONMENT 'ESCDELAY' TO '25'.
           ACCEPT    WS-NUML FROM LINES
           ACCEPT    WS-NUMC FROM COLUMNS
           PERFORM  9000-ABRIR-CLIENTE
             THRU   9000-ABRIR-CLIENTE-FIM.
       1000-INICIO-FIM.
           EXIT.
       2000-PROCESSO SECTION.
       2000.
           ACCEPT  WS-HORA FROM TIME
           ACCEPT  WS-DATA FROM DATE
           MOVE "GERACAO"           TO WS-OP
           MOVE "ESC PARA ENCERRAR" TO WS-STATUS
           MOVE ZEROS               TO FS-EXIT.
           DISPLAY SS-TELA-REGISTRO
           DISPLAY SS-GERACAO
           ACCEPT T-ARQUIVO
           IF WS-ARQIMP EQUAL SPACES
              MOVE "FAVOR INFORMAR O NOME DO ARQUIVO" TO WS-MSGERRO
              DISPLAY WS-MSGERRO at 2118
           ELSE
              MOVE WS-ARQIMP TO WID-ARQUIVO-GER
              PERFORM 9050-ABRIR-ARQUIVOS
              MOVE "N"       TO WS-ERRO
              MOVE "CONFIRMA A GERACAO DO ARQUIVO (S/N)?" TO
                   WS-MSGERRO
              ACCEPT SS-ERRO
              IF E-SIM THEN
                 PERFORM 9200-LE-CLIENTE
                 PERFORM 6100-GERACAO UNTIL FS-TERMINA
                 DISPLAY WS-LIMPA AT 0728
                 MOVE "ARQUIVO GERADO COM SUCESSO" TO WS-MSGERRO
                 DISPLAY WS-MSGERRO AT 2118
                 CLOSE FILE3
                 MOVE SPACES TO WS-ARQIMP
              ELSE
                 MOVE 99     to FS-EXIT
                 MOVE SPACES TO WS-MSGERRO
                 DISPLAY WS-LIMPA AT 2118
               END-IF.
       2000-PROCESSO-FIM.
           EXIT.
       6100-GERACAO SECTION.
       6100.
           PERFORM 9000-ABRIR-VENDEDOR
           PERFORM 9200-LE-VENDEDOR
           PERFORM UNTIL FS-TERMINA-VEN
               PERFORM 6200-GRAVAR
           END-PERFORM.
           CLOSE FILE2.
           PERFORM 9200-LE-CLIENTE.
       6100-GERACAO-FIM.
           EXIT.
       6200-GRAVAR SECTION.
       6200.
           INITIALIZE FILE3-REC WS-MSGERRO.
           MOVE ";"              TO FS3-PTO1 FS3-PTO2 FS3-PTO3
                                    FS3-PTO4 FS3-PTO5
           MOVE FS-CODIGO        TO FS3-CODCLI
           MOVE FS-NOME          TO FS3-RAZSOC
           MOVE FS2-CODIGO       TO FS3-CODVEN
           MOVE FS2-NOME         TO FS3-NOMVEN
           MOVE ZEROS            TO FS3-DISTANCIA
           WRITE FILE3-REC.
           PERFORM 9200-LE-VENDEDOR.
       6200-GRAVAR-FIM.
           EXIT.
       8000-FINALIZA SECTION.
           CLOSE FILE1
                 FILE3.
       8000-FINALIZA-FIM.
           EXIT.
      * -----------------------------------
       9000-ABRIR-CLIENTE.
      * -----------------------------------
      * ABRE ARQUIVOS PARA ENTRADA
      * -----------------------------------
           OPEN INPUT FILE1
           IF FS-NAO-EXISTE THEN
               STRING "ERRO ABERTURA ARQUIVO CLIENTES: " FS-STAT
                      INTO WS-MSGERRO
               PERFORM 9900-MOSTRA-ERRO
                  THRU 9900-MOSTRA-ERRO-FIM
           END-IF.
       9000-ABRIR-CLIENTE-FIM.
           EXIT.
       9000-ABRIR-VENDEDOR.
      * -----------------------------------
      * ABRE ARQUIVOS PARA ENTRADA
      * -----------------------------------
           OPEN INPUT FILE2
           IF FS-NAO-EXISTE THEN
               STRING "ERRO ABERTURA ARQUIVO VENDEDORES: " FS-STAT
                      INTO WS-MSGERRO
               PERFORM 9900-MOSTRA-ERRO
                  THRU 9900-MOSTRA-ERRO-FIM
           END-IF.
       9000-ABRIR-VENDEDOR-FIM.
           EXIT.
      * -----------------------------------
       9050-ABRIR-ARQUIVOS.
      * -----------------------------------
      * ABRE ARQUIVOS PARA SAÍDA
      * -----------------------------------
           OPEN OUTPUT FILE3
           IF FS-STAT NOT EQUAL "00"
              STRING "ERRO ABERTURA ARQUIVO DE SAIDA: "
                     WID-ARQUIVO-GER INTO WS-MSGERRO
              PERFORM 9900-MOSTRA-ERRO
                 THRU 9900-MOSTRA-ERRO-FIM
           END-IF.
       9050-ABRIR-ARQUIVOS-FIM.
           EXIT.
      * LE CLIENTE NEXT
       9200-LE-CLIENTE SECTION.
       9200.
           READ FILE1 NEXT RECORD
                   AT END
                      MOVE 99 to FS-EXIT
           END-READ.
           IF FS-STAT NOT EQUAL "00" AND "10"
              MOVE SPACES TO WS-MSGERRO
              STRING "ERRO LEITURA ARQUIVO CLIENTE - STATUS: "
                     FS-STAT INTO WS-MSGERRO
              PERFORM 9900-MOSTRA-ERRO
                 THRU 9900-MOSTRA-ERRO-FIM
           END-IF.
       9200-LE-CLIENTE-FIM.
           EXIT.
      * LE VENDEDOR NEXT
       9200-LE-VENDEDOR SECTION.
       9200.
           READ FILE2 NEXT RECORD
                   AT END
                      MOVE 88 TO FS-EXIT
           END-READ.
           IF FS-STAT NOT EQUAL "00" AND "10"
              MOVE SPACES TO WS-MSGERRO
              STRING "ERRO LEITURA ARQUIVO VENDEDOR - STATUS: "
                     FS-STAT INTO WS-MSGERRO
              PERFORM 9900-MOSTRA-ERRO
                 THRU 9900-MOSTRA-ERRO-FIM
           END-IF.
       9200-LE-VENDEDOR-FIM.
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
