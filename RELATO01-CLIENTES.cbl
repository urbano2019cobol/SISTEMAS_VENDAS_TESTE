       IDENTIFICATION DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID.    RELATORIO-CLIENTES.
      * AUTHOR.        ALBERI NUNES.
      * DATE-WRITTEN.  21/06/2019.
      * REMARKS.
      *----------------------------------------------------------------*
      * SISTEMA:      VENDAS
      * PROGRAMA:     RELATORIO-CLIENTES
      *
      * OBJETIVO:     LISTAR CLIENTES
      *
      * VERSOES:      DATA        DESCRICAO
      *               ----------  --------------------------------------
      *               21/06/2019  LISTAR CLIENTES
      *



      *===============================================================*
       ENVIRONMENT DIVISION.
      *----------------------------------------------------------------*
       CONFIGURATION SECTION.
        SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SORT-FILE ASSIGN TO DISK "CLIENTES".
           SELECT CLIENTES ASSIGN TO DISK "clientes.dat"
                  ORGANIZATION IS INDEXED
                  RECORD KEY FS-KEY
                  ACCESS MODE IS SEQUENTIAL.
           SELECT CLIENTES-REL   ASSIGN TO DISK "cliente01.rel"
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS WT-ST-CLIENTES.

           SELECT CLIENTES-GER   ASSIGN TO DISK "cliente02.rel"
                  ORGANIZATION IS LINE SEQUENTIAL.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  CLIENTES.
       01  FD-RECORD.
           05 FS-KEY.
               10 FS-CODIGO PIC 9(007).
           05 FS-NOME       PIC X(040).
           05 FS-CNPJ       PIC 9(014).
           05 FS-LATITUDE   PIC S9(013).
           05 FS-LONGITUDE  PIC S9(013).

       SD  SORT-FILE.
       01  SORT-RECORD.
           05 SD-KEY.
               10 SD-CODIGO PIC 9(007).
           05 SD-NOME       PIC X(040).
           05 SD-CNPJ       PIC 9(014).
           05 SD-LATITUDE   PIC S9(013).
           05 SD-LONGITUDE  PIC S9(013).

       FD  CLIENTES-REL.
       01  REL-REGISTRO.
           05 R1-KEY.
               10 R1-CODIGO PIC 9(007).
           05 R1-NOME       PIC X(040).
           05 R1-CNPJ       PIC 9(014).
           05 R1-LATITUDE   PIC S9(009).
           05 R1-LONGITUDE  PIC S9(009).

       FD  CLIENTES-GER.
       01  REL-REGISTRO-GER                         PIC X(140).

      *================================================================*
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
       77  WS-OPCAO-ORDEM                    PIC X VALUE SPACES.
       77  WS-OPCAO-CLASS                    PIC X VALUE SPACES.
       77  WS-OPCAO-FILTRO-COD               PIC 999999999
                   VALUE ZEROS.
       77  WS-OPCAO-FILTRO-RZ                PIC X(40)
                   VALUE SPACES.


       01  WC-CONSTANTES.
           03 WC-LINHAS-POR-PAGINA     PIC  9(002) VALUE 60.

       01 WT-FILE-STATUS.
           03 WT-ST-CLIENTES           PIC  X(002) VALUE SPACES.

       01 WT-CONTADORES.
           03 WT-CT-PAGINA             PIC  9(003) VALUE ZEROS.
           03 WT-CT-LINHAS             PIC  9(002) VALUE 99.
           03 WT-CT-LIDOS              PIC  9(006) VALUE ZEROS.


       01 WT-AUXILIARES.
           03 WT-NM-FDAT               PIC X(008) VALUE "CLIENTES".
           03 WT-RC-FDAT               PIC 9(002) VALUE ZEROS.
           03 WT-DT-SISTEMA.
              05 ANO                   PIC 9(002) VALUE ZEROS.
              05 MES                   PIC 9(002) VALUE ZEROS.
              05 DIA                   PIC 9(002) VALUE ZEROS.
           03 WT-HR-SISTEMA.
              05 HORA                  PIC 9(002) VALUE ZEROS.
              05 MINUTO                PIC 9(002) VALUE ZEROS.
              05 SEGUNDO               PIC 9(002) VALUE ZEROS.

       01 WA-ARGUMENTOS.
           03 WA-CD-CLIENTES           PIC X(007) VALUE SPACES.

       01 WR-CAB1.
           03 FILLER                   PIC X(050) VALUE
              "EMPRESA HBSIS-SUPERO".
           03 FILLER                   PIC X(006) VALUE
              "DATA: ".
           03 WR-CAB-DATA.
              05 DIA                   PIC 9(002) VALUE ZEROS.
              05 FILLER                PIC X(001) VALUE "/".
              05 MES                   PIC 9(002) VALUE ZEROS.
              05 FILLER                PIC X(001) VALUE "/".
              05 ANO                   PIC 9(002) VALUE ZEROS.
           03 FILLER                   PIC X(007) VALUE
              " HORA: ".
           03 WR-CAB-HORA.
              05 HOR                   PIC 9(002) VALUE ZEROS.
              05 FILLER                PIC X(001) VALUE ":".
              05 MINUTO                PIC 9(002) VALUE ZEROS.
              05 FILLER                PIC X(001) VALUE ":".
              05 SEGUNDO               PIC 9(002) VALUE ZEROS.


       01 WR-SEP1.
           03  FILLER                  PIC X(095) VALUE ALL "-".



       01 WR-CAB3.
           03 FILLER                   PIC X(031) VALUE
           "CLIENTES".
           03 FILLER                   PIC X(035) VALUE
              "CLIENTES CADASTRADOS".
           03 FILLER                   PIC X(011) VALUE
              "PAGINA: ".
           03 WR-CAB-PAGINA            PIC ZZ9 VALUE ZEROS.

       01 WR-CAB4.
           03  FILLER                  PIC X(008) VALUE "CLIENTE ".
           03  FILLER                  PIC X(043)
               VALUE "RAZAO SOCIAL".
           03  FILLER                  PIC X(021) VALUE "CNPJ ".
           03  FILLER                  PIC X(013) VALUE "LATITUDE".
           03  FILLER                  PIC X(013) VALUE "LONGITUDE".


       01 WR-DET1.
           03  WR-DET-CLIENTE          PIC X(007) VALUE SPACES.
           03  FILLER                  PIC X(001) VALUE SPACES.
           03  WR-DET-RAZAO-SOCIAL     PIC X(040) VALUE SPACES.
           03  FILLER                  PIC X(002) VALUE SPACES.
           03  WR-DET-CNPJ             PIC ZZ.ZZZ.ZZ9/9999B99
                VALUE SPACES.
           03  FILLER                  PIC X(002) VALUE SPACES.
           03  WR-DET-LATITUDE         PIC X(013) VALUE SPACES.
           03  FILLER                  PIC X(003) VALUE SPACES.
           03  WR-DET-LONGITUDE        PIC X(013) VALUE SPACES.


       SCREEN SECTION.
       01 SS-MENU FOREGROUND-COLOR 6.
               10 BLANK SCREEN.
               10 LINE 01 COLUMN 18 VALUE
                   "RELATORIO DE CLIENTES".
               10 LINE 08 COLUMN 15 VALUE
                      "ORDEM  1-ASCE / 2-DESC ......... --> ".
               10 LINE 08 COL PLUS 1 USING WS-OPCAO-ORDEM AUTO.
               10 LINE 09 COLUMN 15 VALUE
                      "CLASS. 1-CODIGO / 2-RAZAO SOCIAL --> ".
               10 LINE 09 COL PLUS 1 USING WS-OPCAO-CLASS AUTO.
               10 LINE 10 COLUMN 15 VALUE
                      "FILTRO CODIGO ................. --> ".
               10 LINE 10 COL PLUS 1 USING
                        WS-OPCAO-FILTRO-COD AUTO.
               10 LINE 11 COLUMN 15 VALUE
                      "FILTRO RAZÃO SOCIAL............ --> ".
               10 LINE 11 COL PLUS 1 USING
                        WS-OPCAO-FILTRO-RZ AUTO.

      *================================================================*
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
       0-PRINCIPAL.

           PERFORM 0000-INICIA
           PERFORM 0002-PROCESSA   UNTIL WT-ST-CLIENTES NOT = "00"
           PERFORM 0003-TERMINA
           STOP RUN.
      *----------------------------------------------------------------*


       0000-INICIA.
           DISPLAY SS-MENU
           ACCEPT  SS-MENU
           IF WS-OPCAO-ORDEM = 1 AND WS-OPCAO-CLASS = 1
               SORT SORT-FILE
                   ON ASCENDING KEY FS-CODIGO
                   USING CLIENTES
                   GIVING CLIENTES-REL
           END-IF.
           IF WS-OPCAO-ORDEM = 2 AND WS-OPCAO-CLASS = 1
               SORT SORT-FILE
                   ON DESCENDING KEY FS-CODIGO
                   USING CLIENTES
                   GIVING CLIENTES-REL
           END-IF.

           IF WS-OPCAO-ORDEM = 1 AND WS-OPCAO-CLASS = 2
               SORT SORT-FILE
                   ON ASCENDING KEY FS-NOME
                   USING CLIENTES
                   GIVING CLIENTES-REL
           END-IF.
           IF WS-OPCAO-ORDEM = 2 AND WS-OPCAO-CLASS = 2
               SORT SORT-FILE
                   ON DESCENDING
                   KEY FS-NOME
                   USING CLIENTES
                   GIVING CLIENTES-REL
           END-IF


           OPEN INPUT  CLIENTES-REL.
           OPEN OUTPUT CLIENTES-GER.
           READ CLIENTES-REL NEXT.


       0002-PROCESSA.


           IF WT-CT-LINHAS > WC-LINHAS-POR-PAGINA
                 PERFORM 0004-IMPRIME-CABECALHO
           END-IF.

           MOVE R1-CODIGO    TO WR-DET-CLIENTE.
           MOVE R1-NOME      TO WR-DET-RAZAO-SOCIAL.
           MOVE R1-CNPJ      TO WR-DET-CNPJ.
           MOVE R1-LATITUDE  TO WR-DET-LATITUDE.
           MOVE R1-LONGITUDE TO WR-DET-LONGITUDE.

           DISPLAY " TESTE - > " WR-DET1.

           WRITE REL-REGISTRO-GER FROM WR-DET1.
           ADD 1 TO WT-CT-LINHAS.
           ADD 1 TO WT-CT-LIDOS.

           READ CLIENTES-REL NEXT.

       0003-TERMINA.

           CLOSE CLIENTES-REL CLIENTES-GER.
      *----------------------------------------------------------------*
      * IMPRIME CABECALHO
      *----------------------------------------------------------------*
       0004-IMPRIME-CABECALHO.

           ACCEPT WT-DT-SISTEMA FROM DATE
           ACCEPT WT-HR-SISTEMA FROM TIME
           ADD 1 TO WT-CT-PAGINA

           MOVE CORR WT-DT-SISTEMA TO WR-CAB-DATA
           MOVE CORR WT-HR-SISTEMA TO WR-CAB-HORA
           MOVE WT-CT-PAGINA TO WR-CAB-PAGINA


           WRITE REL-REGISTRO-GER FROM WR-CAB1
           WRITE REL-REGISTRO-GER FROM WR-CAB3
           WRITE REL-REGISTRO-GER FROM WR-SEP1
           WRITE REL-REGISTRO-GER FROM WR-CAB4
           WRITE REL-REGISTRO-GER FROM WR-SEP1


           MOVE 8 TO WT-CT-LINHAS.
