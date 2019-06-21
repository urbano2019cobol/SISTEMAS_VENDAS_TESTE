       IDENTIFICATION DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID.    RELATORIO-VENDEDORES.
      * AUTHOR.        ALBERI NUNES.
      * DATE-WRITTEN.  21/06/2019..
      * REMARKS.
      *----------------------------------------------------------------*
      * SISTEMA:      VENDAS
      * PROGRAMA:     RELATORIO-VENDEDORES
      *
      * OBJETIVO:     LISTAR VENDEDORES
      *
      * VERSOES:      DATA        DESCRICAO
      *               ----------  --------------------------------------
      *               21/06/2019  LISTAR VENDEDORES
      *



      *===============================================================*
       ENVIRONMENT DIVISION.
      *----------------------------------------------------------------*
       CONFIGURATION SECTION.
        SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SORT-FILE ASSIGN TO DISK "VENDEDORES".
           SELECT VENDEDORES ASSIGN TO DISK "vendedores.dat"
                  ORGANIZATION IS INDEXED
                  RECORD KEY FS-KEY
                  ACCESS MODE IS SEQUENTIAL.
           SELECT VENDEDORES-REL   ASSIGN TO DISK "vendedores01.rel"
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS  IS WT-ST-VENDEDORES.

           SELECT VENDEDORES-GER   ASSIGN TO DISK "vendedores02.rel"
                  ORGANIZATION IS LINE SEQUENTIAL.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  VENDEDORES.
       01  FD-RECORD.
           05 FS-KEY.
               10 FS-CODIGO PIC 9(003).
           05 FS-NOME       PIC X(040).
           05 FS-CPF        PIC 9(011).
           05 FS-LATITUDE   PIC s9(003)v9(008).
           05 FS-LONGITUDE  PIC s9(003)v9(008).
           05 FILLER        PIC X(20).

       SD  SORT-FILE.
       01  SORT-RECORD.
           05 SD-KEY.
               10 SD-CODIGO PIC 9(003).
           05 SD-NOME       PIC X(040).
           05 SD-CPF        PIC 9(011).
           05 SD-LATITUDE   PIC s9(003)v9(008).
           05 SD-LONGITUDE  PIC s9(003)v9(008).
           05 FILLER        PIC X(20).

       FD  VENDEDORES-REL.
       01  REL-REGISTRO.
           05 R1-KEY.
               10 R1-CODIGO PIC 9(003).
           05 R1-NOME       PIC X(040).
           05 R1-CPF        PIC 9(011).
           05 R1-LATITUDE   PIC s9(003)v9(008).
           05 R1-LONGITUDE  PIC s9(003)v9(008).
           05 FILLER        PIC X(20).

       FD  VENDEDORES-GER.
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
           03 WC-LINHAS-POR-PAGINA       PIC  9(002) VALUE 60.

       01 WT-FILE-STATUS.
           03 WT-ST-VENDEDORES           PIC  X(002) VALUE SPACES.

       01 WT-CONTADORES.
           03 WT-CT-PAGINA             PIC  9(003) VALUE ZEROS.
           03 WT-CT-LINHAS             PIC  9(002) VALUE 99.
           03 WT-CT-LIDOS              PIC  9(006) VALUE ZEROS.


       01 WT-AUXILIARES.
           03 WT-NM-FDAT               PIC X(010) VALUE "VENDEDORES".
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
           03 WA-CD-VENDEDORES         PIC X(007) VALUE SPACES.

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
           "VENDEDORES ".
           03 FILLER                   PIC X(035) VALUE
              "VENDEDORES CADASTRADOS".
           03 FILLER                   PIC X(011) VALUE
              "PAGINA: ".
           03 WR-CAB-PAGINA            PIC ZZ9 VALUE ZEROS.

       01 WR-CAB4.
           03  FILLER                  PIC X(011) VALUE
               "VENDEDORES ".
           03  FILLER                  PIC X(040)
               VALUE " NOME".
           03  FILLER                  PIC X(017) VALUE "CPF ".
           03  FILLER                  PIC X(014) VALUE "LATITUDE".
           03  FILLER                  PIC X(010) VALUE "LONGITUDE".


       01 WR-DET1.
           03  WR-DET-VENDEDOR         PIC X(003) VALUE SPACES.
           03  FILLER                  PIC X(008) VALUE SPACES.
           03  WR-DET-NOME             PIC X(038) VALUE SPACES.
           03  FILLER                  PIC X(002) VALUE SPACES.
           03  WR-DET-CPF              PIC ZZZ.ZZZ.ZZZ/ZZ
                VALUE SPACES.
           03  FILLER                  PIC X(002) VALUE SPACES.
           03  WR-DET-LATITUDE         PIC ZZZ,ZZZZZZZZ
               VALUE SPACES.
           03  FILLER                  PIC X(002) VALUE SPACES.
           03  WR-DET-LONGITUDE        PIC ZZZ,ZZZZZZZZ VALUE SPACES.


       SCREEN SECTION.
       01 SS-MENU FOREGROUND-COLOR 6.
               10 BLANK SCREEN.
               10 LINE 01 COLUMN 18 VALUE
                   "RELATORIO DE VENDEDORES".
               10 LINE 08 COLUMN 15 VALUE
                      "ORDEM  1-ASCE / 2-DESC ......... --> ".
               10 LINE 08 COL PLUS 1 USING WS-OPCAO-ORDEM AUTO.
               10 LINE 09 COLUMN 15 VALUE
                      "CLASS. 1-CODIGO / 2-NOME ....... --> ".
               10 LINE 09 COL PLUS 1 USING WS-OPCAO-CLASS AUTO.
               10 LINE 10 COLUMN 15 VALUE
                      "FILTRO CODIGO .................. --> ".
               10 LINE 10 COL PLUS 1 USING
                        WS-OPCAO-FILTRO-COD AUTO.
               10 LINE 11 COLUMN 15 VALUE
                      "FILTRO NOME .................... --> ".
               10 LINE 11 COL PLUS 1 USING
                        WS-OPCAO-FILTRO-RZ AUTO.

      *================================================================*
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
       0-PRINCIPAL.

           PERFORM 0000-INICIA
           PERFORM 0002-PROCESSA   UNTIL WT-ST-VENDEDORES NOT = "00"
           PERFORM 0003-TERMINA
           STOP RUN.
      *----------------------------------------------------------------*


       0000-INICIA.
           DISPLAY SS-MENU
           ACCEPT  SS-MENU
           IF WS-OPCAO-ORDEM = 1 AND WS-OPCAO-CLASS = 1
               SORT SORT-FILE
                   ON ASCENDING KEY FS-CODIGO
                   USING  VENDEDORES
                   GIVING VENDEDORES-REL
           END-IF.
           IF WS-OPCAO-ORDEM = 2 AND WS-OPCAO-CLASS = 1
               SORT SORT-FILE
                   ON DESCENDING KEY FS-CODIGO
                   USING VENDEDORES
                   GIVING VENDEDORES-REL
           END-IF.

           IF WS-OPCAO-ORDEM = 1 AND WS-OPCAO-CLASS = 2
               SORT SORT-FILE
                   ON ASCENDING KEY FS-NOME
                   USING VENDEDORES
                   GIVING VENDEDORES-REL
           END-IF.
           IF WS-OPCAO-ORDEM = 2 AND WS-OPCAO-CLASS = 2
               SORT SORT-FILE
                   ON DESCENDING KEY FS-NOME
                   USING VENDEDORES
                   GIVING VENDEDORES-REL
           END-IF


           OPEN INPUT  VENDEDORES-REL.
           OPEN OUTPUT VENDEDORES-GER.
           READ VENDEDORES-REL NEXT.


       0002-PROCESSA.


           IF WT-CT-LINHAS > WC-LINHAS-POR-PAGINA
                 PERFORM 0004-IMPRIME-CABECALHO
           END-IF.

           MOVE R1-CODIGO    TO WR-DET-VENDEDOR.
           MOVE R1-NOME      TO WR-DET-NOME.
           MOVE R1-CPF       TO WR-DET-CPF.
           MOVE R1-LATITUDE  TO WR-DET-LATITUDE.
           MOVE R1-LONGITUDE TO WR-DET-LONGITUDE.

           DISPLAY " TESTE - > " WR-DET1.

           WRITE REL-REGISTRO-GER FROM WR-DET1.
           ADD 1 TO WT-CT-LINHAS.
           ADD 1 TO WT-CT-LIDOS.

           READ VENDEDORES-REL NEXT.

       0003-TERMINA.

           CLOSE VENDEDORES-REL VENDEDORES-GER.
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
