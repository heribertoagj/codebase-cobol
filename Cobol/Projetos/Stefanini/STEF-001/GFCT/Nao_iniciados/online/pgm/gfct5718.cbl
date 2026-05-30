----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
***************************** Top of Data ******************************
      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT5718.
       AUTHOR.     HOMI.
      *================================================================*
      *      S O N D A  I T                                            *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT5718                                    *
      *    PROGRAMADOR.:   HOMI                                        *
      *    ANALISTA....:   PAGNOCCA - SONDA IT - GR. 50                *
      *    DATA........:   OUT/2015                                    *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   INVESTIMENTOS ULTIMOS 03 MESES              *
      *                    CONSISTENCIA E GERACAO DE IMAGEM DA TABELA  *
      *                    GFCTB0P0 - TPOSIC_SDO_APLIC + CNPJ/CPF      *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                          INCLUDE/BOOK    *
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
      *                     ENTRADA                      GFCTWF17      *
VI0516*                     DEPARCTA                     I#CLIBGB      *
      *                     REJEITAD                     GFCTWF18      *
      *                     GFCTB0P0                     GFCTWF45      *
      *----------------------------------------------------------------*
      *    BOOKS.......:                                               *
      *    GFCTWF03 - AREA DE COMUNICACAO DO DE/PARA DE CESTAS         *
      *    GFCTWF04 - AREA DE COMUNICACAO DE/PARA DE CONTAS HSBC       *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    GFCT5800 - EFETUAR DE/PARA DE CESTAS HSBC PARA BRADESCO     *
VI0516**** GFCT5801 - EFETUAR DE/PARA DE CONTAS HSBC/BRADESCO          *
VI0516**** RETIRADO MODULO E FEITO BALANCE LINE COM ARQUIVO DEPARA.    *
      *    POOL1470 - VALIDAR DATA                                     *
      *    POOL7100 - TRATAMENTO DE ERROS.                             *
      *                                                                *
      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*

       FILE-CONTROL.

           SELECT ENTRADA   ASSIGN     TO UT-S-ENTRADA
           FILE STATUS                 IS WRK-FS-ENTRADA.

VI0516     SELECT DEPARCTA ASSIGN      TO UT-S-DEPARCTA
VI0516     FILE STATUS                 IS WRK-FS-DEPARCTA.

           SELECT REJEITAD ASSIGN      TO UT-S-REJEITAD
           FILE STATUS                 IS WRK-FS-REJEITAD.

----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
           SELECT GFCTB0P0 ASSIGN      TO UT-S-GFCTB0P0
           FILE STATUS                 IS WRK-FS-GFCTB0P0.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:  ARQUIVO ENTRADA                                     *
      *            ORG. SEQUENCIAL     - LRECL = 200                   *
      *----------------------------------------------------------------*

       FD  ENTRADA
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
       01  FD-ENTRADA                  PIC X(200).

VI0516*----------------------------------------------------------------*
VI0516*    INPUT:  ARQUIVO DEPARCTA                                    *
VI0516*            ORG. SEQUENCIAL     - LRECL = 104                   *
VI0516*----------------------------------------------------------------*
VI0516
VI0516 FD  DEPARCTA
VI0516     RECORDING MODE IS F
VI0516     LABEL RECORD IS STANDARD
VI0516     BLOCK CONTAINS 0 RECORDS.
VI0516
VI0516 01  FD-DEPARCTA                 PIC X(104).

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO REGISTROS REJEITADOS.                       *
      *            ORG. SEQUENCIAL     - LRECL = 200                   *
      *----------------------------------------------------------------*

       FD  REJEITAD
           RECORDING MODE IS F
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REJEITAD                 PIC X(200).

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO IMAGEM DA TABELA GFCTB0P0 + CNPJ/CPF        *
      *                                   + AGENCIA/CONTA/DE           *
      *            ORG. SEQUENCIAL     - LRECL = 116                   *
      *----------------------------------------------------------------*

       FD  GFCTB0P0
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-GFCTB0P0                 PIC X(116).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(033)         VALUE
           'INICIO DA WORKING STORAGE SECTION'.
      *----------------------------------------------------------------*

       01  WRK-CAMPO-TIRA-SINAL        PIC +9(017)         VALUE ZEROS.
       01  FILLER  REDEFINES           WRK-CAMPO-TIRA-SINAL.
           05  WRK-CAMPO-TIRA-SINAL-S  PIC X.
           05  WRK-CAMPO-TIRA-SINAL-17 PIC 9(17).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(01).
               10 WRK-CAMPO-TIRA-SINAL-16 PIC 9(16).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(02).
               10 WRK-CAMPO-TIRA-SINAL-15 PIC 9(15).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(03).
               10 WRK-CAMPO-TIRA-SINAL-14 PIC 9(14).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(04).
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
               10 WRK-CAMPO-TIRA-SINAL-13 PIC 9(13).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(05).
               10 WRK-CAMPO-TIRA-SINAL-12 PIC 9(12).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(06).
               10 WRK-CAMPO-TIRA-SINAL-11 PIC 9(11).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(07).
               10 WRK-CAMPO-TIRA-SINAL-10 PIC 9(10).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(08).
               10 WRK-CAMPO-TIRA-SINAL-09 PIC 9(09).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(09).
               10 WRK-CAMPO-TIRA-SINAL-08 PIC 9(08).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(10).
               10 WRK-CAMPO-TIRA-SINAL-07 PIC 9(07).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(11).
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
               10 WRK-CAMPO-TIRA-SINAL-06 PIC 9(06).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(12).
               10 WRK-CAMPO-TIRA-SINAL-05 PIC 9(05).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(13).
               10 WRK-CAMPO-TIRA-SINAL-04 PIC 9(04).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(14).
               10 WRK-CAMPO-TIRA-SINAL-03 PIC 9(03).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(15).
               10 WRK-CAMPO-TIRA-SINAL-02 PIC 9(02).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(16).
               10 WRK-CAMPO-TIRA-SINAL-01 PIC 9(01).

       01  WRK-CAMPO-POE-SINAL-01        PIC +9(01)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-01.
           05  WRK-CAMPO-POE-SINAL-01-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-01-C  PIC 9(01).
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--

       01  WRK-CAMPO-POE-SINAL-02        PIC +9(02)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-02.
           05  WRK-CAMPO-POE-SINAL-02-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-02-C  PIC 9(02).

       01  WRK-CAMPO-POE-SINAL-03        PIC +9(03)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-03.
           05  WRK-CAMPO-POE-SINAL-03-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-03-C  PIC 9(03).

       01  WRK-CAMPO-POE-SINAL-04        PIC +9(04)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-04.
           05  WRK-CAMPO-POE-SINAL-04-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-04-C  PIC 9(04).

       01  WRK-CAMPO-POE-SINAL-05        PIC +9(05)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-05.
           05  WRK-CAMPO-POE-SINAL-05-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-05-C  PIC 9(05).

----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
       01  WRK-CAMPO-POE-SINAL-06        PIC +9(06)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-06.
           05  WRK-CAMPO-POE-SINAL-06-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-06-C  PIC 9(06).

       01  WRK-CAMPO-POE-SINAL-07        PIC +9(07)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-07.
           05  WRK-CAMPO-POE-SINAL-07-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-07-C  PIC 9(07).

       01  WRK-CAMPO-POE-SINAL-08        PIC +9(08)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-08.
           05  WRK-CAMPO-POE-SINAL-08-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-08-C  PIC 9(08).

       01  WRK-CAMPO-POE-SINAL-09        PIC +9(09)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-09.
           05  WRK-CAMPO-POE-SINAL-09-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-09-C  PIC 9(09).

       01  WRK-CAMPO-POE-SINAL-10        PIC +9(10)      VALUE ZEROS.
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-10.
           05  WRK-CAMPO-POE-SINAL-10-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-10-C  PIC 9(10).

       01  WRK-CAMPO-POE-SINAL-11        PIC +9(11)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-11.
           05  WRK-CAMPO-POE-SINAL-11-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-11-C  PIC 9(11).

       01  WRK-CAMPO-POE-SINAL-12        PIC +9(12)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-12.
           05  WRK-CAMPO-POE-SINAL-12-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-12-C  PIC 9(12).

       01  WRK-CAMPO-POE-SINAL-13        PIC +9(13)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-13.
           05  WRK-CAMPO-POE-SINAL-13-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-13-C  PIC 9(13).

       01  WRK-CAMPO-POE-SINAL-14        PIC +9(14)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-14.
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
           05  WRK-CAMPO-POE-SINAL-14-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-14-C  PIC 9(14).

       01  WRK-CAMPO-POE-SINAL-15        PIC +9(15)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-15.
           05  WRK-CAMPO-POE-SINAL-15-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-15-C  PIC 9(15).

       01  WRK-CAMPO-POE-SINAL-16        PIC +9(16)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-16.
           05  WRK-CAMPO-POE-SINAL-16-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-16-C  PIC 9(16).

       01  WRK-CAMPO-POE-SINAL-17        PIC +9(17)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-17.
           05  WRK-CAMPO-POE-SINAL-17-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-17-C  PIC 9(17).

           COPY 'GFCTWFW5'.

      *----------------------------------------------------------------*
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
       77  FILLER                      PIC  X(022)         VALUE
           'AREA PARA ACUMULADORES'.
      *----------------------------------------------------------------*

       77  ACU-LIDOS-ENTRADA           PIC  9(015) COMP-3  VALUE ZEROS.
VI0516 77  ACU-LIDOS-DEPARCTA          PIC  9(015) COMP-3  VALUE ZEROS.
       77  ACU-ACESSOS-5800            PIC  9(015) COMP-3  VALUE ZEROS.
       77  ACU-ACESSOS-5801            PIC  9(015) COMP-3  VALUE ZEROS.
       77  ACU-GRAVADOS-REJEITAD       PIC  9(015) COMP-3  VALUE ZEROS.
       77  WRK-CTM                     PIC  9(015) COMP-3  VALUE ZEROS.
       77  WRK-COUNT                   PIC  9(015) COMP-3  VALUE ZEROS.
       77  ACU-GRAVADOS-GFCTB0P0       PIC  9(015) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(030)         VALUE
           'AREA PARA TESTE DE FILE STATUS'.
      *----------------------------------------------------------------*

       77  WRK-FS-ENTRADA              PIC  X(002)         VALUE SPACES.
VI0516 77  WRK-FS-DEPARCTA             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-REJEITAD             PIC  X(002)         VALUE SPACES.
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
       77  WRK-FS-GFCTB0P0             PIC  X(002)         VALUE SPACES.

       77  WRK-ABERTURA                PIC  X(013)         VALUE
           ' NA ABERTURA '.
       77  WRK-LEITURA                 PIC  X(013)         VALUE
           ' NA  LEITURA '.
       77  WRK-GRAVACAO                PIC  X(013)         VALUE
           ' NA GRAVACAO '.
       77  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(030)         VALUE
           'AREA PARA VARIAVEIS AUXILIARES'.
      *----------------------------------------------------------------*
       77  WRK-BATCH                   PIC  X(008)        VALUE 'BATCH'.
       77  WRK-MODULO                  PIC  X(008)        VALUE SPACES.
       77  WRK-MASCARA             PIC  ZZZZZZZZZZ.ZZZ.999 VALUE ZEROS.
       77  WRK-ERRO                    PIC  X(001)        VALUE SPACES.
       77  WRK-ACHOU                   PIC  X(001)        VALUE SPACES.
       77  WRK-MSG                     PIC  X(030)        VALUE SPACES.
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
       77  WRK-NOME-ARQUIVO            PIC  X(030)        VALUE SPACES.
       77  WRK-TIMESTAMP-INI           PIC  X(026)        VALUE SPACES.
       77  WRK-TIMESTAMP-FIM           PIC  X(026)        VALUE SPACES.
       77  WRK-DATA-VALIDA             PIC  X(001)        VALUE 'N'.
VI0516 77  WRK-ENCONTROU               PIC  X(001)        VALUE 'N'.
       77  WRK-ULT-TR-LIDO             PIC  9(002)        VALUE ZEROS.
______*77__WRK-CURRENT-TIMESTAMP_______PIC__X(026)________VALUE_SPACES.

       01  WRK-DATA-GERACAO.
           03 WRK-DG-DD                PIC  9(002)        VALUE ZEROS.
           03                          PIC  X             VALUE '/'.
           03 WRK-DG-MM                PIC  9(002)        VALUE ZEROS.
           03                          PIC  X             VALUE '/'.
           03 WRK-DG-AAAA              PIC  9(004)        VALUE ZEROS.

       01  WRK-DATA-DMA                PIC  9(008)         VALUE ZEROS.
       01  REDEFINES WRK-DATA-DMA.
           03  WRK-DIA-DMA             PIC  9(002).
           03  WRK-MES-DMA             PIC  9(002).
           03  WRK-ANO-DMA             PIC  9(004).

----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
       01  WRK-DATA-AMD                PIC  9(008)         VALUE ZEROS.
       01  REDEFINES WRK-DATA-AMD.
           03  WRK-ANO-AMD             PIC  9(004).
           03  WRK-MES-AMD             PIC  9(002).
           03  WRK-DIA-AMD             PIC  9(002).

       01  WRK-DATA-AM                 PIC  9(006)         VALUE ZEROS.
       01  REDEFINES WRK-DATA-AM.
           03  WRK-ANO-AM              PIC  9(004).
           03  WRK-MES-AM              PIC  9(002).

VI0516 01  WRK-AGENCIA-PARA            PIC  9(005)        VALUE ZEROS.
VI0516 01  WRK-CONTA-PARA              PIC  9(013)        VALUE ZEROS.
VI0516 01  REDEFINES  WRK-CONTA-PARA.
VI0516     03  FILLER                  PIC  9(006).
VI0516     03  WRK-CONTA-7-PARA        PIC  9(007).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(016)        VALUE
           'AREA PARA CHAVES'.
      *----------------------------------------------------------------*
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--

VI0516 01  WRK-XAV-DEPARCTA.
VI0516     05 WRK-XAV-AGENCIA          PIC  9(005)        VALUE ZEROS.
VI0516     05 WRK-XAV-CONTA            PIC  9(013)        VALUE ZEROS.

       01  WRK-CH-ANT.
           03 WRK-CH-ANT-AGCT.
              05 WRK-ANT-AGENCIA       PIC  9(005)        VALUE ZEROS.
              05 WRK-ANT-CONTA         PIC  9(013)        VALUE ZEROS.
           03 WRK-ANT-AM-APUR          PIC  9(006)        VALUE ZEROS.

       01  WRK-CH-ATU.
           03 WRK-CH-ATU-AGCT.
              05 WRK-ATU-AGENCIA       PIC  9(005)        VALUE ZEROS.
              05 WRK-ATU-CONTA         PIC  9(013)        VALUE ZEROS.
           03 WRK-ATU-AM-APUR          PIC  9(006)        VALUE ZEROS.

       01  WRK-CH-ANT-CESTA.
           05 WRK-ANT-CESTA            PIC  9(005)        VALUE ZEROS.

       01  WRK-CH-ATU-CESTA.
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
           05 WRK-ATU-CESTA            PIC  9(005)        VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(019)         VALUE
           ' AREA DE ENTRADA'.
      *----------------------------------------------------------------*

       COPY 'GFCTWF17'.

VI0516*----------------------------------------------------------------*
VI0516 01  FILLER                      PIC  X(019)         VALUE
VI0516     ' AREA DE DEPARCTA'.
VI0516*----------------------------------------------------------------*
VI0516
VI0516 COPY 'I#CLIBGB'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(019)         VALUE
           ' AREA DE REJEITAD'.
      *----------------------------------------------------------------*

----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
       COPY 'GFCTWF18'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(019)         VALUE
           ' AREA DE GFCTB0P0'.
      *----------------------------------------------------------------*

       COPY 'GFCTWF45'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(026)         VALUE
           'AREA DE ERRO - POOL7100'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-FILE-STATUS.
           05  FILLER                  PIC  X(008)         VALUE
               '** ERRO '.
           05  WRK-OPERACAO            PIC  X(013)         VALUE SPACES.
           05  FILLER                  PIC  X(012)         VALUE
               ' DO ARQUIVO '.
           05  WRK-NOME-ARQ            PIC  X(008)         VALUE SPACES.
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
           05  FILLER                  PIC  X(017)         VALUE
               ' - FILE-STATUS = '.
           05  WRK-FILE-STATUS         PIC  X(002)         VALUE SPACES.
           05  FILLER                  PIC  X(003)         VALUE ' **'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(019)         VALUE
           ' AREA PARA POOL7100'.
      *----------------------------------------------------------------*

       COPY 'POL7100C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA DE COMUNICACAO DO - GFCT5800'.
      *---------------------------------------------------------------*

       COPY 'GFCTWF03'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
           'AREA PARA TABELA DB2'.
      *---------------------------------------------------------------*

           EXEC SQL INCLUDE SQLCA    END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(030)         VALUE
           'FIM DA WORKING STORAGE SECTION'.
      *----------------------------------------------------------------*

      *================================================================*
VI0516 PROCEDURE DIVISION.
      *================================================================*
           MOVE ZEROS TO
               WRK-CAMPO-TIRA-SINAL
               WRK-CAMPO-TIRA-SINAL-17
               WRK-CAMPO-TIRA-SINAL-16
               WRK-CAMPO-TIRA-SINAL-15
               WRK-CAMPO-TIRA-SINAL-14
               WRK-CAMPO-TIRA-SINAL-13
               WRK-CAMPO-TIRA-SINAL-12
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
               WRK-CAMPO-TIRA-SINAL-11
               WRK-CAMPO-TIRA-SINAL-10
               WRK-CAMPO-TIRA-SINAL-09
               WRK-CAMPO-TIRA-SINAL-08
               WRK-CAMPO-TIRA-SINAL-07
               WRK-CAMPO-TIRA-SINAL-06
               WRK-CAMPO-TIRA-SINAL-05
               WRK-CAMPO-TIRA-SINAL-04
               WRK-CAMPO-TIRA-SINAL-03
               WRK-CAMPO-TIRA-SINAL-02
               WRK-CAMPO-TIRA-SINAL-01
               WRK-CAMPO-POE-SINAL-01
               WRK-CAMPO-POE-SINAL-01-C
               WRK-CAMPO-POE-SINAL-02
               WRK-CAMPO-POE-SINAL-02-C
               WRK-CAMPO-POE-SINAL-03
               WRK-CAMPO-POE-SINAL-03-C
               WRK-CAMPO-POE-SINAL-04
               WRK-CAMPO-POE-SINAL-04-C
               WRK-CAMPO-POE-SINAL-05
               WRK-CAMPO-POE-SINAL-05-C
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
               WRK-CAMPO-POE-SINAL-06
               WRK-CAMPO-POE-SINAL-06-C
               WRK-CAMPO-POE-SINAL-07
               WRK-CAMPO-POE-SINAL-07-C
               WRK-CAMPO-POE-SINAL-08
               WRK-CAMPO-POE-SINAL-08-C
               WRK-CAMPO-POE-SINAL-09
               WRK-CAMPO-POE-SINAL-09-C
               WRK-CAMPO-POE-SINAL-10
               WRK-CAMPO-POE-SINAL-10-C
               WRK-CAMPO-POE-SINAL-11
               WRK-CAMPO-POE-SINAL-11-C
               WRK-CAMPO-POE-SINAL-12
               WRK-CAMPO-POE-SINAL-12-C
               WRK-CAMPO-POE-SINAL-13
               WRK-CAMPO-POE-SINAL-13-C
               WRK-CAMPO-POE-SINAL-14
               WRK-CAMPO-POE-SINAL-14-C
               WRK-CAMPO-POE-SINAL-15
               WRK-CAMPO-POE-SINAL-15-C
               WRK-CAMPO-POE-SINAL-16
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
               WRK-CAMPO-POE-SINAL-16-C
               WRK-CAMPO-POE-SINAL-17
               WRK-CAMPO-POE-SINAL-17-C.

           MOVE '+' TO
               WRK-CAMPO-TIRA-SINAL-S
               WRK-CAMPO-POE-SINAL-01-S
               WRK-CAMPO-POE-SINAL-02-S
               WRK-CAMPO-POE-SINAL-03-S
               WRK-CAMPO-POE-SINAL-04-S
               WRK-CAMPO-POE-SINAL-05-S
               WRK-CAMPO-POE-SINAL-06-S
               WRK-CAMPO-POE-SINAL-07-S
               WRK-CAMPO-POE-SINAL-08-S
               WRK-CAMPO-POE-SINAL-09-S
               WRK-CAMPO-POE-SINAL-10-S
               WRK-CAMPO-POE-SINAL-11-S
               WRK-CAMPO-POE-SINAL-12-S
               WRK-CAMPO-POE-SINAL-13-S
               WRK-CAMPO-POE-SINAL-14-S
               WRK-CAMPO-POE-SINAL-15-S
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
               WRK-CAMPO-POE-SINAL-16-S
               WRK-CAMPO-POE-SINAL-17-S.
      ******************************************************************
      *    CHAMA AS ROTINAS PRINCIPAIS DO PROCESSAMENTO.               *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL1050'.

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-VERIFICAR-VAZIO.

           PERFORM 3000-PROCESSAR      UNTIL
               WRK-FS-ENTRADA          EQUAL '10'.

           PERFORM 9000-FINALIZAR.

      *----------------------------------------------------------------*
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCEDIMENTOS INICIAIS DO PROGRAMA                          *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT  ENTRADA
VI0516                 DEPARCTA
                OUTPUT REJEITAD
                       GFCTB0P0.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

           EXEC SQL SET :WRK-TIMESTAMP-INI = CURRENT TIMESTAMP
           END-EXEC.
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--


      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA QUE EFETUA TESTE DE FILE-STATUS.                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-ENTRADA.

           PERFORM 1120-TESTAR-FS-REJEITAD.

           PERFORM 1130-TESTAR-FS-GFCTB0P0.

VI0516     PERFORM 1140-TESTAR-FS-DEPARCTA.

----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE TESTE PARA ARQUIVO ENTRADA.                       *
      ******************************************************************
      *----------------------------------------------------------------*
       1110-TESTAR-FS-ENTRADA          SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ENTRADA          NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'ENTRADA'          TO WRK-NOME-ARQ
               MOVE WRK-FS-ENTRADA     TO WRK-FILE-STATUS
               MOVE WRK-ERRO-FILE-STATUS  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE TESTE PARA ARQUIVO REJEITAD                       *
      ******************************************************************
      *----------------------------------------------------------------*
       1120-TESTAR-FS-REJEITAD          SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-REJEITAD         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'REJEITAD'         TO WRK-NOME-ARQ
               MOVE WRK-FS-REJEITAD    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-FILE-STATUS  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
      ******************************************************************
      *    ROTINA DE TESTE PARA ARQUIVO GFCTB0P0                       *
      ******************************************************************
      *----------------------------------------------------------------*
       1130-TESTAR-FS-GFCTB0P0          SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-GFCTB0P0         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'GFCTB0P0'         TO WRK-NOME-ARQ
               MOVE WRK-FS-GFCTB0P0    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-FILE-STATUS  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

VI0516******************************************************************
VI0516*    ROTINA DE TESTE PARA ARQUIVO DEPARCTA.                      *
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
VI0516******************************************************************
VI0516*----------------------------------------------------------------*
VI0516 1140-TESTAR-FS-DEPARCTA       SECTION.
VI0516*----------------------------------------------------------------*
VI0516
VI0516     IF  WRK-FS-DEPARCTA         NOT EQUAL '00'
VI0516         MOVE 'APL'              TO ERR-TIPO-ACESSO
VI0516         MOVE 'DEPARCTA'         TO WRK-NOME-ARQ
VI0516         MOVE WRK-FS-DEPARCTA    TO WRK-FILE-STATUS
VI0516         MOVE WRK-ERRO-FILE-STATUS  TO ERR-TEXTO
VI0516         PERFORM 9999-PROCESSAR-ROTINA-ERRO
VI0516     END-IF.
VI0516
VI0516*----------------------------------------------------------------*
VI0516 1140-99-FIM.                    EXIT.
VI0516*----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICAR SE ARQUIVO ENTRADA ESTA VAZIO                     *
      ******************************************************************
      *----------------------------------------------------------------*
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
       2000-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

VI0516     PERFORM 2050-LER-DEPARCTA.

VI0516     IF  WRK-FS-DEPARCTA         EQUAL '10'
VI0516         MOVE 'APL'              TO ERR-TIPO-ACESSO
VI0516         MOVE 'DEPARCTA'         TO WRK-NOME-ARQ
VI0516         MOVE WRK-FS-DEPARCTA    TO WRK-FILE-STATUS
VI0516         MOVE WRK-ERRO-FILE-STATUS  TO ERR-TEXTO
VI0516         DISPLAY '*------------------------------------------*'
VI0516         DISPLAY '*             PROGRAMA GFCT5718            *'
VI0516         DISPLAY '*             -----------------            *'
VI0516         DISPLAY '*                                          *'
VI0516         DISPLAY '*         ARQUIVO DE DEPARCTA VAZIO        *'
VI0516         DISPLAY '*------------------------------------------*'
VI0516         PERFORM 9999-PROCESSAR-ROTINA-ERRO
VI0516     END-IF.

           PERFORM 2100-LER-ENTRADA.

----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

VI0516******************************************************************
VI0516*  ROTINA QUE EFETUA LEITURA E TRATAMENTO DO ARQUIVO DE DEPARCTA *
VI0516******************************************************************
VI0516*----------------------------------------------------------------*
VI0516 2050-LER-DEPARCTA               SECTION.
VI0516*----------------------------------------------------------------*
VI0516
VI0516     READ  DEPARCTA              INTO  REG-CLIB-INC.
VI0516
VI0516     IF  WRK-FS-DEPARCTA         EQUAL '10'
VI0516         MOVE  HIGH-VALUES       TO WRK-XAV-DEPARCTA
VI0516         GO TO 2050-99-FIM
VI0516     END-IF.
VI0516
VI0516     MOVE WRK-LEITURA            TO WRK-OPERACAO.
VI0516
VI0516     PERFORM 1140-TESTAR-FS-DEPARCTA.
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
VI0516
VI0516     ADD 1                       TO ACU-LIDOS-DEPARCTA.
VI0516
VI0516     MOVE INC-AGENCIA-DE         TO WRK-XAV-AGENCIA.
VI0516     MOVE INC-CONTA-DE           TO WRK-XAV-CONTA.
VI0516
VI0516
VI0516*----------------------------------------------------------------*
VI0516 2050-99-FIM.                    EXIT.
VI0516*----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA QUE EFETUA LEITURA E TRATAMENTO DO ARQUIVO DE ENTRADA*
      ******************************************************************
      *----------------------------------------------------------------*
       2100-LER-ENTRADA                SECTION.
      *----------------------------------------------------------------*

           READ ENTRADA INTO F17-REGISTRO.

           IF  WRK-FS-ENTRADA          EQUAL '10'
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
               GO TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1110-TESTAR-FS-ENTRADA.

           ADD 1                       TO ACU-LIDOS-ENTRADA.

           MOVE WRK-CH-ATU-CESTA       TO WRK-CH-ANT-CESTA.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCESSAMENTO PRINCIPAL                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--

           PERFORM 3100-CONSISTENCIA-FISICA.

           IF WRK-ERRO EQUAL TO 'N' AND
              F17-TR   EQUAL TO 1
              PERFORM 3200-CONSISTENCIA-LOGICA
           END-IF.

           IF F17-TR EQUAL TO 1
              IF WRK-ERRO EQUAL TO 'S'
                 PERFORM 3500-GRAVAR-REJEITADOS
              ELSE
                 PERFORM 3600-GRAVAR-GFCTB0P0
              END-IF
           END-IF.

           PERFORM 2100-LER-ENTRADA.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--

      ******************************************************************
      *    ROTINA DE CONSISTENCIA FISICA                               *
      ******************************************************************
      *----------------------------------------------------------------*
       3100-CONSISTENCIA-FISICA        SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-ERRO.
           MOVE SPACES                 TO WRK-MSG.

           IF (F17-TR                  NOT NUMERIC     OR
              (F17-TR                  NOT EQUAL ZERO  AND
               F17-TR                  NOT EQUAL 1     AND
               F17-TR                  NOT EQUAL 9))
               MOVE 'S'                 TO WRK-ERRO
               MOVE 'TIPO DE REGISTRO INVALIDO' TO WRK-MSG
               GO TO 3100-99-FIM
           END-IF.

           IF (ACU-LIDOS-ENTRADA       EQUAL 1         AND
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
               F17-TR                  NOT EQUAL ZERO)
              MOVE 'APL'               TO ERR-TIPO-ACESSO
              MOVE 'FALTOU REGISTRO HEADER' TO ERR-TEXTO
              PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE F17-TR TO WRK-ULT-TR-LIDO.

           EVALUATE F17-TR
               WHEN 0 PERFORM 3110-CONSISTE-HEADER
               WHEN 1 PERFORM 3120-CONSISTE-DADOS
               WHEN 9 PERFORM 3130-CONSISTE-TRAILER
           END-EVALUATE.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE CONSISTENCIA FISICA HEADER                        *
      ******************************************************************
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
      *----------------------------------------------------------------*
       3110-CONSISTE-HEADER            SECTION.
      *----------------------------------------------------------------*

           IF (F17-DATA-GERACAO  NOT NUMERIC OR
               F17-DATA-GERACAO EQUAL ZEROS)
              MOVE 'APL'               TO ERR-TIPO-ACESSO
              MOVE 'DATA GERACAO INVALIDA' TO ERR-TEXTO
              PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE F17-DATA-GERACAO TO WRK-DATA-AMD.
           PERFORM 5700-VALIDAR-DATA.

           IF WRK-DATA-VALIDA EQUAL 'N'
              MOVE 'APL'               TO ERR-TIPO-ACESSO
              MOVE 'DATA GERACAO INVALIDA' TO ERR-TEXTO
              PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF F17-NOME-ARQUIVO EQUAL SPACES
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
              MOVE 'APL'               TO ERR-TIPO-ACESSO
              MOVE 'NOME ARQUIVO INVALIDO' TO ERR-TEXTO
              PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE  WRK-DIA-AMD           TO WRK-DG-DD.
           MOVE  WRK-MES-AMD           TO WRK-DG-MM.
           MOVE  WRK-ANO-AMD           TO WRK-DG-AAAA.
           MOVE  WRK-MES-AMD           TO WRK-MES-AM.
           MOVE  WRK-ANO-AMD           TO WRK-ANO-AM.
           MOVE  F17-NOME-ARQUIVO      TO WRK-NOME-ARQUIVO.

      *----------------------------------------------------------------*
       3110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE CONSISTENCIA FISICA DADOS                         *
      ******************************************************************
      *----------------------------------------------------------------*
       3120-CONSISTE-DADOS             SECTION.
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
      *----------------------------------------------------------------*

           IF (F17-AGENCIA NOT NUMERIC OR
               F17-AGENCIA EQUAL ZEROS)
              MOVE 'S'                TO WRK-ERRO
              MOVE 'AGENCIA INVALIDA' TO WRK-MSG
              GO TO 3120-99-FIM
           END-IF.

           IF (F17-CONTA NOT NUMERIC OR
               F17-CONTA EQUAL ZEROS)
              MOVE 'S'                TO WRK-ERRO
              MOVE 'CONTA INVALIDA' TO WRK-MSG
              GO TO 3120-99-FIM
           END-IF.

           IF F17-DIGITO NOT NUMERIC
              MOVE 'S'                TO WRK-ERRO
              MOVE 'DIGITO INVALIDO' TO WRK-MSG
              GO TO 3120-99-FIM
           END-IF.
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--

           IF F17-CPF-CNPJ NOT NUMERIC
              MOVE 'S'                TO WRK-ERRO
              MOVE 'CPF CNPJ INVALIDO' TO WRK-MSG
              GO TO 3120-99-FIM
           END-IF.

           IF F17-FILIAL NOT NUMERIC
              MOVE 'S'                TO WRK-ERRO
              MOVE 'FILIAL CNPJ INVALIDO' TO WRK-MSG
              GO TO 3120-99-FIM
           END-IF.

           IF F17-DIGITO-FJ NOT NUMERIC
              MOVE 'S'                TO WRK-ERRO
              MOVE 'DIGITO CPF CNPJ INVALIDO' TO WRK-MSG
              GO TO 3120-99-FIM
           END-IF.

           IF (F17-CESTA NOT NUMERIC OR
               F17-CESTA EQUAL ZEROS)
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
              MOVE 'S'                TO WRK-ERRO
              MOVE 'CESTA INVALIDA' TO WRK-MSG
              GO TO 3120-99-FIM
           END-IF.

           IF F17-AM-APUR NOT NUMERIC
              MOVE 'S'                TO WRK-ERRO
              MOVE 'ANO MES APURACAO INVALIDO' TO WRK-MSG
              GO TO 3120-99-FIM
           END-IF.

           MOVE F17-AM-APUR TO WRK-DATA-AM.
           MOVE WRK-ANO-AM  TO WRK-ANO-AMD.
           MOVE WRK-MES-AM  TO WRK-MES-AMD.
           MOVE 1           TO WRK-DIA-AMD.

           PERFORM 5700-VALIDAR-DATA.

           IF WRK-DATA-VALIDA EQUAL 'N'
              MOVE 'S'                TO WRK-ERRO
              MOVE 'ANO MES APURACAO INVALIDO' TO WRK-MSG
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
              GO TO 3120-99-FIM
           END-IF.

           MOVE F17-AGENCIA            TO WRK-ATU-AGENCIA.
           MOVE F17-CONTA              TO WRK-ATU-CONTA.
           MOVE F17-AM-APUR            TO WRK-ATU-AM-APUR.

           IF WRK-CH-ATU          LESS THAN WRK-CH-ANT

              DISPLAY '************ GFCT5718 ************'
              DISPLAY '*                                *'
              DISPLAY '*        ARQUIVO ENTRADA         *'
              DISPLAY '*        NAO CLASSIFICADO        *'
              DISPLAY '*                                *'
              DISPLAY '*       PROGRAMA ENCERRADO       *'
              DISPLAY '*                                *'
              DISPLAY '************ GFCT4701 ************'
              MOVE 'APL'              TO ERR-TIPO-ACESSO
              MOVE 'ENTRADA'          TO WRK-NOME-ARQ
              MOVE WRK-FS-ENTRADA     TO WRK-FILE-STATUS
              MOVE WRK-ERRO-FILE-STATUS  TO ERR-TEXTO
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
              PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.


           IF F17-VSDO-MED-FUNDO NOT NUMERIC
              MOVE 'S'                TO WRK-ERRO
              MOVE 'SALDO MEDIO FUNDOS INVALIDO' TO WRK-MSG
              GO TO 3120-99-FIM
           END-IF.

           IF F17-VSDO-MED-POUP NOT NUMERIC
              MOVE 'S'                TO WRK-ERRO
              MOVE 'SALDO MEDIO POUPANCA INVALIDO' TO WRK-MSG
              GO TO 3120-99-FIM
           END-IF.

           IF F17-VSDO-MED-CDB NOT NUMERIC
              MOVE 'S'                TO WRK-ERRO
              MOVE 'SALDO MEDIO CDB INVALIDO' TO WRK-MSG
              GO TO 3120-99-FIM
           END-IF.
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--

           IF F17-VSDO-MED-PREVD NOT NUMERIC
              MOVE 'S'                TO WRK-ERRO
              MOVE 'SALDO MEDIO PREVID. INVALIDO' TO WRK-MSG
              GO TO 3120-99-FIM
           END-IF.

           IF F17-VSDO-APLIC-ACOES NOT NUMERIC
              MOVE 'S'                TO WRK-ERRO
              MOVE 'SALDO EM ACOES INVALIDO' TO WRK-MSG
              GO TO 3120-99-FIM
           END-IF.

           IF F17-VSDO-APLIC-TSORO NOT NUMERIC
              MOVE 'S'                TO WRK-ERRO
              MOVE 'SALDO TESOURO DIRETO INVALIDO' TO WRK-MSG
              GO TO 3120-99-FIM
           END-IF.

           IF (F17-VSDO-MED-FUNDO   EQUAL TO ZEROS AND
               F17-VSDO-MED-POUP    EQUAL TO ZEROS AND
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
               F17-VSDO-MED-CDB     EQUAL TO ZEROS AND
               F17-VSDO-MED-PREVD   EQUAL TO ZEROS AND
               F17-VSDO-APLIC-ACOES EQUAL TO ZEROS AND
               F17-VSDO-APLIC-TSORO EQUAL TO ZEROS)
              MOVE 'S'                TO WRK-ERRO
              MOVE 'TODOS VALORES ZERADOS' TO WRK-MSG
              GO TO 3120-99-FIM
           END-IF.

      *----------------------------------------------------------------*
       3120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE CONSISTENCIA FISICA TRAILER                       *
      ******************************************************************
      *----------------------------------------------------------------*
       3130-CONSISTE-TRAILER           SECTION.
      *----------------------------------------------------------------*

           IF (F17-QTDE-REGISTROS NOT NUMERIC OR
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
               F17-QTDE-REGISTROS NOT EQUAL ACU-LIDOS-ENTRADA)
              MOVE 'APL'              TO ERR-TIPO-ACESSO
              MOVE 'REGISTRO TRAILER INVALIDO' TO ERR-TEXTO
              PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       3130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE CONSISTENCIA LOGICA                               *
      ******************************************************************
      *----------------------------------------------------------------*
       3200-CONSISTENCIA-LOGICA        SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-ERRO.
           MOVE SPACES                 TO WRK-MSG.

           IF WRK-CH-ATU-AGCT  NOT EQUAL TO WRK-CH-ANT-AGCT
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
VI0516        MOVE 'N' TO WRK-ENCONTROU
VI0516        PERFORM 3300-BALANCE-CLIB
           END-IF.

VI0516     IF WRK-ENCONTROU EQUAL 'N'
VI0516        MOVE 'S'                          TO WRK-ERRO
VI0516        MOVE 'AG/CTA HSBC NAO LOCALIZADA' TO WRK-MSG
VI0516     END-IF.

           IF WRK-ERRO EQUAL TO 'S'
              GO TO 3200-99-FIM
           END-IF.

           MOVE F17-CESTA                 TO WRK-ATU-CESTA.

           IF WRK-CH-ATU-CESTA NOT EQUAL TO WRK-CH-ANT-CESTA
              PERFORM 6000-VERIFICA-CHAMADA-5800
           END-IF.

           EVALUATE F03-TAR-COD-RET
               WHEN ZEROS
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
                   CONTINUE
               WHEN 04
                   MOVE 'S'                TO WRK-ERRO
                   MOVE 'TARIFA HSBC NAO LOCALIZADA' TO WRK-MSG
               WHEN 08
                   MOVE 'S'                TO WRK-ERRO
                   MOVE 'TARIFA HSBC INVALIDA' TO WRK-MSG
               WHEN OTHER
                   MOVE 'S'                TO WRK-ERRO
                   MOVE 'GFCT5800 - ERRO SISTEMICO' TO WRK-MSG
           END-EVALUATE.

           IF WRK-ERRO EQUAL TO 'S'
              GO TO 3200-99-FIM
           END-IF.

           IF WRK-CH-ATU EQUAL TO WRK-CH-ANT
              MOVE 'S'             TO WRK-ERRO
              MOVE 'REGISTRO DUPLICADO' TO WRK-MSG
              GO TO 3200-99-FIM
           END-IF.
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--

           MOVE WRK-CH-ATU             TO WRK-CH-ANT.
      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

VI0516******************************************************************
VI0516*   BALANCE LINE COM ARQUIVO CLIB DEPARA DE CONTA HSBC X BRAD    *
VI0516******************************************************************
VI0516*----------------------------------------------------------------*
VI0516 3300-BALANCE-CLIB               SECTION.
VI0516*----------------------------------------------------------------*
VI0516
VI0516     IF WRK-CH-ATU-AGCT  GREATER  WRK-XAV-DEPARCTA
VI0516        PERFORM 2050-LER-DEPARCTA
VI0516        GO TO  3300-BALANCE-CLIB
VI0516     ELSE
VI0516        IF WRK-CH-ATU-AGCT  EQUAL  WRK-XAV-DEPARCTA
VI0516           MOVE  'S' TO  WRK-ENCONTROU
VI0516           MOVE  BRAD-AGENCIA-PARA TO  WRK-AGENCIA-PARA
VI0516           MOVE  BRAD-CONTA-PARA   TO  WRK-CONTA-PARA
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
VI0516        END-IF
VI0516     END-IF.
VI0516
VI0516*----------------------------------------------------------------*
VI0516 3300-99-FIM.                    EXIT.
VI0516*----------------------------------------------------------------*

      ******************************************************************
      *   GRAVAR REJEITADOS                                            *
      ******************************************************************
      *----------------------------------------------------------------*
       3500-GRAVAR-REJEITADOS          SECTION.
      *----------------------------------------------------------------*

           MOVE F17-DADOS              TO F18-DADOS.
           MOVE WRK-MSG                TO F18-MSG-ERRO.
           MOVE SPACES                 TO F18-RESERVA.

           WRITE FD-REJEITAD           FROM F18-REGISTRO.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--

           PERFORM 1120-TESTAR-FS-REJEITAD.

           ADD 1 TO ACU-GRAVADOS-REJEITAD.

      *----------------------------------------------------------------*
       3500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *   GRAVAR GFCTB0P0 - MOVTO_EVNTO_CRRTT                          *
      ******************************************************************
      *----------------------------------------------------------------*
       3600-GRAVAR-GFCTB0P0            SECTION.
      *----------------------------------------------------------------*

           MOVE 237                    TO WF45-CBCO.
VI0516     MOVE WRK-AGENCIA-PARA       TO WF45-CAG-BCRIA.
VI0516     MOVE WRK-CONTA-7-PARA       TO WF45-CCTA-CLI.
           MOVE F17-AM-APUR            TO WF45-DANO-MES-SDO.
           MOVE F03-TAR-BRAD           TO WF45-CSERVC-TARIF.
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
           MOVE F17-VSDO-MED-FUNDO     TO WF45-VSDO-MED-FUNDO.
           MOVE F17-VSDO-MED-POUP      TO WF45-VSDO-MED-POUP.
           MOVE F17-VSDO-MED-CDB       TO WF45-VSDO-MED-CERTF.
           MOVE F17-VSDO-MED-PREVD     TO WF45-VSDO-MED-PREVD.
           MOVE F17-VSDO-APLIC-ACOES   TO WF45-VSDO-APLIC-ACOES.
           MOVE F17-VSDO-APLIC-TSORO   TO WF45-VSDO-APLIC-TSORO.
           MOVE ZEROS                  TO WF45-VSDO-APLIC-DBTRS.
           MOVE ZEROS                  TO WF45-VSDO-APLIC-DERIV.
VI0516     MOVE F17-CPF-CNPJ           TO WF45-CNPJ-CPF
VI0516     MOVE F17-FILIAL             TO WF45-FILIAL
VI0516     MOVE F17-DIGITO-FJ          TO WF45-DIGITO-FJ

           MOVE F17-AGENCIA            TO WF45-AGENCIA-DE.
           MOVE F17-CONTA              TO WF45-CONTA-DE.

           WRITE FD-GFCTB0P0           FROM WF45-REGISTRO.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 1130-TESTAR-FS-GFCTB0P0.

----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
           ADD 1 TO ACU-GRAVADOS-GFCTB0P0.

           MOVE 'N'                    TO WRK-ERRO.
           MOVE SPACES                 TO WRK-MSG.

      *----------------------------------------------------------------*
       3600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *   VALIDAR DATA                                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       5700-VALIDAR-DATA               SECTION.
      *----------------------------------------------------------------*

           MOVE 'S'                    TO WRK-DATA-VALIDA.

           MOVE WRK-ANO-AMD            TO WRK-ANO-DMA
           MOVE WRK-MES-AMD            TO WRK-MES-DMA
           MOVE WRK-DIA-AMD            TO WRK-DIA-DMA
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--

           CALL 'POOL1470'             USING WRK-DATA-DMA.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'N'                TO WRK-DATA-VALIDA
           END-IF.

      *----------------------------------------------------------------*
       5700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       6000-VERIFICA-CHAMADA-5800     SECTION.
      *----------------------------------------------------------------*

           MOVE 'N' TO WRK-ACHOU.

           PERFORM VARYING WRK-COUNT FROM 1 BY 1 UNTIL
                           WRK-ACHOU EQUAL 'S' OR
                           WRK-COUNT GREATER WRK-CTM
              IF WRK-ATU-CESTA EQUAL FW5-TAR-HSBC(WRK-COUNT)
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
                 MOVE 'S' TO WRK-ACHOU
                 MOVE FW5-TAB-INT-OCOR(WRK-COUNT) TO
                      F03-DP-CESTA-HSBC-REG
              END-IF
           END-PERFORM.

           IF WRK-ACHOU EQUAL 'N'
              MOVE WRK-ATU-CESTA      TO F03-TAR-HSBC
              MOVE 'GFCT5800'         TO WRK-MODULO
              ADD 1 TO ACU-ACESSOS-5800
              CALL WRK-MODULO   USING F03-DP-CESTA-HSBC-REG
              IF WRK-CTM LESS 1000
                 ADD 1 TO WRK-CTM
                 MOVE F03-DP-CESTA-HSBC-REG(1:18) TO
                      FW5-TAB-INT-OCOR(WRK-CTM)
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       6000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
      ******************************************************************
      *    ROTINA QUE EFETUA PROCESSAMENTO FINAL DO PROGRAMA           *
      ******************************************************************
      *----------------------------------------------------------------*
       9000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           IF (ACU-LIDOS-ENTRADA NOT EQUAL ZEROS AND
               WRK-ULT-TR-LIDO   NOT EQUAL TO 9)
              MOVE 'APL'              TO ERR-TIPO-ACESSO
              MOVE 'FALTOU REGISTRO TRAILER' TO ERR-TEXTO
              PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 9100-EMITIR-DISPLAY.

           CLOSE ENTRADA
VI0516           DEPARCTA
                 REJEITAD
                 GFCTB0P0.

----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

           STOP RUN.

      *----------------------------------------------------------------*
       9000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE EMISSAO DE DISPLAYS DE TOTAIS                     *
      ******************************************************************
      *----------------------------------------------------------------*
       9100-EMITIR-DISPLAY             SECTION.
      *----------------------------------------------------------------*

           IF  ACU-LIDOS-ENTRADA           NOT EQUAL ZEROS
               DISPLAY '*-----------------------------------------------
      -        '--------*'
               DISPLAY '*                   PROGRAMA GFCT5718
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
      -        '        *'
               DISPLAY '*                   -----------------
      -        '        *'
               DISPLAY '*
      -        '        *'
               DISPLAY '*          PROCESSAMENTO  EFETUADO COM SUCESSO
      -        '        *'
               DISPLAY '* INVESTIMENTOS ULTIMOS 03 MESES
      -        '        *'
               DISPLAY '* CONSISTENCIA E GERACAO DE IMAGEM DA TABELA
      -        '        *'
               DISPLAY '* GFCTB0P0 - TPOSIC_SDO_APLIC
      -        '        *'
               DISPLAY '*-----------------------------------------------
      -        '--------*'
               DISPLAY '* ARQUIVO - ' WRK-NOME-ARQUIVO '              *'
               DISPLAY '*
      -        '        *'
               DISPLAY '* GERACAO - ' WRK-DATA-GERACAO
                       '                                  *'
               DISPLAY '*-----------------------------------------------
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
      -        '--------*'
               DISPLAY '************************************************
      -        '*********'
               MOVE ACU-LIDOS-ENTRADA      TO WRK-MASCARA
               DISPLAY '* TOTAL DE REG. LIDOS              = '
                 WRK-MASCARA
                           ' *'
VI0516         MOVE ACU-LIDOS-DEPARCTA     TO WRK-MASCARA
VI0516         DISPLAY '* TOTAL DE REG. LIDOS  DEPARCTA    = '
VI0516           WRK-MASCARA
VI0516                     ' *'
               MOVE ACU-GRAVADOS-GFCTB0P0  TO WRK-MASCARA
               DISPLAY '* TOTAL DE REG. GRAVADOS  GFCTB0P0 = '
                 WRK-MASCARA
                           ' *'
               MOVE ACU-GRAVADOS-REJEITAD  TO WRK-MASCARA
               DISPLAY '* TOTAL DE REG. REJEITADOS         = '
                   WRK-MASCARA
                            ' *'
              MOVE ACU-ACESSOS-5800 TO WRK-MASCARA
              DISPLAY '= ACESSOS GFCT5800 ' WRK-MASCARA
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
              MOVE ACU-ACESSOS-5801 TO WRK-MASCARA
              DISPLAY '= ACESSOS GFCT5801 ' WRK-MASCARA
              DISPLAY '= TMS INIC ' WRK-TIMESTAMP-INI
              EXEC SQL
                 SET :WRK-TIMESTAMP-FIM = CURRENT TIMESTAMP
              END-EXEC
              DISPLAY '= TMS FIM ' WRK-TIMESTAMP-FIM

               DISPLAY '************************************************
      -        '*********'
           ELSE
               DISPLAY '*-----------------------------------------------
      -        '*'
               DISPLAY '*               PROGRAMA GFCT5718
      -        '*'
               DISPLAY '*               -----------------
      -        '*'
               DISPLAY '*
      -        '*'
               DISPLAY '*           ARQUIVO DE ENTRADA VAZIO
      -        '*'
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
               DISPLAY '*-----------------------------------------------
      -        '*'
           END-IF.

      *----------------------------------------------------------------*
       9100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE PROCESSAMENTO DE ERRRO                            *
      ******************************************************************
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT5718'             TO ERR-PGM.

           CALL 'POOL7100'         USING WRK-BATCH
                                         ERRO-AREA.
           MOVE 4                      TO RETURN-CODE.
----+----1----+----2----+----3----+----4----+----5----+----6----+----7--

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
