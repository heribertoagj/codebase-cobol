      *===============================================================*
       IDENTIFICATION DIVISION.
      *===============================================================*

       PROGRAM-ID. GFCT2935.
       AUTHOR.     JOHNNY.

      *===============================================================*
      *                   GPTI -  S I S T E M A S                     *
      *--------------------------------------------------------------*
      *                                                               *
      *      PROGRAMA     : GFCT2935                                  *
      *      ANALISTA     : J.C.R.       -  GPTI                      *
      *      DATA         : 05/01/2009                                *
      *                                                               *
      *      OBJETIVO     : EMISSAO DE FAC                            *
      *                                                               *
      *      ARQUIVOS:                                                *
      *         DDNAME                           INCLUDE/BOOK         *
      *         FACANT                                                *
      *         RATEIDEP                                              *
      *         NOMEFUN1                                              *
      *         FACATU                                                *
      *         RELFAC                                                *
      *===============================================================*
      *                    M A N U T E N C O E S                      *
      *---------------------------------------------------------------*
      *  PROGRAMADOR :  XXXXXXXXXXXXXXXXXXXXXXX                       *
      *  ANALISTA    :  XXXXXXXXXXXXXXXXXXXXXXX                       *
      *  DATA        :  XX/XX/XXXX                                    *
      *---------------------------------------------------------------*
      *  OBJETIVO    :  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
      *                 XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
      *---------------------------------------------------------------*
      *                                                               *
      *===============================================================*
           EJECT
      *===============================================================*
       ENVIRONMENT DIVISION.
      *===============================================================*

       CONFIGURATION SECTION.
      *---------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.


      *---------------------------------------------------------------*
       INPUT-OUTPUT SECTION.
      *---------------------------------------------------------------*

       FILE-CONTROL.

           SELECT FACANT   ASSIGN TO UT-S-FACANT
                      FILE STATUS IS WRK-FS-FACANT.

           SELECT ARQDATA ASSIGN TO UT-S-ARQDATA
                      FILE STATUS IS WRK-FS-ARQDATA.

           SELECT FRAGCO01 ASSIGN TO UT-S-FRAGCO01
                      FILE STATUS IS WRK-FS-FRAGCO01.
           SELECT RATEIDEP ASSIGN TO UT-S-RATEIDEP
                      FILE STATUS IS WRK-FS-RATEIDEP.

           SELECT NOMEFUN1 ASSIGN TO UT-S-NOMEFUN1
                      FILE STATUS IS WRK-FS-NOMEFUN1.

           SELECT FACATU   ASSIGN TO UT-S-FACATU
                      FILE STATUS IS WRK-FS-FACATU.

           SELECT RELFAC   ASSIGN TO UT-S-RELFAC
                      FILE STATUS IS WRK-FS-RELFAC.

      *===============================================================*
       DATA DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       FILE SECTION.
      *---------------------------------------------------------------*


      *    INPUT:     ARQUIVO FACANT                                  *
      *---------------------------------------------------------------*

       FD  FACANT
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01 FD-REG-FACANT.
         05 FD-ANT-NRO-FAC             PIC  9(03).
         05 FD-ANT-PRI-OBJ-1000-1999   PIC  9(07)   COMP-3.
         05 FD-ANT-ULT-OBJ-1000-1999   PIC  9(07)   COMP-3.
         05 FD-ANT-QTD-OBJ-1000-1999   PIC  9(07)   COMP-3.
         05 FD-ANT-PRI-OBJ-2000-2999   PIC  9(07)   COMP-3.
         05 FD-ANT-ULT-OBJ-2000-2999   PIC  9(07)   COMP-3.
         05 FD-ANT-QTD-OBJ-2000-2999   PIC  9(07)   COMP-3.
         05 FD-ANT-PRI-OBJ-3000-3999   PIC  9(07)   COMP-3.
         05 FD-ANT-ULT-OBJ-3000-3999   PIC  9(07)   COMP-3.
         05 FD-ANT-QTD-OBJ-3000-3999   PIC  9(07)   COMP-3.
         05 FD-ANT-PRI-OBJ-4000-4999   PIC  9(07)   COMP-3.
         05 FD-ANT-ULT-OBJ-4000-4999   PIC  9(07)   COMP-3.
         05 FD-ANT-PRI-OBJ-5000-5999   PIC  9(07)   COMP-3.
         05 FD-ANT-ULT-OBJ-5000-5999   PIC  9(07)   COMP-3.
         05 FD-ANT-QTD-OBJ-5000-5999   PIC  9(07)   COMP-3.
         05 FD-ANT-PRI-OBJ-6000-9999   PIC  9(07)   COMP-3.
         05 FD-ANT-ULT-OBJ-6000-9999   PIC  9(07)   COMP-3.
         05 FD-ANT-PRI-AMR-6000-9999   PIC  9(07)   COMP-3.
         05 FD-ANT-ULT-AMR-6000-9999   PIC  9(07)   COMP-3.
         05 FD-ANT-QTD-OBJ-6000-9999   PIC  9(07)   COMP-3.
         05 FD-ANT-QTD-AMR-6000-9999   PIC  9(07)   COMP-3.
         05 FD-ANT-PRI-OBJ-10000-99999 PIC  9(07)   COMP-3.
         05 FD-ANT-ULT-OBJ-10000-99999 PIC  9(07)   COMP-3.
         05 FD-ANT-PRI-AMR-10000-99999 PIC  9(07)   COMP-3.
         05 FD-ANT-ULT-AMR-10000-99999 PIC  9(07)   COMP-3.
         05 FD-ANT-QTD-OBJ-10000-99999 PIC  9(07)   COMP-3.
         05 FD-ANT-QTD-AMR-10000-99999 PIC  9(07)   COMP-3.
         05 FD-ANT-TOTAL-AMARRADOS     PIC  9(07)   COMP-3.
         05 FD-ANT-TOTAL-OBJETOS       PIC  9(07)   COMP-3.
         05 FD-ANT-VALOR-FRANQUIA      PIC 9(13)V99 COMP-3.
         05 FD-ANT-FATURAMENTO         PIC 9(15)V99 COMP-3.
         05 FD-ANT-DATA-GRAVACAO       PIC  X(010).
         05 FD-ANT-HORA-GRAVACAO       PIC  X(006).

      *---------------------------------------------------------------*
      *    INPUT:     ARQUIVO ARQDATA  LRECL = 8                     *
      *---------------------------------------------------------------*

       FD  ARQDATA
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY GFCTWAEA.

      *---------------------------------------------------------------*
      *    INPUT :    ARQUIVO FRAGCO01                                *
      *---------------------------------------------------------------*

       FD  FRAGCO01
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.


      *---------------------------------------------------------------*
      *    INPUT :    ARQUIVO RATEIDEP (DUMMY)                        *
      *---------------------------------------------------------------*

       FD  RATEIDEP
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01 FD-REG-RATEIDEP.
           05 FD-RAT-PERIODO-I       PIC X(010).
           05 FD-RAT-PERIODO-F       PIC X(010).
           05 FD-RAT-GGL             PIC X(004).
           05 FD-RAT-TOT-GGL         PIC S9(012)V99 COMP-3.
           05 FD-RAT-TOT-EXT-GGL     PIC 9(012)     COMP-3.
           05 FD-RAT-DCIP            PIC X(004).
           05 FD-RAT-TOT-DCIP        PIC S9(012)V99 COMP-3.
           05 FD-RAT-TOT-EXT-DCIP    PIC 9(012)     COMP-3.
           05 FD-RAT-DAC             PIC X(004).
           05 FD-RAT-TOT-DAC         PIC S9(012)V99 COMP-3.
           05 FD-RAT-TOT-FUND OCCURS 40 TIMES.
              10 FD-RAT-COD-FUND     PIC 9(005)     COMP-3.
              10 FD-RAT-VAL-FUND     PIC S9(012)V99 COMP-3.
              10 FD-RAT-TOT-EXT-FUND PIC 9(012)     COMP-3.

      *---------------------------------------------------------------*
      *    INPUT :    ARQUIVO NOMEFUN1                                *
      *---------------------------------------------------------------*

       FD  NOMEFUN1
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-NOME-FUNDOS2.
           05  FD-FUND-CODIGO           PIC  9(05) COMP-3.
           05  FD-FUND-NOME             PIC  X(40).
           05  FD-FUND-PRAZO            PIC  9(02) COMP-3.
           05  FD-FUND-SIGLA            PIC  X(05).
           05  FD-FUND-NOME-COMPL       PIC  X(80).
           05  FD-FUND-FLAG-MSG         PIC  9(02).
           05  FD-FUND-FILI-FUNDO       PIC  9(05) COMP-3.
           05  FD-FUND-CTRL-FUNDO       PIC  X(02).
           05  FD-FUND-CNPJ-ADM         PIC  9(09) COMP-3.
           05  FD-FUND-FILI-ADM         PIC  9(05) COMP-3.
           05  FD-FUND-CTRL-ADM         PIC  X(02).
           05  FD-FUND-NOME-ADM         PIC  X(60).
           05  FD-FUND-ENDE-ADM         PIC  X(60).
           05  FD-FUND-NOME-CENTRAL     PIC  X(60).
           05  FD-FUND-FONE-CENTRAL     PIC  X(15).
           05  FD-FUND-ENDE-CENTRAL     PIC  X(60).
           05  FD-FUND-MAIL-CENTRAL     PIC  X(60).
           05  FD-FUND-RENTABIL     PIC S9(08)V9(07) COMP-3.
           05  FD-FUND-COTA-ANT     PIC S9(08)V9(07) COMP-3.
           05  FD-FUND-COTA-FECHAM  PIC S9(08)V9(07) COMP-3.
           05  FD-FUND-DT-COTA-ANT      PIC  X(10).
           05  FD-FUND-DT-COTA-FECHAM   PIC  X(10).
           05  FD-FUND-MENSAGEM-EXTRATO PIC  X(250).
           05  FD-FUND-FILI-FUNDO-ST    PIC  X(04).
           05  FD-FUND-CTRL-FUNDO-ST    PIC  X(02).
           05  FD-FUND-CNPJ-ADM-ST      PIC  X(09).
           05  FD-FUND-FILI-ADM-ST      PIC  X(04).
           05  FD-FUND-CTRL-ADM-ST      PIC  X(02).

      *---------------------------------------------------------------*
      *    OUTPUT:    ARQUIVO FACATU                                  *
      *---------------------------------------------------------------*
       FD  FACATU
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01 FD-REG-FACATU.
          05 FD-ATU-NRO-FAC              PIC  9(003).
          05 FD-ATU-PRI-OBJ-1000-1999    PIC  9(007) COMP-3.
          05 FD-ATU-ULT-OBJ-1000-1999    PIC  9(007) COMP-3.
          05 FD-ATU-QTD-OBJ-1000-1999    PIC  9(007) COMP-3.
          05 FD-ATU-PRI-OBJ-2000-2999    PIC  9(007) COMP-3.
          05 FD-ATU-ULT-OBJ-2000-2999    PIC  9(007) COMP-3.
          05 FD-ATU-QTD-OBJ-2000-2999    PIC  9(007) COMP-3.
          05 FD-ATU-PRI-OBJ-3000-3999    PIC  9(007) COMP-3.
          05 FD-ATU-ULT-OBJ-3000-3999    PIC  9(007) COMP-3.
          05 FD-ATU-QTD-OBJ-3000-3999    PIC  9(007) COMP-3.
          05 FD-ATU-PRI-OBJ-4000-4999    PIC  9(007) COMP-3.
          05 FD-ATU-ULT-OBJ-4000-4999    PIC  9(007) COMP-3.
          05 FD-ATU-QTD-OBJ-4000-4999    PIC  9(007) COMP-3.
          05 FD-ATU-PRI-OBJ-5000-5999    PIC  9(007) COMP-3.
          05 FD-ATU-ULT-OBJ-5000-5999    PIC  9(007) COMP-3.
          05 FD-ATU-PRI-OBJ-6000-9999    PIC  9(007) COMP-3.
          05 FD-ATU-ULT-OBJ-6000-9999    PIC  9(007) COMP-3.
          05 FD-ATU-PRI-AMR-6000-9999    PIC  9(007) COMP-3.
          05 FD-ATU-ULT-AMR-6000-9999    PIC  9(007) COMP-3.
          05 FD-ATU-QTD-OBJ-6000-9999    PIC  9(007) COMP-3.
          05 FD-ATU-QTD-AMR-6000-9999    PIC  9(007) COMP-3.
          05 FD-ATU-PRI-OBJ-10000-99999  PIC  9(007) COMP-3.
          05 FD-ATU-ULT-OBJ-10000-99999  PIC  9(007) COMP-3.
          05 FD-ATU-PRI-AMR-10000-99999  PIC  9(007) COMP-3.
          05 FD-ATU-ULT-AMR-10000-99999  PIC  9(007) COMP-3.
          05 FD-ATU-QTD-OBJ-10000-99999  PIC  9(007) COMP-3.
          05 FD-ATU-QTD-AMR-10000-99999  PIC  9(007) COMP-3.
          05 FD-ATU-TOTAL-AMARRADOS      PIC  9(007) COMP-3.
          05 FD-ATU-TOTAL-OBJETOS        PIC  9(007) COMP-3.
          05 FD-ATU-VALOR-FRANQUIA     PIC 9(013)V99 COMP-3.
          05 FD-ATU-FATURAMENTO        PIC 9(015)V99 COMP-3.
          05 FD-ATU-DATA-GRAVACAO        PIC  X(010).
          05 FD-ATU-HORA-GRAVACAO        PIC  X(006).
          05 FD-ATU-RESERVA              PIC  X(018).

      *---------------------------------------------------------------*
      *---------------------------------------------------------------*

       FD  RELFAC
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-RELFAC                   PIC X(090).


      *---------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *---------------------------------------------------------------*

       77  FILLER                      PIC X(32)        VALUE
                   '* INICIO DA WORKING GFCT2935 *'.

       77  WRK-ABEND                   PIC S9(04) COMP  VALUE +1111.

      *---------------------------------------------------------------*
      *          AREAS AUXILIARES PARA VERIFICACAO DOS FILE-STATUS    *

       77  WRK-FS-FACANT               PIC X(02)        VALUE  SPACES.
       77  WRK-FS-ARQDATA             PIC X(02)        VALUE  SPACES.
       77  WRK-FS-FRAGCO01             PIC X(02)        VALUE  SPACES.
       77  WRK-FS-RATEIDEP             PIC X(02)        VALUE  SPACES.
       77  WRK-FS-NOMEFUN1             PIC X(02)        VALUE  SPACES.
       77  WRK-FS-FACATU               PIC X(02)        VALUE  SPACES.
       77  WRK-FS-RELFAC               PIC X(02)        VALUE  SPACES.
       77  WRK-OPERACAO                PIC X(13) VALUE  SPACES.
       77  WRK-ABERTURA                PIC X(13) VALUE  'NA ABERTURA'.
       77  WRK-LEITURA                 PIC X(13) VALUE  'NA LEITURA'.
       77  WRK-GRAVACAO                PIC X(13) VALUE  'NA GRAVACAO'.
       77  WRK-FECHAMENTO              PIC X(13) VALUE  'NO FECHAMENTO'.

      *---------------------------------------------------------------*
      *          AREAS AUXILIARES PARA CEP                            *
      *---------------------------------------------------------------*

       01  WRK-CEP                     PIC 9(09) VALUE ZEROS.
       01  WRK-CEP-R  REDEFINES WRK-CEP.
           03  WRK-NRO-CEP             PIC 9(06).
      *---------------------------------------------------------------*
      *          AREAS AUXILIARES PARA EDICAO                         *
      *---------------------------------------------------------------*

       77  WRK-EDIT-01                 PIC ZZZ.ZZZ.ZZ9 BLANK WHEN ZEROS
                                                       VALUE ZEROS.
       77  WRK-EDIT-02                 PIC ZZZ.ZZZ.ZZ9 BLANK WHEN ZEROS
                                                       VALUE ZEROS.
       77  WRK-EDIT-03                 PIC ZZZ.ZZZ.ZZ9 BLANK WHEN ZEROS
                                                       VALUE ZEROS.
       77  WRK-EDIT-04                 PIC ZZZ.ZZZ.ZZ9 BLANK WHEN ZEROS
                                                       VALUE ZEROS.

      *---------------------------------------------------------------*
      *          AREAS AUXILIARES PARA ACUMULADORES                   *
      *---------------------------------------------------------------*

       77  WRK-ACU-OBJETOS             PIC 9(07) COMP-3 VALUE  ZEROS.
       77  WRK-ACU-LIDOS-EXT           PIC 9(09) COMP-3 VALUE  ZEROS.
       77  WRK-ACU-LIDOS-FAC           PIC 9(09) COMP-3 VALUE  ZEROS.
       77  WRK-ACU-LIDOS-RAT           PIC 9(09) COMP-3 VALUE  ZEROS.

      *---------------------------------------------------------------*
      *          AREAS AUXILIARES DIVERSAS                            *
      *---------------------------------------------------------------*

       01  WRK-ATU-NRO-FAC             PIC 9(003) COMP-3 VALUE ZEROS.
       01  WRK-FAIXA                   PIC 9(001)  VALUE 9.
       01  WRK-FAIXA-ANT               PIC 9(001)  VALUE ZEROS.
       01  WRK-RESTO                   PIC 9(003) COMP-3 VALUE ZEROS.

      *---------------------------------------------------------------*
      * AREAS PARA CHAMADA DA POOL7600                                *
      *---------------------------------------------------------------*

       01  WRK-POOL7600.
           03  WRK-DT-JULIANA        PIC 9(05) COMP-3 VALUE ZEROS.
           03  WRK-DT-AAMMDD         PIC 9(07) COMP-3 VALUE ZEROS.
           03  WRK-DT-AAAAMMDD       PIC 9(08) COMP-3 VALUE ZEROS.
           03  WRK-TI-HHMMSS         PIC 9(06) COMP-3 VALUE ZEROS.
           03  WRK-TI-HHMMSSMMMMMM   PIC 9(13) COMP-3 VALUE ZEROS.
           03  WRK-TIMESTAMP         PIC X(20)        VALUE SPACES.
      *---------------------------------------------------------------*
      *          AREA  DA  POOL0160                                   *
      *---------------------------------------------------------------*

       01  FILLER                      PIC  X(32)          VALUE
           '*      AREA DA POOL0160       *'.

       01  WRK-NOMEJOB                 PIC X(08)           VALUE SPACES.
       01  WRK-VALORFAC                PIC 9(05)V99 COMP-3 VALUE ZEROS.

      *---------------------------------------------------------------*
      *          AREAS AUXILIARES PARA DATA                           *
      *---------------------------------------------------------------*

       01  WRK-DATA-AUX.
           03  WRK-ANO-AUX                 PIC 9(04) VALUE ZEROS.
           03  WRK-MES-AUX                 PIC 9(02) VALUE ZEROS.
           03  WRK-DIA-AUX                 PIC 9(02) VALUE ZEROS.
       01  WRK-DATA-AUX-R REDEFINES WRK-DATA-AUX PIC 9(08).

       01  WRK-AREA-POOL1285.
           03 WRK-NUMERO-DIAS       PIC S9(05) COMP-3 VALUE ZEROS.
           03 WRK-DATA-SAIDA        PIC 9(08)  COMP-3 VALUE ZEROS.
           03 WRK-MENSAGEM          PIC X(50)         VALUE SPACES.

       01  WRK-DATA-CALCULADA       PIC 9(08) VALUE ZEROS.
       01  FILLER     REDEFINES WRK-DATA-CALCULADA.
           03 WRK-ANO-CALCULADO     PIC 9(04).
           03 WRK-MES-CALCULADO     PIC 9(02).
           03 WRK-DIA-CALCULADO     PIC 9(02).

       01  WRK-DATA                    PIC 9(008) VALUE ZEROS.
       01  WRK-DATA-R                  REDEFINES      WRK-DATA.
           03  WRK-AAAA                PIC 9(004).
           03  WRK-MM                  PIC 9(002).
           03  WRK-DD                  PIC 9(002).

       01  WRK-DATA-TECL               PIC 9(008) VALUE ZEROS.
       01  FILLER                      REDEFINES      WRK-DATA-TECL.
           03  WRK-DIA-TECL            PIC 9(002).
           03  WRK-MES-TECL            PIC 9(002).
           03  WRK-ANO-TECL            PIC 9(004).
       01  WRK-DATA-EDITADA.
           03  WRK-DD                  PIC 9(002) VALUE ZEROS.
           03  FILLER                  PIC X(001) VALUE '/'.
           03  WRK-MM                  PIC 9(002) VALUE ZEROS.
           03  FILLER                  PIC X(001) VALUE '/'.
           03  WRK-AAAA                PIC 9(004) VALUE ZEROS.

       01  WRK-DATA-ARQ.
           03  WRK-DD-ARQ              PIC 9(002) VALUE ZEROS.
           03  FILLER                  PIC X(001) VALUE '/'.
           03  WRK-MM-ARQ              PIC 9(002) VALUE ZEROS.
           03  FILLER                  PIC X(001) VALUE '/'.
           03  WRK-AAAA-ARQ            PIC 9(004) VALUE ZEROS.

       01  WRK-AAAAMM-SIST.
           03  WRK-AAAA-SIST           PIC 9(004) VALUE ZEROS.
           03  WRK-MM-SIST             PIC 9(002) VALUE ZEROS.

       01  WRK-AAAAMM-ARQ.
           03  WRK-AAAA-ARQ            PIC 9(004) VALUE ZEROS.
           03  WRK-MM-ARQ              PIC 9(002) VALUE ZEROS.
      *---------------------------------------------------------------*
      *          AREAS AUXILIARES PARA HORA                           *
      *---------------------------------------------------------------*

       01  WRK-HORA                    PIC 9(006) VALUE ZEROS.
       01  WRK-HORA-R                  REDEFINES      WRK-HORA.
           03  WRK-HOR                 PIC 9(002).
           03  WRK-MIN                 PIC 9(002).
           03  WRK-SEG                 PIC 9(002).

       01  WRK-HORA-EDITADA.
           03  WRK-HOR                 PIC 9(002) VALUE ZEROS.
           03  FILLER                  PIC X(001) VALUE ':'.
           03  WRK-MIN                 PIC 9(002) VALUE ZEROS.

      *---------------------------------------------------------------*
      *  LINHAS DJDE PARA IMPRESSAO DO FAC NA IMPRESSORA XEROX        *
      *---------------------------------------------------------------*

       01  WRK-LIN-DJDE-INICIO.
           03  FILLER                  PIC X(89) VALUE
           03  FILLER                  PIC 9(01) VALUE 1.

       01  WRK-LIN-DJDE-FINAL.
           03  FILLER                  PIC X(89) VALUE
               '1DJDE JDL=DFAULT,JDE=F163S,END;'.
           03  FILLER                  PIC 9(01) VALUE 1.

      *---------------------------------------------------------------*
      *  LINHAS PARA IMPRESSAO DO FAC                                 *
      *---------------------------------------------------------------*

       01  WRK-LINHA-FAC-01.
           03  WRK-L01-CARRO           PIC X(01) VALUE '1'.
           03  FILLER                  PIC X(88) VALUE SPACES.
           03  WRK-L01-FONTINDEX       PIC X(01) VALUE '1'.

       01  WRK-LINHA-FAC-02.
           03  WRK-L02-CARRO           PIC X(01) VALUE '-'.
           03  FILLER                  PIC X(09) VALUE SPACES.
           03  WRK-L02-NOMEJOB         PIC X(08) VALUE SPACES.
           03  FILLER                  PIC X(48) VALUE SPACES.
               05  WRK-L02-DIA-EMIS    PIC 9(02) VALUE ZEROS.
               05  FILLER              PIC X(01) VALUE '/'.
               05  WRK-L02-MES-EMIS    PIC 9(02) VALUE ZEROS.
               05  FILLER              PIC X(01) VALUE '/'.
               05  WRK-L02-ANO-EMIS    PIC 9(04) VALUE ZEROS.
           03  FILLER                  PIC X(13) VALUE SPACES.
           03  WRK-L02-FONTINDEX       PIC X(01) VALUE '1'.

       01  WRK-LINHA-FAC-03.
           03  WRK-L03-CARRO           PIC X(01) VALUE '2'.
           03  FILLER                  PIC X(61) VALUE SPACES.
           03  WRK-L03-NUMERO-FAC      PIC ZZ9   BLANK WHEN ZEROS
                                                 VALUE ZEROS.
           03  FILLER                  PIC X(24) VALUE SPACES.
           03  WRK-L03-FONTINDEX       PIC X(01) VALUE '1'.

       01  WRK-LINHA-FAC-04.
           03  WRK-L04-CARRO           PIC X(01) VALUE '0'.
           03  FILLER                  PIC X(32) VALUE SPACES.
           03  FILLER                  PIC X(56) VALUE
               'BANCO BRADESCO S/A - 60.746.948/0001-12'.

       01  WRK-LINHA-FAC-05.
           03  WRK-L05-CARRO           PIC X(01) VALUE ' '.
           03  FILLER                  PIC X(38) VALUE SPACES.
           03  FILLER                  PIC X(50) VALUE
               '4160/0 - GERENCIA GERAL'.
           03  WRK-L05-FONTINDEX       PIC X(01) VALUE '2'.

       01  WRK-LINHA-FAC-06.
           03  WRK-L06-CARRO           PIC X(01) VALUE ' '.
           03  FILLER                  PIC X(48) VALUE SPACES.
           03  FILLER                  PIC X(40) VALUE
               'C/C - 1'.
           03  WRK-L06-FONTINDEX       PIC X(01) VALUE '2'.

       01  WRK-LINHA-FAC-07.
           03  WRK-L07-CARRO           PIC X(01) VALUE ' '.
           03  FILLER                  PIC X(13) VALUE SPACES.
           03  WRK-L07-FAIXA-INIC      PIC 9(05) VALUE ZEROS.
           03  FILLER                  PIC X(03) VALUE SPACES.
           03  WRK-L07-FAIXA-FIM       PIC 9(05) VALUE ZEROS.
           03  WRK-L07-OBJ-INIC        PIC ZZZZZZ9 BLANK WHEN ZEROS
                                                   VALUE ZEROS.
           03  FILLER                  PIC X(04) VALUE SPACES.
           03  WRK-L07-OBJ-FIM         PIC ZZZZZZ9 BLANK WHEN ZEROS
                                                   VALUE ZEROS.
           03  FILLER                  PIC X(01) VALUE SPACES.
           03  WRK-L07-AMAR-INIC       PIC ZZZZZZ9 BLANK WHEN ZEROS
                                                  VALUE ZEROS.
           03  FILLER                  PIC X(01) VALUE SPACES.
           03  WRK-L07-AMAR-FIM        PIC ZZZZZZ9 BLANK WHEN ZEROS
                                                   VALUE ZEROS.
           03  FILLER                  PIC X(04) VALUE SPACES.
           03  WRK-L07-QTD-OBJ         PIC ZZZZZZ9 BLANK WHEN ZEROS
                                                   VALUE ZEROS.
           03  FILLER                  PIC X(06) VALUE SPACES.
           03  WRK-L07-QTD-AMAR        PIC ZZZZZZ9 BLANK WHEN ZEROS
                                                   VALUE ZEROS.
           03  WRK-L07-FONTINDEX       PIC X(01) VALUE '2'.

       01  WRK-LINHA-FAC-08.
           03  WRK-L08-CARRO           PIC X(01) VALUE '4'.
           03  WRK-L08-TOT-AMAR        PIC ZZZZZZ9 BLANK WHEN ZEROS
                                                   VALUE ZEROS.
           03  FILLER                  PIC X(51) VALUE SPACES.
           03  WRK-L08-FONTINDEX       PIC X(01) VALUE '1'.

       01  WRK-LINHA-FAC-09.
           03  WRK-L09-CARRO           PIC X(01) VALUE '0'.
           03  FILLER                  PIC X(28) VALUE SPACES.
           03  WRK-L09-TOT-OBJ         PIC Z.ZZZ.ZZ9 BLANK WHEN ZEROS
                                                     VALUE ZEROS.
           03  FILLER                  PIC X(51) VALUE SPACES.
           03  WRK-L09-FONTINDEX       PIC X(01) VALUE '1'.

       01  WRK-LINHA-FAC-10.
           03  WRK-L10-CARRO           PIC X(01) VALUE '0'.
           03  FILLER                  PIC X(34) VALUE SPACES.
           03  WRK-L10-VLR-FRANQUIA    PIC Z.ZZZ.ZZZ.ZZZ.ZZ9,99
                                                      BLANK WHEN ZEROS
                                                      VALUE ZEROS.
           03  FILLER                  PIC X(34) VALUE SPACES.
           03  WRK-L10-FONTINDEX       PIC X(01) VALUE '1'.
       01  WRK-LINHA-FAC-11.
           03  WRK-L11-CARRO           PIC X(01) VALUE '0'.
           03  FILLER                  PIC X(33) VALUE SPACES.
           03  WRK-L11-FATURAMENTO     PIC ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99
                                                        BLANK WHEN ZEROS
                                                        VALUE ZEROS.
           03  FILLER                  PIC X(33) VALUE SPACES.
           03  WRK-L11-FONTINDEX       PIC X(01) VALUE '1'.

      *---------------------------------------------------------------*
      *  LINHAS PARA IMPRESSAO DO RELATORIO DE RATEIO                 *
      *---------------------------------------------------------------*

       01  WRK-LINHA-RAT-01.
           03  FILLER                  PIC X(07) VALUE '0'.
           03  FILLER                  PIC X(14) VALUE
               'NR. FAC'.
           03  FILLER                  PIC X(06) VALUE '01'.
           03  FILLER                  PIC X(05) VALUE '-'.
           03  FILLER                  PIC X(15) VALUE
               'DT.FRANQUIA:'.
           03  FILLER                  PIC X(32) VALUE SPACES.
           03  FILLER                  PIC X(01) VALUE '3'.

       01  FILLER                      PIC X(32)        VALUE
           '*  FIM DA WORKING GFCT2935 *'.

      *===============================================================*
       PROCEDURE DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       0000-INICIO-PROCESSAMENTO SECTION.
      *---------------------------------------------------------------*

           PERFORM 1000-INICIO.

           PERFORM 2000-PROCESSA-FAC.

           PERFORM 4000-FINALIZAR.

      *---------------------------------------------------------------*
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1000-INICIO SECTION.
      *---------------------------------------------------------------*

           OPEN INPUT   FACANT
                        FRAGCO01
                        RATEIDEP
                        NOMEFUN1
                        ARQDATA
                OUTPUT  FACATU
                        RELFAC.

           MOVE    WRK-ABERTURA            TO   WRK-OPERACAO.
           PERFORM 1100-TESTAR-FILE-STATUS.

           PERFORM 1200-TESTA-FRAGCO01-VAZIO.

           PERFORM 1210-LER-FRAGCO01  UNTIL
                D9-OUT-ASSUNTO        EQUAL  1000   OR

           PERFORM 1300-TESTA-RATEIDEP-VAZIO.

           PERFORM 1400-TESTA-NOMEFUN1-VAZIO.

           PERFORM 1500-LER-FACANT.

           CALL 'POOL7600'     USING    WRK-POOL7600.
           MOVE WRK-DT-AAAAMMDD   TO    WRK-DATA
                                        WRK-DATA-AUX-R.
           MOVE CORR WRK-DATA-R   TO    WRK-DATA-EDITADA
           MOVE WRK-AAAA OF WRK-DATA-R
                                  TO    WRK-AAAA-SIST.
           MOVE WRK-MM   OF WRK-DATA-R
                                  TO    WRK-MM-SIST.

           MOVE WRK-TI-HHMMSS     TO    WRK-HORA.
           MOVE CORR WRK-HORA-R   TO    WRK-HORA-EDITADA.

           PERFORM 1550-LER-ARQDATA.

               PERFORM 1600-CALCULA-DATA
               MOVE WRK-DIA-CALCULADO TO    WRK-L02-DIA-EMIS
               MOVE WRK-MES-CALCULADO TO    WRK-L02-MES-EMIS
               MOVE WRK-ANO-CALCULADO TO    WRK-L02-ANO-EMIS
               MOVE WRK-L02-DATA-EMIS TO    WRK-LR01-DT-FRANQUIA
           ELSE
               MOVE DATA-FRANQUIA     TO    WRK-DATA-TECL
               MOVE WRK-DIA-TECL      TO    WRK-L02-DIA-EMIS
               MOVE WRK-MES-TECL      TO    WRK-L02-MES-EMIS
               MOVE WRK-ANO-TECL      TO    WRK-L02-ANO-EMIS
               MOVE WRK-L02-DATA-EMIS TO    WRK-LR01-DT-FRANQUIA
           END-IF.

           PERFORM 1700-INICIALIZAR-FACATU.

      ***  GRAVANDO A LINHA DE DJDE

           MOVE WRK-GRAVACAO      TO    WRK-OPERACAO.

           WRITE   FD-REG-RELFAC  FROM  WRK-LIN-DJDE-INICIO.
           PERFORM 1140-TESTAR-FS-RELFAC.
      *---------------------------------------------------------------*
       1000-99-FIM.  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS SECTION.
      *---------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-FACANT.

           PERFORM 1120-TESTAR-FS-FRAGCO01.

           PERFORM 1130-TESTAR-FS-FACATU.

           PERFORM 1140-TESTAR-FS-RELFAC.

           PERFORM 1150-TESTAR-FS-RATEIDEP.

           PERFORM 1160-TESTAR-FS-NOMEFUN1.

           PERFORM 1170-TESTAR-FS-ARQDATA.
      *---------------------------------------------------------------*
       1100-99-FIM.  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1110-TESTAR-FS-FACANT   SECTION.
      *---------------------------------------------------------------*

           IF WRK-FS-FACANT   NOT EQUAL '00'
              DISPLAY '************** GFCT2935 *************'
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
              DISPLAY '*             FACANT                *'
              DISPLAY '*         FILE STATUS =  ' WRK-FS-FACANT
                                                 '         *'
              DISPLAY '************** GFCT2935 *************'
              CALL 'ILBOABN0'     USING WRK-ABEND
           END-IF.

      *---------------------------------------------------------------*
       1110-99-FIM.  EXIT.
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
       1120-TESTAR-FS-FRAGCO01 SECTION.
      *---------------------------------------------------------------*

           IF WRK-FS-FRAGCO01 NOT EQUAL '00' AND '04'
              DISPLAY '************** GFCT2935 *************'
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
              DISPLAY '*            FRAGCO01               *'
              DISPLAY '*         FILE STATUS =  ' WRK-FS-FRAGCO01
                                                 '         *'
              DISPLAY '************** GFCT2935 *************'
              CALL 'ILBOABN0'     USING WRK-ABEND
           END-IF.

      *---------------------------------------------------------------*
       1120-99-FIM.  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1130-TESTAR-FS-FACATU   SECTION.
      *---------------------------------------------------------------*
           IF WRK-FS-FACATU   NOT EQUAL '00'
              DISPLAY '************** GFCT2935 *************'
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
              DISPLAY '*            FACATU                 *'
              DISPLAY '*         FILE STATUS =  ' WRK-FS-FACATU
                                                 '         *'
              DISPLAY '************** GFCT2935 *************'
              CALL 'ILBOABN0'     USING WRK-ABEND
           END-IF.

      *---------------------------------------------------------------*
       1130-99-FIM.  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1140-TESTAR-FS-RELFAC   SECTION.
      *---------------------------------------------------------------*

           IF WRK-FS-RELFAC   NOT EQUAL '00'
              DISPLAY '************** GFCT2935 *************'
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
              DISPLAY '*         FILE STATUS =  ' WRK-FS-RELFAC
                                                 '         *'
              DISPLAY '************** GFCT2935 *************'
              CALL 'ILBOABN0'     USING WRK-ABEND
           END-IF.

      *---------------------------------------------------------------*
       1140-99-FIM.  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1150-TESTAR-FS-RATEIDEP SECTION.
      *---------------------------------------------------------------*

           IF WRK-FS-RATEIDEP NOT EQUAL '00'
              DISPLAY '************** GFCT2935 *************'
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
              DISPLAY '*            RATEIDEP               *'
              DISPLAY '*         FILE STATUS =  ' WRK-FS-RATEIDEP
                                                 '         *'
              DISPLAY '************** GFCT2935 *************'
           END-IF.

      *---------------------------------------------------------------*
       1150-99-FIM.  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1160-TESTAR-FS-NOMEFUN1 SECTION.
      *---------------------------------------------------------------*

           IF WRK-FS-NOMEFUN1 NOT EQUAL '00'
              DISPLAY '************** GFCT2935 *************'
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
              DISPLAY '*            NOMEFUN1               *'
              DISPLAY '*         FILE STATUS =  ' WRK-FS-NOMEFUN1
                                                 '         *'
              DISPLAY '************** GFCT2935 *************'
              CALL 'ILBOABN0'     USING WRK-ABEND
           END-IF.

      *---------------------------------------------------------------*
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1170-TESTAR-FS-ARQDATA SECTION.
      *---------------------------------------------------------------*

           IF WRK-FS-ARQDATA NOT EQUAL '00'
              DISPLAY '************** GFCT2935 *************'
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
              DISPLAY '*            ARQDATA               *'
              DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQDATA
                                                 '         *'
              DISPLAY '************** GFCT2935 *************'
              CALL 'ILBOABN0'     USING WRK-ABEND
           END-IF.

      *---------------------------------------------------------------*
       1170-99-FIM.  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      *---------------------------------------------------------------*

           PERFORM  1210-LER-FRAGCO01.

           IF       WRK-FS-FRAGCO01     EQUAL   '10'
                    DISPLAY '************* GFCT2935 *************'
                    DISPLAY '*                                  *'
                    DISPLAY '*         SR. OPERADOR             *'
                    DISPLAY '*  O ARQUIVO FRAGCO01 ESTA VAZIO   *'
                    DISPLAY '*  O PROCESSAMENTO SERA ENCERRADO. *'
                    DISPLAY '*                                  *'
                    DISPLAY '************** GFCT2935 ************'
                    PERFORM  4000-FINALIZAR
           END-IF.

      *---------------------------------------------------------------*
       1200-99-FIM.  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1210-LER-FRAGCO01 SECTION.

           READ    FRAGCO01.

           IF      WRK-FS-FRAGCO01         EQUAL      '10'
                   GO  TO 1210-99-FIM
           END-IF.

           MOVE    WRK-LEITURA             TO     WRK-OPERACAO.
           PERFORM 1120-TESTAR-FS-FRAGCO01.
           ADD     1                       TO     WRK-ACU-LIDOS-EXT.
           MOVE    D9-OUT-CEP              TO     WRK-CEP.

      *---------------------------------------------------------------*
       1210-99-FIM.  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1300-TESTA-RATEIDEP-VAZIO  SECTION.
      *---------------------------------------------------------------*

           PERFORM  1310-LER-RATEIDEP.
           IF       WRK-FS-RATEIDEP     EQUAL   '10'
                    DISPLAY '************* GFCT2935 *************'
                    DISPLAY '*                                  *'
                    DISPLAY '*         SR. OPERADOR             *'
                    DISPLAY '*  O ARQUIVO RATEIDEP ESTA VAZIO   *'
                    DISPLAY '*  O PROCESSAMENTO SERA ENCERRADO. *'
                    DISPLAY '*                                  *'
                    DISPLAY '************** GFCT2935 ************'
           END-IF.


      *---------------------------------------------------------------*
       1300-99-FIM.  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1310-LER-RATEIDEP SECTION.
      *---------------------------------------------------------------*

           READ    RATEIDEP.

                   GO  TO 1310-99-FIM
           END-IF.

           MOVE    WRK-LEITURA             TO     WRK-OPERACAO.
           PERFORM 1150-TESTAR-FS-RATEIDEP.
           ADD     1                       TO     WRK-ACU-LIDOS-RAT.

      *---------------------------------------------------------------*
       1310-99-FIM.  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1400-TESTA-NOMEFUN1-VAZIO  SECTION.
      *---------------------------------------------------------------*

           PERFORM  1410-LER-NOMEFUN1.

           IF       WRK-FS-NOMEFUN1     EQUAL   '10'
                    DISPLAY '************* GFCT2935 *************'
                    DISPLAY '*                                  *'
                    DISPLAY '*         SR. OPERADOR             *'
                    DISPLAY '*  O PROCESSAMENTO SERA ENCERRADO. *'
                    DISPLAY '*                                  *'
                    DISPLAY '************** GFCT2935 ************'
           END-IF.

      *---------------------------------------------------------------*
       1400-99-FIM.  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1410-LER-NOMEFUN1 SECTION.
      *---------------------------------------------------------------*

           READ    NOMEFUN1.

           IF      WRK-FS-NOMEFUN1         EQUAL      '10'
                   GO  TO 1410-99-FIM
           END-IF.

           MOVE    WRK-LEITURA             TO     WRK-OPERACAO.
           PERFORM 1160-TESTAR-FS-NOMEFUN1.
      *---------------------------------------------------------------*
       1410-99-FIM.  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1500-LER-FACANT   SECTION.
      *---------------------------------------------------------------*

           READ    FACANT.

           IF      WRK-FS-FACANT           EQUAL      '10'
                   GO  TO 1500-99-FIM
           END-IF.

           MOVE    WRK-LEITURA             TO     WRK-OPERACAO.
           PERFORM 1110-TESTAR-FS-FACANT.
           ADD     1                       TO     WRK-ACU-LIDOS-FAC.

      *---------------------------------------------------------------*
       1500-99-FIM.  EXIT.
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
       1550-LER-ARQDATA SECTION.
      *---------------------------------------------------------------*

           READ    ARQDATA.

           IF      WRK-FS-ARQDATA         EQUAL      '10'
                   GO  TO 1550-99-FIM
           END-IF.

           MOVE    WRK-LEITURA             TO     WRK-OPERACAO.
           PERFORM 1170-TESTAR-FS-ARQDATA.

      *---------------------------------------------------------------*
       1550-99-FIM.  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1600-CALCULA-DATA SECTION.
      *---------------------------------------------------------------*

           DISPLAY '*  ARQUIVO ARQDATA VAZIO.           *'.
           DISPLAY '*  ASSUMIDA DATA DO DIA +5 P/       *'.
           DISPLAY '*  DATA DA FRANQUIA.                *'.
           DISPLAY '************** GFCT2935 *************'.

           MOVE  3               TO     WRK-NUMERO-DIAS.
           MOVE  WRK-DATA-AUX-R  TO     WRK-DATA-ENTRADA.
           CALL  'POOL1285'      USING  WRK-AREA-POOL1285.
           MOVE  WRK-DATA-SAIDA  TO     WRK-DATA-CALCULADA.

      *---------------------------------------------------------------*
       1600-99-FIM.  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1700-INICIALIZAR-FACATU SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-FACANT          EQUAL  '10'
               MOVE 1                  TO    FD-ATU-NRO-FAC
           ELSE
               MOVE WRK-AAAA-ARQ       OF    WRK-DATA-ARQ
                                       TO    WRK-AAAA-ARQ
                                       OF    WRK-AAAAMM-ARQ
               MOVE WRK-MM-ARQ         OF    WRK-DATA-ARQ
                                       TO    WRK-MM-ARQ
                                       OF    WRK-AAAAMM-ARQ
               IF   WRK-AAAAMM-SIST    GREATER WRK-AAAAMM-ARQ
                    MOVE  1            TO    FD-ATU-NRO-FAC
               ELSE
                    MOVE FD-ANT-NRO-FAC   TO    FD-ATU-NRO-FAC
M2602               MOVE FD-ATU-NRO-FAC   TO    WRK-ATU-NRO-FAC
M2602               ADD  1                TO    WRK-ATU-NRO-FAC
M2602               MOVE WRK-ATU-NRO-FAC  TO    FD-ATU-NRO-FAC
               END-IF
           END-IF.

           MOVE  WRK-DATA-EDITADA      TO    FD-ATU-DATA-GRAVACAO.
           MOVE  WRK-HORA-EDITADA      TO    FD-ATU-HORA-GRAVACAO.

           CALL  'POOL0160'          USING   WRK-NOMEJOB
                                             WRK-VALORFAC.
           MOVE   WRK-VALORFAC         TO    FD-ATU-VALOR-FRANQUIA.

           MOVE  ZEROS                TO  FD-ATU-PRI-OBJ-1000-1999
                                          FD-ATU-ULT-OBJ-1000-1999
                                          FD-ATU-QTD-OBJ-1000-1999
                                          FD-ATU-PRI-OBJ-2000-2999
                                          FD-ATU-ULT-OBJ-2000-2999
                                          FD-ATU-QTD-OBJ-2000-2999
                                          FD-ATU-PRI-OBJ-3000-3999
                                          FD-ATU-ULT-OBJ-3000-3999
                                          FD-ATU-QTD-OBJ-3000-3999
                                          FD-ATU-PRI-OBJ-4000-4999
                                          FD-ATU-ULT-OBJ-4000-4999
                                          FD-ATU-QTD-OBJ-4000-4999
                                          FD-ATU-PRI-OBJ-5000-5999
                                          FD-ATU-ULT-OBJ-5000-5999
                                          FD-ATU-QTD-OBJ-5000-5999
                                          FD-ATU-PRI-OBJ-6000-9999
                                          FD-ATU-ULT-OBJ-6000-9999
                                          FD-ATU-PRI-AMR-6000-9999
                                          FD-ATU-ULT-AMR-6000-9999
                                          FD-ATU-QTD-AMR-6000-9999
                                          FD-ATU-PRI-OBJ-10000-99999
                                          FD-ATU-ULT-OBJ-10000-99999
                                          FD-ATU-PRI-AMR-10000-99999
                                          FD-ATU-ULT-AMR-10000-99999
                                          FD-ATU-QTD-OBJ-10000-99999
                                          FD-ATU-QTD-AMR-10000-99999.

      *---------------------------------------------------------------*
       1700-99-FIM.  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2000-PROCESSA-FAC SECTION.
      *---------------------------------------------------------------*

           PERFORM 2100-TOTALIZA-FAC   UNTIL  WRK-FS-FRAGCO01
                                       EQUAL  '10'.

           MOVE    WRK-FAIXA           TO     WRK-FAIXA-ANT.
           PERFORM 2200-MOVE-NR-ULT-OBJ.
           PERFORM 2300-CALCULA-AMARRADOS.

           PERFORM 2400-IMPRIMIR-FAC.

           PERFORM 2500-GRAVAR-FACATU.

      *---------------------------------------------------------------*
       2000-99-FIM.  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2100-TOTALIZA-FAC SECTION.
      *---------------------------------------------------------------*

           PERFORM 2110-VERIFICA-FAIXA.

           IF  WRK-FAIXA              EQUAL  9
      *********MOVE    OUT-ACUM-REGISTRO TO  WRK-ACU-OBJETOS
               MOVE    ZEROS             TO  WRK-ACU-OBJETOS
               PERFORM 2120-MOVE-NR-PRI-OBJ
               MOVE    WRK-FAIXA      TO     WRK-FAIXA-ANT

           IF  WRK-FAIXA          NOT EQUAL  WRK-FAIXA-ANT
               PERFORM 2200-MOVE-NR-ULT-OBJ
      *********MOVE    OUT-ACUM-REGISTRO TO  WRK-ACU-OBJETOS
               MOVE    ZEROS             TO  WRK-ACU-OBJETOS
               PERFORM 2120-MOVE-NR-PRI-OBJ
               MOVE    WRK-FAIXA      TO     WRK-FAIXA-ANT
           ELSE
               MOVE    ZEROS             TO  WRK-ACU-OBJETOS
      *********MOVE    OUT-ACUM-REGISTRO TO  WRK-ACU-OBJETOS.
           END-IF.

           PERFORM 1210-LER-FRAGCO01.

           PERFORM 1210-LER-FRAGCO01  UNTIL
                D9-OUT-ASSUNTO        EQUAL  1000   OR
                   WRK-FS-FRAGCO01    EQUAL  '10'.

      *---------------------------------------------------------------*
       2100-99-FIM.  EXIT.
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
       2110-VERIFICA-FAIXA SECTION.
      *---------------------------------------------------------------*

           EVALUATE WRK-NRO-CEP
             WHEN 1000  THRU  1999
               MOVE         1           TO    WRK-FAIXA
             WHEN 2000  THRU  2999
               MOVE         2           TO    WRK-FAIXA
             WHEN 3000  THRU  3999
               MOVE         3           TO    WRK-FAIXA
             WHEN 4000  THRU  4999
               MOVE         4           TO    WRK-FAIXA
             WHEN 5000  THRU  5999
               MOVE         5           TO    WRK-FAIXA
             WHEN 6000  THRU  9999
               MOVE         6           TO    WRK-FAIXA
             WHEN 10000 THRU  99999
               MOVE         7           TO    WRK-FAIXA
           END-EVALUATE.

       2110-99-FIM.  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2120-MOVE-NR-PRI-OBJ SECTION.
      *---------------------------------------------------------------*

           EVALUATE WRK-FAIXA
             WHEN 1
                MOVE WRK-ACU-OBJETOS   TO   FD-ATU-PRI-OBJ-1000-1999
             WHEN 2
                MOVE WRK-ACU-OBJETOS   TO   FD-ATU-PRI-OBJ-2000-2999
             WHEN 3
                MOVE WRK-ACU-OBJETOS   TO   FD-ATU-PRI-OBJ-3000-3999
             WHEN 4
                MOVE WRK-ACU-OBJETOS   TO   FD-ATU-PRI-OBJ-4000-4999
             WHEN 5
                MOVE WRK-ACU-OBJETOS   TO   FD-ATU-PRI-OBJ-5000-5999
             WHEN 6
                MOVE WRK-ACU-OBJETOS   TO   FD-ATU-PRI-OBJ-6000-9999
             WHEN 7
           END-EVALUATE.

      *---------------------------------------------------------------*
       2120-99-FIM.  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2200-MOVE-NR-ULT-OBJ SECTION.
      *---------------------------------------------------------------*

           EVALUATE WRK-FAIXA-ANT
             WHEN 1
                MOVE WRK-ACU-OBJETOS   TO       FD-ATU-ULT-OBJ-1000-1999
                COMPUTE FD-ATU-QTD-OBJ-1000-1999 = 1 +
                     FD-ATU-ULT-OBJ-1000-1999 - FD-ATU-PRI-OBJ-1000-1999
             WHEN 2
                MOVE WRK-ACU-OBJETOS   TO       FD-ATU-ULT-OBJ-2000-2999
                COMPUTE FD-ATU-QTD-OBJ-2000-2999 = 1 +
                     FD-ATU-ULT-OBJ-2000-2999 - FD-ATU-PRI-OBJ-2000-2999
             WHEN 3
                MOVE WRK-ACU-OBJETOS   TO       FD-ATU-ULT-OBJ-3000-3999
                     FD-ATU-ULT-OBJ-3000-3999 - FD-ATU-PRI-OBJ-3000-3999
             WHEN 4
                MOVE WRK-ACU-OBJETOS   TO       FD-ATU-ULT-OBJ-4000-4999
                COMPUTE FD-ATU-QTD-OBJ-4000-4999 = 1 +
                     FD-ATU-ULT-OBJ-4000-4999 - FD-ATU-PRI-OBJ-4000-4999
             WHEN 5
                MOVE WRK-ACU-OBJETOS   TO       FD-ATU-ULT-OBJ-5000-5999
                COMPUTE FD-ATU-QTD-OBJ-5000-5999 = 1 +
                     FD-ATU-ULT-OBJ-5000-5999 - FD-ATU-PRI-OBJ-5000-5999
             WHEN 6
                MOVE WRK-ACU-OBJETOS   TO       FD-ATU-ULT-OBJ-6000-9999
                COMPUTE FD-ATU-QTD-OBJ-6000-9999 = 1 +
                     FD-ATU-ULT-OBJ-6000-9999 - FD-ATU-PRI-OBJ-6000-9999
             WHEN 7
                MOVE WRK-ACU-OBJETOS   TO     FD-ATU-ULT-OBJ-10000-99999
                COMPUTE FD-ATU-QTD-OBJ-10000-99999 = 1 +
                        FD-ATU-ULT-OBJ-10000-99999 -
                        FD-ATU-PRI-OBJ-10000-99999
           END-EVALUATE.

      *---------------------------------------------------------------*
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2300-CALCULA-AMARRADOS SECTION.
      *---------------------------------------------------------------*

           IF  FD-ATU-QTD-OBJ-6000-9999   GREATER  ZEROS
               MOVE 1                  TO      FD-ATU-PRI-AMR-6000-9999
               DIVIDE FD-ATU-QTD-OBJ-6000-9999     BY    350
                                       GIVING  FD-ATU-ULT-AMR-6000-9999
                                       REMAINDER WRK-RESTO
               IF  WRK-RESTO           GREATER  ZEROS
                   ADD 1               TO      FD-ATU-ULT-AMR-6000-9999
                   MOVE FD-ATU-ULT-AMR-6000-9999
                                       TO      FD-ATU-QTD-AMR-6000-9999
               ELSE
                   MOVE 1              TO      FD-ATU-PRI-AMR-6000-9999
                   MOVE FD-ATU-ULT-AMR-6000-9999
                                       TO      FD-ATU-QTD-AMR-6000-9999
               END-IF
           END-IF.
           IF  FD-ATU-QTD-OBJ-10000-99999 GREATER  ZEROS
             MOVE FD-ATU-ULT-AMR-6000-9999 TO FD-ATU-PRI-AMR-10000-99999
             ADD  1                        TO FD-ATU-PRI-AMR-10000-99999
             DIVIDE FD-ATU-QTD-OBJ-10000-99999   BY    350
                                       GIVING FD-ATU-ULT-AMR-10000-99999
                                       REMAINDER WRK-RESTO
               IF  WRK-RESTO           GREATER  ZEROS
                   ADD 1               TO     FD-ATU-ULT-AMR-10000-99999
                   MOVE FD-ATU-ULT-AMR-10000-99999
                                       TO     FD-ATU-QTD-AMR-10000-99999
                   ADD FD-ATU-ULT-AMR-6000-9999
                                       TO FD-ATU-ULT-AMR-10000-99999
               ELSE
                   MOVE FD-ATU-ULT-AMR-10000-99999
                                       TO    FD-ATU-QTD-AMR-10000-99999
                   ADD FD-ATU-ULT-AMR-6000-9999
                                       TO    FD-ATU-ULT-AMR-10000-99999
               END-IF
           END-IF.

           COMPUTE FD-ATU-TOTAL-AMARRADOS = FD-ATU-QTD-AMR-6000-9999  +

           COMPUTE FD-ATU-TOTAL-OBJETOS   = FD-ATU-QTD-OBJ-1000-1999  +
                                            FD-ATU-QTD-OBJ-2000-2999  +
                                            FD-ATU-QTD-OBJ-3000-3999  +
                                            FD-ATU-QTD-OBJ-4000-4999  +
                                            FD-ATU-QTD-OBJ-5000-5999  +
                                            FD-ATU-QTD-OBJ-6000-9999  +
                                            FD-ATU-QTD-OBJ-10000-99999.


      *---------------------------------------------------------------*
       2300-99-FIM.  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2400-IMPRIMIR-FAC SECTION.
      *---------------------------------------------------------------*

           MOVE   WRK-GRAVACAO              TO   WRK-OPERACAO.

           MOVE   WRK-NOMEJOB               TO   WRK-L02-NOMEJOB.

           WRITE  FD-REG-RELFAC             FROM WRK-LINHA-FAC-01.
           PERFORM 1140-TESTAR-FS-RELFAC.

           WRITE  FD-REG-RELFAC             FROM WRK-LINHA-FAC-02.
           PERFORM 1140-TESTAR-FS-RELFAC.

           WRITE  FD-REG-RELFAC             FROM WRK-LINHA-FAC-03.
           PERFORM 1140-TESTAR-FS-RELFAC.

           WRITE  FD-REG-RELFAC             FROM WRK-LINHA-FAC-04.
           PERFORM 1140-TESTAR-FS-RELFAC.

           WRITE  FD-REG-RELFAC             FROM WRK-LINHA-FAC-05.
           PERFORM 1140-TESTAR-FS-RELFAC.

           WRITE  FD-REG-RELFAC             FROM WRK-LINHA-FAC-06.
           PERFORM 1140-TESTAR-FS-RELFAC.

           MOVE   '3'                       TO   WRK-L07-CARRO.

           MOVE   1999                      TO   WRK-L07-FAIXA-FIM.
           MOVE   FD-ATU-PRI-OBJ-1000-1999  TO   WRK-L07-OBJ-INIC.
           MOVE   FD-ATU-ULT-OBJ-1000-1999  TO   WRK-L07-OBJ-FIM.
           MOVE   ZEROS                     TO   WRK-L07-AMAR-INIC
                                                 WRK-L07-AMAR-FIM
                                                 WRK-L07-QTD-AMAR.
           MOVE   FD-ATU-QTD-OBJ-1000-1999  TO   WRK-L07-QTD-OBJ.

           WRITE  FD-REG-RELFAC             FROM WRK-LINHA-FAC-07.
           PERFORM 1140-TESTAR-FS-RELFAC.

           MOVE   ' '                       TO   WRK-L07-CARRO.

           MOVE   2000                      TO   WRK-L07-FAIXA-INIC.
           MOVE   2999                      TO   WRK-L07-FAIXA-FIM.
           MOVE   FD-ATU-PRI-OBJ-2000-2999  TO   WRK-L07-OBJ-INIC.
           MOVE   FD-ATU-ULT-OBJ-2000-2999  TO   WRK-L07-OBJ-FIM.
           MOVE   ZEROS                     TO   WRK-L07-AMAR-INIC
                                                 WRK-L07-AMAR-FIM
                                                 WRK-L07-QTD-AMAR.
           MOVE   FD-ATU-QTD-OBJ-2000-2999  TO   WRK-L07-QTD-OBJ.
           WRITE  FD-REG-RELFAC             FROM WRK-LINHA-FAC-07.
           PERFORM 1140-TESTAR-FS-RELFAC.

           MOVE   3000                      TO   WRK-L07-FAIXA-INIC.
           MOVE   3999                      TO   WRK-L07-FAIXA-FIM.
           MOVE   FD-ATU-PRI-OBJ-3000-3999  TO   WRK-L07-OBJ-INIC.
           MOVE   FD-ATU-ULT-OBJ-3000-3999  TO   WRK-L07-OBJ-FIM.
           MOVE   ZEROS                     TO   WRK-L07-AMAR-INIC
                                                 WRK-L07-AMAR-FIM
                                                 WRK-L07-QTD-AMAR.
           MOVE   FD-ATU-QTD-OBJ-3000-3999  TO   WRK-L07-QTD-OBJ.

           WRITE  FD-REG-RELFAC             FROM WRK-LINHA-FAC-07.
           PERFORM 1140-TESTAR-FS-RELFAC.

           MOVE   4000                      TO   WRK-L07-FAIXA-INIC.
           MOVE   4999                      TO   WRK-L07-FAIXA-FIM.
           MOVE   FD-ATU-PRI-OBJ-4000-4999  TO   WRK-L07-OBJ-INIC.
           MOVE   FD-ATU-ULT-OBJ-4000-4999  TO   WRK-L07-OBJ-FIM.
           MOVE   ZEROS                     TO   WRK-L07-AMAR-INIC
                                                 WRK-L07-AMAR-FIM
           MOVE   FD-ATU-QTD-OBJ-4000-4999  TO   WRK-L07-QTD-OBJ.

           WRITE  FD-REG-RELFAC             FROM WRK-LINHA-FAC-07.
           PERFORM 1140-TESTAR-FS-RELFAC.

           MOVE   5000                      TO   WRK-L07-FAIXA-INIC.
           MOVE   5999                      TO   WRK-L07-FAIXA-FIM.
           MOVE   FD-ATU-PRI-OBJ-5000-5999  TO   WRK-L07-OBJ-INIC.
           MOVE   FD-ATU-ULT-OBJ-5000-5999  TO   WRK-L07-OBJ-FIM.
           MOVE   ZEROS                     TO   WRK-L07-AMAR-INIC
                                                 WRK-L07-AMAR-FIM
                                                 WRK-L07-QTD-AMAR.
           MOVE   FD-ATU-QTD-OBJ-5000-5999  TO   WRK-L07-QTD-OBJ.

           WRITE  FD-REG-RELFAC             FROM WRK-LINHA-FAC-07.
           PERFORM 1140-TESTAR-FS-RELFAC.

           MOVE   6000                      TO   WRK-L07-FAIXA-INIC.
           MOVE   6999                      TO   WRK-L07-FAIXA-FIM.
           MOVE   FD-ATU-PRI-OBJ-6000-9999  TO   WRK-L07-OBJ-INIC.
           MOVE   FD-ATU-ULT-OBJ-6000-9999  TO   WRK-L07-OBJ-FIM.
           MOVE   FD-ATU-ULT-AMR-6000-9999  TO   WRK-L07-AMAR-FIM.
           MOVE   FD-ATU-QTD-OBJ-6000-9999  TO   WRK-L07-QTD-OBJ.
           MOVE   FD-ATU-QTD-AMR-6000-9999  TO   WRK-L07-QTD-AMAR.

           WRITE  FD-REG-RELFAC             FROM WRK-LINHA-FAC-07.
           PERFORM 1140-TESTAR-FS-RELFAC.

           MOVE   10000                      TO  WRK-L07-FAIXA-INIC.
           MOVE   99999                      TO  WRK-L07-FAIXA-FIM.
           MOVE   FD-ATU-PRI-OBJ-10000-99999 TO  WRK-L07-OBJ-INIC.
           MOVE   FD-ATU-ULT-OBJ-10000-99999 TO  WRK-L07-OBJ-FIM.
           MOVE   FD-ATU-PRI-AMR-10000-99999 TO  WRK-L07-AMAR-INIC.
           MOVE   FD-ATU-ULT-AMR-10000-99999 TO  WRK-L07-AMAR-FIM.
           MOVE   FD-ATU-QTD-OBJ-10000-99999 TO  WRK-L07-QTD-OBJ.
           MOVE   FD-ATU-QTD-AMR-10000-99999 TO  WRK-L07-QTD-AMAR.

           WRITE  FD-REG-RELFAC              FROM WRK-LINHA-FAC-07.
           PERFORM 1140-TESTAR-FS-RELFAC.

           MOVE   FD-ATU-TOTAL-AMARRADOS     TO  WRK-L08-TOT-AMAR.
           MOVE   FD-ATU-TOTAL-OBJETOS       TO  WRK-L09-TOT-OBJ.
           COMPUTE FD-ATU-FATURAMENTO = FD-ATU-VALOR-FRANQUIA *
                                        FD-ATU-TOTAL-OBJETOS.
           MOVE   FD-ATU-FATURAMENTO         TO  WRK-L11-FATURAMENTO.

           WRITE  FD-REG-RELFAC              FROM  WRK-LINHA-FAC-08.
           PERFORM 1140-TESTAR-FS-RELFAC.

           WRITE  FD-REG-RELFAC              FROM  WRK-LINHA-FAC-09.
           PERFORM 1140-TESTAR-FS-RELFAC.

           WRITE  FD-REG-RELFAC              FROM  WRK-LINHA-FAC-10.
           PERFORM 1140-TESTAR-FS-RELFAC.

           WRITE  FD-REG-RELFAC              FROM  WRK-LINHA-FAC-11.
           PERFORM 1140-TESTAR-FS-RELFAC.

      *---------------------------------------------------------------*
       2400-99-FIM.  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      *---------------------------------------------------------------*

           MOVE  WRK-GRAVACAO         TO  WRK-OPERACAO.

           WRITE FD-REG-FACATU.
           PERFORM 1130-TESTAR-FS-FACATU.

           ADD   1                    TO  WRK-ACU-GRAVADOS.

      *---------------------------------------------------------------*
       2500-99-FIM.  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       4000-FINALIZAR SECTION.
      *---------------------------------------------------------------*

           MOVE  WRK-ACU-LIDOS-EXT     TO   WRK-EDIT-01.
           MOVE  WRK-ACU-LIDOS-FAC     TO   WRK-EDIT-02.
           MOVE  WRK-ACU-LIDOS-RAT     TO   WRK-EDIT-03.
           MOVE  WRK-ACU-GRAVADOS      TO   WRK-EDIT-04.
           DISPLAY '****************** GFCT2935 *****************'.
           DISPLAY '*  REGISTROS LIDOS FRAGCO01.: ' WRK-EDIT-01 '   *'.
           DISPLAY '*  REGISTROS LIDOS FACANT...: ' WRK-EDIT-02 '   *'.
           DISPLAY '*  REGISTROS LIDOS RATEIDEP.: ' WRK-EDIT-03 '   *'.
           DISPLAY '*  REGISTROS GRAVADOS FACATU: ' WRK-EDIT-04 '   *'.
           DISPLAY '****************** GFCT2935 *****************'.

      ***  GRAVANDO DJDE FINAL

           WRITE  FD-REG-RELFAC        FROM WRK-LIN-DJDE-FINAL.
           MOVE   WRK-GRAVACAO         TO   WRK-OPERACAO.
           PERFORM 1140-TESTAR-FS-RELFAC.

           CLOSE  FACANT
                  FRAGCO01
                  RATEIDEP
                  NOMEFUN1
                  ARQDATA
                  FACATU
                  RELFAC.

           PERFORM 1100-TESTAR-FILE-STATUS.

           STOP RUN.

      *---------------------------------------------------------------*
       4000-99-FIM.  EXIT.
      *---------------------------------------------------------------*



