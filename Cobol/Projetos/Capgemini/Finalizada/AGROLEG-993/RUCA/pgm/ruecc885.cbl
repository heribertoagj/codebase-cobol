      *===============================================================*
       IDENTIFICATION                  DIVISION.
      *===============================================================*

       PROGRAM-ID.    RUECC885.
       AUTHOR.        EDUARDO WATANABE.

      *===============================================================*
      *                  S O N D A   P R O C W O R K                  *
      *---------------------------------------------------------------*
      *                                                               *
      *      PROGRAMADOR  : EDUARDO WATANABE                          *
      *      ANALISTA     : EDUARDO WATANABE                          *
      *      DATA         : 04/02/2016                                *
      *                                                               *
      *      OBJETIVO     : ENVIAR COR0001A DAS LIBERACOES.           *
      *                     PROGRAMA CLONE DO RUEC0885.               *
      *                                                               *
      *      ARQUIVOS                                                 *
      *        E/S DDNAME   LAYOUT    DESCRICAO                       *
CAT408*        I   ENTDESCL --------  OPERACOES DESCLASSIFICADAS      *
      *        I   ARQDATAC I#RURACR  ARQUIVO DE DATAS DO SISTEMA RURA*
      *        O   ARQLIBER --------  ARQUIVO LIBERACOES              *
CAT408*        O   SAIGPDES --------  GRUPOS DE DESCLASSIFICACAO      *
      *                                                               *
      *    BANCO DE DADOS:                                            *
      *      DB2                                                      *
      *      TABLE                                 INCLUDE/BOOK       *
      *                                                               *
      *    MODULOS CHAMADOS:                                          *
      *      BRAD0650 - TESTE DE BITS                                 *
      *      BRAD7100 - MODULO DE TRATAMENTO DE ERRO                  *
      *                                                               *
EG0616*================================================================*
EG0916*                      A L T E R A C A O                         *
EG0916*================================================================*
EG0916*    ANALISTA....:   EVANDRO GUIMARAES       - BSI TECNOLOGIA    *
EG0916*    DATA........:   SETEMBRO/2016                               *
EG0916*    OBJETIVO....:   ADAPTACAO CATALOGO 4.08                     *
EG0916*================================================================*
BR0521*                      A L T E R A C A O                         *
BR0521*    ANALISTA....:   SIMONE                  - BRQ               *
BR0521*    DATA........:   JUNHO/2021                                  *
BR0521*    OBJETIVO....:   SUBSTITUIR I#RUECC5                         *
BR0521*================================================================*

      *===============================================================*
       ENVIRONMENT                     DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *---------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *---------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *---------------------------------------------------------------*
       FILE-CONTROL.

                       FILE   STATUS  IS  WRK-FS-CADRURAL.

CAT408     SELECT  ENTDESCL   ASSIGN  TO  UT-S-ENTDESCL
CAT408                 FILE   STATUS  IS  WRK-FS-ENTDESCL.
CAT408
           SELECT  ARQDATAC   ASSIGN  TO  UT-S-ARQDATAC
                       FILE   STATUS  IS  WRK-FS-ARQDATAC.

           SELECT  ARQLIBER   ASSIGN  TO  UT-S-ARQLIBER
                       FILE   STATUS  IS  WRK-FS-ARQLIBER.

CAT408     SELECT  SAIGPDES   ASSIGN  TO  UT-S-SAIGPDES
CAT408                 FILE   STATUS  IS  WRK-FS-SAIGPDES.
CAT408
      *===============================================================*
       DATA                            DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       FILE                            SECTION.
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    CADASTRO CREDITO RURAL                                     *
      *---------------------------------------------------------------*

       FD  CADRURAL
           LABEL RECORD STANDARD
           BLOCK CONTAINS  0 RECORDS
           RECORDING F.

       01 FD-REG-CADRURAL              PIC X(350).

CAT408*---------------------------------------------------------------*
CAT408*    OPERACOES DESCLASSIFICADAS                                 *
CAT408*---------------------------------------------------------------*
CAT408
CAT408 FD  ENTDESCL
CAT408     LABEL RECORD STANDARD
CAT408     BLOCK CONTAINS  0 RECORDS
CAT408     RECORDING F.
CAT408
CAT408 01 FD-REG-ENTDESCL              PIC X(029).
      *---------------------------------------------------------------*
      *    ARQUIVO DE DATAS DO SISTEMA RURA                           *
      *---------------------------------------------------------------*

       FD  ARQDATAC
           LABEL RECORD STANDARD
           BLOCK CONTAINS  0 RECORDS
           RECORDING F.

       01 FD-REG-ARQDATAC              PIC X(210).

      *---------------------------------------------------------------*'
      *    ARQUIVO EVENTO LIBERACOES                                  *
      *---------------------------------------------------------------*

       FD  ARQLIBER
           LABEL RECORD STANDARD
           BLOCK CONTAINS  0 RECORDS
           RECORDING F.

       01  FD-ARQLIBER                 PIC X(750).
CAT408*---------------------------------------------------------------*
CAT408*    OPERACOES DESCLASSIFICADAS                                 *
CAT408*---------------------------------------------------------------*
CAT408
CAT408 FD  SAIGPDES
CAT408     LABEL RECORD STANDARD
CAT408     BLOCK CONTAINS  0 RECORDS
CAT408     RECORDING F.
CAT408
CAT408 01  FD-SAIGPDES                 PIC X(500).
CAT408
      *---------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(32) VALUE
           '*** INICIO DA WORKING-STORAGE **'.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
           '* AREA PARA TESTE DE FS        *'.
      *---------------------------------------------------------------*

       77  WRK-FS-CADRURAL             PIC  X(02) VALUE SPACES.
CAT408 77  WRK-FS-ENTDESCL             PIC  X(02) VALUE SPACES.
       77  WRK-FS-ARQDATAC             PIC  X(02) VALUE SPACES.
       77  WRK-FS-ARQLIBER             PIC  X(02) VALUE SPACES.
CAT408 77  WRK-FS-SAIGPDES             PIC  X(02) VALUE SPACES.

       77  WRK-BATCH                   PIC  X(08) VALUE 'BATCH'.
       77  WRK-FECHAMENTO              PIC  X(13) VALUE 'NO FECHAMENTO'.
       77  WRK-ABERTURA                PIC  X(13) VALUE 'NA ABERTURA'.
       77  WRK-LEITURA                 PIC  X(13) VALUE 'NA LEITURA'.
       77  WRK-GRAVACAO                PIC  X(13) VALUE 'NA GRAVACAO'.

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(32) VALUE
           '* VARIAVEIS AUXILIARES GERAIS  *'.
      *---------------------------------------------------------------*

       77  WRK-JOBNOME                 PIC X(08)           VALUE SPACES.

       77  WRK-IND                     PIC 9(03)    COMP-3 VALUE ZEROS.
       77  WRK-TEM-LIBERACAO-DIA       PIC X(01)           VALUE 'N'.
CAT408 77  WRK-GRUPO-DESCL             PIC X(330)          VALUE SPACES.
CAT408 77  WRK-POS-INI                 PIC 9(005)          VALUE ZEROS.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(32) VALUE
           '* AREAS AUXILIARES HEXA        *'.
      *---------------------------------------------------------------*
CAT408
CAT408 01  WRK-CH19                    PIC  X(19)          VALUE ZEROS.
CAT408 01  FILLER REDEFINES WRK-CH19.
CAT408     05 FILLER                   PIC  X(04).
CAT408     05 WRK-CH15                 PIC  X(15).
CAT408
CAT408 01  WRK-SZD13V2                 PIC +9(13)V99       VALUE ZEROS.
CAT408 01  FILLER                      REDEFINES WRK-SZD13V2.
CAT408     05 FILLER                   PIC  X(01).
CAT408     05 WRK-ZD13V2               PIC  9(13)V99.
CAT408     05 WRK-ZD13V2-R-CH15        REDEFINES
CAT408
       01  WRK-HEXAVISION.
           03  WRK-N13V2-COM-SINAL     PIC +9(13)V99        VALUE ZEROS.
           03  FILLER REDEFINES WRK-N13V2-COM-SINAL.
               05  FILLER              PIC  X(01).
               05  WRK-N13V2-SEM-SINAL PIC  9(13)V99.

BR0521*---------------------------------------------------------------*
.      01  FILLER                      PIC  X(32) VALUE
.          '* AREA AUXILIAR DESCLASSIFIC   *'.
.     *---------------------------------------------------------------*
.
.      01  WRK-COR0005-5.
.          05  WRK-GRUPO-COR0005-DESCLASS.
.              10  FILLER               PIC  X(006)     VALUE '{1200}'.
.              10  WRK-GP-COR0005-DESCLASS.
.                  15  WRK-DTA-DESCLASS PIC  X(008)     VALUE SPACES.
.                  15  WRK-VLR-DESCLASS PIC  X(019)     VALUE SPACES.
.                  15  WRK-MTV-DESCLASS PIC  X(003)     VALUE SPACES.
BR0521

       01  FILLER                      PIC  X(32) VALUE
           '* AREAS AUXILIARES DE DATA     *'.
      *---------------------------------------------------------------*

       01  WRK-COMPILACAO.
           03  WRK-DTCOMPILA.
               05  WRK-MMCOMPILA       PIC  X(002)         VALUE SPACES.
               05  FILLER              PIC  X(001)         VALUE SPACES.
               05  WRK-DDCOMPILA       PIC  X(002)         VALUE SPACES.
               05  FILLER              PIC  X(001)         VALUE SPACES.
               05  WRK-AACOMPILA       PIC  X(002)         VALUE SPACES.
           03  WRK-HRCOMPILA           PIC  X(008)         VALUE SPACES.

       01  WRK-DATA-ISO-N              PIC  9(009)         VALUE ZEROS.
       01  WRK-DATA-ISO    REDEFINES   WRK-DATA-ISO-N.
           03  FILLER                  PIC  X(001).
           03  WRK-ANO-ISO             PIC  X(004).
           03  WRK-MES-ISO             PIC  X(002).
           03  WRK-DIA-ISO             PIC  X(002).
CAT408 01  FILLER          REDEFINES   WRK-DATA-ISO-N.
CAT408     03  FILLER                  PIC  X(001).
CAT408      05 WRK-DATA-ISO-8N         PIC  9(008).

CAT408 01  WRK-DATA-DB2                PIC  X(010)         VALUE SPACES.
CAT408 01  FILLER                      REDEFINES WRK-DATA-DB2.
CAT408     03  WRK-DIA-DB2             PIC  X(002).
CAT408     03  FILLER                  PIC  X(001).
CAT408     03  WRK-MES-DB2             PIC  X(002).
CAT408     03  FILLER                  PIC  X(001).
CAT408     03  WRK-ANO-DB2             PIC  X(004).
CAT408
       01  WRK-DATA-EDIT.
           03  WRK-DIA-EDIT            PIC  X(002)         VALUE SPACES.
           03  FILLER                  PIC  X(001)         VALUE '/'.
           03  WRK-MES-EDIT            PIC  X(002)         VALUE SPACES.
           03  FILLER                  PIC  X(001)         VALUE '/'.
           03  WRK-ANO-EDIT            PIC  X(004)         VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(32) VALUE
           '* AREA DOS ACUMULADORES        *'.
      *---------------------------------------------------------------*
       01  ACU-LIDOS-CADRURAL          PIC  9(09)  COMP-3  VALUE ZEROS.
CAT408 01  ACU-LIDOS-ENTDESCL          PIC  9(09)  COMP-3  VALUE ZEROS.
       01  ACU-LIDOS-ARQDATAC          PIC  9(09)  COMP-3  VALUE ZEROS.
       01  ACU-GRAVA-ARQLIBER          PIC  9(09)  COMP-3  VALUE ZEROS.
CAT408 01  ACU-GRAVA-SAIGPDES          PIC  9(09)  COMP-3  VALUE ZEROS.
CAT408 01  ACU-GRAVA-SAIGPDES-VAZIO    PIC  9(09)  COMP-3  VALUE ZEROS.
CAT408 01  ACU-DESCLASSIFICACOES       PIC  9(09)  COMP-3  VALUE ZEROS.
CAT408 01  ACU-DT-MAIOR                PIC  9(09)  COMP-3  VALUE ZEROS.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(32) VALUE
           '* AREA DO ARQUIVO CADRURAL     *'.
      *---------------------------------------------------------------*

       COPY 'I#RURAJC'.

CAT408*---------------------------------------------------------------*
CAT408 01  FILLER                      PIC  X(32) VALUE
CAT408     '* AREA DO ARQUIVO ENTDESCL     *'.
CAT408*---------------------------------------------------------------*
CAT408
CAT408     03  WRK-DESCL-CCONTR        PIC  9(09)   COMP-3 VALUE ZEROS.
CAT408     03  WRK-DESCL-NMOTVO-DCLAS-CONTR
CAT408                                 PIC  9(05)   COMP-3 VALUE ZEROS.
CAT408     03  WRK-DESCL-CMOTVO-DCLAS-BACEN
CAT408                                 PIC  X(03)          VALUE SPACES.
CAT408     03  WRK-DESCL-DDCLAS-OPER-CREDT
CAT408                                 PIC  X(10)          VALUE SPACES.
CAT408     03  WRK-DESCL-VPRINC-DCLAS-CONTR
CAT408                                 PIC  9(13)V99 COMP-3 VALUE ZEROS.
CAT408
CAT408*---------------------------------------------------------------*
CAT408 01  FILLER                      PIC  X(32) VALUE
CAT408     '* AREA DO ARQUIVO SAIGPDES     *'.
CAT408*---------------------------------------------------------------*
CAT408
CAT408 01  WRK-SAIGPDES-REG.
CAT408     03  WRK-DES-CCONTR          PIC 9(009)          VALUE ZEROS.
CAT408     03  WRK-DES-VAZIO           PIC X(001)          VALUE SPACES.
CAT408     03  WRK-DES-GRUPO-DESCL     PIC X(330)          VALUE SPACES.
CAT408     03  FILLER                  PIC X(160)          VALUE SPACES.
CAT408
       01  FILLER                      PIC  X(32) VALUE
           '* AREA DO ARQUIVO DE DATAC     *'.
      *---------------------------------------------------------------*

       COPY 'I#RURACR'.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(32) VALUE
           '* AREA DO ARQUIVO LIBERACOES   *'.
      *---------------------------------------------------------------*

       01  WRK-ARQLIBER.
           05  WRK-LIB-CHAVE           PIC  9(09)           VALUE ZEROS.
           05  WRK-LIB-IND             PIC  9(03)    COMP-3 VALUE ZEROS.
           05  WRK-LIB-OCORRENCIAS  OCCURS  30 TIMES.
               10  WRK-LIB-DATA        PIC  X(08)           VALUE ZEROS.
               10  WRK-LIB-VALOR       PIC  9(13)V99        VALUE ZEROS.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(32) VALUE
           '* TAB. INTERNA PARA LIBERACOES *'.
