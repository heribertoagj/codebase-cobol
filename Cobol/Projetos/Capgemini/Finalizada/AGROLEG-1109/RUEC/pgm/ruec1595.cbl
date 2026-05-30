      *===============================================================*
       IDENTIFICATION DIVISION.
      *===============================================================*
       PROGRAM-ID.    RUEC1595.
       AUTHOR.        BSI TECNOLOGIA.

      *===============================================================*
      *                                                               *
      *      PROGRAMA     : RUEC1595                                  *
      *      PROGRAMADOR  : BSI TECNOLOGIA.                           *
      *      ANALISTA     : BSI TECNOLOGIA.                           *
      *      DATA         : 18/01/2013                                *
      *                                                               *
      *      OBJETIVO     :                                           *
      *      EXTRACAO DIARIA PARA CONFRONTO                           *
      *      EXTRACAO DIARIA PARA CONFRONTO                           *
      *      EXTRACAO DIARIA PARA CONFRONTO                           *
      *                                                               *
      *      DB2:                                                     *
      *        RUFIB002 - DB2PRD.CONTRATO_RURAL                       *
      *        RURCB028 - DB2PRD.PLANO_REFIN                          *
      *        RUCAB004 - DB2PRD.RUCA_CAD_DEP                         *
      *        RUECV016 - DB2PRD.V01PARAMETRO_RURAL                   *
      *                                                               *
EG0615*===============================================================*
EG0615*      A L T E R A C A O                                        *
EG0615*===============================================================*
EG0615*      PROGRAMADOR  : BSI TECNOLOGIA.                           *
EG0615*      ANALISTA     : BSI TECNOLOGIA.                           *
EG0615*      DATA         : JUNHO/2015                                *
EG0615*                                                               *
EG0615*      OBJETIVO     :                                           *
EG0615*      INCLUSAO DO CAMPO CODIGO DE SUBPROGRAMA                  *
EG0615*                                                               *
EG0615*===============================================================*
PRI499*                ALTERACAO - PRIME                               *
PRI499*----------------------------------------------------------------*
PRI499*                                                                *
PRI499* ANALISTA REPONSAVEL: WALTER MESSAS                             *
PRI499* DATA...............: 24/11/2022                                *
PRI499* OBJETIVOS..........: INCLUSAO DE TIPO DE CLIMA NO CODIGO       *
PRI499*================================================================*

      *===============================================================*
       ENVIRONMENT DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       CONFIGURATION SECTION.
      *---------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.


      *---------------------------------------------------------------*
       INPUT-OUTPUT SECTION.
      *---------------------------------------------------------------*

       FILE-CONTROL.

           SELECT  ARQDATA   ASSIGN  TO  UT-S-ARQDATA

           SELECT  ARQACUMU  ASSIGN  TO  UT-S-ARQACUMU
                   FILE      STATUS  IS  WRK-FS-ARQACUMU.

      *===============================================================*
       DATA DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       FILE SECTION.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      *  INPUT....: ARQUIVO DE DATAS                                  *
      *             ORG. SEQUENCIAL    -  LRECL = 210 BYTES           *
      *---------------------------------------------------------------*

       FD  ARQDATA
           LABEL       RECORD    STANDARD
           RECORDING   MODE      F
           BLOCK       CONTAINS  0.
       01  FD-REG-ARQDATA                 PIC  X(210).

      *---------------------------------------------------------------*
      *  OUTPUT...: ARQUIVO ACUMULADO RECOR                           *
      *             ORG. SEQUENCIAL    -  LRECL = 100 BYTES           *
      *---------------------------------------------------------------*

       FD  ARQACUMU
           LABEL       RECORD    STANDARD
           RECORDING   MODE      F
           BLOCK       CONTAINS  0.

       01  FD-REG-ARQACUMU                PIC  X(100).

      *---------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      *          AREAS AUXILIARES                                     *
      *---------------------------------------------------------------*
       77  FILLER                       PIC X(32)        VALUE
           '* INICIO DA WORKING RUEC1596 *'.

       77  WRK-BATCH               PIC X(08)  VALUE  'BATCH'.

       77  WRK-MODULO              PIC X(08)  VALUE SPACES.
       77  WRK-MODULO-BRAD0015     PIC X(08)  VALUE 'BRAD0015'.
       77  WRK-MODULO-RUEC8822     PIC  X(08) VALUE 'RUEC8822'.
       77  WRK-NENVIO-RECOR        PIC S9(05) COMP-3 VALUE ZEROS.

       01  WRK-QTD-SSINAL          PIC   9(05) VALUE ZEROS.
       01  FILLER                  REDEFINES WRK-QTD-SSINAL.
           05 WRK-QTD-CSINAL       PIC  S9(05).

       01  WRK-QTD-PESQ         PIC S9(05)  COMP-3  VALUE ZEROS.

       77  WRK-SQLCA                   PIC X(136) VALUE SPACES.

       77  WRK-FIM                 PIC 9(01) VALUE ZEROS.
       88  WRK-FIM-RUFIB002    VALUE 1.


       77  WRK-IND-OCOR            PIC S9(05) COMP-3 VALUE ZEROS.

       77  WRK-IND-B004            PIC S9(04) COMP-3 VALUE ZEROS.
       77  WRK-CODE100-B004        PIC  X(01) VALUE 'N'.

       77  WRK-IND-B014            PIC S9(04) COMP-3 VALUE ZEROS.
       77  WRK-CODE100-B014        PIC  X(01) VALUE 'N'.

       77  WRK-IND-B09D            PIC S9(04) COMP-3 VALUE ZEROS.
       77  WRK-CODE100-B09D        PIC  X(01) VALUE 'N'.

       77  WRK-IND-B09G            PIC S9(04) COMP-3 VALUE ZEROS.
       77  WRK-CODE100-B09G        PIC  X(01) VALUE 'N'.

       77  WRK-TEMPROPR-B014       PIC  X(01) VALUE 'N'.
       77  WRK-SEQREG              PIC  9(05) COMP-3 VALUE ZEROS.

       77  WRK-VRECTA-BRUTA        PIC S9(15)V99 COMP-3 VALUE ZEROS.

       77  WRK-NUM-PARCELA         PIC  9(03)    COMP-3 VALUE ZEROS.
       77  WRK-NPSSOA-CONTR        PIC S9(03) COMP-3 VALUE ZEROS.

       01  WRK-VALOR                   PIC -9(13)V99       VALUE ZEROS.
       01  FILLER  REDEFINES  WRK-VALOR.
           03  FILLER                  PIC  X(01).
           03  WRK-VALOR-SS            PIC  9(13)V99.
           03  FILLER  REDEFINES  WRK-VALOR-SS.
               05  FILLER              PIC  X(10).
               05  WRK-VALOR-TX        PIC  9(03)V99.

       01  FILLER.
           05  WRK-DVCTO-CONTR99   PIC  X(10)       VALUE SPACES.
           05  FILLER  REDEFINES  WRK-DVCTO-CONTR99.
               07  WRK-DVCTO-DIA99 PIC  9(02).
               07  FILLER          PIC  X(01).
               07  WRK-DVCTO-MES99 PIC  9(02).
               07  FILLER          PIC  X(01).
               07  WRK-DVCTO-ANO99 PIC  9(04).

           05  WRK-DVCTO-CONTR02   PIC  X(10)       VALUE SPACES.
           05  FILLER  REDEFINES  WRK-DVCTO-CONTR02.
               07  FILLER          PIC  X(01).
               07  WRK-DVCTO-MES02 PIC  9(02).
               07  FILLER          PIC  X(01).
               07  WRK-DVCTO-ANO02 PIC  9(04).

           05  WRK-DVCTO99-MIN     PIC  9(08)      VALUE ZEROS.
           05  WRK-DVCTO02         PIC  9(08)      VALUE ZEROS.

       01  WRK-COD3-NUM        PIC  9(3)        VALUE ZEROS.
       01  FILLER              REDEFINES WRK-COD3-NUM.
           05 FILLER           PIC  9(1).
           05 WRK-COD3-NUM2    PIC  9(2).

       01  WRK-COD3-CHAR       PIC  X(3)        VALUE SPACES.
       01  FILLER              REDEFINES WRK-COD3-CHAR.
           05 WRK-COD3-CHAR1   PIC  X(1).
           05 FILLER           PIC  X(2).

       01  WRK-AUX3-NUM        PIC  9(3)        VALUE ZEROS.
       01  FILLER              REDEFINES WRK-AUX3-NUM.
           05 FILLER           PIC  9(2).

       01  WRK-COD5-NUM        PIC  9(5)        VALUE ZEROS.
       01  FILLER              REDEFINES WRK-COD5-NUM.
           05 FILLER           PIC  9(1).
           05 WRK-COD5-NUM4    PIC  9(4).

       01  WRK-SAFRA.
           05 WRK-DINIC-SAFRA  PIC  X(4) VALUE SPACES.
           05 WRK-DFIM-SAFRA   PIC  X(4) VALUE SPACES.

       01  WRK-COD6-NUM        PIC  9(6)        VALUE ZEROS.
       01  FILLER              REDEFINES WRK-COD6-NUM.
           05 WRK-COD6-CHAR    PIC  X(6).

       01  WRK-AUX6-CHAR       PIC  X(6)        VALUE SPACES.
       01  FILLER              REDEFINES WRK-AUX6-CHAR.
           05 FILLER           PIC  X(2).
           05 WRK-AUX6-CHAR4   PIC  X(4).

       01  WRK-COD9-NUM            PIC  9(09)  VALUE ZEROS.
       01  FILLER            REDEFINES  WRK-COD9-NUM.
           05 WRK-COD9-NUM6        PIC  9(06).

       01  WRK-DATA                PIC  X(10)  VALUE SPACES.
       01  FILLER                  REDEFINES   WRK-DATA.
           05 WRK-DATA-DIA         PIC  9(02).
           05 FILLER               PIC  X(01).
           05 WRK-DATA-MES         PIC  9(02).
           05 FILLER               PIC  X(01).
           05 WRK-DATA-ANO         PIC  9(04).

       01  WRK-CTPO-GARNT-RECOR    PIC -9(03)      VALUE ZEROS.
       01  FILLER   REDEFINES  WRK-CTPO-GARNT-RECOR.
           05  FILLER              PIC  X(02).
           05  WRK-CTPO-GARNT-X    PIC  X(02).

       01  WRK-QUND-FINCD11      PIC -9(09)V9(2)       VALUE ZEROS.
       01  FILLER   REDEFINES  WRK-QUND-FINCD11.
           05  FILLER            PIC  X(01).
           05  FILLER            PIC  9(02).
           05  WRK-QUND-FINCD9   PIC  9(07)V9(2).

       01  FILLER                 REDEFINES WRK-CONTRATU-CSINAL.
           05 WRK-CONTRATU-SSINAL PIC  9(09).

       01  WRK-VFINAN-CSINAL      PIC S9(13)V99  VALUE ZEROS.
       01  FILLER                 REDEFINES WRK-VFINAN-CSINAL.
           05 WRK-VFINAN-SSINAL   PIC  9(13)V99.

       01  WRK-VREC-PPRIO-CSINAL    PIC S9(13)V99 VALUE ZEROS.
       01  FILLER                 REDEFINES WRK-VREC-PPRIO-CSINAL.
           05 WRK-VREC-PPRIO-SSINAL PIC  9(13)V99.

       01  WRK-QUND-FINCD-CSINAL    PIC S9(09)V99  VALUE ZEROS.
       01  FILLER                 REDEFINES WRK-QUND-FINCD-CSINAL.
           05 WRK-QUND-FINCD-SSINAL PIC  9(09)V99.

       01  WRK-QUND-PROD-CSINAL     PIC S9(09)V99  VALUE ZEROS.
       01  FILLER                 REDEFINES  WRK-QUND-PROD-CSINAL.
           05 WRK-QUND-PROD-SSINAL  PIC  9(09)V99.

       01  WRK-VRECTA-CSINAL          PIC S9(15)V99  VALUE ZEROS.
       01  FILLER                 REDEFINES WRK-VRECTA-CSINAL.

       01  WRK-VPCELA-LIBRC-CSINAL    PIC S9(13)V99  VALUE ZEROS.
       01  FILLER                 REDEFINES WRK-VPCELA-LIBRC-CSINAL.
           05 WRK-VPCELA-LIBRC-SSINAL PIC  9(13)V99.

       01  WRK-VPCELA-REC-CSINAL    PIC S9(13)V99  VALUE ZEROS.
       01  FILLER                 REDEFINES WRK-VPCELA-REC-CSINAL.
           05 WRK-VPCELA-REC-SSINAL PIC  9(13)V99.

       01  WRK-NPCELA-CSINAL    PIC S9(03)     VALUE ZEROS.
       01  FILLER                 REDEFINES WRK-NPCELA-CSINAL.
           05 WRK-NPCELA-SSINAL PIC  9(03).

       01  WRK-AUXTOT-COMP         PIC    9(05) COMP-3 VALUE 999.

       01  WRK-AUXTOT              PIC    9(05)        VALUE 999.

       01  WRK-NITEM-ENV           PIC    9(05) COMP-3 VALUE ZEROS.

       01  WRK-QTDREG-B097         PIC S9(05) COMP-3 VALUE ZEROS.

       01  FILLER                  REDEFINES WRK-QTDB097-CSINAL.
           05 WRK-QTDB097          PIC    9(05).

EG0615 01  WRK-DATA-DB2                PIC  X(10)          VALUE SPACES.
EG0615 01  FILLER REDEFINES WRK-DATA-DB2.
EG0615     05 WRK-DIA-DB2              PIC  X(02).
EG0615     05 FILLER                   PIC  X(01).
EG0615     05 WRK-MES-DB2              PIC  X(02).
EG0615     05 FILLER                   PIC  X(01).
EG0615     05 WRK-ANO-DB2              PIC  X(04).
EG0615
EG0615 01  WRK-DATA-ISO                PIC  X(08)          VALUE SPACES.
EG0615 01  FILLER REDEFINES WRK-DATA-ISO.
EG0615     05 WRK-ANO-ISO              PIC  X(04).
EG0615     05 WRK-MES-ISO              PIC  X(02).
EG0615     05 WRK-DIA-ISO              PIC  X(02).
EG0615
      *----------------------------------------------------------------*
      *      AREA INPUT ARQDATA                                        *
      *----------------------------------------------------------------*


      *----------------------------------------------------------------*
      *      AREA OUTPUT ARQACUMU                                      *
      *----------------------------------------------------------------*

       COPY I#RUEC81.

      *----------------------------------------------------------------*
      *      AREA PARA O MODULO RUEC8800                               *
      *----------------------------------------------------------------*

       COPY I#RUEC58.

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(079)          VALUE
           '*** AREA PARA CHAMADA MODULO RUEC8822 ***'.
      *---------------------------------------------------------------*

       COPY 'I#RUEC79'.

EG0615*---------------------------------------------------------------*
EG0615     '*** AREA DO MODULO RUEC8825  ***'.
EG0615*---------------------------------------------------------------*
EG0615
EG0615 COPY 'RUECW824'.
EG0615
      *---------------------------------------------------------------*
      *  AREA PARA UTILIZACAO POOL7100.                               *
      *---------------------------------------------------------------*

       COPY POL7100C.

      *---------------------------------------------------------------*
      *  AREA BRAD0015- CALCULAR DIFERENCA DE DIAS ENTRE DATAS        *
      *---------------------------------------------------------------*
      *----------------------------------------------------------------*
      * FORMATO DATA = DDMMAAAA
      *----------------------------------------------------------------*

       01  WRK-DATA-INFERIOR.
           05 WRK-DTINF-DIA          PIC 9(02) VALUE ZEROS.
           05 WRK-DTINF-MES          PIC 9(02) VALUE ZEROS.

       01  WRK-DATA-SUPERIOR.
           05 WRK-DTSUP-DIA          PIC 9(02) VALUE ZEROS.
           05 WRK-DTSUP-MES          PIC 9(02) VALUE ZEROS.
           05 WRK-DTSUP-ANO          PIC 9(04) VALUE ZEROS.

       77  WRK-DIFER-DIAS            PIC S9(05)  COMP-3 VALUE ZEROS.
      *---------------------------------------------------------------*
      *01  FILLER                      PIC  X(32) VALUE
      *    '* AREA DA BRAD0650             *'.
      *---------------------------------------------------------------*

      *01  WRK-BYTE                PIC X(01)  VALUE SPACES.
      *01  WRK-MASCARA             PIC 9(04)  COMP  VALUE 08.

      *01  WRK-AREA                PIC X(16)  VALUE SPACES.
      *01  FILLER                  REDEFINES  WRK-AREA.
      *    05  FILLER              PIC X(07).
      *    05  WRK-CONTR1          PIC X(01).
      *    05  FILLER              PIC X(08).

      *                 DECLARACAO DE ACUMULADORES E AUXILIARES       *
      *---------------------------------------------------------------*

       01  FILLER.
           03  ACU-LIDOS-B002     PIC  9(07) COMP-3 VALUE ZEROS.
           03  ACU-LIDOS-B09B     PIC  9(07) COMP-3 VALUE ZEROS.
           03  ACU-GRAVA-ARQACUMU PIC  9(07) COMP-3 VALUE ZEROS.
           03  ACU-DESPR-B09B     PIC  9(07) COMP-3 VALUE ZEROS.

      *---------------------------------------------------------------*
      *          CAMPOS UTILIZADOS PARA TESTES DE FILE-STATUS         *
      *---------------------------------------------------------------*

       01  FILLER.
           03  WRK-FS-ARQDATA          PIC  X(02) VALUE  SPACES.
           03  WRK-FS-ARQACUMU         PIC  X(02) VALUE  SPACES.
           03  WRK-OPERACAO            PIC  X(13) VALUE  SPACES.
           03  WRK-LEITURA             PIC  X(13) VALUE 'NA LEITURA '.
           03  WRK-ABERTURA            PIC  X(13) VALUE 'NA ABERTURA'.
           03  WRK-GRAVACAO            PIC  X(13) VALUE 'NA GRAVACAO'.
           03  WRK-FECHAMENTO          PIC  X(13) VALUE 'NO FECHAMENTO'.
      *---------------------------------------------------------------*
      *  CAMPOS UTILIZADOS PARA O TRATAMENTO DE NULIDADE              *
      *---------------------------------------------------------------*

       01  WRK-B002-NULL.
           05 WRK-B002-DINIC-NULL       PIC S9(04) COMP VALUE +0.
           05 WRK-B002-DFIM-NULL        PIC S9(04) COMP VALUE +0.
           05 WRK-B002-CMUN-NULL        PIC S9(04) COMP VALUE +0.
           05 WRK-B002-CFNALD-NULL      PIC S9(04) COMP VALUE +0.
           05 WRK-B002-CATVDD-NULL      PIC S9(04) COMP VALUE +0.
           05 WRK-B002-CORIGE-NULL      PIC S9(04) COMP VALUE +0.
           05 WRK-B002-CPRODT-NULL      PIC S9(04) COMP VALUE +0.
           05 WRK-B002-MEDD-NULL        PIC S9(04) COMP VALUE +0.
           05 WRK-B002-CESPCE-NULL      PIC S9(04) COMP VALUE +0.
           05 WRK-B002-PROTR-NULL       PIC S9(04) COMP VALUE +0.
           05 WRK-B002-VRECTA-NULL      PIC S9(04) COMP VALUE +0.

       01  WRK-B004-NULL.
           05 WRK-B004-DTEVENTO-NULL    PIC S9(04) COMP VALUE +0.

       01  WRK-B099-NULL.
           05 WRK-B099-DVCTO-NULL   PIC S9(04) COMP VALUE +0.
           05 WRK-B099-VFINAN-NULL  PIC S9(04) COMP VALUE +0.
           05 WRK-B099-CCATEG-NULL  PIC S9(04) COMP VALUE +0.
           05 WRK-B099-CPROG-NULL   PIC S9(04) COMP VALUE +0.
           05 WRK-B099-CMUN-NULL    PIC S9(04) COMP VALUE +0.
           05 WRK-B099-LIBRC-NULL   PIC S9(04) COMP VALUE +0.
           05 WRK-B099-REC-NULL     PIC S9(04) COMP VALUE +0.
           05 WRK-B099-FINCD-NULL   PIC S9(04) COMP VALUE +0.
           05 WRK-B099-PROD-NULL    PIC S9(04) COMP VALUE +0.
           05 WRK-B099-DINIC-NULL   PIC S9(04) COMP VALUE +0.
           05 WRK-B099-DFIM-NULL    PIC S9(04) COMP VALUE +0.
           05 WRK-B099-GARNT-NULL   PIC S9(04) COMP VALUE +0.

       01  WRK-B09B-NULL.
           05 WRK-B09B-CMSGEM-NULL      PIC S9(04) COMP VALUE +0.
           05 WRK-B09B-HINCL-NULL       PIC S9(04) COMP VALUE +0.
           05 WRK-B09B-CCTRL-NULL       PIC S9(04) COMP VALUE +0.
           05 WRK-B09B-CUNIC-NULL       PIC S9(04) COMP VALUE +0.
           05 WRK-B09B-CREFT-NULL       PIC S9(04) COMP VALUE +0.
           05 WRK-B09B-CSIT-NULL        PIC S9(04) COMP VALUE +0.
           05 WRK-B09B-CUSUAR-NULL      PIC S9(04) COMP VALUE +0.
       01  WRK-B09D-NULL.
           05 WRK-B09D-DVCTO-NULL       PIC S9(04) COMP VALUE +0.

       01  WRK-B09G-NULL.
           05 WRK-B09G-CPRODT-NULL      PIC S9(04) COMP VALUE +0.

      *---------------------------------------------------------------*
      *                 DECLARACAO DE TABELAS DE DB2                  *
      *---------------------------------------------------------------*

           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
                INCLUDE RUECB007
           END-EXEC.

           EXEC SQL
                INCLUDE RUECB014
           END-EXEC.
           EXEC SQL
                INCLUDE RUECB061
           END-EXEC.

           EXEC SQL
                INCLUDE RUECB063
           END-EXEC.

           EXEC SQL
                INCLUDE RUECB097
           END-EXEC.

           EXEC SQL
                INCLUDE RUECB099
           END-EXEC.

           EXEC SQL
                INCLUDE RUECB09B
           END-EXEC.

           EXEC SQL
           END-EXEC.

           EXEC SQL
                INCLUDE RUECB09G
           END-EXEC.

           EXEC SQL
                INCLUDE RUECV016
           END-EXEC.

           EXEC SQL
                INCLUDE RUCAB004
           END-EXEC.

           EXEC SQL
                INCLUDE RUFIB002
           END-EXEC.

           EXEC SQL
                INCLUDE RURCB000
           END-EXEC.
           EXEC SQL
                INCLUDE RURCB018
           END-EXEC.

           EXEC SQL
                INCLUDE RURCB028
           END-EXEC.

      *---------------------------------------------------------------*
      *                 DECLARACAO DO CSR01-RUFIB002                  *
      *---------------------------------------------------------------*

           EXEC SQL
           DECLARE CSR01-RUFIB002 CURSOR WITH HOLD FOR
           SELECT
                   CCONTR,
                   CCDULA,
                   DCDULA,
                   DINIC_SAFRA,
                   DVCTO_CONTR,
                   VFINAN,
                   QUND_PROD_PROVV,
                   VREC_PPRIO,
                   DFIM_SAFRA,
                   CJUNC_DEPDC,
                   CMUN_RURAL,
                   CFNALD_RURAL,
                   CATVDD_RURAL,
                   CORIGE_REC,
                   CPRODT_RURAL,
                   CUND_MEDD_FINCD,
                   CESPCE_TITLO_GARNT,
                   CTPO_PROTR_RURAL,
                   DIMPRE_CONTR_RURAL,
                   CCTRO_CUSTO,
                   CCESTA_SAFRA_RURAL,
                   CMODLD_RURAL,
                   CVAR_PRODT_RURAL,
                   CINDCD_ZNMTO,
                   CINDCD_CSCIO,
                   CTPO_AGROP_RURAL,
                   CTPO_INTGC_RURAL,
                   CTPO_IGCAO_RURAL,
                   CTPO_CTIVO_RURAL,
                   CCICLO_PROD_RURAL,
                   CREFT_RECOR,
                   CTPO_CDULA_RECOR,
                   VRECTA_BRUTA_EMPTO
           FROM  DB2PRD.CONTRATO_RURAL
           WHERE ((CSTTUS_CONTR_RURAL = 0) OR
                 (CSTTUS_CONTR_RURAL = 8))
           AND   CREFT_BACEN <> 0
           AND   ((CREFT_RECOR <> '00000000000')  AND
                 (CREFT_RECOR <> ' '))
BSI214     AND   CCTRO_CUSTO <> 'CRUR'
           ORDER  BY  CCONTR
           END-EXEC.

      *---------------------------------------------------------------*
      *                 DECLARACAO DO CSR02-RUCAB004                  *
      *---------------------------------------------------------------*

           EXEC SQL
           SELECT
                   B004_DTEVENTO,
                   B004_VLPRICOR
           FROM  DB2PRD.RUCA_CAD_DEP
           WHERE B004_CH_CONTR = :RUCAB004.B004-CH-CONTR
           AND   B004_CH_TIPO  = 100
           AND   B004_DEBCRED  = '6'
           AND   ((B004_CODLANC  <  050)  AND
                  (B004_CODLANC  <> 023)  AND
                  (B004_CODLANC  <> 024)  AND
                  (B004_CODLANC  <> 035)  AND
                  (B004_CODLANC  <> 040)  AND
                  (B004_CODLANC  <> 045)  AND
                  (B004_CODLANC  <> 047))
           AND   B004_VLAUXIL  <> 0
           ORDER BY  B004_DTEVENTO
           END-EXEC.

      *---------------------------------------------------------------*
      *                 DECLARACAO DO CSR01-RUECB09B                  *
      *---------------------------------------------------------------*
           EXEC SQL
           DECLARE CSR01-RUECB09B CURSOR  WITH HOLD  FOR
           SELECT
                   CCONTR_CREDT_RURAL,
                   NENVIO_RECOR,
                   HULT_ATULZ,
                   CMSGEM_RECOR,
                   HINCL,
                   CCTRL_RQUIS_FINCR,
                   CUNIC_MSGEM,
                   CREFT_RECOR,
                   CSIT_MSGEM,
                   CUSUAR_RESP
           FROM  DB2PRD.THIST_ENVIO_RECOR
           WHERE CCONTR_CREDT_RURAL = :RUECB09B.CCONTR-CREDT-RURAL
           ORDER BY   NENVIO_RECOR       DESC
           END-EXEC.

      *---------------------------------------------------------------*
      *                 DECLARACAO DO CSR01-RUECB09G                  *
      *---------------------------------------------------------------*
           EXEC SQL
           DECLARE CSR01-RUECB09G CURSOR FOR
           SELECT CPRODT_RURAL,
                  NPRODT_RURAL_CSCDO
           FROM  DB2PRD.TPRODT_RURAL_CSCDO
           WHERE CCONTR_CREDT_RURAL = :RUECB09G.CCONTR-CREDT-RURAL
           ORDER BY CPRODT_RURAL ASC
           END-EXEC.

      *---------------------------------------------------------------*
      *                 DECLARACAO DO CSR01-RUECB014                  *
      *---------------------------------------------------------------*

           EXEC SQL
           DECLARE CSR01-RUECB014 CURSOR FOR
           SELECT CTPO_PSSOA,
                  CTPO_RESP_PSSOA,
                  CCGC_CPF,
                  CFLIAL_CGC,
                  CCTRL_CPF_CGC
           FROM  DB2PRD.PESSOA_CONTR_RURAL
           AND   CTPO_RESP_PSSOA    = :RUECB014.CTPO-RESP-PSSOA
           ORDER BY CSEQ_PSSOA ASC
           END-EXEC.

      *---------------------------------------------------------------*
      *                 DECLARACAO DO CSR01-RUECB09D                  *
      *---------------------------------------------------------------*

           EXEC  SQL
           DECLARE CSR01-RUECB09D CURSOR  WITH HOLD  FOR
           SELECT  CCONTR_CREDT_RURAL,
                   NENVIO_RECOR,
                   NPCELA_CONTR,
                   DVCTO_PCELA_CONTR,
                   VPCELA_RURAL
           FROM  DB2PRD.TPCELA_ENVIO_RECOR
           WHERE CCONTR_CREDT_RURAL = :RUECB09D.CCONTR-CREDT-RURAL
           AND   NENVIO_RECOR       = :RUECB09D.NENVIO-RECOR
           ORDER  BY  CCONTR_CREDT_RURAL,
                      NENVIO_RECOR,
                      NPCELA_CONTR

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(32)        VALUE
           '*  FIM DA WORKING RUEC1596 *'.
      *---------------------------------------------------------------*

EG0615*---------------------------------------------------------------*
EG0615 LINKAGE                         SECTION.
EG0615*---------------------------------------------------------------*
EG0615
EG0615 01  LNK-PARM.
EG0615     03  LNK-TAMANHO             PIC S9(04) COMP.
EG0615     03  LNK-DATA                PIC  X(08).
EG0615
      *===============================================================*
       PROCEDURE DIVISION              USING LNK-PARM.
      *===============================================================*

      *---------------------------------------------------------------*
       0000-INICIO  SECTION.
      *---------------------------------------------------------------*
           CALL 'POOL1050'.

           OPEN    INPUT   ARQDATA
                   OUTPUT  ARQACUMU.

           MOVE    WRK-ABERTURA        TO    WRK-OPERACAO.
           PERFORM 0100-TESTAR-FILE-STATUS.

           PERFORM 1000-LER-ARQDATA.

EG0615     IF  LNK-TAMANHO             EQUAL ZEROS
EG0615     OR  LNK-TAMANHO             NOT EQUAL 08
EG0615     OR  LNK-DATA                NOT NUMERIC
EG0615         DISPLAY 'PARM INVALIDO. UTILIZADA DATA PADRAO 20150701'
EG0615         MOVE '20150701'         TO LNK-DATA
EG0615     END-IF.
EG0615
           PERFORM 2000-SELECT-RUECV016.

           PERFORM 3000-OPEN-CSR01-RUFIB002.


           PERFORM 3020-CLOSE-CSR01-RUFIB002.

           PERFORM 9000-DISPLAY-SYSOUT.

           STOP RUN.

      *---------------------------------------------------------------*
       0000-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       0100-TESTAR-FILE-STATUS SECTION.
      *---------------------------------------------------------------*

           PERFORM 0110-TESTAR-FS-ARQDATA.
           PERFORM 0120-TESTAR-FS-ARQACUMU.

      *---------------------------------------------------------------*
       0100-99-FIM. EXIT.
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
       0110-TESTAR-FS-ARQDATA   SECTION.
      *---------------------------------------------------------------*

           IF WRK-FS-ARQDATA        NOT EQUAL  '00' AND
              WRK-FS-ARQDATA        NOT EQUAL  '10'
              DISPLAY '************** RUEC1596 *************'
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
              DISPLAY '*              ARQDATA             *'
              DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQDATA
                                                 '         *'
              DISPLAY '************** RUEC1595 *************'
              PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       0110-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       0120-TESTAR-FS-ARQACUMU  SECTION.

           IF WRK-FS-ARQACUMU       NOT EQUAL  '00'
              DISPLAY '************** RUEC1595 *************'
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
              DISPLAY '*              ARQACUMU             *'
              DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQACUMU
                                                 '         *'
              DISPLAY '************** RUEC1595 *************'
              PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       0120-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-LER-ARQDATA                SECTION.
      *----------------------------------------------------------------*

           READ ARQDATA  INTO N090RURA.


           PERFORM 0110-TESTAR-FS-ARQDATA.

           IF  WRK-FS-ARQDATA          EQUAL '10'
               DISPLAY '**************** RUEC1610 ******************'
               DISPLAY '*                                          *'
               DISPLAY '*         ARQUIVO ARQDATA VAZIO           *'
               DISPLAY '*                                          *'
               DISPLAY '**************** RUEC1610 ******************'
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           MOVE N090DPCB  TO  WRK-DATA.
           MOVE WRK-DATA-DIA  TO WRK-DTSUP-DIA.
           MOVE WRK-DATA-MES  TO WRK-DTSUP-MES.
           MOVE WRK-DATA-ANO  TO WRK-DTSUP-ANO.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

       2000-SELECT-RUECV016            SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                SELECT VINFER_PARM_RURAL
                INTO   :RUECV016.VINFER-PARM-RURAL
                FROM   DB2PRD.V01PARAMETRO_RURAL
                WHERE  CPARM_RURAL  =  'PZALR'
           END-EXEC.

           IF (SQLCODE           NOT EQUAL   +0 AND +100)    OR
              (SQLWARN0              EQUAL  'W')
               MOVE 'DB2'                    TO   ERR-TIPO-ACESSO
               MOVE 'V01PARAMETRO_RURAL'     TO   ERR-DBD-TAB
               MOVE 'SELECT  '               TO   ERR-FUN-COMANDO
               MOVE SQLCODE                  TO   ERR-SQL-CODE
               MOVE '2000'                   TO   ERR-LOCAL
               MOVE SPACES                   TO   ERR-SEGM
               PERFORM   9999-ROTINA-ERRO
           END-IF.

       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       3000-OPEN-CSR01-RUFIB002 SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
                OPEN CSR01-RUFIB002
           END-EXEC.

           IF (SQLCODE           NOT EQUAL   +0)    OR
              (SQLWARN0              EQUAL  'W')
               MOVE 'DB2'                    TO   ERR-TIPO-ACESSO
               MOVE 'CONTRATO_RURAL'         TO   ERR-DBD-TAB
               MOVE 'OPEN    '               TO   ERR-FUN-COMANDO
               MOVE SQLCODE                  TO   ERR-SQL-CODE
               MOVE '3000'                   TO   ERR-LOCAL
               MOVE SPACES                   TO   ERR-SEGM
               PERFORM   9999-ROTINA-ERRO
           END-IF.
      *---------------------------------------------------------------*
       3000-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3010-FETCH-CSR01-RUFIB002 SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
                FETCH CSR01-RUFIB002
                  INTO
                   :RUFIB002.CCONTR,
                   :RUFIB002.CCDULA,
                   :RUFIB002.DCDULA,
                   :RUFIB002.DINIC-SAFRA
                        :WRK-B002-DINIC-NULL,
                   :RUFIB002.DVCTO-CONTR,
                   :RUFIB002.VFINAN,
                   :RUFIB002.QUND-FINCD-RURAL,
                   :RUFIB002.QUND-PROD-PROVV,
                   :RUFIB002.VREC-PPRIO,
                        :WRK-B002-DFIM-NULL,
                   :RUFIB002.CJUNC-DEPDC,
                   :RUFIB002.CMUN-RURAL
                        :WRK-B002-CMUN-NULL,
                   :RUFIB002.CFNALD-RURAL
                        :WRK-B002-CFNALD-NULL,
                   :RUFIB002.CATVDD-RURAL
                        :WRK-B002-CATVDD-NULL,
                   :RUFIB002.CORIGE-REC
                        :WRK-B002-CORIGE-NULL,
                   :RUFIB002.CPRODT-RURAL
                        :WRK-B002-CPRODT-NULL,
                   :RUFIB002.CUND-MEDD-FINCD
                        :WRK-B002-MEDD-NULL,
                   :RUFIB002.CESPCE-TITLO-GARNT
                        :WRK-B002-CESPCE-NULL,
                   :RUFIB002.CTPO-PROTR-RURAL
                        :WRK-B002-PROTR-NULL,
                   :RUFIB002.DIMPRE-CONTR-RURAL,
                   :RUFIB002.CCTRO-CUSTO,
                   :RUFIB002.CCESTA-SAFRA-RURAL,
                   :RUFIB002.CVAR-PRODT-RURAL,
                   :RUFIB002.CINDCD-ZNMTO,
                   :RUFIB002.CINDCD-CSCIO,
                   :RUFIB002.CTPO-AGROP-RURAL,
                   :RUFIB002.CTPO-INTGC-RURAL,
                   :RUFIB002.CINDCD-UND-PRDTV,
                   :RUFIB002.CTPO-IGCAO-RURAL,
                   :RUFIB002.CTPO-CTIVO-RURAL,
                   :RUFIB002.CCICLO-PROD-RURAL,
                   :RUFIB002.CREFT-RECOR,
                   :RUFIB002.CTPO-CDULA-RECOR,
                   :RUFIB002.VRECTA-BRUTA-EMPTO
                        :WRK-B002-VRECTA-NULL
             END-EXEC.

           IF (SQLCODE           NOT EQUAL  +0 AND +100)  OR
              (SQLWARN0              EQUAL  'W')
               MOVE 'DB2'                    TO   ERR-TIPO-ACESSO
               MOVE 'CONTRATO_RURAL'         TO   ERR-DBD-TAB
               MOVE 'FETCH   '               TO   ERR-FUN-COMANDO
               MOVE SQLCODE                  TO   ERR-SQL-CODE
               MOVE SPACES                   TO   ERR-SEGM
               PERFORM   9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE      EQUAL +100
               MOVE  1        TO  WRK-FIM
               GO             TO  3010-99-FIM
           END-IF.

ATEN       IF  CCONTR OF RUFIB002 EQUAL 008387062
ATEN           DISPLAY 'RUEC1595 - REG. DESPR. = ' CCONTR OF RUFIB002
ATEN           GO TO 3010-FETCH-CSR01-RUFIB002
ATEN       END-IF.

           IF  SQLCODE      EQUAL ZEROS
               PERFORM 3011-VER-NULOS-B002
               ADD   1      TO  ACU-LIDOS-B002
           END-IF.

PRI499     PERFORM 3012-OBTER-TPO-CLIMA.

       3010-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3011-VER-NULOS-B002  SECTION.
      *---------------------------------------------------------------*

           IF WRK-B002-DINIC-NULL         LESS ZEROS
              MOVE ZEROS   TO DINIC-SAFRA        OF RUFIB002
           END-IF.

           IF WRK-B002-DFIM-NULL         LESS ZEROS
              MOVE ZEROS   TO DFIM-SAFRA         OF RUFIB002
           END-IF.

           IF WRK-B002-CMUN-NULL         LESS ZEROS
              MOVE ZEROS   TO CMUN-RURAL         OF RUFIB002
           END-IF.

           IF WRK-B002-CFNALD-NULL       LESS ZEROS
              MOVE ZEROS   TO CFNALD-RURAL       OF RUFIB002

           IF WRK-B002-CATVDD-NULL       LESS ZEROS
              MOVE ZEROS   TO CATVDD-RURAL       OF RUFIB002
           END-IF.

           IF WRK-B002-CORIGE-NULL       LESS ZEROS
              MOVE ZEROS   TO CORIGE-REC         OF RUFIB002
           END-IF.

           IF WRK-B002-CPRODT-NULL       LESS ZEROS
              MOVE ZEROS   TO CPRODT-RURAL       OF RUFIB002
           END-IF.

           IF WRK-B002-MEDD-NULL         LESS ZEROS
              MOVE ZEROS   TO CUND-MEDD-FINCD    OF RUFIB002
           END-IF.

           IF WRK-B002-CESPCE-NULL       LESS ZEROS
              MOVE ZEROS   TO CESPCE-TITLO-GARNT OF RUFIB002
           END-IF.

              MOVE ZEROS   TO CTPO-PROTR-RURAL   OF RUFIB002
           END-IF.

           IF WRK-B002-VRECTA-NULL      LESS ZEROS
              MOVE ZEROS   TO VRECTA-BRUTA-EMPTO OF RUFIB002
           END-IF.

      *---------------------------------------------------------------*
       3011-99-FIM. EXIT.
      *---------------------------------------------------------------*

PRI499*---------------------------------------------------------------*
PRI499 3012-OBTER-TPO-CLIMA            SECTION.
PRI499*---------------------------------------------------------------*
PRI499
PRI499     EXEC SQL
PRI499      SELECT CCLIMT_RURAL_BACEN
PRI499        INTO :RUFIB002.CCLIMT-RURAL-BACEN
PRI499        FROM DB2PRD.CONTRATO_RURAL
PRI499      WHERE  CCONTR = :RUFIB002.CCONTR
PRI499     END-EXEC.
PRI499     IF (SQLCODE           NOT EQUAL  +0 AND +100)  OR
PRI499        (SQLWARN0              EQUAL  'W')
PRI499         MOVE 'DB2'                    TO   ERR-TIPO-ACESSO
PRI499         MOVE 'CONTRATO_RURAL'         TO   ERR-DBD-TAB
PRI499         MOVE 'FETCH   '               TO   ERR-FUN-COMANDO
PRI499         MOVE SQLCODE                  TO   ERR-SQL-CODE
PRI499         MOVE '3012'                   TO   ERR-LOCAL
PRI499         MOVE SPACES                   TO   ERR-SEGM
PRI499         PERFORM   9999-ROTINA-ERRO
PRI499     END-IF.
PRI499
PRI499     IF  SQLCODE      EQUAL +100
PRI499         MOVE  1        TO  WRK-FIM
PRI499     END-IF.
PRI499
PRI499*---------------------------------------------------------------*
PRI499 3012-99-FIM.                    EXIT.
PRI499*---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3020-CLOSE-CSR01-RUFIB002 SECTION.

           EXEC SQL
               CLOSE CSR01-RUFIB002
           END-EXEC.

           IF (SQLCODE           NOT EQUAL   +0)    OR
              (SQLWARN0              EQUAL  'W')
               MOVE 'DB2'                    TO   ERR-TIPO-ACESSO
               MOVE 'CONTRATO_RURAL'         TO   ERR-DBD-TAB
               MOVE 'CLOSE   '               TO   ERR-FUN-COMANDO
               MOVE SQLCODE                  TO   ERR-SQL-CODE
               MOVE '3020'                   TO   ERR-LOCAL
               MOVE SPACES                   TO   ERR-SEGM
               PERFORM   9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       3020-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      *---------------------------------------------------------------*

           MOVE CCONTR       OF RUFIB002  TO B004-CH-CONTR
                                          OF RUCAB004.

           EXEC SQL
              OPEN      CSR02-RUCAB004
           END-EXEC.

           IF (SQLCODE           NOT EQUAL   +0)    OR
              (SQLWARN0              EQUAL  'W')
               MOVE 'DB2'                    TO   ERR-TIPO-ACESSO
               MOVE 'RUCA_CAD_DEP'           TO   ERR-DBD-TAB
               MOVE 'OPEN    '               TO   ERR-FUN-COMANDO
               MOVE SQLCODE                  TO   ERR-SQL-CODE
               MOVE '3030'                   TO   ERR-LOCAL
               MOVE SPACES                   TO   ERR-SEGM
               PERFORM   9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       3040-SELECT-RUCAB004      SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
              FETCH CSR02-RUCAB004 INTO
                 :RUCAB004.B004-DTEVENTO
                      :WRK-B004-DTEVENTO-NULL,
                 :RUCAB004.B004-VLPRICOR
           END-EXEC.

           IF (SQLCODE           NOT EQUAL ZEROS AND +100)  OR
              (SQLWARN0              EQUAL  'W')
               MOVE 'DB2'                    TO   ERR-TIPO-ACESSO
               MOVE 'RUCA_CAD_DEP'           TO   ERR-DBD-TAB
               MOVE 'FETCH   '               TO   ERR-FUN-COMANDO
               MOVE SQLCODE                  TO   ERR-SQL-CODE
               MOVE '3040'                   TO   ERR-LOCAL
               MOVE SPACES                   TO   ERR-SEGM
           END-IF.

           IF SQLCODE   EQUAL  +100
              MOVE 'S' TO WRK-CODE100-B004
           END-IF.

           IF SQLCODE   EQUAL  ZEROS
              PERFORM 3041-VER-NULOS-B004
           END-IF.

      *---------------------------------------------------------------*
       3040-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3041-VER-NULOS-B004  SECTION.
      *---------------------------------------------------------------*

           IF WRK-B004-DTEVENTO-NULL  LESS  ZEROS
              MOVE SPACES  TO  B004-DTEVENTO  OF RUCAB004
           END-IF.
      *----------------------------------------------------------------*
       3041-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       3050-CLOSE-CSR02-RUCAB004 SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR02-RUCAB004
           END-EXEC.

           IF (SQLCODE           NOT EQUAL   +0)    OR
              (SQLWARN0              EQUAL  'W')
               MOVE 'DB2'                    TO   ERR-TIPO-ACESSO
               MOVE 'RUCA_CAD_DEP'           TO   ERR-DBD-TAB
               MOVE 'CLOSE   '               TO   ERR-FUN-COMANDO
               MOVE SQLCODE                  TO   ERR-SQL-CODE
               MOVE '3050'                   TO   ERR-LOCAL
               MOVE SPACES                   TO   ERR-SEGM
               PERFORM   9999-ROTINA-ERRO

      *---------------------------------------------------------------*
       3050-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3060-SELECT-RURCB018        SECTION.
      *---------------------------------------------------------------*

           MOVE CESPCE-TITLO-GARNT  OF RUFIB002
                                    TO CESPCE-TITLO-GARNT OF RURCB018.

           EXEC SQL
             SELECT CTPO_GARNT_RECOR
                  INTO    :RURCB018.CTPO-GARNT-RECOR
             FROM   DB2PRD.ESPCE_TITLO_GARNT
             WHERE CESPCE_TITLO_GARNT = :RURCB018.CESPCE-TITLO-GARNT
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 'ESPCE_TITLO_GARNT' TO ERR-DBD-TAB
               MOVE 'SELECT'            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '3060'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                  EQUAL +100
               MOVE ZEROS TO   CTPO-GARNT-RECOR  OF RURCB018
               DISPLAY 'RUEC1595-NAO ENCONTRADO CESPCE-TITLO-GARNT = '
                             CESPCE-TITLO-GARNT OF RURCB018
               DISPLAY 'PARA CONTRATO = ' CCONTR OF RUFIB002
           END-IF.

      *---------------------------------------------------------------*
       3060-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3080-SELECT-RUECB014     SECTION.

           MOVE CCONTR              OF RUFIB002
                                    TO CCONTR  OF RUECB014.

           EXEC SQL
           SELECT CTPO_PSSOA,
                  CTPO_RESP_PSSOA,
                  CCGC_CPF,
                  CFLIAL_CGC,
                  CCTRL_CPF_CGC
           INTO :RUECB014.CTPO-PSSOA,
                :RUECB014.CTPO-RESP-PSSOA,
                :RUECB014.CCGC-CPF,
                :RUECB014.CFLIAL-CGC,
                :RUECB014.CCTRL-CPF-CGC
           FROM  DB2PRD.PESSOA_CONTR_RURAL
           WHERE CCONTR         = :RUECB014.CCONTR
           AND CTPO_RESP_PSSOA  = 'T'
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'PESSOA_CONTR_RURAL' TO ERR-DBD-TAB
               MOVE 'SELECT'            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '3080'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       3080-99-FIM.                EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3090-OPEN-CSR01-RUECB09G    SECTION.
      *---------------------------------------------------------------*

           MOVE CCONTR         OF RUFIB002
                               TO CCONTR-CREDT-RURAL  OF RUECB09G.

           EXEC SQL
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'TPRODT_RURAL-CSCDO' TO ERR-DBD-TAB
               MOVE 'OPEN  '            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '3090'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       3090-99-FIM.                EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3100-FETCH-CSR01-RUECB09G         SECTION.
      *---------------------------------------------------------------*

           FETCH CSR01-RUECB09G
           INTO  :RUECB09G.CPRODT-RURAL
                      :WRK-B09G-CPRODT-NULL,
                 :RUECB09G.NPRODT-RURAL-CSCDO
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'TPRODT_RURAL_CSCDO' TO ERR-DBD-TAB
               MOVE 'FETCH '            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '3100'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE  EQUAL  ZEROS
               PERFORM 3101-B09G-VER-NULOS
               GO 3100-99-FIM
           END-IF.
           IF  SQLCODE  EQUAL  +100
               MOVE 'S' TO WRK-CODE100-B09G
           END-IF.

      *---------------------------------------------------------------*
       3100-99-FIM.                EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3101-B09G-VER-NULOS                SECTION.
      *---------------------------------------------------------------*

           IF  WRK-B09G-CPRODT-NULL  LESS  ZEROS
               MOVE ZEROS TO CPRODT-RURAL OF RUECB09G
           END-IF.

      *---------------------------------------------------------------*
       3101-99-FIM.                EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      *---------------------------------------------------------------*

           EXEC SQL
           CLOSE CSR01-RUECB09G
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'TPRODT_RURAL_CSCDO' TO ERR-DBD-TAB
               MOVE 'CLOSE '            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '3110'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       3110-99-FIM.                EXIT.
      *---------------------------------------------------------------*

       3120-SELECT-RURCB000      SECTION.
      *---------------------------------------------------------------*

           MOVE CORIGE-REC          OF RUFIB002  TO CORIGE-REC
                                                 OF RURCB000.

           EXEC SQL
           SELECT CPLANO_REFIN_RURAL,
                  CPROG_RURAL
           INTO   :RURCB000.CPLANO-REFIN-RURAL,
                  :RURCB000.CPROG-RURAL
           FROM  DB2PRD.ORIGEM_RECURSO
           WHERE CORIGE_REC           = :RURCB000.CORIGE-REC
           END-EXEC.

           IF (SQLCODE   NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0      EQUAL 'W')
               MOVE 'DB2'                TO ERR-TIPO-ACESSO
               MOVE 'ORIGEM_RECURSO'     TO ERR-DBD-TAB
               MOVE 'SELECT'             TO ERR-FUN-COMANDO
               MOVE SQLCODE              TO ERR-SQL-CODE
               MOVE SPACES               TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                  EQUAL +100
               MOVE ZEROS  TO  CPLANO-REFIN-RURAL OF RURCB000
               MOVE SPACES TO  CPROG-RURAL        OF RURCB000
               DISPLAY 'RUEC1595-NAO ENCONTRADO CORIGE-REC = '
                             CORIGE-REC         OF RURCB000
               DISPLAY 'PARA CONTRATO = ' CCONTR OF RUFIB002
           END-IF.

      *----------------------------------------------------------------*
       3120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       3130-SELECT-RURCB028      SECTION.
      *---------------------------------------------------------------*

           MOVE CPLANO-REFIN-RURAL  OF RURCB000  TO CPLANO-REFIN

           EXEC SQL
           SELECT CPLANO_REFIN,
                  CPLANO_REFIN_RECOR
           INTO   :RURCB028.CPLANO-REFIN,
                  :RURCB028.CPLANO-REFIN-RECOR
           FROM  DB2PRD.PLANO_REFIN
           WHERE CPLANO_REFIN          = :RURCB028.CPLANO-REFIN
           END-EXEC.

           IF (SQLCODE   NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0      EQUAL 'W')
               MOVE 'DB2'                TO ERR-TIPO-ACESSO
               MOVE 'PLANO_REFIN'        TO ERR-DBD-TAB
               MOVE 'SELECT'             TO ERR-FUN-COMANDO
               MOVE SQLCODE              TO ERR-SQL-CODE
               MOVE '3130'               TO ERR-LOCAL
               MOVE SPACES               TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

               MOVE ZEROS  TO  CPLANO-REFIN       OF RURCB028
               MOVE SPACES TO  CPLANO-REFIN-RECOR OF RURCB028
               DISPLAY 'RUEC1595-NAO ENCONTRADO CPLANO-REFIN-RURAL = '
                             CPLANO-REFIN       OF RURCB028
               DISPLAY 'PARA CONTRATO = ' CCONTR OF RUFIB002
           END-IF.

      *----------------------------------------------------------------*
       3130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       3140-OPEN-CSR01-RUECB014          SECTION.
      *---------------------------------------------------------------*

           MOVE 'P'                    TO CTPO-RESP-PSSOA OF RUECB014
           MOVE ZEROS                  TO WRK-NPSSOA-CONTR
           MOVE CCONTR  OF RUFIB002    TO CCONTR          OF RUECB014

           EXEC SQL
           OPEN CSR01-RUECB014

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'PSSOA_RURAL      ' TO ERR-DBD-TAB
               MOVE 'OPEN  '            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '3140'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       3140-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       3150-FETCH-CSR01-RUECB014          SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
           INTO  :RUECB014.CTPO-PSSOA,
                 :RUECB014.CTPO-RESP-PSSOA,
                 :RUECB014.CCGC-CPF,
                 :RUECB014.CFLIAL-CGC,
                 :RUECB014.CCTRL-CPF-CGC
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'PSSOA_RURAL      ' TO ERR-DBD-TAB
               MOVE 'FETCH '            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '3150'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF (SQLCODE  EQUAL  +100) AND
              (WRK-TEMPROPR-B014  EQUAL 'N')
               MOVE 'S' TO  WRK-CODE100-B014
               PERFORM 7270-FORMATA-PROPRIET
               MOVE 'S'  TO  WRK-TEMPROPR-B014
               GO TO 3150-99-FIM
           ELSE
              IF (SQLCODE  EQUAL  +100)
                  MOVE 'S' TO  WRK-CODE100-B014
                  GO TO 3150-99-FIM
              END-IF
           END-IF.

           MOVE  'S'  TO  WRK-TEMPROPR-B014.

      *----------------------------------------------------------------*
       3150-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       3160-CLOSE-CSR01-RUECB014          SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'PSSOA_RURAL      ' TO ERR-DBD-TAB
               MOVE 'CLOSE '            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '3160'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       3160-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       3170-OPEN-CSR01-RUECB09D  SECTION.
      *---------------------------------------------------------------*

                                                OF RUECB09D.
           MOVE NENVIO-RECOR       OF RUECB09B  TO NENVIO-RECOR
                                                OF RUECB09D.

           EXEC SQL
           OPEN      CSR01-RUECB09D
           END-EXEC.

           IF (SQLCODE           NOT EQUAL   +0)    OR
              (SQLWARN0              EQUAL  'W')
               MOVE 'DB2'                    TO   ERR-TIPO-ACESSO
               MOVE 'TPCELA_ENVIO_RECOR'     TO   ERR-DBD-TAB
               MOVE 'OPEN    '               TO   ERR-FUN-COMANDO
               MOVE SQLCODE                  TO   ERR-SQL-CODE
               MOVE '3170'                   TO   ERR-LOCAL
               MOVE SPACES                   TO   ERR-SEGM
               PERFORM   9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       3170-99-FIM.                    EXIT.

      *---------------------------------------------------------------*
       3180-SELECT-RUECB09D      SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
           FETCH CSR01-RUECB09D
           INTO  :RUECB09D.CCONTR-CREDT-RURAL,
                 :RUECB09D.NENVIO-RECOR,
                 :RUECB09D.NPCELA-CONTR,
                 :RUECB09D.DVCTO-PCELA-CONTR
                      :WRK-B09D-DVCTO-NULL,
                 :RUECB09D.VPCELA-RURAL
           END-EXEC.

           IF (SQLCODE           NOT EQUAL   +0 AND +100)    OR
              (SQLWARN0              EQUAL  'W')
               MOVE 'DB2'                    TO   ERR-TIPO-ACESSO
               MOVE 'TPCELA_ENVIO_RECOR'     TO   ERR-DBD-TAB
               MOVE 'FETCH   '               TO   ERR-FUN-COMANDO
               MOVE SQLCODE                  TO   ERR-SQL-CODE
               MOVE SPACES                   TO   ERR-SEGM
               PERFORM   9999-ROTINA-ERRO
           END-IF.

           IF SQLCODE   EQUAL  +100
              MOVE 'S' TO WRK-CODE100-B09D
           END-IF.

           IF SQLCODE   EQUAL  ZEROS
              PERFORM 3181-B09D-VER-NULOS
           END-IF.

      *---------------------------------------------------------------*
       3180-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3181-B09D-VER-NULOS  SECTION.
      *---------------------------------------------------------------*

           IF WRK-B09D-DVCTO-NULL  LESS  ZEROS
           END-IF.

      *----------------------------------------------------------------*
       3181-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       3190-CLOSE-CSR01-RUECB09D SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
           CLOSE CSR01-RUECB09D
           END-EXEC.

           IF (SQLCODE           NOT EQUAL   +0)    OR
              (SQLWARN0              EQUAL  'W')
               MOVE 'DB2'                    TO   ERR-TIPO-ACESSO
               MOVE 'TPCELA_ENVIO_RECOR'     TO   ERR-DBD-TAB
               MOVE 'CLOSE   '               TO   ERR-FUN-COMANDO
               MOVE SQLCODE                  TO   ERR-SQL-CODE
               MOVE '3190'                   TO   ERR-LOCAL
               PERFORM   9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       3190-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3240-OPEN-CSR01-RUECB09B          SECTION.
      *---------------------------------------------------------------*

           MOVE CCONTR  OF RUFIB002  TO  CCONTR-CREDT-RURAL
                                     OF  RUECB09B.

           EXEC SQL
                OPEN CSR01-RUECB09B
           END-EXEC.

           IF (SQLCODE           NOT EQUAL   +0)    OR
              (SQLWARN0              EQUAL  'W')
               MOVE 'DB2'                    TO   ERR-TIPO-ACESSO
               MOVE 'OPEN    '               TO   ERR-FUN-COMANDO
               MOVE SQLCODE                  TO   ERR-SQL-CODE
               MOVE '3240'                   TO   ERR-LOCAL
               MOVE SPACES                   TO   ERR-SEGM
               PERFORM   9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       3240-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3250-FETCH-CSR01-RUECB09B SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
                FETCH CSR01-RUECB09B
                  INTO
                   :RUECB09B.CCONTR-CREDT-RURAL,
                   :RUECB09B.NENVIO-RECOR,
                   :RUECB09B.HULT-ATULZ,
                        :WRK-B09B-CMSGEM-NULL,
                   :RUECB09B.HINCL
                        :WRK-B09B-HINCL-NULL,
                   :RUECB09B.CCTRL-RQUIS-FINCR
                        :WRK-B09B-CCTRL-NULL,
                   :RUECB09B.CUNIC-MSGEM
                        :WRK-B09B-CUNIC-NULL,
                   :RUECB09B.CREFT-RECOR
                        :WRK-B09B-CREFT-NULL,
                   :RUECB09B.CSIT-MSGEM
                        :WRK-B09B-CSIT-NULL,
                   :RUECB09B.CUSUAR-RESP
                        :WRK-B09B-CUSUAR-NULL
             END-EXEC.

           IF (SQLCODE           NOT EQUAL   +0 AND +100)    OR
              (SQLWARN0              EQUAL  'W')
               MOVE 'DB2'                    TO   ERR-TIPO-ACESSO
               MOVE 'THIST_ENVIO_RECOR'      TO   ERR-DBD-TAB
               MOVE 'FETCH   '               TO   ERR-FUN-COMANDO
               MOVE 'FETCH   '               TO   ERR-FUN-COMANDO
               MOVE '3250'                   TO   ERR-LOCAL
               MOVE SPACES                   TO   ERR-SEGM
               PERFORM   9999-ROTINA-ERRO
           END-IF.

EW0514     IF (SQLCODE       EQUAL  +100)
EW0514         DISPLAY 'CONTRATO INEX. RUECB09B = ' CCONTR OF RUFIB002
EW0514         GO             TO  3250-99-FIM
EW0514     END-IF.

           IF  SQLCODE      EQUAL ZEROS
               PERFORM 3251-B09B-VER-NULOS
               ADD   1      TO  ACU-LIDOS-B09B
           END-IF.

      *---------------------------------------------------------------*
       3250-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3251-B09B-VER-NULOS  SECTION.

           IF WRK-B09B-CMSGEM-NULL       LESS ZEROS
              MOVE ZEROS   TO CMSGEM-RECOR       OF RUECB09B
           END-IF.

           IF WRK-B09B-HINCL-NULL        LESS ZEROS
              MOVE SPACES TO  HINCL              OF RUECB09B
           END-IF.

           IF WRK-B09B-CCTRL-NULL        LESS ZEROS
              MOVE SPACES TO  CCTRL-RQUIS-FINCR  OF RUECB09B
           END-IF.

           IF WRK-B09B-CUNIC-NULL        LESS ZEROS
              MOVE SPACES TO  CUNIC-MSGEM        OF RUECB09B
           END-IF.

           IF WRK-B09B-CREFT-NULL        LESS ZEROS
              MOVE SPACES TO  CREFT-RECOR        OF RUECB09B
           END-IF.

              MOVE ZEROS  TO  CSIT-MSGEM   OF RUECB09B
           END-IF.

           IF WRK-B09B-CUSUAR-NULL       LESS ZEROS
              MOVE SPACES TO  CUSUAR-RESP        OF RUECB09B
           END-IF.

      *---------------------------------------------------------------*
       3251-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3260-CLOSE-CSR01-RUECB09B SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR01-RUECB09B
           END-EXEC.

           IF (SQLCODE           NOT EQUAL   +0)    OR
              (SQLWARN0              EQUAL  'W')
               MOVE 'THIST_ENVIO_RECOR'      TO   ERR-DBD-TAB
               MOVE 'CLOSE   '               TO   ERR-FUN-COMANDO
               MOVE SQLCODE                  TO   ERR-SQL-CODE
               MOVE '3260'                   TO   ERR-LOCAL
               MOVE SPACES                   TO   ERR-SEGM
               PERFORM   9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       3260-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3270-OBTER-DADOS-B099     SECTION.
      *---------------------------------------------------------------*

           MOVE CCONTR-CREDT-RURAL  OF RUECB09B  TO CCONTR-CREDT-RURAL
                                                 OF RUECB099.

           MOVE NENVIO-RECOR        OF RUECB09B  TO NENVIO-RECOR
                                                 OF RUECB099.
           EXEC SQL
           SELECT CCONTR_CREDT_RURAL,
                  NENVIO_RECOR,
                  CCNPJ_ENTID_RESP,
                  CFLIAL_CNPJ_ENTID,
                  CCTRL_CNPJ_ENTID,
                  CCNPJ_PARTC,
                  CFLIAL_CNPJ_PARTC,
                  CCTRL_CNPJ_PARTC,
                  CSIT_OPER_RURAL,
                  DCDULA,
                  DVCTO_CONTR,
                  CCDULA_RECOR,
                  CTPO_INSTT_CREDT,
                  VFINAN,
                  CCATEG_PROTR_RECOR,
                  CPROG_RURAL,
                  CTPO_FONTE_RECOR,
                  CMUN_REFT_BACEN,
                  CEMPTO_BACEN,
                  CSIST_PROD,
                  VPCELA_REC_PPRIO,
                  PALIQT_PROAGRO,
                  PJURO_ENCAR_FINCR,
                  CTPO_ENCAR_COMPL,
                  CTSORO_NACIO,
                  PRISCO_TSORO_NACIO,
                  PRISCO_FUNDO_CNSTT,
                  QAREA_RECOR,
                  QUND_FINCD_RURAL,
                  CUND_MEDD_FINCD,
                  QUND_PROD_PROVV,
                  CUND_MEDD_PROVV,
                  DINIC_SAFRA,
                  DFIM_SAFRA,
                  CTPO_GARNT_RECOR,
                  VRECTA_BRUTA_RECOR,
                  DMOVTO_RECOR
           INTO   :RUECB099.CCONTR-CREDT-RURAL,
                  :RUECB099.NENVIO-RECOR,
                  :RUECB099.CCNPJ-ENTID-RESP,
                  :RUECB099.CFLIAL-CNPJ-ENTID,
                  :RUECB099.CCNPJ-PARTC,
                  :RUECB099.CFLIAL-CNPJ-PARTC,
                  :RUECB099.CCTRL-CNPJ-PARTC,
                  :RUECB099.CSIT-OPER-RURAL,
                  :RUECB099.DCDULA
                       :WRK-B099-DCDULA-NULL,
                  :RUECB099.DVCTO-CONTR
                       :WRK-B099-DVCTO-NULL,
                  :RUECB099.CCDULA-RECOR,
                  :RUECB099.CTPO-INSTT-CREDT,
                  :RUECB099.VFINAN
                       :WRK-B099-VFINAN-NULL,
                  :RUECB099.CCATEG-PROTR-RECOR
                       :WRK-B099-CCATEG-NULL,
                  :RUECB099.CPROG-RURAL
                       :WRK-B099-CPROG-NULL,
                  :RUECB099.CTPO-FONTE-RECOR,
                  :RUECB099.CMUN-REFT-BACEN
                       :WRK-B099-CMUN-NULL,
                  :RUECB099.CEMPTO-BACEN,
                  :RUECB099.CSIST-PROD,
                       :WRK-B099-LIBRC-NULL,
                  :RUECB099.VPCELA-REC-PPRIO
                       :WRK-B099-REC-NULL,
                  :RUECB099.PALIQT-PROAGRO,
                  :RUECB099.PJURO-ENCAR-FINCR,
                  :RUECB099.CTPO-ENCAR-COMPL,
                  :RUECB099.CTSORO-NACIO,
                  :RUECB099.PRISCO-TSORO-NACIO,
                  :RUECB099.PRISCO-FUNDO-CNSTT,
                  :RUECB099.QAREA-RECOR,
                  :RUECB099.QUND-FINCD-RURAL
                      :WRK-B099-FINCD-NULL,
                  :RUECB099.CUND-MEDD-FINCD,
                  :RUECB099.QUND-PROD-PROVV
                       :WRK-B099-PROD-NULL,
                  :RUECB099.CUND-MEDD-PROVV,
                  :RUECB099.DINIC-SAFRA
                       :WRK-B099-DINIC-NULL,
                  :RUECB099.DFIM-SAFRA
                       :WRK-B099-DFIM-NULL,
                  :RUECB099.CTPO-GARNT-RECOR
                  :RUECB099.VRECTA-BRUTA-RECOR,
                  :RUECB099.DMOVTO-RECOR
           FROM  DB2PRD.TCONTR_ENVIO_RECOR
           WHERE CCONTR_CREDT_RURAL   = :RUECB099.CCONTR-CREDT-RURAL
           AND   NENVIO_RECOR         = :RUECB099.NENVIO-RECOR
           END-EXEC.

           IF (SQLCODE   NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0      EQUAL 'W')
               MOVE 'DB2'                TO ERR-TIPO-ACESSO
               MOVE 'TCONTR_ENVIO_RECOR' TO ERR-DBD-TAB
               MOVE 'SELECT'             TO ERR-FUN-COMANDO
               MOVE SQLCODE              TO ERR-SQL-CODE
               MOVE '3270'               TO ERR-LOCAL
               MOVE SPACES               TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

MF3107     IF (SQLCODE       EQUAL  +100)
MF3107        DISPLAY ' '
MF3107        DISPLAY 'CONTRATO INEX. RUECB099 = '
MF3107        DISPLAY 'NENVIO   = ' NENVIO-RECOR        OF RUECB09B
MF3107        GO  TO  3270-99-FIM
MF3107     END-IF.

           IF (SQLCODE       EQUAL  +100)
               MOVE 'DB2'                TO ERR-TIPO-ACESSO
               MOVE 'TCONTR_ENVIO_RECOR' TO ERR-DBD-TAB
               MOVE 'SELECT'             TO ERR-FUN-COMANDO
               MOVE SQLCODE              TO ERR-SQL-CODE
               MOVE '406A'               TO ERR-LOCAL
               MOVE SPACES               TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF SQLCODE   EQUAL ZEROS
              PERFORM 3271-B099-VER-NULOS
           END-IF.

      *----------------------------------------------------------------*
       3270-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3271-B099-VER-NULOS            SECTION.
      *----------------------------------------------------------------*

           IF WRK-B099-DCDULA-NULL  LESS ZEROS
              MOVE SPACES TO DCDULA              OF RUECB099
           END-IF.

           IF WRK-B099-DVCTO-NULL   LESS ZEROS
              MOVE SPACES TO  DVCTO-CONTR        OF RUECB099
           END-IF.

           IF WRK-B099-VFINAN-NULL LESS ZEROS
              MOVE ZEROS  TO VFINAN              OF RUECB099
           END-IF.

           IF WRK-B099-CCATEG-NULL LESS ZEROS
              MOVE SPACES TO CCATEG-PROTR-RECOR  OF RUECB099
           END-IF.

           IF WRK-B099-CPROG-NULL    LESS ZEROS
           END-IF.

           IF WRK-B099-CMUN-NULL LESS ZEROS
              MOVE ZEROS  TO CMUN-REFT-BACEN     OF RUECB099
           END-IF.

           IF WRK-B099-LIBRC-NULL LESS ZEROS
              MOVE ZEROS  TO VPCELA-LIBRC        OF RUECB099
           END-IF.

           IF WRK-B099-REC-NULL LESS ZEROS
              MOVE ZEROS  TO VPCELA-REC-PPRIO    OF RUECB099
           END-IF.

           IF WRK-B099-FINCD-NULL LESS ZEROS
              MOVE ZEROS  TO QUND-FINCD-RURAL    OF RUECB099
           END-IF.

           IF WRK-B099-PROD-NULL  LESS ZEROS
              MOVE ZEROS  TO QUND-PROD-PROVV     OF RUECB099
           END-IF.
           IF WRK-B099-DINIC-NULL LESS ZEROS
              MOVE ZEROS  TO DINIC-SAFRA         OF RUECB099
           END-IF.

           IF WRK-B099-DFIM-NULL LESS ZEROS
              MOVE ZEROS  TO DFIM-SAFRA          OF RUECB099
           END-IF.

           IF WRK-B099-GARNT-NULL LESS ZEROS
              MOVE ZEROS  TO CTPO-GARNT-RECOR    OF RUECB099
           END-IF.

      *---------------------------------------------------------------*
       3271-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3320-COUNT-RUECB097       SECTION.
      *---------------------------------------------------------------*

           MOVE CJUNC-DEPDC  OF RUFIB002 TO CJUNC-DEPDC
           MOVE CCDULA       OF RUFIB002 TO CCDULA-CREDT-RURAL
                                          OF RUECB097.

           EXEC SQL
           SELECT  COUNT(*)
           INTO  :WRK-QTDREG-B097
           FROM  DB2PRD.TCDULA_COMCZ
           WHERE CJUNC_DEPDC          = :RUECB097.CJUNC-DEPDC
           AND   CCDULA_CREDT_RURAL   = :RUECB097.CCDULA-CREDT-RURAL
           END-EXEC.

           IF (SQLCODE   NOT EQUAL ZEROS AND -305 AND -310)  OR
              (SQLWARN0      EQUAL 'W')
               MOVE 'DB2'                TO  ERR-TIPO-ACESSO
               MOVE 'TCDULA_COMCZ'       TO  ERR-DBD-TAB
               MOVE 'COUNT'              TO  ERR-FUN-COMANDO
               MOVE SQLCODE              TO  ERR-SQL-CODE
               MOVE '3320'               TO  ERR-LOCAL
               MOVE SPACES               TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.
      *---------------------------------------------------------------*
       3320-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3700-ACESSAR-RUCAB004-TAXA      SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
                  SELECT B004_VLAUXIL
                  INTO   :B004-VLAUXIL
                  FROM   DB2PRD.RUCA_CAD_DEP
                  WHERE  B004_CH_CONTR = :RUFIB002.CCONTR
                    AND  B004_CH_TIPO  = 100
                    AND  B004_DEBCRED  = '8'
                    AND  B004_CODLANC  BETWEEN 200 AND 299
                  ORDER BY B004_DTEVENTO DESC
                  FETCH FIRST 1 ROW ONLY
           END-EXEC

           IF (SQLCODE   NOT EQUAL ZEROS AND +100)  OR
               MOVE 'DB2'                TO  ERR-TIPO-ACESSO
               MOVE 'RUCA_CAD_DEP'       TO  ERR-DBD-TAB
               MOVE 'SELECT'             TO  ERR-FUN-COMANDO
               MOVE SQLCODE              TO  ERR-SQL-CODE
               MOVE '3700'               TO  ERR-LOCAL
               MOVE SPACES               TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE       EQUAL +100
               MOVE  ZEROS               TO  B004-VLAUXIL
           END-IF.

           MOVE B004-VLAUXIL             TO WRK-VALOR.

      *---------------------------------------------------------------*
       3700-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       6000-SELECIONAR           SECTION.

           PERFORM 3010-FETCH-CSR01-RUFIB002.

           IF SQLCODE EQUAL +100
              GO      TO    6000-99-FIM
           END-IF.

MF0813     IF  CORIGE-REC OF RUFIB002  EQUAL  104 OR 140
MF0813         GO      TO    6000-99-FIM
MF0813     END-IF.

           IF  CORIGE-REC OF RUFIB002  NOT  LESS  060
           AND CORIGE-REC OF RUFIB002  NOT  GREATER  081
               GO      TO    6000-99-FIM
           END-IF.

           PERFORM 7000-CALCDIFE-DATA.

           IF (WRK-DIFER-DIAS GREATER VINFER-PARM-RURAL OF RUECV016)
           OR (WRK-DIFER-DIAS EQUAL   VINFER-PARM-RURAL OF RUECV016)
              PERFORM 3240-OPEN-CSR01-RUECB09B
              IF SQLCODE EQUAL +100
                 PERFORM 3260-CLOSE-CSR01-RUECB09B
                 GO TO 6000-99-FIM
              END-IF
              PERFORM 7200-GERAR-ACUMB09X
              PERFORM 3260-CLOSE-CSR01-RUECB09B
              GO TO 6000-99-FIM
           ELSE
              PERFORM 3240-OPEN-CSR01-RUECB09B
              PERFORM 7100-VER-B09B
              IF SQLCODE EQUAL +100
                 PERFORM 3260-CLOSE-CSR01-RUECB09B
                 GO TO 6000-99-FIM
              END-IF
              PERFORM 6100-GERAR-ARQACUMU
              PERFORM 3260-CLOSE-CSR01-RUECB09B
              GO TO 6000-99-FIM
           END-IF.

      *---------------------------------------------------------------*
       6000-99-FIM. EXIT.

      *---------------------------------------------------------------*
       6100-GERAR-ARQACUMU  SECTION.
      *---------------------------------------------------------------*

           PERFORM 3270-OBTER-DADOS-B099.

MF3107     IF  SQLCODE  EQUAL  +100
MF3107         GO  TO  6100-99-FIM
MF3107     END-IF.

           PERFORM 6110-FORMATA-CONTROLE.

           PERFORM 6120-FORMATA-CONTRATO.

      * FORMATA EMITENTE
           MOVE 'T'                             TO CTPO-RESP-PSSOA
                                                OF RUECB014.

           PERFORM 3080-SELECT-RUECB014.


           PERFORM 6140-FORMATA-DESTFIN.

      * FORMATA PRODUTO CONSORCIO
           MOVE 'N'              TO  WRK-CODE100-B09G.

MF0613     MOVE ZEROS              TO WRK-SEQREG.
           PERFORM 3090-OPEN-CSR01-RUECB09G.
           PERFORM VARYING  WRK-IND-B09G     FROM 1 BY 1
                   UNTIL WRK-CODE100-B09G  EQUAL 'S'
                   PERFORM 3100-FETCH-CSR01-RUECB09G
                   PERFORM 7250-FORMATA-PRODCONS
           END-PERFORM.

           MOVE ZEROS TO WRK-SEQREG.
           MOVE 'N'   TO WRK-CODE100-B09G.
           MOVE ZEROS TO WRK-IND-B09G.
           PERFORM 3110-CLOSE-CSR01-RUECB09G.

           PERFORM 3700-ACESSAR-RUCAB004-TAXA


      * FORMATA PROPRIETARIO
           MOVE 'N'              TO  WRK-CODE100-B014.

           MOVE 'P'                             TO CTPO-RESP-PSSOA
                                                OF RUECB014.
           PERFORM 3140-OPEN-CSR01-RUECB014.
           PERFORM VARYING  WRK-IND-B014     FROM 1 BY 1
                   UNTIL WRK-CODE100-B014  EQUAL 'S'
                   PERFORM 3150-FETCH-CSR01-RUECB014
                   PERFORM 7270-FORMATA-PROPRIET
           END-PERFORM.

           MOVE 'N'   TO WRK-TEMPROPR-B014.
           MOVE ZEROS TO WRK-SEQREG.
           MOVE 'N'   TO WRK-CODE100-B014.
           MOVE ZEROS TO WRK-IND-B014.
           PERFORM 3160-CLOSE-CSR01-RUECB014.

           PERFORM 6180-FORMATA-TPGARAN.


      * FORMATA PARCELA

           PERFORM 3030-OPEN-CSR02-RUCAB004.
           PERFORM VARYING  WRK-IND-B004     FROM 1 BY 1
                   UNTIL WRK-CODE100-B004  EQUAL 'S'
                   PERFORM 3040-SELECT-RUCAB004
                   PERFORM 6200-FORMATA-PARCELA
           END-PERFORM.

           MOVE ZEROS TO WRK-SEQREG.
           MOVE ZEROS TO WRK-NUM-PARCELA.
           MOVE 'N'   TO WRK-CODE100-B004.
           MOVE ZEROS TO WRK-IND-B004.
           PERFORM 3050-CLOSE-CSR02-RUCAB004.

           PERFORM 7310-FORMATA-FORNECED.

           PERFORM 7320-FORMATA-DATAMOV.

      *---------------------------------------------------------------*
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       6110-FORMATA-CONTROLE           SECTION.
      *---------------------------------------------------------------*

           MOVE SPACES  TO RUEC81-REGISTRO.

      * FORMATA CHAVE
           MOVE CCONTR             OF RUFIB002
                                   TO RUEC81-CCONTR.

           MOVE ZEROS              TO RUEC81-TPREG.

           MOVE ZEROS              TO RUEC81-SEQREG.

      * FORMATA DADOS

           MOVE CREFT-RECOR        OF RUFIB002
                                   TO RUEC81-CREFT-RECOR.

                                   TO RUEC81-DIMPRE-4VIAS.

           PERFORM 7211-FORMATA-CSIT.

           WRITE FD-REG-ARQACUMU  FROM  RUEC81-REGISTRO.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.
           PERFORM 0120-TESTAR-FS-ARQACUMU.

           ADD   1            TO  ACU-GRAVA-ARQACUMU.

      *---------------------------------------------------------------*
       6110-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       6120-FORMATA-CONTRATO           SECTION.
      *---------------------------------------------------------------*

           MOVE SPACES  TO RUEC81-REGISTRO.

           MOVE CCONTR             OF RUFIB002
                                   TO RUEC81-CCONTR.

           MOVE 01                 TO RUEC81-TPREG.

           MOVE ZEROS              TO RUEC81-SEQREG.

      * FORMATA DADOS

           MOVE CTPO-CDULA-RECOR   OF RUFIB002  TO WRK-COD3-CHAR.
           MOVE WRK-COD3-CHAR1     TO RUEC81-TIPO-CEDULA.

           MOVE DIMPRE-CONTR-RURAL OF RUFIB002
                                   TO RUEC81-DEMISSAO.

           MOVE DVCTO-CONTR        OF RUFIB002
                                   TO RUEC81-DVCTO-CTR-RECOR.

           MOVE CCDULA-RECOR       OF RUECB099
                                   TO RUEC81-NUM-CEDULA.

           MOVE CTPO-GARNT-RECOR OF RURCB018
                                        TO WRK-CTPO-GARNT-RECOR.
           MOVE WRK-CTPO-GARNT-X        TO RUEC81-TIPO-CRED.

           MOVE VFINAN             OF RUFIB002
                                   TO WRK-VFINAN-CSINAL.
           MOVE WRK-VFINAN-SSINAL  TO RUEC81-VFINAN-RECOR.


           MOVE CCATEG-PROTR-RECOR OF RUECB099
                                   TO RUEC81-CATEG-EMIT.

           WRITE FD-REG-ARQACUMU  FROM  RUEC81-REGISTRO.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.
           PERFORM 0120-TESTAR-FS-ARQACUMU.

           ADD   1            TO  ACU-GRAVA-ARQACUMU.

      *---------------------------------------------------------------*
       6120-99-FIM. EXIT.

      *---------------------------------------------------------------*
       6140-FORMATA-DESTFIN            SECTION.
      *---------------------------------------------------------------*

           MOVE SPACES  TO RUEC81-REGISTRO.

      * FORMATA CHAVE
           MOVE CCONTR             OF RUFIB002
                                   TO RUEC81-CCONTR.

           MOVE 05                 TO RUEC81-TPREG.

           MOVE ZEROS              TO RUEC81-SEQREG.

      * FORMATA DADOS
           PERFORM 3120-SELECT-RURCB000.
           MOVE CPROG-RURAL        OF RURCB000
                                   TO RUEC81-CPROG-RECOR.

EG0615     PERFORM 6141-FORMATA-SUBPROG
           PERFORM 3130-SELECT-RURCB028.
           MOVE CPLANO-REFIN-RECOR OF RURCB028
                                   TO RUEC81-FONTE-REC.

           MOVE CMUN-RURAL        OF RUFIB002
                                   TO RUEC81-CMUN-RECOR.

           WRITE FD-REG-ARQACUMU  FROM  RUEC81-REGISTRO.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.
           PERFORM 0120-TESTAR-FS-ARQACUMU.

           ADD   1            TO  ACU-GRAVA-ARQACUMU.

      *---------------------------------------------------------------*
       6140-99-FIM. EXIT.
      *---------------------------------------------------------------*

EG0615*---------------------------------------------------------------*
EG0615 6141-FORMATA-SUBPROG            SECTION.
EG0615*---------------------------------------------------------------*
EG0615     MOVE DIMPRE-CONTR-RURAL     OF RUFIB002
EG0615                                 TO WRK-DATA-DB2.
EG0615     MOVE WRK-DIA-DB2            TO WRK-DIA-ISO.
EG0615     MOVE WRK-MES-DB2            TO WRK-MES-ISO.
EG0615     MOVE WRK-ANO-DB2            TO WRK-ANO-ISO.
EG0615
EG0615     IF  WRK-DATA-ISO            LESS LNK-DATA
EG0615         MOVE SPACES             TO  RUEC81-CSUB-PROG-RECOR
EG0615         GO TO 6141-99-FIM
EG0615     END-IF.
EG0615
EG0615     INITIALIZE RUECW824-REGISTRO.
EG0615
EG0615     MOVE CORIGE-REC             OF  RUFIB002
EG0615                                 TO  RUECW824-E-CORIGE-REC
EG0615     MOVE CFNALD-RURAL           OF  RUFIB002
EG0615                                 TO  RUECW824-E-CFNALD-RURAL
EG0615     MOVE CPRODT-RURAL           OF  RUFIB002
EG0615                                 TO  RUECW824-E-CPRODT-RURAL
EG0615     MOVE SPACES                 TO  RUECW824-E-RESERVA
EG0615
EG0615
EG0615     CALL  WRK-MODULO        USING   RUECW824-COPYBOOK
EG0615                                     ERRO-AREA
EG0615                                     SQLCA.
EG0615
EG0615     IF  RUECW824-COD-RETORNO    NOT EQUAL ZEROS
EG0615         PERFORM 9999-ROTINA-ERRO
EG0615     END-IF.
EG0615
EG0615     MOVE RUECW824-S-SUB-PROG-RURAL
EG0615                                 TO  RUEC81-CSUB-PROG-RECOR.
EG0615
EG0615*----------------------------------------------------------------*
EG0615 6141-99-FIM.                    EXIT.
EG0615*----------------------------------------------------------------*
EG0615
      *---------------------------------------------------------------*
       6160-FORMATA-EMPREEND           SECTION.
      *---------------------------------------------------------------*

           PERFORM 7261-CHAMAR-RUEC8800.
           MOVE SPACES  TO RUEC81-REGISTRO.

      * FORMATA CHAVE
           MOVE CCONTR             OF RUFIB002
                                   TO RUEC81-CCONTR.

           MOVE 13                 TO RUEC81-TPREG.

           MOVE ZEROS              TO RUEC81-SEQREG.

      * FORMATA DADOS

           MOVE RUEC58-EMPREEND    TO RUEC81-CEMPTO-RECOR.

           MOVE RUEC58-SIST-PROD   TO RUEC81-SIST-PROD.
           IF  RUEC81-SIST-PROD(10:02)  EQUAL SPACES OR LOW-VALUES
               MOVE '00'           TO RUEC81-SIST-PROD(10:02)
           END-IF

           MOVE VFINAN             OF RUFIB002
                                   TO WRK-VFINAN-CSINAL.

           MOVE VREC-PPRIO         OF RUFIB002
                                   TO WRK-VREC-PPRIO-CSINAL.
           MOVE WRK-VREC-PPRIO-SSINAL TO RUEC81-VL-RECPPRIO.

           MOVE WRK-VALOR-TX       TO RUEC81-PERC-FINANC.

           MOVE ZEROS              TO RUEC81-TIP-ENC-FIN.

           IF  CUND-MEDD-FINCD  OF RUFIB002  EQUAL  1 OR 25
               MOVE QUND-FINCD-RURAL OF RUFIB002 TO WRK-QUND-FINCD11
               MOVE WRK-QUND-FINCD9     TO RUEC81-AREA-FINCD
               MOVE ZEROS               TO RUEC81-ITEM-FINCD
           ELSE
               MOVE ZEROS                 TO RUEC81-AREA-FINCD
               MOVE QUND-FINCD-RURAL      OF RUFIB002
                                          TO WRK-QUND-FINCD-CSINAL
               MOVE WRK-QUND-FINCD-SSINAL TO RUEC81-ITEM-FINCD

           END-IF.

                                     TO WRK-QUND-PROD-CSINAL.
           MOVE WRK-QUND-PROD-SSINAL TO RUEC81-PROD-PROVV.

MF0513     MOVE  DINIC-SAFRA  OF RUECB099  TO WRK-COD6-NUM.
           MOVE  WRK-COD6-CHAR      TO WRK-AUX6-CHAR.
           MOVE  WRK-AUX6-CHAR4     TO WRK-DINIC-SAFRA.
MF0513     MOVE  DFIM-SAFRA   OF RUECB099  TO WRK-COD6-NUM.
           MOVE  WRK-COD6-CHAR      TO WRK-AUX6-CHAR.
           MOVE  WRK-AUX6-CHAR4     TO WRK-DFIM-SAFRA.
           MOVE  WRK-SAFRA          TO RUEC81-SAFRA.

           MOVE 9                  TO RUEC81-TIP-GARNT.

           WRITE FD-REG-ARQACUMU  FROM  RUEC81-REGISTRO.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.
           PERFORM 0120-TESTAR-FS-ARQACUMU.

           ADD   1            TO  ACU-GRAVA-ARQACUMU.

      *---------------------------------------------------------------*
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       6180-FORMATA-TPGARAN            SECTION.
      *---------------------------------------------------------------*

           MOVE SPACES  TO RUEC81-REGISTRO.

      * FORMATA CHAVE
           MOVE CCONTR             OF RUFIB002
                                   TO RUEC81-CCONTR.

           MOVE 16                 TO RUEC81-TPREG.

           MOVE ZEROS              TO RUEC81-SEQREG.

      * FORMATA DADOS
           MOVE 9                  TO RUEC81-TPGARNT.

           WRITE FD-REG-ARQACUMU  FROM  RUEC81-REGISTRO.

           PERFORM 0120-TESTAR-FS-ARQACUMU.

           ADD   1            TO  ACU-GRAVA-ARQACUMU.

      *---------------------------------------------------------------*
       6180-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       6190-FORMATA-RECEITA            SECTION.
      *---------------------------------------------------------------*

           MOVE SPACES  TO RUEC81-REGISTRO.

      * FORMATA CHAVE
           MOVE CCONTR             OF RUFIB002
                                   TO RUEC81-CCONTR.

           MOVE 18                 TO RUEC81-TPREG.

           MOVE ZEROS              TO RUEC81-SEQREG.
      * FORMATA DADOS
           PERFORM  6191-CALCULA-VRECTA-BRUTA.

MF0613     MOVE VRECTA-BRUTA-RECOR OF RUECB099
MF0613                             TO WRK-VRECTA-CSINAL.
MF0613     MOVE WRK-VRECTA-SSINAL  TO RUEC81-RECTA-BRUTA.

           WRITE FD-REG-ARQACUMU  FROM  RUEC81-REGISTRO.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.
           PERFORM 0120-TESTAR-FS-ARQACUMU.

           ADD   1            TO  ACU-GRAVA-ARQACUMU.

      *---------------------------------------------------------------*
       6190-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       6191-CALCULA-VRECTA-BRUTA            SECTION.
      *---------------------------------------------------------------*
           MOVE VRECTA-BRUTA-EMPTO OF RUFIB002 TO WRK-VRECTA-BRUTA.

           IF  CCTRO-CUSTO  OF  RUFIB002  EQUAL  'MASS'
               MOVE  VFINAN OF RUFIB002 TO WRK-VRECTA-BRUTA
               GO  TO  6191-99-FIM
           END-IF.

           IF  RUEC58-EMPREEND(1:2)  NOT EQUAL  '12'
               GO  TO  6191-99-FIM
           END-IF.

           MOVE CCONTR  OF RUFIB002  TO CCONTR    OF RUECB007.
           EXEC SQL
               SELECT  CCLI_UNIC_BDSCO
                 INTO :RUECB007.CCLI-UNIC-BDSCO
               FROM    DB2PRD.CEDULA_TEMPR
               WHERE   CCONTR           = :RUECB007.CCONTR
           END-EXEC.

MF0513     IF (SQLCODE           NOT EQUAL ZEROS AND +100 AND -811) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 'CEDULA_TEMPR     ' TO ERR-DBD-TAB
               MOVE 'SELECT'            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '6191'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

MF0513     IF  SQLCODE                  EQUAL +100 OR -811
MF0513         GO  TO  6191-99-FIM
MF0513     END-IF.

           MOVE CCLI-UNIC-BDSCO OF RUECB007 TO CCLUB    OF RUECB063.

           EXEC SQL
             SELECT  HULT_EFTVC_ALT
             INTO  :RUECB063.HULT-EFTVC-ALT
             FROM   DB2PRD.TRSUMO_CLI_RURAL
             WHERE CCLUB              = :RUECB063.CCLUB
               AND CSIT_CAD_CLI       = 'V'
           END-EXEC.
           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'TRSUMO_CLI_RURAL ' TO ERR-DBD-TAB
               MOVE 'SELECT'            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '619A'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                  EQUAL +100
               GO  TO  6191-99-FIM
           END-IF.

           MOVE CCLI-UNIC-BDSCO OF RUECB007 TO CCLUB    OF RUECB061.
           MOVE HULT-EFTVC-ALT  OF RUECB063 TO
                                       HULT-ATULZ-EFETU OF RUECB061.
           MOVE CPRODT-RURAL  OF RUFIB002 TO CPRODT-RURAL OF RUECB061.

           EXEC SQL
                 INTO :RUECB061.VRENDA-AGROP
               FROM    DB2PRD.TRENDA_AGROP_PROTR
               WHERE   CCLUB            = :RUECB061.CCLUB
                 AND   HULT_ATULZ_EFETU < :RUECB061.HULT-ATULZ-EFETU
                 AND   CPRODT_RURAL     = :RUECB061.CPRODT-RURAL
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'TRENDA_AGROP_PROTR' TO ERR-DBD-TAB
               MOVE 'SELECT'            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '619B'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                  EQUAL +100
               GO  TO  6191-99-FIM
           END-IF.
      * CALCULO
      * ***** ATIVIDADE = AGRICOLA

           IF  RUEC58-EMPREEND(1:2)  EQUAL  '12'
               COMPUTE WRK-VRECTA-BRUTA = QUND-FINCD-RURAL OF RUFIB002 *
                  QUND-PROD-PROVV OF RUFIB002 * VRENDA-AGROP OF RUECB061
           END-IF.

      *---------------------------------------------------------------*
       6191-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       6200-FORMATA-PARCELA            SECTION.
      *---------------------------------------------------------------*

           IF SQLCODE EQUAL +100
              GO TO 6200-99-FIM
           END-IF.

           MOVE SPACES  TO RUEC81-REGISTRO.
      * FORMATA CHAVE
           MOVE CCONTR             OF RUFIB002
                                   TO RUEC81-CCONTR.

           MOVE 20                 TO RUEC81-TPREG.

           ADD  1                  TO WRK-SEQREG.
           MOVE WRK-SEQREG         TO RUEC81-SEQREG.

      * FORMATA DADOS

           ADD  1   TO   WRK-NUM-PARCELA.
           MOVE WRK-NUM-PARCELA    TO RUEC81-NUM-PARC.

           MOVE B004-DTEVENTO      OF RUCAB004
                                   TO RUEC81-DVCTO-PAR-RECOR.

           MOVE B004-VLPRICOR      OF RUCAB004
                                   TO RUEC81-VLPRIN-RECOR.

           WRITE FD-REG-ARQACUMU  FROM  RUEC81-REGISTRO.
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.
           PERFORM 0120-TESTAR-FS-ARQACUMU.

           ADD   1            TO  ACU-GRAVA-ARQACUMU.

      *---------------------------------------------------------------*
       6200-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *----------------------------------------------------------------*
       7000-CALCDIFE-DATA   SECTION.
      *----------------------------------------------------------------*

           MOVE DIMPRE-CONTR-RURAL OF RUFIB002  TO WRK-DATA.
           MOVE WRK-DATA-DIA   TO  WRK-DTINF-DIA.
           MOVE WRK-DATA-MES   TO  WRK-DTINF-MES.
           MOVE WRK-DATA-ANO   TO  WRK-DTINF-ANO.

           CALL WRK-MODULO-BRAD0015  USING  WRK-DATA-INFERIOR
                                            WRK-DATA-SUPERIOR
                                            WRK-DIFER-DIAS.
           IF  RETURN-CODE  EQUAL  4
               DISPLAY 'UMA DAS DATAS OU AMBAS SAO INCONSISTENTES'
               DISPLAY 'DATA SUPERIOR = ' WRK-DATA-SUPERIOR
               DISPLAY 'DATA INFERIOR = ' WRK-DATA-INFERIOR
               PERFORM  9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       7000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       7100-VER-B09B        SECTION.
      *---------------------------------------------------------------*
      * LER NOVAMENTE B09B                                            *
      *---------------------------------------------------------------*
       7100-01-LEROUTRO.

           PERFORM 3250-FETCH-CSR01-RUECB09B.

           IF SQLCODE EQUAL +100
           END-IF.

           IF CMSGEM-RECOR OF RUECB09B  EQUAL  'COR0002  '
              ADD  1  TO  ACU-DESPR-B09B
              GO TO  7100-01-LEROUTRO
           END-IF.

MF0914     IF  CSIT-MSGEM   OF RUECB09B  EQUAL  3 OR 4
MF0914         CONTINUE
MF0914     ELSE
MF0914         IF  CSIT-MSGEM OF RUECB09B EQUAL 5
MF0914         AND CCONTR-CREDT-RURAL OF RUECB09B NOT LESS    539221
MF0914         AND CCONTR-CREDT-RURAL OF RUECB09B NOT GREATER 557201
MF0914             CONTINUE
MF0914         ELSE
                   ADD  1  TO  ACU-DESPR-B09B
                   GO TO  7100-01-LEROUTRO
               END-IF
           END-IF.

           MOVE CCONTR-CREDT-RURAL   OF RUECB09B
           IF WRK-CONTRATU-SSINAL  EQUAL  WRK-CONTR
              ADD  1  TO  ACU-DESPR-B09B
              GO TO  7100-99-FIM
           ELSE
              MOVE CCONTR-CREDT-RURAL OF RUECB09B  TO  WRK-CONTR
           END-IF.

      *----------------------------------------------------------------*
       7100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       7200-GERAR-ACUMB09X  SECTION.
      *---------------------------------------------------------------*

           PERFORM 3270-OBTER-DADOS-B099.

MF3107     IF  SQLCODE  EQUAL  +100
MF3107         GO  TO  7200-99-FIM
MF3107     END-IF.


           PERFORM 7220-FORMATA-CONTRATO.

      * FORMATA EMITENTE
           MOVE 'T'                             TO CTPO-RESP-PSSOA
                                                OF RUECB014.

           PERFORM 3080-SELECT-RUECB014.

           PERFORM 7230-FORMATA-EMITENTE.

           PERFORM 7240-FORMATA-DESTFIN.

      * FORMATA PRODUTO CONSORCIO
           MOVE 'N'              TO  WRK-CODE100-B09G.

MF0613     MOVE ZEROS              TO WRK-SEQREG.
           PERFORM 3090-OPEN-CSR01-RUECB09G.
           PERFORM VARYING  WRK-IND-B09G     FROM 1 BY 1
                   UNTIL WRK-CODE100-B09G  EQUAL 'S'
                   PERFORM 3100-FETCH-CSR01-RUECB09G
           END-PERFORM.

           MOVE ZEROS TO WRK-SEQREG.
           MOVE 'N'   TO WRK-CODE100-B09G.
           MOVE ZEROS TO WRK-IND-B09G.
           PERFORM 3110-CLOSE-CSR01-RUECB09G.

           PERFORM 3700-ACESSAR-RUCAB004-TAXA

           PERFORM 7260-FORMATA-EMPREEND.

      * FORMATA PROPRIETARIO
           MOVE 'N'              TO  WRK-CODE100-B014.

           MOVE 'P'                             TO CTPO-RESP-PSSOA
                                                OF RUECB014.
           PERFORM 3140-OPEN-CSR01-RUECB014.
           PERFORM VARYING  WRK-IND-B014     FROM 1 BY 1
                   UNTIL WRK-CODE100-B014  EQUAL 'S'
                   PERFORM 3150-FETCH-CSR01-RUECB014
                   PERFORM 7270-FORMATA-PROPRIET

           MOVE 'N'   TO WRK-TEMPROPR-B014.
           MOVE ZEROS TO WRK-SEQREG.
           MOVE 'N'   TO WRK-CODE100-B014.
           MOVE ZEROS TO WRK-IND-B014.
           PERFORM 3160-CLOSE-CSR01-RUECB014.

           PERFORM 7280-FORMATA-TPGARAN.

           PERFORM 7290-FORMATA-RECEITA.

      * FORMATA PARCELA
           MOVE 'N'              TO  WRK-CODE100-B09D.

           PERFORM 3170-OPEN-CSR01-RUECB09D.
           PERFORM VARYING  WRK-IND-B09D     FROM 1 BY 1
                   UNTIL WRK-CODE100-B09D  EQUAL 'S'
                   PERFORM 3180-SELECT-RUECB09D
                   PERFORM 7300-FORMATA-PARCELA
           END-PERFORM.

           MOVE 'N'   TO WRK-CODE100-B09D.
           MOVE ZEROS TO WRK-IND-B09D.
           PERFORM 3190-CLOSE-CSR01-RUECB09D.

           PERFORM 7310-FORMATA-FORNECED

           PERFORM 7320-FORMATA-DATAMOV.

      *----------------------------------------------------------------*
       7200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       7210-FORMATA-CONTROLE           SECTION.
      *---------------------------------------------------------------*

           MOVE SPACES  TO RUEC81-REGISTRO.

      * FORMATA CHAVE
           MOVE CCONTR-CREDT-RURAL OF RUECB09B
                                   TO RUEC81-CCONTR.
           MOVE ZEROS              TO RUEC81-TPREG.

           MOVE ZEROS              TO RUEC81-SEQREG.

      * FORMATA DADOS

           MOVE CREFT-RECOR        OF RUECB09B
                                   TO RUEC81-CREFT-RECOR.

           MOVE DIMPRE-CONTR-RURAL OF RUFIB002
                                   TO RUEC81-DIMPRE-4VIAS.

           PERFORM 7211-FORMATA-CSIT.

           WRITE FD-REG-ARQACUMU  FROM  RUEC81-REGISTRO.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.
           PERFORM 0120-TESTAR-FS-ARQACUMU.

           ADD   1            TO  ACU-GRAVA-ARQACUMU.

       7210-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       7211-FORMATA-CSIT               SECTION.
      *---------------------------------------------------------------*

           MOVE  1  TO RUEC81-CSIT-RECOR.

           MOVE ZEROS              TO WRK-NENVIO-RECOR.
           EXEC SQL
             SELECT  MIN (NENVIO_RECOR)
               INTO :WRK-NENVIO-RECOR
             FROM  DB2PRD.THIST_ENVIO_RECOR
               WHERE CCONTR_CREDT_RURAL = :RUFIB002.CCONTR
                 AND CSIT_MSGEM         = 4
                 AND CMSGEM_RECOR       = 'COR0001'
           END-EXEC.

           IF (SQLCODE           NOT EQUAL   +0 AND -305)    OR
              (SQLWARN0              EQUAL  'W')
               MOVE 'THIST_ENVIO_RECOR '     TO   ERR-DBD-TAB
               MOVE 'SELECT  '               TO   ERR-FUN-COMANDO
               MOVE SQLCODE                  TO   ERR-SQL-CODE
               MOVE '7211'                   TO   ERR-LOCAL
               MOVE SPACES                   TO   ERR-SEGM
               PERFORM   9999-ROTINA-ERRO
           END-IF.

           EXEC SQL
           SELECT DVCTO_CONTR
           INTO   :WRK-DVCTO-CONTR99
                       :WRK-B099-DVCTO-NULL
           FROM  DB2PRD.TCONTR_ENVIO_RECOR
           WHERE CCONTR_CREDT_RURAL   = :RUFIB002.CCONTR
           AND   NENVIO_RECOR         = :WRK-NENVIO-RECOR
           END-EXEC.

           IF (SQLCODE   NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0      EQUAL 'W')
               MOVE 'DB2'                TO ERR-TIPO-ACESSO
               MOVE 'TCONTR_ENVIO_RECOR' TO ERR-DBD-TAB
               MOVE SQLCODE              TO ERR-SQL-CODE
               MOVE '7212'               TO ERR-LOCAL
               MOVE SPACES               TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  WRK-B099-DVCTO-NULL  LESS  0
               MOVE '01.01.0001'         TO WRK-DVCTO-CONTR99
           END-IF.

           IF (SQLCODE       EQUAL  +100)
               MOVE DVCTO-CONTR OF RUFIB002
                                         TO WRK-DVCTO-CONTR99
           END-IF.

           MOVE WRK-DVCTO-ANO99          TO WRK-DVCTO99-MIN(1:4)
           MOVE WRK-DVCTO-MES99          TO WRK-DVCTO99-MIN(5:2)
           MOVE WRK-DVCTO-DIA99          TO WRK-DVCTO99-MIN(7:2).

           MOVE DVCTO-CONTR OF RUFIB002  TO WRK-DVCTO-CONTR02

           MOVE WRK-DVCTO-MES02          TO WRK-DVCTO02(5:2)
           MOVE WRK-DVCTO-DIA02          TO WRK-DVCTO02(7:2).

           IF  WRK-DVCTO99-MIN  EQUAL  WRK-DVCTO02
               GO  TO  7211-99-FIM
           END-IF.

           IF  WRK-DVCTO02  LESS    WRK-DVCTO99-MIN
               MOVE  4  TO RUEC81-CSIT-RECOR
           ELSE
               MOVE  3  TO RUEC81-CSIT-RECOR
           END-IF.

      *---------------------------------------------------------------*
       7211-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       7220-FORMATA-CONTRATO           SECTION.
      *---------------------------------------------------------------*


      * FORMATA CHAVE
           MOVE CCONTR-CREDT-RURAL OF RUECB09B
                                   TO RUEC81-CCONTR.

           MOVE 01                 TO RUEC81-TPREG.

           MOVE ZEROS              TO RUEC81-SEQREG.

      * FORMATA DADOS

           MOVE CTPO-CDULA-RECOR   OF RUFIB002  TO WRK-COD3-CHAR.
           MOVE WRK-COD3-CHAR1     TO RUEC81-TIPO-CEDULA.

           MOVE DIMPRE-CONTR-RURAL OF RUFIB002
                                   TO RUEC81-DEMISSAO.

           MOVE DVCTO-CONTR        OF RUFIB002
                                   TO RUEC81-DVCTO-CTR-RECOR.

           MOVE CCDULA-RECOR       OF RUECB099

           PERFORM 3060-SELECT-RURCB018.
           MOVE CTPO-GARNT-RECOR OF RURCB018
                                        TO WRK-CTPO-GARNT-RECOR.
           MOVE WRK-CTPO-GARNT-X        TO RUEC81-TIPO-CRED.

           MOVE VFINAN             OF RUECB099
                                   TO WRK-VFINAN-CSINAL.
           MOVE WRK-VFINAN-SSINAL  TO RUEC81-VFINAN-RECOR.

           MOVE CCATEG-PROTR-RECOR OF RUECB099
                                   TO RUEC81-CATEG-EMIT.

           WRITE FD-REG-ARQACUMU  FROM  RUEC81-REGISTRO.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.
           PERFORM 0120-TESTAR-FS-ARQACUMU.

           ADD   1            TO  ACU-GRAVA-ARQACUMU.

      *---------------------------------------------------------------*
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       7230-FORMATA-EMITENTE           SECTION.
      *---------------------------------------------------------------*

           MOVE SPACES  TO RUEC81-REGISTRO.

      * FORMATA CHAVE
           MOVE CCONTR             OF RUFIB002
                                   TO RUEC81-CCONTR.

           MOVE 03                 TO RUEC81-TPREG.

           ADD  1                  TO WRK-SEQREG.
           MOVE WRK-SEQREG         TO RUEC81-SEQREG.

      * FORMATA DADOS

      * GRUPO EMIT TAB B09E
           IF  CTPO-PSSOA  EQUAL  1
               MOVE CCGC-CPF           OF  RUECB014
                                       TO  WRK-COD9-NUM
               MOVE CFLIAL-CGC         OF  RUECB014
                                       TO  WRK-COD5-NUM
               MOVE CCTRL-CPF-CGC      OF  RUECB014
                                       TO  WRK-COD3-NUM
               MOVE WRK-COD9-NUM       TO  RUEC81-CPF-CNPJ-EMIT
               MOVE WRK-COD5-NUM4      TO  RUEC81-FIL-CNPJ-EMIT
               MOVE WRK-COD3-NUM2      TO  RUEC81-CTRL-EMIT
           ELSE
               MOVE  'J'               TO  RUEC81-TIP-PES-EMIT
               MOVE CCGC-CPF           OF  RUECB014
                                       TO  WRK-COD9-NUM
               MOVE CFLIAL-CGC         OF  RUECB014
                                       TO  WRK-COD5-NUM
               MOVE CCTRL-CPF-CGC      OF  RUECB014
                                       TO  WRK-COD3-NUM
               MOVE WRK-COD9-NUM       TO  RUEC81-CPF-CNPJ-EMIT
               MOVE WRK-COD5-NUM4      TO  RUEC81-FIL-CNPJ-EMIT
               MOVE WRK-COD3-NUM2      TO  RUEC81-CTRL-EMIT
           END-IF.
           WRITE FD-REG-ARQACUMU  FROM  RUEC81-REGISTRO.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.
           PERFORM 0120-TESTAR-FS-ARQACUMU.

           ADD   1            TO  ACU-GRAVA-ARQACUMU.

      *---------------------------------------------------------------*
       7230-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       7240-FORMATA-DESTFIN            SECTION.
      *---------------------------------------------------------------*

           MOVE SPACES  TO RUEC81-REGISTRO.

      * FORMATA CHAVE
           MOVE CCONTR-CREDT-RURAL OF RUECB09B
                                   TO RUEC81-CCONTR.


           MOVE ZEROS              TO RUEC81-SEQREG.

      * FORMATA DADOS
           MOVE CPROG-RURAL        OF RUECB099
                                   TO RUEC81-CPROG-RECOR.

EG0615     PERFORM 6141-FORMATA-SUBPROG
EG0615
           PERFORM 3120-SELECT-RURCB000.
           PERFORM 3130-SELECT-RURCB028.
           MOVE CPLANO-REFIN-RECOR OF RURCB028
                                   TO RUEC81-FONTE-REC.

           MOVE CMUN-REFT-BACEN    OF RUECB099
                                   TO RUEC81-CMUN-RECOR.

           WRITE FD-REG-ARQACUMU  FROM  RUEC81-REGISTRO.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.
           PERFORM 0120-TESTAR-FS-ARQACUMU.
           ADD   1            TO  ACU-GRAVA-ARQACUMU.

      *---------------------------------------------------------------*
       7240-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       7250-FORMATA-PRODCONS           SECTION.
      *---------------------------------------------------------------*

           IF SQLCODE EQUAL +100
              MOVE  'S'  TO  WRK-CODE100-B09G
              GO TO 7250-99-FIM
           END-IF.

           MOVE SPACES  TO RUEC81-REGISTRO.

      * FORMATA CHAVE
           MOVE CCONTR             OF RUFIB002
                                   TO RUEC81-CCONTR.


           ADD  1                  TO WRK-SEQREG.
           MOVE WRK-SEQREG         TO RUEC81-SEQREG.

      * FORMATA DADOS

           MOVE CPRODT-RURAL       OF RUECB09G
                                   TO RUEC81-PROD-CONS.

           WRITE FD-REG-ARQACUMU  FROM  RUEC81-REGISTRO.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.
           PERFORM 0120-TESTAR-FS-ARQACUMU.

           ADD   1            TO  ACU-GRAVA-ARQACUMU.

      *---------------------------------------------------------------*
       7250-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      *---------------------------------------------------------------*

           MOVE SPACES  TO RUEC81-REGISTRO.

      * FORMATA CHAVE
           MOVE CCONTR-CREDT-RURAL OF RUECB09B
                                   TO RUEC81-CCONTR.

           MOVE 13                 TO RUEC81-TPREG.

           MOVE ZEROS              TO RUEC81-SEQREG.

      * FORMATA DADOS

           MOVE CEMPTO-BACEN       OF RUECB099
                                   TO RUEC81-CEMPTO-RECOR.

           MOVE CSIST-PROD         OF RUECB099
                                   TO RUEC81-SIST-PROD
           IF  RUEC81-SIST-PROD(10:02)  EQUAL SPACES OR LOW-VALUES
               MOVE '00'           TO RUEC81-SIST-PROD(10:02)

           MOVE VPCELA-LIBRC            OF RUECB099
                                        TO WRK-VPCELA-LIBRC-CSINAL.
           MOVE WRK-VPCELA-LIBRC-SSINAL TO RUEC81-VL-CREDITO.

           MOVE VPCELA-REC-PPRIO      OF RUECB099
                                      TO WRK-VPCELA-REC-CSINAL.
           MOVE WRK-VPCELA-REC-SSINAL TO  RUEC81-VL-RECPPRIO.

           MOVE WRK-VALOR-TX       TO RUEC81-PERC-FINANC.

           MOVE ZEROS              TO RUEC81-TIP-ENC-FIN.

           IF  CUND-MEDD-FINCD  OF RUFIB002  EQUAL  1 OR 25
               MOVE QUND-FINCD-RURAL OF RUFIB002 TO WRK-QUND-FINCD11
               MOVE WRK-QUND-FINCD9     TO RUEC81-AREA-FINCD
               MOVE ZEROS               TO RUEC81-ITEM-FINCD
           ELSE
               MOVE ZEROS                 TO RUEC81-AREA-FINCD
               MOVE QUND-FINCD-RURAL      OF RUFIB002
                                          TO WRK-QUND-FINCD-CSINAL
           END-IF.

           MOVE QUND-PROD-PROVV      OF RUFIB002
                                     TO WRK-QUND-PROD-CSINAL.
           MOVE WRK-QUND-PROD-SSINAL TO RUEC81-PROD-PROVV.

MF0513     MOVE  DINIC-SAFRA  OF RUECB099  TO WRK-COD6-NUM.
           MOVE  WRK-COD6-CHAR      TO WRK-AUX6-CHAR.
           MOVE  WRK-AUX6-CHAR4     TO WRK-DINIC-SAFRA.
MF0513     MOVE  DFIM-SAFRA   OF RUECB099  TO WRK-COD6-NUM.
           MOVE  WRK-COD6-CHAR      TO WRK-AUX6-CHAR.
           MOVE  WRK-AUX6-CHAR4     TO WRK-DFIM-SAFRA.
           MOVE  WRK-SAFRA          TO RUEC81-SAFRA.

           MOVE CTPO-GARNT-RECOR   OF RUECB099
                                   TO WRK-AUX3-NUM.
           MOVE WRK-AUX3-NUM1      TO RUEC81-TIP-GARNT.

           WRITE FD-REG-ARQACUMU  FROM  RUEC81-REGISTRO.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           ADD   1            TO  ACU-GRAVA-ARQACUMU.

      *---------------------------------------------------------------*
       7260-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       7261-CHAMAR-RUEC8800            SECTION.
      *---------------------------------------------------------------*

           INITIALIZE  RUEC58-AREA.
           MOVE 'RUEC1595'                     TO RUEC58-PGM-CHAMADOR
           MOVE CCONTR             OF RUFIB002 TO RUEC58-CONTRATO
           MOVE CFNALD-RURAL       OF RUFIB002 TO RUEC58-FINALIDADE
           MOVE CATVDD-RURAL       OF RUFIB002 TO RUEC58-ATIVIDADE
           MOVE CMODLD-RURAL       OF RUFIB002 TO RUEC58-MODALIDADE
           MOVE CPRODT-RURAL       OF RUFIB002 TO RUEC58-PRODUTO
           MOVE CVAR-PRODT-RURAL   OF RUFIB002 TO RUEC58-VARIEDADE
           MOVE CCESTA-SAFRA-RURAL OF RUFIB002 TO RUEC58-CESTA-SAFRA
           MOVE CINDCD-ZNMTO       OF RUFIB002 TO RUEC58-ZONEAMENTO
           MOVE CTPO-AGROP-RURAL   OF RUFIB002 TO RUEC58-TIPO-AGROP
           MOVE CTPO-INTGC-RURAL   OF RUFIB002 TO RUEC58-TIPO-INTEG
           MOVE CINDCD-UND-PRDTV   OF RUFIB002 TO RUEC58-GRAO-SEMENTE
           MOVE CTPO-IGCAO-RURAL   OF RUFIB002 TO RUEC58-TIPO-IRRIG
           MOVE CTPO-CTIVO-RURAL   OF RUFIB002 TO RUEC58-TIPO-CULTIVO
           MOVE CCICLO-PROD-RURAL  OF RUFIB002 TO RUEC58-CICLO-PROD
PRI499     MOVE CCLIMT-RURAL-BACEN OF RUFIB002 TO RUEC58-CLIMA-PROD
           MOVE DCDULA             OF RUFIB002 TO RUEC58-DCDULA

           MOVE 'RUEC8800'             TO  WRK-MODULO.
           CALL  WRK-MODULO        USING   RUEC58-AREA.

           IF RUEC58-COD-RET            EQUAL   99
              DISPLAY 'RUEC8800 - ERRO DB2'
              DISPLAY 'RUEC58-COD-RET  = ' RUEC58-COD-RET
              DISPLAY 'RUEC58-MENSAGEM = ' RUEC58-MENSAGEM
              MOVE RUEC58-SQLCA        TO SQLCA
              MOVE RUEC58-ERRO-AREA    TO ERRO-AREA
              PERFORM 9999-ROTINA-ERRO
           END-IF.

              (RUEC58-COD-RET     EQUAL 02)  OR
              (RUEC58-COD-RET     EQUAL 03)  OR
              (RUEC58-COD-RET     EQUAL 04)  OR
              (RUEC58-COD-RET     EQUAL 05)
               DISPLAY 'RUEC8800 - DADOS INVALIDOS/NAO CADASTRADOS'
               DISPLAY 'RUEC8800 - ASSUMIDO EMPREENDIMENTO ZEROS  '
               DISPLAY 'RUEC8800 - CONTRATO = ' CCONTR OF RUFIB002
               DISPLAY 'RUEC58-COD-RET  = ' RUEC58-COD-RET
               DISPLAY 'RUEC58-MENSAGEM = ' RUEC58-MENSAGEM
               MOVE RUEC58-SQLCA        TO SQLCA
               MOVE RUEC58-ERRO-AREA    TO ERRO-AREA
               MOVE '00000000000000'    TO RUEC58-EMPREEND
           END-IF.

      *---------------------------------------------------------------*
       7261-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       7270-FORMATA-PROPRIET           SECTION.
      *---------------------------------------------------------------*
           IF (WRK-CODE100-B014  EQUAL  'S')  AND
              (WRK-TEMPROPR-B014 EQUAL  'S')
MF0513        MOVE  'S'  TO  WRK-CODE100-B014
MF0513        GO TO 7270-99-FIM
MF0513     END-IF.

           MOVE SPACES  TO RUEC81-REGISTRO.

      * FORMATA CHAVE
           MOVE CCONTR             OF RUFIB002
                                   TO RUEC81-CCONTR.

           MOVE 15                 TO RUEC81-TPREG.

           ADD  1                  TO WRK-SEQREG.
           MOVE WRK-SEQREG         TO RUEC81-SEQREG.

      * FORMATA DADOS
           IF  CTPO-PSSOA  OF RUECB014  EQUAL  1
               MOVE  'F'               TO RUEC81-TIP-PES-PROP
               MOVE CCGC-CPF      OF RUECB014 TO WRK-COD9-NUM
               MOVE CCTRL-CPF-CGC OF RUECB014 TO WRK-COD3-NUM
               MOVE WRK-COD9-NUM               TO RUEC81-CPF-CNPJ-PROP
               MOVE WRK-COD5-NUM4              TO RUEC81-FIL-CNPJ-PROP
               MOVE WRK-COD3-NUM2              TO RUEC81-CTRL-PROP
           ELSE
               MOVE  'J'                       TO RUEC81-TIP-PES-PROP
               MOVE CCGC-CPF      OF RUECB014 TO WRK-COD9-NUM
               MOVE CFLIAL-CGC    OF RUECB014 TO WRK-COD5-NUM
               MOVE CCTRL-CPF-CGC OF RUECB014 TO WRK-COD3-NUM
               MOVE WRK-COD9-NUM               TO RUEC81-CPF-CNPJ-PROP
               MOVE WRK-COD5-NUM4              TO RUEC81-FIL-CNPJ-PROP
               MOVE WRK-COD3-NUM2              TO RUEC81-CTRL-PROP
           END-IF.

           WRITE FD-REG-ARQACUMU  FROM  RUEC81-REGISTRO.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.
           PERFORM 0120-TESTAR-FS-ARQACUMU.

           ADD   1            TO  ACU-GRAVA-ARQACUMU.

       7270-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       7280-FORMATA-TPGARAN            SECTION.
      *---------------------------------------------------------------*

           MOVE SPACES  TO RUEC81-REGISTRO.

      * FORMATA CHAVE
           MOVE CCONTR-CREDT-RURAL OF RUECB09B
                                   TO RUEC81-CCONTR.

           MOVE 16                 TO RUEC81-TPREG.

           MOVE ZEROS              TO RUEC81-SEQREG.

      * FORMATA DADOS

           MOVE CTPO-GARNT-RECOR   OF RUECB099
                                   TO WRK-AUX3-NUM.

           WRITE FD-REG-ARQACUMU  FROM  RUEC81-REGISTRO.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.
           PERFORM 0120-TESTAR-FS-ARQACUMU.

           ADD   1            TO  ACU-GRAVA-ARQACUMU.

      *---------------------------------------------------------------*
       7280-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       7290-FORMATA-RECEITA            SECTION.
      *---------------------------------------------------------------*

           MOVE SPACES  TO RUEC81-REGISTRO.

      * FORMATA CHAVE
           MOVE CCONTR-CREDT-RURAL OF RUECB09B
                                   TO RUEC81-CCONTR.
           MOVE 18                 TO RUEC81-TPREG.

           MOVE ZEROS              TO RUEC81-SEQREG.

      * FORMATA DADOS

           MOVE VRECTA-BRUTA-RECOR OF RUECB099
                                   TO WRK-VRECTA-CSINAL.
           MOVE WRK-VRECTA-SSINAL  TO RUEC81-RECTA-BRUTA.


           WRITE FD-REG-ARQACUMU  FROM  RUEC81-REGISTRO.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.
           PERFORM 0120-TESTAR-FS-ARQACUMU.

           ADD   1            TO  ACU-GRAVA-ARQACUMU.

      *---------------------------------------------------------------*
       7290-99-FIM. EXIT.
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
       7300-FORMATA-PARCELA            SECTION.
      *---------------------------------------------------------------*

           IF SQLCODE EQUAL +100
              GO TO 7300-99-FIM
           END-IF.

           MOVE SPACES  TO RUEC81-REGISTRO.

      * FORMATA CHAVE
           MOVE CCONTR-CREDT-RURAL OF RUECB09B
                                   TO RUEC81-CCONTR.

           MOVE 20                 TO RUEC81-TPREG.

           ADD  1                  TO WRK-SEQREG.
           MOVE WRK-SEQREG         TO RUEC81-SEQREG.

      * FORMATA DADOS

                                   TO WRK-NPCELA-CSINAL.
           MOVE WRK-NPCELA-SSINAL  TO RUEC81-NUM-PARC.

           MOVE DVCTO-PCELA-CONTR  OF RUECB09D
                                   TO RUEC81-DVCTO-PAR-RECOR.

           MOVE VPCELA-RURAL       OF RUECB09D
                                   TO RUEC81-VLPRIN-RECOR.

           WRITE FD-REG-ARQACUMU  FROM  RUEC81-REGISTRO.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.
           PERFORM 0120-TESTAR-FS-ARQACUMU.

           ADD   1            TO  ACU-GRAVA-ARQACUMU.

      *---------------------------------------------------------------*
       7300-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      *---------------------------------------------------------------*

            PERFORM  3320-COUNT-RUECB097.

            MOVE WRK-QTDREG-B097  TO  WRK-QTDB097-CSINAL.
            IF WRK-QTDB097  EQUAL  ZEROS
               GO TO 7310-99-FIM
            END-IF.

      * FORMATA NOVOS FORNECEDORES
       7310-00-NOVOFORN.

            INITIALIZE  RUEC79-REGISTRO.

            MOVE  'RUEC1595'              TO RUEC79-PROGRAMA.
            MOVE  CJUNC-DEPDC OF RUFIB002 TO RUEC79-CJUNC-DEPDC.
            MOVE  CCDULA      OF RUFIB002 TO RUEC79-CCDULA.
            MOVE  WRK-NITEM-ENV           TO RUEC79-NITEM-ENV.
            MOVE  999                     TO RUEC79-QTD-PESQ.

            CALL WRK-MODULO-RUEC8822  USING  RUEC79-REGISTRO
                                             WRK-SQLCA.

      ***************************************************************
      * RUEC79-COD-RET:
      *            00 - DADOS RETORNADOS
      *            01 - DADOS INCONSISTENTES
      *            02 - NAO ENCONTROU REGISTROS
      *            99 - PROBLEMAS DB2
      ***************************************************************

           IF (RUEC79-COD-RET   EQUAL  01)
           OR (RUEC79-COD-RET   EQUAL  99)
               DISPLAY 'RETORNO RUEC8822 = ' RUEC79-COD-RET
               DISPLAY '01 - DADOS INCONSISTENTES'
               DISPLAY '99 - PROBLEMAS DB2'
               PERFORM   9999-ROTINA-ERRO
           END-IF.

           IF (RUEC79-COD-RET   EQUAL  02)
               GO TO 7310-99-FIM
           END-IF.
           MOVE RUEC79-QTD-RET   TO  WRK-QTD-SSINAL.
           MOVE WRK-QTD-CSINAL   TO  WRK-QTD-PESQ.

           MOVE  1  TO  WRK-IND-OCOR.
           PERFORM VARYING  WRK-IND-OCOR     FROM 1 BY 1
           UNTIL   WRK-IND-OCOR      GREATER WRK-QTD-PESQ
                   PERFORM 7311-FORMATA-OCORR-FORN
           END-PERFORM.

           IF WRK-QTDB097  GREATER  WRK-AUXTOT
              ADD  999 TO WRK-NITEM-ENV
              ADD  999 TO WRK-AUXTOT-COMP
              MOVE WRK-AUXTOT-COMP TO WRK-AUXTOT
              GO TO  7310-00-NOVOFORN
           END-IF.

           MOVE ZEROS TO WRK-QTDREG-B097.
           MOVE 999   TO WRK-AUXTOT.
           MOVE 999   TO WRK-AUXTOT-COMP.
           MOVE ZEROS TO WRK-NITEM-ENV.
           MOVE ZEROS TO WRK-IND-OCOR.

      *---------------------------------------------------------------*
       7310-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       7311-FORMATA-OCORR-FORN         SECTION.
      *---------------------------------------------------------------*

           MOVE SPACES  TO RUEC81-REGISTRO.

      * FORMATA CHAVE
           MOVE CCONTR             OF RUFIB002
                                   TO RUEC81-CCONTR.

           MOVE 23                 TO RUEC81-TPREG.

           ADD  1                  TO WRK-SEQREG.
           MOVE WRK-SEQREG         TO RUEC81-SEQREG.

      * FORMATA DADOS
               MOVE  'F'               TO  RUEC81-TIP-PES-FORN
               MOVE RUEC79-CCNPJ-PROTR(WRK-IND-OCOR)
                                       TO  RUEC81-CPF-CNPJ-FORN
               MOVE RUEC79-CFLIAL-PROTR(WRK-IND-OCOR)
                                       TO  RUEC81-FIL-CNPJ-FORN
               MOVE RUEC79-CCTRL-PROTR(WRK-IND-OCOR)
                                       TO  RUEC81-CTRL-FORN
           ELSE
               MOVE  'J'               TO  RUEC81-TIP-PES-FORN
               MOVE RUEC79-CCNPJ-PROTR(WRK-IND-OCOR)
                                       TO  RUEC81-CPF-CNPJ-FORN
               MOVE RUEC79-CFLIAL-PROTR(WRK-IND-OCOR)
                                       TO  RUEC81-FIL-CNPJ-FORN
               MOVE RUEC79-CCTRL-PROTR(WRK-IND-OCOR)
                                       TO  RUEC81-CTRL-FORN
           END-IF.

           MOVE RUEC79-DNOTA-FSCAL(WRK-IND-OCOR)  TO RUEC81-DTCOMPRA.

           MOVE RUEC79-VUTLZD-NF(WRK-IND-OCOR)
                                   TO RUEC81-VLCOMPRA.
           MOVE RUEC79-CMUN-RURAL(WRK-IND-OCOR)
                                   TO RUEC81-CMUN-COMPRA.

           MOVE RUEC79-QUND-FINCR(WRK-IND-OCOR)
                                   TO RUEC81-QTD-COMPRA.

           MOVE RUEC79-SAFRA-INI-FIM(WRK-IND-OCOR)
                                   TO RUEC81-SAFRA-COMPRA.

           WRITE FD-REG-ARQACUMU  FROM  RUEC81-REGISTRO.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.
           PERFORM 0120-TESTAR-FS-ARQACUMU.

           ADD   1            TO  ACU-GRAVA-ARQACUMU.

      *---------------------------------------------------------------*
       7311-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      *---------------------------------------------------------------*

           MOVE SPACES  TO RUEC81-REGISTRO.

      * FORMATA CHAVE
           MOVE CCONTR             OF RUFIB002
                                   TO RUEC81-CCONTR.

           MOVE 25                 TO RUEC81-TPREG.

           MOVE ZEROS              TO RUEC81-SEQREG.

      * FORMATA DADOS

           MOVE DMOVTO-RECOR OF RUECB099
                             TO RUEC81-DMOVTO.

           WRITE FD-REG-ARQACUMU  FROM  RUEC81-REGISTRO.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.
           PERFORM 0120-TESTAR-FS-ARQACUMU.
           ADD   1            TO  ACU-GRAVA-ARQACUMU.

      *---------------------------------------------------------------*
       7320-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       9000-DISPLAY-SYSOUT  SECTION.
      *---------------------------------------------------------------*

           CLOSE   ARQDATA
                   ARQACUMU.

           MOVE  WRK-FECHAMENTO        TO  WRK-OPERACAO.
           PERFORM 0100-TESTAR-FILE-STATUS.

           DISPLAY '************************** RUEC1595 ****************
      -            '***********'.
           DISPLAY '*'.
           DISPLAY '* TOTAL REGS LIDOS RUFIB002.........................
      -            ': '  ACU-LIDOS-B002.
           DISPLAY '* TOTAL REGS GERADOS ARQUIVO ARQACUMU...............
      -            ': '  ACU-GRAVA-ARQACUMU.
           DISPLAY '*'.
           DISPLAY '************************** RUEC1595 ****************
      -            '***********'.

      *---------------------------------------------------------------*
       9000-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       9999-ROTINA-ERRO  SECTION.
      *---------------------------------------------------------------*

           MOVE 'RUEC1595'     TO  ERR-PGM.

           CALL  'POOL7100'     USING    WRK-BATCH
                                         ERRO-AREA
                                         SQLCA
           GOBACK.

       9999-99-FIM. EXIT.
      *----------------------------------------------------------------*
