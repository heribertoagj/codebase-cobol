      *===============================================================*
       IDENTIFICATION DIVISION.
      *===============================================================*
       PROGRAM-ID.    RUEC1550.
       AUTHOR.        BSI TECNOLOGIA.

      *===============================================================*
      *                                                               *
      *      PROGRAMA     : RUEC1550                                  *
      *      PROGRAMADOR  : BSI TECNOLOGIA.                           *
      *      ANALISTA     : BSI TECNOLOGIA.                           *
      *      DATA         : 23/11/2012                                *
      *                                                               *
      *      OBJETIVO     :                                           *
      *      GERAR ARQUIVO(COR00001) TRANSITO PARA ROTINA FIMA.       *
      *      GERAR ARQUIVO(COR00001) TRANSITO PARA ROTINA FIMA.       *
      *      GERAR ARQUIVO(COR00001) TRANSITO PARA ROTINA FIMA.       *
      *                                                               *
      *      DB2:                                                     *
      *        RUFIB002 - DB2PRD.CONTRATO_RURAL                       *
      *        RUECB09B - DB2PRD.THIST_ENVIO_RECOR                    *
      *        RUECB09E - DB2PRD.TPSSOA_ENVIO_RECOR                   *
      *                                                               *
      *===============================================================*
EG0615*===============================================================*
EG0615*                                                               *
EG0615*      ALTERACAO  - BSI TECNOLOGIA                              *
EG0615*                                                               *
EG0615*      ANALISTA     : BSI TECNOLOGIA.                           *
EG0615*      DATA         : JUNHO/2015                                *
EG0615*                                                               *
EG0615*      OBJETIVO     :                                           *
EG0615*      SICOR 4.06 - INCLUSAO DO CAMPO SUBPROGRAMA               *
EG0615*                                                               *
EG0615*===============================================================*
WIP001*===============================================================*
-     *                                                               *
-     *      ALTERACAO  - WIPRO                                       *
-     *                                                               *
-     *      ANALISTA     : WIPRO.                                    *
-     *      DATA         : OUTUBRO/2022                              *
-     *      OBJETIVO     :                                           *
-     *      SICOR 5.05 - ALTERACAO DO CAMPO RUEC78-CSIST-PROD        *
-     *                                                               *
WIP001*===============================================================*
PRI499*                ALTERACAO - PRIME                               *
PRI499*----------------------------------------------------------------*
PRI499*                                                                *
PRI499* ANALISTA REPONSAVEL: WALTER MESSAS                             *
PRI499* DATA...............: 24/11/2022                                *
PRI499* OBJETIVOS..........: INCLUSAO DE TIPO DE CLIMA NO CODIGO       *
PRI499*                      DO SISTEMA DE PRODUCAO                    *
PRI499*================================================================*
EG0615
STE001*===============================================================*
-     *                                                               *
-     *      ALTERACAO  - STEFANINI                                   *
-     *                                                               *
-     *      ANALISTA     : STEFANINI                                 *
-     *      DATA         : MAIO / 2024                               *
-     *                                                               *
-     *      OBJETIVO     :                                           *
-     *                                                               *
STE001*===============================================================*
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

           SELECT  ARQCOR01  ASSIGN  TO  UT-S-ARQCOR01

      *===============================================================*
       DATA DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       FILE SECTION.
      *---------------------------------------------------------------*


      *---------------------------------------------------------------*
      *  OUTPUT...: ARQUIVO DE OCORRENCIAS                            *
EG0615*             ORG. SEQUENCIAL    -  LRECL = 450 BYTES           *
      *---------------------------------------------------------------*

       FD  ARQCOR01
           LABEL       RECORD    STANDARD
           RECORDING   MODE      F
           BLOCK       CONTAINS  0.

EG0615 01  FD-REG-COR01          PIC  X(450).
      *---------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *---------------------------------------------------------------*

       77  FILLER                       PIC X(32)        VALUE
           '* INICIO DA WORKING RUEC1550 *'.

       77  WRK-BATCH                    PIC X(08)  VALUE  'BATCH'.
       77  WRK-MAX-NENVIO            PIC S9(05) COMP-3 VALUE ZEROS.


       77  WRK-FIM                      PIC 9(01) VALUE ZEROS.
       88  WRK-FIM-RUFIB002             VALUE 1.

EG0615 01  FILLER.
EG0615     03  WRK-MODULO              PIC  X(08)          VALUE SPACES.
EG0615
EG0615 01  WRK-DATA-DB2                PIC  X(10)          VALUE SPACES.
EG0615 01  FILLER REDEFINES WRK-DATA-DB2.
EG0615     05 WRK-DIA-DB2              PIC  X(02).
EG0615     05 FILLER                   PIC  X(01).
EG0615     05 FILLER                   PIC  X(01).
EG0615     05 WRK-ANO-DB2              PIC  X(04).
EG0615
EG0615 01  WRK-DATA-ISO                PIC  X(08)          VALUE SPACES.
EG0615 01  FILLER REDEFINES WRK-DATA-ISO.
EG0615     05 WRK-ANO-ISO              PIC  X(04).
EG0615     05 WRK-MES-ISO              PIC  X(02).
EG0615     05 WRK-DIA-ISO              PIC  X(02).
EG0615
EG0615*---------------------------------------------------------------*
EG0615 01  FILLER                      PIC X(032)          VALUE
EG0615     '*** AREA DO MODULO RUEC8824  ***'.
EG0615*---------------------------------------------------------------*
EG0615
EG0615 COPY 'RUECW824'.
EG0615
      *---------------------------------------------------------------*
      *  AREA BOOK I#RUEC78.                                          *
      *---------------------------------------------------------------*

       COPY I#RUEC78.
      *---------------------------------------------------------------*
      *  AREA PARA UTILIZACAO POOL7100.                               *
      *---------------------------------------------------------------*

       COPY POL7100C.

      *---------------------------------------------------------------*
      *                 DECLARACAO DE ACUMULADORES E AUXILIARES       *
      *---------------------------------------------------------------*

       01  FILLER.
           03  ACU-LIDOS-B002     PIC  9(07) COMP-3 VALUE ZEROS.
           03  ACU-DESPR-B002     PIC  9(07) COMP-3 VALUE ZEROS.
           03  ACU-GRAVA-ARQCOR01 PIC  9(07) COMP-3 VALUE ZEROS.
           03  ACU-DESPR-B09B     PIC  9(07) COMP-3 VALUE ZEROS.

      *---------------------------------------------------------------*
      *          CAMPOS UTILIZADOS PARA TESTES DE FILE-STATUS         *
      *---------------------------------------------------------------*

       01  FILLER.
           03  WRK-OPERACAO            PIC  X(13) VALUE  SPACES.
           03  WRK-ABERTURA            PIC  X(13) VALUE 'NA ABERTURA'.
           03  WRK-GRAVACAO            PIC  X(13) VALUE 'NA GRAVACAO'.
           03  WRK-FECHAMENTO          PIC  X(13) VALUE 'NO FECHAMENTO'.

      *---------------------------------------------------------------*
      *          AREAS AUXILIARES                                     *
      *---------------------------------------------------------------*

       01  WRK-CAMPO11-X      PIC  X(11)  VALUE SPACES.
       01  WRK-CAMPO11-N      REDEFINES   WRK-CAMPO11-X
                              PIC  9(11).

       01  WRK-COD5-NUM       PIC  9(05)  VALUE ZEROS.
       01  FILLER             REDEFINES  WRK-COD5-NUM.
           05 FILLER          PIC  9(01).
           05 WRK-COD5-NUM4   PIC  9(04).

       01  WRK-COD3-CHAR       PIC  X(3)        VALUE SPACES.
       01  FILLER              REDEFINES WRK-COD3-CHAR.
           05 WRK-COD3-CHAR1   PIC  X(1).

       01  WRK-COD1-CHAR       PIC  X(1)        VALUE SPACES.
       01  FILLER              REDEFINES WRK-COD1-CHAR.
           05 WRK-COD1-NUM     PIC  9(1).

      *---------------------------------------------------------------*
      *  CAMPOS UTILIZADOS PARA O TRATAMENTO DE NULIDADE              *
      *---------------------------------------------------------------*

       01  WRK-NULL-B002.
           05 WRK-B002-CORIGE-NULL      PIC S9(04) COMP VALUE +0.
EG0615     05 WRK-B002-CFNALD-NULL      PIC S9(04) COMP VALUE +0.
EG0615     05 WRK-B002-CPRODT-NULL      PIC S9(04) COMP VALUE +0.

       01  WRK-NULL-B099.
           05 WRK-B099-DCDULA-NULL  PIC S9(04) COMP VALUE +0.
           05 WRK-B099-DVCTO-NULL   PIC S9(04) COMP VALUE +0.
           05 WRK-B099-VFINAN       PIC S9(04) COMP VALUE +0.
           05 WRK-B099-CCATEG-NULL  PIC S9(04) COMP VALUE +0.
           05 WRK-B099-CPROG-NULL   PIC S9(04) COMP VALUE +0.
           05 WRK-B099-CMUN-NULL    PIC S9(04) COMP VALUE +0.
           05 WRK-B099-REC-NULL     PIC S9(04) COMP VALUE +0.
           05 WRK-B099-FINCD-NULL   PIC S9(04) COMP VALUE +0.
           05 WRK-B099-PROD-NULL    PIC S9(04) COMP VALUE +0.
           05 WRK-B099-DINIC-NULL   PIC S9(04) COMP VALUE +0.
           05 WRK-B099-DFIM-NULL    PIC S9(04) COMP VALUE +0.
           05 WRK-B099-GARNT-NULL   PIC S9(04) COMP VALUE +0.

       01  WRK-NULL-B09B.
           05 WRK-B09B-CMSGEM-NULL      PIC S9(04) COMP VALUE +0.
           05 WRK-B09B-HINCL-NULL       PIC S9(04) COMP VALUE +0.
           05 WRK-B09B-CCTRL-NULL       PIC S9(04) COMP VALUE +0.
           05 WRK-B09B-CUNIC-NULL       PIC S9(04) COMP VALUE +0.
           05 WRK-B09B-CREFT-NULL       PIC S9(04) COMP VALUE +0.
           05 WRK-B09B-CSIT-NULL        PIC S9(04) COMP VALUE +0.
           05 WRK-B09B-CUSUAR-NULL      PIC S9(04) COMP VALUE +0.

       01  WRK-NULL-B09E.
           05 WRK-B09E-RESP-NULL   PIC S9(04) COMP VALUE +0.
           05 WRK-B09B-PSSOA-NULL  PIC S9(04) COMP VALUE +0.
           05 WRK-B09E-CCNPJ-NULL  PIC S9(04) COMP VALUE +0.
           05 WRK-B09E-CFLIAL-NULL PIC S9(04) COMP VALUE +0.

      *---------------------------------------------------------------*
      *                 DECLARACAO DE TABELAS DE DB2                  *
      *---------------------------------------------------------------*

           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
                INCLUDE RUFIB002
           END-EXEC.

           EXEC SQL
                INCLUDE RUECB099
           END-EXEC.

           EXEC SQL
                INCLUDE RUECB09B
           END-EXEC.

                INCLUDE RUECB09E
           END-EXEC.


      *---------------------------------------------------------------*
      *                 DECLARACAO DO CSR01-RUFIB002                  *
      *---------------------------------------------------------------*

           EXEC SQL
                DECLARE CSR01-RUFIB002 CURSOR     FOR
                SELECT
                        CCONTR,
                        CCDULA,
                        DCDULA,
                        CJUNC_DEPDC,
                        CCTA_CORR,
                        CREFT_BACEN,
                        CREFT_RECOR,
                        CSTTUS_CONTR_RURAL,
                        CORIGE_REC,
                        CTPO_CDULA_RECOR,
EG0615                  CPRODT_RURAL,
EG0615                  DIMPRE_CONTR_RURAL
                FROM  DB2PRD.CONTRATO_RURAL
                WHERE (CSTTUS_CONTR_RURAL = 9)
                ORDER  BY  CCONTR,
                           CCDULA
           END-EXEC.

      *---------------------------------------------------------------*
      *                 DECLARACAO DO CSR01-RUECB09E                  *
      *---------------------------------------------------------------*

           EXEC  SQL
              DECLARE  CSR01-RUECB09E CURSOR  WITH HOLD  FOR
             SELECT CCONTR_CREDT_RURAL,
                    NENVIO_RECOR,
                    NPSSOA_CONTR,
                    CTPO_RESP_PSSOA,
                    CTPO_PSSOA,
                    CCNPJ_CPF,
                    CFLIAL_CNPJ,
           FROM  DB2PRD.TPSSOA_ENVIO_RECOR
           WHERE CCONTR_CREDT_RURAL = :RUECB09E.CCONTR-CREDT-RURAL
           AND   NENVIO_RECOR       = :RUECB09E.NENVIO-RECOR
           AND   CTPO_RESP_PSSOA    = 'E'
           ORDER  BY  CCONTR_CREDT_RURAL,
                      NENVIO_RECOR
           END-EXEC.


      *---------------------------------------------------------------*
       01  FILLER                      PIC X(32)        VALUE
           '*  FIM DA WORKING RUEC1550 *'.
      *---------------------------------------------------------------*

EG0615*---------------------------------------------------------------*
EG0615 LINKAGE                         SECTION.
EG0615*---------------------------------------------------------------*
EG0615
EG0615 01  LNK-PARM.
EG0615     03  LNK-TAMANHO             PIC S9(04) COMP.
EG0615     03  LNK-DATA                PIC  X(08).
      *===============================================================*
EG0615 PROCEDURE DIVISION              USING LNK-PARM.
      *===============================================================*

      *---------------------------------------------------------------*
       0000-INICI0  SECTION.
      *---------------------------------------------------------------*

           CALL 'POOL1050'.

           OPEN    OUTPUT  ARQCOR01.

           MOVE    WRK-ABERTURA        TO    WRK-OPERACAO.
           PERFORM 0010-TESTAR-FILE-STATUS.

EG0615     IF  LNK-TAMANHO             EQUAL ZEROS
EG0615     OR  LNK-TAMANHO             NOT EQUAL 08
EG0615     OR  LNK-DATA                NOT NUMERIC
EG0615         DISPLAY 'PARM INVALIDO. UTILIZADA DATA PADRAO 20150701'
EG0615         MOVE '20150701'         TO LNK-DATA
EG0615     END-IF.
           PERFORM 3000-OPEN-CSR01-RUFIB002.

           PERFORM 4000-SELECIONAR    UNTIL WRK-FIM-RUFIB002.

           PERFORM 5000-CLOSE-CSR01-RUFIB002.

           PERFORM 6000-DISPLAY-SYSOUT.

           STOP RUN.

      *---------------------------------------------------------------*
       0000-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       0010-TESTAR-FILE-STATUS SECTION.
      *---------------------------------------------------------------*

           PERFORM 0030-TESTAR-FS-ARQCOR01.

      *---------------------------------------------------------------*
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       0030-TESTAR-FS-ARQCOR01  SECTION.
      *---------------------------------------------------------------*

           IF WRK-FS-ARQCOR01       NOT EQUAL  '00'
              DISPLAY '************** RUEC1550 *************'
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
              DISPLAY '*              ARQCOR01             *'
              DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQCOR01
                                                 '         *'
              DISPLAY '************** RUEC1550 *************'
              PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       0030-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      *---------------------------------------------------------------*

           EXEC SQL
                OPEN CSR01-RUFIB002
           END-EXEC.

           IF (SQLCODE           NOT EQUAL   +0)    OR
              (SQLWARN0              EQUAL  'W')
               MOVE 'DB2'                    TO   ERR-TIPO-ACESSO
               MOVE 'CONTRATO_RURAL  '       TO   ERR-DBD-TAB
               MOVE 'OPEN    '               TO   ERR-FUN-COMANDO
               MOVE SQLCODE                  TO   ERR-SQL-CODE
               MOVE '3000'                   TO   ERR-LOCAL
               MOVE SPACES                   TO   ERR-SEGM
               PERFORM   9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       3000-99-FIM. EXIT.
      *---------------------------------------------------------------*

       4000-SELECIONAR           SECTION.
      *---------------------------------------------------------------*

           PERFORM 4100-FETCH-CSR01-RUFIB002.

           IF SQLCODE EQUAL +100
              GO      TO    4000-99-FIM
           END-IF.

           PERFORM 4120-SELMAX-B09B.

           PERFORM 4130-SELECT-B09B.

           IF SQLCODE EQUAL +100
              GO      TO    4000-99-FIM
           END-IF.

           IF CSIT-MSGEM OF RUECB09B NOT EQUAL 4
              ADD 1 TO ACU-DESPR-B09B
              GO      TO    4000-99-FIM
           END-IF.
           PERFORM 4140-OBTER-DADOS-B099.

           PERFORM 4150-OPEN-CSR01-RUECB09E.

           PERFORM 4160-SELECT-RUECB09E.

           PERFORM 4110-GERAR-ARQCOR01.

           PERFORM 4170-CLOSE-CSR01-RUECB09E.

      *---------------------------------------------------------------*
       4000-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       4100-FETCH-CSR01-RUFIB002 SECTION.
      *---------------------------------------------------------------*
       4100-010-FETCH.
      *---------------------------------------------------------------*

           EXEC SQL
                  INTO
                   :RUFIB002.CCONTR,
                   :RUFIB002.CCDULA,
                   :RUFIB002.DCDULA,
                   :RUFIB002.CJUNC-DEPDC,
                   :RUFIB002.CCTA-CORR,
                   :RUFIB002.CREFT-BACEN,
                   :RUFIB002.CREFT-RECOR,
                   :RUFIB002.CSTTUS-CONTR-RURAL,
                   :RUFIB002.CORIGE-REC
                        :WRK-B002-CORIGE-NULL,
                   :RUFIB002.CTPO-CDULA-RECOR,
EG0615             :RUFIB002.CFNALD-RURAL
EG0615                  :WRK-B002-CFNALD-NULL,
EG0615             :RUFIB002.CPRODT-RURAL
EG0615                  :WRK-B002-CPRODT-NULL,
EG0615             :RUFIB002.DIMPRE-CONTR-RURAL
             END-EXEC.

           IF (SQLCODE           NOT EQUAL   +0 AND +100)    OR
              (SQLWARN0              EQUAL  'W')
               MOVE 'CONTRATO_RURAL  '       TO   ERR-DBD-TAB
               MOVE 'FETCH   '               TO   ERR-FUN-COMANDO
               MOVE SQLCODE                  TO   ERR-SQL-CODE
               MOVE '4100'                   TO   ERR-LOCAL
               MOVE SPACES                   TO   ERR-SEGM
               PERFORM   9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE      EQUAL +100
               MOVE  1        TO  WRK-FIM
               GO             TO  4100-99-FIM
           END-IF.

           IF  SQLCODE      EQUAL ZEROS
               PERFORM 4101-VER-NULOS-B002
               IF (CORIGE-REC   OF RUFIB002 GREATER 59  AND
                   CORIGE-REC   OF RUFIB002 LESS    81)
                   ADD   1        TO  ACU-LIDOS-B002
               ELSE
                   ADD   1        TO  ACU-DESPR-B002
                   GO  TO   4100-010-FETCH
           END-IF.

MF0313     IF  SQLCODE      EQUAL ZEROS
MF0313         DISPLAY ' '
MF0313         DISPLAY 'CONTRATO BNDES SELECIONADO = '
MF0313                           CCONTR OF RUFIB002
MF0313     END-IF.

      *---------------------------------------------------------------*
       4100-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       4101-VER-NULOS-B002  SECTION.
      *---------------------------------------------------------------*

           IF WRK-B002-CORIGE-NULL       LESS ZEROS
              MOVE ZEROS   TO CORIGE-REC    OF RUFIB002
           END-IF.

EG0615     IF WRK-B002-CFNALD-NULL       LESS ZEROS
EG0615     END-IF.
EG0615
EG0615     IF WRK-B002-CPRODT-NULL       LESS ZEROS
EG0615        MOVE ZEROS   TO CPRODT-RURAL  OF RUFIB002
EG0615     END-IF.
EG0615
      *---------------------------------------------------------------*
       4101-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       4110-GERAR-ARQCOR01  SECTION.
      *---------------------------------------------------------------*

           INITIALIZE  RUEC78-REG-FIMA.

           MOVE CCONTR             OF RUFIB002 TO RUEC78-CCONTR.

           MOVE CMSGEM-RECOR       OF RUECB09B
                                   TO RUEC78-CMSGEM-RECBD-MSGRA.

                                   TO RUEC78-CCTRL-RECBD-MSGRA.

           MOVE CCNPJ-ENTID-RESP   OF RUECB099
                                   TO RUEC78-CCNPJ-ENTID-RESP.

           MOVE CFLIAL-CNPJ-ENTID  OF RUECB099
                                   TO RUEC78-CFLIAL-CNPJ-ENTID.

           MOVE CCTRL-CNPJ-ENTID   OF RUECB099
                                   TO RUEC78-CCTRL-CNPJ-ENTID.

           MOVE CCNPJ-PARTC        OF RUECB099
                                   TO RUEC78-CCNPJ-PARTC.

           MOVE CFLIAL-CNPJ-PARTC  OF RUECB099
                                   TO WRK-COD5-NUM.
           MOVE WRK-COD5-NUM4      TO RUEC78-CFLIAL-CNPJ-PARTC.

           MOVE CCTRL-CNPJ-PARTC   OF RUECB099
                                   TO RUEC78-CCTRL-CNPJ-PARTC.

                                   TO WRK-CAMPO11-X
           MOVE WRK-CAMPO11-N      TO RUEC78-CCONTR-REFT-BACEN.

           MOVE CTPO-CDULA-RECOR   TO WRK-COD3-CHAR.
           MOVE WRK-COD3-CHAR1     TO WRK-COD1-CHAR.
           MOVE WRK-COD1-NUM       TO RUEC78-CINDCD-TPO-RECOR.

           MOVE DCDULA             OF RUECB099
                                   TO RUEC78-DCDULA-OPER-RECOR.

           MOVE DVCTO-CONTR        OF RUECB099
                                   TO RUEC78-DVCTO-CONTR.

           MOVE CCDULA-RECOR       OF RUECB099
                                   TO RUEC78-CCDULA-RECOR.

           MOVE CTPO-INSTT-CREDT   OF RUECB099
                                   TO RUEC78-CTPO-INSTT-CREDT.

           MOVE ZEROS              TO RUEC78-CCNPJ-ENTID-EMPTM.


           MOVE CCNPJ-CPF          OF RUECB09E
                                   TO RUEC78-CCNPJ-ENTID-EMITT.

           MOVE CFLIAL-CNPJ        OF RUECB09E
                                   TO WRK-COD5-NUM.
           MOVE WRK-COD5-NUM4      TO RUEC78-CFLIAL-CNPJ-EMITT.

           MOVE CCTRL-CNPJ-CPF     OF RUECB09E
                                   TO RUEC78-CCTRL-CNPJ-EMITT.

           MOVE VFINAN             OF RUECB099
                                   TO RUEC78-VTOT-OPER.

           MOVE CCATEG-PROTR-RECOR OF RUECB099
                                   TO RUEC78-CCATEG-EMITT.

           MOVE CTPO-PSSOA         OF RUECB09E
                                   TO RUEC78-CTPO-PSSOA-EMITT.

           MOVE SPACES             TO RUEC78-IGRP-EMITT-OPER.
           MOVE SPACES             TO RUEC78-RDSTNO-FINAN.

           MOVE ZEROS              TO RUEC78-CCONTR-BACEN-RENEG.

           MOVE ZEROS              TO RUEC78-VTOT-OPER-RENEG.

           MOVE SPACES             TO RUEC78-IGRP-RENEG-RURAL.

           MOVE CPROG-RURAL        OF RUECB099
                                   TO RUEC78-CPROG-RURAL.

           MOVE CTPO-FONTE-RECOR   OF RUECB099
                                   TO RUEC78-CTPO-FONTE-RECOR.

           MOVE CMUN-REFT-BACEN    OF RUECB099
                                   TO RUEC78-CMUN-REFT-BACEN.

           MOVE CEMPTO-BACEN       OF RUECB099
                                   TO RUEC78-CEMPTO-BACEN.

           MOVE CSIST-PROD         OF RUECB099
           IF  RUEC78-CSIST-PROD(10:02) EQUAL SPACES OR LOW-VALUES
               MOVE '00'           TO RUEC78-CSIST-PROD(10:02)
           END-IF
STE001     IF  RUEC78-CSIST-PROD(12:02) EQUAL SPACES OR LOW-VALUES
STE001         MOVE '99'           TO RUEC78-CSIST-PROD(12:02)
STE001     END-IF
PRI499*   XMOVE '00'               TO RUEC78-CSIST-PROD(10:02)

           MOVE VPCELA-LIBRC       OF RUECB099
                                   TO RUEC78-VPCELA-LIBRC.

           MOVE VPCELA-REC-PPRIO   OF RUECB099
                                   TO RUEC78-VPCELA-REC-PPRIO.

           MOVE PALIQT-PROAGRO     OF RUECB099
                                   TO RUEC78-PALIQT-PROAGRO.

           MOVE PJURO-ENCAR-FINCR  OF RUECB099
                                   TO RUEC78-PJURO-ENCAR-FINCR.

           MOVE CTPO-ENCAR-COMPL   OF RUECB099

           MOVE PRISCO-TSORO-NACIO OF RUECB099
                                   TO RUEC78-PRISCO-TSORO-NACIO.

           MOVE PRISCO-FUNDO-CNSTT OF RUECB099
                                   TO RUEC78-PRISCO-FUNDO-CNSTT.

           MOVE QAREA-RECOR        OF RUECB099
                                   TO RUEC78-QAREA-RECOR.

           MOVE QUND-FINCD-RURAL   OF RUECB099
                                   TO RUEC78-QUND-FINCD-RURAL.

           MOVE QUND-PROD-PROVV    OF RUECB099
                                   TO RUEC78-QUND-PROD-PROVV.

           MOVE DINIC-SAFRA        OF RUECB099
                                   TO RUEC78-DINIC-SAFRA.

           MOVE DFIM-SAFRA         OF RUECB099
                                   TO RUEC78-DFIM-SAFRA.
           MOVE HULT-ATULZ         OF RUECB09B
                                   TO RUEC78-HATULZ.

EG0615     PERFORM 4111-FORMATA-SUBPROG
EG0615     MOVE SPACES             TO RUEC78-RESERVA.

           WRITE  FD-REG-COR01 FROM RUEC78-REG-FIMA.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.
           PERFORM 0030-TESTAR-FS-ARQCOR01.

MF0313     DISPLAY 'CONTRATO BNDES GRAVADO P/FIMA = '
MF0313                                 CCONTR OF RUFIB002.
           ADD   1            TO  ACU-GRAVA-ARQCOR01.

      *---------------------------------------------------------------*
       4110-99-FIM. EXIT.
      *---------------------------------------------------------------*

EG0615*---------------------------------------------------------------*
EG0615 4111-FORMATA-SUBPROG            SECTION.
EG0615
EG0615     MOVE DIMPRE-CONTR-RURAL     OF RUFIB002
EG0615                                 TO WRK-DATA-DB2.
EG0615     MOVE WRK-DIA-DB2            TO WRK-DIA-ISO.
EG0615     MOVE WRK-MES-DB2            TO WRK-MES-ISO.
EG0615     MOVE WRK-ANO-DB2            TO WRK-ANO-ISO.
EG0615
EG0615     IF  WRK-DATA-ISO            LESS LNK-DATA
EG0615         MOVE SPACES             TO  RUEC78-CSUB-PROG-RURAL
EG0615         GO TO 4111-99-FIM
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
EG0615     MOVE 'RUEC8824'             TO  WRK-MODULO.
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
EG0615                                 TO  RUEC78-CSUB-PROG-RURAL.
EG0615
EG0615*----------------------------------------------------------------*
EG0615 4111-99-FIM.                    EXIT.
EG0615*----------------------------------------------------------------*
EG0615
      *----------------------------------------------------------------*
       4120-SELMAX-B09B               SECTION.
      *----------------------------------------------------------------*

                                      OF RUECB09B.

           EXEC SQL
           SELECT MAX(NENVIO_RECOR)
           INTO :WRK-MAX-NENVIO
           FROM DB2PRD.THIST_ENVIO_RECOR
           WHERE  CCONTR_CREDT_RURAL = :RUECB09B.CCONTR-CREDT-RURAL
           END-EXEC.

           IF (SQLCODE   NOT EQUAL ZEROS  AND
               SQLCODE   NOT EQUAL -305)  OR
              (SQLWARN0      EQUAL 'W')
               MOVE 'DB2'                TO ERR-TIPO-ACESSO
               MOVE 'THIST_ENVIO_RECOR'  TO ERR-DBD-TAB
               MOVE 'SELECT'             TO ERR-FUN-COMANDO
               MOVE SQLCODE              TO ERR-SQL-CODE
               MOVE '4120'               TO ERR-LOCAL
               MOVE SPACES               TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

       4120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4130-SELECT-B09B               SECTION.
      *----------------------------------------------------------------*

           MOVE CCONTR  OF RUFIB002   TO CCONTR-CREDT-RURAL
                                      OF RUECB09B.

           MOVE WRK-MAX-NENVIO        TO NENVIO-RECOR
                                      OF RUECB09B.

           EXEC SQL
           SELECT CCONTR_CREDT_RURAL,
                  NENVIO_RECOR,
                  HULT_ATULZ,
                  CMSGEM_RECOR,
                  HINCL,
                  CCTRL_RQUIS_FINCR,
                  CUNIC_MSGEM,
                  CSIT_MSGEM,
                  CUSUAR_RESP
           INTO   :RUECB09B.CCONTR-CREDT-RURAL,
                  :RUECB09B.NENVIO-RECOR,
                  :RUECB09B.HULT-ATULZ,
                  :RUECB09B.CMSGEM-RECOR
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
           FROM  DB2PRD.THIST_ENVIO_RECOR
           WHERE CCONTR_CREDT_RURAL  = :RUECB09B.CCONTR-CREDT-RURAL
           END-EXEC.

           IF (SQLCODE   NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0      EQUAL 'W')
               MOVE 'DB2'                TO ERR-TIPO-ACESSO
               MOVE 'THIST_ENVIO_RECOR'  TO ERR-DBD-TAB
               MOVE 'SELECT'             TO ERR-FUN-COMANDO
               MOVE SQLCODE              TO ERR-SQL-CODE
               MOVE '4130'               TO ERR-LOCAL
               MOVE SPACES               TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.


           IF  SQLCODE   EQUAL ZEROS
               PERFORM 4131-VER-NULOS-B09B
           END-IF.

      *---------------------------------------------------------------*
       4130-99-FIM.                EXIT.
      *---------------------------------------------------------------*
      *----------------------------------------------------------------*
      * VER NULIDADE DOS CAMPOS                                        *
      *----------------------------------------------------------------*
       4131-VER-NULOS-B09B            SECTION.
      *----------------------------------------------------------------*

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

           IF WRK-B09B-CSIT-NULL         LESS ZEROS
              MOVE ZEROS  TO  CSIT-MSGEM   OF RUECB09B
           END-IF.

           IF WRK-B09B-CUSUAR-NULL       LESS ZEROS
              MOVE SPACES TO  CUSUAR-RESP        OF RUECB09B
           END-IF.

      *----------------------------------------------------------------*
       4131-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       4140-OBTER-DADOS-B099     SECTION.
      *---------------------------------------------------------------*

           MOVE CCONTR  OF RUFIB002       TO CCONTR-CREDT-RURAL

           MOVE NENVIO-RECOR OF RUECB09B  TO NENVIO-RECOR
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
                  CMUN_REFT_BACEN,
                  CEMPTO_BACEN,
                  CSIST_PROD,
                  VPCELA_LIBRC,
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
                  :RUECB099.NENVIO-RECOR,
                  :RUECB099.CCNPJ-ENTID-RESP,
                  :RUECB099.CFLIAL-CNPJ-ENTID,
                  :RUECB099.CCTRL-CNPJ-ENTID,
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
                  :WRK-B099-VFINAN,
                  :RUECB099.CCATEG-PROTR-RECOR
                  :WRK-B099-CCATEG-NULL,
                  :RUECB099.CPROG-RURAL
                  :WRK-B099-CPROG-NULL,
                  :RUECB099.CTPO-FONTE-RECOR,
                  :WRK-B099-CMUN-NULL,
                  :RUECB099.CEMPTO-BACEN,
                  :RUECB099.CSIST-PROD,
                  :RUECB099.VPCELA-LIBRC
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
                  :RUECB099.DFIM-SAFRA
                  :WRK-B099-DFIM-NULL,
                  :RUECB099.CTPO-GARNT-RECOR
                  :WRK-B099-GARNT-NULL,
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
               MOVE '4140'               TO ERR-LOCAL
               MOVE SPACES               TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.
           IF (SQLCODE       EQUAL  +100)
               MOVE 'DB2'                TO ERR-TIPO-ACESSO
               MOVE 'TCONTR_ENVIO_RECOR' TO ERR-DBD-TAB
               MOVE 'SELECT'             TO ERR-FUN-COMANDO
               MOVE SQLCODE              TO ERR-SQL-CODE
               MOVE '414A'               TO ERR-LOCAL
               MOVE SPACES               TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE   EQUAL ZEROS
               PERFORM 4141-VER-NULOS-B099
           END-IF.

      *----------------------------------------------------------------*
       4140-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * VER NULIDADE DOS CAMPOS                                        *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*

           IF WRK-B099-DCDULA-NULL  LESS ZEROS
              MOVE SPACES TO DCDULA              OF RUECB099
           END-IF.

           IF WRK-B099-DVCTO-NULL   LESS ZEROS
              MOVE SPACES TO  DVCTO-CONTR        OF RUECB099
           END-IF.

           IF WRK-B099-VFINAN LESS ZEROS
              MOVE ZEROS  TO VFINAN              OF RUECB099
           END-IF.

           IF WRK-B099-CCATEG-NULL LESS ZEROS
              MOVE SPACES TO CCATEG-PROTR-RECOR  OF RUECB099
           END-IF.

           IF WRK-B099-CPROG-NULL    LESS ZEROS
              MOVE SPACES TO CPROG-RURAL         OF RUECB099
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
           END-IF.

           IF WRK-B099-DFIM-NULL LESS ZEROS
              MOVE ZEROS  TO DFIM-SAFRA          OF RUECB099
           END-IF.

           IF WRK-B099-GARNT-NULL LESS ZEROS
              MOVE ZEROS  TO CTPO-GARNT-RECOR    OF RUECB099
           END-IF.

      *----------------------------------------------------------------*
       4141-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       4150-OPEN-CSR01-RUECB09E  SECTION.
      *---------------------------------------------------------------*

           MOVE CCONTR        OF RUFIB002  TO CCONTR-CREDT-RURAL
                                           OF RUECB09E.
           MOVE NENVIO-RECOR  OF RUECB09B  TO NENVIO-RECOR

           EXEC SQL
              OPEN      CSR01-RUECB09E
           END-EXEC.

           IF (SQLCODE           NOT EQUAL   +0)    OR
              (SQLWARN0              EQUAL  'W')
               MOVE 'DB2'                    TO   ERR-TIPO-ACESSO
               MOVE 'TPSSOA_ENVIO_RECOR'     TO   ERR-DBD-TAB
               MOVE 'OPEN    '               TO   ERR-FUN-COMANDO
               MOVE SQLCODE                  TO   ERR-SQL-CODE
               MOVE '4150'                   TO   ERR-LOCAL
               MOVE SPACES                   TO   ERR-SEGM
               PERFORM   9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       4150-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
      *---------------------------------------------------------------*

           EXEC SQL
              FETCH CSR01-RUECB09E INTO
                 :RUECB09E.CCONTR-CREDT-RURAL,
                 :RUECB09E.NENVIO-RECOR,
                 :RUECB09E.NPSSOA-CONTR,
                 :RUECB09E.CTPO-RESP-PSSOA
                 :WRK-B09E-RESP-NULL,
                 :RUECB09E.CTPO-PSSOA
                 :WRK-B09B-PSSOA-NULL,
                 :RUECB09E.CCNPJ-CPF
                 :WRK-B09E-CCNPJ-NULL,
                 :RUECB09E.CFLIAL-CNPJ
                 :WRK-B09E-CFLIAL-NULL,
                 :RUECB09E.CCTRL-CNPJ-CPF
                 :WRK-B09E-CCTRL-NULL
           END-EXEC.

           IF (SQLCODE           NOT EQUAL   +0 AND +100)    OR
              (SQLWARN0              EQUAL  'W')
               MOVE 'TPSSOA_ENVIO_RECOR'     TO   ERR-DBD-TAB
               MOVE 'FETCH   '               TO   ERR-FUN-COMANDO
               MOVE SQLCODE                  TO   ERR-SQL-CODE
               MOVE '4160'                   TO   ERR-LOCAL
               MOVE SPACES                   TO   ERR-SEGM
               PERFORM   9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE      EQUAL +100
               MOVE 'DB2'                    TO   ERR-TIPO-ACESSO
               MOVE 'TPSSOA_ENVIO_RECOR'     TO   ERR-DBD-TAB
               MOVE 'FETCH   '               TO   ERR-FUN-COMANDO
               MOVE SQLCODE                  TO   ERR-SQL-CODE
               MOVE '416A'                   TO   ERR-LOCAL
               MOVE SPACES                   TO   ERR-SEGM
               PERFORM   9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE      EQUAL ZEROS
               PERFORM 4161-VER-NULOS-B09E
           END-IF.
      *---------------------------------------------------------------*
       4160-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       4161-VER-NULOS-B09E  SECTION.
      *---------------------------------------------------------------*

           IF WRK-B09E-RESP-NULL    LESS ZEROS
              MOVE SPACES TO CTPO-RESP-PSSOA OF RUECB09E
           END-IF.

           IF WRK-B09B-PSSOA-NULL      LESS ZEROS
              MOVE ZEROS  TO CTPO-PSSOA   OF RUECB09E
           END-IF.

           IF WRK-B09E-CCNPJ-NULL LESS ZEROS
              MOVE ZEROS  TO CCNPJ-CPF    OF RUECB09E
           END-IF.

           IF WRK-B09E-CFLIAL-NULL LESS ZEROS
           END-IF.

           IF WRK-B09E-CCTRL-NULL      LESS ZEROS
              MOVE ZEROS  TO CCTRL-CNPJ-CPF OF RUECB09E
           END-IF.

      *----------------------------------------------------------------*
       4161-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       4170-CLOSE-CSR01-RUECB09E SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR01-RUECB09E
           END-EXEC.

           IF (SQLCODE           NOT EQUAL   +0)    OR
              (SQLWARN0              EQUAL  'W')
               MOVE 'DB2'                    TO   ERR-TIPO-ACESSO
               MOVE 'CLOSE   '               TO   ERR-FUN-COMANDO
               MOVE SQLCODE                  TO   ERR-SQL-CODE
               MOVE '4170'                   TO   ERR-LOCAL
               MOVE SPACES                   TO   ERR-SEGM
               PERFORM   9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       4170-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       5000-CLOSE-CSR01-RUFIB002 SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR01-RUFIB002
           END-EXEC.

           IF (SQLCODE           NOT EQUAL   +0)    OR
              (SQLWARN0              EQUAL  'W')
               MOVE 'CONTRATO_RURAL  '       TO   ERR-DBD-TAB
               MOVE 'CLOSE   '               TO   ERR-FUN-COMANDO
               MOVE SQLCODE                  TO   ERR-SQL-CODE
               MOVE '5000'                   TO   ERR-LOCAL
               MOVE SPACES                   TO   ERR-SEGM
               PERFORM   9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       5000-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       6000-DISPLAY-SYSOUT  SECTION.
      *---------------------------------------------------------------*

           CLOSE   ARQCOR01.

           MOVE  WRK-FECHAMENTO        TO  WRK-OPERACAO.
           PERFORM 0010-TESTAR-FILE-STATUS.

      -            '***********'
           DISPLAY '*
      -            '          *'
           DISPLAY '* TOTAL REGS LIDOS RUFIB002.........................
      -            ': '  ACU-LIDOS-B002   ' *'
           DISPLAY '*
      -            '          *'
           DISPLAY '* TOTAL REGS GERADOS ARQUIVO ARQCOR01...............
      -            ': '  ACU-GRAVA-ARQCOR01 ' *'
           DISPLAY '*
      -            '          *'
           DISPLAY '* TOTAL REGS DESPR RUFIB002(CORIGE-REC < 60 OU > 80)
      -            ': '  ACU-DESPR-B002   ' *'
           DISPLAY '*
      -            '          *'
           DISPLAY '* TOTAL REGS DESPR RUECB09E(SITUACAO # 3)...........
      -            ': '  ACU-DESPR-B09B   ' *'
           DISPLAY '*
      -            '          *'
           DISPLAY '************************** RUEC1550 ****************
      -            '***********'.
      *---------------------------------------------------------------*
       6000-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       9999-ROTINA-ERRO  SECTION.
      *---------------------------------------------------------------*

           MOVE 'RUEC1550'     TO  ERR-PGM.

           CALL  'POOL7100'     USING    WRK-BATCH
                                         ERRO-AREA
                                         SQLCA
           GOBACK.

      *----------------------------------------------------------------*
       9999-EXIT. EXIT.
      *----------------------------------------------------------------*
