      *===============================================================*
       IDENTIFICATION DIVISION.
      *===============================================================*
       PROGRAM-ID.    RUEC1590.
       AUTHOR.        BSI TECNOLOGIA.

      *===============================================================*
      *                                                               *
      *      PROGRAMA     : RUEC1590                                  *
      *      PROGRAMADOR  : BSI TECNOLOGIA.                           *
      *      ANALISTA     : BSI TECNOLOGIA.                           *
      *      DATA         : 16/01/2013                                *
      *                                                               *
      *      OBJETIVO     :                                           *
      *      GERAR ARQUIVO ACUMULADO RECOR - CARGA INICIAL.           *
      *      GERAR ARQUIVO ACUMULADO RECOR - CARGA INICIAL.           *
      *      GERAR ARQUIVO ACUMULADO RECOR - CARGA INICIAL.           *
      *                                                               *
      *      DB2:                                                     *
      *        RUECB099 - DB2PRD.TCONTR_ENVIO_RECOR                   *
      *        RUECB09D - DB2PRD.TPCELA_ENVIO_RECOR                   *
      *        RUECB09E - DB2PRD.TPSSOA_ENVIO_RECOR                   *
      *        RUECB09F - DB2PRD.TPRODT_RURAL_CSCDO                   *
      *        RUFIB002 - DB2PRD.CONTRATO_RURAL                       *
      *                                                               *
      *===============================================================*

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

       FILE-CONTROL.

           SELECT  ARQACUMU  ASSIGN  TO  UT-S-ARQACUMU
                   FILE      STATUS  IS  WRK-FS-ARQACUMU.

      *===============================================================*
       DATA DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       FILE SECTION.
      *---------------------------------------------------------------*


      *---------------------------------------------------------------*
      *  OUTPUT...: ARQUIVO ACUMULADO RECOR                           *
      *             ORG. SEQUENCIAL    -  LRECL = 100 BYTES           *
      *---------------------------------------------------------------*

       FD  ARQACUMU
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
           '* INICIO DA WORKING RUEC1590 *'.

       77  WRK-BATCH               PIC X(08)  VALUE  'BATCH'.
       77  WRK-MODULO-RUEC8822     PIC X(08)  VALUE 'RUEC8822'.
       77  WRK-CCONTR-INI          PIC S9(09)  COMP-3  VALUE ZEROS.
       77  WRK-CCONTR-FIM          PIC S9(09)  COMP-3  VALUE ZEROS.


       77  WRK-FIM                 PIC 9(01) VALUE ZEROS.
       88  WRK-FIM-RUECB09B    VALUE 1.

       77  WRK-IND-OCOR            PIC S9(05) COMP-3 VALUE ZEROS.

       77  WRK-IND-B09D            PIC S9(04) COMP-3 VALUE ZEROS.
       77  WRK-CODE100-B09D        PIC  X(01) VALUE 'N'.

       77  WRK-IND-B09E            PIC S9(04) COMP-3 VALUE ZEROS.
       77  WRK-CODE100-B09E        PIC  X(01) VALUE 'N'.

       77  WRK-IND-B09F            PIC S9(04) COMP-3 VALUE ZEROS.
       77  WRK-CODE100-B09F        PIC  X(01) VALUE 'N'.

       77  WRK-CONTR               PIC S9(09) COMP-3 VALUE ZEROS.
       77  WRK-SEQREG              PIC  9(05) COMP-3 VALUE ZEROS.

       01  WRK-QTD-SSINAL          PIC   9(05) VALUE ZEROS.
       01  FILLER                  REDEFINES WRK-QTD-SSINAL.
           05 WRK-QTD-CSINAL       PIC  S9(05).
       01  WRK-QTD-PESQ         PIC S9(05)  COMP-3  VALUE ZEROS.

       01  WRK-COD9-NUM        PIC  9(9)        VALUE ZEROS.

       01  WRK-COD5-NUM        PIC  9(5)        VALUE ZEROS.
       01  FILLER              REDEFINES WRK-COD5-NUM.
           05 FILLER           PIC  9(1).
           05 WRK-COD5-NUM4    PIC  9(4).

       01  WRK-COD3-NUM        PIC  9(3)        VALUE ZEROS.
       01  FILLER              REDEFINES WRK-COD3-NUM.
           05 FILLER           PIC  9(1).
           05 WRK-COD3-NUM2    PIC  9(2).

       01  WRK-AUX3-NUM        PIC  9(3)        VALUE ZEROS.
       01  FILLER              REDEFINES WRK-AUX3-NUM.
           05 FILLER           PIC  9(2).
           05 WRK-AUX3-NUM1    PIC  9(1).

       01  WRK-COD3-CHAR       PIC  X(3)        VALUE SPACES.
       01  FILLER              REDEFINES WRK-COD3-CHAR.
           05 FILLER           PIC  X(2).

       01  WRK-VRECTA-CSINAL          PIC S9(15)V99  VALUE ZEROS.
       01  FILLER                 REDEFINES WRK-VRECTA-CSINAL.
           05 WRK-VRECTA-SSINAL       PIC  9(15)V99.

       01  WRK-VPCELA-LIBRC-CSINAL    PIC S9(13)V99  VALUE ZEROS.
       01  FILLER                 REDEFINES WRK-VPCELA-LIBRC-CSINAL.
           05 WRK-VPCELA-LIBRC-SSINAL PIC  9(13)V99.

       01  WRK-VPCELA-REC-CSINAL    PIC S9(13)V99  VALUE ZEROS.
       01  FILLER                 REDEFINES WRK-VPCELA-REC-CSINAL.
           05 WRK-VPCELA-REC-SSINAL PIC  9(13)V99.

       01  WRK-QAREA-CSINAL         PIC S9(07)V99  VALUE ZEROS.
       01  FILLER                 REDEFINES WRK-QAREA-CSINAL.
           05 WRK-QAREA-SSINAL      PIC  9(07)V99.

       01  WRK-QUND-FINCD-CSINAL    PIC S9(09)V99  VALUE ZEROS.
       01  FILLER                 REDEFINES WRK-QUND-FINCD-CSINAL.
           05 WRK-QUND-FINCD-SSINAL PIC  9(09)V99.
       01  WRK-QUND-PROD-CSINAL     PIC S9(09)V99  VALUE ZEROS.
       01  FILLER                 REDEFINES  WRK-QUND-PROD-CSINAL.
           05 WRK-QUND-PROD-SSINAL  PIC  9(09)V99.

       01  WRK-PERC-FINANC-CSINAL    PIC S9(08)V9(5) VALUE ZEROS.
       01  FILLER                 REDEFINES WRK-PERC-FINANC-CSINAL.
           05 FILLER                 PIC  9(05).
           05 WRK-PERC-FINANC-SSINAL PIC  9(03)V9(2).
           05 FILLER                 PIC  9(03).

       01  WRK-SAFRA.
           05 WRK-DINIC-SAFRA  PIC  X(4) VALUE SPACES.
           05 WRK-DFIM-SAFRA   PIC  X(4) VALUE SPACES.

       01  WRK-COD6-NUM        PIC  9(6)        VALUE ZEROS.
       01  FILLER              REDEFINES WRK-COD6-NUM.
           05 WRK-COD6-CHAR    PIC  X(6).

       01  WRK-AUX6-CHAR       PIC  X(6)        VALUE SPACES.
       01  FILLER              REDEFINES WRK-AUX6-CHAR.
           05 FILLER           PIC  X(2).

       01  WRK-AUXTOT-COMP         PIC    9(05) COMP-3 VALUE 999.

       01  WRK-AUXTOT              PIC    9(05)        VALUE 999.

       01  WRK-NITEM-ENV           PIC    9(05) COMP-3 VALUE ZEROS.

       01  WRK-QTDREG-B097         PIC S9(05) COMP-3 VALUE ZEROS.

       01  WRK-QTDB097-CSINAL      PIC   S9(05) VALUE ZEROS.
       01  FILLER                  REDEFINES WRK-QTDB097-CSINAL.
           05 WRK-QTDB097          PIC    9(05).

      *---------------------------------------------------------------*
      *  AREA DE LEITURA                                              *
      *---------------------------------------------------------------*

       COPY I#RUEC81.

      *---------------------------------------------------------------*
      *  AREA PARA UTILIZACAO POOL7100.                               *

       COPY POL7100C.

      *---------------------------------------------------------------*
      *                 DECLARACAO DE ACUMULADORES E AUXILIARES       *
      *---------------------------------------------------------------*

       01  FILLER.
           03  ACU-LIDOS-B09B     PIC  9(07) COMP-3 VALUE ZEROS.
           03  ACU-DESPR-B09B     PIC  9(07) COMP-3 VALUE ZEROS.
           03  ACU-NACHOU-B002    PIC  9(07) COMP-3 VALUE ZEROS.
           03  ACU-DESPR-B002     PIC  9(07) COMP-3 VALUE ZEROS.
           03  ACU-GRAVA-ARQACUMU PIC  9(07) COMP-3 VALUE ZEROS.

      *---------------------------------------------------------------*
      *          CAMPOS UTILIZADOS PARA TESTES DE FILE-STATUS         *
      *---------------------------------------------------------------*

       01  FILLER.
           03  WRK-FS-ARQACUMU         PIC  X(02) VALUE  SPACES.
           03  WRK-OPERACAO            PIC  X(13) VALUE  SPACES.
           03  WRK-GRAVACAO            PIC  X(13) VALUE 'NA GRAVACAO'.
           03  WRK-FECHAMENTO          PIC  X(13) VALUE 'NO FECHAMENTO'.

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(079)          VALUE
           '*** AREA PARA CHAMADA MODULO RUEC8822 ***'.
      *---------------------------------------------------------------*

       COPY 'I#RUEC79'.

      *---------------------------------------------------------------*
      *  CAMPOS UTILIZADOS PARA O TRATAMENTO DE NULIDADE              *
      *---------------------------------------------------------------*

       01  WRK-B099-NULL.
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

       01  WRK-B09E-NULL.
           05 WRK-B09E-RESP-NULL        PIC S9(04) COMP VALUE +0.
           05 WRK-B09E-CCNPJ-NULL       PIC S9(04) COMP VALUE +0.
           05 WRK-B09E-CFLIAL-NULL      PIC S9(04) COMP VALUE +0.
           05 WRK-B09E-CCTRL-NULL       PIC S9(04) COMP VALUE +0.

       01  WRK-B09F-NULL.
           05 WRK-B09F-CPRODT-NULL      PIC S9(04) COMP VALUE +0.

       01  WRK-B002-NULL.
           05 WRK-B002-CORIGE-NULL      PIC S9(04) COMP VALUE +0.

      *---------------------------------------------------------------*
      *                 DECLARACAO DE TABELAS DE DB2                  *
      *---------------------------------------------------------------*

           EXEC SQL
                INCLUDE SQLCA
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
                INCLUDE RUECB09D
           END-EXEC.

           EXEC SQL
                INCLUDE RUECB09E
           END-EXEC.

           EXEC SQL
                INCLUDE RUECB09F
           END-EXEC.

           EXEC SQL
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
                                             AND :WRK-CCONTR-FIM
                ORDER  BY  CCONTR_CREDT_RURAL ASC,
                           NENVIO_RECOR       DESC
           END-EXEC.

      *---------------------------------------------------------------*
      *                 DECLARACAO DO CSR01-RUECB09D                  *
      *---------------------------------------------------------------*

           EXEC  SQL
             DECLARE  CSR01-RUECB09D CURSOR  WITH HOLD  FOR
             SELECT CCONTR_CREDT_RURAL,
                    NENVIO_RECOR,
                    NPCELA_CONTR,
                    DVCTO_PCELA_CONTR,
                    VPCELA_RURAL
           FROM  DB2PRD.TPCELA_ENVIO_RECOR
           WHERE CCONTR_CREDT_RURAL = :RUECB09D.CCONTR-CREDT-RURAL
           AND   NENVIO_RECOR       = :RUECB09D.NENVIO-RECOR
           ORDER  BY  CCONTR_CREDT_RURAL,
                      NENVIO_RECOR,
           END-EXEC.

      *---------------------------------------------------------------*
      *                 DECLARACAO DO CSR01-RUECB09E                  *
      *---------------------------------------------------------------*

           EXEC SQL
           DECLARE CSR01-RUECB09E CURSOR  WITH HOLD  FOR
           SELECT  CCONTR_CREDT_RURAL,
                   NENVIO_RECOR,
                   NPSSOA_CONTR,
                   CTPO_RESP_PSSOA,
                   CTPO_PSSOA,
                   CCNPJ_CPF,
                   CFLIAL_CNPJ,
                   CCTRL_CNPJ_CPF
           FROM  DB2PRD.TPSSOA_ENVIO_RECOR
           WHERE CCONTR_CREDT_RURAL = :RUECB09E.CCONTR-CREDT-RURAL
           AND   NENVIO_RECOR       = :RUECB09E.NENVIO-RECOR
           AND   CTPO_RESP_PSSOA    = :RUECB09E.CTPO-RESP-PSSOA
           ORDER BY  CCONTR_CREDT_RURAL,
           END-EXEC.

      *---------------------------------------------------------------*
      *                 DECLARACAO DO CSR01-RUECB09F                  *
      *---------------------------------------------------------------*

           EXEC SQL
           DECLARE CSR01-RUECB09F CURSOR FOR
           SELECT  CPRODT_RURAL
           FROM  DB2PRD.TPRODT_RURAL_RECOR
           WHERE CCONTR_CREDT_RURAL = :RUECB09F.CCONTR-CREDT-RURAL
           AND   NENVIO_RECOR       = :RUECB09F.NENVIO-RECOR
           ORDER BY CPRODT_RURAL ASC
           END-EXEC.

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(32)        VALUE
           '*  FIM DA WORKING RUEC1590 *'.
      *---------------------------------------------------------------*

      *===============================================================*
      *===============================================================*

      *---------------------------------------------------------------*
       0000-INICI0  SECTION.
      *---------------------------------------------------------------*

           CALL 'POOL1050'.

           OPEN    OUTPUT  ARQACUMU.

           MOVE    WRK-ABERTURA        TO    WRK-OPERACAO.
           PERFORM 0100-TESTAR-FILE-STATUS.

           PERFORM 3000-OPEN-CSR01-RUECB09B.

           PERFORM 5000-SELECIONAR    UNTIL WRK-FIM-RUECB09B.

           PERFORM 3020-CLOSE-CSR01-RUECB09B.

           PERFORM 7000-DISPLAY-SYSOUT.


      *---------------------------------------------------------------*
       0000-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       0100-TESTAR-FILE-STATUS SECTION.
      *---------------------------------------------------------------*

           PERFORM 0200-TESTAR-FS-ARQACUMU.

      *---------------------------------------------------------------*
       0100-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       0200-TESTAR-FS-ARQACUMU  SECTION.
      *---------------------------------------------------------------*

           IF WRK-FS-ARQACUMU       NOT EQUAL  '00'
              DISPLAY '************** RUEC1590 *************'
              DISPLAY '*              ARQACUMU             *'
              DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQACUMU
                                                 '         *'
              DISPLAY '************** RUEC1590 *************'
              PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       0200-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3000-OPEN-CSR01-RUECB09B SECTION.
      *---------------------------------------------------------------*

           MOVE 539221               TO WRK-CCONTR-INI.
           MOVE 557201               TO WRK-CCONTR-FIM.

           EXEC SQL
                OPEN CSR01-RUECB09B
           END-EXEC.
           IF (SQLCODE           NOT EQUAL   +0)    OR
              (SQLWARN0              EQUAL  'W')
               MOVE 'DB2'                    TO   ERR-TIPO-ACESSO
               MOVE 'THIST_ENVIO_RECOR'      TO   ERR-DBD-TAB
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
       3010-FETCH-CSR01-RUECB09B SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
                FETCH CSR01-RUECB09B
                   :RUECB09B.CCONTR-CREDT-RURAL,
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
             END-EXEC.

           IF (SQLCODE           NOT EQUAL   +0 AND +100)    OR
              (SQLWARN0              EQUAL  'W')
               MOVE 'THIST_ENVIO_RECOR'      TO   ERR-DBD-TAB
               MOVE 'FETCH   '               TO   ERR-FUN-COMANDO
               MOVE SQLCODE                  TO   ERR-SQL-CODE
               MOVE '3010'                   TO   ERR-LOCAL
               MOVE SPACES                   TO   ERR-SEGM
               PERFORM   9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE      EQUAL +100
               MOVE  1        TO  WRK-FIM
               GO             TO  3010-99-FIM
           END-IF.

           IF  SQLCODE      EQUAL ZEROS
               PERFORM 3011-B09B-VER-NULOS
               ADD   1      TO  ACU-LIDOS-B09B
           END-IF.

      *---------------------------------------------------------------*
       3010-99-FIM. EXIT.
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
       3011-B09B-VER-NULOS  SECTION.
      *---------------------------------------------------------------*

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
           END-IF.

           IF WRK-B09B-CSIT-NULL         LESS ZEROS
              MOVE ZEROS  TO  CSIT-MSGEM   OF RUECB09B
           END-IF.

           IF WRK-B09B-CUSUAR-NULL       LESS ZEROS
              MOVE SPACES TO  CUSUAR-RESP        OF RUECB09B
           END-IF.

      *---------------------------------------------------------------*
       3011-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3020-CLOSE-CSR01-RUECB09B SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR01-RUECB09B
           END-EXEC.
           IF (SQLCODE           NOT EQUAL   +0)    OR
              (SQLWARN0              EQUAL  'W')
               MOVE 'DB2'                    TO   ERR-TIPO-ACESSO
               MOVE 'THIST_ENVIO_RECOR'      TO   ERR-DBD-TAB
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
       3030-OPEN-CSR01-RUECB09D  SECTION.
      *---------------------------------------------------------------*

           MOVE CCONTR-CREDT-RURAL OF RUECB09B  TO CCONTR-CREDT-RURAL
                                                OF RUECB09D.
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
               MOVE '3030'                   TO   ERR-LOCAL
               MOVE SPACES                   TO   ERR-SEGM
               PERFORM   9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       3030-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

       3040-SELECT-RUECB09D      SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
              FETCH CSR01-RUECB09D INTO
                 :RUECB09D.CCONTR-CREDT-RURAL,
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
               MOVE '3040'                   TO   ERR-LOCAL
               MOVE SPACES                   TO   ERR-SEGM
           END-IF.

           IF SQLCODE   EQUAL  +100
              MOVE 'S' TO WRK-CODE100-B09D
           END-IF.

           IF SQLCODE   EQUAL  ZEROS
              PERFORM 3041-B09D-VER-NULOS
           END-IF.

      *---------------------------------------------------------------*
       3040-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3041-B09D-VER-NULOS  SECTION.
      *---------------------------------------------------------------*

           IF WRK-B09D-DVCTO-NULL  LESS  ZEROS
              MOVE SPACES  TO  DVCTO-PCELA-CONTR  OF RUECB09D
           END-IF.
      *----------------------------------------------------------------*
       3041-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       3050-CLOSE-CSR01-RUECB09D SECTION.
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
               MOVE '3050'                   TO   ERR-LOCAL
               MOVE SPACES                   TO   ERR-SEGM
               PERFORM   9999-ROTINA-ERRO

      *---------------------------------------------------------------*
       3050-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3060-OPEN-CSR01-RUECB09E  SECTION.
      *---------------------------------------------------------------*

           MOVE CCONTR-CREDT-RURAL OF RUECB09B  TO CCONTR-CREDT-RURAL
                                                OF RUECB09E.
           MOVE NENVIO-RECOR       OF RUECB09B  TO NENVIO-RECOR
                                                OF RUECB09E.

           EXEC SQL
              OPEN      CSR01-RUECB09E
           END-EXEC.

           IF (SQLCODE           NOT EQUAL   +0)    OR
              (SQLWARN0              EQUAL  'W')
               MOVE 'DB2'                    TO   ERR-TIPO-ACESSO
               MOVE 'OPEN    '               TO   ERR-FUN-COMANDO
               MOVE SQLCODE                  TO   ERR-SQL-CODE
               MOVE '3060'                   TO   ERR-LOCAL
               MOVE SPACES                   TO   ERR-SEGM
               PERFORM   9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       3060-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3070-SELECT-RUECB09E      SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
              FETCH CSR01-RUECB09E INTO
                 :RUECB09E.CCONTR-CREDT-RURAL,
                 :RUECB09E.NENVIO-RECOR,
                 :RUECB09E.NPSSOA-CONTR,
                 :RUECB09E.CTPO-RESP-PSSOA
                 :RUECB09E.CTPO-PSSOA
                      :WRK-B09E-PSSOA-NULL,
                 :RUECB09E.CCNPJ-CPF
                      :WRK-B09E-CCNPJ-NULL,
                 :RUECB09E.CFLIAL-CNPJ
                      :WRK-B09E-CFLIAL-NULL,
                 :RUECB09E.CCTRL-CNPJ-CPF
                      :WRK-B09E-CCTRL-NULL
           END-EXEC.

           IF (SQLCODE   NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0      EQUAL 'W')
               MOVE 'DB2'                TO  ERR-TIPO-ACESSO
               MOVE 'TPSSOA_ENVIO_RECOR' TO  ERR-DBD-TAB
               MOVE 'SELECT'             TO  ERR-FUN-COMANDO
               MOVE SQLCODE              TO  ERR-SQL-CODE
               MOVE '3070'               TO  ERR-LOCAL
               MOVE SPACES               TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

              MOVE 'S' TO WRK-CODE100-B09E
           END-IF.

           IF  SQLCODE   EQUAL ZEROS
               PERFORM 3071-B09E-VER-NULOS
           END-IF.

      *---------------------------------------------------------------*
       3070-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3071-B09E-VER-NULOS    SECTION.
      *---------------------------------------------------------------*

           IF WRK-B09E-RESP-NULL    LESS ZEROS
              MOVE SPACES TO CTPO-RESP-PSSOA OF RUECB09E
           END-IF.

           IF WRK-B09E-PSSOA-NULL      LESS ZEROS
              MOVE ZEROS  TO CTPO-PSSOA   OF RUECB09E

           IF WRK-B09E-CCNPJ-NULL LESS ZEROS
              MOVE ZEROS  TO CCNPJ-CPF    OF RUECB09E
           END-IF.

           IF WRK-B09E-CFLIAL-NULL LESS ZEROS
              MOVE ZEROS  TO CFLIAL-CNPJ  OF RUECB09E
           END-IF.

           IF WRK-B09E-CCTRL-NULL      LESS ZEROS
              MOVE ZEROS  TO CCTRL-CNPJ-CPF OF RUECB09E
           END-IF.

      *---------------------------------------------------------------*
       3071-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3080-CLOSE-CSR01-RUECB09E SECTION.
      *---------------------------------------------------------------*

               CLOSE CSR01-RUECB09E
           END-EXEC.

           IF (SQLCODE           NOT EQUAL   +0)    OR
              (SQLWARN0              EQUAL  'W')
               MOVE 'DB2'                    TO   ERR-TIPO-ACESSO
               MOVE 'TPSSOA_ENVIO_RECOR'     TO   ERR-DBD-TAB
               MOVE 'CLOSE   '               TO   ERR-FUN-COMANDO
               MOVE SQLCODE                  TO   ERR-SQL-CODE
               MOVE '3080'                   TO   ERR-LOCAL
               MOVE SPACES                   TO   ERR-SEGM
               PERFORM   9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       3080-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3090-OPEN-CSR01-RUECB09F  SECTION.
      *---------------------------------------------------------------*
           MOVE CCONTR-CREDT-RURAL OF RUECB09B  TO CCONTR-CREDT-RURAL
                                                OF RUECB09F.

           MOVE NENVIO-RECOR       OF RUECB09B  TO NENVIO-RECOR
                                                OF RUECB09F.

           EXEC SQL
              OPEN      CSR01-RUECB09F
           END-EXEC.

           IF (SQLCODE           NOT EQUAL   +0)    OR
              (SQLWARN0              EQUAL  'W')
               MOVE 'DB2'                    TO   ERR-TIPO-ACESSO
               MOVE 'TPRODT_RURAL_RECOR'     TO   ERR-DBD-TAB
               MOVE 'OPEN    '               TO   ERR-FUN-COMANDO
               MOVE SQLCODE                  TO   ERR-SQL-CODE
               MOVE '3090'                   TO   ERR-LOCAL
               MOVE SPACES                   TO   ERR-SEGM
               PERFORM   9999-ROTINA-ERRO
           END-IF.

       3090-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3100-SELECT-RUECB09F      SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
           FETCH  CSR01-RUECB09F
           INTO   :RUECB09F.CPRODT-RURAL
                       :WRK-B09F-CPRODT-NULL
           END-EXEC.

           IF (SQLCODE   NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0      EQUAL 'W')
               MOVE 'DB2'                TO  ERR-TIPO-ACESSO
               MOVE 'TPRODT_RURAL_RECOR' TO  ERR-DBD-TAB
               MOVE 'SELECT'             TO  ERR-FUN-COMANDO
               MOVE SQLCODE              TO  ERR-SQL-CODE
               MOVE '3100'               TO  ERR-LOCAL
               MOVE SPACES               TO  ERR-SEGM
           END-IF.

           IF SQLCODE EQUAL +100
              MOVE 'S' TO WRK-CODE100-B09F
           END-IF.

           IF  SQLCODE   EQUAL ZEROS
               PERFORM 3101-B09F-VER-NULOS
           END-IF.

      *---------------------------------------------------------------*
       3100-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3101-B09F-VER-NULOS    SECTION.
      *---------------------------------------------------------------*

           IF WRK-B09F-CPRODT-NULL  LESS ZEROS
              MOVE ZEROS  TO CPRODT-RURAL    OF RUECB09F
           END-IF.
      *---------------------------------------------------------------*
       3101-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3110-CLOSE-CSR01-RUECB09F SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR01-RUECB09F
           END-EXEC.

           IF (SQLCODE           NOT EQUAL   +0)    OR
              (SQLWARN0              EQUAL  'W')
               MOVE 'DB2'                    TO   ERR-TIPO-ACESSO
               MOVE 'TPRODT_RURAL_RECOR'     TO   ERR-DBD-TAB
               MOVE 'CLOSE   '               TO   ERR-FUN-COMANDO
               MOVE SQLCODE                  TO   ERR-SQL-CODE
               MOVE '3110'                   TO   ERR-LOCAL
               MOVE SPACES                   TO   ERR-SEGM
               PERFORM   9999-ROTINA-ERRO

      *---------------------------------------------------------------*
       3110-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3120-COUNT-RUECB097       SECTION.
      *---------------------------------------------------------------*

           MOVE CJUNC-DEPDC  OF RUFIB002 TO CJUNC-DEPDC
                                         OF RUECB097.
           MOVE CCDULA       OF RUFIB002 TO CCDULA-CREDT-RURAL
                                          OF RUECB097.

           EXEC SQL
           SELECT  COUNT(*)
           INTO  :WRK-QTDREG-B097
           FROM  DB2PRD.TCDULA_COMCZ
           WHERE CJUNC_DEPDC          = :RUECB097.CJUNC-DEPDC
           AND   CCDULA_CREDT_RURAL   = :RUECB097.CCDULA-CREDT-RURAL
           END-EXEC.
           IF (SQLCODE   NOT EQUAL ZEROS)  OR
              (SQLWARN0      EQUAL 'W')
               MOVE 'DB2'                TO  ERR-TIPO-ACESSO
               MOVE 'TCDULA_COMCZ'       TO  ERR-DBD-TAB
               MOVE 'COUNT'              TO  ERR-FUN-COMANDO
               MOVE SQLCODE              TO  ERR-SQL-CODE
               MOVE '3120'               TO  ERR-LOCAL
               MOVE SPACES               TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       3120-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       5000-SELECIONAR           SECTION.
      *---------------------------------------------------------------*

           PERFORM 3010-FETCH-CSR01-RUECB09B.

              GO      TO    5000-99-FIM
           END-IF.

           IF CMSGEM-RECOR OF RUECB09B  NOT EQUAL  'COR0001  '
              ADD 1  TO  ACU-DESPR-B09B
              GO TO 5000-99-FIM
           END-IF.

           IF CCONTR-CREDT-RURAL OF RUECB09B  EQUAL  WRK-CONTR
              ADD 1 TO ACU-DESPR-B09B
              GO      TO    5000-99-FIM
           ELSE
              MOVE CCONTR-CREDT-RURAL OF RUECB09B  TO  WRK-CONTR
           END-IF.

           PERFORM 5500-SELECT-RUFIB002.

           IF SQLCODE  EQUAL  +100
              ADD 1  TO  ACU-NACHOU-B002
              GO     TO  5000-99-FIM
           ELSE
                 ADD 1  TO  ACU-DESPR-B002
                 GO     TO  5000-99-FIM
              END-IF
              IF  CORIGE-REC         OF RUFIB002  NOT  LESS  60
              AND CORIGE-REC         OF RUFIB002  NOT  GREATER  81
                 ADD 1  TO  ACU-DESPR-B002
                 GO     TO  5000-99-FIM
              END-IF
           END-IF.

           PERFORM 5100-GERAR-ARQACUMU.

      *---------------------------------------------------------------*
       5000-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       5100-GERAR-ARQACUMU  SECTION.
      *---------------------------------------------------------------*

           PERFORM 5200-OBTER-DADOS-B099.
           PERFORM 5300-FORMATA-CONTROLE.

           PERFORM 5310-FORMATA-CONTRATO.

      *
      * FORMATA EMITENTE
      *
           MOVE 'N'              TO  WRK-CODE100-B09E.

           MOVE 'E'                             TO CTPO-RESP-PSSOA
                                                OF RUECB09E.

           PERFORM 3060-OPEN-CSR01-RUECB09E.
           PERFORM VARYING  WRK-IND-B09E     FROM 1 BY 1
                   UNTIL WRK-CODE100-B09E  EQUAL 'S'
                   PERFORM 3070-SELECT-RUECB09E
                   PERFORM 5320-FORMATA-EMITENTE
           END-PERFORM.

           MOVE ZEROS TO WRK-SEQREG.
           MOVE 'N'   TO WRK-CODE100-B09E.
           PERFORM 3080-CLOSE-CSR01-RUECB09E.

           PERFORM 5330-FORMATA-DESTFIN.

      *
      * FORMATA PRODUTO CONSORCIO
      *
           MOVE 'N'              TO  WRK-CODE100-B09F.

           PERFORM 3090-OPEN-CSR01-RUECB09F.
           PERFORM VARYING  WRK-IND-B09F     FROM 1 BY 1
                   UNTIL WRK-CODE100-B09F  EQUAL 'S'
                   PERFORM 3100-SELECT-RUECB09F
                   PERFORM 5340-FORMATA-PRODCONS
           END-PERFORM.

           MOVE ZEROS TO WRK-SEQREG.
           MOVE 'N'   TO WRK-CODE100-B09F.
           MOVE ZEROS TO WRK-IND-B09F.
           PERFORM 3110-CLOSE-CSR01-RUECB09F.


      *
      * FORMATA PROPRIETARIO
      *
           MOVE 'N'              TO  WRK-CODE100-B09E.

           MOVE 'P'                             TO CTPO-RESP-PSSOA
                                                OF RUECB09E.

           PERFORM 3060-OPEN-CSR01-RUECB09E.
           PERFORM VARYING  WRK-IND-B09E     FROM 1 BY 1
                   UNTIL WRK-CODE100-B09E  EQUAL 'S'
                   PERFORM 3070-SELECT-RUECB09E
                   PERFORM 5360-FORMATA-PROPRIET
           END-PERFORM.

           MOVE ZEROS TO WRK-SEQREG.
           MOVE 'N'   TO WRK-CODE100-B09E.
           MOVE ZEROS TO WRK-IND-B09E.
           PERFORM 3080-CLOSE-CSR01-RUECB09E.


           PERFORM 5380-FORMATA-RECEITA.

      *
      * FORMATA PARCELA
      *
           MOVE 'N'              TO  WRK-CODE100-B09D.

           PERFORM 3030-OPEN-CSR01-RUECB09D.
           PERFORM VARYING  WRK-IND-B09D     FROM 1 BY 1
                   UNTIL WRK-CODE100-B09D  EQUAL 'S'
                   PERFORM 3040-SELECT-RUECB09D
                   PERFORM 5390-FORMATA-PARCELA
           END-PERFORM.

           MOVE ZEROS TO WRK-SEQREG.
           MOVE 'N'   TO WRK-CODE100-B09D.
           MOVE ZEROS TO WRK-IND-B09D.
           PERFORM 3050-CLOSE-CSR01-RUECB09D.

           PERFORM 5400-FORMATA-FORNECED.
           PERFORM 5420-FORMATA-DATAMOV.

      *---------------------------------------------------------------*
       5100-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       5200-OBTER-DADOS-B099     SECTION.
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
                  VPCELA_LIBRC,
                  VPCELA_REC_PPRIO,
                  PALIQT_PROAGRO,
                  PJURO_ENCAR_FINCR,
                  CTPO_ENCAR_COMPL,
                  CTSORO_NACIO,
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
                  :RUECB099.CCTRL-CNPJ-ENTID,
                  :RUECB099.CCNPJ-PARTC,
                  :RUECB099.CFLIAL-CNPJ-PARTC,
                  :RUECB099.CCTRL-CNPJ-PARTC,
                  :RUECB099.CSIT-OPER-RURAL,
                  :RUECB099.DCDULA
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
                  :RUECB099.CMUN-REFT-BACEN
                  :WRK-B099-CMUN-NULL,
                  :RUECB099.CEMPTO-BACEN,
                  :RUECB099.CSIST-PROD,
                  :RUECB099.VPCELA-LIBRC
                  :WRK-B099-LIBRC-NULL,
                  :RUECB099.VPCELA-REC-PPRIO
                  :WRK-B099-REC-NULL,
                  :RUECB099.PALIQT-PROAGRO,
                  :RUECB099.PJURO-ENCAR-FINCR,
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
                  :WRK-B099-GARNT-NULL,
                  :RUECB099.VRECTA-BRUTA-RECOR,
                  :RUECB099.DMOVTO-RECOR
           FROM  DB2PRD.TCONTR_ENVIO_RECOR
           WHERE CCONTR_CREDT_RURAL   = :RUECB099.CCONTR-CREDT-RURAL
           AND   NENVIO_RECOR         = :RUECB099.NENVIO-RECOR

           IF (SQLCODE   NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0      EQUAL 'W')
               MOVE 'DB2'                TO ERR-TIPO-ACESSO
               MOVE 'TCONTR_ENVIO_RECOR' TO ERR-DBD-TAB
               MOVE 'SELECT'             TO ERR-FUN-COMANDO
               MOVE SQLCODE              TO ERR-SQL-CODE
               MOVE '5200'               TO ERR-LOCAL
               MOVE SPACES               TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF (SQLCODE       EQUAL  +100)
               MOVE 'DB2'                TO ERR-TIPO-ACESSO
               MOVE 'TCONTR_ENVIO_RECOR' TO ERR-DBD-TAB
               MOVE 'SELECT'             TO ERR-FUN-COMANDO
               MOVE SQLCODE              TO ERR-SQL-CODE
               MOVE '520A'               TO ERR-LOCAL
               MOVE SPACES               TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.
           IF  SQLCODE   EQUAL ZEROS
               PERFORM 5201-B099-VER-NULOS
           END-IF.

      *----------------------------------------------------------------*
       5200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * VER NULIDADE DOS CAMPOS                                        *
      *----------------------------------------------------------------*
       5201-B099-VER-NULOS            SECTION.
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
      *----------------------------------------------------------------*
       5201-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       5300-FORMATA-CONTROLE           SECTION.
      *---------------------------------------------------------------*

           MOVE  SPACES TO RUEC81-REGISTRO.

      *
      * FORMATA CHAVE
      *
           MOVE CCONTR-CREDT-RURAL OF RUECB09B
                                   TO RUEC81-CCONTR.

           MOVE ZEROS              TO RUEC81-TPREG.

           MOVE ZEROS              TO RUEC81-SEQREG.

      *
      *

           MOVE CREFT-RECOR        OF RUECB09B
                                   TO RUEC81-CREFT-RECOR.

           MOVE DCDULA             OF RUECB099
                                   TO RUEC81-DIMPRE-4VIAS.

           MOVE 01                 TO RUEC81-CSIT-RECOR.

           WRITE FD-REG-ARQACUMU  FROM  RUEC81-REGISTRO.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.
           PERFORM 0200-TESTAR-FS-ARQACUMU.

           ADD   1            TO  ACU-GRAVA-ARQACUMU.

      *---------------------------------------------------------------*
       5300-99-FIM. EXIT.
      *---------------------------------------------------------------*

       5310-FORMATA-CONTRATO           SECTION.
      *---------------------------------------------------------------*

           MOVE  SPACES TO RUEC81-REGISTRO.

      *
      * FORMATA CHAVE
      *
           MOVE CCONTR-CREDT-RURAL OF RUECB09B
                                   TO RUEC81-CCONTR.

           MOVE 01                 TO RUEC81-TPREG.

           MOVE ZEROS              TO RUEC81-SEQREG.

      *
      * FORMATA DADOS

           MOVE CTPO-CDULA-RECOR   TO WRK-COD3-CHAR.
           MOVE WRK-COD3-CHAR1     TO RUEC81-TIPO-CEDULA.

                                   TO RUEC81-DEMISSAO.

           MOVE DVCTO-CONTR        OF RUECB099
                                   TO RUEC81-DVCTO-CTR-RECOR.

           MOVE CCDULA-RECOR       OF RUECB099
                                   TO RUEC81-NUM-CEDULA.

           MOVE CTPO-INSTT-CREDT OF RUECB099
                                   TO RUEC81-TIPO-CRED.

           MOVE VFINAN             OF RUECB099
                                   TO RUEC81-VFINAN-RECOR.

           MOVE CCATEG-PROTR-RECOR OF RUECB099
                                   TO RUEC81-CATEG-EMIT.

           WRITE FD-REG-ARQACUMU  FROM  RUEC81-REGISTRO.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.
           PERFORM 0200-TESTAR-FS-ARQACUMU.
           ADD   1            TO  ACU-GRAVA-ARQACUMU.

      *---------------------------------------------------------------*
       5310-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       5320-FORMATA-EMITENTE           SECTION.
      *---------------------------------------------------------------*

           IF SQLCODE EQUAL +100
              GO TO 5320-99-FIM
           END-IF.

           MOVE  SPACES TO RUEC81-REGISTRO.

      *
      * FORMATA CHAVE
      *
           MOVE CCONTR-CREDT-RURAL OF RUECB09B
                                   TO RUEC81-CCONTR.
           MOVE 03                 TO RUEC81-TPREG.

           ADD  1                  TO WRK-SEQREG.
           MOVE WRK-SEQREG         TO RUEC81-SEQREG.

      *
      * FORMATA DADOS
      *

      * GRUPO EMIT TAB B09E
           IF  CTPO-PSSOA  EQUAL  1
               MOVE  'F'               TO  RUEC81-TIP-PES-EMIT
               MOVE CCNPJ-CPF          TO  WRK-COD9-NUM
               MOVE CFLIAL-CNPJ        TO  WRK-COD5-NUM
               MOVE CCTRL-CNPJ-CPF     TO  WRK-COD3-NUM
               MOVE WRK-COD9-NUM       TO  RUEC81-CPF-CNPJ-EMIT
               MOVE WRK-COD5-NUM4      TO  RUEC81-FIL-CNPJ-EMIT
               MOVE WRK-COD3-NUM2      TO  RUEC81-CTRL-EMIT
           ELSE
               MOVE  'J'               TO  RUEC81-TIP-PES-EMIT
               MOVE CCNPJ-CPF          TO  WRK-COD9-NUM
               MOVE CCTRL-CNPJ-CPF     TO  WRK-COD3-NUM
               MOVE WRK-COD9-NUM       TO  RUEC81-CPF-CNPJ-EMIT
               MOVE WRK-COD5-NUM4      TO  RUEC81-FIL-CNPJ-EMIT
               MOVE WRK-COD3-NUM2      TO  RUEC81-CTRL-EMIT
           END-IF.

           WRITE FD-REG-ARQACUMU  FROM  RUEC81-REGISTRO.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.
           PERFORM 0200-TESTAR-FS-ARQACUMU.

           ADD   1            TO  ACU-GRAVA-ARQACUMU.

      *---------------------------------------------------------------*
       5320-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       5330-FORMATA-DESTFIN            SECTION.
      *---------------------------------------------------------------*


      *
      * FORMATA CHAVE
      *
           MOVE CCONTR-CREDT-RURAL OF RUECB09B
                                   TO RUEC81-CCONTR.

           MOVE 05                 TO RUEC81-TPREG.

           MOVE ZEROS              TO RUEC81-SEQREG.

      *
      * FORMATA DADOS
      *

           MOVE CPROG-RURAL        OF RUECB099
                                   TO RUEC81-CPROG-RECOR.

           MOVE CTPO-FONTE-RECOR   OF RUECB099
                                   TO RUEC81-FONTE-REC.

                                   TO RUEC81-CMUN-RECOR.

           WRITE FD-REG-ARQACUMU  FROM  RUEC81-REGISTRO.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.
           PERFORM 0200-TESTAR-FS-ARQACUMU.

           ADD   1            TO  ACU-GRAVA-ARQACUMU.

      *---------------------------------------------------------------*
       5330-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       5340-FORMATA-PRODCONS           SECTION.
      *---------------------------------------------------------------*

           IF SQLCODE EQUAL +100
              GO TO 5340-99-FIM
           END-IF.


      *
      * FORMATA CHAVE
      *
           MOVE CCONTR-CREDT-RURAL OF RUECB09B
                                   TO RUEC81-CCONTR.

           MOVE 08                 TO RUEC81-TPREG.

           ADD  1                  TO WRK-SEQREG.
           MOVE WRK-SEQREG         TO RUEC81-SEQREG.

      *
      * FORMATA DADOS
      *

           MOVE CPRODT-RURAL       OF RUECB09F
                                   TO RUEC81-PROD-CONS.

           WRITE FD-REG-ARQACUMU  FROM  RUEC81-REGISTRO.

           PERFORM 0200-TESTAR-FS-ARQACUMU.

           ADD   1            TO  ACU-GRAVA-ARQACUMU.

      *---------------------------------------------------------------*
       5340-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       5350-FORMATA-EMPREEND           SECTION.
      *---------------------------------------------------------------*

           MOVE SPACES TO RUEC81-REGISTRO.

      *
      * FORMATA CHAVE
      *
           MOVE CCONTR-CREDT-RURAL OF RUECB09B
                                   TO RUEC81-CCONTR.

           MOVE 13                 TO RUEC81-TPREG.
           MOVE ZEROS              TO RUEC81-SEQREG.

      *
      * FORMATA DADOS
      *

           MOVE CEMPTO-BACEN       OF RUECB099
                                   TO RUEC81-CEMPTO-RECOR.

           MOVE CSIST-PROD         OF RUECB099
                                   TO RUEC81-SIST-PROD.
           IF  RUEC81-SIST-PROD(10:02)  EQUAL SPACES OR LOW-VALUES
               MOVE '00'           TO RUEC81-SIST-PROD(10:02)
           END-IF

           MOVE VPCELA-LIBRC            OF RUECB099
                                        TO WRK-VPCELA-LIBRC-CSINAL.
           MOVE WRK-VPCELA-LIBRC-SSINAL TO RUEC81-VL-CREDITO.

           MOVE VPCELA-REC-PPRIO      OF RUECB099
                                      TO WRK-VPCELA-REC-CSINAL.

           MOVE PJURO-ENCAR-FINCR  OF RUECB099
                                   TO WRK-PERC-FINANC-CSINAL.
           MOVE WRK-PERC-FINANC-SSINAL TO RUEC81-PERC-FINANC.

           MOVE CTPO-ENCAR-COMPL   OF RUECB099
                                   TO RUEC81-TIP-ENC-FIN.

           MOVE QAREA-RECOR        OF RUECB099
                                   TO WRK-QAREA-CSINAL.
           MOVE WRK-QAREA-SSINAL   TO RUEC81-AREA-FINCD.

           MOVE QUND-FINCD-RURAL       OF RUECB099
                                       TO WRK-QUND-FINCD-CSINAL.
           MOVE WRK-QUND-FINCD-SSINAL  TO RUEC81-ITEM-FINCD.

           MOVE QUND-PROD-PROVV      OF RUECB099
                                     TO WRK-QUND-PROD-CSINAL.
           MOVE WRK-QUND-PROD-SSINAL TO RUEC81-PROD-PROVV.

           MOVE  DINIC-SAFRA  OF RUECB099  TO WRK-COD6-NUM.
           MOVE  WRK-AUX6-CHAR4     TO WRK-DINIC-SAFRA.
           MOVE  DFIM-SAFRA   OF RUECB099  TO WRK-COD6-NUM.
           MOVE  WRK-COD6-CHAR      TO WRK-AUX6-CHAR.
           MOVE  WRK-AUX6-CHAR4     TO WRK-DFIM-SAFRA.
           MOVE  WRK-SAFRA          TO RUEC81-SAFRA.

           MOVE CTPO-GARNT-RECOR   OF RUECB099
                                   TO WRK-AUX3-NUM.
           MOVE WRK-AUX3-NUM1      TO RUEC81-TIP-GARNT.

           WRITE FD-REG-ARQACUMU  FROM  RUEC81-REGISTRO.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.
           PERFORM 0200-TESTAR-FS-ARQACUMU.

           ADD   1            TO  ACU-GRAVA-ARQACUMU.

      *---------------------------------------------------------------*
       5350-99-FIM. EXIT.
      *---------------------------------------------------------------*

       5360-FORMATA-PROPRIET           SECTION.
      *---------------------------------------------------------------*

           IF SQLCODE EQUAL +100
              GO TO 5360-99-FIM
           END-IF.

           MOVE SPACES TO RUEC81-REGISTRO.

      *
      * FORMATA CHAVE
      *
           MOVE CCONTR-CREDT-RURAL OF RUECB09B
                                   TO RUEC81-CCONTR.

           MOVE 15                 TO RUEC81-TPREG.

           ADD  1                  TO WRK-SEQREG.
           MOVE WRK-SEQREG         TO RUEC81-SEQREG.

      *
      *

           IF  CTPO-PSSOA  EQUAL  1
               MOVE  'F'               TO  RUEC81-TIP-PES-PROP
               MOVE CCNPJ-CPF          TO  WRK-COD9-NUM
               MOVE CFLIAL-CNPJ        TO  WRK-COD5-NUM
               MOVE CCTRL-CNPJ-CPF     TO  WRK-COD3-NUM
               MOVE WRK-COD9-NUM       TO  RUEC81-CPF-CNPJ-PROP
               MOVE WRK-COD5-NUM4      TO  RUEC81-FIL-CNPJ-PROP
               MOVE WRK-COD3-NUM2      TO  RUEC81-CTRL-PROP
           ELSE
               MOVE  'J'               TO  RUEC81-TIP-PES-PROP
               MOVE CCNPJ-CPF          TO  WRK-COD9-NUM
               MOVE CFLIAL-CNPJ        TO  WRK-COD5-NUM
               MOVE CCTRL-CNPJ-CPF     TO  WRK-COD3-NUM
               MOVE WRK-COD9-NUM       TO  RUEC81-CPF-CNPJ-PROP
               MOVE WRK-COD5-NUM4      TO  RUEC81-FIL-CNPJ-PROP
               MOVE WRK-COD3-NUM2      TO  RUEC81-CTRL-PROP
           END-IF.

           WRITE FD-REG-ARQACUMU  FROM  RUEC81-REGISTRO.
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.
           PERFORM 0200-TESTAR-FS-ARQACUMU.

           ADD   1            TO  ACU-GRAVA-ARQACUMU.

      *---------------------------------------------------------------*
       5360-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       5370-FORMATA-TPGARAN            SECTION.
      *---------------------------------------------------------------*

           MOVE SPACES TO RUEC81-REGISTRO.

      *
      * FORMATA CHAVE
      *
           MOVE CCONTR-CREDT-RURAL OF RUECB09B
                                   TO RUEC81-CCONTR.


           MOVE ZEROS              TO RUEC81-SEQREG.

      *
      * FORMATA DADOS
      *


           MOVE CTPO-GARNT-RECOR   OF RUECB099
                                   TO WRK-AUX3-NUM.
           MOVE WRK-AUX3-NUM1      TO RUEC81-TPGARNT.

           WRITE FD-REG-ARQACUMU  FROM  RUEC81-REGISTRO.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.
           PERFORM 0200-TESTAR-FS-ARQACUMU.

           ADD   1            TO  ACU-GRAVA-ARQACUMU.

      *---------------------------------------------------------------*
       5370-99-FIM. EXIT.

      *---------------------------------------------------------------*
       5380-FORMATA-RECEITA            SECTION.
      *---------------------------------------------------------------*

           MOVE SPACES TO RUEC81-REGISTRO.

      *
      * FORMATA CHAVE
      *
           MOVE CCONTR-CREDT-RURAL OF RUECB09B
                                   TO RUEC81-CCONTR.

           MOVE 18                 TO RUEC81-TPREG.

           MOVE ZEROS              TO RUEC81-SEQREG.

      *
      * FORMATA DADOS
      *

                                   TO WRK-VRECTA-CSINAL.
           MOVE WRK-VRECTA-SSINAL  TO RUEC81-RECTA-BRUTA.

           WRITE FD-REG-ARQACUMU  FROM  RUEC81-REGISTRO.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.
           PERFORM 0200-TESTAR-FS-ARQACUMU.

           ADD   1            TO  ACU-GRAVA-ARQACUMU.

      *---------------------------------------------------------------*
       5380-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       5390-FORMATA-PARCELA            SECTION.
      *---------------------------------------------------------------*

           IF SQLCODE EQUAL +100
              GO TO 5390-99-FIM
           END-IF.
           MOVE SPACES TO RUEC81-REGISTRO.

      *
      * FORMATA CHAVE
      *
           MOVE CCONTR-CREDT-RURAL OF RUECB09B
                                   TO RUEC81-CCONTR.

           MOVE 20                 TO RUEC81-TPREG.

           ADD  1                  TO WRK-SEQREG.
           MOVE WRK-SEQREG         TO RUEC81-SEQREG.

      *
      * FORMATA DADOS
      *

           MOVE NPCELA-CONTR       OF RUECB09D
                                   TO RUEC81-NUM-PARC

           MOVE DVCTO-PCELA-CONTR  OF RUECB09D

           MOVE VPCELA-RURAL       OF RUECB09D
                                   TO RUEC81-VLPRIN-RECOR.

           WRITE FD-REG-ARQACUMU  FROM  RUEC81-REGISTRO.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.
           PERFORM 0200-TESTAR-FS-ARQACUMU.

           ADD   1            TO  ACU-GRAVA-ARQACUMU.

      *---------------------------------------------------------------*
       5390-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       5400-FORMATA-FORNECED           SECTION.
      *---------------------------------------------------------------*

            PERFORM  3120-COUNT-RUECB097.

            IF WRK-QTDB097  EQUAL  ZEROS
               GO TO 5400-99-FIM
            END-IF.

      * FORMATA NOVOS FORNECEDORES
       5400-00-NOVOFORN.

            INITIALIZE  RUEC79-REGISTRO.

            MOVE  'RUEC1590'              TO RUEC79-PROGRAMA.
            MOVE  CJUNC-DEPDC OF RUFIB002 TO RUEC79-CJUNC-DEPDC.
            MOVE  CCDULA      OF RUFIB002 TO RUEC79-CCDULA.
            MOVE  WRK-NITEM-ENV           TO RUEC79-NITEM-ENV.
            MOVE  999                     TO RUEC79-QTD-PESQ.

            CALL WRK-MODULO-RUEC8822  USING  RUEC79-REGISTRO
                                             ERRO-AREA
                                             WRK-SQLCA.

      ***************************************************************
      * RUEC79-COD-RET:
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
               GO TO 5400-99-FIM
           END-IF.

           MOVE RUEC79-QTD-RET   TO  WRK-QTD-SSINAL.
           MOVE WRK-QTD-CSINAL   TO  WRK-QTD-PESQ.

           MOVE  1  TO  WRK-IND-OCOR.
           UNTIL   WRK-IND-OCOR      GREATER WRK-QTD-PESQ
                   PERFORM 5410-FORMATA-OCORR-FORN
           END-PERFORM.

           IF WRK-QTDB097  GREATER  WRK-AUXTOT
              ADD  999 TO WRK-NITEM-ENV
              ADD  999 TO WRK-AUXTOT-COMP
              MOVE WRK-AUXTOT-COMP TO WRK-AUXTOT
              GO TO  5400-00-NOVOFORN
           END-IF.

           MOVE ZEROS TO WRK-QTDREG-B097.
           MOVE 999   TO WRK-AUXTOT.
           MOVE 999   TO WRK-AUXTOT-COMP.
           MOVE ZEROS TO WRK-NITEM-ENV.
           MOVE ZEROS TO WRK-IND-OCOR.
           MOVE ZEROS TO WRK-SEQREG.

      *---------------------------------------------------------------*
       5400-99-FIM. EXIT.
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
       5410-FORMATA-OCORR-FORN         SECTION.
      *---------------------------------------------------------------*

           MOVE SPACES TO RUEC81-REGISTRO.

      *
      * FORMATA CHAVE
      *
           MOVE CCONTR-CREDT-RURAL OF RUECB09B
                                   TO RUEC81-CCONTR.

           MOVE 23                 TO RUEC81-TPREG.

           ADD  1                  TO WRK-SEQREG.
           MOVE WRK-SEQREG         TO RUEC81-SEQREG.

      *
      * FORMATA DADOS
      *
           IF  RUEC79-CFLIAL-PROTR(WRK-IND-OCOR)  EQUAL  ZEROS
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

                                   TO RUEC81-CMUN-COMPRA.

           MOVE RUEC79-QUND-FINCR(WRK-IND-OCOR)
                                   TO RUEC81-QTD-COMPRA.

           MOVE RUEC79-SAFRA-INI-FIM(WRK-IND-OCOR)
                                   TO RUEC81-SAFRA-COMPRA.

           WRITE FD-REG-ARQACUMU  FROM  RUEC81-REGISTRO.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.
           PERFORM 0200-TESTAR-FS-ARQACUMU.

           ADD   1            TO  ACU-GRAVA-ARQACUMU.

      *---------------------------------------------------------------*
       5410-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       5420-FORMATA-DATAMOV            SECTION.

           MOVE SPACES TO  RUEC81-REGISTRO.

      *
      * FORMATA CHAVE
      *
           MOVE CCONTR-CREDT-RURAL OF RUECB09B
                                   TO RUEC81-CCONTR.

           MOVE 25                 TO RUEC81-TPREG.

           MOVE ZEROS              TO RUEC81-SEQREG.

      *
      * FORMATA DADOS
      *
           MOVE DMOVTO-RECOR OF RUECB099
                                   TO RUEC81-DMOVTO.

           WRITE FD-REG-ARQACUMU  FROM  RUEC81-REGISTRO.

           PERFORM 0200-TESTAR-FS-ARQACUMU.

           ADD   1            TO  ACU-GRAVA-ARQACUMU.

      *---------------------------------------------------------------*
       5420-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       5500-SELECT-RUFIB002             SECTION.
      *---------------------------------------------------------------*

           MOVE CCONTR-CREDT-RURAL  OF RUECB09B  TO CCONTR
                                                 OF RUFIB002.

           EXEC SQL
           SELECT CCDULA,
                  CJUNC_DEPDC,
                  CSTTUS_CONTR_RURAL,
                  CTPO_CDULA_RECOR
                , CORIGE_REC
                  :RUFIB002.CJUNC-DEPDC,
                  :RUFIB002.CSTTUS-CONTR-RURAL,
                  :RUFIB002.CTPO-CDULA-RECOR
               ,  :RUFIB002.CORIGE-REC  :WRK-B002-CORIGE-NULL
           FROM  DB2PRD.CONTRATO_RURAL
           WHERE CCONTR               = :RUFIB002.CCONTR
           END-EXEC.

           IF (SQLCODE   NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0      EQUAL 'W')
               MOVE 'DB2'                TO ERR-TIPO-ACESSO
               MOVE 'CONTRATO_RURAL'     TO ERR-DBD-TAB
               MOVE 'SELECT'             TO ERR-FUN-COMANDO
               MOVE SQLCODE              TO ERR-SQL-CODE
               MOVE '5500'               TO ERR-LOCAL
               MOVE SPACES               TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  WRK-B002-CORIGE-NULL  LESS  ZEROS
               MOVE ZEROS                 TO CORIGE-REC OF RUFIB002

      *---------------------------------------------------------------*
       5500-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       7000-DISPLAY-SYSOUT  SECTION.
      *---------------------------------------------------------------*

           CLOSE   ARQACUMU.

           MOVE  WRK-FECHAMENTO        TO  WRK-OPERACAO.
           PERFORM 0100-TESTAR-FILE-STATUS.

           DISPLAY '************************** RUEC1590 ****************
      -            '***********'.
           DISPLAY '*'.
           DISPLAY '* TOTAL REGS LIDOS RUECB09B.........................
      -            ': '  ACU-LIDOS-B09B.
           DISPLAY '*'.
           DISPLAY '* TOTAL REGS DESPR RUECB09B.........................
           DISPLAY '*'.
           DISPLAY '* TOTAL REGS NAO ENCONTRADOS RUFIB002...............
      -            ': '  ACU-NACHOU-B002.
           DISPLAY '*'.
           DISPLAY '* TOTAL REGS DESPR RUFIB002.........................
      -            ': '  ACU-DESPR-B002.
           DISPLAY '*'.
           DISPLAY '* TOTAL REGS GERADOS ARQUIVO ARQACUMU...............
      -            ': '  ACU-GRAVA-ARQACUMU.
           DISPLAY '*'.
           DISPLAY '************************** RUEC1590 ****************
      -            '***********'.

      *---------------------------------------------------------------*
       7000-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       9999-ROTINA-ERRO  SECTION.
      *---------------------------------------------------------------*


           CALL  'POOL7100'     USING    WRK-BATCH
                                         ERRO-AREA
                                         SQLCA
           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM. EXIT.
      *----------------------------------------------------------------*
