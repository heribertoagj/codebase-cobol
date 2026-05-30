      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. RUEC8818
       AUTHOR.     SONDAIT.
      *================================================================*
      *                    S O N D A  P R O C W O R K                  *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   RUEC8818                                    *
      *    ANALISTA....:   SONDAIT                                     *
      *    DATA........:   08/2012                                     *
      *    PROJETO.....:   PWT 12/9931    2012/81536/5-01              *
      *                    REFORMULACAO MENSAGERIA BACEN - RECOR       *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   GERAR SISMSG COR0002 P/ ENVIO SISTEMA RCOR  *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#RUEC75 - AREA DE RETORNO DO MODULO RUEC8818               *
      *    I#RCOR70 - AREA DE RETORNO DO MODULO RCOR7000               *
      *    I#BRAD7C - AREA PARA TRATAMENTO DE ERRO DB2.                *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    BRAD7100 - VERIFICAR DISPONIBILIDADE                        *
      *    RUEC8818 - FORMATA/ENVIA SISMSG COR0002                     *
      *                                                                *
      *    TABELA                                                      *
      *    TENVIO_RECOR_TEMPR                        RUECB09A          *
      *    THIST_ENVIO_RECOR                         RUECB09B          *
      *    TCONTR_ENVIO_RECOR                        RUECB099          *
      *================================================================*

      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

       CONFIGURATION                   SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *---------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       77  FILLER                      PIC X(079)          VALUE
           '*** INICIO DA WORKING RUEC8818 ***'.
      *---------------------------------------------------------------*
       77  FILLER                      PIC X(079)          VALUE
           '*** VARIAVEIS AUXILIARES ***'.
      *---------------------------------------------------------------*

       77  WRK-LNK-SQLCA               PIC X(136) VALUE SPACES.

       77  WRK-TAM                 PIC  9(05) COMP-3 VALUE ZEROS.

       77  WRK-MODULO-RCOR5010     PIC  X(08) VALUE 'RCOR5010'.
       77  WRK-TAMANHO-SISMSG      PIC S9(4)  COMP VALUE ZEROS.
       77  WRK-MAX-NREG      PIC S9(05) COMP-3 VALUE ZEROS.

      *---------------------------------------------------------------*


       01  WRK-COD9-NUM        PIC  9(9)        VALUE ZEROS.
       01  FILLER              REDEFINES WRK-COD9-NUM.
           05 FILLER           PIC  9(1).
           05 WRK-COD9-NUM8    PIC  9(8).

       01  WRK-COD5-NUM        PIC  9(5)        VALUE ZEROS.
       01  FILLER              REDEFINES WRK-COD5-NUM.
           05 FILLER           PIC  9(1).
           05 WRK-COD5-NUM4    PIC  9(4).

       01  WRK-COD3-NUM        PIC  9(3)        VALUE ZEROS.
       01  FILLER              REDEFINES WRK-COD3-NUM.
           05 FILLER           PIC  9(1).
           05 WRK-COD3-NUM2    PIC  9(2).

       01  WRK-EDIT-CNPJ.
           05 WRK-EDIT-NCNPJ       PIC  9(08) VALUE ZEROS.
           05 WRK-EDIT-FCNPJ       PIC  9(04) VALUE ZEROS.
           05 WRK-EDIT-CCNPJ       PIC  9(02) VALUE ZEROS.
       01  FILLER               REDEFINES WRK-EDIT-CNPJ.
           05 WRK-EDIT-CNPJ-R      PIC  9(14).

       01  WRK-EDIT-DATA.
           05 WRK-EDIT-AAAA        PIC  9(04) VALUE ZEROS.
           05 WRK-EDIT-MM          PIC  9(02) VALUE ZEROS.
           05 WRK-EDIT-DD          PIC  9(02) VALUE ZEROS.
       01  FILLER               REDEFINES WRK-EDIT-DATA.
           05 WRK-EDIT-DATA-R      PIC  9(08).

       77  WRK-ENVIO-LEN            PIC S9(004) COMP VALUE ZEROS.

       01  WRK-ENVIO-SIS            PIC X(32000)    VALUE SPACES.
       01  WRK-ENVIO-SIS-R          REDEFINES WRK-ENVIO-SIS.
           05 FILLER                PIC X(00009).
           05 WRK-ENVIO-NUMCTRLIF   PIC X(00020).
           05 FILLER                PIC X(31971).

       01  WRK-B09A-SIS            PIC X(32000)     VALUE SPACES.
       01  WRK-B09A-SIS-R          REDEFINES WRK-B09A-SIS.
           05 FILLER               PIC X(00009).
           05 WRK-B09A-NUMCTRLIF   PIC X(00020).
           05 FILLER               PIC X(31971).

       01  WRK-TIMESTAMP           PIC X(26) VALUE SPACES.
       01  WRK-FORMA-DATA          REDEFINES WRK-TIMESTAMP.
           05 WRK-FORMA-AAAA       PIC  9(04).
           05 FILLER               PIC  X(01).
           05 WRK-FORMA-MM         PIC  9(02).
           05 FILLER               PIC  X(01).
           05 WRK-FORMA-DD         PIC  9(02).
           05 FILLER               PIC  X(01).
           05 WRK-FORMA-HORA       PIC  9(02).
           05 FILLER               PIC  X(01).
           05 WRK-FORMA-MIN        PIC  9(02).
           05 FILLER               PIC  X(01).
           05 WRK-FORMA-SEG        PIC  9(02).
           05 FILLER               PIC  X(01).
           05 WRK-FORMA-MILE       PIC  9(06).

       01  WRK-DATA.
           05 WRK-DD          PIC  9(02) VALUE ZEROS.
           05 FILLER          PIC  X(01) VALUE '.' .
           05 WRK-MM          PIC  9(02) VALUE ZEROS.
           05 FILLER          PIC  X(01) VALUE '.' .
           05 WRK-AAAA        PIC  9(04) VALUE ZEROS.

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(079)          VALUE
           '*** AREA DE FORMATACAO P/ MODULO RCOR5010 ***'.
      *---------------------------------------------------------------*

       COPY 'I#RUEC76'.

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(079)          VALUE
           '*** AREA PARA CHAMADA MODULO RCOR5010 ***'.
      *---------------------------------------------------------------*

*******COPY 'I#RCOR10'.

       01 WRK-RCOR10-REGISTRO.
          03 WRK-RCOR10-ENTRADA.
             05 WRK-RCOR10-CCUSTO       PIC  X(00004) VALUE SPACES.
             05 WRK-RCOR10-TAM-SISMSG   PIC S9(00008) COMP VALUE ZEROS.
             05 WRK-RCOR10-SEQ-MSG      PIC  9(00003) VALUE ZEROS.
             05 WRK-RCOR10-IND-CONTINUA PIC  X(00001) VALUE SPACES.
             05 WRK-RCOR10-NRO-CTRLIF   PIC  X(00020) VALUE SPACES.
             05 WRK-RCOR10-SISMSG       PIC  X(30000) VALUE SPACES.
          03 WRK-RCOR10-SAIDA.
             05 WRK-RCOR10-NRO-CTRL-IF  PIC  X(00020) VALUE SPACES.
             05 WRK-RCOR10-COD-RETORNO  PIC  9(00005) VALUE ZEROS.

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(079)          VALUE
           '*** AREA PARA CHAMADA BRAD7600   ***'.
      *---------------------------------------------------------------*

       01  WRK-DATA-HORA.
           03  WRK-DT-JULIANA          PIC 9(05) COMP-3  VALUE ZEROS.
           03  WRK-DT-AAMMDD           PIC 9(07) COMP-3  VALUE ZEROS.
           03  WRK-DT-AAAAMMDD         PIC 9(09) COMP-3  VALUE ZEROS.
           03  WRK-TI-HHMMSS           PIC 9(07) COMP-3  VALUE ZEROS.
           03  WRK-TI-HHMMSSMMMMMM     PIC 9(13) COMP-3  VALUE ZEROS.
           03  WRK-TI-TIMESTAMP        PIC X(20)         VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(079)          VALUE
           '*** AREA PARA FORMATACAO DE ERRO ***'.
      *---------------------------------------------------------------*

       COPY 'I#BRAD7C'.



      *---------------------------------------------------------------*
       01  FILLER                      PIC X(079)          VALUE
           '*** AREA PARA TABELAS DB2 ***'.
      *---------------------------------------------------------------*

           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE RUECB09A
           END-EXEC.

           EXEC SQL
               INCLUDE RUECB09B
           END-EXEC.

           EXEC SQL
               INCLUDE RUECB099
           END-EXEC.


      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE NULIDADE      ***'.
      *----------------------------------------------------------------*

       01  WRK-NULL-B09A.
           05 WRK-CSIT-NULL        PIC S9(04) COMP VALUE +0.
           05 WRK-HINCL-NULL       PIC S9(04) COMP VALUE +0.

       01  WRK-NULL-B09B.
           05 WRK-B09B-CMSGEM-NULL      PIC S9(04) COMP VALUE +0.
           05 WRK-B09B-HINCL-NULL       PIC S9(04) COMP VALUE +0.
           05 WRK-B09B-CCTRL-NULL       PIC S9(04) COMP VALUE +0.
           05 WRK-B09B-CUNIC-NULL       PIC S9(04) COMP VALUE +0.
           05 WRK-B09B-CREFT-NULL       PIC S9(04) COMP VALUE +0.
           05 WRK-B09B-CSIT-NULL        PIC S9(04) COMP VALUE +0.
           05 WRK-B09B-CUSUAR-NULL      PIC S9(04) COMP VALUE +0.

       01  WRK-NULL-B099.
           05 WRK-DCDULA-NULL       PIC S9(04) COMP VALUE +0.
           05 WRK-DVCTO-CONTR-NULL  PIC S9(04) COMP VALUE +0.
           05 WRK-VFINAN            PIC S9(04) COMP VALUE +0.
           05 WRK-CCATEG-PROTR-NULL PIC S9(04) COMP VALUE +0.
           05 WRK-CPROG-RURAL-NULL  PIC S9(04) COMP VALUE +0.
           05 WRK-CMUN-REFT-NULL    PIC S9(04) COMP VALUE +0.
           05 WRK-VPCELA-LIBRC-NULL PIC S9(04) COMP VALUE +0.
           05 WRK-VPCELA-REC-NULL   PIC S9(04) COMP VALUE +0.
           05 WRK-QUND-FINCD-NULL   PIC S9(04) COMP VALUE +0.
           05 WRK-QUND-PROD-NULL    PIC S9(04) COMP VALUE +0.
           05 WRK-DINIC-SAFRA-NULL  PIC S9(04) COMP VALUE +0.
           05 WRK-DFIM-SAFRA-NULL   PIC S9(04) COMP VALUE +0.
           05 WRK-CTPO-GARNT-NULL   PIC S9(04) COMP VALUE +0.

      *---------------------------------------------------------------*
       01  FILLER   PIC X(32) VALUE '* FIM DA WORKING RUEC8818 *'.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       LINKAGE SECTION.
      *---------------------------------------------------------------*

       COPY 'I#RUEC75'.

      *================================================================*
       PROCEDURE DIVISION          USING RUEC75-AREA.
      *================================================================*

      *---------------------------------------------------------------*
       0000-INICIO                 SECTION.
      *---------------------------------------------------------------*

           CALL 'CKRS1000'.

           MOVE ZEROS              TO RUEC75-COD-RET.

           PERFORM  1000-PROCESSAMENTO-PRINCIPAL.

           PERFORM  9000-ROTINA-FIM.

      *---------------------------------------------------------------*
       0000-99-FIM.                EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1000-PROCESSAMENTO-PRINCIPAL SECTION.
      *---------------------------------------------------------------*

           CALL 'POOL7600'  USING  WRK-DATA-HORA.

           PERFORM 2000-VALIDAR-ARG-ENTRADA.

           IF  RUEC75-COD-RET  EQUAL  ZEROS
               IF RUEC75-FUNCAO   EQUAL  'F'
                  PERFORM 3000-OBTEM-DADOS-B099
                  PERFORM 3004-OBTEM-DADOS-B09B
                  PERFORM 3100-FORMATA-SIS1
                  PERFORM 4000-INSERT-B09A
                  PERFORM 4100-ENVIO-RCOR5010
               ELSE
                  IF RUEC75-FUNCAO   EQUAL  'R'
                     PERFORM 4010-SELMAX-B09A
                     PERFORM 3002-OBTEM-DADOS-B09A
                     PERFORM 4100-ENVIO-RCOR5010
                  END-IF
               END-IF
           END-IF.

      *---------------------------------------------------------------*
       1000-99-FIM.                EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2000-VALIDAR-ARG-ENTRADA    SECTION.
      *---------------------------------------------------------------*

           IF  RUEC75-PGM-CHAMADOR EQUAL SPACES
               MOVE 01             TO RUEC75-COD-RET
               MOVE 'PGM CHAMADOR INVALIDO'
                                   TO RUEC75-MENSAGEM
               GO  TO  2000-99-FIM
           END-IF.

           IF (RUEC75-CCONTR       NOT   NUMERIC)
           OR (RUEC75-CCONTR       EQUAL ZEROS)
               MOVE 01             TO RUEC75-COD-RET
               MOVE 'CONTRATO INVALIDO'
                                   TO RUEC75-MENSAGEM
               GO  TO  2000-99-FIM
           END-IF.

           IF (RUEC75-NENVIO-RECOR NOT   NUMERIC)
           OR (RUEC75-NENVIO-RECOR EQUAL ZEROS)
               MOVE 01             TO RUEC75-COD-RET
               MOVE 'NENVIO-RECOR INVALIDO'
                                   TO RUEC75-MENSAGEM
               GO  TO  2000-99-FIM
           END-IF.

           IF  (RUEC75-FUNCAO  NOT EQUAL 'F')
           AND (RUEC75-FUNCAO  NOT EQUAL 'R')
               MOVE 01             TO RUEC75-COD-RET
               MOVE 'FUNCAO DIFERENTE DE F OU R'
                                   TO RUEC75-MENSAGEM
               GO  TO  2000-99-FIM
           END-IF.

      *---------------------------------------------------------------*
       2000-99-FIM.                EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3000-OBTEM-DADOS-B099       SECTION.
      *---------------------------------------------------------------*

           MOVE RUEC75-CCONTR         TO CCONTR-CREDT-RURAL
                                         OF RUECB099.

           MOVE RUEC75-NENVIO-RECOR   TO NENVIO-RECOR
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
                  :WRK-DCDULA-NULL,
                  :RUECB099.DVCTO-CONTR
                  :WRK-DVCTO-CONTR-NULL,
                  :RUECB099.CCDULA-RECOR,
                  :RUECB099.CTPO-INSTT-CREDT,
                  :RUECB099.VFINAN
                  :WRK-VFINAN,
                  :RUECB099.CCATEG-PROTR-RECOR
                  :WRK-CCATEG-PROTR-NULL,
                  :RUECB099.CPROG-RURAL
                  :WRK-CPROG-RURAL-NULL,
                  :RUECB099.CTPO-FONTE-RECOR,
                  :RUECB099.CMUN-REFT-BACEN
                  :WRK-CMUN-REFT-NULL,
                  :RUECB099.CEMPTO-BACEN,
                  :RUECB099.CSIST-PROD,
                  :RUECB099.VPCELA-LIBRC
                  :WRK-VPCELA-LIBRC-NULL,
                  :RUECB099.VPCELA-REC-PPRIO
                  :WRK-VPCELA-REC-NULL,
                  :RUECB099.PALIQT-PROAGRO,
                  :RUECB099.PJURO-ENCAR-FINCR,
                  :RUECB099.CTPO-ENCAR-COMPL,
                  :RUECB099.CTSORO-NACIO,
                  :RUECB099.PRISCO-TSORO-NACIO,
                  :RUECB099.PRISCO-FUNDO-CNSTT,
                  :RUECB099.QAREA-RECOR,
                  :RUECB099.QUND-FINCD-RURAL
                  :WRK-QUND-FINCD-NULL,
                  :RUECB099.CUND-MEDD-FINCD,
                  :RUECB099.QUND-PROD-PROVV
                  :WRK-QUND-PROD-NULL,
                  :RUECB099.CUND-MEDD-PROVV,
                  :RUECB099.DINIC-SAFRA
                  :WRK-DINIC-SAFRA-NULL,
                  :RUECB099.DFIM-SAFRA
                  :WRK-DFIM-SAFRA-NULL,
                  :RUECB099.CTPO-GARNT-RECOR
                  :WRK-CTPO-GARNT-NULL,
                  :RUECB099.VRECTA-BRUTA-RECOR,
                  :RUECB099.DMOVTO-RECOR
           FROM  DB2PRD.TCONTR_ENVIO_RECOR
           WHERE CCONTR_CREDT_RURAL   = :RUECB099.CCONTR-CREDT-RURAL
           AND   NENVIO_RECOR         = :RUECB099.NENVIO-RECOR
           END-EXEC.

           IF  SQLCODE   EQUAL ZEROS
               PERFORM 3001-VER-NULOS-B099
           END-IF.

           IF (SQLCODE       EQUAL  +100)
               MOVE 03                   TO RUEC75-COD-RET
               MOVE 'CONTRATO/NENVIO-RCOR NAO ENCONTRATO B099'
                                         TO RUEC75-MENSAGEM
               MOVE SPACES               TO RUEC75-SQLCA
               MOVE SPACES               TO RUEC75-ERRO-AREA
               PERFORM 9000-ROTINA-FIM
           END-IF.

           IF (SQLCODE   NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0      EQUAL 'W')
               MOVE 99                   TO RUEC75-COD-RET
               MOVE 'ERRO ACESSO DB2'    TO RUEC75-MENSAGEM
               MOVE 'DB2'                TO ERR-TIPO-ACESSO
               MOVE 'TCONTR_ENVIO_RECOR' TO ERR-DBD-TAB
               MOVE 'SELECT'             TO ERR-FUN-COMANDO
               MOVE SQLCODE              TO ERR-SQL-CODE
               MOVE '3000'               TO ERR-LOCAL
               MOVE SPACES               TO ERR-SEGM
               PERFORM 9000-ROTINA-FIM
           END-IF.

      *---------------------------------------------------------------*
       3000-99-FIM.                EXIT.
      *---------------------------------------------------------------*

      *----------------------------------------------------------------*
      * VER NULIDADE DOS CAMPOS                                        *
      *----------------------------------------------------------------*
       3001-VER-NULOS-B099            SECTION.
      *----------------------------------------------------------------*

           IF WRK-DCDULA-NULL       LESS ZEROS
              MOVE SPACES TO DCDULA              OF RUECB099
           END-IF.

           IF WRK-DVCTO-CONTR-NULL  LESS ZEROS
              MOVE SPACES TO  DVCTO-CONTR        OF RUECB099
           END-IF.

           IF WRK-VFINAN LESS ZEROS
              MOVE ZEROS  TO VFINAN              OF RUECB099
           END-IF.

           IF WRK-CCATEG-PROTR-NULL LESS ZEROS
              MOVE SPACES TO CCATEG-PROTR-RECOR  OF RUECB099
           END-IF.

           IF WRK-CPROG-RURAL-NULL   LESS ZEROS
              MOVE SPACES TO CPROG-RURAL         OF RUECB099
           END-IF.

           IF WRK-CMUN-REFT-NULL LESS ZEROS
              MOVE ZEROS  TO CMUN-REFT-BACEN     OF RUECB099
           END-IF.

           IF WRK-VPCELA-LIBRC-NULL LESS ZEROS
              MOVE ZEROS  TO VPCELA-LIBRC        OF RUECB099
           END-IF.

           IF WRK-VPCELA-REC-NULL LESS ZEROS
              MOVE ZEROS  TO VPCELA-REC-PPRIO    OF RUECB099
           END-IF.

           IF WRK-QUND-FINCD-NULL LESS ZEROS
              MOVE ZEROS  TO QUND-FINCD-RURAL    OF RUECB099
           END-IF.

           IF WRK-QUND-PROD-NULL  LESS ZEROS
              MOVE ZEROS  TO QUND-PROD-PROVV     OF RUECB099
           END-IF.

           IF WRK-DINIC-SAFRA-NULL LESS ZEROS
              MOVE ZEROS  TO DINIC-SAFRA         OF RUECB099
           END-IF.

           IF WRK-DFIM-SAFRA-NULL LESS ZEROS
              MOVE ZEROS  TO DFIM-SAFRA          OF RUECB099
           END-IF.

           IF WRK-CTPO-GARNT-NULL LESS ZEROS
              MOVE ZEROS  TO CTPO-GARNT-RECOR    OF RUECB099
           END-IF.

      *----------------------------------------------------------------*
       3001-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       3002-OBTEM-DADOS-B09A       SECTION.
      *---------------------------------------------------------------*

           IF (SQLCODE       EQUAL  -305)
               MOVE 03                   TO RUEC75-COD-RET
               MOVE 'NAO EXISTE CONTRATO/NENVIO PARA SER REENVIADO'
                                         TO RUEC75-MENSAGEM
               MOVE SPACES               TO RUEC75-SQLCA
               MOVE SPACES               TO RUEC75-ERRO-AREA
               PERFORM 9000-ROTINA-FIM
           END-IF.

           MOVE RUEC75-CCONTR         TO CCONTR-CREDT-RURAL
                                         OF RUECB09A.
           MOVE RUEC75-NENVIO-RECOR   TO NENVIO-RECOR
                                         OF RUECB09A.

           MOVE NREG-MSGEM-RECOR      TO WRK-MAX-NREG.

           EXEC SQL
           SELECT CCONTR_CREDT_RURAL,
                  NENVIO_RECOR,
                  NREG_MSGEM_RECOR,
                  CSIT_MSGEM,
                  HINCL,
                  WCONTD_MSGEM_RURAL
           INTO   :RUECB09A.CCONTR-CREDT-RURAL,
                  :RUECB09A.NENVIO-RECOR,
                  :RUECB09A.NREG-MSGEM-RECOR,
                  :RUECB09A.CSIT-MSGEM
                  :WRK-CSIT-NULL,
                  :RUECB09A.HINCL
                  :WRK-HINCL-NULL,
                  :RUECB09A.WCONTD-MSGEM-RURAL
           FROM  DB2PRD.TENVIO_RECOR_TEMPR
           WHERE CCONTR_CREDT_RURAL   = :RUECB09A.CCONTR-CREDT-RURAL
           AND   NENVIO_RECOR         = :RUECB09A.NENVIO-RECOR
           AND   NREG_MSGEM_RECOR     = :RUECB09A.NREG-MSGEM-RECOR
           END-EXEC.

           IF (SQLCODE   NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0      EQUAL 'W')
               MOVE 99                   TO RUEC75-COD-RET
               MOVE 'ERRO ACESSO DB2'    TO RUEC75-MENSAGEM
               MOVE 'DB2'                TO ERR-TIPO-ACESSO
               MOVE 'TENVIO_RECOR_TEMPR' TO ERR-DBD-TAB
               MOVE 'SELECT'             TO ERR-FUN-COMANDO
               MOVE SQLCODE              TO ERR-SQL-CODE
               MOVE '3002'               TO ERR-LOCAL
               MOVE SPACES               TO ERR-SEGM
               PERFORM 9000-ROTINA-FIM
           END-IF.

           IF (SQLCODE       EQUAL  +100)
               MOVE 03                   TO RUEC75-COD-RET
               MOVE 'CONTRATO/NENVIO-RCOR NAO ENCONTRATO B09A'
                                         TO RUEC75-MENSAGEM
               MOVE SPACES               TO RUEC75-SQLCA
               MOVE SPACES               TO RUEC75-ERRO-AREA
               PERFORM 9000-ROTINA-FIM
           END-IF.

           IF  SQLCODE   EQUAL ZEROS
               PERFORM 3003-VER-NULOS-B09A
           END-IF.

      *---------------------------------------------------------------*
       3002-99-FIM.                EXIT.
      *---------------------------------------------------------------*


      *----------------------------------------------------------------*
      * VER NULIDADE DOS CAMPOS                                        *
      *----------------------------------------------------------------*
       3003-VER-NULOS-B09A            SECTION.
      *----------------------------------------------------------------*

           IF WRK-CSIT-NULL         LESS ZEROS
              MOVE ZEROS  TO CSIT-MSGEM          OF RUECB09A
           END-IF.

           IF WRK-HINCL-NULL        LESS ZEROS
              MOVE SPACES TO  HINCL              OF RUECB09A
           END-IF.

      *----------------------------------------------------------------*
       3003-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       3004-OBTEM-DADOS-B09B       SECTION.
      *---------------------------------------------------------------*

           PERFORM 4310-SELMAX-B09B.

           MOVE RUEC75-CCONTR         TO CCONTR-CREDT-RURAL
                                         OF RUECB09B.

           MOVE RUEC75-NENVIO-RECOR   TO NENVIO-RECOR
                                         OF RUECB09B.

           EXEC SQL
           SELECT CCONTR_CREDT_RURAL,
                  NENVIO_RECOR,
                  HULT_ATULZ,
                  CMSGEM_RECOR,
                  HINCL,
                  CCTRL_RQUIS_FINCR,
                  CUNIC_MSGEM,
                  CREFT_RECOR,
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
           AND   NENVIO_RECOR        = :RUECB09B.NENVIO-RECOR
           AND   HULT_ATULZ          = :RUECB09B.HULT-ATULZ
           END-EXEC.

           IF (SQLCODE   NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0      EQUAL 'W')
               MOVE 99                   TO RUEC75-COD-RET
               MOVE 'ERRO ACESSO DB2'    TO RUEC75-MENSAGEM
               MOVE 'DB2'                TO ERR-TIPO-ACESSO
               MOVE 'THIST_ENVIO_RECOR'  TO ERR-DBD-TAB
               MOVE 'SELECT'             TO ERR-FUN-COMANDO
               MOVE SQLCODE              TO ERR-SQL-CODE
               MOVE '3004'               TO ERR-LOCAL
               MOVE SPACES               TO ERR-SEGM
               PERFORM 9000-ROTINA-FIM
           END-IF.

           IF (SQLCODE       EQUAL  +100)
               MOVE 03                   TO RUEC75-COD-RET
               MOVE 'CONTRATO NAO ENCONTRATO B002'
                                         TO RUEC75-MENSAGEM
               MOVE SPACES               TO RUEC75-SQLCA
               MOVE SPACES               TO RUEC75-ERRO-AREA
               PERFORM 9000-ROTINA-FIM
           END-IF.

           IF  SQLCODE   EQUAL ZEROS
               PERFORM 3005-VER-NULOS-B09B
           END-IF.

      *---------------------------------------------------------------*
       3004-99-FIM.                EXIT.
      *---------------------------------------------------------------*

      *----------------------------------------------------------------*
      * VER NULIDADE DOS CAMPOS                                        *
      *----------------------------------------------------------------*
       3005-VER-NULOS-B09B            SECTION.
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
       3005-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       3100-FORMATA-SIS1           SECTION.
      *---------------------------------------------------------------*

           MOVE 'COR0002'          TO RUEC76-CODMSG.

           MOVE SPACES             TO RUEC76-NUMCTRLIF.

      * FORMATAR CNPJ
           MOVE CCNPJ-ENTID-RESP   OF RUECB099  TO WRK-COD9-NUM.
           MOVE CFLIAL-CNPJ-ENTID  OF RUECB099  TO WRK-COD5-NUM.
           MOVE CCTRL-CNPJ-ENTID   OF RUECB099  TO WRK-COD3-NUM.
           MOVE WRK-COD9-NUM8      TO WRK-EDIT-NCNPJ.
           MOVE WRK-COD5-NUM4      TO WRK-EDIT-FCNPJ.
           MOVE WRK-COD3-NUM2      TO WRK-EDIT-CCNPJ.
           MOVE WRK-EDIT-CNPJ-R    TO RUEC76-CNPJ-ENT-RESPONS.

           MOVE CREFT-RECOR  OF RUECB09B
                                   TO RUEC76-NUMREF-BCCOROR.

           MOVE CSIT-OPER-RURAL  OF RUECB099
                                   TO RUEC76-MOTVO-EXCLUCOR.

      * FORMATAR DATA AAAAMMDD
           MOVE DMOVTO-RECOR       TO  WRK-DATA.
           MOVE WRK-DD             TO  WRK-EDIT-DD.
           MOVE WRK-MM             TO  WRK-EDIT-MM.
           MOVE WRK-AAAA           TO  WRK-EDIT-AAAA.
           MOVE WRK-EDIT-DATA-R    TO  RUEC76-DTMOVTO.

           MOVE RUEC76-COR0002     TO WRK-B09A-SIS.
           ADD  LENGTH OF RUEC76-COR0002  TO  WRK-TAM.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      *---------------------------------------------------------------*
       4000-INSERT-B09A          SECTION.
      *---------------------------------------------------------------*

           PERFORM 4010-SELMAX-B09A.

           IF SQLCODE  EQUAL  -305
              MOVE ZEROS              TO  WRK-MAX-NREG
              ADD  1                  TO  WRK-MAX-NREG
           ELSE
              IF SQLCODE  EQUAL  ZEROS
                 MOVE NREG-MSGEM-RECOR   TO  WRK-MAX-NREG
                 ADD  1                  TO  WRK-MAX-NREG
              END-IF
           END-IF.

           MOVE WRK-MAX-NREG     TO NREG-MSGEM-RECOR
                                    OF RUECB09A.

           MOVE 1                TO CSIT-MSGEM   OF RUECB09A.

           EXEC SQL
           SET :WRK-TIMESTAMP = CURRENT TIMESTAMP
           END-EXEC.

           MOVE WRK-TIMESTAMP         TO HINCL  OF RUECB09A.

           MOVE WRK-TAM               TO WRK-TAMANHO-SISMSG.

           MOVE WRK-TAMANHO-SISMSG    TO WCONTD-MSGEM-RURAL-LEN
                                         OF RUECB09A.

           MOVE WRK-B09A-SIS          TO WCONTD-MSGEM-RURAL-TEXT
                                         OF RUECB09A.

           EXEC SQL
           INSERT INTO DB2PRD.TENVIO_RECOR_TEMPR
                  (CCONTR_CREDT_RURAL,
                   NENVIO_RECOR,
                   NREG_MSGEM_RECOR,
                   CSIT_MSGEM,
                   HINCL,
                   WCONTD_MSGEM_RURAL)
           VALUES
                  (:RUECB09A.CCONTR-CREDT-RURAL,
                   :RUECB09A.NENVIO-RECOR,
                   :RUECB09A.NREG-MSGEM-RECOR,
                   :RUECB09A.CSIT-MSGEM,
                   :RUECB09A.HINCL,
                   :RUECB09A.WCONTD-MSGEM-RURAL)
           END-EXEC.


           IF (SQLCODE   NOT EQUAL ZEROS) OR
              (SQLWARN0      EQUAL 'W')
               MOVE 99                   TO RUEC75-COD-RET
               MOVE 'ERRO ACESSO DB2'    TO RUEC75-MENSAGEM
               MOVE 'DB2'                TO ERR-TIPO-ACESSO
               MOVE 'TENVIO_RECOR_TEMPR' TO ERR-DBD-TAB
               MOVE 'INSERT'             TO ERR-FUN-COMANDO
               MOVE SQLCODE              TO ERR-SQL-CODE
               MOVE '4000'               TO ERR-LOCAL
               MOVE SPACES               TO ERR-SEGM
               PERFORM 9000-ROTINA-FIM
           END-IF.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4010-SELMAX-B09A               SECTION.
      *----------------------------------------------------------------*

           MOVE RUEC75-CCONTR         TO CCONTR-CREDT-RURAL
                                         OF RUECB09A.
           MOVE RUEC75-NENVIO-RECOR   TO NENVIO-RECOR
                                         OF RUECB09A.

           EXEC SQL
           SELECT MAX(NREG_MSGEM_RECOR)
           INTO :RUECB09A.NREG-MSGEM-RECOR
           FROM DB2PRD.TENVIO_RECOR_TEMPR
           WHERE  CCONTR_CREDT_RURAL = :RUECB09A.CCONTR-CREDT-RURAL
           AND    NENVIO_RECOR       = :RUECB09A.NENVIO-RECOR
           AND    NREG_MSGEM_RECOR > 0
           END-EXEC.

           IF (SQLCODE   NOT EQUAL ZEROS  AND
               SQLCODE   NOT EQUAL -305)  OR
              (SQLWARN0      EQUAL 'W')
               MOVE 99                   TO RUEC75-COD-RET
               MOVE 'ERRO ACESSO DB2'    TO RUEC75-MENSAGEM
               MOVE 'DB2'                TO ERR-TIPO-ACESSO
               MOVE 'TENVIO_RECOR_TEMPR' TO ERR-DBD-TAB
               MOVE 'SELECT'             TO ERR-FUN-COMANDO
               MOVE SQLCODE              TO ERR-SQL-CODE
               MOVE '4010'               TO ERR-LOCAL
               MOVE SPACES               TO ERR-SEGM
               PERFORM 9000-ROTINA-FIM
           END-IF.


      *----------------------------------------------------------------*
       4010-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       4100-ENVIO-RCOR5010         SECTION.
      *---------------------------------------------------------------*

           INITIALIZE  WRK-RCOR10-REGISTRO.

           MOVE 'RUEC'                  TO WRK-RCOR10-CCUSTO.
           MOVE 1                       TO WRK-RCOR10-SEQ-MSG.
           MOVE 'N'                     TO WRK-RCOR10-IND-CONTINUA.

           MOVE WCONTD-MSGEM-RURAL-LEN  TO WRK-RCOR10-TAM-SISMSG.
           MOVE WCONTD-MSGEM-RURAL-TEXT(1: LENGTH OF WRK-RCOR10-SISMSG)
                                        TO WRK-RCOR10-SISMSG.
           MOVE WCONTD-MSGEM-RURAL-LEN  TO WRK-ENVIO-LEN.
           MOVE WCONTD-MSGEM-RURAL-TEXT(1: LENGTH OF WRK-ENVIO-SIS)
                                        TO WRK-ENVIO-SIS.

           CALL WRK-MODULO-RCOR5010  USING  WRK-RCOR10-REGISTRO
                                            ERRO-AREA
                                            WRK-LNK-SQLCA.

           IF WRK-RCOR10-COD-RETORNO EQUAL ZEROS
              MOVE WRK-RCOR10-NRO-CTRL-IF TO WRK-ENVIO-NUMCTRLIF
              PERFORM 4200-UPDATE-B09A
              PERFORM 4300-UPDATE-B09B
           ELSE
              MOVE 99                   TO RUEC75-COD-RET
              MOVE 'ERRO RCOR5010'      TO RUEC75-MENSAGEM
              MOVE ERRO-AREA            TO RUEC75-ERRO-AREA
              MOVE WRK-LNK-SQLCA        TO RUEC75-SQLCA
              PERFORM 9000-ROTINA-FIM
           END-IF.

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       4200-UPDATE-B09A            SECTION.
      *---------------------------------------------------------------*

           MOVE RUEC75-CCONTR         TO CCONTR-CREDT-RURAL
                                         OF RUECB09A.

           MOVE RUEC75-NENVIO-RECOR   TO NENVIO-RECOR
                                         OF RUECB09A.

           MOVE WRK-MAX-NREG          TO NREG-MSGEM-RECOR
                                         OF RUECB09A.

           EXEC SQL
           SET :WRK-TIMESTAMP = CURRENT TIMESTAMP
           END-EXEC.

           MOVE WRK-TIMESTAMP         TO HINCL  OF RUECB09A.

           MOVE WRK-ENVIO-LEN         TO WCONTD-MSGEM-RURAL-LEN
                                         OF RUECB09A.

           MOVE WRK-ENVIO-SIS         TO WCONTD-MSGEM-RURAL-TEXT
                                         OF RUECB09A.

           MOVE 2                     TO CSIT-MSGEM
                                         OF RUECB09A.


           EXEC SQL
           UPDATE DB2PRD.TENVIO_RECOR_TEMPR
           SET CSIT_MSGEM         = :RUECB09A.CSIT-MSGEM,
               HINCL              = :RUECB09A.HINCL,
               WCONTD_MSGEM_RURAL = :RUECB09A.WCONTD-MSGEM-RURAL
           WHERE CCONTR_CREDT_RURAL = :RUECB09A.CCONTR-CREDT-RURAL
           AND   NENVIO_RECOR       = :RUECB09A.NENVIO-RECOR
           AND   NREG_MSGEM_RECOR   = :RUECB09A.NREG-MSGEM-RECOR
           END-EXEC.

           IF (SQLCODE   NOT EQUAL ZEROS) OR
              (SQLWARN0      EQUAL 'W')
               MOVE 99                   TO RUEC75-COD-RET
               MOVE 'ERRO ACESSO DB2'    TO RUEC75-MENSAGEM
               MOVE 'DB2'                TO ERR-TIPO-ACESSO
               MOVE 'TENVIO_RECOR_TEMPR' TO ERR-DBD-TAB
               MOVE 'UPDATE'             TO ERR-FUN-COMANDO
               MOVE SQLCODE              TO ERR-SQL-CODE
               MOVE '4200'               TO ERR-LOCAL
               MOVE SPACES               TO ERR-SEGM
               PERFORM 9000-ROTINA-FIM
           END-IF.

      *----------------------------------------------------------------*
       4200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       4300-UPDATE-B09B            SECTION.
      *---------------------------------------------------------------*

           PERFORM 4310-SELMAX-B09B.

           MOVE RUEC75-CCONTR           TO CCONTR-CREDT-RURAL
                                           OF RUECB09B.

           MOVE RUEC75-NENVIO-RECOR     TO NENVIO-RECOR
                                           OF RUECB09B.

           MOVE WRK-RCOR10-NRO-CTRL-IF  TO CCTRL-RQUIS-FINCR
                                           OF RUECB09B.

           MOVE 2                     TO CSIT-MSGEM OF RUECB09B.

           EXEC SQL
           SET :WRK-TIMESTAMP = CURRENT TIMESTAMP
           END-EXEC.

           MOVE WRK-TIMESTAMP         TO HINCL  OF RUECB09B.

           MOVE '999999999'           TO CUSUAR-RESP OF RUECB09B.


           EXEC SQL
           UPDATE DB2PRD.THIST_ENVIO_RECOR
           SET CCTRL_RQUIS_FINCR  = :RUECB09B.CCTRL-RQUIS-FINCR,
               CSIT_MSGEM         = :RUECB09B.CSIT-MSGEM,
               HINCL              = :RUECB09B.HINCL,
               CUSUAR_RESP        = :RUECB09B.CUSUAR-RESP
           WHERE CCONTR_CREDT_RURAL = :RUECB09B.CCONTR-CREDT-RURAL
           AND   NENVIO_RECOR       = :RUECB09B.NENVIO-RECOR
           AND   HULT_ATULZ         = :RUECB09B.HULT-ATULZ
           END-EXEC.

           IF (SQLCODE   NOT EQUAL ZEROS) OR
              (SQLWARN0      EQUAL 'W')
               MOVE 99                   TO RUEC75-COD-RET
               MOVE 'ERRO ACESSO DB2'    TO RUEC75-MENSAGEM
               MOVE 'DB2'                TO ERR-TIPO-ACESSO
               MOVE 'THIST_ENVIO_RECOR'  TO ERR-DBD-TAB
               MOVE 'UPDATE'             TO ERR-FUN-COMANDO
               MOVE SQLCODE              TO ERR-SQL-CODE
               MOVE '4210'               TO ERR-LOCAL
               MOVE SPACES               TO ERR-SEGM
               PERFORM 9000-ROTINA-FIM
           END-IF.

      *----------------------------------------------------------------*
       4300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4310-SELMAX-B09B               SECTION.
      *----------------------------------------------------------------*

           MOVE RUEC75-CCONTR         TO CCONTR-CREDT-RURAL
                                         OF RUECB09B.
           MOVE RUEC75-NENVIO-RECOR   TO NENVIO-RECOR
                                         OF RUECB09B.

           EXEC SQL
           SELECT MAX(HULT_ATULZ)
           INTO :RUECB09B.HULT-ATULZ
           FROM DB2PRD.THIST_ENVIO_RECOR
           WHERE  CCONTR_CREDT_RURAL = :RUECB09B.CCONTR-CREDT-RURAL
           AND    NENVIO_RECOR       = :RUECB09B.NENVIO-RECOR
           END-EXEC.

           IF (SQLCODE   NOT EQUAL ZEROS  AND
               SQLCODE   NOT EQUAL -305)  OR
              (SQLWARN0      EQUAL 'W')
               MOVE 99                   TO RUEC75-COD-RET
               MOVE 'ERRO ACESSO DB2'    TO RUEC75-MENSAGEM
               MOVE 'DB2'                TO ERR-TIPO-ACESSO
               MOVE 'THIST_ENVIO_RECOR'  TO ERR-DBD-TAB
               MOVE 'SELECT'             TO ERR-FUN-COMANDO
               MOVE SQLCODE              TO ERR-SQL-CODE
               MOVE '4310'               TO ERR-LOCAL
               MOVE SPACES               TO ERR-SEGM
               PERFORM 9000-ROTINA-FIM
           END-IF.

           IF SQLCODE  EQUAL  -305
              MOVE 99                   TO RUEC75-COD-RET
              MOVE 'ERRO ACESSO DB2'    TO RUEC75-MENSAGEM
              MOVE 'DB2'                TO ERR-TIPO-ACESSO
              MOVE 'THIST_ENVIO_RECOR'  TO ERR-DBD-TAB
              MOVE 'SELECT'             TO ERR-FUN-COMANDO
              MOVE SQLCODE              TO ERR-SQL-CODE
              MOVE '431A'               TO ERR-LOCAL
              MOVE SPACES               TO ERR-SEGM
              PERFORM 9000-ROTINA-FIM
           END-IF.


      *----------------------------------------------------------------*
       4310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       9000-ROTINA-FIM             SECTION.
      *---------------------------------------------------------------*

           IF  RUEC75-COD-RET      EQUAL 99
               MOVE 'RUEC8818'     TO ERR-MODULO
               MOVE ERRO-AREA      TO RUEC75-ERRO-AREA
               MOVE SQLCA(1:136)   TO RUEC75-SQLCA
           END-IF.

           GOBACK.

      *---------------------------------------------------------------*
       9000-99-FIM.                EXIT.
      *---------------------------------------------------------------**
