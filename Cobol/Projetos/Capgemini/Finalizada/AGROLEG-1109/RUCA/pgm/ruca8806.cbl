      ******************************************************************
       IDENTIFICATION                  DIVISION.
      ******************************************************************
       PROGRAM-ID.    RUCA8806.
       AUTHOR.        WAGNER NUNES.

      *================================================================*
      *                   S I S   C O N S U L T O R I A                *
      *----------------------------------------------------------------*
      *                                                                *
      *      PROGRAMADOR  : WAGNER N.           - SIS                  *
      *      ANALISTA     : WAGNER N.           - SIS                  *
      *      DATA         : 19/10/2020                                 *
      *                                                                *
      *      OBJETIVO     :                                            *
      *        TRATA RETORNO DA MENSAGEM BACEN.                        *
      *                                                                *
      *----------------------------------------------------------------*
      *  MODULOS CHAMADOS:                                             *
      *                                                                *
      *     RUCA8807 - EFETIVA AS ALTERACOES ENVIADAS PARA O BACEN     *
      *     RUCA8812 - ENVIAR PARA O BACEN COR0001 E COR0005           *
      *     RCOR5005 - ENVIA CONTRATO PARA O BACEN                     *
      *     BRAD7600 - DATA/HORA                                       *
      *                                                                *
      *----------------------------------------------------------------*
      *  BASES BD2:                                                    *
      *                                                                *
      *     RUCAB012 - DB2PRD.TCTRL_ENVIO_MANUT                        *
      *     RUCAB013 - DB2PRD.TENVIO_RETOR_MANUT                       *
      *     RUCAB015 - DB2PRD.TMANUT_CONTR_RECOR                       *
      *     RUCAB019 - DB2PRD.TSIT_MANUT_RECOR                         *
      *                                                                *
      *================================================================*
IN0923*                     ULTIMA ALTERACAO                           *
IN0923*----------------------------------------------------------------*
IN0923*                                                                *
IN0923*      ANALISTA     : M. KAZUO            - OTI                  *
IN0923*      DATA         : 09/2023                                    *
IN0923*                                                                *
IN0923*      OBJETIVO     :                                            *
IN0923*        ATENDIMENTO IN8966990 - IDENTIFICAR INCONSISTENCIA      *
IN0923*                                                                *
IN0923*================================================================*
7COMMP*                   A L T E R A C A O                            *
.     *----------------------------------------------------------------*
.     *    ANALISTA....:  PRISCILA PEREIRA                             *
.     *    DATA........:  SETEMBRO/2023                                *
.     *----------------------------------------------------------------*
.     *    OBJETIVO....:  SICOR 5.07                                   *
.     *    PESQUISA....:  7COMMP                                       *
7COMMP*================================================================*
7C5.10*================================================================*
.     * ANALISTA REPONSAVEL: EDSON/NIKSON                              *
.     * DATA...............: AGOSTO/2025                               *
.     * OBJETIVOS..........: FLASH - AGRO7-1628 - ALTERAÇÃO DA COR0001 *
.     *                      - SICOR 5.10                              *
7C5.10*================================================================*

      ******************************************************************
       ENVIRONMENT                     DIVISION.
      ******************************************************************

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      ******************************************************************
       DATA                            DIVISION.
      ******************************************************************

      ******************************************************************
       WORKING-STORAGE                 SECTION.
      ******************************************************************

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** INICIO DA WORKING-STORAGE ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** AREA PARA DRESSCODE ***'.
      *----------------------------------------------------------------*

       01  WRK-S09-DRESS               PIC +9(09)          VALUE ZEROS.
       01  FILLER                      REDEFINES  WRK-S09-DRESS.
           05  FILLER                  PIC  X(01).
           05  WRK-N09-DRESS           PIC  9(09).
       01  FILLER                      REDEFINES  WRK-S09-DRESS.
           05  FILLER                  PIC  X(05).
           05  WRK-N05-DRESS           PIC  9(05).

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** AREAS AUXILIARES ***'.
      *----------------------------------------------------------------*

       01  WRK-MODULO                  PIC  X(08)          VALUE SPACES.

IN0923 01  WRK-ERR-TEXTO.
IN0923     05  FILLER                  PIC  X(20)          VALUE
IN0923         'ERRO COD. RETORNO = '.
IN0923     05  WRK-ERR-TEXTO-COD       PIC  9(05)          VALUE ZEROS.
IN0923
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** AREAS AUXILIARES PARA DATAS ***'.
      *----------------------------------------------------------------*

       01  WRK-TIMESTAMP.
           05  WRK-ANO-TIMES           PIC  9(04)          VALUE ZEROS.
           05  WRK-TR1-TIMES           PIC  X(01)          VALUE '-'.
           05  WRK-MES-TIMES           PIC  9(02)          VALUE ZEROS.
           05  WRK-TR2-TIMES           PIC  X(01)          VALUE '-'.
           05  WRK-DIA-TIMES           PIC  9(02)          VALUE ZEROS.
           05  WRK-TR3-TIMES           PIC  X(01)          VALUE '-'.
           05  WRK-HOR-TIMES           PIC  9(02)          VALUE ZEROS.
           05  WRK-PT1-TIMES           PIC  X(01)          VALUE '.'.
           05  WRK-MIN-TIMES           PIC  9(02)          VALUE ZEROS.
           05  WRK-PT2-TIMES           PIC  X(01)          VALUE '.'.
           05  WRK-SEG-TIMES           PIC  9(02)          VALUE ZEROS.
           05  WRK-PT3-TIMES           PIC  X(01)          VALUE '.'.
           05  WRK-MIL-TIMES           PIC  9(06)          VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREAS DA BRAD7600 ***'.
      *----------------------------------------------------------------*

       01  WRK-7600-DATA-HORA.
           05  WRK-7600-DT-JULIANA     PIC  9(05) COMP-3   VALUE ZEROS.
           05  WRK-7600-DT-AAMMDD      PIC  9(07) COMP-3   VALUE ZEROS.
           05  WRK-7600-DT-AAAAMMDD    PIC  9(09) COMP-3   VALUE ZEROS.
           05  WRK-7600-TI-HHMMSS      PIC  9(07) COMP-3   VALUE ZEROS.
           05  WRK-7600-TI-HHMMSSMMMMMM
                                       PIC  9(13) COMP-3   VALUE ZEROS.
           05  WRK-7600-TIMESTAMP      PIC  X(20)          VALUE SPACES.
           05  FILLER                  REDEFINES  WRK-7600-TIMESTAMP.
               07  WRK-7600-ANO-TIMES  PIC  9(04).
               07  WRK-7600-MES-TIMES  PIC  9(02).
               07  WRK-7600-DIA-TIMES  PIC  9(02).
               07  WRK-7600-HOR-TIMES  PIC  9(02).
               07  WRK-7600-MIN-TIMES  PIC  9(02).
               07  WRK-7600-SEG-TIMES  PIC  9(02).
               07  WRK-7600-MIL-TIMES  PIC  9(06).

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(050)          VALUE
           '*** AREA DA MENSAGEM TMNF ***'.
      *---------------------------------------------------------------*

       COPY 'I#RCOR03'.

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(050)          VALUE
           '*** AREA DA MENSAGEM GEN0004 ***'.
      *---------------------------------------------------------------*

       COPY 'I#RCOR05'.

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(050)          VALUE
           '*** AREA DE MENSAGEM COR0001R1 ***'.
      *---------------------------------------------------------------*

       COPY 'I#RUEC89'.

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(050)          VALUE
           '*** AREAS DO MODULO RUCA8807 ***'.
      *---------------------------------------------------------------*

       01  WRK-AREA-RUCA8807.
           05  WRK-8807-ENTRADA.
               07  WRK-8807-CHAVE      PIC  9(09)          VALUE ZEROS.
               07  WRK-8807-CTRLNAV.
                   10  WRK-8807-ANO-CTRLNAV
                                       PIC  9(04)          VALUE ZEROS.
                   10  WRK-8807-MES-CTRLNAV
                                       PIC  9(02)          VALUE ZEROS.
                   10  WRK-8807-DIA-CTRLNAV
                                       PIC  9(02)          VALUE ZEROS.
                   10  WRK-8807-HOR-CTRLNAV
                                       PIC  9(02)          VALUE ZEROS.
                   10  WRK-8807-MIN-CTRLNAV
                                       PIC  9(02)          VALUE ZEROS.
                   10  WRK-8807-SEG-CTRLNAV
                                       PIC  9(02)          VALUE ZEROS.
                   10  WRK-8807-MIL-CTRLNAV
                                       PIC  9(06)          VALUE ZEROS.
                   10  WRK-8807-NENVIO PIC  9(05)          VALUE ZEROS.
7C5.10             10  WRK-8807-CODMSG PIC  X(09)          VALUE SPACES.
           05  WRK-8812-SAIDA.
               07  WRK-8807-COD-RETORNO
                                       PIC  9(02)          VALUE ZEROS.
               07  WRK-8807-MENSAGEM   PIC  X(75)          VALUE SPACES.
               07  WRK-8807-ERRO-AREA  PIC  X(107)         VALUE SPACES.
               07  WRK-8807-SQLCA      PIC  X(136)         VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(050)          VALUE
           '*** AREAS DO MODULO RUCA8812 ***'.
      *---------------------------------------------------------------*

       01  WRK-AREA-RUCA8812.
           05  WRK-8812-ENTRADA.
               07  WRK-8812-CHAVE      PIC  9(09)          VALUE ZEROS.
               07  WRK-8812-CTRLNAV.
                   10  WRK-8812-ANO-CTRLNAV
                                       PIC  9(04)          VALUE ZEROS.
                   10  WRK-8812-MES-CTRLNAV
                                       PIC  9(02)          VALUE ZEROS.
                   10  WRK-8812-DIA-CTRLNAV
                                       PIC  9(02)          VALUE ZEROS.
                   10  WRK-8812-HOR-CTRLNAV
                                       PIC  9(02)          VALUE ZEROS.
                   10  WRK-8812-MIN-CTRLNAV
                                       PIC  9(02)          VALUE ZEROS.
                   10  WRK-8812-SEG-CTRLNAV
                                       PIC  9(02)          VALUE ZEROS.
                   10  WRK-8812-MIL-CTRLNAV
                                       PIC  9(06)          VALUE ZEROS.
           05  WRK-8812-SAIDA.
               07  WRK-8812-COD-RETORNO
                                       PIC  9(02)          VALUE ZEROS.
               07  WRK-8812-MENSAGEM   PIC  X(75)          VALUE SPACES.
               07  WRK-8812-ERRO-AREA  PIC  X(107)         VALUE SPACES.
               07  WRK-8812-SQLCA      PIC  X(136)         VALUE SPACES.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** AREAS PARA TRATAR NULO ***'.
      *----------------------------------------------------------------*

       01  WRK-B012-CCTRL-RQUIS-FINCR  PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B012-CUNIC-MSGEM        PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B012-CREFT-RECOR        PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B019-CMSGEM-RECOR       PIC S9(04)  COMP    VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** TABELAS DB2 ***'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE RUCAB012
           END-EXEC.

           EXEC SQL
               INCLUDE RUCAB013
           END-EXEC.

           EXEC SQL
               INCLUDE RUCAB015
           END-EXEC.

           EXEC SQL
               INCLUDE RUCAB019
           END-EXEC.

           EXEC SQL
              DECLARE  CSR01-RUCAB012 CURSOR WITH HOLD FOR
               SELECT  CCONTR             ,
                       HINCL_REG          ,
                       NENVIO_RECOR       ,
                       CCTRL_RQUIS_FINCR  ,
                       CUNIC_MSGEM        ,
                       CREFT_RECOR
                 FROM  DB2PRD.TCTRL_ENVIO_MANUT
                WHERE  CCTRL_RQUIS_FINCR  = :RUCAB012.CCTRL-RQUIS-FINCR
                ORDER  BY  NENVIO_RECOR     ASC
           END-EXEC.

           EXEC SQL
              DECLARE  CSR02-RUCAB012 CURSOR WITH HOLD FOR
               SELECT  CCONTR             ,
                       HINCL_REG          ,
                       NENVIO_RECOR       ,
                       CCTRL_RQUIS_FINCR  ,
                       CUNIC_MSGEM        ,
                       CREFT_RECOR
                 FROM  DB2PRD.TCTRL_ENVIO_MANUT
                WHERE  CUNIC_MSGEM  = :RUCAB012.CUNIC-MSGEM
                ORDER  BY  NENVIO_RECOR     ASC
           END-EXEC.

           EXEC SQL
              DECLARE  CSR03-RUCAB019 CURSOR WITH HOLD FOR
               SELECT  CCONTR             ,
                       HINCL_REG          ,
                       NENVIO_RECOR       ,
                       NREG_MSGEM_RECOR   ,
                       CSIT_MSGEM
                 FROM  DB2PRD.TSIT_MANUT_RECOR
                WHERE  CCONTR             = :RUCAB019.CCONTR
                  AND  HINCL_REG          = :RUCAB019.HINCL-REG
                  AND  NENVIO_RECOR       = :RUCAB019.NENVIO-RECOR
                  AND  NREG_MSGEM_RECOR  >= :RUCAB019.NREG-MSGEM-RECOR
                ORDER  BY  NENVIO_RECOR     ASC,
                           NREG_MSGEM_RECOR ASC
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** FIM DA WORKING-STORAGE ***'.
      *----------------------------------------------------------------*

      ******************************************************************
       LINKAGE                         SECTION.
      ******************************************************************

       COPY 'I#RUEC60'.

       COPY I#BRAD7C.

       01  LNK-SQLCA                   PIC X(136).

      ******************************************************************
       PROCEDURE                      DIVISION USING RUEC60-REGISTRO
                                                     ERRO-AREA
                                                     LNK-SQLCA.
      ******************************************************************

      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           PERFORM  1000-CONSISTIR-ENTRADA
           PERFORM  5000-PROCESSAR-DADOS
           PERFORM  8000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-CONSISTIR-ENTRADA          SECTION.
      *----------------------------------------------------------------*

           IF  RUEC60-COD-MSG       EQUAL  SPACES
               MOVE  01                TO  RUEC60-COD-RETORNO
IN0923                                     WRK-ERR-TEXTO-COD
IN0923         MOVE 'APL'              TO  ERR-TIPO-ACESSO
IN0923         MOVE 'RUCA8806'         TO  ERR-PGM
IN0923         MOVE WRK-ERR-TEXTO      TO  ERR-TEXTO
               PERFORM  8000-FINALIZAR
           END-IF.

           IF  RUEC60-SISMSG        EQUAL  SPACES
               MOVE  02                TO  RUEC60-COD-RETORNO
IN0923                                     WRK-ERR-TEXTO-COD
IN0923         MOVE 'APL'              TO  ERR-TIPO-ACESSO
IN0923         MOVE 'RUCA8806'         TO  ERR-PGM
IN0923         MOVE WRK-ERR-TEXTO      TO  ERR-TEXTO
               PERFORM  8000-FINALIZAR
           END-IF.

           IF  RUEC60-COD-MSG  EQUAL  'COR0001R1'  OR  'COR0001E ' OR
                                      'COR0005R1'  OR  'COR0005E ' OR
                                      'GEN0004  '  OR  'TMNF     '
               MOVE  ZEROS             TO  RUEC60-COD-RETORNO
           ELSE
               MOVE  03                TO  RUEC60-COD-RETORNO
IN0923                                     WRK-ERR-TEXTO-COD
IN0923         MOVE 'APL'              TO  ERR-TIPO-ACESSO
IN0923         MOVE 'RUCA8806'         TO  ERR-PGM
IN0923         MOVE WRK-ERR-TEXTO      TO  ERR-TEXTO
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5000-PROCESSAR-DADOS            SECTION.
      *----------------------------------------------------------------*

           PERFORM  5005-CONSULTAR-ENVIO
           PERFORM  5060-ATUALIZAR-SITUACAO
           PERFORM  5070-ATUALIZAR-CTRL-ENVIO
           PERFORM  5075-SALVAR-RETORNO-RCOR

           IF  RUEC60-COD-MSG        EQUAL  'TMNF     '  AND
               RCOR03-CRETOR-PRINC   IS      NUMERIC     AND
               RCOR03-CRETOR-PRINC   EQUAL   ZEROS
               PERFORM  5095-ENVIAR-CONTINUACAO
           END-IF.

           PERFORM  5100-UPDATE-RUCAB015

           IF  CSIT-ALT-REG OF RUCAB015  EQUAL  6
               PERFORM  5105-ATUALIZAR-CONTRATO
           END-IF.

      *----------------------------------------------------------------*
       5000-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5005-CONSULTAR-ENVIO            SECTION.
      *----------------------------------------------------------------*

           IF  RUEC60-COD-MSG  EQUAL  'GEN0004  '
               PERFORM  5025-OPEN-CSR02-RUCAB012
               PERFORM  5030-FETCH-CSR02-RUCAB012
               PERFORM  5035-CLOSE-CSR02-RUCAB012
           ELSE
               PERFORM  5010-OPEN-CSR01-RUCAB012
               PERFORM  5015-FETCH-CSR01-RUCAB012
               PERFORM  5020-CLOSE-CSR01-RUCAB012
           END-IF.

           PERFORM  5040-OBTER-SITUACAO.

      *----------------------------------------------------------------*
       5005-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5010-OPEN-CSR01-RUCAB012        SECTION.
      *----------------------------------------------------------------*

           MOVE  RUEC60-NRO-CTRL-IF    TO  CCTRL-RQUIS-FINCR OF RUCAB012

           EXEC SQL
                OPEN CSR01-RUCAB012
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
              ( SQLWARN0            EQUAL  'W'    )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TCTRL_ENVIO_MANUT'
                                       TO  ERR-DBD-TAB
               MOVE 'OPEN'             TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0001'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  SQLCA(1:136)      TO  LNK-SQLCA
               MOVE  04                TO  RUEC60-COD-RETORNO
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5010-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5015-FETCH-CSR01-RUCAB012       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                FETCH  CSR01-RUCAB012
                INTO   :RUCAB012.CCONTR             ,
                       :RUCAB012.HINCL-REG          ,
                       :RUCAB012.NENVIO-RECOR       ,
                       :RUCAB012.CCTRL-RQUIS-FINCR
                         :WRK-B012-CCTRL-RQUIS-FINCR,
                       :RUCAB012.CUNIC-MSGEM
                         :WRK-B012-CUNIC-MSGEM      ,
                       :RUCAB012.CREFT-RECOR
                         :WRK-B012-CREFT-RECOR
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  AND +100 )  OR
              ( SQLWARN0            EQUAL  'W'             )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TCTRL_ENVIO_MANUT'
                                       TO  ERR-DBD-TAB
               MOVE 'FETCH'            TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0002'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  SQLCA(1:136)      TO  LNK-SQLCA
               MOVE  05                TO  RUEC60-COD-RETORNO
               PERFORM  8000-FINALIZAR
           END-IF.

           IF  SQLCODE              EQUAL  ZEROS
               IF  WRK-B012-CCTRL-RQUIS-FINCR  EQUAL -1
                   MOVE  SPACES        TO  CCTRL-RQUIS-FINCR OF RUCAB012
               END-IF

               IF  WRK-B012-CUNIC-MSGEM        EQUAL -1
                   MOVE  SPACES        TO  CUNIC-MSGEM       OF RUCAB012
               END-IF

               IF  WRK-B012-CREFT-RECOR        EQUAL -1
                   MOVE  SPACES        TO  CREFT-RECOR       OF RUCAB012
               END-IF
           ELSE
               MOVE  06                TO  RUEC60-COD-RETORNO
IN0923                                     WRK-ERR-TEXTO-COD
IN0923         MOVE 'APL'              TO  ERR-TIPO-ACESSO
IN0923         MOVE 'RUCA8806'         TO  ERR-PGM
IN0923         MOVE WRK-ERR-TEXTO      TO  ERR-TEXTO
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5015-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5020-CLOSE-CSR01-RUCAB012       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                CLOSE CSR01-RUCAB012
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
              ( SQLWARN0            EQUAL  'W'    )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TCTRL_ENVIO_MANUT'
                                       TO  ERR-DBD-TAB
               MOVE 'CLOSE'            TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0003'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  SQLCA(1:136)      TO  LNK-SQLCA
               MOVE  07                TO  RUEC60-COD-RETORNO
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5020-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5025-OPEN-CSR02-RUCAB012        SECTION.
      *----------------------------------------------------------------*

           MOVE  RUEC60-SISMSG(1: LENGTH OF RCOR05-REGISTRO)
                                       TO  RCOR05-REGISTRO
           MOVE  RCOR05-NRO-OPE-ORIG   TO  CUNIC-MSGEM      OF RUCAB012

           EXEC SQL
                OPEN CSR02-RUCAB012
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
              ( SQLWARN0            EQUAL  'W'    )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TCTRL_ENVIO_MANUT'
                                       TO  ERR-DBD-TAB
               MOVE 'OPEN'             TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0004'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  SQLCA(1:136)      TO  LNK-SQLCA
               MOVE  08                TO  RUEC60-COD-RETORNO
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5025-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5030-FETCH-CSR02-RUCAB012       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                FETCH  CSR02-RUCAB012
                INTO   :RUCAB012.CCONTR             ,
                       :RUCAB012.HINCL-REG          ,
                       :RUCAB012.NENVIO-RECOR       ,
                       :RUCAB012.CCTRL-RQUIS-FINCR
                         :WRK-B012-CCTRL-RQUIS-FINCR,
                       :RUCAB012.CUNIC-MSGEM
                         :WRK-B012-CUNIC-MSGEM      ,
                       :RUCAB012.CREFT-RECOR
                         :WRK-B012-CREFT-RECOR
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  AND +100 )  OR
              ( SQLWARN0            EQUAL  'W'             )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TCTRL_ENVIO_MANUT'
                                       TO  ERR-DBD-TAB
               MOVE 'FETCH'            TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0005'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  SQLCA(1:136)      TO  LNK-SQLCA
               MOVE  09                TO  RUEC60-COD-RETORNO
               PERFORM  8000-FINALIZAR
           END-IF.

           IF  SQLCODE              EQUAL  ZEROS
               IF  WRK-B012-CCTRL-RQUIS-FINCR  EQUAL -1
                   MOVE  SPACES        TO  CCTRL-RQUIS-FINCR OF RUCAB012
               END-IF

               IF  WRK-B012-CUNIC-MSGEM        EQUAL -1
                   MOVE  SPACES        TO  CUNIC-MSGEM       OF RUCAB012
               END-IF

               IF  WRK-B012-CREFT-RECOR        EQUAL -1
                   MOVE  SPACES        TO  CREFT-RECOR       OF RUCAB012
               END-IF
           ELSE
               MOVE  10                TO  RUEC60-COD-RETORNO
IN0923                                     WRK-ERR-TEXTO-COD
IN0923         MOVE 'APL'              TO  ERR-TIPO-ACESSO
IN0923         MOVE 'RUCA8806'         TO  ERR-PGM
IN0923         MOVE WRK-ERR-TEXTO      TO  ERR-TEXTO
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5030-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5035-CLOSE-CSR02-RUCAB012       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                CLOSE CSR02-RUCAB012
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
              ( SQLWARN0            EQUAL  'W'    )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TCTRL_ENVIO_MANUT'
                                       TO  ERR-DBD-TAB
               MOVE 'CLOSE'            TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0006'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  SQLCA(1:136)      TO  LNK-SQLCA
               MOVE  11                TO  RUEC60-COD-RETORNO
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5035-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5040-OBTER-SITUACAO             SECTION.
      *----------------------------------------------------------------*

           MOVE  ZEROS                 TO  CSIT-MSGEM       OF RUCAB019

           PERFORM  5045-OPEN-CSR03-RUCAB019

           PERFORM  5050-FETCH-CSR03-RUCAB019
             UNTIL  SQLCODE                EQUAL  +100
                OR  CSIT-MSGEM OF RUCAB019 EQUAL  2

           IF  SQLCODE              EQUAL  +100
               MOVE  12                TO  RUEC60-COD-RETORNO
IN0923                                     WRK-ERR-TEXTO-COD
IN0923         MOVE 'APL'              TO  ERR-TIPO-ACESSO
IN0923         MOVE 'RUCA8806'         TO  ERR-PGM
IN0923         MOVE WRK-ERR-TEXTO      TO  ERR-TEXTO
               PERFORM  5055-CLOSE-CSR03-RUCAB019
               PERFORM  8000-FINALIZAR
           ELSE
               PERFORM  5055-CLOSE-CSR03-RUCAB019
           END-IF.

      *----------------------------------------------------------------*
       5040-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5045-OPEN-CSR03-RUCAB019        SECTION.
      *----------------------------------------------------------------*

           MOVE  CCONTR       OF RUCAB012
                                       TO  CCONTR           OF RUCAB019
           MOVE  HINCL-REG    OF RUCAB012
                                       TO  HINCL-REG        OF RUCAB019
           MOVE  NENVIO-RECOR OF RUCAB012
                                       TO  NENVIO-RECOR     OF RUCAB019
           MOVE  ZEROS                 TO  NREG-MSGEM-RECOR OF RUCAB019

           EXEC SQL
                OPEN CSR03-RUCAB019
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
              ( SQLWARN0            EQUAL  'W'    )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TSIT_MANUT_RECOR' TO  ERR-DBD-TAB
               MOVE 'OPEN'             TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0007'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  SQLCA(1:136)      TO  LNK-SQLCA
               MOVE  13                TO  RUEC60-COD-RETORNO
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5045-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5050-FETCH-CSR03-RUCAB019       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                FETCH  CSR03-RUCAB019
                INTO   :RUCAB019.CCONTR             ,
                       :RUCAB019.HINCL-REG          ,
                       :RUCAB019.NENVIO-RECOR       ,
                       :RUCAB019.NREG-MSGEM-RECOR   ,
                       :RUCAB019.CSIT-MSGEM
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  AND +100 )  OR
              ( SQLWARN0            EQUAL  'W'             )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TSIT_MANUT_RECOR' TO  ERR-DBD-TAB
               MOVE 'FETCH'            TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0008'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  SQLCA(1:136)      TO  LNK-SQLCA
               MOVE  14                TO  RUEC60-COD-RETORNO
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5050-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5055-CLOSE-CSR03-RUCAB019       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                CLOSE CSR03-RUCAB019
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
              ( SQLWARN0            EQUAL  'W'    )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TSIT_MANUT_RECOR' TO  ERR-DBD-TAB
               MOVE 'CLOSE'            TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0009'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  SQLCA(1:136)      TO  LNK-SQLCA
               MOVE  15                TO  RUEC60-COD-RETORNO
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5055-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5060-ATUALIZAR-SITUACAO         SECTION.
      *----------------------------------------------------------------*

           EVALUATE  RUEC60-COD-MSG

           WHEN  'COR0001R1'
                 MOVE  RUEC60-SISMSG   TO  RCOR04-REGISTRO

                 IF  RCOR04-COD-ERRO EQUAL '********'
                     MOVE  4           TO  CSIT-MSGEM       OF RUCAB019
                     MOVE  6           TO  CSIT-ALT-REG     OF RUCAB015
                 ELSE
                     MOVE  3           TO  CSIT-MSGEM       OF RUCAB019
                     MOVE  5           TO  CSIT-ALT-REG     OF RUCAB015
                 END-IF

           WHEN  'COR0005R1'
                 MOVE  4               TO  CSIT-MSGEM       OF RUCAB019
                 MOVE  6               TO  CSIT-ALT-REG     OF RUCAB015

           WHEN  'COR0001E '
           WHEN  'COR0005E '
                  MOVE  3              TO  CSIT-MSGEM       OF RUCAB019
                  MOVE  5              TO  CSIT-ALT-REG     OF RUCAB015

           WHEN  'GEN0004  '
                  MOVE  3              TO  CSIT-MSGEM       OF RUCAB019
                  MOVE  5              TO  CSIT-ALT-REG     OF RUCAB015

           WHEN  'TMNF     '
                  MOVE RUEC60-SISMSG(1: LENGTH OF RCOR03-REGISTRO)
                                       TO RCOR03-REGISTRO

                  IF  RCOR03-CRETOR-PRINC  IS   NUMERIC  AND
                      RCOR03-CRETOR-PRINC EQUAL ZEROS
                      MOVE  2          TO  CSIT-MSGEM       OF RUCAB019
                      MOVE  4          TO  CSIT-ALT-REG     OF RUCAB015
                  ELSE
                      MOVE  3          TO  CSIT-MSGEM       OF RUCAB019
                      MOVE  5          TO  CSIT-ALT-REG     OF RUCAB015
                  END-IF
           END-EVALUATE.

           MOVE RUEC60-COD-MSG         TO  CMSGEM-RECOR     OF RUCAB019

           PERFORM  5065-UPDATE-RUCAB019.

      *----------------------------------------------------------------*
       5060-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5065-UPDATE-RUCAB019            SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                UPDATE  DB2PRD.TSIT_MANUT_RECOR

                   SET  CSIT_MSGEM       = :RUCAB019.CSIT-MSGEM    ,
                        CMSGEM_RECOR     = :RUCAB019.CMSGEM-RECOR
                                             :WRK-B019-CMSGEM-RECOR

                 WHERE  CCONTR           = :RUCAB019.CCONTR
                   AND  HINCL_REG        = :RUCAB019.HINCL-REG
                   AND  NENVIO_RECOR     = :RUCAB019.NENVIO-RECOR
                   AND  NREG_MSGEM_RECOR = :RUCAB019.NREG-MSGEM-RECOR
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  AND  +100 )  OR
              ( SQLWARN0            EQUAL  'W'              )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TSIT_MANUT_RECOR'
                                       TO  ERR-DBD-TAB
               MOVE 'UPDATE'           TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0010'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  SQLCA(1:136)      TO  LNK-SQLCA
               MOVE  16                TO  RUEC60-COD-RETORNO
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5065-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5070-ATUALIZAR-CTRL-ENVIO       SECTION.
      *----------------------------------------------------------------*

           EVALUATE  RUEC60-COD-MSG

           WHEN  'GEN0004  '
                  MOVE  RCOR05-NRO-OPE-ORIG
                                       TO  CUNIC-MSGEM      OF RUCAB012
                  MOVE  ZEROS          TO  WRK-B012-CUNIC-MSGEM

           WHEN  'TMNF     '

                  IF  RCOR03-CRETOR-PRINC   IS  NUMERIC  AND
                      RCOR03-CRETOR-PRINC EQUAL ZEROS
                      MOVE  RCOR03-CNRO-UNIC-OPER
                                       TO  CUNIC-MSGEM      OF RUCAB012
                      MOVE  ZEROS      TO  WRK-B012-CUNIC-MSGEM
                  END-IF

           WHEN  OTHER
                 MOVE  SPACES          TO  CUNIC-MSGEM      OF RUCAB012
                 MOVE  -1              TO  WRK-B012-CUNIC-MSGEM
           END-EVALUATE.

           IF  RUEC60-COD-MSG           EQUAL  'COR0001R1'  AND
               RCOR04-NRO-REF-BCCOR NOT EQUAL  SPACES
               MOVE  RCOR04-NRO-REF-BCCOR
                                       TO  CREFT-RECOR      OF RUCAB012
               MOVE  ZEROS             TO  WRK-B012-CREFT-RECOR
           END-IF.

           EXEC SQL
                UPDATE  DB2PRD.TCTRL_ENVIO_MANUT

                   SET  CUNIC_MSGEM       = :RUCAB012.CUNIC-MSGEM
                                              :WRK-B012-CUNIC-MSGEM ,
                        CREFT_RECOR       = :RUCAB012.CREFT-RECOR
                                              :WRK-B012-CREFT-RECOR

                 WHERE  CCONTR            = :RUCAB012.CCONTR
                   AND  HINCL_REG         = :RUCAB012.HINCL-REG
                   AND  NENVIO_RECOR      = :RUCAB012.NENVIO-RECOR
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  AND  +100 )  OR
              ( SQLWARN0            EQUAL  'W'              )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TCTRL_ENVIO_MANUT'
                                       TO  ERR-DBD-TAB
               MOVE 'UPDATE'           TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0011'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  SQLCA(1:136)      TO  LNK-SQLCA
               MOVE  17                TO  RUEC60-COD-RETORNO
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5070-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5075-SALVAR-RETORNO-RCOR        SECTION.
      *----------------------------------------------------------------*

           PERFORM  5080-INSERT-RUCAB013

           IF  SQLCODE              EQUAL  -803
               PERFORM  5085-UPDATE-RUCAB013
           END-IF.

      *----------------------------------------------------------------*
       5075-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5080-INSERT-RUCAB013            SECTION.
      *----------------------------------------------------------------*

           PERFORM  5090-OBTER-DATA-HORA

           MOVE  CCONTR    OF RUCAB019 TO  CCONTR           OF RUCAB013
           MOVE  HINCL-REG OF RUCAB019 TO  HINCL-REG        OF RUCAB013
           MOVE  NENVIO-RECOR          OF  RUCAB019
                                       TO  NENVIO-RECOR     OF RUCAB013
           MOVE  NREG-MSGEM-RECOR OF RUCAB019
                                       TO  NREG-MSGEM-RECOR OF RUCAB013

           IF  RUEC60-COD-MSG       EQUAL  'TMNF     '
               MOVE  2                 TO  CTPO-CONTD-MSGEM OF RUCAB013
           ELSE
               MOVE  3                 TO  CTPO-CONTD-MSGEM OF RUCAB013
           END-IF.

           MOVE  WRK-MES-TIMES         TO  CPTCAO-TBELA     OF RUCAB013

           MOVE  RUEC60-TAM-SISMSG     TO  WCONTD-MSGEM-RURAL-LEN
                                                            OF RUCAB013
           MOVE  RUEC60-SISMSG         TO  WCONTD-MSGEM-RURAL-TEXT
                                                            OF RUCAB013
           MOVE  WRK-TIMESTAMP         TO  HMOVTO-MSGEM-RECOR
                                                            OF RUCAB013

           EXEC SQL
                INSERT  INTO DB2PRD.TENVIO_RETOR_MANUT
                     (  CCONTR                         ,
                        HINCL_REG                      ,
                        NENVIO_RECOR                   ,
                        NREG_MSGEM_RECOR               ,
                        CTPO_CONTD_MSGEM               ,
                        CPTCAO_TBELA                   ,
                        WCONTD_MSGEM_RURAL             ,
                        HMOVTO_MSGEM_RECOR             )
                VALUES
                     ( :RUCAB013.CCONTR                ,
                       :RUCAB013.HINCL-REG             ,
                       :RUCAB013.NENVIO-RECOR          ,
                       :RUCAB013.NREG-MSGEM-RECOR      ,
                       :RUCAB013.CTPO-CONTD-MSGEM      ,
                       :RUCAB013.CPTCAO-TBELA          ,
                       :RUCAB013.WCONTD-MSGEM-RURAL    ,
                       :RUCAB013.HMOVTO-MSGEM-RECOR    )
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  AND  -803 )  OR
              ( SQLWARN0            EQUAL  'W'              )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TENVIO_RETOR_MANUT'
                                       TO  ERR-DBD-TAB
               MOVE 'INSERT'           TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0012'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  SQLCA(1:136)      TO  LNK-SQLCA
               MOVE  18                TO  RUEC60-COD-RETORNO
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5080-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5085-UPDATE-RUCAB013            SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               UPDATE  DB2PRD.TENVIO_RETOR_MANUT

                  SET WCONTD_MSGEM_RURAL = :RUCAB013.WCONTD-MSGEM-RURAL,
                      HMOVTO_MSGEM_RECOR = :RUCAB013.HMOVTO-MSGEM-RECOR

                WHERE  CCONTR            = :RUCAB013.CCONTR
                  AND  HINCL_REG         = :RUCAB013.HINCL-REG
                  AND  NENVIO_RECOR      = :RUCAB013.NENVIO-RECOR
                  AND  NREG_MSGEM_RECOR  = :RUCAB013.NREG-MSGEM-RECOR
                  AND  CTPO_CONTD_MSGEM  = :RUCAB013.CTPO-CONTD-MSGEM
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  AND  -803 )  OR
              ( SQLWARN0            EQUAL  'W'              )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TENVIO_RETOR_MANUT'
                                       TO  ERR-DBD-TAB
               MOVE 'UPDATE'           TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0013'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  SQLCA(1:136)      TO  LNK-SQLCA
               MOVE  19                TO  RUEC60-COD-RETORNO
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5085-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5090-OBTER-DATA-HORA            SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                      WRK-7600-DATA-HORA

           CALL 'BRAD7600'          USING  WRK-7600-DATA-HORA

           MOVE  WRK-7600-ANO-TIMES    TO  WRK-ANO-TIMES
           MOVE  WRK-7600-MES-TIMES    TO  WRK-MES-TIMES
           MOVE  WRK-7600-DIA-TIMES    TO  WRK-DIA-TIMES
           MOVE  WRK-7600-HOR-TIMES    TO  WRK-HOR-TIMES
           MOVE  WRK-7600-MIN-TIMES    TO  WRK-MIN-TIMES
           MOVE  WRK-7600-SEG-TIMES    TO  WRK-SEG-TIMES
           MOVE  WRK-7600-MIL-TIMES    TO  WRK-MIL-TIMES.

      *----------------------------------------------------------------*
       5090-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5095-ENVIAR-CONTINUACAO         SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                      WRK-AREA-RUCA8812

           MOVE  CCONTR    OF RUCAB019 TO  WRK-S09-DRESS
           MOVE  WRK-N09-DRESS         TO  WRK-8812-CHAVE
           MOVE  HINCL-REG OF RUCAB019 TO  WRK-TIMESTAMP
           MOVE  WRK-ANO-TIMES         TO  WRK-8812-ANO-CTRLNAV
           MOVE  WRK-MES-TIMES         TO  WRK-8812-MES-CTRLNAV
           MOVE  WRK-DIA-TIMES         TO  WRK-8812-DIA-CTRLNAV
           MOVE  WRK-HOR-TIMES         TO  WRK-8812-HOR-CTRLNAV
           MOVE  WRK-MIN-TIMES         TO  WRK-8812-MIN-CTRLNAV
           MOVE  WRK-SEG-TIMES         TO  WRK-8812-SEG-CTRLNAV
           MOVE  WRK-MIL-TIMES         TO  WRK-8812-MIL-CTRLNAV
           MOVE  'RUCA8812'            TO  WRK-MODULO

           CALL  WRK-MODULO         USING  WRK-AREA-RUCA8812

           EVALUATE  WRK-8812-COD-RETORNO

           WHEN  ZEROS
                 MOVE  4               TO  CSIT-ALT-REG     OF RUCAB015

           WHEN  03
                 CONTINUE

           WHEN  88
                 MOVE  WRK-8812-ERRO-AREA
                                       TO  ERRO-AREA
                 MOVE  WRK-8812-SQLCA  TO  LNK-SQLCA
                 MOVE  20              TO  RUEC60-COD-RETORNO
                 PERFORM  8000-FINALIZAR

           WHEN  99
                 MOVE  WRK-8812-ERRO-AREA
                                       TO  ERRO-AREA
                 MOVE  WRK-8812-SQLCA  TO  LNK-SQLCA
                 MOVE  21              TO  RUEC60-COD-RETORNO
                 PERFORM  8000-FINALIZAR

           WHEN  OTHER
                 MOVE  22              TO  RUEC60-COD-RETORNO
IN0923                                     WRK-ERR-TEXTO-COD
IN0923           MOVE 'APL'            TO  ERR-TIPO-ACESSO
IN0923           MOVE 'RUCA8806'       TO  ERR-PGM
IN0923           MOVE WRK-ERR-TEXTO    TO  ERR-TEXTO
                 PERFORM  8000-FINALIZAR

           END-EVALUATE.

      *----------------------------------------------------------------*
       5095-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5100-UPDATE-RUCAB015            SECTION.
      *----------------------------------------------------------------*

           MOVE  CCONTR    OF RUCAB012 TO  CCONTR           OF RUCAB015
           MOVE  HINCL-REG OF RUCAB012 TO  HINCL-REG        OF RUCAB015

           EXEC SQL
             UPDATE DB2PRD.TMANUT_CONTR_RECOR

                SET CSIT_ALT_REG       = :RUCAB015.CSIT-ALT-REG

             WHERE  CCONTR             = :RUCAB015.CCONTR
               AND  HINCL_REG          = :RUCAB015.HINCL-REG
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  AND  +100 )  OR
              ( SQLWARN0            EQUAL  'W'              )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TMANUT_CONTR_RECOR'
                                       TO  ERR-DBD-TAB
               MOVE 'UPDATE'           TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0014'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  SQLCA(1:136)      TO  LNK-SQLCA
               MOVE  23                TO  RUEC60-COD-RETORNO
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5100-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5105-ATUALIZAR-CONTRATO         SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                      WRK-AREA-RUCA8807

           MOVE  CCONTR    OF RUCAB019 TO  WRK-S09-DRESS
           MOVE  WRK-N09-DRESS         TO  WRK-8807-CHAVE
           MOVE  HINCL-REG OF RUCAB019 TO  WRK-TIMESTAMP
           MOVE  WRK-ANO-TIMES         TO  WRK-8807-ANO-CTRLNAV
           MOVE  WRK-MES-TIMES         TO  WRK-8807-MES-CTRLNAV
           MOVE  WRK-DIA-TIMES         TO  WRK-8807-DIA-CTRLNAV
           MOVE  WRK-HOR-TIMES         TO  WRK-8807-HOR-CTRLNAV
           MOVE  WRK-MIN-TIMES         TO  WRK-8807-MIN-CTRLNAV
           MOVE  WRK-SEG-TIMES         TO  WRK-8807-SEG-CTRLNAV
           MOVE  WRK-MIL-TIMES         TO  WRK-8807-MIL-CTRLNAV
           MOVE  NENVIO-RECOR          OF  RUCAB019
                                       TO  WRK-S09-DRESS
           MOVE  WRK-N05-DRESS         TO  WRK-8807-NENVIO
7C5.10     MOVE  RUEC60-COD-MSG        TO  WRK-8807-CODMSG

           MOVE  'RUCA8807'            TO  WRK-MODULO

           CALL  WRK-MODULO         USING  WRK-AREA-RUCA8807

           EVALUATE  WRK-8807-COD-RETORNO

           WHEN  ZEROS
                 CONTINUE

           WHEN  99
                 MOVE  WRK-8807-ERRO-AREA
                                       TO  ERRO-AREA
                 MOVE  WRK-8807-SQLCA  TO  LNK-SQLCA
                 MOVE  24              TO  RUEC60-COD-RETORNO
                 PERFORM  8000-FINALIZAR

           WHEN  OTHER
                 MOVE  25              TO  RUEC60-COD-RETORNO
IN0923                                     WRK-ERR-TEXTO-COD
IN0923           MOVE 'APL'            TO  ERR-TIPO-ACESSO
IN0923           MOVE 'RUCA8806'       TO  ERR-PGM
IN0923           MOVE WRK-ERR-TEXTO    TO  ERR-TEXTO
                 PERFORM  8000-FINALIZAR

           END-EVALUATE.

      *----------------------------------------------------------------*
       5105-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       8000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       8000-99-FIM.  EXIT.
      *----------------------------------------------------------------*
