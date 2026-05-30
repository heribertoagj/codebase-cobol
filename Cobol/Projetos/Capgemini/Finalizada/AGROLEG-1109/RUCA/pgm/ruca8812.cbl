      ******************************************************************
       IDENTIFICATION                  DIVISION.
      ******************************************************************
       PROGRAM-ID.    RUCA8812.
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
      *        ENVIAR PARA O BACEN COR0001 E COR0005 DE ALTERACAO.     *
      *                                                                *
      *----------------------------------------------------------------*
      *  MODULOS CHAMADOS:                                             *
      *                                                                *
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
           '*** AREA PARA TRATAMENTO DE ERRO ***'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** AREA DE MENSAGENS ***'.
      *----------------------------------------------------------------*

       01  WRK-MENSAGENS.
           05  WRK-MSG-00              PIC  X(75)          VALUE
           '00 - ENVIO EFETUADO COM SUCESSO'.
           05  WRK-MSG-01              PIC  X(75)          VALUE
           '01 - CAMPO CHAVE COM CONTEUDO INVALIDO'.
           05  WRK-MSG-02              PIC  X(75)          VALUE
           '02 - CAMPO HORA DE INCLUSAO COM CONTEUDO INVALIDO'.
           05  WRK-MSG-03              PIC  X(75)          VALUE
           '03 - NAO EXISTE ALTERACAO PARA SER ENVIADA AO RCOR'.
           05  WRK-MSG-88              PIC  X(75)          VALUE
           '88 - ERRO ACESSO RCOR5005'.
           05  WRK-MSG-99              PIC  X(75)          VALUE
           '99 - ERRO DE DB2'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** AREA PARA DRESSCODE ***'.
      *----------------------------------------------------------------*

       01  WRK-S05-DRESS               PIC +9(05)          VALUE ZEROS.
       01  FILLER                      REDEFINES  WRK-S05-DRESS.
           05  FILLER                  PIC  X(03).
           05  WRK-N03-DRESS           PIC  9(03).

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** ACUMULADORES ***'.
      *----------------------------------------------------------------*

       01  ACU-QUANTIDADE              PIC  9(03) COMP-3   VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** AREAS AUXILIARES ***'.
      *----------------------------------------------------------------*

       01  WRK-NENVIO-RECOR            PIC S9(05) COMP-3   VALUE ZEROS.
       01  WRK-NREG-MSGEM-RECOR        PIC S9(05) COMP-3   VALUE ZEROS.
       01  WRK-MODULO                  PIC  X(08)          VALUE SPACES.

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

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(050)          VALUE
           '*** AREAS DO MODULO RCOR5005 ***'.
      *---------------------------------------------------------------*

       COPY 'I#RCOR10'.

       01  WRK-5005-ERRO-AREA          PIC  X(107)        VALUE SPACES.
       01  WRK-5005-SQLCA              PIC  X(136)        VALUE SPACES.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** AREAS PARA TRATAR NULO ***'.
      *----------------------------------------------------------------*

       01  WRK-B012-CCTRL-RQUIS-FINCR  PIC S9(04)  COMP    VALUE ZEROS.

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
              DECLARE  CSR01-RUCAB019 CURSOR WITH HOLD FOR
               SELECT  CCONTR             ,
                       HINCL_REG          ,
                       NENVIO_RECOR       ,
                       NREG_MSGEM_RECOR   ,
                       CSIT_MSGEM
                 FROM  DB2PRD.TSIT_MANUT_RECOR
                WHERE  CCONTR             = :RUCAB019.CCONTR
                  AND  HINCL_REG          = :RUCAB019.HINCL-REG
                  AND  NENVIO_RECOR      >= :RUCAB019.NENVIO-RECOR
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

       01  COMU-AREA-RUCA8812.
           05  COMU-8812-ENTRADA.
               07  COMU-8812-CHAVE     PIC  9(09).
               07  COMU-8812-CTRLNAV.
                   10  COMU-8812-ANO-CTRLNAV
                                       PIC  9(04).
                   10  COMU-8812-MES-CTRLNAV
                                       PIC  9(02).
                   10  COMU-8812-DIA-CTRLNAV
                                       PIC  9(02).
                   10  COMU-8812-HOR-CTRLNAV
                                       PIC  9(02).
                   10  COMU-8812-MIN-CTRLNAV
                                       PIC  9(02).
                   10  COMU-8812-SEG-CTRLNAV
                                       PIC  9(02).
                   10  COMU-8812-MIL-CTRLNAV
                                       PIC  9(06).
           05  COMU-8812-SAIDA.
               07  COMU-8812-COD-RETORNO
                                       PIC  9(02).
               07  COMU-8812-MENSAGEM  PIC  X(75).
               07  COMU-8812-ERRO-AREA PIC  X(107).
               07  COMU-8812-SQLCA     PIC  X(136).

      ******************************************************************
       PROCEDURE                      DIVISION USING COMU-AREA-RUCA8812.
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

           MOVE  ZEROS                 TO  COMU-8812-COD-RETORNO
           MOVE  WRK-MSG-00            TO  COMU-8812-MENSAGEM
           MOVE  SPACES                TO  COMU-8812-ERRO-AREA
                                           COMU-8812-SQLCA

           IF  COMU-8812-CHAVE         IS  NUMERIC  AND
               COMU-8812-CHAVE    GREATER  ZEROS
               CONTINUE
           ELSE
               MOVE  01                TO  COMU-8812-COD-RETORNO
               MOVE  WRK-MSG-01        TO  COMU-8812-MENSAGEM
               PERFORM  8000-FINALIZAR
           END-IF.

           IF  COMU-8812-CTRLNAV       IS  NUMERIC  AND
               COMU-8812-CTRLNAV  GREATER  ZEROS
               MOVE  COMU-8812-ANO-CTRLNAV
                                       TO  WRK-ANO-TIMES
               MOVE  COMU-8812-MES-CTRLNAV
                                       TO  WRK-MES-TIMES
               MOVE  COMU-8812-DIA-CTRLNAV
                                       TO  WRK-DIA-TIMES
               MOVE  COMU-8812-HOR-CTRLNAV
                                       TO  WRK-HOR-TIMES
               MOVE  COMU-8812-MIN-CTRLNAV
                                       TO  WRK-MIN-TIMES
               MOVE  COMU-8812-SEG-CTRLNAV
                                       TO  WRK-SEG-TIMES
               MOVE  COMU-8812-MIL-CTRLNAV
                                       TO  WRK-MIL-TIMES
           ELSE
               MOVE  02                TO  COMU-8812-COD-RETORNO
               MOVE  WRK-MSG-02        TO  COMU-8812-MENSAGEM
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5000-PROCESSAR-DADOS            SECTION.
      *----------------------------------------------------------------*

           PERFORM  5005-OBTER-ENVIO
           PERFORM  5025-SELECT-RUCAB012
           PERFORM  5030-SELECT-RUCAB013
           PERFORM  5035-ENVIAR-ALTERACAO.

      *----------------------------------------------------------------*
       5000-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5005-OBTER-ENVIO                SECTION.
      *----------------------------------------------------------------*

           MOVE  ZEROS                 TO  ACU-QUANTIDADE

           PERFORM  5010-OPEN-CSR01-RUCAB019
           PERFORM  5015-FETCH-CSR01-RUCAB019

           PERFORM
             UNTIL  SQLCODE   EQUAL  +100

                IF  CSIT-MSGEM OF RUCAB019 EQUAL  1
                    IF  ACU-QUANTIDADE   EQUAL  ZEROS
                        MOVE  NENVIO-RECOR     OF  RUCAB019
                                       TO  WRK-NENVIO-RECOR
                        MOVE  NREG-MSGEM-RECOR OF  RUCAB019
                                       TO  WRK-NREG-MSGEM-RECOR
                        ADD   1        TO  ACU-QUANTIDADE
                    ELSE
                    IF  NENVIO-RECOR OF RUCAB019 EQUAL WRK-NENVIO-RECOR
                        ADD   1        TO  ACU-QUANTIDADE
                    END-IF
                    END-IF
                END-IF

                PERFORM  5015-FETCH-CSR01-RUCAB019
           END-PERFORM.

           PERFORM  5020-CLOSE-CSR01-RUCAB019

           IF  ACU-QUANTIDADE       EQUAL  ZEROS
               MOVE  03                TO  COMU-8812-COD-RETORNO
               MOVE  WRK-MSG-03        TO  COMU-8812-MENSAGEM
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5005-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5010-OPEN-CSR01-RUCAB019        SECTION.
      *----------------------------------------------------------------*

           MOVE  COMU-8812-CHAVE       TO  CCONTR           OF RUCAB019
           MOVE  WRK-TIMESTAMP         TO  HINCL-REG        OF RUCAB019
           MOVE  ZEROS                 TO  NENVIO-RECOR     OF RUCAB019
                                           NREG-MSGEM-RECOR OF RUCAB019

           EXEC SQL
                OPEN CSR01-RUCAB019
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
              ( SQLWARN0            EQUAL  'W'    )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TSIT_MANUT_RECOR' TO  ERR-DBD-TAB
               MOVE 'OPEN'             TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0001'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8812-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8812-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8812-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8812-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5010-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5015-FETCH-CSR01-RUCAB019       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                FETCH  CSR01-RUCAB019
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
               MOVE '0002'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8812-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8812-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8812-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8812-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5015-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5020-CLOSE-CSR01-RUCAB019       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                CLOSE CSR01-RUCAB019
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
              ( SQLWARN0            EQUAL  'W'    )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TSIT_MANUT_RECOR' TO  ERR-DBD-TAB
               MOVE 'CLOSE'            TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0003'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8812-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8812-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8812-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8812-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5020-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5025-SELECT-RUCAB012            SECTION.
      *----------------------------------------------------------------*

           MOVE  COMU-8812-CHAVE       TO  CCONTR           OF RUCAB012
           MOVE  WRK-TIMESTAMP         TO  HINCL-REG        OF RUCAB012
           MOVE  WRK-NENVIO-RECOR      TO  NENVIO-RECOR     OF RUCAB012

           EXEC SQL
                SELECT
                        CCONTR                    ,
                        HINCL_REG                 ,
                        NENVIO_RECOR              ,
                        CCTRL_RQUIS_FINCR
                  INTO
                       :RUCAB012.CCONTR           ,
                       :RUCAB012.HINCL-REG        ,
                       :RUCAB012.NENVIO-RECOR     ,
                       :RUCAB012.CCTRL-RQUIS-FINCR
                         :WRK-B012-CCTRL-RQUIS-FINCR
                  FROM  DB2PRD.TCTRL_ENVIO_MANUT
                 WHERE  CCONTR        = :RUCAB012.CCONTR
                   AND  HINCL_REG     = :RUCAB012.HINCL-REG
                   AND  NENVIO_RECOR  = :RUCAB012.NENVIO-RECOR
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
              ( SQLCODE             EQUAL +100    )  OR
              ( SQLWARN0            EQUAL  'W'    )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TCTRL_ENVIO_MANUT'
                                       TO  ERR-DBD-TAB
               MOVE 'SELECT'           TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0004'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8812-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8812-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8812-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8812-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

           IF  WRK-B012-CCTRL-RQUIS-FINCR  EQUAL  -1
               MOVE  SPACES            TO  CCTRL-RQUIS-FINCR OF RUCAB012
           END-IF.

      *----------------------------------------------------------------*
       5025-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5030-SELECT-RUCAB013            SECTION.
      *----------------------------------------------------------------*

           MOVE  COMU-8812-CHAVE       TO  CCONTR           OF RUCAB013
           MOVE  WRK-TIMESTAMP         TO  HINCL-REG        OF RUCAB013
           MOVE  WRK-NENVIO-RECOR      TO  NENVIO-RECOR     OF RUCAB013
           MOVE  WRK-NREG-MSGEM-RECOR  TO  NREG-MSGEM-RECOR OF RUCAB013
           MOVE  1                     TO  CTPO-CONTD-MSGEM OF RUCAB013

           EXEC SQL
                SELECT
                        CCONTR                      ,
                        HINCL_REG                   ,
                        NENVIO_RECOR                ,
                        NREG_MSGEM_RECOR            ,
                        CTPO_CONTD_MSGEM            ,
                        WCONTD_MSGEM_RURAL
                  INTO
                       :RUCAB013.CCONTR             ,
                       :RUCAB013.HINCL-REG          ,
                       :RUCAB013.NENVIO-RECOR       ,
                       :RUCAB013.NREG-MSGEM-RECOR   ,
                       :RUCAB013.CTPO-CONTD-MSGEM   ,
                       :RUCAB013.WCONTD-MSGEM-RURAL
                  FROM  DB2PRD.TENVIO_RETOR_MANUT
                 WHERE  CCONTR           = :RUCAB013.CCONTR
                   AND  HINCL_REG        = :RUCAB013.HINCL-REG
                   AND  NENVIO_RECOR     = :RUCAB013.NENVIO-RECOR
                   AND  NREG_MSGEM_RECOR = :RUCAB013.NREG-MSGEM-RECOR
                   AND  CTPO_CONTD_MSGEM = :RUCAB013.CTPO-CONTD-MSGEM
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
              ( SQLCODE             EQUAL +100    )  OR
              ( SQLWARN0            EQUAL  'W'    )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TENVIO_RETOR_MANUT'
                                       TO  ERR-DBD-TAB
               MOVE 'SELECT'           TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0005'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8812-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8812-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8812-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8812-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5030-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5035-ENVIAR-ALTERACAO           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                      RCOR10-REGISTRO

           MOVE  'RUC1'                TO  RCOR10-CCUSTO

           MOVE  WCONTD-MSGEM-RURAL-LEN    OF RUCAB013
                                       TO  RCOR10-TAM-SISMSG

           MOVE  WRK-NREG-MSGEM-RECOR  TO  WRK-S05-DRESS
           MOVE  WRK-N03-DRESS         TO  RCOR10-SEQ-MSG

           IF  ACU-QUANTIDADE       EQUAL  1
              MOVE 'N'                 TO  RCOR10-IND-CONTINUA
           ELSE
              MOVE 'S'                 TO  RCOR10-IND-CONTINUA
           END-IF.

           MOVE  CCTRL-RQUIS-FINCR     OF  RUCAB012
                                       TO  RCOR10-NRO-CTRLIF

           MOVE  WCONTD-MSGEM-RURAL-TEXT   OF RUCAB013
                                           (1: LENGTH OF RCOR10-SISMSG)
                                       TO  RCOR10-SISMSG

           MOVE  'RCOR5005'            TO  WRK-MODULO

           CALL  WRK-MODULO         USING  RCOR10-REGISTRO
                                           WRK-5005-ERRO-AREA
                                           WRK-5005-SQLCA

           IF  RCOR10-COD-RETORNO   EQUAL  ZEROS
               PERFORM  5040-UPDATE-RUCAB015
               PERFORM  5045-UPDATE-RUCAB012
               PERFORM  5050-UPDATE-RUCAB019
               PERFORM  5055-UPDATE-RUCAB013
           ELSE
               MOVE  88                TO  COMU-8812-COD-RETORNO
               MOVE  WRK-MSG-88        TO  COMU-8812-MENSAGEM
               MOVE  WRK-5005-ERRO-AREA
                                       TO  COMU-8812-ERRO-AREA
               MOVE  WRK-5005-SQLCA    TO  COMU-8812-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5035-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5040-UPDATE-RUCAB015            SECTION.
      *----------------------------------------------------------------*

           MOVE  COMU-8812-CHAVE       TO  CCONTR           OF RUCAB015
           MOVE  WRK-TIMESTAMP         TO  HINCL-REG        OF RUCAB015
           MOVE  3                     TO  CSIT-ALT-REG     OF RUCAB015

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
               MOVE '0006'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8812-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8812-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8812-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8812-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5040-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5045-UPDATE-RUCAB012            SECTION.
      *----------------------------------------------------------------*

           MOVE  COMU-8812-CHAVE       TO  CCONTR           OF RUCAB012
           MOVE  WRK-TIMESTAMP         TO  HINCL-REG        OF RUCAB012
           MOVE  WRK-NENVIO-RECOR      TO  NENVIO-RECOR     OF RUCAB012
           MOVE  RCOR10-NRO-CTRL-IF    TO  CCTRL-RQUIS-FINCR
                                                            OF RUCAB012
           MOVE  ZEROS                 TO  WRK-B012-CCTRL-RQUIS-FINCR


           EXEC SQL
                UPDATE  DB2PRD.TCTRL_ENVIO_MANUT

                   SET  CCTRL_RQUIS_FINCR = :RUCAB012.CCTRL-RQUIS-FINCR
                                             :WRK-B012-CCTRL-RQUIS-FINCR

                 WHERE  CCONTR         = :RUCAB012.CCONTR
                   AND  HINCL_REG      = :RUCAB012.HINCL-REG
                   AND  NENVIO_RECOR   = :RUCAB012.NENVIO-RECOR
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  AND  +100 )  OR
              ( SQLWARN0            EQUAL  'W'              )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TCTRL_ENVIO_MANUT'
                                       TO  ERR-DBD-TAB
               MOVE 'UPDATE'           TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0007'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8812-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8812-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8812-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8812-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5045-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5050-UPDATE-RUCAB019            SECTION.
      *----------------------------------------------------------------*

           MOVE  COMU-8812-CHAVE       TO  CCONTR           OF RUCAB019
           MOVE  WRK-TIMESTAMP         TO  HINCL-REG        OF RUCAB019
           MOVE  WRK-NENVIO-RECOR      TO  NENVIO-RECOR     OF RUCAB019
           MOVE  WRK-NREG-MSGEM-RECOR  TO  NREG-MSGEM-RECOR OF RUCAB019
           MOVE  2                     TO  CSIT-MSGEM       OF RUCAB019

           EXEC SQL
                UPDATE  DB2PRD.TSIT_MANUT_RECOR

                   SET  CSIT_MSGEM       = :RUCAB019.CSIT-MSGEM

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
               MOVE '0008'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8812-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8812-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8812-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8812-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5050-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5055-UPDATE-RUCAB013            SECTION.
      *----------------------------------------------------------------*

           MOVE  COMU-8812-CHAVE       TO  CCONTR           OF RUCAB013
           MOVE  WRK-TIMESTAMP         TO  HINCL-REG        OF RUCAB013
           MOVE  WRK-NENVIO-RECOR      TO  NENVIO-RECOR     OF RUCAB013
           MOVE  WRK-NREG-MSGEM-RECOR  TO  NREG-MSGEM-RECOR OF RUCAB013
           MOVE  1                     TO  CTPO-CONTD-MSGEM OF RUCAB013
           MOVE  RCOR10-NRO-CTRL-IF    TO  WCONTD-MSGEM-RURAL-TEXT
                                           OF RUCAB013 (10: 20)

           EXEC SQL
                UPDATE  DB2PRD.TENVIO_RETOR_MANUT

                   SET WCONTD_MSGEM_RURAL = :RUCAB013.WCONTD-MSGEM-RURAL

                 WHERE  CCONTR            = :RUCAB013.CCONTR
                   AND  HINCL_REG         = :RUCAB013.HINCL-REG
                   AND  NENVIO_RECOR      = :RUCAB013.NENVIO-RECOR
                   AND  NREG_MSGEM_RECOR  = :RUCAB013.NREG-MSGEM-RECOR
                   AND  CTPO_CONTD_MSGEM  = :RUCAB013.CTPO-CONTD-MSGEM
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  AND  +100 )  OR
              ( SQLWARN0            EQUAL  'W'              )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TENVIO_RETOR_MANUT'
                                       TO  ERR-DBD-TAB
               MOVE 'UPDATE'           TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0009'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8812-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8812-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8812-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8812-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5055-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       8000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       8000-99-FIM.  EXIT.
      *----------------------------------------------------------------*
