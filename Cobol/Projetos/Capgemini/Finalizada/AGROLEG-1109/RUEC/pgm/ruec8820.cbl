      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. RUEC8820.
       AUTHOR.     MARINA FUJITA.
      *================================================================*
      *                    S O N D A  P R O C W O R K                  *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   RUEC8820                                    *
      *    ANALISTA....:   MARINA FUJITA           - SONDA PROCWORK    *
      *    DATA........:   21/06/2012                                  *
      *    PROJETO.....:   PWT 12/9931    2012/81536/5-01              *
      *                    REFORMULACAO MENSAGERIA BACEN - RECOR       *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   GERAR DADOS P/ ENVIO RECOR NAS TABELAS DB2  *
      *                    RUECB099, RUECB09B - COR0002 (EXCLUSAO)     *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#RUEC62 - AREA DE RETORNO DO MODULO.                       *
      *    I#BRAD7C - AREA PARA TRATAMENTO DE ERRO DB2.                *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    BRAD7100 - VERIFICAR DISPONIBILIDADE                        *
      *    MESU9032 - OBTER DADOS DA AGENCIA                           *
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
           '*** INICIO DA WORKING RUEC8820 ***'.
      *---------------------------------------------------------------*
       77  FILLER                      PIC X(079)          VALUE
           '*** VARIAVEIS AUXILIARES ***'.
       77  WRK-MODULO                  PIC X(008)     VALUE SPACES.
       77  WRK-CINDCD-SIT-CDULA-NULL   PIC S9(04) COMP VALUE ZEROS.

       01  WRK-CTRL-CHAR        PIC X(02)  VALUE SPACES.
       01  FILLER               REDEFINES  WRK-CTRL-CHAR.
           05 WRK-CTRL-NUM      PIC 9(02).

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
           03  WRK-TIMESTAMP           PIC X(20)         VALUE SPACES.

       01  FILLER                      PIC X(079)          VALUE
           '*** AREA PARA CHAMADA MODULO MESU9116 ***'.
      *---------------------------------------------------------------*

       COPY I#MESUM9.

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(079)          VALUE
           '*** AREA PARA CHAMADA MODULO MESU9032 ***'.
      *---------------------------------------------------------------*

       01  WRK-MESU9032.
           03  WRK-ENTRADA-MESU9032.
               05  WRK-NOME-PGM        PIC X(08)     VALUE SPACES.
               05  WRK-COD-RETORNO     PIC 9(02)     VALUE ZEROS.
               05  WRK-AGENCIA         PIC 9(05)     VALUE ZEROS.
           03  WRK-SAIDAS-MESU9032.
               05  WRK-NOME            PIC X(20)     VALUE SPACES.
               05  WRK-ENDERECO        PIC X(30)     VALUE SPACES.
               05  WRK-CIDADE          PIC X(25)     VALUE SPACES.
               05  WRK-ESTADO          PIC X(02)     VALUE SPACES.
               05  WRK-CEP             PIC 9(05)     VALUE ZEROS.
               05  WRK-REGIO-DIRET     PIC 9(03)     VALUE ZEROS.
               05  WRK-JUNC-REGIO      PIC 9(05)     VALUE ZEROS.
               05  WRK-NOVOCEP         PIC 9(09)     VALUE ZEROS.
               05  WRK-NOME-GER        PIC X(30)     VALUE SPACES.
               05  WRK-DT-INAUGUR      PIC X(10)     VALUE SPACES.
               05  WRK-DT-ENCRESU      PIC X(10)     VALUE SPACES.
               05  WRK-LOJA            PIC 9(05)     VALUE ZEROS.
               05  WRK-CONTROLE        PIC X(02)     VALUE SPACES.

       01  WRK-POOL7100-MESU9032       PIC X(107)    VALUE SPACES.
       01  WRK-SQLCA-MESU9032          PIC X(136)    VALUE SPACES.
       01  WRK-AMBIENTE-MESU9032       PIC X(01)     VALUE SPACES.
       01  WRK-IO-PCB-MESU9032         PIC X(32)     VALUE SPACES.
       01  WRK-ALT-PCB-MESU9032        PIC X(32)     VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(079)          VALUE
           '*** AREA PARA FORMATACAO DE ERRO ***'.
      *---------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(079)          VALUE
           '*** AREA PARA TABELAS DB2 ***'.
      *---------------------------------------------------------------*

           EXEC SQL INCLUDE SQLCA    END-EXEC.

           EXEC SQL INCLUDE RUFIB002 END-EXEC.

           EXEC SQL INCLUDE RUECB096 END-EXEC.

           EXEC SQL INCLUDE RUECB099 END-EXEC.

           EXEC SQL INCLUDE RUECB09B END-EXEC.

           EXEC SQL
             DECLARE CSR01-RUECB096 CURSOR FOR
              SELECT CINDCD_SIT_CDULA
                   , CREFT_RECOR
                   , CMOTVO_EXCL_RECOR
                FROM  DB2PRD.THIST_SIT_CDULA
               WHERE  CCONTR_CREDT_RURAL = :RUECB096.CCONTR-CREDT-RURAL
               ORDER BY NSEQ_HIST_SIT DESC
           END-EXEC.

      *---------------------------------------------------------------*
       01  FILLER   PIC X(32) VALUE '* FIM DA WORKING RUEC8820 *'.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       LINKAGE SECTION.
      *---------------------------------------------------------------*

       COPY 'I#RUEC62'.

      *================================================================*
       PROCEDURE DIVISION          USING RUEC62-AREA.
      *================================================================*

      *---------------------------------------------------------------*
       0000-INICIO                 SECTION.
      *---------------------------------------------------------------*

           CALL  'CKRS1000'.

           MOVE ZEROS              TO RUEC62-COD-RET.

           PERFORM  1000-PROCESSAMENTO-PRINCIPAL.

           PERFORM  4000-ROTINA-FIM.

      *---------------------------------------------------------------*
       0000-99-FIM.                EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1000-PROCESSAMENTO-PRINCIPAL SECTION.
      *---------------------------------------------------------------*

           CALL 'POOL7600'  USING  WRK-DATA-HORA.

           PERFORM 2000-VALIDAR-ARG-ENTRADA.

           IF  RUEC62-COD-RET  EQUAL  ZEROS
               PERFORM 3000-OBTEM-DADOS
               PERFORM 3100-GERA-CONTRATO
               PERFORM 3900-ATUALIZA-CONTRATO
           END-IF.

      *---------------------------------------------------------------*
       1000-99-FIM.                EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2000-VALIDAR-ARG-ENTRADA    SECTION.
      *---------------------------------------------------------------*

           IF  RUEC62-PGM-CHAMADOR EQUAL SPACES
               MOVE 01             TO RUEC62-COD-RET
               MOVE 'PGM CHAMADOR INVALIDO'
                                   TO RUEC62-MENSAGEM
               GO  TO  2000-99-FIM
           END-IF.

           IF (RUEC62-CCONTR       NOT   NUMERIC)
           OR (RUEC62-CCONTR       EQUAL ZEROS)
               MOVE 01             TO RUEC62-COD-RET
               MOVE 'CONTRATO INVALIDO'
                                   TO RUEC62-MENSAGEM
               GO  TO  2000-99-FIM
           END-IF.

           IF  RUEC62-PROCESS      NOT  EQUAL  'B'  AND 'O'
               MOVE 01             TO RUEC62-COD-RET
               MOVE 'FORMA DE PROCESSAMENTO INVALIDA'
                                   TO RUEC62-MENSAGEM
               GO  TO  2000-99-FIM
           END-IF.

      *---------------------------------------------------------------*
       2000-99-FIM.                EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3000-OBTEM-DADOS            SECTION.
      *---------------------------------------------------------------*

           MOVE RUEC62-CCONTR     TO CCONTR             OF RUFIB002

           EXEC SQL
               SELECT   CCONTR
                      , CJUNC_DEPDC
                      , CREFT_RECOR
               INTO     :RUFIB002.CCONTR
                      , :RUFIB002.CJUNC-DEPDC
                      , :RUFIB002.CREFT-RECOR
                 FROM DB2PRD.CONTRATO_RURAL
                WHERE CCONTR              = :RUFIB002.CCONTR
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 99                  TO RUEC62-COD-RET
               MOVE 'ERRO ACESSO DB2'   TO RUEC62-MENSAGEM
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'CONTRATO_RURAL   ' TO ERR-DBD-TAB
               MOVE 'SELECT'            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0010'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 4000-ROTINA-FIM
           END-IF.

           IF  SQLCODE                  EQUAL +100
               MOVE 03                  TO RUEC62-COD-RET
               MOVE 'CONTRATO INEXISTENTE NO RUFIB002'
                                        TO RUEC62-MENSAGEM
               PERFORM 4000-ROTINA-FIM
           END-IF.

           IF  CREFT-RECOR OF RUFIB002      EQUAL  SPACES  OR  ZEROS
               MOVE 04                  TO RUEC62-COD-RET
               MOVE 'NUMERO RECOR INEXISTENTE P/ CONTRATO'
                                        TO RUEC62-MENSAGEM
               PERFORM 4000-ROTINA-FIM
           END-IF.

           PERFORM  3060-CHAMADA-MESU.
           PERFORM  3070-PESQUISA-RUECB096.

      *---------------------------------------------------------------*
       3000-99-FIM.                EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3060-CHAMADA-MESU           SECTION.
      *---------------------------------------------------------------*

           IF  RUEC62-PROCESS      EQUAL  'B'
               GO  TO  3060-50-MESU9116
           END-IF.

           INITIALIZE  WRK-MESU9032
                       WRK-POOL7100-MESU9032
                       WRK-SQLCA-MESU9032
                       WRK-AMBIENTE-MESU9032
                       WRK-IO-PCB-MESU9032
                       WRK-ALT-PCB-MESU9032.
           MOVE 'MESU9032'         TO  WRK-MODULO.
           MOVE CJUNC-DEPDC OF RUFIB002  TO WRK-AGENCIA.
           MOVE 'O'                TO  WRK-AMBIENTE-MESU9032.
           CALL WRK-MODULO  USING WRK-MESU9032
                                  WRK-POOL7100-MESU9032
                                  WRK-SQLCA-MESU9032
                                  WRK-AMBIENTE-MESU9032
                                  WRK-IO-PCB-MESU9032
                                  WRK-ALT-PCB-MESU9032.

           IF  WRK-COD-RETORNO  NOT  EQUAL  ZEROS
               MOVE 03                  TO RUEC62-COD-RET
               MOVE 'AGENCIA INEXISTENTE NO MESU9032'
                                        TO RUEC62-MENSAGEM
               PERFORM 4000-ROTINA-FIM
           END-IF.

           MOVE WRK-LOJA             TO CFLIAL-CNPJ-ENTID.
           MOVE WRK-CONTROLE         TO WRK-CTRL-CHAR.
           MOVE WRK-CTRL-NUM         TO CCTRL-CNPJ-ENTID.

           GO  TO  3060-99-FIM.

      *---------------------------------------------------------------*
       3060-50-MESU9116.
      *---------------------------------------------------------------*

           INITIALIZE  COMU-DADOS-SAIDA.

           MOVE 'MESU9116'               TO WRK-MODULO.
           MOVE 237                      TO COMU-COD-EMPR.
           MOVE CJUNC-DEPDC OF RUFIB002  TO COMU-COD-DEPDC.
           MOVE 'VRS001'                 TO COMU-VERSAO.
           MOVE SPACES                   TO COMU-MENSAGEM.

           CALL    WRK-MODULO     USING     COMU-VERSAO
                                            COMU-MENSAGEM
                                            COMU-DADOS-ENTRADA
                                            COMU-DADOS-SAIDA.

           IF  RETURN-CODE      NOT  EQUAL  ZEROS
               MOVE 03                  TO RUEC62-COD-RET
               MOVE 'AGENCIA INEXISTENTE NO MESU9116'
                                        TO RUEC62-MENSAGEM
               PERFORM 4000-ROTINA-FIM
           END-IF.

           MOVE COMU-NUM-LOJ-CGC     TO CFLIAL-CNPJ-ENTID.
           MOVE COMU-NUM-CONT-CGC    TO WRK-CTRL-CHAR.
           MOVE WRK-CTRL-NUM         TO CCTRL-CNPJ-ENTID.

      *---------------------------------------------------------------*
       3060-99-FIM.                EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3070-PESQUISA-RUECB096      SECTION.
      *---------------------------------------------------------------*

           MOVE RUEC62-CCONTR          TO CCONTR-CREDT-RURAL OF RUECB096

           EXEC SQL
               OPEN CSR01-RUECB096
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 99                  TO RUEC62-COD-RET
               MOVE 'ERRO ACESSO DB2'   TO RUEC62-MENSAGEM
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'THIST_SIT_CDULA  ' TO ERR-DBD-TAB
               MOVE 'OPEN  '            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0140'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 4000-ROTINA-FIM
           END-IF.


      *---------------------------------------------------------------*
      * SELECIONAR
      *---------------------------------------------------------------*
       3070-20-FETCH.

           EXEC SQL
                FETCH    CSR01-RUECB096
                  INTO    :RUECB096.CINDCD-SIT-CDULA
                               :WRK-CINDCD-SIT-CDULA-NULL
                        , :RUECB096.CREFT-RECOR
                        , :RUECB096.CMOTVO-EXCL-RECOR
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 99                  TO RUEC62-COD-RET
               MOVE 'ERRO ACESSO DB2'   TO RUEC62-MENSAGEM
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'THIST_SIT_CDULA  ' TO ERR-DBD-TAB
               MOVE 'FETCH '            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0150'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 4000-ROTINA-FIM
           END-IF.

           IF (SQLCODE                  EQUAL +100)
               MOVE '001'               TO CMOTVO-EXCL-RECOR
               GO  TO  3070-90-CLOSE
           END-IF.

           IF  WRK-CINDCD-SIT-CDULA-NULL  LESS  ZEROS
               MOVE SPACES              TO CINDCD-SIT-CDULA
           END-IF.

           IF  CINDCD-SIT-CDULA  NOT  EQUAL  'E'
               GO  TO  3070-20-FETCH
           END-IF.


      *---------------------------------------------------------------*
      * FECHAR CURSOR CSR01-RUECB096
      *---------------------------------------------------------------*
       3070-90-CLOSE.

           EXEC SQL
               CLOSE CSR01-RUECB096
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 99                  TO RUEC62-COD-RET
               MOVE 'ERRO ACESSO DB2'   TO RUEC62-MENSAGEM
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'THIST_SIT_CDULA  ' TO ERR-DBD-TAB
               MOVE 'CLOSE '            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0170'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 4000-ROTINA-FIM
           END-IF.

      *---------------------------------------------------------------*
       3070-99-FIM.                EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3100-GERA-CONTRATO          SECTION.
      *---------------------------------------------------------------*

       3100-10-INSERT-RUECB09B.

           MOVE ZEROS              TO NENVIO-RECOR       OF RUECB09B.
           MOVE RUEC62-CCONTR      TO CCONTR-CREDT-RURAL OF RUECB09B.
           EXEC SQL
             SELECT  MAX (NENVIO_RECOR)
               INTO :RUECB09B.NENVIO-RECOR
             FROM  DB2PRD.THIST_ENVIO_RECOR
               WHERE CCONTR_CREDT_RURAL = :RUECB09B.CCONTR-CREDT-RURAL
           END-EXEC.

           IF (SQLCODE           NOT EQUAL ZEROS AND -305 AND -310) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 99                  TO RUEC62-COD-RET
               MOVE 'ERRO ACESSO DB2'   TO RUEC62-MENSAGEM
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'THIST_ENVIO_RECOR' TO ERR-DBD-TAB
               MOVE 'SELECT'            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0080'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 4000-ROTINA-FIM
           END-IF.

           ADD  1                  TO NENVIO-RECOR       OF RUECB09B.
           MOVE 'COR0002'          TO CMSGEM-RECOR       OF RUECB09B.
           MOVE SPACES             TO CCTRL-RQUIS-FINCR  OF RUECB09B.
           MOVE SPACES             TO CUNIC-MSGEM        OF RUECB09B.
           MOVE CREFT-RECOR OF RUFIB002
                                   TO CREFT-RECOR        OF RUECB09B.
           MOVE 1                  TO CSIT-MSGEM         OF RUECB09B.
           IF  RUEC62-USER  EQUAL  SPACES
               MOVE '999999999'    TO CUSUAR-RESP        OF RUECB09B
           ELSE
               MOVE RUEC62-USER    TO CUSUAR-RESP        OF RUECB09B
           END-IF.

           MOVE NENVIO-RECOR OF RUECB09B  TO RUEC62-NENVIO-RECOR.

           EXEC SQL
             INSERT INTO DB2PRD.THIST_ENVIO_RECOR
                   (CCONTR_CREDT_RURAL
                  , NENVIO_RECOR
                  , HULT_ATULZ
                  , CMSGEM_RECOR
                  , HINCL
                  , CCTRL_RQUIS_FINCR
                  , CUNIC_MSGEM
                  , CREFT_RECOR
                  , CSIT_MSGEM
                  , CUSUAR_RESP)
             VALUES
                   (:RUECB09B.CCONTR-CREDT-RURAL
                  , :RUECB09B.NENVIO-RECOR
                  , CURRENT TIMESTAMP
                  , :RUECB09B.CMSGEM-RECOR
                  , CURRENT TIMESTAMP
                  , :RUECB09B.CCTRL-RQUIS-FINCR
                  , :RUECB09B.CUNIC-MSGEM
                  , :RUECB09B.CREFT-RECOR
                  , :RUECB09B.CSIT-MSGEM
                  , :RUECB09B.CUSUAR-RESP)
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 99                  TO RUEC62-COD-RET
               MOVE 'ERRO ACESSO DB2'   TO RUEC62-MENSAGEM
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'THIST_ENVIO_RECOR ' TO ERR-DBD-TAB
               MOVE 'INSERT'            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0090'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 4000-ROTINA-FIM
           END-IF.

      *---------------------------------------------------------------*
      * INSERIR REGISTRO RUECB099
      *---------------------------------------------------------------*
       3100-20-INSERT-RUECB099.

           MOVE NENVIO-RECOR OF RUECB09B TO NENVIO-RECOR   OF RUECB099.
           MOVE RUEC62-CCONTR        TO CCONTR-CREDT-RURAL OF RUECB099.
           MOVE 60746948             TO CCNPJ-ENTID-RESP.
           MOVE ZEROS                TO CCNPJ-PARTC
           MOVE ZEROS                TO CFLIAL-CNPJ-PARTC
           MOVE ZEROS                TO CCTRL-CNPJ-PARTC
           MOVE CMOTVO-EXCL-RECOR(2:2) TO CSIT-OPER-RURAL
           MOVE SPACES               TO CCDULA-RECOR
                                        CTPO-INSTT-CREDT
                                        CTPO-FONTE-RECOR
                                        CEMPTO-BACEN
                                        CSIST-PROD
                                        CTSORO-NACIO
           MOVE ZEROS                TO PALIQT-PROAGRO
                                        PJURO-ENCAR-FINCR
                                        CTPO-ENCAR-COMPL
                                        PRISCO-TSORO-NACIO
                                        PRISCO-FUNDO-CNSTT
                                        QAREA-RECOR
                                        VRECTA-BRUTA-RECOR
           MOVE 1                    TO CUND-MEDD-FINCD OF RUECB099
                                        CUND-MEDD-PROVV OF RUECB099.

           EXEC SQL
             INSERT INTO DB2PRD.TCONTR_ENVIO_RECOR
                   (CCONTR_CREDT_RURAL
                  , NENVIO_RECOR
                  , CCNPJ_ENTID_RESP
                  , CFLIAL_CNPJ_ENTID
                  , CCTRL_CNPJ_ENTID
                  , CCNPJ_PARTC
                  , CFLIAL_CNPJ_PARTC
                  , CCTRL_CNPJ_PARTC
                  , CSIT_OPER_RURAL
                  , CCDULA_RECOR
                  , CTPO_INSTT_CREDT
                  , CTPO_FONTE_RECOR
                  , CEMPTO_BACEN
                  , CSIST_PROD
                  , PALIQT_PROAGRO
                  , PJURO_ENCAR_FINCR
                  , CTPO_ENCAR_COMPL
                  , CTSORO_NACIO
                  , PRISCO_TSORO_NACIO
                  , PRISCO_FUNDO_CNSTT
                  , QAREA_RECOR
                  , CUND_MEDD_FINCD
                  , CUND_MEDD_PROVV
                  , VRECTA_BRUTA_RECOR
                  , DMOVTO_RECOR)
             VALUES
                   (:RUECB099.CCONTR-CREDT-RURAL
                  , :RUECB099.NENVIO-RECOR
                  , :RUECB099.CCNPJ-ENTID-RESP
                  , :RUECB099.CFLIAL-CNPJ-ENTID
                  , :RUECB099.CCTRL-CNPJ-ENTID
                  , :RUECB099.CCNPJ-PARTC
                  , :RUECB099.CFLIAL-CNPJ-PARTC
                  , :RUECB099.CCTRL-CNPJ-PARTC
                  , :RUECB099.CSIT-OPER-RURAL
                  , :RUECB099.CCDULA-RECOR
                  , :RUECB099.CTPO-INSTT-CREDT
                  , :RUECB099.CTPO-FONTE-RECOR
                  , :RUECB099.CEMPTO-BACEN
                  , :RUECB099.CSIST-PROD
                  , :RUECB099.PALIQT-PROAGRO
                  , :RUECB099.PJURO-ENCAR-FINCR
                  , :RUECB099.CTPO-ENCAR-COMPL
                  , :RUECB099.CTSORO-NACIO
                  , :RUECB099.PRISCO-TSORO-NACIO
                  , :RUECB099.PRISCO-FUNDO-CNSTT
                  , :RUECB099.QAREA-RECOR
                  , :RUECB099.CUND-MEDD-FINCD
                  , :RUECB099.CUND-MEDD-PROVV
                  , :RUECB099.VRECTA-BRUTA-RECOR
                  , CURRENT DATE)
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 99                  TO RUEC62-COD-RET
               MOVE 'ERRO ACESSO DB2'   TO RUEC62-MENSAGEM
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'TCONTR_ENVIO_RECOR' TO ERR-DBD-TAB
               MOVE 'INSERT'            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0100'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 4000-ROTINA-FIM
           END-IF.

      *---------------------------------------------------------------*
       3100-99-FIM.                EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3900-ATUALIZA-CONTRATO      SECTION.
      *---------------------------------------------------------------*

           MOVE  ZEROS                 TO CREFT-BACEN OF RUFIB002

           EXEC SQL
               UPDATE DB2PRD.CONTRATO_RURAL
                  SET CREFT_BACEN        = :RUFIB002.CREFT-BACEN
               WHERE  CCONTR             = :RUFIB002.CCONTR
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 99                  TO RUEC62-COD-RET
               MOVE 'ERRO ACESSO DB2'   TO RUEC62-MENSAGEM
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'CONTRATO_RURAL   ' TO ERR-DBD-TAB
               MOVE 'UPDATE'            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0280'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 4000-ROTINA-FIM
           END-IF.

      *---------------------------------------------------------------*
       3900-99-FIM.                EXIT.
      *---------------------------------------------------------------**

      *---------------------------------------------------------------*
       4000-ROTINA-FIM             SECTION.
      *---------------------------------------------------------------*

           IF  RUEC62-COD-RET      EQUAL 99
               MOVE 'RUEC8820'     TO ERR-MODULO
               MOVE ERRO-AREA      TO RUEC62-ERRO-AREA
               MOVE SQLCA(1:136)   TO RUEC62-SQLCA
           END-IF.

           GOBACK.

      *---------------------------------------------------------------*
       4000-99-FIM.                EXIT.
      *---------------------------------------------------------------**
