      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT0464.
       AUTHOR.     LEANDRO MENDES.
      *================================================================*
      *                   C P M  -  S I S T E M A S                    *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT0464                                    *
      *    PROGRAMADOR.:   LEANDRO MENDES          - CPM PATO BRANCO   *
      *    ANALISTA CPM:   FELIPE SOMMER           - CPM PATO BRANCO   *
      *    ANALISTA....:   RENATO TAMANAHA         - PROCKWORK GP/50   *
      *    DATA........:   24/02/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   UNIFICACAO DE DEBITO PARA CPF/CNPJ -        *
      *        CANCELAMENTO.                                           *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                         INCLUDE/BOOK    *
      *                    DB2PRD.TDEB_UNFCA_CLI         GFCTB0H8      *
      *                    DB2PRD.PARM_DATA_PROCM        GFCTB0A1      *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#GFCTIS - AREA DE COMUNICACAO - ENTRADA                    *
      *    I#GFCTIT - AREA DE COMUNICACAO - SAIDA                      *
      *    I#GFCTG2 - AREA DE COMUNICACAO - ENTRADA(DESC. DE MENSAGEM) *
      *    I#GFCTG3 - AREA DE COMUNICACAO - SAIDA  (DESC. DE MENSAGEM) *
      *    I#GFCT0M - AREA DE COMUNICACAO - ERROS                      *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    GFCT5523 - OBTER DESCRICAO DE MENSAGENS                     *
      *================================================================*

      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** GFCT0464 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREAS AUXILIARES ***'.
      *----------------------------------------------------------------*

       77  IND-1                       PIC  9(003) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREAS AUXILIARES ***'.
      *----------------------------------------------------------------*

       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-FINALIZAR               PIC  X(001)         VALUE SPACES.
       77  WRK-CONT-UNIFICACAO         PIC S9(004) COMP    VALUE ZEROS.
       77  WRK-RESULTADO               PIC  9(003)         VALUE ZEROS.

       01  WRK-TIMESTAMP               PIC  X(026)         VALUE SPACES.

       01  WRK-CFUNC-BDSCO             PIC  9(009)         VALUE ZEROS.
       01  FILER                       REDEFINES WRK-CFUNC-BDSCO.
           03  FILLER                  PIC  9(002).
           03  WRK-CFUNC-BDSCO-ALFA    PIC  X(007).

       01  WRK-MSG-ERRO.
           03  FILLER                  PIC  X(026)         VALUE
               'ERRO NA CHAMADA DO MODULO'.
           03  WRK-MODULO-MSG          PIC  X(008)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREAS PARA DATA E HORA ***'.
      *----------------------------------------------------------------*

       01  WRK-TIMESTAMP-AUX.
           03  WRK-ANO                 PIC 9(004)          VALUE ZEROS.
           03  FILLER                  PIC X(001)          VALUE '-'.
           03  WRK-MES                 PIC 9(002)          VALUE ZEROS.
           03  FILLER                  PIC X(001)          VALUE '-'.
           03  WRK-DIA                 PIC 9(002)          VALUE ZEROS.
           03  FILLER                  PIC X(001)          VALUE '-'.
           03  WRK-HOR                 PIC 9(002)          VALUE ZEROS.
           03  FILLER                  PIC X(001)          VALUE '.'.
           03  WRK-MIN                 PIC 9(002)          VALUE ZEROS.
           03  FILLER                  PIC X(001)          VALUE '.'.
           03  WRK-SEG                 PIC 9(002)          VALUE ZEROS.
           03  FILLER                  PIC X(001)          VALUE '.'.
           03  WRK-MIL                 PIC 9(006)          VALUE ZEROS.

       01  WRK-DATA-AUX.
           03  WRK-DIA-AUX             PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  WRK-MES-AUX             PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  WRK-ANO-AUX             PIC  9(004)         VALUE ZEROS.

       01  WRK-DATA-AUX1.
           03  WRK-ANO-AUX1            PIC  9(004)         VALUE ZEROS.
           03  WRK-MES-AUX1            PIC  9(002)         VALUE ZEROS.
           03  WRK-DIA-AUX1            PIC  9(002)         VALUE ZEROS.

       01  WRK-DATA-AUX2.
           03  WRK-ANO-AUX2            PIC  9(004)         VALUE ZEROS.
           03  WRK-MES-AUX2            PIC  9(002)         VALUE ZEROS.
           03  WRK-DIA-AUX2            PIC  9(002)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA INC ***'.
      *----------------------------------------------------------------*

       01  WRK-5000-AREA.
           05  WRK-5000-AREA-ENVIADA.
            10 WRK-5000-AMBIENTE       PIC  X(001)         VALUE ' '.
            10 WRK-5000-COD-MSG        PIC  9(004)         VALUE ZEROS.
           05  WRK-5000-AREA-RECEBE.
            10 WRK-5000-COD-RETORNO    PIC  9(002)         VALUE ZEROS.
            10 WRK-5000-COD-SQL-ERRO   PIC  9(003)         VALUE ZEROS.
            10 WRK-5000-DESC-MSG       PIC  X(070)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA INC ***'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTG2'.

       COPY 'I#GFCTG3'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE DB2 ***'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0H8
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0A1
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** GFCT0464 - FIM DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY 'I#GFCTIS'.

       COPY 'I#GFCTIT'.

       COPY 'I#GFCT0M'.

      *================================================================*
       PROCEDURE DIVISION              USING GFCTIS-ENTRADA
                                             GFCTIT-SAIDA
                                             GFCT0M-AREA-ERROS.
      *================================================================*

      *----------------------------------------------------------------*
       000000-INICIAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 100000-INICIALIZAR.

           PERFORM 200000-PROCESSAR.

           PERFORM 300000-FINALIZAR.

      *----------------------------------------------------------------*
       000000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       100000-INICIALIZAR              SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCT0M-AREA-ERROS
                                          GFCTIT-SAIDA.

           INITIALIZE GFCT0M-AREA-ERROS
                      GFCTIT-SAIDA.

           PERFORM 110000-MONTAR-AREA-SAIDA.

           PERFORM 120000-CONSISTIR-CAMPOS.

           PERFORM 130000-OBTER-DATA-PROC.

      *----------------------------------------------------------------*
       100000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       110000-MONTAR-AREA-SAIDA        SECTION.
      *----------------------------------------------------------------*

           MOVE +362                   TO GFCTIT-LL.
           MOVE GFCTIS-TRANSACAO       TO GFCTIT-TRANSACAO.
           MOVE GFCTIS-FUNCAO          TO GFCTIT-FUNCAO.
           MOVE GFCTIS-FILTRO          TO GFCTIT-FILTRO.
           MOVE GFCTIS-NOME            TO GFCTIT-NOME.

           IF  GFCTIS-QTDE-OCOR        NOT NUMERIC OR
               GFCTIS-QTDE-OCOR        EQUAL ZEROS
               MOVE ZEROS              TO GFCTIT-QTDE-OCOR
           ELSE
               MOVE GFCTIS-QTDE-OCOR   TO GFCTIT-QTDE-OCOR
           END-IF.

           MOVE ZEROS                  TO GFCTIT-QTDE-TOT-REG.
           MOVE GFCTIS-FUNC-BDSCO      TO GFCTIT-FUNC-BDSCO.
           MOVE GFCTIS-PONTEIRO        TO GFCTIT-PONTEIRO.

           IF  GFCTIS-DPTO             NOT NUMERIC OR
               GFCTIS-DPTO             EQUAL ZEROS
               MOVE ZEROS              TO GFCTIT-DPTO
           ELSE
               MOVE GFCTIS-DPTO        TO GFCTIT-DPTO
           END-IF.

           IF  GFCTIS-MASTER           EQUAL SPACES OR
               GFCTIS-MASTER           EQUAL LOW-VALUES
               MOVE SPACES             TO GFCTIT-MASTER
           ELSE
               MOVE GFCTIS-MASTER      TO GFCTIT-MASTER
           END-IF.

           MOVE GFCTIS-TERMINAL        TO GFCTIT-TERMINAL.
           MOVE GFCTIS-ACAO            TO GFCTIT-ACAO.

           MOVE ZEROS                  TO GFCTIT-ERRO
                                          GFCTIT-COD-SQL-ERRO
                                          GFCTIT-COD-MSG-ERRO.
           MOVE '0000'                 TO GFCTG2-COD-MSG.

           PERFORM 111000-OBTER-DESCRICAO-MSG.

           MOVE GFCTG3-DESC-MSG        TO GFCTIT-DESC-MSG-ERRO.
           MOVE 'S'                    TO GFCTIT-FIM.

      *----------------------------------------------------------------*
       110000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       111000-OBTER-DESCRICAO-MSG      SECTION.
      *----------------------------------------------------------------*

           MOVE +100                       TO GFCTG2-LL.
           MOVE ZEROS                      TO GFCTG2-ZZ.
           MOVE GFCTIS-TRANSACAO           TO GFCTG2-TRANSACAO.
           MOVE GFCTIS-FUNCAO              TO GFCTG2-FUNCAO.
           MOVE GFCTIS-FUNC-BDSCO          TO GFCTG2-FUNC-BDSCO.
           MOVE 'N'                        TO GFCTG2-FIM.
           MOVE 'O'                        TO GFCTG2-TIPO-PROC.
           MOVE 'GFCT5523'                 TO WRK-MODULO.

           CALL WRK-MODULO                 USING GFCTG2-ENTRADA
                                                 GFCTG3-SAIDA
                                                 GFCT0M-AREA-ERROS.

           IF  RETURN-CODE                 NOT EQUAL ZEROS
               MOVE 9                      TO GFCTIT-ERRO
               MOVE WRK-MODULO             TO WRK-MODULO-MSG
               MOVE WRK-MSG-ERRO           TO GFCT0M-TEXTO
               MOVE WRK-MODULO             TO GFCT0M-PROGRAMA
               MOVE 'APL'                  TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0464'             TO GFCT0M-TRANSACAO
               PERFORM 300000-FINALIZAR
           ELSE
               IF  GFCTG3-ERRO             EQUAL 9
                   MOVE 9                  TO GFCTIT-ERRO
                   IF  GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2'
                       MOVE WRK-MODULO     TO GFCT0M-PROGRAMA
                       MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
                       MOVE 'GFCT0464'     TO GFCT0M-TRANSACAO
                   END-IF
                   PERFORM 300000-FINALIZAR
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       111000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       120000-CONSISTIR-CAMPOS         SECTION.
      *---------------------------------------------------------------*

           IF (GFCTIS-TRANSACAO        EQUAL SPACES OR
                                       LOW-VALUES)  OR
              (GFCTIS-FUNCAO           EQUAL SPACES OR
                                       LOW-VALUES)  OR
              (GFCTIS-FUNC-BDSCO       NOT NUMERIC  OR
               GFCTIS-FUNC-BDSCO       EQUAL ZEROS) OR
              (GFCTIS-CPF-CNPJ         NOT NUMERIC) OR
              (GFCTIS-FILIAL           NOT NUMERIC) OR
              (GFCTIS-CTRL             NOT NUMERIC) OR
              (GFCTIS-QTDE-OCOR        NOT NUMERIC  OR
               GFCTIS-QTDE-OCOR        EQUAL ZEROS) OR
              (GFCTIS-QTDE-TOT-REG     NOT NUMERIC  OR
               GFCTIS-QTDE-TOT-REG     EQUAL ZEROS) OR
              (GFCTIS-CPF-CNPJ-P       NOT NUMERIC) OR
              (GFCTIS-FILIAL-P         NOT NUMERIC) OR
              (GFCTIS-TARIFA-P         NOT NUMERIC)
               MOVE 1                  TO GFCTIT-ERRO
               MOVE 0001               TO GFCTIT-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               MOVE ZEROS              TO GFCTIT-COD-SQL-ERRO
               PERFORM 111000-OBTER-DESCRICAO-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTIT-DESC-MSG-ERRO

               PERFORM 121000-MONTAR-OCORRENCIA-SAIDA

               PERFORM 300000-FINALIZAR
           END-IF.

      *---------------------------------------------------------------*
       120000-99-FIM.                  EXIT.
      *---------------------------------------------------------------*

      *----------------------------------------------------------------*
       121000-MONTAR-OCORRENCIA-SAIDA  SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTIS-QTDE-TOT-REG    TO GFCTIT-QTDE-TOT-REG.

           PERFORM
           VARYING IND-1               FROM 1 BY 1
             UNTIL IND-1               GREATER GFCTIS-QTDE-OCOR
                   ADD +108            TO GFCTIT-LL

                   MOVE GFCTIS-TARIFA(IND-1)
                                       TO GFCTIT-TARIFA(IND-1)
                   MOVE GFCTIS-DESC-TARIFA(IND-1)
                                       TO GFCTIT-DESC-TARIFA(IND-1)
                   MOVE GFCTIS-SIT(IND-1)
                                       TO GFCTIT-SIT(IND-1)
                   MOVE GFCTIS-DT-INI(IND-1)
                                       TO GFCTIT-DT-INI(IND-1)
                   MOVE GFCTIS-DT-FIM(IND-1)
                                       TO GFCTIT-DT-FIM(IND-1)
                   MOVE GFCTIS-DT-INI-TIMESTAMP(IND-1)
                                       TO
                                         GFCTIT-DT-INI-TIMESTAMP(IND-1)
                   MOVE GFCTIS-DT-FIM-TIMESTAMP(IND-1)
                                       TO
                                         GFCTIT-DT-FIM-TIMESTAMP(IND-1)

           END-PERFORM.

      *----------------------------------------------------------------*
       121000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       130000-OBTER-DATA-PROC          SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC      OF GFCTB0A1.

           EXEC SQL
             SELECT
                   DPROCM_ATUAL
             INTO
                   :GFCTB0A1.DPROCM-ATUAL
             FROM   DB2PRD.PARM_DATA_PROCM
             WHERE
                   CSIST_PRINC         = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS ) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'GFCT0464'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE 'PARM_DATA_PROCM'  TO GFCT0M-NOME-TAB
               MOVE SQLCODE            TO GFCT0M-SQLCODE
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE '0010'             TO GFCT0M-LOCAL
               MOVE 9                  TO GFCTIT-ERRO
               MOVE SQLCODE            TO GFCTIT-COD-SQL-ERRO
               MOVE 0012               TO GFCTIT-COD-MSG-ERRO
               MOVE '0012'             TO GFCTG2-COD-MSG
               PERFORM 111000-OBTER-DESCRICAO-MSG
               STRING GFCTG3-DESC-MSG ' - GFCTB0A1'
               DELIMITED BY '  '       INTO GFCTIT-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO WRK-DATA-AUX.
           MOVE WRK-DIA-AUX            TO WRK-DIA-AUX1.
           MOVE WRK-MES-AUX            TO WRK-MES-AUX1.
           MOVE WRK-ANO-AUX            TO WRK-ANO-AUX1.

      *----------------------------------------------------------------*
       130000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       200000-PROCESSAR                SECTION.
      *----------------------------------------------------------------*

ST25X6*    MOVE GFCTIS-CPF-CNPJ        TO CCGC-CPF       OF GFCTB0H8.
ST25X6*    MOVE GFCTIS-FILIAL          TO CFLIAL-CGC     OF GFCTB0H8.
ST25X6*    MOVE GFCTIS-CTRL            TO CCTRL-CPF-CGC  OF GFCTB0H8.
ST25X6     MOVE GFCTIS-CPF-CNPJ        TO CCGC-CPF-ST    OF GFCTB0H8.
ST25X6     MOVE GFCTIS-FILIAL          TO CFLIAL-CGC-ST  OF GFCTB0H8.
ST25X6     MOVE GFCTIS-CTRL            TO CCTRL-CPF-CGC-ST OF GFCTB0H8.
           MOVE '0001-01-01-01.01.01.000001'
                                       TO HEXCL-DEB-UNIC OF GFCTB0H8.

           EXEC SQL
             SELECT COUNT(*)
             INTO
                   :WRK-CONT-UNIFICACAO
             FROM   DB2PRD.TDEB_UNFCA_CLI
             WHERE
ST25X6*               CCGC_CPF           = :GFCTB0H8.CCGC-CPF         AND
ST25X6*               CFLIAL_CGC         = :GFCTB0H8.CFLIAL-CGC       AND
ST25X6*               CCTRL_CPF_CGC      = :GFCTB0H8.CCTRL-CPF-CGC    AND
ST25X6                CCGC_CPF_ST        = :GFCTB0H8.CCGC-CPF-ST    AND
ST25X6                CFLIAL_CGC_ST      = :GFCTB0H8.CFLIAL-CGC-ST  AND
ST25X6                CCTRL_CPF_CGC_ST = :GFCTB0H8.CCTRL-CPF-CGC-ST AND
                   HEXCL_DEB_UNIC     = :GFCTB0H8.HEXCL-DEB-UNIC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS ) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'GFCT0464'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE 'TDEB_UNFCA_CLI'   TO GFCT0M-NOME-TAB
               MOVE SQLCODE            TO GFCT0M-SQLCODE
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE '0020'             TO GFCT0M-LOCAL
               MOVE 9                  TO GFCTIT-ERRO
               MOVE SQLCODE            TO GFCTIT-COD-SQL-ERRO
               MOVE 0012               TO GFCTIT-COD-MSG-ERRO
               MOVE '0012'             TO GFCTG2-COD-MSG
               PERFORM 111000-OBTER-DESCRICAO-MSG
               STRING GFCTG3-DESC-MSG ' - GFCTB0H8'
               DELIMITED BY '  '       INTO GFCTIT-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

           COMPUTE WRK-RESULTADO       = (WRK-CONT-UNIFICACAO
                                          - GFCTIS-QTDE-OCOR).

           IF  WRK-RESULTADO           EQUAL 1
               MOVE 1                  TO GFCTIT-ERRO
               MOVE ZEROS              TO GFCTIT-COD-SQL-ERRO
               MOVE 0600               TO GFCTIT-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               PERFORM 111000-OBTER-DESCRICAO-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTIT-DESC-MSG-ERRO

               PERFORM 121000-MONTAR-OCORRENCIA-SAIDA

               PERFORM 300000-FINALIZAR
           END-IF.

           MOVE 1                      TO IND-1.

           PERFORM 210000-PROCESSAR-CANCELAR   UNTIL
               IND-1                   GREATER GFCTIS-QTDE-OCOR.

      *----------------------------------------------------------------*
       200000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       210000-PROCESSAR-CANCELAR       SECTION.
      *----------------------------------------------------------------*

ST25X6*    MOVE GFCTIS-CPF-CNPJ        TO CCGC-CPF         OF GFCTB0H8.
ST25X6*    MOVE GFCTIS-FILIAL          TO CFLIAL-CGC       OF GFCTB0H8.
ST25X6*    MOVE GFCTIS-CTRL            TO CCTRL-CPF-CGC    OF GFCTB0H8.
ST25X6     MOVE GFCTIS-CPF-CNPJ        TO CCGC-CPF-ST      OF GFCTB0H8.
ST25X6     MOVE GFCTIS-FILIAL          TO CFLIAL-CGC-ST    OF GFCTB0H8.
ST25X6     MOVE GFCTIS-CTRL            TO CCTRL-CPF-CGC-ST OF GFCTB0H8.
           MOVE GFCTIS-TARIFA(IND-1)   TO CSERVC-TARIF     OF GFCTB0H8.
           MOVE GFCTIS-DT-INI-TIMESTAMP(IND-1)
                                       TO HINCL-DEB-UNIC   OF GFCTB0H8
                                          WRK-TIMESTAMP-AUX.
           MOVE GFCTIS-FUNC-BDSCO      TO WRK-CFUNC-BDSCO-ALFA.
           MOVE WRK-CFUNC-BDSCO        TO CFUNC-EXCL-SIST  OF GFCTB0H8.

           MOVE WRK-DIA                TO WRK-DIA-AUX2.
           MOVE WRK-MES                TO WRK-MES-AUX2.
           MOVE WRK-ANO                TO WRK-ANO-AUX2.

           IF  WRK-DATA-AUX2           GREATER WRK-DATA-AUX1
               PERFORM 211000-EFETUAR-DELETE
           ELSE
               PERFORM 213000-OBTER-DATA-SISTEMA

               MOVE WRK-TIMESTAMP      TO HEXCL-DEB-UNIC OF GFCTB0H8

               PERFORM 212000-EFETUAR-UPDATE
           END-IF.

           ADD 1                       TO IND-1.

      *----------------------------------------------------------------*
       210000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       211000-EFETUAR-DELETE           SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             DELETE FROM    DB2PRD.TDEB_UNFCA_CLI
             WHERE
ST25X6*           CCGC_CPF           = :GFCTB0H8.CCGC-CPF         AND
ST25X6*           CFLIAL_CGC         = :GFCTB0H8.CFLIAL-CGC       AND
ST25X6*           CCTRL_CPF_CGC      = :GFCTB0H8.CCTRL-CPF-CGC    AND
ST25X6            CCGC_CPF_ST        = :GFCTB0H8.CCGC-CPF-ST      AND
ST25X6            CFLIAL_CGC_ST      = :GFCTB0H8.CFLIAL-CGC-ST    AND
ST25X6            CCTRL_CPF_CGC_ST   = :GFCTB0H8.CCTRL-CPF-CGC-ST AND
               CSERVC_TARIF       = :GFCTB0H8.CSERVC-TARIF     AND
               HINCL_DEB_UNIC     = :GFCTB0H8.HINCL-DEB-UNIC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS ) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'GFCT0464'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'DELETE'           TO GFCT0M-COMANDO-SQL
               MOVE 'TDEB_UNFCA_CLI'   TO GFCT0M-NOME-TAB
               MOVE SQLCODE            TO GFCT0M-SQLCODE
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE '0030'             TO GFCT0M-LOCAL
               MOVE 9                  TO GFCTIT-ERRO
               MOVE SQLCODE            TO GFCTIT-COD-SQL-ERRO
               MOVE 0020               TO GFCTIT-COD-MSG-ERRO
               MOVE '0020'             TO GFCTG2-COD-MSG
               PERFORM 111000-OBTER-DESCRICAO-MSG
               STRING GFCTG3-DESC-MSG ' - GFCTB0H8'
               DELIMITED BY '  '       INTO GFCTIT-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       211000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       212000-EFETUAR-UPDATE           SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             UPDATE   DB2PRD.TDEB_UNFCA_CLI
               SET
                   HEXCL_DEB_UNIC     = :GFCTB0H8.HEXCL-DEB-UNIC,
                   CFUNC_EXCL_SIST    = :GFCTB0H8.CFUNC-EXCL-SIST
             WHERE
ST25X6*               CCGC_CPF           = :GFCTB0H8.CCGC-CPF         AND
ST25X6*               CFLIAL_CGC         = :GFCTB0H8.CFLIAL-CGC       AND
ST25X6*               CCTRL_CPF_CGC      = :GFCTB0H8.CCTRL-CPF-CGC    AND
ST25X6                CCGC_CPF_ST       = :GFCTB0H8.CCGC-CPF-ST    AND
ST25X6                CFLIAL_CGC_ST     = :GFCTB0H8.CFLIAL-CGC-ST  AND
ST25X6                CCTRL_CPF_CGC_ST = :GFCTB0H8.CCTRL-CPF-CGC-ST AND
                   CSERVC_TARIF       = :GFCTB0H8.CSERVC-TARIF     AND
                   HINCL_DEB_UNIC     = :GFCTB0H8.HINCL-DEB-UNIC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS ) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'GFCT0464'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'UPDATE'           TO GFCT0M-COMANDO-SQL
               MOVE 'TDEB_UNFCA_CLI'   TO GFCT0M-NOME-TAB
               MOVE SQLCODE            TO GFCT0M-SQLCODE
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE '0040'             TO GFCT0M-LOCAL
               MOVE 9                  TO GFCTIT-ERRO
               MOVE SQLCODE            TO GFCTIT-COD-SQL-ERRO
               MOVE 0019               TO GFCTIT-COD-MSG-ERRO
               MOVE '0019'             TO GFCTG2-COD-MSG
               PERFORM 111000-OBTER-DESCRICAO-MSG
               STRING GFCTG3-DESC-MSG ' - GFCTB0H8'
               DELIMITED BY '  '       INTO GFCTIT-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       212000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       213000-OBTER-DATA-SISTEMA       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               SET :WRK-TIMESTAMP      = CURRENT TIMESTAMP
           END-EXEC.

           MOVE DPROCM-ATUAL OF GFCTB0A1(7:4)  TO WRK-TIMESTAMP(1:4).
           MOVE DPROCM-ATUAL OF GFCTB0A1(4:2)  TO WRK-TIMESTAMP(6:2).
           MOVE DPROCM-ATUAL OF GFCTB0A1(1:2)  TO WRK-TIMESTAMP(9:2).

      *----------------------------------------------------------------*
       213000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       300000-FINALIZAR                SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       300000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
