      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT5241.
       AUTHOR. EVANISE NUNES.
      *================================================================*
      *                   C P M   S I S T E M A S                      *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : GFCT5241                                     *
      *    ANALISTA     : EVANISE NUNES   - PROCWORK  - GRUPO 50       *
      *    DATA         : 10/05/2005                                   *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      GERAR DADOS PARA COMBO DE SEGMENTOS GFCT                  *
      *                                                                *
      *    BANCO DE DADOS:                                             *
      *     DB2                                                        *
      *      TABLE                               INCLUDE/BOOK          *
      *      DB2PRD.TTPO_SGMTO_GSTAO              GFCTB0H5             *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      GFCT5000 - OBTER DESCRICAO DE MENSAGENS                   *
      *      GFCT5507 - VERIFICA MASTER                                *
      *      POOL0081 - ALOCAR DINAMICAMENTE UM MODULO                 *
      *      SENH0315 - OBTER JUNCAO                                   *
      *                                                                *
      *    CHAMADO POR: GFCT7740                                       *
      *                                                                *
      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
        CONFIGURATION                  SECTION.
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
       77  FILLER                      PIC X(32)           VALUE
           '*  INICIO DA WORKING GFCT5241  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC X(32)           VALUE
           '*         INDEXADORES          *'.
      *----------------------------------------------------------------*

       77  IND-OCOR                    PIC 9(03)           VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC X(32)           VALUE
           '*     VARIAVEIS AUXILIARES     *'.
      *----------------------------------------------------------------*

       77  WRK-MODULO                  PIC X(08)           VALUE SPACES.
       77  WRK-FINALIZAR               PIC X(01)           VALUE 'N'.
       77  WRK-NOME-TAB                PIC X(11)           VALUE SPACES.

       01  WRK-MSG001.
           05 FILLER                   PIC  X(026)         VALUE
              'ERRO NA CHAMADA AO MODULO '.
           05 WRK-MODULO-MSG           PIC  X(008)         VALUE SPACES.

      *----------------------------------------------------------------*
       77  FILLER                      PIC X(32)           VALUE
           '*   INDICADORES DE NULIDADE    *'.
      *----------------------------------------------------------------*

       01  WRK-GFCTB0H5-NULL.
           03 WRK-DCADTO-NULL          PIC S9(04) COMP     VALUE +0.
           03 WRK-DULT-ALT-NULL        PIC S9(04) COMP     VALUE +0.

      *----------------------------------------------------------------*
      *01  FILLER                      PIC X(32)           VALUE
      *    '* AREA DE COMUNICACAO GFCT5000 *'.
      *----------------------------------------------------------------*

      *01  WRK-5000-AREA.
      *    03  WRK-5000-ENVIA.
      *        05  WRK-5000-AMBIENTE   PIC X(001)          VALUE SPACES.
      *        05  GFCTG2-COD-MSG    PIC 9(004)          VALUE ZEROS.
      *    03  WRK-5000-RECEBE.
      *        05  WRK-5000-COD-RETORNO
      *                                PIC 9(002)          VALUE ZEROS.
      *        05  WRK-5000-COD-SQL-ERRO
      *                                PIC 9(003)          VALUE ZEROS.
      *        05  GFCTG3-DESC-MSG   PIC X(070)          VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5523 - ENTRADA *'.
      *----------------------------------------------------------------*

       COPY I#GFCTG2.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5523 - SAIDA *'.
      *----------------------------------------------------------------*

       COPY I#GFCTG3.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    AREA P/ MODULO GFCT5507   *'.
      *----------------------------------------------------------------*

       01  WRK-5507-AREA.
           03  WRK-5507-AREA-ENVIA.
               05  WRK-5507-AMBIENTE   PIC X(001)          VALUE SPACES.
               05  WRK-5507-CJUNC-DEPDC
                                       PIC 9(005)          VALUE ZEROS.
           03  WRK-5507-RECEBE.
               05  WRK-5507-COD-RETORNO
                                       PIC 9(002)          VALUE ZEROS.
               05  WRK-5507-COD-SQL-ERRO
                                       PIC 9(003)          VALUE ZEROS.
               05  WRK-5507-MSG-RETORNO
                                       PIC X(045)          VALUE SPACES.
               05  WRK-5507-IDTFD-MASTER
                                       PIC X(001)          VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    AREA P/ MODULO SENH0315   *'.
      *----------------------------------------------------------------*

       01  WRK-VERSAO-0315             PIC  X(006)         VALUE
           'VRS001'.
       01  WRK-MENSAGEM-0315.
           03  WRK-COD-MENSAGEM-0315   PIC  9(003)         VALUE ZEROS.
           03  WRK-HIFEN-MENSAGEM-0315 PIC  X(001)         VALUE SPACES.
           03  WRK-TEXTO-MENSAGEM-0315 PIC  X(079)         VALUE SPACES.
       01  WRK-COD-FUNCIONAL-0315      PIC  X(007)         VALUE SPACES.
       01  WRK-NOME-FUNC-0315          PIC  X(040)         VALUE SPACES.
       01  WRK-COD-BANCO-0315          PIC  9(005)         VALUE ZEROS.
       01  WRK-NOME-BANCO-0315         PIC  X(040)         VALUE SPACES.
       01  WRK-CNPJ-BANCO-0315.
ST25X6*    03  WRK-CNPJ-PRI-0315       PIC  9(009)         VALUE ZEROS.
ST25X6*    03  WRK-CNPJ-FIL-0315       PIC  9(005)         VALUE ZEROS.
ST25X6*    03  WRK-CNPJ-CON-0315       PIC  9(002)         VALUE ZEROS.
           03  WRK-CNPJ-PRI-0315       PIC  X(009)         VALUE SPACES.
           03  WRK-CNPJ-FIL-0315       PIC  X(004)         VALUE SPACES.
           03  WRK-CNPJ-CON-0315       PIC  X(002)         VALUE SPACES.
       01  WRK-COD-JUNCAO-0315         PIC  9(005)         VALUE ZEROS.
       01  WRK-NOME-DEPTO-0315         PIC  X(040)         VALUE SPACES.
       01  WRK-SECAO-0315              PIC  9(003)         VALUE ZEROS.
       01  WRK-STATUS-0315             PIC  X(001)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*        AREA P/ DB2           *'.
      *----------------------------------------------------------------*

           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0H5
           END-EXEC.

           EXEC SQL
                DECLARE CSR-GFCTB0H5-1 CURSOR FOR
                SELECT  CSGMTO_GSTAO_TARIF,
                        CTPO_PSSOA,
                        ISGMTO_GSTAO_TARIF
                FROM    DB2PRD.TTPO_SGMTO_GSTAO
                WHERE   CSGMTO_GSTAO_TARIF   >=
                        :GFCTB0H5.CSGMTO-GSTAO-TARIF
                ORDER BY CSGMTO_GSTAO_TARIF
           END-EXEC.


      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    FIM DA WORKING GFCT5241   *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY  I#GFCTUG.

       COPY  I#GFCTUH.

       COPY  I#GFCT0M.

      *================================================================*
       PROCEDURE DIVISION              USING GFCTUG-ENTRADA
                                             GFCTUH-SAIDA
                                             GFCT0M-AREA-ERROS.
      *================================================================*

      *----------------------------------------------------------------*
       00000-ROTINA-PRINCIPAL          SECTION.
      *----------------------------------------------------------------*

           PERFORM 10000-INICIAR-PROCESSO.

           PERFORM 20000-PROCESSAR-GFCT5241.

           PERFORM 30000-FINALIZAR-PROGRAMA.

      *----------------------------------------------------------------*
       00000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       10000-INICIAR-PROCESSO          SECTION.
      *----------------------------------------------------------------*

           PERFORM 11000-INICIALIZAR-AREA-SAIDA.

           PERFORM 12000-CONSISTIR-DADOS-RECEB.

      *----------------------------------------------------------------*
       10000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       11000-INICIALIZAR-AREA-SAIDA    SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO IND-OCOR.
           MOVE SPACES                 TO GFCTUH-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE                  SQLCA
      *                                WRK-5000-AREA
                                       GFCTUH-SAIDA
                                       GFCT0M-AREA-ERROS.

           MOVE 159                    TO GFCTUH-LL.
           MOVE GFCTUG-ZZ              TO GFCTUH-ZZ.
           MOVE GFCTUG-TRANSACAO       TO GFCTUH-TRANSACAO.
           MOVE GFCTUG-FUNCAO          TO GFCTUH-FUNCAO.
           MOVE GFCTUG-FILTRO          TO GFCTUH-FILTRO.

           IF  GFCTUG-QTDE-OCOR        IS NOT NUMERIC
               MOVE ZEROS              TO GFCTUH-QTDE-OCOR
           ELSE
               MOVE GFCTUG-QTDE-OCOR   TO GFCTUH-QTDE-OCOR
           END-IF.

           IF  GFCTUG-QTDE-TOT-REG     IS NOT NUMERIC
               MOVE ZEROS              TO GFCTUH-QTDE-TOT-REG
           ELSE
               MOVE GFCTUG-QTDE-TOT-REG
                                       TO GFCTUH-QTDE-TOT-REG
           END-IF.

           MOVE GFCTUG-PONTEIRO        TO GFCTUH-PONTEIRO.
           MOVE GFCTUG-FUNC-BDSCO      TO GFCTUH-FUNC-BDSCO.
           MOVE GFCTUG-TERMINAL        TO GFCTUH-TERMINAL.
           MOVE ZEROS                  TO GFCTUH-ERRO
                                          GFCTUH-COD-SQL-ERRO
                                          GFCTUH-COD-MSG-ERRO
                                          GFCTG2-COD-MSG.
           MOVE 'N'                    TO GFCTUH-FIM.

           PERFORM 11100-OBTER-MENSAGEM.

           MOVE GFCTG3-DESC-MSG      TO GFCTUH-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       11000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       11100-OBTER-MENSAGEM            SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  GFCTG3-SAIDA.

           MOVE +100                   TO GFCTG2-LL.
           MOVE ZEROS                  TO GFCTG2-ZZ.
           MOVE GFCTUG-TRANSACAO       TO GFCTG2-TRANSACAO.
           MOVE GFCTUG-FUNCAO          TO GFCTG2-FUNCAO.
           MOVE GFCTUG-FUNC-BDSCO      TO GFCTG2-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTG2-FIM.
           MOVE 'O'                    TO GFCTG2-TIPO-PROC.

           MOVE 'GFCT5523'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTG2-ENTRADA
                                             GFCTG3-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTUH-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5241'         TO GFCT0M-TRANSACAO
               PERFORM 30000-FINALIZAR-PROGRAMA
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 9
               MOVE 9                  TO GFCTUH-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5241'         TO GFCT0M-TRANSACAO
               PERFORM 30000-FINALIZAR-PROGRAMA
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 1
               MOVE 1                  TO GFCTUH-ERRO
               MOVE GFCTG3-COD-SQL-ERRO
                                       TO GFCTUH-COD-SQL-ERRO
               MOVE GFCTG3-COD-MSG-ERRO
                                       TO GFCTUH-COD-MSG-ERRO
               MOVE GFCTG3-DESC-MSG-ERRO
                                       TO GFCTUH-DESC-MSG-ERRO
               PERFORM 30000-FINALIZAR-PROGRAMA
           END-IF.

      *    MOVE 'O'                    TO WRK-5000-AMBIENTE.
      *    MOVE 'GFCT5000'             TO WRK-MODULO.

      *    CALL 'POOL0081'             USING WRK-MODULO
      *                                      WRK-5000-ENVIA
      *                                      WRK-5000-RECEBE.

      *    IF  WRK-5000-COD-RETORNO    NOT EQUAL ZEROS
      *        IF  WRK-5000-COD-RETORNO
      *                                EQUAL 1
      *            MOVE 1              TO GFCTUH-ERRO
      *            MOVE ZEROS          TO GFCTUH-COD-SQL-ERRO
      *                                   GFCTUH-COD-MSG-ERRO
      *            MOVE GFCTG3-DESC-MSG
      *                                TO GFCTUH-DESC-MSG-ERRO
      *        END-IF
      *        IF  WRK-5000-COD-RETORNO
      *                                EQUAL 99
      *            MOVE 1              TO GFCTUH-ERRO
      *            MOVE WRK-5000-COD-SQL-ERRO
      *                                TO GFCTUH-COD-SQL-ERRO
      *            MOVE ZEROS          TO GFCTUH-COD-MSG-ERRO
      *            MOVE GFCTG3-DESC-MSG
      *                                TO GFCTUH-DESC-MSG-ERRO
      *        END-IF
      *        MOVE SPACES             TO GFCTUH-DADOS
      *        INITIALIZE GFCTUH-DADOS
      *        PERFORM 30000-FINALIZAR-PROGRAMA
      *    END-IF.

      *----------------------------------------------------------------*
       11100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       12000-CONSISTIR-DADOS-RECEB     SECTION.
      *----------------------------------------------------------------*

           IF (GFCTUG-TRANSACAO        EQUAL SPACES) OR
              (GFCTUG-FUNCAO           EQUAL SPACES) OR
              (GFCTUG-FILTRO           NOT NUMERIC)  OR
              (GFCTUG-QTDE-OCOR        NOT NUMERIC)  OR
              (GFCTUG-QTDE-TOT-REG     NOT NUMERIC)  OR
              (GFCTUG-FUNC-BDSCO       EQUAL SPACES) OR
              (GFCTUG-PONTEIRO         NOT NUMERIC)  OR
              (GFCTUG-FIM              NOT EQUAL 'S' AND 'N')
               MOVE 1                  TO GFCTUH-ERRO
               MOVE ZEROS              TO GFCTUH-COD-SQL-ERRO
               MOVE 0001               TO GFCTUH-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               MOVE 'S'                TO GFCTUH-FIM
               PERFORM 11100-OBTER-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTUH-DESC-MSG-ERRO
               PERFORM 30000-FINALIZAR-PROGRAMA
           END-IF.

      *----------------------------------------------------------------*
       12000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       20000-PROCESSAR-GFCT5241        SECTION.
      *----------------------------------------------------------------*

           PERFORM 21000-OBTER-COD-JUNCAO.

           PERFORM 22000-VERIFICAR-MASTER.

           PERFORM 23000-TRATAR-CSR-GFCTB0H5-1.

      *----------------------------------------------------------------*
       20000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       21000-OBTER-COD-JUNCAO          SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                     TO WRK-VERSAO-0315
                                              WRK-COD-FUNCIONAL-0315
                                              WRK-NOME-FUNC-0315
                                              WRK-NOME-BANCO-0315
                                              WRK-NOME-DEPTO-0315
                                              WRK-STATUS-0315
                                              WRK-HIFEN-MENSAGEM-0315
                                              WRK-TEXTO-MENSAGEM-0315.
           MOVE ZEROS                      TO WRK-COD-JUNCAO-0315
                                              WRK-COD-BANCO-0315
                                              WRK-SECAO-0315
                                              WRK-COD-MENSAGEM-0315
                                              WRK-CNPJ-PRI-0315
                                              WRK-CNPJ-FIL-0315
                                              WRK-CNPJ-CON-0315.
           MOVE 'VRS001'                   TO WRK-VERSAO-0315.
           MOVE GFCTUG-FUNC-BDSCO          TO WRK-COD-FUNCIONAL-0315.

           CALL 'SENH0315'                 USING WRK-VERSAO-0315
                                                 WRK-MENSAGEM-0315
                                                 WRK-COD-FUNCIONAL-0315
                                                 WRK-NOME-FUNC-0315
                                                 WRK-COD-BANCO-0315
                                                 WRK-NOME-BANCO-0315
                                                 WRK-CNPJ-BANCO-0315
                                                 WRK-COD-JUNCAO-0315
                                                 WRK-NOME-DEPTO-0315
                                                 WRK-SECAO-0315
                                                 WRK-STATUS-0315.

           IF  RETURN-CODE                 NOT EQUAL ZEROS AND 04
               MOVE 1                      TO GFCTUH-ERRO
               MOVE RETURN-CODE            TO GFCTUH-COD-SQL-ERRO
               MOVE 0056                   TO GFCTUH-COD-MSG-ERRO
                                              GFCTG2-COD-MSG
               PERFORM 11100-OBTER-MENSAGEM
               MOVE GFCTG3-DESC-MSG      TO GFCTUH-DESC-MSG-ERRO
               PERFORM 30000-FINALIZAR-PROGRAMA
           END-IF.

           IF  RETURN-CODE                 EQUAL 04
               IF  WRK-COD-MENSAGEM-0315   EQUAL 001
                   MOVE 1                  TO GFCTUH-ERRO
                   MOVE ZEROS              TO GFCTUH-COD-SQL-ERRO
                   MOVE 0057               TO GFCTUH-COD-MSG-ERRO
                                              GFCTG2-COD-MSG
                   PERFORM 11100-OBTER-MENSAGEM
                   MOVE GFCTG3-DESC-MSG  TO GFCTUH-DESC-MSG-ERRO
                   PERFORM 30000-FINALIZAR-PROGRAMA
               END-IF

               IF  WRK-COD-MENSAGEM-0315   EQUAL 154
                   MOVE 1                  TO GFCTUH-ERRO
                   MOVE ZEROS              TO GFCTUH-COD-SQL-ERRO
                   MOVE 0058               TO GFCTUH-COD-MSG-ERRO
                                              GFCTG2-COD-MSG
                   PERFORM 11100-OBTER-MENSAGEM
                   MOVE GFCTG3-DESC-MSG    TO GFCTUH-DESC-MSG-ERRO
                   PERFORM 30000-FINALIZAR-PROGRAMA
               END-IF

               IF  WRK-COD-MENSAGEM-0315   EQUAL 242
                   MOVE 1                  TO GFCTUH-ERRO
                   MOVE ZEROS              TO GFCTUH-COD-SQL-ERRO
                   MOVE 0059               TO GFCTUH-COD-MSG-ERRO
                                              GFCTG2-COD-MSG
                   PERFORM 11100-OBTER-MENSAGEM
                   MOVE GFCTG3-DESC-MSG    TO GFCTUH-DESC-MSG-ERRO
                   PERFORM 30000-FINALIZAR-PROGRAMA
               END-IF

               IF  WRK-COD-MENSAGEM-0315   EQUAL 009
                   MOVE ZEROS              TO GFCTUH-ERRO
                                              GFCTUH-COD-SQL-ERRO
                                              GFCTUH-COD-MSG-ERRO
                                              GFCTG2-COD-MSG
                   PERFORM 11100-OBTER-MENSAGEM
                   MOVE GFCTG3-DESC-MSG    TO GFCTUH-DESC-MSG-ERRO
                   PERFORM 30000-FINALIZAR-PROGRAMA
               END-IF

               IF  WRK-COD-MENSAGEM-0315   EQUAL 138
                   MOVE 1                  TO GFCTUH-ERRO
                   MOVE ZEROS              TO GFCTUH-COD-SQL-ERRO
                   MOVE 0060               TO GFCTUH-COD-MSG-ERRO
                                              GFCTG2-COD-MSG
                   PERFORM 11100-OBTER-MENSAGEM
                   MOVE GFCTG3-DESC-MSG    TO GFCTUH-DESC-MSG-ERRO
                   PERFORM 30000-FINALIZAR-PROGRAMA
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       21000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       22000-VERIFICAR-MASTER          SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  WRK-5507-AREA.

           MOVE 'O'                    TO WRK-5507-AMBIENTE.
           MOVE WRK-COD-JUNCAO-0315    TO WRK-5507-CJUNC-DEPDC.
           MOVE 'GFCT5507'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             WRK-5507-AREA.

           IF  WRK-5507-COD-RETORNO    NOT EQUAL ZEROS
               IF  WRK-5507-COD-RETORNO
                                       EQUAL 01
                   MOVE 1              TO GFCTUH-ERRO
                   MOVE ZEROS          TO GFCTUH-COD-SQL-ERRO
                                          GFCTUH-COD-MSG-ERRO
                   MOVE WRK-5507-MSG-RETORNO
                                       TO GFCTUH-DESC-MSG-ERRO
               ELSE
                   IF  WRK-5507-COD-RETORNO
                                       EQUAL 99
                       MOVE 1          TO GFCTUH-ERRO
                       MOVE WRK-5507-COD-SQL-ERRO
                                       TO GFCTUH-COD-SQL-ERRO
                       MOVE ZEROS      TO GFCTUH-COD-MSG-ERRO
                       MOVE WRK-5507-MSG-RETORNO
                                       TO GFCTUH-DESC-MSG-ERRO
                   END-IF
               END-IF
               MOVE SPACES             TO GFCTUH-DADOS
               INITIALIZE GFCTUH-DADOS
               PERFORM 30000-FINALIZAR-PROGRAMA
           END-IF.

      *----------------------------------------------------------------*
       22000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       23000-TRATAR-CSR-GFCTB0H5-1     SECTION.
      *----------------------------------------------------------------*

           PERFORM 23100-ABRIR-CSR-GFCTB0H5-1.

           PERFORM 23200-LER-CSR-GFCTB0H5-1.

           MOVE 1                      TO IND-OCOR.

           PERFORM 23300-PROCESSAR-CSR-GFCTB0H5-1
                   UNTIL WRK-FINALIZAR EQUAL 'S'.

      *----------------------------------------------------------------*
       23000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       23100-ABRIR-CSR-GFCTB0H5-1      SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTUG-PONTEIRO  TO   CSGMTO-GSTAO-TARIF  OF GFCTB0H5.
           MOVE 'N'              TO   WRK-FINALIZAR.

           EXEC SQL
                OPEN CSR-GFCTB0H5-1
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'GFCT5241'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'TTPO_SGMTO_GSTAO' TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-STACODE
                                          GFCTUH-COD-SQL-ERRO
               MOVE '0001'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 9                  TO GFCTUH-ERRO
               MOVE 0005               TO GFCTUH-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               PERFORM 11100-OBTER-MENSAGEM
               MOVE ' - GFCTB0H5'      TO WRK-NOME-TAB
               MOVE SPACES             TO GFCTUH-DESC-MSG-ERRO
               STRING GFCTG3-DESC-MSG
                      WRK-NOME-TAB
               DELIMITED BY '  '       INTO GFCTUH-DESC-MSG-ERRO
           END-IF.

      *----------------------------------------------------------------*
       23100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       23200-LER-CSR-GFCTB0H5-1        SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                FETCH   CSR-GFCTB0H5-1
                INTO   :GFCTB0H5.CSGMTO-GSTAO-TARIF,
                       :GFCTB0H5.CTPO-PSSOA,
                       :GFCTB0H5.ISGMTO-GSTAO-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'GFCT5241'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'TTPO_SGMTO_GSTAO' TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-STACODE
                                          GFCTUH-COD-SQL-ERRO
               MOVE '0002'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 9                  TO GFCTUH-ERRO
               MOVE 0006               TO GFCTUH-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               PERFORM 11100-OBTER-MENSAGEM
               MOVE ' - GFCTB0H5'      TO WRK-NOME-TAB
               MOVE SPACES             TO GFCTUH-DESC-MSG-ERRO
               STRING GFCTG3-DESC-MSG
                      WRK-NOME-TAB
               DELIMITED BY '  '       INTO GFCTUH-DESC-MSG-ERRO
           ELSE
               IF  SQLCODE             EQUAL +100
                   MOVE 'S'            TO WRK-FINALIZAR
                                          GFCTUH-FIM
                   IF  IND-OCOR        EQUAL ZEROS
                       MOVE ZEROS      TO GFCTUH-ERRO
                                          GFCTUH-COD-SQL-ERRO
                       MOVE 0007       TO GFCTUH-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
                       PERFORM 11100-OBTER-MENSAGEM
                       MOVE ' - GFCTB0H5'
                                       TO WRK-NOME-TAB
                       MOVE SPACES     TO GFCTUH-DESC-MSG-ERRO
                       STRING GFCTG3-DESC-MSG
                              WRK-NOME-TAB
                       DELIMITED BY '  '
                                       INTO GFCTUH-DESC-MSG-ERRO
                   END-IF
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       23200-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       23300-PROCESSAR-CSR-GFCTB0H5-1  SECTION.
      *----------------------------------------------------------------*

           ADD 23                     TO GFCTUH-LL.
           ADD 1                      TO GFCTUH-QTDE-TOT-REG.

           MOVE CSGMTO-GSTAO-TARIF    OF GFCTB0H5 TO
                                         GFCTUH-COD-SEGMENTO(IND-OCOR).
           MOVE ISGMTO-GSTAO-TARIF    OF GFCTB0H5 TO
                                         GFCTUH-DESC-SEGMENTO(IND-OCOR).

           PERFORM 23200-LER-CSR-GFCTB0H5-1.

           ADD 1                       TO IND-OCOR.

           IF  IND-OCOR                GREATER 37
               IF  WRK-FINALIZAR       NOT EQUAL 'S'
                   MOVE CSGMTO-GSTAO-TARIF    OF GFCTB0H5
                                       TO GFCTUH-PONTEIRO
                   MOVE 'S'            TO WRK-FINALIZAR
               ELSE
                   MOVE ZEROS          TO GFCTUH-PONTEIRO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       23300-99-FIM.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       30000-FINALIZAR-PROGRAMA        SECTION.
      *----------------------------------------------------------------*

           PERFORM 31000-FECHAR-CSR-GFCTB0H5-1

           GOBACK.

      *----------------------------------------------------------------*
       30000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       31000-FECHAR-CSR-GFCTB0H5-1     SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                CLOSE CSR-GFCTB0H5-1
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'GFCT5241'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'TTPO_SGMTO_GSTAO' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-STACODE
                                          GFCTUH-COD-SQL-ERRO
               MOVE '0005'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 9                  TO GFCTUH-ERRO
               MOVE 0011               TO GFCTUH-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               PERFORM 11100-OBTER-MENSAGEM
               MOVE ' - GFCTB0H5'      TO WRK-NOME-TAB
               MOVE SPACES             TO GFCTUH-DESC-MSG-ERRO
               STRING GFCTG3-DESC-MSG
                      WRK-NOME-TAB
               DELIMITED BY '  '       INTO GFCTUH-DESC-MSG-ERRO
           END-IF.

      *----------------------------------------------------------------*
       31000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

