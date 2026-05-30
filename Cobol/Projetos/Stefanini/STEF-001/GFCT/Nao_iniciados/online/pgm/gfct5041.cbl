      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT5041.
       AUTHOR. MIRELLI VENTURA.
      *================================================================*
      *                   C P M   S I S T E M A S                      *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : GFCT5041                                     *
      *    PROGRAMADORA : MIRELLI VENTURA             - CPM/FPOLIS     *
      *    ANALISTA CPM : JOSIANE T. SOARES           - CPM/FPOLIS     *
      *    ANALISTA     : RICARDO PEREIRA - PROCWORK  - GRUPO 50       *
      *    DATA         : 10/05/2005                                   *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      GERAR DADOS PARA COMBO DE SEGMENTOS.                      *
      *                                                                *
      *    BANCO DE DADOS:                                             *
      *     DB2                                                        *
      *      TABLE                               INCLUDE/BOOK          *
      *      DB2PRD.TPO_SGMTO_CLI                  CLIEB084            *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      GFCT5000 - OBTER DESCRICAO DE MENSAGENS                   *
      *      GFCT5507 - VERIFICA MASTER                                *
      *      POOL0081 - ALOCAR DINAMICAMENTE UM MODULO                 *
      *      SENH0315 - OBTER JUNCAO                                   *
      *                                                                *
      *    CHAMADO POR: GFCT7740                                       *
      *                                                                *
      *========================= A L T E R A C A O ====================*
      *    ANALISTA     : WAGNER          - PROCWORK  - GRUPO 50       *
      *    DATA         : 18/08/2005                                   *
      *                                                                *
      *    OBJETIVO     : INIBIR ACESSO AO CAMPO CJUNC_DEPDC_SGMTO     *
      *                   NA TABELA CLIEB084                           *
      *================================================================*

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
           '*  INICIO DA WORKING GFCT5041  *'.
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
       77  WRK-CSR-ABERTO-1            PIC X(01)           VALUE SPACES.
       77  WRK-CSR-ABERTO-2            PIC X(01)           VALUE SPACES.
       77  WRK-NOME-TAB                PIC X(11)           VALUE SPACES.

      *----------------------------------------------------------------*
       77  FILLER                      PIC X(32)           VALUE
           '*   INDICADORES DE NULIDADE    *'.
      *----------------------------------------------------------------*

       01  WRK-CLIEB084-NULL.
           03 WRK-DCADTO-NULL          PIC S9(04) COMP     VALUE +0.
           03 WRK-DULT-ALT-NULL        PIC S9(04) COMP     VALUE +0.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '* AREA DE COMUNICACAO GFCT5000 *'.
      *----------------------------------------------------------------*

       01  WRK-5000-AREA.
           03  WRK-5000-ENVIA.
               05  WRK-5000-AMBIENTE   PIC X(001)          VALUE SPACES.
               05  WRK-5000-COD-MSG    PIC 9(004)          VALUE ZEROS.
           03  WRK-5000-RECEBE.
               05  WRK-5000-COD-RETORNO
                                       PIC 9(002)          VALUE ZEROS.
               05  WRK-5000-COD-SQL-ERRO
                                       PIC 9(003)          VALUE ZEROS.
               05  WRK-5000-DESC-MSG   PIC X(070)          VALUE SPACES.

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
ST25X6*     03  WRK-CNPJ-PRI-0315       PIC  9(009)         VALUE ZEROS.
ST25X6*     03  WRK-CNPJ-FIL-0315       PIC  9(005)         VALUE ZEROS.
ST25X6*     03  WRK-CNPJ-CON-0315       PIC  9(002)         VALUE ZEROS.
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
                INCLUDE CLIEB084
           END-EXEC.

           EXEC SQL
                DECLARE CSR-CLIEB084-1 CURSOR FOR
                SELECT  CSGMTO_CLI,
                        CTPO_PSSOA,
                        ISGMTO_CLI,
                        RSGMTO_CLI,
                        DCADTO,
                        DULT_ALT
************************CJUNC_DEPDC_SGMTO
                FROM    DB2PRD.TPO_SGMTO_CLI
                WHERE   CSGMTO_CLI       >= :CLIEB084.CSGMTO-CLI
                ORDER BY CSGMTO_CLI
           END-EXEC.

           EXEC SQL
                DECLARE CSR-CLIEB084-2 CURSOR FOR
                SELECT  CSGMTO_CLI,
                        CTPO_PSSOA,
                        ISGMTO_CLI,
                        RSGMTO_CLI,
                        DCADTO,
                        DULT_ALT
************************CJUNC_DEPDC_SGMTO
                FROM    DB2PRD.TPO_SGMTO_CLI
                WHERE   CSGMTO_CLI       >= :CLIEB084.CSGMTO-CLI
****************AND     CJUNC_DEPDC_SGMTO = :CLIEB084.CJUNC-DEPDC-SGMTO
                ORDER BY CSGMTO_CLI
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    FIM DA WORKING GFCT5041   *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

-INC I#GFCT8E

-INC I#GFCT8F

-INC I#GFCT0M

      *================================================================*
       PROCEDURE DIVISION              USING GFCT8E-ENTRADA
                                             GFCT8F-SAIDA
                                             GFCT0M-AREA-ERROS.
      *================================================================*

      *----------------------------------------------------------------*
       00000-ROTINA-PRINCIPAL          SECTION.
      *----------------------------------------------------------------*

           PERFORM 10000-INICIAR-PROCESSO.

           PERFORM 20000-PROCESSAR-GFCT5041.

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
           MOVE SPACES                 TO GFCT8F-SAIDA
                                          GFCT0M-AREA-ERROS
                                          WRK-CSR-ABERTO-1
                                          WRK-CSR-ABERTO-2.

           INITIALIZE                  SQLCA
                                       WRK-5000-AREA
                                       GFCT8F-SAIDA
                                       GFCT0M-AREA-ERROS.

           MOVE 159                    TO GFCT8F-LL.
           MOVE GFCT8E-ZZ              TO GFCT8F-ZZ.
           MOVE GFCT8E-TRANSACAO       TO GFCT8F-TRANSACAO.
           MOVE GFCT8E-FUNCAO          TO GFCT8F-FUNCAO.
           MOVE GFCT8E-FILTRO          TO GFCT8F-FILTRO.

           IF  GFCT8E-QTDE-OCOR        IS NOT NUMERIC
               MOVE ZEROS              TO GFCT8F-QTDE-OCOR
           ELSE
               MOVE GFCT8E-QTDE-OCOR   TO GFCT8F-QTDE-OCOR
           END-IF.

           IF  GFCT8E-QTDE-TOT-REG     IS NOT NUMERIC
               MOVE ZEROS              TO GFCT8F-QTDE-TOT-REG
           ELSE
               MOVE GFCT8E-QTDE-TOT-REG
                                       TO GFCT8F-QTDE-TOT-REG
           END-IF.

           MOVE GFCT8E-PONTEIRO        TO GFCT8F-PONTEIRO.
           MOVE GFCT8E-FUNC-BDSCO      TO GFCT8F-FUNC-BDSCO.
           MOVE GFCT8E-TERMINAL        TO GFCT8F-TERMINAL.
           MOVE ZEROS                  TO GFCT8F-ERRO
                                          GFCT8F-COD-SQL-ERRO
                                          GFCT8F-COD-MSG-ERRO
                                          WRK-5000-COD-MSG.
           MOVE 'N'                    TO GFCT8F-FIM.

           PERFORM 11100-OBTER-MENSAGEM.

           MOVE WRK-5000-DESC-MSG      TO GFCT8F-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       11000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       11100-OBTER-MENSAGEM            SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  WRK-5000-AREA.

           MOVE 'O'                    TO WRK-5000-AMBIENTE.
           MOVE 'GFCT5000'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             WRK-5000-ENVIA
                                             WRK-5000-RECEBE.

           IF  WRK-5000-COD-RETORNO    NOT EQUAL ZEROS
               IF  WRK-5000-COD-RETORNO
                                       EQUAL 1
                   MOVE 1              TO GFCT8F-ERRO
                   MOVE ZEROS          TO GFCT8F-COD-SQL-ERRO
                                          GFCT8F-COD-MSG-ERRO
                   MOVE WRK-5000-DESC-MSG
                                       TO GFCT8F-DESC-MSG-ERRO
               END-IF
               IF  WRK-5000-COD-RETORNO
                                       EQUAL 99
                   MOVE 1              TO GFCT8F-ERRO
                   MOVE WRK-5000-COD-SQL-ERRO
                                       TO GFCT8F-COD-SQL-ERRO
                   MOVE ZEROS          TO GFCT8F-COD-MSG-ERRO
                   MOVE WRK-5000-DESC-MSG
                                       TO GFCT8F-DESC-MSG-ERRO
               END-IF
               MOVE SPACES             TO GFCT8F-DADOS
               INITIALIZE GFCT8F-DADOS
               PERFORM 30000-FINALIZAR-PROGRAMA
           END-IF.

      *----------------------------------------------------------------*
       11100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       12000-CONSISTIR-DADOS-RECEB     SECTION.
      *----------------------------------------------------------------*

           IF (GFCT8E-TRANSACAO        EQUAL SPACES) OR
              (GFCT8E-FUNCAO           EQUAL SPACES) OR
              (GFCT8E-FILTRO           NOT NUMERIC)  OR
              (GFCT8E-QTDE-OCOR        NOT NUMERIC)  OR
              (GFCT8E-QTDE-TOT-REG     NOT NUMERIC)  OR
              (GFCT8E-FUNC-BDSCO       EQUAL SPACES) OR
CPM***********(GFCT8E-TERMINAL         EQUAL SPACES) OR
              (GFCT8E-PONTEIRO         NOT NUMERIC)  OR
              (GFCT8E-FIM              NOT EQUAL 'S' AND 'N')
               MOVE 1                  TO GFCT8F-ERRO
               MOVE ZEROS              TO GFCT8F-COD-SQL-ERRO
               MOVE 0001               TO GFCT8F-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'S'                TO GFCT8F-FIM
               PERFORM 11100-OBTER-MENSAGEM
               MOVE WRK-5000-DESC-MSG  TO GFCT8F-DESC-MSG-ERRO
               PERFORM 30000-FINALIZAR-PROGRAMA
           END-IF.

      *----------------------------------------------------------------*
       12000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       20000-PROCESSAR-GFCT5041        SECTION.
      *----------------------------------------------------------------*

           PERFORM 21000-OBTER-COD-JUNCAO.

           PERFORM 22000-VERIFICAR-MASTER.

           IF  WRK-5507-IDTFD-MASTER   EQUAL 'S'
               PERFORM 23000-TRATAR-CSR-CLIEB084-1
           ELSE
               PERFORM 24000-TRATAR-CSR-CLIEB084-2
           END-IF.

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
           MOVE GFCT8E-FUNC-BDSCO          TO WRK-COD-FUNCIONAL-0315.

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
               MOVE 1                      TO GFCT8F-ERRO
               MOVE RETURN-CODE            TO GFCT8F-COD-SQL-ERRO
               MOVE 0056                   TO GFCT8F-COD-MSG-ERRO
                                              WRK-5000-COD-MSG
               PERFORM 11100-OBTER-MENSAGEM
               MOVE WRK-5000-DESC-MSG      TO GFCT8F-DESC-MSG-ERRO
               PERFORM 30000-FINALIZAR-PROGRAMA
           END-IF.

           IF  RETURN-CODE                 EQUAL 04
               IF  WRK-COD-MENSAGEM-0315   EQUAL 001
                   MOVE 1                  TO GFCT8F-ERRO
                   MOVE ZEROS              TO GFCT8F-COD-SQL-ERRO
                   MOVE 0057               TO GFCT8F-COD-MSG-ERRO
                                              WRK-5000-COD-MSG
                   PERFORM 11100-OBTER-MENSAGEM
                   MOVE WRK-5000-DESC-MSG  TO GFCT8F-DESC-MSG-ERRO
                   PERFORM 30000-FINALIZAR-PROGRAMA
               END-IF

               IF  WRK-COD-MENSAGEM-0315   EQUAL 154
                   MOVE 1                  TO GFCT8F-ERRO
                   MOVE ZEROS              TO GFCT8F-COD-SQL-ERRO
                   MOVE 0058               TO GFCT8F-COD-MSG-ERRO
                                              WRK-5000-COD-MSG
                   PERFORM 11100-OBTER-MENSAGEM
                   MOVE WRK-5000-DESC-MSG  TO GFCT8F-DESC-MSG-ERRO
                   PERFORM 30000-FINALIZAR-PROGRAMA
               END-IF

               IF  WRK-COD-MENSAGEM-0315   EQUAL 242
                   MOVE 1                  TO GFCT8F-ERRO
                   MOVE ZEROS              TO GFCT8F-COD-SQL-ERRO
                   MOVE 0059               TO GFCT8F-COD-MSG-ERRO
                                              WRK-5000-COD-MSG
                   PERFORM 11100-OBTER-MENSAGEM
                   MOVE WRK-5000-DESC-MSG  TO GFCT8F-DESC-MSG-ERRO
                   PERFORM 30000-FINALIZAR-PROGRAMA
               END-IF

               IF  WRK-COD-MENSAGEM-0315   EQUAL 009
                   MOVE ZEROS              TO GFCT8F-ERRO
                                              GFCT8F-COD-SQL-ERRO
                                              GFCT8F-COD-MSG-ERRO
                                              WRK-5000-COD-MSG
                   PERFORM 11100-OBTER-MENSAGEM
                   MOVE WRK-5000-DESC-MSG  TO GFCT8F-DESC-MSG-ERRO
                   PERFORM 30000-FINALIZAR-PROGRAMA
               END-IF

               IF  WRK-COD-MENSAGEM-0315   EQUAL 138
                   MOVE 1                  TO GFCT8F-ERRO
                   MOVE ZEROS              TO GFCT8F-COD-SQL-ERRO
                   MOVE 0060               TO GFCT8F-COD-MSG-ERRO
                                              WRK-5000-COD-MSG
                   PERFORM 11100-OBTER-MENSAGEM
                   MOVE WRK-5000-DESC-MSG  TO GFCT8F-DESC-MSG-ERRO
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
                   MOVE 1              TO GFCT8F-ERRO
                   MOVE ZEROS          TO GFCT8F-COD-SQL-ERRO
                                          GFCT8F-COD-MSG-ERRO
                   MOVE WRK-5507-MSG-RETORNO
                                       TO GFCT8F-DESC-MSG-ERRO
               ELSE
                   IF  WRK-5507-COD-RETORNO
                                       EQUAL 99
                       MOVE 1          TO GFCT8F-ERRO
                       MOVE WRK-5507-COD-SQL-ERRO
                                       TO GFCT8F-COD-SQL-ERRO
                       MOVE ZEROS      TO GFCT8F-COD-MSG-ERRO
                       MOVE WRK-5507-MSG-RETORNO
                                       TO GFCT8F-DESC-MSG-ERRO
                   END-IF
               END-IF
               MOVE SPACES             TO GFCT8F-DADOS
               INITIALIZE GFCT8F-DADOS
               PERFORM 30000-FINALIZAR-PROGRAMA
           END-IF.

      *----------------------------------------------------------------*
       22000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       23000-TRATAR-CSR-CLIEB084-1     SECTION.
      *----------------------------------------------------------------*

           PERFORM 23100-ABRIR-CSR-CLIEB084-1.

           PERFORM 23200-LER-CSR-CLIEB084-1.

           MOVE 1                      TO IND-OCOR.

           PERFORM 23300-PROCESSAR-CSR-CLIEB084-1
                   UNTIL WRK-FINALIZAR EQUAL 'S'.

      *----------------------------------------------------------------*
       23000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       23100-ABRIR-CSR-CLIEB084-1      SECTION.
      *----------------------------------------------------------------*

           MOVE GFCT8E-PONTEIRO        TO CSGMTO-CLI        OF CLIEB084.
           MOVE 'N'                    TO WRK-FINALIZAR.

           EXEC SQL
                OPEN CSR-CLIEB084-1
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'GFCT5041'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'TPO_SGMTO_CLI'    TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO  GFCT0M-STACODE
                                          GFCT8F-COD-SQL-ERRO
               MOVE '0001'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 9                  TO GFCT8F-ERRO
               MOVE 0005               TO GFCT8F-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 11100-OBTER-MENSAGEM
               MOVE ' - CLIEB084'      TO WRK-NOME-TAB
               MOVE SPACES             TO GFCT8F-DESC-MSG-ERRO
               STRING WRK-5000-DESC-MSG
                      WRK-NOME-TAB
               DELIMITED BY '  '       INTO GFCT8F-DESC-MSG-ERRO
           END-IF.

           MOVE 'S'                    TO WRK-CSR-ABERTO-1.

      *----------------------------------------------------------------*
       23100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       23200-LER-CSR-CLIEB084-1        SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                FETCH   CSR-CLIEB084-1
                INTO   :CLIEB084.CSGMTO-CLI,
                       :CLIEB084.CTPO-PSSOA,
                       :CLIEB084.ISGMTO-CLI,
                       :CLIEB084.RSGMTO-CLI,
                       :CLIEB084.DCADTO
                           :WRK-DCADTO-NULL,
                       :CLIEB084.DULT-ALT
                           :WRK-DULT-ALT-NULL
***********************:CLIEB084.CJUNC-DEPDC-SGMTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'GFCT5041'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'TPO_SGMTO_CLI'    TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO  GFCT0M-STACODE
                                          GFCT8F-COD-SQL-ERRO
               MOVE '0002'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 9                  TO GFCT8F-ERRO
               MOVE 0006               TO GFCT8F-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 11100-OBTER-MENSAGEM
               MOVE ' - CLIEB084'      TO WRK-NOME-TAB
               MOVE SPACES             TO GFCT8F-DESC-MSG-ERRO
               STRING WRK-5000-DESC-MSG
                      WRK-NOME-TAB
               DELIMITED BY '  '       INTO GFCT8F-DESC-MSG-ERRO
           ELSE
               IF  SQLCODE             EQUAL +100
                   MOVE 'S'            TO WRK-FINALIZAR
                                          GFCT8F-FIM
                   IF  IND-OCOR        EQUAL ZEROS
                       MOVE ZEROS      TO GFCT8F-ERRO
                                          GFCT8F-COD-SQL-ERRO
                       MOVE 0007       TO GFCT8F-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                       PERFORM 11100-OBTER-MENSAGEM
                       MOVE ' - CLIEB084'
                                       TO WRK-NOME-TAB
                       MOVE SPACES     TO GFCT8F-DESC-MSG-ERRO
                       STRING WRK-5000-DESC-MSG
                              WRK-NOME-TAB
                       DELIMITED BY '  '
                                       INTO GFCT8F-DESC-MSG-ERRO
                   END-IF
               ELSE
                   IF  WRK-DCADTO-NULL LESS ZEROS
                       MOVE ZEROS      TO DCADTO   OF CLIEB084
                   END-IF
                   IF  WRK-DULT-ALT-NULL
                                       LESS ZEROS
                       MOVE ZEROS      TO DULT-ALT OF CLIEB084
                   END-IF
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       23200-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       23300-PROCESSAR-CSR-CLIEB084-1  SECTION.
      *----------------------------------------------------------------*

           ADD 23                      TO GFCT8F-LL.
CPM        ADD 1                       TO GFCT8F-QTDE-TOT-REG.

           MOVE CSGMTO-CLI OF CLIEB084 TO GFCT8F-COD-SEGMENTO(IND-OCOR).
           MOVE ISGMTO-CLI OF CLIEB084 TO
                                         GFCT8F-DESC-SEGMENTO(IND-OCOR).

           PERFORM 23200-LER-CSR-CLIEB084-1.

           ADD 1                       TO IND-OCOR.

           IF  IND-OCOR                GREATER 37
               IF  WRK-FINALIZAR       NOT EQUAL 'S'
                   MOVE CSGMTO-CLI OF CLIEB084
                                       TO GFCT8F-PONTEIRO
                   MOVE 'S'            TO WRK-FINALIZAR
               ELSE
                   MOVE ZEROS          TO GFCT8F-PONTEIRO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       23300-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       24000-TRATAR-CSR-CLIEB084-2     SECTION.
      *----------------------------------------------------------------*

           PERFORM 24100-ABRIR-CSR-CLIEB084-2.

           PERFORM 24200-LER-CSR-CLIEB084-2.

           MOVE 1                      TO IND-OCOR.

           PERFORM 24300-PROCESSAR-CSR-CLIEB084-2
                   UNTIL WRK-FINALIZAR EQUAL 'S'.

      *----------------------------------------------------------------*
       24000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       24100-ABRIR-CSR-CLIEB084-2      SECTION.
      *----------------------------------------------------------------*

***********MOVE WRK-COD-JUNCAO-0315    TO CJUNC-DEPDC-SGMTO OF CLIEB084.
           MOVE GFCT8E-PONTEIRO        TO CSGMTO-CLI        OF CLIEB084.
           MOVE 'N'                    TO WRK-FINALIZAR.

           EXEC SQL
                OPEN CSR-CLIEB084-2
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'GFCT5041'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'TPO_SGMTO_CLI'    TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO  GFCT0M-STACODE
                                          GFCT8F-COD-SQL-ERRO
               MOVE '0003'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 9                  TO GFCT8F-ERRO
               MOVE 0005               TO GFCT8F-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 11100-OBTER-MENSAGEM
               MOVE ' - CLIEB084'      TO WRK-NOME-TAB
               MOVE SPACES             TO GFCT8F-DESC-MSG-ERRO
               STRING WRK-5000-DESC-MSG
                      WRK-NOME-TAB
               DELIMITED BY '  '       INTO GFCT8F-DESC-MSG-ERRO
           END-IF.

           MOVE 'S'                    TO WRK-CSR-ABERTO-2.

      *----------------------------------------------------------------*
       24100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       24200-LER-CSR-CLIEB084-2        SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                FETCH CSR-CLIEB084-2
                INTO :CLIEB084.CSGMTO-CLI,
                     :CLIEB084.CTPO-PSSOA,
                     :CLIEB084.ISGMTO-CLI,
                     :CLIEB084.RSGMTO-CLI,
                     :CLIEB084.DCADTO
                         :WRK-DCADTO-NULL,
                     :CLIEB084.DULT-ALT
                         :WRK-DULT-ALT-NULL
*********************:CLIEB084.CJUNC-DEPDC-SGMTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'GFCT5041'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'TPO_SGMTO_CLI'    TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO  GFCT0M-STACODE
                                          GFCT8F-COD-SQL-ERRO
               MOVE '0004'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 9                  TO GFCT8F-ERRO
               MOVE 0006               TO GFCT8F-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 11100-OBTER-MENSAGEM
               MOVE ' - CLIEB084'      TO WRK-NOME-TAB
               MOVE SPACES             TO GFCT8F-DESC-MSG-ERRO
               STRING WRK-5000-DESC-MSG
                      WRK-NOME-TAB
               DELIMITED BY '  '       INTO GFCT8F-DESC-MSG-ERRO
           ELSE
               IF  SQLCODE             EQUAL +100
                   MOVE 'S'            TO WRK-FINALIZAR
CPM                MOVE 'S'            TO GFCT8F-FIM
                   IF  IND-OCOR        EQUAL ZEROS
                       MOVE ZEROS      TO GFCT8F-ERRO
                                          GFCT8F-COD-SQL-ERRO
                       MOVE 0007       TO GFCT8F-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                       PERFORM 11100-OBTER-MENSAGEM
                       MOVE ' - CLIEB084'
                                       TO WRK-NOME-TAB
                       MOVE SPACES     TO GFCT8F-DESC-MSG-ERRO
                       STRING WRK-5000-DESC-MSG
                              WRK-NOME-TAB
                       DELIMITED BY '  '
                                       INTO GFCT8F-DESC-MSG-ERRO
                   END-IF
               ELSE
                   IF  WRK-DCADTO-NULL LESS ZEROS
                       MOVE ZEROS      TO DCADTO   OF CLIEB084
                   END-IF
                   IF  WRK-DULT-ALT-NULL
                                       LESS ZEROS
                       MOVE ZEROS      TO DULT-ALT OF CLIEB084
                   END-IF
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       24200-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       24300-PROCESSAR-CSR-CLIEB084-2  SECTION.
      *----------------------------------------------------------------*

           ADD 23                      TO GFCT8F-LL.
CPM        ADD 1                       TO GFCT8F-QTDE-TOT-REG.

           MOVE CSGMTO-CLI             OF CLIEB084
                                       TO GFCT8F-COD-SEGMENTO(IND-OCOR).
           MOVE ISGMTO-CLI             OF CLIEB084
                                       TO
                                         GFCT8F-DESC-SEGMENTO(IND-OCOR).

           PERFORM 24200-LER-CSR-CLIEB084-2.

           ADD 1                       TO IND-OCOR.

           IF  IND-OCOR                GREATER 37
               IF  WRK-FINALIZAR       NOT EQUAL 'S'
                   MOVE CSGMTO-CLI     OF CLIEB084
                                       TO GFCT8F-PONTEIRO
                   MOVE 'S'            TO WRK-FINALIZAR
               ELSE
                   MOVE ZEROS          TO GFCT8F-PONTEIRO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       24300-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       30000-FINALIZAR-PROGRAMA        SECTION.
      *----------------------------------------------------------------*

           IF  WRK-CSR-ABERTO-1        EQUAL 'S'
               PERFORM 31000-FECHAR-CSR-CLIEB084-1
           ELSE
               IF  WRK-CSR-ABERTO-2    EQUAL 'S'
                   PERFORM 32000-FECHAR-CSR-CLIEB084-2
               END-IF
           END-IF.

           GOBACK.

      *----------------------------------------------------------------*
       30000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       31000-FECHAR-CSR-CLIEB084-1     SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                CLOSE CSR-CLIEB084-1
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'GFCT5041'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'TPO_SGMTO_CLI'    TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO  GFCT0M-STACODE
                                          GFCT8F-COD-SQL-ERRO
               MOVE '0005'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 9                  TO GFCT8F-ERRO
               MOVE 0011               TO GFCT8F-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 11100-OBTER-MENSAGEM
               MOVE ' - CLIEB084'      TO WRK-NOME-TAB
               MOVE SPACES             TO GFCT8F-DESC-MSG-ERRO
               STRING WRK-5000-DESC-MSG
                      WRK-NOME-TAB
               DELIMITED BY '  '       INTO GFCT8F-DESC-MSG-ERRO
           END-IF.

      *----------------------------------------------------------------*
       31000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       32000-FECHAR-CSR-CLIEB084-2     SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                CLOSE CSR-CLIEB084-2
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'GFCT5041'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'TPO_SGMTO_CLI'    TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO  GFCT0M-STACODE
                                          GFCT8F-COD-SQL-ERRO
               MOVE '0006'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 9                  TO GFCT8F-ERRO
               MOVE 0011               TO GFCT8F-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 11100-OBTER-MENSAGEM
               MOVE ' - CLIEB084'      TO WRK-NOME-TAB
               MOVE SPACES             TO GFCT8F-DESC-MSG-ERRO
               STRING WRK-5000-DESC-MSG
                      WRK-NOME-TAB
               DELIMITED BY '  '       INTO GFCT8F-DESC-MSG-ERRO
           END-IF.

      *----------------------------------------------------------------*
       32000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*
