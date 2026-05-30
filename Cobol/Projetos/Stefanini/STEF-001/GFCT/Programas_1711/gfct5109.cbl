      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT5109.
       AUTHOR.     PROCWORK OUTSOURCING.
      *================================================================*
      *                   C P M   S I S T E M A S                      *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : GFCT5109                                     *
      *    PROGRAMADOR  : PROCWORK OUTSOURCING                         *
      *    ANALISTA PWO : MARCUS VINICIUS - PROCWORK    - GRUPO 50     *
      *    ANALISTA     : RICARDO PEREIRA - PROCWORK    - GRUPO 50     *
      *    DATA         : 08/12/2005                                   *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      GERAR DADOS PARA A TEXT EXCLUSIVA DE PAB'S.               *
      *                                                                *
      *    BANCO DE DADOS:                                             *
      *      DB2                                                       *
      *       TABLE                                INCLUDE/BOOK        *
      *       DB2PRD.POSTO_BCRIO_SERVC               MESUB062          *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      GFCT5000 - OBTER DESCRICAO DE MENSAGENS.                  *
      *      POOL0081 - ALOCAR DINAMICAMENTE UM MODULO.                *
      *                                                                *
      *    NAVEGACAO                                                   *
      *      CHAMADO POR: GFCT7372.                                    *
      *                                                                *
      *================================================================*

      *================================================================*
      *                        A L T E R A C A O                       *
      *----------------------------------------------------------------*
      *    PROGRAMADORA:   VANESSA FROZZA          - CPM BRAXIS - P.BCO*
      *    ANALISTA CPM:   LUIS VIACELLI FERNANDES - CPM BRAXIS - P.BCO*
      *    ANALISTA....:   ALTAIR ALEXANDRE        - STEFANINI  - GP.61*
      *    DATA........:   07/12/2007                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   INCLUIR O CAMPO BANCO NA CONDICAO DE BUSCA  *
      *      DE DADOS NA TABELA MESUB062.                              *
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
       01  FILLER                      PIC X(32)           VALUE
           '*  INICIO DA WORKING GFCT5109  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     VARIAVEIS AUXILIARES     *'.
      *----------------------------------------------------------------*

       01  WRK-MODULO                  PIC X(08)           VALUE SPACES.
       01  WRK-NOME-TAB                PIC X(11)           VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  AREA PARA MODULO GFCT5000   *'.
      *----------------------------------------------------------------*

       01  WRK-5000-AREA.
           03  WRK-5000-AREA-ENVIA.
               05  WRK-5000-AMBIENTE   PIC X(01)           VALUE SPACES.
               05  WRK-5000-COD-MSG    PIC 9(04)           VALUE ZEROS.
           03  WRK-5000-AREA-RECEBE.
               05  WRK-5000-COD-RETORNO
                                       PIC 9(02)           VALUE ZEROS.
               05  WRK-5000-COD-SQL-ERRO
                                       PIC 9(03)           VALUE ZEROS.
               05  WRK-5000-DESC-MSG   PIC X(70)           VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     AREA DE TABELAS DB2      *'.
      *----------------------------------------------------------------*

           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
                INCLUDE MESUB062
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*    FIM DA WORKING GFCT5109   *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY I#GFCTGB.
       COPY I#GFCTGC.
       COPY I#GFCT0M.

      *================================================================*
       PROCEDURE DIVISION              USING GFCTGB-ENTRADA
                                             GFCTGC-SAIDA
                                             GFCT0M-AREA-ERROS.
      *================================================================*

      *----------------------------------------------------------------*
       00000-ROTINA-PRINCIPAL          SECTION.
      *----------------------------------------------------------------*

           PERFORM 10000-INICIALIZAR.

           PERFORM 20000-PROCESSAR.

           PERFORM 30000-FINALIZAR.

      *----------------------------------------------------------------*
       00000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       10000-INICIALIZAR               SECTION.
      *----------------------------------------------------------------*

           PERFORM 11000-INICIALIZAR-SAIDA.

           PERFORM 12000-CONSISTIR-DADOS.

      *----------------------------------------------------------------*
       10000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       11000-INICIALIZAR-SAIDA         SECTION.
      *----------------------------------------------------------------*

           INITIALIZE GFCTGC-SAIDA
                      WRK-5000-AREA.

           MOVE 205                    TO GFCTGC-LL.
           MOVE GFCTGB-ZZ              TO GFCTGC-ZZ.
           MOVE GFCTGB-TRANSACAO       TO GFCTGC-TRANSACAO.
           MOVE GFCTGB-FUNCAO          TO GFCTGC-FUNCAO.

           IF  GFCTGB-QTDE-OCOR        NOT NUMERIC
               MOVE ZEROS              TO GFCTGC-QTDE-OCOR
           ELSE
               MOVE GFCTGB-QTDE-OCOR   TO GFCTGC-QTDE-OCOR
           END-IF.

           IF  GFCTGB-QTDE-TOT-REG     NOT NUMERIC
               MOVE ZEROS              TO GFCTGC-QTDE-TOT-REG
           ELSE
               MOVE GFCTGB-QTDE-TOT-REG
                                       TO GFCTGC-QTDE-TOT-REG
           END-IF.

           MOVE GFCTGB-FUNC-BDSCO      TO GFCTGC-FUNC-BDSCO.
           MOVE GFCTGB-TERMINAL        TO GFCTGC-TERMINAL.
           MOVE 'N'                    TO GFCTGC-FIM.

           PERFORM 11100-OBTER-DESC-MENSAGEM.

           MOVE WRK-5000-DESC-MSG      TO GFCTGC-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       11000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       11100-OBTER-DESC-MENSAGEM       SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-5000-AREA-RECEBE.

           MOVE 'O'                    TO WRK-5000-AMBIENTE.
           MOVE 'GFCT5000'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             WRK-5000-AREA-ENVIA
                                             WRK-5000-AREA-RECEBE.

           IF  WRK-5000-COD-RETORNO    NOT EQUAL ZEROS
               IF  WRK-5000-COD-RETORNO
                                       EQUAL 01
                   MOVE 1              TO GFCTGC-ERRO
                   MOVE ZEROS          TO GFCTGC-COD-SQL-ERRO
                                          GFCTGC-COD-MSG-ERRO
                   MOVE WRK-5000-DESC-MSG
                                       TO GFCTGC-DESC-MSG-ERRO
               END-IF
               IF  WRK-5000-COD-RETORNO
                                       EQUAL 99
                   MOVE 1              TO GFCTGC-ERRO
                   MOVE WRK-5000-COD-SQL-ERRO
                                       TO GFCTGC-COD-SQL-ERRO
                   MOVE ZEROS          TO GFCTGC-COD-MSG-ERRO
                   MOVE WRK-5000-DESC-MSG
                                       TO GFCTGC-DESC-MSG-ERRO
               END-IF
               INITIALIZE GFCTGC-DADOS
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       11100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       12000-CONSISTIR-DADOS           SECTION.
      *----------------------------------------------------------------*

           IF  GFCTGB-TRANSACAO        EQUAL SPACES  OR
               GFCTGB-FUNCAO           EQUAL SPACES  OR
               GFCTGB-QTDE-OCOR        NOT NUMERIC   OR
               GFCTGB-QTDE-TOT-REG     NOT NUMERIC   OR
               GFCTGB-FUNC-BDSCO       EQUAL SPACES  OR
               GFCTGB-COD-DEPDC        NOT NUMERIC   OR
               GFCTGB-COD-POSTO        NOT NUMERIC   OR
              (GFCTGB-FIM              NOT EQUAL 'S' AND 'N')
               MOVE 1                  TO GFCTGC-ERRO
               MOVE ZEROS              TO GFCTGC-COD-SQL-ERRO
               MOVE 0001               TO GFCTGC-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'S'                TO GFCTGC-FIM
               PERFORM 11100-OBTER-DESC-MENSAGEM
               MOVE WRK-5000-DESC-MSG  TO GFCTGC-DESC-MSG-ERRO
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       12000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       20000-PROCESSAR                 SECTION.
      *----------------------------------------------------------------*

           PERFORM 21000-PROCESSAR-DADOS.

           MOVE 'S'                    TO GFCTGC-FIM.

      *----------------------------------------------------------------*
       20000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       21000-PROCESSAR-DADOS           SECTION.
      *----------------------------------------------------------------*

           MOVE 237                    TO CEMPR-INC    OF MESUB062.
           MOVE GFCTGB-COD-DEPDC       TO CDEPDC       OF MESUB062.
           MOVE GFCTGB-COD-POSTO       TO CPOSTO-SERVC OF MESUB062.

           PERFORM 21100-LER-MESUB062.

           COMPUTE GFCTGC-LL           = GFCTGC-LL + 115.

           IF  SQLCODE                 EQUAL +100
               MOVE 1                  TO GFCTGC-ERRO
               MOVE SQLCODE            TO GFCTGC-COD-SQL-ERRO
               MOVE 0154               TO GFCTGC-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 11100-OBTER-DESC-MENSAGEM
               MOVE WRK-5000-DESC-MSG  TO GFCTGC-DESC-MSG-ERRO
               MOVE 1                  TO GFCTGC-ERRO-POSTO(01)
               MOVE GFCTGB-COD-POSTO   TO GFCTGC-COD-POSTO(01)
               MOVE SPACES             TO GFCTGC-DESC-POSTO(01)
               MOVE SQLCODE            TO GFCTGC-COD-SQL-ERRO-GEN(01)
               MOVE 0155               TO GFCTGC-COD-MSG-ERRO-GEN(01)
                                          WRK-5000-COD-MSG
               PERFORM 11100-OBTER-DESC-MENSAGEM
               MOVE WRK-5000-DESC-MSG  TO GFCTGC-DESC-MSG-ERRO-GEN(01)
           ELSE
               IF  CTPO-POSTO-SERVC OF MESUB062
                                       EQUAL 9
                   MOVE 1              TO GFCTGC-ERRO
                   MOVE SQLCODE        TO GFCTGC-COD-SQL-ERRO
                   MOVE 0594           TO GFCTGC-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   PERFORM 11100-OBTER-DESC-MENSAGEM
                   MOVE WRK-5000-DESC-MSG
                                       TO GFCTGC-DESC-MSG-ERRO
                   MOVE 1              TO GFCTGC-ERRO-POSTO(01)
                   MOVE GFCTGB-COD-POSTO
                                       TO GFCTGC-COD-POSTO(01)
                   MOVE SPACES         TO GFCTGC-DESC-POSTO(01)
                   MOVE SQLCODE        TO GFCTGC-COD-SQL-ERRO-GEN(01)
                   MOVE 0155           TO GFCTGC-COD-MSG-ERRO-GEN(01)
                                          WRK-5000-COD-MSG
                   PERFORM 11100-OBTER-DESC-MENSAGEM
                   MOVE WRK-5000-DESC-MSG
                                       TO GFCTGC-DESC-MSG-ERRO-GEN(01)
               ELSE
                   MOVE CPOSTO-SERVC OF MESUB062
                                       TO GFCTGC-COD-POSTO(01)
                   MOVE IPOSTO-SERVC OF MESUB062
                                       TO GFCTGC-DESC-POSTO(01)
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       21000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       21100-LER-MESUB062              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               SELECT CPOSTO_SERVC,
                      CTPO_POSTO_SERVC,
                      IPOSTO_SERVC
               INTO  :MESUB062.CPOSTO-SERVC,
                     :MESUB062.CTPO-POSTO-SERVC,
                     :MESUB062.IPOSTO-SERVC
               FROM  DB2PRD.POSTO_BCRIO_SERVC
               WHERE CEMPR_INC          = :MESUB062.CEMPR-INC
                 AND CDEPDC             = :MESUB062.CDEPDC
                 AND CPOSTO_SERVC       = :MESUB062.CPOSTO-SERVC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'GFCT5109'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'POSTO_BCRIO_SERVC'
                                       TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTGC-COD-SQL-ERRO
               MOVE '0001'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 9                  TO GFCTGC-ERRO
               MOVE 0012               TO GFCTGC-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 11100-OBTER-DESC-MENSAGEM
               MOVE ' - MESUB062'      TO WRK-NOME-TAB
               PERFORM 21110-CONCATENAR-DESCRICAO
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       21100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       21110-CONCATENAR-DESCRICAO      SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTGC-DESC-MSG-ERRO.

           STRING WRK-5000-DESC-MSG
                  WRK-NOME-TAB
           DELIMITED BY '   '          INTO GFCTGC-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       21110-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       30000-FINALIZAR                 SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       30000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*
