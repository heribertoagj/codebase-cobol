      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT5209.
       AUTHOR.     PROCWORK OUTSOURCING.
      *================================================================*
      *                   C P M   S I S T E M A S                      *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : GFCT5209                                     *
      *    PROGRAMADOR  : PROCWORK OUTSOURCING                         *
      *    ANALISTA     : MARCUS VINICIUS - PROCWORK    - GRUPO 50     *
      *    DATA         : 26/08/2008                                   *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      GERAR DADOS PARA A TEXT DE CORRESPONDENTE BANCARIO.       *
      *      IDENTIFICANDO SE BANCO POSTAL OU BRADESCO EXPRESSO.       *
      *                                                                *
      *    BANCO DE DADOS:                                             *
      *      DB2                                                       *
      *       TABLE                                INCLUDE/BOOK        *
      *       DB2PRD.POSTO_BCRIO_SERVC               MESUB062          *
      *       DB2PRD.TCAD_CORSP_BCRIO                YMKCB001          *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      GFCT5000 - OBTER DESCRICAO DE MENSAGENS.                  *
      *                                                                *
      *================================================================*
      *                                                                *
      *    ANALISTA....:  CIBELE          - SONDA IT                   *
      *    DATA........:  05 / 2014                                    *
      *    OBJETIVO....:  SUBSTITUIR O ACESSO A BASE YMKCB001          *
      *                   PELO MODULO KCIM0184.                        *
      *                                                                *
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
           '*  INICIO DA WORKING GFCT5209  *'.
      *----------------------------------------------------------------*

       COPY KCIMW184.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     VARIAVEIS AUXILIARES     *'.
      *----------------------------------------------------------------*

       01  WRK-MODULO                  PIC X(08)           VALUE SPACES.
       01  WRK-NOME-TAB                PIC X(11)           VALUE SPACES.

       01  WRK-SQL-CODE-N              PIC S9(09)   VALUE ZEROES.
       01  WRK-SQL-CODE-R  REDEFINES  WRK-SQL-CODE-N.
           03  FILLER                  PIC  9(06).
           03  WRK-SQL-CODE            PIC  9(03).

       01  WRK-CPAB-9-05             PIC  9(005)         VALUE ZEROS.
       01  WRK-FILLER                REDEFINES WRK-CPAB-9-05.
           05 FILLER                 PIC  9(02).
           05 WRK-CPAB-9-03          PIC  9(03).

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
           '*    FIM DA WORKING GFCT5209   *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY GFCTWABC.
       COPY GFCTWABD.
       COPY I#GFCT0M.

      *================================================================*
       PROCEDURE DIVISION              USING WABC-ENTRADA
                                             WABD-SAIDA
                                             GFCT0M-AREA-ERROS.
      *================================================================*

      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL          SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-PROCESSAR.

           PERFORM 9000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-INICIALIZAR               SECTION.
      *----------------------------------------------------------------*

           PERFORM 1100-INICIALIZAR-SAIDA.

           PERFORM 1200-CONSISTIR-DADOS.

      *----------------------------------------------------------------*
       1000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1100-INICIALIZAR-SAIDA         SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WABD-SAIDA
                      WRK-5000-AREA.

           MOVE 208                    TO WABD-LL.
           MOVE WABC-ZZ                TO WABD-ZZ.
           MOVE WABC-TRANSACAO         TO WABD-TRANSACAO.
           MOVE WABC-FUNCAO            TO WABD-FUNCAO.

           IF  WABC-QTDE-OCOR        NOT NUMERIC
               MOVE ZEROS              TO WABD-QTDE-OCOR
           ELSE
               MOVE WABC-QTDE-OCOR     TO WABD-QTDE-OCOR
           END-IF.

           IF  WABC-QTDE-TOT-REG     NOT NUMERIC
               MOVE ZEROS              TO WABD-QTDE-TOT-REG
           ELSE
               MOVE WABC-QTDE-TOT-REG
                                       TO WABD-QTDE-TOT-REG
           END-IF.

           MOVE WABC-FUNC-BDSCO        TO WABD-FUNC-BDSCO.
           MOVE WABC-TERMINAL          TO WABD-TERMINAL.
           MOVE 'N'                    TO WABD-FIM.

           PERFORM 1110-OBTER-DESC-MENSAGEM.

           MOVE WRK-5000-DESC-MSG      TO WABD-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       1100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1110-OBTER-DESC-MENSAGEM       SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-5000-AREA-RECEBE.

           MOVE 'O'                    TO WRK-5000-AMBIENTE.
           MOVE 'GFCT5000'             TO WRK-MODULO.

           CALL  WRK-MODULO            USING WRK-5000-AREA-ENVIA
                                             WRK-5000-AREA-RECEBE.

           IF  WRK-5000-COD-RETORNO    NOT EQUAL ZEROS
               IF  WRK-5000-COD-RETORNO
                                       EQUAL 01
                   MOVE 1              TO WABD-ERRO
                   MOVE ZEROS          TO WABD-COD-SQL-ERRO
                                          WABD-COD-MSG-ERRO
                   MOVE WRK-5000-DESC-MSG
                                       TO WABD-DESC-MSG-ERRO
               END-IF
               IF  WRK-5000-COD-RETORNO
                                       EQUAL 99
                   MOVE 1              TO WABD-ERRO
                   MOVE WRK-5000-COD-SQL-ERRO
                                       TO WABD-COD-SQL-ERRO
                   MOVE ZEROS          TO WABD-COD-MSG-ERRO
                   MOVE WRK-5000-DESC-MSG
                                       TO WABD-DESC-MSG-ERRO
               END-IF
               INITIALIZE WABD-DADOS
               PERFORM 9000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1200-CONSISTIR-DADOS           SECTION.
      *----------------------------------------------------------------*

           IF  WABC-TRANSACAO        EQUAL SPACES  OR
               WABC-FUNCAO           EQUAL SPACES  OR
               WABC-QTDE-OCOR        NOT NUMERIC   OR
               WABC-QTDE-TOT-REG     NOT NUMERIC   OR
               WABC-FUNC-BDSCO       EQUAL SPACES  OR
               WABC-COD-DEPDC        NOT NUMERIC   OR
               WABC-COD-POSTO        NOT NUMERIC   OR
               WABC-COD-AGRUP        NOT NUMERIC   OR
              (WABC-COD-AGRUP        NOT EQUAL 17 AND 20)  OR
              (WABC-FIM              NOT EQUAL 'S' AND 'N')
               MOVE 1                  TO WABD-ERRO
               MOVE ZEROS              TO WABD-COD-SQL-ERRO
               MOVE 0001               TO WABD-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'S'                TO WABD-FIM
               PERFORM 1110-OBTER-DESC-MENSAGEM
               MOVE WRK-5000-DESC-MSG  TO WABD-DESC-MSG-ERRO
               PERFORM 9000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1200-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000-PROCESSAR                 SECTION.
      *----------------------------------------------------------------*

           IF  WABC-COD-AGRUP          EQUAL 17
               PERFORM 3000-PROCESSAR-POSTAL
           ELSE
               PERFORM 4000-PROCESSAR-EXPRES
           END-IF.

           MOVE 'S'                    TO WABD-FIM.

      *----------------------------------------------------------------*
       2000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3000-PROCESSAR-POSTAL          SECTION.
      *----------------------------------------------------------------*

           MOVE 237                    TO CEMPR-INC    OF MESUB062.
           MOVE WABC-COD-DEPDC         TO CDEPDC       OF MESUB062.
           MOVE WABC-COD-POSTO         TO CPOSTO-SERVC OF MESUB062.

           PERFORM 5000-LER-MESUB062.

           COMPUTE WABD-LL           = WABD-LL + 115.

           IF  SQLCODE                 EQUAL +100
               MOVE 1                  TO WABD-ERRO
               MOVE SQLCODE            TO WRK-SQL-CODE-N
               MOVE WRK-SQL-CODE       TO WABD-COD-SQL-ERRO
               MOVE 1491               TO WABD-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 1110-OBTER-DESC-MENSAGEM
               MOVE WRK-5000-DESC-MSG  TO WABD-DESC-MSG-ERRO
               MOVE 1                  TO WABD-ERRO-POSTO(01)
               MOVE WABC-COD-POSTO     TO WABD-COD-POSTO(01)
               MOVE SPACES             TO WABD-DESC-POSTO(01)
               MOVE SQLCODE            TO WRK-SQL-CODE-N
               MOVE WRK-SQL-CODE       TO WABD-COD-SQL-ERRO-GEN(01)
               MOVE 0592               TO WABD-COD-MSG-ERRO-GEN(01)
                                          WRK-5000-COD-MSG
               PERFORM 1110-OBTER-DESC-MENSAGEM
               MOVE WRK-5000-DESC-MSG  TO WABD-DESC-MSG-ERRO-GEN(01)
           ELSE
               IF  CTPO-POSTO-SERVC OF MESUB062
                                       NOT EQUAL 9
                   MOVE 1              TO WABD-ERRO
                   MOVE SQLCODE        TO WRK-SQL-CODE-N
                   MOVE WRK-SQL-CODE   TO WABD-COD-SQL-ERRO
                   MOVE 0521           TO WABD-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   PERFORM 1110-OBTER-DESC-MENSAGEM
                   MOVE WRK-5000-DESC-MSG
                                       TO WABD-DESC-MSG-ERRO
                   MOVE 1              TO WABD-ERRO-POSTO(01)
                   MOVE WABC-COD-POSTO
                                       TO WABD-COD-POSTO(01)
                   MOVE SPACES         TO WABD-DESC-POSTO(01)
                   MOVE SQLCODE        TO WRK-SQL-CODE-N
                   MOVE WRK-SQL-CODE   TO WABD-COD-SQL-ERRO-GEN(01)
                   MOVE 0592           TO WABD-COD-MSG-ERRO-GEN(01)
                                          WRK-5000-COD-MSG
                   PERFORM 1110-OBTER-DESC-MENSAGEM
                   MOVE WRK-5000-DESC-MSG
                                       TO WABD-DESC-MSG-ERRO-GEN(01)
               ELSE
                   INITIALIZE KCIMW184-AREA-COMUNICACAO
                   MOVE 01             TO KCIMW184-E-FUNCAO
                   MOVE WABC-COD-DEPDC TO KCIMW184-E-CAG-BCRIA-1
                   MOVE WABC-COD-POSTO TO WRK-CPAB-9-05
                   MOVE WRK-CPAB-9-03  TO KCIMW184-E-CPOSTO-BCRIO-1

                   PERFORM 6000-LER-YMKCB001

                   PERFORM 3100-TRATAR-POSTAL
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3100-TRATAR-POSTAL             SECTION.
      *----------------------------------------------------------------*

           IF  KCIMW184-S-COD-RETORNO EQUAL 98
               MOVE 1                  TO WABD-ERRO
               MOVE SQLCODE            TO WRK-SQL-CODE-N
               MOVE WRK-SQL-CODE       TO WABD-COD-SQL-ERRO
               MOVE 1491               TO WABD-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 1110-OBTER-DESC-MENSAGEM
               MOVE WRK-5000-DESC-MSG  TO WABD-DESC-MSG-ERRO
               MOVE 1                  TO WABD-ERRO-POSTO(01)
               MOVE WABC-COD-POSTO     TO WABD-COD-POSTO(01)
               MOVE SPACES             TO WABD-DESC-POSTO(01)
               MOVE SQLCODE            TO WRK-SQL-CODE-N
               MOVE WRK-SQL-CODE       TO WABD-COD-SQL-ERRO-GEN(01)
               MOVE 0592               TO WABD-COD-MSG-ERRO-GEN(01)
                                          WRK-5000-COD-MSG
               PERFORM 1110-OBTER-DESC-MENSAGEM
               MOVE WRK-5000-DESC-MSG  TO WABD-DESC-MSG-ERRO-GEN(01)
           ELSE
               IF  KCIMW184-S-CEMPR-CORSP(1)   NOT EQUAL 10000
                   MOVE 1              TO WABD-ERRO
                   MOVE SQLCODE        TO WRK-SQL-CODE-N
                   MOVE WRK-SQL-CODE   TO WABD-COD-SQL-ERRO
                   MOVE 0521           TO WABD-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   PERFORM 1110-OBTER-DESC-MENSAGEM
                   MOVE WRK-5000-DESC-MSG
                                       TO WABD-DESC-MSG-ERRO
                   MOVE 1              TO WABD-ERRO-POSTO(01)
                   MOVE WABC-COD-POSTO
                                       TO WABD-COD-POSTO(01)
                   MOVE SPACES         TO WABD-DESC-POSTO(01)
                   MOVE SQLCODE        TO WRK-SQL-CODE-N
                   MOVE WRK-SQL-CODE   TO WABD-COD-SQL-ERRO-GEN(01)
                   MOVE 0592           TO WABD-COD-MSG-ERRO-GEN(01)
                                          WRK-5000-COD-MSG
                   PERFORM 1110-OBTER-DESC-MENSAGEM
                   MOVE WRK-5000-DESC-MSG
                                       TO WABD-DESC-MSG-ERRO-GEN(01)
               ELSE
                   MOVE CPOSTO-SERVC OF MESUB062
                                       TO WABD-COD-POSTO(01)
                   MOVE IPOSTO-SERVC OF MESUB062
                                       TO WABD-DESC-POSTO(01)
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4000-PROCESSAR-EXPRES          SECTION.
      *----------------------------------------------------------------*

           MOVE 237                    TO CEMPR-INC    OF MESUB062.
           MOVE WABC-COD-DEPDC         TO CDEPDC       OF MESUB062.
           MOVE WABC-COD-POSTO         TO CPOSTO-SERVC OF MESUB062.

           PERFORM 5000-LER-MESUB062.

           COMPUTE WABD-LL           = WABD-LL + 115.

           IF  SQLCODE                 EQUAL +100
               MOVE 1                  TO WABD-ERRO
               MOVE SQLCODE            TO WRK-SQL-CODE-N
               MOVE WRK-SQL-CODE       TO WABD-COD-SQL-ERRO
               MOVE 1480               TO WABD-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 1110-OBTER-DESC-MENSAGEM
               MOVE WRK-5000-DESC-MSG  TO WABD-DESC-MSG-ERRO
               MOVE 1                  TO WABD-ERRO-POSTO(01)
               MOVE WABC-COD-POSTO     TO WABD-COD-POSTO(01)
               MOVE SPACES             TO WABD-DESC-POSTO(01)
               MOVE SQLCODE            TO WRK-SQL-CODE-N
               MOVE WRK-SQL-CODE       TO WABD-COD-SQL-ERRO-GEN(01)
               MOVE 0592               TO WABD-COD-MSG-ERRO-GEN(01)
                                          WRK-5000-COD-MSG
               PERFORM 1110-OBTER-DESC-MENSAGEM
               MOVE WRK-5000-DESC-MSG  TO WABD-DESC-MSG-ERRO-GEN(01)
           ELSE
               IF  CTPO-POSTO-SERVC OF MESUB062
                                       NOT EQUAL 9
                   MOVE 1              TO WABD-ERRO
                   MOVE SQLCODE        TO WRK-SQL-CODE-N
                   MOVE WRK-SQL-CODE   TO WABD-COD-SQL-ERRO
                   MOVE 1480           TO WABD-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   PERFORM 1110-OBTER-DESC-MENSAGEM
                   MOVE WRK-5000-DESC-MSG
                                       TO WABD-DESC-MSG-ERRO
                   MOVE 1              TO WABD-ERRO-POSTO(01)
                   MOVE WABC-COD-POSTO
                                       TO WABD-COD-POSTO(01)
                   MOVE SPACES         TO WABD-DESC-POSTO(01)
                   MOVE SQLCODE        TO WRK-SQL-CODE-N
                   MOVE WRK-SQL-CODE   TO WABD-COD-SQL-ERRO-GEN(01)
                   MOVE 0592           TO WABD-COD-MSG-ERRO-GEN(01)
                                          WRK-5000-COD-MSG
                   PERFORM 1110-OBTER-DESC-MENSAGEM
                   MOVE WRK-5000-DESC-MSG
                                       TO WABD-DESC-MSG-ERRO-GEN(01)
               ELSE
                   INITIALIZE KCIMW184-AREA-COMUNICACAO
                   MOVE 01             TO KCIMW184-E-FUNCAO
                   MOVE WABC-COD-DEPDC TO KCIMW184-E-CAG-BCRIA-1
                   MOVE WABC-COD-POSTO TO WRK-CPAB-9-05
                   MOVE WRK-CPAB-9-03  TO KCIMW184-E-CPOSTO-BCRIO-1

                   PERFORM 6000-LER-YMKCB001

                   PERFORM 4100-TRATAR-EXPRES
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       4000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4100-TRATAR-EXPRES             SECTION.
      *----------------------------------------------------------------*

           IF  SQLCODE                 EQUAL +100
               MOVE 1                  TO WABD-ERRO
               MOVE SQLCODE            TO WRK-SQL-CODE-N
               MOVE WRK-SQL-CODE       TO WABD-COD-SQL-ERRO
               MOVE 1480               TO WABD-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 1110-OBTER-DESC-MENSAGEM
               MOVE WRK-5000-DESC-MSG  TO WABD-DESC-MSG-ERRO
               MOVE 1                  TO WABD-ERRO-POSTO(01)
               MOVE WABC-COD-POSTO     TO WABD-COD-POSTO(01)
               MOVE SPACES             TO WABD-DESC-POSTO(01)
               MOVE SQLCODE            TO WRK-SQL-CODE-N
               MOVE WRK-SQL-CODE       TO WABD-COD-SQL-ERRO-GEN(01)
               MOVE 0592               TO WABD-COD-MSG-ERRO-GEN(01)
                                          WRK-5000-COD-MSG
               PERFORM 1110-OBTER-DESC-MENSAGEM
               MOVE WRK-5000-DESC-MSG  TO WABD-DESC-MSG-ERRO-GEN(01)
           ELSE
               IF  KCIMW184-S-CEMPR-CORSP(1)
                                       EQUAL 10000
                   MOVE 1              TO WABD-ERRO
                   MOVE SQLCODE        TO WRK-SQL-CODE-N
                   MOVE WRK-SQL-CODE   TO WABD-COD-SQL-ERRO
                   MOVE 1480           TO WABD-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   PERFORM 1110-OBTER-DESC-MENSAGEM
                   MOVE WRK-5000-DESC-MSG
                                       TO WABD-DESC-MSG-ERRO
                   MOVE 1              TO WABD-ERRO-POSTO(01)
                   MOVE WABC-COD-POSTO
                                       TO WABD-COD-POSTO(01)
                   MOVE SPACES         TO WABD-DESC-POSTO(01)
                   MOVE SQLCODE        TO WRK-SQL-CODE-N
                   MOVE WRK-SQL-CODE   TO WABD-COD-SQL-ERRO-GEN(01)
                   MOVE 0592           TO WABD-COD-MSG-ERRO-GEN(01)
                                          WRK-5000-COD-MSG
                   PERFORM 1110-OBTER-DESC-MENSAGEM
                   MOVE WRK-5000-DESC-MSG
                                       TO WABD-DESC-MSG-ERRO-GEN(01)
               ELSE
                   MOVE CPOSTO-SERVC OF MESUB062
                                       TO WABD-COD-POSTO(01)
                   MOVE IPOSTO-SERVC OF MESUB062
                                       TO WABD-DESC-POSTO(01)
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       4100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5000-LER-MESUB062              SECTION.
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
               MOVE 'GFCT5209'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'POSTO_BCRIO_SERVC'
                                       TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO WRK-SQL-CODE-N
               MOVE WRK-SQL-CODE       TO WABD-COD-SQL-ERRO
               MOVE SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0010'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 9                  TO WABD-ERRO
               MOVE 0012               TO WABD-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 1110-OBTER-DESC-MENSAGEM
               MOVE ' - MESUB062'      TO WRK-NOME-TAB
               PERFORM 8000-CONCATENAR-DESCRICAO
               PERFORM 9000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       6000-LER-YMKCB001              SECTION.
      *----------------------------------------------------------------*

           MOVE 'KCIM0184'             TO WRK-MODULO.

           CALL WRK-MODULO             USING KCIMW184-AREA-COMUNICACAO.

           IF KCIMW184-S-COD-RETORNO  NOT EQUAL 00 AND 98
               MOVE 1                  TO WABD-ERRO
               MOVE ZEROS              TO WABD-COD-SQL-ERRO
               MOVE 1875               TO WABD-COD-MSG-ERRO
               MOVE 1875             TO WRK-5000-COD-MSG
               PERFORM 1110-OBTER-DESC-MENSAGEM
               MOVE WRK-5000-DESC-MSG  TO WABD-DESC-MSG-ERRO
               PERFORM 9000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       6000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       8000-CONCATENAR-DESCRICAO      SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO WABD-DESC-MSG-ERRO.

           STRING WRK-5000-DESC-MSG
                  WRK-NOME-TAB
           DELIMITED BY '   '          INTO WABD-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       8000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9000-FINALIZAR                 SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       9000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*
