      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT5093.
       AUTHOR. MIRELLI VENTURA.
      *================================================================*
      *                   C P M   S I S T E M A S                      *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : GFCT5093                                     *
      *    PROGRAMADORA : MIRELLI VENTURA               - CPM/FPOLIS   *
      *    ANALISTA CPM : DANIELE GOMES                 - CPM/FPOLIS   *
      *    ANALISTA     : RICARDO PEREIRA - PROCWORK    - GRUPO 50     *
      *    DATA         : 04/11/2005                                   *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      GERAR DADOS PARA COMBO DE TARIFA DO AGRUPAMENTO -         *
      *      TABELA GFCTB0A7.                                          *
      *                                                                *
      *    BANCO DE DADOS:                                             *
      *      DB2                                                       *
      *       TABLE                                INCLUDE/BOOK        *
      *       DB2PRD.PDIDO_ESTRN_GRP                 GFCTB0A7          *
      *       DB2PRD.SERVC_TARIF_PRINC               GFCTB0D8          *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      GFCT5507 - VERIFICA MASTER                                *
      *      GFCT5522 - VERIFICAR ON-LINE                              *
      *      GFCT5523 - OBTER DESCRICAO DE MENSAGENS                   *
      *      POOL0081 - ALOCAR DINAMICAMENTE UM MODULO                 *
      *      POOL7600 - OBTER DATA E HORA DO SISTEMA                   *
      *      SENH0315 - OBTER JUNCAO.                                  *
0410  *      GFCT5558 - OBTER DEPENDENCIA GESTOR MASTER                *
      *                                                                *
      *    NAVEGACAO                                                   *
      *      CHAMADO POR: GFCT7381                                     *
      *                   GFCT7385                                     *
      *                                                                *
      *----------------------------------------------------------------*
0410  *    ULTIMA ALTERACAO EM 09/04/2010 - HELIO SANTONI NETO         *
0410  *  - INCLUSAO DO DEPARTAMENTO AUTORIZADO                         *
0410  *  - SUBSTITUICAO DO MODULO GFCT5507 PELO GFCT5558 PARA DEPTO    *
0410  *    AUTORIZADO.                                                 *
0410  *                                                                *
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
           '*  INICIO DA WORKING GFCT5093  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC X(32)           VALUE
           '*         INDEXADORES          *'.
      *----------------------------------------------------------------*

       77  IND                         PIC 9(03)           VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC X(32)           VALUE
           '*     VARIAVEIS AUXILIARES     *'.
      *----------------------------------------------------------------*

       77  WRK-FINALIZAR               PIC X(01)           VALUE SPACES.
       77  WRK-MODULO                  PIC X(08)           VALUE SPACES.
       77  WRK-TABELA                  PIC X(08)           VALUE SPACES.

       01  WRK-PONTEIRO-AUX.
           03  WRK-TARIFA-AUX          PIC 9(05)           VALUE ZEROS.

       01  WRK-CSIT-SOLTC-INI          PIC S9(02)V COMP-3  VALUE ZEROS.
       01  WRK-CSIT-SOLTC-FIM          PIC S9(02)V COMP-3  VALUE ZEROS.

       01  WRK-CSIT-PROCM-INI          PIC S9(01)V COMP-3  VALUE ZEROS.
       01  WRK-CSIT-PROCM-FIM          PIC S9(01)V COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     AREA DE MENSAGENS        *'.
      *----------------------------------------------------------------*

       01  WRK-MSG05.
           03  FILLER                  PIC X(26)           VALUE
               'ERRO NA CHAMADA DO MODULO '.
           03  WRK-NOME-MOD            PIC X(08)           VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*   AREA DO MODULO SENH0315    *'.
      *----------------------------------------------------------------*

       01  WRK-VERSAO-0315             PIC X(06)           VALUE SPACES.
       01  WRK-MENSAGEM-0315.
           03  WRK-COD-MSG-0315        PIC 9(03)           VALUE ZEROS.
           03  WRK-HIFEN-MENSAGEM-0315 PIC X(01)           VALUE SPACES.
           03  WRK-TEXTO-MENSAGEM-0315 PIC X(79)           VALUE SPACES.
       01  WRK-COD-FUNCIONAL-0315      PIC X(07)           VALUE SPACES.
       01  WRK-NOME-FUNC-0315          PIC X(40)           VALUE SPACES.
       01  WRK-COD-BANCO-0315          PIC 9(05)           VALUE ZEROS.
       01  WRK-NOME-BANCO-0315         PIC X(40)           VALUE SPACES.
       01  WRK-CNPJ-BANCO-0315.
ST25X6*     03  WRK-CNPJ-PRI-0315       PIC 9(09)           VALUE ZEROS.
ST25X6*     03  WRK-CNPJ-FIL-0315       PIC 9(05)           VALUE ZEROS.
ST25X6*     03  WRK-CNPJ-CON-0315       PIC 9(02)           VALUE ZEROS.
           03  WRK-CNPJ-PRI-0315       PIC X(09)           VALUE SPACES.
           03  WRK-CNPJ-FIL-0315       PIC X(04)           VALUE SPACES.
           03  WRK-CNPJ-CON-0315       PIC X(02)           VALUE SPACES.
       01  WRK-COD-JUNCAO-0315         PIC 9(05)           VALUE ZEROS.
       01  WRK-NOME-DEPTO-0315         PIC X(40)           VALUE SPACES.
       01  WRK-SECAO-0315              PIC 9(03)           VALUE ZEROS.
       01  WRK-STATUS-0315             PIC X(01)           VALUE SPACES.

0410  *----------------------------------------------------------------*
0410   01  FILLER                      PIC  X(32)          VALUE
0410        '*    AREA DO MODULO GFCT5558   *'.
0410  *----------------------------------------------------------------*
0410
0410   COPY I#GFCTF9.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  AREA PARA MODULO GFCT5522   *'.
      *----------------------------------------------------------------*

       COPY I#GFCTFZ.
       COPY I#GFCTG1.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  AREA PARA MODULO GFCT5523   *'.
      *----------------------------------------------------------------*

       COPY I#GFCTG2.
       COPY I#GFCTG3.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     AREAS DE TABELAS DB2     *'.
      *----------------------------------------------------------------*

           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0A7
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0D8
           END-EXEC.

           EXEC SQL
               DECLARE CSR-GFCTB0A7 CURSOR FOR
               SELECT  DISTINCT CSERVC_TARIF
               FROM    DB2PRD.PDIDO_ESTRN_GRP
               WHERE  (CSIT_SOLTC_ESTRN >= :WRK-CSIT-SOLTC-INI
                 AND   CSIT_SOLTC_ESTRN <= :WRK-CSIT-SOLTC-FIM)
                 AND  (CSIT_PROCM_ESTRN >= :WRK-CSIT-PROCM-INI
                 AND   CSIT_PROCM_ESTRN <= :WRK-CSIT-PROCM-FIM)
                 AND   CAGPTO_CTA        = :GFCTB0A7.CAGPTO-CTA
                 AND   CSERVC_TARIF     >= :GFCTB0A7.CSERVC-TARIF
               ORDER   BY CSERVC_TARIF
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*    FIM DA WORKING GFCT5093   *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY I#GFCTGT.
       COPY I#GFCTGU.
       COPY I#GFCT0M.

      *================================================================*
       PROCEDURE DIVISION              USING GFCTGT-ENTRADA
                                             GFCTGU-SAIDA
                                             GFCT0M-AREA-ERROS.
      *================================================================*

      *----------------------------------------------------------------*
       00000-ROTINA-PRINCIPAL          SECTION.
      *----------------------------------------------------------------*

           PERFORM 10000-INICIALIZAR-PROGRAMA.

           PERFORM 20000-PROCESSAR-DADOS.

           PERFORM 30000-FINALIZAR-PROGRAMA.

      *----------------------------------------------------------------*
       00000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       10000-INICIALIZAR-PROGRAMA      SECTION.
      *----------------------------------------------------------------*

           PERFORM 11000-INICIALIZAR-SAIDA.

           PERFORM 12000-CONSISTIR-DADOS.

           PERFORM 13000-VERIFICAR-ONLINE.

      *----------------------------------------------------------------*
       10000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       11000-INICIALIZAR-SAIDA         SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTGU-SAIDA.

           INITIALIZE                  GFCTGU-SAIDA.

           MOVE 170                    TO GFCTGU-LL.
           MOVE GFCTGT-ZZ              TO GFCTGU-ZZ.
           MOVE GFCTGT-TRANSACAO       TO GFCTGU-TRANSACAO.
           MOVE GFCTGT-FUNCAO          TO GFCTGU-FUNCAO.
           MOVE GFCTGT-FUNC-BDSCO      TO GFCTGU-FUNC-BDSCO.
           MOVE GFCTGT-TERMINAL        TO GFCTGU-TERMINAL.
           MOVE ZEROS                  TO GFCTGU-ERRO
                                          GFCTGU-COD-SQL-ERRO
                                          GFCTGU-COD-MSG-ERRO
                                          GFCTG2-COD-MSG.
           MOVE 'N'                    TO GFCTGU-FIM.

           IF  GFCTGT-QTDE-OCOR        IS NUMERIC
               MOVE GFCTGT-QTDE-OCOR   TO GFCTGU-QTDE-OCOR
           END-IF.

           IF  GFCTGT-QTDE-TOT-REG     IS NUMERIC
               MOVE GFCTGT-QTDE-TOT-REG
                                       TO GFCTGU-QTDE-TOT-REG
           END-IF.

           IF  GFCTGT-PONTEIRO-TARIFA  IS NUMERIC
               MOVE GFCTGT-PONTEIRO-TARIFA
                                       TO GFCTGU-PONTEIRO-TARIFA
           END-IF.

           IF  GFCTGT-COD-AGRUPAMENTO  IS NUMERIC
               MOVE GFCTGT-COD-AGRUPAMENTO
                                       TO GFCTGU-COD-AGRUPAMENTO
           END-IF.

           PERFORM 11100-OBTER-DESC-MSG.

           MOVE GFCTG3-DESC-MSG        TO GFCTGU-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       11000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       11100-OBTER-DESC-MSG            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTGT-LL              TO GFCTG2-LL.
           MOVE GFCTGT-ZZ              TO GFCTG2-ZZ.
           MOVE GFCTGT-TRANSACAO       TO GFCTG2-TRANSACAO.
           MOVE GFCTGT-FUNCAO          TO GFCTG2-FUNCAO.
           MOVE GFCTGT-FUNC-BDSCO      TO GFCTG2-FUNC-BDSCO.
           MOVE GFCTGT-FIM             TO GFCTG2-FIM.
           MOVE SPACES                 TO GFCTG3-SAIDA.

           INITIALIZE                  GFCTG3-SAIDA.

           MOVE 'O'                    TO GFCTG2-TIPO-PROC.
           MOVE 'GFCT5523'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTG2-ENTRADA
                                             GFCTG3-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-NOME-MOD
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5093'         TO GFCT0M-TRANSACAO
               MOVE 9                  TO GFCTGU-ERRO
               PERFORM 30000-FINALIZAR-PROGRAMA
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 9
               IF  GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2'
                   MOVE WRK-MODULO     TO GFCT0M-PROGRAMA
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
                   MOVE 'GFCT5093'     TO GFCT0M-TRANSACAO
               END-IF
               MOVE 9                  TO GFCTGU-ERRO
               PERFORM 30000-FINALIZAR-PROGRAMA
           END-IF.

      *----------------------------------------------------------------*
       11100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       12000-CONSISTIR-DADOS           SECTION.
      *----------------------------------------------------------------*

           IF (GFCTGT-TRANSACAO        EQUAL SPACES)          OR
              (GFCTGT-FUNCAO           EQUAL SPACES)          OR
              (GFCTGT-COD-AGRUPAMENTO  NOT NUMERIC)           OR
              (GFCTGT-QTDE-OCOR        NOT NUMERIC)           OR
              (GFCTGT-QTDE-TOT-REG     NOT NUMERIC)           OR
              (GFCTGT-FUNC-BDSCO       EQUAL SPACES)          OR
              (GFCTGT-PONTEIRO-TARIFA  NOT NUMERIC)           OR
              (GFCTGT-FIM              NOT EQUAL 'S' AND 'N') OR
              (GFCTGT-TIPO-FUNCAO      NOT EQUAL 1   AND 2)
               MOVE 1                  TO GFCTGU-ERRO
               MOVE ZEROS              TO GFCTGU-COD-SQL-ERRO
               MOVE 0001               TO GFCTGU-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               MOVE 'S'                TO GFCTGU-FIM
               PERFORM 11100-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTGU-DESC-MSG-ERRO
               PERFORM 30000-FINALIZAR-PROGRAMA
           END-IF.

      *----------------------------------------------------------------*
       12000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       13000-VERIFICAR-ONLINE          SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTGT-LL              TO GFCTFZ-LL.
           MOVE GFCTGT-ZZ              TO GFCTFZ-ZZ.
           MOVE GFCTGT-TRANSACAO       TO GFCTFZ-TRANSACAO.
           MOVE GFCTGT-FUNCAO          TO GFCTFZ-FUNCAO.
           MOVE GFCTGT-FUNC-BDSCO      TO GFCTFZ-FUNC-BDSCO.
           MOVE GFCTGT-FIM             TO GFCTFZ-FIM.
           MOVE 'GFCT'                 TO GFCTFZ-SISTEMA.
           MOVE SPACES                 TO GFCTG1-SAIDA.

           INITIALIZE                  GFCTG1-SAIDA.

           MOVE 'GFCT5522'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTFZ-ENTRADA
                                             GFCTG1-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-NOME-MOD
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5093'         TO GFCT0M-TRANSACAO
               MOVE 9                  TO GFCTGU-ERRO
               PERFORM 30000-FINALIZAR-PROGRAMA
           END-IF.

           IF  GFCTG1-ERRO             EQUAL 9
               IF  GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2'
                   MOVE WRK-MODULO     TO GFCT0M-PROGRAMA
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
                   MOVE 'GFCT5093'     TO GFCT0M-TRANSACAO
               END-IF
               MOVE 9                  TO GFCTGU-ERRO
               PERFORM 30000-FINALIZAR-PROGRAMA
           END-IF.

           IF  GFCTG1-ERRO             EQUAL 1
               MOVE 'S'                TO GFCTGU-FIM
               MOVE 1                  TO GFCTGU-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5093'         TO GFCT0M-TRANSACAO
               MOVE GFCTG1-DESC-MSG-ERRO
                                       TO GFCTGU-DESC-MSG-ERRO
               PERFORM 30000-FINALIZAR-PROGRAMA
           END-IF.

           IF  GFCTG1-CSIT-ONLINE      EQUAL 1
               MOVE 'SISTEMA INDISPONIVEL'
                                       TO GFCT0M-TEXTO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5093'         TO GFCT0M-TRANSACAO
               MOVE 9                  TO GFCTGU-ERRO
               PERFORM 30000-FINALIZAR-PROGRAMA
           END-IF.

      *----------------------------------------------------------------*
       13000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       20000-PROCESSAR-DADOS           SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTGT-PONTEIRO        TO WRK-TARIFA-AUX.
           MOVE ZEROS                  TO IND.

           IF  GFCTGT-TIPO-FUNCAO      EQUAL  1
               PERFORM 21000-TRATAR-CSR-GFCTB0A7
           ELSE
               PERFORM 22000-OBTER-COD-JUNCAO
               PERFORM 23000-VERIFICA-MASTER
0410           IF  GFCTF9-TIPO-DEPENDENCIA
0410                                   EQUAL 'GM' OR 'DA'
                   PERFORM 21000-TRATAR-CSR-GFCTB0A7
               ELSE
                   PERFORM 21100-ABRIR-CSR-GFCTB0A7
                   PERFORM 25000-TRATAR-REG-JUNCAO
                       UNTIL WRK-FINALIZAR EQUAL 'S'
                   PERFORM 21400-FECHAR-CSR-GFCTB0A7
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       20000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       21000-TRATAR-CSR-GFCTB0A7       SECTION.
      *----------------------------------------------------------------*

           PERFORM 21100-ABRIR-CSR-GFCTB0A7.

           PERFORM 21200-LER-CSR-GFCTB0A7.

           PERFORM VARYING IND         FROM 1 BY 1
               UNTIL       IND         GREATER 8 OR
               WRK-FINALIZAR           EQUAL 'S'
               PERFORM 21300-MOVER-CAMPOS
           END-PERFORM.

           PERFORM 21400-FECHAR-CSR-GFCTB0A7.

      *----------------------------------------------------------------*
       21000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       21100-ABRIR-CSR-GFCTB0A7        SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FINALIZAR.

           IF  GFCTGT-TIPO-FUNCAO      EQUAL  1
               MOVE 00                 TO WRK-CSIT-SOLTC-INI
               MOVE 99                 TO WRK-CSIT-SOLTC-FIM

               MOVE 0                  TO WRK-CSIT-PROCM-INI
               MOVE 9                  TO WRK-CSIT-PROCM-FIM
           ELSE
               IF  GFCTGT-TIPO-FUNCAO  EQUAL  2
                   MOVE 01             TO WRK-CSIT-SOLTC-INI
                   MOVE 01             TO WRK-CSIT-SOLTC-FIM

                   MOVE 3              TO WRK-CSIT-PROCM-INI
                   MOVE 3              TO WRK-CSIT-PROCM-FIM
               END-IF
           END-IF.

           MOVE GFCTGT-COD-AGRUPAMENTO TO CAGPTO-CTA   OF GFCTB0A7.
           MOVE WRK-TARIFA-AUX         TO CSERVC-TARIF OF GFCTB0A7.

           EXEC SQL
               OPEN CSR-GFCTB0A7
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE '0001'             TO GFCT0M-LOCAL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTGU-COD-SQL-ERRO
               MOVE 0005               TO GFCTGU-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               MOVE 'PDIDO_ESTRN_GRP'  TO GFCT0M-NOME-TAB
               MOVE 'GFCTB0A7'         TO WRK-TABELA
               PERFORM 21110-MOVER-ERRO-SQL
           END-IF.

      *----------------------------------------------------------------*
       21100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       21110-MOVER-ERRO-SQL            SECTION.
      *----------------------------------------------------------------*

           MOVE 9                      TO GFCTGU-ERRO.
           MOVE 'GFCT5093'             TO GFCT0M-PROGRAMA.
           MOVE 'DB2'                  TO GFCT0M-TIPO-ACESSO.
           MOVE SQLCA                  TO GFCT0M-SQLCA-AREA.
           MOVE SPACES                 TO GFCTGU-DESC-MSG-ERRO.

           PERFORM 11100-OBTER-DESC-MSG.

           STRING GFCTG3-DESC-MSG ' - '
                  WRK-TABELA
           DELIMITED BY '  '           INTO GFCTGU-DESC-MSG-ERRO.

           PERFORM 30000-FINALIZAR-PROGRAMA.

      *----------------------------------------------------------------*
       21110-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       21200-LER-CSR-GFCTB0A7          SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                FETCH CSR-GFCTB0A7
                INTO :GFCTB0A7.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE '0002'             TO GFCT0M-LOCAL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTGU-COD-SQL-ERRO
               MOVE 0006               TO GFCTGU-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               MOVE 'PDIDO_ESTRN_GRP'  TO GFCT0M-NOME-TAB
               MOVE 'GFCTB0A7'         TO WRK-TABELA
               PERFORM 21110-MOVER-ERRO-SQL
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FINALIZAR
                                          GFCTGU-FIM
               IF  IND                 EQUAL ZEROS
                   IF  GFCTGT-TIPO-FUNCAO
                                       EQUAL 1 OR
0410                   GFCTF9-TIPO-DEPENDENCIA
0410                                   EQUAL 'GM' OR 'DA'
                       MOVE 0509       TO GFCTGU-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
                   ELSE
                       MOVE 0511       TO GFCTGU-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
                   END-IF
                   PERFORM 21210-TRATAR-FIM-CSR
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       21200-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       21210-TRATAR-FIM-CSR            SECTION.
      *----------------------------------------------------------------*

           MOVE 1                      TO GFCTGU-ERRO.
           MOVE ZEROS                  TO GFCTGU-COD-SQL-ERRO.
           MOVE SPACES                 TO GFCTGU-DESC-MSG-ERRO.

           PERFORM 11100-OBTER-DESC-MSG

           MOVE GFCTG3-DESC-MSG        TO GFCTGU-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       21210-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       21300-MOVER-CAMPOS              SECTION.
      *----------------------------------------------------------------*

           ADD 105                     TO GFCTGU-LL.
           ADD 1                       TO GFCTGU-QTDE-TOT-REG.

           MOVE CSERVC-TARIF           OF GFCTB0A7
                                       TO GFCTGU-COD-TARIFA(IND)
                                          CSERVC-TARIF OF GFCTB0D8.

           PERFORM 24000-OBTER-DADOS-GFCTB0D8.

           MOVE RSERVC-TARIF-REDZD     OF GFCTB0D8
                                       TO GFCTGU-DESC-TARIFA(IND).

           PERFORM 21200-LER-CSR-GFCTB0A7.

           IF  WRK-FINALIZAR           EQUAL 'S'
               GO                      TO 21300-99-FIM
           END-IF.

           IF  IND                     EQUAL 8
               MOVE 'S'                TO WRK-FINALIZAR
               MOVE CSERVC-TARIF       OF GFCTB0A7
                                       TO WRK-TARIFA-AUX
               MOVE WRK-PONTEIRO-AUX   TO GFCTGU-PONTEIRO-TARIFA
           END-IF.

      *----------------------------------------------------------------*
       21300-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       21400-FECHAR-CSR-GFCTB0A7       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE   CSR-GFCTB0A7
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE '0003'             TO GFCT0M-LOCAL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTGU-COD-SQL-ERRO
               MOVE 0011               TO GFCTGU-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               MOVE 'PDIDO_ESTRN_GRP'  TO GFCT0M-NOME-TAB
               MOVE 'GFCTB0A7'         TO WRK-TABELA
               PERFORM 21110-MOVER-ERRO-SQL
           END-IF.

      *----------------------------------------------------------------*
       21400-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       22000-OBTER-COD-JUNCAO          SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTGT-FUNC-BDSCO      TO WRK-COD-FUNCIONAL-0315.
           MOVE 'VRS001'               TO WRK-VERSAO-0315.

           CALL 'SENH0315'             USING WRK-VERSAO-0315
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

           IF  RETURN-CODE             NOT EQUAL ZEROS AND 04
               MOVE 1                  TO GFCTGU-ERRO
               MOVE RETURN-CODE        TO GFCTGU-COD-SQL-ERRO
               MOVE 0056               TO GFCTGU-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               PERFORM 11100-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTGU-DESC-MSG-ERRO
               PERFORM 30000-FINALIZAR-PROGRAMA
           END-IF.

           IF  RETURN-CODE             EQUAL 04
               IF  WRK-COD-MSG-0315    EQUAL 001
                   MOVE 1              TO GFCTGU-ERRO
                   MOVE ZEROS          TO GFCTGU-COD-SQL-ERRO
                   MOVE 0057           TO GFCTGU-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
                   PERFORM 11100-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG
                                       TO GFCTGU-DESC-MSG-ERRO
                   PERFORM 30000-FINALIZAR-PROGRAMA
               END-IF
               IF  WRK-COD-MSG-0315    EQUAL 154
                   MOVE 1              TO GFCTGU-ERRO
                   MOVE ZEROS          TO GFCTGU-COD-SQL-ERRO
                   MOVE 0058           TO GFCTGU-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
                   PERFORM 11100-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG
                                       TO GFCTGU-DESC-MSG-ERRO
                   PERFORM 30000-FINALIZAR-PROGRAMA
               END-IF
               IF  WRK-COD-MSG-0315    EQUAL 242
                   MOVE 1              TO GFCTGU-ERRO
                   MOVE ZEROS          TO GFCTGU-COD-SQL-ERRO
                   MOVE 0059           TO GFCTGU-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
                   PERFORM 11100-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG
                                       TO GFCTGU-DESC-MSG-ERRO
                   PERFORM 30000-FINALIZAR-PROGRAMA
               END-IF
               IF  WRK-COD-MSG-0315    EQUAL 009
                   MOVE ZEROS          TO GFCTGU-ERRO
                                          GFCTGU-COD-SQL-ERRO
                                          GFCTGU-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
                   PERFORM 11100-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG
                                       TO GFCTGU-DESC-MSG-ERRO
                   PERFORM 30000-FINALIZAR-PROGRAMA
               END-IF
               IF  WRK-COD-MSG-0315    EQUAL 138
                   MOVE 1              TO GFCTGU-ERRO
                   MOVE ZEROS          TO GFCTGU-COD-SQL-ERRO
                   MOVE 0060           TO GFCTGU-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
                   PERFORM 11100-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG
                                       TO GFCTGU-DESC-MSG-ERRO
                   PERFORM 30000-FINALIZAR-PROGRAMA
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       22000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       23000-VERIFICA-MASTER           SECTION.
      *----------------------------------------------------------------*

0410       INITIALIZE                  GFCTF9-GFCT5515.
0410       MOVE GFCTGT-FUNC-BDSCO      TO GFCTF9-COD-FUNC.
0410       MOVE 'GFCT5558'             TO WRK-MODULO.
0410
0410       CALL  WRK-MODULO         USING GFCTF9-GFCT5515.
0410
0410       IF  RETURN-CODE            NOT EQUAL ZEROS
0410           MOVE 9                  TO GFCTGU-ERRO
0410
0410           MOVE 'GFCT5093'         TO GFCT0M-PROGRAMA
0410           MOVE 'GFCT5558'         TO WRK-NOME-MOD
0410           MOVE WRK-MSG05          TO GFCT0M-TEXTO
0410           PERFORM 30000-FINALIZAR-PROGRAMA
0410       END-IF.
0410
0410       IF  GFCTF9-COD-RETORNO     NOT EQUAL ZEROS
0410           MOVE 1                  TO GFCTGU-ERRO
0410           MOVE ZEROS              TO GFCTGU-COD-SQL-ERRO
0410                                      GFCTGU-COD-MSG-ERRO
0410           MOVE GFCTF9-DESC-MSG    TO GFCTGU-DESC-MSG-ERRO
0410           MOVE 'S'                TO GFCTGU-FIM
0410           INITIALIZE              GFCTGU-DADOS
0410           PERFORM 30000-FINALIZAR-PROGRAMA
0410       END-IF.

      *----------------------------------------------------------------*
       23000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       24000-OBTER-DADOS-GFCTB0D8      SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0A7
                                       TO CSERVC-TARIF OF GFCTB0D8.

           EXEC SQL
                SELECT CDEPDC,
                       RSERVC_TARIF_REDZD
                INTO  :GFCTB0D8.CDEPDC,
                      :GFCTB0D8.RSERVC-TARIF-REDZD
                FROM   DB2PRD.SERVC_TARIF_PRINC
                WHERE  CSERVC_TARIF       = :GFCTB0D8.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100 AND
                                                           -811) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0005'             TO GFCT0M-LOCAL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTGU-COD-SQL-ERRO
               MOVE 0012               TO GFCTGU-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               MOVE 'SERVC_TARIF_PRINC'
                                       TO GFCT0M-NOME-TAB
               MOVE 'GFCTB0D8'         TO WRK-TABELA
               PERFORM 21110-MOVER-ERRO-SQL
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 0510               TO GFCTGU-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               PERFORM 21210-TRATAR-FIM-CSR
               PERFORM 30000-FINALIZAR-PROGRAMA
           END-IF.

      *----------------------------------------------------------------*
       24000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       25000-TRATAR-REG-JUNCAO         SECTION.
      *----------------------------------------------------------------*

           PERFORM 21200-LER-CSR-GFCTB0A7.

           IF  WRK-FINALIZAR           EQUAL 'S'
               GO                      TO 25000-99-FIM
           END-IF.

           PERFORM 24000-OBTER-DADOS-GFCTB0D8.

           IF  WRK-COD-JUNCAO-0315     EQUAL CDEPDC OF GFCTB0D8

               IF  IND                 EQUAL 8
                   MOVE 'S'            TO WRK-FINALIZAR
                   MOVE CSERVC-TARIF   OF GFCTB0A7
                                       TO WRK-TARIFA-AUX
                   MOVE WRK-PONTEIRO-AUX
                                       TO GFCTGU-PONTEIRO-TARIFA
               ELSE
                   ADD 1               TO IND
                                          GFCTGU-QTDE-TOT-REG
                   ADD 105             TO GFCTGU-LL
                   MOVE CSERVC-TARIF   OF GFCTB0A7
                                       TO GFCTGU-COD-TARIFA(IND)
                                          CSERVC-TARIF OF GFCTB0D8
                   MOVE RSERVC-TARIF-REDZD OF GFCTB0D8
                                       TO GFCTGU-DESC-TARIFA(IND)
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       25000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       30000-FINALIZAR-PROGRAMA        SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO RETURN-CODE.

           GOBACK.

      *----------------------------------------------------------------*
       30000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*
