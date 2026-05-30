      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT5565.
       AUTHOR.     FLAVIO AUGUSTO MARIA.
      *================================================================*
      *                   C P M  -  S I S T E M A S                    *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT5565                                    *
      *    PROGRAMADOR.:   FLAVIO AUGUSTO MARIA    - CPM PATO BRANCO   *
      *    ANALISTA CPM:   RICARDO LUDWIG          - CPM PATO BRANCO   *
      *    ANALISTA....:   RICARDO PEREIRA         - PWI / GRUPO 50    *
      *    DATA........:   07/04/2005                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   GERAR DADOS PARA TEXT DE SIGLAS DE SISTEMA. *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                         INCLUDE/BOOK    *
      *                    DB2PRD.V01CTRO_CUSTO          ANPRV005      *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    I#GFCT0M - AREA DE COMUNICACAO - ERROS.                     *
      *    I#GFCTG2 - AREA DE COMUNICACAO - ENTRADA DO MODULO GFCT5523 *
      *    I#GFCTG3 - AREA DE COMUNICACAO - SAIDA DO MODULO GFCT5523   *
      *    I#GFCTUE - AREA DE COMUNICACAO - ENTRADA.                   *
      *    I#GFCTUF - AREA DE COMUNICACAO - SAIDA.                     *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    GFCT5000 - OBTER DESCRICAO DE MENSAGENS.                    *
      *    GFCT5523 - OBTER DESCRICAO DE MENSAGENS                     *
      *    POOL0081 - ALOCAR DINAMICAMENTE UM MODULO.                  *
      *    SENH0315 - OBTER JUNCAO.                                    *
      *================================================================*
AT0522* ALTERACOES:
AT0522*================================================================
AT0522* DATA       NOME          DESCRICAO
AT0522*========== ============   ======================================
AT0522*19/05/2022 ANGELO TROMBIM - ALTERACAO NO ACESSO A TABELA
AT0522*                            ANPRV005 PARA O SERVICO CARO8200.
AT0522*                          - AJUSTE DAS VARIAVEIS REFERENTE A
AT0522*                            TABELA ANPRB005 PARA CARO8200.
      *================================================================*
           EJECT
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
           '*** GFCT5565 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA INDEXADOR ***'.
      *----------------------------------------------------------------*

       77  IND-1                       PIC  9(003)         VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*

       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-BUSCA                   PIC  X(004)         VALUE SPACES.

       01  WRK-NOME-TABELA.
           03  FILLER                  PIC  X(003)         VALUE ' - '.
           03  WRK-NOME-TAB            PIC  X(008)         VALUE SPACES.

       01  WRK-MSG001.
           05 FILLER                   PIC  X(026)         VALUE
              'ERRO NA CHAMADA AO MODULO '.
           05 WRK-MODULO-MSG           PIC  X(008)         VALUE SPACES.

      *----------------------------------------------------------------*
      *01  FILLER                      PIC  X(050)         VALUE
      *    '*** AREA PARA MODULO GFCT5000 ***'.
      *----------------------------------------------------------------*
      *
      *01  WRK-5000-AREA.
      *    03  WRK-5000-AREA-ENVIA.
      *        05  WRK-5000-AMBIENTE     PIC X(001)        VALUE SPACES.
      *        05  GFCTG2-COD-MSG        PIC 9(004)        VALUE ZEROS.
      *    03  WRK-5000-AREA-RECEBE.
      *        05  WRK-5000-COD-RETORNO  PIC 9(002)        VALUE ZEROS.
      *        05  WRK-5000-COD-SQL-ERRO PIC 9(003)        VALUE ZEROS.
      *        05  WRK-5000-DESC-MSG     PIC X(070)        VALUE SPACES.
      *
      *    EJECT

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
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA MODULO SENH0315 ***'.
      *----------------------------------------------------------------*

       01  WRK-VERSAO-0315             PIC  X(006)         VALUE
           'VRS001'.
       01  WRK-COD-FUNCIONAL-0315      PIC  X(007)         VALUE SPACES.
       01  WRK-NOME-FUNC-0315          PIC  X(040)         VALUE SPACES.
       01  WRK-COD-BANCO-0315          PIC  9(005)         VALUE ZEROS.
       01  WRK-NOME-BANCO-0315         PIC  X(040)         VALUE SPACES.
       01  WRK-COD-JUNCAO-0315         PIC  9(005)         VALUE ZEROS.
       01  WRK-NOME-DEPTO-0315         PIC  X(040)         VALUE SPACES.
       01  WRK-SECAO-0315              PIC  9(003)         VALUE ZEROS.
       01  WRK-STATUS-0315             PIC  X(001)         VALUE SPACES.

       01  WRK-MENSAGEM-0315.
           03  WRK-COD-MENSAGEM-0315   PIC  9(003)         VALUE ZEROS.
           03  WRK-HIFEN-MENSAGEM-0315 PIC  X(001)         VALUE SPACES.
           03  WRK-TEXTO-MENSAGEM-0315 PIC  X(079)         VALUE SPACES.

       01  WRK-CNPJ-BANCO-0315.
ST25X6*     03  WRK-CNPJ-PRI-0315       PIC  9(009)         VALUE ZEROS.
ST25X6*     03  WRK-CNPJ-FIL-0315       PIC  9(005)         VALUE ZEROS.
ST25X6*     03  WRK-CNPJ-CON-0315       PIC  9(002)         VALUE ZEROS.
           03  WRK-CNPJ-PRI-0315       PIC  X(009)         VALUE SPACES.
           03  WRK-CNPJ-FIL-0315       PIC  X(004)         VALUE SPACES.
           03  WRK-CNPJ-CON-0315       PIC  X(002)         VALUE SPACES.

           EJECT
AT0522*----------------------------------------------------------------*
AT0522 01  FILLER                      PIC X(050)  VALUE
AT0522     'AREA DE INTERFACE COM SERVICO FUNCIONAL CARO8200'.
AT0522*----------------------------------------------------------------*
AT0522
AT0522 01  WRK-CARO8200                PIC  X(008)         VALUE
AT0522     'CARO8200'.
AT0522
AT0522     COPY 'CAROW820'.
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** GFCT5565 - FIM DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY I#GFCTUE.

           EJECT
       COPY I#GFCTUF.

           EJECT
       COPY I#GFCT0M.

           EJECT
      *================================================================*
       PROCEDURE DIVISION              USING GFCTUE-ENTRADA
                                             GFCTUF-SAIDA
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

           MOVE SPACES                 TO
                                          GFCT0M-AREA-ERROS
                                          GFCTUF-SAIDA
                                          WRK-MENSAGEM-0315
                                          WRK-NOME-FUNC-0315
                                          WRK-NOME-BANCO-0315
                                          WRK-NOME-DEPTO-0315
                                          WRK-STATUS-0315.

           INITIALIZE
                                       GFCT0M-AREA-ERROS
                                       GFCTUF-SAIDA
                                       WRK-MENSAGEM-0315
                                       WRK-COD-BANCO-0315
                                       WRK-CNPJ-BANCO-0315
                                       WRK-COD-JUNCAO-0315
                                       WRK-SECAO-0315.

           PERFORM 110000-INICIALIZA-SAIDA.

           PERFORM 120000-CONSISTIR-DADOS.

      *----------------------------------------------------------------*
       100000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       110000-INICIALIZA-SAIDA         SECTION.
      *----------------------------------------------------------------*

           MOVE 340                    TO GFCTUF-LL.
           MOVE GFCTUE-ZZ              TO GFCTUF-ZZ.
           MOVE GFCTUE-TRANSACAO       TO GFCTUF-TRANSACAO.
           MOVE GFCTUE-FUNCAO          TO GFCTUF-FUNCAO.

           IF  GFCTUE-QTDE-OCOR        NOT NUMERIC
               MOVE ZEROS              TO GFCTUF-QTDE-OCOR
           ELSE
               MOVE GFCTUE-QTDE-OCOR   TO GFCTUF-QTDE-OCOR
           END-IF.

           IF  GFCTUE-QTDE-TOT-REG     NOT NUMERIC
               MOVE ZEROS              TO GFCTUF-QTDE-TOT-REG
           ELSE
               MOVE GFCTUE-QTDE-TOT-REG TO GFCTUF-QTDE-TOT-REG
           END-IF.

           MOVE GFCTUE-FUNC-BDSCO      TO GFCTUF-FUNC-BDSCO.
           MOVE GFCTUE-TERMINAL        TO GFCTUF-TERMINAL.
           MOVE GFCTUE-CCTRO-CUSTO     TO GFCTUF-COD-SISTEMA
                                          WRK-BUSCA.
           MOVE 'N'                    TO GFCTUF-FIM.
           MOVE ZEROS                  TO GFCTUF-ERRO
                                          GFCTUF-COD-SQL-ERRO
                                          GFCTUF-COD-MSG-ERRO
                                          GFCTG2-COD-MSG.

           PERFORM 111000-OBTER-DESCRICAO-MSG.

           MOVE GFCTG3-DESC-MSG        TO GFCTUF-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       110000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       111000-OBTER-DESCRICAO-MSG      SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTG3-SAIDA.

           INITIALIZE GFCTG3-SAIDA.

           MOVE +100                   TO GFCTG2-LL.
           MOVE ZEROS                  TO GFCTG2-ZZ.
           MOVE GFCTUE-TRANSACAO       TO GFCTG2-TRANSACAO.
           MOVE GFCTUE-FUNCAO          TO GFCTG2-FUNCAO.
           MOVE GFCTUE-FUNC-BDSCO      TO GFCTG2-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTG2-FIM.
           MOVE 'O'                    TO GFCTG2-TIPO-PROC.

           MOVE 'GFCT5523'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTG2-ENTRADA
                                             GFCTG3-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTUF-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0480'         TO GFCT0M-TRANSACAO
               PERFORM 300000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 9
               MOVE 9                  TO GFCTUF-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0480'         TO GFCT0M-TRANSACAO
               PERFORM 300000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 1
               MOVE 1                  TO GFCTUF-ERRO
               MOVE GFCTG3-COD-SQL-ERRO
                                       TO GFCTUF-COD-SQL-ERRO
               MOVE GFCTG3-COD-MSG-ERRO
                                       TO GFCTUF-COD-MSG-ERRO
               MOVE GFCTG3-DESC-MSG-ERRO
                                       TO GFCTUF-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       111000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       120000-CONSISTIR-DADOS          SECTION.
      *----------------------------------------------------------------*

           IF (GFCTUE-TRANSACAO        EQUAL SPACES OR LOW-VALUES OR
               GFCTUE-FUNCAO           EQUAL SPACES OR LOW-VALUES OR
               GFCTUE-QTDE-OCOR        NOT NUMERIC  OR
               GFCTUE-QTDE-TOT-REG     NOT NUMERIC  OR
               GFCTUE-FUNC-BDSCO       EQUAL SPACES OR LOW-VALUES OR
               GFCTUE-CCTRO-CUSTO      EQUAL SPACES OR LOW-VALUES OR
               GFCTUE-FIM              NOT EQUAL 'S' AND 'N')
               MOVE 1                  TO GFCTUF-ERRO
                                          GFCTUF-COD-MSG-ERRO
               MOVE 0001               TO GFCTG2-COD-MSG
               MOVE ZEROS              TO GFCTUF-COD-SQL-ERRO
               MOVE 'S'                TO GFCTUF-FIM
               PERFORM 111000-OBTER-DESCRICAO-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTUF-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       120000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       200000-PROCESSAR                SECTION.
      *----------------------------------------------------------------*

           PERFORM 210000-OBTER-COD-JUNCAO.


AT0522     PERFORM 2300-SELECT-CARO8200-CUSTO.

      *----------------------------------------------------------------*
       200000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       210000-OBTER-COD-JUNCAO         SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTUE-FUNC-BDSCO           TO WRK-COD-FUNCIONAL-0315.

           CALL 'SENH0315'                  USING WRK-VERSAO-0315
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

           IF  RETURN-CODE                    NOT EQUAL ZEROS AND 4
               MOVE 1                         TO GFCTUF-ERRO
               MOVE RETURN-CODE               TO GFCTUF-COD-SQL-ERRO
               MOVE 0056                      TO GFCTUF-COD-MSG-ERRO
                                                 GFCTG2-COD-MSG
               PERFORM 111000-OBTER-DESCRICAO-MSG
               MOVE GFCTG3-DESC-MSG           TO GFCTUF-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

           IF  RETURN-CODE                    EQUAL 4
               EVALUATE  WRK-COD-MENSAGEM-0315

                   WHEN 001
                       MOVE 1                 TO GFCTUF-ERRO
                       MOVE ZEROS             TO GFCTUF-COD-SQL-ERRO
                       MOVE 0057              TO GFCTUF-COD-MSG-ERRO
                                                 GFCTG2-COD-MSG
                       PERFORM 111000-OBTER-DESCRICAO-MSG
                       MOVE GFCTG3-DESC-MSG   TO GFCTUF-DESC-MSG-ERRO
                       PERFORM 300000-FINALIZAR

                   WHEN 154
                       MOVE 1                 TO GFCTUF-ERRO
                       MOVE ZEROS             TO GFCTUF-COD-SQL-ERRO
                       MOVE 0058              TO GFCTUF-COD-MSG-ERRO
                                                 GFCTG2-COD-MSG
                       PERFORM 111000-OBTER-DESCRICAO-MSG
                       MOVE GFCTG3-DESC-MSG   TO GFCTUF-DESC-MSG-ERRO
                       PERFORM 300000-FINALIZAR

                   WHEN 242
                       MOVE 1                 TO GFCTUF-ERRO
                       MOVE ZEROS             TO GFCTUF-COD-SQL-ERRO
                       MOVE 0059              TO GFCTUF-COD-MSG-ERRO
                                                 GFCTG2-COD-MSG
                       PERFORM 111000-OBTER-DESCRICAO-MSG
                       MOVE GFCTG3-DESC-MSG   TO GFCTUF-DESC-MSG-ERRO
                       PERFORM 300000-FINALIZAR

                   WHEN 009
                       MOVE ZEROS             TO GFCTUF-ERRO
                                                 GFCTUF-COD-SQL-ERRO
                                                 GFCTUF-COD-MSG-ERRO
                                                 GFCTG2-COD-MSG
                       PERFORM 111000-OBTER-DESCRICAO-MSG
                       MOVE GFCTG3-DESC-MSG   TO GFCTUF-DESC-MSG-ERRO
                       PERFORM 300000-FINALIZAR

                   WHEN 138
                       MOVE 1                 TO GFCTUF-ERRO
                       MOVE ZEROS             TO GFCTUF-COD-SQL-ERRO
                       MOVE 0060              TO GFCTUF-COD-MSG-ERRO
                                                 GFCTG2-COD-MSG
                       PERFORM 111000-OBTER-DESCRICAO-MSG
                       MOVE GFCTG3-DESC-MSG   TO GFCTUF-DESC-MSG-ERRO
                       PERFORM 300000-FINALIZAR
               END-EVALUATE
           END-IF.

      *----------------------------------------------------------------*
       210000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT

      *----------------------------------------------------------------*
AT0522 2300-SELECT-CARO8200-CUSTO      SECTION.
      *----------------------------------------------------------------*

AT0522     INITIALIZE                       CARO01-COMUAREA.

AT0522     MOVE 'S'                     TO  CARO01-ACAO.
AT0522     MOVE 05                      TO  CARO01-TIPO-PESQUISA.
AT0522     MOVE WRK-BUSCA               TO  CARO01-CCTRO-CUSTO.

AT0522     CALL  WRK-CARO8200 USING CARO01-COMUAREA.

AT0522     IF (CARO01-COD-RETORNO      NOT EQUAL ZEROS AND 01  )
               MOVE 'GFCT5565'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
AT0522         MOVE 'TCARO_ATIV        'TO GFCT0M-NOME-TAB
AT0522         MOVE 'CARO8200  '        TO GFCT0M-COMANDO-SQL
AT0522         MOVE CARO01-COD-RETORNO  TO GFCT0M-SQLCODE
                                           GFCTUF-COD-SQL-ERRO
               MOVE '0001'              TO GFCT0M-LOCAL
               MOVE 9                   TO GFCTUF-ERRO
               MOVE 0012                TO GFCTUF-COD-MSG-ERRO
                                           GFCTG2-COD-MSG
               PERFORM 111000-OBTER-DESCRICAO-MSG
               MOVE 'CARO8200'          TO WRK-NOME-TAB
               PERFORM 231000-CONCATENAR-NOME-TAB
               PERFORM 300000-FINALIZAR
           END-IF.

AT0522     IF  CARO01-COD-RETORNO       EQUAL 01
               MOVE 1                   TO GFCTUF-ERRO
AT0522         MOVE CARO01-COD-RETORNO  TO GFCTUF-COD-SQL-ERRO
               MOVE 0151                TO GFCTUF-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               PERFORM 111000-OBTER-DESCRICAO-MSG
               MOVE GFCTG3-DESC-MSG     TO GFCTUF-DESC-MSG-ERRO
               MOVE 1                   TO GFCTUF-ERR-SISTEMA
               MOVE GFCTUE-CCTRO-CUSTO  TO GFCTUF-COD-SISTEMA
               MOVE SPACES              TO GFCTUF-DESC-SISTEMA
AT0522         MOVE CARO01-COD-RETORNO  TO GFCTUF-COD-SQL-ERRO-GEN
               MOVE 0151                TO GFCTUF-COD-MSG-ERRO-GEN
                                          GFCTG2-COD-MSG
               PERFORM 111000-OBTER-DESCRICAO-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTUF-DESC-MSG-ERRO-GEN
           ELSE
AT0522         MOVE CARO01-CCTRO-CUSTO
                                       TO GFCTUF-COD-SISTEMA
AT0522         MOVE CARO01-ICTRO-CUSTO-ROTNA
                                       TO GFCTUF-DESC-SISTEMA
           END-IF.

      *----------------------------------------------------------------*
AT0522 2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       231000-CONCATENAR-NOME-TAB      SECTION.
      *----------------------------------------------------------------*

           PERFORM
             VARYING IND-1                      FROM 68 BY -1
               UNTIL GFCTG3-DESC-MSG(IND-1:1) NOT EQUAL SPACES
                  OR IND-1                      EQUAL ZEROS
                  IF GFCTG3-DESC-MSG(IND-1:1) NOT EQUAL SPACES
                     MOVE GFCTG3-DESC-MSG(1:IND-1)
                                       TO GFCTUF-DESC-MSG-ERRO(1:IND-1)
                  END-IF
           END-PERFORM.

           MOVE SPACES                 TO GFCTUF-DESC-MSG-ERRO.

           MOVE GFCTG3-DESC-MSG(1:IND-1)
                                       TO GFCTUF-DESC-MSG-ERRO(1:IND-1).

           ADD 1                       TO IND-1.

           MOVE WRK-NOME-TABELA       TO GFCTUF-DESC-MSG-ERRO(IND-1:11).

      *----------------------------------------------------------------*
       231000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       300000-FINALIZAR                SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       300000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
