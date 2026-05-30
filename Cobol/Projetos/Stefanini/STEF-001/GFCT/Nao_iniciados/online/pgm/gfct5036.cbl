      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT5036.
       AUTHOR.     FLAVIO AUGUSTO MARIA.
      *================================================================*
      *                   C P M  -  S I S T E M A S                    *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT5036                                    *
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
      *    I#GFCT8U - AREA DE COMUNICACAO - ENTRADA.                   *
      *    I#GFCT8V - AREA DE COMUNICACAO - SAIDA.                     *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    GFCT5000 - OBTER DESCRICAO DE MENSAGENS.                    *
      *    POOL0081 - ALOCAR DINAMICAMENTE UM MODULO.                  *
      *    GFCT5507 - VERIFICA MASTER.                                 *
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
           '*** GFCT5036 - INICIO DA AREA DE WORKING ***'.
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

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA MODULO GFCT5000 ***'.
      *----------------------------------------------------------------*

       01  WRK-5000-AREA.
           03  WRK-5000-AREA-ENVIA.
               05  WRK-5000-AMBIENTE     PIC X(001)        VALUE SPACES.
               05  WRK-5000-COD-MSG      PIC 9(004)        VALUE ZEROS.
           03  WRK-5000-AREA-RECEBE.
               05  WRK-5000-COD-RETORNO  PIC 9(002)        VALUE ZEROS.
               05  WRK-5000-COD-SQL-ERRO PIC 9(003)        VALUE ZEROS.
               05  WRK-5000-DESC-MSG     PIC X(070)        VALUE SPACES.

           EJECT
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA MODULO GFCT5507 ***'.
      *----------------------------------------------------------------*

       01  WRK-5507-AREA.
           03  WRK-5507-AREA-ENVIA.
               05  WRK-5507-AMBIENTE     PIC X(001)        VALUE SPACES.
               05  WRK-5507-CJUNC-DEPDC  PIC 9(005)        VALUE ZEROS.
           03  WRK-5507-AREA-RECEBE.
               05  WRK-5507-COD-RETORNO  PIC 9(002)        VALUE ZEROS.
               05  WRK-5507-COD-SQL-ERRO PIC 9(003)        VALUE ZEROS.
               05  WRK-5507-MSG-RETORNO  PIC X(045)        VALUE SPACES.
               05  WRK-5507-IDTFD-MASTER PIC X(001)        VALUE SPACES.

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
           '*** GFCT5036 - FIM DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

-INC I#GFCT8U

           EJECT
-INC I#GFCT8V

           EJECT
-INC I#GFCT0M

           EJECT
      *================================================================*
       PROCEDURE DIVISION              USING GFCT8U-ENTRADA
                                             GFCT8V-SAIDA
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
                                          GFCT8V-SAIDA
                                          WRK-5507-AREA
                                          WRK-5000-AREA
                                          WRK-MENSAGEM-0315
                                          WRK-NOME-FUNC-0315
                                          WRK-NOME-BANCO-0315
                                          WRK-NOME-DEPTO-0315
                                          WRK-STATUS-0315.

           INITIALIZE
                                       GFCT0M-AREA-ERROS
                                       GFCT8V-SAIDA
                                       WRK-5507-AREA
                                       WRK-5000-AREA
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

           MOVE 340                    TO GFCT8V-LL.
           MOVE GFCT8U-ZZ              TO GFCT8V-ZZ.
           MOVE GFCT8U-TRANSACAO       TO GFCT8V-TRANSACAO.
           MOVE GFCT8U-FUNCAO          TO GFCT8V-FUNCAO.

           IF  GFCT8U-QTDE-OCOR        NOT NUMERIC
               MOVE ZEROS              TO GFCT8V-QTDE-OCOR
           ELSE
               MOVE GFCT8U-QTDE-OCOR   TO GFCT8V-QTDE-OCOR
           END-IF.

           IF  GFCT8U-QTDE-TOT-REG     NOT NUMERIC
               MOVE ZEROS              TO GFCT8V-QTDE-TOT-REG
           ELSE
               MOVE GFCT8U-QTDE-TOT-REG TO GFCT8V-QTDE-TOT-REG
           END-IF.

           MOVE GFCT8U-FUNC-BDSCO      TO GFCT8V-FUNC-BDSCO.
           MOVE GFCT8U-TERMINAL        TO GFCT8V-TERMINAL.
           MOVE GFCT8U-CCTRO-CUSTO     TO GFCT8V-COD-SISTEMA
                                          WRK-BUSCA.
           MOVE 'N'                    TO GFCT8V-FIM.
           MOVE ZEROS                  TO GFCT8V-ERRO
                                          GFCT8V-COD-SQL-ERRO
                                          GFCT8V-COD-MSG-ERRO
                                          WRK-5000-COD-MSG.

           PERFORM 111000-OBTER-DESCRICAO-MSG.

           MOVE WRK-5000-DESC-MSG      TO GFCT8V-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       110000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       111000-OBTER-DESCRICAO-MSG      SECTION.
      *----------------------------------------------------------------*

           MOVE 'O'                          TO WRK-5000-AMBIENTE.
           MOVE 'GFCT5000'                   TO WRK-MODULO.

           CALL 'POOL0081'                   USING WRK-MODULO
                                                   WRK-5000-AREA-ENVIA
                                                   WRK-5000-AREA-RECEBE.


           IF  WRK-5000-COD-RETORNO          NOT EQUAL ZEROS
               IF  WRK-5000-COD-RETORNO      EQUAL 01
                   MOVE 1                    TO GFCT8V-ERRO
                   MOVE ZEROS                TO GFCT8V-COD-SQL-ERRO
                                                GFCT8V-COD-MSG-ERRO
                   MOVE WRK-5000-DESC-MSG    TO GFCT8V-DESC-MSG-ERRO
               ELSE
                   IF  WRK-5000-COD-RETORNO  EQUAL 99
                       MOVE 1                TO GFCT8V-ERRO
                       MOVE ZEROS            TO GFCT8V-COD-MSG-ERRO
                       MOVE WRK-5000-COD-SQL-ERRO
                                             TO GFCT8V-COD-SQL-ERRO
                       MOVE WRK-5000-DESC-MSG
                                             TO GFCT8V-DESC-MSG-ERRO
                   END-IF
               END-IF
               MOVE SPACES                   TO GFCT8V-DADOS
               INITIALIZE GFCT8V-DADOS
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       111000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       120000-CONSISTIR-DADOS          SECTION.
      *----------------------------------------------------------------*

           IF (GFCT8U-TRANSACAO        EQUAL SPACES OR LOW-VALUES OR
               GFCT8U-FUNCAO           EQUAL SPACES OR LOW-VALUES OR
               GFCT8U-QTDE-OCOR        NOT NUMERIC  OR
               GFCT8U-QTDE-TOT-REG     NOT NUMERIC  OR
               GFCT8U-FUNC-BDSCO       EQUAL SPACES OR LOW-VALUES OR
               GFCT8U-TERMINAL         EQUAL SPACES OR LOW-VALUES OR
               GFCT8U-CCTRO-CUSTO      EQUAL SPACES OR LOW-VALUES OR
               GFCT8U-FIM              NOT EQUAL 'S' AND 'N')
               MOVE 1                  TO GFCT8V-ERRO
                                          GFCT8V-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE ZEROS              TO GFCT8V-COD-SQL-ERRO
               MOVE 'S'                TO GFCT8V-FIM
               PERFORM 111000-OBTER-DESCRICAO-MSG
               MOVE WRK-5000-DESC-MSG  TO GFCT8V-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       120000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       200000-PROCESSAR                SECTION.
      *----------------------------------------------------------------*

           PERFORM 210000-OBTER-COD-JUNCAO.

           PERFORM 220000-ACESSAR-GFCT5507.

           IF  WRK-5507-IDTFD-MASTER       EQUAL 'S'
AT0522         PERFORM 2300-SELECT-CAROB001-CUSTO
           ELSE
AT0522         PERFORM 2400-SELECT-CAROB001-DEPDC
           END-IF.

      *----------------------------------------------------------------*
       200000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       210000-OBTER-COD-JUNCAO         SECTION.
      *----------------------------------------------------------------*

           MOVE GFCT8U-FUNC-BDSCO           TO WRK-COD-FUNCIONAL-0315.

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
               MOVE 1                         TO GFCT8V-ERRO
               MOVE RETURN-CODE               TO GFCT8V-COD-SQL-ERRO
               MOVE 0056                      TO GFCT8V-COD-MSG-ERRO
                                                 WRK-5000-COD-MSG
               PERFORM 111000-OBTER-DESCRICAO-MSG
               MOVE WRK-5000-DESC-MSG         TO GFCT8V-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

           IF  RETURN-CODE                    EQUAL 4
               EVALUATE  WRK-COD-MENSAGEM-0315

                   WHEN 001
                       MOVE 1                 TO GFCT8V-ERRO
                       MOVE ZEROS             TO GFCT8V-COD-SQL-ERRO
                       MOVE 0057              TO GFCT8V-COD-MSG-ERRO
                                                 WRK-5000-COD-MSG
                       PERFORM 111000-OBTER-DESCRICAO-MSG
                       MOVE WRK-5000-DESC-MSG TO GFCT8V-DESC-MSG-ERRO
                       PERFORM 300000-FINALIZAR

                   WHEN 154
                       MOVE 1                 TO GFCT8V-ERRO
                       MOVE ZEROS             TO GFCT8V-COD-SQL-ERRO
                       MOVE 0058              TO GFCT8V-COD-MSG-ERRO
                                                 WRK-5000-COD-MSG
                       PERFORM 111000-OBTER-DESCRICAO-MSG
                       MOVE WRK-5000-DESC-MSG TO GFCT8V-DESC-MSG-ERRO
                       PERFORM 300000-FINALIZAR

                   WHEN 242
                       MOVE 1                 TO GFCT8V-ERRO
                       MOVE ZEROS             TO GFCT8V-COD-SQL-ERRO
                       MOVE 0059              TO GFCT8V-COD-MSG-ERRO
                                                 WRK-5000-COD-MSG
                       PERFORM 111000-OBTER-DESCRICAO-MSG
                       MOVE WRK-5000-DESC-MSG TO GFCT8V-DESC-MSG-ERRO
                       PERFORM 300000-FINALIZAR

                   WHEN 009
                       MOVE ZEROS             TO GFCT8V-ERRO
                                                 GFCT8V-COD-SQL-ERRO
                                                 GFCT8V-COD-MSG-ERRO
                                                 WRK-5000-COD-MSG
                       PERFORM 111000-OBTER-DESCRICAO-MSG
                       MOVE WRK-5000-DESC-MSG TO GFCT8V-DESC-MSG-ERRO
                       PERFORM 300000-FINALIZAR

                   WHEN 138
                       MOVE 1                 TO GFCT8V-ERRO
                       MOVE ZEROS             TO GFCT8V-COD-SQL-ERRO
                       MOVE 0060              TO GFCT8V-COD-MSG-ERRO
                                                 WRK-5000-COD-MSG
                       PERFORM 111000-OBTER-DESCRICAO-MSG
                       MOVE WRK-5000-DESC-MSG TO GFCT8V-DESC-MSG-ERRO
                       PERFORM 300000-FINALIZAR
               END-EVALUATE
           END-IF.

      *----------------------------------------------------------------*
       210000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       220000-ACESSAR-GFCT5507         SECTION.
      *----------------------------------------------------------------*

           MOVE 'O'                          TO WRK-5507-AMBIENTE.
           MOVE WRK-COD-JUNCAO-0315          TO WRK-5507-CJUNC-DEPDC.

           MOVE 'GFCT5507'                   TO WRK-MODULO.

           CALL 'POOL0081'                   USING WRK-MODULO
                                                   WRK-5507-AREA.

           IF  WRK-5507-COD-RETORNO          NOT EQUAL ZEROS
               IF  WRK-5507-COD-RETORNO      EQUAL 01
                   MOVE 1                    TO GFCT8V-ERRO
                   MOVE ZEROS                TO GFCT8V-COD-SQL-ERRO
                                                GFCT8V-COD-MSG-ERRO
                   MOVE WRK-5507-MSG-RETORNO TO GFCT8V-DESC-MSG-ERRO
               ELSE
                   IF  WRK-5507-COD-RETORNO  EQUAL 99
                       MOVE 1                TO GFCT8V-ERRO
                       MOVE ZEROS            TO GFCT8V-COD-MSG-ERRO
                       MOVE WRK-5507-COD-SQL-ERRO
                                             TO GFCT8V-COD-SQL-ERRO
                       MOVE WRK-5507-MSG-RETORNO
                                             TO GFCT8V-DESC-MSG-ERRO
                   END-IF
               END-IF
               MOVE SPACES                   TO GFCT8V-DADOS
               INITIALIZE GFCT8V-DADOS
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       220000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
AT0522 2300-SELECT-CAROB001-CUSTO      SECTION.
      *----------------------------------------------------------------*

AT0522     INITIALIZE                       CARO01-COMUAREA.

AT0522     MOVE 'S'                     TO  CARO01-ACAO.
AT0522     MOVE 05                      TO  CARO01-TIPO-PESQUISA.
AT0522     MOVE WRK-BUSCA               TO  CARO01-CCTRO-CUSTO.

AT0522     CALL  WRK-CARO8200 USING CARO01-COMUAREA.

AT0522     IF (CARO01-COD-RETORNO      NOT EQUAL ZEROS AND 01  )
               MOVE 'GFCT5036'           TO GFCT0M-PROGRAMA
               MOVE 'DB2'                TO GFCT0M-TIPO-ACESSO
AT0522         MOVE 'TCARO_ATIV        ' TO GFCT0M-NOME-TAB
AT0522         MOVE 'CARO8200  '         TO GFCT0M-COMANDO-SQL
AT0522         MOVE CARO01-COD-RETORNO   TO GFCT0M-SQLCODE
                                            GFCT8V-COD-SQL-ERRO
               MOVE '0001'               TO GFCT0M-LOCAL
               MOVE 9                    TO GFCT8V-ERRO
               MOVE 0012                 TO GFCT8V-COD-MSG-ERRO
                                            WRK-5000-COD-MSG
               PERFORM 111000-OBTER-DESCRICAO-MSG
AT0522         MOVE 'CARO8200'           TO WRK-NOME-TAB
               PERFORM 231000-CONCATENAR-NOME-TAB
               PERFORM 300000-FINALIZAR
           END-IF.

AT0522     IF  CARO01-COD-RETORNO      EQUAL 01
               MOVE 1                   TO GFCT8V-ERRO
AT0522         MOVE CARO01-COD-RETORNO  TO GFCT8V-COD-SQL-ERRO
               MOVE 0061                TO GFCT8V-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 111000-OBTER-DESCRICAO-MSG
               MOVE WRK-5000-DESC-MSG   TO GFCT8V-DESC-MSG-ERRO
               MOVE 1                   TO GFCT8V-ERR-SISTEMA
               MOVE GFCT8U-CCTRO-CUSTO  TO GFCT8V-COD-SISTEMA
               MOVE SPACES              TO GFCT8V-DESC-SISTEMA
AT0522         MOVE CARO01-COD-RETORNO  TO GFCT8V-COD-SQL-ERRO-GEN
               MOVE 0151                TO GFCT8V-COD-MSG-ERRO-GEN
                                          WRK-5000-COD-MSG
               PERFORM 111000-OBTER-DESCRICAO-MSG
               MOVE WRK-5000-DESC-MSG   TO GFCT8V-DESC-MSG-ERRO-GEN
           ELSE
AT0522         MOVE CARO01-CCTRO-CUSTO
                                        TO GFCT8V-COD-SISTEMA
AT0522         MOVE CARO01-ICTRO-CUSTO-ROTNA
                                        TO GFCT8V-DESC-SISTEMA
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
               UNTIL WRK-5000-DESC-MSG(IND-1:1) NOT EQUAL SPACES
                  OR IND-1                      EQUAL ZEROS
                  IF WRK-5000-DESC-MSG(IND-1:1) NOT EQUAL SPACES
                     MOVE WRK-5000-DESC-MSG(1:IND-1)
                                       TO GFCT8V-DESC-MSG-ERRO(1:IND-1)
                  END-IF
           END-PERFORM.

           MOVE SPACES                 TO GFCT8V-DESC-MSG-ERRO.

           MOVE WRK-5000-DESC-MSG(1:IND-1)
                                       TO GFCT8V-DESC-MSG-ERRO(1:IND-1).

           ADD 1                       TO IND-1.

           MOVE WRK-NOME-TABELA       TO GFCT8V-DESC-MSG-ERRO(IND-1:11).

      *----------------------------------------------------------------*
       231000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
AT0522 2400-SELECT-CAROB001-DEPDC      SECTION.
      *----------------------------------------------------------------*

AT0522     INITIALIZE                     CARO01-COMUAREA.

AT0522     MOVE 'S'                    TO CARO01-ACAO.
AT0522     MOVE 05                     TO CARO01-TIPO-PESQUISA.
AT0522     MOVE WRK-COD-JUNCAO-0315    TO CARO01-CJUNC-DEPDC.
AT0522     MOVE WRK-BUSCA              TO CARO01-CCTRO-CUSTO.

AT0522     CALL  WRK-CARO8200 USING CARO01-COMUAREA.

AT0522     IF (CARO01-COD-RETORNO      NOT EQUAL ZEROS AND 01  )
               MOVE 'GFCT5036'           TO GFCT0M-PROGRAMA
               MOVE 'DB2'                TO GFCT0M-TIPO-ACESSO
AT0522         MOVE 'TCARO_ATIV        ' TO GFCT0M-NOME-TAB
AT0522         MOVE 'CARO8200  '         TO GFCT0M-COMANDO-SQL
AT0522         MOVE CARO01-COD-RETORNO   TO GFCT0M-SQLCODE
                                            GFCT8V-COD-SQL-ERRO
               MOVE '0002'               TO GFCT0M-LOCAL
               MOVE 9                    TO GFCT8V-ERRO
               MOVE 0012                 TO GFCT8V-COD-MSG-ERRO
                                            WRK-5000-COD-MSG
               PERFORM 111000-OBTER-DESCRICAO-MSG
AT0522         MOVE 'CARO8200'           TO WRK-NOME-TAB
               PERFORM 231000-CONCATENAR-NOME-TAB
               PERFORM 300000-FINALIZAR
           END-IF.

AT0522     IF  CARO01-COD-RETORNO      EQUAL 01
               MOVE 1                   TO GFCT8V-ERRO
AT0522         MOVE CARO01-COD-RETORNO  TO GFCT8V-COD-SQL-ERRO
               MOVE 0061                TO GFCT8V-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 111000-OBTER-DESCRICAO-MSG
               MOVE WRK-5000-DESC-MSG   TO GFCT8V-DESC-MSG-ERRO
               MOVE 1                   TO GFCT8V-ERR-SISTEMA
               MOVE GFCT8U-CCTRO-CUSTO  TO GFCT8V-COD-SISTEMA
               MOVE SPACES              TO GFCT8V-DESC-SISTEMA
AT0522         MOVE CARO01-COD-RETORNO  TO GFCT8V-COD-SQL-ERRO-GEN
               MOVE 0151                TO GFCT8V-COD-MSG-ERRO-GEN
                                          WRK-5000-COD-MSG
               PERFORM 111000-OBTER-DESCRICAO-MSG
               MOVE WRK-5000-DESC-MSG   TO GFCT8V-DESC-MSG-ERRO-GEN
           ELSE
AT0522         MOVE CARO01-CCTRO-CUSTO
                                        TO GFCT8V-COD-SISTEMA
AT0522         MOVE CARO01-ICTRO-CUSTO-ROTNA
                                        TO GFCT8V-DESC-SISTEMA
           END-IF.

      *----------------------------------------------------------------*
AT0522 2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       300000-FINALIZAR                SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       300000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
