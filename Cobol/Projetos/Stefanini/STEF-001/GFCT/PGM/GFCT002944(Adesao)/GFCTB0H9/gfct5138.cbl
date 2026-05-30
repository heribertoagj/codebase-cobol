      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT5138.
       AUTHOR.     DOUGLAS ROGERIO RAMOS.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT5138                                    *
      *    PROGRAMADOR.:   DOUGLAS ROGERIO RAMOS   - CPM PATO BRANCO   *
      *    ANALISTA CPM:   JOSSIANE CARLA GAZZONI  - CPM PATO BRANCO   *
      *    ANALISTA....:   MARCELO CREMM           - PROCWORK / GP.50  *
      *    DATA........:   13/04/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   LISTA DE AGRUPAMENTOS DA EXCECAO, ONDE A    *
      *      AGENCIA   E  CONTA  IMPUTADAS  FACAM  PARTE  DE  ALGUM    *
      *      AGRUPAMENTO.                                              *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                        INCLUDE/BOOK     *
      *                   DB2PRD.V01MUNICIPIO           CBC0V002       *
      *                   DB2PRD.V01UF                  DCITV002       *
      *                   DB2PRD.PARM_DATA_PROCM        GFCTB0A1       *
      *                   DB2PRD.TPO_AGPTO_CTA          GFCTB0F3       *
      *                   DB2PRD.TADSAO_GRP_CLI         GFCTB0H9       *
      *                   DB2PRD.TTPO_SGMTO_GSTAO       GFCTB0H5       *
      *                   DB2PRD.ADSAO_COMP_PCOTE       GFCTB001       *
      *                   DB2PRD.ADSAO_GRP_DEPDC        GFCTB002       *
      *                   DB2PRD.ADSAO_GRP_EMPR         GFCTB003       *
      *                   DB2PRD.ADSAO_GRP_MUN          GFCTB004       *
      *                   DB2PRD.ADSAO_GRP_PAB          GFCTB005       *
      *                   DB2PRD.ADSAO_GRP_PSTAL        GFCTB006       *
      *                   DB2PRD.ADSAO_GRP_SGMTO        GFCTB007       *
      *                   DB2PRD.ADSAO_GRP_UF           GFCTB008       *
      *                   DB2PRD.EXCEC_ADSAO_COMP       GFCTB048       *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    I#GFCTO3 - AREA DE COMUNICACAO - ENTRADA                    *
      *    I#GFCTO4 - AREA DE COMUNICACAO - SAIDA                      *
      *    I#GFCTFZ - ENTRADA - DISPONIBILIDADE DO SISTEMA.            *
      *    I#GFCTG1 - SAIDA   - DISPONIBILIDADE DO SISTEMA.            *
      *    I#GFCTG2 - ENTRADA - OBTER MENSAGEM.                        *
      *    I#GFCTG3 - SAIDA   - OBTER MENSAGEM.                        *
      *    I#GFCTGB - ENTRADA - TEXT DE CORRESPONDENTE BANCARIO / PAB. *
      *    I#GFCTGC - SAIDA   - TEXT DE CORRESPONDENTE BANCARIO / PAB. *
      *    I#GFCT1V - ENTRADA - OBTER DESCRICAO DE DEPENDENCIA.        *
      *    I#GFCT1X - SAIDA   - OBTER DESCRICAO DE DEPENDENCIA.        *
      *    I#GFCTRK - ENTRADA E SAIDA - OBTER COD UF E MUNICIPIO DA    *
      *               AGENCIA.
      *    I#RURC88 - AREA PARA MODULO RURC9020.                       *
      *    I#GFCTJM - AREA DE COMUNICAO - ENTRADA / SAIDA.             *
      *    I#GFCT0M - AREA DE COMUNICACAO - ERROS.                     *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    DCIT8000 - OBTEM UF.                                        *
      *    GFCT5006 - DESCRICAO DEPENDENCIA.                           *
      *    GFCT5209 - DESCRICAO BANCO POSTAL OU BRADESCO EXPRESSO      *
      *    GFCT5109 - OBTER DESCRICAO DE PAB.                          *
      *    GFCT5522 - VERIFICAR ON-LINE.                               *
      *    GFCT5523 - OBTER DESCRICAO DE MENSAGENS.                    *
      *    GFCT5527 - OBTER CPF/CNPF/POSTO DE SERVICO.                 *
      *    GFCT5534 - OBTER CODIGO SEGMENTO GESTAO TARIFA.             *
      *    GFCT5564 - OBTER COD DA UF E MUNICIPIO DA AGENCIA.          *
      *    POOL0081 - ALOCAR DINAMICAMENTE UM MODULO.                  *
      *    RURC9020 - OBTEM MUNICIPIO.                                 *
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
           '* INICIO DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA VARIAVEIS AUXILIARES *'.
      *---------------------------------------------------------------*

       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-NOME-TAB                PIC  X(008)         VALUE SPACES.
       77  WRK-MSG-TAB                 PIC  X(100)         VALUE SPACES.
       77  WRK-INICIO                  PIC  9(008)         VALUE ZEROS.
       77  WRK-FIM                     PIC  9(008)         VALUE ZEROS.
       77  WRK-LISTA                   PIC  X(001)         VALUE SPACES.
       77  WRK-IO-PCB                  PIC  X(032)         VALUE SPACES.
       77  WRK-ALT-PCB                 PIC  X(032)         VALUE SPACES.
       77  WRK-AGRUP                   PIC S9(003) COMP-3  VALUE ZEROES.

       01  WRK-DPROCM-FTDA.
           05 WRK-DPROCM-FTDA-D        PIC  9(002)         VALUE ZEROS.
           05 FILLER                   PIC  X(001)         VALUE '.'.
           05 WRK-DPROCM-FTDA-M        PIC  9(002)         VALUE ZEROS.
           05 FILLER                   PIC  X(001)         VALUE '.'.
           05 WRK-DPROCM-FTDA-A        PIC  9(004)         VALUE ZEROS.

       01  WRK-INI-ADESAO              PIC  9(008)         VALUE ZEROS.
       01  WRK-INI-ADESAO-R            REDEFINES        WRK-INI-ADESAO.
           05 WRK-INI-ADESAO-D         PIC  9(002).
           05 WRK-INI-ADESAO-M         PIC  9(002).
           05 WRK-INI-ADESAO-A         PIC  9(004).

       01  WRK-DPROCM                  PIC  9(008)         VALUE ZEROS.
       01  WRK-DPROCM-R                REDEFINES           WRK-DPROCM.
           05 WRK-DPROCM-A             PIC  9(004).
           05 WRK-DPROCM-M             PIC  9(002).
           05 WRK-DPROCM-D             PIC  9(002).

       01  WRK-MSG05.
           05 FILLER                   PIC  X(026)         VALUE
              'ERRO NA CHAMADA AO MODULO '.
           05 WRK-NOME-MODULO          PIC  X(008)         VALUE SPACES.

       01  WRK-MSG06                   PIC  X(075)         VALUE
           'ERRO NOS PARAMETROS DO MODULO'.

       01  WRK-MSG07                   PIC  X(075)         VALUE
           'INCONSISTENCIA NOS PARAMETROS DO MODULO'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA INDEXADORES *'.
      *----------------------------------------------------------------*

       01  IND-1                       PIC  9(003) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5522 - ENTRADA *'.
      *---------------------------------------------------------------*

       COPY 'I#GFCTFZ'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5522 - SAIDA *'.
      *---------------------------------------------------------------*

       COPY 'I#GFCTG1'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5523 - ENTRADA *'.
      *---------------------------------------------------------------*

       COPY 'I#GFCTG2'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5523 - SAIDA *'.
      *---------------------------------------------------------------*

       COPY 'I#GFCTG3'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5109 - ENTRADA *'.
      *---------------------------------------------------------------*

       COPY 'I#GFCTGB'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5109 - SAIDA *'.
      *---------------------------------------------------------------*

       COPY 'I#GFCTGC'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5209 - ENTRADA *'.
      *---------------------------------------------------------------*

       COPY 'GFCTWABC'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5209 - SAIDA *'.
      *---------------------------------------------------------------*

       COPY 'GFCTWABD'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5006 - ENTRADA *'.
      *---------------------------------------------------------------*

       COPY 'I#GFCT1V'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5006 - SAIDA *'.
      *---------------------------------------------------------------*

       COPY 'I#GFCT1X'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5564 *'.
      *---------------------------------------------------------------*

       COPY 'I#GFCTRK'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM RURC9020 *'.
      *---------------------------------------------------------------*

       COPY 'I#RURC88'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5534 *'.
      *---------------------------------------------------------------*

       COPY 'I#GFCTJM'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA MODULO GFCT5527 *'.
      *----------------------------------------------------------------*

       01  WRK-5527-AREA-ENTRADA.
           05 WRK-5527-AMBIENTE            PIC  X(001)     VALUE SPACES.
           05 WRK-5527-CHAMADOR            PIC  X(008)     VALUE SPACES.
           05 WRK-5527-AGENCIA-CLIENTE     PIC  9(005)     VALUE ZEROS.
           05 WRK-5527-CONTA-CLIENTE.
             10 FILLER                     PIC  9(006)     VALUE ZEROS.
             10 WRK-5527-CONTA-CLIE        PIC  9(007)     VALUE ZEROS.

       01  WRK-5527-AREA-SAIDA.
           05 WRK-5527-COD-RETORNO         PIC  9(002)     VALUE ZEROS.
           05 WRK-5527-COD-SQL-ERRO        PIC S9(003)     VALUE ZEROS.
           05 WRK-5527-COD-MSG-GFCT        PIC  9(004)     VALUE ZEROS.
           05 WRK-5527-DESC-MSG            PIC  X(070)     VALUE SPACES.
           05 WRK-5527-QTD-OCORR           PIC  9(005)     VALUE ZEROS.
           05 WRK-5527-DADOS-RETORNO.
ST2506*      10 WRK-5527-CCGC-CPF        PIC  9(009)     VALUE ZEROS.
ST2506*      10 WRK-5527-CFLIAL-CGC      PIC  9(004)     VALUE ZEROS.
ST2506       10 WRK-5527-CCGC-CPF        PIC  X(009)     VALUE SPACES.
ST2506       10 WRK-5527-CFLIAL-CGC      PIC  X(004)     VALUE SPACES.
             10 WRK-5527-CCTRL-CPF-CGC   PIC  9(002)     VALUE ZEROS.
             10 WRK-5527-CPOSTO-SERVC    PIC  9(003)     VALUE ZEROS.
             10 WRK-5527-CSGMTO-CLI      PIC  9(003)     VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM O MODULO DCIT8000 *'.
      *----------------------------------------------------------------*

       01  WRK-DCIT-UNIDADE-FEDERATIVA.
           05 WRK-AREA-DCLGEN-DCITV002 PIC  X(070)         VALUE SPACES.
           05 WRK-DCIT-CODIGO-RETORNO  PIC  9(002)         VALUE ZEROS.
       01  WRK-ERRO-AREA               PIC  X(107)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA TABELA DB2 *'.
      *---------------------------------------------------------------*

           EXEC SQL
             INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
             INCLUDE DCITV002
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0A1
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0F3
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0H5
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0H9
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB001
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB002
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB003
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB004
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB005
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB006
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB007
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB008
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB048
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* FIM DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY 'I#GFCTO3'.

       COPY 'I#GFCTO4'.

       COPY 'I#GFCT0M'.

      *================================================================*
       PROCEDURE                       DIVISION USING GFCTO3-ENTRADA
                                                      GFCTO4-SAIDA
                                                      GFCT0M-AREA-ERROS.
      *================================================================*

      ******************************************************************
      *    CHAMA AS PRINCIPAIS ROTINAS DO PROCESSAMENTO                *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-PROCESSAR.

           PERFORM 3000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCEDIMENTOS INICIAIS DO PROGRAMA                          *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCT0M-AREA-ERROS
                                          GFCTO4-SAIDA.

           INITIALIZE GFCT0M-AREA-ERROS
                      GFCTO4-SAIDA.

           PERFORM 1100-VERIFICAR-ON-LINE.

           PERFORM 1200-MONTAR-AREA-SAIDA.

           PERFORM 1300-CONSISTIR-DADOS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICA A SISTUACAO DO SISTEMA.                            *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-VERIFICAR-ON-LINE          SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTO3-TRANSACAO           TO GFCTFZ-TRANSACAO.
           MOVE GFCTO3-FUNCAO              TO GFCTFZ-FUNCAO.
           MOVE GFCTO3-FUNC-BDSCO          TO GFCTFZ-FUNC-BDSCO.
           MOVE 'GFCT'                     TO GFCTFZ-SISTEMA.
           MOVE 'N'                        TO GFCTFZ-FIM.

           MOVE 'GFCT5522'                 TO WRK-MODULO.

           CALL 'POOL0081'              USING WRK-MODULO
                                              GFCTFZ-ENTRADA
                                              GFCTG1-SAIDA
                                              GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO             TO GFCT0M-PROGRAMA
                                              WRK-NOME-MODULO
               MOVE WRK-MSG05              TO GFCT0M-TEXTO
               MOVE 'APL'                  TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5138'             TO GFCT0M-TRANSACAO
               MOVE 9                      TO GFCTO4-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-ERRO             EQUAL 9
               MOVE 'GFCT5138'             TO GFCT0M-TRANSACAO
               MOVE GFCTG1-COD-SQL-ERRO    TO GFCT0M-SQLCODE
                                              GFCTO4-COD-SQL-ERRO
               MOVE GFCTG1-ERRO            TO GFCTO4-ERRO
               MOVE GFCTG1-COD-MSG-ERRO    TO GFCTO4-COD-MSG-ERRO
               MOVE GFCTG1-DESC-MSG-ERRO   TO GFCTO4-DESC-MSG-ERRO
                                              GFCT0M-TEXTO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-CSIT-ONLINE      EQUAL 1
               MOVE 9                      TO GFCTO4-ERRO
               MOVE 'SISTEMA INDISPONIVEL' TO GFCT0M-TEXTO
               MOVE WRK-MODULO             TO GFCT0M-PROGRAMA
               MOVE 'APL'                  TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5138'             TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    MONTA AREA DE SAIDA.                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       1200-MONTAR-AREA-SAIDA          SECTION.
      *----------------------------------------------------------------*

           MOVE 194                        TO GFCTO4-LL.
           MOVE ZEROS                      TO GFCTO4-ZZ.
           MOVE GFCTO3-TRANSACAO           TO GFCTO4-TRANSACAO.
           MOVE GFCTO3-FUNCAO              TO GFCTO4-FUNCAO.

           IF GFCTO3-QTDE-OCOR             NOT NUMERIC
               MOVE ZEROS                  TO GFCTO4-QTDE-OCOR
           ELSE
               MOVE GFCTO3-QTDE-OCOR       TO GFCTO4-QTDE-OCOR
           END-IF.

           IF GFCTO3-QTDE-TOT-REG          NOT NUMERIC
               MOVE ZEROS                  TO GFCTO4-QTDE-TOT-REG
           ELSE
               MOVE GFCTO3-QTDE-TOT-REG    TO GFCTO4-QTDE-TOT-REG
           END-IF.

           MOVE GFCTO3-FUNC-BDSCO          TO GFCTO4-FUNC-BDSCO.
           MOVE GFCTO3-TERMINAL            TO GFCTO4-TERMINAL.
           MOVE GFCTO3-FILTRO              TO GFCTO4-FILTRO.
           MOVE GFCTO3-PONT-AGENCIA        TO GFCTO4-PONT-AGENCIA.
           MOVE GFCTO3-PONT-CONTA          TO GFCTO4-PONT-CONTA.
           MOVE GFCTO3-PONT-PACOTE         TO GFCTO4-PONT-PACOTE.
           MOVE GFCTO3-PONT-CAGPTO         TO GFCTO4-PONT-CAGPTO.

           MOVE ZEROS                      TO GFCTO4-ERRO
                                              GFCTO4-COD-SQL-ERRO
                                              GFCTO4-COD-MSG-ERRO
                                              GFCTG2-COD-MSG.

           PERFORM 1310-OBTER-DESC-MENSAGEM.

           MOVE GFCTG3-DESC-MSG            TO GFCTO4-DESC-MSG-ERRO.

           MOVE 'N'                        TO GFCTO4-FIM.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    CONSISTE OS DADOS DA INC I#GFCTO3                           *
      ******************************************************************
      *----------------------------------------------------------------*
       1300-CONSISTIR-DADOS            SECTION.
      *----------------------------------------------------------------*

           IF (GFCTO3-TRANSACAO        EQUAL SPACES OR LOW-VALUES) OR
              (GFCTO3-FUNCAO           EQUAL SPACES OR LOW-VALUES) OR
              (GFCTO3-FUNC-BDSCO       EQUAL SPACES OR LOW-VALUES) OR
              (GFCTO3-QTDE-OCOR        NOT NUMERIC)                OR
              (GFCTO3-QTDE-TOT-REG     NOT NUMERIC)                OR
              (GFCTO3-FILT-AGENCIA     NOT NUMERIC                 OR
               GFCTO3-FILT-AGENCIA     EQUAL ZEROS)                OR
              (GFCTO3-FILT-CONTA       NOT NUMERIC                 OR
               GFCTO3-FILT-CONTA       EQUAL ZEROS)                OR
              (GFCTO3-FILT-PACOTE      NOT NUMERIC                 OR
               GFCTO3-FILT-PACOTE      EQUAL ZEROS)                OR
              (GFCTO3-PONT-AGENCIA     NOT NUMERIC)                OR
              (GFCTO3-PONT-CONTA       NOT NUMERIC)                OR
              (GFCTO3-PONT-PACOTE      NOT NUMERIC)                OR
              (GFCTO3-PONT-CAGPTO      NOT NUMERIC)
               MOVE 'S'                    TO GFCTO4-FIM
               MOVE 1                      TO GFCTO4-ERRO
                                              GFCTO4-COD-MSG-ERRO
               MOVE ZEROS                  TO GFCTO4-COD-SQL-ERRO
               MOVE '0001'                 TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG        TO GFCTO4-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           PERFORM 1320-SELECIONAR-GFCTB0A1.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    OBTEM DESCRICAO DA MENSAGEM.                                *
      ******************************************************************
      *----------------------------------------------------------------*
       1310-OBTER-DESC-MENSAGEM        SECTION.
      *----------------------------------------------------------------*

           MOVE 100                        TO GFCTG2-LL.
           MOVE ZEROS                      TO GFCTG2-ZZ.
           MOVE GFCTO3-TRANSACAO           TO GFCTG2-TRANSACAO.
           MOVE GFCTO3-FUNCAO              TO GFCTG2-FUNCAO.
           MOVE GFCTO3-FUNC-BDSCO          TO GFCTG2-FUNC-BDSCO.
           MOVE 'O'                        TO GFCTG2-TIPO-PROC.
           MOVE 'N'                        TO GFCTG2-FIM.

           MOVE SPACES                     TO GFCTG3-SAIDA.

           MOVE 'GFCT5523'                 TO WRK-MODULO.

           CALL 'POOL0081'                 USING WRK-MODULO
                                                 GFCTG2-ENTRADA
                                                 GFCTG3-SAIDA
                                                 GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                          TO GFCTO4-ERRO
               MOVE WRK-MODULO                 TO GFCT0M-PROGRAMA
                                                  WRK-NOME-MODULO
               MOVE WRK-MSG05                  TO GFCT0M-TEXTO
               MOVE 'APL'                      TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5138'                 TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           EVALUATE GFCTG3-ERRO
               WHEN 9
                   MOVE 9                      TO GFCTO4-ERRO
                   MOVE WRK-MODULO             TO GFCT0M-PROGRAMA
                                                  WRK-NOME-MODULO
                   MOVE WRK-MSG05              TO GFCT0M-TEXTO
                   MOVE 'APL'                  TO GFCT0M-TIPO-ACESSO
                   MOVE 'GFCT5138'             TO GFCT0M-TRANSACAO
                   PERFORM 3000-FINALIZAR
               WHEN 1
                   MOVE 'S'                    TO GFCTO4-FIM
                   MOVE 1                      TO GFCTO4-ERRO
                   MOVE GFCTG3-COD-SQL-ERRO    TO GFCTO4-COD-SQL-ERRO
                   MOVE GFCTG3-COD-MSG-ERRO    TO GFCTO4-COD-MSG-ERRO
                   MOVE GFCTG3-DESC-MSG-ERRO   TO GFCTO4-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
           END-EVALUATE.

      *----------------------------------------------------------------*
       1310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    SELECIONA O CAMPO DPROCM_ATUAL DA TABELA GFCTB0A1.          *
      ******************************************************************
      *----------------------------------------------------------------*
       1320-SELECIONAR-GFCTB0A1        SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC     OF GFCTB0A1.

           EXEC SQL
             SELECT
                   DPROCM_ATUAL
             INTO
                   :GFCTB0A1.DPROCM-ATUAL
             FROM   DB2PRD.PARM_DATA_PROCM
             WHERE
                   CSIST_PRINC        = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'PARM_DATA_PROCM ' TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
               MOVE '0010'             TO GFCT0M-LOCAL
               MOVE 0012               TO GFCTO4-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               MOVE 'GFCTB0A1'         TO WRK-NOME-TAB
               PERFORM 1321-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           PERFORM 1322-CONVERTER-DATA-DPROCM.

      *----------------------------------------------------------------*
       1320-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *   MOVE OS CAMPOS DE ERRO DE SQL PARA A INC I#GFCTO4 E I#GFCT0M *
      ******************************************************************
      *----------------------------------------------------------------*
       1321-MOVER-ERRO-SQL             SECTION.
      *----------------------------------------------------------------*

           MOVE 9                      TO GFCTO4-ERRO.
           MOVE 'GFCT5138'             TO GFCT0M-PROGRAMA.
           MOVE 'DB2'                  TO GFCT0M-TIPO-ACESSO.
           MOVE SQLCA                  TO GFCT0M-SQLCA-AREA.
           MOVE SQLCODE                TO GFCT0M-SQLCODE
                                          GFCTO4-COD-SQL-ERRO.
           MOVE SPACES                 TO GFCTO4-DESC-MSG-ERRO.
           PERFORM 1310-OBTER-DESC-MENSAGEM.
           STRING GFCTG3-DESC-MSG
               WRK-MSG-TAB
           DELIMITED BY ' '            INTO GFCTO4-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       1321-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    CONVERTE A DATA DD.MM.AAAA DO CAMPO DPROCM-ATUAL DA TABELA  *
      *    GFCTB0A1 PARA O FORMATO AAAAMMDD DO CAMPO WRK-DPROCM.       *
      ******************************************************************
      *----------------------------------------------------------------*
       1322-CONVERTER-DATA-DPROCM      SECTION.
      *----------------------------------------------------------------*

           MOVE DPROCM-ATUAL OF GFCTB0A1(1:2) TO WRK-DPROCM-D.
           MOVE DPROCM-ATUAL OF GFCTB0A1(4:2) TO WRK-DPROCM-M.
           MOVE DPROCM-ATUAL OF GFCTB0A1(7:4) TO WRK-DPROCM-A.

      *----------------------------------------------------------------*
       1322-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCESSAMENTO PRINCIPAL.                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           MOVE 1                      TO IND-1.

           PERFORM 2100-PROCESSAR-CLIENTE.

           PERFORM 2200-PROCESSAR-SEGMENTO.

           PERFORM 2300-PROCESSAR-AGENCIA.

           IF WRK-5527-CPOSTO-SERVC EQUAL ZEROS
              NEXT SENTENCE
           ELSE
              PERFORM 2400-PROCESSAR-PAB
              MOVE  17  TO  WRK-AGRUP
              PERFORM 2500-PROC-POSTAL-EXPRE
              MOVE  20  TO  WRK-AGRUP
              PERFORM 2500-PROC-POSTAL-EXPRE
           END-IF.

           PERFORM 2600-PROCESSAR-MUNICIPIO.

           PERFORM 2700-PROCESSAR-UF.

           IF IND-1                      EQUAL 1
               MOVE 'S'                    TO GFCTO4-FIM
               MOVE 1                      TO GFCTO4-ERRO
               MOVE 845                    TO GFCTO4-COD-MSG-ERRO
               MOVE '0845'                 TO GFCTG2-COD-MSG
               MOVE ZEROS                  TO GFCTO4-COD-SQL-ERRO
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG        TO GFCTO4-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE 'S'                        TO GFCTO4-FIM.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    REALIZA PROCESSAMENTO CLIENTE.                              *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-PROCESSAR-CLIENTE          SECTION.
      *----------------------------------------------------------------*

           PERFORM 2110-OBTER-CGC-CPF.
           PERFORM 2120-VE-SE-LISTA-CLIENTE.

           IF WRK-LISTA                  EQUAL 'S'
ST2506*        MOVE CCGC-CPF           OF GFCTB0H9
ST2506*                                TO GFCTO4-CHAVE-NUM9(IND-1)
ST2506*        MOVE CFLIAL-CGC         OF GFCTB0H9
ST2506*                                TO GFCTO4-CHAVE-NUM5(IND-1)
ST2506*        MOVE CCTRL-CPF-CGC      OF GFCTB0H9
ST2506*                                TO GFCTO4-CHAVE-NUM2(IND-1)
ST2506         MOVE CCGC-CPF-ST        OF GFCTB0H9
ST2506                                 TO GFCTO4-CHAVE-CCGC-CPF(IND-1)
ST2506         MOVE CFLIAL-CGC-ST      OF GFCTB0H9
ST2506                                 TO GFCTO4-CHAVE-FILIAL(IND-1)
ST2506         MOVE CCTRL-CPF-CGC-ST   OF GFCTB0H9
ST2506                                 TO GFCTO4-CHAVE-NUM2(IND-1)
               PERFORM 2140-LISTA-CAMPOS-DEFAULT
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    OBTEM CGC/CPF.                                              *
      ******************************************************************
      *----------------------------------------------------------------*
       2110-OBTER-CGC-CPF              SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  WRK-5527-CHAMADOR.

           MOVE 'O'                    TO WRK-5527-AMBIENTE.
           MOVE 'GFCT5138'             TO WRK-5527-CHAMADOR.
           MOVE GFCTO3-FILT-AGENCIA    TO WRK-5527-AGENCIA-CLIENTE.
           MOVE GFCTO3-FILT-CONTA      TO WRK-5527-CONTA-CLIENTE.

           MOVE 'GFCT5527'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             WRK-5527-AREA-ENTRADA
                                             WRK-5527-AREA-SAIDA
                                             GFCT0M-AREA-ERROS
                                             WRK-IO-PCB
                                             WRK-ALT-PCB.

           IF RETURN-CODE              NOT EQUAL ZEROS
               MOVE 9                            TO GFCTO4-ERRO
               MOVE WRK-MODULO                   TO WRK-NOME-MODULO
                                                    GFCT0M-PROGRAMA
               MOVE 'APL'                        TO GFCT0M-TIPO-ACESSO
               MOVE WRK-MSG05                    TO GFCT0M-TEXTO
               PERFORM 3000-FINALIZAR
           END-IF.

           EVALUATE TRUE
               WHEN WRK-5527-COD-RETORNO   EQUAL 9
                   MOVE 9                        TO GFCTO4-ERRO
                   MOVE WRK-5527-COD-MSG-GFCT    TO GFCTO4-COD-MSG-ERRO
                   MOVE WRK-5527-COD-SQL-ERRO    TO GFCTO4-COD-SQL-ERRO
                   MOVE WRK-5527-DESC-MSG        TO GFCTO4-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               WHEN WRK-5527-COD-RETORNO   NOT EQUAL ZEROS
                   MOVE 'S'                      TO GFCTO4-FIM
                   MOVE 1                        TO GFCTO4-ERRO
                   MOVE WRK-5527-COD-MSG-GFCT    TO GFCTO4-COD-MSG-ERRO
                   MOVE WRK-5527-COD-SQL-ERRO    TO GFCTO4-COD-SQL-ERRO
                   MOVE WRK-5527-DESC-MSG        TO GFCTO4-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
           END-EVALUATE.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICA A EXISTENCIA DE INFORMACOES DE ADESAO CLIENTE.     *
      ******************************************************************
      *----------------------------------------------------------------*
       2120-VE-SE-LISTA-CLIENTE        SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTO3-FILT-PACOTE       TO CSERVC-TARIF    OF GFCTB001.
           MOVE 04                       TO CAGPTO-CTA      OF GFCTB001.
           MOVE DPROCM-ATUAL OF GFCTB0A1 TO DFIM-ADSAO-COMP OF GFCTB001.

ST2506*    MOVE WRK-5527-CCGC-CPF        TO CCGC-CPF        OF GFCTB0H9.
ST2506*    MOVE WRK-5527-CFLIAL-CGC      TO CFLIAL-CGC      OF GFCTB0H9.
ST2506*    MOVE WRK-5527-CCTRL-CPF-CGC   TO CCTRL-CPF-CGC   OF GFCTB0H9.
ST2506     MOVE WRK-5527-CCGC-CPF        TO CCGC-CPF-ST     OF GFCTB0H9.
ST2506     MOVE WRK-5527-CFLIAL-CGC      TO CFLIAL-CGC-ST   OF GFCTB0H9.
ST2506     MOVE WRK-5527-CCTRL-CPF-CGC   TO CCTRL-CPF-CGC-ST 
ST2506                                                      OF GFCTB0H9.
           EXEC SQL
             SELECT
                   A.CSERVC_TARIF,
                   A.CAGPTO_CTA,
                   A.CSEQ_AGPTO_CTA,
                   A.DINIC_ADSAO_COMP,
                   A.DFIM_ADSAO_COMP,
ST2506*            B.CCGC_CPF,
ST2506*            B.CFLIAL_CGC,
ST2506*            B.CCTRL_CPF_CGC
ST2506             B.CCGC_CPF_ST,
ST2506             B.CFLIAL_CGC_ST,
ST2506             B.CCTRL_CPF_CGC_ST
             INTO
                   :GFCTB001.CSERVC-TARIF,
                   :GFCTB001.CAGPTO-CTA,
                   :GFCTB001.CSEQ-AGPTO-CTA,
                   :GFCTB001.DINIC-ADSAO-COMP,
                   :GFCTB001.DFIM-ADSAO-COMP,
ST2506*            :GFCTB0H9.CCGC-CPF,
ST2506*            :GFCTB0H9.CFLIAL-CGC,
ST2506*            :GFCTB0H9.CCTRL-CPF-CGC
ST2506             :GFCTB0H9.CCGC-CPF-ST,
ST2506             :GFCTB0H9.CFLIAL-CGC-ST,
ST2506             :GFCTB0H9.CCTRL-CPF-CGC-ST
             FROM   DB2PRD.ADSAO_COMP_PCOTE  A,
                    DB2PRD.TADSAO_GRP_CLI    B
             WHERE
                   A.CSERVC_TARIF      = B.CSERVC_TARIF             AND
                   A.CAGPTO_CTA        = B.CAGPTO_CTA               AND
                   A.CSEQ_AGPTO_CTA    = B.CSEQ_AGPTO_CTA           AND
                   A.DINIC_ADSAO_COMP  = B.DINIC_ADSAO_COMP         AND
                   A.CSERVC_TARIF      = :GFCTB001.CSERVC-TARIF     AND
                   A.CAGPTO_CTA        = :GFCTB001.CAGPTO-CTA       AND
                   A.DFIM_ADSAO_COMP   > :GFCTB001.DFIM-ADSAO-COMP  AND
ST2506*            B.CCGC_CPF          = :GFCTB0H9.CCGC-CPF         AND
ST2506*            B.CFLIAL_CGC        = :GFCTB0H9.CFLIAL-CGC       AND
ST2506*            B.CCTRL_CPF_CGC     = :GFCTB0H9.CCTRL-CPF-CGC
ST2506             B.CCGC_CPF_ST       = :GFCTB0H9.CCGC-CPF-ST      AND
ST2506             B.CFLIAL_CGC_ST     = :GFCTB0H9.CFLIAL-CGC-ST    AND
ST2506             B.CCTRL_CPF_CGC_ST  = :GFCTB0H9.CCTRL-CPF-CGC-ST
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'TADSAO_GRP_CLI'   TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
               MOVE '0020'             TO GFCT0M-LOCAL
               MOVE 0012               TO GFCTO4-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               MOVE 'GFCTB0H9'         TO WRK-NOME-TAB
               PERFORM 1321-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           ELSE
               IF SQLCODE             EQUAL ZEROS
                   PERFORM 2130-MONTA-VIGENCIAS
               ELSE
                   MOVE 'N'            TO WRK-LISTA
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    MONTA VIGENCIAS.                                            *
      ******************************************************************
      *----------------------------------------------------------------*
       2130-MONTA-VIGENCIAS            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2131-MOVE-DATAS.

           IF WRK-DPROCM-M             EQUAL WRK-INI-ADESAO-M
               IF WRK-DPROCM-M         LESS 12
                   MOVE 01             TO WRK-DPROCM-D
                   ADD   1             TO WRK-DPROCM-M
               ELSE
                   IF WRK-DPROCM-M     EQUAL 12
                       MOVE 01         TO WRK-DPROCM-D
                                          WRK-DPROCM-M
                       ADD  1          TO WRK-DPROCM-A
                   END-IF
               END-IF
               MOVE WRK-DPROCM-D       TO WRK-DPROCM-FTDA-D
               MOVE WRK-DPROCM-M       TO WRK-DPROCM-FTDA-M
               MOVE WRK-DPROCM-A       TO WRK-DPROCM-FTDA-A
               MOVE WRK-DPROCM-FTDA    TO GFCTO4-DATA-INICIO(IND-1)
           ELSE
               MOVE DINIC-ADSAO-COMP   OF GFCTB001
                                       TO GFCTO4-DATA-INICIO(IND-1)
           END-IF.

           MOVE GFCTO4-DATA-INICIO(IND-1)   TO WRK-DPROCM-FTDA.
           MOVE WRK-DPROCM-FTDA-A           TO WRK-INICIO(1:4).
           MOVE WRK-DPROCM-FTDA-M           TO WRK-INICIO(5:2).
           MOVE WRK-DPROCM-FTDA-D           TO WRK-INICIO(7:2).
           MOVE DFIM-ADSAO-COMP OF GFCTB001 TO WRK-DPROCM-FTDA.
           MOVE WRK-DPROCM-FTDA-A           TO WRK-FIM(1:4).
           MOVE WRK-DPROCM-FTDA-M           TO WRK-FIM(5:2).
           MOVE WRK-DPROCM-FTDA-D           TO WRK-FIM(7:2).

           IF WRK-INICIO               GREATER WRK-FIM
               MOVE 'N'                TO WRK-LISTA
               MOVE SPACES             TO GFCTO4-DATA-INICIO(IND-1)
           ELSE
               MOVE DFIM-ADSAO-COMP    OF GFCTB001
                                       TO GFCTO4-DATA-FIM(IND-1)
               MOVE 'S'                TO WRK-LISTA
           END-IF.

      *----------------------------------------------------------------*
       2130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    MOVIMENTA DATAS.                                            *
      ******************************************************************
      *----------------------------------------------------------------*
       2131-MOVE-DATAS                 SECTION.
      *----------------------------------------------------------------*

           MOVE DPROCM-ATUAL     OF GFCTB0A1(1:2) TO WRK-DPROCM-D.
           MOVE DPROCM-ATUAL     OF GFCTB0A1(4:2) TO WRK-DPROCM-M.
           MOVE DPROCM-ATUAL     OF GFCTB0A1(7:4) TO WRK-DPROCM-A.

           MOVE DINIC-ADSAO-COMP OF GFCTB001(1:2) TO WRK-INI-ADESAO-D.
           MOVE DINIC-ADSAO-COMP OF GFCTB001(4:2) TO WRK-INI-ADESAO-M.
           MOVE DINIC-ADSAO-COMP OF GFCTB001(7:4) TO WRK-INI-ADESAO-A.

      *----------------------------------------------------------------*
       2131-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    LISTA OS CAMPOS DEFAULT.                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2140-LISTA-CAMPOS-DEFAULT       SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA             OF GFCTB001
                                       TO GFCTO4-CAGPTO(IND-1).

           PERFORM 2141-OBTER-DESC-AGRUPAMENTO.

           MOVE RAGPTO-CTA             OF GFCTB0F3
                                       TO GFCTO4-RAGPTO(IND-1).
           MOVE CSEQ-AGPTO-CTA         OF GFCTB001
                                       TO GFCTO4-SEQUENCIA(IND-1).
           MOVE DINIC-ADSAO-COMP       OF GFCTB001
                                       TO GFCTO4-DINI-ADESAO(IND-1).
           MOVE CINDCD-AGPTO-TOT       OF GFCTB001
                                       TO GFCTO4-INDIC-TOTAL(IND-1).

           ADD 136                     TO GFCTO4-LL.
           ADD 1                       TO GFCTO4-QTDE-TOT-REG
                                          IND-1.

      *----------------------------------------------------------------*
       2140-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    OBTEM DESCRICAO AGRUPAMENTO.                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2141-OBTER-DESC-AGRUPAMENTO     SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA  OF GFCTB001    TO CAGPTO-CTA    OF GFCTB0F3.

           EXEC SQL
             SELECT
                   RAGPTO_CTA
             INTO
                   :GFCTB0F3.RAGPTO-CTA
             FROM   DB2PRD.TPO_AGPTO_CTA
             WHERE CAGPTO_CTA          = :GFCTB0F3.CAGPTO-CTA
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'TPO_AGPTO_CTA'    TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
               MOVE '0030'             TO GFCT0M-LOCAL
               MOVE 0012               TO GFCTO4-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               MOVE 'GFCTB0F3'         TO WRK-NOME-TAB
               PERFORM 1321-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2141-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCESSA SEGMENTO.                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-PROCESSAR-SEGMENTO         SECTION.
      *----------------------------------------------------------------*

           PERFORM 2210-OBTER-SEGMENTO.
           PERFORM 2220-VE-SE-LISTA-SEGMENTO.

           IF WRK-LISTA                  EQUAL 'S'
               MOVE CSGMTO-GSTAO-TARIF OF GFCTB007
                                       TO GFCTO4-CHAVE-NUM9(IND-1)
               PERFORM 2230-OBTER-DESC-SEGMENTO
               MOVE ISGMTO-GSTAO-TARIF OF GFCTB0H5
                                       TO GFCTO4-DESC-ARGUMENTO(IND-1)
               PERFORM 2140-LISTA-CAMPOS-DEFAULT
           END-IF.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    OBTEM SEGMENTO.                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       2210-OBTER-SEGMENTO             SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTJM-AREA-RETORNO.
           INITIALIZE                  GFCTJM-AREA-RETORNO.
           MOVE 'O'                    TO GFCTJM-TPO-AMBIENTE.
           MOVE GFCTO3-FILT-AGENCIA    TO GFCTJM-CJUNC-DEPDC.
           MOVE GFCTO3-FILT-CONTA      TO GFCTJM-CCTA-CLI.
           MOVE 'GFCT5534'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTJM-AREA
                                             GFCT0M-AREA-ERROS.

           IF RETURN-CODE              NOT EQUAL ZEROS
               MOVE 9                  TO GFCTO4-ERRO
               MOVE WRK-MODULO         TO WRK-NOME-MODULO
                                          GFCT0M-PROGRAMA
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 3000-FINALIZAR
           END-IF.

           EVALUATE TRUE
               WHEN GFCTJM-COD-RETORNO EQUAL 9
                   MOVE WRK-MODULO     TO WRK-NOME-MODULO
                                          GFCT0M-PROGRAMA
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
                   MOVE WRK-MSG05      TO GFCT0M-TEXTO
                   PERFORM 3000-FINALIZAR
               WHEN GFCTJM-COD-RETORNO NOT EQUAL ZEROS
                   MOVE ZEROS          TO GFCTJM-CSGMTO-GSTAO-TARIF
           END-EVALUATE.

      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICA A EXISTENCIA DE INFORMACOES DE ADESAO SEGMENTO.    *
      ******************************************************************
      *----------------------------------------------------------------*
       2220-VE-SE-LISTA-SEGMENTO       SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTO3-FILT-PACOTE       TO CSERVC-TARIF    OF GFCTB001.
           MOVE 09                       TO CAGPTO-CTA      OF GFCTB001.
           MOVE DPROCM-ATUAL OF GFCTB0A1 TO DFIM-ADSAO-COMP OF GFCTB001.

           MOVE GFCTJM-CSGMTO-GSTAO-TARIF TO CSGMTO-GSTAO-TARIF
                                          OF GFCTB007.

           EXEC SQL
             SELECT
                   A.CSERVC_TARIF,
                   A.CAGPTO_CTA,
                   A.CSEQ_AGPTO_CTA,
                   A.DINIC_ADSAO_COMP,
                   A.DFIM_ADSAO_COMP,
                   B.CSGMTO_GSTAO_TARIF
             INTO
                   :GFCTB001.CSERVC-TARIF,
                   :GFCTB001.CAGPTO-CTA,
                   :GFCTB001.CSEQ-AGPTO-CTA,
                   :GFCTB001.DINIC-ADSAO-COMP,
                   :GFCTB001.DFIM-ADSAO-COMP,
                   :GFCTB007.CSGMTO-GSTAO-TARIF
             FROM   DB2PRD.ADSAO_COMP_PCOTE  A,
                    DB2PRD.ADSAO_GRP_SGMTO   B
             WHERE
                   A.CSERVC_TARIF       = B.CSERVC_TARIF            AND
                   A.CAGPTO_CTA         = B.CAGPTO_CTA              AND
                   A.CSEQ_AGPTO_CTA     = B.CSEQ_AGPTO_CTA          AND
                   A.DINIC_ADSAO_COMP   = B.DINIC_ADSAO_COMP        AND
                   A.CSERVC_TARIF       = :GFCTB001.CSERVC-TARIF    AND
                   A.CAGPTO_CTA         = :GFCTB001.CAGPTO-CTA      AND
                   A.DFIM_ADSAO_COMP    > :GFCTB001.DFIM-ADSAO-COMP AND
                   B.CSGMTO_GSTAO_TARIF = :GFCTB007.CSGMTO-GSTAO-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'ADSAO_GRP_CLI'    TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
               MOVE '0040'             TO GFCT0M-LOCAL
               MOVE 0012               TO GFCTO4-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               MOVE 'GFCTB007'         TO WRK-NOME-TAB
               PERFORM 1321-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           ELSE
               IF SQLCODE             EQUAL ZEROS
                   PERFORM 2130-MONTA-VIGENCIAS
               ELSE
                   MOVE 'N'            TO WRK-LISTA
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    OBTEM DESCRICAO SEGMENTO.                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2230-OBTER-DESC-SEGMENTO        SECTION.
      *----------------------------------------------------------------*

           MOVE CSGMTO-GSTAO-TARIF     OF GFCTB007 TO CSGMTO-GSTAO-TARIF
                                       OF GFCTB0H5.

           EXEC SQL
               SELECT
                     ISGMTO_GSTAO_TARIF
               INTO
                     :GFCTB0H5.ISGMTO-GSTAO-TARIF
               FROM  DB2PRD.TTPO_SGMTO_GSTAO
               WHERE CSGMTO_GSTAO_TARIF = :GFCTB007.CSGMTO-GSTAO-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND -811) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE 'TTPO_SGMTO_GSTAO' TO GFCT0M-NOME-TAB
               MOVE '0050'             TO GFCT0M-LOCAL
               MOVE 0012               TO GFCTO4-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               MOVE 'GFCTB0H5'         TO WRK-NOME-TAB
               PERFORM 1321-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCESSA AGENCIA.                                           *
      ******************************************************************
      *----------------------------------------------------------------*
       2300-PROCESSAR-AGENCIA          SECTION.
      *----------------------------------------------------------------*

           PERFORM 2310-VE-SE-LISTA-AGENCIA.

           IF WRK-LISTA                  EQUAL 'S'
              MOVE CDEPDC OF GFCTB002   TO GFCTO4-CHAVE-NUM9(IND-1)
              IF CDEPDC OF GFCTB002 EQUAL ZEROS
                 MOVE 'TODAS' TO GFCTO4-DESC-ARGUMENTO(IND-1)
              ELSE
                 MOVE SPACES               TO GFCT1V-ENTRADA
                 MOVE CDEPDC OF GFCTB002   TO GFCT1V-COD-DEPDC(1)
                 PERFORM 2320-OBTER-DESC-AGENCIA
                 MOVE GFCT1X-DESC-DEPDC(1)
                                       TO GFCTO4-DESC-ARGUMENTO(IND-1)
                 PERFORM 2140-LISTA-CAMPOS-DEFAULT
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICAR EXISTENCIA DE INFORMACOES DE ADESAO AGENCIA.      *
      ******************************************************************
      *----------------------------------------------------------------*
       2310-VE-SE-LISTA-AGENCIA        SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTO3-FILT-PACOTE       TO CSERVC-TARIF    OF GFCTB001.
           MOVE 14                       TO CAGPTO-CTA      OF GFCTB001.
           MOVE DPROCM-ATUAL OF GFCTB0A1 TO DFIM-ADSAO-COMP OF GFCTB001.
           MOVE GFCTO3-FILT-AGENCIA      TO CDEPDC          OF GFCTB002.

           EXEC SQL
             SELECT
                   A.CSERVC_TARIF,
                   A.CAGPTO_CTA,
                   A.CSEQ_AGPTO_CTA,
                   A.DINIC_ADSAO_COMP,
                   A.DFIM_ADSAO_COMP,
                   A.CINDCD_AGPTO_TOT,
                   B.CDEPDC
             INTO
                   :GFCTB001.CSERVC-TARIF,
                   :GFCTB001.CAGPTO-CTA,
                   :GFCTB001.CSEQ-AGPTO-CTA,
                   :GFCTB001.DINIC-ADSAO-COMP,
                   :GFCTB001.DFIM-ADSAO-COMP,
                   :GFCTB001.CINDCD-AGPTO-TOT,
                   :GFCTB002.CDEPDC
             FROM   DB2PRD.ADSAO_COMP_PCOTE  A,
                    DB2PRD.ADSAO_GRP_DEPDC   B
             WHERE
                   A.CSERVC_TARIF       = B.CSERVC_TARIF            AND
                   A.CAGPTO_CTA         = B.CAGPTO_CTA              AND
                   A.CSEQ_AGPTO_CTA     = B.CSEQ_AGPTO_CTA          AND
                   A.DINIC_ADSAO_COMP   = B.DINIC_ADSAO_COMP        AND
                   A.CSERVC_TARIF       = :GFCTB001.CSERVC-TARIF    AND
                   A.CAGPTO_CTA         = :GFCTB001.CAGPTO-CTA      AND
                   A.DFIM_ADSAO_COMP    > :GFCTB001.DFIM-ADSAO-COMP AND
                 ( B.CDEPDC             = :GFCTB002.CDEPDC          OR
                   B.CDEPDC             = 0 )
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'ADSAO_GRP_DEPDC'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
               MOVE '0060'             TO GFCT0M-LOCAL
               MOVE 0012               TO GFCTO4-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               MOVE 'GFCTB002'         TO WRK-NOME-TAB
               PERFORM 1321-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           ELSE
               IF SQLCODE             EQUAL ZEROS
                   PERFORM 2130-MONTA-VIGENCIAS
               ELSE
                   MOVE 'N'            TO WRK-LISTA
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    OBTEM DESCRICAO DE AGENCIA.                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2320-OBTER-DESC-AGENCIA         SECTION.
      *----------------------------------------------------------------*

           MOVE 660                    TO GFCT1V-LL.
           MOVE ZEROS                  TO GFCT1V-ZZ.
           MOVE GFCTO3-TRANSACAO       TO GFCT1V-TRANSACAO.
           MOVE GFCTO3-FUNCAO          TO GFCT1V-FUNCAO.
           MOVE 001                    TO GFCT1V-QTDE-OCOR.
           MOVE ZEROS                  TO GFCT1V-QTDE-TOT-REG.
           MOVE GFCTO3-FUNC-BDSCO      TO GFCT1V-FUNC-BDSCO.
           MOVE 'N'                    TO GFCT1V-FIM.

           MOVE SPACES                 TO GFCT1X-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE                  GFCT1X-SAIDA
                                       GFCT0M-AREA-ERROS.

           MOVE 'GFCT5006'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCT1V-ENTRADA
                                             GFCT1X-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF RETURN-CODE              NOT EQUAL ZEROS
               MOVE 9                  TO GFCTO4-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-NOME-MODULO
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5138'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF GFCT1X-ERRO              NOT EQUAL ZEROS
               MOVE GFCT1X-ERRO        TO GFCTO4-ERRO
               MOVE GFCT1X-COD-SQL-ERRO TO GFCTO4-COD-SQL-ERRO
               MOVE GFCT1X-COD-MSG-ERRO TO GFCTO4-COD-MSG-ERRO
               IF GFCT1X-ERRO          EQUAL 1
                   MOVE 'S'                  TO GFCTO4-FIM
                   MOVE GFCT1X-DESC-MSG-ERRO TO GFCTO4-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5006-' GFCT1X-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO GFCTO4-DESC-MSG-ERRO
               END-IF
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2320-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCESSA PAB.                                               *
      ******************************************************************
      *----------------------------------------------------------------*
       2400-PROCESSAR-PAB              SECTION.
      *----------------------------------------------------------------*

           PERFORM 2410-VE-SE-LISTA-PAB.

           IF WRK-LISTA                  EQUAL 'S'
               MOVE CDEPDC OF GFCTB005 TO GFCTO4-CHAVE-NUM9(IND-1)
               MOVE CPOSTO-SERVC OF GFCTB005 TO GFCTO4-CHAVE-NUM5(IND-1)
               IF CPOSTO-SERVC OF GFCTB005 EQUAL ZEROS
                   MOVE 'TODOS'            TO GFCTO4-DESC-AUX(IND-1)
               ELSE
                   PERFORM 2420-OBTER-DESC-PAB
                   MOVE GFCTGC-DESC-POSTO(1) TO GFCTO4-DESC-AUX(IND-1)
               END-IF

               MOVE SPACES             TO GFCT1V-ENTRADA
               MOVE CDEPDC OF GFCTB005 TO GFCT1V-COD-DEPDC(1)
               PERFORM 2320-OBTER-DESC-AGENCIA
               MOVE GFCT1X-DESC-DEPDC(1) TO GFCTO4-DESC-ARGUMENTO(IND-1)
               PERFORM 2140-LISTA-CAMPOS-DEFAULT
           END-IF.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICA A EXISTENCIA DE INFORMACOES DE ADESAO AGENCIA/PAB  *
      ******************************************************************
      *----------------------------------------------------------------*
       2410-VE-SE-LISTA-PAB            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTO3-FILT-PACOTE       TO CSERVC-TARIF    OF GFCTB001.
           MOVE 15                       TO CAGPTO-CTA      OF GFCTB001.
           MOVE DPROCM-ATUAL OF GFCTB0A1 TO DFIM-ADSAO-COMP OF GFCTB001.
           MOVE GFCTO3-FILT-AGENCIA      TO CDEPDC          OF GFCTB005.
           MOVE WRK-5527-CPOSTO-SERVC    TO CPOSTO-SERVC    OF GFCTB005.

           EXEC SQL
             SELECT
                   A.CSERVC_TARIF,
                   A.CAGPTO_CTA,
                   A.CSEQ_AGPTO_CTA,
                   A.DINIC_ADSAO_COMP,
                   A.DFIM_ADSAO_COMP,
                   A.CINDCD_AGPTO_TOT,
                   B.CDEPDC,
                   B.CPOSTO_SERVC
             INTO
                   :GFCTB001.CSERVC-TARIF,
                   :GFCTB001.CAGPTO-CTA,
                   :GFCTB001.CSEQ-AGPTO-CTA,
                   :GFCTB001.DINIC-ADSAO-COMP,
                   :GFCTB001.DFIM-ADSAO-COMP,
                   :GFCTB001.CINDCD-AGPTO-TOT,
                   :GFCTB005.CDEPDC,
                   :GFCTB005.CPOSTO-SERVC
             FROM   DB2PRD.ADSAO_COMP_PCOTE  A,
                    DB2PRD.ADSAO_GRP_PAB     B
             WHERE
                   A.CSERVC_TARIF       = B.CSERVC_TARIF            AND
                   A.CAGPTO_CTA         = B.CAGPTO_CTA              AND
                   A.CSEQ_AGPTO_CTA     = B.CSEQ_AGPTO_CTA          AND
                   A.DINIC_ADSAO_COMP   = B.DINIC_ADSAO_COMP        AND
                   A.CSERVC_TARIF       = :GFCTB001.CSERVC-TARIF    AND
                   A.CAGPTO_CTA         = :GFCTB001.CAGPTO-CTA      AND
                   A.DFIM_ADSAO_COMP    > :GFCTB001.DFIM-ADSAO-COMP AND
                   B.CDEPDC             = :GFCTB005.CDEPDC          AND
                 ( B.CPOSTO_SERVC       = :GFCTB005.CPOSTO-SERVC    OR
                   B.CPOSTO_SERVC       = 0 )
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'ADSAO_GRP_PAB'    TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
               MOVE '0070'             TO GFCT0M-LOCAL
               MOVE 0012               TO GFCTO4-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               MOVE 'GFCTB005'         TO WRK-NOME-TAB
               PERFORM 1321-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           ELSE
               IF SQLCODE             EQUAL ZEROS
                   PERFORM 2130-MONTA-VIGENCIAS
               ELSE
                   MOVE 'N'            TO WRK-LISTA
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2410-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    OBTER DESCRICAO PAB.                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       2420-OBTER-DESC-PAB             SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  GFCTGB-ENTRADA.

           MOVE 100                    TO GFCTGB-LL.
           MOVE ZEROS                  TO GFCTGB-ZZ.
           MOVE GFCTO3-TRANSACAO       TO GFCTGB-TRANSACAO.
           MOVE GFCTO3-FUNCAO          TO GFCTGB-FUNCAO.
           MOVE ZEROS                  TO GFCTGB-QTDE-OCOR.
           MOVE ZEROS                  TO GFCTGB-QTDE-TOT-REG.
           MOVE GFCTO3-FUNC-BDSCO      TO GFCTGB-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTGB-FIM.
           MOVE CDEPDC       OF GFCTB005 TO GFCTGB-COD-DEPDC.
           MOVE CPOSTO-SERVC OF GFCTB005 TO GFCTGB-COD-POSTO.


           MOVE SPACES                 TO GFCTGC-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE                  GFCTGC-SAIDA
                                       GFCT0M-AREA-ERROS.

           MOVE 'GFCT5109'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTGB-ENTRADA
                                             GFCTGC-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF RETURN-CODE              NOT EQUAL ZEROS
               MOVE 9                  TO GFCTO4-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-NOME-MODULO
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0137'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF GFCTGC-ERRO              NOT EQUAL ZEROS
               MOVE GFCTGC-ERRO        TO GFCTO4-ERRO
               MOVE GFCTGC-COD-SQL-ERRO TO GFCTO4-COD-SQL-ERRO
               MOVE GFCTGC-COD-MSG-ERRO TO GFCTO4-COD-MSG-ERRO

               IF GFCTGC-ERRO          EQUAL 1
                   MOVE 'S'                  TO GFCTO4-FIM
                   MOVE GFCTGC-DESC-MSG-ERRO TO GFCTO4-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5109-' GFCTGC-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO GFCTO4-DESC-MSG-ERRO
               END-IF
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2420-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCESSA POSTAL.                                            *
      ******************************************************************
      *----------------------------------------------------------------*
       2500-PROC-POSTAL-EXPRE          SECTION.
      *----------------------------------------------------------------*

           PERFORM 2510-VE-SE-LISTA-POSTAL.

           IF WRK-LISTA                  EQUAL 'S'
               MOVE CDEPDC OF GFCTB006 TO GFCTO4-CHAVE-NUM9(IND-1)
               MOVE CPOSTO-SERVC OF GFCTB006 TO GFCTO4-CHAVE-NUM5(IND-1)
               IF CPOSTO-SERVC OF GFCTB006 EQUAL ZEROS
                   MOVE 'TODOS'            TO GFCTO4-DESC-AUX(IND-1)
               ELSE
                   PERFORM 2520-OBTER-DESC-POSTAL
                   MOVE WABD-DESC-POSTO(1) TO GFCTO4-DESC-AUX(IND-1)
               END-IF
               MOVE SPACES             TO GFCT1V-ENTRADA
               IF CDEPDC OF GFCTB006 EQUAL ZEROS
                   MOVE 'TODOS'        TO GFCTO4-DESC-ARGUMENTO(IND-1)
               ELSE
                   MOVE CDEPDC OF GFCTB006 TO GFCT1V-COD-DEPDC(1)
                   PERFORM 2320-OBTER-DESC-AGENCIA
                   MOVE GFCT1X-DESC-DEPDC(1)
                                       TO GFCTO4-DESC-ARGUMENTO(IND-1)
               END-IF
               PERFORM 2140-LISTA-CAMPOS-DEFAULT
           END-IF.

      *----------------------------------------------------------------*
       2500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VEREFICA A EXISTENCIA DE INFORMACOES DE ADESAO              *
      *    AGENCIA/POSTAL.                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       2510-VE-SE-LISTA-POSTAL         SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTO3-FILT-PACOTE      TO CSERVC-TARIF     OF GFCTB001.
           MOVE WRK-AGRUP               TO CAGPTO-CTA       OF GFCTB001.
           MOVE DPROCM-ATUAL OF GFCTB0A1 TO DFIM-ADSAO-COMP OF GFCTB001.
           MOVE GFCTO3-FILT-AGENCIA      TO CDEPDC          OF GFCTB006.
           MOVE WRK-5527-CPOSTO-SERVC    TO CPOSTO-SERVC    OF GFCTB006.

           EXEC SQL
             SELECT
                   A.CSERVC_TARIF,
                   A.CAGPTO_CTA,
                   A.CSEQ_AGPTO_CTA,
                   A.DINIC_ADSAO_COMP,
                   A.DFIM_ADSAO_COMP,
                   A.CINDCD_AGPTO_TOT,
                   B.CDEPDC,
                   B.CPOSTO_SERVC
             INTO
                   :GFCTB001.CSERVC-TARIF,
                   :GFCTB001.CAGPTO-CTA,
                   :GFCTB001.CSEQ-AGPTO-CTA,
                   :GFCTB001.DINIC-ADSAO-COMP,
                   :GFCTB001.DFIM-ADSAO-COMP,
                   :GFCTB001.CINDCD-AGPTO-TOT,
                   :GFCTB006.CDEPDC,
                   :GFCTB006.CPOSTO-SERVC
             FROM   DB2PRD.ADSAO_COMP_PCOTE  A,
                    DB2PRD.ADSAO_GRP_PSTAL   B
             WHERE
                   A.CSERVC_TARIF       = B.CSERVC_TARIF            AND
                   A.CAGPTO_CTA         = B.CAGPTO_CTA              AND
                   A.CSEQ_AGPTO_CTA     = B.CSEQ_AGPTO_CTA          AND
                   A.DINIC_ADSAO_COMP   = B.DINIC_ADSAO_COMP        AND
                   A.CSERVC_TARIF       = :GFCTB001.CSERVC-TARIF    AND
                   A.CAGPTO_CTA         = :GFCTB001.CAGPTO-CTA      AND
                   A.DFIM_ADSAO_COMP    > :GFCTB001.DFIM-ADSAO-COMP AND
PW1334            (B.CDEPDC             = 0                         OR
PW1334             B.CDEPDC             = :GFCTB006.CDEPDC          AND
PW1334             B.CPOSTO_SERVC       = :GFCTB006.CPOSTO-SERVC )
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'ADSAO_GRP_PSTAL'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
               MOVE '0080'             TO GFCT0M-LOCAL
               MOVE 0012               TO GFCTO4-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               MOVE 'GFCTB006'         TO WRK-NOME-TAB
               PERFORM 1321-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           ELSE
               IF SQLCODE             EQUAL ZEROS
                   PERFORM 2130-MONTA-VIGENCIAS
               ELSE
                   MOVE 'N'            TO WRK-LISTA
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2510-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    OBTEM DESCRICAO POSTAL.                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2520-OBTER-DESC-POSTAL          SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                      TO WABC-ENTRADA.

           MOVE 103                         TO WABC-LL.
           MOVE ZEROS                       TO WABC-ZZ
                                               WABC-QTDE-OCOR
                                               WABC-QTDE-TOT-REG.
           MOVE GFCTO3-TRANSACAO            TO WABC-TRANSACAO.
           MOVE GFCTO3-FUNCAO               TO WABC-FUNCAO.
           MOVE GFCTO3-FUNC-BDSCO           TO WABC-FUNC-BDSCO.
           MOVE 'N'                         TO WABC-FIM.
           MOVE CDEPDC       OF GFCTB006    TO WABC-COD-DEPDC.
           MOVE CPOSTO-SERVC OF GFCTB006    TO WABC-COD-POSTO.
           MOVE WRK-AGRUP                   TO WABC-COD-AGRUP.

           MOVE 'GFCT5209'                  TO WRK-MODULO.

           MOVE SPACES                      TO WABD-SAIDA
                                               GFCT0M-AREA-ERROS.

           INITIALIZE                  WABD-SAIDA
                                       GFCT0M-AREA-ERROS.

           CALL 'POOL0081'             USING WRK-MODULO
                                             WABC-ENTRADA
                                             WABD-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF RETURN-CODE              NOT EQUAL ZEROS
               MOVE 9                       TO GFCTO4-ERRO
               MOVE WRK-MODULO              TO GFCT0M-PROGRAMA
                                               WRK-NOME-MODULO
               MOVE WRK-MSG05               TO GFCT0M-TEXTO
               MOVE 'APL'                   TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5138'              TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF WABD-ERRO              NOT EQUAL ZEROS
               MOVE WABD-ERRO             TO GFCTO4-ERRO
               MOVE WABD-COD-SQL-ERRO     TO GFCTO4-COD-SQL-ERRO
               MOVE WABD-COD-MSG-ERRO     TO GFCTO4-COD-MSG-ERRO

               IF WABD-ERRO          EQUAL 1
                  MOVE 'S'                TO GFCTO4-FIM
                  MOVE WABD-DESC-MSG-ERRO TO GFCTO4-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5209-'
                          WABD-DESC-MSG-ERRO
                   DELIMITED BY SIZE      INTO GFCTO4-DESC-MSG-ERRO
               END-IF
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2520-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    REALIZA PROCESSAMENTO DO MUNICIPIO.                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2600-PROCESSAR-MUNICIPIO        SECTION.
      *----------------------------------------------------------------*

           PERFORM 2610-OBTER-UF-MUNICIPIO.
           PERFORM 2620-VE-SE-LISTA-MUNICIPIO.

           IF WRK-LISTA                  EQUAL 'S'
               MOVE CMUN-IBGE OF GFCTB004 TO GFCTO4-CHAVE-NUM9(IND-1)
               PERFORM 2630-OBTER-DESC-MUNICIPIO
               MOVE NOME-MUNIC-RURC88    TO GFCTO4-DESC-ARGUMENTO(IND-1)
               PERFORM 2140-LISTA-CAMPOS-DEFAULT
           END-IF.

      *----------------------------------------------------------------*
       2600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    OBTEM CODIGO DO MUNICIPIO E SIGLA DA UF.                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2610-OBTER-UF-MUNICIPIO         SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTRK-AREA.

           INITIALIZE GFCTRK-AREA.

           MOVE 'O'                    TO GFCTRK-TPO-AMBIENTE.
           MOVE GFCTO3-FILT-AGENCIA    TO GFCTRK-CJUNC-DEPDC.

           MOVE 'GFCT5564'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTRK-AREA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTO4-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-NOME-MODULO
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5138'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTRK-COD-RETORNO      EQUAL 01 OR 02 OR 99
               MOVE 9                  TO GFCTO4-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-NOME-MODULO
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5138'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTRK-COD-RETORNO      NOT EQUAL ZEROS
               MOVE ZEROS              TO GFCTRK-CMUN-RURAL
               MOVE SPACES             TO GFCTRK-CSGL-UF
           END-IF.

      *----------------------------------------------------------------*
       2610-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICA A EXISTENCIA DE INFORMACOES DE ADESAO MUNICIPIO.   *
      ******************************************************************
      *----------------------------------------------------------------*
       2620-VE-SE-LISTA-MUNICIPIO      SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTO3-FILT-PACOTE       TO CSERVC-TARIF    OF GFCTB001.
           MOVE 18                       TO CAGPTO-CTA      OF GFCTB001.
           MOVE DPROCM-ATUAL OF GFCTB0A1 TO DFIM-ADSAO-COMP OF GFCTB001.
           MOVE GFCTRK-CMUN-RURAL        TO CMUN-IBGE       OF GFCTB004.

           EXEC SQL
             SELECT
                   A.CSERVC_TARIF,
                   A.CAGPTO_CTA,
                   A.CSEQ_AGPTO_CTA,
                   A.DINIC_ADSAO_COMP,
                   A.DFIM_ADSAO_COMP,
                   B.CMUN_IBGE
             INTO
                   :GFCTB001.CSERVC-TARIF,
                   :GFCTB001.CAGPTO-CTA,
                   :GFCTB001.CSEQ-AGPTO-CTA,
                   :GFCTB001.DINIC-ADSAO-COMP,
                   :GFCTB001.DFIM-ADSAO-COMP,
                   :GFCTB004.CMUN-IBGE
             FROM   DB2PRD.ADSAO_COMP_PCOTE  A,
                    DB2PRD.ADSAO_GRP_MUN     B
             WHERE
                   A.CSERVC_TARIF       = B.CSERVC_TARIF            AND
                   A.CAGPTO_CTA         = B.CAGPTO_CTA              AND
                   A.CSEQ_AGPTO_CTA     = B.CSEQ_AGPTO_CTA          AND
                   A.DINIC_ADSAO_COMP   = B.DINIC_ADSAO_COMP        AND
                   A.CSERVC_TARIF       = :GFCTB001.CSERVC-TARIF    AND
                   A.CAGPTO_CTA         = :GFCTB001.CAGPTO-CTA      AND
                   A.DFIM_ADSAO_COMP    > :GFCTB001.DFIM-ADSAO-COMP AND
                   B.CMUN_IBGE          = :GFCTB004.CMUN-IBGE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'ADSAO_GRP_MUN'    TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
               MOVE '0090'             TO GFCT0M-LOCAL
               MOVE 0012               TO GFCTO4-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               MOVE 'GFCTB004'         TO WRK-NOME-TAB
               PERFORM 1321-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           ELSE
               IF SQLCODE             EQUAL ZEROS
                   PERFORM 2130-MONTA-VIGENCIAS
               ELSE
                   MOVE 'N'            TO WRK-LISTA
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2620-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    OBTEM DESCRICAO DO MUNICIPIO.                               *
      ******************************************************************
      *----------------------------------------------------------------*
       2630-OBTER-DESC-MUNICIPIO       SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  AREA-RURC88.

           MOVE CMUN-IBGE OF GFCTB004      TO COD-MUNIC-RURC88.
           MOVE 'RURC9020'                 TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             AREA-RURC88.

           IF RETURN-CODE              NOT EQUAL ZEROS
               MOVE 'S'                    TO GFCTO4-FIM
               MOVE 1                      TO GFCTO4-ERRO
               MOVE ZEROS                  TO GFCTO4-COD-SQL-ERRO
               MOVE 732                    TO GFCTO4-COD-MSG-ERRO
               MOVE '0732'                 TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG        TO GFCTO4-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           EVALUATE COD-RETORNO-RURC88
               WHEN 10
                   MOVE 'S'                    TO GFCTO4-FIM
                   MOVE 1                      TO GFCTO4-ERRO
                   MOVE ZEROS                  TO GFCTO4-COD-SQL-ERRO
                   MOVE 675                    TO GFCTO4-COD-MSG-ERRO
                   MOVE '0675'                 TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESC-MENSAGEM
                   MOVE GFCTG3-DESC-MSG        TO GFCTO4-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               WHEN 20
                   MOVE 'S'                    TO GFCTO4-FIM
                   MOVE 1                      TO GFCTO4-ERRO
                   MOVE ZEROS                  TO GFCTO4-COD-SQL-ERRO
                   MOVE 733                    TO GFCTO4-COD-MSG-ERRO
                   MOVE '0733'                 TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESC-MENSAGEM
                   MOVE GFCTG3-DESC-MSG        TO GFCTO4-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               WHEN 99
                   MOVE 'S'                    TO GFCTO4-FIM
                   MOVE 1                      TO GFCTO4-ERRO
                   MOVE SQLCODE-RURC88         TO GFCTO4-COD-SQL-ERRO
                   MOVE 734                    TO GFCTO4-COD-MSG-ERRO
                   MOVE '0734'                 TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESC-MENSAGEM
                   MOVE GFCTG3-DESC-MSG        TO GFCTO4-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
           END-EVALUATE.

      *----------------------------------------------------------------*
       2630-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    REALIZA O PROCESSAMENTO DA UF.                              *
      ******************************************************************
      *----------------------------------------------------------------*
       2700-PROCESSAR-UF               SECTION.
      *----------------------------------------------------------------*

           PERFORM 2710-VE-SE-LISTA-UF.

           IF WRK-LISTA                  EQUAL 'S'
               MOVE CSGL-UF OF GFCTB008 TO GFCTO4-CHAVE-ALP2(IND-1)
               PERFORM 2720-OBTER-DESC-UF
               MOVE IUF OF DCITV002     TO GFCTO4-DESC-ARGUMENTO(IND-1)
               PERFORM 2140-LISTA-CAMPOS-DEFAULT
           END-IF.

      *----------------------------------------------------------------*
       2700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICA A EXISTENCIA DE INFORMACOES DE ADESAO DA UF.       *
      ******************************************************************
      *----------------------------------------------------------------*
       2710-VE-SE-LISTA-UF             SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTO3-FILT-PACOTE      TO CSERVC-TARIF     OF GFCTB001.
           MOVE 19                      TO CAGPTO-CTA       OF GFCTB001.
           MOVE DPROCM-ATUAL OF GFCTB0A1 TO DFIM-ADSAO-COMP OF GFCTB001.
           MOVE GFCTRK-CSGL-UF             TO CSGL-UF       OF GFCTB008.

           EXEC SQL
             SELECT
                   A.CSERVC_TARIF,
                   A.CAGPTO_CTA,
                   A.CSEQ_AGPTO_CTA,
                   A.DINIC_ADSAO_COMP,
                   A.DFIM_ADSAO_COMP,
                   B.CSGL_UF
             INTO
                   :GFCTB001.CSERVC-TARIF,
                   :GFCTB001.CAGPTO-CTA,
                   :GFCTB001.CSEQ-AGPTO-CTA,
                   :GFCTB001.DINIC-ADSAO-COMP,
                   :GFCTB001.DFIM-ADSAO-COMP,
                   :GFCTB008.CSGL-UF
             FROM   DB2PRD.ADSAO_COMP_PCOTE  A,
                    DB2PRD.ADSAO_GRP_UF      B
             WHERE
                   A.CSERVC_TARIF       = B.CSERVC_TARIF            AND
                   A.CAGPTO_CTA         = B.CAGPTO_CTA              AND
                   A.CSEQ_AGPTO_CTA     = B.CSEQ_AGPTO_CTA          AND
                   A.DINIC_ADSAO_COMP   = B.DINIC_ADSAO_COMP        AND
                   A.CSERVC_TARIF       = :GFCTB001.CSERVC-TARIF    AND
                   A.CAGPTO_CTA         = :GFCTB001.CAGPTO-CTA      AND
                   A.DFIM_ADSAO_COMP    > :GFCTB001.DFIM-ADSAO-COMP AND
                   B.CSGL_UF            = :GFCTB008.CSGL-UF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'ADSAO_GRP_UF'     TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
               MOVE '0100'             TO GFCT0M-LOCAL
               MOVE 0012               TO GFCTO4-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               MOVE 'GFCTB008'         TO WRK-NOME-TAB
               PERFORM 1321-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           ELSE
               IF SQLCODE             EQUAL ZEROS
                   PERFORM 2130-MONTA-VIGENCIAS
               ELSE
                   MOVE 'N'            TO WRK-LISTA
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2710-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    OBTEM DESCRICAO DA UNIDADE FEDERATIVA.                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2720-OBTER-DESC-UF              SECTION.
      *----------------------------------------------------------------*

           MOVE CSGL-UF OF GFCTB008         TO CSGL-UF OF DCITV002.
           MOVE DCITV002                    TO WRK-AREA-DCLGEN-DCITV002.
           MOVE 'DCIT8000'                  TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             WRK-DCIT-UNIDADE-FEDERATIVA
                                             WRK-ERRO-AREA
                                             SQLCA.

           IF RETURN-CODE              NOT EQUAL ZEROS
               MOVE 'S'                      TO GFCTO4-FIM
               MOVE 1                        TO GFCTO4-ERRO
               MOVE ZEROS                    TO GFCTO4-COD-SQL-ERRO
               MOVE 735                      TO GFCTO4-COD-MSG-ERRO
               MOVE '0735'                   TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG          TO GFCTO4-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           EVALUATE WRK-DCIT-CODIGO-RETORNO
               WHEN 01
                   MOVE 'S'                  TO GFCTO4-FIM
                   MOVE 1                    TO GFCTO4-ERRO
                   MOVE ZEROS                TO GFCTO4-COD-SQL-ERRO
                   MOVE 736                  TO GFCTO4-COD-MSG-ERRO
                   MOVE '0736'               TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESC-MENSAGEM
                   MOVE GFCTG3-DESC-MSG      TO GFCTO4-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               WHEN 02
                   MOVE 'S'                  TO GFCTO4-FIM
                   MOVE 1                    TO GFCTO4-ERRO
                   MOVE ZEROS                TO GFCTO4-COD-SQL-ERRO
                   MOVE 737                  TO GFCTO4-COD-MSG-ERRO
                   MOVE '0737'               TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESC-MENSAGEM
                   MOVE GFCTG3-DESC-MSG      TO GFCTO4-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               WHEN 03
                   MOVE 'S'                  TO GFCTO4-FIM
                   MOVE 1                    TO GFCTO4-ERRO
                   MOVE ZEROS                TO GFCTO4-COD-SQL-ERRO
                   MOVE 738                  TO GFCTO4-COD-MSG-ERRO
                   MOVE '0738'               TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESC-MENSAGEM
                   MOVE GFCTG3-DESC-MSG      TO GFCTO4-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               WHEN 04
                   INITIALIZE               GFCT0M-ERRO-SQL
                   MOVE 'DCITV002'           TO WRK-NOME-TAB
                   MOVE 'V01_UF'             TO GFCT0M-NOME-TAB
                   MOVE 'SELECT'             TO GFCT0M-COMANDO-SQL
                   MOVE '0090'               TO GFCT0M-LOCAL
                   MOVE 0005                 TO GFCTO4-COD-MSG-ERRO
                                                GFCTG2-COD-MSG
                   MOVE 9                    TO GFCTO4-ERRO
                   MOVE 'GFCT5138'           TO GFCT0M-PROGRAMA
                   MOVE 'DB2'                TO GFCT0M-TIPO-ACESSO
                   MOVE SQLCA                TO GFCT0M-SQLCA-AREA
                   MOVE SQLCODE              TO GFCT0M-SQLCODE
                                                GFCTO4-COD-SQL-ERRO
                   MOVE SPACES               TO GFCTO4-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
           END-EVALUATE.

           MOVE WRK-AREA-DCLGEN-DCITV002     TO DCITV002.

      *----------------------------------------------------------------*
       2720-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    FINALIZA O PROCESSAMENTO E RETORNA AO PROGRAMA CHAMADOR.    *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO RETURN-CODE.

           GOBACK.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
