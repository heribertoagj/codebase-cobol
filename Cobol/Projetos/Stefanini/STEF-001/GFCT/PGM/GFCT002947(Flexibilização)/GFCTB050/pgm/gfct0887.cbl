      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT0887.
       AUTHOR.     VITOR VIGANO.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT0887                                    *
      *    PROGRAMADOR.:   VITOR VIGANO            - CPM PATO BRANCO   *
      *    ANALISTA CPM:   ADRIANO LOPES SANT'ANNA - CPM PATO BRANCO   *
      *    ANALISTA....:   VALERIA                 - PROCWORK /GP.50   *
      *    DATA........:   09/05/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   ENCERRAMENTO FLEXIBILIZACAO PARA AGRUPAMEN- *
      *      TO NA TABELA QUENTE.                                      *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                        INCLUDE/BOOK     *
      *                    DB2PRD.PARM_DATA_PROCM       GFCTB0A1       *
      *                    DB2PRD.PDIDO_FLEXZ_AGPTO     GFCTB0B2       *
      *                    DB2PRD.FLEXZ_AGPTO_CTA       GFCTB049       *
      *                    DB2PRD.FLEXZ_GRP_CLI         GFCTB050       *
      *                    DB2PRD.FLEXZ_GRP_DEPDC       GFCTB051       *
      *                    DB2PRD.FLEXZ_GRP_MUN         GFCTB053       *
      *                    DB2PRD.FLEXZ_GRP_PAB         GFCTB054       *
      *                    DB2PRD.FLEXZ_GRP_PSTAL       GFCTB055       *
      *                    DB2PRD.FLEXZ_GRP_SGMTO       GFCTB056       *
      *                    DB2PRD.FLEXZ_GRP_UF          GFCTB057       *
110510*                    DB2PRD.TFLEXZ_GRP_RZ         GFCTB0M8       *
      *                    DB2PRD.TTPO_SGMTO_GSTAO      GFCTB0H5       *
      *                    DB2PRD.V01UF                 DCITV002       *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#GFCTNT - AREA DE COMUNICACAO - ENTRADA.                   *
      *    I#GFCTNU - AREA DE COMUNICACAO - SAIDA.                     *
      *    I#GFCTFZ - AREA DE COMUNICACAO - ENTRADA GFCT5522.          *
      *    I#GFCTG1 - AREA DE COMUNICACAO - SAIDA GFCT5522.            *
      *    I#GFCTG2 - AREA DE COMUNICACAO - ENTRADA GFCT5523.          *
      *    I#GFCTG3 - AREA DE COMUNICACAO - SAIDA GFCT5523.            *
      *    I#GFCT1V - AREA DE COMUNICACAO - ENTRADA GFCT5006.          *
      *    I#GFCT1X - AREA DE COMUNICACAO - SAIDA GFCT5006.            *
      *    I#GFCTGB - AREA DE COMUNICACAO - ENTRADA GFCT5109, 5090.    *
      *    I#GFCTGC - AREA DE COMUNICACAO - SAIDA GFCT5109, 5090.      *
      *    I#RURC88 - AREA DE COMUNICACAO - RURC9020.                  *
      *    I#GFCT0M - AREA DE COMUNICACAO - ERROS.                     *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    DCIT8000 - VERIFICA UF.                                     *
      *    RURC9020 - VERIFICA MUNICIPIO.                              *
      *    GFCT5006 - DESCRICAO DEPENDENCIA.                           *
      *    GFCT5090 - DESCRICAO POSTO SERVICO.                         *
      *    GFCT5109 - OBTER DESCRICAO DE PAB.                          *
      *    GFCT5522 - VERIFICAR DISPONIBILIDADE DO ON-LINE.            *
      *    GFCT5523 - OBTER DESCRICAO DE MENSAGENS.                    *
      *    GFCT5505 - OBTER CODIGO DA JUNCAO.                          *
      *    POOL1470 - VERIFICA CONSISTENCIA DA DATA.                   *
      *    POOL0081 - ALOCAR DINAMICAMENTE UM MODULO.                  *
      *----------------------------------------------------------------*
      *    ULTIMA ALTERACAO EM 11/06/2008 - VALERIA                    *
      *    ALTERACOES REFERENTES AOS AGRUPAMENTOS PERSONALIZADOS       *
      *    - INCLUSAO DOS AGRUPAMENTOS PERSONALIZADOS                  *
      *----------------------------------------------------------------*
0410  *    ULTIMA ALTERACAO EM 01/04/2010 - HELIO SANTONI              *
0410  *    - ACRESCENTAR TRATAMENTO DOS AGRUPAMENTOS 20 E 26.          *
      *                                                                *
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

       77  WRK-NOME-TAB                PIC  X(008)         VALUE SPACES.
       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.

       77  WRK-DATA-INI                PIC  9(008)         VALUE ZEROS.
       77  WRK-DATA-FIM                PIC  9(008)         VALUE ZEROS.
       77  WRK-DATA-ATU                PIC  9(008)         VALUE ZEROS.
       77  WRK-DATA-DIGITADA           PIC  9(008)         VALUE ZEROS.
       77  WRK-DATA-1470               PIC  9(008)         VALUE ZEROS.

       01  WRK-DATA                    PIC  X(010)         VALUE SPACES.
       01  WRK-DATA-R                  REDEFINES WRK-DATA.
           05 WRK-DIA-R                PIC  9(002).
           05 FILLER                   PIC  X(001).
           05 WRK-MES-R                PIC  9(002).
           05 FILLER                   PIC  X(001).
           05 WRK-ANO-R                PIC  9(004).

HEXA   01  WRK-COD-SQL-ERRO            PIC S9(09)     VALUE ZEROS.
       01  FILLER                      REDEFINES      WRK-COD-SQL-ERRO.
           05  FILLER                  PIC  X(006).
           05  WRK-COD-SQL-ERRO-2      PIC  9(003).

       01  WRK-MSG05.
           05 FILLER                   PIC  X(026)         VALUE
               'ERRO NA CHAMADA DO MODULO '.
           05 WRK-NOME-MOD             PIC  X(008)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(051)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5006 - ENTRADA *'.
      *---------------------------------------------------------------*

       COPY 'I#GFCT1V'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5506 - SAIDA *'.
      *---------------------------------------------------------------*

       COPY 'I#GFCT1X'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(057)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5109 E GFCT5090 - ENTRADA *'.
      *---------------------------------------------------------------*

       COPY 'I#GFCTGB'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(057)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5109 E GFCT5090 - SAIDA *'.
      *---------------------------------------------------------------*

       COPY 'I#GFCTGC'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(051)         VALUE
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
           '* AREA DE COMUNICACAO COM RURC9020 *'.
      *---------------------------------------------------------------*

       COPY 'I#RURC88'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM MODULO DCIT8000 * '.
      *---------------------------------------------------------------*

       01  WRK-DCIT-UNIDADE-FEDERATIVA.
           05  WRK-AREA-DCLGEN-DCITV002     PIC X(070)     VALUE SPACES.
           05  WRK-DCIT-CODIGO-RETORNO      PIC 9(002)     VALUE ZEROS.

       01  WRK-POOL7100                     PIC X(107)     VALUE SPACES.
       01  WRK-SQLCA                        PIC X(136)     VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM MODULO GFCT5505 * '.
      *---------------------------------------------------------------*

       01  WRK-5505-AREA-ENTRADA.
           05 WRK-5505-AMBIENTE        PIC X(001)          VALUE SPACES.
           05 WRK-5505-FUNC-BDSCO      PIC X(007)          VALUE SPACES.

       01  WRK-5505-AREA-SAIDA.
           05 WRK-5505-COD-RETORNO     PIC 9(002)          VALUE ZEROS.
           05 WRK-5505-COD-SQL-ERRO    PIC 9(003)          VALUE ZEROS.
           05 WRK-5505-DESC-MSG        PIC X(070)          VALUE SPACES.
           05 WRK-5505-DADOS-RETORNO.
             10 WRK-5505-COD-JUNCAO    PIC 9(005)          VALUE ZEROS.

       01  WRK-IO-PCB.
           05 WRK-IO-LTERM                    PIC  X(008)  VALUE SPACES.
           05 FILLER                          PIC  X(002)  VALUE SPACES.
           05 WRK-IO-STATUS                   PIC  X(002)  VALUE SPACES.
           05 FILLER                          PIC  X(012)  VALUE SPACES.
           05 WRK-IO-MODNAME                  PIC  X(008)  VALUE SPACES.

       01  WRK-ALT-PCB.
           05 WRK-ALT-LTERM                   PIC  X(008)  VALUE SPACES.
           05 FILLER                          PIC  X(002)  VALUE SPACES.
           05 WRK-ALT-STATUS                  PIC  X(002)  VALUE SPACES.
           05 FILLER                          PIC  X(012)  VALUE SPACES.
           05 WRK-ALT-MODNAME                 PIC  X(008)  VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA TABELA DB2 *'.
      *---------------------------------------------------------------*

           EXEC SQL
             INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0A1
           END-EXEC.


           EXEC SQL
             INCLUDE GFCTB0B2
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB049
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB050
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB051
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB053
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB054
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB055
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB056
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB057
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0H5
           END-EXEC.

           EXEC SQL
             INCLUDE DCITV002
           END-EXEC.

110510     EXEC SQL
110510       INCLUDE GFCTB0M8
110510     END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* FIM DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY 'I#GFCTNT'.

       COPY 'I#GFCTNU'.

       COPY 'I#GFCT0M'.

      *================================================================*
       PROCEDURE                       DIVISION USING GFCTNT-ENTRADA
                                                      GFCTNU-SAIDA
                                                      GFCT0M-AREA-ERROS.
      *================================================================*

      ******************************************************************
      *    CHAMA AS ROTINAS PRINCIPAIS DO PROCESSAMENTO                *
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
                                          GFCTNU-SAIDA.

           INITIALIZE GFCTNU-SAIDA
                      GFCT0M-AREA-ERROS.

           MOVE 1010                   TO GFCTNU-LL.
           MOVE ZEROS                  TO GFCTNU-ZZ.

           PERFORM 1100-VERIFICAR-SIST-DISPONIVEL.
           PERFORM 1200-INICIALIZA-AREA-SAIDA.
           PERFORM 1300-CONSISTIR-DADOS.
           PERFORM 1400-OBTER-DATA-ATUAL.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICA SISTEMA  DISPONIVEL.                               *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-VERIFICAR-SIST-DISPONIVEL  SECTION.
      *----------------------------------------------------------------*

           MOVE 100                    TO GFCTFZ-LL.
           MOVE ZEROS                  TO GFCTFZ-ZZ.
           MOVE GFCTNT-TRANSACAO       TO GFCTFZ-TRANSACAO.
           MOVE GFCTNT-FUNCAO          TO GFCTFZ-FUNCAO.
           MOVE GFCTNT-FUNC-BDSCO      TO GFCTFZ-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTFZ-FIM.
           MOVE 'GFCT'                 TO GFCTFZ-SISTEMA.
           MOVE 'GFCT5522'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                             GFCTFZ-ENTRADA
                                             GFCTG1-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF (RETURN-CODE             NOT EQUAL ZEROS)
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-NOME-MOD
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 9                  TO GFCTNU-ERRO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0887'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF (GFCTG1-ERRO             NOT EQUAL ZEROS)
               MOVE GFCTG1-ERRO        TO GFCTNU-ERRO
               IF (GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2')
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
                   MOVE WRK-MODULO     TO GFCT0M-PROGRAMA
               END-IF
               MOVE 'GFCT0887'         TO GFCT0M-TRANSACAO
               MOVE GFCTG1-COD-SQL-ERRO
                                       TO GFCT0M-SQLCODE
                                          GFCTNU-COD-SQL-ERRO
               MOVE GFCTG1-COD-MSG-ERRO
                                       TO GFCTNU-COD-MSG-ERRO
               MOVE GFCTG1-DESC-MSG-ERRO
                                       TO GFCT0M-TEXTO
                                          GFCTNU-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF GFCTG1-CSIT-ONLINE       EQUAL 1
              MOVE 9                   TO GFCTNU-ERRO
              MOVE 'SISTEMA INDISPONIVEL' TO GFCT0M-TEXTO
              MOVE WRK-MODULO          TO GFCT0M-PROGRAMA
              MOVE 'APL'               TO GFCT0M-TIPO-ACESSO
              MOVE 'GFCT0887'          TO GFCT0M-TRANSACAO
              PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    MONTA AREA DE SAIDA                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       1200-INICIALIZA-AREA-SAIDA      SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTNT-TRANSACAO       TO GFCTNU-TRANSACAO.
           MOVE GFCTNT-FUNCAO          TO GFCTNU-FUNCAO.

           IF  GFCTNT-QTDE-OCOR        NOT NUMERIC
               MOVE ZEROS              TO GFCTNU-QTDE-OCOR
           ELSE
               MOVE GFCTNT-QTDE-OCOR   TO GFCTNU-QTDE-OCOR
           END-IF.

           IF  GFCTNT-QTDE-TOT-REG     NOT NUMERIC
               MOVE ZEROS              TO GFCTNU-QTDE-TOT-REG
           ELSE
               MOVE GFCTNT-QTDE-TOT-REG TO GFCTNU-QTDE-TOT-REG
           END-IF.

           MOVE GFCTNT-FUNC-BDSCO      TO GFCTNU-FUNC-BDSCO.
           MOVE 'S'                    TO GFCTNU-FIM.

           MOVE ZEROS                  TO GFCTNU-ERRO
                                          GFCTNU-COD-SQL-ERRO
                                          GFCTNU-COD-MSG-ERRO.
           MOVE '0000'                 TO GFCTG2-COD-MSG.

           PERFORM 1500-OBTER-DESC-MENSAGEM

           MOVE GFCTG3-DESC-MSG        TO GFCTNU-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    CONSISTE DADOS RECEBIDOS                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       1300-CONSISTIR-DADOS            SECTION.
      *----------------------------------------------------------------*

           IF (GFCTNT-TRANSACAO        EQUAL SPACES OR LOW-VALUES) OR
              (GFCTNT-FUNCAO           EQUAL SPACES OR LOW-VALUES) OR
              (GFCTNT-FUNC-BDSCO       EQUAL SPACES OR LOW-VALUES) OR
              (GFCTNT-DATA-INIC        EQUAL SPACES OR LOW-VALUES) OR
              (GFCTNT-DATA-FIM         EQUAL SPACES OR LOW-VALUES) OR
              (GFCTNT-AGRUPMTO         NOT NUMERIC) OR
110608*       (GFCTNT-AGRUPMTO         NOT EQUAL 04 AND 09 AND 14  AND
110608*                                          15 AND 17 AND 18  AND
110608*                                          19) OR
              (GFCTNT-TARIFA           NOT NUMERIC) OR
              (GFCTNT-TARIFA           EQUAL ZEROS) OR
ST2506*       (GFCTNT-CGC-CPF          NOT NUMERIC) OR
ST2506*       (GFCTNT-FILIAL           NOT NUMERIC) OR
ST2506        (GFCTNT-CGC-CPF          EQUAL LOW-VALUES) OR
ST2506        (GFCTNT-FILIAL           EQUAL LOW-VALUES) OR
              (GFCTNT-AGENCIA          NOT NUMERIC) OR
              (GFCTNT-POSTO            NOT NUMERIC) OR
              (GFCTNT-SEGMENTO         NOT NUMERIC) OR
              (GFCTNT-MUNICIPIO        NOT NUMERIC) OR
0410          (GFCTNT-GRUPO-CTBIL      NOT NUMERIC) OR
0410          (GFCTNT-SUB-GRUPO-CTBIL  NOT NUMERIC)
               MOVE 1                  TO GFCTNU-ERRO
                                          GFCTNU-COD-MSG-ERRO
               MOVE ZEROS              TO GFCTNU-COD-SQL-ERRO
               MOVE '0001'             TO GFCTG2-COD-MSG
               PERFORM 1500-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTNU-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           EVALUATE TRUE

0410           WHEN GFCTNT-AGRUPMTO    EQUAL 15 OR 17 OR 20
                   IF (GFCTNT-AGENCIA  EQUAL ZEROS OR
                       GFCTNT-POSTO    EQUAL ZEROS)
                       MOVE 1          TO GFCTNU-ERRO
                                          GFCTNU-COD-MSG-ERRO
                       MOVE ZEROS      TO GFCTNU-COD-SQL-ERRO
                       MOVE '0001'     TO GFCTG2-COD-MSG
                       PERFORM 1500-OBTER-DESC-MENSAGEM
                       MOVE GFCTG3-DESC-MSG
                                       TO GFCTNU-DESC-MSG-ERRO
                       PERFORM 3000-FINALIZAR
                   END-IF
               WHEN GFCTNT-AGRUPMTO    EQUAL 4
                   IF (GFCTNT-CONTROLE EQUAL SPACES OR LOW-VALUES) OR
ST2506*               (GFCTNT-CGC-CPF  EQUAL ZEROS)
ST2506                (GFCTNT-CGC-CPF  EQUAL SPACES OR LOW-VALUES)
                       MOVE 1          TO GFCTNU-ERRO
                                          GFCTNU-COD-MSG-ERRO
                       MOVE ZEROS      TO GFCTNU-COD-SQL-ERRO
                       MOVE '0001'     TO GFCTG2-COD-MSG
                       PERFORM 1500-OBTER-DESC-MENSAGEM
                       MOVE GFCTG3-DESC-MSG
                                       TO GFCTNU-DESC-MSG-ERRO
                       PERFORM 3000-FINALIZAR
                   END-IF
               WHEN GFCTNT-AGRUPMTO    EQUAL 9
                   IF (GFCTNT-SEGMENTO EQUAL ZEROS)
                       MOVE 1          TO GFCTNU-ERRO
                                          GFCTNU-COD-MSG-ERRO
                       MOVE ZEROS      TO GFCTNU-COD-SQL-ERRO
                       MOVE '0001'     TO GFCTG2-COD-MSG
                       PERFORM 1500-OBTER-DESC-MENSAGEM
                       MOVE GFCTG3-DESC-MSG
                                       TO GFCTNU-DESC-MSG-ERRO
                       PERFORM 3000-FINALIZAR
                   END-IF
               WHEN GFCTNT-AGRUPMTO    EQUAL 18
                   IF (GFCTNT-MUNICIPIO
                                       EQUAL ZEROS)
                       MOVE 1          TO GFCTNU-ERRO
                                          GFCTNU-COD-MSG-ERRO
                       MOVE ZEROS      TO GFCTNU-COD-SQL-ERRO
                       MOVE '0001'     TO GFCTG2-COD-MSG
                       PERFORM 1500-OBTER-DESC-MENSAGEM
                       MOVE GFCTG3-DESC-MSG
                                       TO GFCTNU-DESC-MSG-ERRO
                       PERFORM 3000-FINALIZAR
                   END-IF
               WHEN GFCTNT-AGRUPMTO    EQUAL 19
                   IF (GFCTNT-UF       EQUAL SPACES OR LOW-VALUES)
                       MOVE 1          TO GFCTNU-ERRO
                                          GFCTNU-COD-MSG-ERRO
                       MOVE ZEROS      TO GFCTNU-COD-SQL-ERRO
                       MOVE '0001'     TO GFCTG2-COD-MSG
                       PERFORM 1500-OBTER-DESC-MENSAGEM
                       MOVE GFCTG3-DESC-MSG
                                       TO GFCTNU-DESC-MSG-ERRO
                       PERFORM 3000-FINALIZAR
                   END-IF
0410           WHEN GFCTNT-AGRUPMTO    EQUAL 26
0410               IF (GFCTNT-GRUPO-CTBIL
0410                                   EQUAL ZEROS)
0410                   MOVE 1          TO GFCTNU-ERRO
0410                                      GFCTNU-COD-MSG-ERRO
0410                   MOVE ZEROS      TO GFCTNU-COD-SQL-ERRO
0410                   MOVE '0001'     TO GFCTG2-COD-MSG
0410                   PERFORM 1500-OBTER-DESC-MENSAGEM
0410                   MOVE GFCTG3-DESC-MSG
0410                                   TO GFCTNU-DESC-MSG-ERRO
0410                   PERFORM 3000-FINALIZAR
0410               END-IF

           END-EVALUATE.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    OBTEM A DATA ATUAL DA TABELA GFCTB0A1.                      *
      ******************************************************************
      *----------------------------------------------------------------*
       1400-OBTER-DATA-ATUAL           SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC
                                       OF GFCTB0A1.
           EXEC SQL
             SELECT
                   DPROCM_ANTER,
                   DPROCM_ATUAL,
                   DPROCM_PROX
             INTO
                   :GFCTB0A1.DPROCM-ANTER,
                   :GFCTB0A1.DPROCM-ATUAL,
                   :GFCTB0A1.DPROCM-PROX
             FROM   DB2PRD.PARM_DATA_PROCM
             WHERE
                   CSIST_PRINC        = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'GFCT0887'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'PARM_DATA_PROCM ' TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
                                          GFCTNU-COD-SQL-ERRO
               MOVE  '0010'            TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 9                  TO GFCTNU-ERRO
               MOVE 0010               TO GFCTNU-COD-MSG-ERRO
               MOVE '0010'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0A1'         TO WRK-NOME-TAB
               PERFORM 1500-OBTER-DESC-MENSAGEM
               STRING GFCTG3-DESC-MSG ' - ' WRK-NOME-TAB
               DELIMITED BY '  '       INTO GFCTNU-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    OBTER DESCRICAO DA MENSAGEM                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       1500-OBTER-DESC-MENSAGEM        SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTG3-SAIDA.

           INITIALIZE GFCTG3-SAIDA.

           MOVE 100                    TO GFCTG2-LL.
           MOVE ZEROS                  TO GFCTG2-ZZ.
           MOVE GFCTNT-TRANSACAO       TO GFCTG2-TRANSACAO.
           MOVE GFCTNT-FUNCAO          TO GFCTG2-FUNCAO.
           MOVE GFCTNT-FUNC-BDSCO      TO GFCTG2-FUNC-BDSCO.
           MOVE 'O'                    TO GFCTG2-TIPO-PROC.
           MOVE 'N'                    TO GFCTG2-FIM.

           MOVE 'GFCT5523'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                             GFCTG2-ENTRADA
                                             GFCTG3-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF (RETURN-CODE             NOT EQUAL ZEROS)
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-NOME-MOD
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 9                  TO GFCTNU-ERRO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0887'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF (GFCTG3-ERRO             NOT EQUAL ZEROS)
               MOVE GFCTG3-ERRO        TO GFCTNU-ERRO
               IF (GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2')
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
                   MOVE WRK-MODULO     TO GFCT0M-PROGRAMA
               END-IF
               MOVE 'GFCT0887'         TO GFCT0M-TRANSACAO
               MOVE GFCTG3-COD-MSG-ERRO
                                       TO GFCTNU-COD-MSG-ERRO
               MOVE GFCTG3-COD-SQL-ERRO
                                       TO GFCT0M-SQLCODE
                                          GFCTNU-COD-SQL-ERRO
               MOVE GFCTG3-DESC-MSG-ERRO
                                       TO GFCT0M-TEXTO
                                          GFCTNU-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCESSAMENTO PRINCIPAL                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*


           PERFORM 2050-ACESSAR-GFCT5505.

           PERFORM 2100-FORMATAR-DATA.
           PERFORM 2200-FAZER-UPDATE-ENC.

           PERFORM 2300-FAZER-UPDATE-PEDIDO.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA ACESSAR GFCT5505                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2050-ACESSAR-GFCT5505           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  WRK-5505-AREA-SAIDA.
           MOVE 'O'                    TO WRK-5505-AMBIENTE.
           MOVE GFCTNT-FUNC-BDSCO      TO WRK-5505-FUNC-BDSCO.

           MOVE 'GFCT5505'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                             WRK-5505-AREA-ENTRADA
                                             WRK-5505-AREA-SAIDA
                                             WRK-IO-PCB
                                             WRK-ALT-PCB.

           IF  WRK-5505-COD-RETORNO    NOT EQUAL ZEROS
               MOVE ZEROS              TO GFCTNU-COD-SQL-ERRO
               MOVE 1                  TO GFCTNU-ERRO
               MOVE 0112               TO GFCTNU-COD-MSG-ERRO
               MOVE WRK-5505-DESC-MSG  TO GFCTNU-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2050-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      ******************************************************************
      *    FORMATA A DATA.                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-FORMATAR-DATA              SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTNT-DATA-FIM        TO WRK-DATA.
           MOVE WRK-DIA-R              TO WRK-DATA-1470(1:2)
                                          WRK-DATA-DIGITADA(7:2).
           MOVE WRK-MES-R              TO WRK-DATA-1470(3:2)
                                          WRK-DATA-DIGITADA(5:2).
           MOVE WRK-ANO-R              TO WRK-DATA-1470(5:4)
                                          WRK-DATA-DIGITADA(1:4).

           CALL 'POOL1470'             USING WRK-DATA-1470.


           IF (RETURN-CODE             NOT EQUAL ZEROS AND 04)
               MOVE 'ERRO DE ACESSO A POOL1470'
                                       TO GFCT0M-TEXTO
                                          GFCTNU-DESC-MSG-ERRO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0887'         TO GFCT0M-PROGRAMA
               MOVE 9                  TO GFCTNU-ERRO
               PERFORM 3000-FINALIZAR
           ELSE
               IF (RETURN-CODE         EQUAL 04)
                   MOVE 1              TO GFCTNU-ERRO
                   MOVE ZEROS          TO GFCTNU-COD-SQL-ERRO
                   MOVE 0689           TO GFCTNU-COD-MSG-ERRO
                   MOVE '0689'         TO GFCTG2-COD-MSG
                   PERFORM 1500-OBTER-DESC-MENSAGEM
                   MOVE GFCTG3-DESC-MSG
                                       TO GFCTNU-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               END-IF
           END-IF.

           PERFORM 2110-LER-TABELA-GFCTB049.

           MOVE DINIC-FLEXZ-AGPTO      OF GFCTB049
                                       TO WRK-DATA.
           MOVE WRK-ANO-R              TO WRK-DATA-INI(1:4).
           MOVE WRK-MES-R              TO WRK-DATA-INI(5:2).
           MOVE WRK-DIA-R              TO WRK-DATA-INI(7:2).
           MOVE SPACES                 TO WRK-DATA.
           MOVE DFIM-FLEXZ-AGPTO       OF GFCTB049
                                       TO WRK-DATA.
           MOVE WRK-ANO-R              TO WRK-DATA-FIM(1:4).
           MOVE WRK-MES-R              TO WRK-DATA-FIM(5:2).
           MOVE WRK-DIA-R              TO WRK-DATA-FIM(7:2).
           MOVE SPACES                 TO WRK-DATA.
           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO WRK-DATA.
           MOVE WRK-ANO-R              TO WRK-DATA-ATU(1:4).
           MOVE WRK-MES-R              TO WRK-DATA-ATU(5:2).
           MOVE WRK-DIA-R              TO WRK-DATA-ATU(7:2).

           IF (WRK-DATA-DIGITADA       LESS WRK-DATA-ATU) OR
              (WRK-DATA-DIGITADA       LESS WRK-DATA-INI) OR
              (WRK-DATA-DIGITADA       GREATER WRK-DATA-FIM)
               MOVE '0666'             TO GFCTG2-COD-MSG
               MOVE 1                  TO GFCTNU-ERRO
               MOVE ZEROS              TO GFCTNU-COD-SQL-ERRO
               PERFORM 1500-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTNU-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    EFETUA A LEITURA DA TABELA GFCTB049.                        *
      ******************************************************************
      *----------------------------------------------------------------*
       2110-LER-TABELA-GFCTB049        SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTNT-AGRUPMTO        TO CAGPTO-CTA
                                       OF GFCTB049.
           MOVE GFCTNT-TIMESTAMP       TO HIDTFD-FLEXZ-AGPTO
                                       OF GFCTB049.
           MOVE GFCTNT-TARIFA          TO CSERVC-TARIF
                                       OF GFCTB049.

           EXEC SQL
             SELECT
                   CSERVC_TARIF,
                   CAGPTO_CTA,
                   DINIC_FLEXZ_AGPTO,
                   DFIM_FLEXZ_AGPTO,
                   QFRANQ_FLEXZ_AGPTO,
                   CPER_FRANQ_AGPTO,
                   VNEGOC_FLEXZ_AGPTO,
                   PDESC_FLEXZ_AGPTO,
                   QDIA_FLEXZ_AGPTO,
                   CINDCD_AGPTO_TOT
             INTO
                   :GFCTB049.CSERVC-TARIF,
                   :GFCTB049.CAGPTO-CTA,
                   :GFCTB049.DINIC-FLEXZ-AGPTO,
                   :GFCTB049.DFIM-FLEXZ-AGPTO,
                   :GFCTB049.QFRANQ-FLEXZ-AGPTO,
                   :GFCTB049.CPER-FRANQ-AGPTO,
                   :GFCTB049.VNEGOC-FLEXZ-AGPTO,
                   :GFCTB049.PDESC-FLEXZ-AGPTO,
                   :GFCTB049.QDIA-FLEXZ-AGPTO,
                   :GFCTB049.CINDCD-AGPTO-TOT
             FROM   DB2PRD.FLEXZ_AGPTO_CTA
             WHERE
                   CSERVC_TARIF       = :GFCTB049.CSERVC-TARIF       AND
                   CAGPTO_CTA         = :GFCTB049.CAGPTO-CTA         AND
                   HIDTFD_FLEXZ_AGPTO = :GFCTB049.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'GFCT0887'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'FLEXZ_AGPTO_CTA ' TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
                                          GFCTNU-COD-SQL-ERRO
               MOVE  '0020'            TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 9                  TO GFCTNU-ERRO
               MOVE 0010               TO GFCTNU-COD-MSG-ERRO
               MOVE '0010'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB049'         TO WRK-NOME-TAB
               PERFORM 1500-OBTER-DESC-MENSAGEM
               STRING GFCTG3-DESC-MSG ' - ' WRK-NOME-TAB
               DELIMITED BY '  '       INTO GFCTNU-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE GFCTNT-TARIFA          TO GFCTNU-TARIFA.
           MOVE QFRANQ-FLEXZ-AGPTO     OF GFCTB049
                                       TO GFCTNU-FRANQUIA.
           MOVE CPER-FRANQ-AGPTO       OF GFCTB049
                                       TO GFCTNU-PERIODO.
           MOVE VNEGOC-FLEXZ-AGPTO     OF GFCTB049
                                       TO GFCTNU-VR-NEGOCIADO.
           MOVE CPER-FRANQ-AGPTO       OF GFCTB049
                                       TO GFCTNU-PERC-DESCTO.
           MOVE QDIA-FLEXZ-AGPTO       OF GFCTB049
                                       TO GFCTNU-DIAS-RETENCAO.
           MOVE GFCTNT-DATA-INIC       TO GFCTNU-INIC-VIG-FLEX.
           MOVE GFCTNT-DATA-FIM        TO GFCTNU-FIM-VIG-FLEX.

           EVALUATE GFCTNT-AGRUPMTO

               WHEN 15
                   PERFORM 2111-SELECIONAR-PAB
               WHEN 14
                   PERFORM 2113-SELECIONAR-AGENCIA
               WHEN 17
                   PERFORM 2115-SELECIONAR-POSTAL
               WHEN 4
                   PERFORM 2116-SELECIONAR-CLIENTE
               WHEN 9
                   PERFORM 2117-SELECIONAR-SEGMENTO
               WHEN 18
                   PERFORM 2119-SELECIONAR-MUNICIPIO
               WHEN 19
                   PERFORM 2120-SELECIONAR-UF
0410           WHEN 20
0410               PERFORM 2115-SELECIONAR-POSTAL
0410           WHEN 26
0410               PERFORM 2121-SELECIONAR-RAZAO

           END-EVALUATE.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    SELECIONA PAB.                                              *
      ******************************************************************
      *----------------------------------------------------------------*
       2111-SELECIONAR-PAB             SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB049
                                       TO CSERVC-TARIF
                                       OF GFCTB054.
           MOVE HIDTFD-FLEXZ-AGPTO     OF GFCTB049
                                       TO HIDTFD-FLEXZ-AGPTO
                                       OF GFCTB054.
           MOVE CAGPTO-CTA             OF GFCTB049
                                       TO CAGPTO-CTA
                                       OF GFCTB054.

           EXEC SQL
             SELECT
                   CEMPR_INC,
                   CDEPDC,
                   CPOSTO_SERVC
             INTO
                   :GFCTB054.CEMPR-INC,
                   :GFCTB054.CDEPDC,
                   :GFCTB054.CPOSTO-SERVC
             FROM   DB2PRD.FLEXZ_GRP_PAB
             WHERE
                   CSERVC_TARIF       = :GFCTB054.CSERVC-TARIF       AND
                   CAGPTO_CTA         = :GFCTB054.CAGPTO-CTA         AND
                   HIDTFD_FLEXZ_AGPTO = :GFCTB054.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'GFCT0887'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'FLEXZ_GRP_PAB'    TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
                                          GFCTNU-COD-SQL-ERRO
               MOVE  '0030'            TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 9                  TO GFCTNU-ERRO
               MOVE 0010               TO GFCTNU-COD-MSG-ERRO
               MOVE '0010'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB054'         TO WRK-NOME-TAB
               PERFORM 1500-OBTER-DESC-MENSAGEM
               STRING GFCTG3-DESC-MSG ' - ' WRK-NOME-TAB
               DELIMITED BY '  '       INTO GFCTNU-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF (SQLCODE                 EQUAL +100)
               MOVE 1                  TO GFCTNU-ERRO
               MOVE ZEROS              TO GFCTNU-COD-SQL-ERRO
               MOVE 0926               TO GFCTNU-COD-MSG-ERRO
               MOVE '0926'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB054'         TO WRK-NOME-TAB
               PERFORM 1500-OBTER-DESC-MENSAGEM
               STRING GFCTG3-DESC-MSG ' - ' WRK-NOME-TAB
               DELIMITED BY '  '            INTO GFCTNU-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE CDEPDC                 OF GFCTB054
                                       TO GFCTNU-AGENCIA.
           MOVE CPOSTO-SERVC           OF GFCTB054
                                       TO GFCTNU-POSTO.

           PERFORM 2112-OBTER-DESC-PAB.

           MOVE GFCTGC-DESC-POSTO(1)   TO GFCTNU-DESCR-AGPTO.

      *----------------------------------------------------------------*
       2111-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    OBTEM A DESCRICAO DO PAB.                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2112-OBTER-DESC-PAB             SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTGB-ENTRADA.

           INITIALIZE GFCTGB-ENTRADA.

           MOVE 100                    TO GFCTGB-LL.
           MOVE ZEROS                  TO GFCTGB-ZZ.
           MOVE GFCTNT-TRANSACAO       TO GFCTGB-TRANSACAO.
           MOVE GFCTNT-FUNCAO          TO GFCTGB-FUNCAO.
           MOVE ZEROS                  TO GFCTGB-QTDE-OCOR
                                          GFCTGB-QTDE-TOT-REG.
           MOVE GFCTNT-FUNC-BDSCO      TO GFCTGB-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTGB-FIM.
           MOVE CDEPDC                 OF GFCTB054
                                       TO GFCTGB-COD-DEPDC.
           MOVE CPOSTO-SERVC           OF GFCTB054
                                       TO GFCTGB-COD-POSTO.
           MOVE SPACES                 TO GFCTGC-SAIDA
                                          GFCT0M-AREA-ERROS.
           INITIALIZE GFCTGC-SAIDA
                      GFCT0M-AREA-ERROS.

           MOVE 'GFCT5109'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                             GFCTGB-ENTRADA
                                             GFCTGC-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF (RETURN-CODE             NOT EQUAL ZEROS)
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-NOME-MOD
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 9                  TO GFCTNU-ERRO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0887'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF (GFCTGC-ERRO             NOT EQUAL ZEROS)
               MOVE GFCTGC-ERRO        TO GFCTNU-ERRO
               MOVE GFCTGC-COD-SQL-ERRO
                                       TO GFCTNU-COD-SQL-ERRO
               MOVE GFCTGC-COD-MSG-ERRO
                                       TO GFCTNU-COD-MSG-ERRO
               IF (GFCTGC-ERRO         EQUAL 1)
                   MOVE GFCTGC-DESC-MSG-ERRO
                                       TO GFCTNU-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5109 - '
                                       GFCTGC-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO GFCTNU-DESC-MSG-ERRO
               END-IF
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2112-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    SELECIONA A AGENCIA.                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       2113-SELECIONAR-AGENCIA         SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB049
                                       TO CSERVC-TARIF
                                       OF GFCTB051.
           MOVE HIDTFD-FLEXZ-AGPTO     OF GFCTB049
                                       TO HIDTFD-FLEXZ-AGPTO
                                       OF GFCTB051.
           MOVE CAGPTO-CTA             OF GFCTB049
                                       TO CAGPTO-CTA
                                       OF GFCTB051.

           EXEC SQL
             SELECT
                   CEMPR_INC,
                   CDEPDC
             INTO
                   :GFCTB051.CEMPR-INC,
                   :GFCTB051.CDEPDC
             FROM   DB2PRD.FLEXZ_GRP_DEPDC
             WHERE
                   CSERVC_TARIF       = :GFCTB051.CSERVC-TARIF       AND
                   CAGPTO_CTA         = :GFCTB051.CAGPTO-CTA         AND
                   HIDTFD_FLEXZ_AGPTO = :GFCTB051.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'GFCT0887'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'FLEXZ_GRP_DEPDC'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
                                          GFCTNU-COD-SQL-ERRO
               MOVE  '0040'            TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 9                  TO GFCTNU-ERRO
               MOVE 0010               TO GFCTNU-COD-MSG-ERRO
               MOVE '0010'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB051'         TO WRK-NOME-TAB
               PERFORM 1500-OBTER-DESC-MENSAGEM
               STRING GFCTG3-DESC-MSG ' - ' WRK-NOME-TAB
               DELIMITED BY '  '       INTO GFCTNU-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF (SQLCODE                 EQUAL +100)
               MOVE 1                  TO GFCTNU-ERRO
               MOVE ZEROS              TO GFCTNU-COD-SQL-ERRO
               MOVE 0926               TO GFCTNU-COD-MSG-ERRO
               MOVE '0926'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB054'         TO WRK-NOME-TAB
               PERFORM 1500-OBTER-DESC-MENSAGEM
               STRING GFCTG3-DESC-MSG ' - ' WRK-NOME-TAB
               DELIMITED BY '  '       INTO GFCTNU-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE CDEPDC                 OF GFCTB051
                                       TO GFCT1V-COD-DEPDC(1)
                                          GFCTNU-AGENCIA.


           IF (CDEPDC                  OF GFCTB051
                                       EQUAL ZEROS)
               MOVE 'TODAS'            TO GFCTNU-DESCR-AGPTO
           ELSE
               PERFORM 2114-OBTER-DESC-AGENCIA
               MOVE GFCT1X-DESC-DEPDC(1)
                                       TO GFCTNU-DESCR-AGPTO
           END-IF.

      *----------------------------------------------------------------*
       2113-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    OBTEM A DESCRICAO DA AGENCIA.                               *
      ******************************************************************
      *----------------------------------------------------------------*
       2114-OBTER-DESC-AGENCIA         SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCT1V-ENTRADA.

           INITIALIZE GFCT1V-ENTRADA.

           MOVE CDEPDC                 OF GFCTB051
                                       TO GFCT1V-COD-DEPDC(1).
           MOVE 660                    TO GFCT1V-LL.
           MOVE ZEROS                  TO GFCT1V-ZZ.
           MOVE GFCTNT-TRANSACAO       TO GFCT1V-TRANSACAO.
           MOVE GFCTNT-FUNCAO          TO GFCT1V-FUNCAO.
           MOVE 001                    TO GFCT1V-QTDE-OCOR.
           MOVE ZEROS                  TO GFCT1V-QTDE-TOT-REG.
           MOVE GFCTNT-FUNC-BDSCO      TO GFCT1V-FUNC-BDSCO.
           MOVE 'N'                    TO GFCT1V-FIM.
           MOVE CDEPDC                 OF GFCTB051
                                       TO GFCT1V-COD-DEPDC(1).
           MOVE 'GFCT5006'             TO WRK-MODULO.
           MOVE SPACES                 TO GFCT1X-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE GFCT1X-SAIDA
                      GFCT0M-AREA-ERROS.

           CALL WRK-MODULO USING
                                             GFCT1V-ENTRADA
                                             GFCT1X-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF (RETURN-CODE             NOT EQUAL ZEROS)
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-NOME-MOD
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 9                  TO GFCTNU-ERRO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0887'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF (GFCT1X-ERRO             NOT EQUAL ZEROS)
               MOVE GFCT1X-ERRO        TO GFCTNU-ERRO
               MOVE GFCT1X-COD-SQL-ERRO
                                       TO GFCTNU-COD-SQL-ERRO
               MOVE GFCT1X-COD-MSG-ERRO
                                       TO GFCTNU-COD-MSG-ERRO
               IF (GFCT1X-ERRO         EQUAL 1)
                   MOVE GFCT1X-DESC-MSG-ERRO
                                       TO GFCTNU-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5006 - ' GFCTGC-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO GFCTNU-DESC-MSG-ERRO
               END-IF
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2114-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    SELECIONA O POSTAL.                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2115-SELECIONAR-POSTAL          SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB049
                                       TO CSERVC-TARIF
                                       OF GFCTB055.
           MOVE HIDTFD-FLEXZ-AGPTO     OF GFCTB049
                                       TO HIDTFD-FLEXZ-AGPTO
                                       OF GFCTB055.
           MOVE CAGPTO-CTA             OF GFCTB049
                                       TO CAGPTO-CTA
                                       OF GFCTB055.

           EXEC SQL
             SELECT
                   CEMPR_INC,
                   CDEPDC,
                   CPOSTO_SERVC
             INTO
                   :GFCTB055.CEMPR-INC,
                   :GFCTB055.CDEPDC,
                   :GFCTB055.CPOSTO-SERVC
             FROM   DB2PRD.FLEXZ_GRP_PSTAL
             WHERE
                   CSERVC_TARIF       = :GFCTB055.CSERVC-TARIF       AND
                   CAGPTO_CTA         = :GFCTB055.CAGPTO-CTA         AND
                   HIDTFD_FLEXZ_AGPTO = :GFCTB055.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'GFCT0887'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'FLEXZ_GRP_PSTAL'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
                                          GFCTNU-COD-SQL-ERRO
               MOVE  '0050'            TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 9                  TO GFCTNU-ERRO
               MOVE 0010               TO GFCTNU-COD-MSG-ERRO
               MOVE '0010'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB055'         TO WRK-NOME-TAB
               PERFORM 1500-OBTER-DESC-MENSAGEM
               STRING GFCTG3-DESC-MSG ' - ' WRK-NOME-TAB
               DELIMITED BY '  '       INTO GFCTNU-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF (SQLCODE                 EQUAL +100)
               MOVE 1                  TO GFCTNU-ERRO
               MOVE ZEROS              TO GFCTNU-COD-SQL-ERRO
               MOVE 0926               TO GFCTNU-COD-MSG-ERRO
               MOVE '0926'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB055'         TO WRK-NOME-TAB
               PERFORM 1500-OBTER-DESC-MENSAGEM
               STRING GFCTG3-DESC-MSG ' - ' WRK-NOME-TAB
               DELIMITED BY '  '       INTO GFCTNU-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE SPACES                 TO GFCTGB-ENTRADA.

           INITIALIZE GFCTGB-ENTRADA.

           MOVE 100                    TO GFCTGB-LL.
           MOVE ZEROS                  TO GFCTGB-ZZ.
           MOVE GFCTNT-TRANSACAO       TO GFCTGB-TRANSACAO.
           MOVE GFCTNT-FUNCAO          TO GFCTGB-FUNCAO.
           MOVE ZEROS                  TO GFCTGB-QTDE-OCOR.
           MOVE ZEROS                  TO GFCTGB-TRANSACAO
                                          GFCTGB-QTDE-TOT-REG.
           MOVE GFCTNT-FUNC-BDSCO      TO GFCTGB-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTGB-FIM.
           MOVE CDEPDC                 OF GFCTB055
                                       TO GFCTGB-COD-DEPDC.
           MOVE CPOSTO-SERVC           OF GFCTB055
                                       TO GFCTGB-COD-POSTO.
           MOVE SPACES                 TO GFCTGC-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE GFCTGC-SAIDA
                      GFCT0M-AREA-ERROS.

           MOVE 'GFCT5090'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                             GFCTGB-ENTRADA
                                             GFCTGC-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF (RETURN-CODE             NOT EQUAL ZEROS)
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-NOME-MOD
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 9                  TO GFCTNU-ERRO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0887'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF (GFCTGC-ERRO             NOT EQUAL ZEROS)
               MOVE GFCTGC-ERRO        TO GFCTNU-ERRO
               MOVE GFCTGC-COD-SQL-ERRO
                                       TO GFCTNU-COD-SQL-ERRO
               MOVE GFCTGC-COD-MSG-ERRO
                                       TO GFCTNU-COD-MSG-ERRO
               IF (GFCTGC-ERRO         EQUAL 1)
                   MOVE GFCTGC-DESC-MSG-ERRO
                                       TO GFCTNU-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5090 - ' GFCTGC-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO GFCTNU-DESC-MSG-ERRO
               END-IF
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE GFCTGC-DESC-POSTO(1)   TO GFCTNU-DESCR-AGPTO.
           MOVE CDEPDC                 OF GFCTB055
                                       TO GFCTNU-AGENCIA.
           MOVE CPOSTO-SERVC           OF GFCTB055
                                       TO GFCTNU-POSTO.

      *----------------------------------------------------------------*
       2115-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    SELECIONA O CLIENTE.                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       2116-SELECIONAR-CLIENTE         SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB049
                                       TO CSERVC-TARIF
                                       OF GFCTB050.
           MOVE HIDTFD-FLEXZ-AGPTO     OF GFCTB049
                                       TO HIDTFD-FLEXZ-AGPTO
                                       OF GFCTB050.
           MOVE CAGPTO-CTA             OF GFCTB049
                                       TO CAGPTO-CTA
                                       OF GFCTB050.

           EXEC SQL
             SELECT
ST2506*            CCGC_CPF,
ST2506*            CFLIAL_CGC,
ST2506*            CCTRL_CPF_CGC
ST2506             CCGC_CPF_ST,
ST2506             CFLIAL_CGC_ST,
ST2506             CCTRL_CPF_CGC_ST
             INTO
ST2506*            :GFCTB050.CCGC-CPF,
ST2506*            :GFCTB050.CFLIAL-CGC,
ST2506*            :GFCTB050.CCTRL-CPF-CGC
ST2506             :GFCTB050.CCGC-CPF-ST,
ST2506             :GFCTB050.CFLIAL-CGC-ST,
ST2506             :GFCTB050.CCTRL-CPF-CGC-ST
             FROM   DB2PRD.FLEXZ_GRP_CLI
             WHERE
                   CSERVC_TARIF       = :GFCTB050.CSERVC-TARIF       AND
                   CAGPTO_CTA         = :GFCTB050.CAGPTO-CTA         AND
                   HIDTFD_FLEXZ_AGPTO = :GFCTB050.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'GFCT0887'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'FLEXZ_GRP_CLI'    TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
                                          GFCTNU-COD-SQL-ERRO
               MOVE  '0060'            TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 9                  TO GFCTNU-ERRO
               MOVE 0010               TO GFCTNU-COD-MSG-ERRO
               MOVE '0010'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB050'         TO WRK-NOME-TAB
               PERFORM 1500-OBTER-DESC-MENSAGEM
               STRING GFCTG3-DESC-MSG ' - ' WRK-NOME-TAB
               DELIMITED BY '  '       INTO GFCTNU-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF (SQLCODE                 EQUAL +100)
               MOVE 1                  TO GFCTNU-ERRO
               MOVE ZEROS              TO GFCTNU-COD-SQL-ERRO
               MOVE 0926               TO GFCTNU-COD-MSG-ERRO
               MOVE '0926'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB050'         TO WRK-NOME-TAB
               PERFORM 1500-OBTER-DESC-MENSAGEM
               STRING GFCTG3-DESC-MSG ' - ' WRK-NOME-TAB
               DELIMITED BY '  '       INTO GFCTNU-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE SPACES                 TO GFCTNU-DESCR-AGPTO.
ST2508*    MOVE CCGC-CPF               OF GFCTB050
ST2508*                                TO GFCTNU-CGC-CPF.
ST2508*    MOVE CFLIAL-CGC             OF GFCTB050
ST2508*                                TO GFCTNU-FILIAL.
ST2508*    MOVE CCTRL-CPF-CGC          OF GFCTB050
ST2508     MOVE CCGC-CPF-ST            OF GFCTB050
ST2508                                 TO GFCTNU-CGC-CPF.
ST2508     MOVE CFLIAL-CGC-ST          OF GFCTB050
ST2508                                 TO GFCTNU-FILIAL.
ST2508     MOVE CCTRL-CPF-CGC-ST       OF GFCTB050
                                       TO GFCTNU-CONTROLE.

      *----------------------------------------------------------------*
       2116-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    SELECIONA O SEGMENTO.                                       *
      ******************************************************************
      *----------------------------------------------------------------*
       2117-SELECIONAR-SEGMENTO        SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB049
                                       TO CSERVC-TARIF
                                       OF GFCTB056.
           MOVE HIDTFD-FLEXZ-AGPTO     OF GFCTB049
                                       TO HIDTFD-FLEXZ-AGPTO
                                       OF GFCTB056.
           MOVE CAGPTO-CTA             OF GFCTB049
                                       TO CAGPTO-CTA
                                       OF GFCTB056.

           EXEC SQL
             SELECT
                   CSGMTO_GSTAO_TARIF
             INTO
                   :GFCTB056.CSGMTO-GSTAO-TARIF
             FROM   DB2PRD.FLEXZ_GRP_SGMTO
             WHERE
                   CSERVC_TARIF       = :GFCTB056.CSERVC-TARIF       AND
                   CAGPTO_CTA         = :GFCTB056.CAGPTO-CTA         AND
                   HIDTFD_FLEXZ_AGPTO = :GFCTB056.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'GFCT0887'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'FLEXZ_GRP_SGMTO'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
                                          GFCTNU-COD-SQL-ERRO
               MOVE  '0070'            TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 9                  TO GFCTNU-ERRO
               MOVE 0010               TO GFCTNU-COD-MSG-ERRO
               MOVE '0010'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB056'         TO WRK-NOME-TAB
               PERFORM 1500-OBTER-DESC-MENSAGEM
               STRING GFCTG3-DESC-MSG ' - ' WRK-NOME-TAB
               DELIMITED BY '  '       INTO GFCTNU-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF (SQLCODE                 EQUAL +100)
               MOVE 1                  TO GFCTNU-ERRO
               MOVE ZEROS              TO GFCTNU-COD-SQL-ERRO
               MOVE 0926               TO GFCTNU-COD-MSG-ERRO
               MOVE '0926'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB056'         TO WRK-NOME-TAB
               PERFORM 1500-OBTER-DESC-MENSAGEM
               STRING GFCTG3-DESC-MSG ' - ' WRK-NOME-TAB
               DELIMITED BY '  '       INTO GFCTNU-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           PERFORM 2118-OBTER-DESC-SEGMENTO.

           MOVE ISGMTO-GSTAO-TARIF     OF GFCTB0H5
                                       TO GFCTNU-DESCR-AGPTO.
           MOVE CSGMTO-GSTAO-TARIF     OF GFCTB056
                                       TO CSGMTO-GSTAO-TARIF
                                       OF GFCTB0H5
                                          GFCTNU-SEGMENTO.

      *----------------------------------------------------------------*
       2117-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    OBTEM A DESCRICAO DO SEGMENTO.                              *
      ******************************************************************
      *----------------------------------------------------------------*
       2118-OBTER-DESC-SEGMENTO        SECTION.
      *----------------------------------------------------------------*

           MOVE CSGMTO-GSTAO-TARIF     OF GFCTB056
                                       TO CSGMTO-GSTAO-TARIF
                                       OF GFCTB0H5.

           EXEC SQL
             SELECT
                   ISGMTO_GSTAO_TARIF
             INTO
                   :GFCTB0H5.ISGMTO-GSTAO-TARIF
             FROM   DB2PRD.TTPO_SGMTO_GSTAO
             WHERE
                   CSGMTO_GSTAO_TARIF = :GFCTB0H5.CSGMTO-GSTAO-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'GFCT0887'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'TTPO_SGMTO_GSTAO' TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
                                          GFCTNU-COD-SQL-ERRO
               MOVE  '0080'            TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 9                  TO GFCTNU-ERRO
               MOVE 0010               TO GFCTNU-COD-MSG-ERRO
               MOVE '0010'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0H5'         TO WRK-NOME-TAB
               PERFORM 1500-OBTER-DESC-MENSAGEM
               STRING GFCTG3-DESC-MSG ' - ' WRK-NOME-TAB
               DELIMITED BY '  '            INTO GFCTNU-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF (SQLCODE                 EQUAL +100)
               MOVE 1                  TO GFCTNU-ERRO
               MOVE ZEROS              TO GFCTNU-COD-SQL-ERRO
               MOVE 0500               TO GFCTNU-COD-MSG-ERRO
               MOVE '0500'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0H5'         TO WRK-NOME-TAB
               PERFORM 1500-OBTER-DESC-MENSAGEM
               STRING GFCTG3-DESC-MSG ' - ' WRK-NOME-TAB
               DELIMITED BY '  '       INTO GFCTNU-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2118-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    SELECIONA O MUNICIPIO.                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2119-SELECIONAR-MUNICIPIO       SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB049
                                       TO CSERVC-TARIF
                                       OF GFCTB053.
           MOVE HIDTFD-FLEXZ-AGPTO     OF GFCTB049
                                       TO HIDTFD-FLEXZ-AGPTO
                                       OF GFCTB053.
           MOVE CAGPTO-CTA             OF GFCTB049
                                       TO CAGPTO-CTA
                                       OF GFCTB053.

           EXEC SQL
             SELECT
                   CMUN_IBGE
             INTO
                   :GFCTB053.CMUN-IBGE
             FROM DB2PRD.FLEXZ_GRP_MUN
             WHERE
                   CSERVC_TARIF       = :GFCTB053.CSERVC-TARIF       AND
                   CAGPTO_CTA         = :GFCTB053.CAGPTO-CTA         AND
                   HIDTFD_FLEXZ_AGPTO = :GFCTB053.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'GFCT0887'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'FLEXZ_GRP_MUN'    TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
                                          GFCTNU-COD-SQL-ERRO
               MOVE  '0090'            TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 9                  TO GFCTNU-ERRO
               MOVE 0010               TO GFCTNU-COD-MSG-ERRO
               MOVE '0010'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB053'         TO WRK-NOME-TAB
               PERFORM 1500-OBTER-DESC-MENSAGEM
               STRING GFCTG3-DESC-MSG ' - ' WRK-NOME-TAB
               DELIMITED BY '  '       INTO GFCTNU-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF (SQLCODE                 EQUAL +100)
               MOVE 1                  TO GFCTNU-ERRO
               MOVE ZEROS              TO GFCTNU-COD-SQL-ERRO
               MOVE 0926               TO GFCTNU-COD-MSG-ERRO
               MOVE '0926'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB053'         TO WRK-NOME-TAB
               PERFORM 1500-OBTER-DESC-MENSAGEM
               STRING GFCTG3-DESC-MSG ' - ' WRK-NOME-TAB
               DELIMITED BY '  '       INTO GFCTNU-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE SPACES                 TO AREA-RURC88.

           INITIALIZE AREA-RURC88.

           MOVE CMUN-IBGE              OF GFCTB053
                                       TO  COD-MUNIC-RURC88.
           MOVE 'RURC9020'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                             AREA-RURC88.

           IF (RETURN-CODE             NOT EQUAL ZEROS)
               MOVE 1                  TO GFCTNU-ERRO
               MOVE ZEROS              TO GFCTNU-COD-SQL-ERRO
               MOVE 0732               TO GFCTNU-COD-MSG-ERRO
               MOVE '0732'             TO GFCTG2-COD-MSG
               PERFORM 1500-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTNU-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           EVALUATE COD-RETORNO-RURC88

               WHEN 10
                   MOVE 1              TO GFCTNU-ERRO
                   MOVE ZEROS          TO GFCTNU-COD-SQL-ERRO
                   MOVE 0675           TO GFCTNU-COD-MSG-ERRO
                   MOVE '0675'         TO GFCTG2-COD-MSG
                   PERFORM 1500-OBTER-DESC-MENSAGEM
                   MOVE GFCTG3-DESC-MSG
                                       TO GFCTNU-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR

               WHEN 20
                   MOVE 1              TO GFCTNU-ERRO
                   MOVE ZEROS          TO GFCTNU-COD-SQL-ERRO
                   MOVE 0733           TO GFCTNU-COD-MSG-ERRO
                   MOVE '0733'         TO GFCTG2-COD-MSG
                   PERFORM 1500-OBTER-DESC-MENSAGEM
                   MOVE GFCTG3-DESC-MSG
                                       TO GFCTNU-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR

               WHEN 99
                   MOVE 1              TO GFCTNU-ERRO
                   MOVE ZEROS          TO GFCTNU-COD-SQL-ERRO
                   MOVE 0734           TO GFCTNU-COD-MSG-ERRO
                   MOVE '0734'         TO GFCTG2-COD-MSG
                   PERFORM 1500-OBTER-DESC-MENSAGEM
                   MOVE GFCTG3-DESC-MSG
                                       TO GFCTNU-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR

           END-EVALUATE.

           MOVE NOME-MUNIC-RURC88      TO GFCTNU-DESCR-AGPTO.
           MOVE CMUN-IBGE              OF GFCTB053
                                       TO GFCTNU-MUNICIPIO.

      *----------------------------------------------------------------*
       2119-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    SELECIONAR UF.                                              *
      ******************************************************************
      *----------------------------------------------------------------*
       2120-SELECIONAR-UF              SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB049
                                       TO CSERVC-TARIF
                                       OF GFCTB057.
           MOVE HIDTFD-FLEXZ-AGPTO     OF GFCTB049
                                       TO HIDTFD-FLEXZ-AGPTO
                                       OF GFCTB057.
           MOVE CAGPTO-CTA             OF GFCTB049
                                       TO CAGPTO-CTA
                                       OF GFCTB057.

           EXEC SQL
             SELECT
                   CSGL_UF
             INTO
                   :GFCTB057.CSGL-UF
             FROM   DB2PRD.FLEXZ_GRP_UF
             WHERE
                   CSERVC_TARIF       = :GFCTB057.CSERVC-TARIF       AND
                   CAGPTO_CTA         = :GFCTB057.CAGPTO-CTA         AND
                   HIDTFD_FLEXZ_AGPTO = :GFCTB057.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'GFCT0887'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'FLEXZ_GRP_UF'     TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
                                          GFCTNU-COD-SQL-ERRO
               MOVE  '0100'            TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 9                  TO GFCTNU-ERRO
               MOVE 0010               TO GFCTNU-COD-MSG-ERRO
               MOVE '0010'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB057'         TO WRK-NOME-TAB
               PERFORM 1500-OBTER-DESC-MENSAGEM
               STRING GFCTG3-DESC-MSG ' - ' WRK-NOME-TAB
               DELIMITED BY '  '       INTO GFCTNU-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF (SQLCODE                 EQUAL +100)
               MOVE 1                  TO GFCTNU-ERRO
               MOVE ZEROS              TO GFCTNU-COD-SQL-ERRO
               MOVE 0926               TO GFCTNU-COD-MSG-ERRO
               MOVE '0926'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB057'         TO WRK-NOME-TAB
               PERFORM 1500-OBTER-DESC-MENSAGEM
               STRING GFCTG3-DESC-MSG ' - ' WRK-NOME-TAB
               DELIMITED BY '  '       INTO GFCTNU-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE CSGL-UF                OF GFCTB057
                                       TO CSGL-UF
                                       OF DCITV002.
           MOVE DCITV002               TO WRK-AREA-DCLGEN-DCITV002.
           MOVE 'DCIT8000'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                             WRK-DCIT-UNIDADE-FEDERATIVA
                                             WRK-POOL7100
                                             WRK-SQLCA.

           IF (RETURN-CODE             NOT EQUAL ZEROS)
               MOVE 1                  TO GFCTNU-ERRO
               MOVE ZEROS              TO GFCTNU-COD-SQL-ERRO
               MOVE 0735               TO GFCTNU-COD-MSG-ERRO
               MOVE '0735'             TO GFCTG2-COD-MSG
               PERFORM 1500-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTNU-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           EVALUATE WRK-DCIT-CODIGO-RETORNO

               WHEN 1
                   MOVE 1              TO GFCTNU-ERRO
                   MOVE ZEROS          TO GFCTNU-COD-SQL-ERRO
                   MOVE 0736           TO GFCTNU-COD-MSG-ERRO
                   MOVE '0736'         TO GFCTG2-COD-MSG
                   PERFORM 1500-OBTER-DESC-MENSAGEM
                   MOVE GFCTG3-DESC-MSG
                                       TO GFCTNU-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR

               WHEN 2
                   MOVE 1              TO GFCTNU-ERRO
                   MOVE ZEROS          TO GFCTNU-COD-SQL-ERRO
                   MOVE 0737           TO GFCTNU-COD-MSG-ERRO
                   MOVE '0737'         TO GFCTG2-COD-MSG
                   PERFORM 1500-OBTER-DESC-MENSAGEM
                   MOVE GFCTG3-DESC-MSG
                                       TO GFCTNU-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR

               WHEN 3
                   MOVE 1              TO GFCTNU-ERRO
                   MOVE ZEROS          TO GFCTNU-COD-SQL-ERRO
                   MOVE 0738           TO GFCTNU-COD-MSG-ERRO
                   MOVE '0738'         TO GFCTG2-COD-MSG
                   PERFORM 1500-OBTER-DESC-MENSAGEM
                   MOVE GFCTG3-DESC-MSG
                                       TO GFCTNU-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR

               WHEN 4
                   MOVE 'GFCT0887'     TO GFCT0M-PROGRAMA
                   MOVE 'DB2'          TO GFCT0M-TIPO-ACESSO
                   MOVE 'V01_UF'       TO GFCT0M-NOME-TAB
                   MOVE 'SELECT  '     TO GFCT0M-COMANDO-SQL
                   MOVE  SQLCODE       TO GFCT0M-SQLCODE
                                          GFCTNU-COD-SQL-ERRO
                   MOVE  '0110'        TO GFCT0M-LOCAL
                   MOVE SQLCA          TO GFCT0M-SQLCA-AREA
                   MOVE 9              TO GFCTNU-ERRO
                   MOVE 0005           TO GFCTNU-COD-MSG-ERRO
                   MOVE '0005'         TO GFCTG2-COD-MSG
                   MOVE 'DCITV002'     TO WRK-NOME-TAB
                   PERFORM 1500-OBTER-DESC-MENSAGEM
                   STRING GFCTG3-DESC-MSG ' - ' WRK-NOME-TAB
               DELIMITED BY '  '       INTO GFCTNU-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR

           END-EVALUATE.

           MOVE WRK-AREA-DCLGEN-DCITV002
                                       TO DCITV002.
           MOVE IUF                    OF DCITV002
                                       TO GFCTNU-DESCR-AGPTO.
           MOVE CSGL-UF                OF GFCTB057
                                       TO GFCTNU-UF.

      *----------------------------------------------------------------*
       2120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

0410  ******************************************************************
0410  *    SELECIONA O RAZAO                                           *
0410  ******************************************************************
0410  *----------------------------------------------------------------*
0410   2121-SELECIONAR-RAZAO           SECTION.
0410  *----------------------------------------------------------------*
0410
0410       MOVE CSERVC-TARIF           OF GFCTB049
0410                                   TO CSERVC-TARIF       OF GFCTB0M8
0410       MOVE HIDTFD-FLEXZ-AGPTO     OF GFCTB049
0410                                   TO HIDTFD-FLEXZ-AGPTO OF GFCTB0M8
0410       MOVE CAGPTO-CTA             OF GFCTB049
0410                                   TO CAGPTO-CTA         OF GFCTB0M8
0410
0410       EXEC SQL
0410         SELECT
0410               CGRP_CTBIL_ATUAL,
0410               CSGRP_CTBIL
0410         INTO
0410               :GFCTB0M8.CGRP-CTBIL-ATUAL,
0410               :GFCTB0M8.CSGRP-CTBIL
0410         FROM   DB2PRD.TFLEXZ_GRP_RZ
0410         WHERE
0410               CSERVC_TARIF       = :GFCTB0M8.CSERVC-TARIF       AND
0410               CAGPTO_CTA         = :GFCTB0M8.CAGPTO-CTA         AND
0410               HIDTFD_FLEXZ_AGPTO = :GFCTB0M8.HIDTFD-FLEXZ-AGPTO
0410       END-EXEC.
0410
0410       IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
0410          (SQLWARN0                EQUAL 'W')
0410           MOVE 'GFCT0887'         TO GFCT0M-PROGRAMA
0410           MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
0410           MOVE 'TFLEXZ_GRP_RZ   '  TO GFCT0M-NOME-TAB
0410           MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
0410           MOVE  SQLCODE           TO GFCT0M-SQLCODE
HEXA                                      WRK-COD-SQL-ERRO
HEXA           MOVE  WRK-COD-SQL-ERRO-2 TO GFCTNU-COD-SQL-ERRO
0410           MOVE  '0050'            TO GFCT0M-LOCAL
0410           MOVE SQLCA              TO GFCT0M-SQLCA-AREA
0410           MOVE 9                  TO GFCTNU-ERRO
0410           MOVE 0010               TO GFCTNU-COD-MSG-ERRO
0410           MOVE '0010'             TO GFCTG2-COD-MSG
0410           MOVE 'GFCTB0M8'         TO WRK-NOME-TAB
0410           PERFORM 1500-OBTER-DESC-MENSAGEM
0410           STRING GFCTG3-DESC-MSG ' - ' WRK-NOME-TAB
0410           DELIMITED BY '  '       INTO GFCTNU-DESC-MSG-ERRO
0410           PERFORM 3000-FINALIZAR
0410       END-IF.
0410
0410       IF (SQLCODE                 EQUAL +100)
0410           MOVE 1                  TO GFCTNU-ERRO
0410           MOVE ZEROS              TO GFCTNU-COD-SQL-ERRO
0410           MOVE 0926               TO GFCTNU-COD-MSG-ERRO
0410           MOVE '0926'             TO GFCTG2-COD-MSG
0410           MOVE 'GFCTB0M8'         TO WRK-NOME-TAB
0410           PERFORM 1500-OBTER-DESC-MENSAGEM
0410           STRING GFCTG3-DESC-MSG ' - ' WRK-NOME-TAB
0410           DELIMITED BY '  '       INTO GFCTNU-DESC-MSG-ERRO
0410           PERFORM 3000-FINALIZAR
0410       END-IF.
0410
0410       MOVE CGRP-CTBIL-ATUAL OF GFCTB0M8    TO GFCTNU-GRUPO-CTBIL
0410       MOVE CSGRP-CTBIL      OF GFCTB0M8  TO GFCTNU-SUB-GRUPO-CTBIL.
0410
0410  *----------------------------------------------------------------*
0410   2121-99-FIM.                    EXIT.
0410  *----------------------------------------------------------------*

      ******************************************************************
      *  EFETUA O UPDATE NA TABELA GFCTB049.                           *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-FAZER-UPDATE-ENC           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-5505-COD-JUNCAO    TO CDEPDC-ENCRR-VGCIA
                                                            OF GFCTB049.
           MOVE GFCTNT-FUNC-BDSCO      TO CFUNC-ENCRR-VGCIA
                                                            OF GFCTB049.
           MOVE GFCTNT-DATA-FIM        TO DFIM-FLEXZ-AGPTO
                                                            OF GFCTB049.

           EXEC SQL
             UPDATE   DB2PRD.FLEXZ_AGPTO_CTA
               SET
                   CDEPDC_ENCRR_VGCIA = :GFCTB049.CDEPDC-ENCRR-VGCIA,
                   CFUNC_ENCRR_VGCIA  = :GFCTB049.CFUNC-ENCRR-VGCIA,
                   HVGCIA_REGRA       = CURRENT TIMESTAMP,
                   DFIM_FLEXZ_AGPTO   = :GFCTB049.DFIM-FLEXZ-AGPTO
             WHERE
                   CSERVC_TARIF       = :GFCTB049.CSERVC-TARIF      AND
                   CAGPTO_CTA         = :GFCTB049.CAGPTO-CTA        AND
                   HIDTFD_FLEXZ_AGPTO = :GFCTB049.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'GFCT0887'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'FLEXZ_AGPTO_CTA'  TO GFCT0M-NOME-TAB
               MOVE 'UPDATE '          TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
                                          GFCTNU-COD-SQL-ERRO
               MOVE  '0120'            TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 9                  TO GFCTNU-ERRO
               MOVE 0010               TO GFCTNU-COD-MSG-ERRO
               MOVE '0010'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB049'         TO WRK-NOME-TAB
               PERFORM 1500-OBTER-DESC-MENSAGEM
               STRING GFCTG3-DESC-MSG ' - ' WRK-NOME-TAB
               DELIMITED BY '  '       INTO GFCTNU-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  EFETUA O UPDATE NA TABELA GFCTB0B2.                           *
      ******************************************************************
      *----------------------------------------------------------------*
       2300-FAZER-UPDATE-PEDIDO        SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTNT-AGRUPMTO        TO CAGPTO-CTA       OF GFCTB0B2.
           MOVE GFCTNT-TARIFA          TO CSERVC-TARIF     OF GFCTB0B2.
           MOVE GFCTNT-TIMESTAMP       TO HIDTFD-FLEXZ-AGPTO
                                                           OF GFCTB0B2.
           MOVE 8                      TO CSIT-SOLTC-FLEXZ OF GFCTB0B2.

           EXEC SQL
             UPDATE   DB2PRD.PDIDO_FLEXZ_AGPTO
               SET
                   CSIT_SOLTC_FLEXZ     = :GFCTB0B2.CSIT-SOLTC-FLEXZ
             WHERE
                   CAGPTO_CTA           = :GFCTB0B2.CAGPTO-CTA      AND
                   CSERVC_TARIF         = :GFCTB0B2.CSERVC-TARIF    AND
                   HIDTFD_FLEXZ_AGPTO   = :GFCTB0B2.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'GFCT0887'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_AGPTO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'UPDATE '          TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
                                          GFCTNU-COD-SQL-ERRO
               MOVE  '0130'            TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 9                  TO GFCTNU-ERRO
               MOVE 0010               TO GFCTNU-COD-MSG-ERRO
               MOVE '0010'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0B2'         TO WRK-NOME-TAB
               PERFORM 1500-OBTER-DESC-MENSAGEM
               STRING GFCTG3-DESC-MSG ' - ' WRK-NOME-TAB
               DELIMITED BY '  '       INTO GFCTNU-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA PARA FINALIZAR O PROGRAMA                            *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
