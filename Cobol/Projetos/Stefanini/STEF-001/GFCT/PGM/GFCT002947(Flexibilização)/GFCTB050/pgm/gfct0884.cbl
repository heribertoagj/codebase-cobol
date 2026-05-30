      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT0884.
       AUTHOR.     MARCELO FARIA.
      *================================================================*
      *                   C P M  -  S I S T E M A S.                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT0884                                    *
      *    PROGRAMADOR.:   MARCELO FARIA           - CPM PATO BRANCO   *
      *    ANALISTA CPM:   ELIAS BOSCATO           - CPM PATO BRANCO   *
      *    ANALISTA....:   VALERIA                 - PROCKWORK GP/75   *
      *    DATA........:   10/05/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   FLEXIBILIZACAO - CONSULTA DETALHADA DE FLE- *
      *        XIBILIZACAO PARA AGRUPAMENTO.                           *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                         INCLUDE/BOOK    *
      *                    DB2PRD.PARM_DATA_PROCM        GFCTB0A1      *
      *                    DB2PRD.PDIDO_FLEXZ_AGPTO      GFCTB0B2      *
      *                    DB2PRD.PDIDO_FLEXZ_CLI        GFCTB0B3      *
      *                    DB2PRD.PDIDO_FLEXZ_AG         GFCTB0B1      *
      *                    DB2PRD.PDIDO_FLEXZ_SGMTO      GFCTB0C0      *
      *                    DB2PRD.PDIDO_FLEXZ_PAB        GFCTB0B8      *
      *                    DB2PRD.PDIDO_FLEXZ_PSTAL      GFCTB0B9      *
      *                    DB2PRD.PDIDO_FLEXZ_MUN        GFCTB0B7      *
      *                    DB2PRD.PDIDO_FLEXZ_UF         GFCTB0C1      *
      *                    DB2PRD.PDIDO_FLEXZ_RZ         GFCTB0M7      *
      *                    DB2PRD.TTPO_SGMTO_GSTAO       GFCTB0H5      *
      *                    DB2PRD.V01UF                  DCITV002      *
      *                    DB2PRD.SERVC_TARIF_PRINC      GFCTB0D8      *
      *                    DB2PRD.TPO_OPER_MOTVO         GFCTB0G0      *
      *                    DB2PRD.MOTVO_JUSTF_OPER       GFCTB089      *
      *                    DB2PRD.FLEXZ-GRP-UF           GFCTB057      *
      *                    DB2PRD.FLEXZ-GRP-MUN          GFCTB053      *
      *                    DB2PRD.FLEXZ-GRP-PSTAL        GFCTB055      *
      *                    DB2PRD.FLEXZ-GRP-PAB          GFCTB054      *
      *                    DB2PRD.FLEXZ-GRP-SGMTO        GFCTB056      *
      *                    DB2PRD.FLEXZ-GRP-DEPDC        GFCTB051      *
      *                    DB2PRD.FLEXZ-GRP-CLI          GFCTB050      *
      *                    DB2PRD.FLEXZ-AGPTO-CTA        GFCTB049      *
      *                    DB2PRD.FLEXZ_GRP_RZ           GFCTB0M8      *
090708*                    DB2PRD.TPO_AGPTO_CTA          GFCTB0F3      *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    I#GFCTNL - AREA DE COMUNICACAO - ENTRADA                    *
      *    I#GFCTNM - AREA DE COMUNICACAO - SAIDA                      *
      *    I#GFCTG2 - AREA DE COMUNICACAO - ENTRADA (GFCT5523)         *
      *    I#GFCTG3 - AREA DE COMUNICACAO - SAIDA   (GFCT5523)         *
      *    I#GFCTFZ - AREA DE COMUNICACAO - ENTRADA (GFCT5522)         *
      *    I#GFCTG1 - AREA DE COMUNICACAO - SAIDA   (GFCT5522)         *
      *    I#GFCT1V - AREA DE COMUNICACAO - ENTRADA (GFCT5006)         *
      *    I#GFCT1X - AREA DE COMUNICACAO - ENTRADA (GFCT5006)         *
      *    I#GFCTGB - AREA DE COMUNICACAO - ENTRADA (GFCT5109-GFCT5090)*
      *    I#GFCTGC - AREA DE COMUNICACAO - ENTRADA (GFCT5109-GFCT5090)*
      *    I#RURC88 - AREA DE COMUNICACAO COM MODULO RURC9020          *
      *    I#GFCT0M - AREA DE COMUNICACAO - ERROS                      *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    POOL0081 - ALOCA DINAMICAMENTE UM MODULO                    *
      *    GFCT5006 - DESCRICAO DEPENDENCIA                            *
      *    GFCT5090 - DESCRICAO POSTO SERVICO                          *
      *    GFCT5109 - OBTER DESCRICAO DE PAB                           *
      *    GFCT5522 - VERIFICA ON-LINE                                 *
      *    GFCT5523 - OBTER DESCRICAO DE MENSAGENS                     *
      *    DCIT8000 - VERIFICAR UF                                     *
      *    RURC9020 - VERIFICAR MUNICIPIO                              *
      *    MESU9023 - PESQUISAR NOME DA JUNCAO                         *
090708*    SARH8000 - MODULO PARA PERMITIR AOS SISTEMAS LEGADOS O ACES-*
090708*               SO AS INFORMACOES DE FUNCIONARIOS.               *
      *================================================================*
      *    ULTIMA ALTERACAO EM 11/06/2008 - VALERIA                    *
      *    ALTERACOES REFERENTES AOS AGRUPAMENTOS PERSONALIZADOS       *
      *    - INCLUSAO DOS AGRUPAMENTOS PERSONALIZADOS                  *
      *    - INCLUSAO CAMPOS PERIODO E INICIO DE PERIODO DE COBRANCA   *
      *    - INCLUSAO CAMPOS COD E NOME FUNCIONARIO SOLICITACAO E      *
      *      PARECER E PESQUISA DO NOME ATRAVES DO MODULO SARH8000     *
      *----------------------------------------------------------------*
0310  *    ULTIMA ALTERACAO EM 29/03/2010 - HELIO SANTONI              *
0310  *    - ACRESCENTAR TRATAMENTO DOS AGRUPAMENTOS 20 E 26.          *
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
       77  WRK-GFCTNM-QTDE-TOT-REG     PIC  9(009) COMP-3  VALUE ZERO.
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREAS AUXILIARES *'.
      *----------------------------------------------------------------*

       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-NOME-TAB                PIC  X(011)         VALUE SPACES.

       77  WRK-DATA-INIC-FLEX          PIC  9(008)         VALUE ZEROS.
       77  WRK-DATA-FIM-FLEX           PIC  9(008)         VALUE ZEROS.
       77  WRK-DATA-AUX1               PIC  9(008)         VALUE ZEROS.
       77  WRK-DATA-AUX2               PIC  9(008)         VALUE ZEROS.
       77  WRK-DATA-INIC-TARIF         PIC  9(008)         VALUE ZEROS.
       77  WRK-DATA-FIM-TARIF          PIC  9(008)         VALUE ZEROS.

       01  WRK-DATA-AUX.
           05  WRK-DIA-AUX             PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  WRK-MES-AUX             PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  WRK-ANO-AUX             PIC  9(004)         VALUE ZEROS.

       01  WRK-DATA-INV                PIC  9(008)         VALUE ZEROS.
       01  WRK-DATA-INV-R              REDEFINES WRK-DATA-INV.
           05  WRK-ANO-INV             PIC  9(004).
           05  WRK-MES-INV             PIC  9(002).
           05  WRK-DIA-INV             PIC  9(002).

090708 01  WRK-AUX-9-S                 PIC +9(009)         VALUE ZEROS.
090708 01  FILLER                      REDEFINES WRK-AUX-9-S.
090708     05 FILLER                   PIC  X(001).
090708     05 WRK-AUX-9                PIC  9(009).
090708     05 FILLER                   REDEFINES WRK-AUX-9.
090708       10 FILLER                 PIC  X(002).
090708       10 WRK-AUX-9-7-A.
090708         15 WRK-AUX-9-7          PIC  9(007).

       01  WRK-MSG01.
           05  FILLER                  PIC  X(026)         VALUE
               'ERRO NA CHAMADA DO MODULO'.
           05  WRK-NOME-MOD            PIC  X(008)         VALUE SPACES.

       01  WRK-COD-SQL-ERRO            PIC S9(09)     VALUE ZEROS.
       01  FILLER                      REDEFINES      WRK-COD-SQL-ERRO.
           05  FILLER                  PIC  X(006).
           05  WRK-COD-SQL-ERRO-2      PIC  9(003).
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA MODULO MESU9023 *'.
      *----------------------------------------------------------------*

       01  WRK-AMBIENTE                PIC  X(001)         VALUE SPACES.

       01  WRK-PARM.
           05  WRK-ENTRADA.
             10  WRK-ENT-NOME-PGM      PIC  X(008)         VALUE SPACES.
             10  WRK-ENT-COD-RETORNO   PIC  9(002)         VALUE ZEROS.
             10  WRK-ENT-TPO-RELAC     PIC  9(003)         VALUE ZEROS.
             10  WRK-ENT-BANCO         PIC  9(005)         VALUE ZEROS.
             10  WRK-ENT-AGENCIA       PIC  9(005)         VALUE ZEROS.
           05  WRK-SAIDA.
             10  WRK-SAI-EMPR-RELAC    PIC  9(005)         VALUE ZEROS.
             10  WRK-SAI-DEPTO-RELAC   PIC  9(005)         VALUE ZEROS.
             10  WRK-SAI-NOME-DEPDC    PIC  X(050)         VALUE SPACES.

       01  WRK-IO-PCB.
           05  WRK-IO-LTERM            PIC X(008)          VALUE SPACES.
           05  FILLER                  PIC X(002)          VALUE SPACES.
           05  WRK-IO-STATUS           PIC X(002)          VALUE SPACES.
           05  FILLER                  PIC X(012)          VALUE SPACES.
           05  WRK-IO-MODNAME          PIC X(008)          VALUE SPACES.

       01  WRK-ALT-PCB.
           05  WRK-ALT-LTERM           PIC X(008)          VALUE SPACES.
           05  FILLER                  PIC X(002)          VALUE SPACES.
           05  WRK-ALT-STATUS          PIC X(002)          VALUE SPACES.
           05  FILLER                  PIC X(012)          VALUE SPACES.
           05  WRK-ALT-MODNAME         PIC X(008)          VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA MODULO DCIT8000 *'.
      *----------------------------------------------------------------*

       01  WRK-DCIT-UNIDADE-FEDERATIVA.
           05  WRK-AREA-DCLGEN-DCITV002 PIC X(070)         VALUE SPACES.
           05  WRK-DCIT-CODIGO-RETORNO  PIC 9(002)         VALUE ZEROS.

       01  WRK-POOL7100.
           05  WRK-TIPO-ACESSO          PIC  X(03)         VALUE SPACES.
           05  WRK-PGM                  PIC  X(08)         VALUE SPACES.
           05  WRK-DADOS-IMS-DB2.
             10  WRK-DBD-TAB            PIC  X(18)         VALUE SPACES.
             10  WRK-FUN-COMANDO        PIC  X(10)         VALUE SPACES.
             10  WRK-STA-CODE           PIC  X(04)         VALUE SPACES.
             10  WRK-SQL-CODE REDEFINES WRK-STA-CODE PIC S9(09) COMP-4.
             10  WRK-LOCAL              PIC  X(04)         VALUE SPACES.
             10  WRK-SEGM               PIC  X(08)         VALUE SPACES.
             10  FILLER                 PIC  X(31)         VALUE SPACES.
           05  WRK-MQSERIES   REDEFINES WRK-DADOS-IMS-DB2.
             10  WRK-MQ-FILA            PIC  X(48).
             10  WRK-MQ-FUNCAO          PIC  X(10).
             10  WRK-MQ-RETURN-CODE     PIC S9(09) COMP-4.
             10  WRK-MQ-REASON-CODE     PIC S9(09) COMP-4.
             10  WRK-MQ-LOCAL           PIC  X(04).
             10  FILLER                 PIC  X(05).
           05  WRK-TEXTO      REDEFINES WRK-MQSERIES PIC X(75).

       01  WRK-SQLCA                    PIC X(136)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREAS DE COMUNICACAO COM GFCT5522 *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTFZ'.

       COPY 'I#GFCTG1'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREAS DE COMUNICACAO COM GFCT5523 *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTG2'.

       COPY 'I#GFCTG3'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREAS DE COMUNICACAO COM GFCT5006 *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCT1V'.

       COPY 'I#GFCT1X'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREAS DE COMUNICACAO COM GFCT5109 - GFCT5090 *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTGB'.

       COPY 'I#GFCTGC'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREAS DE COMUNICACAO COM RURC9020 *'.
      *----------------------------------------------------------------*

       COPY 'I#RURC88'.

090708*----------------------------------------------------------------*
090708 01  FILLER                      PIC  X(032)         VALUE
090708     'AREA DE COMUNICACAO COM SARH8000'.
090708*----------------------------------------------------------------*
090708
090708 01 WRK-ENT.
090708    05 WRK-PESQUISA-ENT          PIC 9(009)          VALUE ZEROS.
090708    05 WRK-TAMANHO-ENT           PIC 9(004)          VALUE ZEROS.
090708    05 WRK-SOLICITA-ENT.
090708      10 FILLER                  OCCURS 255 TIMES.
090708        15 WRK-CAMPO-ENT         PIC 9(003)          VALUE ZEROS.
090708
090708 01 WRK-SAI.
090708    05 WRK-COD-RETORNO-SAI       PIC 9(002)          VALUE ZEROS.
090708    05 WRK-TAMANHO-SAI           PIC 9(004)          VALUE ZEROS.
090708    05 WRK-REGISTRO-SAI.
090708      10 FILLER                  OCCURS 0 TO 1000 TIMES
090708                                 DEPENDING ON WRK-TAMANHO-SAI.
090708        15 WRK-POSICAO-SAI       PIC X(001)          VALUE SPACES.
090708
090708 01  WRK-AREA-RET.
090708     05 WRK-NOME-RET             PIC  X(040)         VALUE SPACES.
090708     05 FILLER                   PIC  X(040)         VALUE SPACES.
090708
090708 01  WRK-ERRO-AREA               PIC  X(107)         VALUE SPACES.
090708 01  WRK-SQLCA-NOME              PIC  X(136)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE DB2 *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0A1
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0B1
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0B2
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0B3
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0B7
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0B8
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0B9
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0C0
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0C1
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0D8
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0G0
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0H5
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
               INCLUDE GFCTB089
           END-EXEC.

090708     EXEC SQL
090708         INCLUDE GFCTB0F3
090708     END-EXEC.

           EXEC SQL
               INCLUDE DCITV002
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0M7
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0M8
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* FIM DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY 'I#GFCTNL'.

       COPY 'I#GFCTNM'.

       COPY 'I#GFCT0M'.

      *================================================================*
       PROCEDURE DIVISION              USING GFCTNL-ENTRADA
                                             GFCTNM-SAIDA
                                             GFCT0M-AREA-ERROS.
      *================================================================*

      ******************************************************************
      * ROTINA INICIAL DO PROGRAMA                                     *
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
      * ROTINA QUE SERA PROCESSADA APENA UMA VEZ NO INICIO DO PROGRAMA *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTNM-SAIDA
                                          GFCTG2-ENTRADA
                                          GFCTG3-SAIDA
                                          GFCTFZ-ENTRADA
                                          GFCTG1-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE GFCTNM-SAIDA
                      GFCTG2-ENTRADA
                      GFCTG3-SAIDA
                      GFCTFZ-ENTRADA
                      GFCTG1-SAIDA
                      GFCT0M-AREA-ERROS.

           PERFORM 1100-VERIFICAR-SIST-DISPONIVEL.

           PERFORM 1200-INICIALIZAR-SAIDA.

           PERFORM 1300-CONSISTIR-DADOS.

           PERFORM 1400-OBTER-DATA-ATUAL.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA VERIFICAR DISPONIBILIDADE DO SISTEMA               *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-VERIFICAR-SIST-DISPONIVEL  SECTION.
      *----------------------------------------------------------------*

           MOVE  100                           TO GFCTFZ-LL.
           MOVE  ZEROS                         TO GFCTFZ-ZZ.
           MOVE 'N'                            TO GFCTFZ-FIM.
           MOVE  GFCTNL-FUNC-BDSCO             TO GFCTFZ-FUNC-BDSCO.
           MOVE  GFCTNL-TRANSACAO              TO GFCTFZ-TRANSACAO.
           MOVE  GFCTNL-FUNCAO                 TO GFCTFZ-FUNCAO.
           MOVE 'GFCT'                         TO GFCTFZ-SISTEMA.

           MOVE 'GFCT5522'                     TO WRK-MODULO.

           CALL WRK-MODULO USING
                                                     GFCTFZ-ENTRADA
                                                     GFCTG1-SAIDA
                                                     GFCT0M-AREA-ERROS.

           IF  RETURN-CODE                     NOT EQUAL ZEROS
               MOVE 9                          TO GFCTNM-ERRO
               MOVE WRK-MODULO                 TO WRK-NOME-MOD
                                                  GFCT0M-PROGRAMA
               MOVE WRK-MSG01                  TO GFCT0M-TEXTO
               MOVE 'APL'                      TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0884'                 TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-ERRO                     EQUAL 9
               IF  GFCT0M-TIPO-ACESSO          NOT EQUAL 'DB2'
                   MOVE GFCTG1-DESC-MSG-ERRO   TO GFCT0M-TEXTO
                   MOVE 'APL'                  TO GFCT0M-TIPO-ACESSO
               ELSE
                   MOVE GFCTG1-COD-SQL-ERRO    TO GFCTNM-COD-SQL-ERRO
               END-IF
               MOVE WRK-MODULO                 TO GFCT0M-PROGRAMA
               MOVE 'GFCT0884'                 TO GFCT0M-TRANSACAO
               MOVE GFCTG1-ERRO                TO GFCTNM-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-CSIT-ONLINE              EQUAL 1
               MOVE 9                          TO GFCTNM-ERRO
               MOVE 'SISTEMA INDISPONIVEL'     TO GFCT0M-TEXTO
               MOVE WRK-MODULO                 TO GFCT0M-PROGRAMA
               MOVE 'APL'                      TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0884'                 TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA INICIALIZAR AREA DE SAIDA                          *
      ******************************************************************
      *----------------------------------------------------------------*
       1200-INICIALIZAR-SAIDA          SECTION.
      *----------------------------------------------------------------*

           MOVE  1010                  TO GFCTNM-LL.
           MOVE  ZEROS                 TO GFCTNM-ZZ.
           MOVE  GFCTNL-TRANSACAO      TO GFCTNM-TRANSACAO.
           MOVE  GFCTNL-FUNCAO         TO GFCTNM-FUNCAO.

           IF  GFCTNL-QTDE-OCOR        NOT NUMERIC
               MOVE ZEROS              TO GFCTNM-QTDE-OCOR
           ELSE
               MOVE GFCTNL-QTDE-OCOR   TO GFCTNM-QTDE-OCOR
           END-IF.

           IF  GFCTNL-QTDE-TOT-REG     NOT NUMERIC
               MOVE ZEROS              TO GFCTNM-QTDE-TOT-REG
           ELSE
               MOVE GFCTNL-QTDE-TOT-REG TO GFCTNM-QTDE-TOT-REG
           END-IF.

           MOVE GFCTNL-FUNC-BDSCO      TO GFCTNM-FUNC-BDSCO.

           MOVE  ZEROS                 TO GFCTNM-COD-SQL-ERRO
                                          GFCTNM-COD-MSG-ERRO
                                          GFCTNM-ERRO.
           MOVE '0000'                 TO GFCTG2-COD-MSG.

           PERFORM 1310-OBTER-DESC-MENSAGEM.

           MOVE GFCTG3-DESC-MSG        TO GFCTNM-DESC-MSG-ERRO.

           MOVE 'S'                    TO GFCTNM-FIM.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA CONSISTIR DADOS DE ENTRADA                         *
      ******************************************************************
      *----------------------------------------------------------------*
       1300-CONSISTIR-DADOS            SECTION.
      *----------------------------------------------------------------*

           IF  GFCTNL-TRANSACAO        EQUAL SPACES     OR
               GFCTNL-TRANSACAO        EQUAL LOW-VALUES OR
               GFCTNL-FUNCAO           EQUAL SPACES     OR
               GFCTNL-FUNCAO           EQUAL LOW-VALUES OR
               GFCTNL-FUNC-BDSCO       EQUAL SPACES     OR
               GFCTNL-FUNC-BDSCO       EQUAL LOW-VALUES OR
               GFCTNL-AGRUPMTO-PED     NOT NUMERIC      OR
               GFCTNL-AGRUPMTO         NOT NUMERIC      OR
               GFCTNL-TARIFA-PED       NOT NUMERIC      OR
               GFCTNL-TARIFA           NOT NUMERIC      OR
               GFCTNL-SEQUENCIA-PED    NOT NUMERIC
               MOVE 1                  TO GFCTNM-ERRO
                                          GFCTNM-COD-MSG-ERRO
               MOVE ZEROS              TO GFCTNM-COD-SQL-ERRO
               MOVE '0001'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTNM-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA OBTER DESCRICAO DA MENSAGEM                        *
      ******************************************************************
      *----------------------------------------------------------------*
       1310-OBTER-DESC-MENSAGEM        SECTION.
      *----------------------------------------------------------------*

           MOVE 100                      TO GFCTG2-LL.
           MOVE ZEROS                    TO GFCTG2-ZZ.
           MOVE GFCTNL-TRANSACAO         TO GFCTG2-TRANSACAO.
           MOVE GFCTNL-FUNCAO            TO GFCTG2-FUNCAO.
           MOVE GFCTNL-FUNC-BDSCO        TO GFCTG2-FUNC-BDSCO.
           MOVE 'O'                      TO GFCTG2-TIPO-PROC.
           MOVE 'N'                      TO GFCTG2-FIM.

           MOVE 'GFCT5523'               TO WRK-MODULO.

           CALL WRK-MODULO USING
                                               GFCTG2-ENTRADA
                                               GFCTG3-SAIDA
                                               GFCT0M-AREA-ERROS.

           IF  RETURN-CODE               NOT EQUAL ZEROS
               MOVE 9                    TO GFCTNM-ERRO
               MOVE WRK-MODULO           TO GFCT0M-PROGRAMA
                                            WRK-NOME-MOD
               MOVE WRK-MSG01            TO GFCT0M-TEXTO
               MOVE 'APL'                TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0884'           TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO               EQUAL 9
               MOVE 9                    TO GFCTNM-ERRO
               MOVE WRK-MODULO           TO GFCT0M-PROGRAMA
                                            WRK-NOME-MOD
               MOVE WRK-MSG01            TO GFCT0M-TEXTO
               MOVE 'APL'                TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0884'           TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO               EQUAL 1
               MOVE 1                    TO GFCTNM-ERRO
               MOVE GFCTG3-COD-SQL-ERRO  TO GFCTNM-COD-SQL-ERRO
               MOVE GFCTG3-COD-MSG-ERRO  TO GFCTNM-COD-MSG-ERRO
               MOVE GFCTG3-DESC-MSG-ERRO TO GFCTNM-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1400-OBTER-DATA-ATUAL           SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC OF GFCTB0A1.

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
              (SQLWARN0                EQUAL 'W'      )
               MOVE  '0010'            TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               STRING GFCTG3-DESC-MSG ' - GFCTB0A1'
                   DELIMITED BY '  '   INTO GFCTNM-DESC-MSG-ERRO
               MOVE 'GFCT0884'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'PARM_DATA_PROCM'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
                                          GFCTNM-COD-SQL-ERRO
               MOVE '0010'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCTNM-ERRO
               MOVE  10                TO GFCTNM-COD-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA PROCESSAMENTO DO PROGRAMA                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           IF  GFCTNL-TIPO-CONSULTA        EQUAL 'H' OR 'C' OR 'R'
               MOVE  GFCTNL-AGRUPMTO-PED   TO GFCTNM-AGRUPMTO
               MOVE  GFCTNL-SEQUENCIA-PED  TO GFCTNM-SEQUENCIA
               MOVE  GFCTNL-TARIFA-PED     TO GFCTNM-TARIFA
               MOVE  GFCTNL-TIMESTAMP-PED  TO GFCTNM-TIMESTAMP
               IF  GFCTNL-TIPO-CONSULTA    EQUAL 'R'
                   PERFORM 2050-PESQUISA-PEDIDO-FLEX
                   PERFORM 2200-TRATAR-CSIT-SOLTC
               ELSE
                   MOVE  GFCTNL-TIMESTAMP-PED  TO GFCTNM-TIMESTAMP
                   PERFORM 2100-LER-TAB-HISTORICO
                   PERFORM 2200-TRATAR-CSIT-SOLTC
               END-IF
               MOVE  GFCTNL-TARIFA-PED     TO CSERVC-TARIF OF GFCTB0D8
           ELSE
               MOVE  GFCTNL-TIMESTAMP-PED  TO GFCTNM-TIMESTAMP
               MOVE  GFCTNL-AGRUPMTO       TO GFCTNM-AGRUPMTO
               MOVE  GFCTNL-TARIFA         TO GFCTNM-TARIFA
               PERFORM 2300-LER-TAB-QUENTE

               MOVE  GFCTNL-TARIFA         TO CSERVC-TARIF OF GFCTB0D8
           END-IF.

           PERFORM 2210-PESQUISAR-NOME-TARIFA.

           MOVE  RSERVC-TARIF-REDZD    OF GFCTB0D8
                                       TO GFCTNM-NOME-TARIFA.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2050-PESQUISA-PEDIDO-FLEX       SECTION.
      *----------------------------------------------------------------*

           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO DINIC-FLEXZ-AGPTO OF GFCTB0B2
                                          DFIM-FLEXZ-AGPTO  OF GFCTB0B2.

           MOVE  GFCTNL-AGRUPMTO-PED   TO CAGPTO-CTA       OF GFCTB0B2.
           MOVE  GFCTNL-TARIFA-PED     TO CSERVC-TARIF     OF GFCTB0B2.
           MOVE  ZEROS                 TO RJUSTF-SOLTC-FLEXZ-LEN
                                                           OF GFCTB0B2
                                          RJUSTF-PAREC-FLEXZ-LEN
                                                           OF GFCTB0B2.
           MOVE  SPACES                TO RJUSTF-SOLTC-FLEXZ-TEXT
                                                           OF GFCTB0B2
                                          RJUSTF-PAREC-FLEXZ-TEXT
                                                           OF GFCTB0B2.


           EVALUATE GFCTNL-AGRUPMTO-PED
               WHEN 15
                   PERFORM 2051-SELECIONAR-PAB-PEN

               WHEN 14
                   IF  CINDCD-AGPTO-TOT    OF GFCTB0B2 NOT EQUAL 'S'
                       PERFORM 2052-SELECIONAR-AGENCIA-PEN
                   ELSE
                       MOVE  ZEROS         TO GFCTNM-AGENCIA
                   END-IF

               WHEN 17
                   PERFORM 2053-SELECIONAR-POSTAL-PEN

               WHEN 4
                   PERFORM 2054-SELECIONAR-CLIENTE-PEN

               WHEN 9
                   PERFORM 2055-SELECIONAR-SEGMENTO-PEN

               WHEN 18
                   PERFORM 2056-SELECIONAR-MUNICIPIO-PEN

               WHEN 19
                   PERFORM 2057-SELECIONAR-UF-PEN

0310           WHEN 20
0310               PERFORM 2053-SELECIONAR-POSTAL-PEN
0310
0310           WHEN 26
0310               PERFORM 2060-SELECIONAR-RAZAO-PEN

090708         WHEN OTHER
090708             PERFORM 2058-SELECIONAR-PERSONALIZ

           END-EVALUATE.

           MOVE  HIDTFD-FLEXZ-AGPTO    OF GFCTB0B2
                                       TO GFCTNM-TIMESTAMP.

090708     IF CFUNC-SOLTC-FLEXZ  OF GFCTB0B2
090708                                 NOT EQUAL ZEROS
090708        MOVE CFUNC-SOLTC-FLEXZ   OF GFCTB0B2
090708                                 TO WRK-AUX-9-S
090708        MOVE WRK-AUX-9           TO WRK-PESQUISA-ENT
090708
090708        PERFORM 2059-OBTER-NOME-FUNC
090708        MOVE WRK-NOME-RET(1:40) TO GFCTNM-NOME-FUNC-SOLIC
090708        MOVE WRK-AUX-9-7        TO GFCTNM-FUNC-SOLIC
090708     ELSE
090708        MOVE SPACES             TO GFCTNM-NOME-FUNC-SOLIC
090708        MOVE ZEROS              TO GFCTNM-FUNC-SOLIC
090708     END-IF
090708
090708     IF CFUNC-PAREC-FLEXZ  OF GFCTB0B2
090708                                 NOT EQUAL ZEROS
090708        MOVE CFUNC-PAREC-FLEXZ   OF GFCTB0B2
090708                                 TO WRK-AUX-9-S
090708        MOVE WRK-AUX-9           TO WRK-PESQUISA-ENT
090708
090708        PERFORM 2059-OBTER-NOME-FUNC
090708        MOVE WRK-NOME-RET(1:40) TO GFCTNM-NOME-FUNC-PAREC
090708        MOVE WRK-AUX-9-7        TO GFCTNM-FUNC-PAREC
090708     ELSE
090708        MOVE SPACES             TO GFCTNM-NOME-FUNC-PAREC
090708        MOVE ZEROS              TO GFCTNM-FUNC-PAREC
090708     END-IF.

      *----------------------------------------------------------------*
       2050-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2051-SELECIONAR-PAB-PEN         SECTION.
      *----------------------------------------------------------------*

           MOVE  GFCTNL-AGRUPMTO-PED   TO CAGPTO-CTA       OF GFCTB0B8.
           MOVE  GFCTNL-TARIFA-PED     TO CSERVC-TARIF     OF GFCTB0B8.
           MOVE  GFCTNL-AGENCIA        TO CDEPDC           OF GFCTB0B8.
           MOVE  GFCTNL-POSTO          TO CPOSTO-SERVC     OF GFCTB0B8.

           EXEC SQL
               SELECT
                 A.CAGPTO_CTA,
                 A.CSERVC_TARIF,
                 A.HIDTFD_FLEXZ_AGPTO,
                 A.CFUNC_SOLTC_FLEXZ,
                 A.CFUNC_PAREC_FLEXZ,
                 A.CTPO_OPER_MOTVO,
                 A.CMOTVO_JUSTF,
                 A.CEMPR_ORIGE_FLEXZ,
                 A.CDEPDC_ORIGE_FLEXZ,
                 A.CEMPR_PAREC_FLEXZ,
                 A.CDEPDC_PAREC_FLEXZ,
                 A.DINIC_FLEXZ_AGPTO,
                 A.DFIM_FLEXZ_AGPTO,
                 A.QFRANQ_FLEXZ_AGPTO,
                 A.CPER_FRANQ_AGPTO,
                 A.VNEGOC_FLEXZ_AGPTO,
                 A.PDESC_FLEXZ_AGPTO,
                 A.DDIA_AGNDA,
                 A.QDIA_FLEXZ_AGPTO,
                 A.HSOLTC_FLEXZ,
                 A.RJUSTF_SOLTC_FLEXZ,
                 A.HPAREC_FLEXZ,
                 A.RJUSTF_PAREC_FLEXZ,
                 A.CSIT_SOLTC_FLEXZ,
090708           A.CINDCD_AGPTO_TOT,
090708           A.CPER_COBR_TARIF,
090708           A.CINIC_PER_COBR
               INTO
                   :GFCTB0B2.CAGPTO-CTA,
                   :GFCTB0B2.CSERVC-TARIF,
                   :GFCTB0B2.HIDTFD-FLEXZ-AGPTO,
                   :GFCTB0B2.CFUNC-SOLTC-FLEXZ,
                   :GFCTB0B2.CFUNC-PAREC-FLEXZ,
                   :GFCTB0B2.CTPO-OPER-MOTVO,
                   :GFCTB0B2.CMOTVO-JUSTF,
                   :GFCTB0B2.CEMPR-ORIGE-FLEXZ,
                   :GFCTB0B2.CDEPDC-ORIGE-FLEXZ,
                   :GFCTB0B2.CEMPR-PAREC-FLEXZ,
                   :GFCTB0B2.CDEPDC-PAREC-FLEXZ,
                   :GFCTB0B2.DINIC-FLEXZ-AGPTO,
                   :GFCTB0B2.DFIM-FLEXZ-AGPTO,
                   :GFCTB0B2.QFRANQ-FLEXZ-AGPTO,
                   :GFCTB0B2.CPER-FRANQ-AGPTO,
                   :GFCTB0B2.VNEGOC-FLEXZ-AGPTO,
                   :GFCTB0B2.PDESC-FLEXZ-AGPTO,
                   :GFCTB0B2.DDIA-AGNDA,
                   :GFCTB0B2.QDIA-FLEXZ-AGPTO,
                   :GFCTB0B2.HSOLTC-FLEXZ,
                   :GFCTB0B2.RJUSTF-SOLTC-FLEXZ,
                   :GFCTB0B2.HPAREC-FLEXZ,
                   :GFCTB0B2.RJUSTF-PAREC-FLEXZ,
                   :GFCTB0B2.CSIT-SOLTC-FLEXZ,
090708             :GFCTB0B2.CINDCD-AGPTO-TOT,
090708             :GFCTB0B2.CPER-COBR-TARIF,
090708             :GFCTB0B2.CINIC-PER-COBR
             FROM   DB2PRD.PDIDO_FLEXZ_AGPTO A,
                    DB2PRD.PDIDO_FLEXZ_PAB   B
             WHERE
              A.CAGPTO_CTA           = B.CAGPTO_CTA                  AND
              A.CSERVC_TARIF         = B.CSERVC_TARIF                AND
              A.HIDTFD_FLEXZ_AGPTO   = B.HIDTFD_FLEXZ_AGPTO          AND
              A.CSERVC_TARIF         = :GFCTB0B2.CSERVC-TARIF        AND
              B.CAGPTO_CTA           = :GFCTB0B8.CAGPTO-CTA          AND
              B.CSERVC_TARIF         = :GFCTB0B8.CSERVC-TARIF        AND
              B.CDEPDC               = :GFCTB0B8.CDEPDC              AND
              B.CPOSTO_SERVC         = :GFCTB0B8.CPOSTO-SERVC        AND
             (A.CSIT_SOLTC_FLEXZ  IN  (05, 08)                       OR
              (A.CSIT_SOLTC_FLEXZ    = 02                            AND
              (A.DINIC_FLEXZ_AGPTO <= :GFCTB0B2.DINIC-FLEXZ-AGPTO))) AND
              A.HIDTFD_FLEXZ_AGPTO  IN
                                 (SELECT MAX (A.HIDTFD_FLEXZ_AGPTO)
                                  FROM   DB2PRD.PDIDO_FLEXZ_AGPTO A,
                                         DB2PRD.PDIDO_FLEXZ_PAB   B
                                  WHERE A.CAGPTO_CTA            =
                                        B.CAGPTO_CTA
                                    AND A.CSERVC_TARIF          =
                                        B.CSERVC_TARIF
                                    AND A.HIDTFD_FLEXZ_AGPTO    =
                                        B.HIDTFD_FLEXZ_AGPTO
                                    AND B.CAGPTO_CTA            =
                                        :GFCTB0B8.CAGPTO-CTA
                                    AND B.CSERVC_TARIF          =
                                        :GFCTB0B8.CSERVC-TARIF
                                    AND B.CDEPDC                =
                                        :GFCTB0B8.CDEPDC
                                    AND B.CPOSTO_SERVC          =
                                        :GFCTB0B8.CPOSTO-SERVC
                                    AND  A.CAGPTO_CTA           =
                                        :GFCTB0B2.CAGPTO-CTA
                                    AND  A.CSERVC_TARIF         =
                                        :GFCTB0B2.CSERVC-TARIF
                                    AND (A.CSIT_SOLTC_FLEXZ IN  (05, 08)
                                     OR  (A.CSIT_SOLTC_FLEXZ    = 02
                                    AND   (A.DINIC_FLEXZ_AGPTO <=
                                        :GFCTB0B2.DINIC-FLEXZ-AGPTO))))
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE '0010'              TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE ' - GFCTB0B2'       TO WRK-NOME-TAB
               MOVE 'GFCT0884'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'            TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0020'              TO GFCT0M-LOCAL
               MOVE  SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE  9                  TO GFCTNM-ERRO
               MOVE  SQLCODE            TO GFCTNM-COD-SQL-ERRO
               MOVE  SPACES             TO GFCTNM-DESC-MSG-ERRO
               MOVE  0010               TO GFCTNM-COD-MSG-ERRO
               PERFORM 2110-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE  1                 TO GFCTNM-ERRO
               MOVE  ZEROS             TO GFCTNM-COD-SQL-ERRO
               MOVE  0010              TO GFCTNM-COD-MSG-ERRO
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTNM-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE DINIC-FLEXZ-AGPTO      OF GFCTB0B2
                                       TO WRK-DATA-AUX.
           MOVE WRK-DIA-AUX            TO WRK-DIA-INV.
           MOVE WRK-MES-AUX            TO WRK-MES-INV.
           MOVE WRK-ANO-AUX            TO WRK-ANO-INV.
           MOVE WRK-DATA-INV           TO WRK-DATA-AUX1.
           MOVE DPROCM-ATUAL           TO WRK-DATA-AUX.
           MOVE WRK-DIA-AUX            TO WRK-DIA-INV.
           MOVE WRK-MES-AUX            TO WRK-MES-INV.
           MOVE WRK-ANO-AUX            TO WRK-ANO-INV.
           MOVE WRK-DATA-INV           TO WRK-DATA-AUX2.

           IF  WRK-DATA-AUX1           GREATER WRK-DATA-AUX2
                                       AND CSIT-SOLTC-FLEXZ NOT = 05
               MOVE 1                  TO GFCTNM-ERRO
               MOVE ZEROS              TO GFCTNM-COD-SQL-ERRO
               MOVE 0659               TO GFCTNM-COD-MSG-ERRO
               MOVE '0659'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               STRING GFCTG3-DESC-MSG ' - GFCTB0B2'
                   DELIMITED BY '  '   INTO GFCTNM-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           PERFORM 2120-PROCESSAR-TPO-AGPTO-PEN.

      *----------------------------------------------------------------*
       2051-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2052-SELECIONAR-AGENCIA-PEN     SECTION.
      *----------------------------------------------------------------*

           MOVE  GFCTNL-AGRUPMTO-PED   TO CAGPTO-CTA       OF GFCTB0B1.
           MOVE  GFCTNL-TARIFA-PED     TO CSERVC-TARIF     OF GFCTB0B1.
           MOVE  GFCTNL-AGENCIA        TO CDEPDC           OF GFCTB0B1.

           EXEC SQL
               SELECT
                 A.CAGPTO_CTA,
                 A.CSERVC_TARIF,
                 A.HIDTFD_FLEXZ_AGPTO,
                 A.CFUNC_SOLTC_FLEXZ,
                 A.CFUNC_PAREC_FLEXZ,
                 A.CTPO_OPER_MOTVO,
                 A.CMOTVO_JUSTF,
                 A.CEMPR_ORIGE_FLEXZ,
                 A.CDEPDC_ORIGE_FLEXZ,
                 A.CEMPR_PAREC_FLEXZ,
                 A.CDEPDC_PAREC_FLEXZ,
                 A.DINIC_FLEXZ_AGPTO,
                 A.DFIM_FLEXZ_AGPTO,
                 A.QFRANQ_FLEXZ_AGPTO,
                 A.CPER_FRANQ_AGPTO,
                 A.VNEGOC_FLEXZ_AGPTO,
                 A.PDESC_FLEXZ_AGPTO,
                 A.DDIA_AGNDA,
                 A.QDIA_FLEXZ_AGPTO,
                 A.HSOLTC_FLEXZ,
                 A.RJUSTF_SOLTC_FLEXZ,
                 A.HPAREC_FLEXZ,
                 A.RJUSTF_PAREC_FLEXZ,
                 A.CSIT_SOLTC_FLEXZ,
090708           A.CINDCD_AGPTO_TOT,
090708           A.CPER_COBR_TARIF,
090708           A.CINIC_PER_COBR
               INTO
                   :GFCTB0B2.CAGPTO-CTA,
                   :GFCTB0B2.CSERVC-TARIF,
                   :GFCTB0B2.HIDTFD-FLEXZ-AGPTO,
                   :GFCTB0B2.CFUNC-SOLTC-FLEXZ,
                   :GFCTB0B2.CFUNC-PAREC-FLEXZ,
                   :GFCTB0B2.CTPO-OPER-MOTVO,
                   :GFCTB0B2.CMOTVO-JUSTF,
                   :GFCTB0B2.CEMPR-ORIGE-FLEXZ,
                   :GFCTB0B2.CDEPDC-ORIGE-FLEXZ,
                   :GFCTB0B2.CEMPR-PAREC-FLEXZ,
                   :GFCTB0B2.CDEPDC-PAREC-FLEXZ,
                   :GFCTB0B2.DINIC-FLEXZ-AGPTO,
                   :GFCTB0B2.DFIM-FLEXZ-AGPTO,
                   :GFCTB0B2.QFRANQ-FLEXZ-AGPTO,
                   :GFCTB0B2.CPER-FRANQ-AGPTO,
                   :GFCTB0B2.VNEGOC-FLEXZ-AGPTO,
                   :GFCTB0B2.PDESC-FLEXZ-AGPTO,
                   :GFCTB0B2.DDIA-AGNDA,
                   :GFCTB0B2.QDIA-FLEXZ-AGPTO,
                   :GFCTB0B2.HSOLTC-FLEXZ,
                   :GFCTB0B2.RJUSTF-SOLTC-FLEXZ,
                   :GFCTB0B2.HPAREC-FLEXZ,
                   :GFCTB0B2.RJUSTF-PAREC-FLEXZ,
                   :GFCTB0B2.CSIT-SOLTC-FLEXZ,
090708             :GFCTB0B2.CINDCD-AGPTO-TOT,
090708             :GFCTB0B2.CPER-COBR-TARIF,
090708             :GFCTB0B2.CINIC-PER-COBR
             FROM   DB2PRD.PDIDO_FLEXZ_AGPTO A,
                    DB2PRD.PDIDO_FLEXZ_AG    B
             WHERE
              A.CAGPTO_CTA           = B.CAGPTO_CTA                  AND
              A.CSERVC_TARIF         = B.CSERVC_TARIF                AND
              A.HIDTFD_FLEXZ_AGPTO   = B.HIDTFD_FLEXZ_AGPTO          AND
              A.CSERVC_TARIF         = :GFCTB0B2.CSERVC-TARIF        AND
              B.CAGPTO_CTA           = :GFCTB0B1.CAGPTO-CTA          AND
              B.CSERVC_TARIF         = :GFCTB0B1.CSERVC-TARIF        AND
              B.CDEPDC               = :GFCTB0B1.CDEPDC              AND
             (A.CSIT_SOLTC_FLEXZ     IN  (05, 08)                    OR
              (A.CSIT_SOLTC_FLEXZ    = 02                            AND
              (A.DINIC_FLEXZ_AGPTO <= :GFCTB0B2.DINIC-FLEXZ-AGPTO))) AND
              A.HIDTFD_FLEXZ_AGPTO  IN
                                 (SELECT MAX (A.HIDTFD_FLEXZ_AGPTO)
                                  FROM   DB2PRD.PDIDO_FLEXZ_AGPTO A,
                                         DB2PRD.PDIDO_FLEXZ_AG    B
                                  WHERE A.CAGPTO_CTA            =
                                        B.CAGPTO_CTA
                                    AND A.CSERVC_TARIF          =
                                        B.CSERVC_TARIF
                                    AND A.HIDTFD_FLEXZ_AGPTO    =
                                        B.HIDTFD_FLEXZ_AGPTO
                                    AND B.CAGPTO_CTA            =
                                        :GFCTB0B1.CAGPTO-CTA
                                    AND B.CSERVC_TARIF          =
                                        :GFCTB0B1.CSERVC-TARIF
                                    AND B.CDEPDC                =
                                        :GFCTB0B1.CDEPDC
                                    AND  A.CAGPTO_CTA           =
                                        :GFCTB0B2.CAGPTO-CTA
                                    AND  A.CSERVC_TARIF         =
                                        :GFCTB0B2.CSERVC-TARIF
                                    AND (A.CSIT_SOLTC_FLEXZ IN  (05, 08)
                                     OR  (A.CSIT_SOLTC_FLEXZ    = 02
                                    AND   (A.DINIC_FLEXZ_AGPTO <=
                                        :GFCTB0B2.DINIC-FLEXZ-AGPTO))))
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE '0010'              TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE ' - GFCTB0B2'       TO WRK-NOME-TAB
               MOVE 'GFCT0884'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'            TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0030'              TO GFCT0M-LOCAL
               MOVE  SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE  9                  TO GFCTNM-ERRO
               MOVE  SQLCODE            TO GFCTNM-COD-SQL-ERRO
               MOVE  SPACES             TO GFCTNM-DESC-MSG-ERRO
               MOVE  0010               TO GFCTNM-COD-MSG-ERRO
               PERFORM 2110-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE  1                 TO GFCTNM-ERRO
               MOVE  ZEROS             TO GFCTNM-COD-SQL-ERRO
               MOVE  0010              TO GFCTNM-COD-MSG-ERRO
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTNM-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE DINIC-FLEXZ-AGPTO      OF GFCTB0B2
                                       TO WRK-DATA-AUX.
           MOVE WRK-DIA-AUX            TO WRK-DIA-INV.
           MOVE WRK-MES-AUX            TO WRK-MES-INV.
           MOVE WRK-ANO-AUX            TO WRK-ANO-INV.
           MOVE WRK-DATA-INV           TO WRK-DATA-AUX1.
           MOVE DPROCM-ATUAL           TO WRK-DATA-AUX.
           MOVE WRK-DIA-AUX            TO WRK-DIA-INV.
           MOVE WRK-MES-AUX            TO WRK-MES-INV.
           MOVE WRK-ANO-AUX            TO WRK-ANO-INV.
           MOVE WRK-DATA-INV           TO WRK-DATA-AUX2.

           IF  WRK-DATA-AUX1           GREATER WRK-DATA-AUX2
                                       AND CSIT-SOLTC-FLEXZ NOT = 05
               MOVE 1                  TO GFCTNM-ERRO
               MOVE ZEROS              TO GFCTNM-COD-SQL-ERRO
               MOVE 0659               TO GFCTNM-COD-MSG-ERRO
               MOVE '0659'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               STRING GFCTG3-DESC-MSG ' - GFCTB0B2'
                   DELIMITED BY '  '   INTO GFCTNM-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           PERFORM 2120-PROCESSAR-TPO-AGPTO-PEN.

      *----------------------------------------------------------------*
       2052-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2053-SELECIONAR-POSTAL-PEN      SECTION.
      *----------------------------------------------------------------*

           MOVE  GFCTNL-AGRUPMTO-PED   TO CAGPTO-CTA       OF GFCTB0B9.
           MOVE  GFCTNL-TARIFA-PED     TO CSERVC-TARIF     OF GFCTB0B9.
           MOVE  GFCTNL-AGENCIA        TO CDEPDC           OF GFCTB0B9.
           MOVE  GFCTNL-POSTO          TO CPOSTO-SERVC     OF GFCTB0B9.

           EXEC SQL
               SELECT
                 A.CAGPTO_CTA,
                 A.CSERVC_TARIF,
                 A.HIDTFD_FLEXZ_AGPTO,
                 A.CFUNC_SOLTC_FLEXZ,
                 A.CFUNC_PAREC_FLEXZ,
                 A.CTPO_OPER_MOTVO,
                 A.CMOTVO_JUSTF,
                 A.CEMPR_ORIGE_FLEXZ,
                 A.CDEPDC_ORIGE_FLEXZ,
                 A.CEMPR_PAREC_FLEXZ,
                 A.CDEPDC_PAREC_FLEXZ,
                 A.DINIC_FLEXZ_AGPTO,
                 A.DFIM_FLEXZ_AGPTO,
                 A.QFRANQ_FLEXZ_AGPTO,
                 A.CPER_FRANQ_AGPTO,
                 A.VNEGOC_FLEXZ_AGPTO,
                 A.PDESC_FLEXZ_AGPTO,
                 A.DDIA_AGNDA,
                 A.QDIA_FLEXZ_AGPTO,
                 A.HSOLTC_FLEXZ,
                 A.RJUSTF_SOLTC_FLEXZ,
                 A.HPAREC_FLEXZ,
                 A.RJUSTF_PAREC_FLEXZ,
                 A.CSIT_SOLTC_FLEXZ,
090708           A.CINDCD_AGPTO_TOT,
090708           A.CPER_COBR_TARIF,
090708           A.CINIC_PER_COBR
               INTO
                   :GFCTB0B2.CAGPTO-CTA,
                   :GFCTB0B2.CSERVC-TARIF,
                   :GFCTB0B2.HIDTFD-FLEXZ-AGPTO,
                   :GFCTB0B2.CFUNC-SOLTC-FLEXZ,
                   :GFCTB0B2.CFUNC-PAREC-FLEXZ,
                   :GFCTB0B2.CTPO-OPER-MOTVO,
                   :GFCTB0B2.CMOTVO-JUSTF,
                   :GFCTB0B2.CEMPR-ORIGE-FLEXZ,
                   :GFCTB0B2.CDEPDC-ORIGE-FLEXZ,
                   :GFCTB0B2.CEMPR-PAREC-FLEXZ,
                   :GFCTB0B2.CDEPDC-PAREC-FLEXZ,
                   :GFCTB0B2.DINIC-FLEXZ-AGPTO,
                   :GFCTB0B2.DFIM-FLEXZ-AGPTO,
                   :GFCTB0B2.QFRANQ-FLEXZ-AGPTO,
                   :GFCTB0B2.CPER-FRANQ-AGPTO,
                   :GFCTB0B2.VNEGOC-FLEXZ-AGPTO,
                   :GFCTB0B2.PDESC-FLEXZ-AGPTO,
                   :GFCTB0B2.DDIA-AGNDA,
                   :GFCTB0B2.QDIA-FLEXZ-AGPTO,
                   :GFCTB0B2.HSOLTC-FLEXZ,
                   :GFCTB0B2.RJUSTF-SOLTC-FLEXZ,
                   :GFCTB0B2.HPAREC-FLEXZ,
                   :GFCTB0B2.RJUSTF-PAREC-FLEXZ,
                   :GFCTB0B2.CSIT-SOLTC-FLEXZ,
090708             :GFCTB0B2.CINDCD-AGPTO-TOT,
090708             :GFCTB0B2.CPER-COBR-TARIF,
090708             :GFCTB0B2.CINIC-PER-COBR
             FROM   DB2PRD.PDIDO_FLEXZ_AGPTO A,
                    DB2PRD.PDIDO_FLEXZ_PSTAL B
             WHERE
              A.CAGPTO_CTA           = B.CAGPTO_CTA                  AND
              A.CSERVC_TARIF         = B.CSERVC_TARIF                AND
              A.HIDTFD_FLEXZ_AGPTO   = B.HIDTFD_FLEXZ_AGPTO          AND
              A.CSERVC_TARIF         = :GFCTB0B2.CSERVC-TARIF        AND
              B.CAGPTO_CTA           = :GFCTB0B9.CAGPTO-CTA          AND
              B.CSERVC_TARIF         = :GFCTB0B9.CSERVC-TARIF        AND
              B.CDEPDC               = :GFCTB0B9.CDEPDC              AND
              B.CPOSTO_SERVC         = :GFCTB0B9.CPOSTO-SERVC        AND
             (A.CSIT_SOLTC_FLEXZ     IN  (05, 08)                    OR
              (A.CSIT_SOLTC_FLEXZ    = 02                            AND
              (A.DINIC_FLEXZ_AGPTO <= :GFCTB0B2.DINIC-FLEXZ-AGPTO))) AND
              A.HIDTFD_FLEXZ_AGPTO  IN
                                 (SELECT MAX (A.HIDTFD_FLEXZ_AGPTO)
                                  FROM   DB2PRD.PDIDO_FLEXZ_AGPTO A,
                                         DB2PRD.PDIDO_FLEXZ_PSTAL B
                                  WHERE A.CAGPTO_CTA            =
                                        B.CAGPTO_CTA
                                    AND A.CSERVC_TARIF          =
                                        B.CSERVC_TARIF
                                    AND A.HIDTFD_FLEXZ_AGPTO    =
                                        B.HIDTFD_FLEXZ_AGPTO
                                    AND B.CAGPTO_CTA            =
                                        :GFCTB0B9.CAGPTO-CTA
                                    AND B.CSERVC_TARIF          =
                                        :GFCTB0B9.CSERVC-TARIF
                                    AND B.CDEPDC                =
                                        :GFCTB0B9.CDEPDC
                                    AND B.CPOSTO_SERVC          =
                                        :GFCTB0B9.CPOSTO-SERVC
                                    AND  A.CAGPTO_CTA           =
                                        :GFCTB0B2.CAGPTO-CTA
                                    AND  A.CSERVC_TARIF         =
                                        :GFCTB0B2.CSERVC-TARIF
                                    AND (A.CSIT_SOLTC_FLEXZ IN  (05, 08)
                                     OR  (A.CSIT_SOLTC_FLEXZ    = 02
                                    AND   (A.DINIC_FLEXZ_AGPTO <=
                                        :GFCTB0B2.DINIC-FLEXZ-AGPTO))))
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE '0010'              TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE ' - GFCTB0B2'       TO WRK-NOME-TAB
               MOVE 'GFCT0884'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'            TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0040'              TO GFCT0M-LOCAL
               MOVE  SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE  9                  TO GFCTNM-ERRO
               MOVE  SQLCODE            TO GFCTNM-COD-SQL-ERRO
               MOVE  SPACES             TO GFCTNM-DESC-MSG-ERRO
               MOVE  0010               TO GFCTNM-COD-MSG-ERRO
               PERFORM 2110-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE  1                 TO GFCTNM-ERRO
               MOVE  ZEROS             TO GFCTNM-COD-SQL-ERRO
               MOVE  0010              TO GFCTNM-COD-MSG-ERRO
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTNM-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE DINIC-FLEXZ-AGPTO      OF GFCTB0B2
                                       TO WRK-DATA-AUX.
           MOVE WRK-DIA-AUX            TO WRK-DIA-INV.
           MOVE WRK-MES-AUX            TO WRK-MES-INV.
           MOVE WRK-ANO-AUX            TO WRK-ANO-INV.
           MOVE WRK-DATA-INV           TO WRK-DATA-AUX1.
           MOVE DPROCM-ATUAL           TO WRK-DATA-AUX.
           MOVE WRK-DIA-AUX            TO WRK-DIA-INV.
           MOVE WRK-MES-AUX            TO WRK-MES-INV.
           MOVE WRK-ANO-AUX            TO WRK-ANO-INV.
           MOVE WRK-DATA-INV           TO WRK-DATA-AUX2.

           IF  WRK-DATA-AUX1           GREATER WRK-DATA-AUX2
                                       AND CSIT-SOLTC-FLEXZ NOT = 05
               MOVE 1                  TO GFCTNM-ERRO
               MOVE ZEROS              TO GFCTNM-COD-SQL-ERRO
               MOVE 0659               TO GFCTNM-COD-MSG-ERRO
               MOVE '0659'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               STRING GFCTG3-DESC-MSG ' - GFCTB0B2'
                   DELIMITED BY '  '   INTO GFCTNM-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           PERFORM 2120-PROCESSAR-TPO-AGPTO-PEN.

      *----------------------------------------------------------------*
       2053-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2054-SELECIONAR-CLIENTE-PEN     SECTION.
      *----------------------------------------------------------------*

           MOVE  GFCTNL-AGRUPMTO-PED   TO CAGPTO-CTA       OF GFCTB0B3.
           MOVE  GFCTNL-TARIFA-PED     TO CSERVC-TARIF     OF GFCTB0B3.
ST2506*    M0VE  GFCTNL-CGC-CPF        TO CCGC-CPF         OF GFCTB0B3.
ST2506*    M0VE  GFCTNL-FILIAL         TO CFLIAL-CGC       OF GFCTB0B3.
ST2506     MOVE  GFCTNL-CGC-CPF        TO CCGC-CPF-ST      OF GFCTB0B3.
ST2506     MOVE  GFCTNL-FILIAL         TO CFLIAL-CGC-ST    OF GFCTB0B3.
           MOVE  GFCTNL-CONTROLE       TO CCTRL-CPF-CGC    OF GFCTB0B3.

           EXEC SQL
               SELECT
                 A.CAGPTO_CTA,
                 A.CSERVC_TARIF,
                 A.HIDTFD_FLEXZ_AGPTO,
                 A.CFUNC_SOLTC_FLEXZ,
                 A.CFUNC_PAREC_FLEXZ,
                 A.CTPO_OPER_MOTVO,
                 A.CMOTVO_JUSTF,
                 A.CEMPR_ORIGE_FLEXZ,
                 A.CDEPDC_ORIGE_FLEXZ,
                 A.CEMPR_PAREC_FLEXZ,
                 A.CDEPDC_PAREC_FLEXZ,
                 A.DINIC_FLEXZ_AGPTO,
                 A.DFIM_FLEXZ_AGPTO,
                 A.QFRANQ_FLEXZ_AGPTO,
                 A.CPER_FRANQ_AGPTO,
                 A.VNEGOC_FLEXZ_AGPTO,
                 A.PDESC_FLEXZ_AGPTO,
                 A.DDIA_AGNDA,
                 A.QDIA_FLEXZ_AGPTO,
                 A.HSOLTC_FLEXZ,
                 A.RJUSTF_SOLTC_FLEXZ,
                 A.HPAREC_FLEXZ,
                 A.RJUSTF_PAREC_FLEXZ,
                 A.CSIT_SOLTC_FLEXZ,
090708           A.CINDCD_AGPTO_TOT,
090708           A.CPER_COBR_TARIF,
090708           A.CINIC_PER_COBR
               INTO
                   :GFCTB0B2.CAGPTO-CTA,
                   :GFCTB0B2.CSERVC-TARIF,
                   :GFCTB0B2.HIDTFD-FLEXZ-AGPTO,
                   :GFCTB0B2.CFUNC-SOLTC-FLEXZ,
                   :GFCTB0B2.CFUNC-PAREC-FLEXZ,
                   :GFCTB0B2.CTPO-OPER-MOTVO,
                   :GFCTB0B2.CMOTVO-JUSTF,
                   :GFCTB0B2.CEMPR-ORIGE-FLEXZ,
                   :GFCTB0B2.CDEPDC-ORIGE-FLEXZ,
                   :GFCTB0B2.CEMPR-PAREC-FLEXZ,
                   :GFCTB0B2.CDEPDC-PAREC-FLEXZ,
                   :GFCTB0B2.DINIC-FLEXZ-AGPTO,
                   :GFCTB0B2.DFIM-FLEXZ-AGPTO,
                   :GFCTB0B2.QFRANQ-FLEXZ-AGPTO,
                   :GFCTB0B2.CPER-FRANQ-AGPTO,
                   :GFCTB0B2.VNEGOC-FLEXZ-AGPTO,
                   :GFCTB0B2.PDESC-FLEXZ-AGPTO,
                   :GFCTB0B2.DDIA-AGNDA,
                   :GFCTB0B2.QDIA-FLEXZ-AGPTO,
                   :GFCTB0B2.HSOLTC-FLEXZ,
                   :GFCTB0B2.RJUSTF-SOLTC-FLEXZ,
                   :GFCTB0B2.HPAREC-FLEXZ,
                   :GFCTB0B2.RJUSTF-PAREC-FLEXZ,
                   :GFCTB0B2.CSIT-SOLTC-FLEXZ,
090708             :GFCTB0B2.CINDCD-AGPTO-TOT,
090708             :GFCTB0B2.CPER-COBR-TARIF,
090708             :GFCTB0B2.CINIC-PER-COBR
             FROM   DB2PRD.PDIDO_FLEXZ_AGPTO A,
                    DB2PRD.PDIDO_FLEXZ_CLI   B
             WHERE
              A.CAGPTO_CTA           = B.CAGPTO_CTA                  AND
              A.CSERVC_TARIF         = B.CSERVC_TARIF                AND
              A.HIDTFD_FLEXZ_AGPTO   = B.HIDTFD_FLEXZ_AGPTO          AND
              A.CSERVC_TARIF         = :GFCTB0B2.CSERVC-TARIF        AND
              B.CAGPTO_CTA           = :GFCTB0B3.CAGPTO-CTA          AND
              B.CSERVC_TARIF         = :GFCTB0B3.CSERVC-TARIF        AND
ST2506*       B.CCGC_CPF             = :GFCTB0B3.CCGC-CPF            AND
ST2506*       B.CFLIAL_CGC           = :GFCTB0B3.CFLIAL-CGC          AND
ST2506*       B.CCTRL_CPF_CGC        = :GFCTB0B3.CCTRL-CPF-CGC       AND
ST2506        B.CCGC_CPF_ST          = :GFCTB0B3.CCGC-CPF-ST         AND
ST2506        B.CFLIAL_CGC_ST        = :GFCTB0B3.CFLIAL-CGC-ST       AND
ST2506        B.CCTRL_CPF_CGC_ST     = :GFCTB0B3.CCTRL-CPF-CGC-ST    AND
             (A.CSIT_SOLTC_FLEXZ     IN  (05, 08)                    OR
              (A.CSIT_SOLTC_FLEXZ    = 02                            AND
              (A.DINIC_FLEXZ_AGPTO <= :GFCTB0B2.DINIC-FLEXZ-AGPTO))) AND
              A.HIDTFD_FLEXZ_AGPTO  IN
                                 (SELECT MAX (A.HIDTFD_FLEXZ_AGPTO)
                                  FROM   DB2PRD.PDIDO_FLEXZ_AGPTO A,
                                         DB2PRD.PDIDO_FLEXZ_CLI   B
                                  WHERE A.CAGPTO_CTA            =
                                        B.CAGPTO_CTA
                                    AND A.CSERVC_TARIF          =
                                        B.CSERVC_TARIF
                                    AND A.HIDTFD_FLEXZ_AGPTO    =
                                        B.HIDTFD_FLEXZ_AGPTO
                                    AND B.CAGPTO_CTA            =
                                        :GFCTB0B3.CAGPTO-CTA
                                    AND B.CSERVC_TARIF          =
                                        :GFCTB0B3.CSERVC-TARIF
ST2506*                             AND B.CCGC_CPF              =
ST2506*                                 :GFCTB0B3.CCGC-CPF
ST2506*                             AND B.CFLIAL_CGC            =
ST2506*                                 :GFCTB0B3.CFLIAL-CGC
ST2506*                             AND B.CCTRL_CPF_CGC         =
ST2506*                                 :GFCTB0B3.CCTRL-CPF-CGC
ST2506                              AND B.CCGC_CPF_ST           =
ST2506                                  :GFCTB0B3.CCGC-CPF-ST
ST2506                              AND B.CFLIAL_CGC_ST         =
ST2506                                  :GFCTB0B3.CFLIAL-CGC-ST
ST2506                              AND B.CCTRL_CPF_CGC_ST      =
ST2506                                  :GFCTB0B3.CCTRL-CPF-CGC-ST
                                    AND  A.CAGPTO_CTA           =
                                        :GFCTB0B2.CAGPTO-CTA
                                    AND  A.CSERVC_TARIF         =
                                        :GFCTB0B2.CSERVC-TARIF
                                    AND (A.CSIT_SOLTC_FLEXZ IN  (05, 08)
                                     OR  (A.CSIT_SOLTC_FLEXZ    = 02
                                    AND   (A.DINIC_FLEXZ_AGPTO <=
                                        :GFCTB0B2.DINIC-FLEXZ-AGPTO))))
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE '0010'              TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE ' - GFCTB0B2'       TO WRK-NOME-TAB
               MOVE 'GFCT0884'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'            TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0050'              TO GFCT0M-LOCAL
               MOVE  SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE  9                  TO GFCTNM-ERRO
               MOVE  SQLCODE            TO GFCTNM-COD-SQL-ERRO
               MOVE  SPACES             TO GFCTNM-DESC-MSG-ERRO
               MOVE  0010               TO GFCTNM-COD-MSG-ERRO
               PERFORM 2110-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE  1                 TO GFCTNM-ERRO
               MOVE  ZEROS             TO GFCTNM-COD-SQL-ERRO
               MOVE  0010              TO GFCTNM-COD-MSG-ERRO
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTNM-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE DINIC-FLEXZ-AGPTO      OF GFCTB0B2
                                       TO WRK-DATA-AUX.
           MOVE WRK-DIA-AUX            TO WRK-DIA-INV.
           MOVE WRK-MES-AUX            TO WRK-MES-INV.
           MOVE WRK-ANO-AUX            TO WRK-ANO-INV.
           MOVE WRK-DATA-INV           TO WRK-DATA-AUX1.
           MOVE DPROCM-ATUAL           TO WRK-DATA-AUX.
           MOVE WRK-DIA-AUX            TO WRK-DIA-INV.
           MOVE WRK-MES-AUX            TO WRK-MES-INV.
           MOVE WRK-ANO-AUX            TO WRK-ANO-INV.
           MOVE WRK-DATA-INV           TO WRK-DATA-AUX2.

           IF  WRK-DATA-AUX1           GREATER WRK-DATA-AUX2
                                       AND CSIT-SOLTC-FLEXZ NOT = 05
               MOVE 1                  TO GFCTNM-ERRO
               MOVE ZEROS              TO GFCTNM-COD-SQL-ERRO
               MOVE 0659               TO GFCTNM-COD-MSG-ERRO
               MOVE '0659'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               STRING GFCTG3-DESC-MSG ' - GFCTB0B2'
                   DELIMITED BY '  '   INTO GFCTNM-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           PERFORM 2120-PROCESSAR-TPO-AGPTO-PEN.

      *----------------------------------------------------------------*
       2054-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2055-SELECIONAR-SEGMENTO-PEN    SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTNL-AGRUPMTO-PED TO CAGPTO-CTA         OF GFCTB0C0.
           MOVE GFCTNL-TARIFA-PED   TO CSERVC-TARIF       OF GFCTB0C0.
           MOVE GFCTNL-SEGMENTO     TO CSGMTO-GSTAO-TARIF OF GFCTB0C0.

           EXEC SQL
               SELECT
                 A.CAGPTO_CTA,
                 A.CSERVC_TARIF,
                 A.HIDTFD_FLEXZ_AGPTO,
                 A.CFUNC_SOLTC_FLEXZ,
                 A.CFUNC_PAREC_FLEXZ,
                 A.CTPO_OPER_MOTVO,
                 A.CMOTVO_JUSTF,
                 A.CEMPR_ORIGE_FLEXZ,
                 A.CDEPDC_ORIGE_FLEXZ,
                 A.CEMPR_PAREC_FLEXZ,
                 A.CDEPDC_PAREC_FLEXZ,
                 A.DINIC_FLEXZ_AGPTO,
                 A.DFIM_FLEXZ_AGPTO,
                 A.QFRANQ_FLEXZ_AGPTO,
                 A.CPER_FRANQ_AGPTO,
                 A.VNEGOC_FLEXZ_AGPTO,
                 A.PDESC_FLEXZ_AGPTO,
                 A.DDIA_AGNDA,
                 A.QDIA_FLEXZ_AGPTO,
                 A.HSOLTC_FLEXZ,
                 A.RJUSTF_SOLTC_FLEXZ,
                 A.HPAREC_FLEXZ,
                 A.RJUSTF_PAREC_FLEXZ,
                 A.CSIT_SOLTC_FLEXZ,
090708           A.CINDCD_AGPTO_TOT,
090708           A.CPER_COBR_TARIF,
090708           A.CINIC_PER_COBR
               INTO
                   :GFCTB0B2.CAGPTO-CTA,
                   :GFCTB0B2.CSERVC-TARIF,
                   :GFCTB0B2.HIDTFD-FLEXZ-AGPTO,
                   :GFCTB0B2.CFUNC-SOLTC-FLEXZ,
                   :GFCTB0B2.CFUNC-PAREC-FLEXZ,
                   :GFCTB0B2.CTPO-OPER-MOTVO,
                   :GFCTB0B2.CMOTVO-JUSTF,
                   :GFCTB0B2.CEMPR-ORIGE-FLEXZ,
                   :GFCTB0B2.CDEPDC-ORIGE-FLEXZ,
                   :GFCTB0B2.CEMPR-PAREC-FLEXZ,
                   :GFCTB0B2.CDEPDC-PAREC-FLEXZ,
                   :GFCTB0B2.DINIC-FLEXZ-AGPTO,
                   :GFCTB0B2.DFIM-FLEXZ-AGPTO,
                   :GFCTB0B2.QFRANQ-FLEXZ-AGPTO,
                   :GFCTB0B2.CPER-FRANQ-AGPTO,
                   :GFCTB0B2.VNEGOC-FLEXZ-AGPTO,
                   :GFCTB0B2.PDESC-FLEXZ-AGPTO,
                   :GFCTB0B2.DDIA-AGNDA,
                   :GFCTB0B2.QDIA-FLEXZ-AGPTO,
                   :GFCTB0B2.HSOLTC-FLEXZ,
                   :GFCTB0B2.RJUSTF-SOLTC-FLEXZ,
                   :GFCTB0B2.HPAREC-FLEXZ,
                   :GFCTB0B2.RJUSTF-PAREC-FLEXZ,
                   :GFCTB0B2.CSIT-SOLTC-FLEXZ,
090708             :GFCTB0B2.CINDCD-AGPTO-TOT,
090708             :GFCTB0B2.CPER-COBR-TARIF,
090708             :GFCTB0B2.CINIC-PER-COBR
             FROM   DB2PRD.PDIDO_FLEXZ_AGPTO A,
                    DB2PRD.PDIDO_FLEXZ_SGMTO B
             WHERE
              A.CAGPTO_CTA           = B.CAGPTO_CTA                  AND
              A.CSERVC_TARIF         = B.CSERVC_TARIF                AND
              A.HIDTFD_FLEXZ_AGPTO   = B.HIDTFD_FLEXZ_AGPTO          AND
              A.CSERVC_TARIF         = :GFCTB0B2.CSERVC-TARIF        AND
              B.CAGPTO_CTA           = :GFCTB0C0.CAGPTO-CTA          AND
              B.CSERVC_TARIF         = :GFCTB0C0.CSERVC-TARIF        AND
              B.CSGMTO_GSTAO_TARIF   = :GFCTB0C0.CSGMTO-GSTAO-TARIF  AND
             (A.CSIT_SOLTC_FLEXZ     IN  (05, 08)                    OR
              (A.CSIT_SOLTC_FLEXZ    = 02                            AND
              (A.DINIC_FLEXZ_AGPTO <= :GFCTB0B2.DINIC-FLEXZ-AGPTO))) AND
              A.HIDTFD_FLEXZ_AGPTO  IN
                                 (SELECT MAX (A.HIDTFD_FLEXZ_AGPTO)
                                  FROM   DB2PRD.PDIDO_FLEXZ_AGPTO A,
                                         DB2PRD.PDIDO_FLEXZ_SGMTO B
                                  WHERE A.CAGPTO_CTA            =
                                        B.CAGPTO_CTA
                                    AND A.CSERVC_TARIF          =
                                        B.CSERVC_TARIF
                                    AND A.HIDTFD_FLEXZ_AGPTO    =
                                        B.HIDTFD_FLEXZ_AGPTO
                                    AND B.CAGPTO_CTA            =
                                        :GFCTB0C0.CAGPTO-CTA
                                    AND B.CSERVC_TARIF          =
                                        :GFCTB0C0.CSERVC-TARIF
                                    AND B.CSGMTO_GSTAO_TARIF    =
                                        :GFCTB0C0.CSGMTO-GSTAO-TARIF
                                    AND  A.CAGPTO_CTA           =
                                        :GFCTB0B2.CAGPTO-CTA
                                    AND  A.CSERVC_TARIF         =
                                        :GFCTB0B2.CSERVC-TARIF
                                    AND (A.CSIT_SOLTC_FLEXZ IN  (05, 08)
                                     OR  (A.CSIT_SOLTC_FLEXZ    = 02
                                    AND   (A.DINIC_FLEXZ_AGPTO <=
                                        :GFCTB0B2.DINIC-FLEXZ-AGPTO))))
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE '0010'              TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE ' - GFCTB0B2'       TO WRK-NOME-TAB
               MOVE 'GFCT0884'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'            TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0060'              TO GFCT0M-LOCAL
               MOVE  SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE  9                  TO GFCTNM-ERRO
               MOVE  SQLCODE            TO GFCTNM-COD-SQL-ERRO
               MOVE  SPACES             TO GFCTNM-DESC-MSG-ERRO
               MOVE  0010               TO GFCTNM-COD-MSG-ERRO
               PERFORM 2110-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE  1                 TO GFCTNM-ERRO
               MOVE  ZEROS             TO GFCTNM-COD-SQL-ERRO
               MOVE  0010              TO GFCTNM-COD-MSG-ERRO
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTNM-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE DINIC-FLEXZ-AGPTO      OF GFCTB0B2
                                       TO WRK-DATA-AUX.
           MOVE WRK-DIA-AUX            TO WRK-DIA-INV.
           MOVE WRK-MES-AUX            TO WRK-MES-INV.
           MOVE WRK-ANO-AUX            TO WRK-ANO-INV.
           MOVE WRK-DATA-INV           TO WRK-DATA-AUX1.
           MOVE DPROCM-ATUAL           TO WRK-DATA-AUX.
           MOVE WRK-DIA-AUX            TO WRK-DIA-INV.
           MOVE WRK-MES-AUX            TO WRK-MES-INV.
           MOVE WRK-ANO-AUX            TO WRK-ANO-INV.
           MOVE WRK-DATA-INV           TO WRK-DATA-AUX2.

           IF  WRK-DATA-AUX1           GREATER WRK-DATA-AUX2
                                       AND CSIT-SOLTC-FLEXZ NOT = 05
               MOVE 1                  TO GFCTNM-ERRO
               MOVE ZEROS              TO GFCTNM-COD-SQL-ERRO
               MOVE 0659               TO GFCTNM-COD-MSG-ERRO
               MOVE '0659'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               STRING GFCTG3-DESC-MSG ' - GFCTB0B2'
                   DELIMITED BY '  '   INTO GFCTNM-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           PERFORM 2120-PROCESSAR-TPO-AGPTO-PEN.

      *----------------------------------------------------------------*
       2055-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2056-SELECIONAR-MUNICIPIO-PEN   SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTNL-AGRUPMTO-PED    TO CAGPTO-CTA        OF GFCTB0B7.
           MOVE GFCTNL-TARIFA-PED      TO CSERVC-TARIF      OF GFCTB0B7.
           MOVE GFCTNL-MUNICIPIO       TO CMUN-IBGE         OF GFCTB0B7.

           EXEC SQL
               SELECT
                 A.CAGPTO_CTA,
                 A.CSERVC_TARIF,
                 A.HIDTFD_FLEXZ_AGPTO,
                 A.CFUNC_SOLTC_FLEXZ,
                 A.CFUNC_PAREC_FLEXZ,
                 A.CTPO_OPER_MOTVO,
                 A.CMOTVO_JUSTF,
                 A.CEMPR_ORIGE_FLEXZ,
                 A.CDEPDC_ORIGE_FLEXZ,
                 A.CEMPR_PAREC_FLEXZ,
                 A.CDEPDC_PAREC_FLEXZ,
                 A.DINIC_FLEXZ_AGPTO,
                 A.DFIM_FLEXZ_AGPTO,
                 A.QFRANQ_FLEXZ_AGPTO,
                 A.CPER_FRANQ_AGPTO,
                 A.VNEGOC_FLEXZ_AGPTO,
                 A.PDESC_FLEXZ_AGPTO,
                 A.DDIA_AGNDA,
                 A.QDIA_FLEXZ_AGPTO,
                 A.HSOLTC_FLEXZ,
                 A.RJUSTF_SOLTC_FLEXZ,
                 A.HPAREC_FLEXZ,
                 A.RJUSTF_PAREC_FLEXZ,
                 A.CSIT_SOLTC_FLEXZ,
090708           A.CINDCD_AGPTO_TOT,
090708           A.CPER_COBR_TARIF,
090708           A.CINIC_PER_COBR
               INTO
                   :GFCTB0B2.CAGPTO-CTA,
                   :GFCTB0B2.CSERVC-TARIF,
                   :GFCTB0B2.HIDTFD-FLEXZ-AGPTO,
                   :GFCTB0B2.CFUNC-SOLTC-FLEXZ,
                   :GFCTB0B2.CFUNC-PAREC-FLEXZ,
                   :GFCTB0B2.CTPO-OPER-MOTVO,
                   :GFCTB0B2.CMOTVO-JUSTF,
                   :GFCTB0B2.CEMPR-ORIGE-FLEXZ,
                   :GFCTB0B2.CDEPDC-ORIGE-FLEXZ,
                   :GFCTB0B2.CEMPR-PAREC-FLEXZ,
                   :GFCTB0B2.CDEPDC-PAREC-FLEXZ,
                   :GFCTB0B2.DINIC-FLEXZ-AGPTO,
                   :GFCTB0B2.DFIM-FLEXZ-AGPTO,
                   :GFCTB0B2.QFRANQ-FLEXZ-AGPTO,
                   :GFCTB0B2.CPER-FRANQ-AGPTO,
                   :GFCTB0B2.VNEGOC-FLEXZ-AGPTO,
                   :GFCTB0B2.PDESC-FLEXZ-AGPTO,
                   :GFCTB0B2.DDIA-AGNDA,
                   :GFCTB0B2.QDIA-FLEXZ-AGPTO,
                   :GFCTB0B2.HSOLTC-FLEXZ,
                   :GFCTB0B2.RJUSTF-SOLTC-FLEXZ,
                   :GFCTB0B2.HPAREC-FLEXZ,
                   :GFCTB0B2.RJUSTF-PAREC-FLEXZ,
                   :GFCTB0B2.CSIT-SOLTC-FLEXZ,
090708             :GFCTB0B2.CINDCD-AGPTO-TOT,
090708             :GFCTB0B2.CPER-COBR-TARIF,
090708             :GFCTB0B2.CINIC-PER-COBR
             FROM   DB2PRD.PDIDO_FLEXZ_AGPTO A,
                    DB2PRD.PDIDO_FLEXZ_MUN   B
             WHERE
              A.CAGPTO_CTA           = B.CAGPTO_CTA                  AND
              A.CSERVC_TARIF         = B.CSERVC_TARIF                AND
              A.HIDTFD_FLEXZ_AGPTO   = B.HIDTFD_FLEXZ_AGPTO          AND
              A.CSERVC_TARIF         = :GFCTB0B2.CSERVC-TARIF        AND
              B.CAGPTO_CTA           = :GFCTB0B7.CAGPTO-CTA          AND
              B.CSERVC_TARIF         = :GFCTB0B7.CSERVC-TARIF        AND
              B.CMUN_IBGE            = :GFCTB0B7.CMUN-IBGE           AND
             (A.CSIT_SOLTC_FLEXZ     IN  (05, 08)                    OR
              (A.CSIT_SOLTC_FLEXZ    = 02                            AND
              (A.DINIC_FLEXZ_AGPTO <= :GFCTB0B2.DINIC-FLEXZ-AGPTO))) AND
              A.HIDTFD_FLEXZ_AGPTO  IN
                                 (SELECT MAX (A.HIDTFD_FLEXZ_AGPTO)
                                  FROM   DB2PRD.PDIDO_FLEXZ_AGPTO A,
                                         DB2PRD.PDIDO_FLEXZ_MUN   B
                                  WHERE A.CAGPTO_CTA            =
                                        B.CAGPTO_CTA
                                    AND A.CSERVC_TARIF          =
                                        B.CSERVC_TARIF
                                    AND A.HIDTFD_FLEXZ_AGPTO    =
                                        B.HIDTFD_FLEXZ_AGPTO
                                    AND B.CAGPTO_CTA            =
                                        :GFCTB0B7.CAGPTO-CTA
                                    AND B.CSERVC_TARIF          =
                                        :GFCTB0B7.CSERVC-TARIF
                                    AND B.CMUN_IBGE             =
                                        :GFCTB0B7.CMUN-IBGE
                                    AND  A.CAGPTO_CTA           =
                                        :GFCTB0B2.CAGPTO-CTA
                                    AND  A.CSERVC_TARIF         =
                                        :GFCTB0B2.CSERVC-TARIF
                                    AND (A.CSIT_SOLTC_FLEXZ IN  (05, 08)
                                     OR  (A.CSIT_SOLTC_FLEXZ    = 02
                                    AND   (A.DINIC_FLEXZ_AGPTO <=
                                        :GFCTB0B2.DINIC-FLEXZ-AGPTO))))
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE '0010'              TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE ' - GFCTB0B2'       TO WRK-NOME-TAB
               MOVE 'GFCT0884'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'            TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0070'              TO GFCT0M-LOCAL
               MOVE  SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE  9                  TO GFCTNM-ERRO
               MOVE  SQLCODE            TO GFCTNM-COD-SQL-ERRO
               MOVE  SPACES             TO GFCTNM-DESC-MSG-ERRO
               MOVE  0010               TO GFCTNM-COD-MSG-ERRO
               PERFORM 2110-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE  1                 TO GFCTNM-ERRO
               MOVE  ZEROS             TO GFCTNM-COD-SQL-ERRO
               MOVE  0010              TO GFCTNM-COD-MSG-ERRO
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTNM-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE DINIC-FLEXZ-AGPTO      OF GFCTB0B2
                                       TO WRK-DATA-AUX.
           MOVE WRK-DIA-AUX            TO WRK-DIA-INV.
           MOVE WRK-MES-AUX            TO WRK-MES-INV.
           MOVE WRK-ANO-AUX            TO WRK-ANO-INV.
           MOVE WRK-DATA-INV           TO WRK-DATA-AUX1.
           MOVE DPROCM-ATUAL           TO WRK-DATA-AUX.
           MOVE WRK-DIA-AUX            TO WRK-DIA-INV.
           MOVE WRK-MES-AUX            TO WRK-MES-INV.
           MOVE WRK-ANO-AUX            TO WRK-ANO-INV.
           MOVE WRK-DATA-INV           TO WRK-DATA-AUX2.

           IF  WRK-DATA-AUX1           GREATER WRK-DATA-AUX2
                                       AND CSIT-SOLTC-FLEXZ NOT = 05
               MOVE 1                  TO GFCTNM-ERRO
               MOVE ZEROS              TO GFCTNM-COD-SQL-ERRO
               MOVE 0659               TO GFCTNM-COD-MSG-ERRO
               MOVE '0659'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               STRING GFCTG3-DESC-MSG ' - GFCTB0B2'
                   DELIMITED BY '  '   INTO GFCTNM-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           PERFORM 2120-PROCESSAR-TPO-AGPTO-PEN.

      *----------------------------------------------------------------*
       2056-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2057-SELECIONAR-UF-PEN  SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTNL-AGRUPMTO-PED    TO CAGPTO-CTA        OF GFCTB0C1.
           MOVE GFCTNL-TARIFA-PED      TO CSERVC-TARIF      OF GFCTB0C1.
           MOVE GFCTNL-UF              TO CSGL-UF           OF GFCTB0C1.

           EXEC SQL
               SELECT
                 A.CAGPTO_CTA,
                 A.CSERVC_TARIF,
                 A.HIDTFD_FLEXZ_AGPTO,
                 A.CFUNC_SOLTC_FLEXZ,
                 A.CFUNC_PAREC_FLEXZ,
                 A.CTPO_OPER_MOTVO,
                 A.CMOTVO_JUSTF,
                 A.CEMPR_ORIGE_FLEXZ,
                 A.CDEPDC_ORIGE_FLEXZ,
                 A.CEMPR_PAREC_FLEXZ,
                 A.CDEPDC_PAREC_FLEXZ,
                 A.DINIC_FLEXZ_AGPTO,
                 A.DFIM_FLEXZ_AGPTO,
                 A.QFRANQ_FLEXZ_AGPTO,
                 A.CPER_FRANQ_AGPTO,
                 A.VNEGOC_FLEXZ_AGPTO,
                 A.PDESC_FLEXZ_AGPTO,
                 A.DDIA_AGNDA,
                 A.QDIA_FLEXZ_AGPTO,
                 A.HSOLTC_FLEXZ,
                 A.RJUSTF_SOLTC_FLEXZ,
                 A.HPAREC_FLEXZ,
                 A.RJUSTF_PAREC_FLEXZ,
                 A.CSIT_SOLTC_FLEXZ,
090708           A.CINDCD_AGPTO_TOT,
090708           A.CPER_COBR_TARIF,
090708           A.CINIC_PER_COBR
               INTO
                   :GFCTB0B2.CAGPTO-CTA,
                   :GFCTB0B2.CSERVC-TARIF,
                   :GFCTB0B2.HIDTFD-FLEXZ-AGPTO,
                   :GFCTB0B2.CFUNC-SOLTC-FLEXZ,
                   :GFCTB0B2.CFUNC-PAREC-FLEXZ,
                   :GFCTB0B2.CTPO-OPER-MOTVO,
                   :GFCTB0B2.CMOTVO-JUSTF,
                   :GFCTB0B2.CEMPR-ORIGE-FLEXZ,
                   :GFCTB0B2.CDEPDC-ORIGE-FLEXZ,
                   :GFCTB0B2.CEMPR-PAREC-FLEXZ,
                   :GFCTB0B2.CDEPDC-PAREC-FLEXZ,
                   :GFCTB0B2.DINIC-FLEXZ-AGPTO,
                   :GFCTB0B2.DFIM-FLEXZ-AGPTO,
                   :GFCTB0B2.QFRANQ-FLEXZ-AGPTO,
                   :GFCTB0B2.CPER-FRANQ-AGPTO,
                   :GFCTB0B2.VNEGOC-FLEXZ-AGPTO,
                   :GFCTB0B2.PDESC-FLEXZ-AGPTO,
                   :GFCTB0B2.DDIA-AGNDA,
                   :GFCTB0B2.QDIA-FLEXZ-AGPTO,
                   :GFCTB0B2.HSOLTC-FLEXZ,
                   :GFCTB0B2.RJUSTF-SOLTC-FLEXZ,
                   :GFCTB0B2.HPAREC-FLEXZ,
                   :GFCTB0B2.RJUSTF-PAREC-FLEXZ,
                   :GFCTB0B2.CSIT-SOLTC-FLEXZ,
090708             :GFCTB0B2.CINDCD-AGPTO-TOT,
090708             :GFCTB0B2.CPER-COBR-TARIF,
090708             :GFCTB0B2.CINIC-PER-COBR
             FROM   DB2PRD.PDIDO_FLEXZ_AGPTO A,
                    DB2PRD.PDIDO_FLEXZ_UF    B
             WHERE
              A.CAGPTO_CTA           = B.CAGPTO_CTA                  AND
              A.CSERVC_TARIF         = B.CSERVC_TARIF                AND
              A.HIDTFD_FLEXZ_AGPTO   = B.HIDTFD_FLEXZ_AGPTO          AND
              A.CSERVC_TARIF         = :GFCTB0B2.CSERVC-TARIF        AND
              B.CAGPTO_CTA           = :GFCTB0C1.CAGPTO-CTA          AND
              B.CSERVC_TARIF         = :GFCTB0C1.CSERVC-TARIF        AND
              B.CSGL_UF              = :GFCTB0C1.CSGL-UF             AND
             (A.CSIT_SOLTC_FLEXZ     IN  (05, 08)                    OR
              (A.CSIT_SOLTC_FLEXZ    = 02                            AND
              (A.DINIC_FLEXZ_AGPTO <= :GFCTB0B2.DINIC-FLEXZ-AGPTO))) AND
              A.HIDTFD_FLEXZ_AGPTO  IN
                                 (SELECT MAX (A.HIDTFD_FLEXZ_AGPTO)
                                  FROM   DB2PRD.PDIDO_FLEXZ_AGPTO A,
                                         DB2PRD.PDIDO_FLEXZ_UF    B
                                  WHERE A.CAGPTO_CTA            =
                                        B.CAGPTO_CTA
                                    AND A.CSERVC_TARIF          =
                                        B.CSERVC_TARIF
                                    AND A.HIDTFD_FLEXZ_AGPTO    =
                                        B.HIDTFD_FLEXZ_AGPTO
                                    AND B.CAGPTO_CTA            =
                                        :GFCTB0C1.CAGPTO-CTA
                                    AND B.CSERVC_TARIF          =
                                        :GFCTB0C1.CSERVC-TARIF
                                    AND B.CSGL_UF               =
                                        :GFCTB0C1.CSGL-UF
                                    AND  A.CAGPTO_CTA           =
                                        :GFCTB0B2.CAGPTO-CTA
                                    AND  A.CSERVC_TARIF         =
                                        :GFCTB0B2.CSERVC-TARIF
                                    AND (A.CSIT_SOLTC_FLEXZ IN  (05, 08)
                                     OR  (A.CSIT_SOLTC_FLEXZ    = 02
                                    AND   (A.DINIC_FLEXZ_AGPTO <=
                                        :GFCTB0B2.DINIC-FLEXZ-AGPTO))))
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE '0010'              TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE ' - GFCTB0B2'       TO WRK-NOME-TAB
               MOVE 'GFCT0884'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'            TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0080'              TO GFCT0M-LOCAL
               MOVE  SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE  9                  TO GFCTNM-ERRO
               MOVE  SQLCODE            TO GFCTNM-COD-SQL-ERRO
               MOVE  SPACES             TO GFCTNM-DESC-MSG-ERRO
               MOVE  0010               TO GFCTNM-COD-MSG-ERRO
               PERFORM 2110-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE  1                 TO GFCTNM-ERRO
               MOVE  ZEROS             TO GFCTNM-COD-SQL-ERRO
               MOVE  0010              TO GFCTNM-COD-MSG-ERRO
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTNM-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE DINIC-FLEXZ-AGPTO      OF GFCTB0B2
                                       TO WRK-DATA-AUX.
           MOVE WRK-DIA-AUX            TO WRK-DIA-INV.
           MOVE WRK-MES-AUX            TO WRK-MES-INV.
           MOVE WRK-ANO-AUX            TO WRK-ANO-INV.
           MOVE WRK-DATA-INV           TO WRK-DATA-AUX1.
           MOVE DPROCM-ATUAL           TO WRK-DATA-AUX.
           MOVE WRK-DIA-AUX            TO WRK-DIA-INV.
           MOVE WRK-MES-AUX            TO WRK-MES-INV.
           MOVE WRK-ANO-AUX            TO WRK-ANO-INV.
           MOVE WRK-DATA-INV           TO WRK-DATA-AUX2.

           IF  WRK-DATA-AUX1           GREATER WRK-DATA-AUX2
                                       AND CSIT-SOLTC-FLEXZ NOT = 05
               MOVE 1                  TO GFCTNM-ERRO
               MOVE ZEROS              TO GFCTNM-COD-SQL-ERRO
               MOVE 0659               TO GFCTNM-COD-MSG-ERRO
               MOVE '0659'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               STRING GFCTG3-DESC-MSG ' - GFCTB0B2'
                   DELIMITED BY '  '   INTO GFCTNM-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           PERFORM 2120-PROCESSAR-TPO-AGPTO-PEN.

      *----------------------------------------------------------------*
       2057-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

090708*----------------------------------------------------------------*
090708 2058-SELECIONAR-PERSONALIZ      SECTION.
090708*----------------------------------------------------------------*
090708
090708     MOVE GFCTNL-AGRUPMTO-PED    TO CAGPTO-CTA        OF GFCTB0B2.
090708     MOVE GFCTNL-TARIFA-PED      TO CSERVC-TARIF      OF GFCTB0B2.
090708
090708     EXEC SQL
090708         SELECT
090708           CAGPTO_CTA,
090708           CSERVC_TARIF,
090708           HIDTFD_FLEXZ_AGPTO,
090708           CFUNC_SOLTC_FLEXZ,
090708           CFUNC_PAREC_FLEXZ,
090708           CTPO_OPER_MOTVO,
090708           CMOTVO_JUSTF,
090708           CEMPR_ORIGE_FLEXZ,
090708           CDEPDC_ORIGE_FLEXZ,
090708           CEMPR_PAREC_FLEXZ,
090708           CDEPDC_PAREC_FLEXZ,
090708           DINIC_FLEXZ_AGPTO,
090708           DFIM_FLEXZ_AGPTO,
090708           QFRANQ_FLEXZ_AGPTO,
090708           CPER_FRANQ_AGPTO,
090708           VNEGOC_FLEXZ_AGPTO,
090708           PDESC_FLEXZ_AGPTO,
090708           DDIA_AGNDA,
090708           QDIA_FLEXZ_AGPTO,
090708           HSOLTC_FLEXZ,
090708           RJUSTF_SOLTC_FLEXZ,
090708           HPAREC_FLEXZ,
090708           RJUSTF_PAREC_FLEXZ,
090708           CSIT_SOLTC_FLEXZ,
090708           CINDCD_AGPTO_TOT,
090708           CPER_COBR_TARIF,
090708           CINIC_PER_COBR
090708         INTO
090708             :GFCTB0B2.CAGPTO-CTA,
090708             :GFCTB0B2.CSERVC-TARIF,
090708             :GFCTB0B2.HIDTFD-FLEXZ-AGPTO,
090708             :GFCTB0B2.CFUNC-SOLTC-FLEXZ,
090708             :GFCTB0B2.CFUNC-PAREC-FLEXZ,
090708             :GFCTB0B2.CTPO-OPER-MOTVO,
090708             :GFCTB0B2.CMOTVO-JUSTF,
090708             :GFCTB0B2.CEMPR-ORIGE-FLEXZ,
090708             :GFCTB0B2.CDEPDC-ORIGE-FLEXZ,
090708             :GFCTB0B2.CEMPR-PAREC-FLEXZ,
090708             :GFCTB0B2.CDEPDC-PAREC-FLEXZ,
090708             :GFCTB0B2.DINIC-FLEXZ-AGPTO,
090708             :GFCTB0B2.DFIM-FLEXZ-AGPTO,
090708             :GFCTB0B2.QFRANQ-FLEXZ-AGPTO,
090708             :GFCTB0B2.CPER-FRANQ-AGPTO,
090708             :GFCTB0B2.VNEGOC-FLEXZ-AGPTO,
090708             :GFCTB0B2.PDESC-FLEXZ-AGPTO,
090708             :GFCTB0B2.DDIA-AGNDA,
090708             :GFCTB0B2.QDIA-FLEXZ-AGPTO,
090708             :GFCTB0B2.HSOLTC-FLEXZ,
090708             :GFCTB0B2.RJUSTF-SOLTC-FLEXZ,
090708             :GFCTB0B2.HPAREC-FLEXZ,
090708             :GFCTB0B2.RJUSTF-PAREC-FLEXZ,
090708             :GFCTB0B2.CSIT-SOLTC-FLEXZ,
090708             :GFCTB0B2.CINDCD-AGPTO-TOT,
090708             :GFCTB0B2.CPER-COBR-TARIF,
090708             :GFCTB0B2.CINIC-PER-COBR
090708       FROM   DB2PRD.PDIDO_FLEXZ_AGPTO
090708       WHERE
090708          CSERVC_TARIF         = :GFCTB0B2.CSERVC-TARIF        AND
090708          CAGPTO_CTA           = :GFCTB0B2.CAGPTO-CTA          AND
090708          CSERVC_TARIF         = :GFCTB0B2.CSERVC-TARIF        AND
090708         (CSIT_SOLTC_FLEXZ     IN  (05, 08)                    OR
090708          (CSIT_SOLTC_FLEXZ    = 02                            AND
090708          (DINIC_FLEXZ_AGPTO <= :GFCTB0B2.DINIC-FLEXZ-AGPTO))) AND
090708          HIDTFD_FLEXZ_AGPTO  IN
090708                           (SELECT MAX (HIDTFD_FLEXZ_AGPTO)
090708                            FROM   DB2PRD.PDIDO_FLEXZ_AGPTO
090708                            WHERE   CAGPTO_CTA            =
090708                                  :GFCTB0B2.CAGPTO-CTA
090708                              AND   CSERVC_TARIF          =
090708                                  :GFCTB0B2.CSERVC-TARIF
090708                              AND   (CSIT_SOLTC_FLEXZ IN  (05, 08)
090708                               OR    (CSIT_SOLTC_FLEXZ    = 02
090708                              AND     (DINIC_FLEXZ_AGPTO <=
090708                                  :GFCTB0B2.DINIC-FLEXZ-AGPTO))))
090708     END-EXEC.
090708
090708     IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
090708        (SQLWARN0                 EQUAL 'W')
090708         MOVE '0010'              TO GFCTG2-COD-MSG
090708         PERFORM 1310-OBTER-DESC-MENSAGEM
090708         MOVE ' - GFCTB0B2'       TO WRK-NOME-TAB
090708         MOVE 'GFCT0884'          TO GFCT0M-PROGRAMA
090708         MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
090708         MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
090708         MOVE 'SELECT'            TO GFCT0M-COMANDO-SQL
090708         MOVE  SQLCODE            TO GFCT0M-SQLCODE
090708         MOVE '0080'              TO GFCT0M-LOCAL
090708         MOVE  SQLCA              TO GFCT0M-SQLCA-AREA
090708         MOVE  9                  TO GFCTNM-ERRO
090708         MOVE  SQLCODE            TO GFCTNM-COD-SQL-ERRO
090708         MOVE  SPACES             TO GFCTNM-DESC-MSG-ERRO
090708         MOVE  0010               TO GFCTNM-COD-MSG-ERRO
090708         PERFORM 2110-CONCATENAR-MENSAGEM
090708         PERFORM 3000-FINALIZAR
090708     END-IF.
090708
090708     IF  SQLCODE                 EQUAL +100
090708         MOVE  1                 TO GFCTNM-ERRO
090708         MOVE  ZEROS             TO GFCTNM-COD-SQL-ERRO
090708         MOVE  0010              TO GFCTNM-COD-MSG-ERRO
090708         MOVE '0010'             TO GFCTG2-COD-MSG
090708         PERFORM 1310-OBTER-DESC-MENSAGEM
090708         MOVE GFCTG3-DESC-MSG    TO GFCTNM-DESC-MSG-ERRO
090708         PERFORM 3000-FINALIZAR
090708     END-IF.
090708
090708     MOVE DINIC-FLEXZ-AGPTO      OF GFCTB0B2
090708                                 TO WRK-DATA-AUX.
090708     MOVE WRK-DIA-AUX            TO WRK-DIA-INV.
090708     MOVE WRK-MES-AUX            TO WRK-MES-INV.
090708     MOVE WRK-ANO-AUX            TO WRK-ANO-INV.
090708     MOVE WRK-DATA-INV           TO WRK-DATA-AUX1.
090708     MOVE DPROCM-ATUAL           TO WRK-DATA-AUX.
090708     MOVE WRK-DIA-AUX            TO WRK-DIA-INV.
090708     MOVE WRK-MES-AUX            TO WRK-MES-INV.
090708     MOVE WRK-ANO-AUX            TO WRK-ANO-INV.
090708     MOVE WRK-DATA-INV           TO WRK-DATA-AUX2.
090708
090708     IF  WRK-DATA-AUX1           GREATER WRK-DATA-AUX2
090708                                 AND CSIT-SOLTC-FLEXZ NOT = 05
090708         MOVE 1                  TO GFCTNM-ERRO
090708         MOVE ZEROS              TO GFCTNM-COD-SQL-ERRO
090708         MOVE 0659               TO GFCTNM-COD-MSG-ERRO
090708         MOVE '0659'             TO GFCTG2-COD-MSG
090708         PERFORM 1310-OBTER-DESC-MENSAGEM
090708         STRING GFCTG3-DESC-MSG ' - GFCTB0B2'
090708             DELIMITED BY '  '   INTO GFCTNM-DESC-MSG-ERRO
090708         PERFORM 3000-FINALIZAR
090708     END-IF.
090708
090708     PERFORM 2120-PROCESSAR-TPO-AGPTO-PEN.
090708
090708*----------------------------------------------------------------*
090708 2058-99-FIM.                    EXIT.
090708*----------------------------------------------------------------*

090708******************************************************************
090708*  OBTER NOME DO FUNCIONARIO ATRAVES DO MODULO SARH8000.         *
090708******************************************************************
090708*----------------------------------------------------------------*
090708 2059-OBTER-NOME-FUNC            SECTION.
090708*----------------------------------------------------------------*
090708
090708     MOVE 0086                   TO WRK-TAMANHO-ENT.
090708     MOVE 002                    TO WRK-CAMPO-ENT(1).
090708     MOVE 'SARH8000'             TO WRK-MODULO.
090708
090708     CALL WRK-MODULO             USING WRK-ENT
090708                                       WRK-SAI
090708                                       WRK-ERRO-AREA
090708                                       WRK-SQLCA-NOME.
090708
090708     IF  WRK-COD-RETORNO-SAI     EQUAL ZEROS
090708         MOVE WRK-REGISTRO-SAI(1:80)
090708                                 TO WRK-AREA-RET
090708         GO TO 2059-99-FIM
090708     END-IF.
090708
090708     IF  WRK-COD-RETORNO-SAI     EQUAL 10 OR 12
090708         MOVE ALL '*'            TO WRK-NOME-RET
090708     ELSE
090708         MOVE 'NAO ENCONTRADO'   TO WRK-NOME-RET
090708     END-IF.
090708
090708*----------------------------------------------------------------*
090708 2059-99-FIM.                    EXIT.
090708*----------------------------------------------------------------*

0310  ******************************************************************
0310  * SELECIONAR RAZAO                                               *
0310  ******************************************************************
0310  *----------------------------------------------------------------*
0310   2060-SELECIONAR-RAZAO-PEN       SECTION.
0310  *----------------------------------------------------------------*
0310
0610XS     MOVE GFCTNL-TARIFA-PED      TO CSERVC-TARIF     OF GFCTB0B2.
0310
0310       MOVE GFCTNL-AGRUPMTO-PED    TO CAGPTO-CTA       OF GFCTB0M7.
0310       MOVE GFCTNL-TARIFA-PED      TO CSERVC-TARIF     OF GFCTB0M7.
0310       MOVE GFCTNL-GRUPO-CTBIL     TO CGRP-CTBIL-ATUAL OF GFCTB0M7.
0310       MOVE GFCTNL-SUB-GRUPO-CTBIL TO CSGRP-CTBIL      OF GFCTB0M7.
0310
0310       PERFORM 2061-OBTER-MAX
0310
0310       EXEC SQL
0310           SELECT
0310             A.CAGPTO_CTA,
0310             A.CSERVC_TARIF,
0310             A.HIDTFD_FLEXZ_AGPTO,
0310             A.CFUNC_SOLTC_FLEXZ,
0310             A.CFUNC_PAREC_FLEXZ,
0310             A.CTPO_OPER_MOTVO,
0310             A.CMOTVO_JUSTF,
0310             A.CEMPR_ORIGE_FLEXZ,
0310             A.CDEPDC_ORIGE_FLEXZ,
0310             A.CEMPR_PAREC_FLEXZ,
0310             A.CDEPDC_PAREC_FLEXZ,
0310             A.DINIC_FLEXZ_AGPTO,
0310             A.DFIM_FLEXZ_AGPTO,
0310             A.QFRANQ_FLEXZ_AGPTO,
0310             A.CPER_FRANQ_AGPTO,
0310             A.VNEGOC_FLEXZ_AGPTO,
0310             A.PDESC_FLEXZ_AGPTO,
0310             A.DDIA_AGNDA,
0310             A.QDIA_FLEXZ_AGPTO,
0310             A.HSOLTC_FLEXZ,
0310             A.RJUSTF_SOLTC_FLEXZ,
0310             A.HPAREC_FLEXZ,
0310             A.RJUSTF_PAREC_FLEXZ,
0310             A.CSIT_SOLTC_FLEXZ,
0310             A.CINDCD_AGPTO_TOT,
0310             A.CPER_COBR_TARIF,
0310             A.CINIC_PER_COBR
0310           INTO
0310               :GFCTB0B2.CAGPTO-CTA,
0310               :GFCTB0B2.CSERVC-TARIF,
0310               :GFCTB0B2.HIDTFD-FLEXZ-AGPTO,
0310               :GFCTB0B2.CFUNC-SOLTC-FLEXZ,
0310               :GFCTB0B2.CFUNC-PAREC-FLEXZ,
0310               :GFCTB0B2.CTPO-OPER-MOTVO,
0310               :GFCTB0B2.CMOTVO-JUSTF,
0310               :GFCTB0B2.CEMPR-ORIGE-FLEXZ,
0310               :GFCTB0B2.CDEPDC-ORIGE-FLEXZ,
0310               :GFCTB0B2.CEMPR-PAREC-FLEXZ,
0310               :GFCTB0B2.CDEPDC-PAREC-FLEXZ,
0310               :GFCTB0B2.DINIC-FLEXZ-AGPTO,
0310               :GFCTB0B2.DFIM-FLEXZ-AGPTO,
0310               :GFCTB0B2.QFRANQ-FLEXZ-AGPTO,
0310               :GFCTB0B2.CPER-FRANQ-AGPTO,
0310               :GFCTB0B2.VNEGOC-FLEXZ-AGPTO,
0310               :GFCTB0B2.PDESC-FLEXZ-AGPTO,
0310               :GFCTB0B2.DDIA-AGNDA,
0310               :GFCTB0B2.QDIA-FLEXZ-AGPTO,
0310               :GFCTB0B2.HSOLTC-FLEXZ,
0310               :GFCTB0B2.RJUSTF-SOLTC-FLEXZ,
0310               :GFCTB0B2.HPAREC-FLEXZ,
0310               :GFCTB0B2.RJUSTF-PAREC-FLEXZ,
0310               :GFCTB0B2.CSIT-SOLTC-FLEXZ,
0310               :GFCTB0B2.CINDCD-AGPTO-TOT,
0310               :GFCTB0B2.CPER-COBR-TARIF,
0310               :GFCTB0B2.CINIC-PER-COBR
0310         FROM   DB2PRD.PDIDO_FLEXZ_AGPTO A,
0310                DB2PRD.TPDIDO_FLEXZ_RZ   B
0310         WHERE
0310          A.CAGPTO_CTA           = B.CAGPTO_CTA                  AND
0310          A.CSERVC_TARIF         = B.CSERVC_TARIF                AND
0310          A.HIDTFD_FLEXZ_AGPTO   = B.HIDTFD_FLEXZ_AGPTO          AND
0310          A.CSERVC_TARIF         = :GFCTB0B2.CSERVC-TARIF        AND
0310          B.CAGPTO_CTA           = :GFCTB0M7.CAGPTO-CTA          AND
0310          B.CSERVC_TARIF         = :GFCTB0M7.CSERVC-TARIF        AND
0310          B.CGRP_CTBIL_ATUAL     = :GFCTB0M7.CGRP-CTBIL-ATUAL    AND
0310          B.CSGRP_CTBIL          = :GFCTB0M7.CSGRP-CTBIL         AND
0310          A.HIDTFD_FLEXZ_AGPTO   = :GFCTB0B2.HIDTFD-FLEXZ-AGPTO
0310       END-EXEC.
0310
0310       IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
0310          (SQLWARN0                 EQUAL 'W')
0310           MOVE '0010'              TO GFCTG2-COD-MSG
0310           PERFORM 1310-OBTER-DESC-MENSAGEM
0310           MOVE 'GFCTB0M7'           TO WRK-NOME-TAB
0310           MOVE 'GFCT0884'           TO GFCT0M-PROGRAMA
0310           MOVE 'DB2'                TO GFCT0M-TIPO-ACESSO
0310           MOVE 'TPDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
0310           MOVE 'SELECT'             TO GFCT0M-COMANDO-SQL
0310           MOVE  SQLCODE             TO GFCT0M-SQLCODE
0310           MOVE '0040'               TO GFCT0M-LOCAL
0310           MOVE  SQLCA               TO GFCT0M-SQLCA-AREA
0310           MOVE  9                   TO GFCTNM-ERRO
0310           MOVE  SQLCODE             TO WRK-COD-SQL-ERRO
0310           MOVE  WRK-COD-SQL-ERRO-2 TO GFCTNM-COD-SQL-ERRO
0310           MOVE  SPACES              TO GFCTNM-DESC-MSG-ERRO
0310           MOVE  0010                TO GFCTNM-COD-MSG-ERRO
0310           PERFORM 2110-CONCATENAR-MENSAGEM
0310           PERFORM 3000-FINALIZAR
0310       END-IF.
0310
0310       IF  SQLCODE                 EQUAL +100
0310           MOVE  1                 TO GFCTNM-ERRO
0310           MOVE  ZEROS             TO GFCTNM-COD-SQL-ERRO
0310           MOVE  0010              TO GFCTNM-COD-MSG-ERRO
0310           MOVE '0010'             TO GFCTG2-COD-MSG
0310           PERFORM 1310-OBTER-DESC-MENSAGEM
0310           MOVE GFCTG3-DESC-MSG    TO GFCTNM-DESC-MSG-ERRO
0310           PERFORM 3000-FINALIZAR
0310       END-IF.
0310
0310       MOVE DINIC-FLEXZ-AGPTO      OF GFCTB0B2
0310                                   TO WRK-DATA-AUX.
0310       MOVE WRK-DIA-AUX            TO WRK-DIA-INV.
0310       MOVE WRK-MES-AUX            TO WRK-MES-INV.
0310       MOVE WRK-ANO-AUX            TO WRK-ANO-INV.
0310       MOVE WRK-DATA-INV           TO WRK-DATA-AUX1.
0310       MOVE DPROCM-ATUAL           TO WRK-DATA-AUX.
0310       MOVE WRK-DIA-AUX            TO WRK-DIA-INV.
0310       MOVE WRK-MES-AUX            TO WRK-MES-INV.
0310       MOVE WRK-ANO-AUX            TO WRK-ANO-INV.
0310       MOVE WRK-DATA-INV           TO WRK-DATA-AUX2.
0310
020610     IF CSIT-SOLTC-FLEXZ OF GFCTB0B2 EQUAL 02 AND
.             WRK-DATA-AUX1  NOT GREATER WRK-DATA-AUX2
.             PERFORM 2120-PROCESSAR-TPO-AGPTO-PEN
.          ELSE
.             IF CSIT-SOLTC-FLEXZ OF GFCTB0B2 EQUAL 02 OR 08
.                PERFORM 2120-PROCESSAR-TPO-AGPTO-PEN
.             ELSE
.                MOVE 1                  TO GFCTNM-ERRO
.                MOVE ZEROS              TO GFCTNM-COD-SQL-ERRO
.                MOVE 0659               TO GFCTNM-COD-MSG-ERRO
.                MOVE '0659'             TO GFCTG2-COD-MSG
.                PERFORM 1310-OBTER-DESC-MENSAGEM
.                STRING GFCTG3-DESC-MSG ' - GFCTB0B2'
.                DELIMITED BY '  '   INTO GFCTNM-DESC-MSG-ERRO
.                PERFORM 3000-FINALIZAR
.             END-IF
020610     END-IF.
0310  *----------------------------------------------------------------*
0310   2060-99-FIM.                    EXIT.
0310  *----------------------------------------------------------------*

0310  ******************************************************************
0310  * SELECIONAR MAIOR-TIMES-TAMP-DO REG.                            *
0310  ******************************************************************
0310  *----------------------------------------------------------------*
0310   2061-OBTER-MAX                  SECTION.
0310  *----------------------------------------------------------------*
0310
0310       EXEC SQL
0310           SELECT
0310             MAX (A.HIDTFD_FLEXZ_AGPTO)
0310           INTO
0310               :GFCTB0B2.HIDTFD-FLEXZ-AGPTO
0310         FROM   DB2PRD.PDIDO_FLEXZ_AGPTO A,
0310                DB2PRD.TPDIDO_FLEXZ_RZ   B
0310         WHERE
0310          A.CAGPTO_CTA           = B.CAGPTO_CTA                  AND
0310          A.CSERVC_TARIF         = B.CSERVC_TARIF                AND
0310          A.HIDTFD_FLEXZ_AGPTO   = B.HIDTFD_FLEXZ_AGPTO          AND
0310          A.CSERVC_TARIF         = :GFCTB0B2.CSERVC-TARIF        AND
0310          B.CAGPTO_CTA           = :GFCTB0M7.CAGPTO-CTA          AND
0310          B.CSERVC_TARIF         = :GFCTB0M7.CSERVC-TARIF        AND
0310          B.CGRP_CTBIL_ATUAL     = :GFCTB0M7.CGRP-CTBIL-ATUAL    AND
0310          B.CSGRP_CTBIL          = :GFCTB0M7.CSGRP-CTBIL
0310       END-EXEC.
0310
0310       IF (SQLCODE                  NOT EQUAL ZEROS AND -305) OR
0310          (SQLWARN0                 EQUAL 'W')
0310           MOVE '0010'              TO GFCTG2-COD-MSG
0310           PERFORM 1310-OBTER-DESC-MENSAGEM
0310           MOVE ' - GFCTB0B2'        TO WRK-NOME-TAB
0310           MOVE 'GFCT0884'           TO GFCT0M-PROGRAMA
0310           MOVE 'DB2'                TO GFCT0M-TIPO-ACESSO
0310           MOVE 'TPDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
0310           MOVE 'SELECT'             TO GFCT0M-COMANDO-SQL
0310           MOVE  SQLCODE             TO GFCT0M-SQLCODE
0310           MOVE '0040'               TO GFCT0M-LOCAL
0310           MOVE  SQLCA               TO GFCT0M-SQLCA-AREA
0310           MOVE  9                   TO GFCTNM-ERRO
0310           MOVE  SQLCODE             TO WRK-COD-SQL-ERRO
0310           MOVE  WRK-COD-SQL-ERRO-2  TO GFCTNM-COD-SQL-ERRO
0310           MOVE  SPACES              TO GFCTNM-DESC-MSG-ERRO
0310           MOVE  0010                TO GFCTNM-COD-MSG-ERRO
0310           PERFORM 2110-CONCATENAR-MENSAGEM
0310           PERFORM 3000-FINALIZAR
0310       END-IF.
0310
0310       IF  SQLCODE                 EQUAL -305
0310           MOVE '0001-01-01-01.01.01.000001'
0310                                   TO
0310                                     HIDTFD-FLEXZ-AGPTO OF GFCTB0B2
0310       END-IF.
0310
0310  *----------------------------------------------------------------*
0310   2061-99-FIM.                    EXIT.
0310  *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA LEITURA DA TABELA DE HISTORICO                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-LER-TAB-HISTORICO          SECTION.
      *----------------------------------------------------------------*

           MOVE  GFCTNL-AGRUPMTO-PED   TO CAGPTO-CTA       OF GFCTB0B2.
           MOVE  GFCTNL-TARIFA-PED     TO CSERVC-TARIF     OF GFCTB0B2.
           MOVE  GFCTNL-TIMESTAMP-PED  TO HIDTFD-FLEXZ-AGPTO
                                                           OF GFCTB0B2.
           MOVE  ZEROS                 TO RJUSTF-SOLTC-FLEXZ-LEN
                                                           OF GFCTB0B2
                                          RJUSTF-PAREC-FLEXZ-LEN
                                                           OF GFCTB0B2.
           MOVE  SPACES                TO RJUSTF-SOLTC-FLEXZ-TEXT
                                                           OF GFCTB0B2
                                          RJUSTF-PAREC-FLEXZ-TEXT
                                                           OF GFCTB0B2.

           EXEC SQL
               SELECT
                   CAGPTO_CTA,
                   CSERVC_TARIF,
                   HIDTFD_FLEXZ_AGPTO,
                   CFUNC_SOLTC_FLEXZ,
                   CFUNC_PAREC_FLEXZ,
                   CTPO_OPER_MOTVO,
                   CMOTVO_JUSTF,
                   CEMPR_ORIGE_FLEXZ,
                   CDEPDC_ORIGE_FLEXZ,
                   CEMPR_PAREC_FLEXZ,
                   CDEPDC_PAREC_FLEXZ,
                   DINIC_FLEXZ_AGPTO,
                   DFIM_FLEXZ_AGPTO,
                   QFRANQ_FLEXZ_AGPTO,
                   CPER_FRANQ_AGPTO,
                   VNEGOC_FLEXZ_AGPTO,
                   PDESC_FLEXZ_AGPTO,
                   DDIA_AGNDA,
                   QDIA_FLEXZ_AGPTO,
                   HSOLTC_FLEXZ,
                   RJUSTF_SOLTC_FLEXZ,
                   HPAREC_FLEXZ,
                   RJUSTF_PAREC_FLEXZ,
                   CSIT_SOLTC_FLEXZ,
090708             CINDCD_AGPTO_TOT,
090708             CPER_COBR_TARIF,
090708             CINIC_PER_COBR
               INTO
                   :GFCTB0B2.CAGPTO-CTA,
                   :GFCTB0B2.CSERVC-TARIF,
                   :GFCTB0B2.HIDTFD-FLEXZ-AGPTO,
                   :GFCTB0B2.CFUNC-SOLTC-FLEXZ,
                   :GFCTB0B2.CFUNC-PAREC-FLEXZ,
                   :GFCTB0B2.CTPO-OPER-MOTVO,
                   :GFCTB0B2.CMOTVO-JUSTF,
                   :GFCTB0B2.CEMPR-ORIGE-FLEXZ,
                   :GFCTB0B2.CDEPDC-ORIGE-FLEXZ,
                   :GFCTB0B2.CEMPR-PAREC-FLEXZ,
                   :GFCTB0B2.CDEPDC-PAREC-FLEXZ,
                   :GFCTB0B2.DINIC-FLEXZ-AGPTO,
                   :GFCTB0B2.DFIM-FLEXZ-AGPTO,
                   :GFCTB0B2.QFRANQ-FLEXZ-AGPTO,
                   :GFCTB0B2.CPER-FRANQ-AGPTO,
                   :GFCTB0B2.VNEGOC-FLEXZ-AGPTO,
                   :GFCTB0B2.PDESC-FLEXZ-AGPTO,
                   :GFCTB0B2.DDIA-AGNDA,
                   :GFCTB0B2.QDIA-FLEXZ-AGPTO,
                   :GFCTB0B2.HSOLTC-FLEXZ,
                   :GFCTB0B2.RJUSTF-SOLTC-FLEXZ,
                   :GFCTB0B2.HPAREC-FLEXZ,
                   :GFCTB0B2.RJUSTF-PAREC-FLEXZ,
                   :GFCTB0B2.CSIT-SOLTC-FLEXZ,
090708             :GFCTB0B2.CINDCD-AGPTO-TOT,
090708             :GFCTB0B2.CPER-COBR-TARIF,
090708             :GFCTB0B2.CINIC-PER-COBR
               FROM DB2PRD.PDIDO_FLEXZ_AGPTO
               WHERE
                   CAGPTO_CTA          = :GFCTB0B2.CAGPTO-CTA   AND
                   CSERVC_TARIF        = :GFCTB0B2.CSERVC-TARIF AND
                   HIDTFD_FLEXZ_AGPTO  = :GFCTB0B2.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE '0010'              TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE ' - GFCTB0B2'       TO WRK-NOME-TAB
               MOVE 'GFCT0884'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'            TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0090'              TO GFCT0M-LOCAL
               MOVE  SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE  9                  TO GFCTNM-ERRO
               MOVE  SQLCODE            TO GFCTNM-COD-SQL-ERRO
               MOVE  SPACES             TO GFCTNM-DESC-MSG-ERRO
               MOVE  0010               TO GFCTNM-COD-MSG-ERRO
               PERFORM 2110-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE  1                 TO GFCTNM-ERRO
               MOVE  ZEROS             TO GFCTNM-COD-SQL-ERRO
               MOVE  0010              TO GFCTNM-COD-MSG-ERRO
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTNM-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           PERFORM 2120-PROCESSAR-TPO-AGPTO-PEN.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA CONCATENAR MENSAGEM                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2110-CONCATENAR-MENSAGEM        SECTION.
      *----------------------------------------------------------------*

           STRING GFCTG3-DESC-MSG      WRK-NOME-TAB

           DELIMITED BY '  '           INTO GFCTNM-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA PROCESSAR TIPO DE AGRUPAMENTO-PED                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2120-PROCESSAR-TPO-AGPTO-PEN    SECTION.
      *----------------------------------------------------------------*

           EVALUATE GFCTNL-AGRUPMTO-PED
               WHEN 15
                   PERFORM 2121-SELECIONAR-PAB-PEN

               WHEN 14
                   IF  CINDCD-AGPTO-TOT    OF GFCTB0B2 NOT EQUAL 'S'
                       PERFORM 2122-SELECIONAR-AGENCIA-PEN
                   ELSE
                       MOVE 'TODAS'        TO GFCTNM-DESCR-AGPTO
                       MOVE  ZEROS         TO GFCTNM-AGENCIA
                   END-IF

               WHEN 17
                   PERFORM 2123-SELECIONAR-POSTAL-PEN

               WHEN 4
                   PERFORM 2124-SELECIONAR-CLIENTE-PEN

               WHEN 9
                   PERFORM 2125-SELECIONAR-SEGMENTO-PEN

               WHEN 18
                   PERFORM 2126-SELECIONAR-MUNICIPIO-PEN

               WHEN 19
                   PERFORM 2127-SELECIONAR-UF-PEN

0310           WHEN 20
0310               PERFORM 2123-SELECIONAR-POSTAL-PEN
0310
0310           WHEN 26
0310               PERFORM 2132-SELECIONAR-RAZAO-PEN

090708         WHEN OTHER
090708
090708             MOVE CAGPTO-CTA     OF GFCTB0B2
090708                                 TO CAGPTO-CTA OF GFCTB0F3
090708             PERFORM 2131-SELECIONAR-AGRUP

           END-EVALUATE.

           MOVE  DINIC-FLEXZ-AGPTO         OF GFCTB0B2
                                           TO GFCTNM-INIC-VIG-FLEX.
           MOVE  DFIM-FLEXZ-AGPTO          OF GFCTB0B2
                                           TO GFCTNM-FIM-VIG-FLEX.
           MOVE  QFRANQ-FLEXZ-AGPTO        OF GFCTB0B2
                                           TO GFCTNM-FRANQUIA.
           MOVE  CPER-FRANQ-AGPTO          OF GFCTB0B2
                                           TO GFCTNM-PERIODO.
           MOVE  VNEGOC-FLEXZ-AGPTO        OF GFCTB0B2
                                           TO GFCTNM-VR-NEGOCIADO.
           MOVE  PDESC-FLEXZ-AGPTO         OF GFCTB0B2
                                           TO GFCTNM-PERC-DESCTO.
           MOVE  DDIA-AGNDA                OF GFCTB0B2
                                           TO GFCTNM-DIA-AGEND.
           MOVE  QDIA-FLEXZ-AGPTO          OF GFCTB0B2
                                           TO GFCTNM-DIAS-RETENCAO.
           MOVE  CDEPDC-ORIGE-FLEXZ        OF GFCTB0B2
                                           TO GFCTNM-JUNCAO-SOLIC.
           MOVE  CDEPDC-PAREC-FLEXZ        OF GFCTB0B2
                                           TO GFCTNM-JUNCAO-PAREC.
           MOVE  CTPO-OPER-MOTVO           OF GFCTB0B2
                                           TO GFCTNM-CTIPO-OPER-MOTVO.
           MOVE  CMOTVO-JUSTF              OF GFCTB0B2
                                           TO GFCTNM-CMOTVO-JUSTF.
           MOVE  RJUSTF-SOLTC-FLEXZ-TEXT   OF GFCTB0B2
                                           TO GFCTNM-RJUSTF-SOLIC.
           MOVE  RJUSTF-PAREC-FLEXZ-TEXT   OF GFCTB0B2
                                           TO GFCTNM-RJUSTF-PAREC.
090708     MOVE  CPER-COBR-TARIF           OF GFCTB0B2 TO
090708                                     GFCTNM-CPER-COBR-TARIF
090708     MOVE  CINIC-PER-COBR            OF GFCTB0B2 TO
090708                                     GFCTNM-CINIC-PER-COBR

           MOVE '.'                        TO GFCTNM-DATA-SOLIC(3:1)
                                              GFCTNM-DATA-SOLIC(6:1).
           MOVE  HSOLTC-FLEXZ              OF GFCTB0B2(1:4)
                                           TO GFCTNM-DATA-SOLIC(7:4).
           MOVE  HSOLTC-FLEXZ              OF GFCTB0B2(6:2)
                                           TO GFCTNM-DATA-SOLIC(4:2).
           MOVE  HSOLTC-FLEXZ              OF GFCTB0B2(9:2)
                                           TO GFCTNM-DATA-SOLIC(1:2).
           MOVE '.'                        TO GFCTNM-DATA-PAREC(3:1)
                                              GFCTNM-DATA-PAREC(6:1).
           MOVE  HPAREC-FLEXZ              OF GFCTB0B2(1:4)
                                           TO GFCTNM-DATA-PAREC(7:4).
           MOVE  HPAREC-FLEXZ              OF GFCTB0B2(6:2)
                                           TO GFCTNM-DATA-PAREC(4:2).
           MOVE  HPAREC-FLEXZ              OF GFCTB0B2(9:2)
                                           TO GFCTNM-DATA-PAREC(1:2).

           PERFORM 2128-PESQUISAR-NOME-OPERACAO.

           MOVE RTPO-OPER-MOTVO            OF GFCTB0G0
                                           TO GFCTNM-NOME-OPER-MOTVO.

           PERFORM 2129-PESQUISAR-NOME-MOTIVO.

           MOVE RMOTVO-JUSTF               OF GFCTB089
                                           TO GFCTNM-NOME-MOTVO.

           MOVE  CDEPDC-ORIGE-FLEXZ        OF GFCTB0B2
                                           TO GFCT1V-COD-DEPDC(1).

           PERFORM 2130-PESQUISAR-NOME-JUNCAO

           MOVE  GFCT1X-DESC-DEPDC(1)      TO GFCTNM-NOME-JCAO-SOL.

           IF  CDEPDC-PAREC-FLEXZ          OF GFCTB0B2
                                           NOT EQUAL ZEROS
               MOVE  CDEPDC-PAREC-FLEXZ    OF GFCTB0B2
                                           TO GFCT1V-COD-DEPDC(1)

               PERFORM 2130-PESQUISAR-NOME-JUNCAO

               MOVE  GFCT1X-DESC-DEPDC(1)  TO GFCTNM-NOME-JCAO-PAREC
           END-IF.

090708     IF CFUNC-SOLTC-FLEXZ  OF GFCTB0B2
090708                                 NOT EQUAL ZEROS
090708        MOVE CFUNC-SOLTC-FLEXZ   OF GFCTB0B2
090708                                 TO WRK-AUX-9-S
090708        MOVE WRK-AUX-9           TO WRK-PESQUISA-ENT
090708
090708        PERFORM 2059-OBTER-NOME-FUNC
090708        MOVE WRK-NOME-RET(1:40) TO GFCTNM-NOME-FUNC-SOLIC
090708        MOVE WRK-AUX-9-7        TO GFCTNM-FUNC-SOLIC
090708     ELSE
090708        MOVE SPACES             TO GFCTNM-NOME-FUNC-SOLIC
090708        MOVE ZEROS              TO GFCTNM-FUNC-SOLIC
090708     END-IF
090708
090708     IF CFUNC-PAREC-FLEXZ  OF GFCTB0B2
090708                                 NOT EQUAL ZEROS
090708        MOVE CFUNC-PAREC-FLEXZ   OF GFCTB0B2
090708                                 TO WRK-AUX-9-S
090708        MOVE WRK-AUX-9           TO WRK-PESQUISA-ENT
090708
090708        PERFORM 2059-OBTER-NOME-FUNC
090708        MOVE WRK-NOME-RET(1:40) TO GFCTNM-NOME-FUNC-PAREC
090708        MOVE WRK-AUX-9-7        TO GFCTNM-FUNC-PAREC
090708     ELSE
090708        MOVE SPACES             TO GFCTNM-NOME-FUNC-PAREC
090708        MOVE ZEROS              TO GFCTNM-FUNC-PAREC
090708     END-IF.

      *----------------------------------------------------------------*
       2120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA SELECIONAR PAB.                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2121-SELECIONAR-PAB-PEN         SECTION.
      *----------------------------------------------------------------*

           MOVE  CSERVC-TARIF          OF GFCTB0B2
                                       TO CSERVC-TARIF     OF GFCTB0B8.
           MOVE  CAGPTO-CTA            OF GFCTB0B2
                                       TO CAGPTO-CTA       OF GFCTB0B8.
           MOVE  HIDTFD-FLEXZ-AGPTO    OF GFCTB0B2
                                       TO HIDTFD-FLEXZ-AGPTO
                                                           OF GFCTB0B8.

           EXEC SQL
               SELECT
                   CEMPR_INC,
                   CDEPDC,
                   CPOSTO_SERVC
               INTO
                   :GFCTB0B8.CEMPR-INC,
                   :GFCTB0B8.CDEPDC,
                   :GFCTB0B8.CPOSTO-SERVC
               FROM DB2PRD.PDIDO_FLEXZ_PAB
               WHERE
                   CAGPTO_CTA           = :GFCTB0B8.CAGPTO-CTA   AND
                   CSERVC_TARIF         = :GFCTB0B8.CSERVC-TARIF AND
                   HIDTFD_FLEXZ_AGPTO   = :GFCTB0B8.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               INITIALIZE               GFCT0M-ERRO-SQL
               MOVE '0010'              TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE ' - GFCTB0B8'       TO WRK-NOME-TAB
               MOVE 'GFCT0884'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_PAB'   TO GFCT0M-NOME-TAB
               MOVE 'SELECT'            TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0100'              TO GFCT0M-LOCAL
               MOVE  SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE  9                  TO GFCTNM-ERRO
               MOVE  SQLCODE            TO GFCTNM-COD-SQL-ERRO
               MOVE  SPACES             TO GFCTNM-DESC-MSG-ERRO
               MOVE  0010               TO GFCTNM-COD-MSG-ERRO
               PERFORM 2110-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE  1                 TO GFCTNM-ERRO
               MOVE  ZEROS             TO GFCTNM-COD-SQL-ERRO
               MOVE  0926              TO GFCTNM-COD-MSG-ERRO
               MOVE '0926'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTNM-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE  SPACES                        TO GFCTGB-ENTRADA.
           MOVE  100                           TO GFCTGB-LL.
           MOVE  ZEROS                         TO GFCTGB-ZZ.
           MOVE  GFCTNL-TRANSACAO              TO GFCTGB-TRANSACAO.
           MOVE  GFCTNL-FUNCAO                 TO GFCTGB-FUNCAO.
           MOVE  ZEROS                         TO GFCTGB-QTDE-OCOR
                                                  GFCTGB-QTDE-TOT-REG.
           MOVE  GFCTNL-FUNC-BDSCO             TO GFCTGB-FUNC-BDSCO.
           MOVE 'N'                            TO GFCTGB-FIM.
           MOVE  CDEPDC       OF GFCTB0B8      TO GFCTGB-COD-DEPDC.
           MOVE  CPOSTO-SERVC OF GFCTB0B8      TO GFCTGB-COD-POSTO.
           MOVE  SPACES                        TO GFCTGC-SAIDA
                                                  GFCT0M-AREA-ERROS.
           MOVE 'GFCT5109'                     TO WRK-MODULO.

           INITIALIZE GFCTGC-SAIDA
                      GFCT0M-AREA-ERROS.

           CALL WRK-MODULO USING
                                                     GFCTGB-ENTRADA
                                                     GFCTGC-SAIDA
                                                     GFCT0M-AREA-ERROS.

           IF  RETURN-CODE                     NOT EQUAL ZEROS
               MOVE 9                          TO GFCTNM-ERRO
               MOVE WRK-MODULO                 TO WRK-NOME-MOD
                                                  GFCT0M-PROGRAMA
               MOVE WRK-MSG01                  TO GFCT0M-TEXTO
               MOVE 'APL'                      TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0884'                 TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTGC-ERRO                     NOT EQUAL ZEROS
               MOVE 'S'                        TO GFCTNM-FIM
               MOVE  GFCTGC-ERRO               TO GFCTNM-ERRO
               MOVE  GFCTGC-COD-SQL-ERRO       TO GFCTNM-COD-SQL-ERRO
               MOVE  GFCTGC-COD-MSG-ERRO       TO GFCTNM-COD-MSG-ERRO
               IF  GFCTGC-ERRO                 EQUAL 1
                   MOVE  GFCTGC-DESC-MSG-ERRO  TO GFCTNM-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5109- ' GFCTGC-DESC-MSG-ERRO
                           DELIMITED BY SIZE   INTO GFCTNM-DESC-MSG-ERRO
               END-IF
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE  GFCTGC-DESC-POSTO(1)      TO GFCTNM-DESCR-AGPTO.
           MOVE  CDEPDC       OF GFCTB0B8  TO GFCTNM-AGENCIA.
           MOVE  CPOSTO-SERVC OF GFCTB0B8  TO GFCTNM-POSTO.

      *----------------------------------------------------------------*
       2121-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA SELECIONAR AGENCIA.                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2122-SELECIONAR-AGENCIA-PEN     SECTION.
      *----------------------------------------------------------------*

           MOVE  CSERVC-TARIF          OF GFCTB0B2
                                       TO CSERVC-TARIF     OF GFCTB0B1.
           MOVE  CAGPTO-CTA            OF GFCTB0B2
                                       TO CAGPTO-CTA       OF GFCTB0B1.
           MOVE  HIDTFD-FLEXZ-AGPTO    OF GFCTB0B2
                                       TO HIDTFD-FLEXZ-AGPTO
                                                           OF GFCTB0B1.

           EXEC SQL
               SELECT
                   CEMPR_INC,
                   CDEPDC
               INTO
                   :GFCTB0B1.CEMPR-INC,
                   :GFCTB0B1.CDEPDC
               FROM DB2PRD.PDIDO_FLEXZ_AG
               WHERE
                   CAGPTO_CTA          = :GFCTB0B1.CAGPTO-CTA   AND
                   CSERVC_TARIF        = :GFCTB0B1.CSERVC-TARIF AND
                   HIDTFD_FLEXZ_AGPTO  = :GFCTB0B1.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE ' - GFCTB0B1'      TO WRK-NOME-TAB
               MOVE 'GFCT0884'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_PAB'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE '0110'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCTNM-ERRO
               MOVE  SQLCODE           TO GFCTNM-COD-SQL-ERRO
               MOVE  SPACES            TO GFCTNM-DESC-MSG-ERRO
               MOVE  0010              TO GFCTNM-COD-MSG-ERRO
               PERFORM 2110-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE  1                 TO GFCTNM-ERRO
               MOVE  ZEROS             TO GFCTNM-COD-SQL-ERRO
               MOVE  0926              TO GFCTNM-COD-MSG-ERRO
               MOVE '0926'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTNM-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE  SPACES                      TO GFCT1V-ENTRADA.
           MOVE  660                         TO GFCT1V-LL.
           MOVE  ZEROS                       TO GFCT1V-ZZ.
           MOVE  GFCTNL-TRANSACAO            TO GFCT1V-TRANSACAO.
           MOVE  GFCTNL-FUNCAO               TO GFCT1V-FUNCAO.
           MOVE  001                         TO GFCT1V-QTDE-OCOR
           MOVE  ZEROS                       TO GFCT1V-QTDE-TOT-REG.
           MOVE  GFCTNL-FUNC-BDSCO           TO GFCT1V-FUNC-BDSCO.
           MOVE 'N'                          TO GFCT1V-FIM.
           MOVE  CDEPDC OF GFCTB0B1          TO GFCT1V-COD-DEPDC(1).
           MOVE  SPACES                      TO GFCT1X-SAIDA
                                                GFCT0M-AREA-ERROS.
           MOVE 'GFCT5006'                   TO WRK-MODULO.

           INITIALIZE GFCT1X-SAIDA
                      GFCT0M-AREA-ERROS.

           CALL WRK-MODULO USING
                                                     GFCT1V-ENTRADA
                                                     GFCT1X-SAIDA
                                                     GFCT0M-AREA-ERROS.

           IF  RETURN-CODE                     NOT EQUAL ZEROS
               MOVE 9                          TO GFCTNM-ERRO
               MOVE WRK-MODULO                 TO WRK-NOME-MOD
                                                  GFCT0M-PROGRAMA
               MOVE WRK-MSG01                  TO GFCT0M-TEXTO
               MOVE 'APL'                      TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0884'                 TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCT1X-ERRO                     NOT EQUAL ZEROS
               MOVE 'S'                        TO GFCTNM-FIM
               MOVE  GFCT1X-ERRO               TO GFCTNM-ERRO
               MOVE  GFCT1X-COD-SQL-ERRO       TO GFCTNM-COD-SQL-ERRO
               MOVE  GFCT1X-COD-MSG-ERRO       TO GFCTNM-COD-MSG-ERRO
               IF  GFCT1X-ERRO                 EQUAL 1
                   MOVE  GFCT1X-DESC-MSG-ERRO  TO GFCTNM-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5006- ' GFCT1X-DESC-MSG-ERRO
                           DELIMITED BY SIZE   INTO GFCTNM-DESC-MSG-ERRO
               END-IF
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE  GFCT1X-DESC-DEPDC(1)      TO GFCTNM-DESCR-AGPTO.

      *----------------------------------------------------------------*
       2122-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA SELECIONAR POSTAL.                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2123-SELECIONAR-POSTAL-PEN      SECTION.
      *----------------------------------------------------------------*

           MOVE  CSERVC-TARIF          OF GFCTB0B2
                                       TO CSERVC-TARIF     OF GFCTB0B9.
           MOVE  CAGPTO-CTA            OF GFCTB0B2
                                       TO CAGPTO-CTA       OF GFCTB0B9.
           MOVE  HIDTFD-FLEXZ-AGPTO    OF GFCTB0B2
                                       TO HIDTFD-FLEXZ-AGPTO
                                                           OF GFCTB0B9.

           EXEC SQL
               SELECT
                   CEMPR_INC,
                   CDEPDC,
                   CPOSTO_SERVC
               INTO
                   :GFCTB0B9.CEMPR-INC,
                   :GFCTB0B9.CDEPDC,
                   :GFCTB0B9.CPOSTO-SERVC
               FROM DB2PRD.PDIDO_FLEXZ_PSTAL
               WHERE
                   CAGPTO_CTA          = :GFCTB0B9.CAGPTO-CTA   AND
                   CSERVC_TARIF        = :GFCTB0B9.CSERVC-TARIF AND
                   HIDTFD_FLEXZ_AGPTO  = :GFCTB0B9.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               INITIALIZE               GFCT0M-ERRO-SQL
               MOVE '0010'              TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE ' - GFCTB0B9'       TO WRK-NOME-TAB
               MOVE 'GFCT0884'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_PSTAL' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'            TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0120'              TO GFCT0M-LOCAL
               MOVE  SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE  9                  TO GFCTNM-ERRO
               MOVE  SQLCODE            TO GFCTNM-COD-SQL-ERRO
               MOVE  SPACES             TO GFCTNM-DESC-MSG-ERRO
               MOVE  0010               TO GFCTNM-COD-MSG-ERRO
               PERFORM 2110-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE  1                 TO GFCTNM-ERRO
               MOVE  ZEROS             TO GFCTNM-COD-SQL-ERRO
               MOVE  0926              TO GFCTNM-COD-MSG-ERRO
               MOVE '0926'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTNM-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE  SPACES                        TO GFCTGB-ENTRADA.
           MOVE  100                           TO GFCTGB-LL.
           MOVE  ZEROS                         TO GFCTGB-ZZ.
           MOVE  GFCTNL-TRANSACAO              TO GFCTGB-TRANSACAO.
           MOVE  GFCTNL-FUNCAO                 TO GFCTGB-FUNCAO.
           MOVE  ZEROS                         TO GFCTGB-QTDE-OCOR
                                                  GFCTGB-QTDE-TOT-REG.
           MOVE  GFCTNL-FUNC-BDSCO             TO GFCTGB-FUNC-BDSCO.
           MOVE 'N'                            TO GFCTGB-FIM.
           MOVE  CDEPDC       OF GFCTB0B9      TO GFCTGB-COD-DEPDC.
           MOVE  CPOSTO-SERVC OF GFCTB0B9      TO GFCTGB-COD-POSTO.
           MOVE  SPACES                        TO GFCTGC-SAIDA
                                                  GFCT0M-AREA-ERROS.
           MOVE 'GFCT5090'                     TO WRK-MODULO.

           INITIALIZE GFCTGC-SAIDA
                      GFCT0M-AREA-ERROS.

           CALL WRK-MODULO USING
                                                     GFCTGB-ENTRADA
                                                     GFCTGC-SAIDA
                                                     GFCT0M-AREA-ERROS.

           IF  RETURN-CODE                     NOT EQUAL ZEROS
               MOVE 9                          TO GFCTNM-ERRO
               MOVE WRK-MODULO                 TO WRK-NOME-MOD
                                                  GFCT0M-PROGRAMA
               MOVE WRK-MSG01                  TO GFCT0M-TEXTO
               MOVE 'APL'                      TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0884'                 TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTGC-ERRO                     NOT EQUAL ZEROS
020908**       MOVE 'S'                        TO GFCTNM-FIM
020908**       MOVE  GFCTGC-ERRO               TO GFCTNM-ERRO
020908**       MOVE  GFCTGC-COD-SQL-ERRO       TO GFCTNM-COD-SQL-ERRO
020908**       MOVE  GFCTGC-COD-MSG-ERRO       TO GFCTNM-COD-MSG-ERRO
               IF  GFCTGC-ERRO                 EQUAL 1
020908**           MOVE  GFCTGC-DESC-MSG-ERRO  TO GFCTNM-DESC-MSG-ERRO
020908             MOVE  GFCTGC-DESC-MSG-ERRO  TO GFCTGC-DESC-POSTO(1)
               ELSE
                   STRING 'GFCT5090- ' GFCTGC-DESC-MSG-ERRO
                           DELIMITED BY SIZE   INTO GFCTNM-DESC-MSG-ERRO
020908             MOVE 'S'                    TO GFCTNM-FIM
020908             MOVE  GFCTGC-ERRO           TO GFCTNM-ERRO
020908             MOVE  GFCTGC-COD-SQL-ERRO   TO GFCTNM-COD-SQL-ERRO
020908             MOVE  GFCTGC-COD-MSG-ERRO   TO GFCTNM-COD-MSG-ERRO
020908             PERFORM 3000-FINALIZAR
               END-IF
020908**       PERFORM 3000-FINALIZAR
           END-IF.

           MOVE  GFCTGC-DESC-POSTO(1)      TO GFCTNM-DESCR-AGPTO.
           MOVE  CDEPDC       OF GFCTB0B9  TO GFCTNM-AGENCIA.
           MOVE  CPOSTO-SERVC OF GFCTB0B9  TO GFCTNM-POSTO.

      *----------------------------------------------------------------*
       2123-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA SELECIONAR CLIENTE.                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2124-SELECIONAR-CLIENTE-PEN     SECTION.
      *----------------------------------------------------------------*

           MOVE  CSERVC-TARIF          OF GFCTB0B2
                                       TO CSERVC-TARIF     OF GFCTB0B3.
           MOVE  CAGPTO-CTA            OF GFCTB0B2
                                       TO CAGPTO-CTA       OF GFCTB0B3.
           MOVE  HIDTFD-FLEXZ-AGPTO    OF GFCTB0B2
                                       TO HIDTFD-FLEXZ-AGPTO
                                                           OF GFCTB0B3.

           EXEC SQL
               SELECT
ST2506*            CCGC_CPF,
ST2506*            CFLIAL_CGC,
ST2506*            CCTRL_CPF_CGC
ST2506             CCGC_CPF_ST,
ST2506             CFLIAL_CGC_ST,
ST2506             CCTRL_CPF_CGC_ST
               INTO
ST2506*            :GFCTB0B3.CCGC-CPF,
ST2506*            :GFCTB0B3.CFLIAL-CGC,
ST2506*            :GFCTB0B3.CCTRL-CPF-CGC
ST2506             :GFCTB0B3.CCGC-CPF-ST,
ST2506             :GFCTB0B3.CFLIAL-CGC-ST,
ST2506             :GFCTB0B3.CCTRL-CPF-CGC-ST
               FROM DB2PRD.PDIDO_FLEXZ_CLI
               WHERE
                   CAGPTO_CTA          = :GFCTB0B3.CAGPTO-CTA   AND
                   CSERVC_TARIF        = :GFCTB0B3.CSERVC-TARIF AND
                   HIDTFD_FLEXZ_AGPTO  = :GFCTB0B3.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               INITIALIZE               GFCT0M-ERRO-SQL
               MOVE '0010'              TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE ' - GFCTB0B3'       TO WRK-NOME-TAB
               MOVE 'GFCT0884'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_CLI'   TO GFCT0M-NOME-TAB
               MOVE 'SELECT'            TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0130'              TO GFCT0M-LOCAL
               MOVE  SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE  9                  TO GFCTNM-ERRO
               MOVE  SQLCODE            TO GFCTNM-COD-SQL-ERRO
               MOVE  SPACES             TO GFCTNM-DESC-MSG-ERRO
               MOVE  0010               TO GFCTNM-COD-MSG-ERRO
               PERFORM 2110-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE  1                 TO GFCTNM-ERRO
               MOVE  ZEROS             TO GFCTNM-COD-SQL-ERRO
               MOVE  0926              TO GFCTNM-COD-MSG-ERRO
               MOVE '0926'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTNM-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE  SPACES                    TO GFCTNM-DESCR-AGPTO.
ST2506*    MOVE  CCGC-CPF      OF GFCTB0B3 TO GFCTNM-CGC-CPF.
ST2506*    MOVE  CFLIAL-CGC    OF GFCTB0B3 TO GFCTNM-FILIAL.
ST2506*    MOVE  CCTRL-CPF-CGC OF GFCTB0B3 TO GFCTNM-CONTROLE.
ST2506     MOVE  CCGC-CPF-ST               OF GFCTB0B3 
ST2506                                     TO GFCTNM-CGC-CPF.
ST2506     MOVE  CFLIAL-CGC-ST             OF GFCTB0B3 
ST2506                                     TO GFCTNM-FILIAL.
ST2506     MOVE  CCTRL-CPF-CGC-ST          OF GFCTB0B3 
                                           TO GFCTNM-CONTROLE.

      *----------------------------------------------------------------*
       2124-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA SELECIONAR SEGMENTO.                               *
      ******************************************************************
      *----------------------------------------------------------------*
       2125-SELECIONAR-SEGMENTO-PEN    SECTION.
      *----------------------------------------------------------------*

           MOVE  CSERVC-TARIF          OF GFCTB0B2
                                       TO CSERVC-TARIF     OF GFCTB0C0.
           MOVE  CAGPTO-CTA            OF GFCTB0B2
                                       TO CAGPTO-CTA       OF GFCTB0C0.
           MOVE  HIDTFD-FLEXZ-AGPTO    OF GFCTB0B2
                                       TO HIDTFD-FLEXZ-AGPTO
                                                           OF GFCTB0C0.

           EXEC SQL
               SELECT CSGMTO_GSTAO_TARIF
               INTO  :GFCTB0C0.CSGMTO-GSTAO-TARIF
               FROM   DB2PRD.PDIDO_FLEXZ_SGMTO
               WHERE
                   CAGPTO_CTA          = :GFCTB0C0.CAGPTO-CTA   AND
                   CSERVC_TARIF        = :GFCTB0C0.CSERVC-TARIF AND
                   HIDTFD_FLEXZ_AGPTO  = :GFCTB0C0.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               INITIALIZE               GFCT0M-ERRO-SQL
               MOVE '0010'              TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE ' - GFCTB0C0'       TO WRK-NOME-TAB
               MOVE 'GFCT0884'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_SGMTO' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'            TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0140'              TO GFCT0M-LOCAL
               MOVE  SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE  9                  TO GFCTNM-ERRO
               MOVE  SQLCODE            TO GFCTNM-COD-SQL-ERRO
               MOVE  SPACES             TO GFCTNM-DESC-MSG-ERRO
               MOVE  0010               TO GFCTNM-COD-MSG-ERRO
               PERFORM 2110-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE  1                 TO GFCTNM-ERRO
               MOVE  ZEROS             TO GFCTNM-COD-SQL-ERRO
               MOVE  0926              TO GFCTNM-COD-MSG-ERRO
               MOVE '0926'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTNM-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE  CSGMTO-GSTAO-TARIF   OF GFCTB0C0
                                      TO CSGMTO-GSTAO-TARIF OF GFCTB0H5.

           EXEC SQL
               SELECT ISGMTO_GSTAO_TARIF
               INTO  :GFCTB0H5.ISGMTO-GSTAO-TARIF
               FROM  DB2PRD.TTPO_SGMTO_GSTAO
               WHERE CSGMTO_GSTAO_TARIF = :GFCTB0H5.CSGMTO-GSTAO-TARIF
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               INITIALIZE               GFCT0M-ERRO-SQL
               MOVE '0010'              TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE ' - GFCTB0H5'       TO WRK-NOME-TAB
               MOVE 'GFCT0884'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'TTPO_SGMTO_GSTAO'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'            TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0150'              TO GFCT0M-LOCAL
               MOVE  SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE  9                  TO GFCTNM-ERRO
               MOVE  SQLCODE            TO GFCTNM-COD-SQL-ERRO
               MOVE  SPACES             TO GFCTNM-DESC-MSG-ERRO
               MOVE  0010               TO GFCTNM-COD-MSG-ERRO
               PERFORM 2110-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE  1                 TO GFCTNM-ERRO
               MOVE  ZEROS             TO GFCTNM-COD-SQL-ERRO
               MOVE  0500              TO GFCTNM-COD-MSG-ERRO
               MOVE '0500'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTNM-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE  ISGMTO-GSTAO-TARIF    OF GFCTB0H5
                                       TO GFCTNM-DESCR-AGPTO.
           MOVE  CSGMTO-GSTAO-TARIF   OF GFCTB0C0
                                       TO GFCTNM-SEGMENTO.

      *----------------------------------------------------------------*
       2125-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA SELECIONAR MUNICIPIO.                              *
      ******************************************************************
      *----------------------------------------------------------------*
       2126-SELECIONAR-MUNICIPIO-PEN   SECTION.
      *----------------------------------------------------------------*

           MOVE  CSERVC-TARIF          OF GFCTB0B2
                                       TO CSERVC-TARIF     OF GFCTB0B7.
           MOVE  CAGPTO-CTA            OF GFCTB0B2
                                       TO CAGPTO-CTA       OF GFCTB0B7.
           MOVE  HIDTFD-FLEXZ-AGPTO    OF GFCTB0B2
                                       TO HIDTFD-FLEXZ-AGPTO
                                                           OF GFCTB0B7.

           EXEC SQL
               SELECT CMUN_IBGE
               INTO  :GFCTB0B7.CMUN-IBGE
               FROM   DB2PRD.PDIDO_FLEXZ_MUN
               WHERE
                   CAGPTO_CTA         = :GFCTB0B7.CAGPTO-CTA   AND
                   CSERVC_TARIF       = :GFCTB0B7.CSERVC-TARIF AND
                   HIDTFD_FLEXZ_AGPTO = :GFCTB0B7.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               INITIALIZE               GFCT0M-ERRO-SQL
               MOVE '0010'              TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE ' - GFCTB0B7'       TO WRK-NOME-TAB
               MOVE 'GFCT0884'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_MUN'   TO GFCT0M-NOME-TAB
               MOVE 'SELECT'            TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0160'              TO GFCT0M-LOCAL
               MOVE  SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE  9                  TO GFCTNM-ERRO
               MOVE  SQLCODE            TO GFCTNM-COD-SQL-ERRO
               MOVE  SPACES             TO GFCTNM-DESC-MSG-ERRO
               MOVE  0010               TO GFCTNM-COD-MSG-ERRO
               PERFORM 2110-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE  1                 TO GFCTNM-ERRO
               MOVE  ZEROS             TO GFCTNM-COD-SQL-ERRO
               MOVE  0926              TO GFCTNM-COD-MSG-ERRO
               MOVE '0926'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTNM-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           INITIALIZE AREA-RURC88.

           MOVE  CMUN-IBGE OF GFCTB0B7 TO COD-MUNIC-RURC88.
           MOVE 'RURC9020'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                             AREA-RURC88.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE  1                 TO GFCTNM-ERRO
               MOVE  0732              TO GFCTNM-COD-MSG-ERRO
               MOVE '0732'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTNM-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           EVALUATE COD-RETORNO-RURC88
               WHEN 10
                   MOVE  1              TO GFCTNM-ERRO
                   MOVE  ZEROS          TO GFCTNM-COD-SQL-ERRO
                   MOVE  0675           TO GFCTNM-COD-MSG-ERRO
                   MOVE '0675'          TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESC-MENSAGEM
                   MOVE GFCTG3-DESC-MSG TO GFCTNM-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR

               WHEN 20
                   MOVE  1              TO GFCTNM-ERRO
                   MOVE  ZEROS          TO GFCTNM-COD-SQL-ERRO
                   MOVE  0733           TO GFCTNM-COD-MSG-ERRO
                   MOVE '0733'          TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESC-MENSAGEM
                   MOVE GFCTG3-DESC-MSG TO GFCTNM-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR

               WHEN 99
                   MOVE  1              TO GFCTNM-ERRO
                   MOVE  ZEROS          TO GFCTNM-COD-SQL-ERRO
                   MOVE  0734           TO GFCTNM-COD-MSG-ERRO
                   MOVE '0734'          TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESC-MENSAGEM
                   MOVE GFCTG3-DESC-MSG TO GFCTNM-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR

           END-EVALUATE.

           MOVE  NOME-MUNIC-RURC88     TO GFCTNM-DESCR-AGPTO.
           MOVE  CMUN-IBGE OF GFCTB0B7 TO GFCTNM-MUNICIPIO.

      *----------------------------------------------------------------*
       2126-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA SELECIONAR UF.                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2127-SELECIONAR-UF-PEN          SECTION.
      *----------------------------------------------------------------*

           MOVE  CSERVC-TARIF          OF GFCTB0B2
                                       TO CSERVC-TARIF     OF GFCTB0C1.
           MOVE  CAGPTO-CTA            OF GFCTB0B2
                                       TO CAGPTO-CTA       OF GFCTB0C1.
           MOVE  HIDTFD-FLEXZ-AGPTO    OF GFCTB0B2
                                       TO HIDTFD-FLEXZ-AGPTO
                                                           OF GFCTB0C1.

           EXEC SQL
               SELECT CSGL_UF
               INTO  :GFCTB0C1.CSGL-UF
               FROM   DB2PRD.PDIDO_FLEXZ_UF
               WHERE
                   CAGPTO_CTA          = :GFCTB0C1.CAGPTO-CTA   AND
                   CSERVC_TARIF        = :GFCTB0C1.CSERVC-TARIF AND
                   HIDTFD_FLEXZ_AGPTO  = :GFCTB0C1.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               INITIALIZE               GFCT0M-ERRO-SQL
               MOVE '0010'              TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE ' - GFCTB0C1'       TO WRK-NOME-TAB
               MOVE 'GFCT0884'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_UF'    TO GFCT0M-NOME-TAB
               MOVE 'SELECT'            TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0170'              TO GFCT0M-LOCAL
               MOVE  SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE  9                  TO GFCTNM-ERRO
               MOVE  SQLCODE            TO GFCTNM-COD-SQL-ERRO
               MOVE  SPACES             TO GFCTNM-DESC-MSG-ERRO
               MOVE  0010               TO GFCTNM-COD-MSG-ERRO
               PERFORM 2110-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE  1                 TO GFCTNM-ERRO
               MOVE  ZEROS             TO GFCTNM-COD-SQL-ERRO
               MOVE  0926              TO GFCTNM-COD-MSG-ERRO
               MOVE '0926'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTNM-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE  CSGL-UF   OF GFCTB0C1 TO CSGL-UF          OF DCITV002.
           MOVE  DCITV002              TO WRK-AREA-DCLGEN-DCITV002.
           MOVE 'DCIT8000'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                             WRK-DCIT-UNIDADE-FEDERATIVA
                                             WRK-POOL7100
                                             WRK-SQLCA.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE  1                 TO GFCTNM-ERRO
               MOVE  ZEROS             TO GFCTNM-COD-SQL-ERRO
               MOVE  0735              TO GFCTNM-COD-MSG-ERRO
               MOVE '0735'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTNM-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           EVALUATE WRK-DCIT-CODIGO-RETORNO
               WHEN 1
                   MOVE  1              TO GFCTNM-ERRO
                   MOVE  ZEROS          TO GFCTNM-COD-SQL-ERRO
                   MOVE  0736           TO GFCTNM-COD-MSG-ERRO
                   MOVE '0736'          TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESC-MENSAGEM
                   MOVE GFCTG3-DESC-MSG TO GFCTNM-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR

               WHEN 2
                   MOVE  1              TO GFCTNM-ERRO
                   MOVE  ZEROS          TO GFCTNM-COD-SQL-ERRO
                   MOVE  0737           TO GFCTNM-COD-MSG-ERRO
                   MOVE '0737'          TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESC-MENSAGEM
                   MOVE GFCTG3-DESC-MSG TO GFCTNM-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR

               WHEN 3
                   MOVE  1              TO GFCTNM-ERRO
                   MOVE  ZEROS          TO GFCTNM-COD-SQL-ERRO
                   MOVE  0738           TO GFCTNM-COD-MSG-ERRO
                   MOVE '0738'          TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESC-MENSAGEM
                   MOVE GFCTG3-DESC-MSG TO GFCTNM-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR

               WHEN 4
                   INITIALIZE               GFCT0M-ERRO-SQL
                   MOVE '0005'              TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESC-MENSAGEM
                   MOVE ' - DCITV002'       TO WRK-NOME-TAB
                   MOVE 'GFCT0884'          TO GFCT0M-PROGRAMA
                   MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
                   MOVE 'V01_UF'            TO GFCT0M-NOME-TAB
                   MOVE 'SELECT'            TO GFCT0M-COMANDO-SQL
                   MOVE  SQLCODE            TO GFCT0M-SQLCODE
                   MOVE '0180'              TO GFCT0M-LOCAL
                   MOVE  SQLCA              TO GFCT0M-SQLCA-AREA
                   MOVE  9                  TO GFCTNM-ERRO
                   MOVE  SQLCODE            TO GFCTNM-COD-SQL-ERRO
                   MOVE  SPACES             TO GFCTNM-DESC-MSG-ERRO
                   MOVE  0005               TO GFCTNM-COD-MSG-ERRO
                   PERFORM 2110-CONCATENAR-MENSAGEM
                   PERFORM 3000-FINALIZAR

           END-EVALUATE.

           MOVE  WRK-AREA-DCLGEN-DCITV002  TO DCITV002.
           MOVE  IUF     OF DCITV002       TO GFCTNM-DESCR-AGPTO.
           MOVE  CSGL-UF OF GFCTB0C1       TO GFCTNM-UF.

      *----------------------------------------------------------------*
       2127-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA PESQUISAR NOME TARIFA.                             *
      ******************************************************************
      *----------------------------------------------------------------*
       2128-PESQUISAR-NOME-OPERACAO    SECTION.
      *----------------------------------------------------------------*

           MOVE  CTPO-OPER-MOTVO       OF GFCTB0B2
                                       TO CTPO-OPER-MOTVO  OF GFCTB0G0.

           EXEC SQL
               SELECT RTPO_OPER_MOTVO
               INTO  :GFCTB0G0.RTPO-OPER-MOTVO
               FROM   DB2PRD.TPO_OPER_MOTVO
               WHERE  CTPO_OPER_MOTVO  = :GFCTB0G0.CTPO-OPER-MOTVO
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               INITIALIZE               GFCT0M-ERRO-SQL
               MOVE '0010'              TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE ' - GFCTB0G0'       TO WRK-NOME-TAB
               MOVE 'GFCT0884'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'TPO_OPER_MOTVO'    TO GFCT0M-NOME-TAB
               MOVE 'SELECT'            TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0190'              TO GFCT0M-LOCAL
               MOVE  SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE  9                  TO GFCTNM-ERRO
               MOVE  SQLCODE            TO GFCTNM-COD-SQL-ERRO
               MOVE  SPACES             TO GFCTNM-DESC-MSG-ERRO
               MOVE  0010               TO GFCTNM-COD-MSG-ERRO
               PERFORM 2110-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE  1                 TO GFCTNM-ERRO
               MOVE  ZEROS             TO GFCTNM-COD-SQL-ERRO
               MOVE  0070              TO GFCTNM-COD-MSG-ERRO
               MOVE '0070'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTNM-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2128-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA PESQUISAR NOME DO MOTIVO.                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2129-PESQUISAR-NOME-MOTIVO      SECTION.
      *----------------------------------------------------------------*

           MOVE  CTPO-OPER-MOTVO       OF GFCTB0B2
                                       TO CTPO-OPER-MOTVO  OF GFCTB089.
           MOVE  CMOTVO-JUSTF          OF GFCTB0B2
                                       TO CMOTVO-JUSTF     OF GFCTB089.

           EXEC SQL
               SELECT RMOTVO_JUSTF
               INTO  :GFCTB089.RMOTVO-JUSTF
               FROM   DB2PRD.MOTVO_JUSTF_OPER
               WHERE
                   CTPO_OPER_MOTVO     = :GFCTB089.CTPO-OPER-MOTVO AND
                   CMOTVO_JUSTF        = :GFCTB089.CMOTVO-JUSTF
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE ' - GFCTB089'      TO WRK-NOME-TAB
               MOVE 'GFCT0884'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'MOTVO_JUSTF_OPER' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE '0200'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCTNM-ERRO
               MOVE  SQLCODE           TO GFCTNM-COD-SQL-ERRO
               MOVE  SPACES            TO GFCTNM-DESC-MSG-ERRO
               MOVE  0010              TO GFCTNM-COD-MSG-ERRO
               PERFORM 2110-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE  1                 TO GFCTNM-ERRO
               MOVE  ZEROS             TO GFCTNM-COD-SQL-ERRO
               MOVE  0074              TO GFCTNM-COD-MSG-ERRO
               MOVE '0074'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTNM-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2129-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA PESQUISAR NOME JUNCAO.                             *
      ******************************************************************
      *----------------------------------------------------------------*
       2130-PESQUISAR-NOME-JUNCAO      SECTION.
      *----------------------------------------------------------------*

           MOVE  660                         TO GFCT1V-LL.
           MOVE  ZEROS                       TO GFCT1V-ZZ.

           MOVE  GFCTNL-TRANSACAO            TO GFCT1V-TRANSACAO.
           MOVE  GFCTNL-FUNCAO               TO GFCT1V-FUNCAO.
           MOVE  001                         TO GFCT1V-QTDE-OCOR
           MOVE  ZEROS                       TO GFCT1V-QTDE-TOT-REG.
           MOVE  GFCTNL-FUNC-BDSCO           TO GFCT1V-FUNC-BDSCO.
           MOVE 'N'                          TO GFCT1V-FIM.

           MOVE  SPACES                      TO GFCT1X-SAIDA
                                                GFCT0M-AREA-ERROS.

           INITIALIZE GFCT1X-SAIDA
                      GFCT0M-AREA-ERROS.

           MOVE 'GFCT5006'                   TO WRK-MODULO.

           CALL WRK-MODULO USING
                                                     GFCT1V-ENTRADA
                                                     GFCT1X-SAIDA
                                                     GFCT0M-AREA-ERROS.

           IF  RETURN-CODE                     NOT EQUAL ZEROS
               MOVE 9                          TO GFCTNM-ERRO
               MOVE WRK-MODULO                 TO WRK-NOME-MOD
                                                  GFCT0M-PROGRAMA
               MOVE WRK-MSG01                  TO GFCT0M-TEXTO
               MOVE 'APL'                      TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0884'                 TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCT1X-ERRO                     NOT EQUAL ZEROS
               MOVE 'S'                        TO GFCTNM-FIM
               MOVE  GFCT1X-ERRO               TO GFCTNM-ERRO
               MOVE  GFCT1X-COD-SQL-ERRO       TO GFCTNM-COD-SQL-ERRO
               MOVE  GFCT1X-COD-MSG-ERRO       TO GFCTNM-COD-MSG-ERRO

               IF  GFCT1X-ERRO                 EQUAL 1
                   MOVE  GFCT1X-DESC-MSG-ERRO  TO GFCTNM-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5006- ' GFCT1X-DESC-MSG-ERRO
                           DELIMITED BY SIZE   INTO GFCTNM-DESC-MSG-ERRO
               END-IF

               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

090708******************************************************************
090708*PESQUISAR NOME DO AGRUPAMENTO PERSONALIZADO                     *
090708******************************************************************
090708*----------------------------------------------------------------*
090708 2131-SELECIONAR-AGRUP           SECTION.
090708*----------------------------------------------------------------*
090708
090708     EXEC SQL
090708       SELECT  RAGPTO_CTA
090708         INTO  :GFCTB0F3.RAGPTO-CTA
090708         FROM  DB2PRD.TPO_AGPTO_CTA
090708        WHERE  CAGPTO_CTA         = :GFCTB0F3.CAGPTO-CTA
090708     END-EXEC.
090708
090708     IF (SQLCODE                 NOT EQUAL ZEROS
090708                                 AND +100 AND -811) OR
090708        (SQLWARN0                EQUAL 'W')
090708         INITIALIZE              GFCT0M-ERRO-SQL
090708         MOVE '0010'             TO GFCTG2-COD-MSG
090708         PERFORM 1310-OBTER-DESC-MENSAGEM
090708         MOVE ' - GFCTB0F3'      TO WRK-NOME-TAB
090708         MOVE 'TPO_AGPTO_CTA'    TO GFCT0M-NOME-TAB
090708         MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
090708         MOVE 'GFCT0884'         TO GFCT0M-PROGRAMA
090708         MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
090708         MOVE  SQLCODE           TO GFCT0M-SQLCODE
090708         MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
090708         MOVE  9                 TO GFCTNM-ERRO
090708         MOVE  SQLCODE           TO GFCTNM-COD-SQL-ERRO
090708         MOVE  SPACES            TO GFCTNM-DESC-MSG-ERRO
090708         MOVE  0010              TO GFCTNM-COD-MSG-ERRO
090708         PERFORM 2110-CONCATENAR-MENSAGEM
090708         MOVE '0215'             TO GFCT0M-LOCAL
090708         MOVE  0010              TO GFCTNM-COD-MSG-ERRO
090708         PERFORM 3000-FINALIZAR
090708     END-IF.
090708
090708     IF  SQLCODE                 EQUAL +100
090708         MOVE 'AGRUPAMENTO NAO ENCONTRADO'
090708                                 TO RAGPTO-CTA OF GFCTB0F3
090708     END-IF.
090708
090708     MOVE RAGPTO-CTA             OF GFCTB0F3
090708                                 TO GFCTNM-DESCR-AGPTO.
090708
090708*----------------------------------------------------------------*
090708 2131-99-FIM.                    EXIT.
090708*----------------------------------------------------------------*

      ******************************************************************
0310  * ROTINA PARA SELECIONAR RAZAO                                   *
0310  ******************************************************************
0310  *----------------------------------------------------------------*
0310   2132-SELECIONAR-RAZAO-PEN       SECTION.
0310  *----------------------------------------------------------------*
0310
0310       MOVE  CSERVC-TARIF          OF GFCTB0B2
0310                                   TO CSERVC-TARIF       OF GFCTB0M7
0310       MOVE  CAGPTO-CTA            OF GFCTB0B2
0310                                   TO CAGPTO-CTA         OF GFCTB0M7
0310       MOVE  HIDTFD-FLEXZ-AGPTO    OF GFCTB0B2
0310                                   TO HIDTFD-FLEXZ-AGPTO OF GFCTB0M7
0310
0310       EXEC SQL
0310           SELECT
0310               CGRP_CTBIL_ATUAL,
0310               CSGRP_CTBIL
0310           INTO
0310               :GFCTB0M7.CGRP-CTBIL-ATUAL,
0310               :GFCTB0M7.CSGRP-CTBIL
0310           FROM DB2PRD.TPDIDO_FLEXZ_RZ
0310           WHERE
0310               CAGPTO_CTA          = :GFCTB0M7.CAGPTO-CTA   AND
0310               CSERVC_TARIF        = :GFCTB0M7.CSERVC-TARIF AND
0310               HIDTFD_FLEXZ_AGPTO  = :GFCTB0M7.HIDTFD-FLEXZ-AGPTO
0310       END-EXEC.
0310
0310       IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
0310          (SQLWARN0                 EQUAL 'W')
0310           INITIALIZE               GFCT0M-ERRO-SQL
0310           MOVE '0010'              TO GFCTG2-COD-MSG
0310           PERFORM 1310-OBTER-DESC-MENSAGEM
0310           MOVE 'GFCTB0M7'           TO WRK-NOME-TAB
0310           MOVE 'GFCT0884'           TO GFCT0M-PROGRAMA
0310           MOVE 'DB2'                TO GFCT0M-TIPO-ACESSO
0310           MOVE 'TPDIDO_FLEXZ_RZ   ' TO GFCT0M-NOME-TAB
0310           MOVE 'SELECT'             TO GFCT0M-COMANDO-SQL
0310           MOVE  SQLCODE             TO GFCT0M-SQLCODE
0310           MOVE '0120'               TO GFCT0M-LOCAL
0310           MOVE  SQLCA               TO GFCT0M-SQLCA-AREA
0310           MOVE  9                   TO GFCTNM-ERRO
0310           MOVE  SQLCODE             TO WRK-COD-SQL-ERRO
0310           MOVE  WRK-COD-SQL-ERRO-2  TO GFCTNM-COD-SQL-ERRO
0310           MOVE  SPACES              TO GFCTNM-DESC-MSG-ERRO
0310           MOVE  0010                TO GFCTNM-COD-MSG-ERRO
0310           PERFORM 2110-CONCATENAR-MENSAGEM
0310           PERFORM 3000-FINALIZAR
0310       END-IF.
0310
0310       IF  SQLCODE                 EQUAL +100
0310           MOVE  1                 TO GFCTNM-ERRO
0310           MOVE  ZEROS             TO GFCTNM-COD-SQL-ERRO
0310           MOVE  0926              TO GFCTNM-COD-MSG-ERRO
0310           MOVE '0926'             TO GFCTG2-COD-MSG
0310           PERFORM 1310-OBTER-DESC-MENSAGEM
0310           MOVE GFCTG3-DESC-MSG    TO GFCTNM-DESC-MSG-ERRO
0310           PERFORM 3000-FINALIZAR
0310       END-IF.
0310
0310       MOVE  CGRP-CTBIL-ATUAL OF GFCTB0M7 TO GFCTNM-GRUPO-CTBIL
0310       MOVE  CSGRP-CTBIL OF GFCTB0M7      TO GFCTNM-SUB-GRUPO-CTBIL.
0310
0310  *----------------------------------------------------------------*
0310   2132-99-FIM.                    EXIT.
0310  *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA PESQUISAR NOME TARIFA.                             *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-TRATAR-CSIT-SOLTC          SECTION.
      *----------------------------------------------------------------*

           EVALUATE CSIT-SOLTC-FLEXZ   OF GFCTB0B2
               WHEN 01
                   MOVE 'AGUARDANDO PARECER  ' TO GFCTNM-DESCR-SITUACAO

               WHEN 02
                   MOVE 'DEFERIDA            ' TO GFCTNM-DESCR-SITUACAO

               WHEN 03
                   MOVE 'INDEFERIDA          ' TO GFCTNM-DESCR-SITUACAO

               WHEN 04
                   MOVE 'INDEFERIDA POR PRAZO' TO GFCTNM-DESCR-SITUACAO

               WHEN 05
                   MOVE 'DEFERIDA CANCELADA  ' TO GFCTNM-DESCR-SITUACAO

               WHEN 06
                   MOVE 'PEDIDO CANCELADO    ' TO GFCTNM-DESCR-SITUACAO

               WHEN 08
                   MOVE 'ENCERRADA           ' TO GFCTNM-DESCR-SITUACAO

               WHEN 09
                   MOVE 'CANCELADA POR TRAG  ' TO GFCTNM-DESCR-SITUACAO

           END-EVALUATE.

           MOVE GFCTNM-QTDE-TOT-REG TO WRK-GFCTNM-QTDE-TOT-REG.
           ADD 1                       TO WRK-GFCTNM-QTDE-TOT-REG.
           MOVE WRK-GFCTNM-QTDE-TOT-REG TO GFCTNM-QTDE-TOT-REG.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA PESQUISAR NOME TARIFA.                             *
      ******************************************************************
      *----------------------------------------------------------------*
       2210-PESQUISAR-NOME-TARIFA      SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               SELECT RSERVC_TARIF_REDZD
               INTO  :GFCTB0D8.RSERVC-TARIF-REDZD
               FROM   DB2PRD.SERVC_TARIF_PRINC
               WHERE  CSERVC_TARIF     = :GFCTB0D8.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               INITIALIZE               GFCT0M-ERRO-SQL
               MOVE '0010'              TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE ' - GFCTB0D8'       TO WRK-NOME-TAB
               MOVE 'GFCT0884'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'SERVC_TARIF_PRINC' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'            TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0210'              TO GFCT0M-LOCAL
               MOVE  SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE  9                  TO GFCTNM-ERRO
               MOVE  SQLCODE            TO GFCTNM-COD-SQL-ERRO
               MOVE  SPACES             TO GFCTNM-DESC-MSG-ERRO
               MOVE  0010               TO GFCTNM-COD-MSG-ERRO
               PERFORM 2110-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE  1                 TO GFCTNM-ERRO
               MOVE  ZEROS             TO GFCTNM-COD-SQL-ERRO
               MOVE  0054              TO GFCTNM-COD-MSG-ERRO
               MOVE '0054'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTNM-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA LEITURA DA TABELA QUENTE.                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2300-LER-TAB-QUENTE             SECTION.
      *----------------------------------------------------------------*

           MOVE  GFCTNL-AGRUPMTO       TO CAGPTO-CTA        OF GFCTB049.
           MOVE  GFCTNL-TARIFA         TO CSERVC-TARIF      OF GFCTB049.
           MOVE  GFCTNL-TIMESTAMP-PED  TO HIDTFD-FLEXZ-AGPTO
                                                            OF GFCTB049.

           EXEC SQL
               SELECT
                   CSERVC_TARIF,
                   CAGPTO_CTA,
                   HIDTFD_FLEXZ_AGPTO,
                   DINIC_FLEXZ_AGPTO,
                   DFIM_FLEXZ_AGPTO,
                   QFRANQ_FLEXZ_AGPTO,
                   CPER_FRANQ_AGPTO,
                   CINDCD_AGPTO_TOT,
                   DDIA_AGNDA,
                   VNEGOC_FLEXZ_AGPTO,
                   PDESC_FLEXZ_AGPTO,
090708             QDIA_FLEXZ_AGPTO,
090708             CPER_COBR_TARIF,
090708             CINIC_PER_COBR
               INTO
                   :GFCTB049.CSERVC-TARIF,
                   :GFCTB049.CAGPTO-CTA,
                   :GFCTB049.HIDTFD-FLEXZ-AGPTO,
                   :GFCTB049.DINIC-FLEXZ-AGPTO,
                   :GFCTB049.DFIM-FLEXZ-AGPTO,
                   :GFCTB049.QFRANQ-FLEXZ-AGPTO,
                   :GFCTB049.CPER-FRANQ-AGPTO,
                   :GFCTB049.CINDCD-AGPTO-TOT,
                   :GFCTB049.DDIA-AGNDA,
                   :GFCTB049.VNEGOC-FLEXZ-AGPTO,
                   :GFCTB049.PDESC-FLEXZ-AGPTO,
090708             :GFCTB049.QDIA-FLEXZ-AGPTO,
090708             :GFCTB049.CPER-COBR-TARIF,
090708             :GFCTB049.CINIC-PER-COBR
             FROM   DB2PRD.FLEXZ_AGPTO_CTA
             WHERE
                   CSERVC_TARIF        = :GFCTB049.CSERVC-TARIF      AND
                   CAGPTO_CTA          = :GFCTB049.CAGPTO-CTA        AND
                   HIDTFD_FLEXZ_AGPTO  = :GFCTB049.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE ' - GFCTB049'      TO WRK-NOME-TAB
               MOVE 'GFCT0884'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'FLEXZ_AGPTO_CTA'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE '0220'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCTNM-ERRO
               MOVE  SQLCODE           TO GFCTNM-COD-SQL-ERRO
               MOVE  SPACES            TO GFCTNM-DESC-MSG-ERRO
               MOVE  0010              TO GFCTNM-COD-MSG-ERRO
               PERFORM 2110-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

           PERFORM 2320-PROCESSAR-TPO-AGPTO.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA PROCESSAR TIPO DE AGRUPAMENTO QUENTE               *
      ******************************************************************
      *----------------------------------------------------------------*
       2320-PROCESSAR-TPO-AGPTO        SECTION.
      *----------------------------------------------------------------*

           EVALUATE GFCTNL-AGRUPMTO
               WHEN 15
                   PERFORM 2321-SELECIONAR-PAB

               WHEN 14
                   IF  CINDCD-AGPTO-TOT    OF GFCTB049
                                           NOT EQUAL 'S'
                       PERFORM 2322-SELECIONAR-AGENCIA
                   ELSE
                       MOVE 'TODAS'        TO GFCTNM-DESCR-AGPTO
                       MOVE  ZEROS         TO GFCTNM-AGENCIA
                   END-IF

               WHEN 17
                   PERFORM 2323-SELECIONAR-POSTAL

               WHEN 4
                   PERFORM 2324-SELECIONAR-CLIENTE

               WHEN 9
                   PERFORM 2325-SELECIONAR-SEGMENTO

               WHEN 18
                   PERFORM 2326-SELECIONAR-MUNICIPIO

               WHEN 19
                   PERFORM 2327-SELECIONAR-UF

0310           WHEN 20
0310               PERFORM 2323-SELECIONAR-POSTAL
0310
0310           WHEN 26
0310               PERFORM 2328-SELECIONAR-RAZAO

090708         WHEN OTHER
090708             MOVE CAGPTO-CTA     OF GFCTB049
090708                                 TO CAGPTO-CTA OF GFCTB0F3
090708             PERFORM 2131-SELECIONAR-AGRUP

           END-EVALUATE.

           MOVE  DINIC-FLEXZ-AGPTO         OF GFCTB049
                                           TO GFCTNM-INIC-VIG-FLEX.
           MOVE  DFIM-FLEXZ-AGPTO          OF GFCTB049
                                           TO GFCTNM-FIM-VIG-FLEX.
           MOVE  QFRANQ-FLEXZ-AGPTO        OF GFCTB049
                                           TO GFCTNM-FRANQUIA.
           MOVE  CPER-FRANQ-AGPTO          OF GFCTB049
                                           TO GFCTNM-PERIODO.
           MOVE  VNEGOC-FLEXZ-AGPTO        OF GFCTB049
                                           TO GFCTNM-VR-NEGOCIADO.
           MOVE  PDESC-FLEXZ-AGPTO         OF GFCTB049
                                           TO GFCTNM-PERC-DESCTO.
           MOVE  DDIA-AGNDA                OF GFCTB049
                                           TO GFCTNM-DIA-AGEND.
           MOVE  QDIA-FLEXZ-AGPTO          OF GFCTB049
                                           TO GFCTNM-DIAS-RETENCAO.
090708     MOVE  CPER-COBR-TARIF           OF GFCTB049 TO
090708                                     GFCTNM-CPER-COBR-TARIF
090708     MOVE  CINIC-PER-COBR            OF GFCTB049 TO
090708                                     GFCTNM-CINIC-PER-COBR
           MOVE '.'                        TO GFCTNM-DATA-SOLIC(3:1)
                                              GFCTNM-DATA-SOLIC(6:1).
           MOVE  HIDTFD-FLEXZ-AGPTO        OF GFCTB049(1:4)
                                           TO GFCTNM-DATA-SOLIC(7:4).
           MOVE  HIDTFD-FLEXZ-AGPTO        OF GFCTB049(6:2)
                                           TO GFCTNM-DATA-SOLIC(4:2).
           MOVE  HIDTFD-FLEXZ-AGPTO        OF GFCTB049(9:2)
                                           TO GFCTNM-DATA-SOLIC(1:2).

      *----------------------------------------------------------------*
       2320-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA SELECIONAR PAB.                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2321-SELECIONAR-PAB             SECTION.
      *----------------------------------------------------------------*

           MOVE  CSERVC-TARIF          OF GFCTB049
                                       TO CSERVC-TARIF     OF GFCTB054.
           MOVE  CAGPTO-CTA            OF GFCTB049
                                       TO CAGPTO-CTA       OF GFCTB054.
           MOVE  HIDTFD-FLEXZ-AGPTO    OF GFCTB049
                                       TO HIDTFD-FLEXZ-AGPTO
                                                           OF GFCTB054.

           EXEC SQL
               SELECT
                   CDEPDC,
                   CPOSTO_SERVC
               INTO
                   :GFCTB054.CDEPDC,
                   :GFCTB054.CPOSTO-SERVC
               FROM DB2PRD.FLEXZ_GRP_PAB
               WHERE
                   CSERVC_TARIF        = :GFCTB054.CSERVC-TARIF      AND
                   CAGPTO_CTA          = :GFCTB054.CAGPTO-CTA        AND
                   HIDTFD_FLEXZ_AGPTO  = :GFCTB054.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               INITIALIZE               GFCT0M-ERRO-SQL
               MOVE '0010'              TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE ' - GFCTB054'       TO WRK-NOME-TAB
               MOVE 'GFCT0884'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'FLEXZ_GRP_PAB'     TO GFCT0M-NOME-TAB
               MOVE 'SELECT'            TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0230'              TO GFCT0M-LOCAL
               MOVE  SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE  9                  TO GFCTNM-ERRO
               MOVE  SQLCODE            TO GFCTNM-COD-SQL-ERRO
               MOVE  SPACES             TO GFCTNM-DESC-MSG-ERRO
               MOVE  0010               TO GFCTNM-COD-MSG-ERRO
               PERFORM 2110-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE  1                 TO GFCTNM-ERRO
               MOVE  ZEROS             TO GFCTNM-COD-SQL-ERRO
               MOVE  0926              TO GFCTNM-COD-MSG-ERRO
               MOVE '0926'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTNM-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE  SPACES                        TO GFCTGB-ENTRADA.
           MOVE  100                           TO GFCTGB-LL.
           MOVE  ZEROS                         TO GFCTGB-ZZ.
           MOVE  GFCTNL-TRANSACAO              TO GFCTGB-TRANSACAO.
           MOVE  GFCTNL-FUNCAO                 TO GFCTGB-FUNCAO.
           MOVE  ZEROS                         TO GFCTGB-QTDE-OCOR
                                                  GFCTGB-QTDE-TOT-REG.
           MOVE  GFCTNL-FUNC-BDSCO             TO GFCTGB-FUNC-BDSCO.
           MOVE 'N'                            TO GFCTGB-FIM.
           MOVE  CDEPDC       OF GFCTB054      TO GFCTGB-COD-DEPDC.
           MOVE  CPOSTO-SERVC OF GFCTB054      TO GFCTGB-COD-POSTO.
           MOVE  SPACES                        TO GFCTGC-SAIDA
                                                  GFCT0M-AREA-ERROS.
           MOVE 'GFCT5109'                     TO WRK-MODULO.

           INITIALIZE GFCTGC-SAIDA
                      GFCT0M-AREA-ERROS.

           CALL WRK-MODULO USING
                                                     GFCTGB-ENTRADA
                                                     GFCTGC-SAIDA
                                                     GFCT0M-AREA-ERROS.

           IF  RETURN-CODE                     NOT EQUAL ZEROS
               MOVE 9                          TO GFCTNM-ERRO
               MOVE WRK-MODULO                 TO WRK-NOME-MOD
                                                  GFCT0M-PROGRAMA
               MOVE WRK-MSG01                  TO GFCT0M-TEXTO
               MOVE 'APL'                      TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0884'                 TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTGC-ERRO                     NOT EQUAL ZEROS
020908**       MOVE 'S'                        TO GFCTNM-FIM
020908**       MOVE  GFCTGC-ERRO               TO GFCTNM-ERRO
020908**       MOVE  GFCTGC-COD-SQL-ERRO       TO GFCTNM-COD-SQL-ERRO
020908**       MOVE  GFCTGC-COD-MSG-ERRO       TO GFCTNM-COD-MSG-ERRO
               IF  GFCTGC-ERRO                 EQUAL 1
020908**           MOVE  GFCTGC-DESC-MSG-ERRO  TO GFCTNM-DESC-MSG-ERRO
020908             MOVE  GFCTGC-DESC-MSG-ERRO  TO GFCTGC-DESC-POSTO(1)
               ELSE
                   STRING 'GFCT5109- ' GFCTGC-DESC-MSG-ERRO
                           DELIMITED BY SIZE   INTO GFCTNM-DESC-MSG-ERRO
020908             MOVE 'S'                    TO GFCTNM-FIM
020908             MOVE  GFCTGC-ERRO           TO GFCTNM-ERRO
020908             MOVE  GFCTGC-COD-SQL-ERRO   TO GFCTNM-COD-SQL-ERRO
020908             MOVE  GFCTGC-COD-MSG-ERRO   TO GFCTNM-COD-MSG-ERRO
020908             PERFORM 3000-FINALIZAR
               END-IF
020908**       PERFORM 3000-FINALIZAR
           END-IF.

           MOVE  GFCTGC-DESC-POSTO(1)      TO GFCTNM-DESCR-AGPTO.
           MOVE  CDEPDC       OF GFCTB054  TO GFCTNM-AGENCIA.
           MOVE  CPOSTO-SERVC OF GFCTB054  TO GFCTNM-POSTO.

      *----------------------------------------------------------------*
       2321-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA SELECIONAR AGENCIA.                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2322-SELECIONAR-AGENCIA         SECTION.
      *----------------------------------------------------------------*

           MOVE  CSERVC-TARIF          OF GFCTB049
                                       TO CSERVC-TARIF      OF GFCTB051.
           MOVE  CAGPTO-CTA            OF GFCTB049
                                       TO CAGPTO-CTA        OF GFCTB051.
           MOVE  HIDTFD-FLEXZ-AGPTO    OF GFCTB049
                                       TO HIDTFD-FLEXZ-AGPTO
                                                            OF GFCTB051.
           EXEC SQL
               SELECT CDEPDC
               INTO  :GFCTB051.CDEPDC
               FROM   DB2PRD.FLEXZ_GRP_DEPDC
               WHERE
                   CSERVC_TARIF        = :GFCTB051.CSERVC-TARIF      AND
                   CAGPTO_CTA          = :GFCTB051.CAGPTO-CTA        AND
                   HIDTFD_FLEXZ_AGPTO  = :GFCTB051.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE ' - GFCTB051'      TO WRK-NOME-TAB
               MOVE 'GFCT0884'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'FLEXZ_GRP_DEPDC'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE '0240'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCTNM-ERRO
               MOVE  SQLCODE           TO GFCTNM-COD-SQL-ERRO
               MOVE  SPACES            TO GFCTNM-DESC-MSG-ERRO
               MOVE  0010              TO GFCTNM-COD-MSG-ERRO
               PERFORM 2110-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE  1                 TO GFCTNM-ERRO
               MOVE  ZEROS             TO GFCTNM-COD-SQL-ERRO
               MOVE  0926              TO GFCTNM-COD-MSG-ERRO
               MOVE '0926'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTNM-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE  SPACES                      TO GFCT1V-ENTRADA.
           MOVE  660                         TO GFCT1V-LL.
           MOVE  ZEROS                       TO GFCT1V-ZZ.
           MOVE  GFCTNL-TRANSACAO            TO GFCT1V-TRANSACAO.
           MOVE  GFCTNL-FUNCAO               TO GFCT1V-FUNCAO.
           MOVE  001                         TO GFCT1V-QTDE-OCOR
           MOVE  ZEROS                       TO GFCT1V-QTDE-TOT-REG.
           MOVE  GFCTNL-FUNC-BDSCO           TO GFCT1V-FUNC-BDSCO.
           MOVE 'N'                          TO GFCT1V-FIM.
           MOVE  CDEPDC OF GFCTB051          TO GFCT1V-COD-DEPDC(1).
           MOVE  SPACES                      TO GFCT1X-SAIDA
                                                GFCT0M-AREA-ERROS.
           MOVE 'GFCT5006'                   TO WRK-MODULO.

           INITIALIZE GFCT1X-SAIDA
                      GFCT0M-AREA-ERROS.

           CALL WRK-MODULO USING
                                                     GFCT1V-ENTRADA
                                                     GFCT1X-SAIDA
                                                     GFCT0M-AREA-ERROS.

           IF  RETURN-CODE                     NOT EQUAL ZEROS
               MOVE 9                          TO GFCTNM-ERRO
               MOVE WRK-MODULO                 TO WRK-NOME-MOD
                                                  GFCT0M-PROGRAMA
               MOVE WRK-MSG01                  TO GFCT0M-TEXTO
               MOVE 'APL'                      TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0884'                 TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCT1X-ERRO                     NOT EQUAL ZEROS
               MOVE 'S'                        TO GFCTNM-FIM
               MOVE  GFCT1X-ERRO               TO GFCTNM-ERRO
               MOVE  GFCT1X-COD-SQL-ERRO       TO GFCTNM-COD-SQL-ERRO
               MOVE  GFCT1X-COD-MSG-ERRO       TO GFCTNM-COD-MSG-ERRO
               IF  GFCT1X-ERRO                 EQUAL 1
                   MOVE  GFCT1X-DESC-MSG-ERRO  TO GFCTNM-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5006- ' GFCT1X-DESC-MSG-ERRO
                           DELIMITED BY SIZE   INTO GFCTNM-DESC-MSG-ERRO
               END-IF
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE  GFCT1X-DESC-DEPDC(1)      TO GFCTNM-DESCR-AGPTO.
           MOVE  CDEPDC OF GFCTB051        TO GFCTNM-AGENCIA.

      *----------------------------------------------------------------*
       2322-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA SELECIONAR POSTAL.                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2323-SELECIONAR-POSTAL          SECTION.
      *----------------------------------------------------------------*

           MOVE  CSERVC-TARIF          OF GFCTB049
                                       TO CSERVC-TARIF      OF GFCTB055.
           MOVE  CAGPTO-CTA            OF GFCTB049
                                       TO CAGPTO-CTA        OF GFCTB055.
           MOVE  HIDTFD-FLEXZ-AGPTO    OF GFCTB049
                                       TO HIDTFD-FLEXZ-AGPTO
                                                            OF GFCTB055.
           EXEC SQL
               SELECT
                   CDEPDC,
                   CPOSTO_SERVC
               INTO
                   :GFCTB055.CDEPDC,
                   :GFCTB055.CPOSTO-SERVC
               FROM DB2PRD.FLEXZ_GRP_PSTAL
               WHERE
                   CSERVC_TARIF        = :GFCTB055.CSERVC-TARIF      AND
                   CAGPTO_CTA          = :GFCTB055.CAGPTO-CTA        AND
                   HIDTFD_FLEXZ_AGPTO  = :GFCTB055.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               INITIALIZE               GFCT0M-ERRO-SQL
               MOVE '0010'              TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE ' - GFCTB055'       TO WRK-NOME-TAB
               MOVE 'GFCT0884'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'FLEXZ_GRP_PSTAL'   TO GFCT0M-NOME-TAB
               MOVE 'SELECT'            TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0250'              TO GFCT0M-LOCAL
               MOVE  SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE  9                  TO GFCTNM-ERRO
               MOVE  SQLCODE            TO GFCTNM-COD-SQL-ERRO
               MOVE  SPACES             TO GFCTNM-DESC-MSG-ERRO
               MOVE  0010               TO GFCTNM-COD-MSG-ERRO
               PERFORM 2110-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE  1                 TO GFCTNM-ERRO
               MOVE  ZEROS             TO GFCTNM-COD-SQL-ERRO
               MOVE  0926              TO GFCTNM-COD-MSG-ERRO
               MOVE '0926'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTNM-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE  SPACES                        TO GFCTGB-ENTRADA.
           MOVE  100                           TO GFCTGB-LL.
           MOVE  ZEROS                         TO GFCTGB-ZZ.
           MOVE  GFCTNL-TRANSACAO              TO GFCTGB-TRANSACAO.
           MOVE  GFCTNL-FUNCAO                 TO GFCTGB-FUNCAO.
           MOVE  ZEROS                         TO GFCTGB-QTDE-OCOR
                                                  GFCTGB-QTDE-TOT-REG.
           MOVE  GFCTNL-FUNC-BDSCO             TO GFCTGB-FUNC-BDSCO.
           MOVE 'N'                            TO GFCTGB-FIM.
           MOVE  CDEPDC       OF GFCTB055      TO GFCTGB-COD-DEPDC.
           MOVE  CPOSTO-SERVC OF GFCTB055      TO GFCTGB-COD-POSTO.
           MOVE  SPACES                        TO GFCTGC-SAIDA
                                                  GFCT0M-AREA-ERROS.
           MOVE 'GFCT5090'                     TO WRK-MODULO.

           INITIALIZE GFCTGC-SAIDA
                      GFCT0M-AREA-ERROS.

           CALL WRK-MODULO USING
                                                     GFCTGB-ENTRADA
                                                     GFCTGC-SAIDA
                                                     GFCT0M-AREA-ERROS.

           IF  RETURN-CODE                     NOT EQUAL ZEROS
               MOVE 9                          TO GFCTNM-ERRO
               MOVE WRK-MODULO                 TO WRK-NOME-MOD
                                                  GFCT0M-PROGRAMA
               MOVE WRK-MSG01                  TO GFCT0M-TEXTO
               MOVE 'APL'                      TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0884'                 TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTGC-ERRO                     NOT EQUAL ZEROS
               MOVE 'S'                        TO GFCTNM-FIM
               MOVE  GFCTGC-ERRO               TO GFCTNM-ERRO
               MOVE  GFCTGC-COD-SQL-ERRO       TO GFCTNM-COD-SQL-ERRO
               MOVE  GFCTGC-COD-MSG-ERRO       TO GFCTNM-COD-MSG-ERRO
               IF  GFCTGC-ERRO                 EQUAL 1
                   MOVE  GFCTGC-DESC-MSG-ERRO  TO GFCTNM-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5090- ' GFCTGC-DESC-MSG-ERRO
                           DELIMITED BY SIZE   INTO GFCTNM-DESC-MSG-ERRO
               END-IF
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE  GFCTGC-DESC-POSTO(1)      TO GFCTNM-DESCR-AGPTO.
           MOVE  CDEPDC       OF GFCTB055  TO GFCTNM-AGENCIA.
           MOVE  CPOSTO-SERVC OF GFCTB055  TO GFCTNM-POSTO.

      *----------------------------------------------------------------*
       2323-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA SELECIONAR CLIENTE.                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2324-SELECIONAR-CLIENTE         SECTION.
      *----------------------------------------------------------------*

           MOVE  CSERVC-TARIF          OF GFCTB049
                                       TO CSERVC-TARIF      OF GFCTB050.
           MOVE  CAGPTO-CTA            OF GFCTB049
                                       TO CAGPTO-CTA        OF GFCTB050.
           MOVE  HIDTFD-FLEXZ-AGPTO    OF GFCTB049
                                       TO HIDTFD-FLEXZ-AGPTO
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
               FROM DB2PRD.FLEXZ_GRP_CLI
               WHERE
                   CSERVC_TARIF        = :GFCTB050.CSERVC-TARIF      AND
                   CAGPTO_CTA          = :GFCTB050.CAGPTO-CTA        AND
                   HIDTFD_FLEXZ_AGPTO  = :GFCTB050.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               INITIALIZE               GFCT0M-ERRO-SQL
               MOVE '0010'              TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE ' - GFCTB050'       TO WRK-NOME-TAB
               MOVE 'GFCT0884'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'FLEXZ_GRP_CLI'     TO GFCT0M-NOME-TAB
               MOVE 'SELECT'            TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0260'              TO GFCT0M-LOCAL
               MOVE  SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE  9                  TO GFCTNM-ERRO
               MOVE  SQLCODE            TO GFCTNM-COD-SQL-ERRO
               MOVE  SPACES             TO GFCTNM-DESC-MSG-ERRO
               MOVE  0010               TO GFCTNM-COD-MSG-ERRO
               PERFORM 2110-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE  1                 TO GFCTNM-ERRO
               MOVE  ZEROS             TO GFCTNM-COD-SQL-ERRO
               MOVE  0926              TO GFCTNM-COD-MSG-ERRO
               MOVE '0926'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTNM-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE  SPACES                    TO GFCTNM-DESCR-AGPTO.
ST2506*    MOVE  CCGC-CPF      OF GFCTB050 TO GFCTNM-CGC-CPF.
ST2506*    MOVE  CFLIAL-CGC    OF GFCTB050 TO GFCTNM-FILIAL.
ST2506*    MOVE  CCTRL-CPF-CGC OF GFCTB050 TO GFCTNM-CONTROLE.
ST2506     MOVE  CCGC-CPF-ST      OF GFCTB050 TO GFCTNM-CGC-CPF.
ST2506     MOVE  CFLIAL-CGC-ST    OF GFCTB050 TO GFCTNM-FILIAL.
ST2506     MOVE  CCTRL-CPF-CGC-ST OF GFCTB050 TO GFCTNM-CONTROLE.

      *----------------------------------------------------------------*
       2324-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA SELECIONAR SEGMENTO.                               *
      ******************************************************************
      *----------------------------------------------------------------*
       2325-SELECIONAR-SEGMENTO        SECTION.
      *----------------------------------------------------------------*

           MOVE  CSERVC-TARIF          OF GFCTB049
                                       TO CSERVC-TARIF      OF GFCTB056.
           MOVE  CAGPTO-CTA            OF GFCTB049
                                       TO CAGPTO-CTA        OF GFCTB056.
           MOVE  HIDTFD-FLEXZ-AGPTO    OF GFCTB049
                                       TO HIDTFD-FLEXZ-AGPTO
                                                            OF GFCTB056.

           EXEC SQL
               SELECT CSGMTO_GSTAO_TARIF
               INTO  :GFCTB056.CSGMTO-GSTAO-TARIF
               FROM   DB2PRD.FLEXZ_GRP_SGMTO
               WHERE
                   CSERVC_TARIF        = :GFCTB056.CSERVC-TARIF      AND
                   CAGPTO_CTA          = :GFCTB056.CAGPTO-CTA        AND
                   HIDTFD_FLEXZ_AGPTO  = :GFCTB056.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               INITIALIZE               GFCT0M-ERRO-SQL
               MOVE '0010'              TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE ' - GFCTB056'       TO WRK-NOME-TAB
               MOVE 'GFCT0884'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'FLEXZ_GRP_SGMTO'   TO GFCT0M-NOME-TAB
               MOVE 'SELECT'            TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0270'              TO GFCT0M-LOCAL
               MOVE  SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE  9                  TO GFCTNM-ERRO
               MOVE  SQLCODE            TO GFCTNM-COD-SQL-ERRO
               MOVE  SPACES             TO GFCTNM-DESC-MSG-ERRO
               MOVE  0010               TO GFCTNM-COD-MSG-ERRO
               PERFORM 2110-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE  1                 TO GFCTNM-ERRO
               MOVE  ZEROS             TO GFCTNM-COD-SQL-ERRO
               MOVE  0926              TO GFCTNM-COD-MSG-ERRO
               MOVE '0926'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTNM-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE  CSGMTO-GSTAO-TARIF   OF GFCTB056
                                      TO CSGMTO-GSTAO-TARIF OF GFCTB0H5.

           EXEC SQL
               SELECT ISGMTO_GSTAO_TARIF
               INTO  :GFCTB0H5.ISGMTO-GSTAO-TARIF
               FROM  DB2PRD.TTPO_SGMTO_GSTAO
               WHERE CSGMTO_GSTAO_TARIF = :GFCTB0H5.CSGMTO-GSTAO-TARIF
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               INITIALIZE               GFCT0M-ERRO-SQL
               MOVE '0010'              TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE ' - GFCTB0H5'       TO WRK-NOME-TAB
               MOVE 'GFCT0884'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'TTPO_SGMTO_GSTAO'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'            TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0280'              TO GFCT0M-LOCAL
               MOVE  SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE  9                  TO GFCTNM-ERRO
               MOVE  SQLCODE            TO GFCTNM-COD-SQL-ERRO
               MOVE  SPACES             TO GFCTNM-DESC-MSG-ERRO
               MOVE  0010               TO GFCTNM-COD-MSG-ERRO
               PERFORM 2110-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE  1                 TO GFCTNM-ERRO
               MOVE  ZEROS             TO GFCTNM-COD-SQL-ERRO
               MOVE  0500              TO GFCTNM-COD-MSG-ERRO
               MOVE '0500'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTNM-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE  ISGMTO-GSTAO-TARIF    OF GFCTB0H5
                                       TO GFCTNM-DESCR-AGPTO.
           MOVE  CSGMTO-GSTAO-TARIF    OF GFCTB056
                                       TO GFCTNM-SEGMENTO.

      *----------------------------------------------------------------*
       2325-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA SELECIONAR MUNICIPIO.                              *
      ******************************************************************
      *----------------------------------------------------------------*
       2326-SELECIONAR-MUNICIPIO       SECTION.
      *----------------------------------------------------------------*

           MOVE  CSERVC-TARIF          OF GFCTB049
                                       TO CSERVC-TARIF      OF GFCTB053.
           MOVE  CAGPTO-CTA            OF GFCTB049
                                       TO CAGPTO-CTA        OF GFCTB053.
           MOVE  HIDTFD-FLEXZ-AGPTO    OF GFCTB049
                                       TO HIDTFD-FLEXZ-AGPTO
                                                            OF GFCTB053.

           EXEC SQL
               SELECT CMUN_IBGE
               INTO  :GFCTB053.CMUN-IBGE
               FROM   DB2PRD.FLEXZ_GRP_MUN
               WHERE
                   CSERVC_TARIF        = :GFCTB053.CSERVC-TARIF      AND
                   CAGPTO_CTA          = :GFCTB053.CAGPTO-CTA        AND
                   HIDTFD_FLEXZ_AGPTO  = :GFCTB053.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               INITIALIZE               GFCT0M-ERRO-SQL
               MOVE '0010'              TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE ' - GFCTB053'       TO WRK-NOME-TAB
               MOVE 'GFCT0884'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'FLEXZ_GRP_MUN'     TO GFCT0M-NOME-TAB
               MOVE 'SELECT'            TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0290'              TO GFCT0M-LOCAL
               MOVE  SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE  9                  TO GFCTNM-ERRO
               MOVE  SQLCODE            TO GFCTNM-COD-SQL-ERRO
               MOVE  SPACES             TO GFCTNM-DESC-MSG-ERRO
               MOVE  0010               TO GFCTNM-COD-MSG-ERRO
               PERFORM 2110-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE  1                 TO GFCTNM-ERRO
               MOVE  ZEROS             TO GFCTNM-COD-SQL-ERRO
               MOVE  0926              TO GFCTNM-COD-MSG-ERRO
               MOVE '0926'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTNM-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           INITIALIZE AREA-RURC88.

           MOVE  CMUN-IBGE OF GFCTB053 TO COD-MUNIC-RURC88.
           MOVE 'RURC9020'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                             AREA-RURC88.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE  1                 TO GFCTNM-ERRO
               MOVE  0732              TO GFCTNM-COD-MSG-ERRO
               MOVE '0732'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTNM-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           EVALUATE COD-RETORNO-RURC88
               WHEN 10
                   MOVE  1              TO GFCTNM-ERRO
                   MOVE  ZEROS          TO GFCTNM-COD-SQL-ERRO
                   MOVE  0675           TO GFCTNM-COD-MSG-ERRO
                   MOVE '0675'          TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESC-MENSAGEM
                   MOVE GFCTG3-DESC-MSG TO GFCTNM-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR

               WHEN 20
                   MOVE  1              TO GFCTNM-ERRO
                   MOVE  ZEROS          TO GFCTNM-COD-SQL-ERRO
                   MOVE  0733           TO GFCTNM-COD-MSG-ERRO
                   MOVE '0733'          TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESC-MENSAGEM
                   MOVE GFCTG3-DESC-MSG TO GFCTNM-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR

               WHEN 99
                   MOVE  1              TO GFCTNM-ERRO
                   MOVE  ZEROS          TO GFCTNM-COD-SQL-ERRO
                   MOVE  0734           TO GFCTNM-COD-MSG-ERRO
                   MOVE '0734'          TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESC-MENSAGEM
                   MOVE GFCTG3-DESC-MSG TO GFCTNM-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR

           END-EVALUATE.

           MOVE  NOME-MUNIC-RURC88     TO GFCTNM-DESCR-AGPTO.
           MOVE  CMUN-IBGE OF GFCTB053 TO GFCTNM-MUNICIPIO.

      *----------------------------------------------------------------*
       2326-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA SELECIONAR UF.                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2327-SELECIONAR-UF              SECTION.
      *----------------------------------------------------------------*

           MOVE  CSERVC-TARIF          OF GFCTB049
                                       TO CSERVC-TARIF      OF GFCTB057.
           MOVE  CAGPTO-CTA            OF GFCTB049
                                       TO CAGPTO-CTA        OF GFCTB057.
           MOVE  HIDTFD-FLEXZ-AGPTO    OF GFCTB049
                                       TO HIDTFD-FLEXZ-AGPTO
                                                            OF GFCTB057.

           EXEC SQL
               SELECT CSGL_UF
               INTO  :GFCTB057.CSGL-UF
               FROM   DB2PRD.FLEXZ_GRP_UF
               WHERE
                   CSERVC_TARIF        = :GFCTB057.CSERVC-TARIF      AND
                   CAGPTO_CTA          = :GFCTB057.CAGPTO-CTA        AND
                   HIDTFD_FLEXZ_AGPTO = :GFCTB057.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               INITIALIZE               GFCT0M-ERRO-SQL
               MOVE '0010'              TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE ' - GFCTB057'       TO WRK-NOME-TAB
               MOVE 'GFCT0884'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'FLEXZ_GRP_UF'      TO GFCT0M-NOME-TAB
               MOVE 'SELECT'            TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0300'              TO GFCT0M-LOCAL
               MOVE  SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE  9                  TO GFCTNM-ERRO
               MOVE  SQLCODE            TO GFCTNM-COD-SQL-ERRO
               MOVE  SPACES             TO GFCTNM-DESC-MSG-ERRO
               MOVE  0010               TO GFCTNM-COD-MSG-ERRO
               PERFORM 2110-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE  1                 TO GFCTNM-ERRO
               MOVE  ZEROS             TO GFCTNM-COD-SQL-ERRO
               MOVE  0926              TO GFCTNM-COD-MSG-ERRO
               MOVE '0926'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTNM-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE  CSGL-UF   OF GFCTB057 TO CSGL-UF          OF DCITV002.
           MOVE  DCITV002              TO WRK-AREA-DCLGEN-DCITV002.
           MOVE 'DCIT8000'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                             WRK-DCIT-UNIDADE-FEDERATIVA
                                             WRK-POOL7100
                                             WRK-SQLCA.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE  1                 TO GFCTNM-ERRO
               MOVE  ZEROS             TO GFCTNM-COD-SQL-ERRO
               MOVE  0735              TO GFCTNM-COD-MSG-ERRO
               MOVE '0735'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTNM-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           EVALUATE WRK-DCIT-CODIGO-RETORNO
               WHEN 1
                   MOVE  1              TO GFCTNM-ERRO
                   MOVE  ZEROS          TO GFCTNM-COD-SQL-ERRO
                   MOVE  0736           TO GFCTNM-COD-MSG-ERRO
                   MOVE '0736'          TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESC-MENSAGEM
                   MOVE GFCTG3-DESC-MSG TO GFCTNM-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR

               WHEN 2
                   MOVE  1              TO GFCTNM-ERRO
                   MOVE  ZEROS          TO GFCTNM-COD-SQL-ERRO
                   MOVE  0737           TO GFCTNM-COD-MSG-ERRO
                   MOVE '0737'          TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESC-MENSAGEM
                   MOVE GFCTG3-DESC-MSG TO GFCTNM-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR

               WHEN 3
                   MOVE  1              TO GFCTNM-ERRO
                   MOVE  ZEROS          TO GFCTNM-COD-SQL-ERRO
                   MOVE  0738           TO GFCTNM-COD-MSG-ERRO
                   MOVE '0738'          TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESC-MENSAGEM
                   MOVE GFCTG3-DESC-MSG TO GFCTNM-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR

               WHEN 4
                   INITIALIZE               GFCT0M-ERRO-SQL
                   MOVE '0005'              TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESC-MENSAGEM
                   MOVE ' - DCITV002'       TO WRK-NOME-TAB
                   MOVE 'GFCT0884'          TO GFCT0M-PROGRAMA
                   MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
                   MOVE 'V01_UF'            TO GFCT0M-NOME-TAB
                   MOVE 'SELECT'            TO GFCT0M-COMANDO-SQL
                   MOVE  SQLCODE            TO GFCT0M-SQLCODE
                   MOVE '0310'              TO GFCT0M-LOCAL
                   MOVE  SQLCA              TO GFCT0M-SQLCA-AREA
                   MOVE  9                  TO GFCTNM-ERRO
                   MOVE  SQLCODE            TO GFCTNM-COD-SQL-ERRO
                   MOVE  SPACES             TO GFCTNM-DESC-MSG-ERRO
                   MOVE  0005               TO GFCTNM-COD-MSG-ERRO
                   PERFORM 2110-CONCATENAR-MENSAGEM
                   PERFORM 3000-FINALIZAR

           END-EVALUATE.

           MOVE  WRK-AREA-DCLGEN-DCITV002  TO DCITV002.
           MOVE  IUF     OF DCITV002       TO GFCTNM-DESCR-AGPTO.
           MOVE  CSGL-UF OF GFCTB057       TO GFCTNM-UF.

      *----------------------------------------------------------------*
       2327-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

0310  ******************************************************************
0310  * ROTINA PARA SELECIONAR RAZAO                                   *
0310  ******************************************************************
0310  *----------------------------------------------------------------*
0310   2328-SELECIONAR-RAZAO           SECTION.
0310  *----------------------------------------------------------------*
0310
0310       MOVE  CSERVC-TARIF          OF GFCTB049
0310                                   TO CSERVC-TARIF       OF GFCTB0M8
0310       MOVE  CAGPTO-CTA            OF GFCTB049
0310                                   TO CAGPTO-CTA         OF GFCTB0M8
0310       MOVE  HIDTFD-FLEXZ-AGPTO    OF GFCTB049
0310                                   TO HIDTFD-FLEXZ-AGPTO OF GFCTB0M8
0310       EXEC SQL
0310           SELECT
0310               CGRP_CTBIL_ATUAL,
0310               CSGRP_CTBIL
0310           INTO
0310               :GFCTB0M8.CGRP-CTBIL-ATUAL,
0310               :GFCTB0M8.CSGRP-CTBIL
0310           FROM DB2PRD.TFLEXZ_GRP_RZ
0310           WHERE
0310               CSERVC_TARIF        = :GFCTB0M8.CSERVC-TARIF      AND
0310               CAGPTO_CTA          = :GFCTB0M8.CAGPTO-CTA        AND
0310               HIDTFD_FLEXZ_AGPTO  = :GFCTB0M8.HIDTFD-FLEXZ-AGPTO
0310       END-EXEC.
0310
0310       IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
0310          (SQLWARN0                 EQUAL 'W')
0310           INITIALIZE               GFCT0M-ERRO-SQL
0310           MOVE '0010'              TO GFCTG2-COD-MSG
0310           PERFORM 1310-OBTER-DESC-MENSAGEM
0310           MOVE 'GFCTB0M8'          TO WRK-NOME-TAB
0310           MOVE 'GFCT0884'          TO GFCT0M-PROGRAMA
0310           MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
0310           MOVE 'TFLEXZ_GRP_RZ  '   TO GFCT0M-NOME-TAB
0310           MOVE 'SELECT'            TO GFCT0M-COMANDO-SQL
0310           MOVE  SQLCODE            TO GFCT0M-SQLCODE
0310           MOVE '0250'              TO GFCT0M-LOCAL
0310           MOVE  SQLCA              TO GFCT0M-SQLCA-AREA
0310           MOVE  9                  TO GFCTNM-ERRO
0310           MOVE  SQLCODE            TO WRK-COD-SQL-ERRO
0310           MOVE  WRK-COD-SQL-ERRO-2 TO GFCTNM-COD-SQL-ERRO
0310           MOVE  SPACES             TO GFCTNM-DESC-MSG-ERRO
0310           MOVE  0010               TO GFCTNM-COD-MSG-ERRO
0310           PERFORM 2110-CONCATENAR-MENSAGEM
0310           PERFORM 3000-FINALIZAR
0310       END-IF.
0310
0310       IF  SQLCODE                 EQUAL +100
0310           MOVE  1                 TO GFCTNM-ERRO
0310           MOVE  ZEROS             TO GFCTNM-COD-SQL-ERRO
0310           MOVE  0926              TO GFCTNM-COD-MSG-ERRO
0310           MOVE '0926'             TO GFCTG2-COD-MSG
0310           PERFORM 1310-OBTER-DESC-MENSAGEM
0310           MOVE GFCTG3-DESC-MSG    TO GFCTNM-DESC-MSG-ERRO
0310           PERFORM 3000-FINALIZAR
0310       END-IF.
0310
0310       MOVE  CGRP-CTBIL-ATUAL OF GFCTB0M8 TO GFCTNM-GRUPO-CTBIL
0310       MOVE  CSGRP-CTBIL OF GFCTB0M8 TO GFCTNM-SUB-GRUPO-CTBIL.
0310
0310  *----------------------------------------------------------------*
0310   2328-99-FIM.                    EXIT.
0310  *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA FINALIZAR PROCESSAMENTO                            *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
