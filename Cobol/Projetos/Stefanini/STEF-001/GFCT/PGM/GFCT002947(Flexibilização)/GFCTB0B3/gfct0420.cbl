      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT0420.
       AUTHOR.     FERNANDO BATTISTI.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT0420                                    *
      *    PROGRAMADOR.:   FERNANDO BATTISTI       - CPM PATO BRANCO   *
      *    ANALISTA CPM:   JOSSIANE CARLA GAZZONI  - CPM PATO BRANCO   *
      *    ANALISTA....:   VALERIA                 - PROCWORK /GP.75   *
      *    DATA........:   05/05/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   FLEXIBILIZACAO - LISTA DE CANCELAMENTO      *
      *      DE FLEXIBILIZACAO PARA AGRUPAMENTO.                       *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                        INCLUDE/BOOK     *
      *                   DB2PRD.PARM_DATA_PROCM        GFCTB0A1       *
      *                   DB2PRD.PDIDO_FLEXZ_AGPTO      GFCTB0B2       *
      *                   DB2PRD.PDIDO_FLEXZ_CLI        GFCTB0B3       *
      *                   DB2PRD.PDIDO_FLEXZ_AG         GFCTB0B1       *
      *                   DB2PRD.PDIDO_FLEXZ_SGMTO      GFCTB0C0       *
      *                   DB2PRD.PDIDO_FLEXZ_PAB        GFCTB0B8       *
      *                   DB2PRD.PDIDO_FLEXZ_PSTAL      GFCTB0B9       *
      *                   DB2PRD.PDIDO_FLEXZ_MUN        GFCTB0B7       *
      *                   DB2PRD.PDIDO_FLEXZ_UF         GFCTB0C1       *
      *                   DB2PRD.TTPO_SGMTO_GSTAO       GFCTB0H5       *
      *                   DB2PRD.V01UF                  DCITV002       *
      *                   DB2PRD.SERVC_TARIFA_PRINC     GFCTB0D8       *
110510*                   DB2PRD.TPDIDO_FLEXZ_RZ        GFCTB0M7       *
090610*                   DB2PRD.RAZAO_CONTABIL         PLCCB004       *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#GFCTND - AREA DE COMUNICACAO - ENTRADA                    *
      *    I#GFCTNE - AREA DE COMUNICACAO - SAIDA                      *
      *    I#GFCTG2 - AREA DE COMUNICACAO - ENTRADA DO MODULO GFCT5523 *
      *    I#GFCTG3 - AREA DE COMUNICACAO - SAIDA DO MODULO GFCT5523   *
      *    I#GFCTFZ - AREA DE COMUNICACAO - ENTRADA DO MODULO GFCT5522 *
      *    I#GFCTG1 - AREA DE COMUNICACAO - SAIDA DO MODULO GFCT5522   *
      *    I#GFCT0M - AREA DE COMUNICACAO - ERROS                      *
      *    I#RURC88 - AREA PARA MODULO RURC9020                        *
      *    I#GFCT1V - AREA DE COMUNICACAO - ENTRADA-GFCT5006           *
      *    I#GFCT1X - AREA DE COMUNICACAO - SAIDA  -GFCT5006           *
      *    I#GFCTGB - AREA DE COMUNICACAO - ENTRADA-GFCT5109 GFCTB5090 *
      *    I#GFCTGC - AREA DE COMUNICACAO - SAIDA  -GFCT5109 GFCT5090  *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    GFCT5522 - VERIFICAR DISPONIBILIDADE DO ON-LINE             *
      *    GFCT5523 - OBTER DESCRICAO DE MENSAGENS                     *
      *    GFCT5505 - OBTER CODIGO DE JUNCAO                           *
      *    GFCT5006 - DESCRICAO DEPENDENCIA                            *
      *    GFCT5109 - OBTER DESCRICAO DE PAB                           *
      *    GFCT5090 - DESCRICAO POSTO SERVICO                          *
      *    DCIT8000 - VERIFICA UF                                      *
      *    RURC9020 - VERIFICA MUNICIPIO                               *
      *    POOL0081 - ALOCAR DINAMICAMENTE UM MODULO                   *
      *----------------------------------------------------------------*
      *    ULTIMA ALTERACAO EM 11/06/2008 - VALERIA                    *
      *    ALTERACOES REFERENTES AOS AGRUPAMENTOS PERSONALIZADOS       *
      *    - INCLUSAO DOS AGRUPAMENTOS PERSONALIZADOS                  *
      *----------------------------------------------------------------*
0310  *    ULTIMA ALTERACAO EM 31/03/2010 - HELIO SANTONI              *
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
           '* INICIO DA AREA DE WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*
       77  WRK-GFCTND-QTDE-OCOR        PIC  9(007)   COMP-3 VALUE ZERO.
       77  WRK-GFCTNE-QTDE-OCOR        PIC  9(007)   COMP-3 VALUE ZERO.
       77  WRK-GFCTNE-QTDE-TOT-REG     PIC  9(009)   COMP-3 VALUE ZERO.

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA INDEXADORES *'.
      *---------------------------------------------------------------*

       77  IND-1                       PIC  9(007) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA VARIAVEIS AUXILIARES *'.
      *----------------------------------------------------------------*

       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-NOM-TAB                 PIC  X(011)         VALUE SPACES.
       77  WRK-FIM-CSR01-GFCTB0B2      PIC  X(001)         VALUE SPACES.
       77  WRK-DESPREZA                PIC  9(007)         VALUE ZEROS.
       77  WRK-DESPREZA-PK             PIC  9(007) COMP-3  VALUE ZEROS.

       01  WRK-TARIFA-INI              PIC S9(005) COMP-3  VALUE ZEROS.
       01  WRK-TARIFA-FIM              PIC S9(005) COMP-3  VALUE ZEROS.
       01  WRK-DPROCM-ATUAL            PIC  X(010)         VALUE SPACES.
       01  WRK-COUNT                   PIC S9(009) COMP    VALUE ZEROS.

       01  WRK-MSG001.
           05 FILLER                   PIC  X(026)         VALUE
              'ERRO NA CHAMADA AO MODULO '.
           05 WRK-MODULO-MSG           PIC  X(008)         VALUE SPACES.

HEXA   01  WRK-COD-SQL-ERRO            PIC S9(09)     VALUE ZEROS.
       01  FILLER                      REDEFINES      WRK-COD-SQL-ERRO.
           05  FILLER                  PIC  X(006).
           05  WRK-COD-SQL-ERRO-2      PIC  9(003).
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(051)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5522 - ENTRADA *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTFZ'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5522 - SAIDA *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTG1'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5523 - ENTRADA *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTG2'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5523 - SAIDA *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTG3'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5006 - ENTRADA *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCT1V'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5006 - SAIDA *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCT1X'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO GFCT5109 E GFCT5090 *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTGB'.

       COPY 'I#GFCTGC'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM RURC9020 *'.
      *----------------------------------------------------------------*

       COPY 'I#RURC88'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM DCIT8000  *'.
      *----------------------------------------------------------------*

       01  WRK-DCIT-UNIDADE-FEDERATIVA.
           05 WRK-AREA-DCLGEN-DCITV002 PIC  X(070)         VALUE SPACES.
           05 WRK-DCIT-CODIGO-RETORNO  PIC  9(002)         VALUE ZEROS.

       01  WRK-POOL7100                PIC  X(107)         VALUE SPACES.
       01  WRK-SQLCA                   PIC  X(136)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5505  *'.
      *----------------------------------------------------------------*

       01  WRK-5505-AREA-ENTRADA.
           05 WRK-5505-AMBIENTE        PIC  X(001)         VALUE SPACES.
           05 WRK-5505-FUNC-BDSCO      PIC  X(007)         VALUE SPACES.
       01  WRK-5505-AREA-SAIDA.
           05 WRK-5505-COD-RETORNO     PIC  9(002)         VALUE ZEROS.
           05 WRK-5505-COD-SQL-ERRO    PIC  9(003)         VALUE ZEROS.
           05 WRK-5505-DESC-MSG        PIC  X(070)         VALUE SPACES.
           05 WRK-5505-DADOS-RETORNO.
             10 WRK-5505-COD-JUNCAO    PIC  9(005)         VALUE ZEROS.

       01  WRK-IO-PCB                  PIC  X(032)         VALUE SPACES.
       01  WRK-ALT-PCB                 PIC  X(032)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA TABELA DB2 *'.
      *----------------------------------------------------------------*

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
             INCLUDE GFCTB0B3
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0B1
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0C0
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0B8
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0B9
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0B7
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0C1
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0H5
           END-EXEC.

           EXEC SQL
             INCLUDE DCITV002
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0D8
           END-EXEC.

           EXEC SQL
             INCLUDE PLCCB004
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0M7
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA CURSOR *'.
      *----------------------------------------------------------------*

           EXEC SQL
               DECLARE   CSR01-GFCTB0B2  CURSOR  FOR
                   SELECT
                       CAGPTO_CTA,
                       CSERVC_TARIF,
                       HIDTFD_FLEXZ_AGPTO,
                       DINIC_FLEXZ_AGPTO,
                       CSIT_SOLTC_FLEXZ,
                       CINDCD_AGPTO_TOT
               FROM  DB2PRD.PDIDO_FLEXZ_AGPTO
               WHERE
                   CAGPTO_CTA           = :GFCTB0B2.CAGPTO-CTA AND
                  (CSERVC_TARIF        >= :WRK-TARIFA-INI      AND
                   CSERVC_TARIF        <= :WRK-TARIFA-FIM)     AND
                  (CSIT_SOLTC_FLEXZ     =  01                  OR
                  (CSIT_SOLTC_FLEXZ    IN (02)                 AND
                   DINIC_FLEXZ_AGPTO   >  :WRK-DPROCM-ATUAL))
               ORDER BY
                       CSERVC_TARIF,
                       DINIC_FLEXZ_AGPTO
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* FIM DA AREA DE WORKING *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY 'I#GFCTND'.

       COPY 'I#GFCTNE'.

       COPY 'I#GFCT0M'.

      *================================================================*
       PROCEDURE                       DIVISION USING GFCTND-ENTRADA
                                                      GFCTNE-SAIDA
                                                      GFCT0M-AREA-ERROS.
      *================================================================*

      ******************************************************************
      *INICIA O PROGRAMA                                               *
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
      *MONTA AREA DE SAIDA, VERIFICA DISPONIBILIDADE DO SISTEMA E CON- *
      *SISTE DADOS.                                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCT0M-AREA-ERROS
                                          GFCTNE-SAIDA.

           INITIALIZE GFCTNE-SAIDA
                      IND-1
                      GFCT0M-AREA-ERROS.

           PERFORM 1100-VERIFICAR-DISP-SISTEMA.

           PERFORM 1200-INICIALIZAR-AREA-SAIDA.

           PERFORM 1300-CONSISTIR-DADOS.

           PERFORM 1400-SELECIONAR-GFCTB0A1.

           PERFORM 1500-ACESSAR-MODULO-GFCT5505.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *VERIFICA DISPONIBILIDADE DO SISTEMA                             *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-VERIFICAR-DISP-SISTEMA     SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTG1-SAIDA.

           INITIALIZE GFCTG1-SAIDA.

           MOVE +100                   TO GFCTFZ-LL.
           MOVE ZEROS                  TO GFCTFZ-ZZ.
           MOVE 'GFCT'                 TO GFCTFZ-SISTEMA.
           MOVE GFCTND-TRANSACAO       TO GFCTFZ-TRANSACAO.
           MOVE GFCTND-FUNCAO          TO GFCTFZ-FUNCAO.
           MOVE GFCTND-FUNC-BDSCO      TO GFCTFZ-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTFZ-FIM.

           MOVE 'GFCT5522'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                             GFCTFZ-ENTRADA
                                             GFCTG1-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTNE-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0420'         TO GFCT0M-TRANSACAO
               MOVE 'S'                TO GFCTNE-FIM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-ERRO             EQUAL 9
               MOVE 'GFCT0420'         TO GFCT0M-TRANSACAO
               MOVE GFCTG1-COD-SQL-ERRO
                                       TO GFCT0M-SQLCODE
                                          GFCTNE-COD-SQL-ERRO
               MOVE GFCTG1-ERRO        TO GFCTNE-ERRO
               MOVE GFCTG1-COD-MSG-ERRO
                                       TO GFCTNE-COD-MSG-ERRO
               MOVE GFCTG1-DESC-MSG-ERRO
                                       TO GFCT0M-TEXTO
                                          GFCTNE-DESC-MSG-ERRO
               MOVE 'S'                TO GFCTNE-FIM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-ERRO             EQUAL 1
               MOVE 'GFCT0420'         TO GFCT0M-TRANSACAO
               MOVE ZEROS              TO GFCT0M-SQLCODE
                                          GFCTNE-COD-SQL-ERRO
               MOVE GFCTG1-ERRO        TO GFCTNE-ERRO
               MOVE GFCTG1-COD-MSG-ERRO
                                       TO GFCTNE-COD-MSG-ERRO
               MOVE GFCTG1-DESC-MSG-ERRO
                                       TO GFCT0M-TEXTO
                                          GFCTNE-DESC-MSG-ERRO
               MOVE 'S'                TO GFCTNE-FIM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-CSIT-ONLINE      EQUAL 1
               MOVE 'SISTEMA INDISPONIVEL'
                                       TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0420'         TO GFCT0M-TRANSACAO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
               MOVE 9                  TO GFCTNE-ERRO
               MOVE 'S'                TO GFCTNE-FIM
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *INICIALIZA AREA DE SAIDA                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       1200-INICIALIZAR-AREA-SAIDA     SECTION.
      *----------------------------------------------------------------*

0510       MOVE +246                   TO GFCTNE-LL.
           MOVE ZEROS                  TO GFCTNE-ZZ.
           MOVE GFCTND-TRANSACAO       TO GFCTNE-TRANSACAO.
           MOVE GFCTND-FUNCAO          TO GFCTNE-FUNCAO.

           IF  GFCTND-QTDE-OCOR        NOT NUMERIC
               MOVE ZEROS              TO GFCTNE-QTDE-OCOR
           ELSE
               MOVE GFCTND-QTDE-OCOR   TO GFCTNE-QTDE-OCOR
           END-IF.

           IF  GFCTND-QTDE-TOT-REG     NOT NUMERIC
               MOVE ZEROS              TO GFCTNE-QTDE-TOT-REG
           ELSE
               MOVE GFCTND-QTDE-TOT-REG
                                       TO GFCTNE-QTDE-TOT-REG
           END-IF.

           MOVE GFCTND-FUNC-BDSCO      TO GFCTNE-FUNC-BDSCO.
           MOVE GFCTND-FILTRO          TO GFCTNE-FILTRO.
           MOVE GFCTND-PONTEIRO        TO GFCTNE-PONTEIRO.

           MOVE ZEROS                  TO GFCTNE-ERRO
                                          GFCTNE-COD-SQL-ERRO
                                          GFCTNE-COD-MSG-ERRO.
           MOVE '0000'                 TO GFCTG2-COD-MSG.

           PERFORM 1310-OBTER-DESC-MSG.

           MOVE GFCTG3-DESC-MSG        TO GFCTNE-DESC-MSG-ERRO.

           MOVE 'N'                    TO GFCTNE-FIM.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *CONSISTE DADOS                                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       1300-CONSISTIR-DADOS            SECTION.
      *----------------------------------------------------------------*

           IF (GFCTND-TRANSACAO        EQUAL SPACES OR LOW-VALUES) OR
              (GFCTND-FUNCAO           EQUAL SPACES OR LOW-VALUES) OR
              (GFCTND-FUNC-BDSCO       EQUAL SPACES OR LOW-VALUES) OR
               GFCTND-AGRUPMTO-FIL     NOT NUMERIC  OR
               GFCTND-TARIFA-FIL       NOT NUMERIC  OR
               GFCTND-AGRUPMTO-PONT    NOT NUMERIC  OR
               GFCTND-TARIFA-PONT      NOT NUMERIC
               MOVE  1                 TO GFCTNE-ERRO
               MOVE  ZEROS             TO GFCTNE-COD-SQL-ERRO
               MOVE  0001              TO GFCTNE-COD-MSG-ERRO
               MOVE '0001'             TO GFCTG2-COD-MSG
               MOVE 'S'                TO GFCTNE-FIM
               PERFORM 1310-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTNE-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *OBTEM DESCRICAO DA MENSAGEM                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1310-OBTER-DESC-MSG             SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTG3-SAIDA.

           INITIALIZE GFCTG3-SAIDA.

           MOVE +100                   TO GFCTG2-LL.
           MOVE ZEROS                  TO GFCTG2-ZZ.
           MOVE GFCTND-TRANSACAO       TO GFCTG2-TRANSACAO.
           MOVE GFCTND-FUNCAO          TO GFCTG2-FUNCAO.
           MOVE GFCTND-FUNC-BDSCO      TO GFCTG2-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTG2-FIM.
           MOVE 'O'                    TO GFCTG2-TIPO-PROC.

           MOVE 'GFCT5523'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                             GFCTG2-ENTRADA
                                             GFCTG3-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTNE-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0420'         TO GFCT0M-TRANSACAO
               MOVE 'S'                TO GFCTNE-FIM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 9
               MOVE 9                  TO GFCTNE-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0420'         TO GFCT0M-TRANSACAO
               MOVE 'S'                TO GFCTNE-FIM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 1
               MOVE 1                  TO GFCTNE-ERRO
               MOVE GFCTG3-COD-SQL-ERRO
                                       TO GFCTNE-COD-SQL-ERRO
               MOVE GFCTG3-COD-MSG-ERRO
                                       TO GFCTNE-COD-MSG-ERRO
               MOVE GFCTG3-DESC-MSG-ERRO
                                       TO GFCTNE-DESC-MSG-ERRO
               MOVE 'S'                TO GFCTNE-FIM
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *SELECIONAR DATA ATUAL DA TABELA GFCTB0A1                        *
      ******************************************************************
      *----------------------------------------------------------------*
       1400-SELECIONAR-GFCTB0A1        SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC      OF GFCTB0A1.

           EXEC SQL
             SELECT DPROCM_ATUAL
             INTO  :GFCTB0A1.DPROCM-ATUAL
             FROM   DB2PRD.PARM_DATA_PROCM
             WHERE
                   CSIST_PRINC         = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0A1'      TO WRK-NOM-TAB
               MOVE 'PARM_DATA_PROCM'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0010'             TO GFCT0M-LOCAL
               MOVE  0010              TO GFCTNE-COD-MSG-ERRO
               PERFORM 1410-FORMATAR-ERRO-DB2
               MOVE SPACES             TO GFCTNE-DADOS-RST
               MOVE 'S'                TO GFCTNE-FIM
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FORMATA ERROS DB2                                               *
      ******************************************************************
      *----------------------------------------------------------------*
       1410-FORMATAR-ERRO-DB2          SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT0420'             TO GFCT0M-PROGRAMA.
           MOVE 'DB2'                  TO GFCT0M-TIPO-ACESSO.
           MOVE  SQLCA                 TO GFCT0M-SQLCA-AREA.
           MOVE  9                     TO GFCTNE-ERRO.
HEXA       MOVE  SQLCODE               TO WRK-COD-SQL-ERRO
                                          GFCT0M-SQLCODE
HEXA       MOVE  WRK-COD-SQL-ERRO-2    TO GFCTNE-COD-SQL-ERRO
           MOVE SPACES                 TO GFCTNE-DESC-MSG-ERRO.

           STRING GFCTG3-DESC-MSG      WRK-NOM-TAB
           DELIMITED BY '  '           INTO GFCTNE-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       1410-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ACESSAR MODULO GFCT5505                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       1500-ACESSAR-MODULO-GFCT5505    SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  WRK-5505-AREA-SAIDA.
           MOVE 'O'                    TO WRK-5505-AMBIENTE.
           MOVE GFCTND-FUNC-BDSCO      TO WRK-5505-FUNC-BDSCO.
           MOVE 'GFCT5505'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                             WRK-5505-AREA-ENTRADA
                                             WRK-5505-AREA-SAIDA
                                             WRK-IO-PCB
                                             WRK-ALT-PCB.

           IF  WRK-5505-COD-RETORNO    NOT EQUAL ZEROS
               MOVE 1                  TO GFCTNE-ERRO
               MOVE ZEROS              TO GFCTNE-COD-SQL-ERRO
               MOVE 0112               TO GFCTNE-COD-MSG-ERRO
               MOVE WRK-5505-DESC-MSG  TO GFCTNE-DESC-MSG-ERRO
               MOVE 'S'                TO GFCTNE-FIM
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *PROCESSAMENTO PRINCIPAL                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTND-AGRUPMTO-FIL    TO CAGPTO-CTA OF GFCTB0B2.

           IF  GFCTND-TARIFA-FIL       EQUAL ZEROS
               MOVE ZEROS              TO WRK-TARIFA-INI
               MOVE 99999              TO WRK-TARIFA-FIM
           ELSE
               MOVE GFCTND-TARIFA-FIL  TO WRK-TARIFA-INI
                                          WRK-TARIFA-FIM
           END-IF.

           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO WRK-DPROCM-ATUAL.

           PERFORM 2100-PROCESSAR-COUNT.

           MOVE 'N'                    TO WRK-FIM-CSR01-GFCTB0B2.

           PERFORM 2200-OPEN-CURSOR1.

           MOVE GFCTND-QTDE-OCOR TO WRK-GFCTND-QTDE-OCOR
           COMPUTE WRK-DESPREZA-PK = WRK-GFCTND-QTDE-OCOR + 1.
           MOVE WRK-DESPREZA-PK TO WRK-DESPREZA.

           PERFORM 2300-FETCH-CURSOR1  VARYING IND-1
               FROM 1                  BY 1
               UNTIL IND-1             GREATER WRK-DESPREZA OR
               WRK-FIM-CSR01-GFCTB0B2  EQUAL 'S'.

           PERFORM 2400-PROCESSAR      VARYING IND-1
               FROM 1                  BY 1
               UNTIL IND-1             GREATER 4 OR
               WRK-FIM-CSR01-GFCTB0B2  EQUAL 'S'.

           IF  IND-1                   EQUAL 1
               MOVE ZEROS              TO GFCTNE-COD-SQL-ERRO
                                          GFCTNE-ERRO
               MOVE 0007               TO GFCTNE-COD-MSG-ERRO
               MOVE '0007'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNE-DESC-MSG-ERRO
           END-IF.

           PERFORM 2500-CLOSE-CURSOR1.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *PROCESSAR COUNT                                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-PROCESSAR-COUNT            SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               SELECT  COUNT(*)
               INTO    :WRK-COUNT
               FROM    DB2PRD.PDIDO_FLEXZ_AGPTO
               WHERE
                   CAGPTO_CTA          =  :GFCTB0B2.CAGPTO-CTA AND
                  (CSERVC_TARIF        >= :WRK-TARIFA-INI      AND
                   CSERVC_TARIF        <= :WRK-TARIFA-FIM)     AND
                  (CSIT_SOLTC_FLEXZ     =  01                  OR
                  (CSIT_SOLTC_FLEXZ    IN (02)                 AND
                   DINIC_FLEXZ_AGPTO   >  :WRK-DPROCM-ATUAL))
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0B2'      TO WRK-NOM-TAB
               MOVE 'PDIDO_FLEXZ_AGPTO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0020'             TO GFCT0M-LOCAL
               MOVE  0010              TO GFCTNE-COD-MSG-ERRO
               PERFORM 1410-FORMATAR-ERRO-DB2
               MOVE SPACES             TO GFCTNE-DADOS-RST
               MOVE 'S'                TO GFCTNE-FIM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-COUNT               GREATER 150
               MOVE ZEROS              TO GFCTNE-ERRO
                                          GFCTNE-COD-SQL-ERRO
               MOVE 662                TO GFCTNE-COD-MSG-ERRO
               MOVE '0662'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNE-DESC-MSG-ERRO
               MOVE 'S'                TO GFCTNE-FIM
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ABERTURA DO CURSOR                                              *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-OPEN-CURSOR1               SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               OPEN CSR01-GFCTB0B2
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0005'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0B2'      TO WRK-NOM-TAB
               MOVE 'PDIDO_FLEXZ_AGPTO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE '0030'             TO GFCT0M-LOCAL
               MOVE  0005              TO GFCTNE-COD-MSG-ERRO
               PERFORM 1410-FORMATAR-ERRO-DB2
               MOVE GFCTG3-DESC-MSG    TO GFCTNE-DESC-MSG-ERRO
               MOVE 'S'                TO GFCTNE-FIM
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ABERTURA DO CURSOR                                              *
      ******************************************************************
      *----------------------------------------------------------------*
       2300-FETCH-CURSOR1              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH   CSR01-GFCTB0B2    INTO
                   :GFCTB0B2.CAGPTO-CTA,
                   :GFCTB0B2.CSERVC-TARIF,
                   :GFCTB0B2.HIDTFD-FLEXZ-AGPTO,
                   :GFCTB0B2.DINIC-FLEXZ-AGPTO,
                   :GFCTB0B2.CSIT-SOLTC-FLEXZ,
                   :GFCTB0B2.CINDCD-AGPTO-TOT
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0006'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0B2'      TO WRK-NOM-TAB
               MOVE 'PDIDO_FLEXZ_AGPTO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE '0040'             TO GFCT0M-LOCAL
               MOVE  0006              TO GFCTNE-COD-MSG-ERRO
               PERFORM 1410-FORMATAR-ERRO-DB2
               MOVE GFCTG3-DESC-MSG    TO GFCTNE-DESC-MSG-ERRO
               MOVE 'S'                TO GFCTNE-FIM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-CSR01-GFCTB0B2
                                          GFCTNE-FIM
           END-IF.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *PROCESSO                                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       2400-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

120510     COMPUTE GFCTNE-LL = ( GFCTNE-LL + 191 ).

           MOVE CSERVC-TARIF           OF GFCTB0B2
                                       TO GFCTNE-COD-TARIFA(IND-1).
           MOVE HIDTFD-FLEXZ-AGPTO     OF GFCTB0B2
                                       TO GFCTNE-TIMESTAMP(IND-1).
           MOVE 1                      TO GFCTNE-SEQUENCIA(IND-1).

           MOVE GFCTNE-QTDE-TOT-REG TO WRK-GFCTNE-QTDE-TOT-REG.
           MOVE GFCTNE-QTDE-OCOR TO WRK-GFCTNE-QTDE-OCOR.

           ADD 1                       TO WRK-GFCTNE-QTDE-TOT-REG
                                          WRK-GFCTNE-QTDE-OCOR.
           MOVE WRK-GFCTNE-QTDE-TOT-REG TO GFCTNE-QTDE-TOT-REG.
           MOVE WRK-GFCTNE-QTDE-OCOR TO GFCTNE-QTDE-OCOR.

           EVALUATE GFCTND-AGRUPMTO-FIL
               WHEN 15
                   PERFORM 2410-SELECIONAR-PAB
               WHEN 14
                   IF  CINDCD-AGPTO-TOT
                                       OF GFCTB0B2
                                       NOT EQUAL 'S'
                       PERFORM 2420-SELECIONAR-AGENCIA
                   ELSE
                       MOVE 'TODOS'    TO GFCTNE-DESCR-AGPTO(IND-1)
                   END-IF
               WHEN 4
                   PERFORM 2440-SELECIONAR-CLIENTE
               WHEN 9
                   PERFORM 2450-SELECIONAR-SEGMENTO
               WHEN 18
                   PERFORM 2460-SELECIONAR-MUNICIPIO
               WHEN 19
                   PERFORM 2470-SELECIONAR-UF
0310           WHEN 17
0310               PERFORM 2430-SELECIONAR-POSTAL
0310           WHEN 20
0310               PERFORM 2430-SELECIONAR-POSTAL
0310           WHEN 26
0310               PERFORM 2490-SELECIONAR-RAZAO
110608         WHEN OTHER
110608             MOVE ZEROS          TO GFCTNE-CGC-CPF(IND-1)
110608                                    GFCTNE-FILIAL(IND-1)
110608                                    GFCTNE-SEGMENTO(IND-1)
110608                                    GFCTNE-AGENCIA(IND-1)
110608                                    GFCTNE-POSTO(IND-1)
110608                                    GFCTNE-MUNICIPIO(IND-1)
0310                                      GFCTNE-GRUPO-CTBIL    (IND-1)
0310                                      GFCTNE-SUB-GRUPO-CTBIL(IND-1)
110608             MOVE SPACES         TO GFCTNE-UF(IND-1)
110608                                    GFCTNE-CONTROLE(IND-1)
110608                                    GFCTNE-DESCR-AGPTO(IND-1)
120510                                    GFCTNE-DESC-CTBIL (IND-1)
           END-EVALUATE.

           IF  CSIT-SOLTC-FLEXZ        OF GFCTB0B2
                                       EQUAL 1
               MOVE 'PEDIDO'           TO GFCTNE-SITUACAO(IND-1)
           ELSE
               IF  CSIT-SOLTC-FLEXZ    OF GFCTB0B2
                                       EQUAL 2
                   MOVE 'DEFERIDA'     TO GFCTNE-SITUACAO(IND-1)
               ELSE
                   IF  CSIT-SOLTC-FLEXZ OF GFCTB0B2
                                       EQUAL 8
                       MOVE 'ENCERRADA'
                                       TO GFCTNE-SITUACAO(IND-1)
                   END-IF
               END-IF
           END-IF.

           PERFORM 2480-PESQUISAR-TARIFA.

           MOVE RSERVC-TARIF-REDZD     OF GFCTB0D8
                                       TO GFCTNE-DESCR-TARIFA(IND-1).

           PERFORM 2300-FETCH-CURSOR1.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *SELECIONAR PAB                                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2410-SELECIONAR-PAB             SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0B2
           TO   CSERVC-TARIF           OF GFCTB0B8.

           MOVE HIDTFD-FLEXZ-AGPTO     OF GFCTB0B2
           TO   HIDTFD-FLEXZ-AGPTO     OF GFCTB0B8.

           MOVE CAGPTO-CTA             OF GFCTB0B2
           TO   CAGPTO-CTA             OF GFCTB0B8.

           EXEC SQL
               SELECT
                   CEMPR_INC,
                   CDEPDC,
                   CPOSTO_SERVC
               INTO
                   :GFCTB0B8.CEMPR-INC,
                   :GFCTB0B8.CDEPDC,
                   :GFCTB0B8.CPOSTO-SERVC
               FROM    DB2PRD.PDIDO_FLEXZ_PAB
               WHERE
                   CAGPTO_CTA          = :GFCTB0B8.CAGPTO-CTA      AND
                   CSERVC_TARIF        = :GFCTB0B8.CSERVC-TARIF    AND
                   HIDTFD_FLEXZ_AGPTO  = :GFCTB0B8.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0012'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0B8'      TO WRK-NOM-TAB
               MOVE 'PDIDO_FLEXZ_PAB ' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0050'             TO GFCT0M-LOCAL
               MOVE  0012               TO GFCTNE-COD-MSG-ERRO
HEXA           MOVE  SQLCODE            TO WRK-COD-SQL-ERRO
HEXA           MOVE  WRK-COD-SQL-ERRO-2 TO GFCTNE-COD-SQL-ERRO
               MOVE  9                  TO GFCTNE-ERRO
               PERFORM 1410-FORMATAR-ERRO-DB2
               MOVE 'S'                 TO GFCTNE-FIM
               MOVE GFCTG3-DESC-MSG     TO GFCTNE-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE  1                 TO GFCTNE-ERRO
               MOVE ZEROS              TO GFCTNE-COD-SQL-ERRO
               MOVE  0926              TO GFCTNE-COD-MSG-ERRO
               MOVE '0926'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE 'S'                TO GFCTNE-FIM
               MOVE GFCTG3-DESC-MSG    TO GFCTNE-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           PERFORM 2411-OBTER-DESC-PAB.

           MOVE GFCTGC-DESC-POSTO(1)   TO GFCTNE-DESCR-AGPTO(IND-1).
           MOVE CDEPDC    OF GFCTB0B8  TO GFCTNE-AGENCIA(IND-1).
           MOVE CPOSTO-SERVC           OF GFCTB0B8
                                       TO GFCTNE-POSTO(IND-1).

      *----------------------------------------------------------------*
       2410-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *OBTEM DESCRICAO DA MENSAGEM PAB                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2411-OBTER-DESC-PAB             SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTGB-ENTRADA.

           INITIALIZE GFCTGB-ENTRADA.

           MOVE 100                    TO GFCTGB-LL.
           MOVE ZEROS                  TO GFCTGB-ZZ.
           MOVE GFCTND-TRANSACAO       TO GFCTGB-TRANSACAO.
           MOVE GFCTND-FUNCAO          TO GFCTGB-FUNCAO.
           MOVE ZEROS                  TO GFCTGB-QTDE-OCOR
                                          GFCTGB-QTDE-TOT-REG.
           MOVE GFCTND-FUNC-BDSCO      TO GFCTGB-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTGB-FIM.

           MOVE CDEPDC                 OF GFCTB0B8
                                       TO GFCTGB-COD-DEPDC.

           MOVE CPOSTO-SERVC           OF GFCTB0B8
                                       TO GFCTGB-COD-POSTO.

           MOVE SPACES                 TO GFCTGC-SAIDA.

           INITIALIZE GFCTGC-SAIDA
                      GFCT0M-AREA-ERROS.

           MOVE 'GFCT5109'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                             GFCTGB-ENTRADA
                                             GFCTGC-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTNE-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0420'         TO GFCT0M-TRANSACAO
               MOVE 'S'                TO GFCTNE-FIM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTGC-ERRO             NOT EQUAL ZEROS
               MOVE 'S'                TO GFCTNE-FIM
               MOVE GFCTGC-ERRO        TO GFCTNE-ERRO
               MOVE GFCTGC-COD-SQL-ERRO
                                       TO GFCTNE-COD-SQL-ERRO
               MOVE GFCTGC-COD-MSG-ERRO
                                       TO GFCTNE-COD-MSG-ERRO

               IF  GFCTGC-ERRO         EQUAL 1
                   MOVE GFCTGC-DESC-MSG-ERRO
                                       TO GFCTNE-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5109-'  GFCTGC-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO GFCTNE-DESC-MSG-ERRO
               END-IF
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2411-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *SELECIONAR AGENCIA                                              *
      ******************************************************************
      *----------------------------------------------------------------*
       2420-SELECIONAR-AGENCIA         SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0B2
           TO   CSERVC-TARIF           OF GFCTB0B1.

           MOVE HIDTFD-FLEXZ-AGPTO     OF GFCTB0B2
           TO   HIDTFD-FLEXZ-AGPTO     OF GFCTB0B1.

           MOVE CAGPTO-CTA             OF GFCTB0B2
           TO   CAGPTO-CTA             OF GFCTB0B1.

           EXEC SQL
               SELECT
                   CEMPR_INC,
                   CDEPDC
               INTO
                   :GFCTB0B1.CEMPR-INC,
                   :GFCTB0B1.CDEPDC
               FROM    DB2PRD.PDIDO_FLEXZ_AG
               WHERE
                   CAGPTO_CTA          = :GFCTB0B1.CAGPTO-CTA      AND
                   CSERVC_TARIF        = :GFCTB0B1.CSERVC-TARIF    AND
                   HIDTFD_FLEXZ_AGPTO  = :GFCTB0B1.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0B8'      TO WRK-NOM-TAB
               MOVE 'PDIDO_FLEXZ_AG'   TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0060'             TO GFCT0M-LOCAL
               MOVE  0010               TO GFCTNE-COD-MSG-ERRO
HEXA           MOVE  SQLCODE            TO WRK-COD-SQL-ERRO
HEXA           MOVE  WRK-COD-SQL-ERRO-2 TO GFCTNE-COD-SQL-ERRO
               MOVE  9                  TO GFCTNE-ERRO
               PERFORM 1410-FORMATAR-ERRO-DB2
               MOVE 'S'                 TO GFCTNE-FIM
               MOVE GFCTG3-DESC-MSG     TO GFCTNE-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE  1                 TO GFCTNE-ERRO
               MOVE ZEROS              TO GFCTNE-COD-SQL-ERRO
               MOVE  0926              TO GFCTNE-COD-MSG-ERRO
               MOVE '0926'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE 'S'                TO GFCTNE-FIM
               MOVE GFCTG3-DESC-MSG    TO GFCTNE-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           PERFORM 2421-OBTER-DESC-AG.

           MOVE GFCT1X-DESC-DEPDC(1)(1:30)   TO
           GFCTNE-DESCR-AGPTO(IND-1).
           MOVE CDEPDC    OF GFCTB0B1  TO GFCTNE-AGENCIA(IND-1).

      *----------------------------------------------------------------*
       2420-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *OBTEM DESCRICAO DA MENSAGEM AG                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2421-OBTER-DESC-AG              SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCT1V-ENTRADA.

           INITIALIZE GFCT1V-ENTRADA.

           MOVE 655                    TO GFCT1V-LL.
           MOVE ZEROS                  TO GFCT1V-ZZ.
           MOVE GFCTND-TRANSACAO       TO GFCT1V-TRANSACAO.
           MOVE GFCTND-FUNCAO          TO GFCT1V-FUNCAO.
           MOVE 001                    TO GFCT1V-QTDE-OCOR
           MOVE ZEROS                  TO GFCT1V-QTDE-TOT-REG.
           MOVE GFCTND-FUNC-BDSCO      TO GFCT1V-FUNC-BDSCO.
           MOVE 'N'                    TO GFCT1V-FIM.

           MOVE CDEPDC                 OF GFCTB0B1
                                       TO GFCT1V-COD-DEPDC(1).

           MOVE SPACES                 TO GFCT1X-SAIDA.

           INITIALIZE GFCT1X-SAIDA
                      GFCT0M-AREA-ERROS.

           MOVE 'GFCT5006'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                             GFCT1V-ENTRADA
                                             GFCT1X-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTNE-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0420'         TO GFCT0M-TRANSACAO
               MOVE 'S'                TO GFCTNE-FIM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCT1X-ERRO             NOT EQUAL ZEROS
               MOVE 'S'                TO GFCTNE-FIM
               MOVE GFCT1X-ERRO        TO GFCTNE-ERRO
               MOVE GFCT1X-COD-SQL-ERRO
                                       TO GFCTNE-COD-SQL-ERRO
               MOVE GFCT1X-COD-MSG-ERRO
                                       TO GFCTNE-COD-MSG-ERRO

               IF  GFCT1X-ERRO         EQUAL 1
                   MOVE GFCT1X-DESC-MSG-ERRO
                                       TO GFCTNE-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5006-'  GFCT1X-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO GFCTNE-DESC-MSG-ERRO
               END-IF
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2421-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *SELECIONAR POSTAL                                               *
      ******************************************************************
      *----------------------------------------------------------------*
       2430-SELECIONAR-POSTAL          SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0B2
           TO   CSERVC-TARIF           OF GFCTB0B9.

           MOVE HIDTFD-FLEXZ-AGPTO     OF GFCTB0B2
           TO   HIDTFD-FLEXZ-AGPTO     OF GFCTB0B9.

           MOVE CAGPTO-CTA             OF GFCTB0B2
           TO   CAGPTO-CTA             OF GFCTB0B9.

           EXEC SQL
               SELECT
                   CEMPR_INC,
                   CDEPDC,
                   CPOSTO_SERVC
               INTO
                   :GFCTB0B9.CEMPR-INC,
                   :GFCTB0B9.CDEPDC,
                   :GFCTB0B9.CPOSTO-SERVC
               FROM    DB2PRD.PDIDO_FLEXZ_PSTAL
               WHERE
                   CAGPTO_CTA          = :GFCTB0B9.CAGPTO-CTA      AND
                   CSERVC_TARIF        = :GFCTB0B9.CSERVC-TARIF    AND
                   HIDTFD_FLEXZ_AGPTO  = :GFCTB0B9.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0B9'      TO WRK-NOM-TAB
               MOVE 'PDIDO_FLEXZ_PSTAL'
                                       TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0070'             TO GFCT0M-LOCAL
               MOVE  0010               TO GFCTNE-COD-MSG-ERRO
HEXA           MOVE  SQLCODE            TO WRK-COD-SQL-ERRO
HEXA           MOVE  WRK-COD-SQL-ERRO-2 TO GFCTNE-COD-SQL-ERRO
               MOVE  9                  TO GFCTNE-ERRO
               PERFORM 1410-FORMATAR-ERRO-DB2
               MOVE 'S'                 TO GFCTNE-FIM
               MOVE GFCTG3-DESC-MSG     TO GFCTNE-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE  1                 TO GFCTNE-ERRO
               MOVE ZEROS              TO GFCTNE-COD-SQL-ERRO
               MOVE  0926              TO GFCTNE-COD-MSG-ERRO
               MOVE '0926'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE 'S'                TO GFCTNE-FIM
               MOVE GFCTG3-DESC-MSG    TO GFCTNE-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           PERFORM 2431-OBTER-DESC-PSTAL.

           MOVE GFCTGC-DESC-POSTO(1)   TO GFCTNE-DESCR-AGPTO(IND-1).
           MOVE CDEPDC    OF GFCTB0B9  TO GFCTNE-AGENCIA(IND-1).
           MOVE CPOSTO-SERVC           OF GFCTB0B9
                                       TO GFCTNE-POSTO(IND-1).

      *---------------------------------------------------------------*
       2430-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *OBTEM DESCRICAO DA MENSAGEM POSTAL                              *
      ******************************************************************
      *----------------------------------------------------------------*
       2431-OBTER-DESC-PSTAL           SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTGB-ENTRADA.

           INITIALIZE GFCTGB-ENTRADA.

           MOVE 100                    TO GFCTGB-LL.
           MOVE ZEROS                  TO GFCTGB-ZZ.
           MOVE GFCTND-TRANSACAO       TO GFCTGB-TRANSACAO.
           MOVE GFCTND-FUNCAO          TO GFCTGB-FUNCAO.
           MOVE ZEROS                  TO GFCTGB-QTDE-OCOR
                                          GFCTGB-QTDE-TOT-REG.
           MOVE GFCTND-FUNC-BDSCO      TO GFCTGB-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTGB-FIM.

           MOVE CDEPDC                 OF GFCTB0B9
                                       TO GFCTGB-COD-DEPDC.

           MOVE CPOSTO-SERVC           OF GFCTB0B9
                                       TO GFCTGB-COD-POSTO.

           MOVE SPACES                 TO GFCTGC-SAIDA.

           INITIALIZE GFCTGC-SAIDA
                      GFCT0M-AREA-ERROS.

           MOVE 'GFCT5090'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                             GFCTGB-ENTRADA
                                             GFCTGC-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTNE-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0420'         TO GFCT0M-TRANSACAO
               MOVE 'S'                TO GFCTNE-FIM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTGC-ERRO             NOT EQUAL ZEROS
               MOVE 'S'                TO GFCTNE-FIM
               MOVE GFCTGC-ERRO        TO GFCTNE-ERRO
               MOVE GFCTGC-COD-SQL-ERRO
                                       TO GFCTNE-COD-SQL-ERRO
               MOVE GFCTGC-COD-MSG-ERRO
                                       TO GFCTNE-COD-MSG-ERRO

               IF  GFCTGC-ERRO         EQUAL 1
                   MOVE GFCTGC-DESC-MSG-ERRO
                                       TO GFCTNE-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5090-'  GFCTGC-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO GFCTNE-DESC-MSG-ERRO
               END-IF
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2431-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *SELECIONAR CLIENTE                                              *
      ******************************************************************
      *----------------------------------------------------------------*
       2440-SELECIONAR-CLIENTE         SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0B2
           TO   CSERVC-TARIF           OF GFCTB0B3.

           MOVE HIDTFD-FLEXZ-AGPTO     OF GFCTB0B2
           TO   HIDTFD-FLEXZ-AGPTO     OF GFCTB0B3.

           MOVE CAGPTO-CTA             OF GFCTB0B2
           TO   CAGPTO-CTA             OF GFCTB0B3.

           EXEC SQL
ST2506*        SELECT
ST2506*            CCGC_CPF,
ST2506*            CFLIAL_CGC,
ST2506*            CCTRL_CPF_CGC
ST2506*        INTO
ST2506*            :GFCTB0B3.CCGC-CPF,
ST2506*            :GFCTB0B3.CFLIAL-CGC,
ST2506*            :GFCTB0B3.CCTRL-CPF-CGC
ST2506         SELECT
ST2506             CCGC_CPF_ST,
ST2506             CFLIAL_CGC_ST,
ST2506             CCTRL_CPF_CGC_ST
ST2506         INTO
ST2506             :GFCTB0B3.CCGC-CPF-ST,
ST2506             :GFCTB0B3.CFLIAL-CGC-ST,
ST2506             :GFCTB0B3.CCTRL-CPF-CGC-ST
               FROM    DB2PRD.PDIDO_FLEXZ_CLI
               WHERE
                   CAGPTO_CTA          = :GFCTB0B3.CAGPTO-CTA      AND
                   CSERVC_TARIF        = :GFCTB0B3.CSERVC-TARIF    AND
                   HIDTFD_FLEXZ_AGPTO  = :GFCTB0B3.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0B3'      TO WRK-NOM-TAB
               MOVE 'PDIDO_FLEXZ_CLI'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0080'             TO GFCT0M-LOCAL
               MOVE  0010              TO GFCTNE-COD-MSG-ERRO
HEXA           MOVE  SQLCODE            TO WRK-COD-SQL-ERRO
HEXA           MOVE  WRK-COD-SQL-ERRO-2 TO GFCTNE-COD-SQL-ERRO
               MOVE  9                 TO GFCTNE-ERRO
               PERFORM 1410-FORMATAR-ERRO-DB2
               MOVE 'S'                TO GFCTNE-FIM
               MOVE GFCTG3-DESC-MSG    TO GFCTNE-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE  1                 TO GFCTNE-ERRO
               MOVE ZEROS              TO GFCTNE-COD-SQL-ERRO
               MOVE  0926              TO GFCTNE-COD-MSG-ERRO
               MOVE '0926'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE 'S'                TO GFCTNE-FIM
               MOVE GFCTG3-DESC-MSG    TO GFCTNE-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

ST2506*    MOVE CCGC-CPF   OF GFCTB0B3 TO GFCTNE-CGC-CPF(IND-1).
ST2506*    MOVE CFLIAL-CGC OF GFCTB0B3 TO GFCTNE-FILIAL(IND-1).
ST2506*    MOVE CCTRL-CPF-CGC          OF GFCTB0B3
ST2506*                                TO GFCTNE-CONTROLE(IND-1).
ST2506     MOVE CCGC-CPF-ST   OF GFCTB0B3 TO GFCTNE-CGC-CPF(IND-1).
ST2506     MOVE CFLIAL-CGC-ST OF GFCTB0B3 TO GFCTNE-FILIAL(IND-1).
ST2506     MOVE CCTRL-CPF-CGC-ST          OF GFCTB0B3
ST2506                                    TO GFCTNE-CONTROLE(IND-1).

      *----------------------------------------------------------------*
       2440-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *SELECIONAR SEGMENTO                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       2450-SELECIONAR-SEGMENTO        SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0B2
           TO   CSERVC-TARIF           OF GFCTB0C0.

           MOVE HIDTFD-FLEXZ-AGPTO     OF GFCTB0B2
           TO   HIDTFD-FLEXZ-AGPTO     OF GFCTB0C0.

           MOVE CAGPTO-CTA             OF GFCTB0B2
           TO   CAGPTO-CTA             OF GFCTB0C0.

           EXEC SQL
               SELECT
                   CSGMTO_GSTAO_TARIF
               INTO
                   :GFCTB0C0.CSGMTO-GSTAO-TARIF
               FROM    DB2PRD.PDIDO_FLEXZ_SGMTO
               WHERE
                   CAGPTO_CTA          = :GFCTB0C0.CAGPTO-CTA      AND
                   CSERVC_TARIF        = :GFCTB0C0.CSERVC-TARIF    AND
                   HIDTFD_FLEXZ_AGPTO  = :GFCTB0C0.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0C0'      TO WRK-NOM-TAB
               MOVE 'PDIDO_FLEXZ_SGMTO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0090'             TO GFCT0M-LOCAL
               MOVE  0010              TO GFCTNE-COD-MSG-ERRO
HEXA           MOVE  SQLCODE            TO WRK-COD-SQL-ERRO
HEXA           MOVE  WRK-COD-SQL-ERRO-2 TO GFCTNE-COD-SQL-ERRO
               MOVE  9                 TO GFCTNE-ERRO
               PERFORM 1410-FORMATAR-ERRO-DB2
               MOVE 'S'                TO GFCTNE-FIM
               MOVE GFCTG3-DESC-MSG    TO GFCTNE-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE  1                 TO GFCTNE-ERRO
               MOVE ZEROS              TO GFCTNE-COD-SQL-ERRO
               MOVE  0926              TO GFCTNE-COD-MSG-ERRO
               MOVE '0926'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE 'S'                TO GFCTNE-FIM
               MOVE GFCTG3-DESC-MSG    TO GFCTNE-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           PERFORM 2451-OBTER-DESC-SGMTO.

           MOVE CSGMTO-GSTAO-TARIF     OF GFCTB0C0
                                       TO GFCTNE-SEGMENTO(IND-1).
           MOVE ISGMTO-GSTAO-TARIF     OF GFCTB0H5
                                       TO GFCTNE-DESCR-AGPTO(IND-1).

      *---------------------------------------------------------------*
       2450-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *OBTEM DESCRICAO DA MENSAGEM SEGMENTO                            *
      ******************************************************************
      *----------------------------------------------------------------*
       2451-OBTER-DESC-SGMTO           SECTION.
      *----------------------------------------------------------------*

           MOVE CSGMTO-GSTAO-TARIF     OF GFCTB0C0
           TO CSGMTO-GSTAO-TARIF       OF GFCTB0H5.

           EXEC SQL
               SELECT  ISGMTO_GSTAO_TARIF
               INTO    :GFCTB0H5.ISGMTO-GSTAO-TARIF
               FROM   DB2PRD.TTPO_SGMTO_GSTAO
               WHERE
                   CSGMTO_GSTAO_TARIF  = :GFCTB0H5.CSGMTO-GSTAO-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0H5'      TO WRK-NOM-TAB
               MOVE 'TTPO_SGMTO_GSTAO' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0100'             TO GFCT0M-LOCAL
               MOVE  0010              TO GFCTNE-COD-MSG-ERRO
HEXA           MOVE  SQLCODE            TO WRK-COD-SQL-ERRO
HEXA           MOVE  WRK-COD-SQL-ERRO-2 TO GFCTNE-COD-SQL-ERRO
               MOVE  9                 TO GFCTNE-ERRO
               PERFORM 1410-FORMATAR-ERRO-DB2
               MOVE 'S'                TO GFCTNE-FIM
               MOVE GFCTG3-DESC-MSG    TO GFCTNE-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE  1                 TO GFCTNE-ERRO
               MOVE ZEROS              TO GFCTNE-COD-SQL-ERRO
               MOVE  0500              TO GFCTNE-COD-MSG-ERRO
               MOVE '0500'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE 'S'                TO GFCTNE-FIM
               MOVE GFCTG3-DESC-MSG    TO GFCTNE-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2451-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *SELECIONAR MUNICIPIO                                            *
      ******************************************************************
      *----------------------------------------------------------------*
       2460-SELECIONAR-MUNICIPIO       SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0B2
           TO   CSERVC-TARIF           OF GFCTB0B7.

           MOVE HIDTFD-FLEXZ-AGPTO     OF GFCTB0B2
           TO   HIDTFD-FLEXZ-AGPTO     OF GFCTB0B7.

           MOVE CAGPTO-CTA             OF GFCTB0B2
           TO   CAGPTO-CTA             OF GFCTB0B7.

           EXEC SQL
               SELECT  CMUN_IBGE
               INTO    :GFCTB0B7.CMUN-IBGE
               FROM    DB2PRD.PDIDO_FLEXZ_MUN
               WHERE
                   CAGPTO_CTA          = :GFCTB0B7.CAGPTO-CTA      AND
                   CSERVC_TARIF        = :GFCTB0B7.CSERVC-TARIF    AND
                   HIDTFD_FLEXZ_AGPTO  = :GFCTB0B7.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0B7'      TO WRK-NOM-TAB
               MOVE 'PDIDO_FLEXZ_MUN'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0110'             TO GFCT0M-LOCAL
               MOVE  0010              TO GFCTNE-COD-MSG-ERRO
HEXA           MOVE  SQLCODE            TO WRK-COD-SQL-ERRO
HEXA           MOVE  WRK-COD-SQL-ERRO-2 TO GFCTNE-COD-SQL-ERRO
               MOVE  9                 TO GFCTNE-ERRO
               PERFORM 1410-FORMATAR-ERRO-DB2
               MOVE 'S'                TO GFCTNE-FIM
               MOVE GFCTG3-DESC-MSG    TO GFCTNE-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE  1                 TO GFCTNE-ERRO
               MOVE ZEROS              TO GFCTNE-COD-SQL-ERRO
               MOVE  0926              TO GFCTNE-COD-MSG-ERRO
               MOVE '0926'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE 'S'                TO GFCTNE-FIM
               MOVE GFCTG3-DESC-MSG    TO GFCTNE-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           PERFORM 2461-OBTER-DESC-MUN.

           MOVE CMUN-IBGE  OF GFCTB0B7 TO GFCTNE-MUNICIPIO(IND-1).
           MOVE NOME-MUNIC-RURC88      TO GFCTNE-DESCR-AGPTO(IND-1).

      *----------------------------------------------------------------*
       2460-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *OBTEM DESCRICAO DA MENSAGEM MUNICIPIO                           *
      ******************************************************************
      *----------------------------------------------------------------*
       2461-OBTER-DESC-MUN             SECTION.
      *----------------------------------------------------------------*

           INITIALIZE AREA-RURC88.

           MOVE CMUN-IBGE OF GFCTB0B7  TO COD-MUNIC-RURC88.
           MOVE 'RURC9020'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                             AREA-RURC88.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 1                  TO GFCTNE-ERRO
               MOVE ZEROS              TO GFCTNE-COD-SQL-ERRO
               MOVE 0732               TO GFCTNE-COD-MSG-ERRO
               MOVE '0732'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNE-DESC-MSG-ERRO
               MOVE 'S'                TO GFCTNE-FIM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  COD-RETORNO-RURC88      EQUAL 10
               MOVE 1                  TO GFCTNE-ERRO
               MOVE ZEROS              TO GFCTNE-COD-SQL-ERRO
               MOVE 0675               TO GFCTNE-COD-MSG-ERRO
               MOVE '0675'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNE-DESC-MSG-ERRO
               MOVE 'S'                TO GFCTNE-FIM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  COD-RETORNO-RURC88      EQUAL 20
               MOVE 1                  TO GFCTNE-ERRO
               MOVE ZEROS              TO GFCTNE-COD-SQL-ERRO
               MOVE 0733               TO GFCTNE-COD-MSG-ERRO
               MOVE '0733'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNE-DESC-MSG-ERRO
               MOVE 'S'                TO GFCTNE-FIM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  COD-RETORNO-RURC88      EQUAL 99
               MOVE 1                  TO GFCTNE-ERRO
               MOVE SQLCODE-RURC88     TO GFCTNE-COD-SQL-ERRO
               MOVE 0734               TO GFCTNE-COD-MSG-ERRO
               MOVE '0734'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNE-DESC-MSG-ERRO
               MOVE 'S'                TO GFCTNE-FIM
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2461-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *SELECIONAR UF                                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2470-SELECIONAR-UF              SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0B2
           TO   CSERVC-TARIF           OF GFCTB0C1.

           MOVE HIDTFD-FLEXZ-AGPTO     OF GFCTB0B2
           TO   HIDTFD-FLEXZ-AGPTO     OF GFCTB0C1.

           MOVE CAGPTO-CTA             OF GFCTB0B2
           TO   CAGPTO-CTA             OF GFCTB0C1.

           EXEC SQL
               SELECT  CSGL_UF
               INTO    :GFCTB0C1.CSGL-UF
               FROM    DB2PRD.PDIDO_FLEXZ_UF
               WHERE
                   CAGPTO_CTA          = :GFCTB0C1.CAGPTO-CTA      AND
                   CSERVC_TARIF        = :GFCTB0C1.CSERVC-TARIF    AND
                   HIDTFD_FLEXZ_AGPTO  = :GFCTB0C1.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0C1'      TO WRK-NOM-TAB
               MOVE 'PDIDO_FLEXZ_MUN'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0120'             TO GFCT0M-LOCAL
               MOVE  0010              TO GFCTNE-COD-MSG-ERRO
HEXA           MOVE  SQLCODE            TO WRK-COD-SQL-ERRO
HEXA           MOVE  WRK-COD-SQL-ERRO-2 TO GFCTNE-COD-SQL-ERRO
               MOVE  9                 TO GFCTNE-ERRO
               PERFORM 1410-FORMATAR-ERRO-DB2
               MOVE 'S'                TO GFCTNE-FIM
               MOVE GFCTG3-DESC-MSG    TO GFCTNE-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE  1                 TO GFCTNE-ERRO
               MOVE ZEROS              TO GFCTNE-COD-SQL-ERRO
               MOVE  0926              TO GFCTNE-COD-MSG-ERRO
               MOVE '0926'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE 'S'                TO GFCTNE-FIM
               MOVE GFCTG3-DESC-MSG    TO GFCTNE-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           PERFORM 2471-OBTER-DESC-UF.

           MOVE CSGL-UF   OF GFCTB0C1  TO GFCTNE-UF(IND-1).
           MOVE WRK-AREA-DCLGEN-DCITV002
                                       TO DCITV002.
           MOVE IUF     OF DCITV002    TO GFCTNE-DESCR-AGPTO(IND-1).

      *----------------------------------------------------------------*
       2470-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *OBTEM DESCRICAO DA MENSAGEM UF                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2471-OBTER-DESC-UF              SECTION.
      *----------------------------------------------------------------*

           MOVE CSGL-UF  OF GFCTB0C1   TO CSGL-UF OF DCITV002.
           MOVE DCITV002               TO WRK-AREA-DCLGEN-DCITV002.
           MOVE 'DCIT8000'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                             WRK-DCIT-UNIDADE-FEDERATIVA
                                             WRK-POOL7100
                                             WRK-SQLCA.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 1                  TO GFCTNE-ERRO
               MOVE ZEROS              TO GFCTNE-COD-SQL-ERRO
               MOVE 0735               TO GFCTNE-COD-MSG-ERRO
               MOVE '0735'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNE-DESC-MSG-ERRO
               MOVE 'S'                TO GFCTNE-FIM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-DCIT-CODIGO-RETORNO EQUAL 1
               MOVE 1                  TO GFCTNE-ERRO
               MOVE ZEROS              TO GFCTNE-COD-SQL-ERRO
               MOVE 0736               TO GFCTNE-COD-MSG-ERRO
               MOVE '0736'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNE-DESC-MSG-ERRO
               MOVE 'S'                TO GFCTNE-FIM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-DCIT-CODIGO-RETORNO EQUAL 2
               MOVE 1                  TO GFCTNE-ERRO
               MOVE ZEROS              TO GFCTNE-COD-SQL-ERRO
               MOVE 0737               TO GFCTNE-COD-MSG-ERRO
               MOVE '0737'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNE-DESC-MSG-ERRO
               MOVE 'S'                TO GFCTNE-FIM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-DCIT-CODIGO-RETORNO EQUAL 3
               MOVE 1                  TO GFCTNE-ERRO
               MOVE ZEROS              TO GFCTNE-COD-SQL-ERRO
               MOVE 0738               TO GFCTNE-COD-MSG-ERRO
               MOVE '0738'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNE-DESC-MSG-ERRO
               MOVE 'S'                TO GFCTNE-FIM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-DCIT-CODIGO-RETORNO EQUAL  4
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'DCITV002'         TO WRK-NOM-TAB
               MOVE 'V01_UF'           TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0130'             TO GFCT0M-LOCAL
               MOVE  0005              TO GFCTNE-COD-MSG-ERRO
               MOVE '0005'             TO GFCTG2-COD-MSG
               MOVE 9                  TO GFCTNE-ERRO
               MOVE 'GFCT0420'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE WRK-SQLCA          TO SQLCA
                                          GFCT0M-SQLCA-AREA
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-COD-SQL-ERRO
HEXA           MOVE  WRK-COD-SQL-ERRO-2 TO GFCTNE-COD-SQL-ERRO
               MOVE SPACES             TO GFCTNE-DESC-MSG-ERRO
               MOVE 'S'                TO GFCTNE-FIM
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2471-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *PESQUISAR NOME TARIFA                                           *
      ******************************************************************
      *----------------------------------------------------------------*
       2480-PESQUISAR-TARIFA           SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0B2
           TO   CSERVC-TARIF           OF GFCTB0D8.

           EXEC SQL
               SELECT  RSERVC_TARIF_REDZD
               INTO    :GFCTB0D8.RSERVC-TARIF-REDZD
               FROM    DB2PRD.SERVC_TARIF_PRINC
               WHERE
                   CSERVC_TARIF        = :GFCTB0D8.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0D8'      TO WRK-NOM-TAB
               MOVE 'SERVC_RATIF_PRINC'
                                       TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0130'             TO GFCT0M-LOCAL
               MOVE  0010              TO GFCTNE-COD-MSG-ERRO
HEXA           MOVE  SQLCODE            TO WRK-COD-SQL-ERRO
HEXA           MOVE  WRK-COD-SQL-ERRO-2 TO GFCTNE-COD-SQL-ERRO
               MOVE  9                 TO GFCTNE-ERRO
               PERFORM 1410-FORMATAR-ERRO-DB2
               MOVE 'S'                TO GFCTNE-FIM
               MOVE GFCTG3-DESC-MSG    TO GFCTNE-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2480-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

0310  ******************************************************************
0310  *SELECIONAR RAZAO                                                *
0310  ******************************************************************
0310  *----------------------------------------------------------------*
0310   2490-SELECIONAR-RAZAO           SECTION.
0310  *----------------------------------------------------------------*
0310
0310       MOVE CSERVC-TARIF           OF GFCTB0B2
0310                                   TO CSERVC-TARIF       OF GFCTB0M7
0310       MOVE CAGPTO-CTA             OF GFCTB0B2
0310                                   TO CAGPTO-CTA         OF GFCTB0M7
0310       MOVE HIDTFD-FLEXZ-AGPTO     OF GFCTB0B2
0310                                   TO HIDTFD-FLEXZ-AGPTO OF GFCTB0M7
0310
0310       EXEC SQL
0310           SELECT
0310                  CGRP_CTBIL_ATUAL,
0310                  CSGRP_CTBIL
0310           INTO
0310                 :GFCTB0M7.CGRP-CTBIL-ATUAL,
0310                 :GFCTB0M7.CSGRP-CTBIL
0310           FROM   DB2PRD.TPDIDO_FLEXZ_RZ
0310           WHERE
0310               CAGPTO_CTA          = :GFCTB0M7.CAGPTO-CTA      AND
0310               CSERVC_TARIF        = :GFCTB0M7.CSERVC-TARIF    AND
0310               HIDTFD_FLEXZ_AGPTO  = :GFCTB0M7.HIDTFD-FLEXZ-AGPTO
0310       END-EXEC.
0310
0310       IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
0310          (SQLWARN0                EQUAL 'W'      )
0310           INITIALIZE              GFCT0M-ERRO-SQL
0310           MOVE '0010'             TO GFCTG2-COD-MSG
0310           PERFORM 1310-OBTER-DESC-MSG
0310           MOVE ' - GFCTB0M7'      TO WRK-NOM-TAB
0310           MOVE 'TPDIDO_FLEXZ_RZ  '
0310                                   TO GFCT0M-NOME-TAB
0310           MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
0310           MOVE '0070'             TO GFCT0M-LOCAL
0310           MOVE  0010              TO GFCTNE-COD-MSG-ERRO
HEXA           MOVE  SQLCODE            TO WRK-COD-SQL-ERRO
HEXA           MOVE  WRK-COD-SQL-ERRO-2 TO GFCTNE-COD-SQL-ERRO
0310           MOVE  9                 TO GFCTNE-ERRO
0310           PERFORM 1410-FORMATAR-ERRO-DB2
0310           MOVE 'S'                TO GFCTNE-FIM
0310           MOVE GFCTG3-DESC-MSG    TO GFCTNE-DESC-MSG-ERRO
0310           PERFORM 3000-FINALIZAR
0310       END-IF.
0310
0310       IF  SQLCODE                 EQUAL +100
0310           MOVE  1                 TO GFCTNE-ERRO
0310           MOVE ZEROS              TO GFCTNE-COD-SQL-ERRO
0310           MOVE  0926              TO GFCTNE-COD-MSG-ERRO
0310           MOVE '0926'             TO GFCTG2-COD-MSG
0310           PERFORM 1310-OBTER-DESC-MSG
0310           MOVE 'S'                TO GFCTNE-FIM
0310           MOVE GFCTG3-DESC-MSG    TO GFCTNE-DESC-MSG-ERRO
0310           PERFORM 3000-FINALIZAR
0310       END-IF.
0310
0510       PERFORM 2495-BUSCAR-DESCRICAO-CONTABIL.
0310
0310       MOVE CGRP-CTBIL-ATUAL       OF GFCTB0M7
0310                                   TO GFCTNE-GRUPO-CTBIL    (IND-1)
0310       MOVE CSGRP-CTBIL            OF GFCTB0M7
0310                                   TO GFCTNE-SUB-GRUPO-CTBIL(IND-1).
0510       MOVE IRZ-CTBIL              OF PLCCB004
0310                                   TO GFCTNE-DESC-CTBIL(IND-1).
0310
0310  *---------------------------------------------------------------*
0310   2490-99-FIM.                    EXIT.
0310  *----------------------------------------------------------------*

      ******************************************************************
      *ACESSAR PLCC PARA BUSCAR DESCRICAO RAZAO CONTABIL               *
      ******************************************************************
0510  *----------------------------------------------------------------*
0510   2495-BUSCAR-DESCRICAO-CONTABIL  SECTION.
0510  *----------------------------------------------------------------*
0510
0510       EXEC SQL
0510           SELECT  IRZ_CTBIL
0510           INTO   :IRZ-CTBIL
0510           FROM    DB2PRD.RAZAO_CONTABIL
0510           WHERE   CEMPR = 237
0510           AND     CGRP_CTBIL_ATUAL =:GFCTB0M7.CGRP-CTBIL-ATUAL
0510           AND     CSGRP_CTBIL      =:GFCTB0M7.CSGRP-CTBIL
0510       END-EXEC.
0510
0510       IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
0510          (SQLWARN0                EQUAL 'W'      )
0510           INITIALIZE              GFCT0M-ERRO-SQL
0510           MOVE '0010'             TO GFCTG2-COD-MSG
0510           PERFORM 1310-OBTER-DESC-MSG
0510           MOVE ' - PLCCB004'      TO WRK-NOM-TAB
0510           MOVE 'RAZAO_CONTABIL   '
0510                                   TO GFCT0M-NOME-TAB
0510           MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
0510           MOVE '0075'             TO GFCT0M-LOCAL
0510           MOVE  0010              TO GFCTNE-COD-MSG-ERRO
HEXA           MOVE  SQLCODE            TO WRK-COD-SQL-ERRO
HEXA           MOVE  WRK-COD-SQL-ERRO-2 TO GFCTNE-COD-SQL-ERRO
0510           MOVE  9                 TO GFCTNE-ERRO
0510           PERFORM 1410-FORMATAR-ERRO-DB2
0510           MOVE 'S'                TO GFCTNE-FIM
0510           MOVE GFCTG3-DESC-MSG    TO GFCTNE-DESC-MSG-ERRO
0510           PERFORM 3000-FINALIZAR
0510       END-IF.
0510
0510       IF  SQLCODE                 EQUAL +100
0510           MOVE  1                 TO GFCTNE-ERRO
0510           MOVE ZEROS              TO GFCTNE-COD-SQL-ERRO
0510           MOVE  1690              TO GFCTNE-COD-MSG-ERRO
0510           MOVE '1690'             TO GFCTG2-COD-MSG
0510           PERFORM 1310-OBTER-DESC-MSG
0510           MOVE 'S'                TO GFCTNE-FIM
0510           MOVE GFCTG3-DESC-MSG    TO GFCTNE-DESC-MSG-ERRO
0510           PERFORM 3000-FINALIZAR
0510       END-IF.

      *----------------------------------------------------------------*
       2495-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      ******************************************************************
      *FECHAMENTO DO CURSOR                                            *
      ******************************************************************
      *----------------------------------------------------------------*
       2500-CLOSE-CURSOR1              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR01-GFCTB0B2
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0011'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0B2'      TO WRK-NOM-TAB
               MOVE 'PDIDO_FLEXZ_AGPTO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE '0130'             TO GFCT0M-LOCAL
               MOVE  0011              TO GFCTNE-COD-MSG-ERRO
               PERFORM 1410-FORMATAR-ERRO-DB2
               MOVE GFCTG3-DESC-MSG    TO GFCTNE-DESC-MSG-ERRO
               MOVE 'S'                TO GFCTNE-FIM
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *RETORNA AO CHAMADOR                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
