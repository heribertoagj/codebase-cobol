      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT0913.
       AUTHOR.     JEFERSON PAULO DALPONTE.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT0913                                    *
      *    PROGRAMADOR.:   JEFERSON PAULO DALPONTE - CPM PATO BRANCO   *
      *    ANALISTA CPM:   ADRIANO LOPES SANT'ANNA - CPM PATO BRANCO   *
      *    ANALISTA....:   RENATO TAMANAHA         - PROCWORK /GP.50   *
      *    DATA........:   22/06/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   GERAR DADOS PARA DEMONSTRATIVO DEBITO       *
      *      PARCIAL.                                                  *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                     INCLUDE/BOOK        *
TESTE *                   DB2PRD.TDEB_PCIAL_TARIF    GFCTB0H7          *
      *                   DB2PRD.MOVTO_EVNTO_CRRTT   GFCTB092          *
      *                   DB2PRD.V01CLIENTE_AGENCIA  CLIEV004          *
TESTE *                   DB2PRD.V01CTA_POUPANCA     CLIEV007          *
      *                   DB2PRD.V01CTA_CORRENTE     CLIEV008          *
      *                   DB2PRD.HIST_TARIF_PRINC    GFCTB087          *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#GFCT0M - AREA DE COMUNICACAO - ERROS.                     *
      *    I#GFCTG2 - ENTRADA - AREA DE COMUNICACAO - GFCT5523         *
      *    I#GFCTG3 - SAIDA   - AREA DE COMUNICACAO - GFCT5523         *
      *    I#GFCTTE - DEBITO PARCIAL ENTRADA.                          *
      *    I#GFCTTF - DEBITO PARCIAL SAIDA.                            *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    POOL0081 - ALOCA DINAMICAMENTE UM MODULO.                   *
      *    GFCT5523 - OBTEM DESCRICAO DE MENSAGENS.                    *
      *    SENH0315 - OBTER JUNCAO.                                    *
      *================================================================*

HEXA  *
HEXA  *----------------------------------------------------------------*
HEXA  * ***  HEXASOLUTION - 03/2009 - CONVERSAO FAIXA DE AGENCIAS  *** *
HEXA  *----------------------------------------------------------------*
HEXA  *
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

      *----------------------------------------------------------------*
       77  FILLER                      PIC X(050)          VALUE
           '* AREA PARA INDEXADORES *'.
      *----------------------------------------------------------------*

       77  IND-1                       PIC 9(003) COMP-3   VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC X(050)          VALUE
           '* AREA PARA ACUMULADORES *'.
      *----------------------------------------------------------------*

       77  ACU-CONTADOR                PIC 9(003) COMP-3   VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)          VALUE
           '* AREA PARA VARIAVEIS AUXILIARES *'.
      *----------------------------------------------------------------*

       77  WRK-FINALIZAR               PIC  X(001)         VALUE SPACES.
       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-TABELA                  PIC  X(008)         VALUE SPACES.
       77  WRK-VALOR                   PIC S9(009) COMP    VALUE ZEROS.
       77  WRK-TARIFA-INI              PIC S9(005) COMP-3  VALUE ZEROS.
       77  WRK-TARIFA-FIM              PIC S9(005) COMP-3  VALUE ZEROS.
       77  WRK-PERIODO-INI             PIC  X(010)         VALUE SPACES.
       77  WRK-PERIODO-FIM             PIC  X(010)         VALUE SPACES.

       01  WRK-MSG05.
           05 FILLER                   PIC X(026)          VALUE
              'ERRO NA CHAMADA DO MODULO '.
           05 WRK-NOME-MOD             PIC X(008)          VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)          VALUE
           '* AREA PARA VARIAVEIS DE DATAS *'.
      *----------------------------------------------------------------*

       01  WRK-DATA.
           05 WRK-DIA                  PIC 9(002)          VALUE ZEROS.
           05 WRK-MES                  PIC 9(002)          VALUE ZEROS.
           05 WRK-ANO                  PIC 9(004)          VALUE ZEROS.
       01  WRK-DATA-R                  REDEFINES WRK-DATA
                                       PIC 9(008).
       01  WRK-DATA-INI.
           05 WRK-ANO-INI              PIC 9(004)          VALUE ZEROS.
           05 WRK-MES-INI              PIC 9(002)          VALUE ZEROS.
           05 WRK-DIA-INI              PIC 9(002)          VALUE ZEROS.

       01  WRK-DATA-FIM.
           05 WRK-ANO-FIM              PIC 9(004)          VALUE ZEROS.
           05 WRK-MES-FIM              PIC 9(002)          VALUE ZEROS.
           05 WRK-DIA-FIM              PIC 9(002)          VALUE ZEROS.

       01  WRK-DATA-DB2.
           05 WRK-DIA-DB2              PIC 9(002)          VALUE ZEROS.
           05 FILLER                   PIC X(001)          VALUE '.'.
           05 WRK-MES-DB2              PIC 9(002)          VALUE ZEROS.
           05 FILLER                   PIC X(001)          VALUE '.'.
           05 WRK-ANO-DB2              PIC 9(004)          VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)          VALUE
           '* AREA DO MODULO SENH *'.
      *----------------------------------------------------------------*

       01  WRK-VERSAO-0315             PIC X(006)          VALUE
           'VRS001'.
       01  WRK-MENSAGEM-0315.
           05 WRK-COD-MSG-0315         PIC 9(003)          VALUE ZEROS.
           05 WRK-HIFEN-MENSAGEM-0315  PIC X(001)          VALUE SPACES.
           05 WRK-TEXTO-MENSAGEM-0315  PIC X(079)          VALUE SPACES.
       01  WRK-COD-FUNCIONAL-0315      PIC X(007)          VALUE SPACES.
       01  WRK-NOME-FUNC-0315          PIC X(040)          VALUE SPACES.
       01  WRK-COD-BANCO-0315          PIC 9(005)          VALUE ZEROS.
       01  WRK-NOME-BANCO-0315         PIC X(040)          VALUE SPACES.
       01  WRK-CNPJ-BANCO-0315.
ST25X6*    05 WRK-CNPJ-PRI-0315        PIC 9(009)          VALUE ZEROS.
ST25X6*    05 WRK-CNPJ-FIL-0315        PIC 9(005)          VALUE ZEROS.
           05 WRK-CNPJ-PRI-0315        PIC X(009)          VALUE SPACES.
           05 WRK-CNPJ-FIL-0315        PIC X(005)          VALUE SPACES.
           05 WRK-CNPJ-CON-0315        PIC 9(002)          VALUE ZEROS.
       01  WRK-COD-JUNCAO-0315         PIC 9(005)          VALUE ZEROS.
       01  WRK-NOME-DEPTO-0315         PIC X(040)          VALUE SPACES.
       01  WRK-SECAO-0315              PIC 9(003)          VALUE ZEROS.
       01  WRK-STATUS-0315             PIC X(001)          VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)           VALUE
           '* AREA PARA MODULO GFCT5523 *'.
      *----------------------------------------------------------------*

       COPY I#GFCTG2.

       COPY I#GFCTG3.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)          VALUE
           '* AREAS DE TABELAS DB2 *'.
      *----------------------------------------------------------------*

           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.

TESTE      EXEC SQL
TESTE           INCLUDE GFCTB0H7
TESTE      END-EXEC.

           EXEC SQL
                INCLUDE GFCTB092
           END-EXEC.

           EXEC SQL
                INCLUDE CLIEV004
           END-EXEC.

TESTE      EXEC SQL
TESTE           INCLUDE CLIEV007
TESTE      END-EXEC.

           EXEC SQL
                INCLUDE CLIEV008
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB087
           END-EXEC.

TESTE *--- EXEC SQL
TESTE *---   DECLARE CSR01-GFCTB092 CURSOR FOR
TESTE *---     SELECT DENVIO_MOVTO_TARIF,
TESTE *---            CROTNA_ORIGE_MOVTO,
TESTE *---            CNRO_ARQ_MOVTO,
TESTE *---            CSEQ_MOVTO,
TESTE *---            CSERVC_TARIF,
TESTE *---            QEVNTO_AGRUP,
TESTE *---            VTARIF_BRUTO_MOVTO,
TESTE *---            VTARIF_LIQ_MOVTO,
TESTE *---            VTARIF_DEB_MOVTO,
TESTE *---            CSIT_EVNTO_RECBD,
TESTE *---            DOCOR_EVNTO,
TESTE *---            VTARIF_CADTR_SIST
TESTE *---     FROM   DB2PRD.MOVTO_EVNTO_CRRTT
TESTE *---     WHERE  CAG_DSTNO_MOVTO  = :GFCTB092.CAG-DSTNO-MOVTO
TESTE *---      AND   CCTA_DSTNO_MOVTO = :GFCTB092.CCTA-DSTNO-MOVTO
TESTE *---      AND   DOCOR_EVNTO     >= :WRK-PERIODO-INI
TESTE *---      AND   DOCOR_EVNTO     <= :WRK-PERIODO-FIM
TESTE *---      AND   CSERVC_TARIF    >= :WRK-TARIFA-INI
TESTE *---      AND   CSERVC_TARIF    <= :WRK-TARIFA-FIM
TESTE *---     ORDER BY
TESTE *---            DOCOR_EVNTO,
TESTE *---            CSERVC_TARIF,
TESTE *---            CNRO_ARQ_MOVTO,
TESTE *---            CSEQ_MOVTO
TESTE *--- END-EXEC.

TESTE      EXEC SQL
TESTE        DECLARE CSR01-GFCTB092 CURSOR FOR
TESTE          SELECT A.DENVIO_MOVTO_TARIF,
TESTE                 A.CROTNA_ORIGE_MOVTO,
TESTE                 A.CNRO_ARQ_MOVTO,
TESTE                 A.CSEQ_MOVTO,
TESTE                 A.CSERVC_TARIF,
TESTE                 A.QEVNTO_AGRUP,
TESTE                 A.VTARIF_BRUTO_MOVTO,
TESTE                 A.VTARIF_LIQ_MOVTO,
TESTE                 A.VTARIF_DEB_MOVTO,
TESTE                 A.CSIT_EVNTO_RECBD,
TESTE                 A.DOCOR_EVNTO,
TESTE                 A.VTARIF_CADTR_SIST
TESTE          FROM   DB2PRD.MOVTO_EVNTO_CRRTT A
TESTE          WHERE  A.CAG_DSTNO_MOVTO    = :GFCTB092.CAG-DSTNO-MOVTO
TESTE           AND   A.CCTA_DSTNO_MOVTO   = :GFCTB092.CCTA-DSTNO-MOVTO
TESTE           AND   A.DOCOR_EVNTO       >= :WRK-PERIODO-INI
TESTE           AND   A.DOCOR_EVNTO       <= :WRK-PERIODO-FIM
TESTE           AND   A.CSERVC_TARIF      >= :WRK-TARIFA-INI
TESTE           AND   A.CSERVC_TARIF      <= :WRK-TARIFA-FIM
TESTE           AND   EXISTS (SELECT B.DENVIO_MOVTO_TARIF
TESTE                         FROM   DB2PRD.TDEB_PCIAL_TARIF B
TESTE                         WHERE  A.DENVIO_MOVTO_TARIF
TESTE                                            = B.DENVIO_MOVTO_TARIF
TESTE                          AND   A.CROTNA_ORIGE_MOVTO
TESTE                                            = B.CROTNA_ORIGE_MOVTO
TESTE                          AND   A.CNRO_ARQ_MOVTO
TESTE                                            = B.CNRO_ARQ_MOVTO
TESTE                          AND   A.CSEQ_MOVTO
TESTE                                            = B.CSEQ_MOVTO)
TESTE          ORDER BY
TESTE                 A.DOCOR_EVNTO,
TESTE                 A.CSERVC_TARIF,
TESTE                 A.CNRO_ARQ_MOVTO,
TESTE                 A.CSEQ_MOVTO
TESTE      END-EXEC.
HEXA  *
HEXA  *---------------------------------------------------------------*
HEXA  * AREA DE DECLARACAO PARA SUPORTE A MULTIPLAS FAIXAS DE AGENCIA *
HEXA  *---------------------------------------------------------------*
HEXA  *
HEXA       COPY I#HEXA01.
HEXA  *
HEXA       COPY I#BRAD7C.
HEXA  *

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)          VALUE
           '* FIM DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY I#GFCTTE.

       COPY I#GFCTTF.

       COPY I#GFCT0M.

      *================================================================*
       PROCEDURE DIVISION              USING GFCTTE-ENTRADA
                                             GFCTTF-SAIDA
                                             GFCT0M-AREA-ERROS.
      *================================================================*

      ******************************************************************
      *ROTINA PRINCIPAL DO PROGRAMA                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*
HEXA  *
HEXA  *---------------------------------------------------------------*
HEXA  * AREA DE MOVIMENTACAO DA ROTINA MESU E DE ABEND                *
HEXA  *---------------------------------------------------------------*
HEXA  *
HEXA       MOVE        'MESU9410'  TO          WRK-MODULO-MESUX0.
HEXA       MOVE        'BRAD7100'  TO          WRK-ABEND-MESUX0.
HEXA  *

           PERFORM 1000-INICIALIZAR-PROGRAMA.

           PERFORM 2000-PROCESSAR-DADOS.

           PERFORM 3000-FINALIZAR-PROGRAMA.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA INICIALIZAR PROGRAMA                                *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR-PROGRAMA       SECTION.
      *----------------------------------------------------------------*

           PERFORM 1100-INICIALIZAR-SAIDA.

           PERFORM 1200-CONSISTIR-DADOS.

           PERFORM 1300-OBTER-CODIGO-JUNCAO.

           PERFORM 1400-CONSISTE-CONTA-TARIFA.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA INICIALIZAR SAIDA                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-INICIALIZAR-SAIDA          SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTTF-SAIDA.

           INITIALIZE                  GFCTTF-SAIDA.

           MOVE 282                    TO GFCTTF-LL.
           MOVE ZEROS                  TO GFCTTF-ZZ.
           MOVE GFCTTE-TRANSACAO       TO GFCTTF-TRANSACAO.
           MOVE GFCTTE-FUNCAO          TO GFCTTF-FUNCAO.

TESTE *--- IF  GFCTTE-AGENCIA          NUMERIC
               MOVE GFCTTE-AGENCIA     TO GFCTTF-AGENCIA
TESTE *--- END-IF.

TESTE *--- IF  GFCTTE-CONTA            NUMERIC
               MOVE GFCTTE-CONTA       TO GFCTTF-CONTA
TESTE *--- END-IF.

           MOVE GFCTTE-NOME-CLIENTE    TO GFCTTF-NOME-CLIENTE.
           MOVE GFCTTE-PERIODO-INI     TO GFCTTF-PERIODO-INI.
           MOVE GFCTTE-PERIODO-FIM     TO GFCTTF-PERIODO-FIM.

           IF  GFCTTE-COD-TARIFA       NOT NUMERIC OR
               GFCTTE-COD-TARIFA       EQUAL ZEROS
               MOVE ZEROS              TO GFCTTF-COD-TARIFA
                                          WRK-TARIFA-INI
               MOVE 999999             TO WRK-TARIFA-FIM
           ELSE
               MOVE GFCTTE-COD-TARIFA  TO GFCTTF-COD-TARIFA
                                          WRK-TARIFA-INI
                                          WRK-TARIFA-FIM
           END-IF.

           MOVE GFCTTE-NOME-TARIFA     TO GFCTTF-NOME-TARIFA.

           IF  GFCTTE-OCOR-ATUAL       NOT NUMERIC
               MOVE ZEROS              TO GFCTTF-OCOR-ATUAL
                                          GFCTTE-OCOR-ATUAL
           ELSE
               MOVE GFCTTE-OCOR-ATUAL  TO GFCTTF-OCOR-ATUAL
           END-IF.

           IF  GFCTTE-OCOR-TOTAL       NOT NUMERIC
               MOVE ZEROS              TO GFCTTE-OCOR-TOTAL
           ELSE
               MOVE GFCTTE-OCOR-TOTAL  TO GFCTTF-OCOR-TOTAL
           END-IF.

           MOVE 'N'                    TO GFCTTF-DETALHES
                                          GFCTTF-CONTINUA.

           IF  GFCTTE-QTDE-OCOR        NUMERIC
               MOVE GFCTTE-QTDE-OCOR   TO GFCTTF-QTDE-OCOR
           END-IF.

           IF  GFCTTE-QTDE-TOT-REG     NUMERIC
               MOVE GFCTTE-QTDE-TOT-REG
                                       TO GFCTTF-QTDE-TOT-REG
           END-IF.

           IF  GFCTTE-DEPTO-BDSCO      NOT NUMERIC
               MOVE ZEROS              TO GFCTTF-DEPTO-BDSCO
           ELSE
               MOVE GFCTTE-DEPTO-BDSCO TO GFCTTF-DEPTO-BDSCO
           END-IF.

           MOVE GFCTTE-FUNC-BDSCO      TO GFCTTF-FUNC-BDSCO.
           MOVE GFCTTE-TERMINAL        TO GFCTTF-TERMINAL.

           IF  GFCTTE-PONTEIRO         NUMERIC
               MOVE GFCTTE-PONTEIRO    TO GFCTTF-PONTEIRO
           END-IF.

           MOVE ZEROS                  TO GFCTTF-ERRO
                                          GFCTTF-COD-SQL-ERRO
                                          GFCTTF-COD-MSG-ERRO
                                          GFCTG2-COD-MSG.

           PERFORM 1110-OBTER-DESC-MSG.

           MOVE GFCTG3-DESC-MSG        TO GFCTTF-DESC-MSG-ERRO.

           MOVE 'N'                    TO GFCTTF-FIM.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA OBTER DESCRICAO DA MENSAGEM                         *
      ******************************************************************
      *----------------------------------------------------------------*
       1110-OBTER-DESC-MSG             SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTG3-SAIDA.

           INITIALIZE                  GFCTG3-SAIDA.

           MOVE 100                    TO GFCTG2-LL.
           MOVE ZEROS                  TO GFCTG2-ZZ.
           MOVE GFCTTE-TRANSACAO       TO GFCTG2-TRANSACAO.
           MOVE GFCTTE-FUNCAO          TO GFCTG2-FUNCAO.
           MOVE GFCTTE-FUNC-BDSCO      TO GFCTG2-FUNC-BDSCO.
           MOVE 'O'                    TO GFCTG2-TIPO-PROC.
           MOVE 'N'                    TO GFCTG2-FIM.

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
               MOVE 'GFCT0913'         TO GFCT0M-TRANSACAO
               MOVE 9                  TO GFCTTF-ERRO
               MOVE 'S'                TO GFCTTF-FIM
               PERFORM 3000-FINALIZAR-PROGRAMA
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 9
               IF  GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2'
                   MOVE WRK-MODULO     TO GFCT0M-PROGRAMA
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
                   MOVE 'GFCT0913'     TO GFCT0M-TRANSACAO
               END-IF
               MOVE 9                  TO GFCTTF-ERRO
               MOVE 'S'                TO GFCTTF-FIM
               PERFORM 3000-FINALIZAR-PROGRAMA
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA CONSISTIR  DADOS                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       1200-CONSISTIR-DADOS            SECTION.
      *----------------------------------------------------------------*

           IF (GFCTTE-TRANSACAO            EQUAL SPACES)          OR
              (GFCTTE-FUNCAO               EQUAL SPACES)          OR
              (GFCTTE-AGENCIA              NOT NUMERIC)           OR
              (GFCTTE-CONTA                NOT NUMERIC)           OR
              (GFCTTE-PERIODO-INI(01:02)   NOT NUMERIC            OR
               GFCTTE-PERIODO-INI(04:02)   NOT NUMERIC            OR
               GFCTTE-PERIODO-INI(07:04)   NOT NUMERIC)           OR
              (GFCTTE-PERIODO-FIM(01:02)   NOT NUMERIC            OR
               GFCTTE-PERIODO-FIM(04:02)   NOT NUMERIC            OR
               GFCTTE-PERIODO-FIM(07:04)   NOT NUMERIC)           OR
              (GFCTTE-QTDE-OCOR            NOT NUMERIC)           OR
              (GFCTTE-QTDE-TOT-REG         NOT NUMERIC)           OR
              (GFCTTE-FUNC-BDSCO           EQUAL SPACES)          OR
              (GFCTTE-PONTEIRO             NOT NUMERIC)           OR
              (GFCTTE-FIM                  NOT EQUAL 'S' AND 'N')
               MOVE 1                      TO GFCTTF-ERRO
               MOVE ZEROS                  TO GFCTTF-COD-SQL-ERRO
               MOVE 0001                   TO GFCTTF-COD-MSG-ERRO
                                              GFCTG2-COD-MSG
               MOVE 'S'                    TO GFCTTF-FIM
               PERFORM 1110-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG        TO GFCTTF-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR-PROGRAMA
           END-IF.

           MOVE GFCTTE-PERIODO-INI         TO WRK-DATA-DB2.
           MOVE WRK-DIA-DB2                TO WRK-DIA.
           MOVE WRK-MES-DB2                TO WRK-MES.
           MOVE WRK-ANO-DB2                TO WRK-ANO.

           CALL 'POOL1470'                 USING WRK-DATA-R.

           IF  RETURN-CODE                 NOT EQUAL ZEROS
               MOVE 1                      TO GFCTTF-ERRO
               MOVE ZEROS                  TO GFCTTF-COD-SQL-ERRO
               MOVE 0200                   TO GFCTTF-COD-MSG-ERRO
                                              GFCTG2-COD-MSG
               MOVE 'S'                    TO GFCTTF-FIM
               PERFORM 1110-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG        TO GFCTTF-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR-PROGRAMA
           END-IF.

           MOVE GFCTTE-PERIODO-FIM         TO WRK-DATA-DB2.
           MOVE WRK-DIA-DB2                TO WRK-DIA.
           MOVE WRK-MES-DB2                TO WRK-MES.
           MOVE WRK-ANO-DB2                TO WRK-ANO.

           CALL 'POOL1470'                 USING WRK-DATA-R.

           IF  RETURN-CODE                 NOT EQUAL ZEROS
               MOVE 1                      TO GFCTTF-ERRO
               MOVE ZEROS                  TO GFCTTF-COD-SQL-ERRO
               MOVE 0200                   TO GFCTTF-COD-MSG-ERRO
               MOVE '0200'                 TO GFCTG2-COD-MSG
               MOVE 'S'                    TO GFCTTF-FIM
               PERFORM 1110-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG        TO GFCTTF-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR-PROGRAMA
           END-IF.

           MOVE GFCTTE-PERIODO-FIM         TO WRK-DATA-DB2.
           MOVE WRK-DIA-DB2                TO WRK-DIA-FIM.
           MOVE WRK-MES-DB2                TO WRK-MES-FIM.
           MOVE WRK-ANO-DB2                TO WRK-ANO-FIM.
           MOVE GFCTTE-PERIODO-INI         TO WRK-DATA-DB2.
           MOVE WRK-DIA-DB2                TO WRK-DIA-INI.
           MOVE WRK-MES-DB2                TO WRK-MES-INI.
           MOVE WRK-ANO-DB2                TO WRK-ANO-INI.

           IF  WRK-DATA-FIM                LESS  WRK-DATA-INI
               MOVE 1                      TO GFCTTF-ERRO
               MOVE ZEROS                  TO GFCTTF-COD-SQL-ERRO
               MOVE 0201                   TO GFCTTF-COD-MSG-ERRO
               MOVE '0201'                 TO GFCTG2-COD-MSG
               MOVE 'S'                    TO GFCTTF-FIM
               PERFORM 1110-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG        TO GFCTTF-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR-PROGRAMA
           END-IF.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA OBTER CODIGO DA JUNCAO                              *
      ******************************************************************
      *----------------------------------------------------------------*
       1300-OBTER-CODIGO-JUNCAO        SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTTE-FUNC-BDSCO      TO WRK-COD-FUNCIONAL-0315.

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
               MOVE 1                  TO GFCTTF-ERRO
               MOVE RETURN-CODE        TO GFCTTF-COD-SQL-ERRO
               MOVE 0056               TO GFCTTF-COD-MSG-ERRO
               MOVE '0056'             TO GFCTG2-COD-MSG
               MOVE 'S'                TO GFCTTF-FIM
               PERFORM 1110-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTTF-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR-PROGRAMA
           END-IF.

           IF  RETURN-CODE             EQUAL 04
               IF  WRK-COD-MSG-0315    EQUAL 001
                   MOVE 1              TO GFCTTF-ERRO
                   MOVE ZEROS          TO GFCTTF-COD-SQL-ERRO
                   MOVE 0057           TO GFCTTF-COD-MSG-ERRO
                   MOVE '0057'         TO GFCTG2-COD-MSG
                   MOVE 'S'            TO GFCTTF-FIM
                   PERFORM 1110-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG
                                       TO GFCTTF-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR-PROGRAMA
               END-IF
               IF  WRK-COD-MSG-0315    EQUAL 154
                   MOVE 1              TO GFCTTF-ERRO
                   MOVE ZEROS          TO GFCTTF-COD-SQL-ERRO
                   MOVE 0058           TO GFCTTF-COD-MSG-ERRO
                   MOVE '0058'         TO GFCTG2-COD-MSG
                   MOVE 'S'            TO GFCTTF-FIM
                   PERFORM 1110-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG
                                       TO GFCTTF-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR-PROGRAMA
               END-IF
               IF  WRK-COD-MSG-0315    EQUAL 242
                   MOVE 1              TO GFCTTF-ERRO
                   MOVE ZEROS          TO GFCTTF-COD-SQL-ERRO
                   MOVE 0059           TO GFCTTF-COD-MSG-ERRO
                   MOVE '0059'         TO GFCTG2-COD-MSG
                   MOVE 'S'            TO GFCTTF-FIM
                   PERFORM 1110-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG
                                       TO GFCTTF-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR-PROGRAMA
               END-IF
               IF  WRK-COD-MSG-0315    EQUAL 009
                   MOVE 1              TO GFCTTF-ERRO
                   MOVE ZEROS          TO GFCTTF-COD-SQL-ERRO
                                          GFCTTF-COD-MSG-ERRO
                   MOVE '0000'         TO GFCTG2-COD-MSG
                   MOVE 'S'            TO GFCTTF-FIM
                   PERFORM 1110-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG
                                       TO GFCTTF-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR-PROGRAMA
               END-IF
               IF  WRK-COD-MSG-0315    EQUAL 138
                   MOVE 1              TO GFCTTF-ERRO
                   MOVE ZEROS          TO GFCTTF-COD-SQL-ERRO
                   MOVE 0060           TO GFCTTF-COD-MSG-ERRO
                   MOVE '0060'         TO GFCTG2-COD-MSG
                   MOVE 'S'            TO GFCTTF-FIM
                   PERFORM 1110-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG
                                       TO GFCTTF-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR-PROGRAMA
               END-IF
           END-IF.

           MOVE WRK-COD-JUNCAO-0315    TO GFCTTF-DEPTO-BDSCO
                                          GFCTTE-DEPTO-BDSCO.

HEXA  *----------------------------------------------------------------*
HEXA  *            INICIO     DO     TEMPLATE_IF
HEXA  *----------------------------------------------------------------*
HEXA  ***  ALTERADO PARA SUPORTE A MULTIPLAS FAIXAS DE AGENCIA
HEXA  *
HEXA  * HX_IF  WRK-COD-JUNCAO-0315     NOT LESS 4000
HEXA  *
HEXA       MOVE        WRK-COD-JUNCAO-0315 TO  WRK-MESUX0-AGENCIA
HEXA       PERFORM     9990-10-CHAMA-ROTINA-FX-AG
HEXA  *
HEXA       IF  WRK-NAO-EH-FX-AG
HEXA  *
HEXA  *----------------------------------------------------------------*
HEXA  *            TERMINO    DO     TEMPLATE_IF
HEXA  *----------------------------------------------------------------*
HEXA  *
               MOVE 'S'                TO GFCTTF-DETALHES
           ELSE
               MOVE 'N'                TO GFCTTF-DETALHES
           END-IF.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA CONSISTIR CONTA TARIFA                              *
      ******************************************************************
      *----------------------------------------------------------------*
       1400-CONSISTE-CONTA-TARIFA      SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTTE-AGENCIA         TO CJUNC-DEPDC  OF CLIEV008.
           MOVE GFCTTE-CONTA           TO CCTA-CLI     OF CLIEV008.

TESTE      PERFORM  1410-PESQUISA-CTA-CORRENTE.

TESTE      IF  SQLCODE                 EQUAL +100
TESTE          MOVE GFCTTE-AGENCIA     TO CJUNC-DEPDC  OF CLIEV007
TESTE          MOVE GFCTTE-CONTA       TO CCTA-CLI     OF CLIEV007

TESTE          PERFORM 1420-PESQUISA-CTA-POUPANCA

               IF  SQLCODE             EQUAL +100
                   MOVE 1              TO GFCTTF-ERRO
                   MOVE 'N'            TO GFCTTF-CONTINUA
                   MOVE 'S'            TO WRK-FINALIZAR
                                          GFCTTF-FIM
                   MOVE ZEROS          TO GFCTTF-COD-SQL-ERRO
                   MOVE 0220           TO GFCTTF-COD-MSG-ERRO
                   MOVE '0220'         TO GFCTG2-COD-MSG
                   PERFORM 1110-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG
                                       TO GFCTTF-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR-PROGRAMA
TESTE          ELSE
TESTE              MOVE CID-CLI OF CLIEV007
TESTE                                  TO CID-CLI OF CLIEV004
               END-IF
TESTE      ELSE
               MOVE CID-CLI OF CLIEV008
                                       TO CID-CLI OF CLIEV004
TESTE      END-IF.

           PERFORM  1430-PESQUISA-CLIENTE.

           IF  SQLCODE                 EQUAL +100
               MOVE 1                  TO GFCTTF-ERRO
               MOVE 'N'                TO GFCTTF-CONTINUA
               MOVE 'S'                TO WRK-FINALIZAR
                                          GFCTTF-FIM
               MOVE ZEROS              TO GFCTTF-COD-SQL-ERRO
               MOVE 0220               TO GFCTTF-COD-MSG-ERRO
               MOVE '0220'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTTF-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR-PROGRAMA
           END-IF.

           MOVE IPSSOA-COPLT           OF CLIEV004
                                       TO GFCTTF-NOME-CLIENTE.

           IF  GFCTTF-COD-TARIFA       EQUAL ZEROS
               GO TO 1400-99-FIM
           END-IF.

           MOVE GFCTTE-COD-TARIFA      TO CSERVC-TARIF OF GFCTB087.
TESTE      MOVE 'S'                    TO CINDCD-DEB-PCIAL
TESTE                                                  OF GFCTB087.

TESTE      PERFORM 1440-PESQUISA-HIST-TARIFA.

           IF  SQLCODE                 EQUAL +100
               MOVE 1                  TO GFCTTF-ERRO
               MOVE 'N'                TO GFCTTF-CONTINUA
               MOVE 'S'                TO WRK-FINALIZAR
                                          GFCTTF-FIM
               MOVE ZEROS              TO GFCTTF-COD-SQL-ERRO
               MOVE 0054               TO GFCTTF-COD-MSG-ERRO
               MOVE '0054'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTTF-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR-PROGRAMA
           END-IF.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA PESQUISA NA BASE DE CONTA-CORRENTE                  *
      ******************************************************************
      *----------------------------------------------------------------*
       1410-PESQUISA-CTA-CORRENTE     SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             SELECT CID_CLI
             INTO   :CLIEV008.CID-CLI
             FROM   DB2PRD.V01CTA_CORRENTE
             WHERE  CJUNC_DEPDC        = :CLIEV008.CJUNC-DEPDC
               AND  CCTA_CLI           = :CLIEV008.CCTA-CLI
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0001'             TO GFCT0M-LOCAL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTTF-COD-SQL-ERRO
               MOVE 0005               TO GFCTTF-COD-MSG-ERRO
               MOVE '0005'             TO GFCTG2-COD-MSG
               MOVE 'V01CTA_CORRENTE'  TO GFCT0M-NOME-TAB
               MOVE 'CLIEV008'         TO WRK-TABELA
               PERFORM 1450-MOVE-ERRO-SQL
           END-IF.

      *----------------------------------------------------------------*
       1410-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

TESTE ******************************************************************
TESTE *ROTINA PARA PESQUISA NA BASE DE POUPANCA                        *
TESTE ******************************************************************
TESTE *----------------------------------------------------------------*
TESTE  1420-PESQUISA-CTA-POUPANCA     SECTION.
TESTE *----------------------------------------------------------------*
TESTE
TESTE      EXEC SQL
TESTE        SELECT CID_CLI
TESTE        INTO   :CLIEV007.CID-CLI
TESTE        FROM   DB2PRD.V01CTA_POUPANCA
TESTE        WHERE  CJUNC_DEPDC        = :CLIEV007.CJUNC-DEPDC
TESTE          AND  CCTA_CLI           = :CLIEV007.CCTA-CLI
TESTE      END-EXEC.
TESTE
TESTE      IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
TESTE         (SQLWARN0                EQUAL 'W')
TESTE          INITIALIZE              GFCT0M-ERRO-SQL
TESTE          MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
TESTE          MOVE '0002'             TO GFCT0M-LOCAL
TESTE          MOVE SQLCODE            TO GFCT0M-SQLCODE
TESTE                                     GFCTTF-COD-SQL-ERRO
TESTE          MOVE 0005               TO GFCTTF-COD-MSG-ERRO
TESTE          MOVE '0005'             TO GFCTG2-COD-MSG
TESTE          MOVE 'V01CTA_POUPANCA'  TO GFCT0M-NOME-TAB
TESTE          MOVE 'CLIEV007'         TO WRK-TABELA
TESTE          PERFORM 1450-MOVE-ERRO-SQL
TESTE      END-IF.
TESTE
TESTE *----------------------------------------------------------------*
TESTE  1420-99-FIM.                    EXIT.
TESTE *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA PESQUISA NA BASE DE CLIENTE                         *
      ******************************************************************
      *----------------------------------------------------------------*
       1430-PESQUISA-CLIENTE          SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             SELECT IPSSOA_COPLT
             INTO   :CLIEV004.IPSSOA-COPLT
             FROM   DB2PRD.V01CLIENTE_AGENCIA
             WHERE  CID_CLI            = :CLIEV004.CID-CLI
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0003'             TO GFCT0M-LOCAL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTTF-COD-SQL-ERRO
               MOVE 0005               TO GFCTTF-COD-MSG-ERRO
               MOVE '0005'             TO GFCTG2-COD-MSG
               MOVE 'V01CTA_AGENCIA'   TO GFCT0M-NOME-TAB
               MOVE 'CLIEV004'         TO WRK-TABELA
               PERFORM 1450-MOVE-ERRO-SQL
           END-IF.

      *----------------------------------------------------------------*
       1430-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA PESQUISA NA BASE HISTORICO TARIFA                   *
      ******************************************************************
      *----------------------------------------------------------------*
       1440-PESQUISA-HIST-TARIFA      SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             SELECT RSERVC_TARIF_REDZD
             INTO  :GFCTB087.RSERVC-TARIF-REDZD
             FROM   DB2PRD.HIST_TARIF_PRINC
             WHERE  CSERVC_TARIF       = :GFCTB087.CSERVC-TARIF
TESTE          AND  CINDCD_DEB_PCIAL   = :GFCTB087.CINDCD-DEB-PCIAL
               AND  CSEQ_MANUT_SERVC   IN
            (SELECT MAX(CSEQ_MANUT_SERVC)
             FROM   DB2PRD.HIST_TARIF_PRINC
             WHERE  CSERVC_TARIF       = :GFCTB087.CSERVC-TARIF)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0004'             TO GFCT0M-LOCAL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTTF-COD-SQL-ERRO
               MOVE 0005               TO GFCTTF-COD-MSG-ERRO
               MOVE '0005'             TO GFCTG2-COD-MSG
               MOVE 'HIST_TARIF_PRINC' TO GFCT0M-NOME-TAB
               MOVE 'GFCTB087'         TO WRK-TABELA
               PERFORM 1450-MOVE-ERRO-SQL
           END-IF.

      *----------------------------------------------------------------*
       1440-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA MOVER ERRO SQL                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       1450-MOVE-ERRO-SQL             SECTION.
      *----------------------------------------------------------------*

           MOVE 9                      TO GFCTTF-ERRO.
           MOVE 'GFCT0913'             TO GFCT0M-PROGRAMA.
           MOVE 'DB2'                  TO GFCT0M-TIPO-ACESSO.
           MOVE SQLCA                  TO GFCT0M-SQLCA-AREA.

           PERFORM 1110-OBTER-DESC-MSG.

           STRING GFCTG3-DESC-MSG ' - ' WRK-TABELA
           DELIMITED BY '  '           INTO GFCTTF-DESC-MSG-ERRO.

           MOVE 'S'                    TO GFCTTF-FIM.

           PERFORM 3000-FINALIZAR-PROGRAMA.

      *----------------------------------------------------------------*
       1450-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA PROCESSAR DADOS                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-PROCESSAR-DADOS            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTTE-AGENCIA         TO CAG-DSTNO-MOVTO  OF GFCTB092.
           MOVE GFCTTE-CONTA           TO CCTA-DSTNO-MOVTO OF GFCTB092.
           MOVE GFCTTE-PERIODO-INI     TO WRK-PERIODO-INI.
           MOVE GFCTTE-PERIODO-FIM     TO WRK-PERIODO-FIM.
           MOVE 'N'                    TO WRK-FINALIZAR.
           MOVE ZEROS                  TO IND-1.

           IF  GFCTTE-OCOR-TOTAL       EQUAL ZEROS
               PERFORM 2100-CONTAR-OCORRENCIA
           END-IF.

           PERFORM 2200-ABRIR-CURSOR.

           PERFORM 2300-LER-CSR-GFCTB092
             UNTIL WRK-FINALIZAR       EQUAL 'S'
                OR ACU-CONTADOR        GREATER GFCTTE-OCOR-ATUAL.

           MOVE SPACES                 TO GFCTTF-DADOS.

           INITIALIZE GFCTTF-DADOS.

           PERFORM 2400-MONTAR-SAIDA  UNTIL WRK-FINALIZAR EQUAL 'S'.

           PERFORM 2500-FECHAR-CSR-GFCTB092.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA CONTAR OCORRENCIAS                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-CONTAR-OCORRENCIA          SECTION.
      *----------------------------------------------------------------*

TESTE *--- INSPECT GFCTTE-PERIODO-INI  REPLACING ALL '/' BY '.'.
TESTE *--- INSPECT GFCTTE-PERIODO-FIM  REPLACING ALL '/' BY '.'.
           MOVE GFCTTE-PERIODO-INI     TO WRK-PERIODO-INI.
           MOVE GFCTTE-PERIODO-FIM     TO WRK-PERIODO-FIM.

TESTE *--- EXEC SQL
TESTE *---   SELECT COUNT(*)
TESTE *---   INTO   :WRK-VALOR
TESTE *---   FROM   DB2PRD.MOVTO_EVNTO_CRRTT
TESTE *---   WHERE  CAG_DSTNO_MOVTO    = :GFCTB092.CAG-DSTNO-MOVTO
TESTE *---    AND   CCTA_DSTNO_MOVTO   = :GFCTB092.CCTA-DSTNO-MOVTO
TESTE *---    AND   DOCOR_EVNTO       >= :WRK-PERIODO-INI
TESTE *---    AND   DOCOR_EVNTO       <= :WRK-PERIODO-FIM
TESTE *---    AND   CSERVC_TARIF      >= :WRK-TARIFA-INI
TESTE *---    AND   CSERVC_TARIF      <= :WRK-TARIFA-FIM
TESTE *--- END-EXEC.

TESTE      EXEC SQL
TESTE        SELECT COUNT(*)
TESTE        INTO   :WRK-VALOR
TESTE        FROM   DB2PRD.MOVTO_EVNTO_CRRTT A
TESTE        WHERE  A.CAG_DSTNO_MOVTO    = :GFCTB092.CAG-DSTNO-MOVTO
TESTE         AND   A.CCTA_DSTNO_MOVTO   = :GFCTB092.CCTA-DSTNO-MOVTO
TESTE         AND   A.DOCOR_EVNTO       >= :WRK-PERIODO-INI
TESTE         AND   A.DOCOR_EVNTO       <= :WRK-PERIODO-FIM
TESTE         AND   A.CSERVC_TARIF      >= :WRK-TARIFA-INI
TESTE         AND   A.CSERVC_TARIF      <= :WRK-TARIFA-FIM
TESTE         AND   EXISTS (SELECT B.DENVIO_MOVTO_TARIF
TESTE                       FROM   DB2PRD.TDEB_PCIAL_TARIF B
TESTE                       WHERE  A.DENVIO_MOVTO_TARIF
TESTE                                            = B.DENVIO_MOVTO_TARIF
TESTE                        AND   A.CROTNA_ORIGE_MOVTO
TESTE                                            = B.CROTNA_ORIGE_MOVTO
TESTE                        AND   A.CNRO_ARQ_MOVTO
TESTE                                            = B.CNRO_ARQ_MOVTO
TESTE                        AND   A.CSEQ_MOVTO  = B.CSEQ_MOVTO)
TESTE      END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'COUNT'            TO GFCT0M-COMANDO-SQL
               MOVE '0005'             TO GFCT0M-LOCAL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTTF-COD-SQL-ERRO
               MOVE 0501               TO GFCTTF-COD-MSG-ERRO
               MOVE '0501'             TO GFCTG2-COD-MSG
               MOVE 'MOVTO_EVNTO_CRRTT '
                                       TO GFCT0M-NOME-TAB
               MOVE 'GFCTB092'         TO WRK-TABELA
               PERFORM 1450-MOVE-ERRO-SQL
           END-IF.

           MOVE WRK-VALOR              TO GFCTTF-OCOR-TOTAL.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA ABRIR CURSOR                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-ABRIR-CURSOR               SECTION.
      *----------------------------------------------------------------*

TESTE *--- INSPECT WRK-PERIODO-INI     REPLACING ALL '/' BY '.'.
TESTE *--- INSPECT WRK-PERIODO-FIM     REPLACING ALL '/' BY '.'.

           EXEC SQL
               OPEN CSR01-GFCTB092
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE '0006'             TO GFCT0M-LOCAL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTTF-COD-SQL-ERRO
               MOVE 0005               TO GFCTTF-COD-MSG-ERRO
               MOVE '0005'             TO GFCTG2-COD-MSG
               MOVE 'MOVTO_EVNTO_CRRTT '
                                       TO GFCT0M-NOME-TAB
               MOVE 'GFCTB092'         TO WRK-TABELA
               PERFORM 1450-MOVE-ERRO-SQL
           END-IF.

           MOVE ZEROS                  TO ACU-CONTADOR.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA LER CURSOR GFCTB092                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2300-LER-CSR-GFCTB092           SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                FETCH CSR01-GFCTB092
                INTO :GFCTB092.DENVIO-MOVTO-TARIF,
                     :GFCTB092.CROTNA-ORIGE-MOVTO,
                     :GFCTB092.CNRO-ARQ-MOVTO,
                     :GFCTB092.CSEQ-MOVTO,
                     :GFCTB092.CSERVC-TARIF,
                     :GFCTB092.QEVNTO-AGRUP,
                     :GFCTB092.VTARIF-BRUTO-MOVTO,
                     :GFCTB092.VTARIF-LIQ-MOVTO,
                     :GFCTB092.VTARIF-DEB-MOVTO,
                     :GFCTB092.CSIT-EVNTO-RECBD,
                     :GFCTB092.DOCOR-EVNTO,
                     :GFCTB092.VTARIF-CADTR-SIST
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE '0007'             TO GFCT0M-LOCAL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTTF-COD-SQL-ERRO
               MOVE 0006               TO GFCTTF-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               MOVE 'MOVTO_EVNTO_CRRTT '
                                       TO GFCT0M-NOME-TAB
               MOVE 'GFCTB092'         TO WRK-TABELA
               PERFORM 1450-MOVE-ERRO-SQL
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FINALIZAR
                                          GFCTTF-FIM
TESTE *---     MOVE ZEROS              TO GFCTTF-ERRO
TESTE *---                                GFCTTF-COD-MSG-ERRO
               IF  IND-1               EQUAL ZEROS
                   MOVE 1              TO GFCTTF-ERRO
TESTE              MOVE ZEROS          TO GFCTTF-COD-SQL-ERRO
                   MOVE 0202           TO GFCTTF-COD-MSG-ERRO
                   MOVE '0202'         TO GFCTG2-COD-MSG
                   PERFORM 1110-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG
                                       TO GFCTTF-DESC-MSG-ERRO
               END-IF
           ELSE
               ADD 1                   TO ACU-CONTADOR
           END-IF.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA MONTAR SAIDA                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       2400-MONTAR-SAIDA               SECTION.
      *----------------------------------------------------------------*

           ADD 104                        TO GFCTTF-LL.
           ADD 1                          TO IND-1
TESTE *---                                   GFCTTF-QTDE-OCOR.
TESTE                                        GFCTTF-QTDE-TOT-REG.

           MOVE DOCOR-EVNTO               OF GFCTB092
                                          TO GFCTTF-DT-EVENTO(IND-1).
           MOVE CSERVC-TARIF              OF GFCTB092
                                          TO GFCTTF-CD-TARIFA(IND-1).
           MOVE QEVNTO-AGRUP              OF GFCTB092
                                          TO GFCTTF-QT-EVENTOS(IND-1).
           MOVE VTARIF-CADTR-SIST         OF GFCTB092
                                          TO GFCTTF-VL-POTENCIAL(IND-1).
           MOVE VTARIF-LIQ-MOVTO          OF GFCTB092
                                          TO GFCTTF-VL-CALCULADO(IND-1).
           MOVE DENVIO-MOVTO-TARIF        OF GFCTB092

                                          TO GFCTTF-DT-MOVIMENTO(IND-1).
           MOVE CROTNA-ORIGE-MOVTO        OF GFCTB092
                                          TO GFCTTF-ROTINA(IND-1).
           MOVE CNRO-ARQ-MOVTO            OF GFCTB092
                                          TO GFCTTF-ARQUIVO(IND-1).
           MOVE CSEQ-MOVTO                OF GFCTB092
                                          TO GFCTTF-SEQ-ARQUIVO(IND-1).
           MOVE ACU-CONTADOR              TO GFCTTF-SEQ-TELA(IND-1).

           IF  CSIT-EVNTO-RECBD           OF GFCTB092 EQUAL 06 OR 07
               MOVE VTARIF-DEB-MOVTO      OF GFCTB092
                                          TO GFCTTF-VL-ESTORNADO(IND-1)
           ELSE
               MOVE DEFETV-DEB-MOVTO      OF GFCTB092
                                          TO WRK-DATA-DB2
               IF  WRK-DIA-DB2            EQUAL '01' AND
                   WRK-MES-DB2            EQUAL '01' AND
                   WRK-ANO-DB2            EQUAL '0001'
                   MOVE VTARIF-LIQ-MOVTO  OF GFCTB092
                                          TO GFCTTF-VL-RECEBER(IND-1)
               ELSE
                   MOVE VTARIF-DEB-MOVTO  OF GFCTB092
                                          TO GFCTTF-VL-RECEBIDO(IND-1)
CREMM              IF CSIT-EVNTO-RECBD OF GFCTB092
CREMM                                         EQUAL 03 OR 03 OR 13
CREMM                 COMPUTE GFCTTF-VL-RECEBER(IND-1) =
CREMM                 (VTARIF-LIQ-MOVTO OF GFCTB092  -
CREMM                  VTARIF-DEB-MOVTO OF GFCTB092)
CREMM              ELSE
CREMM                 COMPUTE GFCTTF-VL-PERDA(IND-1)  =
CREMM                 (VTARIF-LIQ-MOVTO OF GFCTB092 -
CREMM                  VTARIF-DEB-MOVTO OF GFCTB092)
CREMM              END-IF
               END-IF
           END-IF.

CREMM**    IF  GFCTTF-VL-RECEBIDO(IND-1)  NOT EQUAL ZEROS
CREMM**        COMPUTE GFCTTF-VL-PERDA(IND-1)
CREMM**                                   = GFCTTF-VL-CALCULADO(IND-1)
CREMM**                                   - GFCTTF-VL-RECEBIDO(IND-1)
CREMM**    END-IF.

           PERFORM 2300-LER-CSR-GFCTB092.

           IF  IND-1                      GREATER 6
TESTE          MOVE 'S'                   TO GFCTTF-FIM

TESTE          MOVE GFCTTF-SEQ-TELA(IND-1)
TESTE                                     TO GFCTTF-OCOR-ATUAL

               IF  WRK-FINALIZAR          EQUAL 'N'
TESTE *---         MOVE 'N'               TO GFCTTF-FIM
                   MOVE GFCTTF-SEQ-TELA(IND-1)
                                          TO GFCTTF-PONTEIRO
TESTE *---                                   GFCTTF-OCOR-ATUAL
                   MOVE 'S'               TO WRK-FINALIZAR
                                             GFCTTF-CONTINUA
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA FECHAR CURSOR                                       *
      ******************************************************************
      *----------------------------------------------------------------*
       2500-FECHAR-CSR-GFCTB092        SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR01-GFCTB092
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               INITIALIZE               GFCT0M-ERRO-SQL
               MOVE 'CLOSE'             TO GFCT0M-COMANDO-SQL
               MOVE '0008'              TO GFCT0M-LOCAL
               MOVE SQLCODE             TO GFCT0M-SQLCODE
                                           GFCTTF-COD-SQL-ERRO
               MOVE 0011                TO GFCTTF-COD-MSG-ERRO
               MOVE '0011'              TO GFCTG2-COD-MSG
               MOVE 'MOVTO_EVNTO_CRRTT' TO GFCT0M-NOME-TAB
               MOVE 'GFCTB092'          TO WRK-TABELA
               PERFORM 1450-MOVE-ERRO-SQL
           END-IF.

      *----------------------------------------------------------------*
       2500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA FINALIZAR PROGRAMA                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-FINALIZAR-PROGRAMA         SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO RETURN-CODE.

           GOBACK.

      *----------------------------------------------------------------*
       300-99-FIM.                     EXIT.
      *----------------------------------------------------------------*
HEXA  *----------------------------------------------------------------*
HEXA  * SECTION PARA CHAMADA DA ROTINA DE CONVERSAO DE AGENCIA
HEXA  *----------------------------------------------------------------*
HEXA  *
HEXA  *----------------------------------------------------------------*
HEXA   9990-10-CHAMA-ROTINA-FX-AG SECTION.
HEXA  *----------------------------------------------------------------*
HEXA  *
HEXA       CALL        WRK-MODULO-MESUX0 USING WRK-AREA-MESUX0.
HEXA  *
HEXA       IF          WRK-EH-ERRO-MESU-ABEND
HEXA           MOVE    'APL'       TO          ERR-TIPO-ACESSO
HEXA           MOVE    'ERRO NA CHAMADA DA ROTINA DE FAIXAS DE AGENCIA'
HEXA             TO    ERR-TEXTO
HEXA           PERFORM 9991-10-CHAMA-ROTINA-ABEND
HEXA       ELSE
HEXA           MOVE    ZEROS       TO          WRK-MESUX0-AGENCIA
HEXA       END-IF.
HEXA  *
HEXA  *----------------------------------------------------------------*
HEXA   9990-90-EXIT. EXIT.
HEXA  *----------------------------------------------------------------*
HEXA  *
HEXA  *----------------------------------------------------------------*
HEXA  * SECTION PARA CHAMADA DA ROTINA DE ABEND
HEXA  *----------------------------------------------------------------*
HEXA  *
HEXA  *----------------------------------------------------------------*
HEXA   9991-10-CHAMA-ROTINA-ABEND SECTION.
HEXA  *----------------------------------------------------------------*
HEXA  *
HEXA       MOVE    'GFCT0913'      TO          ERR-PGM.
HEXA  *
HEXA       CALL    WRK-ABEND-MESUX0 USING      WRK-BATCH-MESUX0
HEXA                                           ERRO-AREA.
HEXA  *
HEXA       GOBACK.
HEXA  *
HEXA  *----------------------------------------------------------------*
HEXA   9991-90-EXIT. EXIT.
HEXA  *----------------------------------------------------------------*
HEXA  *
