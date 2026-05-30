      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT0987.
       AUTHOR. ALINE THIESEN WOLFF.
      *================================================================*
      *                C P M   B R A X I S   S I S T E M A S           *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : GFCT0987                                     *
      *    PROGRAMADORA : ALINE THIESEN WOLFF      - CPM BRAXIS/FPOLIS *
      *    ANALISTA CPM : MAURICIO NAPOLEAO        - CPM BARXIS/FPOLIS *
      *    ANALISTA     : RICARDO PEREIRA DA SILVA - GRUPO 50          *
      *    DATA         : 13/11/2007                                   *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      DEMONSTRATIVO DE EVENTOS DO CLIENTE - CONSULTA.           *
      *                                                                *
      *    BANCO DE DADOS:                                             *
      *     DB2                                                        *
      *      TABLE                                INCLUDE/BOOK         *
      *        DB2PRD.TMOVTO_TARIF_CTA              GFCTB0G7           *
      *        DB2PRD.TASSOC_CJTO_NATUZ             GFCTB0H2           *
      *        DB2PRD.PARM_SERVC_TARIF              GFCTB0A2           *
301110*        DB2PRD.TMOVTO_PCELD_CRRTT            GFCTB0I3           *
301110*        DB2PRD.TCTRL_NGTVC_CLI               GFCTB0N2           *
      *        DB2PRD.SERVC_TARIF_PRINC             GFCTB0D8           *
      *        DB2PRD.V01CLIENTE_AGENCIA            CLIEV004           *
      *        DB2PRD.V01CTA_POUPANCA               CLIEV007           *
      *        DB2PRD.V01CTA_CORRENTE               CLIEV008           *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      GFCT5523 - OBTER DESCRICAO DE MENSAGENS                   *
      *      SENH0315 - BUSCAR DEPARTAMENTO DO USUARIO                 *
      *      BRAD1470 - CONSISTENCIA DE DATAS                          *
      *                                                                *
      *----------------------------------------------------------------*
      *                      A L T E R A C A O                         *
      *----------------------------------------------------------------*
      *    RESPONSAVEL .: XXXXXXXXXXXXX - XXXXXXXXXXXXXXXX - GRUPO 50  *
      *    DATA ........: DD/MM/AAAA                                   *
      *                                                                *
      *    OBJETIVO ....: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  *
      *                   XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  *
301110*                                                                *
301110*----------------------------------------------------------------*
301110*                      A L T E R A C A O                         *
301110*----------------------------------------------------------------*
301110*    RESPONSAVEL .: SONDAPROCWORK                                *
301110*    DATA ........: 30/11/2010                                   *
301110*    OBJETIVO ....: INCLUSAO DO TRATAMENTO DOS INDICADORES       *
301110*                   DE CONSULTA DE IMPEDIMENTOS/RESTRICOES       *
301110*                   NA COBRANCA ATRAVES DA ROTINA IRES, E        *
301110*                   DE PARCELAMENTO DE COBRANCA DA TARIFA        *
301110*                                                                *
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
       01  FILLER                      PIC  X(032)         VALUE
           '*  INICIO DA WORKING GFCT0987  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*        ACUMULADORES          *'.
      *----------------------------------------------------------------*

       01  ACU-ACUMULADORES.
           05  ACU-DESPREZA            PIC  9(007) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*        INDEXADORES           *'.
      *----------------------------------------------------------------*

       01  IND-INDEXADORES.
           05  IND-C                   PIC  9(003) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*     VARIAVEIS AUXILIARES     *'.
      *----------------------------------------------------------------*

       01  WRK-AUXILIARES.
           05  WRK-FINALIZAR           PIC  X(001)         VALUE SPACES.

           05  WRK-FIM-CURSOR          PIC  X(001)         VALUE SPACES.
           05  WRK-TEM-ASSOC-CONJ-SERVC
                                       PIC  X(001)         VALUE SPACES.

           05  WRK-MODULO              PIC  X(008)         VALUE SPACES.

301110     05  WRK-CONTADOR            PIC S9(009) COMP    VALUE ZEROS.

           05  WRK-DESPREZA            PIC  9(007) COMP-3  VALUE ZEROS.

           05  WRK-GFCT63-OCOR-ATUAL   PIC  9(007) COMP-3  VALUE ZEROS.

           05  WRK-GFCT63-QTDE-TOT-REG PIC  9(006) COMP-3  VALUE ZEROS.

           05  WRK-GFCT63-SEQ-TELA     PIC  9(004) COMP-3  VALUE ZEROS.

           05  WRK-PERIODO-INI         PIC  X(010)         VALUE
               '01.01.0001'.
           05  FILLER                  REDEFINES WRK-PERIODO-INI.
               10  WRK-PERIODO-DIA-INI PIC  9(002).
               10  FILLER              PIC  X(001).
               10  WRK-PERIODO-MES-INI PIC  9(002).
               10  FILLER              PIC  X(001).
               10  WRK-PERIODO-ANO-INI PIC  9(004).

           05  WRK-PERIODO-FIM         PIC  X(010)         VALUE
               '01.01.0001'.
           05  FILLER                  REDEFINES WRK-PERIODO-FIM.
               10  WRK-PERIODO-DIA-FIM PIC  9(002).
               10  FILLER              PIC  X(001).
               10  WRK-PERIODO-MES-FIM PIC  9(002).
               10  FILLER              PIC  X(001).
               10  WRK-PERIODO-ANO-FIM PIC  9(004).

           05  WRK-TARIFA-INI          PIC S9(005) COMP-3  VALUE ZEROS.
           05  WRK-TARIFA-FIM          PIC S9(005) COMP-3  VALUE ZEROS.

           05  WRK-DATA-AUX.
               10  WRK-DIA-AUX         PIC  9(002)         VALUE ZEROS.
               10  FILLER              PIC  X(001)         VALUE SPACES.
               10  WRK-MES-AUX         PIC  9(002)         VALUE ZEROS.
               10  FILLER              PIC  X(001)         VALUE SPACES.
               10  WRK-ANO-AUX         PIC  9(004)         VALUE ZEROS.

           05  WRK-DATA-INV-INI.
               10  WRK-ANO-INV-INI     PIC  9(004)         VALUE ZEROS.
               10  WRK-MES-INV-INI     PIC  9(002)         VALUE ZEROS.
               10  WRK-DIA-INV-INI     PIC  9(002)         VALUE ZEROS.
           05  WRK-DATA-INV-INI-R      REDEFINES WRK-DATA-INV-INI
                                       PIC  9(008).

           05  WRK-DATA-INV-FIM.
               10  WRK-ANO-INV-FIM     PIC  9(004)         VALUE ZEROS.
               10  WRK-MES-INV-FIM     PIC  9(002)         VALUE ZEROS.
               10  WRK-DIA-INV-FIM     PIC  9(002)         VALUE ZEROS.
           05  WRK-DATA-INV-FIM-R      REDEFINES WRK-DATA-INV-FIM
                                       PIC  9(008).

           05  WRK-CSERVC-TARIF        PIC S9(005)         VALUE ZEROS.
           05  FILLER                  REDEFINES WRK-CSERVC-TARIF.
               10  WRK-CD-TARIFA       PIC  9(005).

           05  WRK-QEVNTO-AGRUP        PIC S9(006)         VALUE ZEROS.
           05  FILLER                  REDEFINES WRK-QEVNTO-AGRUP.
               10  WRK-QTD-EVENTOS     PIC  9(006).

           05  WRK-VTARIF-CADTR-SIST   PIC S9(009)V99      VALUE ZEROS.
           05  FILLER                  REDEFINES WRK-VTARIF-CADTR-SIST.
               10  WRK-VLR-POTENCIAL   PIC  9(009)V99.

           05  WRK-VTARIF-MOVTO-LIQ    PIC S9(010)V99      VALUE ZEROS.
           05  FILLER                  REDEFINES WRK-VTARIF-MOVTO-LIQ.
               10  WRK-VLR-CALCULADO   PIC  9(010)V99.

           05  WRK-VTARIF-MOVTO-DEB    PIC S9(010)V99      VALUE ZEROS.
           05  FILLER                  REDEFINES WRK-VTARIF-MOVTO-DEB.
               10  WRK-VLR-RECEBIDO    PIC  9(010)V99.

           05  WRK-VTARIF-RECBR-CLI    PIC S9(010)V99      VALUE ZEROS.
           05  FILLER                  REDEFINES WRK-VTARIF-RECBR-CLI.
               10  WRK-VLR-RECEBER     PIC  9(010)V99.

           05  WRK-VTARIF-ESTRN-CLI    PIC S9(010)V99      VALUE ZEROS.
           05  FILLER                  REDEFINES WRK-VTARIF-ESTRN-CLI.
               10  WRK-VLR-ESTORNADO   PIC  9(010)V99.

           05  WRK-VTARIF-PERDA-CLI    PIC S9(008)V99      VALUE ZEROS.
           05  FILLER                  REDEFINES WRK-VTARIF-PERDA-CLI.
               10  WRK-VLR-PERDA       PIC  9(008)V99.

           05  WRK-CNRO-ARQ-MOVTO      PIC S9(002)         VALUE ZEROS.
           05  FILLER                  REDEFINES WRK-CNRO-ARQ-MOVTO.
               10  WRK-ARQUIVO         PIC  9(002).

           05  WRK-CSEQ-MOVTO          PIC S9(011)         VALUE ZEROS.
           05  FILLER                  REDEFINES WRK-CSEQ-MOVTO.
               10  WRK-SEQ-ARQUIVO     PIC  9(011).

           05  WRK-CONTA.
               10  FILLER              PIC  9(006)         VALUE ZEROS.
               10  WRK-CONTA-7         PIC  9(007)         VALUE ZEROS.
           05  WRK-CONTA-13            REDEFINES WRK-CONTA
                                       PIC  9(013).

           05  WRK-RSERVC-TARIF-TAM-100.
               10  WRK-RSERVC-TARIF-TAM-30
                                       PIC  X(030)         VALUE SPACES.
               10  FILLER              PIC  X(070)         VALUE SPACES.

           05  WRK-SQLCODE             PIC S9(009)         VALUE ZEROS.
           05  FILLER                  REDEFINES WRK-SQLCODE.
               10  FILLER              PIC  X(006).
               10  WRK-COD-SQL         PIC  9(003).

       01  WRK-MSG05.
           05  FILLER                  PIC  X(026)         VALUE
               'ERRO NA CHAMADA DO MODULO'.
           05  WRK-NOME-MOD            PIC  X(008)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*   AREA PARA MODULO SENH0315  *'.
      *----------------------------------------------------------------*

       01  WRK-VERSAO-0315             PIC  X(006)         VALUE
           'VRS001'.

       01  WRK-MENSAGEM-0315.
           05  WRK-COD-MENSAGEM-0315   PIC  9(003)         VALUE ZEROS.
           05  WRK-HIFEN-MENSAGEM-0315
                                       PIC  X(001)         VALUE SPACES.
           05  WRK-TEXTO-MENSAGEM-0315
                                       PIC  X(079)         VALUE SPACES.

       01  WRK-COD-FUNCIONAL-0315      PIC  X(007)         VALUE SPACES.

       01  WRK-NOME-FUNC-0315          PIC  X(040)         VALUE SPACES.

       01  WRK-COD-BANCO-0315          PIC  9(005)         VALUE ZEROS.

       01  WRK-NOME-BANCO-0315         PIC  X(040)         VALUE SPACES.

ST25X6* 01  WRK-CNPJ-BANCO-0315.
ST25X6*     05  WRK-CNPJ-PRI-0315       PIC  9(009)         VALUE ZEROS.
ST25X6*     05  WRK-CNPJ-FIL-0315       PIC  9(005)         VALUE ZEROS.
ST25X6*     05  WRK-CNPJ-CON-0315       PIC  9(002)         VALUE ZEROS.
ST25X6  01  WRK-CNPJ-BANCO-0315.
ST25X6      05  WRK-CNPJ-PRI-0315       PIC  X(009)        VALUE SPACES.
ST25X6      05  WRK-CNPJ-FIL-0315       PIC  X(004)        VALUE SPACES.
ST25X6      05  WRK-CNPJ-CON-0315       PIC  9(002)         VALUE ZEROS.

       01  WRK-COD-JUNCAO-0315         PIC  9(005)         VALUE ZEROS.

       01  WRK-NOME-DEPTO-0315         PIC  X(040)         VALUE SPACES.

       01  WRK-SECAO-0315              PIC  9(003)         VALUE ZEROS.

       01  WRK-STATUS-0315             PIC  X(001)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*   AREA DA BRAD1470           *'.
      *----------------------------------------------------------------*

       01  WRK-DATA-BRAD1470           PIC  9(008)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-DATA-BRAD1470.
           05  WRK-DIA-BRAD1470        PIC  9(002).
           05  WRK-MES-BRAD1470        PIC  9(002).
           05  WRK-ANO-BRAD1470        PIC  9(004).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*   AREA PARA MODULO GFCT5523  *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTG2'.

       COPY 'I#GFCTG3'.
HEXA  *
HEXA  *---------------------------------------------------------------*
HEXA  * AREA DE DECLARACAO PARA SUPORTE A MULTIPLAS FAIXAS DE AGENCIA *
HEXA  *---------------------------------------------------------------*
HEXA  *
HEXA       COPY 'I#HEXA01'.
HEXA  *
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*     AREA PARA BRAD7100       *'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*      AREA DE TABELAS DB2     *'.
      *----------------------------------------------------------------*

           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0G7
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0H2
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0A2
           END-EXEC.

301110     EXEC SQL
301110          INCLUDE GFCTB0I3
301110     END-EXEC.
301110
301110     EXEC SQL
301110          INCLUDE GFCTB0N2
301110     END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0D8
           END-EXEC.

           EXEC SQL
                INCLUDE CLIEV004
           END-EXEC.

           EXEC SQL
                INCLUDE CLIEV007
           END-EXEC.

           EXEC SQL
                INCLUDE CLIEV008
           END-EXEC.

           EXEC SQL
            DECLARE
                     CSR01-GFCTB0G7 CURSOR FOR
             SELECT
                     DOCOR_EVNTO,
                     CSERVC_TARIF,
                     CSEQ_EVNTO_TARIF,
                     CINDCD_DEB_PCIAL,
                     QEVNTO_AGRUP,
                     VTARIF_CADTR_SIST,
                     VTARIF_MOVTO_LIQ,
                     VTARIF_MOVTO_DEB,
                     VTARIF_RECBR_CLI,
                     VTARIF_ESTRN_CLI,
                     VTARIF_PERDA_CLI,
                     CROTNA_ORIGE_MOVTO,
                     DENVIO_MOVTO_TARIF,
                     CNRO_ARQ_MOVTO,
                     CSEQ_MOVTO
               FROM
                     DB2PRD.TMOVTO_TARIF_CTA
              WHERE
                     CBCO_DSTNO_MOVTO   = :GFCTB0G7.CBCO-DSTNO-MOVTO
                AND  CAG_DSTNO_MOVTO    = :GFCTB0G7.CAG-DSTNO-MOVTO
                AND  CCTA_DSTNO_MOVTO   = :GFCTB0G7.CCTA-DSTNO-MOVTO
                AND  DOCOR_EVNTO  BETWEEN :WRK-PERIODO-INI
                                      AND :WRK-PERIODO-FIM
                AND  CSERVC_TARIF BETWEEN :WRK-TARIFA-INI
                                      AND :WRK-TARIFA-FIM
           ORDER BY
                     DOCOR_EVNTO,
                     CSERVC_TARIF,
                     CSEQ_EVNTO_TARIF
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*   FIM DA WORKING GFCT0987    *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY 'I#GFCT62'.

       COPY 'I#GFCT63'.

       COPY 'I#GFCT0M'.

      *================================================================*
       PROCEDURE DIVISION              USING GFCT62-ENTRADA
                                             GFCT63-SAIDA
                                             GFCT0M-AREA-ERROS.
      *================================================================*

      *----------------------------------------------------------------*
      * ROTINA PRINCIPAL                                               *
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
           PERFORM 0100-INICIALIZAR-PROCESSAMENTO.

           PERFORM 1000-CONTROLAR-PROCESSAMENTO.

           PERFORM 9999-FINALIZAR-PROGRAMA.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA INICIALIZAR                                             *
      *----------------------------------------------------------------*
       0100-INICIALIZAR-PROCESSAMENTO  SECTION.
      *----------------------------------------------------------------*

           PERFORM 0200-INICIAR-SAIDA.

           PERFORM 0300-CONSISTIR-DADOS-RECEB.

           IF  WRK-COD-JUNCAO-0315     EQUAL ZEROS
               PERFORM 0400-OBTER-CODIGO-JUNCAO
           END-IF.

           PERFORM 0500-CONSISTE-CONTA-TARIFA.

      *----------------------------------------------------------------*
       0100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA INICIAR AREA DE SAIDA                              *
      *----------------------------------------------------------------*
       0200-INICIAR-SAIDA              SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO SQLCA,
                                          GFCT63-SAIDA,
                                          GFCT0M-AREA-ERROS.

           INITIALIZE                  SQLCA,
                                       GFCT63-SAIDA,
                                       GFCT0M-AREA-ERROS.

           MOVE +235                   TO GFCT63-LL.
           MOVE GFCT62-ZZ              TO GFCT63-ZZ.
           MOVE GFCT62-TRANSACAO       TO GFCT63-TRANSACAO.
           MOVE GFCT62-FUNCAO          TO GFCT63-FUNCAO.

           IF  GFCT62-AGENCIA          NOT NUMERIC
               MOVE ZEROS              TO GFCT63-AGENCIA
           ELSE
               MOVE GFCT62-AGENCIA     TO GFCT63-AGENCIA
           END-IF.

           IF  GFCT62-CONTA            NOT NUMERIC
               MOVE ZEROS              TO GFCT63-CONTA
           ELSE
               MOVE GFCT62-CONTA       TO GFCT63-CONTA
           END-IF.

           MOVE GFCT62-PERIODO-INI     TO GFCT63-PERIODO-INI.
           MOVE GFCT62-PERIODO-FIM     TO GFCT63-PERIODO-FIM.

           IF  GFCT62-COD-CJTO-SERVC   NOT NUMERIC
               MOVE ZEROS              TO GFCT63-COD-CJTO-SERVC
           ELSE
               MOVE GFCT62-COD-CJTO-SERVC
                                       TO GFCT63-COD-CJTO-SERVC
           END-IF.

           IF  GFCT62-COD-TARIFA       NOT NUMERIC
               MOVE ZEROS              TO GFCT63-COD-TARIFA
           ELSE
               MOVE GFCT62-COD-TARIFA  TO GFCT63-COD-TARIFA
           END-IF.

           IF  GFCT62-PONTEIRO         NOT NUMERIC
               MOVE ZEROS              TO GFCT63-PONTEIRO
           ELSE
               MOVE GFCT62-PONTEIRO    TO GFCT63-PONTEIRO
           END-IF.

           IF  GFCT62-OCOR-ATUAL       NOT NUMERIC
               MOVE ZEROS              TO GFCT63-OCOR-ATUAL
           ELSE
               MOVE GFCT62-OCOR-ATUAL  TO GFCT63-OCOR-ATUAL
           END-IF.

           IF  GFCT62-OCOR-TOTAL       NOT NUMERIC
               MOVE ZEROS              TO GFCT63-OCOR-TOTAL
           ELSE
               MOVE GFCT62-OCOR-TOTAL  TO GFCT63-OCOR-TOTAL
           END-IF.

           MOVE 'N'                    TO GFCT63-DETALHES.
           MOVE 'N'                    TO GFCT63-CONTINUA.

           IF  GFCT62-QTDE-OCOR        NOT NUMERIC
               MOVE ZEROS              TO GFCT63-QTDE-OCOR
           ELSE
               MOVE GFCT62-QTDE-OCOR   TO GFCT63-QTDE-OCOR
           END-IF.

           IF  GFCT62-QTDE-TOT-REG     NOT NUMERIC
               MOVE ZEROS              TO GFCT63-QTDE-TOT-REG
           ELSE
               MOVE GFCT62-QTDE-TOT-REG
                                       TO GFCT63-QTDE-TOT-REG
           END-IF.

           IF  GFCT62-DEPTO-BDSCO      NOT NUMERIC
               MOVE ZEROS              TO GFCT63-DEPTO-BDSCO
           ELSE
               MOVE GFCT62-DEPTO-BDSCO TO GFCT63-DEPTO-BDSCO
           END-IF.

           MOVE GFCT62-FUNC-BDSCO      TO GFCT63-FUNC-BDSCO.

           MOVE ZEROS                  TO GFCT63-ERRO,
                                          GFCT63-COD-SQL-ERRO,
                                          GFCT63-COD-MSG-ERRO.
           MOVE 'S'                    TO GFCT63-FIM.

           MOVE '0000'                 TO GFCTG2-COD-MSG.

           PERFORM 9000-OBTER-DESC-MSG.

           MOVE GFCTG3-DESC-MSG        TO GFCT63-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       0200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA CONSISTENCIA DOS CAMPOS RECEBIDOS                  *
      *----------------------------------------------------------------*
       0300-CONSISTIR-DADOS-RECEB      SECTION.
      *----------------------------------------------------------------*

           STRING GFCT62-PERIODO-INI(7:4)
                  GFCT62-PERIODO-INI(4:2)
                  GFCT62-PERIODO-INI(1:2)
           DELIMITED BY SIZE           INTO WRK-DATA-INV-INI.

           STRING GFCT62-PERIODO-FIM(7:4)
                  GFCT62-PERIODO-FIM(4:2)
                  GFCT62-PERIODO-FIM(1:2)
           DELIMITED BY SIZE           INTO WRK-DATA-INV-FIM.

           IF (GFCT62-TRANSACAO        EQUAL SPACES OR LOW-VALUES) OR
              (GFCT62-FUNCAO           EQUAL SPACES OR LOW-VALUES) OR
               GFCT62-AGENCIA          NOT NUMERIC                 OR
               GFCT62-CONTA            NOT NUMERIC                 OR
               WRK-DATA-INV-INI-R      NOT NUMERIC                 OR
               WRK-DATA-INV-FIM-R      NOT NUMERIC                 OR
               GFCT62-COD-CJTO-SERVC   NOT NUMERIC                 OR
               GFCT62-COD-TARIFA       NOT NUMERIC                 OR
               GFCT62-PONTEIRO         NOT NUMERIC                 OR
               GFCT62-OCOR-TOTAL       NOT NUMERIC                 OR
               GFCT62-QTDE-OCOR        NOT NUMERIC                 OR
               GFCT62-QTDE-TOT-REG     NOT NUMERIC                 OR
               GFCT62-DEPTO-BDSCO      NOT NUMERIC                 OR
              (GFCT62-FUNC-BDSCO       EQUAL SPACES OR LOW-VALUES) OR
               GFCT62-FIM              NOT EQUAL 'S' AND 'N'
               MOVE 1                  TO GFCT63-ERRO
               MOVE ZEROS              TO GFCT63-COD-SQL-ERRO
               MOVE 0001               TO GFCT63-COD-MSG-ERRO

               MOVE '0001'             TO GFCTG2-COD-MSG

               PERFORM 9000-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO GFCT63-DESC-MSG-ERRO

               PERFORM 9999-FINALIZAR-PROGRAMA
           END-IF.

           MOVE GFCT62-PERIODO-INI     TO WRK-DATA-AUX.
           MOVE WRK-DIA-AUX            TO WRK-DIA-BRAD1470.
           MOVE WRK-MES-AUX            TO WRK-MES-BRAD1470.
           MOVE WRK-ANO-AUX            TO WRK-ANO-BRAD1470.

           PERFORM 0310-CONSISTIR-DATA.

           MOVE GFCT62-PERIODO-FIM     TO WRK-DATA-AUX.
           MOVE WRK-DIA-AUX            TO WRK-DIA-BRAD1470.
           MOVE WRK-MES-AUX            TO WRK-MES-BRAD1470.
           MOVE WRK-ANO-AUX            TO WRK-ANO-BRAD1470.

           PERFORM 0310-CONSISTIR-DATA.

           IF  WRK-DATA-INV-FIM        LESS WRK-DATA-INV-INI
               MOVE 1                  TO GFCT63-ERRO
               MOVE ZEROS              TO GFCT63-COD-SQL-ERRO
               MOVE 1283               TO GFCT63-COD-MSG-ERRO
               MOVE '1283'             TO GFCTG2-COD-MSG

               PERFORM 9000-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO GFCT63-DESC-MSG-ERRO

               PERFORM 9999-FINALIZAR-PROGRAMA
           END-IF.

      *----------------------------------------------------------------*
       0300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA CONSISTENCIA DE DATAS                           *
      *----------------------------------------------------------------*
       0310-CONSISTIR-DATA             SECTION.
      *----------------------------------------------------------------*

           CALL 'BRAD1470'             USING WRK-DATA-BRAD1470.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 1                  TO GFCT63-ERRO
               MOVE ZEROS              TO GFCT63-COD-SQL-ERRO
               MOVE 1282               TO GFCT63-COD-MSG-ERRO

               MOVE '1282'             TO GFCTG2-COD-MSG

               PERFORM 9000-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO GFCT63-DESC-MSG-ERRO
               MOVE ZEROS              TO RETURN-CODE

               PERFORM 9999-FINALIZAR-PROGRAMA
           END-IF.

      *----------------------------------------------------------------*
       0310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA OBTER CODIGO DA JUNCAO                          *
      *----------------------------------------------------------------*
       0400-OBTER-CODIGO-JUNCAO        SECTION.
      *----------------------------------------------------------------*

           MOVE 'VRS001'               TO WRK-VERSAO-0315.
           MOVE GFCT62-FUNC-BDSCO      TO WRK-COD-FUNCIONAL-0315.

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
               MOVE 1                  TO GFCT63-ERRO
               MOVE RETURN-CODE        TO GFCT63-COD-SQL-ERRO
               MOVE 0056               TO GFCT63-COD-MSG-ERRO

               MOVE '0056'             TO GFCTG2-COD-MSG

               PERFORM 9000-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO GFCT63-DESC-MSG-ERRO
               MOVE ZEROS              TO RETURN-CODE

               PERFORM 9999-FINALIZAR-PROGRAMA
           END-IF.

           IF  RETURN-CODE             EQUAL 04
               EVALUATE WRK-COD-MENSAGEM-0315

                   WHEN 001

                        MOVE 1         TO GFCT63-ERRO
                        MOVE ZEROS     TO GFCT63-COD-SQL-ERRO
                        MOVE 0057      TO GFCT63-COD-MSG-ERRO

                        MOVE '0057'    TO GFCTG2-COD-MSG

                        PERFORM 9000-OBTER-DESC-MSG

                        MOVE GFCTG3-DESC-MSG
                                       TO GFCT63-DESC-MSG-ERRO

                        PERFORM 9999-FINALIZAR-PROGRAMA

                   WHEN 009

                        MOVE ZEROS     TO GFCT63-ERRO
                                          GFCT63-COD-SQL-ERRO
                                          GFCT63-COD-MSG-ERRO

                        MOVE '0000'    TO GFCTG2-COD-MSG

                        PERFORM 9000-OBTER-DESC-MSG

                        MOVE GFCTG3-DESC-MSG
                                       TO GFCT63-DESC-MSG-ERRO

                        PERFORM 9999-FINALIZAR-PROGRAMA

                   WHEN 138

                        MOVE 1         TO GFCT63-ERRO
                        MOVE ZEROS     TO GFCT63-COD-SQL-ERRO
                        MOVE 0060      TO GFCT63-COD-MSG-ERRO

                        MOVE '0060'    TO GFCTG2-COD-MSG

                        PERFORM 9000-OBTER-DESC-MSG

                        MOVE GFCTG3-DESC-MSG
                                       TO GFCT63-DESC-MSG-ERRO

                        PERFORM 9999-FINALIZAR-PROGRAMA

                   WHEN 154

                        MOVE 1         TO GFCT63-ERRO
                        MOVE ZEROS     TO GFCT63-COD-SQL-ERRO
                        MOVE 0058      TO GFCT63-COD-MSG-ERRO

                        MOVE '0058'    TO GFCTG2-COD-MSG

                        PERFORM 9000-OBTER-DESC-MSG

                        MOVE GFCTG3-DESC-MSG
                                       TO GFCT63-DESC-MSG-ERRO

                        PERFORM 9999-FINALIZAR-PROGRAMA

                   WHEN 242

                        MOVE 1         TO GFCT63-ERRO
                        MOVE ZEROS     TO GFCT63-COD-SQL-ERRO
                        MOVE 0059      TO GFCT63-COD-MSG-ERRO

                        MOVE '0059'    TO GFCTG2-COD-MSG

                        PERFORM 9000-OBTER-DESC-MSG

                        MOVE GFCTG3-DESC-MSG
                                       TO GFCT63-DESC-MSG-ERRO

                        PERFORM 9999-FINALIZAR-PROGRAMA

               END-EVALUATE
           END-IF.

           MOVE WRK-COD-JUNCAO-0315    TO GFCT62-DEPTO-BDSCO,
                                          GFCT63-DEPTO-BDSCO.

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
               MOVE 'S'                TO GFCT63-DETALHES
           ELSE
               MOVE 'N'                TO GFCT63-DETALHES
           END-IF.

      *----------------------------------------------------------------*
       0400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA CONSISTENCIA DA CONTA TARIFA                       *
      *----------------------------------------------------------------*
       0500-CONSISTE-CONTA-TARIFA      SECTION.
      *----------------------------------------------------------------*

           PERFORM 0510-VERIFICA-CONTA-CORRENTE.

           IF  SQLCODE                 EQUAL +100
               PERFORM 0520-VERIFICA-CONTA-POUPANCA

               MOVE CID-CLI OF CLIEV007
                                       TO CID-CLI OF CLIEV004
           ELSE
               MOVE CID-CLI OF CLIEV008
                                       TO CID-CLI OF CLIEV004
           END-IF.

           PERFORM 0530-VERIFICA-CLIENTE.

      *----------------------------------------------------------------*
       0500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA CONSISTENCIA DA CONTA CORRENTE                     *
      *----------------------------------------------------------------*
       0510-VERIFICA-CONTA-CORRENTE    SECTION.
      *----------------------------------------------------------------*

           MOVE GFCT62-AGENCIA         TO CJUNC-DEPDC OF CLIEV008.
           MOVE GFCT62-CONTA           TO WRK-CONTA-13.
           MOVE WRK-CONTA-7            TO CCTA-CLI    OF CLIEV008.

           EXEC SQL
             SELECT
                     CID_CLI
               INTO
                    :CLIEV008.CID-CLI
               FROM
                     DB2PRD.V01CTA_CORRENTE
              WHERE
                     CJUNC_DEPDC        = :CLIEV008.CJUNC-DEPDC
                AND  CCTA_CLI           = :CLIEV008.CCTA-CLI
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'GFCT0987'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'V01CTA_CORRENTE'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0001'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA

               MOVE 9                  TO GFCT63-ERRO

               MOVE SQLCODE            TO WRK-SQLCODE
               MOVE WRK-COD-SQL        TO GFCT63-COD-SQL-ERRO

               MOVE 0011               TO GFCT63-COD-MSG-ERRO

               MOVE '0011'             TO GFCTG2-COD-MSG

               PERFORM 9000-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO GFCT63-DESC-MSG-ERRO

               PERFORM 9999-FINALIZAR-PROGRAMA
           END-IF.

      *----------------------------------------------------------------*
       0510-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA CONSISTENCIA DA CONTA POUPANCA                     *
      *----------------------------------------------------------------*
       0520-VERIFICA-CONTA-POUPANCA    SECTION.
      *----------------------------------------------------------------*

           MOVE GFCT62-AGENCIA         TO CJUNC-DEPDC OF CLIEV007.
           MOVE GFCT62-CONTA           TO WRK-CONTA-13.
           MOVE WRK-CONTA-7            TO CCTA-CLI    OF CLIEV007.

           EXEC SQL
             SELECT
                     CID_CLI
               INTO
                    :CLIEV007.CID-CLI
               FROM
                     DB2PRD.V01CTA_POUPANCA
              WHERE
                     CJUNC_DEPDC        = :CLIEV007.CJUNC-DEPDC
                AND  CCTA_CLI           = :CLIEV007.CCTA-CLI
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'GFCT0987'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'V01CTA_POUPANCA'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0002'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA

               MOVE 9                  TO GFCT63-ERRO

               MOVE SQLCODE            TO WRK-SQLCODE
               MOVE WRK-COD-SQL        TO GFCT63-COD-SQL-ERRO

               MOVE 0011               TO GFCT63-COD-MSG-ERRO

               MOVE '0011'             TO GFCTG2-COD-MSG

               PERFORM 9000-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO GFCT63-DESC-MSG-ERRO

               PERFORM 9999-FINALIZAR-PROGRAMA
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 1                  TO GFCT63-ERRO
               MOVE ZEROS              TO GFCT63-COD-SQL-ERRO
               MOVE 0220               TO GFCT63-COD-MSG-ERRO

               MOVE '0220'             TO GFCTG2-COD-MSG

               PERFORM 9000-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO GFCT63-DESC-MSG-ERRO

               PERFORM 9999-FINALIZAR-PROGRAMA
           END-IF.

      *----------------------------------------------------------------*
       0520-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA CONSISTENCIA DO CLIENTE                            *
      *----------------------------------------------------------------*
       0530-VERIFICA-CLIENTE           SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             SELECT
                     IPSSOA_COPLT
               INTO
                    :CLIEV004.IPSSOA-COPLT
               FROM
                     DB2PRD.V01CLIENTE_AGENCIA
              WHERE
                     CID_CLI            = :CLIEV004.CID-CLI
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'GFCT0987'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'V01CLIENTE_AGENCIA'
                                       TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0003'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA

               MOVE 9                  TO GFCT63-ERRO

               MOVE SQLCODE            TO WRK-SQLCODE
               MOVE WRK-COD-SQL        TO GFCT63-COD-SQL-ERRO

               MOVE 0011               TO GFCT63-COD-MSG-ERRO

               MOVE '0011'             TO GFCTG2-COD-MSG

               PERFORM 9000-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO GFCT63-DESC-MSG-ERRO

               PERFORM 9999-FINALIZAR-PROGRAMA
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 1                  TO GFCT63-ERRO
               MOVE ZEROS              TO GFCT63-COD-SQL-ERRO
               MOVE 0220               TO GFCT63-COD-MSG-ERRO

               MOVE '0220'             TO GFCTG2-COD-MSG

               PERFORM 9000-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO GFCT63-DESC-MSG-ERRO

               PERFORM 9999-FINALIZAR-PROGRAMA
           END-IF.

      *----------------------------------------------------------------*
       0530-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA DE PROCESSAMENTO GERAL                                  *
      *----------------------------------------------------------------*
       1000-CONTROLAR-PROCESSAMENTO    SECTION.
      *----------------------------------------------------------------*

           MOVE 237                    TO CBCO-DSTNO-MOVTO OF GFCTB0G7.
           MOVE GFCT62-AGENCIA         TO CAG-DSTNO-MOVTO  OF GFCTB0G7.
           MOVE GFCT62-CONTA           TO CCTA-DSTNO-MOVTO OF GFCTB0G7.

           MOVE GFCT62-PERIODO-INI     TO WRK-DATA-AUX.
           MOVE WRK-DIA-AUX            TO WRK-PERIODO-DIA-INI.
           MOVE WRK-MES-AUX            TO WRK-PERIODO-MES-INI.
           MOVE WRK-ANO-AUX            TO WRK-PERIODO-ANO-INI.

           MOVE GFCT62-PERIODO-FIM     TO WRK-DATA-AUX.
           MOVE WRK-DIA-AUX            TO WRK-PERIODO-DIA-FIM.
           MOVE WRK-MES-AUX            TO WRK-PERIODO-MES-FIM.
           MOVE WRK-ANO-AUX            TO WRK-PERIODO-ANO-FIM.

           IF  GFCT62-COD-TARIFA       EQUAL ZEROS
               MOVE ZEROS              TO WRK-TARIFA-INI
               MOVE 99999              TO WRK-TARIFA-FIM
           ELSE
               MOVE GFCT62-COD-TARIFA  TO WRK-TARIFA-INI
                                          WRK-TARIFA-FIM
           END-IF.

           MOVE 'N'                    TO WRK-FINALIZAR.

           MOVE 'N'                    TO WRK-FIM-CURSOR.

           PERFORM 1200-ABRIR-CURSOR-GFCTB0G7.

           MOVE GFCT63-OCOR-ATUAL      TO WRK-GFCT63-OCOR-ATUAL,
                                          WRK-DESPREZA.

           MOVE GFCT63-QTDE-TOT-REG    TO WRK-GFCT63-QTDE-TOT-REG.

           MOVE ZEROS                  TO WRK-GFCT63-SEQ-TELA.

           PERFORM 1300-LER-CURSOR-GFCTB0G7.

           PERFORM 1300-LER-CURSOR-GFCTB0G7
           VARYING ACU-DESPREZA        FROM 1 BY 1
             UNTIL ACU-DESPREZA        GREATER WRK-DESPREZA
                OR WRK-FIM-CURSOR      EQUAL 'S'.

           MOVE ZEROS                  TO IND-C.

           PERFORM 1100-TRATAR-OCORRENCIA-SAIDA
             UNTIL WRK-FIM-CURSOR      EQUAL 'S'
                OR WRK-FINALIZAR       EQUAL 'S'.

           PERFORM 1400-FECHAR-CURSOR-GFCTB0G7.

           MOVE WRK-GFCT63-OCOR-ATUAL  TO GFCT63-OCOR-ATUAL.

           MOVE WRK-GFCT63-QTDE-TOT-REG
                                       TO GFCT63-QTDE-TOT-REG.

           IF  GFCT63-CONTINUA         EQUAL 'N' AND
               GFCT63-OCOR-ATUAL       NOT EQUAL ZEROS
               MOVE 1                  TO GFCT63-ERRO
               MOVE ZEROS              TO GFCT63-COD-SQL-ERRO

               MOVE 0007               TO GFCT63-COD-MSG-ERRO

               MOVE '0007'             TO GFCTG2-COD-MSG

               PERFORM 9000-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO GFCT63-DESC-MSG-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA TRATAR A MONTAGEM DAS OCORRENCIAS DA TELA          *
      *----------------------------------------------------------------*
       1100-TRATAR-OCORRENCIA-SAIDA    SECTION.
      *----------------------------------------------------------------*

           ADD  1                      TO WRK-GFCT63-OCOR-ATUAL.

           IF  GFCT62-COD-CJTO-SERVC   EQUAL ZEROS
               PERFORM 1130-MONTAR-OCORRENCIA-SAIDA

               PERFORM 1300-LER-CURSOR-GFCTB0G7
           ELSE
               PERFORM 1120-VALIDAR-ASSOC-CONJ-SERV

               IF  WRK-TEM-ASSOC-CONJ-SERVC
                                       EQUAL 'S'
                   PERFORM 1130-MONTAR-OCORRENCIA-SAIDA
               END-IF

               MOVE 'N'                TO WRK-TEM-ASSOC-CONJ-SERVC

               PERFORM 1110-OBTER-PROXIMO-EVENTO
                 UNTIL WRK-FIM-CURSOR  EQUAL 'S'
                    OR WRK-TEM-ASSOC-CONJ-SERVC
                                       EQUAL 'S'
           END-IF.

           IF  WRK-FIM-CURSOR          EQUAL 'N'
               IF  IND-C               EQUAL 5
                   MOVE 'S'            TO GFCT63-CONTINUA

                   MOVE 'S'            TO WRK-FINALIZAR
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA OBTER O PROXIMO EVENTO COM ASSOCIACAO VALIDA       *
      *----------------------------------------------------------------*
       1110-OBTER-PROXIMO-EVENTO       SECTION.
      *----------------------------------------------------------------*

           PERFORM 1300-LER-CURSOR-GFCTB0G7.

           PERFORM 1120-VALIDAR-ASSOC-CONJ-SERV.

           IF  WRK-TEM-ASSOC-CONJ-SERVC
                                       EQUAL 'N'
               ADD  1                  TO WRK-GFCT63-OCOR-ATUAL
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA VALIDAR ASSOCIACAO CONJUNTO SERVICO                     *
      *----------------------------------------------------------------*
       1120-VALIDAR-ASSOC-CONJ-SERV    SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF OF GFCTB0G7
                                       TO CSERVC-TARIF OF GFCTB0A2.

           MOVE GFCT62-COD-CJTO-SERVC  TO CCJTO-SERVC  OF GFCTB0H2.

           EXEC SQL
             SELECT
                     T1.CSERVC_TARIF
               INTO
                    :GFCTB0A2.CSERVC-TARIF
               FROM
                     DB2PRD.PARM_SERVC_TARIF  T1,
                     DB2PRD.TASSOC_CJTO_NATUZ T2
              WHERE
                     T1.CSERVC_TARIF    = :GFCTB0A2.CSERVC-TARIF
                AND  T1.CNATUZ_SERVC    =  T2.CNATUZ_SERVC
                AND  T2.CCJTO_SERVC     = :GFCTB0H2.CCJTO-SERVC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100
                                                       AND -811) OR
              (SQLWARN0                EQUAL 'W')
               PERFORM 1400-FECHAR-CURSOR-GFCTB0G7

               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'GFCT0987'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'PARM_SERVC_TARIF' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0004'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA

               MOVE 9                  TO GFCT63-ERRO

               MOVE SQLCODE            TO WRK-SQLCODE
               MOVE WRK-COD-SQL        TO GFCT63-COD-SQL-ERRO

               MOVE 0011               TO GFCT63-COD-MSG-ERRO

               MOVE '0011'             TO GFCTG2-COD-MSG

               PERFORM 9000-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO GFCT63-DESC-MSG-ERRO

               PERFORM 9999-FINALIZAR-PROGRAMA
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'N'                TO WRK-TEM-ASSOC-CONJ-SERVC
           ELSE
               MOVE 'S'                TO WRK-TEM-ASSOC-CONJ-SERVC
           END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA MONTAR AREA DE SAIDA                               *
      *----------------------------------------------------------------*
       1130-MONTAR-OCORRENCIA-SAIDA    SECTION.
      *----------------------------------------------------------------*

           COMPUTE GFCT63-LL            = GFCT63-LL + 155.

           ADD  1                      TO WRK-GFCT63-QTDE-TOT-REG.

           ADD  1                      TO IND-C.

           IF  CROTNA-ORIGE-MOVTO OF GFCTB0G7
                                       EQUAL SPACES
               MOVE 'N'                TO GFCT63-SEL(IND-C)
           ELSE
               MOVE 'S'                TO GFCT63-SEL(IND-C)
           END-IF.

           MOVE DOCOR-EVNTO OF GFCTB0G7
                                       TO GFCT63-DT-EVENTO(IND-C).

           IF  CINDCD-DEB-PCIAL OF GFCTB0G7
                                       EQUAL 'S'
               MOVE 'S'                TO GFCT63-DEB-PARCIAL(IND-C)
           ELSE
               MOVE 'N'                TO GFCT63-DEB-PARCIAL(IND-C)
           END-IF.

301110     MOVE ZEROS                  TO WRK-CONTADOR.
301110
301110     PERFORM 1131-VERIFICAR-CONS-IMPED-REST.
301110
301110     IF  WRK-CONTADOR            EQUAL ZEROS
301110         MOVE 'N'                TO GFCT63-CONS-IMPED-RESTR
301110                                                      (IND-C)
301110     ELSE
301110         MOVE 'S'                TO GFCT63-CONS-IMPED-RESTR
301110                                                      (IND-C)
301110     END-IF.
301110
301110     MOVE ZEROS                  TO WRK-CONTADOR.
301110
301110     PERFORM 1132-VERIFICAR-PARC-COBRANCA.
301110
301110     IF  WRK-CONTADOR            EQUAL ZEROS
301110         MOVE 'N'                TO GFCT63-PARCELAMENTO-COBR
301110                                                      (IND-C)
301110     ELSE
301110         MOVE 'S'                TO GFCT63-PARCELAMENTO-COBR
301110                                                      (IND-C)
301110     END-IF.

           MOVE CSERVC-TARIF OF GFCTB0G7
                                       TO WRK-CSERVC-TARIF.
           MOVE WRK-CD-TARIFA          TO GFCT63-CD-TARIFA(IND-C).

           PERFORM 1133-OBTER-DESCRICAO-TARIFA.

           MOVE RSERVC-TARIF-REDZD OF GFCTB0D8
                                       TO WRK-RSERVC-TARIF-TAM-100.
           MOVE WRK-RSERVC-TARIF-TAM-30
                                       TO GFCT63-NOME-TARIFA(IND-C).

           MOVE QEVNTO-AGRUP OF GFCTB0G7
                                       TO WRK-QEVNTO-AGRUP.
           MOVE WRK-QTD-EVENTOS        TO GFCT63-QT-EVENTOS(IND-C).

           MOVE VTARIF-CADTR-SIST OF GFCTB0G7
                                       TO WRK-VTARIF-CADTR-SIST.
           MOVE WRK-VLR-POTENCIAL      TO GFCT63-VL-POTENCIAL(IND-C).

           MOVE VTARIF-MOVTO-LIQ OF GFCTB0G7
                                       TO WRK-VTARIF-MOVTO-LIQ.
           MOVE WRK-VLR-CALCULADO      TO GFCT63-VL-CALCULADO(IND-C).

           MOVE VTARIF-MOVTO-DEB OF GFCTB0G7
                                       TO WRK-VTARIF-MOVTO-DEB.
           MOVE WRK-VLR-RECEBIDO       TO GFCT63-VL-RECEBIDO(IND-C).

           MOVE VTARIF-RECBR-CLI OF GFCTB0G7
                                       TO WRK-VTARIF-RECBR-CLI.
           MOVE WRK-VLR-RECEBER        TO GFCT63-VL-RECEBER(IND-C).

           MOVE VTARIF-ESTRN-CLI OF GFCTB0G7
                                       TO WRK-VTARIF-ESTRN-CLI.
           MOVE WRK-VLR-ESTORNADO      TO GFCT63-VL-ESTORNADO(IND-C).

           MOVE VTARIF-PERDA-CLI OF GFCTB0G7
                                       TO WRK-VTARIF-PERDA-CLI.
           MOVE WRK-VLR-PERDA          TO GFCT63-VL-PERDA(IND-C).

           MOVE CROTNA-ORIGE-MOVTO OF GFCTB0G7
                                       TO GFCT63-ROTINA(IND-C).
           MOVE DENVIO-MOVTO-TARIF OF GFCTB0G7
                                       TO GFCT63-DT-MOVIMENTO(IND-C).

           MOVE CNRO-ARQ-MOVTO OF GFCTB0G7
                                       TO WRK-CNRO-ARQ-MOVTO.
           MOVE WRK-ARQUIVO            TO GFCT63-ARQUIVO(IND-C).

           MOVE CSEQ-MOVTO OF GFCTB0G7 TO WRK-CSEQ-MOVTO.
           MOVE WRK-SEQ-ARQUIVO        TO GFCT63-SEQ-ARQUIVO(IND-C).

           ADD  1                      TO WRK-GFCT63-SEQ-TELA.
           MOVE WRK-GFCT63-SEQ-TELA    TO GFCT63-SEQ-TELA(IND-C).

      *----------------------------------------------------------------*
       1130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

301110******************************************************************
301110* ROTINA PARA VERIFICAR A EXISTENCIA DE CONSULTA DE IMPEDIMENTO  *
301110* E/OU RESTRICOES                                                *
301110******************************************************************
301110*----------------------------------------------------------------*
301110 1131-VERIFICAR-CONS-IMPED-REST  SECTION.
301110*----------------------------------------------------------------*
301110
301110     MOVE CROTNA-ORIGE-MOVTO OF GFCTB0G7
301110                                 TO CROTNA-ORIGE-MOVTO
301110                                                     OF GFCTB0N2.
301110     MOVE DENVIO-MOVTO-TARIF OF GFCTB0G7
301110                                 TO DENVIO-MOVTO-TARIF
301110                                                     OF GFCTB0N2.
301110     MOVE CNRO-ARQ-MOVTO OF GFCTB0G7
301110                                 TO CNRO-ARQ-MOVTO   OF GFCTB0N2.
301110     MOVE CSEQ-MOVTO OF GFCTB0G7 TO CSEQ-MOVTO       OF GFCTB0N2.
301110
301110     EXEC SQL
301110       SELECT
301110               COUNT(DCONS_NGTVC_CLI)
301110         INTO
301110              :WRK-CONTADOR
301110         FROM
301110               DB2PRD.TCTRL_NGTVC_CLI
301110        WHERE
301110               CROTNA_ORIGE_MOVTO
301110                                  = :GFCTB0N2.CROTNA-ORIGE-MOVTO
301110          AND  DENVIO_MOVTO_TARIF
301110                                  = :GFCTB0N2.DENVIO-MOVTO-TARIF
301110          AND  CNRO_ARQ_MOVTO     = :GFCTB0N2.CNRO-ARQ-MOVTO
301110          AND  CSEQ_MOVTO         = :GFCTB0N2.CSEQ-MOVTO
301110     END-EXEC.
301110
301110     IF (SQLCODE                 NOT EQUAL ZEROS AND -305) OR
301110        (SQLWARN0                EQUAL 'W')
301110         PERFORM 1400-FECHAR-CURSOR-GFCTB0G7
301110
301110         INITIALIZE              GFCT0M-ERRO-SQL
301110
301110         MOVE 'GFCT0987'         TO GFCT0M-PROGRAMA
301110         MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
301110         MOVE 'TCTRL_NGTVC_CLI'  TO GFCT0M-NOME-TAB
301110         MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
301110         MOVE SQLCODE            TO GFCT0M-SQLCODE
301110         MOVE '0005'             TO GFCT0M-LOCAL
301110         MOVE SQLCA              TO GFCT0M-SQLCA-AREA
301110
301110         MOVE 9                  TO GFCT63-ERRO
301110
301110         MOVE SQLCODE            TO WRK-SQLCODE
301110         MOVE WRK-COD-SQL        TO GFCT63-COD-SQL-ERRO
301110
301110         MOVE 0011               TO GFCT63-COD-MSG-ERRO
301110
301110         MOVE '0011'             TO GFCTG2-COD-MSG
301110
301110         PERFORM 9000-OBTER-DESC-MSG
301110
301110         MOVE GFCTG3-DESC-MSG    TO GFCT63-DESC-MSG-ERRO
301110
301110     END-IF.
301110
301110     IF  SQLCODE                 EQUAL -305
301110         MOVE ZEROS              TO WRK-CONTADOR
301110     END-IF.
301110
301110*----------------------------------------------------------------*
301110 1131-99-FIM.                    EXIT.
301110*----------------------------------------------------------------*
301110
301110******************************************************************
301110* ROTINA PARA VERIFICAR A EXISTENCIA DE PARCELAMENTO DE COBRANCA *
301110* DE TARIFA                                                      *
301110******************************************************************
301110*----------------------------------------------------------------*
301110 1132-VERIFICAR-PARC-COBRANCA    SECTION.
301110*----------------------------------------------------------------*
301110
301110     MOVE CROTNA-ORIGE-MOVTO OF GFCTB0G7
301110                                 TO CROTNA-ORIGE-MOVTO
301110                                                     OF GFCTB0I3.
301110     MOVE DENVIO-MOVTO-TARIF OF GFCTB0G7
301110                                 TO DENVIO-MOVTO-TARIF
301110                                                     OF GFCTB0I3.
301110     MOVE CNRO-ARQ-MOVTO OF GFCTB0G7
301110                                 TO CNRO-ARQ-MOVTO   OF GFCTB0I3.
301110     MOVE CSEQ-MOVTO OF GFCTB0G7 TO CSEQ-MOVTO       OF GFCTB0I3.
301110
301110     EXEC SQL
301110       SELECT
301110               COUNT(CPCELA_TARIF)
301110         INTO
301110              :WRK-CONTADOR
301110         FROM
301110               DB2PRD.TMOVTO_PCELD_CRRTT
301110        WHERE
301110               CROTNA_ORIGE_MOVTO
301110                                  = :GFCTB0I3.CROTNA-ORIGE-MOVTO
301110          AND  DENVIO_MOVTO_TARIF
301110                                  = :GFCTB0I3.DENVIO-MOVTO-TARIF
301110          AND  CNRO_ARQ_MOVTO     = :GFCTB0I3.CNRO-ARQ-MOVTO
301110          AND  CSEQ_MOVTO         = :GFCTB0I3.CSEQ-MOVTO
301110     END-EXEC.
301110
301110     IF (SQLCODE                 NOT EQUAL ZEROS AND -305) OR
301110        (SQLWARN0                EQUAL 'W')
301110         PERFORM 1400-FECHAR-CURSOR-GFCTB0G7
301110
301110         INITIALIZE              GFCT0M-ERRO-SQL
301110
301110         MOVE 'GFCT0987'         TO GFCT0M-PROGRAMA
301110         MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
301110         MOVE 'TMOVTO_PCELD_CRRTT'
301110                                 TO GFCT0M-NOME-TAB
301110         MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
301110         MOVE SQLCODE            TO GFCT0M-SQLCODE
301110         MOVE '0006'             TO GFCT0M-LOCAL
301110         MOVE SQLCA              TO GFCT0M-SQLCA-AREA
301110
301110         MOVE 9                  TO GFCT63-ERRO
301110
301110         MOVE SQLCODE            TO WRK-SQLCODE
301110         MOVE WRK-COD-SQL        TO GFCT63-COD-SQL-ERRO
301110
301110         MOVE 0011               TO GFCT63-COD-MSG-ERRO
301110
301110         MOVE '0011'             TO GFCTG2-COD-MSG
301110
301110         PERFORM 9000-OBTER-DESC-MSG
301110
301110         MOVE GFCTG3-DESC-MSG    TO GFCT63-DESC-MSG-ERRO
301110
301110     END-IF.
301110
301110     IF  SQLCODE                 EQUAL -305
301110         MOVE ZEROS              TO WRK-CONTADOR
301110     END-IF.
301110
301110*----------------------------------------------------------------*
301110 1132-99-FIM.                    EXIT.
301110*----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA OBTER DESCRICAO TARIFA.                                 *
      *----------------------------------------------------------------*
       1133-OBTER-DESCRICAO-TARIFA     SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF OF GFCTB0G7
                                       TO CSERVC-TARIF OF GFCTB0D8.

           EXEC SQL
             SELECT
                     RSERVC_TARIF_REDZD
               INTO
                    :GFCTB0D8.RSERVC-TARIF-REDZD
               FROM
                     DB2PRD.SERVC_TARIF_PRINC
              WHERE
                     CSERVC_TARIF       = :GFCTB0D8.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               PERFORM 1400-FECHAR-CURSOR-GFCTB0G7

               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'GFCT0987'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'SERVC_TARIF_PRINC'
                                       TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0007'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA

               MOVE 9                  TO GFCT63-ERRO

               MOVE SQLCODE            TO WRK-SQLCODE
               MOVE WRK-COD-SQL        TO GFCT63-COD-SQL-ERRO

               MOVE 0011               TO GFCT63-COD-MSG-ERRO

               MOVE '0011'             TO GFCTG2-COD-MSG

               PERFORM 9000-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO GFCT63-DESC-MSG-ERRO

               PERFORM 9999-FINALIZAR-PROGRAMA
           END-IF.

      *----------------------------------------------------------------*
       1133-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA ABRIR CURSOR                                       *
      *----------------------------------------------------------------*
       1200-ABRIR-CURSOR-GFCTB0G7      SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               OPEN
                     CSR01-GFCTB0G7
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'GFCT0987'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'TMOVTO_TARIF_CTA' TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0008'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA

               MOVE 9                  TO GFCT63-ERRO

               MOVE SQLCODE            TO WRK-SQLCODE
               MOVE WRK-COD-SQL        TO GFCT63-COD-SQL-ERRO

               MOVE 0005               TO GFCT63-COD-MSG-ERRO

               MOVE '0005'             TO GFCTG2-COD-MSG

               PERFORM 9000-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO GFCT63-DESC-MSG-ERRO

               PERFORM 9999-FINALIZAR-PROGRAMA
           END-IF.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA LER CURSOR                                         *
      *----------------------------------------------------------------*
       1300-LER-CURSOR-GFCTB0G7        SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
              FETCH
                     CSR01-GFCTB0G7
               INTO
                    :GFCTB0G7.DOCOR-EVNTO,
                    :GFCTB0G7.CSERVC-TARIF,
                    :GFCTB0G7.CSEQ-EVNTO-TARIF,
                    :GFCTB0G7.CINDCD-DEB-PCIAL,
                    :GFCTB0G7.QEVNTO-AGRUP,
                    :GFCTB0G7.VTARIF-CADTR-SIST,
                    :GFCTB0G7.VTARIF-MOVTO-LIQ,
                    :GFCTB0G7.VTARIF-MOVTO-DEB,
                    :GFCTB0G7.VTARIF-RECBR-CLI,
                    :GFCTB0G7.VTARIF-ESTRN-CLI,
                    :GFCTB0G7.VTARIF-PERDA-CLI,
                    :GFCTB0G7.CROTNA-ORIGE-MOVTO,
                    :GFCTB0G7.DENVIO-MOVTO-TARIF,
                    :GFCTB0G7.CNRO-ARQ-MOVTO,
                    :GFCTB0G7.CSEQ-MOVTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               PERFORM 1400-FECHAR-CURSOR-GFCTB0G7

               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'GFCT0987'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'TMOVTO_TARIF_CTA' TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0009'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA

               MOVE 9                  TO GFCT63-ERRO

               MOVE SQLCODE            TO WRK-SQLCODE
               MOVE WRK-COD-SQL        TO GFCT63-COD-SQL-ERRO

               MOVE 0006               TO GFCT63-COD-MSG-ERRO

               MOVE '0006'             TO GFCTG2-COD-MSG

               PERFORM 9000-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO GFCT63-DESC-MSG-ERRO

               PERFORM 1400-FECHAR-CURSOR-GFCTB0G7

               PERFORM 9999-FINALIZAR-PROGRAMA
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-CURSOR

               IF  IND-C               EQUAL ZEROS AND
                   GFCT63-OCOR-ATUAL   EQUAL ZEROS
                   MOVE 1              TO GFCT63-ERRO
                   MOVE ZEROS          TO GFCT63-COD-SQL-ERRO

                   MOVE 1286           TO GFCT63-COD-MSG-ERRO

                   MOVE '1286'         TO GFCTG2-COD-MSG

                   PERFORM 9000-OBTER-DESC-MSG

                   MOVE GFCTG3-DESC-MSG
                                       TO GFCT63-DESC-MSG-ERRO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA FECHAR CURSOR                                      *
      *----------------------------------------------------------------*
       1400-FECHAR-CURSOR-GFCTB0G7     SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
              CLOSE
                     CSR01-GFCTB0G7
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'GFCT0987'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'TMOVTO_TARIF_CTA' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0010'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA

               MOVE 9                  TO GFCT63-ERRO

               MOVE SQLCODE            TO WRK-SQLCODE
               MOVE WRK-COD-SQL        TO GFCT63-COD-SQL-ERRO

               MOVE 0011               TO GFCT63-COD-MSG-ERRO

               MOVE '0011'             TO GFCTG2-COD-MSG

               PERFORM 9000-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO GFCT63-DESC-MSG-ERRO

               PERFORM 9999-FINALIZAR-PROGRAMA
           END-IF.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA OBTER MENSAGEM DE ERRO                             *
      *----------------------------------------------------------------*
       9000-OBTER-DESC-MSG             SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTG3-SAIDA.

           INITIALIZE                  GFCTG3-SAIDA.

           MOVE GFCT62-TRANSACAO       TO GFCTG2-TRANSACAO.
           MOVE GFCT62-FUNCAO          TO GFCTG2-FUNCAO.
           MOVE GFCT62-FUNC-BDSCO      TO GFCTG2-FUNC-BDSCO.
           MOVE 'O'                    TO GFCTG2-TIPO-PROC.
           MOVE 'N'                    TO GFCTG2-FIM.

           MOVE 'GFCT5523'             TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCTG2-ENTRADA
                                             GFCTG3-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 1                  TO GFCT63-ERRO

               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-NOME-MOD

               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0987'         TO GFCT0M-TRANSACAO
               MOVE ZEROS              TO RETURN-CODE

               PERFORM 9999-FINALIZAR-PROGRAMA
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 9
               MOVE GFCTG3-ERRO        TO GFCT63-ERRO
               MOVE 'GFCT0987'         TO GFCT0M-TRANSACAO

               IF  GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2'
                   MOVE WRK-MODULO     TO GFCT0M-PROGRAMA
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
                   MOVE GFCTG3-DESC-MSG-ERRO(1:75)
                                       TO GFCT0M-TEXTO
               END-IF

               PERFORM 9999-FINALIZAR-PROGRAMA
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 1
               MOVE GFCTG3-DESC-MSG-ERRO(1:70)
                                       TO GFCTG3-DESC-MSG
           END-IF.

      *----------------------------------------------------------------*
       9000-99-FIM.                    EXIT.
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
HEXA       MOVE    'GFCT0987'      TO          ERR-PGM.
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
      *----------------------------------------------------------------*
      * ROTINA PARA FINALIZAR PROGRAMA                                 *
      *----------------------------------------------------------------*
       9999-FINALIZAR-PROGRAMA         SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
