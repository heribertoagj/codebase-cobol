      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT0839.
       AUTHOR. FERNANDO SCHUTZ.
      *================================================================*
      *                   C P M   S I S T E M A S                      *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : GFCT0839                                     *
      *    PROGRAMADORA : FERNANDO SCHUTZ               - CPM/FPOLIS   *
      *    ANALISTA CPM : DANIELE GOMES                 - CPM/FPOLIS   *
      *    ANALISTA     : WAGNER SILVA - PROCWORK       - GRUPO 50     *
      *    DATA         : 07/11/2005                                   *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      GERAR DADOS PARA DEMONSTRATIVO DE EVENTOS CORRENTISTAS.   *
      *                                                                *
      *    BANCO DE DADOS:                                             *
      *      DB2                                                       *
      *       TABLE                                INCLUDE/BOOK        *
      *       DB2PRD.MOVTO_EVNTO_CRRTT               GFCTB092          *
      *       DB2PRD.V01CLIENTE_AGENCIA              CLIEV004          *
      *       DB2PRD.V01CTA_POUPANCA                 CLIEV007          *
      *       DB2PRD.V01CTA_CORRENTE                 CLIEV008          *
      *       DB2PRD.HIST_TARIF_PRINC                GFCTB087          *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      GFCT5523 - OBTER DESCRICAO DE MENSAGENS                   *
      *      POOL0081 - ALOCAR DINAMICAMENTE UM MODULO                 *
      *      SENH0315 - OBTER JUNCAO.                                  *
      *                                                                *
      *    NAVEGACAO                                                   *
      *      CHAMADO POR: GFCT7838                                     *
      *                                                                *
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
           '*  INICIO DA WORKING GFCT0839  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*         INDEXADORES          *'.
      *----------------------------------------------------------------*

       01  IND                         PIC 9(03)           VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*         ACUMULADORES         *'.
      *----------------------------------------------------------------*

       01  ACU-CONTADOR                PIC 9(03)           VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     VARIAVEIS AUXILIARES     *'.
      *----------------------------------------------------------------*

       01  WRK-FINALIZAR               PIC X(01)           VALUE SPACES.
       01  WRK-MODULO                  PIC X(08)           VALUE SPACES.
       01  WRK-TABELA                  PIC X(08)           VALUE SPACES.
       01  WRK-VALOR                   PIC S9(09) COMP     VALUE ZEROS.
       01  WRK-TARIFA-INI              PIC S9(05) COMP-3   VALUE ZEROS.
       01  WRK-TARIFA-FIM              PIC S9(05) COMP-3   VALUE ZEROS.
       01  WRK-PERIODO-INI             PIC X(10)           VALUE SPACES.
       01  WRK-PERIODO-FIM             PIC X(10)           VALUE SPACES.

       01  WRK-MSG05.
           03  FILLER                  PIC X(26)           VALUE
               'ERRO NA CHAMADA DO MODULO '.
           03  WRK-NOME-MOD            PIC X(08)           VALUE SPACES.

       01  WRK-DATA.
           03 WRK-DIA                  PIC 9(02)           VALUE ZEROS.
           03 WRK-MES                  PIC 9(02)           VALUE ZEROS.
           03 WRK-ANO                  PIC 9(04)           VALUE ZEROS.
       01  WRK-DATA-R REDEFINES
           WRK-DATA                    PIC 9(08).

       01  WRK-DATA-INI.
           03 WRK-ANO-INI              PIC 9(04)           VALUE ZEROS.
           03 WRK-MES-INI              PIC 9(02)           VALUE ZEROS.
           03 WRK-DIA-INI              PIC 9(02)           VALUE ZEROS.

       01  WRK-DATA-FIM.
           03 WRK-ANO-FIM              PIC 9(04)           VALUE ZEROS.
           03 WRK-MES-FIM              PIC 9(02)           VALUE ZEROS.
           03 WRK-DIA-FIM              PIC 9(02)           VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*      AREA DO MODULO SENH     *'.
      *----------------------------------------------------------------*

       01  WRK-VERSAO-0315             PIC X(06)           VALUE
           'VRS001'.
       01  WRK-MENSAGEM-0315.
           03  WRK-COD-MSG-0315        PIC 9(03)           VALUE ZEROS.
           03  WRK-HIFEN-MENSAGEM-0315 PIC X(01)           VALUE SPACES.
           03  WRK-TEXTO-MENSAGEM-0315 PIC X(79)           VALUE SPACES.
       01  WRK-COD-FUNCIONAL-0315      PIC X(07)           VALUE SPACES.
       01  WRK-NOME-FUNC-0315          PIC X(40)           VALUE SPACES.
       01  WRK-COD-BANCO-0315          PIC 9(05)           VALUE ZEROS.
       01  WRK-NOME-BANCO-0315         PIC X(40)           VALUE SPACES.
       01  WRK-CNPJ-BANCO-0315.
           03  WRK-CNPJ-PRI-0315       PIC 9(09)           VALUE ZEROS.
           03  WRK-CNPJ-FIL-0315       PIC 9(05)           VALUE ZEROS.
           03  WRK-CNPJ-CON-0315       PIC 9(02)           VALUE ZEROS.
       01  WRK-COD-JUNCAO-0315         PIC 9(05)           VALUE ZEROS.
       01  WRK-NOME-DEPTO-0315         PIC X(40)           VALUE SPACES.
       01  WRK-SECAO-0315              PIC 9(03)           VALUE ZEROS.
       01  WRK-STATUS-0315             PIC X(01)           VALUE SPACES.

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
                INCLUDE GFCTB092
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
                INCLUDE GFCTB087
           END-EXEC.

           EXEC SQL
             DECLARE CSR-GFCTB092 CURSOR FOR
               SELECT DENVIO_MOVTO_TARIF,
                      CROTNA_ORIGE_MOVTO,
                      CNRO_ARQ_MOVTO,
                      CSEQ_MOVTO,
                      CSERVC_TARIF,
                      QEVNTO_AGRUP,
                      VTARIF_BRUTO_MOVTO,
                      VTARIF_LIQ_MOVTO,
                      VTARIF_DEB_MOVTO,
                      DEFETV_DEB_MOVTO,
                      CSIT_EVNTO_RECBD,
                      DOCOR_EVNTO,
                      VTARIF_CADTR_SIST
               FROM   DB2PRD.MOVTO_EVNTO_CRRTT
               WHERE  CAG_DSTNO_MOVTO  = :GFCTB092.CAG-DSTNO-MOVTO
                AND   CCTA_DSTNO_MOVTO = :GFCTB092.CCTA-DSTNO-MOVTO
                AND   DOCOR_EVNTO     >= :WRK-PERIODO-INI
                AND   DOCOR_EVNTO     <= :WRK-PERIODO-FIM
                AND   CSERVC_TARIF    >= :WRK-TARIFA-INI
                AND   CSERVC_TARIF    <= :WRK-TARIFA-FIM
               ORDER BY DOCOR_EVNTO,
                         CSERVC_TARIF,
                         CNRO_ARQ_MOVTO,
                         CSEQ_MOVTO
           END-EXEC.
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
       01  FILLER                      PIC X(32)           VALUE
           '*    FIM DA WORKING GFCT0839   *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY I#GFCTFO.
       COPY I#GFCTFP.
       COPY I#GFCT0M.

      *================================================================*
       PROCEDURE DIVISION              USING GFCTFO-ENTRADA
                                             GFCTFP-SAIDA
                                             GFCT0M-AREA-ERROS.
      *================================================================*

      *----------------------------------------------------------------*
       00000-ROTINA-PRINCIPAL          SECTION.
      *----------------------------------------------------------------*
HEXA  *
HEXA  *---------------------------------------------------------------*
HEXA  * AREA DE MOVIMENTACAO DA ROTINA MESU E DE ABEND                *
HEXA  *---------------------------------------------------------------*
HEXA  *
HEXA       MOVE        'MESU9410'  TO          WRK-MODULO-MESUX0.
HEXA       MOVE        'BRAD7100'  TO          WRK-ABEND-MESUX0.
HEXA  *

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

           PERFORM 13000-OBTER-CODIGO-JUNCAO.

           PERFORM 14000-CONSISTE-CONTA-TARIFA.

      *----------------------------------------------------------------*
       10000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       11000-INICIALIZAR-SAIDA         SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTFP-SAIDA.

           INITIALIZE                  GFCTFP-SAIDA.

           MOVE 282                    TO GFCTFP-LL.
           MOVE GFCTFO-ZZ              TO GFCTFP-ZZ.
           MOVE GFCTFO-TRANSACAO       TO GFCTFP-TRANSACAO.
           MOVE GFCTFO-FUNCAO          TO GFCTFP-FUNCAO.

           IF  GFCTFO-AGENCIA          NUMERIC
               MOVE GFCTFO-AGENCIA     TO GFCTFP-AGENCIA
           END-IF.

           IF  GFCTFO-CONTA            NUMERIC
               MOVE GFCTFO-CONTA       TO GFCTFP-CONTA
           END-IF.

           MOVE GFCTFO-NOME-CLIENTE    TO GFCTFP-NOME-CLIENTE.
           MOVE GFCTFO-PERIODO-INI     TO GFCTFP-PERIODO-INI.
           MOVE GFCTFO-PERIODO-FIM     TO GFCTFP-PERIODO-FIM.

           IF  GFCTFO-COD-TARIFA       NOT NUMERIC OR
               GFCTFO-COD-TARIFA       EQUAL ZEROS
               MOVE ZEROS              TO GFCTFP-COD-TARIFA
                                          WRK-TARIFA-INI
               MOVE 999999             TO WRK-TARIFA-FIM
           ELSE
               MOVE GFCTFO-COD-TARIFA  TO GFCTFP-COD-TARIFA
                                          WRK-TARIFA-INI
                                          WRK-TARIFA-FIM
           END-IF.

           MOVE GFCTFO-NOME-TARIFA     TO GFCTFP-NOME-TARIFA.

           IF  GFCTFO-OCOR-ATUAL       NOT NUMERIC
               MOVE ZEROS              TO GFCTFP-OCOR-ATUAL
                                          GFCTFO-OCOR-ATUAL
           ELSE
               MOVE GFCTFO-OCOR-ATUAL  TO GFCTFP-OCOR-ATUAL
           END-IF.

           IF  GFCTFO-OCOR-TOTAL       NOT NUMERIC
               MOVE ZEROS              TO GFCTFO-OCOR-TOTAL
           ELSE
               MOVE GFCTFO-OCOR-TOTAL  TO GFCTFP-OCOR-TOTAL
           END-IF.

           MOVE 'N'                    TO GFCTFP-DETALHES
                                          GFCTFP-CONTINUA.

           IF  GFCTFO-QTDE-OCOR        NUMERIC
               MOVE GFCTFO-QTDE-OCOR   TO GFCTFP-QTDE-OCOR
           END-IF.

           IF  GFCTFO-QTDE-TOT-REG     NUMERIC
               MOVE GFCTFO-QTDE-TOT-REG
                                       TO GFCTFP-QTDE-TOT-REG
           END-IF.

           IF  GFCTFO-DEPTO-BDSCO      NOT NUMERIC
               MOVE ZEROS              TO GFCTFP-DEPTO-BDSCO
           ELSE
               MOVE GFCTFO-DEPTO-BDSCO TO GFCTFP-DEPTO-BDSCO
           END-IF.

           MOVE GFCTFO-FUNC-BDSCO      TO GFCTFP-FUNC-BDSCO.
           MOVE GFCTFO-TERMINAL        TO GFCTFP-TERMINAL.

           IF  GFCTFO-PONTEIRO         NUMERIC
               MOVE GFCTFO-PONTEIRO    TO GFCTFP-PONTEIRO
           END-IF.

           MOVE ZEROS                  TO GFCTFP-ERRO
                                          GFCTFP-COD-SQL-ERRO
                                          GFCTFP-COD-MSG-ERRO
                                          GFCTG2-COD-MSG.
           MOVE 'N'                    TO GFCTFP-FIM.

           PERFORM 11100-OBTER-DESC-MSG.

           MOVE GFCTG3-DESC-MSG        TO GFCTFP-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       11000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       11100-OBTER-DESC-MSG            SECTION.
      *----------------------------------------------------------------*

           MOVE 100                    TO GFCTG2-LL.
           MOVE ZEROS                  TO GFCTG2-ZZ.

           MOVE GFCTFO-TRANSACAO       TO GFCTG2-TRANSACAO.
           MOVE GFCTFO-FUNCAO          TO GFCTG2-FUNCAO.
           MOVE GFCTFO-FUNC-BDSCO      TO GFCTG2-FUNC-BDSCO.
           MOVE 'O'                    TO GFCTG2-TIPO-PROC.
           MOVE 'N'                    TO GFCTG2-FIM.

           MOVE SPACES                 TO GFCTG3-SAIDA.
           INITIALIZE                  GFCTG3-SAIDA.

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
               MOVE 'GFCT0839'         TO GFCT0M-TRANSACAO
               MOVE 9                  TO GFCTFP-ERRO
               MOVE 'S'                TO GFCTFP-FIM
               PERFORM 30000-FINALIZAR-PROGRAMA
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 9
               IF  GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2'
                   MOVE WRK-MODULO     TO GFCT0M-PROGRAMA
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
                   MOVE 'GFCT0839'     TO GFCT0M-TRANSACAO
               END-IF
               MOVE 9                  TO GFCTFP-ERRO
               MOVE 'S'                TO GFCTFP-FIM
               PERFORM 30000-FINALIZAR-PROGRAMA
           END-IF.

      *----------------------------------------------------------------*
       11100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       12000-CONSISTIR-DADOS           SECTION.
      *----------------------------------------------------------------*

           IF (GFCTFO-TRANSACAO        EQUAL SPACES)          OR
              (GFCTFO-FUNCAO           EQUAL SPACES)          OR
              (GFCTFO-AGENCIA          NOT NUMERIC)           OR
              (GFCTFO-CONTA            NOT NUMERIC)           OR
              (GFCTFO-PERIODO-INI(01:02)
                                       NOT NUMERIC            OR
               GFCTFO-PERIODO-INI(04:02)
                                       NOT NUMERIC            OR
               GFCTFO-PERIODO-INI(07:04)
                                       NOT NUMERIC)           OR
              (GFCTFO-PERIODO-FIM(01:02)
                                       NOT NUMERIC            OR
               GFCTFO-PERIODO-FIM(04:02)
                                       NOT NUMERIC            OR
               GFCTFO-PERIODO-FIM(07:04)
                                       NOT NUMERIC)           OR
              (GFCTFO-QTDE-OCOR        NOT NUMERIC)           OR
              (GFCTFO-QTDE-TOT-REG     NOT NUMERIC)           OR
              (GFCTFO-FUNC-BDSCO       EQUAL SPACES)          OR
              (GFCTFO-PONTEIRO         NOT NUMERIC)           OR
              (GFCTFO-FIM              NOT EQUAL 'S' AND 'N')
               MOVE 1                  TO GFCTFP-ERRO
               MOVE ZEROS              TO GFCTFP-COD-SQL-ERRO
               MOVE 0001               TO GFCTFP-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               MOVE 'S'                TO GFCTFP-FIM
               PERFORM 11100-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTFP-DESC-MSG-ERRO
               PERFORM 30000-FINALIZAR-PROGRAMA
           END-IF.

           MOVE GFCTFO-PERIODO-INI(01:02)
                                       TO WRK-DIA.
           MOVE GFCTFO-PERIODO-INI(04:02)
                                       TO WRK-MES.
           MOVE GFCTFO-PERIODO-INI(07:04)
                                       TO WRK-ANO.

           CALL 'POOL1470'             USING WRK-DATA-R.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 1                  TO GFCTFP-ERRO
               MOVE ZEROS              TO GFCTFP-COD-SQL-ERRO
               MOVE 0200               TO GFCTFP-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               MOVE 'S'                TO GFCTFP-FIM
               PERFORM 11100-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTFP-DESC-MSG-ERRO
               PERFORM 30000-FINALIZAR-PROGRAMA
           END-IF.

           MOVE GFCTFO-PERIODO-FIM(01:02)
                                       TO WRK-DIA.
           MOVE GFCTFO-PERIODO-FIM(04:02)
                                       TO WRK-MES.
           MOVE GFCTFO-PERIODO-FIM(07:04)
                                       TO WRK-ANO.

           CALL 'POOL1470'             USING WRK-DATA-R.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 1                  TO GFCTFP-ERRO
               MOVE ZEROS              TO GFCTFP-COD-SQL-ERRO
               MOVE 0200               TO GFCTFP-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               MOVE 'S'                TO GFCTFP-FIM
               PERFORM 11100-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTFP-DESC-MSG-ERRO
               PERFORM 30000-FINALIZAR-PROGRAMA
           END-IF.

           MOVE GFCTFO-PERIODO-FIM(01:02)
                                       TO WRK-DIA-FIM.
           MOVE GFCTFO-PERIODO-FIM(04:02)
                                       TO WRK-MES-FIM.
           MOVE GFCTFO-PERIODO-FIM(07:04)
                                       TO WRK-ANO-FIM.
           MOVE GFCTFO-PERIODO-INI(01:02)
                                       TO WRK-DIA-INI.
           MOVE GFCTFO-PERIODO-INI(04:02)
                                       TO WRK-MES-INI.
           MOVE GFCTFO-PERIODO-INI(07:04)
                                       TO WRK-ANO-INI.

           IF  WRK-DATA-FIM            LESS  WRK-DATA-INI
               MOVE 1                  TO GFCTFP-ERRO
               MOVE ZEROS              TO GFCTFP-COD-SQL-ERRO
               MOVE 0201               TO GFCTFP-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               MOVE 'S'                TO GFCTFP-FIM
               PERFORM 11100-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTFP-DESC-MSG-ERRO
               PERFORM 30000-FINALIZAR-PROGRAMA
           END-IF.

      *----------------------------------------------------------------*
       12000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       13000-OBTER-CODIGO-JUNCAO       SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTFO-FUNC-BDSCO      TO WRK-COD-FUNCIONAL-0315.

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
               MOVE 1                  TO GFCTFP-ERRO
               MOVE RETURN-CODE        TO GFCTFP-COD-SQL-ERRO
               MOVE 0056               TO GFCTFP-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               MOVE 'S'                TO GFCTFP-FIM
               PERFORM 11100-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTFP-DESC-MSG-ERRO
               PERFORM 30000-FINALIZAR-PROGRAMA
           END-IF.

           IF  RETURN-CODE             EQUAL 04
               IF  WRK-COD-MSG-0315    EQUAL 001
                   MOVE 1              TO GFCTFP-ERRO
                   MOVE ZEROS          TO GFCTFP-COD-SQL-ERRO
                   MOVE 0057           TO GFCTFP-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
                   MOVE 'S'            TO GFCTFP-FIM
                   PERFORM 11100-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG
                                       TO GFCTFP-DESC-MSG-ERRO
                   PERFORM 30000-FINALIZAR-PROGRAMA
               END-IF
               IF  WRK-COD-MSG-0315    EQUAL 154
                   MOVE 1              TO GFCTFP-ERRO
                   MOVE ZEROS          TO GFCTFP-COD-SQL-ERRO
                   MOVE 0058           TO GFCTFP-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
                   MOVE 'S'            TO GFCTFP-FIM
                   PERFORM 11100-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG
                                       TO GFCTFP-DESC-MSG-ERRO
                   PERFORM 30000-FINALIZAR-PROGRAMA
               END-IF
               IF  WRK-COD-MSG-0315    EQUAL 242
                   MOVE 1              TO GFCTFP-ERRO
                   MOVE ZEROS          TO GFCTFP-COD-SQL-ERRO
                   MOVE 0059           TO GFCTFP-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
                   MOVE 'S'            TO GFCTFP-FIM
                   PERFORM 11100-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG
                                       TO GFCTFP-DESC-MSG-ERRO
                   PERFORM 30000-FINALIZAR-PROGRAMA
               END-IF
               IF  WRK-COD-MSG-0315    EQUAL 009
                   MOVE 1              TO GFCTFP-ERRO
                   MOVE ZEROS          TO GFCTFP-COD-SQL-ERRO
                                          GFCTFP-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
                   MOVE 'S'            TO GFCTFP-FIM
                   PERFORM 11100-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG
                                       TO GFCTFP-DESC-MSG-ERRO
                   PERFORM 30000-FINALIZAR-PROGRAMA
               END-IF
               IF  WRK-COD-MSG-0315    EQUAL 138
                   MOVE 1              TO GFCTFP-ERRO
                   MOVE ZEROS          TO GFCTFP-COD-SQL-ERRO
                   MOVE 0060           TO GFCTFP-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
                   MOVE 'S'            TO GFCTFP-FIM
                   PERFORM 11100-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG
                                       TO GFCTFP-DESC-MSG-ERRO
                   PERFORM 30000-FINALIZAR-PROGRAMA
               END-IF
           END-IF.

           MOVE WRK-COD-JUNCAO-0315    TO GFCTFP-DEPTO-BDSCO
                                          GFCTFO-DEPTO-BDSCO.

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
               MOVE 'S'                TO GFCTFP-DETALHES
           ELSE
               MOVE 'N'                TO GFCTFP-DETALHES
           END-IF.

      *----------------------------------------------------------------*
       13000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       14000-CONSISTE-CONTA-TARIFA     SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTFO-AGENCIA         TO CJUNC-DEPDC OF CLIEV008.
           MOVE GFCTFO-CONTA           TO CCTA-CLI OF CLIEV008.

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
                                          GFCTFP-COD-SQL-ERRO
               MOVE 0005               TO GFCTFP-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               MOVE 'V01CTA_CORRENTE'  TO GFCT0M-NOME-TAB
               MOVE 'CLIEV008'         TO WRK-TABELA
               PERFORM 14100-MOVER-ERRO-SQL
           END-IF.

           IF  SQLCODE                 EQUAL +100
               PERFORM 14010-VERIFICA-POUPANCA
               MOVE CID-CLI OF CLIEV007   TO CID-CLI OF CLIEV004
           ELSE
               MOVE CID-CLI OF CLIEV008   TO CID-CLI OF CLIEV004
           END-IF.

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
               MOVE '0002'             TO GFCT0M-LOCAL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTFP-COD-SQL-ERRO
               MOVE 0005               TO GFCTFP-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               MOVE 'V01CTA_AGENCIA'   TO GFCT0M-NOME-TAB
               MOVE 'CLIEV004'         TO WRK-TABELA
               PERFORM 14100-MOVER-ERRO-SQL
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 1                  TO GFCTFP-ERRO
               MOVE 'N'                TO GFCTFP-CONTINUA
               MOVE 'S'                TO WRK-FINALIZAR
                                          GFCTFP-FIM
               MOVE ZEROS              TO GFCTFP-COD-SQL-ERRO
               MOVE 0220               TO GFCTFP-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               PERFORM 11100-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTFP-DESC-MSG-ERRO
               PERFORM 30000-FINALIZAR-PROGRAMA
           END-IF.

           MOVE IPSSOA-COPLT OF CLIEV004
                                       TO GFCTFP-NOME-CLIENTE.

           IF GFCTFP-COD-TARIFA        EQUAL ZEROS
              GO                       TO 14000-99-FIM
           END-IF.

           MOVE GFCTFO-COD-TARIFA      TO CSERVC-TARIF OF GFCTB087.

           EXEC SQL
             SELECT RSERVC_TARIF_REDZD
             INTO  :GFCTB087.RSERVC-TARIF-REDZD
             FROM   DB2PRD.HIST_TARIF_PRINC
             WHERE  CSERVC_TARIF       = :GFCTB087.CSERVC-TARIF
               AND  CSEQ_MANUT_SERVC   IN (SELECT MAX(CSEQ_MANUT_SERVC)
                                           FROM DB2PRD.HIST_TARIF_PRINC
                                           WHERE CSERVC_TARIF =
                                                :GFCTB087.CSERVC-TARIF)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0003'             TO GFCT0M-LOCAL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTFP-COD-SQL-ERRO
               MOVE 0005               TO GFCTFP-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               MOVE 'HIST_TARIF_PRINC' TO GFCT0M-NOME-TAB
               MOVE 'GFCTB087'         TO WRK-TABELA
               PERFORM 14100-MOVER-ERRO-SQL
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 1                  TO GFCTFP-ERRO
               MOVE 'N'                TO GFCTFP-CONTINUA
               MOVE 'S'                TO WRK-FINALIZAR
                                          GFCTFP-FIM
               MOVE ZEROS              TO GFCTFP-COD-SQL-ERRO
               MOVE 0054               TO GFCTFP-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               PERFORM 11100-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTFP-DESC-MSG-ERRO
               PERFORM 30000-FINALIZAR-PROGRAMA
           END-IF.

      *----------------------------------------------------------------*
       14000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       14010-VERIFICA-POUPANCA         SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTFO-AGENCIA         TO CJUNC-DEPDC OF CLIEV007.
           MOVE GFCTFO-CONTA           TO CCTA-CLI OF CLIEV007.

           EXEC SQL
             SELECT CID_CLI
             INTO   :CLIEV007.CID-CLI
             FROM   DB2PRD.V01CTA_POUPANCA
             WHERE  CJUNC_DEPDC        = :CLIEV007.CJUNC-DEPDC
               AND  CCTA_CLI           = :CLIEV007.CCTA-CLI
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0001'             TO GFCT0M-LOCAL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTFP-COD-SQL-ERRO
               MOVE 0005               TO GFCTFP-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               MOVE 'V01CTA_POUPANCA'  TO GFCT0M-NOME-TAB
               MOVE 'CLIEV007'         TO WRK-TABELA
               PERFORM 14100-MOVER-ERRO-SQL
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 1                  TO GFCTFP-ERRO
               MOVE 'N'                TO GFCTFP-CONTINUA
               MOVE 'S'                TO WRK-FINALIZAR
                                          GFCTFP-FIM
               MOVE ZEROS              TO GFCTFP-COD-SQL-ERRO
               MOVE 0220               TO GFCTFP-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               PERFORM 11100-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTFP-DESC-MSG-ERRO
               PERFORM 30000-FINALIZAR-PROGRAMA
           END-IF.

      *----------------------------------------------------------------*
       14010-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       14100-MOVER-ERRO-SQL            SECTION.
      *----------------------------------------------------------------*

           MOVE 9                      TO GFCTFP-ERRO.
           MOVE 'GFCT0839'             TO GFCT0M-PROGRAMA.
           MOVE 'DB2'                  TO GFCT0M-TIPO-ACESSO.
           MOVE SQLCA                  TO GFCT0M-SQLCA-AREA.

           PERFORM 11100-OBTER-DESC-MSG.

           STRING GFCTG3-DESC-MSG ' - ' WRK-TABELA
           DELIMITED BY '  '           INTO GFCTFP-DESC-MSG-ERRO.

           MOVE 'S'                    TO GFCTFP-FIM.

           PERFORM 30000-FINALIZAR-PROGRAMA.

      *----------------------------------------------------------------*
       14100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       20000-PROCESSAR-DADOS           SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTFO-AGENCIA         TO CAG-DSTNO-MOVTO OF GFCTB092.
           MOVE GFCTFO-CONTA           TO CCTA-DSTNO-MOVTO OF GFCTB092.
           MOVE GFCTFO-PERIODO-INI     TO WRK-PERIODO-INI.
           MOVE GFCTFO-PERIODO-FIM     TO WRK-PERIODO-FIM.
           MOVE 'N'                    TO WRK-FINALIZAR.
           MOVE ZEROS                  TO IND.

           IF  GFCTFO-OCOR-TOTAL       EQUAL ZEROS
               PERFORM 21000-CONTAR-OCORRENCIA
           END-IF.

           PERFORM 22000-ABRIR-CURSOR.

           PERFORM 23000-LER-CSR-GFCTB092
             UNTIL WRK-FINALIZAR       EQUAL 'S'
                OR ACU-CONTADOR        GREATER GFCTFO-OCOR-ATUAL.

           MOVE SPACES                 TO GFCTFP-DADOS.

           INITIALIZE GFCTFP-DADOS.

           PERFORM 24000-MONTAR-SAIDA  UNTIL WRK-FINALIZAR EQUAL 'S'.

           PERFORM 25000-FECHAR-CSR-GFCTB092.

      *----------------------------------------------------------------*
       20000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       21000-CONTAR-OCORRENCIA         SECTION.
      *----------------------------------------------------------------*

           INSPECT GFCTFO-PERIODO-INI  REPLACING ALL '/' BY '.'.
           INSPECT GFCTFO-PERIODO-FIM  REPLACING ALL '/' BY '.'.

           EXEC SQL
             SELECT COUNT(*)
             INTO   :WRK-VALOR
             FROM   DB2PRD.MOVTO_EVNTO_CRRTT
             WHERE  CAG_DSTNO_MOVTO    = :GFCTB092.CAG-DSTNO-MOVTO
              AND   CCTA_DSTNO_MOVTO   = :GFCTB092.CCTA-DSTNO-MOVTO
              AND   DOCOR_EVNTO       >= :GFCTFO-PERIODO-INI
              AND   DOCOR_EVNTO       <= :GFCTFO-PERIODO-FIM
              AND   CSERVC_TARIF      >= :WRK-TARIFA-INI
              AND   CSERVC_TARIF      <= :WRK-TARIFA-FIM
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'COUNT'            TO GFCT0M-COMANDO-SQL
               MOVE '0004'             TO GFCT0M-LOCAL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTFP-COD-SQL-ERRO
               MOVE 0501               TO GFCTFP-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               MOVE 'MOVTO_EVNTO_CRRTT '
                                       TO GFCT0M-NOME-TAB
               MOVE 'GFCTB092'         TO WRK-TABELA
               PERFORM 14100-MOVER-ERRO-SQL
           END-IF.

           MOVE WRK-VALOR              TO GFCTFP-OCOR-TOTAL.

      *----------------------------------------------------------------*
       21000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       22000-ABRIR-CURSOR              SECTION.
      *----------------------------------------------------------------*

           INSPECT WRK-PERIODO-INI     REPLACING ALL '/' BY '.'.
           INSPECT WRK-PERIODO-FIM     REPLACING ALL '/' BY '.'.

           EXEC SQL
               OPEN CSR-GFCTB092
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE '0005'             TO GFCT0M-LOCAL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTFP-COD-SQL-ERRO
               MOVE 0005               TO GFCTFP-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               MOVE 'MOVTO_EVNTO_CRRTT '
                                       TO GFCT0M-NOME-TAB
               MOVE 'GFCTB092'         TO WRK-TABELA
               PERFORM 14100-MOVER-ERRO-SQL
           END-IF.

           MOVE ZEROS                  TO ACU-CONTADOR.

      *----------------------------------------------------------------*
       22000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       23000-LER-CSR-GFCTB092          SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                FETCH CSR-GFCTB092
                INTO :GFCTB092.DENVIO-MOVTO-TARIF,
                     :GFCTB092.CROTNA-ORIGE-MOVTO,
                     :GFCTB092.CNRO-ARQ-MOVTO,
                     :GFCTB092.CSEQ-MOVTO,
                     :GFCTB092.CSERVC-TARIF,
                     :GFCTB092.QEVNTO-AGRUP,
                     :GFCTB092.VTARIF-BRUTO-MOVTO,
                     :GFCTB092.VTARIF-LIQ-MOVTO,
                     :GFCTB092.VTARIF-DEB-MOVTO,
                     :GFCTB092.DEFETV-DEB-MOVTO,
                     :GFCTB092.CSIT-EVNTO-RECBD,
                     :GFCTB092.DOCOR-EVNTO,
                     :GFCTB092.VTARIF-CADTR-SIST
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE '0006'             TO GFCT0M-LOCAL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTFP-COD-SQL-ERRO
               MOVE 0006               TO GFCTFP-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               MOVE 'MOVTO_EVNTO_CRRTT '
                                       TO GFCT0M-NOME-TAB
               MOVE 'GFCTB092'         TO WRK-TABELA
               PERFORM 14100-MOVER-ERRO-SQL
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FINALIZAR
                                          GFCTFP-FIM
               MOVE ZEROS              TO GFCTFP-ERRO
                                          GFCTFP-COD-MSG-ERRO
               IF  IND                 EQUAL ZEROS
                   MOVE 1              TO GFCTFP-ERRO
                   MOVE 0202           TO GFCTFP-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
                   PERFORM 11100-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG
                                       TO GFCTFP-DESC-MSG-ERRO
               END-IF
           ELSE
               ADD 1                   TO ACU-CONTADOR
           END-IF.

      *----------------------------------------------------------------*
       23000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       24000-MONTAR-SAIDA              SECTION.
      *----------------------------------------------------------------*

           ADD 104                     TO GFCTFP-LL.
           ADD 1                       TO IND
                                          GFCTFP-QTDE-TOT-REG.

           MOVE DOCOR-EVNTO OF GFCTB092
                                       TO GFCTFP-DT-EVENTO(IND).
           MOVE CSERVC-TARIF OF GFCTB092
                                       TO GFCTFP-CD-TARIFA(IND).
           MOVE QEVNTO-AGRUP OF GFCTB092
                                       TO GFCTFP-QT-EVENTOS(IND).
           MOVE VTARIF-CADTR-SIST OF GFCTB092
                                       TO GFCTFP-VL-POTENCIAL(IND).
           MOVE VTARIF-LIQ-MOVTO OF GFCTB092
                                       TO GFCTFP-VL-CALCULADO(IND).
           MOVE DENVIO-MOVTO-TARIF OF GFCTB092
                                       TO GFCTFP-DT-MOVIMENTO(IND).
           MOVE CROTNA-ORIGE-MOVTO OF GFCTB092
                                       TO GFCTFP-ROTINA(IND).
           MOVE CNRO-ARQ-MOVTO OF GFCTB092
                                       TO GFCTFP-ARQUIVO(IND).
           MOVE CSEQ-MOVTO OF GFCTB092 TO GFCTFP-SEQ-ARQUIVO(IND).
           MOVE ACU-CONTADOR           TO GFCTFP-SEQ-TELA(IND).

           IF  CSIT-EVNTO-RECBD OF GFCTB092
                                       EQUAL 06 OR 07
               MOVE VTARIF-DEB-MOVTO OF GFCTB092
                                       TO GFCTFP-VL-ESTORNADO(IND)
           ELSE
               IF  DEFETV-DEB-MOVTO OF GFCTB092 (01:02)
                                       EQUAL '01' AND
                   DEFETV-DEB-MOVTO OF GFCTB092(04:02)
                                       EQUAL '01' AND
                   DEFETV-DEB-MOVTO OF GFCTB092(07:04)
                                       EQUAL '0001'
                   MOVE VTARIF-LIQ-MOVTO OF GFCTB092
                                       TO GFCTFP-VL-RECEBER(IND)
               ELSE
                   MOVE VTARIF-DEB-MOVTO OF GFCTB092
                                       TO GFCTFP-VL-RECEBIDO(IND)
CREMM              IF CSIT-EVNTO-RECBD OF GFCTB092
CREMM                                         EQUAL 03 OR 03 OR 13
CREMM                 COMPUTE GFCTFP-VL-RECEBER(IND) =
CREMM                 (VTARIF-LIQ-MOVTO OF GFCTB092  -
CREMM                  VTARIF-DEB-MOVTO OF GFCTB092)
CREMM              ELSE
CREMM                 COMPUTE GFCTFP-VL-PERDA(IND)  =
CREMM                 (VTARIF-LIQ-MOVTO OF GFCTB092 -
CREMM                  VTARIF-DEB-MOVTO OF GFCTB092)
CREMM              END-IF
               END-IF
           END-IF.

CREMM***** IF  GFCTFP-VL-RECEBIDO(IND) NOT EQUAL ZEROS
CREM******     COMPUTE GFCTFP-VL-PERDA(IND)
CREMM*****                             = GFCTFP-VL-CALCULADO(IND) -
CREMM*****                               GFCTFP-VL-RECEBIDO(IND)
CREMM***** END-IF.

           PERFORM 23000-LER-CSR-GFCTB092.

           IF  IND                     GREATER 6
               MOVE 'S'                TO GFCTFP-FIM

               IF  WRK-FINALIZAR       EQUAL 'N'
                   MOVE GFCTFP-SEQ-TELA(IND)
                                       TO GFCTFP-PONTEIRO
                                          GFCTFP-OCOR-ATUAL

                   MOVE 'S'            TO WRK-FINALIZAR
                                          GFCTFP-CONTINUA
               ELSE
                   MOVE GFCTFP-SEQ-TELA(IND) TO GFCTFP-OCOR-ATUAL
               END-IF
           ELSE
               MOVE GFCTFP-SEQ-TELA(IND) TO GFCTFP-OCOR-ATUAL
           END-IF.

      *----------------------------------------------------------------*
       24000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       25000-FECHAR-CSR-GFCTB092       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE   CSR-GFCTB092
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE '0007'             TO GFCT0M-LOCAL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTFP-COD-SQL-ERRO
               MOVE 0011               TO GFCTFP-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               MOVE 'MOVTO_EVNTO_CRRTT '
                                       TO GFCT0M-NOME-TAB
               MOVE 'GFCTB092'         TO WRK-TABELA
               PERFORM 14100-MOVER-ERRO-SQL
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
HEXA       MOVE    'GFCT0839'      TO          ERR-PGM.
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
