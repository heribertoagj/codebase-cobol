      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT0982.
       AUTHOR.     SONDAPROCWORK.

      *================================================================*
      *                   S O N D A   P R O C W O R K                  *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT0982                                    *
      *    ANALISTA....:   FRANCISCO LOURENCO         - SONDAPROCWORK  *
      *    DATA........:   NOV/2010                                    *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   GERAR DADOS PARA O DEMONSTRATIVO DE EVENTOS *
      *                    CORRENTISTAS.                               *
      *                                                                *
      *----------------------------------------------------------------*
      *    BANCO DE DADOS:                                             *
      *      DB2                                                       *
      *       TABLE                                INCLUDE/BOOK        *
      *       DB2PRD.TESTRN_COBRD_EXTRT              GFCTB0N0          *
      *       DB2PRD.TMOVTO_COBRD_EXTRT              GFCTB0N1          *
      *       DB2PRD.V01CTA_POUPANCA                 CLIEV007          *
      *       DB2PRD.V01CTA_CORRENTE                 CLIEV008          *
      *       DB2PRD.HIST_TARIF_PRINC                GFCTB087          *
      *                                                                *
      *----------------------------------------------------------------*
      *    MODULOS CHAMADOS:                                           *
      *      GFCT5522 - VERIFICACAO DE DISPONIBILIDADE ON-LINE         *
      *      GFCT5523 - OBTER DESCRICAO DE MENSAGENS                   *
      *      SENH0315 - OBTER JUNCAO.                                  *
      *      BRAD1470 - CONSISTENCIA DE DATAS                          *
      *                                                                *
      *----------------------------------------------------------------*
      *    NAVEGACAO                                                   *
      *      CHAMADO POR: GFCT7838                                     *
      *                                                                *
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
       01  FILLER                      PIC  X(050)         VALUE
           '*  INICIO DA WORKING GFCT0982  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*         INDEXADORES          *'.
      *----------------------------------------------------------------*

       01  IND-1                       PIC  9(007) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*         ACUMULADORES         *'.
      *----------------------------------------------------------------*

       01  WRK-DESPREZA                PIC  9(007)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*     VARIAVEIS AUXILIARES     *'.
      *----------------------------------------------------------------*

       01  WRK-FINALIZAR               PIC  X(001)         VALUE SPACES.

       01  WRK-FIM-CSR-GFCTB0N0        PIC  X(001)         VALUE SPACES.

       01  WRK-MODULO                  PIC  X(008)         VALUE SPACES.

       01  WRK-TABELA                  PIC  X(008)         VALUE SPACES.

       01  WRK-QUANTIDADE              PIC S9(007) COMP-3  VALUE ZEROS.

       01  WRK-WAO2-FLT-CONTA          PIC  9(013)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-WAO2-FLT-CONTA.
           05  FILLER                  PIC  9(006).
           05  WRK-COD-CONTA           PIC  9(007).

       01  WRK-QTD-EVENTO              PIC S9(007)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-QTD-EVENTO.
           05  WRK-WAO3-PTR-OCOR-TOTAL PIC  9(007).

       01  WRK-WAO3-PTR-OCOR-ATUAL     PIC  9(007) COMP-3  VALUE ZEROS.

       01  WRK-WAO3-QTDE-TOT-REG       PIC  9(006) COMP-3  VALUE ZEROS.

       01  WRK-WAO3-SEQ-TELA           PIC  9(004) COMP-3  VALUE ZEROS.

       01  WRK-CSERVC-TARIF-INI        PIC S9(005) COMP-3  VALUE ZEROS.

       01  WRK-CSERVC-TARIF-FIM        PIC S9(005) COMP-3  VALUE ZEROS.

       01  WRK-DEFETV-DEB-MOVTO-INI    PIC  X(010)         VALUE SPACES.

       01  WRK-DEFETV-DEB-MOVTO-FIM    PIC  X(010)         VALUE SPACES.

       01  WRK-CSEQ-ESTRN-EVNTO-INI    PIC S9(002) COMP-3  VALUE ZEROS.

       01  WRK-CSEQ-ESTRN-EVNTO-FIM    PIC S9(002) COMP-3  VALUE ZEROS.

       01  WRK-CSERVC-TARIF            PIC S9(005)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-CSERVC-TARIF.
           05  WRK-COD-TARIFA          PIC  9(005).

       01  WRK-QEVNTO-AGRUP            PIC S9(006)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-QEVNTO-AGRUP.
           05  WRK-QTDE-EVENTO         PIC  9(006).

       01  WRK-VTARIF-CADASTR-SIT      PIC S9(009)V99      VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-VTARIF-CADASTR-SIT.
           05  WRK-VLR-POTENCIAL       PIC  9(009)V99.

       01  WRK-VTARIF-DEB-MOVTO        PIC S9(009)V99      VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-VTARIF-DEB-MOVTO.
           05  WRK-VLR-DEBITO          PIC  9(009)V99.

       01  WRK-VRTEIO-VLR-SOLTD-COMP-3 PIC S9(009)V99 COMP-3
                                                           VALUE ZEROS.

       01  WRK-VRTEIO-VLR-SOLTD        PIC S9(009)V99      VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-VRTEIO-VLR-SOLTD.
           05  WRK-VLR-ESTORNO         PIC  9(009)V99.

       01  WRK-CNRO-ARQ-MOVTO          PIC S9(002)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-CNRO-ARQ-MOVTO.
           05  WRK-NUM-ARQUIVO         PIC  9(002).

       01  WRK-CSEQ-MOVTO              PIC S9(011)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-CSEQ-MOVTO.
           05  FILLER                  PIC  9(003).
           05  WRK-SEQ-ARQUIVO         PIC  9(008).

       01  WRK-SQLCODE                 PIC S9(009)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-SQLCODE.
           05  FILLER                  PIC  X(006).
           05  WRK-COD-SQL             PIC  9(003).

       01  WRK-DATA-DB2                PIC  X(010)         VALUE
           '01.01.0001'.
       01  FILLER                      REDEFINES WRK-DATA-DB2.
           05  WRK-DIA-DB2             PIC  9(002).
           05  FILLER                  PIC  X(001).
           05  WRK-MES-DB2             PIC  9(002).
           05  FILLER                  PIC  X(001).
           05  WRK-ANO-DB2             PIC  9(004).

       01  WRK-DATA-INI-INV.
           03  WRK-ANO-INI-INV         PIC  9(004)         VALUE ZEROS.
           03  WRK-MES-INI-INV         PIC  9(002)         VALUE ZEROS.
           03  WRK-DIA-INI-INV         PIC  9(002)         VALUE ZEROS.

       01  WRK-DATA-FIM-INV.
           03  WRK-ANO-FIM-INV         PIC  9(004)         VALUE ZEROS.
           03  WRK-MES-FIM-INV         PIC  9(002)         VALUE ZEROS.
           03  WRK-DIA-FIM-INV         PIC  9(002)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE MENSAGEM ***'.
      *----------------------------------------------------------------*

       01  WRK-MSG01.
           05  FILLER                  PIC  X(026)         VALUE
               'ERRO NA CHAMADA DO MODULO '.
           05  WRK-MSG01-MODULO        PIC  X(008)         VALUE SPACES.

       01  WRK-MSG02                   PIC  X(075)         VALUE
           'SISTEMA INDISPONIVEL'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*      AREA DO MODULO SENH     *'.
      *----------------------------------------------------------------*

       01  WRK-VERSAO-0315             PIC  X(006)         VALUE
           'VRS001'.

       01  WRK-MENSAGEM-0315.
           03  WRK-COD-MSG-0315        PIC  9(003)         VALUE ZEROS.
           03  WRK-HIFEN-MENSAGEM-0315 PIC  X(001)         VALUE SPACES.
           03  WRK-TEXTO-MENSAGEM-0315 PIC  X(079)         VALUE SPACES.

       01  WRK-COD-FUNCIONAL-0315      PIC  X(007)         VALUE SPACES.

       01  WRK-NOME-FUNC-0315          PIC  X(040)         VALUE SPACES.

       01  WRK-COD-BANCO-0315          PIC  9(005)         VALUE ZEROS.

       01  WRK-NOME-BANCO-0315         PIC  X(040)         VALUE SPACES.

       01  WRK-CNPJ-BANCO-0315.
           03  WRK-CNPJ-PRI-0315       PIC  9(009)         VALUE ZEROS.
           03  WRK-CNPJ-FIL-0315       PIC  9(005)         VALUE ZEROS.
           03  WRK-CNPJ-CON-0315       PIC  9(002)         VALUE ZEROS.

       01  WRK-COD-JUNCAO-0315         PIC  9(005)         VALUE ZEROS.

       01  WRK-NOME-DEPTO-0315         PIC  X(040)         VALUE SPACES.

       01  WRK-SECAO-0315              PIC  9(003)         VALUE ZEROS.

       01  WRK-STATUS-0315             PIC  X(001)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*   AREA DA BRAD1470    *'.
      *----------------------------------------------------------------*

       01  WRK-1470-DATA               PIC  9(008)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-1470-DATA.
           05  WRK-1470-DIA            PIC  9(002).
           05  WRK-1470-MES            PIC  9(002).
           05  WRK-1470-ANO            PIC  9(004).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5522 *'.
      *----------------------------------------------------------------*

       COPY I#GFCTFZ.

       COPY I#GFCTG1.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5523 *'.
      *----------------------------------------------------------------*

       COPY I#GFCTG2.

       COPY I#GFCTG3.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*     AREAS DE TABELAS DB2     *'.
      *----------------------------------------------------------------*

           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0N0
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0N1
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

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*  CURSORES  *'.
      *----------------------------------------------------------------*

           EXEC SQL
            DECLARE
                     CSR01-GFCTB0N1 CURSOR FOR
             SELECT
                     CROTNA_ORIGE_MOVTO,
                     DENVIO_MOVTO_TARIF,
                     CNRO_ARQ_MOVTO,
                     CSEQ_MOVTO,
                     CSERVC_TARIF,
                     QEVNTO_AGRUP,
                     VTARIF_CADTR_SIST,
                     VTARIF_DEB_MOVTO,
                     DEFETV_DEB_MOVTO,
                     CSIT_EVNTO_RECBD
               FROM
                     DB2PRD.TMOVTO_COBRD_EXTRT
              WHERE
                     CBCO_DSTNO_MOVTO   = :GFCTB0N1.CBCO-DSTNO-MOVTO
                AND  CAG_DSTNO_MOVTO    = :GFCTB0N1.CAG-DSTNO-MOVTO
                AND  CCTA_DSTNO_MOVTO   = :GFCTB0N1.CCTA-DSTNO-MOVTO
                AND  DEFETV_DEB_MOVTO
                                  BETWEEN :WRK-DEFETV-DEB-MOVTO-INI
                                      AND :WRK-DEFETV-DEB-MOVTO-FIM
                AND  CSERVC_TARIF BETWEEN :WRK-CSERVC-TARIF-INI
                                      AND :WRK-CSERVC-TARIF-FIM
           ORDER BY
                     DEFETV_DEB_MOVTO,
                     CSERVC_TARIF,
                     CNRO_ARQ_MOVTO,
                     CSEQ_MOVTO
           END-EXEC.

           EXEC SQL
            DECLARE
                     CSR02-GFCTB0N0 CURSOR FOR
             SELECT
                     VRTEIO_VLR_SOLTD
               FROM
                     DB2PRD.TESTRN_COBRD_EXTRT
              WHERE
                     CROTNA_ORIGE_MOVTO = :GFCTB0N0.CROTNA-ORIGE-MOVTO
                AND  DENVIO_MOVTO_TARIF = :GFCTB0N0.DENVIO-MOVTO-TARIF
                AND  CNRO_ARQ_MOVTO     = :GFCTB0N0.CNRO-ARQ-MOVTO
                AND  CSEQ_MOVTO         = :GFCTB0N0.CSEQ-MOVTO
                AND  CSEQ_ESTRN_EVNTO
                                  BETWEEN :WRK-CSEQ-ESTRN-EVNTO-INI
                                      AND :WRK-CSEQ-ESTRN-EVNTO-FIM
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*    FIM DA WORKING GFCT0982   *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY GFCTWAO2.
       COPY GFCTWAO3.
       COPY I#GFCT0M.

      *================================================================*
       PROCEDURE DIVISION              USING WAO2-ENTRADA
                                             WAO3-SAIDA
                                             GFCT0M-AREA-ERROS.
      *================================================================*

      ******************************************************************
      * ROTINA PARA CONTROLAR O PROCESSAMENTO                          *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-CONTROLAR-PROCESSAMENTO    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR-PROCESSAMENTO.

           PERFORM 2000-EXECUTAR-PROCESSAMENTO.

           PERFORM 9999-FINALIZAR-PROCESSAMENTO.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ROTINA PARA INICIALIZAR O PROCESSAMENTO                       *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR-PROCESSAMENTO  SECTION.
      *----------------------------------------------------------------*

           PERFORM 1100-VERIFICAR-ONLINE.

           PERFORM 1200-INICIALIZAR-AREA-SAIDA.

           PERFORM 1300-CONSISTIR-DADOS.

           PERFORM 1400-CONSISTE-AGENCIA-CONTA.

           IF  WAO2-FLT-COD-TARIFA     NOT EQUAL ZEROS
               PERFORM 1500-CONSISTIR-TARIFA
           END-IF.

           PERFORM 1600-OBTER-CODIGO-JUNCAO.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ROTINA PARA VERIFICAR DISPONIBILIDADE ONLINE DO SISTEMA       *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-VERIFICAR-ONLINE           SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTG1-SAIDA.

           INITIALIZE GFCTG1-SAIDA.

           MOVE +101                   TO GFCTFZ-LL.
           MOVE ZEROS                  TO GFCTFZ-ZZ.
           MOVE 'GFCT'                 TO GFCTFZ-SISTEMA.
           MOVE WAO2-TRANSACAO         TO GFCTFZ-TRANSACAO.
           MOVE WAO2-FUNCAO            TO GFCTFZ-FUNCAO.
           MOVE WAO2-FUNC-BDSCO        TO GFCTFZ-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTFZ-FIM.

           MOVE 'GFCT5522'             TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCTFZ-ENTRADA
                                             GFCTG1-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE ZEROS              TO RETURN-CODE
               MOVE SPACES             TO WAO3-DESC-MSG-ERRO
               MOVE 9                  TO WAO3-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MSG01-MODULO
               MOVE WRK-MSG01          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0982'         TO GFCT0M-TRANSACAO

               PERFORM 9999-FINALIZAR-PROCESSAMENTO
           END-IF.

           IF  GFCTG1-ERRO             NOT EQUAL ZEROS
               MOVE 9                  TO WAO3-ERRO

               IF  GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2'
                   MOVE SPACES         TO WAO3-DESC-MSG-ERRO
                   MOVE GFCTG1-DESC-MSG-ERRO(1:75)
                                       TO GFCT0M-TEXTO
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
               ELSE
                   MOVE GFCTG1-COD-SQL-ERRO
                                       TO WAO3-COD-SQL-ERRO
                   MOVE GFCTG1-DESC-MSG-ERRO
                                       TO WAO3-DESC-MSG-ERRO
               END-IF

               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
               MOVE 'GFCT0982'         TO GFCT0M-TRANSACAO

               PERFORM 9999-FINALIZAR-PROCESSAMENTO
           END-IF.

           IF  GFCTG1-CSIT-ONLINE      EQUAL 1
               MOVE 9                  TO WAO3-ERRO
               MOVE SPACES             TO WAO3-DESC-MSG-ERRO
               MOVE WRK-MSG02          TO GFCT0M-TEXTO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0982'         TO GFCT0M-TRANSACAO

               PERFORM 9999-FINALIZAR-PROCESSAMENTO
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ROTINA PARA INICIALIZAR A AREA DE SAIDA                       *
      ******************************************************************
      *----------------------------------------------------------------*
       1200-INICIALIZAR-AREA-SAIDA     SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO WAO3-SAIDA.

           INITIALIZE                  WAO3-SAIDA.

           MOVE +205                   TO WAO3-LL.
           MOVE WAO2-ZZ                TO WAO3-ZZ.
           MOVE WAO2-TRANSACAO         TO WAO3-TRANSACAO.
           MOVE WAO2-FUNCAO            TO WAO3-FUNCAO.
           MOVE WAO2-FILTRO            TO WAO3-FILTRO.
           MOVE WAO2-PONTEIRO          TO WAO3-PONTEIRO.
           MOVE WAO2-QTDE-OCOR         TO WAO3-QTDE-OCOR.
           MOVE WAO2-QTDE-TOT-REG      TO WAO3-QTDE-TOT-REG.
           MOVE WAO2-DEPTO-BDSCO       TO WAO3-DEPTO-BDSCO.
           MOVE WAO2-FUNC-BDSCO        TO WAO3-FUNC-BDSCO.

           MOVE 'N'                    TO WAO3-FIM.

           MOVE ZEROS                  TO WAO3-ERRO
                                          WAO3-COD-MSG-ERRO
                                          WAO3-COD-SQL-ERRO.

           MOVE '0000'                 TO GFCTG2-COD-MSG.

           PERFORM 9990-OBTER-DESCRICAO-MENSAGEM.

           MOVE GFCTG3-DESC-MSG        TO WAO3-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ROTINA PARA CONSISTIR DADOS RECEBIDOS                         *
      ******************************************************************
      *----------------------------------------------------------------*
       1300-CONSISTIR-DADOS            SECTION.
      *----------------------------------------------------------------*

           IF (WAO2-TRANSACAO          EQUAL SPACES OR LOW-VALUES) OR
              (WAO2-FUNCAO             EQUAL SPACES OR LOW-VALUES) OR
              (WAO2-FLT-AGENCIA        NOT NUMERIC)                OR
              (WAO2-FLT-AGENCIA        EQUAL ZEROS)                OR
              (WAO2-FLT-CONTA          NOT NUMERIC)                OR
              (WAO2-FLT-CONTA          EQUAL ZEROS)                OR
              (WAO2-FLT-PERIODO-INI(01:02)
                                       NOT NUMERIC)                OR
              (WAO2-FLT-PERIODO-INI(04:02)
                                       NOT NUMERIC)                OR
              (WAO2-FLT-PERIODO-INI(07:04)
                                       NOT NUMERIC)                OR
              (WAO2-FLT-PERIODO-FIM(01:02)
                                       NOT NUMERIC)                OR
              (WAO2-FLT-PERIODO-FIM(04:02)
                                       NOT NUMERIC)                OR
              (WAO2-FLT-PERIODO-FIM(07:04)
                                       NOT NUMERIC)                OR
              (WAO2-FLT-COD-TARIFA     NOT NUMERIC)                OR
              (WAO2-PTR-OCOR-ATUAL     NOT NUMERIC)                OR
              (WAO2-PTR-OCOR-TOTAL     NOT NUMERIC)                OR
              (WAO2-PTR-CONTINUA       NOT EQUAL 'S' AND 'N')      OR
              (WAO2-QTDE-OCOR          NOT NUMERIC)                OR
              (WAO2-QTDE-TOT-REG       NOT NUMERIC)                OR
              (WAO2-DEPTO-BDSCO        NOT NUMERIC)                OR
              (WAO2-FUNC-BDSCO         EQUAL SPACES OR LOW-VALUES) OR
              (WAO2-FIM                NOT EQUAL 'S' AND 'N')
               MOVE 'S'                TO WAO3-FIM

               MOVE 1                  TO WAO3-ERRO
               MOVE ZEROS              TO WAO3-COD-SQL-ERRO
               MOVE 0001               TO WAO3-COD-MSG-ERRO

               MOVE '0001'             TO GFCTG2-COD-MSG

               PERFORM 9990-OBTER-DESCRICAO-MENSAGEM

               MOVE GFCTG3-DESC-MSG    TO WAO3-DESC-MSG-ERRO

               PERFORM 9999-FINALIZAR-PROCESSAMENTO
           END-IF.

           MOVE WAO2-FLT-PERIODO-INI   TO WRK-DATA-DB2.
           MOVE WRK-DIA-DB2            TO WRK-1470-DIA.
           MOVE WRK-MES-DB2            TO WRK-1470-MES.
           MOVE WRK-ANO-DB2            TO WRK-1470-ANO.

           PERFORM 1310-CONSISTIR-DATA.

           MOVE WRK-DIA-DB2            TO WRK-DIA-INI-INV.
           MOVE WRK-MES-DB2            TO WRK-MES-INI-INV.
           MOVE WRK-ANO-DB2            TO WRK-ANO-INI-INV.

           MOVE WAO2-FLT-PERIODO-FIM   TO WRK-DATA-DB2.
           MOVE WRK-DIA-DB2            TO WRK-1470-DIA.
           MOVE WRK-MES-DB2            TO WRK-1470-MES.
           MOVE WRK-ANO-DB2            TO WRK-1470-ANO.

           PERFORM 1310-CONSISTIR-DATA.

           MOVE WRK-DIA-DB2            TO WRK-DIA-FIM-INV.
           MOVE WRK-MES-DB2            TO WRK-MES-FIM-INV.
           MOVE WRK-ANO-DB2            TO WRK-ANO-FIM-INV.

           IF  WRK-DATA-FIM-INV        LESS WRK-DATA-INI-INV
               MOVE 'S'                TO WAO3-FIM

               MOVE 1                  TO WAO3-ERRO
               MOVE 0201               TO WAO3-COD-MSG-ERRO
               MOVE ZEROS              TO WAO3-COD-SQL-ERRO

               MOVE '0201'             TO GFCTG2-COD-MSG

               PERFORM 9990-OBTER-DESCRICAO-MENSAGEM

               MOVE GFCTG3-DESC-MSG    TO WAO3-DESC-MSG-ERRO

               PERFORM 9999-FINALIZAR-PROCESSAMENTO
           END-IF.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ROTINA PARA CONSISTIR DATAS RECEBIDAS                         *
      ******************************************************************
      *----------------------------------------------------------------*
       1310-CONSISTIR-DATA             SECTION.
      *----------------------------------------------------------------*

           CALL 'BRAD1470'             USING WRK-1470-DATA.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'S'                TO WAO3-FIM

               MOVE 1                  TO WAO3-ERRO
               MOVE ZEROS              TO WAO3-COD-SQL-ERRO
               MOVE 1282               TO WAO3-COD-MSG-ERRO

               MOVE '1282'             TO GFCTG2-COD-MSG

               PERFORM 9990-OBTER-DESCRICAO-MENSAGEM

               MOVE GFCTG3-DESC-MSG    TO WAO3-DESC-MSG-ERRO
               MOVE ZEROS              TO RETURN-CODE

               PERFORM 9999-FINALIZAR-PROCESSAMENTO
           END-IF.

      *----------------------------------------------------------------*
       1310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ROTINA PARA CONSISTIR AGENCIA/CONTA                           *
      ******************************************************************
      *----------------------------------------------------------------*
       1400-CONSISTE-AGENCIA-CONTA     SECTION.
      *----------------------------------------------------------------*

           PERFORM 1410-VERIFICA-CONTA-CORRENTE.

           IF  SQLCODE                 EQUAL +100
               PERFORM 1420-VERIFICA-CONTA-POUPANCA

               IF  SQLCODE             EQUAL +100
                   MOVE 'S'            TO WAO3-FIM

                   MOVE 1              TO WAO3-ERRO
                   MOVE 0220           TO WAO3-COD-MSG-ERRO
                   MOVE ZEROS          TO WAO3-COD-SQL-ERRO

                   MOVE '0220'         TO GFCTG2-COD-MSG

                   PERFORM 9990-OBTER-DESCRICAO-MENSAGEM

                   MOVE GFCTG3-DESC-MSG
                                       TO WAO3-DESC-MSG-ERRO

                   PERFORM 9999-FINALIZAR-PROCESSAMENTO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ROTINA PARA VERIFICAR CONTA CORRENTE                          *
      ******************************************************************
      *----------------------------------------------------------------*
       1410-VERIFICA-CONTA-CORRENTE    SECTION.
      *----------------------------------------------------------------*

           MOVE WAO2-FLT-AGENCIA       TO CJUNC-DEPDC OF CLIEV008.

           MOVE WAO2-FLT-CONTA         TO WRK-WAO2-FLT-CONTA.
           MOVE WRK-COD-CONTA          TO CCTA-CLI OF CLIEV008.

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
               MOVE SPACES             TO GFCT0M-ERRO-SQL

               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'V01CTA_CORRENTE'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0001'             TO GFCT0M-LOCAL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-SQLCODE

               MOVE 0012               TO WAO3-COD-MSG-ERRO
               MOVE WRK-COD-SQL        TO WAO3-COD-SQL-ERRO

               MOVE '0012'             TO GFCTG2-COD-MSG

               MOVE 'CLIEV008'         TO WRK-TABELA

               PERFORM 9900-MOVER-ERRO-SQL
           END-IF.

      *----------------------------------------------------------------*
       1410-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ROTINA PARA VERIFICAR CONTA POUPANCA                          *
      ******************************************************************
      *----------------------------------------------------------------*
       1420-VERIFICA-CONTA-POUPANCA    SECTION.
      *----------------------------------------------------------------*

           MOVE WAO2-FLT-AGENCIA       TO CJUNC-DEPDC OF CLIEV007.

           MOVE WAO2-FLT-CONTA         TO WRK-WAO2-FLT-CONTA.
           MOVE WRK-COD-CONTA          TO CCTA-CLI OF CLIEV007.

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
               MOVE SPACES             TO GFCT0M-ERRO-SQL

               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'V01CTA_POUPANCA'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0002'             TO GFCT0M-LOCAL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-SQLCODE

               MOVE 0012               TO WAO3-COD-MSG-ERRO
               MOVE WRK-COD-SQL        TO WAO3-COD-SQL-ERRO

               MOVE '0012'             TO GFCTG2-COD-MSG

               MOVE 'CLIEV007'         TO WRK-TABELA

               PERFORM 9900-MOVER-ERRO-SQL
           END-IF.

      *----------------------------------------------------------------*
       1420-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ROTINA PARA CONSISTIR TARIFA                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       1500-CONSISTIR-TARIFA           SECTION.
      *----------------------------------------------------------------*

           MOVE WAO2-FLT-COD-TARIFA    TO CSERVC-TARIF OF GFCTB087.

           PERFORM 1510-OBTER-MAX-SEQ-MANUTENCAO.

           EXEC SQL
             SELECT
                     RSERVC_TARIF_REDZD
               INTO
                    :GFCTB087.RSERVC-TARIF-REDZD
               FROM
                     DB2PRD.HIST_TARIF_PRINC
              WHERE
                     CSERVC_TARIF       = :GFCTB087.CSERVC-TARIF
                AND  HIDTFD_MANUT_SERVC
                                        = :GFCTB087.HIDTFD-MANUT-SERVC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-ERRO-SQL

               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'HIST_TARIF_PRINC' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0003'             TO GFCT0M-LOCAL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-SQLCODE

               MOVE 0012               TO WAO3-COD-MSG-ERRO
               MOVE WRK-COD-SQL        TO WAO3-COD-SQL-ERRO

               MOVE '0012'             TO GFCTG2-COD-MSG

               MOVE 'GFCTB087'         TO WRK-TABELA

               PERFORM 9900-MOVER-ERRO-SQL
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WAO3-FIM

               MOVE 1                  TO WAO3-ERRO
               MOVE 0054               TO WAO3-COD-MSG-ERRO
               MOVE ZEROS              TO WAO3-COD-SQL-ERRO

               MOVE '0054'             TO GFCTG2-COD-MSG

               PERFORM 9990-OBTER-DESCRICAO-MENSAGEM

               MOVE GFCTG3-DESC-MSG    TO WAO3-DESC-MSG-ERRO

               PERFORM 9999-FINALIZAR-PROCESSAMENTO
           END-IF.

      *----------------------------------------------------------------*
       1500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ROTINA PARA OBTER ULTIMA SEQUENCIA DE MANUTENCAO DA TARIFA    *
      ******************************************************************
      *----------------------------------------------------------------*
       1510-OBTER-MAX-SEQ-MANUTENCAO  SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             SELECT
                     MAX(HIDTFD_MANUT_SERVC)
               INTO
                    :GFCTB087.HIDTFD-MANUT-SERVC
               FROM
                     DB2PRD.HIST_TARIF_PRINC
              WHERE
                     CSERVC_TARIF       = :GFCTB087.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND -305) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-ERRO-SQL

               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'HIST_TARIF_PRINC' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0004'             TO GFCT0M-LOCAL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-SQLCODE

               MOVE 0012               TO WAO3-COD-MSG-ERRO
               MOVE WRK-COD-SQL        TO WAO3-COD-SQL-ERRO

               MOVE '0012'             TO GFCTG2-COD-MSG

               MOVE 'GFCTB087'         TO WRK-TABELA

               PERFORM 9900-MOVER-ERRO-SQL
           END-IF.

           IF  SQLCODE                 EQUAL -305
               MOVE '0001-01-01-01.01.01.000001'
                                       TO HIDTFD-MANUT-SERVC
                                                            OF GFCTB087
           END-IF.

      *----------------------------------------------------------------*
       1510-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ROTINA PARA OBTER CODIGO DE JUNCAO                            *
      ******************************************************************
      *----------------------------------------------------------------*
       1600-OBTER-CODIGO-JUNCAO        SECTION.
      *----------------------------------------------------------------*

           MOVE WAO2-FUNC-BDSCO        TO WRK-COD-FUNCIONAL-0315.

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

           IF  RETURN-CODE             EQUAL 12
               MOVE 'S'                TO WAO3-FIM

               MOVE 1                  TO WAO3-ERRO
               MOVE 0056               TO WAO3-COD-MSG-ERRO
               MOVE ZEROS              TO WAO3-COD-SQL-ERRO

               MOVE '0056'             TO GFCTG2-COD-MSG

               PERFORM 9990-OBTER-DESCRICAO-MENSAGEM

               MOVE GFCTG3-DESC-MSG    TO WAO3-DESC-MSG-ERRO

               PERFORM 9999-FINALIZAR-PROCESSAMENTO
           END-IF.

           IF  RETURN-CODE             EQUAL 04
               IF  WRK-COD-MSG-0315    EQUAL 001
                   MOVE 'S'            TO WAO3-FIM

                   MOVE 1              TO WAO3-ERRO
                   MOVE 0057           TO WAO3-COD-MSG-ERRO
                   MOVE ZEROS          TO WAO3-COD-SQL-ERRO

                   MOVE '0057'         TO GFCTG2-COD-MSG

                   PERFORM 9990-OBTER-DESCRICAO-MENSAGEM

                   MOVE GFCTG3-DESC-MSG
                                       TO WAO3-DESC-MSG-ERRO

                   PERFORM 9999-FINALIZAR-PROCESSAMENTO
               END-IF

               IF  WRK-COD-MSG-0315    EQUAL 144
                   MOVE 'S'            TO WAO3-FIM

                   MOVE 1              TO WAO3-ERRO
                   MOVE 0058           TO WAO3-COD-MSG-ERRO
                   MOVE ZEROS          TO WAO3-COD-SQL-ERRO

                   MOVE '0058'         TO GFCTG2-COD-MSG

                   PERFORM 9990-OBTER-DESCRICAO-MENSAGEM

                   MOVE GFCTG3-DESC-MSG
                                       TO WAO3-DESC-MSG-ERRO

                   PERFORM 9999-FINALIZAR-PROCESSAMENTO
               END-IF

               IF  WRK-COD-MSG-0315    EQUAL 242
                   MOVE 'S'            TO WAO3-FIM

                   MOVE 1              TO WAO3-ERRO
                   MOVE 0059           TO WAO3-COD-MSG-ERRO
                   MOVE ZEROS          TO WAO3-COD-SQL-ERRO

                   MOVE '0059'         TO GFCTG2-COD-MSG

                   PERFORM 9990-OBTER-DESCRICAO-MENSAGEM

                   MOVE GFCTG3-DESC-MSG
                                       TO WAO3-DESC-MSG-ERRO

                   PERFORM 9999-FINALIZAR-PROCESSAMENTO
               END-IF
           END-IF.

           IF  RETURN-CODE             EQUAL ZEROS
               IF  WRK-COD-MSG-0315    EQUAL 138
                   MOVE 'S'            TO WAO3-FIM

                   MOVE 1              TO WAO3-ERRO
                   MOVE 0060           TO WAO3-COD-MSG-ERRO
                   MOVE ZEROS          TO WAO3-COD-SQL-ERRO

                   MOVE '0060'         TO GFCTG2-COD-MSG

                   PERFORM 9990-OBTER-DESCRICAO-MENSAGEM

                   MOVE GFCTG3-DESC-MSG
                                       TO WAO3-DESC-MSG-ERRO

                   PERFORM 9999-FINALIZAR-PROCESSAMENTO
               END-IF
           END-IF.

           MOVE WRK-COD-JUNCAO-0315    TO WAO3-DEPTO-BDSCO.

      *----------------------------------------------------------------*
       1600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ROTINA PARA EXECUTAR O PROCESSAMENTO                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-EXECUTAR-PROCESSAMENTO     SECTION.
      *----------------------------------------------------------------*

           MOVE 237                    TO CBCO-DSTNO-MOVTO OF GFCTB0N1.

           MOVE WAO2-FLT-AGENCIA       TO CAG-DSTNO-MOVTO  OF GFCTB0N1.
           MOVE WAO2-FLT-CONTA         TO CCTA-DSTNO-MOVTO OF GFCTB0N1.

           MOVE WAO2-FLT-PERIODO-INI   TO WRK-DEFETV-DEB-MOVTO-INI.
           MOVE WAO2-FLT-PERIODO-FIM   TO WRK-DEFETV-DEB-MOVTO-FIM.

           IF  WAO2-FLT-COD-TARIFA     EQUAL ZEROS
               MOVE ZEROS              TO WRK-CSERVC-TARIF-INI
               MOVE 999999             TO WRK-CSERVC-TARIF-FIM
           ELSE
               MOVE WAO2-FLT-COD-TARIFA
                                       TO WRK-CSERVC-TARIF-INI
                                          WRK-CSERVC-TARIF-FIM
           END-IF.

           IF  WAO2-PTR-OCOR-TOTAL     EQUAL ZEROS
               PERFORM 2100-CONTAR-QTDE-EVENTO
           END-IF.

           MOVE 'N'                    TO WRK-FINALIZAR.

           PERFORM 2300-ABRIR-CSR-GFCTB0N1.

           MOVE WAO2-PTR-OCOR-ATUAL    TO WRK-DESPREZA.

           PERFORM 2400-LER-CSR-GFCTB0N1.

           PERFORM 2400-LER-CSR-GFCTB0N1
           VARYING IND-1               FROM 1 BY 1
             UNTIL IND-1               GREATER WRK-DESPREZA
                OR WRK-FINALIZAR       EQUAL 'S'.

           IF  WRK-FINALIZAR           EQUAL 'S'
               IF  WAO3-PTR-OCOR-ATUAL EQUAL ZEROS
                   MOVE 'S'            TO WAO3-FIM

                   MOVE 1              TO WAO3-ERRO
                   MOVE ZEROS          TO WAO3-COD-SQL-ERRO

                   MOVE 0202           TO WAO3-COD-MSG-ERRO

                   MOVE '0202'         TO GFCTG2-COD-MSG

                   PERFORM 9990-OBTER-DESCRICAO-MENSAGEM

                   MOVE GFCTG3-DESC-MSG
                                       TO WAO3-DESC-MSG-ERRO
               END-IF
           END-IF.

           MOVE WAO3-PTR-OCOR-ATUAL    TO WRK-WAO3-PTR-OCOR-ATUAL.
           MOVE WAO3-QTDE-TOT-REG      TO WRK-WAO3-QTDE-TOT-REG.

           MOVE ZEROS                  TO WRK-WAO3-SEQ-TELA.

           MOVE ZEROS                  TO IND-1.

           PERFORM 2200-MONTAR-OCORR-SAIDA
             UNTIL WRK-FINALIZAR       EQUAL 'S'.

           PERFORM 2500-FECHAR-CSR-GFCTB0N1.

           MOVE WRK-WAO3-PTR-OCOR-ATUAL
                                       TO WAO3-PTR-OCOR-ATUAL.
           MOVE WRK-WAO3-QTDE-TOT-REG  TO WAO3-QTDE-TOT-REG.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ROTINA PARA CONTAR A QUANTIDADE DE EVENTOS DA LISTA           *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-CONTAR-QTDE-EVENTO         SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             SELECT
                     COUNT(CSEQ_MOVTO)
               INTO
                    :WRK-QUANTIDADE
               FROM
                     DB2PRD.TMOVTO_COBRD_EXTRT
              WHERE
                     CBCO_DSTNO_MOVTO   = :GFCTB0N1.CBCO-DSTNO-MOVTO
                AND  CAG_DSTNO_MOVTO    = :GFCTB0N1.CAG-DSTNO-MOVTO
                AND  CCTA_DSTNO_MOVTO   = :GFCTB0N1.CCTA-DSTNO-MOVTO
                AND  DEFETV_DEB_MOVTO
                                  BETWEEN :WRK-DEFETV-DEB-MOVTO-INI
                                      AND :WRK-DEFETV-DEB-MOVTO-FIM
                AND  CSERVC_TARIF BETWEEN :WRK-CSERVC-TARIF-INI
                                      AND :WRK-CSERVC-TARIF-FIM
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-ERRO-SQL

               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'TMOVTO_COBRD_EXTRT'
                                       TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0005'             TO GFCT0M-LOCAL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-SQLCODE

               MOVE 0501               TO WAO3-COD-MSG-ERRO
               MOVE WRK-COD-SQL        TO WAO3-COD-SQL-ERRO

               MOVE '0501'             TO GFCTG2-COD-MSG

               MOVE 'GFCTB0N1'         TO WRK-TABELA

               PERFORM 9900-MOVER-ERRO-SQL
           END-IF.

           MOVE WRK-QUANTIDADE         TO WRK-QTD-EVENTO.
           MOVE WRK-WAO3-PTR-OCOR-TOTAL
                                       TO WAO3-PTR-OCOR-TOTAL.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ROTINA PARA MONTAR DADOS OCORRENCIA DE SAIDA                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-MONTAR-OCORR-SAIDA         SECTION.
      *----------------------------------------------------------------*

           ADD +115                    TO WAO3-LL.

           ADD  1                      TO WRK-WAO3-PTR-OCOR-ATUAL.

           ADD  1                      TO WRK-WAO3-QTDE-TOT-REG.

           ADD  1                      TO IND-1.

           MOVE 'S'                    TO WAO3-SEL(IND-1).

           MOVE DEFETV-DEB-MOVTO OF GFCTB0N1
                                       TO WAO3-DT-EVENTO(IND-1).

           MOVE CSERVC-TARIF OF GFCTB0N1
                                       TO WRK-CSERVC-TARIF.
           MOVE WRK-COD-TARIFA         TO WAO3-CD-TARIFA(IND-1).

           MOVE QEVNTO-AGRUP OF GFCTB0N1
                                       TO WRK-QEVNTO-AGRUP.
           MOVE WRK-QTDE-EVENTO        TO WAO3-QT-EVENTOS(IND-1).

           MOVE VTARIF-CADTR-SIST OF GFCTB0N1
                                       TO WRK-VTARIF-CADASTR-SIT.
           MOVE WRK-VLR-POTENCIAL      TO WAO3-VL-POTENCIAL(IND-1).

           MOVE VTARIF-DEB-MOVTO OF GFCTB0N1
                                       TO WRK-VTARIF-DEB-MOVTO.
           MOVE WRK-VLR-DEBITO         TO WAO3-VL-CALCULADO(IND-1),
                                          WAO3-VL-RECEBIDO(IND-1).

           MOVE ZEROS                  TO WRK-VRTEIO-VLR-SOLTD.

           IF  CSIT-EVNTO-RECBD OF GFCTB0N1
                                       EQUAL 06 OR
               CSIT-EVNTO-RECBD OF GFCTB0N1
                                       EQUAL 07
               PERFORM 2210-TRATAR-VALOR-ESTORNO
           END-IF.

           MOVE WRK-VLR-ESTORNO        TO WAO3-VL-ESTORNADO(IND-1).

           MOVE ZEROS                  TO WAO3-VL-RECEBER(IND-1),
                                          WAO3-VL-PERDA(IND-1).

           MOVE CROTNA-ORIGE-MOVTO OF GFCTB0N1
                                       TO WAO3-ROTINA(IND-1).
           MOVE DENVIO-MOVTO-TARIF OF GFCTB0N1
                                       TO WAO3-DT-MOVIMENTO(IND-1).

           MOVE CNRO-ARQ-MOVTO OF GFCTB0N1
                                       TO WRK-CNRO-ARQ-MOVTO.
           MOVE WRK-NUM-ARQUIVO        TO WAO3-ARQUIVO(IND-1).

           MOVE CSEQ-MOVTO OF GFCTB0N1 TO WRK-CSEQ-MOVTO.
           MOVE WRK-SEQ-ARQUIVO        TO WAO3-SEQ-ARQUIVO(IND-1).

           ADD  1                      TO WRK-WAO3-SEQ-TELA.
           MOVE WRK-WAO3-SEQ-TELA      TO WAO3-SEQ-TELA(IND-1).

           PERFORM 2400-LER-CSR-GFCTB0N1.

           IF  IND-1                   GREATER 6
               MOVE 'S'                TO WAO3-FIM

               IF  WRK-FINALIZAR       EQUAL 'N'
                   MOVE 'S'            TO WAO3-PTR-CONTINUA
               END-IF

               MOVE 'S'                TO WRK-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ROTINA PARA TRATAR O VALOR DE ESTORNO COBRADO NO EXTRATO      *
      ******************************************************************
      *----------------------------------------------------------------*
       2210-TRATAR-VALOR-ESTORNO       SECTION.
      *----------------------------------------------------------------*


           MOVE CROTNA-ORIGE-MOVTO OF GFCTB0N1
                                       TO CROTNA-ORIGE-MOVTO
                                                            OF GFCTB0N0.
           MOVE DENVIO-MOVTO-TARIF OF GFCTB0N1
                                       TO DENVIO-MOVTO-TARIF
                                                            OF GFCTB0N0.
           MOVE CNRO-ARQ-MOVTO OF GFCTB0N1
                                       TO CNRO-ARQ-MOVTO    OF GFCTB0N0.
           MOVE CSEQ-MOVTO OF GFCTB0N1 TO CSEQ-MOVTO        OF GFCTB0N0.

           MOVE ZEROS                  TO WRK-CSEQ-ESTRN-EVNTO-INI.
           MOVE 99                     TO WRK-CSEQ-ESTRN-EVNTO-FIM.

           MOVE 'N'                    TO WRK-FIM-CSR-GFCTB0N0.

           MOVE ZEROS                  TO WRK-VRTEIO-VLR-SOLTD-COMP-3.

           PERFORM 2212-ABRIR-CSR-GFCTB0N0.

           PERFORM 2213-LER-CSR-GFCTB0N0.

           PERFORM 2211-ACUMULAR-VALOR-ESTORNO
             UNTIL WRK-FIM-CSR-GFCTB0N0
                                       EQUAL 'S'.

           PERFORM 2214-FECHAR-CSR-GFCTB0N0.

           MOVE WRK-VRTEIO-VLR-SOLTD-COMP-3
                                       TO WRK-VRTEIO-VLR-SOLTD.

      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ROTINA PARA ACUMULAR O VALOR DE ESTORNO COBRADO NO EXTRATO    *
      ******************************************************************
      *----------------------------------------------------------------*
       2211-ACUMULAR-VALOR-ESTORNO          SECTION.
      *----------------------------------------------------------------*

           ADD  VRTEIO-VLR-SOLTD OF GFCTB0N0
                                       TO WRK-VRTEIO-VLR-SOLTD-COMP-3.

           PERFORM 2213-LER-CSR-GFCTB0N0.

      *----------------------------------------------------------------*
       2211-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ROTINA PARA ABRIR O CURSOR                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2212-ABRIR-CSR-GFCTB0N0              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               OPEN
                     CSR02-GFCTB0N0
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-ERRO-SQL

               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'TESTRN_COBRD_EXTRT'
                                       TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE '0006'             TO GFCT0M-LOCAL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-SQLCODE

               MOVE 0005               TO WAO3-COD-MSG-ERRO
               MOVE WRK-COD-SQL        TO WAO3-COD-SQL-ERRO

               MOVE '0005'             TO GFCTG2-COD-MSG

               MOVE 'GFCTB0N0'         TO WRK-TABELA

               PERFORM 9900-MOVER-ERRO-SQL
           END-IF.

           MOVE ZEROS                  TO WRK-DESPREZA.

      *----------------------------------------------------------------*
       2212-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ROTINA PARA LER O CURSOR                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2213-LER-CSR-GFCTB0N0          SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
              FETCH
                     CSR02-GFCTB0N0
               INTO
                    :GFCTB0N0.VRTEIO-VLR-SOLTD
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-ERRO-SQL

               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'TESTRN_COBRD_EXTRT'
                                       TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE '0007'             TO GFCT0M-LOCAL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-SQLCODE

               MOVE 0006               TO WAO3-COD-MSG-ERRO
               MOVE WRK-COD-SQL        TO WAO3-COD-SQL-ERRO

               MOVE '0006'             TO GFCTG2-COD-MSG

               MOVE 'GFCTB0N0'         TO WRK-TABELA

               PERFORM 9900-MOVER-ERRO-SQL
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-CSR-GFCTB0N0
           END-IF.

      *----------------------------------------------------------------*
       2213-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ROTINA PARA FECHAR O CURSOR                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2214-FECHAR-CSR-GFCTB0N0       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
              CLOSE
                     CSR02-GFCTB0N0
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-ERRO-SQL

               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'TESTRN_COBRD_EXTRT'
                                       TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE '0008'             TO GFCT0M-LOCAL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-SQLCODE

               MOVE 0011               TO WAO3-COD-MSG-ERRO
               MOVE WRK-COD-SQL        TO WAO3-COD-SQL-ERRO

               MOVE '0011'             TO GFCTG2-COD-MSG

               MOVE 'GFCTB0N0'         TO WRK-TABELA

               PERFORM 9900-MOVER-ERRO-SQL
           END-IF.

      *----------------------------------------------------------------*
       2214-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ROTINA PARA ABRIR O CURSOR                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2300-ABRIR-CSR-GFCTB0N1              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               OPEN
                     CSR01-GFCTB0N1
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-ERRO-SQL

               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'TMOVTO_COBRD_EXTRT'
                                       TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE '0009'             TO GFCT0M-LOCAL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-SQLCODE

               MOVE 0005               TO WAO3-COD-MSG-ERRO
               MOVE WRK-COD-SQL        TO WAO3-COD-SQL-ERRO

               MOVE '0005'             TO GFCTG2-COD-MSG

               MOVE 'GFCTB0N1'         TO WRK-TABELA

               PERFORM 9900-MOVER-ERRO-SQL
           END-IF.

           MOVE ZEROS                  TO WRK-DESPREZA.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ROTINA PARA LER O CURSOR                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2400-LER-CSR-GFCTB0N1          SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
              FETCH
                     CSR01-GFCTB0N1
               INTO
                    :GFCTB0N1.CROTNA-ORIGE-MOVTO,
                    :GFCTB0N1.DENVIO-MOVTO-TARIF,
                    :GFCTB0N1.CNRO-ARQ-MOVTO,
                    :GFCTB0N1.CSEQ-MOVTO,
                    :GFCTB0N1.CSERVC-TARIF,
                    :GFCTB0N1.QEVNTO-AGRUP,
                    :GFCTB0N1.VTARIF-CADTR-SIST,
                    :GFCTB0N1.VTARIF-DEB-MOVTO,
                    :GFCTB0N1.DEFETV-DEB-MOVTO,
                    :GFCTB0N1.CSIT-EVNTO-RECBD
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-ERRO-SQL

               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'TMOVTO_COBRD_EXTRT'
                                       TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE '0010'             TO GFCT0M-LOCAL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-SQLCODE

               MOVE 0006               TO WAO3-COD-MSG-ERRO
               MOVE WRK-COD-SQL        TO WAO3-COD-SQL-ERRO

               MOVE '0006'             TO GFCTG2-COD-MSG

               MOVE 'GFCTB0N1'         TO WRK-TABELA

               PERFORM 9900-MOVER-ERRO-SQL
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WAO3-FIM,
                                          WRK-FINALIZAR

               IF  IND-1               EQUAL ZEROS
                   IF  WAO3-PTR-OCOR-ATUAL
                                       EQUAL ZEROS
                       MOVE 1          TO WAO3-ERRO
                       MOVE 0202       TO WAO3-COD-MSG-ERRO
                       MOVE ZEROS      TO WAO3-COD-SQL-ERRO

                       MOVE '0202'     TO GFCTG2-COD-MSG

                       PERFORM 9990-OBTER-DESCRICAO-MENSAGEM

                       MOVE GFCTG3-DESC-MSG
                                       TO WAO3-DESC-MSG-ERRO
                   END-IF
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ROTINA PARA FECHAR O CURSOR                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2500-FECHAR-CSR-GFCTB0N1       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
              CLOSE
                     CSR01-GFCTB0N1
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-ERRO-SQL

               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'TMOVTO_COBRD_EXTRT'
                                       TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE '0011'             TO GFCT0M-LOCAL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-SQLCODE

               MOVE 0011               TO WAO3-COD-MSG-ERRO
               MOVE WRK-COD-SQL        TO WAO3-COD-SQL-ERRO

               MOVE '0011'             TO GFCTG2-COD-MSG

               MOVE 'GFCTB0N1'         TO WRK-TABELA

               PERFORM 9900-MOVER-ERRO-SQL
           END-IF.

      *----------------------------------------------------------------*
       2500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ROTINA PARA MONTAR OS DADOS DE ERROS NO ACESSO A TABELA       *
      ******************************************************************
      *----------------------------------------------------------------*
       9900-MOVER-ERRO-SQL            SECTION.
      *----------------------------------------------------------------*

           MOVE 'S'                    TO WAO3-FIM.

           MOVE 9                      TO WAO3-ERRO.
           MOVE 'GFCT0982'             TO GFCT0M-PROGRAMA.
           MOVE 'DB2'                  TO GFCT0M-TIPO-ACESSO.
           MOVE SQLCA                  TO GFCT0M-SQLCA-AREA.

           PERFORM 9990-OBTER-DESCRICAO-MENSAGEM.

           STRING GFCTG3-DESC-MSG ' - ' WRK-TABELA
           DELIMITED BY '  '           INTO WAO3-DESC-MSG-ERRO.

           PERFORM 9999-FINALIZAR-PROCESSAMENTO.

      *----------------------------------------------------------------*
       9900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ROTINA PARA CONSISTIR DADOS RECEBIDOS                         *
      ******************************************************************
      *----------------------------------------------------------------*
       9990-OBTER-DESCRICAO-MENSAGEM   SECTION.
      *----------------------------------------------------------------*

           MOVE +100                   TO GFCTG2-LL.
           MOVE ZEROS                  TO GFCTG2-ZZ.
           MOVE WAO2-TRANSACAO         TO GFCTG2-TRANSACAO.
           MOVE WAO2-FUNCAO            TO GFCTG2-FUNCAO.
           MOVE WAO2-FUNC-BDSCO        TO GFCTG2-FUNC-BDSCO.
           MOVE 'O'                    TO GFCTG2-TIPO-PROC.
           MOVE 'N'                    TO GFCTG2-FIM.

           MOVE SPACES                 TO GFCTG3-SAIDA.

           INITIALIZE                  GFCTG3-SAIDA.

           MOVE 'GFCT5523'             TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCTG2-ENTRADA
                                             GFCTG3-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 1                  TO WAO3-ERRO

               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MSG01-MODULO

               MOVE WRK-MSG01          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0982'         TO GFCT0M-TRANSACAO
               MOVE ZEROS              TO RETURN-CODE

               PERFORM 9999-FINALIZAR-PROCESSAMENTO
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 9
               MOVE GFCTG3-ERRO        TO WAO3-ERRO

               MOVE 'GFCT0982'         TO GFCT0M-TRANSACAO

               IF  GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2'
                   MOVE WRK-MODULO     TO GFCT0M-PROGRAMA
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
                   MOVE GFCTG3-DESC-MSG-ERRO(1:75)
                                       TO GFCT0M-TEXTO
               END-IF

               PERFORM 9999-FINALIZAR-PROCESSAMENTO
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 1
               MOVE 1                  TO WAO3-ERRO
               MOVE GFCTG3-COD-SQL-ERRO
                                       TO WAO3-COD-SQL-ERRO
               MOVE GFCTG3-COD-MSG-ERRO
                                       TO WAO3-COD-MSG-ERRO
               MOVE GFCTG3-DESC-MSG-ERRO
                                       TO WAO3-DESC-MSG-ERRO

               PERFORM 9999-FINALIZAR-PROCESSAMENTO
           END-IF.

      *----------------------------------------------------------------*
       9990-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ROTINA PARA FINALIZAR O PROCESSAMENTO                         *
      ******************************************************************
      *----------------------------------------------------------------*
       9999-FINALIZAR-PROCESSAMENTO    SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO RETURN-CODE.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
