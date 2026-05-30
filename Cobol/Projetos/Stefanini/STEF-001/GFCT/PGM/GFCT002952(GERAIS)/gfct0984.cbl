      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT0984.
       AUTHOR.     SONDAPROCWORK.

      *================================================================*
      *                   S O N D A   P R O C W O R K                  *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT0984                                    *
      *    ANALISTA....:   FRANCISCO LOURENCO         - SONDAPROCWORK  *
      *    DATA........:   NOV/2010                                    *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   GERAR DADOS PARA O DEMONSTRATIVO DE EVENTOS *
      *                    DO CLIENTE                                  *
      *                                                                *
      *----------------------------------------------------------------*
      *    BANCO DE DADOS:                                             *
      *     DB2                                                        *
      *      TABLE                                       INCLUDE/BOOK  *
      *        DB2PRD.TESTRN_COBRD_EXTRT                   GFCTB0N0    *
      *        DB2PRD.TMOVTO_COBRD_EXTRT                   GFCTB0N1    *
      *        DB2PRD.TASSOC_CJTO_NATUZ                    GFCTB0H2    *
      *        DB2PRD.PARM_SERVC_TARIF                     GFCTB0A2    *
      *        DB2PRD.SERVC_TARIF_PRINC                    GFCTB0D8    *
      *        DB2PRD.V01CTA_POUPANCA                      CLIEV007    *
      *        DB2PRD.V01CTA_CORRENTE                      CLIEV008    *
      *                                                                *
      *----------------------------------------------------------------*
      *    MODULOS CHAMADOS:                                           *
      *      GFCT5522 - VERIFICACAO DE DISPONIBILIDADE ON-LINE         *
      *      GFCT5523 - OBTER DESCRICAO DE MENSAGENS                   *
      *      SENH0315 - OBTER JUNCAO.                                  *
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
       01  FILLER                      PIC  X(050)         VALUE
           '*  INICIO DA WORKING GFCT0984  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*        ACUMULADORES          *'.
      *----------------------------------------------------------------*

       01  ACU-ACUMULADORES.
           05  ACU-DESPREZA            PIC  9(007) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*        INDEXADORES           *'.
      *----------------------------------------------------------------*

       01  IND-INDEXADORES.
           05  IND-1                   PIC  9(003) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*     VARIAVEIS AUXILIARES     *'.
      *----------------------------------------------------------------*

       01  WRK-FINALIZAR               PIC  X(001)         VALUE SPACES.

       01  WRK-FIM-CSR-GFCTB0N0        PIC  X(001)         VALUE SPACES.

       01  WRK-TEM-ASSOC-CONJ-SERVC    PIC  X(001)         VALUE SPACES.

       01  WRK-MODULO                  PIC  X(008)         VALUE SPACES.

       01  WRK-TABELA                  PIC  X(008)         VALUE SPACES.

       01  WRK-DESPREZA                PIC  9(007) COMP-3  VALUE ZEROS.

       01  WRK-WAOP-FLT-CONTA          PIC  9(013)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-WAOP-FLT-CONTA.
           05  FILLER                  PIC  9(006).
           05  WRK-COD-CONTA           PIC  9(007).

       01  WRK-WAOQ-PTR-OCOR-ATUAL     PIC  9(007) COMP-3  VALUE ZEROS.

       01  WRK-WAOQ-QTDE-TOT-REG       PIC  9(006) COMP-3  VALUE ZEROS.

       01  WRK-WAOQ-SEQ-TELA           PIC  9(004) COMP-3  VALUE ZEROS.

       01  WRK-DEFETV-DEB-MOVTO-INI    PIC  X(010)         VALUE
           '01.01.0001'.
       01  FILLER                      REDEFINES
                                               WRK-DEFETV-DEB-MOVTO-INI.
           05  WRK-DIA-EFETV-DEB-MOVTO-INI
                                       PIC  9(002).
           05  FILLER                  PIC  X(001).
           05  WRK-MES-EFETV-DEB-MOVTO-INI
                                       PIC  9(002).
           05  FILLER                  PIC  X(001).
           05  WRK-ANO-EFETV-DEB-MOVTO-INI
                                       PIC  9(004).

       01  WRK-DEFETV-DEB-MOVTO-FIM    PIC  X(010)         VALUE
           '01.01.0001'.
       01  FILLER                      REDEFINES
                                               WRK-DEFETV-DEB-MOVTO-FIM.
           05  WRK-DIA-EFETV-DEB-MOVTO-FIM
                                       PIC  9(002).
           05  FILLER                  PIC  X(001).
           05  WRK-MES-EFETV-DEB-MOVTO-FIM
                                       PIC  9(002).
           05  FILLER                  PIC  X(001).
           05  WRK-ANO-EFETV-DEB-MOVTO-FIM
                                       PIC  9(004).

       01  WRK-CSERVC-TARIF-INI        PIC S9(005) COMP-3  VALUE ZEROS.
       01  WRK-CSERVC-TARIF-FIM        PIC S9(005) COMP-3  VALUE ZEROS.

       01  WRK-CSEQ-ESTRN-EVNTO-INI    PIC S9(002) COMP-3  VALUE ZEROS.
       01  WRK-CSEQ-ESTRN-EVNTO-FIM    PIC S9(002) COMP-3  VALUE ZEROS.

       01  WRK-DATA-AUX.
           05  WRK-DIA-AUX             PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  WRK-MES-AUX             PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  WRK-ANO-AUX             PIC  9(004)         VALUE ZEROS.

       01  WRK-DATA-INI-INV.
           05  WRK-ANO-INI-INV         PIC  9(004)         VALUE ZEROS.
           05  WRK-MES-INI-INV         PIC  9(002)         VALUE ZEROS.
           05  WRK-DIA-INI-INV         PIC  9(002)         VALUE ZEROS.
       01  WRK-DATA-INI-INV-R          REDEFINES WRK-DATA-INI-INV
                                       PIC  9(008).

       01  WRK-DATA-FIM-INV.
           05  WRK-ANO-FIM-INV         PIC  9(004)         VALUE ZEROS.
           05  WRK-MES-FIM-INV         PIC  9(002)         VALUE ZEROS.
           05  WRK-DIA-FIM-INV         PIC  9(002)         VALUE ZEROS.
       01  WRK-DATA-FIM-INV-R          REDEFINES WRK-DATA-FIM-INV
                                       PIC  9(008).

       01  WRK-CSERVC-TARIF            PIC S9(005)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-CSERVC-TARIF.
           05  WRK-COD-TARIFA          PIC  9(005).

       01  WRK-QEVNTO-AGRUP            PIC S9(006)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-QEVNTO-AGRUP.
           05  WRK-QTD-EVENTO          PIC  9(006).

       01  WRK-VTARIF-CADTR-SIST       PIC S9(009)V99      VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-VTARIF-CADTR-SIST.
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
           05  WRK-ARQUIVO             PIC  9(002).

       01  WRK-CSEQ-MOVTO              PIC S9(011)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-CSEQ-MOVTO.
           05  WRK-SEQ-ARQUIVO         PIC  9(011).

       01  WRK-RSERVC-TARIF            PIC  X(100)         VALUE SPACES.
       01  FILLER                      REDEFINES WRK-RSERVC-TARIF.
           05  WRK-DESC-TARIFA         PIC  X(030).
           05  FILLER                  PIC  X(070).

       01  WRK-SQLCODE                 PIC S9(009)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-SQLCODE.
           05  FILLER                  PIC  X(006).
           05  WRK-COD-SQL             PIC  9(003).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE MENSAGEM ***'.
      *----------------------------------------------------------------*

       01  WRK-MSG01.
           05  FILLER                  PIC  X(026)         VALUE
               'ERRO NA CHAMADA DO MODULO'.
           05  WRK-MSG01-MODULO        PIC  X(008)         VALUE SPACES.

       01  WRK-MSG02                   PIC  X(075)         VALUE
           'SISTEMA INDISPONIVEL'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
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

       01  WRK-CNPJ-BANCO-0315.
           05  WRK-CNPJ-PRI-0315       PIC  9(009)         VALUE ZEROS.
           05  WRK-CNPJ-FIL-0315       PIC  9(005)         VALUE ZEROS.
           05  WRK-CNPJ-CON-0315       PIC  9(002)         VALUE ZEROS.

       01  WRK-COD-JUNCAO-0315         PIC  9(005)         VALUE ZEROS.

       01  WRK-NOME-DEPTO-0315         PIC  X(040)         VALUE SPACES.

       01  WRK-SECAO-0315              PIC  9(003)         VALUE ZEROS.

       01  WRK-STATUS-0315             PIC  X(001)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*   AREA DA BRAD1470           *'.
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
           '*   AREA PARA MODULO GFCT5523  *'.
      *----------------------------------------------------------------*

       COPY I#GFCTG2.

       COPY I#GFCTG3.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*      AREA DE TABELAS DB2     *'.
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
                INCLUDE GFCTB0H2
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0A2
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0D8
           END-EXEC.

           EXEC SQL
                INCLUDE CLIEV007
           END-EXEC.

           EXEC SQL
                INCLUDE CLIEV008
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
           '*   FIM DA WORKING GFCT0984    *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY GFCTWAOP.

       COPY GFCTWAOQ.

       COPY I#GFCT0M.

      *================================================================*
       PROCEDURE DIVISION              USING WAOP-ENTRADA
                                             WAOQ-SAIDA
                                             GFCT0M-AREA-ERROS.
      *================================================================*

      ******************************************************************
      * ROTINA PARA CONTROLAR O PROCESSAMENTO                          *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-CONTROLAR-PROCESSAMENTO    SECTION.
      *----------------------------------------------------------------*

           PERFORM 0100-INICIALIZAR-PROCESSAMENTO.

           PERFORM 2000-EXECUTAR-PROCESSAMENTO.

           PERFORM 9999-FINALIZAR-PROCESSAMENTO.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ROTINA PARA INICIALIZAR O PROCESSAMENTO                       *
      ******************************************************************
      *----------------------------------------------------------------*
       0100-INICIALIZAR-PROCESSAMENTO  SECTION.
      *----------------------------------------------------------------*

           PERFORM 1100-VERIFICAR-ONLINE.

           PERFORM 1200-INICIALIZAR-AREA-SAIDA.

           PERFORM 1300-CONSISTIR-DADOS.

           PERFORM 1400-CONSISTE-AGENCIA-CONTA.

           IF  WAOP-DEPTO-BDSCO        EQUAL ZEROS
               PERFORM 1500-OBTER-CODIGO-JUNCAO
           END-IF.

      *----------------------------------------------------------------*
       0100-99-FIM.                    EXIT.
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
           MOVE WAOP-TRANSACAO         TO GFCTFZ-TRANSACAO.
           MOVE WAOP-FUNCAO            TO GFCTFZ-FUNCAO.
           MOVE WAOP-FUNC-BDSCO        TO GFCTFZ-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTFZ-FIM.

           MOVE 'GFCT5522'             TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCTFZ-ENTRADA
                                             GFCTG1-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE ZEROS              TO RETURN-CODE
               MOVE SPACES             TO WAOQ-DESC-MSG-ERRO
               MOVE 9                  TO WAOQ-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MSG01-MODULO
               MOVE WRK-MSG01          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0982'         TO GFCT0M-TRANSACAO

               PERFORM 9999-FINALIZAR-PROCESSAMENTO
           END-IF.

           IF  GFCTG1-ERRO             NOT EQUAL ZEROS
               MOVE 9                  TO WAOQ-ERRO

               IF  GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2'
                   MOVE SPACES         TO WAOQ-DESC-MSG-ERRO
                   MOVE GFCTG1-DESC-MSG-ERRO(1:75)
                                       TO GFCT0M-TEXTO
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
               ELSE
                   MOVE GFCTG1-COD-SQL-ERRO
                                       TO WAOQ-COD-SQL-ERRO
                   MOVE GFCTG1-DESC-MSG-ERRO
                                       TO WAOQ-DESC-MSG-ERRO
               END-IF

               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
               MOVE 'GFCT0982'         TO GFCT0M-TRANSACAO

               PERFORM 9999-FINALIZAR-PROCESSAMENTO
           END-IF.

           IF  GFCTG1-CSIT-ONLINE      EQUAL 1
               MOVE 9                  TO WAOQ-ERRO
               MOVE SPACES             TO WAOQ-DESC-MSG-ERRO
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

           MOVE SPACES                 TO WAOQ-SAIDA.

           INITIALIZE                  WAOQ-SAIDA.

           MOVE +235                   TO WAOQ-LL.
           MOVE WAOP-ZZ                TO WAOQ-ZZ.
           MOVE WAOP-TRANSACAO         TO WAOQ-TRANSACAO.
           MOVE WAOP-FUNCAO            TO WAOQ-FUNCAO.
           MOVE WAOP-FILTRO            TO WAOQ-FILTRO.
           MOVE WAOP-PONTEIRO          TO WAOQ-PONTEIRO.
           MOVE WAOP-QTDE-OCOR         TO WAOQ-QTDE-OCOR.
           MOVE WAOP-QTDE-TOT-REG      TO WAOQ-QTDE-TOT-REG.
           MOVE WAOP-DEPTO-BDSCO       TO WAOQ-DEPTO-BDSCO.
           MOVE WAOP-FUNC-BDSCO        TO WAOQ-FUNC-BDSCO.

           MOVE 'N'                    TO WAOQ-FIM.

           MOVE ZEROS                  TO WAOQ-ERRO,
                                          WAOQ-COD-SQL-ERRO,
                                          WAOQ-COD-MSG-ERRO.

           MOVE '0000'                 TO GFCTG2-COD-MSG.

           PERFORM 9990-OBTER-DESCRICAO-MENSAGEM.

           MOVE GFCTG3-DESC-MSG        TO WAOQ-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ROTINA PARA CONSISTIR DADOS RECEBIDOS                         *
      ******************************************************************
      *----------------------------------------------------------------*
       1300-CONSISTIR-DADOS            SECTION.
      *----------------------------------------------------------------*

           STRING WAOP-FLT-PERIODO-INI(7:4)
                  WAOP-FLT-PERIODO-INI(4:2)
                  WAOP-FLT-PERIODO-INI(1:2)
           DELIMITED BY SIZE           INTO WRK-DATA-INI-INV.

           STRING WAOP-FLT-PERIODO-FIM(7:4)
                  WAOP-FLT-PERIODO-FIM(4:2)
                  WAOP-FLT-PERIODO-FIM(1:2)
           DELIMITED BY SIZE           INTO WRK-DATA-FIM-INV.

           IF (WAOP-TRANSACAO          EQUAL SPACES OR LOW-VALUES) OR
              (WAOP-FUNCAO             EQUAL SPACES OR LOW-VALUES) OR
              (WAOP-FLT-AGENCIA        NOT NUMERIC)                OR
              (WAOP-FLT-AGENCIA        EQUAL ZEROS)                OR
              (WAOP-FLT-CONTA          NOT NUMERIC)                OR
              (WAOP-FLT-CONTA          EQUAL ZEROS)                OR
              (WRK-DATA-INI-INV-R      NOT NUMERIC)                OR
              (WRK-DATA-FIM-INV-R      NOT NUMERIC)                OR
              (WAOP-FLT-COD-CJTO-SERVC NOT NUMERIC)                OR
              (WAOP-FLT-COD-TARIFA     NOT NUMERIC)                OR
              (WAOP-PTR-OCOR-ATUAL     NOT NUMERIC)                OR
              (WAOP-PTR-CONTINUA       NOT EQUAL 'S' AND 'N')      OR
              (WAOP-QTDE-OCOR          NOT NUMERIC)                OR
              (WAOP-QTDE-TOT-REG       NOT NUMERIC)                OR
              (WAOP-DEPTO-BDSCO        NOT NUMERIC)                OR
              (WAOP-FUNC-BDSCO         EQUAL SPACES OR LOW-VALUES) OR
              (WAOP-FIM                NOT EQUAL 'S' AND 'N')
               MOVE 'S'                TO WAOQ-FIM

               MOVE 1                  TO WAOQ-ERRO
               MOVE ZEROS              TO WAOQ-COD-SQL-ERRO
               MOVE 0001               TO WAOQ-COD-MSG-ERRO

               MOVE '0001'             TO GFCTG2-COD-MSG

               PERFORM 9990-OBTER-DESCRICAO-MENSAGEM

               MOVE GFCTG3-DESC-MSG    TO WAOQ-DESC-MSG-ERRO

               PERFORM 9999-FINALIZAR-PROCESSAMENTO
           END-IF.

           MOVE WAOP-FLT-PERIODO-INI   TO WRK-DATA-AUX.
           MOVE WRK-DIA-AUX            TO WRK-1470-DIA.
           MOVE WRK-MES-AUX            TO WRK-1470-MES.
           MOVE WRK-ANO-AUX            TO WRK-1470-ANO.

           PERFORM 1310-CONSISTIR-DATA.

           MOVE WAOP-FLT-PERIODO-FIM   TO WRK-DATA-AUX.
           MOVE WRK-DIA-AUX            TO WRK-1470-DIA.
           MOVE WRK-MES-AUX            TO WRK-1470-MES.
           MOVE WRK-ANO-AUX            TO WRK-1470-ANO.

           PERFORM 1310-CONSISTIR-DATA.

           IF  WRK-DATA-FIM-INV        LESS WRK-DATA-INI-INV
               MOVE 'S'                TO WAOQ-FIM

               MOVE 1                  TO WAOQ-ERRO
               MOVE ZEROS              TO WAOQ-COD-SQL-ERRO
               MOVE 1283               TO WAOQ-COD-MSG-ERRO
               MOVE '1283'             TO GFCTG2-COD-MSG

               PERFORM 9990-OBTER-DESCRICAO-MENSAGEM

               MOVE GFCTG3-DESC-MSG    TO WAOQ-DESC-MSG-ERRO

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
               MOVE 'S'                TO WAOQ-FIM

               MOVE 1                  TO WAOQ-ERRO
               MOVE ZEROS              TO WAOQ-COD-SQL-ERRO
               MOVE 1282               TO WAOQ-COD-MSG-ERRO

               MOVE '1282'             TO GFCTG2-COD-MSG

               PERFORM 9990-OBTER-DESCRICAO-MENSAGEM

               MOVE GFCTG3-DESC-MSG    TO WAOQ-DESC-MSG-ERRO
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
                   MOVE 'S'            TO WAOQ-FIM

                   MOVE 1              TO WAOQ-ERRO
                   MOVE 0220           TO WAOQ-COD-MSG-ERRO
                   MOVE ZEROS          TO WAOQ-COD-SQL-ERRO

                   MOVE '0220'         TO GFCTG2-COD-MSG

                   PERFORM 9990-OBTER-DESCRICAO-MENSAGEM

                   MOVE GFCTG3-DESC-MSG
                                       TO WAOQ-DESC-MSG-ERRO

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

           MOVE WAOP-FLT-AGENCIA       TO CJUNC-DEPDC OF CLIEV008.

           MOVE WAOP-FLT-CONTA         TO WRK-WAOP-FLT-CONTA.
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

               MOVE 0012               TO WAOQ-COD-MSG-ERRO
               MOVE WRK-COD-SQL        TO WAOQ-COD-SQL-ERRO

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

           MOVE WAOP-FLT-AGENCIA       TO CJUNC-DEPDC OF CLIEV007.

           MOVE WAOP-FLT-CONTA         TO WRK-WAOP-FLT-CONTA.
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

               MOVE 0012               TO WAOQ-COD-MSG-ERRO
               MOVE WRK-COD-SQL        TO WAOQ-COD-SQL-ERRO

               MOVE '0012'             TO GFCTG2-COD-MSG

               MOVE 'CLIEV007'         TO WRK-TABELA

               PERFORM 9900-MOVER-ERRO-SQL
           END-IF.

      *----------------------------------------------------------------*
       1420-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ROTINA PARA OBTER CODIGO DE JUNCAO                            *
      ******************************************************************
      *----------------------------------------------------------------*
       1500-OBTER-CODIGO-JUNCAO        SECTION.
      *----------------------------------------------------------------*

           MOVE WAOP-FUNC-BDSCO        TO WRK-COD-FUNCIONAL-0315.

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
               MOVE 'S'                TO WAOQ-FIM

               MOVE 1                  TO WAOQ-ERRO
               MOVE 0056               TO WAOQ-COD-MSG-ERRO
               MOVE ZEROS              TO WAOQ-COD-SQL-ERRO

               MOVE '0056'             TO GFCTG2-COD-MSG

               PERFORM 9990-OBTER-DESCRICAO-MENSAGEM

               MOVE GFCTG3-DESC-MSG    TO WAOQ-DESC-MSG-ERRO

               PERFORM 9999-FINALIZAR-PROCESSAMENTO
           END-IF.

           IF  RETURN-CODE             EQUAL 04
               IF  WRK-COD-MENSAGEM-0315
                                       EQUAL 001
                   MOVE 'S'            TO WAOQ-FIM

                   MOVE 1              TO WAOQ-ERRO
                   MOVE 0057           TO WAOQ-COD-MSG-ERRO
                   MOVE ZEROS          TO WAOQ-COD-SQL-ERRO

                   MOVE '0057'         TO GFCTG2-COD-MSG

                   PERFORM 9990-OBTER-DESCRICAO-MENSAGEM

                   MOVE GFCTG3-DESC-MSG
                                       TO WAOQ-DESC-MSG-ERRO

                   PERFORM 9999-FINALIZAR-PROCESSAMENTO
               END-IF

               IF  WRK-COD-MENSAGEM-0315
                                       EQUAL 144
                   MOVE 'S'            TO WAOQ-FIM

                   MOVE 1              TO WAOQ-ERRO
                   MOVE 0058           TO WAOQ-COD-MSG-ERRO
                   MOVE ZEROS          TO WAOQ-COD-SQL-ERRO

                   MOVE '0058'         TO GFCTG2-COD-MSG

                   PERFORM 9990-OBTER-DESCRICAO-MENSAGEM

                   MOVE GFCTG3-DESC-MSG
                                       TO WAOQ-DESC-MSG-ERRO

                   PERFORM 9999-FINALIZAR-PROCESSAMENTO
               END-IF

               IF  WRK-COD-MENSAGEM-0315
                                       EQUAL 242
                   MOVE 'S'            TO WAOQ-FIM

                   MOVE 1              TO WAOQ-ERRO
                   MOVE 0059           TO WAOQ-COD-MSG-ERRO
                   MOVE ZEROS          TO WAOQ-COD-SQL-ERRO

                   MOVE '0059'         TO GFCTG2-COD-MSG

                   PERFORM 9990-OBTER-DESCRICAO-MENSAGEM

                   MOVE GFCTG3-DESC-MSG
                                       TO WAOQ-DESC-MSG-ERRO

                   PERFORM 9999-FINALIZAR-PROCESSAMENTO
               END-IF
           END-IF.

           IF  RETURN-CODE             EQUAL ZEROS
               IF  WRK-COD-MENSAGEM-0315
                                       EQUAL 138
                   MOVE 'S'            TO WAOQ-FIM

                   MOVE 1              TO WAOQ-ERRO
                   MOVE 0060           TO WAOQ-COD-MSG-ERRO
                   MOVE ZEROS          TO WAOQ-COD-SQL-ERRO

                   MOVE '0060'         TO GFCTG2-COD-MSG

                   PERFORM 9990-OBTER-DESCRICAO-MENSAGEM

                   MOVE GFCTG3-DESC-MSG
                                       TO WAOQ-DESC-MSG-ERRO

                   PERFORM 9999-FINALIZAR-PROCESSAMENTO
               END-IF
           END-IF.

           MOVE WRK-COD-JUNCAO-0315    TO WAOQ-DEPTO-BDSCO.

      *----------------------------------------------------------------*
       1500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ROTINA PARA EXECUTAR O PROCESSAMENTO                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-EXECUTAR-PROCESSAMENTO     SECTION.
      *----------------------------------------------------------------*

           MOVE 237                    TO CBCO-DSTNO-MOVTO OF GFCTB0N1.
           MOVE WAOP-FLT-AGENCIA       TO CAG-DSTNO-MOVTO  OF GFCTB0N1.
           MOVE WAOP-FLT-CONTA         TO CCTA-DSTNO-MOVTO OF GFCTB0N1.

           MOVE WAOP-FLT-PERIODO-INI   TO WRK-DATA-AUX.
           MOVE WRK-DIA-AUX            TO WRK-DIA-EFETV-DEB-MOVTO-INI.
           MOVE WRK-MES-AUX            TO WRK-MES-EFETV-DEB-MOVTO-INI.
           MOVE WRK-ANO-AUX            TO WRK-ANO-EFETV-DEB-MOVTO-INI.

           MOVE WAOP-FLT-PERIODO-FIM   TO WRK-DATA-AUX.
           MOVE WRK-DIA-AUX            TO WRK-DIA-EFETV-DEB-MOVTO-FIM.
           MOVE WRK-MES-AUX            TO WRK-MES-EFETV-DEB-MOVTO-FIM.
           MOVE WRK-ANO-AUX            TO WRK-ANO-EFETV-DEB-MOVTO-FIM.

           IF  WAOP-FLT-COD-TARIFA     EQUAL ZEROS
               MOVE ZEROS              TO WRK-CSERVC-TARIF-INI
               MOVE 99999              TO WRK-CSERVC-TARIF-FIM
           ELSE
               MOVE WAOP-FLT-COD-TARIFA
                                       TO WRK-CSERVC-TARIF-INI
                                          WRK-CSERVC-TARIF-FIM
           END-IF.

           MOVE 'N'                    TO WRK-FINALIZAR.

           PERFORM 2200-ABRIR-CURSOR-GFCTB0N1.

           MOVE WAOQ-PTR-OCOR-ATUAL    TO WRK-WAOQ-PTR-OCOR-ATUAL,
                                          WRK-DESPREZA.

           MOVE WAOQ-QTDE-TOT-REG      TO WRK-WAOQ-QTDE-TOT-REG.

           MOVE ZEROS                  TO WRK-WAOQ-SEQ-TELA.

           PERFORM 2300-LER-CURSOR-GFCTB0N1.

           PERFORM 2300-LER-CURSOR-GFCTB0N1
           VARYING ACU-DESPREZA        FROM 1 BY 1
             UNTIL ACU-DESPREZA        GREATER WRK-DESPREZA
                OR WRK-FINALIZAR       EQUAL 'S'.

           IF  WRK-FINALIZAR           EQUAL 'S'
               IF  WAOQ-PTR-OCOR-ATUAL EQUAL ZEROS
                   MOVE 'S'            TO WAOQ-FIM

                   MOVE 1              TO WAOQ-ERRO
                   MOVE ZEROS          TO WAOQ-COD-SQL-ERRO

                   MOVE 1286           TO WAOQ-COD-MSG-ERRO

                   MOVE '1286'         TO GFCTG2-COD-MSG

                   PERFORM 9990-OBTER-DESCRICAO-MENSAGEM

                   MOVE GFCTG3-DESC-MSG
                                       TO WAOQ-DESC-MSG-ERRO
               END-IF
           END-IF.

           MOVE ZEROS                  TO IND-1.

           PERFORM 2100-TRATAR-OCORRENCIA-SAIDA
             UNTIL WRK-FINALIZAR       EQUAL 'S'.

           PERFORM 2400-FECHAR-CURSOR-GFCTB0N1.

           MOVE WRK-WAOQ-PTR-OCOR-ATUAL
                                       TO WAOQ-PTR-OCOR-ATUAL.

           MOVE WRK-WAOQ-QTDE-TOT-REG  TO WAOQ-QTDE-TOT-REG.

           IF  WAOQ-PTR-CONTINUA       EQUAL 'N' AND
               WAOQ-PTR-OCOR-ATUAL     NOT EQUAL ZEROS
               MOVE 'S'                TO WAOQ-FIM

               MOVE 1                  TO WAOQ-ERRO
               MOVE ZEROS              TO WAOQ-COD-SQL-ERRO

               MOVE 0007               TO WAOQ-COD-MSG-ERRO

               MOVE '0007'             TO GFCTG2-COD-MSG

               PERFORM 9990-OBTER-DESCRICAO-MENSAGEM

               MOVE GFCTG3-DESC-MSG    TO WAOQ-DESC-MSG-ERRO
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ROTINA PARA TRATAR A MONTAGEM DAS OCORRENCIAS DA TELA         *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-TRATAR-OCORRENCIA-SAIDA    SECTION.
      *----------------------------------------------------------------*

           ADD  1                      TO WRK-WAOQ-PTR-OCOR-ATUAL.

           IF  WAOP-FLT-COD-CJTO-SERVC EQUAL ZEROS
               PERFORM 2130-MONTAR-OCORRENCIA-SAIDA

               PERFORM 2300-LER-CURSOR-GFCTB0N1
           ELSE
               PERFORM 2120-VALIDAR-ASSOC-CONJ-SERV

               IF  WRK-TEM-ASSOC-CONJ-SERVC
                                       EQUAL 'S'
                   PERFORM 2130-MONTAR-OCORRENCIA-SAIDA
               END-IF

               MOVE 'N'                TO WRK-TEM-ASSOC-CONJ-SERVC

               PERFORM 2110-OBTER-PROXIMO-EVENTO
                 UNTIL WRK-FINALIZAR   EQUAL 'S'
                    OR WRK-TEM-ASSOC-CONJ-SERVC
                                       EQUAL 'S'
           END-IF.

           IF  IND-1                   EQUAL 5
               MOVE 'S'                TO WAOQ-FIM

               IF  WRK-FINALIZAR       EQUAL 'N'
                   MOVE 'S'            TO WAOQ-PTR-CONTINUA
               END-IF

               MOVE 'S'                TO WRK-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ROTINA PARA OBTER O PROXIMO EVENTO COM ASSOCIACAO VALIDA      *
      ******************************************************************
      *----------------------------------------------------------------*
       2110-OBTER-PROXIMO-EVENTO       SECTION.
      *----------------------------------------------------------------*

           PERFORM 2300-LER-CURSOR-GFCTB0N1.

           PERFORM 2120-VALIDAR-ASSOC-CONJ-SERV.

           IF  WRK-TEM-ASSOC-CONJ-SERVC
                                       EQUAL 'N'
               ADD  1                  TO WRK-WAOQ-PTR-OCOR-ATUAL
           END-IF.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ROTINA PARA VALIDAR ASSOCIACAO CONJUNTO SERVICO               *
      ******************************************************************
      *----------------------------------------------------------------*
       2120-VALIDAR-ASSOC-CONJ-SERV    SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF OF GFCTB0N1
                                       TO CSERVC-TARIF OF GFCTB0A2.

           MOVE WAOP-FLT-COD-CJTO-SERVC
                                       TO CCJTO-SERVC  OF GFCTB0H2.

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
               PERFORM 2400-FECHAR-CURSOR-GFCTB0N1

               MOVE SPACES             TO GFCT0M-ERRO-SQL

               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'PARM_SERVC_TARIF' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0003'             TO GFCT0M-LOCAL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-SQLCODE

               MOVE 0011               TO WAOQ-COD-MSG-ERRO
               MOVE WRK-COD-SQL        TO WAOQ-COD-SQL-ERRO

               MOVE '0011'             TO GFCTG2-COD-MSG

               MOVE 'GFCTB0A2'         TO WRK-TABELA

               PERFORM 9900-MOVER-ERRO-SQL
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'N'                TO WRK-TEM-ASSOC-CONJ-SERVC
           ELSE
               MOVE 'S'                TO WRK-TEM-ASSOC-CONJ-SERVC
           END-IF.

      *----------------------------------------------------------------*
       2120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ROTINA PARA MONTAR DADOS OCORRENCIA DE SAIDA                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2130-MONTAR-OCORRENCIA-SAIDA    SECTION.
      *----------------------------------------------------------------*

           ADD +155                    TO WAOQ-LL.

           ADD  1                      TO WRK-WAOQ-QTDE-TOT-REG.

           ADD  1                      TO IND-1.

           MOVE 'S'                    TO WAOQ-SEL(IND-1)

           MOVE DEFETV-DEB-MOVTO OF GFCTB0N1
                                       TO WAOQ-DT-EVENTO(IND-1).

           MOVE CSERVC-TARIF OF GFCTB0N1
                                       TO WRK-CSERVC-TARIF.
           MOVE WRK-COD-TARIFA         TO WAOQ-CD-TARIFA(IND-1).

           PERFORM 2131-OBTER-DESCRICAO-TARIFA.

           MOVE 'NAO'                  TO WAOQ-DEB-PARCIAL(IND-1)

           MOVE RSERVC-TARIF-REDZD OF GFCTB0D8
                                       TO WRK-RSERVC-TARIF
           MOVE WRK-DESC-TARIFA        TO WAOQ-NOME-TARIFA(IND-1).

           MOVE QEVNTO-AGRUP OF GFCTB0N1
                                       TO WRK-QEVNTO-AGRUP.
           MOVE WRK-QTD-EVENTO         TO WAOQ-QT-EVENTOS(IND-1).

           MOVE VTARIF-CADTR-SIST OF GFCTB0N1
                                       TO WRK-VTARIF-CADTR-SIST.
           MOVE WRK-VLR-POTENCIAL      TO WAOQ-VL-POTENCIAL(IND-1).

           MOVE VTARIF-DEB-MOVTO OF GFCTB0N1
                                       TO WRK-VTARIF-DEB-MOVTO.
           MOVE WRK-VLR-DEBITO         TO WAOQ-VL-CALCULADO(IND-1),
                                          WAOQ-VL-RECEBIDO(IND-1).

           MOVE ZEROS                  TO WRK-VRTEIO-VLR-SOLTD.

           IF  CSIT-EVNTO-RECBD OF GFCTB0N1
                                       EQUAL 06 OR
               CSIT-EVNTO-RECBD OF GFCTB0N1
                                       EQUAL 07
               PERFORM 2140-TRATAR-VALOR-ESTORNO
           END-IF.

           MOVE WRK-VLR-ESTORNO        TO WAOQ-VL-ESTORNADO(IND-1)

           MOVE ZEROS                  TO WAOQ-VL-RECEBER(IND-1),
                                          WAOQ-VL-PERDA(IND-1).

           MOVE CROTNA-ORIGE-MOVTO OF GFCTB0N1
                                       TO WAOQ-ROTINA(IND-1).
           MOVE DENVIO-MOVTO-TARIF OF GFCTB0N1
                                       TO WAOQ-DT-MOVIMENTO(IND-1).

           MOVE CNRO-ARQ-MOVTO OF GFCTB0N1
                                       TO WRK-CNRO-ARQ-MOVTO.
           MOVE WRK-ARQUIVO            TO WAOQ-ARQUIVO(IND-1).

           MOVE CSEQ-MOVTO OF GFCTB0N1 TO WRK-CSEQ-MOVTO.
           MOVE WRK-SEQ-ARQUIVO        TO WAOQ-SEQ-ARQUIVO(IND-1).

           ADD  1                      TO WRK-WAOQ-SEQ-TELA.
           MOVE WRK-WAOQ-SEQ-TELA      TO WAOQ-SEQ-TELA(IND-1).

      *----------------------------------------------------------------*
       2130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ROTINA PARA OBTER DESCRICAO DA TARIFA                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2131-OBTER-DESCRICAO-TARIFA     SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF OF GFCTB0N1
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
               PERFORM 2400-FECHAR-CURSOR-GFCTB0N1

               MOVE SPACES             TO GFCT0M-ERRO-SQL

               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'SERVC_TARIF_PRINC'
                                       TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0004'             TO GFCT0M-LOCAL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-SQLCODE

               MOVE 0012               TO WAOQ-COD-MSG-ERRO
               MOVE WRK-COD-SQL        TO WAOQ-COD-SQL-ERRO

               MOVE '0012'             TO GFCTG2-COD-MSG

               MOVE 'GFCTB0D8'         TO WRK-TABELA

               PERFORM 9900-MOVER-ERRO-SQL
           END-IF.

      *----------------------------------------------------------------*
       2131-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ROTINA PARA TRATAR O VALOR DE ESTORNO COBRADO NO EXTRATO      *
      ******************************************************************
      *----------------------------------------------------------------*
       2140-TRATAR-VALOR-ESTORNO       SECTION.
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

           PERFORM 2142-ABRIR-CSR-GFCTB0N0.

           PERFORM 2143-LER-CSR-GFCTB0N0.

           PERFORM 2141-ACUMULAR-VALOR-ESTORNO
             UNTIL WRK-FIM-CSR-GFCTB0N0
                                       EQUAL 'S'.

           PERFORM 2144-FECHAR-CSR-GFCTB0N0.

           MOVE WRK-VRTEIO-VLR-SOLTD-COMP-3
                                       TO WRK-VRTEIO-VLR-SOLTD.

      *----------------------------------------------------------------*
       2140-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ROTINA PARA ACUMULAR O VALOR DE ESTORNO COBRADO NO EXTRATO    *
      ******************************************************************
      *----------------------------------------------------------------*
       2141-ACUMULAR-VALOR-ESTORNO          SECTION.
      *----------------------------------------------------------------*

           ADD  VRTEIO-VLR-SOLTD OF GFCTB0N0
                                       TO WRK-VRTEIO-VLR-SOLTD-COMP-3.

           PERFORM 2143-LER-CSR-GFCTB0N0.

      *----------------------------------------------------------------*
       2141-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ROTINA PARA ABRIR O CURSOR                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2142-ABRIR-CSR-GFCTB0N0              SECTION.
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
               MOVE '0005'             TO GFCT0M-LOCAL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-SQLCODE

               MOVE 0005               TO WAOQ-COD-MSG-ERRO
               MOVE WRK-COD-SQL        TO WAOQ-COD-SQL-ERRO

               MOVE '0005'             TO GFCTG2-COD-MSG

               MOVE 'GFCTB0N0'         TO WRK-TABELA

               PERFORM 9900-MOVER-ERRO-SQL
           END-IF.

           MOVE ZEROS                  TO WRK-DESPREZA.

      *----------------------------------------------------------------*
       2142-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ROTINA PARA LER O CURSOR                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2143-LER-CSR-GFCTB0N0          SECTION.
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
               MOVE '0006'             TO GFCT0M-LOCAL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-SQLCODE

               MOVE 0006               TO WAOQ-COD-MSG-ERRO
               MOVE WRK-COD-SQL        TO WAOQ-COD-SQL-ERRO

               MOVE '0006'             TO GFCTG2-COD-MSG

               MOVE 'GFCTB0N0'         TO WRK-TABELA

               PERFORM 9900-MOVER-ERRO-SQL
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-CSR-GFCTB0N0
           END-IF.

      *----------------------------------------------------------------*
       2143-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ROTINA PARA FECHAR O CURSOR                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2144-FECHAR-CSR-GFCTB0N0       SECTION.
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
               MOVE '0007'             TO GFCT0M-LOCAL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-SQLCODE

               MOVE 0011               TO WAOQ-COD-MSG-ERRO
               MOVE WRK-COD-SQL        TO WAOQ-COD-SQL-ERRO

               MOVE '0011'             TO GFCTG2-COD-MSG

               MOVE 'GFCTB0N0'         TO WRK-TABELA

               PERFORM 9900-MOVER-ERRO-SQL
           END-IF.

      *----------------------------------------------------------------*
       2144-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ROTINA PARA ABRIR O CURSOR                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-ABRIR-CURSOR-GFCTB0N1      SECTION.
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
               MOVE '0008'             TO GFCT0M-LOCAL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-SQLCODE

               MOVE 0005               TO WAOQ-COD-MSG-ERRO
               MOVE WRK-COD-SQL        TO WAOQ-COD-SQL-ERRO

               MOVE '0005'             TO GFCTG2-COD-MSG

               MOVE 'GFCTB0N1'         TO WRK-TABELA

               PERFORM 9900-MOVER-ERRO-SQL
           END-IF.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ROTINA PARA LER O CURSOR                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2300-LER-CURSOR-GFCTB0N1        SECTION.
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
               PERFORM 2400-FECHAR-CURSOR-GFCTB0N1

               MOVE SPACES             TO GFCT0M-ERRO-SQL

               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'TMOVTO_COBRD_EXTRT'
                                       TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE '0009'             TO GFCT0M-LOCAL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-SQLCODE

               MOVE 0006               TO WAOQ-COD-MSG-ERRO
               MOVE WRK-COD-SQL        TO WAOQ-COD-SQL-ERRO

               MOVE '0006'             TO GFCTG2-COD-MSG

               MOVE 'GFCTB0N1'         TO WRK-TABELA

               PERFORM 9900-MOVER-ERRO-SQL
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WAOQ-FIM,
                                          WRK-FINALIZAR

               IF  IND-1               EQUAL ZEROS
                   IF  WAOQ-PTR-OCOR-ATUAL
                                       EQUAL ZEROS
                       MOVE 1          TO WAOQ-ERRO
                       MOVE ZEROS      TO WAOQ-COD-SQL-ERRO

                       MOVE 1286       TO WAOQ-COD-MSG-ERRO

                       MOVE '1286'     TO GFCTG2-COD-MSG

                       PERFORM 9990-OBTER-DESCRICAO-MENSAGEM

                       MOVE GFCTG3-DESC-MSG
                                       TO WAOQ-DESC-MSG-ERRO
                   END-IF
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ROTINA PARA FECHAR O CURSOR                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2400-FECHAR-CURSOR-GFCTB0N1     SECTION.
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
               MOVE '0010'             TO GFCT0M-LOCAL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-SQLCODE

               MOVE 0011               TO WAOQ-COD-MSG-ERRO
               MOVE WRK-COD-SQL        TO WAOQ-COD-SQL-ERRO

               MOVE '0011'             TO GFCTG2-COD-MSG

               MOVE 'GFCTB0N1'         TO WRK-TABELA

               PERFORM 9900-MOVER-ERRO-SQL
           END-IF.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ROTINA PARA MONTAR OS DADOS DE ERROS NO ACESSO A TABELA       *
      ******************************************************************
      *----------------------------------------------------------------*
       9900-MOVER-ERRO-SQL            SECTION.
      *----------------------------------------------------------------*

           MOVE 'S'                    TO WAOQ-FIM.

           MOVE 9                      TO WAOQ-ERRO.
           MOVE 'GFCT0984'             TO GFCT0M-PROGRAMA.
           MOVE 'DB2'                  TO GFCT0M-TIPO-ACESSO.
           MOVE SQLCA                  TO GFCT0M-SQLCA-AREA.

           PERFORM 9990-OBTER-DESCRICAO-MENSAGEM.

           STRING GFCTG3-DESC-MSG ' - ' WRK-TABELA
           DELIMITED BY '  '           INTO WAOQ-DESC-MSG-ERRO.

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
           MOVE WAOP-TRANSACAO         TO GFCTG2-TRANSACAO.
           MOVE WAOP-FUNCAO            TO GFCTG2-FUNCAO.
           MOVE WAOP-FUNC-BDSCO        TO GFCTG2-FUNC-BDSCO.
           MOVE 'O'                    TO GFCTG2-TIPO-PROC.
           MOVE 'N'                    TO GFCTG2-FIM.

           MOVE SPACES                 TO GFCTG3-SAIDA.

           INITIALIZE                  GFCTG3-SAIDA.

           MOVE 'GFCT5523'             TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCTG2-ENTRADA
                                             GFCTG3-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 1                  TO WAOQ-ERRO

               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MSG01-MODULO

               MOVE WRK-MSG01          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0984'         TO GFCT0M-TRANSACAO
               MOVE ZEROS              TO RETURN-CODE

               PERFORM 9999-FINALIZAR-PROCESSAMENTO
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 9
               MOVE GFCTG3-ERRO        TO WAOQ-ERRO

               MOVE 'GFCT0984'         TO GFCT0M-TRANSACAO

               IF  GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2'
                   MOVE WRK-MODULO     TO GFCT0M-PROGRAMA
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
                   MOVE GFCTG3-DESC-MSG-ERRO(1:75)
                                       TO GFCT0M-TEXTO
               END-IF

               PERFORM 9999-FINALIZAR-PROCESSAMENTO
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 1
               MOVE 1                  TO WAOQ-ERRO
               MOVE GFCTG3-COD-SQL-ERRO
                                       TO WAOQ-COD-SQL-ERRO
               MOVE GFCTG3-COD-MSG-ERRO
                                       TO WAOQ-COD-MSG-ERRO
               MOVE GFCTG3-DESC-MSG-ERRO
                                       TO WAOQ-DESC-MSG-ERRO

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
