      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT0876.
       AUTHOR.     ROBSON FAUSTO CESA.
      *================================================================*
      *                   C P M  -  S I S T E M A S                    *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT0876                                    *
      *    PROGRAMADOR.:   ROBSON FAUSTO CESA      - CPM PATO BRANCO   *
      *    ANALISTA CPM:   FELIPE ALEX SOMMER      - CPM PATO BRANCO   *
      *    ANALISTA....:   VALERIA                 - PROCKWORK GP/50   *
      *    DATA........:   24/02/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   FLEXIBILIZACAO - DEFERIMENTO DA FLEXIBILIZA-*
      *                    CAO POR AGENCIA/CONTA.                      *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                         INCLUDE/BOOK    *
      *                    DB2PRD.PARM_DATA_PROCM        GFCTB0A1      *
      *                    DB2PRD.PDIDO_FLEXZ_CTA        GFCTB0B5      *
      *                    DB2PRD.HIERQ_ALCAD_EXCEC      GFCTB081      *
      *                    DB2PRD.HIERQ_ALCAD_PDRAO      GFCTB083      *
      *                    DB2PRD.PARM_CTRL_GSTAO        GFCTB0A0      *
2201  *                    DB2PRD.TPRMSS_FLEXZ_CTA       GFCTB0I9      *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    I#GFCTL1 - AREA DE COMUNICACAO - ENTRADA                    *
      *    I#GFCTL2 - AREA DE COMUNICACAO - SAIDA                      *
      *    I#GFCTG2 - AREA DE COMUNICACAO - ENTRADA (DESCR. MENSAGEM)  *
      *    I#GFCTG3 - AREA DE COMUNICACAO - SAIDA   (DESCR. MENSAGEM)  *
      *    I#GFCTFZ - AREA DE COMUNICACAO - ENTRADA (VERIFICA ON-LINE) *
      *    I#GFCTG1 - AREA DE COMUNICACAO - SAIDA   (VERIFICA ON-LINE) *
      *    I#GFCT0M - AREA DE COMUNICACAO - ERROS                      *
      *    I#GFCTJL - AREA DO MODULO GFCT5539                          *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    POOL0081 - ALOCA DINAMICAMENTE UM MODULO.                   *
      *    POOL1280 - CALCULA DIFERENCA DE DIAS ENTRE DUAS DATAS       *
      *    POOL1570 - CONSISTE DATA                                    *
      *    GFCT5523 - OBTER DESCRICAO DE MENSAGENS.                    *
      *    GFCT5522 - VERIFICA ON-LINE.                                *
      *    GFCT5018 - OBTER DADOS DE TARIFA                            *
      *    GFCT5539 - OBTER CODIGO DA JUNCAO                           *
      *================================================================*
      *================================================================*
      *    ULTIMA ALTERACAO - 22/01/2007 - VALERIA                     *
      *    INCLUSAO DA TABELA DE PERMISSOES - DB2PRD.TPRMSS_FLEXZ_CTA  *
      *    PARA A CONSISTENCIA DA DATA INICIAL E FINAL DA VIGENCIA DA  *
      *    FLEXIBILIZACAO.                                             *
      *================================================================*
      *================================================================*
      *    ULTIMA ALTERACAO - 09/02/2007 - VALERIA                     *
      *    - INIBICAO DA CONSISTENCIA NA TABELA DE PERMISSAO           *
      *    - ACERTO NA CONSISTENCIA DE DATA INICIAL DE VIGENCIA,       *
      *      QUE TEM QUE SER MAIOR QUE DPROCM-ATUAL.                   *
      *================================================================*
      *                        A L T E R A C A O                       *
      *----------------------------------------------------------------*
      *    PROGRAMADOR..:  JANIEL FREITAS     - SONDA PROCWORK - GP.50 *
      *    ANALISTA.....:  CIBELE BARBOSA     - SONDA PROCWORK - GP.50 *
      *    DATA.........:  04/02/2010                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO.....:                                              *
      *      - INCLUIR CONSISTENCIA DE "SITUACAO" = 10  (PENDENTE DE   *
      *        DE ANALISE).                                            *
      *      - INCLUIR CONSISTENCIA DE "OPERACAO" = 'T' (ENVIO A PROXI-*
      *        DEPENDENCIA), PARA "SITUACAO" = 7.                      *
      *================================================================*
BI0413*----------------------------------------------------------------*
BI0413*                 U L T I M A   A L T E R A C A O                *
BI0413*----------------------------------------------------------------*
BI0413*        SONDA PROCWORK - CONSULTORIA - ALTERACAO - BI0413       *
BI0413*----------------------------------------------------------------*
BI0413*                                                                *
BI0413*    ANALISTA....:  UBIRAJARA(BIRA) - SONDA - IT                 *
BI0413*    DATA........:  04 / 2013                                    *
BI0413*    OBJETIVO....:  O CAMPO (DIAS DE CARENCIA) PASSA A TER       *
BI0413*                   3 BYTES.                                     *
JUN10 *================================================================*
JUN10 *                        A L T E R A C A O                       *
JUN10 *----------------------------------------------------------------*
JUN10 *    PROGRAMADOR..:  FABRICA SOFTWARE   - SONDA PROCWORK - GP.50 *
JUN10 *    ANALISTA.....:  CIBELE BARBOSA     - SONDA PROCWORK - GP.50 *
JUN10 *    DATA.........:  JUN/2010                                    *
JUN10 *----------------------------------------------------------------*
JUN10 *    OBJETIVO.....:                                              *
JUN10 *      PREVER NA CONFIRMACAO A ENTRADA DE MAIS QUE UMA TARIFA.   *
JUN10 *================================================================*
AB1901*              C D - A L T E R A C A O                           *
      *----------------------------------------------------------------*
      *                                                                *
      *    ANALISTA     : LAURA                                        *
      *    PROGRAMADOR  : ABILIO                                       *
      *    DATA         : 30/01/2019                                   *
      *                                                                *
      *    OBJETIVO     : SUBSTITUIR O MODULO GFCT5533 PELO GFCT5539   *
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
           '*   INICIO DA WORKING STORAGE SECTION   *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA INDEXADORES *'.
      *----------------------------------------------------------------*

JUN10  77  IND-1                       PIC  9(002) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREAS AUXILIARES *'.
      *----------------------------------------------------------------*

       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-DATA-INIC-FLEX          PIC  9(008)         VALUE ZEROS.
       77  WRK-DATA-FIM-FLEX           PIC  9(008)         VALUE ZEROS.
       77  WRK-DATA-INIC-TARIF         PIC  9(008)         VALUE ZEROS.
       77  WRK-DATA-FIM-TARIF          PIC  9(008)         VALUE ZEROS.
2201   77  WRK-DATA-INIC-PERMIS        PIC  9(008)         VALUE ZEROS.
2201   77  WRK-DATA-FIM-PERMIS         PIC  9(008)         VALUE ZEROS.

       01  WRK-MSG05.
           05  FILLER                  PIC  X(026)         VALUE
               'ERRO NA CHAMADA DO MODULO'.
           05  WRK-MODULO-MSG          PIC  X(008)         VALUE SPACES.

       01  WRK-DATA-AUX.
           05  WRK-DIA-AUX             PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  WRK-MES-AUX             PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  WRK-ANO-AUX             PIC  9(004)         VALUE ZEROS.

       01  WRK-FUNC-AUX.
           05  WRK-FUNC-NUM            PIC  9(007)         VALUE ZEROS.

       01  WRK-DATA-INV                PIC  9(008)         VALUE ZEROS.
       01  WRK-DATA-INV-R              REDEFINES WRK-DATA-INV.
           05  WRK-ANO-INV             PIC  9(004).
           05  WRK-MES-INV             PIC  9(002).
           05  WRK-DIA-INV             PIC  9(002).

       01  WRK-FUNC-TAB.
           05  WRK-FUNC-NUM-TAB        PIC  9(009)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA POOL1280 *'.
      *----------------------------------------------------------------*

       01  WRK-1280-DATA-INFERIOR      PIC  9(008) COMP-3  VALUE ZEROS.
       01  WRK-1280-DATA-SUPERIOR      PIC  9(008) COMP-3  VALUE ZEROS.
       01  WRK-1280-NRO-DIAS           PIC S9(005) COMP-3  VALUE ZEROS.
       01  WRK-1280-MENSAGEM           PIC  X(050)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA POOL1570 *'.
      *----------------------------------------------------------------*

       01  WRK-1570-ROTINA-DATAS.
           05  WRK-1570-DATA.
               10  FILLER              PIC X(002)          VALUE SPACES.
               10  WRK-1570-DATA-CONS  PIC 9(008)          VALUE ZEROS.
           05  WRK-1570-DATA-R    REDEFINES WRK-1570-DATA  PIC  X(010).
           05  WRK-1570-FORMATO-DATA   PIC 9(002)          VALUE ZEROS.
           05  WRK-1570-MENSAGEM       PIC X(080)          VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA VARIAVEIS DE NULO *'.
      *----------------------------------------------------------------*

       01  WRK-GFCTB083-NULL.
           05 WRK-CIDTFD-DEPDC-SUPE-NULL PIC S9(004) COMP  VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREAS PARA GFCT5018 *'.
      *----------------------------------------------------------------*

       01  WRK-5018-AREA-ENTRADA.
           05  WRK-5018-AMBIENTE       PIC  X(001)         VALUE SPACES.
           05  WRK-5018-CODIGO-TARIFA  PIC  9(005)         VALUE ZEROS.
           05  WRK-5018-DT-REFER       PIC  X(010)         VALUE SPACES.

       01  WRK-5018-AREA-SAIDA.
           05  WRK-5018-COD-RETORNO    PIC  9(002)         VALUE ZEROS.
           05  WRK-5018-COD-SQL-ERRO   PIC  9(003)         VALUE ZEROS.
           05  WRK-5018-DESC-MSG       PIC  X(070)         VALUE SPACES.

       01  WRK-5018-DADOS-RETORNO.
           05  WRK-5018-DADOS-PRINCIPAIS.
               10  WRK-5018-CSERVC-TARIF       PIC  9(005) VALUE ZEROS.
               10  WRK-5018-CBCO-CCREN         PIC  9(003) VALUE ZEROS.
               10  WRK-5018-CIDTFD-LCTO        PIC  9(005) VALUE ZEROS.
               10  WRK-5018-CIDTFD-SNAL-LCTO   PIC  9(001) VALUE ZEROS.
               10  WRK-5018-CEMPR-INC          PIC  9(005) VALUE ZEROS.
               10  WRK-5018-CDEPDC             PIC  9(005) VALUE ZEROS.
               10  WRK-5018-CTPO-SERVC-TARIF   PIC  9(001) VALUE ZEROS.
               10  WRK-5018-RSERVC-TARIF       PIC  X(255) VALUE SPACES.
               10  WRK-5018-RSERVC-TARIF-REDZD PIC  X(100) VALUE SPACES.
               10  WRK-5018-CPSSOA-SERVC-TARIF PIC  X(001) VALUE SPACES.
               10  WRK-5018-CTPO-CTA-DEB-TARIF PIC  9(001) VALUE ZEROS.
               10  WRK-5018-CTPO-DEB-COBR      PIC  9(001) VALUE ZEROS.
               10  WRK-5018-RUND-COBR-SERVC    PIC  X(020) VALUE SPACES.
               10  WRK-5018-CLCTO-SERVC        PIC  9(005) VALUE ZEROS.
               10  WRK-5018-HIDTFD-MANUT-SERVC-R
                                               PIC  X(026) VALUE SPACES.
               10  WRK-5018-CSEQ-MANUT-SERVC-R PIC  9(002) VALUE ZEROS.
080706         10  WRK-5018-CTPO-LCTO-COBR     PIC  9(002) VALUE ZEROS.
080706         10  WRK-5018-CTPO-LCTO-ESTRN    PIC  9(002) VALUE ZEROS.
080706         10  WRK-5018-CCART-DEB-TARIF    PIC  9(005) VALUE ZEROS.
080706         10  WRK-5018-CINDCD-DEB-PCIAL   PIC  X(001) VALUE SPACES.
080706         10  WRK-5018-CINDCD-PCIAL-ULT   PIC  X(001) VALUE SPACES.
080706         10  WRK-5018-CINDCD-EVNTO-PCELD PIC  X(001) VALUE SPACES.

           05  WRK-5018-DADOS-DE-PACOTE  OCCURS   50.
               10  WRK-5018-DINIC-VGCIA-COMPS  PIC  X(010) VALUE SPACES.
               10  WRK-5018-CSEQ-COMPO-PCOTE   PIC  9(003) VALUE ZEROS.
               10  WRK-5018-DFIM-VGCIA-COMPS   PIC  X(010) VALUE SPACES.
               10  WRK-5018-CGRP-SERVC         PIC  9(005) VALUE ZEROS.
               10  WRK-5018-DINIC-VGCIA-SERVC  PIC  X(010) VALUE SPACES.
               10  WRK-5018-CTARIF-COMPO-PCOTE PIC  9(005) VALUE ZEROS.
               10  WRK-5018-QFRANQ-COMPO-PCOTE PIC  9(003) VALUE ZEROS.
               10  WRK-5018-CPER-COBR-COMPO    PIC  9(001) VALUE ZEROS.
               10  WRK-5018-VEXCED-COMPO-PCOTE PIC  9(011)V9(002)
                                                           VALUE ZEROS.
               10  WRK-5018-PEXCED-COMPO-PCOTE PIC  9(007)V9(004)
                                                           VALUE ZEROS.
               10  WRK-5018-QDIA-COBR-COMPO    PIC  9(002) VALUE ZEROS.
               10  WRK-5018-RSEGDA-LIN-EXCED   PIC  X(032) VALUE SPACES.
               10  WRK-5018-HIDTFD-MANUT-SERVC PIC  X(026) VALUE SPACES.
               10  WRK-5018-CSEQ-MANUT-SERVC   PIC  9(002) VALUE ZEROS.
           05  WRK-5018-DADOS-DE-PARAMENTROS.
               10  WRK-5018-DINIC-VGCIA-TARIF-P
                                               PIC  X(010) VALUE SPACES.
               10  WRK-5018-DFIM-VGCIA-TARIF   PIC  X(010) VALUE SPACES.
               10  WRK-5018-CNATUZ-SERVC       PIC  9(003) VALUE ZEROS.
               10  WRK-5018-CCJTO-SERVC        PIC  9(003) VALUE ZEROS.
               10  WRK-5018-CINDCD-DEB-CONSL   PIC  X(001) VALUE SPACES.
               10  WRK-5018-CINDCD-EXCED-CONSL PIC  X(001) VALUE SPACES.
               10  WRK-5018-QFRANQ-TARIF       PIC  9(003) VALUE ZEROS.
               10  WRK-5018-QMAX-TENTV-DEB     PIC  9(003) VALUE ZEROS.
               10  WRK-5018-CPER-FRANQ-TARIF   PIC  9(001) VALUE ZEROS.
               10  WRK-5018-CPER-CALC-TARIF    PIC  9(001) VALUE ZEROS.
               10  WRK-5018-CINIC-PER-CALC     PIC  9(001) VALUE ZEROS.
               10  WRK-5018-DDIA-COBR-TARIF    PIC  9(002) VALUE ZEROS.
BI0413         10  WRK-5018-QDIA-CAREN-TARIF   PIC  9(003) VALUE ZEROS.
               10  WRK-5018-PDESC-CAREN-TARIF  PIC  9(007)V9(004)
                                                           VALUE ZEROS.
               10  WRK-5018-CINDCD-INIC-CAREN  PIC  9(001) VALUE ZEROS.
               10  WRK-5018-QDIA-RTCAO-COBR    PIC  9(002) VALUE ZEROS.
               10  WRK-5018-DINIC-CAREN-TARIF  PIC  X(010) VALUE SPACES.
               10  WRK-5018-RSEGDA-LIN-EXTRT   PIC  X(032) VALUE SPACES.
               10  WRK-5018-HIDTFD-MANUT-SERVC-P
                                               PIC  X(026) VALUE SPACES.
               10  WRK-5018-CSEQ-MANUT-SERVC-P PIC  9(002) VALUE ZEROS.
               10  WRK-5018-CTPO-FRANQ-TARIF   PIC  9(001) VALUE ZEROS.
           05  WRK-5018-DADOS-DE-VALORES       OCCURS   50.
               10  WRK-5018-DINIC-VGCIA-TARIF  PIC  X(010) VALUE SPACES.
               10  WRK-5018-CSEQ-COMPO-TARIF   PIC  9(002) VALUE ZEROS.
               10  WRK-5018-CMOEDA-BACEN       PIC  9(005) VALUE ZEROS.
               10  WRK-5018-RCOMPO-TARIF       PIC  X(255) VALUE SPACES.
               10  WRK-5018-VCOMPO-TARIF       PIC  9(011)V9(002)
                                                           VALUE ZEROS.
               10  WRK-5018-PSERVC-PTADO       PIC  9(007)V9(004)
                                                           VALUE ZEROS.
               10  WRK-5018-VMAX-COMPO-TARIF   PIC  9(011)V9(002)
                                                           VALUE ZEROS.
               10  WRK-5018-VMIN-COMPO-TARIF   PIC  9(011)V9(002)
                                                           VALUE ZEROS.
               10  WRK-5018-CMOMEN-CONVS-MOEDA PIC  9(001) VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA GFCT5505 *'.
      *----------------------------------------------------------------*

       01  WRK-5505-AREA-ENTRADA.
           05  WRK-5505-AMBIENTE       PIC X(001)          VALUE SPACES.
           05  WRK-5505-FUNC-BDSCO     PIC X(007)          VALUE SPACES.

       01  WRK-5505-AREA-SAIDA.
           05  WRK-5505-COD-RETORNO    PIC 9(002)          VALUE ZEROS.
           05  WRK-5505-COD-SQL-ERRO   PIC 9(003)          VALUE ZEROS.
           05  WRK-5505-DESC-MSG       PIC X(070)          VALUE SPACES.
           05  WRK-5505-DADOS-RETORNO.
               10  WRK-5505-COD-JUNCAO PIC 9(005)          VALUE ZEROS.

       01  IO-PCB.
           05  IO-LTERM                PIC X(008)          VALUE SPACES.
           05  FILLER                  PIC X(002)          VALUE SPACES.
           05  IO-STATUS               PIC X(002)          VALUE SPACES.
           05  FILLER                  PIC X(012)          VALUE SPACES.
           05  IO-MODNAME              PIC X(008)          VALUE SPACES.

       01  ALT-PCB.
           05  ALT-LTERM               PIC X(008)          VALUE SPACES.
           05  FILLER                  PIC X(002)          VALUE SPACES.
           05  ALT-STATUS              PIC X(002)          VALUE SPACES.
           05  FILLER                  PIC X(012)          VALUE SPACES.
           05  ALT-MODNAME             PIC X(008)          VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA GFCT5539 *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTJL'.

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
           '* AREA DE DB2 *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0A1
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0B5
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB081
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB083
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0A0
           END-EXEC.

2201       EXEC SQL
2201           INCLUDE GFCTB0I9
2201       END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* FIM DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

JUN10  COPY 'GFCTWANR'.

JUN10  COPY 'GFCTWANU'.

       COPY 'I#GFCT0M'.

      *================================================================*
JUN10  PROCEDURE DIVISION              USING GFCTNR-ENTRADA
JUN10                                        GFCTNU-SAIDA
                                             GFCT0M-AREA-ERROS.
      *================================================================*

      ******************************************************************
      *  CHAMA AS ROTINAS PRINCIPAIS DO PROGRAMA                       *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

JUN10      PERFORM 2000-PROCESSAR VARYING IND-1 FROM 1 BY 1
JUN10          UNTIL IND-1 GREATER 20.

           PERFORM 3000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  INICIALIZA AS AREAS DE COMUNICACAO                            *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCT0M-AREA-ERROS
                                          GFCTFZ-ENTRADA
JUN10                                     GFCTNU-SAIDA.

           INITIALIZE GFCT0M-AREA-ERROS
                      GFCTFZ-ENTRADA
JUN10                 GFCTNU-SAIDA
                      IND-1.

           PERFORM 1100-VERIFICAR-ON-LINE.

           PERFORM 1200-MONTAR-AREA-SAIDA.

           PERFORM 1300-CONSISTIR-DADOS.

           PERFORM 1400-OBTER-DATA-ATUAL.

           PERFORM 1500-ACESSAR-GFCT5505.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ACESSA O MODULO GFCT5522                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-VERIFICAR-ON-LINE          SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO GFCTFZ-SISTEMA.
JUN10      MOVE GFCTNR-TRANSACAO       TO GFCTFZ-TRANSACAO.
JUN10      MOVE GFCTNR-FUNCAO          TO GFCTFZ-FUNCAO.
JUN10      MOVE GFCTNR-FUNC-BDSCO      TO GFCTFZ-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTFZ-FIM.

           MOVE 'GFCT5522'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                             GFCTFZ-ENTRADA
                                             GFCTG1-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
JUN10          MOVE 9                  TO GFCTNU-ERRO
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0876'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-ERRO             EQUAL 9
               MOVE 'GFCT0876'         TO GFCT0M-TRANSACAO
               MOVE GFCTG1-COD-SQL-ERRO
                                       TO GFCT0M-SQLCODE
JUN10                                     GFCTNU-COD-SQL-ERRO
JUN10          MOVE GFCTG1-ERRO        TO GFCTNU-ERRO
               MOVE GFCTG1-COD-MSG-ERRO
JUN10                                  TO GFCTNU-COD-MSG-ERRO
               MOVE GFCTG1-DESC-MSG-ERRO
JUN10                                  TO GFCTNU-DESC-MSG-ERRO
                                          GFCT0M-TEXTO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-CSIT-ONLINE      EQUAL 1
JUN10          MOVE 9                  TO GFCTNU-ERRO
               MOVE 'SISTEMA INDISPONIVEL'
                                       TO GFCT0M-TEXTO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0876'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  FORMATA A AREA DE SAIDA                                       *
      ******************************************************************
      *----------------------------------------------------------------*
       1200-MONTAR-AREA-SAIDA          SECTION.
      *----------------------------------------------------------------*

JUN10      MOVE 1010                       TO GFCTNU-LL.
JUN10      MOVE GFCTNR-ZZ                  TO GFCTNU-ZZ.
JUN10      MOVE GFCTNR-TRANSACAO           TO GFCTNU-TRANSACAO
JUN10                                         GFCTG2-TRANSACAO.
JUN10      MOVE GFCTNR-FUNCAO              TO GFCTNU-FUNCAO
                                              GFCTG2-FUNCAO.

JUN10      IF  GFCTNR-QTDE-OCOR            NOT NUMERIC
JUN10          MOVE ZEROS                  TO GFCTNU-QTDE-OCOR
           ELSE
JUN10          MOVE GFCTNR-QTDE-OCOR       TO GFCTNU-QTDE-OCOR
           END-IF.

JUN10      IF  GFCTNR-QTDE-TOT-REG         NOT NUMERIC
JUN10          MOVE ZEROS                  TO GFCTNU-QTDE-TOT-REG
           ELSE
JUN10          MOVE GFCTNR-QTDE-TOT-REG    TO GFCTNU-QTDE-TOT-REG
           END-IF.

JUN10      MOVE GFCTNR-FUNC-BDSCO          TO GFCTNU-FUNC-BDSCO.
JUN10      MOVE 'S'                        TO GFCTNU-FIM.

JUN10      MOVE GFCTNR-AGENCIA-CONS        TO GFCTNU-AGENCIA
JUN10      MOVE GFCTNR-CONTA-CONS          TO GFCTNU-CONTA
JUN10      MOVE GFCTNR-LOTE-CONS           TO GFCTNU-COD-LOTE
JUN10      MOVE GFCTNR-TIMESTAMP-CONS      TO GFCTNU-TIMESTAMP
JUN10      MOVE GFCTNR-OPERACAO            TO GFCTNU-OPERACAO
JUN10
JUN10      PERFORM VARYING IND-1 FROM 1 BY 1
JUN10        UNTIL IND-1 GREATER 20
JUN10          MOVE GFCTNR-COD-TARI-CONS(IND-1)
JUN10                                      TO GFCTNU-COD-TARIFA(IND-1)
JUN10          MOVE ZEROS            TO GFCTNU-COD-MSG-ERRO-TAR(IND-1)
JUN10      END-PERFORM.

JUN10      MOVE ZEROS                      TO GFCTNU-COD-SQL-ERRO
JUN10                                         GFCTNU-COD-MSG-ERRO
JUN10                                         GFCTNU-ERRO.
           MOVE '0000'                     TO GFCTG2-COD-MSG.

           PERFORM 1310-OBTER-DESCRICAO-MSG.

JUN10      MOVE GFCTG3-DESC-MSG            TO GFCTNU-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  CONSISTE OS DADOS DE ENTRADA                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       1300-CONSISTIR-DADOS            SECTION.
      *----------------------------------------------------------------*

JUN10      IF  GFCTNR-TRANSACAO          EQUAL SPACES OR
JUN10          GFCTNR-TRANSACAO          EQUAL LOW-VALUES OR
JUN10          GFCTNR-FUNCAO             EQUAL SPACES OR
JUN10          GFCTNR-FUNCAO             EQUAL LOW-VALUES OR
JUN10          GFCTNR-FUNC-BDSCO         NOT NUMERIC OR
JUN10          GFCTNR-AGENCIA-CONS       NOT NUMERIC OR
JUN10          GFCTNR-AGENCIA-CONS       EQUAL ZEROS OR
JUN10          GFCTNR-CONTA-CONS         NOT NUMERIC OR
JUN10          GFCTNR-CONTA-CONS         EQUAL ZEROS OR
JUN10          GFCTNR-TARIFAS-CONS(1)    NOT NUMERIC OR
JUN10          GFCTNR-TARIFAS-CONS(2)    NOT NUMERIC OR
JUN10          GFCTNR-TARIFAS-CONS(3)    NOT NUMERIC OR
JUN10          GFCTNR-TARIFAS-CONS(4)    NOT NUMERIC OR
JUN10          GFCTNR-TARIFAS-CONS(5)    NOT NUMERIC OR
JUN10          GFCTNR-TARIFAS-CONS(6)    NOT NUMERIC OR
JUN10          GFCTNR-TARIFAS-CONS(7)    NOT NUMERIC OR
JUN10          GFCTNR-TARIFAS-CONS(8)    NOT NUMERIC OR
JUN10          GFCTNR-TARIFAS-CONS(9)    NOT NUMERIC OR
JUN10          GFCTNR-TARIFAS-CONS(10)   NOT NUMERIC OR
JUN10          GFCTNR-TARIFAS-CONS(11)   NOT NUMERIC OR
JUN10          GFCTNR-TARIFAS-CONS(12)   NOT NUMERIC OR
JUN10          GFCTNR-TARIFAS-CONS(13)   NOT NUMERIC OR
JUN10          GFCTNR-TARIFAS-CONS(14)   NOT NUMERIC OR
JUN10          GFCTNR-TARIFAS-CONS(15)   NOT NUMERIC OR
JUN10          GFCTNR-TARIFAS-CONS(16)   NOT NUMERIC OR
JUN10          GFCTNR-TARIFAS-CONS(17)   NOT NUMERIC OR
JUN10          GFCTNR-TARIFAS-CONS(18)   NOT NUMERIC OR
JUN10          GFCTNR-TARIFAS-CONS(19)   NOT NUMERIC OR
JUN10          GFCTNR-TARIFAS-CONS(20)   NOT NUMERIC OR
JUN10          GFCTNR-TIMESTAMP-CONS     EQUAL SPACES OR
JUN10          GFCTNR-TIMESTAMP-CONS     EQUAL LOW-VALUES OR
JUN10          GFCTNR-OPERACAO           NOT EQUAL 'D' AND 'I' AND 'T'
JUN10          MOVE 1                    TO GFCTNU-ERRO
JUN10                                       GFCTNU-COD-MSG-ERRO
JUN10          MOVE ZEROS                TO GFCTNU-COD-SQL-ERRO
               MOVE '0001'               TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESCRICAO-MSG
JUN10          MOVE GFCTG3-DESC-MSG      TO GFCTNU-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ACESSA O MODULO GFCT5523                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       1310-OBTER-DESCRICAO-MSG        SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                     TO GFCTG3-SAIDA.

           INITIALIZE GFCTG3-SAIDA.

           MOVE 100                        TO GFCTG2-LL.
           MOVE ZEROS                      TO GFCTG2-ZZ.
JUN10      MOVE GFCTNR-TRANSACAO           TO GFCTG2-TRANSACAO.
JUN10      MOVE GFCTNR-FUNCAO              TO GFCTG2-FUNCAO.
JUN10      MOVE GFCTNR-FUNC-BDSCO          TO GFCTG2-FUNC-BDSCO.
           MOVE 'O'                        TO GFCTG2-TIPO-PROC.
           MOVE 'N'                        TO GFCTG2-FIM.

           MOVE 'GFCT5523'                 TO WRK-MODULO.

           CALL WRK-MODULO USING
                                                 GFCTG2-ENTRADA
                                                 GFCTG3-SAIDA
                                                 GFCT0M-AREA-ERROS.

           IF  RETURN-CODE                 NOT EQUAL ZEROS
JUN10          MOVE 9                      TO GFCTNU-ERRO
               MOVE WRK-MODULO             TO WRK-MODULO-MSG
               MOVE WRK-MSG05              TO GFCT0M-TEXTO
               MOVE WRK-MODULO             TO GFCT0M-PROGRAMA
               MOVE 'APL'                  TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0876'             TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO                 EQUAL 9
JUN10          MOVE 9                      TO GFCTNU-ERRO
               MOVE WRK-MODULO             TO WRK-MODULO-MSG
               MOVE WRK-MSG05              TO GFCT0M-TEXTO
               MOVE WRK-MODULO             TO GFCT0M-PROGRAMA
               MOVE 'APL'                  TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0876'             TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           ELSE
               IF  GFCTG3-ERRO             EQUAL 1
JUN10              MOVE 1                  TO GFCTNU-ERRO
                   MOVE GFCTG3-COD-SQL-ERRO
JUN10                                      TO GFCTNU-COD-SQL-ERRO
                   MOVE GFCTG3-COD-MSG-ERRO
JUN10                                      TO GFCTNU-COD-MSG-ERRO
                   MOVE GFCTG3-DESC-MSG-ERRO
JUN10                                      TO GFCTNU-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               END-IF
            END-IF.

      *----------------------------------------------------------------*
       1310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  CONSULTA A TABELA GFCTB0A1                                    *
      ******************************************************************
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
               PERFORM 1310-OBTER-DESCRICAO-MSG
               STRING GFCTG3-DESC-MSG ' - GFCTB0A1'
JUN10              DELIMITED BY '  '   INTO GFCTNU-DESC-MSG-ERRO
               MOVE 'GFCT0876'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'PARM_DATA_PROCM'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
JUN10                                     GFCTNU-COD-SQL-ERRO
               MOVE '0010'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
JUN10          MOVE  9                 TO GFCTNU-ERRO
JUN10          MOVE  10                TO GFCTNU-COD-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ACESSA O MODULO GFCT5505                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       1500-ACESSAR-GFCT5505           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-5505-AREA-ENTRADA
                      WRK-5505-AREA-SAIDA.

           MOVE 'O'                    TO WRK-5505-AMBIENTE.
JUN10      MOVE GFCTNR-FUNC-BDSCO      TO WRK-5505-FUNC-BDSCO.
           MOVE 'GFCT5505'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                             WRK-5505-AREA-ENTRADA
                                             WRK-5505-AREA-SAIDA
                                             IO-PCB
                                             ALT-PCB.

           IF  RETURN-CODE             NOT EQUAL ZEROS
JUN10          MOVE 9                  TO GFCTNU-ERRO
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-5505-COD-RETORNO    NOT EQUAL ZEROS
JUN10          MOVE 1                  TO GFCTNU-ERRO
JUN10          MOVE ZEROS              TO GFCTNU-COD-SQL-ERRO
JUN10          MOVE WRK-5505-DESC-MSG  TO GFCTNU-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  PROCESSAMENTO DO PROGRAMA                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

JUN10      IF  GFCTNR-COD-TARI-CONS(IND-1) EQUAL ZEROS
JUN10          GO                      TO 2000-99-FIM
JUN10      END-IF.
JUN10
           PERFORM 2100-LER-TABELA.

           PERFORM 2200-PROCESSAMENTO-PRINCIPAL.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  EFETUA LEITURA NA TABELA GFCTB0B5                             *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-LER-TABELA                 SECTION.
      *----------------------------------------------------------------*

           MOVE 237                    TO CBCO-CTA-CLI     OF GFCTB0B5.
JUN10      MOVE GFCTNR-AGENCIA-CONS    TO CJUNC-DEPDC      OF GFCTB0B5.
JUN10      MOVE GFCTNR-CONTA-CONS      TO CCTA-CLI         OF GFCTB0B5.
JUN10      MOVE GFCTNR-COD-TARI-CONS(IND-1)
JUN10                                  TO CSERVC-TARIF     OF GFCTB0B5.
JUN10      MOVE GFCTNR-TIMESTAMP-CONS  TO HIDTFD-FLEXZ-CTA OF GFCTB0B5.

           EXEC SQL
             SELECT
                   CBCO_CTA_CLI,
                   CJUNC_DEPDC,
                   CCTA_CLI,
                   CSERVC_TARIF,
                   HIDTFD_FLEXZ_CTA,
                   DINIC_FLEXZ_CTA,
                   DFIM_FLEXZ_CTA,
                   QFRANQ_FLEXZ_CTA,
                   CPER_FRANQ_CTA,
                   VNEGOC_FLEXZ_CTA,
                   PDESC_FLEXZ_CTA,
                   DDIA_AGNDA,
                   QDIA_FLEXZ_CTA,
                   CEMPR_ORIGE_FLEXZ,
                   CDEPDC_ORIGE_FLEXZ,
                   CFUNC_SOLTC_FLEXZ,
                   HSOLTC_FLEXZ,
                   CTPO_OPER_MOTVO,
                   CMOTVO_JUSTF,
                   RJUSTF_SOLTC_FLEXZ,
                   CEMPR_PAREC_FLEXZ,
                   CIDTFD_DEPDC,
                   CDEPDC_PAREC_FLEXZ,
                   CFUNC_PAREC_FLEXZ,
                   HPAREC_FLEXZ,
                   RJUSTF_PAREC_FLEXZ,
                   CSIT_SOLTC_FLEXZ,
                   CFUNC_EXCL_SIST
             INTO
                   :GFCTB0B5.CBCO-CTA-CLI,
                   :GFCTB0B5.CJUNC-DEPDC,
                   :GFCTB0B5.CCTA-CLI,
                   :GFCTB0B5.CSERVC-TARIF,
                   :GFCTB0B5.HIDTFD-FLEXZ-CTA,
                   :GFCTB0B5.DINIC-FLEXZ-CTA,
                   :GFCTB0B5.DFIM-FLEXZ-CTA,
                   :GFCTB0B5.QFRANQ-FLEXZ-CTA,
                   :GFCTB0B5.CPER-FRANQ-CTA,
                   :GFCTB0B5.VNEGOC-FLEXZ-CTA,
                   :GFCTB0B5.PDESC-FLEXZ-CTA,
                   :GFCTB0B5.DDIA-AGNDA,
                   :GFCTB0B5.QDIA-FLEXZ-CTA,
                   :GFCTB0B5.CEMPR-ORIGE-FLEXZ,
                   :GFCTB0B5.CDEPDC-ORIGE-FLEXZ,
                   :GFCTB0B5.CFUNC-SOLTC-FLEXZ,
                   :GFCTB0B5.HSOLTC-FLEXZ,
                   :GFCTB0B5.CTPO-OPER-MOTVO,
                   :GFCTB0B5.CMOTVO-JUSTF,
                   :GFCTB0B5.RJUSTF-SOLTC-FLEXZ,
                   :GFCTB0B5.CEMPR-PAREC-FLEXZ,
                   :GFCTB0B5.CIDTFD-DEPDC,
                   :GFCTB0B5.CDEPDC-PAREC-FLEXZ,
                   :GFCTB0B5.CFUNC-PAREC-FLEXZ,
                   :GFCTB0B5.HPAREC-FLEXZ,
                   :GFCTB0B5.RJUSTF-PAREC-FLEXZ,
                   :GFCTB0B5.CSIT-SOLTC-FLEXZ,
                   :GFCTB0B5.CFUNC-EXCL-SIST
             FROM   DB2PRD.PDIDO_FLEXZ_CTA
             WHERE
                   CBCO_CTA_CLI       = :GFCTB0B5.CBCO-CTA-CLI       AND
                   CJUNC_DEPDC        = :GFCTB0B5.CJUNC-DEPDC        AND
                   CCTA_CLI           = :GFCTB0B5.CCTA-CLI           AND
                   CSERVC_TARIF       = :GFCTB0B5.CSERVC-TARIF       AND
                   HIDTFD_FLEXZ_CTA   = :GFCTB0B5.HIDTFD-FLEXZ-CTA
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE  '0010'            TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESCRICAO-MSG
               STRING GFCTG3-DESC-MSG ' - GFCTB0B5'
JUN10              DELIMITED BY '  '   INTO GFCTNU-DESC-MSG-ERRO
               MOVE 'GFCT0876'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_CTA'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
JUN10                                     GFCTNU-COD-SQL-ERRO
               MOVE '0020'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
JUN10          MOVE  9                 TO GFCTNU-ERRO
JUN10          MOVE  10                TO GFCTNU-COD-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  PROCESSAMENTO PRINCIPAL DO PROGRAMA                           *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-PROCESSAMENTO-PRINCIPAL    SECTION.
      *----------------------------------------------------------------*

JUN10      IF  GFCTNR-OPERACAO         EQUAL 'D' AND
               CSIT-SOLTC-FLEXZ        OF GFCTB0B5
JF                                     NOT EQUAL 01 AND 07 AND 10
JUN10          MOVE 1                  TO GFCTNU-ERRO
JUN10          MOVE ZEROS              TO GFCTNU-COD-SQL-ERRO
JUN10          MOVE 0693               TO GFCTNU-COD-MSG-ERRO
               MOVE '0693'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESCRICAO-MSG
JUN10          MOVE GFCTG3-DESC-MSG    TO GFCTNU-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

JUN10      IF  GFCTNR-OPERACAO         EQUAL 'I' AND
               CSIT-SOLTC-FLEXZ        OF GFCTB0B5
JF                                     NOT EQUAL 01 AND 07 AND 10
JUN10          MOVE 1                  TO GFCTNU-ERRO
JUN10          MOVE ZEROS              TO GFCTNU-COD-SQL-ERRO
JUN10          MOVE 0694               TO GFCTNU-COD-MSG-ERRO
JUN10          MOVE '0694'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESCRICAO-MSG
JUN10          MOVE GFCTG3-DESC-MSG    TO GFCTNU-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

JUN10      IF  GFCTNR-OPERACAO         EQUAL 'T' AND
               WRK-5505-COD-JUNCAO     NOT EQUAL CDEPDC-PAREC-FLEXZ
                                       OF GFCTB0B5 AND
JF                                     CSIT-SOLTC-FLEXZ
JF                                     OF GFCTB0B5
JF                                     NOT EQUAL 07
JUN10          MOVE 1                  TO GFCTNU-ERRO
JUN10          MOVE ZEROS              TO GFCTNU-COD-SQL-ERRO
JUN10          MOVE 0695               TO GFCTNU-COD-MSG-ERRO
               MOVE '0695'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESCRICAO-MSG
JUN10          MOVE GFCTG3-DESC-MSG    TO GFCTNU-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

JUN10      IF  GFCTNR-OPERACAO         EQUAL 'I'
JUN10          IF  GFCTNR-RJUSTF-DEFER EQUAL SPACES OR LOW-VALUES
JUN10              MOVE 1              TO GFCTNU-ERRO
JUN10              MOVE ZEROS          TO GFCTNU-COD-SQL-ERRO
JUN10              MOVE 0691           TO GFCTNU-COD-MSG-ERRO
                   MOVE '0691'         TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESCRICAO-MSG
JUN10              MOVE GFCTG3-DESC-MSG TO GFCTNU-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               ELSE
JUN10              MOVE GFCTNR-RJUSTF-DEFER
                                       TO RJUSTF-PAREC-FLEXZ-TEXT
                                       OF GFCTB0B5
JUN10              MOVE LENGTH         OF GFCTNR-RJUSTF-DEFER
                                       TO RJUSTF-PAREC-FLEXZ-LEN
                                       OF GFCTB0B5
                   MOVE 03             TO CSIT-SOLTC-FLEXZ
                                       OF GFCTB0B5
                   PERFORM 2210-ATUALIZAR-GFCTB0B5
JUN10 *************PERFORM 3000-FINALIZAR
               END-IF
           ELSE
JUN10          IF  GFCTNR-OPERACAO      EQUAL 'T'
                   PERFORM 2220-TRATAR-PROX-DEP
                   PERFORM 2210-ATUALIZAR-GFCTB0B5
JUN10 *************PERFORM 3000-FINALIZAR
               ELSE
JUN10              IF  GFCTNR-INIC-VIG-FLEX
                                       NOT EQUAL DINIC-FLEXZ-CTA
                                       OF GFCTB0B5   OR
JUN10                  GFCTNR-FIM-VIG-FLEX
                                       NOT EQUAL DFIM-FLEXZ-CTA
                                       OF GFCTB0B5
                       PERFORM 2230-CONS-NOVA-DT-VIG
                   ELSE

JUN10                  MOVE GFCTNR-INIC-VIG-FLEX   TO WRK-DATA-AUX
                       MOVE WRK-DIA-AUX            TO WRK-DIA-INV
                       MOVE WRK-MES-AUX            TO WRK-MES-INV
                       MOVE WRK-ANO-AUX            TO WRK-ANO-INV
                       MOVE WRK-DATA-INV           TO
                                                   WRK-DATA-INIC-FLEX

0207                   MOVE DPROCM-PROX            OF GFCTB0A1
                                                   TO WRK-DATA-AUX
                       MOVE WRK-DIA-AUX            TO WRK-DIA-INV
                       MOVE WRK-MES-AUX            TO WRK-MES-INV
                       MOVE WRK-ANO-AUX            TO WRK-ANO-INV
                       IF  WRK-DATA-INIC-FLEX
                                            LESS WRK-DATA-INV
JUN10                      MOVE 1           TO GFCTNU-ERRO
JUN10                      MOVE ZEROS       TO GFCTNU-COD-SQL-ERRO
JUN10                      MOVE 0854        TO GFCTNU-COD-MSG-ERRO
                           MOVE '0854'      TO GFCTG2-COD-MSG
                           PERFORM 1310-OBTER-DESCRICAO-MSG
                           MOVE GFCTG3-DESC-MSG
JUN10                                       TO GFCTNU-DESC-MSG-ERRO
                           PERFORM 3000-FINALIZAR
                       END-IF

                   END-IF
JUN10              IF  GFCTNR-RJUSTF-DEFER EQUAL SPACES OR LOW-VALUES
JUN10                  MOVE 1              TO GFCTNU-ERRO
JUN10                  MOVE ZEROS          TO GFCTNU-COD-SQL-ERRO
JUN10                  MOVE 0691           TO GFCTNU-COD-MSG-ERRO
                       MOVE '0691'         TO GFCTG2-COD-MSG
                       PERFORM 1310-OBTER-DESCRICAO-MSG
JUN10                  MOVE GFCTG3-DESC-MSG TO GFCTNU-DESC-MSG-ERRO
                       PERFORM 3000-FINALIZAR
                   ELSE
JUN10                  MOVE GFCTNR-RJUSTF-DEFER
                                       TO RJUSTF-PAREC-FLEXZ-TEXT
                                       OF GFCTB0B5
JUN10                  MOVE LENGTH     OF GFCTNR-RJUSTF-DEFER
                                       TO RJUSTF-PAREC-FLEXZ-LEN
                                       OF GFCTB0B5
                       MOVE 02         TO CSIT-SOLTC-FLEXZ
                                       OF GFCTB0B5
                       PERFORM 2210-ATUALIZAR-GFCTB0B5
JUN10 *****************PERFORM 3000-FINALIZAR
                   END-IF
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  EFETUA ATUALIZACAO NA TABELA GFCTB0B5                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2210-ATUALIZAR-GFCTB0B5         SECTION.
      *----------------------------------------------------------------*

JUN10      MOVE GFCTNR-INIC-VIG-FLEX   TO DINIC-FLEXZ-CTA OF GFCTB0B5.
JUN10      MOVE GFCTNR-FIM-VIG-FLEX    TO DFIM-FLEXZ-CTA  OF GFCTB0B5.

JUN10      MOVE GFCTNR-FUNC-BDSCO      TO WRK-FUNC-AUX.
           MOVE WRK-FUNC-NUM           TO CFUNC-PAREC-FLEXZ OF GFCTB0B5.


VUL        MOVE CFUNC-SOLTC-FLEXZ      OF GFCTB0B5
                                       TO WRK-FUNC-NUM-TAB

           IF WRK-FUNC-NUM-TAB(3:7)    EQUAL WRK-FUNC-NUM
                   MOVE 1              TO GFCTNU-ERRO
                   MOVE ZEROS          TO GFCTNU-COD-SQL-ERRO
                   MOVE 0688           TO GFCTNU-COD-MSG-ERRO
                   MOVE '2013'         TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESCRICAO-MSG
                   MOVE GFCTG3-DESC-MSG TO GFCTNU-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
           END-IF.



           EXEC SQL
             UPDATE   DB2PRD.PDIDO_FLEXZ_CTA
               SET
                   DINIC_FLEXZ_CTA    = :GFCTB0B5.DINIC-FLEXZ-CTA,
                   DFIM_FLEXZ_CTA     = :GFCTB0B5.DFIM-FLEXZ-CTA,
                   CIDTFD_DEPDC       = :GFCTB0B5.CIDTFD-DEPDC,
                   CDEPDC_PAREC_FLEXZ = :GFCTB0B5.CDEPDC-PAREC-FLEXZ,
                   CFUNC_PAREC_FLEXZ  = :GFCTB0B5.CFUNC-PAREC-FLEXZ,
                   HPAREC_FLEXZ       = CURRENT TIMESTAMP,
                   RJUSTF_PAREC_FLEXZ = :GFCTB0B5.RJUSTF-PAREC-FLEXZ,
                   CSIT_SOLTC_FLEXZ   = :GFCTB0B5.CSIT-SOLTC-FLEXZ
             WHERE
                   CBCO_CTA_CLI       = :GFCTB0B5.CBCO-CTA-CLI       AND
                   CJUNC_DEPDC        = :GFCTB0B5.CJUNC-DEPDC        AND
                   CCTA_CLI           = :GFCTB0B5.CCTA-CLI           AND
                   CSERVC_TARIF       = :GFCTB0B5.CSERVC-TARIF       AND
                   HIDTFD_FLEXZ_CTA   = :GFCTB0B5.HIDTFD-FLEXZ-CTA
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE  '0010'            TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESCRICAO-MSG
               STRING GFCTG3-DESC-MSG ' - GFCTB0B5'
JUN10              DELIMITED BY '  '   INTO GFCTNU-DESC-MSG-ERRO
               MOVE 'GFCT0876'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_CTA'  TO GFCT0M-NOME-TAB
               MOVE 'UPDATE'           TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
JUN10                                     GFCTNU-COD-SQL-ERRO
               MOVE '0030'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
JUN10          MOVE  9                 TO GFCTNU-ERRO
JUN10          MOVE  10                TO GFCTNU-COD-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ACESSA A TABELA GFCTB081                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2220-TRATAR-PROX-DEP            SECTION.
      *----------------------------------------------------------------*

           MOVE CTPO-OPER-MOTVO        OF GFCTB0B5
                                       TO CTPO-OPER-MOTVO   OF GFCTB081.
           MOVE CSERVC-TARIF           OF GFCTB0B5
                                       TO CSERVC-TARIF      OF GFCTB081.
           MOVE CIDTFD-DEPDC           OF GFCTB0B5
                                       TO CIDTFD-DEPDC      OF GFCTB081.
           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO DINIC-ALCAD-EXCEC OF GFCTB081
                                          DFIM-ALCAD-EXCEC  OF GFCTB081.

           EXEC SQL
             SELECT
                   CDEPDC_SUPE
             INTO
                   :GFCTB081.CDEPDC-SUPE
             FROM   DB2PRD.HIERQ_ALCAD_EXCEC
             WHERE
                   CTPO_OPER_MOTVO    = :GFCTB081.CTPO-OPER-MOTVO    AND
                   CSERVC_TARIF       = :GFCTB081.CSERVC-TARIF       AND
                   CIDTFD_DEPDC       = :GFCTB081.CIDTFD-DEPDC       AND
                   DINIC_ALCAD_EXCEC <= :GFCTB081.DINIC-ALCAD-EXCEC  AND
                   DFIM_ALCAD_EXCEC  >= :GFCTB081.DFIM-ALCAD-EXCEC   AND
VULNE              CSIT_HIERQ_ALCAD   = 2
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE  '0010'            TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESCRICAO-MSG
               STRING GFCTG3-DESC-MSG ' - GFCTB0B5'
JUN10              DELIMITED BY '  '   INTO GFCTNU-DESC-MSG-ERRO
               MOVE 'GFCT0876'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'HIERQ_ALCAD_EXCEC'
                                       TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
JUN10                                     GFCTNU-COD-SQL-ERRO
               MOVE '0040'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
JUN10          MOVE  9                 TO GFCTNU-ERRO
JUN10          MOVE  10                TO GFCTNU-COD-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               MOVE CDEPDC-SUPE        OF GFCTB081
                                       TO CDEPDC-PAREC-FLEXZ OF GFCTB0B5
               MOVE 'XX'               TO CIDTFD-DEPDC       OF GFCTB0B5
           ELSE
               PERFORM 2221-ACESSAR-GFCTB083
               PERFORM 2222-OBTER-JUNCAO
           END-IF.

      *----------------------------------------------------------------*
       2220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ACESSA A TABELA GFCTB083                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2221-ACESSAR-GFCTB083           SECTION.
      *----------------------------------------------------------------*

           MOVE CTPO-OPER-MOTVO        OF GFCTB0B5
                                       TO CTPO-OPER-MOTVO   OF GFCTB083.
           MOVE CIDTFD-DEPDC           OF GFCTB0B5
                                       TO CIDTFD-DEPDC-SUBOR
                                                            OF GFCTB083.
           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO DINIC-ALCAD-PDRAO OF GFCTB083
                                          DFIM-ALCAD-PDRAO  OF GFCTB083.

           EXEC SQL
             SELECT
                   CIDTFD_DEPDC_SUPE
             INTO
                   :GFCTB083.CIDTFD-DEPDC-SUPE
                     :WRK-CIDTFD-DEPDC-SUPE-NULL
             FROM   DB2PRD.HIERQ_ALCAD_PDRAO
             WHERE
                   CTPO_OPER_MOTVO    = :GFCTB083.CTPO-OPER-MOTVO    AND
                   CIDTFD_DEPDC_SUBOR = :GFCTB083.CIDTFD-DEPDC-SUBOR AND
                   DINIC_ALCAD_PDRAO <= :GFCTB083.DINIC-ALCAD-PDRAO  AND
                   DFIM_ALCAD_PDRAO  >= :GFCTB083.DFIM-ALCAD-PDRAO   AND
VULNE              CSIT_HIERQ_ALCAD   = 2
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE  '0010'            TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESCRICAO-MSG
               STRING GFCTG3-DESC-MSG ' - GFCTB083'
JUN10              DELIMITED BY '  '   INTO GFCTNU-DESC-MSG-ERRO
               MOVE 'GFCT0876'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'HIERQ_ALCAD_PDRAO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
JUN10                                     GFCTNU-COD-SQL-ERRO
               MOVE '0050'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
JUN10          MOVE  9                 TO GFCTNU-ERRO
JUN10          MOVE  10                TO GFCTNU-COD-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
JUN10          MOVE 1                  TO GFCTNU-ERRO
JUN10          MOVE ZEROS              TO GFCTNU-COD-SQL-ERRO
JUN10          MOVE 0455               TO GFCTNU-COD-MSG-ERRO
               MOVE '0455'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESCRICAO-MSG
JUN10          MOVE GFCTG3-DESC-MSG    TO GFCTNU-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-CIDTFD-DEPDC-SUPE-NULL
                                       NOT EQUAL ZEROS
JUN10          MOVE 1                  TO GFCTNU-ERRO
JUN10          MOVE ZEROS              TO GFCTNU-COD-SQL-ERRO
JUN10          MOVE 0696               TO GFCTNU-COD-MSG-ERRO
               MOVE '0696'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESCRICAO-MSG
JUN10          MOVE GFCTG3-DESC-MSG    TO GFCTNU-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE CIDTFD-DEPDC-SUPE      OF GFCTB083
                                       TO CIDTFD-DEPDC OF GFCTB0B5.

      *----------------------------------------------------------------*
       2221-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ACESSA O MODULO GFCT5539                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2222-OBTER-JUNCAO               SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCT0M-AREA-ERROS
                                          GFCTJL-AREA.

           INITIALIZE GFCT0M-AREA-ERROS
                      GFCTJL-AREA.

           MOVE 'O'                    TO GFCTJL-TPO-AMBIENTE.
           MOVE CIDTFD-DEPDC-SUPE      OF GFCTB083
                                       TO GFCTJL-CIDTFD-DEPDC.
JUN10      MOVE GFCTNR-AGENCIA-CONS    TO GFCTJL-CJUNC-DEPDC.
JUN10      MOVE GFCTNR-CONTA-CONS(7:7) TO GFCTJL-CCTA-CLI.
JUN10      MOVE GFCTNR-COD-TARI-CONS(IND-1)
                                       TO GFCTJL-CSERVC-TARIF.
           MOVE 'GFCT5539'             TO WRK-MODULO.

           CALL WRK-MODULO USING             GFCTJL-AREA
                                             GFCT0M-AREA-ERROS.

           IF  GFCTJL-COD-RETORNO      NOT EQUAL ZEROS

               EVALUATE GFCTJL-COD-RETORNO
                   WHEN 01
                   WHEN 02
                   WHEN 06
                   WHEN 08
JUN10                  MOVE 1                TO GFCTNU-ERRO
JUN10                  MOVE ZEROS            TO GFCTNU-COD-SQL-ERRO
JUN10                  MOVE 0001             TO GFCTNU-COD-MSG-ERRO
                       MOVE '0001'           TO GFCTG2-COD-MSG
                       PERFORM 1310-OBTER-DESCRICAO-MSG
JUN10                  MOVE GFCTG3-DESC-MSG  TO GFCTNU-DESC-MSG-ERRO

                   WHEN 03
                   WHEN 04
                   WHEN 05
                   WHEN 09
JUN10                  MOVE 1                TO GFCTNU-ERRO
JUN10                  MOVE ZEROS            TO GFCTNU-COD-SQL-ERRO
JUN10                  MOVE 0871             TO GFCTNU-COD-MSG-ERRO
                       MOVE '0871'           TO GFCTG2-COD-MSG
                       PERFORM 1310-OBTER-DESCRICAO-MSG
JUN10                  MOVE GFCTG3-DESC-MSG  TO GFCTNU-DESC-MSG-ERRO

                   WHEN 07
                   WHEN 10
JUN10                  MOVE 9                  TO GFCTNU-ERRO
                       MOVE WRK-MODULO         TO WRK-MODULO-MSG
                       MOVE WRK-MSG05          TO GFCT0M-TEXTO
                       MOVE 'GFCT0876'         TO GFCT0M-PROGRAMA
                       MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
                       MOVE 'GFCT0876'         TO GFCT0M-TRANSACAO

AB1901             WHEN 15
.                  WHEN 16
.                       MOVE 1         TO GFCTNU-ERRO
.                       MOVE ZEROS     TO GFCTNU-COD-SQL-ERRO
.                       MOVE 15        TO GFCTNU-COD-MSG-ERRO
AB1901                  MOVE GFCT0M-TEXTO TO GFCTNU-DESC-MSG-ERRO
                   WHEN OTHER
JUN10                  MOVE 9                  TO GFCTNU-ERRO
                       MOVE WRK-MODULO         TO WRK-MODULO-MSG
                       MOVE WRK-MSG05          TO GFCT0M-TEXTO
                       MOVE 'GFCT0876'         TO GFCT0M-PROGRAMA
                       MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
                       MOVE 'GFCT0876'         TO GFCT0M-TRANSACAO
               END-EVALUATE
               PERFORM 3000-FINALIZAR
           END-IF.


           MOVE GFCTJL-CDEPDC                  TO CDEPDC-PAREC-FLEXZ
                                                           OF GFCTB0B5.

      *----------------------------------------------------------------*
       2222-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  CONSISTE NOVA DATA VIGENTE                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2230-CONS-NOVA-DT-VIG           SECTION.
      *----------------------------------------------------------------*

JUN10      MOVE GFCTNR-INIC-VIG-FLEX   TO WRK-1570-DATA.

           PERFORM 2231-VALIDAR-DATA.

JUN10      IF  RETURN-CODE             EQUAL 04
JUN10          MOVE 1                  TO GFCTNU-ERRO
JUN10          MOVE ZEROS              TO GFCTNU-COD-SQL-ERRO
0107           MOVE 0801               TO GFCTNU-COD-MSG-ERRO
0107           MOVE '0801'             TO GFCTG2-COD-MSG
0107           PERFORM 1310-OBTER-DESCRICAO-MSG
JUN10          MOVE GFCTG3-DESC-MSG    TO GFCTNU-DESC-MSG-ERRO
0107           PERFORM 3000-FINALIZAR
0107       END-IF.

JUN10      MOVE GFCTNR-INIC-VIG-FLEX   TO WRK-DATA-AUX.
           MOVE WRK-DIA-AUX            TO WRK-DIA-INV.
           MOVE WRK-MES-AUX            TO WRK-MES-INV.
           MOVE WRK-ANO-AUX            TO WRK-ANO-INV.
           MOVE WRK-DATA-INV           TO WRK-DATA-INIC-FLEX.

JUN10      MOVE GFCTNR-FIM-VIG-FLEX    TO WRK-1570-DATA.

           PERFORM 2231-VALIDAR-DATA.

JUN10      IF  RETURN-CODE             EQUAL 04
JUN10          MOVE 1                  TO GFCTNU-ERRO
JUN10          MOVE ZEROS              TO GFCTNU-COD-SQL-ERRO
JUN10          MOVE 0802               TO GFCTNU-COD-MSG-ERRO
0107           MOVE '0802'             TO GFCTG2-COD-MSG
0107           PERFORM 1310-OBTER-DESCRICAO-MSG
JUN10          MOVE GFCTG3-DESC-MSG    TO GFCTNU-DESC-MSG-ERRO
0107           PERFORM 3000-FINALIZAR
0107       END-IF.

JUN10      MOVE GFCTNR-FIM-VIG-FLEX    TO WRK-DATA-AUX.
           MOVE WRK-DIA-AUX            TO WRK-DIA-INV.
           MOVE WRK-MES-AUX            TO WRK-MES-INV.
           MOVE WRK-ANO-AUX            TO WRK-ANO-INV.
           MOVE WRK-DATA-INV           TO WRK-DATA-FIM-FLEX.

           PERFORM 2232-ACESSAR-GFCT5018.

           MOVE WRK-5018-DINIC-VGCIA-TARIF-P
                                       TO WRK-DATA-AUX.
           MOVE WRK-DIA-AUX            TO WRK-DIA-INV.
           MOVE WRK-MES-AUX            TO WRK-MES-INV.
           MOVE WRK-ANO-AUX            TO WRK-ANO-INV.
           MOVE WRK-DATA-INV           TO WRK-DATA-INIC-TARIF.
           MOVE WRK-5018-DFIM-VGCIA-TARIF
                                       TO WRK-DATA-AUX.
           MOVE WRK-DIA-AUX            TO WRK-DIA-INV.
           MOVE WRK-MES-AUX            TO WRK-MES-INV.
           MOVE WRK-ANO-AUX            TO WRK-ANO-INV.
           MOVE WRK-DATA-INV           TO WRK-DATA-FIM-TARIF.

           IF  WRK-DATA-INIC-FLEX      LESS WRK-DATA-INIC-TARIF
JUN10          MOVE 1                  TO GFCTNU-ERRO
JUN10          MOVE ZEROS              TO GFCTNU-COD-SQL-ERRO
JUN10          MOVE 0610               TO GFCTNU-COD-MSG-ERRO
               MOVE '0610'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESCRICAO-MSG
JUN10          MOVE GFCTG3-DESC-MSG    TO GFCTNU-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-DATA-FIM-FLEX       GREATER WRK-DATA-FIM-TARIF
JUN10          MOVE 1                  TO GFCTNU-ERRO
               MOVE ZEROS              TO GFCTNU-COD-SQL-ERRO
               MOVE 0611               TO GFCTNU-COD-MSG-ERRO
               MOVE '0611'             TO  GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESCRICAO-MSG
JUN10          MOVE GFCTG3-DESC-MSG    TO GFCTNU-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-DATA-FIM-FLEX       LESS WRK-DATA-INIC-FLEX
JUN10          MOVE 1                  TO GFCTNU-ERRO
JUN10          MOVE ZEROS              TO GFCTNU-COD-SQL-ERRO
JUN10          MOVE 0611               TO GFCTNU-COD-MSG-ERRO
               MOVE '0611'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESCRICAO-MSG
JUN10          MOVE GFCTG3-DESC-MSG    TO GFCTNU-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO WRK-DATA-AUX.
           MOVE WRK-DIA-AUX            TO WRK-DIA-INV.
           MOVE WRK-MES-AUX            TO WRK-MES-INV.
           MOVE WRK-ANO-AUX            TO WRK-ANO-INV.

           IF  WRK-DATA-INIC-FLEX      NOT GREATER WRK-DATA-INV
JUN10          MOVE 1                  TO GFCTNU-ERRO
JUN10          MOVE ZEROS              TO GFCTNU-COD-SQL-ERRO
JUN10          MOVE 0854               TO GFCTNU-COD-MSG-ERRO
               MOVE '0854'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESCRICAO-MSG
JUN10          MOVE GFCTG3-DESC-MSG    TO GFCTNU-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           PERFORM 2233-ACESSAR-PARM-GESTAO.

           PERFORM 2234-CONSISTIR-PARM-GESTAO.

      *----------------------------------------------------------------*
       2230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ACESSA POOL1570 PARA VALIDAR DATA                             *
      ******************************************************************
      *----------------------------------------------------------------*
       2231-VALIDAR-DATA               SECTION.
      *----------------------------------------------------------------*

0107       MOVE ZEROS                  TO RETURN-CODE.

           MOVE 09                     TO WRK-1570-FORMATO-DATA.

           CALL 'POOL1570'             USING WRK-1570-DATA-R
                                             WRK-1570-FORMATO-DATA
                                             WRK-1570-MENSAGEM.

0107       IF  RETURN-CODE             NOT EQUAL ZEROS AND 04
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0876'         TO GFCT0M-PROGRAMA
               MOVE 'POOL1570'         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
JUN10                                     GFCTNU-DESC-MSG-ERRO
JUN10          MOVE 9                  TO GFCTNU-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2231-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ACESSA O MODULO GFCT5018                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2232-ACESSAR-GFCT5018           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE  WRK-5018-AREA-SAIDA
                       WRK-5018-DADOS-RETORNO.

           MOVE 'O'                    TO WRK-5018-AMBIENTE.
JUN10      MOVE GFCTNR-COD-TARI-CONS(IND-1)
                                       TO WRK-5018-CODIGO-TARIFA.
JUN10      MOVE GFCTNR-INIC-VIG-FLEX   TO WRK-5018-DT-REFER.
           MOVE 'GFCT5018'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                             WRK-5018-AREA-ENTRADA
                                             WRK-5018-AREA-SAIDA
                                             WRK-5018-DADOS-RETORNO
                                             IO-PCB
                                             ALT-PCB.

           IF  WRK-5018-COD-RETORNO    NOT EQUAL ZEROS AND 02
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0876'         TO GFCT0M-PROGRAMA
               MOVE 'GFCT5018'         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
JUN10                                     GFCTNU-DESC-MSG-ERRO
JUN10          MOVE 9                  TO GFCTNU-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-5018-COD-RETORNO    EQUAL 02
JUN10          MOVE 1                  TO GFCTNU-ERRO
JUN10          MOVE ZEROS              TO GFCTNU-COD-SQL-ERRO
JUN10          MOVE 0054               TO GFCTNU-COD-MSG-ERRO
               MOVE '0054'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESCRICAO-MSG
JUN10          MOVE GFCTG3-DESC-MSG   TO GFCTNU-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2232-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ACESSA A TABELA GFCTB0A0                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2233-ACESSAR-PARM-GESTAO        SECTION.
      *----------------------------------------------------------------*

           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO DFIM-VGCIA-GSTOR
                                       OF GFCTB0A0.

           EXEC SQL
             SELECT
                   QMAX_FLEXZ,
                   QMAX_RTCAO,
                   QMAX_FRANQ_TARIF
             INTO
                   :GFCTB0A0.QMAX-FLEXZ,
                   :GFCTB0A0.QMAX-RTCAO,
                   :GFCTB0A0.QMAX-FRANQ-TARIF
             FROM   DB2PRD.PARM_CTRL_GSTAO
             WHERE
                   DINIC_VGCIA_GSTOR IN
                   (SELECT MAX(DINIC_VGCIA_GSTOR)
                   FROM DB2PRD.PARM_CTRL_GSTAO) AND
                   DFIM_VGCIA_GSTOR  >= :GFCTB0A0.DFIM-VGCIA-GSTOR
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE  '0010'            TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESCRICAO-MSG
               STRING GFCTG3-DESC-MSG ' - GFCTB0A0'
JUN10              DELIMITED BY '  '   INTO GFCTNU-DESC-MSG-ERRO
               MOVE 'GFCT0876'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'PARM_CTRL_GSTAO'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
JUN10                                     GFCTNU-COD-SQL-ERRO
               MOVE '0060'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
JUN10          MOVE  9                 TO GFCTNU-ERRO
JUN10          MOVE  10                TO GFCTNU-COD-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
JUN10          MOVE 1                  TO GFCTNU-ERRO
JUN10          MOVE ZEROS              TO GFCTNU-COD-SQL-ERRO
JUN10          MOVE 0752               TO GFCTNU-COD-MSG-ERRO
               MOVE '0752'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESCRICAO-MSG
JUN10          MOVE GFCTG3-DESC-MSG    TO GFCTNU-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2233-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  CONSISTE OS DADOS OBTIDOS NA TABELA GFCTB0A0                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2234-CONSISTIR-PARM-GESTAO      SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-DATA-INIC-FLEX     TO WRK-1280-DATA-INFERIOR.
           MOVE WRK-DATA-FIM-FLEX      TO WRK-1280-DATA-SUPERIOR.
           MOVE ZEROS                  TO WRK-1280-NRO-DIAS.

           CALL 'POOL1280'             USING WRK-1280-DATA-INFERIOR
                                             WRK-1280-DATA-SUPERIOR
                                             WRK-1280-NRO-DIAS
                                             WRK-1280-MENSAGEM.

           IF  RETURN-CODE             EQUAL 4 OR 8
JUN10          MOVE 1                  TO GFCTNU-ERRO
JUN10          MOVE ZEROS              TO GFCTNU-COD-SQL-ERRO
JUN10          MOVE 0628               TO GFCTNU-COD-MSG-ERRO
               MOVE '0628'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESCRICAO-MSG
JUN10          MOVE GFCTG3-DESC-MSG    TO GFCTNU-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-1280-NRO-DIAS       GREATER QMAX-FLEXZ
                                       OF GFCTB0A0
JUN10          MOVE 1                  TO GFCTNU-ERRO
JUN10          MOVE ZEROS              TO GFCTNU-COD-SQL-ERRO
JUN10          MOVE 0621               TO GFCTNU-COD-MSG-ERRO
               MOVE '0621'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESCRICAO-MSG
JUN10          MOVE GFCTG3-DESC-MSG    TO GFCTNU-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2234-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

2201  *----------------------------------------------------------------*
2201  * SELECAO NA TABELA GFCTB0I9                                     *
2201  *----------------------------------------------------------------*
2201   2235-ACESSAR-GFCTB0I9           SECTION.
2201  *----------------------------------------------------------------*
2201
JUN10      MOVE GFCTNR-COD-TARI-CONS(IND-1)
JUN10                                  TO CSERVC-TARIF OF GFCTB0I9.
2201
2201       EXEC SQL
2201           SELECT  DINIC_PRMSS_FLEXZ,
2201                   DFIM_PRMSS_FLEXZ,
2201                   CINDCD_PRMSS_AGNDA,
2201                   CINDCD_PRMSS_VLR,
2201                   CINDCD_PRMSS_PERC,
2201                   CINDCD_PRMSS_FRANQ
2201           INTO   :GFCTB0I9.DINIC-PRMSS-FLEXZ,
2201                  :GFCTB0I9.DFIM-PRMSS-FLEXZ,
2201                  :GFCTB0I9.CINDCD-PRMSS-AGNDA,
2201                  :GFCTB0I9.CINDCD-PRMSS-VLR,
2201                  :GFCTB0I9.CINDCD-PRMSS-PERC,
2201                  :GFCTB0I9.CINDCD-PRMSS-FRANQ
2201           FROM    DB2PRD.TPRMSS_FLEXZ_CTA
2201           WHERE   CSERVC_TARIF       = :GFCTB0I9.CSERVC-TARIF
2201       END-EXEC.
2201
2201       IF (SQLCODE                 NOT EQUAL ZEROS AND +100 AND
2201                                   -811) OR
2201          (SQLWARN0                EQUAL 'W'      )
2201           MOVE  '0010'            TO GFCTG2-COD-MSG
2201           PERFORM 1310-OBTER-DESCRICAO-MSG
2201           STRING GFCTG3-DESC-MSG ' - GFCTB0I9'
JUN10              DELIMITED BY '  '   INTO GFCTNU-DESC-MSG-ERRO
2201           MOVE 'GFCT0876'         TO GFCT0M-PROGRAMA
2201           MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
2201           MOVE 'TPRMSS_FLEXZ_CTA' TO GFCT0M-NOME-TAB
2201           MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
2201           MOVE  SQLCODE           TO GFCT0M-SQLCODE
JUN10                                     GFCTNU-COD-SQL-ERRO
2201           MOVE '0070'             TO GFCT0M-LOCAL
2201           MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
JUN10          MOVE  9                 TO GFCTNU-ERRO
JUN10          MOVE  10                TO GFCTNU-COD-MSG-ERRO
2201           PERFORM 3000-FINALIZAR
2201       END-IF.
2201
2201       IF  SQLCODE                 EQUAL +100
JUN10          MOVE 1                  TO GFCTNU-ERRO
JUN10          MOVE ZEROS              TO GFCTNU-COD-SQL-ERRO
JUN10          MOVE 1216               TO GFCTNU-COD-MSG-ERRO
2201           MOVE '1216'             TO GFCTG2-COD-MSG
2201           PERFORM 1310-OBTER-DESCRICAO-MSG
JUN10          MOVE GFCTG3-DESC-MSG    TO GFCTNU-DESC-MSG-ERRO
2201           PERFORM 3000-FINALIZAR
2201       END-IF.
2201
2201  *----------------------------------------------------------------*
2201   2235-99-FIM.                    EXIT.
2201  *----------------------------------------------------------------*


      ******************************************************************
      *  FINALIZA PROCESSAMENTO E RETORNA AO CHAMADOR                  *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

JUN10      IF  IND-1                   GREATER ZEROS AND
JUN10          IND-1                   LESS    21
JUN10          MOVE 1                  TO GFCTNU-COD-MSG-ERRO-TAR(IND-1)
JUN10      END-IF.
JUN10
           GOBACK.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
