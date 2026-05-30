      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT1550.
       AUTHOR.     DANIELLI MAGNABOSCO.
      *================================================================*
      *                    C P M - S I S T E M A S                     *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT1550                                    *
      *    PROGRAMADORA:   DANIELLI MAGNABOSCO     - CPM PATO BRANCO   *
      *    ANALISTA CPM:   ADRIANO SANTANA         - CPM PATO BRANCO   *
      *    ANALISTA....:   MARCELO CREMM           - PROCKWORK/ GP. 50 *
      *    DATA........:   28/12/2005                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   TRATAR CALCULO DE AGRUPAMENTO DE EMPRESA DO *
      *    DIA. ESTORNARA AS EMPRESAS SILICITADAS E SEUS FUNCIONARIOS  *
      *    TAMBEM.                                                     *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                       INCLUDE/BOOK      *
      *                    DB2PRD.PARM_DATA_PROCM      GFCTB0A1        *
      *                    DB2PRD.PDIDO_ESTRN_CLI      GFCTB0A6        *
      *                    DB2PRD.PDIDO_ESTRN_GRP      GFCTB0A7        *
      *                    DB2PRD.MOVTO_EVNTO_CRRTT    GFCTB092        *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                      INCLUDE/BOOK        *
      *                  CADEMPR                                       *
      *                  RELATO                         -              *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    POL7100C - AREA PARA TRATAMENTO DE ERRO PELA POOL7100       *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    POOL7600 - OBTEM DATA E HORA DO SISTEMA                     *
      *    POOL7100 - MODULO PARA CANCELAMENTO DO PROGRAMA             *
      *================================================================*
      *   ULTIMA ALTERACAO:                                            *
      *   =================                                            *
      *                                                                *
      *   MARCELO CREMM - PROCWORK - SETEMBRO-2006                     *
      *                                                                *
      *   ===> PASSAR A TRATAR TAMBEM CSIT-EVNTO-RECBD = 15, 16 E 17.  *
      *   ===> TRATAR TAMBEM OS RELATORIOS.                            *
      *                                                                *
      *================================================================*
080208*----------------------------------------------------------------*
080208*              SONDA PROCWORK - CONSULTORIA - ALTERACAO          *
080208*----------------------------------------------------------------*
080208*    ANALISTA....:   VINICIUS                         / GRUPO 50 *
080208*    DATA........:   08/02/2008                                  *
080208*    OBJETIVO....:   INCLUSAO DE TRATAMENTO PARA SITUACOES:      *
080208*                    18, 19 E NN.                                *
080208*----------------------------------------------------------------*

      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*

       FILE-CONTROL.

           SELECT CADEMPR   ASSIGN     TO UT-S-CADEMPR
           FILE STATUS                 IS WRK-FS-CADEMPR.

           SELECT RELATO   ASSIGN      TO UT-S-RELATO
           FILE STATUS                 IS WRK-FS-RELATO.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:  CADASTRO DE EMPRESAS CADASTRADAS NO CREC            *
      *            ORG. SEQUENCIAL     -   LRECL   =   070             *
      *----------------------------------------------------------------*

       FD  CADEMPR
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  REG-FUNCREC.
           03  CREC-AG-EMPR-PAG        PIC  9(005) COMP-3.
           03  CREC-CTA-EMPR-PAG       PIC  9(007) COMP-3.
           03  CREC-VLDEB-PAG          PIC  9(013)V99 COMP-3.
           03  CREC-NM-EMPR-PAGADORA   PIC  X(035).
           03  CREC-AG-CLI-FUNC        PIC  9(005) COMP-3.
           03  CREC-CTA-CLI-FUNC       PIC  9(007) COMP-3.
           03  CREC-VLCRED-FUNC        PIC  9(013)V99 COMP-3.
           03  FILLER                  PIC  X(005).

      *----------------------------------------------------------------*
      *    OUTPUT: RELATORIO TOTALIZADOR DE NAO CADASTRADOS            *
      *            ORG. SEQUENCIAL     -   LRECL   =   133             *
      *----------------------------------------------------------------*

       FD  RELATO
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  REG-RELATO                  PIC  X(133).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** GFCT1550 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA ACUMULADORES ***'.
      *----------------------------------------------------------------*

       77  ACU-PAGINAS              PIC  9(005)    COMP-3  VALUE ZEROS.
       77  ACU-LINHAS               PIC  9(002)            VALUE 60.
       77  ACU-OCORRENCIAS          PIC  9(009)    COMP-3  VALUE ZEROS.
       77  ACU-EMPRE-DESPRE         PIC  9(009)    COMP-3  VALUE ZEROS.
       77  ACU-LIDOS-CRS1           PIC  9(009)    COMP-3  VALUE ZEROS.
       77  ACU-QTD-ESTORNO          PIC  9(011)V99 COMP-3   VALUE ZEROS.
       77  ACU-VLR-ESTORNO          PIC  9(011)V99 COMP-3  VALUE ZEROS.
       77  ACU-QTD-CANCEL           PIC  9(011)V99 COMP-3  VALUE ZEROS.
       77  ACU-VLR-CANCEL           PIC  9(011)V99 COMP-3  VALUE ZEROS.
       77  ACU-QTD-ESTORNO-TOT      PIC  9(011)V99 COMP-3   VALUE ZEROS.
       77  ACU-VLR-ESTORNO-TOT      PIC  9(011)V99 COMP-3  VALUE ZEROS.
       77  ACU-QTD-CANCEL-TOT       PIC  9(011)V99 COMP-3  VALUE ZEROS.
       77  ACU-VLR-CANCEL-TOT       PIC  9(011)V99 COMP-3  VALUE ZEROS.
       77  WRK-MASCARA-1            PIC  ZZZZZZZZ9.
       77  WRK-MASCARA-2            PIC  Z.ZZZ.ZZZ.ZZ9,99.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA AUXILIARES ***'.
      *----------------------------------------------------------------*

       01  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.
       01  WRK-FIM-C1                  PIC  X(001)         VALUE SPACES.
       01  WRK-FIM-C2                  PIC  X(001)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA TESTES DE FILE STATUS ***'.
      *----------------------------------------------------------------*

       01  WRK-FS-RELATO               PIC  X(002)         VALUE SPACES.
       01  WRK-FS-CADEMPR              PIC  X(002)         VALUE SPACES.

       01  WRK-ABERTURA                PIC  X(013)         VALUE
           ' NA ABERTURA '.
       01  WRK-LEITURA                 PIC  X(013)         VALUE
           ' NA  LEITURA '.
       01  WRK-GRAVACAO                PIC  X(013)         VALUE
           ' NA GRAVACAO '.
       01  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** MENSAGEM DE ERRO DE FILE STATUS ***'.
      *----------------------------------------------------------------*

       01  WRK-MENSAGEM-ERRO.
           03  FILLER                  PIC  X(008)         VALUE
               '** ERRO '.
           03  WRK-OPERACAO            PIC  X(013)         VALUE SPACES.
           03  FILLER                  PIC  X(012)         VALUE
               ' DO ARQUIVO '.
           03  WRK-NOME-ARQ            PIC  X(008)         VALUE SPACES.
           03  FILLER                  PIC  X(017)         VALUE
               ' - FILE-STATUS = '.
           03  WRK-FILE-STATUS         PIC  X(002)         VALUE SPACES.
           03  FILLER                  PIC  X(003)         VALUE ' **'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA MENSAGENS ***'.
      *----------------------------------------------------------------*

       01  WRK-MSG01                   PIC  X(075)         VALUE
           'QUANTIDADE DE OCORRENCIAS ENVIADO PELO PARM, PARA COMMIT, IN
      -    'VALIDO'.
       01  WRK-MSG03                   PIC  X(075)         VALUE
           '** CANCELADO APOS O PRIMEIRO COMMIT **'.

       01  WRK-PARM-AREA.
           03  WRK-QTDE-COMMIT         PIC  9(007)         VALUE ZEROS.

       01  WRK-CHAVE-CRS1.
           03  WRK-CAG-EMPR            PIC  9(005)         VALUE ZEROS.
           03  WRK-CTA-DEB             PIC  9(013)         VALUE ZEROS.

       01  WRK-CHAVE-EMPRESA.
           03  WRK-AGEN-PAG            PIC  9(005)         VALUE ZEROS.
           03  WRK-CTA-PAG             PIC  9(013)         VALUE ZEROS.

AQUI   01  W-CH-ANT-EMPRESA.
           03  WRK-AGEN-PAG-ANT        PIC  9(005)         VALUE ZEROS.
           03  WRK-CTA-PAG-ANT         PIC  9(013)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA POOL7600 ***'.
      *----------------------------------------------------------------*

       01  WRK-DATA-HORA.
           03  WRK-DATA-JULIANA        PIC  9(005) COMP-3  VALUE ZEROS.
           03  WRK-DATA-AAMMDD         PIC  9(007) COMP-3  VALUE ZEROS.
           03  WRK-DATA-AAAAMMDD       PIC  9(009) COMP-3  VALUE ZEROS.
           03  WRK-HORA-HHMMSS         PIC  9(007) COMP-3  VALUE ZEROS.
           03  WRK-HORA-HHMMSSMMMMMM   PIC  9(013) COMP-3  VALUE ZEROS.
           03  WRK-TIMESTAMP.
               05  WRK-ANO             PIC  9(004)         VALUE ZEROS.
               05  WRK-MES             PIC  9(002)         VALUE ZEROS.
               05  WRK-DIA             PIC  9(002)         VALUE ZEROS.
               05  WRK-HOR             PIC  9(002)         VALUE ZEROS.
               05  WRK-MIN             PIC  9(002)         VALUE ZEROS.
               05  WRK-SEG             PIC  9(002)         VALUE ZEROS.
               05  WRK-MIL             PIC  9(006)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA CABECALHOS ***'.
      *----------------------------------------------------------------*

       01  CABEC1.
           03  FILLER                  PIC  X(001)         VALUE '1'.
           03  CB1-DIA                 PIC  99.
           03  FILLER                  PIC  X(001)         VALUE '/'.
           03  CB1-MES                 PIC  99.
           03  FILLER                  PIC  X(001)         VALUE '/'.
           03  CB1-ANO                 PIC  9999.
           03  FILLER                  PIC  X(040)         VALUE SPACES.
           03  FILLER                  PIC  X(011)         VALUE
               'B A N C O'.
           03  FILLER                  PIC  X(017)         VALUE
               'B R A D E S C O'.
           03  FILLER                  PIC  X(043)         VALUE 'S/A'.
           03  CB1-HOR                 PIC  99.
           03  FILLER                  PIC  X(001)         VALUE ':'.
           03  CB1-MIN                 PIC  99.
           03  FILLER                  PIC  X(001)         VALUE ':'.
           03  CB1-SEG                 PIC  99.

       01  CABEC2.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(036)         VALUE
               'GFCT1550'.
           03  FILLER                  PIC  X(045)         VALUE
               'GFCT - GESTAO, FLEXIBILIZACAO E COBRANCA DE'.
           03  FILLER                  PIC  X(040)         VALUE
               'TARIFAS'.
           03  FILLER                  PIC  X(004)         VALUE 'PAG.'.
           03  CB2-PAGINA              PIC  ZZ99.

       01  CABEC3.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(025)         VALUE SPACES.
           03  FILLER                  PIC  X(054)         VALUE
               'RELATORIO DE MOVIMENTO DIARIO DE EMPRESAS DESPREZADAS '.
           03  FILLER                  PIC  X(026)         VALUE
               '- CALCULO POR AGRUPAMENTO.'.

       01  CABEC4.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(131)         VALUE ALL
               '-'.

       01  CABEC5.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(053)         VALUE SPACES.
           03  FILLER                  PIC  X(013)         VALUE
               'MOVIMENTO DE '.
           03  CB5-DIA-MOV             PIC  99.
           03  FILLER                  PIC  X(001)         VALUE '/'.
           03  CB5-MES-MOV             PIC  99.
           03  FILLER                  PIC  X(001)         VALUE '/'.
           03  CB5-ANO-MOV             PIC  9999.

       01  CABEC6.
           03  FILLER                  PIC  X(001)         VALUE '-'.
           03  FILLER                  PIC  X(009)         VALUE
               ' EMPRESAS'.

       01  CABEC7.
           03  FILLER                  PIC  X(001)         VALUE SPACES.

       01  CABEC8.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(015)         VALUE
               ' AGENCIA:'.
           03  FILLER                  PIC  X(018)         VALUE
               'CONTA:'.
           03  FILLER                  PIC  X(007)         VALUE
               'MOTIVO:'.

       01  CABEC9.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(015)         VALUE
               ' ========'.
           03  FILLER                  PIC  X(013)         VALUE
               ALL '='.
           03  FILLER                  PIC  X(005)         VALUE SPACES.
           03  FILLER                  PIC  X(060)         VALUE
               ALL '='.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA LINHAS DE DETALHE ***'.
      *----------------------------------------------------------------*

       01  LINDET1.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(002)         VALUE SPACES.
           03  LD1-AGENCIA             PIC  ZZZZ9.
           03  FILLER                  PIC  X(008)         VALUE SPACES.
           03  LD1-CONTA               PIC  ZZZZZZZZZZZZ9.
           03  FILLER                  PIC  X(005)         VALUE '  -'.
           03  FILLER                  PIC  X(054)         VALUE
               'DESPREZADA POR NAO HAVER RELACIONAMENTO COM ARQUIVO DO'.
           03  FILLER                  PIC  X(006)         VALUE
               ' CREC.'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA LINHAS DE TOTAIS ***'.
      *----------------------------------------------------------------*

       01  LINTOT1.
           03  FILLER                  PIC  X(001)         VALUE '-'.
           03  FILLER                  PIC  X(016)         VALUE
               '  TOTAL GERAL:'.
           03  LT1-EMPRE-DESPRE        PIC  ZZZZZZ9.
           03  FILLER                  PIC  X(022)         VALUE
               ' EMPRESAS DESPREZADAS.'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE DB2 ***'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0A1
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB044
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0A6
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0A7
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB092
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA DECLARACAO DE CURSOR ***'.
      *----------------------------------------------------------------*

           EXEC SQL
             DECLARE   CRS1-GFCTB0A7  CURSOR  WITH HOLD FOR
               SELECT
                   A7.CAGPTO_CTA,
                   A7.CSERVC_TARIF,
                   A7.DOCOR_EVNTO,
                   A7.HIDTFD_ESTRN_AGPTO,
                   A7.CSIT_SOLTC_ESTRN,
                   A7.CSIT_PROCM_ESTRN,
                   A7.CINDCD_AGPTO_TOT,
                   A7.CDEPDC_SOLTC_ESTRN,
                   A7.CFUNC_SOLTC_ESTRN,
                   A7.HSOLTC_ESTRN,
                   A7.CTPO_OPER_MOTVO,
                   A7.CMOTVO_JUSTF,
                   A7.RJUSTF_SOLTC_ESTRN,
                   A7.CDEPDC_PAREC_ESTRN,
                   A7.CFUNC_PAREC_ESTRN,
                   A7.HPAREC_ESTRN,
                   A7.RJUSTF_PAREC_ESTRN,
                   A7.QESTRN_AGPTO,
                   A7.VESTRN_SOLTD,
                   A7.QCANCT_AGPTO,
                   A7.VCANCT_SOLTD,
                   A7.DEFETV_CREDT_ESTRN,
                   A6.CAGPTO_CTA,
                   A6.CSERVC_TARIF,
                   A6.DOCOR_EVNTO,
                   A6.HIDTFD_ESTRN_AGPTO,
                   A6.CAG_EMPR,
                   A6.CRZ_CTA_EMPR,
                   A6.CNRO_CTA_DEB,
                   A6.CLCTO_CTA_EMPR
             FROM  DB2PRD.PDIDO_ESTRN_GRP A7,
                   DB2PRD.PDIDO_ESTRN_EMPR A6
             WHERE
                 A7.CAGPTO_CTA        = A6.CAGPTO_CTA                AND
                 A7.CSERVC_TARIF      = A6.CSERVC_TARIF              AND
                 A7.DOCOR_EVNTO       = A6.DOCOR_EVNTO               AND
                 A7.HIDTFD_ESTRN_AGPTO = A6.HIDTFD_ESTRN_AGPTO  AND
                 A7.CAGPTO_CTA        = 16                           AND
                 A7.CSIT_SOLTC_ESTRN  = 1                            AND
                 A7.CSIT_PROCM_ESTRN  = 2
             ORDER BY
               A6.CAG_EMPR,
               A6.CNRO_CTA_DEB,
               A6.CSERVC_TARIF,
               A6.DOCOR_EVNTO
           END-EXEC.

           EXEC SQL
             DECLARE   CRS2-GFCTB092 CURSOR  WITH HOLD FOR
               SELECT
                   DENVIO_MOVTO_TARIF,
                   CROTNA_ORIGE_MOVTO,
                   CNRO_ARQ_MOVTO,
                   CSEQ_MOVTO,
                   CSERVC_TARIF,
                   CMIDIA_SERVC_MOVTO,
                   CPERIF_SERVC_MOVTO,
                   QEVNTO_AGRUP,
                   HPREST_SERVC_MOVTO,
                   CTPO_CTA_MOVTO,
                   CBCO_DSTNO_MOVTO,
                   CAG_DSTNO_MOVTO,
                   CCTA_DSTNO_MOVTO,
                   CPAB_DSTNO_MOVTO,
                   CAG_CTLZA_DEB,
                   CCTA_CTLZA_DEB,
                   VTARIF_BRUTO_MOVTO,
                   VTARIF_LIQ_MOVTO,
                   VTARIF_DEB_MOVTO,
                   DAGNDA_DEB_MOVTO,
                   DEFETV_DEB_MOVTO,
                   QMAX_TENTV_DEB,
                   QTENTV_DEB_MOVTO,
                   CSIT_EVNTO_RECBD,
                   CIDTFD_PGMC_COBR,
                   DOCOR_EVNTO,
                   VTARIF_CADTR_SIST
             FROM  DB2PRD.MOVTO_EVNTO_CRRTT
             WHERE
                   CSERVC_TARIF       = :GFCTB092.CSERVC-TARIF       AND
                   DOCOR_EVNTO        = :GFCTB092.DOCOR-EVNTO        AND
                   CAG_DSTNO_MOVTO    = :GFCTB092.CAG-DSTNO-MOVTO    AND
                   CCTA_DSTNO_MOVTO   = :GFCTB092.CCTA-DSTNO-MOVTO
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA POOL7100 ***'.
      *----------------------------------------------------------------*

       COPY POL7100C.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** GFCT1550 - FIM DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  LKG-AREA-PARM.
           05  LKG-TAMA-PARM           PIC S9(004) COMP.
           05  LKG-QTDE-COMMIT         PIC  9(007).

      *================================================================*
       PROCEDURE                       DIVISION USING LKG-AREA-PARM.
      *================================================================*

      *----------------------------------------------------------------*
       000000-INICIAR                  SECTION.
      *----------------------------------------------------------------*

           IF  LKG-QTDE-COMMIT         NOT NUMERIC OR
               LKG-QTDE-COMMIT         EQUAL ZEROS
               DISPLAY '*************** GFCT1550 ****************'
               DISPLAY '*                                       *'
               DISPLAY '*    QUANTIDADE DE OCORRENCIAS ENVIADO  *'
               DISPLAY '*    PELO PARM, PARA COMMIT, INVALIDO.  *'
               DISPLAY '*                                       *'
               DISPLAY '*    QUANTIDADE: ' WRK-QTDE-COMMIT
                                               '                *'
               DISPLAY '*                                       *'
               DISPLAY '*          PROGRAMA CANCELADO!          *'
               DISPLAY '*                                       *'
               DISPLAY '*************** GFCT1550 ****************'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-MSG01          TO ERR-TEXTO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE LKG-QTDE-COMMIT        TO WRK-QTDE-COMMIT.

           PERFORM 100000-INICIALIZAR.

           PERFORM 200000-VERIFICAR-VAZIO.

           PERFORM 300000-PROCESSAR    UNTIL
                   WRK-FIM-C1          EQUAL 'S'.

           PERFORM 400000-FECHAR-CURSOR1.

           PERFORM 500000-FINALIZAR.

      *----------------------------------------------------------------*
       000000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       100000-INICIALIZAR              SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL1050'.

           OPEN INPUT  CADEMPR
                OUTPUT RELATO.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 110000-TESTAR-FILE-STATUS.

           PERFORM 120000-ACESSAR-GFCTB0A1.

           CALL 'POOL7600'             USING WRK-DATA-HORA.

           MOVE WRK-DIA                TO CB1-DIA
                                          CB5-DIA-MOV.
           MOVE WRK-MES                TO CB1-MES
                                          CB5-MES-MOV.
           MOVE WRK-ANO                TO CB1-ANO
                                          CB5-ANO-MOV.
           MOVE WRK-HOR                TO CB1-HOR.
           MOVE WRK-MIN                TO CB1-MIN.
           MOVE WRK-SEG                TO CB1-SEG.

      *----------------------------------------------------------------*
       100000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       110000-TESTAR-FILE-STATUS       SECTION.
      *----------------------------------------------------------------*

           PERFORM 111000-TESTAR-FS-CADEMPR.

           PERFORM 112000-TESTAR-FS-RELATO.

      *----------------------------------------------------------------*
       110000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       111000-TESTAR-FS-CADEMPR        SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-CADEMPR          NOT EQUAL '00'
               MOVE 'CADEMPR '         TO WRK-NOME-ARQ
               MOVE WRK-FS-CADEMPR     TO WRK-FILE-STATUS
               MOVE WRK-MENSAGEM-ERRO  TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       111000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       112000-TESTAR-FS-RELATO         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-RELATO           NOT EQUAL '00'
               MOVE 'RELATO  '         TO WRK-NOME-ARQ
               MOVE WRK-FS-RELATO      TO WRK-FILE-STATUS
               MOVE WRK-MENSAGEM-ERRO  TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       112000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       120000-ACESSAR-GFCTB0A1         SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC OF GFCTB0A1.

           EXEC SQL
             SELECT CSIST_PRINC,
                    DPROCM_ANTER,
                    DPROCM_ATUAL,
                    DPROCM_PROX
               INTO :GFCTB0A1.CSIST-PRINC,
                    :GFCTB0A1.DPROCM-ANTER,
                    :GFCTB0A1.DPROCM-ATUAL,
                    :GFCTB0A1.DPROCM-PROX
               FROM DB2PRD.PARM_DATA_PROCM
              WHERE CSIST_PRINC        = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PARM_DATA_PROCM ' TO ERR-DBD-TAB
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0010'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       120000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       200000-VERIFICAR-VAZIO          SECTION.
      *----------------------------------------------------------------*

           PERFORM 210000-LER-CADEMPR.

           IF  WRK-FS-CADEMPR          EQUAL '10'
               DISPLAY '************** GFCT1550 **************'
               DISPLAY '*                                    *'
               DISPLAY '*      ARQUIVO CADEMPR VAZIO        *'
               DISPLAY '*       PROCESSAMENTO ENCERRADO      *'
               DISPLAY '*                                    *'
               DISPLAY '************** GFCT1550 **************'
               PERFORM 500000-FINALIZAR
           END-IF.

           PERFORM 220000-ABRIR-CURSOR1.

           PERFORM 230000-LER-CURSOR1.

           IF  WRK-FIM-C1              EQUAL 'S'
               DISPLAY '************** GFCT1550 **************'
               DISPLAY '*                                    *'
               DISPLAY '*        NAO HA CALCULOS POR         *'
               DISPLAY '*      AGRUPAMENTO EMPRESA PARA      *'
               DISPLAY '*       MOVIMENTO: '
                           DPROCM-ATUAL OF GFCTB0A1 '        *'
               DISPLAY '*                                    *'
               DISPLAY '************** GFCT1550 **************'
           END-IF.

      *----------------------------------------------------------------*
       200000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       210000-LER-CADEMPR              SECTION.
      *----------------------------------------------------------------*

           READ CADEMPR.

           IF  WRK-FS-CADEMPR           EQUAL '10'
               MOVE HIGH-VALUES         TO WRK-CHAVE-EMPRESA
               GO TO 210000-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 111000-TESTAR-FS-CADEMPR.

           MOVE CREC-AG-EMPR-PAG       TO WRK-AGEN-PAG.
           MOVE CREC-CTA-EMPR-PAG      TO WRK-CTA-PAG.

      *----------------------------------------------------------------*
       210000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       211000-EMITIR-DISPLAY           SECTION.
      *----------------------------------------------------------------*

           DISPLAY '************** GFCT1550 **************'.
           DISPLAY '*                                    *'.
           DISPLAY '*      FIM DO  ARQUIVO CADEMPR      *'.
           DISPLAY '*     SEM TERMINAR PROCESSAMENTO     *'.
           DISPLAY '*                                    *'.
           DISPLAY '************** GFCT1550 **************'.

      *----------------------------------------------------------------*
       211000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       220000-ABRIR-CURSOR1            SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FIM-C1.

           EXEC SQL
               OPEN CRS1-GFCTB0A7
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PDIDO_ESTRN_GRP ' TO ERR-DBD-TAB
               MOVE 'OPEN    '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0030'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       220000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       230000-LER-CURSOR1              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
              FETCH CRS1-GFCTB0A7
               INTO
                   :GFCTB0A7.CAGPTO-CTA,
                   :GFCTB0A7.CSERVC-TARIF,
                   :GFCTB0A7.DOCOR-EVNTO,
                   :GFCTB0A7.HIDTFD-ESTRN-AGPTO,
                   :GFCTB0A7.CSIT-SOLTC-ESTRN,
                   :GFCTB0A7.CSIT-PROCM-ESTRN,
                   :GFCTB0A7.CINDCD-AGPTO-TOT,
                   :GFCTB0A7.CDEPDC-SOLTC-ESTRN,
                   :GFCTB0A7.CFUNC-SOLTC-ESTRN,
                   :GFCTB0A7.HSOLTC-ESTRN,
                   :GFCTB0A7.CTPO-OPER-MOTVO,
                   :GFCTB0A7.CMOTVO-JUSTF,
                   :GFCTB0A7.RJUSTF-SOLTC-ESTRN,
                   :GFCTB0A7.CDEPDC-PAREC-ESTRN,
                   :GFCTB0A7.CFUNC-PAREC-ESTRN,
                   :GFCTB0A7.HPAREC-ESTRN,
                   :GFCTB0A7.RJUSTF-PAREC-ESTRN,
                   :GFCTB0A7.QESTRN-AGPTO,
                   :GFCTB0A7.VESTRN-SOLTD,
                   :GFCTB0A7.QCANCT-AGPTO,
                   :GFCTB0A7.VCANCT-SOLTD,
                   :GFCTB0A7.DEFETV-CREDT-ESTRN,
                   :GFCTB0A6.CAGPTO-CTA,
                   :GFCTB0A6.CSERVC-TARIF,
                   :GFCTB0A6.DOCOR-EVNTO,
                   :GFCTB0A6.HIDTFD-ESTRN-AGPTO,
                   :GFCTB0A6.CAG-EMPR,
                   :GFCTB0A6.CRZ-CTA-EMPR,
                   :GFCTB0A6.CNRO-CTA-DEB,
                   :GFCTB0A6.CLCTO-CTA-EMPR
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'               )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PDIDO_ESTRN_GRP ' TO ERR-DBD-TAB
               MOVE 'FETCH   '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0040'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-C1
           ELSE
               ADD 1                   TO ACU-LIDOS-CRS1
               MOVE CAG-EMPR           OF GFCTB0A6
                                       TO WRK-CAG-EMPR
               MOVE CNRO-CTA-DEB       OF GFCTB0A6
                                       TO WRK-CTA-DEB
           END-IF.

      *----------------------------------------------------------------*
       230000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       300000-PROCESSAR                SECTION.
      *----------------------------------------------------------------*

           IF  WRK-CHAVE-EMPRESA               EQUAL WRK-CHAVE-CRS1
               PERFORM 310000-PROCESSAR-EMPRESA
               MOVE WRK-CHAVE-EMPRESA    TO W-CH-ANT-EMPRESA
               PERFORM 320000-PROCESSAR-FUNC UNTIL
                   WRK-CHAVE-EMPRESA           NOT EQUAL WRK-CHAVE-CRS1
               MOVE 3                          TO CSIT-PROCM-ESTRN
                                                           OF GFCTB0A7
               MOVE 1                          TO CSIT-SOLTC-ESTRN
                                                           OF GFCTB0A7
               PERFORM 33000-EXECUTAR-CARIMBA-A7
           ELSE
               IF  WRK-CHAVE-EMPRESA           LESS WRK-CHAVE-CRS1
                   PERFORM 210000-LER-CADEMPR  UNTIL
                       WRK-CHAVE-EMPRESA       NOT LESS WRK-CHAVE-CRS1
                   IF  WRK-CHAVE-EMPRESA       GREATER WRK-CHAVE-CRS1
                       PERFORM 340000-EMITIR-RELATORIO
                       MOVE 3                  TO CSIT-PROCM-ESTRN
                                                           OF GFCTB0A7
                       MOVE 1                  TO CSIT-SOLTC-ESTRN
                                                           OF GFCTB0A7
                       PERFORM 33000-EXECUTAR-CARIMBA-A7
                   ELSE
                       PERFORM 310000-PROCESSAR-EMPRESA
                       MOVE WRK-CHAVE-EMPRESA  TO W-CH-ANT-EMPRESA
                       PERFORM 320000-PROCESSAR-FUNC UNTIL
                       WRK-CHAVE-EMPRESA       NOT EQUAL WRK-CHAVE-CRS1
                       MOVE 3                  TO CSIT-PROCM-ESTRN
                                                           OF GFCTB0A7
                       MOVE 1                  TO CSIT-SOLTC-ESTRN
                                                           OF GFCTB0A7
                       PERFORM 33000-EXECUTAR-CARIMBA-A7
                   END-IF
               ELSE
                   PERFORM 340000-EMITIR-RELATORIO
                   MOVE 3                      TO CSIT-PROCM-ESTRN
                                                           OF GFCTB0A7
                   MOVE 1                      TO CSIT-SOLTC-ESTRN
                                                           OF GFCTB0A7
                   PERFORM 33000-EXECUTAR-CARIMBA-A7
               END-IF
           END-IF.

           IF  WRK-FIM-C1                      NOT EQUAL 'S'
               PERFORM 230000-LER-CURSOR1
           END-IF.

           IF  WRK-FIM-C1                      NOT EQUAL 'S' AND
               W-CH-ANT-EMPRESA                EQUAL WRK-CHAVE-CRS1
               PERFORM 350000-REABRIR-ARQUIVO
               PERFORM 210000-LER-CADEMPR      UNTIL
                       WRK-CHAVE-EMPRESA       NOT LESS WRK-CHAVE-CRS1
           END-IF.

           IF  ACU-OCORRENCIAS                 EQUAL WRK-QTDE-COMMIT
               PERFORM 360000-EXECUTAR-COMMIT
               MOVE ZEROS                      TO ACU-OCORRENCIAS
           END-IF.

      *----------------------------------------------------------------*
       300000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       310000-PROCESSAR-EMPRESA        SECTION.
      *----------------------------------------------------------------*

           MOVE CREC-AG-EMPR-PAG       TO CAG-DSTNO-MOVTO  OF GFCTB092.
           MOVE CREC-CTA-EMPR-PAG      TO CCTA-DSTNO-MOVTO OF GFCTB092.

           PERFORM 331000-ABRIR-CURSOR2.

           PERFORM 332000-LER-CURSOR2.

           PERFORM 333000-PROCESSAR-C2 UNTIL
               WRK-FIM-C2              EQUAL 'S'.

           PERFORM 334000-FECHAR-CURSOR2.

      *----------------------------------------------------------------*
       310000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       331000-ABRIR-CURSOR2            SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FIM-C2.
           MOVE CSERVC-TARIF           OF GFCTB0A7
                                       TO CSERVC-TARIF     OF GFCTB092.
           MOVE DOCOR-EVNTO            OF GFCTB0A7
                                       TO DOCOR-EVNTO      OF GFCTB092.

           EXEC SQL
               OPEN CRS2-GFCTB092
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'MOVTO_EVNTO_CRRTT'  TO ERR-DBD-TAB
               MOVE 'OPEN    '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0050'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       331000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       332000-LER-CURSOR2              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH   CRS2-GFCTB092  INTO
                   :GFCTB092.DENVIO-MOVTO-TARIF,
                   :GFCTB092.CROTNA-ORIGE-MOVTO,
                   :GFCTB092.CNRO-ARQ-MOVTO,
                   :GFCTB092.CSEQ-MOVTO,
                   :GFCTB092.CSERVC-TARIF,
                   :GFCTB092.CMIDIA-SERVC-MOVTO,
                   :GFCTB092.CPERIF-SERVC-MOVTO,
                   :GFCTB092.QEVNTO-AGRUP,
                   :GFCTB092.HPREST-SERVC-MOVTO,
                   :GFCTB092.CTPO-CTA-MOVTO,
                   :GFCTB092.CBCO-DSTNO-MOVTO,
                   :GFCTB092.CAG-DSTNO-MOVTO,
                   :GFCTB092.CCTA-DSTNO-MOVTO,
                   :GFCTB092.CPAB-DSTNO-MOVTO,
                   :GFCTB092.CAG-CTLZA-DEB,
                   :GFCTB092.CCTA-CTLZA-DEB,
                   :GFCTB092.VTARIF-BRUTO-MOVTO,
                   :GFCTB092.VTARIF-LIQ-MOVTO,
                   :GFCTB092.VTARIF-DEB-MOVTO,
                   :GFCTB092.DAGNDA-DEB-MOVTO,
                   :GFCTB092.DEFETV-DEB-MOVTO,
                   :GFCTB092.QMAX-TENTV-DEB,
                   :GFCTB092.QTENTV-DEB-MOVTO,
                   :GFCTB092.CSIT-EVNTO-RECBD,
                   :GFCTB092.CIDTFD-PGMC-COBR,
                   :GFCTB092.DOCOR-EVNTO,
                   :GFCTB092.VTARIF-CADTR-SIST
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'MOVTO_EVNTO_CRRTT' TO ERR-DBD-TAB
               MOVE 'FETCH   '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE  '0060'            TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-C2
           END-IF.

      *----------------------------------------------------------------*
       332000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       333000-PROCESSAR-C2             SECTION.
      *----------------------------------------------------------------*

080208     IF          CSIT-EVNTO-RECBD    OF GFCTB092
080208                         EQUAL 4 OR 5 OR 6 OR 10
080208         IF          CSIT-EVNTO-RECBD OF GFCTB092
080208                         EQUAL 6
080208             PERFORM 333100-OBTER-VLR-ESTORNADO
080208             COMPUTE VTARIF-DEB-MOVTO        OF GFCTB092
080208                         = (VTARIF-DEB-MOVTO OF GFCTB092
080208                         -  VRTEIO-VLR-SOLTD OF GFCTB044)
080208         END-IF
080208         ADD 1                       TO ACU-QTD-ESTORNO
080208         ADD VTARIF-DEB-MOVTO OF GFCTB092
080208                                     TO ACU-VLR-ESTORNO
080208     ELSE
080208         IF CSIT-EVNTO-RECBD     OF GFCTB092
080208                                 EQUAL 7 OR 8 OR 13
080208             NEXT SENTENCE
080208         ELSE
080208             ADD 1               TO ACU-QTD-CANCEL
080208             ADD VTARIF-BRUTO-MOVTO  OF GFCTB092
080208                                 TO ACU-VLR-CANCEL
080208     END-IF.

           PERFORM 332000-LER-CURSOR2.

      *----------------------------------------------------------------*
       333000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       333100-OBTER-VLR-ESTORNADO      SECTION.
      *----------------------------------------------------------------*
           MOVE DENVIO-MOVTO-TARIF     OF GFCTB092
                                       TO DENVIO-MOVTO-TARIF
                                       OF GFCTB044.
           MOVE CROTNA-ORIGE-MOVTO     OF GFCTB092
                                       TO CROTNA-ORIGE-MOVTO
                                       OF GFCTB044.
           MOVE CNRO-ARQ-MOVTO         OF GFCTB092
                                       TO CNRO-ARQ-MOVTO OF GFCTB044.
           MOVE CSEQ-MOVTO             OF GFCTB092
                                       TO CSEQ-MOVTO     OF GFCTB044.
           EXEC SQL
             SELECT  SUM(VRTEIO_VLR_SOLTD)
               INTO  :GFCTB044.VRTEIO-VLR-SOLTD
               FROM  DB2PRD.ESTRN_COM_REG
              WHERE  DENVIO_MOVTO_TARIF = :GFCTB044.DENVIO-MOVTO-TARIF
                AND  CROTNA_ORIGE_MOVTO = :GFCTB044.CROTNA-ORIGE-MOVTO
                AND  CNRO_ARQ_MOVTO     = :GFCTB044.CNRO-ARQ-MOVTO
                AND  CSEQ_MOVTO         = :GFCTB044.CSEQ-MOVTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'ESTRN_COM_REG    ' TO ERR-DBD-TAB
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE  '0071'            TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       333100-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       334000-FECHAR-CURSOR2           SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE   CRS2-GFCTB092
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'MOVTO_EVNTO_CRRTT' TO ERR-DBD-TAB
               MOVE 'CLOSE   '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE  '0070'            TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       334000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       320000-PROCESSAR-FUNC           SECTION.
      *----------------------------------------------------------------*

           MOVE CREC-AG-CLI-FUNC       TO CAG-DSTNO-MOVTO  OF GFCTB092.
           MOVE CREC-CTA-CLI-FUNC      TO CCTA-DSTNO-MOVTO OF GFCTB092.

           PERFORM 331000-ABRIR-CURSOR2.

           PERFORM 332000-LER-CURSOR2.

           PERFORM 333000-PROCESSAR-C2 UNTIL
               WRK-FIM-C2              EQUAL 'S'.

           PERFORM 334000-FECHAR-CURSOR2.

           PERFORM 210000-LER-CADEMPR.

      *----------------------------------------------------------------*
       320000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       33000-EXECUTAR-CARIMBA-A7       SECTION.
      *----------------------------------------------------------------*

           MOVE ACU-QTD-ESTORNO        TO QESTRN-AGPTO     OF GFCTB0A7.
           MOVE ACU-VLR-ESTORNO        TO VESTRN-SOLTD     OF GFCTB0A7.
           MOVE ACU-QTD-CANCEL         TO QCANCT-AGPTO     OF GFCTB0A7.
           MOVE ACU-VLR-CANCEL         TO VCANCT-SOLTD     OF GFCTB0A7.
           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO DEFETV-CREDT-ESTRN
                                                           OF GFCTB0A7.

           EXEC SQL
             UPDATE   DB2PRD.PDIDO_ESTRN_GRP
               SET
                   CSIT_SOLTC_ESTRN   = :GFCTB0A7.CSIT-SOLTC-ESTRN,
                   CSIT_PROCM_ESTRN   = :GFCTB0A7.CSIT-PROCM-ESTRN,
                   QESTRN_AGPTO       = :GFCTB0A7.QESTRN-AGPTO,
                   VESTRN_SOLTD       = :GFCTB0A7.VESTRN-SOLTD,
                   QCANCT_AGPTO       = :GFCTB0A7.QCANCT-AGPTO,
                   VCANCT_SOLTD       = :GFCTB0A7.VCANCT-SOLTD,
                   DEFETV_CREDT_ESTRN = :GFCTB0A7.DEFETV-CREDT-ESTRN
             WHERE
                   CAGPTO_CTA         = :GFCTB0A7.CAGPTO-CTA         AND
                   CSERVC_TARIF       = :GFCTB0A7.CSERVC-TARIF       AND
                   DOCOR_EVNTO        = :GFCTB0A7.DOCOR-EVNTO        AND
                   HIDTFD_ESTRN_AGPTO = :GFCTB0A7.HIDTFD-ESTRN-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PDIDO_ESTRN_GRP ' TO ERR-DBD-TAB
               MOVE 'UPDATE  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE  '0080'            TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

           ADD 1                       TO ACU-OCORRENCIAS.

           ADD  ACU-QTD-ESTORNO        TO ACU-QTD-ESTORNO-TOT
           ADD  ACU-VLR-ESTORNO        TO ACU-VLR-ESTORNO-TOT
           ADD  ACU-QTD-CANCEL         TO ACU-QTD-CANCEL-TOT
           ADD  ACU-VLR-CANCEL         TO ACU-VLR-CANCEL-TOT

           MOVE ZEROS                  TO ACU-QTD-ESTORNO
                                          ACU-VLR-ESTORNO
                                          ACU-QTD-CANCEL
                                          ACU-VLR-CANCEL.

      *----------------------------------------------------------------*
       330000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       340000-EMITIR-RELATORIO         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           IF  ACU-LINHAS              GREATER 59
               PERFORM 341000-IMPRIMI-CABECALHO
           END-IF.

           MOVE WRK-CAG-EMPR           TO LD1-AGENCIA.
           MOVE WRK-CTA-DEB            TO LD1-CONTA.

           WRITE REG-RELATO            FROM LINDET1.
           PERFORM 112000-TESTAR-FS-RELATO.

           ADD 1                       TO ACU-LINHAS
                                          ACU-EMPRE-DESPRE.

      *----------------------------------------------------------------*
       340000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       341000-IMPRIMI-CABECALHO        SECTION.
      *----------------------------------------------------------------*

           ADD 1                       TO ACU-PAGINAS.

           MOVE ACU-PAGINAS            TO CB2-PAGINA.

           WRITE REG-RELATO            FROM CABEC1.
           PERFORM 112000-TESTAR-FS-RELATO.

           WRITE REG-RELATO            FROM CABEC2.
           PERFORM 112000-TESTAR-FS-RELATO.

           WRITE REG-RELATO            FROM CABEC3.
           PERFORM 112000-TESTAR-FS-RELATO.

           WRITE REG-RELATO            FROM CABEC4.
           PERFORM 112000-TESTAR-FS-RELATO.

           WRITE REG-RELATO            FROM CABEC5.
           PERFORM 112000-TESTAR-FS-RELATO.

           WRITE REG-RELATO            FROM CABEC4.
           PERFORM 112000-TESTAR-FS-RELATO.

           WRITE REG-RELATO            FROM CABEC6.
           PERFORM 112000-TESTAR-FS-RELATO.

           WRITE REG-RELATO            FROM CABEC7.
           PERFORM 112000-TESTAR-FS-RELATO.

           WRITE REG-RELATO            FROM CABEC8.
           PERFORM 112000-TESTAR-FS-RELATO.

           WRITE REG-RELATO            FROM CABEC9.
           PERFORM 112000-TESTAR-FS-RELATO.

           MOVE 12                     TO ACU-LINHAS.

      *----------------------------------------------------------------*
       341000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       350000-REABRIR-ARQUIVO          SECTION.
      *----------------------------------------------------------------*

           CLOSE CADEMPR.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 111000-TESTAR-FS-CADEMPR.

           OPEN INPUT CADEMPR.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 111000-TESTAR-FS-CADEMPR.

           PERFORM 210000-LER-CADEMPR.

           IF  WRK-FS-CADEMPR          EQUAL '10'
               DISPLAY '************** GFCT1550 **************'
               DISPLAY '*                                    *'
               DISPLAY '*      ARQUIVO CADEMPR VAZIO        *'
               DISPLAY '*       PROCESSAMENTO ENCERRADO      *'
               DISPLAY '*                                    *'
               DISPLAY '************** GFCT1550 **************'
               PERFORM 500000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       350000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       360000-EXECUTAR-COMMIT          SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               COMMIT
           END-EXEC.

           IF  SQLCODE                 NOT EQUAL ZEROS
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'COMMIT'           TO ERR-DBD-TAB
                                          ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0120'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       360000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*


      *----------------------------------------------------------------*
       400000-FECHAR-CURSOR1           SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CRS1-GFCTB0A7
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PDIDO_ESTRN_GRP ' TO ERR-DBD-TAB
               MOVE 'CLOSE   '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0130'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       400000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       500000-FINALIZAR                SECTION.
      *----------------------------------------------------------------*

           IF  ACU-PAGINAS             GREATER ZEROS
               PERFORM 510000-EMITIR-TOTAIS
           END-IF.

           IF  ACU-OCORRENCIAS         GREATER ZEROS
               PERFORM 360000-EXECUTAR-COMMIT
           END-IF.

           CLOSE CADEMPR
                 RELATO.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 110000-TESTAR-FILE-STATUS.

           DISPLAY '*********************** GFCT1550 *******************
      -            '****'.
           DISPLAY '*
      -            '   *'.

           MOVE ACU-LIDOS-CRS1             TO WRK-MASCARA-1.

           DISPLAY '*   ** QUANTIDADE DE SEGMENTOS SOLICITADOS:'
                    WRK-MASCARA-1 '   *'.
           MOVE ACU-QTD-ESTORNO-TOT        TO WRK-MASCARA-1.
           DISPLAY '*   ** QUANTIDADE DE ESTORNOS     :        '
                    WRK-MASCARA-1 '   *'.

           MOVE ACU-VLR-ESTORNO-TOT        TO WRK-MASCARA-2.

           DISPLAY '*   ** VALOR DE ESTORNOS          : '
                    WRK-MASCARA-2 '   *'.

           MOVE ACU-QTD-CANCEL-TOT         TO WRK-MASCARA-1.

           DISPLAY '*   ** QUANTIDADE DE CANCELAMENTOS:        '
                    WRK-MASCARA-1 '   *'.

           MOVE ACU-VLR-CANCEL-TOT         TO WRK-MASCARA-2.
           DISPLAY '*   ** VALOR DE CANCELAMENTOS     : '
                    WRK-MASCARA-2 '   *'.
           DISPLAY '*
      -            '   *'.
           DISPLAY '*********************** GFCT1550 *******************
      -            '****'.

           GOBACK.

      *----------------------------------------------------------------*
       500000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       510000-EMITIR-TOTAIS            SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.
           MOVE ACU-EMPRE-DESPRE       TO LT1-EMPRE-DESPRE.

           WRITE REG-RELATO            FROM LINTOT1.
           PERFORM 112000-TESTAR-FS-RELATO.

      *----------------------------------------------------------------*
       510000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       999999-PROCESSAR-ROTINA-ERRO    SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT1550'             TO ERR-PGM.

           IF  ERR-TIPO-ACESSO         EQUAL 'APL'
               CALL 'POOL7100'         USING WRK-BATCH
                                             ERRO-AREA
           ELSE
               CALL 'POOL7100'         USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA
           END-IF.

           GOBACK.

      *----------------------------------------------------------------*
       999999-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
