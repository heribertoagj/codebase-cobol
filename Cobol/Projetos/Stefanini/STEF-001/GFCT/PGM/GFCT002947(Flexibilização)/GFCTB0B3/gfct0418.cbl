      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT0418.
       AUTHOR.     ANDRE LUIZ LONGO.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT0418                                    *
      *    PROGRAMADOR.:   ANDRE LUIZ LONGO        - CPM PATO BRANCO   *
      *    ANALISTA CPM:   EDSON LUIZ DOS SANTOS   - CPM PATO BRANCO   *
      *    ANALISTA....:   VALERIA                 - PROCWORK /GP.75   *
      *    DATA........:   09/05/2006                                  *
      *                                                                *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   FLEXIBILIZACAO - RENOVACAO DE FLEXIBILIZACAO*
      *      PARA AGRUPAMENTO.                                         *
      *                                                                *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                        INCLUDE/BOOK     *
      *                   DB2PRD.PARM_CTRL_GSTAO        GFCTB0A0       *
      *                   DB2PRD.PARM_DATA_PROCM        GFCTB0A1       *
      *                   DB2PRD.TTPO_SGMTO_GSTAO       GFCTB0H5       *
      *                   DB2PRD.V01UF                  DCITV002       *
      *                   DB2PRD.PDIDO_FLEXZ_AG         GFCTB0B1       *
      *                   DB2PRD.PDIDO_FLEXZ_AGPTO      GFCTB0B2       *
      *                   DB2PRD.PDIDO_FLEXZ_CLI        GFCTB0B3       *
      *                   DB2PRD.PDIDO_FLEXZ_MUN        GFCTB0B7       *
      *                   DB2PRD.PDIDO_FLEXZ_PAB        GFCTB0B8       *
      *                   DB2PRD.PDIDO_FLEXZ_PSTAL      GFCTB0B9       *
      *                   DB2PRD.PDIDO_FLEXZ_SGMTO      GFCTB0C0       *
      *                   DB2PRD.PDIDO_FLEXZ_UF         GFCTB0C1       *
0310  *                   DB2PRD.TPDIDO_FLEXZ_RZ        GFCTB0M7       *
080508*                   DB2PRD.TPRMSS_FLEXZ_AGPTO     GFCTB0J7       *
      *                                                                *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#GFCTNB - AREA DE COMUNICACAO - ENTRADA                    *
      *    I#GFCTNC - AREA DE COMUNICACAO - SAIDA                      *
      *    I#GFCTG1 - AREA DE COMUNICACAO - SAIDA DO MODULO GFCT5522   *
      *    I#GFCTG2 - AREA DE COMUNICACAO - ENTRADA DO MODULO GFCT5523 *
      *    I#GFCTG3 - AREA DE COMUNICACAO - SAIDA DO MODULO GFCT5523   *
      *    I#GFCTFZ - AREA DE COMUNICACAO - ENTRADA DO MODULO GFCT5522 *
      *    I#RURC88 - AREA DE COMUNICACAO - ENTRADA DO MODULO RURC9020 *
      *    I#GFCTIU - AREA DE COMUNICACAO - ENTRADA DO MODULO GFCT5110 *
      *    I#GFCTIV - AREA DE COMUNICACAO - SAIDA   DO MODULO GFCT5110 *
      *    I#MESUM8 - AREA DE COMUNICACAO COM MESU9018                 *
      *    I#MESUM6 - AREA DE COMUNICACAO COM MESU9015                 *
      *    I#GFCT0M - AREA DE COMUNICACAO - ERROS                      *
      *                                                                *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    GFCT5522 - VERIFICAR DISPONIBILIDADE DO ON-LINE.            *
      *    GFCT5523 - OBTER DESCRICAO DE MENSAGENS.                    *
      *    GFCT5018 - MODULO PARA ACESSAR TIPO TARIFA.                 *
      *    GFCT5521 - CALCULA VALOR DE TARIFA.                         *
      *    MESU9015 - DEVOLVE DADOS DO BANCO.                          *
      *    MESU9018 - DEVOLVE INFORMACOES DO PAB.                      *
      *    GFCT5505 - OBTER CODIGO DA JUNCAO.                          *
      *    RURC9020 - OBTER CODIGO OU NOME DO MUNICIPIO.               *
      *    DCIT8000 - VALIDAR UNIDADE FEDERATIVA.                      *
      *    POOL0081 - ALOCAR DINAMICAMENTE UM MODULO.                  *
      *    POOL1470 - MODULO PARA CONSISTIR DATA.                      *
      *    POOL1280 - MODULO PARA CALCULAR DIAS ENTRE DATAS.           *
0310  *    GFCT5296 - VERIFICAR SE BANCO EH POSTAL OU EXPRESSO.        *
      *                                                                *
      *----------------------------------------------------------------*
      *    ULTIMA ALTERACAO EM 08/02/2007 - VALERIA                    *
      *    VALORIZAR A DATA DE EVENTO NA CHAMADA DO MODULO GFCT5521    *
      *    COM A DATA DE INICIO DE VIGENCIA DA FLEXIBILIZACAO DIGITADA *
      *    E PASSOU A CHAMAR SOMENTE PARA QUANDO TIVER VALOR INFORMADO *
      *                                                                *
      *----------------------------------------------------------------*
      *    ULTIMA ALTERACAO EM 08/05/2008 - VALERIA                    *
      *    ALTERACOES REFERENTES AOS AGRUPAMENTOS PERSONALIZADOS       *
      *    - INCLUSAO DO AGRUPAMENTO 07                                *
      *    - INCLUSAO DOS AGRUPAMENTOS PERSONALIZADOS                  *
      *    - INCLUSAO DO PERIODO DE COBRANCA                           *
      *                                                                *
      *----------------------------------------------------------------*
0310  *    ULTIMA ALTERACAO EM 30/03/2010 - HELIO SANTONI              *
0310  *    - INCLUSAO DO TRATAMENTO MODULO GFCT5296 (POSTAL E EXPRESSO)*
0310  *    - ACRESCENTAR TRATAMENTO DOS AGRUPAMENTOS 20 E 26.          *
      *                                                                *
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
BI0413*================================================================*
      *================================================================*
      *----------------------------------------------------------------*
      *                 U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
      *        SONDA PROCWORK - CONSULTORIA - ALTERACAO - 170816       *
      *----------------------------------------------------------------*
      *                                                                *
      *    ANALISTA....:  VALERIA         - SONDA - IT                 *
      *    DATA........:  08 / 2016                                    *
      *    OBJETIVO....:  INCLUSAO DOS NOVOS AGRUPAMENTOS DO HSBC      *
      *                   42 - CONTAS P.FIS BCO. MIGRADO 2016          *
      *                   43 -  CONTAS P.JUR BCO. MIGRADO 2016         *
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

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA INDEXADORES *'.
      *----------------------------------------------------------------*

       77  IND-1                       PIC  9(003) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA VARIAVEIS AUXILIARES *'.
      *----------------------------------------------------------------*

       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-NOME-TAB                PIC  X(011)         VALUE SPACES.
080508 77  WRK-DATA-INIC-FLEX          PIC  9(008)         VALUE ZEROS.
080508 77  WRK-DATA-FIM-FLEX           PIC  9(008)         VALUE ZEROS.
080508 77  WRK-DATA-INIC-PERMIS        PIC  9(008)         VALUE ZEROS.
080508 77  WRK-DATA-FIM-PERMIS         PIC  9(008)         VALUE ZEROS.

       01  WRK-ERRO-AREA               PIC  X(107)         VALUE SPACES.

       01  WRK-CONTROLE                PIC  X(002)         VALUE SPACES.
       01  WRK-CONTROLE-R              REDEFINES WRK-CONTROLE
                                       PIC  9(002).

       01  WRK-MSG01.
           05 FILLER                   PIC  X(026)         VALUE
              'ERRO NA CHAMADA AO MODULO '.
           05 WRK-NOME-MOD             PIC  X(008)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(051)         VALUE
           '* AREA PARA VARIAVEIS DE DATAS *'.
      *----------------------------------------------------------------*

       01  WRK-DATA                    PIC  9(008)         VALUE ZEROS.
       01  WRK-FIM-FLEX                PIC  9(008)         VALUE ZEROS.
       01  WRK-INIC-FLEX               PIC  9(008)         VALUE ZEROS.
       01  WRK-INIC-TARIF              PIC  9(008)         VALUE ZEROS.
       01  WRK-FIM-TARIF               PIC  9(008)         VALUE ZEROS.
       01  WRK-DATA-PROCS              PIC  9(008)         VALUE ZEROS.
       01  WRK-TIMESTAMP-AUX           PIC  X(026)         VALUE SPACES.

       01  WRK-DATA-DB2.
           05 WRK-DIA-DB2              PIC  9(002)         VALUE ZEROS.
           05 FILLER                   PIC  X(001)         VALUE '.'.
           05 WRK-MES-DB2              PIC  9(002)         VALUE ZEROS.
           05 FILLER                   PIC  X(001)         VALUE '.'.
           05 WRK-ANO-DB2              PIC  9(004)         VALUE ZEROS.

       01  WRK-DATA-INV-R              PIC  9(008)         VALUE ZEROS.
       01  WRK-DATA-INV                REDEFINES WRK-DATA-INV-R.
           05 WRK-ANO-INV              PIC  9(004).
           05 WRK-MES-INV              PIC  9(002).
           05 WRK-DIA-INV              PIC  9(002).

       01  WRK-DATA-INV2-R             PIC  9(008)         VALUE ZEROS.
       01  WRK-DATA-INV2               REDEFINES WRK-DATA-INV2-R.
           05 WRK-ANO-INV2             PIC  9(004).
           05 WRK-MES-INV2             PIC  9(002).
           05 WRK-DIA-INV2             PIC  9(002).

080508 01  WRK-DATA-AUX.
080508     05  WRK-DIA-AUX             PIC  9(002)         VALUE ZEROS.
080508     05  FILLER                  PIC  X(001)         VALUE SPACES.
080508     05  WRK-MES-AUX             PIC  9(002)         VALUE ZEROS.
080508     05  FILLER                  PIC  X(001)         VALUE SPACES.
080508     05  WRK-ANO-AUX             PIC  9(004)         VALUE ZEROS.
080508
080508 01  WRK-DATA-INV-PERM           PIC  9(008)         VALUE ZEROS.
080508 01  WRK-DATA-INV-R-PERM         REDEFINES WRK-DATA-INV-PERM.
080508     05  WRK-ANO-INV-PERM        PIC  9(004).
080508     05  WRK-MES-INV-PERM        PIC  9(002).
080508     05  WRK-DIA-INV-PERM        PIC  9(002).

       01  WRK-CDEPDC-INICIAL          PIC S9(005) COMP-3  VALUE ZEROS.
       01  WRK-CDEPDC-FINAL            PIC S9(005) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA POOL1280 ***'.
      *----------------------------------------------------------------*

       01  WRK-1280-DATA-INFERIOR      PIC  9(008) COMP-3  VALUE ZEROS.
       01  WRK-1280-DATA-SUPERIOR      PIC  9(008) COMP-3  VALUE ZEROS.
       01  WRK-1280-NRO-DIAS           PIC S9(005) COMP-3  VALUE ZEROS.
       01  WRK-1280-MENSAGEM           PIC  X(050)         VALUE SPACES.

ST2506*----------------------------------------------------------------*
ST2506*01  FILLER                      PIC  X(050)         VALUE
ST2506*    '*** AREAS DA POOL0110 ***'.
ST2506*----------------------------------------------------------------*
ST2506*
ST2506*01  WRK-CGC-CPF-0110.
ST2506*    05  WRK-NUMERO-0110         PIC  9(009)         VALUE ZEROS.
ST2506*    05  WRK-FILIAL-0110         PIC  9(005)         VALUE ZEROS.
ST2506*
ST2506*01  WRK-CONTROLE-0110           PIC  X(002)         VALUE SPACES.

ST2506 01  WRK-9-02-X.
ST2506     05 WRK-9-02                 PIC  9(02) VALUE ZEROS.
ST2506
ST2506*----------------------------------------------------------------*
ST2506 01 WRK-BRAD2000                 PIC  X(08)  VALUE
ST2506       'BRAD2000'.
ST2506 
ST2506 01 WRK-AREA-BRAD2000.
ST2506    05 WRK-CAD-CPFCGC.
ST2506       10 WRK-CAD-NUMERO         PIC X(09)           VALUE SPACES.
ST2506       10 WRK-CAD-FILIAL         PIC X(04)           VALUE SPACES.        
ST2506 77  WRK-CAD-CONTROLE            PIC 9(02)           VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM DCIT8000  *'.
      *----------------------------------------------------------------*

       01  WRK-5505-AREA-ENTRADA.
           05 WRK-5505-AMBIENTE        PIC X(001)          VALUE SPACES.
           05 WRK-5505-FUNC-BDSCO      PIC X(007)          VALUE SPACES.

       01  WRK-5505-AREA-SAIDA.
           05 WRK-5505-COD-RETORNO     PIC 9(002)          VALUE ZEROS.
           05 WRK-5505-COD-SQL-ERRO    PIC 9(003)          VALUE ZEROS.
           05 WRK-5505-DESC-MSG        PIC X(070)          VALUE SPACES.
           05 WRK-5505-DADOS-RETORNO.
             10 WRK-5505-COD-JUNCAO    PIC 9(005)          VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM DCIT8000  *'.
      *----------------------------------------------------------------*

       01  WRK-DCIT-UNIDADE-FEDERATIVA.
           05  WRK-AREA-DCLGEN-DCITV002 PIC  X(070)        VALUE SPACES.
           05  WRK-DCIT-CODIGO-RETORNO  PIC  9(002)        VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA O MODULO GFCT5521 *'.
      *----------------------------------------------------------------*

       01  WRK-5521-AREA-ENTRADA.
           05 WRK-5521-AMBIENTE          PIC  X(001)       VALUE SPACES.
           05 WRK-5521-CHAMADOR          PIC  X(008)       VALUE SPACES.
           05 WRK-5521-COD-TARIFA        PIC  9(005)       VALUE ZEROS.
           05 WRK-5521-QTD-EVN-AGRUP     PIC  9(007)       VALUE ZEROS.
           05 WRK-5521-DATA-DEBITO       PIC  X(010)       VALUE SPACES.
           05 WRK-5521-DATA-EVN          PIC  X(010)       VALUE SPACES.
           05 WRK-5521-VALOR-EVN         PIC  9(011)V99    VALUE ZEROS.
           05 WRK-5521-TIPO-MOEDA-EVN    PIC  9(002)       VALUE ZEROS.
           05 WRK-5521-DATA-CONV-EVN     PIC  X(010)       VALUE SPACES.
           05 WRK-5521-TIPO-MOVTO        PIC  9(002)       VALUE ZEROS.
           05 FILLER                     PIC  X(050)       VALUE SPACES.

       01  WRK-5521-AREA-SAIDA.
           05 WRK-5521-COD-RETORNO       PIC  9(002)       VALUE ZEROS.
           05 WRK-5521-COD-SQL-ERRO      PIC  9(003)       VALUE ZEROS.
           05 WRK-5521-COD-MSG-GFCT      PIC  9(004)       VALUE ZEROS.
           05 WRK-5521-DESC-MSG          PIC  X(070)       VALUE SPACES.
           05 WRK-5521-QTD-OCORR         PIC  9(005)       VALUE ZEROS.
           05 WRK-5521-RETORNO-VALOR-TARIFA.
             10 WRK-5521-TAXA-MOEDA-TAR  PIC  9(006)V9(12) VALUE ZEROS.
             10 WRK-5521-TAXA-MOEDA-EVN  PIC  9(006)V9(12) VALUE ZEROS.
             10 WRK-5521-VLR-TOT-TAR-PERC  PIC  9(009)V99  VALUE ZEROS.
             10 WRK-5521-VLR-SOMA-AJU-MINI PIC  9(009)V99  VALUE ZEROS.
             10 WRK-5521-VLR-SUBT-AJU-MAXI PIC  9(009)V99  VALUE ZEROS.
             10 WRK-5521-VLR-CALC-TAR-PERC PIC  9(009)V99  VALUE ZEROS.
             10 WRK-5521-VLR-CALC-TAR-FIXO PIC  9(009)V99  VALUE ZEROS.
             10 WRK-5521-VLR-TOTAL-TARIFA  PIC  9(009)V99  VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(051)         VALUE
           '* AREA PARA COMUNICACAO COM MODULO GFCT5018 *'.
      *----------------------------------------------------------------*

       01  WRK-5018-AREA-ENTRADA.
           05 WRK-5018-AMBIENTE               PIC  X(001)  VALUE SPACES.
           05 WRK-5018-CODIGO-TARIFA          PIC  9(005)  VALUE ZEROS.
           05 WRK-5018-DT-REFER               PIC  X(010)  VALUE SPACES.

       01  WRK-5018-AREA-SAIDA.
           05 WRK-5018-COD-RETORNO            PIC  9(002)  VALUE ZEROS.
           05 WRK-5018-COD-SQL-ERRO           PIC  9(003)  VALUE ZEROS.
           05 WRK-5018-DESC-MSG               PIC  X(070)  VALUE SPACES.

       01  WRK-5018-DADOS-RETORNO.
           05 WRK-5018-DADOS-PRINCIPAIS.
             10 WRK-5018-CSERVC-TARIF         PIC  9(005)  VALUE ZEROS.
             10 WRK-5018-CBCO-CCREN           PIC  9(003)  VALUE ZEROS.
             10 WRK-5018-CIDTFD-LCTO          PIC  9(005)  VALUE ZEROS.
             10 WRK-5018-CIDTFD-SNAL-LCTO     PIC  9(001)  VALUE ZEROS.
             10 WRK-5018-CEMPR-INC            PIC  9(005)  VALUE ZEROS.
             10 WRK-5018-CDEPDC               PIC  9(005)  VALUE ZEROS.
             10 WRK-5018-CTPO-SERVC-TARIF     PIC  9(001)  VALUE ZEROS.
             10 WRK-5018-RSERVC-TARIF         PIC  X(255)  VALUE SPACES.
             10 WRK-5018-RSERVC-TARIF-REDZD   PIC  X(100)  VALUE SPACES.
             10 WRK-5018-CPSSOA-SERVC-TARIF   PIC  X(001)  VALUE SPACES.
             10 WRK-5018-CTPO-CTA-DEB-TARIF   PIC  9(001)  VALUE ZEROS.
             10 WRK-5018-CTPO-DEB-COBR        PIC  9(001)  VALUE ZEROS.
             10 WRK-5018-RUND-COBR-SERVC      PIC  X(020)  VALUE SPACES.
             10 WRK-5018-CLCTO-SERVC          PIC  9(005)  VALUE ZEROS.
             10 WRK-5018-HIDTFD-MANUT-SERVC-R PIC  X(026)  VALUE SPACES.
             10 WRK-5018-CSEQ-MANUT-SERVC-R   PIC  9(002)  VALUE ZEROS.
             10 WRK-5018-CTPO-LCTO-COBR       PIC  9(002)  VALUE ZEROS.
             10 WRK-5018-CTPO-LCTO-ESTRN      PIC  9(002)  VALUE ZEROS.
             10 WRK-5018-CCART-DEB-TARIF      PIC  9(005)  VALUE ZEROS.
             10 WRK-5018-CINDCD-DEB-PCIAL     PIC  X(001)  VALUE SPACES.
             10 WRK-5018-CINDCD-PCIAL-ULT     PIC  X(001)  VALUE SPACES.
             10 WRK-5018-CINDCD-EVNTO-PCELD   PIC  X(001)  VALUE SPACES.

           05 WRK-5018-DADOS-DE-PACOTE        OCCURS 50.
             10 WRK-5018-DINIC-VGCIA-COMPS    PIC  X(010)  VALUE SPACES.
             10 WRK-5018-CSEQ-COMPO-PCOTE     PIC  9(003)  VALUE ZEROS.
             10 WRK-5018-DFIM-VGCIA-COMPS     PIC  X(010)  VALUE SPACES.
             10 WRK-5018-CGRP-SERVC           PIC  9(005)  VALUE ZEROS.
             10 WRK-5018-DINIC-VGCIA-SERVC    PIC  X(010)  VALUE SPACES.
             10 WRK-5018-CTARIF-COMPO-PCOTE   PIC  9(005)  VALUE ZEROS.
             10 WRK-5018-QFRANQ-COMPO-PCOTE   PIC  9(003)  VALUE ZEROS.
             10 WRK-5018-CPER-COBR-COMPO      PIC  9(001)  VALUE ZEROS.
             10 WRK-5018-VEXCED-COMPO-PCOTE   PIC  9(011)V9(002)
                                                           VALUE ZEROS.
             10 WRK-5018-PEXCED-COMPO-PCOTE   PIC  9(007)V9(004)
                                                           VALUE ZEROS.
             10 WRK-5018-QDIA-COBR-COMPO      PIC  9(002)  VALUE ZEROS.
             10 WRK-5018-RSEGDA-LIN-EXCED     PIC  X(032)  VALUE SPACES.
             10 WRK-5018-HIDTFD-MANUT-SERVC   PIC  X(026)  VALUE SPACES.
             10 WRK-5018-CSEQ-MANUT-SERVC     PIC  9(002)  VALUE ZEROS.
           05 WRK-5018-DADOS-DE-PARAMENTROS.
             10 WRK-5018-DINIC-VGCIA-TARIF-P  PIC  X(010)  VALUE SPACES.
             10 WRK-5018-DFIM-VGCIA-TARIF     PIC  X(010)  VALUE SPACES.
             10 WRK-5018-CNATUZ-SERVC         PIC  9(003)  VALUE ZEROS.
             10 WRK-5018-CCJTO-SERVC          PIC  9(003)  VALUE ZEROS.
             10 WRK-5018-CINDCD-DEB-CONSL     PIC  X(001)  VALUE SPACES.
             10 WRK-5018-CINDCD-EXCED-CONSL   PIC  X(001)  VALUE SPACES.
             10 WRK-5018-QFRANQ-TARIF         PIC  9(003)  VALUE ZEROS.
             10 WRK-5018-QMAX-TENTV-DEB       PIC  9(003)  VALUE ZEROS.
             10 WRK-5018-CPER-FRANQ-TARIF     PIC  9(001)  VALUE ZEROS.
             10 WRK-5018-CPER-CALC-TARIF      PIC  9(001)  VALUE ZEROS.
             10 WRK-5018-CINIC-PER-CALC       PIC  9(001)  VALUE ZEROS.
             10 WRK-5018-DDIA-COBR-TARIF      PIC  9(002)  VALUE ZEROS.
BI0413       10 WRK-5018-QDIA-CAREN-TARIF     PIC  9(003)  VALUE ZEROS.
             10 WRK-5018-PDESC-CAREN-TARIF    PIC  9(007)V9(004)
                                                           VALUE ZEROS.
             10 WRK-5018-CINDCD-INIC-CAREN    PIC  9(001)  VALUE ZEROS.
             10 WRK-5018-QDIA-RTCAO-COBR      PIC  9(002)  VALUE ZEROS.
             10 WRK-5018-DINIC-CAREN-TARIF    PIC  X(010)  VALUE SPACES.
             10 WRK-5018-RSEGDA-LIN-EXTRT     PIC  X(032)  VALUE SPACES.
             10 WRK-5018-HIDTFD-MANUT-SERVC-P PIC  X(026)  VALUE SPACES.
             10 WRK-5018-CSEQ-MANUT-SERVC-P   PIC  9(002)  VALUE ZEROS.
             10 WRK-5018-CTPO-FRANQ-TARIF     PIC  9(001)  VALUE ZEROS.
           05 WRK-5018-DADOS-DE-VALORES       OCCURS 50.
             10 WRK-5018-DINIC-VGCIA-TARIF    PIC  X(010)  VALUE SPACES.
             10 WRK-5018-CSEQ-COMPO-TARIF     PIC  9(002)  VALUE ZEROS.
             10 WRK-5018-CMOEDA-BACEN         PIC  9(005)  VALUE ZEROS.
             10 WRK-5018-RCOMPO-TARIF         PIC  X(255)  VALUE SPACES.
             10 WRK-5018-VCOMPO-TARIF         PIC  9(011)V9(002)
                                                           VALUE ZEROS.
             10 WRK-5018-PSERVC-PTADO         PIC  9(007)V9(004)
                                                           VALUE ZEROS.
             10 WRK-5018-VMAX-COMPO-TARIF     PIC  9(011)V9(002)
                                                           VALUE ZEROS.
             10 WRK-5018-VMIN-COMPO-TARIF     PIC  9(011)V9(002)
                                                           VALUE ZEROS.
             10 WRK-5018-CMOMEN-CONVS-MOEDA   PIC  9(001)  VALUE ZEROS.

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
       01  FILLER                      PIC  X(051)         VALUE
           '* AREA PARA INCLUDES *'.
      *----------------------------------------------------------------*

       COPY 'I#MESUM8'.

       COPY 'I#MESUM6'.

       COPY 'I#GFCTIU'.

       COPY 'I#GFCTIV'.

       COPY 'I#RURC88'.

       COPY 'I#GFCTFZ'.

       COPY 'I#GFCTG1'.

       COPY 'I#GFCTG2'.

       COPY 'I#GFCTG3'.

       COPY 'GFCTWAM2'.

       COPY 'GFCTWAM3'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA TABELA DB2 *'.
      *----------------------------------------------------------------*

           EXEC SQL
             INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0A0
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0A1
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0H5
           END-EXEC.

           EXEC SQL
             INCLUDE DCITV002
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

080508     EXEC SQL
080508       INCLUDE GFCTB0J7
080508     END-EXEC.

110510     EXEC SQL
110510       INCLUDE PLCCB004
110510     END-EXEC.

110510     EXEC SQL
110510       INCLUDE GFCTB0M7
110510     END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* FIM DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY 'I#GFCTNB'.

       COPY 'I#GFCTNC'.

       COPY 'I#GFCT0M'.

      *================================================================*
       PROCEDURE                       DIVISION USING GFCTNB-ENTRADA
                                                      GFCTNC-SAIDA
                                                      GFCT0M-AREA-ERROS.
      *================================================================*

      ******************************************************************
      *ROTINA INICIAL DO PROGRAMA                                      *
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
      *ROTINA PARA PROCESSAMENTOS INICIAIS                             *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCT0M-AREA-ERROS
                                          GFCTNC-SAIDA.

           INITIALIZE GFCTNC-SAIDA
                      GFCT0M-AREA-ERROS
                      RETURN-CODE.

           MOVE 1010                   TO GFCTNC-LL.
           MOVE ZEROS                  TO GFCTNC-ZZ.

           PERFORM 1100-VERIFICAR-DISP-SISTEMA.

           PERFORM 1200-MONTAR-AREA-SAIDA.

           PERFORM 1300-CONSISTIR-DADOS.

           PERFORM 1400-OBTER-DATA-PROCS.

           PERFORM 1500-VALIDA-AGRUPAMENTO.

           PERFORM 1600-ACESSAR-GFCT5505.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *VERIFICA DISPONIBILIDADE DO SISTEMA                             *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-VERIFICAR-DISP-SISTEMA     SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                     TO GFCTG1-SAIDA.

           INITIALIZE GFCTG1-SAIDA.

           MOVE +100                       TO GFCTFZ-LL.
           MOVE 'GFCT5522'                 TO WRK-MODULO.
           MOVE 'GFCT'                     TO GFCTFZ-SISTEMA.
           MOVE GFCTNB-TRANSACAO           TO GFCTFZ-TRANSACAO.
           MOVE GFCTNB-FUNCAO              TO GFCTFZ-FUNCAO.
           MOVE GFCTNB-FUNC-BDSCO          TO GFCTFZ-FUNC-BDSCO.
           MOVE 'N'                        TO GFCTFZ-FIM.

           CALL WRK-MODULO USING
                                                 GFCTFZ-ENTRADA
                                                 GFCTG1-SAIDA
                                                 GFCT0M-AREA-ERROS.

           IF  RETURN-CODE                 NOT EQUAL ZEROS
               MOVE 9                      TO GFCTNC-ERRO
               MOVE WRK-MODULO             TO WRK-NOME-MOD
                                              GFCT0M-PROGRAMA
               MOVE WRK-MSG01              TO GFCT0M-TEXTO
               MOVE 'APL'                  TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0418'             TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-ERRO                 EQUAL 9
               MOVE 'GFCT0418'             TO GFCT0M-TRANSACAO
               MOVE GFCTG1-COD-SQL-ERRO    TO GFCT0M-SQLCODE
                                              GFCTNC-COD-SQL-ERRO
               MOVE GFCTG1-ERRO            TO GFCTNC-ERRO
               MOVE GFCTG1-COD-MSG-ERRO    TO GFCTNC-COD-MSG-ERRO
               MOVE GFCTG1-DESC-MSG-ERRO   TO GFCT0M-TEXTO
                                              GFCTNC-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-CSIT-ONLINE          EQUAL 1
               MOVE 9                      TO GFCTNC-ERRO
               MOVE 'SISTEMA INDISPONIVEL' TO GFCT0M-TEXTO
               MOVE WRK-MODULO             TO GFCT0M-PROGRAMA
               MOVE 'APL'                  TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0418'             TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      **ROTINA PARA MONTAR AREA DE SAIDA                               *
      ******************************************************************
      *----------------------------------------------------------------*
       1200-MONTAR-AREA-SAIDA          SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTNB-TRANSACAO       TO GFCTNC-TRANSACAO.
           MOVE GFCTNB-FUNCAO          TO GFCTNC-FUNCAO.

           IF  GFCTNB-QTDE-OCOR        NOT NUMERIC
               MOVE ZEROS              TO GFCTNC-QTDE-OCOR
           ELSE
               MOVE GFCTNB-QTDE-OCOR   TO GFCTNC-QTDE-OCOR
           END-IF.

           IF  GFCTNB-QTDE-TOT-REG     NOT NUMERIC
               MOVE ZEROS              TO GFCTNC-QTDE-TOT-REG
           ELSE
               MOVE GFCTNB-QTDE-TOT-REG
                                       TO GFCTNC-QTDE-TOT-REG
           END-IF.

           MOVE GFCTNB-FUNC-BDSCO      TO GFCTNC-FUNC-BDSCO.

           MOVE ZEROS                  TO GFCTNC-ERRO
                                          GFCTNC-COD-SQL-ERRO
                                          GFCTNC-COD-MSG-ERRO.
           MOVE '0000'                 TO GFCTG2-COD-MSG.

           PERFORM 1310-OBTER-DESC-MSG.

           MOVE  GFCTG3-DESC-MSG       TO GFCTNC-DESC-MSG-ERRO.

           MOVE 'S'                    TO GFCTNC-FIM.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA CONSISTIR DADOS                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1300-CONSISTIR-DADOS            SECTION.
      *----------------------------------------------------------------*

           IF  GFCTNB-TRANSACAO            EQUAL LOW-VALUES           OR
               GFCTNB-TRANSACAO            EQUAL SPACES               OR
               GFCTNB-FUNCAO               EQUAL LOW-VALUES           OR
               GFCTNB-FUNCAO               EQUAL SPACES               OR
               GFCTNB-FUNC-BDSCO           EQUAL LOW-VALUES           OR
               GFCTNB-FUNC-BDSCO           EQUAL SPACES               OR
               GFCTNB-INIC-VIG-FLEX        EQUAL LOW-VALUES           OR
               GFCTNB-INIC-VIG-FLEX        EQUAL SPACES               OR
               GFCTNB-FIM-VIG-FLEX         EQUAL LOW-VALUES           OR
               GFCTNB-FIM-VIG-FLEX         EQUAL SPACES               OR
               GFCTNB-RJUSTF-MANUT-SERVC   EQUAL LOW-VALUES           OR
               GFCTNB-RJUSTF-MANUT-SERVC   EQUAL SPACES               OR
080508***     (GFCTNB-AGRUPAMENTO          NOT EQUAL 4 AND 14 AND 09 AND
080508***                                    15 AND 17 AND 18 AND 19) OR
               GFCTNB-COD-TARIFA           NOT NUMERIC                OR
               GFCTNB-COD-TARIFA           EQUAL ZEROS                OR
               GFCTNB-CTIPO-OPER-MOTVO     NOT NUMERIC                OR
               GFCTNB-CTIPO-OPER-MOTVO     EQUAL ZEROS                OR
               GFCTNB-CMOTVO-JUSTF         NOT NUMERIC                OR
               GFCTNB-CMOTVO-JUSTF         EQUAL ZEROS                OR
               GFCTNB-VR-NEGOCIADO         NOT NUMERIC                OR
               GFCTNB-FRANQUIA             NOT NUMERIC                OR
               GFCTNB-PERIODO              NOT NUMERIC                OR
               GFCTNB-DIA-AGEND            NOT NUMERIC                OR
               GFCTNB-PERC-DESCTO          NOT NUMERIC                OR
               GFCTNB-DIAS-RETENCAO        NOT NUMERIC                OR
               GFCTNB-QTDE-OCOR            NOT NUMERIC                OR
               GFCTNB-DIA-SEMANA           NOT NUMERIC                OR
              (GFCTNB-IND-VALOR            NOT EQUAL 1 AND 2 AND 3)   OR
              (GFCTNB-AGENDAMENTO          NOT EQUAL 'S' AND 'N')     OR
              (GFCTNB-TODAS-AG             NOT EQUAL 'S' AND 'N')
               MOVE 1                      TO GFCTNC-ERRO
                                              GFCTNC-COD-MSG-ERRO
               MOVE ZEROS                  TO GFCTNC-COD-SQL-ERRO
               MOVE '0001'                 TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG       TO GFCTNC-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           EVALUATE GFCTNB-AGRUPAMENTO
             WHEN 4
               IF  GFCTNB-CGC-CPF          NOT NUMERIC OR
                   GFCTNB-CGC-CPF          EQUAL ZEROS
                   MOVE 1                  TO GFCTNC-ERRO
                   MOVE 0773               TO GFCTNC-COD-MSG-ERRO
                   MOVE ZEROS              TO GFCTNC-COD-SQL-ERRO
                   MOVE '0773'             TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG    TO GFCTNC-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               END-IF

             WHEN 14
               IF (GFCTNB-AGENCIA          NOT NUMERIC) OR
                  (GFCTNB-AGENCIA          EQUAL ZEROS  AND
                   GFCTNB-TODAS-AG         NOT EQUAL 'S')
                   MOVE 1                  TO GFCTNC-ERRO
                   MOVE 0774               TO GFCTNC-COD-MSG-ERRO
                   MOVE ZEROS              TO GFCTNC-COD-SQL-ERRO
                   MOVE '0774'             TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG    TO GFCTNC-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               END-IF

             WHEN 9
               IF  GFCTNB-SEGMENTO         NOT NUMERIC OR
                   GFCTNB-SEGMENTO         EQUAL ZEROS
                   MOVE 1                  TO GFCTNC-ERRO
                   MOVE 0775               TO GFCTNC-COD-MSG-ERRO
                   MOVE ZEROS              TO GFCTNC-COD-SQL-ERRO
                   MOVE '0775'             TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG    TO GFCTNC-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               END-IF

             WHEN 15
               IF  GFCTNB-POSTO            NOT NUMERIC OR
                   GFCTNB-AGENCIA          NOT NUMERIC OR
                   GFCTNB-AGENCIA          EQUAL ZEROS
                   MOVE 1                  TO GFCTNC-ERRO
                   MOVE 0776               TO GFCTNC-COD-MSG-ERRO
                   MOVE ZEROS              TO GFCTNC-COD-SQL-ERRO
                   MOVE '0776'             TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG    TO GFCTNC-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               END-IF

             WHEN 17
               IF  GFCTNB-POSTO            NOT NUMERIC OR
                   GFCTNB-AGENCIA          NOT NUMERIC OR
                   GFCTNB-AGENCIA          EQUAL ZEROS
                   MOVE 1                  TO GFCTNC-ERRO
                   MOVE 0778               TO GFCTNC-COD-MSG-ERRO
                   MOVE ZEROS              TO GFCTNC-COD-SQL-ERRO
                   MOVE '0778'             TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG    TO GFCTNC-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               END-IF

             WHEN 18
               IF  GFCTNB-MUNIC            EQUAL LOW-VALUES OR
                   GFCTNB-MUNIC            EQUAL SPACES
                   MOVE 1                  TO GFCTNC-ERRO
                   MOVE 0779               TO GFCTNC-COD-MSG-ERRO
                   MOVE ZEROS              TO GFCTNC-COD-SQL-ERRO
                   MOVE '0779'             TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG    TO GFCTNC-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               END-IF

             WHEN 19
               IF  GFCTNB-UF               EQUAL LOW-VALUES OR
                   GFCTNB-UF               EQUAL SPACES
                   MOVE 1                  TO GFCTNC-ERRO
                   MOVE 0780               TO GFCTNC-COD-MSG-ERRO
                   MOVE ZEROS              TO GFCTNC-COD-SQL-ERRO
                   MOVE '0780'             TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG    TO GFCTNC-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               END-IF

0310         WHEN 20
0310           IF  GFCTNB-POSTO            NOT NUMERIC OR
0310               GFCTNB-AGENCIA          NOT NUMERIC OR
0310               GFCTNB-AGENCIA          EQUAL ZEROS
0310               MOVE 1                  TO GFCTNC-ERRO
0310               MOVE 0778               TO GFCTNC-COD-MSG-ERRO
0310               MOVE ZEROS              TO GFCTNC-COD-SQL-ERRO
0310               MOVE '0778'             TO GFCTG2-COD-MSG
0310               PERFORM 1310-OBTER-DESC-MSG
0310               MOVE GFCTG3-DESC-MSG    TO GFCTNC-DESC-MSG-ERRO
0310               PERFORM 3000-FINALIZAR
0310           END-IF

0310         WHEN 26
0310           IF  GFCTNB-GRUPO-CTBIL      NOT NUMERIC OR
0310               GFCTNB-SUB-GRUPO-CTBIL  NOT NUMERIC OR
0310               GFCTNB-GRUPO-CTBIL      EQUAL ZEROS
0310               MOVE 1                  TO GFCTNC-ERRO
0310               MOVE 0778               TO GFCTNC-COD-MSG-ERRO
0310               MOVE ZEROS              TO GFCTNC-COD-SQL-ERRO
0310               MOVE '0778'             TO GFCTG2-COD-MSG
0310               PERFORM 1310-OBTER-DESC-MSG
0310               MOVE GFCTG3-DESC-MSG    TO GFCTNC-DESC-MSG-ERRO
0310               PERFORM 3000-FINALIZAR
0310           END-IF

           END-EVALUATE.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1310-OBTER-DESC-MSG             SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTG3-SAIDA.

           INITIALIZE GFCTG3-SAIDA.

           MOVE 100                    TO GFCTG2-LL.
           MOVE ZEROS                  TO GFCTG2-ZZ.
           MOVE GFCTNB-TRANSACAO       TO GFCTG2-TRANSACAO.
           MOVE GFCTNB-FUNCAO          TO GFCTG2-FUNCAO.
           MOVE GFCTNB-FUNC-BDSCO      TO GFCTG2-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTG2-FIM.
           MOVE 'O'                    TO GFCTG2-TIPO-PROC.
           MOVE 'GFCT5523'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                             GFCTG2-ENTRADA
                                             GFCTG3-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTNC-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-NOME-MOD
               MOVE WRK-MSG01          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0418'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 9
               MOVE 9                  TO GFCTNC-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-NOME-MOD
               MOVE WRK-MSG01          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0418'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO               EQUAL 1
               MOVE 1                    TO GFCTNC-ERRO
               MOVE GFCTG3-COD-SQL-ERRO  TO GFCTNC-COD-SQL-ERRO
               MOVE GFCTG3-COD-MSG-ERRO  TO GFCTNC-COD-MSG-ERRO
               MOVE GFCTG3-DESC-MSG-ERRO TO GFCTNC-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA OBTER DATA DE PROCESSAMENTO                         *
      ******************************************************************
      *----------------------------------------------------------------*
       1400-OBTER-DATA-PROCS           SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC      OF GFCTB0A1.

           EXEC SQL
             SELECT
                   DPROCM_ATUAL
             INTO
                   :GFCTB0A1.DPROCM-ATUAL
             FROM  DB2PRD.PARM_DATA_PROCM
             WHERE CSIST_PRINC        = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0A1'      TO WRK-NOME-TAB
               MOVE 'PARM_DATA_PROCM ' TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
               MOVE '0010'             TO GFCT0M-LOCAL
               MOVE  0010              TO GFCTNC-COD-MSG-ERRO
               PERFORM 1410-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA MOVER ERRO SQL                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       1410-MOVER-ERRO-SQL             SECTION.
      *----------------------------------------------------------------*

           MOVE  9                     TO GFCTNC-ERRO.
           MOVE 'GFCT0418'             TO GFCT0M-PROGRAMA.
           MOVE 'DB2'                  TO GFCT0M-TIPO-ACESSO.
           MOVE  SQLCA                 TO GFCT0M-SQLCA-AREA.
           MOVE  SQLCODE               TO GFCT0M-SQLCODE
                                          GFCTNC-COD-SQL-ERRO.
           MOVE SPACES                 TO GFCTNC-DESC-MSG-ERRO.

           STRING GFCTG3-DESC-MSG      WRK-NOME-TAB

           DELIMITED BY '  '           INTO GFCTNC-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       1410-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA VALIDAR AGRUPAMENTO                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       1500-VALIDA-AGRUPAMENTO         SECTION.
      *----------------------------------------------------------------*

           EVALUATE GFCTNB-AGRUPAMENTO
               WHEN 15
                   PERFORM 1510-CONSISTIR-PAB

               WHEN 14
                   IF  GFCTNB-TODAS-AG NOT EQUAL 'S'
                       PERFORM 1520-CONSISTIR-AGENCIA
                   END-IF

               WHEN 17
0310               PERFORM 1530-VERIFICA-EXPSS-PTAL

               WHEN 4
                   PERFORM 1540-CONSISTIR-CLIENTE

               WHEN 9
                   PERFORM 1550-CONSISTIR-SEGMENTO

               WHEN 18
                   PERFORM 1560-CONSISTIR-MUNICIPIO

               WHEN 19
                   PERFORM 1570-CONSISTIR-UF

0310           WHEN 20
0310               PERFORM 1530-VERIFICA-EXPSS-PTAL

0310           WHEN 26
0310               PERFORM 1580-CONSISTIR-RAZAO

           END-EVALUATE.

      *----------------------------------------------------------------*
       1500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA CONSISTIR PAB                                       *
      ******************************************************************
      *----------------------------------------------------------------*
       1510-CONSISTIR-PAB              SECTION.
      *----------------------------------------------------------------*

           PERFORM 1520-CONSISTIR-AGENCIA.

           MOVE 'VRS001'                       TO COMU-VERSAO.
           MOVE 237                            TO COMU-COD-EMPR.
           MOVE GFCTNB-AGENCIA                 TO COMU-COD-DEPDC.
           MOVE GFCTNB-POSTO                   TO COMU-COD-POSTO.
           MOVE SPACES                         TO COMU-CORR-BANCARIO.
           MOVE 'MESU9018'                     TO WRK-MODULO.

           CALL WRK-MODULO USING
                                                     COMU-DADOS-9018.

           IF  RETURN-CODE                     NOT EQUAL ZEROS
               EVALUATE RETURN-CODE
                 WHEN 4
                   MOVE 1                      TO GFCTNC-ERRO
                   MOVE 0518                   TO GFCTNC-COD-MSG-ERRO
                   MOVE '0518'                 TO GFCTG2-COD-MSG
                   MOVE ZEROS                  TO GFCTNC-COD-SQL-ERRO
                   PERFORM 1310-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG        TO GFCTNC-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR

                 WHEN 8
                   MOVE 1                      TO GFCTNC-ERRO
                   MOVE 0519                   TO GFCTNC-COD-MSG-ERRO
                   MOVE '0519'                 TO GFCTG2-COD-MSG
                   MOVE ZEROS                  TO GFCTNC-COD-SQL-ERRO
                   PERFORM 1310-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG        TO GFCTNC-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR

                 WHEN 12
                   MOVE 1                      TO GFCTNC-ERRO
                   MOVE 0520                   TO GFCTNC-COD-MSG-ERRO
                   MOVE '0520'                 TO GFCTG2-COD-MSG
                   MOVE ZEROS                  TO GFCTNC-COD-SQL-ERRO
                   PERFORM 1310-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG        TO GFCTNC-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR

               END-EVALUATE
           ELSE
               IF  COMU-COD-TIPO-POSTO         EQUAL 9
                   MOVE 1                      TO GFCTNC-ERRO
                   MOVE 0594                   TO GFCTNC-COD-MSG-ERRO
                   MOVE '0594'                 TO GFCTG2-COD-MSG
                   MOVE ZEROS                  TO GFCTNC-COD-SQL-ERRO
                   PERFORM 1310-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG        TO GFCTNC-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1510-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA CONSISTIR AGENCIA                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       1520-CONSISTIR-AGENCIA          SECTION.
      *----------------------------------------------------------------*

           MOVE 237                    TO COMU-BANCO.
           MOVE GFCTNB-AGENCIA         TO COMU-DEPDC.
           MOVE 'GFCT0418'             TO COMU-NOME-PGM.
           MOVE 'MESU9015'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                             COMU-PARM
                                             WRK-POOL7100
                                             WRK-SQLCA.

           IF  COMU-COD-RETORNO         NOT EQUAL ZEROS
               MOVE 1                   TO GFCTNC-ERRO
               MOVE 0465                TO GFCTNC-COD-MSG-ERRO
               MOVE '0465'              TO GFCTG2-COD-MSG
               MOVE ZEROS               TO GFCTNC-COD-SQL-ERRO
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG      TO GFCTNC-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1520-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA CONSISTIR POSTAL E BRAD. EXPRESSO                   *
      ******************************************************************
      *----------------------------------------------------------------*
       1530-VERIFICA-EXPSS-PTAL        SECTION.
      *----------------------------------------------------------------*
0610XS
0610XS     MOVE SPACES                 TO WAM2-ENTRADA
0610XS                                    WAM3-SAIDA
0610XS                                    GFCT0M-AREA-ERROS.

           INITIALIZE                     GFCT0M-AREA-ERROS
                                          WAM3-SAIDA
0610XS
0610XS     MOVE +100                   TO WAM2-LL.
0610XS     MOVE ZEROS                  TO WAM2-ZZ.
0610XS     MOVE GFCTNB-TRANSACAO       TO WAM2-TRANSACAO.
0610XS     MOVE GFCTNB-FUNCAO          TO WAM2-FUNCAO.
0610XS     MOVE GFCTNB-FUNC-BDSCO      TO WAM2-FUNC-BDSCO.
0610XS     MOVE ZEROS                  TO WAM2-TERMINAL.
0610XS     MOVE 'N'                    TO WAM2-FIM.

           MOVE GFCTNB-AGRUPAMENTO     TO WAM2-CAGPTO
           MOVE GFCTNB-AGENCIA         TO WAM2-CAGENCIA
           MOVE GFCTNB-POSTO           TO WAM2-CPAB

           MOVE 'GFCT5296'             TO WRK-MODULO

           CALL WRK-MODULO          USING WAM2-ENTRADA
                                          WAM3-SAIDA
                                          GFCT0M-AREA-ERROS.

           IF  RETURN-CODE            NOT EQUAL ZEROS
               MOVE 9                  TO GFCTNC-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-NOME-MOD
               MOVE WRK-MSG01          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0418'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WAM3-ERRO              NOT EQUAL ZEROS
               MOVE WAM3-ERRO          TO GFCTNC-ERRO
               MOVE ZEROS              TO GFCTNC-COD-SQL-ERRO
               MOVE WAM3-COD-MSG-ERRO  TO GFCTNC-COD-MSG-ERRO
               MOVE WAM3-DESC-MSG-ERRO TO GFCTNC-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1530-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA CONSISTIR CLIENTE                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       1540-CONSISTIR-CLIENTE          SECTION.
      *----------------------------------------------------------------*

ST2506*    M0VE SPACES                 TO WRK-CONTROLE-0110.
ST2506*    M0VE GFCTNB-CGC-CPF         TO WRK-NUMERO-0110.
ST2506*    M0VE GFCTNB-FILIAL          TO WRK-FILIAL-0110.
ST2506*
ST2506*    CALL 'POOL0110'             USING WRK-CGC-CPF-0110
ST2506*                                      WRK-CONTROLE-0110.
ST2506     MOVE ZEROS                 TO WRK-CAD-CONTROLE. 
ST2506     MOVE GFCTNB-CGC-CPF        TO WRK-CAD-NUMERO.
ST2506     MOVE GFCTNB-FILIAL         TO WRK-CAD-FILIAL.
ST2506
ST2506     CALL WRK-BRAD2000          USING WRK-CAD-CPFCGC
ST2506                                       WRK-CAD-CONTROLE.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 1                  TO GFCTNC-ERRO
               MOVE 0468               TO GFCTNC-COD-MSG-ERRO
               MOVE '0468'             TO GFCTG2-COD-MSG
               MOVE ZEROS              TO GFCTNC-COD-SQL-ERRO
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNC-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

ST2506*    IF  GFCTNB-CONTROLE         NOT EQUAL WRK-CONTROLE-0110
ST2506     IF  GFCTNB-CONTROLE         NOT EQUAL WRK-CAD-CONTROLE
               MOVE 1                  TO GFCTNC-ERRO
               MOVE 0507               TO GFCTNC-COD-MSG-ERRO
               MOVE '0507'             TO GFCTG2-COD-MSG
               MOVE ZEROS              TO GFCTNC-COD-SQL-ERRO
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNC-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           PERFORM 1541-PESQUISAR-BASE-CLIE.

      *----------------------------------------------------------------*
       1540-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA PESQUISAR BASE CLIE                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       1541-PESQUISAR-BASE-CLIE        SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTIU-ENTRADA
                                          GFCTIV-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE GFCTIU-ENTRADA
                      GFCTIV-SAIDA
                      GFCT0M-AREA-ERROS.

           MOVE 100                    TO GFCTIU-LL.
           MOVE ZEROS                  TO GFCTIU-ZZ.
           MOVE GFCTNB-TRANSACAO       TO GFCTIU-TRANSACAO.
           MOVE GFCTNB-FUNCAO          TO GFCTIU-FUNCAO.
           MOVE GFCTNB-FUNC-BDSCO      TO GFCTIU-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTIU-FIM.
ST2506*    MOVE GFCTNB-CGC-CPF         TO GFCTIU-NRO-CPF-CNPJ.
ST2506*    MOVE GFCTNB-FILIAL          TO GFCTIU-NRO-FILIAL.
ST2506     MOVE GFCTNB-CGC-CPF         TO GFCTIU-COD-CPF-CNPJ.
ST2506     MOVE GFCTNB-FILIAL          TO GFCTIU-COD-FILIAL.
           MOVE GFCTNB-CONTROLE        TO WRK-CONTROLE.
           MOVE WRK-CONTROLE-R         TO GFCTIU-CTRL-CPF-CNPJ.
           MOVE 'GFCT5110'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                             GFCTIU-ENTRADA
                                             GFCTIV-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTNC-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-NOME-MOD
               MOVE WRK-MSG01          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0418'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTIV-ERRO              NOT EQUAL ZEROS
               MOVE GFCTIV-ERRO         TO GFCTNC-ERRO
               MOVE GFCTIV-COD-SQL-ERRO TO GFCTNC-COD-SQL-ERRO
               MOVE GFCTIV-COD-MSG-ERRO TO GFCTNC-COD-MSG-ERRO
               STRING 'GFCT5110 - '     GFCTIV-DESC-MSG-ERRO
               DELIMITED BY SIZE        INTO GFCTNC-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1541-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA CONSISTIR SEGMENTO                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       1550-CONSISTIR-SEGMENTO         SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTNB-SEGMENTO       TO CSGMTO-GSTAO-TARIF OF GFCTB0H5.
           MOVE DPROCM-ATUAL          OF GFCTB0A1
                                      TO DINIC-VGCIA-SGMTO  OF GFCTB0H5.
           MOVE DPROCM-ATUAL          OF GFCTB0A1
                                      TO DFIM-VGCIA-SGMTO   OF GFCTB0H5.

           EXEC SQL
             SELECT
                   CSGMTO_GSTAO_TARIF
             INTO  :GFCTB0H5.CSGMTO-GSTAO-TARIF
             FROM  DB2PRD.TTPO_SGMTO_GSTAO
             WHERE CSGMTO_GSTAO_TARIF = :GFCTB0H5.CSGMTO-GSTAO-TARIF
               AND DINIC_VGCIA_SGMTO <= :GFCTB0H5.DINIC-VGCIA-SGMTO
               AND DFIM_VGCIA_SGMTO  >= :GFCTB0H5.DFIM-VGCIA-SGMTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0012'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0H5'      TO WRK-NOME-TAB
               MOVE 'TTPO_SGMTO_GSTAO' TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
               MOVE '0020'             TO GFCT0M-LOCAL
               MOVE  0012              TO GFCTNC-COD-MSG-ERRO
               PERFORM 1410-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 1                  TO GFCTNC-ERRO
               MOVE 0500               TO GFCTNC-COD-MSG-ERRO
               MOVE '0500'             TO GFCTG2-COD-MSG
               MOVE ZEROS              TO GFCTNC-COD-SQL-ERRO
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNC-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1550-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA CONSISTIR MUNICIPIO                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       1560-CONSISTIR-MUNICIPIO        SECTION.
      *----------------------------------------------------------------*

           INITIALIZE AREA-RURC88.

           MOVE GFCTNB-MUNIC           TO COD-MUNIC-RURC88.
           MOVE 'RURC9020'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                             AREA-RURC88.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 1                  TO GFCTNC-ERRO
               MOVE ZEROS              TO GFCTNC-COD-SQL-ERRO
               MOVE 0732               TO GFCTNC-COD-MSG-ERRO
               MOVE '0732'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNC-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  COD-RETORNO-RURC88      EQUAL 10
               MOVE 1                  TO GFCTNC-ERRO
               MOVE ZEROS              TO GFCTNC-COD-SQL-ERRO
               MOVE 0675               TO GFCTNC-COD-MSG-ERRO
               MOVE '0675'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNC-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  COD-RETORNO-RURC88      EQUAL 20
               MOVE 1                  TO GFCTNC-ERRO
               MOVE ZEROS              TO GFCTNC-COD-SQL-ERRO
               MOVE 0733               TO GFCTNC-COD-MSG-ERRO
               MOVE '0733'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNC-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  COD-RETORNO-RURC88      EQUAL 99
               MOVE 1                  TO GFCTNC-ERRO
               MOVE SQLCODE-RURC88     TO GFCTNC-COD-SQL-ERRO
               MOVE 0734               TO GFCTNC-COD-MSG-ERRO
               MOVE '0734'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNC-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1560-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA CONSISTIR UF                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       1570-CONSISTIR-UF               SECTION.
      *----------------------------------------------------------------*

           INITIALIZE DCITV002.

           MOVE GFCTNB-UF              TO CSGL-UF OF DCITV002.
           MOVE DCITV002               TO WRK-AREA-DCLGEN-DCITV002.
           MOVE 'DCIT8000'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                             WRK-DCIT-UNIDADE-FEDERATIVA
                                             WRK-ERRO-AREA
                                             SQLCA.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 1                  TO GFCTNC-ERRO
               MOVE ZEROS              TO GFCTNC-COD-SQL-ERRO
               MOVE 0735               TO GFCTNC-COD-MSG-ERRO
               MOVE '0735'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNC-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-DCIT-CODIGO-RETORNO EQUAL 1
               MOVE 1                  TO GFCTNC-ERRO
               MOVE ZEROS              TO GFCTNC-COD-SQL-ERRO
               MOVE 0736               TO GFCTNC-COD-MSG-ERRO
               MOVE '0736'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNC-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-DCIT-CODIGO-RETORNO EQUAL 2
               MOVE 1                  TO GFCTNC-ERRO
               MOVE ZEROS              TO GFCTNC-COD-SQL-ERRO
               MOVE 0737               TO GFCTNC-COD-MSG-ERRO
               MOVE '0737'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNC-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-DCIT-CODIGO-RETORNO EQUAL 3
               MOVE 1                  TO GFCTNC-ERRO
               MOVE ZEROS              TO GFCTNC-COD-SQL-ERRO
               MOVE 0738               TO GFCTNC-COD-MSG-ERRO
               MOVE '0738'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNC-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-DCIT-CODIGO-RETORNO EQUAL  4
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0005'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - DCITV002'      TO WRK-NOME-TAB
               MOVE 'V01_UF'           TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0030'             TO GFCT0M-LOCAL
               MOVE  0005              TO GFCTNC-COD-MSG-ERRO
               PERFORM 1410-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1570-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

0310  ******************************************************************
0310  *ROTINA PARA CONSISTIR SE AGENCIA VALIDA                         *
0310  ******************************************************************
0310  *----------------------------------------------------------------*
0310   1580-CONSISTIR-RAZAO            SECTION.
0310  *----------------------------------------------------------------*
0310
           MOVE GFCTNB-GRUPO-CTBIL  TO CGRP-CTBIL-ATUAL  OF PLCCB004.
           MOVE GFCTNB-SUB-GRUPO-CTBIL TO CSGRP-CTBIL    OF PLCCB004.
           MOVE 237                    TO CEMPR          OF PLCCB004.

           EXEC SQL
             SELECT
                   CEMPR
             INTO  :PLCCB004.CEMPR
             FROM  DB2PRD.RAZAO_CONTABIL
             WHERE CGRP_CTBIL_ATUAL = :PLCCB004.CGRP-CTBIL-ATUAL
               AND CSGRP_CTBIL      = :PLCCB004.CSGRP-CTBIL
               AND CEMPR            = :PLCCB004.CEMPR
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0012'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - PLCCB004'      TO WRK-NOME-TAB
               MOVE 'RAZAO_CONTABIL  ' TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
               MOVE '0025'             TO GFCT0M-LOCAL
               MOVE  0012              TO GFCTNC-COD-MSG-ERRO
               PERFORM 1410-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 1                  TO GFCTNC-ERRO
               MOVE 1689               TO GFCTNC-COD-MSG-ERRO
               MOVE '1689'             TO GFCTG2-COD-MSG
               MOVE ZEROS              TO GFCTNC-COD-SQL-ERRO
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNC-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

0310  *----------------------------------------------------------------*
0310   1580-99-FIM.                    EXIT.
0310  *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA ACESSAR GFCT5505                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       1600-ACESSAR-GFCT5505           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  WRK-5505-AREA-SAIDA.

           MOVE 'O'                    TO WRK-5505-AMBIENTE.
           MOVE GFCTNB-FUNC-BDSCO      TO WRK-5505-FUNC-BDSCO.
           MOVE 'GFCT5505'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                             WRK-5505-AREA-ENTRADA
                                             WRK-5505-AREA-SAIDA
                                             WRK-IO-PCB
                                             WRK-ALT-PCB.


           IF  WRK-5505-COD-RETORNO    NOT EQUAL ZEROS
               MOVE ZEROS              TO GFCTNC-COD-SQL-ERRO
               MOVE 1                  TO GFCTNC-ERRO
               MOVE 0112               TO GFCTNC-COD-MSG-ERRO
               MOVE WRK-5505-DESC-MSG  TO GFCTNC-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *PROCESSAMENTO PRINCIPAL                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-CONSISTIR-PEDIDO.

           PERFORM 2200-CONSISTIR-DATAS.

           PERFORM 2300-CONSISTIR-CAMPOS.

           PERFORM 2400-INCLUIR-SOLICITACAO.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA CONSISTIR PEDIDO                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-CONSISTIR-PEDIDO           SECTION.
      *----------------------------------------------------------------*

           EVALUATE GFCTNB-AGRUPAMENTO
               WHEN 15
                   PERFORM 2110-CONSISTIR-PED-PAB

               WHEN 14
                   PERFORM 2120-CONSISTIR-PED-AGENCIA

               WHEN 17
                   PERFORM 2130-CONSISTIR-PED-POSTAL

               WHEN 4
                   PERFORM 2140-CONSISTIR-PED-CLIENTE

               WHEN 9
                   PERFORM 2150-CONSISTIR-PED-SEGMENTO

               WHEN 18
                   PERFORM 2160-CONSISTIR-PED-MUNICIPIO

               WHEN 19
                   PERFORM 2170-CONSISTIR-PED-UF

170816*  OS AGRUPAMENTOS HSBC TERAO O MESMO TRATAMENTO DO AGRUPAMENTO
170816*  FUNCIONARIO BRADESCO
170816         WHEN 42
170816         WHEN 43
080508         WHEN 7
080508             PERFORM 2180-CONSISTIR-PED-FUNC-BRAD

0310           WHEN 20
0310               PERFORM 2130-CONSISTIR-PED-POSTAL

0310           WHEN 26
0310               PERFORM 2195-CONSISTIR-PED-RAZAO

080508         WHEN OTHER
080508             PERFORM 2190-CONS-PED-AGRUP-PERSON

           END-EVALUATE.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA CONSISTIR PEDIDO PAB                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2110-CONSISTIR-PED-PAB          SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTNB-AGENCIA         TO CDEPDC            OF GFCTB0B8.
           MOVE GFCTNB-POSTO           TO CPOSTO-SERVC      OF GFCTB0B8.
           MOVE GFCTNB-COD-TARIFA      TO CSERVC-TARIF      OF GFCTB0B2
                                          CSERVC-TARIF      OF GFCTB0B8.
           MOVE GFCTNB-AGRUPAMENTO     TO CAGPTO-CTA        OF GFCTB0B2
                                          CAGPTO-CTA        OF GFCTB0B8.

           EXEC SQL
             SELECT A.CSIT_SOLTC_FLEXZ,
                    A.DINIC_FLEXZ_AGPTO
             INTO   :GFCTB0B2.CSIT-SOLTC-FLEXZ,
                    :GFCTB0B2.DINIC-FLEXZ-AGPTO
             FROM   DB2PRD.PDIDO_FLEXZ_AGPTO A,
                    DB2PRD.PDIDO_FLEXZ_PAB   B
             WHERE  A.CAGPTO_CTA         = B.CAGPTO_CTA
               AND  A.CSERVC_TARIF       = B.CSERVC_TARIF
               AND  A.HIDTFD_FLEXZ_AGPTO = B.HIDTFD_FLEXZ_AGPTO
               AND  A.CAGPTO_CTA         = :GFCTB0B2.CAGPTO-CTA
               AND  A.CSERVC_TARIF       = :GFCTB0B2.CSERVC-TARIF
               AND  B.CDEPDC             = :GFCTB0B8.CDEPDC
               AND  B.CPOSTO_SERVC       = :GFCTB0B8.CPOSTO-SERVC
               AND  B.HIDTFD_FLEXZ_AGPTO IN
              (SELECT MAX(HIDTFD_FLEXZ_AGPTO)
               FROM  DB2PRD.PDIDO_FLEXZ_PAB
               WHERE CAGPTO_CTA          = :GFCTB0B8.CAGPTO-CTA
                AND  CSERVC_TARIF        = :GFCTB0B8.CSERVC-TARIF
                AND  CDEPDC              = :GFCTB0B8.CDEPDC
                AND  CPOSTO_SERVC        = :GFCTB0B8.CPOSTO-SERVC)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0B2'      TO WRK-NOME-TAB
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
               MOVE '0040'             TO GFCT0M-LOCAL
               MOVE  0010              TO GFCTNC-COD-MSG-ERRO
               PERFORM 1410-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS AND
               CSIT-SOLTC-FLEXZ        OF GFCTB0B2
                                       EQUAL 1
               MOVE 1                  TO GFCTNC-ERRO
               MOVE ZEROS              TO GFCTNC-COD-SQL-ERRO
               MOVE 1021               TO GFCTNC-COD-MSG-ERRO
               MOVE '1021'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNC-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS AND
              (CSIT-SOLTC-FLEXZ        OF GFCTB0B2
                                       EQUAL 3 OR 4)
               MOVE 1                  TO GFCTNC-ERRO
               MOVE ZEROS              TO GFCTNC-COD-SQL-ERRO
               MOVE 0645               TO GFCTNC-COD-MSG-ERRO
               MOVE '0645'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNC-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA CONSISTIR PEDIDO AGENCIA                            *
      ******************************************************************
      *----------------------------------------------------------------*
       2120-CONSISTIR-PED-AGENCIA      SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTNB-AGENCIA         TO CDEPDC            OF GFCTB0B1.
           MOVE GFCTNB-COD-TARIFA      TO CSERVC-TARIF      OF GFCTB0B2
                                          CSERVC-TARIF      OF GFCTB0B1.
           MOVE GFCTNB-AGRUPAMENTO     TO CAGPTO-CTA        OF GFCTB0B2
                                          CAGPTO-CTA        OF GFCTB0B1.

           EXEC SQL
             SELECT A.CSIT_SOLTC_FLEXZ,
                    A.DINIC_FLEXZ_AGPTO
             INTO   :GFCTB0B2.CSIT-SOLTC-FLEXZ,
                    :GFCTB0B2.DINIC-FLEXZ-AGPTO
             FROM   DB2PRD.PDIDO_FLEXZ_AGPTO   A,
                    DB2PRD.PDIDO_FLEXZ_AG      B
              WHERE A.CAGPTO_CTA         = B.CAGPTO_CTA
               AND  A.CSERVC_TARIF       = B.CSERVC_TARIF
               AND  A.HIDTFD_FLEXZ_AGPTO = B.HIDTFD_FLEXZ_AGPTO
               AND  A.CAGPTO_CTA         = :GFCTB0B2.CAGPTO-CTA
               AND  A.CSERVC_TARIF       = :GFCTB0B2.CSERVC-TARIF
               AND  B.CDEPDC             = :GFCTB0B1.CDEPDC
               AND  B.HIDTFD_FLEXZ_AGPTO IN
              (SELECT MAX(HIDTFD_FLEXZ_AGPTO)
               FROM  DB2PRD.PDIDO_FLEXZ_AG
               WHERE CAGPTO_CTA          = :GFCTB0B1.CAGPTO-CTA
                AND  CSERVC_TARIF        = :GFCTB0B1.CSERVC-TARIF
                AND  CDEPDC              = :GFCTB0B1.CDEPDC)
           END-EXEC.

           IF (SQLCODE                     NOT EQUAL ZEROS) OR
              (SQLWARN0                    EQUAL 'W')
               INITIALIZE                  GFCT0M-ERRO-SQL
               MOVE '0010'                 TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0B2'          TO WRK-NOME-TAB
               MOVE 'PDIDO_FLEXZ_AGPTO'    TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '             TO GFCT0M-COMANDO-SQL
               MOVE '0050'                 TO GFCT0M-LOCAL
               MOVE  0010                  TO GFCTNC-COD-MSG-ERRO
               PERFORM 1410-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS AND
               CSIT-SOLTC-FLEXZ        OF GFCTB0B2
                                       EQUAL 1
               MOVE 1                  TO GFCTNC-ERRO
               MOVE ZEROS              TO GFCTNC-COD-SQL-ERRO
               MOVE 1021               TO GFCTNC-COD-MSG-ERRO
               MOVE '1021'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNC-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS AND
              (CSIT-SOLTC-FLEXZ        OF GFCTB0B2
                                       EQUAL 3 OR 4)
               MOVE 1                  TO GFCTNC-ERRO
               MOVE ZEROS              TO GFCTNC-COD-SQL-ERRO
               MOVE 0645               TO GFCTNC-COD-MSG-ERRO
               MOVE '0645'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNC-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           PERFORM 2125-CONSISTIR-EXISTENCIA-PED.

      *----------------------------------------------------------------*
       2120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA CONSISTIR EXISTENCIA DE PEDIDO DE AGRUPAMENTO       *
      ******************************************************************
      *----------------------------------------------------------------*
       2125-CONSISTIR-EXISTENCIA-PED   SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTNB-AGRUPAMENTO     TO CAGPTO-CTA        OF GFCTB0B1.
           MOVE GFCTNB-COD-TARIFA      TO CSERVC-TARIF      OF GFCTB0B1.
050908***  MOVE GFCTNB-TIMESTAMP       TO HIDTFD-FLEXZ-AGPTO
050908***                                                   OF GFCTB0B2.

           IF  GFCTNB-TODAS-AG         EQUAL 'S'
               MOVE 00000              TO WRK-CDEPDC-INICIAL
               MOVE 99999              TO WRK-CDEPDC-FINAL
           ELSE
               MOVE 00000              TO WRK-CDEPDC-INICIAL
                                          WRK-CDEPDC-FINAL
           END-IF.

           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO DFIM-FLEXZ-AGPTO  OF GFCTB0B2.

           EXEC SQL
             SELECT B.CDEPDC,
                    B.HIDTFD_FLEXZ_AGPTO
             INTO   :GFCTB0B1.CDEPDC,
                    :GFCTB0B1.HIDTFD-FLEXZ-AGPTO
             FROM   DB2PRD.PDIDO_FLEXZ_AGPTO A,
                    DB2PRD.PDIDO_FLEXZ_AG    B
             WHERE  A.CAGPTO_CTA         = B.CAGPTO_CTA
               AND  A.CSERVC_TARIF       = B.CSERVC_TARIF
050908***      AND  A.HIDTFD_FLEXZ_AGPTO = :GFCTB0B2.HIDTFD-FLEXZ-AGPTO
               AND  B.CAGPTO_CTA         = :GFCTB0B1.CAGPTO-CTA
               AND  B.CSERVC_TARIF       = :GFCTB0B1.CSERVC-TARIF
               AND (B.CDEPDC            >= :WRK-CDEPDC-INICIAL
               AND  B.CDEPDC            <= :WRK-CDEPDC-FINAL)
               AND  A.DINIC_FLEXZ_AGPTO <= :GFCTB0B2.DINIC-FLEXZ-AGPTO
               AND  A.DFIM_FLEXZ_AGPTO  >= :GFCTB0B2.DFIM-FLEXZ-AGPTO
               AND  A.CSIT_SOLTC_FLEXZ  IN (01, 02)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND
               SQLCODE                 NOT EQUAL +100  AND
               SQLCODE                 NOT EQUAL -811) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE                 GFCT0M-ERRO-SQL
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0B2'      TO WRK-NOME-TAB
               MOVE 'PDIDO_FLEXZ_AGPTO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
               MOVE '0060'             TO GFCT0M-LOCAL
               MOVE  0010              TO GFCTNC-COD-MSG-ERRO
               PERFORM 1410-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

050908***  IF  SQLCODE                 EQUAL ZEROS OR -811
050908     IF  SQLCODE                 EQUAL -811
               MOVE 1                  TO GFCTNC-ERRO
               MOVE ZEROS              TO GFCTNC-COD-SQL-ERRO

               IF  GFCTNB-TODAS-AG     EQUAL 'S'
                   MOVE 1001           TO GFCTNC-COD-MSG-ERRO
                   MOVE '1001'         TO GFCTG2-COD-MSG
               ELSE
                   MOVE 1002           TO GFCTNC-COD-MSG-ERRO
                   MOVE '1002'         TO GFCTG2-COD-MSG
               END-IF

               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNC-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

050908     IF  SQLCODE                 EQUAL ZEROS
050908         IF GFCTNB-TIMESTAMP NOT EQUAL HIDTFD-FLEXZ-AGPTO
050908                                                      OF GFCTB0B1
050908             MOVE 1                  TO GFCTNC-ERRO
050908             MOVE ZEROS              TO GFCTNC-COD-SQL-ERRO
050908
050908             IF  GFCTNB-TODAS-AG     EQUAL 'S'
050908                 MOVE 1001           TO GFCTNC-COD-MSG-ERRO
050908                 MOVE '1001'         TO GFCTG2-COD-MSG
050908             ELSE
050908                 MOVE 1002           TO GFCTNC-COD-MSG-ERRO
050908                 MOVE '1002'         TO GFCTG2-COD-MSG
050908             END-IF
050908
050908             PERFORM 1310-OBTER-DESC-MSG
050908             MOVE GFCTG3-DESC-MSG    TO GFCTNC-DESC-MSG-ERRO
050908             PERFORM 3000-FINALIZAR
050908         END-IF
050908     END-IF.
050908

      *----------------------------------------------------------------*
       2125-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA CONSISTIR PEDIDO POSTAL                             *
      ******************************************************************
      *----------------------------------------------------------------*
       2130-CONSISTIR-PED-POSTAL       SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTNB-AGENCIA         TO CDEPDC            OF GFCTB0B9.
           MOVE GFCTNB-POSTO           TO CPOSTO-SERVC      OF GFCTB0B9.
           MOVE GFCTNB-COD-TARIFA      TO CSERVC-TARIF      OF GFCTB0B9
                                          CSERVC-TARIF      OF GFCTB0B2.
           MOVE GFCTNB-AGRUPAMENTO     TO CAGPTO-CTA        OF GFCTB0B9
                                          CAGPTO-CTA        OF GFCTB0B2.

           EXEC SQL
             SELECT A.CSIT_SOLTC_FLEXZ,
                    A.DINIC_FLEXZ_AGPTO
             INTO   :GFCTB0B2.CSIT-SOLTC-FLEXZ,
                    :GFCTB0B2.DINIC-FLEXZ-AGPTO
             FROM   DB2PRD.PDIDO_FLEXZ_AGPTO A,
                    DB2PRD.PDIDO_FLEXZ_PSTAL B
             WHERE  A.CAGPTO_CTA         = B.CAGPTO_CTA
               AND  A.CSERVC_TARIF       = B.CSERVC_TARIF
               AND  A.HIDTFD_FLEXZ_AGPTO = B.HIDTFD_FLEXZ_AGPTO
               AND  A.CAGPTO_CTA         = :GFCTB0B2.CAGPTO-CTA
               AND  A.CSERVC_TARIF       = :GFCTB0B2.CSERVC-TARIF
               AND  B.CDEPDC             = :GFCTB0B9.CDEPDC
               AND  B.CPOSTO_SERVC       = :GFCTB0B9.CPOSTO-SERVC
               AND  B.HIDTFD_FLEXZ_AGPTO IN
              (SELECT MAX(HIDTFD_FLEXZ_AGPTO)
               FROM  DB2PRD.PDIDO_FLEXZ_PSTAL
               WHERE CAGPTO_CTA          = :GFCTB0B9.CAGPTO-CTA
                AND  CSERVC_TARIF        = :GFCTB0B9.CSERVC-TARIF
                AND  CDEPDC              = :GFCTB0B9.CDEPDC
                AND  CPOSTO_SERVC        = :GFCTB0B9.CPOSTO-SERVC)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0B2'      TO WRK-NOME-TAB
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
               MOVE '0070'             TO GFCT0M-LOCAL
               MOVE  0010              TO GFCTNC-COD-MSG-ERRO
               PERFORM 1410-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS AND
               CSIT-SOLTC-FLEXZ        OF GFCTB0B2
                                       EQUAL 1
               MOVE 1                  TO GFCTNC-ERRO
               MOVE ZEROS              TO GFCTNC-COD-SQL-ERRO
               MOVE 1021               TO GFCTNC-COD-MSG-ERRO
               MOVE '1021'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNC-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS AND
              (CSIT-SOLTC-FLEXZ        OF GFCTB0B2
                                       EQUAL 3 OR 4)
               MOVE 1                  TO GFCTNC-ERRO
               MOVE ZEROS              TO GFCTNC-COD-SQL-ERRO
               MOVE 0645               TO GFCTNC-COD-MSG-ERRO
               MOVE '0645'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNC-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA CONSISTIR PEDIDO CLIENTE                            *
      ******************************************************************
      *----------------------------------------------------------------*
       2140-CONSISTIR-PED-CLIENTE      SECTION.
      *----------------------------------------------------------------*

ST2506*    MOVE GFCTNB-CGC-CPF         TO CCGC-CPF          OF GFCTB0B3.
ST2506*    MOVE GFCTNB-FILIAL          TO CFLIAL-CGC        OF GFCTB0B3.
ST2506*    MOVE GFCTNB-CONTROLE        TO CCTRL-CPF-CGC     OF GFCTB0B3.
ST2506     MOVE GFCTNB-CGC-CPF         TO CCGC-CPF-ST       OF GFCTB0B3.
ST2506     MOVE GFCTNB-FILIAL          TO CFLIAL-CGC-ST     OF GFCTB0B3.
ST2506     MOVE GFCTNB-CONTROLE        TO CCTRL-CPF-CGC-ST  OF GFCTB0B3.
           MOVE GFCTNB-COD-TARIFA      TO CSERVC-TARIF      OF GFCTB0B3
                                          CSERVC-TARIF      OF GFCTB0B2.
           MOVE GFCTNB-AGRUPAMENTO     TO CAGPTO-CTA        OF GFCTB0B3
                                          CAGPTO-CTA        OF GFCTB0B2.

           EXEC SQL
             SELECT A.CSIT_SOLTC_FLEXZ,
                    A.DINIC_FLEXZ_AGPTO
             INTO   :GFCTB0B2.CSIT-SOLTC-FLEXZ,
                    :GFCTB0B2.DINIC-FLEXZ-AGPTO
             FROM   DB2PRD.PDIDO_FLEXZ_AGPTO   A,
                    DB2PRD.PDIDO_FLEXZ_CLI     B
             WHERE  A.CAGPTO_CTA         = B.CAGPTO_CTA
               AND  A.CSERVC_TARIF       = B.CSERVC_TARIF
               AND  A.HIDTFD_FLEXZ_AGPTO = B.HIDTFD_FLEXZ_AGPTO
               AND  A.CAGPTO_CTA         = :GFCTB0B2.CAGPTO-CTA
               AND  A.CSERVC_TARIF       = :GFCTB0B2.CSERVC-TARIF
ST2506*        AND  B.CCGC_CPF           = :GFCTB0B3.CCGC-CPF
ST2506*        AND  B.CFLIAL_CGC         = :GFCTB0B3.CFLIAL-CGC
ST2506*        AND  B.CCTRL_CPF_CGC      = :GFCTB0B3.CCTRL-CPF-CGC
ST2506         AND  B.CCGC_CPF_ST        = :GFCTB0B3.CCGC-CPF-ST
ST2506         AND  B.CFLIAL_CGC_ST      = :GFCTB0B3.CFLIAL-CGC-ST
ST2506         AND  B.CCTRL_CPF_CGC_ST   = :GFCTB0B3.CCTRL-CPF-CGC-ST
               AND  B.HIDTFD_FLEXZ_AGPTO IN
              (SELECT MAX(HIDTFD_FLEXZ_AGPTO)
               FROM  DB2PRD.PDIDO_FLEXZ_CLI
               WHERE CAGPTO_CTA          = :GFCTB0B3.CAGPTO-CTA
                AND  CSERVC_TARIF        = :GFCTB0B3.CSERVC-TARIF
ST2506*         AND  CCGC_CPF            = :GFCTB0B3.CCGC-CPF
ST2506*         AND  CFLIAL_CGC          = :GFCTB0B3.CFLIAL-CGC
ST2506*         AND  CCTRL_CPF_CGC       = :GFCTB0B3.CCTRL-CPF-CGC)
ST2506          AND  CCGC_CPF_ST         = :GFCTB0B3.CCGC-CPF-ST
ST2506          AND  CFLIAL_CGC_ST       = :GFCTB0B3.CFLIAL-CGC-ST
ST2506          AND  CCTRL_CPF_CGC_ST    = :GFCTB0B3.CCTRL-CPF-CGC-ST)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0B2'      TO WRK-NOME-TAB
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
               MOVE '0080'             TO GFCT0M-LOCAL
               MOVE  0010              TO GFCTNC-COD-MSG-ERRO
               PERFORM 1410-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS AND
               CSIT-SOLTC-FLEXZ        OF GFCTB0B2
                                       EQUAL 1
               MOVE 1                  TO GFCTNC-ERRO
               MOVE ZEROS              TO GFCTNC-COD-SQL-ERRO
               MOVE 1021               TO GFCTNC-COD-MSG-ERRO
               MOVE '1021'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNC-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS AND
              (CSIT-SOLTC-FLEXZ        OF GFCTB0B2
                                       EQUAL 3 OR 4)
               MOVE 1                  TO GFCTNC-ERRO
               MOVE ZEROS              TO GFCTNC-COD-SQL-ERRO
               MOVE 0645               TO GFCTNC-COD-MSG-ERRO
               MOVE '0645'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNC-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2140-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA CONSISTIR PEDIDO SEGMENTO                           *
      ******************************************************************
      *----------------------------------------------------------------*
       2150-CONSISTIR-PED-SEGMENTO     SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTNB-SEGMENTO       TO CSGMTO-GSTAO-TARIF OF GFCTB0C0.
           MOVE GFCTNB-COD-TARIFA     TO CSERVC-TARIF       OF GFCTB0C0
                                         CSERVC-TARIF       OF GFCTB0B2.
           MOVE GFCTNB-AGRUPAMENTO    TO CAGPTO-CTA         OF GFCTB0C0
                                         CAGPTO-CTA         OF GFCTB0B2.

           EXEC SQL
            SELECT A.CSIT_SOLTC_FLEXZ,
                   A.DINIC_FLEXZ_AGPTO
            INTO   :GFCTB0B2.CSIT-SOLTC-FLEXZ,
                   :GFCTB0B2.DINIC-FLEXZ-AGPTO
            FROM   DB2PRD.PDIDO_FLEXZ_AGPTO   A,
                   DB2PRD.PDIDO_FLEXZ_SGMTO   B
             WHERE A.CAGPTO_CTA         = B.CAGPTO_CTA
              AND  A.CSERVC_TARIF       = B.CSERVC_TARIF
              AND  A.HIDTFD_FLEXZ_AGPTO = B.HIDTFD_FLEXZ_AGPTO
              AND  A.CAGPTO_CTA         = :GFCTB0B2.CAGPTO-CTA
              AND  A.CSERVC_TARIF       = :GFCTB0B2.CSERVC-TARIF
              AND  B.CSGMTO_GSTAO_TARIF = :GFCTB0C0.CSGMTO-GSTAO-TARIF
              AND  B.HIDTFD_FLEXZ_AGPTO IN
             (SELECT MAX(HIDTFD_FLEXZ_AGPTO)
              FROM  DB2PRD.PDIDO_FLEXZ_SGMTO
              WHERE CAGPTO_CTA          = :GFCTB0C0.CAGPTO-CTA
               AND  CSERVC_TARIF        = :GFCTB0C0.CSERVC-TARIF
               AND  CSGMTO_GSTAO_TARIF  = :GFCTB0C0.CSGMTO-GSTAO-TARIF)
           END-EXEC.

           IF (SQLCODE                    NOT EQUAL ZEROS) OR
              (SQLWARN0                   EQUAL 'W')
               INITIALIZE                 GFCT0M-ERRO-SQL
               MOVE '0010'                TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0B2'         TO WRK-NOME-TAB
               MOVE 'PDIDO_FLEXZ_AGPTO'   TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '            TO GFCT0M-COMANDO-SQL
               MOVE '0090'                TO GFCT0M-LOCAL
               MOVE  0010                 TO GFCTNC-COD-MSG-ERRO
               PERFORM 1410-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS AND
               CSIT-SOLTC-FLEXZ        OF GFCTB0B2
                                       EQUAL 1
               MOVE 1                  TO GFCTNC-ERRO
               MOVE ZEROS              TO GFCTNC-COD-SQL-ERRO
               MOVE 1021               TO GFCTNC-COD-MSG-ERRO
               MOVE '1021'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNC-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS AND
              (CSIT-SOLTC-FLEXZ        OF GFCTB0B2
                                       EQUAL 3 OR 4)
               MOVE 1                  TO GFCTNC-ERRO
               MOVE ZEROS              TO GFCTNC-COD-SQL-ERRO
               MOVE 0645               TO GFCTNC-COD-MSG-ERRO
               MOVE '0645'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNC-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2150-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA CONSISTIR PEDIDO MUNICIPIO                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2160-CONSISTIR-PED-MUNICIPIO    SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTNB-MUNIC           TO CMUN-IBGE         OF GFCTB0B7.
           MOVE GFCTNB-COD-TARIFA      TO CSERVC-TARIF      OF GFCTB0B7
                                          CSERVC-TARIF      OF GFCTB0B2.
           MOVE GFCTNB-AGRUPAMENTO     TO CAGPTO-CTA        OF GFCTB0B7
                                          CAGPTO-CTA        OF GFCTB0B2.

           EXEC SQL
             SELECT A.CSIT_SOLTC_FLEXZ,
                    A.DINIC_FLEXZ_AGPTO
             INTO   :GFCTB0B2.CSIT-SOLTC-FLEXZ,
                    :GFCTB0B2.DINIC-FLEXZ-AGPTO
             FROM   DB2PRD.PDIDO_FLEXZ_AGPTO   A,
                    DB2PRD.PDIDO_FLEXZ_MUN     B
             WHERE  A.CAGPTO_CTA         = B.CAGPTO_CTA
               AND  A.CSERVC_TARIF       = B.CSERVC_TARIF
               AND  A.HIDTFD_FLEXZ_AGPTO = B.HIDTFD_FLEXZ_AGPTO
               AND  A.CAGPTO_CTA         = :GFCTB0B2.CAGPTO-CTA
               AND  A.CSERVC_TARIF       = :GFCTB0B2.CSERVC-TARIF
               AND  B.CMUN_IBGE          = :GFCTB0B7.CMUN-IBGE
               AND  B.HIDTFD_FLEXZ_AGPTO IN
              (SELECT MAX(HIDTFD_FLEXZ_AGPTO)
               FROM  DB2PRD.PDIDO_FLEXZ_MUN
               WHERE CAGPTO_CTA          = :GFCTB0B7.CAGPTO-CTA
                AND  CSERVC_TARIF        = :GFCTB0B7.CSERVC-TARIF
                AND  CMUN_IBGE           = :GFCTB0B7.CMUN-IBGE)
           END-EXEC.

           IF (SQLCODE                     NOT EQUAL ZEROS) OR
              (SQLWARN0                    EQUAL 'W')
               INITIALIZE                  GFCT0M-ERRO-SQL
               MOVE '0010'                 TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0B2'          TO WRK-NOME-TAB
               MOVE 'PDIDO_FLEXZ_AGPTO'    TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '             TO GFCT0M-COMANDO-SQL
               MOVE '0100'                 TO GFCT0M-LOCAL
               MOVE  0010                  TO GFCTNC-COD-MSG-ERRO
               PERFORM 1410-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS AND
               CSIT-SOLTC-FLEXZ        OF GFCTB0B2
                                       EQUAL 1
               MOVE 1                  TO GFCTNC-ERRO
               MOVE ZEROS              TO GFCTNC-COD-SQL-ERRO
               MOVE 1021               TO GFCTNC-COD-MSG-ERRO
               MOVE '1021'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNC-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS AND
              (CSIT-SOLTC-FLEXZ        OF GFCTB0B2
                                       EQUAL 3 OR 4)
               MOVE 1                  TO GFCTNC-ERRO
               MOVE ZEROS              TO GFCTNC-COD-SQL-ERRO
               MOVE 0645               TO GFCTNC-COD-MSG-ERRO
               MOVE '0645'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNC-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2160-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA CONSISTIR PEDIDO UF                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2170-CONSISTIR-PED-UF           SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTNB-UF              TO CSGL-UF           OF GFCTB0C1.
           MOVE GFCTNB-COD-TARIFA      TO CSERVC-TARIF      OF GFCTB0C1
                                          CSERVC-TARIF      OF GFCTB0B2.
           MOVE GFCTNB-AGRUPAMENTO     TO CAGPTO-CTA        OF GFCTB0C1
                                          CAGPTO-CTA        OF GFCTB0B2.

           EXEC SQL
             SELECT A.CSIT_SOLTC_FLEXZ,
                    A.DINIC_FLEXZ_AGPTO
             INTO   :GFCTB0B2.CSIT-SOLTC-FLEXZ,
                    :GFCTB0B2.DINIC-FLEXZ-AGPTO
             FROM   DB2PRD.PDIDO_FLEXZ_AGPTO   A,
                    DB2PRD.PDIDO_FLEXZ_UF      B
             WHERE  A.CAGPTO_CTA         = B.CAGPTO_CTA
               AND  A.CSERVC_TARIF       = B.CSERVC_TARIF
               AND  A.HIDTFD_FLEXZ_AGPTO = B.HIDTFD_FLEXZ_AGPTO
               AND  A.CAGPTO_CTA         = :GFCTB0B2.CAGPTO-CTA
               AND  A.CSERVC_TARIF       = :GFCTB0B2.CSERVC-TARIF
               AND  B.CSGL_UF            = :GFCTB0C1.CSGL-UF
               AND  B.HIDTFD_FLEXZ_AGPTO IN
              (SELECT MAX(HIDTFD_FLEXZ_AGPTO)
               FROM  DB2PRD.PDIDO_FLEXZ_UF
               WHERE CAGPTO_CTA          = :GFCTB0C1.CAGPTO-CTA
                AND  CSERVC_TARIF        = :GFCTB0C1.CSERVC-TARIF
                AND  CSGL_UF             = :GFCTB0C1.CSGL-UF)
           END-EXEC.

           IF (SQLCODE                     NOT EQUAL ZEROS) OR
              (SQLWARN0                    EQUAL 'W')
               INITIALIZE                  GFCT0M-ERRO-SQL
               MOVE '0010'                 TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0B2'          TO WRK-NOME-TAB
               MOVE 'PDIDO_FLEXZ_AGPTO'    TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '             TO GFCT0M-COMANDO-SQL
               MOVE '0110'                 TO GFCT0M-LOCAL
               MOVE  0010                  TO GFCTNC-COD-MSG-ERRO
               PERFORM 1410-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.
           IF  SQLCODE                 EQUAL ZEROS AND
               CSIT-SOLTC-FLEXZ        OF GFCTB0B2
                                       EQUAL 1
               MOVE 1                  TO GFCTNC-ERRO
               MOVE ZEROS              TO GFCTNC-COD-SQL-ERRO
               MOVE 1021               TO GFCTNC-COD-MSG-ERRO
               MOVE '1021'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNC-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS AND
              (CSIT-SOLTC-FLEXZ        OF GFCTB0B2
                                       EQUAL 3 OR 4)
               MOVE 1                  TO GFCTNC-ERRO
               MOVE ZEROS              TO GFCTNC-COD-SQL-ERRO
               MOVE 0645               TO GFCTNC-COD-MSG-ERRO
               MOVE '0645'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNC-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2170-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

080508******************************************************************
080508*ROTINA PARA CONSISTIR PEDIDO FUNCIONARIO BRADESCO               *
080508******************************************************************
080508*----------------------------------------------------------------*
080508 2180-CONSISTIR-PED-FUNC-BRAD    SECTION.
080508*----------------------------------------------------------------*
080508
080508     MOVE GFCTNB-COD-TARIFA      TO CSERVC-TARIF      OF GFCTB0B2.
080508     MOVE GFCTNB-AGRUPAMENTO     TO CAGPTO-CTA        OF GFCTB0B2.
080508     MOVE DPROCM-ATUAL           OF GFCTB0A1
080508                                 TO DINIC-FLEXZ-AGPTO OF GFCTB0B2
080508                                    DFIM-FLEXZ-AGPTO  OF GFCTB0B2.
080508
080508     EXEC SQL
080508       SELECT CSIT_SOLTC_FLEXZ
080508       INTO   :GFCTB0B2.CSIT-SOLTC-FLEXZ
080508       FROM   DB2PRD.PDIDO_FLEXZ_AGPTO
080508       WHERE  CAGPTO_CTA         = :GFCTB0B2.CAGPTO-CTA
080508         AND  CSERVC_TARIF       = :GFCTB0B2.CSERVC-TARIF
080508         AND  DFIM_FLEXZ_AGPTO  >= :GFCTB0B2.DFIM-FLEXZ-AGPTO
080508         AND  HIDTFD_FLEXZ_AGPTO IN
080508        (SELECT MAX(HIDTFD_FLEXZ_AGPTO)
080508         FROM  DB2PRD.PDIDO_FLEXZ_AGPTO
080508         WHERE CAGPTO_CTA         = :GFCTB0B2.CAGPTO-CTA
080508          AND  CSERVC_TARIF       = :GFCTB0B2.CSERVC-TARIF
080508          AND  DFIM_FLEXZ_AGPTO  >= :GFCTB0B2.DFIM-FLEXZ-AGPTO)
080508     END-EXEC.
080508
080508     IF (SQLCODE                     NOT EQUAL ZEROS AND +100) OR
080508        (SQLWARN0                    EQUAL 'W')
080508         INITIALIZE                  GFCT0M-ERRO-SQL
080508         MOVE '0010'                 TO GFCTG2-COD-MSG
080508         PERFORM 1310-OBTER-DESC-MSG
080508         MOVE ' - GFCTB0B2'          TO WRK-NOME-TAB
080508         MOVE 'PDIDO_FLEXZ_AGPTO'    TO GFCT0M-NOME-TAB
080508         MOVE 'SELECT  '             TO GFCT0M-COMANDO-SQL
080508         MOVE '0120'                 TO GFCT0M-LOCAL
080508         MOVE  0010                  TO GFCTNC-COD-MSG-ERRO
080508         PERFORM 1410-MOVER-ERRO-SQL
080508         PERFORM 3000-FINALIZAR
080508     END-IF.
080508
080508     IF  SQLCODE                     EQUAL ZEROS AND
080508        (CSIT-SOLTC-FLEXZ            OF GFCTB0B2
080508                                     EQUAL 3 OR 4)
080508         MOVE 1                      TO GFCTNC-ERRO
080508         MOVE ZEROS                  TO GFCTNC-COD-SQL-ERRO
080508         MOVE 0645                   TO GFCTNC-COD-MSG-ERRO
080508         MOVE '0645'                 TO GFCTG2-COD-MSG
080508         PERFORM 1310-OBTER-DESC-MSG
080508         MOVE GFCTG3-DESC-MSG        TO GFCTNC-DESC-MSG-ERRO
080508         PERFORM 3000-FINALIZAR
080508     END-IF.
080508
080508     IF  SQLCODE                     EQUAL ZEROS AND
080508         CSIT-SOLTC-FLEXZ            OF GFCTB0B2
080508                                     EQUAL 1
080508         MOVE 1                      TO GFCTNC-ERRO
080508         MOVE ZEROS                  TO GFCTNC-COD-SQL-ERRO
080508         MOVE 0920                   TO GFCTNC-COD-MSG-ERRO
080508         MOVE '0920'                 TO GFCTG2-COD-MSG
080508         PERFORM 1310-OBTER-DESC-MSG
080508         MOVE GFCTG3-DESC-MSG        TO GFCTNC-DESC-MSG-ERRO
080508         PERFORM 3000-FINALIZAR
080508     END-IF.
080508
080508*----------------------------------------------------------------*
080508 2180-99-FIM.                    EXIT.
080508*----------------------------------------------------------------*
080508
080508******************************************************************
080508*ROTINA PARA CONSISTIR PEDIDO AGRUPAMENTO PERSONALIZADO          *
080508******************************************************************
080508*----------------------------------------------------------------*
080508 2190-CONS-PED-AGRUP-PERSON SECTION.
080508*----------------------------------------------------------------*
080508
080508     MOVE GFCTNB-COD-TARIFA      TO CSERVC-TARIF      OF GFCTB0B2.
080508     MOVE GFCTNB-AGRUPAMENTO     TO CAGPTO-CTA        OF GFCTB0B2.
080508     MOVE DPROCM-ATUAL           OF GFCTB0A1
080508                                 TO DINIC-FLEXZ-AGPTO OF GFCTB0B2
080508                                    DFIM-FLEXZ-AGPTO  OF GFCTB0B2.
080508
080508     EXEC SQL
080508       SELECT CSIT_SOLTC_FLEXZ
080508       INTO   :GFCTB0B2.CSIT-SOLTC-FLEXZ
080508       FROM   DB2PRD.PDIDO_FLEXZ_AGPTO
080508       WHERE  CAGPTO_CTA         = :GFCTB0B2.CAGPTO-CTA
080508         AND  CSERVC_TARIF       = :GFCTB0B2.CSERVC-TARIF
080508         AND  DFIM_FLEXZ_AGPTO  >= :GFCTB0B2.DFIM-FLEXZ-AGPTO
080508         AND  HIDTFD_FLEXZ_AGPTO IN
080508        (SELECT MAX(HIDTFD_FLEXZ_AGPTO)
080508         FROM  DB2PRD.PDIDO_FLEXZ_AGPTO
080508         WHERE CAGPTO_CTA         = :GFCTB0B2.CAGPTO-CTA
080508          AND  CSERVC_TARIF       = :GFCTB0B2.CSERVC-TARIF
080508          AND  DFIM_FLEXZ_AGPTO  >= :GFCTB0B2.DFIM-FLEXZ-AGPTO)
080508     END-EXEC.
080508
080508     IF (SQLCODE                     NOT EQUAL ZEROS AND +100) OR
080508        (SQLWARN0                    EQUAL 'W')
080508         INITIALIZE                  GFCT0M-ERRO-SQL
080508         MOVE '0010'                 TO GFCTG2-COD-MSG
080508         PERFORM 1310-OBTER-DESC-MSG
080508         MOVE ' - GFCTB0B2'          TO WRK-NOME-TAB
080508         MOVE 'PDIDO_FLEXZ_AGPTO'    TO GFCT0M-NOME-TAB
080508         MOVE 'SELECT  '             TO GFCT0M-COMANDO-SQL
080508         MOVE '0130'                 TO GFCT0M-LOCAL
080508         MOVE  0010                  TO GFCTNC-COD-MSG-ERRO
080508         PERFORM 1410-MOVER-ERRO-SQL
080508         PERFORM 3000-FINALIZAR
080508     END-IF.
080508
080508     IF  SQLCODE                     EQUAL ZEROS AND
080508        (CSIT-SOLTC-FLEXZ            OF GFCTB0B2
080508                                     EQUAL 3 OR 4)
080508         MOVE 1                      TO GFCTNC-ERRO
080508         MOVE ZEROS                  TO GFCTNC-COD-SQL-ERRO
080508         MOVE 0645                   TO GFCTNC-COD-MSG-ERRO
080508         MOVE '0645'                 TO GFCTG2-COD-MSG
080508         PERFORM 1310-OBTER-DESC-MSG
080508         MOVE GFCTG3-DESC-MSG        TO GFCTNC-DESC-MSG-ERRO
080508         PERFORM 3000-FINALIZAR
080508     END-IF.
080508
080508     IF  SQLCODE                     EQUAL ZEROS AND
080508         CSIT-SOLTC-FLEXZ            OF GFCTB0B2
080508                                     EQUAL 1
080508         MOVE 1                      TO GFCTNC-ERRO
080508         MOVE ZEROS                  TO GFCTNC-COD-SQL-ERRO
080508         MOVE 0920                   TO GFCTNC-COD-MSG-ERRO
080508         MOVE '0920'                 TO GFCTG2-COD-MSG
080508         PERFORM 1310-OBTER-DESC-MSG
080508         MOVE GFCTG3-DESC-MSG        TO GFCTNC-DESC-MSG-ERRO
080508         PERFORM 3000-FINALIZAR
080508     END-IF.
080508
080508*----------------------------------------------------------------*
080508 2190-99-FIM.                    EXIT.
080508*----------------------------------------------------------------*

0310  ******************************************************************
0310  *ROTINA PARA CONSISTIR PEDIDO RAZAO                              *
0310  ******************************************************************
0310  *----------------------------------------------------------------*
0310   2195-CONSISTIR-PED-RAZAO        SECTION.
0310  *----------------------------------------------------------------*
0310
0310       MOVE GFCTNB-GRUPO-CTBIL     TO CGRP-CTBIL-ATUAL  OF GFCTB0M7
0310       MOVE GFCTNB-SUB-GRUPO-CTBIL TO CSGRP-CTBIL       OF GFCTB0M7
0310       MOVE GFCTNB-COD-TARIFA      TO CSERVC-TARIF      OF GFCTB0M7
0310       MOVE GFCTNB-AGRUPAMENTO     TO CAGPTO-CTA        OF GFCTB0M7
0310
0310       PERFORM 2196-MAX-PED-RAZAO
0310
0310       EXEC SQL
0310         SELECT A.CSIT_SOLTC_FLEXZ ,
0310                A.DFIM_FLEXZ_AGPTO
0310         INTO   :GFCTB0B2.CSIT-SOLTC-FLEXZ,
0310                :GFCTB0B2.DFIM-FLEXZ-AGPTO
0310         FROM   DB2PRD.PDIDO_FLEXZ_AGPTO A,
0310                DB2PRD.TPDIDO_FLEXZ_RZ   B
0310         WHERE  A.CAGPTO_CTA         = B.CAGPTO_CTA
0310           AND  A.CSERVC_TARIF       = B.CSERVC_TARIF
0310           AND  A.HIDTFD_FLEXZ_AGPTO = B.HIDTFD_FLEXZ_AGPTO
0310           AND  B.CAGPTO_CTA         = :GFCTB0M7.CAGPTO-CTA
0310           AND  B.CSERVC_TARIF       = :GFCTB0M7.CSERVC-TARIF
0310           AND  B.CGRP_CTBIL_ATUAL   = :GFCTB0M7.CGRP-CTBIL-ATUAL
0310           AND  B.CSGRP_CTBIL        = :GFCTB0M7.CSGRP-CTBIL
0310           AND  B.HIDTFD_FLEXZ_AGPTO = :GFCTB0M7.HIDTFD-FLEXZ-AGPTO
0310       END-EXEC.
0310
0310       IF (SQLCODE                     NOT EQUAL ZEROS AND +100) OR
0310          (SQLWARN0                    EQUAL 'W')
0310           INITIALIZE                  GFCT0M-ERRO-SQL
0310           MOVE '0010'                 TO GFCTG2-COD-MSG
0310           PERFORM 1310-OBTER-DESC-MSG
0310           MOVE ' - GFCTB0B2'          TO WRK-NOME-TAB
0310           MOVE 'PDIDO_FLEXZ_AGPTO'    TO GFCT0M-NOME-TAB
0310           MOVE 'SELECT  '             TO GFCT0M-COMANDO-SQL
0310           MOVE '0140'                 TO GFCT0M-LOCAL
0310           MOVE  0010                  TO GFCTNC-COD-MSG-ERRO
0310           PERFORM 1410-MOVER-ERRO-SQL
0310           PERFORM 3000-FINALIZAR
0310       END-IF.
0310
0310       IF  SQLCODE                     EQUAL ZEROS AND
0310          (CSIT-SOLTC-FLEXZ            OF GFCTB0B2
0310                                       EQUAL 3 OR 4)
0310           MOVE 1                      TO GFCTNC-ERRO
0310           MOVE ZEROS                  TO GFCTNC-COD-SQL-ERRO
0310           MOVE 0645                   TO GFCTNC-COD-MSG-ERRO
0310           MOVE '0645'                 TO GFCTG2-COD-MSG
0310           PERFORM 1310-OBTER-DESC-MSG
0310           MOVE GFCTG3-DESC-MSG        TO GFCTNC-DESC-MSG-ERRO
0310           PERFORM 3000-FINALIZAR
0310       END-IF.
0310
0310       IF  SQLCODE                     EQUAL ZEROS AND
0310           CSIT-SOLTC-FLEXZ            OF GFCTB0B2
0310                                       EQUAL 1
0310           MOVE 1                      TO GFCTNC-ERRO
0310           MOVE ZEROS                  TO GFCTNC-COD-SQL-ERRO
0310           MOVE 0920                   TO GFCTNC-COD-MSG-ERRO
0310           MOVE '0920'                 TO GFCTG2-COD-MSG
0310           PERFORM 1310-OBTER-DESC-MSG
0310           MOVE GFCTG3-DESC-MSG        TO GFCTNC-DESC-MSG-ERRO
0310           PERFORM 3000-FINALIZAR
0310       END-IF.
0310
0310  *----------------------------------------------------------------*
0310   2195-99-FIM.                    EXIT.
0310  *----------------------------------------------------------------*

0310  ******************************************************************
0310  *ROTINA PARA OBTER O MAIOR-TIMESTAMP                             *
0310  ******************************************************************
0310  *----------------------------------------------------------------*
0310   2196-MAX-PED-RAZAO              SECTION.
0310  *----------------------------------------------------------------*
0310
0310       EXEC SQL
0310         SELECT MAX(B.HIDTFD_FLEXZ_AGPTO)
0310         INTO   :GFCTB0M7.HIDTFD-FLEXZ-AGPTO
0310         FROM   DB2PRD.PDIDO_FLEXZ_AGPTO A,
0310                DB2PRD.TPDIDO_FLEXZ_RZ    B
0310         WHERE  A.CAGPTO_CTA         = B.CAGPTO_CTA
0310           AND  A.CSERVC_TARIF       = B.CSERVC_TARIF
0310           AND  A.HIDTFD_FLEXZ_AGPTO = B.HIDTFD_FLEXZ_AGPTO
0310           AND  B.CAGPTO_CTA         = :GFCTB0M7.CAGPTO-CTA
0310           AND  B.CSERVC_TARIF       = :GFCTB0M7.CSERVC-TARIF
0310           AND  B.CGRP_CTBIL_ATUAL   = :GFCTB0M7.CGRP-CTBIL-ATUAL
0310           AND  B.CSGRP_CTBIL        = :GFCTB0M7.CSGRP-CTBIL
0310       END-EXEC.
0310
0310       IF (SQLCODE                     NOT EQUAL ZEROS AND -305) OR
0310          (SQLWARN0                    EQUAL 'W')
0310           INITIALIZE                  GFCT0M-ERRO-SQL
0310           MOVE '0010'                 TO GFCTG2-COD-MSG
0310           PERFORM 1310-OBTER-DESC-MSG
0310           MOVE ' - GFCTB0B2'          TO WRK-NOME-TAB
0310           MOVE 'TPDIDO_FLEXZ_AGPTO'   TO GFCT0M-NOME-TAB
0310           MOVE 'SELECT  '             TO GFCT0M-COMANDO-SQL
0310           MOVE '0150'                 TO GFCT0M-LOCAL
0310           MOVE  0010                  TO GFCTNC-COD-MSG-ERRO
0310           PERFORM 1410-MOVER-ERRO-SQL
0310           PERFORM 3000-FINALIZAR
0310       END-IF.
0310
0310       IF  SQLCODE                  EQUAL -305
0610XS         MOVE '0001-01-01-01.01.01.000001'
0610XS                                  TO
0610XS                                   HIDTFD-FLEXZ-AGPTO OF GFCTB0M7
0310       END-IF.
0310
0310  *----------------------------------------------------------------*
0310   2196-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA CONSISTIR DATAS                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-CONSISTIR-DATAS            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTNB-INIC-VIG-FLEX   TO WRK-DATA-DB2.
           MOVE WRK-DIA-DB2            TO WRK-DIA-INV
                                          WRK-DATA(1:2).
           MOVE WRK-MES-DB2            TO WRK-MES-INV
                                          WRK-DATA(3:2).
           MOVE WRK-ANO-DB2            TO WRK-ANO-INV
                                          WRK-DATA(5:4).

           CALL 'POOL1470'             USING WRK-DATA.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 1                  TO GFCTNC-ERRO
               MOVE ZEROS              TO GFCTNC-COD-SQL-ERRO
               MOVE 0801               TO GFCTNC-COD-MSG-ERRO
               MOVE '0801'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTNC-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE WRK-DATA-INV-R         TO WRK-INIC-FLEX
080508                                    WRK-DATA-INIC-FLEX.

           MOVE GFCTNB-FIM-VIG-FLEX    TO WRK-DATA-DB2.
           MOVE WRK-DIA-DB2            TO WRK-DIA-INV
                                          WRK-DATA(1:2).
           MOVE WRK-MES-DB2            TO WRK-MES-INV
                                          WRK-DATA(3:2).
           MOVE WRK-ANO-DB2            TO WRK-ANO-INV
                                          WRK-DATA(5:4).

           CALL 'POOL1470'             USING WRK-DATA.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 1                  TO GFCTNC-ERRO
               MOVE ZEROS              TO GFCTNC-COD-SQL-ERRO
               MOVE 0802               TO GFCTNC-COD-MSG-ERRO
               MOVE '0802'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTNC-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE WRK-DATA-INV-R         TO WRK-FIM-FLEX
080508                                    WRK-DATA-FIM-FLEX.

           PERFORM 2210-ACESSAR-GFCT5018.

           MOVE WRK-5018-DINIC-VGCIA-TARIF-P
                                       TO WRK-DATA-DB2.
           MOVE WRK-DIA-DB2            TO WRK-DIA-INV.
           MOVE WRK-MES-DB2            TO WRK-MES-INV.
           MOVE WRK-ANO-DB2            TO WRK-ANO-INV.
           MOVE WRK-DATA-INV-R         TO WRK-INIC-TARIF.
           MOVE WRK-5018-DFIM-VGCIA-TARIF
                                       TO WRK-DATA-DB2.
           MOVE WRK-DIA-DB2            TO WRK-DIA-INV.
           MOVE WRK-MES-DB2            TO WRK-MES-INV.
           MOVE WRK-ANO-DB2            TO WRK-ANO-INV.
           MOVE WRK-DATA-INV           TO WRK-FIM-TARIF.
           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO WRK-DATA-DB2.
           MOVE WRK-DIA-DB2            TO WRK-DIA-INV.
           MOVE WRK-MES-DB2            TO WRK-MES-INV.
           MOVE WRK-ANO-DB2            TO WRK-ANO-INV.
           MOVE WRK-DATA-INV           TO WRK-DATA-PROCS.

           IF  WRK-INIC-FLEX           LESS WRK-INIC-TARIF
               MOVE 1                  TO GFCTNC-ERRO
               MOVE ZEROS              TO GFCTNC-COD-SQL-ERRO
               MOVE 0610               TO GFCTNC-COD-MSG-ERRO
               MOVE '0610'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTNC-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-FIM-FLEX            GREATER WRK-FIM-TARIF
               MOVE 1                  TO GFCTNC-ERRO
               MOVE ZEROS              TO GFCTNC-COD-SQL-ERRO
               MOVE 0611               TO GFCTNC-COD-MSG-ERRO
               MOVE '0611'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTNC-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-FIM-FLEX            LESS WRK-INIC-FLEX
               MOVE 1                  TO GFCTNC-ERRO
               MOVE ZEROS              TO GFCTNC-COD-SQL-ERRO
               MOVE 0611               TO GFCTNC-COD-MSG-ERRO
               MOVE '0611'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTNC-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-INIC-FLEX           NOT GREATER WRK-DATA-PROCS
               MOVE 1                  TO GFCTNC-ERRO
               MOVE ZEROS              TO GFCTNC-COD-SQL-ERRO
               MOVE 0610               TO GFCTNC-COD-MSG-ERRO
               MOVE '0610'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTNC-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

080508     PERFORM 2220-ACESSAR-GFCTB0J7.
080508
080508     MOVE DINIC-PRMSS-FLEXZ      OF GFCTB0J7
080508                                 TO WRK-DATA-AUX.
080508     MOVE WRK-DIA-AUX            TO WRK-DIA-INV-PERM
080508     MOVE WRK-MES-AUX            TO WRK-MES-INV-PERM
080508     MOVE WRK-ANO-AUX            TO WRK-ANO-INV-PERM
080508     MOVE WRK-DATA-INV-PERM      TO WRK-DATA-INIC-PERMIS.
080508
080508     MOVE DFIM-PRMSS-FLEXZ       OF GFCTB0J7
080508                                 TO WRK-DATA-AUX.
080508     MOVE WRK-DIA-AUX            TO WRK-DIA-INV-PERM
080508     MOVE WRK-MES-AUX            TO WRK-MES-INV-PERM
080508     MOVE WRK-ANO-AUX            TO WRK-ANO-INV-PERM
080508     MOVE WRK-DATA-INV-PERM      TO WRK-DATA-FIM-PERMIS.
080508
080508     IF  WRK-DATA-INIC-FLEX      LESS WRK-DATA-INIC-PERMIS
080508         MOVE 1                  TO GFCTNC-ERRO
080508         MOVE ZEROS              TO GFCTNC-COD-SQL-ERRO
080508         MOVE 1210               TO GFCTNC-COD-MSG-ERRO
080508         MOVE '1210'             TO GFCTG2-COD-MSG
080508         PERFORM 1310-OBTER-DESC-MSG
080508         MOVE GFCTG3-DESC-MSG    TO GFCTNC-DESC-MSG-ERRO
080508         PERFORM 3000-FINALIZAR
080508     END-IF.
080508
080508     IF  WRK-DATA-FIM-FLEX       GREATER WRK-DATA-FIM-PERMIS
080508         MOVE 1                  TO GFCTNC-ERRO
080508         MOVE ZEROS              TO GFCTNC-COD-SQL-ERRO
080508         MOVE 1211               TO GFCTNC-COD-MSG-ERRO
080508         MOVE '1211'             TO GFCTG2-COD-MSG
080508         PERFORM 1310-OBTER-DESC-MSG
080508         MOVE GFCTG3-DESC-MSG    TO GFCTNC-DESC-MSG-ERRO
080508         PERFORM 3000-FINALIZAR
080508     END-IF.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA ACESSAR MODULO GFCT5018                             *
      ******************************************************************
      *----------------------------------------------------------------*
       2210-ACESSAR-GFCT5018           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-5018-AREA-ENTRADA
                      WRK-5018-AREA-SAIDA
                      WRK-5018-DADOS-RETORNO.

           MOVE 'O'                    TO WRK-5018-AMBIENTE.
           MOVE GFCTNB-COD-TARIFA      TO WRK-5018-CODIGO-TARIFA.

PROC****   MOVE DPROCM-ATUAL           OF GFCTB0A1
PROC****                               TO WRK-5018-DT-REFER.

PROC       MOVE GFCTNB-INIC-VIG-FLEX
PROC                                   TO WRK-5018-DT-REFER.

           MOVE 'GFCT5018'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                             WRK-5018-AREA-ENTRADA
                                             WRK-5018-AREA-SAIDA
                                             WRK-5018-DADOS-RETORNO
                                             WRK-IO-PCB
                                             WRK-ALT-PCB.

           IF  WRK-5018-COD-RETORNO    NOT EQUAL ZEROS AND 02
               MOVE 9                  TO GFCTNC-ERRO
               MOVE WRK-MODULO         TO WRK-NOME-MOD
               MOVE WRK-MSG01          TO GFCT0M-TEXTO
                                          GFCTNC-DESC-MSG-ERRO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0418'         TO GFCT0M-PROGRAMA
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-5018-COD-RETORNO    EQUAL 02
               MOVE 1                  TO GFCTNC-ERRO
               MOVE ZEROS              TO GFCTNC-COD-SQL-ERRO
               MOVE 1118               TO GFCTNC-COD-MSG-ERRO
               MOVE '1118'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTNC-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
080508*----------------------------------------------------------------*
080508* SELECAO NA TABELA GFCTB0J7                                     *
080508*----------------------------------------------------------------*
080508 2220-ACESSAR-GFCTB0J7           SECTION.
080508*----------------------------------------------------------------*
080508
080508     MOVE GFCTNB-AGRUPAMENTO     TO CAGPTO-CTA        OF GFCTB0J7.
080508     MOVE GFCTNB-COD-TARIFA      TO CSERVC-TARIF      OF GFCTB0J7.
080508     MOVE ZEROS                  TO CFUNC-ENCRR-VGCIA OF GFCTB0J7.
080508
080508     EXEC SQL
080508         SELECT  DINIC_PRMSS_FLEXZ,
080508                 DFIM_PRMSS_FLEXZ
080508         INTO   :GFCTB0J7.DINIC-PRMSS-FLEXZ,
080508                :GFCTB0J7.DFIM-PRMSS-FLEXZ
080508         FROM    DB2PRD.TPRMSS_FLEXZ_AGPTO
080508         WHERE   CAGPTO_CTA         = :GFCTB0J7.CAGPTO-CTA
080508         AND     CSERVC_TARIF       = :GFCTB0J7.CSERVC-TARIF
080508         AND     CFUNC_ENCRR_VGCIA  = :GFCTB0J7.CFUNC-ENCRR-VGCIA
080508     END-EXEC.
080508
080508     IF (SQLCODE                 NOT EQUAL ZEROS AND +100 AND
080508                                 -811) OR
080508        (SQLWARN0                EQUAL 'W'      )
080508         MOVE  '0010'            TO GFCTG2-COD-MSG
080508         PERFORM 1310-OBTER-DESC-MSG
080508         STRING GFCTG3-DESC-MSG ' - GFCTB0J7'
080508             DELIMITED BY '  '   INTO GFCTNC-DESC-MSG-ERRO
080508         MOVE 'GFCT0408'         TO GFCT0M-PROGRAMA
080508         MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
080508         MOVE 'TPRMSS_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
080508         MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
080508         MOVE  SQLCODE           TO GFCT0M-SQLCODE
080508                                    GFCTNC-COD-SQL-ERRO
080508         MOVE '0160'             TO GFCT0M-LOCAL
080508         MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
080508         MOVE  9                 TO GFCTNC-ERRO
080508         MOVE  10                TO GFCTNC-COD-MSG-ERRO
080508         PERFORM 3000-FINALIZAR
080508     END-IF.
080508
080508     IF  SQLCODE                 EQUAL +100
080508         MOVE 1                  TO GFCTNC-ERRO
080508         MOVE ZEROS              TO GFCTNC-COD-SQL-ERRO
080508         MOVE 1216               TO GFCTNC-COD-MSG-ERRO
080508         MOVE '1216'             TO GFCTG2-COD-MSG
080508         PERFORM 1310-OBTER-DESC-MSG
080508         MOVE GFCTG3-DESC-MSG    TO GFCTNC-DESC-MSG-ERRO
080508         PERFORM 3000-FINALIZAR
080508     END-IF.
080508
080508
080508*----------------------------------------------------------------*
080508 2220-99-FIM.                    EXIT.
080508*----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA CONSISTIR CAMPOS                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2300-CONSISTIR-CAMPOS           SECTION.
      *----------------------------------------------------------------*

           PERFORM 2310-CONSISTIR-CAMPO-DIGITADO.

           PERFORM 2320-ACESSAR-PARM-GESTAO.

           PERFORM 2330-CONSISTIR-PARM-GESTAO.

           PERFORM 2340-CONSISTIR-CAMPO-TARIFA.

           IF  GFCTNB-IND-VALOR        EQUAL  2
               PERFORM 2350-ACESSAR-GFCT5521

               IF  WRK-5521-COD-MSG-GFCT  NOT EQUAL 0443
                   PERFORM 2360-CONSISTIR-CAMPO-GFCT5521
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA CONSISTIR CAMPOS DIGITADOS                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2310-CONSISTIR-CAMPO-DIGITADO   SECTION.
      *----------------------------------------------------------------*

           IF  GFCTNB-FRANQUIA             EQUAL ZEROS
               IF  GFCTNB-PERIODO          NOT EQUAL ZEROS
                   MOVE 1                  TO GFCTNC-ERRO
                   MOVE ZEROS              TO GFCTNC-COD-SQL-ERRO
                   MOVE 0612               TO GFCTNC-COD-MSG-ERRO
                   MOVE '0612'             TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESC-MSG
                   MOVE  GFCTG3-DESC-MSG   TO GFCTNC-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               END-IF
           ELSE
               IF GFCTNB-PERIODO           NOT EQUAL 0 AND 1 AND 2 AND
                                           3 AND 4
                   MOVE 1                  TO GFCTNC-ERRO
                   MOVE ZEROS              TO GFCTNC-COD-SQL-ERRO
                   MOVE 0618               TO GFCTNC-COD-MSG-ERRO
                   MOVE '0618'             TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESC-MSG
                   MOVE  GFCTG3-DESC-MSG   TO GFCTNC-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               END-IF
           END-IF.

           IF  GFCTNB-IND-VALOR            EQUAL 1
               IF  GFCTNB-VR-NEGOCIADO     NOT EQUAL ZEROS OR
                   GFCTNB-PERC-DESCTO      NOT EQUAL ZEROS
                   MOVE 1                  TO GFCTNC-ERRO
                   MOVE ZEROS              TO GFCTNC-COD-SQL-ERRO
                   MOVE 0619               TO GFCTNC-COD-MSG-ERRO
                   MOVE '0619'             TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESC-MSG
                   MOVE  GFCTG3-DESC-MSG   TO GFCTNC-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               END-IF
           END-IF.

           IF  GFCTNB-IND-VALOR            EQUAL 2
               IF  GFCTNB-VR-NEGOCIADO     EQUAL ZEROS
                   MOVE 1                  TO GFCTNC-ERRO
                   MOVE ZEROS              TO GFCTNC-COD-SQL-ERRO
                   MOVE 0642               TO GFCTNC-COD-MSG-ERRO
                   MOVE '0642'             TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESC-MSG
                   MOVE  GFCTG3-DESC-MSG   TO GFCTNC-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               END-IF
               IF  GFCTNB-PERC-DESCTO      NOT EQUAL ZEROS
                   MOVE 1                  TO GFCTNC-ERRO
                   MOVE ZEROS              TO GFCTNC-COD-SQL-ERRO
                   MOVE 0614               TO GFCTNC-COD-MSG-ERRO
                   MOVE '0614'             TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESC-MSG
                   MOVE  GFCTG3-DESC-MSG   TO GFCTNC-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               END-IF
           END-IF.

           IF  GFCTNB-IND-VALOR            EQUAL 3
               IF  GFCTNB-VR-NEGOCIADO     NOT EQUAL ZEROS
                   MOVE 1                  TO GFCTNC-ERRO
                   MOVE ZEROS              TO GFCTNC-COD-SQL-ERRO
                   MOVE 0642               TO GFCTNC-COD-MSG-ERRO
                   MOVE '0642'             TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESC-MSG
                   MOVE  GFCTG3-DESC-MSG   TO GFCTNC-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               END-IF
               IF  GFCTNB-PERC-DESCTO      GREATER 100,0000 OR
                   GFCTNB-PERC-DESCTO      LESS 0,0001
                   MOVE 1                  TO GFCTNC-ERRO
                   MOVE ZEROS              TO GFCTNC-COD-SQL-ERRO
                   MOVE 0614               TO GFCTNC-COD-MSG-ERRO
                   MOVE '0614'             TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESC-MSG
                   MOVE  GFCTG3-DESC-MSG   TO GFCTNC-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               END-IF
           END-IF.

           IF  GFCTNB-VR-NEGOCIADO         NOT EQUAL ZEROS AND
               GFCTNB-PERC-DESCTO          NOT EQUAL ZEROS
               MOVE 1                      TO GFCTNC-ERRO
               MOVE ZEROS                  TO GFCTNC-COD-SQL-ERRO
               MOVE 0619                   TO GFCTNC-COD-MSG-ERRO
               MOVE '0619'                 TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG       TO GFCTNC-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTNB-AGENDAMENTO          EQUAL 'S'
               IF  GFCTNB-DIAS-RETENCAO    NOT EQUAL ZEROS
                   MOVE 1                  TO GFCTNC-ERRO
                   MOVE ZEROS              TO GFCTNC-COD-SQL-ERRO
                   MOVE 0615               TO GFCTNC-COD-MSG-ERRO
                   MOVE '0615'             TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESC-MSG
                   MOVE  GFCTG3-DESC-MSG   TO GFCTNC-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               END-IF
           END-IF.

           IF  GFCTNB-AGENDAMENTO          EQUAL 'S'
               IF  GFCTNB-DIA-AGEND        LESS 4 OR
                   GFCTNB-DIA-AGEND        GREATER 31
                   MOVE 1                  TO GFCTNC-ERRO
                   MOVE ZEROS              TO GFCTNC-COD-SQL-ERRO
                   MOVE 0616               TO GFCTNC-COD-MSG-ERRO
                   MOVE '0616'             TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESC-MSG
                   MOVE  GFCTG3-DESC-MSG   TO GFCTNC-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               END-IF
           ELSE
               IF GFCTNB-DIA-AGEND         NOT EQUAL ZEROS
                   MOVE 1                  TO GFCTNC-ERRO
                   MOVE ZEROS              TO GFCTNC-COD-SQL-ERRO
                   MOVE 0616               TO GFCTNC-COD-MSG-ERRO
                   MOVE '0616'             TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESC-MSG
                   MOVE  GFCTG3-DESC-MSG   TO GFCTNC-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               END-IF
           END-IF.

           IF  GFCTNB-AGENDAMENTO          EQUAL 'S'
               IF  GFCTNB-FRANQUIA         EQUAL ZEROS AND
                   GFCTNB-VR-NEGOCIADO     EQUAL ZEROS AND
                   GFCTNB-PERC-DESCTO      EQUAL ZEROS AND
080508             GFCTNB-PERIODO-COBR     EQUAL ZEROS
                   MOVE 1                  TO GFCTNC-ERRO
                   MOVE ZEROS              TO GFCTNC-COD-SQL-ERRO
                   MOVE 0623               TO GFCTNC-COD-MSG-ERRO
                   MOVE '0623'             TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESC-MSG
                   MOVE  GFCTG3-DESC-MSG   TO GFCTNC-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               END-IF
           END-IF.

           IF  GFCTNB-FRANQUIA             EQUAL ZEROS AND
               GFCTNB-VR-NEGOCIADO         EQUAL ZEROS AND
               GFCTNB-PERC-DESCTO          EQUAL ZEROS AND
               GFCTNB-DIAS-RETENCAO        EQUAL ZEROS AND
080508         GFCTNB-PERIODO-COBR         EQUAL ZEROS
               MOVE 1                      TO GFCTNC-ERRO
               MOVE ZEROS                  TO GFCTNC-COD-SQL-ERRO
               MOVE 0806                   TO GFCTNC-COD-MSG-ERRO
               MOVE '0806'                 TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG       TO GFCTNC-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

080508     IF (GFCTNB-PERIODO-COBR     NOT = 0 AND 1 AND 2 AND 3 AND 4)
080508             MOVE 1              TO GFCTNC-ERRO
080508             MOVE ZEROS          TO GFCTNC-COD-SQL-ERRO
080508             MOVE 1254           TO GFCTNC-COD-MSG-ERRO
080508             MOVE '1254'         TO GFCTG2-COD-MSG
080508             PERFORM 1310-OBTER-DESC-MSG
080508             MOVE GFCTG3-DESC-MSG    TO GFCTNC-DESC-MSG-ERRO
080508             PERFORM 3000-FINALIZAR
080508     END-IF.

080508     IF (GFCTNB-INIC-PER-COBR    NOT = 0 AND 1 AND 2 AND 3 AND
080508                                       4 AND 5 AND 6 AND 7)
080508             MOVE 1              TO GFCTNC-ERRO
080508             MOVE ZEROS          TO GFCTNC-COD-SQL-ERRO
080508             MOVE 1255           TO GFCTNC-COD-MSG-ERRO
080508             PERFORM 1310-OBTER-DESC-MSG
080508             MOVE GFCTG3-DESC-MSG    TO GFCTNC-DESC-MSG-ERRO
080508             PERFORM 3000-FINALIZAR
080508     END-IF.

080508     IF  GFCTNB-PERIODO-COBR         = 2
080508         IF (GFCTNB-INIC-PER-COBR      NOT = 1 AND 2 AND 3 AND
080508                                             4 AND 5 AND 6 AND 7)
080508             MOVE 1              TO GFCTNC-ERRO
080508             MOVE ZEROS          TO GFCTNC-COD-SQL-ERRO
080508             MOVE 1256           TO GFCTNC-COD-MSG-ERRO
080508             MOVE '1256'         TO GFCTG2-COD-MSG
080508             PERFORM 1310-OBTER-DESC-MSG
080508             MOVE GFCTG3-DESC-MSG    TO GFCTNC-DESC-MSG-ERRO
080508             PERFORM 3000-FINALIZAR
080508         END-IF
080508     END-IF.

      *----------------------------------------------------------------*
       2310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA ACESSAR PARAMETROS DA GESTAO                        *
      ******************************************************************
      *----------------------------------------------------------------*
       2320-ACESSAR-PARM-GESTAO        SECTION.
      *----------------------------------------------------------------*

           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO DFIM-VGCIA-GSTOR OF GFCTB0A0.

           EXEC SQL
             SELECT
                   QMAX_FLEXZ,
                   QMAX_RTCAO,
                   QMAX_FRANQ_TARIF
             INTO  :GFCTB0A0.QMAX-FLEXZ,
                   :GFCTB0A0.QMAX-RTCAO,
                   :GFCTB0A0.QMAX-FRANQ-TARIF
             FROM  DB2PRD.PARM_CTRL_GSTAO
             WHERE DFIM_VGCIA_GSTOR    >= :GFCTB0A0.DFIM-VGCIA-GSTOR
              AND  DINIC_VGCIA_GSTOR   IN
              (SELECT MAX(DINIC_VGCIA_GSTOR)
               FROM  DB2PRD.PARM_CTRL_GSTAO
               WHERE DFIM_VGCIA_GSTOR  >= :GFCTB0A0.DFIM-VGCIA-GSTOR)
           END-EXEC.

           IF (SQLCODE                     NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                    EQUAL 'W')
               INITIALIZE                  GFCT0M-ERRO-SQL
               MOVE '0010'                 TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0A0'          TO WRK-NOME-TAB
               MOVE 'PARM_CTRL_GSTAO '     TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '             TO GFCT0M-COMANDO-SQL
               MOVE '0170'                 TO GFCT0M-LOCAL
               MOVE  0010                  TO GFCTNC-COD-MSG-ERRO
               PERFORM 1410-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                     EQUAL +100
               MOVE 1                      TO GFCTNC-ERRO
               MOVE ZEROS                  TO GFCTNC-COD-SQL-ERRO
               MOVE 0752                   TO GFCTNC-COD-MSG-ERRO
               MOVE '0752'                 TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG       TO GFCTNC-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2320-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA CONSISTIR PARAMETROS DA GESTAO                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2330-CONSISTIR-PARM-GESTAO      SECTION.
      *----------------------------------------------------------------*

           PERFORM 2331-ACESSAR-POOL1280.

           IF  WRK-1280-NRO-DIAS       GREATER QMAX-FLEXZ
                                       OF GFCTB0A0
               MOVE 1                  TO GFCTNC-ERRO
               MOVE ZEROS              TO GFCTNC-COD-SQL-ERRO
               MOVE 0621               TO GFCTNC-COD-MSG-ERRO
               MOVE '0621'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNC-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTNB-DIAS-RETENCAO    GREATER QMAX-RTCAO
                                       OF GFCTB0A0
               MOVE 1                  TO GFCTNC-ERRO
               MOVE ZEROS              TO GFCTNC-COD-SQL-ERRO
               MOVE 0625               TO GFCTNC-COD-MSG-ERRO
               MOVE '0625'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNC-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTNB-FRANQUIA         GREATER QMAX-FRANQ-TARIF
                                       OF GFCTB0A0
               MOVE 1                  TO GFCTNC-ERRO
               MOVE ZEROS              TO GFCTNC-COD-SQL-ERRO
               MOVE 0622               TO GFCTNC-COD-MSG-ERRO
               MOVE '0622'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNC-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2330-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA ACESSAR POOL1280                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2331-ACESSAR-POOL1280           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INIC-FLEX          TO WRK-1280-DATA-INFERIOR.
           MOVE WRK-FIM-FLEX           TO WRK-1280-DATA-SUPERIOR.
           MOVE ZEROS                  TO WRK-1280-NRO-DIAS.

           CALL 'POOL1280'             USING WRK-1280-DATA-INFERIOR
                                             WRK-1280-DATA-SUPERIOR
                                             WRK-1280-NRO-DIAS
                                             WRK-1280-MENSAGEM.

           IF  RETURN-CODE             EQUAL 4 OR 8
               MOVE 1                  TO GFCTNC-ERRO
               MOVE ZEROS              TO GFCTNC-COD-SQL-ERRO
               MOVE 0628               TO GFCTNC-COD-MSG-ERRO
               MOVE '0628'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNC-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2331-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA CONSISTIR CAMPOS TARIFA                             *
      ******************************************************************
      *----------------------------------------------------------------*
       2340-CONSISTIR-CAMPO-TARIFA     SECTION.
      *----------------------------------------------------------------*

BI0115     IF  (WRK-5018-CTPO-SERVC-TARIF   EQUAL 2 OR 4) AND
               GFCTNB-FRANQUIA             NOT EQUAL ZEROS
               MOVE 1                      TO GFCTNC-ERRO
               MOVE ZEROS                  TO GFCTNC-COD-SQL-ERRO
               MOVE 0613                   TO GFCTNC-COD-MSG-ERRO
               MOVE '0613'                 TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG        TO GFCTNC-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           PERFORM VARYING IND-1 FROM 1 BY 1     UNTIL
               IND-1                             GREATER 50   OR
               WRK-5018-DINIC-VGCIA-TARIF(IND-1) EQUAL SPACES
                 IF WRK-5018-PSERVC-PTADO(IND-1) GREATER ZEROS AND
                    GFCTNB-VR-NEGOCIADO          NOT EQUAL ZEROS
                    MOVE 1                       TO GFCTNC-ERRO
                    MOVE ZEROS                   TO GFCTNC-COD-SQL-ERRO
                    MOVE 0624                    TO GFCTNC-COD-MSG-ERRO
                    MOVE '0624'                  TO GFCTG2-COD-MSG
                    PERFORM 1310-OBTER-DESC-MSG
                    MOVE GFCTG3-DESC-MSG         TO GFCTNC-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
                 END-IF
                 IF WRK-5018-CMOEDA-BACEN(IND-1) GREATER ZEROS AND
                    GFCTNB-VR-NEGOCIADO          NOT EQUAL ZEROS
                    MOVE 1                       TO GFCTNC-ERRO
                    MOVE ZEROS                   TO GFCTNC-COD-SQL-ERRO
                    MOVE 0629                    TO GFCTNC-COD-MSG-ERRO
                    MOVE '0629'                  TO GFCTG2-COD-MSG
                    PERFORM 1310-OBTER-DESC-MSG
                    MOVE GFCTG3-DESC-MSG         TO GFCTNC-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
                 END-IF
           END-PERFORM.

      *----------------------------------------------------------------*
       2340-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA ACESSAR GFCT5521                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2350-ACESSAR-GFCT5521           SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                     TO WRK-5521-AREA-ENTRADA
                                              WRK-5521-AREA-SAIDA
                                              GFCT0M-AREA-ERROS.

           INITIALIZE WRK-5521-AREA-ENTRADA
                      WRK-5521-AREA-SAIDA
                      GFCT0M-AREA-ERROS.


           MOVE 'O'                        TO WRK-5521-AMBIENTE.
           MOVE 'GFCT0418'                 TO WRK-5521-CHAMADOR.
           MOVE GFCTNB-COD-TARIFA          TO WRK-5521-COD-TARIFA.
           MOVE 1                          TO WRK-5521-QTD-EVN-AGRUP.
PROC       MOVE DPROCM-ATUAL OF GFCTB0A1   TO WRK-5521-DATA-EVN.
PROC       MOVE GFCTNB-INIC-VIG-FLEX       TO WRK-5521-DATA-DEBITO
           MOVE ZEROS                      TO WRK-5521-VALOR-EVN
                                              WRK-5521-TIPO-MOEDA-EVN.
           MOVE SPACES                     TO WRK-5521-DATA-CONV-EVN.
           MOVE 10                         TO WRK-5521-TIPO-MOVTO.
           MOVE 'GFCT5521'                 TO WRK-MODULO.

           CALL WRK-MODULO USING
                                                 WRK-5521-AREA-ENTRADA
                                                 WRK-5521-AREA-SAIDA
                                                 GFCT0M-AREA-ERROS.

           IF  WRK-5521-COD-RETORNO        NOT EQUAL ZEROS AND
               WRK-5521-COD-MSG-GFCT       EQUAL ZEROS
               MOVE 9                      TO GFCTNC-ERRO
               MOVE WRK-MODULO             TO WRK-NOME-MOD
               MOVE WRK-MSG01              TO GFCT0M-TEXTO
                                              GFCTNC-DESC-MSG-ERRO
               MOVE 'APL'                  TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0418'             TO GFCT0M-PROGRAMA
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-5521-COD-MSG-GFCT       NOT EQUAL ZEROS AND
               WRK-5521-COD-MSG-GFCT       NOT EQUAL 0476  AND
               WRK-5521-COD-MSG-GFCT       NOT EQUAL 0443
               MOVE 1                      TO GFCTNC-ERRO
               MOVE WRK-5521-COD-SQL-ERRO  TO GFCTNC-COD-SQL-ERRO
               MOVE WRK-5521-COD-MSG-GFCT  TO GFCTNC-COD-MSG-ERRO
               MOVE WRK-5521-DESC-MSG      TO GFCTNC-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2350-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA CONSISTIR CAMPO GFCT5521                            *
      ******************************************************************
      *----------------------------------------------------------------*
       2360-CONSISTIR-CAMPO-GFCT5521   SECTION.
      *----------------------------------------------------------------*

           IF  WRK-5521-VLR-TOTAL-TARIFA NOT GREATER GFCTNB-VR-NEGOCIADO
               MOVE 1                  TO GFCTNC-ERRO
               MOVE ZEROS              TO GFCTNC-COD-SQL-ERRO
               MOVE 0805               TO GFCTNC-COD-MSG-ERRO
               MOVE '0805'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNC-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2360-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA INCLUIR SOLICITACAO                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2400-INCLUIR-SOLICITACAO        SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               SET :WRK-TIMESTAMP-AUX = CURRENT TIMESTAMP
           END-EXEC.

           MOVE WRK-TIMESTAMP-AUX     TO HIDTFD-FLEXZ-AGPTO OF GFCTB0B2.
           MOVE GFCTNB-AGRUPAMENTO    TO CAGPTO-CTA         OF GFCTB0B2.
           MOVE GFCTNB-COD-TARIFA     TO CSERVC-TARIF       OF GFCTB0B2.
           MOVE GFCTNB-INIC-VIG-FLEX  TO DINIC-FLEXZ-AGPTO  OF GFCTB0B2.
           MOVE GFCTNB-FIM-VIG-FLEX   TO DFIM-FLEXZ-AGPTO   OF GFCTB0B2.
           MOVE GFCTNB-FRANQUIA       TO QFRANQ-FLEXZ-AGPTO OF GFCTB0B2.
           MOVE GFCTNB-PERIODO        TO CPER-FRANQ-AGPTO   OF GFCTB0B2.
           MOVE GFCTNB-VR-NEGOCIADO   TO VNEGOC-FLEXZ-AGPTO OF GFCTB0B2.
           MOVE GFCTNB-PERC-DESCTO    TO PDESC-FLEXZ-AGPTO  OF GFCTB0B2.
           MOVE GFCTNB-DIA-AGEND      TO DDIA-AGNDA         OF GFCTB0B2.
           MOVE GFCTNB-DIAS-RETENCAO  TO QDIA-FLEXZ-AGPTO   OF GFCTB0B2.
           MOVE 237                   TO CEMPR-ORIGE-FLEXZ  OF GFCTB0B2.
           MOVE ZEROS                 TO CEMPR-PAREC-FLEXZ  OF GFCTB0B2
                                         CDEPDC-PAREC-FLEXZ OF GFCTB0B2
                                         CFUNC-PAREC-FLEXZ  OF GFCTB0B2
                                         CINIC-PER-FRANQ    OF GFCTB0B2
                                         CFUNC-EXCL-SIST    OF GFCTB0B2.
           MOVE GFCTNB-CTIPO-OPER-MOTVO TO CTPO-OPER-MOTVO  OF GFCTB0B2.
           MOVE GFCTNB-CMOTVO-JUSTF   TO CMOTVO-JUSTF       OF GFCTB0B2.
           MOVE GFCTNB-FUNC-BDSCO     TO CFUNC-SOLTC-FLEXZ  OF GFCTB0B2.
           MOVE WRK-5505-COD-JUNCAO   TO CDEPDC-ORIGE-FLEXZ OF GFCTB0B2.

080508     MOVE GFCTNB-PERIODO-COBR    TO CPER-COBR-TARIF   OF GFCTB0B2.
080508     MOVE GFCTNB-INIC-PER-COBR   TO CINIC-PER-COBR    OF GFCTB0B2.

           MOVE '0001-01-01-01.01.01.000001'
                                       TO HPAREC-FLEXZ      OF GFCTB0B2
           MOVE SPACES                 TO RJUSTF-PAREC-FLEXZ-TEXT
                                                            OF GFCTB0B2.

           MOVE ZEROS                  TO RJUSTF-PAREC-FLEXZ-LEN
                                                            OF GFCTB0B2.
           MOVE GFCTNB-TODAS-AG        TO CINDCD-AGPTO-TOT  OF GFCTB0B2.
           MOVE 1                      TO CSIT-SOLTC-FLEXZ  OF GFCTB0B2.
           MOVE GFCTNB-RJUSTF-MANUT-SERVC
                                       TO RJUSTF-SOLTC-FLEXZ-TEXT
                                                            OF GFCTB0B2.
           MOVE LENGTH                 OF GFCTNB-RJUSTF-MANUT-SERVC
                                       TO RJUSTF-SOLTC-FLEXZ-LEN
                                                            OF GFCTB0B2.
230610     MOVE ZEROS                  TO CCJTO-SERVC OF GFCTB0B2.

           EXEC SQL
             INSERT INTO DB2PRD.PDIDO_FLEXZ_AGPTO
                  (CAGPTO_CTA,
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
                   CINDCD_AGPTO_TOT,
                   CINIC_PER_FRANQ,
                   CFUNC_EXCL_SIST,
080508             CPER_COBR_TARIF,
080508             CINIC_PER_COBR,
230610             CCJTO_SERVC)
               VALUES
                  (:GFCTB0B2.CAGPTO-CTA,
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
                   CURRENT TIMESTAMP,
                   :GFCTB0B2.RJUSTF-SOLTC-FLEXZ,
                   :GFCTB0B2.HPAREC-FLEXZ,
                   :GFCTB0B2.RJUSTF-PAREC-FLEXZ,
                   :GFCTB0B2.CSIT-SOLTC-FLEXZ,
                   :GFCTB0B2.CINDCD-AGPTO-TOT,
                   :GFCTB0B2.CINIC-PER-FRANQ,
                   :GFCTB0B2.CFUNC-EXCL-SIST,
080508             :GFCTB0B2.CPER-COBR-TARIF,
080508             :GFCTB0B2.CINIC-PER-COBR,
230610             :GFCTB0B2.CCJTO-SERVC)
           END-EXEC.

           IF (SQLCODE                     NOT EQUAL ZEROS) OR
              (SQLWARN0                    EQUAL 'W')
               INITIALIZE                  GFCT0M-ERRO-SQL
               MOVE '0010'                 TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0B2'          TO WRK-NOME-TAB
               MOVE 'PDIDO_FLEXZ_AGPTO'    TO GFCT0M-NOME-TAB
               MOVE 'INSERT  '             TO GFCT0M-COMANDO-SQL
               MOVE '0180'                 TO GFCT0M-LOCAL
               MOVE  0010                  TO GFCTNC-COD-MSG-ERRO
               PERFORM 1410-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           EVALUATE GFCTNB-AGRUPAMENTO
               WHEN 15
                   PERFORM 2410-INCLUIR-PEDIDO-PAB

               WHEN 14
                   PERFORM 2420-INCLUIR-PEDIDO-AGENCIA

               WHEN 17
                   PERFORM 2430-INCLUIR-PEDIDO-POSTAL

               WHEN 4
                   PERFORM 2440-INCLUIR-PEDIDO-CLIENTE

               WHEN 9
                   PERFORM 2450-INCLUIR-PEDIDO-SEGMENTO

               WHEN 18
                   PERFORM 2460-INCLUIR-PEDIDO-MUNICIPIO

               WHEN 19
                   PERFORM 2470-INCLUIR-PEDIDO-UF

0310           WHEN 20
0310               PERFORM 2430-INCLUIR-PEDIDO-POSTAL

0310           WHEN 26
0310               PERFORM 2480-INCLUIR-PEDIDO-RAZAO

           END-EVALUATE.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA INCLUIR PEDIDO PAB                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2410-INCLUIR-PEDIDO-PAB         SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTNB-AGRUPAMENTO    TO CAGPTO-CTA         OF GFCTB0B8.
           MOVE GFCTNB-COD-TARIFA     TO CSERVC-TARIF       OF GFCTB0B8.
           MOVE WRK-TIMESTAMP-AUX     TO HIDTFD-FLEXZ-AGPTO OF GFCTB0B8.
           MOVE GFCTNB-AGENCIA        TO CDEPDC             OF GFCTB0B8.
           MOVE GFCTNB-POSTO          TO CPOSTO-SERVC       OF GFCTB0B8.
           MOVE 237                   TO CEMPR-INC          OF GFCTB0B8.

           EXEC SQL
             INSERT INTO DB2PRD.PDIDO_FLEXZ_PAB
                  (CAGPTO_CTA,
                   CSERVC_TARIF,
                   HIDTFD_FLEXZ_AGPTO,
                   CEMPR_INC,
                   CDEPDC,
                   CPOSTO_SERVC)
               VALUES
                  (:GFCTB0B8.CAGPTO-CTA,
                   :GFCTB0B8.CSERVC-TARIF,
                   :GFCTB0B8.HIDTFD-FLEXZ-AGPTO,
                   :GFCTB0B8.CEMPR-INC,
                   :GFCTB0B8.CDEPDC,
                   :GFCTB0B8.CPOSTO-SERVC)
           END-EXEC.

           IF (SQLCODE                     NOT EQUAL ZEROS) OR
              (SQLWARN0                    EQUAL 'W'      )
               INITIALIZE                  GFCT0M-ERRO-SQL
               MOVE '0010'                 TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0B8'          TO WRK-NOME-TAB
               MOVE 'PDIDO_FLEXZ_PAB '     TO GFCT0M-NOME-TAB
               MOVE 'INSERT  '             TO GFCT0M-COMANDO-SQL
               MOVE '0190'                 TO GFCT0M-LOCAL
               MOVE  0010                  TO GFCTNC-COD-MSG-ERRO
               PERFORM 1410-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2410-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA INCLUIR PEDIDO AGENCIA                              *
      ******************************************************************
      *----------------------------------------------------------------*
       2420-INCLUIR-PEDIDO-AGENCIA     SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTNB-AGRUPAMENTO    TO CAGPTO-CTA         OF GFCTB0B1.
           MOVE GFCTNB-COD-TARIFA     TO CSERVC-TARIF       OF GFCTB0B1.
           MOVE WRK-TIMESTAMP-AUX     TO HIDTFD-FLEXZ-AGPTO OF GFCTB0B1.

           IF  GFCTNB-TODAS-AG        EQUAL 'S'
               MOVE ZEROS             TO CDEPDC             OF GFCTB0B1
           ELSE
               MOVE GFCTNB-AGENCIA    TO CDEPDC             OF GFCTB0B1
           END-IF.

           MOVE 237                   TO CEMPR-INC          OF GFCTB0B1.

           EXEC SQL
             INSERT INTO DB2PRD.PDIDO_FLEXZ_AG
                  (CAGPTO_CTA,
                   CSERVC_TARIF,
                   HIDTFD_FLEXZ_AGPTO,
                   CEMPR_INC,
                   CDEPDC)
               VALUES
                  (:GFCTB0B1.CAGPTO-CTA,
                   :GFCTB0B1.CSERVC-TARIF,
                   :GFCTB0B1.HIDTFD-FLEXZ-AGPTO,
                   :GFCTB0B1.CEMPR-INC,
                   :GFCTB0B1.CDEPDC)
           END-EXEC.

           IF (SQLCODE                     NOT EQUAL ZEROS) OR
              (SQLWARN0                    EQUAL 'W'      )
               INITIALIZE                  GFCT0M-ERRO-SQL
               MOVE '0010'                 TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0B1'          TO WRK-NOME-TAB
               MOVE 'PDIDO_FLEXZ_AG  '     TO GFCT0M-NOME-TAB
               MOVE 'INSERT  '             TO GFCT0M-COMANDO-SQL
               MOVE '0210'                 TO GFCT0M-LOCAL
               MOVE  0010                  TO GFCTNC-COD-MSG-ERRO
               PERFORM 1410-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2420-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA INCLUIR PEDIDO POSTAL                               *
      ******************************************************************
      *----------------------------------------------------------------*
       2430-INCLUIR-PEDIDO-POSTAL      SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTNB-AGRUPAMENTO    TO CAGPTO-CTA         OF GFCTB0B9.
           MOVE GFCTNB-COD-TARIFA     TO CSERVC-TARIF       OF GFCTB0B9.
           MOVE WRK-TIMESTAMP-AUX     TO HIDTFD-FLEXZ-AGPTO OF GFCTB0B9.
           MOVE GFCTNB-AGENCIA        TO CDEPDC             OF GFCTB0B9.
           MOVE GFCTNB-POSTO          TO CPOSTO-SERVC       OF GFCTB0B9.
           MOVE 237                   TO CEMPR-INC          OF GFCTB0B9.

           EXEC SQL
             INSERT INTO DB2PRD.PDIDO_FLEXZ_PSTAL
                  (CAGPTO_CTA,
                   CSERVC_TARIF,
                   HIDTFD_FLEXZ_AGPTO,
                   CEMPR_INC,
                   CDEPDC,
                   CPOSTO_SERVC)
               VALUES
                  (:GFCTB0B9.CAGPTO-CTA,
                   :GFCTB0B9.CSERVC-TARIF,
                   :GFCTB0B9.HIDTFD-FLEXZ-AGPTO,
                   :GFCTB0B9.CEMPR-INC,
                   :GFCTB0B9.CDEPDC,
                   :GFCTB0B9.CPOSTO-SERVC)
           END-EXEC.

           IF (SQLCODE                     NOT EQUAL ZEROS) OR
              (SQLWARN0                    EQUAL 'W'      )
               INITIALIZE                  GFCT0M-ERRO-SQL
               MOVE '0010'                 TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0B9'          TO WRK-NOME-TAB
               MOVE 'PDIDO_FLEXZ_PSTAL'    TO GFCT0M-NOME-TAB
               MOVE 'INSERT  '             TO GFCT0M-COMANDO-SQL
               MOVE '0220'                 TO GFCT0M-LOCAL
               MOVE  0010                  TO GFCTNC-COD-MSG-ERRO
               PERFORM 1410-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2430-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA INCLUIR PEDIDO CLIENTE                              *
      ******************************************************************
      *----------------------------------------------------------------*
       2440-INCLUIR-PEDIDO-CLIENTE     SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTNB-AGRUPAMENTO    TO CAGPTO-CTA         OF GFCTB0B3.
           MOVE GFCTNB-COD-TARIFA     TO CSERVC-TARIF       OF GFCTB0B3.
           MOVE WRK-TIMESTAMP-AUX     TO HIDTFD-FLEXZ-AGPTO OF GFCTB0B3.
ST2506*    MOVE GFCTNB-CGC-CPF        TO CCGC-CPF           OF GFCTB0B3.
ST2506*    MOVE GFCTNB-FILIAL         TO CFLIAL-CGC         OF GFCTB0B3.
ST2506*    MOVE GFCTNB-CONTROLE       TO WRK-CONTROLE.
ST2506*    MOVE WRK-CONTROLE-R        TO CCTRL-CPF-CGC      OF GFCTB0B3.
ST2506     MOVE GFCTNB-CGC-CPF        TO CCGC-CPF-ST        OF GFCTB0B3.
ST2506     MOVE GFCTNB-FILIAL         TO CFLIAL-CGC-ST      OF GFCTB0B3.
ST2506     MOVE GFCTNB-CONTROLE       TO WRK-CONTROLE.
ST2506     MOVE WRK-CONTROLE-R        TO CCTRL-CPF-CGC-ST   OF GFCTB0B3.

           EXEC SQL
             INSERT INTO DB2PRD.PDIDO_FLEXZ_CLI
                  (CAGPTO_CTA,
                   CSERVC_TARIF,
                   HIDTFD_FLEXZ_AGPTO,
                   CCGC_CPF,
                   CFLIAL_CGC,
                   CCTRL_CPF_CGC,
ST2506             CCGC_CPF_ST,
ST2506             CFLIAL_CGC_ST,
ST2506             CCTRL_CPF_CGC_ST)
               VALUES
                  (:GFCTB0B3.CAGPTO-CTA,
                   :GFCTB0B3.CSERVC-TARIF,
                   :GFCTB0B3.HIDTFD-FLEXZ-AGPTO,
                   :GFCTB0B3.CCGC-CPF,
                   :GFCTB0B3.CFLIAL-CGC,
                   :GFCTB0B3.CCTRL-CPF-CGC,
ST2506             :GFCTB0B3.CCGC-CPF-ST,
ST2506             :GFCTB0B3.CFLIAL-CGC-ST,
ST2506             :GFCTB0B3.CCTRL-CPF-CGC-ST)
           END-EXEC.

           IF (SQLCODE                     NOT EQUAL ZEROS) OR
              (SQLWARN0                    EQUAL 'W'      )
               INITIALIZE                  GFCT0M-ERRO-SQL
               MOVE '0010'                 TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0B3'          TO WRK-NOME-TAB
               MOVE 'PDIDO_FLEXZ_CLI '     TO GFCT0M-NOME-TAB
               MOVE 'INSERT  '             TO GFCT0M-COMANDO-SQL
               MOVE '0230'                 TO GFCT0M-LOCAL
               MOVE  0010                  TO GFCTNC-COD-MSG-ERRO
               PERFORM 1410-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2440-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA INCLUIR PEDIDO SEGMENTO                             *
      ******************************************************************
      *----------------------------------------------------------------*
       2450-INCLUIR-PEDIDO-SEGMENTO    SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTNB-AGRUPAMENTO    TO CAGPTO-CTA         OF GFCTB0C0.
           MOVE GFCTNB-COD-TARIFA     TO CSERVC-TARIF       OF GFCTB0C0.
           MOVE WRK-TIMESTAMP-AUX     TO HIDTFD-FLEXZ-AGPTO OF GFCTB0C0.
           MOVE GFCTNB-SEGMENTO       TO CSGMTO-GSTAO-TARIF OF GFCTB0C0.

           EXEC SQL
             INSERT INTO DB2PRD.PDIDO_FLEXZ_SGMTO
                  (CAGPTO_CTA,
                   CSERVC_TARIF,
                   HIDTFD_FLEXZ_AGPTO,
                   CSGMTO_GSTAO_TARIF)
               VALUES
                  (:GFCTB0C0.CAGPTO-CTA,
                   :GFCTB0C0.CSERVC-TARIF,
                   :GFCTB0C0.HIDTFD-FLEXZ-AGPTO,
                   :GFCTB0C0.CSGMTO-GSTAO-TARIF)
           END-EXEC.

           IF (SQLCODE                     NOT EQUAL ZEROS) OR
              (SQLWARN0                    EQUAL 'W'      )
               INITIALIZE                  GFCT0M-ERRO-SQL
               MOVE '0010'                 TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0C0'          TO WRK-NOME-TAB
               MOVE 'PDIDO_FLEXZ_SGMTO'    TO GFCT0M-NOME-TAB
               MOVE 'INSERT  '             TO GFCT0M-COMANDO-SQL
               MOVE '0240'                 TO GFCT0M-LOCAL
               MOVE  0010                  TO GFCTNC-COD-MSG-ERRO
               PERFORM 1410-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2450-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA INCLUIR PEDIDO MUNICIPIO                            *
      ******************************************************************
      *----------------------------------------------------------------*
       2460-INCLUIR-PEDIDO-MUNICIPIO   SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTNB-AGRUPAMENTO    TO CAGPTO-CTA         OF GFCTB0B7.
           MOVE GFCTNB-COD-TARIFA     TO CSERVC-TARIF       OF GFCTB0B7.
           MOVE WRK-TIMESTAMP-AUX     TO HIDTFD-FLEXZ-AGPTO OF GFCTB0B7.
           MOVE GFCTNB-MUNIC          TO CMUN-IBGE          OF GFCTB0B7.

           EXEC SQL
             INSERT INTO DB2PRD.PDIDO_FLEXZ_MUN
                  (CAGPTO_CTA,
                   CSERVC_TARIF,
                   HIDTFD_FLEXZ_AGPTO,
                   CMUN_IBGE)
               VALUES
                  (:GFCTB0B7.CAGPTO-CTA,
                   :GFCTB0B7.CSERVC-TARIF,
                   :GFCTB0B7.HIDTFD-FLEXZ-AGPTO,
                   :GFCTB0B7.CMUN-IBGE)
           END-EXEC.

           IF (SQLCODE                     NOT EQUAL ZEROS) OR
              (SQLWARN0                    EQUAL 'W'      )
               INITIALIZE                  GFCT0M-ERRO-SQL
               MOVE '0010'                 TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0B7'          TO WRK-NOME-TAB
               MOVE 'PDIDO_FLEXZ_MUN '     TO GFCT0M-NOME-TAB
               MOVE 'INSERT  '             TO GFCT0M-COMANDO-SQL
               MOVE '0250'                 TO GFCT0M-LOCAL
               MOVE  0010                  TO GFCTNC-COD-MSG-ERRO
               PERFORM 1410-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2460-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA INCLUIR PEDIDO UF                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2470-INCLUIR-PEDIDO-UF          SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTNB-AGRUPAMENTO    TO CAGPTO-CTA         OF GFCTB0C1.
           MOVE GFCTNB-COD-TARIFA     TO CSERVC-TARIF       OF GFCTB0C1.
           MOVE WRK-TIMESTAMP-AUX     TO HIDTFD-FLEXZ-AGPTO OF GFCTB0C1.
           MOVE GFCTNB-UF             TO CSGL-UF            OF GFCTB0C1.

           EXEC SQL
             INSERT INTO DB2PRD.PDIDO_FLEXZ_UF
                  (CAGPTO_CTA,
                   CSERVC_TARIF,
                   HIDTFD_FLEXZ_AGPTO,
                   CSGL_UF)
               VALUES
                  (:GFCTB0C1.CAGPTO-CTA,
                   :GFCTB0C1.CSERVC-TARIF,
                   :GFCTB0C1.HIDTFD-FLEXZ-AGPTO,
                   :GFCTB0C1.CSGL-UF)
           END-EXEC.

           IF (SQLCODE                     NOT EQUAL ZEROS) OR
              (SQLWARN0                    EQUAL 'W'      )
               INITIALIZE                  GFCT0M-ERRO-SQL
               MOVE '0010'                 TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0C1'          TO WRK-NOME-TAB
               MOVE 'PDIDO_FLEXZ_UF  '     TO GFCT0M-NOME-TAB
               MOVE 'INSERT  '             TO GFCT0M-COMANDO-SQL
               MOVE '0260'                 TO GFCT0M-LOCAL
               MOVE  0010                  TO GFCTNC-COD-MSG-ERRO
               PERFORM 1410-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2470-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

0310  ******************************************************************
0310  *ROTINA PARA INCLUIR PEDIDO RAZAO                                *
0310  ******************************************************************
0310  *----------------------------------------------------------------*
0310   2480-INCLUIR-PEDIDO-RAZAO       SECTION.
0310  *----------------------------------------------------------------*
0310
0310       MOVE 237                   TO CEMPR              OF GFCTB0M7
0310       MOVE GFCTNB-AGRUPAMENTO    TO CAGPTO-CTA         OF GFCTB0M7
0310       MOVE GFCTNB-COD-TARIFA     TO CSERVC-TARIF       OF GFCTB0M7
0310       MOVE WRK-TIMESTAMP-AUX     TO HIDTFD-FLEXZ-AGPTO OF GFCTB0M7
0310       MOVE GFCTNB-GRUPO-CTBIL    TO CGRP-CTBIL-ATUAL   OF GFCTB0M7
0310       MOVE GFCTNB-SUB-GRUPO-CTBIL
0310                                  TO CSGRP-CTBIL        OF GFCTB0M7
0310       MOVE 237                   TO CEMPR              OF GFCTB0M7
0310
0310       EXEC SQL
0310         INSERT INTO DB2PRD.TPDIDO_FLEXZ_RZ
0310              (CAGPTO_CTA,
0310               CSERVC_TARIF,
0310               HIDTFD_FLEXZ_AGPTO,
0310               CGRP_CTBIL_ATUAL,
0310               CSGRP_CTBIL,
0310               CEMPR)
0310           VALUES
0310              (:GFCTB0M7.CAGPTO-CTA,
0310               :GFCTB0M7.CSERVC-TARIF,
0310               :GFCTB0M7.HIDTFD-FLEXZ-AGPTO,
0310               :GFCTB0M7.CGRP-CTBIL-ATUAL,
0310               :GFCTB0M7.CSGRP-CTBIL,
0310               :GFCTB0M7.CEMPR)
0310       END-EXEC.
0310
0310       IF (SQLCODE                     NOT EQUAL ZEROS) OR
0310          (SQLWARN0                    EQUAL 'W'      )
0310           INITIALIZE                  GFCT0M-ERRO-SQL
0310           MOVE '0010'                 TO GFCTG2-COD-MSG
0310           PERFORM 1310-OBTER-DESC-MSG
0310           MOVE ' - GFCTB0M7'          TO WRK-NOME-TAB
0310           MOVE 'TPDIDO_FLEXZ_RZ'      TO GFCT0M-NOME-TAB
0310           MOVE 'INSERT  '             TO GFCT0M-COMANDO-SQL
0310           MOVE '0230'                 TO GFCT0M-LOCAL
0310           MOVE  0010                  TO GFCTNC-COD-MSG-ERRO
0310           PERFORM 1410-MOVER-ERRO-SQL
0310           PERFORM 3000-FINALIZAR
0310       END-IF.
0310
0310  *----------------------------------------------------------------*
0310   2480-99-FIM.                    EXIT.
0310  *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA FINALIZAR PROGRAMA                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTNB-AGRUPAMENTO         TO GFCTNC-AGRUPAMENTO.
           MOVE GFCTNB-COD-TARIFA          TO GFCTNC-COD-TARIFA.

           MOVE GFCTNB-CGC-CPF             TO GFCTNC-CGC-CPF.
           MOVE GFCTNB-FILIAL              TO GFCTNC-FILIAL.
           MOVE GFCTNB-CONTROLE            TO GFCTNC-CONTROLE.
           MOVE GFCTNB-SEGMENTO            TO GFCTNC-SEGMENTO.
           MOVE GFCTNB-AGENCIA             TO GFCTNC-AGENCIA.
           MOVE GFCTNB-POSTO               TO GFCTNC-POSTO.
           MOVE GFCTNB-MUNIC               TO GFCTNC-MUNIC.
           MOVE GFCTNB-UF                  TO GFCTNC-UF.
           MOVE GFCTNB-TIMESTAMP           TO GFCTNC-TIMESTAMP.

           MOVE GFCTNB-TODAS-AG            TO GFCTNC-TODAS-AG.
           MOVE GFCTNB-INIC-VIG-FLEX       TO GFCTNC-INIC-VIG-FLEX.
           MOVE GFCTNB-FIM-VIG-FLEX        TO GFCTNC-FIM-VIG-FLEX.
           MOVE GFCTNB-AGENDAMENTO         TO GFCTNC-AGENDAMENTO.
           MOVE GFCTNB-DIA-AGEND           TO GFCTNC-DIA-AGEND.
           MOVE GFCTNB-DIAS-RETENCAO       TO GFCTNC-DIAS-RETENCAO.
           MOVE GFCTNB-IND-VALOR           TO GFCTNC-IND-VALOR.
           MOVE GFCTNB-VR-NEGOCIADO        TO GFCTNC-VR-NEGOCIADO.
           MOVE GFCTNB-PERC-DESCTO         TO GFCTNC-PERC-DESCTO.
           MOVE GFCTNB-FRANQUIA            TO GFCTNC-FRANQUIA.
           MOVE GFCTNB-PERIODO             TO GFCTNC-PERIODO.
           MOVE GFCTNB-CTIPO-OPER-MOTVO    TO GFCTNC-CTIPO-OPER-MOTVO.
           MOVE GFCTNB-CMOTVO-JUSTF        TO GFCTNC-CMOTVO-JUSTF.
           MOVE GFCTNB-RJUSTF-MANUT-SERVC  TO GFCTNC-RJUSTF-MANUT-SERVC.
080508     MOVE GFCTNB-PERIODO-COBR        TO GFCTNC-PERIODO-COBR
080508     MOVE GFCTNB-INIC-PER-COBR       TO GFCTNC-INIC-PER-COBR

           GOBACK.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
