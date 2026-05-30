      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT0408.
       AUTHOR.     JEFERSON PAULO DALPONTE.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT0408                                    *
      *    PROGRAMADOR.:   JEFERSON PAULO DALPONTE - CPM PATO BRANCO   *
      *    ANALISTA CPM:   ELIAS AUGUSTO BOSCATO   - CPM PATO BRANCO   *
      *    ANALISTA....:   VALERIA                 - PROCWORK /GP.50   *
      *    DATA........:   02/05/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   FLEXIBILIZACAO - INCLUSAO DE FLEXIBILIZACAO *
      *      PARA AGRUPAMENTO.                                         *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                        INCLUDE/BOOK     *
0410  *                   DB2PRD.TASSOC_CJTO_NATUZ      GFCTB0H2       *
0410  *                   DB2PRD.PARM_SERVC_TARIF       GFCTB0A2       *
0410  *                   DB2PRD.TPDIDO_FLEXZ_RZ        GFCTB0M7       *
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
110510*                   DB2PRD.TPDIDO_FLEXZ_UF        GFCTB0M7       *
080508*                   DB2PRD.TPRMSS_FLEXZ_AGPTO     GFCTB0J7       *
110508*                   DB2PRD.RAZAO_CONTABIL         PLCCB004       *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#GFCTN9 - AREA DE COMUNICACAO - ENTRADA                    *
      *    I#GFCTNA - AREA DE COMUNICACAO - SAIDA                      *
      *    I#GFCTFZ - AREA DE COMUNICACAO - ENTRADA DO MODULO GFCT5522 *
      *    I#GFCTG1 - AREA DE COMUNICACAO - SAIDA DO MODULO GFCT5522   *
      *    I#GFCTG2 - AREA DE COMUNICACAO - ENTRADA DO MODULO GFCT5523 *
      *    I#GFCTG3 - AREA DE COMUNICACAO - SAIDA DO MODULO GFCT5523   *
      *    I#GFCTIU - AREA DE COMUNICACAO - ENTRADA DO MODULO GFCT5110 *
      *    I#GFCTIV - AREA DE COMUNICACAO - SAIDA   DO MODULO GFCT5110 *
      *    I#RURC88 - AREA DE COMUNICACAO COM RURC9020                 *
      *    I#MESUM8 - AREA DE COMUNICACAO COM MESU9018                 *
      *    I#MESUM6 - AREA DE COMUNICACAO COM MESU9015                 *
      *    I#GFCT0M - AREA DE COMUNICACAO - ERROS                      *
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
      *    POOL1280 - CALCULA NUMERO DE DIAS ENTRE DUAS DATAS.         *
      *    POOL1470 - MODULO PARA CONSISTIR DATA.                      *
      *    POOL0110 - MODULO PARA CONSISTIR CPF/CNPJ.                  *
0410  *    GFCT5296 - VERIFICAR SE BANCO EH POSTAL OU EXPRESSO.        *
      *----------------------------------------------------------------*
      *    ULTIMA ALTERACAO EM 08/02/2007 - VALERIA                    *
      *    VALORIZAR A DATA DE EVENTO NA CHAMADA DO MODULO GFCT5521    *
      *    COM A DATA DE INICIO DE VIGENCIA DA FLEXIBILIZACAO DIGITADA *
      *    E PASSOU A CHAMAR SOMENTE QUANDO INFORMADO CAMPO DE VALOR   *
      *----------------------------------------------------------------*
      *    ULTIMA ALTERACAO EM 08/05/2008 - VALERIA                    *
      *    ALTERACOES REFERENTES AOS AGRUPAMENTOS PERSONALIZADOS       *
      *    - INCLUSAO DO AGRUPAMENTO 07                                *
      *    - INCLUSAO DOS AGRUPAMENTOS PERSONALIZADOS                  *
      *    - INCLUSAO DO PERIODO DE COBRANCA                           *
      *----------------------------------------------------------------*
0410  *    ULTIMA ALTERACAO EM 01/04/2010 - HELIO SANTONI NETO         *
0410  *    ALTERACOES REFERENTES AOS AGRUPAMENTOS 20 E 26.             *
      *                                                                *
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

0410   01  WRK-FIM-CURSOR              PIC  X(001)         VALUE 'N'.
0410XS 01  WRK-TARIFA-OK               PIC  X(001)         VALUE 'N'.
0410   01  WRK-TEM-PERMISSAO           PIC  X(001)         VALUE 'N'.

0410   01  WRK-ACM-SOLICITACAO         PIC  9(005) COMP-3  VALUE ZEROS.

0410XS 01  WRK-CPF-CGC-CONTROLE        PIC  X(002)         VALUE SPACES.
0410XS 01  FILLER                      REDEFINES WRK-CPF-CGC-CONTROLE.
0410XS     05  WRK-CPF-CGC-CONTROLE-9  PIC  9(002).

0410XS 01  WRK-CSERVC-TARIF            PIC S9(005)         VALUE ZEROS.
0410XS 01  FILLER                      REDEFINES WRK-CSERVC-TARIF.
0410XS     05  WRK-COD-TARIFA          PIC  9(005).

0410XS 01  WRK-QMAX-RTCAO              PIC S9(003)         VALUE ZEROS.
0410XS 01  FILLER                      REDEFINES WRK-QMAX-RTCAO.
0410XS     05  FILLER                  PIC  9(001).
0410XS     05  WRK-QTDE-MAX-RTCAO      PIC  9(002).

0410XS 01  WRK-QMAX-FRANQ-TARIF        PIC S9(003)         VALUE ZEROS.
0410XS 01  FILLER                      REDEFINES WRK-QMAX-FRANQ-TARIF.
0410XS     05  WRK-QTDE-MAX-FRANQ-TARIF
0410XS                                 PIC  9(003).

0410XS 01  WRK-PARM-PRMSS-AGNDA        PIC  9(001)         VALUE ZEROS.
0410XS 01  WRK-PARM-PRMSS-VLR          PIC  9(001)         VALUE ZEROS.
0410XS 01  WRK-PARM-PRMSS-PERC         PIC  9(001)         VALUE ZEROS.
0410XS 01  WRK-PARM-PRMSS-FRANQ        PIC  9(001)         VALUE ZEROS.
0410XS 01  WRK-PARM-PRMSS-PERDC        PIC  9(001)         VALUE ZEROS.

0410XS 01  WRK-SQLCODE                 PIC S9(009)         VALUE ZEROS.
0410XS 01  FILLER                      REDEFINES WRK-SQLCODE.
0410XS     05  FILLER                  PIC  X(006).
0410XS     05  WRK-COD-SQL             PIC  9(003).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(051)         VALUE
           '* AREA PARA MENSAGEM *'.
      *----------------------------------------------------------------*

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

0410XS 01  WRK-DT-INIC-FLEX-INV        PIC  9(008)         VALUE ZEROS.
0410XS 01  WRK-DT-FIM-FLEX-INV         PIC  9(008)         VALUE ZEROS.
0410XS 01  WRK-DT-INIC-TARIF-INV       PIC  9(008)         VALUE ZEROS.
0410XS 01  WRK-DT-FIM-TARIF-INV        PIC  9(008)         VALUE ZEROS.
0410XS 01  WRK-DT-INIC-PRMSS-INV       PIC  9(008)         VALUE ZEROS.
0410XS 01  WRK-DT-FIM-PRMSS-INV        PIC  9(008)         VALUE ZEROS.
0410XS 01  WRK-DT-PROCM-INV            PIC  9(008)         VALUE ZEROS.

0410XS 01  WRK-DATA-NOR                PIC  9(008)         VALUE ZEROS.
0410XS 01  FILLER                      REDEFINES WRK-DATA-NOR.
0410XS     05 WRK-DIA-NOR              PIC  9(002).
0410XS     05 WRK-MES-NOR              PIC  9(002).
0410XS     05 WRK-ANO-NOR              PIC  9(004).

       01  WRK-DATA-DB2.
           05 WRK-DIA-DB2              PIC  9(002)         VALUE ZEROS.
           05 FILLER                   PIC  X(001)         VALUE '.'.
           05 WRK-MES-DB2              PIC  9(002)         VALUE ZEROS.
           05 FILLER                   PIC  X(001)         VALUE '.'.
           05 WRK-ANO-DB2              PIC  9(004)         VALUE ZEROS.

       01  WRK-DATA-INV                PIC  9(008)         VALUE ZEROS.
0410XS 01  FILLER                      REDEFINES WRK-DATA-INV.
           05 WRK-ANO-INV              PIC  9(004).
           05 WRK-MES-INV              PIC  9(002).
           05 WRK-DIA-INV              PIC  9(002).

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
ST2506*----------------------------------------------------------------*
ST2506 01 WRK-BRAD2000                 PIC  X(08)  VALUE
ST2506       'BRAD2000'.
ST2506 
ST2506 01 WRK-AREA-BRAD2000.
ST2506    05 WRK-CAD-CPFCGC.
ST2506       10 WRK-CAD-NUMERO         PIC X(09)           VALUE SPACES.
ST2506       10 WRK-CAD-FILIAL         PIC X(04)           VALUE SPACES.        
ST2506 77  WRK-CAD-CONTROLE            PIC 9(02)           VALUE ZEROS.

ST2506*------------------------------------*----------------------------*
ST2506 01 FILLER                       PIC  X(051) VALUE
ST2506       '* AREA PARA MODULO GFCT20PJ *'.
ST2506*---------------------------------------------------------------*
ST2506 01  WRK-GFCT20PJ                PIC X(08)   VALUE
ST2506     'GFCT20PJ'.
ST2506
ST2506 01 WRK-AREA-GFCT20PJ.
ST2506 COPY 'GFCTW2PJ'.
ST2506
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
             10 WRK-5018-CTPO-FRANQ-TARIF     PIC  9(001) VALUE ZEROS.

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

0410       EXEC SQL
0410         INCLUDE GFCTB0A2
0410       END-EXEC.

0410       EXEC SQL
0410         INCLUDE GFCTB0H2
0410       END-EXEC.

0410       EXEC SQL
0410         INCLUDE GFCTB0M7
0410       END-EXEC.

0410       EXEC SQL
0410         INCLUDE PLCCB004
0410       END-EXEC.

0410XS*----------------------------------------------------------------*
0410XS 01  FILLER                      PIC  X(050)         VALUE
0410XS     '* AREA PARA DECLARE    *'.
0410XS*----------------------------------------------------------------*
0410XS
0410XS     EXEC SQL
0410XS      DECLARE
0410XS               CSR01-GFCTB0A2-JN CURSOR FOR
0410XS       SELECT
0410XS               DISTINCT(T1.CSERVC_TARIF)
0410XS         FROM
0410XS               DB2PRD.PARM_SERVC_TARIF  T1,
0410XS               DB2PRD.TASSOC_CJTO_NATUZ T2
0410XS        WHERE
0410XS              T1.CNATUZ_SERVC     = T2.CNATUZ_SERVC
0410XS          AND T2.CCJTO_SERVC      = :GFCTB0H2.CCJTO-SERVC
0410XS     ORDER BY
0410XS              T1.CSERVC_TARIF
0410XS     END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* FIM DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY 'I#GFCTN9'.

       COPY 'I#GFCTNA'.

       COPY 'I#GFCT0M'.

      *================================================================*
       PROCEDURE                       DIVISION USING GFCTN9-ENTRADA
                                                      GFCTNA-SAIDA
                                                      GFCT0M-AREA-ERROS.
      *================================================================*

      ******************************************************************
      *ROTINA INICIAL DO PROGRAMA                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

0410       IF  GFCTN9-CIDTFD-AGRP      EQUAL 'S'
0410           PERFORM 3000-PROCESSAR-POR-CONJ-SERV
0410       ELSE
               PERFORM 2000-PROCESSAR
0410       END-IF.

           PERFORM 9999-FINALIZAR.

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
                                          GFCTNA-SAIDA.

           INITIALIZE GFCTNA-SAIDA
                      GFCT0M-AREA-ERROS.

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

           MOVE 'GFCT5522'                 TO WRK-MODULO.
           MOVE 101                        TO GFCTFZ-LL.
           MOVE 'GFCT'                     TO GFCTFZ-SISTEMA.
           MOVE GFCTN9-TRANSACAO           TO GFCTFZ-TRANSACAO.
           MOVE GFCTN9-FUNCAO              TO GFCTFZ-FUNCAO.
           MOVE GFCTN9-FUNC-BDSCO          TO GFCTFZ-FUNC-BDSCO.
           MOVE 'N'                        TO GFCTFZ-FIM.

           CALL WRK-MODULO USING
                                                 GFCTFZ-ENTRADA
                                                 GFCTG1-SAIDA
                                                 GFCT0M-AREA-ERROS.

           IF  RETURN-CODE                 NOT EQUAL ZEROS
               MOVE 9                      TO GFCTNA-ERRO
               MOVE WRK-MODULO             TO WRK-NOME-MOD
                                              GFCT0M-PROGRAMA
               MOVE WRK-MSG01              TO GFCT0M-TEXTO
               MOVE 'APL'                  TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0408'             TO GFCT0M-TRANSACAO
               PERFORM 9999-FINALIZAR
           END-IF.

           IF  GFCTG1-ERRO                 EQUAL 9
               MOVE 'GFCT0408'             TO GFCT0M-TRANSACAO
               MOVE GFCTG1-COD-SQL-ERRO    TO GFCT0M-SQLCODE
                                              GFCTNA-COD-SQL-ERRO
               MOVE GFCTG1-ERRO            TO GFCTNA-ERRO
               MOVE GFCTG1-COD-MSG-ERRO    TO GFCTNA-COD-MSG-ERRO
               MOVE GFCTG1-DESC-MSG-ERRO   TO GFCT0M-TEXTO
                                              GFCTNA-DESC-MSG-ERRO
               PERFORM 9999-FINALIZAR
           END-IF.

           IF  GFCTG1-CSIT-ONLINE          EQUAL 1
               MOVE 9                      TO GFCTNA-ERRO
               MOVE 'SISTEMA INDISPONIVEL' TO GFCT0M-TEXTO
               MOVE WRK-MODULO             TO GFCT0M-PROGRAMA
               MOVE 'APL'                  TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0408'             TO GFCT0M-TRANSACAO
               PERFORM 9999-FINALIZAR
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

           MOVE 1010                       TO GFCTNA-LL.
           MOVE ZEROS                      TO GFCTNA-ZZ.
           MOVE GFCTN9-TRANSACAO           TO GFCTNA-TRANSACAO.
           MOVE GFCTN9-FUNCAO              TO GFCTNA-FUNCAO.
           MOVE GFCTN9-FUNC-BDSCO          TO GFCTNA-FUNC-BDSCO.

           MOVE ZEROS                      TO GFCTNA-ERRO
                                              GFCTNA-COD-SQL-ERRO
                                              GFCTNA-COD-MSG-ERRO.
           MOVE '0000'                     TO GFCTG2-COD-MSG.

           PERFORM 9900-OBTER-DESC-MSG.

           MOVE  GFCTG3-DESC-MSG           TO GFCTNA-DESC-MSG-ERRO.
080816     MOVE  SPACES                    TO GFCTNA-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA CONSISTIR DADOS                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1300-CONSISTIR-DADOS            SECTION.
      *----------------------------------------------------------------*

           IF  GFCTN9-TRANSACAO            EQUAL LOW-VALUES          OR
               GFCTN9-TRANSACAO            EQUAL SPACES              OR
               GFCTN9-FUNCAO               EQUAL LOW-VALUES          OR
               GFCTN9-FUNCAO               EQUAL SPACES              OR
               GFCTN9-FUNC-BDSCO           EQUAL LOW-VALUES          OR
               GFCTN9-FUNC-BDSCO           EQUAL SPACES              OR
               GFCTN9-INIC-VIG-FLEX        EQUAL LOW-VALUES          OR
               GFCTN9-INIC-VIG-FLEX        EQUAL SPACES              OR
               GFCTN9-FIM-VIG-FLEX         EQUAL LOW-VALUES          OR
               GFCTN9-FIM-VIG-FLEX         EQUAL SPACES              OR
               GFCTN9-RJUSTF-MANUT-SERVC   EQUAL LOW-VALUES          OR
               GFCTN9-RJUSTF-MANUT-SERVC   EQUAL SPACES              OR
               GFCTN9-COD-TARIFA           NOT NUMERIC               OR
               GFCTN9-CTIPO-OPER-MOTVO     NOT NUMERIC               OR
               GFCTN9-CTIPO-OPER-MOTVO     EQUAL ZEROS               OR
               GFCTN9-CMOTVO-JUSTF         NOT NUMERIC               OR
               GFCTN9-CMOTVO-JUSTF         EQUAL ZEROS               OR
               GFCTN9-VR-NEGOCIADO         NOT NUMERIC               OR
               GFCTN9-FRANQUIA             NOT NUMERIC               OR
               GFCTN9-PERIODO              NOT NUMERIC               OR
               GFCTN9-DIA-AGEND            NOT NUMERIC               OR
               GFCTN9-PERC-DESCTO          NOT NUMERIC               OR
               GFCTN9-DIAS-RETENCAO        NOT NUMERIC               OR
               GFCTN9-QTDE-OCOR            NOT NUMERIC               OR
               GFCTN9-DIA-SEMANA           NOT NUMERIC               OR
              (GFCTN9-IND-VALOR            NOT EQUAL 1 AND 2 AND 3)  OR
              (GFCTN9-AGENDAMENTO          NOT EQUAL 'S' AND 'N')    OR
230808        (GFCTN9-TODAS-AG             NOT EQUAL 'S' AND 'N')
               MOVE 1                      TO GFCTNA-ERRO
                                              GFCTNA-COD-MSG-ERRO
               MOVE ZEROS                  TO GFCTNA-COD-SQL-ERRO
               MOVE '0001'                 TO GFCTG2-COD-MSG
               PERFORM 9900-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG       TO GFCTNA-DESC-MSG-ERRO
               PERFORM 9999-FINALIZAR
           END-IF.

           EVALUATE GFCTN9-AGRUPAMENTO
             WHEN 4
ST2506          MOVE GFCTN9-CGC-CPF         TO GFCTW2PJ-E-CGC-CNPJ 
ST2506          MOVE GFCTN9-FILIAL          TO GFCTW2PJ-E-FILIAL  
ST2506          PERFORM 1311-VALIDAR-FORMATO-PJ
              
ST2506*        IF  GFCTN9-CGC-CPF          NOT NUMERIC OR
ST2506*            GFCTN9-CGC-CPF          EQUAL ZEROS OR
ST2506*            GFCTN9-FILIAL           NOT NUMERIC OR
ST2506         IF  GFCTN9-CGC-CPF          EQUAL SPACES OR
ST2506             GFCTN9-CGC-CPF          EQUAL LOW-VALUES  OR
ST2506             GFCTN9-FILIAL           EQUAL LOW-VALUES OR
                   GFCTN9-CONTROLE         NOT NUMERIC OR 
ST2506             GFCTW2PJ-S-INVALIDO 
                   MOVE 1                  TO GFCTNA-ERRO
                   MOVE 0773               TO GFCTNA-COD-MSG-ERRO
                   MOVE ZEROS              TO GFCTNA-COD-SQL-ERRO
                   MOVE '0773'             TO GFCTG2-COD-MSG
                   PERFORM 9900-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG    TO GFCTNA-DESC-MSG-ERRO
                   PERFORM 9999-FINALIZAR
               END-IF

             WHEN 9
               IF  GFCTN9-SEGMENTO         NOT NUMERIC OR
                   GFCTN9-SEGMENTO         EQUAL ZEROS
                   MOVE 1                  TO GFCTNA-ERRO
                   MOVE 0775               TO GFCTNA-COD-MSG-ERRO
                   MOVE ZEROS              TO GFCTNA-COD-SQL-ERRO
                   MOVE '0775'             TO GFCTG2-COD-MSG
                   PERFORM 9900-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG    TO GFCTNA-DESC-MSG-ERRO
                   PERFORM 9999-FINALIZAR
               END-IF

             WHEN 14
               IF  GFCTN9-AGENCIA          NOT NUMERIC OR
                  (GFCTN9-AGENCIA          EQUAL ZEROS AND
                   GFCTN9-TODAS-AG         NOT EQUAL 'S')
                   MOVE 1                  TO GFCTNA-ERRO
                   MOVE 0774               TO GFCTNA-COD-MSG-ERRO
                   MOVE ZEROS              TO GFCTNA-COD-SQL-ERRO
                   MOVE '0774'             TO GFCTG2-COD-MSG
                   PERFORM 9900-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG    TO GFCTNA-DESC-MSG-ERRO
                   PERFORM 9999-FINALIZAR
               END-IF

             WHEN 15
               IF  GFCTN9-AGENCIA          NOT NUMERIC OR
                   GFCTN9-POSTO            NOT NUMERIC OR
                   GFCTN9-AGENCIA          EQUAL ZEROS OR
                   GFCTN9-POSTO            EQUAL ZEROS
                   MOVE 1                  TO GFCTNA-ERRO
                   MOVE 0776               TO GFCTNA-COD-MSG-ERRO
                   MOVE ZEROS              TO GFCTNA-COD-SQL-ERRO
                   MOVE '0776'             TO GFCTG2-COD-MSG
                   PERFORM 9900-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG    TO GFCTNA-DESC-MSG-ERRO
                   PERFORM 9999-FINALIZAR
               END-IF

             WHEN 17
               IF  GFCTN9-AGENCIA          NOT NUMERIC OR
                   GFCTN9-POSTO            NOT NUMERIC OR
                   GFCTN9-AGENCIA          EQUAL ZEROS OR
                   GFCTN9-POSTO            EQUAL ZEROS
                   MOVE 1                  TO GFCTNA-ERRO
                   MOVE 0778               TO GFCTNA-COD-MSG-ERRO
                   MOVE ZEROS              TO GFCTNA-COD-SQL-ERRO
                   MOVE '0778'             TO GFCTG2-COD-MSG
                   PERFORM 9900-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG    TO GFCTNA-DESC-MSG-ERRO
                   PERFORM 9999-FINALIZAR
               END-IF

             WHEN 18
               IF  GFCTN9-MUNIC            NOT NUMERIC OR
                   GFCTN9-MUNIC            EQUAL ZEROS
                   MOVE 1                  TO GFCTNA-ERRO
                   MOVE 0779               TO GFCTNA-COD-MSG-ERRO
                   MOVE ZEROS              TO GFCTNA-COD-SQL-ERRO
                   MOVE '0779'             TO GFCTG2-COD-MSG
                   PERFORM 9900-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG    TO GFCTNA-DESC-MSG-ERRO
                   PERFORM 9999-FINALIZAR
               END-IF

             WHEN 19
               IF  GFCTN9-UF               EQUAL LOW-VALUES OR
                   GFCTN9-UF               EQUAL SPACES
                   MOVE 1                  TO GFCTNA-ERRO
                   MOVE 0780               TO GFCTNA-COD-MSG-ERRO
                   MOVE ZEROS              TO GFCTNA-COD-SQL-ERRO
                   MOVE '0780'             TO GFCTG2-COD-MSG
                   PERFORM 9900-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG    TO GFCTNA-DESC-MSG-ERRO
                   PERFORM 9999-FINALIZAR
               END-IF

0410         WHEN 20
0410           IF  GFCTN9-AGENCIA          NOT NUMERIC OR
0410               GFCTN9-POSTO            NOT NUMERIC OR
0410               GFCTN9-AGENCIA          EQUAL ZEROS OR
0410               GFCTN9-POSTO            EQUAL ZEROS
0410               MOVE 1                  TO GFCTNA-ERRO
0410               MOVE 0778               TO GFCTNA-COD-MSG-ERRO
0410               MOVE ZEROS              TO GFCTNA-COD-SQL-ERRO
0410               MOVE '0778'             TO GFCTG2-COD-MSG
0410               PERFORM 9900-OBTER-DESC-MSG
0410               MOVE GFCTG3-DESC-MSG    TO GFCTNA-DESC-MSG-ERRO
0410               PERFORM 9999-FINALIZAR
0410           END-IF

0410         WHEN 26
0410           IF  GFCTN9-GRUPO-CTBIL      NOT NUMERIC OR
0410               GFCTN9-SUB-GRUPO-CTBIL  NOT NUMERIC OR
0410               GFCTN9-GRUPO-CTBIL      EQUAL ZEROS
0410               MOVE 1                  TO GFCTNA-ERRO
0410               MOVE 1693               TO GFCTNA-COD-MSG-ERRO
0410               MOVE ZEROS              TO GFCTNA-COD-SQL-ERRO
0410               MOVE '1693'             TO GFCTG2-COD-MSG
0410               PERFORM 9900-OBTER-DESC-MSG
0410               MOVE GFCTG3-DESC-MSG    TO GFCTNA-DESC-MSG-ERRO
0410               PERFORM 9999-FINALIZAR
0410           END-IF

           END-EVALUATE.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
ST2506*----------------------------------------------------------------*
ST2506 1311-VALIDAR-FORMATO-PJ         SECTION.
ST2506*----------------------------------------------------------------*
ST2506  
ST2506     CALL WRK-GFCT20PJ           USING WRK-AREA-GFCT20PJ.
ST2506
ST2506     IF RETURN-CODE NOT EQUAL ZEROS 
ST2506     OR GFCTW2PJ-S-COD-RETORNO NOT EQUAL '00'
              MOVE 1                      TO GFCTNA-ERRO
              MOVE 0520                   TO GFCTNA-COD-MSG-ERRO
              MOVE '0520'                 TO GFCTG2-COD-MSG
              MOVE ZEROS                  TO GFCTNA-COD-SQL-ERRO
              PERFORM 9900-OBTER-DESC-MSG
              MOVE GFCTG3-DESC-MSG        TO GFCTNA-DESC-MSG-ERRO
              PERFORM 9999-FINALIZAR
ST2506     END-IF. 
ST2506                                 
ST2506*----------------------------------------------------------------*
ST2506 1311-99-FIM.                    EXIT.
ST2506*----------------------------------------------------------------*

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
               PERFORM 9900-OBTER-DESC-MSG
               MOVE ' - GFCTB0A1'      TO WRK-NOME-TAB
               MOVE 'PARM_DATA_PROCM ' TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
               MOVE '0010'             TO GFCT0M-LOCAL
               MOVE  0010              TO GFCTNA-COD-MSG-ERRO
               PERFORM 9000-MOVER-ERRO-SQL
               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA VALIDAR AGRUPAMENTO                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       1500-VALIDA-AGRUPAMENTO         SECTION.
      *----------------------------------------------------------------*

           EVALUATE GFCTN9-AGRUPAMENTO
               WHEN 15
                   PERFORM 1510-CONSISTIR-PAB

               WHEN 14
                   IF  GFCTN9-TODAS-AG NOT EQUAL 'S'
                       PERFORM 1520-CONSISTIR-AGENCIA
                   END-IF

               WHEN 17
0410               PERFORM 1530-VERIFICA-EXPSS-PTAL

               WHEN 4
                   PERFORM 1540-CONSISTIR-CLIENTE

               WHEN 9
                   PERFORM 1550-CONSISTIR-SEGMENTO

               WHEN 18
                   PERFORM 1560-CONSISTIR-MUNICIPIO

               WHEN 19
                   PERFORM 1570-CONSISTIR-UF

0410           WHEN 20
0410               PERFORM 1530-VERIFICA-EXPSS-PTAL

0410           WHEN 26
0410               PERFORM 1590-CONSISTIR-RAZAO

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
           MOVE GFCTN9-AGENCIA                 TO COMU-COD-DEPDC.
           MOVE GFCTN9-POSTO                   TO COMU-COD-POSTO.
           MOVE SPACES                         TO COMU-CORR-BANCARIO.

           MOVE 'MESU9018'                     TO WRK-MODULO.

           CALL WRK-MODULO USING
                                                     COMU-DADOS-9018.

           IF  RETURN-CODE                     NOT EQUAL ZEROS
               EVALUATE RETURN-CODE
                 WHEN 4
                   MOVE 1                      TO GFCTNA-ERRO
                   MOVE 0518                   TO GFCTNA-COD-MSG-ERRO
                   MOVE '0518'                 TO GFCTG2-COD-MSG
                   MOVE ZEROS                  TO GFCTNA-COD-SQL-ERRO
                   PERFORM 9900-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG        TO GFCTNA-DESC-MSG-ERRO
                   PERFORM 9999-FINALIZAR

                 WHEN 8
                   MOVE 1                      TO GFCTNA-ERRO
                   MOVE 0155                   TO GFCTNA-COD-MSG-ERRO
                   MOVE '0155'                 TO GFCTG2-COD-MSG
                   MOVE ZEROS                  TO GFCTNA-COD-SQL-ERRO
                   PERFORM 9900-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG        TO GFCTNA-DESC-MSG-ERRO
                   PERFORM 9999-FINALIZAR

                 WHEN 12
                   MOVE 1                      TO GFCTNA-ERRO
                   MOVE 0520                   TO GFCTNA-COD-MSG-ERRO
                   MOVE '0520'                 TO GFCTG2-COD-MSG
                   MOVE ZEROS                  TO GFCTNA-COD-SQL-ERRO
                   PERFORM 9900-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG        TO GFCTNA-DESC-MSG-ERRO
                   PERFORM 9999-FINALIZAR

               END-EVALUATE
           ELSE
               IF  COMU-COD-TIPO-POSTO         EQUAL 9
                   MOVE 1                      TO GFCTNA-ERRO
                   MOVE 0594                   TO GFCTNA-COD-MSG-ERRO
                   MOVE '0594'                 TO GFCTG2-COD-MSG
                   MOVE ZEROS                  TO GFCTNA-COD-SQL-ERRO
                   PERFORM 9900-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG        TO GFCTNA-DESC-MSG-ERRO
                   PERFORM 9999-FINALIZAR
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
           MOVE GFCTN9-AGENCIA         TO COMU-DEPDC.
           MOVE 'GFCT0408'             TO COMU-NOME-PGM.

           MOVE 'MESU9015'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                             COMU-PARM
                                             WRK-POOL7100
                                             WRK-SQLCA.

           IF  COMU-COD-RETORNO         NOT EQUAL ZEROS
               MOVE 1                   TO GFCTNA-ERRO
               MOVE 0465                TO GFCTNA-COD-MSG-ERRO
               MOVE '0465'              TO GFCTG2-COD-MSG
               MOVE ZEROS               TO GFCTNA-COD-SQL-ERRO
               PERFORM 9900-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG     TO GFCTNA-DESC-MSG-ERRO
               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1520-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

0410  ******************************************************************
0410  *ROTINA PARA CONSISTIR POSTAL E BRAD. EXPRESSO                   *
0410  ******************************************************************
0410  *----------------------------------------------------------------*
0410   1530-VERIFICA-EXPSS-PTAL        SECTION.
0410  *----------------------------------------------------------------*
0410XS
0410XS     MOVE SPACES                 TO WAM2-ENTRADA
0410XS                                    WAM3-SAIDA
0410XS                                    GFCT0M-AREA-ERROS.
0410
0410       INITIALIZE                     GFCT0M-AREA-ERROS
0410                                      WAM3-SAIDA
0410XS
0410XS     MOVE +100                   TO WAM2-LL.
0410XS     MOVE ZEROS                  TO WAM2-ZZ.
0410XS     MOVE GFCTN9-TRANSACAO       TO WAM2-TRANSACAO.
0410XS     MOVE GFCTN9-FUNCAO          TO WAM2-FUNCAO.
0410XS     MOVE GFCTN9-FUNC-BDSCO      TO WAM2-FUNC-BDSCO.
0410XS     MOVE ZEROS                  TO WAM2-TERMINAL.
0410XS     MOVE 'N'                    TO WAM2-FIM.
0410
0410       MOVE GFCTN9-AGRUPAMENTO     TO WAM2-CAGPTO
0410       MOVE GFCTN9-AGENCIA         TO WAM2-CAGENCIA
0410       MOVE GFCTN9-POSTO           TO WAM2-CPAB
0410       MOVE 'GFCT5296'             TO WRK-MODULO
0410
0410       CALL WRK-MODULO          USING WAM2-ENTRADA
0410                                      WAM3-SAIDA
0410                                      GFCT0M-AREA-ERROS.
0410
0410       IF  RETURN-CODE         NOT EQUAL ZEROS
0410           MOVE 9                  TO GFCTNA-ERRO
0410           MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
0410                                      WRK-NOME-MOD
0410           MOVE WRK-MSG01          TO GFCT0M-TEXTO
0410           MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
0410           MOVE 'GFCT0408'         TO GFCT0M-TRANSACAO
0410           PERFORM 9999-FINALIZAR
0410       END-IF.
0410
0410       IF  WAM3-ERRO              NOT EQUAL ZEROS
0410           MOVE WAM3-ERRO          TO GFCTNA-ERRO
0410           MOVE ZEROS              TO GFCTNA-COD-SQL-ERRO
0410           MOVE WAM3-COD-MSG-ERRO  TO GFCTNA-COD-MSG-ERRO
0410           MOVE WAM3-DESC-MSG-ERRO TO GFCTNA-DESC-MSG-ERRO
0410           PERFORM 9999-FINALIZAR
0410       END-IF.
0410
0410  *----------------------------------------------------------------*
0410   1530-99-FIM.                    EXIT.
0410  *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA CONSISTIR CLIENTE                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       1540-CONSISTIR-CLIENTE          SECTION.
      *----------------------------------------------------------------*

ST2506*    MOVE SPACES                 TO WRK-CONTROLE-0110.
ST2506*    MOVE GFCTN9-CGC-CPF         TO WRK-NUMERO-0110.
ST2506*    MOVE GFCTN9-FILIAL          TO WRK-FILIAL-0110.
ST2506*
ST2506*    CALL 'POOL0110'             USING WRK-CGC-CPF-0110
ST2506*                                      WRK-CONTROLE-0110.
ST2506*

ST2506     MOVE ZEROS          TO WRK-CAD-CONTROLE. 
ST2506     MOVE GFCTN9-CGC-CPF TO WRK-CAD-NUMERO.
ST2506     MOVE GFCTN9-FILIAL  TO WRK-CAD-FILIAL.
ST2506
ST2506     CALL WRK-BRAD2000     USING WRK-CAD-CPFCGC
ST2506                                 WRK-CAD-CONTROLE.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 1                  TO GFCTNA-ERRO
               MOVE 0468               TO GFCTNA-COD-MSG-ERRO
               MOVE '0468'             TO GFCTG2-COD-MSG
               MOVE ZEROS              TO GFCTNA-COD-SQL-ERRO
               PERFORM 9900-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNA-DESC-MSG-ERRO
               PERFORM 9999-FINALIZAR
           END-IF.

ST2506*    IF  GFCTN9-CONTROLE         NOT EQUAL WRK-CONTROLE-0110
ST2506     IF  GFCTN9-CONTROLE         NOT EQUAL WRK-CAD-CONTROLE
               MOVE 1                  TO GFCTNA-ERRO
               MOVE 0507               TO GFCTNA-COD-MSG-ERRO
               MOVE '0507'             TO GFCTG2-COD-MSG
               MOVE ZEROS              TO GFCTNA-COD-SQL-ERRO
               PERFORM 9900-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNA-DESC-MSG-ERRO
               PERFORM 9999-FINALIZAR
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
           MOVE GFCTN9-TRANSACAO       TO GFCTIU-TRANSACAO.
           MOVE GFCTN9-FUNCAO          TO GFCTIU-FUNCAO.
           MOVE GFCTN9-FUNC-BDSCO      TO GFCTIU-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTIU-FIM.
ST2506*    MOVE GFCTN9-CGC-CPF         TO GFCTIU-NRO-CPF-CNPJ.
ST2506*    MOVE GFCTN9-FILIAL          TO GFCTIU-NRO-FILIAL.
ST2506     MOVE GFCTN9-CGC-CPF         TO GFCTIU-COD-CPF-CNPJ.
ST2506     MOVE GFCTN9-FILIAL          TO GFCTIU-COD-FILIAL.
           MOVE GFCTN9-CONTROLE        TO WRK-CONTROLE.
           MOVE WRK-CONTROLE-R         TO GFCTIU-CTRL-CPF-CNPJ.

           MOVE 'GFCT5110'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                             GFCTIU-ENTRADA
                                             GFCTIV-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTNA-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-NOME-MOD
               MOVE WRK-MSG01          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0408'         TO GFCT0M-TRANSACAO
               PERFORM 9999-FINALIZAR
           END-IF.

           IF  GFCTIV-ERRO              NOT EQUAL ZEROS
               MOVE GFCTIV-ERRO         TO GFCTNA-ERRO
               MOVE GFCTIV-COD-SQL-ERRO TO GFCTNA-COD-SQL-ERRO
               MOVE GFCTIV-COD-MSG-ERRO TO GFCTNA-COD-MSG-ERRO
               STRING 'GFCT5110-'       GFCTIV-DESC-MSG-ERRO
               DELIMITED BY SIZE        INTO GFCTNA-DESC-MSG-ERRO
               PERFORM 9999-FINALIZAR
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

           MOVE GFCTN9-SEGMENTO       TO CSGMTO-GSTAO-TARIF OF GFCTB0H5.
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
               PERFORM 9900-OBTER-DESC-MSG
               MOVE ' - GFCTB0H5'      TO WRK-NOME-TAB
               MOVE 'TTPO_SGMTO_GSTAO' TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
               MOVE '0020'             TO GFCT0M-LOCAL
               MOVE  0012              TO GFCTNA-COD-MSG-ERRO
               PERFORM 9000-MOVER-ERRO-SQL
               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 1                  TO GFCTNA-ERRO
               MOVE 0500               TO GFCTNA-COD-MSG-ERRO
               MOVE '0500'             TO GFCTG2-COD-MSG
               MOVE ZEROS              TO GFCTNA-COD-SQL-ERRO
               PERFORM 9900-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNA-DESC-MSG-ERRO
               PERFORM 9999-FINALIZAR
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

           MOVE GFCTN9-MUNIC           TO COD-MUNIC-RURC88.
           MOVE 'RURC9020'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                             AREA-RURC88.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 1                  TO GFCTNA-ERRO
               MOVE ZEROS              TO GFCTNA-COD-SQL-ERRO
               MOVE 0732               TO GFCTNA-COD-MSG-ERRO
               MOVE '0732'             TO GFCTG2-COD-MSG
               PERFORM 9900-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNA-DESC-MSG-ERRO
               PERFORM 9999-FINALIZAR
           END-IF.

           IF  COD-RETORNO-RURC88      EQUAL 10
               MOVE 1                  TO GFCTNA-ERRO
               MOVE ZEROS              TO GFCTNA-COD-SQL-ERRO
               MOVE 0675               TO GFCTNA-COD-MSG-ERRO
               MOVE '0675'             TO GFCTG2-COD-MSG
               PERFORM 9900-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNA-DESC-MSG-ERRO
               PERFORM 9999-FINALIZAR
           END-IF.

           IF  COD-RETORNO-RURC88      EQUAL 20
               MOVE 1                  TO GFCTNA-ERRO
               MOVE ZEROS              TO GFCTNA-COD-SQL-ERRO
               MOVE 0733               TO GFCTNA-COD-MSG-ERRO
               MOVE '0733'             TO GFCTG2-COD-MSG
               PERFORM 9900-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNA-DESC-MSG-ERRO
               PERFORM 9999-FINALIZAR
           END-IF.

           IF  COD-RETORNO-RURC88      EQUAL 99
               MOVE 1                  TO GFCTNA-ERRO
               MOVE SQLCODE-RURC88     TO GFCTNA-COD-SQL-ERRO
               MOVE 0734               TO GFCTNA-COD-MSG-ERRO
               MOVE '0734'             TO GFCTG2-COD-MSG
               PERFORM 9900-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNA-DESC-MSG-ERRO
               PERFORM 9999-FINALIZAR
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

           MOVE GFCTN9-UF              TO CSGL-UF OF DCITV002.
           MOVE DCITV002               TO WRK-AREA-DCLGEN-DCITV002.

           MOVE 'DCIT8000'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                             WRK-DCIT-UNIDADE-FEDERATIVA
                                             WRK-ERRO-AREA
                                             SQLCA.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 1                  TO GFCTNA-ERRO
               MOVE ZEROS              TO GFCTNA-COD-SQL-ERRO
               MOVE 0735               TO GFCTNA-COD-MSG-ERRO
               MOVE '0735'             TO GFCTG2-COD-MSG
               PERFORM 9900-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNA-DESC-MSG-ERRO
               PERFORM 9999-FINALIZAR
           END-IF.

           IF  WRK-DCIT-CODIGO-RETORNO EQUAL 1
               MOVE 1                  TO GFCTNA-ERRO
               MOVE ZEROS              TO GFCTNA-COD-SQL-ERRO
               MOVE 0736               TO GFCTNA-COD-MSG-ERRO
               MOVE '0736'             TO GFCTG2-COD-MSG
               PERFORM 9900-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNA-DESC-MSG-ERRO
               PERFORM 9999-FINALIZAR
           END-IF.

           IF  WRK-DCIT-CODIGO-RETORNO EQUAL 2
               MOVE 1                  TO GFCTNA-ERRO
               MOVE ZEROS              TO GFCTNA-COD-SQL-ERRO
               MOVE 0737               TO GFCTNA-COD-MSG-ERRO
               MOVE '0737'             TO GFCTG2-COD-MSG
               PERFORM 9900-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNA-DESC-MSG-ERRO
               PERFORM 9999-FINALIZAR
           END-IF.

           IF  WRK-DCIT-CODIGO-RETORNO EQUAL 3
               MOVE 1                  TO GFCTNA-ERRO
               MOVE ZEROS              TO GFCTNA-COD-SQL-ERRO
               MOVE 0738               TO GFCTNA-COD-MSG-ERRO
               MOVE '0738'             TO GFCTG2-COD-MSG
               PERFORM 9900-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNA-DESC-MSG-ERRO
               PERFORM 9999-FINALIZAR
           END-IF.

           IF  WRK-DCIT-CODIGO-RETORNO EQUAL  4
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0005'             TO GFCTG2-COD-MSG
               PERFORM 9900-OBTER-DESC-MSG
               MOVE ' - DCITV002'      TO WRK-NOME-TAB
               MOVE 'V01_UF'           TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0030'             TO GFCT0M-LOCAL
               MOVE  0005              TO GFCTNA-COD-MSG-ERRO
               PERFORM 9000-MOVER-ERRO-SQL
               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1570-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * CONSITIR RAZAO                                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       1590-CONSISTIR-RAZAO            SECTION.
      *----------------------------------------------------------------*

           MOVE 237                    TO CEMPR            OF PLCCB004.
           MOVE GFCTN9-GRUPO-CTBIL     TO CGRP-CTBIL-ATUAL OF PLCCB004.
           MOVE GFCTN9-SUB-GRUPO-CTBIL TO CSGRP-CTBIL      OF PLCCB004.

           EXEC SQL
             SELECT
                   CEMPR
             INTO  :PLCCB004.CEMPR
             FROM  DB2PRD.RAZAO_CONTABIL
             WHERE CEMPR            = :PLCCB004.CEMPR
               AND CGRP_CTBIL_ATUAL = :PLCCB004.CGRP-CTBIL-ATUAL
               AND CSGRP_CTBIL      = :PLCCB004.CSGRP-CTBIL
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0012'             TO GFCTG2-COD-MSG
               PERFORM 9900-OBTER-DESC-MSG
               MOVE ' - PLCCB004'      TO WRK-NOME-TAB
               MOVE 'RAZAO_CONTABIL  ' TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
               MOVE '0025'             TO GFCT0M-LOCAL
               MOVE  0012              TO GFCTNA-COD-MSG-ERRO
               PERFORM 9000-MOVER-ERRO-SQL
               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 1                  TO GFCTNA-ERRO
               MOVE 1689               TO GFCTNA-COD-MSG-ERRO
               MOVE '1689'             TO GFCTG2-COD-MSG
               MOVE ZEROS              TO GFCTNA-COD-SQL-ERRO
               PERFORM 9900-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNA-DESC-MSG-ERRO
               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1590-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA ACESSAR GFCT5505                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       1600-ACESSAR-GFCT5505           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  WRK-5505-AREA-SAIDA.
           MOVE 'O'                    TO WRK-5505-AMBIENTE.
           MOVE GFCTN9-FUNC-BDSCO      TO WRK-5505-FUNC-BDSCO.

           MOVE 'GFCT5505'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                             WRK-5505-AREA-ENTRADA
                                             WRK-5505-AREA-SAIDA
                                             WRK-IO-PCB
                                             WRK-ALT-PCB.


           IF  WRK-5505-COD-RETORNO    NOT EQUAL ZEROS
               MOVE ZEROS              TO GFCTNA-COD-SQL-ERRO
               MOVE 1                  TO GFCTNA-ERRO
               MOVE 0112               TO GFCTNA-COD-MSG-ERRO
               MOVE WRK-5505-DESC-MSG  TO GFCTNA-DESC-MSG-ERRO
               PERFORM 9999-FINALIZAR
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

           EVALUATE GFCTN9-AGRUPAMENTO
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

0410           WHEN 20
0410               PERFORM 2130-CONSISTIR-PED-POSTAL

0410           WHEN 26
0410               PERFORM 2195-CONSISTIR-PED-RAZAO

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

           INITIALIZE                     GFCTB0B8

           MOVE GFCTN9-AGENCIA         TO CDEPDC            OF GFCTB0B8.
           MOVE GFCTN9-POSTO           TO CPOSTO-SERVC      OF GFCTB0B8.
           MOVE GFCTN9-COD-TARIFA      TO CSERVC-TARIF      OF GFCTB0B8.
           MOVE GFCTN9-AGRUPAMENTO     TO CAGPTO-CTA        OF GFCTB0B8.
           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO DINIC-FLEXZ-AGPTO OF GFCTB0B2
                                          DFIM-FLEXZ-AGPTO  OF GFCTB0B2.

           EXEC SQL
             SELECT A.CSIT_SOLTC_FLEXZ
             INTO   :GFCTB0B2.CSIT-SOLTC-FLEXZ
             FROM   DB2PRD.PDIDO_FLEXZ_AGPTO A,
                    DB2PRD.PDIDO_FLEXZ_PAB   B
             WHERE  A.CAGPTO_CTA         = B.CAGPTO_CTA
               AND  A.CSERVC_TARIF       = B.CSERVC_TARIF
               AND  A.HIDTFD_FLEXZ_AGPTO = B.HIDTFD_FLEXZ_AGPTO
               AND  A.DFIM_FLEXZ_AGPTO  >= :GFCTB0B2.DFIM-FLEXZ-AGPTO
               AND  B.CAGPTO_CTA         = :GFCTB0B8.CAGPTO-CTA
               AND  B.CSERVC_TARIF       = :GFCTB0B8.CSERVC-TARIF
               AND  B.CDEPDC             = :GFCTB0B8.CDEPDC
               AND  B.CPOSTO_SERVC       = :GFCTB0B8.CPOSTO-SERVC
               AND  B.HIDTFD_FLEXZ_AGPTO IN
              (SELECT MAX(B.HIDTFD_FLEXZ_AGPTO)
               FROM  DB2PRD.PDIDO_FLEXZ_AGPTO A,
                     DB2PRD.PDIDO_FLEXZ_PAB   B
               WHERE A.CAGPTO_CTA         = B.CAGPTO_CTA
                AND  A.CSERVC_TARIF       = B.CSERVC_TARIF
                AND  A.HIDTFD_FLEXZ_AGPTO = B.HIDTFD_FLEXZ_AGPTO
                AND  A.DFIM_FLEXZ_AGPTO  >= :GFCTB0B2.DFIM-FLEXZ-AGPTO
                AND  B.CAGPTO_CTA         = :GFCTB0B8.CAGPTO-CTA
                AND  B.CSERVC_TARIF       = :GFCTB0B8.CSERVC-TARIF
                AND  B.CDEPDC             = :GFCTB0B8.CDEPDC
                AND  B.CPOSTO_SERVC       = :GFCTB0B8.CPOSTO-SERVC)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0010'             TO GFCTG2-COD-MSG
IN             MOVE 9                  TO GFCTNA-ERRO
               PERFORM 9900-OBTER-DESC-MSG
               MOVE ' - GFCTB0B2'      TO WRK-NOME-TAB
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
               MOVE '0040'             TO GFCT0M-LOCAL
               MOVE  0010              TO GFCTNA-COD-MSG-ERRO
IN             PERFORM 9000-MOVER-ERRO-SQL
               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS AND
              (CSIT-SOLTC-FLEXZ        OF GFCTB0B2
                                       EQUAL 2 OR 8)
               MOVE 1                  TO GFCTNA-ERRO
               MOVE ZEROS              TO GFCTNA-COD-SQL-ERRO
               MOVE 0921               TO GFCTNA-COD-MSG-ERRO
               MOVE '0921'             TO GFCTG2-COD-MSG
               PERFORM 9900-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNA-DESC-MSG-ERRO
               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS AND
               CSIT-SOLTC-FLEXZ        OF GFCTB0B2
                                       EQUAL 1
               MOVE 1                  TO GFCTNA-ERRO
               MOVE ZEROS              TO GFCTNA-COD-SQL-ERRO
               MOVE 0920               TO GFCTNA-COD-MSG-ERRO
               MOVE '0920'             TO GFCTG2-COD-MSG
               PERFORM 9900-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNA-DESC-MSG-ERRO
               PERFORM 9999-FINALIZAR
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

           MOVE GFCTN9-AGENCIA         TO CDEPDC            OF GFCTB0B1.
           MOVE GFCTN9-COD-TARIFA      TO CSERVC-TARIF      OF GFCTB0B1.
           MOVE GFCTN9-AGRUPAMENTO     TO CAGPTO-CTA        OF GFCTB0B1.
           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO DINIC-FLEXZ-AGPTO OF GFCTB0B2
                                          DFIM-FLEXZ-AGPTO  OF GFCTB0B2.

           EXEC SQL
             SELECT A.CSIT_SOLTC_FLEXZ
             INTO   :GFCTB0B2.CSIT-SOLTC-FLEXZ
             FROM   DB2PRD.PDIDO_FLEXZ_AGPTO   A,
                    DB2PRD.PDIDO_FLEXZ_AG      B
             WHERE  A.CAGPTO_CTA         = B.CAGPTO_CTA
               AND  A.CSERVC_TARIF       = B.CSERVC_TARIF
               AND  A.HIDTFD_FLEXZ_AGPTO = B.HIDTFD_FLEXZ_AGPTO
               AND  A.DFIM_FLEXZ_AGPTO  >= :GFCTB0B2.DFIM-FLEXZ-AGPTO
               AND  B.CAGPTO_CTA         = :GFCTB0B1.CAGPTO-CTA
               AND  B.CSERVC_TARIF       = :GFCTB0B1.CSERVC-TARIF
               AND  B.CDEPDC             = :GFCTB0B1.CDEPDC
               AND  B.HIDTFD_FLEXZ_AGPTO IN
              (SELECT MAX(B.HIDTFD_FLEXZ_AGPTO)
               FROM  DB2PRD.PDIDO_FLEXZ_AGPTO   A,
                     DB2PRD.PDIDO_FLEXZ_AG      B
               WHERE A.CAGPTO_CTA         = B.CAGPTO_CTA
                AND  A.CSERVC_TARIF       = B.CSERVC_TARIF
                AND  A.HIDTFD_FLEXZ_AGPTO = B.HIDTFD_FLEXZ_AGPTO
                AND  A.DFIM_FLEXZ_AGPTO  >= :GFCTB0B2.DFIM-FLEXZ-AGPTO
                AND  B.CAGPTO_CTA         = :GFCTB0B1.CAGPTO-CTA
                AND  B.CSERVC_TARIF       = :GFCTB0B1.CSERVC-TARIF
                AND  B.CDEPDC             = :GFCTB0B1.CDEPDC)
           END-EXEC.

           IF (SQLCODE                     NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                    EQUAL 'W')
               INITIALIZE                  GFCT0M-ERRO-SQL
               MOVE '0010'                 TO GFCTG2-COD-MSG
               PERFORM 9900-OBTER-DESC-MSG
               MOVE ' - GFCTB0B2'          TO WRK-NOME-TAB
               MOVE 'PDIDO_FLEXZ_AGPTO'    TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '             TO GFCT0M-COMANDO-SQL
               MOVE '0050'                 TO GFCT0M-LOCAL
               MOVE  0010                  TO GFCTNA-COD-MSG-ERRO
               PERFORM 9000-MOVER-ERRO-SQL
               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                     EQUAL ZEROS AND
              (CSIT-SOLTC-FLEXZ            OF GFCTB0B2
                                           EQUAL 2 OR 8)
               MOVE 1                      TO GFCTNA-ERRO
               MOVE ZEROS                  TO GFCTNA-COD-SQL-ERRO
               MOVE 0921                   TO GFCTNA-COD-MSG-ERRO
               MOVE '0921'                 TO GFCTG2-COD-MSG
               PERFORM 9900-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG        TO GFCTNA-DESC-MSG-ERRO
               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                     EQUAL ZEROS AND
               CSIT-SOLTC-FLEXZ            OF GFCTB0B2
                                           EQUAL 1
               MOVE 1                      TO GFCTNA-ERRO
               MOVE ZEROS                  TO GFCTNA-COD-SQL-ERRO
               MOVE 0920                   TO GFCTNA-COD-MSG-ERRO
               MOVE '0920'                 TO GFCTG2-COD-MSG
               PERFORM 9900-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG        TO GFCTNA-DESC-MSG-ERRO
               PERFORM 9999-FINALIZAR
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

           MOVE GFCTN9-AGRUPAMENTO     TO CAGPTO-CTA        OF GFCTB0B1.
           MOVE GFCTN9-COD-TARIFA      TO CSERVC-TARIF      OF GFCTB0B1.

           IF  GFCTN9-TODAS-AG         EQUAL 'S'
               MOVE 00000              TO WRK-CDEPDC-INICIAL
               MOVE 99999              TO WRK-CDEPDC-FINAL
           ELSE
               MOVE 00000              TO WRK-CDEPDC-INICIAL
                                          WRK-CDEPDC-FINAL
           END-IF.

           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO DFIM-FLEXZ-AGPTO  OF GFCTB0B2.

           EXEC SQL
             SELECT B.CDEPDC
             INTO   :GFCTB0B1.CDEPDC
             FROM   DB2PRD.PDIDO_FLEXZ_AGPTO A,
                    DB2PRD.PDIDO_FLEXZ_AG    B
             WHERE  A.CAGPTO_CTA        = B.CAGPTO_CTA
               AND  A.CSERVC_TARIF      = B.CSERVC_TARIF
               AND  B.CAGPTO_CTA        = :GFCTB0B1.CAGPTO-CTA
               AND  B.CSERVC_TARIF      = :GFCTB0B1.CSERVC-TARIF
               AND (B.CDEPDC           >= :WRK-CDEPDC-INICIAL
               AND  B.CDEPDC           <= :WRK-CDEPDC-FINAL)
               AND  A.DFIM_FLEXZ_AGPTO >= :GFCTB0B2.DFIM-FLEXZ-AGPTO
               AND  A.CSIT_SOLTC_FLEXZ IN (1, 2, 8)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100
                                                       AND -811) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE                 GFCT0M-ERRO-SQL
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 9900-OBTER-DESC-MSG
               MOVE ' - GFCTB0B2'      TO WRK-NOME-TAB
               MOVE 'PDIDO_FLEXZ_AGPTO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
               MOVE '0060'             TO GFCT0M-LOCAL
               MOVE  0010              TO GFCTNA-COD-MSG-ERRO
               PERFORM 9000-MOVER-ERRO-SQL
               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS OR -811
               MOVE 1                  TO GFCTNA-ERRO
               MOVE ZEROS              TO GFCTNA-COD-SQL-ERRO

               IF  GFCTN9-TODAS-AG     EQUAL 'S'
                   MOVE 1001           TO GFCTNA-COD-MSG-ERRO
                   MOVE '1001'         TO GFCTG2-COD-MSG
               ELSE
                   MOVE 1002           TO GFCTNA-COD-MSG-ERRO
                   MOVE '1002'         TO GFCTG2-COD-MSG
               END-IF

               PERFORM 9900-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNA-DESC-MSG-ERRO
               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2125-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA CONSISTIR PEDIDO POSTAL                             *
      ******************************************************************
      *----------------------------------------------------------------*
       2130-CONSISTIR-PED-POSTAL       SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTN9-AGENCIA         TO CDEPDC            OF GFCTB0B9.
           MOVE GFCTN9-POSTO           TO CPOSTO-SERVC      OF GFCTB0B9.
           MOVE GFCTN9-COD-TARIFA      TO CSERVC-TARIF      OF GFCTB0B9.
           MOVE GFCTN9-AGRUPAMENTO     TO CAGPTO-CTA        OF GFCTB0B9.
           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO DINIC-FLEXZ-AGPTO OF GFCTB0B2
                                          DFIM-FLEXZ-AGPTO  OF GFCTB0B2.

           EXEC SQL
             SELECT A.CSIT_SOLTC_FLEXZ
             INTO   :GFCTB0B2.CSIT-SOLTC-FLEXZ
             FROM   DB2PRD.PDIDO_FLEXZ_AGPTO A,
                    DB2PRD.PDIDO_FLEXZ_PSTAL B
             WHERE  A.CAGPTO_CTA         = B.CAGPTO_CTA
               AND  A.CSERVC_TARIF       = B.CSERVC_TARIF
               AND  A.HIDTFD_FLEXZ_AGPTO = B.HIDTFD_FLEXZ_AGPTO
               AND  A.DFIM_FLEXZ_AGPTO  >= :GFCTB0B2.DFIM-FLEXZ-AGPTO
               AND  B.CAGPTO_CTA         = :GFCTB0B9.CAGPTO-CTA
               AND  B.CSERVC_TARIF       = :GFCTB0B9.CSERVC-TARIF
               AND  B.CDEPDC             = :GFCTB0B9.CDEPDC
               AND  B.CPOSTO_SERVC       = :GFCTB0B9.CPOSTO-SERVC
               AND  B.HIDTFD_FLEXZ_AGPTO IN
              (SELECT MAX(B.HIDTFD_FLEXZ_AGPTO)
               FROM  DB2PRD.PDIDO_FLEXZ_AGPTO A,
                     DB2PRD.PDIDO_FLEXZ_PSTAL B
               WHERE A.CAGPTO_CTA         = B.CAGPTO_CTA
                AND  A.CSERVC_TARIF       = B.CSERVC_TARIF
                AND  A.HIDTFD_FLEXZ_AGPTO = B.HIDTFD_FLEXZ_AGPTO
                AND  A.DFIM_FLEXZ_AGPTO  >= :GFCTB0B2.DFIM-FLEXZ-AGPTO
                AND  B.CAGPTO_CTA         = :GFCTB0B9.CAGPTO-CTA
                AND  B.CSERVC_TARIF       = :GFCTB0B9.CSERVC-TARIF
                AND  B.CDEPDC             = :GFCTB0B9.CDEPDC
                AND  B.CPOSTO_SERVC       = :GFCTB0B9.CPOSTO-SERVC)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 9900-OBTER-DESC-MSG
               MOVE ' - GFCTB0B2'      TO WRK-NOME-TAB
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
               MOVE '0070'             TO GFCT0M-LOCAL
               MOVE  0010              TO GFCTNA-COD-MSG-ERRO
               PERFORM 9000-MOVER-ERRO-SQL
               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS AND
              (CSIT-SOLTC-FLEXZ        OF GFCTB0B2
                                       EQUAL 2 OR 8)
               MOVE 1                  TO GFCTNA-ERRO
               MOVE ZEROS              TO GFCTNA-COD-SQL-ERRO
               MOVE 0921               TO GFCTNA-COD-MSG-ERRO
               MOVE '0921'             TO GFCTG2-COD-MSG
               PERFORM 9900-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNA-DESC-MSG-ERRO
               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS AND
               CSIT-SOLTC-FLEXZ        OF GFCTB0B2
                                       EQUAL 1
               MOVE 1                  TO GFCTNA-ERRO
               MOVE ZEROS              TO GFCTNA-COD-SQL-ERRO
               MOVE 0920               TO GFCTNA-COD-MSG-ERRO
               MOVE '0920'             TO GFCTG2-COD-MSG
               PERFORM 9900-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNA-DESC-MSG-ERRO
               PERFORM 9999-FINALIZAR
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

           MOVE GFCTN9-CGC-CPF         TO CCGC-CPF          OF GFCTB0B3.
           MOVE GFCTN9-FILIAL          TO CFLIAL-CGC        OF GFCTB0B3.
           MOVE GFCTN9-CONTROLE        TO CCTRL-CPF-CGC     OF GFCTB0B3.
           MOVE GFCTN9-COD-TARIFA      TO CSERVC-TARIF      OF GFCTB0B3.
           MOVE GFCTN9-AGRUPAMENTO     TO CAGPTO-CTA        OF GFCTB0B3.
           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO DINIC-FLEXZ-AGPTO OF GFCTB0B2
                                          DFIM-FLEXZ-AGPTO  OF GFCTB0B2.

           EXEC SQL
             SELECT A.CSIT_SOLTC_FLEXZ
             INTO   :GFCTB0B2.CSIT-SOLTC-FLEXZ
             FROM   DB2PRD.PDIDO_FLEXZ_AGPTO   A,
                    DB2PRD.PDIDO_FLEXZ_CLI     B
             WHERE  A.CAGPTO_CTA         = B.CAGPTO_CTA
               AND  A.CSERVC_TARIF       = B.CSERVC_TARIF
               AND  A.HIDTFD_FLEXZ_AGPTO = B.HIDTFD_FLEXZ_AGPTO
               AND  A.DFIM_FLEXZ_AGPTO  >= :GFCTB0B2.DFIM-FLEXZ-AGPTO
               AND  B.CAGPTO_CTA         = :GFCTB0B3.CAGPTO-CTA
               AND  B.CSERVC_TARIF       = :GFCTB0B3.CSERVC-TARIF
               AND  B.CCGC_CPF           = :GFCTB0B3.CCGC-CPF
               AND  B.CFLIAL_CGC         = :GFCTB0B3.CFLIAL-CGC
               AND  B.CCTRL_CPF_CGC      = :GFCTB0B3.CCTRL-CPF-CGC
               AND  B.HIDTFD_FLEXZ_AGPTO IN
              (SELECT MAX(B.HIDTFD_FLEXZ_AGPTO)
               FROM  DB2PRD.PDIDO_FLEXZ_AGPTO  A,
                     DB2PRD.PDIDO_FLEXZ_CLI    B
               WHERE A.CAGPTO_CTA         = B.CAGPTO_CTA
                AND  A.CSERVC_TARIF       = B.CSERVC_TARIF
                AND  A.HIDTFD_FLEXZ_AGPTO = B.HIDTFD_FLEXZ_AGPTO
                AND  A.DFIM_FLEXZ_AGPTO  >= :GFCTB0B2.DFIM-FLEXZ-AGPTO
                AND  B.CAGPTO_CTA         = :GFCTB0B3.CAGPTO-CTA
                AND  B.CSERVC_TARIF       = :GFCTB0B3.CSERVC-TARIF
                AND  B.CCGC_CPF           = :GFCTB0B3.CCGC-CPF
                AND  B.CFLIAL_CGC         = :GFCTB0B3.CFLIAL-CGC
                AND  B.CCTRL_CPF_CGC      = :GFCTB0B3.CCTRL-CPF-CGC)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 9900-OBTER-DESC-MSG
               MOVE ' - GFCTB0B2'      TO WRK-NOME-TAB
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
               MOVE '0080'             TO GFCT0M-LOCAL
               MOVE  0010              TO GFCTNA-COD-MSG-ERRO
               PERFORM 9000-MOVER-ERRO-SQL
               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS AND
              (CSIT-SOLTC-FLEXZ        OF GFCTB0B2
                                       EQUAL 2 OR 8)
               MOVE 1                  TO GFCTNA-ERRO
               MOVE ZEROS              TO GFCTNA-COD-SQL-ERRO
               MOVE 0921               TO GFCTNA-COD-MSG-ERRO
               MOVE '0921'             TO GFCTG2-COD-MSG
               PERFORM 9900-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNA-DESC-MSG-ERRO
               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS AND
               CSIT-SOLTC-FLEXZ        OF GFCTB0B2
                                       EQUAL 1
               MOVE 1                  TO GFCTNA-ERRO
               MOVE ZEROS              TO GFCTNA-COD-SQL-ERRO
               MOVE 0920               TO GFCTNA-COD-MSG-ERRO
               MOVE '0920'             TO GFCTG2-COD-MSG
               PERFORM 9900-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNA-DESC-MSG-ERRO
               PERFORM 9999-FINALIZAR
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

           MOVE GFCTN9-SEGMENTO       TO CSGMTO-GSTAO-TARIF OF GFCTB0C0.
           MOVE GFCTN9-COD-TARIFA     TO CSERVC-TARIF       OF GFCTB0C0
                                         CSERVC-TARIF       OF GFCTB0B2.
           MOVE GFCTN9-AGRUPAMENTO    TO CAGPTO-CTA         OF GFCTB0C0
                                         CAGPTO-CTA         OF GFCTB0B2.
           MOVE DPROCM-ATUAL          OF GFCTB0A1
                                      TO DINIC-FLEXZ-AGPTO  OF GFCTB0B2
                                         DFIM-FLEXZ-AGPTO   OF GFCTB0B2.

           EXEC SQL
            SELECT A.CSIT_SOLTC_FLEXZ
            INTO   :GFCTB0B2.CSIT-SOLTC-FLEXZ
            FROM   DB2PRD.PDIDO_FLEXZ_AGPTO   A,
                   DB2PRD.PDIDO_FLEXZ_SGMTO   B
            WHERE  A.CAGPTO_CTA         = B.CAGPTO_CTA
              AND  A.CSERVC_TARIF       = B.CSERVC_TARIF
              AND  A.HIDTFD_FLEXZ_AGPTO = B.HIDTFD_FLEXZ_AGPTO
              AND  A.CAGPTO_CTA         = :GFCTB0B2.CAGPTO-CTA
              AND  A.CSERVC_TARIF       = :GFCTB0B2.CSERVC-TARIF
              AND  A.DFIM_FLEXZ_AGPTO  >= :GFCTB0B2.DFIM-FLEXZ-AGPTO
              AND  B.CAGPTO_CTA         = :GFCTB0C0.CAGPTO-CTA
              AND  B.CSERVC_TARIF       = :GFCTB0C0.CSERVC-TARIF
              AND  B.CSGMTO_GSTAO_TARIF = :GFCTB0C0.CSGMTO-GSTAO-TARIF
              AND  B.HIDTFD_FLEXZ_AGPTO IN
             (SELECT MAX(B.HIDTFD_FLEXZ_AGPTO)
              FROM  DB2PRD.PDIDO_FLEXZ_AGPTO   A,
                    DB2PRD.PDIDO_FLEXZ_SGMTO   B
              WHERE A.CAGPTO_CTA         = B.CAGPTO_CTA
               AND  A.CSERVC_TARIF       = B.CSERVC_TARIF
               AND  A.HIDTFD_FLEXZ_AGPTO = B.HIDTFD_FLEXZ_AGPTO
               AND  A.CAGPTO_CTA         = :GFCTB0B2.CAGPTO-CTA
               AND  A.CSERVC_TARIF       = :GFCTB0B2.CSERVC-TARIF
               AND  A.DFIM_FLEXZ_AGPTO  >= :GFCTB0B2.DFIM-FLEXZ-AGPTO
               AND  B.CAGPTO_CTA         = :GFCTB0C0.CAGPTO-CTA
               AND  B.CSERVC_TARIF       = :GFCTB0C0.CSERVC-TARIF
               AND  B.CSGMTO_GSTAO_TARIF = :GFCTB0C0.CSGMTO-GSTAO-TARIF)
           END-EXEC.

           IF (SQLCODE                    NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                   EQUAL 'W')
               INITIALIZE                 GFCT0M-ERRO-SQL
               MOVE '0010'                TO GFCTG2-COD-MSG
               PERFORM 9900-OBTER-DESC-MSG
               MOVE ' - GFCTB0B2'         TO WRK-NOME-TAB
               MOVE 'PDIDO_FLEXZ_AGPTO'   TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '            TO GFCT0M-COMANDO-SQL
               MOVE '0090'                TO GFCT0M-LOCAL
               MOVE  0010                 TO GFCTNA-COD-MSG-ERRO
               PERFORM 9000-MOVER-ERRO-SQL
               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                    EQUAL ZEROS AND
              (CSIT-SOLTC-FLEXZ           OF GFCTB0B2
                                          EQUAL 2 OR 8)
               MOVE 1                     TO GFCTNA-ERRO
               MOVE ZEROS                 TO GFCTNA-COD-SQL-ERRO
               MOVE 0921                  TO GFCTNA-COD-MSG-ERRO
               MOVE '0921'                TO GFCTG2-COD-MSG
               PERFORM 9900-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG       TO GFCTNA-DESC-MSG-ERRO
               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                    EQUAL ZEROS AND
               CSIT-SOLTC-FLEXZ           OF GFCTB0B2
                                          EQUAL 1
               MOVE 1                     TO GFCTNA-ERRO
               MOVE ZEROS                 TO GFCTNA-COD-SQL-ERRO
               MOVE 0920                  TO GFCTNA-COD-MSG-ERRO
               MOVE '0920'                TO GFCTG2-COD-MSG
               PERFORM 9900-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG       TO GFCTNA-DESC-MSG-ERRO
               PERFORM 9999-FINALIZAR
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

           MOVE GFCTN9-MUNIC           TO CMUN-IBGE         OF GFCTB0B7.
           MOVE GFCTN9-COD-TARIFA      TO CSERVC-TARIF      OF GFCTB0B7
                                          CSERVC-TARIF      OF GFCTB0B2.
           MOVE GFCTN9-AGRUPAMENTO     TO CAGPTO-CTA        OF GFCTB0B7
                                          CAGPTO-CTA        OF GFCTB0B2.
           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO DINIC-FLEXZ-AGPTO OF GFCTB0B2
                                          DFIM-FLEXZ-AGPTO  OF GFCTB0B2.

           EXEC SQL
             SELECT A.CSIT_SOLTC_FLEXZ
             INTO   :GFCTB0B2.CSIT-SOLTC-FLEXZ
             FROM   DB2PRD.PDIDO_FLEXZ_AGPTO   A,
                    DB2PRD.PDIDO_FLEXZ_MUN     B
             WHERE  A.CAGPTO_CTA         = B.CAGPTO_CTA
               AND  A.CSERVC_TARIF       = B.CSERVC_TARIF
               AND  A.HIDTFD_FLEXZ_AGPTO = B.HIDTFD_FLEXZ_AGPTO
               AND  A.CAGPTO_CTA         = :GFCTB0B2.CAGPTO-CTA
               AND  A.CSERVC_TARIF       = :GFCTB0B2.CSERVC-TARIF
               AND  A.DFIM_FLEXZ_AGPTO  >= :GFCTB0B2.DFIM-FLEXZ-AGPTO
               AND  B.CAGPTO_CTA         = :GFCTB0B7.CAGPTO-CTA
               AND  B.CSERVC_TARIF       = :GFCTB0B7.CSERVC-TARIF
               AND  B.CMUN_IBGE          = :GFCTB0B7.CMUN-IBGE
               AND  B.HIDTFD_FLEXZ_AGPTO IN
              (SELECT MAX(B.HIDTFD_FLEXZ_AGPTO)
               FROM  DB2PRD.PDIDO_FLEXZ_AGPTO   A,
                     DB2PRD.PDIDO_FLEXZ_MUN     B
               WHERE A.CAGPTO_CTA         = B.CAGPTO_CTA
                AND  A.CSERVC_TARIF       = B.CSERVC_TARIF
                AND  A.HIDTFD_FLEXZ_AGPTO = B.HIDTFD_FLEXZ_AGPTO
                AND  A.CAGPTO_CTA         = :GFCTB0B2.CAGPTO-CTA
                AND  A.CSERVC_TARIF       = :GFCTB0B2.CSERVC-TARIF
                AND  A.DFIM_FLEXZ_AGPTO  >= :GFCTB0B2.DFIM-FLEXZ-AGPTO
                AND  B.CAGPTO_CTA         = :GFCTB0B7.CAGPTO-CTA
                AND  B.CSERVC_TARIF       = :GFCTB0B7.CSERVC-TARIF
                AND  B.CMUN_IBGE          = :GFCTB0B7.CMUN-IBGE)
           END-EXEC.

           IF (SQLCODE                     NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                    EQUAL 'W')
               INITIALIZE                  GFCT0M-ERRO-SQL
               MOVE '0010'                 TO GFCTG2-COD-MSG
               PERFORM 9900-OBTER-DESC-MSG
               MOVE ' - GFCTB0B2'          TO WRK-NOME-TAB
               MOVE 'PDIDO_FLEXZ_AGPTO'    TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '             TO GFCT0M-COMANDO-SQL
               MOVE '0100'                 TO GFCT0M-LOCAL
               MOVE  0010                  TO GFCTNA-COD-MSG-ERRO
               PERFORM 9000-MOVER-ERRO-SQL
               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                     EQUAL ZEROS AND
              (CSIT-SOLTC-FLEXZ            OF GFCTB0B2
                                           EQUAL 2 OR 8)
               MOVE 1                      TO GFCTNA-ERRO
               MOVE ZEROS                  TO GFCTNA-COD-SQL-ERRO
               MOVE 0921                   TO GFCTNA-COD-MSG-ERRO
               MOVE '0921'                 TO GFCTG2-COD-MSG
               PERFORM 9900-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG        TO GFCTNA-DESC-MSG-ERRO
               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                     EQUAL ZEROS AND
               CSIT-SOLTC-FLEXZ            OF GFCTB0B2
                                           EQUAL 1
               MOVE 1                      TO GFCTNA-ERRO
               MOVE ZEROS                  TO GFCTNA-COD-SQL-ERRO
               MOVE 0920                   TO GFCTNA-COD-MSG-ERRO
               MOVE '0920'                 TO GFCTG2-COD-MSG
               PERFORM 9900-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG        TO GFCTNA-DESC-MSG-ERRO
               PERFORM 9999-FINALIZAR
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

           MOVE GFCTN9-UF              TO CSGL-UF           OF GFCTB0C1.
           MOVE GFCTN9-COD-TARIFA      TO CSERVC-TARIF      OF GFCTB0C1
                                          CSERVC-TARIF      OF GFCTB0B2.
           MOVE GFCTN9-AGRUPAMENTO     TO CAGPTO-CTA        OF GFCTB0C1
                                          CAGPTO-CTA        OF GFCTB0B2.
           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO DINIC-FLEXZ-AGPTO OF GFCTB0B2
                                          DFIM-FLEXZ-AGPTO  OF GFCTB0B2.

           EXEC SQL
             SELECT A.CSIT_SOLTC_FLEXZ
             INTO   :GFCTB0B2.CSIT-SOLTC-FLEXZ
             FROM   DB2PRD.PDIDO_FLEXZ_AGPTO   A,
                    DB2PRD.PDIDO_FLEXZ_UF      B
             WHERE  A.CAGPTO_CTA         = B.CAGPTO_CTA
               AND  A.CSERVC_TARIF       = B.CSERVC_TARIF
               AND  A.HIDTFD_FLEXZ_AGPTO = B.HIDTFD_FLEXZ_AGPTO
               AND  A.CAGPTO_CTA         = :GFCTB0B2.CAGPTO-CTA
               AND  A.CSERVC_TARIF       = :GFCTB0B2.CSERVC-TARIF
               AND  A.DFIM_FLEXZ_AGPTO  >= :GFCTB0B2.DFIM-FLEXZ-AGPTO
               AND  B.CAGPTO_CTA         = :GFCTB0C1.CAGPTO-CTA
               AND  B.CSERVC_TARIF       = :GFCTB0C1.CSERVC-TARIF
               AND  B.CSGL_UF            = :GFCTB0C1.CSGL-UF
               AND  B.HIDTFD_FLEXZ_AGPTO IN
              (SELECT MAX(B.HIDTFD_FLEXZ_AGPTO)
               FROM  DB2PRD.PDIDO_FLEXZ_AGPTO   A,
                     DB2PRD.PDIDO_FLEXZ_UF      B
               WHERE A.CAGPTO_CTA         = B.CAGPTO_CTA
                AND  A.CSERVC_TARIF       = B.CSERVC_TARIF
                AND  A.HIDTFD_FLEXZ_AGPTO = B.HIDTFD_FLEXZ_AGPTO
                AND  A.CAGPTO_CTA         = :GFCTB0B2.CAGPTO-CTA
                AND  A.CSERVC_TARIF       = :GFCTB0B2.CSERVC-TARIF
                AND  A.DFIM_FLEXZ_AGPTO  >= :GFCTB0B2.DFIM-FLEXZ-AGPTO
                AND  B.CAGPTO_CTA         = :GFCTB0C1.CAGPTO-CTA
                AND  B.CSERVC_TARIF       = :GFCTB0C1.CSERVC-TARIF
                AND  B.CSGL_UF            = :GFCTB0C1.CSGL-UF)
           END-EXEC.

           IF (SQLCODE                     NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                    EQUAL 'W')
               INITIALIZE                  GFCT0M-ERRO-SQL
               MOVE '0010'                 TO GFCTG2-COD-MSG
               PERFORM 9900-OBTER-DESC-MSG
               MOVE ' - GFCTB0B2'          TO WRK-NOME-TAB
               MOVE 'PDIDO_FLEXZ_AGPTO'    TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '             TO GFCT0M-COMANDO-SQL
               MOVE '0110'                 TO GFCT0M-LOCAL
               MOVE  0010                  TO GFCTNA-COD-MSG-ERRO
               PERFORM 9000-MOVER-ERRO-SQL
               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                     EQUAL ZEROS AND
              (CSIT-SOLTC-FLEXZ            OF GFCTB0B2
                                           EQUAL 2 OR 8)
               MOVE 1                      TO GFCTNA-ERRO
               MOVE ZEROS                  TO GFCTNA-COD-SQL-ERRO
               MOVE 0921                   TO GFCTNA-COD-MSG-ERRO
               MOVE '0921'                 TO GFCTG2-COD-MSG
               PERFORM 9900-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG        TO GFCTNA-DESC-MSG-ERRO
               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                     EQUAL ZEROS AND
               CSIT-SOLTC-FLEXZ            OF GFCTB0B2
                                           EQUAL 1
               MOVE 1                      TO GFCTNA-ERRO
               MOVE ZEROS                  TO GFCTNA-COD-SQL-ERRO
               MOVE 0920                   TO GFCTNA-COD-MSG-ERRO
               MOVE '0920'                 TO GFCTG2-COD-MSG
               PERFORM 9900-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG        TO GFCTNA-DESC-MSG-ERRO
               PERFORM 9999-FINALIZAR
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
080508     MOVE GFCTN9-COD-TARIFA      TO CSERVC-TARIF      OF GFCTB0B2.
080508     MOVE GFCTN9-AGRUPAMENTO     TO CAGPTO-CTA        OF GFCTB0B2.
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
080508         PERFORM 9900-OBTER-DESC-MSG
080508         MOVE ' - GFCTB0B2'          TO WRK-NOME-TAB
080508         MOVE 'PDIDO_FLEXZ_AGPTO'    TO GFCT0M-NOME-TAB
080508         MOVE 'SELECT  '             TO GFCT0M-COMANDO-SQL
080508         MOVE '0120'                 TO GFCT0M-LOCAL
080508         MOVE  0010                  TO GFCTNA-COD-MSG-ERRO
080508         PERFORM 9000-MOVER-ERRO-SQL
080508         PERFORM 9999-FINALIZAR
080508     END-IF.
080508
080508     IF  SQLCODE                     EQUAL ZEROS AND
080508        (CSIT-SOLTC-FLEXZ            OF GFCTB0B2
080508                                     EQUAL 2 OR 8)
080508         MOVE 1                      TO GFCTNA-ERRO
080508         MOVE ZEROS                  TO GFCTNA-COD-SQL-ERRO
080508         MOVE 0921                   TO GFCTNA-COD-MSG-ERRO
080508         MOVE '0921'                 TO GFCTG2-COD-MSG
080508         PERFORM 9900-OBTER-DESC-MSG
080508         MOVE GFCTG3-DESC-MSG        TO GFCTNA-DESC-MSG-ERRO
080508         PERFORM 9999-FINALIZAR
080508     END-IF.
080508
080508     IF  SQLCODE                     EQUAL ZEROS AND
080508         CSIT-SOLTC-FLEXZ            OF GFCTB0B2
080508                                     EQUAL 1
080508         MOVE 1                      TO GFCTNA-ERRO
080508         MOVE ZEROS                  TO GFCTNA-COD-SQL-ERRO
080508         MOVE 0920                   TO GFCTNA-COD-MSG-ERRO
080508         MOVE '0920'                 TO GFCTG2-COD-MSG
080508         PERFORM 9900-OBTER-DESC-MSG
080508         MOVE GFCTG3-DESC-MSG        TO GFCTNA-DESC-MSG-ERRO
080508         PERFORM 9999-FINALIZAR
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
080508     MOVE GFCTN9-COD-TARIFA      TO CSERVC-TARIF      OF GFCTB0B2.
080508     MOVE GFCTN9-AGRUPAMENTO     TO CAGPTO-CTA        OF GFCTB0B2.
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
080508         PERFORM 9900-OBTER-DESC-MSG
080508         MOVE ' - GFCTB0B2'          TO WRK-NOME-TAB
080508         MOVE 'PDIDO_FLEXZ_AGPTO'    TO GFCT0M-NOME-TAB
080508         MOVE 'SELECT  '             TO GFCT0M-COMANDO-SQL
080508         MOVE '0130'                 TO GFCT0M-LOCAL
080508         MOVE  0010                  TO GFCTNA-COD-MSG-ERRO
080508         PERFORM 9000-MOVER-ERRO-SQL
080508         PERFORM 9999-FINALIZAR
080508     END-IF.
080508
080508     IF  SQLCODE                     EQUAL ZEROS AND
080508        (CSIT-SOLTC-FLEXZ            OF GFCTB0B2
080508                                     EQUAL 2 OR 8)
080508         MOVE 1                      TO GFCTNA-ERRO
080508         MOVE ZEROS                  TO GFCTNA-COD-SQL-ERRO
080508         MOVE 0921                   TO GFCTNA-COD-MSG-ERRO
080508         MOVE '0921'                 TO GFCTG2-COD-MSG
080508         PERFORM 9900-OBTER-DESC-MSG
080508         MOVE GFCTG3-DESC-MSG        TO GFCTNA-DESC-MSG-ERRO
080508         PERFORM 9999-FINALIZAR
080508     END-IF.
080508
080508     IF  SQLCODE                     EQUAL ZEROS AND
080508         CSIT-SOLTC-FLEXZ            OF GFCTB0B2
080508                                     EQUAL 1
080508         MOVE 1                      TO GFCTNA-ERRO
080508         MOVE ZEROS                  TO GFCTNA-COD-SQL-ERRO
080508         MOVE 0920                   TO GFCTNA-COD-MSG-ERRO
080508         MOVE '0920'                 TO GFCTG2-COD-MSG
080508         PERFORM 9900-OBTER-DESC-MSG
080508         MOVE GFCTG3-DESC-MSG        TO GFCTNA-DESC-MSG-ERRO
080508         PERFORM 9999-FINALIZAR
080508     END-IF.
080508
080508*----------------------------------------------------------------*
080508 2190-99-FIM.                    EXIT.
080508*----------------------------------------------------------------*

0410  ******************************************************************
0410  *ROTINA PARA CONSISTIR PEDIDO RAZAO                              *
0410  ******************************************************************
0410  *----------------------------------------------------------------*
0410   2195-CONSISTIR-PED-RAZAO        SECTION.
0410  *----------------------------------------------------------------*
0410
0410       PERFORM 2196-MAX-PED-RAZAO
0410
0410       EXEC SQL
0410         SELECT A.CSIT_SOLTC_FLEXZ,
0410                A.DFIM_FLEXZ_AGPTO
0410         INTO   :GFCTB0B2.CSIT-SOLTC-FLEXZ,
0410                :GFCTB0B2.DFIM-FLEXZ-AGPTO
0410         FROM   DB2PRD.PDIDO_FLEXZ_AGPTO A,
0410                DB2PRD.TPDIDO_FLEXZ_RZ   B
0410         WHERE  A.CAGPTO_CTA         = B.CAGPTO_CTA
0410           AND  A.CSERVC_TARIF       = B.CSERVC_TARIF
0410           AND  A.HIDTFD_FLEXZ_AGPTO = B.HIDTFD_FLEXZ_AGPTO
0410           AND  B.CAGPTO_CTA         = :GFCTB0M7.CAGPTO-CTA
0410           AND  B.CSERVC_TARIF       = :GFCTB0M7.CSERVC-TARIF
0410           AND  B.CGRP_CTBIL_ATUAL   = :GFCTB0M7.CGRP-CTBIL-ATUAL
0410           AND  B.CSGRP_CTBIL        = :GFCTB0M7.CSGRP-CTBIL
0410           AND  B.HIDTFD_FLEXZ_AGPTO = :GFCTB0M7.HIDTFD-FLEXZ-AGPTO
0410       END-EXEC.
0410
0410       IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
0410          (SQLWARN0                EQUAL 'W')
0410           INITIALIZE              GFCT0M-ERRO-SQL
0410           MOVE '0010'             TO GFCTG2-COD-MSG
0410           PERFORM 9900-OBTER-DESC-MSG
0410           MOVE ' - GFCTB0B2'      TO WRK-NOME-TAB
0410           MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
0410           MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
0410           MOVE '0140'             TO GFCT0M-LOCAL
0410           MOVE  0010              TO GFCTNA-COD-MSG-ERRO
0410           PERFORM 9000-MOVER-ERRO-SQL
0410           PERFORM 9999-FINALIZAR
0410       END-IF.
0410
0710XS     IF  SQLCODE              EQUAL ZEROS
0710XS         MOVE DFIM-FLEXZ-AGPTO OF GFCTB0B2
0710XS                                 TO WRK-DATA-DB2
0710XS         MOVE WRK-DIA-DB2        TO WRK-DIA-INV
0710XS         MOVE WRK-MES-DB2        TO WRK-MES-INV
0710XS         MOVE WRK-ANO-DB2        TO WRK-ANO-INV
0710XS         MOVE WRK-DATA-INV       TO WRK-DT-FIM-FLEX-INV
0710XS     END-IF.
0410
0410       IF  SQLCODE                 EQUAL ZEROS AND
0410          (CSIT-SOLTC-FLEXZ        OF GFCTB0B2
0410                                   EQUAL 2 OR 8) AND
0710XS         WRK-DT-FIM-FLEX-INV     NOT LESS WRK-DT-PROCM-INV
0410           MOVE 1                  TO GFCTNA-ERRO
0410           MOVE ZEROS              TO GFCTNA-COD-SQL-ERRO
0410           MOVE 0921               TO GFCTNA-COD-MSG-ERRO
0410           MOVE '0921'             TO GFCTG2-COD-MSG
0410           PERFORM 9900-OBTER-DESC-MSG
0410           MOVE GFCTG3-DESC-MSG    TO GFCTNA-DESC-MSG-ERRO
0410           PERFORM 9999-FINALIZAR
0410       END-IF.
0410
0410       IF  SQLCODE                 EQUAL ZEROS AND
0410           CSIT-SOLTC-FLEXZ        OF GFCTB0B2
0410                                   EQUAL 1     AND
0710XS         WRK-DT-FIM-FLEX-INV     NOT LESS WRK-DT-PROCM-INV
0410           MOVE 1                  TO GFCTNA-ERRO
0410           MOVE ZEROS              TO GFCTNA-COD-SQL-ERRO
0410           MOVE 0920               TO GFCTNA-COD-MSG-ERRO
0410           MOVE '0920'             TO GFCTG2-COD-MSG
0410           PERFORM 9900-OBTER-DESC-MSG
0410           MOVE GFCTG3-DESC-MSG    TO GFCTNA-DESC-MSG-ERRO
0410           PERFORM 9999-FINALIZAR
0410       END-IF.
0410
0410  *----------------------------------------------------------------*
0410   2195-99-FIM.                    EXIT.
0410  *----------------------------------------------------------------*

0410  ******************************************************************
0410  *ROTINA PARA OBTER MAIOR-TIMES-TAMP-                             *
0410  ******************************************************************
0410  *----------------------------------------------------------------*
0410   2196-MAX-PED-RAZAO              SECTION.
0410  *----------------------------------------------------------------*
0410
0410       MOVE GFCTN9-GRUPO-CTBIL     TO CGRP-CTBIL-ATUAL  OF GFCTB0M7
0410       MOVE GFCTN9-SUB-GRUPO-CTBIL TO CSGRP-CTBIL       OF GFCTB0M7
0410       MOVE GFCTN9-COD-TARIFA      TO CSERVC-TARIF      OF GFCTB0M7
0410       MOVE GFCTN9-AGRUPAMENTO     TO CAGPTO-CTA        OF GFCTB0M7
0410       MOVE DPROCM-ATUAL           OF GFCTB0A1
0410                                   TO DINIC-FLEXZ-AGPTO OF GFCTB0B2
0410                                      DFIM-FLEXZ-AGPTO  OF GFCTB0B2
0410
0410       EXEC SQL
0410         SELECT MAX(B.HIDTFD_FLEXZ_AGPTO)
0410         INTO   :GFCTB0M7.HIDTFD-FLEXZ-AGPTO
0410         FROM   DB2PRD.PDIDO_FLEXZ_AGPTO A,
0410                DB2PRD.TPDIDO_FLEXZ_RZ    B
0410         WHERE  A.CAGPTO_CTA         = B.CAGPTO_CTA
0410           AND  A.CSERVC_TARIF       = B.CSERVC_TARIF
0410           AND  A.HIDTFD_FLEXZ_AGPTO = B.HIDTFD_FLEXZ_AGPTO
0410           AND  B.CAGPTO_CTA         = :GFCTB0M7.CAGPTO-CTA
0410           AND  B.CSERVC_TARIF       = :GFCTB0M7.CSERVC-TARIF
0410           AND  B.CGRP_CTBIL_ATUAL   = :GFCTB0M7.CGRP-CTBIL-ATUAL
0410           AND  B.CSGRP_CTBIL        = :GFCTB0M7.CSGRP-CTBIL
0410       END-EXEC.
0410
0410       IF (SQLCODE                 NOT EQUAL ZEROS AND -305) OR
0410          (SQLWARN0                EQUAL 'W')
0410           INITIALIZE              GFCT0M-ERRO-SQL
0410           MOVE '0010'             TO GFCTG2-COD-MSG
0410           PERFORM 9900-OBTER-DESC-MSG
0410           MOVE ' - GFCTB0B2'      TO WRK-NOME-TAB
0410           MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
0410           MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
0410           MOVE '0150'             TO GFCT0M-LOCAL
0410           MOVE  0010              TO GFCTNA-COD-MSG-ERRO
0410           PERFORM 9000-MOVER-ERRO-SQL
0410           PERFORM 9999-FINALIZAR
0410       END-IF.
0410
0410       IF  SQLCODE                 EQUAL -305
0410XS         MOVE '0001-01-01-01.01.01.000001'
0410XS                                 TO HIDTFD-FLEXZ-AGPTO
0410XS                                                      OF GFCTB0M7
0410       END-IF.
0410
0410  *----------------------------------------------------------------*
0410   2196-99-FIM.                    EXIT.
0410  *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA CONSISTIR DATAS                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-CONSISTIR-DATAS            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTN9-INIC-VIG-FLEX   TO WRK-DATA-DB2.
           MOVE WRK-DIA-DB2            TO WRK-DIA-INV
                                          WRK-DATA(1:2).
           MOVE WRK-MES-DB2            TO WRK-MES-INV
                                          WRK-DATA(3:2).
           MOVE WRK-ANO-DB2            TO WRK-ANO-INV
                                          WRK-DATA(5:4).

           CALL 'POOL1470'             USING WRK-DATA.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 1                  TO GFCTNA-ERRO
               MOVE ZEROS              TO GFCTNA-COD-SQL-ERRO
               MOVE 0801               TO GFCTNA-COD-MSG-ERRO
               MOVE '0801'             TO GFCTG2-COD-MSG
               PERFORM 9900-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTNA-DESC-MSG-ERRO
               PERFORM 9999-FINALIZAR
           END-IF.

           MOVE WRK-DATA-INV           TO WRK-INIC-FLEX
080508                                    WRK-DATA-INIC-FLEX.

           MOVE GFCTN9-FIM-VIG-FLEX    TO WRK-DATA-DB2
           MOVE WRK-DIA-DB2            TO WRK-DIA-INV
                                          WRK-DATA(1:2).
           MOVE WRK-MES-DB2            TO WRK-MES-INV
                                          WRK-DATA(3:2).
           MOVE WRK-ANO-DB2            TO WRK-ANO-INV
                                          WRK-DATA(5:4).

           CALL 'POOL1470'             USING WRK-DATA.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 1                  TO GFCTNA-ERRO
               MOVE ZEROS              TO GFCTNA-COD-SQL-ERRO
               MOVE 0802               TO GFCTNA-COD-MSG-ERRO
               MOVE '0802'             TO GFCTG2-COD-MSG
               PERFORM 9900-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTNA-DESC-MSG-ERRO
               PERFORM 9999-FINALIZAR
           END-IF.

           MOVE WRK-DATA-INV           TO WRK-FIM-FLEX
080508                                    WRK-DATA-FIM-FLEX.

           PERFORM 2210-ACESSAR-GFCT5018.

           MOVE WRK-5018-DINIC-VGCIA-TARIF-P
                                       TO WRK-DATA-DB2.
           MOVE WRK-DIA-DB2            TO WRK-DIA-INV
           MOVE WRK-MES-DB2            TO WRK-MES-INV
           MOVE WRK-ANO-DB2            TO WRK-ANO-INV
           MOVE WRK-DATA-INV           TO WRK-INIC-TARIF.
           MOVE WRK-5018-DFIM-VGCIA-TARIF
                                       TO WRK-DATA-DB2.
           MOVE WRK-DIA-DB2            TO WRK-DIA-INV
           MOVE WRK-MES-DB2            TO WRK-MES-INV
           MOVE WRK-ANO-DB2            TO WRK-ANO-INV
           MOVE WRK-DATA-INV           TO WRK-FIM-TARIF.
           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO WRK-DATA-DB2.
           MOVE WRK-DIA-DB2            TO WRK-DIA-INV.
           MOVE WRK-MES-DB2            TO WRK-MES-INV.
           MOVE WRK-ANO-DB2            TO WRK-ANO-INV.
           MOVE WRK-DATA-INV           TO WRK-DATA-PROCS.

           IF  WRK-INIC-FLEX           LESS WRK-INIC-TARIF
               MOVE 1                  TO GFCTNA-ERRO
               MOVE ZEROS              TO GFCTNA-COD-SQL-ERRO
               MOVE 0807               TO GFCTNA-COD-MSG-ERRO
               MOVE '0807'             TO GFCTG2-COD-MSG
               PERFORM 9900-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTNA-DESC-MSG-ERRO
               PERFORM 9999-FINALIZAR
           END-IF.

           IF  WRK-FIM-FLEX            GREATER WRK-FIM-TARIF
               MOVE 1                  TO GFCTNA-ERRO
               MOVE ZEROS              TO GFCTNA-COD-SQL-ERRO
               MOVE 0808               TO GFCTNA-COD-MSG-ERRO
               MOVE '0808'             TO GFCTG2-COD-MSG
               PERFORM 9900-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTNA-DESC-MSG-ERRO
               PERFORM 9999-FINALIZAR
           END-IF.

           IF  WRK-FIM-FLEX            LESS WRK-INIC-FLEX
               MOVE 1                  TO GFCTNA-ERRO
               MOVE ZEROS              TO GFCTNA-COD-SQL-ERRO
               MOVE 0611               TO GFCTNA-COD-MSG-ERRO
               MOVE '0611'             TO GFCTG2-COD-MSG
               PERFORM 9900-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTNA-DESC-MSG-ERRO
               PERFORM 9999-FINALIZAR
           END-IF.

           IF  WRK-INIC-FLEX           NOT GREATER WRK-DATA-PROCS
               MOVE 1                  TO GFCTNA-ERRO
               MOVE ZEROS              TO GFCTNA-COD-SQL-ERRO
               MOVE 0610               TO GFCTNA-COD-MSG-ERRO
               MOVE '0610'             TO GFCTG2-COD-MSG
               PERFORM 9900-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTNA-DESC-MSG-ERRO
               PERFORM 9999-FINALIZAR
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
080508         MOVE 1                  TO GFCTNA-ERRO
080508         MOVE ZEROS              TO GFCTNA-COD-SQL-ERRO
080508         MOVE 1210               TO GFCTNA-COD-MSG-ERRO
080508         MOVE '1210'             TO GFCTG2-COD-MSG
080508         PERFORM 9900-OBTER-DESC-MSG
080508         MOVE GFCTG3-DESC-MSG    TO GFCTNA-DESC-MSG-ERRO
080508         PERFORM 9999-FINALIZAR
080508     END-IF.
080508
080508     IF  WRK-DATA-FIM-FLEX       GREATER WRK-DATA-FIM-PERMIS
080508         MOVE 1                  TO GFCTNA-ERRO
080508         MOVE ZEROS              TO GFCTNA-COD-SQL-ERRO
080508         MOVE 1211               TO GFCTNA-COD-MSG-ERRO
080508         MOVE '1211'             TO GFCTG2-COD-MSG
080508         PERFORM 9900-OBTER-DESC-MSG
080508         MOVE GFCTG3-DESC-MSG    TO GFCTNA-DESC-MSG-ERRO
080508         PERFORM 9999-FINALIZAR
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
           MOVE GFCTN9-COD-TARIFA      TO WRK-5018-CODIGO-TARIFA.

PROC       MOVE GFCTN9-INIC-VIG-FLEX
PROC                                   TO WRK-5018-DT-REFER.

           MOVE 'GFCT5018'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                             WRK-5018-AREA-ENTRADA
                                             WRK-5018-AREA-SAIDA
                                             WRK-5018-DADOS-RETORNO
                                             WRK-IO-PCB
                                             WRK-ALT-PCB.

           IF  WRK-5018-COD-RETORNO    NOT EQUAL ZEROS AND 02
               MOVE 9                  TO GFCTNA-ERRO
               MOVE WRK-MODULO         TO WRK-NOME-MOD
               MOVE WRK-MSG01          TO GFCT0M-TEXTO
                                          GFCTNA-DESC-MSG-ERRO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0408'         TO GFCT0M-PROGRAMA
               PERFORM 9999-FINALIZAR
           END-IF.

           IF  WRK-5018-COD-RETORNO    EQUAL 02
               MOVE 1                  TO GFCTNA-ERRO
               MOVE ZEROS              TO GFCTNA-COD-SQL-ERRO
               MOVE 1118               TO GFCTNA-COD-MSG-ERRO
               MOVE '1118'             TO GFCTG2-COD-MSG
               PERFORM 9900-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTNA-DESC-MSG-ERRO
               PERFORM 9999-FINALIZAR
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
080508     MOVE GFCTN9-AGRUPAMENTO     TO CAGPTO-CTA        OF GFCTB0J7.
080508     MOVE GFCTN9-COD-TARIFA      TO CSERVC-TARIF      OF GFCTB0J7.
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
080508         PERFORM 9900-OBTER-DESC-MSG
080508         STRING GFCTG3-DESC-MSG ' - GFCTB0J7'
080508             DELIMITED BY '  '   INTO GFCTNA-DESC-MSG-ERRO
080508         MOVE 'GFCT0408'         TO GFCT0M-PROGRAMA
080508         MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
080508         MOVE 'TPRMSS_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
080508         MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
080508         MOVE  SQLCODE           TO GFCT0M-SQLCODE
080508                                    GFCTNA-COD-SQL-ERRO
080508         MOVE '0160'             TO GFCT0M-LOCAL
080508         MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
080508         MOVE  9                 TO GFCTNA-ERRO
080508         MOVE  10                TO GFCTNA-COD-MSG-ERRO
080508         PERFORM 9999-FINALIZAR
080508     END-IF.
080508
080508     IF  SQLCODE                 EQUAL +100
080508         MOVE 1                  TO GFCTNA-ERRO
080508         MOVE ZEROS              TO GFCTNA-COD-SQL-ERRO
080508         MOVE 1216               TO GFCTNA-COD-MSG-ERRO
080508         MOVE '1216'             TO GFCTG2-COD-MSG
080508         PERFORM 9900-OBTER-DESC-MSG
080508         MOVE GFCTG3-DESC-MSG    TO GFCTNA-DESC-MSG-ERRO
080508         PERFORM 9999-FINALIZAR
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

      *--- PARA TARIFA PERCENTUAL NAO TEM VALOR
           IF  GFCTN9-IND-VALOR        EQUAL 2
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

           IF  GFCTN9-FRANQUIA         EQUAL ZEROS
               IF  GFCTN9-PERIODO      NOT EQUAL ZEROS
                   MOVE 1                  TO GFCTNA-ERRO
                   MOVE ZEROS              TO GFCTNA-COD-SQL-ERRO
                   MOVE 0612               TO GFCTNA-COD-MSG-ERRO
                   MOVE '0612'             TO GFCTG2-COD-MSG
                   PERFORM 9900-OBTER-DESC-MSG
                   MOVE  GFCTG3-DESC-MSG   TO GFCTNA-DESC-MSG-ERRO
                   PERFORM 9999-FINALIZAR
               END-IF
           ELSE
               IF GFCTN9-PERIODO           NOT EQUAL 1 AND 2 AND
                                           3 AND 4
                   MOVE 1                  TO GFCTNA-ERRO
                   MOVE ZEROS              TO GFCTNA-COD-SQL-ERRO
                   MOVE 0618               TO GFCTNA-COD-MSG-ERRO
                   MOVE '0618'             TO GFCTG2-COD-MSG
                   PERFORM 9900-OBTER-DESC-MSG
                   MOVE  GFCTG3-DESC-MSG   TO GFCTNA-DESC-MSG-ERRO
                   PERFORM 9999-FINALIZAR
               END-IF
           END-IF.

           IF  GFCTN9-IND-VALOR            EQUAL 1
               IF  GFCTN9-VR-NEGOCIADO     NOT EQUAL ZEROS OR
                   GFCTN9-PERC-DESCTO      NOT EQUAL ZEROS
                   MOVE 1                  TO GFCTNA-ERRO
                   MOVE ZEROS              TO GFCTNA-COD-SQL-ERRO
                   MOVE 0619               TO GFCTNA-COD-MSG-ERRO
                   MOVE '0619'             TO GFCTG2-COD-MSG
                   PERFORM 9900-OBTER-DESC-MSG
                   MOVE  GFCTG3-DESC-MSG   TO GFCTNA-DESC-MSG-ERRO
                   PERFORM 9999-FINALIZAR
               END-IF
           END-IF.

           IF  GFCTN9-IND-VALOR            EQUAL 2
               IF  GFCTN9-VR-NEGOCIADO     EQUAL ZEROS
                   MOVE 1                  TO GFCTNA-ERRO
                   MOVE ZEROS              TO GFCTNA-COD-SQL-ERRO
                   MOVE 0642               TO GFCTNA-COD-MSG-ERRO
                   MOVE '0642'             TO GFCTG2-COD-MSG
                   PERFORM 9900-OBTER-DESC-MSG
                   MOVE  GFCTG3-DESC-MSG   TO GFCTNA-DESC-MSG-ERRO
                   PERFORM 9999-FINALIZAR
               END-IF
               IF  GFCTN9-PERC-DESCTO      NOT EQUAL ZEROS
                   MOVE 1                  TO GFCTNA-ERRO
                   MOVE ZEROS              TO GFCTNA-COD-SQL-ERRO
                   MOVE 0614               TO GFCTNA-COD-MSG-ERRO
                   MOVE '0614'             TO GFCTG2-COD-MSG
                   PERFORM 9900-OBTER-DESC-MSG
                   MOVE  GFCTG3-DESC-MSG   TO GFCTNA-DESC-MSG-ERRO
                   PERFORM 9999-FINALIZAR
               END-IF
           END-IF.

           IF  GFCTN9-IND-VALOR            EQUAL 3
               IF  GFCTN9-VR-NEGOCIADO     NOT EQUAL ZEROS
                   MOVE 1                  TO GFCTNA-ERRO
                   MOVE ZEROS              TO GFCTNA-COD-SQL-ERRO
                   MOVE 0642               TO GFCTNA-COD-MSG-ERRO
                   MOVE '0642'             TO GFCTG2-COD-MSG
                   PERFORM 9900-OBTER-DESC-MSG
                   MOVE  GFCTG3-DESC-MSG   TO GFCTNA-DESC-MSG-ERRO
                   PERFORM 9999-FINALIZAR
               END-IF
               IF  GFCTN9-PERC-DESCTO      GREATER 100,0000 OR
                   GFCTN9-PERC-DESCTO      LESS 0,0001
                   MOVE 1                  TO GFCTNA-ERRO
                   MOVE ZEROS              TO GFCTNA-COD-SQL-ERRO
                   MOVE 0614               TO GFCTNA-COD-MSG-ERRO
                   MOVE '0614'             TO GFCTG2-COD-MSG
                   PERFORM 9900-OBTER-DESC-MSG
                   MOVE  GFCTG3-DESC-MSG   TO GFCTNA-DESC-MSG-ERRO
                   PERFORM 9999-FINALIZAR
               END-IF
           END-IF.

           IF  GFCTN9-VR-NEGOCIADO         NOT EQUAL ZEROS AND
               GFCTN9-PERC-DESCTO          NOT EQUAL ZEROS
               MOVE 1                      TO GFCTNA-ERRO
               MOVE ZEROS                  TO GFCTNA-COD-SQL-ERRO
               MOVE 0619                   TO GFCTNA-COD-MSG-ERRO
               MOVE '0619'                 TO GFCTG2-COD-MSG
               PERFORM 9900-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG       TO GFCTNA-DESC-MSG-ERRO
               PERFORM 9999-FINALIZAR
           END-IF.

           IF  GFCTN9-AGENDAMENTO          EQUAL 'S'
               IF  GFCTN9-DIAS-RETENCAO    NOT EQUAL ZEROS
                   MOVE 1                  TO GFCTNA-ERRO
                   MOVE ZEROS              TO GFCTNA-COD-SQL-ERRO
                   MOVE 0615               TO GFCTNA-COD-MSG-ERRO
                   MOVE '0615'             TO GFCTG2-COD-MSG
                   PERFORM 9900-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG    TO GFCTNA-DESC-MSG-ERRO
                   PERFORM 9999-FINALIZAR
               END-IF
           END-IF.

           IF  GFCTN9-AGENDAMENTO          EQUAL 'S'
               IF  GFCTN9-DIA-AGEND        LESS 4 OR
                   GFCTN9-DIA-AGEND        GREATER 31
                   MOVE 1                  TO GFCTNA-ERRO
                   MOVE ZEROS              TO GFCTNA-COD-SQL-ERRO
                   MOVE 0616               TO GFCTNA-COD-MSG-ERRO
                   MOVE '0616'             TO GFCTG2-COD-MSG
                   PERFORM 9900-OBTER-DESC-MSG
                   MOVE  GFCTG3-DESC-MSG   TO GFCTNA-DESC-MSG-ERRO
                   PERFORM 9999-FINALIZAR
               END-IF
           ELSE
               IF GFCTN9-DIA-AGEND         NOT EQUAL ZEROS
                   MOVE 1                  TO GFCTNA-ERRO
                   MOVE ZEROS              TO GFCTNA-COD-SQL-ERRO
                   MOVE 0616               TO GFCTNA-COD-MSG-ERRO
                   MOVE '0616'             TO GFCTG2-COD-MSG
                   PERFORM 9900-OBTER-DESC-MSG
                   MOVE  GFCTG3-DESC-MSG   TO GFCTNA-DESC-MSG-ERRO
                   PERFORM 9999-FINALIZAR
               END-IF
           END-IF.

           IF  GFCTN9-AGENDAMENTO          EQUAL 'S'
               IF  GFCTN9-FRANQUIA         EQUAL ZEROS AND
                   GFCTN9-VR-NEGOCIADO     EQUAL ZEROS AND
                   GFCTN9-PERC-DESCTO      EQUAL ZEROS AND
080508             GFCTN9-PERIODO-COBR     EQUAL ZEROS
                   MOVE 1                  TO GFCTNA-ERRO
                   MOVE ZEROS              TO GFCTNA-COD-SQL-ERRO
                   MOVE 0623               TO GFCTNA-COD-MSG-ERRO
                   MOVE '0623'             TO GFCTG2-COD-MSG
                   PERFORM 9900-OBTER-DESC-MSG
                   MOVE  GFCTG3-DESC-MSG   TO GFCTNA-DESC-MSG-ERRO
                   PERFORM 9999-FINALIZAR
               END-IF
           END-IF.

           IF  GFCTN9-FRANQUIA             EQUAL ZEROS AND
               GFCTN9-VR-NEGOCIADO         EQUAL ZEROS AND
               GFCTN9-PERC-DESCTO          EQUAL ZEROS AND
               GFCTN9-DIAS-RETENCAO        EQUAL ZEROS AND
080508         GFCTN9-PERIODO-COBR         EQUAL ZEROS
               MOVE 1                      TO GFCTNA-ERRO
               MOVE ZEROS                  TO GFCTNA-COD-SQL-ERRO
               MOVE 0806                   TO GFCTNA-COD-MSG-ERRO
               MOVE '0806'                 TO GFCTG2-COD-MSG
               PERFORM 9900-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG       TO GFCTNA-DESC-MSG-ERRO
               PERFORM 9999-FINALIZAR
           END-IF.

080508     IF (GFCTN9-PERIODO-COBR     NOT = 0 AND 1 AND 2 AND 3 AND 4)
080508             MOVE 1              TO GFCTNA-ERRO
080508             MOVE ZEROS          TO GFCTNA-COD-SQL-ERRO
080508             MOVE 1254           TO GFCTNA-COD-MSG-ERRO
080508             MOVE '1254'         TO GFCTG2-COD-MSG
080508             PERFORM 9900-OBTER-DESC-MSG
080508             MOVE GFCTG3-DESC-MSG    TO GFCTNA-DESC-MSG-ERRO
080508             PERFORM 9999-FINALIZAR
080508     END-IF.

080508     IF (GFCTN9-INIC-PER-COBR    NOT = 0 AND 1 AND 2 AND 3 AND
080508                                       4 AND 5 AND 6 AND 7)
080508             MOVE 1              TO GFCTNA-ERRO
080508             MOVE ZEROS          TO GFCTNA-COD-SQL-ERRO
080508             MOVE 1255           TO GFCTNA-COD-MSG-ERRO
080508             MOVE '1255'         TO GFCTG2-COD-MSG
080508             PERFORM 9900-OBTER-DESC-MSG
080508             MOVE GFCTG3-DESC-MSG    TO GFCTNA-DESC-MSG-ERRO
080508             PERFORM 9999-FINALIZAR
080508     END-IF.

080508     IF  GFCTN9-PERIODO-COBR         = 2
080508         IF (GFCTN9-INIC-PER-COBR      NOT = 1 AND 2 AND 3 AND
080508                                             4 AND 5 AND 6 AND 7)
080508             MOVE 1              TO GFCTNA-ERRO
080508             MOVE ZEROS          TO GFCTNA-COD-SQL-ERRO
080508             MOVE 1256           TO GFCTNA-COD-MSG-ERRO
080508             MOVE '1256'         TO GFCTG2-COD-MSG
080508             PERFORM 9900-OBTER-DESC-MSG
080508             MOVE GFCTG3-DESC-MSG    TO GFCTNA-DESC-MSG-ERRO
080508             PERFORM 9999-FINALIZAR
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
               PERFORM 9900-OBTER-DESC-MSG
               MOVE ' - GFCTB0A0'          TO WRK-NOME-TAB
               MOVE 'PARM_CTRL_GSTAO '     TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '             TO GFCT0M-COMANDO-SQL
               MOVE '0170'                 TO GFCT0M-LOCAL
               MOVE  0010                  TO GFCTNA-COD-MSG-ERRO
               PERFORM 9000-MOVER-ERRO-SQL
               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                     EQUAL +100
               MOVE 1                      TO GFCTNA-ERRO
               MOVE ZEROS                  TO GFCTNA-COD-SQL-ERRO
               MOVE 0752                   TO GFCTNA-COD-MSG-ERRO
               MOVE '0752'                 TO GFCTG2-COD-MSG
               PERFORM 9900-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG       TO GFCTNA-DESC-MSG-ERRO
               PERFORM 9999-FINALIZAR
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
               MOVE 1                  TO GFCTNA-ERRO
               MOVE ZEROS              TO GFCTNA-COD-SQL-ERRO
               MOVE 0621               TO GFCTNA-COD-MSG-ERRO
               MOVE '0621'             TO GFCTG2-COD-MSG
               PERFORM 9900-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNA-DESC-MSG-ERRO
               PERFORM 9999-FINALIZAR
           END-IF.

           IF  GFCTN9-DIAS-RETENCAO    GREATER QMAX-RTCAO
                                       OF GFCTB0A0
               MOVE 1                  TO GFCTNA-ERRO
               MOVE ZEROS              TO GFCTNA-COD-SQL-ERRO
               MOVE 0625               TO GFCTNA-COD-MSG-ERRO
               MOVE '0625'             TO GFCTG2-COD-MSG
               PERFORM 9900-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNA-DESC-MSG-ERRO
               PERFORM 9999-FINALIZAR
           END-IF.

           IF  GFCTN9-FRANQUIA         GREATER QMAX-FRANQ-TARIF
                                       OF GFCTB0A0
               MOVE 1                  TO GFCTNA-ERRO
               MOVE ZEROS              TO GFCTNA-COD-SQL-ERRO
               MOVE 0622               TO GFCTNA-COD-MSG-ERRO
               MOVE '0622'             TO GFCTG2-COD-MSG
               PERFORM 9900-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNA-DESC-MSG-ERRO
               PERFORM 9999-FINALIZAR
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
               MOVE 1                  TO GFCTNA-ERRO
               MOVE ZEROS              TO GFCTNA-COD-SQL-ERRO
               MOVE 0628               TO GFCTNA-COD-MSG-ERRO
               MOVE '0628'             TO GFCTG2-COD-MSG
               PERFORM 9900-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNA-DESC-MSG-ERRO
               PERFORM 9999-FINALIZAR
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
               GFCTN9-FRANQUIA             NOT EQUAL ZEROS
               MOVE 1                      TO GFCTNA-ERRO
               MOVE ZEROS                  TO GFCTNA-COD-SQL-ERRO
               MOVE 0613                   TO GFCTNA-COD-MSG-ERRO
               MOVE '0613'                 TO GFCTG2-COD-MSG
               PERFORM 9900-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG        TO GFCTNA-DESC-MSG-ERRO
               PERFORM 9999-FINALIZAR
           END-IF.

           PERFORM VARYING IND-1 FROM 1 BY 1     UNTIL
               IND-1                             GREATER 50 OR
               WRK-5018-DINIC-VGCIA-TARIF(IND-1) EQUAL SPACES
                 IF WRK-5018-PSERVC-PTADO(IND-1) GREATER ZEROS AND
                    GFCTN9-VR-NEGOCIADO          NOT EQUAL ZEROS
                    MOVE 1                       TO GFCTNA-ERRO
                    MOVE ZEROS                   TO GFCTNA-COD-SQL-ERRO
                    MOVE 0624                    TO GFCTNA-COD-MSG-ERRO
                    MOVE '0624'                  TO GFCTG2-COD-MSG
                    PERFORM 9900-OBTER-DESC-MSG
                    MOVE GFCTG3-DESC-MSG         TO GFCTNA-DESC-MSG-ERRO
                   PERFORM 9999-FINALIZAR
                 END-IF
                 IF WRK-5018-CMOEDA-BACEN(IND-1) GREATER ZEROS AND
                    GFCTN9-VR-NEGOCIADO          NOT EQUAL ZEROS
                    MOVE 1                       TO GFCTNA-ERRO
                    MOVE ZEROS                   TO GFCTNA-COD-SQL-ERRO
                    MOVE 0629                    TO GFCTNA-COD-MSG-ERRO
                    MOVE '0629'                  TO GFCTG2-COD-MSG
                    PERFORM 9900-OBTER-DESC-MSG
                    MOVE GFCTG3-DESC-MSG         TO GFCTNA-DESC-MSG-ERRO
                   PERFORM 9999-FINALIZAR
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


           MOVE 'O'                        TO WRK-5521-AMBIENTE
           MOVE 'GFCT0408'                 TO WRK-5521-CHAMADOR
           MOVE GFCTN9-COD-TARIFA          TO WRK-5521-COD-TARIFA
           MOVE 1                          TO WRK-5521-QTD-EVN-AGRUP
PROC       MOVE DPROCM-ATUAL OF GFCTB0A1   TO WRK-5521-DATA-EVN.
PROC       MOVE GFCTN9-INIC-VIG-FLEX       TO WRK-5521-DATA-DEBITO
           MOVE ZEROS                      TO WRK-5521-VALOR-EVN
                                              WRK-5521-TIPO-MOEDA-EVN
           MOVE SPACES                     TO WRK-5521-DATA-CONV-EVN
           MOVE 10                         TO WRK-5521-TIPO-MOVTO.

           MOVE  'GFCT5521'                TO WRK-MODULO.

           CALL WRK-MODULO USING
                                                 WRK-5521-AREA-ENTRADA
                                                 WRK-5521-AREA-SAIDA
                                                 GFCT0M-AREA-ERROS.

           IF  WRK-5521-COD-RETORNO        NOT EQUAL ZEROS AND
               WRK-5521-COD-MSG-GFCT       EQUAL ZEROS
               MOVE 9                      TO GFCTNA-ERRO
               MOVE WRK-MODULO             TO WRK-NOME-MOD
               MOVE WRK-MSG01              TO GFCT0M-TEXTO
                                              GFCTNA-DESC-MSG-ERRO
               MOVE 'APL'                  TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0408'             TO GFCT0M-PROGRAMA
               PERFORM 9999-FINALIZAR
           END-IF.

           IF  WRK-5521-COD-MSG-GFCT       NOT EQUAL ZEROS AND
               WRK-5521-COD-MSG-GFCT       NOT EQUAL 0476  AND
               WRK-5521-COD-MSG-GFCT       NOT EQUAL 0443
               MOVE 1                      TO GFCTNA-ERRO
               MOVE WRK-5521-COD-SQL-ERRO  TO GFCTNA-COD-SQL-ERRO
               MOVE WRK-5521-COD-MSG-GFCT  TO GFCTNA-COD-MSG-ERRO
               MOVE WRK-5521-DESC-MSG      TO GFCTNA-DESC-MSG-ERRO
               PERFORM 9999-FINALIZAR
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

           IF  WRK-5521-VLR-TOTAL-TARIFA NOT GREATER GFCTN9-VR-NEGOCIADO
               MOVE 1                  TO GFCTNA-ERRO
               MOVE ZEROS              TO GFCTNA-COD-SQL-ERRO
               MOVE 0805               TO GFCTNA-COD-MSG-ERRO
               MOVE '0805'             TO GFCTG2-COD-MSG
               PERFORM 9900-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTNA-DESC-MSG-ERRO
               PERFORM 9999-FINALIZAR
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
               SET :WRK-TIMESTAMP-AUX   = CURRENT TIMESTAMP
           END-EXEC.

           MOVE GFCTN9-AGRUPAMENTO     TO CAGPTO-CTA        OF GFCTB0B2.
           MOVE GFCTN9-COD-TARIFA      TO CSERVC-TARIF      OF GFCTB0B2.
           MOVE WRK-TIMESTAMP-AUX      TO HIDTFD-FLEXZ-AGPTO
                                                            OF GFCTB0B2.
           MOVE GFCTN9-FUNC-BDSCO      TO CFUNC-SOLTC-FLEXZ OF GFCTB0B2.
           MOVE ZEROS                  TO CFUNC-PAREC-FLEXZ OF GFCTB0B2
           MOVE GFCTN9-CTIPO-OPER-MOTVO
                                       TO CTPO-OPER-MOTVO   OF GFCTB0B2.
           MOVE GFCTN9-CMOTVO-JUSTF    TO CMOTVO-JUSTF      OF GFCTB0B2.
           MOVE 237                    TO CEMPR-ORIGE-FLEXZ OF GFCTB0B2.
           MOVE WRK-5505-COD-JUNCAO    TO CDEPDC-ORIGE-FLEXZ
                                                            OF GFCTB0B2.
           MOVE ZEROS                  TO CEMPR-PAREC-FLEXZ OF GFCTB0B2
                                          CDEPDC-PAREC-FLEXZ
                                                            OF GFCTB0B2
           MOVE GFCTN9-INIC-VIG-FLEX   TO DINIC-FLEXZ-AGPTO OF GFCTB0B2.
           MOVE GFCTN9-FIM-VIG-FLEX    TO DFIM-FLEXZ-AGPTO  OF GFCTB0B2.
           MOVE GFCTN9-FRANQUIA        TO QFRANQ-FLEXZ-AGPTO
                                                            OF GFCTB0B2.
           MOVE GFCTN9-PERIODO         TO CPER-FRANQ-AGPTO  OF GFCTB0B2.
           MOVE GFCTN9-VR-NEGOCIADO    TO VNEGOC-FLEXZ-AGPTO
                                                            OF GFCTB0B2.
           MOVE GFCTN9-PERC-DESCTO     TO PDESC-FLEXZ-AGPTO OF GFCTB0B2.
           MOVE GFCTN9-DIA-AGEND       TO DDIA-AGNDA        OF GFCTB0B2.
           MOVE GFCTN9-DIAS-RETENCAO   TO QDIA-FLEXZ-AGPTO  OF GFCTB0B2.
           MOVE WRK-TIMESTAMP-AUX      TO HSOLTC-FLEXZ      OF GFCTB0B2.
           MOVE GFCTN9-RJUSTF-MANUT-SERVC
                                       TO RJUSTF-SOLTC-FLEXZ-TEXT
                                                            OF GFCTB0B2.
           MOVE LENGTH                 OF GFCTN9-RJUSTF-MANUT-SERVC
                                       TO RJUSTF-SOLTC-FLEXZ-LEN
                                                            OF GFCTB0B2.
080508     MOVE GFCTN9-PERIODO-COBR    TO CPER-COBR-TARIF   OF GFCTB0B2.
080508     MOVE GFCTN9-INIC-PER-COBR   TO CINIC-PER-COBR    OF GFCTB0B2.
           MOVE '0001-01-01-01.01.01.000001'
                                       TO HPAREC-FLEXZ      OF GFCTB0B2
           MOVE SPACES                 TO RJUSTF-PAREC-FLEXZ-TEXT
                                                            OF GFCTB0B2.
           MOVE ZEROS                  TO RJUSTF-PAREC-FLEXZ-LEN
                                                            OF GFCTB0B2.
           MOVE 01                     TO CSIT-SOLTC-FLEXZ  OF GFCTB0B2.
           MOVE GFCTN9-TODAS-AG        TO CINDCD-AGPTO-TOT  OF GFCTB0B2.
           MOVE ZEROS                  TO CINIC-PER-FRANQ   OF GFCTB0B2
                                          CFUNC-EXCL-SIST   OF GFCTB0B2.

0410XS     IF  GFCTN9-CIDTFD-AGRP      EQUAL 'S'
0410XS         MOVE GFCTN9-COD-CJTO-SERVC
0410XS                                 TO CCJTO-SERVC       OF GFCTB0B2
0410XS     ELSE
0410XS         MOVE ZEROS              TO CCJTO-SERVC       OF GFCTB0B2
0410XS     END-IF.

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
070510             CCJTO_SERVC)
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
                   :GFCTB0B2.HSOLTC-FLEXZ,
                   :GFCTB0B2.RJUSTF-SOLTC-FLEXZ,
                   :GFCTB0B2.HPAREC-FLEXZ,
                   :GFCTB0B2.RJUSTF-PAREC-FLEXZ,
                   :GFCTB0B2.CSIT-SOLTC-FLEXZ,
                   :GFCTB0B2.CINDCD-AGPTO-TOT,
                   :GFCTB0B2.CINIC-PER-FRANQ,
                   :GFCTB0B2.CFUNC-EXCL-SIST,
080508             :GFCTB0B2.CPER-COBR-TARIF,
080508             :GFCTB0B2.CINIC-PER-COBR,
070510             :GFCTB0B2.CCJTO-SERVC)
           END-EXEC.

           IF (SQLCODE                     NOT EQUAL ZEROS) OR
              (SQLWARN0                    EQUAL 'W')
               INITIALIZE                  GFCT0M-ERRO-SQL
IN             MOVE 9                      TO GFCTNA-ERRO
               MOVE '0010'                 TO GFCTG2-COD-MSG
               PERFORM 9900-OBTER-DESC-MSG
               MOVE ' - GFCTB0B2'          TO WRK-NOME-TAB
               MOVE 'PDIDO_FLEXZ_AGPTO'    TO GFCT0M-NOME-TAB
               MOVE 'INSERT  '             TO GFCT0M-COMANDO-SQL
               MOVE '0180'                 TO GFCT0M-LOCAL
               MOVE  0010                  TO GFCTNA-COD-MSG-ERRO
IN             PERFORM 9000-MOVER-ERRO-SQL
               PERFORM 9999-FINALIZAR
           END-IF.

           EVALUATE GFCTN9-AGRUPAMENTO
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

               WHEN 20
                   PERFORM 2430-INCLUIR-PEDIDO-POSTAL

               WHEN 26
                   PERFORM 2475-INCLUIR-PEDIDO-RAZAO

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

           MOVE GFCTN9-AGRUPAMENTO    TO CAGPTO-CTA         OF GFCTB0B8.
           MOVE GFCTN9-COD-TARIFA     TO CSERVC-TARIF       OF GFCTB0B8.
           MOVE WRK-TIMESTAMP-AUX     TO HIDTFD-FLEXZ-AGPTO OF GFCTB0B8.
           MOVE GFCTN9-AGENCIA        TO CDEPDC             OF GFCTB0B8.
           MOVE GFCTN9-POSTO          TO CPOSTO-SERVC       OF GFCTB0B8.
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
               PERFORM 9900-OBTER-DESC-MSG
               MOVE ' - GFCTB0B8'          TO WRK-NOME-TAB
               MOVE 'PDIDO_FLEXZ_PAB '     TO GFCT0M-NOME-TAB
               MOVE 'INSERT  '             TO GFCT0M-COMANDO-SQL
               MOVE '0190'                 TO GFCT0M-LOCAL
               MOVE  0010                  TO GFCTNA-COD-MSG-ERRO
               PERFORM 9000-MOVER-ERRO-SQL
               PERFORM 9999-FINALIZAR
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

           MOVE GFCTN9-AGRUPAMENTO    TO CAGPTO-CTA         OF GFCTB0B1.
           MOVE GFCTN9-COD-TARIFA     TO CSERVC-TARIF       OF GFCTB0B1.
           MOVE WRK-TIMESTAMP-AUX     TO HIDTFD-FLEXZ-AGPTO OF GFCTB0B1.

           IF  GFCTN9-TODAS-AG        EQUAL 'S'
               MOVE ZEROS             TO CDEPDC             OF GFCTB0B1
           ELSE
               MOVE GFCTN9-AGENCIA    TO CDEPDC             OF GFCTB0B1
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
               PERFORM 9900-OBTER-DESC-MSG
               MOVE ' - GFCTB0B1'          TO WRK-NOME-TAB
               MOVE 'PDIDO_FLEXZ_AG  '     TO GFCT0M-NOME-TAB
               MOVE 'INSERT  '             TO GFCT0M-COMANDO-SQL
               MOVE '0200'                 TO GFCT0M-LOCAL
               MOVE  0010                  TO GFCTNA-COD-MSG-ERRO
               PERFORM 9000-MOVER-ERRO-SQL
               PERFORM 9999-FINALIZAR
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

           MOVE GFCTN9-AGRUPAMENTO    TO CAGPTO-CTA         OF GFCTB0B9.
           MOVE GFCTN9-COD-TARIFA     TO CSERVC-TARIF       OF GFCTB0B9.
           MOVE WRK-TIMESTAMP-AUX     TO HIDTFD-FLEXZ-AGPTO OF GFCTB0B9.
           MOVE GFCTN9-AGENCIA        TO CDEPDC             OF GFCTB0B9.
           MOVE GFCTN9-POSTO          TO CPOSTO-SERVC       OF GFCTB0B9.
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
               PERFORM 9900-OBTER-DESC-MSG
               MOVE ' - GFCTB0B9'          TO WRK-NOME-TAB
               MOVE 'PDIDO_FLEXZ_PSTAL'    TO GFCT0M-NOME-TAB
               MOVE 'INSERT  '             TO GFCT0M-COMANDO-SQL
               MOVE '0210'                 TO GFCT0M-LOCAL
               MOVE  0010                  TO GFCTNA-COD-MSG-ERRO
               PERFORM 9000-MOVER-ERRO-SQL
               PERFORM 9999-FINALIZAR
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

           MOVE GFCTN9-AGRUPAMENTO    TO CAGPTO-CTA         OF GFCTB0B3.
           MOVE GFCTN9-COD-TARIFA     TO CSERVC-TARIF       OF GFCTB0B3.
           MOVE WRK-TIMESTAMP-AUX     TO HIDTFD-FLEXZ-AGPTO OF GFCTB0B3.
           MOVE GFCTN9-CGC-CPF        TO CCGC-CPF           OF GFCTB0B3.
           MOVE GFCTN9-FILIAL         TO CFLIAL-CGC         OF GFCTB0B3.
           MOVE GFCTN9-CONTROLE       TO CCTRL-CPF-CGC      OF GFCTB0B3.

           EXEC SQL
             INSERT INTO DB2PRD.PDIDO_FLEXZ_CLI
                  (CAGPTO_CTA,
                   CSERVC_TARIF,
                   HIDTFD_FLEXZ_AGPTO,
                   CCGC_CPF,
                   CFLIAL_CGC,
                   CCTRL_CPF_CGC)
               VALUES
                  (:GFCTB0B3.CAGPTO-CTA,
                   :GFCTB0B3.CSERVC-TARIF,
                   :GFCTB0B3.HIDTFD-FLEXZ-AGPTO,
                   :GFCTB0B3.CCGC-CPF,
                   :GFCTB0B3.CFLIAL-CGC,
                   :GFCTB0B3.CCTRL-CPF-CGC)
           END-EXEC.

           IF (SQLCODE                     NOT EQUAL ZEROS) OR
              (SQLWARN0                    EQUAL 'W'      )
               INITIALIZE                  GFCT0M-ERRO-SQL
               MOVE '0010'                 TO GFCTG2-COD-MSG
               PERFORM 9900-OBTER-DESC-MSG
               MOVE ' - GFCTB0B3'          TO WRK-NOME-TAB
               MOVE 'PDIDO_FLEXZ_CLI '     TO GFCT0M-NOME-TAB
               MOVE 'INSERT  '             TO GFCT0M-COMANDO-SQL
               MOVE '0220'                 TO GFCT0M-LOCAL
               MOVE  0010                  TO GFCTNA-COD-MSG-ERRO
               PERFORM 9000-MOVER-ERRO-SQL
               PERFORM 9999-FINALIZAR
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

           MOVE GFCTN9-AGRUPAMENTO    TO CAGPTO-CTA         OF GFCTB0C0.
           MOVE GFCTN9-COD-TARIFA     TO CSERVC-TARIF       OF GFCTB0C0.
           MOVE WRK-TIMESTAMP-AUX     TO HIDTFD-FLEXZ-AGPTO OF GFCTB0C0.
           MOVE GFCTN9-SEGMENTO       TO CSGMTO-GSTAO-TARIF OF GFCTB0C0.

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
               PERFORM 9900-OBTER-DESC-MSG
               MOVE ' - GFCTB0C0'          TO WRK-NOME-TAB
               MOVE 'PDIDO_FLEXZ_SGMTO'    TO GFCT0M-NOME-TAB
               MOVE 'INSERT  '             TO GFCT0M-COMANDO-SQL
               MOVE '0230'                 TO GFCT0M-LOCAL
               MOVE  0010                  TO GFCTNA-COD-MSG-ERRO
               PERFORM 9000-MOVER-ERRO-SQL
               PERFORM 9999-FINALIZAR
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

           MOVE GFCTN9-AGRUPAMENTO    TO CAGPTO-CTA         OF GFCTB0B7.
           MOVE GFCTN9-COD-TARIFA     TO CSERVC-TARIF       OF GFCTB0B7.
           MOVE WRK-TIMESTAMP-AUX     TO HIDTFD-FLEXZ-AGPTO OF GFCTB0B7.
           MOVE GFCTN9-MUNIC          TO CMUN-IBGE          OF GFCTB0B7.

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
               PERFORM 9900-OBTER-DESC-MSG
               MOVE ' - GFCTB0B7'          TO WRK-NOME-TAB
               MOVE 'PDIDO_FLEXZ_MUN '     TO GFCT0M-NOME-TAB
               MOVE 'INSERT  '             TO GFCT0M-COMANDO-SQL
               MOVE '0240'                 TO GFCT0M-LOCAL
               MOVE  0010                  TO GFCTNA-COD-MSG-ERRO
               PERFORM 9000-MOVER-ERRO-SQL
               PERFORM 9999-FINALIZAR
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

           MOVE GFCTN9-AGRUPAMENTO    TO CAGPTO-CTA         OF GFCTB0C1.
           MOVE GFCTN9-COD-TARIFA     TO CSERVC-TARIF       OF GFCTB0C1.
           MOVE WRK-TIMESTAMP-AUX     TO HIDTFD-FLEXZ-AGPTO OF GFCTB0C1.
           MOVE GFCTN9-UF             TO CSGL-UF            OF GFCTB0C1.

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
               PERFORM 9900-OBTER-DESC-MSG
               MOVE ' - GFCTB0C1'          TO WRK-NOME-TAB
               MOVE 'PDIDO_FLEXZ_UF  '     TO GFCT0M-NOME-TAB
               MOVE 'INSERT  '             TO GFCT0M-COMANDO-SQL
               MOVE '0250'                 TO GFCT0M-LOCAL
               MOVE  0010                  TO GFCTNA-COD-MSG-ERRO
               PERFORM 9000-MOVER-ERRO-SQL
               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2470-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA INCLUIR PEDIDO RAZAO                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2475-INCLUIR-PEDIDO-RAZAO       SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTN9-AGRUPAMENTO     TO CAGPTO-CTA        OF GFCTB0M7.
           MOVE GFCTN9-COD-TARIFA      TO CSERVC-TARIF      OF GFCTB0M7.
           MOVE WRK-TIMESTAMP-AUX     TO HIDTFD-FLEXZ-AGPTO OF GFCTB0M7.
           MOVE GFCTN9-GRUPO-CTBIL     TO CGRP-CTBIL-ATUAL  OF GFCTB0M7.
           MOVE GFCTN9-SUB-GRUPO-CTBIL TO CSGRP-CTBIL       OF GFCTB0M7.
           MOVE 237                    TO CEMPR             OF GFCTB0M7.

           EXEC SQL
             INSERT INTO DB2PRD.TPDIDO_FLEXZ_RZ
                  (CAGPTO_CTA,
                   CSERVC_TARIF,
                   HIDTFD_FLEXZ_AGPTO,
                   CEMPR,
                   CGRP_CTBIL_ATUAL,
                   CSGRP_CTBIL     )
               VALUES
                  (:GFCTB0M7.CAGPTO-CTA,
                   :GFCTB0M7.CSERVC-TARIF,
                   :GFCTB0M7.HIDTFD-FLEXZ-AGPTO,
                   :GFCTB0M7.CEMPR,
                   :GFCTB0M7.CGRP-CTBIL-ATUAL,
                   :GFCTB0M7.CSGRP-CTBIL     )
           END-EXEC.

           IF (SQLCODE                     NOT EQUAL ZEROS) OR
              (SQLWARN0                    EQUAL 'W'      )
               INITIALIZE                  GFCT0M-ERRO-SQL
               MOVE '0010'                 TO GFCTG2-COD-MSG
               PERFORM 9900-OBTER-DESC-MSG
               MOVE ' - GFCTB0MZ'          TO WRK-NOME-TAB
               MOVE 'TPDIDO_FLEXZ_RZ   '   TO GFCT0M-NOME-TAB
               MOVE 'INSERT  '             TO GFCT0M-COMANDO-SQL
               MOVE '0210'                 TO GFCT0M-LOCAL
               MOVE  0010                  TO GFCTNA-COD-MSG-ERRO
               PERFORM 9000-MOVER-ERRO-SQL
               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2475-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
0410XS
0410XS******************************************************************
0410XS* PROCESSAMENTO POR AGRUPAMENTO POR LOTE DE CONJUNTO DE SERVICO  *
0410XS******************************************************************
0410XS*----------------------------------------------------------------*
0410XS 3000-PROCESSAR-POR-CONJ-SERV    SECTION.
0410XS*----------------------------------------------------------------*
0410XS
0410XS     PERFORM 3010-CONSISTIR-DATA-VGCIA-FLEX.
0410XS
0410XS     PERFORM 3020-CONSISTIR-PARMS-DIGITADO.
0410XS
0410XS     PERFORM 3030-ACESSAR-PARM-GESTAO.
0410XS
0410XS     PERFORM 3040-CONSISTIR-PARM-GESTAO.
0410XS
0410XS     MOVE GFCTN9-COD-CJTO-SERVC  TO CCJTO-SERVC OF GFCTB0H2.
0410XS
0410XS     MOVE ZEROS                  TO WRK-ACM-SOLICITACAO.
0410XS
0410XS     MOVE 'N'                    TO WRK-FIM-CURSOR.
0410XS
0410XS     PERFORM 3700-ABRIR-CSR01.
0410XS
0410XS     PERFORM 3800-LER-CSR01.
0410XS
0410XS     PERFORM 3100-PROCESSAR-CURSOR
0410XS       UNTIL WRK-FIM-CURSOR      EQUAL 'S'.
0410XS
0410XS     PERFORM 3900-FECHAR-CSR01.
0410XS
0410XS     IF  WRK-ACM-SOLICITACAO     EQUAL ZEROS
0410XS         MOVE  ZEROS             TO GFCTNA-COD-SQL-ERRO
0410XS         MOVE  1792              TO GFCTNA-COD-MSG-ERRO
0410XS
0410XS         MOVE '1792'             TO GFCTG2-COD-MSG
0410XS
0410XS         PERFORM 9900-OBTER-DESC-MSG
0410XS
0410XS         MOVE GFCTG3-DESC-MSG    TO GFCTNA-DESC-MSG-ERRO
0410XS     ELSE
0410XS         MOVE  ZEROS             TO GFCTNA-COD-SQL-ERRO
0410XS         MOVE  1791              TO GFCTNA-COD-MSG-ERRO
0410XS
0410XS         MOVE '1791'             TO GFCTG2-COD-MSG
0410XS
0410XS         PERFORM 9900-OBTER-DESC-MSG
0410XS
0410XS         MOVE GFCTG3-DESC-MSG    TO GFCTNA-DESC-MSG-ERRO
0410XS     END-IF.
0410XS
0410XS*----------------------------------------------------------------*
0410XS 3000-99-FIM.                    EXIT.
0410XS*----------------------------------------------------------------*
0410XS
0410XS******************************************************************
0410XS*ROTINA PARA CONSISTIR DATA VIGENCIA FLEXIBILIZACAO              *
0410XS******************************************************************
0410XS*----------------------------------------------------------------*
0410XS 3010-CONSISTIR-DATA-VGCIA-FLEX  SECTION.
0410XS*----------------------------------------------------------------*
0410XS
0410XS     MOVE GFCTN9-INIC-VIG-FLEX   TO WRK-DATA-DB2.
0410XS     MOVE WRK-DIA-DB2            TO WRK-DIA-NOR
0410XS                                    WRK-DIA-INV.
0410XS     MOVE WRK-MES-DB2            TO WRK-MES-NOR
0410XS                                    WRK-MES-INV.
0410XS     MOVE WRK-ANO-DB2            TO WRK-ANO-NOR
0410XS                                    WRK-ANO-INV.
0410XS
0410XS     MOVE WRK-DATA-NOR           TO WRK-DATA.
0410XS
0410XS     CALL 'POOL1470'             USING WRK-DATA.
0410XS
0410XS     IF  RETURN-CODE             NOT EQUAL ZEROS
0410XS         MOVE  1                 TO GFCTNA-ERRO
0410XS
0410XS         MOVE  ZEROS             TO GFCTNA-COD-SQL-ERRO
0410XS         MOVE  0801              TO GFCTNA-COD-MSG-ERRO
0410XS
0410XS         MOVE '0801'             TO GFCTG2-COD-MSG
0410XS
0410XS         PERFORM 9900-OBTER-DESC-MSG
0410XS
0410XS         MOVE  GFCTG3-DESC-MSG   TO GFCTNA-DESC-MSG-ERRO
0410XS
0410XS         PERFORM 9999-FINALIZAR
0410XS     END-IF.
0410XS
0410XS     MOVE WRK-DATA-INV           TO WRK-DT-INIC-FLEX-INV.
0410XS
0410XS     MOVE GFCTN9-FIM-VIG-FLEX    TO WRK-DATA-DB2
0410XS     MOVE WRK-DIA-DB2            TO WRK-DIA-NOR
0410XS                                    WRK-DIA-INV.
0410XS     MOVE WRK-MES-DB2            TO WRK-MES-NOR
0410XS                                    WRK-MES-INV.
0410XS     MOVE WRK-ANO-DB2            TO WRK-ANO-NOR
0410XS                                    WRK-ANO-INV.
0410XS
0410XS     CALL 'POOL1470'             USING WRK-DATA.
0410XS
0410XS     IF  RETURN-CODE             NOT EQUAL ZEROS
0410XS         MOVE  1                 TO GFCTNA-ERRO
0410XS
0410XS         MOVE  ZEROS             TO GFCTNA-COD-SQL-ERRO
0410XS         MOVE  0802              TO GFCTNA-COD-MSG-ERRO
0410XS
0410XS         MOVE '0802'             TO GFCTG2-COD-MSG
0410XS
0410XS         PERFORM 9900-OBTER-DESC-MSG
0410XS
0410XS         MOVE  GFCTG3-DESC-MSG   TO GFCTNA-DESC-MSG-ERRO
0410XS
0410XS         PERFORM 9999-FINALIZAR
0410XS     END-IF.
0410XS
0410XS     MOVE WRK-DATA-INV           TO WRK-DT-FIM-FLEX-INV.
0410XS
0410XS     MOVE DPROCM-ATUAL OF GFCTB0A1
0410XS                                 TO WRK-DATA-DB2.
0410XS     MOVE WRK-DIA-DB2            TO WRK-DIA-INV.
0410XS     MOVE WRK-MES-DB2            TO WRK-MES-INV.
0410XS     MOVE WRK-ANO-DB2            TO WRK-ANO-INV.
0410XS     MOVE WRK-DATA-INV           TO WRK-DT-PROCM-INV.
0410XS
0410XS     IF  WRK-DT-INIC-FLEX-INV    NOT GREATER WRK-DT-PROCM-INV
0410XS         MOVE 1                  TO GFCTNA-ERRO
0410XS
0410XS         MOVE  ZEROS             TO GFCTNA-COD-SQL-ERRO
0410XS         MOVE  0610              TO GFCTNA-COD-MSG-ERRO
0410XS
0410XS         MOVE '0610'             TO GFCTG2-COD-MSG
0410XS
0410XS         PERFORM 9900-OBTER-DESC-MSG
0410XS
0410XS         MOVE  GFCTG3-DESC-MSG   TO GFCTNA-DESC-MSG-ERRO
0410XS
0410XS         PERFORM 9999-FINALIZAR
0410XS     END-IF.
0410XS
0410XS     IF  WRK-DT-FIM-FLEX-INV     LESS WRK-DT-INIC-FLEX-INV
0410XS         MOVE 1                  TO GFCTNA-ERRO
0410XS
0410XS         MOVE  ZEROS             TO GFCTNA-COD-SQL-ERRO
0410XS         MOVE  0611              TO GFCTNA-COD-MSG-ERRO
0410XS
0410XS         MOVE '0611'             TO GFCTG2-COD-MSG
0410XS
0410XS         PERFORM 9900-OBTER-DESC-MSG
0410XS
0410XS         MOVE  GFCTG3-DESC-MSG   TO GFCTNA-DESC-MSG-ERRO
0410XS
0410XS         PERFORM 9999-FINALIZAR
0410XS     END-IF.
0410XS
0410XS*----------------------------------------------------------------*
0410XS 3010-99-FIM.                    EXIT.
0410XS*----------------------------------------------------------------*
0410XS
0410XS******************************************************************
0410XS*ROTINA PARA CONSISTIR CAMPOS DIGITADOS                          *
0410XS******************************************************************
0410XS*----------------------------------------------------------------*
0410XS 3020-CONSISTIR-PARMS-DIGITADO   SECTION.
0410XS*----------------------------------------------------------------*
0410XS
0410XS     IF  GFCTN9-FRANQUIA         EQUAL ZEROS
0410XS         IF  GFCTN9-PERIODO      NOT EQUAL ZEROS
0410XS             MOVE  1             TO GFCTNA-ERRO
0410XS
0410XS             MOVE  ZEROS         TO GFCTNA-COD-SQL-ERRO
0410XS             MOVE  0612          TO GFCTNA-COD-MSG-ERRO
0410XS
0410XS             MOVE '0612'         TO GFCTG2-COD-MSG
0410XS
0410XS             PERFORM 9900-OBTER-DESC-MSG
0410XS
0410XS             MOVE  GFCTG3-DESC-MSG
0410XS                                 TO GFCTNA-DESC-MSG-ERRO
0410XS
0410XS             PERFORM 9999-FINALIZAR
0410XS         END-IF
0410XS     ELSE
0410XS         IF  GFCTN9-PERIODO      NOT EQUAL 1 AND
0410XS             GFCTN9-PERIODO      NOT EQUAL 2 AND
0410XS             GFCTN9-PERIODO      NOT EQUAL 3 AND
0410XS             GFCTN9-PERIODO      NOT EQUAL 4
0410XS             MOVE  1             TO GFCTNA-ERRO
0410XS
0410XS             MOVE  ZEROS         TO GFCTNA-COD-SQL-ERRO
0410XS             MOVE  0618          TO GFCTNA-COD-MSG-ERRO
0410XS
0410XS             MOVE '0618'         TO GFCTG2-COD-MSG
0410XS
0410XS             PERFORM 9900-OBTER-DESC-MSG
0410XS
0410XS             MOVE  GFCTG3-DESC-MSG
0410XS                                 TO GFCTNA-DESC-MSG-ERRO
0410XS
0410XS             PERFORM 9999-FINALIZAR
0410XS         END-IF
0410XS     END-IF.
0410XS
0410XS     IF  GFCTN9-IND-VALOR        EQUAL 1
0410XS         IF  GFCTN9-VR-NEGOCIADO NOT EQUAL ZEROS OR
0410XS             GFCTN9-PERC-DESCTO  NOT EQUAL ZEROS
0410XS             MOVE  1             TO GFCTNA-ERRO
0410XS
0410XS             MOVE  ZEROS         TO GFCTNA-COD-SQL-ERRO
0410XS             MOVE  0619          TO GFCTNA-COD-MSG-ERRO
0410XS
0410XS             MOVE '0619'         TO GFCTG2-COD-MSG
0410XS
0410XS             PERFORM 9900-OBTER-DESC-MSG
0410XS
0410XS             MOVE  GFCTG3-DESC-MSG
0410XS                                 TO GFCTNA-DESC-MSG-ERRO
0410XS
0410XS             PERFORM 9999-FINALIZAR
0410XS         END-IF
0410XS     ELSE
0410XS         IF  GFCTN9-IND-VALOR    EQUAL 2
0410XS             IF  GFCTN9-VR-NEGOCIADO
0410XS                                 EQUAL ZEROS
0410XS                 MOVE  1         TO GFCTNA-ERRO
0410XS
0410XS                 MOVE  ZEROS     TO GFCTNA-COD-SQL-ERRO
0410XS                 MOVE  0642      TO GFCTNA-COD-MSG-ERRO
0410XS
0410XS                 MOVE '0642'     TO GFCTG2-COD-MSG
0410XS
0410XS                 PERFORM 9900-OBTER-DESC-MSG
0410XS
0410XS                 MOVE  GFCTG3-DESC-MSG
0410XS                                 TO GFCTNA-DESC-MSG-ERRO
0410XS
0410XS                 PERFORM 9999-FINALIZAR
0410XS             END-IF
0410XS
0410XS             IF  GFCTN9-PERC-DESCTO
0410XS                                 NOT EQUAL ZEROS
0410XS                 MOVE  1         TO GFCTNA-ERRO
0410XS
0410XS                 MOVE  ZEROS     TO GFCTNA-COD-SQL-ERRO
0410XS                 MOVE  0614      TO GFCTNA-COD-MSG-ERRO
0410XS
0410XS                 MOVE '0614'     TO GFCTG2-COD-MSG
0410XS
0410XS                 PERFORM 9900-OBTER-DESC-MSG
0410XS
0410XS                 MOVE  GFCTG3-DESC-MSG
0410XS                                 TO GFCTNA-DESC-MSG-ERRO
0410XS
0410XS                 PERFORM 9999-FINALIZAR
0410XS             END-IF
0410XS         ELSE
0410XS             IF  GFCTN9-IND-VALOR
0410XS                                 EQUAL 3
0410XS                 IF  GFCTN9-VR-NEGOCIADO
0410XS                                 NOT EQUAL ZEROS
0410XS                     MOVE  1     TO GFCTNA-ERRO
0410XS
0410XS                     MOVE  ZEROS TO GFCTNA-COD-SQL-ERRO
0410XS                     MOVE  0642  TO GFCTNA-COD-MSG-ERRO
0410XS                     MOVE '0642' TO GFCTG2-COD-MSG
0410XS
0410XS                     PERFORM 9900-OBTER-DESC-MSG
0410XS
0410XS                     MOVE  GFCTG3-DESC-MSG
0410XS                                 TO GFCTNA-DESC-MSG-ERRO
0410XS
0410XS                     PERFORM 9999-FINALIZAR
0410XS                 END-IF
0410XS
0410XS                 IF  GFCTN9-PERC-DESCTO
0410XS                                 GREATER 100,0000 OR
0410XS                     GFCTN9-PERC-DESCTO
0410XS                                 LESS 0,0001
0410XS                     MOVE  1     TO GFCTNA-ERRO
0410XS
0410XS                     MOVE  ZEROS TO GFCTNA-COD-SQL-ERRO
0410XS                     MOVE  0614  TO GFCTNA-COD-MSG-ERRO
0410XS
0410XS                     MOVE '0614' TO GFCTG2-COD-MSG
0410XS
0410XS                     PERFORM 9900-OBTER-DESC-MSG
0410XS
0410XS                     MOVE  GFCTG3-DESC-MSG
0410XS                                 TO GFCTNA-DESC-MSG-ERRO
0410XS
0410XS                     PERFORM 9999-FINALIZAR
0410XS                 END-IF
0410XS             END-IF
0410XS         END-IF
0410XS     END-IF.
0410XS
0410XS     IF  GFCTN9-VR-NEGOCIADO     NOT EQUAL ZEROS AND
0410XS         GFCTN9-PERC-DESCTO      NOT EQUAL ZEROS
0410XS         MOVE  1                 TO GFCTNA-ERRO
0410XS
0410XS         MOVE  ZEROS             TO GFCTNA-COD-SQL-ERRO
0410XS         MOVE  0619              TO GFCTNA-COD-MSG-ERRO
0410XS
0410XS         MOVE '0619'             TO GFCTG2-COD-MSG
0410XS
0410XS         PERFORM 9900-OBTER-DESC-MSG
0410XS
0410XS         MOVE  GFCTG3-DESC-MSG   TO GFCTNA-DESC-MSG-ERRO
0410XS
0410XS         PERFORM 9999-FINALIZAR
0410XS     END-IF.
0410XS
0410XS     IF  GFCTN9-AGENDAMENTO      EQUAL 'S'
0410XS         IF  GFCTN9-DIAS-RETENCAO
0410XS                                 NOT EQUAL ZEROS
0410XS             MOVE  1             TO GFCTNA-ERRO
0410XS
0410XS             MOVE  ZEROS         TO GFCTNA-COD-SQL-ERRO
0410XS             MOVE  0615          TO GFCTNA-COD-MSG-ERRO
0410XS
0410XS             MOVE '0615'         TO GFCTG2-COD-MSG
0410XS
0410XS             PERFORM 9900-OBTER-DESC-MSG
0410XS
0410XS             MOVE  GFCTG3-DESC-MSG
0410XS                                 TO GFCTNA-DESC-MSG-ERRO
0410XS
0410XS             PERFORM 9999-FINALIZAR
0410XS         END-IF
0410XS
0410XS         IF  GFCTN9-DIA-AGEND    LESS 4 OR
0410XS             GFCTN9-DIA-AGEND    GREATER 31
0410XS             MOVE  1             TO GFCTNA-ERRO
0410XS
0410XS             MOVE  ZEROS         TO GFCTNA-COD-SQL-ERRO
0410XS             MOVE  0616          TO GFCTNA-COD-MSG-ERRO
0410XS
0410XS             MOVE '0616'         TO GFCTG2-COD-MSG
0410XS
0410XS             PERFORM 9900-OBTER-DESC-MSG
0410XS
0410XS             MOVE  GFCTG3-DESC-MSG
0410XS                                 TO GFCTNA-DESC-MSG-ERRO
0410XS
0410XS             PERFORM 9999-FINALIZAR
0410XS         END-IF
0410XS
0410XS         IF  GFCTN9-FRANQUIA     EQUAL ZEROS AND
0410XS             GFCTN9-VR-NEGOCIADO EQUAL ZEROS AND
0410XS             GFCTN9-PERC-DESCTO  EQUAL ZEROS AND
0410XS             GFCTN9-PERIODO-COBR EQUAL ZEROS
0410XS             MOVE  1             TO GFCTNA-ERRO
0410XS
0410XS             MOVE  ZEROS         TO GFCTNA-COD-SQL-ERRO
0410XS             MOVE  0623          TO GFCTNA-COD-MSG-ERRO
0410XS
0410XS             MOVE '0623'         TO GFCTG2-COD-MSG
0410XS
0410XS             PERFORM 9900-OBTER-DESC-MSG
0410XS
0410XS             MOVE  GFCTG3-DESC-MSG
0410XS                                 TO GFCTNA-DESC-MSG-ERRO
0410XS
0410XS             PERFORM 9999-FINALIZAR
0410XS         END-IF
0410XS     ELSE
0410XS         IF  GFCTN9-DIA-AGEND    NOT EQUAL ZEROS
0410XS             MOVE  1             TO GFCTNA-ERRO
0410XS
0410XS             MOVE  ZEROS         TO GFCTNA-COD-SQL-ERRO
0410XS             MOVE  0616          TO GFCTNA-COD-MSG-ERRO
0410XS
0410XS             MOVE '0616'         TO GFCTG2-COD-MSG
0410XS
0410XS             PERFORM 9900-OBTER-DESC-MSG
0410XS
0410XS             MOVE  GFCTG3-DESC-MSG
0410XS                                 TO GFCTNA-DESC-MSG-ERRO
0410XS
0410XS             PERFORM 9999-FINALIZAR
0410XS         END-IF
0410XS     END-IF.
0410XS
0410XS     IF  GFCTN9-FRANQUIA         EQUAL ZEROS AND
0410XS         GFCTN9-VR-NEGOCIADO     EQUAL ZEROS AND
0410XS         GFCTN9-PERC-DESCTO      EQUAL ZEROS AND
0410XS         GFCTN9-DIAS-RETENCAO    EQUAL ZEROS AND
0410XS         GFCTN9-PERIODO-COBR     EQUAL ZEROS
0410XS         MOVE  1                 TO GFCTNA-ERRO
0410XS
0410XS         MOVE  ZEROS             TO GFCTNA-COD-SQL-ERRO
0410XS         MOVE  0806              TO GFCTNA-COD-MSG-ERRO
0410XS
0410XS         MOVE '0806'             TO GFCTG2-COD-MSG
0410XS
0410XS         PERFORM 9900-OBTER-DESC-MSG
0410XS
0410XS         MOVE  GFCTG3-DESC-MSG   TO GFCTNA-DESC-MSG-ERRO
0410XS
0410XS         PERFORM 9999-FINALIZAR
0410XS     END-IF.
0410XS
0410XS     IF  GFCTN9-PERIODO-COBR     NOT EQUAL 0 AND
0410XS         GFCTN9-PERIODO-COBR     NOT EQUAL 1 AND
0410XS         GFCTN9-PERIODO-COBR     NOT EQUAL 2 AND
0410XS         GFCTN9-PERIODO-COBR     NOT EQUAL 3 AND
0410XS         GFCTN9-PERIODO-COBR     NOT EQUAL 4
0410XS         MOVE  1                 TO GFCTNA-ERRO
0410XS
0410XS         MOVE  ZEROS             TO GFCTNA-COD-SQL-ERRO
0410XS         MOVE  1254              TO GFCTNA-COD-MSG-ERRO
0410XS
0410XS         MOVE '1254'             TO GFCTG2-COD-MSG
0410XS
0410XS         PERFORM 9900-OBTER-DESC-MSG
0410XS
0410XS         MOVE  GFCTG3-DESC-MSG   TO GFCTNA-DESC-MSG-ERRO
0410XS
0410XS         PERFORM 9999-FINALIZAR
0410XS     END-IF.
0410XS
0410XS     IF  GFCTN9-INIC-PER-COBR    NOT EQUAL 0 AND
0410XS         GFCTN9-INIC-PER-COBR    NOT EQUAL 1 AND
0410XS         GFCTN9-INIC-PER-COBR    NOT EQUAL 2 AND
0410XS         GFCTN9-INIC-PER-COBR    NOT EQUAL 3 AND
0410XS         GFCTN9-INIC-PER-COBR    NOT EQUAL 4 AND
0410XS         GFCTN9-INIC-PER-COBR    NOT EQUAL 5 AND
0410XS         GFCTN9-INIC-PER-COBR    NOT EQUAL 6 AND
0410XS         GFCTN9-INIC-PER-COBR    NOT EQUAL 7
0410XS         MOVE  1                 TO GFCTNA-ERRO
0410XS
0410XS         MOVE  ZEROS             TO GFCTNA-COD-SQL-ERRO
0410XS         MOVE  1255              TO GFCTNA-COD-MSG-ERRO
0410XS
0410XS         MOVE '1255'             TO GFCTG2-COD-MSG
0410XS
0410XS         PERFORM 9900-OBTER-DESC-MSG
0410XS
0410XS         MOVE  GFCTG3-DESC-MSG   TO GFCTNA-DESC-MSG-ERRO
0410XS
0410XS         PERFORM 9999-FINALIZAR
0410XS     END-IF.
0410XS
0410XS     IF  GFCTN9-PERIODO-COBR     EQUAL 2
0410XS         IF  GFCTN9-INIC-PER-COBR
0410XS                                 NOT EQUAL 1 AND
0410XS             GFCTN9-INIC-PER-COBR
0410XS                                 NOT EQUAL 2 AND
0410XS             GFCTN9-INIC-PER-COBR
0410XS                                 NOT EQUAL 3 AND
0410XS             GFCTN9-INIC-PER-COBR
0410XS                                 NOT EQUAL 4 AND
0410XS             GFCTN9-INIC-PER-COBR
0410XS                                 NOT EQUAL 5 AND
0410XS             GFCTN9-INIC-PER-COBR
0410XS                                 NOT EQUAL 6 AND
0410XS             GFCTN9-INIC-PER-COBR
0410XS                                 NOT EQUAL 7
0410XS             MOVE  1             TO GFCTNA-ERRO
0410XS
0410XS             MOVE  ZEROS         TO GFCTNA-COD-SQL-ERRO
0410XS             MOVE  1256          TO GFCTNA-COD-MSG-ERRO
0410XS
0410XS             MOVE '1256'         TO GFCTG2-COD-MSG
0410XS             PERFORM 9900-OBTER-DESC-MSG
0410XS
0410XS             MOVE  GFCTG3-DESC-MSG
0410XS                                 TO GFCTNA-DESC-MSG-ERRO
0410XS
0410XS             PERFORM 9999-FINALIZAR
0410XS         END-IF
0410XS     END-IF.
0410XS
0410XS*----------------------------------------------------------------*
0410XS 3020-99-FIM.                    EXIT.
0410XS*----------------------------------------------------------------*
0410XS
0410XS******************************************************************
0410XS*ROTINA PARA ACESSAR PARAMETROS DA GESTAO                        *
0410XS******************************************************************
0410XS*----------------------------------------------------------------*
0410XS 3030-ACESSAR-PARM-GESTAO        SECTION.
0410XS*----------------------------------------------------------------*
0410XS
0410XS     MOVE DPROCM-ATUAL OF GFCTB0A1
0410XS                                 TO DFIM-VGCIA-GSTOR OF GFCTB0A0.
0410XS
0410XS     PERFORM 3031-OBTER-MAX-DATA-INI-VGCIA.
0410XS
0410XS     EXEC SQL
0410XS       SELECT
0410XS               QMAX_FLEXZ,
0410XS               QMAX_RTCAO,
0410XS               QMAX_FRANQ_TARIF
0410XS         INTO
0410XS              :GFCTB0A0.QMAX-FLEXZ,
0410XS              :GFCTB0A0.QMAX-RTCAO,
0410XS              :GFCTB0A0.QMAX-FRANQ-TARIF
0410XS         FROM
0410XS               DB2PRD.PARM_CTRL_GSTAO
0410XS        WHERE
0410XS               DFIM_VGCIA_GSTOR  >= :GFCTB0A0.DFIM-VGCIA-GSTOR
0410XS          AND  DINIC_VGCIA_GSTOR  = :GFCTB0A0.DINIC-VGCIA-GSTOR
0410XS     END-EXEC.
0410XS
0410XS     IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
0410XS        (SQLWARN0                EQUAL 'W')
0410XS         MOVE  SPACES            TO GFCT0M-ERRO-SQL
0410XS
0410XS         INITIALIZE                 GFCT0M-ERRO-SQL
0410XS
0410XS         MOVE 'PARM_CTRL_GSTAO'  TO GFCT0M-NOME-TAB
0410XS         MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
0410XS         MOVE '0300'             TO GFCT0M-LOCAL
0410XS
0410XS         MOVE  0010              TO GFCTNA-COD-MSG-ERRO
0410XS
0410XS         MOVE '0010'             TO GFCTG2-COD-MSG
0410XS
0410XS         PERFORM 9900-OBTER-DESC-MSG
0410XS
0410XS         MOVE ' - GFCTB0A0'      TO WRK-NOME-TAB
0410XS
0410XS         PERFORM 9000-MOVER-ERRO-SQL
0410XS
0410XS         PERFORM 9999-FINALIZAR
0410XS     END-IF.
0410XS
0410XS     IF  SQLCODE                 EQUAL +100
0410XS         MOVE 1                  TO GFCTNA-ERRO
0410XS
0410XS         MOVE ZEROS              TO GFCTNA-COD-SQL-ERRO
0410XS         MOVE 0752               TO GFCTNA-COD-MSG-ERRO
0410XS
0410XS         MOVE '0752'             TO GFCTG2-COD-MSG
0410XS
0410XS         PERFORM 9900-OBTER-DESC-MSG
0410XS
0410XS         MOVE  GFCTG3-DESC-MSG   TO GFCTNA-DESC-MSG-ERRO
0410XS
0410XS         PERFORM 9999-FINALIZAR
0410XS     END-IF.
0410XS
0410XS*----------------------------------------------------------------*
0410XS 3030-99-FIM.                    EXIT.
0410XS*----------------------------------------------------------------*
0410XS
0410XS******************************************************************
0410XS*ROTINA PARA OBTER ULTIMO PARAMETRO DE GESTAO                    *
0410XS******************************************************************
0410XS*----------------------------------------------------------------*
0410XS 3031-OBTER-MAX-DATA-INI-VGCIA   SECTION.
0410XS*----------------------------------------------------------------*
0410XS
0410XS     EXEC SQL
0410XS       SELECT
0410XS               MAX(DINIC_VGCIA_GSTOR)
0410XS         INTO
0410XS              :GFCTB0A0.DINIC-VGCIA-GSTOR
0410XS         FROM
0410XS               DB2PRD.PARM_CTRL_GSTAO
0410XS         WHERE
0410XS               DFIM_VGCIA_GSTOR  >= :GFCTB0A0.DFIM-VGCIA-GSTOR
0410XS     END-EXEC.
0410XS
0410XS     IF (SQLCODE                 NOT EQUAL ZEROS AND -305) OR
0410XS        (SQLWARN0                EQUAL 'W')
0410XS         MOVE  SPACES            TO GFCT0M-ERRO-SQL
0410XS
0410XS         INITIALIZE                 GFCT0M-ERRO-SQL
0410XS
0410XS         MOVE 'PARM_CTRL_GSTAO'  TO GFCT0M-NOME-TAB
0410XS         MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
0410XS         MOVE '0310'             TO GFCT0M-LOCAL
0410XS
0410XS         MOVE  0010              TO GFCTNA-COD-MSG-ERRO
0410XS
0410XS         MOVE '0010'             TO GFCTG2-COD-MSG
0410XS
0410XS         PERFORM 9900-OBTER-DESC-MSG
0410XS
0410XS         MOVE ' - GFCTB0A0'      TO WRK-NOME-TAB
0410XS
0410XS         PERFORM 9000-MOVER-ERRO-SQL
0410XS
0410XS         PERFORM 9999-FINALIZAR
0410XS     END-IF.
0410XS
0410XS     IF  SQLCODE                 EQUAL -305
0410XS         MOVE '01.01.0001'       TO DINIC-VGCIA-GSTOR OF GFCTB0A0
0410XS     END-IF.
0410XS
0410XS*----------------------------------------------------------------*
0410XS 3031-99-FIM.                    EXIT.
0410XS*----------------------------------------------------------------*
0410XS
0410XS******************************************************************
0410XS*ROTINA PARA CONSISTIR PARAMETROS DA GESTAO                      *
0410XS******************************************************************
0410XS*----------------------------------------------------------------*
0410XS 3040-CONSISTIR-PARM-GESTAO      SECTION.
0410XS*----------------------------------------------------------------*
0410XS
0410XS     PERFORM 3041-ACESSAR-POOL1280.
0410XS
0410XS     IF  WRK-1280-NRO-DIAS       GREATER QMAX-FLEXZ OF GFCTB0A0
0410XS         MOVE  1                 TO GFCTNA-ERRO
0410XS
0410XS         MOVE  ZEROS             TO GFCTNA-COD-SQL-ERRO
0410XS         MOVE  0621              TO GFCTNA-COD-MSG-ERRO
0410XS
0410XS         MOVE '0621'             TO GFCTG2-COD-MSG
0410XS
0410XS         PERFORM 9900-OBTER-DESC-MSG
0410XS
0410XS         MOVE  GFCTG3-DESC-MSG   TO GFCTNA-DESC-MSG-ERRO
0410XS
0410XS         PERFORM 9999-FINALIZAR
0410XS     END-IF.
0410XS
0410XS     MOVE QMAX-RTCAO OF GFCTB0A0 TO WRK-QMAX-RTCAO.
0410XS
0410XS     IF  GFCTN9-DIAS-RETENCAO    GREATER WRK-QTDE-MAX-RTCAO
0410XS         MOVE  1                 TO GFCTNA-ERRO
0410XS
0410XS         MOVE  ZEROS             TO GFCTNA-COD-SQL-ERRO
0410XS         MOVE  0625              TO GFCTNA-COD-MSG-ERRO
0410XS
0410XS         MOVE '0625'             TO GFCTG2-COD-MSG
0410XS
0410XS         PERFORM 9900-OBTER-DESC-MSG
0410XS
0410XS         MOVE GFCTG3-DESC-MSG    TO GFCTNA-DESC-MSG-ERRO
0410XS
0410XS         PERFORM 9999-FINALIZAR
0410XS     END-IF.
0410XS
0410XS     MOVE QMAX-FRANQ-TARIF OF GFCTB0A0
0410XS                                 TO WRK-QMAX-FRANQ-TARIF.
0410XS
0410XS     IF  GFCTN9-FRANQUIA         GREATER WRK-QTDE-MAX-FRANQ-TARIF
0410XS         MOVE  1                 TO GFCTNA-ERRO
0410XS
0410XS         MOVE  ZEROS             TO GFCTNA-COD-SQL-ERRO
0410XS         MOVE  0622              TO GFCTNA-COD-MSG-ERRO
0410XS
0410XS         MOVE '0622'             TO GFCTG2-COD-MSG
0410XS
0410XS         PERFORM 9900-OBTER-DESC-MSG
0410XS
0410XS         MOVE  GFCTG3-DESC-MSG   TO GFCTNA-DESC-MSG-ERRO
0410XS
0410XS         PERFORM 9999-FINALIZAR
0410XS     END-IF.
0410XS
0410XS*----------------------------------------------------------------*
0410XS 3040-99-FIM.                    EXIT.
0410XS*----------------------------------------------------------------*
0410XS
0410XS******************************************************************
0410XS*ROTINA PARA ACESSAR POOL1280                                    *
0410XS******************************************************************
0410XS*----------------------------------------------------------------*
0410XS 3041-ACESSAR-POOL1280           SECTION.
0410XS*----------------------------------------------------------------*
0410XS
0410XS     MOVE WRK-DT-INIC-FLEX-INV   TO WRK-1280-DATA-INFERIOR.
0410XS     MOVE WRK-DT-FIM-FLEX-INV    TO WRK-1280-DATA-SUPERIOR.
0410XS     MOVE ZEROS                  TO WRK-1280-NRO-DIAS.
0410XS
0410XS     CALL 'POOL1280'             USING WRK-1280-DATA-INFERIOR
0410XS                                       WRK-1280-DATA-SUPERIOR
0410XS                                       WRK-1280-NRO-DIAS
0410XS                                       WRK-1280-MENSAGEM.
0410XS
0410XS     IF  RETURN-CODE             EQUAL 4 OR
0410XS         RETURN-CODE             EQUAL 8
0410XS         MOVE  1                 TO GFCTNA-ERRO
0410XS
0410XS         MOVE  ZEROS             TO GFCTNA-COD-SQL-ERRO
0410XS         MOVE  0628              TO GFCTNA-COD-MSG-ERRO
0410XS
0410XS         MOVE '0628'             TO GFCTG2-COD-MSG
0410XS
0410XS         PERFORM 9900-OBTER-DESC-MSG
0410XS
0410XS         MOVE  GFCTG3-DESC-MSG   TO GFCTNA-DESC-MSG-ERRO
0410XS
0410XS         PERFORM 9999-FINALIZAR
0410XS     END-IF.
0410XS
0410XS*----------------------------------------------------------------*
0410XS 3041-99-FIM.                    EXIT.
0410XS*----------------------------------------------------------------*
0410XS
0410XS******************************************************************
0410XS* PROCESSAMENTO DO CURSOR                                        *
0410XS******************************************************************
0410XS*----------------------------------------------------------------*
0410XS 3100-PROCESSAR-CURSOR           SECTION.
0410XS*----------------------------------------------------------------*
0410XS
0410XS     MOVE 'S'                    TO WRK-TARIFA-OK.
0410XS
0410XS     PERFORM 3200-CONSISTIR-PEDIDO.
0410XS
0410XS     IF  WRK-TARIFA-OK           EQUAL 'S'
0410XS         PERFORM 3400-CONSISTIR-DATAS-VIGENCIAS
0410XS
0410XS         IF  WRK-TARIFA-OK       EQUAL 'S'
0410XS             PERFORM 3500-CONSISTIR-CAMPOS-TARIFA
0410XS
0410XS             IF  WRK-TARIFA-OK   EQUAL 'S'
0410XS                 PERFORM 3600-VERIFICAR-PERMISSAO
0410XS
0410XS                 IF  WRK-TARIFA-OK
0410XS                                 EQUAL 'S'
0410XS                     MOVE WRK-COD-TARIFA
0410XS                                 TO GFCTN9-COD-TARIFA
0410XS
0410XS                     PERFORM 2400-INCLUIR-SOLICITACAO
0410XS
0410XS                     ADD  1      TO WRK-ACM-SOLICITACAO
0410XS
0410XS                     MOVE ZEROS  TO GFCTN9-COD-TARIFA
0410XS                 END-IF
0410XS             END-IF
0410XS         END-IF
0410XS     END-IF.
0410XS
0410XS     PERFORM 3800-LER-CSR01.
0410XS
0410XS*----------------------------------------------------------------*
0410XS 3100-99-FIM.                    EXIT.
0410XS*----------------------------------------------------------------
0410XS
0410XS******************************************************************
0410XS*ROTINA PARA CONSISTIR PEDIDO                                    *
0410XS******************************************************************
0410XS*----------------------------------------------------------------*
0410XS 3200-CONSISTIR-PEDIDO           SECTION.
0410XS*----------------------------------------------------------------*
0410XS
0410XS     EVALUATE GFCTN9-AGRUPAMENTO
0410XS
0410XS         WHEN 15
0410XS
0410XS              PERFORM 3210-CONSISTIR-PED-PAB
0410XS
0410XS         WHEN 14
0410XS
0410XS              PERFORM 3220-CONSISTIR-PED-AGENCIA
0410XS
0410XS         WHEN 17
0410XS         WHEN 20
0410XS
0410XS              PERFORM 3230-CONSISTIR-PED-POSTAL
0410XS
0410XS         WHEN 4
0410XS
0410XS              PERFORM 3240-CONSISTIR-PED-CLIENTE
0410XS
0410XS         WHEN 9
0410XS
0410XS              PERFORM 3250-CONSISTIR-PED-SEGMENTO
0410XS
0410XS         WHEN 18
0410XS
0410XS              PERFORM 3260-CONSISTIR-PED-MUNICIPIO
0410XS
0410XS         WHEN 19
0410XS
0410XS              PERFORM 3270-CONSISTIR-PED-UF
0410XS
170816*  OS AGRUPAMENTOS HSBC TERAO O MESMO TRATAMENTO DO AGRUPAMENTO
170816*  FUNCIONARIO BRADESCO
170816         WHEN 42
170816         WHEN 43
0410XS         WHEN 7
0410XS
0410XS              PERFORM 3280-CONSISTIR-PED-FUNC-BRAD
0410XS
0410XS         WHEN 26
0410XS
0410XS              PERFORM 3290-CONSISTIR-PED-RAZAO
0410XS
0410XS         WHEN OTHER
0410XS
0410XS              PERFORM 3300-CONS-PED-AGRUP-PERSONALIZ
0410XS
0410XS     END-EVALUATE.
0410XS
0410XS*----------------------------------------------------------------*
0410XS 3200-99-FIM.                    EXIT.
0410XS*----------------------------------------------------------------*
0410XS
0410XS******************************************************************
0410XS*ROTINA PARA CONSISTIR PEDIDO PAB                                *
0410XS******************************************************************
0410XS*----------------------------------------------------------------*
0410XS 3210-CONSISTIR-PED-PAB          SECTION.
0410XS*----------------------------------------------------------------*
0410XS
0410XS     MOVE DPROCM-ATUAL OF GFCTB0A1
0410XS                                 TO DINIC-FLEXZ-AGPTO OF GFCTB0B2
0410XS                                    DFIM-FLEXZ-AGPTO  OF GFCTB0B2.
0410XS
0410XS     MOVE GFCTN9-AGRUPAMENTO     TO CAGPTO-CTA        OF GFCTB0B2
0410XS                                    CAGPTO-CTA        OF GFCTB0B8.
0410XS
0410XS     MOVE WRK-COD-TARIFA         TO CSERVC-TARIF      OF GFCTB0B2
0410XS                                    CSERVC-TARIF      OF GFCTB0B8.
0410XS
0410XS     MOVE GFCTN9-AGENCIA         TO CDEPDC            OF GFCTB0B8.
0410XS     MOVE GFCTN9-POSTO           TO CPOSTO-SERVC      OF GFCTB0B8.
0410XS
0410XS     PERFORM 3211-OBTER-MAX-FLEXIBILIZACAO.
0410XS
0410XS     EXEC SQL
0410XS       SELECT
0410XS               A.CSIT_SOLTC_FLEXZ
0410XS         INTO
0410XS              :GFCTB0B2.CSIT-SOLTC-FLEXZ
0410XS         FROM
0410XS               DB2PRD.PDIDO_FLEXZ_AGPTO A,
0410XS               DB2PRD.PDIDO_FLEXZ_PAB   B
0410XS        WHERE
0410XS               A.CAGPTO_CTA       = B.CAGPTO_CTA
0410XS          AND  A.CSERVC_TARIF     = B.CSERVC_TARIF
0410XS          AND  A.HIDTFD_FLEXZ_AGPTO
0410XS                                  = B.HIDTFD_FLEXZ_AGPTO
0410XS          AND (A.CAGPTO_CTA       = :GFCTB0B2.CAGPTO-CTA
0410XS          AND  A.CSERVC_TARIF     = :GFCTB0B2.CSERVC-TARIF
0410XS          AND  A.DFIM_FLEXZ_AGPTO
0410XS                                 >= :GFCTB0B2.DFIM-FLEXZ-AGPTO)
0410XS          AND (B.CAGPTO_CTA       = :GFCTB0B8.CAGPTO-CTA
0410XS          AND  B.CSERVC_TARIF     = :GFCTB0B8.CSERVC-TARIF
0410XS          AND  B.CDEPDC           = :GFCTB0B8.CDEPDC
0410XS          AND  B.CPOSTO_SERVC     = :GFCTB0B8.CPOSTO-SERVC
0410XS          AND  B.HIDTFD_FLEXZ_AGPTO
0410XS                                  = :GFCTB0B8.HIDTFD-FLEXZ-AGPTO)
0410XS     END-EXEC.
0410XS
0410XS     IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
0410XS        (SQLWARN0                EQUAL 'W')
0410XS         MOVE  SPACES            TO GFCT0M-ERRO-SQL
0410XS
0410XS         INITIALIZE                 GFCT0M-ERRO-SQL
0410XS
0410XS         MOVE 'PDIDO_FLEXZ_AGPTO'
0410XS                                 TO GFCT0M-NOME-TAB
0410XS         MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
0410XS         MOVE '0320'             TO GFCT0M-LOCAL
0410XS
0410XS         MOVE  0010              TO GFCTNA-COD-MSG-ERRO
0410XS
0410XS         MOVE '0010'             TO GFCTG2-COD-MSG
0410XS
0410XS         PERFORM 9900-OBTER-DESC-MSG
0410XS
0410XS         MOVE ' - GFCTB0B2'      TO WRK-NOME-TAB
0410XS
0410XS         PERFORM 9000-MOVER-ERRO-SQL
0410XS
0410XS         PERFORM 9999-FINALIZAR
0410XS     END-IF.
0410XS
0410XS     IF  SQLCODE                 EQUAL ZEROS
0410XS         IF  CSIT-SOLTC-FLEXZ OF GFCTB0B2
0410XS                                 EQUAL 1 OR
0410XS             CSIT-SOLTC-FLEXZ OF GFCTB0B2
0410XS                                 EQUAL 2 OR
0410XS             CSIT-SOLTC-FLEXZ OF GFCTB0B2
0410XS                                 EQUAL 5 OR
0410XS             CSIT-SOLTC-FLEXZ OF GFCTB0B2
0410XS                                 EQUAL 7 OR
0410XS             CSIT-SOLTC-FLEXZ OF GFCTB0B2
0410XS                                 EQUAL 8
0410XS             MOVE 'N'            TO WRK-TARIFA-OK
0410XS         END-IF
0410XS     END-IF.
0410XS
0410XS*----------------------------------------------------------------*
0410XS 3210-99-FIM.                    EXIT.
0410XS*----------------------------------------------------------------*
0410XS
0410XS******************************************************************
0410XS*ROTINA PARA OBTER ULTIMO PEDIDO FLEXIBILIZACAO PAB              *
0410XS******************************************************************
0410XS*----------------------------------------------------------------*
0410XS 3211-OBTER-MAX-FLEXIBILIZACAO   SECTION.
0410XS*----------------------------------------------------------------*
0410XS
0410XS     EXEC SQL
0410XS       SELECT
0410XS               MAX(B.HIDTFD_FLEXZ_AGPTO)
0410XS         INTO
0410XS              :GFCTB0B8.HIDTFD-FLEXZ-AGPTO
0410XS         FROM
0410XS               DB2PRD.PDIDO_FLEXZ_AGPTO A,
0410XS               DB2PRD.PDIDO_FLEXZ_PAB   B
0410XS        WHERE
0410XS               A.CAGPTO_CTA       = B.CAGPTO_CTA
0410XS          AND  A.CSERVC_TARIF     = B.CSERVC_TARIF
0410XS          AND  A.HIDTFD_FLEXZ_AGPTO
0410XS                                  = B.HIDTFD_FLEXZ_AGPTO
0410XS          AND (A.CAGPTO_CTA       = :GFCTB0B2.CAGPTO-CTA
0410XS          AND  A.CSERVC_TARIF     = :GFCTB0B2.CSERVC-TARIF
0410XS          AND  A.DFIM_FLEXZ_AGPTO
0410XS                                 >= :GFCTB0B2.DFIM-FLEXZ-AGPTO)
0410XS          AND (B.CAGPTO_CTA       = :GFCTB0B8.CAGPTO-CTA
0410XS          AND  B.CSERVC_TARIF     = :GFCTB0B8.CSERVC-TARIF
0410XS          AND  B.CDEPDC           = :GFCTB0B8.CDEPDC
0410XS          AND  B.CPOSTO_SERVC     = :GFCTB0B8.CPOSTO-SERVC)
0410XS     END-EXEC.
0410XS
0410XS     IF (SQLCODE                 NOT EQUAL ZEROS AND -305) OR
0410XS        (SQLWARN0                EQUAL 'W')
0410XS         MOVE  SPACES            TO GFCT0M-ERRO-SQL
0410XS
0410XS         INITIALIZE                 GFCT0M-ERRO-SQL
0410XS
0410XS         MOVE 'PDIDO_FLEXZ_PAB'  TO GFCT0M-NOME-TAB
0410XS         MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
0410XS         MOVE '0330'             TO GFCT0M-LOCAL
0410XS
0410XS         MOVE  0010              TO GFCTNA-COD-MSG-ERRO
0410XS
0410XS         MOVE '0010'             TO GFCTG2-COD-MSG
0410XS
0410XS         PERFORM 9900-OBTER-DESC-MSG
0410XS
0410XS         MOVE ' - GFCTB0B8'      TO WRK-NOME-TAB
0410XS
0410XS         PERFORM 9000-MOVER-ERRO-SQL
0410XS
0410XS         PERFORM 9999-FINALIZAR
0410XS     END-IF.
0410XS
0410XS     IF  SQLCODE                 EQUAL -305
0410XS         MOVE '0001-01-01-01.01.01.000001'
0410XS                                 TO HIDTFD-FLEXZ-AGPTO
0410XS                                                      OF GFCTB0B8
0410XS     END-IF.
0410XS
0410XS*----------------------------------------------------------------*
0410XS 3211-99-FIM.                    EXIT.
0410XS*----------------------------------------------------------------*
0410XS
0410XS******************************************************************
0410XS*ROTINA PARA CONSISTIR PEDIDO AGENCIA                            *
0410XS******************************************************************
0410XS*----------------------------------------------------------------*
0410XS 3220-CONSISTIR-PED-AGENCIA      SECTION.
0410XS*----------------------------------------------------------------*
0410XS
0410XS     MOVE DPROCM-ATUAL OF GFCTB0A1
0410XS                                 TO DINIC-FLEXZ-AGPTO OF GFCTB0B2
0410XS                                    DFIM-FLEXZ-AGPTO  OF GFCTB0B2.
0410XS
0410XS     MOVE GFCTN9-AGRUPAMENTO     TO CAGPTO-CTA        OF GFCTB0B2
0410XS                                    CAGPTO-CTA        OF GFCTB0B1.
0410XS
0410XS     MOVE WRK-COD-TARIFA         TO CSERVC-TARIF      OF GFCTB0B2
0410XS                                    CSERVC-TARIF      OF GFCTB0B1.
0410XS
0410XS     MOVE GFCTN9-AGENCIA         TO CDEPDC            OF GFCTB0B1.
0410XS
0410XS     PERFORM 3221-OBTER-MAX-FLEXIBILIZACAO.
0410XS
0410XS     EXEC SQL
0410XS       SELECT
0410XS               A.CSIT_SOLTC_FLEXZ
0410XS         INTO
0410XS              :GFCTB0B2.CSIT-SOLTC-FLEXZ
0410XS         FROM
0410XS               DB2PRD.PDIDO_FLEXZ_AGPTO   A,
0410XS               DB2PRD.PDIDO_FLEXZ_AG      B
0410XS        WHERE
0410XS               A.CAGPTO_CTA       = B.CAGPTO_CTA
0410XS          AND  A.CSERVC_TARIF     = B.CSERVC_TARIF
0410XS          AND  A.HIDTFD_FLEXZ_AGPTO
0410XS                                  = B.HIDTFD_FLEXZ_AGPTO
0410XS          AND (A.CAGPTO_CTA       = :GFCTB0B2.CAGPTO-CTA
0410XS          AND  A.CSERVC_TARIF     = :GFCTB0B2.CSERVC-TARIF
0410XS          AND  A.DFIM_FLEXZ_AGPTO
0410XS                                 >= :GFCTB0B2.DFIM-FLEXZ-AGPTO)
0410XS          AND (B.CAGPTO_CTA       = :GFCTB0B1.CAGPTO-CTA
0410XS          AND  B.CSERVC_TARIF     = :GFCTB0B1.CSERVC-TARIF
0410XS          AND  B.CDEPDC           = :GFCTB0B1.CDEPDC
0410XS          AND  B.HIDTFD_FLEXZ_AGPTO
0410XS                                  = :GFCTB0B1.HIDTFD-FLEXZ-AGPTO)
0410XS     END-EXEC.
0410XS
0410XS     IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
0410XS        (SQLWARN0                EQUAL 'W')
0410XS         MOVE  SPACES            TO GFCT0M-ERRO-SQL
0410XS
0410XS         INITIALIZE                 GFCT0M-ERRO-SQL
0410XS
0410XS         MOVE 'PDIDO_FLEXZ_AGPTO'
0410XS                                 TO GFCT0M-NOME-TAB
0410XS         MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
0410XS         MOVE '0340'             TO GFCT0M-LOCAL
0410XS
0410XS         MOVE  0010              TO GFCTNA-COD-MSG-ERRO
0410XS
0410XS         MOVE '0010'             TO GFCTG2-COD-MSG
0410XS
0410XS         PERFORM 9900-OBTER-DESC-MSG
0410XS
0410XS         MOVE ' - GFCTB0B2'      TO WRK-NOME-TAB
0410XS
0410XS         PERFORM 9000-MOVER-ERRO-SQL
0410XS
0410XS         PERFORM 9999-FINALIZAR
0410XS     END-IF.
0410XS
0410XS     IF  SQLCODE                 EQUAL ZEROS
0410XS         IF  CSIT-SOLTC-FLEXZ OF GFCTB0B2
0410XS                                 EQUAL 1 OR
0410XS             CSIT-SOLTC-FLEXZ OF GFCTB0B2
0410XS                                 EQUAL 2 OR
0410XS             CSIT-SOLTC-FLEXZ OF GFCTB0B2
0410XS                                 EQUAL 5 OR
0410XS             CSIT-SOLTC-FLEXZ OF GFCTB0B2
0410XS                                 EQUAL 7 OR
0410XS             CSIT-SOLTC-FLEXZ OF GFCTB0B2
0410XS                                 EQUAL 8
0410XS             MOVE 'N'            TO WRK-TARIFA-OK
0410XS         END-IF
0410XS     END-IF.
0410XS
0410XS     IF  WRK-TARIFA-OK           EQUAL 'S'
0410XS         PERFORM 3225-CONSISTIR-EXISTENCIA-PED
0410XS     END-IF.
0410XS
0410XS*----------------------------------------------------------------*
0410XS 3220-99-FIM.                    EXIT.
0410XS*----------------------------------------------------------------*
0410XS
0410XS******************************************************************
0410XS*ROTINA PARA OBTER ULTIMO PEDIDO FLEXIBILIZACAO AGENCIA          *
0410XS******************************************************************
0410XS*----------------------------------------------------------------*
0410XS 3221-OBTER-MAX-FLEXIBILIZACAO   SECTION.
0410XS*----------------------------------------------------------------*
0410XS
0410XS     EXEC SQL
0410XS       SELECT
0410XS               MAX(B.HIDTFD_FLEXZ_AGPTO)
0410XS         INTO
0410XS              :GFCTB0B1.HIDTFD-FLEXZ-AGPTO
0410XS         FROM
0410XS               DB2PRD.PDIDO_FLEXZ_AGPTO A,
0410XS               DB2PRD.PDIDO_FLEXZ_AG    B
0410XS        WHERE
0410XS               A.CAGPTO_CTA       = B.CAGPTO_CTA
0410XS          AND  A.CSERVC_TARIF     = B.CSERVC_TARIF
0410XS          AND  A.HIDTFD_FLEXZ_AGPTO
0410XS                                  = B.HIDTFD_FLEXZ_AGPTO
0410XS          AND (A.CAGPTO_CTA       = :GFCTB0B2.CAGPTO-CTA
0410XS          AND  A.CSERVC_TARIF     = :GFCTB0B2.CSERVC-TARIF
0410XS          AND  A.DFIM_FLEXZ_AGPTO
0410XS                                 >= :GFCTB0B2.DFIM-FLEXZ-AGPTO)
0410XS          AND (B.CAGPTO_CTA       = :GFCTB0B1.CAGPTO-CTA
0410XS          AND  B.CSERVC_TARIF     = :GFCTB0B1.CSERVC-TARIF
0410XS          AND  B.CDEPDC           = :GFCTB0B1.CDEPDC)
0410XS     END-EXEC.
0410XS
0410XS     IF (SQLCODE                 NOT EQUAL ZEROS AND -305) OR
0410XS        (SQLWARN0                EQUAL 'W')
0410XS         MOVE  SPACES            TO GFCT0M-ERRO-SQL
0410XS
0410XS         INITIALIZE                 GFCT0M-ERRO-SQL
0410XS
0410XS         MOVE 'PDIDO_FLEXZ_AG'   TO GFCT0M-NOME-TAB
0410XS         MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
0410XS         MOVE '0350'             TO GFCT0M-LOCAL
0410XS
0410XS         MOVE  0010              TO GFCTNA-COD-MSG-ERRO
0410XS
0410XS         MOVE '0010'             TO GFCTG2-COD-MSG
0410XS
0410XS         PERFORM 9900-OBTER-DESC-MSG
0410XS
0410XS         MOVE ' - GFCTB0B1'      TO WRK-NOME-TAB
0410XS
0410XS         PERFORM 9000-MOVER-ERRO-SQL
0410XS
0410XS         PERFORM 9999-FINALIZAR
0410XS     END-IF.
0410XS
0410XS     IF  SQLCODE                 EQUAL -305
0410XS         MOVE '0001-01-01-01.01.01.000001'
0410XS                                 TO HIDTFD-FLEXZ-AGPTO
0410XS                                                      OF GFCTB0B1
0410XS     END-IF.
0410XS
0410XS*----------------------------------------------------------------*
0410XS 3221-99-FIM.                    EXIT.
0410XS*----------------------------------------------------------------*
0410XS
0410XS******************************************************************
0410XS*ROTINA PARA CONSISTIR EXISTENCIA DE PEDIDO DE AGRUPAMENTO       *
0410XS******************************************************************
0410XS*----------------------------------------------------------------*
0410XS 3225-CONSISTIR-EXISTENCIA-PED   SECTION.
0410XS*----------------------------------------------------------------*
0410XS
0410XS     MOVE DPROCM-ATUAL OF GFCTB0A1
0410XS                                 TO DFIM-FLEXZ-AGPTO  OF GFCTB0B2.
0410XS
0410XS     MOVE GFCTN9-AGRUPAMENTO     TO CAGPTO-CTA        OF GFCTB0B2
0410XS                                    CAGPTO-CTA        OF GFCTB0B1.
0410XS
0410XS     MOVE WRK-COD-TARIFA         TO CSERVC-TARIF      OF GFCTB0B2
0410XS                                    CSERVC-TARIF      OF GFCTB0B1.
0410XS
0410XS     IF  GFCTN9-TODAS-AG         EQUAL 'S'
0410XS         MOVE 00000              TO WRK-CDEPDC-INICIAL
0410XS         MOVE 99999              TO WRK-CDEPDC-FINAL
0410XS     ELSE
0410XS         MOVE 00000              TO WRK-CDEPDC-INICIAL
0410XS                                    WRK-CDEPDC-FINAL
0410XS     END-IF.
0410XS
0410XS     EXEC SQL
0410XS       SELECT
0410XS               B.CDEPDC
0410XS         INTO
0410XS              :GFCTB0B1.CDEPDC
0410XS         FROM
0410XS               DB2PRD.PDIDO_FLEXZ_AGPTO A,
0410XS               DB2PRD.PDIDO_FLEXZ_AG    B
0410XS        WHERE
0410XS               A.CAGPTO_CTA       = B.CAGPTO_CTA
0410XS          AND  A.CSERVC_TARIF     = B.CSERVC_TARIF
0410XS          AND (A.CAGPTO_CTA       = :GFCTB0B2.CAGPTO-CTA
0410XS          AND  A.CSERVC_TARIF     = :GFCTB0B2.CSERVC-TARIF
0410XS          AND  A.DFIM_FLEXZ_AGPTO
0410XS                                 >= :GFCTB0B2.DFIM-FLEXZ-AGPTO
0410XS          AND  A.CSIT_SOLTC_FLEXZ
0410XS                                 IN (1, 2, 5, 7, 8))
0410XS          AND (B.CAGPTO_CTA       = :GFCTB0B1.CAGPTO-CTA
0410XS          AND  B.CSERVC_TARIF     = :GFCTB0B1.CSERVC-TARIF
0410XS          AND  B.CDEPDC     BETWEEN :WRK-CDEPDC-INICIAL
0410XS                                AND :WRK-CDEPDC-FINAL)
0410XS     END-EXEC.
0410XS
0410XS     IF (SQLCODE                 NOT EQUAL ZEROS AND +100
0410XS                                                 AND -811) OR
0410XS        (SQLWARN0                EQUAL 'W')
0410XS         MOVE  SPACES            TO GFCT0M-ERRO-SQL
0410XS
0410XS         INITIALIZE                 GFCT0M-ERRO-SQL
0410XS
0410XS         MOVE 'PDIDO_FLEXZ_AG'   TO GFCT0M-NOME-TAB
0410XS         MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
0410XS         MOVE '0360'             TO GFCT0M-LOCAL
0410XS
0410XS         MOVE  0010              TO GFCTNA-COD-MSG-ERRO
0410XS
0410XS         MOVE '0010'             TO GFCTG2-COD-MSG
0410XS
0410XS         PERFORM 9900-OBTER-DESC-MSG
0410XS
0410XS         MOVE ' - GFCTB0B1'      TO WRK-NOME-TAB
0410XS
0410XS         PERFORM 9000-MOVER-ERRO-SQL
0410XS
0410XS         PERFORM 9999-FINALIZAR
0410XS     END-IF.
0410XS
0410XS     IF  SQLCODE                 EQUAL ZEROS OR -811
0410XS         MOVE 'N'                TO WRK-TARIFA-OK
0410XS     END-IF.
0410XS
0410XS*----------------------------------------------------------------*
0410XS 3225-99-FIM.                    EXIT.
0410XS*----------------------------------------------------------------*
0410XS
0410XS******************************************************************
0410XS*ROTINA PARA CONSISTIR PEDIDO POSTAL                             *
0410XS******************************************************************
0410XS*----------------------------------------------------------------*
0410XS 3230-CONSISTIR-PED-POSTAL       SECTION.
0410XS*----------------------------------------------------------------*
0410XS
0410XS     MOVE DPROCM-ATUAL OF GFCTB0A1
0410XS                                 TO DINIC-FLEXZ-AGPTO OF GFCTB0B2
0410XS                                    DFIM-FLEXZ-AGPTO  OF GFCTB0B2.
0410XS
0410XS     MOVE GFCTN9-AGRUPAMENTO     TO CAGPTO-CTA        OF GFCTB0B2
0410XS                                    CAGPTO-CTA        OF GFCTB0B9.
0410XS
0410XS     MOVE WRK-COD-TARIFA         TO CSERVC-TARIF      OF GFCTB0B2
0410XS                                    CSERVC-TARIF      OF GFCTB0B9.
0410XS
0410XS     MOVE GFCTN9-AGENCIA         TO CDEPDC            OF GFCTB0B9.
0410XS     MOVE GFCTN9-POSTO           TO CPOSTO-SERVC      OF GFCTB0B9.
0410XS
0410XS     PERFORM 3231-OBTER-MAX-FLEXIBILIZACAO.
0410XS
0410XS     EXEC SQL
0410XS       SELECT
0410XS               A.CSIT_SOLTC_FLEXZ
0410XS         INTO
0410XS              :GFCTB0B2.CSIT-SOLTC-FLEXZ
0410XS         FROM
0410XS               DB2PRD.PDIDO_FLEXZ_AGPTO A,
0410XS               DB2PRD.PDIDO_FLEXZ_PSTAL B
0410XS        WHERE
0410XS               A.CAGPTO_CTA       = B.CAGPTO_CTA
0410XS          AND  A.CSERVC_TARIF     = B.CSERVC_TARIF
0410XS          AND  A.HIDTFD_FLEXZ_AGPTO
0410XS                                  = B.HIDTFD_FLEXZ_AGPTO
0410XS          AND (A.CAGPTO_CTA       = :GFCTB0B2.CAGPTO-CTA
0410XS          AND  A.CSERVC_TARIF     = :GFCTB0B2.CSERVC-TARIF
0410XS          AND  A.DFIM_FLEXZ_AGPTO
0410XS                                 >= :GFCTB0B2.DFIM-FLEXZ-AGPTO)
0410XS          AND (B.CAGPTO_CTA       = :GFCTB0B9.CAGPTO-CTA
0410XS          AND  B.CSERVC_TARIF     = :GFCTB0B9.CSERVC-TARIF
0410XS          AND  B.CDEPDC           = :GFCTB0B9.CDEPDC
0410XS          AND  B.CPOSTO_SERVC     = :GFCTB0B9.CPOSTO-SERVC
0410XS          AND  B.HIDTFD_FLEXZ_AGPTO
0410XS                                  = :GFCTB0B9.HIDTFD-FLEXZ-AGPTO)
0410XS     END-EXEC.
0410XS
0410XS     IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
0410XS        (SQLWARN0                EQUAL 'W')
0410XS         MOVE  SPACES            TO GFCT0M-ERRO-SQL
0410XS
0410XS         INITIALIZE                 GFCT0M-ERRO-SQL
0410XS
0410XS         MOVE 'PDIDO_FLEXZ_AGPTO'
0410XS                                 TO GFCT0M-NOME-TAB
0410XS         MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
0410XS         MOVE '0370'             TO GFCT0M-LOCAL
0410XS
0410XS         MOVE  0010              TO GFCTNA-COD-MSG-ERRO
0410XS
0410XS         MOVE '0010'             TO GFCTG2-COD-MSG
0410XS
0410XS         PERFORM 9900-OBTER-DESC-MSG
0410XS
0410XS         MOVE ' - GFCTB0B2'      TO WRK-NOME-TAB
0410XS
0410XS         PERFORM 9000-MOVER-ERRO-SQL
0410XS
0410XS         PERFORM 9999-FINALIZAR
0410XS     END-IF.
0410XS
0410XS     IF  SQLCODE                 EQUAL ZEROS
0410XS         IF  CSIT-SOLTC-FLEXZ OF GFCTB0B2
0410XS                                 EQUAL 1 OR
0410XS             CSIT-SOLTC-FLEXZ OF GFCTB0B2
0410XS                                 EQUAL 2 OR
0410XS             CSIT-SOLTC-FLEXZ OF GFCTB0B2
0410XS                                 EQUAL 5 OR
0410XS             CSIT-SOLTC-FLEXZ OF GFCTB0B2
0410XS                                 EQUAL 7 OR
0410XS             CSIT-SOLTC-FLEXZ OF GFCTB0B2
0410XS                                 EQUAL 8
0410XS             MOVE 'N'            TO WRK-TARIFA-OK
0410XS         END-IF
0410XS     END-IF.
0410XS
0410XS*----------------------------------------------------------------*
0410XS 3230-99-FIM.                    EXIT.
0410XS*----------------------------------------------------------------*
0410XS
0410XS******************************************************************
0410XS*ROTINA PARA OBTER ULTIMO PEDIDO FLEXIBILIZACAO POSTAL           *
0410XS******************************************************************
0410XS*----------------------------------------------------------------*
0410XS 3231-OBTER-MAX-FLEXIBILIZACAO   SECTION.
0410XS*----------------------------------------------------------------*
0410XS
0410XS     EXEC SQL
0410XS       SELECT
0410XS               MAX(B.HIDTFD_FLEXZ_AGPTO)
0410XS         INTO
0410XS              :GFCTB0B9.HIDTFD-FLEXZ-AGPTO
0410XS         FROM
0410XS               DB2PRD.PDIDO_FLEXZ_AGPTO A,
0410XS               DB2PRD.PDIDO_FLEXZ_PSTAL B
0410XS        WHERE
0410XS               A.CAGPTO_CTA       = B.CAGPTO_CTA
0410XS          AND  A.CSERVC_TARIF     = B.CSERVC_TARIF
0410XS          AND  A.HIDTFD_FLEXZ_AGPTO
0410XS                                  = B.HIDTFD_FLEXZ_AGPTO
0410XS          AND (A.CAGPTO_CTA       = :GFCTB0B2.CAGPTO-CTA
0410XS          AND  A.CSERVC_TARIF     = :GFCTB0B2.CSERVC-TARIF
0410XS          AND  A.DFIM_FLEXZ_AGPTO
0410XS                                 >= :GFCTB0B2.DFIM-FLEXZ-AGPTO)
0410XS          AND (B.CAGPTO_CTA       = :GFCTB0B9.CAGPTO-CTA
0410XS          AND  B.CSERVC_TARIF     = :GFCTB0B9.CSERVC-TARIF
0410XS          AND  B.CDEPDC           = :GFCTB0B9.CDEPDC
0410XS          AND  B.CPOSTO_SERVC     = :GFCTB0B9.CPOSTO-SERVC)
0410XS     END-EXEC.
0410XS
0410XS     IF (SQLCODE                 NOT EQUAL ZEROS AND -305) OR
0410XS        (SQLWARN0                EQUAL 'W')
0410XS         MOVE  SPACES            TO GFCT0M-ERRO-SQL
0410XS
0410XS         INITIALIZE                 GFCT0M-ERRO-SQL
0410XS
0410XS         MOVE 'PDIDO_FLEXZ_PSTAL'
0410XS                                 TO GFCT0M-NOME-TAB
0410XS         MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
0410XS         MOVE '0380'             TO GFCT0M-LOCAL
0410XS
0410XS         MOVE  0010              TO GFCTNA-COD-MSG-ERRO
0410XS
0410XS         MOVE '0010'             TO GFCTG2-COD-MSG
0410XS
0410XS         PERFORM 9900-OBTER-DESC-MSG
0410XS
0410XS         MOVE ' - GFCTB0B9'      TO WRK-NOME-TAB
0410XS
0410XS         PERFORM 9000-MOVER-ERRO-SQL
0410XS
0410XS         PERFORM 9999-FINALIZAR
0410XS     END-IF.
0410XS
0410XS     IF  SQLCODE                 EQUAL -305
0410XS         MOVE '0001-01-01-01.01.01.000001'
0410XS                                 TO HIDTFD-FLEXZ-AGPTO
0410XS                                                      OF GFCTB0B9
0410XS     END-IF.
0410XS
0410XS*----------------------------------------------------------------*
0410XS 3231-99-FIM.                    EXIT.
0410XS*----------------------------------------------------------------*
0410XS
0410XS******************************************************************
0410XS*ROTINA PARA CONSISTIR PEDIDO CLIENTE                            *
0410XS******************************************************************
0410XS*----------------------------------------------------------------*
0410XS 3240-CONSISTIR-PED-CLIENTE      SECTION.
0410XS*----------------------------------------------------------------*
0410XS
0410XS     MOVE DPROCM-ATUAL OF GFCTB0A1
0410XS                                 TO DINIC-FLEXZ-AGPTO OF GFCTB0B2
0410XS                                    DFIM-FLEXZ-AGPTO  OF GFCTB0B2.
0410XS
0410XS     MOVE GFCTN9-AGRUPAMENTO     TO CAGPTO-CTA        OF GFCTB0B2
0410XS                                    CAGPTO-CTA        OF GFCTB0B3.
0410XS
0410XS     MOVE WRK-COD-TARIFA         TO CSERVC-TARIF      OF GFCTB0B2
0410XS                                    CSERVC-TARIF      OF GFCTB0B3.
0410XS
0410XS     MOVE GFCTN9-CGC-CPF         TO CCGC-CPF          OF GFCTB0B3.
0410XS     MOVE GFCTN9-FILIAL          TO CFLIAL-CGC        OF GFCTB0B3.
0410XS
0410XS     MOVE GFCTN9-CONTROLE        TO WRK-CPF-CGC-CONTROLE.
0410XS     MOVE WRK-CPF-CGC-CONTROLE-9 TO CCTRL-CPF-CGC     OF GFCTB0B3.
0410XS
0410XS     PERFORM 3241-OBTER-MAX-FLEXIBILIZACAO.
0410XS
0410XS     EXEC SQL
0410XS       SELECT
0410XS               A.CSIT_SOLTC_FLEXZ
0410XS         INTO
0410XS              :GFCTB0B2.CSIT-SOLTC-FLEXZ
0410XS         FROM
0410XS               DB2PRD.PDIDO_FLEXZ_AGPTO A,
0410XS               DB2PRD.PDIDO_FLEXZ_CLI   B
0410XS        WHERE
0410XS               A.CAGPTO_CTA       = B.CAGPTO_CTA
0410XS          AND  A.CSERVC_TARIF     = B.CSERVC_TARIF
0410XS          AND  A.HIDTFD_FLEXZ_AGPTO
0410XS                                  = B.HIDTFD_FLEXZ_AGPTO
0410XS          AND (A.CAGPTO_CTA       = :GFCTB0B2.CAGPTO-CTA
0410XS          AND  A.CSERVC_TARIF     = :GFCTB0B2.CSERVC-TARIF
0410XS          AND  A.DFIM_FLEXZ_AGPTO
0410XS                                 >= :GFCTB0B2.DFIM-FLEXZ-AGPTO)
0410XS          AND (B.CAGPTO_CTA       = :GFCTB0B3.CAGPTO-CTA
0410XS          AND  B.CSERVC_TARIF     = :GFCTB0B3.CSERVC-TARIF
0410XS          AND  B.CCGC_CPF         = :GFCTB0B3.CCGC-CPF
0410XS          AND  B.CFLIAL_CGC       = :GFCTB0B3.CFLIAL-CGC
0410XS          AND  B.CCTRL_CPF_CGC    = :GFCTB0B3.CCTRL-CPF-CGC
0410XS          AND  B.HIDTFD_FLEXZ_AGPTO
0410XS                                  = :GFCTB0B3.HIDTFD-FLEXZ-AGPTO)
0410XS     END-EXEC.
0410XS
0410XS     IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
0410XS        (SQLWARN0                EQUAL 'W')
0410XS         MOVE  SPACES            TO GFCT0M-ERRO-SQL
0410XS
0410XS         INITIALIZE                 GFCT0M-ERRO-SQL
0410XS
0410XS         MOVE 'PDIDO_FLEXZ_AGPTO'
0410XS                                 TO GFCT0M-NOME-TAB
0410XS         MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
0410XS         MOVE '0390'             TO GFCT0M-LOCAL
0410XS
0410XS         MOVE  0010              TO GFCTNA-COD-MSG-ERRO
0410XS
0410XS         MOVE '0010'             TO GFCTG2-COD-MSG
0410XS
0410XS         PERFORM 9900-OBTER-DESC-MSG
0410XS
0410XS         MOVE ' - GFCTB0B2'      TO WRK-NOME-TAB
0410XS
0410XS         PERFORM 9000-MOVER-ERRO-SQL
0410XS
0410XS         PERFORM 9999-FINALIZAR
0410XS     END-IF.
0410XS
0410XS     IF  SQLCODE                 EQUAL ZEROS
0410XS         IF  CSIT-SOLTC-FLEXZ OF GFCTB0B2
0410XS                                 EQUAL 1 OR
0410XS             CSIT-SOLTC-FLEXZ OF GFCTB0B2
0410XS                                 EQUAL 2 OR
0410XS             CSIT-SOLTC-FLEXZ OF GFCTB0B2
0410XS                                 EQUAL 5 OR
0410XS             CSIT-SOLTC-FLEXZ OF GFCTB0B2
0410XS                                 EQUAL 7 OR
0410XS             CSIT-SOLTC-FLEXZ OF GFCTB0B2
0410XS                                 EQUAL 8
0410XS             MOVE 'N'            TO WRK-TARIFA-OK
0410XS         END-IF
0410XS     END-IF.
0410XS
0410XS*----------------------------------------------------------------*
0410XS 3240-99-FIM.                    EXIT.
0410XS*----------------------------------------------------------------*
0410XS
0410XS******************************************************************
0410XS*ROTINA PARA OBTER ULTIMO PEDIDO FLEXIBILIZACAO CLIENTE          *
0410XS******************************************************************
0410XS*----------------------------------------------------------------*
0410XS 3241-OBTER-MAX-FLEXIBILIZACAO   SECTION.
0410XS*----------------------------------------------------------------*
0410XS
0410XS     EXEC SQL
0410XS       SELECT
0410XS               MAX(B.HIDTFD_FLEXZ_AGPTO)
0410XS         INTO
0410XS              :GFCTB0B3.HIDTFD-FLEXZ-AGPTO
0410XS         FROM
0410XS               DB2PRD.PDIDO_FLEXZ_AGPTO A,
0410XS               DB2PRD.PDIDO_FLEXZ_CLI   B
0410XS        WHERE
0410XS               A.CAGPTO_CTA       = B.CAGPTO_CTA
0410XS          AND  A.CSERVC_TARIF     = B.CSERVC_TARIF
0410XS          AND  A.HIDTFD_FLEXZ_AGPTO
0410XS                                  = B.HIDTFD_FLEXZ_AGPTO
0410XS          AND (A.CAGPTO_CTA       = :GFCTB0B2.CAGPTO-CTA
0410XS          AND  A.CSERVC_TARIF     = :GFCTB0B2.CSERVC-TARIF
0410XS          AND  A.DFIM_FLEXZ_AGPTO
0410XS                                 >= :GFCTB0B2.DFIM-FLEXZ-AGPTO)
0410XS          AND (B.CAGPTO_CTA       = :GFCTB0B3.CAGPTO-CTA
0410XS          AND  B.CSERVC_TARIF     = :GFCTB0B3.CSERVC-TARIF
0410XS          AND  B.CCGC_CPF         = :GFCTB0B3.CCGC-CPF
0410XS          AND  B.CFLIAL_CGC       = :GFCTB0B3.CFLIAL-CGC
0410XS          AND  B.CCTRL_CPF_CGC    = :GFCTB0B3.CCTRL-CPF-CGC)
0410XS     END-EXEC.
0410XS
0410XS     IF (SQLCODE                 NOT EQUAL ZEROS AND -305) OR
0410XS        (SQLWARN0                EQUAL 'W')
0410XS         MOVE  SPACES            TO GFCT0M-ERRO-SQL
0410XS
0410XS         INITIALIZE                 GFCT0M-ERRO-SQL
0410XS
0410XS         MOVE 'PDIDO_FLEXZ_CLI'  TO GFCT0M-NOME-TAB
0410XS         MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
0410XS         MOVE '0400'             TO GFCT0M-LOCAL
0410XS
0410XS         MOVE  0010              TO GFCTNA-COD-MSG-ERRO
0410XS
0410XS         MOVE '0010'             TO GFCTG2-COD-MSG
0410XS
0410XS         PERFORM 9900-OBTER-DESC-MSG
0410XS
0410XS         MOVE ' - GFCTB0B3'      TO WRK-NOME-TAB
0410XS
0410XS         PERFORM 9000-MOVER-ERRO-SQL
0410XS
0410XS         PERFORM 9999-FINALIZAR
0410XS     END-IF.
0410XS
0410XS     IF  SQLCODE                 EQUAL -305
0410XS         MOVE '0001-01-01-01.01.01.000001'
0410XS                                 TO HIDTFD-FLEXZ-AGPTO
0410XS                                                      OF GFCTB0B3
0410XS     END-IF.
0410XS
0410XS*----------------------------------------------------------------*
0410XS 3241-99-FIM.                    EXIT.
0410XS*----------------------------------------------------------------*
0410XS
0410XS******************************************************************
0410XS*ROTINA PARA CONSISTIR PEDIDO SEGMENTO                           *
0410XS******************************************************************
0410XS*----------------------------------------------------------------*
0410XS 3250-CONSISTIR-PED-SEGMENTO     SECTION.
0410XS*----------------------------------------------------------------*
0410XS
0410XS     MOVE DPROCM-ATUAL OF GFCTB0A1
0410XS                                 TO DINIC-FLEXZ-AGPTO OF GFCTB0B2
0410XS                                    DFIM-FLEXZ-AGPTO  OF GFCTB0B2.
0410XS
0410XS     MOVE GFCTN9-AGRUPAMENTO     TO CAGPTO-CTA        OF GFCTB0B2
0410XS                                    CAGPTO-CTA        OF GFCTB0C0.
0410XS
0410XS     MOVE WRK-COD-TARIFA         TO CSERVC-TARIF      OF GFCTB0B2
0410XS                                    CSERVC-TARIF      OF GFCTB0C0.
0410XS
0410XS     MOVE GFCTN9-SEGMENTO        TO CSGMTO-GSTAO-TARIF
0410XS                                                      OF GFCTB0C0.
0410XS
0410XS     PERFORM 3251-OBTER-MAX-FLEXIBILIZACAO.
0410XS
0410XS     EXEC SQL
0410XS       SELECT
0410XS               A.CSIT_SOLTC_FLEXZ
0410XS         INTO
0410XS              :GFCTB0B2.CSIT-SOLTC-FLEXZ
0410XS         FROM
0410XS               DB2PRD.PDIDO_FLEXZ_AGPTO A,
0410XS               DB2PRD.PDIDO_FLEXZ_SGMTO B
0410XS        WHERE
0410XS               A.CAGPTO_CTA       = B.CAGPTO_CTA
0410XS          AND  A.CSERVC_TARIF     = B.CSERVC_TARIF
0410XS          AND  A.HIDTFD_FLEXZ_AGPTO
0410XS                                  = B.HIDTFD_FLEXZ_AGPTO
0410XS          AND (A.CAGPTO_CTA       = :GFCTB0B2.CAGPTO-CTA
0410XS          AND  A.CSERVC_TARIF     = :GFCTB0B2.CSERVC-TARIF
0410XS          AND  A.DFIM_FLEXZ_AGPTO
0410XS                                 >= :GFCTB0B2.DFIM-FLEXZ-AGPTO)
0410XS          AND (B.CAGPTO_CTA       = :GFCTB0C0.CAGPTO-CTA
0410XS          AND  B.CSERVC_TARIF     = :GFCTB0C0.CSERVC-TARIF
0410XS          AND  B.CSGMTO_GSTAO_TARIF
0410XS                                  = :GFCTB0C0.CSGMTO-GSTAO-TARIF
0410XS          AND  B.HIDTFD_FLEXZ_AGPTO
0410XS                                  = :GFCTB0C0.HIDTFD-FLEXZ-AGPTO)
0410XS     END-EXEC.
0410XS
0410XS     IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
0410XS        (SQLWARN0                EQUAL 'W')
0410XS         MOVE  SPACES            TO GFCT0M-ERRO-SQL
0410XS
0410XS         INITIALIZE                 GFCT0M-ERRO-SQL
0410XS
0410XS         MOVE 'PDIDO_FLEXZ_AGPTO'
0410XS                                 TO GFCT0M-NOME-TAB
0410XS         MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
0410XS         MOVE '0410'             TO GFCT0M-LOCAL
0410XS
0410XS         MOVE  0010              TO GFCTNA-COD-MSG-ERRO
0410XS
0410XS         MOVE '0010'             TO GFCTG2-COD-MSG
0410XS
0410XS         PERFORM 9900-OBTER-DESC-MSG
0410XS
0410XS         MOVE ' - GFCTB0B2'      TO WRK-NOME-TAB
0410XS
0410XS         PERFORM 9000-MOVER-ERRO-SQL
0410XS
0410XS         PERFORM 9999-FINALIZAR
0410XS     END-IF.
0410XS
0410XS     IF  SQLCODE                 EQUAL ZEROS
0410XS         IF  CSIT-SOLTC-FLEXZ OF GFCTB0B2
0410XS                                 EQUAL 1 OR
0410XS             CSIT-SOLTC-FLEXZ OF GFCTB0B2
0410XS                                 EQUAL 2 OR
0410XS             CSIT-SOLTC-FLEXZ OF GFCTB0B2
0410XS                                 EQUAL 5 OR
0410XS             CSIT-SOLTC-FLEXZ OF GFCTB0B2
0410XS                                 EQUAL 7 OR
0410XS             CSIT-SOLTC-FLEXZ OF GFCTB0B2
0410XS                                 EQUAL 8
0410XS             MOVE 'N'            TO WRK-TARIFA-OK
0410XS         END-IF
0410XS     END-IF.
0410XS
0410XS*----------------------------------------------------------------*
0410XS 3250-99-FIM.                    EXIT.
0410XS*----------------------------------------------------------------*
0410XS
0410XS******************************************************************
0410XS*ROTINA PARA OBTER ULTIMA PEDIDO FLEXIBILIZACAO SEGMENTO         *
0410XS******************************************************************
0410XS*----------------------------------------------------------------*
0410XS 3251-OBTER-MAX-FLEXIBILIZACAO   SECTION.
0410XS*----------------------------------------------------------------*
0410XS
0410XS     EXEC SQL
0410XS       SELECT
0410XS               MAX(B.HIDTFD_FLEXZ_AGPTO)
0410XS         INTO
0410XS              :GFCTB0C0.HIDTFD-FLEXZ-AGPTO
0410XS         FROM
0410XS               DB2PRD.PDIDO_FLEXZ_AGPTO A,
0410XS               DB2PRD.PDIDO_FLEXZ_SGMTO B
0410XS        WHERE
0410XS               A.CAGPTO_CTA       = B.CAGPTO_CTA
0410XS          AND  A.CSERVC_TARIF     = B.CSERVC_TARIF
0410XS          AND  A.HIDTFD_FLEXZ_AGPTO
0410XS                                  = B.HIDTFD_FLEXZ_AGPTO
0410XS          AND (A.CAGPTO_CTA       = :GFCTB0B2.CAGPTO-CTA
0410XS          AND  A.CSERVC_TARIF     = :GFCTB0B2.CSERVC-TARIF
0410XS          AND  A.DFIM_FLEXZ_AGPTO
0410XS                                 >= :GFCTB0B2.DFIM-FLEXZ-AGPTO)
0410XS          AND (B.CAGPTO_CTA       = :GFCTB0C0.CAGPTO-CTA
0410XS          AND  B.CSERVC_TARIF     = :GFCTB0C0.CSERVC-TARIF
0410XS          AND  B.CSGMTO_GSTAO_TARIF
0410XS                                  = :GFCTB0C0.CSGMTO-GSTAO-TARIF)
0410XS     END-EXEC.
0410XS
0410XS     IF (SQLCODE                 NOT EQUAL ZEROS AND -305) OR
0410XS        (SQLWARN0                EQUAL 'W')
0410XS         MOVE  SPACES            TO GFCT0M-ERRO-SQL
0410XS
0410XS         INITIALIZE                 GFCT0M-ERRO-SQL
0410XS
0410XS         MOVE 'PDIDO_FLEXZ_SGMTO'
0410XS                                 TO GFCT0M-NOME-TAB
0410XS         MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
0410XS         MOVE '0420'             TO GFCT0M-LOCAL
0410XS
0410XS         MOVE  0010              TO GFCTNA-COD-MSG-ERRO
0410XS
0410XS         MOVE '0010'             TO GFCTG2-COD-MSG
0410XS
0410XS         PERFORM 9900-OBTER-DESC-MSG
0410XS
0410XS         MOVE ' - GFCTB0C0'      TO WRK-NOME-TAB
0410XS
0410XS         PERFORM 9000-MOVER-ERRO-SQL
0410XS
0410XS         PERFORM 9999-FINALIZAR
0410XS     END-IF.
0410XS
0410XS     IF  SQLCODE                 EQUAL -305
0410XS         MOVE '0001-01-01-01.01.01.000001'
0410XS                                 TO HIDTFD-FLEXZ-AGPTO
0410XS                                                      OF GFCTB0C0
0410XS     END-IF.
0410XS
0410XS*----------------------------------------------------------------*
0410XS 3251-99-FIM.                    EXIT.
0410XS*----------------------------------------------------------------*
0410XS
0410XS******************************************************************
0410XS*ROTINA PARA CONSISTIR PEDIDO MUNICIPIO                          *
0410XS******************************************************************
0410XS*----------------------------------------------------------------*
0410XS 3260-CONSISTIR-PED-MUNICIPIO    SECTION.
0410XS*----------------------------------------------------------------*
0410XS
0410XS     MOVE DPROCM-ATUAL OF GFCTB0A1
0410XS                                 TO DINIC-FLEXZ-AGPTO OF GFCTB0B2
0410XS                                    DFIM-FLEXZ-AGPTO  OF GFCTB0B2.
0410XS
0410XS     MOVE GFCTN9-AGRUPAMENTO     TO CAGPTO-CTA        OF GFCTB0B2
0410XS                                    CAGPTO-CTA        OF GFCTB0B7.
0410XS
0410XS     MOVE WRK-COD-TARIFA         TO CSERVC-TARIF      OF GFCTB0B2
0410XS                                    CSERVC-TARIF      OF GFCTB0B7.
0410XS
0410XS     MOVE GFCTN9-MUNIC           TO CMUN-IBGE         OF GFCTB0B7.
0410XS
0410XS     PERFORM 3261-OBTER-MAX-FLEXIBILIZACAO.
0410XS
0410XS     EXEC SQL
0410XS       SELECT
0410XS               A.CSIT_SOLTC_FLEXZ
0410XS         INTO
0410XS              :GFCTB0B2.CSIT-SOLTC-FLEXZ
0410XS         FROM
0410XS               DB2PRD.PDIDO_FLEXZ_AGPTO A,
0410XS               DB2PRD.PDIDO_FLEXZ_MUN   B
0410XS        WHERE
0410XS               A.CAGPTO_CTA       = B.CAGPTO_CTA
0410XS          AND  A.CSERVC_TARIF     = B.CSERVC_TARIF
0410XS          AND  A.HIDTFD_FLEXZ_AGPTO
0410XS                                  = B.HIDTFD_FLEXZ_AGPTO
0410XS          AND (A.CAGPTO_CTA       = :GFCTB0B2.CAGPTO-CTA
0410XS          AND  A.CSERVC_TARIF     = :GFCTB0B2.CSERVC-TARIF
0410XS          AND  A.DFIM_FLEXZ_AGPTO
0410XS                                 >= :GFCTB0B2.DFIM-FLEXZ-AGPTO)
0410XS          AND (B.CAGPTO_CTA       = :GFCTB0B7.CAGPTO-CTA
0410XS          AND  B.CSERVC_TARIF     = :GFCTB0B7.CSERVC-TARIF
0410XS          AND  B.CMUN_IBGE        = :GFCTB0B7.CMUN-IBGE
0410XS          AND  B.HIDTFD_FLEXZ_AGPTO
0410XS                                  = :GFCTB0B7.HIDTFD-FLEXZ-AGPTO)
0410XS     END-EXEC.
0410XS
0410XS     IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
0410XS        (SQLWARN0                EQUAL 'W')
0410XS         MOVE  SPACES            TO GFCT0M-ERRO-SQL
0410XS
0410XS         INITIALIZE                 GFCT0M-ERRO-SQL
0410XS
0410XS         MOVE 'PDIDO_FLEXZ_AGPTO'
0410XS                                 TO GFCT0M-NOME-TAB
0410XS         MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
0410XS         MOVE '0430'             TO GFCT0M-LOCAL
0410XS
0410XS         MOVE  0010              TO GFCTNA-COD-MSG-ERRO
0410XS
0410XS         MOVE '0010'             TO GFCTG2-COD-MSG
0410XS
0410XS         PERFORM 9900-OBTER-DESC-MSG
0410XS
0410XS         MOVE ' - GFCTB0B2'      TO WRK-NOME-TAB
0410XS
0410XS         PERFORM 9000-MOVER-ERRO-SQL
0410XS
0410XS         PERFORM 9999-FINALIZAR
0410XS     END-IF.
0410XS
0410XS     IF  SQLCODE                 EQUAL ZEROS
0410XS         IF  CSIT-SOLTC-FLEXZ OF GFCTB0B2
0410XS                                 EQUAL 1 OR
0410XS             CSIT-SOLTC-FLEXZ OF GFCTB0B2
0410XS                                 EQUAL 2 OR
0410XS             CSIT-SOLTC-FLEXZ OF GFCTB0B2
0410XS                                 EQUAL 5 OR
0410XS             CSIT-SOLTC-FLEXZ OF GFCTB0B2
0410XS                                 EQUAL 7 OR
0410XS             CSIT-SOLTC-FLEXZ OF GFCTB0B2
0410XS                                 EQUAL 8
0410XS             MOVE 'N'            TO WRK-TARIFA-OK
0410XS         END-IF
0410XS     END-IF.
0410XS
0410XS*----------------------------------------------------------------*
0410XS 3260-99-FIM.                    EXIT.
0410XS*----------------------------------------------------------------*
0410XS
0410XS******************************************************************
0410XS*ROTINA PARA OBTER ULTIMO PEDIDO FLEXIBILIZACAO MUNICIPIO        *
0410XS******************************************************************
0410XS*----------------------------------------------------------------*
0410XS 3261-OBTER-MAX-FLEXIBILIZACAO   SECTION.
0410XS*----------------------------------------------------------------*
0410XS
0410XS     EXEC SQL
0410XS       SELECT
0410XS               MAX(B.HIDTFD_FLEXZ_AGPTO)
0410XS         INTO
0410XS              :GFCTB0B7.HIDTFD-FLEXZ-AGPTO
0410XS         FROM
0410XS               DB2PRD.PDIDO_FLEXZ_AGPTO A,
0410XS               DB2PRD.PDIDO_FLEXZ_MUN   B
0410XS        WHERE
0410XS               A.CAGPTO_CTA       = B.CAGPTO_CTA
0410XS          AND  A.CSERVC_TARIF     = B.CSERVC_TARIF
0410XS          AND  A.HIDTFD_FLEXZ_AGPTO
0410XS                                  = B.HIDTFD_FLEXZ_AGPTO
0410XS          AND (A.CAGPTO_CTA       = :GFCTB0B2.CAGPTO-CTA
0410XS          AND  A.CSERVC_TARIF     = :GFCTB0B2.CSERVC-TARIF
0410XS          AND  A.DFIM_FLEXZ_AGPTO
0410XS                                 >= :GFCTB0B2.DFIM-FLEXZ-AGPTO)
0410XS          AND (B.CAGPTO_CTA       = :GFCTB0B7.CAGPTO-CTA
0410XS          AND  B.CSERVC_TARIF     = :GFCTB0B7.CSERVC-TARIF
0410XS          AND  B.CMUN_IBGE        = :GFCTB0B7.CMUN-IBGE)
0410XS     END-EXEC.
0410XS
0410XS     IF (SQLCODE                 NOT EQUAL ZEROS AND -305) OR
0410XS        (SQLWARN0                EQUAL 'W')
0410XS         MOVE  SPACES            TO GFCT0M-ERRO-SQL
0410XS
0410XS         INITIALIZE                 GFCT0M-ERRO-SQL
0410XS
0410XS         MOVE 'PDIDO_FLEXZ_MUN'  TO GFCT0M-NOME-TAB
0410XS         MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
0410XS         MOVE '0440'             TO GFCT0M-LOCAL
0410XS
0410XS         MOVE  0010              TO GFCTNA-COD-MSG-ERRO
0410XS
0410XS         MOVE '0010'             TO GFCTG2-COD-MSG
0410XS
0410XS         PERFORM 9900-OBTER-DESC-MSG
0410XS
0410XS         MOVE ' - GFCTB0B7'      TO WRK-NOME-TAB
0410XS
0410XS         PERFORM 9000-MOVER-ERRO-SQL
0410XS
0410XS         PERFORM 9999-FINALIZAR
0410XS     END-IF.
0410XS
0410XS     IF  SQLCODE                 EQUAL -305
0410XS         MOVE '0001-01-01-01.01.01.000001'
0410XS                                 TO HIDTFD-FLEXZ-AGPTO
0410XS                                                      OF GFCTB0B7
0410XS     END-IF.
0410XS
0410XS*----------------------------------------------------------------*
0410XS 3261-99-FIM.                    EXIT.
0410XS*----------------------------------------------------------------*
0410XS
0410XS******************************************************************
0410XS*ROTINA PARA CONSISTIR PEDIDO UF                                 *
0410XS******************************************************************
0410XS*----------------------------------------------------------------*
0410XS 3270-CONSISTIR-PED-UF           SECTION.
0410XS*----------------------------------------------------------------*
0410XS
0410XS     MOVE DPROCM-ATUAL OF GFCTB0A1
0410XS                                 TO DINIC-FLEXZ-AGPTO OF GFCTB0B2
0410XS                                    DFIM-FLEXZ-AGPTO  OF GFCTB0B2.
0410XS
0410XS     MOVE GFCTN9-AGRUPAMENTO     TO CAGPTO-CTA        OF GFCTB0B2
0410XS                                    CAGPTO-CTA        OF GFCTB0C1.
0410XS
0410XS     MOVE WRK-COD-TARIFA         TO CSERVC-TARIF      OF GFCTB0B2
0410XS                                    CSERVC-TARIF      OF GFCTB0C1.
0410XS
0410XS     MOVE GFCTN9-UF              TO CSGL-UF           OF GFCTB0C1.
0410XS
0410XS     PERFORM 3271-OBTER-MAX-FLEXIBILIZACAO.
0410XS
0410XS     EXEC SQL
0410XS       SELECT
0410XS               A.CSIT_SOLTC_FLEXZ
0410XS         INTO
0410XS              :GFCTB0B2.CSIT-SOLTC-FLEXZ
0410XS         FROM
0410XS               DB2PRD.PDIDO_FLEXZ_AGPTO A,
0410XS               DB2PRD.PDIDO_FLEXZ_UF    B
0410XS        WHERE
0410XS               A.CAGPTO_CTA       = B.CAGPTO_CTA
0410XS          AND  A.CSERVC_TARIF     = B.CSERVC_TARIF
0410XS          AND  A.HIDTFD_FLEXZ_AGPTO
0410XS                                  = B.HIDTFD_FLEXZ_AGPTO
0410XS          AND (A.CAGPTO_CTA       = :GFCTB0B2.CAGPTO-CTA
0410XS          AND  A.CSERVC_TARIF     = :GFCTB0B2.CSERVC-TARIF
0410XS          AND  A.DFIM_FLEXZ_AGPTO
0410XS                                 >= :GFCTB0B2.DFIM-FLEXZ-AGPTO)
0410XS          AND (B.CAGPTO_CTA       = :GFCTB0C1.CAGPTO-CTA
0410XS          AND  B.CSERVC_TARIF     = :GFCTB0C1.CSERVC-TARIF
0410XS          AND  B.CSGL_UF          = :GFCTB0C1.CSGL-UF
0410XS          AND  B.HIDTFD_FLEXZ_AGPTO
0410XS                                  = :GFCTB0C1.HIDTFD-FLEXZ-AGPTO)
0410XS     END-EXEC.
0410XS
0410XS     IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
0410XS        (SQLWARN0                EQUAL 'W')
0410XS         MOVE  SPACES            TO GFCT0M-ERRO-SQL
0410XS
0410XS         INITIALIZE                 GFCT0M-ERRO-SQL
0410XS
0410XS         MOVE 'PDIDO_FLEXZ_AGPTO'
0410XS                                 TO GFCT0M-NOME-TAB
0410XS         MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
0410XS         MOVE '0450'             TO GFCT0M-LOCAL
0410XS
0410XS         MOVE  0010              TO GFCTNA-COD-MSG-ERRO
0410XS
0410XS         MOVE '0010'             TO GFCTG2-COD-MSG
0410XS
0410XS         PERFORM 9900-OBTER-DESC-MSG
0410XS
0410XS         MOVE ' - GFCTB0B2'      TO WRK-NOME-TAB
0410XS
0410XS         PERFORM 9000-MOVER-ERRO-SQL
0410XS
0410XS         PERFORM 9999-FINALIZAR
0410XS     END-IF.
0410XS
0410XS     IF  SQLCODE                 EQUAL ZEROS
0410XS         IF  CSIT-SOLTC-FLEXZ OF GFCTB0B2
0410XS                                 EQUAL 1 OR
0410XS             CSIT-SOLTC-FLEXZ OF GFCTB0B2
0410XS                                 EQUAL 2 OR
0410XS             CSIT-SOLTC-FLEXZ OF GFCTB0B2
0410XS                                 EQUAL 5 OR
0410XS             CSIT-SOLTC-FLEXZ OF GFCTB0B2
0410XS                                 EQUAL 7 OR
0410XS             CSIT-SOLTC-FLEXZ OF GFCTB0B2
0410XS                                 EQUAL 8
0410XS             MOVE 'N'            TO WRK-TARIFA-OK
0410XS         END-IF
0410XS     END-IF.
0410XS
0410XS*----------------------------------------------------------------*
0410XS 3270-99-FIM.                    EXIT.
0410XS*----------------------------------------------------------------*
0410XS
0410XS******************************************************************
0410XS*ROTINA PARA OBTER ULTIMO PEDIDO FLEXIBILIZACAO UF               *
0410XS******************************************************************
0410XS*----------------------------------------------------------------*
0410XS 3271-OBTER-MAX-FLEXIBILIZACAO   SECTION.
0410XS*----------------------------------------------------------------*
0410XS
0410XS     EXEC SQL
0410XS       SELECT
0410XS               MAX(B.HIDTFD_FLEXZ_AGPTO)
0410XS         INTO
0410XS              :GFCTB0C1.HIDTFD-FLEXZ-AGPTO
0410XS         FROM
0410XS               DB2PRD.PDIDO_FLEXZ_AGPTO A,
0410XS               DB2PRD.PDIDO_FLEXZ_UF    B
0410XS        WHERE
0410XS               A.CAGPTO_CTA       = B.CAGPTO_CTA
0410XS          AND  A.CSERVC_TARIF     = B.CSERVC_TARIF
0410XS          AND  A.HIDTFD_FLEXZ_AGPTO
0410XS                                  = B.HIDTFD_FLEXZ_AGPTO
0410XS          AND (A.CAGPTO_CTA       = :GFCTB0B2.CAGPTO-CTA
0410XS          AND  A.CSERVC_TARIF     = :GFCTB0B2.CSERVC-TARIF
0410XS          AND  A.DFIM_FLEXZ_AGPTO
0410XS                                 >= :GFCTB0B2.DFIM-FLEXZ-AGPTO)
0410XS          AND (B.CAGPTO_CTA       = :GFCTB0C1.CAGPTO-CTA
0410XS          AND  B.CSERVC_TARIF     = :GFCTB0C1.CSERVC-TARIF
0410XS          AND  B.CSGL_UF          = :GFCTB0C1.CSGL-UF)
0410XS     END-EXEC.
0410XS
0410XS     IF (SQLCODE                 NOT EQUAL ZEROS AND -305) OR
0410XS        (SQLWARN0                EQUAL 'W')
0410XS         MOVE  SPACES            TO GFCT0M-ERRO-SQL
0410XS
0410XS         INITIALIZE                 GFCT0M-ERRO-SQL
0410XS
0410XS         MOVE 'PDIDO_FLEXZ_UF'   TO GFCT0M-NOME-TAB
0410XS         MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
0410XS         MOVE '0460'             TO GFCT0M-LOCAL
0410XS
0410XS         MOVE  0010              TO GFCTNA-COD-MSG-ERRO
0410XS
0410XS         MOVE '0010'             TO GFCTG2-COD-MSG
0410XS
0410XS         PERFORM 9900-OBTER-DESC-MSG
0410XS
0410XS         MOVE ' - GFCTB0C1'      TO WRK-NOME-TAB
0410XS
0410XS         PERFORM 9000-MOVER-ERRO-SQL
0410XS
0410XS         PERFORM 9999-FINALIZAR
0410XS     END-IF.
0410XS
0410XS     IF  SQLCODE                 EQUAL -305
0410XS         MOVE '0001-01-01-01.01.01.000001'
0410XS                                 TO HIDTFD-FLEXZ-AGPTO
0410XS                                                      OF GFCTB0C1
0410XS     END-IF.
0410XS
0410XS*----------------------------------------------------------------*
0410XS 3271-99-FIM.                    EXIT.
0410XS*----------------------------------------------------------------*
0410XS
0410XS******************************************************************
0410XS*ROTINA PARA CONSISTIR PEDIDO FUNCIONARIO BRADESCO               *
0410XS******************************************************************
0410XS*----------------------------------------------------------------*
0410XS 3280-CONSISTIR-PED-FUNC-BRAD    SECTION.
0410XS*----------------------------------------------------------------*
0410XS
0410XS     MOVE DPROCM-ATUAL OF GFCTB0A1
0410XS                                 TO DINIC-FLEXZ-AGPTO OF GFCTB0B2
0410XS                                    DFIM-FLEXZ-AGPTO  OF GFCTB0B2.
0410XS
0410XS     MOVE GFCTN9-AGRUPAMENTO     TO CAGPTO-CTA        OF GFCTB0B2.
0410XS
0410XS     MOVE WRK-COD-TARIFA         TO CSERVC-TARIF      OF GFCTB0B2.
0410XS
0410XS     PERFORM 3281-OBTER-MAX-FLEXIBILIZACAO.
0410XS
0410XS     EXEC SQL
0410XS       SELECT
0410XS               CSIT_SOLTC_FLEXZ
0410XS         INTO
0410XS              :GFCTB0B2.CSIT-SOLTC-FLEXZ
0410XS         FROM
0410XS               DB2PRD.PDIDO_FLEXZ_AGPTO
0410XS        WHERE
0410XS               CAGPTO_CTA         = :GFCTB0B2.CAGPTO-CTA
0410XS          AND  CSERVC_TARIF       = :GFCTB0B2.CSERVC-TARIF
0410XS          AND  DFIM_FLEXZ_AGPTO  >= :GFCTB0B2.DFIM-FLEXZ-AGPTO
0410XS          AND  HIDTFD_FLEXZ_AGPTO = :GFCTB0B2.HIDTFD-FLEXZ-AGPTO
0410XS     END-EXEC.
0410XS
0410XS     IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
0410XS        (SQLWARN0                EQUAL 'W')
0410XS         MOVE  SPACES            TO GFCT0M-ERRO-SQL
0410XS
0410XS         INITIALIZE                 GFCT0M-ERRO-SQL
0410XS
0410XS         MOVE 'PDIDO_FLEXZ_AGPTO'
0410XS                                 TO GFCT0M-NOME-TAB
0410XS         MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
0410XS         MOVE '0470'             TO GFCT0M-LOCAL
0410XS
0410XS         MOVE  0010              TO GFCTNA-COD-MSG-ERRO
0410XS
0410XS         MOVE '0010'             TO GFCTG2-COD-MSG
0410XS
0410XS         PERFORM 9900-OBTER-DESC-MSG
0410XS
0410XS         MOVE ' - GFCTB0B2'      TO WRK-NOME-TAB
0410XS
0410XS         PERFORM 9000-MOVER-ERRO-SQL
0410XS
0410XS         PERFORM 9999-FINALIZAR
0410XS     END-IF.
0410XS
0410XS     IF  SQLCODE                 EQUAL ZEROS
0410XS         IF  CSIT-SOLTC-FLEXZ OF GFCTB0B2
0410XS                                 EQUAL 1 OR
0410XS             CSIT-SOLTC-FLEXZ OF GFCTB0B2
0410XS                                 EQUAL 2 OR
0410XS             CSIT-SOLTC-FLEXZ OF GFCTB0B2
0410XS                                 EQUAL 5 OR
0410XS             CSIT-SOLTC-FLEXZ OF GFCTB0B2
0410XS                                 EQUAL 7 OR
0410XS             CSIT-SOLTC-FLEXZ OF GFCTB0B2
0410XS                                 EQUAL 8
0410XS             MOVE 'N'            TO WRK-TARIFA-OK
0410XS         END-IF
0410XS     END-IF.
0410XS
0410XS*----------------------------------------------------------------*
0410XS 3280-99-FIM.                    EXIT.
0410XS*----------------------------------------------------------------*
0410XS
0410XS******************************************************************
0410XS*ROTINA PARA OBTER ULTIMO PEDIDO FLEXIBILIZACAO FUNC BRADESCO    *
0410XS******************************************************************
0410XS*----------------------------------------------------------------*
0410XS 3281-OBTER-MAX-FLEXIBILIZACAO   SECTION.
0410XS*----------------------------------------------------------------*
0410XS
0410XS     EXEC SQL
0410XS       SELECT
0410XS               MAX(HIDTFD_FLEXZ_AGPTO)
0410XS         INTO
0410XS              :GFCTB0B2.HIDTFD-FLEXZ-AGPTO
0410XS         FROM
0410XS               DB2PRD.PDIDO_FLEXZ_AGPTO
0410XS        WHERE
0410XS               CAGPTO_CTA         = :GFCTB0B2.CAGPTO-CTA
0410XS          AND  CSERVC_TARIF       = :GFCTB0B2.CSERVC-TARIF
0410XS          AND  DFIM_FLEXZ_AGPTO  >= :GFCTB0B2.DFIM-FLEXZ-AGPTO
0410XS     END-EXEC.
0410XS
0410XS     IF (SQLCODE                 NOT EQUAL ZEROS AND -305) OR
0410XS        (SQLWARN0                EQUAL 'W')
0410XS         MOVE  SPACES            TO GFCT0M-ERRO-SQL
0410XS
0410XS         INITIALIZE                 GFCT0M-ERRO-SQL
0410XS
0410XS         MOVE 'PDIDO_FLEXZ_AGPTO'
0410XS                                 TO GFCT0M-NOME-TAB
0410XS         MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
0410XS         MOVE '0480'             TO GFCT0M-LOCAL
0410XS
0410XS         MOVE  0010              TO GFCTNA-COD-MSG-ERRO
0410XS
0410XS         MOVE '0010'             TO GFCTG2-COD-MSG
0410XS
0410XS         PERFORM 9900-OBTER-DESC-MSG
0410XS
0410XS         MOVE ' - GFCTB0B2'      TO WRK-NOME-TAB
0410XS
0410XS         PERFORM 9000-MOVER-ERRO-SQL
0410XS
0410XS         PERFORM 9999-FINALIZAR
0410XS     END-IF.
0410XS
0410XS     IF  SQLCODE                 EQUAL -305
0410XS         MOVE '0001-01-01-01.01.01.000001'
0410XS                                 TO HIDTFD-FLEXZ-AGPTO
0410XS                                                      OF GFCTB0B2
0410XS     END-IF.
0410XS
0410XS*----------------------------------------------------------------*
0410XS 3281-99-FIM.                    EXIT.
0410XS*----------------------------------------------------------------*
0410XS
0410XS******************************************************************
0410XS*ROTINA PARA CONSISTIR PEDIDO RAZAO                              *
0410XS******************************************************************
0410XS*----------------------------------------------------------------*
0410XS 3290-CONSISTIR-PED-RAZAO        SECTION.
0410XS*----------------------------------------------------------------*
0410XS
0410XS     MOVE DPROCM-ATUAL OF GFCTB0A1
0410XS                                 TO DINIC-FLEXZ-AGPTO OF GFCTB0B2
0410XS                                    DFIM-FLEXZ-AGPTO  OF GFCTB0B2.
0410XS
0410XS     MOVE GFCTN9-AGRUPAMENTO     TO CAGPTO-CTA        OF GFCTB0B2
0410XS                                    CAGPTO-CTA        OF GFCTB0M7.
0410XS
0410XS     MOVE WRK-COD-TARIFA         TO CSERVC-TARIF      OF GFCTB0B2
0410XS                                    CSERVC-TARIF      OF GFCTB0M7.
0410XS
0410XS     MOVE GFCTN9-GRUPO-CTBIL     TO CGRP-CTBIL-ATUAL  OF GFCTB0M7.
0410XS     MOVE GFCTN9-SUB-GRUPO-CTBIL TO CSGRP-CTBIL       OF GFCTB0M7.
0410XS
0410XS     PERFORM 3291-OBTER-MAX-FLEXIBILIZACAO.
0410XS
0410XS     EXEC SQL
0410XS       SELECT
0710XS               A.CSIT_SOLTC_FLEXZ,
0710XS               A.DFIM_FLEXZ_AGPTO
0410XS         INTO
0710XS              :GFCTB0B2.CSIT-SOLTC-FLEXZ,
0710XS              :GFCTB0B2.DFIM-FLEXZ-AGPTO
0410XS         FROM
0410XS               DB2PRD.PDIDO_FLEXZ_AGPTO A,
0410XS               DB2PRD.TPDIDO_FLEXZ_RZ   B
0410XS        WHERE
0410XS               A.CAGPTO_CTA       = B.CAGPTO_CTA
0410XS          AND  A.CSERVC_TARIF     = B.CSERVC_TARIF
0410XS          AND  A.HIDTFD_FLEXZ_AGPTO
0410XS                                  = B.HIDTFD_FLEXZ_AGPTO
0410XS          AND (A.CAGPTO_CTA       = :GFCTB0B2.CAGPTO-CTA
0410XS          AND  A.CSERVC_TARIF     = :GFCTB0B2.CSERVC-TARIF
0410XS          AND  A.DFIM_FLEXZ_AGPTO
0410XS                                 >= :GFCTB0B2.DFIM-FLEXZ-AGPTO)
0410XS          AND (B.CAGPTO_CTA       = :GFCTB0M7.CAGPTO-CTA
0410XS          AND  B.CSERVC_TARIF     = :GFCTB0M7.CSERVC-TARIF
0410XS          AND  B.CGRP_CTBIL_ATUAL = :GFCTB0M7.CGRP-CTBIL-ATUAL
0410XS          AND  B.CSGRP_CTBIL      = :GFCTB0M7.CSGRP-CTBIL
0410XS          AND  B.HIDTFD_FLEXZ_AGPTO
0410XS                                  = :GFCTB0M7.HIDTFD-FLEXZ-AGPTO)
0410XS     END-EXEC.
0410XS
0410XS     IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
0410XS        (SQLWARN0                EQUAL 'W')
0410XS         MOVE  SPACES            TO GFCT0M-ERRO-SQL
0410XS
0410XS         INITIALIZE                 GFCT0M-ERRO-SQL
0410XS
0410XS         MOVE 'PDIDO_FLEXZ_AGPTO'
0410XS                                 TO GFCT0M-NOME-TAB
0410XS         MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
0410XS         MOVE '0490'             TO GFCT0M-LOCAL
0410XS
0410XS         MOVE  0010              TO GFCTNA-COD-MSG-ERRO
0410XS
0410XS         MOVE '0010'             TO GFCTG2-COD-MSG
0410XS
0410XS         PERFORM 9900-OBTER-DESC-MSG
0410XS
0410XS         MOVE ' - GFCTB0B2'      TO WRK-NOME-TAB
0410XS
0410XS         PERFORM 9000-MOVER-ERRO-SQL
0410XS
0410XS         PERFORM 9999-FINALIZAR
0410XS     END-IF.
0410XS
0410XS     IF  SQLCODE                 EQUAL ZEROS
0410XS         MOVE DFIM-FLEXZ-AGPTO OF GFCTB0B2
0410XS                                 TO WRK-DATA-DB2
0410XS         MOVE WRK-DIA-DB2        TO WRK-DIA-INV
0410XS         MOVE WRK-MES-DB2        TO WRK-MES-INV
0410XS         MOVE WRK-ANO-DB2        TO WRK-ANO-INV
0410XS         MOVE WRK-DATA-INV       TO WRK-DT-FIM-FLEX-INV
0410XS
0410XS         IF (CSIT-SOLTC-FLEXZ OF GFCTB0B2
0410XS                                 EQUAL 1 OR
0410XS             CSIT-SOLTC-FLEXZ OF GFCTB0B2
0410XS                                 EQUAL 2 OR
0410XS             CSIT-SOLTC-FLEXZ OF GFCTB0B2
0410XS                                 EQUAL 5 OR
0410XS             CSIT-SOLTC-FLEXZ OF GFCTB0B2
0410XS                                 EQUAL 7 OR
0410XS             CSIT-SOLTC-FLEXZ OF GFCTB0B2
0410XS                                 EQUAL 8) AND
0710XS             WRK-DT-FIM-FLEX-INV NOT LESS WRK-DT-PROCM-INV
0410XS             MOVE 'N'            TO WRK-TARIFA-OK
0410XS         END-IF
0410XS     END-IF.
0410XS
0410XS*----------------------------------------------------------------*
0410XS 3290-99-FIM.                    EXIT.
0410XS*----------------------------------------------------------------*
0410XS
0410XS******************************************************************
0410XS*ROTINA PARA OBTER ULTIMO PEDIDO FLEXIBILIZACAO RAZAO            *
0410XS******************************************************************
0410XS*----------------------------------------------------------------*
0410XS 3291-OBTER-MAX-FLEXIBILIZACAO   SECTION.
0410XS*----------------------------------------------------------------*
0410XS
0410XS     EXEC SQL
0410XS       SELECT
0410XS               MAX(B.HIDTFD_FLEXZ_AGPTO)
0410XS         INTO
0410XS              :GFCTB0M7.HIDTFD-FLEXZ-AGPTO
0410XS         FROM
0410XS               DB2PRD.PDIDO_FLEXZ_AGPTO A,
0410XS               DB2PRD.TPDIDO_FLEXZ_RZ   B
0410XS        WHERE
0410XS               A.CAGPTO_CTA       = B.CAGPTO_CTA
0410XS          AND  A.CSERVC_TARIF     = B.CSERVC_TARIF
0410XS          AND  A.HIDTFD_FLEXZ_AGPTO
0410XS                                  = B.HIDTFD_FLEXZ_AGPTO
0410XS          AND (A.CAGPTO_CTA       = :GFCTB0B2.CAGPTO-CTA
0410XS          AND  A.CSERVC_TARIF     = :GFCTB0B2.CSERVC-TARIF
0410XS          AND  A.DFIM_FLEXZ_AGPTO
0410XS                                 >= :GFCTB0B2.DFIM-FLEXZ-AGPTO)
0410XS          AND (B.CAGPTO_CTA       = :GFCTB0M7.CAGPTO-CTA
0410XS          AND  B.CSERVC_TARIF     = :GFCTB0M7.CSERVC-TARIF
0410XS          AND  B.CGRP_CTBIL_ATUAL = :GFCTB0M7.CGRP-CTBIL-ATUAL
0410XS          AND  B.CSGRP_CTBIL      = :GFCTB0M7.CSGRP-CTBIL)
0410XS     END-EXEC.
0410XS
0410XS     IF (SQLCODE                 NOT EQUAL ZEROS AND -305) OR
0410XS        (SQLWARN0                EQUAL 'W')
0410XS         MOVE  SPACES            TO GFCT0M-ERRO-SQL
0410XS
0410XS         INITIALIZE                 GFCT0M-ERRO-SQL
0410XS
0410XS         MOVE 'TPDIDO_FLEXZ_RZ'  TO GFCT0M-NOME-TAB
0410XS         MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
0410XS         MOVE '0500'             TO GFCT0M-LOCAL
0410XS
0410XS         MOVE  0010              TO GFCTNA-COD-MSG-ERRO
0410XS
0410XS         MOVE '0010'             TO GFCTG2-COD-MSG
0410XS
0410XS         PERFORM 9900-OBTER-DESC-MSG
0410XS
0410XS         MOVE ' - GFCTB0M7'      TO WRK-NOME-TAB
0410XS
0410XS         PERFORM 9000-MOVER-ERRO-SQL
0410XS
0410XS         PERFORM 9999-FINALIZAR
0410XS     END-IF.
0410XS
0410XS     IF  SQLCODE                 EQUAL -305
0410XS         MOVE '0001-01-01-01.01.01.000001'
0410XS                                 TO HIDTFD-FLEXZ-AGPTO
0410XS                                                      OF GFCTB0M7
0410XS     END-IF.
0410XS
0410XS*----------------------------------------------------------------*
0410XS 3291-99-FIM.                    EXIT.
0410XS*----------------------------------------------------------------*
0410XS
0410XS******************************************************************
0410XS*ROTINA PARA CONSISTIR PEDIDO AGRUPAMENTO PERSONALIZADO          *
0410XS******************************************************************
0410XS*----------------------------------------------------------------*
0410XS 3300-CONS-PED-AGRUP-PERSONALIZ  SECTION.
0410XS*----------------------------------------------------------------*
0410XS
0410XS     MOVE DPROCM-ATUAL OF GFCTB0A1
0410XS                                 TO DINIC-FLEXZ-AGPTO OF GFCTB0B2
0410XS                                    DFIM-FLEXZ-AGPTO  OF GFCTB0B2.
0410XS
0410XS     MOVE GFCTN9-AGRUPAMENTO     TO CAGPTO-CTA        OF GFCTB0B2.
0410XS
0410XS     MOVE WRK-COD-TARIFA         TO CSERVC-TARIF      OF GFCTB0B2.
0410XS
0410XS     PERFORM 3301-OBTER-MAX-FLEXIBILIZACAO.
0410XS
0410XS     EXEC SQL
0410XS       SELECT
0410XS               CSIT_SOLTC_FLEXZ
0410XS         INTO
0410XS              :GFCTB0B2.CSIT-SOLTC-FLEXZ
0410XS         FROM
0410XS               DB2PRD.PDIDO_FLEXZ_AGPTO
0410XS        WHERE
0410XS               CAGPTO_CTA         = :GFCTB0B2.CAGPTO-CTA
0410XS          AND  CSERVC_TARIF       = :GFCTB0B2.CSERVC-TARIF
0410XS          AND  DFIM_FLEXZ_AGPTO  >= :GFCTB0B2.DFIM-FLEXZ-AGPTO
0410XS          AND  HIDTFD_FLEXZ_AGPTO = :GFCTB0B2.HIDTFD-FLEXZ-AGPTO
0410XS     END-EXEC.
0410XS
0410XS     IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
0410XS        (SQLWARN0                EQUAL 'W')
0410XS         MOVE  SPACES            TO GFCT0M-ERRO-SQL
0410XS
0410XS         INITIALIZE                 GFCT0M-ERRO-SQL
0410XS
0410XS         MOVE 'PDIDO_FLEXZ_AGPTO'
0410XS                                 TO GFCT0M-NOME-TAB
0410XS         MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
0410XS         MOVE '0510'             TO GFCT0M-LOCAL
0410XS
0410XS         MOVE  0010              TO GFCTNA-COD-MSG-ERRO
0410XS
0410XS         MOVE '0010'             TO GFCTG2-COD-MSG
0410XS
0410XS         PERFORM 9900-OBTER-DESC-MSG
0410XS
0410XS         MOVE ' - GFCTB0B2'      TO WRK-NOME-TAB
0410XS
0410XS         PERFORM 9000-MOVER-ERRO-SQL
0410XS
0410XS         PERFORM 9999-FINALIZAR
0410XS     END-IF.
0410XS
0410XS     IF  SQLCODE                 EQUAL ZEROS
0410XS         IF  CSIT-SOLTC-FLEXZ OF GFCTB0B2
0410XS                                 EQUAL 1 OR
0410XS             CSIT-SOLTC-FLEXZ OF GFCTB0B2
0410XS                                 EQUAL 2 OR
0410XS             CSIT-SOLTC-FLEXZ OF GFCTB0B2
0410XS                                 EQUAL 5 OR
0410XS             CSIT-SOLTC-FLEXZ OF GFCTB0B2
0410XS                                 EQUAL 7 OR
0410XS             CSIT-SOLTC-FLEXZ OF GFCTB0B2
0410XS                                 EQUAL 8
0410XS             MOVE 'N'            TO WRK-TARIFA-OK
0410XS         END-IF
0410XS     END-IF.
0410XS
0410XS*----------------------------------------------------------------*
0410XS 3300-99-FIM.                    EXIT.
0410XS*----------------------------------------------------------------*
0410XS
0410XS******************************************************************
0410XS*ROTINA PARA OBTER ULTIMO PEDIDO FLEXIBILIZACAO AGRUP PERSONALIZ *
0410XS******************************************************************
0410XS*----------------------------------------------------------------*
0410XS 3301-OBTER-MAX-FLEXIBILIZACAO   SECTION.
0410XS*----------------------------------------------------------------*
0410XS
0410XS     EXEC SQL
0410XS       SELECT
0410XS               MAX(HIDTFD_FLEXZ_AGPTO)
0410XS         INTO
0410XS              :GFCTB0B2.HIDTFD-FLEXZ-AGPTO
0410XS         FROM
0410XS               DB2PRD.PDIDO_FLEXZ_AGPTO
0410XS        WHERE
0410XS               CAGPTO_CTA         = :GFCTB0B2.CAGPTO-CTA
0410XS          AND  CSERVC_TARIF       = :GFCTB0B2.CSERVC-TARIF
0410XS          AND  DFIM_FLEXZ_AGPTO  >= :GFCTB0B2.DFIM-FLEXZ-AGPTO
0410XS     END-EXEC.
0410XS
0410XS     IF (SQLCODE                 NOT EQUAL ZEROS AND -305) OR
0410XS        (SQLWARN0                EQUAL 'W')
0410XS         MOVE  SPACES            TO GFCT0M-ERRO-SQL
0410XS
0410XS         INITIALIZE                 GFCT0M-ERRO-SQL
0410XS
0410XS         MOVE 'PDIDO_FLEXZ_AGPTO'
0410XS                                 TO GFCT0M-NOME-TAB
0410XS         MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
0410XS         MOVE '0520'             TO GFCT0M-LOCAL
0410XS
0410XS         MOVE  0010              TO GFCTNA-COD-MSG-ERRO
0410XS
0410XS         MOVE '0010'             TO GFCTG2-COD-MSG
0410XS
0410XS         PERFORM 9900-OBTER-DESC-MSG
0410XS
0410XS         MOVE ' - GFCTB0B2'      TO WRK-NOME-TAB
0410XS
0410XS         PERFORM 9000-MOVER-ERRO-SQL
0410XS
0410XS         PERFORM 9999-FINALIZAR
0410XS     END-IF.
0410XS
0410XS     IF  SQLCODE                 EQUAL -305
0410XS         MOVE '0001-01-01-01.01.01.000001'
0410XS                                 TO HIDTFD-FLEXZ-AGPTO
0410XS                                                      OF GFCTB0B2
0410XS     END-IF.
0410XS
0410XS*----------------------------------------------------------------*
0410XS 3301-99-FIM.                    EXIT.
0410XS*----------------------------------------------------------------*
0410XS
0410XS******************************************************************
0410XS*ROTINA PARA CONSISTIR DATAS                                     *
0410XS******************************************************************
0410XS*----------------------------------------------------------------*
0410XS 3400-CONSISTIR-DATAS-VIGENCIAS  SECTION.
0410XS*----------------------------------------------------------------*
0410XS
0410XS     PERFORM 3410-ACESSAR-GFCT5018.
0410XS
0410XS     IF  WRK-TARIFA-OK           EQUAL 'S'
0410XS         MOVE WRK-5018-DINIC-VGCIA-TARIF-P
0410XS                                 TO WRK-DATA-DB2
0410XS         MOVE WRK-DIA-DB2        TO WRK-DIA-INV
0410XS         MOVE WRK-MES-DB2        TO WRK-MES-INV
0410XS         MOVE WRK-ANO-DB2        TO WRK-ANO-INV
0410XS         MOVE WRK-DATA-INV       TO WRK-DT-INIC-TARIF-INV
0410XS
0410XS         IF  WRK-DT-INIC-FLEX-INV
0410XS                                 LESS WRK-DT-INIC-TARIF-INV
0410XS             MOVE 'N'            TO WRK-TARIFA-OK
0410XS         END-IF
0410XS
0410XS         IF  WRK-TARIFA-OK       EQUAL 'S'
0410XS             MOVE WRK-5018-DFIM-VGCIA-TARIF
0410XS                                 TO WRK-DATA-DB2
0410XS             MOVE WRK-DIA-DB2    TO WRK-DIA-INV
0410XS             MOVE WRK-MES-DB2    TO WRK-MES-INV
0410XS             MOVE WRK-ANO-DB2    TO WRK-ANO-INV
0410XS             MOVE WRK-DATA-INV   TO WRK-DT-FIM-TARIF-INV
0410XS
0410XS             IF  WRK-DT-FIM-FLEX-INV
0410XS                                 GREATER WRK-DT-FIM-TARIF-INV
0410XS                 MOVE 'N'        TO WRK-TARIFA-OK
0410XS             END-IF
0410XS         END-IF
0410XS     END-IF.
0410XS
0410XS     IF  WRK-TARIFA-OK           EQUAL 'S'
0410XS         PERFORM 3420-ACESSAR-GFCTB0J7
0410XS
0410XS         IF  WRK-TARIFA-OK       EQUAL 'S'
0410XS             MOVE DINIC-PRMSS-FLEXZ OF GFCTB0J7
0410XS                                 TO WRK-DATA-DB2
0410XS             MOVE WRK-DIA-DB2    TO WRK-DIA-INV
0410XS             MOVE WRK-MES-DB2    TO WRK-MES-INV
0410XS             MOVE WRK-ANO-DB2    TO WRK-ANO-INV
0410XS             MOVE WRK-DATA-INV   TO WRK-DT-INIC-PRMSS-INV
0410XS
0410XS             IF  WRK-DT-INIC-FLEX-INV
0410XS                                 LESS WRK-DT-INIC-PRMSS-INV
0410XS                 MOVE 'N'        TO WRK-TARIFA-OK
0410XS             END-IF
0410XS
0410XS             IF  WRK-TARIFA-OK   EQUAL 'S'
0410XS                 MOVE DFIM-PRMSS-FLEXZ OF GFCTB0J7
0410XS                                 TO WRK-DATA-DB2
0410XS                 MOVE WRK-DIA-DB2
0410XS                                 TO WRK-DIA-INV
0410XS                 MOVE WRK-MES-DB2
0410XS                                 TO WRK-MES-INV
0410XS                 MOVE WRK-ANO-DB2
0410XS                                 TO WRK-ANO-INV
0410XS                 MOVE WRK-DATA-INV
0410XS                                 TO WRK-DT-FIM-PRMSS-INV
0410XS
0410XS                 IF  WRK-DT-FIM-FLEX-INV
0410XS                                 GREATER WRK-DT-FIM-PRMSS-INV
0410XS                     MOVE 'N'    TO WRK-TARIFA-OK
0410XS                 END-IF
0410XS             END-IF
0410XS         END-IF
0410XS     END-IF.
0410XS
0410XS*----------------------------------------------------------------*
0410XS 3400-99-FIM.                    EXIT.
0410XS*----------------------------------------------------------------*
0410XS
0410XS******************************************************************
0410XS*ROTINA PARA ACESSAR MODULO GFCT5018                             *
0410XS******************************************************************
0410XS*----------------------------------------------------------------*
0410XS 3410-ACESSAR-GFCT5018           SECTION.
0410XS*----------------------------------------------------------------*
0410XS
0410XS     INITIALIZE                  WRK-5018-AREA-ENTRADA
0410XS                                 WRK-5018-AREA-SAIDA
0410XS                                 WRK-5018-DADOS-RETORNO.
0410XS
0410XS     MOVE 'O'                    TO WRK-5018-AMBIENTE.
0410XS     MOVE WRK-COD-TARIFA         TO WRK-5018-CODIGO-TARIFA.
0410XS
0410XS     MOVE GFCTN9-INIC-VIG-FLEX   TO WRK-5018-DT-REFER.
0410XS
0410XS     MOVE 'GFCT5018'             TO WRK-MODULO.
0410XS
0410XS     CALL WRK-MODULO             USING WRK-5018-AREA-ENTRADA
0410XS                                       WRK-5018-AREA-SAIDA
0410XS                                       WRK-5018-DADOS-RETORNO
0410XS                                       WRK-IO-PCB
0410XS                                       WRK-ALT-PCB.
0410XS
0410XS     IF  WRK-5018-COD-RETORNO    NOT EQUAL ZEROS AND
0410XS         WRK-5018-COD-RETORNO    NOT EQUAL 02
0410XS         MOVE 9                  TO GFCTNA-ERRO
0410XS
0410XS         MOVE WRK-MODULO         TO WRK-NOME-MOD
0410XS         MOVE WRK-MSG01          TO GFCT0M-TEXTO
0410XS                                    GFCTNA-DESC-MSG-ERRO
0410XS
0410XS         MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
0410XS         MOVE 'GFCT0408'         TO GFCT0M-PROGRAMA
0410XS
0410XS         PERFORM 9999-FINALIZAR
0410XS     END-IF.
0410XS
0410XS     IF  WRK-5018-COD-RETORNO    EQUAL 02
0410XS         MOVE 'N'                TO WRK-TARIFA-OK
0410XS     END-IF.
0410XS
0410XS*----------------------------------------------------------------*
0410XS 3410-99-FIM.                    EXIT.
0410XS*----------------------------------------------------------------*
0410XS
0410XS******************************************************************
0410XS* SELECAO NA TABELA GFCTB0J7                                     *
0410XS******************************************************************
0410XS*----------------------------------------------------------------*
0410XS 3420-ACESSAR-GFCTB0J7           SECTION.
0410XS*----------------------------------------------------------------*
0410XS
0410XS     MOVE GFCTN9-AGRUPAMENTO     TO CAGPTO-CTA        OF GFCTB0J7.
0410XS     MOVE WRK-COD-TARIFA         TO CSERVC-TARIF      OF GFCTB0J7.
0410XS     MOVE ZEROS                  TO CFUNC-ENCRR-VGCIA OF GFCTB0J7.
0410XS
0410XS     EXEC SQL
0410XS       SELECT
0410XS               DINIC_PRMSS_FLEXZ,
0410XS               DFIM_PRMSS_FLEXZ
0410XS         INTO
0410XS              :GFCTB0J7.DINIC-PRMSS-FLEXZ,
0410XS              :GFCTB0J7.DFIM-PRMSS-FLEXZ
0410XS         FROM
0410XS               DB2PRD.TPRMSS_FLEXZ_AGPTO
0410XS        WHERE
0410XS               CAGPTO_CTA         = :GFCTB0J7.CAGPTO-CTA
0410XS          AND  CSERVC_TARIF       = :GFCTB0J7.CSERVC-TARIF
0410XS          AND  CFUNC_ENCRR_VGCIA  = :GFCTB0J7.CFUNC-ENCRR-VGCIA
0410XS     END-EXEC.
0410XS
0410XS     IF (SQLCODE                 NOT EQUAL ZEROS AND +100
0410XS                                                 AND -811) OR
0410XS        (SQLWARN0                EQUAL 'W')
0410XS         MOVE  SPACES            TO GFCT0M-ERRO-SQL
0410XS
0410XS         INITIALIZE                 GFCT0M-ERRO-SQL
0410XS
0410XS         MOVE 'TPRMSS_FLEXZ_AGPTO'
0410XS                                 TO GFCT0M-NOME-TAB
0410XS         MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
0410XS         MOVE '0530'             TO GFCT0M-LOCAL
0410XS
0410XS         MOVE  0010              TO GFCTNA-COD-MSG-ERRO
0410XS
0410XS         MOVE '0010'             TO GFCTG2-COD-MSG
0410XS
0410XS         PERFORM 9900-OBTER-DESC-MSG
0410XS
0410XS         MOVE ' - GFCTB0J7'      TO WRK-NOME-TAB
0410XS
0410XS         PERFORM 9000-MOVER-ERRO-SQL
0410XS
0410XS         PERFORM 9999-FINALIZAR
0410XS     END-IF.
0410XS
0410XS     IF  SQLCODE                 EQUAL +100
0410XS         MOVE 'N'                TO WRK-TARIFA-OK
0410XS     END-IF.
0410XS
0410XS*----------------------------------------------------------------*
0410XS 3420-99-FIM.                    EXIT.
0410XS*----------------------------------------------------------------*
0410XS
0410XS******************************************************************
0410XS*ROTINA PARA CONSISTIR OS PARAMETROS DA TARIFA                   *
0410XS******************************************************************
0410XS*----------------------------------------------------------------*
0410XS 3500-CONSISTIR-CAMPOS-TARIFA    SECTION.
0410XS*----------------------------------------------------------------*
0410XS
0410XS     PERFORM 3510-CONSISTIR-CAMPO-TARIFA.
0410XS
0410XS     IF  WRK-TARIFA-OK           EQUAL 'S'
0410XS
0410XS*--- PARA TARIFA PERCENTUAL NAO TEM VALOR
0410XS
0410XS         IF  GFCTN9-IND-VALOR    EQUAL 2
0410XS             PERFORM 3520-ACESSAR-GFCT5521
0410XS         END-IF
0410XS     END-IF.
0410XS
0410XS*----------------------------------------------------------------*
0410XS 3500-99-FIM.                    EXIT.
0410XS*----------------------------------------------------------------*
0410XS
0410XS******************************************************************
0410XS*ROTINA PARA CONSISTIR CAMPOS TARIFA                             *
0410XS******************************************************************
0410XS*----------------------------------------------------------------*
0410XS 3510-CONSISTIR-CAMPO-TARIFA     SECTION.
0410XS*----------------------------------------------------------------*
0410XS
BI0115     IF  (WRK-5018-CTPO-SERVC-TARIF
BI0115                                 EQUAL 2 OR 4) AND
0410XS         GFCTN9-FRANQUIA         NOT EQUAL ZEROS
0410XS         MOVE 'N'                TO WRK-TARIFA-OK
0410XS     END-IF.
0410XS
0410XS     IF  WRK-TARIFA-OK           EQUAL 'S'
0410XS         PERFORM
0410XS         VARYING IND-1           FROM 1 BY 1
0410XS           UNTIL IND-1           GREATER 50 OR
0410XS                 WRK-5018-DINIC-VGCIA-TARIF(IND-1)
0410XS                                 EQUAL SPACES
0410XS                 IF  WRK-5018-PSERVC-PTADO(IND-1)
0410XS                                 GREATER ZEROS AND
0410XS                     GFCTN9-VR-NEGOCIADO
0410XS                                 NOT EQUAL ZEROS
0410XS                     MOVE 'N'    TO WRK-TARIFA-OK
0410XS                 END-IF
0410XS
0410XS                 IF  WRK-TARIFA-OK
0410XS                                 EQUAL 'S'
0410XS                     IF  WRK-5018-CMOEDA-BACEN(IND-1)
0410XS                                 GREATER ZEROS AND
0410XS                         GFCTN9-VR-NEGOCIADO
0410XS                                 NOT EQUAL ZEROS
0410XS                         MOVE 'N'
0410XS                                 TO WRK-TARIFA-OK
0410XS                     END-IF
0410XS                 END-IF
0410XS         END-PERFORM
0410XS     END-IF.
0410XS
0410XS*----------------------------------------------------------------*
0410XS 3510-99-FIM.                    EXIT.
0410XS*----------------------------------------------------------------*
0410XS
0410XS******************************************************************
0410XS*ROTINA PARA ACESSAR GFCT5521                                    *
0410XS******************************************************************
0410XS*----------------------------------------------------------------*
0410XS 3520-ACESSAR-GFCT5521           SECTION.
0410XS*----------------------------------------------------------------*
0410XS
0410XS     INITIALIZE                  WRK-5521-AREA-ENTRADA
0410XS                                 WRK-5521-AREA-SAIDA
0410XS                                 GFCT0M-AREA-ERROS.
0410XS
0410XS     MOVE SPACES                 TO WRK-5521-AREA-ENTRADA
0410XS                                    WRK-5521-AREA-SAIDA
0410XS                                    GFCT0M-AREA-ERROS.
0410XS
0410XS     MOVE 'O'                    TO WRK-5521-AMBIENTE.
0410XS     MOVE 'GFCT0408'             TO WRK-5521-CHAMADOR.
0410XS     MOVE  WRK-COD-TARIFA        TO WRK-5521-COD-TARIFA.
0410XS     MOVE  1                     TO WRK-5521-QTD-EVN-AGRUP.
0410XS     MOVE  DPROCM-ATUAL OF GFCTB0A1
0410XS                                 TO WRK-5521-DATA-EVN.
0410XS     MOVE  GFCTN9-INIC-VIG-FLEX  TO WRK-5521-DATA-DEBITO.
0410XS     MOVE  ZEROS                 TO WRK-5521-VALOR-EVN
0410XS                                    WRK-5521-TIPO-MOEDA-EVN.
0410XS     MOVE  SPACES                TO WRK-5521-DATA-CONV-EVN.
0410XS     MOVE  10                    TO WRK-5521-TIPO-MOVTO.
0410XS
0410XS     MOVE 'GFCT5521'             TO WRK-MODULO.
0410XS
0410XS     CALL WRK-MODULO             USING WRK-5521-AREA-ENTRADA
0410XS                                       WRK-5521-AREA-SAIDA
0410XS                                       GFCT0M-AREA-ERROS.
0410XS
0410XS     IF  WRK-5521-COD-RETORNO    NOT EQUAL ZEROS AND
0410XS         WRK-5521-COD-MSG-GFCT   EQUAL ZEROS
0410XS         MOVE  9                 TO GFCTNA-ERRO
0410XS
0410XS         MOVE  WRK-MODULO        TO WRK-NOME-MOD
0410XS         MOVE  WRK-MSG01         TO GFCT0M-TEXTO
0410XS                                    GFCTNA-DESC-MSG-ERRO
0410XS
0410XS         MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
0410XS         MOVE 'GFCT0408'         TO GFCT0M-PROGRAMA
0410XS
0410XS         PERFORM 9999-FINALIZAR
0410XS     END-IF.
0410XS
0410XS     IF  WRK-5521-COD-MSG-GFCT   NOT EQUAL ZEROS AND
0410XS         WRK-5521-COD-MSG-GFCT   NOT EQUAL 0476  AND
0410XS         WRK-5521-COD-MSG-GFCT   NOT EQUAL 0443
0410XS         MOVE 'N'                TO WRK-TARIFA-OK
0410XS     END-IF.
0410XS
0410XS     IF  WRK-TARIFA-OK           EQUAL 'S'
0410XS         IF  WRK-5521-COD-MSG-GFCT
0410XS                                 NOT EQUAL 0443
0410XS             PERFORM 3521-CONSISTIR-CAMPO-GFCT5521
0410XS         END-IF
0410XS     END-IF.
0410XS
0410XS*----------------------------------------------------------------*
0410XS 3520-99-FIM.                    EXIT.
0410XS*----------------------------------------------------------------*
0410XS
0410XS******************************************************************
0410XS*ROTINA PARA CONSISTIR CAMPO GFCT5521                            *
0410XS******************************************************************
0410XS*----------------------------------------------------------------*
0410XS 3521-CONSISTIR-CAMPO-GFCT5521   SECTION.
0410XS*----------------------------------------------------------------*
0410XS
0410XS     IF  WRK-5521-VLR-TOTAL-TARIFA
0410XS                                 NOT GREATER GFCTN9-VR-NEGOCIADO
0410XS         MOVE 'N'                TO WRK-TARIFA-OK
0410XS     END-IF.
0410XS
0410XS*----------------------------------------------------------------*
0410XS 3521-99-FIM.                    EXIT.
0410XS*----------------------------------------------------------------*
0410XS
0410XS*----------------------------------------------------------------*
0410XS* SELECAO NA TABELA GFCTB0J7 VALIDA PERMISS DA TARIFA            *
0410XS*----------------------------------------------------------------*
0410XS 3600-VERIFICAR-PERMISSAO        SECTION.
0410XS*----------------------------------------------------------------*
0410XS
0410XS     PERFORM 3610-ACESSAR-GFCTB0J7.
0410XS
0410XS     IF  WRK-TARIFA-OK           EQUAL 'S'
0410XS         MOVE DINIC-PRMSS-FLEXZ OF GFCTB0J7
0410XS                                 TO WRK-DATA-DB2
0410XS         MOVE WRK-DIA-DB2        TO WRK-DIA-INV
0410XS         MOVE WRK-MES-DB2        TO WRK-MES-INV
0410XS         MOVE WRK-ANO-DB2        TO WRK-ANO-INV
0410XS         MOVE WRK-DATA-INV       TO WRK-DT-INIC-PRMSS-INV
0410XS
0410XS         IF  WRK-DT-INIC-PRMSS-INV
0410XS                                 GREATER WRK-DT-PROCM-INV
0410XS             MOVE 'N'            TO WRK-TARIFA-OK
0410XS         END-IF
0410XS
0410XS         IF  WRK-TARIFA-OK       EQUAL 'S'
0410XS             MOVE DFIM-PRMSS-FLEXZ OF GFCTB0J7
0410XS                                 TO WRK-DATA-DB2
0410XS             MOVE WRK-DIA-DB2    TO WRK-DIA-INV
0410XS             MOVE WRK-MES-DB2    TO WRK-MES-INV
0410XS             MOVE WRK-ANO-DB2    TO WRK-ANO-INV
0410XS             MOVE WRK-DATA-INV   TO WRK-DT-FIM-PRMSS-INV
0410XS
0410XS             IF  WRK-DT-FIM-PRMSS-INV
0410XS                                 LESS WRK-DT-PROCM-INV
0410XS                 MOVE 'N'        TO WRK-TARIFA-OK
0410XS             END-IF
0410XS         END-IF
0410XS     END-IF.
0410XS
0410XS     IF  WRK-TARIFA-OK           EQUAL 'S'
0410XS         PERFORM 3620-IDENTIFICA-PARM-DIGITADO
0410XS
0410XS         MOVE 'N'                TO WRK-TEM-PERMISSAO
0410XS
0410XS         IF  CINDCD-PRMSS-AGNDA OF GFCTB0J7
0410XS                                 NOT EQUAL ZEROS AND
0410XS             WRK-PARM-PRMSS-AGNDA
0410XS                                 NOT EQUAL ZEROS
0410XS             MOVE 'S'            TO WRK-TEM-PERMISSAO
0410XS         END-IF
0410XS
0410XS         IF  CINDCD-PRMSS-VLR OF GFCTB0J7
0410XS                                 NOT EQUAL ZEROS AND
0410XS             WRK-PARM-PRMSS-VLR  NOT EQUAL ZEROS
0410XS             MOVE 'S'            TO WRK-TEM-PERMISSAO
0410XS         END-IF
0410XS
0410XS         IF  CINDCD-PRMSS-PERC OF GFCTB0J7
0410XS                                 NOT EQUAL ZEROS AND
0410XS             WRK-PARM-PRMSS-PERC NOT EQUAL ZEROS
0410XS             MOVE 'S'            TO WRK-TEM-PERMISSAO
0410XS         END-IF
0410XS
0410XS         IF  CINDCD-PRMSS-FRANQ OF GFCTB0J7
0410XS                                 NOT EQUAL ZEROS AND
0410XS             WRK-PARM-PRMSS-FRANQ
0410XS                                 NOT EQUAL ZEROS
0410XS             MOVE 'S'            TO WRK-TEM-PERMISSAO
0410XS         END-IF
0410XS
0410XS         IF  CINDCD-PRMSS-PERDC OF GFCTB0J7
0410XS                                 NOT EQUAL ZEROS AND
0410XS             WRK-PARM-PRMSS-PERDC
0410XS                                 NOT EQUAL ZEROS
0410XS             MOVE 'S'            TO WRK-TEM-PERMISSAO
0410XS         END-IF
0410XS
0410XS         MOVE WRK-TEM-PERMISSAO  TO WRK-TARIFA-OK
0410XS     END-IF.
0410XS
0410XS*----------------------------------------------------------------*
0410XS 3600-99-FIM.                    EXIT.
0410XS*----------------------------------------------------------------*
0410XS
0410XS******************************************************************
0410XS* SELECAO NA TABELA GFCTB0J7                                     *
0410XS******************************************************************
0410XS*----------------------------------------------------------------*
0410XS 3610-ACESSAR-GFCTB0J7           SECTION.
0410XS*----------------------------------------------------------------*
0410XS
0410XS     MOVE GFCTN9-AGRUPAMENTO     TO CAGPTO-CTA        OF GFCTB0J7.
0410XS
0410XS     MOVE WRK-COD-TARIFA         TO CSERVC-TARIF      OF GFCTB0J7.
0410XS
0410XS     MOVE ZEROS                  TO CFUNC-ENCRR-VGCIA OF GFCTB0J7.
0410XS
0410XS     EXEC SQL
0410XS       SELECT
0410XS               DINIC_PRMSS_FLEXZ,
0410XS               DFIM_PRMSS_FLEXZ,
0410XS               CINDCD_PRMSS_AGNDA,
0410XS               CINDCD_PRMSS_VLR,
0410XS               CINDCD_PRMSS_PERC,
0410XS               CINDCD_PRMSS_FRANQ,
0410XS               CINDCD_PRMSS_PERDC
0410XS         INTO
0410XS              :GFCTB0J7.DINIC-PRMSS-FLEXZ,
0410XS              :GFCTB0J7.DFIM-PRMSS-FLEXZ,
0410XS              :GFCTB0J7.CINDCD-PRMSS-AGNDA,
0410XS              :GFCTB0J7.CINDCD-PRMSS-VLR,
0410XS              :GFCTB0J7.CINDCD-PRMSS-PERC,
0410XS              :GFCTB0J7.CINDCD-PRMSS-FRANQ,
0410XS              :GFCTB0J7.CINDCD-PRMSS-PERDC
0410XS         FROM
0410XS               DB2PRD.TPRMSS_FLEXZ_AGPTO
0410XS        WHERE
0410XS               CAGPTO_CTA         = :GFCTB0J7.CAGPTO-CTA
0410XS          AND  CSERVC_TARIF       = :GFCTB0J7.CSERVC-TARIF
0410XS          AND  CFUNC_ENCRR_VGCIA  = :GFCTB0J7.CFUNC-ENCRR-VGCIA
0410XS     END-EXEC.
0410XS
0410XS     IF (SQLCODE                 NOT EQUAL ZEROS AND +100
0410XS                                                 AND -811) OR
0410XS        (SQLWARN0                EQUAL 'W')
0410XS         MOVE  SPACES            TO GFCT0M-ERRO-SQL
0410XS
0410XS         INITIALIZE                 GFCT0M-ERRO-SQL
0410XS
0410XS         MOVE 'TPRMSS_FLEXZ_AGPTO'
0410XS                                 TO GFCT0M-NOME-TAB
0410XS         MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
0410XS         MOVE '0540'             TO GFCT0M-LOCAL
0410XS
0410XS         MOVE  0010              TO GFCTNA-COD-MSG-ERRO
0410XS
0410XS         MOVE '0010'             TO GFCTG2-COD-MSG
0410XS
0410XS         PERFORM 9900-OBTER-DESC-MSG
0410XS
0410XS         MOVE ' - GFCTB0J7'      TO WRK-NOME-TAB
0410XS
0410XS         PERFORM 9000-MOVER-ERRO-SQL
0410XS
0410XS         PERFORM 9999-FINALIZAR
0410XS     END-IF.
0410XS
0410XS     IF  SQLCODE                 EQUAL +100
0410XS         MOVE 'N'                TO WRK-TARIFA-OK
0410XS     END-IF.
0410XS
0410XS*----------------------------------------------------------------*
0410XS 3610-99-FIM.                    EXIT.
0410XS*----------------------------------------------------------------*
0410XS
0410XS******************************************************************
0410XS* IDENTIFICACAO PARAMETROS FLEXIBILIZACAO DIGITADO               *
0410XS******************************************************************
0410XS*----------------------------------------------------------------*
0410XS 3620-IDENTIFICA-PARM-DIGITADO   SECTION.
0410XS*----------------------------------------------------------------*
0410XS
0410XS     IF  GFCTN9-DIA-AGEND        EQUAL ZEROS AND
0410XS         GFCTN9-DIAS-RETENCAO    EQUAL ZEROS
0410XS         MOVE ZERO               TO WRK-PARM-PRMSS-AGNDA
0410XS     ELSE
0410XS         MOVE 1                  TO WRK-PARM-PRMSS-AGNDA
0410XS     END-IF.
0410XS
0410XS     IF  GFCTN9-VR-NEGOCIADO     EQUAL ZEROS
0410XS         MOVE ZERO               TO WRK-PARM-PRMSS-VLR
0410XS     ELSE
0410XS         MOVE 1                  TO WRK-PARM-PRMSS-VLR
0410XS     END-IF.
0410XS
0410XS     IF  GFCTN9-PERC-DESCTO      EQUAL ZEROS
0410XS         MOVE ZERO               TO WRK-PARM-PRMSS-PERC
0410XS     ELSE
0410XS         MOVE 1                  TO WRK-PARM-PRMSS-PERC
0410XS     END-IF.
0410XS
0410XS     IF  GFCTN9-FRANQUIA         EQUAL ZEROS
0410XS         MOVE ZERO               TO WRK-PARM-PRMSS-FRANQ
0410XS     ELSE
0410XS         MOVE 1                  TO WRK-PARM-PRMSS-FRANQ
0410XS     END-IF.
0410XS
0410XS     IF  GFCTN9-PERIODO-COBR     EQUAL ZEROS
0410XS         MOVE ZERO               TO WRK-PARM-PRMSS-PERDC
0410XS     ELSE
0410XS         MOVE 1                  TO WRK-PARM-PRMSS-PERDC
0410XS     END-IF.
0410XS
0410XS*----------------------------------------------------------------*
0410XS 3620-99-FIM.                    EXIT.
0410XS*----------------------------------------------------------------*
0410XS
0410XS******************************************************************
0410XS*    RETORNA AO PROGRAMA CHAMADOR                                *
0410XS******************************************************************
0410XS*----------------------------------------------------------------*
0410XS 3700-ABRIR-CSR01                SECTION.
0410XS*----------------------------------------------------------------*
0410XS
0410XS     EXEC SQL
0410XS         OPEN
0410XS                CSR01-GFCTB0A2-JN
0410XS     END-EXEC.
0410XS
0410XS     IF (SQLCODE                 NOT EQUAL ZEROS) OR
0410XS        (SQLWARN0                EQUAL 'W')
0410XS         MOVE  SPACES            TO GFCT0M-ERRO-SQL
0410XS
0410XS         INITIALIZE                 GFCT0M-ERRO-SQL
0410XS
0410XS         MOVE 'PARM_SERVC_TARIF' TO GFCT0M-NOME-TAB
0410XS         MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
0410XS         MOVE '0550'             TO GFCT0M-LOCAL
0410XS
0410XS         MOVE  0005              TO GFCTNA-COD-MSG-ERRO
0410XS
0410XS         MOVE '0005'             TO GFCTG2-COD-MSG
0410XS
0410XS         PERFORM 9900-OBTER-DESC-MSG
0410XS
0410XS         MOVE ' - GFCTB0A2'      TO WRK-NOME-TAB
0410XS
0410XS         PERFORM 9000-MOVER-ERRO-SQL
0410XS
0410XS         PERFORM 9999-FINALIZAR
0410XS     END-IF.
0410XS
0410XS*----------------------------------------------------------------*
0410XS 3700-99-FIM.                    EXIT.
0410XS*----------------------------------------------------------------*
0410XS
0410XS******************************************************************
0410XS*    RETORNA AO PROGRAMA CHAMADOR                                *
0410XS******************************************************************
0410XS*----------------------------------------------------------------*
0410XS 3800-LER-CSR01                  SECTION.
0410XS*----------------------------------------------------------------*
0410XS
0410XS     EXEC SQL
0410XS        FETCH
0410XS                CSR01-GFCTB0A2-JN
0410XS         INTO
0410XS               :GFCTB0A2.CSERVC-TARIF
0410XS     END-EXEC.
0410XS
0410XS     IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
0410XS        (SQLWARN0                EQUAL 'W')
0410XS         MOVE  SPACES            TO GFCT0M-ERRO-SQL
0410XS
0410XS         INITIALIZE                 GFCT0M-ERRO-SQL
0410XS
0410XS         MOVE 'PARM_SERVC_TARIF' TO GFCT0M-NOME-TAB
0410XS         MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
0410XS         MOVE '0560'             TO GFCT0M-LOCAL
0410XS
0410XS         MOVE  0006              TO GFCTNA-COD-MSG-ERRO
0410XS
0410XS         MOVE '0006'             TO GFCTG2-COD-MSG
0410XS
0410XS         PERFORM 9900-OBTER-DESC-MSG
0410XS
0410XS         MOVE ' - GFCTB0A2'      TO WRK-NOME-TAB
0410XS
0410XS         PERFORM 9000-MOVER-ERRO-SQL
0410XS
0410XS         PERFORM 9999-FINALIZAR
0410XS     END-IF.
0410XS
0410XS     IF  SQLCODE                 EQUAL ZEROS
0410XS         MOVE CSERVC-TARIF OF GFCTB0A2
0410XS                                 TO WRK-CSERVC-TARIF
0410XS     ELSE
0410XS         MOVE 'S'                TO WRK-FIM-CURSOR
0410XS     END-IF.
0410XS
0410XS*----------------------------------------------------------------*
0410XS 3800-99-FIM.                    EXIT.
0410XS*----------------------------------------------------------------*
0410XS
0410XS******************************************************************
0410XS*    RETORNA AO PROGRAMA CHAMADOR                                *
0410XS******************************************************************
0410XS*----------------------------------------------------------------*
0410XS 3900-FECHAR-CSR01               SECTION.
0410XS*----------------------------------------------------------------*
0410XS
0410XS     EXEC SQL
0410XS        CLOSE
0410XS                CSR01-GFCTB0A2-JN
0410XS     END-EXEC.
0410XS
0410XS     IF (SQLCODE                 NOT EQUAL ZEROS) OR
0410XS        (SQLWARN0                EQUAL 'W')
0410XS         MOVE  SPACES            TO GFCT0M-ERRO-SQL
0410XS
0410XS         INITIALIZE                 GFCT0M-ERRO-SQL
0410XS
0410XS         MOVE 'PARM_SERVC_TARIF' TO GFCT0M-NOME-TAB
0410XS         MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
0410XS         MOVE '0570'             TO GFCT0M-LOCAL
0410XS
0410XS         MOVE  0011              TO GFCTNA-COD-MSG-ERRO
0410XS
0410XS         MOVE '0011'             TO GFCTG2-COD-MSG
0410XS
0410XS         PERFORM 9900-OBTER-DESC-MSG
0410XS
0410XS         MOVE ' - GFCTB0A2'      TO WRK-NOME-TAB
0410XS
0410XS         PERFORM 9000-MOVER-ERRO-SQL
0410XS
0410XS         PERFORM 9999-FINALIZAR
0410XS     END-IF.
0410XS
0410XS*----------------------------------------------------------------*
0410XS 3900-99-FIM.                    EXIT.
0410XS*----------------------------------------------------------------

      ******************************************************************
      *ROTINA PARA MOVER ERRO SQL                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       9000-MOVER-ERRO-SQL             SECTION.
      *----------------------------------------------------------------*

           MOVE  9                     TO GFCTNA-ERRO.

           MOVE 'GFCT0408'             TO GFCT0M-PROGRAMA.
           MOVE 'DB2'                  TO GFCT0M-TIPO-ACESSO.
           MOVE  SQLCA                 TO GFCT0M-SQLCA-AREA.
           MOVE  SQLCODE               TO GFCT0M-SQLCODE
                                          WRK-SQLCODE.

           MOVE  WRK-COD-SQL           TO GFCTNA-COD-SQL-ERRO.

           MOVE  SPACES                TO GFCTNA-DESC-MSG-ERRO.

           STRING GFCTG3-DESC-MSG
                  WRK-NOME-TAB
           DELIMITED BY '  '           INTO GFCTNA-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       9000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9900-OBTER-DESC-MSG             SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTG3-SAIDA.

           INITIALIZE GFCTG3-SAIDA.

           MOVE 100                    TO GFCTG2-LL.
           MOVE ZEROS                  TO GFCTG2-ZZ.
           MOVE GFCTN9-TRANSACAO       TO GFCTG2-TRANSACAO.
           MOVE GFCTN9-FUNCAO          TO GFCTG2-FUNCAO.
           MOVE GFCTN9-FUNC-BDSCO      TO GFCTG2-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTG2-FIM.
           MOVE 'O'                    TO GFCTG2-TIPO-PROC.

           MOVE 'GFCT5523'             TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCTG2-ENTRADA
                                             GFCTG3-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTNA-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-NOME-MOD
               MOVE WRK-MSG01          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0408'         TO GFCT0M-TRANSACAO

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 9
               MOVE 9                  TO GFCTNA-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-NOME-MOD
               MOVE WRK-MSG01          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0408'         TO GFCT0M-TRANSACAO

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO               EQUAL 1
               MOVE 1                    TO GFCTNA-ERRO
               MOVE GFCTG3-COD-SQL-ERRO  TO GFCTNA-COD-SQL-ERRO
               MOVE GFCTG3-COD-MSG-ERRO  TO GFCTNA-COD-MSG-ERRO
               MOVE GFCTG3-DESC-MSG-ERRO TO GFCTNA-DESC-MSG-ERRO

               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       9900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA FINALIZAR PROGRAMA                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       9999-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTN9-AGRUPAMENTO         TO GFCTNA-AGRUPAMENTO.
           MOVE GFCTN9-COD-TARIFA          TO GFCTNA-COD-TARIFA.
           MOVE GFCTN9-INIC-VIG-FLEX       TO GFCTNA-INIC-VIG-FLEX.
           MOVE GFCTN9-FIM-VIG-FLEX        TO GFCTNA-FIM-VIG-FLEX.
           MOVE GFCTN9-FRANQUIA            TO GFCTNA-FRANQUIA.
           MOVE GFCTN9-PERIODO             TO GFCTNA-PERIODO.
           MOVE GFCTN9-AGENDAMENTO         TO GFCTNA-AGENDAMENTO.
           MOVE GFCTN9-DIA-AGEND           TO GFCTNA-DIA-AGEND.
           MOVE GFCTN9-VR-NEGOCIADO        TO GFCTNA-VR-NEGOCIADO.
           MOVE GFCTN9-PERC-DESCTO         TO GFCTNA-PERC-DESCTO.
           MOVE GFCTN9-DIAS-RETENCAO       TO GFCTNA-DIAS-RETENCAO.
           MOVE GFCTN9-CTIPO-OPER-MOTVO    TO GFCTNA-CTIPO-OPER-MOTVO.
           MOVE GFCTN9-CMOTVO-JUSTF        TO GFCTNA-CMOTVO-JUSTF.
           MOVE GFCTN9-CGC-CPF             TO GFCTNA-CGC-CPF.
           MOVE GFCTN9-FILIAL              TO GFCTNA-FILIAL.
           MOVE GFCTN9-CONTROLE            TO GFCTNA-CONTROLE.
           MOVE GFCTN9-SEGMENTO            TO GFCTNA-SEGMENTO.
           MOVE GFCTN9-AGENCIA             TO GFCTNA-AGENCIA.
           MOVE GFCTN9-POSTO               TO GFCTNA-POSTO.
           MOVE GFCTN9-MUNIC               TO GFCTNA-MUNIC.
           MOVE GFCTN9-UF                  TO GFCTNA-UF.
           MOVE GFCTN9-TODAS-AG            TO GFCTNA-TODAS-AG.
           MOVE GFCTN9-IND-VALOR           TO GFCTNA-IND-VALOR.
           MOVE GFCTN9-RJUSTF-MANUT-SERVC  TO GFCTNA-RJUSTF-MANUT-SERVC.
080508     MOVE GFCTN9-PERIODO-COBR        TO GFCTNA-PERIODO-COBR
080508     MOVE GFCTN9-INIC-PER-COBR       TO GFCTNA-INIC-PER-COBR

0410       MOVE GFCTN9-GRUPO-CTBIL         TO GFCTNA-GRUPO-CTBIL
0410       MOVE GFCTN9-SUB-GRUPO-CTBIL     TO GFCTNA-SUB-GRUPO-CTBIL

250816     MOVE 1                          TO GFCTNA-QTDE-OCOR
250816     MOVE 1                          TO GFCTNA-QTDE-TOT-REG
250816
           MOVE 'S'                        TO GFCTNA-FIM.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
