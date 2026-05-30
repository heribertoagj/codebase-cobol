      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. GFCT1963.
       AUTHOR.     JEFERSON TERHORST.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT1963                                    *
      *    PROGRAMADOR.:   JEFERSON TERHORST        - CPM PATO BRANCO  *
      *    ANALISTA CPM:   CARLA FLACH ZANETTI      - CPM PATO BRANCO  *
      *    ANALISTA....:   MARIA VALERIA TORQUATO   - PROCWORK /GP.50  *
      *    DATA........:   18/12/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   CONVERTER TARIFAS DO ARQUIVO DE CONTAS COR- *
      *      RENTES, PARA CARGA INICIAL NAS TABELAS DE FLEXIBILIZACAO  *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                          INCLUDE/BOOK    *
      *                     ENTRADA                          -         *
      *                     SAIDA                            -         *
      *                     REJEITA                          -         *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                          INCLUDE/BOOK   *
      *                    DB2PRD.PARM_DATA_PROCM         GFCTB0A1     *
      *                    DB2PRD.TARIF_CORRL_SIST        GFCTB0F0     *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    POL7100C - AREA CHAMADA POOL7100 - CANCELAMENTO.            *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    POOL1050 - FAZ CONEXAO COM DB2.                             *
      *    POOL1470 - MODULO PARA CONSISTIR DATA.                      *
      *    POOL7100 - AREA PARA TRATAMENTO DE ERROS.                   *
      *    GFCT5018 - MODULO PARA OBTENCAO DE DADOS DE TARIFA.         *
      *================================================================*
      *================================================================*
      *      S O N D A  P R O C W O R K  -  C O N S U L T O R I A      *
      *----------------------------------------------------------------*
      *                A   L   T   E   R   A   C   A   O               *
      *----------------------------------------------------------------*
      *    PROGRAMADOR.:   VINICIUS CRISTIAN MADUREIRA - PROCWORK/GP50 *
      *    ANALISTA....:   MARIA VALERIA               - PROCWORK/GP50 *
      *    DATA........:   08/01/2009                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   ALTERACAO DO LAYOUT DOS 03 ARQUIVOS.        *
      *----------------------------------------------------------------*
      *    PROGRAMADOR.:   ALEXANDRE MOLERO  (AM1304)  - PROCWORK      *
      *    ANALISTA....:   MARIA VALERIA               - PROCWORK/GP50 *
      *    DATA........:   13/04/2009                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   INCLUSAO DO PARM NO PROGRAMA DAS DATAS      *
      *    DE INICIO E FIM PARA PESQUISA NO PROGRAMA.                  *
      *----------------------------------------------------------------*
      *    PROGRAMADOR.:   VALERIA TORQUATO            - PROCWORK      *
      *    ANALISTA....:   VALERIA TORQUATO            - PROCWORK/GP50 *
      *    DATA........:   29/06/2010                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   ACERTO NO FORMATO DA DATA NA CHAMADA        *
      *    DA POOL1470.                                                *
      *----------------------------------------------------------------*
      *================================================================*

      *===============================================================*
      *                   M A N U T E N C O E S                       *
      *===============================================================*
      *---------------------------------------------------------------*
      *    OBJETIVO....:   ALTERACAO DO TAMANHO DA COLUNA DA TABELA   *
      *     (JC0413)       GFCTBA2 - QDIA_CAREN_TARIF DE 9(02) PARA   *
      *                    9(03) -  O  MODULO : GFCT5018              *
      *                    CHAMADO  PELO PROGRAMA  POSSUEM O CAMPO    *
      *                    QDIA-CAREN-TARIF NA AREA DE LINKAGE.       *
      *---------------------------------------------------------------*
      *    PROGRAMADOR.:   J.C.R.                    - SONDA IT       *
      *    ANALISTA DDS:   HELENA                    - GP.50          *
      *    ANALISTA....:   PAGNNOCA                  - SONDA IT       *
      *    DATA........:   ABRIL/2013                                 *
      *---------------------------------------------------------------*
      *    ANALISTA DS :   LAURA KONKOL                               *
      *    DATA........:   OUTUBRO/2017                               *
      *    OBJETIVO....:   AJUSTES CONSISTÍNCIA CARGA MASSIVA FLEXIBIL*
      *                    ALTERAR TRATAMENTO PARA DIA AGENDAMENTO    *
      *                    RETIRAR VALOR, PERCENTUAL E FRANQUIA DA    *
      *                    NEGOCIACAO                                 *
      *---------------------------------------------------------------*
      *===============================================================*

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

           SELECT ENTRADA   ASSIGN     TO UT-S-ENTRADA
           FILE STATUS                 IS WRK-FS-ENTRADA.

           SELECT SAIDA     ASSIGN     TO UT-S-SAIDA
           FILE STATUS                 IS WRK-FS-SAIDA.

           SELECT REJEITA   ASSIGN     TO UT-S-REJEITA
           FILE STATUS                 IS WRK-FS-REJEITA.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:  REGISTROS ARQUIVO CONTA CORRENTE.                   *
      *            ORG. SEQUENCIAL     - LRECL = 175                   *
      *----------------------------------------------------------------*

       FD  ENTRADA
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'GFCTWAE4'.

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO CONTA CORRENTE COM TARIFA GFCT.             *
      *            ORG. SEQUENCIAL     - LRECL = 175                   *
      *----------------------------------------------------------------*

       FD  SAIDA
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'GFCTWAE5'.

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO REGISTROS REJEITADOS.                       *
      *            ORG. SEQUENCIAL     - LRECL = 215                   *
      *----------------------------------------------------------------*

       FD  REJEITA
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'GFCTWAE6'.

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(033)         VALUE
           'INICIO DA WORKING STORAGE SECTION'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(022)         VALUE
           'AREA PARA ACUMULADORES'.
      *----------------------------------------------------------------*

       77  ACU-LIDOS                   PIC  9(010) COMP-3  VALUE ZEROS.
       77  ACU-GRAV-SAIDA              PIC  9(010) COMP-3  VALUE ZEROS.
       77  ACU-GRAV-REJEITA            PIC  9(010) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(030)         VALUE
           'AREA PARA TESTE DE FILE STATUS'.
      *----------------------------------------------------------------*

       77  WRK-FS-ENTRADA              PIC  X(002)         VALUE SPACES.
       77  WRK-FS-SAIDA                PIC  X(002)         VALUE SPACES.
       77  WRK-FS-REJEITA              PIC  X(002)         VALUE SPACES.

       77  WRK-ABERTURA                PIC  X(013)         VALUE
           ' NA ABERTURA '.
       77  WRK-LEITURA                 PIC  X(013)         VALUE
           ' NA  LEITURA '.
       77  WRK-GRAVACAO                PIC  X(013)         VALUE
           ' NA GRAVACAO '.
       77  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(030)         VALUE
           'AREA PARA VARIAVEIS AUXILIARES'.
      *----------------------------------------------------------------*

       77  WRK-MASCARA                 PIC  ZZZZ.ZZZ.999   VALUE ZEROS.
       77  WRK-TEM-TARIFA              PIC  X(001)         VALUE SPACES.
       77  WRK-INCONS                  PIC  X(001)         VALUE SPACES.
       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-DATA-INIC-TARIF         PIC  9(008)         VALUE ZEROS.
       77  WRK-DATA-FIM-TARIF          PIC  9(008)         VALUE ZEROS.
PROC   77  WRK-DATA-PROCX-INV          PIC  9(008)         VALUE ZEROS.
       77  WRK-DATA-INI-9-9            PIC  9(009)         VALUE ZEROS.
2704   77  WRK-DATA-FIM-9-9            PIC  9(009)         VALUE ZEROS.
       77  WRK-PERIODO                 PIC  9(001)         VALUE ZEROS.
       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.

       01  WRK-DATA-INV                PIC  9(008)         VALUE ZEROS.
       01  WRK-DATA-INV-R              REDEFINES          WRK-DATA-INV.
           05 WRK-ANO-INV              PIC  9(004).
           05 WRK-MES-INV              PIC  9(002).
           05 WRK-DIA-INV              PIC  9(002).

       01  WRK-DATA-AUX                PIC  9(008)         VALUE ZEROS.
       01  WRK-DATA-AUX-R              REDEFINES          WRK-DATA-AUX.
           05 WRK-DIA-AUX              PIC  9(002).
           05 WRK-MES-AUX              PIC  9(002).
           05 WRK-ANO-AUX              PIC  9(004).

       01  WRK-DATA-9-9                PIC  9(009)         VALUE ZEROS.
       01  FILLER                      REDEFINES          WRK-DATA-9-9.
           05 FILLER                   PIC  9(001).
           05 WRK-DATA-9-8             PIC  9(008).

       01  WRK-DATA-DB2.
           05 WRK-DIA-DB2              PIC 9(002)          VALUE ZEROS.
           05 FILLER                   PIC X(001)          VALUE SPACES.
           05 WRK-MES-DB2              PIC 9(002)          VALUE ZEROS.
           05 FILLER                   PIC X(001)          VALUE SPACES.
           05 WRK-ANO-DB2              PIC 9(004)          VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA PARA MENSAGENS'.
      *----------------------------------------------------------------*

MAB    01  WRK-MSG00.
 .         05 FILLER                   PIC  X(006)         VALUE
 .         'CAMPO '.
 .         05 WRK-MSG00-CAMPO          PIC  9(002)         VALUE
 .         ZEROS.
 .         05 FILLER                   PIC  X(032)         VALUE
 .         ' NAO NUMERICO                   '.
       01  WRK-MSG01                   PIC  X(040)         VALUE
           'DATA VIGENCIA INIC INVALIDA'.
       01  WRK-MSG02                   PIC  X(040)         VALUE
           'DATA VIGENCIA FIM INVALIDA'.
       01  WRK-MSG04                   PIC  X(040)         VALUE
           'VIGENCIA INVALIDA - DATA INIC MAIOR FIM'.
       01  WRK-MSG05                   PIC  X(040)         VALUE
           'TARIFA CORRELATA SEM CORRESPONDENTE'.
       01  WRK-MSG06                   PIC  X(040)         VALUE
           'REGRAS TARIFA GFCT NAO ENCONTRADA'.
       01  WRK-MSG09                   PIC  X(040)         VALUE
           'PERCENTUAL INVALIDO'.
       01  WRK-MSG612                   PIC  X(040)         VALUE
           'P/ FRANQUIA ZERADA NAO TEM PERIODO'.
       01  WRK-MSG614                   PIC  X(040)         VALUE
           'PERCENTUAL INVALIDO'.
       01  WRK-MSG618                   PIC  X(040)         VALUE
           'PERIODO FRANQUIA TEM QUE SER IGUAL 4'.
       01  WRK-MSG619                   PIC  X(040)         VALUE
            'VLR E PERCENT SAO EXCLUSIVOS'.
       01  WRK-MSG615                   PIC  X(040)         VALUE
           'RETENC E DIA DE COBR. SAO EXCLUSIVOS'.
       01  WRK-MSG616                   PIC  X(040)         VALUE
           'DIA DE AGEND. DE FLEXIB. INVALIDO'.
       01  WRK-MSG806                   PIC  X(040)         VALUE
           'NAO FOI SELEC. NENH. NEG. P/ ESTA FLEXI.'.
       01  WRK-MSG1254                   PIC  X(040)        VALUE
           'NAO FOI SELEC. PERIODO DE COBRANCA'.
       01  WRK-MSG1255                   PIC  X(040)        VALUE
           'INICIO PERIODO DE COBRANCA INVALIDO'.
       01  WRK-MSG1256                   PIC  X(040)        VALUE
           'INICIO DE PERIODO EH OBRIGATORIO'.
       01  WRK-MSG1257                   PIC  X(040)        VALUE
           'TPO.OPER.MOTVO NAO TEM NA GFCTB089'.
       01  WRK-MSG980                    PIC  X(040)        VALUE
           'PERIODICIDADE DIFERENTE DE D.Q.M.S.'.
       01  WRK-MSG981                    PIC  X(040)        VALUE
           'JUSTIFICATIVA EM BRANCO            '.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(016)         VALUE
           'AREA PARA CHAVES'.
      *----------------------------------------------------------------*

       01  WRK-CHAVE-ANT.
           05 WRK-CCUSTO-ANT           PIC  X(004)         VALUE SPACES.
           05 WRK-AGENCIA-ANT          PIC S9(005)  COMP-3 VALUE ZEROS.
           05 WRK-CONTA-ANT            PIC S9(007)  COMP-3 VALUE ZEROS.
           05 WRK-TARIFA-ANT           PIC S9(005)  COMP-3 VALUE ZEROS.

       01  WRK-CHAVE-ATU.
           05 WRK-CCUSTO-ATU           PIC  X(004)         VALUE SPACES.
           05 WRK-AGENCIA-ATU          PIC S9(005)  COMP-3 VALUE ZEROS.
           05 WRK-CONTA-ATU            PIC S9(007)  COMP-3 VALUE ZEROS.
           05 WRK-TARIFA-ATU           PIC S9(005)  COMP-3 VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREAS DA POOL1470'.
      *----------------------------------------------------------------*

       01  WRK-DATA                    PIC  9(008)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA DE ACESSO AO MODULO GFCT5018'.
      *----------------------------------------------------------------*

       01  WRK-5018-AREA-ENTRADA.
           05 WRK-5018-AMBIENTE                PIC  X(001) VALUE SPACES.
           05 WRK-5018-CODIGO-TARIFA           PIC  9(005) VALUE ZEROS.
           05 WRK-5018-DT-REFER                PIC  X(010) VALUE SPACES.

       01  WRK-5018-AREA-SAIDA.
           05 WRK-5018-COD-RETORNO             PIC  9(002) VALUE ZEROS.
           05 WRK-5018-COD-SQL-ERRO            PIC  9(003) VALUE ZEROS.
           05 WRK-5018-DESC-MSG                PIC  X(070) VALUE SPACES.

       01  WRK-5018-DADOS-RETORNO.
           05 WRK-5018-DADOS-PRINCIPAIS.
             10 WRK-5018-CSERVC-TARIF          PIC  9(005) VALUE ZEROS.
             10 WRK-5018-CBCO-CCREN            PIC  9(003) VALUE ZEROS.
             10 WRK-5018-CIDTFD-LCTO           PIC  9(005) VALUE ZEROS.
             10 WRK-5018-CIDTFD-SNAL-LCTO      PIC  9(001) VALUE ZEROS.
             10 WRK-5018-CEMPR-INC             PIC  9(005) VALUE ZEROS.
             10 WRK-5018-CDEPDC                PIC  9(005) VALUE ZEROS.
             10 WRK-5018-CTPO-SERVC-TARIF      PIC  9(001) VALUE ZEROS.
             10 WRK-5018-RSERVC-TARIF          PIC  X(255) VALUE SPACES.
             10 WRK-5018-RSERVC-TARIF-REDZD    PIC  X(100) VALUE SPACES.
             10 WRK-5018-CPSSOA-SERVC-TARIF    PIC  X(001) VALUE SPACES.
             10 WRK-5018-CTPO-CTA-DEB-TARIF    PIC  9(001) VALUE ZEROS.
             10 WRK-5018-CTPO-DEB-COBR         PIC  9(001) VALUE ZEROS.
             10 WRK-5018-RUND-COBR-SERVC       PIC  X(020) VALUE SPACES.
             10 WRK-5018-CLCTO-SERVC           PIC  9(005) VALUE ZEROS.
             10 WRK-5018-HIDTFD-MANUT-SERVC-R  PIC  X(026) VALUE SPACES.
             10 WRK-5018-CSEQ-MANUT-SERVC-R    PIC  9(002) VALUE ZEROS.
             10 WRK-5018-CTPO-LCTO-COBR        PIC  9(002) VALUE ZEROS.
             10 WRK-5018-CTPO-LCTO-ESTRN       PIC  9(002) VALUE ZEROS.
             10 WRK-5018-CCART-DEB-TARIF       PIC  9(005) VALUE ZEROS.
             10 WRK-5018-CINCD-DEB-PCIAL       PIC  X(001) VALUE SPACES.
             10 WRK-5018-CINCD-PCIAL-ULT       PIC  X(001) VALUE SPACES.
             10 WRK-5018-CINCD-EVNTO-PCELD     PIC  X(001) VALUE SPACES.

           05 WRK-5018-DADOS-DE-PACOTE         OCCURS   50.
             10 WRK-5018-DINIC-VGCIA-COMPS     PIC  X(010) VALUE SPACES.
             10 WRK-5018-CSEQ-COMPO-PCOTE      PIC  9(003) VALUE ZEROS.
             10 WRK-5018-DFIM-VGCIA-COMPS      PIC  X(010) VALUE SPACES.
             10 WRK-5018-CGRP-SERVC            PIC  9(005) VALUE ZEROS.
             10 WRK-5018-DINIC-VGCIA-SERVC     PIC  X(010) VALUE SPACES.
             10 WRK-5018-CTARIF-COMPO-PCOTE    PIC  9(005) VALUE ZEROS.
             10 WRK-5018-QFRANQ-COMPO-PCOTE    PIC  9(003) VALUE ZEROS.
             10 WRK-5018-CPER-COBR-COMPO       PIC  9(001) VALUE ZEROS.
             10 WRK-5018-VEXCED-COMPO-PCOTE    PIC  9(011)V9(002)
                                                           VALUE ZEROS.
             10 WRK-5018-PEXCED-COMPO-PCOTE    PIC  9(007)V9(004)
                                                           VALUE ZEROS.
             10 WRK-5018-QDIA-COBR-COMPO       PIC  9(002) VALUE ZEROS.
             10 WRK-5018-RSEGDA-LIN-EXCED      PIC  X(032) VALUE SPACES.
             10 WRK-5018-HIDTFD-MANUT-SERVC    PIC  X(026) VALUE SPACES.
             10 WRK-5018-CSEQ-MANUT-SERVC      PIC  9(002) VALUE ZEROS.

           05 WRK-5018-DADOS-DE-PARAMENTROS.
             10 WRK-5018-DINIC-VGCIA-TARIF-P   PIC  X(010) VALUE SPACES.
             10 WRK-5018-DFIM-VGCIA-TARIF      PIC  X(010) VALUE SPACES.
             10 WRK-5018-CNATUZ-SERVC          PIC  9(003) VALUE ZEROS.
             10 WRK-5018-CCJTO-SERVC           PIC  9(003) VALUE ZEROS.
             10 WRK-5018-CINDCD-DEB-CONSL      PIC  X(001) VALUE SPACES.
             10 WRK-5018-CINDCD-EXCED-CONSL    PIC  X(001) VALUE SPACES.
             10 WRK-5018-QFRANQ-TARIF          PIC  9(003) VALUE ZEROS.
             10 WRK-5018-QMAX-TENTV-DEB        PIC  9(003) VALUE ZEROS.
             10 WRK-5018-CPER-FRANQ-TARIF      PIC  9(001) VALUE ZEROS.
             10 WRK-5018-CPER-CALC-TARIF       PIC  9(001) VALUE ZEROS.
             10 WRK-5018-CINIC-PER-CALC        PIC  9(001) VALUE ZEROS.
             10 WRK-5018-DDIA-COBR-TARIF       PIC  9(002) VALUE ZEROS.
JC0413       10 WRK-5018-QDIA-CAREN-TARIF      PIC  9(003) VALUE ZEROS.
             10 WRK-5018-PDESC-CAREN-TARIF     PIC  9(007)V9(004)
                                                           VALUE ZEROS.
             10 WRK-5018-CINDCD-INIC-CAREN     PIC  9(001) VALUE ZEROS.
             10 WRK-5018-QDIA-RTCAO-COBR       PIC  9(002) VALUE ZEROS.
             10 WRK-5018-DINIC-CAREN-TARIF     PIC  X(010) VALUE SPACES.
             10 WRK-5018-RSEGDA-LIN-EXTRT      PIC  X(032) VALUE SPACES.
             10 WRK-5018-HIDTFD-MANUT-SERVC-P  PIC  X(026) VALUE SPACES.
             10 WRK-5018-CSEQ-MANUT-SERVC-P    PIC  9(002) VALUE ZEROS.
             10 WRK-5018-CTPO-FRANQ-TARIF      PIC  9(001) VALUE ZEROS.

           05 WRK-5018-DADOS-DE-VALORES        OCCURS   50.
             10 WRK-5018-DINIC-VGCIA-TARIF     PIC  X(010) VALUE SPACES.
             10 WRK-5018-CSEQ-COMPO-TARIF      PIC  9(002) VALUE ZEROS.
             10 WRK-5018-CMOEDA-BACEN          PIC  9(005) VALUE ZEROS.
             10 WRK-5018-RCOMPO-TARIF          PIC  X(255) VALUE SPACES.
             10 WRK-5018-VCOMPO-TARIF          PIC  9(011)V9(002)
                                                           VALUE ZEROS.
             10 WRK-5018-PSERVC-PTADO          PIC  9(007)V9(004)
                                                           VALUE ZEROS.
             10 WRK-5018-VMAX-COMPO-TARIF      PIC  9(011)V9(002)
                                                           VALUE ZEROS.
             10 WRK-5018-VMIN-COMPO-TARIF      PIC  9(011)V9(002)
                                                           VALUE ZEROS.
             10 WRK-5018-CMOMEN-CONVS-MOEDA    PIC  9(001) VALUE ZEROS.

      *----------------------------------------------------------------*
AM1304 01  FILLER                      PIC  X(050)         VALUE
AM1304     'AREA DE ERRO PARM'.
      *----------------------------------------------------------------*
AM1304 01  WRK-MSG-ERRO2.
AM1304     03  FILLER                 PIC X(007)     VALUE SPACES.
AM1304     03  FILLER                 PIC X(004)     VALUE '*** '.
AM1304     03  WRK-MSG-ERR1           PIC X(040)     VALUE SPACES.
AM1304     03  WRK-MSG-ERR2           PIC X(013)     VALUE SPACES.
AM1304     03  FILLER                 PIC X(004)     VALUE ' ***'.

AM1304 01  WRK-MSG-ERRO3.
AM1304     03  FILLER                 PIC X(007)     VALUE SPACES.
AM1304     03  FILLER                 PIC X(002)     VALUE '* '.
AM1304     03  WRK-MSG-ERR3-1         PIC X(023)     VALUE SPACES.
AM1304     03  WRK-MSG-ERR3-2         PIC X(017)     VALUE SPACES.
AM1304     03  WRK-MSG-ERR3-3         PIC X(017)     VALUE SPACES.
AM1304     03  FILLER                 PIC X(002)     VALUE ' *'.

AM1304 01  WRK-DATA-L                 PIC 9(008)     VALUE ZEROS.
AM1304 01  WRK-DATA-X                 REDEFINES  WRK-DATA-L
AM1304                                PIC X(008).

AM1304 01  WRK-DEMAIS-CPOS.
AM1304     03  WRK-DTINI-PARM.
AM1304         05  WRK-ANOINI-PARM    PIC  9(004)    VALUE ZEROS.
AM1304         05  WRK-MESINI-PARM    PIC  9(002)    VALUE ZEROS.
AM1304         05  WRK-DIAINI-PARM    PIC  9(002)    VALUE ZEROS.
AM1304     03  WRK-DTINI-PARM-I       REDEFINES  WRK-DTINI-PARM
AM1304                                PIC  9(008).

AM1304     03  WRK-DTFIM-PARM.
AM1304         05  WRK-ANOFIM-PARM    PIC  9(004)    VALUE ZEROS.
AM1304         05  WRK-MESFIM-PARM    PIC  9(002)    VALUE ZEROS.
AM1304         05  WRK-DIAFIM-PARM    PIC  9(002)    VALUE ZEROS.
AM1304     03  WRK-DTFIM-PARM-I       REDEFINES  WRK-DTFIM-PARM
AM1304                                PIC  9(008).

AM1304 01  WRK-DTPR-DB2.
AM1304     03 WRK-DIA-PARM-DB2         PIC 9(002)          VALUE ZEROS.
AM1304     03 FILLER                   PIC X(001)          VALUE '.'.
AM1304     03 WRK-MES-PARM-DB2         PIC 9(002)          VALUE ZEROS.
AM1304     03 FILLER                   PIC X(001)          VALUE '.'.
AM1304     03 WRK-ANO-PARM-DB2         PIC 9(004)          VALUE ZEROS.

AM1304*----------------------------------------------------------------*
AM1304 01  FILLER                     PIC  X(032)         VALUE
AM1304     '*   AREA DA BRAD1470           *'.
AM1304*----------------------------------------------------------------*
AM1304 01  WRK-DATA-BRAD1470          PIC  9(008)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(026)         VALUE
           'AREA DE ERRO - FILE-STATUS'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-POOL7100.
           05 FILLER                   PIC  X(008)         VALUE
              '** ERRO '.
           05 WRK-OPERACAO             PIC  X(013)         VALUE SPACES.
           05 FILLER                   PIC  X(012)         VALUE
              ' DO ARQUIVO '.
           05 WRK-NOME-ARQ             PIC  X(008)         VALUE SPACES.
           05 FILLER                   PIC  X(020)         VALUE
              ' - FILE-INDICADOR = '.
           05 WRK-FILE-STATUS          PIC  X(002)         VALUE SPACES.
           05 FILLER                   PIC  X(003)         VALUE ' **'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(019)         VALUE
           ' AREA PARA POOL7100'.
      *----------------------------------------------------------------*

       COPY 'POL7100C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA PARA TABELA DB2'.
      *---------------------------------------------------------------*

           EXEC SQL
             INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0A1
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0F0
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB089
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(030)         VALUE
           'FIM DA WORKING STORAGE SECTION'.
      *----------------------------------------------------------------*

AM1304 LINKAGE SECTION.
AM1304*---------------*
AM1304 01  LNK-PARAM.
AM1304     03  LNK-TAM                 PIC 9(004) COMP.
AM1304     03  LNK-DATA-INI            PIC 9(008).
AM1304     03  LNK-DATA-FIM            PIC 9(008).

      *================================================================*
       PROCEDURE DIVISION   USING     LNK-PARAM.
      *================================================================*

      ******************************************************************
      *    CHAMA AS ROTINAS PRINCIPAIS DO PROCESSAMENTO.               *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-PROCESSAR      UNTIL
               WRK-FS-ENTRADA          EQUAL '10'.

           PERFORM 3000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCEDIMENTOS INICIAIS DO PROGRAMA                          *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL1050'.

           OPEN INPUT  ENTRADA
                OUTPUT SAIDA
                       REJEITA.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

AM1304     PERFORM 1150-00-VER-PARM.

           PERFORM 1300-LER-ENTRADA.

           IF  WRK-FS-ENTRADA          EQUAL '00'
               PERFORM 1200-BUSCAR-DATA
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.               EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA QUE EFETUA TESTE DE FILE-STATUS.                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-ENTRADA.

           PERFORM 1120-TESTAR-FS-SAIDA.

           PERFORM 1130-TESTAR-FS-REJEITA.

      *----------------------------------------------------------------*
       1100-99-FIM.               EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE TESTE PARA ARQUIVO ENTRADA.                      *
      ******************************************************************
      *----------------------------------------------------------------*
       1110-TESTAR-FS-ENTRADA          SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ENTRADA          NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'ENTRADA'          TO WRK-NOME-ARQ
               MOVE WRK-FS-ENTRADA     TO WRK-FILE-STATUS
               MOVE WRK-ERRO-POOL7100  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE TESTE PARA ARQUIVO SAIDA.                      *
      ******************************************************************
      *----------------------------------------------------------------*
       1120-TESTAR-FS-SAIDA            SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SAIDA            NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'SAIDA'            TO WRK-NOME-ARQ
               MOVE WRK-FS-SAIDA       TO WRK-FILE-STATUS
               MOVE WRK-ERRO-POOL7100  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE TESTE PARA ARQUIVO REJEITA.                       *
      ******************************************************************
      *----------------------------------------------------------------*
       1130-TESTAR-FS-REJEITA          SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-REJEITA          NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'REJEITA'          TO WRK-NOME-ARQ
               MOVE WRK-FS-REJEITA     TO WRK-FILE-STATUS
               MOVE WRK-ERRO-POOL7100  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
      * VERIFICA PARM E CONSISTE AS DATAS                             *
      *---------------------------------------------------------------*
AM1304 1150-00-VER-PARM          SECTION.

AM1304     IF      LNK-TAM               GREATER ZEROS
AM1304       IF    LNK-DATA-INI             NOT  NUMERIC
AM1304             MOVE   'APL'             TO   ERR-TIPO-ACESSO
AM1304             MOVE   ' DATA INI INFORMADA NO PARM INVALIDA  = '
AM1304                                      TO   WRK-MSG-ERR1
AM1304             MOVE   LNK-DATA-INI      TO   WRK-DATA-L
AM1304             MOVE   WRK-DATA-X        TO   WRK-MSG-ERR2
AM1304             MOVE   WRK-MSG-ERRO2     TO   ERR-TEXTO
AM1304             PERFORM 9999-PROCESSAR-ROTINA-ERRO
AM1304       ELSE
AM1304             MOVE LNK-DATA-INI        TO   WRK-DATA-BRAD1470
AM1304             CALL 'POOL1470'        USING  WRK-DATA-BRAD1470
AM1304             IF   RETURN-CODE   NOT EQUAL  ZEROS
AM1304                  MOVE  'APL'         TO   ERR-TIPO-ACESSO
AM1304                  MOVE  ' DATA INI INFORMADA NO PARM INVALIDA  = '
AM1304                                      TO   WRK-MSG-ERR1
AM1304                  MOVE LNK-DATA-INI   TO   WRK-DATA-L
AM1304                  MOVE WRK-DATA-X     TO   WRK-MSG-ERR2
AM1304                  MOVE WRK-MSG-ERRO2  TO   ERR-TEXTO
AM1304                  PERFORM 9999-PROCESSAR-ROTINA-ERRO
AM1304             ELSE
AM1304                  MOVE LNK-DATA-INI        TO   WRK-DATA-L
AM1304                  MOVE WRK-DATA-L (1:2)    TO
AM1304                                           WRK-DIAINI-PARM
AM1304                  MOVE WRK-DATA-L (3:2)    TO
AM1304                                           WRK-MESINI-PARM
AM1304                  MOVE WRK-DATA-L (5:4)    TO
AM1304                                           WRK-ANOINI-PARM
AM1304             END-IF
AM1304       END-IF
AM1304       IF    LNK-DATA-FIM             NOT  NUMERIC
AM1304             MOVE   'APL'             TO   ERR-TIPO-ACESSO
AM1304             MOVE   ' DATA FIM INFORMADA NO PARM INVALIDA  = '
AM1304                                      TO   WRK-MSG-ERR1
AM1304             MOVE   LNK-DATA-FIM      TO   WRK-DATA-L
AM1304             MOVE   WRK-DATA-X        TO   WRK-MSG-ERR2
AM1304             MOVE    WRK-MSG-ERRO2    TO   ERR-TEXTO
AM1304             PERFORM 9999-PROCESSAR-ROTINA-ERRO
AM1304       ELSE
AM1304             MOVE LNK-DATA-FIM        TO   WRK-DATA-BRAD1470
AM1304             CALL 'POOL1470'        USING  WRK-DATA-BRAD1470
AM1304             IF   RETURN-CODE   NOT EQUAL  ZEROS
AM1304                  MOVE  'APL'         TO   ERR-TIPO-ACESSO
AM1304                  MOVE  ' DATA FIM INFORMADA NO PARM INVALIDA  = '
AM1304                                      TO   WRK-MSG-ERR1
AM1304                  MOVE LNK-DATA-FIM   TO   WRK-DATA-L
AM1304                  MOVE WRK-DATA-X     TO   WRK-MSG-ERR2
AM1304                  MOVE WRK-MSG-ERRO2  TO   ERR-TEXTO
AM1304                  PERFORM 9999-PROCESSAR-ROTINA-ERRO
AM1304             ELSE
AM1304                  MOVE LNK-DATA-FIM        TO   WRK-DATA-L
AM1304                  MOVE WRK-DATA-L (1:2)    TO
AM1304                                           WRK-DIAFIM-PARM
AM1304                  MOVE WRK-DATA-L (3:2)    TO
AM1304                                           WRK-MESFIM-PARM
AM1304                  MOVE WRK-DATA-L (5:4)    TO
AM1304                                           WRK-ANOFIM-PARM
AM1304             END-IF
AM1304       END-IF
AM1304     END-IF.

AM2704     IF      LNK-TAM             GREATER   ZEROS
VT2906             MOVE WRK-DTFIM-PARM-I TO WRK-DATA-9-8
AM1304             MOVE WRK-DATA-9-9   TO   WAE4-DATA-FIM
AM2704     END-IF.

      *----------------------------------------------------------------*
AM1304 1150-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      ******************************************************************
      *    ROTINA QUE LE TABELA GFCTB0A1 E BUSCA DATA PROCM            *
      ******************************************************************
      *----------------------------------------------------------------*
       1200-BUSCAR-DATA                SECTION.
      *----------------------------------------------------------------*

PROC       MOVE 'GFCT'                 TO CSIST-PRINC OF GFCTB0A1.

           EXEC SQL
             SELECT  DPROCM_PROX
               INTO :GFCTB0A1.DPROCM-PROX
               FROM  DB2PRD.PARM_DATA_PROCM
PROC           WHERE CSIST_PRINC       = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS) OR
               (SQLWARN0               EQUAL 'W')
                MOVE 'DB2'             TO ERR-TIPO-ACESSO
                MOVE 'PARM_DATA_PROCM' TO ERR-DBD-TAB
                MOVE 'SELECT'          TO ERR-FUN-COMANDO
                MOVE  SQLCODE          TO ERR-SQL-CODE
                MOVE '0010'            TO ERR-LOCAL
                MOVE  SPACES           TO ERR-SEGM
                PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

PROC       MOVE DPROCM-PROX            OF GFCTB0A1
PROC                                   TO WRK-DATA-DB2.
PROC       MOVE WRK-DIA-DB2            TO WRK-DIA-INV.
PROC       MOVE WRK-MES-DB2            TO WRK-MES-INV.
PROC       MOVE WRK-ANO-DB2            TO WRK-ANO-INV.
PROC       MOVE WRK-DATA-INV           TO WRK-DATA-PROCX-INV.

AM1304     IF      LNK-TAM             GREATER   ZEROS
AM1304         IF  WRK-DATA-PROCX-INV  GREATER   WRK-DTINI-PARM-I
AM1304             MOVE   'APL'             TO   ERR-TIPO-ACESSO
AM1304             MOVE   ' DT PROC MAIOR DT INI: '
AM1304                                       TO   WRK-MSG-ERR3-1
AM1304             MOVE   WRK-DTINI-PARM-I   TO   WRK-DATA-L
AM1304             MOVE   WRK-DATA-X         TO   WRK-MSG-ERR3-2
AM1304             MOVE   WRK-DATA-PROCX-INV TO   WRK-DATA-L
AM1304             MOVE   WRK-DATA-X         TO   WRK-MSG-ERR3-3
AM1304             MOVE   WRK-MSG-ERRO3      TO   ERR-TEXTO
AM1304             PERFORM 9999-PROCESSAR-ROTINA-ERRO
AM1304         ELSE
AM1304             MOVE WRK-DTINI-PARM-I     TO   WRK-DATA-PROCX-INV
AM1304         END-IF
AM1304     END-IF.
      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA QUE EFETUA LEITURA E TRATAMENTO DO ARQUIVO DE ENTRADA*
      ******************************************************************
      *----------------------------------------------------------------*
       1300-LER-ENTRADA                SECTION.
      *----------------------------------------------------------------*

           READ ENTRADA.

           IF  WRK-FS-ENTRADA          EQUAL '10'
               GO TO 1300-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1110-TESTAR-FS-ENTRADA.

           ADD 1                       TO ACU-LIDOS.

           MOVE WAE4-CENTRO-CUSTO      TO WRK-CCUSTO-ATU.
           MOVE WAE4-AGENCIA           TO WRK-AGENCIA-ATU.
           MOVE WAE4-CONTA             TO WRK-CONTA-ATU.
           MOVE WAE4-TARIFA            TO WRK-TARIFA-ATU.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCESSAMENTO PRINCIPAL                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

MAB        MOVE ZEROS                  TO WRK-MSG00-CAMPO
 .
 .         IF  WAE4-AGENCIA           NOT NUMERIC
 .             MOVE 01                 TO WRK-MSG00-CAMPO
 .             MOVE GFCTWAE4-REG       TO GFCTWAE6-REG
 .             MOVE WRK-MSG00          TO WAE6-MENSAGEM
 .             PERFORM 2200-GRAVAR-REJEITA
 .             PERFORM 1300-LER-ENTRADA
 .             MOVE SPACES             TO WRK-INCONS
 .             GO TO 2000-99-FIM
 .         END-IF.
 .
 .         IF  WAE4-GRUPO             NOT NUMERIC
 .             MOVE 02                 TO WRK-MSG00-CAMPO
 .             MOVE GFCTWAE4-REG       TO GFCTWAE6-REG
 .             MOVE WRK-MSG00          TO WAE6-MENSAGEM
 .             PERFORM 2200-GRAVAR-REJEITA
 .             PERFORM 1300-LER-ENTRADA
 .             MOVE SPACES             TO WRK-INCONS
 .             GO TO 2000-99-FIM
 .         END-IF.
 .
 .         IF  WAE4-SUB-GRUPO         NOT NUMERIC
 .             MOVE 03                 TO WRK-MSG00-CAMPO
 .             MOVE GFCTWAE4-REG       TO GFCTWAE6-REG
 .             MOVE WRK-MSG00          TO WAE6-MENSAGEM
 .             PERFORM 2200-GRAVAR-REJEITA
 .             PERFORM 1300-LER-ENTRADA
 .             MOVE SPACES             TO WRK-INCONS
 .             GO TO 2000-99-FIM
 .         END-IF.
 .
 .         IF  WAE4-CONTA             NOT NUMERIC
 .             MOVE 04                 TO WRK-MSG00-CAMPO
 .             MOVE GFCTWAE4-REG       TO GFCTWAE6-REG
 .             MOVE WRK-MSG00          TO WAE6-MENSAGEM
 .             PERFORM 2200-GRAVAR-REJEITA
 .             PERFORM 1300-LER-ENTRADA
 .             MOVE SPACES             TO WRK-INCONS
 .             GO TO 2000-99-FIM
 .         END-IF.
 .
 .         IF  WAE4-TARIFA            NOT NUMERIC
 .             MOVE 05                 TO WRK-MSG00-CAMPO
 .             MOVE GFCTWAE4-REG       TO GFCTWAE6-REG
 .             MOVE WRK-MSG00          TO WAE6-MENSAGEM
 .             PERFORM 2200-GRAVAR-REJEITA
 .             PERFORM 1300-LER-ENTRADA
 .             MOVE SPACES             TO WRK-INCONS
 .             GO TO 2000-99-FIM
 .         END-IF.
 .
 .         IF  WAE4-PERCENT           NOT NUMERIC
 .             MOVE 06                 TO WRK-MSG00-CAMPO
 .             MOVE GFCTWAE4-REG       TO GFCTWAE6-REG
 .             MOVE WRK-MSG00          TO WAE6-MENSAGEM
 .             PERFORM 2200-GRAVAR-REJEITA
 .             PERFORM 1300-LER-ENTRADA
 .             MOVE SPACES             TO WRK-INCONS
 .             GO TO 2000-99-FIM
 .         END-IF.
 .
 .         IF  WAE4-VNEGOCI           NOT NUMERIC
 .             MOVE 07                 TO WRK-MSG00-CAMPO
 .             MOVE GFCTWAE4-REG       TO GFCTWAE6-REG
 .             MOVE WRK-MSG00          TO WAE6-MENSAGEM
 .             PERFORM 2200-GRAVAR-REJEITA
 .             PERFORM 1300-LER-ENTRADA
 .             MOVE SPACES             TO WRK-INCONS
 .             GO TO 2000-99-FIM
 .         END-IF.
 .
 .         IF  WAE4-INIPER-COBR       NOT NUMERIC
 .             MOVE 09                 TO WRK-MSG00-CAMPO
 .             MOVE GFCTWAE4-REG       TO GFCTWAE6-REG
 .             MOVE WRK-MSG00          TO WAE6-MENSAGEM
 .             PERFORM 2200-GRAVAR-REJEITA
 .             PERFORM 1300-LER-ENTRADA
 .             MOVE SPACES             TO WRK-INCONS
 .             GO TO 2000-99-FIM
 .         END-IF.
 .
 .         IF  WAE4-QFRANQ            NOT NUMERIC
 .             MOVE 10                 TO WRK-MSG00-CAMPO
 .             MOVE GFCTWAE4-REG       TO GFCTWAE6-REG
 .             MOVE WRK-MSG00          TO WAE6-MENSAGEM
 .             PERFORM 2200-GRAVAR-REJEITA
 .             PERFORM 1300-LER-ENTRADA
 .             MOVE SPACES             TO WRK-INCONS
 .             GO TO 2000-99-FIM
 .         END-IF.
 .
 .         IF  WAE4-CTPO-FRANQ-TARIF  NOT NUMERIC
 .             MOVE 11                 TO WRK-MSG00-CAMPO
 .             MOVE GFCTWAE4-REG       TO GFCTWAE6-REG
 .             MOVE WRK-MSG00          TO WAE6-MENSAGEM
 .             PERFORM 2200-GRAVAR-REJEITA
 .             PERFORM 1300-LER-ENTRADA
 .             MOVE SPACES             TO WRK-INCONS
 .             GO TO 2000-99-FIM
 .         END-IF.
 .
 .         IF  WAE4-CPER-FRANQ        NOT NUMERIC
 .             MOVE 12                 TO WRK-MSG00-CAMPO
 .             MOVE GFCTWAE4-REG       TO GFCTWAE6-REG
 .             MOVE WRK-MSG00          TO WAE6-MENSAGEM
 .             PERFORM 2200-GRAVAR-REJEITA
 .             PERFORM 1300-LER-ENTRADA
 .             MOVE SPACES             TO WRK-INCONS
 .             GO TO 2000-99-FIM
 .         END-IF.
 .
 .         IF  WAE4-DDIA-AGDA         NOT NUMERIC
 .             MOVE 15                 TO WRK-MSG00-CAMPO
 .             MOVE GFCTWAE4-REG       TO GFCTWAE6-REG
 .             MOVE WRK-MSG00          TO WAE6-MENSAGEM
 .             PERFORM 2200-GRAVAR-REJEITA
 .             PERFORM 1300-LER-ENTRADA
 .             MOVE SPACES             TO WRK-INCONS
 .             GO TO 2000-99-FIM
 .         END-IF.
 .
 .         IF  WAE4-DIAS-RETENCAO     NOT NUMERIC
 .             MOVE 16                 TO WRK-MSG00-CAMPO
 .             MOVE GFCTWAE4-REG       TO GFCTWAE6-REG
 .             MOVE WRK-MSG00          TO WAE6-MENSAGEM
 .             PERFORM 2200-GRAVAR-REJEITA
 .             PERFORM 1300-LER-ENTRADA
 .             MOVE SPACES             TO WRK-INCONS
 .             GO TO 2000-99-FIM
 .         END-IF.
 .
 .         IF  WAE4-DATA-FIM          NOT NUMERIC
 .             MOVE 17                 TO WRK-MSG00-CAMPO
 .             MOVE GFCTWAE4-REG       TO GFCTWAE6-REG
 .             MOVE WRK-MSG00          TO WAE6-MENSAGEM
 .             PERFORM 2200-GRAVAR-REJEITA
 .             PERFORM 1300-LER-ENTRADA
 .             MOVE SPACES             TO WRK-INCONS
 .             GO TO 2000-99-FIM
 .         END-IF.
 .
 .         IF  WAE4-CTP-OPER-MOTVO    NOT NUMERIC
 .             MOVE 18                 TO WRK-MSG00-CAMPO
 .             MOVE GFCTWAE4-REG       TO GFCTWAE6-REG
 .             MOVE WRK-MSG00          TO WAE6-MENSAGEM
 .             PERFORM 2200-GRAVAR-REJEITA
 .             PERFORM 1300-LER-ENTRADA
 .             MOVE SPACES             TO WRK-INCONS
 .             GO TO 2000-99-FIM
 .         END-IF.
 .
 .         IF  WAE4-CMOTVO-JUSTF      NOT NUMERIC
 .             MOVE 19                 TO WRK-MSG00-CAMPO
 .             MOVE GFCTWAE4-REG       TO GFCTWAE6-REG
 .             MOVE WRK-MSG00          TO WAE6-MENSAGEM
 .             PERFORM 2200-GRAVAR-REJEITA
 .             PERFORM 1300-LER-ENTRADA
 .             MOVE SPACES             TO WRK-INCONS
 .             GO TO 2000-99-FIM
 .         END-IF.

AM2704     IF      LNK-TAM             GREATER   ZEROS
VT2906             MOVE WRK-DTFIM-PARM-I TO WRK-DATA-9-8
AM1304             MOVE WRK-DATA-9-9   TO   WAE4-DATA-FIM
AM2704     END-IF.

           MOVE WRK-DATA-PROCX-INV     TO WRK-DATA-INV.
           MOVE WRK-DIA-INV            TO WRK-DIA-AUX.
           MOVE WRK-MES-INV            TO WRK-MES-AUX.
           MOVE WRK-ANO-INV            TO WRK-ANO-AUX.
           MOVE WRK-DATA-AUX           TO WRK-DATA.
           PERFORM 2100-CONSISTIR-DATA.

           IF  WRK-INCONS              NOT EQUAL SPACES
               MOVE GFCTWAE4-REG       TO GFCTWAE6-REG
               MOVE WRK-MSG01          TO WAE6-MENSAGEM
               PERFORM 2200-GRAVAR-REJEITA
               PERFORM 1300-LER-ENTRADA
               MOVE SPACES             TO WRK-INCONS
               GO TO 2000-99-FIM
           END-IF.

VT2906     MOVE WAE4-DATA-FIM          TO WRK-DATA-9-9.
VT2906     MOVE WRK-DATA-9-8           TO WRK-DATA-INV.
VT2906     MOVE WRK-DATA-INV           TO WRK-DATA-FIM-9-9.
VT2906     MOVE WRK-DIA-INV            TO WRK-DIA-AUX.
VT2906     MOVE WRK-MES-INV            TO WRK-MES-AUX.
VT2906     MOVE WRK-ANO-INV            TO WRK-ANO-AUX.
           MOVE WRK-DATA-AUX           TO WRK-DATA.

           PERFORM 2100-CONSISTIR-DATA.

           IF  WRK-INCONS              NOT EQUAL SPACES
               MOVE GFCTWAE4-REG       TO GFCTWAE6-REG
               MOVE WRK-MSG02          TO WAE6-MENSAGEM
               PERFORM 2200-GRAVAR-REJEITA
               PERFORM 1300-LER-ENTRADA
               MOVE SPACES             TO WRK-INCONS
               GO TO 2000-99-FIM
           END-IF.

           MOVE WRK-DATA-PROCX-INV     TO WRK-DATA-INI-9-9

2704       IF  WRK-DATA-INI-9-9        GREATER WRK-DATA-FIM-9-9
               MOVE GFCTWAE4-REG       TO GFCTWAE6-REG
               MOVE WRK-MSG04          TO WAE6-MENSAGEM
               PERFORM 2200-GRAVAR-REJEITA
               PERFORM 1300-LER-ENTRADA
               GO TO 2000-99-FIM
           END-IF.

           IF  WRK-CHAVE-ANT           NOT EQUAL WRK-CHAVE-ATU
               MOVE 'S'                TO WRK-TEM-TARIFA
               PERFORM 2500-PESQUISAR-TARIFA
               MOVE WRK-CHAVE-ATU      TO WRK-CHAVE-ANT
           END-IF.

           IF  WRK-TEM-TARIFA          EQUAL 'N'
               MOVE GFCTWAE4-REG       TO GFCTWAE6-REG
               MOVE WRK-MSG05          TO WAE6-MENSAGEM
               PERFORM 2200-GRAVAR-REJEITA
               PERFORM 1300-LER-ENTRADA
               GO TO 2000-99-FIM
           END-IF.

           PERFORM 2300-ACESSAR-GFCT5018.

           IF  WRK-5018-COD-RETORNO    NOT EQUAL ZEROS
               MOVE GFCTWAE4-REG       TO GFCTWAE6-REG
               MOVE WRK-MSG06          TO WAE6-MENSAGEM
               PERFORM 2200-GRAVAR-REJEITA
               PERFORM 1300-LER-ENTRADA
               GO TO 2000-99-FIM
           END-IF.

           MOVE WRK-5018-DINIC-VGCIA-TARIF-P
                                       TO WRK-DATA-DB2.
           MOVE WRK-DIA-DB2            TO WRK-DIA-INV.
           MOVE WRK-MES-DB2            TO WRK-MES-INV.
           MOVE WRK-ANO-DB2            TO WRK-ANO-INV.
           MOVE WRK-DATA-INV           TO WRK-DATA-INIC-TARIF.

           MOVE WRK-5018-DFIM-VGCIA-TARIF
                                       TO WRK-DATA-DB2.
           MOVE WRK-DIA-DB2            TO WRK-DIA-INV.
           MOVE WRK-MES-DB2            TO WRK-MES-INV.
           MOVE WRK-ANO-DB2            TO WRK-ANO-INV.
           MOVE WRK-DATA-INV           TO WRK-DATA-FIM-TARIF.

           IF  WAE4-PERCENT            GREATER 100,00
               MOVE GFCTWAE4-REG       TO GFCTWAE6-REG
               MOVE WRK-MSG09          TO WAE6-MENSAGEM
               PERFORM 2200-GRAVAR-REJEITA
               PERFORM 1300-LER-ENTRADA
               GO TO 2000-99-FIM
           END-IF.

           MOVE WAE4-DATA-FIM          TO WRK-DATA-9-9.
PROC
           IF  WRK-DATA-9-8           GREATER WRK-DATA-FIM-TARIF
PROC           MOVE WRK-DATA-FIM-TARIF TO WAE4-DATA-FIM
PROC       END-IF.

PROC       MOVE 0 TO WRK-PERIODO

           IF WAE4-PERIODI EQUAL 'D'
               MOVE 1 TO WRK-PERIODO
           ELSE
               IF WAE4-PERIODI EQUAL 'S'
                   MOVE 2 TO WRK-PERIODO
               ELSE
                   IF WAE4-PERIODI EQUAL 'M'
                       MOVE 4 TO WRK-PERIODO
                   ELSE
                       IF WAE4-PERIODI EQUAL 'Q'
                           MOVE 3 TO WRK-PERIODO
                       END-IF
                   END-IF
               END-IF
           END-IF.

           IF  WAE4-PERIODI              NOT EQUAL 'D' AND 'S' AND 'M'
                                               AND 'Q'
               MOVE GFCTWAE4-REG       TO GFCTWAE6-REG
               MOVE WRK-MSG980         TO WAE6-MENSAGEM
               PERFORM 2200-GRAVAR-REJEITA
               PERFORM 1300-LER-ENTRADA
               GO TO 2000-99-FIM
           END-IF.

           IF  WAE4-RJU-MNUT-SRVC         EQUAL SPACES
               MOVE GFCTWAE4-REG       TO GFCTWAE6-REG
               MOVE WRK-MSG981         TO WAE6-MENSAGEM
               PERFORM 2200-GRAVAR-REJEITA
               PERFORM 1300-LER-ENTRADA
               GO TO 2000-99-FIM
           END-IF.

           IF  WAE4-QFRANQ                 EQUAL ZEROS
               IF  WAE4-CPER-FRANQ         NOT EQUAL ZEROS
                   MOVE GFCTWAE4-REG       TO GFCTWAE6-REG
                   MOVE WRK-MSG612         TO WAE6-MENSAGEM
                   PERFORM 2200-GRAVAR-REJEITA
                   PERFORM 1300-LER-ENTRADA
                   GO TO 2000-99-FIM
               END-IF
           ELSE
               IF  WAE4-CPER-FRANQ         NOT EQUAL  4
                   MOVE GFCTWAE4-REG       TO GFCTWAE6-REG
                   MOVE WRK-MSG618         TO WAE6-MENSAGEM
                   PERFORM 2200-GRAVAR-REJEITA
                   PERFORM 1300-LER-ENTRADA
                   GO TO 2000-99-FIM
               END-IF
           END-IF.

           IF WAE4-PERCENT NOT EQUAL ZEROS
               IF  WAE4-PERCENT  GREATER 100,00
                   MOVE GFCTWAE4-REG       TO GFCTWAE6-REG
                   MOVE WRK-MSG614     TO WAE6-MENSAGEM
                   PERFORM 2200-GRAVAR-REJEITA
                   PERFORM 1300-LER-ENTRADA
                   GO TO 2000-99-FIM
               END-IF
           END-IF.

           IF  WAE4-VNEGOCI      NOT EQUAL ZEROS AND
               WAE4-PERCENT      NOT EQUAL ZEROS
               MOVE GFCTWAE4-REG       TO GFCTWAE6-REG
               MOVE WRK-MSG619         TO WAE6-MENSAGEM
                   PERFORM 2200-GRAVAR-REJEITA
                   PERFORM 1300-LER-ENTRADA
                   GO TO 2000-99-FIM
           END-IF.

           IF  WAE4-AGENDAMENTO      EQUAL 'S'
               AND WAE4-DDIA-AGDA EQUAL ZEROS
                   MOVE GFCTWAE4-REG       TO GFCTWAE6-REG
                   MOVE WRK-MSG616         TO WAE6-MENSAGEM
                   PERFORM 2200-GRAVAR-REJEITA
                   PERFORM 1300-LER-ENTRADA
                   GO TO 2000-99-FIM
           END-IF.

           IF  WAE4-DDIA-AGDA      NOT EQUAL ZEROS
               IF WAE4-DIAS-RETENCAO NOT EQUAL ZEROS
                   MOVE GFCTWAE4-REG       TO GFCTWAE6-REG
                   MOVE WRK-MSG615         TO WAE6-MENSAGEM
                   PERFORM 2200-GRAVAR-REJEITA
                   PERFORM 1300-LER-ENTRADA
                   GO TO 2000-99-FIM
               END-IF
           END-IF.

           IF  WAE4-AGENDAMENTO      EQUAL 'S'
PROC           IF  WAE4-DDIA-AGDA    GREATER 28
                   MOVE GFCTWAE4-REG       TO GFCTWAE6-REG
                   MOVE WRK-MSG616         TO WAE6-MENSAGEM
                   PERFORM 2200-GRAVAR-REJEITA
                   PERFORM 1300-LER-ENTRADA
                   GO TO 2000-99-FIM
               END-IF
           ELSE
               IF  WAE4-DDIA-AGDA    NOT EQUAL ZEROS
                   MOVE GFCTWAE4-REG       TO GFCTWAE6-REG
                   MOVE WRK-MSG616         TO WAE6-MENSAGEM
                   PERFORM 2200-GRAVAR-REJEITA
                   PERFORM 1300-LER-ENTRADA
                   GO TO 2000-99-FIM
               END-IF
           END-IF.


           IF  WAE4-QFRANQ         EQUAL ZEROS AND
               WAE4-VNEGOCI        EQUAL ZEROS AND
               WAE4-PERCENT        EQUAL ZEROS AND
               WAE4-DIAS-RETENCAO  EQUAL ZEROS AND
               WAE4-PERIODI        EQUAL SPACES
                   MOVE GFCTWAE4-REG       TO GFCTWAE6-REG
                   MOVE WRK-MSG806       TO WAE6-MENSAGEM
                   PERFORM 2200-GRAVAR-REJEITA
                   PERFORM 1300-LER-ENTRADA
                   GO TO 2000-99-FIM
           END-IF.

           IF (WRK-PERIODO         NOT EQUAL 0 AND 1 AND 2 AND 3 AND 4)
                   MOVE GFCTWAE4-REG       TO GFCTWAE6-REG
                   MOVE WRK-MSG1254        TO WAE6-MENSAGEM
                   PERFORM 2200-GRAVAR-REJEITA
                   PERFORM 1300-LER-ENTRADA
                   GO TO 2000-99-FIM
           END-IF.

           IF (WAE4-INIPER-COBR      NOT EQUAL 0 AND 1 AND 2 AND 3 AND
                                               4 AND 5 AND 6 AND 7)
                   MOVE GFCTWAE4-REG       TO GFCTWAE6-REG
                   MOVE WRK-MSG1255           TO WAE6-MENSAGEM
                   PERFORM 2200-GRAVAR-REJEITA
                   PERFORM 1300-LER-ENTRADA
                   GO TO 2000-99-FIM
           END-IF.

           IF  WRK-PERIODO              EQUAL 2
               IF (WAE4-INIPER-COBR      NOT EQUAL 1 AND 2 AND 3 AND
                                                   4 AND 5 AND 6 AND 7)
                   MOVE GFCTWAE4-REG     TO GFCTWAE6-REG
                   MOVE WRK-MSG1256      TO WAE6-MENSAGEM
                   PERFORM 2200-GRAVAR-REJEITA
                   PERFORM 1300-LER-ENTRADA
                   GO TO 2000-99-FIM
               END-IF
           ELSE
               MOVE ZEROS                TO WAE4-INIPER-COBR
           END-IF.

           PERFORM 2600-VALIDAR-TPO-MOTVO

           IF  SQLCODE              EQUAL +100
               MOVE GFCTWAE4-REG       TO GFCTWAE6-REG
               MOVE WRK-MSG1257        TO WAE6-MENSAGEM
               PERFORM 2200-GRAVAR-REJEITA
               PERFORM 1300-LER-ENTRADA
               GO TO 2000-99-FIM
           END-IF.

           PERFORM 2400-GRAVAR-SAIDA.
           PERFORM 1300-LER-ENTRADA.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    CONSISTE DATA ATRAVES DA POOL1570.                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-CONSISTIR-DATA             SECTION.
      *----------------------------------------------------------------*

           CALL  'POOL1470'            USING WRK-DATA.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE '*'                TO WRK-INCONS
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA QUE EFETUA GRAVACAO NO ARQUIVO REJEITA.              *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-GRAVAR-REJEITA             SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO            TO WRK-OPERACAO.

           WRITE GFCTWAE6-REG.

           PERFORM 1130-TESTAR-FS-REJEITA.

           ADD 1                        TO ACU-GRAV-REJEITA.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ACESSA O MODULO GFCT5018.                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2300-ACESSAR-GFCT5018           SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO WRK-5018-AREA-SAIDA
                                          WRK-5018-DADOS-RETORNO.

           INITIALIZE WRK-5018-AREA-SAIDA
                      WRK-5018-DADOS-RETORNO.

           MOVE 'B'                    TO WRK-5018-AMBIENTE.
           MOVE CSERVC-TARIF           OF GFCTB0F0
                                       TO WRK-5018-CODIGO-TARIFA.

PROC       MOVE DPROCM-PROX            TO WRK-5018-DT-REFER.

AM1304     IF      LNK-TAM             GREATER   ZEROS
AM1304             MOVE WRK-DIAINI-PARM     TO   WRK-DIA-PARM-DB2
AM1304             MOVE WRK-MESINI-PARM     TO   WRK-MES-PARM-DB2
AM1304             MOVE WRK-ANOINI-PARM     TO   WRK-ANO-PARM-DB2
AM1304             MOVE WRK-DTPR-DB2        TO   WRK-5018-DT-REFER
AM1304     END-IF.

           MOVE 'GFCT5018'             TO WRK-MODULO.

           CALL WRK-MODULO             USING WRK-5018-AREA-ENTRADA
                                             WRK-5018-AREA-SAIDA
                                             WRK-5018-DADOS-RETORNO.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    GRAVA O ARQUIVO SAIDA.                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2400-GRAVAR-SAIDA               SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTWAE4-REG           TO GFCTWAE5-REG.
           MOVE CSERVC-TARIF           OF GFCTB0F0
                                       TO WAE5-TARIFA.

           WRITE GFCTWAE5-REG.
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.
           PERFORM 1120-TESTAR-FS-SAIDA.

           ADD 1                       TO ACU-GRAV-SAIDA.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PESQUISA TARIFA.                                            *
      ******************************************************************
      *----------------------------------------------------------------*
       2500-PESQUISAR-TARIFA           SECTION.
      *----------------------------------------------------------------*

           MOVE WAE4-CENTRO-CUSTO    TO CSIST-CORRL        OF GFCTB0F0.
           MOVE WAE4-TARIFA          TO CSERVC-TARIF-CORRL OF GFCTB0F0.

           EXEC SQL
             SELECT  CSERVC_TARIF
               INTO :GFCTB0F0.CSERVC-TARIF
               FROM  DB2PRD.TARIF_CORRL_SIST
              WHERE  CSIST_CORRL        = :GFCTB0F0.CSIST-CORRL
                AND  CSERVC_TARIF_CORRL = :GFCTB0F0.CSERVC-TARIF-CORRL
           END-EXEC.

           IF  (SQLCODE                    NOT EQUAL ZEROS AND +100) OR
               (SQLWARN0                   EQUAL 'W')
                MOVE 'DB2'                 TO ERR-TIPO-ACESSO
                MOVE 'TARIF_CORRL_SIST  '  TO ERR-DBD-TAB
                MOVE 'SELECT    '          TO ERR-FUN-COMANDO
                MOVE SQLCODE               TO ERR-SQL-CODE
                MOVE '0020'                TO ERR-LOCAL
                MOVE SPACES                TO ERR-SEGM
                PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                     EQUAL +100
               MOVE 'N'                    TO WRK-TEM-TARIFA
           END-IF.

      *----------------------------------------------------------------*
       2500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VALIDAR TIPO MOTIVO NA TABELA GFCTB089                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2600-VALIDAR-TPO-MOTVO          SECTION.
      *----------------------------------------------------------------*

           MOVE WAE4-CTP-OPER-MOTVO    TO CTPO-OPER-MOTVO OF GFCTB089.
           MOVE WAE4-CMOTVO-JUSTF      TO CMOTVO-JUSTF    OF GFCTB089.

           EXEC SQL
             SELECT CTPO_OPER_MOTVO
             INTO  :GFCTB089.CTPO-OPER-MOTVO
             FROM   DB2PRD.MOTVO_JUSTF_OPER
             WHERE  CTPO_OPER_MOTVO        = :GFCTB089.CTPO-OPER-MOTVO
             AND    CMOTVO_JUSTF           = :GFCTB089.CMOTVO-JUSTF
           END-EXEC.

           IF (SQLCODE                NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'MOTVO_JUSTF_OPER' TO ERR-DBD-TAB
               MOVE 'SELECT '          TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '0021'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       2600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA QUE EFETUA PROCESSAMENTO FINAL DO PROGRAMA           *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           CLOSE ENTRADA
                 SAIDA
                 REJEITA.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

           PERFORM 3100-EMITIR-DISPLAY.

           STOP RUN.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE EXIBICAO DA ESTATISTICA DO PROCESSAMENTO          *
      ******************************************************************
      *----------------------------------------------------------------*
       3100-EMITIR-DISPLAY             SECTION.
      *----------------------------------------------------------------*

           IF (ACU-LIDOS               NOT EQUAL ZEROS)
               DISPLAY '*-----------------------------------------------
      -        '*'
               DISPLAY '*               PROGRAMA GFCT1963
      -        '*'
               DISPLAY '*               -----------------
      -        '*'
               DISPLAY '*
      -        '*'
               DISPLAY '*      PROCESSAMENTO EFETUADO COM SUCESSO
      -        '*'
               DISPLAY '*-----------------------------------------------
      -        '*'
               DISPLAY '*-----------------------------------------------
      -        '*'
               DISPLAY '************************************************
      -        '*'
               MOVE ACU-LIDOS          TO WRK-MASCARA
               DISPLAY '* TOTAL DE REGISTROS LIDOS      = ' WRK-MASCARA
                                                                     '
      -        '*'
               MOVE ACU-GRAV-SAIDA     TO WRK-MASCARA
               DISPLAY '* TOTAL DE REGISTROS GRAVADOS   = ' WRK-MASCARA
                                                                     '
      -        '*'
               MOVE ACU-GRAV-REJEITA   TO WRK-MASCARA
               DISPLAY '* TOTAL DE REGISTROS REJEITADOS = ' WRK-MASCARA
                                                                     '
      -        '*'
               DISPLAY '************************************************
      -        '*'
           ELSE
               DISPLAY '*-----------------------------------------------
      -        '*'
               DISPLAY '*               PROGRAMA GFCT1963
      -        '*'
               DISPLAY '*               -----------------
      -        '*'
               DISPLAY '*
      -        '*'
               DISPLAY '*           ARQUIVO DE ENTRADA VAZIO
      -        '*'
               DISPLAY '*-----------------------------------------------
      -        '*'
           END-IF.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE PROCESSAMENTE DE ERRRO                            *
      ******************************************************************
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT1963'             TO ERR-PGM.

           CALL 'POOL7100'             USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
