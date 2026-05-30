      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. GFCT0237.
       AUTHOR.     DIEGO MARCEL ZANOTTO.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT0237                                   *
      *    PROGRAMADOR.:   DIEGO MARCEL ZANOTTO    - CPM PATO BRANCO   *
      *    ANALISTA CPM:   MARCOS JOSE CAVEILEAN   - CPM PATO BRANCO   *
      *    ANALISTA....:   MARCELO                 - PROCKWORK GP/50   *
      *    DATA........:   06/04/2005                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   MANUTENCAO DE TARIFAS - REATIVACAO          *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                   TABLE                    INCLUDE/BOOK        *
      *                   -----                    ------------        *
      *                    DB2PRD.HIST_EVNTO_CTBIL   GFCTB085          *
      *                    DB2PRD.HIST_PARM_TARIF    GFCTB086          *
      *                    DB2PRD.HIST_TARIF_PRINC   GFCTB087          *
      *                    DB2PRD.HIST_VLR_COMPO     GFCTB088 HISTORIC *
      *                                                                *
      *                    DB2PRD.PARM_DATA_PROCM    GFCTB0A1          *
      *                                                                *
      *                    DB2PRD.SERVC_TARIF_PRINC  GFCTB0D8          *
      *                    DB2PRD.PARM_SERVC_TARIF   GFCTB0A2          *
      *                    DB2PRD.TARIF_EVNTO_CTBIL  GFCTB047          *
      *                    DB2PRD.VLR_COMPO_TARIF    GFCTB0G3 QUENTES  *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    I#GFCT7F - AREA DE COMUNICACAO ENTRADA                      *
      *    I#GFCT7G - AREA DE COMUNICACAO SAIDA                        *
      *    I#GFCT0M - AREA DE COMUNICACAO ERRO                         *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    GFCT5000 - OBTER DESCRICAO DE MENSAGENS                     *
      *    POOL0081 - ALOCAR DINAMICAMENTE UM MODULO                   *
      *    POOL7600 - OBTER DATA E HORA DO SISTEMA                     *
      *    POOL0025 - SOMAR/SUBTRAIR NUMERO DE DIAS DE UMA DATA        *
      *    POOL1205 - IDENTIFICAR DIAS UTEIS E INFORMACOES ADICIONAIS  *
      *                                                                *
      *----------------------------------------------------------------*
      *    ULTIMA ALTERACAO EM 05/09/2007 - VALERIA                    *
      *----------------------------------------------------------------*
      *  - INCLUSAO DOS CAMPOS                                         *
      *    CINDCD-QTD-REG                                              *
      *    CINDCD_TARIF_PROML                                          *
      *    CINDCD_TENTV_COBR                                           *
      *    QTENTV_SEM_COBR                                             *
      *    CINDCD_RETOR_COBR                                           *
      *    CINDCD_CALC_PRORT                                           *
      *    CINDCD_PER_CAREN                                            *
      *    QMES_CAREN_TARIF                                            *
      *                                                                *
      *----------------------------------------------------------------*
      *            SONDA PROCWORK - CONSULTORIA - ALTERACAO            *
      *----------------------------------------------------------------*
      *    ANALISTA....:   VINICIUS CRISTIAN MADUREIRA      / GRUPO 50 *
      *    DATA........:   12/02/2008                                  *
      *    OBJETIVO....:   CONSISTENCIA NA DATA DE INICIO E FIM DE     *
      *                    VIGENCIA.                                   *
      *                                                                *
      *----------------------------------------------------------------*
      *                 U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
      *        SONDA PROCWORK - CONSULTORIA - ALTERACAO - SP2902       *
      *----------------------------------------------------------------*
      *                                                                *
      *    ANALISTA....:  VALERIA TORQUATO   - SONDA/PROCWORK          *
      *    DATA........:  29/02/2008                                   *
      *    OBJETIVO....:  TRATAR A INCLUSAO DAS COLUNAS: CEMPR,        *
      *                   CGRP_RZ_CTBIL, CSGRP_RZ_CTBIL,               *
      *                   CCTA_EVNTO_CTBIL, CDOCTO_CTBIL, NAS          *
      *                   TABELAS GFCTB047, GFCTB085 E GFCTB087        *
      *                                                                *
      *================================================================*
161110*                      A L T E R A C A O                         *
161110*----------------------------------------------------------------*
161110*    RESPONSAVEL .: SONDAPROCWORK                                *
161110*    DATA ........: 16/11/2010                                   *
161110*    OBJETIVO ....: INCLUSAO DO TRATAMENTO DOS PARAMETROS DE     *
161110*                   PARCELAMENTO DE COBRANCA DA TARIFA E DO      *
161110*                   INDICADOR DE GERACAO DE CREDITO PARA         *
161110*                   CELULAR                                      *
161110*                                                                *
161110*================================================================*
BI0411*                 U L T I M A   A L T E R A C A O                *
BI0411*----------------------------------------------------------------*
BI0411*        SONDA PROCWORK - CONSULTORIA - ALTERACAO - BI0411       *
BI0411*----------------------------------------------------------------*
BI0411*                                                                *
BI0411*    ANALISTA....:  UBIRAJARA(BIRA) - SONDA/PROCWORK             *
BI0411*    DATA........:  04 / 2011                                    *
BI0411*    OBJETIVO....:  TRATAR O CAMPO NOVO CLCTO-ESTRN-ANTER DE     *
BI0411*                   ESTORNO DE TARIFAS SEMESTRE ANTERIOR.        *
BI0411*                                                                *
      *================================================================*
022012*----------------------------------------------------------------*
022012*                 U L T I M A   A L T E R A C A O                *
022012*----------------------------------------------------------------*
022012*        SONDA PROCWORK - CONSULTORIA - ALTERACAO                *
022012*----------------------------------------------------------------*
022012*                                                                *
022012*    PROGRAMADOR.:  FABRICA         - SONDA/PROCWORK             *
022012*    ANALISTA....:  PAGNOCCA        - SONDA/PROCWORK             *
022012*    DATA........:  FEV / 2012                                   *
022012*    OBJETIVO....:  PROJETO FRANQUIA INVERSA.                    *
022012*                                                                *
022012*================================================================*
      *================================================================*
      *   EM 20.05.2013 ALTERADO POR PAGNOCCA SONDA IT APENAS COMPILADO*
      *   DEVIDO ALTERACAO DO CAMPO DE DIAS DE CARENCIA PARA 9(3).     *
      *                                                                *
BI0115*----------------------------------------------------------------*
BI0115*                 U L T I M A   A L T E R A C A O                *
BI0115*----------------------------------------------------------------*
BI0115*                 SONDA IT  - ALTERACAO - BI0115                 *
BI0115*----------------------------------------------------------------*
BI0115*                                                                *
BI0115*    ANALISTA....:  UBIRAJARA(BIRA) - SONDA - IT                 *
BI0115*    DATA........:  01 / 2015                                    *
BI0115*    OBJETIVO....:  TRATAR TIPO DE TARIFA = 4                    *
BI0115*                   ( PACOTE PERSONALIZADO )                     *

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

       01  WRK-CAMPO-TIRA-SINAL        PIC +9(017)         VALUE ZEROS.
       01  FILLER  REDEFINES           WRK-CAMPO-TIRA-SINAL.
           05  WRK-CAMPO-TIRA-SINAL-S  PIC X.
           05  WRK-CAMPO-TIRA-SINAL-17 PIC 9(17).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(01).
               10 WRK-CAMPO-TIRA-SINAL-16 PIC 9(16).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(02).
               10 WRK-CAMPO-TIRA-SINAL-15 PIC 9(15).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(03).
               10 WRK-CAMPO-TIRA-SINAL-14 PIC 9(14).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(04).
               10 WRK-CAMPO-TIRA-SINAL-13 PIC 9(13).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(05).
               10 WRK-CAMPO-TIRA-SINAL-12 PIC 9(12).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(06).
               10 WRK-CAMPO-TIRA-SINAL-11 PIC 9(11).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(07).
               10 WRK-CAMPO-TIRA-SINAL-10 PIC 9(10).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(08).
               10 WRK-CAMPO-TIRA-SINAL-09 PIC 9(09).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(09).
               10 WRK-CAMPO-TIRA-SINAL-08 PIC 9(08).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(10).
               10 WRK-CAMPO-TIRA-SINAL-07 PIC 9(07).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(11).
               10 WRK-CAMPO-TIRA-SINAL-06 PIC 9(06).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(12).
               10 WRK-CAMPO-TIRA-SINAL-05 PIC 9(05).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(13).
               10 WRK-CAMPO-TIRA-SINAL-04 PIC 9(04).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(14).
               10 WRK-CAMPO-TIRA-SINAL-03 PIC 9(03).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(15).
               10 WRK-CAMPO-TIRA-SINAL-02 PIC 9(02).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(16).
               10 WRK-CAMPO-TIRA-SINAL-01 PIC 9(01).

       01  WRK-CAMPO-POE-SINAL-01        PIC +9(01)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-01.
           05  WRK-CAMPO-POE-SINAL-01-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-01-C  PIC 9(01).

       01  WRK-CAMPO-POE-SINAL-02        PIC +9(02)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-02.
           05  WRK-CAMPO-POE-SINAL-02-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-02-C  PIC 9(02).

       01  WRK-CAMPO-POE-SINAL-03        PIC +9(03)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-03.
           05  WRK-CAMPO-POE-SINAL-03-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-03-C  PIC 9(03).

       01  WRK-CAMPO-POE-SINAL-04        PIC +9(04)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-04.
           05  WRK-CAMPO-POE-SINAL-04-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-04-C  PIC 9(04).

       01  WRK-CAMPO-POE-SINAL-05        PIC +9(05)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-05.
           05  WRK-CAMPO-POE-SINAL-05-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-05-C  PIC 9(05).

       01  WRK-CAMPO-POE-SINAL-06        PIC +9(06)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-06.
           05  WRK-CAMPO-POE-SINAL-06-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-06-C  PIC 9(06).

       01  WRK-CAMPO-POE-SINAL-07        PIC +9(07)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-07.
           05  WRK-CAMPO-POE-SINAL-07-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-07-C  PIC 9(07).

       01  WRK-CAMPO-POE-SINAL-08        PIC +9(08)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-08.
           05  WRK-CAMPO-POE-SINAL-08-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-08-C  PIC 9(08).

       01  WRK-CAMPO-POE-SINAL-09        PIC +9(09)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-09.
           05  WRK-CAMPO-POE-SINAL-09-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-09-C  PIC 9(09).

       01  WRK-CAMPO-POE-SINAL-10        PIC +9(10)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-10.
           05  WRK-CAMPO-POE-SINAL-10-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-10-C  PIC 9(10).

       01  WRK-CAMPO-POE-SINAL-11        PIC +9(11)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-11.
           05  WRK-CAMPO-POE-SINAL-11-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-11-C  PIC 9(11).

       01  WRK-CAMPO-POE-SINAL-12        PIC +9(12)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-12.
           05  WRK-CAMPO-POE-SINAL-12-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-12-C  PIC 9(12).

       01  WRK-CAMPO-POE-SINAL-13        PIC +9(13)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-13.
           05  WRK-CAMPO-POE-SINAL-13-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-13-C  PIC 9(13).

       01  WRK-CAMPO-POE-SINAL-14        PIC +9(14)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-14.
           05  WRK-CAMPO-POE-SINAL-14-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-14-C  PIC 9(14).

       01  WRK-CAMPO-POE-SINAL-15        PIC +9(15)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-15.
           05  WRK-CAMPO-POE-SINAL-15-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-15-C  PIC 9(15).

       01  WRK-CAMPO-POE-SINAL-16        PIC +9(16)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-16.
           05  WRK-CAMPO-POE-SINAL-16-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-16-C  PIC 9(16).

       01  WRK-CAMPO-POE-SINAL-17        PIC +9(17)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-17.
           05  WRK-CAMPO-POE-SINAL-17-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-17-C  PIC 9(17).


      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** GFCT6230 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*

       77  WRK-FIM-CURSOR              PIC  X(001)         VALUE SPACES.
       77  WRK-FIM-47                  PIC  X(001)         VALUE SPACES.
       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-CSERVC-TARIF            PIC  9(005)         VALUE ZEROS.
       77  WRK-DINIC-VGCIA-TARIF       PIC  X(010)         VALUE SPACES.
       77  WRK-CSEQ-MANUT-SERVC        PIC  9(002)         VALUE ZEROS.
       77  WRK-HIDTFD-MANUT-SERVC      PIC  X(026)         VALUE SPACES.
       77  WRK-CSEQ-MANUT-SERVC-NVA    PIC  9(002)         VALUE ZEROS.
       77  W-JUNCAO-USUARIO            PIC  9(005)         VALUE ZEROS.
       77  W-JUNCAO-MASTER             PIC  9(005)         VALUE ZEROS.
       77  W-CTPO-SERVC-TARIF          PIC S9(1)V USAGE COMP-3 VALUE 0.
       77  W-TREIS                     PIC S9(1)V USAGE COMP-3 VALUE 3.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA AUXILIARES DE DATA ***'.
      *----------------------------------------------------------------*

       01  WRK-DATA-AUX.
           03  WRK-DIA-AUX             PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  WRK-MES-AUX             PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  WRK-ANO-AUX             PIC  9(004)         VALUE ZEROS.

       01  WRK-DATA-AUX2.
           03  WRK-ANO-AUX             PIC  9(004)         VALUE ZEROS.
           03  WRK-MES-AUX             PIC  9(002)         VALUE ZEROS.
           03  WRK-DIA-AUX             PIC  9(002)         VALUE ZEROS.

       01  WRK-DATA-AUX3.
           03  WRK-ANO-AUX             PIC  9(004)         VALUE ZEROS.
           03  WRK-MES-AUX             PIC  9(002)         VALUE ZEROS.
           03  WRK-DIA-AUX             PIC  9(002)         VALUE ZEROS.

       01  WRK-DATA-POL                PIC  9(008)         VALUE ZEROS.
       01  WRK-DATA-POL-R              REDEFINES WRK-DATA-POL.
           03  WRK-ANO-AUX             PIC  9(004).
           03  WRK-MES-AUX             PIC  9(002).
           03  WRK-DIA-AUX             PIC  9(002).

       01  WRK-DATA-ANO1.
           03  WRK-ANO-AUX1            PIC  9(004)         VALUE ZEROS.

       01  WRK-DATA-ANO2.
           03  WRK-ANO-AUX2            PIC  9(004)         VALUE ZEROS.

       01  WRK-TIMESTAMP-AUX.
           03  WRK-ANO                 PIC  9(004)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '-'.
           03  WRK-MES                 PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '-'.
           03  WRK-DIA                 PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '-'.
           03  WRK-HOR                 PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '.'.
           03  WRK-MIN                 PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '.'.
           03  WRK-SEG                 PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '.'.
           03  WRK-MIL                 PIC  9(006)         VALUE ZEROS.

       01  WRK-DATA-FIM-VIG            PIC  9(009)  COMP-3 VALUE ZEROS.

       01  WRK-DATA-AUX-3              PIC  9(008)         VALUE ZEROS.
       01  WRK-DATA-AUX-2              REDEFINES WRK-DATA-AUX-3.
           03  WRK-ANO-AUX-2           PIC  9(004).
           03  WRK-MES-AUX-2           PIC  9(002).
           03  WRK-DIA-AUX-2           PIC  9(002).

       01  WRK-DATA-POOL25.
           05 WRK-ANO-POOL25               PIC  9(004)   VALUE ZEROS.
           05 WRK-MES-POOL25               PIC  9(002)   VALUE ZEROS.
           05 WRK-DIA-POOL25               PIC  9(002)   VALUE ZEROS.

       01  WRK-DATA-E25                    PIC  9(009)   VALUE ZEROS.
       01  WRK-DATA-E25-R REDEFINES WRK-DATA-E25.
           05 FILLER                       PIC  X(001).
           05 WRK-ANO-E25                  PIC  9(004).
           05 WRK-MES-E25                  PIC  9(002).
           05 WRK-DIA-E25                  PIC  9(002).

       01  WRK-DATA-S25                    PIC  9(009)   VALUE ZEROS.
       01  WRK-DATA-S25-R REDEFINES WRK-DATA-S25.
           05 FILLER                       PIC  X(001).
           05 WRK-ANO-S25                  PIC  9(004).
           05 WRK-MES-S25                  PIC  9(002).
           05 WRK-DIA-S25                  PIC  9(002).

       01  WRK-DATA-ENTRADA            PIC   9(009) COMP-3  VALUE ZEROS.
       01  WRK-NUMERO-DIAS             PIC  S9(005) COMP-3  VALUE ZEROS.
       01  WRK-DATA-SAIDA              PIC   9(009) COMP-3  VALUE ZEROS.

       01  WRK-DATA-INI.
           03  WRK-ANO-INI             PIC  9(004)         VALUE ZEROS.
           03  WRK-MES-INI             PIC  9(002)         VALUE ZEROS.
           03  WRK-DIA-INI             PIC  9(002)         VALUE ZEROS.

       01  WRK-DATA-FIM.
           03  WRK-ANO-FIM             PIC  9(004)         VALUE ZEROS.
           03  WRK-MES-FIM             PIC  9(002)         VALUE ZEROS.
           03  WRK-DIA-FIM             PIC  9(002)         VALUE ZEROS.

       01  WRK-DINIC-VGCIA.
           03  WRK-DINIC-DIA           PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '.'.
           03  WRK-DINIC-MES           PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '.'.
           03  WRK-DINIC-ANO           PIC  9(004)         VALUE ZEROS.

       01  WRK-DFIM-VGCIA.
           03  WRK-DFIM-DIA            PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '.'.
           03  WRK-DFIM-MES            PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '.'.
           03  WRK-DFIM-ANO            PIC  9(004)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREAS DA POOL1205 ***'.
      *----------------------------------------------------------------*
       01  WRK-VERSAO-0315             PIC  X(06)          VALUE SPACES.
       01  WRK-MENSAGEM-0315.
           03  WRK-COD-MSG-0315        PIC  9(03)          VALUE ZEROS.
           03  WRK-HIFEN-MENSAGEM-0315 PIC  X(01)          VALUE SPACES.
           03  WRK-TEXTO-MENSAGEM-0315 PIC  X(79)          VALUE SPACES.
       01  WRK-COD-FUNCIONAL-0315      PIC  X(07)          VALUE SPACES.
       01  WRK-NOME-FUNC-0315          PIC  X(40)          VALUE SPACES.
       01  WRK-COD-BANCO-0315          PIC  9(05)          VALUE ZEROS.
       01  WRK-NOME-BANCO-0315         PIC  X(40)          VALUE SPACES.
       01  WRK-CNPJ-BANCO-0315.
           05  WRK-CNPJ-PRI-0315       PIC  9(09)          VALUE ZEROS.
           05  WRK-CNPJ-FIL-0315       PIC  9(05)          VALUE ZEROS.
           05  WRK-CNPJ-CON-0315       PIC  9(02)          VALUE ZEROS.
       01  WRK-COD-JUNCAO-0315         PIC  9(05)          VALUE ZEROS.
       01  WRK-NOME-DEPTO-0315         PIC  X(40)          VALUE SPACES.
       01  WRK-SECAO-0315              PIC  9(03)          VALUE ZEROS.
       01  WRK-STATUS-0315             PIC  X(01)          VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREAS DA POOL1205 ***'.
      *----------------------------------------------------------------*

       01  WRK-MENSAGEM-1205           PIC  X(050)         VALUE SPACES.

       01  WRK-AREA-POOL1205.
           03  WRK-DT-ENVIADA          PIC  9(008)  COMP-3 VALUE ZEROS.
           03  WRK-OPCAO               PIC  X(001)         VALUE SPACES.
           03  WRK-DT-JULIANA          PIC  9(007)  COMP-3 VALUE ZEROS.
           03  WRK-DT-EDITADA          PIC  X(010)         VALUE SPACES.
           03  WRK-DT-GREGORI          PIC  9(008)         VALUE ZEROS.
           03  WRK-DIA-SEMANA          PIC  X(013)         VALUE SPACES.
           03  WRK-MES-EXTENSO         PIC  X(009)         VALUE SPACES.
           03  WRK-DIA-UT-ANTER        PIC  9(008)         VALUE ZEROS.
           03  WRK-DIA-UT-POSTE        PIC  9(008)         VALUE ZEROS.

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
           '*** AREA DO MODULO GFCT5000 ***'.
      *----------------------------------------------------------------*

       01  WRK-5000-AREA.
           03  WRK-5000-AREA-ENVIA.
               05  WRK-5000-AMBIENTE     PIC  X(001)       VALUE SPACES.
               05  WRK-5000-COD-MSG      PIC  9(004)       VALUE ZEROS.
           03  WRK-5000-AREA-RECEBE.
               05  WRK-5000-COD-RETORNO  PIC  9(002)       VALUE ZEROS.
               05  WRK-5000-COD-SQL-ERRO PIC  9(003)       VALUE ZEROS.
               05  WRK-5000-DESC-MSG     PIC  X(070)       VALUE SPACES.

           EJECT
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA DB2 ***'.
      *----------------------------------------------------------------*

           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.

           EJECT
           EXEC SQL
                INCLUDE GFCTB085
           END-EXEC.

           EJECT
           EXEC SQL
                INCLUDE GFCTB086
           END-EXEC.

           EJECT
           EXEC SQL
                INCLUDE GFCTB087
           END-EXEC.

           EJECT
           EXEC SQL
                INCLUDE GFCTB088
           END-EXEC.

           EJECT
           EXEC SQL
                INCLUDE GFCTB0A0
           END-EXEC.

           EJECT
           EXEC SQL
                INCLUDE GFCTB0A1
           END-EXEC.

           EJECT
           EXEC SQL
                INCLUDE GFCTB0D8
           END-EXEC.

           EJECT
           EXEC SQL
                INCLUDE GFCTB0A2
           END-EXEC.

           EJECT
           EXEC SQL
                INCLUDE GFCTB047
           END-EXEC.

           EJECT
           EXEC SQL
                INCLUDE GFCTB0G3
           END-EXEC.


           EXEC SQL
                DECLARE CSR01-GFCTB047 CURSOR FOR
                SELECT  CSERVC_TARIF,
                        CTPO_LCTO_CTBIL,
                        CSEQ_EVNTO_CTBIL
                FROM    DB2PRD.TARIF_EVNTO_CTBIL
                WHERE   CSERVC_TARIF       = :GFCTB047.CSERVC-TARIF
           END-EXEC.

           EXEC SQL
             DECLARE CRS-GFCTB088 CURSOR   FOR
               SELECT
                   CSERVC_TARIF,
                   HIDTFD_MANUT_SERVC,
                   CSEQ_MANUT_SERVC,
                   DINIC_VGCIA_TARIF,
                   CSEQ_COMPO_TARIF,
                   RCOMPO_TARIF,
                   VCOMPO_TARIF,
                   PSERVC_PTADO,
                   VMAX_COMPO_TARIF,
                   VMIN_COMPO_TARIF,
                   CMOMEN_CONVS_MOEDA,
                   CMOEDA_BACEN
             FROM  DB2PRD.HIST_VLR_COMPO
             WHERE
                   CSERVC_TARIF       = :GFCTB088.CSERVC-TARIF       AND
                   HIDTFD_MANUT_SERVC = :GFCTB088.HIDTFD-MANUT-SERVC AND
                   CSEQ_MANUT_SERVC   = :GFCTB088.CSEQ-MANUT-SERVC
           END-EXEC.


           EXEC SQL
             DECLARE CRS-GFCTB085 CURSOR   FOR
               SELECT
                   CSERVC_TARIF,
                   HIDTFD_MANUT_SERVC,
                   CSEQ_MANUT_SERVC,
                   CTPO_LCTO_CTBIL,
                   CSEQ_EVNTO_CTBIL,
                   CINDCD_DEPDC_CTBIL,
                   PATRIC_CTBIL,
                   CDEPDC_CTLZO,
                   CPRODT_CTBIL,
                   COPER_CTBIL,
                   CEMPR,
                   CGRP_RZ_CTBIL,
                   CSGRP_RZ_CTBIL,
                   CCTA_EVNTO_CTBIL,
                   CDOCTO_CTBIL
             FROM   DB2PRD.HIST_EVNTO_CTBIL
             WHERE
                   CSERVC_TARIF       = :GFCTB085.CSERVC-TARIF       AND
                   HIDTFD_MANUT_SERVC = :GFCTB085.HIDTFD-MANUT-SERVC AND
                   CSEQ_MANUT_SERVC   = :GFCTB085.CSEQ-MANUT-SERVC
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** GFCT6230- FIM DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY 'I#GFCT7F'.

           EJECT
       COPY 'I#GFCT7G'.

           EJECT
       COPY 'I#GFCT0M'.

           EJECT
      *================================================================*
       PROCEDURE DIVISION              USING GFCT7F-ENTRADA
                                             GFCT7G-SAIDA
                                             GFCT0M-AREA-ERROS.
      *================================================================*

           MOVE '+' TO
               WRK-CAMPO-TIRA-SINAL-S
               WRK-CAMPO-POE-SINAL-01-S
               WRK-CAMPO-POE-SINAL-02-S
               WRK-CAMPO-POE-SINAL-03-S
               WRK-CAMPO-POE-SINAL-04-S
               WRK-CAMPO-POE-SINAL-05-S
               WRK-CAMPO-POE-SINAL-06-S
               WRK-CAMPO-POE-SINAL-07-S
               WRK-CAMPO-POE-SINAL-08-S
               WRK-CAMPO-POE-SINAL-09-S
               WRK-CAMPO-POE-SINAL-10-S
               WRK-CAMPO-POE-SINAL-11-S
               WRK-CAMPO-POE-SINAL-12-S
               WRK-CAMPO-POE-SINAL-13-S
               WRK-CAMPO-POE-SINAL-14-S
               WRK-CAMPO-POE-SINAL-15-S
               WRK-CAMPO-POE-SINAL-16-S
               WRK-CAMPO-POE-SINAL-17-S.
            MOVE ZEROS TO
               WRK-CAMPO-TIRA-SINAL
               WRK-CAMPO-TIRA-SINAL-17
               WRK-CAMPO-TIRA-SINAL-16
               WRK-CAMPO-TIRA-SINAL-15
               WRK-CAMPO-TIRA-SINAL-14
               WRK-CAMPO-TIRA-SINAL-13
               WRK-CAMPO-TIRA-SINAL-12
               WRK-CAMPO-TIRA-SINAL-11
               WRK-CAMPO-TIRA-SINAL-10
               WRK-CAMPO-TIRA-SINAL-09
               WRK-CAMPO-TIRA-SINAL-08
               WRK-CAMPO-TIRA-SINAL-07
               WRK-CAMPO-TIRA-SINAL-06
               WRK-CAMPO-TIRA-SINAL-05
               WRK-CAMPO-TIRA-SINAL-04
               WRK-CAMPO-TIRA-SINAL-03
               WRK-CAMPO-TIRA-SINAL-02
               WRK-CAMPO-TIRA-SINAL-01
               WRK-CAMPO-POE-SINAL-01
               WRK-CAMPO-POE-SINAL-01-C
               WRK-CAMPO-POE-SINAL-02
               WRK-CAMPO-POE-SINAL-02-C
               WRK-CAMPO-POE-SINAL-03
               WRK-CAMPO-POE-SINAL-03-C
               WRK-CAMPO-POE-SINAL-04
               WRK-CAMPO-POE-SINAL-04-C
               WRK-CAMPO-POE-SINAL-05
               WRK-CAMPO-POE-SINAL-05-C
               WRK-CAMPO-POE-SINAL-06
               WRK-CAMPO-POE-SINAL-06-C
               WRK-CAMPO-POE-SINAL-07
               WRK-CAMPO-POE-SINAL-07-C
               WRK-CAMPO-POE-SINAL-08
               WRK-CAMPO-POE-SINAL-08-C
               WRK-CAMPO-POE-SINAL-09
               WRK-CAMPO-POE-SINAL-09-C
               WRK-CAMPO-POE-SINAL-10
               WRK-CAMPO-POE-SINAL-10-C
               WRK-CAMPO-POE-SINAL-11
               WRK-CAMPO-POE-SINAL-11-C
               WRK-CAMPO-POE-SINAL-12
               WRK-CAMPO-POE-SINAL-12-C
               WRK-CAMPO-POE-SINAL-13
               WRK-CAMPO-POE-SINAL-13-C
               WRK-CAMPO-POE-SINAL-14
               WRK-CAMPO-POE-SINAL-14-C
               WRK-CAMPO-POE-SINAL-15
               WRK-CAMPO-POE-SINAL-15-C
               WRK-CAMPO-POE-SINAL-16
               WRK-CAMPO-POE-SINAL-16-C
               WRK-CAMPO-POE-SINAL-17
               WRK-CAMPO-POE-SINAL-17-C.



      *----------------------------------------------------------------*
       000000-ROTINA-PRINCIPAL         SECTION.
      *----------------------------------------------------------------*

           PERFORM 100000-INICIALIZAR.

           PERFORM 200000-PROCESSAR.

BI0115     IF W-CTPO-SERVC-TARIF       EQUAL 2 OR 4
              MOVE 01                  TO GFCT7G-ERRO
              MOVE ZEROS               TO GFCT7G-COD-SQL-ERRO
              MOVE 0393                TO GFCT7G-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
              PERFORM                  121000-OBTER-DESC-MSG
              MOVE WRK-5000-DESC-MSG   TO GFCT7G-DESC-MSG-ERRO
           END-IF.

           PERFORM 300000-FINALIZAR.

      *----------------------------------------------------------------*
       000000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-DELETE-47 SECTION.
      *----------------------------------------------------------------*
            PERFORM 1105-OPEN-47.

            MOVE 'N' TO WRK-FIM-47.

            PERFORM 1110-FETCH-47 UNTIL WRK-FIM-47 EQUAL 'S'.

            PERFORM 1120-CLOSE-47.

      *----------------------------------------------------------------*
       1000-99-FIM.   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1105-OPEN-47 SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF OF GFCTB087 TO CSERVC-TARIF OF GFCTB047.

           EXEC SQL
                OPEN CSR01-GFCTB047
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'GFCT0237'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'TARIF_EVNTO_CTBIL' TO GFCT0M-NOME-TAB
               MOVE 'OPEN'         TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE  '0910'            TO GFCT0M-LOCAL
               MOVE  SQLCA(1:136) TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCT7G-ERRO
               MOVE  SQLCODE           TO WRK-CAMPO-TIRA-SINAL
               MOVE  WRK-CAMPO-TIRA-SINAL-03 TO GFCT7G-COD-SQL-ERRO
               MOVE  0012              TO GFCT7G-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 121000-OBTER-DESC-MSG
               STRING WRK-5000-DESC-MSG
                      ' - GFCTB047'
               DELIMITED BY '  '       INTO GFCT7G-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.


      *----------------------------------------------------------------*
       1105-99-FIM. EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1110-FETCH-47 SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                FETCH CSR01-GFCTB047
                INTO  :GFCTB047.CSERVC-TARIF,
                      :GFCTB047.CTPO-LCTO-CTBIL,
                      :GFCTB047.CSEQ-EVNTO-CTBIL
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'GFCT0237'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'TARIF_EVNTO_CTBIL' TO GFCT0M-NOME-TAB
               MOVE 'FETCH'         TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE  '0911'            TO GFCT0M-LOCAL
               MOVE  SQLCA(1:136) TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCT7G-ERRO
               MOVE  SQLCODE           TO WRK-CAMPO-TIRA-SINAL
               MOVE  WRK-CAMPO-TIRA-SINAL-03 TO GFCT7G-COD-SQL-ERRO
               MOVE  0012              TO GFCT7G-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 121000-OBTER-DESC-MSG
               STRING WRK-5000-DESC-MSG
                      ' - GFCTB047'
               DELIMITED BY '  '       INTO GFCT7G-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-47
           ELSE
              EXEC SQL
                 DELETE FROM  DB2PRD.TARIF_EVNTO_CTBIL
                 WHERE CSERVC_TARIF = :GFCTB047.CSERVC-TARIF
                 AND   CTPO_LCTO_CTBIL = :GFCTB047.CTPO-LCTO-CTBIL
                 AND   CSEQ_EVNTO_CTBIL = :GFCTB047.CSEQ-EVNTO-CTBIL
              END-EXEC
              IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
                 (SQLWARN0                EQUAL 'W')
                  MOVE 'GFCT0237'         TO GFCT0M-PROGRAMA
                  MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
                  MOVE 'TARIF_EVNTO_CTBIL' TO GFCT0M-NOME-TAB
                  MOVE 'DELETE'         TO GFCT0M-COMANDO-SQL
                  MOVE  SQLCODE           TO GFCT0M-SQLCODE
                  MOVE  '0912'            TO GFCT0M-LOCAL
                  MOVE  SQLCA(1:136) TO GFCT0M-SQLCA-AREA
                  MOVE  9                 TO GFCT7G-ERRO
               MOVE  SQLCODE           TO WRK-CAMPO-TIRA-SINAL
               MOVE  WRK-CAMPO-TIRA-SINAL-03 TO GFCT7G-COD-SQL-ERRO
                  MOVE  0012              TO GFCT7G-COD-MSG-ERRO
                                             WRK-5000-COD-MSG
                  PERFORM 121000-OBTER-DESC-MSG
                  STRING WRK-5000-DESC-MSG
                         ' - GFCTB047'
                  DELIMITED BY '  '       INTO GFCT7G-DESC-MSG-ERRO
                  PERFORM 300000-FINALIZAR
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM. EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1120-CLOSE-47 SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                CLOSE CSR01-GFCTB047
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'GFCT0237'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'TARIF_EVNTO_CTBIL' TO GFCT0M-NOME-TAB
               MOVE 'FETCH'         TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE  '0913'            TO GFCT0M-LOCAL
               MOVE  SQLCA(1:136) TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCT7G-ERRO
               MOVE  SQLCODE           TO WRK-CAMPO-TIRA-SINAL
               MOVE  WRK-CAMPO-TIRA-SINAL-03 TO GFCT7G-COD-SQL-ERRO
               MOVE  0012              TO GFCT7G-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 121000-OBTER-DESC-MSG
               STRING WRK-5000-DESC-MSG
                      ' - GFCTB047'
               DELIMITED BY '  '       INTO GFCT7G-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM. EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       100000-INICIALIZAR              SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCT0M-AREA-ERROS
                                          GFCT7G-SAIDA.

           INITIALIZE GFCT0M-AREA-ERROS
                      GFCT7G-SAIDA.

           PERFORM 120000-MONTAR-AREA-SAIDA.

      *----------------------------------------------------------------*
       100000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       120000-MONTAR-AREA-SAIDA        SECTION.
      *----------------------------------------------------------------*

           MOVE 1010                    TO GFCT7G-LL.
           MOVE GFCT7F-TRANSACAO        TO GFCT7G-TRANSACAO.
           MOVE GFCT7F-FUNCAO           TO GFCT7G-FUNCAO.
           MOVE GFCT7F-QTDE-OCOR        TO GFCT7G-QTDE-OCOR.
           MOVE GFCT7F-QTDE-TOT-REG     TO GFCT7G-QTDE-TOT-REG.
           MOVE GFCT7F-FUNC-BDSCO       TO GFCT7G-FUNC-BDSCO.
           MOVE GFCT7F-TERMINAL         TO GFCT7G-TERMINAL.
           MOVE GFCT7F-TARIFA           TO GFCT7G-TARIFA.
           MOVE GFCT7F-DT-INI-VGCIA-ANT TO GFCT7G-DT-INI-VGCIA-ANT.
           MOVE GFCT7F-DT-FIM-VGCIA-ANT TO GFCT7G-DT-FIM-VGCIA-ANT.
           MOVE GFCT7F-DT-INI-VGCIA     TO GFCT7G-DT-INI-VGCIA.
           MOVE GFCT7F-DT-FIM-VGCIA     TO GFCT7G-DT-FIM-VGCIA.
           MOVE ZEROS                   TO GFCT7G-COD-SQL-ERRO
                                           GFCT7G-ERRO
                                           GFCT7G-COD-MSG-ERRO
                                           WRK-5000-COD-MSG.

           PERFORM 121000-OBTER-DESC-MSG.

           MOVE WRK-5000-DESC-MSG       TO GFCT7G-DESC-MSG-ERRO.

           MOVE 'S'                     TO GFCT7G-FIM.

      *----------------------------------------------------------------*
       120000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       121000-OBTER-DESC-MSG           SECTION.
      *----------------------------------------------------------------*

           MOVE 'O'                            TO WRK-5000-AMBIENTE.
           MOVE 'GFCT5000'                     TO WRK-MODULO.

           CALL WRK-MODULO USING
                                                   WRK-5000-AREA-ENVIA
                                                   WRK-5000-AREA-RECEBE.

           IF  WRK-5000-COD-RETORNO            NOT EQUAL ZEROS
               IF  WRK-5000-COD-RETORNO        EQUAL 01
                   MOVE 9                      TO GFCT7G-ERRO
                   MOVE ZEROS                  TO GFCT7G-COD-SQL-ERRO
                   MOVE ZEROS                  TO GFCT7G-COD-MSG-ERRO
                   MOVE WRK-5000-DESC-MSG      TO GFCT7G-DESC-MSG-ERRO
               ELSE
                   IF  WRK-5000-COD-RETORNO    EQUAL 99
                       MOVE 9                  TO GFCT7G-ERRO
                       MOVE WRK-5000-COD-SQL-ERRO
                                               TO GFCT7G-COD-SQL-ERRO
                       MOVE ZEROS              TO GFCT7G-COD-MSG-ERRO
                       MOVE WRK-5000-DESC-MSG  TO GFCT7G-DESC-MSG-ERRO
                   END-IF
               PERFORM 300000-FINALIZAR
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       121000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       200000-PROCESSAR                SECTION.
      *----------------------------------------------------------------*

           IF  GFCT7F-TRANSACAO        EQUAL SPACES OR
               GFCT7F-FUNCAO           EQUAL SPACES OR
               GFCT7F-TARIFA           NOT NUMERIC  OR
               GFCT7F-TARIFA           EQUAL ZEROS  OR
               (GFCT7F-QTDE-OCOR       NOT NUMERIC) OR
               GFCT7F-FUNC-BDSCO       EQUAL SPACES
               MOVE 1                  TO GFCT7G-ERRO
               MOVE ZEROS              TO GFCT7G-COD-SQL-ERRO
               MOVE 0001               TO GFCT7G-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 121000-OBTER-DESC-MSG
               MOVE WRK-5000-DESC-MSG  TO GFCT7G-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

           PERFORM 210000-ACESSAR-GFCTB0A1.

           PERFORM 220000-CONSISTIR-DATA.

           PERFORM 225000-VESEJATEM.

           PERFORM 230000-GERAR-REATIVACAO.

      *----------------------------------------------------------------*
       200000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       210000-ACESSAR-GFCTB0A1         SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC      OF GFCTB0A1.

           EXEC SQL
             SELECT
                   CSIST_PRINC,
                   DPROCM_ANTER,
                   DPROCM_ATUAL,
                   DPROCM_PROX
             INTO
                   :GFCTB0A1.CSIST-PRINC,
                   :GFCTB0A1.DPROCM-ANTER,
                   :GFCTB0A1.DPROCM-ATUAL,
                   :GFCTB0A1.DPROCM-PROX
             FROM   DB2PRD.PARM_DATA_PROCM
             WHERE
                   CSIST_PRINC        = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'GFCT0237'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'PARM_DATA_PROCM ' TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE  '0010'            TO GFCT0M-LOCAL
               MOVE  SQLCA(1:136) TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCT7G-ERRO
               MOVE  SQLCODE           TO GFCT7G-COD-SQL-ERRO
               MOVE  0012              TO GFCT7G-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 121000-OBTER-DESC-MSG
               STRING WRK-5000-DESC-MSG
                      ' - GFCTB0A1'
               DELIMITED BY '  '       INTO GFCT7G-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       210000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       220000-CONSISTIR-DATA           SECTION.
      *----------------------------------------------------------------*

           MOVE GFCT7F-DT-FIM-VGCIA     TO WRK-DATA-AUX.
           MOVE WRK-ANO-AUX OF WRK-DATA-AUX
                                        TO WRK-ANO-AUX1.
           MOVE DPROCM-ATUAL  OF GFCTB0A1
                                        TO WRK-DATA-AUX.
           MOVE WRK-ANO-AUX OF WRK-DATA-AUX
                                        TO WRK-ANO-AUX2.

           MOVE GFCT7F-DT-INI-VGCIA     TO WRK-DATA-AUX.
           MOVE CORR WRK-DATA-AUX       TO WRK-DATA-POL-R.

           MOVE WRK-DATA-POL            TO WRK-DT-ENVIADA.
           MOVE SPACES                  TO WRK-OPCAO.

           CALL  'POOL1205'             USING WRK-AREA-POOL1205
                                             WRK-MENSAGEM-1205.

           IF  RETURN-CODE              NOT EQUAL ZEROS
               MOVE 01                  TO GFCT7G-ERRO
               MOVE ZEROS               TO GFCT7G-COD-SQL-ERRO
               MOVE 0396                TO GFCT7G-COD-MSG-ERRO
                                           WRK-5000-COD-MSG
               PERFORM 121000-OBTER-DESC-MSG
               MOVE WRK-5000-DESC-MSG   TO GFCT7G-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

           MOVE GFCT7F-DT-FIM-VGCIA     TO WRK-DATA-AUX.
           MOVE CORR  WRK-DATA-AUX      TO WRK-DATA-AUX2.

      *--- VALIDAR ANO DE FIM DE VIGENCIA PARA TARIFA PACOTE

           IF  WRK-ANO-AUX              OF WRK-DATA-AUX2
                                        GREATER  2098
               MOVE 01                  TO GFCT7G-ERRO
               MOVE ZEROS               TO GFCT7G-COD-SQL-ERRO
               MOVE 1074                TO GFCT7G-COD-MSG-ERRO
                                           WRK-5000-COD-MSG
               PERFORM 121000-OBTER-DESC-MSG
               MOVE WRK-5000-DESC-MSG   TO GFCT7G-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

           MOVE GFCT7G-DT-INI-VGCIA-ANT
                                        TO WRK-DATA-AUX.
           MOVE CORR WRK-DATA-AUX       TO WRK-DATA-AUX3.

           IF  WRK-DATA-AUX2            LESS WRK-DATA-AUX3
               MOVE 01                  TO GFCT7G-ERRO
               MOVE ZEROS               TO GFCT7G-COD-SQL-ERRO
               MOVE 0123                TO GFCT7G-COD-MSG-ERRO
                                           WRK-5000-COD-MSG
               PERFORM 121000-OBTER-DESC-MSG
               MOVE WRK-5000-DESC-MSG   TO GFCT7G-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

           MOVE GFCT7G-DT-INI-VGCIA     TO WRK-DATA-AUX.
           MOVE CORR WRK-DATA-AUX       TO WRK-DATA-AUX3.

           IF  WRK-DATA-AUX2            LESS WRK-DATA-AUX3
               MOVE 01                  TO GFCT7G-ERRO
               MOVE ZEROS               TO GFCT7G-COD-SQL-ERRO
               MOVE 0395                TO GFCT7G-COD-MSG-ERRO
                                           WRK-5000-COD-MSG
               PERFORM 121000-OBTER-DESC-MSG
               MOVE WRK-5000-DESC-MSG   TO GFCT7G-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

           MOVE DPROCM-ATUAL OF GFCTB0A1 TO WRK-DATA-AUX.
           MOVE CORR WRK-DATA-AUX       TO WRK-DATA-AUX3.

           IF  WRK-DATA-AUX2            LESS WRK-DATA-AUX3
               MOVE 01                  TO GFCT7G-ERRO
               MOVE ZEROS               TO GFCT7G-COD-SQL-ERRO
               MOVE 0394                TO GFCT7G-COD-MSG-ERRO
                                           WRK-5000-COD-MSG
               PERFORM 121000-OBTER-DESC-MSG
               MOVE WRK-5000-DESC-MSG   TO GFCT7G-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

           MOVE GFCT7G-DT-INI-VGCIA     TO WRK-DATA-AUX.
           MOVE CORR WRK-DATA-AUX       TO WRK-DATA-AUX2.

           IF  WRK-DATA-AUX2            LESS WRK-DATA-AUX3
               MOVE 01                  TO GFCT7G-ERRO
               MOVE ZEROS               TO GFCT7G-COD-SQL-ERRO
               MOVE 0398                TO GFCT7G-COD-MSG-ERRO
                                           WRK-5000-COD-MSG
               PERFORM 121000-OBTER-DESC-MSG
               MOVE WRK-5000-DESC-MSG   TO GFCT7G-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

      *---------------- CONSISTENCIA DE DATAS --------------------------

           MOVE GFCT7F-DT-INI-VGCIA      TO WRK-DINIC-VGCIA.
           MOVE WRK-DINIC-DIA            TO WRK-DIA-INI.
           MOVE WRK-DINIC-MES            TO WRK-MES-INI.
           MOVE WRK-DINIC-ANO            TO WRK-ANO-INI.

           MOVE GFCT7F-DT-FIM-VGCIA      TO WRK-DFIM-VGCIA.
           MOVE WRK-DFIM-DIA             TO WRK-DIA-FIM.
           MOVE WRK-DFIM-MES             TO WRK-MES-FIM.
           MOVE WRK-DFIM-ANO             TO WRK-ANO-FIM.

               IF WRK-DIA-INI  NOT EQUAL 01
                   MOVE 01                  TO GFCT7G-ERRO
                   MOVE  '1349'             TO GFCT7G-COD-MSG-ERRO
                                               WRK-5000-COD-MSG
                   PERFORM 121000-OBTER-DESC-MSG
                   MOVE WRK-5000-DESC-MSG   TO GFCT7G-DESC-MSG-ERRO
                   PERFORM 300000-FINALIZAR
               END-IF.

      ******** ACESSO A POOL0025 PARA OBTER ULTIMO DIA DO MES

               INITIALIZE WRK-DATA-SAIDA
                          WRK-DATA-ENTRADA.

               MOVE WRK-DATA-FIM TO WRK-DATA-POOL25

               MOVE 01 TO WRK-DIA-POOL25

               IF WRK-MES-POOL25 EQUAL 12
                   ADD  1  TO WRK-ANO-POOL25
                   MOVE 01 TO WRK-MES-POOL25
               ELSE
                   ADD 1 TO WRK-MES-POOL25
               END-IF.

               MOVE WRK-DIA-POOL25         TO WRK-DIA-E25.
               MOVE WRK-MES-POOL25         TO WRK-MES-E25.
               MOVE WRK-ANO-POOL25         TO WRK-ANO-E25.
               MOVE WRK-DATA-E25           TO WRK-DATA-ENTRADA.
               MOVE -01                    TO WRK-NUMERO-DIAS.

               CALL 'POOL0025'             USING WRK-DATA-ENTRADA
                                                 WRK-NUMERO-DIAS
                                                 WRK-DATA-SAIDA.

               IF  RETURN-CODE             NOT EQUAL ZEROS
                   MOVE 01                  TO GFCT7G-ERRO
                   MOVE  '1351'             TO GFCT7G-COD-MSG-ERRO
                                               WRK-5000-COD-MSG
                   PERFORM 121000-OBTER-DESC-MSG
                   MOVE WRK-5000-DESC-MSG   TO GFCT7G-DESC-MSG-ERRO
                   PERFORM 300000-FINALIZAR
               END-IF.

               MOVE WRK-DATA-SAIDA         TO WRK-DATA-S25.
               MOVE WRK-DIA-S25            TO WRK-DIA-POOL25
               MOVE WRK-MES-S25            TO WRK-MES-POOL25
               MOVE WRK-ANO-S25            TO WRK-ANO-POOL25

               IF WRK-DATA-FIM   NOT EQUAL WRK-DATA-POOL25
                   MOVE 01                 TO GFCT7G-ERRO
                   MOVE  '1350'            TO GFCT7G-COD-MSG-ERRO
                                              WRK-5000-COD-MSG
                   PERFORM 121000-OBTER-DESC-MSG
                   MOVE WRK-5000-DESC-MSG   TO GFCT7G-DESC-MSG-ERRO
                   PERFORM 300000-FINALIZAR

               END-IF.
      *---------------- CONSISTENCIA DE DATAS --------------------------

           PERFORM 221000-OBTER-PARM-GERAIS.

           PERFORM 222000-OBTER-HIST-PARM-TAR.

       220000-CONSISTENCIA.
      *-------------------
      *
      *--- VALIDAR DATAS DE VIGENCIA X PERIODO DE CALCULO DA TARIFA
      *
           IF  CPER-CALC-TARIF OF GFCTB086
                                       EQUAL  4

               GO   TO  220000-CONS-PERIODO-MENSAL
           ELSE
               IF  CPER-CALC-TARIF OF GFCTB086
                                       EQUAL  3

                   GO   TO  220000-CONS-PERIODO-QUINZENAL
               ELSE
                   IF  CPER-CALC-TARIF OF GFCTB086
                                       EQUAL  2

                       GO   TO  220000-CONS-PERIODO-SEMANAL
                   ELSE
                       IF  CPER-CALC-TARIF OF GFCTB086
                                       EQUAL  1

                           GO   TO  220000-99-FIM
                       END-IF
                   END-IF
               END-IF
           END-IF.

       220000-CONS-PERIODO-MENSAL.
      *--------------------------

      *--- VALIDAR DIA DE INICIO DE VIGENCIA DE PERIODO MENSAL

           IF  GFCT7F-DT-INI-VGCIA(01:2)
                                        NOT EQUAL '01'
               MOVE 01                  TO GFCT7G-ERRO
               MOVE ZEROS               TO GFCT7G-COD-SQL-ERRO
               MOVE 0197                TO GFCT7G-COD-MSG-ERRO
                                           WRK-5000-COD-MSG

               PERFORM 121000-OBTER-DESC-MSG
               MOVE WRK-5000-DESC-MSG   TO GFCT7G-DESC-MSG-ERRO

               GO   TO 300000-FINALIZAR
           END-IF.
      *
      *--- OBTER ULTIMO DIA DO MES DE FIM DE VIGENCIA
      *
           MOVE GFCT7F-DT-FIM-VGCIA(01:2)
                                        TO WRK-DIA-AUX-2.
           MOVE GFCT7F-DT-FIM-VGCIA(04:2)
                                        TO WRK-MES-AUX-2.
           MOVE GFCT7F-DT-FIM-VGCIA(07:4)
                                        TO WRK-ANO-AUX-2.

           MOVE WRK-DATA-AUX-3          TO WRK-DATA-FIM-VIG.

           MOVE 1                       TO WRK-DIA-AUX-2.

           IF  WRK-MES-AUX-2            EQUAL 12
               MOVE 1                   TO WRK-MES-AUX-2

               ADD  1                   TO WRK-ANO-AUX-2
           ELSE
               ADD  1                   TO WRK-MES-AUX-2
           END-IF.

           MOVE WRK-DATA-AUX-3          TO WRK-DATA-ENTRADA.

           PERFORM  223000-OBTER-ULTIMO-DIA-MES.

           IF  WRK-DATA-SAIDA           NOT EQUAL WRK-DATA-FIM-VIG
               MOVE 01                  TO GFCT7G-ERRO
               MOVE ZEROS               TO GFCT7G-COD-SQL-ERRO
               MOVE 1039                TO GFCT7G-COD-MSG-ERRO
                                           WRK-5000-COD-MSG

               PERFORM 121000-OBTER-DESC-MSG
               MOVE WRK-5000-DESC-MSG   TO GFCT7G-DESC-MSG-ERRO

               GO   TO 300000-FINALIZAR
           END-IF.

           GO   TO  220000-99-FIM.

       220000-CONS-PERIODO-QUINZENAL.
      *-----------------------------

      *--- VALIDAR DIA DE INICIO DE VIGENCIA DE PERIODO QUINZENAL

           IF  GFCT7F-DT-INI-VGCIA(01:2)
                                        NOT EQUAL '01'  AND
               GFCT7F-DT-INI-VGCIA(01:2)
                                        NOT EQUAL '16'
               MOVE 01                  TO GFCT7G-ERRO
               MOVE ZEROS               TO GFCT7G-COD-SQL-ERRO
               MOVE 0198                TO GFCT7G-COD-MSG-ERRO
                                           WRK-5000-COD-MSG

               PERFORM 121000-OBTER-DESC-MSG
               MOVE WRK-5000-DESC-MSG   TO GFCT7G-DESC-MSG-ERRO

               GO   TO 300000-FINALIZAR
           END-IF.

      *--- VALIDAR DIA DE FIM DE VIGENCIA DE PERIODO QUINZENAL

           IF  GFCT7F-DT-INI-VGCIA(01:2)
                                        EQUAL '01'  AND
               GFCT7F-DT-FIM-VGCIA(01:2)
                                        NOT EQUAL '15'
               MOVE 01                  TO GFCT7G-ERRO
               MOVE ZEROS               TO GFCT7G-COD-SQL-ERRO
               MOVE 1040                TO GFCT7G-COD-MSG-ERRO
                                           WRK-5000-COD-MSG

               PERFORM 121000-OBTER-DESC-MSG
               MOVE WRK-5000-DESC-MSG   TO GFCT7G-DESC-MSG-ERRO

               GO   TO 300000-FINALIZAR
           END-IF.

           IF  GFCT7F-DT-INI-VGCIA(01:2)
                                        EQUAL '16'
      *
      *--- OBTER ULTIMO DIA DO MES DE FIM DE VIGENCIA
      *
               MOVE GFCT7F-DT-FIM-VGCIA(01:2)
                                        TO WRK-DIA-AUX-2
               MOVE GFCT7F-DT-FIM-VGCIA(04:2)
                                        TO WRK-MES-AUX-2
               MOVE GFCT7F-DT-FIM-VGCIA(07:4)
                                        TO WRK-ANO-AUX-2

               MOVE WRK-DATA-AUX-3      TO WRK-DATA-FIM-VIG

               MOVE 1                   TO WRK-DIA-AUX-2

               IF  WRK-MES-AUX-2        EQUAL 12
                   MOVE 1               TO WRK-MES-AUX-2

                   ADD  1               TO WRK-ANO-AUX-2
               ELSE
                   ADD  1               TO WRK-MES-AUX-2
               END-IF

               MOVE WRK-DATA-AUX-3      TO WRK-DATA-ENTRADA

               PERFORM  223000-OBTER-ULTIMO-DIA-MES

               IF  WRK-DATA-SAIDA       NOT EQUAL WRK-DATA-FIM-VIG
                   MOVE 01              TO GFCT7G-ERRO
                   MOVE ZEROS           TO GFCT7G-COD-SQL-ERRO
                   MOVE 1039            TO GFCT7G-COD-MSG-ERRO
                                           WRK-5000-COD-MSG

                   PERFORM 121000-OBTER-DESC-MSG
                   MOVE WRK-5000-DESC-MSG
                                        TO GFCT7G-DESC-MSG-ERRO

                   GO   TO 300000-FINALIZAR
               END-IF
           END-IF.

           GO   TO  220000-99-FIM.

       220000-CONS-PERIODO-SEMANAL.
      *---------------------------

      *--- OBTER DIA DA SEMANA DE INICIO DO PERIODO SEMANAL

           MOVE GFCT7F-DT-INI-VGCIA(01:2)
                                        TO WRK-DIA-AUX-2.
           MOVE GFCT7F-DT-INI-VGCIA(04:2)
                                        TO WRK-MES-AUX-2.
           MOVE GFCT7F-DT-INI-VGCIA(07:4)
                                        TO WRK-ANO-AUX-2.

           PERFORM 224000-OBTER-DIA-SEMANA.

      *--- VALIDAR DIA DE INICIO DE VIGENCIA DE PERIODO SEMANAL

           IF (CINIC-PER-CALC OF GFCTB086
                                        EQUAL 1 AND
               WRK-DIA-SEMANA           NOT EQUAL 'DOMINGO')       OR
              (CINIC-PER-CALC OF GFCTB086
                                        EQUAL 2 AND
               WRK-DIA-SEMANA           NOT EQUAL 'SEGUNDA-FEIRA') OR
              (CINIC-PER-CALC OF GFCTB086
                                        EQUAL 3 AND
               WRK-DIA-SEMANA           NOT EQUAL 'TERCA-FEIRA')   OR
              (CINIC-PER-CALC OF GFCTB086
                                        EQUAL 4 AND
               WRK-DIA-SEMANA           NOT EQUAL 'QUARTA-FEIRA')  OR
              (CINIC-PER-CALC OF GFCTB086
                                        EQUAL 5 AND
               WRK-DIA-SEMANA           NOT EQUAL 'QUINTA-FEIRA')  OR
              (CINIC-PER-CALC OF GFCTB086
                                        EQUAL 6 AND
               WRK-DIA-SEMANA           NOT EQUAL 'SEXTA-FEIRA')   OR
              (CINIC-PER-CALC OF GFCTB086
                                        EQUAL 7 AND
               WRK-DIA-SEMANA           NOT EQUAL 'SABADO')
               MOVE 01                  TO GFCT7G-ERRO
               MOVE ZEROS               TO GFCT7G-COD-SQL-ERRO
               MOVE 0199                TO GFCT7G-COD-MSG-ERRO
                                           WRK-5000-COD-MSG

               PERFORM 121000-OBTER-DESC-MSG
               MOVE WRK-5000-DESC-MSG   TO GFCT7G-DESC-MSG-ERRO

               GO   TO 300000-FINALIZAR
           END-IF.

      *--- OBTER DIA DA SEMANA DE FIM DO PERIODO SEMANAL

           MOVE GFCT7F-DT-FIM-VGCIA(01:2)
                                        TO WRK-DIA-AUX-2.
           MOVE GFCT7F-DT-FIM-VGCIA(04:2)
                                        TO WRK-MES-AUX-2.
           MOVE GFCT7F-DT-FIM-VGCIA(07:4)
                                        TO WRK-ANO-AUX-2.

           PERFORM 224000-OBTER-DIA-SEMANA.

           IF  GFCT7G-ERRO       EQUAL 'S'

               GO   TO 300000-FINALIZAR
           END-IF.

      *--- VALIDAR DIA DA SEMANA DE FIM DE VIGENCIA DE PERIODO SEMANAL

           IF (CINIC-PER-CALC OF GFCTB086
                                        EQUAL 1 AND
               WRK-DIA-SEMANA           NOT EQUAL 'SABADO')        OR
              (CINIC-PER-CALC OF GFCTB086
                                        EQUAL 2 AND
               WRK-DIA-SEMANA           NOT EQUAL 'DOMINGO')       OR
              (CINIC-PER-CALC OF GFCTB086
                                        EQUAL 3 AND
               WRK-DIA-SEMANA           NOT EQUAL 'SEGUNDA-FEIRA') OR
              (CINIC-PER-CALC OF GFCTB086
                                        EQUAL 4 AND
               WRK-DIA-SEMANA           NOT EQUAL 'TERCA-FEIRA')   OR
              (CINIC-PER-CALC OF GFCTB086
                                        EQUAL 5 AND
               WRK-DIA-SEMANA           NOT EQUAL 'QUARTA-FEIRA')  OR
              (CINIC-PER-CALC OF GFCTB086
                                        EQUAL 6 AND
               WRK-DIA-SEMANA           NOT EQUAL 'QUINTA-FEIRA')  OR
              (CINIC-PER-CALC OF GFCTB086
                                        EQUAL 7 AND
               WRK-DIA-SEMANA           NOT EQUAL 'SEXTA-FEIRA')
               MOVE 01                  TO GFCT7G-ERRO
               MOVE ZEROS               TO GFCT7G-COD-SQL-ERRO
               MOVE 1041                TO GFCT7G-COD-MSG-ERRO
                                           WRK-5000-COD-MSG

               PERFORM 121000-OBTER-DESC-MSG
               MOVE WRK-5000-DESC-MSG   TO GFCT7G-DESC-MSG-ERRO

               GO   TO 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       220000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       221000-OBTER-PARM-GERAIS        SECTION.
      *----------------------------------------------------------------*

           MOVE GFCT7F-TARIFA          TO CSERVC-TARIF     OF GFCTB086.
           MOVE GFCT7F-DT-INI-VGCIA-ANT
                                       TO DINIC-VGCIA-TARIF
                                                           OF GFCTB086.

           EXEC SQL
             SELECT
                   HIDTFD_MANUT_SERVC,
                   CSEQ_MANUT_SERVC
             INTO
                   :GFCTB086.HIDTFD-MANUT-SERVC,
                   :GFCTB086.CSEQ-MANUT-SERVC
             FROM   DB2PRD.HIST_PARM_TARIF
             WHERE
                   CSERVC_TARIF        = :GFCTB086.CSERVC-TARIF      AND
                   DINIC_VGCIA_TARIF   = :GFCTB086.DINIC-VGCIA-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'GFCT0237'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'HIST_PARM_TARIF ' TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE  '0020'            TO GFCT0M-LOCAL
               MOVE  SQLCA(1:136) TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCT7G-ERRO
               MOVE  SQLCODE           TO GFCT7G-COD-SQL-ERRO
               MOVE  0012              TO GFCT7G-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 121000-OBTER-DESC-MSG
               STRING WRK-5000-DESC-MSG
                      ' - GFCTB086'
               DELIMITED BY '  '       INTO GFCT7G-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

           MOVE GFCT7F-TARIFA          TO WRK-CSERVC-TARIF.
           MOVE GFCT7F-DT-INI-VGCIA-ANT
                                       TO WRK-DINIC-VGCIA-TARIF.
           MOVE HIDTFD-MANUT-SERVC  OF GFCTB086
                                       TO WRK-HIDTFD-MANUT-SERVC.
           MOVE CSEQ-MANUT-SERVC    OF GFCTB086
                                       TO WRK-CSEQ-MANUT-SERVC.

      *----------------------------------------------------------------*
       221000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       222000-OBTER-HIST-PARM-TAR      SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-CSERVC-TARIF       TO CSERVC-TARIF     OF GFCTB086.
           MOVE WRK-CSEQ-MANUT-SERVC   TO CSEQ-MANUT-SERVC OF GFCTB086.
           MOVE WRK-HIDTFD-MANUT-SERVC TO HIDTFD-MANUT-SERVC
                                                           OF GFCTB086.
           MOVE WRK-DINIC-VGCIA-TARIF  TO DINIC-VGCIA-TARIF
                                                           OF GFCTB086.

           EXEC SQL
             SELECT
                   CSERVC_TARIF,
                   HIDTFD_MANUT_SERVC,
                   CSEQ_MANUT_SERVC,
                   DINIC_VGCIA_TARIF,
                   DFIM_VGCIA_TARIF,
                   CNATUZ_SERVC,
                   CCJTO_SERVC,
                   QMAX_TENTV_DEB,
                   QFRANQ_TARIF,
                   CPER_FRANQ_TARIF,
                   CPER_CALC_TARIF,
                   CINIC_PER_CALC,
                   DDIA_COBR_TARIF,
                   QDIA_CAREN_TARIF,
                   PDESC_CAREN_TARIF,
                   CINDCD_INIC_CAREN,
                   DINIC_CAREN_TARIF,
                   CINDCD_DEB_CONSL,
                   CINDCD_EXCED_CONSL,
                   RSEGDA_LIN_EXTRT,
                   QDIA_RTCAO_COBR,
                   CINIC_PER_FRANQ,
                   VMIN_ENVIO_COBR,
                   CINDCD_TARIF_PROML,
                   CINDCD_TENTV_COBR,
                   QTENTV_SEM_COBR,
                   CINDCD_RETOR_COBR,
                   CINDCD_CALC_PRORT,
                   CINDCD_PER_CAREN,
                   QMES_CAREN_TARIF,
RIC                QDIA_COBR_TARIF,
RIC                CIDTFD_COBR_FERI,
161110             CINDCD_PCMTO_COBR,
161110             QPCELA_COBR_TARIF,
161110             CIDTFD_COBR_PCELA,
161110             QDIA_COBR_PCELA,
161110             CINDCD_CREDT_CLULR,
BI0411             CINDCD_REPAS_CLULR,
BI0411             VREPAS_CREDT_CLULR,
022012             QDIA_RTCAO_CREDT,
022012             CTPO_FRANQ_TARIF,
                   CINDCD_PER_COBR
             INTO
                   :GFCTB086.CSERVC-TARIF,
                   :GFCTB086.HIDTFD-MANUT-SERVC,
                   :GFCTB086.CSEQ-MANUT-SERVC,
                   :GFCTB086.DINIC-VGCIA-TARIF,
                   :GFCTB086.DFIM-VGCIA-TARIF,
                   :GFCTB086.CNATUZ-SERVC,
                   :GFCTB086.CCJTO-SERVC,
                   :GFCTB086.QMAX-TENTV-DEB,
                   :GFCTB086.QFRANQ-TARIF,
                   :GFCTB086.CPER-FRANQ-TARIF,
                   :GFCTB086.CPER-CALC-TARIF,
                   :GFCTB086.CINIC-PER-CALC,
                   :GFCTB086.DDIA-COBR-TARIF,
                   :GFCTB086.QDIA-CAREN-TARIF,
                   :GFCTB086.PDESC-CAREN-TARIF,
                   :GFCTB086.CINDCD-INIC-CAREN,
                   :GFCTB086.DINIC-CAREN-TARIF,
                   :GFCTB086.CINDCD-DEB-CONSL,
                   :GFCTB086.CINDCD-EXCED-CONSL,
                   :GFCTB086.RSEGDA-LIN-EXTRT,
                   :GFCTB086.QDIA-RTCAO-COBR,
                   :GFCTB086.CINIC-PER-FRANQ,
                   :GFCTB086.VMIN-ENVIO-COBR,
                   :GFCTB086.CINDCD-TARIF-PROML,
                   :GFCTB086.CINDCD-TENTV-COBR,
                   :GFCTB086.QTENTV-SEM-COBR,
                   :GFCTB086.CINDCD-RETOR-COBR,
                   :GFCTB086.CINDCD-CALC-PRORT,
                   :GFCTB086.CINDCD-PER-CAREN,
                   :GFCTB086.QMES-CAREN-TARIF,
RIC                :GFCTB086.QDIA-COBR-TARIF,
RIC                :GFCTB086.CIDTFD-COBR-FERI,
161110             :GFCTB086.CINDCD-PCMTO-COBR,
161110             :GFCTB086.QPCELA-COBR-TARIF,
161110             :GFCTB086.CIDTFD-COBR-PCELA,
161110             :GFCTB086.QDIA-COBR-PCELA,
161110             :GFCTB086.CINDCD-CREDT-CLULR,
BI0411             :GFCTB086.CINDCD-REPAS-CLULR,
BI0411             :GFCTB086.VREPAS-CREDT-CLULR,
022012             :GFCTB086.QDIA-RTCAO-CREDT,
022012             :GFCTB086.CTPO-FRANQ-TARIF,
022012             :GFCTB086.CINDCD-PER-COBR
             FROM   DB2PRD.HIST_PARM_TARIF
             WHERE
                   CSERVC_TARIF       = :GFCTB086.CSERVC-TARIF       AND
                   HIDTFD_MANUT_SERVC = :GFCTB086.HIDTFD-MANUT-SERVC AND
                   CSEQ_MANUT_SERVC   = :GFCTB086.CSEQ-MANUT-SERVC   AND
                   DINIC_VGCIA_TARIF  = :GFCTB086.DINIC-VGCIA-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'GFCT0237'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'HIST_PARM_TARIF ' TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE  '0030'            TO GFCT0M-LOCAL
               MOVE  SQLCA(1:136) TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCT7G-ERRO
               MOVE  SQLCODE           TO GFCT7G-COD-SQL-ERRO
               MOVE  0012              TO GFCT7G-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 121000-OBTER-DESC-MSG
               STRING WRK-5000-DESC-MSG
                      ' - GFCTB086'
               DELIMITED BY '  '       INTO GFCT7G-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       222000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       223000-OBTER-ULTIMO-DIA-MES     SECTION.
      *----------------------------------------------------------------*

           MOVE -1                      TO WRK-NUMERO-DIAS.

           CALL 'POOL0025'              USING WRK-DATA-ENTRADA
                                              WRK-NUMERO-DIAS
                                              WRK-DATA-SAIDA.

           IF  RETURN-CODE              NOT EQUAL ZEROS
               MOVE ZEROS               TO RETURN-CODE

               MOVE 01                  TO GFCT7G-ERRO
               MOVE ZEROS               TO GFCT7G-COD-SQL-ERRO
               MOVE 1044                TO GFCT7G-COD-MSG-ERRO
                                           WRK-5000-COD-MSG

               PERFORM 121000-OBTER-DESC-MSG
               MOVE WRK-5000-DESC-MSG   TO GFCT7G-DESC-MSG-ERRO

               GO   TO 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       223000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       224000-OBTER-DIA-SEMANA         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-DATA-AUX-3          TO WRK-DT-ENVIADA.
           MOVE SPACES                  TO WRK-OPCAO.

           CALL 'POOL1205'              USING WRK-AREA-POOL1205
                                              WRK-MENSAGEM-1205.

           IF  RETURN-CODE              NOT EQUAL ZEROS
               MOVE ZEROS               TO RETURN-CODE

               MOVE 01                  TO GFCT7G-ERRO
               MOVE ZEROS               TO GFCT7G-COD-SQL-ERRO
               MOVE 0223                TO GFCT7G-COD-MSG-ERRO
                                           WRK-5000-COD-MSG

               PERFORM 121000-OBTER-DESC-MSG
               MOVE WRK-5000-DESC-MSG   TO GFCT7G-DESC-MSG-ERRO

               GO   TO 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       224000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       225000-VESEJATEM                SECTION.
      *----------------------------------------------------------------*

           MOVE GFCT7F-TARIFA TO CSERVC-TARIF OF GFCTB087.

           EXEC SQL
             SELECT
                   MAX(B.DINIC_VGCIA_TARIF)
             INTO
                  :GFCTB086.DINIC-VGCIA-TARIF
             FROM DB2PRD.HIST_TARIF_PRINC AS A,
                  DB2PRD.HIST_PARM_TARIF  AS B
             WHERE A.CSERVC_TARIF      = B.CSERVC_TARIF
             AND   A.CSERVC_TARIF      = :GFCTB087.CSERVC-TARIF
             AND   A.CSIT_LIBRC_TARIF  > :W-TREIS
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND -305) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'GFCT0237'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'UNION - 87 - 86 ' TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE  '0040'            TO GFCT0M-LOCAL
               MOVE  SQLCA(1:136) TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCT7G-ERRO
               MOVE  SQLCODE           TO GFCT7G-COD-SQL-ERRO
               MOVE  0012              TO GFCT7G-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 121000-OBTER-DESC-MSG
               STRING WRK-5000-DESC-MSG
                      ' - GFCTB086'
               DELIMITED BY '  '       INTO GFCT7G-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

           IF SQLCODE EQUAL ZEROS
              MOVE DINIC-VGCIA-TARIF OF GFCTB086 TO WRK-DATA-AUX
              MOVE CORR WRK-DATA-AUX       TO WRK-DATA-AUX3
              MOVE DPROCM-ATUAL     OF GFCTB0A1 TO WRK-DATA-AUX
              MOVE CORR WRK-DATA-AUX       TO WRK-DATA-AUX2
              IF  WRK-DATA-AUX3            GREATER WRK-DATA-AUX2
                  MOVE 01                  TO GFCT7G-ERRO
                  MOVE ZEROS               TO GFCT7G-COD-SQL-ERRO
                  MOVE     399             TO GFCT7G-COD-MSG-ERRO
                                              WRK-5000-COD-MSG
                  PERFORM 121000-OBTER-DESC-MSG
                  MOVE WRK-5000-DESC-MSG   TO GFCT7G-DESC-MSG-ERRO
                  PERFORM 300000-FINALIZAR
              END-IF
           END-IF.

           MOVE GFCT7F-TARIFA TO CSERVC-TARIF OF GFCTB087.

           EXEC SQL
             SELECT
                   MAX(B.DFIM_VGCIA_TARIF)
             INTO
                  :GFCTB086.DFIM-VGCIA-TARIF
             FROM DB2PRD.HIST_TARIF_PRINC AS A,
                  DB2PRD.HIST_PARM_TARIF  AS B
             WHERE A.CSERVC_TARIF      = B.CSERVC_TARIF
             AND   A.CSERVC_TARIF      = :GFCTB087.CSERVC-TARIF
             AND   A.CSIT_LIBRC_TARIF  > :W-TREIS
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND -305) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'GFCT0237'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'UNION - 87 - 86 ' TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE  '0050'            TO GFCT0M-LOCAL
               MOVE  SQLCA(1:136) TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCT7G-ERRO
               MOVE  SQLCODE           TO GFCT7G-COD-SQL-ERRO
               MOVE  0012              TO GFCT7G-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 121000-OBTER-DESC-MSG
               STRING WRK-5000-DESC-MSG
                      ' - GFCTB086'
               DELIMITED BY '  '       INTO GFCT7G-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

           IF SQLCODE EQUAL ZEROS
              MOVE DFIM-VGCIA-TARIF OF GFCTB086 TO WRK-DATA-AUX
              MOVE CORR WRK-DATA-AUX       TO WRK-DATA-AUX3
              MOVE DPROCM-ATUAL     OF GFCTB0A1 TO WRK-DATA-AUX
              MOVE CORR WRK-DATA-AUX       TO WRK-DATA-AUX2
              IF  WRK-DATA-AUX3            GREATER WRK-DATA-AUX2
                  MOVE 01                  TO GFCT7G-ERRO
                  MOVE ZEROS               TO GFCT7G-COD-SQL-ERRO
                  MOVE     400             TO GFCT7G-COD-MSG-ERRO
                                              WRK-5000-COD-MSG
                  PERFORM 121000-OBTER-DESC-MSG
                  MOVE WRK-5000-DESC-MSG   TO GFCT7G-DESC-MSG-ERRO
                  PERFORM 300000-FINALIZAR
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       225000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       230000-GERAR-REATIVACAO         SECTION.
      *----------------------------------------------------------------*

           PERFORM 231000-PREPARAR-PARM.

           PERFORM 232000-CRIAR-NVA-HIST-PRINC.

           PERFORM 233000-CRIAR-NVA-HIST-PARM-TAR.

           PERFORM 234000-CRIAR-HIST-EVEN-CTBIL.

           PERFORM 235000-CRIAR-HIST-VLR-COMP.

      *----------------------------------------------------------------*
       230000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       231000-PREPARAR-PARM            SECTION.
      *----------------------------------------------------------------*

           PERFORM 231100-OBTER-TIMESTAMP.

           PERFORM 231300-OBTER-NVA-SEQ-SERVC.

      *----------------------------------------------------------------*
       231000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       231100-OBTER-TIMESTAMP          SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL7600'             USING WRK-DATA-HORA.

           MOVE CORR WRK-TIMESTAMP     TO WRK-TIMESTAMP-AUX.

      *----------------------------------------------------------------*
       231100-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       231300-OBTER-NVA-SEQ-SERVC      SECTION.
      *----------------------------------------------------------------*

           MOVE GFCT7F-TARIFA          TO CSERVC-TARIF     OF GFCTB087.

           EXEC SQL
             SELECT MAX
                   (CSEQ_MANUT_SERVC)
             INTO
                   :GFCTB087.CSEQ-MANUT-SERVC
             FROM   DB2PRD.HIST_TARIF_PRINC
             WHERE
                   CSERVC_TARIF       = :GFCTB087.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'GFCT0237'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'HIST_TARIF-PRINC' TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE  '0060'            TO GFCT0M-LOCAL
               MOVE  SQLCA(1:136) TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCT7G-ERRO
               MOVE  SQLCODE           TO GFCT7G-COD-SQL-ERRO
               MOVE  0012              TO GFCT7G-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 121000-OBTER-DESC-MSG
               STRING WRK-5000-DESC-MSG
                      ' - GFCTB087'
               DELIMITED BY '  '       INTO GFCT7G-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

           MOVE CSEQ-MANUT-SERVC  OF GFCTB087
                                       TO WRK-CSEQ-MANUT-SERVC-NVA.

           ADD 1                       TO WRK-CSEQ-MANUT-SERVC-NVA.

      *----------------------------------------------------------------*
       231300-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       232000-CRIAR-NVA-HIST-PRINC     SECTION.
      *----------------------------------------------------------------*

           PERFORM 232100-OBTER-HIST-PRINC.

           PERFORM 232200-GERAR-HIST-PRINC.

BI0115     IF W-CTPO-SERVC-TARIF NOT EQUAL 2  AND  4
              PERFORM 232300-GERAR-D8
           END-IF.

      *----------------------------------------------------------------*
       232000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       232100-OBTER-HIST-PRINC         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-CSERVC-TARIF       TO CSERVC-TARIF     OF GFCTB087.
           MOVE WRK-CSEQ-MANUT-SERVC   TO CSEQ-MANUT-SERVC OF GFCTB087.
           MOVE WRK-HIDTFD-MANUT-SERVC TO HIDTFD-MANUT-SERVC
                                                           OF GFCTB087.

           EXEC SQL
             SELECT
                   CSERVC_TARIF,
                   HIDTFD_MANUT_SERVC,
                   CSEQ_MANUT_SERVC,
                   CFUNC_MANUT_SERVC,
                   CFUNC_PAREC_SERVC,
                   CBCO_CCREN,
                   CIDTFD_LCTO,
                   CIDTFD_SNAL_LCTO,
                   CTPO_SERVC_TARIF,
                   RSERVC_TARIF,
                   RSERVC_TARIF_REDZD,
                   CPSSOA_SERVC_TARIF,
                   CTPO_CTA_DEB_TARIF,
                   CTPO_DEB_COBR,
                   RUND_COBR_SERVC,
                   CEMPR_GSTAO_SERVC,
                   CDEPDC_GSTAO_SERVC,
                   CLCTO_SERVC,
                   CDEPDC_MANUT_SERVC,
                   CTPO_MANUT_SERVC,
                   CTERM_MANUT_SERVC,
                   HMANUT_SERVC,
                   CDEPDC_PAREC_SERVC,
                   CTERM_PAREC_SERVC,
                   HPAREC_SERVC,
                   RJUSTF_MANUT_SERVC,
                   CSIT_LIBRC_TARIF,
                   CPAREC_GSTAO_SERVC,
                   RJUSTF_PAREC_SERVC,
                   CTPO_LCTO_COBR,
                   CTPO_LCTO_ESTRN,
                   CCART_DEB_TARIF,
                   CINDCD_DEB_PCIAL,
                   CINDCD_PCIAL_ULT,
                   CINDCD_EVNTO_PCELD,
                   CINDCD_QTD_REG,
                   CLCTO_COBR_PJ,
                   CLCTO_ESTRN_PJ,
RIC                CINDCD_NRO_DOCTO,
161110             CINDCD_CONS_IMPED,
BI0411             CLCTO_ESTRN_ANTER
             INTO
                   :GFCTB087.CSERVC-TARIF,
                   :GFCTB087.HIDTFD-MANUT-SERVC,
                   :GFCTB087.CSEQ-MANUT-SERVC,
                   :GFCTB087.CFUNC-MANUT-SERVC,
                   :GFCTB087.CFUNC-PAREC-SERVC,
                   :GFCTB087.CBCO-CCREN,
                   :GFCTB087.CIDTFD-LCTO,
                   :GFCTB087.CIDTFD-SNAL-LCTO,
                   :GFCTB087.CTPO-SERVC-TARIF,
                   :GFCTB087.RSERVC-TARIF,
                   :GFCTB087.RSERVC-TARIF-REDZD,
                   :GFCTB087.CPSSOA-SERVC-TARIF,
                   :GFCTB087.CTPO-CTA-DEB-TARIF,
                   :GFCTB087.CTPO-DEB-COBR,
                   :GFCTB087.RUND-COBR-SERVC,
                   :GFCTB087.CEMPR-GSTAO-SERVC,
                   :GFCTB087.CDEPDC-GSTAO-SERVC,
                   :GFCTB087.CLCTO-SERVC,
                   :GFCTB087.CDEPDC-MANUT-SERVC,
                   :GFCTB087.CTPO-MANUT-SERVC,
                   :GFCTB087.CTERM-MANUT-SERVC,
                   :GFCTB087.HMANUT-SERVC,
                   :GFCTB087.CDEPDC-PAREC-SERVC,
                   :GFCTB087.CTERM-PAREC-SERVC,
                   :GFCTB087.HPAREC-SERVC,
                   :GFCTB087.RJUSTF-MANUT-SERVC,
                   :GFCTB087.CSIT-LIBRC-TARIF,
                   :GFCTB087.CPAREC-GSTAO-SERVC,
                   :GFCTB087.RJUSTF-PAREC-SERVC,
                   :GFCTB087.CTPO-LCTO-COBR,
                   :GFCTB087.CTPO-LCTO-ESTRN,
                   :GFCTB087.CCART-DEB-TARIF,
                   :GFCTB087.CINDCD-DEB-PCIAL,
                   :GFCTB087.CINDCD-PCIAL-ULT,
                   :GFCTB087.CINDCD-EVNTO-PCELD,
                   :GFCTB087.CINDCD-QTD-REG,
                   :GFCTB087.CLCTO-COBR-PJ,
                   :GFCTB087.CLCTO-ESTRN-PJ,
RIC                :GFCTB087.CINDCD-NRO-DOCTO,
161110             :GFCTB087.CINDCD-CONS-IMPED,
BI0411             :GFCTB087.CLCTO-ESTRN-ANTER
             FROM   DB2PRD.HIST_TARIF_PRINC
             WHERE
                   CSERVC_TARIF       = :GFCTB087.CSERVC-TARIF       AND
                   HIDTFD_MANUT_SERVC = :GFCTB087.HIDTFD-MANUT-SERVC AND
                   CSEQ_MANUT_SERVC   = :GFCTB087.CSEQ-MANUT-SERVC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'GFCT0237'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'HIST_TARIF_PRINC' TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE  '0070'            TO GFCT0M-LOCAL
               MOVE  SQLCA(1:136) TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCT7G-ERRO
               MOVE  SQLCODE           TO GFCT7G-COD-SQL-ERRO
               MOVE  0012              TO GFCT7G-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 121000-OBTER-DESC-MSG
               STRING WRK-5000-DESC-MSG
                      ' - GFCTB087'
               DELIMITED BY '  '       INTO GFCT7G-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

           IF  CTPO-SERVC-TARIF  OF GFCTB087
                                       EQUAL 3
               MOVE 3                  TO GFCT7G-SIT-REATIVACAO
               MOVE 159                TO WRK-5000-COD-MSG
               PERFORM 121000-OBTER-DESC-MSG
               MOVE WRK-5000-DESC-MSG  TO GFCT7G-DESC-MSG-ERRO
           END-IF.

           MOVE CTPO-SERVC-TARIF OF GFCTB087 TO W-CTPO-SERVC-TARIF.

      *----------------------------------------------------------------*
       232100-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       232200-GERAR-HIST-PRINC         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-CSERVC-TARIF       TO CSERVC-TARIF     OF GFCTB087.
           MOVE WRK-TIMESTAMP-AUX      TO HIDTFD-MANUT-SERVC
                                                           OF GFCTB087.
           MOVE WRK-CSEQ-MANUT-SERVC-NVA
                                       TO CSEQ-MANUT-SERVC OF GFCTB087.

           PERFORM 232210-VER-MASTER

BI0115     IF  CTPO-SERVC-TARIF        OF GFCTB087 EQUAL 2 OR 4
               MOVE     4              TO CSIT-LIBRC-TARIF OF GFCTB087
           ELSE
               IF  W-JUNCAO-USUARIO EQUAL W-JUNCAO-MASTER
                   MOVE 7              TO CSIT-LIBRC-TARIF OF GFCTB087
               ELSE
                   MOVE 5              TO CSIT-LIBRC-TARIF OF GFCTB087
               END-IF
           END-IF.

           EXEC SQL
             INSERT INTO DB2PRD.HIST_TARIF_PRINC
                  (CSERVC_TARIF,
                   HIDTFD_MANUT_SERVC,
                   CSEQ_MANUT_SERVC,
                   CFUNC_MANUT_SERVC,
                   CFUNC_PAREC_SERVC,
                   CBCO_CCREN,
                   CIDTFD_LCTO,
                   CIDTFD_SNAL_LCTO,
                   CTPO_SERVC_TARIF,
                   RSERVC_TARIF,
                   RSERVC_TARIF_REDZD,
                   CPSSOA_SERVC_TARIF,
                   CTPO_CTA_DEB_TARIF,
                   CTPO_DEB_COBR,
                   RUND_COBR_SERVC,
                   CEMPR_GSTAO_SERVC,
                   CDEPDC_GSTAO_SERVC,
                   CLCTO_SERVC,
                   CDEPDC_MANUT_SERVC,
                   CTPO_MANUT_SERVC,
                   CTERM_MANUT_SERVC,
                   HMANUT_SERVC,
                   CDEPDC_PAREC_SERVC,
                   CTERM_PAREC_SERVC,
                   HPAREC_SERVC,
                   RJUSTF_MANUT_SERVC,
                   CSIT_LIBRC_TARIF,
                   CPAREC_GSTAO_SERVC,
                   RJUSTF_PAREC_SERVC,
                   CTPO_LCTO_COBR,
                   CTPO_LCTO_ESTRN,
                   CCART_DEB_TARIF,
                   CINDCD_DEB_PCIAL,
                   CINDCD_PCIAL_ULT ,
                   CINDCD_EVNTO_PCELD,
                   CINDCD_QTD_REG,
                   CLCTO_COBR_PJ,
                   CLCTO_ESTRN_PJ,
RIC                CINDCD_NRO_DOCTO,
161110             CINDCD_CONS_IMPED,
BI0411             CLCTO_ESTRN_ANTER)
               VALUES
                  (:GFCTB087.CSERVC-TARIF,
                   :GFCTB087.HIDTFD-MANUT-SERVC,
                   :GFCTB087.CSEQ-MANUT-SERVC,
                   :GFCTB087.CFUNC-MANUT-SERVC,
                   :GFCTB087.CFUNC-PAREC-SERVC,
                   :GFCTB087.CBCO-CCREN,
                   :GFCTB087.CIDTFD-LCTO,
                   :GFCTB087.CIDTFD-SNAL-LCTO,
                   :GFCTB087.CTPO-SERVC-TARIF,
                   :GFCTB087.RSERVC-TARIF,
                   :GFCTB087.RSERVC-TARIF-REDZD,
                   :GFCTB087.CPSSOA-SERVC-TARIF,
                   :GFCTB087.CTPO-CTA-DEB-TARIF,
                   :GFCTB087.CTPO-DEB-COBR,
                   :GFCTB087.RUND-COBR-SERVC,
                   :GFCTB087.CEMPR-GSTAO-SERVC,
                   :GFCTB087.CDEPDC-GSTAO-SERVC,
                   :GFCTB087.CLCTO-SERVC,
                   :GFCTB087.CDEPDC-MANUT-SERVC,
                   :GFCTB087.CTPO-MANUT-SERVC,
                   :GFCTB087.CTERM-MANUT-SERVC,
                   :GFCTB087.HMANUT-SERVC,
                   :GFCTB087.CDEPDC-PAREC-SERVC,
                   :GFCTB087.CTERM-PAREC-SERVC,
                   :GFCTB087.HPAREC-SERVC,
                   :GFCTB087.RJUSTF-MANUT-SERVC,
                   :GFCTB087.CSIT-LIBRC-TARIF,
                   :GFCTB087.CPAREC-GSTAO-SERVC,
                   :GFCTB087.RJUSTF-PAREC-SERVC,
                   :GFCTB087.CTPO-LCTO-COBR,
                   :GFCTB087.CTPO-LCTO-ESTRN,
                   :GFCTB087.CCART-DEB-TARIF,
                   :GFCTB087.CINDCD-DEB-PCIAL,
                   :GFCTB087.CINDCD-PCIAL-ULT,
                   :GFCTB087.CINDCD-EVNTO-PCELD,
                   :GFCTB087.CINDCD-QTD-REG,
                   :GFCTB087.CLCTO-COBR-PJ,
                   :GFCTB087.CLCTO-ESTRN-PJ,
RIC                :GFCTB087.CINDCD-NRO-DOCTO,
161110             :GFCTB087.CINDCD-CONS-IMPED,
BI0411             :GFCTB087.CLCTO-ESTRN-ANTER)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'GFCT0237'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'HIST_TARIF_PRINC' TO GFCT0M-NOME-TAB
               MOVE 'INSERT  '         TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE  '0080'            TO GFCT0M-LOCAL
               MOVE  SQLCA(1:136) TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCT7G-ERRO
               MOVE  SQLCODE           TO GFCT7G-COD-SQL-ERRO
               MOVE  0018              TO GFCT7G-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 121000-OBTER-DESC-MSG
               STRING WRK-5000-DESC-MSG
                      ' - GFCTB087'
               DELIMITED BY '  '       INTO GFCT7G-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       232200-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       232210-VER-MASTER               SECTION.
      *----------------------------------------------------------------*

           MOVE GFCT7F-FUNC-BDSCO      TO WRK-COD-FUNCIONAL-0315.
           MOVE 'VRS001'               TO WRK-VERSAO-0315.

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
              MOVE 01                  TO GFCT7G-ERRO
              MOVE ZEROS               TO GFCT7G-COD-SQL-ERRO
              MOVE 0397                TO GFCT7G-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
              PERFORM                  121000-OBTER-DESC-MSG
              MOVE WRK-5000-DESC-MSG   TO GFCT7G-DESC-MSG-ERRO
              PERFORM 300000-FINALIZAR
           END-IF.

           MOVE WRK-COD-JUNCAO-0315    TO   W-JUNCAO-USUARIO.

           EXEC SQL
              SELECT MAX(DINIC_VGCIA_GSTOR)
              INTO   :GFCTB0A0.DINIC-VGCIA-GSTOR
              FROM DB2PRD.PARM_CTRL_GSTAO
              WHERE DINIC_VGCIA_GSTOR  <= :GFCTB0A1.DPROCM-ATUAL
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'GFCT0237'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'PARM_CTRL_GSTAO' TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE  '0090'            TO GFCT0M-LOCAL
               MOVE  SQLCA(1:136) TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCT7G-ERRO
               MOVE  SQLCODE           TO GFCT7G-COD-SQL-ERRO
               MOVE  0012              TO GFCT7G-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 121000-OBTER-DESC-MSG
               STRING WRK-5000-DESC-MSG
                      ' - GFCTB087'
               DELIMITED BY '  '       INTO GFCT7G-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           ELSE
               EXEC SQL
                  SELECT MAX(CDEPDC_GSTOR)
                  INTO :GFCTB0A0.CDEPDC-GSTOR
                  FROM DB2PRD.PARM_CTRL_GSTAO
                  WHERE DINIC_VGCIA_GSTOR = :GFCTB0A0.DINIC-VGCIA-GSTOR
               END-EXEC
               IF (SQLCODE                 NOT EQUAL ZEROS) OR
                  (SQLWARN0                EQUAL 'W'      )
                   MOVE 'GFCT0237'         TO GFCT0M-PROGRAMA
                   MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
                   MOVE 'PARM_CTRL_GSTAO' TO GFCT0M-NOME-TAB
                   MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
                   MOVE  SQLCODE           TO GFCT0M-SQLCODE
                   MOVE  '0091'            TO GFCT0M-LOCAL
                   MOVE  SQLCA(1:136) TO GFCT0M-SQLCA-AREA
                   MOVE  9                 TO GFCT7G-ERRO
               MOVE  SQLCODE           TO WRK-CAMPO-TIRA-SINAL
               MOVE  WRK-CAMPO-TIRA-SINAL-03 TO GFCT7G-COD-SQL-ERRO
                   MOVE  0012              TO GFCT7G-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   PERFORM 121000-OBTER-DESC-MSG
                   STRING WRK-5000-DESC-MSG
                         ' - GFCTB087'
                   DELIMITED BY '  '       INTO GFCT7G-DESC-MSG-ERRO
                   PERFORM 300000-FINALIZAR
                END-IF
           END-IF.

______*____EXEC_SQL
______*______SELECT_CDEPDC_GSTOR
______*______INTO__:GFCTB0A0.CDEPDC-GSTOR
______*______FROM___DB2PRD.PARM_CTRL_GSTAO
______*______WHERE__DFIM_VGCIA_GSTOR___IN
______*______*______*(SELECT_MAX_(DFIM_VGCIA_GSTOR)
______*______*______*_FROM___DB2PRD.PARM_CTRL_GSTAO
______*______*______*_WHERE_DINIC_VGCIA_GSTOR_<=_:GFCTB0A1.DPROCM-ATUAL)
______*______AND____DINIC_VGCIA_GSTOR__IN
______*______*______*(SELECT_MAX_(DINIC_VGCIA_GSTOR)
______*______*______*_FROM___DB2PRD.PARM_CTRL_GSTAO
______*______*______*_WHERE_DINIC_VGCIA_GSTOR_<=_:GFCTB0A1.DPROCM-ATUAL)
______*____END-EXEC.

______*____IF_(SQLCODE______*______*___NOT_EQUAL_ZEROS)_OR
______*______*(SQLWARN0______*______*__EQUAL_'W'______)
______*______*_MOVE_'GFCT0237'______*__TO_GFCT0M-PROGRAMA
______*______*_MOVE_'DB2'______*______*TO_GFCT0M-TIPO-ACESSO
______*______*_MOVE_'PARM_CTRL_GSTAO'_TO_GFCT0M-NOME-TAB
______*______*_MOVE_'INSERT__'______*__TO_GFCT0M-COMANDO-SQL
______*______*_MOVE__SQLCODE______*____TO_GFCT0M-SQLCODE
______*______*_MOVE__'0090'______*_____TO_GFCT0M-LOCAL
______*______*_MOVE__SQLCA(1:136)_TO_GFCT0M-SQLCA-AREA
______*______*_MOVE__9______*______*___TO_GFCT7G-ERRO
______*______*_MOVE__SQLCODE______*____TO_GFCT7G-COD-SQL-ERRO
______*______*_MOVE__0012______*______*TO_GFCT7G-COD-MSG-ERRO
______*______*______*______*______*______*WRK-5000-COD-MSG
______*______*_PERFORM_121000-OBTER-DESC-MSG
______*______*_STRING_WRK-5000-DESC-MSG
______*______*______*_'_-_GFCTB087'
______*______*_DELIMITED_BY_'__'______*INTO_GFCT7G-DESC-MSG-ERRO
______*______*_PERFORM_300000-FINALIZAR
______*____END-IF.

           MOVE CDEPDC-GSTOR OF GFCTB0A0 TO W-JUNCAO-MASTER.

      *----------------------------------------------------------------*
       232210-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       232300-GERAR-D8                 SECTION.
      *----------------------------------------------------------------*

           PERFORM 232310-MOVER-D8.
           PERFORM 232320-UPDATE-D8.

      *----------------------------------------------------------------*
       232300-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       232310-MOVER-D8                 SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF       OF GFCTB087
                                       TO CSERVC-TARIF      OF GFCTB0D8.
           MOVE CBCO-CCREN         OF GFCTB087
                                       TO CBCO-CCREN        OF GFCTB0D8.
           MOVE CIDTFD-LCTO        OF GFCTB087
                                       TO CIDTFD-LCTO       OF GFCTB0D8.
           MOVE CIDTFD-SNAL-LCTO   OF GFCTB087
                                       TO CIDTFD-SNAL-LCTO  OF GFCTB0D8.
           MOVE CEMPR-GSTAO-SERVC  OF GFCTB087
                                       TO CEMPR-INC         OF GFCTB0D8.
           MOVE CDEPDC-GSTAO-SERVC OF GFCTB087
                                       TO CDEPDC            OF GFCTB0D8.
           MOVE CTPO-SERVC-TARIF   OF GFCTB087
                                       TO CTPO-SERVC-TARIF  OF GFCTB0D8
           MOVE RSERVC-TARIF       OF GFCTB087
                                       TO RSERVC-TARIF      OF GFCTB0D8.
           MOVE RSERVC-TARIF-REDZD OF GFCTB087
                                       TO RSERVC-TARIF-REDZD
                                                            OF GFCTB0D8.
           MOVE CPSSOA-SERVC-TARIF OF GFCTB087
                                       TO CPSSOA-SERVC-TARIF
                                                            OF GFCTB0D8.
           MOVE CTPO-CTA-DEB-TARIF OF GFCTB087
                                       TO CTPO-CTA-DEB-TARIF
                                                            OF GFCTB0D8.
           MOVE CTPO-DEB-COBR      OF GFCTB087
                                       TO CTPO-DEB-COBR     OF GFCTB0D8.
           MOVE RUND-COBR-SERVC    OF GFCTB087
                                       TO RUND-COBR-SERVC   OF GFCTB0D8.
           MOVE CLCTO-SERVC        OF GFCTB087
                                       TO CLCTO-SERVC       OF GFCTB0D8.
           MOVE HIDTFD-MANUT-SERVC OF GFCTB087
                                       TO HIDTFD-MANUT-SERVC
                                                            OF GFCTB0D8.
           MOVE CSEQ-MANUT-SERVC   OF GFCTB087
                                       TO CSEQ-MANUT-SERVC  OF GFCTB0D8.
           MOVE CTPO-LCTO-COBR     OF GFCTB087
                                       TO CTPO-LCTO-COBR    OF GFCTB0D8.
           MOVE CTPO-LCTO-ESTRN    OF GFCTB087
                                       TO CTPO-LCTO-ESTRN   OF GFCTB0D8.

           MOVE CCART-DEB-TARIF    OF GFCTB087
                                      TO CCART-DEB-TARIF    OF GFCTB0D8.
           MOVE CINDCD-DEB-PCIAL   OF GFCTB087
                                      TO CINDCD-DEB-PCIAL   OF GFCTB0D8.
           MOVE CINDCD-PCIAL-ULT   OF GFCTB087
                                      TO CINDCD-PCIAL-ULT   OF GFCTB0D8.
           MOVE CINDCD-EVNTO-PCELD OF GFCTB087
                                      TO CINDCD-EVNTO-PCELD OF GFCTB0D8.

           MOVE CINDCD-QTD-REG     OF GFCTB087
                                      TO CINDCD-QTD-REG     OF GFCTB0D8.

RIC        MOVE CINDCD-NRO-DOCTO   OF GFCTB087
RIC                                   TO CINDCD-NRO-DOCTO   OF GFCTB0D8.

161110     MOVE CINDCD-CONS-IMPED  OF GFCTB087
161110                                TO CINDCD-CONS-IMPED  OF GFCTB0D8.
BI0411     MOVE CLCTO-ESTRN-ANTER  OF GFCTB087
BI0411                                TO CLCTO-ESTRN-ANTER  OF GFCTB0D8.

      *----------------------------------------------------------------*
       232310-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       232320-UPDATE-D8                SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               UPDATE DB2PRD.SERVC_TARIF_PRINC SET
                 CBCO_CCREN         = :GFCTB0D8.CBCO-CCREN,
                 CIDTFD_LCTO        = :GFCTB0D8.CIDTFD-LCTO,
                 CIDTFD_SNAL_LCTO   = :GFCTB0D8.CIDTFD-SNAL-LCTO,
                 CEMPR_INC          = :GFCTB0D8.CEMPR-INC,
                 CDEPDC             = :GFCTB0D8.CDEPDC,
                 CTPO_SERVC_TARIF   = :GFCTB0D8.CTPO-SERVC-TARIF,
                 RSERVC_TARIF       = :GFCTB0D8.RSERVC-TARIF,
                 RSERVC_TARIF_REDZD = :GFCTB0D8.RSERVC-TARIF-REDZD,
                 CPSSOA_SERVC_TARIF = :GFCTB0D8.CPSSOA-SERVC-TARIF,
                 CTPO_CTA_DEB_TARIF = :GFCTB0D8.CTPO-CTA-DEB-TARIF,
                 CTPO_DEB_COBR      = :GFCTB0D8.CTPO-DEB-COBR,
                 RUND_COBR_SERVC    = :GFCTB0D8.RUND-COBR-SERVC,
                 CLCTO_SERVC        = :GFCTB0D8.CLCTO-SERVC,
                 HIDTFD_MANUT_SERVC = :GFCTB0D8.HIDTFD-MANUT-SERVC,
                 CSEQ_MANUT_SERVC   = :GFCTB0D8.CSEQ-MANUT-SERVC,
                 CTPO_LCTO_COBR     = :GFCTB0D8.CTPO-LCTO-COBR,
                 CTPO_LCTO_ESTRN    = :GFCTB0D8.CTPO-LCTO-ESTRN,
                 CCART_DEB_TARIF    = :GFCTB0D8.CCART-DEB-TARIF,
                 CINDCD_DEB_PCIAL   = :GFCTB0D8.CINDCD-DEB-PCIAL,
                 CINDCD_PCIAL_ULT   = :GFCTB0D8.CINDCD-PCIAL-ULT,
                 CINDCD_EVNTO_PCELD = :GFCTB0D8.CINDCD-EVNTO-PCELD,
                 CINDCD_QTD_REG     = :GFCTB0D8.CINDCD-QTD-REG,
RIC              CINDCD_NRO_DOCTO   = :GFCTB0D8.CINDCD-NRO-DOCTO,
161110           CINDCD_CONS_IMPED  = :GFCTB0D8.CINDCD-CONS-IMPED,
BI0411           CLCTO_ESTRN_ANTER  = :GFCTB0D8.CLCTO-ESTRN-ANTER
               WHERE CSERVC_TARIF   = :GFCTB0D8.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS)
               MOVE 'GFCT0237'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'SERVC_TARIF_PRINC' TO GFCT0M-NOME-TAB
               MOVE 'UPDATE  '         TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE  '0100'            TO GFCT0M-LOCAL
               MOVE  SQLCA(1:136) TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCT7G-ERRO
               MOVE  SQLCODE           TO GFCT7G-COD-SQL-ERRO
               MOVE  0019              TO GFCT7G-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 121000-OBTER-DESC-MSG
               STRING WRK-5000-DESC-MSG
                      ' - GFCTB086'
               DELIMITED BY '  '       INTO GFCT7G-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       232320-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       233000-CRIAR-NVA-HIST-PARM-TAR  SECTION.
      *----------------------------------------------------------------*

           PERFORM 233200-GERAR-HIST-PARM-TAR.

BI0115     IF W-CTPO-SERVC-TARIF NOT EQUAL 2  AND  4
              PERFORM 233300-GERAR-A2
           END-IF.

      *----------------------------------------------------------------*
       233000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       233200-GERAR-HIST-PARM-TAR      SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-CSERVC-TARIF       TO CSERVC-TARIF     OF GFCTB086.
           MOVE WRK-TIMESTAMP-AUX      TO HIDTFD-MANUT-SERVC
                                                           OF GFCTB086.
           MOVE WRK-CSEQ-MANUT-SERVC-NVA
                                       TO CSEQ-MANUT-SERVC OF GFCTB086.
           MOVE GFCT7F-DT-INI-VGCIA    TO DINIC-VGCIA-TARIF
                                                           OF GFCTB086.
           MOVE GFCT7F-DT-FIM-VGCIA    TO DFIM-VGCIA-TARIF OF GFCTB086.

           EXEC SQL
             INSERT INTO DB2PRD.HIST_PARM_TARIF
                  (CSERVC_TARIF,
                   HIDTFD_MANUT_SERVC,
                   CSEQ_MANUT_SERVC,
                   DINIC_VGCIA_TARIF,
                   DFIM_VGCIA_TARIF,
                   CNATUZ_SERVC,
                   CCJTO_SERVC,
                   QMAX_TENTV_DEB,
                   QFRANQ_TARIF,
                   CPER_FRANQ_TARIF,
                   CPER_CALC_TARIF,
                   CINIC_PER_CALC,
                   DDIA_COBR_TARIF,
                   QDIA_CAREN_TARIF,
                   PDESC_CAREN_TARIF,
                   CINDCD_INIC_CAREN,
                   DINIC_CAREN_TARIF,
                   CINDCD_DEB_CONSL,
                   CINDCD_EXCED_CONSL,
                   RSEGDA_LIN_EXTRT,
                   QDIA_RTCAO_COBR,
                   CINIC_PER_FRANQ,
                   VMIN_ENVIO_COBR,
                   CINDCD_TARIF_PROML,
                   CINDCD_TENTV_COBR,
                   QTENTV_SEM_COBR,
                   CINDCD_RETOR_COBR,
                   CINDCD_CALC_PRORT,
                   CINDCD_PER_CAREN,
                   QMES_CAREN_TARIF,
RIC                QDIA_COBR_TARIF,
RIC                CIDTFD_COBR_FERI,
161110             CINDCD_PCMTO_COBR,
161110             QPCELA_COBR_TARIF,
161110             CIDTFD_COBR_PCELA,
161110             QDIA_COBR_PCELA,
161110             CINDCD_CREDT_CLULR,
BI0411             CINDCD_REPAS_CLULR,
BI0411             VREPAS_CREDT_CLULR,
022012             QDIA_RTCAO_CREDT,
022012             CTPO_FRANQ_TARIF,
022012             CINDCD_PER_COBR)
               VALUES
                  (:GFCTB086.CSERVC-TARIF,
                   :GFCTB086.HIDTFD-MANUT-SERVC,
                   :GFCTB086.CSEQ-MANUT-SERVC,
                   :GFCTB086.DINIC-VGCIA-TARIF,
                   :GFCTB086.DFIM-VGCIA-TARIF,
                   :GFCTB086.CNATUZ-SERVC,
                   :GFCTB086.CCJTO-SERVC,
                   :GFCTB086.QMAX-TENTV-DEB,
                   :GFCTB086.QFRANQ-TARIF,
                   :GFCTB086.CPER-FRANQ-TARIF,
                   :GFCTB086.CPER-CALC-TARIF,
                   :GFCTB086.CINIC-PER-CALC,
                   :GFCTB086.DDIA-COBR-TARIF,
                   :GFCTB086.QDIA-CAREN-TARIF,
                   :GFCTB086.PDESC-CAREN-TARIF,
                   :GFCTB086.CINDCD-INIC-CAREN,
                   :GFCTB086.DINIC-CAREN-TARIF,
                   :GFCTB086.CINDCD-DEB-CONSL,
                   :GFCTB086.CINDCD-EXCED-CONSL,
                   :GFCTB086.RSEGDA-LIN-EXTRT,
                   :GFCTB086.QDIA-RTCAO-COBR,
                   :GFCTB086.CINIC-PER-FRANQ,
                   :GFCTB086.VMIN-ENVIO-COBR,
                   :GFCTB086.CINDCD-TARIF-PROML,
                   :GFCTB086.CINDCD-TENTV-COBR,
                   :GFCTB086.QTENTV-SEM-COBR,
                   :GFCTB086.CINDCD-RETOR-COBR,
                   :GFCTB086.CINDCD-CALC-PRORT,
                   :GFCTB086.CINDCD-PER-CAREN,
                   :GFCTB086.QMES-CAREN-TARIF,
RIC                :GFCTB086.QDIA-COBR-TARIF,
RIC                :GFCTB086.CIDTFD-COBR-FERI,
161110             :GFCTB086.CINDCD-PCMTO-COBR,
161110             :GFCTB086.QPCELA-COBR-TARIF,
161110             :GFCTB086.CIDTFD-COBR-PCELA,
161110             :GFCTB086.QDIA-COBR-PCELA,
161110             :GFCTB086.CINDCD-CREDT-CLULR,
BI0411             :GFCTB086.CINDCD-REPAS-CLULR,
BI0411             :GFCTB086.VREPAS-CREDT-CLULR,
022012             :GFCTB086.QDIA-RTCAO-CREDT,
022012             :GFCTB086.CTPO-FRANQ-TARIF,
022012             :GFCTB086.CINDCD-PER-COBR)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'GFCT0237'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'HIST_PARM_TARIF ' TO GFCT0M-NOME-TAB
               MOVE 'INSERT  '         TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE  '0110'            TO GFCT0M-LOCAL
               MOVE  SQLCA(1:136) TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCT7G-ERRO
               MOVE  SQLCODE           TO GFCT7G-COD-SQL-ERRO
               MOVE  0018              TO GFCT7G-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 121000-OBTER-DESC-MSG
               STRING WRK-5000-DESC-MSG
                      ' - GFCTB086'
               DELIMITED BY '  '       INTO GFCT7G-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       233200-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       233300-GERAR-A2                 SECTION.
      *----------------------------------------------------------------*

            PERFORM 233310-MOVER-A2.

            PERFORM 233320-INSERT-A2.

      *----------------------------------------------------------------*
       233300-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       233310-MOVER-A2                 SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF       OF GFCTB086
                                       TO CSERVC-TARIF      OF GFCTB0A2.
           MOVE DINIC-VGCIA-TARIF  OF GFCTB086
                                       TO DINIC-VGCIA-TARIF OF GFCTB0A2.
           MOVE DFIM-VGCIA-TARIF   OF GFCTB086
                                       TO DFIM-VGCIA-TARIF  OF GFCTB0A2.
           MOVE CNATUZ-SERVC       OF GFCTB086
                                       TO CNATUZ-SERVC      OF GFCTB0A2.
           MOVE CCJTO-SERVC        OF GFCTB086
                                       TO CCJTO-SERVC       OF GFCTB0A2.
           MOVE CINDCD-DEB-CONSL   OF GFCTB086
                                       TO CINDCD-DEB-CONSL  OF GFCTB0A2.
           MOVE CINDCD-EXCED-CONSL OF GFCTB086
                                       TO CINDCD-EXCED-CONSL
                                                            OF GFCTB0A2.
           MOVE QFRANQ-TARIF       OF GFCTB086
                                       TO QFRANQ-TARIF      OF GFCTB0A2.
           MOVE QMAX-TENTV-DEB     OF GFCTB086
                                       TO QMAX-TENTV-DEB    OF GFCTB0A2.
           MOVE CPER-FRANQ-TARIF   OF GFCTB086
                                       TO CPER-FRANQ-TARIF  OF GFCTB0A2.
           MOVE CPER-CALC-TARIF    OF GFCTB086
                                       TO CPER-CALC-TARIF   OF GFCTB0A2.
           MOVE CINIC-PER-CALC     OF GFCTB086
                                       TO CINIC-PER-CALC    OF GFCTB0A2.
           MOVE DDIA-COBR-TARIF    OF GFCTB086
                                       TO DDIA-COBR-TARIF   OF GFCTB0A2.
           MOVE QDIA-CAREN-TARIF   OF GFCTB086
                                       TO QDIA-CAREN-TARIF  OF GFCTB0A2.
           MOVE PDESC-CAREN-TARIF  OF GFCTB086
                                       TO PDESC-CAREN-TARIF OF GFCTB0A2.
           MOVE CINDCD-INIC-CAREN  OF GFCTB086
                                       TO CINDCD-INIC-CAREN OF GFCTB0A2.
           MOVE QDIA-RTCAO-COBR    OF GFCTB086
                                       TO QDIA-RTCAO-COBR   OF GFCTB0A2.
           MOVE DINIC-CAREN-TARIF  OF GFCTB086
                                       TO DINIC-CAREN-TARIF OF GFCTB0A2.
           MOVE RSEGDA-LIN-EXTRT   OF GFCTB086
                                       TO RSEGDA-LIN-EXTRT  OF GFCTB0A2.
           MOVE HIDTFD-MANUT-SERVC OF GFCTB086
                                       TO HIDTFD-MANUT-SERVC
                                                            OF GFCTB0A2.
           MOVE CSEQ-MANUT-SERVC   OF GFCTB086
                                       TO CSEQ-MANUT-SERVC  OF GFCTB0A2.
           MOVE CINIC-PER-FRANQ    OF GFCTB086
                                       TO CINIC-PER-FRANQ   OF GFCTB0A2.
           MOVE VMIN-ENVIO-COBR    OF GFCTB086
                                       TO VMIN-ENVIO-COBR   OF GFCTB0A2.

           MOVE CINDCD-TARIF-PROML OF GFCTB086
                                       TO CINDCD-TARIF-PROML
                                                            OF GFCTB0A2.
           MOVE CINDCD-TENTV-COBR  OF GFCTB086
                                       TO CINDCD-TENTV-COBR
                                                            OF GFCTB0A2.
           MOVE QTENTV-SEM-COBR    OF GFCTB086
                                       TO QTENTV-SEM-COBR   OF GFCTB0A2.
           MOVE CINDCD-RETOR-COBR  OF GFCTB086
                                       TO CINDCD-RETOR-COBR OF GFCTB0A2.
           MOVE CINDCD-CALC-PRORT  OF GFCTB086
                                       TO CINDCD-CALC-PRORT OF GFCTB0A2.
           MOVE CINDCD-PER-CAREN   OF GFCTB086
                                       TO CINDCD-PER-CAREN  OF GFCTB0A2.
           MOVE QMES-CAREN-TARIF   OF GFCTB086
                                       TO QMES-CAREN-TARIF  OF GFCTB0A2.
RIC        MOVE QDIA-COBR-TARIF    OF GFCTB086
RIC                                    TO QDIA-COBR-TARIF   OF GFCTB0A2.
RIC        MOVE CIDTFD-COBR-FERI   OF GFCTB086
RIC                                    TO CIDTFD-COBR-FERI  OF GFCTB0A2.

161110     MOVE CINDCD-PCMTO-COBR  OF GFCTB086
161110                                 TO CINDCD-PCMTO-COBR OF GFCTB0A2.
161110     MOVE QPCELA-COBR-TARIF  OF GFCTB086
161110                                 TO QPCELA-COBR-TARIF OF GFCTB0A2.
161110     MOVE CIDTFD-COBR-PCELA  OF GFCTB086
161110                                 TO CIDTFD-COBR-PCELA OF GFCTB0A2.
161110     MOVE QDIA-COBR-PCELA    OF GFCTB086
161110                                 TO QDIA-COBR-PCELA   OF GFCTB0A2.
161110     MOVE CINDCD-CREDT-CLULR OF GFCTB086
161110                                 TO CINDCD-CREDT-CLULR
161110                                                      OF GFCTB0A2.
BI0411     MOVE CINDCD-REPAS-CLULR OF GFCTB086
BI0411                                 TO CINDCD-REPAS-CLULR
BI0411                                                      OF GFCTB0A2.
BI0411     MOVE VREPAS-CREDT-CLULR OF GFCTB086
BI0411                                 TO VREPAS-CREDT-CLULR
BI0411                                                      OF GFCTB0A2.
BI0411     MOVE QDIA-RTCAO-CREDT   OF GFCTB086
BI0411                                 TO QDIA-RTCAO-CREDT
BI0411                                                      OF GFCTB0A2.
022012     MOVE CTPO-FRANQ-TARIF   OF GFCTB086
022012                                 TO CTPO-FRANQ-TARIF  OF GFCTB0A2.
           MOVE CINDCD-PER-COBR   OF GFCTB086
                                       TO CINDCD-PER-COBR  OF GFCTB0A2.

      *----------------------------------------------------------------*
       233310-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       233320-INSERT-A2                SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               INSERT  INTO DB2PRD.PARM_SERVC_TARIF
                       (CSERVC_TARIF,
                        DINIC_VGCIA_TARIF,
                        DFIM_VGCIA_TARIF,
                        CNATUZ_SERVC,
                        CCJTO_SERVC,
                        CINDCD_DEB_CONSL,
                        CINDCD_EXCED_CONSL,
                        QFRANQ_TARIF,
                        QMAX_TENTV_DEB,
                        CPER_FRANQ_TARIF,
                        CPER_CALC_TARIF,
                        CINIC_PER_CALC,
                        DDIA_COBR_TARIF,
                        QDIA_CAREN_TARIF,
                        PDESC_CAREN_TARIF,
                        CINDCD_INIC_CAREN,
                        QDIA_RTCAO_COBR,
                        DINIC_CAREN_TARIF,
                        RSEGDA_LIN_EXTRT,
                        HIDTFD_MANUT_SERVC,
                        CSEQ_MANUT_SERVC,
                        CINIC_PER_FRANQ,
                        VMIN_ENVIO_COBR,
                        CINDCD_TARIF_PROML,
                        CINDCD_TENTV_COBR,
                        QTENTV_SEM_COBR,
                        CINDCD_RETOR_COBR,
                        CINDCD_CALC_PRORT,
                        CINDCD_PER_CAREN,
                        QMES_CAREN_TARIF,
RIC                     QDIA_COBR_TARIF,
RIC                     CIDTFD_COBR_FERI,
161110                  CINDCD_PCMTO_COBR,
161110                  QPCELA_COBR_TARIF,
161110                  CIDTFD_COBR_PCELA,
161110                  QDIA_COBR_PCELA,
161110                  CINDCD_CREDT_CLULR,
BI0411                  CINDCD_REPAS_CLULR,
BI0411                  VREPAS_CREDT_CLULR,
022012                  QDIA_RTCAO_CREDT,
022012                  CTPO_FRANQ_TARIF,
022012                  CINDCD_PER_COBR)
               VALUES  (:GFCTB0A2.CSERVC-TARIF,
                        :GFCTB0A2.DINIC-VGCIA-TARIF,
                        :GFCTB0A2.DFIM-VGCIA-TARIF,
                        :GFCTB0A2.CNATUZ-SERVC,
                        :GFCTB0A2.CCJTO-SERVC,
                        :GFCTB0A2.CINDCD-DEB-CONSL,
                        :GFCTB0A2.CINDCD-EXCED-CONSL,
                        :GFCTB0A2.QFRANQ-TARIF,
                        :GFCTB0A2.QMAX-TENTV-DEB,
                        :GFCTB0A2.CPER-FRANQ-TARIF,
                        :GFCTB0A2.CPER-CALC-TARIF,
                        :GFCTB0A2.CINIC-PER-CALC,
                        :GFCTB0A2.DDIA-COBR-TARIF,
                        :GFCTB0A2.QDIA-CAREN-TARIF,
                        :GFCTB0A2.PDESC-CAREN-TARIF,
                        :GFCTB0A2.CINDCD-INIC-CAREN,
                        :GFCTB0A2.QDIA-RTCAO-COBR,
                        :GFCTB0A2.DINIC-CAREN-TARIF,
                        :GFCTB0A2.RSEGDA-LIN-EXTRT,
                        :GFCTB0A2.HIDTFD-MANUT-SERVC,
                        :GFCTB0A2.CSEQ-MANUT-SERVC,
                        :GFCTB0A2.CINIC-PER-FRANQ,
                        :GFCTB0A2.VMIN-ENVIO-COBR,
                        :GFCTB0A2.CINDCD-TARIF-PROML,
                        :GFCTB0A2.CINDCD-TENTV-COBR,
                        :GFCTB0A2.QTENTV-SEM-COBR,
                        :GFCTB0A2.CINDCD-RETOR-COBR,
                        :GFCTB0A2.CINDCD-CALC-PRORT,
                        :GFCTB0A2.CINDCD-PER-CAREN,
                        :GFCTB0A2.QMES-CAREN-TARIF,
RIC                     :GFCTB0A2.QDIA-COBR-TARIF,
RIC                     :GFCTB0A2.CIDTFD-COBR-FERI,
161110                  :GFCTB0A2.CINDCD-PCMTO-COBR,
161110                  :GFCTB0A2.QPCELA-COBR-TARIF,
161110                  :GFCTB0A2.CIDTFD-COBR-PCELA,
161110                  :GFCTB0A2.QDIA-COBR-PCELA,
161110                  :GFCTB0A2.CINDCD-CREDT-CLULR,
BI0411                  :GFCTB0A2.CINDCD-REPAS-CLULR,
BI0411                  :GFCTB0A2.VREPAS-CREDT-CLULR,
022012                  :GFCTB0A2.QDIA-RTCAO-CREDT,
022012                  :GFCTB0A2.CTPO-FRANQ-TARIF,
022012                  :GFCTB0A2.CINDCD-PER-COBR)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS ) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'GFCT0237'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'PARM_SERVC_TARIF' TO GFCT0M-NOME-TAB
               MOVE 'INSERT  '         TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE  '0120'            TO GFCT0M-LOCAL
               MOVE  SQLCA(1:136) TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCT7G-ERRO
               MOVE  SQLCODE           TO GFCT7G-COD-SQL-ERRO
               MOVE  0018              TO GFCT7G-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 121000-OBTER-DESC-MSG
               STRING WRK-5000-DESC-MSG
                      ' - GFCTB085'
               DELIMITED BY '  '       INTO GFCT7G-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       233320-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       234000-CRIAR-HIST-EVEN-CTBIL    SECTION.
      *----------------------------------------------------------------*

BI0115     IF W-CTPO-SERVC-TARIF NOT EQUAL 2  AND  4
              PERFORM 234500-DELETE-47
           END-IF.

            PERFORM 234100-OPEN-85.

            PERFORM 234200-FETCH-85.

            PERFORM 234300-PROCESSA-85 UNTIL SQLCODE EQUAL +100.

            PERFORM 234400-CLOSE-85.

      *----------------------------------------------------------------*
       234000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       234100-OPEN-85                  SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-CSERVC-TARIF       TO CSERVC-TARIF     OF GFCTB085.
           MOVE WRK-HIDTFD-MANUT-SERVC TO HIDTFD-MANUT-SERVC
                                                           OF GFCTB085.
           MOVE WRK-CSEQ-MANUT-SERVC   TO CSEQ-MANUT-SERVC OF GFCTB085.

           EXEC SQL
             OPEN CRS-GFCTB085
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS ) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'GFCT0237'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'HIST_EVNTO_CTBIL' TO GFCT0M-NOME-TAB
               MOVE 'OPEN    '         TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE  '0130'            TO GFCT0M-LOCAL
               MOVE  SQLCA(1:136) TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCT7G-ERRO
               MOVE  SQLCODE           TO GFCT7G-COD-SQL-ERRO
               MOVE  0005              TO GFCT7G-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 121000-OBTER-DESC-MSG
               STRING WRK-5000-DESC-MSG
                      ' - GFCTB085'
               DELIMITED BY '  '       INTO GFCT7G-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       234100-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       234200-FETCH-85                 SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH CRS-GFCTB085
             INTO
                   :GFCTB085.CSERVC-TARIF,
                   :GFCTB085.HIDTFD-MANUT-SERVC,
                   :GFCTB085.CSEQ-MANUT-SERVC,
                   :GFCTB085.CTPO-LCTO-CTBIL,
                   :GFCTB085.CSEQ-EVNTO-CTBIL,
                   :GFCTB085.CINDCD-DEPDC-CTBIL,
                   :GFCTB085.PATRIC-CTBIL,
                   :GFCTB085.CDEPDC-CTLZO,
                   :GFCTB085.CPRODT-CTBIL,
                   :GFCTB085.COPER-CTBIL,
                   :GFCTB085.CEMPR,
                   :GFCTB085.CGRP-RZ-CTBIL,
                   :GFCTB085.CSGRP-RZ-CTBIL,
                   :GFCTB085.CCTA-EVNTO-CTBIL,
                   :GFCTB085.CDOCTO-CTBIL
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'GFCT0237'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'HIST_EVNTO_CTBIL' TO GFCT0M-NOME-TAB
               MOVE 'FETCH   '         TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE  '0140'            TO GFCT0M-LOCAL
               MOVE  SQLCA(1:136) TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCT7G-ERRO
               MOVE  SQLCODE           TO GFCT7G-COD-SQL-ERRO
               MOVE  0006              TO GFCT7G-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 121000-OBTER-DESC-MSG
               STRING WRK-5000-DESC-MSG
                      ' - GFCTB085'
               DELIMITED BY '  '       INTO GFCT7G-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       234200-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       234300-PROCESSA-85              SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-CSERVC-TARIF       TO CSERVC-TARIF     OF GFCTB085.
           MOVE WRK-TIMESTAMP-AUX      TO HIDTFD-MANUT-SERVC
                                                           OF GFCTB085.
           MOVE WRK-CSEQ-MANUT-SERVC-NVA TO CSEQ-MANUT-SERVC
                                                           OF GFCTB085.

           EXEC SQL
             INSERT INTO DB2PRD.HIST_EVNTO_CTBIL
                  (CSERVC_TARIF,
                   HIDTFD_MANUT_SERVC,
                   CSEQ_MANUT_SERVC,
                   CTPO_LCTO_CTBIL,
                   CSEQ_EVNTO_CTBIL,
                   CINDCD_DEPDC_CTBIL,
                   PATRIC_CTBIL,
                   CDEPDC_CTLZO,
                   CPRODT_CTBIL,
                   COPER_CTBIL,
                   CEMPR,
                   CGRP_RZ_CTBIL,
                   CSGRP_RZ_CTBIL,
                   CCTA_EVNTO_CTBIL,
                   CDOCTO_CTBIL)
               VALUES
                  (:GFCTB085.CSERVC-TARIF,
                   :GFCTB085.HIDTFD-MANUT-SERVC,
                   :GFCTB085.CSEQ-MANUT-SERVC,
                   :GFCTB085.CTPO-LCTO-CTBIL,
                   :GFCTB085.CSEQ-EVNTO-CTBIL,
                   :GFCTB085.CINDCD-DEPDC-CTBIL,
                   :GFCTB085.PATRIC-CTBIL,
                   :GFCTB085.CDEPDC-CTLZO,
                   :GFCTB085.CPRODT-CTBIL,
                   :GFCTB085.COPER-CTBIL,
                   :GFCTB085.CEMPR,
                   :GFCTB085.CGRP-RZ-CTBIL,
                   :GFCTB085.CSGRP-RZ-CTBIL,
                   :GFCTB085.CCTA-EVNTO-CTBIL,
                   :GFCTB085.CDOCTO-CTBIL)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'GFCT0237'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'HIST_EVNTO_CTBIL' TO GFCT0M-NOME-TAB
               MOVE 'INSERT  '         TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE  '0150'            TO GFCT0M-LOCAL
               MOVE  SQLCA(1:136) TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCT7G-ERRO
               MOVE  SQLCODE           TO GFCT7G-COD-SQL-ERRO
               MOVE  0018              TO GFCT7G-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 121000-OBTER-DESC-MSG
               STRING WRK-5000-DESC-MSG
                      ' - GFCTB085'
               DELIMITED BY '  '       INTO GFCT7G-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

BI0115     IF W-CTPO-SERVC-TARIF NOT EQUAL 2  AND  4
              PERFORM 234310-GERAR-47
           END-IF.

           PERFORM 234200-FETCH-85.

      *----------------------------------------------------------------*
       234300-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       234310-GERAR-47                 SECTION.
      *----------------------------------------------------------------*

           PERFORM 234311-MOVER-47.

           PERFORM 234312-INSERT-47.

      *----------------------------------------------------------------*
       234310-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       234311-MOVER-47                 SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF       OF GFCTB085
                                       TO CSERVC-TARIF      OF GFCTB047.
           MOVE CTPO-LCTO-CTBIL    OF GFCTB085
                                       TO CTPO-LCTO-CTBIL   OF GFCTB047.
           MOVE CSEQ-EVNTO-CTBIL   OF GFCTB085
                                       TO CSEQ-EVNTO-CTBIL  OF GFCTB047.
           MOVE CINDCD-DEPDC-CTBIL OF GFCTB085
                                       TO CINDCD-DEPDC-CTBIL
                                                            OF GFCTB047.
           MOVE PATRIC-CTBIL       OF GFCTB085
                                       TO PATRIC-CTBIL      OF GFCTB047.
           MOVE CDEPDC-CTLZO       OF GFCTB085
                                       TO CDEPDC-CTLZO      OF GFCTB047.
           MOVE CPRODT-CTBIL       OF GFCTB085
                                       TO CPRODT-CTBIL      OF GFCTB047.
           MOVE COPER-CTBIL        OF GFCTB085
                                       TO COPER-CTBIL       OF GFCTB047.
           MOVE HIDTFD-MANUT-SERVC OF GFCTB085
                                       TO HIDTFD-MANUT-SERVC
                                                            OF GFCTB047.
           MOVE CSEQ-MANUT-SERVC   OF GFCTB085
                                       TO CSEQ-MANUT-SERVC  OF GFCTB047.

           MOVE CEMPR              OF GFCTB085
                                       TO CEMPR             OF GFCTB047.
           MOVE CGRP-RZ-CTBIL      OF GFCTB085
                                       TO CGRP-RZ-CTBIL     OF GFCTB047.
           MOVE CSGRP-RZ-CTBIL     OF GFCTB085
                                       TO CSGRP-RZ-CTBIL    OF GFCTB047.
           MOVE CCTA-EVNTO-CTBIL   OF GFCTB085
                                       TO CCTA-EVNTO-CTBIL  OF GFCTB047.
           MOVE CDOCTO-CTBIL       OF GFCTB085
                                       TO CDOCTO-CTBIL      OF GFCTB047.

      *----------------------------------------------------------------*
       234311-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       234312-INSERT-47                SECTION.
      *----------------------------------------------------------------*
           EXEC SQL
               INSERT  INTO DB2PRD.TARIF_EVNTO_CTBIL
                       (CSERVC_TARIF,
                        CTPO_LCTO_CTBIL,
                        CSEQ_EVNTO_CTBIL,
                        CINDCD_DEPDC_CTBIL,
                        PATRIC_CTBIL,
                        CDEPDC_CTLZO,
                        CPRODT_CTBIL,
                        COPER_CTBIL,
                        HIDTFD_MANUT_SERVC,
                        CSEQ_MANUT_SERVC,
                        CEMPR,
                        CGRP_RZ_CTBIL,
                        CSGRP_RZ_CTBIL,
                        CCTA_EVNTO_CTBIL,
                        CDOCTO_CTBIL)
               VALUES  (:GFCTB047.CSERVC-TARIF,
                        :GFCTB047.CTPO-LCTO-CTBIL,
                        :GFCTB047.CSEQ-EVNTO-CTBIL,
                        :GFCTB047.CINDCD-DEPDC-CTBIL,
                        :GFCTB047.PATRIC-CTBIL,
                        :GFCTB047.CDEPDC-CTLZO,
                        :GFCTB047.CPRODT-CTBIL,
                        :GFCTB047.COPER-CTBIL,
                        :GFCTB047.HIDTFD-MANUT-SERVC,
                        :GFCTB047.CSEQ-MANUT-SERVC,
                        :GFCTB047.CEMPR,
                        :GFCTB047.CGRP-RZ-CTBIL,
                        :GFCTB047.CSGRP-RZ-CTBIL,
                        :GFCTB047.CCTA-EVNTO-CTBIL,
                        :GFCTB047.CDOCTO-CTBIL)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS ) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'GFCT0237'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'TARIF_EVNTO_CTBIL' TO GFCT0M-NOME-TAB
               MOVE 'INSERT  '         TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE  '0160'            TO GFCT0M-LOCAL
               MOVE  SQLCA(1:136) TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCT7G-ERRO
               MOVE  SQLCODE           TO GFCT7G-COD-SQL-ERRO
               MOVE  0018              TO GFCT7G-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 121000-OBTER-DESC-MSG
               STRING WRK-5000-DESC-MSG
                      ' - GFCTB085'
               DELIMITED BY '  '       INTO GFCT7G-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       234312-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       234400-CLOSE-85                 SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE  CRS-GFCTB085
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'GFCT0237'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'HIST_EVNTO_CTBIL' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE  '0170'            TO GFCT0M-LOCAL
               MOVE  SQLCA(1:136) TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCT7G-ERRO
               MOVE  SQLCODE           TO GFCT7G-COD-SQL-ERRO
               MOVE  0011              TO GFCT7G-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 121000-OBTER-DESC-MSG
               STRING WRK-5000-DESC-MSG
                      ' - GFCTB088'
               DELIMITED BY '  '       INTO GFCT7G-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       234400-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       234500-DELETE-47                SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-DELETE-47.


______*____MOVE_CSERVC-TARIF_OF_GFCTB087_TO_CSERVC-TARIF_OF_GFCTB047.

______*____EXEC_SQL
______*______*_DELETE_FROM__DB2PRD.TARIF_EVNTO_CTBIL
______*______*_WHERE__CSERVC_TARIF_=_:GFCTB047.CSERVC-TARIF
______*____END-EXEC.

______*____IF_(SQLCODE______*______*___NOT_EQUAL_ZEROS_AND_+100)_OR
______*______*(SQLWARN0______*______*__EQUAL_'W'______)
______*______*_MOVE_'GFCT0237'______*__TO_GFCT0M-PROGRAMA
______*______*_MOVE_'DB2'______*______*TO_GFCT0M-TIPO-ACESSO
______*______*_MOVE_'TARIF_EVNTO_CTBIL'_TO_GFCT0M-NOME-TAB
______*______*_MOVE_'DELETE__'______*__TO_GFCT0M-COMANDO-SQL
______*______*_MOVE__SQLCODE______*____TO_GFCT0M-SQLCODE
______*______*_MOVE__'0180'______*_____TO_GFCT0M-LOCAL
______*______*_MOVE__SQLCA______*______TO_GFCT0M-SQLCA-AREA
______*______*_MOVE__9______*______*___TO_GFCT7G-ERRO
______*______*_MOVE__SQLCODE______*____TO_GFCT7G-COD-SQL-ERRO
______*______*_MOVE__0020______*______*TO_GFCT7G-COD-MSG-ERRO
______*______*______*______*______*______*WRK-5000-COD-MSG
______*______*_PERFORM_121000-OBTER-DESC-MSG
______*______*_STRING_WRK-5000-DESC-MSG
______*______*______*_'_-_GFCTB085'
______*______*_DELIMITED_BY_'__'______*INTO_GFCT7G-DESC-MSG-ERRO
______*______*_PERFORM_300000-FINALIZAR
______*____END-IF.

      *----------------------------------------------------------------*
       234500-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       235000-CRIAR-HIST-VLR-COMP      SECTION.
      *----------------------------------------------------------------*

            PERFORM 235100-ABRIR-CRS-GFCTB088.

            PERFORM 235200-LER-CRS-GFCTB088.

            PERFORM 235300-PROC-VLR-COMP   UNTIL
                    WRK-FIM-CURSOR         EQUAL 'S'.

            PERFORM 235400-FECHAR-CRS-GFCTB088.

      *----------------------------------------------------------------*
       235000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       235100-ABRIR-CRS-GFCTB088       SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FIM-CURSOR.
           MOVE WRK-CSERVC-TARIF       TO CSERVC-TARIF     OF GFCTB088.
           MOVE WRK-CSEQ-MANUT-SERVC   TO CSEQ-MANUT-SERVC OF GFCTB088.
           MOVE WRK-HIDTFD-MANUT-SERVC TO HIDTFD-MANUT-SERVC
                                                           OF GFCTB088.

           EXEC SQL
               OPEN CRS-GFCTB088
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'GFCT0237'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'HIST_VLR_COMPO  ' TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE  '0190'            TO GFCT0M-LOCAL
               MOVE  SQLCA(1:136) TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCT7G-ERRO
               MOVE  SQLCODE           TO GFCT7G-COD-SQL-ERRO
               MOVE  0005              TO GFCT7G-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 121000-OBTER-DESC-MSG
               STRING WRK-5000-DESC-MSG
                      ' - GFCTB088'
               DELIMITED BY '  '       INTO GFCT7G-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       235100-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       235200-LER-CRS-GFCTB088         SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH CRS-GFCTB088  INTO
                   :GFCTB088.CSERVC-TARIF,
                   :GFCTB088.HIDTFD-MANUT-SERVC,
                   :GFCTB088.CSEQ-MANUT-SERVC,
                   :GFCTB088.DINIC-VGCIA-TARIF,
                   :GFCTB088.CSEQ-COMPO-TARIF,
                   :GFCTB088.RCOMPO-TARIF,
                   :GFCTB088.VCOMPO-TARIF,
                   :GFCTB088.PSERVC-PTADO,
                   :GFCTB088.VMAX-COMPO-TARIF,
                   :GFCTB088.VMIN-COMPO-TARIF,
                   :GFCTB088.CMOMEN-CONVS-MOEDA,
                   :GFCTB088.CMOEDA-BACEN
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'               )
               MOVE 'GFCT0237'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'HIST_VLR_COMPO  ' TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE  '0200'            TO GFCT0M-LOCAL
               MOVE  SQLCA(1:136) TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCT7G-ERRO
               MOVE  SQLCODE           TO GFCT7G-COD-SQL-ERRO
               MOVE  0006              TO GFCT7G-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 121000-OBTER-DESC-MSG
               STRING WRK-5000-DESC-MSG
                      ' - GFCTB088'
               DELIMITED BY '  '       INTO GFCT7G-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-CURSOR
           END-IF.

      *----------------------------------------------------------------*
       235200-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       235300-PROC-VLR-COMP            SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-CSERVC-TARIF       TO CSERVC-TARIF     OF GFCTB088.
           MOVE WRK-CSEQ-MANUT-SERVC-NVA
                                       TO CSEQ-MANUT-SERVC OF GFCTB088.
           MOVE WRK-TIMESTAMP-AUX      TO HIDTFD-MANUT-SERVC
                                                           OF GFCTB088.
           INSPECT GFCT7F-DT-INI-VGCIA REPLACING  ALL '/' BY '.'.
           MOVE GFCT7F-DT-INI-VGCIA  TO DINIC-VGCIA-TARIF  OF GFCTB088.

           EXEC SQL
             INSERT INTO DB2PRD.HIST_VLR_COMPO
                  (CSERVC_TARIF,
                   HIDTFD_MANUT_SERVC,
                   CSEQ_MANUT_SERVC,
                   DINIC_VGCIA_TARIF,
                   CSEQ_COMPO_TARIF,
                   RCOMPO_TARIF,
                   VCOMPO_TARIF,
                   PSERVC_PTADO,
                   VMAX_COMPO_TARIF,
                   VMIN_COMPO_TARIF,
                   CMOMEN_CONVS_MOEDA,
                   CMOEDA_BACEN)
               VALUES
                  (:GFCTB088.CSERVC-TARIF,
                   :GFCTB088.HIDTFD-MANUT-SERVC,
                   :GFCTB088.CSEQ-MANUT-SERVC,
                   :GFCTB088.DINIC-VGCIA-TARIF,
                   :GFCTB088.CSEQ-COMPO-TARIF,
                   :GFCTB088.RCOMPO-TARIF,
                   :GFCTB088.VCOMPO-TARIF,
                   :GFCTB088.PSERVC-PTADO,
                   :GFCTB088.VMAX-COMPO-TARIF,
                   :GFCTB088.VMIN-COMPO-TARIF,
                   :GFCTB088.CMOMEN-CONVS-MOEDA,
                   :GFCTB088.CMOEDA-BACEN)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'GFCT0237'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'HIST_VLR_COMPO  ' TO GFCT0M-NOME-TAB
               MOVE 'INSERT'           TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE  '0210'            TO GFCT0M-LOCAL
               MOVE  SQLCA(1:136) TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCT7G-ERRO
               MOVE  SQLCODE           TO GFCT7G-COD-SQL-ERRO
               MOVE  0018              TO GFCT7G-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 121000-OBTER-DESC-MSG
               STRING WRK-5000-DESC-MSG
                      ' - GFCTB088'
               DELIMITED BY '  '       INTO GFCT7G-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

BI0115     IF W-CTPO-SERVC-TARIF NOT EQUAL 2  AND  4
              PERFORM 235310-GERAR-G3
           END-IF.

           PERFORM 235200-LER-CRS-GFCTB088.

      *----------------------------------------------------------------*
       235300-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       235310-GERAR-G3                 SECTION.
      *----------------------------------------------------------------*

           PERFORM 235311-MOVER-G3.
           PERFORM 235312-INSERT-G3.

      *----------------------------------------------------------------*
       235310-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       235311-MOVER-G3                 SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF      OF GFCTB088
                                       TO CSERVC-TARIF      OF GFCTB0G3.
           MOVE DINIC-VGCIA-TARIF OF GFCTB088
                                       TO DINIC-VGCIA-TARIF OF GFCTB0G3.
           MOVE CSEQ-COMPO-TARIF   OF GFCTB088
                                       TO CSEQ-COMPO-TARIF  OF GFCTB0G3.
           MOVE CMOEDA-BACEN       OF GFCTB088
                                       TO CMOEDA-BACEN      OF GFCTB0G3.
           MOVE RCOMPO-TARIF       OF GFCTB088
                                       TO RCOMPO-TARIF      OF GFCTB0G3.
           MOVE VCOMPO-TARIF       OF GFCTB088
                                       TO VCOMPO-TARIF      OF GFCTB0G3.
           MOVE PSERVC-PTADO       OF GFCTB088
                                       TO PSERVC-PTADO      OF GFCTB0G3.
           MOVE VMAX-COMPO-TARIF   OF GFCTB088
                                       TO VMAX-COMPO-TARIF  OF GFCTB0G3.
           MOVE VMIN-COMPO-TARIF   OF GFCTB088
                                       TO VMIN-COMPO-TARIF  OF GFCTB0G3.
           MOVE CMOMEN-CONVS-MOEDA OF GFCTB088
                                       TO CMOMEN-CONVS-MOEDA
                                                            OF GFCTB0G3.

      *----------------------------------------------------------------*
       235311-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       235312-INSERT-G3                SECTION.
      *----------------------------------------------------------------*
           EXEC SQL
               INSERT  INTO DB2PRD.VLR_COMPO_TARIF
                       (CSERVC_TARIF,
                        DINIC_VGCIA_TARIF,
                        CSEQ_COMPO_TARIF,
                        CMOEDA_BACEN,
                        RCOMPO_TARIF,
                        VCOMPO_TARIF,
                        PSERVC_PTADO,
                        VMAX_COMPO_TARIF,
                        VMIN_COMPO_TARIF,
                        CMOMEN_CONVS_MOEDA)
               VALUES  (:GFCTB0G3.CSERVC-TARIF,
                        :GFCTB0G3.DINIC-VGCIA-TARIF,
                        :GFCTB0G3.CSEQ-COMPO-TARIF,
                        :GFCTB0G3.CMOEDA-BACEN,
                        :GFCTB0G3.RCOMPO-TARIF,
                        :GFCTB0G3.VCOMPO-TARIF,
                        :GFCTB0G3.PSERVC-PTADO,
                        :GFCTB0G3.VMAX-COMPO-TARIF,
                        :GFCTB0G3.VMIN-COMPO-TARIF,
                        :GFCTB0G3.CMOMEN-CONVS-MOEDA)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'GFCT0237'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'VLR_COMPO_TARIF'  TO GFCT0M-NOME-TAB
               MOVE 'INSERT'           TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE  '0220'            TO GFCT0M-LOCAL
               MOVE  SQLCA(1:136) TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCT7G-ERRO
               MOVE  SQLCODE           TO GFCT7G-COD-SQL-ERRO
               MOVE  0018              TO GFCT7G-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 121000-OBTER-DESC-MSG
               STRING WRK-5000-DESC-MSG
                      ' - GFCTB088'
               DELIMITED BY '  '       INTO GFCT7G-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       235312-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       235400-FECHAR-CRS-GFCTB088      SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE  CRS-GFCTB088
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'GFCT0237'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'HIST_VLR_COMPO  ' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE  '0230'            TO GFCT0M-LOCAL
               MOVE  SQLCA(1:136) TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCT7G-ERRO
               MOVE  SQLCODE           TO GFCT7G-COD-SQL-ERRO
               MOVE  0011              TO GFCT7G-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 121000-OBTER-DESC-MSG
               STRING WRK-5000-DESC-MSG
                      ' - GFCTB088'
               DELIMITED BY '  '       INTO GFCT7G-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       235400-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       300000-FINALIZAR                SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       300000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
