      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT1884.
       AUTHOR.     DOUGLAS ROGERIO RAMOS.
      *================================================================*
      *                    C P M - S I S T E M A S                     *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT1884                                    *
      *    PROGRAMADOR.:   DOUGLAS ROGERIO RAMOS   - CPM / PATO BRANCO *
      *    ANALISTA CPM:   FELIPE ALEX SOMMER      - CPM / PATO BRANCO *
      *    ANALISTA....:   MARIA VALERIA           - PROCKWORK / GP 50 *
      *    DATA........:   04/10/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   APLICACAO DA FLEXIBILIZACAO POR AGRUPAMENTO *
      *      NA ROTINA DE COBRANCA.                                    *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                      INCLUDE/BOOK        *
      *                  EVENTO92                    I#GFCTVA          *
      *                  FRANQUIA                    I#GFCTVG          *
      *                  ISENTO92                    I#GFCTVA          *
      *                  EVENTOAT                    I#GFCTVA          *
      *                  DESCTO41                    I#GFCTV4          *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                       INCLUDE/BOOK      *
      *                    DB2PRD.PARM_DATA_PROCM      GFCTB0A1        *
      *                    DB2PRD.FLEXZ_AGPTO_CTA      GFCTB049        *
      *                    DB2PRD.FLEXZ_GRP_CLI        GFCTB050        *
      *                    DB2PRD.FLEXZ_GRP_DEPDC      GFCTB051        *
      *                    DB2PRD.FLEXZ_GRP_MUN        GFCTB053        *
      *                    DB2PRD.FLEXZ_GRP_PAB        GFCTB054        *
      *                    DB2PRD.FLEXZ_GRP_PSTAL      GFCTB055        *
      *                    DB2PRD.FLEXZ_GRP_SGMTO      GFCTB056        *
      *                    DB2PRD.FLEXZ_GRP_UF         GFCTB057        *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    POL7100C - AREA PARA TRATAMENTO DE ERRO PELA POOL7100.      *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    POOL1050 - CONEXAO COM DB2.                                 *
      *    POOL1285 - SOMA UM DETERMINADO NUMERO DE DIAS UTEIS, EM UMA *
      *               UMA DATA QUALQUER.                               *
      *    POOL7100 - MODULO PARA CANCELAMENTO DO PROGRAMA.            *
      *================================================================*
      *================================================================*
      *    ULTIMA ALTERACAO EM 22/08/2008 - VALERIA                    *
      *  - INCLUSAO DOS CAMPOS PERIODO DE COBRANCA E INICIO DO PERIODO *
      *    DE COBRANCA.                                                *
      *  - CALCULO DA DATA DE AGENDAMENTO QUANDO PERIODO INFORMADO     *
      *  - TRATAMENTO DOS AGRUPAMENTOS PERSONALIZADOS                  *
      *================================================================*
060110*================================================================*
060110*       ALTERACAO EM 06/01/2010  - VINICIUS / WAGNER - GP50      *
060110*================================================================*
060110*  - SOLUCAO DE ABEND.                                           *
060110*    - RETIRADA DO SINAL DO CAMPO WRK-DAGNDA-DEB-MOVTO           *
060110*================================================================*
      *    PROJETO - PWT-09-2711 - ALTERACAO JUNHO/2010                *
      *    INCLUSAO DOS AGPTOS 20 - BRADESCO EXPRESSO E 26 - RAZAO     *
      *    ATUALIZACAO DA INC I#GFCTVA                                 *
      *================================================================*
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

           SELECT EVENTO92 ASSIGN      TO UT-S-EVENTO92
           FILE STATUS                 IS WRK-FS-EVENTO92.

           SELECT FRANQUIA ASSIGN      TO UT-S-FRANQUIA
           FILE STATUS                 IS WRK-FS-FRANQUIA.

           SELECT ISENTO92 ASSIGN      TO UT-S-ISENTO92
           FILE STATUS                 IS WRK-FS-ISENTO92.

           SELECT EVENTOAT ASSIGN      TO UT-S-EVENTOAT
           FILE STATUS                 IS WRK-FS-EVENTOAT.

           SELECT DESCTO41 ASSIGN      TO UT-S-DESCTO41
           FILE STATUS                 IS WRK-FS-DESCTO41.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:  REGISTROS EVENTO DO DIA                             *
      *            ORG. SEQUENCIAL     -   LRECL   =   250             *
      *----------------------------------------------------------------*

       FD  EVENTO92
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS  0 RECORDS.

       01  REG-EVENTO92                    PIC X(250).

      *----------------------------------------------------------------*
      *   INPUT:   QUANTIDADE DE FRANQUIAS CONCEDIAS NO PERIODO        *
      *            ORG. SEQUENCIAL     -   LRECL   =   100             *
      *----------------------------------------------------------------*

       FD  FRANQUIA
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS  0 RECORDS.

       COPY 'I#GFCTVG'.

      *----------------------------------------------------------------*
      *   OUTPUT:  REGISTROS DE EVENTOS ISENTOS                        *
      *            ORG. SEQUENCIAL     -   LRECL   =   250             *
      *----------------------------------------------------------------*

       FD  ISENTO92
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS  0 RECORDS.

       01  REG-ISENTO92                    PIC X(250).

      *----------------------------------------------------------------*
      *   OUTPUT:  REGISTROS EVENTOS ATUALIZADOS                       *
      *            ORG. SEQUENCIAL     -   LRECL   =   250             *
      *----------------------------------------------------------------*

       FD  EVENTOAT
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS  0 RECORDS.

       01  REG-EVENTOAT                    PIC X(250).

      *----------------------------------------------------------------*
      *   OUTPUT:  REGISTROS DE DESCONTOS                              *
      *            ORG. SEQUENCIAL     -   LRECL   =   150             *
      *----------------------------------------------------------------*

       FD  DESCTO41
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS  0 RECORDS.

       COPY 'I#GFCTV4'.

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** INICIO DA WORKING-STORAGE SECTION ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** INDICES ***'.
      *----------------------------------------------------------------*

       77  IND-1                       PIC  9(003) COMP-3  VALUE ZEROS.
       77  IND-2                       PIC  9(003) COMP-3  VALUE ZEROS.
       77  IND-CIDTFD-DESC             PIC S9(005) COMP-3  VALUE ZEROS.
220808 77  IND-3                       PIC S9(003) COMP-3  VALUE ZEROS.
220808

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** ACUMULADORES ***'.
      *----------------------------------------------------------------*

       77  ACU-LIDOS-EVENTO92          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVS-ISENTO92          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVS-EVENTOAT          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVS-DESCTO41          PIC  9(009) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*

       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.

       77  WRK-MASCARA                 PIC  ZZZZ.ZZZ.999   VALUE ZEROS.
       77  WRK-VLR-DESC                PIC S9(009)V9(002)  COMP-3
                                                           VALUE ZEROS.
       77  WRK-VLR-PERC                PIC S9(009)V9(004)  COMP-3
                                                           VALUE ZEROS.
       77  WRK-VLR-PERC-DESC           PIC S9(009)V9(004)  VALUE ZEROS.
       77  WRK-VALOR-LIQUIDO           PIC S9(009)V9(002)  COMP-3
                                                           VALUE ZEROS.
       77  WRK-VALOR-PERCENT2          PIC S9(009)V9(002)  COMP-3
                                                           VALUE ZEROS.
       77  WRK-SEM-FLEXZ               PIC  X(001)         VALUE SPACES.
       77  WRK-JA-GRAVOU               PIC  X(001)         VALUE SPACES.
       77  WRK-GRAVAR-EVENTOS          PIC  X(001)         VALUE SPACES.

LFM    01  WRK-HIERARQUIA-AGPT         PIC 9(18)           VALUE ZEROS.
LFM    01  FILLER    REDEFINES         WRK-HIERARQUIA-AGPT.
LFM        05  WRK-AGPT-REG            PIC 9(02)           OCCURS 9.
LFM    01  WRK-GFCTVA-DOCOR-EVNTO      PIC  X(010)       VALUE SPACES.
LFM    01  FILLER           REDEFINES  WRK-GFCTVA-DOCOR-EVNTO.
LFM        05  WRK-DIA-EVNTO           PIC  9(002).
LFM        05  FILLER                  PIC  X(001).
LFM        05  WRK-MES-EVNTO           PIC  9(002).
LFM        05  FILLER                  PIC  X(001).
LFM        05  WRK-ANO-EVNTO           PIC  9(004).
LFM    01  WRK-GFCTVA-DOCOR-EVNTO-INV  PIC  9(008)         VALUE ZEROS.
LFM    01  FILLER           REDEFINES   WRK-GFCTVA-DOCOR-EVNTO-INV.
LFM        05  WRK-ANO-EVNTO-INV       PIC  9(004).
LFM        05  WRK-MES-EVNTO-INV       PIC  9(002).
LFM        05  WRK-DIA-EVNTO-INV       PIC  9(002).
LFM
LFM    01 WRK-CEMPR                PIC S9(5) COMP-3  VALUE ZEROS.
LFM    01 WRK-CGRP-CTBIL-ATUAL     PIC S9(3) COMP-3  VALUE ZEROS.
LFM    01 WRK-CSGRP-CTBIL          PIC S9(3) COMP-3  VALUE ZEROS.
220808
220808*---------------------------------------------------------------*
220808 01  FILLER                      PIC  X(028)         VALUE
220808     'AREA PARA AUXILIARES DE DATA'.
220808*---------------------------------------------------------------*
220808
220808 01  WRK-DT-SISTEMA              PIC  X(010)         VALUE SPACES.
220808
220808 01  WRK-DT-APURADA              PIC  X(010)         VALUE SPACES.
220808 01  WRK-DT-APURADA-R            REDEFINES WRK-DT-APURADA.
220808     05  WRK-DIA-DT-APURADA      PIC  9(002).
220808     05  WRK-PONTO-3             PIC  X(001).
220808     05  WRK-MES-DT-APURADA      PIC  9(002).
220808     05  WRK-PONTO-4             PIC  X(001).
220808     05  WRK-ANO-DT-APURADA      PIC  9(004).
220808
220808 01  WRK-DATA-EDI.
220808     05  WRK-DIA-EDI             PIC  9(002)         VALUE ZEROS.
220808     05  FILLER                  PIC  X(001)         VALUE '.'.
220808     05  WRK-MES-EDI             PIC  9(002)         VALUE ZEROS.
220808     05  FILLER                  PIC  X(001)         VALUE '.'.
220808     05  WRK-ANO-EDI             PIC  9(004)         VALUE ZEROS.
220808
220808 01  WRK-DATA-INV-CBR            PIC  9(008)         VALUE ZEROS.
220808 01  WRK-DATA-INV-R-CBR          REDEFINES WRK-DATA-INV-CBR.
220808     05  WRK-ANO-INV-CBR         PIC  9(004).
220808     05  WRK-MES-INV-CBR         PIC  9(002).
220808     05  WRK-DIA-INV-CBR         PIC  9(002).
220808
RU0125 01 WRK-CPTCAO-GFCTB041          PIC  X(010) VALUE SPACES.
RU0125 01 FILLER                       REDEFINES WRK-CPTCAO-GFCTB041.
           05 WRK-CPTCAO-B041          PIC 9(002).
RU0125     05 FILLER                   PIC X(008).
220808*----------------------------------------------------------------*
220808 01  FILLER                      PIC  X(024)         VALUE
220808     'AREA PARA TABELA INTERNA'.
220808*----------------------------------------------------------------*
220808
220808 01  TAB-TABELA-DIAS.
220808     05 FILLER                 PIC  X(014) VALUE '1DOMINGO'.
220808     05 FILLER                 PIC  X(014) VALUE '2SEGUNDA-FEIRA'.
220808     05 FILLER                 PIC  X(014) VALUE '3TERCA-FEIRA'.
220808     05 FILLER                 PIC  X(014) VALUE '4QUARTA-FEIRA'.
220808     05 FILLER                 PIC  X(014) VALUE '5QUINTA-FEIRA'.
220808     05 FILLER                 PIC  X(014) VALUE '6SEXTA-FEIRA'.
220808     05 FILLER                 PIC  X(014) VALUE '7SABADO'.
220808 01  FILLER      REDEFINES     TAB-TABELA-DIAS.
220808     05 FILLER                 OCCURS 7 TIMES.
220808        10 TAB-DIA-CINIC       PIC  9(001).
220808        10 TAB-DIA-SEMANA      PIC  X(013).
220808

       01  WRK-TIMESTAMP               PIC  X(026)         VALUE SPACES.

       01  WRK-TAB-FRANQUIA.
           05  WRK-TAB-FRANQUIA-OCOR   OCCURS 9.
             10  WRK-CONT-FRANQUIA     PIC S9(003) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** REGISTRO DE EVENTOS    ***'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTVA'.

       01  WRK-CAMPOS.
LFM        05  WRK-CAMPOS-OCOR          OCCURS 9.
220808       10  WRK-DINIC-AMD          PIC  9(008)        VALUE ZEROS.
             10  FILLER    REDEFINES         WRK-DINIC-AMD .
220808           15  WRK-DINIC-ANO      PIC  9(004).
220808           15  WRK-DINIC-MES      PIC  9(002).
220808           15  WRK-DINIC-DIA      PIC  9(002).
220808       10  WRK-DFIM-AMD           PIC  9(008)        VALUE ZEROS.
             10  FILLER    REDEFINES         WRK-DFIM-AMD.
220808           15  WRK-DFIM-ANO       PIC  9(004).
220808           15  WRK-DFIM-MES       PIC  9(002).
220808           15  WRK-DFIM-DIA       PIC  9(002).
             10  WRK-QFRANQ-FLEXZ-CTA   PIC S9(003).
             10  WRK-DDIA-AGNDA         PIC S9(002).
             10  WRK-QDIA-FLEXZ-CTA     PIC S9(002)        VALUE ZEROS.
             10  WRK-VNEGOC-FLEXZ-AGPTO PIC S9(009)V9(002) COMP-3
                                                           VALUE ZEROS.
             10  WRK-PDESC-FLEXZ-CTA    PIC S9(003)V9(004) COMP-3
                                                           VALUE ZEROS.
220808       10  WRK-CPER-COBR          PIC S9(001)        VALUE ZEROS.
220808       10  WRK-CINIC-PER          PIC S9(001)        VALUE ZEROS.
             10  WRK-TEM-FLEXZ          PIC  X(001)        VALUE SPACES.

       01  WRK-VALOR-PERCENT-TOT       PIC S9(009)V9(004) VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-VALOR-PERCENT-TOT.
           05  WRK-VLR-PERC-V2         PIC S9(009)V9(002).
           05  FILLER                  PIC S9(002).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** VARIAVEIS DE DATA ***'.
      *----------------------------------------------------------------*

       01  WRK-DATA-DB2                PIC  X(010)         VALUE SPACES.
       01  FILLER                      REDEFINES           WRK-DATA-DB2.
           05  WRK-DIA-DB2             PIC  9(002).
           05  FILLER                  PIC  X(001).
           05  WRK-MES-DB2             PIC  9(002).
           05  FILLER                  PIC  X(001).
           05  WRK-ANO-DB2             PIC  9(004).

       01  WRK-DOCOR-EVNTO             PIC  X(010)         VALUE SPACES.
       01  FILLER                      REDEFINES        WRK-DOCOR-EVNTO.
           05  WRK-DIA-DOCOR           PIC  9(002).
           05  FILLER                  PIC  X(001).
           05  WRK-MES-DOCOR           PIC  9(002).
           05  FILLER                  PIC  X(001).
           05  WRK-ANO-DOCOR           PIC  9(004).

       01  WRK-DAGNDA-DEB-MOVTO        PIC  X(010)         VALUE SPACES.
       01  FILLER                      REDEFINES   WRK-DAGNDA-DEB-MOVTO.
060110     05  WRK-DIA-DAGNDA          PIC  9(002).
           05  FILLER                  PIC  X(001).
060110     05  WRK-MES-DAGNDA          PIC  9(002).
           05  FILLER                  PIC  X(001).
060110     05  WRK-ANO-DAGNDA          PIC  9(004).

       01  WRK-DATA-1285               PIC  9(008)         VALUE ZEROS.
       01  FILLER                      REDEFINES          WRK-DATA-1285.
060110     05  WRK-ANO-1285            PIC  9(004).
060110     05  WRK-MES-1285            PIC  9(002).
060110     05  WRK-DIA-1285            PIC  9(002).

       01  WRK-DATA-AMD1               PIC  9(008)         VALUE ZEROS.
       01  FILLER                      REDEFINES          WRK-DATA-AMD1.
060110     05  WRK-ANO-AMD1            PIC  9(004).
060110     05  WRK-MES-AMD1            PIC  9(002).
060110     05  WRK-DIA-AMD1            PIC  9(002).

       01  WRK-DATA-AMD2               PIC  9(008)         VALUE ZEROS.
       01  FILLER                      REDEFINES          WRK-DATA-AMD2.
           05  WRK-ANO-AMD2            PIC  9(004).
           05  WRK-MES-AMD2            PIC  9(002).
           05  WRK-DIA-AMD2            PIC  9(002).

220808 01  WRK-DOCOR-AMD               PIC  9(008)        VALUE ZEROS.
220808 01  FILLER                      REDEFINES          WRK-DOCOR-AMD.
220808     05  WRK-DOCOR-ANO           PIC  9(004).
220808     05  WRK-DOCOR-MES           PIC  9(002).
220808     05  WRK-DOCOR-DIA           PIC  9(002).

060110 01  WRK-DDIA-AGNDA-SINAL        PIC S9(002)              VALUE 0.
060110 01  FILLER                      REDEFINES   WRK-DDIA-AGNDA-SINAL.
060110     05  WRK-DDIA-AGNDA-SEM      PIC 9(002).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA POOL1285 ***'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-P1285              PIC  X(075)         VALUE
           '*** ERRO NO ACESSO A POOL1285 ***'.

       01  WRK-AREA-POOL1285.
           05  WRK-DATA-ENTRADA-1285   PIC  9(008) COMP-3  VALUE ZEROS.
           05  WRK-NUMERO-DIAS-1285    PIC S9(005) COMP-3  VALUE ZEROS.
           05  WRK-DATA-SAIDA-1285     PIC  9(008) COMP-3  VALUE ZEROS.
           05  WRK-MENSAGEM            PIC  X(050)         VALUE SPACES.

220808
220808*----------------------------------------------------------------*
220808 01  FILLER                      PIC  X(017)         VALUE
220808     'AREAS DA POOL1205'.
220808*----------------------------------------------------------------*
220808
220808 01  WRK-ERRO-P1205              PIC  X(031)         VALUE
220808     '** ERRO NO ACESSO A POOL1205 **'.
220808
220808 01  WRK-MENSAGEM-1205           PIC  X(050)         VALUE SPACES.
220808
220808 01  WRK-AREA-POOL1205.
220808     05  WRK-DT-ENVIADA          PIC  9(008)  COMP-3 VALUE ZEROS.
220808     05  WRK-OPCAO               PIC  X(001)         VALUE SPACES.
220808     05  WRK-DT-JULIANA          PIC  9(007)  COMP-3 VALUE ZEROS.
220808     05  WRK-DT-EDITADA          PIC  X(010)         VALUE SPACES.
220808     05  WRK-DT-GREGORI          PIC  9(008)         VALUE ZEROS.
220808     05  WRK-DIA-SEMANA          PIC  X(013)         VALUE SPACES.
220808     05  WRK-MES-EXTENSO         PIC  X(009)         VALUE SPACES.
220808     05  WRK-DIA-UT-ANT          PIC  9(008)         VALUE ZEROS.
220808     05  WRK-DIA-UT-POS          PIC  9(008)         VALUE ZEROS.
220808
220808*----------------------------------------------------------------*
220808 01  FILLER                      PIC  X(018)         VALUE
220808     'AREA PARA POOL1640'.
220808*----------------------------------------------------------------*
220808
220808 01  WRK-AREA-POOL1640.
220808     05  WRK-DT-ENTRADA-1640     PIC  X(010)         VALUE SPACES.
220808     05  WRK-FORMATO-ENT-1640    PIC  9(002)         VALUE ZEROS.
220808     05  WRK-TP-INCREM-1640      PIC  X(002)         VALUE SPACES.
220808     05  WRK-INCREMENTO-1640     PIC  9(003)         VALUE ZEROS.
220808     05  WRK-FORMATO-SAI-1640    PIC  9(002)         VALUE ZEROS.
220808     05  WRK-DT-SAIDA-1640       PIC  X(010)         VALUE SPACES.
220808     05  WRK-MENSAGEM-1640       PIC  X(080)         VALUE SPACES.
220808
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA CHAVES ***'.
      *----------------------------------------------------------------*

       01  WRK-CHAVE92-ANT.
           05  WRK-CHAVE01-ARQ92-ANT.
             10  WRK-CHV92-BCO-ANT     PIC S9(003)         VALUE ZEROS.
             10  WRK-CHV92-AG-ANT      PIC S9(005)         VALUE ZEROS.
             10  WRK-CHV92-CTA-ANT     PIC S9(013)         VALUE ZEROS.
             10  WRK-CHV92-TARIFA-ANT  PIC S9(005)         VALUE ZEROS.
           05  WRK-CHAVE02-ARQ92-ANT.
             10  WRK-CHV92-ANO-ANT     PIC  9(004)         VALUE ZEROS.
             10  WRK-CHV92-MES-ANT     PIC  9(002)         VALUE ZEROS.

220808 01  WRK-ARQ92-DIA-ANT           PIC  9(002)         VALUE ZEROS.

       01  WRK-CHAVE92-ATU.
           05  WRK-CHAVE01-ARQ92-ATU.
             10  WRK-CHV92-BCO-ATU     PIC S9(003)         VALUE ZEROS.
             10  WRK-CHV92-AG-ATU      PIC S9(005)         VALUE ZEROS.
             10  WRK-CHV92-CTA-ATU     PIC S9(013)         VALUE ZEROS.
             10  WRK-CHV92-TARIFA-ATU  PIC S9(005)         VALUE ZEROS.
           05  WRK-CHAVE02-ARQ92-ATU.
             10  WRK-CHV92-ANO-ATU     PIC  9(004)         VALUE ZEROS.
             10  WRK-CHV92-MES-ATU     PIC  9(002)         VALUE ZEROS.

220808 01  WRK-ARQ92-DIA-ATU           PIC  9(002)         VALUE ZEROS.

       01  WRK-CHAVE-FRANQUIA.
           10  WRK-CHVFRQ-BCO          PIC S9(003)         VALUE ZEROS.
           10  WRK-CHVFRQ-AG           PIC S9(005)         VALUE ZEROS.
           10  WRK-CHVFRQ-CTA          PIC S9(013)         VALUE ZEROS.
           10  WRK-CHVFRQ-TARIFA       PIC S9(005)         VALUE ZEROS.
           10  WRK-CHVFRQ-ANO          PIC  9(004)         VALUE ZEROS.
           10  WRK-CHVFRQ-MES          PIC  9(002)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA TESTES DE FILE-STATUS ***'.
      *----------------------------------------------------------------*

       01  WRK-FS-EVENTO92             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-FRANQUIA             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-ISENTO92             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-EVENTOAT             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-DESCTO41             PIC  X(002)         VALUE SPACES.

       01  WRK-ABERTURA                PIC  X(013)         VALUE
           ' NA ABERTURA '.
       01  WRK-LEITURA                 PIC  X(013)         VALUE
           ' NA LEITURA  '.
       01  WRK-GRAVACAO                PIC  X(013)         VALUE
           ' NA GRAVACAO '.
       01  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** MENSAGEM DE ERRO DE FILE-STATUS ***'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-POOL7100.
           05 FILLER                   PIC  X(009)         VALUE
              '*** ERRO '.
           05 WRK-OPERACAO             PIC  X(013)         VALUE SPACES.
           05 FILLER                   PIC  X(012)         VALUE
              ' DO ARQUIVO '.
           05 WRK-NOME-ARQ             PIC  X(008)         VALUE SPACES.
           05 FILLER                   PIC  X(017)         VALUE
              ' - FILE-STATUS = '.
           05 WRK-FILE-STATUS          PIC  X(002)         VALUE SPACES.
           05 FILLER                   PIC  X(004)         VALUE ' ***'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA POOL7100 ***'.
      *----------------------------------------------------------------*

       COPY POL7100C.

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
               INCLUDE GFCTB049
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB050
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB051
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB053
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB054
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB055
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB056
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB057
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0M8
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** FIM DA WORKING-STORAGE SECTION ***'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      ******************************************************************
      *    CHAMA AS PRINCIPAIS ROTINAS DO PROGRAMA.                    *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-VERIFICAR-VAZIO.

           PERFORM 3000-PROCESSAR      UNTIL
                   WRK-FS-EVENTO92     EQUAL '10'.

           PERFORM 4000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ABERTURA DOS ARQUIVOS E TESTE DE FILE-STATUS.               *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL1050'.

           PERFORM 1100-OBTER-DATA-GFCTB0A1.

           OPEN INPUT  EVENTO92
                       FRANQUIA
                OUTPUT ISENTO92
                       EVENTOAT
                       DESCTO41.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 1200-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    EFETUA UM S E L E C T NA TABELA GFCTB0A1.                   *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-OBTER-DATA-GFCTB0A1        SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             SELECT
                   DPROCM_ATUAL,
                   DPROCM_PROX
             INTO
                   :GFCTB0A1.DPROCM-ATUAL,
                   :GFCTB0A1.DPROCM-PROX
             FROM   DB2PRD.PARM_DATA_PROCM
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PARM_DATA_PROCM'  TO ERR-DBD-TAB
               MOVE 'SELECT    '       TO ERR-FUN-COMANDO
               MOVE '0010'             TO ERR-LOCAL
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTAR FILE-STATUS DOS ARQUIVOS.                            *
      ******************************************************************
      *----------------------------------------------------------------*
       1200-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1210-TESTAR-FS-EVENTO92.

           PERFORM 1220-TESTAR-FS-FRANQUIA.

           PERFORM 1230-TESTAR-FS-ISENTO92.

           PERFORM 1240-TESTAR-FS-EVENTOAT.

           PERFORM 1250-TESTAR-FS-DESCTO41.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTAR FILE-STATUS DO ARQUIVO EVENTO92.                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1210-TESTAR-FS-EVENTO92         SECTION.
      *----------------------------------------------------------------*

           IF (WRK-FS-EVENTO92         NOT EQUAL '00')
               MOVE 'APL'              TO  ERR-TIPO-ACESSO
               MOVE 'EVENTO92'         TO  WRK-NOME-ARQ
               MOVE  WRK-FS-EVENTO92   TO  WRK-FILE-STATUS
               MOVE  WRK-ERRO-POOL7100 TO  ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTAR FILE-STATUS DO ARQUIVO FRANQUIA.                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1220-TESTAR-FS-FRANQUIA         SECTION.
      *----------------------------------------------------------------*

           IF (WRK-FS-FRANQUIA         NOT EQUAL '00')
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'FRANQUIA'         TO WRK-NOME-ARQ
               MOVE  WRK-FS-FRANQUIA   TO WRK-FILE-STATUS
               MOVE  WRK-ERRO-POOL7100 TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTAR FILE-STATUS DO ARQUIVO ISENTO92.                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1230-TESTAR-FS-ISENTO92         SECTION.
      *----------------------------------------------------------------*

           IF (WRK-FS-ISENTO92         NOT EQUAL '00')
               MOVE 'APL'              TO  ERR-TIPO-ACESSO
               MOVE 'ISENTO92'         TO  WRK-NOME-ARQ
               MOVE  WRK-FS-ISENTO92   TO  WRK-FILE-STATUS
               MOVE  WRK-ERRO-POOL7100 TO  ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTAR FILE-STATUS DO ARQUIVO EVENTOAT.                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1240-TESTAR-FS-EVENTOAT         SECTION.
      *----------------------------------------------------------------*

           IF (WRK-FS-EVENTOAT         NOT EQUAL '00')
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'EVENTOAT'         TO WRK-NOME-ARQ
               MOVE  WRK-FS-EVENTOAT   TO WRK-FILE-STATUS
               MOVE  WRK-ERRO-POOL7100 TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1240-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTAR FILE-STATUS DO ARQUIVO DESCTO41.                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1250-TESTAR-FS-DESCTO41         SECTION.
      *----------------------------------------------------------------*

           IF (WRK-FS-DESCTO41         NOT EQUAL '00')
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'DESCTO41'         TO WRK-NOME-ARQ
               MOVE  WRK-FS-DESCTO41   TO WRK-FILE-STATUS
               MOVE  WRK-ERRO-POOL7100 TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1250-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICAR SE OS ARQUIVOS DE ENTRADA ESTAO VAZIOS.           *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2200-LER-EVENTO92.

           IF (WRK-FS-EVENTO92             EQUAL '10')
               DISPLAY '**********************************************'
               DISPLAY '*                                            *'
               DISPLAY '*              PROGRAMA GFCT1884             *'
               DISPLAY '*              -----------------             *'
               DISPLAY '*                                            *'
               DISPLAY '*          ARQUIVO DE EVENTO92 VAZIO         *'
               DISPLAY '*                                            *'
               DISPLAY '*          PROCESSAMENTO  FINALIZADO         *'
               DISPLAY '*                                            *'
               DISPLAY '**********************************************'
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    LE O ARQUIVO FRANQUIA.                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-LER-FRANQUIA               SECTION.
      *----------------------------------------------------------------*

           READ FRANQUIA.

           IF (WRK-FS-FRANQUIA         EQUAL '10')
               MOVE HIGH-VALUES        TO WRK-CHAVE-FRANQUIA
               GO TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1220-TESTAR-FS-FRANQUIA.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    LE O ARQUIVO EVENTO92.                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-LER-EVENTO92               SECTION.
      *----------------------------------------------------------------*

           READ EVENTO92.

           IF (WRK-FS-EVENTO92           EQUAL '10')
               GO TO 2200-99-FIM
           END-IF.

           MOVE WRK-LEITURA              TO WRK-OPERACAO.

           PERFORM 1210-TESTAR-FS-EVENTO92.

           ADD 1                         TO ACU-LIDOS-EVENTO92.


           MOVE REG-EVENTO92             TO GFCTVA-GFCTB092.
           MOVE GFCTVA-CBCO-DSTNO-MOVTO  TO WRK-CHV92-BCO-ATU.
           MOVE GFCTVA-CAG-DSTNO-MOVTO   TO WRK-CHV92-AG-ATU.
           MOVE GFCTVA-CCTA-DSTNO-MOVTO  TO WRK-CHV92-CTA-ATU.
           MOVE GFCTVA-CSERVC-TARIF      TO WRK-CHV92-TARIFA-ATU.
           MOVE GFCTVA-DOCOR-EVNTO       TO WRK-DOCOR-EVNTO.
           MOVE WRK-ANO-DOCOR            TO WRK-CHV92-ANO-ATU
220808                                      WRK-DOCOR-ANO.
           MOVE WRK-MES-DOCOR            TO WRK-CHV92-MES-ATU
220808                                      WRK-DOCOR-MES.
220808     MOVE WRK-DIA-DOCOR            TO WRK-ARQ92-DIA-ATU
220808                                      WRK-DOCOR-DIA.

LFM        MOVE GFCTVA-AGPT-REG(1)      TO   WRK-AGPT-REG(1)
LFM        MOVE GFCTVA-AGPT-REG(2)      TO   WRK-AGPT-REG(2)
LFM        MOVE GFCTVA-AGPT-REG(3)      TO   WRK-AGPT-REG(3)
LFM        MOVE GFCTVA-AGPT-REG(4)      TO   WRK-AGPT-REG(4)
LFM        MOVE GFCTVA-AGPT-REG(5)      TO   WRK-AGPT-REG(5)
LFM        MOVE GFCTVA-AGPT-REG(6)      TO   WRK-AGPT-REG(6)
LFM        MOVE GFCTVA-AGPT-REG(7)      TO   WRK-AGPT-REG(7)

LFM        MOVE GFCTVA-AGPT-CMPL(1)     TO   WRK-AGPT-REG(8)
LFM        MOVE GFCTVA-AGPT-CMPL(2)     TO   WRK-AGPT-REG(9)

           IF (WRK-CHAVE92-ANT           NOT EQUAL WRK-CHAVE92-ATU)
               INITIALIZE                WRK-TAB-FRANQUIA
               PERFORM 2210-CONTAR-FRANQUIAS   UNTIL
                       WRK-CHAVE-FRANQUIA  GREATER WRK-CHAVE92-ATU OR
                       WRK-CHAVE-FRANQUIA  EQUAL HIGH-VALUES
               MOVE WRK-ANO-DOCOR        TO WRK-CHV92-ANO-ANT
               MOVE WRK-MES-DOCOR        TO WRK-CHV92-MES-ANT
           END-IF.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ATUALIZA CHAVE FRANQUIA.                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2210-CONTAR-FRANQUIAS           SECTION.
      *----------------------------------------------------------------*

           IF (WRK-CHAVE-FRANQUIA       EQUAL WRK-CHAVE92-ATU)
               MOVE VG-CIDTFD-DESC      TO IND-CIDTFD-DESC
               ADD 1                    TO
                                      WRK-CONT-FRANQUIA(IND-CIDTFD-DESC)
           END-IF.

           PERFORM 2100-LER-FRANQUIA.

           IF WRK-CHAVE-FRANQUIA  EQUAL HIGH-VALUES
               GO TO 2210-99-FIM
           END-IF.

           MOVE VG-CBCO-DSTNO-MOVTO     TO WRK-CHVFRQ-BCO.
           MOVE VG-CAG-DSTNO-MOVTO      TO WRK-CHVFRQ-AG.
           MOVE VG-CCTA-DSTNO-MOVTO     TO WRK-CHVFRQ-CTA.
           MOVE VG-CSERVC-TARIF         TO WRK-CHVFRQ-TARIFA.
           MOVE VG-DOCOR-EVNTO          TO WRK-DATA-DB2.
           MOVE WRK-ANO-DB2             TO WRK-CHVFRQ-ANO.
           MOVE WRK-MES-DB2             TO WRK-CHVFRQ-MES.

      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCESSAMENTO PRINCIPAL DO PROGRAMA.                        *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           IF (WRK-CHAVE92-ANT         NOT EQUAL    WRK-CHAVE92-ATU)
               INITIALIZE              WRK-CAMPOS
               MOVE ZEROS              TO  WRK-VALOR-PERCENT-TOT
               PERFORM 3100-PESQUISAR-FLEXIBILIZACAO VARYING
                       IND-1 FROM 1 BY 1  UNTIL
LFM                    IND-1 GREATER   9

               MOVE WRK-CHAVE92-ATU     TO WRK-CHAVE92-ANT
220808         MOVE WRK-DIA-DOCOR       TO WRK-ARQ92-DIA-ANT
           END-IF.

220808     IF (WRK-ARQ92-DIA-ANT       NOT EQUAL    WRK-ARQ92-DIA-ATU)
220808         PERFORM 3105-VERIFICA-VIGENCIA-FLEX VARYING
220808                 IND-1 FROM 1 BY 1  UNTIL
LFM                    IND-1 GREATER   9
220808
220808         MOVE WRK-DIA-DOCOR       TO  WRK-ARQ92-DIA-ANT
220808     END-IF

           MOVE 'N'                        TO WRK-GRAVAR-EVENTOS.

           MOVE ' '                        TO WRK-JA-GRAVOU

           PERFORM 3200-PROCESSAR-FLEXIBILIZACAO VARYING
                   IND-1 FROM 1 BY 1  UNTIL
LFM                IND-1 GREATER   9  OR
                   GFCTVA-VTARIF-LIQ-MOVTO = ZEROS.

           IF  WRK-JA-GRAVOU           EQUAL ' '
               PERFORM 3300-GRAVAR-EVENTOS
           END-IF.

           PERFORM 2200-LER-EVENTO92.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PESQUISA FLEXIBILIZACAO.                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       3100-PESQUISAR-FLEXIBILIZACAO   SECTION.
      *----------------------------------------------------------------*

LFM        EVALUATE  WRK-AGPT-REG (IND-1)

               WHEN 4
                    PERFORM 3110-AGRUPAMENTO-CLIENTE

               WHEN 14
                    PERFORM 3120-AGRUPAMENTO-AGENCIA

               WHEN 9
                    PERFORM 3130-AGRUPAMENTO-SEGMENTO

               WHEN 15
                    PERFORM 3140-AGRUPAMENTO-PAB

               WHEN 17
                    PERFORM 3150-AGRUPAMENTO-POSTAL

               WHEN 18
                    PERFORM 3160-AGRUPAMENTO-MUNICIPIO

               WHEN 19
                    PERFORM 3170-AGRUPAMENTO-UF

LFM            WHEN 20
LFM                 PERFORM 3155-AGRUPAMENTO-BRAD-EXPRS
LFM
LFM            WHEN 26
LFM                 PERFORM 3180-AGRUPAMENTO-RAZAO
LFM
           END-EVALUATE.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

220808******************************************************************
220808*    VERIFICA SE A FLEXIBILIZACAO EH VIGENTE NO DIA DO EVENTO    *
220808******************************************************************
220808*----------------------------------------------------------------*
220808 3105-VERIFICA-VIGENCIA-FLEX     SECTION.
220808*----------------------------------------------------------------*
220808
LFM        EVALUATE WRK-AGPT-REG(IND-1)
220808
220808         WHEN 4
220808          IF   WRK-DOCOR-AMD       NOT LESS  WRK-DINIC-AMD(1)
220808               AND  WRK-DOCOR-AMD  NOT GREATER WRK-DFIM-AMD(1)
220808               MOVE WRK-DIA-DOCOR  TO  WRK-ARQ92-DIA-ANT
220808          ELSE
220808               PERFORM 3110-AGRUPAMENTO-CLIENTE
220808          END-IF
220808
220808         WHEN 14
220808          IF   WRK-DOCOR-AMD       NOT LESS  WRK-DINIC-AMD(3)
220808               AND  WRK-DOCOR-AMD  NOT GREATER WRK-DFIM-AMD(3)
220808               MOVE WRK-DIA-DOCOR  TO  WRK-ARQ92-DIA-ANT
220808          ELSE
220808               PERFORM 3120-AGRUPAMENTO-AGENCIA
220808          END-IF
220808
220808         WHEN 9
220808          IF   WRK-DOCOR-AMD       NOT LESS  WRK-DINIC-AMD(2)
220808               AND  WRK-DOCOR-AMD  NOT GREATER WRK-DFIM-AMD(2)
220808               MOVE WRK-DIA-DOCOR  TO  WRK-ARQ92-DIA-ANT
220808          ELSE
220808               PERFORM 3130-AGRUPAMENTO-SEGMENTO
220808          END-IF
220808
220808         WHEN 15
220808          IF   WRK-DOCOR-AMD       NOT LESS  WRK-DINIC-AMD(4)
220808               AND  WRK-DOCOR-AMD  NOT GREATER WRK-DFIM-AMD(4)
220808               MOVE WRK-DIA-DOCOR  TO  WRK-ARQ92-DIA-ANT
220808          ELSE
220808               PERFORM 3140-AGRUPAMENTO-PAB
220808          END-IF
220808
220808         WHEN 17
220808          IF   WRK-DOCOR-AMD       NOT LESS  WRK-DINIC-AMD(5)
220808               AND  WRK-DOCOR-AMD  NOT GREATER WRK-DFIM-AMD(5)
220808               MOVE WRK-DIA-DOCOR  TO  WRK-ARQ92-DIA-ANT
220808          ELSE
220808               PERFORM 3150-AGRUPAMENTO-POSTAL
220808          END-IF
220808
220808         WHEN 18
220808          IF   WRK-DOCOR-AMD       NOT LESS  WRK-DINIC-AMD(6)
220808               AND  WRK-DOCOR-AMD  NOT GREATER WRK-DFIM-AMD(6)
220808               MOVE WRK-DIA-DOCOR  TO  WRK-ARQ92-DIA-ANT
220808          ELSE
220808               PERFORM 3160-AGRUPAMENTO-MUNICIPIO
220808          END-IF
220808
220808         WHEN 19
220808          IF   WRK-DOCOR-AMD       NOT LESS  WRK-DINIC-AMD(7)
220808               AND  WRK-DOCOR-AMD  NOT GREATER WRK-DFIM-AMD(7)
220808               MOVE WRK-DIA-DOCOR  TO  WRK-ARQ92-DIA-ANT
220808          ELSE
220808               PERFORM 3170-AGRUPAMENTO-UF
220808          END-IF
220808
LFM            WHEN 20
LFM             IF   WRK-DOCOR-AMD       NOT LESS  WRK-DINIC-AMD(8)
LFM                  AND  WRK-DOCOR-AMD  NOT GREATER WRK-DFIM-AMD(8)
LFM                  MOVE WRK-DIA-DOCOR  TO  WRK-ARQ92-DIA-ANT
LFM             ELSE
LFM                  PERFORM 3155-AGRUPAMENTO-BRAD-EXPRS
LFM             END-IF
LFM
LFM            WHEN 26
LFM             IF   WRK-DOCOR-AMD       NOT LESS  WRK-DINIC-AMD(9)
LFM                  AND  WRK-DOCOR-AMD  NOT GREATER WRK-DFIM-AMD(9)
LFM                  MOVE WRK-DIA-DOCOR  TO  WRK-ARQ92-DIA-ANT
LFM             ELSE
LFM                  PERFORM 3180-AGRUPAMENTO-RAZAO
LFM             END-IF
220808
220808     END-EVALUATE.
220808
220808
220808*----------------------------------------------------------------*
220808 3105-99-FIM.                    EXIT.
220808*----------------------------------------------------------------*

      ******************************************************************
      *    AGRUPA CLIENTE.                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       3110-AGRUPAMENTO-CLIENTE        SECTION.
      *----------------------------------------------------------------*

ST2506*    MOVE GFCTVA-CPF-CNPJ        TO CCGC-CPF          OF GFCTB050.
ST2506*    MOVE GFCTVA-FILIAL          TO CFLIAL-CGC        OF GFCTB050.
ST2506*    MOVE GFCTVA-CONTROLE        TO CCTRL-CPF-CGC     OF GFCTB050.
ST2506     MOVE GFCTVA-CPF-CNPJ        TO CCGC-CPF-ST       OF GFCTB050.
ST2506     MOVE GFCTVA-FILIAL          TO CFLIAL-CGC-ST     OF GFCTB050.
ST2506     MOVE GFCTVA-CONTROLE        TO CCTRL-CPF-CGC-ST  OF GFCTB050.
           MOVE GFCTVA-CSERVC-TARIF    TO CSERVC-TARIF      OF GFCTB050.
LFM        MOVE WRK-AGPT-REG(IND-1)    TO CAGPTO-CTA        OF GFCTB050.
           MOVE GFCTVA-DOCOR-EVNTO     TO DINIC-FLEXZ-AGPTO OF GFCTB049
                                          DFIM-FLEXZ-AGPTO  OF GFCTB049.
           MOVE ZEROS                  TO CDEPDC-ENCRR-VGCIA
                                                            OF GFCTB049
                                          CFUNC-ENCRR-VGCIA OF GFCTB049.


           EXEC SQL
             SELECT
220808             DINIC_FLEXZ_AGPTO,
220808             DFIM_FLEXZ_AGPTO,
                   QFRANQ_FLEXZ_AGPTO,
                   VNEGOC_FLEXZ_AGPTO,
                   PDESC_FLEXZ_AGPTO,
                   DDIA_AGNDA,
                   QDIA_FLEXZ_AGPTO,
220808             CPER_COBR_TARIF,
220808             CINIC_PER_COBR
             INTO
220808             :GFCTB049.DINIC-FLEXZ-AGPTO,
220808             :GFCTB049.DFIM-FLEXZ-AGPTO,
                   :GFCTB049.QFRANQ-FLEXZ-AGPTO,
                   :GFCTB049.VNEGOC-FLEXZ-AGPTO,
                   :GFCTB049.PDESC-FLEXZ-AGPTO,
                   :GFCTB049.DDIA-AGNDA,
                   :GFCTB049.QDIA-FLEXZ-AGPTO,
220808             :GFCTB049.CPER-COBR-TARIF,
220808             :GFCTB049.CINIC-PER-COBR
             FROM   DB2PRD.FLEXZ_AGPTO_CTA A,
                    DB2PRD.FLEXZ_GRP_CLI   B
             WHERE
                   A.CAGPTO_CTA          =   B.CAGPTO_CTA
              AND  A.CSERVC_TARIF        =   B.CSERVC_TARIF
              AND  A.HIDTFD_FLEXZ_AGPTO  =   B.HIDTFD_FLEXZ_AGPTO
              AND  A.CAGPTO_CTA          =  :GFCTB050.CAGPTO-CTA
              AND  A.CSERVC_TARIF        =  :GFCTB050.CSERVC-TARIF
ST2506*       AND  B.CCGC_CPF            =  :GFCTB050.CCGC-CPF
ST2506*       AND  B.CFLIAL_CGC          =  :GFCTB050.CFLIAL-CGC
ST2506*       AND  B.CCTRL_CPF_CGC       =  :GFCTB050.CCTRL-CPF-CGC
ST2506        AND  B.CCGC_CPF_ST         =  :GFCTB050.CCGC-CPF-ST
ST2506        AND  B.CFLIAL_CGC_ST       =  :GFCTB050.CFLIAL-CGC-ST
ST2506        AND  B.CCTRL_CPF_CGC_ST    =  :GFCTB050.CCTRL-CPF-CGC-ST
              AND  A.DINIC_FLEXZ_AGPTO  <=  :GFCTB049.DINIC-FLEXZ-AGPTO
              AND  A.DFIM_FLEXZ_AGPTO   >=  :GFCTB049.DFIM-FLEXZ-AGPTO
              AND  A.CDEPDC_ENCRR_VGCIA  =  :GFCTB049.CDEPDC-ENCRR-VGCIA
              AND  A.CFUNC_ENCRR_VGCIA   =  :GFCTB049.CFUNC-ENCRR-VGCIA
           END-EXEC.

ATEND      IF (SQLCODE        NOT EQUAL ZEROS AND +100 AND -811) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'FLEXZ_AGPTO_CTA'  TO ERR-DBD-TAB
               MOVE 'SELECT    '       TO ERR-FUN-COMANDO
               MOVE '0020'             TO ERR-LOCAL
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

ATEND      IF (SQLCODE                 EQUAL ZEROS) OR
ATEND         (SQLCODE                 EQUAL  -811)
220808         MOVE DINIC-FLEXZ-AGPTO  OF GFCTB049
220808                                 TO WRK-DATA-DB2
220808         MOVE WRK-ANO-DB2        TO WRK-DINIC-ANO(1)
220808         MOVE WRK-MES-DB2        TO WRK-DINIC-MES(1)
220808         MOVE WRK-DIA-DB2        TO WRK-DINIC-DIA(1)
220808         MOVE DFIM-FLEXZ-AGPTO   OF GFCTB049
220808                                 TO WRK-DATA-DB2
220808         MOVE WRK-ANO-DB2        TO WRK-DFIM-ANO(1)
220808         MOVE WRK-MES-DB2        TO WRK-DFIM-MES(1)
220808         MOVE WRK-DIA-DB2        TO WRK-DFIM-DIA(1)
               MOVE QFRANQ-FLEXZ-AGPTO OF GFCTB049
                                       TO WRK-QFRANQ-FLEXZ-CTA(1)
               MOVE VNEGOC-FLEXZ-AGPTO OF GFCTB049
                                       TO WRK-VNEGOC-FLEXZ-AGPTO(1)
               MOVE PDESC-FLEXZ-AGPTO  OF GFCTB049
                                       TO WRK-PDESC-FLEXZ-CTA(1)
               MOVE DDIA-AGNDA         OF GFCTB049
                                       TO WRK-DDIA-AGNDA(1)
               MOVE QDIA-FLEXZ-AGPTO   OF GFCTB049
                                       TO WRK-QDIA-FLEXZ-CTA(1)
220808         MOVE CPER-COBR-TARIF    OF GFCTB049
220808                                 TO WRK-CPER-COBR(1)
220808         MOVE CINIC-PER-COBR     OF GFCTB049
220808                                 TO WRK-CINIC-PER(1)
               MOVE 'S'                TO WRK-TEM-FLEXZ(1)
           ELSE
               MOVE 'N'                TO WRK-TEM-FLEXZ(1)
           END-IF.

      *----------------------------------------------------------------*
       3110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    AGRUPA AGENCIA.                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       3120-AGRUPAMENTO-AGENCIA        SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTVA-CBCO-DSTNO-MOVTO
                                       TO CEMPR-INC         OF GFCTB051.
           MOVE GFCTVA-CAG-DSTNO-MOVTO TO CDEPDC            OF GFCTB051.
           MOVE GFCTVA-CSERVC-TARIF    TO CSERVC-TARIF      OF GFCTB051.
LFM        MOVE WRK-AGPT-REG(IND-1)    TO CAGPTO-CTA        OF GFCTB051.
           MOVE GFCTVA-DOCOR-EVNTO     TO DINIC-FLEXZ-AGPTO OF GFCTB049
                                          DFIM-FLEXZ-AGPTO  OF GFCTB049.
           MOVE ZEROS                  TO CDEPDC-ENCRR-VGCIA
                                                            OF GFCTB049
                                          CFUNC-ENCRR-VGCIA OF GFCTB049.


           EXEC SQL
             SELECT
220808             DINIC_FLEXZ_AGPTO,
220808             DFIM_FLEXZ_AGPTO,
                   QFRANQ_FLEXZ_AGPTO,
                   VNEGOC_FLEXZ_AGPTO,
                   PDESC_FLEXZ_AGPTO,
                   DDIA_AGNDA,
                   QDIA_FLEXZ_AGPTO,
220808             CPER_COBR_TARIF,
220808             CINIC_PER_COBR
             INTO
220808             :GFCTB049.DINIC-FLEXZ-AGPTO,
220808             :GFCTB049.DFIM-FLEXZ-AGPTO,
                   :GFCTB049.QFRANQ-FLEXZ-AGPTO,
                   :GFCTB049.VNEGOC-FLEXZ-AGPTO,
                   :GFCTB049.PDESC-FLEXZ-AGPTO,
                   :GFCTB049.DDIA-AGNDA,
                   :GFCTB049.QDIA-FLEXZ-AGPTO,
220808             :GFCTB049.CPER-COBR-TARIF,
220808             :GFCTB049.CINIC-PER-COBR
             FROM   DB2PRD.FLEXZ_AGPTO_CTA A,
                    DB2PRD.FLEXZ_GRP_DEPDC B
             WHERE
                   A.CAGPTO_CTA          =   B.CAGPTO_CTA
              AND  A.CSERVC_TARIF        =   B.CSERVC_TARIF
              AND  A.HIDTFD_FLEXZ_AGPTO  =   B.HIDTFD_FLEXZ_AGPTO
              AND  A.CAGPTO_CTA          =  :GFCTB051.CAGPTO-CTA
              AND  A.CSERVC_TARIF        =  :GFCTB051.CSERVC-TARIF
              AND  B.CEMPR_INC           =  :GFCTB051.CEMPR-INC
              AND  B.CDEPDC              =  :GFCTB051.CDEPDC
              AND  A.DINIC_FLEXZ_AGPTO  <=  :GFCTB049.DINIC-FLEXZ-AGPTO
              AND  A.DFIM_FLEXZ_AGPTO   >=  :GFCTB049.DFIM-FLEXZ-AGPTO
              AND  A.CDEPDC_ENCRR_VGCIA  =  :GFCTB049.CDEPDC-ENCRR-VGCIA
              AND  A.CFUNC_ENCRR_VGCIA   =  :GFCTB049.CFUNC-ENCRR-VGCIA
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'FLEXZ_AGPTO_CTA'  TO ERR-DBD-TAB
               MOVE 'SELECT    '       TO ERR-FUN-COMANDO
               MOVE '0030'             TO ERR-LOCAL
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF (SQLCODE                 EQUAL ZEROS)
220808         MOVE DINIC-FLEXZ-AGPTO  OF GFCTB049
220808                                 TO WRK-DATA-DB2
220808         MOVE WRK-ANO-DB2        TO WRK-DINIC-ANO(3)
220808         MOVE WRK-MES-DB2        TO WRK-DINIC-MES(3)
220808         MOVE WRK-DIA-DB2        TO WRK-DINIC-DIA(3)
220808         MOVE DFIM-FLEXZ-AGPTO   OF GFCTB049
220808                                 TO WRK-DATA-DB2
220808         MOVE WRK-ANO-DB2        TO WRK-DFIM-ANO(3)
220808         MOVE WRK-MES-DB2        TO WRK-DFIM-MES(3)
220808         MOVE WRK-DIA-DB2        TO WRK-DFIM-DIA(3)
               MOVE QFRANQ-FLEXZ-AGPTO OF GFCTB049
                                       TO WRK-QFRANQ-FLEXZ-CTA(3)
               MOVE VNEGOC-FLEXZ-AGPTO OF GFCTB049
                                       TO WRK-VNEGOC-FLEXZ-AGPTO(3)
               MOVE PDESC-FLEXZ-AGPTO  OF GFCTB049
                                       TO WRK-PDESC-FLEXZ-CTA(3)
               MOVE DDIA-AGNDA         OF GFCTB049
                                       TO WRK-DDIA-AGNDA(3)
               MOVE QDIA-FLEXZ-AGPTO   OF GFCTB049
                                       TO WRK-QDIA-FLEXZ-CTA(3)
220808         MOVE CPER-COBR-TARIF    OF GFCTB049
220808                                 TO WRK-CPER-COBR(3)
220808         MOVE CINIC-PER-COBR     OF GFCTB049
220808                                 TO WRK-CINIC-PER(3)
               MOVE 'S'                TO WRK-TEM-FLEXZ(3)
           ELSE
               PERFORM 3121-TODAS-AGENCIAS
           END-IF.

      *----------------------------------------------------------------*
       3120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TODAS AGENCIAS.                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       3121-TODAS-AGENCIAS             SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO CDEPDC            OF GFCTB051.
           MOVE GFCTVA-CSERVC-TARIF    TO CSERVC-TARIF      OF GFCTB051.
LFM        MOVE WRK-AGPT-REG (IND-1)   TO CAGPTO-CTA        OF GFCTB051.
           MOVE GFCTVA-DOCOR-EVNTO     TO DINIC-FLEXZ-AGPTO OF GFCTB049
                                          DFIM-FLEXZ-AGPTO  OF GFCTB049.
           MOVE ZEROS                  TO CDEPDC-ENCRR-VGCIA
                                                            OF GFCTB049
                                          CFUNC-ENCRR-VGCIA OF GFCTB049.

           EXEC SQL
             SELECT
220808             DINIC_FLEXZ_AGPTO,
220808             DFIM_FLEXZ_AGPTO,
                   QFRANQ_FLEXZ_AGPTO,
                   VNEGOC_FLEXZ_AGPTO,
                   PDESC_FLEXZ_AGPTO,
                   DDIA_AGNDA,
                   QDIA_FLEXZ_AGPTO,
220808             CPER_COBR_TARIF,
220808             CINIC_PER_COBR
             INTO
220808             :GFCTB049.DINIC-FLEXZ-AGPTO,
220808             :GFCTB049.DFIM-FLEXZ-AGPTO,
                   :GFCTB049.QFRANQ-FLEXZ-AGPTO,
                   :GFCTB049.VNEGOC-FLEXZ-AGPTO,
                   :GFCTB049.PDESC-FLEXZ-AGPTO,
                   :GFCTB049.DDIA-AGNDA,
                   :GFCTB049.QDIA-FLEXZ-AGPTO,
220808             :GFCTB049.CPER-COBR-TARIF,
220808             :GFCTB049.CINIC-PER-COBR
             FROM   DB2PRD.FLEXZ_AGPTO_CTA A,
                    DB2PRD.FLEXZ_GRP_DEPDC B
             WHERE
                   A.CAGPTO_CTA          =   B.CAGPTO_CTA
              AND  A.CSERVC_TARIF        =   B.CSERVC_TARIF
              AND  A.HIDTFD_FLEXZ_AGPTO  =   B.HIDTFD_FLEXZ_AGPTO
              AND  A.CAGPTO_CTA          =  :GFCTB051.CAGPTO-CTA
              AND  A.CSERVC_TARIF        =  :GFCTB051.CSERVC-TARIF
              AND  B.CDEPDC              =  :GFCTB051.CDEPDC
              AND  A.DINIC_FLEXZ_AGPTO  <=  :GFCTB049.DINIC-FLEXZ-AGPTO
              AND  A.DFIM_FLEXZ_AGPTO   >=  :GFCTB049.DFIM-FLEXZ-AGPTO
              AND  A.CDEPDC_ENCRR_VGCIA  =  :GFCTB049.CDEPDC-ENCRR-VGCIA
              AND  A.CFUNC_ENCRR_VGCIA   =  :GFCTB049.CFUNC-ENCRR-VGCIA
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'FLEXZ_AGPTO_CTA'  TO ERR-DBD-TAB
               MOVE 'SELECT    '       TO ERR-FUN-COMANDO
               MOVE '0040'             TO ERR-LOCAL
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF (SQLCODE                 EQUAL ZEROS)
220808         MOVE DINIC-FLEXZ-AGPTO  OF GFCTB049
220808                                 TO WRK-DATA-DB2
220808         MOVE WRK-ANO-DB2        TO WRK-DINIC-ANO(3)
220808         MOVE WRK-MES-DB2        TO WRK-DINIC-MES(3)
220808         MOVE WRK-DIA-DB2        TO WRK-DINIC-DIA(3)
220808         MOVE DFIM-FLEXZ-AGPTO   OF GFCTB049
220808                                 TO WRK-DATA-DB2
220808         MOVE WRK-ANO-DB2        TO WRK-DFIM-ANO(3)
220808         MOVE WRK-MES-DB2        TO WRK-DFIM-MES(3)
220808         MOVE WRK-DIA-DB2        TO WRK-DFIM-DIA(3)
               MOVE QFRANQ-FLEXZ-AGPTO OF GFCTB049
                                       TO WRK-QFRANQ-FLEXZ-CTA(3)
               MOVE VNEGOC-FLEXZ-AGPTO OF GFCTB049
                                       TO WRK-VNEGOC-FLEXZ-AGPTO(3)
               MOVE PDESC-FLEXZ-AGPTO  OF GFCTB049
                                       TO WRK-PDESC-FLEXZ-CTA(3)
               MOVE DDIA-AGNDA         OF GFCTB049
                                       TO WRK-DDIA-AGNDA(3)
               MOVE QDIA-FLEXZ-AGPTO   OF GFCTB049
                                       TO WRK-QDIA-FLEXZ-CTA(3)
220808         MOVE CPER-COBR-TARIF    OF GFCTB049
220808                                 TO WRK-CPER-COBR(3)
220808         MOVE CINIC-PER-COBR     OF GFCTB049
220808                                 TO WRK-CINIC-PER(3)
               MOVE 'S'                TO WRK-TEM-FLEXZ(3)
           ELSE
               MOVE 'N'                TO WRK-TEM-FLEXZ(3)
           END-IF.

      *----------------------------------------------------------------*
       3121-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    AGRUPA SEGMENTO.                                            *
      ******************************************************************
      *----------------------------------------------------------------*
       3130-AGRUPAMENTO-SEGMENTO       SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTVA-CSGMTO-GSTAO-TARIF
                                       TO CSGMTO-GSTAO-TARIF
                                                            OF GFCTB056.
           MOVE GFCTVA-CSERVC-TARIF    TO CSERVC-TARIF      OF GFCTB056.
LFM        MOVE WRK-AGPT-REG(IND-1)    TO CAGPTO-CTA        OF GFCTB056.
           MOVE GFCTVA-DOCOR-EVNTO     TO DINIC-FLEXZ-AGPTO OF GFCTB049
                                          DFIM-FLEXZ-AGPTO  OF GFCTB049.
           MOVE ZEROS                  TO CDEPDC-ENCRR-VGCIA
                                                            OF GFCTB049
                                          CFUNC-ENCRR-VGCIA OF GFCTB049.

           EXEC SQL
             SELECT
220808             DINIC_FLEXZ_AGPTO,
220808             DFIM_FLEXZ_AGPTO,
                   QFRANQ_FLEXZ_AGPTO,
                   VNEGOC_FLEXZ_AGPTO,
                   PDESC_FLEXZ_AGPTO,
                   DDIA_AGNDA,
                   QDIA_FLEXZ_AGPTO,
220808             CPER_COBR_TARIF,
220808             CINIC_PER_COBR
             INTO
220808             :GFCTB049.DINIC-FLEXZ-AGPTO,
220808             :GFCTB049.DFIM-FLEXZ-AGPTO,
                   :GFCTB049.QFRANQ-FLEXZ-AGPTO,
                   :GFCTB049.VNEGOC-FLEXZ-AGPTO,
                   :GFCTB049.PDESC-FLEXZ-AGPTO,
                   :GFCTB049.DDIA-AGNDA,
                   :GFCTB049.QDIA-FLEXZ-AGPTO,
220808             :GFCTB049.CPER-COBR-TARIF,
220808             :GFCTB049.CINIC-PER-COBR
             FROM   DB2PRD.FLEXZ_AGPTO_CTA A,
                    DB2PRD.FLEXZ_GRP_SGMTO B
             WHERE
                   A.CAGPTO_CTA          =   B.CAGPTO_CTA
              AND  A.CSERVC_TARIF        =   B.CSERVC_TARIF
              AND  A.HIDTFD_FLEXZ_AGPTO  =   B.HIDTFD_FLEXZ_AGPTO
              AND  A.CAGPTO_CTA          =  :GFCTB056.CAGPTO-CTA
              AND  A.CSERVC_TARIF        =  :GFCTB056.CSERVC-TARIF
              AND  B.CSGMTO_GSTAO_TARIF  =  :GFCTB056.CSGMTO-GSTAO-TARIF
              AND  A.DINIC_FLEXZ_AGPTO  <=  :GFCTB049.DINIC-FLEXZ-AGPTO
              AND  A.DFIM_FLEXZ_AGPTO   >=  :GFCTB049.DFIM-FLEXZ-AGPTO
              AND  A.CDEPDC_ENCRR_VGCIA  =  :GFCTB049.CDEPDC-ENCRR-VGCIA
              AND  A.CFUNC_ENCRR_VGCIA   =  :GFCTB049.CFUNC-ENCRR-VGCIA
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'FLEXZ_AGPTO_CTA'  TO ERR-DBD-TAB
               MOVE 'SELECT    '       TO ERR-FUN-COMANDO
               MOVE '0050'             TO ERR-LOCAL
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF (SQLCODE                 EQUAL ZEROS)
220808         MOVE DINIC-FLEXZ-AGPTO  OF GFCTB049
220808                                 TO WRK-DATA-DB2
220808         MOVE WRK-ANO-DB2        TO WRK-DINIC-ANO(2)
220808         MOVE WRK-MES-DB2        TO WRK-DINIC-MES(2)
220808         MOVE WRK-DIA-DB2        TO WRK-DINIC-DIA(2)
220808         MOVE DFIM-FLEXZ-AGPTO   OF GFCTB049
220808                                 TO WRK-DATA-DB2
220808         MOVE WRK-ANO-DB2        TO WRK-DFIM-ANO(2)
220808         MOVE WRK-MES-DB2        TO WRK-DFIM-MES(2)
220808         MOVE WRK-DIA-DB2        TO WRK-DFIM-DIA(2)
               MOVE QFRANQ-FLEXZ-AGPTO OF GFCTB049
                                       TO WRK-QFRANQ-FLEXZ-CTA(2)
               MOVE VNEGOC-FLEXZ-AGPTO OF GFCTB049
                                       TO WRK-VNEGOC-FLEXZ-AGPTO(2)
               MOVE PDESC-FLEXZ-AGPTO  OF GFCTB049
                                       TO WRK-PDESC-FLEXZ-CTA(2)
               MOVE DDIA-AGNDA         OF GFCTB049
                                       TO WRK-DDIA-AGNDA(2)
               MOVE QDIA-FLEXZ-AGPTO   OF GFCTB049
                                       TO WRK-QDIA-FLEXZ-CTA(2)
220808         MOVE CPER-COBR-TARIF    OF GFCTB049
220808                                 TO WRK-CPER-COBR(2)
220808         MOVE CINIC-PER-COBR     OF GFCTB049
220808                                 TO WRK-CINIC-PER(2)
               MOVE 'S'                TO WRK-TEM-FLEXZ(2)
           ELSE
               MOVE 'N'                TO WRK-TEM-FLEXZ(2)
           END-IF.

      *----------------------------------------------------------------*
       3130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    AGRUPA PAB.                                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       3140-AGRUPAMENTO-PAB            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTVA-CBCO-DSTNO-MOVTO
                                       TO CEMPR-INC         OF GFCTB054.
           MOVE GFCTVA-CAG-DSTNO-MOVTO TO CDEPDC            OF GFCTB054.
           MOVE GFCTVA-CPAB            TO CPOSTO-SERVC      OF GFCTB054.
           MOVE GFCTVA-CSERVC-TARIF    TO CSERVC-TARIF      OF GFCTB054.
LFM        MOVE WRK-AGPT-REG(IND-1)    TO CAGPTO-CTA        OF GFCTB054.
           MOVE GFCTVA-DOCOR-EVNTO     TO DINIC-FLEXZ-AGPTO OF GFCTB049
                                          DFIM-FLEXZ-AGPTO  OF GFCTB049.
           MOVE ZEROS                  TO CDEPDC-ENCRR-VGCIA
                                                            OF GFCTB049
                                          CFUNC-ENCRR-VGCIA OF GFCTB049.

           EXEC SQL
             SELECT
220808             DINIC_FLEXZ_AGPTO,
220808             DFIM_FLEXZ_AGPTO,
                   QFRANQ_FLEXZ_AGPTO,
                   VNEGOC_FLEXZ_AGPTO,
                   PDESC_FLEXZ_AGPTO,
                   DDIA_AGNDA,
                   QDIA_FLEXZ_AGPTO,
220808             CPER_COBR_TARIF,
220808             CINIC_PER_COBR
             INTO
220808             :GFCTB049.DINIC-FLEXZ-AGPTO,
220808             :GFCTB049.DFIM-FLEXZ-AGPTO,
                   :GFCTB049.QFRANQ-FLEXZ-AGPTO,
                   :GFCTB049.VNEGOC-FLEXZ-AGPTO,
                   :GFCTB049.PDESC-FLEXZ-AGPTO,
                   :GFCTB049.DDIA-AGNDA,
                   :GFCTB049.QDIA-FLEXZ-AGPTO,
220808             :GFCTB049.CPER-COBR-TARIF,
220808             :GFCTB049.CINIC-PER-COBR
             FROM   DB2PRD.FLEXZ_AGPTO_CTA A,
                    DB2PRD.FLEXZ_GRP_PAB   B
             WHERE
                   A.CAGPTO_CTA          =   B.CAGPTO_CTA
              AND  A.CSERVC_TARIF        =   B.CSERVC_TARIF
              AND  A.HIDTFD_FLEXZ_AGPTO  =   B.HIDTFD_FLEXZ_AGPTO
              AND  A.CAGPTO_CTA          =  :GFCTB054.CAGPTO-CTA
              AND  A.CSERVC_TARIF        =  :GFCTB054.CSERVC-TARIF
              AND  B.CEMPR_INC           =  :GFCTB054.CEMPR-INC
              AND  B.CDEPDC              =  :GFCTB054.CDEPDC
              AND  B.CPOSTO_SERVC        =  :GFCTB054.CPOSTO-SERVC
              AND  A.DINIC_FLEXZ_AGPTO  <=  :GFCTB049.DINIC-FLEXZ-AGPTO
              AND  A.DFIM_FLEXZ_AGPTO   >=  :GFCTB049.DFIM-FLEXZ-AGPTO
              AND  A.CDEPDC_ENCRR_VGCIA  =  :GFCTB049.CDEPDC-ENCRR-VGCIA
              AND  A.CFUNC_ENCRR_VGCIA   =  :GFCTB049.CFUNC-ENCRR-VGCIA
IN            FETCH FIRST 1 ROW ONLY
           END-EXEC.

ATEND      IF (SQLCODE      NOT EQUAL ZEROS AND +100 AND -811) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'FLEXZ_AGPTO_CTA'  TO ERR-DBD-TAB
               MOVE 'SELECT    '       TO ERR-FUN-COMANDO
               MOVE '0060'             TO ERR-LOCAL
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF (SQLCODE                 EQUAL ZEROS)
220808         MOVE DINIC-FLEXZ-AGPTO  OF GFCTB049
220808                                 TO WRK-DATA-DB2
220808         MOVE WRK-ANO-DB2        TO WRK-DINIC-ANO(4)
220808         MOVE WRK-MES-DB2        TO WRK-DINIC-MES(4)
220808         MOVE WRK-DIA-DB2        TO WRK-DINIC-DIA(4)
220808         MOVE DFIM-FLEXZ-AGPTO   OF GFCTB049
220808                                 TO WRK-DATA-DB2
220808         MOVE WRK-ANO-DB2        TO WRK-DFIM-ANO(4)
220808         MOVE WRK-MES-DB2        TO WRK-DFIM-MES(4)
220808         MOVE WRK-DIA-DB2        TO WRK-DFIM-DIA(4)
               MOVE QFRANQ-FLEXZ-AGPTO OF GFCTB049
                                       TO WRK-QFRANQ-FLEXZ-CTA(4)
               MOVE VNEGOC-FLEXZ-AGPTO OF GFCTB049
                                       TO WRK-VNEGOC-FLEXZ-AGPTO(4)
               MOVE PDESC-FLEXZ-AGPTO  OF GFCTB049
                                       TO WRK-PDESC-FLEXZ-CTA(4)
               MOVE DDIA-AGNDA         OF GFCTB049
                                       TO WRK-DDIA-AGNDA(4)
               MOVE QDIA-FLEXZ-AGPTO   OF GFCTB049
                                       TO WRK-QDIA-FLEXZ-CTA(4)
220808         MOVE CPER-COBR-TARIF    OF GFCTB049
220808                                 TO WRK-CPER-COBR(4)
220808         MOVE CINIC-PER-COBR     OF GFCTB049
220808                                 TO WRK-CINIC-PER(4)
               MOVE 'S'                TO WRK-TEM-FLEXZ(4)
           ELSE
               MOVE 'N'                TO WRK-TEM-FLEXZ(4)
           END-IF.

      *----------------------------------------------------------------*
       3140-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    AGRUPA POSTAL.                                              *
      ******************************************************************
      *----------------------------------------------------------------*
       3150-AGRUPAMENTO-POSTAL         SECTION.
      *----------------------------------------------------------------*

1208       MOVE 'N'                    TO WRK-TEM-FLEXZ(5)

           MOVE GFCTVA-CBCO-DSTNO-MOVTO
                                       TO CEMPR-INC         OF GFCTB055.
           MOVE GFCTVA-CAG-DSTNO-MOVTO TO CDEPDC            OF GFCTB055.
           MOVE GFCTVA-BCO-POSTAL      TO CPOSTO-SERVC      OF GFCTB055.
           MOVE GFCTVA-CSERVC-TARIF    TO CSERVC-TARIF      OF GFCTB055.
LFM        MOVE WRK-AGPT-REG(IND-1)    TO CAGPTO-CTA        OF GFCTB055.
           MOVE GFCTVA-DOCOR-EVNTO     TO DINIC-FLEXZ-AGPTO OF GFCTB049
                                          DFIM-FLEXZ-AGPTO  OF GFCTB049.
           MOVE ZEROS                  TO CDEPDC-ENCRR-VGCIA
                                                            OF GFCTB049
                                          CFUNC-ENCRR-VGCIA OF GFCTB049.

           EXEC SQL
             SELECT
220808             DINIC_FLEXZ_AGPTO,
220808             DFIM_FLEXZ_AGPTO,
                   QFRANQ_FLEXZ_AGPTO,
                   VNEGOC_FLEXZ_AGPTO,
                   PDESC_FLEXZ_AGPTO,
                   DDIA_AGNDA,
                   QDIA_FLEXZ_AGPTO,
220808             CPER_COBR_TARIF,
220808             CINIC_PER_COBR
             INTO
220808             :GFCTB049.DINIC-FLEXZ-AGPTO,
220808             :GFCTB049.DFIM-FLEXZ-AGPTO,
                   :GFCTB049.QFRANQ-FLEXZ-AGPTO,
                   :GFCTB049.VNEGOC-FLEXZ-AGPTO,
                   :GFCTB049.PDESC-FLEXZ-AGPTO,
                   :GFCTB049.DDIA-AGNDA,
                   :GFCTB049.QDIA-FLEXZ-AGPTO,
220808             :GFCTB049.CPER-COBR-TARIF,
220808             :GFCTB049.CINIC-PER-COBR
             FROM   DB2PRD.FLEXZ_AGPTO_CTA A,
                    DB2PRD.FLEXZ_GRP_PSTAL B
             WHERE
                   A.CAGPTO_CTA          =   B.CAGPTO_CTA
              AND  A.CSERVC_TARIF        =   B.CSERVC_TARIF
              AND  A.HIDTFD_FLEXZ_AGPTO  =   B.HIDTFD_FLEXZ_AGPTO
              AND  A.CAGPTO_CTA          =  :GFCTB055.CAGPTO-CTA
              AND  A.CSERVC_TARIF        =  :GFCTB055.CSERVC-TARIF
              AND  B.CEMPR_INC           =  :GFCTB055.CEMPR-INC
              AND  B.CDEPDC              =  :GFCTB055.CDEPDC
              AND  B.CPOSTO_SERVC        =  :GFCTB055.CPOSTO-SERVC
              AND  A.DINIC_FLEXZ_AGPTO  <=  :GFCTB049.DINIC-FLEXZ-AGPTO
              AND  A.DFIM_FLEXZ_AGPTO   >=  :GFCTB049.DFIM-FLEXZ-AGPTO
              AND  A.CDEPDC_ENCRR_VGCIA  =  :GFCTB049.CDEPDC-ENCRR-VGCIA
              AND  A.CFUNC_ENCRR_VGCIA   =  :GFCTB049.CFUNC-ENCRR-VGCIA
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'FLEXZ_AGPTO_CTA'  TO ERR-DBD-TAB
               MOVE 'SELECT    '       TO ERR-FUN-COMANDO
               MOVE '0070'             TO ERR-LOCAL
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF (SQLCODE                 EQUAL ZEROS)
220808         MOVE DINIC-FLEXZ-AGPTO  OF GFCTB049
220808                                 TO WRK-DATA-DB2
220808         MOVE WRK-ANO-DB2        TO WRK-DINIC-ANO(5)
220808         MOVE WRK-MES-DB2        TO WRK-DINIC-MES(5)
220808         MOVE WRK-DIA-DB2        TO WRK-DINIC-DIA(5)
220808         MOVE DFIM-FLEXZ-AGPTO   OF GFCTB049
220808                                 TO WRK-DATA-DB2
220808         MOVE WRK-ANO-DB2        TO WRK-DFIM-ANO(5)
220808         MOVE WRK-MES-DB2        TO WRK-DFIM-MES(5)
220808         MOVE WRK-DIA-DB2        TO WRK-DFIM-DIA(5)
               MOVE QFRANQ-FLEXZ-AGPTO OF GFCTB049
                                       TO WRK-QFRANQ-FLEXZ-CTA(5)
               MOVE VNEGOC-FLEXZ-AGPTO OF GFCTB049
                                       TO WRK-VNEGOC-FLEXZ-AGPTO(5)
               MOVE PDESC-FLEXZ-AGPTO  OF GFCTB049
                                       TO WRK-PDESC-FLEXZ-CTA(5)
               MOVE DDIA-AGNDA         OF GFCTB049
                                       TO WRK-DDIA-AGNDA(5)
               MOVE QDIA-FLEXZ-AGPTO   OF GFCTB049
                                       TO WRK-QDIA-FLEXZ-CTA(5)
220808         MOVE CPER-COBR-TARIF    OF GFCTB049
220808                                 TO WRK-CPER-COBR(5)
220808         MOVE CINIC-PER-COBR     OF GFCTB049
220808                                 TO WRK-CINIC-PER(5)
               MOVE 'S'                TO WRK-TEM-FLEXZ(5)
           ELSE
                   MOVE 'N'            TO WRK-TEM-FLEXZ(5)
           END-IF.

      *----------------------------------------------------------------*
       3150-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

291108******************************************************************
291108*    AGRUPAMENTO BRADESCO EXPRESSO.                              *
291108******************************************************************
291108*----------------------------------------------------------------*
291108 3155-AGRUPAMENTO-BRAD-EXPRS    SECTION.
291108*----------------------------------------------------------------*
291108
LFM        MOVE 'N'                    TO WRK-TEM-FLEXZ(8)
291108     MOVE GFCTVA-CBCO-DSTNO-MOVTO
291108                                 TO CEMPR-INC         OF GFCTB055.
291108     MOVE GFCTVA-CAG-DSTNO-MOVTO TO CDEPDC            OF GFCTB055.
LFM        MOVE GFCTVA-BCO-POSTAL      TO CPOSTO-SERVC      OF GFCTB055
LFM        MOVE GFCTVA-CSERVC-TARIF    TO CSERVC-TARIF      OF GFCTB055.
LFM        MOVE WRK-AGPT-REG(IND-1)    TO CAGPTO-CTA        OF GFCTB055
291108     MOVE GFCTVA-DOCOR-EVNTO     TO DINIC-FLEXZ-AGPTO OF GFCTB049
291108                                    DFIM-FLEXZ-AGPTO  OF GFCTB049.
291108     MOVE ZEROS                  TO CDEPDC-ENCRR-VGCIA
291108                                                      OF GFCTB049
291108                                    CFUNC-ENCRR-VGCIA OF GFCTB049.
291108
291108     EXEC SQL
291108       SELECT
291108             DINIC_FLEXZ_AGPTO,
291108             DFIM_FLEXZ_AGPTO,
291108             QFRANQ_FLEXZ_AGPTO,
291108             VNEGOC_FLEXZ_AGPTO,
291108             PDESC_FLEXZ_AGPTO,
291108             DDIA_AGNDA,
291108             QDIA_FLEXZ_AGPTO,
291108             CPER_COBR_TARIF,
291108             CINIC_PER_COBR
291108       INTO
291108             :GFCTB049.DINIC-FLEXZ-AGPTO,
291108             :GFCTB049.DFIM-FLEXZ-AGPTO,
291108             :GFCTB049.QFRANQ-FLEXZ-AGPTO,
291108             :GFCTB049.VNEGOC-FLEXZ-AGPTO,
291108             :GFCTB049.PDESC-FLEXZ-AGPTO,
291108             :GFCTB049.DDIA-AGNDA,
291108             :GFCTB049.QDIA-FLEXZ-AGPTO,
291108             :GFCTB049.CPER-COBR-TARIF,
291108             :GFCTB049.CINIC-PER-COBR
291108       FROM   DB2PRD.FLEXZ_AGPTO_CTA A,
291108              DB2PRD.FLEXZ_GRP_PSTAL B
291108       WHERE
291108             A.CAGPTO_CTA          =   B.CAGPTO_CTA
291108        AND  A.CSERVC_TARIF        =   B.CSERVC_TARIF
291108        AND  A.HIDTFD_FLEXZ_AGPTO  =   B.HIDTFD_FLEXZ_AGPTO
291108        AND  A.CAGPTO_CTA          =  :GFCTB055.CAGPTO-CTA
291108        AND  A.CSERVC_TARIF        =  :GFCTB055.CSERVC-TARIF
291108        AND  B.CEMPR_INC           =  :GFCTB055.CEMPR-INC
291108        AND  B.CDEPDC              =  :GFCTB055.CDEPDC
291108        AND  B.CPOSTO_SERVC        =  :GFCTB055.CPOSTO-SERVC
291108        AND  A.DINIC_FLEXZ_AGPTO  <=  :GFCTB049.DINIC-FLEXZ-AGPTO
291108        AND  A.DFIM_FLEXZ_AGPTO   >=  :GFCTB049.DFIM-FLEXZ-AGPTO
291108        AND  A.CDEPDC_ENCRR_VGCIA  =  :GFCTB049.CDEPDC-ENCRR-VGCIA
291108        AND  A.CFUNC_ENCRR_VGCIA   =  :GFCTB049.CFUNC-ENCRR-VGCIA
291108     END-EXEC.
291108
291108     IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
291108        (SQLWARN0                EQUAL 'W')
291108         MOVE 'DB2'              TO ERR-TIPO-ACESSO
291108         MOVE 'FLEXZ_AGPTO_CTA'  TO ERR-DBD-TAB
291108         MOVE 'SELECT    '       TO ERR-FUN-COMANDO
291108         MOVE '0070'             TO ERR-LOCAL
291108         PERFORM 9999-PROCESSAR-ROTINA-ERRO
291108     END-IF.
291108
291108     IF (SQLCODE                 EQUAL ZEROS)
291108         MOVE DINIC-FLEXZ-AGPTO  OF GFCTB049
291108                                 TO WRK-DATA-DB2
LFM            MOVE WRK-ANO-DB2        TO WRK-DINIC-ANO(8)
LFM            MOVE WRK-MES-DB2        TO WRK-DINIC-MES(8)
LFM            MOVE WRK-DIA-DB2        TO WRK-DINIC-DIA(8)
291108         MOVE DFIM-FLEXZ-AGPTO   OF GFCTB049
291108                                 TO WRK-DATA-DB2
LFM            MOVE WRK-ANO-DB2        TO WRK-DFIM-ANO(8)
LFM            MOVE WRK-MES-DB2        TO WRK-DFIM-MES(8)
LFM            MOVE WRK-DIA-DB2        TO WRK-DFIM-DIA(8)
291108         MOVE QFRANQ-FLEXZ-AGPTO OF GFCTB049
LFM                                    TO WRK-QFRANQ-FLEXZ-CTA(8)
291108         MOVE VNEGOC-FLEXZ-AGPTO OF GFCTB049
LFM                                    TO WRK-VNEGOC-FLEXZ-AGPTO(8)
291108         MOVE PDESC-FLEXZ-AGPTO  OF GFCTB049
LFM                                    TO WRK-PDESC-FLEXZ-CTA(8)
291108         MOVE DDIA-AGNDA         OF GFCTB049
LFM                                    TO WRK-DDIA-AGNDA(8)
291108         MOVE QDIA-FLEXZ-AGPTO   OF GFCTB049
LFM                                    TO WRK-QDIA-FLEXZ-CTA(8)
291108         MOVE CPER-COBR-TARIF    OF GFCTB049
LFM                                    TO WRK-CPER-COBR(8)
291108         MOVE CINIC-PER-COBR     OF GFCTB049
LFM                                    TO WRK-CINIC-PER(8)
LFM            MOVE 'S'                TO WRK-TEM-FLEXZ(8)
291108     ELSE
LFM            MOVE 'N'                TO WRK-TEM-FLEXZ(8)
291108     END-IF.
291108
291108*----------------------------------------------------------------*
291108 3155-99-FIM.                    EXIT.
291108*----------------------------------------------------------------*

      ******************************************************************
      *    AGRUPA MUNICIPIO.                                           *
      ******************************************************************
      *----------------------------------------------------------------*
       3160-AGRUPAMENTO-MUNICIPIO      SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTVA-CMUN-RURAL      TO CMUN-IBGE         OF GFCTB053.
           MOVE GFCTVA-CSERVC-TARIF    TO CSERVC-TARIF      OF GFCTB053.
LFM        MOVE WRK-AGPT-REG(IND-1)    TO CAGPTO-CTA        OF GFCTB053.
           MOVE GFCTVA-DOCOR-EVNTO     TO DINIC-FLEXZ-AGPTO OF GFCTB049
                                          DFIM-FLEXZ-AGPTO  OF GFCTB049.
           MOVE ZEROS                  TO CDEPDC-ENCRR-VGCIA
                                                            OF GFCTB049
                                          CFUNC-ENCRR-VGCIA OF GFCTB049.

           EXEC SQL
             SELECT
220808             DINIC_FLEXZ_AGPTO,
220808             DFIM_FLEXZ_AGPTO,
                   QFRANQ_FLEXZ_AGPTO,
                   VNEGOC_FLEXZ_AGPTO,
                   PDESC_FLEXZ_AGPTO,
                   DDIA_AGNDA,
                   QDIA_FLEXZ_AGPTO,
220808             CPER_COBR_TARIF,
220808             CINIC_PER_COBR
             INTO
220808             :GFCTB049.DINIC-FLEXZ-AGPTO,
220808             :GFCTB049.DFIM-FLEXZ-AGPTO,
                   :GFCTB049.QFRANQ-FLEXZ-AGPTO,
                   :GFCTB049.VNEGOC-FLEXZ-AGPTO,
                   :GFCTB049.PDESC-FLEXZ-AGPTO,
                   :GFCTB049.DDIA-AGNDA,
                   :GFCTB049.QDIA-FLEXZ-AGPTO,
220808             :GFCTB049.CPER-COBR-TARIF,
220808             :GFCTB049.CINIC-PER-COBR
             FROM   DB2PRD.FLEXZ_AGPTO_CTA A,
                    DB2PRD.FLEXZ_GRP_MUN   B
             WHERE
                   A.CAGPTO_CTA          =   B.CAGPTO_CTA
              AND  A.CSERVC_TARIF        =   B.CSERVC_TARIF
              AND  A.HIDTFD_FLEXZ_AGPTO  =   B.HIDTFD_FLEXZ_AGPTO
              AND  A.CAGPTO_CTA          =  :GFCTB053.CAGPTO-CTA
              AND  A.CSERVC_TARIF        =  :GFCTB053.CSERVC-TARIF
              AND  B.CMUN_IBGE           =  :GFCTB053.CMUN-IBGE
              AND  A.DINIC_FLEXZ_AGPTO  <=  :GFCTB049.DINIC-FLEXZ-AGPTO
              AND  A.DFIM_FLEXZ_AGPTO   >=  :GFCTB049.DFIM-FLEXZ-AGPTO
              AND  A.CDEPDC_ENCRR_VGCIA  =  :GFCTB049.CDEPDC-ENCRR-VGCIA
              AND  A.CFUNC_ENCRR_VGCIA   =  :GFCTB049.CFUNC-ENCRR-VGCIA
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'FLEXZ_AGPTO_CTA'  TO ERR-DBD-TAB
               MOVE 'SELECT    '       TO ERR-FUN-COMANDO
               MOVE '0080'             TO ERR-LOCAL
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF (SQLCODE                 EQUAL ZEROS)
220808         MOVE DINIC-FLEXZ-AGPTO  OF GFCTB049
220808                                 TO WRK-DATA-DB2
220808         MOVE WRK-ANO-DB2        TO WRK-DINIC-ANO(6)
220808         MOVE WRK-MES-DB2        TO WRK-DINIC-MES(6)
220808         MOVE WRK-DIA-DB2        TO WRK-DINIC-DIA(6)
220808         MOVE DFIM-FLEXZ-AGPTO   OF GFCTB049
220808                                 TO WRK-DATA-DB2
220808         MOVE WRK-ANO-DB2        TO WRK-DFIM-ANO(6)
220808         MOVE WRK-MES-DB2        TO WRK-DFIM-MES(6)
220808         MOVE WRK-DIA-DB2        TO WRK-DFIM-DIA(6)
               MOVE QFRANQ-FLEXZ-AGPTO OF GFCTB049
                                       TO WRK-QFRANQ-FLEXZ-CTA(6)
               MOVE VNEGOC-FLEXZ-AGPTO OF GFCTB049
                                       TO WRK-VNEGOC-FLEXZ-AGPTO(6)
               MOVE PDESC-FLEXZ-AGPTO  OF GFCTB049
                                       TO WRK-PDESC-FLEXZ-CTA(6)
               MOVE DDIA-AGNDA         OF GFCTB049
                                       TO WRK-DDIA-AGNDA(6)
               MOVE QDIA-FLEXZ-AGPTO   OF GFCTB049
                                       TO WRK-QDIA-FLEXZ-CTA(6)
220808         MOVE CPER-COBR-TARIF    OF GFCTB049
220808                                 TO WRK-CPER-COBR(6)
220808         MOVE CINIC-PER-COBR     OF GFCTB049
220808                                 TO WRK-CINIC-PER(6)
               MOVE 'S'                TO WRK-TEM-FLEXZ(6)
           ELSE
               MOVE 'N'                TO WRK-TEM-FLEXZ(6)
           END-IF.

      *----------------------------------------------------------------*
       3160-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    AGRUPA UNIDADE FEDERATIVA.                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       3170-AGRUPAMENTO-UF             SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTVA-CSGL-UF         TO CSGL-UF           OF GFCTB057.
           MOVE GFCTVA-CSERVC-TARIF    TO CSERVC-TARIF      OF GFCTB057.
LFM        MOVE WRK-AGPT-REG(IND-1)    TO CAGPTO-CTA        OF GFCTB057.
           MOVE GFCTVA-DOCOR-EVNTO     TO DINIC-FLEXZ-AGPTO OF GFCTB049
                                          DFIM-FLEXZ-AGPTO  OF GFCTB049.
           MOVE ZEROS                  TO CDEPDC-ENCRR-VGCIA
                                                            OF GFCTB049
                                          CFUNC-ENCRR-VGCIA OF GFCTB049.

           EXEC SQL
             SELECT
220808             DINIC_FLEXZ_AGPTO,
220808             DFIM_FLEXZ_AGPTO,
                   QFRANQ_FLEXZ_AGPTO,
                   VNEGOC_FLEXZ_AGPTO,
                   PDESC_FLEXZ_AGPTO,
                   DDIA_AGNDA,
                   QDIA_FLEXZ_AGPTO,
220808             CPER_COBR_TARIF,
220808             CINIC_PER_COBR
             INTO
220808             :GFCTB049.DINIC-FLEXZ-AGPTO,
220808             :GFCTB049.DFIM-FLEXZ-AGPTO,
                   :GFCTB049.QFRANQ-FLEXZ-AGPTO,
                   :GFCTB049.VNEGOC-FLEXZ-AGPTO,
                   :GFCTB049.PDESC-FLEXZ-AGPTO,
                   :GFCTB049.DDIA-AGNDA,
                   :GFCTB049.QDIA-FLEXZ-AGPTO,
220808             :GFCTB049.CPER-COBR-TARIF,
220808             :GFCTB049.CINIC-PER-COBR
             FROM   DB2PRD.FLEXZ_AGPTO_CTA A,
                    DB2PRD.FLEXZ_GRP_UF    B
             WHERE
                   A.CAGPTO_CTA          =   B.CAGPTO_CTA
              AND  A.CSERVC_TARIF        =   B.CSERVC_TARIF
              AND  A.HIDTFD_FLEXZ_AGPTO  =   B.HIDTFD_FLEXZ_AGPTO
              AND  A.CAGPTO_CTA          =  :GFCTB057.CAGPTO-CTA
              AND  A.CSERVC_TARIF        =  :GFCTB057.CSERVC-TARIF
              AND  B.CSGL_UF             =  :GFCTB057.CSGL-UF
              AND  A.DINIC_FLEXZ_AGPTO  <=  :GFCTB049.DINIC-FLEXZ-AGPTO
              AND  A.DFIM_FLEXZ_AGPTO   >=  :GFCTB049.DFIM-FLEXZ-AGPTO
              AND  A.CDEPDC_ENCRR_VGCIA  =  :GFCTB049.CDEPDC-ENCRR-VGCIA
              AND  A.CFUNC_ENCRR_VGCIA   =  :GFCTB049.CFUNC-ENCRR-VGCIA
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'FLEXZ_AGPTO_CTA'  TO ERR-DBD-TAB
               MOVE 'SELECT    '       TO ERR-FUN-COMANDO
               MOVE '0090'             TO ERR-LOCAL
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF (SQLCODE                 EQUAL ZEROS)
220808         MOVE DINIC-FLEXZ-AGPTO  OF GFCTB049
220808                                 TO WRK-DATA-DB2
220808         MOVE WRK-ANO-DB2        TO WRK-DINIC-ANO(7)
220808         MOVE WRK-MES-DB2        TO WRK-DINIC-MES(7)
220808         MOVE WRK-DIA-DB2        TO WRK-DINIC-DIA(7)
220808         MOVE DFIM-FLEXZ-AGPTO   OF GFCTB049
220808                                 TO WRK-DATA-DB2
220808         MOVE WRK-ANO-DB2        TO WRK-DFIM-ANO(7)
220808         MOVE WRK-MES-DB2        TO WRK-DFIM-MES(7)
220808         MOVE WRK-DIA-DB2        TO WRK-DFIM-DIA(7)
               MOVE QFRANQ-FLEXZ-AGPTO OF GFCTB049
                                       TO WRK-QFRANQ-FLEXZ-CTA(7)
               MOVE VNEGOC-FLEXZ-AGPTO OF GFCTB049
                                       TO WRK-VNEGOC-FLEXZ-AGPTO(7)
               MOVE PDESC-FLEXZ-AGPTO  OF GFCTB049
                                       TO WRK-PDESC-FLEXZ-CTA(7)
               MOVE DDIA-AGNDA         OF GFCTB049
                                       TO WRK-DDIA-AGNDA(7)
               MOVE QDIA-FLEXZ-AGPTO   OF GFCTB049
                                       TO WRK-QDIA-FLEXZ-CTA(7)
220808         MOVE CPER-COBR-TARIF    OF GFCTB049
220808                                 TO WRK-CPER-COBR(7)
220808         MOVE CINIC-PER-COBR     OF GFCTB049
220808                                 TO WRK-CINIC-PER(7)
               MOVE 'S'                TO WRK-TEM-FLEXZ(7)
           ELSE
               MOVE 'N'                TO WRK-TEM-FLEXZ(7)
           END-IF.

      *----------------------------------------------------------------*
       3170-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

LFM   ******************************************************************
LFM   *    AGRUPA RAZAO.                                               *
LFM   ******************************************************************
LFM   *----------------------------------------------------------------*
LFM    3180-AGRUPAMENTO-RAZAO          SECTION.
LFM   *----------------------------------------------------------------*
LFM
LFM        MOVE  237                   TO WRK-CEMPR
LFM        MOVE GFCTVA-GRUPO           TO WRK-CGRP-CTBIL-ATUAL
LFM        MOVE GFCTVA-SUB-GRUPO       TO WRK-CSGRP-CTBIL
LFM        MOVE GFCTVA-CSERVC-TARIF    TO CSERVC-TARIF      OF GFCTB0M8.
LFM        MOVE WRK-AGPT-REG(IND-1)    TO CAGPTO-CTA        OF GFCTB0M8.
LFM        MOVE GFCTVA-DOCOR-EVNTO     TO WRK-GFCTVA-DOCOR-EVNTO.
LFM        MOVE WRK-DIA-EVNTO          TO WRK-DIA-EVNTO-INV.
LFM        MOVE WRK-MES-EVNTO          TO WRK-MES-EVNTO-INV.
LFM        MOVE WRK-ANO-EVNTO          TO WRK-ANO-EVNTO-INV.

           MOVE GFCTVA-DOCOR-EVNTO     TO DINIC-FLEXZ-AGPTO OF GFCTB049
                                          DFIM-FLEXZ-AGPTO  OF GFCTB049.

LFM        MOVE ZEROS                  TO CDEPDC-ENCRR-VGCIA
LFM                                                          OF GFCTB049
LFM                                        CFUNC-ENCRR-VGCIA OF GFCTB049
LFM
LFM        EXEC SQL
LFM           SELECT
LFM               A.DINIC_FLEXZ_AGPTO,
LFM               A.DFIM_FLEXZ_AGPTO,
LFM               A.QFRANQ_FLEXZ_AGPTO,
LFM               A.VNEGOC_FLEXZ_AGPTO,
LFM               A.PDESC_FLEXZ_AGPTO,
LFM               A.DDIA_AGNDA,
LFM               A.QDIA_FLEXZ_AGPTO,
LFM               A.CPER_COBR_TARIF,
LFM               A.CINIC_PER_COBR
LFM         INTO
LFM               :GFCTB049.DINIC-FLEXZ-AGPTO,
LFM               :GFCTB049.DFIM-FLEXZ-AGPTO,
LFM               :GFCTB049.QFRANQ-FLEXZ-AGPTO,
LFM               :GFCTB049.VNEGOC-FLEXZ-AGPTO,
LFM               :GFCTB049.PDESC-FLEXZ-AGPTO,
LFM               :GFCTB049.DDIA-AGNDA,
LFM               :GFCTB049.QDIA-FLEXZ-AGPTO,
LFM               :GFCTB049.CPER-COBR-TARIF,
LFM               :GFCTB049.CINIC-PER-COBR
LFM         FROM   DB2PRD.FLEXZ_AGPTO_CTA A,
LFM                DB2PRD.TFLEXZ_GRP_RZ  B
LFM         WHERE
LFM                 A.CAGPTO_CTA          =  B.CAGPTO_CTA
LFM            AND  A.CSERVC_TARIF        =  B.CSERVC_TARIF
LFM            AND  A.HIDTFD_FLEXZ_AGPTO  =  B.HIDTFD_FLEXZ_AGPTO
LFM            AND  A.CAGPTO_CTA          = :GFCTB0M8.CAGPTO-CTA
LFM            AND  A.CSERVC_TARIF        = :GFCTB0M8.CSERVC-TARIF
LFM            AND  B.CEMPR               = :WRK-CEMPR
LFM            AND  B.CGRP_CTBIL_ATUAL    = :WRK-CGRP-CTBIL-ATUAL
LFM            AND  B.CSGRP_CTBIL         = :WRK-CSGRP-CTBIL
LFM            AND  A.CDEPDC_ENCRR_VGCIA  = :GFCTB049.CDEPDC-ENCRR-VGCIA
LFM            AND  A.CFUNC_ENCRR_VGCIA   = :GFCTB049.CFUNC-ENCRR-VGCIA
               AND  A.DINIC_FLEXZ_AGPTO  <= :GFCTB049.DINIC-FLEXZ-AGPTO
               AND  A.DFIM_FLEXZ_AGPTO   >= :GFCTB049.DFIM-FLEXZ-AGPTO
            FETCH FIRST 1 ROW ONLY
LFM        END-EXEC.
LFM
LFM        IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
LFM           (SQLWARN0                EQUAL 'W')
LFM            MOVE 'DB2'              TO ERR-TIPO-ACESSO
LFM            MOVE 'FLEXZ_AGPTO_CTA'  TO ERR-DBD-TAB
LFM            MOVE 'SELECT    '       TO ERR-FUN-COMANDO
LFM            MOVE '0100'             TO ERR-LOCAL
LFM            PERFORM 9999-PROCESSAR-ROTINA-ERRO
LFM        END-IF.
LFM
LFM        IF (SQLCODE                 EQUAL ZEROS)
LFM
LFM             MOVE DINIC-FLEXZ-AGPTO  OF GFCTB049
LFM                                     TO WRK-DATA-DB2
LFM             MOVE WRK-ANO-DB2        TO WRK-DINIC-ANO(9)
LFM             MOVE WRK-MES-DB2        TO WRK-DINIC-MES(9)
LFM             MOVE WRK-DIA-DB2        TO WRK-DINIC-DIA(9)
LFM             MOVE DFIM-FLEXZ-AGPTO   OF GFCTB049
LFM                                     TO WRK-DATA-DB2
LFM             MOVE WRK-ANO-DB2        TO WRK-DFIM-ANO(9)
LFM             MOVE WRK-MES-DB2        TO WRK-DFIM-MES(9)
LFM             MOVE WRK-DIA-DB2        TO WRK-DFIM-DIA(9)
LFM            IF  WRK-DINIC-AMD(9)     NOT GREATER
LFM                                     WRK-GFCTVA-DOCOR-EVNTO-INV
LFM                AND WRK-DFIM-AMD(9)  NOT LESS
LFM                                     WRK-GFCTVA-DOCOR-EVNTO-INV
LFM
LFM               MOVE QFRANQ-FLEXZ-AGPTO OF GFCTB049
LFM                                       TO WRK-QFRANQ-FLEXZ-CTA(9)
LFM               MOVE VNEGOC-FLEXZ-AGPTO OF GFCTB049
LFM                                       TO WRK-VNEGOC-FLEXZ-AGPTO(9)
LFM               MOVE PDESC-FLEXZ-AGPTO  OF GFCTB049
LFM                                       TO WRK-PDESC-FLEXZ-CTA(9)
LFM
LFM               MOVE DDIA-AGNDA         OF GFCTB049
LFM                                       TO WRK-DDIA-AGNDA(9)
LFM               MOVE QDIA-FLEXZ-AGPTO   OF GFCTB049
LFM                                       TO WRK-QDIA-FLEXZ-CTA(9)
LFM               MOVE CPER-COBR-TARIF    OF GFCTB049
LFM                                       TO WRK-CPER-COBR(9)
LFM               MOVE CINIC-PER-COBR     OF GFCTB049
LFM                                       TO WRK-CINIC-PER(9)
LFM               MOVE 'S'                TO WRK-TEM-FLEXZ(9)
LFM             ELSE
LFM                 MOVE 'N'                TO WRK-TEM-FLEXZ(9)
LFM             END-IF
LFM
LFM         ELSE
LFM            MOVE 'N'                TO WRK-TEM-FLEXZ(9)
LFM         END-IF.
LFM
LFM   *----------------------------------------------------------------*
LFM    3180-99-FIM.                    EXIT.
LFM   *----------------------------------------------------------------*

      ******************************************************************
      *    PROCESSA FLEXIBILIZACAO.                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       3200-PROCESSAR-FLEXIBILIZACAO   SECTION.
      *----------------------------------------------------------------*

           MOVE 'S'                    TO WRK-SEM-FLEXZ.
           MOVE IND-1                  TO IND-2.

LFM        EVALUATE WRK-AGPT-REG(IND-1)

               WHEN 4
                    MOVE 1             TO IND-2
                                          GFCTV4-CIDTFD-DESC

               WHEN 14
                    MOVE 3             TO IND-2
                                          GFCTV4-CIDTFD-DESC

               WHEN 9
                    MOVE 2             TO IND-2
                                          GFCTV4-CIDTFD-DESC

               WHEN 15
                    MOVE 4             TO IND-2
                                          GFCTV4-CIDTFD-DESC

               WHEN 17
                    MOVE 5             TO IND-2
                                          GFCTV4-CIDTFD-DESC

               WHEN 18
                    MOVE 6             TO IND-2
                                          GFCTV4-CIDTFD-DESC

               WHEN 19
                    MOVE 7             TO IND-2
                                          GFCTV4-CIDTFD-DESC
LFM            WHEN 20
LFM                 MOVE 8             TO IND-2
LFM                                       GFCTV4-CIDTFD-DESC
LFM            WHEN 26
LFM                 MOVE 9             TO IND-2
                                          GFCTV4-CIDTFD-DESC

               WHEN OTHER
LFM                 MOVE 10            TO IND-1
LFM                 MOVE 9             TO IND-2
                    MOVE 'N'           TO WRK-TEM-FLEXZ(IND-2)

           END-EVALUATE.

           IF (WRK-TEM-FLEXZ(IND-2)            EQUAL 'S')
               IF (WRK-QFRANQ-FLEXZ-CTA(IND-2) NOT EQUAL ZEROS)
                   PERFORM 3210-ROTINA-FRANQUIA
               ELSE
                   PERFORM 3220-DEMAIS-NEGOCIACOES
               END-IF
               MOVE SPACES                     TO WRK-SEM-FLEXZ
           END-IF.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA FRANQUIA.                                            *
      ******************************************************************
      *----------------------------------------------------------------*
       3210-ROTINA-FRANQUIA            SECTION.
      *----------------------------------------------------------------*

           IF (WRK-CONT-FRANQUIA(IND-2)  LESS
                                         WRK-QFRANQ-FLEXZ-CTA(IND-2))
               MOVE GFCTVA-VTARIF-LIQ-MOVTO
                                         TO WRK-VLR-DESC
               MOVE ZEROS                TO GFCTVA-VTARIF-LIQ-MOVTO
               MOVE 3                    TO GFCTV4-CPARM-DESC
               ADD  1                    TO WRK-CONT-FRANQUIA(IND-2)
               MOVE 'N'                  TO WRK-GRAVAR-EVENTOS
               PERFORM 3211-GRAVAR-DESCONTOS
               PERFORM 3212-GRAVAR-ISENTOS
               MOVE 'S'                    TO WRK-JA-GRAVOU
           ELSE
               PERFORM 3220-DEMAIS-NEGOCIACOES
           END-IF.

      *----------------------------------------------------------------*
       3210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    GRAVA UM REGISTRO NO ARQUIVO DESCTO41.                      *
      ******************************************************************
      *----------------------------------------------------------------*
       3211-GRAVAR-DESCONTOS           SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                SET   :WRK-TIMESTAMP = CURRENT TIMESTAMP
           END-EXEC.

           MOVE GFCTVA-CBCO-DSTNO-MOVTO   TO GFCTV4-CBCO-DSTNO-MOVTO.
           MOVE 2                         TO GFCTV4-CIDTFD-TPO-DESC.
           MOVE 6                         TO GFCTV4-CIDTFD-MOTVO-DESC.
           MOVE GFCTVA-CAG-DSTNO-MOVTO    TO GFCTV4-CAG-DSTNO-MOVTO.
           MOVE GFCTVA-CCTA-DSTNO-MOVTO   TO GFCTV4-CCTA-DSTNO-MOVTO.
           MOVE GFCTVA-CSERVC-TARIF       TO GFCTV4-CSERVC-TARIF.
           MOVE SPACES                    TO GFCTV4-CCONTR-FLEXZ.
           MOVE GFCTVA-CPAB-DSTNO-MOVTO   TO GFCTV4-CPAB-DSTNO-MOVTO.
           MOVE GFCTVA-QEVNTO-AGRUP       TO GFCTV4-QEVNTO-DESC.
           MOVE WRK-DOCOR-EVNTO           TO GFCTV4-DOCOR-EVNTO.
           MOVE GFCTVA-HPREST-SERVC-MOVTO TO GFCTV4-HPREST-MOVTO.
           MOVE WRK-TIMESTAMP             TO GFCTV4-IDTFD-TIMESTAMP.
           MOVE GFCTVA-DENVIO-MOVTO-TARIF TO GFCTV4-DENVIO-MOVTO-TARIF.
RU0125     MOVE GFCTVA-DENVIO-MOVTO-TARIF TO WRK-CPTCAO-GFCTB041.
RU0125     MOVE WRK-CPTCAO-B041           TO GFCTV4-CPTCAO.
           MOVE GFCTVA-CROTNA-ORIGE-MOVTO TO GFCTV4-CROTNA-ORIGE-MOVTO.
           MOVE GFCTVA-CNRO-ARQ-MOVTO     TO GFCTV4-CNRO-ARQ-MOVTO.
           MOVE GFCTVA-CSEQ-MOVTO         TO GFCTV4-CSEQ-MOVTO.
           MOVE WRK-VLR-DESC              TO GFCTV4-VCONCS-DESC.

           WRITE GFCTV4-GFCTB041.

           MOVE  WRK-GRAVACAO          TO WRK-OPERACAO.

           PERFORM 1250-TESTAR-FS-DESCTO41.

           ADD 1                       TO ACU-GRAVS-DESCTO41.

      *----------------------------------------------------------------*
       3211-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    GRAVA UM REGISTRO NO ARQUIVO ISENTO92.                      *
      ******************************************************************
      *----------------------------------------------------------------*
       3212-GRAVAR-ISENTOS             SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTVA-DAGNDA-DEB-MOVTO   TO GFCTVA-DEFETV-DEB-MOVTO.

           MOVE 16                        TO GFCTVA-CSIT-EVNTO-RECBD.

           WRITE REG-ISENTO92             FROM GFCTVA-GFCTB092.

           MOVE  WRK-GRAVACAO             TO WRK-OPERACAO.

           PERFORM 1230-TESTAR-FS-ISENTO92.

           ADD 1                          TO ACU-GRAVS-ISENTO92.

      *----------------------------------------------------------------*
       3212-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    DEMAIS NEGOCIACOES.                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       3220-DEMAIS-NEGOCIACOES         SECTION.
      *----------------------------------------------------------------*

           IF (WRK-DDIA-AGNDA(IND-2)    NOT EQUAL ZEROS)
               PERFORM 3221-DATA-AGENDAMENTO
           END-IF.

           IF  GFCTVA-FLAG-DATA         NOT EQUAL 'S'
               IF (WRK-QDIA-FLEXZ-CTA(IND-2) NOT EQUAL ZEROS)
                   PERFORM 3222-CALCULO-DATA
               END-IF
           END-IF.

220808     IF  GFCTVA-FLAG-DATA            NOT EQUAL 'S'
220808         IF  WRK-CPER-COBR(IND-2)    NOT EQUAL ZEROS
220808             PERFORM 3224-APURAR-DT-AGENDAMENTO
220808             MOVE 'S'                TO GFCTVA-FLAG-DATA
220808             MOVE WRK-DT-APURADA     TO GFCTVA-DAGNDA-DEB-MOVTO
220808         END-IF
220808     END-IF.

           IF (WRK-VNEGOC-FLEXZ-AGPTO(IND-2) NOT EQUAL ZEROS)
               IF (WRK-VNEGOC-FLEXZ-AGPTO(IND-2)
                                          LESS GFCTVA-VTARIF-LIQ-MOVTO)
                   COMPUTE WRK-VLR-DESC = GFCTVA-VTARIF-LIQ-MOVTO -
                                          WRK-VNEGOC-FLEXZ-AGPTO(IND-2)
                   MOVE WRK-VNEGOC-FLEXZ-AGPTO(IND-2)
                                        TO GFCTVA-VTARIF-LIQ-MOVTO
                   MOVE 2               TO GFCTV4-CPARM-DESC
                   IF WRK-VLR-DESC      NOT EQUAL ZEROS
                       PERFORM 3211-GRAVAR-DESCONTOS
                   END-IF
               END-IF
           ELSE
               IF (WRK-PDESC-FLEXZ-CTA(IND-2) NOT EQUAL ZEROS)
                   COMPUTE WRK-VLR-PERC = GFCTVA-VTARIF-LIQ-MOVTO
                                          * WRK-PDESC-FLEXZ-CTA(IND-2)
                                          / 100
                   MOVE WRK-VLR-PERC       TO WRK-VLR-PERC-DESC
                   MOVE WRK-VLR-PERC-DESC  TO WRK-VALOR-PERCENT-TOT
                   MOVE WRK-VLR-PERC-V2    TO WRK-VALOR-PERCENT2
                   COMPUTE WRK-VALOR-LIQUIDO  =
                           GFCTVA-VTARIF-LIQ-MOVTO  - WRK-VALOR-PERCENT2

                   MOVE WRK-VALOR-PERCENT2 TO WRK-VLR-DESC

                   MOVE WRK-VALOR-LIQUIDO TO GFCTVA-VTARIF-LIQ-MOVTO
                   MOVE 1                 TO GFCTV4-CPARM-DESC
                   IF WRK-VLR-DESC        NOT EQUAL ZEROS
                       PERFORM 3211-GRAVAR-DESCONTOS
                   END-IF
               END-IF
           END-IF.

           IF (GFCTVA-VTARIF-LIQ-MOVTO  EQUAL ZEROS)
               MOVE 'N'                 TO WRK-GRAVAR-EVENTOS
               PERFORM 3212-GRAVAR-ISENTOS
               MOVE 'S'                 TO WRK-JA-GRAVOU
           ELSE
               MOVE 'S'                 TO WRK-GRAVAR-EVENTOS
           END-IF.

      *----------------------------------------------------------------*
       3220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    DATA AGENDAMENTO.                                           *
      ******************************************************************
      *----------------------------------------------------------------*
       3221-DATA-AGENDAMENTO           SECTION.
      *----------------------------------------------------------------*

           IF (GFCTVA-FLAG-DATA           NOT EQUAL 'S')
               MOVE 'S'                   TO GFCTVA-FLAG-DATA
               MOVE GFCTVA-DAGNDA-DEB-MOVTO
                                          TO WRK-DAGNDA-DEB-MOVTO
060110         MOVE WRK-DDIA-AGNDA(IND-2) TO WRK-DDIA-AGNDA-SINAL
060110         MOVE WRK-DDIA-AGNDA-SEM    TO WRK-DIA-DAGNDA
               MOVE WRK-DIA-DAGNDA        TO WRK-DIA-AMD1
               MOVE WRK-MES-DAGNDA        TO WRK-MES-AMD1
               MOVE WRK-ANO-DAGNDA        TO WRK-ANO-AMD1
               MOVE DPROCM-ATUAL          OF GFCTB0A1
                                          TO WRK-DATA-DB2
               MOVE WRK-DIA-DB2           TO WRK-DIA-AMD2
               MOVE WRK-MES-DB2           TO WRK-MES-AMD2
               MOVE WRK-ANO-DB2           TO WRK-ANO-AMD2
               IF (WRK-DATA-AMD1          LESS WRK-DATA-AMD2)
                   MOVE DPROCM-PROX       OF GFCTB0A1
                                          TO GFCTVA-DAGNDA-DEB-MOVTO
               ELSE
                   MOVE WRK-DAGNDA-DEB-MOVTO
                                          TO GFCTVA-DAGNDA-DEB-MOVTO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3221-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    SOMA A QUANTIDADE DE DIAS UTEIS EM WRK-DAGNDA-DEB-MOVTO.    *
      ******************************************************************
      *----------------------------------------------------------------*
       3222-CALCULO-DATA               SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTVA-DAGNDA-DEB-MOVTO
                                           TO WRK-DAGNDA-DEB-MOVTO

           MOVE WRK-DIA-DAGNDA             TO WRK-DIA-1285.
           MOVE WRK-MES-DAGNDA             TO WRK-MES-1285.
           MOVE WRK-ANO-DAGNDA             TO WRK-ANO-1285.
           MOVE WRK-QDIA-FLEXZ-CTA(IND-2)  TO WRK-NUMERO-DIAS-1285.
           MOVE WRK-DATA-1285              TO WRK-DATA-ENTRADA-1285.

           CALL  'POOL1285'                USING WRK-AREA-POOL1285.

           IF (RETURN-CODE                 NOT EQUAL ZEROS)
               MOVE 'APL'                  TO ERR-TIPO-ACESSO
               MOVE WRK-ERRO-P1285         TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-DATA-SAIDA-1285        TO WRK-DATA-1285.
           MOVE WRK-DIA-1285               TO WRK-DIA-DAGNDA.
           MOVE WRK-MES-1285               TO WRK-MES-DAGNDA.
           MOVE WRK-ANO-1285               TO WRK-ANO-DAGNDA.
           MOVE WRK-DAGNDA-DEB-MOVTO       TO GFCTVA-DAGNDA-DEB-MOVTO.

      *----------------------------------------------------------------*
       3222-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
220808******************************************************************
220808*    APURA DATA DE AGENDAMENTO                                   *
220808******************************************************************
220808*----------------------------------------------------------------*
220808 3224-APURAR-DT-AGENDAMENTO      SECTION.
220808*----------------------------------------------------------------*
220808
220808     MOVE DPROCM-PROX                OF GFCTB0A1
220808                                     TO WRK-DT-SISTEMA.
220808
220808     MOVE GFCTVA-DOCOR-EVNTO         TO WRK-DT-APURADA.
220808
220808     EVALUATE WRK-CPER-COBR(IND-2)
220808
220808         WHEN  1
220808             MOVE WRK-DT-SISTEMA     TO WRK-DT-APURADA
220808
220808         WHEN  2
220808             MOVE WRK-DIA-DT-APURADA TO WRK-DIA-INV-CBR
220808             MOVE WRK-MES-DT-APURADA TO WRK-MES-INV-CBR
220808             MOVE WRK-ANO-DT-APURADA TO WRK-ANO-INV-CBR
220808             MOVE WRK-CINIC-PER(IND-2)
220808                                     TO IND-3
220808             MOVE 001                TO WRK-INCREMENTO-1640
220808             MOVE 'DC'               TO WRK-TP-INCREM-1640
220808             MOVE SPACES             TO WRK-DIA-SEMANA
220808
220808             PERFORM 3226-BUSCAR-DIA-SEMANA  UNTIL
220808                 WRK-DIA-SEMANA      EQUAL TAB-DIA-SEMANA(IND-3)
220808
220808             IF (WRK-OPCAO           NOT EQUAL 'U')
220808                 MOVE 001            TO WRK-INCREMENTO-1640
220808                 MOVE 'DU'           TO WRK-TP-INCREM-1640
220808                 PERFORM 3225-ACESSAR-POOL1640
220808             END-IF
220808
220808         WHEN  3
220808             IF  WRK-DIA-DT-APURADA  LESS 16
220808                 MOVE 16             TO WRK-DIA-DT-APURADA
220808             ELSE
220808                 MOVE 01             TO WRK-DIA-DT-APURADA
220808                 MOVE 001            TO WRK-INCREMENTO-1640
220808                 MOVE 'M'            TO WRK-TP-INCREM-1640
220808                 PERFORM 3225-ACESSAR-POOL1640
220808             END-IF
220808
220808         WHEN  4
220808             MOVE 01                 TO WRK-DIA-DT-APURADA
220808             MOVE 001                TO WRK-INCREMENTO-1640
220808             MOVE 'M'                TO WRK-TP-INCREM-1640
220808             PERFORM 3225-ACESSAR-POOL1640
220808
220808     END-EVALUATE.
220808
220808*----------------------------------------------------------------*
220808 3224-99-FIM.                    EXIT.
220808*----------------------------------------------------------------*
220808
220808******************************************************************
220808*    SOMA QUANTIDADE DE DIAS CORRIDOS NA DATA APURADA.           *
220808******************************************************************
220808*----------------------------------------------------------------*
220808 3225-ACESSAR-POOL1640           SECTION.
220808*----------------------------------------------------------------*
220808
220808     MOVE WRK-DT-APURADA         TO WRK-DT-ENTRADA-1640.
220808     MOVE 09                     TO WRK-FORMATO-ENT-1640
220808                                    WRK-FORMATO-SAI-1640.
220808
220808     CALL 'POOL1640'             USING WRK-DT-ENTRADA-1640
220808                                       WRK-FORMATO-ENT-1640
220808                                       WRK-TP-INCREM-1640
220808                                       WRK-INCREMENTO-1640
220808                                       WRK-FORMATO-SAI-1640
220808                                       WRK-DT-SAIDA-1640
220808                                       WRK-MENSAGEM-1640.
220808
220808     IF  RETURN-CODE             NOT EQUAL ZEROS
220808         MOVE 'APL'              TO ERR-TIPO-ACESSO
220808         MOVE WRK-MENSAGEM-1640  TO ERR-TEXTO
220808         PERFORM 9999-PROCESSAR-ROTINA-ERRO
220808     END-IF.
220808
220808     MOVE WRK-DT-SAIDA-1640      TO WRK-DT-APURADA.
220808
220808*----------------------------------------------------------------*
220808 3225-99-FIM.                    EXIT.
220808*----------------------------------------------------------------*
220808
220808******************************************************************
220808*    ENCONTRA PROXIMO DIA DA SEMANA DEFINIDO EM CINIC-PER-CALC   *
220808******************************************************************
220808*---------------------------------------------------------------*
220808 3226-BUSCAR-DIA-SEMANA          SECTION.
220808*---------------------------------------------------------------*
220808
220808     PERFORM 3225-ACESSAR-POOL1640.
220808
220808     MOVE WRK-DT-SAIDA-1640      TO WRK-DATA-EDI.
220808     MOVE WRK-DIA-EDI            TO WRK-DIA-INV-CBR
220808     MOVE WRK-MES-EDI            TO WRK-MES-INV-CBR
220808     MOVE WRK-ANO-EDI            TO WRK-ANO-INV-CBR
220808     MOVE WRK-DATA-INV-CBR       TO WRK-DT-ENVIADA.
220808
220808     PERFORM 3227-ACESSAR-POOL1205.
220808
220808     MOVE WRK-DT-EDITADA         TO WRK-DT-APURADA.
220808     MOVE '.'                    TO WRK-PONTO-3
220808                                    WRK-PONTO-4.
220808
220808*---------------------------------------------------------------*
220808 3226-99-FIM.                    EXIT.
220808*---------------------------------------------------------------*
220808
220808******************************************************************
220808*    IDENTIFICA DIA DA SEMANA E INFORMACOES ADICIONAIS           *
220808******************************************************************
220808*---------------------------------------------------------------*
220808 3227-ACESSAR-POOL1205           SECTION.
220808*---------------------------------------------------------------*
220808
220808     MOVE SPACES                 TO WRK-OPCAO.
220808
220808     CALL  'POOL1205'            USING WRK-AREA-POOL1205
220808                                       WRK-MENSAGEM-1205.
220808
220808     IF  RETURN-CODE             NOT EQUAL ZEROS
220808         MOVE 'APL'              TO ERR-TIPO-ACESSO
220808         MOVE WRK-ERRO-P1205     TO ERR-TEXTO
220808         PERFORM 9999-PROCESSAR-ROTINA-ERRO
220808     END-IF.
220808
220808*---------------------------------------------------------------*
220808 3227-99-FIM.                    EXIT.
220808*---------------------------------------------------------------*

      ******************************************************************
      *    GRAVA UM REGISTRO NO ARQUIVO EVENTO92.                      *
      ******************************************************************
      *----------------------------------------------------------------*
       3300-GRAVAR-EVENTOS             SECTION.
      *----------------------------------------------------------------*

           MOVE 02                     TO GFCTVA-CSIT-EVNTO-RECBD.

           WRITE REG-EVENTOAT          FROM GFCTVA-GFCTB092.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 1240-TESTAR-FS-EVENTOAT.

           ADD 1                       TO ACU-GRAVS-EVENTOAT.

      *----------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    FINALIZAR PROGRAMA.                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           IF (ACU-LIDOS-EVENTO92      NOT EQUAL ZEROS)
               PERFORM 4100-EMITIR-DISPLAY
           END-IF.

           CLOSE EVENTO92
                 FRANQUIA
                 ISENTO92
                 EVENTOAT
                 DESCTO41.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 1200-TESTAR-FILE-STATUS.

           STOP RUN.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    EMITIR D I S P L A Y DE LIDOS E GRAVADOS.             *
      ******************************************************************
      *----------------------------------------------------------------*
       4100-EMITIR-DISPLAY            SECTION.
      *----------------------------------------------------------------*

           DISPLAY '****************************************************
      -            '*************'.
           DISPLAY '*
      -            '            *'.
           DISPLAY '*                       PROGRAMA GFCT1884
      -            '            *'.
           DISPLAY '*                       -----------------
      -            '            *'.
           DISPLAY '*
      -            '            *'.
           DISPLAY '*               PROCESSAMENTO EFETUADO COM SUCESSO
      -            '            *'.
           DISPLAY '*
      -            '            *'.
           DISPLAY '****************************************************
      -            '*************'.

           MOVE ACU-LIDOS-EVENTO92     TO WRK-MASCARA.

           DISPLAY '*  TOTAL DE REGISTROS LIDOS    NO EVENTOS     = '
                 WRK-MASCARA '    *'.

           MOVE ACU-GRAVS-EVENTOAT     TO WRK-MASCARA.

           DISPLAY '*  TOTAL DE REGISTROS GRAVADOS EM EVENTOS-ATU = '
                  WRK-MASCARA '    *'.

           MOVE ACU-GRAVS-ISENTO92     TO WRK-MASCARA.

           DISPLAY '*  TOTAL DE REGISTROS GRAVADOS EM ISENTOS     = '
                  WRK-MASCARA '    *'.

           MOVE ACU-GRAVS-DESCTO41     TO WRK-MASCARA.

           DISPLAY '*  TOTAL DE REGISTROS GRAVADOS EM DESCONTOS   = '
                  WRK-MASCARA '    *'.
           DISPLAY '****************************************************
      -            '*************'.

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TRATAMENTO PARA CANCELAMENTO DO PROGRAMA.                   *
      ******************************************************************
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT1884'             TO ERR-PGM.

           IF (ERR-TIPO-ACESSO         EQUAL 'APL')
               CALL 'POOL7100'         USING WRK-BATCH
                                             ERRO-AREA
           ELSE
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE  SPACES            TO ERR-SEGM
               CALL 'POOL7100'         USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA
           END-IF.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
