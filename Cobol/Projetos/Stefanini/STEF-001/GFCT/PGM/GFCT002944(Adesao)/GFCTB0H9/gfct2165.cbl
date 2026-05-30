      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT2165.
       AUTHOR.     DIEGO DE SOUZA.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT2165                                    *
      *    PROGRAMADOR.:   DIEGO DE SOUZA          - CPM PATO BRANCO   *
      *    ANALISTA CPM:   FELIPE ALEX SOMMER      - CPM PATO BRANCO   *
      *    ANALISTA....:   MARCELO CREMM           - PROCWORK /GP.50   *
      *    DATA........:   25/05/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   GERACAO DE MOVIMENTO DE ADESAO COMPULSORIA. *
      *      - SEG, COM BASE NOS REGISTROS  OBTIDOS EM CLIESEG,  SERAO *
      *      GRAVADOS NO  ARQUIVO ADESSEG,  DE ACORDO  COM OS  PACOTES *
      *      SOLICITADOS,  DA  TABELA   DE  SOLICITACOES   DE   ADESAO *
      *      (GFCTB007).                                               *
      *      SERAO GRAVADOS  PARA  CADA OCORRENCIA DO ARQUIVO CLIESEG, *
      *      COM  MESMO  CODIGO  DE SEG  SOLICITADO  NO  CURSOR, TODOS *
      *      PACOTES GRAVADOS NA TABELA 1.                             *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                          INCLUDE/BOOK    *
      *                     CLIESEG                      I#GFCTRY      *
      *                     ADESSEG                      GFCTWAGM      *
      *                     RELADSEG                         -         *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                          INCLUDE/BOOK   *
      *                    DB2PRD.PARM_DATA_PROCM         GFCTB0A1     *
      *                    DB2PRD.ADSAO_COMP_PCOTE        GFCTB001     *
      *                    DB2PRD.ADSAO_GRP_SGMTO         GFCTB007     *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#GFCTRY - ARQ. MOVTO DE CLIENTES.                          *
      *    GFCTWAGM - ARQ. MOVTO DE CLIENTES DOS PACOTES SOLICITADOS.  *
      *    POL7100C - AREA CHAMADA POOL7100 - CANCELAMENTO.            *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    POOL0025 - SOMAR/SUBTRAIR NUMERO DE DIAS DE UMA DATA.       *
      *    POOL1050 - FAZ CONEXAO COM DB2.                             *
      *    POOL7100 - AREA PARA TRATAMENTO DE ERROS.                   *
      *    POOL7600 - OBTEM DATA E HORA DO SISTEMA.                    *
      *================================================================*
      *----------------------------------------------------------------*
      *                                                                *
      *    COMPLEMENTO: MARCELO CREMM ( INFORMACOES COM PWI )          *
      *    OUTUBRO-2006 - INCLUSAO DE DATAS COM VIGENCIA DOS REGISTROS.*
      *                                                                *
      *================================================================*
SV2204*================================================================*
SV2204*      S O N D A  P R O C W O R K  -  C O N S U L T O R I A      *
SV2204*----------------------------------------------------------------*
SV2204*    PROGRAMADOR.:   VINICIUS CRISTIAN MADUREIRA - SONDPROC/GP.50*
SV2204*    ANALISTA....:   MARCUS VINICIUS PINHEIRO    - SONDPROC/GP.50*
SV2204*    DATA........:   22/04/2009                                  *
SV2204*----------------------------------------------------------------*
SV2204*    OBJETIVO....:   OBTER SOMENTE O MES CORRENTE DAS ADESOES.   *
SV2204*----------------------------------------------------------------*
BI0810*    ULTIMA ALTERACAO EM AGO/2010 - BIRA                         *
BI0810*    - GERAR NO ARQUIVO DE SAIDA TIPO DE PESSOA (ADESAO) E       *
BI0810*      SUBSTITUIR O USO DA INC I#GFCTRZ PARA GFCTWAGM.           *
BI0810*                                                                *
BI0810*----------------------------------------------------------------*
BI0412*----------------------------------------------------------------*
BI0412*                 U L T I M A   A L T E R A C A O                *
BI0412*----------------------------------------------------------------*
BI0412*        SONDA PROCWORK - CONSULTORIA - ALTERACAO - BI0412       *
BI0412*----------------------------------------------------------------*
BI0412*                                                                *
BI0412*    ANALISTA....:  UBIRAJARA(BIRA) - SONDA - IT                 *
BI0412*    DATA........:  04 / 2012                                    *
BI0412*    OBJETIVO....:  SETAR FLAG PARA PERMITIR VIZUALIZACAO        *
BI0412*                   DE ADESAO COMPULSORIA, A ADESAO COMPULSORIA  *
BI0412*                   ASSINALADA NO ONLINE SERA INSERIDA NA        *
BI0412*                   BASE GFCTB009 DE ADESAO INDIVIDUAL.          *
BI0412*                                                                *
BI1212*----------------------------------------------------------------*
BI1212*                 U L T I M A   A L T E R A C A O                *
BI1212*----------------------------------------------------------------*
BI1212*        SONDA PROCWORK - CONSULTORIA - ALTERACAO - BI1212       *
BI1212*----------------------------------------------------------------*
BI1212*                                                                *
BI1212*    ANALISTA....:  UBIRAJARA(BIRA) - SONDA - IT                 *
BI1212*    DATA........:  12 / 2012                                    *
BI1212*    OBJETIVO....:  CORRECAO NA PESQUISA DA BASE GFCTB0C6        *
BI1212*                   ALTERACAO NA DATA DE VIGENCIA.               *
BI1212*                                                                *
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

           SELECT CLIESEG   ASSIGN     TO UT-S-CLIESEG
           FILE STATUS                 IS WRK-FS-CLIESEG.

           SELECT ADESSEG   ASSIGN     TO UT-S-ADESSEG
           FILE STATUS                 IS WRK-FS-ADESSEG.

           SELECT RELADSEG  ASSIGN     TO UT-S-RELADSEG
           FILE STATUS                 IS WRK-FS-RELADSEG.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:  ARQUIVO DE CLIENTES - ENTRADA                       *
      *            ORG. SEQUENCIAL     - LRECL   =   050               *
      *----------------------------------------------------------------*

       FD  CLIESEG
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTRY'.

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE CLIENTES ADERIDOS                        *
      *            ORG. SEQUENCIAL     - LRECL   =   050               *
      *----------------------------------------------------------------*

       FD  ADESSEG
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

BI0810 COPY 'GFCTWAGM'.

      *----------------------------------------------------------------*
      *    OUTPUT: RELATORIO ANALITICO E TOTALIZADOR                   *
      *            ORG. SEQUENCIAL     - LRECL   =   133               *
      *----------------------------------------------------------------*

       FD  RELADSEG
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-RELADSEG                 PIC  X(133).

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
           '* AREA PARA ACUMULADORES *'.
      *----------------------------------------------------------------*

       77  ACU-LIDOS-CSR01             PIC  9(012) COMP-3  VALUE ZEROS.
       77  ACU-LIDOS-CLIESEG           PIC  9(012) COMP-3  VALUE ZEROS.
       77  ACU-GRAVADOS-ADESSEG        PIC  9(012) COMP-3  VALUE ZEROS.
       77  ACU-PAGINAS                 PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-LINHAS                  PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-TOTAL-SEGMENTO          PIC  9(012) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           'AREA PARA TESTE DE FILE STATUS'.
      *----------------------------------------------------------------*

       77  WRK-FS-CLIESEG              PIC  X(002)         VALUE SPACES.
       77  WRK-FS-ADESSEG              PIC  X(002)         VALUE SPACES.
       77  WRK-FS-RELADSEG             PIC  X(002)         VALUE SPACES.

       77  WRK-ABERTURA                PIC  X(013)         VALUE
           ' NA ABERTURA '.
       77  WRK-LEITURA                 PIC  X(013)         VALUE
           ' NA  LEITURA '.
       77  WRK-GRAVACAO                PIC  X(013)         VALUE
           ' NA GRAVACAO '.
       77  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA VARIAVEIS AUXILIARES *'.
      *----------------------------------------------------------------*

       77  WRK-SEGMENTO-ANT            PIC  9(007)         VALUE ZEROS.
       77  WRK-INICIO                  PIC  X(010)         VALUE SPACES.
       77  WRK-FIM                     PIC  X(010)         VALUE SPACES.
       77  WRK-FIM-CSR01               PIC  X(001)         VALUE 'N'.
       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.

       01  WRK-DATA                    PIC  X(010)         VALUE SPACES.
       01  WRK-DATA-R                  REDEFINES WRK-DATA.
           05  WRK-DATA-DIA            PIC  9(002).
           05  FILLER                  PIC  X(001).
           05  WRK-DATA-MES            PIC  9(002).
           05  FILLER                  PIC  X(001).
           05  WRK-DATA-ANO            PIC  9(004).

       01  WRK-DATA-AXX                PIC  9(009)         VALUE ZEROS.
       01  WRK-DATA-AXX-R              REDEFINES WRK-DATA-AXX.
           05  FILLER                  PIC  9(001).
           05  WRK-DIA-AXX             PIC  9(002).
           05  WRK-MES-AXX             PIC  9(002).
           05  WRK-ANO-AXX             PIC  9(004).

SV2204 01  WRK-DATA-AUX0.
SV2204     05 WRK-DIA-AUX0             PIC  9(002) VALUE ZEROS.
SV2204     05 WRK-P1                   PIC  X(001) VALUE SPACES.
SV2204     05 WRK-MES-AUX0             PIC  9(002) VALUE ZEROS.
SV2204     05 WRK-P2                   PIC  X(001) VALUE SPACES.
SV2204     05 WRK-ANO-AUX0             PIC  9(004) VALUE ZEROS.

       01  WRK-DATA-AUX1-R             PIC 9(009) VALUE ZEROS.
       01  WRK-DATA-AUX1 REDEFINES WRK-DATA-AUX1-R.
           03  FILLER                  PIC  9(001).
           03  WRK-ANOMES-AUX1.
               05  WRK-ANO-AUX1        PIC  9(004).
               05  WRK-MES-AUX1        PIC  9(002).
           03  WRK-DIA-AUX1            PIC  9(002).

       01  WRK-DATA-NUM                PIC  9(008)         VALUE ZEROS.
       01  WRK-ANOMES-AUX              PIC  X(006)         VALUE SPACES.

       01  WRK-MSG01.
           05  FILLER                  PIC  X(020)         VALUE
               'DATA ATUAL INVALIDA'.

       01  WRK-MSG02.
           05  FILLER                  PIC  X(021)         VALUE
               'ARQUIVO CLIESEG VAZIO'.

       01  WRK-MSG03.
           05  FILLER                  PIC  X(028)         VALUE
               'ESTOURO NA TABELA INTERNA 1'.

SV2204 01  WRK-DATA-DB2.
SV2204     05  WRK-DIA-DB2             PIC  9(002)         VALUE ZEROS.
SV2204     05  FILLER                  PIC  X(001)         VALUE '.'.
SV2204     05  WRK-MES-DB2             PIC  9(002)         VALUE ZEROS.
SV2204     05  FILLER                  PIC  X(001)         VALUE '.'.
SV2204     05  WRK-ANO-DB2             PIC  9(004)         VALUE ZEROS.

SV2204 01  WRK-DATA-AUX                PIC  9(009)         VALUE ZEROS.
SV2204 01  FILLER                      REDEFINES WRK-DATA-AUX.
SV2204     05  FILLER                  PIC  9(001).
SV2204     05  WRK-DIA-AUX             PIC  9(002).
SV2204     05  WRK-MES-AUX             PIC  9(002).
SV2204     05  WRK-ANO-AUX             PIC  9(004).

SV2204 01  WRK-MES-AUX-COMP            PIC  9(002) COMP-3 VALUE ZEROS.
SV2204 01  WRK-ANO-AUX-COMP            PIC  9(004) COMP-3 VALUE ZEROS.

SV2204 01  WRK-DATA-INI-PERIODO-ATU.
SV2204     03  WRK-DIA-INI-PERIODO-ATU PIC  9(002)         VALUE ZEROS.
SV2204     03  FILLER                  PIC  X(001)         VALUE '.'.
SV2204     03  WRK-MES-INI-PERIODO-ATU PIC  9(002)         VALUE ZEROS.
SV2204     03  FILLER                  PIC  X(001)         VALUE '.'.
SV2204     03  WRK-ANO-INI-PERIODO-ATU PIC  9(004)         VALUE ZEROS.

SV2204 01  WRK-DATA-FIM-PERIODO-ATU.
SV2204     03  WRK-DIA-FIM-PERIODO-ATU PIC  9(002)         VALUE ZEROS.
SV2204     03  FILLER                  PIC  X(001)         VALUE '.'.
SV2204     03  WRK-MES-FIM-PERIODO-ATU PIC  9(002)         VALUE ZEROS.
SV2204     03  FILLER                  PIC  X(001)         VALUE '.'.
SV2204     03  WRK-ANO-FIM-PERIODO-ATU PIC  9(004)         VALUE ZEROS.

BI0412 01  WRK-AUX-B0C6.
BI0412     05 WRK-B0C6-PACOTE          PIC S9(005) COMP-3  VALUE ZEROS.
BI0412     05 FILLER   REDEFINES  WRK-B0C6-PACOTE.
BI0412        06 WRK-B0C6-PACOTE-SS    PIC  9(005) COMP-3.
BI0412
BI0412     05 WRK-B0C6-AGPTO           PIC S9(003) COMP-3  VALUE ZEROS.
BI0412     05 FILLER   REDEFINES  WRK-B0C6-AGPTO.
BI0412        06 WRK-B0C6-AGPTO-SS     PIC  9(003) COMP-3.
BI0412
BI0412     05 WRK-B0C6-SEQ             PIC S9(009) COMP-3  VALUE ZEROS.
BI0412     05 FILLER   REDEFINES  WRK-B0C6-SEQ.
BI0412        06 WRK-B0C6-SEQ-SS       PIC  9(009) COMP-3.
BI0412
BI0412     05 WRK-B0C6-DINI            PIC  X(010)         VALUE SPACES.

BI0412 01  WRK-VISUALIZA               PIC  X(001)         VALUE 'N'.

BI0412 01  WRK-CINDCD-ADSAO-INDVD      PIC  X(001)         VALUE 'C'.
BI0412 01  WRK-CINDCD-ADSAO-AMBAS      PIC  X(001)         VALUE 'A'.

SV2204*----------------------------------------------------------------*
SV2204 01  FILLER                      PIC  X(032)         VALUE
SV2204     '*        AREA POOL0025         *'.
SV2204*----------------------------------------------------------------*
SV2204
SV2204 01  WRK-0025-DATA-ENTRADA       PIC  9(009) COMP-3  VALUE ZEROS.
SV2204 01  WRK-0025-DATA-SAIDA         PIC  9(009) COMP-3  VALUE ZEROS.
SV2204 01  WRK-0025-NUMERO-DIAS        PIC S9(005) COMP-3  VALUE ZEROS.
SV2204
SV2204 01  WRK-ERRO-POOL0025.
SV2204     05  FILLER                  PIC  X(040)         VALUE
SV2204         'ERRO MODULO POOL0025 - RETURN CODE = '.
SV2204     05  WRK-ERRO-0025-RET-COD   PIC  X(004)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ERRO - FILE-STATUS ***'.
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
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA TABELA INTERNA *'.
      *----------------------------------------------------------------*

       01  TAB-1-SEGMENTO.
           05  TAB-1-SEGMENTO-N        PIC  9(007)         VALUE ZEROS.
           05  TAB-1-DADOS             OCCURS 300 TIMES.
             10  TAB-1-PACOTE          PIC  9(005)         VALUE ZEROS.
             10  TAB-1-AGPTO           PIC  9(003)         VALUE ZEROS.
             10  TAB-1-SEQ             PIC  9(009)         VALUE ZEROS.
             10  TAB-1-DINI            PIC  X(010)         VALUE SPACES.
BI0810       10  TAB-1-TPSSOA-ADESAO   PIC  X(001)         VALUE SPACES.


      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA RELATORIO *'.
      *----------------------------------------------------------------*

       01  WRK-GFCTB007-NULL.
           05 WRK-CSGMTO-GSTAO-TARIF-NULL PIC S9(004) COMP VALUE +0.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA RELATORIO *'.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA CABECARIO *'.
      *----------------------------------------------------------------*

       01  CABEC1-REL1.
           05  FILLER                  PIC  X(001)         VALUE '1'.
           05  CB1-REL1-DIA            PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '/'.
           05  CB1-REL1-MES            PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '/'.
           05  CB1-REL1-ANO            PIC  9(004)         VALUE ZEROS.
           05  FILLER                  PIC  X(039)         VALUE SPACES.
           05  FILLER                  PIC  X(011)         VALUE
               'B A N C O'.
           05  FILLER                  PIC  X(017)         VALUE
               'B R A D E S C O'.
           05  FILLER                  PIC  X(042)         VALUE 'S/A'.
           05  CB1-REL1-HORA           PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE ':'.
           05  CB1-REL1-MIN            PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE ':'.
           05  CB1-REL1-SEG            PIC  9(002)         VALUE ZEROS.

       01  CABEC2-REL1.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  FILLER                  PIC  X(037)         VALUE
               'GFCT2165'.
           05  FILLER                  PIC  X(045)         VALUE
               'GFCT - GESTAO, FLEXIBILIZACAO E COBRANCA DE'.
           05  FILLER                  PIC  X(037)         VALUE
               'TARIFAS'.
           05  FILLER                  PIC  X(004)         VALUE 'PAG.'.
           05  CB2-REL1-PAG            PIC  ZZ99           VALUE ZEROS.

       01  CABEC3-REL1.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  FILLER                  PIC  X(034)         VALUE SPACES.
           05  FILLER                  PIC  X(054)         VALUE
               'RELATORIO DE CLIENTES PARTICIPANTES DE ADESOES POR SEG'.
           05  FILLER                  PIC  X(005)         VALUE
               'MENTO'.

       01  CABEC4-REL1.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  FILLER                  PIC  X(131)         VALUE
               ALL '-'.

       01  CABEC5-REL1.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  FILLER                  PIC  X(049)         VALUE SPACES.
           05  FILLER                  PIC  X(013)         VALUE
               'MOVIMENTO DE '.
           05  CB5-REL1-DIA-MOV        PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '/'.
           05  CB5-REL1-MES-MOV        PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '/'.
           05  CB5-REL1-ANO-MOV        PIC  9(004)         VALUE ZEROS.

       01  CABEC6-REL1.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  FILLER                  PIC  X(005)         VALUE SPACES.
           05  FILLER                  PIC  X(015)         VALUE
               'AGENCIA'.
           05  FILLER                  PIC  X(011)         VALUE
               'CONTA'.
           05  FILLER                  PIC  X(012)         VALUE
               'PACOTE'.
           05  FILLER                  PIC  X(017)         VALUE
               'AGRUPAMENTO'.
           05  FILLER                  PIC  X(015)         VALUE
               'SEQUENCIA'.
           05  FILLER                  PIC  X(014)         VALUE
               'DATA DE INICIO'.

       01  CABEC7-REL1.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  FILLER                  PIC  X(031)         VALUE SPACES.
           05  FILLER                  PIC  X(054)         VALUE
               '----------------------- TOTALIZADOR FINAL ------------'.
           05  FILLER                  PIC  X(010)         VALUE
               '----------'.

       01  CABEC8-REL1-BRANCO.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  FILLER                  PIC  X(130)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA DETALHE *'.
      *---------------------------------------------------------------*

       01  LINDET1-REL1.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  FILLER                  PIC  X(005)         VALUE SPACES.
           05  LD1-REL1-AGENCIA        PIC  9(005)         VALUE ZEROS.
           05  FILLER                  PIC  X(007)         VALUE SPACES.
           05  LD1-REL1-CONTA          PIC  9(007)         VALUE ZEROS.
           05  FILLER                  PIC  X(007)         VALUE SPACES.
           05  LD1-REL1-PACOTE         PIC  9(005)         VALUE ZEROS.
           05  FILLER                  PIC  X(009)         VALUE SPACES.
           05  LD1-REL1-AGRUPAMENTO    PIC  9(003)         VALUE ZEROS.
           05  FILLER                  PIC  X(011)         VALUE SPACES.
           05  LD1-REL1-SEQUENCIA      PIC  9(009)         VALUE ZEROS.
           05  FILLER                  PIC  X(008)         VALUE SPACES.
           05  LD1-REL1-DIA-INI        PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '/'.
           05  LD1-REL1-MES-INI        PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '/'.
           05  LD1-REL1-ANO-INI        PIC  9(004)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA TOTAL *'.
      *----------------------------------------------------------------*

       01  LINTOT1-REL1.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  FILLER                  PIC  X(041)         VALUE
               'QUANTIDADE TOTAL DE CLIENTES DO SEGMENTO '.
           05  LT1-REL1-SEGMENTO       PIC  ZZZZ9          VALUE ZEROS.
           05  FILLER                  PIC  X(003)         VALUE ' : '.
           05  LT1-REL1-TOT-CLIENTES   PIC  ZZZ.ZZZ.ZZZ.ZZ9 VALUE ZEROS.

       01  LINTOT2-REL1.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  FILLER                  PIC  X(031)         VALUE SPACES.
           05  FILLER                  PIC  X(052)         VALUE
               '1- QUANTIDADE DE CLIENTES LIDOS EM CLIESEG........: '.
           05  LT2-REL1-TOT-LIDOS      PIC  ZZZ.ZZZ.ZZZ.ZZ9 VALUE ZEROS.

       01  LINTOT3-REL1.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  FILLER                  PIC  X(031)         VALUE SPACES.
           05  FILLER                  PIC  X(052)         VALUE
               '2- QUANTIDADE DE SOLICITACOES DE ADESAO...........: '.
           05  LT3-REL1-TOT-SOLIC      PIC  ZZZ.ZZZ.ZZZ.ZZ9 VALUE ZEROS.

       01  LINTOT4-REL1.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  FILLER                  PIC  X(031)         VALUE SPACES.
           05  FILLER                  PIC  X(052)         VALUE
               '3- QUANTIDADE DE CLIENTES GRAVADOS EM ADESSEG.....: '.
           05  LT4-REL1-TOT-GRAVA      PIC  ZZZ.ZZZ.ZZZ.ZZ9 VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA POOL7100 *'.
      *----------------------------------------------------------------*

       COPY POL7100C.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DA POOL7600 *'.
      *----------------------------------------------------------------*

       01  WRK-DATA-HORA.
           05  WRK-DATA-JULIANA        PIC  9(005) COMP-3  VALUE ZEROS.
           05  WRK-DATA-AAMMDD         PIC  9(007) COMP-3  VALUE ZEROS.
           05  WRK-DATA-AAAAMMDD       PIC  9(009) COMP-3  VALUE ZEROS.
           05  WRK-HORA-HHMMSS         PIC  9(007) COMP-3  VALUE ZEROS.
           05  WRK-HORA-HHMMSSMMMMMM   PIC  9(013) COMP-3  VALUE ZEROS.
           05  WRK-TIMESTAMP.
             10  WRK-ANO               PIC  9(004)         VALUE ZEROS.
             10  WRK-MES               PIC  9(002)         VALUE ZEROS.
             10  WRK-DIA               PIC  9(002)         VALUE ZEROS.
             10  WRK-HOR               PIC  9(002)         VALUE ZEROS.
             10  WRK-MIN               PIC  9(002)         VALUE ZEROS.
             10  WRK-SEG               PIC  9(002)         VALUE ZEROS.
             10  FILLER                PIC  X(006)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA TABELA DB2 *'.
      *---------------------------------------------------------------*

           EXEC SQL
             INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0A1
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB001
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB007
           END-EXEC.

BI0412     EXEC SQL
BI0412       INCLUDE GFCTB0C6
BI0412     END-EXEC.

           EXEC SQL
             DECLARE CSR01-GFCTB007 CURSOR FOR
             SELECT
                   A.CSERVC_TARIF,
                   A.CAGPTO_CTA,
                   A.CSEQ_AGPTO_CTA,
                   A.DINIC_ADSAO_COMP,
                   A.CSGMTO_GSTAO_TARIF,
                   B.DFIM_ADSAO_COMP,
BI0810             B.CPSSOA_SERVC_TARIF
             FROM  DB2PRD.ADSAO_GRP_SGMTO   A,
                   DB2PRD.ADSAO_COMP_PCOTE  B
             WHERE A.CSERVC_TARIF        =  B.CSERVC_TARIF
             AND   A.CAGPTO_CTA          =  B.CAGPTO_CTA
             AND   A.CSEQ_AGPTO_CTA      =  B.CSEQ_AGPTO_CTA
             AND   A.DINIC_ADSAO_COMP    =  B.DINIC_ADSAO_COMP
             AND   B.DINIC_ADSAO_COMP   <=  :GFCTB001.DINIC-ADSAO-COMP
             AND   B.DFIM_ADSAO_COMP    >=  :GFCTB001.DFIM-ADSAO-COMP
             ORDER BY
                   A.CSGMTO_GSTAO_TARIF,
                   A.CSERVC_TARIF,
BI0412             A.CAGPTO_CTA,
BI0412             A.CSEQ_AGPTO_CTA,
BI0412             A.DINIC_ADSAO_COMP
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* FIM DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      ******************************************************************
      *    CHAMA AS ROTINAS PRINCIPAIS DO PROCESSAMENTO.               *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-VERIFICAR-VAZIO.

           PERFORM 3000-PROCESSAR.

           PERFORM 4000-FINALIZAR.

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

           OPEN INPUT  CLIESEG
                OUTPUT ADESSEG
                       RELADSEG.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.
           PERFORM 1100-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       1000-99-FIM.               EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA QUE EFETUA TESTE DE FILE-STATUS.                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-CLIESEG.

           PERFORM 1120-TESTAR-FS-ADESSEG.

           PERFORM 1130-TESTAR-FS-RELADSEG.

      *----------------------------------------------------------------*
       1100-99-FIM.               EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE TESTE PARA ARQUIVO CLIESEG.                       *
      ******************************************************************
      *----------------------------------------------------------------*
       1110-TESTAR-FS-CLIESEG          SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-CLIESEG          NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'CLIESEG'          TO WRK-NOME-ARQ
               MOVE WRK-FS-CLIESEG     TO WRK-FILE-STATUS
               MOVE WRK-ERRO-POOL7100  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE TESTE PARA ARQUIVO ADESSEG.                       *
      ******************************************************************
      *----------------------------------------------------------------*
       1120-TESTAR-FS-ADESSEG          SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ADESSEG          NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'ADESSEG'          TO WRK-NOME-ARQ
               MOVE WRK-FS-ADESSEG     TO WRK-FILE-STATUS
               MOVE WRK-ERRO-POOL7100  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE TESTE PARA ARQUIVO RELADSEG.                      *
      ******************************************************************
      *----------------------------------------------------------------*
       1130-TESTAR-FS-RELADSEG         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-RELADSEG         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'RELADSEG'         TO WRK-NOME-ARQ
               MOVE WRK-FS-RELADSEG    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-POOL7100  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE VERIFICACAO DE ARQUIVO VAZIO.                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-OBTER-PARAMETROS.

           PERFORM 2200-LER-CLIESEG

           IF  WRK-FS-CLIESEG          EQUAL '10' AND
               ACU-LIDOS-CLIESEG       EQUAL ZEROS
               DISPLAY '************* GFCT2165 *************'
               DISPLAY '*                                  *'
               DISPLAY '*      ARQUIVO CLIESEG  VAZIO      *'
               DISPLAY '*                                  *'
               DISPLAY '*     PROCESSAMENTO  ENCERRADO     *'
               DISPLAY '*                                  *'
               DISPLAY '************* GFCT2165 *************'
               MOVE WRK-MSG02          TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           CALL 'POOL7600'             USING WRK-DATA-HORA.

           MOVE WRK-DIA                TO CB1-REL1-DIA.
           MOVE WRK-MES                TO CB1-REL1-MES.
           MOVE WRK-ANO                TO CB1-REL1-ANO.
           MOVE WRK-HOR                TO CB1-REL1-HORA.
           MOVE WRK-MIN                TO CB1-REL1-MIN.
           MOVE WRK-SEG                TO CB1-REL1-SEG.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA QUE SELECIONA REGISTROS NA TABELA GFCTB0A1.          *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-OBTER-PARAMETROS           SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC OF GFCTB0A1.

           EXEC SQL
             SELECT
                   DPROCM_ANTER,
                   DPROCM_ATUAL
             INTO
                   :GFCTB0A1.DPROCM-ANTER,
                   :GFCTB0A1.DPROCM-ATUAL
             FROM   DB2PRD.PARM_DATA_PROCM
             WHERE
                   CSIST_PRINC         = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PARM_DATA_PROCM'  TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0010'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE DPROCM-ATUAL OF GFCTB0A1 TO WRK-DATA.

           MOVE WRK-DATA-DIA           TO CB5-REL1-DIA-MOV.
           MOVE WRK-DATA-MES           TO CB5-REL1-MES-MOV.
           MOVE WRK-DATA-ANO           TO CB5-REL1-ANO-MOV.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA QUE EFETUA LEITURA DO ARQUIVO DE ENTRADA CLIESEG.    *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-LER-CLIESEG                SECTION.
      *----------------------------------------------------------------*

           READ CLIESEG.

           IF  WRK-FS-CLIESEG          EQUAL '10'
               MOVE 999             TO GFCTRY-SEGTO-GFCT
               GO TO 2200-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.
           PERFORM 1110-TESTAR-FS-CLIESEG.

           ADD 1                       TO ACU-LIDOS-CLIESEG.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCESSAMENTO PRINCIPAL                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 3100-OBTER-DATAS.

           PERFORM 3200-ABRIR-CSR01-GFCTB007.

           PERFORM 3300-LER-CSR01-GFCTB007.

           IF  WRK-FIM-CSR01           EQUAL 'S'
               PERFORM 3600-EMITIR-DISPLAY
           END-IF.

           PERFORM 3400-PROCESSAR-CSR01-GFCTB007 UNTIL
               WRK-FIM-CSR01           EQUAL 'S' AND
               WRK-FS-CLIESEG          EQUAL '10'.

           PERFORM 3500-CLOSE-CSR01-GFCTB007.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA PARA OBTER DADAS.                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       3100-OBTER-DATAS                SECTION.
      *----------------------------------------------------------------*

SV2204*=== MONTAGEM DO PERIODO ATUAL (MES ATUAL)
SV2204
SV2204*--- DATA INICIO DO PERIODO ATUAL (MES ATUAL)
SV2204
SV2204     MOVE DPROCM-ATUAL OF GFCTB0A1
SV2204                                 TO WRK-DATA-DB2.
SV2204
SV2204     MOVE 01                     TO WRK-DIA-DB2
SV2204
SV2204     MOVE WRK-DATA-DB2           TO WRK-DATA-INI-PERIODO-ATU.
SV2204
SV2204*--- DATA FIM DO PERIODO ATUAL (MES ATUAL)
SV2204
SV2204     MOVE WRK-DIA-DB2            TO WRK-DIA-AUX.
SV2204     MOVE WRK-MES-DB2            TO WRK-MES-AUX-COMP.
SV2204     MOVE WRK-ANO-DB2            TO WRK-ANO-AUX-COMP.
SV2204
SV2204     IF  WRK-MES-AUX-COMP        EQUAL 12
SV2204         MOVE 01                 TO WRK-MES-AUX-COMP
SV2204         ADD  1                  TO WRK-ANO-AUX-COMP
SV2204     ELSE
SV2204         ADD  1                  TO WRK-MES-AUX-COMP
SV2204     END-IF.
SV2204
SV2204     MOVE WRK-MES-AUX-COMP       TO WRK-MES-AUX.
SV2204     MOVE WRK-ANO-AUX-COMP       TO WRK-ANO-AUX.
SV2204
SV2204     MOVE WRK-DATA-AUX           TO WRK-0025-DATA-ENTRADA.
SV2204     MOVE -1                     TO WRK-0025-NUMERO-DIAS.
SV2204
SV2204     CALL 'POOL0025'             USING WRK-0025-DATA-ENTRADA
SV2204                                       WRK-0025-NUMERO-DIAS
SV2204                                       WRK-0025-DATA-SAIDA.
SV2204
SV2204     IF  RETURN-CODE             NOT EQUAL ZEROS
SV2204         MOVE 'APL'              TO ERR-TIPO-ACESSO
SV2204
SV2204         MOVE RETURN-CODE        TO WRK-ERRO-0025-RET-COD
SV2204         MOVE WRK-ERRO-POOL0025  TO ERR-TEXTO
SV2204
SV2204         PERFORM 9999-PROCESSAR-ROTINA-ERRO
SV2204     END-IF.
SV2204
SV2204     MOVE WRK-0025-DATA-SAIDA    TO WRK-DATA-AUX.
SV2204     MOVE WRK-DIA-AUX            TO WRK-DIA-FIM-PERIODO-ATU.
SV2204     MOVE WRK-MES-AUX            TO WRK-MES-FIM-PERIODO-ATU.
SV2204     MOVE WRK-ANO-AUX            TO WRK-ANO-FIM-PERIODO-ATU.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ABRE O CURSOR GFCTB007 DAS TABELAS GFCTB001 E GFCTB007.     *
      ******************************************************************
      *----------------------------------------------------------------*
       3200-ABRIR-CSR01-GFCTB007       SECTION.
      *----------------------------------------------------------------*

SV2204     MOVE WRK-DATA-INI-PERIODO-ATU
SV2204                         TO DINIC-ADSAO-COMP OF GFCTB001
SV2204                            WRK-INICIO.
SV2204     MOVE WRK-DATA-FIM-PERIODO-ATU
SV2204                         TO DFIM-ADSAO-COMP  OF GFCTB001
SV2204                            WRK-FIM.

           EXEC SQL
               OPEN CSR01-GFCTB007
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'ADSAO_COMP_PCOTE' TO ERR-DBD-TAB
               MOVE 'OPEN'             TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0020'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    LER O CURSOR GFCTB007 DAS TABELAS GFCTB001 E GFCTB007.      *
      ******************************************************************
      *----------------------------------------------------------------*
       3300-LER-CSR01-GFCTB007         SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH CSR01-GFCTB007    INTO
                   :GFCTB007.CSERVC-TARIF,
                   :GFCTB007.CAGPTO-CTA,
                   :GFCTB007.CSEQ-AGPTO-CTA,
                   :GFCTB007.DINIC-ADSAO-COMP,
                   :GFCTB007.CSGMTO-GSTAO-TARIF,
                   :GFCTB001.DFIM-ADSAO-COMP,
BI0810             :GFCTB001.CPSSOA-SERVC-TARIF,
                   :WRK-CSGMTO-GSTAO-TARIF-NULL
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'               )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'ADSAO_GRP_SGMTO'  TO ERR-DBD-TAB
               MOVE 'FETCH   '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0030'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 999             TO CSGMTO-GSTAO-TARIF   OF GFCTB007
               MOVE 'S'             TO WRK-FIM-CSR01
           ELSE
               ADD 1                TO ACU-LIDOS-CSR01
           END-IF.

      *----------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCESSAMENTO DO CURSOR GFCTB007 DAS TABELAS GFCTB001 E     *
      *    GFCTB007.                                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       3400-PROCESSAR-CSR01-GFCTB007   SECTION.
      *----------------------------------------------------------------*

           EVALUATE TRUE
             WHEN CSGMTO-GSTAO-TARIF   OF GFCTB007
                                       LESS GFCTRY-SEGTO-GFCT
               PERFORM 3300-LER-CSR01-GFCTB007                     UNTIL
               CSGMTO-GSTAO-TARIF      OF GFCTB007
                                       NOT LESS GFCTRY-SEGTO-GFCT

             WHEN CSGMTO-GSTAO-TARIF   OF GFCTB007
                                       GREATER GFCTRY-SEGTO-GFCT
               PERFORM 2200-LER-CLIESEG                            UNTIL
               GFCTRY-SEGTO-GFCT       NOT LESS CSGMTO-GSTAO-TARIF
                                       OF GFCTB007

             WHEN CSGMTO-GSTAO-TARIF   OF GFCTB007
                                       EQUAL GFCTRY-SEGTO-GFCT  AND
                  GFCTRY-SEGTO-GFCT    NOT EQUAL  999
               INITIALIZE TAB-1-SEGMENTO
                          IND-1
               MOVE CSGMTO-GSTAO-TARIF OF GFCTB007
                                       TO TAB-1-SEGMENTO-N
               PERFORM 3410-MONTA-TABELA-1                         UNTIL
                   CSGMTO-GSTAO-TARIF  OF GFCTB007
                                       GREATER TAB-1-SEGMENTO-N
               PERFORM 3420-DESCARREGA-SEGMENTO                    UNTIL
                   GFCTRY-SEGTO-GFCT   GREATER TAB-1-SEGMENTO-N
           END-EVALUATE.

      *----------------------------------------------------------------*
       3400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA QUE GUARDA PACOTES                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       3410-MONTA-TABELA-1             SECTION.
      *----------------------------------------------------------------*

           ADD 1                               TO IND-1.

           IF  IND-1                           GREATER 300
               DISPLAY '*************** GFCT2165 ***************'
               DISPLAY '*                                      *'
               DISPLAY '*      ESTOURO NA TABELA INTERNA 1     *'
               DISPLAY '*                                      *'
               DISPLAY '*************** GFCT2165 ***************'
               MOVE 'APL'                      TO ERR-TIPO-ACESSO
               MOVE WRK-MSG03                  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE CSERVC-TARIF     OF GFCTB007   TO TAB-1-PACOTE (IND-1).
           MOVE CAGPTO-CTA       OF GFCTB007   TO TAB-1-AGPTO  (IND-1).
           MOVE CSEQ-AGPTO-CTA   OF GFCTB007   TO TAB-1-SEQ    (IND-1).
           MOVE DINIC-ADSAO-COMP OF GFCTB007   TO TAB-1-DINI   (IND-1).
BI0810     MOVE CPSSOA-SERVC-TARIF  OF GFCTB001
BI0810                                 TO TAB-1-TPSSOA-ADESAO(IND-1).

           PERFORM 3300-LER-CSR01-GFCTB007.

      *----------------------------------------------------------------*
       3410-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA QUE DESCARREGA SEGMENTO.                             *
      ******************************************************************
      *----------------------------------------------------------------*
       3420-DESCARREGA-SEGMENTO        SECTION.
      *----------------------------------------------------------------*

           PERFORM 3421-PROCESSA-PACOTES
               VARYING IND-1           FROM 1 BY 1 UNTIL
               TAB-1-PACOTE(IND-1)     EQUAL ZEROS

           PERFORM 2200-LER-CLIESEG.

      *----------------------------------------------------------------*
       3420-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA PARA PROCESSAMENTO DE PACOTES.                       *
      ******************************************************************
      *----------------------------------------------------------------*
       3421-PROCESSA-PACOTES           SECTION.
      *----------------------------------------------------------------*

           PERFORM 3422-MOVER-DADOS-SAIDA.
           PERFORM 3423-GRAVAR-ADESSEG.

           MOVE GFCTRY-AGENCIA         TO LD1-REL1-AGENCIA.
           MOVE GFCTRY-CONTA           TO LD1-REL1-CONTA.
           MOVE TAB-1-PACOTE (IND-1)   TO LD1-REL1-PACOTE.
           MOVE TAB-1-AGPTO  (IND-1)   TO LD1-REL1-AGRUPAMENTO.
           MOVE TAB-1-SEQ    (IND-1)   TO LD1-REL1-SEQUENCIA.
           MOVE TAB-1-DINI   (IND-1)   TO WRK-DATA.
           MOVE WRK-DATA-DIA          TO LD1-REL1-DIA-INI.
           MOVE WRK-DATA-MES          TO LD1-REL1-MES-INI.
           MOVE WRK-DATA-ANO          TO LD1-REL1-ANO-INI.

           PERFORM 3424-EMITE-RELATORIO.

      *----------------------------------------------------------------*
       3421-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA PARA MOVIMENTACAO DE DADOS PARA ARQUIVO DE SAIDA.    *
      ******************************************************************
      *----------------------------------------------------------------*
       3422-MOVER-DADOS-SAIDA          SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTRY-AGENCIA         TO WAGM-AGENCIA.
           MOVE GFCTRY-RAZAO           TO WAGM-RAZAO.
           MOVE GFCTRY-CONTA           TO WAGM-CONTA.
           MOVE GFCTRY-POSTO-SERV      TO WAGM-PAB.

           MOVE TAB-1-PACOTE (IND-1)   TO WAGM-PACOTE.
           MOVE TAB-1-AGPTO  (IND-1)   TO WAGM-AGPTO.
           MOVE TAB-1-SEQ    (IND-1)   TO WAGM-SEQ.
           MOVE TAB-1-DINI   (IND-1)   TO WAGM-DATA.
BI0810     MOVE TAB-1-TPSSOA-ADESAO(IND-1) TO WAGM-PESSOA-ADESAO.

BI0412     IF  (TAB-1-PACOTE(IND-1)   EQUAL   WRK-B0C6-PACOTE-SS) AND
BI0412         (TAB-1-AGPTO(IND-1)    EQUAL   WRK-B0C6-AGPTO-SS)  AND
BI0412         (TAB-1-SEQ(IND-1)      EQUAL   WRK-B0C6-SEQ-SS)    AND
BI0412         (TAB-1-DINI(IND-1)     EQUAL   WRK-B0C6-DINI)
BI0412         NEXT SENTENCE
BI0412     ELSE
BI0412         MOVE TAB-1-PACOTE(IND-1)   TO  WRK-B0C6-PACOTE-SS
BI0412         MOVE TAB-1-AGPTO(IND-1)    TO  WRK-B0C6-AGPTO-SS
BI0412         MOVE TAB-1-SEQ(IND-1)      TO  WRK-B0C6-SEQ-SS
BI0412         MOVE TAB-1-DINI(IND-1)     TO  WRK-B0C6-DINI
BI0412         PERFORM 3550-ACESSA-GFCTB0C6
BI0412     END-IF.

BI0412     MOVE WRK-VISUALIZA          TO WAGM-VISUALIZA.

           PERFORM 3422-2-MONTA-NO-MES.

      *----------------------------------------------------------------*
       3422-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

SV2204*----------------------------------------------------------------*
SV2204 3422-2-MONTA-NO-MES             SECTION.
SV2204*----------------------------------------------------------------
SV2204
SV2204     MOVE WRK-DATA-INI-PERIODO-ATU TO WRK-DATA-AUX0.
SV2204     MOVE WRK-DIA-AUX0             TO WRK-DIA-AUX1.
SV2204     MOVE WRK-MES-AUX0             TO WRK-MES-AUX1.
SV2204     MOVE WRK-ANO-AUX0             TO WRK-ANO-AUX1.
SV2204     MOVE WRK-DATA-AUX1-R          TO WAGM-DINI.
SV2204
SV2204     MOVE WRK-DATA-FIM-PERIODO-ATU TO WRK-DATA-AUX0.
SV2204     MOVE WRK-DIA-AUX0             TO WRK-DIA-AUX1.
SV2204     MOVE WRK-MES-AUX0             TO WRK-MES-AUX1.
SV2204     MOVE WRK-ANO-AUX0             TO WRK-ANO-AUX1.
SV2204     MOVE WRK-DATA-AUX1-R          TO WAGM-DFIM.
SV2204
SV2204*----------------------------------------------------------------*
SV2204 3422-2-99-FIM.                 EXIT.
SV2204*----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA PARA GRAVACAO DE ADESSEG.                            *
      ******************************************************************
      *----------------------------------------------------------------*
       3423-GRAVAR-ADESSEG             SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           WRITE WAGM-ADESOES.
           PERFORM 1120-TESTAR-FS-ADESSEG.

           ADD 1                       TO ACU-GRAVADOS-ADESSEG.

      *----------------------------------------------------------------*
       3423-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA PARA EMISSAO DE RELATORIO.                           *
      ******************************************************************
      *----------------------------------------------------------------*
       3424-EMITE-RELATORIO            SECTION.
      *----------------------------------------------------------------*

           IF (ACU-LINHAS              GREATER 52) AND
              (GFCTRY-SEGTO-GFCT       NOT EQUAL WRK-SEGMENTO-ANT)
               PERFORM 3425-TOTAL-CLIENTES
               PERFORM 3426-EMITIR-CABEC
               MOVE GFCTRY-SEGTO-GFCT  TO WRK-SEGMENTO-ANT
           ELSE
               IF (ACU-PAGINAS         EQUAL ZEROS) OR
                  (ACU-LINHAS          GREATER 52 )
                   PERFORM 3426-EMITIR-CABEC
               ELSE
                   IF (GFCTRY-SEGTO-GFCT
                                       NOT EQUAL WRK-SEGMENTO-ANT)
                       PERFORM 3425-TOTAL-CLIENTES
                       PERFORM 3427-CABEC-NOVA-AGENCIA
                       MOVE GFCTRY-SEGTO-GFCT
                                       TO WRK-SEGMENTO-ANT
                   END-IF
               END-IF
           END-IF.

           WRITE FD-RELADSEG           FROM LINDET1-REL1.
           PERFORM 1130-TESTAR-FS-RELADSEG.

           ADD 1                       TO ACU-TOTAL-SEGMENTO.
           ADD 1                       TO ACU-LINHAS.

      *----------------------------------------------------------------*
       3424-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA PARA EMISSAO DE TOTAL DE CONTAS POR AGENCIA.         *
      ******************************************************************
      *----------------------------------------------------------------*
       3425-TOTAL-CLIENTES             SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-SEGMENTO-ANT       TO LT1-REL1-SEGMENTO.
           MOVE ACU-TOTAL-SEGMENTO     TO LT1-REL1-TOT-CLIENTES.
           MOVE ZEROS                  TO ACU-TOTAL-SEGMENTO.

           WRITE FD-RELADSEG           FROM CABEC8-REL1-BRANCO.
           PERFORM 1130-TESTAR-FS-RELADSEG.
           WRITE FD-RELADSEG           FROM CABEC8-REL1-BRANCO.
           PERFORM 1130-TESTAR-FS-RELADSEG.
           WRITE FD-RELADSEG           FROM LINTOT1-REL1.
           PERFORM 1130-TESTAR-FS-RELADSEG.

           ADD 3                       TO ACU-LINHAS.

      *----------------------------------------------------------------*
       3425-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA PARA EMISSAO DE CABECARIO.                           *
      ******************************************************************
      *----------------------------------------------------------------*
       3426-EMITIR-CABEC               SECTION.
      *----------------------------------------------------------------*

           ADD 1                       TO ACU-PAGINAS.

           MOVE GFCTRY-SEGTO-GFCT      TO WRK-SEGMENTO-ANT
           MOVE ZEROS                  TO ACU-LINHAS.
           MOVE ACU-PAGINAS            TO CB2-REL1-PAG.

           WRITE FD-RELADSEG           FROM CABEC1-REL1.
           PERFORM 1130-TESTAR-FS-RELADSEG.
           WRITE FD-RELADSEG           FROM CABEC2-REL1.
           PERFORM 1130-TESTAR-FS-RELADSEG.
           WRITE FD-RELADSEG           FROM CABEC3-REL1.
           PERFORM 1130-TESTAR-FS-RELADSEG.
           WRITE FD-RELADSEG           FROM CABEC4-REL1.
           PERFORM 1130-TESTAR-FS-RELADSEG.
           WRITE FD-RELADSEG           FROM CABEC5-REL1.
           PERFORM 1130-TESTAR-FS-RELADSEG.
           WRITE FD-RELADSEG           FROM CABEC4-REL1.
           PERFORM 1130-TESTAR-FS-RELADSEG.
           WRITE FD-RELADSEG           FROM CABEC8-REL1-BRANCO.
           PERFORM 1130-TESTAR-FS-RELADSEG.
           WRITE FD-RELADSEG           FROM CABEC6-REL1.
           PERFORM 1130-TESTAR-FS-RELADSEG.
           WRITE FD-RELADSEG           FROM CABEC4-REL1.
           PERFORM 1130-TESTAR-FS-RELADSEG.

           ADD 9                       TO ACU-LINHAS.

      *----------------------------------------------------------------*
       3426-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA PARA EMISSAO DE CABECARIO DE NOVA AGENCIA.           *
      ******************************************************************
      *----------------------------------------------------------------*
       3427-CABEC-NOVA-AGENCIA         SECTION.
      *----------------------------------------------------------------*

           WRITE FD-RELADSEG           FROM CABEC8-REL1-BRANCO.
           PERFORM 1130-TESTAR-FS-RELADSEG.
           WRITE FD-RELADSEG           FROM CABEC8-REL1-BRANCO.
           PERFORM 1130-TESTAR-FS-RELADSEG.
           WRITE FD-RELADSEG           FROM CABEC6-REL1.
           PERFORM 1130-TESTAR-FS-RELADSEG.
           WRITE FD-RELADSEG           FROM CABEC4-REL1.
           PERFORM 1130-TESTAR-FS-RELADSEG.

      *----------------------------------------------------------------*
       3427-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    FECHAR O CURSOR DA TABELA GFCTB007                          *
      ******************************************************************
      *----------------------------------------------------------------*
       3500-CLOSE-CSR01-GFCTB007       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR01-GFCTB007
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'ADSAO_COMP_PCOTE' TO ERR-DBD-TAB
               MOVE 'CLOSE   '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0040'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       3500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

BI0412*----------------------------------------------------------------*
       3550-ACESSA-GFCTB0C6            SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-VISUALIZA.

           MOVE WRK-B0C6-PACOTE
                                       TO CSERVC-TARIF      OF GFCTB0C6.
           MOVE WRK-B0C6-AGPTO
                                       TO CAGPTO-CTA        OF GFCTB0C6.
           MOVE WRK-B0C6-SEQ
                                       TO CSEQ-AGPTO-CTA    OF GFCTB0C6.
BI1212     MOVE WRK-DATA-INI-PERIODO-ATU
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0C6.
           MOVE WRK-DATA-FIM-PERIODO-ATU
                                       TO DFIM-PRMSS-PCOTE  OF GFCTB0C6.

           MOVE 'C'                    TO WRK-CINDCD-ADSAO-INDVD.
           MOVE 'A'                    TO WRK-CINDCD-ADSAO-AMBAS.

           EXEC SQL
             SELECT
                   CINDCD_VSLAO_COMP
             INTO
                   :GFCTB0C6.CINDCD-VSLAO-COMP
             FROM  DB2PRD.PRMSS_ADSAO_PCOTE
             WHERE
                   CSERVC_TARIF         = :GFCTB0C6.CSERVC-TARIF
               AND CAGPTO_CTA           = :GFCTB0C6.CAGPTO-CTA
               AND CSEQ_AGPTO_CTA       = :GFCTB0C6.CSEQ-AGPTO-CTA
BI1112         AND DINIC_PRMSS_PCOTE   <= :GFCTB0C6.DINIC-PRMSS-PCOTE
               AND DFIM_PRMSS_PCOTE    >= :GFCTB0C6.DFIM-PRMSS-PCOTE
               AND (CINDCD_ADSAO_INDVD  = :WRK-CINDCD-ADSAO-INDVD OR
                    CINDCD_ADSAO_INDVD  = :WRK-CINDCD-ADSAO-AMBAS)
           END-EXEC.

BI1112     IF (SQLCODE       NOT EQUAL ZEROS AND +100 AND -811) OR
              (SQLWARN0                EQUAL 'W'      )
                MOVE 'DB2'                TO ERR-TIPO-ACESSO
                MOVE 'PRMSS_ADSAO_PCOTE'  TO ERR-DBD-TAB
                MOVE 'SELECT'             TO ERR-FUN-COMANDO
                MOVE  SQLCODE             TO ERR-SQL-CODE
                MOVE '0050'               TO ERR-LOCAL
                MOVE  SPACES              TO ERR-SEGM
                PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

BI1112     IF SQLCODE             EQUAL  ZEROS  OR  -811
              IF   CINDCD-VSLAO-COMP OF GFCTB0C6   EQUAL   1
                   MOVE  'S'         TO   WRK-VISUALIZA
              ELSE
                   MOVE  'N'         TO   WRK-VISUALIZA
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       3550-99-FIM.                    EXIT.
BI0412*----------------------------------------------------------------*

      ******************************************************************
      *    EFETUA EMISSAO DO DISPLAY DE TERMINO NORMAL.                *
      ******************************************************************
      *----------------------------------------------------------------*
       3600-EMITIR-DISPLAY             SECTION.
      *----------------------------------------------------------------*

           DISPLAY '*************** GFCT2165 ***************'.
           DISPLAY '*                                      *'.
           DISPLAY '*   NAO EXISTE SOLICITACAO DE ADESAO   *'.
           DISPLAY '*   SEGMENTO PARA O PERIODO DE:        *'.
           DISPLAY '*   DATA INICIO : '
                                    WRK-INICIO '           *'.
           DISPLAY '*   DATA FINAL  : '
                                    WRK-FIM    '           *'.
           DISPLAY '*                                      *'.
           DISPLAY '*           TERMINO NORMAL             *'.
           DISPLAY '*                                      *'.
           DISPLAY '*                                      *'.
           DISPLAY '*************** GFCT2165 ***************'.

      *----------------------------------------------------------------*
       3600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA QUE EFETUA PROCESSAMENTO FINAL DO PROGRAMA           *
      ******************************************************************
      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           IF  ACU-LIDOS-CSR01         NOT EQUAL ZEROS
               PERFORM 3425-TOTAL-CLIENTES
               PERFORM 4100-TOTALIZAR-RELATORIO
           END-IF.

           PERFORM 4200-EMITIR-DISPLAY.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           CLOSE CLIESEG
                 ADESSEG
                 RELADSEG.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.
           PERFORM 1100-TESTAR-FILE-STATUS.

           STOP RUN.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE EMISSAO DE TOTAIS NO RELATORIO.                   *
      ******************************************************************
      *----------------------------------------------------------------*
       4100-TOTALIZAR-RELATORIO        SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-DIA                TO CB1-REL1-DIA.
           MOVE WRK-MES                TO CB1-REL1-MES.
           MOVE WRK-ANO                TO CB1-REL1-ANO.
           MOVE WRK-HOR                TO CB1-REL1-HORA.
           MOVE WRK-MIN                TO CB1-REL1-MIN.
           MOVE WRK-SEG                TO CB1-REL1-SEG.

           ADD 1                       TO ACU-PAGINAS.
           MOVE ACU-PAGINAS            TO CB2-REL1-PAG.

           WRITE FD-RELADSEG           FROM CABEC1-REL1.
           PERFORM 1130-TESTAR-FS-RELADSEG.
           WRITE FD-RELADSEG           FROM CABEC2-REL1.
           PERFORM 1130-TESTAR-FS-RELADSEG.
           WRITE FD-RELADSEG           FROM CABEC3-REL1.
           PERFORM 1130-TESTAR-FS-RELADSEG.
           WRITE FD-RELADSEG           FROM CABEC4-REL1.
           PERFORM 1130-TESTAR-FS-RELADSEG.
           WRITE FD-RELADSEG           FROM CABEC5-REL1.
           PERFORM 1130-TESTAR-FS-RELADSEG.
           WRITE FD-RELADSEG           FROM CABEC4-REL1.
           PERFORM 1130-TESTAR-FS-RELADSEG.
           WRITE FD-RELADSEG           FROM CABEC8-REL1-BRANCO.
           PERFORM 1130-TESTAR-FS-RELADSEG.
           WRITE FD-RELADSEG           FROM CABEC7-REL1.
           PERFORM 1130-TESTAR-FS-RELADSEG.
           WRITE FD-RELADSEG           FROM CABEC8-REL1-BRANCO.
           PERFORM 1130-TESTAR-FS-RELADSEG.
           WRITE FD-RELADSEG           FROM CABEC8-REL1-BRANCO.
           PERFORM 1130-TESTAR-FS-RELADSEG.

           MOVE ACU-LIDOS-CLIESEG      TO LT2-REL1-TOT-LIDOS.
           MOVE ACU-LIDOS-CSR01        TO LT3-REL1-TOT-SOLIC.
           MOVE ACU-GRAVADOS-ADESSEG   TO LT4-REL1-TOT-GRAVA.

           WRITE FD-RELADSEG           FROM LINTOT2-REL1.
           PERFORM 1130-TESTAR-FS-RELADSEG.
           WRITE FD-RELADSEG           FROM CABEC8-REL1-BRANCO.
           PERFORM 1130-TESTAR-FS-RELADSEG.
           WRITE FD-RELADSEG           FROM LINTOT3-REL1.
           PERFORM 1130-TESTAR-FS-RELADSEG.
           WRITE FD-RELADSEG           FROM CABEC8-REL1-BRANCO.
           PERFORM 1130-TESTAR-FS-RELADSEG.
           WRITE FD-RELADSEG           FROM LINTOT4-REL1.
           PERFORM 1130-TESTAR-FS-RELADSEG.

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE EMISSAO DE DISPLAY DE TOTAIS                      *
      ******************************************************************
      *----------------------------------------------------------------*
       4200-EMITIR-DISPLAY             SECTION.
      *----------------------------------------------------------------*

           DISPLAY '*************** GFCT2165 ***************'.
           DISPLAY '*                                      *'.
           DISPLAY '*   PROGRAMA ENCERRADO COM SUCESSO     *'.
           DISPLAY '*                                      *'.
           DISPLAY '*   PROCESSAMENTO DE : '
                       DPROCM-ATUAL OF GFCTB0A1     '      *'.
           DISPLAY '*                                      *'.
           DISPLAY '*            TERMINO NORMAL            *'.
           DISPLAY '*                                      *'.
           DISPLAY '*************** GFCT2165 ***************'.

      *----------------------------------------------------------------*
       4200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE PROCESSAMENTE DE ERRRO                            *
      ******************************************************************
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           IF  ERR-TIPO-ACESSO         EQUAL LOW-VALUES OR SPACES
               MOVE 'APL'              TO ERR-TIPO-ACESSO
           END-IF.

           MOVE 'GFCT2165'             TO ERR-PGM.
           CALL 'POOL7100'             USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

