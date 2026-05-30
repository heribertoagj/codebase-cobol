      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT2125.
       AUTHOR.     TEREZA RACHEL MAFIOLETI.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT2125                                    *
      *    PROGRAMADORA:   TEREZA RACHEL MAFIOLETI - CPM PATO BRANCO   *
      *    ANALISTA CPM:   EDSON LUIZ DOS SANTOS   - CPM PATO BRANCO   *
      *    ANALISTA....:   MARCELO CREMM           - PROCWORK / GP.50  *
      *    DATA........:   29/05/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   GERACAO DE MOVIMENTO DE ADESAO  COMPULSORIA.*
      *      COM BASE NOS REGISTROS OBTIDOS EM  CLIEMUN, SERAO GRAVADOS*
      *      NO ARQUIVO ADESMUN, DE ACORDO COM OS PACOTES  SOLICITADOS,*
      *      DA TABELA DE SOLICITACOES DE ADESAO (GFCTB004). SERAO GRA-*
      *      VADOS  PARA  CADA OCORRENCIA DO  ARQUIVO CLIEMUN COM MESMO*
      *      CODIGO  DE MUNICIPIO  SOLICITADO NO  CURSOR, TODOS PACOTES*
      *      GRAVADOS NA TABELA 1.                                     *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                      INCLUDE/BOOK        *
      *                    CLIEMUN                   I#GFCTRY          *
      *                    ADESMUN                   GFCTWAGM          *
      *                    RELADMUN                      -             *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                         INCLUDE/BOOK    *
      *                    DB2PRD.PARM_DATA_PROCM        GFCTB0A1      *
      *                    DB2PRD.ADSAO_COMP_PCOTE       GFCTB001      *
      *                    DB2PRD.ADSAO_GRP_MUN          GFCTB004      *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    POL7100C - AREA PARA TRATAMENTO DE ERROS PELA POOL7100.     *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    POOL1050 - CONEXAO COM DB2.                                 *
      *    POOL7100 - TRATAMENTO DE ERROS PADRAO BRADESCO.             *
      *    POOL7600 - OBTER DATA E HORA DO SISTEMA.                    *
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
BI0810*----------------------------------------------------------------*
BI0810*    ULTIMA ALTERACAO EM AGO/2010 - BIRA                         *
BI0810*    - GERAR NO ARQUIVO DE SAIDA TIPO DE PESSOA (ADESAO) E       *
BI0810*      SUBSTITUIR O USO DA INC I#GFCTRZ PARA GFCTWAGM.           *
BI0810*                                                                *
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

           SELECT CLIEMUN  ASSIGN      TO UT-S-CLIEMUN
           FILE STATUS                 IS WRK-FS-CLIEMUN.

           SELECT ADESMUN ASSIGN       TO UT-S-ADESMUN
           FILE STATUS                 IS WRK-FS-ADESMUN.

           SELECT RELADMUN ASSIGN      TO UT-S-RELADMUN
           FILE STATUS                 IS WRK-FS-RELADMUN.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT : ARQUIVO DE CLIENTES - ENTRADA - CLIEMUN             *
      *            ORG. SEQUENCIAL     -   LRECL = 050                 *
      *----------------------------------------------------------------*

       FD  CLIEMUN
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTRY'.

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE CLIENTES ADERIDOS  -  ADESMUN            *
      *            ORG. SEQUENCIAL     -   LRECL = 050                 *
      *----------------------------------------------------------------*

       FD  ADESMUN
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

BI0810 COPY 'GFCTWAGM'.

      *----------------------------------------------------------------*
      *    OUTPUT: RELATORIO ANALITICO E TOTALIZADOR - RELADMUN        *
      *            ORG. SEQUENCIAL     -   LRECL = 133                 *
      *----------------------------------------------------------------*

       FD  RELADMUN
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-RELADMUN                 PIC  X(133).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* INICIO DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA DE INDEXADORES *'.
      *----------------------------------------------------------------*

       77  IND-1                       PIC  9(003) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA DE ACUMULADORES *'.
      *----------------------------------------------------------------*

       77  ACU-LINHAS                  PIC  9(003) COMP-3  VALUE 99.
       77  ACU-PAG                     PIC  9(005) COMP-3  VALUE ZEROS.
       77  ACU-LIDOS                   PIC  9(013) COMP-3  VALUE ZEROS.
       77  ACU-GRAVADOS                PIC  9(013) COMP-3  VALUE ZEROS.
       77  ACU-LIDOS-CURSOR            PIC  9(013) COMP-3  VALUE ZEROS.
       77  ACU-CONTA                   PIC  9(011) COMP-3  VALUE ZEROS.

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
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA DE TESTES DE FILE STATUS *'.
      *----------------------------------------------------------------*

       77  WRK-FS-CLIEMUN              PIC  X(002)         VALUE SPACES.
       77  WRK-FS-ADESMUN              PIC  X(002)         VALUE SPACES.
       77  WRK-FS-RELADMUN             PIC  X(002)         VALUE SPACES.

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

       77  WRK-QUOCIENTE               PIC  9(004)         VALUE ZEROS.
       77  WRK-RESTO                   PIC  9(004)         VALUE ZEROS.
       77  WRK-INICIO                  PIC  X(010)         VALUE SPACES.
       77  WRK-FIM                     PIC  X(010)         VALUE SPACES.
       77  WRK-FIM-CSR01-GFCTB004      PIC  X(001)         VALUE SPACES.
       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.

       01  WRK-DATA-TAB                PIC  X(010)         VALUE SPACES.
       01  WRK-DATA-TAB-R              REDEFINES           WRK-DATA-TAB.
           05  WRK-DIA-TAB             PIC  9(002).
           05  FILLER                  PIC  X(001).
           05  WRK-MES-TAB             PIC  9(002).
           05  FILLER                  PIC  X(001).
           05  WRK-ANO-TAB             PIC  9(004).

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA MENSAGENS *'.
      *---------------------------------------------------------------*

       01  WRK-MENSAGENS.
           05 WRK-MSG001               PIC  X(058)         VALUE
           '** ARQUIVO CLIEMUN ESTA VAZIO, PROCESSAMENTO CANCELADO **'.
           05 WRK-MSG002               PIC  X(053)         VALUE
              '** ESTOURO DE TABELA INTERNA1 **'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE ERRO - FILE-STATUS *'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-TEXTO.
           05 FILLER                   PIC  X(008)         VALUE
              '** ERRO '.
           05 WRK-OPERACAO             PIC  X(013)         VALUE SPACES.
           05 FILLER                   PIC  X(012)         VALUE
              ' DO ARQUIVO '.
           05 WRK-NOME-ARQ             PIC  X(008)         VALUE SPACES.
           05 FILLER                   PIC  X(017)         VALUE
              ' - FILE STATUS ='.
           05 WRK-FILE-STATUS          PIC  X(002)         VALUE SPACES.
           05 FILLER                   PIC  X(003)         VALUE ' **'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DA TABELA INTERNA *'.
      *----------------------------------------------------------------*

       01  WRK-TAB-TABELA.
           05 WRK-TAB-MUNICIPIO        PIC  9(007)         VALUE ZEROS.
           05 WRK-TAB-DADOS            OCCURS 300 TIMES.
             10 WRK-TAB-PACOTE         PIC  9(005)         VALUE ZEROS.
             10 WRK-TAB-AGPTO          PIC  9(003)         VALUE ZEROS.
             10 WRK-TAB-SEQ            PIC  9(009)         VALUE ZEROS.
             10 WRK-TAB-DINI           PIC  X(010)         VALUE SPACES.
BI0810       10 WRK-TAB-TPSSOA-ADESAO  PIC  X(001)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DA POOL7600 *'.
      *----------------------------------------------------------------*

       01  WRK-DATA-HORA.
           05 WRK-DATA-JULIANA         PIC  9(005) COMP-3  VALUE ZEROS.
           05 WRK-DATA-AAMMDD          PIC  9(007) COMP-3  VALUE ZEROS.
           05 WRK-DATA-AAAAMMDD        PIC  9(009) COMP-3  VALUE ZEROS.
           05 WRK-HORA-HHMMSS          PIC  9(007) COMP-3  VALUE ZEROS.
           05 WRK-HORA-HHMMSSMMMMMM    PIC  9(013) COMP-3  VALUE ZEROS.
           05 WRK-TIMESTAMP.
             10 WRK-ANO-7600           PIC  9(004)         VALUE ZEROS.
             10 WRK-MES-7600           PIC  9(002)         VALUE ZEROS.
             10 WRK-DIA-7600           PIC  9(002)         VALUE ZEROS.
             10 WRK-HOR-7600           PIC  9(002)         VALUE ZEROS.
             10 WRK-MIN-7600           PIC  9(002)         VALUE ZEROS.
             10 WRK-SEG-7600           PIC  9(002)         VALUE ZEROS.
             10 FILLER                 PIC  9(006)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA RELATORIO *'.
      *----------------------------------------------------------------*

       01  CABEC1-REL1.
           05  FILLER                  PIC  X(001)         VALUE '1'.
           05  CB1-REL1-DD1            PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '/'.
           05  CB1-REL1-MM1            PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '/'.
           05  CB1-REL1-AA1            PIC  9(004)         VALUE ZEROS.
           05  FILLER                  PIC  X(040)         VALUE SPACES.
           05  FILLER                  PIC  X(011)         VALUE
               'B A N C O'.
           05  FILLER                  PIC  X(017)         VALUE
               'B R A D E S C O'.
           05  FILLER                  PIC  X(045)         VALUE 'S/A'.
           05  CB1-REL1-HH             PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE ':'.
           05  CB1-REL1-MM             PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE ':'.
           05  CB1-REL1-SS             PIC  9(002)         VALUE ZEROS.

       01  CABEC2-REL1.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  FILLER                  PIC  X(038)         VALUE
               'GFCT2125'.
           05  FILLER                  PIC  X(045)         VALUE
               'GFCT - GESTAO, FLEXIBILIZACAO E COBRANCA DE'.
           05  FILLER                  PIC  X(040)         VALUE
               'TARIFAS'.
           05  FILLER                  PIC  X(004)         VALUE 'PAG.'.
           05  CB2-REL1-PAG            PIC  ZZ99           VALUE ZEROS.

       01  CABEC3-REL1.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  FILLER                  PIC  X(034)         VALUE SPACES.
           05  FILLER                  PIC  X(054)         VALUE
               'RELATORIO DE CLIENTES PARTICIPANTES DE ADESOES POR MUN'.
           05  FILLER                  PIC  X(006)         VALUE
               'ICIPIO'.

       01  CABEC4-REL1.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  FILLER                  PIC  X(131)         VALUE
               ALL '-'.

       01  CABEC5-REL1.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  FILLER                  PIC  X(050)         VALUE SPACES.
           05  FILLER                  PIC  X(013)         VALUE
               'MOVIMENTO DE '.
           05  CB5-REL1-DD2            PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '/'.
           05  CB5-REL1-MM2            PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '/'.
           05  CB5-REL1-AA2            PIC  9(004)         VALUE ZEROS.

       01  CABEC6-REL1-BCO.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  FILLER                  PIC  X(131)         VALUE SPACES.

       01  CABEC7-REL1.
           05  FILLER                  PIC  X(001)         VALUE '0'.
           05  FILLER                  PIC  X(015)         VALUE
               'AGENCIA'.
           05  FILLER                  PIC  X(012)         VALUE
               'CONTA'.
           05  FILLER                  PIC  X(012)         VALUE
               'PACOTE'.
           05  FILLER                  PIC  X(017)         VALUE
               'AGRUPAMENTO'.
           05  FILLER                  PIC  X(015)         VALUE
               'SEQUENCIA'.
           05  FILLER                  PIC  X(014)         VALUE
               'DATA DE INICIO'.

       01  CABEC8-REL1.
           05  FILLER                  PIC  X(001)         VALUE '-'.
           05  FILLER                  PIC  X(032)         VALUE SPACES.
           05  FILLER                  PIC  X(054)         VALUE
               '----------------------- TOTALIZADOR FINAL ------------'.
           05  FILLER                  PIC  X(010)         VALUE
               '----------'.

       01  LINDET1-REL1.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  LD1-REL1-AGE            PIC  9(005)         VALUE ZEROS.
           05  FILLER                  PIC  X(007)         VALUE SPACES.
           05  LD1-REL1-CTA            PIC  9(007)         VALUE ZEROS.
           05  FILLER                  PIC  X(008)         VALUE SPACES.
           05  LD1-REL1-PCT            PIC  9(005)         VALUE ZEROS.
           05  FILLER                  PIC  X(009)         VALUE SPACES.
           05  LD1-REL1-AGP            PIC  9(003)         VALUE ZEROS.
           05  FILLER                  PIC  X(011)         VALUE SPACES.
           05  LD1-REL1-SQC            PIC  9(009)         VALUE ZEROS.
           05  FILLER                  PIC  X(008)         VALUE SPACES.
           05  LD1-REL1-DATA           PIC  X(010)         VALUE SPACES.

       01  LINTOT1-REL1.
           05  FILLER                  PIC  X(001)         VALUE '0'.
           05  FILLER                  PIC  X(031)         VALUE
               'QUANTIDADE TOTAL DE CONTAS DO'.
           05  FILLER                  PIC  X(011)         VALUE
               'MUNICIPIO'.
           05  LT1-REL1-MUN            PIC  ZZZZ9          VALUE ZEROS.
           05  FILLER                  PIC  X(003)         VALUE ' : '.
           05  LT1-REL1-TOT            PIC  Z.ZZZ.ZZZ.ZZ9  VALUE ZEROS.

       01  LINTOT2-REL1.
           05  FILLER                  PIC  X(001)         VALUE '-'.
           05  FILLER                  PIC  X(032)         VALUE SPACES.
           05  FILLER                  PIC  X(052)         VALUE
               '1- QUANTIDADE DE CLIENTES LIDOS EM CLIEMUN........: '.
           05  LT2-REL1-LI-CLI         PIC  ZZZ.ZZZ.ZZZ.ZZ9 VALUE ZEROS.

       01  LINTOT3-REL1.
           05  FILLER                  PIC  X(001)         VALUE '0'.
           05  FILLER                  PIC  X(032)         VALUE SPACES.
           05  FILLER                  PIC  X(052)         VALUE
               '2- QUANTIDADE DE SOLICITACOES DE ADESAO...........: '.
           05  LT3-REL1-SOL-AD         PIC  ZZZ.ZZZ.ZZZ.ZZ9 VALUE ZEROS.

       01  LINTOT4-REL1.
           05  FILLER                  PIC  X(001)         VALUE '0'.
           05  FILLER                  PIC  X(032)         VALUE SPACES.
           05  FILLER                  PIC  X(052)         VALUE
               '3- QUANTIDADE DE CLIENTES GRAVADOS EM ADESMUN.....: '.
           05  LT4-REL1-GRA-AD         PIC  ZZZ.ZZZ.ZZZ.ZZ9 VALUE ZEROS.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA TRATAMENTO DE ERRO *'.
      *----------------------------------------------------------------*

       COPY POL7100C.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA DB2 *'.
      *----------------------------------------------------------------*

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
             INCLUDE GFCTB004
           END-EXEC.

BI0412     EXEC SQL
BI0412       INCLUDE GFCTB0C6
BI0412     END-EXEC.

           EXEC SQL
               DECLARE CSR01-GFCTB004 CURSOR FOR
                SELECT A.CMUN_IBGE,
                       A.CSERVC_TARIF,
                       A.CAGPTO_CTA,
                       A.CSEQ_AGPTO_CTA,
                       A.DINIC_ADSAO_COMP,
                       B.DFIM_ADSAO_COMP,
BI0810                 B.CPSSOA_SERVC_TARIF
                  FROM DB2PRD.ADSAO_GRP_MUN    A,
                       DB2PRD.ADSAO_COMP_PCOTE B
                 WHERE A.CSERVC_TARIF      = B.CSERVC_TARIF
                   AND A.CAGPTO_CTA        = B.CAGPTO_CTA
                   AND A.CSEQ_AGPTO_CTA    = B.CSEQ_AGPTO_CTA
                   AND A.DINIC_ADSAO_COMP  = B.DINIC_ADSAO_COMP
                   AND B.DINIC_ADSAO_COMP <= :GFCTB001.DINIC-ADSAO-COMP
                   AND B.DFIM_ADSAO_COMP  >= :GFCTB001.DFIM-ADSAO-COMP
              ORDER BY A.CMUN_IBGE,
                       A.CSERVC_TARIF,
BI0412                 A.CAGPTO_CTA,
BI0412                 A.CSEQ_AGPTO_CTA,
BI0412                 A.DINIC_ADSAO_COMP
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* FIM DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      ******************************************************************
      *    CHAMA AS ROTINAS A SEREM PROCESSADAS.                       *
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
      *    CONEXAO COM DB2, ABERTURA DE ARQUIVOS E OBTENCAO DE DT-SIST *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL1050'.

           OPEN INPUT  CLIEMUN
                OUTPUT ADESMUN
                       RELADMUN.

           MOVE  WRK-ABERTURA          TO WRK-OPERACAO.
           PERFORM 1100-TESTAR-FILE-STATUS.

           PERFORM 1200-OBTER-DATA-SISTEMA.

           PERFORM 1300-OBTER-PARAMETROS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTE DE FILE STATUS DOS ARQUIVOS.                          *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-CLIEMUN.

           PERFORM 1120-TESTAR-FS-ADESMUN.

           PERFORM 1130-TESTAR-FS-RELADMUN.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTE DE FILE STATUS DO ARQUIVO CLIEMUN.                    *
      ******************************************************************
      *----------------------------------------------------------------*
       1110-TESTAR-FS-CLIEMUN          SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-CLIEMUN          NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-CLIEMUN     TO WRK-FILE-STATUS
               MOVE 'CLIEMUN'          TO WRK-NOME-ARQ
               MOVE WRK-ERRO-TEXTO     TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTE DE FILE STATUS DO ARQUIVO ADESMUN.                    *
      ******************************************************************
      *----------------------------------------------------------------*
       1120-TESTAR-FS-ADESMUN          SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ADESMUN          NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-ADESMUN     TO WRK-FILE-STATUS
               MOVE 'ADESMUN '         TO WRK-NOME-ARQ
               MOVE WRK-ERRO-TEXTO     TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTE DE FILE STATUS DO ARQUIVO RELADMUN.                   *
      ******************************************************************
      *----------------------------------------------------------------*
       1130-TESTAR-FS-RELADMUN         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-RELADMUN         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-RELADMUN    TO WRK-FILE-STATUS
               MOVE 'RELADMUN '        TO WRK-NOME-ARQ
               MOVE WRK-ERRO-TEXTO     TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    OBTEM DATA E HORA DO SISTEMA.                               *
      ******************************************************************
      *----------------------------------------------------------------*
       1200-OBTER-DATA-SISTEMA         SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL7600'             USING WRK-DATA-HORA.

           MOVE WRK-DIA-7600           TO CB1-REL1-DD1.
           MOVE WRK-MES-7600           TO CB1-REL1-MM1.
           MOVE WRK-ANO-7600           TO CB1-REL1-AA1.
           MOVE WRK-HOR-7600           TO CB1-REL1-HH.
           MOVE WRK-MIN-7600           TO CB1-REL1-MM.
           MOVE WRK-SEG-7600           TO CB1-REL1-SS.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    OBTEM PARAMETROS ATRAVES DA TABELA GFCTB0A1.                *
      ******************************************************************
      *----------------------------------------------------------------*
       1300-OBTER-PARAMETROS           SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC      OF GFCTB0A1.

           EXEC SQL
               SELECT DPROCM_ANTER,
                      DPROCM_ATUAL
                 INTO :GFCTB0A1.DPROCM-ANTER,
                      :GFCTB0A1.DPROCM-ATUAL
                 FROM DB2PRD.PARM_DATA_PROCM
                WHERE CSIST_PRINC      = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PARM_DATA_PROCM ' TO ERR-DBD-TAB
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0010'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO WRK-DATA-TAB.
           MOVE  WRK-DIA-TAB           TO CB5-REL1-DD2.
           MOVE  WRK-MES-TAB           TO CB5-REL1-MM2.
           MOVE  WRK-ANO-TAB           TO CB5-REL1-AA2.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICA SE O ARQUIVO DE CLIEMUN ESTA VAZIO.                *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-LER-CLIEMUN.

           IF  WRK-FS-CLIEMUN          EQUAL '10'
               DISPLAY '*************** GFCT2125 ***************'
               DISPLAY '*                                      *'
               DISPLAY '*         ARQUIVO CLIEMUN VAZIO        *'
               DISPLAY '*                                      *'
               DISPLAY '*        PROCESSAMENTO CANCELADO       *'
               DISPLAY '*                                      *'
               DISPLAY '*************** GFCT2125 ***************'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-MSG001         TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    LEITURA DO ARQUIVO DE PARAMETROS CLIEMUN.                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-LER-CLIEMUN                SECTION.
      *----------------------------------------------------------------*

           READ CLIEMUN.

           IF  WRK-FS-CLIEMUN          EQUAL '10'
               GO TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.
           PERFORM 1110-TESTAR-FS-CLIEMUN.

           ADD 1                       TO ACU-LIDOS.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCESSAMENTO PRINCIPAL PARA CADA REGISTRO DO ADESMUN.      *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 3100-OBTER-DATAS.

           PERFORM 3200-OPEN-CSR01-GFCTB004.

           PERFORM 3300-FETCH-CSR01-GFCTB004.

           IF  WRK-FIM-CSR01-GFCTB004  EQUAL 'S'
               PERFORM 3400-EMITIR-DISPLAY
           END-IF.

           PERFORM 3500-PROCESSAR-CURSOR UNTIL
               WRK-FIM-CSR01-GFCTB004  EQUAL 'S' OR
               WRK-FS-CLIEMUN          EQUAL '10'.

           PERFORM 3600-CLOSE-CSR01-GFCTB004.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    OBTEM DATA INICIAL E FINAL.                                 *
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
      *    ABRE O CURSOR DA TABELA GFCTB004.                           *
      ******************************************************************
      *----------------------------------------------------------------*
       3200-OPEN-CSR01-GFCTB004        SECTION.
      *----------------------------------------------------------------*

SV2204     MOVE WRK-DATA-INI-PERIODO-ATU
SV2204                         TO DINIC-ADSAO-COMP OF GFCTB001
SV2204                            WRK-INICIO.
SV2204     MOVE WRK-DATA-FIM-PERIODO-ATU
SV2204                         TO DFIM-ADSAO-COMP  OF GFCTB001
SV2204                            WRK-FIM.

           MOVE 'N'                    TO WRK-FIM-CSR01-GFCTB004.

           EXEC SQL
               OPEN CSR01-GFCTB004
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'ADSAO_GRP_MUN'    TO ERR-DBD-TAB
               MOVE 'OPEN    '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0020'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    LE O CURSOR DA TABELA GFCTB004.                             *
      ******************************************************************
      *----------------------------------------------------------------*
       3300-FETCH-CSR01-GFCTB004       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH CSR01-GFCTB004
                INTO :GFCTB004.CMUN-IBGE,
                     :GFCTB004.CSERVC-TARIF,
                     :GFCTB004.CAGPTO-CTA,
                     :GFCTB004.CSEQ-AGPTO-CTA,
                     :GFCTB004.DINIC-ADSAO-COMP,
                     :GFCTB001.DFIM-ADSAO-COMP,
BI0810               :GFCTB001.CPSSOA-SERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'ADSAO_GRP_MUN'    TO ERR-DBD-TAB
               MOVE 'FETCH   '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0030'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-CSR01-GFCTB004
               MOVE 9999999            TO CMUN-IBGE        OF GFCTB004
           ELSE
               ADD 1                   TO ACU-LIDOS-CURSOR
           END-IF.

      *----------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    EMITE DISPLAY.                                              *
      ******************************************************************
      *----------------------------------------------------------------*
       3400-EMITIR-DISPLAY             SECTION.
      *----------------------------------------------------------------*

           DISPLAY '*************** GFCT2125 ***************'.
           DISPLAY '*                                      *'.
           DISPLAY '*   NAO EXISTE SOLICITACAO DE ADESAO   *'.
           DISPLAY '*   MUNICIPIO PARA O PERIODO DE:       *'.
           DISPLAY '*   DATA INICIO : ' WRK-INICIO '           *'.
           DISPLAY '*   DATA FINAL  : ' WRK-FIM    '           *'.
           DISPLAY '*                                      *'.
           DISPLAY '*           TERMINO NORMAL             *'.
           DISPLAY '*                                      *'.
           DISPLAY '*************** GFCT2125 ***************'.

      *----------------------------------------------------------------*
       3400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCESSA O CURSOR.                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       3500-PROCESSAR-CURSOR           SECTION.
      *----------------------------------------------------------------*

           IF CMUN-IBGE  OF GFCTB004   LESS GFCTRY-MUNICIPIO
              PERFORM 3300-FETCH-CSR01-GFCTB004 UNTIL
              CMUN-IBGE  OF GFCTB004   NOT LESS GFCTRY-MUNICIPIO OR
              WRK-FIM-CSR01-GFCTB004   EQUAL 'S'
           ELSE
             IF CMUN-IBGE OF GFCTB004  GREATER GFCTRY-MUNICIPIO
                PERFORM 2100-LER-CLIEMUN UNTIL
                GFCTRY-MUNICIPIO       NOT LESS CMUN-IBGE OF GFCTB004 OR
                WRK-FS-CLIEMUN         EQUAL '10'
             ELSE
               INITIALIZE WRK-TAB-TABELA
               MOVE CMUN-IBGE OF GFCTB004   TO WRK-TAB-MUNICIPIO
               PERFORM 3510-MONTAR-TABELA   VARYING IND-1 FROM 1 BY 1
               UNTIL CMUN-IBGE  OF GFCTB004 GREATER WRK-TAB-MUNICIPIO OR
               WRK-FIM-CSR01-GFCTB004       EQUAL 'S'
               PERFORM 3520-TRATAR-MUNICIPIO
             END-IF
           END-IF.

      *----------------------------------------------------------------*
       3500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    MONTA TABELA INTERNA                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       3510-MONTAR-TABELA              SECTION.
      *----------------------------------------------------------------*

           IF  IND-1                   GREATER 300
               PERFORM 3511-EMITIR-DISPLAY-ESTOURO
           END-IF.

           MOVE CSERVC-TARIF           OF GFCTB004
                                       TO WRK-TAB-PACOTE (IND-1).
           MOVE CAGPTO-CTA             OF GFCTB004
                                       TO WRK-TAB-AGPTO  (IND-1).
           MOVE CSEQ-AGPTO-CTA         OF GFCTB004
                                       TO WRK-TAB-SEQ    (IND-1).
           MOVE DINIC-ADSAO-COMP       OF GFCTB004
                                       TO WRK-TAB-DINI   (IND-1).
BI0810     MOVE CPSSOA-SERVC-TARIF     OF GFCTB001
BI0810                                 TO WRK-TAB-TPSSOA-ADESAO(IND-1).

           PERFORM 3300-FETCH-CSR01-GFCTB004.

      *----------------------------------------------------------------*
       3510-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    EMITE DISPLAY COM MENSAGEM DE ESTOURO NA TABELA INTERNA.    *
      ******************************************************************
      *----------------------------------------------------------------*
       3511-EMITIR-DISPLAY-ESTOURO     SECTION.
      *----------------------------------------------------------------*

           DISPLAY '*************** GFCT2125 ***************'.
           DISPLAY '*                                      *'.
           DISPLAY '*       ESTOURO DE TABELA INTERNA1     *'.
           DISPLAY '*                                      *'.
           DISPLAY '*        PROCESSAMENTO CANCELADO       *'.
           DISPLAY '*                                      *'.
           DISPLAY '*************** GFCT2125 ***************'.
           MOVE 'APL'                  TO ERR-TIPO-ACESSO.
           MOVE WRK-MSG002             TO ERR-TEXTO.
           PERFORM 9999-PROCESSAR-ROTINA-ERRO.

      *----------------------------------------------------------------*
       3511-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TRATAR MUNICIPIO.                                           *
      ******************************************************************
      *----------------------------------------------------------------*
       3520-TRATAR-MUNICIPIO           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           IF  ACU-LINHAS              GREATER 55
               PERFORM 3521-IMPRIMIR-CABEC-PRINC
               PERFORM 3522-IMPRIMIR-CABEC-MUN
           ELSE
               PERFORM 3522-IMPRIMIR-CABEC-MUN
           END-IF.

           PERFORM 3523-DESCARREGAR-MUNICIPIO UNTIL
                   GFCTRY-MUNICIPIO    OF CLIEMUN GREATER
                   WRK-TAB-MUNICIPIO   OR
                   WRK-FS-CLIEMUN      EQUAL '10'

           PERFORM 3526-IMPRIMIR-TOTAL-MUN.

      *----------------------------------------------------------------*
       3520-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    IMPRIME CABECALHO PRINCIPAL.                                *
      ******************************************************************
      *----------------------------------------------------------------*
       3521-IMPRIMIR-CABEC-PRINC       SECTION.
      *----------------------------------------------------------------*

           ADD 1                       TO ACU-PAG.
           MOVE ACU-PAG                TO CB2-REL1-PAG.

           WRITE FD-RELADMUN           FROM CABEC1-REL1.
           PERFORM 1130-TESTAR-FS-RELADMUN.

           WRITE FD-RELADMUN           FROM CABEC2-REL1.
           PERFORM 1130-TESTAR-FS-RELADMUN.

           WRITE FD-RELADMUN           FROM CABEC3-REL1.
           PERFORM 1130-TESTAR-FS-RELADMUN.

           WRITE FD-RELADMUN           FROM CABEC4-REL1.
           PERFORM 1130-TESTAR-FS-RELADMUN.

           WRITE FD-RELADMUN           FROM CABEC5-REL1.
           PERFORM 1130-TESTAR-FS-RELADMUN.

           WRITE FD-RELADMUN           FROM CABEC4-REL1.
           PERFORM 1130-TESTAR-FS-RELADMUN.

           MOVE 06                     TO ACU-LINHAS.

      *----------------------------------------------------------------*
       3521-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    IMPRIME CABECALHO DA QUEBRA DE MUNICIPIO.                   *
      ******************************************************************
      *----------------------------------------------------------------*
       3522-IMPRIMIR-CABEC-MUN         SECTION.
      *----------------------------------------------------------------*

           WRITE FD-RELADMUN           FROM CABEC7-REL1.
           PERFORM 1130-TESTAR-FS-RELADMUN.

           WRITE FD-RELADMUN           FROM CABEC4-REL1.
           PERFORM 1130-TESTAR-FS-RELADMUN.

           ADD 3                       TO ACU-LINHAS.

      *----------------------------------------------------------------*
       3522-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    DESCARREGA OS MUNICIPIOS.                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       3523-DESCARREGAR-MUNICIPIO      SECTION.
      *----------------------------------------------------------------*

           PERFORM 3524-PROCESSAR-PACOTES
               VARYING IND-1           FROM 1 BY 1 UNTIL
               WRK-TAB-PACOTE (IND-1)  EQUAL ZEROS.

           PERFORM 2100-LER-CLIEMUN.

      *----------------------------------------------------------------*
       3523-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCESSA OS PACOTES.                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       3524-PROCESSAR-PACOTES          SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO WAGM-ADESOES.
           MOVE GFCTRY-AGENCIA         TO WAGM-AGENCIA.
           MOVE GFCTRY-RAZAO           TO WAGM-RAZAO.
           MOVE GFCTRY-CONTA           TO WAGM-CONTA.
           MOVE GFCTRY-POSTO-SERV      TO WAGM-PAB.
           MOVE WRK-TAB-PACOTE(IND-1)  TO WAGM-PACOTE.
           MOVE WRK-TAB-AGPTO (IND-1)  TO WAGM-AGPTO.
           MOVE WRK-TAB-SEQ   (IND-1)  TO WAGM-SEQ.
           MOVE WRK-TAB-DINI  (IND-1)  TO WAGM-DATA.
BI0810     MOVE WRK-TAB-TPSSOA-ADESAO(IND-1) TO WAGM-PESSOA-ADESAO.

BI0412     IF  (WRK-TAB-PACOTE(IND-1)   EQUAL   WRK-B0C6-PACOTE-SS) AND
BI0412         (WRK-TAB-AGPTO(IND-1)    EQUAL   WRK-B0C6-AGPTO-SS)  AND
BI0412         (WRK-TAB-SEQ(IND-1)      EQUAL   WRK-B0C6-SEQ-SS)    AND
BI0412         (WRK-TAB-DINI(IND-1)     EQUAL   WRK-B0C6-DINI)
BI0412         NEXT SENTENCE
BI0412     ELSE
BI0412         MOVE WRK-TAB-PACOTE(IND-1)   TO  WRK-B0C6-PACOTE-SS
BI0412         MOVE WRK-TAB-AGPTO(IND-1)    TO  WRK-B0C6-AGPTO-SS
BI0412         MOVE WRK-TAB-SEQ(IND-1)      TO  WRK-B0C6-SEQ-SS
BI0412         MOVE WRK-TAB-DINI(IND-1)     TO  WRK-B0C6-DINI
BI0412         PERFORM 3550-ACESSA-GFCTB0C6
BI0412     END-IF.

BI0412     MOVE WRK-VISUALIZA          TO WAGM-VISUALIZA.

           PERFORM 3524-20-MONTA-NO-MES.

           WRITE WAGM-ADESOES.
           PERFORM 1120-TESTAR-FS-ADESMUN.

           ADD 1                       TO ACU-GRAVADOS.

           PERFORM 3525-EMITIR-RELADMUN.

      *----------------------------------------------------------------*
       3524-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

SV2204*----------------------------------------------------------------*
SV2204 3524-20-MONTA-NO-MES             SECTION.
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
SV2204 3524-20-99-FIM.                 EXIT.
SV2204*----------------------------------------------------------------*

      ******************************************************************
      *    EMITE REALTORIO RELADMUN.                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       3525-EMITIR-RELADMUN            SECTION.
      *----------------------------------------------------------------*

           IF  ACU-LINHAS              GREATER 59
               PERFORM 3521-IMPRIMIR-CABEC-PRINC
               PERFORM 3522-IMPRIMIR-CABEC-MUN
           END-IF.

           MOVE GFCTRY-AGENCIA         TO LD1-REL1-AGE.
           MOVE GFCTRY-CONTA           TO LD1-REL1-CTA.
           MOVE WRK-TAB-PACOTE(IND-1)  TO LD1-REL1-PCT.
           MOVE WRK-TAB-AGPTO (IND-1)  TO LD1-REL1-AGP.
           MOVE WRK-TAB-SEQ   (IND-1)  TO LD1-REL1-SQC.
           MOVE WRK-TAB-DINI  (IND-1)  TO LD1-REL1-DATA.

           INSPECT LD1-REL1-DATA       REPLACING ALL '.' BY '/'.

           WRITE FD-RELADMUN           FROM LINDET1-REL1.
           PERFORM 1130-TESTAR-FS-RELADMUN.

           ADD 1                       TO ACU-CONTA
                                          ACU-LINHAS.

      *----------------------------------------------------------------*
       3525-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    IMPRIME TOTAL DE CONTAS DO MUNICIPIO.                       *
      ******************************************************************
      *----------------------------------------------------------------*
       3526-IMPRIMIR-TOTAL-MUN         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.
           MOVE WRK-TAB-MUNICIPIO      TO LT1-REL1-MUN.
           MOVE ACU-CONTA              TO LT1-REL1-TOT.

           WRITE FD-RELADMUN           FROM LINTOT1-REL1.
           PERFORM 1130-TESTAR-FS-RELADMUN.

           WRITE FD-RELADMUN           FROM CABEC6-REL1-BCO.
           PERFORM 1130-TESTAR-FS-RELADMUN.

           MOVE ZEROS                  TO ACU-CONTA.

           ADD 3                       TO ACU-LINHAS.

      *----------------------------------------------------------------*
       3526-99-FIM.                    EXIT.
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
               AND DINIC_PRMSS_PCOTE   <= :GFCTB0C6.DINIC-PRMSS-PCOTE
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
      *    FECHA CURSOR DA TABELA GFCTB004.                            *
      ******************************************************************
      *----------------------------------------------------------------*
       3600-CLOSE-CSR01-GFCTB004       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR01-GFCTB004
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'ADSAO_GRP_MUN'    TO ERR-DBD-TAB
               MOVE 'CLOSE   '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0040'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       3600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCEDIMENTOS REALIZADOS NO FINAL DO PROCESSAMENTO.         *
      ******************************************************************
      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 4100-IMPRIMIR-TOTAIS.

           PERFORM 4200-EMITIR-DISPLAY-FINAL.

           CLOSE CLIEMUN
                 ADESMUN
                 RELADMUN.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.
           PERFORM 1100-TESTAR-FILE-STATUS.

           STOP RUN.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    EMITE TOTAIS DO RELATORIO.                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       4100-IMPRIMIR-TOTAIS            SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 3521-IMPRIMIR-CABEC-PRINC.

           WRITE FD-RELADMUN           FROM CABEC8-REL1.
           PERFORM 1130-TESTAR-FS-RELADMUN.

           MOVE ACU-LIDOS              TO LT2-REL1-LI-CLI.
           WRITE FD-RELADMUN           FROM LINTOT2-REL1.
           PERFORM 1130-TESTAR-FS-RELADMUN.

           MOVE ACU-LIDOS-CURSOR       TO LT3-REL1-SOL-AD.
           WRITE FD-RELADMUN           FROM LINTOT3-REL1.
           PERFORM 1130-TESTAR-FS-RELADMUN.

           MOVE ACU-GRAVADOS           TO LT4-REL1-GRA-AD.
           WRITE FD-RELADMUN           FROM LINTOT4-REL1.
           PERFORM 1130-TESTAR-FS-RELADMUN.

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    EMITE DISPLAY FINAL                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       4200-EMITIR-DISPLAY-FINAL       SECTION.
      *----------------------------------------------------------------*

           DISPLAY '*************** GFCT2125 ***************'.
           DISPLAY '*                                      *'.
           DISPLAY '*   PROGRAMA ENCERRADO COM SUCESSO     *'.
           DISPLAY '*                                      *'.
           DISPLAY '*   PROCESSAMENTO DE : ' DPROCM-ATUAL OF GFCTB0A1 '
      -            '     *'.
           DISPLAY '*                                      *'.
           DISPLAY '*           TERMINO NORMAL             *'.
           DISPLAY '*                                      *'.
           DISPLAY '*************** GFCT2125 ***************'.

      *----------------------------------------------------------------*
       4200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    EXECUTA PROCEDIMENTO DE CANCELAMENTO DO PGM EM CASO DE ERRO *
      ******************************************************************
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT2125'             TO ERR-PGM.

           CALL 'POOL7100'             USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA.
           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

