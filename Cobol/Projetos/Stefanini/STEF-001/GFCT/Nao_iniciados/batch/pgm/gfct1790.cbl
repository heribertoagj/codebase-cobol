      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT1790.
       AUTHOR.     DAYANE FILAKOSKI.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT1790                                    *
      *    PROGRAMADORA:   DAYANE FILAKOSKI        - CPM PATO BRANCO   *
      *    ANALISTA CPM:   ADRIANO LOPES SANT'ANNA - CPM PATO BRANCO   *
      *    ANALISTA....:   MARIA VALERIA TORQUATO  - PROCKWORK/ 50     *
      *    DATA........:   06/07/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   EMISSAO DO RELATORIO DAS FLEXIBILIZACOES    *
      *      A VENCER NO MES SEGUINTE.                                 *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                        INCLUDE/BOOK     *
      *                   DB2PRD.TTPO_SGMTO_GSTAO       GFCTB0H5       *
      *                   DB2PRD.TPO_AGPTO_CTA          GFCTB0F3       *
      *                   DB2PRD.POSTO_BCRIO_SERVC      MESUB062       *
      *                   DB2PRD.DEPDC_BDSCO_ONLINE     MESUB010       *
      *                   DB2PRD.MOTVO_JUSTF_OPER       GFCTB089       *
      *                   DB2PRD.SERVC_TARIF_PRINC      GFCTB0D8       *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                      INCLUDE/BOOK        *
      *                    FLEXVENC                  I#GFCTT9          *
      *                    RELATO                        -             *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    POL7100C - AREA PARA TRATAMENTO DE ERRO PELA POOL7100.      *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    POOL1050 - MODULO PARA CONEXAO COM DB2.                     *
      *    POOL0080 - MODULO DE ALOCACAO DINAMICA DE OUTRO MODULO.     *
      *    POOL7100 - MODULO PARA CANCELAMENTO DO PROGRAMA.            *
      *    POOL7600 - MODULO PARA OBTER DATA E HORA DO SISTEMA.        *
      *================================================================*
      *================================================================*
      *    ULTIMA ALTERACAO EM 22/08/2008 - VALERIA                    *
      *  - INCLUSAO DE TRATAMENTO PARA AGRUPAMENTOS PERSONALIZADOS E   *
      *    AGRUPAMENTO 07 - FUNCIONARIO BRADESCO.                      *
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

           SELECT FLEXVENC ASSIGN      TO UT-S-FLEXVENC
           FILE STATUS                 IS WRK-FS-FLEXVENC.

           SELECT RELATO ASSIGN        TO UT-S-RELATO
           FILE STATUS                 IS WRK-FS-RELATO.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:  FLEXIBILIZACOES DEFERIDAS ATUALIZADAS               *
      *            ORG. SEQUENCIAL                 -     LRECL = 300   *
      *----------------------------------------------------------------*

       FD  FLEXVENC
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY I#GFCTT9.

      *----------------------------------------------------------------*
      *    OUTPUT: RELATORIO PEDIDO DE FLEXIBILIZACAO                  *
      *            ORG. SEQUENCIAL                    - LRECL = 133    *
      *----------------------------------------------------------------*

       FD  RELATO
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-RELATO                   PIC  X(133).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* INICIO DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA ACUMULADORES *'.
      *----------------------------------------------------------------*

       77  ACU-PAG                     PIC  9(005) COMP-3  VALUE ZEROS.
       77  ACU-LINHAS                  PIC  9(003) COMP-3  VALUE 99.
       77  ACU-LIDOS                   PIC  9(009) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA TESTE DE FILE STATUS *'.
      *----------------------------------------------------------------*

       77  WRK-FS-FLEXVENC             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-RELATO               PIC  X(002)         VALUE SPACES.

       77  WRK-ABERTURA                PIC  X(013)         VALUE
           ' NA ABERTURA '.
       77  WRK-LEITURA                 PIC  X(013)         VALUE
           ' NA LEITURA  '.
       77  WRK-GRAVACAO                PIC  X(013)         VALUE
           ' NA GRAVACAO '.
       77  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA VARIAVEIS AUXILIARES *'.
      *----------------------------------------------------------------*

       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.

       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-CDEPDC-ANT              PIC  9(005)         VALUE ZEROS.
       77  WRK-DFIM-ANT                PIC  X(010)         VALUE SPACES.
       77  WRK-AGENCIA-ANT             PIC  9(005)         VALUE ZEROS.
       77  WRK-CONTA-ANT               PIC  9(007)         VALUE ZEROS.
       77  WRK-AG-VIP-ANT              PIC  9(005)         VALUE ZEROS.
       77  WRK-CTA-VIP-ANT             PIC  9(007)         VALUE ZEROS.
       77  WRK-SEGMTO-ANT              PIC  9(005)         VALUE ZEROS.
       77  WRK-AGRUP-ANT               PIC  9(003)         VALUE ZEROS.
       77  WRK-SEGMTO09-ANT            PIC  9(003)         VALUE ZEROS.
       77  WRK-CDEPDC14-ANT            PIC  9(005)         VALUE ZEROS.
       77  WRK-CDEPDC15-ANT            PIC  9(005)         VALUE ZEROS.
       77  WRK-CPOSTO15-ANT            PIC  9(005)         VALUE ZEROS.
       77  WRK-AG16-ANT                PIC  9(005)         VALUE ZEROS.
       77  WRK-CTA16-ANT               PIC  9(007)         VALUE ZEROS.
       77  WRK-CDEPDC17-ANT            PIC  9(005)         VALUE ZEROS.
       77  WRK-CPOSTO17-ANT            PIC  9(005)         VALUE ZEROS.
       77  WRK-TPO-FLEX-ANT            PIC  9(002)         VALUE ZEROS.
       77  WRK-GRP-AGP26-ANT           PIC  9(003)         VALUE ZEROS.
       77  WRK-SGRP-AGP26-ANT          PIC  9(003)         VALUE ZEROS.

       01  WRK-CGC-AUX.
ST25X6*           05  WRK-CPF                 PIC  999.999.999    VALUE ZEROS.
           05  WRK-CPF                 PIC  X(009)         VALUE SPACES.
           05  FILLER                  PIC  X(001)         VALUE '/'.
ST25X6*           05  WRK-FILIAL              PIC  9(004)         VALUE ZEROS.
           05  WRK-FILIAL              PIC  X(004)         VALUE SPACES.
           05  FILLER                  PIC  X(001)         VALUE '-'.
           05  WRK-CONTROLE            PIC  X(002)         VALUE SPACES.

       01  WRK-SEGMENTO-AUX.
           05  WRK-NUMERO              PIC  9(003)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '-'.
           05  WRK-SEGMENTO-DESC       PIC  X(035)         VALUE SPACES.

       01  WRK-AGENCIAP-AUX.
           05  WRK-NAGE                PIC  9(005)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '-'.
           05  WRK-AGENCIA-DESC        PIC  X(035)         VALUE SPACES.

       01  WRK-POSTO-AUX.
           05  WRK-POSTO-1             PIC  9(005)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '/'.
           05  WRK-POSTO-2             PIC  9(005)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '-'.
           05  WRK-POSTO-DESC          PIC  X(035)         VALUE SPACES.

       01  WRK-EMPRESA-AUX.
           05  WRK-EMPRESA-1           PIC  9(005)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '/'.
           05  WRK-EMPRESA-2           PIC  999.999.999    VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '-'.
           05  WRK-EMPRESA-DESC        PIC  X(027)         VALUE SPACES.

       01  WRK-BCO-POSTAL-AUX.
           05  WRK-BCO-POST-1          PIC  9(005)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '/'.
           05  WRK-BCO-POST-2          PIC  9(005)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '-'.
LFM        05  WRK-BCO-POST-DESC       PIC  X(030)         VALUE SPACES.
LFM
LFM    01  WRK-RAZAO-AUX.
LFM        05  WRK-GRP-CTBIL           PIC  9(003)         VALUE ZEROS.
LFM        05  FILLER                  PIC  X(001)         VALUE '/'.
LFM        05  WRK-SGRP-CTBIL          PIC  9(003)         VALUE ZEROS.
LFM        05  FILLER                  PIC  X(001)         VALUE '-'.
LFM        05  WRK-DESC-CTBIL          PIC  X(034)         VALUE SPACES.
LFM
LFM     01  WRK-PLCCEA-AREA-COMUNICACAO.
LFM         03  WRK-BASE-1560-RECEBIDO.
LFM             05  WRK-BASE-1560-GRUPO.
LFM                 07  WRK-BASE-1560-CEMPR   PIC 9(03) VALUE ZEROS.
LFM                 07  WRK-BASE-1560-GRUPOR  PIC 9(03) VALUE ZEROS.
LFM                 07  WRK-BASE-1560-SGRUPO  PIC 9(03) VALUE ZEROS .
LFM         03  WRK-BASE-1560-DEVOLVIDO.
LFM             05  WRK-BASE-1560-NRAZAO      PIC X(40) VALUE SPACES.

       01  WRK-MUNICIPIO-AUX.
           05  WRK-MUNICIPIO           PIC  9(007)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '-'.
           05  WRK-MUNICIPIO-DESC      PIC  X(035)         VALUE SPACES.

       01  WRK-UF-AUX.
           05  WRK-UF                  PIC  X(002)         VALUE SPACES.
           05  FILLER                  PIC  X(001)         VALUE '-'.
           05  WRK-UF-DESC             PIC  X(040)         VALUE SPACES.

       01  WRK-DATA-AUX.
           05  WRK-DIA-AUX             PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  WRK-MES-AUX             PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  WRK-ANO-AUX             PIC  9(004)         VALUE ZEROS.

       01  WRK-IO-PCB.
           05  WRK-IO-LTERM            PIC  X(008)         VALUE SPACES.
           05  FILLER                  PIC  X(002)         VALUE SPACES.
           05  WRK-IO-STATUS           PIC  X(002)         VALUE SPACES.
           05  FILLER                  PIC  X(012)         VALUE SPACES.
           05  WRK-IO-MODNAME          PIC  X(008)         VALUE SPACES.

       01  WRK-ALT-PCB.
           05  WRK-ALT-LTERM           PIC  X(008)         VALUE SPACES.
           05  FILLER                  PIC  X(002)         VALUE SPACES.
           05  WRK-ALT-STATUS          PIC  X(002)         VALUE SPACES.
           05  FILLER                  PIC  X(012)         VALUE SPACES.
           05  WRK-ALT-MODNAME         PIC  X(008)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA POOL7600 *'.
      *----------------------------------------------------------------*

       01  WRK-DATA-HORA.
           05 WRK-DATA-JULIANA         PIC  9(005) COMP-3  VALUE ZEROS.
           05 WRK-DATA-AAMMDD          PIC  9(007) COMP-3  VALUE ZEROS.
           05 WRK-DATA-AAAAMMDD        PIC  9(009) COMP-3  VALUE ZEROS.
           05 WRK-HORA-HHMMSS          PIC  9(007) COMP-3  VALUE ZEROS.
           05 WRK-HORA-HHMMSSMMMMMM    PIC  9(013) COMP-3  VALUE ZEROS.
           05 WRK-TIMESTAMP.
             10 WRK-ANO                PIC  9(004)         VALUE ZEROS.
             10 WRK-MES                PIC  9(002)         VALUE ZEROS.
             10 WRK-DIA                PIC  9(002)         VALUE ZEROS.
             10 WRK-HOR                PIC  9(002)         VALUE ZEROS.
             10 WRK-MIN                PIC  9(002)         VALUE ZEROS.
             10 WRK-SEG                PIC  9(002)         VALUE ZEROS.
             10 FILLER                 PIC  X(006)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA MODULO GFCT5508 *'.
      *----------------------------------------------------------------*

       01  WRK-5508-AREA-ENTRADA.
           05  WRK-5508-AMBIENTE       PIC  X(001)         VALUE SPACES.
           05  WRK-5508-CJUNC-DEPDC    PIC  9(005)         VALUE ZEROS.
           05  WRK-5508-CCTA-CLI       PIC  9(007)         VALUE ZEROS.

       01  WRK-5508-AREA-SAIDA.
           05  WRK-5508-COD-RETORNO    PIC  9(002)         VALUE ZEROS.
           05  WRK-5508-COD-SQL-ERRO   PIC  9(003)         VALUE ZEROS.
           05  WRK-5508-DESC-MSG       PIC  X(070)         VALUE SPACES.
           05  WRK-5508-DADOS-RETORNO.
             10 WRK-5508-IPSSOA-COPLT  PIC  X(070)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA FORMATACAO DE ERROS *'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-POOL7100.
           05 FILLER                   PIC  X(008)         VALUE
              '** ERRO '.
           05 WRK-OPERACAO             PIC  X(013)         VALUE SPACES.
           05 FILLER                   PIC  X(012)         VALUE
              ' DO ARQUIVO '.
           05 WRK-NOME-ARQ             PIC  X(008)         VALUE SPACES.
           05 FILLER                   PIC  X(017)         VALUE
              ' - FILE STATUS = '.
           05 WRK-FILE-STATUS          PIC  X(002)         VALUE SPACES.
           05 FILLER                   PIC  X(003)         VALUE
              ' **'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA RELATORIO *'.
      *----------------------------------------------------------------*

       01  CABEC1-REL1.
           05  FILLER                  PIC  X(001)         VALUE '1'.
           05  CB1-REL1-DIA            PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '/'.
           05  CB1-REL1-MES            PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '/'.
           05  CB1-REL1-ANO            PIC  9(004)         VALUE ZEROS.
           05  FILLER                  PIC  X(041)         VALUE SPACES.
           05  FILLER                  PIC  X(011)         VALUE
               'B A N C O'.
           05  FILLER                  PIC  X(017)         VALUE
               'B R A D E S C O'.
           05  FILLER                  PIC  X(045)         VALUE 'S/A'.
           05  CB1-REL1-HR             PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE ':'.
           05  CB1-REL1-MM             PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE ':'.
           05  CB1-REL1-SS             PIC  9(002)         VALUE ZEROS.

       01  CABEC2-REL1.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  FILLER                  PIC  X(041)         VALUE
               'GFCT1790'.
           05  FILLER                  PIC  X(083)         VALUE
               'GFCT - GESTAO, FLEXIBILIZACAO E COBRANCA DE TARIFAS'.
           05  FILLER                  PIC  X(004)         VALUE 'PAG.'.
           05  CB2-REL1-PAG            PIC  ZZ99           VALUE ZEROS.

       01  CABEC3-REL1.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  FILLER                  PIC  X(046)         VALUE SPACES.
           05  FILLER                  PIC  X(041)         VALUE
               'DEMONSTRATIVO DE FLEXIBILIZACOES A VENCER'.

       01  CABEC4-REL1.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  FILLER                  PIC  X(135)         VALUE
                                                               ALL '-'.

       01  CABEC5-REL1.
PROC       05  FILLER                  PIC  X(001)         VALUE SPACES.
PROC***    05  FILLER                  PIC  X(001)         VALUE '0'.
           05  FILLER                  PIC  X(037)         VALUE SPACES.
           05  FILLER                  PIC  X(025)         VALUE
               'DEPENDENCIA SOLICITANTE: '.
           05  CB5-REL1-DEP-SOLIC      PIC  99999.
           05  FILLER                  PIC  X(003)         VALUE ' - '.
           05  CB5-REL1-DEP-SOLIC-X    PIC  X(031)         VALUE SPACES.

       01  CABEC6-REL1.
PROC       05  FILLER                  PIC  X(001)         VALUE SPACES.
PROC***    05  FILLER                  PIC  X(001)         VALUE '0'.
           05  FILLER                  PIC  X(019)         VALUE
               '  FIM DE VIGENCIA: '.
           05  CB6-REL1-DIA-VIG        PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '/'.
           05  CB6-REL1-MES-VIG        PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '/'.
           05  CB6-REL1-ANO-VIG        PIC  9(004)         VALUE ZEROS.

       01  CABEC7-REL1.
PROC       05  FILLER                  PIC  X(001)         VALUE SPACES.
PROC***    05  FILLER                  PIC  X(001)         VALUE '-'.
           05  CB7-REL1-DESC-FLEX      PIC  X(030)         VALUE SPACES.

       01  CABEC8-REL1.
PROC       05  FILLER                  PIC  X(001)         VALUE SPACES.
PROC***    05  FILLER                  PIC  X(001)         VALUE '0'.
           05  FILLER                  PIC  X(009)         VALUE
               ' AGEN'.
           05  FILLER                  PIC  X(008)         VALUE
               'CONTA'.
           05  FILLER                  PIC  X(028)         VALUE 'NOME'.
           05  FILLER                  PIC  X(008)         VALUE
               'TARIFA'.
           05  FILLER                  PIC  X(020)         VALUE
               'DESCRICAO TARIFA'.
           05  FILLER                  PIC  X(016)         VALUE
               'PERC DESC'.
           05  FILLER                  PIC  X(009)         VALUE
               'VALOR'.
           05  FILLER                  PIC  X(010)         VALUE
               'FRQ MOTIVO'.

       01  CABEC9-REL1.
PROC       05  FILLER                  PIC  X(001)         VALUE SPACES.
PROC***    05  FILLER                  PIC  X(001)         VALUE '0'.
           05  FILLER                  PIC  X(021)         VALUE
               ' AGENCIA'.
           05  FILLER                  PIC  X(007)         VALUE
               'CONTA'.
           05  FILLER                  PIC  X(045)         VALUE 'NOME'.
           05  FILLER                  PIC  X(029)         VALUE
               'PERC DESC'.
           05  FILLER                  PIC  X(006)         VALUE
               'MOTIVO'.

       01  CABEC10-REL1.
PROC       05  FILLER                  PIC  X(001)         VALUE SPACES.
PROC***    05  FILLER                  PIC  X(001)         VALUE '0'.
           05  FILLER                  PIC  X(073)         VALUE
               ' IDENT. SEGMENTO'.
           05  FILLER                  PIC  X(029)         VALUE
               'PERC DESC'.
           05  FILLER                  PIC  X(006)         VALUE
               'MOTIVO'.

       01  CABEC11-REL1.
PROC       05  FILLER                  PIC  X(001)         VALUE SPACES.
PROC***    05  FILLER                  PIC  X(001)         VALUE '0'.
           05  CB11-TIPO-AGRUP         PIC  X(054)         VALUE SPACES.

       01  CABEC12-REL1.
PROC       05  FILLER                  PIC  X(001)         VALUE SPACES.
PROC***    05  FILLER                  PIC  X(001)         VALUE '0'.
           05  FILLER                  PIC  X(044)         VALUE
               'IDENT. AGRUPAMENTO'.
           05  FILLER                  PIC  X(008)         VALUE
               'TARIFA'.
           05  FILLER                  PIC  X(020)         VALUE
               'DESCRICAO TARIFA'.
           05  FILLER                  PIC  X(016)         VALUE
               'PERC DESC'.
           05  FILLER                  PIC  X(009)         VALUE
               'VALOR'.
           05  FILLER                  PIC  X(010)         VALUE
               'FRQ MOTIVO'.

PROC   01  CABEC13-REL1.
PROC       05  FILLER                  PIC  X(001)         VALUE SPACES.
PROC       05  CB13-BRANCOS            PIC  X(030)         VALUE SPACES.

       01  LINDET1-REL1.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
PROC***    05  LD1-REL1-AGE-IND        PIC  9(005)         VALUE ZEROS.
PROC       05  LD1-REL1-AGE-IND        PIC  ZZZZ9          VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
PROC       05  LD1-REL1-CTA-IND        PIC  Z.ZZZ.ZZ9      VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  LD1-REL1-NOME-IND       PIC  X(026)         VALUE SPACES.
           05  FILLER                  PIC  X(002)         VALUE SPACES.
PROC***    05  LD1-REL1-TARI-IND       PIC  9(005)         VALUE ZEROS.
PROC       05  LD1-REL1-TARI-IND       PIC  ZZZZ9          VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '-'.
           05  LD1-REL1-DESC-IND       PIC  X(020)         VALUE SPACES.
           05  FILLER                  PIC  X(002)         VALUE SPACES.
PROC       05  LD1-REL1-PERC-IND       PIC  ZZ9,9999       VALUE ZEROS.
           05  FILLER                  PIC  X(002)         VALUE SPACES.
PROC       05  LD1-REL1-VALOR-IND      PIC  ZZZ.ZZZ.ZZ9,99 VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
PROC***    05  LD1-REL1-FRQ-IND        PIC  9(003)         VALUE ZEROS.
PROC       05  LD1-REL1-FRQ-IND        PIC  ZZ9            VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  LD1-REL1-MOTVO-IND      PIC  X(030)         VALUE SPACES.

       01  LINDET2-REL1.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  FILLER                  PIC  X(002)         VALUE SPACES.
PROC***    05  LD2-REL1-AGE-VIND       PIC  99999.
PROC       05  LD2-REL1-AGE-VIND       PIC  ZZZZ9.
           05  FILLER                  PIC  X(010)         VALUE SPACES.
PROC***    05  LD2-REL1-CTA-VIND       PIC  9.999.999      VALUE ZEROS.
PROC       05  LD2-REL1-CTA-VIND       PIC  Z.ZZZ.ZZ9      VALUE ZEROS.
           05  FILLER                  PIC  X(002)         VALUE SPACES.
           05  LD2-REL1-NOME-VIND      PIC  X(031)         VALUE SPACES.
           05  FILLER                  PIC  X(014)         VALUE SPACES.
PROC***    05  LD2-REL1-PERC-VIND      PIC  999,9999       VALUE ZEROS.
PROC       05  LD2-REL1-PERC-VIND      PIC  ZZ9,9999       VALUE ZEROS.
           05  FILLER                  PIC  X(021)         VALUE SPACES.
           05  LD2-REL1-MOTVO-VIND     PIC  X(030)         VALUE SPACES.

       01  LINDET3-REL1.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
PROC       05  LD3-REL1-COD-VSEG       PIC  999            VALUE ZEROS.
PROC       05  FILLER                  PIC  X(001)         VALUE '-'.
           05  LD3-REL1-IDENT-VSEG     PIC  X(030)         VALUE SPACES.
PROC***    05  FILLER                  PIC  X(042)         VALUE SPACES.
PROC       05  FILLER                  PIC  X(038)         VALUE SPACES.
PROC***    05  LD3-REL1-PERC-VSEG      PIC  999,9999       VALUE ZEROS.
PROC       05  LD3-REL1-PERC-VSEG      PIC  ZZ9,9999       VALUE ZEROS.
           05  FILLER                  PIC  X(021)         VALUE SPACES.
           05  LD3-REL1-MOTVO-VSEG     PIC  X(030)         VALUE SPACES.

       01  LINDET4-REL1.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  LD4-REL1-DADOS          PIC  X(042)         VALUE SPACES.
           05  FILLER                  PIC  X(002)         VALUE SPACES.
PROC***    05  LD4-REL1-TARI           PIC  9(005)         VALUE ZEROS.
PROC       05  LD4-REL1-TARI           PIC  ZZZZ9          VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '-'.
           05  LD4-REL1-DESC           PIC  X(020)         VALUE SPACES.
           05  FILLER                  PIC  X(002)         VALUE SPACES.
PROC       05  LD4-REL1-PERC           PIC  ZZ9,9999       VALUE ZEROS.
           05  FILLER                  PIC  X(002)         VALUE SPACES.
PROC       05  LD4-REL1-VALOR          PIC  ZZZ.ZZZ.ZZ9,99 VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
PROC***    05  LD4-REL1-FRQ            PIC  9(003)         VALUE ZEROS.
PROC       05  LD4-REL1-FRQ            PIC  ZZ9            VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  LD4-REL1-MOTVO          PIC  X(030)         VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA VARIAVEIS DE NULO *'.
      *----------------------------------------------------------------*

       01  WRK-IDEPDC-NULL             PIC S9(04) COMP     VALUE ZEROS.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA POOL7100 *'.
      *----------------------------------------------------------------*

       COPY POL7100C.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA I#PLCCEA *'.
      *----------------------------------------------------------------*

       COPY I#PLCCEA.


      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA DB2 *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0H5
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0F3
           END-EXEC.

           EXEC SQL
               INCLUDE MESUB062
           END-EXEC.

           EXEC SQL
               INCLUDE MESUB010
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0D8
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB089
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* FIM DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      ******************************************************************
      * EXECUTA AS ROTINAS PRINCIPAIS DO PROCESSAMENTO.                *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-VERIFICAR-VAZIO.

           PERFORM 3000-PROCESSAR      UNTIL
               WRK-FS-FLEXVENC         EQUAL '10'.

           PERFORM 4000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * INICIALIZA PROCESSAMENTO E ABRE OS ARQUIVOS.                   *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL1050'.

           OPEN INPUT  FLEXVENC
                OUTPUT RELATO.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

           PERFORM 1200-OBTER-DATA-SIST.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * EXECUTA TESTE DE FILE STATUS DOS ARQUIVOS.                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-FLEXVENC.

           PERFORM 1120-TESTAR-FS-RELATO.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TESTE DE FILE STATUS DO ARQUIVO FLEXVENC.                      *
      ******************************************************************
      *----------------------------------------------------------------*
       1110-TESTAR-FS-FLEXVENC         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-FLEXVENC         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'FLEXVENC'         TO WRK-NOME-ARQ
               MOVE WRK-FS-FLEXVENC    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-POOL7100  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TESTE DE FILE STATUS DO ARQUIVO RELATO.                        *
      ******************************************************************
      *----------------------------------------------------------------*
       1120-TESTAR-FS-RELATO           SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-RELATO           NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'RELATO'           TO WRK-NOME-ARQ
               MOVE WRK-FS-RELATO      TO WRK-FILE-STATUS
               MOVE WRK-ERRO-POOL7100  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * OBTER DATA E HORA DO SISTEMA.                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       1200-OBTER-DATA-SIST            SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL7600'             USING WRK-DATA-HORA.

           MOVE WRK-DIA                TO CB1-REL1-DIA.
           MOVE WRK-MES                TO CB1-REL1-MES.
           MOVE WRK-ANO                TO CB1-REL1-ANO.
           MOVE WRK-HOR                TO CB1-REL1-HR.
           MOVE WRK-MIN                TO CB1-REL1-MM.
           MOVE WRK-SEG                TO CB1-REL1-SS.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR SE EXISTEM REGISTROS NO ARQUIVO FLEXVENC.            *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-LER-FLEXVENC.

           IF  WRK-FS-FLEXVENC         EQUAL '10'
               DISPLAY '*------------------------------------------*'
               DISPLAY '*             PROGRAMA GFCT1790            *'
               DISPLAY '*             -----------------            *'
               DISPLAY '*                                          *'
               DISPLAY '*     ARQUIVO DE FLEXIBILIZACAO VAZIO      *'
               DISPLAY '*------------------------------------------*'
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * LEITURA DO ARQUIVO FLEXVENC.                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-LER-FLEXVENC               SECTION.
      *----------------------------------------------------------------*

           READ FLEXVENC.

           IF  WRK-FS-FLEXVENC         EQUAL '10'
               GO TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1110-TESTAR-FS-FLEXVENC.

           ADD 1                       TO ACU-LIDOS.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAMENTO PRINCIPAL.                                       *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           IF  GFCTT9-DEPEND-SOL       NOT EQUAL WRK-CDEPDC-ANT
               MOVE GFCTT9-DEPEND-SOL  TO CDEPDC OF MESUB010
               PERFORM 3100-PESQUISAR-DESC-DEPEND
               MOVE GFCTT9-DEPEND-SOL  TO CB5-REL1-DEP-SOLIC
               MOVE IDEPDC             OF MESUB010
                                       TO CB5-REL1-DEP-SOLIC-X
               PERFORM 3200-EMITIR-CABECLHOS1
               MOVE GFCTT9-DFIM-FLEXZ  TO WRK-DFIM-ANT
           END-IF.

           IF  GFCTT9-DFIM-FLEXZ       NOT EQUAL WRK-DFIM-ANT
               PERFORM 3200-EMITIR-CABECLHOS1
           END-IF.

           PERFORM 3300-PESQUISAR-DESC-MOTIVO.

           PERFORM 3400-EMITIR-CABECLHOS2.

           MOVE GFCTT9-DEPEND-SOL      TO WRK-CDEPDC-ANT.

           PERFORM 2100-LER-FLEXVENC.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PESQUISA DESCRICAO DA DEPENDENCIA                              *
      ******************************************************************
      *----------------------------------------------------------------*
       3100-PESQUISAR-DESC-DEPEND      SECTION.
      *----------------------------------------------------------------*

           MOVE 237                    TO CEMPR-INC OF MESUB010.

           MOVE ZEROS                  TO WRK-IDEPDC-NULL.

           EXEC SQL
             SELECT
                   IDEPDC
             INTO
                   :MESUB010.IDEPDC
                     :WRK-IDEPDC-NULL
             FROM   DB2PRD.DEPDC_BDSCO_ONLINE
             WHERE
                   CEMPR_INC          = :MESUB010.CEMPR-INC          AND
                   CDEPDC             = :MESUB010.CDEPDC
           END-EXEC.

300808     IF (SQLCODE                   NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                  EQUAL 'W'      )
               MOVE 'DB2'                TO ERR-TIPO-ACESSO
               MOVE 'DEPDC_BDSCO_ONLINE' TO ERR-DBD-TAB
               MOVE 'SELECT  '           TO ERR-FUN-COMANDO
               MOVE  SQLCODE             TO ERR-SQL-CODE
               MOVE  '0020'              TO ERR-LOCAL
               MOVE  SPACES              TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

300808     IF  SQLCODE                   EQUAL +100
300808         MOVE 'DEPARTAMENTO NAO ENCONTRADO' TO IDEPDC
300808                                            OF MESUB010
300808     END-IF.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * IMPRIME CABECALHOS PRINCIPAIS DO RELATORIO RELATO.            *
      ******************************************************************
      *----------------------------------------------------------------*
       3200-EMITIR-CABECLHOS1          SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           IF  (ACU-LINHAS             GREATER 59) OR
               (GFCTT9-DEPEND-SOL      NOT EQUAL WRK-CDEPDC-ANT) OR
               (GFCTT9-DFIM-FLEXZ      NOT EQUAL WRK-DFIM-ANT)
               MOVE ZEROS              TO ACU-LINHAS
               ADD 1                   TO ACU-PAG
               MOVE ACU-PAG            TO CB2-REL1-PAG
               WRITE FD-RELATO         FROM CABEC1-REL1
               PERFORM 1120-TESTAR-FS-RELATO
               WRITE FD-RELATO         FROM CABEC2-REL1
               PERFORM 1120-TESTAR-FS-RELATO
               WRITE FD-RELATO         FROM CABEC3-REL1
               PERFORM 1120-TESTAR-FS-RELATO
               WRITE FD-RELATO         FROM CABEC4-REL1
               PERFORM 1120-TESTAR-FS-RELATO
               WRITE FD-RELATO         FROM CABEC5-REL1
               PERFORM 1120-TESTAR-FS-RELATO
PROC           WRITE FD-RELATO         FROM CABEC13-REL1
PROC           PERFORM 1120-TESTAR-FS-RELATO
               MOVE GFCTT9-DFIM-FLEXZ  TO WRK-DATA-AUX
               MOVE WRK-DIA-AUX        TO CB6-REL1-DIA-VIG
               MOVE WRK-MES-AUX        TO CB6-REL1-MES-VIG
               MOVE WRK-ANO-AUX        TO CB6-REL1-ANO-VIG
               WRITE FD-RELATO         FROM CABEC6-REL1
               PERFORM 1120-TESTAR-FS-RELATO
PROC***        WRITE FD-RELATO         FROM CABEC13-REL1
PROC***        PERFORM 1120-TESTAR-FS-RELATO
PROC***        ADD 8                   TO ACU-LINHAS
PROC           ADD 7                   TO ACU-LINHAS
           END-IF.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PESQUISA DESCRICAO DO MOTIVO.                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       3300-PESQUISAR-DESC-MOTIVO      SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTT9-CTPO-OPER-MOTVO TO CTPO-OPER-MOTVO OF GFCTB089.
           MOVE GFCTT9-CMOTVO-JUSTF    TO CMOTVO-JUSTF    OF GFCTB089.

           EXEC SQL
             SELECT
                   RMOTVO_JUSTF
             INTO
                   :GFCTB089.RMOTVO-JUSTF
             FROM   DB2PRD.MOTVO_JUSTF_OPER
             WHERE
                   CTPO_OPER_MOTVO    = :GFCTB089.CTPO-OPER-MOTVO    AND
                   CMOTVO_JUSTF       = :GFCTB089.CMOTVO-JUSTF
           END-EXEC.

300808     IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'MOTVO_JUSTF_OPER' TO ERR-DBD-TAB
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE  '0030'            TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

300808     IF  SQLCODE                   EQUAL +100
300808         MOVE 'DESCRICAO MOTIVO NAO ENCONTRADO'
300808                                            TO RMOTVO-JUSTF
300808                                            OF GFCTB089
300808     END-IF.

      *----------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * EMITE CABECALHO DE TIPO DE FLEXIBILIZACAO                      *
      ******************************************************************
      *----------------------------------------------------------------*
       3400-EMITIR-CABECLHOS2          SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           EVALUATE GFCTT9-TIPO-FLEX
               WHEN 01
                   IF  GFCTT9-TIPO-FLEX   NOT EQUAL WRK-TPO-FLEX-ANT OR
                       GFCTT9-DEPEND-SOL  NOT EQUAL WRK-CDEPDC-ANT OR
                       GFCTT9-DFIM-FLEXZ  NOT EQUAL WRK-DFIM-ANT
PROC                   WRITE FD-RELATO    FROM CABEC13-REL1
PROC                   PERFORM 1120-TESTAR-FS-RELATO
                       MOVE 'FLEXIBILIZACAO INDIVIDUAL'
                                          TO CB7-REL1-DESC-FLEX
                       WRITE FD-RELATO    FROM CABEC7-REL1
                       PERFORM 1120-TESTAR-FS-RELATO
PROC                   WRITE FD-RELATO    FROM CABEC13-REL1
PROC                   PERFORM 1120-TESTAR-FS-RELATO
PROC                   ADD 3              TO ACU-LINHAS
                   END-IF
                   PERFORM 3410-FLEX-INDIVIDUAL
               WHEN 02
                   IF  GFCTT9-TIPO-FLEX   NOT EQUAL WRK-TPO-FLEX-ANT OR
                       GFCTT9-DEPEND-SOL  NOT EQUAL WRK-CDEPDC-ANT OR
                       GFCTT9-DFIM-FLEXZ  NOT EQUAL WRK-DFIM-ANT
PROC                   WRITE FD-RELATO    FROM CABEC13-REL1
PROC                   PERFORM 1120-TESTAR-FS-RELATO
                       MOVE 'FLEXIBILIZACAO VIP INDIVIDUAL'
                                          TO CB7-REL1-DESC-FLEX
                       WRITE FD-RELATO    FROM CABEC7-REL1
                       PERFORM 1120-TESTAR-FS-RELATO
PROC                   WRITE FD-RELATO    FROM CABEC13-REL1
PROC                   PERFORM 1120-TESTAR-FS-RELATO
PROC                   ADD 3              TO ACU-LINHAS
                   END-IF
                   PERFORM 3420-FLEX-VIP-INDIVIDUAL
               WHEN 03
                   IF  GFCTT9-TIPO-FLEX   NOT EQUAL WRK-TPO-FLEX-ANT OR
                       GFCTT9-DEPEND-SOL  NOT EQUAL WRK-CDEPDC-ANT OR
                       GFCTT9-DFIM-FLEXZ  NOT EQUAL WRK-DFIM-ANT
PROC                   WRITE FD-RELATO    FROM CABEC13-REL1
PROC                   PERFORM 1120-TESTAR-FS-RELATO
                       MOVE 'FLEXIBILIZACAO VIP SEGMENTO'
                                          TO CB7-REL1-DESC-FLEX
                       WRITE FD-RELATO    FROM CABEC7-REL1
                       PERFORM 1120-TESTAR-FS-RELATO
PROC                   WRITE FD-RELATO    FROM CABEC13-REL1
PROC                   PERFORM 1120-TESTAR-FS-RELATO
PROC                   ADD 3              TO ACU-LINHAS
                   END-IF
                   PERFORM 3430-FLEX-VIP-SEGMENTO
               WHEN 04
                   IF  GFCTT9-TIPO-FLEX   NOT EQUAL WRK-TPO-FLEX-ANT OR
                       GFCTT9-DEPEND-SOL  NOT EQUAL WRK-CDEPDC-ANT OR
                       GFCTT9-DFIM-FLEXZ  NOT EQUAL WRK-DFIM-ANT
PROC                   WRITE FD-RELATO    FROM CABEC13-REL1
PROC                   PERFORM 1120-TESTAR-FS-RELATO
                       MOVE 'FLEXIBILIZACAO AGRUPADA'
                                          TO CB7-REL1-DESC-FLEX
                       WRITE FD-RELATO    FROM CABEC7-REL1
                       PERFORM 1120-TESTAR-FS-RELATO
PROC                   WRITE FD-RELATO    FROM CABEC13-REL1
PROC                   PERFORM 1120-TESTAR-FS-RELATO
PROC                   ADD 3              TO ACU-LINHAS
                   END-IF
                   PERFORM 3440-FLEX-AGRUPADA
           END-EVALUATE.

           MOVE GFCTT9-TIPO-FLEX          TO WRK-TPO-FLEX-ANT.
           MOVE GFCTT9-DFIM-FLEXZ         TO WRK-DFIM-ANT.

      *----------------------------------------------------------------*
       3400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TRATA FLEXIBILIZACAO INDIVIDUAL                                *
      ******************************************************************
      *----------------------------------------------------------------*
       3410-FLEX-INDIVIDUAL            SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.
           PERFORM 3411-PESQUISAR-DESC-TARIF.

           IF (GFCTT9-AGENCIA          NOT EQUAL WRK-AGENCIA-ANT AND
               GFCTT9-CONTA            NOT EQUAL WRK-CONTA-ANT)
               MOVE GFCTT9-AGENCIA     TO WRK-5508-CJUNC-DEPDC
               MOVE GFCTT9-CONTA       TO WRK-5508-CCTA-CLI
               PERFORM 3412-PESQUISAR-NOME
           END-IF.

           IF  ACU-LINHAS              GREATER 59 OR
              (GFCTT9-AGENCIA          NOT EQUAL WRK-AGENCIA-ANT AND
               GFCTT9-CONTA            NOT EQUAL WRK-CONTA-ANT) OR
               GFCTT9-DFIM-FLEXZ       NOT EQUAL WRK-DFIM-ANT
               IF  ACU-LINHAS          GREATER 59
                   MOVE ZEROS          TO ACU-LINHAS
                   ADD 1               TO ACU-PAG
                   MOVE ACU-PAG        TO CB2-REL1-PAG
                   WRITE FD-RELATO     FROM CABEC1-REL1
                   PERFORM 1120-TESTAR-FS-RELATO
                   WRITE FD-RELATO     FROM CABEC2-REL1
                   PERFORM 1120-TESTAR-FS-RELATO
                   WRITE FD-RELATO     FROM CABEC3-REL1
                   PERFORM 1120-TESTAR-FS-RELATO
                   WRITE FD-RELATO     FROM CABEC4-REL1
                   PERFORM 1120-TESTAR-FS-RELATO
                   ADD 4               TO ACU-LINHAS
               END-IF
               WRITE FD-RELATO         FROM CABEC8-REL1
               PERFORM 1120-TESTAR-FS-RELATO
PROC           WRITE FD-RELATO         FROM CABEC13-REL1
PROC           PERFORM 1120-TESTAR-FS-RELATO
               ADD 2                   TO ACU-LINHAS
               MOVE GFCTT9-AGENCIA     TO WRK-AGENCIA-ANT
               MOVE GFCTT9-CONTA       TO WRK-CONTA-ANT
           END-IF.

           MOVE GFCTT9-AGENCIA         TO LD1-REL1-AGE-IND.
           MOVE GFCTT9-CONTA           TO LD1-REL1-CTA-IND.
           MOVE WRK-5508-IPSSOA-COPLT  TO LD1-REL1-NOME-IND.
           MOVE GFCTT9-CSERVC-TARIF    TO LD1-REL1-TARI-IND.
           MOVE RSERVC-TARIF-REDZD     OF GFCTB0D8
                                       TO LD1-REL1-DESC-IND.
           MOVE GFCTT9-PDESC-FLEXZ     TO LD1-REL1-PERC-IND.
           MOVE GFCTT9-VNEGOC-FLEXZ    TO LD1-REL1-VALOR-IND.
           MOVE GFCTT9-QFRANQ-FLEXZ    TO LD1-REL1-FRQ-IND.
           MOVE RMOTVO-JUSTF           OF GFCTB089
                                       TO LD1-REL1-MOTVO-IND.

           WRITE FD-RELATO             FROM LINDET1-REL1.
           PERFORM 1120-TESTAR-FS-RELATO.
           ADD 1                       TO ACU-LINHAS.

      *----------------------------------------------------------------*
       3410-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PESQUISA DESCRICAO DA TARIFA                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       3411-PESQUISAR-DESC-TARIF       SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTT9-CSERVC-TARIF    TO CSERVC-TARIF OF GFCTB0D8

           EXEC SQL
             SELECT
                   RSERVC_TARIF_REDZD
             INTO
                   :GFCTB0D8.RSERVC-TARIF-REDZD
             FROM   DB2PRD.SERVC_TARIF_PRINC
             WHERE
                   CSERVC_TARIF       = :GFCTB0D8.CSERVC-TARIF
           END-EXEC.

300808     IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W'      )
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'SERVC_TARIF_PRINC' TO ERR-DBD-TAB
               MOVE 'SELECT  '          TO ERR-FUN-COMANDO
               MOVE  SQLCODE            TO ERR-SQL-CODE
               MOVE  '0040'             TO ERR-LOCAL
               MOVE  SPACES             TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

300808     IF  SQLCODE                   EQUAL +100
300808         MOVE 'DESCRICAO DA TARIFA NAO ENCONTRADA'
300808                                            TO RSERVC-TARIF-REDZD
300808                                            OF GFCTB0D8
300808     END-IF.

      *----------------------------------------------------------------*
       3411-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PESQUISA NOME.                                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       3412-PESQUISAR-NOME             SECTION.
      *----------------------------------------------------------------*

           MOVE 'B'                    TO WRK-5508-AMBIENTE.

           MOVE 'GFCT5508'             TO WRK-MODULO.

           CALL 'POOL0080'             USING WRK-MODULO
                                             WRK-5508-AREA-ENTRADA
                                             WRK-5508-AREA-SAIDA
                                             WRK-IO-PCB
                                             WRK-ALT-PCB.

300808     IF  RETURN-CODE             NOT EQUAL ZEROS
300808         MOVE 'DESCRICAO NOME DO CLIENTE NAO ENCONTRADO' TO
300808                                            WRK-5508-IPSSOA-COPLT
300808**       PERFORM 9999-PROCESSAR-ROTINA-ERRO
300808     END-IF.

300808     IF  WRK-5508-COD-RETORNO    EQUAL 02
300808         MOVE WRK-5508-DESC-MSG  TO WRK-5508-IPSSOA-COPLT
300808     END-IF.

300808**   IF  WRK-5508-COD-RETORNO    EQUAL 02 OR 99
300808     IF  WRK-5508-COD-RETORNO    EQUAL 99
               MOVE WRK-5508-DESC-MSG  TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       3412-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TRATA TIPO FLEXIBILIZACAO VIP INDIVIDUAL                       *
      ******************************************************************
      *----------------------------------------------------------------*
       3420-FLEX-VIP-INDIVIDUAL        SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.
           MOVE GFCTT9-AG-VIP          TO WRK-5508-CJUNC-DEPDC.
           MOVE GFCTT9-CTA-VIP         TO WRK-5508-CCTA-CLI.
           PERFORM 3412-PESQUISAR-NOME.

           IF (GFCTT9-AG-VIP           NOT EQUAL WRK-AG-VIP-ANT  AND
               GFCTT9-CTA-VIP          NOT EQUAL WRK-CTA-VIP-ANT) OR
               GFCTT9-DFIM-FLEXZ       NOT EQUAL WRK-DFIM-ANT OR
               ACU-LINHAS              GREATER 59
               IF  ACU-LINHAS          GREATER 59
                   MOVE ZEROS          TO ACU-LINHAS
                   ADD 1               TO ACU-PAG
                   MOVE ACU-PAG        TO CB2-REL1-PAG
                   WRITE FD-RELATO     FROM CABEC1-REL1
                   PERFORM 1120-TESTAR-FS-RELATO
                   WRITE FD-RELATO     FROM CABEC2-REL1
                   PERFORM 1120-TESTAR-FS-RELATO
                   WRITE FD-RELATO     FROM CABEC3-REL1
                   PERFORM 1120-TESTAR-FS-RELATO
                   WRITE FD-RELATO     FROM CABEC4-REL1
                   PERFORM 1120-TESTAR-FS-RELATO
                   ADD 4               TO ACU-LINHAS
               END-IF
               WRITE FD-RELATO         FROM CABEC9-REL1
               PERFORM 1120-TESTAR-FS-RELATO
PROC           WRITE FD-RELATO         FROM CABEC13-REL1
PROC           PERFORM 1120-TESTAR-FS-RELATO
               ADD 2                   TO ACU-LINHAS
               MOVE GFCTT9-AG-VIP      TO WRK-AG-VIP-ANT
               MOVE GFCTT9-CTA-VIP     TO WRK-CTA-VIP-ANT
           END-IF.

           MOVE GFCTT9-AG-VIP          TO LD2-REL1-AGE-VIND.
           MOVE GFCTT9-CTA-VIP         TO LD2-REL1-CTA-VIND.
           MOVE WRK-5508-IPSSOA-COPLT  TO LD2-REL1-NOME-VIND.
           MOVE GFCTT9-PDESC-FLEXZ     TO LD2-REL1-PERC-VIND.
           MOVE RMOTVO-JUSTF           OF GFCTB089
                                       TO LD2-REL1-MOTVO-VIND.

           WRITE FD-RELATO             FROM LINDET2-REL1.
           PERFORM 1120-TESTAR-FS-RELATO.
           ADD 1                       TO ACU-LINHAS.

      *----------------------------------------------------------------*
       3420-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TRATA TIPO FLEXIBILIZACAO VIP SEGMENTO                         *
      ******************************************************************
      *----------------------------------------------------------------*
       3430-FLEX-VIP-SEGMENTO          SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO          TO WRK-OPERACAO.
           MOVE GFCTT9-SEGMTO         TO CSGMTO-GSTAO-TARIF OF GFCTB0H5

           PERFORM 3431-PESQUISAR-SEGMENTO.

           IF  GFCTT9-SEGMTO          NOT EQUAL WRK-SEGMTO-ANT OR
               ACU-LINHAS             GREATER 59 OR
               GFCTT9-DFIM-FLEXZ      NOT EQUAL WRK-DFIM-ANT
               IF  ACU-LINHAS         GREATER 59
                   MOVE ZEROS         TO ACU-LINHAS
                   ADD 1              TO ACU-PAG
                   MOVE ACU-PAG       TO CB2-REL1-PAG
                   WRITE FD-RELATO    FROM CABEC1-REL1
                   PERFORM 1120-TESTAR-FS-RELATO
                   WRITE FD-RELATO    FROM CABEC2-REL1
                   PERFORM 1120-TESTAR-FS-RELATO
                   WRITE FD-RELATO    FROM CABEC3-REL1
                   PERFORM 1120-TESTAR-FS-RELATO
                   WRITE FD-RELATO    FROM CABEC4-REL1
                   PERFORM 1120-TESTAR-FS-RELATO
                   ADD 4              TO ACU-LINHAS
               END-IF
               WRITE FD-RELATO        FROM CABEC10-REL1
               PERFORM 1120-TESTAR-FS-RELATO
PROC           WRITE FD-RELATO         FROM CABEC13-REL1
PROC           PERFORM 1120-TESTAR-FS-RELATO
               ADD 2                  TO ACU-LINHAS
               MOVE GFCTT9-SEGMTO     TO WRK-SEGMTO-ANT
           END-IF.

PROC       MOVE GFCTT9-SEGMTO         TO LD3-REL1-COD-VSEG.

           MOVE ISGMTO-GSTAO-TARIF    OF GFCTB0H5
                                      TO LD3-REL1-IDENT-VSEG.
           MOVE GFCTT9-PDESC-FLEXZ    TO LD3-REL1-PERC-VSEG.
           MOVE RMOTVO-JUSTF          OF GFCTB089
                                      TO LD3-REL1-MOTVO-VSEG.
           WRITE FD-RELATO            FROM LINDET3-REL1.
           MOVE WRK-GRAVACAO          TO WRK-OPERACAO.
           PERFORM 1120-TESTAR-FS-RELATO.
           ADD 1                      TO ACU-LINHAS.

      *----------------------------------------------------------------*
       3430-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PESQUISA SEGMENTO.                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       3431-PESQUISAR-SEGMENTO         SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             SELECT
                   ISGMTO_GSTAO_TARIF
             INTO
                   :GFCTB0H5.ISGMTO-GSTAO-TARIF
             FROM   DB2PRD.TTPO_SGMTO_GSTAO
             WHERE
                   CSGMTO_GSTAO_TARIF = :GFCTB0H5.CSGMTO-GSTAO-TARIF
           END-EXEC.

300808     IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'TTPO_SGMTO_GSTAO' TO ERR-DBD-TAB
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE  '0050'            TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

300808     IF  SQLCODE                   EQUAL +100
300808         MOVE 'DESCRICAO DO SEGMENTO NAO ENCONTRADA'
300808                                            TO ISGMTO-GSTAO-TARIF
300808                                            OF GFCTB0H5
300808     END-IF.

      *----------------------------------------------------------------*
       3431-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TRATA FLEXIBILIZACAO AGRUPADA.                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       3440-FLEX-AGRUPADA              SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.
           PERFORM 3411-PESQUISAR-DESC-TARIF.

           IF  ACU-LINHAS              NOT GREATER 59
               PERFORM 3441-IMPRIMIR-CABEC-AGRUP
           END-IF.

           IF  GFCTT9-CAGPTO-CTA       NOT EQUAL WRK-AGRUP-ANT OR
               ACU-LINHAS              GREATER 59 OR
               GFCTT9-DFIM-FLEXZ       NOT EQUAL WRK-DFIM-ANT
               IF  ACU-LINHAS          GREATER 59
                   MOVE ZEROS          TO ACU-LINHAS
                   ADD 1               TO ACU-PAG
                   MOVE ACU-PAG        TO CB2-REL1-PAG
                   WRITE FD-RELATO     FROM CABEC1-REL1
                   PERFORM 1120-TESTAR-FS-RELATO
                   WRITE FD-RELATO     FROM CABEC2-REL1
                   PERFORM 1120-TESTAR-FS-RELATO
                   WRITE FD-RELATO     FROM CABEC3-REL1
                   PERFORM 1120-TESTAR-FS-RELATO
                   WRITE FD-RELATO     FROM CABEC4-REL1
                   PERFORM 1120-TESTAR-FS-RELATO
                   ADD 4               TO ACU-LINHAS
               END-IF
               WRITE FD-RELATO         FROM CABEC12-REL1
               PERFORM 1120-TESTAR-FS-RELATO
PROC           WRITE FD-RELATO         FROM CABEC13-REL1
PROC           PERFORM 1120-TESTAR-FS-RELATO
               MOVE GFCTT9-CAGPTO-CTA  TO WRK-AGRUP-ANT
               ADD 2                   TO ACU-LINHAS
           END-IF.

           PERFORM 3444-IMPRIMIR-LINDET.

      *----------------------------------------------------------------*
       3440-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * IMPRIME CABEC DE AGRUPAMENTO                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       3441-IMPRIMIR-CABEC-AGRUP       SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           IF  GFCTT9-CAGPTO-CTA       NOT EQUAL WRK-AGRUP-ANT
               PERFORM 3442-PESQUISAR-AGRUPAMENTO
               EVALUATE GFCTT9-CAGPTO-CTA
                   WHEN 004
                       MOVE SPACES     TO CB11-TIPO-AGRUP
                       STRING 'TIPO AGRUPAMENTO: 004 - ' RAGPTO-CTA
                                       OF GFCTB0F3 DELIMITED BY '  '
                                       INTO CB11-TIPO-AGRUP
300808             WHEN 007
300808                 MOVE SPACES     TO CB11-TIPO-AGRUP
300808                 STRING 'TIPO AGRUPAMENTO: 007 - ' RAGPTO-CTA
300808                                 OF GFCTB0F3 DELIMITED BY '  '
300808                                 INTO CB11-TIPO-AGRUP
                   WHEN 009
                       MOVE SPACES     TO CB11-TIPO-AGRUP
                       STRING 'TIPO AGRUPAMENTO: 009 - ' RAGPTO-CTA
                                       OF GFCTB0F3 DELIMITED BY '  '
                                       INTO CB11-TIPO-AGRUP
                   WHEN 014
                       MOVE SPACES     TO CB11-TIPO-AGRUP
                       STRING 'TIPO AGRUPAMENTO: 014 - ' RAGPTO-CTA
                                       OF GFCTB0F3 DELIMITED BY '  '
                                       INTO CB11-TIPO-AGRUP
                   WHEN 015
                       MOVE SPACES     TO CB11-TIPO-AGRUP
                       STRING 'TIPO AGRUPAMENTO: 015 - ' RAGPTO-CTA
                                       OF GFCTB0F3 DELIMITED BY '  '
                                       INTO CB11-TIPO-AGRUP
                   WHEN 016
                       MOVE SPACES     TO CB11-TIPO-AGRUP
                       STRING 'TIPO AGRUPAMENTO: 016 - ' RAGPTO-CTA
                                       OF GFCTB0F3 DELIMITED BY '  '
                                       INTO CB11-TIPO-AGRUP
                   WHEN 017
                       MOVE SPACES     TO CB11-TIPO-AGRUP
                       STRING 'TIPO AGRUPAMENTO: 017 - ' RAGPTO-CTA
                                       OF GFCTB0F3 DELIMITED BY '  '
                                       INTO CB11-TIPO-AGRUP
                   WHEN 018
                       MOVE SPACES     TO CB11-TIPO-AGRUP
                       STRING 'TIPO AGRUPAMENTO: 018 - ' RAGPTO-CTA
                                       OF GFCTB0F3 DELIMITED BY '  '
                                       INTO CB11-TIPO-AGRUP
                   WHEN 019
                       MOVE SPACES     TO CB11-TIPO-AGRUP
                       STRING 'TIPO AGRUPAMENTO: 019 - ' RAGPTO-CTA
                                       OF GFCTB0F3 DELIMITED BY '  '
                                       INTO CB11-TIPO-AGRUP
LFM                WHEN 020
LFM                    MOVE SPACES     TO CB11-TIPO-AGRUP
LFM                    STRING 'TIPO AGRUPAMENTO: 020 - ' RAGPTO-CTA
LFM                                    OF GFCTB0F3 DELIMITED BY '  '
LFM                                    INTO CB11-TIPO-AGRUP
LFM                WHEN 026
LFM                    MOVE SPACES     TO CB11-TIPO-AGRUP
LFM                    STRING 'TIPO AGRUPAMENTO: 026 - ' RAGPTO-CTA
LFM                                    OF GFCTB0F3 DELIMITED BY '  '
LFM                                    INTO CB11-TIPO-AGRUP
300808             WHEN OTHER
300808                 MOVE SPACES     TO CB11-TIPO-AGRUP
300808                 STRING 'TIPO AGRUPAMENTO: ' GFCTT9-CAGPTO-CTA
300808                        ' - '
300808                        RAGPTO-CTA
300808                                 OF GFCTB0F3 DELIMITED BY '  '
300808                                 INTO CB11-TIPO-AGRUP
               END-EVALUATE

PROC           WRITE FD-RELATO         FROM CABEC13-REL1
PROC           PERFORM 1120-TESTAR-FS-RELATO
               WRITE FD-RELATO         FROM CABEC11-REL1
               PERFORM 1120-TESTAR-FS-RELATO
PROC           WRITE FD-RELATO         FROM CABEC13-REL1
PROC           PERFORM 1120-TESTAR-FS-RELATO
PROC***        ADD 2                   TO ACU-LINHAS
PROC           ADD 3                   TO ACU-LINHAS
           END-IF.

      *----------------------------------------------------------------*
       3441-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PESQUISA AGRUPAMENTO.                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       3442-PESQUISAR-AGRUPAMENTO      SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTT9-CAGPTO-CTA      TO CAGPTO-CTA OF GFCTB0F3.

           EXEC SQL
             SELECT
                   RAGPTO_CTA
             INTO
                   :GFCTB0F3.RAGPTO-CTA
             FROM   DB2PRD.TPO_AGPTO_CTA
             WHERE
                   CAGPTO_CTA         = :GFCTB0F3.CAGPTO-CTA
           END-EXEC.

380808     IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'TPO_AGPTO_CTA   ' TO ERR-DBD-TAB
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE  '0060'            TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

300808     IF  SQLCODE                   EQUAL +100
300808         MOVE 'DESCRICAO DO AGRUPAMENTO NAO ENCONTRADA'
300808                                            TO RAGPTO-CTA
300808                                            OF GFCTB0F3
300808     END-IF.

      *----------------------------------------------------------------*
       3442-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PESQUISA PAB.                                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       3443-PESQUISAR-PAB              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             SELECT
                   IPOSTO_SERVC
             INTO
                   :MESUB062.IPOSTO-SERVC
             FROM   DB2PRD.POSTO_BCRIO_SERVC
             WHERE
                   CDEPDC             = :MESUB062.CDEPDC
PROC         AND   CPOSTO_SERVC       = :MESUB062.CPOSTO-SERVC
           END-EXEC.

300808     IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W'      )
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'POSTO_BCRIO_SERVC' TO ERR-DBD-TAB
               MOVE 'SELECT  '          TO ERR-FUN-COMANDO
               MOVE  SQLCODE            TO ERR-SQL-CODE
               MOVE  '0070'             TO ERR-LOCAL
               MOVE  SPACES             TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

300808     IF  SQLCODE                   EQUAL +100
300808         MOVE 'DESCRICAO DO POSTO NAO ENCONTRADA'
300808                                            TO IPOSTO-SERVC
300808                                            OF MESUB062
300808     END-IF.

      *----------------------------------------------------------------*
       3443-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * IMPRIME LIDET DE AGRUPAMENTO.                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       3444-IMPRIMIR-LINDET            SECTION.
      *----------------------------------------------------------------*

           EVALUATE GFCTT9-CAGPTO-CTA

             WHEN 004
               MOVE GFCTT9-CCGC-AGP04      TO WRK-CPF
               MOVE GFCTT9-CFLIAL-AGP04    TO WRK-FILIAL
               MOVE GFCTT9-CCTRL-AGP04     TO WRK-CONTROLE
               MOVE WRK-CGC-AUX            TO LD4-REL1-DADOS
             WHEN 009
               MOVE GFCTT9-CSGMTO-AGP09    TO WRK-NUMERO
               IF  GFCTT9-CSGMTO-AGP09     NOT EQUAL WRK-SEGMTO09-ANT
                   MOVE GFCTT9-CSGMTO-AGP09
                                           TO CSGMTO-GSTAO-TARIF
                                           OF GFCTB0H5
                                           WRK-SEGMTO09-ANT
                   PERFORM 3431-PESQUISAR-SEGMENTO
               END-IF
               MOVE ISGMTO-GSTAO-TARIF     OF GFCTB0H5
                                           TO WRK-SEGMENTO-DESC
               MOVE WRK-SEGMENTO-AUX       TO LD4-REL1-DADOS
             WHEN 014
               MOVE GFCTT9-CDEPDC-AGP14    TO WRK-NAGE
               IF  GFCTT9-CDEPDC-AGP14     NOT EQUAL WRK-CDEPDC14-ANT
                   MOVE GFCTT9-CDEPDC-AGP14
                                           TO CDEPDC OF MESUB010
                   PERFORM 3100-PESQUISAR-DESC-DEPEND
               END-IF
               MOVE IDEPDC OF MESUB010     TO WRK-AGENCIA-DESC
               MOVE WRK-AGENCIAP-AUX       TO LD4-REL1-DADOS
             WHEN 015
               MOVE GFCTT9-CDEPDC-AGP15    TO WRK-POSTO-1
               MOVE GFCTT9-CPOSTO-AGP15    TO WRK-POSTO-2
               IF  GFCTT9-CDEPDC-AGP15    NOT EQUAL WRK-CDEPDC15-ANT AND
                   GFCTT9-CPOSTO-AGP15     NOT EQUAL WRK-CPOSTO15-ANT
                   MOVE GFCTT9-CDEPDC-AGP15
                                           TO CDEPDC       OF MESUB062
                                              WRK-CDEPDC15-ANT
                   MOVE GFCTT9-CPOSTO-AGP15
                                           TO CPOSTO-SERVC OF MESUB062
                                              WRK-CPOSTO15-ANT
                   PERFORM 3443-PESQUISAR-PAB
               END-IF
               MOVE IPOSTO-SERVC           TO WRK-POSTO-DESC
               MOVE WRK-POSTO-AUX          TO LD4-REL1-DADOS
             WHEN 016
               MOVE GFCTT9-CJUNC-AGP16     TO WRK-EMPRESA-1
               MOVE GFCTT9-CCTA-AGP16      TO WRK-EMPRESA-2
               IF  GFCTT9-CJUNC-AGP16      NOT EQUAL WRK-AG16-ANT AND
                   GFCTT9-CCTA-AGP16       NOT EQUAL WRK-CTA16-ANT
                   MOVE GFCTT9-CJUNC-AGP16 TO WRK-5508-CJUNC-DEPDC
                                              WRK-AG16-ANT
                   MOVE GFCTT9-CCTA-AGP16  TO WRK-5508-CCTA-CLI
                                              WRK-CTA16-ANT
                   PERFORM 3412-PESQUISAR-NOME
               END-IF
               MOVE WRK-5508-IPSSOA-COPLT  TO WRK-EMPRESA-DESC
               MOVE WRK-EMPRESA-AUX        TO LD4-REL1-DADOS
             WHEN 017
               MOVE GFCTT9-CDEPDC-AGP17    TO WRK-BCO-POST-1
               MOVE GFCTT9-CPOSTO-AGP17    TO WRK-BCO-POST-2
               IF  GFCTT9-CDEPDC-AGP17    NOT EQUAL WRK-CDEPDC17-ANT AND
                   GFCTT9-CPOSTO-AGP17     NOT EQUAL WRK-CPOSTO17-ANT
                   MOVE GFCTT9-CDEPDC-AGP17
                                           TO CDEPDC OF MESUB062
                                              WRK-CDEPDC17-ANT
                   MOVE GFCTT9-CPOSTO-AGP17
                                           TO CPOSTO-SERVC OF MESUB062
                                              WRK-CPOSTO17-ANT
                   PERFORM 3443-PESQUISAR-PAB
               END-IF
               MOVE IPOSTO-SERVC           OF MESUB062
                                           TO WRK-BCO-POST-DESC
               MOVE WRK-BCO-POSTAL-AUX     TO LD4-REL1-DADOS
             WHEN 018
               MOVE GFCTT9-CMUN-AGP18      TO WRK-MUNICIPIO
               MOVE GFCTT9-DESCR-MUN-AGP18 TO WRK-MUNICIPIO-DESC
               MOVE WRK-MUNICIPIO-AUX      TO LD4-REL1-DADOS
             WHEN 019
               MOVE GFCTT9-UF-AGP19        TO WRK-UF
               MOVE GFCTT9-DESCR-UF-AGP19  TO WRK-UF-DESC
               MOVE WRK-UF-AUX             TO LD4-REL1-DADOS
LFM          WHEN 020
LFM            MOVE GFCTT9-CDEPDC-AGP17   TO WRK-BCO-POST-1
LFM            MOVE GFCTT9-CPOSTO-AGP17   TO WRK-BCO-POST-2
LFM            IF  GFCTT9-CDEPDC-AGP17    NOT EQUAL WRK-CDEPDC17-ANT AND
LFM                GFCTT9-CPOSTO-AGP17    NOT EQUAL WRK-CPOSTO17-ANT
LFM                MOVE GFCTT9-CDEPDC-AGP17
LFM                                       TO CDEPDC OF MESUB062
LFM                                          WRK-CDEPDC17-ANT
LFM                MOVE GFCTT9-CPOSTO-AGP17
LFM                                       TO CPOSTO-SERVC OF MESUB062
LFM                                          WRK-CPOSTO17-ANT
LFM                PERFORM 3443-PESQUISAR-PAB
LFM            END-IF
LFM            MOVE IPOSTO-SERVC            OF MESUB062
LFM                                         TO WRK-BCO-POST-DESC
LFM            MOVE WRK-BCO-POSTAL-AUX      TO LD4-REL1-DADOS
LFM          WHEN 026
LFM            IF  GFCTT9-GRP-CTBIL-AGP26   NOT EQUAL WRK-GRP-AGP26-ANT
LFM            AND GFCTT9-SGRP-CTBIL-AGP26  NOT EQUAL WRK-SGRP-AGP26-ANT
LFM
LFM                PERFORM 3445-PESQUISAR-RAZAOCONTABIL
LFM                MOVE GFCTT9-GRP-CTBIL-AGP26   TO WRK-GRP-AGP26-ANT
LFM                MOVE GFCTT9-SGRP-CTBIL-AGP26  TO WRK-SGRP-AGP26-ANT
LFM            END-IF
LFM            MOVE GFCTT9-GRP-CTBIL-AGP26  TO  WRK-GRP-CTBIL
LFM            MOVE GFCTT9-SGRP-CTBIL-AGP26 TO  WRK-SGRP-CTBIL
LFM            MOVE WRK-RAZAO-AUX           TO LD4-REL1-DADOS
190908       WHEN OTHER
190908         MOVE SPACES                 TO LD4-REL1-DADOS

           END-EVALUATE.

           MOVE GFCTT9-CSERVC-TARIF        TO LD4-REL1-TARI.
           MOVE RSERVC-TARIF-REDZD         OF GFCTB0D8
                                           TO LD4-REL1-DESC.
           MOVE GFCTT9-PDESC-FLEXZ         TO LD4-REL1-PERC.
           MOVE GFCTT9-VNEGOC-FLEXZ        TO LD4-REL1-VALOR.
           MOVE GFCTT9-QFRANQ-FLEXZ        TO LD4-REL1-FRQ.
           MOVE RMOTVO-JUSTF               OF GFCTB089
                                           TO LD4-REL1-MOTVO.
           WRITE FD-RELATO                 FROM LINDET4-REL1.
           MOVE WRK-GRAVACAO               TO WRK-OPERACAO.
           PERFORM 1120-TESTAR-FS-RELATO.
           ADD 1                           TO ACU-LINHAS.

      *----------------------------------------------------------------*
       3444-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

LFM   *----------------------------------------------------------------*
LFM    3445-PESQUISAR-RAZAOCONTABIL SECTION.
LFM   *----------------------------------------------------------------*
LFM
LFM          MOVE 02                       TO  PLCCEA-ORIGEM
LFM          MOVE 237                      TO  WRK-BASE-1560-CEMPR
LFM          MOVE GFCTT9-GRP-CTBIL-AGP26   TO  WRK-BASE-1560-GRUPOR
LFM          MOVE GFCTT9-SGRP-CTBIL-AGP26  TO  WRK-BASE-1560-SGRUPO
LFM
LFM          MOVE   'PLCC8006'        TO WRK-MODULO
LFM          CALL   WRK-MODULO        USING   PLCCEA-INC
LFM                                          WRK-PLCCEA-AREA-COMUNICACAO
LFM          IF   PLCCEA-COD-RETORNO EQUAL ZEROS
LFM               MOVE  WRK-BASE-1560-NRAZAO(1:34)  TO WRK-DESC-CTBIL
LFM          ELSE
LFM               MOVE 'RAZAO CONTABIL NAO LOCALIZADA' TO WRK-DESC-CTBIL
LFM          END-IF.
LFM
LFM   *----------------------------------------------------------------*
LFM    3445-99-FIM.                    EXIT.
LFM   *----------------------------------------------------------------*

      ******************************************************************
      * PROCEDIMENTOS FINAIS.                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           IF  ACU-PAG                 NOT EQUAL ZEROS
               PERFORM 4200-EMITIR-DISPLAY
           END-IF.

           CLOSE FLEXVENC
                 RELATO.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

           STOP RUN.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * EMITE DISPLAY FINAL.                                           *
      ******************************************************************
      *----------------------------------------------------------------*
       4200-EMITIR-DISPLAY             SECTION.
      *----------------------------------------------------------------*

           DISPLAY '*---------------------------------------------*'.
           DISPLAY '*             PROGRAMA GFCT1790               *'.
           DISPLAY '*             -----------------               *'.
           DISPLAY '*                                             *'.
           DISPLAY '*      PROCESSAMENTO EFETUADO COM SUCESSO     *'.
           DISPLAY '*---------------------------------------------*'.

      *----------------------------------------------------------------*
       4200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TRATA ERROS OCORRIDOS DURANTE PROCESSAMENTO.                   *
      ******************************************************************
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT1790'             TO ERR-PGM.

           CALL 'POOL7100'             USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
