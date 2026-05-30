      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT9311.
       AUTHOR.     DIEGO DE SOUZA.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT9311                                    *
      *    PROGRAMADOR.:   DIEGO DE SOUZA          - CPM PATO BRANCO   *
      *    ANALISTA CPM:   KELLI CRISTINA CALDATO  - CPM PATO BRANCO   *
      *    ANALISTA....:   MARCELO CREMM           - PROCWORK /GP.50   *
      *    DATA........:   22/05/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   GERACAO DE MOVIMENTO DE ADESAO COMPULSORIA. *
      *      - AGENCIA / BANCO POSTAL,  COM BASE NOS REGISTROS OBTIDOS *
      *      EM CLIEADES, SERAO GRAVADOS NO ARQUIVO ADESCRR, DE ACORDO *
      *      COM OS PACOTES SOLICITADOS,  DA TABELA DE SOLICITACOES DE *
      *      ADESAO (GFCTB006).                                        *
      *      SERAO GRAVADOS PARA  CADA OCORRENCIA DO ARQUIVO CLIEADES, *
      *      TODAS OCORRENCIAS  DA TABELA  GFCTB006  QUE ESTIVEREM COM *
      *      CDEPDC IGUAIS E BANCO POSTAL  ZERADOS,  ISTO E,  TODOS OS *
      *      PACOTES QUE TIVEREM  CADASTRADOS NA  TABELA INTERNA 1,  E *
      *      TODOS PACOTES  COM AGENCIA  E BANCO  POSTAL  IGUAIS AO DO *
      *      ARQUIVO DE ENTRADA(TABELA INTERNA 2).                     *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                          INCLUDE/BOOK    *
      *                     CLIEADES                     I#GFCTRU      *
      *                     ADESCRR                      I#GFCTRZ      *
      *                     RELADCRR                         -         *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                          INCLUDE/BOOK   *
      *                    DB2PRD.PARM_DATA_PROCM         GFCTB0A1     *
      *                    DB2PRD.ADSAO_COMP_PCOTE        GFCTB001     *
      *                    DB2PRD.ADSAO_GRP_PSTAL         GFCTB006     *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#GFCTRU - ARQ. MOVTO DE CLIENTES.                          *
      *    I#GFCTRZ - ARQ. MOVTO DE CLIENTES DOS PACOTES SOLICITADOS.  *
      *    I#BRAD7C - AREA CHAMADA BRAD7100 - CANCELAMENTO.            *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    BRAD0025 - SOMAR/SUBTRAIR NUMERO DE DIAS DE UMA DATA.       *
      *    BRAD1050 - FAZ CONEXAO COM DB2.                             *
      *    BRAD1285 - SOMAR OU SUBTRAIR DIAS UTEIS A UMA DATA          *
      *    BRAD7100 - AREA PARA TRATAMENTO DE ERROS.                   *
      *    BRAD7600 - OBTEM DATA E HORA DO SISTEMA.                    *
      *================================================================*
      *----------------------------------------------------------------*
      *                                                                *
      *    COMPLEMENTO: MARCELO CREMM ( INFORMACOES COM PWI )          *
      *    OUTUBRO-2006 - INCLUSAO DE DATAS COM VIGENCIA DOS REGISTROS.*
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

      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*

       FILE-CONTROL.

           SELECT CLIEADES  ASSIGN     TO UT-S-CLIEADES
           FILE STATUS                 IS WRK-FS-CLIEADES.

           SELECT ADESCRR   ASSIGN     TO UT-S-ADESCRR
           FILE STATUS                 IS WRK-FS-ADESCRR.

           SELECT RELADCRR  ASSIGN     TO UT-S-RELADCRR
           FILE STATUS                 IS WRK-FS-RELADCRR.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:  ARQUIVO DE CLIENTES - ENTRADA                       *
      *            ORG. SEQUENCIAL     - LRECL   =   040               *
      *----------------------------------------------------------------*

       FD  CLIEADES
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTRU'.

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE CLIENTES ADERIDOS                        *
      *            ORG. SEQUENCIAL     - LRECL   =   040               *
      *----------------------------------------------------------------*

       FD  ADESCRR
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTRZ'.

      *----------------------------------------------------------------*
      *    OUTPUT: RELATORIO ANALITICO E TOTALIZADOR                   *
      *            ORG. SEQUENCIAL     - LRECL   =   130               *
      *----------------------------------------------------------------*

       FD  RELADCRR
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-RELADCRR                 PIC  X(130).

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
       77  IND-2                       PIC  9(003) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA ACUMULADORES *'.
      *----------------------------------------------------------------*

       77  ACU-LIDOS-CSR01             PIC  9(012) COMP-3  VALUE ZEROS.
       77  ACU-LIDOS-CLIEADES          PIC  9(012) COMP-3  VALUE ZEROS.
       77  ACU-GRAVADOS                PIC  9(012) COMP-3  VALUE ZEROS.
       77  ACU-PAGINAS                 PIC  9(004) COMP-3  VALUE ZEROS.
       77  ACU-LINHAS                  PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-TOTAL-CONTA             PIC  9(012) COMP-3  VALUE ZEROS.
       77  WRK-AGRUPA                  PIC S9(003)V COMP-3 VALUE ZEROS.
       77  WRK-DESC-AGRUPA             PIC  X(017) VALUE SPACES.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           'AREA PARA TESTE DE FILE STATUS'.
      *----------------------------------------------------------------*

       77  WRK-FS-CLIEADES             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-ADESCRR              PIC  X(002)         VALUE SPACES.
       77  WRK-FS-RELADCRR             PIC  X(002)         VALUE SPACES.

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

       77  WRK-INICIO                  PIC  X(010)         VALUE SPACES.
       77  WRK-FIM                     PIC  X(010)         VALUE SPACES.
       77  WRK-FIM-CSR01               PIC  X(001)         VALUE 'N'.
       77  WRK-AGENCIA-ANT             PIC  9(005)         VALUE ZEROS.
       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.

       01  WRK-DPROCM-ATUAL            PIC  X(010)         VALUE SPACES.
       01  FILLER                      REDEFINES WRK-DPROCM-ATUAL.
           05  WRK-DIA-PROCM-ATUAL     PIC  9(002).
           05  FILLER                  PIC  X(001).
           05  WRK-MES-PROCM-ATUAL     PIC  9(002).
           05  FILLER                  PIC  X(001).
           05  WRK-ANO-PROCM-ATUAL     PIC  9(004).

       01  WRK-CDEPDC                  PIC S9(005)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-CDEPDC.
           05  WRK-COD-DEPDC           PIC  9(005).

       01  WRK-CPOSTO-SERVC            PIC S9(005)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-CPOSTO-SERVC.
           05  WRK-COD-POSTO-SERVC     PIC  9(005).

       01  WRK-GFCTRU-AGENCIA          PIC S9(005)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-GFCTRU-AGENCIA.
           05  WRK-COD-AGENCIA         PIC  9(005).

       01  WRK-GFCTRU-RAZAO            PIC S9(005)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-GFCTRU-RAZAO.
           05  WRK-COD-RAZAO           PIC  9(005).

       01  WRK-CHAVE.
           05  WRK-AGENCIA             PIC  9(005)         VALUE ZEROS.
           05  WRK-CORRESP             PIC  9(005)         VALUE ZEROS.

       01  WRK-MES-COMP                PIC  9(002) COMP-3  VALUE ZEROS.

       01  WRK-ANO-COMP                PIC  9(004) COMP-3  VALUE ZEROS.

       01  WRK-DATA                    PIC  X(010)         VALUE SPACES.
       01  WRK-DATA-R                  REDEFINES WRK-DATA.
           05  WRK-DATA-DIA            PIC  9(002).
           05  FILLER                  PIC  X(001).
           05  WRK-DATA-MES            PIC  9(002).
           05  FILLER                  PIC  X(001).
           05  WRK-DATA-ANO            PIC  9(004).

       01  WRK-DATA-AUX.
           05 WRK-DIA-AUX              PIC  9(002) VALUE ZEROS.
           05 WRK-P1                   PIC  X(001) VALUE SPACES.
           05 WRK-MES-AUX              PIC  9(002) VALUE ZEROS.
           05 WRK-P2                   PIC  X(001) VALUE SPACES.
           05 WRK-ANO-AUX              PIC  9(004) VALUE ZEROS.

       01  WRK-DATA-AUX1-R             PIC 9(009) VALUE ZEROS.
       01  WRK-DATA-AUX1 REDEFINES WRK-DATA-AUX1-R.
           03  FILLER                  PIC  9(001).
           03  WRK-ANOMES-AUX1.
               05  WRK-ANO-AUX1        PIC  9(004).
               05  WRK-MES-AUX1        PIC  9(002).
           03  WRK-DIA-AUX1            PIC  9(002).

       01  WRK-ANOMES-AUX              PIC  X(006)         VALUE SPACES.

       01  WRK-DATA-ABERT              PIC  9(009) COMP-3  VALUE ZEROS.
       01  WRK-PRAZO                   PIC S9(005) COMP-3  VALUE ZEROS.
       01  WRK-DATA-VENC               PIC  9(009) COMP-3  VALUE ZEROS.

       01  WRK-1285-DATA-DB2.
           05 WRK-1285-DIA-DB2         PIC  9(002)         VALUE ZEROS.
           05 FILLER                   PIC  X(001)         VALUE SPACES.
           05 WRK-1285-MES-DB2         PIC  9(002)         VALUE ZEROS.
           05 FILLER                   PIC  X(001)         VALUE SPACES.
           05 WRK-1285-ANO-DB2         PIC  9(004)         VALUE ZEROS.

       01  WRK-1285-DATA-AUX           PIC  9(008)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-1285-DATA-AUX.
           05  WRK-1285-ANO-AUX        PIC  9(004).
           05  WRK-1285-MES-AUX        PIC  9(002).
           05  WRK-1285-DIA-AUX        PIC  9(002).

       01  WRK-MSG02.
           05  FILLER                  PIC  X(028)         VALUE
               'ESTOURO NA TABELA INTERNA 1 '.

       01  WRK-MSG03.
           05  FILLER                  PIC  X(028)         VALUE
               'ESTOURO NA TABELA INTERNA 2 '.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ERRO - FILE-STATUS ***'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-BRAD7100.
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

       01  WRK-ERRO-BRAD1285.
           05  FILLER                  PIC  X(040)         VALUE
               'ERRO MODULO BRAD1285 - RETURN CODE = '.
           05  WRK-ERRO-1285-RET-COD   PIC  X(004)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*        AREA BRAD1285         *'.
      *----------------------------------------------------------------*

       01  WRK-1285-AREA.
           05  WRK-1285-DATA-ENTRADA   PIC  9(008) COMP-3  VALUE ZEROS.
           05  WRK-1285-NUMERO-DIAS    PIC S9(005) COMP-3  VALUE ZEROS.
           05  WRK-1285-DATA-SAIDA     PIC  9(008) COMP-3  VALUE ZEROS.
           05  WRK-1285-MENSAGEM       PIC  X(050)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA TABELA INTERNA *'.
      *----------------------------------------------------------------*

       01  TAB-1-AGENCIA.
           05  TAB-1-DADOS             OCCURS 301 TIMES.
             10  TAB-1-PACOTE          PIC  9(005)         VALUE ZEROS.
             10  TAB-1-AGPTO           PIC  9(003)         VALUE ZEROS.
             10  TAB-1-SEQ             PIC  9(009)         VALUE ZEROS.
             10  TAB-1-DINI            PIC  X(010)         VALUE SPACES.

       01  TAB-2-AGENCIA.
           05  TAB-2-CHAVE.
             10 TAB-2-AGENC-N          PIC  9(005)         VALUE ZEROS.
             10 TAB-2-CORRESP          PIC  9(005)         VALUE ZEROS.
           05  TAB-2-DADOS             OCCURS 301 TIMES.
             10  TAB-2-PACOTE          PIC  9(005)         VALUE ZEROS.
             10  TAB-2-AGPTO           PIC  9(003)         VALUE ZEROS.
             10  TAB-2-SEQ             PIC  9(009)         VALUE ZEROS.
             10  TAB-2-DINI            PIC  X(010)         VALUE SPACES.

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
           05  FILLER                  PIC  X(040)         VALUE SPACES.
           05  FILLER                  PIC  X(011)         VALUE
               'B A N C O'.
           05  FILLER                  PIC  X(017)         VALUE
               'B R A D E S C O'.
           05  FILLER                  PIC  X(041)         VALUE 'S/A'.
           05  CB1-REL1-HORA           PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE ':'.
           05  CB1-REL1-MIN            PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE ':'.
           05  CB1-REL1-SEG            PIC  9(002)         VALUE ZEROS.

       01  CABEC2-REL1.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  FILLER                  PIC  X(038)         VALUE
               'GFCT9311'.
           05  FILLER                  PIC  X(045)         VALUE
               'GFCT - GESTAO, FLEXIBILIZACAO E COBRANCA DE'.
           05  FILLER                  PIC  X(036)         VALUE
               'TARIFAS'.
           05  FILLER                  PIC  X(004)         VALUE 'PAG.'.
           05  CB2-REL1-PAG            PIC  ZZ99           VALUE ZEROS.

       01  CABEC3-REL1.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  FILLER                  PIC  X(031)         VALUE SPACES.
           05  FILLER                  PIC  X(054)         VALUE
               'RELATORIO DE CLIENTES PARTICIPANTES DE ADESOES POR AGE'.
           05  FILLER                  PIC  X(007)         VALUE
               'NCIA / '.
           05  CB3-REL1-AGRUPA         PIC  X(017)         VALUE SPACES.

       01  CABEC4-REL1.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  FILLER                  PIC  X(131)         VALUE
               ALL '-'.

       01  CABEC5-REL1.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  FILLER                  PIC  X(050)         VALUE SPACES.
           05  FILLER                  PIC  X(013)         VALUE
               'MOVIMENTO DE '.
           05  CB5-REL1-DIA-MOV        PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '/'.
           05  CB5-REL1-MES-MOV        PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '/'.
           05  CB5-REL1-ANO-MOV        PIC  9(004)         VALUE ZEROS.

       01  CABEC6-REL1.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  FILLER                  PIC  X(015)         VALUE
               'AGENCIA'.
           05  FILLER                  PIC  X(010)         VALUE
               'CONTA'.
           05  FILLER                  PIC  X(008)         VALUE
               'CORRESP'.
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
           05  FILLER                  PIC  X(032)         VALUE SPACES.
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
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  LD1-REL1-AGENCIA        PIC  9(005)         VALUE ZEROS.
           05  FILLER                  PIC  X(007)         VALUE SPACES.
           05  LD1-REL1-CONTA          PIC  9(007)         VALUE ZEROS.
           05  FILLER                  PIC  X(005)         VALUE SPACES.
           05  LD1-REL1-CORRESP        PIC  9(005)         VALUE ZEROS.
           05  FILLER                  PIC  X(003)         VALUE SPACES.
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
           05  FILLER                  PIC  X(031)         VALUE
               'QUANTIDADE TOTAL DE CONTAS DA'.
           05  FILLER                  PIC  X(008)         VALUE
               'AGENCIA '.
           05  LT1-REL1-AGENCIA        PIC  ZZZZ9          VALUE ZEROS.
           05  FILLER                  PIC  X(003)         VALUE ' : '.
           05  LT1-REL1-TOT-CTA        PIC  ZZZ.ZZZ.ZZZ.ZZ9 VALUE ZEROS.

       01  LINTOT2-REL1.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  FILLER                  PIC  X(032)         VALUE SPACES.
           05  FILLER                  PIC  X(052)         VALUE
               '1- QUANTIDADE DE CLIENTES LIDOS EM CLIEADES.......: '.
           05  LT2-REL1-TOT-LIDOS      PIC  ZZZ.ZZZ.ZZZ.ZZ9 VALUE ZEROS.

       01  LINTOT3-REL1.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  FILLER                  PIC  X(032)         VALUE SPACES.
           05  FILLER                  PIC  X(052)         VALUE
               '2- QUANTIDADE DE SOLICITACOES DE ADESAO...........: '.
           05  LT3-REL1-TOT-SOLIC      PIC  ZZZ.ZZZ.ZZZ.ZZ9 VALUE ZEROS.

       01  LINTOT4-REL1.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  FILLER                  PIC  X(032)         VALUE SPACES.
           05  FILLER                  PIC  X(052)         VALUE
               '3- QUANTIDADE DE CLIENTES GRAVADOS EM ADESCRR.....: '.
           05  LT4-REL1-TOT-GRAVA      PIC  ZZZ.ZZZ.ZZZ.ZZ9 VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA BRAD7100 *'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DA BRAD7600 *'.
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
               05  FILLER              PIC  X(006)         VALUE SPACES.

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
             INCLUDE GFCTB006
           END-EXEC.

           EXEC SQL
             DECLARE CSR01-GFCTB006-JN CURSOR FOR
             SELECT
                   A.CSERVC_TARIF,
                   A.CAGPTO_CTA,
                   A.CSEQ_AGPTO_CTA,
                   A.DINIC_ADSAO_COMP,
                   A.CEMPR_INC,
                   A.CDEPDC,
                   A.CPOSTO_SERVC,
                   B.DFIM_ADSAO_COMP
             FROM  DB2PRD.ADSAO_GRP_PSTAL   A,
                   DB2PRD.ADSAO_COMP_PCOTE  B
             WHERE
                   A.CSERVC_TARIF        =  B.CSERVC_TARIF
             AND   A.CAGPTO_CTA          =  B.CAGPTO_CTA
             AND   A.CSEQ_AGPTO_CTA      =  B.CSEQ_AGPTO_CTA
             AND   A.DINIC_ADSAO_COMP    =  B.DINIC_ADSAO_COMP
             AND   A.CAGPTO_CTA          =  :WRK-AGRUPA
             AND   B.DINIC_ADSAO_COMP   <=  :GFCTB001.DINIC-ADSAO-COMP
             AND   B.DFIM_ADSAO_COMP    >=  :GFCTB001.DFIM-ADSAO-COMP
             ORDER BY
                   A.CDEPDC,
                   A.CPOSTO_SERVC,
                   A.CSERVC_TARIF
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* FIM DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  LNK-PARM.
           03 LNK-TAMANHO              PIC  S9(004) COMP.
           03 LNK-IDENT                PIC   9(001).

      *================================================================*
       PROCEDURE                       DIVISION        USING LNK-PARM.
      *================================================================*

      ******************************************************************
      *    CHAMA AS ROTINAS PRINCIPAIS DO PROCESSAMENTO.               *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           IF  LNK-IDENT               NOT NUMERIC OR
              (LNK-IDENT               NOT EQUAL 1 AND 2)
               DISPLAY '******** GFCT9311 ********'
               DISPLAY '*                        *'
               DISPLAY '*   PARAMETRO INVALIDO   *'
               DISPLAY '*   LNK-IDENT: ' LNK-IDENT
                                       '         *'
               DISPLAY '*                        *'
               DISPLAY '******** GFCT9311 ********'
               PERFORM 4000-FINALIZAR
           END-IF.

           IF  LNK-IDENT                  EQUAL    1
               MOVE  17                   TO  WRK-AGRUPA
               MOVE  'BANCO POSTAL'       TO  WRK-DESC-AGRUPA
           ELSE
               MOVE  20                   TO  WRK-AGRUPA
               MOVE  'BRADESCO EXPRESSO'  TO  WRK-DESC-AGRUPA
           END-IF.

           MOVE  WRK-DESC-AGRUPA  TO  CB3-REL1-AGRUPA

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

           CALL 'BRAD1050'.

           OPEN INPUT  CLIEADES
                OUTPUT ADESCRR
                       RELADCRR.

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

           PERFORM 1110-TESTAR-FS-CLIEADES.

           PERFORM 1120-TESTAR-FS-ADESCRR.

           PERFORM 1130-TESTAR-FS-RELADCRR.

      *----------------------------------------------------------------*
       1100-99-FIM.               EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE TESTE PARA ARQUIVO CLIEADES.                      *
      ******************************************************************
      *----------------------------------------------------------------*
       1110-TESTAR-FS-CLIEADES         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-CLIEADES         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'CLIEADES'         TO WRK-NOME-ARQ
               MOVE WRK-FS-CLIEADES    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-BRAD7100  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE TESTE PARA ARQUIVO ADESCRR.                       *
      ******************************************************************
      *----------------------------------------------------------------*
       1120-TESTAR-FS-ADESCRR          SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ADESCRR          NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'ADESCRR'          TO WRK-NOME-ARQ
               MOVE WRK-FS-ADESCRR     TO WRK-FILE-STATUS
               MOVE WRK-ERRO-BRAD7100  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE TESTE PARA ARQUIVO RELADCRR.                      *
      ******************************************************************
      *----------------------------------------------------------------*
       1130-TESTAR-FS-RELADCRR         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-RELADCRR         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'RELADCRR'         TO WRK-NOME-ARQ
               MOVE WRK-FS-RELADCRR    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-BRAD7100  TO ERR-TEXTO
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

           PERFORM 2200-LER-CLIEADES

           IF  WRK-FS-CLIEADES         EQUAL '10' AND
               ACU-LIDOS-CLIEADES      EQUAL ZEROS
               DISPLAY '************* GFCT9311 *************'
               DISPLAY '*                                  *'
               DISPLAY '*      ARQUIVO CLIEADES VAZIO      *'
               DISPLAY '*                                  *'
               DISPLAY '*     PROCESSAMENTO  ENCERRADO     *'
               DISPLAY '*                                  *'
               DISPLAY '************* GFCT9311 *************'
           END-IF.

           CALL 'BRAD7600'             USING WRK-DATA-HORA.

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

           PERFORM 2150-OBTER-NOVA-DATA-PROC.

           MOVE DPROCM-ATUAL OF GFCTB0A1
                                       TO WRK-DPROCM-ATUAL.

           MOVE WRK-DIA-PROCM-ATUAL    TO CB5-REL1-DIA-MOV.
           MOVE WRK-MES-PROCM-ATUAL    TO CB5-REL1-MES-MOV.
           MOVE WRK-ANO-PROCM-ATUAL    TO CB5-REL1-ANO-MOV.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2150-OBTER-NOVA-DATA-PROC       SECTION.
      *----------------------------------------------------------------*

      *--- CALCULAR A DATA DE PROCESSAMENTO ANTERIOR (DPROCM-ANTER),
      *--- UTILIZANDO O PRIMEIRO DIA DO MES ATUAL

           MOVE DPROCM-ATUAL OF GFCTB0A1
                                       TO WRK-1285-DATA-DB2.

           MOVE 01                     TO WRK-1285-DIA-AUX.
           MOVE WRK-1285-MES-DB2       TO WRK-1285-MES-AUX.
           MOVE WRK-1285-ANO-DB2       TO WRK-1285-ANO-AUX.

           MOVE WRK-1285-DATA-AUX      TO WRK-1285-DATA-ENTRADA.
           MOVE -1                     TO WRK-1285-NUMERO-DIAS.

           CALL 'BRAD1285'             USING WRK-1285-AREA.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'              TO ERR-TIPO-ACESSO

               MOVE RETURN-CODE        TO WRK-ERRO-1285-RET-COD
               MOVE WRK-ERRO-BRAD1285  TO ERR-TEXTO

               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-1285-DATA-SAIDA    TO WRK-1285-DATA-AUX.

           MOVE WRK-1285-DIA-AUX       TO WRK-1285-DIA-DB2.
           MOVE WRK-1285-MES-AUX       TO WRK-1285-MES-DB2.
           MOVE WRK-1285-ANO-AUX       TO WRK-1285-ANO-DB2.

           MOVE WRK-1285-DATA-DB2      TO DPROCM-ANTER OF GFCTB0A1.

      *--- CALCULAR A DATA DE PROCESSAMENTO ATUAL (DPROCM-ATUAL),
      *--- UTILIZANDO A DATA DE PROCESSAMENRO ANTERIOR (DPROCM-ANTER)

           MOVE WRK-1285-DATA-AUX      TO WRK-1285-DATA-ENTRADA.
           MOVE +1                     TO WRK-1285-NUMERO-DIAS.

           CALL 'BRAD1285'             USING WRK-1285-AREA.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'              TO ERR-TIPO-ACESSO

               MOVE RETURN-CODE        TO WRK-ERRO-1285-RET-COD
               MOVE WRK-ERRO-BRAD1285  TO ERR-TEXTO

               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-1285-DATA-SAIDA    TO WRK-1285-DATA-AUX.

           MOVE WRK-1285-DIA-AUX       TO WRK-1285-DIA-DB2.
           MOVE WRK-1285-MES-AUX       TO WRK-1285-MES-DB2.
           MOVE WRK-1285-ANO-AUX       TO WRK-1285-ANO-DB2.

           MOVE WRK-1285-DATA-DB2      TO DPROCM-ATUAL OF GFCTB0A1.

      *--- CALCULAR A DATA DE PROCESSAMENTO PROXIMA (DPROCM-PROX),
      *--- UTILIZANDO A DATA DE PROCESSAMENRO ATUAL (DPROCM-ATUAL)

           MOVE WRK-1285-DATA-AUX      TO WRK-1285-DATA-ENTRADA.
           MOVE +1                     TO WRK-1285-NUMERO-DIAS.

           CALL 'BRAD1285'             USING WRK-1285-AREA.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'              TO ERR-TIPO-ACESSO

               MOVE RETURN-CODE        TO WRK-ERRO-1285-RET-COD
               MOVE WRK-ERRO-BRAD1285  TO ERR-TEXTO

               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-1285-DATA-SAIDA    TO WRK-1285-DATA-AUX.

           MOVE WRK-1285-DIA-AUX       TO WRK-1285-DIA-DB2.
           MOVE WRK-1285-MES-AUX       TO WRK-1285-MES-DB2.
           MOVE WRK-1285-ANO-AUX       TO WRK-1285-ANO-DB2.

           MOVE WRK-1285-DATA-DB2      TO DPROCM-PROX OF GFCTB0A1.

      *----------------------------------------------------------------*
       2150-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA QUE EFETUA LEITURA DO ARQUIVO DE ENTRADA CLIEADES.   *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-LER-CLIEADES               SECTION.
      *----------------------------------------------------------------*

           READ CLIEADES.

           IF  WRK-FS-CLIEADES         EQUAL '10'
               MOVE 99999              TO WRK-AGENCIA
               MOVE 99999              TO WRK-CORRESP
               GO TO 2200-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.
           PERFORM 1110-TESTAR-FS-CLIEADES.
AQUI
           MOVE GFCTRU-AGENCIA         TO WRK-GFCTRU-AGENCIA.
           MOVE WRK-COD-AGENCIA        TO WRK-AGENCIA.

           MOVE GFCTRU-POSTO-SERV      TO WRK-CORRESP.

           ADD 1                       TO ACU-LIDOS-CLIEADES.

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

           PERFORM 3200-ABRIR-CSR01-GFCTB006.

           PERFORM 3300-LER-CSR01-GFCTB006.

           IF  WRK-FIM-CSR01           EQUAL 'S'
               PERFORM 3700-EMITIR-DISPLAY
           END-IF.

           PERFORM 3400-PROCESSAR-CSR01-GFCTB006 UNTIL
               WRK-FIM-CSR01           EQUAL 'S' OR
               WRK-FS-CLIEADES         EQUAL '10'.

           PERFORM 3600-CLOSE-CSR01-GFCTB006.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA PARA OBTER DADAS.                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       3100-OBTER-DATAS                SECTION.
      *----------------------------------------------------------------*

PWI        MOVE DPROCM-ANTER OF GFCTB0A1 TO DFIM-ADSAO-COMP
PWI                                         OF GFCTB001
PWI                                         WRK-FIM.

PWI        MOVE DPROCM-ANTER OF GFCTB0A1 TO WRK-DATA-AUX
PWI        MOVE WRK-DIA-AUX              TO WRK-DIA-AUX1
PWI        MOVE WRK-MES-AUX              TO WRK-MES-AUX1
PWI        MOVE WRK-ANO-AUX              TO WRK-ANO-AUX1.
PWI
PWI        IF WRK-MES-AUX1            LESS 12
PWI           MOVE WRK-MES-AUX1       TO WRK-MES-COMP
PWI
PWI           COMPUTE WRK-MES-COMP    = WRK-MES-COMP + 1
PWI
PWI           MOVE WRK-MES-COMP       TO WRK-MES-AUX1
PWI        ELSE
PWI           MOVE 01                 TO WRK-MES-AUX1
PWI
PWI           MOVE WRK-ANO-AUX1       TO WRK-ANO-COMP
PWI
PWI           COMPUTE WRK-ANO-COMP    = WRK-ANO-COMP + 1
PWI
PWI           MOVE WRK-ANO-COMP       TO WRK-ANO-AUX1
PWI        END-IF.
PWI
PWI        MOVE 01                       TO WRK-DIA-AUX.
PWI        MOVE WRK-MES-AUX1             TO WRK-MES-AUX.
PWI        MOVE WRK-ANO-AUX1             TO WRK-ANO-AUX.
PWI        MOVE WRK-DATA-AUX             TO DINIC-ADSAO-COMP
PWI                                         OF GFCTB001
PWI                                         WRK-INICIO.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ABRE O CURSOR GFCTB006 DAS TABELAS GFCTB001 E GFCTB006.     *
      ******************************************************************
      *----------------------------------------------------------------*
       3200-ABRIR-CSR01-GFCTB006       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               OPEN CSR01-GFCTB006-JN
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
      *    LER O CURSOR GFCTB006 DAS TABELAS GFCTB001 E GFCTB006.      *
      ******************************************************************
      *----------------------------------------------------------------*
       3300-LER-CSR01-GFCTB006         SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH CSR01-GFCTB006-JN INTO
                   :GFCTB006.CSERVC-TARIF,
                   :GFCTB006.CAGPTO-CTA,
                   :GFCTB006.CSEQ-AGPTO-CTA,
                   :GFCTB006.DINIC-ADSAO-COMP,
                   :GFCTB006.CEMPR-INC,
                   :GFCTB006.CDEPDC,
                   :GFCTB006.CPOSTO-SERVC,
                   :GFCTB001.DFIM-ADSAO-COMP
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'               )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'ADSAO_COMP_PCOTE' TO ERR-DBD-TAB
               MOVE 'FETCH   '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0030'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-CSR01
               MOVE 99999              TO CDEPDC       OF GFCTB006
               MOVE 99999              TO CPOSTO-SERVC OF GFCTB006
           ELSE
               ADD 1                   TO ACU-LIDOS-CSR01
           END-IF.

      *----------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCESSAMENTO DO CURSOR GFCTB006 DAS TABELAS GFCTB001 E     *
      *    GFCTB006.                                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       3400-PROCESSAR-CSR01-GFCTB006   SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO TAB-1-AGENCIA.

           INITIALIZE TAB-1-AGENCIA
                      IND-1.

           IF  CDEPDC                  OF GFCTB006 EQUAL ZEROS
               PERFORM 3410-GUARDA-PACOTES-CORRESP  UNTIL
                       CDEPDC          OF GFCTB006 NOT EQUAL ZEROS OR
                       WRK-FIM-CSR01   EQUAL 'S'
           END-IF.

           PERFORM 3430-DEMAIS-PACOTES-CORRESP     UNTIL
                   WRK-FS-CLIEADES                 EQUAL '10'.

      *----------------------------------------------------------------*
       3400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA QUE GUARDA PACOTES                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       3410-GUARDA-PACOTES-CORRESP     SECTION.
      *----------------------------------------------------------------*

           ADD 1                               TO IND-1.

           IF  IND-1                           GREATER 300
               DISPLAY '*************** GFCT9311 ***************'
               DISPLAY '*                                      *'
               DISPLAY '*      ESTOURO NA TABELA INTERNA 1     *'
               DISPLAY '*                                      *'
               DISPLAY '*************** GFCT9311 ***************'
               MOVE 'APL'                      TO ERR-TIPO-ACESSO
               MOVE WRK-MSG02                  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE CSERVC-TARIF     OF GFCTB006   TO TAB-1-PACOTE(IND-1).
           MOVE CAGPTO-CTA       OF GFCTB006   TO TAB-1-AGPTO(IND-1).
           MOVE CSEQ-AGPTO-CTA   OF GFCTB006   TO TAB-1-SEQ(IND-1).
           MOVE DINIC-ADSAO-COMP OF GFCTB006   TO TAB-1-DINI(IND-1).

           PERFORM 3300-LER-CSR01-GFCTB006.

      *----------------------------------------------------------------*
       3410-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA QUE DESCARREGA OS DEMAIS PACOTES CORRESP.            *
      ******************************************************************
      *----------------------------------------------------------------*
       3430-DEMAIS-PACOTES-CORRESP     SECTION.
      *----------------------------------------------------------------*

           MOVE  SPACES  TO  TAB-2-AGENCIA

           INITIALIZE TAB-2-AGENCIA
                      IND-2.

           MOVE CDEPDC OF GFCTB006         TO WRK-CDEPDC.
           MOVE WRK-COD-DEPDC              TO TAB-2-AGENC-N.

           MOVE CPOSTO-SERVC OF GFCTB006   TO WRK-CPOSTO-SERVC.
           MOVE WRK-COD-POSTO-SERVC        TO TAB-2-CORRESP.

           PERFORM 3431-MONTAR-TAB-2                               UNTIL
               WRK-COD-DEPDC               GREATER TAB-2-AGENC-N   OR
               WRK-COD-POSTO-SERVC         GREATER TAB-2-CORRESP   OR
               WRK-FIM-CSR01               EQUAL 'S'.

           PERFORM 3500-DESCARREGA-CORRESP-BONS                    UNTIL
               WRK-CHAVE                   GREATER TAB-2-CHAVE     OR
               WRK-FS-CLIEADES             EQUAL '10'.

      *----------------------------------------------------------------*
       3430-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA PARA MONTAGEM DA TABELA INTERNA 2.                   *
      ******************************************************************
      *----------------------------------------------------------------*
       3431-MONTAR-TAB-2               SECTION.
      *----------------------------------------------------------------*

           ADD 1                       TO IND-2.

           IF  IND-2                   GREATER  300
               DISPLAY '*************** GFCT9311 ***************'
               DISPLAY '*                                      *'
               DISPLAY '*      ESTOURO NA TABELA INTERNA 2     *'
               DISPLAY '*                                      *'
               DISPLAY '*************** GFCT9311 ***************'
               MOVE 'APL'                      TO ERR-TIPO-ACESSO
               MOVE WRK-MSG03                  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE CSERVC-TARIF     OF GFCTB006   TO TAB-2-PACOTE(IND-2).
           MOVE CAGPTO-CTA       OF GFCTB006   TO TAB-2-AGPTO(IND-2).
           MOVE CSEQ-AGPTO-CTA   OF GFCTB006   TO TAB-2-SEQ(IND-2).
           MOVE DINIC-ADSAO-COMP OF GFCTB006   TO TAB-2-DINI(IND-2).

           PERFORM 3300-LER-CSR01-GFCTB006.

      *----------------------------------------------------------------*
       3431-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA QUE DESCARREGA CORRESP COM PACOTES BONS.             *
      ******************************************************************
      *----------------------------------------------------------------*
       3500-DESCARREGA-CORRESP-BONS    SECTION.
      *----------------------------------------------------------------*

           IF  WRK-CHAVE                   LESS TAB-2-CHAVE
               PERFORM 3510-PROCESSA-PACOTE-ZERO
                   VARYING IND-1           FROM 1 BY 1 UNTIL
                   TAB-1-PACOTE(IND-1)     EQUAL ZEROS
               PERFORM 2200-LER-CLIEADES
           ELSE
               IF  WRK-CHAVE               EQUAL TAB-2-CHAVE
                   PERFORM 3510-PROCESSA-PACOTE-ZERO
                       VARYING IND-1       FROM 1 BY 1 UNTIL
                       TAB-1-PACOTE(IND-1) EQUAL ZEROS
                   PERFORM 3520-PACOTES-VALIDOS
                       VARYING IND-2   FROM 1 BY 1 UNTIL
                       TAB-2-PACOTE(IND-2) EQUAL ZEROS
                   PERFORM 2200-LER-CLIEADES
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA PARA PROCESSAMENTO DE CORRESP ZERO.                  *
      ******************************************************************
      *----------------------------------------------------------------*
       3510-PROCESSA-PACOTE-ZERO       SECTION.
      *----------------------------------------------------------------*

           PERFORM 3511-MOVER-DADOS.

           MOVE  TAB-1-PACOTE (IND-1)  TO GFCTRZ-PACOTE.
           MOVE  TAB-1-AGPTO  (IND-1)  TO GFCTRZ-AGPTO.
           MOVE  TAB-1-SEQ    (IND-1)  TO GFCTRZ-SEQ.
           MOVE  TAB-1-DINI   (IND-1)  TO GFCTRZ-DATA.

           MOVE  TAB-1-PACOTE (IND-1)  TO LD1-REL1-PACOTE.
           MOVE  TAB-1-AGPTO  (IND-1)  TO LD1-REL1-AGRUPAMENTO.
           MOVE  TAB-1-SEQ    (IND-1)  TO LD1-REL1-SEQUENCIA.
           MOVE  TAB-1-DINI   (IND-1)  TO WRK-DATA.
           MOVE  WRK-DATA-DIA          TO LD1-REL1-DIA-INI.
           MOVE  WRK-DATA-MES          TO LD1-REL1-MES-INI.
           MOVE  WRK-DATA-ANO          TO LD1-REL1-ANO-INI.

           PERFORM 3512-GRAVAR-ADESCRR.

      *----------------------------------------------------------------*
       3510-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA PARA MOVIMENTACAO DE DADOS.                          *
      ******************************************************************
      *----------------------------------------------------------------*
       3511-MOVER-DADOS                SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTRU-AGENCIA         TO WRK-GFCTRU-AGENCIA.
           MOVE WRK-COD-AGENCIA        TO GFCTRZ-AGENCIA.

           MOVE GFCTRU-RAZAO           TO WRK-GFCTRU-RAZAO.
           MOVE WRK-COD-RAZAO          TO GFCTRZ-RAZAO.

           MOVE GFCTRU-CONTA           TO GFCTRZ-CONTA.
           MOVE GFCTRU-POSTO-SERV      TO GFCTRZ-PAB.

           MOVE GFCTRU-AGENCIA         TO WRK-GFCTRU-AGENCIA.
           MOVE WRK-COD-AGENCIA        TO LD1-REL1-AGENCIA.

           MOVE GFCTRU-CONTA           TO LD1-REL1-CONTA.
           MOVE GFCTRU-POSTO-SERV      TO LD1-REL1-CORRESP.

PWI        PERFORM 3511-1-OBTER-PERIODO.

      *----------------------------------------------------------------*
       3511-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
PWI   *----------------------------------------------------------------*
PWI    3511-1-OBTER-PERIODO              SECTION.
PWI   *----------------------------------------------------------------*
PWI
PWI        MOVE DPROCM-ANTER OF GFCTB0A1 TO WRK-DATA-AUX
PWI        MOVE WRK-DIA-AUX              TO WRK-DIA-AUX1
PWI        MOVE WRK-MES-AUX              TO WRK-MES-AUX1
PWI        MOVE WRK-ANO-AUX              TO WRK-ANO-AUX1.
PWI
PWI*****   SALVA ANOMES DA DPROCM-ANTER
PWI
PWI        MOVE WRK-ANOMES-AUX1          TO WRK-ANOMES-AUX
PWI
PWI        MOVE DINIC-ADSAO-COMP       OF GFCTB006
PWI                                      TO WRK-DATA-AUX
PWI        MOVE WRK-DIA-AUX              TO WRK-DIA-AUX1
PWI        MOVE WRK-MES-AUX              TO WRK-MES-AUX1
PWI        MOVE WRK-ANO-AUX              TO WRK-ANO-AUX1.
PWI
PWI        IF WRK-ANOMES-AUX1 NOT GREATER WRK-ANOMES-AUX
PWI           PERFORM 3511-2-MONTA-NOMES
PWI        ELSE
PWI           PERFORM 3511-3-MONTA-FUTURO
PWI        END-IF.
PWI
PWI   *----------------------------------------------------------------*
PWI    3511-1-99-FIM.                 EXIT.
PWI   *----------------------------------------------------------------*
PWI
PWI   *----------------------------------------------------------------*
PWI    3511-2-MONTA-NOMES             SECTION.
PWI   *----------------------------------------------------------------
PWI
PWI        MOVE DPROCM-ANTER OF GFCTB0A1 TO WRK-DATA-AUX.
PWI        MOVE 01                       TO WRK-DIA-AUX1.
PWI        MOVE WRK-MES-AUX              TO WRK-MES-AUX1.
PWI        MOVE WRK-ANO-AUX              TO WRK-ANO-AUX1.
PWI        MOVE WRK-DATA-AUX1-R          TO GFCTRZ-DINI.
PWI
PWI        MOVE DFIM-ADSAO-COMP          OF GFCTB001
PWI                                      TO WRK-DATA-AUX
PWI        MOVE WRK-DIA-AUX              TO WRK-DIA-AUX1
PWI        MOVE WRK-MES-AUX              TO WRK-MES-AUX1
PWI        MOVE WRK-ANO-AUX              TO WRK-ANO-AUX1.
PWI
PWI        IF WRK-ANOMES-AUX1            EQUAL WRK-ANOMES-AUX
PWI           MOVE WRK-DATA-AUX1-R       TO GFCTRZ-DFIM
PWI        ELSE
PWI           PERFORM 3511-4-OBTER-DFIM
PWI        END-IF.
PWI
PWI   *----------------------------------------------------------------*
PWI    3511-2-99-FIM.                 EXIT.
PWI   *----------------------------------------------------------------*
PWI
PWI   *----------------------------------------------------------------*
PWI    3511-3-MONTA-FUTURO            SECTION.
PWI   *----------------------------------------------------------------*
PWI
PWI******  OBTER 1 MES  SEGUINTE AO DA DPROCM-ANTER
PWI******  PARA OBTER PRIMEIRO DIA.
PWI
PWI        MOVE DPROCM-ANTER OF GFCTB0A1 TO WRK-DATA-AUX
PWI        MOVE WRK-DIA-AUX              TO WRK-DIA-AUX1
PWI        MOVE WRK-MES-AUX              TO WRK-MES-AUX1
PWI        MOVE WRK-ANO-AUX              TO WRK-ANO-AUX1.
PWI
PWI        IF WRK-MES-AUX1             LESS 12
PWI           MOVE WRK-MES-AUX1        TO WRK-MES-COMP
PWI
PWI           COMPUTE WRK-MES-COMP     = WRK-MES-COMP + 1
PWI
PWI           MOVE WRK-MES-COMP        TO WRK-MES-AUX1
PWI        ELSE
PWI           MOVE 01                  TO WRK-MES-AUX1
PWI
PWI           MOVE WRK-ANO-AUX1        TO WRK-ANO-COMP
PWI
PWI           COMPUTE WRK-ANO-COMP     = WRK-ANO-COMP + 1
PWI
PWI           MOVE WRK-ANO-COMP        TO WRK-ANO-AUX1
PWI        END-IF.
PWI
PWI        MOVE 01                    TO WRK-DIA-AUX1.
PWI        MOVE WRK-DATA-AUX1-R       TO GFCTRZ-DINI.
PWI
PWI        PERFORM 3511-4-OBTER-DFIM.
PWI
PWI   *----------------------------------------------------------------*
PWI    3511-3-99-FIM.                 EXIT.
PWI   *----------------------------------------------------------------*
PWI   *----------------------------------------------------------------*
PWI    3511-4-OBTER-DFIM              SECTION.
PWI   *----------------------------------------------------------------*
PWI
PWI        MOVE DPROCM-ANTER OF GFCTB0A1 TO WRK-DATA-AUX
PWI        MOVE WRK-DIA-AUX              TO WRK-DIA-AUX1
PWI        MOVE WRK-MES-AUX              TO WRK-MES-AUX1
PWI        MOVE WRK-ANO-AUX              TO WRK-ANO-AUX1.
PWI
PWI******  OBTER 2 MESES SEGUINTES AO DA DPROCM-ANTER
PWI******  PARA RETROAGIR 1 DIA E OBTER DATA DE FIM
PWI******  DO MES SEGUINTE.
PWI
PWI        IF WRK-MES-AUX1            LESS 12
PWI           MOVE WRK-MES-AUX1       TO WRK-MES-COMP
PWI
PWI           COMPUTE WRK-MES-COMP    = WRK-MES-COMP + 1
PWI
PWI           MOVE WRK-MES-COMP       TO WRK-MES-AUX1
PWI        ELSE
PWI           MOVE 01                 TO WRK-MES-AUX1
PWI
PWI           MOVE WRK-ANO-AUX1       TO WRK-ANO-COMP
PWI
PWI           COMPUTE WRK-ANO-COMP    = WRK-ANO-COMP + 1
PWI
PWI           MOVE WRK-ANO-COMP       TO WRK-ANO-AUX1
PWI        END-IF.
PWI
PWI        IF WRK-MES-AUX1            LESS 12
PWI           MOVE WRK-MES-AUX1       TO WRK-MES-COMP
PWI
PWI           COMPUTE WRK-MES-COMP    = WRK-MES-COMP + 1
PWI
PWI           MOVE WRK-MES-COMP       TO WRK-MES-AUX1
PWI        ELSE
PWI           MOVE 01                 TO WRK-MES-AUX1
PWI
PWI           MOVE WRK-ANO-AUX1       TO WRK-ANO-COMP
PWI
PWI           COMPUTE WRK-ANO-COMP    = WRK-ANO-COMP + 1
PWI
PWI           MOVE WRK-ANO-COMP       TO WRK-ANO-AUX1
PWI        END-IF.
PWI
PWI        MOVE 1                      TO WRK-DIA-AUX1
PWI        MOVE WRK-DATA-AUX1-R        TO WRK-DATA-ABERT.
PWI        MOVE -1                     TO WRK-PRAZO.
PWI
PWI        CALL 'BRAD0025'             USING WRK-DATA-ABERT
PWI                                          WRK-PRAZO
PWI                                          WRK-DATA-VENC.
PWI
PWI        IF  RETURN-CODE             NOT EQUAL ZEROS
PWI            MOVE 'APL'              TO ERR-TIPO-ACESSO
PWI            MOVE 'ERRO ACESSO MODULO BRAD0025' TO ERR-TEXTO
PWI            PERFORM 9999-PROCESSAR-ROTINA-ERRO
PWI        END-IF.
PWI
PWI        MOVE WRK-DATA-VENC          TO WRK-DATA-AUX1-R.
PWI        MOVE WRK-DATA-AUX1-R        TO GFCTRZ-DFIM.
PWI
PWI   *----------------------------------------------------------------*
PWI    3511-4-99-FIM.                 EXIT.
PWI   *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA PARA GRAVACAO DE ADESCRR.                            *
      ******************************************************************
      *----------------------------------------------------------------*
       3512-GRAVAR-ADESCRR             SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           WRITE GFCTRZ-ADESOES.

           PERFORM 1120-TESTAR-FS-ADESCRR.

           ADD 1                       TO ACU-GRAVADOS.

           PERFORM 3513-EMITE-RELATORIO.

      *----------------------------------------------------------------*
       3512-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA PARA EMISSAO DE RELATORIO.                           *
      ******************************************************************
      *----------------------------------------------------------------*
       3513-EMITE-RELATORIO            SECTION.
      *----------------------------------------------------------------*

           IF (ACU-LINHAS              GREATER 52) AND
              (WRK-AGENCIA             NOT EQUAL WRK-AGENCIA-ANT)
               PERFORM 3514-TOTAL-CONTAS
               PERFORM 3515-EMITIR-CABEC
               MOVE WRK-AGENCIA        TO WRK-AGENCIA-ANT
           ELSE
               IF (ACU-PAGINAS         EQUAL ZEROS) OR
                  (ACU-LINHAS          GREATER 52 )
                   PERFORM 3515-EMITIR-CABEC
               ELSE
                   IF (WRK-AGENCIA     NOT EQUAL WRK-AGENCIA-ANT)
                       PERFORM 3514-TOTAL-CONTAS
                       PERFORM 3516-CABEC-NOVA-AGENCIA
                       MOVE WRK-AGENCIA
                                       TO WRK-AGENCIA-ANT
                   END-IF
               END-IF
           END-IF.

           WRITE FD-RELADCRR           FROM LINDET1-REL1.
           PERFORM 1130-TESTAR-FS-RELADCRR.

           ADD 1                       TO ACU-TOTAL-CONTA.
           ADD 1                       TO ACU-LINHAS.

      *----------------------------------------------------------------*
       3513-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA PARA EMISSAO DE TOTAL DE CONTAS POR AGENCIA.         *
      ******************************************************************
      *----------------------------------------------------------------*
       3514-TOTAL-CONTAS               SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-AGENCIA-ANT        TO LT1-REL1-AGENCIA.
           MOVE ACU-TOTAL-CONTA        TO LT1-REL1-TOT-CTA.
           MOVE ZEROS                  TO ACU-TOTAL-CONTA.

           WRITE FD-RELADCRR           FROM CABEC8-REL1-BRANCO.
           PERFORM 1130-TESTAR-FS-RELADCRR.
           WRITE FD-RELADCRR           FROM CABEC8-REL1-BRANCO.
           PERFORM 1130-TESTAR-FS-RELADCRR.
           WRITE FD-RELADCRR           FROM LINTOT1-REL1.
           PERFORM 1130-TESTAR-FS-RELADCRR.

           ADD 3                       TO ACU-LINHAS.

      *----------------------------------------------------------------*
       3514-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA PARA EMISSAO DE CABECARIO.                           *
      ******************************************************************
      *----------------------------------------------------------------*
       3515-EMITIR-CABEC               SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-AGENCIA            TO WRK-AGENCIA-ANT.
           ADD 1                       TO ACU-PAGINAS.
           MOVE ACU-PAGINAS            TO CB2-REL1-PAG.
           WRITE FD-RELADCRR           FROM CABEC1-REL1.
           PERFORM 1130-TESTAR-FS-RELADCRR.
           WRITE FD-RELADCRR           FROM CABEC2-REL1.
           PERFORM 1130-TESTAR-FS-RELADCRR.
           WRITE FD-RELADCRR           FROM CABEC3-REL1.
           PERFORM 1130-TESTAR-FS-RELADCRR.
           WRITE FD-RELADCRR           FROM CABEC4-REL1.
           PERFORM 1130-TESTAR-FS-RELADCRR.
           WRITE FD-RELADCRR           FROM CABEC5-REL1.
           PERFORM 1130-TESTAR-FS-RELADCRR.
           WRITE FD-RELADCRR           FROM CABEC4-REL1.
           PERFORM 1130-TESTAR-FS-RELADCRR.
           WRITE FD-RELADCRR           FROM CABEC8-REL1-BRANCO.
           PERFORM 1130-TESTAR-FS-RELADCRR.
           WRITE FD-RELADCRR           FROM CABEC6-REL1.
           PERFORM 1130-TESTAR-FS-RELADCRR.
           WRITE FD-RELADCRR           FROM CABEC4-REL1.
           PERFORM 1130-TESTAR-FS-RELADCRR.
           MOVE 9                      TO ACU-LINHAS.

      *----------------------------------------------------------------*
       3515-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA PARA EMISSAO DE CABECARIO DE NOVA AGENCIA.           *
      ******************************************************************
      *----------------------------------------------------------------*
       3516-CABEC-NOVA-AGENCIA         SECTION.
      *----------------------------------------------------------------*

           WRITE FD-RELADCRR           FROM CABEC8-REL1-BRANCO.
           PERFORM 1130-TESTAR-FS-RELADCRR.
           WRITE FD-RELADCRR           FROM CABEC8-REL1-BRANCO.
           PERFORM 1130-TESTAR-FS-RELADCRR.
           WRITE FD-RELADCRR           FROM CABEC6-REL1.
           PERFORM 1130-TESTAR-FS-RELADCRR.
           WRITE FD-RELADCRR           FROM CABEC4-REL1.
           PERFORM 1130-TESTAR-FS-RELADCRR.

           ADD 4                       TO ACU-LINHAS.

      *----------------------------------------------------------------*
       3516-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA PARA PROCESSAMENTO DE PACOTES VALIDOS.               *
      ******************************************************************
      *----------------------------------------------------------------*
       3520-PACOTES-VALIDOS            SECTION.
      *----------------------------------------------------------------*

           PERFORM 3511-MOVER-DADOS.

           MOVE  TAB-2-PACOTE (IND-2)  TO GFCTRZ-PACOTE.
           MOVE  TAB-2-AGPTO  (IND-2)  TO GFCTRZ-AGPTO.
           MOVE  TAB-2-SEQ    (IND-2)  TO GFCTRZ-SEQ.
           MOVE  TAB-2-DINI   (IND-2)  TO GFCTRZ-DATA.

           MOVE  TAB-2-PACOTE (IND-2)  TO LD1-REL1-PACOTE.
           MOVE  TAB-2-AGPTO  (IND-2)  TO LD1-REL1-AGRUPAMENTO.
           MOVE  TAB-2-SEQ    (IND-2)  TO LD1-REL1-SEQUENCIA.
           MOVE  TAB-2-DINI   (IND-2)  TO WRK-DATA.
           MOVE  WRK-DATA-DIA          TO LD1-REL1-DIA-INI.
           MOVE  WRK-DATA-MES          TO LD1-REL1-MES-INI.
           MOVE  WRK-DATA-ANO          TO LD1-REL1-ANO-INI.

           PERFORM 3512-GRAVAR-ADESCRR.

      *----------------------------------------------------------------*
       3520-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    FECHAR O CURSOR DA TABELA GFCTB006                          *
      ******************************************************************
      *----------------------------------------------------------------*
       3600-CLOSE-CSR01-GFCTB006       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR01-GFCTB006-JN
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
       3600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    EXIBE INFORMACOES DE PROCESSAMENTO.                         *
      ******************************************************************
      *----------------------------------------------------------------*
       3700-EMITIR-DISPLAY             SECTION.
      *----------------------------------------------------------------*

           DISPLAY '*************** GFCT9311 ***************'.
           DISPLAY '*                                      *'.
           DISPLAY '*   NAO EXISTE SOLICITACAO DE ADESAO   *'.
           DISPLAY '*   AGENCIA/'
                                WRK-DESC-AGRUPA '          *'.
           DISPLAY '*   PARA O PERIODO DE:                 *'.
           DISPLAY '*   DATA INICIO : '
                                    WRK-INICIO '           *'.
           DISPLAY '*   DATA FINAL  : '
                                    WRK-FIM    '           *'.
           DISPLAY '*                                      *'.
           DISPLAY '*           TERMINO NORMAL             *'.
           DISPLAY '*                                      *'.
           DISPLAY '*                                      *'.
           DISPLAY '*************** GFCT9311 ***************'.

      *----------------------------------------------------------------*
       3700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA QUE EFETUA PROCESSAMENTO FINAL DO PROGRAMA           *
      ******************************************************************
      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           IF  ACU-GRAVADOS           NOT EQUAL ZEROS
               PERFORM 3514-TOTAL-CONTAS
           END-IF.

           IF  ACU-LIDOS-CSR01         NOT EQUAL ZEROS
               PERFORM 4100-TOTALIZAR-RELATORIO
           END-IF.

           PERFORM 4200-EMITIR-DISPLAY.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           CLOSE CLIEADES
                 ADESCRR
                 RELADCRR.

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

           WRITE FD-RELADCRR           FROM CABEC1-REL1.
           PERFORM 1130-TESTAR-FS-RELADCRR.
           WRITE FD-RELADCRR           FROM CABEC2-REL1.
           PERFORM 1130-TESTAR-FS-RELADCRR.
           WRITE FD-RELADCRR           FROM CABEC3-REL1.
           PERFORM 1130-TESTAR-FS-RELADCRR.
           WRITE FD-RELADCRR           FROM CABEC4-REL1.
           PERFORM 1130-TESTAR-FS-RELADCRR.
           WRITE FD-RELADCRR           FROM CABEC5-REL1.
           PERFORM 1130-TESTAR-FS-RELADCRR.
           WRITE FD-RELADCRR           FROM CABEC4-REL1.
           PERFORM 1130-TESTAR-FS-RELADCRR.
           WRITE FD-RELADCRR           FROM CABEC8-REL1-BRANCO.
           PERFORM 1130-TESTAR-FS-RELADCRR.
           WRITE FD-RELADCRR           FROM CABEC7-REL1.
           PERFORM 1130-TESTAR-FS-RELADCRR.
           WRITE FD-RELADCRR           FROM CABEC8-REL1-BRANCO.
           PERFORM 1130-TESTAR-FS-RELADCRR.
           WRITE FD-RELADCRR           FROM CABEC8-REL1-BRANCO.
           PERFORM 1130-TESTAR-FS-RELADCRR.

           MOVE ACU-LIDOS-CLIEADES     TO LT2-REL1-TOT-LIDOS.
           MOVE ACU-LIDOS-CSR01        TO LT3-REL1-TOT-SOLIC.
           MOVE ACU-GRAVADOS           TO LT4-REL1-TOT-GRAVA.

           WRITE FD-RELADCRR           FROM LINTOT2-REL1.
           PERFORM 1130-TESTAR-FS-RELADCRR.
           WRITE FD-RELADCRR           FROM CABEC8-REL1-BRANCO.
           PERFORM 1130-TESTAR-FS-RELADCRR.
           WRITE FD-RELADCRR           FROM LINTOT3-REL1.
           PERFORM 1130-TESTAR-FS-RELADCRR.
           WRITE FD-RELADCRR           FROM CABEC8-REL1-BRANCO.
           PERFORM 1130-TESTAR-FS-RELADCRR.
           WRITE FD-RELADCRR           FROM LINTOT4-REL1.
           PERFORM 1130-TESTAR-FS-RELADCRR.

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE EXIBICAO DE INFORMACOES FINAIS DE PROCESSAMENTO   *
      ******************************************************************
      *----------------------------------------------------------------*
       4200-EMITIR-DISPLAY             SECTION.
      *----------------------------------------------------------------*

           DISPLAY '*************** GFCT9311 ***************'.
           DISPLAY '*                                      *'.
           DISPLAY '*   PROGRAMA ENCERRADO COM SUCESSO     *'.
           DISPLAY '*                                      *'.
           DISPLAY '*           '
                                WRK-DESC-AGRUPA '          *'.
           DISPLAY '*                                      *'.
           DISPLAY '*   PROCESSAMENTO DE : '
                       DPROCM-ATUAL OF GFCTB0A1     '      *'.
           DISPLAY '*                                      *'.
           DISPLAY '*            TERMINO NORMAL            *'.
           DISPLAY '*                                      *'.
           DISPLAY '*************** GFCT9311 ***************'.

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

           MOVE 'GFCT9311'             TO ERR-PGM.
           CALL 'BRAD7100'             USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
