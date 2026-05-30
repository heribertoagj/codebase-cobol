      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT9305.
       AUTHOR.     CAMILA CRISTINA MAFIOLETI.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT9305                                    *
      *    PROGRAMADORA:   CAMILA C. MAFIOLETI     - CPM PATO BRANCO   *
      *    ANALISTA CPM:   MARCIO C. CUSTIN        - CPM PATO BRANCO   *
      *    ANALISTA....:   MARCELO CREMM           - PROCWORK / GP.50  *
      *    DATA........:   22/05/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   GERACAO DE MOVIMENTO DE ADESAO COMPULSORIA. *
      *      AGENCIA. COM BASE NOS REGISTROS OBTIDOS EM CLIEADES, SERAO*
      *      GRAVADOS NO ARQUIVO ADESAG, DE ACORDO COM OS PACOTES  SO- *
      *      LICITADOS, DA TABELA DE SOLICITACOES DE ADESAO  GFCTB002. *
      *      SERAO GRAVADOS PARA CADA OCORRENCIA DO  ARQUIVO  CLIEADES,*
      *      TODAS OCORRENCIAS DA TABELA GFCTB002  QUE  ESTIVEREM  COM *
      *      CDEPDC ZERADOS, ISTO E, TODOS OS APCOTES QUE TIVEREM  CA- *
      *      DASTRADO NA TABELA INTERNA 1, E TODOS PACOTES COM AGENCIA *
      *      IGUAIS AO DO ARQUIVO DE ENTRADA (TABELA INTERNA2).        *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                         INCLUDE/BOOK     *
      *                    CLIEADES                     I#GFCTRU       *
      *                    ADESAG                       I#GFCTRZ       *
      *                    RELADAG                          -          *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                        INCLUDE/BOOK     *
      *                    DB2PRD.PARM_DATA_PROCM       GFCTB0A1       *
      *                    DB2PRD.ADSAO_COMP_PCOTE      GFCTB001       *
      *                    DB2PRD.ADSAO_GRP_DEPDC       GFCTB002       *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#BRAD7C - AREA PARA TRATAMENTO DE ERROS PELA BRAD7100.     *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    BRAD0025 - SOMA OU SUBTRAI UM DETERMINADO NUMERO DE DATA.   *
      *    BRAD1050 - CONEXAO COM DB2.                                 *
      *    BRAD1285 - SOMAR OU SUBTRAIR DIAS UTEIS A UMA DATA          *
      *    BRAD7100 - TRATAMENTO DE ERROS PADRAO BRADESCO.             *
      *    BRAD7600 - OBTER DATA E HORA DO SISTEMA.                    *
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

           SELECT CLIEADES ASSIGN      TO UT-S-CLIEADES
           FILE STATUS                 IS WRK-FS-CLIEADES.

           SELECT ADESAG   ASSIGN      TO UT-S-ADESAG
           FILE STATUS                 IS WRK-FS-ADESAG.

           SELECT RELADAG  ASSIGN      TO UT-S-RELADAG
           FILE STATUS                 IS WRK-FS-RELADAG.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT : ARQUIVO DE CLIENTES - ENTRADA                       *
      *            ORG. SEQUENCIAL     -   LRECL = 040                 *
      *----------------------------------------------------------------*

       FD  CLIEADES
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTRU'.

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE CLIENTES ADERIDOS.                       *
      *            ORG. SEQUENCIAL     -   LRECL = 040                 *
      *----------------------------------------------------------------*

       FD  ADESAG
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTRZ'.

      *----------------------------------------------------------------*
      *    OUTPUT: RELATORIO ANALITICO E TOTALIZADOR                   *
      *            ORG. SEQUENCIAL     -   LRECL = 133                 *
      *----------------------------------------------------------------*

       FD  RELADAG
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-RELADAG                   PIC  X(133).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(033)         VALUE
           'INICIO DA WORKING STORAGE SECTION'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA DE INDEXADORES *'.
      *----------------------------------------------------------------*

       77  IND-1                       PIC  9(003) COMP-3  VALUE ZEROS.
       77  IND-2                       PIC  9(003) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA DE ACUMULADORES *'.
      *----------------------------------------------------------------*

       77  ACU-LINHAS                  PIC  9(003) COMP-3  VALUE 99.
       77  ACU-PAGINAS                 PIC  9(004) COMP-3  VALUE ZEROS.
       77  ACU-LIDOS-CLIEADES          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-LIDOS-CURSOR            PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-ADESAG            PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-AGENCIA                 PIC  9(009) COMP-3  VALUE ZEROS.

       01  WRK-CDEPDC                  PIC S9(005)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-CDEPDC.
           05  WRK-COD-DEPDC           PIC  9(005).

       01  WRK-GFCTRU-AGENCIA          PIC S9(005)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-GFCTRU-AGENCIA.
           05  WRK-COD-AGENCIA         PIC  9(005).

       01  WRK-GFCTRU-RAZAO            PIC S9(005)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-GFCTRU-RAZAO.
           05  WRK-COD-RAZAO           PIC  9(005).

       01  WRK-MES                     PIC  9(002) COMP-3  VALUE ZEROS.

       01  WRK-ANO                     PIC  9(004) COMP-3  VALUE ZEROS.

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

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA DE TESTES DE FILE STATUS *'.
      *----------------------------------------------------------------*

       77  WRK-FS-CLIEADES             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-ADESAG               PIC  X(002)         VALUE SPACES.
       77  WRK-FS-RELADAG              PIC  X(002)         VALUE SPACES.

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

       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.
       77  WRK-INICIO                  PIC  X(010)         VALUE SPACES.
       77  WRK-FIM                     PIC  X(010)         VALUE SPACES.
       77  WRK-FIM-CURSOR              PIC  X(001)         VALUE SPACES.
       77  WRK-AGENCIA                 PIC  9(005)         VALUE ZEROS.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA MENSAGENS *'.
      *---------------------------------------------------------------*

       01  WRK-MENSAGENS.
           05 WRK-MSG001               PIC  X(053)         VALUE
              '** ARQUIVO CLIEADES ESTA VAZIO, PROGRAMA CANCELADO **'.

           05 WRK-MSG002               PIC  X(053)         VALUE
              '** ESTOURO DE TABELA INTERNA1 **'.

           05 WRK-MSG003               PIC  X(053)         VALUE
              '** ESTOURO DE TABELA INTERNA2 **'.

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

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* MENSAGEM DE ERRO DE FILE-STATUS *'.
      *---------------------------------------------------------------*

       01  WRK-ERRO-TEXTO.
           05 FILLER                   PIC  X(008)         VALUE
              '** ERRO '.
           05 WRK-OPERACAO             PIC  X(013)         VALUE SPACES.
           05 FILLER                   PIC  X(012)         VALUE
              ' DO ARQUIVO '.
           05 WRK-NOME-ARQ             PIC  X(008)         VALUE SPACES.
           05 FILLER                   PIC  X(017)         VALUE
              ' - FILE STATUS = '.
           05 WRK-FILE-STATUS          PIC  X(002)         VALUE SPACES.
           05 FILLER                   PIC  X(003)         VALUE ' **'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DA TABELA INTERNA 1 *'.
      *----------------------------------------------------------------*

       01  TAB-TABELA1.
           05 TAB-1-DADOS              OCCURS 300 TIMES.
             10 TAB-1-PACOTE           PIC  9(005)         VALUE ZEROS.
             10 TAB-1-AGPTO            PIC  9(003)         VALUE ZEROS.
             10 TAB-1-SEQ              PIC  9(009)         VALUE ZEROS.
             10 TAB-1-DINI             PIC  X(010)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DA TABELA INTERNA 2 *'.
      *----------------------------------------------------------------*

       01  TAB-TABELA2.
           05 TAB-2-AGENCIA            PIC  9(005)         VALUE ZEROS.
           05 TAB-2-DADOS              OCCURS 300 TIMES.
             10 TAB-2-PACOTE           PIC  9(005)         VALUE ZEROS.
             10 TAB-2-AGPTO            PIC  9(003)         VALUE ZEROS.
             10 TAB-2-SEQ              PIC  9(009)         VALUE ZEROS.
             10 TAB-2-DINI             PIC  X(010)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DA BRAD7600 *'.
      *----------------------------------------------------------------*

       01  WRK-DATA-HORA.
           05 WRK-DATA-JULIANA         PIC  9(005) COMP-3  VALUE ZEROS.
           05 WRK-DATA-AAMMDD          PIC  9(007) COMP-3  VALUE ZEROS.
           05 WRK-DATA-AAAAMMDD        PIC  9(009) COMP-3  VALUE ZEROS.
           05 WRK-HORA-HHMMSS          PIC  9(007) COMP-3  VALUE ZEROS.
           05 WRK-HORA-HHMMSSMMMMMM    PIC  9(013) COMP-3  VALUE ZEROS.
           05 WRK-TIMESTAMP.
            10 WRK-ANO-7600            PIC  9(004)         VALUE ZEROS.
            10 WRK-MES-7600            PIC  9(002)         VALUE ZEROS.
            10 WRK-DIA-7600            PIC  9(002)         VALUE ZEROS.
            10 WRK-HOR-7600            PIC  9(002)         VALUE ZEROS.
            10 WRK-MIN-7600            PIC  9(002)         VALUE ZEROS.
            10 WRK-SEG-7600            PIC  9(002)         VALUE ZEROS.
            10 FILLER                  PIC  X(006)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA CABECALHOS DE RELATORIO *'.
      *----------------------------------------------------------------*

       01  CABEC1-REL1-AREA.
           05  FILLER                  PIC  X(001)         VALUE '1'.
           05  CB1-REL1-DATA.
               10  CB1-REL1-DIA        PIC  9(002)         VALUE ZEROS.
               10  FILLER              PIC  X(001)         VALUE '/'.
               10  CB1-REL1-MES        PIC  9(002)         VALUE ZEROS.
               10  FILLER              PIC  X(001)         VALUE '/'.
               10  CB1-REL1-ANO        PIC  9(004)         VALUE ZEROS.
           05  FILLER                  PIC  X(040)         VALUE SPACES.
           05  FILLER                  PIC  X(071)         VALUE
               'B A N C O  B R A D E S C O  S/A'.
           05  CB1-REL1-HORA.
               10  CB1-REL1-HOR        PIC  9(002)         VALUE ZEROS.
               10  FILLER              PIC  X(001)         VALUE ':'.
               10  CB1-REL1-MIN        PIC  9(002)         VALUE ZEROS.
               10  FILLER              PIC  X(001)         VALUE ':'.
               10  CB1-REL1-SEG        PIC  9(002)         VALUE ZEROS.

       01  CABEC2-REL1-AREA.
           05  FILLER                  PIC  X(001)         VALUE ' '.
           05  FILLER                  PIC  X(038)         VALUE
               'GFCT9305'.
           05  FILLER                  PIC  X(083)         VALUE
               'GFCT - GESTAO, FLEXIBILIZACAO E COBRANCA DE  TARIFAS'.
           05  FILLER                  PIC  X(004)         VALUE
               'PAG.'.
           05  CB2-REL1-PAG            PIC  ZZ99           VALUE ZEROS.

       01  CABEC3-REL1-AREA.
           05  FILLER                  PIC  X(001)         VALUE ' '.
           05  FILLER                  PIC  X(035)         VALUE SPACES.
           05  FILLER                  PIC  X(064)         VALUE
               'RELATORIO DE CLIENTES PARTICIPANTES DE ADESOES POR AGENC
      -        'IA'.

       01  CABEC4-REL1-AREA.
           05  FILLER                  PIC  X(001)         VALUE ' '.
           05  FILLER                  PIC  X(130)         VALUE
               ALL '-'.

       01  CABEC5-REL1-AREA.
           05  FILLER                  PIC  X(001)         VALUE ' '.
           05  FILLER                  PIC  X(050)         VALUE SPACES.
           05  FILLER                  PIC  X(013)         VALUE
               'MOVIMENTO DE '.
           05  CB5-REL1-DATA           PIC  X(010)         VALUE SPACES.

       01  CABEC6-REL1-AREA.
           05  FILLER                  PIC  X(001)         VALUE '0'.
           05  FILLER                  PIC  X(132)         VALUE
               'AGENCIA        CONTA       PACOTE     AGRUPAMENTO     SE
      -        'QUENCIA      DATA DE INICIO'.

       01  CABEC7-REL1-AREA.
           05  FILLER                  PIC  X(001)         VALUE '-'.
           05  FILLER                  PIC  X(032)         VALUE SPACES.
           05  FILLER                  PIC  X(065)         VALUE
               '----------------------- TOTALIZADOR FINAL --------------
      -        '---------'.

       01  CABEC8-REL1-AREA.
           05  FILLER                  PIC  X(001)         VALUE ' '.
           05  FILLER                  PIC  X(132)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA DETALHES DE RELATORIO *'.
      *----------------------------------------------------------------*

       01  LINDET1-REL1-AREA.
           05  FILLER                  PIC  X(001)         VALUE ' '.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  LD1-REL1-AGENCIA        PIC  9(005)         VALUE ZEROS.
           05  FILLER                  PIC  X(006)         VALUE SPACES.
           05  LD1-REL1-CONTA          PIC  9(007)         VALUE ZEROS.
           05  FILLER                  PIC  X(008)         VALUE SPACES.
           05  LD1-REL1-PACOTE         PIC  9(005)         VALUE ZEROS.
           05  FILLER                  PIC  X(008)         VALUE SPACES.
           05  LD1-REL1-AGRUPAMENTO    PIC  9(003)         VALUE ZEROS.
           05  FILLER                  PIC  X(011)         VALUE SPACES.
           05  LD1-REL1-SEQUENCIA      PIC  9(009)         VALUE ZEROS.
           05  FILLER                  PIC  X(008)         VALUE SPACES.
           05  LD1-REL1-DT-INICIO      PIC  X(010)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA LINHAS DE TOTAIS DO RELATORIO *'.
      *----------------------------------------------------------------*

       01  LINTOT1-REL1-AREA.
           05  FILLER                 PIC  X(001)          VALUE '-'.
           05  FILLER                 PIC  X(037)          VALUE
               'QUANTIDADE TOTAL DE CONTAS DA AGENCIA'.
           05  LT1-REL1-AGENCIA       PIC  ZZZZ9           VALUE ZEROS.
           05  FILLER                 PIC  X(003)          VALUE ' : '.
           05  LT1-REL1-TOTAL         PIC  Z.ZZZ.ZZZ.ZZ9   VALUE ZEROS.

       01  LINTOT2-REL1-AREA.
           05  FILLER                 PIC  X(001)          VALUE '-'.
           05  FILLER                 PIC  X(032)          VALUE SPACES.
           05  FILLER                 PIC  X(052)          VALUE
               '1 - QUANTIDADE DE CLIENTES LIDOS EM CLIEADES.......:'.
           05  LT2-REL1-TOTAL         PIC  ZZZ.ZZZ.ZZZ.ZZ9 VALUE ZEROS.

       01  LINTOT3-REL1-AREA.
           05  FILLER                 PIC  X(001)          VALUE '0'.
           05  FILLER                 PIC  X(032)          VALUE SPACES.
           05  FILLER                 PIC  X(052)          VALUE
               '2 - QUANTIDADE DE SOLICITACOES DE ADESAO...........:'.
           05  LT3-REL1-TOTAL         PIC  ZZZ.ZZZ.ZZZ.ZZ9 VALUE ZEROS.

       01  LINTOT4-REL1-AREA.
           05  FILLER                 PIC  X(001)          VALUE '0'.
           05  FILLER                 PIC  X(032)          VALUE SPACES.
           05  FILLER                 PIC  X(052)          VALUE
               '3 - QUANTIDADE DE CLIENTES GRAVADOS EM ADESAG......:'.
           05  LT4-REL1-TOTAL         PIC  ZZZ.ZZZ.ZZZ.ZZ9 VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA TRATAMENTO DE ERRO *'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

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
             INCLUDE GFCTB002
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA CURSOR *'.
      *----------------------------------------------------------------*

           EXEC SQL
             DECLARE   CSR01-GFCTB002-JN   CURSOR   FOR
               SELECT
                  A.CSERVC_TARIF,
                  A.CAGPTO_CTA,
                  A.CSEQ_AGPTO_CTA,
                  A.DINIC_ADSAO_COMP,
                  A.CEMPR_INC,
                  A.CDEPDC,
                  B.DFIM_ADSAO_COMP
             FROM DB2PRD.ADSAO_GRP_DEPDC  A,
                  DB2PRD.ADSAO_COMP_PCOTE B
             WHERE
                  A.CSERVC_TARIF       = B.CSERVC_TARIF             AND
                  A.CAGPTO_CTA         = B.CAGPTO_CTA               AND
                  A.CSEQ_AGPTO_CTA     = B.CSEQ_AGPTO_CTA           AND
                  A.DINIC_ADSAO_COMP   = B.DINIC_ADSAO_COMP         AND
                  B.DINIC_ADSAO_COMP  <= :GFCTB001.DINIC-ADSAO-COMP AND
                  B.DFIM_ADSAO_COMP   >= :GFCTB001.DFIM-ADSAO-COMP
             ORDER BY
                  A.CDEPDC,
                  A.CSERVC_TARIF
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(030)         VALUE
           'FIM DA WORKING STORAGE SECTION'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      ******************************************************************
      *    CHAMA AS ROTINAS A SEREM PROCESSADAS
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

           CALL 'BRAD1050'.

           OPEN INPUT  CLIEADES
                OUTPUT ADESAG
                       RELADAG.

           MOVE  WRK-ABERTURA          TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

           PERFORM 1200-OBTER-DATA-SISTEMA.

           PERFORM 1300-OBTER-PARAMETROS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTE DE FILE STATUS DOS ARQUIVOS                           *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-CLIEADES.

           PERFORM 1120-TESTAR-FS-ADESAG.

           PERFORM 1130-TESTAR-FS-RELADAG.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTE DE FILE STATUS DO ARQUIVO CLIEADES                    *
      ******************************************************************
      *----------------------------------------------------------------*
       1110-TESTAR-FS-CLIEADES         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-CLIEADES         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-CLIEADES    TO WRK-FILE-STATUS
               MOVE 'CLIEADES'         TO WRK-NOME-ARQ
               MOVE WRK-ERRO-TEXTO     TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTE DE FILE STATUS DO ARQUIVO ADESAG                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1120-TESTAR-FS-ADESAG           SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ADESAG           NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-ADESAG      TO WRK-FILE-STATUS
               MOVE 'ADESAG'           TO WRK-NOME-ARQ
               MOVE WRK-ERRO-TEXTO     TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTE DE FILE STATUS DO ARQUIVO RELADAG                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1130-TESTAR-FS-RELADAG          SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-RELADAG          NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-RELADAG     TO WRK-FILE-STATUS
               MOVE 'RELADAG '         TO WRK-NOME-ARQ
               MOVE WRK-ERRO-TEXTO     TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    OBTEM DATA E HORA DO SISTEMA                                *
      ******************************************************************
      *----------------------------------------------------------------*
       1200-OBTER-DATA-SISTEMA         SECTION.
      *----------------------------------------------------------------*

           CALL 'BRAD7600'             USING WRK-DATA-HORA.

           MOVE WRK-DIA-7600           TO CB1-REL1-DIA.
           MOVE WRK-MES-7600           TO CB1-REL1-MES.
           MOVE WRK-ANO-7600           TO CB1-REL1-ANO.
           MOVE WRK-HOR-7600           TO CB1-REL1-HOR.
           MOVE WRK-MIN-7600           TO CB1-REL1-MIN.
           MOVE WRK-SEG-7600           TO CB1-REL1-SEG.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    OBTEM PARAMETROS ATRAVES DA TABELA GFCTB0A1                 *
      ******************************************************************
      *----------------------------------------------------------------*
       1300-OBTER-PARAMETROS           SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC      OF GFCTB0A1.

           EXEC SQL
             SELECT
                   DPROCM_ANTER,
                   DPROCM_ATUAL
             INTO
                   :GFCTB0A1.DPROCM-ANTER,
                   :GFCTB0A1.DPROCM-ATUAL
             FROM   DB2PRD.PARM_DATA_PROCM
             WHERE
                   CSIST_PRINC        = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PARM_DATA_PROCM ' TO ERR-DBD-TAB
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0010'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 1350-OBTER-NOVA-DATA-PROC.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1350-OBTER-NOVA-DATA-PROC       SECTION.
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
       1350-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICA SE O ARQUIVO DE ENTRADA ESTA VAZIO                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-LER-CLIEADES.

           IF  WRK-FS-CLIEADES         EQUAL '10'
               DISPLAY '*************** GFCT9305 **************'
               DISPLAY '*                                     *'
               DISPLAY '*         ARQUIVO CLIEADES VAZIO      *'
               DISPLAY '*                                     *'
               DISPLAY '*        PROCESSAMENTO CANCELADO      *'
               DISPLAY '*                                     *'
               DISPLAY '*************** GFCT9305 **************'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-MSG001         TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE GFCTRU-AGENCIA         TO WRK-GFCTRU-AGENCIA.
           MOVE WRK-COD-AGENCIA        TO WRK-AGENCIA.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    LEITURA DO ARQUIVO DE PARAMETROS CLIEADES                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-LER-CLIEADES               SECTION.
      *----------------------------------------------------------------*

           READ CLIEADES.

           IF  WRK-FS-CLIEADES         EQUAL '10'
               GO TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1110-TESTAR-FS-CLIEADES.

           ADD 1                       TO ACU-LIDOS-CLIEADES.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCESSAMENTO PRINCIPAL PARA CADA REGISTRO DO ADESAG       *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 3100-OBTER-DATAS.

           PERFORM 3200-ABRIR-CURSOR.

           PERFORM 3300-LER-CURSOR.

           IF  WRK-FIM-CURSOR            EQUAL 'S'
               PERFORM 3400-EMITIR-DISPLAY
           END-IF.

           PERFORM 3500-PROCESSAR-CURSOR UNTIL
                   WRK-FIM-CURSOR        EQUAL 'S' OR
                   WRK-FS-CLIEADES       EQUAL '10'.

           PERFORM 3600-FECHAR-CURSOR.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    OBTEM DATAS                                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       3100-OBTER-DATAS                SECTION.
      *----------------------------------------------------------------*

PWI        MOVE DPROCM-ANTER OF GFCTB0A1 TO DFIM-ADSAO-COMP
                                            OF GFCTB001.
PWI        MOVE DPROCM-ANTER OF GFCTB0A1 TO WRK-DATA-AUX
PWI        MOVE WRK-DIA-AUX              TO WRK-DIA-AUX1
PWI        MOVE WRK-MES-AUX              TO WRK-MES-AUX1
PWI        MOVE WRK-ANO-AUX              TO WRK-ANO-AUX1.
PWI
PWI        IF WRK-MES-AUX1            LESS 12
PWI           MOVE WRK-MES-AUX1       TO WRK-MES
PWI
PWI           COMPUTE WRK-MES         = WRK-MES + 1
PWI
PWI           MOVE WRK-MES            TO WRK-MES-AUX1
PWI        ELSE
PWI           MOVE 01                 TO WRK-MES-AUX1
PWI
PWI           MOVE WRK-ANO-AUX1       TO WRK-ANO
PWI
PWI           COMPUTE WRK-ANO         = WRK-ANO + 1
PWI
PWI           MOVE WRK-ANO            TO WRK-ANO-AUX1
PWI        END-IF.
PWI
PWI        MOVE 01                       TO WRK-DIA-AUX.
PWI        MOVE WRK-MES-AUX1             TO WRK-MES-AUX.
PWI        MOVE WRK-ANO-AUX1             TO WRK-ANO-AUX.
PWI        MOVE WRK-DATA-AUX             TO DINIC-ADSAO-COMP
                                            OF GFCTB001.
      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ABRE O CURSOR                                               *
      ******************************************************************
      *----------------------------------------------------------------*
       3200-ABRIR-CURSOR               SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FIM-CURSOR.

           EXEC SQL
               OPEN CSR01-GFCTB002-JN
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'ADSAO_GRP_DEPDC'  TO ERR-DBD-TAB
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
      *    LE O CURSOR                                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       3300-LER-CURSOR                 SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH CSR01-GFCTB002-JN INTO
                  :GFCTB002.CSERVC-TARIF,
                  :GFCTB002.CAGPTO-CTA,
                  :GFCTB002.CSEQ-AGPTO-CTA,
                  :GFCTB002.DINIC-ADSAO-COMP,
                  :GFCTB002.CEMPR-INC,
                  :GFCTB002.CDEPDC,
                  :GFCTB001.DFIM-ADSAO-COMP
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'               )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'ADSAO_GRP_DEPDC'  TO ERR-DBD-TAB
               MOVE 'FETCH   '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0030'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-CURSOR
               MOVE 99999              TO CDEPDC           OF GFCTB002
           ELSE
               ADD 1                   TO ACU-LIDOS-CURSOR
           END-IF.

           MOVE CDEPDC OF GFCTB002     TO WRK-CDEPDC.

      *----------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    EXIBE INFORMACOES DO PROCESSAMENTO                          *
      ******************************************************************
      *----------------------------------------------------------------*
       3400-EMITIR-DISPLAY             SECTION.
      *----------------------------------------------------------------*

           DISPLAY '*************** GFCT9305 ***************'.
           DISPLAY '*                                      *'.
           DISPLAY '*   NAO EXISTE SOLICITACAO DE ADESAO   *'.
           DISPLAY '*   AGENCIA PARA O PERIODO DE:         *'.
           DISPLAY '*   DATA INICIO : ' WRK-INICIO '           *'.
           DISPLAY '*   DATA FINAL  : ' WRK-FIM    '           *'.
           DISPLAY '*                                      *'.
           DISPLAY '*           TERMINO NORMAL             *'.
           DISPLAY '*                                      *'.
           DISPLAY '*************** GFCT9305 ***************'.

      *----------------------------------------------------------------*
       3400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCESSA O CURSOR                                           *
      ******************************************************************
      *----------------------------------------------------------------*
       3500-PROCESSAR-CURSOR           SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO TAB-TABELA1.

           INITIALIZE TAB-TABELA1
                      IND-1.

           IF  CDEPDC                  OF GFCTB002 EQUAL ZEROS
               PERFORM 3510-GUARDAR-PCTES-AGE-ZERO UNTIL
                       CDEPDC          OF GFCTB002 NOT EQUAL ZEROS OR
                       WRK-FIM-CURSOR  EQUAL 'S'
           END-IF.

           PERFORM 3520-PROCESSAR-DEMAIS-PCTES     UNTIL
                   WRK-FS-CLIEADES                 EQUAL '10'.

      *----------------------------------------------------------------*
       3500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    GUARDAR PACOTES AGE ZERO                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       3510-GUARDAR-PCTES-AGE-ZERO     SECTION.
      *----------------------------------------------------------------*

           ADD 1                       TO IND-1.

           IF  IND-1                   GREATER 300
               DISPLAY '*************** GFCT9305 **************'
               DISPLAY '*                                     *'
               DISPLAY '*      ESTOURO DE TABELA INTERNA1     *'
               DISPLAY '*                                     *'
               DISPLAY '*       PROCESSAMENTO  CANCELADO      *'
               DISPLAY '*                                     *'
               DISPLAY '*************** GFCT9305 **************'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-MSG002         TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE CSERVC-TARIF           OF GFCTB002
                                       TO TAB-1-PACOTE(IND-1).
           MOVE CAGPTO-CTA             OF GFCTB002
                                       TO TAB-1-AGPTO(IND-1).
           MOVE CSEQ-AGPTO-CTA         OF GFCTB002
                                       TO TAB-1-SEQ(IND-1).
           MOVE DINIC-ADSAO-COMP       OF GFCTB002
                                       TO TAB-1-DINI(IND-1).

           PERFORM 3300-LER-CURSOR.

      *----------------------------------------------------------------*
       3510-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCESSAR DEMAIS PACOTES                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       3520-PROCESSAR-DEMAIS-PCTES     SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO TAB-TABELA2.

           INITIALIZE TAB-TABELA2
                      IND-2.

           MOVE WRK-COD-DEPDC          TO TAB-2-AGENCIA.

           PERFORM 3521-MONTAR-TABELA2 UNTIL
                   WRK-COD-DEPDC       GREATER TAB-2-AGENCIA OR
                   WRK-FIM-CURSOR      EQUAL 'S'.

           MOVE GFCTRU-AGENCIA         TO WRK-GFCTRU-AGENCIA.

           PERFORM 3522-DESCARREGAR-AG-PCTES-BONS UNTIL
                   WRK-COD-AGENCIA     GREATER TAB-2-AGENCIA OR
                   WRK-FS-CLIEADES     EQUAL '10'.

           IF  CDEPDC      OF GFCTB002 EQUAL 99999 AND
               WRK-FS-CLIEADES         NOT EQUAL '10'
               MOVE CDEPDC OF GFCTB002 TO TAB-2-AGENCIA
               PERFORM 3522-DESCARREGAR-AG-PCTES-BONS UNTIL
                       WRK-FS-CLIEADES EQUAL '10'
           END-IF.

      *----------------------------------------------------------------*
       3520-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    MONTA TABELA2                                               *
      ******************************************************************
      *----------------------------------------------------------------*
       3521-MONTAR-TABELA2             SECTION.
      *----------------------------------------------------------------*

           ADD 1                       TO IND-2.

           IF  IND-2                   GREATER 300
               DISPLAY '*************** GFCT9305 **************'
               DISPLAY '*                                     *'
               DISPLAY '*      ESTOURO DE TABELA INTERNA2     *'
               DISPLAY '*                                     *'
               DISPLAY '*       PROCESSAMENTO  CANCELADO      *'
               DISPLAY '*                                     *'
               DISPLAY '*************** GFCT9305 **************'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-MSG003         TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE CSERVC-TARIF           OF GFCTB002
                                       TO TAB-2-PACOTE(IND-2).
           MOVE CAGPTO-CTA             OF GFCTB002
                                       TO TAB-2-AGPTO(IND-2).
           MOVE CSEQ-AGPTO-CTA         OF GFCTB002
                                       TO TAB-2-SEQ(IND-2).
           MOVE DINIC-ADSAO-COMP       OF GFCTB002
                                       TO TAB-2-DINI(IND-2).

           PERFORM 3300-LER-CURSOR.

      *----------------------------------------------------------------*
       3521-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    DESCARREGAR PAB COM PACOTES BONS                            *
      ******************************************************************
      *----------------------------------------------------------------*
       3522-DESCARREGAR-AG-PCTES-BONS  SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTRU-AGENCIA         TO WRK-GFCTRU-AGENCIA.

           IF  WRK-COD-AGENCIA                   LESS TAB-2-AGENCIA
               PERFORM 3523-PROCESSAR-AGENCIAS-ZERO
                   VARYING IND-1                 FROM  1 BY 1
                   UNTIL TAB-1-PACOTE(IND-1)     EQUAL ZEROS

               PERFORM 2100-LER-CLIEADES
           ELSE
               IF  WRK-COD-AGENCIA               EQUAL TAB-2-AGENCIA
                   PERFORM 3523-PROCESSAR-AGENCIAS-ZERO
                       VARYING IND-1             FROM  1 BY 1
                       UNTIL TAB-1-PACOTE(IND-1) EQUAL ZEROS

                   PERFORM 3530-PROCESSAR-PCTES-VALIDOS
                       VARYING IND-2             FROM  1 BY 1
                       UNTIL TAB-2-PACOTE(IND-2) EQUAL ZEROS

                   PERFORM 2100-LER-CLIEADES
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3522-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCESSAR AGENCIAS ZEROS                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       3523-PROCESSAR-AGENCIAS-ZERO    SECTION.
      *----------------------------------------------------------------*

           PERFORM 3524-MOVER-DADOS.

           MOVE TAB-1-PACOTE(IND-1)    TO GFCTRZ-PACOTE
                                          LD1-REL1-PACOTE.
           MOVE TAB-1-AGPTO (IND-1)    TO GFCTRZ-AGPTO
                                          LD1-REL1-AGRUPAMENTO.
           MOVE TAB-1-SEQ   (IND-1)    TO GFCTRZ-SEQ
                                          LD1-REL1-SEQUENCIA.
           MOVE TAB-1-DINI  (IND-1)    TO GFCTRZ-DATA
                                          LD1-REL1-DT-INICIO.

           INSPECT LD1-REL1-DT-INICIO REPLACING ALL '.' BY '/'.

           PERFORM 3525-GRAVAR-ADESAG.

      *----------------------------------------------------------------*
       3523-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    MOVER DADOS                                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       3524-MOVER-DADOS                SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTRU-AGENCIA         TO WRK-GFCTRU-AGENCIA.
           MOVE WRK-COD-AGENCIA        TO GFCTRZ-AGENCIA
                                          LD1-REL1-AGENCIA.

           MOVE GFCTRU-RAZAO           TO WRK-GFCTRU-RAZAO.
           MOVE WRK-COD-RAZAO          TO GFCTRZ-RAZAO.

           MOVE GFCTRU-POSTO-SERV      TO GFCTRZ-PAB.
           MOVE GFCTRU-CONTA           TO GFCTRZ-CONTA
                                          LD1-REL1-CONTA.

           PERFORM 3524-10-OBTER-PERIODO.

      *----------------------------------------------------------------*
       3524-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

PWI   *----------------------------------------------------------------*
PWI    3524-10-OBTER-PERIODO           SECTION.
PWI   *----------------------------------------------------------------*
PWI        MOVE DPROCM-ANTER OF GFCTB0A1 TO WRK-DATA-AUX
PWI        MOVE WRK-DIA-AUX              TO WRK-DIA-AUX1
PWI        MOVE WRK-MES-AUX              TO WRK-MES-AUX1
PWI        MOVE WRK-ANO-AUX              TO WRK-ANO-AUX1.
PWI
PWI*****   SALVA ANOMES DA DPROCM-ANTER
PWI
PWI        MOVE WRK-ANOMES-AUX1          TO WRK-ANOMES-AUX
PWI
PWI        MOVE DINIC-ADSAO-COMP       OF GFCTB002
PWI                                      TO WRK-DATA-AUX
PWI        MOVE WRK-DIA-AUX              TO WRK-DIA-AUX1
PWI        MOVE WRK-MES-AUX              TO WRK-MES-AUX1
PWI        MOVE WRK-ANO-AUX              TO WRK-ANO-AUX1.
PWI
PWI        IF WRK-ANOMES-AUX1 NOT GREATER WRK-ANOMES-AUX
PWI           PERFORM 3524-20-MONTA-NO-MES
PWI        ELSE
PWI           PERFORM 3524-30-MONTA-FUTURO
PWI        END-IF.
PWI
PWI   *----------------------------------------------------------------*
PWI    3524-10-99-FIM.                 EXIT.
PWI   *----------------------------------------------------------------*
PWI
PWI   *----------------------------------------------------------------*
PWI    3524-20-MONTA-NO-MES            SECTION.
PWI   *----------------------------------------------------------------*
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
PWI           PERFORM 3524-40-OBTER-DFIM
PWI        END-IF.
PWI
PWI   *----------------------------------------------------------------*
PWI    3524-20-99-FIM.                 EXIT.
PWI   *----------------------------------------------------------------*
PWI   *----------------------------------------------------------------*
PWI    3524-30-MONTA-FUTURO            SECTION.
PWI   *----------------------------------------------------------------*
PWI
PWI******  OBTER 1 MES SEGUINTE AO DA DPROCM-ANTER
PWI******  PARA OBTER PRIMEIRO DIA.
PWI
PWI        MOVE DPROCM-ANTER OF GFCTB0A1 TO WRK-DATA-AUX
PWI        MOVE WRK-DIA-AUX              TO WRK-DIA-AUX1
PWI        MOVE WRK-MES-AUX              TO WRK-MES-AUX1
PWI        MOVE WRK-ANO-AUX              TO WRK-ANO-AUX1.
PWI
PWI        IF WRK-MES-AUX1             LESS 12
PWI           MOVE WRK-MES-AUX1        TO WRK-MES
PWI
PWI           COMPUTE WRK-MES          = WRK-MES + 1
PWI
PWI           MOVE WRK-MES             TO WRK-MES-AUX1
PWI        ELSE
PWI           MOVE 01                  TO WRK-MES-AUX1
PWI
PWI           MOVE WRK-ANO-AUX1        TO WRK-ANO
PWI
PWI           COMPUTE WRK-ANO          = WRK-ANO + 1
PWI
PWI           MOVE WRK-ANO             TO WRK-ANO-AUX1
PWI        END-IF.
PWI
PWI        MOVE 01                    TO WRK-DIA-AUX1.
PWI        MOVE WRK-DATA-AUX1-R       TO GFCTRZ-DINI.
PWI
PWI        PERFORM 3524-40-OBTER-DFIM.
PWI
PWI   *----------------------------------------------------------------*
PWI    3524-30-99-FIM.                 EXIT.
PWI   *----------------------------------------------------------------*
PWI
PWI   *----------------------------------------------------------------*
PWI    3524-40-OBTER-DFIM              SECTION.
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
PWI           MOVE WRK-MES-AUX1       TO WRK-MES
PWI
PWI           COMPUTE WRK-MES         = WRK-MES + 1
PWI
PWI           MOVE WRK-MES            TO WRK-MES-AUX1
PWI        ELSE
PWI           MOVE 01                 TO WRK-MES-AUX1
PWI
PWI           MOVE WRK-ANO-AUX1       TO WRK-ANO
PWI
PWI           COMPUTE WRK-ANO         = WRK-ANO + 1
PWI
PWI           MOVE WRK-ANO            TO WRK-ANO-AUX1
PWI        END-IF.
PWI
PWI        IF WRK-MES-AUX1            LESS 12
PWI           MOVE WRK-MES-AUX1       TO WRK-MES
PWI
PWI           COMPUTE WRK-MES         = WRK-MES + 1
PWI
PWI           MOVE WRK-MES            TO WRK-MES-AUX1
PWI        ELSE
PWI           MOVE 01                 TO WRK-MES-AUX1
PWI
PWI           MOVE WRK-ANO-AUX1       TO WRK-ANO
PWI
PWI           COMPUTE WRK-ANO         = WRK-ANO + 1
PWI
PWI           MOVE WRK-ANO            TO WRK-ANO-AUX1
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
PWI    3524-40-99-FIM.                 EXIT.
PWI   *----------------------------------------------------------------*

      ******************************************************************
      *    GRAVAR ARQUIVO DE SAIDA ADESAG                             *
      ******************************************************************
      *----------------------------------------------------------------*
       3525-GRAVAR-ADESAG              SECTION.
      *----------------------------------------------------------------*

           WRITE GFCTRZ-ADESOES.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 1120-TESTAR-FS-ADESAG.

           ADD 1                       TO ACU-GRAVA-ADESAG.

           PERFORM 3526-EMITIR-RELATORIO.

      *----------------------------------------------------------------*
       3525-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    GERA RELATORIO                                              *
      ******************************************************************
      *----------------------------------------------------------------*
       3526-EMITIR-RELATORIO           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           IF  WRK-AGENCIA             NOT EQUAL LD1-REL1-AGENCIA
               PERFORM 3528-EMITIR-TOTAIS-AGENCIA
               PERFORM 3529-VERIFICAR-QUEBRA
               MOVE ZEROS              TO ACU-AGENCIA
           ELSE
               IF  ACU-LINHAS          GREATER 59
                   PERFORM 3527-EMITIR-CABECALHOS
                   WRITE FD-RELADAG    FROM CABEC6-REL1-AREA
                   PERFORM 1130-TESTAR-FS-RELADAG
                   WRITE FD-RELADAG    FROM CABEC4-REL1-AREA
                   PERFORM 1130-TESTAR-FS-RELADAG
                   ADD 3               TO ACU-LINHAS
               END-IF
           END-IF.

           WRITE FD-RELADAG            FROM LINDET1-REL1-AREA.

           PERFORM 1130-TESTAR-FS-RELADAG.

           MOVE LD1-REL1-AGENCIA       TO WRK-AGENCIA.

           ADD 1                       TO ACU-LINHAS
                                          ACU-AGENCIA.

      *----------------------------------------------------------------*
       3526-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    IMPRIMIR CABECALHOS                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       3527-EMITIR-CABECALHOS          SECTION.
      *----------------------------------------------------------------*

           ADD 1                       TO ACU-PAGINAS.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           WRITE FD-RELADAG            FROM CABEC1-REL1-AREA.

           PERFORM 1130-TESTAR-FS-RELADAG.

           MOVE ACU-PAGINAS            TO CB2-REL1-PAG.

           WRITE FD-RELADAG            FROM CABEC2-REL1-AREA.

           PERFORM 1130-TESTAR-FS-RELADAG.

           WRITE FD-RELADAG            FROM CABEC3-REL1-AREA.

           PERFORM 1130-TESTAR-FS-RELADAG.

           WRITE FD-RELADAG            FROM CABEC4-REL1-AREA.

           PERFORM 1130-TESTAR-FS-RELADAG.

           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO CB5-REL1-DATA.

           INSPECT CB5-REL1-DATA       REPLACING ALL '.' BY '/'.

           WRITE FD-RELADAG            FROM CABEC5-REL1-AREA.

           PERFORM 1130-TESTAR-FS-RELADAG.

           WRITE FD-RELADAG            FROM CABEC4-REL1-AREA.

           PERFORM 1130-TESTAR-FS-RELADAG.

           MOVE 06                     TO ACU-LINHAS.

      *----------------------------------------------------------------*
       3527-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    IMPRIMIR TOTAIS DA AGENCIA                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       3528-EMITIR-TOTAIS-AGENCIA      SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-AGENCIA            TO LT1-REL1-AGENCIA.
           MOVE ACU-AGENCIA            TO LT1-REL1-TOTAL.

           WRITE FD-RELADAG            FROM LINTOT1-REL1-AREA.

           PERFORM 1130-TESTAR-FS-RELADAG.

           ADD 3                       TO ACU-LINHAS.

           MOVE ZEROS                  TO WRK-AGENCIA.

      *----------------------------------------------------------------*
       3528-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  VERIFICA A NECESSIDADE DE QUEBRA DE PAGINA APOS IMP. O TOTAL  *
      ******************************************************************
      *----------------------------------------------------------------*
       3529-VERIFICAR-QUEBRA           SECTION.
      *----------------------------------------------------------------*

           IF  ACU-LINHAS              NOT GREATER 55
               WRITE FD-RELADAG        FROM CABEC8-REL1-AREA
               PERFORM 1130-TESTAR-FS-RELADAG
               WRITE FD-RELADAG        FROM CABEC6-REL1-AREA
               PERFORM 1130-TESTAR-FS-RELADAG
               WRITE FD-RELADAG        FROM CABEC4-REL1-AREA
               PERFORM 1130-TESTAR-FS-RELADAG
               ADD  04                 TO ACU-LINHAS
           ELSE
               PERFORM 3527-EMITIR-CABECALHOS
               WRITE FD-RELADAG        FROM CABEC6-REL1-AREA
               PERFORM 1130-TESTAR-FS-RELADAG
               WRITE FD-RELADAG        FROM CABEC4-REL1-AREA
               PERFORM 1130-TESTAR-FS-RELADAG
               ADD 3                   TO ACU-LINHAS
           END-IF.

      *----------------------------------------------------------------*
       3529-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCESSAR PACOTES VALIDOS                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       3530-PROCESSAR-PCTES-VALIDOS    SECTION.
      *----------------------------------------------------------------*

           PERFORM 3524-MOVER-DADOS.

           MOVE TAB-2-PACOTE(IND-2)    TO GFCTRZ-PACOTE
                                          LD1-REL1-PACOTE.
           MOVE TAB-2-AGPTO (IND-2)    TO GFCTRZ-AGPTO
                                          LD1-REL1-AGRUPAMENTO.
           MOVE TAB-2-SEQ   (IND-2)    TO GFCTRZ-SEQ
                                          LD1-REL1-SEQUENCIA.
           MOVE TAB-2-DINI  (IND-2)    TO GFCTRZ-DATA
                                          LD1-REL1-DT-INICIO.

           INSPECT LD1-REL1-DT-INICIO REPLACING ALL '.' BY '/'.

           PERFORM 3525-GRAVAR-ADESAG.

      *----------------------------------------------------------------*
       3530-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    FECHA  CURSOR                                               *
      ******************************************************************
      *----------------------------------------------------------------*
       3600-FECHAR-CURSOR              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR01-GFCTB002-JN
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'ADSAO_GRP_DEPDC'    TO ERR-DBD-TAB
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
      *    PROCEDIMENTOS REALIZADOS NO FINAL DO PROCESSAMENTO          *
      ******************************************************************
      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           IF  ACU-GRAVA-ADESAG        GREATER ZEROS
               PERFORM 3528-EMITIR-TOTAIS-AGENCIA
           END-IF.

           PERFORM 4100-TOTALIZAR-RELATORIO.

           PERFORM 4200-EMITIR-DISP-FINAL

           CLOSE CLIEADES
                 ADESAG
                 RELADAG.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

           STOP RUN.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    EMITE TOTAIS DO RELATORIO                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       4100-TOTALIZAR-RELATORIO        SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 3527-EMITIR-CABECALHOS.

           WRITE FD-RELADAG            FROM CABEC7-REL1-AREA.

           PERFORM 1130-TESTAR-FS-RELADAG.

           MOVE ACU-LIDOS-CLIEADES     TO LT2-REL1-TOTAL.

           WRITE FD-RELADAG            FROM LINTOT2-REL1-AREA.

           PERFORM 1130-TESTAR-FS-RELADAG.

           MOVE ACU-LIDOS-CURSOR       TO LT3-REL1-TOTAL.

           WRITE FD-RELADAG            FROM LINTOT3-REL1-AREA.

           PERFORM 1130-TESTAR-FS-RELADAG.

           MOVE ACU-GRAVA-ADESAG       TO LT4-REL1-TOTAL.

           WRITE FD-RELADAG            FROM LINTOT4-REL1-AREA.

           PERFORM 1130-TESTAR-FS-RELADAG.

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    EXIBE INFORMACOES DO PROCESSAMENTO                          *
      ******************************************************************
      *----------------------------------------------------------------*
       4200-EMITIR-DISP-FINAL          SECTION.
      *----------------------------------------------------------------*

           DISPLAY '*************** GFCT9305 ***************'.
           DISPLAY '*                                      *'.
           DISPLAY '*   PROGRAMA ENCERRADO COM SUCESSO     *'.
           DISPLAY '*                                      *'.
           DISPLAY '*   PROCESSAMENTO DE : ' DPROCM-ATUAL OF GFCTB0A1 '
      -            '     *'.
           DISPLAY '*                                      *'.
           DISPLAY '*           TERMINO NORMAL             *'.
           DISPLAY '*                                      *'.
           DISPLAY '*************** GFCT9305 ***************'.

      *----------------------------------------------------------------*
       4200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    EXECUTA PROCEDIMENTO DE CANCELAMENTO DO PGM EM CASO DE ERRO *
      ******************************************************************
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT9305'             TO ERR-PGM.

           CALL 'BRAD7100'             USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA.
           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
