      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT9308.
       AUTHOR.     JORGE VARGAS.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT9308                                    *
      *    PROGRAMADOR.:   JORGE VARGAS             - CPM PATO BRANCO  *
      *    ANALISTA CPM:   MARCIO CUSTIN            - CPM PATO BRANCO  *
      *    ANALISTA....:   MARCELO CREMM            - PROCKWORK/ GP 50 *
      *    DATA........:   25/05/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   GERACAO DE MOVIMENTO DE ADESAO COMPULSORIA -*
      *       UF.                                                      *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                       INCLUDE/BOOK       *
      *                    CLIEUF                     I#GFCTRY         *
      *                    ADESUF                     I#GFCTRZ         *
      *                    RELADUF                       -             *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                      INCLUDE/BOOK       *
      *                    DB2PRD.PARM_DATA_PROCM     GFCTB0A1         *
      *                    DB2PRD.ADSAO_COMP_PCOTE    GFCTB001         *
      *                    DB2PRD.ADSAO_GRP_UF        GFCTB008         *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    I#BRAD7C - AREA PARA FORMATACAO DE ERROS DA BRAD7100        *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    BRAD1050 - MODULO PARA CONEXAO COM DB2                      *
      *    BRAD1285 - SOMAR OU SUBTRAIR DIAS UTEIS A UMA DATA          *
      *    BRAD7100 - CANCELAMENTO DO PROGRAMA                         *
      *    BRAD7600 - MODULO PARA OBTER DATA E HORA DO SISTEMA         *
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

           SELECT CLIEUF   ASSIGN      TO UT-S-CLIEUF
           FILE STATUS                 IS WRK-FS-CLIEUF.

           SELECT ADESUF   ASSIGN      TO UT-S-ADESUF
           FILE STATUS                 IS WRK-FS-ADESUF.

           SELECT RELADUF  ASSIGN      TO UT-S-RELADUF
           FILE STATUS                 IS WRK-FS-RELADUF.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:  ARQUIVO DE CLIENTES - ENTRADA                       *
      *            ORG. SEQUENCIAL     -   LRECL   =   050             *
      *----------------------------------------------------------------*

       FD  CLIEUF
           RECORDING MODE IS F
           LABEL RECORD   IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTRY'.

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO CLIENTES ADERIDOS                           *
      *            ORG. SEQUENCIAL     -   LRECL   =  040              *
      *----------------------------------------------------------------*

       FD  ADESUF
           RECORDING MODE IS F
           LABEL RECORD   IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTRZ'.

      *----------------------------------------------------------------*
      *    OUTPUT: RELATORIO ANALITICO E TOTALIZADOR                   *
      *            ORG. SEQUENCIAL     -   LRECL   =  133              *
      *----------------------------------------------------------------*

       FD  RELADUF
           RECORDING MODE IS F
           LABEL RECORD   IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-RELADUF                  PIC  X(133).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)          VALUE
           '*** INICIO DA WORKING STORAGE SECTION ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA INDEXADORES ***'.
      *----------------------------------------------------------------*

       77  IND-1                       PIC  9(003) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA ACUMULADORES ***'.
      *----------------------------------------------------------------*

       77  ACU-LINHAS                  PIC  9(005) COMP-3  VALUE 99.
       77  ACU-PAGINAS                 PIC  9(004) COMP-3  VALUE ZEROS.
       77  ACU-LIDOS-CLIEUF            PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-LIDOS-CURSOR1           PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-ADESUF            PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-UF                      PIC  9(003) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA TESTES DE FILE STATUS ***'.
      *----------------------------------------------------------------*

       77  WRK-FS-CLIEUF               PIC  X(002)         VALUE SPACES.
       77  WRK-FS-ADESUF               PIC  X(002)         VALUE SPACES.
       77  WRK-FS-RELADUF              PIC  X(002)         VALUE SPACES.

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
           '*** AREA PARA VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*

       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.
       77  WRK-FIM-CURSOR1             PIC  X(001)         VALUE SPACES.
       77  WRK-SIGLA-ANTERIOR          PIC  X(002)         VALUE SPACES.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA VARIAVEIS DE DATA ***'.
      *----------------------------------------------------------------*

       77  WRK-DDISPL                  PIC  X(010)         VALUE SPACES.
       77  WRK-DINIC                   PIC  X(010)         VALUE SPACES.
       77  WRK-DFIM                    PIC  X(010)         VALUE SPACES.

       01  WRK-MES                     PIC  9(002) COMP-3  VALUE ZEROS.

       01  WRK-ANO                     PIC  9(004) COMP-3  VALUE ZEROS.

       01  WRK-DATA-ALT.
           05  WRK-DIA-ALT             PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  WRK-MES-ALT             PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  WRK-ANO-ALT             PIC  9(004)         VALUE ZEROS.

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
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA ERRO DE FILE STATUS ***'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-ARQUIVO.
           05  FILLER                  PIC  X(008)         VALUE
               '** ERRO '.
           05  WRK-OPERACAO            PIC  X(013)         VALUE SPACES.
           05  FILLER                  PIC  X(012)         VALUE
               ' DO ARQUIVO '.
           05  WRK-NOME-ARQUIVO        PIC  X(008)         VALUE SPACES.
           05  FILLER                  PIC  X(017)         VALUE
               ' - FILE STATUS = '.
           05  WRK-FILE-STATUS         PIC  X(002)         VALUE SPACES.
           05  FILLER                  PIC  X(003)         VALUE ' **'.

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
           '*** AREA DA BRAD7600 ***'.
      *---------------------------------------------------------------*

       01  WRK-DATA-HORA.
           03  WRK-DT-JULIANA          PIC  9(005) COMP-3  VALUE ZEROS.
           03  WRK-DT-AAMMDD           PIC  9(007) COMP-3  VALUE ZEROS.
           03  WRK-DT-AAAAMMDD         PIC  9(009) COMP-3  VALUE ZEROS.
           03  WRK-TI-HHMMSS           PIC  9(007) COMP-3  VALUE ZEROS.
           03  WRK-TI-HHMMSSMMMMMM     PIC  9(013) COMP-3  VALUE ZEROS.
           03  WRK-TIMESTAMP.
             05  WRK-7600-ANO          PIC  9(004)         VALUE ZEROS.
             05  WRK-7600-MES          PIC  9(002)         VALUE ZEROS.
             05  WRK-7600-DIA          PIC  9(002)         VALUE ZEROS.
             05  WRK-7600-HOR          PIC  9(002)         VALUE ZEROS.
             05  WRK-7600-MIN          PIC  9(002)         VALUE ZEROS.
             05  WRK-7600-SEG          PIC  9(002)         VALUE ZEROS.
             05  FILLER                PIC  X(006)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA TABELA INTERNA ***'.
      *----------------------------------------------------------------*

       01  WRK-TABELA-INTERNA-1.
           05  WRK-TAB-UF              PIC  X(002)         VALUE SPACES.
           05  WRK-TAB-DADOS           OCCURS 300 TIMES.
             10  WRK-TAB-PACOTE        PIC  9(005).
             10  WRK-TAB-AGPTO         PIC  9(003).
             10  WRK-TAB-SEQ           PIC  9(009).
             10  WRK-TAB-DINI          PIC  X(010).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA RELATORIO ***'.
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
           05  FILLER                  PIC  X(045)         VALUE 'S/A'.
           05  CB1-REL1-HOR            PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE ':'.
           05  CB1-REL1-MIN            PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE ':'.
           05  CB1-REL1-SEG            PIC  9(002)         VALUE ZEROS.

       01  CABEC2-REL1.
           05  FILLER                  PIC  X(001)         VALUE ' '.
           05  FILLER                  PIC  X(037)         VALUE
               'GFCT9308'.
           05  FILLER                  PIC  X(045)         VALUE
               'GFCT - GESTAO, FLEXIBILIZACAO E COBRANCA DE'.
           05  FILLER                  PIC  X(038)         VALUE
               'TARIFAS'.
           05  FILLER                  PIC  X(004)         VALUE 'PAG.'.
           05  CB2-REL1-PAG            PIC  ZZ99           VALUE ZEROS.

       01  CABEC3-REL1.
           05  FILLER                  PIC  X(001)         VALUE ' '.
           05  FILLER                  PIC  X(036)         VALUE SPACES.
           05  FILLER                  PIC  X(059)         VALUE
               'RELATORIO DE CLIENTES PARTICIPANTES DE ADESOES POR  UF'.

       01  CABEC4-REL1.
           05  FILLER                  PIC  X(001)         VALUE ' '.
           05  FILLER                  PIC  X(131)         VALUE
               ALL '-'.

       01  CABEC5-REL1.
           05  FILLER                  PIC  X(001)         VALUE ' '.
           05  FILLER                  PIC  X(049)         VALUE SPACES.
           05  FILLER                  PIC  X(013)         VALUE
               'MOVIMENTO DE '.
           05  CB5-REL1-DIA-MOV        PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '/'.
           05  CB5-REL1-MES-MOV        PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '/'.
           05  CB5-REL1-ANO-MOV        PIC  9(004)         VALUE ZEROS.

       01  CABEC6-REL1.
           05  FILLER                  PIC  X(001)         VALUE '0'.
           05  FILLER                  PIC  X(005)         VALUE SPACES.
           05  FILLER                  PIC  X(015)         VALUE
               'AGENCIA'.
           05  FILLER                  PIC  X(012)         VALUE
               'CONTA'.
           05  FILLER                  PIC  X(011)         VALUE
               'PACOTE'.
           05  FILLER                  PIC  X(017)         VALUE
               'AGRUPAMENTO'.
           05  FILLER                  PIC  X(015)         VALUE
               'SEQUENCIA'.
           05  FILLER                  PIC  X(014)         VALUE
               'DATA DE INICIO'.

       01  CABEC7-REL1.
           05  FILLER                  PIC  X(001)         VALUE '-'.
           05  FILLER                  PIC  X(031)         VALUE SPACES.
           05  FILLER                  PIC  X(054)         VALUE
               '----------------------- TOTALIZADOR FINAL ------------'.
           05  FILLER                  PIC  X(010)         VALUE
               '----------'.

       01  CABEC8-REL1-BRANCO.
           05  FILLER                  PIC  X(001)         VALUE ' '.
           05  FILLER                  PIC  X(132)         VALUE SPACES.

       01  LINDET1-REL1.
           05  FILLER                  PIC  X(001)         VALUE ' '.
           05  FILLER                  PIC  X(005)         VALUE SPACES.
           05  LD1-REL1-AGENCIA        PIC  9(005)         VALUE ZEROS.
           05  FILLER                  PIC  X(007)         VALUE SPACES.
           05  LD1-REL1-CONTA          PIC  9(007)         VALUE ZEROS.
           05  FILLER                  PIC  X(008)         VALUE SPACES.
           05  LD1-REL1-PACOTE         PIC  99999          VALUE ZEROS.
           05  FILLER                  PIC  X(008)         VALUE SPACES.
           05  LD1-REL1-AGRUP          PIC  9(003)         VALUE ZEROS.
           05  FILLER                  PIC  X(011)         VALUE SPACES.
           05  LD1-REL1-SEQUENCIA      PIC  9(009)         VALUE ZEROS.
           05  FILLER                  PIC  X(008)         VALUE SPACES.
           05  LD1-REL1-DIA-INIC       PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '/'.
           05  LD1-REL1-MES-INIC       PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '/'.
           05  LD1-REL1-ANO-INIC       PIC  9(004)         VALUE ZEROS.

       01  LINTOT1-REL1.
           05  FILLER                  PIC  X(001)         VALUE '0'.
           05  FILLER                  PIC  X(036)         VALUE
               'QUANTIDADE TOTAL DE CLIENTES DO UF'.
           05  LT1-REL1-UF             PIC  X(002)         VALUE SPACES.
           05  FILLER                  PIC  X(003)         VALUE
               ' : '.
           05  LT1-REL1-TOTAL          PIC  Z.ZZZ.ZZZ.ZZ9  VALUE ZEROS.

       01  LINTOT2-REL1.
           05  FILLER                  PIC  X(001)         VALUE '-'.
           05  FILLER                  PIC  X(031)         VALUE SPACES.
           05  FILLER                  PIC  X(052)         VALUE
               '1- QUANTIDADE DE CLIENTES LIDOS EM CLIEUF.........: '.
           05  LT2-REL1-TT-CLIEUF      PIC  ZZZ.ZZZ.ZZZ.ZZ9
                                                           VALUE ZEROS.

       01  LINTOT3-REL1.
           05  FILLER                  PIC  X(001)         VALUE '0'.
           05  FILLER                  PIC  X(031)         VALUE SPACES.
           05  FILLER                  PIC  X(052)         VALUE
               '2- QUANTIDADE DE SOLICITACOES DE ADESAO...........: '.
           05  LT3-REL1-TT-ADESAO      PIC  ZZZ.ZZZ.ZZZ.ZZ9
                                                           VALUE ZEROS.

       01  LINTOT4-REL1.
           05  FILLER                  PIC  X(001)         VALUE '0'.
           05  FILLER                  PIC  X(031)         VALUE SPACES.
           05  FILLER                  PIC  X(052)         VALUE
               '3- QUANTIDADE DE CLIENTES GRAVADOS EM ADESUF......: '.
           05  LT4-REL1-TT-ADESUF      PIC  ZZZ.ZZZ.ZZZ.ZZ9
                                                           VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA BRAD7100 ***'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

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
               INCLUDE GFCTB001
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB008
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA CURSOR ***'.
      *----------------------------------------------------------------*

           EXEC SQL
               DECLARE CSR01-GFCTB008-JN CURSOR FOR
               SELECT
                   A.CSERVC_TARIF,
                   A.CAGPTO_CTA,
                   A.CSEQ_AGPTO_CTA,
                   A.DINIC_ADSAO_COMP,
                   A.CSGL_UF,
                   B.DFIM_ADSAO_COMP
               FROM DB2PRD.ADSAO_GRP_UF     A,
                    DB2PRD.ADSAO_COMP_PCOTE B
               WHERE
                   A.CSERVC_TARIF      = B.CSERVC_TARIF             AND
                   A.CAGPTO_CTA        = B.CAGPTO_CTA               AND
                   A.CSEQ_AGPTO_CTA    = B.CSEQ_AGPTO_CTA           AND
                   A.DINIC_ADSAO_COMP  = B.DINIC_ADSAO_COMP         AND
                   B.DINIC_ADSAO_COMP <= :GFCTB001.DINIC-ADSAO-COMP AND
                   B.DFIM_ADSAO_COMP  >= :GFCTB001.DFIM-ADSAO-COMP
               ORDER BY
                   A.CSGL_UF,
                   A.CSERVC_TARIF
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** FIM DA WORKING STORAGE SECTION ***'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      ******************************************************************
      * AREA PARA ROTINA PRINCIPAL DO PROGRAMA.                        *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-VER-VAZIO.

           PERFORM 3000-PROCESSAR.

           PERFORM 4000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA DE INICIALIZACAO DO PROGRAMA.                           *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           CALL 'BRAD1050'.

           INITIALIZE  ERRO-AREA.

           OPEN INPUT  CLIEUF
                OUTPUT ADESUF
                       RELADUF.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

           PERFORM 1200-OBTER-PARAMETROS.

           PERFORM 1300-OBTER-DATA-HORA.

           INITIALIZE WRK-TABELA-INTERNA-1.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ROTINA CHAMADORA DOS TESTES DE FILE STATUS DOS ARQUIVOS.      *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-CLIEUF.

           PERFORM 1120-TESTAR-FS-ADESUF.

           PERFORM 1130-TESTAR-FS-RELADUF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * FAZ O TESTE DE FILE STATUS DO ARQUIVO CLIEUF.                  *
      ******************************************************************
      *----------------------------------------------------------------*
       1110-TESTAR-FS-CLIEUF             SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-CLIEUF           NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'CLIEUF  '         TO WRK-NOME-ARQUIVO
               MOVE WRK-FS-CLIEUF      TO WRK-FILE-STATUS
               MOVE WRK-ERRO-ARQUIVO   TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * FAZ O TESTE DE FILE STATUS DO ARQUIVO ADESUF.                  *
      ******************************************************************
      *----------------------------------------------------------------*
       1120-TESTAR-FS-ADESUF           SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ADESUF           NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'ADESUF  '         TO WRK-NOME-ARQUIVO
               MOVE WRK-FS-ADESUF      TO WRK-FILE-STATUS
               MOVE WRK-ERRO-ARQUIVO   TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * FAZ O TESTE DE FILE STATUS DO ARQUIVO RELADUF.                 *
      ******************************************************************
      *----------------------------------------------------------------*
       1130-TESTAR-FS-RELADUF            SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-RELADUF          NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'RELADUF '         TO WRK-NOME-ARQUIVO
               MOVE WRK-FS-RELADUF     TO WRK-FILE-STATUS
               MOVE WRK-ERRO-ARQUIVO   TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA OBTER PARAMETROS.                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       1200-OBTER-PARAMETROS           SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC      OF GFCTB0A1.

           EXEC SQL
               SELECT
                     DPROCM_ANTER,
                     DPROCM_ATUAL,
                     DPROCM_PROX
               INTO
                     :GFCTB0A1.DPROCM-ANTER,
                     :GFCTB0A1.DPROCM-ATUAL,
                     :GFCTB0A1.DPROCM-PROX
               FROM   DB2PRD.PARM_DATA_PROCM
               WHERE  CSIST_PRINC      = :GFCTB0A1.CSIST-PRINC
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

           PERFORM 1210-OBTER-NOVA-DATA-PROC.

           PERFORM 1220-CALCULAR-DATA.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1210-OBTER-NOVA-DATA-PROC       SECTION.
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
       1210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * AREA PARA CALCULAR DATA.                                       *
      ******************************************************************
      *----------------------------------------------------------------*
       1220-CALCULAR-DATA              SECTION.
      *----------------------------------------------------------------*

           MOVE  DPROCM-ATUAL          OF GFCTB0A1
                                       TO WRK-DATA-ALT
                                          WRK-DDISPL.
           MOVE  WRK-DIA-ALT           TO CB5-REL1-DIA-MOV.
           MOVE  WRK-MES-ALT           TO CB5-REL1-MES-MOV.
           MOVE  WRK-ANO-ALT           TO CB5-REL1-ANO-MOV.

PWI        MOVE DPROCM-ANTER OF GFCTB0A1 TO DFIM-ADSAO-COMP
                                            OF GFCTB001 WRK-DFIM.
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
PWI                                      OF GFCTB001 WRK-DINIC.

      *----------------------------------------------------------------*
       1220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ROTINA PARA OBTER DATA E HORA DO SISTEMA.                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1300-OBTER-DATA-HORA            SECTION.
      *----------------------------------------------------------------*

           CALL 'BRAD7600'             USING WRK-DATA-HORA.

           MOVE WRK-7600-DIA           TO CB1-REL1-DIA.
           MOVE WRK-7600-MES           TO CB1-REL1-MES.
           MOVE WRK-7600-ANO           TO CB1-REL1-ANO.
           MOVE WRK-7600-HOR           TO CB1-REL1-HOR.
           MOVE WRK-7600-MIN           TO CB1-REL1-MIN.
           MOVE WRK-7600-SEG           TO CB1-REL1-SEG.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA QUE VERIFICA SE O ARQUIVO DE ENTRADA ESTA VAZIO.        *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-VER-VAZIO                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-LER-CLIEUF.

           IF  WRK-FS-CLIEUF           EQUAL '10'
               DISPLAY '************** GFCT9308 **************'
               DISPLAY '*                                    *'
               DISPLAY '*       ARQUIVO CLIEUF VAZIO         *'
               DISPLAY '*                                    *'
               DISPLAY '*      PROCESSAMENTO CANCELADO       *'
               DISPLAY '*                                    *'
               DISPLAY '************** GFCT9308 **************'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE '** ARQUIVO CLIEUF VAZIO, PROCESSAMENTO CANCELADO
      -        '**'                    TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE GFCTRY-SIGLA-UF        TO WRK-SIGLA-ANTERIOR.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * FAZ A LEITURA DO ARQUIVO CLIEUF.                               *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-LER-CLIEUF                 SECTION.
      *----------------------------------------------------------------*

           READ CLIEUF.

           IF  WRK-FS-CLIEUF           EQUAL '10'
               GO TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1110-TESTAR-FS-CLIEUF.

           ADD 1                       TO ACU-LIDOS-CLIEUF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA DE PROCESSAMENTRO PRINCIPAL DO PROGRAMA.                *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 3100-ABRIR-CURSOR1.

           PERFORM 3200-LER-CURSOR1.

           IF  WRK-FIM-CURSOR1         EQUAL 'S'
               DISPLAY '************** GFCT9308 **************'
               DISPLAY '*                                    *'
               DISPLAY '*  NAO EXISTE SOLICITACAO DE ADESAO  *'
               DISPLAY '*  UF PARA O PERIODO DE:             *'
               DISPLAY '*  DATA INICIO  : ' WRK-DINIC '         *'
               DISPLAY '*  DATA FIM  :    ' WRK-DFIM '         *'
               DISPLAY '*                                    *'
               DISPLAY '*           TERMINO NORMAL           *'
               DISPLAY '*                                    *'
               DISPLAY '************** GFCT9308 **************'
           END-IF.

           PERFORM 3300-PROCESSAR-CURSOR1
                                       UNTIL
                       WRK-FIM-CURSOR1 EQUAL 'S' OR
                       WRK-FS-CLIEUF   EQUAL '10'.

           PERFORM 3400-FECHAR-CURSOR1.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * EFETUA A ABERTURA DO CURSOR1.                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       3100-ABRIR-CURSOR1              SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FIM-CURSOR1.

           EXEC SQL
               OPEN CSR01-GFCTB008-JN
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'ADSAO_GRP_UF  '   TO ERR-DBD-TAB
               MOVE 'OPEN'             TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0020'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * EFETUA A LEITURA DO CURSOR1.                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       3200-LER-CURSOR1                SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH CSR01-GFCTB008-JN INTO
                   :GFCTB008.CSERVC-TARIF,
                   :GFCTB008.CAGPTO-CTA,
                   :GFCTB008.CSEQ-AGPTO-CTA,
                   :GFCTB008.DINIC-ADSAO-COMP,
                   :GFCTB008.CSGL-UF,
                   :GFCTB001.DFIM-ADSAO-COMP
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'ADSAO_GRP_UF  '   TO ERR-DBD-TAB
               MOVE 'FETCH'            TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0030'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-CURSOR1
               MOVE 'ZZ'               TO CSGL-UF          OF GFCTB008
           ELSE
               ADD 1                   TO ACU-LIDOS-CURSOR1
           END-IF.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA DE PROCESSAMENTO DO CURSOR1.                            *
      ******************************************************************
      *----------------------------------------------------------------*
       3300-PROCESSAR-CURSOR1          SECTION.
      *----------------------------------------------------------------*

           IF  CSGL-UF  OF GFCTB008        LESS GFCTRY-SIGLA-UF
               PERFORM 3200-LER-CURSOR1    UNTIL
                   CSGL-UF  OF GFCTB008    NOT LESS GFCTRY-SIGLA-UF
                   OR WRK-FIM-CURSOR1      EQUAL 'S'
           ELSE
               IF  CSGL-UF  OF GFCTB008    GREATER GFCTRY-SIGLA-UF
                   PERFORM 2100-LER-CLIEUF UNTIL
                       GFCTRY-SIGLA-UF     NOT LESS CSGL-UF
                                                           OF GFCTB008
                       OR WRK-FS-CLIEUF    EQUAL '10'
               ELSE
                   INITIALIZE WRK-TABELA-INTERNA-1
                   MOVE CSGL-UF            OF GFCTB008
                                           TO WRK-TAB-UF
                   PERFORM 3310-MONTAR-TABELA1
                                           UNTIL
                           CSGL-UF     OF GFCTB008
                                           GREATER WRK-TAB-UF
                       OR  WRK-FIM-CURSOR1 EQUAL 'S'
                   PERFORM 3320-DESCARREGA-UF
                                           UNTIL
                           GFCTRY-SIGLA-UF GREATER WRK-TAB-UF
                       OR  WRK-FS-CLIEUF   EQUAL '10'
                   PERFORM 3330-IMPRIMIR-LINTOT1
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA DE MONTAGEM DA TABELA INTERNA.                          *
      ******************************************************************
      *----------------------------------------------------------------*
       3310-MONTAR-TABELA1             SECTION.
      *----------------------------------------------------------------*

           ADD 1                       TO IND-1.

           IF  IND-1                   GREATER 300
               DISPLAY '************** GFCT9308 **************'
               DISPLAY '*                                    *'
               DISPLAY '*     ESTOURO DA TABELA INTERNA1     *'
               DISPLAY '*                                    *'
               DISPLAY '*      PROCESSAMENTO CANCELADO       *'
               DISPLAY '*                                    *'
               DISPLAY '************** GFCT9308 **************'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE '** ESTOURO DA TABELA INTERNA1 **'
                                       TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE  CSERVC-TARIF          OF GFCTB008
                                       TO WRK-TAB-PACOTE(IND-1).
           MOVE  CAGPTO-CTA            OF GFCTB008
                                       TO WRK-TAB-AGPTO(IND-1).
           MOVE  CSEQ-AGPTO-CTA        OF GFCTB008
                                       TO WRK-TAB-SEQ(IND-1).
           MOVE  DINIC-ADSAO-COMP      OF GFCTB008
                                       TO WRK-TAB-DINI(IND-1).

           PERFORM 3200-LER-CURSOR1.

      *----------------------------------------------------------------*
       3310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * DESCARREGA UF EM ADESUF.                                       *
      ******************************************************************
      *----------------------------------------------------------------*
       3320-DESCARREGA-UF              SECTION.
      *----------------------------------------------------------------*

           PERFORM VARYING IND-1       FROM 1 BY 1
                   UNTIL IND-1         GREATER 300 OR
                   WRK-TAB-PACOTE(IND-1)
                                       EQUAL ZEROS
                   MOVE WRK-GRAVACAO   TO WRK-OPERACAO
                   PERFORM 3321-PROCESSAR-PACOTES
                   IF  ACU-LINHAS      GREATER 59
                       PERFORM 3322-IMPRIME-CABECS-PAG
                       PERFORM 3323-IMPRIME-CABECS-UF
                   END-IF
                   PERFORM 3324-IMPRIMIR-LINDET1
           END-PERFORM.

           PERFORM 2100-LER-CLIEUF.

      *----------------------------------------------------------------*
       3320-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA PROCESSAR PACOTES.                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       3321-PROCESSAR-PACOTES          SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTRY-AGENCIA         TO GFCTRZ-AGENCIA.
           MOVE GFCTRY-RAZAO           TO GFCTRZ-RAZAO.
           MOVE GFCTRY-CONTA           TO GFCTRZ-CONTA.
           MOVE GFCTRY-POSTO-SERV      TO GFCTRZ-PAB.

           MOVE  WRK-TAB-PACOTE(IND-1) TO GFCTRZ-PACOTE.
           MOVE  WRK-TAB-AGPTO(IND-1)  TO GFCTRZ-AGPTO.
           MOVE  WRK-TAB-SEQ(IND-1)    TO GFCTRZ-SEQ.
           MOVE  WRK-TAB-DINI(IND-1)   TO GFCTRZ-DATA.

           PERFORM 3321-1-OBTER-PERIODO.

           WRITE GFCTRZ-ADESOES.

           PERFORM 1120-TESTAR-FS-ADESUF.

           ADD 1                       TO ACU-GRAVA-ADESUF.

      *----------------------------------------------------------------*
       3321-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

PWI   *----------------------------------------------------------------*
PWI    3321-1-OBTER-PERIODO              SECTION.
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
PWI        MOVE DINIC-ADSAO-COMP       OF GFCTB008
PWI                                      TO WRK-DATA-AUX
PWI        MOVE WRK-DIA-AUX              TO WRK-DIA-AUX1
PWI        MOVE WRK-MES-AUX              TO WRK-MES-AUX1
PWI        MOVE WRK-ANO-AUX              TO WRK-ANO-AUX1.
PWI
PWI        IF WRK-ANOMES-AUX1 NOT GREATER WRK-ANOMES-AUX
PWI           PERFORM 3321-2-MONTA-NO-MES
PWI        ELSE
PWI           PERFORM 3321-3-MONTA-FUTURO
PWI        END-IF.
PWI
PWI   *----------------------------------------------------------------*
PWI    3321-1-99-FIM.                 EXIT.
PWI   *----------------------------------------------------------------*
PWI
PWI   *----------------------------------------------------------------*
PWI    3321-2-MONTA-NO-MES            SECTION.
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
PWI           PERFORM 3321-4-OBTER-DFIM
PWI        END-IF.
PWI
PWI   *----------------------------------------------------------------*
PWI    3321-2-99-FIM.                 EXIT.
PWI   *----------------------------------------------------------------*
PWI   *----------------------------------------------------------------*
PWI    3321-3-MONTA-FUTURO            SECTION.
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
PWI        PERFORM 3321-4-OBTER-DFIM.
PWI
PWI   *----------------------------------------------------------------*
PWI    3321-3-99-FIM.                 EXIT.
PWI   *----------------------------------------------------------------*
PWI
PWI   *----------------------------------------------------------------*
PWI    3321-4-OBTER-DFIM              SECTION.
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
PWI    3321-4-99-FIM.                 EXIT.
PWI   *----------------------------------------------------------------*


      ******************************************************************
      * ROTINA PARA EMITIR CABECALHOS DE TOPO DE PAGINA.               *
      ******************************************************************
      *----------------------------------------------------------------*
       3322-IMPRIME-CABECS-PAG         SECTION.
      *----------------------------------------------------------------*

           ADD 1                       TO ACU-PAGINAS.

           MOVE  ACU-PAGINAS           TO CB2-REL1-PAG.

           WRITE FD-RELADUF            FROM CABEC1-REL1.

           PERFORM 1130-TESTAR-FS-RELADUF.

           WRITE FD-RELADUF            FROM CABEC2-REL1.

           PERFORM 1130-TESTAR-FS-RELADUF.

           WRITE FD-RELADUF            FROM CABEC3-REL1.

           PERFORM 1130-TESTAR-FS-RELADUF.

           WRITE FD-RELADUF            FROM CABEC4-REL1.

           PERFORM 1130-TESTAR-FS-RELADUF.

           WRITE FD-RELADUF            FROM CABEC5-REL1.

           PERFORM 1130-TESTAR-FS-RELADUF.

           WRITE FD-RELADUF            FROM CABEC4-REL1.

           PERFORM 1130-TESTAR-FS-RELADUF.

           MOVE 6                      TO ACU-LINHAS.

      *----------------------------------------------------------------*
       3322-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * IMPRIME CABECALHOS DE QUEBRA DE UF EM RELADUF                  *
      ******************************************************************
      *----------------------------------------------------------------*
       3323-IMPRIME-CABECS-UF          SECTION.
      *----------------------------------------------------------------*

           WRITE FD-RELADUF            FROM CABEC6-REL1.

           PERFORM 1130-TESTAR-FS-RELADUF.

           WRITE FD-RELADUF            FROM CABEC4-REL1.

           PERFORM 1130-TESTAR-FS-RELADUF.

           ADD  3                      TO ACU-LINHAS.

      *----------------------------------------------------------------*
       3323-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * IMPRIME LINHA DE DETALHE EM RELADUF.                           *
      ******************************************************************
      *----------------------------------------------------------------*
       3324-IMPRIMIR-LINDET1           SECTION.
      *----------------------------------------------------------------*

           IF  WRK-TAB-UF              NOT EQUAL WRK-SIGLA-ANTERIOR
               IF  ACU-LINHAS          LESS 56
                   WRITE FD-RELADUF    FROM CABEC8-REL1-BRANCO
                   PERFORM 1130-TESTAR-FS-RELADUF
                   ADD 1               TO ACU-LINHAS
                   PERFORM 3323-IMPRIME-CABECS-UF
               ELSE
                   PERFORM 3322-IMPRIME-CABECS-PAG
                   PERFORM 3323-IMPRIME-CABECS-UF
               END-IF
               MOVE WRK-TAB-UF         TO WRK-SIGLA-ANTERIOR
           END-IF.

           MOVE  GFCTRY-AGENCIA        TO  LD1-REL1-AGENCIA.
           MOVE  GFCTRY-CONTA          TO  LD1-REL1-CONTA.
           MOVE  WRK-TAB-PACOTE(IND-1) TO  LD1-REL1-PACOTE.
           MOVE  WRK-TAB-AGPTO(IND-1)  TO  LD1-REL1-AGRUP.
           MOVE  WRK-TAB-SEQ(IND-1)    TO  LD1-REL1-SEQUENCIA.

           MOVE  WRK-TAB-DINI(IND-1)   TO WRK-DATA-ALT.

           MOVE  WRK-DIA-ALT           TO  LD1-REL1-DIA-INIC.
           MOVE  WRK-MES-ALT           TO  LD1-REL1-MES-INIC.
           MOVE  WRK-ANO-ALT           TO  LD1-REL1-ANO-INIC.

           WRITE FD-RELADUF            FROM LINDET1-REL1.

           PERFORM 1130-TESTAR-FS-RELADUF.

           ADD 1                       TO ACU-LINHAS
                                          ACU-UF.

      *----------------------------------------------------------------*
       3324-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * IMPRIME LINHA DE TOTAL1 EM RELADUF.                            *
      ******************************************************************
      *----------------------------------------------------------------*
       3330-IMPRIMIR-LINTOT1           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           MOVE WRK-TAB-UF             TO LT1-REL1-UF.
           MOVE ACU-UF                 TO LT1-REL1-TOTAL.

           WRITE FD-RELADUF            FROM LINTOT1-REL1.

           PERFORM 1130-TESTAR-FS-RELADUF.

           ADD 2                       TO ACU-LINHAS.

           MOVE ZEROS                  TO ACU-UF
                                          IND-1.

      *----------------------------------------------------------------*
       3330-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * EFETUA O FECHAMENTO DO CURSOR.                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       3400-FECHAR-CURSOR1             SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR01-GFCTB008-JN
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'ADSAO_GRP_UF  '   TO ERR-DBD-TAB
               MOVE 'CLOSE'            TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0040'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       3400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * FINALIZA O PROCESSAMENTO.                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 4100-EMITIR-RELATO-TOTAIS.

           PERFORM 4200-EMITIR-DISPLAY.

           CLOSE CLIEUF
                 ADESUF
                 RELADUF.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

           STOP RUN.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA EMITIR TOTAIS EM RELADUF.                          *
      ******************************************************************
      *----------------------------------------------------------------*
       4100-EMITIR-RELATO-TOTAIS       SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           ADD 1                       TO ACU-PAGINAS.

           MOVE  ACU-PAGINAS           TO CB2-REL1-PAG.

           WRITE FD-RELADUF            FROM CABEC1-REL1.

           PERFORM 1130-TESTAR-FS-RELADUF.

           WRITE FD-RELADUF            FROM CABEC2-REL1.

           PERFORM 1130-TESTAR-FS-RELADUF.

           WRITE FD-RELADUF            FROM CABEC3-REL1.

           PERFORM 1130-TESTAR-FS-RELADUF.

           WRITE FD-RELADUF            FROM CABEC4-REL1.

           PERFORM 1130-TESTAR-FS-RELADUF.

           WRITE FD-RELADUF            FROM CABEC5-REL1.

           PERFORM 1130-TESTAR-FS-RELADUF.

           WRITE FD-RELADUF            FROM CABEC4-REL1.

           PERFORM 1130-TESTAR-FS-RELADUF.

           WRITE FD-RELADUF            FROM CABEC7-REL1.

           PERFORM 1130-TESTAR-FS-RELADUF.

           MOVE  ACU-LIDOS-CLIEUF      TO LT2-REL1-TT-CLIEUF.

           WRITE FD-RELADUF            FROM LINTOT2-REL1.

           PERFORM 1130-TESTAR-FS-RELADUF.

           MOVE  ACU-LIDOS-CURSOR1     TO LT3-REL1-TT-ADESAO.

           WRITE FD-RELADUF            FROM LINTOT3-REL1.

           PERFORM 1130-TESTAR-FS-RELADUF.

           MOVE  ACU-GRAVA-ADESUF      TO LT4-REL1-TT-ADESUF.

           WRITE FD-RELADUF            FROM LINTOT4-REL1.

           PERFORM 1130-TESTAR-FS-RELADUF.

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * EMITE INFORMACOES DO PROCESSAMENTO.                            *
      ******************************************************************
      *----------------------------------------------------------------*
       4200-EMITIR-DISPLAY             SECTION.
      *----------------------------------------------------------------*

           DISPLAY '************* GFCT9308 *************'.
           DISPLAY '*                                  *'.
           DISPLAY '*  PROGRAMA ENCERRADO COM SUCESSO  *'.
           DISPLAY '*                                  *'.
           DISPLAY '*   PROCESSAMENTO DE : ' WRK-DDISPL '  *'.
           DISPLAY '*                                  *'.
           DISPLAY '*         TERMINO NORMAL           *'.
           DISPLAY '*                                  *'.
           DISPLAY '************* GFCT9308 *************'.

      *----------------------------------------------------------------*
       4200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA PROCESSAMENTO DE ERROS DO PROGRAMA.                *
      ******************************************************************
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT9308'             TO ERR-PGM.

           CALL 'BRAD7100'             USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
