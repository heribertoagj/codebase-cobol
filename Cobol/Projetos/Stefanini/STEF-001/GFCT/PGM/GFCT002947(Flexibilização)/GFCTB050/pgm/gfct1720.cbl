      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT1720.
       AUTHOR.     SIMONI FAVRETTO.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT1720                                    *
      *    PROGRAMADORA:   SIMONI FAVRETTO         - CPM PATO BRANCO   *
      *    ANALISTA CPM:   ELIAS AUGUSTO BOSCATO   - CPM PATO BRANCO   *
      *    ANALISTA....:   MARIA VALERIA TORQUATO  - PROCKWORK/ 50     *
      *    DATA........:   13/06/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   EMISSAO DO RELATORIO DA ATUALIZACAO DAS     *
      *      FLEXIBILIZACOES POR AGRUPAMENTO NA TABELA QUENTE E NAS    *
      *      TABELAS FILHAS.                                           *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                        INCLUDE/BOOK     *
      *                   DB2PRD.PARM_DATA_PROCM        GFCTB0A1       *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                      INCLUDE/BOOK        *
      *                    DEFERATU                  I#GFCTT6          *
      *                    RELATO                        -             *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    POL7100C - AREA PARA TRATAMENTO DE ERRO PELA POOL7100.      *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    POOL1050 - MODULO PARA CONEXAO COM DB2.                     *
      *    POOL7100 - MODULO PARA CANCELAMENTO DO PROGRAMA.            *
      *    POOL7600 - MODULO PARA OBTER DATA E HORA DO SISTEMA.        *
      *================================================================*
080826*----------------------------------------------------------------*
080826*    ULTIMA ALTERACAO - VINICIUS CRISTIAN MADUREIRA - SONDPROCW  *
080826*    INCLUSAO DE CAMPOS NA TABELA E GFCTB049                     *
080826*    E ALTERACAO DA INC GFCTT3                                   *
080826*================================================================*
      *----------------------------------------------------------------*
      *    PROJETO - PWT-09-2711 - ALTERACAO JUNHO/2010                *
      *    NOVOS RELATS POR AGRUPAMENTO BRADESCO EXPRESSO E RAZAO      *
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

           SELECT DEFERATU ASSIGN      TO UT-S-DEFERATU
           FILE STATUS                 IS WRK-FS-DEFERATU.

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
      *            ORG. SEQUENCIAL                 -     LRECL = 200   *
      *----------------------------------------------------------------*

       FD  DEFERATU
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTT6'.

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
       77  ACU-MIGRADOS                PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-NAO-ATUALIZADOS         PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-FINALIZADOS             PIC  9(009) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA TESTE DE FILE STATUS *'.
      *----------------------------------------------------------------*

       77  WRK-FS-DEFERATU             PIC  X(002)         VALUE SPACES.
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
       77  WRK-CAGPTO-CTA-ANT          PIC  9(003)         VALUE ZEROS.

       01  WRK-CGC-AUX.
ST2506*    05  WRK-CPF                 PIC  999.999.999    VALUE ZEROS.
ST2506     05  WRK-CPF                 PIC  X(12)          VALUE SPACES.
           05  FILLER                  PIC  X(001)         VALUE '/'.
ST2506*    05  WRK-FILIAL              PIC  9(004)         VALUE ZEROS.
ST2506     05  WRK-FILIAL              PIC  X(004)         VALUE SPACES.
           05  FILLER                  PIC  X(001)         VALUE '-'.
           05  WRK-CONTROLE            PIC  X(002)         VALUE SPACES.

       01  WRK-AGENCIA-AUX.
           05  FILLER                  PIC  X(003)         VALUE SPACES.
           05  WRK-AGENCIA             PIC  9(007)         VALUE ZEROS.

       01  WRK-SEGMENTO-AUX.
           05  FILLER                  PIC  X(005)         VALUE SPACES.
           05  WRK-SEGMENTO            PIC  9(003)         VALUE ZEROS.

       01  WRK-AG-POSTO-AUX.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  WRK-AGENCIA-2           PIC  9(005)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '/'.
           05  WRK-POSTO               PIC  9(005)         VALUE ZEROS.

       01  WRK-EMPRESA-AUX.
           05  WRK-AGENCIA-3           PIC  9(005)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '/'.
           05  WRK-CONTA               PIC  9(007)         VALUE ZEROS.

       01  WRK-BCO-POSTAL-AUX.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  WRK-AGENCIA-4           PIC  9(005)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '/'.
           05  WRK-POSTO-4             PIC  9(005)         VALUE ZEROS.

       01  WRK-MUNICIPIO-AUX.
           05  FILLER                  PIC  X(003)         VALUE SPACES.
           05  WRK-MUNICIPIO           PIC  9(007)         VALUE ZEROS.

       01  WRK-ESTADO-AUX.
           05  FILLER                  PIC  X(006)         VALUE SPACES.
           05  WRK-ESTADO              PIC  X(002)         VALUE SPACES.

LFM    01  WRK-RAZAO-AUX.
LFM        05  FILLER                  PIC  X(001)         VALUE SPACES.
LFM        05  WRK-EMPR                PIC  9(005)         VALUE ZEROS.
LFM        05  FILLER                  PIC  X(001)         VALUE '-'.
LFM        05  WRK-GRP-CTBIL           PIC  9(003)         VALUE ZEROS.
LFM        05  FILLER                  PIC  X(001)         VALUE '/'.
LFM        05  WRK-SGRP-CTBIL          PIC  9(003)         VALUE ZEROS.

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
           05  CB1-REL1-DATA.
             10  CB1-REL1-DIA          PIC  9(002)         VALUE ZEROS.
             10  FILLER                PIC  X(001)         VALUE '/'.
             10  CB1-REL1-MES          PIC  9(002)         VALUE ZEROS.
             10  FILLER                PIC  X(001)         VALUE '/'.
             10  CB1-REL1-ANO          PIC  9(004)         VALUE ZEROS.
           05  FILLER                  PIC  X(044)         VALUE SPACES.
           05  FILLER                  PIC  X(011)         VALUE
               'B A N C O'.
           05  FILLER                  PIC  X(017)         VALUE
               'B R A D E S C O'.
           05  FILLER                  PIC  X(041)         VALUE
               'S / A'.
           05  CB1-REL1-HORA.
             10  CB1-REL1-HOR          PIC  9(002)         VALUE ZEROS.
             10  FILLER                PIC  X(001)         VALUE ':'.
             10  CB1-REL1-MIN          PIC  9(002)         VALUE ZEROS.
             10  FILLER                PIC  X(001)         VALUE ':'.
             10  CB1-REL1-SEG          PIC  9(002)         VALUE ZEROS.

       01  CABEC2-REL1.
           05  FILLER                  PIC  X(001)         VALUE ' '.
           05  FILLER                  PIC  X(044)         VALUE
               'GFCT1720'.
           05  FILLER                  PIC  X(079)         VALUE
               'GFCT - GESTAO, FLEXIBILIZACAO E COBRANCA DE TARIFAS'.
           05  FILLER                  PIC  X(005)         VALUE
               'PAG. '.
           05  CB2-REL1-PAGINA         PIC  ZZ9            VALUE ZEROS.

       01  CABEC3-REL1.
           05  FILLER                  PIC  X(001)         VALUE ' '.
           05  FILLER                  PIC  X(034)         VALUE SPACES.
           05  FILLER                  PIC  X(054)         VALUE
               'PEDIDOS DE FLEXIBILIZACAO DE TARIFAS AGRUPAMENTO INCLU'.
           05  FILLER                  PIC  X(014)         VALUE
               'IDOS/RENOVADOS'.

       01  CABEC4-REL1.
           05  FILLER                  PIC  X(001)         VALUE ' '.
           05  FILLER                  PIC  X(131)         VALUE
               ALL '-'.

       01  CABEC5-REL1.
           05  FILLER                  PIC  X(001)         VALUE ' '.
           05  FILLER                  PIC  X(051)         VALUE SPACES.
           05  FILLER                  PIC  X(028)         VALUE
               'DATA BASE INICIO VIGENCIA : '.
           05  CB5-REL1-DATA-BASE      PIC  X(010)         VALUE SPACES.

       01  CABEC6-REL1.
           05  FILLER                  PIC  X(001)         VALUE '0'.
           05  FILLER                  PIC  X(004)         VALUE SPACES.
           05  FILLER                  PIC  X(030)         VALUE
               '---------- PEDIDO ---------'.
           05  FILLER                  PIC  X(026)         VALUE
               '--- SOLICITANTE ---'.
           05  FILLER                  PIC  X(026)         VALUE
               '----- PARECER -----'.
           05  FILLER                  PIC  X(025)         VALUE
               '------- VIGENCIA --------'.

       01  CABEC7-REL1.
           05  FILLER                  PIC  X(001)         VALUE ' '.
           05  FILLER                  PIC  X(007)         VALUE SPACES.
           05  CB7-REL1-CAGPTO         PIC  X(011)         VALUE SPACES.
           05  FILLER                  PIC  X(006)         VALUE SPACES.
           05  FILLER                  PIC  X(010)         VALUE
               'TARIFA'.
           05  FILLER                  PIC  X(012)         VALUE
               'DEPDC'.
           05  FILLER                  PIC  X(014)         VALUE 'DATA'.
           05  FILLER                  PIC  X(011)         VALUE
               'DEPDC'.
           05  FILLER                  PIC  X(004)         VALUE 'DATA'.

       01  CABEC8-REL1.
           05  FILLER                  PIC  X(001)         VALUE ' '.
           05  FILLER                  PIC  X(001)         VALUE SPACES.

       01  LINDET1-REL1.
           05  FILLER                  PIC  X(001)         VALUE ' '.
           05  FILLER                  PIC  X(005)         VALUE SPACES.
           05  LD1-REL1-DADOS          PIC  X(019)         VALUE SPACES.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  LD1-REL1-TARIFA         PIC  9(005)         VALUE ZEROS.
           05  FILLER                  PIC  X(004)         VALUE SPACES.
           05  LD1-REL1-DEPDC-SOLTC    PIC  9(005)         VALUE ZEROS.
           05  FILLER                  PIC  X(004)         VALUE SPACES.
           05  LD1-REL1-DATA-SOLTC     PIC  X(010)         VALUE SPACES.
           05  FILLER                  PIC  X(007)         VALUE SPACES.
           05  LD1-REL1-DEPDC-PAREC    PIC  9(005)         VALUE ZEROS.
           05  FILLER                  PIC  X(004)         VALUE SPACES.
           05  LD1-REL1-DATA-PAREC     PIC  X(010)         VALUE SPACES.
           05  FILLER                  PIC  X(007)         VALUE SPACES.
           05  LD1-REL1-DATA-INI       PIC  X(010)         VALUE SPACES.
           05  FILLER                  PIC  X(005)         VALUE '  A'.
           05  LD1-REL1-DATA-FIM       PIC  X(010)         VALUE SPACES.
           05  FILLER                  PIC  X(006)         VALUE '   -'.
           05  LD1-REL1-SITUACAO       PIC  X(014)         VALUE SPACES.

       01  LINTOT1-REL1.
           05  FILLER                  PIC  X(001)         VALUE ' '.
           05  FILLER                  PIC  X(039)         VALUE SPACES.
           05  FILLER                  PIC  X(051)         VALUE
               'QTD DE DEFERIDOS LIDOS ....................... ='.
           05  LT1-REL1-LIDOS          PIC  9(009)         VALUE ZEROS.

       01  LINTOT2-REL1.
           05  FILLER                  PIC  X(001)         VALUE '0'.
           05  FILLER                  PIC  X(039)         VALUE SPACES.
           05  FILLER                  PIC  X(051)         VALUE
               'QTD DE VIGENTES SOMENTE MIGRADOS ............. ='.
           05  LT2-REL1-MIGRADOS       PIC  9(009)         VALUE ZEROS.

       01  LINTOT3-REL1.
           05  FILLER                  PIC  X(001)         VALUE ' '.
           05  FILLER                  PIC  X(039)         VALUE SPACES.
           05  FILLER                  PIC  X(051)         VALUE
               'QTD DE VIGENTES NAO ATUALIZADOS E MIGRADOS ... ='.
           05  LT3-REL1-NAO-ATUALIZADOS PIC  9(009)         VALUE ZEROS.

       01  LINTOT4-REL1.
           05  FILLER                  PIC  X(001)         VALUE ' '.
           05  FILLER                  PIC  X(039)         VALUE SPACES.
           05  FILLER                  PIC  X(051)         VALUE
               'QTD DE VIGENTES FINALIZADOS E MIGRADOS ....... ='.
           05  LT4-REL1-FINALIZADOS    PIC  9(009)         VALUE ZEROS.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA POOL7100 *'.
      *----------------------------------------------------------------*

       COPY POL7100C.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA DB2 *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0A1
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
               WRK-FS-DEFERATU         EQUAL '10'.

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

           OPEN INPUT  DEFERATU
                OUTPUT RELATO.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

           PERFORM 1200-OBTER-DATA-SIST.

           PERFORM 1300-OBTER-DATA-PROCM.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * EXECUTA TESTE DE FILE STATUS DOS ARQUIVOS.                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-DEFERATU.

           PERFORM 1120-TESTAR-FS-RELATO.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TESTE DE FILE STATUS DO ARQUIVO DEFERATU.                      *
      ******************************************************************
      *----------------------------------------------------------------*
       1110-TESTAR-FS-DEFERATU         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-DEFERATU         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'DEFERATU'         TO WRK-NOME-ARQ
               MOVE WRK-FS-DEFERATU    TO WRK-FILE-STATUS
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
           MOVE WRK-HOR                TO CB1-REL1-HOR.
           MOVE WRK-MIN                TO CB1-REL1-MIN.
           MOVE WRK-SEG                TO CB1-REL1-SEG.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ACESSAR TABELA GFCTB0A1 PARA OBTER DATA DO PROCESSAMENTO.      *
      ******************************************************************
      *----------------------------------------------------------------*
       1300-OBTER-DATA-PROCM           SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC      OF GFCTB0A1.

           EXEC SQL
             SELECT
                   DPROCM_PROX
             INTO
                   :GFCTB0A1.DPROCM-PROX
             FROM   DB2PRD.PARM_DATA_PROCM
             WHERE
                   CSIST_PRINC        = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PARM_DATA_PROCM ' TO ERR-DBD-TAB
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0010'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR SE EXISTEM REGISTROS NO ARQUIVO DEFERATU.            *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-LER-DEFERATU.

           IF  WRK-FS-DEFERATU         EQUAL '10'
               DISPLAY '*------------------------------------------*'
               DISPLAY '*             PROGRAMA GFCT1720            *'
               DISPLAY '*             -----------------            *'
               DISPLAY '*                                          *'
               DISPLAY '*         ARQUIVO DE DEFERIDO VAZIO        *'
               DISPLAY '*------------------------------------------*'
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * LEITURA DO ARQUIVO DEFERATU.                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-LER-DEFERATU               SECTION.
      *----------------------------------------------------------------*

           READ DEFERATU.

           IF  WRK-FS-DEFERATU         EQUAL '10'
               GO TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1110-TESTAR-FS-DEFERATU.

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

           IF  GFCTT6-CAGPTO-CTA       NOT EQUAL WRK-CAGPTO-CTA-ANT
               PERFORM 3100-EMITIR-CABECALHOS2
               MOVE GFCTT6-CAGPTO-CTA  TO WRK-CAGPTO-CTA-ANT
           END-IF.

           IF  ACU-LINHAS              GREATER 59
               PERFORM 3200-EMITIR-CABECALHOS1
               PERFORM 3100-EMITIR-CABECALHOS2
           END-IF.

           PERFORM 3300-EMITIR-DETALHE.

           PERFORM 2100-LER-DEFERATU.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * IMPRIME CABECALHOS SECUNDARIOS DO RELATORIO RELATO.            *
      ******************************************************************
      *----------------------------------------------------------------*
       3100-EMITIR-CABECALHOS2         SECTION.
      *----------------------------------------------------------------*

           IF  ACU-LINHAS              GREATER 55
               PERFORM 3200-EMITIR-CABECALHOS1
           END-IF.

           WRITE FD-RELATO             FROM CABEC6-REL1.
           PERFORM 1120-TESTAR-FS-RELATO.

           EVALUATE GFCTT6-CAGPTO-CTA

             WHEN 004
               MOVE '   CLIENTE '      TO CB7-REL1-CAGPTO

080826       WHEN 007
080826         MOVE ' FUNC BRAD '      TO CB7-REL1-CAGPTO

             WHEN 014
               MOVE ' AGENCIA   '      TO CB7-REL1-CAGPTO

             WHEN 009
               MOVE ' SEGMENTO  '      TO CB7-REL1-CAGPTO

             WHEN 015
               MOVE ' AG/POSTO  '      TO CB7-REL1-CAGPTO

             WHEN 016
               MOVE ' EMPRESA   '      TO CB7-REL1-CAGPTO

             WHEN 017
               MOVE 'BCO POSTAL '      TO CB7-REL1-CAGPTO

             WHEN 018
               MOVE 'MUNICIPIO  '      TO CB7-REL1-CAGPTO

             WHEN 019
               MOVE '    UF     '      TO CB7-REL1-CAGPTO

LFM          WHEN 020
LFM            MOVE 'BRAD. EXPR '      TO CB7-REL1-CAGPTO
LFM
LFM          WHEN 026
LFM            MOVE 'RAZAO CTBIL'      TO CB7-REL1-CAGPTO

080826       WHEN OTHER
080826         MOVE 'PERSONALIZ '      TO CB7-REL1-CAGPTO

           END-EVALUATE.

           WRITE FD-RELATO             FROM CABEC7-REL1.
           PERFORM 1120-TESTAR-FS-RELATO.

           WRITE FD-RELATO             FROM CABEC8-REL1.
           PERFORM 1120-TESTAR-FS-RELATO.

           ADD 4                       TO ACU-LINHAS.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * IMPRIME CABECALHOS PRINCIPAIS DO RELATORIO RELATO.             *
      ******************************************************************
      *----------------------------------------------------------------*
       3200-EMITIR-CABECALHOS1         SECTION.
      *----------------------------------------------------------------*

           ADD 1                       TO ACU-PAG.
           MOVE ACU-PAG                TO CB2-REL1-PAGINA.

           WRITE FD-RELATO             FROM CABEC1-REL1.
           PERFORM 1120-TESTAR-FS-RELATO.

           WRITE FD-RELATO             FROM CABEC2-REL1.
           PERFORM 1120-TESTAR-FS-RELATO.

           WRITE FD-RELATO             FROM CABEC3-REL1.
           PERFORM 1120-TESTAR-FS-RELATO.

           WRITE FD-RELATO             FROM CABEC4-REL1.
           PERFORM 1120-TESTAR-FS-RELATO.

           MOVE DPROCM-PROX            OF GFCTB0A1
                                       TO CB5-REL1-DATA-BASE.

           INSPECT CB5-REL1-DATA-BASE  REPLACING ALL '.' BY '/'.

           WRITE FD-RELATO             FROM CABEC5-REL1.
           PERFORM 1120-TESTAR-FS-RELATO.

           WRITE FD-RELATO             FROM CABEC4-REL1.
           PERFORM 1120-TESTAR-FS-RELATO.

           MOVE 6                      TO ACU-LINHAS.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * IMPRIME LINHAS DE DETALHE DO RELATORIO RELATO.                 *
      ******************************************************************
      *----------------------------------------------------------------*
       3300-EMITIR-DETALHE             SECTION.
      *----------------------------------------------------------------*

           EVALUATE GFCTT6-CAGPTO-CTA

             WHEN 004
               MOVE GFCTT6-CCGC-CPF04       TO WRK-CPF
               MOVE GFCTT6-CFLIAL-CGC04     TO WRK-FILIAL
               MOVE GFCTT6-CCTRL-CPF-CGC04  TO WRK-CONTROLE
               MOVE WRK-CGC-AUX             TO LD1-REL1-DADOS

             WHEN 014
               MOVE GFCTT6-CDEPDC05         TO WRK-AGENCIA
               MOVE WRK-AGENCIA-AUX         TO LD1-REL1-DADOS

             WHEN 009
               MOVE GFCTT6-CSGMTO-GSTAO-TARIF09
                                            TO WRK-SEGMENTO
               MOVE WRK-SEGMENTO-AUX        TO LD1-REL1-DADOS

             WHEN 015
               MOVE GFCTT6-CDEPDC15         TO WRK-AGENCIA-2
               MOVE GFCTT6-CPOSTO-SERVC15   TO WRK-POSTO
               MOVE WRK-AG-POSTO-AUX        TO LD1-REL1-DADOS

             WHEN 016
               MOVE GFCTT6-CJUNC-DEPDC16    TO WRK-AGENCIA-3
               MOVE GFCTT6-CCTA-CLI16       TO WRK-CONTA
               MOVE WRK-EMPRESA-AUX         TO LD1-REL1-DADOS

             WHEN 017
               MOVE GFCTT6-CDEPDC17         TO WRK-AGENCIA-4
               MOVE GFCTT6-CPOSTO-SERVC17   TO WRK-POSTO-4
               MOVE WRK-BCO-POSTAL-AUX      TO LD1-REL1-DADOS

             WHEN 018
               MOVE GFCTT6-CMUN-IBGE18      TO WRK-MUNICIPIO
               MOVE WRK-MUNICIPIO-AUX       TO LD1-REL1-DADOS

             WHEN 19
               MOVE GFCTT6-CSGL-UF19        TO WRK-ESTADO
               MOVE WRK-ESTADO-AUX          TO LD1-REL1-DADOS

LFM          WHEN 20
LFM            MOVE GFCTT6-CDEPDC17         TO WRK-AGENCIA-4
LFM            MOVE GFCTT6-CPOSTO-SERVC17   TO WRK-POSTO-4
LFM            MOVE WRK-BCO-POSTAL-AUX      TO LD1-REL1-DADOS
LFM
LFM          WHEN 26
LFM            MOVE GFCTT6-EMPR-CTBIL26     TO WRK-EMPR
LFM            MOVE GFCTT6-GRP-CTBIL26      TO WRK-GRP-CTBIL
LFM            MOVE GFCTT6-SGRP-CTBIL26     TO WRK-SGRP-CTBIL
               MOVE WRK-RAZAO-AUX           TO LD1-REL1-DADOS

080826       WHEN OTHER
080826         MOVE SPACES                  TO LD1-REL1-DADOS

           END-EVALUATE.

           MOVE GFCTT6-CSERVC-TARIF         TO LD1-REL1-TARIFA.
           MOVE GFCTT6-CDEPDC-ORIGE-FLEXZ   TO LD1-REL1-DEPDC-SOLTC.
           MOVE GFCTT6-DATA-SOLTC-FLEXZ     TO LD1-REL1-DATA-SOLTC.
           MOVE GFCTT6-CDEPDC-PAREC-FLEXZ   TO LD1-REL1-DEPDC-PAREC.
           MOVE GFCTT6-DATA-PAREC-FLEXZ     TO LD1-REL1-DATA-PAREC.
           MOVE GFCTT6-DINIC-FLEXZ-AGPTO    TO LD1-REL1-DATA-INI.
           MOVE GFCTT6-DFIM-FLEXZ-AGPTO     TO LD1-REL1-DATA-FIM.
           MOVE GFCTT6-MENSAGEM             TO LD1-REL1-SITUACAO.

           INSPECT LD1-REL1-DATA-SOLTC      REPLACING ALL '.' BY '/'.
           INSPECT LD1-REL1-DATA-PAREC      REPLACING ALL '.' BY '/'.
           INSPECT LD1-REL1-DATA-INI        REPLACING ALL '.' BY '/'.
           INSPECT LD1-REL1-DATA-FIM        REPLACING ALL '.' BY '/'.

           WRITE FD-RELATO                  FROM LINDET1-REL1.
           PERFORM 1120-TESTAR-FS-RELATO.

           IF  GFCTT6-MENSAGEM              EQUAL 'MIGRADA        '
               ADD 1                        TO ACU-MIGRADOS
           ELSE
               IF GFCTT6-MENSAGEM           EQUAL 'NAO ATUALIZADA '
                   ADD 1                    TO ACU-NAO-ATUALIZADOS
               ELSE
                   ADD 1                    TO ACU-FINALIZADOS
               END-IF
           END-IF.

           ADD 1                            TO ACU-LINHAS.

      *----------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCEDIMENTOS FINAIS.                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           IF  ACU-PAG                 NOT EQUAL ZEROS
               PERFORM 4100-EMITIR-TOTAIS
               PERFORM 4200-EMITIR-DISPLAY
           END-IF.

           CLOSE DEFERATU
                 RELATO.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

           STOP RUN.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * IMPRIME TOTAIS GERAIS DO RELATORIO RELATO.                     *
      ******************************************************************
      *----------------------------------------------------------------*
       4100-EMITIR-TOTAIS              SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 3200-EMITIR-CABECALHOS1.

           MOVE ACU-LIDOS              TO LT1-REL1-LIDOS.
           WRITE FD-RELATO             FROM LINTOT1-REL1.
           PERFORM 1120-TESTAR-FS-RELATO.

           MOVE ACU-MIGRADOS           TO LT2-REL1-MIGRADOS.
           WRITE FD-RELATO             FROM LINTOT2-REL1.
           PERFORM 1120-TESTAR-FS-RELATO.

           MOVE ACU-NAO-ATUALIZADOS    TO LT3-REL1-NAO-ATUALIZADOS.
           WRITE FD-RELATO             FROM LINTOT3-REL1.
           PERFORM 1120-TESTAR-FS-RELATO.

           MOVE ACU-FINALIZADOS        TO LT4-REL1-FINALIZADOS.
           WRITE FD-RELATO             FROM LINTOT4-REL1.
           PERFORM 1120-TESTAR-FS-RELATO.

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * EMITE DISPLAY FINAL.                                           *
      ******************************************************************
      *----------------------------------------------------------------*
       4200-EMITIR-DISPLAY             SECTION.
      *----------------------------------------------------------------*

           DISPLAY '*---------------------------------------------*'.
           DISPLAY '*             PROGRAMA GFCT1720               *'.
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

           MOVE 'GFCT1720'             TO ERR-PGM.

           CALL 'POOL7100'             USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

