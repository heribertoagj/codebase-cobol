      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT1849.
       AUTHOR.     BELCLEI FASOLO.
      *================================================================*
      *                C P M   B R A X I S  -  S I S T E M A S         *
      *----------------------------------------------------------------*
      *    PROGRAMA....:  GFCT1849                                     *
      *    PROGRAMADOR.:  BELCLEI FASOLO          - CPM BRAXIS - P BCO *
      *    ANALISTA CPM:  MARCIO CUSTIN           - CPM BRAXIS - P BCO *
      *    ANALISTA....:  RICARDO PEREIRA DA SILVA- PROCWORK   - GP.50 *
      *    DATA........:  07/08/2008                                   *
      *----------------------------------------------------------------*
      *    OBJETIVO....:  IDENTIFICAR OS REGISTROS COMO AGRUPAMENTO DE *
      *      FUNCIONARIO.                                              *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                        INCLUDE/BOOK      *
      *                  CADAFUNC                          -           *
      *                  EVTOEMPR                      I#GFCTVA        *
      *                  FUNCPERS                      I#GFCTVA        *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#BRAD7C - AREA CHAMADA BRAD7100 - CANCELAMENTO.            *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    BRAD7100 - TRATAMENTO DE ERRO.                              *
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

           SELECT CADAFUNC ASSIGN      TO UT-S-CADAFUNC
           FILE STATUS                 IS WRK-FS-CADAFUNC.

           SELECT EVTOEMPR ASSIGN      TO UT-S-EVTOEMPR
           FILE STATUS                 IS WRK-FS-EVTOEMPR.

           SELECT FUNCPERS ASSIGN      TO UT-S-FUNCPERS
           FILE STATUS                 IS WRK-FS-FUNCPERS.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:  ARQUIVO CADASTRO DE FUNCIONARIOS.                   *
      *            ORG. SEQUENCIAL     - LRECL = 009                   *
      *----------------------------------------------------------------*

       FD  CADAFUNC
           RECORDING MODE IS F
           LABEL RECORD STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-CADAFUNC                 PIC  X(009).

      *----------------------------------------------------------------*
      *    INPUT:  REGISTRO TABELA DE EVENTOS - GFCTB092.              *
      *            ORG. SEQUENCIAL     - LRECL = 250                   *
      *----------------------------------------------------------------*

       FD  EVTOEMPR
           RECORDING MODE IS F
           LABEL RECORD STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-EVTOEMPR                 PIC  X(250).

      *----------------------------------------------------------------*
      *    OUTPUT: REGISTRO TABELA DE EVENTOS - GFCTB092.              *
      *            ORG. SEQUENCIAL     - LRECL = 250                   *
      *----------------------------------------------------------------*

       FD  FUNCPERS
           RECORDING MODE IS F
           LABEL RECORD STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-FUNCPERS                 PIC  X(250).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           'INICIO DA WORKING STORAGE SECTION'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           'AREA PARA ACUMULADORES'.
      *----------------------------------------------------------------*

       77  ACU-LIDOS-EVTOEMPR          PIC  9(012) COMP-3  VALUE ZEROS.
       77  ACU-LIDOS-CADAFUNC          PIC  9(012) COMP-3  VALUE ZEROS.
       77  ACU-GRAVADOS-FUNCPERS       PIC  9(012) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           'AREA DE FILE-STATUS'.
      *----------------------------------------------------------------*

       77  WRK-FS-CADAFUNC             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-EVTOEMPR             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-FUNCPERS             PIC  X(002)         VALUE SPACES.

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
           'AREA PARA VARIAVEIS AUXILIARES'.
      *----------------------------------------------------------------*

       77  WRK-MASCARA                 PIC ZZZ.ZZZ.ZZZ.ZZ9 VALUE ZEROS.
       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.

       01  WRK-RED-S13                 PIC +9(013)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-RED-S13.
           05  FILLER                  PIC  X(001).
           05  WRK-RED-13              PIC  9(013).

       01  WRK-RED-S5                  PIC +9(005)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-RED-S5.
           05  FILLER                  PIC  X(001).
           05  WRK-RED-5               PIC  9(005).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'MENSAGEM DE ERRO DE FILE-STATUS'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-BRAD7100.
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
           'AREA PARA CHAVES DE ARQUIVOS'.
      *----------------------------------------------------------------*

       01  WRK-CHV-EVTOEMPR.
           05  WRK-AGENCIA-EVTOEMPR    PIC  9(005) COMP-3  VALUE ZEROS.
           05  WRK-CONTA-EVTOEMPR      PIC  9(013) COMP-3  VALUE ZEROS.

       01  WRK-CHV-CADAFUNC.
           05  WRK-AGENCIA-CADAFUNC    PIC  9(005) COMP-3  VALUE ZEROS.
           05  WRK-CONTA-CADAFUNC      PIC  9(013) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA PARA O ARQUIVO CADAFUNC'.
      *----------------------------------------------------------------*

       01  WRK-CADAFUNC.
           05  WRK-ARQ-AGENCIA         PIC  9(005) COMP-3  VALUE ZEROS.
           05  WRK-ARQ-CONTA           PIC  9(007) COMP-3  VALUE ZEROS.
           05  FILLER                  PIC  X(002)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA PARA OS ARQUIVOS EVTOEMPR E FUNCPERS'.
      *----------------------------------------------------------------*

       COPY I#GFCTVA.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA DA BRAD7100'.
      *----------------------------------------------------------------*

       COPY I#BRAD7C.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'FIM DA WORKING STORAGE SECTION'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      ******************************************************************
      * INICIO DO PROCESSAMENTO DO PROGRAMA.                           *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-VERIFICAR-VAZIO.

           PERFORM 3000-PROCESSAR      UNTIL
               WRK-CHV-EVTOEMPR        EQUAL HIGH-VALUES.

           PERFORM 4000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCEDIMENTOS INICIAIS DO PROGRAMA.                            *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT  EVTOEMPR
                       CADAFUNC
                OUTPUT FUNCPERS.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TESTAR FILE-STATUS DOS ARQUIVOS.                               *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-EVTOEMPR.

           PERFORM 1120-TESTAR-FS-CADAFUNC.

           PERFORM 1130-TESTAR-FS-FUNCPERS.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TESTAR FILE-STATUS DO ARQUIVO EVTOEMPR.                        *
      ******************************************************************
      *----------------------------------------------------------------*
       1110-TESTAR-FS-EVTOEMPR         SECTION.
      *----------------------------------------------------------------*

           IF (WRK-FS-EVTOEMPR         NOT EQUAL '00')
               MOVE 'EVTOEMPR'         TO WRK-NOME-ARQ
               MOVE WRK-FS-EVTOEMPR    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-BRAD7100  TO ERR-TEXTO(1:65)
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TESTAR FILE-STATUS DO ARQUIVO CADAFUNC.                        *
      ******************************************************************
      *----------------------------------------------------------------*
       1120-TESTAR-FS-CADAFUNC         SECTION.
      *----------------------------------------------------------------*

           IF (WRK-FS-CADAFUNC         NOT EQUAL '00')
               MOVE 'CADAFUNC'         TO WRK-NOME-ARQ
               MOVE WRK-FS-CADAFUNC    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-BRAD7100  TO ERR-TEXTO(1:65)
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TESTAR FILE-STATUS DO ARQUIVO FUNCPERS.                        *
      ******************************************************************
      *----------------------------------------------------------------*
       1130-TESTAR-FS-FUNCPERS         SECTION.
      *----------------------------------------------------------------*

           IF (WRK-FS-FUNCPERS         NOT EQUAL '00')
               MOVE 'FUNCPERS'         TO WRK-NOME-ARQ
               MOVE WRK-FS-FUNCPERS    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-BRAD7100  TO ERR-TEXTO(1:65)
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR SE O ARQUIVO DE ENTRADA ESTA VAZIO.                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-LER-CADAFUNC.

           IF (WRK-FS-CADAFUNC         EQUAL '10')
               DISPLAY '********* GFCT1849 *********'
               DISPLAY '*                          *'
               DISPLAY '*  ARQUIVO CADAFUNC VAZIO  *'
               DISPLAY '*  PROCESSAMENTO ENCERRADO *'
               DISPLAY '*                          *'
               DISPLAY '********* GFCT1849 *********'
               PERFORM 4000-FINALIZAR
           END-IF.

           PERFORM 2200-LER-EVTOEMPR.

           IF (WRK-FS-EVTOEMPR         EQUAL '10')
               DISPLAY '********* GFCT1849 *********'
               DISPLAY '*                          *'
               DISPLAY '*  ARQUIVO EVTOEMPR VAZIO  *'
               DISPLAY '*  PROCESSAMENTO ENCERRADO *'
               DISPLAY '*                          *'
               DISPLAY '********* GFCT1849 *********'
               PERFORM 4000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA QUE EFETUA LEITURA DO ARQUIVO DE ENTRADA - CADAFUNC.    *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-LER-CADAFUNC               SECTION.
      *----------------------------------------------------------------*

           READ CADAFUNC               INTO WRK-CADAFUNC.

           IF (WRK-FS-CADAFUNC         EQUAL '10')
               MOVE HIGH-VALUES        TO WRK-CHV-CADAFUNC
               GO TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1120-TESTAR-FS-CADAFUNC.

           ADD 1                       TO ACU-LIDOS-CADAFUNC.

           MOVE WRK-ARQ-AGENCIA        TO WRK-AGENCIA-CADAFUNC.
           MOVE WRK-ARQ-CONTA          TO WRK-CONTA-CADAFUNC.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA QUE EFETUA LEITURA DO ARQUIVO DE ENTRADA - EVTOEMPR.    *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-LER-EVTOEMPR               SECTION.
      *----------------------------------------------------------------*

           READ EVTOEMPR                   INTO GFCTVA-GFCTB092.

           IF (WRK-FS-EVTOEMPR             EQUAL '10')
               MOVE HIGH-VALUES            TO WRK-CHV-EVTOEMPR
               GO TO 2200-99-FIM
           END-IF.

           MOVE WRK-LEITURA                TO WRK-OPERACAO.

           PERFORM 1110-TESTAR-FS-EVTOEMPR.

           ADD 1                           TO ACU-LIDOS-EVTOEMPR.

           MOVE GFCTVA-CAG-DSTNO-MOVTO     TO WRK-RED-S5.
           MOVE WRK-RED-5                  TO WRK-AGENCIA-EVTOEMPR.
           MOVE GFCTVA-CCTA-DSTNO-MOVTO    TO WRK-RED-S13.
           MOVE WRK-RED-13                 TO WRK-CONTA-EVTOEMPR.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAMENTO PRINCIPAL DO PROGRAMA.                           *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           IF  WRK-CHV-EVTOEMPR        EQUAL WRK-CHV-CADAFUNC
               MOVE 'F'                TO GFCTVA-AGRUP-FUNC-PERSO
               PERFORM 3100-GRAVAR-FUNCPERS
               PERFORM 2200-LER-EVTOEMPR
           ELSE
               IF  WRK-CHV-EVTOEMPR    LESS WRK-CHV-CADAFUNC
                   MOVE SPACES         TO GFCTVA-AGRUP-FUNC-PERSO
                   PERFORM 3100-GRAVAR-FUNCPERS
                   PERFORM 2200-LER-EVTOEMPR
               ELSE
                   PERFORM 2100-LER-CADAFUNC
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA QUE EFETUA GRAVACAO DO ARQUIVO DE SAIDA - FUNCPERS.     *
      ******************************************************************
      *----------------------------------------------------------------*
       3100-GRAVAR-FUNCPERS            SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           WRITE FD-FUNCPERS           FROM GFCTVA-GFCTB092.

           PERFORM 1130-TESTAR-FS-FUNCPERS.

           ADD 1                       TO ACU-GRAVADOS-FUNCPERS.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCEDIMENTOS FINAIS DO PROGRAMA.                              *
      ******************************************************************
      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 4100-EXIBIR-DISPLAY-FINAL.

           CLOSE EVTOEMPR
                 CADAFUNC
                 FUNCPERS.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

           STOP RUN.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * EXIBIR TOTAIS DO PROCESSAMENTO.                                *
      ******************************************************************
      *----------------------------------------------------------------*
       4100-EXIBIR-DISPLAY-FINAL       SECTION.
      *----------------------------------------------------------------*

           MOVE ACU-LIDOS-EVTOEMPR     TO WRK-MASCARA.

           DISPLAY '*--------------------------------------------*'.
           DISPLAY '*              PROGRAMA GFCT1849             *'.
           DISPLAY '*              -----------------             *'.
           DISPLAY '*                                            *'.
           DISPLAY '*     PROCESSAMENTO EFETUADO COM SUCESSO     *'.
           DISPLAY '*--------------------------------------------*'.
           DISPLAY '*--------------------------------------------*'.
           DISPLAY '*                                            *'.
           DISPLAY '* REGS LIDOS EVTOEMPR      = '
                                                   WRK-MASCARA ' *'.

           MOVE ACU-LIDOS-CADAFUNC     TO WRK-MASCARA.

           DISPLAY '* REGS LIDOS CADAFUNC      = '
                                                   WRK-MASCARA ' *'.

           MOVE ACU-GRAVADOS-FUNCPERS  TO WRK-MASCARA.

           DISPLAY '* REGS GRAVADOS FUNCPERS   = '
                                                   WRK-MASCARA ' *'.
           DISPLAY '*                                            *'.
           DISPLAY '*--------------------------------------------*'.

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TRATAMENTO PARA CANCELAMENTO DO PROGRAMA.                   *
      ******************************************************************
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT1849'             TO ERR-PGM.
           MOVE 'APL'                  TO ERR-TIPO-ACESSO.

           CALL 'BRAD7100'             USING WRK-BATCH
                                             ERRO-AREA.
           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
