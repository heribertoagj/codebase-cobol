      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. SLIG6225.
       AUTHOR.     EDIVAN RANSOLIN.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   SLIG6225                                    *
      *    PROGRAMADOR.:   EDIVAN RANSOLIN          - CPM PATO BRANCO  *
      *    ANALISTA CPM:   FABIO DALACORT           - CPM PATO BRANCO  *
      *    ANALISTA....:   SONIA                    - DTS / GRUPO 73   *
      *    DATA........:   08/09/2005                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO.....:  EFETUAR LIMPEZA DE OCORRENCIAS DOS CHEQUES  *
      *        DO SISTEMA DE INDICES DE LIQUIDEZ.                      *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                          INCLUDE/BOOK    *
      *                    EQTDIASL                          -         *
      *                    EHISTOCO                      I#SLIGC0      *
      *                    SHISTOCO                      I#SLIGC0      *
      *                    SRELCTRL                          -         *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    I#BRAD7C - AREA PARA TRATAMENTO DE ERRO PELA BRAD7100       *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    BRAD7100 -  MODULO PARA CANCELAMENTO DO PROGRAMA            *
      *    BRAD7600 -  MODULO PARA OBTER DATA E HORA DO SISTEMA        *
      *    BRAD0300 -  LEITURA DE ARQUIVOS COMPRIMIDOS                 *
      *    BRAD0315 -  GRAVACAO DE ARQUIVOS COMPRIMIDOS                *
      *    BRAD0025 -  SOMAR/SUBTRAIR NUMERO DE DIAS DE UMA DATA       *
      *================================================================*
           EJECT
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

           SELECT EQTDIASL  ASSIGN     TO UT-S-EQTDIASL
           FILE STATUS                 IS WRK-FS-EQTDIASL.

           SELECT SRELCTRL  ASSIGN     TO UT-S-SRELCTRL
           FILE STATUS                 IS WRK-FS-SRELCTRL.

           EJECT
      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:  ARQUIVO DE DATAS                                    *
      *            ORG. SEQUENCIAL     -  LRECL   =   005              *
      *----------------------------------------------------------------*

       FD  EQTDIASL
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  REG-EQTDIASL.
           03  EQTDIASL-QTDE-DIAS      PIC   9(003).

           EJECT
      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE SAIDA                                    *
      *            ORG. SEQUENCIAL     -  LRECL   =   080              *
      *----------------------------------------------------------------*

       FD  SRELCTRL
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  REG-SRELCTRL                PIC  X(080).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** SLIG6225 - INICIO DA AREA DE WORKING ***'.
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

       77  ACU-LIDOS                   PIC  9(012) COMP-3  VALUE ZEROS.
       77  ACU-GRAVADOS                PIC  9(012) COMP-3  VALUE ZEROS.
       77  ACU-DESPREZADOS             PIC  9(012) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE FILE STATUS ***'.
      *----------------------------------------------------------------*

       77  WRK-FS-EQTDIASL             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-SRELCTRL             PIC  X(002)         VALUE SPACES.

       77  WRK-ABERTURA                PIC  X(013)         VALUE
           ' NA ABERTURA '.
       77  WRK-LEITURA                 PIC  X(013)         VALUE
           ' NA  LEITURA '.
       77  WRK-GRAVACAO                PIC  X(013)         VALUE
           ' NA GRAVACAO '.
       77  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.

           EJECT
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREAS AUXILIARES ***'.
      *----------------------------------------------------------------*

       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.
       77  WRK-OCORR                   PIC  9(003)         VALUE ZEROS.
       77  WRK-FIM-ARQUIVO             PIC  X(001)         VALUE 'N'.
       77  WRK-EHISTOCO                PIC  X(008)         VALUE
           'EHISTOCO'.
       77  WRK-SHISTOCO                PIC  X(008)         VALUE
           'SHISTOCO'.
       77  WRK-REG-EHISTOCO            PIC  X(1050)        VALUE SPACES.
       77  WRK-REG-SHISTOCO            PIC  X(1050)        VALUE SPACES.
       77 WRK-QTDE-DIAS                PIC S9(005)         VALUE ZEROS.
       77 WRK-DATA-LIMPEZA             PIC  9(008)         VALUE ZEROS.

       77  WRK-MSG01                   PIC  X(075)         VALUE
           'ARQUIVO EQTDIASL ESTA VAZIO'.
       77  WRK-MSG02                   PIC  X(075)         VALUE
           'ARQUIVO EHISTOCO ESTA VAZIO'.
       77  WRK-MSG03                   PIC  X(075)         VALUE
           'ERRO NO ACESSO AO MODULO BRAD0025'.

       01  WRK-DATA-AUX                PIC  9(008)         VALUE ZEROS.
       01  WRK-DATA-AUX-R              REDEFINES WRK-DATA-AUX.
           03  WRK-ANO-AUX             PIC  9(004).
           03  WRK-MES-AUX             PIC  9(002).
           03  WRK-DIA-AUX             PIC  9(002).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA AUXILIAR DE HISTORICO CHEQUE ***'.
      *----------------------------------------------------------------*

       01  WRK-HISTORICO-CHEQUE-AUX.
           03  WRK-HISTORICO-CHEQUE OCCURS 20 TIMES.
               05  WRK-CTPO-CHEQ          PIC 9(003) COMP-3 VALUE ZEROS.
               05  WRK-CODI-DEVOL-CHQ     PIC 9(002)        VALUE ZEROS.
               05  WRK-DT-MOVIMENTO       PIC 9(008)        VALUE ZEROS.
               05  WRK-DT-MOVIMENTO-RED REDEFINES WRK-DT-MOVIMENTO.
                   07  WRK-ANO-MOVIMENTO  PIC 9(004).
                   07  WRK-MES-MOVIMENTO  PIC 9(002).
                   07  WRK-DIA-MOVIMENTO  PIC 9(002).
               05  WRK-DT-DEPOSITO        PIC 9(008)        VALUE ZEROS.
               05  WRK-DT-DEPOSITO-RED   REDEFINES WRK-DT-DEPOSITO.
                   07  WRK-ANO-DEPOSITO   PIC 9(004).
                   07  WRK-MES-DEPOSITO   PIC 9(002).
                   07  WRK-DIA-DEPOSITO   PIC 9(002).
               05  WRK-DT-CONTABIL        PIC 9(008)        VALUE ZEROS.
               05  WRK-DT-CONTABIL-RED   REDEFINES WRK-DT-CONTABIL.
                   07  WRK-ANO-CONTABIL   PIC 9(004).
                   07  WRK-MES-CONTABIL   PIC 9(002).
                   07  WRK-DIA-CONTABIL   PIC 9(002).
               05  WRK-DT-ENTRADA-ORIGEM  PIC 9(008)        VALUE ZEROS.
               05  WRK-DT-ENTRADA-ORIGEM-RED REDEFINES
                   WRK-DT-ENTRADA-ORIGEM.
                   07  WRK-ANO-ENTRADA-ORIGEM PIC 9(004).
                   07  WRK-MES-ENTRADA-ORIGEM PIC 9(002).
                   07  WRK-DIA-ENTRADA-ORIGEM PIC 9(002).
               05  WRK-DT-SITUACAO        PIC 9(008)        VALUE ZEROS.
               05  WRK-DT-SITUACAO-RED     REDEFINES WRK-DT-SITUACAO.
                   07  WRK-ANO-SITUACAO   PIC 9(004).
                   07  WRK-MES-SITUACAO   PIC 9(002).
                   07  WRK-DIA-SITUACAO   PIC 9(002).

      *----------------------------------------------------------------*
       01  FILLER                       PIC  X(050)         VALUE
           '*** AREA PARA MENSAGEM DE ERRO DE FILE STATUS ***'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-ARQUIVO.
           03  FILLER                  PIC  X(008)         VALUE
               '** ERRO '.
           03  WRK-OPERACAO            PIC  X(013)         VALUE SPACES.
           03  FILLER                  PIC  X(012)         VALUE
               ' DO ARQUIVO '.
           03  WRK-NOME-ARQ            PIC  X(008)         VALUE SPACES.
           03  FILLER                  PIC  X(017)         VALUE
               ' - FILE STATUS = '.
           03  WRK-FILE-STATUS         PIC  X(002)         VALUE SPACES.
           03  FILLER                  PIC  X(003)         VALUE ' **'.

           EJECT
      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(053)         VALUE
           '*** MENSAGENS DE ERRO PARA BRAD0300/BRAD0315 ***'.
      *---------------------------------------------------------------*

       01  WRK-ERRO-TEXTO.
           03  FILLER                  PIC  X(011)         VALUE
               '** ERRO NA '.
           03  WRK-NOME-BRAD           PIC  X(008)         VALUE SPACES.
           03  FILLER                  PIC  X(022)         VALUE
               ' NO ACESSO AO ARQUIVO '.
           03  WRK-NOME-ARQUIVO        PIC  X(008)         VALUE SPACES.
           03  FILLER                  PIC  X(017)         VALUE
               ' - RETURN CODE = '.
           03  WRK-RETURN-CODE         PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(003)         VALUE
               ' **'.

           EJECT
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA BRAD7600 ***'.
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
           '*** AREA DA BRAD0025 ***'.
      *----------------------------------------------------------------*

       01  WRK-DATA-ENTRADA          PIC  9(009) COMP-3    VALUE ZEROS.
       01  WRK-NUMERO-DIAS           PIC S9(005) COMP-3    VALUE ZEROS.
       01  WRK-DATA-SAIDA            PIC  9(009) COMP-3    VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA SRELCTRL ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA CABECALHOS ***'.
      *----------------------------------------------------------------*

       01  CABEC1.
           03  FILLER                  PIC  X(001)         VALUE ' '.
           03  FILLER                  PIC  X(008)         VALUE
               'SLIG6225'.
           03  FILLER                  PIC  X(003)         VALUE SPACES.
           03  FILLER                  PIC  X(058)         VALUE
               'LIMPEZA E REARRANJO DO ARQUIVO DE OCORRENCIAS HISTORICAS
      -        '  '.
           03  FILLER                  PIC  X(010)         VALUE
               'PAG. UNICA'.

       01  CABEC2.
           03  FILLER                  PIC  X(001)         VALUE ' '.
           03  FILLER                  PIC  X(008)         VALUE
               'BRADESCO'.
           03  FILLER                  PIC  X(061)         VALUE SPACES.
           03  CB2-DATA.
               05  CB2-DIA             PIC  9(002)         VALUE ZEROS.
               05  FILLER              PIC  X(001)         VALUE '/'.
               05  CB2-MES             PIC  9(002)         VALUE ZEROS.
               05  FILLER              PIC  X(001)         VALUE '/'.
               05  CB2-ANO             PIC  9(004)         VALUE ZEROS.

       01  CABEC3.
           03  FILLER                  PIC  X(001)         VALUE ' '.
           03  FILLER                  PIC  X(028)         VALUE SPACES.
           03  FILLER                  PIC  X(021)         VALUE
               'RELATORIO DE CONTROLE'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA LINHAS DE TOTAIS ***'.
      *----------------------------------------------------------------*

       01  LINTOT1.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(012)         VALUE SPACES.
           03  FILLER                  PIC  X(035)         VALUE
               'REGISTROS LIDOS...............  :  '.
           03  LT1-LIDOS               PIC  Z.ZZZ.ZZZ.ZZ9.

       01  LINTOT2.
           03  FILLER                  PIC  X(001)         VALUE ' '.
           03  FILLER                  PIC  X(012)         VALUE SPACES.
           03  FILLER                  PIC  X(035)         VALUE
               'REGISTROS GRAVADOS............  :  '.
           03  LT2-GRAVADOS            PIC  Z.ZZZ.ZZZ.ZZ9.

       01  LINTOT3.
           03  FILLER                  PIC  X(001)         VALUE ' '.
           03  FILLER                  PIC  X(012)         VALUE SPACES.
           03  FILLER                  PIC  X(035)         VALUE
               'REGISTROS DESPREZADOS.........  :  '.
           03  LT3-DESPREZADOS         PIC  Z.ZZZ.ZZZ.ZZ9.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA OS ARQUIVOS EHISTOCO E SHISTOCO ***'.
      *---------------------------------------------------------------*

-INC I#SLIGC0

           EJECT
      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA BRAD7100 ***'.
      *---------------------------------------------------------------*

-INC I#BRAD7C

           EJECT
      *----------------------------------------------------------------*
       01  FILLER                   PIC  X(050)         VALUE
           '*** SLIG6225 - FIM DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*
           EJECT
      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       000000-INICIAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 100000-INICIALIZAR.

           PERFORM 200000-VERIFICAR-VAZIO.

           PERFORM 300000-PROCESSAR    UNTIL
                   WRK-FIM-ARQUIVO      EQUAL 'S'.

           PERFORM 400000-FINALIZAR.

      *----------------------------------------------------------------*
       000000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       100000-INICIALIZAR              SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT  EQTDIASL
                OUTPUT SRELCTRL.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 110000-TESTAR-FILE-STATUS.

           PERFORM 120000-OBTER-DATA-ATUAL.

      *----------------------------------------------------------------*
       100000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       110000-TESTAR-FILE-STATUS       SECTION.
      *----------------------------------------------------------------*

           PERFORM 111000-TESTAR-FS-EQTDIASL.

           PERFORM 112000-TESTAR-FS-SRELCTRL.

      *----------------------------------------------------------------*
       110000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       111000-TESTAR-FS-EQTDIASL        SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-EQTDIASL         NOT EQUAL ZEROS
               MOVE 'EQTDIASL'         TO WRK-NOME-ARQ
               MOVE WRK-FS-EQTDIASL    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-ARQUIVO   TO ERR-TEXTO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       111000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       112000-TESTAR-FS-SRELCTRL        SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SRELCTRL         NOT EQUAL ZEROS
               MOVE 'SRELCTRL'         TO WRK-NOME-ARQ
               MOVE WRK-FS-SRELCTRL    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-ARQUIVO   TO ERR-TEXTO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       112000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       120000-OBTER-DATA-ATUAL         SECTION.
      *----------------------------------------------------------------*

           CALL 'BRAD7600'             USING WRK-DATA-HORA.

           MOVE WRK-DIA                TO CB2-DIA
                                          WRK-DIA-AUX.
           MOVE WRK-MES                TO CB2-MES
                                          WRK-MES-AUX.
           MOVE WRK-ANO                TO CB2-ANO
                                          WRK-ANO-AUX.

      *----------------------------------------------------------------*
       120000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       200000-VERIFICAR-VAZIO          SECTION.
      *----------------------------------------------------------------*

           PERFORM 210000-LER-EQTDIASL.

           IF  WRK-FS-EQTDIASL         EQUAL '10'
               DISPLAY '**************** SLIG6225 ****************'
               DISPLAY '*                                        *'
               DISPLAY '*        ARQUIVO  EQTDIASL  VAZIO        *'
               DISPLAY '*                                        *'
               DISPLAY '*        PROCESSAMENTO  CANCELADO        *'
               DISPLAY '*                                        *'
               DISPLAY '**************** SLIG6225 ****************'
               MOVE WRK-MSG01          TO ERR-TEXTO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 220000-LER-EHISTOCO.

           IF  WRK-FIM-ARQUIVO         EQUAL 'S'
               DISPLAY '**************** SLIG6225 ****************'
               DISPLAY '*                                        *'
               DISPLAY '*        ARQUIVO  EHISTOCO  VAZIO        *'
               DISPLAY '*                                        *'
               DISPLAY '*        PROCESSAMENTO  CANCELADO        *'
               DISPLAY '*                                        *'
               DISPLAY '**************** SLIG6225 ****************'
               MOVE WRK-MSG02          TO ERR-TEXTO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           ELSE
               PERFORM 230000-CALCULAR-DATA
           END-IF.

      *----------------------------------------------------------------*
       200000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       210000-LER-EQTDIASL             SECTION.
      *----------------------------------------------------------------*

           READ EQTDIASL.

           IF  WRK-FS-EQTDIASL         EQUAL '10'
               GO TO 210000-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 111000-TESTAR-FS-EQTDIASL.

      *----------------------------------------------------------------*
       210000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       220000-LER-EHISTOCO             SECTION.
      *----------------------------------------------------------------*

           CALL 'BRAD0300'             USING WRK-EHISTOCO
                                             WRK-REG-EHISTOCO.

           IF  RETURN-CODE             EQUAL 4
               MOVE 'S'                TO WRK-FIM-ARQUIVO
               GO TO 220000-99-FIM
           ELSE
               IF  RETURN-CODE         NOT EQUAL ZEROS
                   MOVE 'BRAD0300'     TO WRK-NOME-BRAD
                   MOVE 'EHISTOCO'     TO WRK-NOME-ARQUIVO
                   MOVE RETURN-CODE    TO WRK-RETURN-CODE
                   MOVE WRK-ERRO-TEXTO TO ERR-TEXTO
                   PERFORM 999999-PROCESSAR-ROTINA-ERRO
               END-IF
           END-IF.

           ADD 1                       TO ACU-LIDOS.

           MOVE WRK-REG-EHISTOCO       TO REG-CHEQUE-OCOR-HISTORICA.

      *----------------------------------------------------------------*
       220000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       230000-CALCULAR-DATA            SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-DATA-AUX           TO WRK-DATA-ENTRADA.
           MOVE EQTDIASL-QTDE-DIAS     TO WRK-QTDE-DIAS.

           COMPUTE WRK-NUMERO-DIAS     = (WRK-QTDE-DIAS * (-1)).

           CALL 'BRAD0025'             USING WRK-DATA-ENTRADA
                                             WRK-NUMERO-DIAS
                                             WRK-DATA-SAIDA.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MSG03          TO ERR-TEXTO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           ELSE
               MOVE WRK-DATA-SAIDA     TO WRK-DATA-LIMPEZA
           END-IF.

      *----------------------------------------------------------------*
       230000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       300000-PROCESSAR                SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-HISTORICO-CHEQUE-AUX.

           MOVE ZEROS                  TO WRK-OCORR
                                          IND-1.

           PERFORM 310000-VERIFICAR-HISTORICO-CHQ  VARYING
                   IND-1  FROM 1 BY 1  UNTIL
                   IND-1               GREATER SLIGC0-QT-OCCORS.

           IF  WRK-OCORR               GREATER ZEROS
               MOVE WRK-OCORR          TO SLIGC0-QT-OCCORS
               MOVE ZEROS              TO IND-1
               PERFORM 320000-FORMATAR-SHISTOCO VARYING
                       IND-1  FROM 1 BY 1  UNTIL
                       IND-1            GREATER SLIGC0-QT-OCCORS
               PERFORM 330000-GRAVAR-SHISTOCO
               ADD 1                       TO ACU-GRAVADOS
           ELSE
               ADD 1                       TO ACU-DESPREZADOS
           END-IF.

           PERFORM 220000-LER-EHISTOCO.

      *----------------------------------------------------------------*
       300000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       310000-VERIFICAR-HISTORICO-CHQ  SECTION.
      *----------------------------------------------------------------*

           IF  SLIGC0-DT-DEPOSITO(IND-1)  NOT LESS WRK-DATA-LIMPEZA
               ADD 1                       TO WRK-OCORR
               MOVE SLIGC0-HISTORICO-CHEQUE(IND-1)
                 TO WRK-HISTORICO-CHEQUE(WRK-OCORR)
           END-IF.

      *----------------------------------------------------------------*
       310000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       320000-FORMATAR-SHISTOCO        SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-HISTORICO-CHEQUE(IND-1)
                                      TO SLIGC0-HISTORICO-CHEQUE(IND-1).

      *----------------------------------------------------------------*
       320000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       330000-GRAVAR-SHISTOCO          SECTION.
      *----------------------------------------------------------------*

           MOVE REG-CHEQUE-OCOR-HISTORICA
                                      TO WRK-REG-SHISTOCO.

           CALL 'BRAD0315'             USING WRK-SHISTOCO
                                             WRK-REG-SHISTOCO.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'BRAD0315'         TO WRK-NOME-BRAD
               MOVE 'EHISTOCO'         TO WRK-NOME-ARQUIVO
               MOVE RETURN-CODE        TO WRK-RETURN-CODE
               MOVE WRK-ERRO-TEXTO     TO ERR-TEXTO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       330000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       400000-FINALIZAR                SECTION.
      *----------------------------------------------------------------*

           PERFORM 410000-EMITIR-SRELCTRL.

           IF  ACU-GRAVADOS            GREATER ZEROS
               CALL 'FECHAARQ'         USING WRK-SHISTOCO
           END-IF.

           CLOSE EQTDIASL
                 SRELCTRL.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 110000-TESTAR-FILE-STATUS.

           GOBACK.

      *----------------------------------------------------------------*
       400000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       410000-EMITIR-SRELCTRL          SECTION.
      *----------------------------------------------------------------*

           MOVE ACU-LIDOS              TO LT1-LIDOS.
           MOVE ACU-GRAVADOS           TO LT2-GRAVADOS.
           MOVE ACU-DESPREZADOS        TO LT3-DESPREZADOS.
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           WRITE REG-SRELCTRL          FROM CABEC1.

           PERFORM 112000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM CABEC2.

           PERFORM 112000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM CABEC3.

           PERFORM 112000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT1.

           PERFORM 112000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT2.

           PERFORM 112000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT3.

           PERFORM 112000-TESTAR-FS-SRELCTRL.

      *----------------------------------------------------------------*
       410000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       999999-PROCESSAR-ROTINA-ERRO    SECTION.
      *----------------------------------------------------------------*

           MOVE 'APL'                  TO ERR-TIPO-ACESSO.
           MOVE 'SLIG6225'             TO ERR-PGM.

           CALL 'BRAD7100'             USING WRK-BATCH
                                             ERRO-AREA.

           GOBACK.

      *----------------------------------------------------------------*
       999999-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
