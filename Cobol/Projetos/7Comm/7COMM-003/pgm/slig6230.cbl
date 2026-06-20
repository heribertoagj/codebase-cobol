      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. SLIG6230.
       AUTHOR.     DANIEL AMARAL PEREIRA.
      *================================================================*
      *                  D T S  -  C O N S U L T I N G                 *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   SLIG6230                                    *
      *    PROGRAMADOR.:   DANIEL AMARAL PEREIRA     - DTS             *
      *    ANALISTA....:   REGINALDO                 - DTS             *
      *    DATA........:   12/11/2005                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   RETIRAR DO ARQUIVO DE CHEQUES AQUELES QUE   *
      *                    FOREM REENTRADAS.                           *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                        INCLUDE/BOOK      *
      *                    ENTRADA                     I#SLIG27        *
      *                    SAIDAOK                     I#SLIG27        *
      *                    DESPREZA                    I#SLIG27        *
      *                    SRELCTRL                                    *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    POL7100C - AREA PARA TRATAMENTO DE ERROS PELA POOL7100      *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    POOL7100 - TRATAMENTO DE ERROS                              *
      *    POOL7600 - DATA E HORA DO SISTEMA                           *
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

           SELECT ENTRADA  ASSIGN      TO UT-S-ENTRADA
           FILE STATUS                 IS WRK-FS-ENTRADA.

           SELECT SAIDAOK  ASSIGN      TO UT-S-SAIDAOK
           FILE STATUS                 IS WRK-FS-SAIDAOK.

           SELECT DESPREZA ASSIGN      TO UT-S-DESPREZA
           FILE STATUS                 IS WRK-FS-DESPREZA.

           SELECT SRELCTRL ASSIGN      TO UT-S-SRELCTRL
           FILE STATUS                 IS WRK-FS-SRELCTRL.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
      *    INPUT:  ARQUIVO DE CHEQUES                                  *
      *            ORG. SEQUENCIAL     -   LRECL = 300                 *
      *----------------------------------------------------------------*

       FD  ENTRADA
           RECORDING MODE IS F
           LABEL RECORD   IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

-INC I#SLIG27

           EJECT
      *----------------------------------------------------------------*
      *   OUTPUT : ARQUIVO DE CHEQUES CORRIGIDOS                       *
      *            ORG. SEQUENCIAL     -   LRECL   =   300             *
      *----------------------------------------------------------------*

       FD  SAIDAOK
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  REG-SAIDAOK                     PIC X(300).

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE CHEQUES DESPREZADOS                      *
      *            ORG. SEQUENCIAL     -   LRECL   =   300             *
      *----------------------------------------------------------------*

       FD  DESPREZA
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  REG-DESPREZA                    PIC X(300).

      *----------------------------------------------------------------*
      *    OUTPUT: DISPLAY DE TOTAIS                                   *
      *            ORG. SEQUENCIAL     -   LRECL   =   081             *
      *----------------------------------------------------------------*

       FD  SRELCTRL
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  REG-SRELCTRL                PIC  X(081).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** SLIG6230 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** ACUMULADORES ***'.
      *----------------------------------------------------------------*

       77  ACU-LIDOS-ENTRADA           PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-SAIDAOK           PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-DESPREZA          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-TP-CONVERTIDO           PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-ORIGINAL                PIC  9(009) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE FILE-STATUS ***'.
      *----------------------------------------------------------------*

       77  WRK-FS-ENTRADA              PIC  X(002)         VALUE SPACES.
       77  WRK-FS-SAIDAOK              PIC  X(002)         VALUE SPACES.
       77  WRK-FS-DESPREZA             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-SRELCTRL             PIC  X(002)         VALUE SPACES.

       77  WRK-ABERTURA                PIC  X(013)         VALUE
           ' NA ABERTURA '.
       77  WRK-LEITURA                 PIC  X(013)         VALUE
           ' NA LEITURA  '.
       77  WRK-GRAVACAO                PIC  X(013)         VALUE
           ' NA GRAVACAO '.
       77  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.

           EJECT
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*

       77  WRK-SQLCA                   PIC  X(136)         VALUE SPACES.
       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.

       01  WRK-CHV-CHEQUE-ATU.
           03  WRK-BANCO-EMIT-ATU      PIC  9(003)         VALUE ZEROS.
           03  WRK-AGENCIA-EMIT-ATU    PIC  9(005)         VALUE ZEROS.
           03  WRK-CONTA-EMIT-ATU      PIC  9(013)         VALUE ZEROS.
           03  WRK-CHEQUE-ATU          PIC  9(007)         VALUE ZEROS.
           03  WRK-DIG-CHEQUE-ATU      PIC  X(001)         VALUE SPACES.
           03  WRK-LOTE-CHEQUE-ATU     PIC  9(007)         VALUE ZEROS.

       01  WRK-CHV-CHEQUE-ANT.
           03  WRK-BANCO-EMIT-ANT      PIC  9(003)         VALUE ZEROS.
           03  WRK-AGENCIA-EMIT-ANT    PIC  9(005)         VALUE ZEROS.
           03  WRK-CONTA-EMIT-ANT      PIC  9(013)         VALUE ZEROS.
           03  WRK-CHEQUE-ANT          PIC  9(007)         VALUE ZEROS.
           03  WRK-DIG-CHEQUE-ANT      PIC  X(001)         VALUE SPACES.
           03  WRK-LOTE-CHEQUE-ANT     PIC  9(007)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA MENSAGENS ***'.
      *----------------------------------------------------------------*

       01  WRK-MENSAGENS.
           03  WRK-MSG001              PIC  X(075)         VALUE
               '** ARQUIVO ENTRADA  VAZIO **'.
           03  WRK-MSG-FORA-SEQ        PIC  X(075)         VALUE
               '** ARQUIVO ENTRADA FORA DE SEQUENCIA **'.

       01  WRK-ERRO-ARQUIVO.
           03  FILLER                  PIC  X(008)         VALUE
               '** ERRO '.
           03  WRK-OPERACAO            PIC  X(013)         VALUE SPACES.
           03  FILLER                  PIC  X(012)         VALUE
               ' DO ARQUIVO '.
           03  WRK-NOME-ARQUIVO        PIC  X(008)         VALUE SPACES.
           03  FILLER                  PIC  X(017)         VALUE
               ' - FILE-STATUS = '.
           03  WRK-FILE-STATUS         PIC  X(002)         VALUE SPACES.
           03  FILLER                  PIC  X(003)         VALUE ' **'.

           EJECT
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA POOL7600 ***'.
      *----------------------------------------------------------------*

       01  WRK-DATA-HORA.
           03  WRK-DT-JULIANA          PIC  9(005) COMP-3  VALUE ZEROS.
           03  WRK-DT-AAMMDD           PIC  9(007) COMP-3  VALUE ZEROS.
           03  WRK-DT-AAAAMMDD         PIC  9(009) COMP-3  VALUE ZEROS.
           03  WRK-TI-HHMMSS           PIC  9(007) COMP-3  VALUE ZEROS.
           03  WRK-TI-HHMMSSMMMMMM     PIC  9(013) COMP-3  VALUE ZEROS.
           03  WRK-TIMESTAMP.
               05  WRK-P7600-ANO       PIC  9(004)         VALUE ZEROS.
               05  WRK-P7600-MES       PIC  9(002)         VALUE ZEROS.
               05  WRK-P7600-DIA       PIC  9(002)         VALUE ZEROS.
               05  FILLER              PIC  X(012)         VALUE SPACES.

           EJECT
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** DEFINICAO DO RELATORIO - CABECALHOS ***'.
      *----------------------------------------------------------------*

       01  CABEC1.
           03  FILLER                  PIC  X(001)         VALUE '1'.
           03  FILLER                  PIC  X(023)         VALUE
               'BRADESCO'.
           03  FILLER                  PIC  X(047)         VALUE
               'SLIG - SISTEMA DE LIQUIDEZ GERAL'.
           03  FILLER                  PIC  X(010)         VALUE
               'FOL. UNICA'.

       01  CABEC2.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(070)         VALUE
               'SLIG6230'.
           03  CB2-DIA                 PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '/'.
           03  CB2-MES                 PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '/'.
           03  CB2-ANO                 PIC  9(004)         VALUE ZEROS.

       01  CABEC3.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(025)         VALUE SPACES.
           03  FILLER                  PIC  X(030)         VALUE
               'CORRECAO DO ARQUIVO DE CHEQUES'.

       01  CABEC4.
           03  FILLER                  PIC  X(001)         VALUE '-'.
           03  FILLER                  PIC  X(029)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'CONTROLE OPERACIONAL'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** DEFINICAO DO RELATORIO - DETALHES ***'.
      *----------------------------------------------------------------*

       01  LINTOT1.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(031)         VALUE
               'REGISTROS LIDOS    (ENTRADA) : '.
           03  LT1-TOT-LIDOS           PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT2.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(031)         VALUE
               'REGISTROS GRAVADOS (SAIDAOK) : '.
           03  LT2-TOT-GRAVADOS        PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT3.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(031)         VALUE
               'REGISTROS GRAVADOS (DESPREZA): '.
           03  LT3-TOT-GRAVADOS        PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT4.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(031)         VALUE
               'REGISTROS CONVERTIDOS        : '.
           03  LT4-TOT-GRAVADOS        PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT5.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(031)         VALUE
               'REGISTROS NAO CONVERTIDOS    : '.
           03  LT5-TOT-GRAVADOS        PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINHIFEN.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(052)         VALUE
               '----------------------------------------------------'.

           EJECT
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA POOL7100 ***'.
      *----------------------------------------------------------------*

-INC POL7100C

           EJECT
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** SLIG6230 - FIM DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       000000-INICIAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 100000-INICIALIZAR.

           PERFORM 200000-VERIFICAR-VAZIO.

           PERFORM 300000-PROCESSAR    UNTIL
                   WRK-FS-ENTRADA      EQUAL '10'.

           PERFORM 400000-FINALIZAR.

      *----------------------------------------------------------------*
       000000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       100000-INICIALIZAR              SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT  ENTRADA
                OUTPUT SAIDAOK
                       DESPREZA
                       SRELCTRL.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 110000-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       100000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       110000-TESTAR-FILE-STATUS       SECTION.
      *----------------------------------------------------------------*

           PERFORM 111000-TESTAR-FS-ENTRADA.

           PERFORM 112000-TESTAR-FS-SAIDAOK.

           PERFORM 113000-TESTAR-FS-DESPREZA.

           PERFORM 114000-TESTAR-FS-SRELCTRL.

      *----------------------------------------------------------------*
       110000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       111000-TESTAR-FS-ENTRADA        SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ENTRADA          NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'ENTRADA'          TO WRK-NOME-ARQUIVO
               MOVE  WRK-FS-ENTRADA    TO WRK-FILE-STATUS
               MOVE  WRK-ERRO-ARQUIVO  TO ERR-TEXTO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       111000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       112000-TESTAR-FS-SAIDAOK        SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SAIDAOK          NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'SAIDAOK'          TO WRK-NOME-ARQUIVO
               MOVE  WRK-FS-SAIDAOK    TO WRK-FILE-STATUS
               MOVE  WRK-ERRO-ARQUIVO  TO ERR-TEXTO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       112000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       113000-TESTAR-FS-DESPREZA       SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-DESPREZA         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'DESPREZA'         TO WRK-NOME-ARQUIVO
               MOVE  WRK-FS-DESPREZA   TO WRK-FILE-STATUS
               MOVE  WRK-ERRO-ARQUIVO  TO ERR-TEXTO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       113000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       114000-TESTAR-FS-SRELCTRL       SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SRELCTRL         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'SRELCTRL'         TO WRK-NOME-ARQUIVO
               MOVE  WRK-FS-SRELCTRL   TO WRK-FILE-STATUS
               MOVE  WRK-ERRO-ARQUIVO  TO ERR-TEXTO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       114000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       200000-VERIFICAR-VAZIO          SECTION.
      *----------------------------------------------------------------*

           PERFORM 210000-LER-ENTRADA.

           IF  WRK-FS-ENTRADA          EQUAL '10'
               DISPLAY '***************** SLIG6230 *****************'
               DISPLAY '*                                          *'
               DISPLAY '*          ARQUIVO ENTRADA VAZIO           *'
               DISPLAY '*                                          *'
               DISPLAY '*         PROCESSAMENTO CANCELADO          *'
               DISPLAY '*                                          *'
               DISPLAY '***************** SLIG6230 *****************'
               MOVE WRK-MSG001         TO ERR-TEXTO
VOLTAR         PERFORM 400000-FINALIZAR
VOLTAR*        P*RFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       200000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       210000-LER-ENTRADA              SECTION.
      *----------------------------------------------------------------*

           READ ENTRADA.

           IF      WRK-FS-ENTRADA        EQUAL '10'
                   GO                    TO    210000-99-FIM.

           MOVE    WRK-LEITURA           TO    WRK-OPERACAO.

           PERFORM 111000-TESTAR-FS-ENTRADA.

           MOVE    SLIG27-BCO-EMITT       TO    WRK-BANCO-EMIT-ATU
           MOVE    SLIG27-AGENCIA-EMITT   TO    WRK-AGENCIA-EMIT-ATU
           MOVE    SLIG27-CONTA-EMITT     TO    WRK-CONTA-EMIT-ATU
           MOVE    SLIG27-NUM-CHEQUE      TO    WRK-CHEQUE-ATU
           MOVE    SLIG27-DIG-CHEQUE      TO    WRK-DIG-CHEQUE-ATU
           MOVE    SLIG27-NUM-LOTE        TO    WRK-LOTE-CHEQUE-ATU

           PERFORM 211000-TESTAR-SEQUENCIA.

           ADD     1                       TO   ACU-LIDOS-ENTRADA.

      *----------------------------------------------------------------*
       210000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       211000-TESTAR-SEQUENCIA         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-CHV-CHEQUE-ATU  LESS  WRK-CHV-CHEQUE-ANT
               DISPLAY '***************** SLIG6230 *******************'
               DISPLAY '*                                            *'
               DISPLAY '*         PROCESSAMENTO INTERROMPIDO         *'
               DISPLAY '*                                            *'
               DISPLAY '*     ARQUIVO MVTCHQSR FORA DE SEQUENCIA     *'
               DISPLAY '*                                            *'
               DISPLAY '*     CHAVE ANTERIOR:                        *'
               DISPLAY '*                                            *'
               DISPLAY '*     BCO-EMITT       : ' WRK-BANCO-EMIT-ANT
                                                  '                  *'
               DISPLAY '*     AGENCIA-EMITT   : ' WRK-AGENCIA-EMIT-ANT
                                                    '                *'
               DISPLAY '*     CONTA-EMITT     : ' WRK-CONTA-EMIT-ANT
                                                            '        *'
               DISPLAY '*     NUM-CHEQUE      : ' WRK-CHEQUE-ANT
                                                            '        *'
               DISPLAY '*     DIG-CHEQUE      : ' WRK-DIG-CHEQUE-ANT
                                                            '        *'
               DISPLAY '*     LOTE-CHEQUE     : ' WRK-LOTE-CHEQUE-ANT
                                                            '        *'
               DISPLAY '*                                            *'
               DISPLAY '*     CHAVE ATUAL:                           *'
               DISPLAY '*                                            *'
               DISPLAY '*     BCO-EMITT       : ' WRK-BANCO-EMIT-ATU
                                                  '                  *'
               DISPLAY '*     AGENCIA-EMITT   : ' WRK-AGENCIA-EMIT-ATU
                                                    '                *'
               DISPLAY '*     CONTA-EMITT     : ' WRK-CONTA-EMIT-ATU
                                                            '        *'
               DISPLAY '*     NUM-CHEQUE      : ' WRK-CHEQUE-ATU
                                                            '        *'
               DISPLAY '*     DIG-CHEQUE      : ' WRK-DIG-CHEQUE-ATU
                                                            '        *'
               DISPLAY '*     LOTE-CHEQUE     : ' WRK-LOTE-CHEQUE-ATU
                                                            '        *'
               DISPLAY '*                                            *'
               DISPLAY '****************** SLIG6230 ******************'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE  WRK-MSG-FORA-SEQ  TO ERR-TEXTO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       211000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       300000-PROCESSAR                SECTION.
      *----------------------------------------------------------------*

           IF WRK-CHV-CHEQUE-ATU NOT EQUAL WRK-CHV-CHEQUE-ANT
              PERFORM 310000-TRATAR-QUEBRA
           ELSE
              PERFORM 330000-GRAVAR-DESPREZA  UNTIL
                      WRK-CHV-CHEQUE-ATU  NOT EQUAL WRK-CHV-CHEQUE-ANT.

      *----------------------------------------------------------------*
       300000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       310000-TRATAR-QUEBRA            SECTION.
      *----------------------------------------------------------------*

           IF  SLIG27-CTPO-CHEQ         EQUAL  4
               ADD  1                   TO     ACU-ORIGINAL
           ELSE
           IF  SLIG27-CTPO-CHEQ         EQUAL  3
               MOVE 4                   TO     SLIG27-CTPO-CHEQ
               ADD  1                   TO     ACU-TP-CONVERTIDO.

           PERFORM 320000-GRAVAR-SAIDAOK

           MOVE    WRK-CHV-CHEQUE-ATU   TO     WRK-CHV-CHEQUE-ANT

           PERFORM 210000-LER-ENTRADA.

      *----------------------------------------------------------------*
       310000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       320000-GRAVAR-SAIDAOK           SECTION.
      *----------------------------------------------------------------*

           WRITE   REG-SAIDAOK         FROM    REG-CHEQUE-DIARIA-DECENAL

           MOVE    WRK-GRAVACAO          TO    WRK-OPERACAO.
           PERFORM 112000-TESTAR-FS-SAIDAOK.

           ADD     1                     TO    ACU-GRAVA-SAIDAOK.

      *----------------------------------------------------------------*
       320000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       330000-GRAVAR-DESPREZA          SECTION.
      *----------------------------------------------------------------*

           WRITE   REG-DESPREZA        FROM    REG-CHEQUE-DIARIA-DECENAL

           MOVE    WRK-GRAVACAO          TO    WRK-OPERACAO.
           PERFORM 113000-TESTAR-FS-DESPREZA.

           ADD     1                     TO    ACU-GRAVA-DESPREZA.

           MOVE     WRK-CHV-CHEQUE-ATU  TO     WRK-CHV-CHEQUE-ANT

           PERFORM 210000-LER-ENTRADA.

      *----------------------------------------------------------------*
       330000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       400000-FINALIZAR                SECTION.
      *----------------------------------------------------------------*

           PERFORM 410000-EMITIR-SRELCTRL.

           CLOSE ENTRADA
                 SAIDAOK
                 DESPREZA
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

           CALL 'POOL7600'             USING WRK-DATA-HORA.

      *--- MOVE DATAS PARA O CABECALHO --------------------------------*

           MOVE WRK-P7600-DIA          TO CB2-DIA.
           MOVE WRK-P7600-MES          TO CB2-MES.
           MOVE WRK-P7600-ANO          TO CB2-ANO.

      *--- MOVE QUANTIDADE DE LIDOS E GRAVADOS ------------------------*

           MOVE ACU-LIDOS-ENTRADA      TO LT1-TOT-LIDOS
           MOVE ACU-GRAVA-SAIDAOK      TO LT2-TOT-GRAVADOS
           MOVE ACU-GRAVA-DESPREZA     TO LT3-TOT-GRAVADOS
           MOVE ACU-TP-CONVERTIDO      TO LT4-TOT-GRAVADOS
           MOVE ACU-ORIGINAL           TO LT5-TOT-GRAVADOS

      *--- GRAVA RELATORIO DE CONTROLE - SRELCTRL ---------------------*

           WRITE REG-SRELCTRL          FROM CABEC1.
           PERFORM 114000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM CABEC2.
           PERFORM 114000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM CABEC3.
           PERFORM 114000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM CABEC4.
           PERFORM 114000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT1.
           PERFORM 114000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT2.
           PERFORM 114000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT3.
           PERFORM 114000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT4.
           PERFORM 114000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT5.
           PERFORM 114000-TESTAR-FS-SRELCTRL.

      *----------------------------------------------------------------*
       410000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       999999-PROCESSAR-ROTINA-ERRO    SECTION.
      *----------------------------------------------------------------*

           MOVE 'SLIG6230'             TO ERR-PGM.

           CALL 'POOL7100'             USING WRK-BATCH
                                             ERRO-AREA
                                             WRK-SQLCA.

           GOBACK.

      *----------------------------------------------------------------*
       999999-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
