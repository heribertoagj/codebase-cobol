      *===============================================================*
       IDENTIFICATION                  DIVISION.
      *===============================================================*

       PROGRAM-ID. SLIG6224.
       AUTHOR.     FERNANDO BATISTI.
      *===============================================================*
      *                    C P M   S I S T E M A S                    *
      *---------------------------------------------------------------*
      *    PROGRAMA....: SLIG6224                                     *
      *    PROGRAMADOR.: FERNANDO BATISTI           - CPM PATO BRANCO *
      *    ANALISTA CPM: FRANCISMARA PAGNONCELLI    - CPM PATO BRANCO *
      *    ANALISTA....: TERRY                      - DTS - GRUPO 75  *
      *    DATA........: 30/03/2004                                   *
      *---------------------------------------------------------------*
      *    OBJETIVO....: CONSOLIDAR ARQUIVO UNIFICADO DIARIO E DECENAL*
      *      POR CLIENTE E EMITENTE.                                  *
      *---------------------------------------------------------------*
      *    ARQUIVOS....:                                              *
      *                 DDNAME                     INCLUDE/BOOK       *
      *                    EMICHESR                   I#SLIG28        *
      *                    CONSCHEA                   I#SLIG43        *
      *                    SRELCTRL                       -           *
      *---------------------------------------------------------------*
      *    INC'S.......:                                              *
      *    POL7100C - AREA DE TRATAMENTO DE ERRO PELA POOL7100        *
      *---------------------------------------------------------------*
      *    MODULOS.....:                                              *
      *    POOL7100 - MODULO DE TRATAMENTO DE ERRO                    *
      *    POOL7600 - OBTEM DATA E HORA DO SISTEMA                    *
      *===============================================================*
      *                    A L T E R A C A O                           *
      *----------------------------------------------------------------*
      *    PROGRAMADOR.:   LUIS VIACELLI FERNANDES   - CPM PATO BRANCO *
      *    ANALISTA CPM:   FABIO FINK                - CPM PATO BRANCO *
      *    ANALISTA....:   TERRY                     - DTS / GP. 75    *
      *    DATA........:   08/04/2004                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   INSERIR CONSISTENCIA NO ARQUIVO EMICHESR,   *
      *      POR ORDEM ASCENDENTE.                                     *
      *----------------------------------------------------------------*
BRQ250* 22/02/2013  - ALTERACAO NO TRATAMENTO DO CLIENTE E EMITENTE    *
BRQ250*               PASSA A SER PELO CODIGO DO CNPJ/CPF     LETO     *
BRQ250*             - MANTER A FILIAL E CONTROLE DO EMITENTE E CLIENTE *
BRQ250*             - MANTER A CONTA CORRENTE DO CLIENTE               *
      *================================================================*

      *===============================================================*
       ENVIRONMENT                     DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *---------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *---------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *---------------------------------------------------------------*

       FILE-CONTROL.

           SELECT EMICHESR ASSIGN      TO UT-S-EMICHESR
           FILE STATUS                 IS WRK-FS-EMICHESR.

           SELECT CONSCHEA ASSIGN      TO UT-S-CONSCHEA
           FILE STATUS                 IS WRK-FS-CONSCHEA.

           SELECT SRELCTRL ASSIGN      TO UT-S-SRELCTRL
           FILE STATUS                 IS WRK-FS-SRELCTRL.


      *===============================================================*
       DATA                            DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       FILE                            SECTION.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      *    INPUT:  ARQUIVO UNIFICADO DIARIO E DECENAL                 *
      *            ORG. SEQUENCIAL     -   LRECL   =   300            *
      *---------------------------------------------------------------*

       FD  EMICHESR
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#SLIG28'.


      *---------------------------------------------------------------*
      *    OUTPUT: ARQUIVO CONSOLIDADO LIQUIDEZ BANCO                 *
      *            ORG. SEQUENCIAL     -   LRECL   =   300            *
      *---------------------------------------------------------------*

       FD  CONSCHEA
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#SLIG43'.


      *---------------------------------------------------------------*
      *    OUTPUT: RELATORIO DE CONTROLE                              *
      *            ORG. SEQUENCIAL     -   LRECL   =   080            *
      *---------------------------------------------------------------*

       FD  SRELCTRL
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-SRELCTRL             PIC  X(080).


      *---------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** SLIG6224 - INICIO DA AREA DE WORKING ***'.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** ACUMULADORES ***'.
      *---------------------------------------------------------------*

       77  ACU-LIDOS-EMICHESR         PIC  9(009) COMP-3   VALUE ZEROS.
       77  ACU-GRAVA-CONSCHEA         PIC  9(009) COMP-3   VALUE ZEROS.

       77  ACU-VCTO-PER-PG            PIC  9(013)V99 COMP-3 VALUE ZEROS.
       77  ACU-VCTO-PER-NAO-PG        PIC  9(013)V99 COMP-3 VALUE ZEROS.
       77  ACU-VCTO-PER-BXADO         PIC  9(013)V99 COMP-3 VALUE ZEROS.
       77  ACU-VCTO-APOS-PER-PG       PIC  9(013)V99 COMP-3 VALUE ZEROS.
       77  ACU-VCTO-APOS-PER-NAO-PG   PIC  9(013)V99 COMP-3 VALUE ZEROS.
       77  ACU-VCTO-APOS-PER-BXADO    PIC  9(013)V99 COMP-3 VALUE ZEROS.
       77  ACU-CHEQ-PRIM-DEVLC        PIC  9(013)V99 COMP-3 VALUE ZEROS.
       77  ACU-CHEQ-REAP              PIC  9(013)V99 COMP-3 VALUE ZEROS.
       77  ACU-CHEQ-REAP-DEVOL        PIC  9(013)V99 COMP-3 VALUE ZEROS.
       77  ACU-CHEQ-REAP-PG           PIC  9(013)V99 COMP-3 VALUE ZEROS.
       77  ACU-CHEQ-ANT-DEVOL         PIC  9(013)V99 COMP-3 VALUE ZEROS.
       77  WRK-FLAG-CHEQ-REAP         PIC  X(001)          VALUE SPACES.
       77  WRK-FLAG-ABERTOS           PIC  X(001)          VALUE SPACES.

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** VARIAVEIS DE FILE-STATUS ***'.
      *---------------------------------------------------------------*

       77  WRK-FS-EMICHESR             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-CONSCHEA             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-SRELCTRL             PIC  X(002)         VALUE SPACES.

       77  WRK-ABERTURA                PIC  X(013)         VALUE
           ' NA ABERTURA '.
       77  WRK-LEITURA                 PIC  X(013)         VALUE
           ' NA  LEITURA '.
       77  WRK-GRAVACAO                PIC  X(013)         VALUE
           ' NA GRAVACAO '.
       77  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.


      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE AUXILIARES ***'.
      *---------------------------------------------------------------*

       77  WRK-BATCH                   PIC  X(008)        VALUE 'BATCH'.

       77  WRK-MSG03                   PIC  X(075)        VALUE
           '** ARQUIVO EMICHESR VAZIO **'.
       77  WRK-MSG-FORA-SEQ            PIC  X(075)         VALUE
           '** ARQUIVO EMICHESR FORA DE SEQUENCIA **'.

       01  WRK-CHV-EMICHESR-ANT        PIC  X(040)        VALUE SPACES.
       01  WRK-CHV-EMICHESR-ATU        PIC  X(040)        VALUE ZEROS.
       01  FILLER REDEFINES WRK-CHV-EMICHESR-ATU.
           03  WRK-BANCO-EMI-ATU       PIC  9(003).
           03  WRK-AGENCIA-EMI-ATU     PIC  9(005).
           03  WRK-CONTA-EMI-ATU       PIC  9(013).
           03  WRK-CGCCPF-CLI-ATU      PIC  X(009).
BRQ250     03  WRK-FILIAL-CLI-ATU      PIC  X(004).
           03  WRK-CARTEIRA-ATU        PIC  9(003).

       01  WRK-CHV-ANT.
           03  WRK-BCO-ANT             PIC  9(003)        VALUE ZEROS.
           03  WRK-AGE-ANT             PIC  9(005)        VALUE ZEROS.
           03  WRK-CTA-ANT             PIC  9(013)        VALUE ZEROS.
           03  WRK-CGC-ANT             PIC  X(009)        VALUE SPACES.
BRQ250     03  WRK-FIL-ANT             PIC  X(004)        VALUE SPACES.
           03  WRK-CAR-ANT             PIC  9(003)        VALUE ZEROS.

       01  WRK-CHV-ATU.
           03  WRK-BCO-ATU             PIC  9(003)        VALUE ZEROS.
           03  WRK-AGE-ATU             PIC  9(005)        VALUE ZEROS.
           03  WRK-CTA-ATU             PIC  9(013)        VALUE ZEROS.
BRQ250     03  WRK-CGC-ATU             PIC  X(009)        VALUE SPACES.
           03  WRK-FIL-ATU             PIC  X(004)        VALUE SPACES.
           03  WRK-CAR-ATU             PIC  9(003)        VALUE ZEROS.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ERROS DE FILE-STATUS ***'.
      *---------------------------------------------------------------*

       01  WRK-ERRO-TEXTO.
           03  FILLER                  PIC  X(008)         VALUE
               '** ERRO '.
           03  WRK-OPERACAO            PIC  X(013)         VALUE SPACES.
           03  FILLER                  PIC  X(012)         VALUE
               ' DO ARQUIVO '.
           03  WRK-NOME-ARQ            PIC  X(008)         VALUE SPACES.
           03  FILLER                  PIC  X(018)         VALUE
               ' - FILE STATUS = '.
           03  WRK-FILE-STATUS         PIC  X(002)         VALUE SPACES.
           03  FILLER                  PIC  X(003)         VALUE ' **'.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA POOL7600 ***'.
      *---------------------------------------------------------------*

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
               05  FILLER              PIC  X(012)         VALUE SPACES.


      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DO RELATORIO SRELCTRL ***'.
      *---------------------------------------------------------------*

       01  CABEC1.
           03  FILLER                  PIC  X(001)         VALUE '1'.
           03  FILLER                  PIC  X(016)         VALUE
               'BRADESCO'.
           03  FILLER                  PIC  X(053)         VALUE
               'SLIG - SISTEMA DE INDICE DE LIQUIDEZ'.
           03  FILLER                  PIC  X(011)         VALUE
               'FOL. UNICA'.

       01  CABEC2.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(069)         VALUE
               'SLIG6224'.
           03  CB2-DIA                 PIC  99             VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '/'.
           03  CB2-MES                 PIC  99             VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '/'.
           03  CB2-ANO                 PIC  9999           VALUE ZEROS.

       01  CABEC3.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(014)         VALUE SPACES.
           03  FILLER                  PIC  X(040)         VALUE
               'CONSOLIDA ARQUIVO UNIFICADO POR EMITENTE'.

       01  CABEC4.
           03  FILLER                  PIC  X(001)         VALUE '-'.
           03  FILLER                  PIC  X(026)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               'CONTROLE OPERACIONAL'.

       01  LINDET1.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(009)         VALUE SPACES.
           03  FILLER                  PIC  X(035)         VALUE
               'REGISTROS LIDOS      (EMICHESR) :  '.
           03  LD1-LIDOS-EMICHESR      PIC  ZZZ.ZZZ.ZZ9    VALUE ZEROS.

       01  LINDET2.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(009)         VALUE SPACES.
           03  FILLER                  PIC  X(035)         VALUE
               'REGISTROS GRAVADOS   (CONSCHEA) :  '.
           03  LD2-GRAVA-CONSCHEA      PIC  ZZZ.ZZZ.ZZ9    VALUE ZEROS.


      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA POOL7100 ***'.
      *---------------------------------------------------------------*

       COPY POL7100C.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** SLIG6224 - FIM DA AREA DE WORKING ***'.
      *---------------------------------------------------------------*

      *===============================================================*
       PROCEDURE                       DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *---------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-VERIFICAR-VAZIO.

           PERFORM 3000-PROCESSAR
             UNTIL WRK-FS-EMICHESR     EQUAL '10'.

           PERFORM 4000-FINALIZAR.

      *---------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *---------------------------------------------------------------*

           OPEN INPUT  EMICHESR
                OUTPUT CONSCHEA
                       SRELCTRL.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

           CALL 'POOL7600'             USING WRK-DATA-HORA.

           MOVE WRK-DIA                TO CB2-DIA.
           MOVE WRK-MES                TO CB2-MES.
           MOVE WRK-ANO                TO CB2-ANO.

      *---------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *---------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-EMICHESR.

           PERFORM 1130-TESTAR-FS-CONSCHEA.

           PERFORM 1140-TESTAR-FS-SRELCTRL.

      *---------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1110-TESTAR-FS-EMICHESR         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-EMICHESR         NOT EQUAL ZEROS
               MOVE 'EMICHESR'         TO WRK-NOME-ARQ
               MOVE WRK-FS-EMICHESR    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-TEXTO     TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1130-TESTAR-FS-CONSCHEA         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-CONSCHEA         NOT EQUAL ZEROS
               MOVE 'CONSCHEA'         TO WRK-NOME-ARQ
               MOVE WRK-FS-CONSCHEA    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-TEXTO     TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1130-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1140-TESTAR-FS-SRELCTRL         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-SRELCTRL         NOT EQUAL ZEROS
               MOVE 'SRELCTRL'         TO WRK-NOME-ARQ
               MOVE WRK-FS-SRELCTRL    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-TEXTO     TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1140-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *---------------------------------------------------------------*

           PERFORM 2200-LER-EMICHESR.

           IF  WRK-FS-EMICHESR         EQUAL '10'
               DISPLAY '************** SLIG6224 ****************'
               DISPLAY '*                                      *'
               DISPLAY '*       ARQUIVO EMICHESR VAZIO         *'
               DISPLAY '*                                      *'
               DISPLAY '*      PROCESSAMENTO  CANCELADO        *'
               DISPLAY '*                                      *'
               DISPLAY '************** SLIG6224 ****************'
               MOVE WRK-MSG03          TO ERR-TEXTO
VOLTAR         PERFORM 4000-FINALIZAR
VOLTAR*        P*RFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2200-LER-EMICHESR               SECTION.
      *---------------------------------------------------------------*

           READ EMICHESR.

           IF  WRK-FS-EMICHESR         EQUAL '10'
               GO TO 2200-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.
           PERFORM 1110-TESTAR-FS-EMICHESR.

           MOVE SLIG28-BCO-EMITT       TO WRK-BANCO-EMI-ATU
                                          WRK-BCO-ATU.
           MOVE SLIG28-AGENCIA-EMITT   TO WRK-AGENCIA-EMI-ATU
                                          WRK-AGE-ATU.
           MOVE SLIG28-CONTA-EMITT     TO WRK-CONTA-EMI-ATU
                                          WRK-CTA-ATU.
           MOVE SLIG28-CGC-CPF-CLI-ST
           TO WRK-CGCCPF-CLI-ATU
                                          WRK-CGC-ATU.
BRQ250     MOVE SLIG28-FILIAL-CLI-ST   TO WRK-FILIAL-CLI-ATU
BRQ250                                    WRK-FIL-ATU.
           MOVE SLIG28-CARTEIRA        TO WRK-CARTEIRA-ATU
                                          WRK-CAR-ATU.

           PERFORM 2210-TESTAR-SEQUENCIA.

           ADD 1                       TO ACU-LIDOS-EMICHESR.

      *---------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2210-TESTAR-SEQUENCIA           SECTION.
      *---------------------------------------------------------------*

           IF  WRK-CHV-ATU             LESS WRK-CHV-ANT
               DISPLAY '***************** SLIG6224 *******************'
               DISPLAY '*                                            *'
               DISPLAY '*         PROCESSAMENTO INTERROMPIDO         *'
               DISPLAY '*                                            *'
               DISPLAY '*     ARQUIVO EMICHESR FORA DE SEQUENCIA     *'
               DISPLAY '*                                            *'
               DISPLAY '*     CHAVE ANTERIOR:                        *'
               DISPLAY '*                                            *'
               DISPLAY '*     BCO-EMITT       : ' WRK-BCO-ANT
                                                  '                  *'
               DISPLAY '*     AGENCIA-EMITT   : ' WRK-AGE-ANT
                                                    '                *'
               DISPLAY '*     CONTA-EMITT     : ' WRK-CTA-ANT
                                                            '        *'
               DISPLAY '*     CGC-CPF-CLI     : ' WRK-CGC-ANT
                                                        '            *'
BRQ250         DISPLAY '*     FILIAL-CLI      : ' WRK-FIL-ANT
BRQ250                                             '                 *'
               DISPLAY '*     CARTEIRA        : ' WRK-CAR-ANT
                                                  '                  *'
               DISPLAY '*                                            *'
               DISPLAY '*     CHAVE ATUAL:                           *'
               DISPLAY '*                                            *'
               DISPLAY '*     BCO-EMITT       : ' WRK-BCO-ATU
                                                  '                  *'
               DISPLAY '*     AGENCIA-EMITT   : ' WRK-AGE-ATU
                                                    '                *'
               DISPLAY '*     CONTA-EMITT     : ' WRK-CTA-ATU
                                                            '        *'
               DISPLAY '*     CGC-CPF-CLI     : ' WRK-CGC-ATU
                                                        '            *'
BRQ250         DISPLAY '*     FILIAL-CLI      : ' WRK-FIL-ATU
BRQ250                                             '                 *'
               DISPLAY '*     CARTEIRA        : ' WRK-CAR-ATU
                                                  '                  *'
               DISPLAY '*                                            *'
               DISPLAY '****************** SLIG6224 ******************'
               MOVE  WRK-MSG-FORA-SEQ  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-CHV-ATU            TO WRK-CHV-ANT.

      *---------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *---------------------------------------------------------------*

           PERFORM 3100-FORMATAR-CAMPOS.

           PERFORM 3200-ACUMULAR-VALORES
             UNTIL WRK-CHV-EMICHESR-ATU NOT EQUAL WRK-CHV-EMICHESR-ANT
                OR WRK-FS-EMICHESR      EQUAL '10'.

           PERFORM 3300-GRAVAR-CONSCHEA.

      *---------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3100-FORMATAR-CAMPOS            SECTION.
      *---------------------------------------------------------------*

           INITIALIZE REG-CHEQUE-CONSOLIDADO-EMITT.

           MOVE SLIG28-CGC-CPF-EMITT-ST     TO SLIG43-CGC-CPF-EMITT-ST.
           MOVE SLIG28-TIPO-PESSOA-EMITT TO SLIG43-TIPO-PESSOA-EMITT.
           MOVE SLIG28-BCO-EMITT         TO SLIG43-BCO-EMITT.
           MOVE SLIG28-AGENCIA-EMITT     TO SLIG43-AGENCIA-EMITT.
           MOVE SLIG28-CONTA-EMITT       TO SLIG43-CONTA-EMITT.
           MOVE SLIG28-CARTEIRA          TO SLIG43-CARTEIRA.
           MOVE SLIG28-NOME-EMITT        TO SLIG43-NOME-EMITT.
           MOVE SLIG28-BCO-CLI           TO SLIG43-BCO-CLI.
           MOVE SLIG28-AGENCIA-CLI       TO SLIG43-AGENCIA-CLI.
           MOVE SLIG28-CGC-CPF-CLI-ST       TO SLIG43-CGC-CPF-CLI-ST.
           MOVE SLIG28-TIPO-PESSOA-CLI   TO SLIG43-TIPO-PESSOA-CLI.
           MOVE SLIG28-NOME-CLI          TO SLIG43-NOME-CLI.
           MOVE SLIG28-BCO-DIR-REGIONAL  TO SLIG43-BCO-DIR-REGIONAL.
           MOVE SLIG28-DIR-REGIONAL      TO SLIG43-DIR-REGIONAL.
           MOVE SLIG28-BCO-GERC-REGIONAL TO SLIG43-BCO-GERC-REGIONAL.
           MOVE SLIG28-GERC-REGIONAL     TO SLIG43-GERC-REGIONAL.
           MOVE SLIG28-GRP-ECONOMICO-EMITT
                                         TO SLIG43-GRP-ECONOMICO-EMITT.
           MOVE SLIG28-GRP-ECONOMICO-CLI TO SLIG43-GRP-ECONOMICO-CLI.

BRQ250     MOVE SLIG28-FILIAL-EMITT-ST      TO SLIG43-FILIAL-EMITT-ST
BRQ250     MOVE SLIG28-CONTROLE-EMITT-ST    TO SLIG43-CONTROLE-EMITT-ST
BRQ250     MOVE SLIG28-FILIAL-CLI-ST        TO SLIG43-FILIAL-CLI-ST
BRQ250     MOVE SLIG28-CONTROLE-CLI-ST      TO SLIG43-CONTROLE-CLI-ST
BRQ250     MOVE SLIG28-CONTA-CLI         TO SLIG43-CONTA-CLI

           MOVE WRK-CHV-EMICHESR-ATU     TO WRK-CHV-EMICHESR-ANT.

      *---------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3200-ACUMULAR-VALORES           SECTION.
      *---------------------------------------------------------------*

      *--- LEGENDA DOS CAMPOS ACUMULADORES ---------------------------*

      *  ABERTOS PERIODO E FUTUROS--ACU-VCTO-APOS-PER-NAO-PG
      *  PAGOS NO PRIMEIRO DEPOSITO--ACU-VCTO-PER-PG
      *  BAIXA ANTECIPADA NO PERIODO-ACU-VCTO-PER-NAO-PG
      *  BAIXA NORMAL----ACU-VCTO-PER-BXADO
      *  REAPRESENTADOS----ACU-VCTO-APOS-PER-PG
      *  REAPRESENTADOS PERIODO ANTERIOR-ACU-VCTO-APOS-PER-BXADO
      *  PRIMEIRA DEVOLUCAO---ACU-CHEQ-PRIM-DEVLC
      *  REAPRESENTADOS DEVOLVIDOS--ACU-CHEQ-REAP-DEVOL
      *  REAPRESENTADOS PAGOS---ACU-CHEQ-REAP-PG
      *  DEVOLUCAO PERIODO ANTERIOR--ACU-CHEQ-ANT-DEVOL
      *  DEPOSITOS NO PERIODO---ACU-CHEQ-REAP

      *--- LEGENDA DOS CAMPOS ACUMULADORES ---------------------------*

           MOVE SPACES TO WRK-FLAG-CHEQ-REAP
                          WRK-FLAG-ABERTOS.

           PERFORM 3204-ABERTOS-FUTURO.
           PERFORM 3201-BAIXAS-ANTECIPADAS.
           PERFORM 3202-BAIXAS-NORMAIS.
           PERFORM 3203-PAGOS-PRIM-DEPOSITO.
           PERFORM 3206-PRIMEIRA-DEVOLUCAO.
           PERFORM 3207-REAPRESENTADOS-PAGOS.
           PERFORM 3208-REAPRESENTADOS-DEVOL.
           PERFORM 3209-DEVOLUCAO-PER-ANTERIOR.
           PERFORM 3211-REAPRESENTADO-PER-ANT.
           PERFORM 3210-DEPOSITOS-PER-ATUAL.

           MOVE WRK-CHV-EMICHESR-ATU   TO WRK-CHV-EMICHESR-ANT.

           PERFORM 2200-LER-EMICHESR.

      *---------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3201-BAIXAS-ANTECIPADAS       SECTION.
      *---------------------------------------------------------------*

      *--------------- CONTABILIZA BAIXAS ANTECIPADAS ----------------*

      *--- CONCEITO:                                                  *
      *--- CHEQUES COM DATA DEPOSITO NO PERIODO ATUAL OU FUTURO E     *
      *--- DATA CONTABIL MENOR QUE A DATA DEPOSITO SENDO A DATA       *
      *--- CONTABIL NO PERIODO ATUAL                                  *

           IF  SLIG28-TOT-PER-BAX-ANT  NOT EQUAL ZEROS OR
               SLIG28-TOT-FUT-BAX-ANT  NOT EQUAL ZEROS
              ADD SLIG28-VL-PRINC-CHEQ TO ACU-VCTO-PER-NAO-PG

              IF WRK-FLAG-ABERTOS EQUAL 'S'
                 SUBTRACT SLIG28-VL-PRINC-CHEQ FROM
                          ACU-VCTO-APOS-PER-NAO-PG
              END-IF
           END-IF.

      *---------------------------------------------------------------*
       3201-99-FIM.                  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3202-BAIXAS-NORMAIS           SECTION.
      *---------------------------------------------------------------*

      *--------------- CONTABILIZA BAIXAS NORMAIS --------------------*

      *--- CONCEITO:                                                  *
      *--- CHEQUES COM DATA DEPOSITO NO PERIODO ATUAL E DATA CONTABIL *
      *--- MAIOR OU IGUAL A DATA-DEPOSITO                             *

           IF SLIG28-TOT-PER-BAIX         NOT EQUAL ZEROS
              ADD SLIG28-VL-PRINC-CHEQ TO ACU-VCTO-PER-BXADO

              IF WRK-FLAG-ABERTOS EQUAL 'S'
                 SUBTRACT SLIG28-VL-PRINC-CHEQ FROM
                          ACU-VCTO-APOS-PER-NAO-PG
              END-IF
           END-IF.

      *---------------------------------------------------------------*
       3202-99-FIM.                  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3203-PAGOS-PRIM-DEPOSITO  SECTION.
      *---------------------------------------------------------------*

      *------- CONTABILIZA CHEQUES PAGOS NO PRIMEIRO DEPOSITO --------*

      *--- CONCEITO:                                                  *
      *--- CHEQUES COM DATA DEPOSITO NO PERIODO ATUAL E SEM DEVOLUCAO *
      *--- OU REAPRESENTACAO NO PERIODO ATUAL E FUTURO                *

           IF SLIG28-ULT-CTPO-CHEQ           EQUAL 2               AND
              SLIG28-TOT-PER-COMP            NOT EQUAL ZEROS       AND
              SLIG28-TOT-PER-DEVL-S-RESP     EQUAL ZEROS           AND
              SLIG28-TOT-PER-DEVL-S-REAP     EQUAL ZEROS           AND
              SLIG28-TOT-PER-DEVL-N-REAP     EQUAL ZEROS           AND
              SLIG28-TOT-FUT-DEVL-S-RESP     EQUAL ZEROS           AND
              SLIG28-TOT-FUT-DEVL-S-REAP     EQUAL ZEROS           AND
              SLIG28-TOT-FUT-DEVL-N-REAP     EQUAL ZEROS           AND
              SLIG28-TOT-PER-BAIX            EQUAL ZEROS           AND
              SLIG28-TOT-FUT-BAIX            EQUAL ZEROS           AND
              SLIG28-TOT-PER-BAX-ANT         EQUAL ZEROS           AND
              SLIG28-TOT-FUT-BAX-ANT         EQUAL ZEROS
              ADD SLIG28-VL-PRINC-CHEQ TO ACU-VCTO-PER-PG
              MOVE 'S'                 TO WRK-FLAG-CHEQ-REAP
           END-IF.

      *---------------------------------------------------------------*
       3203-99-FIM.                  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3204-ABERTOS-FUTURO           SECTION.
      *---------------------------------------------------------------*

      *------- CONTABILIZA CHEQUES ABERTOS NO PERIODO E FUTURO  ------*

      *--- CONCEITO:                                                  *
      *--- CHEQUES EM ABERTO ATE  O FINAL DO PERIODO ATUAL            *
      *--- CHEQUES NORMAIS E REENTRADAS                               *

           IF (SLIG28-TOT-FUT-ABER        NOT EQUAL ZEROS  OR
               SLIG28-TOT-PER-ABER        NOT EQUAL ZEROS) AND
               SLIG28-TOT-PER-DEVL-S-REAP     EQUAL ZEROS  AND
               SLIG28-TOT-PER-DEVL-N-REAP     EQUAL ZEROS  AND
               SLIG28-TOT-FUT-DEVL-S-REAP     EQUAL ZEROS  AND
               SLIG28-TOT-FUT-DEVL-N-REAP     EQUAL ZEROS
              ADD SLIG28-VL-PRINC-CHEQ TO ACU-VCTO-APOS-PER-NAO-PG
              MOVE 'S'                 TO WRK-FLAG-ABERTOS
           END-IF.

      *---------------------------------------------------------------*
       3204-99-FIM.                  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3206-PRIMEIRA-DEVOLUCAO       SECTION.
      *---------------------------------------------------------------*

      *--------- CONTABILIZA CHEQUES DA PRIMEIRA DEVOLUCAO -----------*

      *--- CONCEITO:                                                  *
      *--- CHEQUES COM DATA DEPOSITO NO PERIODO ATUAL E COM APENAS    *
      *--- UMA DEVOLUCAO ATE A CARENCIA (FUTURO)                      *

           IF (SLIG28-TOT-PER-DEVL-S-RESP    EQUAL 1     OR
               SLIG28-TOT-FUT-DEVL-S-RESP    EQUAL 1     )
              ADD SLIG28-VL-PRINC-CHEQ TO ACU-CHEQ-PRIM-DEVLC
              MOVE 'S'                 TO WRK-FLAG-CHEQ-REAP
           END-IF.

      *---------------------------------------------------------------*
       3206-99-FIM.                  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3207-REAPRESENTADOS-PAGOS     SECTION.
      *---------------------------------------------------------------*

      *------- CONTABILIZA CHEQUES REAPRESENTADOS PAGOS --------------*

      *--- CONCEITO:                                                  *
      *--- CHEQUES COM DATA DEPOSITO NO PERIODO ATUAL OU ANTERIOR     *
      *--- SEM DEVOLUCOES (PAGO) ATE A CARENCIA (FUTURO)              *

           IF (SLIG28-ULT-CTPO-CHEQ           EQUAL 2 OR
               SLIG28-ULT-CTPO-CHEQ           EQUAL 3) AND
              (SLIG28-TOT-PER-REAP-S-RESP NOT EQUAL 0  OR
               SLIG28-TOT-PER-REAP-N-RESP NOT EQUAL 0  OR
               SLIG28-TOT-FUT-REAP-S-RESP NOT EQUAL 0  OR
               SLIG28-TOT-FUT-REAP-N-RESP NOT EQUAL 0)
              ADD SLIG28-VL-PRINC-CHEQ TO ACU-CHEQ-REAP-PG
                                          ACU-VCTO-APOS-PER-PG
              MOVE 'S'                 TO WRK-FLAG-CHEQ-REAP
           END-IF.

      *---------------------------------------------------------------*
       3207-99-FIM.                  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3208-REAPRESENTADOS-DEVOL      SECTION.
      *---------------------------------------------------------------*

      *---- CONTABILIZA CHEQUES REAPRESENTADOS DEVOLVIDOS ------------*

      *--- CONCEITO:                                                  *
      *--- CHEQUES COM DATA DEPOSITO NO PERIODO ATUAL OU ANTERIOR     *
      *--- E DEVOLVIDOS ATE A CARENCIA (FUTURO)                       *

           IF  SLIG28-TOT-PER-DEVL-N-REAP     EQUAL 0 AND
             ((SLIG28-TOT-PER-REAP-S-RESP NOT EQUAL 0 AND
               SLIG28-TOT-PER-DEVL-S-RESP >
               SLIG28-TOT-PER-REAP-S-RESP) OR
              (SLIG28-TOT-PER-DEVL-S-RESP EQUAL
               SLIG28-TOT-PER-REAP-S-RESP AND
               SLIG28-TOT-PER-DEVL-S-RESP NOT EQUAL 0 AND
               SLIG28-TOT-FUT-DEVL-S-RESP NOT EQUAL 0 ))
                ADD SLIG28-VL-PRINC-CHEQ TO ACU-CHEQ-REAP-DEVOL
                                            ACU-VCTO-APOS-PER-PG
                MOVE 'S'                 TO WRK-FLAG-CHEQ-REAP
           ELSE
           IF  SLIG28-TOT-FUT-DEVL-N-REAP     EQUAL 0 AND
              (SLIG28-TOT-FUT-REAP-S-RESP NOT EQUAL 0 AND
               SLIG28-TOT-FUT-DEVL-S-RESP >
               SLIG28-TOT-FUT-REAP-S-RESP)
      *       (SLIG28-TOT-FUT-DEVL-S-RESP NOT EQUAL 0 AND
      *       (SLIG28-TOT-FUT-REAP-N-RESP NOT EQUAL 0 OR
      *        SLIG28-TOT-FUT-REAP-S-RESP NOT EQUAL 0 )) )
                ADD SLIG28-VL-PRINC-CHEQ TO ACU-CHEQ-REAP-DEVOL
                                            ACU-VCTO-APOS-PER-PG
                MOVE 'S'                 TO WRK-FLAG-CHEQ-REAP
           END-IF
           END-IF.

      *---------------------------------------------------------------*
       3208-99-FIM.                  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3209-DEVOLUCAO-PER-ANTERIOR   SECTION.
      *---------------------------------------------------------------*

      *---- CONTABILIZA CHEQUES DEVOLUCAO PERIODO ANTERIOR -----------*

      *--- CONCEITO:                                                  *
      *--- CHEQUES DEVOLVIDOS NO PERIODO ATUAL ATE A CARENCIA E       *
      *--- CONSIDERADOS PAGOS NO PERIODO ANTERIOR                     *

           IF ((SLIG28-TOT-ANT-REAP-S-RESP     EQUAL 0   AND
                SLIG28-TOT-ANT-REAP-N-RESP     EQUAL 0   AND
                SLIG28-TOT-ANT-DEVL-S-RESP     EQUAL 0   AND
                SLIG28-TOT-ANT-DEVL-S-REAP     EQUAL 0   AND
                SLIG28-TOT-ANT-DEVL-N-REAP     EQUAL 0   AND
                SLIG28-TOT-ANT-COMP        NOT EQUAL 0)  AND
               (SLIG28-TOT-PER-DEVL-S-RESP NOT EQUAL 0   OR
                SLIG28-TOT-PER-DEVL-S-REAP NOT EQUAL 0   OR
                SLIG28-TOT-PER-DEVL-N-REAP NOT EQUAL 0   OR
                SLIG28-TOT-FUT-DEVL-S-RESP NOT EQUAL 0   OR
                SLIG28-TOT-FUT-DEVL-S-REAP NOT EQUAL 0   OR
                SLIG28-TOT-FUT-DEVL-N-REAP NOT EQUAL 0))
              ADD SLIG28-VL-PRINC-CHEQ TO ACU-CHEQ-ANT-DEVOL
           ELSE
           IF SLIG28-TOT-ANT-DEVL-S-REAP = 0  AND
              SLIG28-TOT-ANT-DEVL-N-REAP = 0  AND
              SLIG28-TOT-ANT-REAP-S-RESP > 0  AND
              SLIG28-TOT-ANT-REAP-S-RESP =
              SLIG28-TOT-ANT-DEVL-S-RESP      AND
            ((SLIG28-TOT-PER-DEVL-S-RESP >
              SLIG28-TOT-ANT-DEVL-S-RESP      OR
              SLIG28-TOT-FUT-DEVL-S-RESP >
              SLIG28-TOT-ANT-DEVL-S-RESP)     OR
             (SLIG28-TOT-PER-DEVL-S-REAP > 0  OR
              SLIG28-TOT-FUT-DEVL-S-REAP > 0  OR
              SLIG28-TOT-PER-DEVL-N-REAP > 0  OR
              SLIG28-TOT-FUT-DEVL-N-REAP > 0))
              ADD SLIG28-VL-PRINC-CHEQ TO ACU-CHEQ-ANT-DEVOL
           ELSE
           IF SLIG28-TOT-ANT-DEVL-N-REAP = 0 AND
              SLIG28-TOT-ANT-REAP-N-RESP > 0 AND
              SLIG28-TOT-ANT-REAP-N-RESP =
              SLIG28-TOT-ANT-DEVL-S-REAP  AND
            ((SLIG28-TOT-PER-DEVL-S-REAP >
              SLIG28-TOT-ANT-DEVL-S-REAP  OR
              SLIG28-TOT-FUT-DEVL-S-REAP >
              SLIG28-TOT-ANT-DEVL-S-REAP) OR
             (SLIG28-TOT-PER-DEVL-S-RESP >
              SLIG28-TOT-ANT-DEVL-S-RESP  OR
              SLIG28-TOT-FUT-DEVL-S-RESP >
              SLIG28-TOT-ANT-DEVL-S-RESP) OR
             (SLIG28-TOT-PER-DEVL-N-REAP > 0 OR
              SLIG28-TOT-FUT-DEVL-N-REAP > 0))
              ADD SLIG28-VL-PRINC-CHEQ TO ACU-CHEQ-ANT-DEVOL
           END-IF
           END-IF
           END-IF.

      *---------------------------------------------------------------*
       3209-99-FIM.                  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3210-DEPOSITOS-PER-ATUAL      SECTION.
      *---------------------------------------------------------------*

      *---- CONTABILIZA DEPOSITOS DE CHEQUES NO PERIODO ATUAL --------*

      *--- CONCEITO:                                                  *
      *--- CHEQUES COM DATA DEPOSITO NO PERIODO ATUAL E SEM BAIXA     *

           IF ((WRK-FLAG-CHEQ-REAP            EQUAL 'S') OR
              (SLIG28-TOT-PER-ABER        NOT EQUAL 0    OR
               SLIG28-TOT-PER-COMP        NOT EQUAL 0    OR
               SLIG28-TOT-PER-REAP-S-RESP NOT EQUAL 0    OR
               SLIG28-TOT-PER-DEVL-S-RESP NOT EQUAL 0)   AND
              (SLIG28-TOT-PER-BAIX            EQUAL 0    AND
               SLIG28-TOT-PER-BAX-ANT         EQUAL 0    AND
               SLIG28-TOT-PER-DEVL-N-REAP     EQUAL 0    AND
               SLIG28-TOT-PER-DEVL-S-REAP     EQUAL 0    AND
               SLIG28-TOT-FUT-DEVL-N-REAP     EQUAL 0    AND
               SLIG28-TOT-FUT-DEVL-S-REAP     EQUAL 0))

               ADD SLIG28-VL-PRINC-CHEQ TO ACU-CHEQ-REAP

               IF WRK-FLAG-ABERTOS EQUAL 'S'
                  SUBTRACT SLIG28-VL-PRINC-CHEQ FROM
                           ACU-VCTO-APOS-PER-NAO-PG
               END-IF
           END-IF.

      *---------------------------------------------------------------*
       3210-99-FIM.                  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3211-REAPRESENTADO-PER-ANT       SECTION.
      *---------------------------------------------------------------*

      *-- CONTABILIZA CHEQUES REAPRESENTADOS NO PERIODO ANTERIOR -----*

      *--- CONCEITO:                                                  *
      *--- CHEQUES COM DATA DEPOSITO NO PERIODO ANTERIOR E            *
      *--- REAPRESENTADOS                                             *

           IF (SLIG28-TOT-ANT-REAP-S-RESP NOT EQUAL 0 OR
               SLIG28-TOT-ANT-REAP-N-RESP NOT EQUAL 0) AND
              (SLIG28-TOT-FUT-DEVL-S-RESP NOT EQUAL 0)
              ADD SLIG28-VL-PRINC-CHEQ TO ACU-VCTO-APOS-PER-BXADO
                                          ACU-CHEQ-ANT-DEVOL
           END-IF.

      *---------------------------------------------------------------*
       3211-99-FIM.                  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3300-GRAVAR-CONSCHEA            SECTION.
      *---------------------------------------------------------------*

           MOVE ACU-VCTO-PER-PG        TO SLIG43-VCTO-PER-PG.
           MOVE ACU-VCTO-PER-NAO-PG    TO SLIG43-VCTO-PER-NAO-PG.
           MOVE ACU-VCTO-PER-BXADO     TO SLIG43-VCTO-PER-BXADO.
           MOVE ACU-VCTO-APOS-PER-PG   TO SLIG43-VCTO-APOS-PER-PG.
           MOVE ACU-VCTO-APOS-PER-NAO-PG
                                       TO SLIG43-VCTO-APOS-PER-NAO-PG.
           MOVE ACU-VCTO-APOS-PER-BXADO
                                       TO SLIG43-VCTO-APOS-PER-BXADO.
           MOVE ACU-CHEQ-PRIM-DEVLC    TO SLIG43-CHEQ-PRIM-DEVLC.
           MOVE ACU-CHEQ-REAP          TO SLIG43-CHEQ-REAP.
           MOVE ACU-CHEQ-REAP-DEVOL    TO SLIG43-CHEQ-REAP-DEVOL.
           MOVE ACU-CHEQ-REAP-PG       TO SLIG43-CHEQ-REAP-PG.
           MOVE ACU-CHEQ-ANT-DEVOL     TO SLIG43-CHEQ-DEVOL-ANTER.
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           WRITE REG-CHEQUE-CONSOLIDADO-EMITT.

           PERFORM 1130-TESTAR-FS-CONSCHEA.

           ADD 1                       TO ACU-GRAVA-CONSCHEA.

           MOVE ZEROS                  TO ACU-VCTO-PER-PG
                                          ACU-VCTO-PER-NAO-PG
                                          ACU-VCTO-PER-BXADO
                                          ACU-VCTO-APOS-PER-PG
                                          ACU-VCTO-APOS-PER-NAO-PG
                                          ACU-VCTO-APOS-PER-BXADO
                                          ACU-CHEQ-PRIM-DEVLC
                                          ACU-CHEQ-REAP
                                          ACU-CHEQ-REAP-DEVOL
                                          ACU-CHEQ-REAP-PG
                                          ACU-CHEQ-ANT-DEVOL.

      *---------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *---------------------------------------------------------------*

           PERFORM 4100-IMPRIMIR-SRELCTRL.

           CLOSE EMICHESR
                 CONSCHEA
                 SRELCTRL.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

           GOBACK.

      *---------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       4100-IMPRIMIR-SRELCTRL          SECTION.
      *---------------------------------------------------------------*

           MOVE ACU-LIDOS-EMICHESR     TO LD1-LIDOS-EMICHESR.
           MOVE ACU-GRAVA-CONSCHEA     TO LD2-GRAVA-CONSCHEA.
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           WRITE FD-REG-SRELCTRL       FROM CABEC1.
           PERFORM 1140-TESTAR-FS-SRELCTRL.

           WRITE FD-REG-SRELCTRL       FROM CABEC2.
           PERFORM 1140-TESTAR-FS-SRELCTRL.

           WRITE FD-REG-SRELCTRL       FROM CABEC3.
           PERFORM 1140-TESTAR-FS-SRELCTRL.

           WRITE FD-REG-SRELCTRL       FROM CABEC4.
           PERFORM 1140-TESTAR-FS-SRELCTRL.

           WRITE FD-REG-SRELCTRL       FROM LINDET1.
           PERFORM 1140-TESTAR-FS-SRELCTRL.

           WRITE FD-REG-SRELCTRL       FROM LINDET2.
           PERFORM 1140-TESTAR-FS-SRELCTRL.

      *---------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *---------------------------------------------------------------*

           MOVE 'APL'                  TO ERR-TIPO-ACESSO.
           MOVE 'SLIG6224'             TO ERR-PGM.

           CALL 'POOL7100'             USING WRK-BATCH
                                             ERRO-AREA.

           GOBACK.

      *---------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *---------------------------------------------------------------*
