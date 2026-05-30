      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT2756.
       AUTHOR.     HELIO SANTONI.
      *================================================================*
      *    S O N D A   P R O C W O R K  -  C O N S U L T O R I A       *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT2756                                    *
      *    PROGRAMADOR.:   HELIO SANTONI           - PROCWORK/ GP. 50  *
      *    ANALISTA....:   RODRIGO RIOS            - PROCWORK/ GP. 50  *
      *    DATA........:   29/09/2008                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:                                               *
      *             TRATAMENTO PARA RETIRAR DADOS PESSOAS JURIDICAS    *
      *             DO EXTRATO ANUAL DE TARIFAS.                       *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                       INCLUDE/BOOK      *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                      INCLUDE/BOOK        *
      *                  EXTRAANO                    GFCTWADR          *
      *                  CLIEJURI                    I#CHEQ04          *
      *                  ANOFISIC                    GFCTWADR          *
      *                  ANOJURID                    GFCTWADR          *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#BRAD7C - AREA PARA TRATAMENTO DE ERRO PELA BRAD7100.      *
      *    GFCTWADR - IMAGEM DA TAB. GFCTB0K5.                         *
      *    I#CHEQ04 - IMAGEM DA TAB. DE CLIENTES.                      *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    BRAD7100 - MODULO PARA CANCELAMENTO DO PROGRAMA.            *
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

           SELECT EXTRAANO  ASSIGN     TO UT-S-EXTRAANO
           FILE STATUS                 IS WRK-FS-EXTRAANO.

           SELECT CLIEJURI  ASSIGN     TO UT-S-CLIEJURI
           FILE STATUS                 IS WRK-FS-CLIEJURI.

           SELECT ANOFISIC  ASSIGN     TO UT-S-ANOFISIC
           FILE STATUS                 IS WRK-FS-ANOFISIC.

           SELECT ANOJURID  ASSIGN     TO UT-S-ANOJURID
           FILE STATUS                 IS WRK-FS-ANOJURID.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:  ARQ. BASE DE DADOS GFCTB0K5                         *
      *            ORG. SEQUENCIAL     -   LRECL   =  091              *
      *----------------------------------------------------------------*

       FD  EXTRAANO
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-EXTRAANO             PIC X(091).

      *----------------------------------------------------------------*
      *    INPUT:  ARQ. COM TODOS CLIENTES JURIDICOS.                  *
      *            ORG. SEQUENCIAL     -   LRECL   =  025              *
      *----------------------------------------------------------------*

       FD  CLIEJURI
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#CHEQ04'.

      *----------------------------------------------------------------*
      *    OUTPUT: ARQ. NAO ENCONTRADOS NO CLIEJURI - PESSOAS FISICA.  *
      *            ORG. SEQUENCIAL     -   LRECL   =  091              *
      *----------------------------------------------------------------*

       FD  ANOFISIC
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-ANOFISIC             PIC X(091).

      *----------------------------------------------------------------*
      *    OUTPUT: ARQ. ENCONTRADOS NO CLIEJURI - PESSOAS JURIDICOS.   *
      *            ORG. SEQUENCIAL     -   LRECL   =  091              *
      *----------------------------------------------------------------*

       FD  ANOJURID
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-ANOJURID             PIC X(091).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           'INICIO DA AREA DE WORKING'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           'AREA PARA ACUMULADORES'.
      *----------------------------------------------------------------*

       77  ACU-LIDOS-EXTRAANO          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-LIDOS-CLIEJURI          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-ANOFISIC          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-ANOJURID          PIC  9(009) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           'AREA PARA TESTES DE FILE-STATUS'.
      *----------------------------------------------------------------*

       77  WRK-FS-EXTRAANO             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-CLIEJURI             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-ANOFISIC             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-ANOJURID             PIC  X(002)         VALUE SPACES.

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
           'AREA PARA AUXILIARES'.
      *----------------------------------------------------------------*

       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.
       77  WRK-MASCARA                 PIC  ZZZ.ZZZ.ZZ9    VALUE ZEROS.

       01  WRK-AUX-5-S                 PIC +9(005)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-AUX-5-S.
           05 FILLER                   PIC  X(001).
           05 WRK-AUX-5                PIC  9(005).

       01  WRK-AUX-13-S                PIC +9(013)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-AUX-13-S.
           05 FILLER                   PIC  X(007).
           05 WRK-AUX-7                PIC  9(007).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA PARA CHAVES'.
      *----------------------------------------------------------------*

       01  WRK-CHAVE-EXTRAANO.
           05 WRK-EXTRAANO-AGENC       PIC  9(005)         VALUE ZEROS.
           05 WRK-EXTRAANO-CONTA       PIC  9(007)         VALUE ZEROS.

       01  WRK-CHAVE-CLIEJURI.
           05 WRK-CLIEJURI-AGENC       PIC  9(005)         VALUE ZEROS.
           05 WRK-CLIEJURI-CONTA       PIC  9(007)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'MENSAGEM DE ERRO DE FILE-STATUS'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-BRAD7100.
           05  FILLER                  PIC  X(009)         VALUE
               '*** ERRO '.
           05  WRK-OPERACAO            PIC  X(013)         VALUE SPACES.
           05  FILLER                  PIC  X(012)         VALUE
               ' DO ARQUIVO '.
           05  WRK-NOME-ARQ            PIC  X(008)         VALUE SPACES.
           05  FILLER                  PIC  X(017)         VALUE
               ' - FILE-STATUS = '.
           05  WRK-FILE-STATUS         PIC  X(002)         VALUE SPACES.
           05  FILLER                  PIC  X(004)         VALUE ' ***'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA PARA BRAD7100'.
      *----------------------------------------------------------------*

       COPY I#BRAD7C.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA DOS ARQ. UTILIZADOS NO PGM.'.
      *----------------------------------------------------------------*

       COPY 'GFCTWADR'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'FIM DA AREA DE WORKING'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      ******************************************************************
      * ROTINA INICIAR.                                                *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-VERIFICAR-VAZIO.

           PERFORM 3000-PROCESSAR      UNTIL
               WRK-CHAVE-EXTRAANO      EQUAL HIGH-VALUES  AND
               WRK-CHAVE-CLIEJURI      EQUAL HIGH-VALUES.

           PERFORM 4000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA INICIALIZAR.                                            *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT  EXTRAANO
                       CLIEJURI
                OUTPUT ANOFISIC
                       ANOJURID.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA TESTAR FILE-STATUS.                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-EXTRAANO

           PERFORM 1120-TESTAR-FS-CLIEJURI

           PERFORM 1130-TESTAR-FS-ANOFISIC

           PERFORM 1140-TESTAR-FS-ANOJURID.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA TESTAR FILE-STATUS DO ARQ. EXTRAANO.                    *
      ******************************************************************
      *----------------------------------------------------------------*
       1110-TESTAR-FS-EXTRAANO         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-EXTRAANO         NOT EQUAL '00'
               MOVE 'EXTRAANO'         TO WRK-NOME-ARQ
               MOVE WRK-FS-EXTRAANO    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-BRAD7100  TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO

               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA TESTAR FILE-STATUS DO ARQ. CLIEJURI.                    *
      ******************************************************************
      *----------------------------------------------------------------*
       1120-TESTAR-FS-CLIEJURI         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-CLIEJURI         NOT EQUAL '00'
               MOVE 'CLIEJURI'         TO WRK-NOME-ARQ
               MOVE WRK-FS-CLIEJURI    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-BRAD7100  TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO

               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA TESTAR FILE-STATUS DO ARQ. ANOFISIC.                    *
      ******************************************************************
      *----------------------------------------------------------------*
       1130-TESTAR-FS-ANOFISIC         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ANOFISIC         NOT EQUAL '00'
               MOVE 'ANOFISIC'         TO WRK-NOME-ARQ
               MOVE WRK-FS-ANOFISIC    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-BRAD7100  TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO

               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA TESTAR FILE-STATUS DO ARQ. ANOJURID.                    *
      ******************************************************************
      *----------------------------------------------------------------*
       1140-TESTAR-FS-ANOJURID         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ANOJURID         NOT EQUAL '00'
               MOVE 'ANOJURID'         TO WRK-NOME-ARQ
               MOVE WRK-FS-ANOJURID    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-BRAD7100  TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO

               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1140-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA VERIFICAR VAZIO.                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-LER-EXTRAANO.

           IF  WRK-CHAVE-EXTRAANO      EQUAL HIGH-VALUES
               DISPLAY '***************** GFCT2756 ******************'
               DISPLAY '*                                           *'
               DISPLAY '*    ARQUIVO DE IMAGEM DA TABELA GFCTB0K5   *'
               DISPLAY '*                 V A Z I O                 *'
               DISPLAY '*                                           *'
               DISPLAY '*          PROCESSAMENTO ENCERRADO          *'
               DISPLAY '*                                           *'
               DISPLAY '***************** GFCT2756 ******************'
               PERFORM 4000-FINALIZAR
           END-IF.

           PERFORM 2200-LER-CLIEJURI.

           IF  WRK-CHAVE-CLIEJURI      EQUAL HIGH-VALUES
               DISPLAY '***************** GFCT2756 ******************'
               DISPLAY '*                                           *'
               DISPLAY '*  ARQUIVO DE CADASTRO DE CHEQUES ENCONTRA  *'
               DISPLAY '*                 V A Z I O                 *'
               DISPLAY '*                                           *'
               DISPLAY '***************** GFCT2756 ******************'
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA LER ARQUIVO DE EXTRAANO                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-LER-EXTRAANO               SECTION.
      *----------------------------------------------------------------*

           READ EXTRAANO               INTO GFCTWADR-REG.

           IF  WRK-FS-EXTRAANO         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHAVE-EXTRAANO
               GO                      TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO

           PERFORM 1110-TESTAR-FS-EXTRAANO

           MOVE WADR-CAG-BCRIA         TO WRK-AUX-5-S
           MOVE WRK-AUX-5              TO WRK-EXTRAANO-AGENC
           MOVE WADR-CCTA-BCRIA-CLI    TO WRK-AUX-13-S
           MOVE WRK-AUX-7              TO WRK-EXTRAANO-CONTA

           ADD 1                       TO ACU-LIDOS-EXTRAANO.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA LER ARQUIVO CLIEJURI
      ******************************************************************
      *----------------------------------------------------------------*
       2200-LER-CLIEJURI               SECTION.
      *----------------------------------------------------------------*

           READ CLIEJURI.

           IF  WRK-FS-CLIEJURI         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHAVE-CLIEJURI
               GO                      TO 2200-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO

           PERFORM 1120-TESTAR-FS-CLIEJURI

           MOVE CHEQ-AGE               TO WRK-CLIEJURI-AGENC
           MOVE CHEQ-CTA               TO WRK-CLIEJURI-CONTA

           ADD 1                       TO ACU-LIDOS-CLIEJURI.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PROCESSAMENTO PRINCIPAL.                                *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           IF  WRK-CHAVE-EXTRAANO      EQUAL WRK-CHAVE-CLIEJURI
               PERFORM 3200-GRAVAR-ANOJURID  UNTIL
               WRK-CHAVE-EXTRAANO  NOT EQUAL WRK-CHAVE-CLIEJURI  OR
               WRK-CHAVE-EXTRAANO      EQUAL HIGH-VALUES
               PERFORM 2200-LER-CLIEJURI
           ELSE
               IF  WRK-CHAVE-EXTRAANO  LESS WRK-CHAVE-CLIEJURI
                   PERFORM 3100-GRAVAR-ANOFISIC
                   PERFORM 2100-LER-EXTRAANO
               ELSE
                   PERFORM 2200-LER-CLIEJURI
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA GRAVAR REG. NAO ENCONTRADOS NO CLIEJURI            *
      ******************************************************************
      *----------------------------------------------------------------*
       3100-GRAVAR-ANOFISIC            SECTION.
      *----------------------------------------------------------------*

           INITIALIZE FD-REG-ANOFISIC.

           WRITE FD-REG-ANOFISIC       FROM GFCTWADR-REG

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO

           PERFORM 1130-TESTAR-FS-ANOFISIC

           ADD 1                       TO ACU-GRAVA-ANOFISIC.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA GRAVAR REG. ENCONTRADOS NO CLIEJURI                *
      ******************************************************************
      *----------------------------------------------------------------*
       3200-GRAVAR-ANOJURID            SECTION.
      *----------------------------------------------------------------*

           INITIALIZE FD-REG-ANOJURID.

           WRITE FD-REG-ANOJURID       FROM GFCTWADR-REG

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO

           PERFORM 1140-TESTAR-FS-ANOJURID

           ADD 1                       TO ACU-GRAVA-ANOJURID
           PERFORM 2100-LER-EXTRAANO.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA FINALIZAR PROCESSAMENTO.                                *
      ******************************************************************
      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 4100-EMITIR-DISPLAY

           CLOSE EXTRAANO
                 CLIEJURI
                 ANOFISIC
                 ANOJURID.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO

           PERFORM 1100-TESTAR-FILE-STATUS

           STOP RUN.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA EMITIR DISPLAY.                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       4100-EMITIR-DISPLAY             SECTION.
      *----------------------------------------------------------------*

           IF  ACU-GRAVA-ANOFISIC      EQUAL ZEROS
               DISPLAY '***************************************'
               DISPLAY '*         PROGRAMA GFCT2756           *'
               DISPLAY '*                                     *'
               DISPLAY '*   ARQUIVO ANOFISIC SEM MOVIMENTO    *'
               DISPLAY '*                                     *'
               DISPLAY '*      PROCESSAMENTO ENCERRADO        *'
               DISPLAY '***************************************'
           END-IF.

           IF  ACU-GRAVA-ANOJURID      EQUAL ZEROS
               DISPLAY '***************************************'
               DISPLAY '*         PROGRAMA GFCT2756           *'
               DISPLAY '*                                     *'
               DISPLAY '*   ARQUIVO ANOJURID SEM MOVIMENTO    *'
               DISPLAY '*                                     *'
               DISPLAY '*      PROCESSAMENTO ENCERRADO        *'
               DISPLAY '***************************************'
           END-IF.

           DISPLAY '**************************************************'
                   '******'.
           DISPLAY '*               PROGRAMA GFCT2756                 '
                   '     *'.
           DISPLAY '*-------------------------------------------------'
                   '-----*'.
           DISPLAY '*                                                 '
                   '      '.
           DISPLAY '*       PROCESSAMENTO EFETUADO COM SUCESSO        '
                   '     *'.
           DISPLAY '*                                                 '
                   '     *'.
           DISPLAY '*-------------------------------------------------'
                   '-----*'.
           DISPLAY '*                                                 '
                   '     *'.

           MOVE ACU-LIDOS-EXTRAANO     TO WRK-MASCARA.

           DISPLAY '* TOTAL DE LIDOS EXTRATO ANUAL...........: '
                                                   WRK-MASCARA ' *'.

           MOVE ACU-LIDOS-CLIEJURI     TO WRK-MASCARA.

           DISPLAY '* TOTAL DEL LIDOS CLIE...................: '
                                                   WRK-MASCARA ' *'.

           MOVE ACU-GRAVA-ANOFISIC     TO WRK-MASCARA.

           DISPLAY '* TOTAL DE GRAVADOS EXTRATO ANUAL - PF...: '
                                                   WRK-MASCARA ' *'.
           MOVE ACU-GRAVA-ANOJURID     TO WRK-MASCARA.

           DISPLAY '* TOTAL DE GRAVADOS EXTRATO ANUAL - PJ...: '
                                                   WRK-MASCARA ' *'.
           DISPLAY '*                                                 '
                   '     *'.
           DISPLAY '**************************************************'
                   '******'.

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PROCESSAR ERRO.                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT2756'             TO ERR-PGM.

           CALL 'BRAD7100'          USING WRK-BATCH
                                          ERRO-AREA

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
