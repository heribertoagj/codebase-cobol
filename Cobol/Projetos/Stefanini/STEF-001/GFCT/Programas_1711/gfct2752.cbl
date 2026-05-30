      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT2752.
       AUTHOR.     HELIO SANTONI.
      *================================================================*
      *    S O N D A   P R O C W O R K  -  C O N S U L T O R I A       *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT2752                                    *
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
      *                  EXTRAMES                    GFCTWADN          *
      *                  CLIEJURI                    I#CLIE04          *
      *                  MESFISIC                    GFCTWADN          *
      *                  MESJURID                    GFCTWADN          *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#BRAD7C - AREA PARA TRATAMENTO DE ERRO PELA BRAD7100.      *
      *    GFCTWADN - IMAGEM DA TAB. GFCTB0K6.                         *
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

           SELECT EXTRAMES  ASSIGN     TO UT-S-EXTRAMES
           FILE STATUS                 IS WRK-FS-EXTRAMES.

           SELECT CLIEJURI  ASSIGN     TO UT-S-CLIEJURI
           FILE STATUS                 IS WRK-FS-CLIEJURI.

           SELECT MESFISIC  ASSIGN     TO UT-S-MESFISIC
           FILE STATUS                 IS WRK-FS-MESFISIC.

           SELECT MESJURID  ASSIGN     TO UT-S-MESJURID
           FILE STATUS                 IS WRK-FS-MESJURID.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:  ARQ. BASE DE DADOS GFCTB0K6                         *
      *            ORG. SEQUENCIAL     -   LRECL   =  093              *
      *----------------------------------------------------------------*

       FD  EXTRAMES
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-EXTRAMES             PIC X(093).

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
      *            ORG. SEQUENCIAL     -   LRECL   =  093              *
      *----------------------------------------------------------------*

       FD  MESFISIC
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-MESFISIC             PIC X(093).

      *----------------------------------------------------------------*
      *    OUTPUT: ARQ. ENCONTRADOS NO CLIEJURI - PESSOAS JURIDICOS.   *
      *            ORG. SEQUENCIAL     -   LRECL   =  093              *
      *----------------------------------------------------------------*

       FD  MESJURID
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-MESJURID             PIC X(093).

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

       77  ACU-LIDOS-EXTRAMES          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-LIDOS-CLIEJURI          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-MESFISIC          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-MESJURID          PIC  9(009) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           'AREA PARA TESTES DE FILE-STATUS'.
      *----------------------------------------------------------------*

       77  WRK-FS-EXTRAMES             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-CLIEJURI             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-MESFISIC             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-MESJURID             PIC  X(002)         VALUE SPACES.

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

       01  WRK-CHAVE-EXTRAMES.
           05 WRK-EXTRAMES-AGENC       PIC  9(005)         VALUE ZEROS.
           05 WRK-EXTRAMES-CONTA       PIC  9(007)         VALUE ZEROS.

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

       COPY 'GFCTWADN'.

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
               WRK-CHAVE-EXTRAMES      EQUAL HIGH-VALUES  AND
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

           OPEN INPUT  EXTRAMES
                       CLIEJURI
                OUTPUT MESFISIC
                       MESJURID.

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

           PERFORM 1110-TESTAR-FS-EXTRAMES

           PERFORM 1120-TESTAR-FS-CLIEJURI

           PERFORM 1130-TESTAR-FS-MESFISIC

           PERFORM 1140-TESTAR-FS-MESJURID.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA TESTAR FILE-STATUS DO ARQ. EXTRAMES.                    *
      ******************************************************************
      *----------------------------------------------------------------*
       1110-TESTAR-FS-EXTRAMES        SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-EXTRAMES         NOT EQUAL '00'
               MOVE 'EXTRAMES'         TO WRK-NOME-ARQ
               MOVE WRK-FS-EXTRAMES    TO WRK-FILE-STATUS
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
      * ROTINA TESTAR FILE-STATUS DO ARQ. MESFISIC.                    *
      ******************************************************************
      *----------------------------------------------------------------*
       1130-TESTAR-FS-MESFISIC         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-MESFISIC         NOT EQUAL '00'
               MOVE 'MESFISIC'         TO WRK-NOME-ARQ
               MOVE WRK-FS-MESFISIC    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-BRAD7100  TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO

               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA TESTAR FILE-STATUS DO ARQ. MESJURID.                    *
      ******************************************************************
      *----------------------------------------------------------------*
       1140-TESTAR-FS-MESJURID         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-MESJURID         NOT EQUAL '00'
               MOVE 'MESJURID'         TO WRK-NOME-ARQ
               MOVE WRK-FS-MESJURID    TO WRK-FILE-STATUS
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

           PERFORM 2100-LER-EXTRAMES.

           IF  WRK-CHAVE-EXTRAMES      EQUAL HIGH-VALUES
               DISPLAY '***************** GFCT2752 ******************'
               DISPLAY '*                                           *'
               DISPLAY '*    ARQUIVO DE IMAGEM DA TABELA GFCTB0K6   *'
               DISPLAY '*                 V A Z I O                 *'
               DISPLAY '*                                           *'
               DISPLAY '*          PROCESSAMENTO ENCERRADO          *'
               DISPLAY '*                                           *'
               DISPLAY '***************** GFCT2752 ******************'
               PERFORM 4000-FINALIZAR
           END-IF.

           PERFORM 2200-LER-CLIEJURI.

           IF  WRK-CHAVE-CLIEJURI      EQUAL HIGH-VALUES
               DISPLAY '***************** GFCT2752 ******************'
               DISPLAY '*                                           *'
               DISPLAY '*  ARQUIVO DE CADASTRO DE CHEQUES ENCONTRA  *'
               DISPLAY '*                 V A Z I O                 *'
               DISPLAY '*                                           *'
               DISPLAY '***************** GFCT2752 ******************'
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA LER ARQUIVO DE EXTRAMES                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-LER-EXTRAMES               SECTION.
      *----------------------------------------------------------------*

           READ EXTRAMES               INTO GFCTWADN-REG.

           IF  WRK-FS-EXTRAMES         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHAVE-EXTRAMES
               GO                      TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO

           PERFORM 1110-TESTAR-FS-EXTRAMES

           MOVE WADN-CAG-BCRIA         TO WRK-AUX-5-S
           MOVE WRK-AUX-5              TO WRK-EXTRAMES-AGENC
           MOVE WADN-CCTA-BCRIA-CLI    TO WRK-AUX-13-S
           MOVE WRK-AUX-7              TO WRK-EXTRAMES-CONTA

           ADD 1                       TO ACU-LIDOS-EXTRAMES.

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

           IF  WRK-CHAVE-EXTRAMES      EQUAL WRK-CHAVE-CLIEJURI
               PERFORM 3200-GRAVAR-MESJURID  UNTIL
               WRK-CHAVE-EXTRAMES  NOT EQUAL WRK-CHAVE-CLIEJURI  OR
               WRK-CHAVE-EXTRAMES      EQUAL HIGH-VALUES
               PERFORM 2200-LER-CLIEJURI
           ELSE
               IF  WRK-CHAVE-EXTRAMES  LESS WRK-CHAVE-CLIEJURI
                   PERFORM 3100-GRAVAR-MESFISIC
                   PERFORM 2100-LER-EXTRAMES
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
       3100-GRAVAR-MESFISIC            SECTION.
      *----------------------------------------------------------------*

           INITIALIZE FD-REG-MESFISIC.

           WRITE FD-REG-MESFISIC       FROM GFCTWADN-REG

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO

           PERFORM 1130-TESTAR-FS-MESFISIC

           ADD 1                       TO ACU-GRAVA-MESFISIC.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA GRAVAR REG. ENCONTRADOS NO CLIEJURI                *
      ******************************************************************
      *----------------------------------------------------------------*
       3200-GRAVAR-MESJURID            SECTION.
      *----------------------------------------------------------------*

           INITIALIZE FD-REG-MESJURID.

           WRITE FD-REG-MESJURID       FROM GFCTWADN-REG

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO

           PERFORM 1140-TESTAR-FS-MESJURID

           ADD 1                       TO ACU-GRAVA-MESJURID
           PERFORM 2100-LER-EXTRAMES.

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

           CLOSE EXTRAMES
                 CLIEJURI
                 MESFISIC
                 MESJURID.

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

           IF  ACU-GRAVA-MESFISIC      EQUAL ZEROS
               DISPLAY '***************************************'
               DISPLAY '*         PROGRAMA GFCT2752           *'
               DISPLAY '*                                     *'
               DISPLAY '*   ARQUIVO MESFISIC SEM MOVIMENTO    *'
               DISPLAY '*                                     *'
               DISPLAY '*      PROCESSAMENTO ENCERRADO        *'
               DISPLAY '***************************************'
           END-IF.

           IF  ACU-GRAVA-MESJURID      EQUAL ZEROS
               DISPLAY '***************************************'
               DISPLAY '*         PROGRAMA GFCT2752           *'
               DISPLAY '*                                     *'
               DISPLAY '*   ARQUIVO MESJURID SEM MOVIMENTO    *'
               DISPLAY '*                                     *'
               DISPLAY '*      PROCESSAMENTO ENCERRADO        *'
               DISPLAY '***************************************'
           END-IF.

           DISPLAY '**************************************************'
                   '******'.
           DISPLAY '*               PROGRAMA GFCT2752                 '
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

           MOVE ACU-LIDOS-EXTRAMES     TO WRK-MASCARA.

           DISPLAY '* TOTAL DE LIDOS EXTRATO MENSAL..........: '
                                                   WRK-MASCARA ' *'.

           MOVE ACU-LIDOS-CLIEJURI     TO WRK-MASCARA.

           DISPLAY '* TOTAL DEL LIDOS CLIE...................: '
                                                   WRK-MASCARA ' *'.

           MOVE ACU-GRAVA-MESFISIC     TO WRK-MASCARA.

           DISPLAY '* TOTAL DE GRAVADOS EXTRATO MENSAL - PF..: '
                                                   WRK-MASCARA ' *'.
           MOVE ACU-GRAVA-MESJURID     TO WRK-MASCARA.

           DISPLAY '* TOTAL DE GRAVADOS EXTRATO MENSAL - PJ..: '
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

           MOVE 'GFCT2752'             TO ERR-PGM.

           CALL 'BRAD7100'          USING WRK-BATCH
                                          ERRO-AREA

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
