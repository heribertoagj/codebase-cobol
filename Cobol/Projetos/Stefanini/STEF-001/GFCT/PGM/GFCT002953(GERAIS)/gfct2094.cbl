      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT2094.
       AUTHOR.     HELIO SANTONI.
      *================================================================*
      *                    C P M - S I S T E M A S                     *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT2094                                    *
      *    PROGRAMADOR.:   HELIO SANTONI           - PROCWORK          *
      *    ANALISTA....:   WAGNER SILVA            - PROCKWORK/ GP. 50 *
      *    DATA........:   18/03/2008                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:                                               *
      *             OBTEM SEGMENTO DO CLIENTE                          *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                       INCLUDE/BOOK      *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                      INCLUDE/BOOK        *
      *                  CADNFUNC                    I#CLIE20          *
      *                  CADSEGMT                    I#CLIETO          *
      *                  ARQNOVOS                    I#GFCTD0          *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    POL7100C - AREA PARA TRATAMENTO DE ERRO PELA POOL7100.      *
      *    I#CLIE20 - ARQ. CADASTRO DE CLIENTES                        *
      *    I#CLIETO - ARQ. COM CADASTRO DE SEGMENTOS                   *
      *    I#GFCTD0 - LAYOUT DO ARQUIVO DE SEGMENTO PARA CONTAS NOVAS  *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    POOL7100 - MODULO PARA CANCELAMENTO DO PROGRAMA.            *
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

           SELECT CADNFUNC  ASSIGN     TO UT-S-CADNFUNC
           FILE STATUS                 IS WRK-FS-CADNFUNC.

           SELECT CADSEGMT  ASSIGN     TO UT-S-CADSEGMT
           FILE STATUS                 IS WRK-FS-CADSEGMT.

           SELECT ARQNOVOS  ASSIGN     TO UT-S-ARQNOVOS
           FILE STATUS                 IS WRK-FS-ARQNOVOS.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT : CADASTRO SEM FUNCIONARIOS                           *
      *            ORG. SEQUENCIAL     -   LRECL   =  180              *
      *----------------------------------------------------------------*

       FD  CADNFUNC
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY  'I#CLIE20'.

      *----------------------------------------------------------------*
      *    INPUT:  CADASTRO DE FUNCIONARIOS                            *
      *            ORG. SEQUENCIAL     -   LRECL   =  009              *
      *----------------------------------------------------------------*

       FD  CADSEGMT
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY  'I#CLIETO'.

      *----------------------------------------------------------------*
      *    OUTPUT: CADASTRO SEM FUNCIONARIOS                           *
      *            ORG. SEQUENCIAL     -   LRECL   =  180              *
      *----------------------------------------------------------------*

       FD  ARQNOVOS
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY  'I#GFCTD0'.

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

       77  ACU-LIDOS-CADNFUNC          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-LIDOS-CADSEGMT          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-ARQNOVOS          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-SEMSEG            PIC  9(009) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           'AREA PARA TESTES DE FILE-STATUS'.
      *----------------------------------------------------------------*

       77  WRK-FS-CADNFUNC             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-CADSEGMT             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-ARQNOVOS             PIC  X(002)         VALUE SPACES.

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

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA PARA CHAVES'.
      *----------------------------------------------------------------*

       01  WRK-CHAVE-CADNFUNC.
           05 WRK-CADNFUNC-AGENC       PIC  9(005)         VALUE ZEROS.
           05 WRK-CADNFUNC-CONTA       PIC  9(007)         VALUE ZEROS.

       01  WRK-CHAVE-CADSEGMT.
           05 WRK-CADSEGMT-AGENC       PIC  9(005)         VALUE ZEROS.
           05 WRK-CADSEGMT-CONTA       PIC  9(007)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'MENSAGEM DE ERRO DE FILE-STATUS'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-POOL7100.
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
           'AREA PARA POOL7100'.
      *----------------------------------------------------------------*

       COPY POL7100C.

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
               WRK-CHAVE-CADNFUNC      EQUAL HIGH-VALUES.

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

           CALL 'POOL1050'.

           OPEN INPUT  CADNFUNC
                       CADSEGMT
                OUTPUT ARQNOVOS.

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

           PERFORM 1110-TESTAR-FS-CADNFUNC

           PERFORM 1120-TESTAR-FS-CADSEGMT

           PERFORM 1130-TESTAR-FS-ARQNOVOS.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA TESTAR FILE-STATUS DO ARQ. CADNFUNC.                    *
      ******************************************************************
      *----------------------------------------------------------------*
       1110-TESTAR-FS-CADNFUNC        SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-CADNFUNC         NOT EQUAL '00'
               MOVE 'CADNFUNC'         TO WRK-NOME-ARQ
               MOVE WRK-FS-CADNFUNC    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-POOL7100  TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO

               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA TESTAR FILE-STATUS DO ARQ. CADSEGMT.                    *
      ******************************************************************
      *----------------------------------------------------------------*
       1120-TESTAR-FS-CADSEGMT         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-CADSEGMT         NOT EQUAL '00'
               MOVE 'CADSEGMT'         TO WRK-NOME-ARQ
               MOVE WRK-FS-CADSEGMT    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-POOL7100  TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO

               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA TESTAR FILE-STATUS DO ARQ. ARQNOVOS.                    *
      ******************************************************************
      *----------------------------------------------------------------*
       1130-TESTAR-FS-ARQNOVOS         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ARQNOVOS         NOT EQUAL '00'
               MOVE 'ARQNOVOS'         TO WRK-NOME-ARQ
               MOVE WRK-FS-ARQNOVOS    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-POOL7100  TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO

               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA VERIFICAR VAZIO.                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-LER-CADNFUNC.

           IF  WRK-CHAVE-CADNFUNC      EQUAL HIGH-VALUES
               DISPLAY '***************** GFCT2094 *****************'
               DISPLAY '*                                          *'
               DISPLAY '*    ARQUIVO CADNFUNC ENCONTRA-SE  VAZIO   *'
               DISPLAY '*                                          *'
               DISPLAY '*          PROCESSAMENTO ENCERRADO         *'
               DISPLAY '*                                          *'
               DISPLAY '***************** GFCT2094 *****************'
           END-IF.

           PERFORM 2200-LER-CADSEGMT.

           IF  WRK-CHAVE-CADSEGMT      EQUAL HIGH-VALUES
               DISPLAY '***************** GFCT2094 *****************'
               DISPLAY '*                                          *'
               DISPLAY '*    ARQUIVO CADSEGMT ENCONTRA-SE  VAZIO   *'
               DISPLAY '*                                          *'
               DISPLAY '***************** GFCT2094 *****************'
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA LER ARQUIVO DE CADNFUNC                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-LER-CADNFUNC               SECTION.
      *----------------------------------------------------------------*

           READ CADNFUNC

           IF  WRK-FS-CADNFUNC         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHAVE-CADNFUNC
               GO                      TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.
           PERFORM 1110-TESTAR-FS-CADNFUNC.

           MOVE CAD-AGENCIA            TO WRK-CADNFUNC-AGENC
           MOVE CAD-CONTA              TO WRK-CADNFUNC-CONTA
           ADD 1                       TO ACU-LIDOS-CADNFUNC.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA LER ARQUIVO CADSEGMT
      ******************************************************************
      *----------------------------------------------------------------*
       2200-LER-CADSEGMT               SECTION.
      *----------------------------------------------------------------*

           READ CADSEGMT.

           IF  WRK-FS-CADSEGMT         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHAVE-CADSEGMT
               GO                      TO 2200-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.
           PERFORM 1120-TESTAR-FS-CADSEGMT.

           MOVE SEG-AGENCIA            TO WRK-CADSEGMT-AGENC
           MOVE SEG-CONTA              TO WRK-CADSEGMT-CONTA
           ADD 1                       TO ACU-LIDOS-CADSEGMT.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PROCESSAMENTO PRINCIPAL.                                *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           IF  WRK-CHAVE-CADNFUNC      EQUAL WRK-CHAVE-CADSEGMT
               MOVE  REG-CADACLIE      TO D0-CADACLIE
               MOVE  SEG-TP-SEGMENTO   TO D0-SEGMENTO
               MOVE  ZEROS             TO D0-TARIFA
               ADD 1                   TO ACU-GRAVA-ARQNOVOS
               PERFORM 3100-GRAVAR-ARQNOVOS
               PERFORM 2100-LER-CADNFUNC
               PERFORM 2200-LER-CADSEGMT
           ELSE
               IF  WRK-CHAVE-CADNFUNC  LESS WRK-CHAVE-CADSEGMT
                   MOVE  REG-CADACLIE  TO D0-CADACLIE
                   MOVE  ZEROS         TO D0-SEGMENTO
                                          D0-TARIFA
                   ADD 1               TO ACU-GRAVA-SEMSEG
                   PERFORM 3100-GRAVAR-ARQNOVOS
                   PERFORM 2100-LER-CADNFUNC
               ELSE
                   PERFORM 2200-LER-CADSEGMT
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA GRAVAR REG. COM SEGMENTO                           *
      ******************************************************************
      *----------------------------------------------------------------*
       3100-GRAVAR-ARQNOVOS            SECTION.
      *----------------------------------------------------------------*

           WRITE D0-REGISTRO

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO
           PERFORM 1130-TESTAR-FS-ARQNOVOS.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA FINALIZAR PROCESSAMENTO.                                *
      ******************************************************************
      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 4100-EMITIR-DISPLAY.

           CLOSE CADNFUNC
                 CADSEGMT
                 ARQNOVOS.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

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

           IF  ACU-GRAVA-ARQNOVOS      EQUAL ZEROS
               DISPLAY '***************************************'
               DISPLAY '*         PROGRAMA GFCT2094           *'
               DISPLAY '*                                     *'
               DISPLAY '*   ARQUIVO CADNFUNC SEM MOVIMENTO    *'
               DISPLAY '*                                     *'
               DISPLAY '*      PROCESSAMENTO ENCERRADO        *'
               DISPLAY '***************************************'
           END-IF.

           DISPLAY '**************************************************'
                   '*'.
           DISPLAY '*               PROGRAMA GFCT2094                 '
                   '*'.
           DISPLAY '*-------------------------------------------------'
                   '*'.
           DISPLAY '*                                                 '
                   '*'.
           DISPLAY '*       PROCESSAMENTO EFETUADO COM SUCESSO        '
                   '*'.
           DISPLAY '*                                                 '
                   '*'.
           DISPLAY '*-------------------------------------------------'
                   '*'.
           DISPLAY '*                                                 '
                   '*'.

           MOVE ACU-LIDOS-CADNFUNC     TO WRK-MASCARA.

           DISPLAY '* QTDE REGISTROS LIDOS    (CADNFUNC)= '
                                                   WRK-MASCARA ' *'.

           MOVE ACU-LIDOS-CADSEGMT     TO WRK-MASCARA.

           DISPLAY '* QTDE REGISTROS LIDOS    (CADSEGMT)= '
                                                   WRK-MASCARA ' *'.

           MOVE ACU-GRAVA-ARQNOVOS     TO WRK-MASCARA.

           DISPLAY '* QTDE REG. ENCONTRADOS   (ARQNOVOS)= '
                                                   WRK-MASCARA ' *'.
           MOVE ACU-GRAVA-SEMSEG       TO WRK-MASCARA.

           DISPLAY '* QTDE REG. SEGMTO ZERADO (ARQNOVOS)= '
                                                   WRK-MASCARA ' *'.
           DISPLAY '*                                                 '
                   '*'.
           DISPLAY '**************************************************'
                   '*'.

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PROCESSAR ERRO.                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT2094'             TO ERR-PGM.

           CALL 'POOL7100'          USING WRK-BATCH
                                          ERRO-AREA

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

