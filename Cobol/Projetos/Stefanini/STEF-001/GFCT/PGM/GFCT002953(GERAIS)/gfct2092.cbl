      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT2092.
       AUTHOR.     HELIO SANTONI.
      *================================================================*
      *                    C P M - S I S T E M A S                     *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT2092                                    *
      *    PROGRAMADOR.:   HELIO SANTONI           - PROCWORK          *
      *    ANALISTA....:   WAGNER SILVA            - PROCKWORK/ GP. 50 *
      *    DATA........:   18/03/2008                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:                                               *
      *             EXCLUIR CONTAS DE FUNCIONARIOS                     *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                       INCLUDE/BOOK      *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                      INCLUDE/BOOK        *
      *                  CADANOVO                    I#CLIE20          *
      *                  CADAFUNC                    --------          *
      *                  CADNFUNC                    I#CLIE20          *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    POL7100C - AREA PARA TRATAMENTO DE ERRO PELA POOL7100.      *
      *    I#CLIE20 - ARQ. COM EVENTOS DE GRUPO MISTO                  *
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

           SELECT CADANOVO  ASSIGN     TO UT-S-CADANOVO
           FILE STATUS                 IS WRK-FS-CADANOVO.

           SELECT CADAFUNC  ASSIGN     TO UT-S-CADAFUNC
           FILE STATUS                 IS WRK-FS-CADAFUNC.

           SELECT CADNFUNC  ASSIGN     TO UT-S-CADNFUNC
           FILE STATUS                 IS WRK-FS-CADNFUNC.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:  CADASTRO DE CLIENTES                                *
      *            ORG. SEQUENCIAL     -   LRECL   =  180              *
      *----------------------------------------------------------------*

       FD  CADANOVO
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  REG-CADANOVO                 PIC X(180).

      *----------------------------------------------------------------*
      *    INPUT:  CADASTRO DE FUNCIONARIOS                            *
      *            ORG. SEQUENCIAL     -   LRECL   =  009              *
      *----------------------------------------------------------------*

       FD  CADAFUNC
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  REGISTRO-FUNC.
           03 ARQ-FUNC-AGENCIA           PIC  9(05) COMP-3.
           03 ARQ-FUNC-CONTA             PIC  9(07) COMP-3.
           03 ARQ-FUNC-DIG-CONTA         PIC  X(01).
           03 ARQ-FUNC-COD               PIC  X(01).

      *----------------------------------------------------------------*
      *    OUTPUT: CADASTRO NAO CONTEM  FUNCIONARIOS                   *
      *            ORG. SEQUENCIAL     -   LRECL   =  180              *
      *----------------------------------------------------------------*

       FD  CADNFUNC
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  REG-CADNFUNC                 PIC X(180).

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

       77  ACU-LIDOS-CADANOVO          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-LIDOS-CADAFUNC          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-CADNFUNC          PIC  9(009) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           'AREA PARA TESTES DE FILE-STATUS'.
      *----------------------------------------------------------------*

       77  WRK-FS-CADANOVO             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-CADAFUNC             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-CADNFUNC             PIC  X(002)         VALUE SPACES.

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

       01  WRK-CHAVE-CADANOVO.
           05 WRK-CADANOVO-AGENC       PIC  9(005)         VALUE ZEROS.
           05 WRK-CADANOVO-CONTA       PIC  9(007)         VALUE ZEROS.

       01  WRK-CHAVE-CADAFUNC.
           05 WRK-CADAFUNC-AGENC       PIC  9(005)         VALUE ZEROS.
           05 WRK-CADAFUNC-CONTA       PIC  9(007)         VALUE ZEROS.

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
           'AREA PARA I#CLIE20'.
      *----------------------------------------------------------------*

       COPY 'I#CLIE20'.

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
               WRK-CHAVE-CADANOVO      EQUAL HIGH-VALUES.

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

           OPEN INPUT  CADANOVO
                       CADAFUNC
                OUTPUT CADNFUNC.

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

           PERFORM 1110-TESTAR-FS-CADANOVO

           PERFORM 1120-TESTAR-FS-CADAFUNC

           PERFORM 1130-TESTAR-FS-CADNFUNC.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA TESTAR FILE-STATUS DO ARQ. CADANOVO.                    *
      ******************************************************************
      *----------------------------------------------------------------*
       1110-TESTAR-FS-CADANOVO        SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-CADANOVO         NOT EQUAL '00'
               MOVE 'CADANOVO'         TO WRK-NOME-ARQ
               MOVE WRK-FS-CADANOVO    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-POOL7100  TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO

               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA TESTAR FILE-STATUS DO ARQ. CADAFUNC.                    *
      ******************************************************************
      *----------------------------------------------------------------*
       1120-TESTAR-FS-CADAFUNC         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-CADAFUNC         NOT EQUAL '00'
               MOVE 'CADAFUNC'         TO WRK-NOME-ARQ
               MOVE WRK-FS-CADAFUNC    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-POOL7100  TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO

               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA TESTAR FILE-STATUS DO ARQ. CADNFUNC.                    *
      ******************************************************************
      *----------------------------------------------------------------*
       1130-TESTAR-FS-CADNFUNC         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-CADNFUNC         NOT EQUAL '00'
               MOVE 'CADNFUNC'         TO WRK-NOME-ARQ
               MOVE WRK-FS-CADNFUNC    TO WRK-FILE-STATUS
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

           PERFORM 2100-LER-CADANOVO.

           IF  WRK-CHAVE-CADANOVO      EQUAL HIGH-VALUES
               DISPLAY '***************** GFCT2092 *****************'
               DISPLAY '*                                          *'
               DISPLAY '*    ARQUIVO CADANOVO ENCONTRA-SE  VAZIO   *'
               DISPLAY '*                                          *'
               DISPLAY '*          PROCESSAMENTO ENCERRADO         *'
               DISPLAY '*                                          *'
               DISPLAY '***************** GFCT2092 *****************'
           END-IF.

           PERFORM 2200-LER-CADAFUNC.

           IF  WRK-CHAVE-CADAFUNC      EQUAL HIGH-VALUES
               DISPLAY '***************** GFCT2092 *****************'
               DISPLAY '*                                          *'
               DISPLAY '*    ARQUIVO CADAFUNC ENCONTRA-SE  VAZIO   *'
               DISPLAY '*                                          *'
               DISPLAY '***************** GFCT2092 *****************'
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA LER ARQUIVO DE CADANOVO                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-LER-CADANOVO               SECTION.
      *----------------------------------------------------------------*

           READ CADANOVO   INTO   REG-CADACLIE.

           IF  WRK-FS-CADANOVO         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHAVE-CADANOVO
               GO TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1110-TESTAR-FS-CADANOVO.

           MOVE CAD-AGENCIA              TO WRK-CADANOVO-AGENC
           MOVE CAD-CONTA                TO WRK-CADANOVO-CONTA

           ADD 1                       TO ACU-LIDOS-CADANOVO.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA LER ARQUIVO CADAFUNC
      ******************************************************************
      *----------------------------------------------------------------*
       2200-LER-CADAFUNC               SECTION.
      *----------------------------------------------------------------*

           READ CADAFUNC.

           IF  WRK-FS-CADAFUNC         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHAVE-CADAFUNC
               GO TO 2200-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1120-TESTAR-FS-CADAFUNC.

           MOVE ARQ-FUNC-AGENCIA         TO WRK-CADAFUNC-AGENC
           MOVE ARQ-FUNC-CONTA           TO WRK-CADAFUNC-CONTA

           ADD 1                       TO ACU-LIDOS-CADAFUNC.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PROCESSAMENTO PRINCIPAL.                                *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           IF  WRK-CHAVE-CADANOVO      EQUAL WRK-CHAVE-CADAFUNC
               PERFORM 2100-LER-CADANOVO
           ELSE
               IF  WRK-CHAVE-CADANOVO  LESS WRK-CHAVE-CADAFUNC
                   PERFORM 3100-GRAVAR-CADNFUNC
                   PERFORM 2100-LER-CADANOVO
               ELSE
                   PERFORM 2200-LER-CADAFUNC
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA GRAVAR REG. NAO FUNCIONARIO.                       *
      ******************************************************************
      *----------------------------------------------------------------*
       3100-GRAVAR-CADNFUNC            SECTION.
      *----------------------------------------------------------------*

           INITIALIZE REG-CADNFUNC.

           WRITE REG-CADNFUNC            FROM REG-CADACLIE.

           MOVE WRK-GRAVACAO               TO WRK-OPERACAO

           PERFORM 1130-TESTAR-FS-CADNFUNC.

           ADD 1                           TO ACU-GRAVA-CADNFUNC.

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

           CLOSE CADANOVO
                 CADAFUNC
                 CADNFUNC.

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

           IF  ACU-GRAVA-CADNFUNC      EQUAL ZEROS
               DISPLAY '***************************************'
               DISPLAY '*         PROGRAMA GFCT2092           *'
               DISPLAY '*                                     *'
               DISPLAY '*   ARQUIVO CADNFUNC SEM MOVIMENTO    *'
               DISPLAY '*                                     *'
               DISPLAY '*      PROCESSAMENTO ENCERRADO        *'
               DISPLAY '***************************************'
           END-IF.

           DISPLAY '**************************************************'
                   '*'.
           DISPLAY '*               PROGRAMA GFCT2092                 '
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

           MOVE ACU-LIDOS-CADANOVO     TO WRK-MASCARA.

           DISPLAY '* QTDE REGISTROS LIDOS    (CADANOVO)= '
                                                   WRK-MASCARA ' *'.

           MOVE ACU-LIDOS-CADAFUNC     TO WRK-MASCARA.

           DISPLAY '* QTDE REGISTROS LIDOS    (CADAFUNC)= '
                                                   WRK-MASCARA ' *'.

           MOVE ACU-GRAVA-CADNFUNC     TO WRK-MASCARA.

           DISPLAY '* QTDE REGISTROS GRAVADOS (CADNFUNC)= '
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

           MOVE 'GFCT2092'             TO ERR-PGM.

           CALL 'POOL7100'          USING WRK-BATCH
                                          ERRO-AREA

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

