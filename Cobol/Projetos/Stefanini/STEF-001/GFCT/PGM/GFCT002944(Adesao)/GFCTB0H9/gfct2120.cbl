      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT2120.
       AUTHOR.     WILLIAN CARARO.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT2120                                    *
      *    PROGRAMADOR.:   WILLIAN CARARO          - CPM PATO BRANCO   *
      *    ANALISTA CPM:   FELIPE ALEX SOMMER      - CPM PATO BRANCO   *
      *    ANALISTA....:   RICARDO PEREIRA         - PROCWORK / GP.50  *
      *    DATA........:   29/05/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   OBTER DADOS DO MUNICIPIO DA AGENCIA.        *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                         INCLUDE/BOOK     *
      *                    CLIEADES                     I#GFCTRU       *
      *                    CLIEMUUF                     I#GFCTRY       *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    I#GFCTRU - AREA PARA ARQUIVO DE ENTRADA.                    *
      *    I#GFCTRY - AREA PARA ARQUIVO DE SAIDA.                      *
      *    I#GFCTRK - AREA PARA ACESSAR MODULO GFCT5564 - OBTER DADOS. *
      *    I#GFCT0M - AREA PARA ACESSAR MODULO GFCT5564 - ERROS.       *
      *    POL7100C - AREA PARA TRATAMENTO DE ERROS PELA POOL7100.     *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    GFCT5564 - MODULO PARA OBTER CODIGO DO MUNICIPIO E SIGLA DA *
      *               UF DA AGENCIA.                                   *
      *    POOL0081 - ALOCAR DINAMICAMENTE UM MODULO.                  *
      *    POOL7100 - TRATAMENTO DE ERROS PADRAO BRADESCO.             *
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

           SELECT CLIEADES ASSIGN      TO UT-S-CLIEADES
           FILE STATUS                 IS WRK-FS-CLIEADES.

           SELECT CLIEMUUF ASSIGN      TO UT-S-CLIEMUUF
           FILE STATUS                 IS WRK-FS-CLIEMUUF.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:  ARQUIVO DE ENTRADA  -   CLIEADES                    *
      *            ORG. SEQUENCIAL     -   LRECL = 040                 *
      *----------------------------------------------------------------*

       FD  CLIEADES
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTRU'.

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE SAIDA    -   CLIEMUUF                    *
      *            ORG. SEQUENCIAL     -   LRECL = 050                 *
      *----------------------------------------------------------------*

       FD  CLIEMUUF
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTRY'.

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* INICIO DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA DE ACUMULADORES *'.
      *----------------------------------------------------------------*

       77  ACU-LIDOS-CLIEADES          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-CLIEMUUF          PIC  9(009) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA DE TESTES DE FILE STATUS *'.
      *----------------------------------------------------------------*

       77  WRK-FS-CLIEADES             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-CLIEMUUF             PIC  X(002)         VALUE SPACES.

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
           '* AREA PARA VARIAVEIS AUXILIARES *'.
      *----------------------------------------------------------------*

       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.
       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-MASCARA                 PIC  ZZZ.ZZZ.ZZ9    VALUE ZEROS.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA MENSAGENS *'.
      *---------------------------------------------------------------*

       01  WRK-MENSAGENS.
           05  WRK-MSG001              PIC  X(075)         VALUE
               '** ARQUIVO CLIEADES ESTA VAZIO, PROGRAMA CANCELADO **'.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* MENSAGEM DE ERRO DE FILE-STATUS *'.
      *---------------------------------------------------------------*

       01  WRK-ERRO-TEXTO.
           05  FILLER                  PIC  X(008)         VALUE
               '** ERRO '.
           05  WRK-OPERACAO            PIC  X(013)         VALUE SPACES.
           05  FILLER                  PIC  X(012)         VALUE
               ' DO ARQUIVO '.
           05  WRK-NOME-ARQ            PIC  X(008)         VALUE SPACES.
           05  FILLER                  PIC  X(017)         VALUE
               ' - FILE STATUS = '.
           05  WRK-FILE-STATUS         PIC  X(002)         VALUE SPACES.
           05  FILLER                  PIC  X(003)         VALUE ' **'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5564 - DADOS *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTRK'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5564 - ERROS *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCT0M'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA TRATAMENTO DE ERRO *'.
      *----------------------------------------------------------------*

       COPY 'POL7100C'.

      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* FIM DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      ******************************************************************
      *    CHAMA AS ROTINAS A SEREM PROCESSADAS                        *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-VERIFICAR-VAZIO.

           PERFORM 3000-PROCESSAR      UNTIL
                   WRK-FS-CLIEADES     EQUAL '10'.

           PERFORM 4000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ABERTURA DOS ARQUIVOS, E ROTINA PARA TESTE DE ARQUIVOS      *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT  CLIEADES
                OUTPUT CLIEMUUF.

           MOVE  WRK-ABERTURA          TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTE DE FILE STATUS DOS ARQUIVOS                           *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-CLIEADES.

           PERFORM 1120-TESTAR-FS-CLIEMUUF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTE DE FILE STATUS DO ARQUIVO CLIEADES                    *
      ******************************************************************
      *----------------------------------------------------------------*
       1110-TESTAR-FS-CLIEADES         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-CLIEADES         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-CLIEADES    TO WRK-FILE-STATUS
               MOVE 'CLIEADES'         TO WRK-NOME-ARQ
               MOVE WRK-ERRO-TEXTO     TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTE DE FILE STATUS DO ARQUIVO CLIEMUUF                    *
      ******************************************************************
      *----------------------------------------------------------------*
       1120-TESTAR-FS-CLIEMUUF         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-CLIEMUUF         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-CLIEMUUF    TO WRK-FILE-STATUS
               MOVE 'CLIEMUUF'         TO WRK-NOME-ARQ
               MOVE WRK-ERRO-TEXTO     TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICA SE O ARQUIVO DE ENTRADA ESTA VAZIO                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-LER-CLIEADES.

           IF  WRK-FS-CLIEADES         EQUAL '10'
               DISPLAY '************** GFCT2120 **************'
               DISPLAY '*                                    *'
               DISPLAY '*       ARQUIVO CLIEADES VAZIO       *'
               DISPLAY '*                                    *'
               DISPLAY '*      PROCESSAMENTO  CANCELADO      *'
               DISPLAY '*                                    *'
               DISPLAY '************** GFCT2120 **************'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-MSG001         TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    LEITURA DO ARQUIVO DE PARAMETROS CLIEADES                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-LER-CLIEADES               SECTION.
      *----------------------------------------------------------------*

           READ CLIEADES.

           IF  WRK-FS-CLIEADES         EQUAL '10'
               GO TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1110-TESTAR-FS-CLIEADES.

           ADD 1                       TO ACU-LIDOS-CLIEADES.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCESSAMENTO PRINCIPAL DO PROGRAMA                         *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 3100-OBTER-MUNICIPIO-AGENDA.

           PERFORM 3200-GRAVAR-CLIEMUUF.

           PERFORM 2100-LER-CLIEADES.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    OBTEM CODIGO DO MUNICIPIO E SIGLA DA UF DA AGENCIA PELO     *
      *    MODULO GFCT5564                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       3100-OBTER-MUNICIPIO-AGENDA     SECTION.
      *----------------------------------------------------------------*

           INITIALIZE GFCTRK-AREA-RETORNO
                      GFCT0M-AREA-ERROS.

           MOVE 'B'                    TO GFCTRK-TPO-AMBIENTE.
           MOVE GFCTRU-AGENCIA         TO GFCTRK-CJUNC-DEPDC.
           MOVE 'GFCT5564'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTRK-AREA
                                             GFCT0M-AREA-ERROS.

           IF (GFCTRK-COD-RETORNO      NOT EQUAL ZEROS)    OR
              (RETURN-CODE             NOT EQUAL ZEROS)
               DISPLAY '************** GFCT2120 **************'
               DISPLAY '*   PROGRAMA GFCT2120                *'
               DISPLAY '*   -----------------                *'
               DISPLAY '*   ERRO NO ACESSO AO MODULO         *'
               DISPLAY '*   GFCT5564 PELA AGENCIA:           *'
               DISPLAY '*   ' GFCTRU-AGENCIA
                                 '                           *'
               DISPLAY '************** GFCT2120 **************'
               PERFORM 4000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    GRAVA OS REGISTROS NO ARQUIVO CLIEMUUF                      *
      ******************************************************************
      *----------------------------------------------------------------*
       3200-GRAVAR-CLIEMUUF            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTRU-AGENCIA         TO GFCTRY-AGENCIA.
           MOVE GFCTRU-RAZAO           TO GFCTRY-RAZAO.
           MOVE GFCTRU-CONTA           TO GFCTRY-CONTA.
           MOVE GFCTRU-CGC-CPF         TO GFCTRY-CGC-CPF.
           MOVE GFCTRU-FILIAL          TO GFCTRY-FILIAL.
           MOVE GFCTRU-CONTR           TO GFCTRY-CONTR.
           MOVE GFCTRU-POSTO-SERV      TO GFCTRY-POSTO-SERV.
           MOVE GFCTRU-DT-ABERT        TO GFCTRY-DT-ABERT.
           MOVE ZEROS                  TO GFCTRY-SEGTO-GFCT.
           MOVE GFCTRK-CMUN-RURAL      TO GFCTRY-MUNICIPIO.
           MOVE GFCTRK-CSGL-UF         TO GFCTRY-SIGLA-UF.

           WRITE GFCTRY-ENTRADA.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 1120-TESTAR-FS-CLIEMUUF.

           ADD 1                       TO ACU-GRAVA-CLIEMUUF.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCEDIMENTOS REALIZADOS NO FINAL DO PROCESSAMENTO          *
      ******************************************************************
      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 4100-EMITIR-DISPLAY.

           CLOSE CLIEADES
                 CLIEMUUF.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

           STOP RUN.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *   EMITE DISPLAY COM A QUANTIDADE DE REGISTROS LIDOS E GRAVADOS *
      ******************************************************************
      *----------------------------------------------------------------*
       4100-EMITIR-DISPLAY             SECTION.
      *----------------------------------------------------------------*

           DISPLAY '************** GFCT2120 **************'.
           DISPLAY '* TOTAL DE REGISTROS:                *'.
           DISPLAY '*                                    *'.
           MOVE ACU-LIDOS-CLIEADES     TO WRK-MASCARA.
           DISPLAY '* LIDOS EM CLIEADES   = ' WRK-MASCARA
                                                      '  *'.
           MOVE ACU-GRAVA-CLIEMUUF     TO WRK-MASCARA.
           DISPLAY '* GRAVADOS EM CLIEMUUF = ' WRK-MASCARA
                                                      '  *'.
           DISPLAY '*                                    *'.
           DISPLAY '************** GFCT2120 **************'.

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    EXECUTA PROCEDIMENTO DE CANCELAMENTO DO PGM EM CASO DE ERRO *
      ******************************************************************
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT2120'             TO ERR-PGM.
           MOVE 4                      TO RETURN-CODE.

           CALL 'POOL7100'             USING WRK-BATCH
                                             ERRO-AREA.
           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
