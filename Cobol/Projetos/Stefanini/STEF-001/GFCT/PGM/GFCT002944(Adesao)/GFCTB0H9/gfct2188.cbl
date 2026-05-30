      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. GFCT2188.
       AUTHOR.     VINICIUS CRISTIAN MADUREIRA.
      *================================================================*
      *      S O N D A  P R O C W O R K  -  C O N S U L T O R I A      *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT2188                                    *
      *    PROGRAMADOR.:   VINICIUS CRISTIAN MADUREIRA - PROCWORK/GP50 *
      *    ANALISTA....:   VINICIUS CRISTIAN MADUREIRA - PROCWORK/GP50 *
      *    DATA........:   16/06/2008                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   IDENTIFICA CLIENTES SEM ADESAO.             *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                          INCLUDE/BOOK    *
      *                     ADESOMES                   I#GFCTSX        *
      *                     CLIEADES                   I#GFCTRU        *
      *                     CLISEMPC                   I#GFCTRU        *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    BRAD1050 - FAZ CONEXAO COM DB2.                             *
      *    BRAD7100 - AREA PARA TRATAMENTO DE ERROS.                   *
      *================================================================*

      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *================================================================*
       INPUT-OUTPUT                    SECTION.
      *================================================================*

       FILE-CONTROL.

           SELECT ADESOMES ASSIGN      TO UT-S-ADESOMES
                      FILE STATUS      IS WRK-FS-ADESOMES.

           SELECT CLIEADES ASSIGN      TO UT-S-CLIEADES
                      FILE STATUS      IS WRK-FS-CLIEADES.

           SELECT CLISEMPC ASSIGN      TO UT-S-CLISEMPC
                      FILE STATUS      IS WRK-FS-CLISEMPC.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *================================================================*
       FILE                            SECTION.
      *================================================================*

      *----------------------------------------------------------------*
      *   INPUT: REGISTRO                                             *
      *           ORG.SEQUENCIAL       - LRECL = 030                   *
      *----------------------------------------------------------------*

       FD  ADESOMES
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTSX'.

      *----------------------------------------------------------------*
      *   INPUT: REGISTRO                                             *
      *           ORG.SEQUENCIAL       - LRECL = 040                   *
      *----------------------------------------------------------------*

       FD  CLIEADES
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTRU'.

      *----------------------------------------------------------------*
      *   OUTPUT: REGISTRO                                             *
      *           ORG.SEQUENCIAL       - LRECL = 040                   *
      *----------------------------------------------------------------*

       FD  CLISEMPC
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTRU'.

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)          VALUE
           '*   INICIO DA WORKING GFCT2188 *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*         ACUMULADORES         *'.
      *----------------------------------------------------------------*

       01  ACU-ACUMULADORES.
           05  ACU-LIDOS-ADESOMES      PIC  9(11)  COMP-3  VALUE ZEROS.
           05  ACU-LIDOS-CLIEADES      PIC  9(11)  COMP-3  VALUE ZEROS.
           05  ACU-GRAVA-CLISEMPC      PIC  9(11)  COMP-3  VALUE ZEROS.
           05  ACU-ENCONTRADOS         PIC  9(11)  COMP-3  VALUE ZEROS.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '* VARIAVEIS AUXILIARES         *'.
      *---------------------------------------------------------------*

       01  WRK-AUXILIARES.
           05  WRK-BATCH               PIC  X(08)           VALUE
               'BATCH'.
           05  WRK-MASCARA             PIC  ZZ.ZZZ.ZZZ.ZZ9  VALUE ZEROS.

       01  WRK-CH-ADESOMES.
           05 WRK-AGENCIA-ADESOMES     PIC  9(005)          VALUE ZEROS.
           05 WRK-CONTA-ADESOMES       PIC  9(007)          VALUE ZEROS.

       01  WRK-CH-CLIEADES.
           05 WRK-AGENCIA-CLIEADES     PIC  9(005)          VALUE ZEROS.
           05 WRK-CONTA-CLIEADES       PIC  9(007)          VALUE ZEROS.

       01  WRK-S9-5                    PIC S9(005)          VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-S9-5.
           05  WRK-9-5                 PIC  9(005).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*  AREA PARA FILE-STATUS       *'.
      *----------------------------------------------------------------*

       01  WRK-AREA-FILE-STATUS.
           05  WRK-FS-CLIEADES         PIC  X(02)          VALUE SPACES.
           05  WRK-FS-ADESOMES         PIC  X(02)          VALUE SPACES.
           05  WRK-FS-CLISEMPC         PIC  X(02)          VALUE SPACES.
           05  WRK-ABERTURA            PIC  X(13)          VALUE
               ' NA ABERTURA'.
           05  WRK-LEITURA             PIC  X(13)          VALUE
               ' NA LEITURA'.
           05  WRK-GRAVACAO            PIC  X(13)          VALUE
               ' NA GRAVACAO'.
           05  WRK-FECHAMENTO          PIC  X(13)          VALUE
               'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*  MENSAGENS DE ERRO           *'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-FS.
           05  FILLER                  PIC  X(07)          VALUE SPACES.
           05  FILLER                  PIC  X(07)          VALUE
               '* ERRO '.
           05  WRK-OPERACAO            PIC  X(13)          VALUE SPACES.
           05  FILLER                  PIC  X(12)          VALUE
               ' DO ARQUIVO '.
           05  WRK-NOME-ARQ            PIC  X(08)          VALUE SPACES.
           05  FILLER                  PIC  X(17)          VALUE
               ' - FILE-STATUS = '.
           05  WRK-FS                  PIC  X(02)          VALUE SPACES.
           05  FILLER                  PIC  X(02)          VALUE ' *'.
           05  FILLER                  PIC  X(07)          VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*       AREA PARA BRAD7100     *'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    FIM DA WORKING GFCT2188  *'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           PERFORM 0100-INICIALIZAR-PROGRAMA.

           PERFORM 0400-VERIFICAR-VAZIO.

           PERFORM 0600-PROCESSAR      UNTIL
                   WRK-CH-CLIEADES     EQUAL HIGH-VALUES.

           PERFORM 1200-FINALIZAR-PROGRAMA.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0100-INICIALIZAR-PROGRAMA       SECTION.
      *----------------------------------------------------------------*

           OPEN    INPUT  ADESOMES
                          CLIEADES
                   OUTPUT CLISEMPC.

           MOVE  WRK-ABERTURA          TO WRK-OPERACAO.

           PERFORM 0200-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       0100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0200-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 0250-TESTAR-FS-ADESOMES.

           PERFORM 0300-TESTAR-FS-CLIEADES.

           PERFORM 0350-TESTAR-FS-CLISEMPC.

      *----------------------------------------------------------------*
       0200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0250-TESTAR-FS-ADESOMES         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ADESOMES         NOT EQUAL '00'
               MOVE 'ADESOMES'         TO WRK-NOME-ARQ
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-ADESOMES    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0250-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0300-TESTAR-FS-CLIEADES         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-CLIEADES         NOT EQUAL '00'
               MOVE 'CLIEADES'         TO WRK-NOME-ARQ
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-CLIEADES    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0350-TESTAR-FS-CLISEMPC         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-CLISEMPC         NOT EQUAL '00'
               MOVE 'CLISEMPC'         TO WRK-NOME-ARQ
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-CLISEMPC    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0350-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0400-VERIFICAR-VAZIO                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 0450-LER-ADESOMES.

           IF  WRK-CH-ADESOMES         EQUAL HIGH-VALUES
               DISPLAY '************* GFCT2188 ************'
               DISPLAY '*                                 *'
               DISPLAY '*     ARQUIVO ADESOMES VAZIO      *'
               DISPLAY '*                                 *'
               DISPLAY '*    PROCESSAMENTO  ENCERRADO     *'
               DISPLAY '*                                 *'
               DISPLAY '************* GFCT2188 ************'
           END-IF.

           PERFORM 0500-LER-CLIEADES.

           IF  WRK-CH-CLIEADES         EQUAL HIGH-VALUES
               DISPLAY '************* GFCT2188 ************'
               DISPLAY '*                                 *'
               DISPLAY '*     ARQUIVO CLIEADES VAZIO      *'
               DISPLAY '*                                 *'
               DISPLAY '*    PROCESSAMENTO  ENCERRADO     *'
               DISPLAY '*                                 *'
               DISPLAY '************* GFCT2188 ************'
           END-IF.

      *----------------------------------------------------------------*
       0400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0450-LER-ADESOMES               SECTION.
      *----------------------------------------------------------------*

           READ ADESOMES.

           IF  WRK-FS-ADESOMES         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CH-ADESOMES
               GO                      TO 0450-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 0250-TESTAR-FS-ADESOMES.

           MOVE GFCTSX-AGENCIA OF ADESOMES TO WRK-AGENCIA-ADESOMES.
           MOVE GFCTSX-CONTA   OF ADESOMES TO WRK-CONTA-ADESOMES.

           ADD 1                       TO ACU-LIDOS-ADESOMES.

      *----------------------------------------------------------------*
       0450-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0500-LER-CLIEADES               SECTION.
      *----------------------------------------------------------------*

           READ CLIEADES.

           IF  WRK-FS-CLIEADES         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CH-CLIEADES
               GO                      TO 0500-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 0300-TESTAR-FS-CLIEADES.

           MOVE GFCTRU-AGENCIA OF CLIEADES TO WRK-S9-5
           MOVE WRK-9-5                    TO WRK-AGENCIA-CLIEADES.
           MOVE GFCTRU-CONTA   OF CLIEADES TO WRK-CONTA-CLIEADES.

           ADD 1                       TO ACU-LIDOS-CLIEADES.

      *----------------------------------------------------------------*
       0500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0600-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           IF WRK-CH-CLIEADES EQUAL WRK-CH-ADESOMES
               PERFORM 0450-LER-ADESOMES
               PERFORM 0500-LER-CLIEADES
               ADD 1 TO ACU-ENCONTRADOS
           ELSE
               IF WRK-CH-CLIEADES LESS WRK-CH-ADESOMES
                   MOVE GFCTRU-ENTRADA OF CLIEADES
                                     TO GFCTRU-ENTRADA OF CLISEMPC
                   PERFORM 0760-GRAVAR-SAIDA-CLISEMPC
                   PERFORM 0500-LER-CLIEADES
               ELSE
                   PERFORM 0450-LER-ADESOMES
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       0600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0760-GRAVAR-SAIDA-CLISEMPC      SECTION.
      *----------------------------------------------------------------*

           WRITE GFCTRU-ENTRADA OF CLISEMPC.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 0350-TESTAR-FS-CLISEMPC.

           ADD 1                       TO ACU-GRAVA-CLISEMPC.

      *----------------------------------------------------------------*
       0760-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA DE FINALIZACAO DO PROGRAMA                           *
      *----------------------------------------------------------------*
       1200-FINALIZAR-PROGRAMA         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1300-EMITIR-DISPLAY.

           CLOSE   ADESOMES
                   CLIEADES
                   CLISEMPC.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.
           PERFORM 0200-TESTAR-FILE-STATUS.

           STOP RUN.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA DE EMISSAO DE TOTAIS DE CONTROLE                     *
      *----------------------------------------------------------------*
       1300-EMITIR-DISPLAY             SECTION.
      *----------------------------------------------------------------*

               DISPLAY '*-----------------------------------------------
      -        '--------*'
               DISPLAY '*                   PROGRAMA GFCT2188
      -        '        *'
               DISPLAY '*                   -----------------
      -        '        *'
               DISPLAY '*
      -        '        *'
               DISPLAY '*          PROCESSAMENTO  EFETUADO COM SUCESSO
      -        '        *'
               DISPLAY '*-----------------------------------------------
      -        '--------*'
               DISPLAY '************************************************
      -        '*********'
               MOVE ACU-LIDOS-ADESOMES   TO WRK-MASCARA
               DISPLAY '* TOTAL DE REGISTROS LIDOS ADESOMES  = '
                 WRK-MASCARA
                              '   *'
               MOVE ACU-LIDOS-CLIEADES      TO WRK-MASCARA
               DISPLAY '* TOTAL DE REGISTROS LIDOS CLIEADES  = '
                 WRK-MASCARA
                              '   *'
               MOVE ACU-ENCONTRADOS      TO WRK-MASCARA
               DISPLAY '* ENCONTRADOS NO CLIE                = '
                 WRK-MASCARA
                              '   *'
               MOVE ACU-GRAVA-CLISEMPC   TO WRK-MASCARA
               DISPLAY '* TOTAL DE REGISTROS GRAV. CLISEMPC  = '
                 WRK-MASCARA
                              '   *'
               DISPLAY '************************************************
      -        '*********'.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     RETORNO DE ERRO                                            *
      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           IF ERR-TIPO-ACESSO         NOT EQUAL 'DB2'
               MOVE 'APL'             TO ERR-TIPO-ACESSO
           END-IF.

           MOVE 'GFCT2188'            TO ERR-PGM.

           CALL 'BRAD7100'            USING WRK-BATCH
                                             ERRO-AREA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                   EXIT.
      *----------------------------------------------------------------*
