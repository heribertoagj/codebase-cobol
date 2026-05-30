      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. GFCT1734.
       AUTHOR.     VINICIUS CRISTIAN MADUREIRA.
      *================================================================*
      *      S O N D A  P R O C W O R K  -  C O N S U L T O R I A      *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT1734                                    *
      *    PROGRAMADOR.:   VINICIUS CRISTIAN MADUREIRA - SONDPROC/GP.50*
      *    ANALISTA....:   VINICIUS CRISTIAN MADUREIRA - SONDPROC/GP.50*
      *    DATA........:   12/06/2008                                  *
      *                                                                *
      *    OBJETIVO    :   BALANCE-LINE ENTRE OS ARQUIVOS SELECAO3 E   *
      *                    ADESOMES.                                   *
      *                                                                *
      *    ARQUIVOS:                                                   *
      *      DDNAME                                    INCLUDE/BOOK    *
      *      SELECAO3                                    I#GFCT61      *
      *      ADESOMES                                    I#GFCTSX      *
      *      SELECAO4                                    I#GFCT61      *
      *      ADPACOTE                                    -             *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      BRAD7100  -  MODULO DE TRATAMENTO DE ERRO.                *
      *                                                                *
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

      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*

       FILE-CONTROL.

           SELECT SELECAO3 ASSIGN      TO UT-S-SELECAO3
                      FILE STATUS      IS WRK-FS-SELECAO3.

           SELECT ADESOMES ASSIGN      TO UT-S-ADESOMES
                      FILE STATUS      IS WRK-FS-ADESOMES.

           SELECT SELECAO4 ASSIGN      TO UT-S-SELECAO4
                      FILE STATUS      IS WRK-FS-SELECAO4.

           SELECT ADPACOTE ASSIGN      TO UT-S-ADPACOTE
                      FILE STATUS      IS WRK-FS-ADPACOTE.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:                                                      *
      *            ORG. SEQUENTIAL     -  LRECL = 030                  *
      *----------------------------------------------------------------*

       FD  SELECAO3
           RECORDING MODE IS F
           LABEL RECORDS IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCT61'.

      *----------------------------------------------------------------*
      *    INPUT:                                                      *
      *            ORG. SEQUENTIAL     -  LRECL = 030                  *
      *----------------------------------------------------------------*

       FD  ADESOMES
           RECORDING MODE IS F
           LABEL RECORDS IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTSX'.

      *----------------------------------------------------------------*
      *    OUTPUT:                                                     *
      *            ORG. SEQUENTIAL     -  LRECL = 030                  *
      *----------------------------------------------------------------*

       FD  SELECAO4
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCT61'.

      *----------------------------------------------------------------*
      *    OUTPUT:                                                     *
      *            ORG. SEQUENTIAL     -  LRECL = 013                  *
      *----------------------------------------------------------------*

       FD  ADPACOTE
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-ADPACOTE.
           03 FD-CPF-NUM            PIC 9(09).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  INICIO DA WORKING GFCT1734  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*        ACUMULADORES          *'.
      *----------------------------------------------------------------*

       01  ACU-ACUMULADORES.
           05 ACU-LIDOS-SELECAO3       PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-LIDOS-ADESOMES       PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-GRAVADOS-SELECAO4    PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-GRAVADOS-ADPACOTE    PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-DESPREZADOS          PIC 9(11) COMP-3    VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    VARIAVEIS AUXILIARES      *'.
      *----------------------------------------------------------------*

       01  WRK-AUXILIARES.
           05 WRK-BATCH                PIC X(08)           VALUE
              'BATCH'.
           05 WRK-MASCARA              PIC ZZ.ZZZ.ZZZ.Z99   VALUE ZEROS.

       01  WRK-ADESOMES-ATU.
           05  WRK-AGENCIA-ATU         PIC 9(05)           VALUE ZEROS.
           05  WRK-CONTA-ATU           PIC 9(07)           VALUE ZEROS.

       01  WRK-ADESOMES-ANT.
           05  WRK-AGENCIA-ANT         PIC 9(05)           VALUE ZEROS.
           05  WRK-CONTA-ANT           PIC 9(07)           VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*       AREA PARA CHAVES       *'.
      *----------------------------------------------------------------*

       01  WRK-CONTA                   PIC  9(013)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-CONTA.
           05  FILLER                  PIC  9(006).
           05  WRK-GFCT-CONTA          PIC  9(007).

       01  WRK-CHV-SELECAO3.
           05 WRK-AG-SELECAO3        PIC 9(05)           VALUE ZEROS.
           05 WRK-CTA-SELECAO3       PIC 9(07)           VALUE ZEROS.

       01  WRK-CHV-ADESOMES.
           05 WRK-AG-ADESOMES        PIC 9(05)           VALUE ZEROS.
           05 WRK-CTA-ADESOMES       PIC 9(07)           VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    AREA PARA FILE STATUS     *'.
      *----------------------------------------------------------------*

       01  WRK-FS-SELECAO3             PIC X(02)           VALUE SPACES.
       01  WRK-FS-ADESOMES             PIC X(02)           VALUE SPACES.
       01  WRK-FS-SELECAO4             PIC X(02)           VALUE SPACES.
       01  WRK-FS-ADPACOTE             PIC X(02)           VALUE SPACES.
       01  WRK-ABERTURA                PIC X(13)           VALUE
           'NA ABERTURA'.
       01  WRK-LEITURA                 PIC X(13)           VALUE
           'NA  LEITURA'.
       01  WRK-GRAVACAO                PIC X(13)           VALUE
           'NA GRAVACAO'.
       01  WRK-FECHAMENTO              PIC X(13)           VALUE
           'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  AREA  DE MENSAGEM  DE ERRO  *'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-FS.
           05  FILLER                  PIC X(07)           VALUE SPACES.
           05  FILLER                  PIC X(07)           VALUE
               '* ERRO '.
           05  WRK-OPERACAO            PIC X(13)           VALUE SPACES.
           05  FILLER                  PIC X(12)           VALUE
              ' DO ARQUIVO '.
           05  WRK-NOME-ARQ            PIC X(08)           VALUE SPACES.
           05  FILLER                  PIC X(17)           VALUE
              ' - FILE-STATUS = '.
           05  WRK-FS                  PIC X(02)           VALUE SPACES.
           05  FILLER                  PIC X(02)           VALUE ' *'.
           05  FILLER                  PIC X(07)           VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     AREA PARA BRAD7100       *'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*   FIM  DA WORKING GFCT1734   *'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
      *    ROTINA PRINCIPAL DO PROGRAMA                                *
      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           PERFORM 0100-INICIALIZAR-PROGRAMA.

           PERFORM 0700-VERIFICAR-VAZIO.

           PERFORM 1000-PROCESSAR      UNTIL
                    WRK-CHV-SELECAO3   EQUAL HIGH-VALUES.

           PERFORM 1300-PROCEDIMENTOS-FINAIS.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA INICIALIZAR O PROGRAMA                          *
      *----------------------------------------------------------------*
       0100-INICIALIZAR-PROGRAMA       SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT SELECAO3
                      ADESOMES
               OUTPUT ADPACOTE
                      SELECAO4.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 0200-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       0100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE STATUS DOS ARQUIVOS DE ENTRADA E SAIDA          *
      *----------------------------------------------------------------*
       0200-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 0300-TESTAR-FS-SELECAO3.

           PERFORM 0400-TESTAR-FS-ADESOMES.

           PERFORM 0500-TESTAR-FS-SELECAO4.

           PERFORM 0600-TESTAR-FS-ADPACOTE.

      *----------------------------------------------------------------*
       0200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO SELECAO3                      *
      *----------------------------------------------------------------*
       0300-TESTAR-FS-SELECAO3         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SELECAO3         NOT EQUAL '00'
               MOVE 'SELECAO3'         TO WRK-NOME-ARQ
               MOVE WRK-FS-SELECAO3    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO ADESOMES                      *
      *----------------------------------------------------------------*
       0400-TESTAR-FS-ADESOMES         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ADESOMES         NOT EQUAL '00'
               MOVE 'ADESOMES'         TO WRK-NOME-ARQ
               MOVE WRK-FS-ADESOMES    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO SELECAO4                      *
      *----------------------------------------------------------------*
       0500-TESTAR-FS-SELECAO4         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SELECAO4         NOT EQUAL '00'
               MOVE 'SELECAO4'         TO WRK-NOME-ARQ
               MOVE WRK-FS-SELECAO4    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO ADPACOTE                      *
      *----------------------------------------------------------------*
       0600-TESTAR-FS-ADPACOTE         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ADPACOTE         NOT EQUAL '00'
               MOVE 'ADPACOTE'         TO WRK-NOME-ARQ
               MOVE WRK-FS-ADPACOTE    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA P/ VERIFICAR SE OS ARQUIVOS DE ENTRADA ESTAO VAZIOS  *
      *----------------------------------------------------------------*
       0700-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 0800-LER-SELECAO3.

           IF  WRK-CHV-SELECAO3        EQUAL HIGH-VALUES
               DISPLAY '*********** GFCT1734 ***********'
               DISPLAY '*                              *'
               DISPLAY '*    ARQUIVO  SELECAO3 VAZIO   *'
               DISPLAY '*    PROCESSAMENTO ENCERRADO   *'
               DISPLAY '*                              *'
               DISPLAY '*********** GFCT1734 ***********'
           END-IF.

           PERFORM 0900-LER-ADESOMES.

           IF  WRK-CHV-ADESOMES            EQUAL HIGH-VALUES
               DISPLAY '*********** GFCT1734 ***********'
               DISPLAY '*                              *'
               DISPLAY '*    ARQUIVO ADESOMES VAZIO    *'
               DISPLAY '*                              *'
               DISPLAY '*********** GFCT1734 ***********'
           END-IF.

      *----------------------------------------------------------------*
       0700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LEITURA DO ARQUIVO SELECAO3                                 *
      *----------------------------------------------------------------*
       0800-LER-SELECAO3               SECTION.
      *----------------------------------------------------------------*

           READ SELECAO3.

           IF  WRK-FS-SELECAO3         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHV-SELECAO3
               GO                      TO 0800-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 0300-TESTAR-FS-SELECAO3.

           MOVE GFCT61-AGENCIA OF SELECAO3 TO WRK-AG-SELECAO3.
           MOVE GFCT61-CONTA   OF SELECAO3 TO WRK-CONTA.
           MOVE WRK-GFCT-CONTA             TO WRK-CTA-SELECAO3.

           ADD 1                       TO ACU-LIDOS-SELECAO3.

      *----------------------------------------------------------------*
       0800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LEITURA DO ARQUIVO ADESOMES                                 *
      *----------------------------------------------------------------*
       0900-LER-ADESOMES               SECTION.
      *----------------------------------------------------------------*

           READ ADESOMES.

           IF  WRK-FS-ADESOMES         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHV-ADESOMES
               GO                      TO 0900-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 0400-TESTAR-FS-ADESOMES.

           MOVE GFCTSX-AGENCIA         TO WRK-AG-ADESOMES
                                          WRK-AGENCIA-ATU.
           MOVE GFCTSX-CONTA           TO WRK-CTA-ADESOMES
                                          WRK-CONTA-ATU.

           ADD 1                       TO ACU-LIDOS-ADESOMES.

           IF WRK-ADESOMES-ATU EQUAL WRK-ADESOMES-ANT
               ADD 1 TO ACU-DESPREZADOS
               GO TO 0900-LER-ADESOMES
           END-IF.

      *----------------------------------------------------------------*
       0900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL DO PROGRAMA                         *
      *----------------------------------------------------------------*
       1000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-ADESOMES-ATU TO WRK-ADESOMES-ANT.

           IF  WRK-CHV-SELECAO3      EQUAL WRK-CHV-ADESOMES
               MOVE GFCT61-CPF-NUM OF SELECAO3 TO FD-CPF-NUM
               WRITE FD-ADPACOTE  OF ADPACOTE
               MOVE WRK-GRAVACAO   TO WRK-OPERACAO
               PERFORM 0600-TESTAR-FS-ADPACOTE
               ADD 1               TO ACU-GRAVADOS-ADPACOTE
               PERFORM 0800-LER-SELECAO3
           ELSE
               IF  WRK-CHV-SELECAO3  LESS WRK-CHV-ADESOMES
                   PERFORM 1100-GRAVAR-SELECAO4
                   PERFORM 0800-LER-SELECAO3
               ELSE
                   PERFORM 0900-LER-ADESOMES
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    GRAVAR ARQUIVO  SELECAO4                                    *
      *----------------------------------------------------------------*
       1100-GRAVAR-SELECAO4            SECTION.
      *----------------------------------------------------------------*

           MOVE REG-GFCT61 OF SELECAO3
                               TO REG-GFCT61 OF SELECAO4.

           WRITE REG-GFCT61 OF SELECAO4.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 0500-TESTAR-FS-SELECAO4.

           ADD 1                       TO ACU-GRAVADOS-SELECAO4.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA FINALIZACAO DO PROGRAMA                         *
      *----------------------------------------------------------------*
       1300-PROCEDIMENTOS-FINAIS       SECTION.
      *----------------------------------------------------------------*

           PERFORM 1400-EMITIR-DISPLAY.

           CLOSE SELECAO3
                 ADESOMES
                 ADPACOTE
                 SELECAO4.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 0200-TESTAR-FILE-STATUS.

           STOP RUN.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA EMISSAO DOS DISPLAYS FINAIS DO PROGRAMA         *
      *----------------------------------------------------------------*
       1400-EMITIR-DISPLAY             SECTION.
      *----------------------------------------------------------------*

           DISPLAY '******************** GFCT1734 ********************'.
           DISPLAY '*                                                *'.
           MOVE ACU-LIDOS-SELECAO3     TO WRK-MASCARA.
           DISPLAY '*  LIDOS EM SELECAO3......: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-LIDOS-ADESOMES     TO WRK-MASCARA.
           DISPLAY '*  LIDOS EM ADESOMES......: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-GRAVADOS-SELECAO4  TO WRK-MASCARA.
           DISPLAY '*  GRAVADOS NO SELECAO4...: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-GRAVADOS-ADPACOTE  TO WRK-MASCARA.
           DISPLAY '*  GRAVADOS NO ADPACOTE...: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-DESPREZADOS        TO WRK-MASCARA.
           DISPLAY '*  REG. DUPLICADO ADESOMES: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           DISPLAY '******************** GFCT1734 ********************'.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAMENTO DE ERRO                              *
      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT1734'             TO ERR-PGM.
           MOVE 'APL'                  TO ERR-TIPO-ACESSO.

           CALL 'BRAD7100'             USING WRK-BATCH
                                             ERRO-AREA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------
