      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. GFCT1740.
       AUTHOR.     VINICIUS CRISTIAN MADUREIRA.
      *================================================================*
      *      S O N D A  P R O C W O R K  -  C O N S U L T O R I A      *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT1740                                    *
      *    PROGRAMADOR.:   VINICIUS CRISTIAN MADUREIRA - SONDPROC/GP.50*
      *    ANALISTA....:   VINICIUS CRISTIAN MADUREIRA - SONDPROC/GP.50*
      *    DATA........:   16/06/2008                                  *
      *                                                                *
      *    OBJETIVO    :   DESPREZAR REGISTROS QUANDO CPF FOR IGUAL AO *
      *                    ENCONTRADO EM ADPACOTE.                     *
      *                                                                *
      *    ARQUIVOS:                                                   *
      *      DDNAME                                    INCLUDE/BOOK    *
      *      SELECAO5                                    I#GFCT61      *
      *      ADPACOTE                                    -             *
      *      SELECAO6                                    I#GFCT61      *
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

           SELECT SELECAO5 ASSIGN      TO UT-S-SELECAO5
                      FILE STATUS      IS WRK-FS-SELECAO5.

           SELECT ADPACOTE ASSIGN      TO UT-S-ADPACOTE
                      FILE STATUS      IS WRK-FS-ADPACOTE.

           SELECT SELECAO6 ASSIGN      TO UT-S-SELECAO6
                      FILE STATUS      IS WRK-FS-SELECAO6.

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

       FD  SELECAO5
           RECORDING MODE IS F
           LABEL RECORDS IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCT61'.

      *----------------------------------------------------------------*
      *    INPUT:                                                      *
      *            ORG. SEQUENTIAL     -  LRECL = 009                  *
      *----------------------------------------------------------------*

       FD  ADPACOTE
           RECORDING MODE IS F
           LABEL RECORDS IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-ADPACOTE.
           03 FD-CPF-NUM            PIC 9(09).

      *----------------------------------------------------------------*
      *    OUTPUT:                                                     *
      *            ORG. SEQUENTIAL     -  LRECL = 030                  *
      *----------------------------------------------------------------*

       FD  SELECAO6
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCT61'.

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  INICIO DA WORKING GFCT1740  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*        ACUMULADORES          *'.
      *----------------------------------------------------------------*

       01  ACU-ACUMULADORES.
           05 ACU-LIDOS-SELECAO5       PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-LIDOS-ADPACOTE       PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-GRAVADOS-SELECAO6    PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-DESPREZADOS          PIC 9(11) COMP-3    VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    VARIAVEIS AUXILIARES      *'.
      *----------------------------------------------------------------*

       01  WRK-AUXILIARES.
           05 WRK-BATCH                PIC X(08)           VALUE
              'BATCH'.
           05 WRK-MASCARA              PIC ZZ.ZZZ.ZZZ.Z99   VALUE ZEROS.

       01  WRK-CHV-SELECAO5.
           05 CPF-SELECAO5             PIC 9(09)           VALUE ZEROS.

       01  WRK-CHV-ADPACOTE.
           05 CPF-ADPACOTE             PIC 9(09)           VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    AREA PARA FILE STATUS     *'.
      *----------------------------------------------------------------*

       01  WRK-FS-SELECAO5             PIC X(02)           VALUE SPACES.
       01  WRK-FS-ADPACOTE             PIC X(02)           VALUE SPACES.
       01  WRK-FS-SELECAO6             PIC X(02)           VALUE SPACES.
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
           '*   FIM  DA WORKING GFCT1740   *'.
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
                    WRK-CHV-SELECAO5   EQUAL HIGH-VALUES.

           PERFORM 1300-PROCEDIMENTOS-FINAIS.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA INICIALIZAR O PROGRAMA                          *
      *----------------------------------------------------------------*
       0100-INICIALIZAR-PROGRAMA       SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT SELECAO5
                      ADPACOTE
               OUTPUT SELECAO6.

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

           PERFORM 0300-TESTAR-FS-SELECAO5.

           PERFORM 0400-TESTAR-FS-ADPACOTE.

           PERFORM 0500-TESTAR-FS-SELECAO6.

      *----------------------------------------------------------------*
       0200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO SELECAO5                      *
      *----------------------------------------------------------------*
       0300-TESTAR-FS-SELECAO5         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SELECAO5         NOT EQUAL '00'
               MOVE 'SELECAO5'         TO WRK-NOME-ARQ
               MOVE WRK-FS-SELECAO5    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO ADPACOTE                      *
      *----------------------------------------------------------------*
       0400-TESTAR-FS-ADPACOTE         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ADPACOTE         NOT EQUAL '00'
               MOVE 'ADPACOTE'         TO WRK-NOME-ARQ
               MOVE WRK-FS-ADPACOTE    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO SELECAO6                      *
      *----------------------------------------------------------------*
       0500-TESTAR-FS-SELECAO6         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SELECAO6         NOT EQUAL '00'
               MOVE 'SELECAO6'         TO WRK-NOME-ARQ
               MOVE WRK-FS-SELECAO6    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA P/ VERIFICAR SE OS ARQUIVOS DE ENTRADA ESTAO VAZIOS  *
      *----------------------------------------------------------------*
       0700-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 0800-LER-SELECAO5.

           IF  WRK-CHV-SELECAO5        EQUAL HIGH-VALUES
               DISPLAY '*********** GFCT1740 ***********'
               DISPLAY '*                              *'
               DISPLAY '*    ARQUIVO  SELECAO5 VAZIO   *'
               DISPLAY '*    PROCESSAMENTO ENCERRADO   *'
               DISPLAY '*                              *'
               DISPLAY '*********** GFCT1740 ***********'
           END-IF.

           PERFORM 0900-LER-ADPACOTE.

           IF  WRK-CHV-ADPACOTE            EQUAL HIGH-VALUES
               DISPLAY '*********** GFCT1740 ***********'
               DISPLAY '*                              *'
               DISPLAY '*    ARQUIVO ADPACOTE VAZIO    *'
               DISPLAY '*                              *'
               DISPLAY '*********** GFCT1740 ***********'
           END-IF.

      *----------------------------------------------------------------*
       0700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LEITURA DO ARQUIVO SELECAO5                                 *
      *----------------------------------------------------------------*
       0800-LER-SELECAO5               SECTION.
      *----------------------------------------------------------------*

           READ SELECAO5.

           IF  WRK-FS-SELECAO5         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHV-SELECAO5
               GO                      TO 0800-99-FIM
           END-IF.

           MOVE GFCT61-CPF-NUM         OF SELECAO5
                                       TO CPF-SELECAO5.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 0300-TESTAR-FS-SELECAO5.

           ADD 1                       TO ACU-LIDOS-SELECAO5.

      *----------------------------------------------------------------*
       0800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LEITURA DO ARQUIVO ADPACOTE                                 *
      *----------------------------------------------------------------*
       0900-LER-ADPACOTE               SECTION.
      *----------------------------------------------------------------*

           READ ADPACOTE.

           IF  WRK-FS-ADPACOTE         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHV-ADPACOTE
               GO                      TO 0900-99-FIM
           END-IF.

           MOVE FD-CPF-NUM          OF ADPACOTE
                                       TO CPF-ADPACOTE.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 0400-TESTAR-FS-ADPACOTE.

           ADD 1                       TO ACU-LIDOS-ADPACOTE.

      *----------------------------------------------------------------*
       0900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL DO PROGRAMA                         *
      *----------------------------------------------------------------*
       1000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           IF  WRK-CHV-SELECAO5      EQUAL WRK-CHV-ADPACOTE
               ADD 1 TO ACU-DESPREZADOS
               PERFORM 0800-LER-SELECAO5
           ELSE
               IF  WRK-CHV-SELECAO5  LESS WRK-CHV-ADPACOTE
                   PERFORM 1100-GRAVAR-SELECAO6
                   PERFORM 0800-LER-SELECAO5
               ELSE
                   PERFORM 0900-LER-ADPACOTE
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    GRAVAR ARQUIVO  SELECAO6                                    *
      *----------------------------------------------------------------*
       1100-GRAVAR-SELECAO6            SECTION.
      *----------------------------------------------------------------*

           MOVE REG-GFCT61  OF SELECAO5
                            TO REG-GFCT61 OF SELECAO6.

           WRITE REG-GFCT61 OF SELECAO6.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 0500-TESTAR-FS-SELECAO6.

           ADD 1                       TO ACU-GRAVADOS-SELECAO6.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA FINALIZACAO DO PROGRAMA                         *
      *----------------------------------------------------------------*
       1300-PROCEDIMENTOS-FINAIS       SECTION.
      *----------------------------------------------------------------*

           PERFORM 1400-EMITIR-DISPLAY.

           CLOSE SELECAO5
                 ADPACOTE
                 SELECAO6.

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

           DISPLAY '******************** GFCT1740 ********************'.
           DISPLAY '*                                                *'.
           MOVE ACU-LIDOS-SELECAO5     TO WRK-MASCARA.
           DISPLAY '*  LIDOS EM SELECAO5......: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-LIDOS-ADPACOTE     TO WRK-MASCARA.
           DISPLAY '*  LIDOS EM ADPACOTE......: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-GRAVADOS-SELECAO6  TO WRK-MASCARA.
           DISPLAY '*  GRAVADOS NO SELECAO6...: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-DESPREZADOS        TO WRK-MASCARA.
           DISPLAY '*  PCTE. PERTENCE OUT. CTA: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           DISPLAY '******************** GFCT1740 ********************'.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAMENTO DE ERRO                              *
      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT1740'             TO ERR-PGM.
           MOVE 'APL'                  TO ERR-TIPO-ACESSO.

           CALL 'BRAD7100'             USING WRK-BATCH
                                             ERRO-AREA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------
