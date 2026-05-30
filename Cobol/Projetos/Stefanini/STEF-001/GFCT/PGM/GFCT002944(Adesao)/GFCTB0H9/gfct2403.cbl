      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. GFCT2403.
       AUTHOR.     VINICIUS CRISTIAN MADUREIRA.
      *================================================================*
      *      S O N D A  P R O C W O R K  -  C O N S U L T O R I A      *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT2403                                    *
      *    PROGRAMADOR.:   VINICIUS CRISTIAN MADUREIRA - SONDPROC/GP.50*
      *    ANALISTA....:   VINICIUS CRISTIAN MADUREIRA - SONDPROC/GP.50*
      *    DATA........:   05/03/2009                                  *
      *                                                                *
      *    OBJETIVO    :   CONTA REGISTROS DELETADOS PARA ATUALIZACAO  *
      *                    NA BASE.                                    *
      *                                                                *
      *    ARQUIVOS:                                                   *
      *      DDNAME                                    INCLUDE/BOOK    *
      *      SOLICPAB                                    GFCTWAFZ      *
      *      DELETPAB                                    GFCTWAF0      *
      *      ATUALPAB                                    GFCTWAFZ      *
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

           SELECT SOLICPAB ASSIGN      TO UT-S-SOLICPAB
                      FILE STATUS      IS WRK-FS-SOLICPAB.

           SELECT DELETPAB ASSIGN      TO UT-S-DELETPAB
                      FILE STATUS      IS WRK-FS-DELETPAB.

           SELECT ATUALPAB ASSIGN      TO UT-S-ATUALPAB
                      FILE STATUS      IS WRK-FS-ATUALPAB.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:                                                      *
      *            ORG. SEQUENTIAL     -  LRECL = 700                  *
      *----------------------------------------------------------------*

       FD  SOLICPAB
           RECORDING MODE IS F
           LABEL RECORDS IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'GFCTWAFZ'.

      *----------------------------------------------------------------*
      *    INPUT:                                                      *
      *            ORG. SEQUENTIAL     -  LRECL = 150                  *
      *----------------------------------------------------------------*

       FD  DELETPAB
           RECORDING MODE IS F
           LABEL RECORDS IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'GFCTWAF0'.

      *----------------------------------------------------------------*
      *    OUTPUT:                                                     *
      *            ORG. SEQUENTIAL     -  LRECL = 700                  *
      *----------------------------------------------------------------*

       FD  ATUALPAB
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'GFCTWAFZ'.

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  INICIO DA WORKING GFCT2403  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*        ACUMULADORES          *'.
      *----------------------------------------------------------------*

       01  ACU-ACUMULADORES.
           05 ACU-LIDOS-SOLICPAB       PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-LIDOS-DELETPAB       PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-GRAVADOS-ATUALPAB    PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-DESPREZADOS          PIC 9(11) COMP-3    VALUE ZEROS.
           05 WRK-ACUMULADOR           PIC 9(11) COMP-3    VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    VARIAVEIS AUXILIARES      *'.
      *----------------------------------------------------------------*

       01  WRK-AUXILIARES.
           05 WRK-BATCH                PIC X(08)            VALUE
              'BATCH'.
           05 WRK-MASCARA              PIC ZZ.ZZZ.ZZZ.Z99   VALUE ZEROS.

       01  WRK-CHV-SOLICPAB.
           03 WRK-AGRUPA-SOLICPAB      PIC 9(003)           VALUE ZEROS.
           03 WRK-TARIF-SOLICPAB       PIC 9(005)           VALUE ZEROS.
           03 WRK-TMSTP-SOLICPAB       PIC X(026)           VALUE ZEROS.

       01  WRK-CHV-DELETPAB.
           03 WRK-AGRUPA-DELETPAB      PIC 9(003)           VALUE ZEROS.
           03 WRK-TARIF-DELETPAB       PIC 9(005)           VALUE ZEROS.
           03 WRK-TMSTP-DELETPAB       PIC X(026)           VALUE ZEROS.

       01  WRK-S9-5                    PIC S9(005)          VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-S9-5.
           05 WRK-9-5                  PIC  9(005).

       01  WRK-S9-3                    PIC S9(003)          VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-S9-3.
           05 WRK-9-3                  PIC  9(003).

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    AREA PARA FILE STATUS     *'.
      *----------------------------------------------------------------*

       01  WRK-FS-SOLICPAB             PIC X(02)           VALUE SPACES.
       01  WRK-FS-DELETPAB             PIC X(02)           VALUE SPACES.
       01  WRK-FS-ATUALPAB             PIC X(02)           VALUE SPACES.
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
           '*   FIM  DA WORKING GFCT2403   *'.
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
                    WRK-CHV-SOLICPAB   EQUAL HIGH-VALUES.

           PERFORM 1300-PROCEDIMENTOS-FINAIS.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA INICIALIZAR O PROGRAMA                          *
      *----------------------------------------------------------------*
       0100-INICIALIZAR-PROGRAMA       SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT SOLICPAB
                      DELETPAB
               OUTPUT ATUALPAB.

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

           PERFORM 0300-TESTAR-FS-SOLICPAB.

           PERFORM 0400-TESTAR-FS-DELETPAB.

           PERFORM 0600-TESTAR-FS-ATUALPAB.

      *----------------------------------------------------------------*
       0200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO SOLICPAB                      *
      *----------------------------------------------------------------*
       0300-TESTAR-FS-SOLICPAB         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SOLICPAB         NOT EQUAL '00'
               MOVE 'SOLICPAB'         TO WRK-NOME-ARQ
               MOVE WRK-FS-SOLICPAB    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO DELETPAB                      *
      *----------------------------------------------------------------*
       0400-TESTAR-FS-DELETPAB         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-DELETPAB         NOT EQUAL '00'
               MOVE 'DELETPAB'         TO WRK-NOME-ARQ
               MOVE WRK-FS-DELETPAB    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO ATUALPAB                      *
      *----------------------------------------------------------------*
       0600-TESTAR-FS-ATUALPAB         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ATUALPAB         NOT EQUAL '00'
               MOVE 'ATUALPAB'         TO WRK-NOME-ARQ
               MOVE WRK-FS-ATUALPAB    TO WRK-FS
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

           PERFORM 0800-LER-SOLICPAB.

           IF  WRK-CHV-SOLICPAB        EQUAL HIGH-VALUES
               DISPLAY '*********** GFCT2403 ***********'
               DISPLAY '*                              *'
               DISPLAY '*    ARQUIVO  SOLICPAB VAZIO   *'
               DISPLAY '*    PROCESSAMENTO ENCERRADO   *'
               DISPLAY '*                              *'
               DISPLAY '*********** GFCT2403 ***********'
           END-IF.

           PERFORM 0900-LER-DELETPAB.

           IF  WRK-CHV-DELETPAB            EQUAL HIGH-VALUES
               DISPLAY '*********** GFCT2403 ***********'
               DISPLAY '*                              *'
               DISPLAY '*    ARQUIVO DELETPAB VAZIO    *'
               DISPLAY '*                              *'
               DISPLAY '*********** GFCT2403 ***********'
           END-IF.

      *----------------------------------------------------------------*
       0700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LEITURA DO ARQUIVO SOLICPAB                                 *
      *----------------------------------------------------------------*
       0800-LER-SOLICPAB               SECTION.
      *----------------------------------------------------------------*

           READ SOLICPAB.

           IF  WRK-FS-SOLICPAB         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHV-SOLICPAB
               GO                      TO 0800-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 0300-TESTAR-FS-SOLICPAB.

           MOVE WAFZ-CAGPTO-CTA   OF SOLICPAB  TO WRK-S9-3.
           MOVE WRK-9-3                        TO WRK-AGRUPA-SOLICPAB.
           MOVE WAFZ-CSERVC-TARIF OF SOLICPAB  TO WRK-S9-5.
           MOVE WRK-9-5                        TO WRK-TARIF-SOLICPAB
           MOVE WAFZ-HINCL-REG    OF SOLICPAB  TO WRK-TMSTP-SOLICPAB.

           ADD 1                       TO ACU-LIDOS-SOLICPAB.

      *----------------------------------------------------------------*
       0800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LEITURA DO ARQUIVO DELETPAB                                 *
      *----------------------------------------------------------------*
       0900-LER-DELETPAB               SECTION.
      *----------------------------------------------------------------*

           READ DELETPAB.

           IF  WRK-FS-DELETPAB         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHV-DELETPAB
               GO                      TO 0900-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 0400-TESTAR-FS-DELETPAB.

           MOVE WAF0-CAGPTO-CTA-S   OF DELETPAB  TO WRK-S9-3.
           MOVE WRK-9-3                          TO WRK-AGRUPA-DELETPAB.
           MOVE WAF0-CSERVC-TARIF-S OF DELETPAB  TO WRK-S9-5.
           MOVE WRK-9-5                          TO WRK-TARIF-DELETPAB
           MOVE WAF0-HINCL-REG-S    OF DELETPAB  TO WRK-TMSTP-DELETPAB.

           ADD 1                       TO ACU-LIDOS-DELETPAB.

      *----------------------------------------------------------------*
       0900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL DO PROGRAMA                         *
      *----------------------------------------------------------------*
       1000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           IF  WRK-CHV-SOLICPAB      EQUAL WRK-CHV-DELETPAB
               ADD 1                 TO WRK-ACUMULADOR
               PERFORM 0900-LER-DELETPAB
           ELSE
               IF  WRK-CHV-SOLICPAB  LESS WRK-CHV-DELETPAB
                   IF WRK-ACUMULADOR EQUAL ZEROS
                       ADD 1         TO ACU-DESPREZADOS
                   END-IF
                   PERFORM 1200-GRAVAR-ATUALPAB
                   MOVE ZEROS        TO WRK-ACUMULADOR
                   PERFORM 0800-LER-SOLICPAB
               ELSE
                   PERFORM 0900-LER-DELETPAB
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    GRAVAR ARQUIVO  ATUALPAB                                    *
      *----------------------------------------------------------------*
       1200-GRAVAR-ATUALPAB            SECTION.
      *----------------------------------------------------------------*

           MOVE REG-GFCTWAFZ    OF SOLICPAB
                                TO REG-GFCTWAFZ      OF ATUALPAB.

           MOVE WRK-ACUMULADOR  TO WAFZ-QADSAO-CANCD OF ATUALPAB.

           WRITE REG-GFCTWAFZ   OF ATUALPAB.

           MOVE WRK-GRAVACAO      TO WRK-OPERACAO.

           PERFORM 0600-TESTAR-FS-ATUALPAB.

           ADD 1                  TO ACU-GRAVADOS-ATUALPAB.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA FINALIZACAO DO PROGRAMA                         *
      *----------------------------------------------------------------*
       1300-PROCEDIMENTOS-FINAIS       SECTION.
      *----------------------------------------------------------------*

           PERFORM 1400-EMITIR-DISPLAY.

           CLOSE SOLICPAB
                 DELETPAB
                 ATUALPAB.

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

           DISPLAY '******************** GFCT2403 ********************'.
           DISPLAY '*                                                *'.
           MOVE ACU-LIDOS-SOLICPAB     TO WRK-MASCARA.
           DISPLAY '*  LIDOS EM SOLICPAB......: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-LIDOS-DELETPAB     TO WRK-MASCARA.
           DISPLAY '*  LIDOS EM DELETPAB......: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-GRAVADOS-ATUALPAB  TO WRK-MASCARA.
           DISPLAY '*  GRAVADOS NO ATUALPAB...: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-DESPREZADOS        TO WRK-MASCARA.
           DISPLAY '*  REG. DE SOLICITACOES DE PABS NAO ENCONTRADO   *'.
           DISPLAY '*  NO ARQV. PABS DELETADOS: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           DISPLAY '******************** GFCT2403 ********************'.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAMENTO DE ERRO                              *
      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT2403'             TO ERR-PGM.
           MOVE 'APL'                  TO ERR-TIPO-ACESSO.

           CALL 'BRAD7100'             USING WRK-BATCH
                                             ERRO-AREA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------

