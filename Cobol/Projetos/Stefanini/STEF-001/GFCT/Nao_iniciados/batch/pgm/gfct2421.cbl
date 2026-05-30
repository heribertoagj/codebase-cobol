      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. GFCT2421.
       AUTHOR.     VINICIUS CRISTIAN MADUREIRA.
      *================================================================*
      *      S O N D A  P R O C W O R K  -  C O N S U L T O R I A      *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT2421                                    *
      *    PROGRAMADOR.:   VINICIUS CRISTIAN MADUREIRA - SONDPROC/GP.50*
      *    ANALISTA....:   VINICIUS CRISTIAN MADUREIRA - SONDPROC/GP.50*
      *    DATA........:   17/03/2009                                  *
      *                                                                *
      *    OBJETIVO    :   ACUMULA SOLICITACOES POR EMPRESA.           *
      *                                                                *
      *    ARQUIVOS:                                                   *
      *      DDNAME                                    INCLUDE/BOOK    *
      *      SOLICEMP                                    GFCTWAFZ      *
      *      SOLIEMP1                                    GFCTWAGL      *
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

           SELECT SOLICEMP ASSIGN      TO UT-S-SOLICEMP
                      FILE STATUS      IS WRK-FS-SOLICEMP.

           SELECT SOLIEMP1 ASSIGN      TO UT-S-SOLIEMP1
                      FILE STATUS      IS WRK-FS-SOLIEMP1.

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

       FD  SOLICEMP
           RECORDING MODE IS F
           LABEL RECORDS IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'GFCTWAFZ'.

      *----------------------------------------------------------------*
      *    OUTPUT:                                                     *
      *            ORG. SEQUENTIAL     -  LRECL = 950                  *
      *----------------------------------------------------------------*

       FD  SOLIEMP1
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'GFCTWAGL'.

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  INICIO DA WORKING GFCT2421  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*        ACUMULADORES          *'.
      *----------------------------------------------------------------*

       01  ACU-ACUMULADORES.
           05 ACU-LIDOS-SOLICEMP       PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-GRAVADOS-SOLIEMP1    PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-DESPREZADOS          PIC 9(11) COMP-3    VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    VARIAVEIS AUXILIARES      *'.
      *----------------------------------------------------------------*

       01  WRK-AUXILIARES.
           05 WRK-BATCH                PIC X(08)            VALUE
              'BATCH'.
           05 WRK-MASCARA              PIC ZZ.ZZZ.ZZZ.Z99   VALUE ZEROS.

       01  WRK-SOLICEMP-ATU.
           03 WRK-AGENCIA-ATU          PIC 9(005)           VALUE ZEROS.
           03 WRK-CONTA-ATU            PIC 9(007)           VALUE ZEROS.

       01  WRK-SOLICEMP-ANT.
           03 WRK-AGENCIA-ANT          PIC 9(005)           VALUE ZEROS.
           03 WRK-CONTA-ANT            PIC 9(007)           VALUE ZEROS.

       01  WRK-S9-13                   PIC S9(013)          VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-S9-13.
           05 FILLER                   PIC  9(006).
           05 WRK-9-7                  PIC  9(007).

       01  WRK-S9-5                    PIC S9(005)          VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-S9-5.
           05 WRK-9-5                  PIC  9(005).

       01  WRK-WAGL-REGS               PIC S9(003) COMP-3   VALUE ZEROS.
       01  WRK-PRIMEIRA-LEITURA        PIC  X(001)          VALUE 'S'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    AREA PARA FILE STATUS     *'.
      *----------------------------------------------------------------*

       01  WRK-FS-SOLICEMP             PIC X(02)           VALUE SPACES.
       01  WRK-FS-SOLIEMP1             PIC X(02)           VALUE SPACES.
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
           '*   FIM  DA WORKING GFCT2421   *'.
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

           PERFORM 1000-PROCESSAR UNTIL WRK-FS-SOLICEMP EQUAL '10'.

           PERFORM 1300-PROCEDIMENTOS-FINAIS.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA INICIALIZAR O PROGRAMA                          *
      *----------------------------------------------------------------*
       0100-INICIALIZAR-PROGRAMA       SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT SOLICEMP
               OUTPUT SOLIEMP1.

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

           PERFORM 0300-TESTAR-FS-SOLICEMP.

           PERFORM 0600-TESTAR-FS-SOLIEMP1.

      *----------------------------------------------------------------*
       0200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO SOLICEMP                      *
      *----------------------------------------------------------------*
       0300-TESTAR-FS-SOLICEMP         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SOLICEMP         NOT EQUAL '00'
               MOVE 'SOLICEMP'         TO WRK-NOME-ARQ
               MOVE WRK-FS-SOLICEMP    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO SOLIEMP1                      *
      *----------------------------------------------------------------*
       0600-TESTAR-FS-SOLIEMP1         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SOLIEMP1         NOT EQUAL '00'
               MOVE 'SOLIEMP1'         TO WRK-NOME-ARQ
               MOVE WRK-FS-SOLIEMP1    TO WRK-FS
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

           PERFORM 0800-LER-SOLICEMP.

           IF  WRK-FS-SOLICEMP        EQUAL '10'
               DISPLAY '*********** GFCT2421 ***********'
               DISPLAY '*                              *'
               DISPLAY '*    ARQUIVO  SOLICEMP VAZIO   *'
               DISPLAY '*    PROCESSAMENTO ENCERRADO   *'
               DISPLAY '*                              *'
               DISPLAY '*********** GFCT2421 ***********'
           END-IF.

           MOVE WAFZ-AGENCIA         TO WAGL-AGENCIA.
           MOVE WAFZ-CONTA           TO WAGL-CONTA.
           MOVE 1                    TO WRK-WAGL-REGS.
           MOVE WRK-WAGL-REGS        TO WAGL-REGS.
           MOVE WAFZ-CAGPTO-CTA      TO WAGL-AGPTO-CTA(WAGL-REGS).
           MOVE WAFZ-CSERVC-TARIF    TO WAGL-CSERVC-TARIF(WAGL-REGS).
           MOVE WAFZ-HINCL-REG       TO WAGL-HINCL-REG(WAGL-REGS).
           MOVE WRK-SOLICEMP-ATU     TO WRK-SOLICEMP-ANT.

      *----------------------------------------------------------------*
       0700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LEITURA DO ARQUIVO SOLICEMP                                 *
      *----------------------------------------------------------------*
       0800-LER-SOLICEMP               SECTION.
      *----------------------------------------------------------------*

           READ SOLICEMP.

           IF  WRK-FS-SOLICEMP         EQUAL '10'
               GO                      TO 0800-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 0300-TESTAR-FS-SOLICEMP.

           MOVE WAFZ-AGENCIA           TO WRK-S9-5.
           MOVE WRK-9-5                TO WRK-AGENCIA-ATU.
           MOVE WAFZ-CONTA             TO WRK-S9-13.
           MOVE WRK-9-7                TO WRK-CONTA-ATU.

           ADD 1                       TO ACU-LIDOS-SOLICEMP.

      *----------------------------------------------------------------*
       0800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL DO PROGRAMA                         *
      *----------------------------------------------------------------*
       1000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           IF WRK-SOLICEMP-ATU NOT EQUAL WRK-SOLICEMP-ANT
               PERFORM 1200-GRAVAR-SOLIEMP1
               INITIALIZE                   REG-GFCTWAGL
               MOVE 1                    TO WRK-WAGL-REGS
               MOVE WRK-WAGL-REGS        TO WAGL-REGS
               MOVE WAFZ-AGENCIA         TO WAGL-AGENCIA
               MOVE WAFZ-CONTA           TO WAGL-CONTA
               MOVE WAFZ-CAGPTO-CTA      TO WAGL-AGPTO-CTA(WAGL-REGS)
               MOVE WAFZ-CSERVC-TARIF    TO WAGL-CSERVC-TARIF(WAGL-REGS)
               MOVE WAFZ-HINCL-REG       TO WAGL-HINCL-REG(WAGL-REGS)
               MOVE WRK-SOLICEMP-ATU     TO WRK-SOLICEMP-ANT
           ELSE
               IF WRK-SOLICEMP-ATU EQUAL WRK-SOLICEMP-ANT
                   IF WRK-PRIMEIRA-LEITURA EQUAL 'N'
                       ADD  1                    TO WRK-WAGL-REGS
                       MOVE WRK-WAGL-REGS        TO WAGL-REGS
                   END-IF
                   IF WAGL-REGS GREATER  30
                       DISPLAY 'MAIS DE 30 TARIFAS SOLICITADAS PARA '
                               'A MESMA EMPRESA'
                       DISPLAY 'SOLICITACAO NAO PODE SER ATENDIDA '
                               'NESSE PROCESSAMENTO'
                       DISPLAY  WAFZ-AGENCIA      '/'
                                WAFZ-CONTA        '/'
                                WAFZ-CAGPTO-CTA   '/'
                                WAFZ-CSERVC-TARIF '/'
                                WAFZ-HINCL-REG
                       DISPLAY ' '
                       MOVE 30            TO WRK-WAGL-REGS
                       MOVE WRK-WAGL-REGS TO WAGL-REGS
                       ADD 1              TO ACU-DESPREZADOS
                   ELSE
                       MOVE WAFZ-CAGPTO-CTA
                                         TO WAGL-AGPTO-CTA(WAGL-REGS)
                       MOVE WAFZ-CSERVC-TARIF
                                         TO WAGL-CSERVC-TARIF(WAGL-REGS)
                       MOVE WAFZ-HINCL-REG
                                         TO WAGL-HINCL-REG(WAGL-REGS)
                   END-IF
                   MOVE 'N'              TO WRK-PRIMEIRA-LEITURA
               END-IF
           END-IF.

           PERFORM 0800-LER-SOLICEMP.

      *----------------------------------------------------------------*
       1000-99-FIM.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    GRAVAR ARQUIVO  SOLIEMP1                                    *
      *----------------------------------------------------------------*
       1200-GRAVAR-SOLIEMP1            SECTION.
      *----------------------------------------------------------------*

           WRITE REG-GFCTWAGL OF SOLIEMP1.

           MOVE WRK-GRAVACAO      TO WRK-OPERACAO.

           PERFORM 0600-TESTAR-FS-SOLIEMP1.

           ADD 1                  TO ACU-GRAVADOS-SOLIEMP1.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA FINALIZACAO DO PROGRAMA                         *
      *----------------------------------------------------------------*
       1300-PROCEDIMENTOS-FINAIS       SECTION.
      *----------------------------------------------------------------*

           PERFORM 1200-GRAVAR-SOLIEMP1.

           PERFORM 1400-EMITIR-DISPLAY.

           CLOSE SOLICEMP
                 SOLIEMP1.

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

           DISPLAY '******************** GFCT2421 ********************'.
           DISPLAY '*                                                *'.
           MOVE ACU-LIDOS-SOLICEMP     TO WRK-MASCARA.
           DISPLAY '*  LIDOS EM SOLICEMP......: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-GRAVADOS-SOLIEMP1  TO WRK-MASCARA.
           DISPLAY '*  GRAVADOS NO SOLIEMP1...: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-DESPREZADOS        TO WRK-MASCARA.
           DISPLAY '*  SOLICITACOES NAO PROCESSADAS POR ULTRAPASSAR  *'.
           DISPLAY '*  LIMITE DE 30 SOLICIT. PARA A MESMA EMPRESA    *'.
           DISPLAY '*  QTDE. DE SOLICI........: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           DISPLAY '******************** GFCT2421 ********************'.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAMENTO DE ERRO                              *
      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT2421'             TO ERR-PGM.
           MOVE 'APL'                  TO ERR-TIPO-ACESSO.

           CALL 'BRAD7100'             USING WRK-BATCH
                                             ERRO-AREA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------


