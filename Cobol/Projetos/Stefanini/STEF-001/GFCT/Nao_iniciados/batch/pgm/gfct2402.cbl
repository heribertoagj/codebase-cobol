      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. GFCT2402.
       AUTHOR.     VINICIUS CRISTIAN MADUREIRA.
      *================================================================*
      *      S O N D A  P R O C W O R K  -  C O N S U L T O R I A      *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT2402                                    *
      *    PROGRAMADOR.:   VINICIUS CRISTIAN MADUREIRA - SONDPROC/GP.50*
      *    ANALISTA....:   VINICIUS CRISTIAN MADUREIRA - SONDPROC/GP.50*
      *    DATA........:   23/03/2009                                  *
      *                                                                *
      *    OBJETIVO    :   EXCLUI CLIENTES DE OUTROS PABS.             *
      *                                                                *
      *    ARQUIVOS:                                                   *
      *      DDNAME                                    INCLUDE/BOOK    *
      *      SOLICPAB                                    GFCTWAFZ      *
      *      CLICESTA                                    GFCTWAF0      *
      *      DELETPAB                                    GFCTWAF0      *
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

           SELECT CLICESTA ASSIGN      TO UT-S-CLICESTA
                      FILE STATUS      IS WRK-FS-CLICESTA.

           SELECT DELETPAB ASSIGN      TO UT-S-DELETPAB
                      FILE STATUS      IS WRK-FS-DELETPAB.

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

       FD  CLICESTA
           RECORDING MODE IS F
           LABEL RECORDS IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'GFCTWAF0'.

      *----------------------------------------------------------------*
      *    OUTPUT:                                                     *
      *            ORG. SEQUENTIAL     -  LRECL = 150                  *
      *----------------------------------------------------------------*

       FD  DELETPAB
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'GFCTWAF0'.

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  INICIO DA WORKING GFCT2402  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*        ACUMULADORES          *'.
      *----------------------------------------------------------------*

       01  ACU-ACUMULADORES.
           05 ACU-LIDOS-SOLICPAB       PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-LIDOS-CLICESTA       PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-GRAVADOS-DELETPAB    PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-DESPREZADOS          PIC 9(11) COMP-3    VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    VARIAVEIS AUXILIARES      *'.
      *----------------------------------------------------------------*

       01  WRK-AUXILIARES.
           05 WRK-BATCH                PIC X(08)            VALUE
              'BATCH'.
           05 WRK-MASCARA              PIC ZZ.ZZZ.ZZZ.Z99   VALUE ZEROS.

       01  WRK-CHV-SOLICPAB.
           03 WRK-TARIFA-SOLICPAB      PIC 9(005)           VALUE ZEROS.
           03 WRK-AGENCIA-SOLICPAB     PIC 9(005)           VALUE ZEROS.
           03 WRK-POSTO-SOLICPAB       PIC 9(005)           VALUE ZEROS.

       01  WRK-CHV-CLICESTA.
           03 WRK-TARIFA-CLICESTA      PIC 9(005)           VALUE ZEROS.
           03 WRK-AGENCIA-CLICESTA     PIC 9(005)           VALUE ZEROS.
           03 WRK-POSTO-CLICESTA       PIC 9(005)           VALUE ZEROS.

       01  WRK-S9-5                    PIC S9(005)          VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-S9-5.
           05 WRK-9-5                  PIC  9(005).

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    AREA PARA FILE STATUS     *'.
      *----------------------------------------------------------------*

       01  WRK-FS-SOLICPAB             PIC X(02)           VALUE SPACES.
       01  WRK-FS-CLICESTA             PIC X(02)           VALUE SPACES.
       01  WRK-FS-DELETPAB             PIC X(02)           VALUE SPACES.
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
           '*   FIM  DA WORKING GFCT2402   *'.
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
                    WRK-CHV-CLICESTA   EQUAL HIGH-VALUES.

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
                      CLICESTA
               OUTPUT DELETPAB.

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

           PERFORM 0400-TESTAR-FS-CLICESTA.

           PERFORM 0600-TESTAR-FS-DELETPAB.

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
      *    TESTAR FILE-STATUS DO ARQUIVO CLICESTA                      *
      *----------------------------------------------------------------*
       0400-TESTAR-FS-CLICESTA         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-CLICESTA         NOT EQUAL '00'
               MOVE 'CLICESTA'         TO WRK-NOME-ARQ
               MOVE WRK-FS-CLICESTA    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO DELETPAB                      *
      *----------------------------------------------------------------*
       0600-TESTAR-FS-DELETPAB         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-DELETPAB         NOT EQUAL '00'
               MOVE 'DELETPAB'         TO WRK-NOME-ARQ
               MOVE WRK-FS-DELETPAB    TO WRK-FS
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
               DISPLAY '*********** GFCT2402 ***********'
               DISPLAY '*                              *'
               DISPLAY '*    ARQUIVO  SOLICPAB VAZIO   *'
               DISPLAY '*    PROCESSAMENTO ENCERRADO   *'
               DISPLAY '*                              *'
               DISPLAY '*********** GFCT2402 ***********'
           END-IF.

           PERFORM 0900-LER-CLICESTA.

           IF  WRK-CHV-CLICESTA            EQUAL HIGH-VALUES
               DISPLAY '*********** GFCT2402 ***********'
               DISPLAY '*                              *'
               DISPLAY '*    ARQUIVO CLICESTA VAZIO    *'
               DISPLAY '*                              *'
               DISPLAY '*********** GFCT2402 ***********'
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

           MOVE WAFZ-AGENCIA      OF SOLICPAB  TO WRK-S9-5.
           MOVE WRK-9-5                        TO WRK-AGENCIA-SOLICPAB.
           MOVE WAFZ-CSERVC-TARIF OF SOLICPAB  TO WRK-S9-5.
           MOVE WRK-9-5                        TO WRK-TARIFA-SOLICPAB.
           MOVE WAFZ-POSTO        OF SOLICPAB  TO WRK-S9-5.
           MOVE WRK-9-5                        TO WRK-POSTO-SOLICPAB.

           ADD 1                       TO ACU-LIDOS-SOLICPAB.

      *----------------------------------------------------------------*
       0800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LEITURA DO ARQUIVO CLICESTA                                 *
      *----------------------------------------------------------------*
       0900-LER-CLICESTA               SECTION.
      *----------------------------------------------------------------*

           READ CLICESTA.

           IF  WRK-FS-CLICESTA         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHV-CLICESTA
               GO                      TO 0900-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 0400-TESTAR-FS-CLICESTA.

           MOVE WAF0-CJUNC-DEPDC  OF CLICESTA TO WRK-S9-5.
           MOVE WRK-9-5                       TO WRK-AGENCIA-CLICESTA.
           MOVE WAF0-CSERVC-TARIF OF CLICESTA TO WRK-S9-5.
           MOVE WRK-9-5                       TO WRK-TARIFA-CLICESTA.
           MOVE WAF0-PAB-CLI      OF CLICESTA TO WRK-S9-5.
           MOVE WRK-9-5                       TO WRK-POSTO-CLICESTA.

           ADD 1                       TO ACU-LIDOS-CLICESTA.

      *----------------------------------------------------------------*
       0900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL DO PROGRAMA                         *
      *----------------------------------------------------------------*
       1000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           IF  WRK-CHV-CLICESTA EQUAL WRK-CHV-SOLICPAB
               PERFORM 1200-GRAVAR-DELETPAB
               PERFORM 0900-LER-CLICESTA
           ELSE
               IF  WRK-CHV-CLICESTA LESS WRK-CHV-SOLICPAB
                   ADD 1       TO ACU-DESPREZADOS
                   PERFORM 0900-LER-CLICESTA
               ELSE
                   PERFORM 0800-LER-SOLICPAB
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    GRAVAR ARQUIVO  DELETPAB                                    *
      *----------------------------------------------------------------*
       1200-GRAVAR-DELETPAB            SECTION.
      *----------------------------------------------------------------*

           MOVE REG-WAF0 OF CLICESTA TO REG-WAF0            OF DELETPAB.

           MOVE WAFZ-CAGPTO-CTA      TO WAF0-CAGPTO-CTA-S   OF DELETPAB.
           MOVE WAFZ-CSERVC-TARIF    TO WAF0-CSERVC-TARIF-S OF DELETPAB.
           MOVE WAFZ-HINCL-REG       TO WAF0-HINCL-REG-S    OF DELETPAB.
           MOVE WAFZ-POSTO           TO WAF0-PAB-SOLIC      OF DELETPAB.

           WRITE REG-WAF0  OF DELETPAB.

           MOVE WRK-GRAVACAO      TO WRK-OPERACAO.

           PERFORM 0600-TESTAR-FS-DELETPAB.

           ADD 1                  TO ACU-GRAVADOS-DELETPAB.

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
                 CLICESTA
                 DELETPAB.

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

           DISPLAY '******************** GFCT2402 ********************'.
           DISPLAY '*                                                *'.
           MOVE ACU-LIDOS-SOLICPAB     TO WRK-MASCARA.
           DISPLAY '*  LIDOS EM SOLICPAB......: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-LIDOS-CLICESTA     TO WRK-MASCARA.
           DISPLAY '*  LIDOS EM CLICESTA......: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-GRAVADOS-DELETPAB  TO WRK-MASCARA.
           DISPLAY '*  GRAVADOS NO DELETPAB...: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-DESPREZADOS        TO WRK-MASCARA.
           DISPLAY '*  REGISTRO DE CESTAS NAO ENCONTRADO NO ARQVO.   *'.
           DISPLAY '*  DE SOLIC. PABS.........: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           DISPLAY '******************** GFCT2402 ********************'.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAMENTO DE ERRO                              *
      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT2402'             TO ERR-PGM.
           MOVE 'APL'                  TO ERR-TIPO-ACESSO.

           CALL 'BRAD7100'             USING WRK-BATCH
                                             ERRO-AREA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------


