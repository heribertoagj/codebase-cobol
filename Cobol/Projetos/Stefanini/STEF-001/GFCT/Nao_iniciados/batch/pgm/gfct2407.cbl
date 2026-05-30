      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. GFCT2407.
       AUTHOR.     VINICIUS CRISTIAN MADUREIRA.
      *================================================================*
      *      S O N D A  P R O C W O R K  -  C O N S U L T O R I A      *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT2407                                    *
      *    PROGRAMADOR.:   VINICIUS CRISTIAN MADUREIRA - SONDPROC/GP.50*
      *    ANALISTA....:   VINICIUS CRISTIAN MADUREIRA - SONDPROC/GP.50*
      *    DATA........:   10/03/2009                                  *
      *                                                                *
      *    OBJETIVO    :   CONTA REGISTROS DELETADOS PARA ATUALIZACAO  *
      *                    NA BASE.                                    *
      *                                                                *
      *    ARQUIVOS:                                                   *
      *      DDNAME                                    INCLUDE/BOOK    *
      *      SOLICEMP                                    GFCTWAFZ      *
      *      DELETEMP                                    GFCTWAF2      *
      *      ATUALEMP                                    GFCTWAFZ      *
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

           SELECT DELETEMP ASSIGN      TO UT-S-DELETEMP
                      FILE STATUS      IS WRK-FS-DELETEMP.

           SELECT ATUALEMP ASSIGN      TO UT-S-ATUALEMP
                      FILE STATUS      IS WRK-FS-ATUALEMP.

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
      *    INPUT:                                                      *
      *            ORG. SEQUENTIAL     -  LRECL = 150                  *
      *----------------------------------------------------------------*

       FD  DELETEMP
           RECORDING MODE IS F
           LABEL RECORDS IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'GFCTWAF2'.

      *----------------------------------------------------------------*
      *    OUTPUT:                                                     *
      *            ORG. SEQUENTIAL     -  LRECL = 700                  *
      *----------------------------------------------------------------*

       FD  ATUALEMP
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'GFCTWAFZ'.

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  INICIO DA WORKING GFCT2407  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*        ACUMULADORES          *'.
      *----------------------------------------------------------------*

       01  ACU-ACUMULADORES.
           05 ACU-LIDOS-SOLICEMP       PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-LIDOS-DELETEMP       PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-GRAVADOS-ATUALEMP    PIC 9(11) COMP-3    VALUE ZEROS.
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

       01  WRK-CHV-SOLICEMP.
           03 WRK-AGRUPA-SOLICEMP      PIC 9(003)           VALUE ZEROS.
           03 WRK-TARIF-SOLICEMP       PIC 9(005)           VALUE ZEROS.
           03 WRK-TMSTP-SOLICEMP       PIC X(026)           VALUE ZEROS.

       01  WRK-CHV-DELETEMP.
           03 WRK-AGRUPA-DELETEMP      PIC 9(003)           VALUE ZEROS.
           03 WRK-TARIF-DELETEMP       PIC 9(005)           VALUE ZEROS.
           03 WRK-TMSTP-DELETEMP       PIC X(026)           VALUE ZEROS.

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

       01  WRK-FS-SOLICEMP             PIC X(02)           VALUE SPACES.
       01  WRK-FS-DELETEMP             PIC X(02)           VALUE SPACES.
       01  WRK-FS-ATUALEMP             PIC X(02)           VALUE SPACES.
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
           '*   FIM  DA WORKING GFCT2407   *'.
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
                    WRK-CHV-SOLICEMP   EQUAL HIGH-VALUES.

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
                      DELETEMP
               OUTPUT ATUALEMP.

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

           PERFORM 0400-TESTAR-FS-DELETEMP.

           PERFORM 0600-TESTAR-FS-ATUALEMP.

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
      *    TESTAR FILE-STATUS DO ARQUIVO DELETEMP                      *
      *----------------------------------------------------------------*
       0400-TESTAR-FS-DELETEMP         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-DELETEMP         NOT EQUAL '00'
               MOVE 'DELETEMP'         TO WRK-NOME-ARQ
               MOVE WRK-FS-DELETEMP    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO ATUALEMP                      *
      *----------------------------------------------------------------*
       0600-TESTAR-FS-ATUALEMP         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ATUALEMP         NOT EQUAL '00'
               MOVE 'ATUALEMP'         TO WRK-NOME-ARQ
               MOVE WRK-FS-ATUALEMP    TO WRK-FS
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

           IF  WRK-CHV-SOLICEMP        EQUAL HIGH-VALUES
               DISPLAY '*********** GFCT2407 ***********'
               DISPLAY '*                              *'
               DISPLAY '*    ARQUIVO  SOLICEMP VAZIO   *'
               DISPLAY '*    PROCESSAMENTO ENCERRADO   *'
               DISPLAY '*                              *'
               DISPLAY '*********** GFCT2407 ***********'
           END-IF.

           PERFORM 0900-LER-DELETEMP.

           IF  WRK-CHV-DELETEMP            EQUAL HIGH-VALUES
               DISPLAY '*********** GFCT2407 ***********'
               DISPLAY '*                              *'
               DISPLAY '*    ARQUIVO DELETEMP VAZIO    *'
               DISPLAY '*                              *'
               DISPLAY '*********** GFCT2407 ***********'
           END-IF.

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
               MOVE HIGH-VALUES        TO WRK-CHV-SOLICEMP
               GO                      TO 0800-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 0300-TESTAR-FS-SOLICEMP.

           MOVE WAFZ-CAGPTO-CTA   OF SOLICEMP  TO WRK-S9-3.
           MOVE WRK-9-3                        TO WRK-AGRUPA-SOLICEMP.
           MOVE WAFZ-CSERVC-TARIF OF SOLICEMP  TO WRK-S9-5.
           MOVE WRK-9-5                        TO WRK-TARIF-SOLICEMP
           MOVE WAFZ-HINCL-REG    OF SOLICEMP  TO WRK-TMSTP-SOLICEMP.

           ADD 1                       TO ACU-LIDOS-SOLICEMP.

      *----------------------------------------------------------------*
       0800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LEITURA DO ARQUIVO DELETEMP                                 *
      *----------------------------------------------------------------*
       0900-LER-DELETEMP               SECTION.
      *----------------------------------------------------------------*

           READ DELETEMP.

           IF  WRK-FS-DELETEMP         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHV-DELETEMP
               GO                      TO 0900-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 0400-TESTAR-FS-DELETEMP.

           MOVE WAF2-CAGPTO-CTA-S      TO WRK-S9-3.
           MOVE WRK-9-3                TO WRK-AGRUPA-DELETEMP.
           MOVE WAF2-CSERVC-TARIF-S    TO WRK-S9-5.
           MOVE WRK-9-5                TO WRK-TARIF-DELETEMP
           MOVE WAF2-HINCL-REG-S       TO WRK-TMSTP-DELETEMP.

           ADD 1                       TO ACU-LIDOS-DELETEMP.

      *----------------------------------------------------------------*
       0900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL DO PROGRAMA                         *
      *----------------------------------------------------------------*
       1000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           IF  WRK-CHV-SOLICEMP      EQUAL WRK-CHV-DELETEMP
               ADD 1                 TO WRK-ACUMULADOR
               PERFORM 0900-LER-DELETEMP
           ELSE
               IF  WRK-CHV-SOLICEMP  LESS WRK-CHV-DELETEMP
                   IF WRK-ACUMULADOR EQUAL ZEROS
                       ADD 1         TO ACU-DESPREZADOS
                   END-IF
                   PERFORM 1200-GRAVAR-ATUALEMP
                   MOVE ZEROS        TO WRK-ACUMULADOR
                   PERFORM 0800-LER-SOLICEMP
               ELSE
                   PERFORM 0900-LER-DELETEMP
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    GRAVAR ARQUIVO  ATUALEMP                                    *
      *----------------------------------------------------------------*
       1200-GRAVAR-ATUALEMP            SECTION.
      *----------------------------------------------------------------*

           MOVE REG-GFCTWAFZ    OF SOLICEMP
                                TO REG-GFCTWAFZ      OF ATUALEMP.

           MOVE WRK-ACUMULADOR  TO WAFZ-QADSAO-CANCD OF ATUALEMP.

           WRITE REG-GFCTWAFZ   OF ATUALEMP.

           MOVE WRK-GRAVACAO      TO WRK-OPERACAO.

           PERFORM 0600-TESTAR-FS-ATUALEMP.

           ADD 1                  TO ACU-GRAVADOS-ATUALEMP.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA FINALIZACAO DO PROGRAMA                         *
      *----------------------------------------------------------------*
       1300-PROCEDIMENTOS-FINAIS       SECTION.
      *----------------------------------------------------------------*

           PERFORM 1400-EMITIR-DISPLAY.

           CLOSE SOLICEMP
                 DELETEMP
                 ATUALEMP.

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

           DISPLAY '******************** GFCT2407 ********************'.
           DISPLAY '*                                                *'.
           MOVE ACU-LIDOS-SOLICEMP     TO WRK-MASCARA.
           DISPLAY '*  LIDOS EM SOLICEMP......: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-LIDOS-DELETEMP     TO WRK-MASCARA.
           DISPLAY '*  LIDOS EM DELETEMP......: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-GRAVADOS-ATUALEMP  TO WRK-MASCARA.
           DISPLAY '*  GRAVADOS NO ATUALEMP...: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-DESPREZADOS        TO WRK-MASCARA.
           DISPLAY '*  REG. DE SOLICITACOES NAO ENCONTRADO NO        *'.
           DISPLAY '*  ARQUIVO DE  DELETADOS..: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           DISPLAY '******************** GFCT2407 ********************'.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAMENTO DE ERRO                              *
      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT2407'             TO ERR-PGM.
           MOVE 'APL'                  TO ERR-TIPO-ACESSO.

           CALL 'BRAD7100'             USING WRK-BATCH
                                             ERRO-AREA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------

