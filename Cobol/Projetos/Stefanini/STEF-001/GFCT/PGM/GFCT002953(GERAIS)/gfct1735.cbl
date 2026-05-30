      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. GFCT1735.
       AUTHOR.     VINICIUS CRISTIAN MADUREIRA.
      *================================================================*
      *      S O N D A  P R O C W O R K  -  C O N S U L T O R I A      *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT1735                                    *
      *    PROGRAMADOR.:   VINICIUS CRISTIAN MADUREIRA - SONDPROC/GP.50*
      *    ANALISTA....:   VINICIUS CRISTIAN MADUREIRA - SONDPROC/GP.50*
      *    DATA........:   12/06/2008                                  *
      *                                                                *
      *    OBJETIVO    :   INCLUI CPF NO ARQUIVO DE ACIONISTAS.        *
      *                                                                *
      *    ARQUIVOS:                                                   *
      *      DDNAME                                    INCLUDE/BOOK    *
      *      SELECAO4                                    I#GFCT61      *
      *      ACOEBBDC                                                  *
      *      ACOESCPF                                    I#GFCTTA      *
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

           SELECT SELECAO4 ASSIGN      TO UT-S-SELECAO4
                      FILE STATUS      IS WRK-FS-SELECAO4.

           SELECT ACOEBBDC ASSIGN      TO UT-S-ACOEBBDC
                      FILE STATUS      IS WRK-FS-ACOEBBDC.

           SELECT ACOESCPF ASSIGN      TO UT-S-ACOESCPF
                      FILE STATUS      IS WRK-FS-ACOESCPF.

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

       FD  SELECAO4
           RECORDING MODE IS F
           LABEL RECORDS IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCT61'.

      *----------------------------------------------------------------*
      *    INPUT:                                                      *
      *            ORG. SEQUENTIAL     -  LRECL = 018                  *
      *----------------------------------------------------------------*

       FD  ACOEBBDC
           RECORDING MODE IS F
           LABEL RECORDS IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-ACOES.
           03  FD-ACOES-AGENCIA       PIC 9(05) COMP-3.
           03  FD-ACOES-RAZAO         PIC 9(05) COMP-3.
           03  FD-ACOES-CONTA         PIC 9(07) COMP-3.
           03  FD-ACOES-QT-ACOES      PIC 9(14) COMP-3.

      *----------------------------------------------------------------*
      *    OUTPUT:                                                     *
      *            ORG. SEQUENTIAL     -  LRECL = 040                  *
      *----------------------------------------------------------------*

       FD  ACOESCPF
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTTA'.

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  INICIO DA WORKING GFCT1735  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*        ACUMULADORES          *'.
      *----------------------------------------------------------------*

       01  ACU-ACUMULADORES.
           05 ACU-LIDOS-SELECAO4       PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-LIDOS-ACOEBBDC       PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-GRAVADOS-ACOESCPF    PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-DESPREZADOS          PIC 9(11) COMP-3    VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    VARIAVEIS AUXILIARES      *'.
      *----------------------------------------------------------------*

       01  WRK-AUXILIARES.
           05 WRK-BATCH                PIC X(08)           VALUE
              'BATCH'.
           05 WRK-MASCARA              PIC ZZ.ZZZ.ZZZ.Z99   VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*       AREA PARA CHAVES       *'.
      *----------------------------------------------------------------*

       01  WRK-CONTA                   PIC  9(013)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-CONTA.
           05  FILLER                  PIC  9(006).
           05  WRK-GFCT-CONTA          PIC  9(007).

       01  WRK-CHV-SELECAO4.
           05 WRK-AG-SELECAO4        PIC 9(05)           VALUE ZEROS.
           05 WRK-CTA-SELECAO4       PIC 9(07)           VALUE ZEROS.

       01  WRK-CHV-ACOEBBDC.
           05 WRK-AG-ACOEBBDC        PIC 9(05)           VALUE ZEROS.
           05 WRK-CTA-ACOEBBDC       PIC 9(07)           VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    AREA PARA FILE STATUS     *'.
      *----------------------------------------------------------------*

       01  WRK-FS-SELECAO4             PIC X(02)           VALUE SPACES.
       01  WRK-FS-ACOEBBDC             PIC X(02)           VALUE SPACES.
       01  WRK-FS-ACOESCPF             PIC X(02)           VALUE SPACES.
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
           '*   FIM  DA WORKING GFCT1735   *'.
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
                    WRK-CHV-SELECAO4   EQUAL HIGH-VALUES.

           PERFORM 1300-PROCEDIMENTOS-FINAIS.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA INICIALIZAR O PROGRAMA                          *
      *----------------------------------------------------------------*
       0100-INICIALIZAR-PROGRAMA       SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT SELECAO4
                      ACOEBBDC
               OUTPUT ACOESCPF.

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

           PERFORM 0300-TESTAR-FS-SELECAO4.

           PERFORM 0400-TESTAR-FS-ACOEBBDC.

           PERFORM 0500-TESTAR-FS-ACOESCPF.

      *----------------------------------------------------------------*
       0200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO SELECAO4                      *
      *----------------------------------------------------------------*
       0300-TESTAR-FS-SELECAO4         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SELECAO4         NOT EQUAL '00'
               MOVE 'SELECAO4'         TO WRK-NOME-ARQ
               MOVE WRK-FS-SELECAO4    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO ACOEBBDC                      *
      *----------------------------------------------------------------*
       0400-TESTAR-FS-ACOEBBDC         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ACOEBBDC         NOT EQUAL '00'
               MOVE 'ACOEBBDC'         TO WRK-NOME-ARQ
               MOVE WRK-FS-ACOEBBDC    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO ACOESCPF                      *
      *----------------------------------------------------------------*
       0500-TESTAR-FS-ACOESCPF         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ACOESCPF         NOT EQUAL '00'
               MOVE 'ACOESCPF'         TO WRK-NOME-ARQ
               MOVE WRK-FS-ACOESCPF    TO WRK-FS
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

           PERFORM 0800-LER-SELECAO4.

           IF  WRK-CHV-SELECAO4        EQUAL HIGH-VALUES
               DISPLAY '*********** GFCT1735 ***********'
               DISPLAY '*                              *'
               DISPLAY '*    ARQUIVO  SELECAO4 VAZIO   *'
               DISPLAY '*    PROCESSAMENTO ENCERRADO   *'
               DISPLAY '*                              *'
               DISPLAY '*********** GFCT1735 ***********'
           END-IF.

           PERFORM 0900-LER-ACOEBBDC.

           IF  WRK-CHV-ACOEBBDC            EQUAL HIGH-VALUES
               DISPLAY '*********** GFCT1735 ***********'
               DISPLAY '*                              *'
               DISPLAY '*    ARQUIVO ACOEBBDC VAZIO    *'
               DISPLAY '*                              *'
               DISPLAY '*********** GFCT1735 ***********'
           END-IF.

      *----------------------------------------------------------------*
       0700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LEITURA DO ARQUIVO SELECAO4                                 *
      *----------------------------------------------------------------*
       0800-LER-SELECAO4               SECTION.
      *----------------------------------------------------------------*

           READ SELECAO4.

           IF  WRK-FS-SELECAO4         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHV-SELECAO4
               GO                      TO 0800-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 0300-TESTAR-FS-SELECAO4.

           MOVE GFCT61-AGENCIA TO WRK-AG-SELECAO4.
           MOVE GFCT61-CONTA   TO WRK-CONTA.
           MOVE WRK-GFCT-CONTA TO WRK-CTA-SELECAO4.

           ADD 1                       TO ACU-LIDOS-SELECAO4.

      *----------------------------------------------------------------*
       0800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LEITURA DO ARQUIVO ACOEBBDC                                 *
      *----------------------------------------------------------------*
       0900-LER-ACOEBBDC               SECTION.
      *----------------------------------------------------------------*

           READ ACOEBBDC.

           IF  WRK-FS-ACOEBBDC         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHV-ACOEBBDC
               GO                      TO 0900-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 0400-TESTAR-FS-ACOEBBDC.

           MOVE FD-ACOES-AGENCIA          TO WRK-AG-ACOEBBDC.
           MOVE FD-ACOES-CONTA            TO WRK-CTA-ACOEBBDC.

           ADD 1                       TO ACU-LIDOS-ACOEBBDC.

      *----------------------------------------------------------------*
       0900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL DO PROGRAMA                         *
      *----------------------------------------------------------------*
       1000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           IF  WRK-CHV-SELECAO4      EQUAL WRK-CHV-ACOEBBDC
               PERFORM 1100-GRAVAR-ACOESCPF
               PERFORM 0800-LER-SELECAO4
           ELSE
               IF  WRK-CHV-SELECAO4  LESS WRK-CHV-ACOEBBDC
                   ADD 1 TO ACU-DESPREZADOS
                   PERFORM 0800-LER-SELECAO4
               ELSE
                   PERFORM 0900-LER-ACOEBBDC
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    GRAVAR ARQUIVO  ACOESCPF                                    *
      *----------------------------------------------------------------*
       1100-GRAVAR-ACOESCPF            SECTION.
      *----------------------------------------------------------------*

           MOVE FD-ACOES-AGENCIA  TO GFCTTA-AGENCIA.
           MOVE FD-ACOES-RAZAO    TO GFCTTA-RAZAO.
           MOVE FD-ACOES-CONTA    TO GFCTTA-CONTA.
           MOVE FD-ACOES-QT-ACOES TO GFCTTA-QT-ACOES.
           MOVE GFCT61-CPF-NUM    TO GFCTTA-CPF-NUM.
           MOVE GFCT61-CPF-CTR    TO GFCTTA-CPF-CTR.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           WRITE REG-GFCTTA OF ACOESCPF.

           ADD 1                       TO ACU-GRAVADOS-ACOESCPF.

           PERFORM 0500-TESTAR-FS-ACOESCPF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA FINALIZACAO DO PROGRAMA                         *
      *----------------------------------------------------------------*
       1300-PROCEDIMENTOS-FINAIS       SECTION.
      *----------------------------------------------------------------*

           PERFORM 1400-EMITIR-DISPLAY.

           CLOSE SELECAO4
                 ACOEBBDC
                 ACOESCPF.

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

           DISPLAY '******************** GFCT1735 ********************'.
           DISPLAY '*                                                *'.
           MOVE ACU-LIDOS-SELECAO4     TO WRK-MASCARA.
           DISPLAY '*  LIDOS EM SELECAO4......: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-LIDOS-ACOEBBDC     TO WRK-MASCARA.
           DISPLAY '*  LIDOS EM ACOEBBDC......: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-GRAVADOS-ACOESCPF  TO WRK-MASCARA.
           DISPLAY '*  GRAVADOS NO ACOESCPF...: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-DESPREZADOS        TO WRK-MASCARA.
           DISPLAY '*  DESPREZADOS............: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           DISPLAY '******************** GFCT1735 ********************'.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAMENTO DE ERRO                              *
      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT1735'             TO ERR-PGM.
           MOVE 'APL'                  TO ERR-TIPO-ACESSO.

           CALL 'BRAD7100'             USING WRK-BATCH
                                             ERRO-AREA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------
