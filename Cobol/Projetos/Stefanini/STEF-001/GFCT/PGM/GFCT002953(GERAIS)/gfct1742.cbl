      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. GFCT1742.
       AUTHOR.     VINICIUS CRISTIAN MADUREIRA.
      *================================================================*
      *      S O N D A  P R O C W O R K  -  C O N S U L T O R I A      *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT1742                                    *
      *    PROGRAMADOR.:   VINICIUS CRISTIAN MADUREIRA - SONDPROC/GP.50*
      *    ANALISTA....:   VINICIUS CRISTIAN MADUREIRA - SONDPROC/GP.50*
      *    DATA........:   21/06/2008                                  *
      *                                                                *
      *    OBJETIVO    :   CONFRONTAR RESP COM CLIENTES A COBRAR       *
      *                    (AG/CTA) E INCLUIR CPF NO RESP.             *
      *                                                                *
      *    ARQUIVOS:                                                   *
      *      DDNAME                                    INCLUDE/BOOK    *
      *      ARQRESP4                                    I#GFCTTC      *
      *      SELECAO4                                    I#GFCT61      *
      *      ARQRESP5                                    I#GFCTU7      *
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

           SELECT ARQRESP4 ASSIGN      TO UT-S-ARQRESP4
                      FILE STATUS      IS WRK-FS-ARQRESP4.

           SELECT SELECAO4 ASSIGN      TO UT-S-SELECAO4
                      FILE STATUS      IS WRK-FS-SELECAO4.

           SELECT ARQRESP5 ASSIGN      TO UT-S-ARQRESP5
                      FILE STATUS      IS WRK-FS-ARQRESP5.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:                                                      *
      *            ORG. SEQUENTIAL     -  LRECL = 020                  *
      *----------------------------------------------------------------*

       FD  ARQRESP4
           RECORDING MODE IS F
           LABEL RECORDS IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTTC'.

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
      *    OUTPUT:                                                     *
      *            ORG. SEQUENTIAL     -  LRECL = 030                  *
      *----------------------------------------------------------------*

       FD  ARQRESP5
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTU7'.

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  INICIO DA WORKING GFCT1742  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*        ACUMULADORES          *'.
      *----------------------------------------------------------------*

       01  ACU-ACUMULADORES.
           05 ACU-LIDOS-ARQRESP4       PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-LIDOS-SELECAO4       PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-GRAVADOS-ARQRESP5    PIC 9(11) COMP-3    VALUE ZEROS.
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

       01  WRK-CHV-ARQRESP4.
           05 WRK-AG-ARQRESP4        PIC 9(05)           VALUE ZEROS.
           05 WRK-CTA-ARQRESP4       PIC 9(07)           VALUE ZEROS.

       01  WRK-CHV-SELECAO4.
           05 WRK-AG-SELECAO4        PIC 9(05)           VALUE ZEROS.
           05 WRK-CTA-SELECAO4       PIC 9(07)           VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    AREA PARA FILE STATUS     *'.
      *----------------------------------------------------------------*

       01  WRK-FS-ARQRESP4             PIC X(02)           VALUE SPACES.
       01  WRK-FS-SELECAO4             PIC X(02)           VALUE SPACES.
       01  WRK-FS-ARQRESP5             PIC X(02)           VALUE SPACES.
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
           '*   FIM  DA WORKING GFCT1742   *'.
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
                    WRK-CHV-ARQRESP4   EQUAL HIGH-VALUES.

           PERFORM 1300-PROCEDIMENTOS-FINAIS.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA INICIALIZAR O PROGRAMA                          *
      *----------------------------------------------------------------*
       0100-INICIALIZAR-PROGRAMA       SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT ARQRESP4
                      SELECAO4
               OUTPUT ARQRESP5.

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

           PERFORM 0300-TESTAR-FS-ARQRESP4.

           PERFORM 0400-TESTAR-FS-SELECAO4.

           PERFORM 0500-TESTAR-FS-ARQRESP5.

      *----------------------------------------------------------------*
       0200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO ARQRESP4                      *
      *----------------------------------------------------------------*
       0300-TESTAR-FS-ARQRESP4         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ARQRESP4         NOT EQUAL '00'
               MOVE 'ARQRESP4'         TO WRK-NOME-ARQ
               MOVE WRK-FS-ARQRESP4    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO SELECAO4                      *
      *----------------------------------------------------------------*
       0400-TESTAR-FS-SELECAO4         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SELECAO4         NOT EQUAL '00'
               MOVE 'SELECAO4'         TO WRK-NOME-ARQ
               MOVE WRK-FS-SELECAO4    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO ARQRESP5                      *
      *----------------------------------------------------------------*
       0500-TESTAR-FS-ARQRESP5         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ARQRESP5         NOT EQUAL '00'
               MOVE 'ARQRESP5'         TO WRK-NOME-ARQ
               MOVE WRK-FS-ARQRESP5    TO WRK-FS
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

           PERFORM 0800-LER-ARQRESP4.

           IF  WRK-CHV-ARQRESP4        EQUAL HIGH-VALUES
               DISPLAY '*********** GFCT1742 ***********'
               DISPLAY '*                              *'
               DISPLAY '*    ARQUIVO  ARQRESP4 VAZIO   *'
               DISPLAY '*    PROCESSAMENTO ENCERRADO   *'
               DISPLAY '*                              *'
               DISPLAY '*********** GFCT1742 ***********'
           END-IF.

           PERFORM 0900-LER-SELECAO4.

           IF  WRK-CHV-SELECAO4            EQUAL HIGH-VALUES
               DISPLAY '*********** GFCT1742 ***********'
               DISPLAY '*                              *'
               DISPLAY '*    ARQUIVO SELECAO4 VAZIO    *'
               DISPLAY '*                              *'
               DISPLAY '*********** GFCT1742 ***********'
           END-IF.

      *----------------------------------------------------------------*
       0700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LEITURA DO ARQUIVO ARQRESP4                                 *
      *----------------------------------------------------------------*
       0800-LER-ARQRESP4               SECTION.
      *----------------------------------------------------------------*

           READ ARQRESP4.

           IF  WRK-FS-ARQRESP4         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHV-ARQRESP4
               GO                      TO 0800-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 0300-TESTAR-FS-ARQRESP4.

           MOVE GFCTTC-AGENCIA OF ARQRESP4 TO WRK-AG-ARQRESP4.
           MOVE GFCTTC-CONTA   OF ARQRESP4 TO WRK-CTA-ARQRESP4.

           ADD 1                       TO ACU-LIDOS-ARQRESP4.

      *----------------------------------------------------------------*
       0800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LEITURA DO ARQUIVO SELECAO4                                 *
      *----------------------------------------------------------------*
       0900-LER-SELECAO4               SECTION.
      *----------------------------------------------------------------*

           READ SELECAO4.

           IF  WRK-FS-SELECAO4         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHV-SELECAO4
               GO                      TO 0900-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 0400-TESTAR-FS-SELECAO4.

           MOVE GFCT61-AGENCIA OF SELECAO4 TO WRK-AG-SELECAO4.
           MOVE GFCT61-CONTA   OF SELECAO4 TO WRK-CONTA.
           MOVE WRK-GFCT-CONTA             TO WRK-CTA-SELECAO4.

           ADD 1                       TO ACU-LIDOS-SELECAO4.

      *----------------------------------------------------------------*
       0900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL DO PROGRAMA                         *
      *----------------------------------------------------------------*
       1000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           IF  WRK-CHV-ARQRESP4      EQUAL WRK-CHV-SELECAO4
               PERFORM 1100-GRAVAR-ARQRESP5
               PERFORM 0800-LER-ARQRESP4
           ELSE
               IF  WRK-CHV-ARQRESP4  LESS WRK-CHV-SELECAO4
                   ADD 1               TO ACU-DESPREZADOS
                   PERFORM 0800-LER-ARQRESP4
               ELSE
                   PERFORM 0900-LER-SELECAO4
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    GRAVAR ARQUIVO  ARQRESP5                                    *
      *----------------------------------------------------------------*
       1100-GRAVAR-ARQRESP5            SECTION.
      *----------------------------------------------------------------*

           MOVE REG-CRESP   OF ARQRESP4
                            TO REG-CRESP1 OF ARQRESP5.

           MOVE GFCT61-CPF-NUM TO GFCTU7-CPF-NUM.
           MOVE GFCT61-CPF-CTR TO GFCTU7-CPF-CTR.

           WRITE REG-CRESP1 OF ARQRESP5.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 0500-TESTAR-FS-ARQRESP5.

           ADD 1                       TO ACU-GRAVADOS-ARQRESP5.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA FINALIZACAO DO PROGRAMA                         *
      *----------------------------------------------------------------*
       1300-PROCEDIMENTOS-FINAIS       SECTION.
      *----------------------------------------------------------------*

           PERFORM 1400-EMITIR-DISPLAY.

           CLOSE ARQRESP4
                 SELECAO4
                 ARQRESP5

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

           DISPLAY '******************** GFCT1742 ********************'.
           DISPLAY '*                                                *'.
           MOVE ACU-LIDOS-ARQRESP4     TO WRK-MASCARA.
           DISPLAY '*  LIDOS EM ARQRESP4......: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-LIDOS-SELECAO4     TO WRK-MASCARA.
           DISPLAY '*  LIDOS EM SELECAO4......: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-GRAVADOS-ARQRESP5  TO WRK-MASCARA.
           DISPLAY '*  GRAVADOS NO ARQRESP5...: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-DESPREZADOS        TO WRK-MASCARA.
           DISPLAY '*  NAO CORRESPONDENTE.....: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           DISPLAY '******************** GFCT1742 ********************'.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAMENTO DE ERRO                              *
      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT1742'             TO ERR-PGM.
           MOVE 'APL'                  TO ERR-TIPO-ACESSO.

           CALL 'BRAD7100'             USING WRK-BATCH
                                             ERRO-AREA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------
