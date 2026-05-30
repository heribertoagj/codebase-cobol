      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. GFCT2401.
       AUTHOR.     VINICIUS CRISTIAN MADUREIRA.
      *================================================================*
      *      S O N D A  P R O C W O R K  -  C O N S U L T O R I A      *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT2401                                    *
      *    PROGRAMADOR.:   VINICIUS CRISTIAN MADUREIRA - SONDPROC/GP.50*
      *    ANALISTA....:   VINICIUS CRISTIAN MADUREIRA - SONDPROC/GP.50*
      *    DATA........:   23/03/2009                                  *
      *                                                                *
      *    OBJETIVO    :   INCLUI INFORMACAO DE PAB NO ARQUIVO COM A   *
      *                    BAIXA DE ADESOES INDIVIDUAIS.               *
      *                                                                *
      *    ARQUIVOS:                                                   *
      *      DDNAME                                    INCLUDE/BOOK    *
      *      ARQCESTA                                    GFCTWAF5      *
      *      ARQCLIE1                                    I#CLIE20      *
      *      CLICESTA                                    GFCTWAF0      *
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

           SELECT ARQCESTA ASSIGN      TO UT-S-ARQCESTA
                      FILE STATUS      IS WRK-FS-ARQCESTA.

           SELECT ARQCLIE1 ASSIGN      TO UT-S-ARQCLIE1
                      FILE STATUS      IS WRK-FS-ARQCLIE1.

           SELECT CLICESTA ASSIGN      TO UT-S-CLICESTA
                      FILE STATUS      IS WRK-FS-CLICESTA.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:                                                      *
      *            ORG. SEQUENTIAL     -  LRECL = 093                  *
      *----------------------------------------------------------------*

       FD  ARQCESTA
           RECORDING MODE IS F
           LABEL RECORDS IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'GFCTWAF5'.

      *----------------------------------------------------------------*
      *    INPUT:                                                      *
      *            ORG. SEQUENTIAL     -  LRECL = 180                  *
      *----------------------------------------------------------------*

       FD  ARQCLIE1
           RECORDING MODE IS F
           LABEL RECORDS IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#CLIE20'.

      *----------------------------------------------------------------*
      *    OUTPUT:                                                     *
      *            ORG. SEQUENTIAL     -  LRECL = 150                  *
      *----------------------------------------------------------------*

       FD  CLICESTA
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'GFCTWAF0'.

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  INICIO DA WORKING GFCT2401  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*        ACUMULADORES          *'.
      *----------------------------------------------------------------*

       01  ACU-ACUMULADORES.
           05 ACU-LIDOS-ARQCESTA       PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-LIDOS-ARQCLIE1       PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-GRAVADOS-CLICESTA    PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-DESPREZADOS          PIC 9(11) COMP-3    VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    VARIAVEIS AUXILIARES      *'.
      *----------------------------------------------------------------*

       01  WRK-AUXILIARES.
           05 WRK-BATCH                PIC X(08)            VALUE
              'BATCH'.
           05 WRK-MASCARA              PIC ZZ.ZZZ.ZZZ.Z99   VALUE ZEROS.

       01  WRK-CHV-ARQCESTA.
           03 WRK-AGENCIA-ARQCESTA     PIC 9(005)           VALUE ZEROS.
           03 WRK-CONTA-ARQCESTA       PIC 9(007)           VALUE ZEROS.

       01  WRK-CHV-ARQCLIE1.
           03 WRK-AGENCIA-ARQCLIE1     PIC 9(005)           VALUE ZEROS.
           03 WRK-CONTA-ARQCLIE1       PIC 9(007)           VALUE ZEROS.

       01  WRK-S9-5                    PIC S9(005)          VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-S9-5.
           05 WRK-9-5                  PIC  9(005).

       01  WRK-S9-7                    PIC S9(007)          VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-S9-7.
           05 WRK-9-7                  PIC  9(007).

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    AREA PARA FILE STATUS     *'.
      *----------------------------------------------------------------*

       01  WRK-FS-ARQCESTA             PIC X(02)           VALUE SPACES.
       01  WRK-FS-ARQCLIE1             PIC X(02)           VALUE SPACES.
       01  WRK-FS-CLICESTA             PIC X(02)           VALUE SPACES.
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
           '*   FIM  DA WORKING GFCT2401   *'.
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
                    WRK-CHV-ARQCESTA   EQUAL HIGH-VALUES.


           PERFORM 1300-PROCEDIMENTOS-FINAIS.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA INICIALIZAR O PROGRAMA                          *
      *----------------------------------------------------------------*
       0100-INICIALIZAR-PROGRAMA       SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT ARQCESTA
                      ARQCLIE1
               OUTPUT CLICESTA.

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

           PERFORM 0300-TESTAR-FS-ARQCESTA.

           PERFORM 0400-TESTAR-FS-ARQCLIE1.

           PERFORM 0600-TESTAR-FS-CLICESTA.

      *----------------------------------------------------------------*
       0200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO ARQCESTA                      *
      *----------------------------------------------------------------*
       0300-TESTAR-FS-ARQCESTA         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ARQCESTA         NOT EQUAL '00'
               MOVE 'ARQCESTA'         TO WRK-NOME-ARQ
               MOVE WRK-FS-ARQCESTA    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO ARQCLIE1                      *
      *----------------------------------------------------------------*
       0400-TESTAR-FS-ARQCLIE1         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ARQCLIE1         NOT EQUAL '00'
               MOVE 'ARQCLIE1'         TO WRK-NOME-ARQ
               MOVE WRK-FS-ARQCLIE1    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO CLICESTA                      *
      *----------------------------------------------------------------*
       0600-TESTAR-FS-CLICESTA         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-CLICESTA         NOT EQUAL '00'
               MOVE 'CLICESTA'         TO WRK-NOME-ARQ
               MOVE WRK-FS-CLICESTA    TO WRK-FS
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

           PERFORM 0800-LER-ARQCESTA.

           IF  WRK-CHV-ARQCESTA        EQUAL HIGH-VALUES
               DISPLAY '*********** GFCT2401 ***********'
               DISPLAY '*                              *'
               DISPLAY '*    ARQUIVO  ARQCESTA VAZIO   *'
               DISPLAY '*    PROCESSAMENTO ENCERRADO   *'
               DISPLAY '*                              *'
               DISPLAY '*********** GFCT2401 ***********'
           END-IF.

           PERFORM 0900-LER-ARQCLIE1.

           IF  WRK-CHV-ARQCLIE1            EQUAL HIGH-VALUES
               DISPLAY '*********** GFCT2401 ***********'
               DISPLAY '*                              *'
               DISPLAY '*    ARQUIVO ARQCLIE1 VAZIO    *'
               DISPLAY '*                              *'
               DISPLAY '*********** GFCT2401 ***********'
           END-IF.

      *----------------------------------------------------------------*
       0700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LEITURA DO ARQUIVO ARQCESTA                                 *
      *----------------------------------------------------------------*
       0800-LER-ARQCESTA               SECTION.
      *----------------------------------------------------------------*

           READ ARQCESTA.

           IF  WRK-FS-ARQCESTA         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHV-ARQCESTA
               GO                      TO 0800-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 0300-TESTAR-FS-ARQCESTA.

           MOVE WAF5-CJUNC-DEPDC       TO WRK-S9-5.
           MOVE WRK-9-5                TO WRK-AGENCIA-ARQCESTA.
           MOVE WAF5-CCTA-CLI          TO WRK-S9-7.
           MOVE WRK-9-7                TO WRK-CONTA-ARQCESTA.

           ADD 1                       TO ACU-LIDOS-ARQCESTA.

      *----------------------------------------------------------------*
       0800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LEITURA DO ARQUIVO ARQCLIE1                                 *
      *----------------------------------------------------------------*
       0900-LER-ARQCLIE1               SECTION.
      *----------------------------------------------------------------*

           READ ARQCLIE1.

           IF  WRK-FS-ARQCLIE1         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHV-ARQCLIE1
               GO                      TO 0900-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 0400-TESTAR-FS-ARQCLIE1.

           MOVE CAD-AGENCIA            TO WRK-AGENCIA-ARQCLIE1.
           MOVE CAD-CONTA              TO WRK-CONTA-ARQCLIE1.

           ADD 1                       TO ACU-LIDOS-ARQCLIE1.

      *----------------------------------------------------------------*
       0900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL DO PROGRAMA                         *
      *----------------------------------------------------------------*
       1000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           IF  WRK-CHV-ARQCESTA      EQUAL WRK-CHV-ARQCLIE1
               PERFORM 1200-GRAVAR-CLICESTA
               PERFORM 0800-LER-ARQCESTA
           ELSE
               IF  WRK-CHV-ARQCESTA  LESS WRK-CHV-ARQCLIE1
                   ADD 1         TO ACU-DESPREZADOS
                   PERFORM 0800-LER-ARQCESTA
               ELSE
                   PERFORM 0900-LER-ARQCLIE1
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    GRAVAR ARQUIVO  CLICESTA                                    *
      *----------------------------------------------------------------*
       1200-GRAVAR-CLICESTA            SECTION.
      *----------------------------------------------------------------*

           MOVE REG-WAF5          TO WAF0-CESTA.
           MOVE CAD-POSTO-SERV    TO WAF0-PAB-CLI.

           WRITE REG-WAF0 OF CLICESTA.

           MOVE WRK-GRAVACAO      TO WRK-OPERACAO.

           PERFORM 0600-TESTAR-FS-CLICESTA.

           ADD 1                  TO ACU-GRAVADOS-CLICESTA.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA FINALIZACAO DO PROGRAMA                         *
      *----------------------------------------------------------------*
       1300-PROCEDIMENTOS-FINAIS       SECTION.
      *----------------------------------------------------------------*

           PERFORM 1400-EMITIR-DISPLAY.

           CLOSE ARQCESTA
                 ARQCLIE1
                 CLICESTA.

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

           DISPLAY '******************** GFCT2401 ********************'.
           DISPLAY '*                                                *'.
           MOVE ACU-LIDOS-ARQCESTA     TO WRK-MASCARA.
           DISPLAY '*  LIDOS EM ARQCESTA......: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-LIDOS-ARQCLIE1     TO WRK-MASCARA.
           DISPLAY '*  LIDOS EM ARQCLIE1......: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-GRAVADOS-CLICESTA  TO WRK-MASCARA.
           DISPLAY '*  GRAVADOS NO CLICESTA...: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-DESPREZADOS        TO WRK-MASCARA.
           DISPLAY '*  CESTAS NAO ENCONTRADAS NO                     *'.
           DISPLAY '*  ARQUIVO DE CLIENTES....: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           DISPLAY '******************** GFCT2401 ********************'.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAMENTO DE ERRO                              *
      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT2401'             TO ERR-PGM.
           MOVE 'APL'                  TO ERR-TIPO-ACESSO.

           CALL 'BRAD7100'             USING WRK-BATCH
                                             ERRO-AREA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *---------------------------------------------------------------

