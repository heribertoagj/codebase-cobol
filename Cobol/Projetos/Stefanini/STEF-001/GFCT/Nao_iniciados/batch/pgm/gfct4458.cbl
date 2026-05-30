      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. GFCT4458.
       AUTHOR. CIBELE BARBOSA.

      *================================================================*
      *                   S O N D A   I T                              *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA    : GFCT4458                                      *
      *    ANALISTA    : CIBELE BARBOSA - SONDA IT                     *
      *    DATA        : ABRIL/2014                                    *
      *    OBJETIVO    :                                               *
      *      BATER ARQUIVO DE BONIFICACOES EFETUADAS COM ACUMULADO     *
      *      SIGB PARA ATUALIZAR O VALOR TOTAL NO SIGB.                *
      *                                                                *
      *    ARQUIVOS:                                                   *
      *      DDNAME                                    INCLUDE/BOOK    *
      *      ARQHISTE                                    GFCTWBBF      *
      *      DADOSIGB                                    GFCTWBBD      *
      *      ARQHISTS                                    GFCTWBBF      *
      *      ATUSIGBS                                    I#GFCTVD      *
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

           SELECT ARQHISTE ASSIGN      TO UT-S-ARQHISTE
                      FILE STATUS      IS WRK-FS-ARQHISTE.

           SELECT DADOSIGB ASSIGN      TO UT-S-DADOSIGB
                      FILE STATUS      IS WRK-FS-DADOSIGB.

           SELECT ARQHISTS ASSIGN      TO UT-S-ARQHISTS
                      FILE STATUS      IS WRK-FS-ARQHISTS.

           SELECT ATUSIGBS ASSIGN      TO UT-S-ATUSIGBS
                      FILE STATUS      IS WRK-FS-ATUSIGBS.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:  BONIFICACAO DESCONTO PROGRESSIVO INDIVIDUAL         *
      *            ORG. SEQUENTIAL     -  LRECL = 140                  *
      *----------------------------------------------------------------*

       FD  ARQHISTE
           RECORDING MODE IS F
           LABEL RECORDS IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-ARQHISTE                 PIC X(140).

      *----------------------------------------------------------------*
      *    INPUT:  CLASSIFICACAO AGENCIA, CONTA E CARTEIRA             *
      *            ORG. SEQUENTIAL     -  LRECL =  34                  *
      *----------------------------------------------------------------*

       FD  DADOSIGB
           RECORDING MODE IS F
           LABEL RECORDS IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-DADOSIGB                 PIC X(34).

      *----------------------------------------------------------------*
      *    OUTPUT:   BONIFICACAO DESCONTO PROGRESSIVO INDIVIDUAL       *
      *              ORG. SEQUENTIAL   - LRECL = 140                   *
      *----------------------------------------------------------------*

       FD  ARQHISTS
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-ARQHISTS                 PIC X(140).

      *----------------------------------------------------------------*
      *    OUTPUT:   CLASSIFICACAO AGENCIA, CONTA E CARTEIRA           *
      *              ORG. SEQUENTIAL     -  LRECL =  34                *
      *----------------------------------------------------------------*

       FD  ATUSIGBS
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-ATUSIGBS                 PIC X(034).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  INICIO DA WORKING GFCT4458  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*        ACUMULADORES          *'.
      *----------------------------------------------------------------*

       01  ACU-ACUMULADORES.
           05 ACU-LIDOS-ARQHISTE    PIC 9(09) COMP-3    VALUE ZEROS.
           05 ACU-LIDOS-DADOSIGB    PIC 9(09) COMP-3    VALUE ZEROS.
           05 ACU-GRAVADOS-ATUSIGBS PIC 9(09) COMP-3    VALUE ZEROS.
           05 ACU-GRAVADOS-ARQHISTS PIC 9(09) COMP-3    VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    VARIAVEIS AUXILIARES      *'.
      *----------------------------------------------------------------*

       01  WRK-AUXILIARES.
           05 WRK-BATCH                PIC X(08)           VALUE
              'BATCH'.
           05 WRK-MASCARA              PIC ZZZ.ZZZ.Z99     VALUE ZEROS.
           05  WRK-S-905               PIC +9(05)          VALUE ZEROS.
           05  FILLER                  REDEFINES WRK-S-905.
             10 FILLER                 PIC X(01).
             10 WRK-905                PIC 9(05).
           05  WRK-S-909               PIC +9(09)          VALUE ZEROS.
           05  WRK-FILLER              REDEFINES WRK-S-909.
             10 FILLER                 PIC X(01).
             10 WRK-909                PIC 9(09).

           05   WRK-CONSE-QTDE         PIC S9(15) COMP-3   VALUE ZEROS.

           05   WRK-CONSE-VLR          PIC S9(13)V99 COMP-3 VALUE ZEROS.

           05   WRK-CONSE-QTDE-ATU     PIC S9(15) COMP-3   VALUE ZEROS.

           05   WRK-CONSE-VLR-ATU      PIC S9(13)V99 COMP-3 VALUE ZEROS.

           05 WRK-SIGB-VALOR-TOTAL PIC S9(013)V9(2) COMP-3 VALUE ZEROS.
           05 WRK-SIGB-VR-TOT-QTDE  REDEFINES
                 WRK-SIGB-VALOR-TOTAL  PIC S9(015)V COMP-3.


      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*       AREA PARA CHAVES       *'.
      *----------------------------------------------------------------*

       01  WRK-CHV-ARQHISTE-ANT.
ST25X6*    05 WRK-CNPJ-22A-ANT         PIC  9(09)       VALUE ZEROS.
ST25X6     05 WRK-CNPJ-22A-ANT         PIC  X(009)       VALUE SPACES.
ST25X6*    05 WRK-FILIAL-22A-ANT       PIC  9(05)       VALUE ZEROS.
ST25X6     05 WRK-FILIAL-22A-ANT       PIC  X(005)       VALUE SPACES.
           05 WRK-CTRL-22A-ANT         PIC 9(02)       VALUE ZEROS.
           05 WRK-AG-22A-ANT           PIC 9(05)       VALUE ZEROS.
           05 WRK-CART-22A-ANT         PIC 9(05)       VALUE ZEROS.
           05 WRK-ANOMES-22A-ANT       PIC 9(06)       VALUE ZEROS.

       01  WRK-CHV-ARQHISTE.
ST25X6*    05 WRK-CNPJ-22A             PIC  9(09)       VALUE ZEROS.
ST25X6     05 WRK-CNPJ-22A             PIC  X(009)       VALUE SPACES.
ST25X6*    05 WRK-FILIAL-22A           PIC  9(05)       VALUE ZEROS.
ST25X6     05 WRK-FILIAL-22A           PIC  X(005)       VALUE SPACES.
           05 WRK-CTRL-22A             PIC 9(02)       VALUE ZEROS.
           05 WRK-AG-22A               PIC 9(05)       VALUE ZEROS.
           05 WRK-CART-22A             PIC 9(05)       VALUE ZEROS.
           05 WRK-ANOMES-22A           PIC 9(06)       VALUE ZEROS.

       01  WRK-CHV-SIGB.
ST25X6*    05 WRK-CNPJ-SIGB            PIC  9(09)       VALUE ZEROS.
ST25X6     05 WRK-CNPJ-SIGB            PIC  X(009)       VALUE SPACES.
ST25X6*    05 WRK-FILIAL-SIGB          PIC  9(05)       VALUE ZEROS.
ST25X6     05 WRK-FILIAL-SIGB          PIC  X(005)       VALUE SPACES.
           05 WRK-CTRL-SIGB            PIC 9(02)       VALUE ZEROS.
           05 WRK-AG-SIGB              PIC 9(05)       VALUE ZEROS.
           05 WRK-CART-SIGB            PIC 9(05)       VALUE ZEROS.
           05 WRK-ANOMES-SIGB          PIC 9(06)       VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    AREA PARA FILE STATUS     *'.
      *----------------------------------------------------------------*

       01  WRK-FS-ARQHISTE             PIC X(02)           VALUE SPACES.
       01  WRK-FS-DADOSIGB             PIC X(02)           VALUE SPACES.
       01  WRK-FS-ARQHISTS             PIC X(02)           VALUE SPACES.
       01  WRK-FS-ATUSIGBS             PIC X(02)           VALUE SPACES.
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

      *-----------------------------------------------------------------
       01  FILLER                      PIC X(32)           VALUE
           '*  AREA PARA ARQHISTE/ARQHISTS *'.
      *-----------------------------------------------------------------

       COPY GFCTWBBF.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*       AREA PARA DADOSIGB     *'.
      *----------------------------------------------------------------*

       COPY GFCTWBBD.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     AREA PARA BRAD7100       *'.
      *----------------------------------------------------------------*

       COPY I#BRAD7C.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*   FIM  DA WORKING GFCT4458   *'.
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
                    WRK-CHV-ARQHISTE   EQUAL HIGH-VALUES AND
                    WRK-CHV-SIGB       EQUAL HIGH-VALUES.

           PERFORM 1350-PROCEDIMENTOS-FINAIS.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA INICIALIZAR O PROGRAMA                          *
      *----------------------------------------------------------------*
       0100-INICIALIZAR-PROGRAMA       SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT ARQHISTE
                      DADOSIGB
               OUTPUT ARQHISTS
                      ATUSIGBS.

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

           PERFORM 0300-TESTAR-FS-ARQHISTE.

           PERFORM 0400-TESTAR-FS-DADOSIGB.

           PERFORM 0500-TESTAR-FS-ARQHISTS.

           PERFORM 0600-TESTAR-FS-ATUSIGBS.

      *----------------------------------------------------------------*
       0200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO ARQHISTE                      *
      *----------------------------------------------------------------*
       0300-TESTAR-FS-ARQHISTE         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ARQHISTE         NOT EQUAL '00'
               MOVE 'ARQHISTE'         TO WRK-NOME-ARQ
               MOVE WRK-FS-ARQHISTE    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO DADOSIGB                      *
      *----------------------------------------------------------------*
       0400-TESTAR-FS-DADOSIGB         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-DADOSIGB         NOT EQUAL '00'
               MOVE 'DADOSIGB'         TO WRK-NOME-ARQ
               MOVE WRK-FS-DADOSIGB    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO ARQHISTS                      *
      *----------------------------------------------------------------*
       0500-TESTAR-FS-ARQHISTS         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ARQHISTS         NOT EQUAL '00'
               MOVE 'ARQHISTS'         TO WRK-NOME-ARQ
               MOVE WRK-FS-ARQHISTS    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO ATUSIGBS                      *
      *----------------------------------------------------------------*
       0600-TESTAR-FS-ATUSIGBS         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ATUSIGBS         NOT EQUAL '00'
               MOVE 'ATUSIGBS'         TO WRK-NOME-ARQ
               MOVE WRK-FS-ATUSIGBS    TO WRK-FS
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

           PERFORM 0800-LER-ARQHISTE.

           IF  WRK-CHV-ARQHISTE        EQUAL HIGH-VALUES
               DISPLAY '*********** GFCT4458 ***********'
               DISPLAY '*                              *'
               DISPLAY '*    ARQUIVO  ARQHISTE VAZIO   *'
               DISPLAY '*    PROCESSAMENTO ENCERRADO   *'
               DISPLAY '*                              *'
               DISPLAY '*********** GFCT4458 ***********'
           END-IF.

           PERFORM 0900-LER-DADOSIGB.

           IF  WRK-CHV-SIGB            EQUAL HIGH-VALUES
               DISPLAY '*********** GFCT4458 ***********'
               DISPLAY '*                              *'
               DISPLAY '*    ARQUIVO DADOSIGB VAZIO    *'
               DISPLAY '*                              *'
               DISPLAY '*********** GFCT4458 ***********'
           END-IF.

      *----------------------------------------------------------------*
       0700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LEITURA DO ARQUIVO ARQHISTE                                 *
      *----------------------------------------------------------------*
       0800-LER-ARQHISTE               SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-CHV-ARQHISTE      TO WRK-CHV-ARQHISTE-ANT.

           READ ARQHISTE               INTO GFCTWBBF-REG.

           IF  WRK-FS-ARQHISTE         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHV-ARQHISTE
               GO                      TO 0800-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 0300-TESTAR-FS-ARQHISTE.

ST25X6*    MOVE WBBF-CPF-CNPJ                        TO WRK-S-909.
ST25X6*    MOVE WRK-909                              TO WRK-CNPJ-22A.
ST25X6*    MOVE WBBF-FILIAL                          TO WRK-S-905.
ST25X6*    MOVE WRK-905                              TO WRK-FILIAL-22A.
ST25X6     MOVE WBBF-CPF-CNPJ                        TO WRK-CNPJ-22A.
ST25X6     MOVE WBBF-FILIAL                          TO WRK-FILIAL-22A.
           MOVE WBBF-CONTROLE                        TO WRK-CTRL-22A.
           MOVE WBBF-CAG-DSTNO-MOVTO                 TO WRK-S-905.
           MOVE WRK-905                              TO WRK-AG-22A.
           MOVE WBBF-23-CCART-ANLSE-GERC             TO WRK-S-905.
           MOVE WRK-905                              TO WRK-CART-22A.

           MOVE WBBF-ANOMES-APURACAO                 TO WRK-ANOMES-22A.

           ADD 1                         TO ACU-LIDOS-ARQHISTE.

           IF   ACU-LIDOS-ARQHISTE  EQUAL 1
                MOVE WRK-CHV-ARQHISTE    TO WRK-CHV-ARQHISTE-ANT
           END-IF.

      *----------------------------------------------------------------*
       0800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LEITURA DO ARQUIVO DADOSIGB                                 *
      *----------------------------------------------------------------*
       0900-LER-DADOSIGB               SECTION.
      *----------------------------------------------------------------*

           READ DADOSIGB               INTO GFCTWBBD-REG.

           IF  WRK-FS-DADOSIGB         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHV-SIGB
               GO                      TO 0900-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 0400-TESTAR-FS-DADOSIGB.

ST25X6*    MOVE WBBD-CPF-CNPJ-PRINC      TO WRK-S-909.
ST25X6*    MOVE WRK-909                  TO WRK-CNPJ-SIGB.
ST25X6*    MOVE WBBD-CPF-CNPJ-FILIAL     TO WRK-S-905.
ST25X6*    MOVE WRK-905                  TO WRK-FILIAL-SIGB.
ST25X6     MOVE WBBD-CPF-CNPJ-PRINC      TO WRK-CNPJ-SIGB.
ST25X6     MOVE WBBD-CPF-CNPJ-FILIAL     TO WRK-FILIAL-SIGB.
           MOVE WBBD-CPF-CNPJ-CTR        TO WRK-CTRL-SIGB.
           MOVE WBBD-AGEN                TO WRK-S-905.
           MOVE WRK-905                  TO WRK-AG-SIGB.
           MOVE WBBD-CARTEIRA-SIGB       TO WRK-S-905.
           MOVE WRK-905                  TO WRK-CART-SIGB.

           MOVE WBBD-AAAA-MM-APURACAO    TO WRK-ANOMES-SIGB.

           MOVE ZEROS                    TO WRK-SIGB-VALOR-TOTAL
                                            WRK-SIGB-VR-TOT-QTDE.

           MOVE WBBD-VALOR-TOTAL         TO WRK-SIGB-VALOR-TOTAL.

           ADD 1                       TO ACU-LIDOS-DADOSIGB.

      *----------------------------------------------------------------*
       0900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL DO PROGRAMA                         *
      *----------------------------------------------------------------*
       1000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           IF  WRK-CHV-ARQHISTE      EQUAL WRK-CHV-SIGB
               MOVE WRK-CHV-ARQHISTE TO WRK-CHV-ARQHISTE-ANT
               PERFORM 1100-ATUALIZA-VALOR
               PERFORM 0800-LER-ARQHISTE
           ELSE
               IF  WRK-CHV-ARQHISTE  LESS WRK-CHV-SIGB
                   PERFORM 1200-GRAVAR-ARQHISTS
                   PERFORM 0800-LER-ARQHISTE
               ELSE
                   PERFORM 1300-GRAVAR-DADOSIGB
                   PERFORM 0900-LER-DADOSIGB
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    GRAVAR ARQUIVO  ARQHISTS                                    *
      *----------------------------------------------------------------*
       1100-ATUALIZA-VALOR             SECTION.
      *----------------------------------------------------------------*

           IF WRK-CHV-ARQHISTE NOT EQUAL WRK-CHV-ARQHISTE-ANT
              PERFORM 1300-GRAVAR-DADOSIGB
              PERFORM 0900-LER-DADOSIGB
           END-IF.

           IF WBBF-23-CIDTFD-TPO-FAIXA EQUAL  1
              MOVE  WRK-SIGB-VR-TOT-QTDE    TO WBBF-CONSE-DADO-ANT
                                               WRK-CONSE-QTDE
              COMPUTE WRK-CONSE-QTDE-ATU    = (WRK-CONSE-QTDE    -
                                               WBBF-QMIN-FAIXA-BONIF)
              MOVE WRK-CONSE-QTDE-ATU       TO WBBF-CONSE-DADO-ATU
                                               WRK-SIGB-VR-TOT-QTDE
           ELSE
              MOVE  WRK-SIGB-VALOR-TOTAL    TO WBBF-CONSE-DADO-ANT-R
                                               WRK-CONSE-VLR
              COMPUTE WRK-CONSE-VLR-ATU     = (WRK-CONSE-VLR     -
                                               WBBF-VMIN-FAIXA-BONIF)
              MOVE WRK-CONSE-VLR-ATU        TO WRK-SIGB-VALOR-TOTAL
                                               WBBF-CONSE-DADO-ATU-R
           END-IF.

           PERFORM 1200-GRAVAR-ARQHISTS.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    GRAVAR ARQUIVO  ARQHISTS                                    *
      *----------------------------------------------------------------*
       1200-GRAVAR-ARQHISTS            SECTION.
      *----------------------------------------------------------------*

           WRITE FD-ARQHISTS      FROM GFCTWBBF-REG.

           MOVE WRK-GRAVACAO      TO WRK-OPERACAO.

           PERFORM 0500-TESTAR-FS-ARQHISTS.

           ADD 1                  TO ACU-GRAVADOS-ARQHISTS.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    GRAVAR ARQUIVO ATUSIGBS                                     *
      *----------------------------------------------------------------*
       1300-GRAVAR-DADOSIGB            SECTION.
      *----------------------------------------------------------------*

           MOVE  WRK-SIGB-VALOR-TOTAL  TO WBBD-VALOR-TOTAL.

           WRITE FD-ATUSIGBS           FROM GFCTWBBD-REG.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 0600-TESTAR-FS-ATUSIGBS.

           ADD 1                       TO ACU-GRAVADOS-ATUSIGBS.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA FINALIZACAO DO PROGRAMA                         *
      *----------------------------------------------------------------*
       1350-PROCEDIMENTOS-FINAIS       SECTION.
      *----------------------------------------------------------------*

           PERFORM 1400-EMITIR-DISPLAY.

           CLOSE ARQHISTE
                 DADOSIGB
                 ARQHISTS
                 ATUSIGBS.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 0200-TESTAR-FILE-STATUS.

           STOP RUN.

      *----------------------------------------------------------------*
       1350-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA EMISSAO DOS DISPLAYS FINAIS DO PROGRAMA         *
      *----------------------------------------------------------------*
       1400-EMITIR-DISPLAY             SECTION.
      *----------------------------------------------------------------*

           MOVE ACU-LIDOS-ARQHISTE     TO WRK-MASCARA.

           DISPLAY '******************** GFCT4458 *******************'
           '**'.

           DISPLAY '*                                     '
           '            *'.
           DISPLAY '* REGISTROS LIDOS EM ARQHISTE......: '
           ' ' WRK-MASCARA ' *'.

           MOVE ACU-LIDOS-DADOSIGB     TO WRK-MASCARA.

           DISPLAY '* REGISTROS LIDOS EM DADOSIGB......: '
           ' ' WRK-MASCARA ' *'.

           MOVE ACU-GRAVADOS-ATUSIGBS  TO WRK-MASCARA.

           DISPLAY '* REGISTROS GRAVADOS NO ATUSIGBS...: '
           ' ' WRK-MASCARA ' *'.

           MOVE ACU-GRAVADOS-ARQHISTS  TO WRK-MASCARA.

           DISPLAY '* REGISTROS GRAVADOS NO ARQHISTS...: '
           ' ' WRK-MASCARA ' *'.

           DISPLAY '*                                     '
           '            *'.
           DISPLAY '******************** GFCT4458 *******************'
           '**'.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAMENTO DE ERRO                              *
      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT4458'             TO ERR-PGM.
           MOVE 'APL'                  TO ERR-TIPO-ACESSO.

           CALL 'BRAD7100'             USING WRK-BATCH
                                             ERRO-AREA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------

