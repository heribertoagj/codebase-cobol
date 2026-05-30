      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. GFCT4454.
       AUTHOR. CIBELE BARBOSA.

      *================================================================*
      *                   S O N D A   I T                              *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA    : GFCT4454                                      *
      *    ANALISTA    : CIBELE BARBOSA - SONDA IT                     *
      *    DATA        : ABRIL/2014                                    *
      *    OBJETIVO    :                                               *
      *      BATER ARQUIVO DE EVENTOS COM O GERADO  PELO SIGB PARA     *
      *      ENCONTRAR FATOR PARA DESCONTO.                            *
      *                                                                *
      *    ARQUIVOS:                                                   *
      *      DDNAME                                    INCLUDE/BOOK    *
      *      APURA22A                                    GFCTWBBE      *
      *      DADOSIGB                                    GFCTWBBD      *
      *      APURA22B                                    GFCTWBBE      *
      *      SAIDA22B                                    I#GFCTVA      *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      BRAD7100  -  MODULO DE TRATAMENTO DE ERRO.                *
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

           SELECT APURA22A ASSIGN      TO UT-S-APURA22A
                      FILE STATUS      IS WRK-FS-APURA22A.

           SELECT DADOSIGB ASSIGN      TO UT-S-DADOSIGB
                      FILE STATUS      IS WRK-FS-DADOSIGB.

           SELECT APURA22B ASSIGN      TO UT-S-APURA22B
                      FILE STATUS      IS WRK-FS-APURA22B.

           SELECT SAIDA22B ASSIGN      TO UT-S-SAIDA22B
                      FILE STATUS      IS WRK-FS-SAIDA22B.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:  BONIFICACAO DESCONTO PROGRESSIVO INDIVIDUAL         *
      *            ORG. SEQUENTIAL     -  LRECL = 320                  *
      *----------------------------------------------------------------*

       FD  APURA22A
           RECORDING MODE IS F
           LABEL RECORDS IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-APURA22A                 PIC X(320).

      *----------------------------------------------------------------*
      *    INPUT:  CLASSIFICACAO AGENCIA, CONTA E CARTEIRA             *
ACERTO*            ORG. SEQUENTIAL     -  LRECL =  34                  *
      *----------------------------------------------------------------*

       FD  DADOSIGB
           RECORDING MODE IS F
           LABEL RECORDS IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

ACERTO 01  FD-DADOSIGB                 PIC X(34).

      *----------------------------------------------------------------*
      *    OUTPUT:   BONIFICACAO DESCONTO PROGRESSIVO INDIVIDUAL       *
      *              ORG. SEQUENTIAL   - LRECL = 320                   *
      *----------------------------------------------------------------*

       FD  APURA22B
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-APURA22B                 PIC X(320).

      *----------------------------------------------------------------*
      *    OUTPUT:   COBRANCA - ROTINA DE FLEXIBILIZACAO               *
      *              ORG. SEQUENTIAL   - LRECL = 250                   *
      *----------------------------------------------------------------*

       FD  SAIDA22B
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-SAIDA22B                 PIC X(250).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  INICIO DA WORKING GFCT4454  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*        ACUMULADORES          *'.
      *----------------------------------------------------------------*

       01  ACU-ACUMULADORES.
           05 ACU-LIDOS-APURA22A       PIC 9(09) COMP-3    VALUE ZEROS.
           05 ACU-LIDOS-DADOSIGB       PIC 9(09) COMP-3    VALUE ZEROS.
           05 ACU-GRAVADOS-SAIDA22B    PIC 9(09) COMP-3    VALUE ZEROS.
           05 ACU-GRAVADOS-APURA22B    PIC 9(09) COMP-3    VALUE ZEROS.

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
           05  WRK-DATA-10             PIC  X(10)         VALUE SPACES.
           05  WRK-DATA                REDEFINES WRK-DATA-10.
             10 WRK-DD-10              PIC 9(02).
             10 FILLER                 PIC X(01).
             10 WRK-MM-10              PIC 9(02).
             10 FILLER                 PIC X(01).
             10 WRK-AAAA-10            PIC 9(04).
           05  WRK-DT-ANOMES           PIC 9(06)         VALUE ZEROS.
           05  WRK-ANOMES              REDEFINES WRK-DT-ANOMES.
             10 WRK-AAAA               PIC 9(04).
             10 WRK-MM                 PIC 9(02).

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*       AREA PARA CHAVES       *'.
      *----------------------------------------------------------------*

       01  WRK-CHV-APURA22A.
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

       01  WRK-FS-APURA22A             PIC X(02)           VALUE SPACES.
       01  WRK-FS-DADOSIGB             PIC X(02)           VALUE SPACES.
       01  WRK-FS-APURA22B             PIC X(02)           VALUE SPACES.
       01  WRK-FS-SAIDA22B             PIC X(02)           VALUE SPACES.
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
           '*  AREA PARA APURA22A/APURA22B *'.
      *-----------------------------------------------------------------

       COPY GFCTWBBE.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*       AREA PARA DADOSIGB     *'.
      *----------------------------------------------------------------*

       COPY GFCTWBBD.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*       AREA PARA SAIDA22B     *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTVA'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     AREA PARA BRAD7100       *'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*   FIM  DA WORKING GFCT4454   *'.
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
                    WRK-CHV-APURA22A   EQUAL HIGH-VALUES.

           PERFORM 1300-PROCEDIMENTOS-FINAIS.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA INICIALIZAR O PROGRAMA                          *
      *----------------------------------------------------------------*
       0100-INICIALIZAR-PROGRAMA       SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT APURA22A
                      DADOSIGB
               OUTPUT APURA22B
                      SAIDA22B.

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

           PERFORM 0300-TESTAR-FS-APURA22A.

           PERFORM 0400-TESTAR-FS-DADOSIGB.

           PERFORM 0500-TESTAR-FS-APURA22B.

           PERFORM 0600-TESTAR-FS-SAIDA22B.

      *----------------------------------------------------------------*
       0200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO APURA22A                      *
      *----------------------------------------------------------------*
       0300-TESTAR-FS-APURA22A         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-APURA22A         NOT EQUAL '00'
               MOVE 'APURA22A'         TO WRK-NOME-ARQ
               MOVE WRK-FS-APURA22A    TO WRK-FS
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
      *    TESTAR FILE-STATUS DO ARQUIVO APURA22B                      *
      *----------------------------------------------------------------*
       0500-TESTAR-FS-APURA22B         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-APURA22B         NOT EQUAL '00'
               MOVE 'APURA22B'         TO WRK-NOME-ARQ
               MOVE WRK-FS-APURA22B    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO SAIDA22B                      *
      *----------------------------------------------------------------*
       0600-TESTAR-FS-SAIDA22B         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SAIDA22B         NOT EQUAL '00'
               MOVE 'SAIDA22B'         TO WRK-NOME-ARQ
               MOVE WRK-FS-SAIDA22B    TO WRK-FS
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

           PERFORM 0800-LER-APURA22A.

           IF  WRK-CHV-APURA22A        EQUAL HIGH-VALUES
               DISPLAY '*********** GFCT4454 ***********'
               DISPLAY '*                              *'
               DISPLAY '*    ARQUIVO  APURA22A VAZIO   *'
               DISPLAY '*    PROCESSAMENTO ENCERRADO   *'
               DISPLAY '*                              *'
               DISPLAY '*********** GFCT4454 ***********'
           END-IF.

           PERFORM 0900-LER-DADOSIGB.

           IF  WRK-CHV-SIGB            EQUAL HIGH-VALUES
               DISPLAY '*********** GFCT4454 ***********'
               DISPLAY '*                              *'
               DISPLAY '*    ARQUIVO DADOSIGB VAZIO    *'
               DISPLAY '*                              *'
               DISPLAY '*********** GFCT4454 ***********'
           END-IF.

      *----------------------------------------------------------------*
       0700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LEITURA DO ARQUIVO APURA22A                                 *
      *----------------------------------------------------------------*
       0800-LER-APURA22A               SECTION.
      *----------------------------------------------------------------*

           READ APURA22A               INTO GFCTWBBE-REG.

           IF  WRK-FS-APURA22A         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHV-APURA22A
               GO                      TO 0800-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 0300-TESTAR-FS-APURA22A.

           MOVE WBBE-CPF-CNPJ                        TO WRK-S-909.
           MOVE WRK-909                              TO WRK-CNPJ-22A.
           MOVE WBBE-FILIAL                          TO WRK-S-905.
           MOVE WRK-905                              TO WRK-FILIAL-22A.
           MOVE WBBE-CONTROLE                        TO WRK-CTRL-22A.
           MOVE WBBE-CAG-DSTNO-MOVTO                 TO WRK-S-905.
           MOVE WRK-905                              TO WRK-AG-22A.
           MOVE WBBE-23-CCART-ANLSE-GERC             TO WRK-S-905.
           MOVE WRK-905                              TO WRK-CART-22A.


           MOVE WBBE-DOCOR-EVNTO                     TO WRK-DATA-10.
           MOVE WRK-MM-10                            TO WRK-MM.
           MOVE WRK-AAAA-10                          TO WRK-AAAA.
           MOVE WRK-DT-ANOMES                        TO WRK-ANOMES-22A.

           ADD 1                         TO ACU-LIDOS-APURA22A.

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

           MOVE WBBD-CPF-CNPJ-PRINC      TO WRK-S-909.
           MOVE WRK-909                  TO WRK-CNPJ-SIGB.
           MOVE WBBD-CPF-CNPJ-FILIAL     TO WRK-S-905.
           MOVE WRK-905                  TO WRK-FILIAL-SIGB.
           MOVE WBBD-CPF-CNPJ-CTR        TO WRK-CTRL-SIGB.
           MOVE WBBD-AGEN                TO WRK-S-905.
           MOVE WRK-905                  TO WRK-AG-SIGB.
           MOVE WBBD-CARTEIRA-SIGB       TO WRK-S-905.
           MOVE WRK-905                  TO WRK-CART-SIGB.

           MOVE WBBD-AAAA-MM-APURACAO    TO WRK-ANOMES-SIGB.


           ADD 1                       TO ACU-LIDOS-DADOSIGB.

      *----------------------------------------------------------------*
       0900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL DO PROGRAMA                         *
      *----------------------------------------------------------------*
       1000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

ACERTO*---------* BALANCE LINE *---------------------------------------*

           IF  WRK-CHV-APURA22A      EQUAL WRK-CHV-SIGB
               PERFORM 1100-GRAVAR-APURA22B
               PERFORM 0800-LER-APURA22A
           ELSE
               IF  WRK-CHV-APURA22A  LESS WRK-CHV-SIGB
                   PERFORM 1200-GRAVAR-SAIDA22B
                   PERFORM 0800-LER-APURA22A
               ELSE
                   PERFORM 0900-LER-DADOSIGB
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    GRAVAR ARQUIVO  APURA22B                                    *
      *----------------------------------------------------------------*
       1100-GRAVAR-APURA22B            SECTION.
      *----------------------------------------------------------------*

ACERTO     MOVE WBBD-VR-TOT-QTDE  TO WBBE-CONSE-DADO-CONS.

           WRITE FD-APURA22B      FROM GFCTWBBE-REG.

           MOVE WRK-GRAVACAO      TO WRK-OPERACAO.

           PERFORM 0500-TESTAR-FS-APURA22B.

           ADD 1                  TO ACU-GRAVADOS-APURA22B.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    GRAVAR ARQUIVO SAIDA22B                                     *
      *----------------------------------------------------------------*
       1200-GRAVAR-SAIDA22B            SECTION.
      *----------------------------------------------------------------*


ACERTO     MOVE WBBE-REG-GFCTVA        TO GFCTVA-GFCTB092.

           WRITE FD-SAIDA22B           FROM GFCTVA-GFCTB092.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 0600-TESTAR-FS-SAIDA22B.

           ADD 1                       TO ACU-GRAVADOS-SAIDA22B.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA FINALIZACAO DO PROGRAMA                         *
      *----------------------------------------------------------------*
       1300-PROCEDIMENTOS-FINAIS       SECTION.
      *----------------------------------------------------------------*

           PERFORM 1400-EMITIR-DISPLAY.

           CLOSE APURA22A
                 DADOSIGB
                 APURA22B
                 SAIDA22B.

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

           MOVE ACU-LIDOS-APURA22A     TO WRK-MASCARA.

           DISPLAY '******************** GFCT4454 *******************'
           '**'.

           DISPLAY '*                                     '
           '            *'.
           DISPLAY '* REGISTROS LIDOS EM APURA22A......: '
           ' ' WRK-MASCARA ' *'.

           MOVE ACU-LIDOS-DADOSIGB     TO WRK-MASCARA.

           DISPLAY '* REGISTROS LIDOS EM DADOSIGB......: '
           ' ' WRK-MASCARA ' *'.

           MOVE ACU-GRAVADOS-SAIDA22B  TO WRK-MASCARA.

           DISPLAY '* REGISTROS GRAVADOS NO SAIDA22B...: '
           ' ' WRK-MASCARA ' *'.

           MOVE ACU-GRAVADOS-APURA22B  TO WRK-MASCARA.

           DISPLAY '* REGISTROS GRAVADOS NO APURA22B...: '
           ' ' WRK-MASCARA ' *'.

           DISPLAY '*                                     '
           '            *'.
           DISPLAY '******************** GFCT4454 *******************'
           '**'.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAMENTO DE ERRO                              *
      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT4454'             TO ERR-PGM.
           MOVE 'APL'                  TO ERR-TIPO-ACESSO.

           CALL 'BRAD7100'             USING WRK-BATCH
                                             ERRO-AREA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------



