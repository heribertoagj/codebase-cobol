      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. GFCT2759.
       AUTHOR.     VINICIUS CRISTIAN MADUREIRA.
      *================================================================*
      *      S O N D A  P R O C W O R K  -  C O N S U L T O R I A      *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT2759                                    *
      *    PROGRAMADOR.:   VINICIUS CRISTIAN MADUREIRA - SONDPROC/GP.50*
      *    ANALISTA....:   VINICIUS CRISTIAN MADUREIRA - SONDPROC/GP.50*
      *    DATA........:   17/07/2009                                  *
      *                                                                *
      *    OBJETIVO    :   BALANCE LINE ENTRE REGISTROS DA TABELA      *
      *                    GFCTB0L4 MAE E GFCTB0L5 FILHA.              *
      *                    TIPO DE AGRUPAMENTO - CLIENTE.              *
      *                                                                *
      *    ARQUIVOS:                                                   *
      *      DDNAME                                    INCLUDE/BOOK    *
      *      GFCTB0L4                                    GFCTWAIB      *
      *      TBCLIENT                                    GFCTWAIC      *
      *      AGRUPCLI                                    GFCTWAIC      *
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

           SELECT GFCTB0L4 ASSIGN      TO UT-S-GFCTB0L4
                      FILE STATUS      IS WRK-FS-GFCTB0L4.

           SELECT TBCLIENT ASSIGN      TO UT-S-TBCLIENT
                      FILE STATUS      IS WRK-FS-TBCLIENT.

           SELECT AGRUPCLI ASSIGN      TO UT-S-AGRUPCLI
                      FILE STATUS      IS WRK-FS-AGRUPCLI.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:                                                      *
      *            ORG. SEQUENTIAL     -  LRECL = 581                  *
      *----------------------------------------------------------------*

       FD  GFCTB0L4
           RECORDING MODE IS F
           LABEL RECORDS IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'GFCTWAIB'.

      *----------------------------------------------------------------*
      *    INPUT:                                                      *
      *            ORG. SEQUENTIAL     -  LRECL = 039                  *
      *----------------------------------------------------------------*

       FD  TBCLIENT
           RECORDING MODE IS F
           LABEL RECORDS IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'GFCTWAIC'.

      *----------------------------------------------------------------*
      *    OUTPUT:                                                     *
      *            ORG. SEQUENTIAL     -  LRECL = 039                  *
      *----------------------------------------------------------------*

       FD  AGRUPCLI
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'GFCTWAIC'.

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  INICIO DA WORKING GFCT2759  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*        ACUMULADORES          *'.
      *----------------------------------------------------------------*

       01  ACU-ACUMULADORES.
           05 ACU-LIDOS-GFCTB0L4       PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-LIDOS-TBCLIENT       PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-GRAVADOS-AGRUPCLI    PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-DESPREZADOS          PIC 9(11) COMP-3    VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    VARIAVEIS AUXILIARES      *'.
      *----------------------------------------------------------------*

       01  WRK-AUXILIARES.
           05 WRK-BATCH                PIC X(08)            VALUE
              'BATCH'.
           05 WRK-MASCARA              PIC ZZ.ZZZ.ZZZ.Z99   VALUE ZEROS.

       01  WRK-CHV-GFCTB0L4.
           03 WRK-CAGPTO-B0L4          PIC  9(003) COMP-3 VALUE ZEROS.
           03 WRK-CINDCD-EMIS-B0L4     PIC  9(001) COMP-3 VALUE ZEROS.
           03 WRK-HINCL-SIST-B0L4      PIC  X(026)        VALUE SPACES.

       01  WRK-CHV-TBCLIENT.
           03 WRK-CAGPTO-B0L5          PIC  9(003) COMP-3 VALUE ZEROS.
           03 WRK-CINDCD-EMIS-B0L5     PIC  9(001) COMP-3 VALUE ZEROS.
           03 WRK-HINCL-SIST-B0L5      PIC  X(026)        VALUE SPACES.

       01  WRK-S9-3                    PIC S9(003)          VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-S9-3.
           05 WRK-9-3                  PIC  9(003).

       01  WRK-S9-1                    PIC S9(001)          VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-S9-1.
           05 WRK-9-1                  PIC  9(001).

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    AREA PARA FILE STATUS     *'.
      *----------------------------------------------------------------*

       01  WRK-FS-GFCTB0L4             PIC X(02)           VALUE SPACES.
       01  WRK-FS-TBCLIENT             PIC X(02)           VALUE SPACES.
       01  WRK-FS-AGRUPCLI             PIC X(02)           VALUE SPACES.
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
           '*   FIM  DA WORKING GFCT2759   *'.
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
                    WRK-CHV-TBCLIENT   EQUAL HIGH-VALUES.

           PERFORM 1300-PROCEDIMENTOS-FINAIS.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA INICIALIZAR O PROGRAMA                          *
      *----------------------------------------------------------------*
       0100-INICIALIZAR-PROGRAMA       SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT GFCTB0L4
                      TBCLIENT
               OUTPUT AGRUPCLI.

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

           PERFORM 0300-TESTAR-FS-GFCTB0L4.

           PERFORM 0400-TESTAR-FS-TBCLIENT.

           PERFORM 0600-TESTAR-FS-AGRUPCLI.

      *----------------------------------------------------------------*
       0200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO GFCTB0L4                      *
      *----------------------------------------------------------------*
       0300-TESTAR-FS-GFCTB0L4         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-GFCTB0L4         NOT EQUAL '00'
               MOVE 'GFCTB0L4'         TO WRK-NOME-ARQ
               MOVE WRK-FS-GFCTB0L4    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO TBCLIENT                      *
      *----------------------------------------------------------------*
       0400-TESTAR-FS-TBCLIENT         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-TBCLIENT         NOT EQUAL '00'
               MOVE 'TBCLIENT'         TO WRK-NOME-ARQ
               MOVE WRK-FS-TBCLIENT    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO AGRUPCLI                      *
      *----------------------------------------------------------------*
       0600-TESTAR-FS-AGRUPCLI         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-AGRUPCLI         NOT EQUAL '00'
               MOVE 'AGRUPCLI'         TO WRK-NOME-ARQ
               MOVE WRK-FS-AGRUPCLI    TO WRK-FS
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

           PERFORM 0800-LER-GFCTB0L4.

           IF  WRK-CHV-GFCTB0L4        EQUAL HIGH-VALUES
               DISPLAY '*********** GFCT2759 ***********'
               DISPLAY '*                              *'
               DISPLAY '*    ARQUIVO  GFCTB0L4 VAZIO   *'
               DISPLAY '*    PROCESSAMENTO ENCERRADO   *'
               DISPLAY '*                              *'
               DISPLAY '*********** GFCT2759 ***********'
           END-IF.

           PERFORM 0900-LER-TBCLIENT.

           IF  WRK-CHV-TBCLIENT            EQUAL HIGH-VALUES
               DISPLAY '*********** GFCT2759 ***********'
               DISPLAY '*                              *'
               DISPLAY '*    ARQUIVO TBCLIENT VAZIO    *'
               DISPLAY '*    PROCESSAMENTO ENCERRADO   *'
               DISPLAY '*                              *'
               DISPLAY '*********** GFCT2759 ***********'
           END-IF.

      *----------------------------------------------------------------*
       0700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LEITURA DO ARQUIVO GFCTB0L4                                 *
      *----------------------------------------------------------------*
       0800-LER-GFCTB0L4               SECTION.
      *----------------------------------------------------------------*

           READ GFCTB0L4.

           IF  WRK-FS-GFCTB0L4         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHV-GFCTB0L4
               GO                      TO 0800-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 0300-TESTAR-FS-GFCTB0L4.

           MOVE WAIB-CAGPTO-CTA        TO WRK-S9-3.
           MOVE WRK-9-3                TO WRK-CAGPTO-B0L4.
           MOVE WAIB-CINDCD-AUTRZ-EMIS TO WRK-S9-1.
           MOVE WRK-9-1                TO WRK-CINDCD-EMIS-B0L4.
           MOVE WAIB-HINCL-REG-SIST    TO WRK-HINCL-SIST-B0L4.

           ADD 1                       TO ACU-LIDOS-GFCTB0L4.

      *----------------------------------------------------------------*
       0800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LEITURA DO ARQUIVO TBCLIENT                                 *
      *----------------------------------------------------------------*
       0900-LER-TBCLIENT               SECTION.
      *----------------------------------------------------------------*

           READ TBCLIENT.

           IF  WRK-FS-TBCLIENT         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHV-TBCLIENT
               GO                      TO 0900-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 0400-TESTAR-FS-TBCLIENT.

           MOVE WAIC-CAGPTO-CTA OF TBCLIENT TO WRK-S9-3.
           MOVE WRK-9-3                     TO WRK-CAGPTO-B0L5.
           MOVE WAIC-CINDCD-AUTRZ-EMIS OF TBCLIENT TO WRK-S9-1.
           MOVE WRK-9-1                TO WRK-CINDCD-EMIS-B0L5.
           MOVE WAIC-HINCL-REG-SIST    OF TBCLIENT
                                       TO WRK-HINCL-SIST-B0L5.

           ADD 1                       TO ACU-LIDOS-TBCLIENT.

      *----------------------------------------------------------------*
       0900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL DO PROGRAMA                         *
      *----------------------------------------------------------------*
       1000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           IF  WRK-CHV-TBCLIENT      EQUAL WRK-CHV-GFCTB0L4
               PERFORM 1200-GRAVAR-AGRUPCLI
               PERFORM 0900-LER-TBCLIENT
           ELSE
               IF  WRK-CHV-TBCLIENT  LESS WRK-CHV-GFCTB0L4
                   ADD 1         TO ACU-DESPREZADOS
                   PERFORM 0900-LER-TBCLIENT
               ELSE
                   PERFORM 0800-LER-GFCTB0L4
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    GRAVAR ARQUIVO  AGRUPCLI                                    *
      *----------------------------------------------------------------*
       1200-GRAVAR-AGRUPCLI            SECTION.
      *----------------------------------------------------------------*

           MOVE REG-WAIC OF TBCLIENT TO REG-WAIC OF AGRUPCLI.

           WRITE REG-WAIC         OF AGRUPCLI.

           MOVE WRK-GRAVACAO      TO WRK-OPERACAO.

           PERFORM 0600-TESTAR-FS-AGRUPCLI.

           ADD 1                  TO ACU-GRAVADOS-AGRUPCLI.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA FINALIZACAO DO PROGRAMA                         *
      *----------------------------------------------------------------*
       1300-PROCEDIMENTOS-FINAIS       SECTION.
      *----------------------------------------------------------------*

           PERFORM 1400-EMITIR-DISPLAY.

           CLOSE GFCTB0L4
                 TBCLIENT
                 AGRUPCLI.

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

           DISPLAY '******************** GFCT2759 ********************'.
           DISPLAY '*                                                *'.
           MOVE ACU-LIDOS-GFCTB0L4     TO WRK-MASCARA.
           DISPLAY '*  LIDOS EM GFCTB0L4......: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-LIDOS-TBCLIENT     TO WRK-MASCARA.
           DISPLAY '*  LIDOS EM TBCLIENT......: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-GRAVADOS-AGRUPCLI  TO WRK-MASCARA.
           DISPLAY '*  GRAV - AGRUP. CLIENTES.: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-DESPREZADOS        TO WRK-MASCARA.
           DISPLAY '*  DESPREZADOS - CLIENTES EXCLUIDOS              *'.
           DISPLAY '*  TABELA GFCTB0L4........: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           DISPLAY '******************** GFCT2759 ********************'.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAMENTO DE ERRO                              *
      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT2759'             TO ERR-PGM.
           MOVE 'APL'                  TO ERR-TIPO-ACESSO.

           CALL 'BRAD7100'             USING WRK-BATCH
                                             ERRO-AREA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *---------------------------------------------------------------

