      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. GFCT2097.
       AUTHOR.     VINICIUS CRISTIAN MADUREIRA.
      *================================================================*
      *      S O N D A  P R O C W O R K  -  C O N S U L T O R I A      *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT2097                                    *
      *    PROGRAMADOR.:   VINICIUS CRISTIAN MADUREIRA - SONDPROC/GP.50*
      *    ANALISTA....:   VINICIUS CRISTIAN MADUREIRA - SONDPROC/GP.50*
      *    DATA........:   14/08/2008                                  *
      *                                                                *
      *    OBJETIVO    :   EXCLUIR CONTAS SALARIO.                     *
      *                                                                *
      *    ARQUIVOS:                                                   *
      *      DDNAME                                    INCLUDE/BOOK    *
      *      ADESAO                                    I#GFCTD0        *
      *      CTASALAR                                  I#SCMPSL        *
      *      SADESAO                                   I#GFCTD0        *
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

           SELECT ADESAO   ASSIGN      TO UT-S-ADESAO
                      FILE STATUS      IS WRK-FS-ADESAO.

           SELECT CTASALAR ASSIGN      TO UT-S-CTASALAR
                      FILE STATUS      IS WRK-FS-CTASALAR.

           SELECT SADESAO ASSIGN       TO UT-S-SADESAO
                      FILE STATUS      IS WRK-FS-SADESAO.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:                                                      *
      *            ORG. SEQUENTIAL     -  LRECL = 200                  *
      *----------------------------------------------------------------*

       FD  ADESAO
           RECORDING MODE IS F
           LABEL RECORDS IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTD0'.

      *----------------------------------------------------------------*
      *    INPUT:                                                      *
      *            ORG. SEQUENTIAL     -  LRECL = 100                  *
      *----------------------------------------------------------------*

       FD  CTASALAR
           RECORDING MODE IS F
           LABEL RECORDS IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#SCMPSL'.

      *----------------------------------------------------------------*
      *    OUTPUT:                                                     *
      *            ORG. SEQUENTIAL     -  LRECL = 200                  *
      *----------------------------------------------------------------*

       FD  SADESAO
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-SADESAO          PIC X(200).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  INICIO DA WORKING GFCT2097  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*        ACUMULADORES          *'.
      *----------------------------------------------------------------*

       01  ACU-ACUMULADORES.
           05 ACU-LIDOS-ADESAO         PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-LIDOS-CTASALAR       PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-GRAVADOS-SADESAO     PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-DESPREZADOS          PIC 9(11) COMP-3    VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                       PIC X(32)           VALUE
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

       01  WRK-CHV-ADESAO.
           05 WRK-AG-ADESAO            PIC 9(05)         VALUE ZEROS.
           05 WRK-CTA-ADESAO           PIC 9(07)         VALUE ZEROS.

       01  WRK-CHV-CTASALAR.
           05 WRK-AG-CTASALAR          PIC 9(05)         VALUE ZEROS.
           05 WRK-CTA-CTASALAR         PIC 9(07)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    AREA PARA FILE STATUS     *'.
      *----------------------------------------------------------------*

       01  WRK-FS-ADESAO             PIC X(02)           VALUE SPACES.
       01  WRK-FS-CTASALAR             PIC X(02)           VALUE SPACES.
       01  WRK-FS-SADESAO             PIC X(02)           VALUE SPACES.
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
           '*   FIM  DA WORKING GFCT2097   *'.
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
                    WRK-FS-ADESAO      EQUAL '10'.

           PERFORM 1300-PROCEDIMENTOS-FINAIS.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA INICIALIZAR O PROGRAMA                          *
      *----------------------------------------------------------------*
       0100-INICIALIZAR-PROGRAMA       SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT ADESAO
                      CTASALAR
               OUTPUT SADESAO.

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

           PERFORM 0300-TESTAR-FS-ADESAO.

           PERFORM 0400-TESTAR-FS-CTASALAR.

           PERFORM 0500-TESTAR-FS-SADESAO.

      *----------------------------------------------------------------*
       0200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO ADESAO                        *
      *----------------------------------------------------------------*
       0300-TESTAR-FS-ADESAO         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ADESAO         NOT EQUAL '00'
               MOVE 'ADESAO'         TO WRK-NOME-ARQ
               MOVE WRK-FS-ADESAO    TO WRK-FS
               MOVE WRK-ERRO-FS      TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO CTASALAR                      *
      *----------------------------------------------------------------*
       0400-TESTAR-FS-CTASALAR         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-CTASALAR         NOT EQUAL '00'
               MOVE 'CTASALAR'         TO WRK-NOME-ARQ
               MOVE WRK-FS-CTASALAR    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO SADESAO                      *
      *----------------------------------------------------------------*
       0500-TESTAR-FS-SADESAO         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SADESAO         NOT EQUAL '00'
               MOVE 'SADESAO'         TO WRK-NOME-ARQ
               MOVE WRK-FS-SADESAO    TO WRK-FS
               MOVE WRK-ERRO-FS       TO ERR-TEXTO
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

           PERFORM 0800-LER-ADESAO.

           IF  WRK-FS-ADESAO EQUAL '10'
               DISPLAY '*********** GFCT2097 ***********'
               DISPLAY '*                              *'
               DISPLAY '*    ARQUIVO  ADESAO VAZIO     *'
               DISPLAY '*    PROCESSAMENTO ENCERRADO   *'
               DISPLAY '*                              *'
               DISPLAY '*********** GFCT2097 ***********'
           END-IF.

           PERFORM 0900-LER-CTASALAR.

           IF  WRK-FS-CTASALAR EQUAL '10'
               DISPLAY '*********** GFCT2097 ***********'
               DISPLAY '*                              *'
               DISPLAY '*    ARQUIVO CTASALAR VAZIO    *'
               DISPLAY '*                              *'
               DISPLAY '*********** GFCT2097 ***********'
           END-IF.

      *----------------------------------------------------------------*
       0700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LEITURA DO ARQUIVO ADESAO                                   *
      *----------------------------------------------------------------*
       0800-LER-ADESAO               SECTION.
      *----------------------------------------------------------------*

           READ ADESAO.

           IF  WRK-FS-ADESAO           EQUAL '10'
               GO                      TO 0800-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 0300-TESTAR-FS-ADESAO.

           MOVE D0-AGENCIA             TO WRK-AG-ADESAO.
           MOVE D0-CONTA               TO WRK-CTA-ADESAO

           ADD 1                       TO ACU-LIDOS-ADESAO.

      *----------------------------------------------------------------*
       0800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LEITURA DO ARQUIVO CTASALAR                                 *
      *----------------------------------------------------------------*
       0900-LER-CTASALAR               SECTION.
      *----------------------------------------------------------------*

           READ CTASALAR.

           IF  WRK-FS-CTASALAR         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHV-CTASALAR
               GO                      TO 0900-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 0400-TESTAR-FS-CTASALAR.

           MOVE CS-AGENCIA             TO WRK-AG-CTASALAR.
           MOVE CS-CONTA               TO WRK-CTA-CTASALAR.

           ADD 1                       TO ACU-LIDOS-CTASALAR.

      *----------------------------------------------------------------*
       0900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL DO PROGRAMA                         *
      *----------------------------------------------------------------*
       1000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           IF  WRK-CHV-ADESAO      EQUAL WRK-CHV-CTASALAR
               ADD 1 TO ACU-DESPREZADOS
               PERFORM 0800-LER-ADESAO
           ELSE
               IF  WRK-CHV-ADESAO  LESS WRK-CHV-CTASALAR
                   PERFORM 1100-GRAVAR-SADESAO
                   PERFORM 0800-LER-ADESAO
               ELSE
                   PERFORM 0900-LER-CTASALAR
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    GRAVAR ARQUIVO  SADESAO                                    *
      *----------------------------------------------------------------*
       1100-GRAVAR-SADESAO            SECTION.
      *----------------------------------------------------------------*

           MOVE  D0-REGISTRO    TO FD-REG-SADESAO.

           WRITE FD-REG-SADESAO OF SADESAO.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 0500-TESTAR-FS-SADESAO.

           ADD 1                       TO ACU-GRAVADOS-SADESAO.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA FINALIZACAO DO PROGRAMA                         *
      *----------------------------------------------------------------*
       1300-PROCEDIMENTOS-FINAIS       SECTION.
      *----------------------------------------------------------------*

           PERFORM 1400-EMITIR-DISPLAY.

           CLOSE ADESAO
                 CTASALAR
                 SADESAO.

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

           DISPLAY '******************** GFCT2097 ********************'.
           DISPLAY '*                                                *'.
           MOVE ACU-LIDOS-ADESAO     TO WRK-MASCARA.
           DISPLAY '*  LIDOS EM ADESAO........: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-LIDOS-CTASALAR     TO WRK-MASCARA.
           DISPLAY '*  LIDOS EM CONTA SALARIO.: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-GRAVADOS-SADESAO  TO WRK-MASCARA.
           DISPLAY '*  GRAVADOS NA SAIDA......: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-DESPREZADOS        TO WRK-MASCARA.
           DISPLAY '*  REG. COM CONTA SALARIO.: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           DISPLAY '******************** GFCT2097 ********************'.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAMENTO DE ERRO                              *
      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT2097'             TO ERR-PGM.
           MOVE 'APL'                  TO ERR-TIPO-ACESSO.

           CALL 'BRAD7100'             USING WRK-BATCH
                                             ERRO-AREA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------
