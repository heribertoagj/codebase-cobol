
***************************** Top of Data ******************************
      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. GFCT2118.
       AUTHOR.     VINICIUS CRISTIAN MADUREIRA.
      *================================================================*
      *      S O N D A  P R O C W O R K  -  C O N S U L T O R I A      *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT2118                                    *
      *    PROGRAMADOR.:   VINICIUS CRISTIAN MADUREIRA - SONDPROC/GP.50*
      *    ANALISTA....:   VINICIUS CRISTIAN MADUREIRA - SONDPROC/GP.50*
      *    DATA........:   16/08/2009                                  *
      *                                                                *
      *    OBJETIVO    :   COMPLETA POUPANCA COM INFORMACOES DE        *
      *                    CNPJ/CPF.                                   *
      *                                                                *
      *    ARQUIVOS:                                                   *
      *      DDNAME                                    INCLUDE/BOOK    *
      *      POUPADES                                    I#GFCTRU      *
      *      CADPREDU                                    I#CHEQ04      *

      *      POUPADE1                                    I#GFCTRU      *
      *      POUPSCAD                                    I#GFCTRU      *
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

           SELECT POUPADES ASSIGN      TO UT-S-POUPADES
                      FILE STATUS      IS WRK-FS-POUPADES.

           SELECT CADPREDU ASSIGN      TO UT-S-CADPREDU
                      FILE STATUS      IS WRK-FS-CADPREDU.

           SELECT POUPSCAD ASSIGN      TO UT-S-POUPSCAD
                      FILE STATUS      IS WRK-FS-POUPSCAD.

           SELECT POUPADE1 ASSIGN      TO UT-S-POUPADE1
                      FILE STATUS      IS WRK-FS-POUPADE1.


      *================================================================*
       DATA                            DIVISION.

      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:                                                      *
      *            ORG. SEQUENTIAL     -  LRECL = 040                  *
      *----------------------------------------------------------------*

       FD  POUPADES
           RECORDING MODE IS F
           LABEL RECORDS IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTRU'.

      *----------------------------------------------------------------*
      *    INPUT:                                                      *
      *            ORG. SEQUENTIAL     -  LRECL = 025                  *

      *----------------------------------------------------------------*

       FD  CADPREDU
           RECORDING MODE IS F
           LABEL RECORDS IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#CHEQ04'.

      *----------------------------------------------------------------*
      *    OUTPUT:                                                     *
      *            ORG. SEQUENTIAL     -  LRECL = 040                  *
      *----------------------------------------------------------------*

       FD  POUPSCAD
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTRU'.


      *----------------------------------------------------------------*
      *    OUTPUT:                                                     *
      *            ORG. SEQUENTIAL     -  LRECL = 040                  *
      *----------------------------------------------------------------*

       FD  POUPADE1
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTRU'.

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  INICIO DA WORKING GFCT2118  *'.
      *----------------------------------------------------------------*


      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*        ACUMULADORES          *'.
      *----------------------------------------------------------------*

       01  ACU-ACUMULADORES.
           05 ACU-LIDOS-POUPADES       PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-LIDOS-CADPREDU       PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-GRAVADOS-POUPSCAD    PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-GRAVADOS-POUPADE1    PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-DESPREZADOS          PIC 9(11) COMP-3    VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    VARIAVEIS AUXILIARES      *'.
      *----------------------------------------------------------------*

       01  WRK-AUXILIARES.
           05 WRK-BATCH                PIC X(08)            VALUE
              'BATCH'.
           05 WRK-MASCARA              PIC ZZ.ZZZ.ZZZ.Z99   VALUE ZEROS.


       01  WRK-CHV-POUPADES.
           03 WRK-AGENCIA-POUPADES     PIC 9(005)           VALUE ZEROS.
           03 WRK-CONTA-POUPADES       PIC 9(007)           VALUE ZEROS.

       01  WRK-CHV-CADPREDU.
           03 WRK-AGENCIA-CADPREDU     PIC 9(005)           VALUE ZEROS.
           03 WRK-CONTA-CADPREDU       PIC 9(007)           VALUE ZEROS.

       01  WRK-S9-5                    PIC S9(005)          VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-S9-5.
           05 WRK-9-5                  PIC  9(005).

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    AREA PARA FILE STATUS     *'.
      *----------------------------------------------------------------*

       01  WRK-FS-POUPADES             PIC X(02)           VALUE SPACES.
       01  WRK-FS-POUPADE1             PIC X(02)           VALUE SPACES.
       01  WRK-FS-CADPREDU             PIC X(02)           VALUE SPACES.

       01  WRK-FS-POUPSCAD             PIC X(02)           VALUE SPACES.
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
           '*   FIM  DA WORKING GFCT2118   *'.
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
                    WRK-CHV-POUPADES   EQUAL HIGH-VALUES.

           PERFORM 1300-PROCEDIMENTOS-FINAIS.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      *----------------------------------------------------------------*
      *    ROTINA PARA INICIALIZAR O PROGRAMA                          *
      *----------------------------------------------------------------*
       0100-INICIALIZAR-PROGRAMA       SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT POUPADES
                      CADPREDU
               OUTPUT POUPSCAD
                      POUPADE1.

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

           PERFORM 0300-TESTAR-FS-POUPADES.

           PERFORM 0400-TESTAR-FS-CADPREDU.

           PERFORM 0500-TESTAR-FS-POUPSCAD.

           PERFORM 0600-TESTAR-FS-POUPADE1.

      *----------------------------------------------------------------*
       0200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO POUPADES                      *
      *----------------------------------------------------------------*
       0300-TESTAR-FS-POUPADES         SECTION.

      *----------------------------------------------------------------*

           IF  WRK-FS-POUPADES         NOT EQUAL '00'
               MOVE 'POUPADES'         TO WRK-NOME-ARQ
               MOVE WRK-FS-POUPADES    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO CADPREDU                      *
      *----------------------------------------------------------------*
       0400-TESTAR-FS-CADPREDU         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-CADPREDU         NOT EQUAL '00'
               MOVE 'CADPREDU'         TO WRK-NOME-ARQ

               MOVE WRK-FS-CADPREDU    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO POUPSCAD                      *
      *----------------------------------------------------------------*
       0500-TESTAR-FS-POUPSCAD         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-POUPSCAD         NOT EQUAL '00'
               MOVE 'POUPSCAD'         TO WRK-NOME-ARQ
               MOVE WRK-FS-POUPSCAD    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.


      *----------------------------------------------------------------*
       0500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO POUPADE1                      *
      *----------------------------------------------------------------*
       0600-TESTAR-FS-POUPADE1         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-POUPADE1         NOT EQUAL '00'
               MOVE 'POUPADE1'         TO WRK-NOME-ARQ
               MOVE WRK-FS-POUPADE1    TO WRK-FS
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

           PERFORM 0800-LER-POUPADES.

           IF  WRK-CHV-POUPADES        EQUAL HIGH-VALUES
               DISPLAY '*********** GFCT2118 ***********'
               DISPLAY '*                              *'
               DISPLAY '*    ARQUIVO  POUPADES VAZIO   *'
               DISPLAY '*    PROCESSAMENTO ENCERRADO   *'
               DISPLAY '*                              *'
               DISPLAY '*********** GFCT2118 ***********'
           END-IF.

           PERFORM 0900-LER-CADPREDU.

           IF  WRK-CHV-CADPREDU        EQUAL HIGH-VALUES

               DISPLAY '*********** GFCT2118 ***********'
               DISPLAY '*                              *'
               DISPLAY '*    ARQUIVO CADPREDU VAZIO    *'
               DISPLAY '*                              *'
               DISPLAY '*********** GFCT2118 ***********'
           END-IF.

      *----------------------------------------------------------------*
       0700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LEITURA DO ARQUIVO POUPADES                                 *
      *----------------------------------------------------------------*
       0800-LER-POUPADES               SECTION.
      *----------------------------------------------------------------*

           READ POUPADES.

           IF  WRK-FS-POUPADES         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHV-POUPADES

               GO                      TO 0800-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 0300-TESTAR-FS-POUPADES.

           MOVE GFCTRU-AGENCIA         OF POUPADES TO WRK-S9-5.
           MOVE WRK-9-5                TO WRK-AGENCIA-POUPADES
           MOVE GFCTRU-CONTA           OF POUPADES TO WRK-CONTA-POUPADES

           ADD 1                       TO ACU-LIDOS-POUPADES.

      *----------------------------------------------------------------*
       0800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LEITURA DO ARQUIVO CADPREDU                                 *
      *----------------------------------------------------------------*
       0900-LER-CADPREDU               SECTION.

      *----------------------------------------------------------------*

           READ CADPREDU.

           IF  WRK-FS-CADPREDU         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHV-CADPREDU
               GO                      TO 0900-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 0400-TESTAR-FS-CADPREDU.

           MOVE CHEQ-AGE               TO WRK-AGENCIA-CADPREDU.
           MOVE CHEQ-CTA               TO WRK-CONTA-CADPREDU.

           ADD 1                       TO ACU-LIDOS-CADPREDU.

      *----------------------------------------------------------------*
       0900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL DO PROGRAMA                         *
      *----------------------------------------------------------------*
       1000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           IF CHEQ-CTA LESS 1000000
               ADD 1 TO ACU-DESPREZADOS
               PERFORM 0900-LER-CADPREDU
           ELSE
               IF  WRK-CHV-POUPADES      EQUAL WRK-CHV-CADPREDU
                   PERFORM 1100-GRAVAR-POUPADE1
                   PERFORM 0800-LER-POUPADES
               ELSE
                   IF  WRK-CHV-POUPADES  LESS WRK-CHV-CADPREDU
                       PERFORM 1200-GRAVAR-POUPSCAD
                       PERFORM 0800-LER-POUPADES
                   ELSE
                       PERFORM 0900-LER-CADPREDU
                   END-IF

               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    GRAVAR ARQUIVO  POUPADE1                                    *
      *----------------------------------------------------------------*
       1100-GRAVAR-POUPADE1            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTRU-ENTRADA    OF POUPADES
                                  TO GFCTRU-ENTRADA OF POUPADE1.

           MOVE CHEQ-CPF-NUM      TO GFCTRU-CGC-CPF OF POUPADE1.
           MOVE CHEQ-CPF-FIL      TO GFCTRU-FILIAL  OF POUPADE1.
           MOVE CHEQ-CPF-CON      TO GFCTRU-CONTR   OF POUPADE1.

           WRITE GFCTRU-ENTRADA   OF POUPADE1.


           MOVE WRK-GRAVACAO      TO WRK-OPERACAO.

           PERFORM 0600-TESTAR-FS-POUPADE1.

           ADD 1                  TO ACU-GRAVADOS-POUPADE1.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    GRAVAR ARQUIVO  POUPSCAD                                    *
      *----------------------------------------------------------------*
       1200-GRAVAR-POUPSCAD            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTRU-ENTRADA    OF POUPADES
                                  TO GFCTRU-ENTRADA OF POUPSCAD.

           WRITE GFCTRU-ENTRADA   OF POUPSCAD.


           MOVE WRK-GRAVACAO      TO WRK-OPERACAO.

           PERFORM 0500-TESTAR-FS-POUPSCAD.

           ADD 1                  TO ACU-GRAVADOS-POUPSCAD.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA FINALIZACAO DO PROGRAMA                         *
      *----------------------------------------------------------------*
       1300-PROCEDIMENTOS-FINAIS       SECTION.
      *----------------------------------------------------------------*

           PERFORM 1400-EMITIR-DISPLAY.

           CLOSE POUPADES
                 CADPREDU

                 POUPSCAD
                 POUPADE1.

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

           DISPLAY '******************** GFCT2118 ********************'.
           DISPLAY '*                                                *'.

           MOVE ACU-LIDOS-POUPADES     TO WRK-MASCARA.
           DISPLAY '*  LIDOS EM POUPADES......: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-LIDOS-CADPREDU     TO WRK-MASCARA.
           DISPLAY '*  LIDOS EM CADPREDU......: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-GRAVADOS-POUPADE1  TO WRK-MASCARA.
           DISPLAY '*  GRAVADOS NO POUPADE1...: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-GRAVADOS-POUPSCAD  TO WRK-MASCARA.
           DISPLAY '*  GRAVADOS NO POUPSCAD...: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-DESPREZADOS        TO WRK-MASCARA.
           DISPLAY '*  REGISTROS DO CADPREDU COM CONTAS MENORES      *'.
           DISPLAY '*  QUE 1.000.000 - CONTA CORRENTE.               *'.
           DISPLAY '*  DESPREZADOS CADPREDU...: '
                                                WRK-MASCARA  '       *'.

           DISPLAY '*                                                *'.
           DISPLAY '******************** GFCT2118 ********************'.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAMENTO DE ERRO                              *
      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT2118'             TO ERR-PGM.
           MOVE 'APL'                  TO ERR-TIPO-ACESSO.

           CALL 'BRAD7100'             USING WRK-BATCH
                                             ERRO-AREA.

           GOBACK.


      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *---------------------------------------------------------------

