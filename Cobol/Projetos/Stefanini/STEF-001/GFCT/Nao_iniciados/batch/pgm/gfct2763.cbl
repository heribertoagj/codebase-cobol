      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. GFCT2763.
       AUTHOR.     VINICIUS CRISTIAN MADUREIRA.
      *================================================================*
      *      S O N D A  P R O C W O R K  -  C O N S U L T O R I A      *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT2763                                    *
      *    PROGRAMADOR.:   VINICIUS CRISTIAN MADUREIRA - SONDPROC/GP.50*
      *    ANALISTA....:   VINICIUS CRISTIAN MADUREIRA - SONDPROC/GP.50*
      *    DATA........:   21/07/2009                                  *
      *                                                                *
      *    OBJETIVO    :   BALANCE LINE ENTRE PROCESSO DE EXTRATO COM  *
      *                    AGRUPAMENTO CLIENTE.                        *
      *                                                                *
      *    ARQUIVOS:                                                   *
      *      DDNAME                                    INCLUDE/BOOK    *
      *      AGRUPCLI                                    GFCTWAIC      *
      *      EXMAGRP1                                    GFCTWAET      *
      *      EXMAGRP2                                    GFCTWAET      *
      *      EXMAUTO2                                    GFCTWAET      *
      *      EXMCANC2                                    GFCTWAET      *
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

           SELECT AGRUPCLI ASSIGN      TO UT-S-AGRUPCLI
                      FILE STATUS      IS WRK-FS-AGRUPCLI.

           SELECT EXMAGRP1 ASSIGN      TO UT-S-EXMAGRP1
                      FILE STATUS      IS WRK-FS-EXMAGRP1.

           SELECT EXMAGRP2 ASSIGN      TO UT-S-EXMAGRP2
                      FILE STATUS      IS WRK-FS-EXMAGRP2.

           SELECT EXMAUTO2 ASSIGN      TO UT-S-EXMAUTO2
                      FILE STATUS      IS WRK-FS-EXMAUTO2.

           SELECT EXMCANC2 ASSIGN      TO UT-S-EXMCANC2
                      FILE STATUS      IS WRK-FS-EXMCANC2.


      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:                                                      *
      *            ORG. SEQUENTIAL     -  LRECL = 039                  *
      *----------------------------------------------------------------*

       FD  AGRUPCLI
           RECORDING MODE IS F
           LABEL RECORDS IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'GFCTWAIC'.

      *----------------------------------------------------------------*
      *    INPUT:                                                      *
      *            ORG. SEQUENTIAL     -  LRECL = 080                  *
      *----------------------------------------------------------------*

       FD  EXMAGRP1
           RECORDING MODE IS F
           LABEL RECORDS IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'GFCTWAET'.

      *----------------------------------------------------------------*
      *    OUTPUT:                                                     *
      *            ORG. SEQUENTIAL     -  LRECL = 080                  *
      *----------------------------------------------------------------*

       FD  EXMAGRP2
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'GFCTWAET'.

      *----------------------------------------------------------------*
      *    OUTPUT:                                                     *
      *            ORG. SEQUENTIAL     -  LRECL = 080                  *
      *----------------------------------------------------------------*

       FD  EXMAUTO2
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'GFCTWAET'.

      *----------------------------------------------------------------*
      *    OUTPUT:                                                     *
      *            ORG. SEQUENTIAL     -  LRECL = 080                  *
      *----------------------------------------------------------------*

       FD  EXMCANC2
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'GFCTWAET'.

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  INICIO DA WORKING GFCT2763  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*        ACUMULADORES          *'.
      *----------------------------------------------------------------*

       01  ACU-ACUMULADORES.
           05 ACU-LIDOS-AGRUPCLI       PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-LIDOS-EXMAGRP1       PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-GRAVADOS-EXMAGRP2    PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-GRAVADOS-EXMAUTO2    PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-GRAVADOS-EXMCANC2    PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-DESPREZADOS          PIC 9(11) COMP-3    VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    VARIAVEIS AUXILIARES      *'.
      *----------------------------------------------------------------*

       01  WRK-AUXILIARES.
           05 WRK-BATCH                PIC X(08)            VALUE
              'BATCH'.
           05 WRK-MASCARA              PIC ZZ.ZZZ.ZZZ.Z99   VALUE ZEROS.

ST25X6* Adequação ao padrão ST: CPF/CNPJ e Filial como alfanuméricos.
ST25X6* Comentando definições originais (numéricas):
ST25X6* 01  WRK-CHV-AGRUPCLI.
ST25X6*     03 WRK-CNPJCPF-AGRUPCLI     PIC  9(009)          VALUE ZEROS.
ST25X6*     03 WRK-CFLIAL-AGRUPCLI      PIC  9(004)          VALUE ZEROS.
ST25X6*     03 WRK-CCTRL-AGRUPCLI       PIC  9(002)          VALUE ZEROS.
       01  WRK-CHV-AGRUPCLI.
           03 WRK-CNPJCPF-AGRUPCLI     PIC  X(009)          VALUE SPACES.
           03 WRK-CFLIAL-AGRUPCLI      PIC  X(004)          VALUE SPACES.
           03 WRK-CCTRL-AGRUPCLI       PIC  9(002)          VALUE ZEROS.

ST25X6* Comentando definições originais (numéricas):
ST25X6* 01  WRK-CHV-EXMAGRP1.
ST25X6*     03 WRK-CNPJCPF-EXMAGRP1     PIC  9(009)          VALUE ZEROS.
ST25X6*     03 WRK-CFLIAL-EXMAGRP1      PIC  9(004)          VALUE ZEROS.
ST25X6*     03 WRK-CCTRL-EXMAGRP1       PIC  9(002)          VALUE ZEROS.
       01  WRK-CHV-EXMAGRP1.
           03 WRK-CNPJCPF-EXMAGRP1     PIC  X(009)          VALUE SPACES.
           03 WRK-CFLIAL-EXMAGRP1      PIC  X(004)          VALUE SPACES.
           03 WRK-CCTRL-EXMAGRP1       PIC  9(002)          VALUE ZEROS.

       01  WRK-S9-9                    PIC S9(009)          VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-S9-9.
           05 WRK-9-9                  PIC  9(009).

       01  WRK-S9-4                    PIC S9(004)          VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-S9-4.
           05 WRK-9-4                  PIC  9(004).

       01  WRK-S9-2                    PIC S9(002)          VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-S9-2.
           05 WRK-9-2                  PIC  9(002).

       01  WRK-S9-5                    PIC S9(005)          VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-S9-5.
           05 FILLER                   PIC  9(001).
           05 WRK-9-4-RED              PIC  9(004).

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    AREA PARA FILE STATUS     *'.
      *----------------------------------------------------------------*

       01  WRK-FS-AGRUPCLI             PIC X(02)           VALUE SPACES.
       01  WRK-FS-EXMAGRP1             PIC X(02)           VALUE SPACES.
       01  WRK-FS-EXMAGRP2             PIC X(02)           VALUE SPACES.
       01  WRK-FS-EXMAUTO2             PIC X(02)           VALUE SPACES.
       01  WRK-FS-EXMCANC2             PIC X(02)           VALUE SPACES.
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
           '*   FIM  DA WORKING GFCT2763   *'.
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

           PERFORM 0800-VERIFICAR-VAZIO.

           PERFORM 1100-PROCESSAR      UNTIL
                    WRK-CHV-EXMAGRP1   EQUAL HIGH-VALUES.

           PERFORM 1500-PROCEDIMENTOS-FINAIS.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA INICIALIZAR O PROGRAMA                          *
      *----------------------------------------------------------------*
       0100-INICIALIZAR-PROGRAMA       SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT AGRUPCLI
                      EXMAGRP1
               OUTPUT EXMAGRP2
                      EXMAUTO2
                      EXMCANC2.

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

           PERFORM 0300-TESTAR-FS-AGRUPCLI.

           PERFORM 0400-TESTAR-FS-EXMAGRP1.

           PERFORM 0500-TESTAR-FS-EXMAGRP2.

           PERFORM 0600-TESTAR-FS-EXMAUTO2.

           PERFORM 0700-TESTAR-FS-EXMCANC2.

      *----------------------------------------------------------------*
       0200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO AGRUPCLI                      *
      *----------------------------------------------------------------*
       0300-TESTAR-FS-AGRUPCLI         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-AGRUPCLI         NOT EQUAL '00'
               MOVE 'AGRUPCLI'         TO WRK-NOME-ARQ
               MOVE WRK-FS-AGRUPCLI    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO EXMAGRP1                      *
      *----------------------------------------------------------------*
       0400-TESTAR-FS-EXMAGRP1         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-EXMAGRP1         NOT EQUAL '00'
               MOVE 'EXMAGRP1'         TO WRK-NOME-ARQ
               MOVE WRK-FS-EXMAGRP1    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO EXMAGRP2                      *
      *----------------------------------------------------------------*
       0500-TESTAR-FS-EXMAGRP2         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-EXMAGRP2         NOT EQUAL '00'
               MOVE 'EXMAGRP2'         TO WRK-NOME-ARQ
               MOVE WRK-FS-EXMAGRP2    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO EXMAUTO2                      *
      *----------------------------------------------------------------*
       0600-TESTAR-FS-EXMAUTO2         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-EXMAUTO2         NOT EQUAL '00'
               MOVE 'EXMAUTO2'         TO WRK-NOME-ARQ
               MOVE WRK-FS-EXMAUTO2    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO EXMCANC2                      *
      *----------------------------------------------------------------*
       0700-TESTAR-FS-EXMCANC2         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-EXMCANC2         NOT EQUAL '00'
               MOVE 'EXMCANC2'         TO WRK-NOME-ARQ
               MOVE WRK-FS-EXMCANC2    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA P/ VERIFICAR SE OS ARQUIVOS DE ENTRADA ESTAO VAZIOS  *
      *----------------------------------------------------------------*
       0800-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 0900-LER-AGRUPCLI.

           IF  WRK-CHV-AGRUPCLI        EQUAL HIGH-VALUES
               DISPLAY '*********** GFCT2763 ***********'
               DISPLAY '*                              *'
               DISPLAY '*    ARQUIVO  AGRUPCLI VAZIO   *'
               DISPLAY '*    PROCESSAMENTO ENCERRADO   *'
               DISPLAY '*                              *'
               DISPLAY '*********** GFCT2763 ***********'
           END-IF.

           PERFORM 1000-LER-EXMAGRP1.

           IF  WRK-CHV-EXMAGRP1            EQUAL HIGH-VALUES
               DISPLAY '*********** GFCT2763 ***********'
               DISPLAY '*                              *'
               DISPLAY '*    ARQUIVO EXMAGRP1 VAZIO    *'
               DISPLAY '*    PROCESSAMENTO ENCERRADO   *'
               DISPLAY '*                              *'
               DISPLAY '*********** GFCT2763 ***********'
           END-IF.

      *----------------------------------------------------------------*
       0800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LEITURA DO ARQUIVO AGRUPCLI                                 *
      *----------------------------------------------------------------*
       0900-LER-AGRUPCLI               SECTION.
      *----------------------------------------------------------------*

           READ AGRUPCLI.

           IF  WRK-FS-AGRUPCLI         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHV-AGRUPCLI
               GO                      TO 0900-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 0300-TESTAR-FS-AGRUPCLI.

           MOVE WAIC-CCNPJ-CPF         TO WRK-S9-9.
           MOVE WRK-9-9                TO WRK-CNPJCPF-AGRUPCLI.
           MOVE WAIC-CFLIAL-CGC        TO WRK-S9-5.
           MOVE WRK-9-4-RED            TO WRK-CFLIAL-AGRUPCLI.
           MOVE WAIC-CCTRL-CPF-CGC     TO WRK-S9-2.
           MOVE WRK-9-2                TO WRK-CCTRL-AGRUPCLI.

           ADD 1                       TO ACU-LIDOS-AGRUPCLI.

      *----------------------------------------------------------------*
       0900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LEITURA DO ARQUIVO EXMAGRP1                                 *
      *----------------------------------------------------------------*
       1000-LER-EXMAGRP1               SECTION.
      *----------------------------------------------------------------*

           READ EXMAGRP1.

           IF  WRK-FS-EXMAGRP1         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHV-EXMAGRP1
               GO                      TO 1000-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 0400-TESTAR-FS-EXMAGRP1.

           MOVE WAET-CCGC-CPF      OF EXMAGRP1 TO WRK-S9-9.
           MOVE WRK-9-9                        TO WRK-CNPJCPF-EXMAGRP1.
           MOVE WAET-CFLIAL-CGC    OF EXMAGRP1 TO WRK-S9-4.
           MOVE WRK-9-4                        TO WRK-CFLIAL-EXMAGRP1.
           MOVE WAET-CCTRL-CPF-CGC OF EXMAGRP1 TO WRK-S9-2.
           MOVE WRK-9-2                        TO WRK-CCTRL-EXMAGRP1.

           ADD 1                       TO ACU-LIDOS-EXMAGRP1.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL DO PROGRAMA                         *
      *----------------------------------------------------------------*
       1100-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           IF  WRK-CHV-EXMAGRP1      EQUAL WRK-CHV-AGRUPCLI
               ADD 1         TO ACU-DESPREZADOS

               IF WAIC-CINDCD-AUTRZ-EMIS EQUAL 1
                   PERFORM 1300-GRAVAR-EXMAUTO2
               ELSE
                   PERFORM 1400-GRAVAR-EXMCANC2
               END-IF

               PERFORM 1000-LER-EXMAGRP1
           ELSE
               IF  WRK-CHV-EXMAGRP1  LESS WRK-CHV-AGRUPCLI
                   PERFORM 1200-GRAVAR-EXMAGRP2
                   PERFORM 1000-LER-EXMAGRP1
               ELSE
                   PERFORM 0900-LER-AGRUPCLI
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    GRAVAR ARQUIVO  EXMAGRP1                                    *
      *----------------------------------------------------------------*
       1200-GRAVAR-EXMAGRP2            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTWAET-REG OF EXMAGRP1 TO GFCTWAET-REG OF EXMAGRP2.

           WRITE GFCTWAET-REG     OF EXMAGRP2.

           MOVE WRK-GRAVACAO      TO WRK-OPERACAO.

           PERFORM 0500-TESTAR-FS-EXMAGRP2.

           ADD 1                  TO ACU-GRAVADOS-EXMAGRP2.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    GRAVAR ARQUIVO  EXMAUTO2                                    *
      *----------------------------------------------------------------*
       1300-GRAVAR-EXMAUTO2            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTWAET-REG OF EXMAGRP1 TO GFCTWAET-REG   OF EXMAUTO2.
           MOVE 'F'                      TO WAET-TIPO-EMIS OF EXMAUTO2.

           WRITE GFCTWAET-REG     OF EXMAUTO2.

           MOVE WRK-GRAVACAO      TO WRK-OPERACAO.

           PERFORM 0600-TESTAR-FS-EXMAUTO2.

           ADD 1                  TO ACU-GRAVADOS-EXMAUTO2.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    GRAVAR ARQUIVO  EXMCANC2                                    *
      *----------------------------------------------------------------*
       1400-GRAVAR-EXMCANC2            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTWAET-REG OF EXMAGRP1 TO GFCTWAET-REG OF EXMCANC2.

           WRITE GFCTWAET-REG     OF EXMCANC2.

           MOVE WRK-GRAVACAO      TO WRK-OPERACAO.

           PERFORM 0700-TESTAR-FS-EXMCANC2.

           ADD 1                  TO ACU-GRAVADOS-EXMCANC2.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA FINALIZACAO DO PROGRAMA                         *
      *----------------------------------------------------------------*
       1500-PROCEDIMENTOS-FINAIS       SECTION.
      *----------------------------------------------------------------*

           PERFORM 1600-EMITIR-DISPLAY.

           CLOSE AGRUPCLI
                 EXMAGRP1
                 EXMAGRP2
                 EXMAUTO2
                 EXMCANC2.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 0200-TESTAR-FILE-STATUS.

           STOP RUN.

      *----------------------------------------------------------------*
       1500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA EMISSAO DOS DISPLAYS FINAIS DO PROGRAMA         *
      *----------------------------------------------------------------*
       1600-EMITIR-DISPLAY             SECTION.
      *----------------------------------------------------------------*

           DISPLAY '******************** GFCT2763 ********************'.
           DISPLAY '*                                                *'.
           MOVE ACU-LIDOS-AGRUPCLI     TO WRK-MASCARA.
           DISPLAY '*  LIDOS EM AGRUPCLI......: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-LIDOS-EXMAGRP1     TO WRK-MASCARA.
           DISPLAY '*  LIDOS EM EXMAGRP1......: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-GRAVADOS-EXMAGRP2  TO WRK-MASCARA.
           DISPLAY '*  GRAVADOS EXMAGRP2......: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-GRAVADOS-EXMAUTO2  TO WRK-MASCARA.
           DISPLAY '*  GRAVADOS EM AUTORIZADOS: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-GRAVADOS-EXMCANC2  TO WRK-MASCARA.
           DISPLAY '*  GRAVADOS EM CANCELADOS.: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-DESPREZADOS        TO WRK-MASCARA.
           DISPLAY '*  DESPREZADOS............: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           DISPLAY '******************** GFCT2763 ********************'.

      *----------------------------------------------------------------*
       1600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAMENTO DE ERRO                              *
      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT2763'             TO ERR-PGM.
           MOVE 'APL'                  TO ERR-TIPO-ACESSO.

           CALL 'BRAD7100'             USING WRK-BATCH
                                             ERRO-AREA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *---------------------------------------------------------------

