      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. GFCT1333.
       AUTHOR.     LAYS FELIX MOLEIRO.
      *================================================================*
      *      S O N D A  P R O C W O R K  -  C O N S U L T O R I A      *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT1333                                    *
      *    PROGRAMADOR.:   LAYS FELIX MOLEIRO          - SONDPROC/GP.50*
      *    ANALISTA....:   LAYS FELIX MOLEIRO          - SONDPROC/GP.50*
      *    DATA........:   16/11/2009                                  *
      *                                                                *
      *    OBJETIVO    :   INCLUIR INFORMACAO DE PESSOA FISICA E JURI- *
      *                    DICA.                                       *
      *                                                                *
      *    ARQUIVOS:                                                   *
      *      DDNAME                                    INCLUDE/BOOK    *
      *      ADESCORR                                    I#GFCTSX      *
      *      CLIEADES                                    I#GFCTRU      *
      *      ADESAOPJ                                    GFCTWA        *
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

           SELECT ADESCORR ASSIGN      TO UT-S-ADESCORR
                      FILE STATUS      IS WRK-FS-ADESCORR.

           SELECT CLIEADES ASSIGN      TO UT-S-CLIEADES
                      FILE STATUS      IS WRK-FS-CLIEADES.

           SELECT ADESAOPJ ASSIGN      TO UT-S-ADESAOPJ
                      FILE STATUS      IS WRK-FS-ADESAOPJ.

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

       FD  ADESCORR
           RECORDING MODE IS F
           LABEL RECORDS IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01 FD-ADESCORR                     PIC X(030).

      *----------------------------------------------------------------*
      *    INPUT:                                                      *
      *            ORG. SEQUENTIAL     -  LRECL = 040                  *
      *----------------------------------------------------------------*

       FD  CLIEADES
           RECORDING MODE IS F
           LABEL RECORDS IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTRU'.

      *----------------------------------------------------------------*
      *    OUTPUT:                                                     *
      *            ORG. SEQUENTIAL     -  LRECL = 030                  *
      *----------------------------------------------------------------*

       FD  ADESAOPJ
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01 FD-ADESAOPJ                     PIC X(030).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  INICIO DA WORKING GFCT1333  *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTSX'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*        ACUMULADORES          *'.
      *----------------------------------------------------------------*

       01  ACU-ACUMULADORES.
           05 ACU-LIDOS-ADESCORR       PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-LIDOS-CLIEADES       PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-GRAVADOS-ADESAOPJ    PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-DESPREZADOS          PIC 9(11) COMP-3    VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    VARIAVEIS AUXILIARES      *'.
      *----------------------------------------------------------------*

       01  WRK-AUXILIARES.
           05 WRK-BATCH                PIC X(08)            VALUE
              'BATCH'.
           05 WRK-MASCARA              PIC ZZ.ZZZ.ZZZ.Z99   VALUE ZEROS.

       01  WRK-CHV-ADESCORR.
           03 WRK-AGENCIA-ADESCORR     PIC 9(005)           VALUE ZEROS.
           03 WRK-CONTA-ADESCORR       PIC 9(007)           VALUE ZEROS.

       01  WRK-CHV-CLIEADES.
           03 WRK-AGENCIA-CLIEADES     PIC 9(005)           VALUE ZEROS.
           03 WRK-CONTA-CLIEADES       PIC 9(007)           VALUE ZEROS.

       01  WRK-S9-5                    PIC S9(005)          VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-S9-5.
           05 WRK-9-5                  PIC  9(005).

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    AREA PARA FILE STATUS     *'.
      *----------------------------------------------------------------*

       01  WRK-FS-ADESCORR             PIC X(02)           VALUE SPACES.
       01  WRK-FS-CLIEADES             PIC X(02)           VALUE SPACES.
       01  WRK-FS-ADESAOPJ             PIC X(02)           VALUE SPACES.
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
           '*   FIM  DA WORKING GFCT1333   *'.
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
                    WRK-CHV-ADESCORR   EQUAL HIGH-VALUES.

           PERFORM 1300-PROCEDIMENTOS-FINAIS.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA INICIALIZAR O PROGRAMA                          *
      *----------------------------------------------------------------*
       0100-INICIALIZAR-PROGRAMA       SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT ADESCORR
                      CLIEADES
               OUTPUT ADESAOPJ.

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

           PERFORM 0300-TESTAR-FS-ADESCORR.

           PERFORM 0400-TESTAR-FS-CLIEADES.

           PERFORM 0600-TESTAR-FS-ADESAOPJ.

      *----------------------------------------------------------------*
       0200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO ADESCORR                      *
      *----------------------------------------------------------------*
       0300-TESTAR-FS-ADESCORR         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ADESCORR         NOT EQUAL '00'
               MOVE 'ADESCORR'         TO WRK-NOME-ARQ
               MOVE WRK-FS-ADESCORR    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO CLIEADES                      *
      *----------------------------------------------------------------*
       0400-TESTAR-FS-CLIEADES         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-CLIEADES         NOT EQUAL '00'
               MOVE 'CLIEADES'         TO WRK-NOME-ARQ
               MOVE WRK-FS-CLIEADES    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO ADESAOPJ                      *
      *----------------------------------------------------------------*
       0600-TESTAR-FS-ADESAOPJ         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ADESAOPJ         NOT EQUAL '00'
               MOVE 'ADESAOPJ'         TO WRK-NOME-ARQ
               MOVE WRK-FS-ADESAOPJ    TO WRK-FS
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

           PERFORM 0800-LER-ADESCORR.

           IF  WRK-CHV-ADESCORR        EQUAL HIGH-VALUES
               DISPLAY '*********** GFCT1333 ***********'
               DISPLAY '*                              *'
               DISPLAY '*    ARQUIVO  ADESCORR VAZIO   *'
               DISPLAY '*    PROCESSAMENTO ENCERRADO   *'
               DISPLAY '*                              *'
               DISPLAY '*********** GFCT1333 ***********'
           END-IF.

           PERFORM 0900-LER-CLIEADES.

           IF  WRK-CHV-CLIEADES            EQUAL HIGH-VALUES
               DISPLAY '*********** GFCT1333 ***********'
               DISPLAY '*                              *'
               DISPLAY '*    ARQUIVO CLIEADES VAZIO    *'
               DISPLAY '*                              *'
               DISPLAY '*********** GFCT1333 ***********'
           END-IF.

      *----------------------------------------------------------------*
       0700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LEITURA DO ARQUIVO ADESCORR                                 *
      *----------------------------------------------------------------*
       0800-LER-ADESCORR               SECTION.
      *----------------------------------------------------------------*

           READ ADESCORR INTO REG-ADESCOMP.

           IF  WRK-FS-ADESCORR         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHV-ADESCORR
               GO                      TO 0800-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 0300-TESTAR-FS-ADESCORR.

           MOVE GFCTSX-AGENCIA   TO WRK-AGENCIA-ADESCORR.
           MOVE GFCTSX-CONTA     TO WRK-CONTA-ADESCORR.

           ADD 1                       TO ACU-LIDOS-ADESCORR.

      *----------------------------------------------------------------*
       0800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LEITURA DO ARQUIVO CLIEADES                                 *
      *----------------------------------------------------------------*
       0900-LER-CLIEADES               SECTION.
      *----------------------------------------------------------------*

           READ CLIEADES.

           IF  WRK-FS-CLIEADES         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHV-CLIEADES
               GO                      TO 0900-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 0400-TESTAR-FS-CLIEADES.

           MOVE GFCTRU-AGENCIA         TO WRK-S9-5.
           MOVE WRK-9-5                TO WRK-AGENCIA-CLIEADES.
           MOVE GFCTRU-CONTA           TO WRK-CONTA-CLIEADES.

           ADD 1                       TO ACU-LIDOS-CLIEADES.

      *----------------------------------------------------------------*
       0900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL DO PROGRAMA                         *
      *----------------------------------------------------------------*
       1000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           IF  GFCTSX-CONTA     NOT LESS 1000000
               PERFORM 1200-GRAVAR-ADESAOPJ
               PERFORM 0800-LER-ADESCORR
           ELSE
             IF  WRK-CHV-ADESCORR      EQUAL WRK-CHV-CLIEADES
                 PERFORM 1200-GRAVAR-ADESAOPJ
                 PERFORM 0800-LER-ADESCORR
             ELSE
                 IF  WRK-CHV-ADESCORR  LESS WRK-CHV-CLIEADES
                     ADD 1         TO ACU-DESPREZADOS
                     PERFORM 0800-LER-ADESCORR
                 ELSE
                   PERFORM 0900-LER-CLIEADES
                 END-IF
             END-IF
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    GRAVAR ARQUIVO  ADESAOPJ                                    *
      *----------------------------------------------------------------*
       1200-GRAVAR-ADESAOPJ            SECTION.
      *----------------------------------------------------------------*

           WRITE FD-ADESAOPJ  FROM REG-ADESCOMP.

           MOVE WRK-GRAVACAO   TO WRK-OPERACAO.

           PERFORM 0600-TESTAR-FS-ADESAOPJ.

           ADD 1               TO ACU-GRAVADOS-ADESAOPJ.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA FINALIZACAO DO PROGRAMA                         *
      *----------------------------------------------------------------*
       1300-PROCEDIMENTOS-FINAIS       SECTION.
      *----------------------------------------------------------------*

           PERFORM 1400-EMITIR-DISPLAY.

           CLOSE ADESCORR
                 CLIEADES
                 ADESAOPJ.

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

           DISPLAY '******************** GFCT1333 ********************'.
           DISPLAY '*                                                *'.
           MOVE ACU-LIDOS-ADESCORR     TO WRK-MASCARA.
           DISPLAY '*  LIDOS EM ADESCORR......: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-LIDOS-CLIEADES     TO WRK-MASCARA.
           DISPLAY '*  LIDOS EM CLIEADES......: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-GRAVADOS-ADESAOPJ  TO WRK-MASCARA.
           DISPLAY '*  GRAVADOS NO ADESAOPJ...: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-DESPREZADOS        TO WRK-MASCARA.
           DISPLAY '*  ADESAO CORRENTE NAO PJ                        *'.
           DISPLAY '*  ADESCORR...............: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           DISPLAY '******************** GFCT1333 ********************'.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAMENTO DE ERRO                              *
      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT1333'             TO ERR-PGM.
           MOVE 'APL'                  TO ERR-TIPO-ACESSO.

           CALL 'BRAD7100'             USING WRK-BATCH
                                             ERRO-AREA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *---------------------------------------------------------------



