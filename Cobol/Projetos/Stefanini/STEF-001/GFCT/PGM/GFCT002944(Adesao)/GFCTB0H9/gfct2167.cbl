      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. GFCT2167.
       AUTHOR.     VINICIUS CRISTIAN MADUREIRA.
      *================================================================*
      *      S O N D A  P R O C W O R K  -  C O N S U L T O R I A      *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT2167                                    *
      *    PROGRAMADOR.:   VINICIUS CRISTIAN MADUREIRA - SONDPROC/GP.50*
      *    ANALISTA....:   VINICIUS CRISTIAN MADUREIRA - SONDPROC/GP.50*
      *    DATA........:   20/04/2009                                  *
      *                                                                *
      *    OBJETIVO    :   INCLUIR INFORMACAO DE PESSOA FISICA E JURI- *
      *                    DICA.                                       *
      *                                                                *
      *    ARQUIVOS:                                                   *
      *      DDNAME                                    INCLUDE/BOOK    *
      *      ADESAGPE                                    GFCTWAGM      *
      *      CLIEADES                                    I#GFCTRU      *
      *      ADESAGPS                                    GFCTWAGM      *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      BRAD7100  -  MODULO DE TRATAMENTO DE ERRO.                *
      *                                                                *
BI0810*----------------------------------------------------------------*
BI0810*    ULTIMA ALTERACAO EM AGO/2010 - BIRA                         *
BI0810*    - SUBSTITUIR O USO DA INC I#GFCTRZ PARA GFCTWAGM.           *
BI0810*                                                                *
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

           SELECT ADESAGPE ASSIGN      TO UT-S-ADESAGPE
                      FILE STATUS      IS WRK-FS-ADESAGPE.

           SELECT CLIEADES ASSIGN      TO UT-S-CLIEADES
                      FILE STATUS      IS WRK-FS-CLIEADES.

           SELECT ADESAGPS ASSIGN      TO UT-S-ADESAGPS
                      FILE STATUS      IS WRK-FS-ADESAGPS.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:                                                      *
      *            ORG. SEQUENTIAL     -  LRECL = 050                  *
      *----------------------------------------------------------------*

       FD  ADESAGPE
           RECORDING MODE IS F
           LABEL RECORDS IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

BI0810 COPY 'GFCTWAGM'.

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
      *            ORG. SEQUENTIAL     -  LRECL = 050                  *
      *----------------------------------------------------------------*

       FD  ADESAGPS
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'GFCTWAGM'.

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  INICIO DA WORKING GFCT2167  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*        ACUMULADORES          *'.
      *----------------------------------------------------------------*

       01  ACU-ACUMULADORES.
           05 ACU-LIDOS-ADESAGPE       PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-LIDOS-CLIEADES       PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-GRAVADOS-ADESAGPS    PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-DESPREZADOS          PIC 9(11) COMP-3    VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    VARIAVEIS AUXILIARES      *'.
      *----------------------------------------------------------------*

       01  WRK-AUXILIARES.
           05 WRK-BATCH                PIC X(08)            VALUE
              'BATCH'.
           05 WRK-MASCARA              PIC ZZ.ZZZ.ZZZ.Z99   VALUE ZEROS.

       01  WRK-CHV-ADESAGPE.
           03 WRK-AGENCIA-ADESAGPE     PIC 9(005)           VALUE ZEROS.
           03 WRK-CONTA-ADESAGPE       PIC 9(007)           VALUE ZEROS.

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

       01  WRK-FS-ADESAGPE             PIC X(02)           VALUE SPACES.
       01  WRK-FS-CLIEADES             PIC X(02)           VALUE SPACES.
       01  WRK-FS-ADESAGPS             PIC X(02)           VALUE SPACES.
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
           '*   FIM  DA WORKING GFCT2167   *'.
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
                    WRK-CHV-ADESAGPE   EQUAL HIGH-VALUES.

           PERFORM 1300-PROCEDIMENTOS-FINAIS.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA INICIALIZAR O PROGRAMA                          *
      *----------------------------------------------------------------*
       0100-INICIALIZAR-PROGRAMA       SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT ADESAGPE
                      CLIEADES
               OUTPUT ADESAGPS.

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

           PERFORM 0300-TESTAR-FS-ADESAGPE.

           PERFORM 0400-TESTAR-FS-CLIEADES.

           PERFORM 0600-TESTAR-FS-ADESAGPS.

      *----------------------------------------------------------------*
       0200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO ADESAGPE                      *
      *----------------------------------------------------------------*
       0300-TESTAR-FS-ADESAGPE         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ADESAGPE         NOT EQUAL '00'
               MOVE 'ADESAGPE'         TO WRK-NOME-ARQ
               MOVE WRK-FS-ADESAGPE    TO WRK-FS
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
      *    TESTAR FILE-STATUS DO ARQUIVO ADESAGPS                      *
      *----------------------------------------------------------------*
       0600-TESTAR-FS-ADESAGPS         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ADESAGPS         NOT EQUAL '00'
               MOVE 'ADESAGPS'         TO WRK-NOME-ARQ
               MOVE WRK-FS-ADESAGPS    TO WRK-FS
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

           PERFORM 0800-LER-ADESAGPE.

           IF  WRK-CHV-ADESAGPE        EQUAL HIGH-VALUES
               DISPLAY '*********** GFCT2167 ***********'
               DISPLAY '*                              *'
               DISPLAY '*    ARQUIVO  ADESAGPE VAZIO   *'
               DISPLAY '*    PROCESSAMENTO ENCERRADO   *'
               DISPLAY '*                              *'
               DISPLAY '*********** GFCT2167 ***********'
           END-IF.

           PERFORM 0900-LER-CLIEADES.

           IF  WRK-CHV-CLIEADES            EQUAL HIGH-VALUES
               DISPLAY '*********** GFCT2167 ***********'
               DISPLAY '*                              *'
               DISPLAY '*    ARQUIVO CLIEADES VAZIO    *'
               DISPLAY '*                              *'
               DISPLAY '*********** GFCT2167 ***********'
           END-IF.

      *----------------------------------------------------------------*
       0700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LEITURA DO ARQUIVO ADESAGPE                                 *
      *----------------------------------------------------------------*
       0800-LER-ADESAGPE               SECTION.
      *----------------------------------------------------------------*

           READ ADESAGPE.

           IF  WRK-FS-ADESAGPE         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHV-ADESAGPE
               GO                      TO 0800-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 0300-TESTAR-FS-ADESAGPE.

BI0810     MOVE WAGM-AGENCIA OF ADESAGPE
BI0810                          TO WRK-AGENCIA-ADESAGPE.
BI0810     MOVE WAGM-CONTA   OF ADESAGPE
BI0810                          TO WRK-CONTA-ADESAGPE.

           ADD 1                       TO ACU-LIDOS-ADESAGPE.

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

           IF  WRK-CHV-ADESAGPE      EQUAL WRK-CHV-CLIEADES
               PERFORM 1200-GRAVAR-ADESAGPS
               PERFORM 0800-LER-ADESAGPE
           ELSE
               IF  WRK-CHV-ADESAGPE  LESS WRK-CHV-CLIEADES
                   ADD 1         TO ACU-DESPREZADOS
                   PERFORM 0800-LER-ADESAGPE
               ELSE
                   PERFORM 0900-LER-CLIEADES
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    GRAVAR ARQUIVO  ADESAGPS                                    *
      *----------------------------------------------------------------*
       1200-GRAVAR-ADESAGPS            SECTION.
      *----------------------------------------------------------------*

BI0810     MOVE WAGM-ADESOES OF ADESAGPE
BI0810                       TO WAGM-ADESOES OF ADESAGPS.

           IF GFCTRU-FILIAL EQUAL ZEROS
BI0810         MOVE 'F' TO WAGM-PESSOA-CLIE OF ADESAGPS
           ELSE
BI0810         MOVE 'J' TO WAGM-PESSOA-CLIE OF ADESAGPS
           END-IF.

           WRITE WAGM-ADESOES  OF ADESAGPS.

           MOVE WRK-GRAVACAO   TO WRK-OPERACAO.

           PERFORM 0600-TESTAR-FS-ADESAGPS.

           ADD 1               TO ACU-GRAVADOS-ADESAGPS.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA FINALIZACAO DO PROGRAMA                         *
      *----------------------------------------------------------------*
       1300-PROCEDIMENTOS-FINAIS       SECTION.
      *----------------------------------------------------------------*

           PERFORM 1400-EMITIR-DISPLAY.

           CLOSE ADESAGPE
                 CLIEADES
                 ADESAGPS.

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

           DISPLAY '******************** GFCT2167 ********************'.
           DISPLAY '*                                                *'.
           MOVE ACU-LIDOS-ADESAGPE     TO WRK-MASCARA.
           DISPLAY '*  LIDOS EM ADESAGPE......: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-LIDOS-CLIEADES     TO WRK-MASCARA.
           DISPLAY '*  LIDOS EM CLIEADES......: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-GRAVADOS-ADESAGPS  TO WRK-MASCARA.
           DISPLAY '*  GRAVADOS NO ADESAGPS...: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-DESPREZADOS        TO WRK-MASCARA.
           DISPLAY '*  NAO ENCONTRADOS NO           *'.
           DISPLAY '*  CLIEADES...............: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           DISPLAY '******************** GFCT2167 ********************'.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAMENTO DE ERRO                              *
      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT2167'             TO ERR-PGM.
           MOVE 'APL'                  TO ERR-TIPO-ACESSO.

           CALL 'BRAD7100'             USING WRK-BATCH
                                             ERRO-AREA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *---------------------------------------------------------------


