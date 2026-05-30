      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT4708.
       AUTHOR.     HOMI.
      *================================================================*
      *      S O N D A  I T                                            *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT4708                                    *
      *    PROGRAMADOR.:   HOMI                                        *
      *    ANALISTA....:   PAGNOCCA - SONDA IT - GR. 50                *
      *    DATA........:   OUT/2015                                    *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   INVESTIMENTOS ULTIMOS 03 MESES              *
      *                    CONSISTENCIA E GERACAO DE IMAGEM DA TABELA  *
      *                    GFCTB0P0 - TPOSIC_SDO_APLIC + CNPJ/CPF      *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                          INCLUDE/BOOK    *
      *                     ENTRADA                      GFCTWF17      *
      *                     REJEITAD                     GFCTWF18      *
      *                     GFCTB0P0                     GFCTWF45      *
      *----------------------------------------------------------------*
      *    BOOKS.......:                                               *
      *    GFCTWF03 - AREA DE COMUNICACAO DO DE/PARA DE CESTAS         *
      *    GFCTWF04 - AREA DE COMUNICACAO DE/PARA DE CONTAS HSBC       *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    GFCT5800 - EFETUAR DE/PARA DE CESTAS HSBC PARA BRADESCO     *
      *    GFCT5801 - EFETUAR DE/PARA DE CONTAS HSBC/BRADESCO          *
      *    POOL1470 - VALIDAR DATA                                     *
      *    POOL7100 - TRATAMENTO DE ERROS.                             *
      *                                                                *
      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*

       FILE-CONTROL.

           SELECT ENTRADA   ASSIGN     TO UT-S-ENTRADA
           FILE STATUS                 IS WRK-FS-ENTRADA.

           SELECT REJEITAD ASSIGN      TO UT-S-REJEITAD
           FILE STATUS                 IS WRK-FS-REJEITAD.

           SELECT GFCTB0P0 ASSIGN      TO UT-S-GFCTB0P0
           FILE STATUS                 IS WRK-FS-GFCTB0P0.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:  ARQUIVO ENTRADA                                     *
      *            ORG. SEQUENCIAL     - LRECL = 200                   *
      *----------------------------------------------------------------*

       FD  ENTRADA
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-ENTRADA                  PIC X(200).

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO REGISTROS REJEITADOS.                       *
      *            ORG. SEQUENCIAL     - LRECL = 200                   *
      *----------------------------------------------------------------*

       FD  REJEITAD
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REJEITAD                 PIC X(200).

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO IMAGEM DA TABELA GFCTB0P0 + CNPJ/CPF        *
      *                                   + AGENCIA/CONTA/DE           *
      *            ORG. SEQUENCIAL     - LRECL = 116                   *
      *----------------------------------------------------------------*

       FD  GFCTB0P0
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-GFCTB0P0                 PIC X(116).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(033)         VALUE
           'INICIO DA WORKING STORAGE SECTION'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(022)         VALUE
           'AREA PARA ACUMULADORES'.
      *----------------------------------------------------------------*

       77  ACU-LIDOS-ENTRADA           PIC  9(015) COMP-3  VALUE ZEROS.
       77  ACU-GRAVADOS-REJEITAD       PIC  9(015) COMP-3  VALUE ZEROS.
       77  ACU-GRAVADOS-GFCTB0P0       PIC  9(015) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(030)         VALUE
           'AREA PARA TESTE DE FILE STATUS'.
      *----------------------------------------------------------------*

       77  WRK-FS-ENTRADA              PIC  X(002)         VALUE SPACES.
       77  WRK-FS-REJEITAD             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-GFCTB0P0             PIC  X(002)         VALUE SPACES.

       77  WRK-ABERTURA                PIC  X(013)         VALUE
           ' NA ABERTURA '.
       77  WRK-LEITURA                 PIC  X(013)         VALUE
           ' NA  LEITURA '.
       77  WRK-GRAVACAO                PIC  X(013)         VALUE
           ' NA GRAVACAO '.
       77  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(030)         VALUE
           'AREA PARA VARIAVEIS AUXILIARES'.
      *----------------------------------------------------------------*
       77  WRK-BATCH                   PIC  X(008)        VALUE 'BATCH'.
       77  WRK-MODULO                  PIC  X(008)        VALUE SPACES.
       77  WRK-MASCARA             PIC  ZZZZZZZZZZ.ZZZ.999 VALUE ZEROS.
       77  WRK-ERRO                    PIC  X(001)        VALUE SPACES.
       77  WRK-MSG                     PIC  X(030)        VALUE SPACES.
       77  WRK-NOME-ARQUIVO            PIC  X(030)        VALUE SPACES.
       77  WRK-DATA-VALIDA             PIC  X(001)        VALUE 'N'.
       77  WRK-ULT-TR-LIDO             PIC  9(002)        VALUE ZEROS.
       77  WRK-CURRENT-TIMESTAMP       PIC  X(026)        VALUE SPACES.
       01  WRK-F04-FIL-BRAD            PIC  9(005)        VALUE ZEROS.
       01  REDEFINES WRK-F04-FIL-BRAD.
           03                          PIC  X.
           03  WRK-F04-FIL-BRAD-4      PIC  9(004).

       01  WRK-DATA-GERACAO.
           03 WRK-DG-DD                PIC  9(002)        VALUE ZEROS.
           03                          PIC  X             VALUE '/'.
           03 WRK-DG-MM                PIC  9(002)        VALUE ZEROS.
           03                          PIC  X             VALUE '/'.
           03 WRK-DG-AAAA              PIC  9(004)        VALUE ZEROS.

       01  WRK-DATA-DMA                PIC  9(008)         VALUE ZEROS.
       01  REDEFINES WRK-DATA-DMA.
           03  WRK-DIA-DMA             PIC  9(002).
           03  WRK-MES-DMA             PIC  9(002).
           03  WRK-ANO-DMA             PIC  9(004).

       01  WRK-DATA-AMD                PIC  9(008)         VALUE ZEROS.
       01  REDEFINES WRK-DATA-AMD.
           03  WRK-ANO-AMD             PIC  9(004).
           03  WRK-MES-AMD             PIC  9(002).
           03  WRK-DIA-AMD             PIC  9(002).

       01  WRK-DATA-AM                 PIC  9(006)         VALUE ZEROS.
       01  REDEFINES WRK-DATA-AM.
           03  WRK-ANO-AM              PIC  9(004).
           03  WRK-MES-AM              PIC  9(002).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(016)        VALUE
           'AREA PARA CHAVES'.
      *----------------------------------------------------------------*

       01  WRK-CH-ANT.
           03 WRK-CH-ANT-AGCT.
              05 WRK-ANT-AGENCIA       PIC  9(005)        VALUE ZEROS.
              05 WRK-ANT-CONTA         PIC  9(013)        VALUE ZEROS.
           03 WRK-ANT-AM-APUR          PIC  9(006)        VALUE ZEROS.

       01  WRK-CH-ATU.
           03 WRK-CH-ATU-AGCT.
              05 WRK-ATU-AGENCIA       PIC  9(005)        VALUE ZEROS.
              05 WRK-ATU-CONTA         PIC  9(013)        VALUE ZEROS.
           03 WRK-ATU-AM-APUR          PIC  9(006)        VALUE ZEROS.

       01  WRK-CH-ANT-CESTA.
           05 WRK-ANT-CESTA            PIC  9(005)        VALUE ZEROS.

       01  WRK-CH-ATU-CESTA.
           05 WRK-ATU-CESTA            PIC  9(005)        VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(019)         VALUE
           ' AREA DE ENTRADA'.
      *----------------------------------------------------------------*

       COPY 'GFCTWF17'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(019)         VALUE
           ' AREA DE REJEITAD'.
      *----------------------------------------------------------------*

       COPY 'GFCTWF18'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(019)         VALUE
           ' AREA DE GFCTB0P0'.
      *----------------------------------------------------------------*

       COPY 'GFCTWF45'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(026)         VALUE
           'AREA DE ERRO - POOL7100'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-FILE-STATUS.
           05  FILLER                  PIC  X(008)         VALUE
               '** ERRO '.
           05  WRK-OPERACAO            PIC  X(013)         VALUE SPACES.
           05  FILLER                  PIC  X(012)         VALUE
               ' DO ARQUIVO '.
           05  WRK-NOME-ARQ            PIC  X(008)         VALUE SPACES.
           05  FILLER                  PIC  X(017)         VALUE
               ' - FILE-STATUS = '.
           05  WRK-FILE-STATUS         PIC  X(002)         VALUE SPACES.
           05  FILLER                  PIC  X(003)         VALUE ' **'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(019)         VALUE
           ' AREA PARA POOL7100'.
      *----------------------------------------------------------------*

       COPY 'POL7100C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA DE COMUNICACAO DO - GFCT5800'.
      *---------------------------------------------------------------*

       COPY 'GFCTWF03'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA DE COMUNICACAO DO - GFCT5801'.
      *---------------------------------------------------------------*

       COPY 'GFCTWF04'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA PARA TABELA DB2'.
      *---------------------------------------------------------------*

           EXEC SQL INCLUDE SQLCA    END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(030)         VALUE
           'FIM DA WORKING STORAGE SECTION'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*
       01  LNK-PARM.
           03  LNK-PARM-TAMANHO        PIC S9(004) COMP.
           03  LNK-PESQ-CLIE           PIC  X(01).

      *================================================================*
       PROCEDURE DIVISION              USING LNK-PARM.
      *================================================================*

      ******************************************************************
      *    CHAMA AS ROTINAS PRINCIPAIS DO PROCESSAMENTO.               *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL1050'.

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-VERIFICAR-VAZIO.

           PERFORM 3000-PROCESSAR      UNTIL
               WRK-FS-ENTRADA          EQUAL '10'.

           PERFORM 9000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCEDIMENTOS INICIAIS DO PROGRAMA                          *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           IF LNK-PARM-TAMANHO         EQUAL ZEROS
              MOVE 'APL'               TO ERR-TIPO-ACESSO
              MOVE 'FALTOU INFORMAR PARAMETRO' TO ERR-TEXTO
              PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF LNK-PESQ-CLIE  NOT EQUAL 'S' AND 'N'
              MOVE 'APL'               TO ERR-TIPO-ACESSO
              MOVE 'PARAMETRO PESQ.CLIE.INVALIDO' TO ERR-TEXTO
              PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           OPEN INPUT  ENTRADA
                OUTPUT REJEITAD
                       GFCTB0P0.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

           EXEC SQL SET :WRK-CURRENT-TIMESTAMP = CURRENT TIMESTAMP
           END-EXEC.


      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA QUE EFETUA TESTE DE FILE-STATUS.                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-ENTRADA.

           PERFORM 1120-TESTAR-FS-REJEITAD.

           PERFORM 1130-TESTAR-FS-GFCTB0P0.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE TESTE PARA ARQUIVO ENTRADA.                       *
      ******************************************************************
      *----------------------------------------------------------------*
       1110-TESTAR-FS-ENTRADA          SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ENTRADA          NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'ENTRADA'          TO WRK-NOME-ARQ
               MOVE WRK-FS-ENTRADA     TO WRK-FILE-STATUS
               MOVE WRK-ERRO-FILE-STATUS  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE TESTE PARA ARQUIVO REJEITAD                       *
      ******************************************************************
      *----------------------------------------------------------------*
       1120-TESTAR-FS-REJEITAD          SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-REJEITAD         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'REJEITAD'         TO WRK-NOME-ARQ
               MOVE WRK-FS-REJEITAD    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-FILE-STATUS  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE TESTE PARA ARQUIVO GFCTB0P0                       *
      ******************************************************************
      *----------------------------------------------------------------*
       1130-TESTAR-FS-GFCTB0P0          SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-GFCTB0P0         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'GFCTB0P0'         TO WRK-NOME-ARQ
               MOVE WRK-FS-GFCTB0P0    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-FILE-STATUS  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICAR SE ARQUIVO ENTRADA ESTA VAZIO                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-LER-ENTRADA.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA QUE EFETUA LEITURA E TRATAMENTO DO ARQUIVO DE ENTRADA*
      ******************************************************************
      *----------------------------------------------------------------*
       2100-LER-ENTRADA                SECTION.
      *----------------------------------------------------------------*

           READ ENTRADA INTO F17-REGISTRO.

           IF  WRK-FS-ENTRADA          EQUAL '10'
               GO TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1110-TESTAR-FS-ENTRADA.

           ADD 1                       TO ACU-LIDOS-ENTRADA.

           MOVE WRK-CH-ATU-CESTA       TO WRK-CH-ANT-CESTA.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCESSAMENTO PRINCIPAL                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 3100-CONSISTENCIA-FISICA.

           IF WRK-ERRO EQUAL TO 'N' AND
              F17-TR   EQUAL TO 1
              PERFORM 3200-CONSISTENCIA-LOGICA
           END-IF.

           IF F17-TR EQUAL TO 1
              IF WRK-ERRO EQUAL TO 'S'
                 PERFORM 3500-GRAVAR-REJEITADOS
              ELSE
                 PERFORM 3600-GRAVAR-GFCTB0P0
              END-IF
           END-IF.

           PERFORM 2100-LER-ENTRADA.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE CONSISTENCIA FISICA                               *
      ******************************************************************
      *----------------------------------------------------------------*
       3100-CONSISTENCIA-FISICA        SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-ERRO.
           MOVE SPACES                 TO WRK-MSG.

           IF (F17-TR                  NOT NUMERIC     OR
              (F17-TR                  NOT EQUAL ZERO  AND
               F17-TR                  NOT EQUAL 1     AND
               F17-TR                  NOT EQUAL 9))
               MOVE 'S'                 TO WRK-ERRO
               MOVE 'TIPO DE REGISTRO INVALIDO' TO WRK-MSG
               GO TO 3100-99-FIM
           END-IF.

           IF (ACU-LIDOS-ENTRADA       EQUAL 1         AND
               F17-TR                  NOT EQUAL ZERO)
              MOVE 'APL'               TO ERR-TIPO-ACESSO
              MOVE 'FALTOU REGISTRO HEADER' TO ERR-TEXTO
              PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE F17-TR TO WRK-ULT-TR-LIDO.

           EVALUATE F17-TR
               WHEN 0 PERFORM 3110-CONSISTE-HEADER
               WHEN 1 PERFORM 3120-CONSISTE-DADOS
               WHEN 9 PERFORM 3130-CONSISTE-TRAILER
           END-EVALUATE.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE CONSISTENCIA FISICA HEADER                        *
      ******************************************************************
      *----------------------------------------------------------------*
       3110-CONSISTE-HEADER            SECTION.
      *----------------------------------------------------------------*

           IF (F17-DATA-GERACAO  NOT NUMERIC OR
               F17-DATA-GERACAO EQUAL ZEROS)
              MOVE 'APL'               TO ERR-TIPO-ACESSO
              MOVE 'DATA GERACAO INVALIDA' TO ERR-TEXTO
              PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE F17-DATA-GERACAO TO WRK-DATA-AMD.
           PERFORM 5700-VALIDAR-DATA.

           IF WRK-DATA-VALIDA EQUAL 'N'
              MOVE 'APL'               TO ERR-TIPO-ACESSO
              MOVE 'DATA GERACAO INVALIDA' TO ERR-TEXTO
              PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF F17-NOME-ARQUIVO EQUAL SPACES
              MOVE 'APL'               TO ERR-TIPO-ACESSO
              MOVE 'NOME ARQUIVO INVALIDO' TO ERR-TEXTO
              PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE  WRK-DIA-AMD           TO WRK-DG-DD.
           MOVE  WRK-MES-AMD           TO WRK-DG-MM.
           MOVE  WRK-ANO-AMD           TO WRK-DG-AAAA.
           MOVE  WRK-MES-AMD           TO WRK-MES-AM.
           MOVE  WRK-ANO-AMD           TO WRK-ANO-AM.
           MOVE  F17-NOME-ARQUIVO      TO WRK-NOME-ARQUIVO.

      *----------------------------------------------------------------*
       3110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE CONSISTENCIA FISICA DADOS                         *
      ******************************************************************
      *----------------------------------------------------------------*
       3120-CONSISTE-DADOS             SECTION.
      *----------------------------------------------------------------*

           IF (F17-AGENCIA NOT NUMERIC OR
               F17-AGENCIA EQUAL ZEROS)
              MOVE 'S'                TO WRK-ERRO
              MOVE 'AGENCIA INVALIDA' TO WRK-MSG
              GO TO 3120-99-FIM
           END-IF.

           IF (F17-CONTA NOT NUMERIC OR
               F17-CONTA EQUAL ZEROS)
              MOVE 'S'                TO WRK-ERRO
              MOVE 'CONTA INVALIDA' TO WRK-MSG
              GO TO 3120-99-FIM
           END-IF.

           IF F17-DIGITO NOT NUMERIC
              MOVE 'S'                TO WRK-ERRO
              MOVE 'DIGITO INVALIDO' TO WRK-MSG
              GO TO 3120-99-FIM
           END-IF.

ST25X6*    IF F17-CPF-CNPJ NOT NUMERIC
ST25X6     IF (F17-CPF-CNPJ EQUAL SPACES OR
ST25X6         F17-CPF-CNPJ EQUAL ZEROS)
ST25X6         MOVE 'S'                TO WRK-ERRO
ST25X6         MOVE 'CPF CNPJ INVALIDO' TO WRK-MSG
ST25X6         GO TO 3120-99-FIM
ST25X6     END-IF.

ST25X6*    IF F17-FILIAL NOT NUMERIC
ST25X6     IF (F17-FILIAL EQUAL SPACES OR
ST25X6         F17-FILIAL EQUAL ZEROS)
ST25X6         MOVE 'S'                TO WRK-ERRO
ST25X6         MOVE 'FILIAL CNPJ INVALIDO' TO WRK-MSG
ST25X6         GO TO 3120-99-FIM
ST25X6     END-IF.

           IF F17-DIGITO-FJ NOT NUMERIC
              MOVE 'S'                TO WRK-ERRO
              MOVE 'DIGITO CPF CNPJ INVALIDO' TO WRK-MSG
              GO TO 3120-99-FIM
           END-IF.

           IF (F17-CESTA NOT NUMERIC OR
               F17-CESTA EQUAL ZEROS)
              MOVE 'S'                TO WRK-ERRO
              MOVE 'CESTA INVALIDA' TO WRK-MSG
              GO TO 3120-99-FIM
           END-IF.

           IF F17-AM-APUR NOT NUMERIC
              MOVE 'S'                TO WRK-ERRO
              MOVE 'ANO MES APURACAO INVALIDO' TO WRK-MSG
              GO TO 3120-99-FIM
           END-IF.

           MOVE F17-AM-APUR TO WRK-DATA-AM.
           MOVE WRK-ANO-AM  TO WRK-ANO-AMD.
           MOVE WRK-MES-AM  TO WRK-MES-AMD.
           MOVE 1           TO WRK-DIA-AMD.

           PERFORM 5700-VALIDAR-DATA.

           IF WRK-DATA-VALIDA EQUAL 'N'
              MOVE 'S'                TO WRK-ERRO
              MOVE 'ANO MES APURACAO INVALIDO' TO WRK-MSG
              GO TO 3120-99-FIM
           END-IF.

           MOVE F17-AGENCIA            TO WRK-ATU-AGENCIA.
           MOVE F17-CONTA              TO WRK-ATU-CONTA.
           MOVE F17-AM-APUR            TO WRK-ATU-AM-APUR.

           IF WRK-CH-ATU          LESS THAN WRK-CH-ANT

              DISPLAY '************ GFCT4708 ************'
              DISPLAY '*                                *'
              DISPLAY '*        ARQUIVO ENTRADA         *'
              DISPLAY '*        NAO CLASSIFICADO        *'
              DISPLAY '*                                *'
              DISPLAY '*       PROGRAMA ENCERRADO       *'
              DISPLAY '*                                *'
              DISPLAY '************ GFCT4701 ************'
              MOVE 'APL'              TO ERR-TIPO-ACESSO
              MOVE 'ENTRADA'          TO WRK-NOME-ARQ
              MOVE WRK-FS-ENTRADA     TO WRK-FILE-STATUS
              MOVE WRK-ERRO-FILE-STATUS  TO ERR-TEXTO
              PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.


           IF F17-VSDO-MED-FUNDO NOT NUMERIC
              MOVE 'S'                TO WRK-ERRO
              MOVE 'SALDO MEDIO FUNDOS INVALIDO' TO WRK-MSG
              GO TO 3120-99-FIM
           END-IF.

           IF F17-VSDO-MED-POUP NOT NUMERIC
              MOVE 'S'                TO WRK-ERRO
              MOVE 'SALDO MEDIO POUPANCA INVALIDO' TO WRK-MSG
              GO TO 3120-99-FIM
           END-IF.

           IF F17-VSDO-MED-CDB NOT NUMERIC
              MOVE 'S'                TO WRK-ERRO
              MOVE 'SALDO MEDIO CDB INVALIDO' TO WRK-MSG
              GO TO 3120-99-FIM
           END-IF.

           IF F17-VSDO-MED-PREVD NOT NUMERIC
              MOVE 'S'                TO WRK-ERRO
              MOVE 'SALDO MEDIO PREVID. INVALIDO' TO WRK-MSG
              GO TO 3120-99-FIM
           END-IF.

           IF F17-VSDO-APLIC-ACOES NOT NUMERIC
              MOVE 'S'                TO WRK-ERRO
              MOVE 'SALDO EM ACOES INVALIDO' TO WRK-MSG
              GO TO 3120-99-FIM
           END-IF.

           IF F17-VSDO-APLIC-TSORO NOT NUMERIC
              MOVE 'S'                TO WRK-ERRO
              MOVE 'SALDO TESOURO DIRETO INVALIDO' TO WRK-MSG
              GO TO 3120-99-FIM
           END-IF.

           IF (F17-VSDO-MED-FUNDO   EQUAL TO ZEROS AND
               F17-VSDO-MED-POUP    EQUAL TO ZEROS AND
               F17-VSDO-MED-CDB     EQUAL TO ZEROS AND
               F17-VSDO-MED-PREVD   EQUAL TO ZEROS AND
               F17-VSDO-APLIC-ACOES EQUAL TO ZEROS AND
               F17-VSDO-APLIC-TSORO EQUAL TO ZEROS)
              MOVE 'S'                TO WRK-ERRO
              MOVE 'TODOS VALORES ZERADOS' TO WRK-MSG
              GO TO 3120-99-FIM
           END-IF.

      *----------------------------------------------------------------*
       3120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE CONSISTENCIA FISICA TRAILER                       *
      ******************************************************************
      *----------------------------------------------------------------*
       3130-CONSISTE-TRAILER           SECTION.
      *----------------------------------------------------------------*

           IF (F17-QTDE-REGISTROS NOT NUMERIC OR
               F17-QTDE-REGISTROS NOT EQUAL ACU-LIDOS-ENTRADA)
              MOVE 'APL'              TO ERR-TIPO-ACESSO
              MOVE 'REGISTRO TRAILER INVALIDO' TO ERR-TEXTO
              PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       3130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE CONSISTENCIA LOGICA                               *
      ******************************************************************
      *----------------------------------------------------------------*
       3200-CONSISTENCIA-LOGICA        SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-ERRO.
           MOVE SPACES                 TO WRK-MSG.

           IF WRK-CH-ATU-AGCT  NOT EQUAL TO WRK-CH-ANT-AGCT
              MOVE F17-AGENCIA      TO F04-AGE-HSBC
              MOVE F17-CONTA        TO F04-CTA-HSBC
              MOVE 1                TO F04-T-CTA-HSBC
              MOVE LNK-PESQ-CLIE    TO F04-PESQ-CLIE
              MOVE 'GFCT5801'       TO WRK-MODULO
              CALL WRK-MODULO USING F04-DP-CONTAS-HSBC-REG

              EVALUATE F04-COD-RET
                  WHEN ZEROS
                       CONTINUE
                  WHEN 04
                       MOVE 'S'             TO WRK-ERRO
                       MOVE 'AG/CTA HSBC NAO LOCALIZADA' TO WRK-MSG
                  WHEN 08
                       MOVE 'S'             TO WRK-ERRO
                       MOVE 'AG/CTA HSBC INVALIDA' TO WRK-MSG
                  WHEN OTHER
                       MOVE 'S'             TO WRK-ERRO
                       MOVE 'GFCT5801 - ERRO SISTEMICO' TO WRK-MSG
              END-EVALUATE
           END-IF.

           MOVE F17-CESTA                 TO WRK-ATU-CESTA.

           IF WRK-CH-ATU-CESTA NOT EQUAL TO WRK-CH-ANT-CESTA
              MOVE WRK-ATU-CESTA         TO F03-TAR-HSBC
              MOVE 'GFCT5800'            TO WRK-MODULO
              CALL WRK-MODULO      USING F03-DP-CESTA-HSBC-REG
              EVALUATE F03-TAR-COD-RET
                  WHEN ZEROS
                       CONTINUE
                  WHEN 04
                       MOVE 'S'                TO WRK-ERRO
                       MOVE 'TARIFA HSBC NAO LOCALIZADA' TO WRK-MSG
                  WHEN 08
                       MOVE 'S'                TO WRK-ERRO
                       MOVE 'TARIFA HSBC INVALIDA' TO WRK-MSG
                  WHEN OTHER
                       MOVE 'S'                TO WRK-ERRO
                       MOVE 'GFCT5800 - ERRO SISTEMICO' TO WRK-MSG
              END-EVALUATE
           END-IF.

           IF WRK-ERRO EQUAL TO 'S'
              GO TO 3200-99-FIM
           END-IF.

           IF WRK-CH-ATU EQUAL TO WRK-CH-ANT
              MOVE 'S'             TO WRK-ERRO
              MOVE 'REGISTRO DUPLICADO' TO WRK-MSG
              GO TO 3200-99-FIM
           END-IF.

           MOVE WRK-CH-ATU             TO WRK-CH-ANT.
      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *   GRAVAR REJEITADOS                                            *
      ******************************************************************
      *----------------------------------------------------------------*
       3500-GRAVAR-REJEITADOS          SECTION.
      *----------------------------------------------------------------*

           MOVE F17-DADOS              TO F18-DADOS.
           MOVE WRK-MSG                TO F18-MSG-ERRO.
           MOVE SPACES                 TO F18-RESERVA.

           WRITE FD-REJEITAD           FROM F18-REGISTRO.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 1120-TESTAR-FS-REJEITAD.

           ADD 1 TO ACU-GRAVADOS-REJEITAD.

      *----------------------------------------------------------------*
       3500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *   GRAVAR GFCTB0P0 - MOVTO_EVNTO_CRRTT                          *
      ******************************************************************
      *----------------------------------------------------------------*
       3600-GRAVAR-GFCTB0P0            SECTION.
      *----------------------------------------------------------------*

           MOVE 237                    TO WF45-CBCO.
           MOVE F04-AGE-BRAD           TO WF45-CAG-BCRIA.
           MOVE F04-CTA-BRAD           TO WF45-CCTA-CLI.
           MOVE F17-AM-APUR            TO WF45-DANO-MES-SDO.
           MOVE F03-TAR-BRAD           TO WF45-CSERVC-TARIF.
           MOVE F17-VSDO-MED-FUNDO     TO WF45-VSDO-MED-FUNDO.
           MOVE F17-VSDO-MED-POUP      TO WF45-VSDO-MED-POUP.
           MOVE F17-VSDO-MED-CDB       TO WF45-VSDO-MED-CERTF.
           MOVE F17-VSDO-MED-PREVD     TO WF45-VSDO-MED-PREVD.
           MOVE F17-VSDO-APLIC-ACOES   TO WF45-VSDO-APLIC-ACOES.
           MOVE F17-VSDO-APLIC-TSORO   TO WF45-VSDO-APLIC-TSORO.
           MOVE ZEROS                  TO WF45-VSDO-APLIC-DBTRS.
           MOVE ZEROS                  TO WF45-VSDO-APLIC-DERIV.
           IF LNK-PESQ-CLIE EQUAL TO 'S'
              MOVE F04-CGC-BRAD        TO WF45-CNPJ-CPF
              MOVE F04-FIL-BRAD        TO WRK-F04-FIL-BRAD
              MOVE WRK-F04-FIL-BRAD-4  TO WF45-FILIAL
              MOVE F04-CTR-BRAD        TO WF45-DIGITO-FJ
           ELSE
              MOVE F17-CPF-CNPJ        TO WF45-CNPJ-CPF
              MOVE F17-FILIAL          TO WF45-FILIAL
              MOVE F17-DIGITO-FJ       TO WF45-DIGITO-FJ
           END-IF.

           MOVE F17-AGENCIA            TO WF45-AGENCIA-DE.
           MOVE F17-CONTA              TO WF45-CONTA-DE.

           WRITE FD-GFCTB0P0           FROM WF45-REGISTRO.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 1130-TESTAR-FS-GFCTB0P0.

           ADD 1 TO ACU-GRAVADOS-GFCTB0P0.

           MOVE 'N'                    TO WRK-ERRO.
           MOVE SPACES                 TO WRK-MSG.

      *----------------------------------------------------------------*
       3600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *   VALIDAR DATA                                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       5700-VALIDAR-DATA               SECTION.
      *----------------------------------------------------------------*

           MOVE 'S'                    TO WRK-DATA-VALIDA.

           MOVE WRK-ANO-AMD            TO WRK-ANO-DMA
           MOVE WRK-MES-AMD            TO WRK-MES-DMA
           MOVE WRK-DIA-AMD            TO WRK-DIA-DMA

           CALL 'POOL1470'             USING WRK-DATA-DMA.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'N'                TO WRK-DATA-VALIDA
           END-IF.

      *----------------------------------------------------------------*
       5700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA QUE EFETUA PROCESSAMENTO FINAL DO PROGRAMA           *
      ******************************************************************
      *----------------------------------------------------------------*
       9000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           IF (ACU-LIDOS-ENTRADA NOT EQUAL ZEROS AND
               WRK-ULT-TR-LIDO   NOT EQUAL TO 9)
              MOVE 'APL'              TO ERR-TIPO-ACESSO
              MOVE 'FALTOU REGISTRO TRAILER' TO ERR-TEXTO
              PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 9100-EMITIR-DISPLAY.

           CLOSE ENTRADA
                 REJEITAD
                 GFCTB0P0.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

           STOP RUN.

      *----------------------------------------------------------------*
       9000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE EMISSAO DE DISPLAYS DE TOTAIS                     *
      ******************************************************************
      *----------------------------------------------------------------*
       9100-EMITIR-DISPLAY             SECTION.
      *----------------------------------------------------------------*

           IF  ACU-LIDOS-ENTRADA           NOT EQUAL ZEROS
               DISPLAY '*-----------------------------------------------
      -        '--------*'
               DISPLAY '*                   PROGRAMA GFCT4708
      -        '        *'
               DISPLAY '*                   -----------------
      -        '        *'
               DISPLAY '*
      -        '        *'
               DISPLAY '*          PROCESSAMENTO  EFETUADO COM SUCESSO
      -        '        *'
               DISPLAY '* INVESTIMENTOS ULTIMOS 03 MESES
      -        '        *'
               DISPLAY '* CONSISTENCIA E GERACAO DE IMAGEM DA TABELA
      -        '        *'
               DISPLAY '* GFCTB0P0 - TPOSIC_SDO_APLIC
      -        '        *'
               DISPLAY '*-----------------------------------------------
      -        '--------*'
               DISPLAY '* ARQUIVO - ' WRK-NOME-ARQUIVO '              *'
               DISPLAY '*
      -        '        *'
               DISPLAY '* GERACAO - ' WRK-DATA-GERACAO
                       '                                  *'
               DISPLAY '*-----------------------------------------------
      -        '--------*'
               DISPLAY '************************************************
      -        '*********'
               MOVE ACU-LIDOS-ENTRADA      TO WRK-MASCARA
               DISPLAY '* TOTAL DE REG. LIDOS              = '
                 WRK-MASCARA
                           ' *'
               MOVE ACU-GRAVADOS-GFCTB0P0  TO WRK-MASCARA
               DISPLAY '* TOTAL DE REG. GRAVADOS  GFCTB0P0 = '
                 WRK-MASCARA
                           ' *'
               MOVE ACU-GRAVADOS-REJEITAD  TO WRK-MASCARA
               DISPLAY '* TOTAL DE REG. REJEITADOS         = '
                   WRK-MASCARA
                            ' *'

               DISPLAY '************************************************
      -        '*********'
           ELSE
               DISPLAY '*-----------------------------------------------
      -        '*'
               DISPLAY '*               PROGRAMA GFCT4708
      -        '*'
               DISPLAY '*               -----------------
      -        '*'
               DISPLAY '*
      -        '*'
               DISPLAY '*           ARQUIVO DE ENTRADA VAZIO
      -        '*'
               DISPLAY '*-----------------------------------------------
      -        '*'
           END-IF.

      *----------------------------------------------------------------*
       9100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE PROCESSAMENTO DE ERRRO                            *
      ******************************************************************
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT4708'             TO ERR-PGM.

           CALL 'POOL7100'         USING WRK-BATCH
                                         ERRO-AREA.
           MOVE 4                      TO RETURN-CODE.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
