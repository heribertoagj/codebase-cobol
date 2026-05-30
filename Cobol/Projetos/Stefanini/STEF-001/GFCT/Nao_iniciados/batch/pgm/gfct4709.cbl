      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT4709.
       AUTHOR.     HOMI.
      *================================================================*
      *      S O N D A  I T                                            *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT4709                                    *
      *    PROGRAMADOR.:   HOMI                                        *
      *    ANALISTA....:   PAGNOCCA - SONDA IT - GR. 50                *
      *    DATA........:   NOV/2015                                    *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   INVESTIMENTOS ULTIMOS 03 MESES CORRETORA    *
      *                    CONSISTENCIA E IMAGEM DA TABELA GFCTB0P0    *
      *                    + CNPJ/CPF                                  *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                          INCLUDE/BOOK    *
      *                     REJEITAD                     GFCTWF21      *
      *                     GFCTB0P0                     GFCTWF25      *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                         INCLUDE/BOOK    *
      *                   DB2PRD.ADSAO_INDVD_PCOTE       GFCTB009      *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    CKRS1000 - CONEXAO COM DB2                                  *
      *    BRAD7100 - MODULO TRATAMENTO DE ERRO                        *
      *    POOL1470 - VALIDAR DATA                                     *
      *                                                                *
      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

           DECIMAL-POINT               IS COMMA.

      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*

       FILE-CONTROL.

           SELECT CORRETOR   ASSIGN    TO UT-S-CORRETOR
           FILE STATUS                 IS WRK-FS-CORRETOR.

           SELECT REJEITAD   ASSIGN    TO UT-S-REJEITAD
           FILE STATUS                 IS WRK-FS-REJEITAD.

           SELECT GFCTB0P0   ASSIGN    TO UT-S-GFCTB0P0
           FILE STATUS                 IS WRK-FS-GFCTB0P0.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:  ARQUIVO CORRETOR                                    *
      *            ORG. SEQUENCIAL     - LRECL = 104                   *
      *----------------------------------------------------------------*

       FD  CORRETOR
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-CORRETOR                 PIC X(104).

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO RGISTROS REJEITADOS.                        *
      *            ORG. SEQUENCIAL     - LRECL = 170                   *
      *----------------------------------------------------------------*

       FD  REJEITAD
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REJEITAD                 PIC X(170).

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO GFCTB0P0                                    *
      *            ORG. SEQUENCIAL     - LRECL = 098                   *
      *----------------------------------------------------------------*

       FD  GFCTB0P0
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-GFCTB0P0                 PIC X(098).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

       77  FILLER                      PIC  X(033)         VALUE
           'INICIO DA WORKING STORAGE SECTION'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(022)         VALUE
           'AREA PARA ACUMULADORES'.
      *----------------------------------------------------------------*

       77  ACU-LIDOS-CORRETOR          PIC  9(015) COMP-3  VALUE ZEROS.
       77  ACU-GRAVADOS-REJEITAD       PIC  9(015) COMP-3  VALUE ZEROS.
       77  ACU-GRAVADOS-GFCTB0P0       PIC  9(015) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(030)         VALUE
           'AREA PARA TESTE DE FILE STATUS'.
      *----------------------------------------------------------------*

       77  WRK-FS-CORRETOR             PIC  X(002)         VALUE SPACES.
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
       77  WRK-MASCARA             PIC  ZZZZZZZZZ.ZZZ.999 VALUE ZEROS.
       77  WRK-ERRO                    PIC  X(001)        VALUE SPACES.
       77  WRK-MSG                     PIC  X(030)        VALUE SPACES.
       77  WRK-NOME-ARQUIVO            PIC  X(030)        VALUE SPACES.
       77  WRK-DATA-VALIDA             PIC  X(001)        VALUE 'N'.
       77  WRK-ULT-TR-LIDO             PIC  9(001)        VALUE ZEROS.

       01  REDEFINES WRK-9-13.
           05                          PIC  X(006).
           05 WRK-9-7                  PIC  9(007).

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
       01  REDEFINES WRK-DATA-AMD.
           03  WRK-ANO-MES-AMD         PIC  9(006).
           03                          PIC  X(002).

       01  WRK-DATA-GERACAO.
           03 WRK-DG-DD                PIC  9(002)        VALUE ZEROS.
           03 WRK-DG-MM                PIC  9(002)        VALUE ZEROS.
           03                          PIC  X             VALUE '/'.
           03 WRK-DG-AAAA              PIC  9(004)        VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(019)         VALUE
           ' AREA DE CORRETOR'.
      *----------------------------------------------------------------*

       COPY 'GFCTWG00'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(019)         VALUE
           ' AREA DE REJEITAD'.
      *----------------------------------------------------------------*

       COPY 'GFCTWF21'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(019)         VALUE
           ' AREA DE GFCTB0P0'.

       01  WRK-WF25-REGISTRO.
       COPY 'GFCTWF25'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(026)         VALUE
           'AREA DE ERRO - FILE-STATUS'.
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

       01  FILLER                      PIC  X(030)         VALUE
           '*       AREA TO BRAD7100    *'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(030)         VALUE
           'FIM DA WORKING STORAGE SECTION'.
      *----------------------------------------------------------------*
      *================================================================*
       PROCEDURE DIVISION.
      *================================================================*

      ******************************************************************
      *    CHAMA AS ROTINAS PRINCIPAIS DO PROCESSAMENTO.               *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*


           PERFORM 2000-VERIFICAR-VAZIO.

           PERFORM 3000-PROCESSAR      UNTIL
               WRK-FS-CORRETOR         EQUAL '10'.

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

           OPEN INPUT  CORRETOR
                OUTPUT REJEITAD

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.


      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA QUE EFETUA TESTE DE FILE-STATUS.                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-CORRETOR.

           PERFORM 1120-TESTAR-FS-REJEITAD.


      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE TESTE PARA ARQUIVO CORRETOR.                      *
      ******************************************************************
      *----------------------------------------------------------------*
       1110-TESTAR-FS-CORRETOR         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-CORRETOR         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'CORRETOR'         TO WRK-NOME-ARQ
               MOVE WRK-FS-CORRETOR    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-FILE-STATUS  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
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
      *    VERIFICAR SE ARQUIVO CORRETOR ESTA VAZIO                    *
      ******************************************************************
       2000-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-LER-CORRETOR.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA QUE EFETUA LEITURA  TRATAMENTO DO ARQUIVO DE CORRETOR*
      ******************************************************************
      *----------------------------------------------------------------*
       2100-LER-CORRETOR               SECTION.
      *----------------------------------------------------------------*

           READ CORRETOR INTO G00-CORRET-HSBC-REG.

           IF  WRK-FS-CORRETOR         EQUAL '10'
               GO TO 2100-99-FIM
           END-IF.
           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1110-TESTAR-FS-CORRETOR.

           ADD 1                       TO ACU-LIDOS-CORRETOR.

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

           IF WRK-ERRO = 'S'
              PERFORM 3500-GRAVA-REJEITADOS
              IF G00-COR-H-TP-REG-R EQUAL TO 1
                 PERFORM 3600-GRAVAR-GFCTB0P0
              END-IF
           END-IF.

           PERFORM 2100-LER-CORRETOR.

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

              (G00-COR-H-TP-REG-R      NOT EQUAL ZERO  AND
               G00-COR-H-TP-REG-R      NOT EQUAL 1     AND
               G00-COR-H-TP-REG-R      NOT EQUAL 9))
               MOVE 'S'                 TO WRK-ERRO
               MOVE 'TIPO DE REGISTRO INVALIDO' TO WRK-MSG
               GO TO 3100-99-FIM
           END-IF.

           IF (ACU-LIDOS-CORRETOR      EQUAL 1         AND
               G00-COR-H-TP-REG-R      NOT EQUAL ZERO)
              MOVE 'APL'               TO ERR-TIPO-ACESSO
              MOVE 'FALTOU REGISTRO HEADER' TO ERR-TEXTO
              PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE G00-COR-H-TP-REG-R TO WRK-ULT-TR-LIDO.

           EVALUATE G00-COR-H-TP-REG-R
               WHEN 0 PERFORM 3110-CONSISTE-HEADER
               WHEN 1 PERFORM 3120-CONSISTE-DADOS
               WHEN 9 PERFORM 3130-CONSISTE-TRAILER

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE CONSISTENCIA FISICA HEADER                        *
      ******************************************************************
      *----------------------------------------------------------------*
       3110-CONSISTE-HEADER            SECTION.
      *----------------------------------------------------------------*

           IF (G00-COR-H-DT-GER  NOT NUMERIC OR
               G00-COR-H-DT-GER EQUAL ZEROS)
              MOVE 'S'                 TO WRK-ERRO
              MOVE 'DATA GERACAO INVALIDA' TO WRK-MSG
              GO TO 3110-99-FIM
           END-IF.

           MOVE G00-COR-H-DT-GER-R TO WRK-DATA-AMD.
           PERFORM 5700-VALIDAR-DATA.
           IF WRK-DATA-VALIDA EQUAL 'N'
              MOVE 'S'                 TO WRK-ERRO
              MOVE 'DATA GERACAO INVALIDA' TO WRK-MSG
              GO TO 3110-99-FIM
           END-IF.

           IF G00-COR-H-NM-REG EQUAL SPACES
              MOVE 'S'                 TO WRK-ERRO
              MOVE 'NOME ARQUIVO INVALIDO' TO WRK-MSG
              GO TO 3110-99-FIM
           END-IF.

           MOVE  WRK-DIA-AMD           TO WRK-DG-DD.
           MOVE  WRK-MES-AMD           TO WRK-DG-MM.
           MOVE  WRK-ANO-AMD           TO WRK-DG-AAAA.
           MOVE  G00-COR-H-NM-REG      TO WRK-NOME-ARQUIVO.

      *----------------------------------------------------------------*
       3110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *    ROTINA DE CONSISTENCIA FISICA DADOS                         *
      ******************************************************************
      *----------------------------------------------------------------*
       3120-CONSISTE-DADOS             SECTION.
      *----------------------------------------------------------------*

           IF (G00-COR-D-AGEN NOT NUMERIC OR
               G00-COR-D-AGEN-R EQUAL ZEROS)
              MOVE 'S'                TO WRK-ERRO
              MOVE 'AGENCIA INVALIDA' TO WRK-MSG
              GO TO 3120-99-FIM
           END-IF.

           IF (G00-COR-D-CTA NOT NUMERIC OR
               G00-COR-D-CTA-R EQUAL ZEROS)
              MOVE 'S'                TO WRK-ERRO
              MOVE 'CONTA INVALIDA' TO WRK-MSG
              GO TO 3120-99-FIM
           END-IF.

ST25X6*    IF G00-COR-D-CPF-CNPJ NOT NUMERIC
ST25X6     IF (G00-COR-D-CPF-CNPJ EQUAL SPACES OR
ST25X6         G00-COR-D-CPF-CNPJ EQUAL ZEROS)
              MOVE 'CPF/CNPJ INVALIDO' TO WRK-MSG
              GO TO 3120-99-FIM
           END-IF.

ST25X6*    IF G00-COR-D-FILIAL NOT NUMERIC
ST25X6     IF (G00-COR-D-FILIAL EQUAL SPACES OR
ST25X6         G00-COR-D-FILIAL EQUAL ZEROS)
              MOVE 'S'                TO WRK-ERRO
              MOVE 'FILIAL CNPJ INVALIDO' TO WRK-MSG
              GO TO 3120-99-FIM
           END-IF.

           IF G00-COR-D-CTR  NOT NUMERIC
              MOVE 'S'                TO WRK-ERRO
              MOVE 'DIGITO CPF/CNPJ INVALIDO' TO WRK-MSG
              GO TO 3120-99-FIM
           END-IF.

           IF (G00-COR-D-AM-APUR NOT NUMERIC OR
               G00-COR-D-AM-APU-R EQUAL ZEROS)
              MOVE 'S'                TO WRK-ERRO
              MOVE 'ANO MES APURACAO INVALIDA' TO WRK-MSG
              GO TO 3120-99-FIM

           MOVE G00-COR-D-AM-APU-R TO WRK-ANO-MES-AMD.
           MOVE 01                 TO WRK-DIA-AMD.
           PERFORM 5700-VALIDAR-DATA.

           IF WRK-DATA-VALIDA EQUAL 'N'
              MOVE 'S'                TO WRK-ERRO
              MOVE 'ANO MES APURACAO INVALIDA' TO WRK-MSG
              GO TO 3120-99-FIM
           END-IF.

           IF G00-COR-D-SD-ACOES NOT NUMERIC
              MOVE 'S'                TO WRK-ERRO
              MOVE 'SALDO ACOES INVALIDO' TO WRK-MSG
              GO TO 3120-99-FIM
           END-IF.

           IF G00-COR-D-SD-TESO NOT NUMERIC
              MOVE 'S'                TO WRK-ERRO
              MOVE 'SALDO TESOURO INVALIDO' TO WRK-MSG
              GO TO 3120-99-FIM

      *----------------------------------------------------------------*
       3120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE CONSISTENCIA FISICA TRAILER                       *
      ******************************************************************
      *----------------------------------------------------------------*
       3130-CONSISTE-TRAILER           SECTION.
      *----------------------------------------------------------------*

           IF (G00-COR-T-QTD-REGS NOT NUMERIC OR
               G00-COR-T-QTD-REGS-R NOT EQUAL ACU-LIDOS-CORRETOR)
              MOVE 'APL'              TO ERR-TIPO-ACESSO
              MOVE 'REGISTRO TRAILER INVALIDO' TO ERR-TEXTO
              PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       3130-99-FIM.                    EXIT.

      ******************************************************************
      *   GRAVAR REJEITADOS                                            *
      ******************************************************************
      *----------------------------------------------------------------*
       3500-GRAVA-REJEITADOS           SECTION.
      *----------------------------------------------------------------*

           MOVE G00-CORRET-HSBC-DETALHE TO F21-CORRET-HSBC-DETALHE.
           MOVE WRK-MSG                 TO F21-MSG-ERRO.
           MOVE SPACES                  TO F21-RESERVA.

           WRITE FD-REJEITAD            FROM F21-REGISTRO.

           MOVE WRK-GRAVACAO            TO WRK-OPERACAO.

           PERFORM 1120-TESTAR-FS-REJEITAD.

           ADD 1 TO ACU-GRAVADOS-REJEITAD.

      *----------------------------------------------------------------*
      *----------------------------------------------------------------*

      ******************************************************************
      *   TRATAR GFCTB0P0                                              *
      ******************************************************************
      *----------------------------------------------------------------*
       3600-GRAVAR-GFCTB0P0            SECTION.
      *----------------------------------------------------------------*

           MOVE 237                    TO WF25-CBCO.
           MOVE G00-COR-D-AGEN-R       TO WF25-CAG-BCRIA.
           MOVE G00-COR-D-CTA-R        TO WRK-9-13.
           MOVE WRK-9-7                TO WF25-CCTA-CLI.
           MOVE G00-COR-D-AM-APU-R     TO WF25-DANO-MES-SDO.
           MOVE ZEROS                  TO WF25-CSERVC-TARIF.
           MOVE ZEROS                  TO WF25-VSDO-MED-FUNDO.
           MOVE ZEROS                  TO WF25-VSDO-MED-POUP.
           MOVE ZEROS                  TO WF25-VSDO-MED-CERTF.
           MOVE ZEROS                  TO WF25-VSDO-MED-PREVD.
           MOVE G00-COR-D-SD-ACOES-R   TO WF25-VSDO-APLIC-ACOES.
           MOVE G00-COR-D-SD-TESO-R    TO WF25-VSDO-APLIC-TSORO.
           MOVE ZEROS                  TO WF25-VSDO-APLIC-DERIV.
           MOVE G00-COR-D-CPF-CNPJ-R   TO WF25-CNPJ-CPF.
           MOVE G00-COR-D-FILIAL-R     TO WF25-FILIAL.
           MOVE G00-COR-D-CTR-R        TO WF25-DIGITO-FJ.

           WRITE FD-GFCTB0P0           FROM WRK-WF25-REGISTRO.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 1130-TESTAR-FS-GFCTB0P0.

           ADD 1 TO ACU-GRAVADOS-GFCTB0P0.

      *----------------------------------------------------------------*
       3600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *   VALIDAR DATA                                                 *
      ******************************************************************
      *----------------------------------------------------------------*
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
       9000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           IF ACU-LIDOS-CORRETOR GREATER THAN ZEROS AND
              WRK-ULT-TR-LIDO NOT EQUAL TO 9
               MOVE ACU-LIDOS-CORRETOR      TO WRK-MASCARA
               DISPLAY '* TOTAL DE REGISTROS LIDOS              = '
                 WRK-MASCARA
               DISPLAY '* WRK-ULT-TR-LIDO                       = '
               WRK-ULT-TR-LIDO
              MOVE 'APL'              TO ERR-TIPO-ACESSO
              MOVE 'FALTOU REGISTRO TRAILER' TO ERR-TEXTO
              PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 9100-EMITIR-DISPLAY.

           CLOSE CORRETOR
                 REJEITAD
                 GFCTB0P0.


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

           IF  ACU-LIDOS-CORRETOR           NOT EQUAL ZEROS
               DISPLAY '*-----------------------------------------------
      -        '--------*'
               DISPLAY '*                   PROGRAMA GFCT4709
      -        '        *'
      -        '        *'
               DISPLAY '*
      -        '        *'
               DISPLAY '*          PROCESSAMENTO  EFETUADO COM SUCESSO
      -        '        *'
               DISPLAY '* INVESTIMENTOS ULTIMOS 03 MESES CORRETORA
      -        '        *'
               DISPLAY '* ARQUIVO - ' WRK-NOME-ARQUIVO '              *'
               DISPLAY '*
      -        '        *'
               DISPLAY '* GERACAO - ' WRK-DATA-GERACAO
                       '                                  *'
               DISPLAY '*-----------------------------------------------
      -        '--------*'
               DISPLAY '************************************************
      -        '*********'
               MOVE ACU-LIDOS-CORRETOR      TO WRK-MASCARA
               DISPLAY '* TOTAL DE REGISTROS LIDOS              = '
                 WRK-MASCARA
                           ' *'
               MOVE ACU-GRAVADOS-REJEITAD  TO WRK-MASCARA
                   WRK-MASCARA
                            ' *'
               MOVE ACU-GRAVADOS-GFCTB0P0 TO WRK-MASCARA
               DISPLAY '* TOTAL DE REGISTROS GRAVADOS GFCTB0P0  = '
                 WRK-MASCARA
                           ' *'
               DISPLAY '************************************************
      -        '*********'
           ELSE
               DISPLAY '*-----------------------------------------------
      -        '*'
               DISPLAY '*               PROGRAMA GFCT4709
      -        '*'
               DISPLAY '*               -----------------
      -        '*'
               DISPLAY '*
      -        '*'
               DISPLAY '*           ARQUIVO DE CORRETOR VAZIO
      -        '*'
               DISPLAY '*-----------------------------------------------
      -        '*'

      *----------------------------------------------------------------*
       9100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE PROCESSAMENTO DE ERRRO                            *
      ******************************************************************
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT4709'             TO ERR-PGM.

           CALL 'BRAD7100'         USING WRK-BATCH
                                         ERRO-AREA.

           MOVE 4                      TO RETURN-CODE.

           GOBACK.

       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
