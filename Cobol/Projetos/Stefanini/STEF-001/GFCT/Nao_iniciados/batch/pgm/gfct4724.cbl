      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT4724.
       AUTHOR.     HOMI.
      *================================================================*
      *      S O N D A  I T                                            *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT4724                                    *
      *    PROGRAMADOR.:   HOMI                                        *
      *    ANALISTA....:   PAGNOCCA - SONDA IT - GR. 50                *
      *    DATA........:   DEZ/2015                                    *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   INVESTIMENTOS ULTIMOS 03 MESES              *
      *                    PARA/DE SIGB - GERACAO DE IMAGEM DA TABELA  *
      *                    GFCTB0P0 - TPOSIC_SDO_APLIC + CNPJ/CPF      *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                          INCLUDE/BOOK    *
      *                     SIGB06IN                     I#SIGB06      *
      *                     REJEITAD                     GFCTWF26      *
      *                     GFCTB0P0                     GFCTWF25      *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                       INCLUDE/BOOK      *
      *                   DB2PRD.PARM_DATA_PROCM     GFCTB0A1          *
      *                   DB2PRD.ADSAO_INDVD_PCOTE   GFCTB009          *
      *----------------------------------------------------------------*
      *    BOOKS.......:                                               *
      *    I#SIGB06 - ENTRADA VINDA DO SIGB                            *
      *    GFCTWF26 - SAIDA PARA REJEITADOS NA CRITICA                 *
      *    GFCTWF25 - SAIDA IMAGEM DA TABELA GFCTB0P0 + CNPJ/CPF       *
      *    I#CLIB20 - AREA DO CLIB1000                                 *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    POOL7100 - TRATAMENTO DE ERROS.                             *
      *    CLIB1000 - OBTER DADOS CADASTRAIS                           *
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

           SELECT SIGB06IN   ASSIGN     TO UT-S-SIGB06IN
           FILE STATUS                 IS WRK-FS-SIGB06IN.

           SELECT REJEITAD   ASSIGN     TO UT-S-REJEITAD
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
      *    INPUT:  ARQUIVO SIGB06IN                                    *
      *            ORG. SEQUENCIAL     - LRECL =  18                   *
      *----------------------------------------------------------------*

       FD  SIGB06IN
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-SIGB06IN                  PIC X(018).

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO REJEITAD                                    *
      *            ORG. SEQUENCIAL     - LRECL =  60                   *
      *----------------------------------------------------------------*

       FD  REJEITAD
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REJEITAD                  PIC X(060).

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO IMAGEM DA TABELA GFCTB0P0 + CNPJ/CPF        *
      *            ORG. SEQUENCIAL     - LRECL = 98                    *
      *----------------------------------------------------------------*

       FD  GFCTB0P0
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-GFCTB0P0                 PIC X(098).

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

       77  ACU-LIDOS-SIGB06IN          PIC  9(011) COMP-3  VALUE ZEROS.
       77  ACU-GRAVADOS-REJEITAD       PIC  9(011) COMP-3  VALUE ZEROS.
       77  ACU-GRAVADOS-GFCTB0P0       PIC  9(011) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(030)         VALUE
           'AREA PARA TESTE DE FILE STATUS'.
      *----------------------------------------------------------------*

       77  WRK-FS-SIGB06IN             PIC  X(002)         VALUE SPACES.
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
       77  WRK-MASCARA                 PIC  ZZZZZ.ZZZ.999 VALUE ZEROS.
       77  WRK-ERRO                    PIC  X(001)        VALUE SPACES.
       77  WRK-MSG                     PIC  X(030)        VALUE SPACES.
       01  WRK-ACU-VALOR               PIC S9(005) COMP-3 VALUE ZEROS.
       01  WRK-DANO-MES-SDO            PIC  9(006)        VALUE ZEROS.
       01  WRK-VALOR-SV                PIC S9(015) COMP-3 VALUE ZEROS.
       01  REDEFINES WRK-VALOR-SV.
           03 WRK-VALOR-CV             PIC S9(013)V99 COMP-3.
       01  WRK-S9-9                    PIC S9(009)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-S9-9.
           05  WRK-9-9                 PIC  9(009).
           05  FILLER                  REDEFINES WRK-9-9.
               07  FILLER              PIC  9(002).
               07  WRK-9-7             PIC  9(007).
           05  FILLER                  REDEFINES WRK-9-9.
               07  FILLER              PIC  9(005).
               07  WRK-9-4             PIC  9(004).
       01  WRK-X-2                     PIC  X(002)         VALUE SPACES.
       01  FILLER                      REDEFINES WRK-X-2.
           05  WRK-9-2                 PIC  9(002).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(016)        VALUE
           'AREA PARA CHAVES'.
      *----------------------------------------------------------------*

       01  WRK-CH-ANT.
           05 WRK-ANT-AGENCIA          PIC  9(005)        VALUE ZEROS.
           05 WRK-ANT-CONTA            PIC  9(007)        VALUE ZEROS.

       01  WRK-CH-ATU.
           05 WRK-ATU-AGENCIA          PIC  9(005)        VALUE ZEROS.
           05 WRK-ATU-CONTA            PIC  9(007)        VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(019)         VALUE
           ' AREA DE SIGB06IN'.
      *----------------------------------------------------------------*

       COPY 'I#SIGB06'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(019)         VALUE
           ' AREA DE REJEITAD'.
      *----------------------------------------------------------------*

       COPY 'GFCTWF26'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(019)         VALUE
           ' AREA DE GFCTB0P0'.
      *----------------------------------------------------------------*

       01  WRK-WF25-REGISTRO.
       COPY 'GFCTWF25'.

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

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA CLIB1000 ***'.
      *---------------------------------------------------------------*

       COPY 'I#CLIB20'.

       01  WRK-AREA-ENTRADA.
           03  WRK-TRANSACAO       PIC 9(05) COMP-3 VALUE 00001.
           03  WRK-VERSAO          PIC X(06)        VALUE 'VRS000'.
           03  WRK-PRODUTO         PIC 9(05) COMP-3 VALUE 00001.
           03  WRK-AREA-TRABALHO.
               05  WRK-BANCO       PIC 9(05) COMP-3 VALUE 00237.
               05  WRK-AGENCIA     PIC 9(05) COMP-3 VALUE ZEROS.
               05  WRK-CONTA       PIC 9(13) COMP-3 VALUE ZEROS.
               05  WRK-FILLER      PIC X(95)        VALUE SPACES.

       01  WRK-AREA-SAIDA.
           03  WRK-CODIGO-RETORNO  PIC 9(03) COMP-3 VALUE ZEROS.
           03  WRK-MENSAGEM-SAIDA  PIC X(998)       VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA PARA TABELA DB2'.
      *---------------------------------------------------------------*

           EXEC SQL INCLUDE SQLCA    END-EXEC.

           EXEC SQL INCLUDE GFCTB0A1 END-EXEC.

           EXEC SQL INCLUDE GFCTB009 END-EXEC.

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

           CALL 'POOL1050'.

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-VERIFICAR-VAZIO.

           PERFORM 3000-PROCESSAR      UNTIL
               WRK-FS-SIGB06IN          EQUAL '10'.

           IF (ACU-LIDOS-SIGB06IN GREATER THAN ZEROS AND
               WRK-ACU-VALOR GREATER THAN ZEROS)
              PERFORM 3600-GRAVAR-GFCTB0P0
           END-IF.

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

           OPEN INPUT  SIGB06IN
                OUTPUT REJEITAD
                       GFCTB0P0.

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

           PERFORM 1110-TESTAR-FS-SIGB06IN.

           PERFORM 1120-TESTAR-FS-REJEITAD.

           PERFORM 1130-TESTAR-FS-GFCTB0P0.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE TESTE PARA ARQUIVO SIGB06IN.
      ******************************************************************
      *----------------------------------------------------------------*
       1110-TESTAR-FS-SIGB06IN          SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SIGB06IN          NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'SIGB06IN'          TO WRK-NOME-ARQ
               MOVE WRK-FS-SIGB06IN     TO WRK-FILE-STATUS
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
      *    VERIFICAR SE ARQUIVO SIGB06IN ESTA VAZIO
      ******************************************************************
      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-LER-SIGB06IN.

           IF ACU-LIDOS-SIGB06IN EQUAL ZEROS
              GO TO 2000-99-FIM
           END-IF.

           MOVE WRK-CH-ATU             TO WRK-CH-ANT.
           INITIALIZE WRK-WF25-REGISTRO.

           MOVE 'GFCT'                 TO CSIST-PRINC OF GFCTB0A1.

           EXEC SQL SELECT DPROCM_ATUAL
                      INTO :GFCTB0A1.DPROCM-ATUAL
                      FROM DB2PRD.PARM_DATA_PROCM
                     WHERE CSIST_PRINC = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS) OR
               (SQLWARN0               EQUAL 'W')
                MOVE 'DB2'             TO ERR-TIPO-ACESSO
                MOVE 'PARM_DATA_PROCM' TO ERR-DBD-TAB
                MOVE 'SELECT'          TO ERR-FUN-COMANDO
                MOVE  SQLCODE          TO ERR-SQL-CODE
                MOVE '0001'            TO ERR-LOCAL
                MOVE  SPACES           TO ERR-SEGM
                PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA QUE EFETUA LEITURA E TRATAMENTO DO ARQUIVO DE SIGB06IN
      ******************************************************************
      *----------------------------------------------------------------*
       2100-LER-SIGB06IN                SECTION.
      *----------------------------------------------------------------*

           READ SIGB06IN INTO CONSE-REG.

           IF  WRK-FS-SIGB06IN          EQUAL '10'
               GO TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1110-TESTAR-FS-SIGB06IN.

           MOVE CONSE-AGENCIA          TO WRK-ATU-AGENCIA.
           MOVE CONSE-CONTA            TO WRK-ATU-CONTA.

           ADD 1                       TO ACU-LIDOS-SIGB06IN.

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

           IF WRK-ERRO EQUAL TO 'S'
              PERFORM 3500-GRAVAR-REJEITADOS
           ELSE
              IF WRK-CH-ATU NOT EQUAL TO WRK-CH-ANT AND
                 WRK-ACU-VALOR GREATER THAN ZEROS
                 PERFORM 3600-GRAVAR-GFCTB0P0
              END-IF
              PERFORM 3200-CALCULAR-VALORES
           END-IF.

           PERFORM 2100-LER-SIGB06IN.

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

           IF (CONSE-AGENCIA NOT NUMERIC OR
               CONSE-AGENCIA EQUAL ZEROS)
              MOVE 'S'                TO WRK-ERRO
              MOVE 'AGENCIA INVALIDA' TO WRK-MSG
              GO TO 3100-99-FIM
           END-IF.

           IF (CONSE-CONTA NOT NUMERIC OR
               CONSE-CONTA EQUAL ZEROS)
              MOVE 'S'                TO WRK-ERRO
              MOVE 'CONTA INVALIDA'   TO WRK-MSG
              GO TO 3100-99-FIM
           END-IF.

           IF WRK-CH-ATU          LESS THAN WRK-CH-ANT

              DISPLAY '************ GFCT4724 ************'
              DISPLAY '*                                *'
              DISPLAY '*        ARQUIVO SIGB06IN         *'
              DISPLAY '*        NAO CLASSIFICADO        *'
              DISPLAY '*                                *'
              DISPLAY '*       PROGRAMA ENCERRADO       *'
              DISPLAY '*                                *'
              DISPLAY '************ GFCT4701 ************'
              MOVE 'APL'              TO ERR-TIPO-ACESSO
              MOVE 'SIGB06IN'          TO WRK-NOME-ARQ
              MOVE WRK-FS-SIGB06IN     TO WRK-FILE-STATUS
              MOVE WRK-ERRO-FILE-STATUS  TO ERR-TEXTO
              PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF (CONSE-COD-CART NOT NUMERIC  OR
               CONSE-COD-CART EQUAL ZEROS)
              MOVE 'S'                 TO WRK-ERRO
              MOVE 'CARTEIRA INVALIDA' TO WRK-MSG
              GO TO 3100-99-FIM
           END-IF.

           IF (CONSE-COD-CART NOT EQUAL 370 AND
               CONSE-COD-CART NOT EQUAL 390 AND
               CONSE-COD-CART NOT EQUAL 360 AND
               CONSE-COD-CART NOT EQUAL 586 AND
               CONSE-COD-CART NOT EQUAL 587 AND
               CONSE-COD-CART NOT EQUAL 588 AND
               CONSE-COD-CART NOT EQUAL 589)
              MOVE 'S'                 TO WRK-ERRO
              MOVE 'CARTEIRA NAO CONTEMPLADA' TO WRK-MSG
              GO TO 3100-99-FIM
           END-IF.

           IF CONSE-DADO-CONS NOT NUMERIC
              MOVE 'S'                 TO WRK-ERRO
              MOVE 'VALOR INVALIDO' TO WRK-MSG
              GO TO 3100-99-FIM
           END-IF.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA PARA CALCULAR VALORES                                *
      ******************************************************************
      *----------------------------------------------------------------*
       3200-CALCULAR-VALORES           SECTION.
      *----------------------------------------------------------------*

           ADD  1                      TO WRK-ACU-VALOR.

           MOVE CONSE-DADO-CONS        TO WRK-VALOR-SV.

           EVALUATE CONSE-COD-CART
               WHEN 370
                    COMPUTE WF25-VSDO-MED-FUNDO =
                            WF25-VSDO-MED-FUNDO + WRK-VALOR-CV

               WHEN 390
                    COMPUTE WF25-VSDO-MED-POUP  =
                            WF25-VSDO-MED-POUP  + WRK-VALOR-CV
               WHEN 360
                    COMPUTE WF25-VSDO-MED-CERTF =
                            WF25-VSDO-MED-CERTF + WRK-VALOR-CV
               WHEN 586
               WHEN 587
               WHEN 588
               WHEN 589
                    COMPUTE WF25-VSDO-MED-PREVD =
                            WF25-VSDO-MED-PREVD + WRK-VALOR-CV
           END-EVALUATE.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      ******************************************************************
      *   GRAVAR REJEITADOS                                            *
      ******************************************************************
      *----------------------------------------------------------------*
       3500-GRAVAR-REJEITADOS          SECTION.
      *----------------------------------------------------------------*

           MOVE CONSE-REG              TO WF26-DADOS.
           MOVE WRK-MSG                TO WF26-MSG-ERRO.
           MOVE SPACES                 TO WF26-RESERVA.

           WRITE FD-REJEITAD           FROM WF26-REGISTRO.

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

           MOVE 237                    TO WF25-CBCO.
           MOVE WRK-ANT-AGENCIA        TO WF25-CAG-BCRIA.
           MOVE WRK-ANT-CONTA          TO WF25-CCTA-CLI.
           STRING DPROCM-ATUAL OF GFCTB0A1(7:4)
                , DPROCM-ATUAL OF GFCTB0A1(4:2)
                                       DELIMITED BY SIZE
                                     INTO WRK-DANO-MES-SDO
           END-STRING.
           MOVE WRK-DANO-MES-SDO       TO WF25-DANO-MES-SDO.

           PERFORM 3700-OBTER-TARIFA.
           PERFORM 3800-OBTER-CPF-CNPJ.

           WRITE FD-GFCTB0P0           FROM WRK-WF25-REGISTRO.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 1130-TESTAR-FS-GFCTB0P0.

           ADD 1 TO ACU-GRAVADOS-GFCTB0P0.

           MOVE 'N'                    TO WRK-ERRO.
           MOVE SPACES                 TO WRK-MSG.
           MOVE WRK-CH-ATU             TO WRK-CH-ANT.
           INITIALIZE WRK-WF25-REGISTRO.
           MOVE ZEROS TO WRK-ACU-VALOR.
      *----------------------------------------------------------------*
       3600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *     OBTER TARIFA                                               *
      ******************************************************************
      *----------------------------------------------------------------*
       3700-OBTER-TARIFA               SECTION.
      *----------------------------------------------------------------*
           MOVE WRK-ANT-AGENCIA        TO CJUNC-DEPDC       OF GFCTB009.
           MOVE WRK-ANT-CONTA          TO CCTA-CLI          OF GFCTB009.
           MOVE ZEROS                  TO CINDCD-EXCL-REG   OF GFCTB009.
           MOVE DPROCM-ATUAL OF GFCTB0A1
                                       TO DINIC-ADSAO-INDVD OF GFCTB009
                                          DFIM-ADSAO-INDVD  OF GFCTB009.

           EXEC SQL
                SELECT CSERVC_TARIF
                  INTO :GFCTB009.CSERVC-TARIF
                  FROM DB2PRD.ADSAO_INDVD_PCOTE
                 WHERE CJUNC_DEPDC        = :GFCTB009.CJUNC-DEPDC
                   AND CCTA_CLI           = :GFCTB009.CCTA-CLI
                   AND CINDCD_EXCL_REG    = :GFCTB009.CINDCD-EXCL-REG
                   AND DINIC_ADSAO_INDVD <= :GFCTB009.DINIC-ADSAO-INDVD
                   AND DFIM_ADSAO_INDVD  >= :GFCTB009.DFIM-ADSAO-INDVD
                 FETCH FIRST 1 ROW ONLY
           END-EXEC.

           IF (SQLCODE NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
              MOVE 'DB2'                TO ERR-TIPO-ACESSO
              MOVE 'ADSAO_INDVD_PCOTE'  TO ERR-DBD-TAB
              MOVE 'SELECT'             TO ERR-FUN-COMANDO
              MOVE  SQLCODE             TO ERR-SQL-CODE
              MOVE '0002'               TO ERR-LOCAL
              MOVE  SPACES              TO ERR-SEGM
              PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF SQLCODE EQUAL +100
              MOVE ZEROS                    TO WF25-CSERVC-TARIF
           ELSE
              MOVE CSERVC-TARIF OF GFCTB009 TO WF25-CSERVC-TARIF
           END-IF.

      *----------------------------------------------------------------*
       3700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    OBTER CPF/CNPJ                                              *
      ******************************************************************
      *----------------------------------------------------------------*
       3800-OBTER-CPF-CNPJ             SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-ANT-AGENCIA        TO WRK-AGENCIA.
           MOVE WRK-ANT-CONTA          TO WRK-CONTA.

           MOVE 'CLIB1000'             TO WRK-MODULO.

           CALL WRK-MODULO USING WRK-AREA-ENTRADA WRK-AREA-SAIDA.

           IF WRK-CODIGO-RETORNO NOT EQUAL ZEROS
              GO TO 3800-99-FIM
           END-IF.

           MOVE WRK-MENSAGEM-SAIDA(1:LENGTH OF WRK-AREA-RETORNO)
                                           TO WRK-AREA-RETORNO.

           IF TIPO-CONTA-C-P EQUAL 'C'
ST25X6*       MOVE NUM-CGCCPF              TO WRK-S9-9
ST25X6*       MOVE WRK-9-9                 TO WF25-CNPJ-CPF
ST25X6*       MOVE FILIAL-CGCCPF           TO WRK-S9-9
ST25X6*       MOVE WRK-9-4                 TO WF25-FILIAL
ST25X6        MOVE NUM-CGCCPF              TO WF25-CNPJ-CPF
ST25X6        MOVE FILIAL-CGCCPF           TO WF25-FILIAL
              MOVE CONTR-CGCCPF            TO WRK-X-2
              MOVE WRK-9-2                 TO WF25-DIGITO-FJ
           ELSE
              MOVE POUPADOR-CGC-CPF        TO WRK-S9-9
              MOVE WRK-9-9                 TO WF25-CNPJ-CPF
              MOVE POUPADOR-FILIAL         TO WRK-S9-9
              MOVE WRK-9-4                 TO WF25-FILIAL
              MOVE POUPADOR-CONTROLE       TO WRK-X-2
              MOVE WRK-9-2                 TO WF25-DIGITO-FJ
           END-IF.

      *----------------------------------------------------------------*
       3800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA QUE EFETUA PROCESSAMENTO FINAL DO PROGRAMA           *
      ******************************************************************
      *----------------------------------------------------------------*
       9000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 9100-EMITIR-DISPLAY.

           CLOSE SIGB06IN
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

           IF  ACU-LIDOS-SIGB06IN           NOT EQUAL ZEROS
               DISPLAY '*-----------------------------------------------
      -        '--------*'
               DISPLAY '*                   PROGRAMA GFCT4724
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
               DISPLAY '************************************************
      -        '*********'
               MOVE ACU-LIDOS-SIGB06IN      TO WRK-MASCARA
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
               DISPLAY '*               PROGRAMA GFCT4724
      -        '*'
               DISPLAY '*               -----------------
      -        '*'
               DISPLAY '*
      -        '*'
               DISPLAY '*           ARQUIVO DE SIGB06IN VAZIO
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

           MOVE 'GFCT4724'             TO ERR-PGM.

           IF  ERR-TIPO-ACESSO         EQUAL 'APL'
               CALL 'POOL7100'         USING WRK-BATCH
                                             ERRO-AREA
           ELSE
               CALL 'POOL7100'         USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA
           END-IF.

           MOVE 4                      TO RETURN-CODE.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
