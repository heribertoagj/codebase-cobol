      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. GFCT2613.
       AUTHOR.     VINICIUS CRISTIAN MADUREIRA.
      *================================================================*
      *      S O N D A  P R O C W O R K  -  C O N S U L T O R I A      *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT2613                                    *
      *    PROGRAMADOR.:   CIBELE BARBOSA              - PROCWORK/GP50 *
      *    ANALISTA....:   CIBELE BARBOSA              - PROCWORK/GP50 *
      *    DATA........:   20/04/2011                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   CONSISTENCIA FISICA E VALIDACAO DE SEGUNDA  *
      *                    LINHA DE EXTRATO DE TARIFAS.                *
      *                    CLONE DO PGM GFCT2602 COM ARQUIVO DE        *
      *                    QTDE DE REGS. REPROCESSADOS E REJEITADOS.   *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                          INCLUDE/BOOK    *
      *                     EXTRAMES                   GFCTWAEX        *
      *                     EXTRAMOK                   GFCTWADJ        *
      *                     EXTRANOK                   GFCTWADK        *
      *                     ARQTOTAL                   GFCTWAQ0        *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                         INCLUDE/BOOK    *
      *                   DB2PRD.PARM_DATA_PROCM       GFCTB0A1        *
      *                   DB2PRD.V01CTA_POUPANCA       CLIEV007        *
      *                   DB2PRD.V01CTA_CORRENTE       CLIEV008        *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    BRAD1050 - FAZ CONEXAO COM DB2.                             *
      *    BRAD7100 - AREA PARA TRATAMENTO DE ERROS.                   *
      *    BRAD0025 - SOMA E SUBTRAI DIAS EM UMA DETERMINADA DATA.     *
      *================================================================*

      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *================================================================*
       INPUT-OUTPUT                    SECTION.
      *================================================================*

       FILE-CONTROL.

           SELECT EXTRAMES ASSIGN      TO UT-S-EXTRAMES
                      FILE STATUS      IS WRK-FS-EXTRAMES.

           SELECT EXTRAMOK ASSIGN      TO UT-S-EXTRAMOK
                      FILE STATUS      IS WRK-FS-EXTRAMOK.

           SELECT EXTRANOK ASSIGN      TO UT-S-EXTRANOK
                      FILE STATUS      IS WRK-FS-EXTRANOK.

           SELECT ARQTOTAL ASSIGN      TO UT-S-ARQTOTAL
                      FILE STATUS      IS WRK-FS-ARQTOTAL.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *================================================================*
       FILE                            SECTION.
      *================================================================*

      *----------------------------------------------------------------*
      *   INPUT: REGISTRO                                              *
      *           ORG.SEQUENCIAL       - LRECL = 132                   *
      *----------------------------------------------------------------*

       FD  EXTRAMES
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-EXTRAMES             PIC X(132).

      *----------------------------------------------------------------*
      *   OUTPUT: REGISTRO                                             *
      *           ORG.SEQUENCIAL       - LRECL = 131                   *
      *----------------------------------------------------------------*

       FD  EXTRAMOK
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-EXTRAMOK             PIC X(131).

      *----------------------------------------------------------------*
      *   OUTPUT: REGISTRO                                             *
      *           ORG.SEQUENCIAL       - LRECL = 171                   *
      *----------------------------------------------------------------*

       FD  EXTRANOK
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-EXTRANOK             PIC X(171).

      *----------------------------------------------------------------*
      *   OUTPUT: REGISTRO                                             *
      *           ORG.SEQUENCIAL       - LRECL = 030                   *
      *----------------------------------------------------------------*

       FD  ARQTOTAL
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-ARQTOTAL             PIC X(030).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)          VALUE
           '*   INICIO DA WORKING GFCT2613 *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*         ACUMULADORES         *'.
      *----------------------------------------------------------------*

       01  ACU-ACUMULADORES.
           05  ACU-LIDOS-EXTRAMES      PIC  9(09)  COMP-3  VALUE ZEROS.
           05  ACU-GRAVA-EXTRAMOK      PIC  9(09)  COMP-3  VALUE ZEROS.
           05  ACU-GRAVA-EXTRANOK      PIC  9(09)  COMP-3  VALUE ZEROS.
           05  ACU-GRAVA-ARQTOTAL      PIC  9(09)  COMP-3  VALUE ZEROS.
           05  ACU-TNOK-MESANO         PIC  9(11)  COMP-3  VALUE ZEROS.
           05  ACU-TOK-MESANO          PIC  9(11)  COMP-3  VALUE ZEROS.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '* VARIAVEIS AUXILIARES         *'.
      *---------------------------------------------------------------*

       01  WRK-AUXILIARES.
           05  WRK-BATCH               PIC  X(08)          VALUE
               'BATCH'.
           05  WRK-MASCARA             PIC  ZZZ.ZZZ.ZZ9    VALUE ZEROS.
           05  WRK-DADOS-INCONS        PIC  X(01)          VALUE 'N'.

       01  WRK-DATA-DB2                PIC  X(010)         VALUE SPACES.
       01  FILLER                      REDEFINES WRK-DATA-DB2.
           05 FILLER                   PIC  X(002).
           05 FILLER                   PIC  X(001).
           05 WRK-MES-DB2              PIC  9(002).
           05 FILLER                   PIC  X(001).
           05 WRK-ANO-DB2              PIC  9(004).

       01  WRK-DATA-FDMA               PIC  9(009)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-DATA-FDMA.
           05 FILLER                   PIC  9(001).
           05 WRK-DIA-FDMA             PIC  9(002).
           05 WRK-MES-FDMA             PIC  9(002).
           05 WRK-ANO-FDMA             PIC  9(004).

       01  WRK-DATA-AM                 PIC  9(006)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-DATA-AM.
           05 WRK-ANO-AM               PIC  9(004).
           05 WRK-MES-AM               PIC  9(002).

       01  WRK-DATA-AM-ARQ             PIC  9(006)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-DATA-AM-ARQ.
           05 WRK-ANO-ARQ              PIC  9(004).
           05 WRK-MES-ARQ              PIC  9(002).

       01  WRK-VLR-LANCAMENTO         PIC 9(012)V99        VALUE ZEROS.
       01  FILLER                     REDEFINES WRK-VLR-LANCAMENTO.
           05 WRK-VALOR               PIC 9(012).
           05 WRK-CENTAVOS            PIC 9(002).

       01  WRK-CLIEV                  PIC X(001)           VALUE 'S'.


       01  WRK-CHAVE-ATU              PIC 9(006)           VALUE ZEROS.
       01  FILLER                     REDEFINES WRK-CHAVE-ATU.
           05  WRK-MES-ATU            PIC 9(002).
           05  WRK-ANO-ATU            PIC 9(004).

       01  WRK-CHAVE-ANT              PIC 9(006)           VALUE ZEROS.
       01  FILLER                     REDEFINES WRK-CHAVE-ANT.
           05  WRK-MES-ANT            PIC 9(002).
           05  WRK-ANO-ANT            PIC 9(004).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*  AREA PARA FILE-STATUS       *'.
      *----------------------------------------------------------------*

       01  WRK-AREA-FILE-STATUS.
           05  WRK-FS-EXTRAMES         PIC  X(02)          VALUE SPACES.
           05  WRK-FS-EXTRAMOK         PIC  X(02)          VALUE SPACES.
           05  WRK-FS-EXTRANOK         PIC  X(02)          VALUE SPACES.
           05  WRK-FS-ARQTOTAL         PIC  X(02)          VALUE SPACES.
           05  WRK-ABERTURA            PIC  X(13)          VALUE
               ' NA ABERTURA'.
           05  WRK-LEITURA             PIC  X(13)          VALUE
               ' NA LEITURA'.
           05  WRK-GRAVACAO            PIC  X(13)          VALUE
               ' NA GRAVACAO'.
           05  WRK-FECHAMENTO          PIC  X(13)          VALUE
               'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*  MENSAGENS DE ERRO           *'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-FS.
           05  FILLER                  PIC  X(07)          VALUE SPACES.
           05  FILLER                  PIC  X(07)          VALUE
               '* ERRO '.
           05  WRK-OPERACAO            PIC  X(13)          VALUE SPACES.
           05  FILLER                  PIC  X(12)          VALUE
               ' DO ARQUIVO '.
           05  WRK-NOME-ARQ            PIC  X(08)          VALUE SPACES.
           05  FILLER                  PIC  X(17)          VALUE
               ' - FILE-STATUS = '.
           05  WRK-FS                  PIC  X(02)          VALUE SPACES.
           05  FILLER                  PIC  X(02)          VALUE ' *'.
           05  FILLER                  PIC  X(07)          VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*        AREA BRAD0025         *'.
      *----------------------------------------------------------------*

       01  WRK-0025-DATA-ENTRADA       PIC  9(009) COMP-3  VALUE ZEROS.
       01  WRK-0025-DATA-SAIDA         PIC  9(009) COMP-3  VALUE ZEROS.
       01  WRK-0025-NUMERO-DIAS        PIC S9(005) COMP-3  VALUE ZEROS.

       01  WRK-MSG0025                 PIC  X(040)         VALUE
           'ERRO NO ACESSO AO MODULO BRAD0025'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*       AREA PARA BRAD7100     *'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*       AREA PARA EXTRAMES     *'.
      *----------------------------------------------------------------*

       COPY 'GFCTWAEX'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*       AREA PARA EXTRAMOK     *'.
      *----------------------------------------------------------------*

       COPY 'GFCTWADJ'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*       AREA PARA EXTRANOK     *'.
      *----------------------------------------------------------------*

       COPY 'GFCTWADK'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*       AREA PARA ARQTOTAL     *'.
      *----------------------------------------------------------------*

       COPY 'GFCTWAQ0'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*         AREA PARA DB2        *'.
      *----------------------------------------------------------------*

           EXEC SQL
             INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
             INCLUDE CLIEV007
           END-EXEC.

           EXEC SQL
             INCLUDE CLIEV008
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0A1
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    FIM DA WORKING GFCT2613  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE SECTION.
      *----------------------------------------------------------------*

       01  LNK-PARM.
           05  FILLER                  PIC S9(04) COMP.
           05  LNK-MES                 PIC  9(01).

      *===============================================================*
       PROCEDURE DIVISION USING LNK-PARM.
      *===============================================================*

      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           CALL 'BRAD1050'.

           PERFORM 0050-CONSISTIR-PARM.

           PERFORM 0100-INICIALIZAR-PROGRAMA.

           PERFORM 0400-BUSCAR-DATA-PROCM

           PERFORM 0300-VERIFICAR-VAZIO.

           PERFORM 0600-PROCESSAR      UNTIL
                       WRK-FS-EXTRAMES EQUAL '10'.

           PERFORM 1200-FINALIZAR-PROGRAMA.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0050-CONSISTIR-PARM            SECTION.
      *----------------------------------------------------------------*

           IF LNK-MES             NOT EQUAL 0 AND 1
               DISPLAY '************** GFCT2613 *************'
               DISPLAY '*  PARAMETRO INVALIDO: ' LNK-MES
                                               '            *'
               DISPLAY '*        DADOS INCONSISTENTES       *'
               DISPLAY '************** GFCT2613 *************'
               MOVE  'APL'                  TO  ERR-TIPO-ACESSO
               MOVE  'PARM'                 TO  WRK-NOME-ARQ
               MOVE  'INCONSISTENTE'        TO  ERR-TEXTO
               PERFORM  9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0050-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0100-INICIALIZAR-PROGRAMA       SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT   EXTRAMES
                OUTPUT  EXTRAMOK
                        EXTRANOK
                        ARQTOTAL.

           MOVE  WRK-ABERTURA          TO WRK-OPERACAO.

           PERFORM 0200-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       0100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0200-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 0210-TESTAR-FS-EXTRAMES.

           PERFORM 0220-TESTAR-FS-EXTRAMOK.

           PERFORM 0230-TESTAR-FS-EXTRANOK.

           PERFORM 0240-TESTAR-FS-ARQTOTAL.

      *----------------------------------------------------------------*
       0200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0210-TESTAR-FS-EXTRAMES         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-EXTRAMES         NOT EQUAL '00'
               MOVE 'EXTRAMES'         TO WRK-NOME-ARQ
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-EXTRAMES    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0220-TESTAR-FS-EXTRAMOK         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-EXTRAMOK         NOT EQUAL '00'
               MOVE 'EXTRAMOK'         TO WRK-NOME-ARQ
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-EXTRAMOK    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0230-TESTAR-FS-EXTRANOK         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-EXTRANOK         NOT EQUAL '00'
               MOVE 'EXTRANOK'         TO WRK-NOME-ARQ
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-EXTRANOK    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0240-TESTAR-FS-ARQTOTAL         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ARQTOTAL         NOT EQUAL '00'
               MOVE 'ARQTOTAL'         TO WRK-NOME-ARQ
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-ARQTOTAL    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0240-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0300-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 0350-LER-EXTRAMES.

           IF  WRK-FS-EXTRAMES        EQUAL '10'
               DISPLAY '*********** GFCT2613 ***********'
               DISPLAY '*                              *'
               DISPLAY '*    ARQUIVO EXTRAMES VAZIO    *'
               DISPLAY '*    PROCESSAMENTO ENCERRADO   *'
               DISPLAY '*                              *'
               DISPLAY '*********** GFCT2613 ***********'
           END-IF.

      *----------------------------------------------------------------*
       0300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0350-LER-EXTRAMES               SECTION.
      *----------------------------------------------------------------*

           READ EXTRAMES  INTO WAEX-EXTRAMES-HEADER.

           IF  WRK-FS-EXTRAMES         EQUAL '10'
               GO                      TO 0350-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 0210-TESTAR-FS-EXTRAMES.

           ADD 1                       TO ACU-LIDOS-EXTRAMES.

           MOVE  WAEX-TIPO-REGISTRO    TO WADJ-TIPO-REGISTRO
                                          WADK-TIPO-REGISTRO.
           MOVE  WAEX-COD-AGENCIA      TO WADJ-COD-AGENCIA
                                          WADK-COD-AGENCIA.
           MOVE  WAEX-CPF-CNPJ         TO WADJ-CPF-CNPJ
                                          WADK-CPF-CNPJ.
           MOVE  WAEX-RAZAO            TO WADJ-RAZAO
                                          WADK-RAZAO.
           MOVE  WAEX-NRO-CONTA        TO WADJ-NRO-CONTA
                                          WADK-NRO-CONTA.
           MOVE  WAEX-NUMERO-DOCUMENTO TO WADJ-NUMERO-DOCUMENTO
                                          WADK-NUMERO-DOCUMENTO.
           MOVE  WAEX-COD-LANCAMENTO   TO WADJ-COD-LANCAMENTO
                                          WADK-COD-LANCAMENTO.
           MOVE  WAEX-DATA-LANCAMENTO  TO WADJ-DATA-LANCAMENTO
                                          WADK-DATA-LANCAMENTO.
           MOVE  WAEX-DESC-LANCAMENTO  TO WADJ-DESC-LANCAMENTO
                                          WADK-DESC-LANCAMENTO.

           MOVE  WAEX-VALOR            TO WRK-VALOR.
           MOVE  WAEX-CENTAVOS         TO WRK-CENTAVOS.

           MOVE  WRK-VLR-LANCAMENTO    TO WADK-VLR-LANCAMENTO
                                          WADJ-VLR-LANCAMENTO.
           MOVE  WAEX-HIST-CPL         TO WADJ-HIST-CPL
                                          WADK-HIST-CPL.

           IF ACU-LIDOS-EXTRAMES  EQUAL 1
              MOVE WAEX-DT-LANCTO-AAAA   TO WRK-ANO-ANT
                                            WRK-ANO-ATU
              MOVE WAEX-DT-LANCTO-MM     TO WRK-MES-ANT
                                            WRK-MES-ATU
           ELSE
              MOVE WRK-CHAVE-ATU       TO WRK-CHAVE-ANT
              MOVE WAEX-DT-LANCTO-MM   TO WRK-MES-ATU
              MOVE WAEX-DT-LANCTO-AAAA TO WRK-ANO-ATU
           END-IF.

           MOVE WAEX-DT-LANCTO-MM      TO WRK-MES-ARQ.

           MOVE WAEX-DT-LANCTO-AAAA    TO WRK-ANO-ARQ.

           MOVE 'N'                    TO WRK-DADOS-INCONS.

           PERFORM 0360-CONSISTIR-DADOS.

      *----------------------------------------------------------------*
       0350-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0360-CONSISTIR-DADOS            SECTION.
      *----------------------------------------------------------------*

           IF WRK-CHAVE-ATU            NOT EQUAL WRK-CHAVE-ANT
              MOVE WRK-MES-ANT         TO  WAQ0-MES-REPROC
              MOVE WRK-ANO-ANT         TO  WAQ0-ANO-REPROC
              MOVE ACU-TNOK-MESANO     TO  WAQ0-QTDE-REJEIT
              MOVE ACU-TOK-MESANO      TO  WAQ0-QTDE-REPROC
              PERFORM 0760-GRAVAR-ARQTOTAL
              MOVE ZEROS               TO  ACU-TOK-MESANO
                                           ACU-TNOK-MESANO
           END-IF.

           IF WADJ-TIPO-REGISTRO           EQUAL SPACES
               MOVE 'TIPO REGISTRO EM BRANCO'      TO WADK-DESC-ERRO
               PERFORM 0750-GRAVAR-SAIDA-NAOK
               MOVE 'S'             TO WRK-DADOS-INCONS
           ELSE
               IF WADJ-TIPO-REGISTRO       NOT EQUAL 'T' AND 'E'
                   MOVE 'TIPO REGISTRO DIFERENTE DE T/E'
                                                   TO WADK-DESC-ERRO
                   PERFORM 0750-GRAVAR-SAIDA-NAOK
                   MOVE 'S'         TO WRK-DADOS-INCONS
               ELSE
                   IF WADJ-COD-AGENCIA     NOT NUMERIC
                       MOVE 'AGENCIA NAO NUMERICA' TO WADK-DESC-ERRO
                       PERFORM 0750-GRAVAR-SAIDA-NAOK
                       MOVE 'S'     TO WRK-DADOS-INCONS
                   ELSE
                       IF WADJ-COD-AGENCIA EQUAL ZEROS
                           MOVE 'AGENCIA ZERADA'   TO WADK-DESC-ERRO
                           PERFORM 0750-GRAVAR-SAIDA-NAOK
                           MOVE 'S' TO WRK-DADOS-INCONS
                       END-IF
                   END-IF
               END-IF
           END-IF.

           IF WRK-DADOS-INCONS NOT EQUAL 'S'
      * ST25X6* Remover checagem NOT NUMERIC para CPF/CNPJ; consistir BRANCO/ZEROS
               IF WADJ-CPF-CNPJ             EQUAL SPACES
                   MOVE 'CPF/CNPJ EM BRANCO' TO WADK-DESC-ERRO
                   PERFORM 0750-GRAVAR-SAIDA-NAOK
                   MOVE 'S'     TO WRK-DADOS-INCONS
               ELSE
                   IF WADJ-CPF-CNPJ         EQUAL ZEROS
                       MOVE 'CPF/CNPJ ZERADO'   TO WADK-DESC-ERRO
                       PERFORM 0750-GRAVAR-SAIDA-NAOK
                       MOVE 'S' TO WRK-DADOS-INCONS
                   ELSE
                       IF WADJ-NRO-CONTA            NOT NUMERIC
                           MOVE 'CONTA NAO NUMERICA' TO WADK-DESC-ERRO
                           PERFORM 0750-GRAVAR-SAIDA-NAOK
                           MOVE 'S'     TO WRK-DADOS-INCONS
                       END-IF
                   END-IF
               END-IF
           END-IF.

           IF WRK-DADOS-INCONS NOT EQUAL 'S'
               IF WADJ-COD-LANCAMENTO       EQUAL ZEROS
                   MOVE 'CODIGO LANCAMENTO ZERADO' TO WADK-DESC-ERRO
                   PERFORM 0750-GRAVAR-SAIDA-NAOK
                   MOVE 'S'     TO WRK-DADOS-INCONS
               ELSE
                   IF WADJ-COD-LANCAMENTO   NOT NUMERIC
                       MOVE 'CODIGO LANCAMENTO NAO NUMERICO'
                                                   TO WADK-DESC-ERRO
                       PERFORM 0750-GRAVAR-SAIDA-NAOK
                       MOVE 'S' TO WRK-DADOS-INCONS
                   ELSE
                       IF WADJ-DATA-LANCAMENTO      NOT NUMERIC
                           MOVE 'DATA LANCAMENTO NAO NUMERICA'
                                                    TO WADK-DESC-ERRO
                           PERFORM 0750-GRAVAR-SAIDA-NAOK
                           MOVE 'S' TO WRK-DADOS-INCONS
                       END-IF
                   END-IF
               END-IF
           END-IF.

           IF WRK-DADOS-INCONS NOT EQUAL 'S'
               IF WADJ-RAZAO      EQUAL ZEROS
                   MOVE 'RAZAO ZERADA' TO WADK-DESC-ERRO
                   PERFORM 0750-GRAVAR-SAIDA-NAOK
                   MOVE 'S' TO WRK-DADOS-INCONS
               ELSE
                   IF WADJ-HIST-CPL         EQUAL SPACES AND
                      WADJ-DESC-LANCAMENTO  EQUAL SPACES
                       MOVE 'DESC LCTO E SGDA LIN EXTRATO EM BRANCO'
                                           TO WADK-DESC-ERRO
                       PERFORM 0750-GRAVAR-SAIDA-NAOK
                       MOVE 'S' TO WRK-DADOS-INCONS
                   END-IF
               END-IF
           END-IF.

           IF WRK-DADOS-INCONS NOT EQUAL 'S'
               IF WADJ-NRO-CONTA        EQUAL ZEROS
                   MOVE 'CONTA ZERADA'   TO WADK-DESC-ERRO
                   PERFORM 0750-GRAVAR-SAIDA-NAOK
                   MOVE 'S' TO WRK-DADOS-INCONS
               ELSE
                   IF WADJ-DATA-LANCAMENTO  EQUAL ZEROS
                       MOVE 'DATA LANCAMENTO ZERADA'
                                               TO WADK-DESC-ERRO
                       PERFORM 0750-GRAVAR-SAIDA-NAOK
                       MOVE 'S' TO WRK-DADOS-INCONS
                   END-IF
                END-IF
           END-IF.

           IF WRK-DADOS-INCONS NOT EQUAL 'S'
               IF WRK-VLR-LANCAMENTO   NOT NUMERIC
                   MOVE 'VLR LANCAMENTO NAO NUMERICO'
                                           TO WADK-DESC-ERRO
                   PERFORM 0750-GRAVAR-SAIDA-NAOK
                   MOVE 'S' TO WRK-DADOS-INCONS
               ELSE
                   IF WRK-VLR-LANCAMENTO       EQUAL ZEROS
                       MOVE 'VLR LANCAMENTO ZERADO'
                                   TO WADK-DESC-ERRO
                       PERFORM 0750-GRAVAR-SAIDA-NAOK
                       MOVE 'S' TO WRK-DADOS-INCONS
                   END-IF
               END-IF
           END-IF.

           IF WRK-DADOS-INCONS NOT EQUAL 'S'
               IF WADJ-NUMERO-DOCUMENTO      EQUAL ZEROS
                   MOVE 'NRO DOCUMENTO ZERADO' TO WADK-DESC-ERRO
                   PERFORM 0750-GRAVAR-SAIDA-NAOK
                   MOVE 'S' TO WRK-DADOS-INCONS
               ELSE
                   IF WADJ-NUMERO-DOCUMENTO   NOT NUMERIC
                       MOVE 'NRO DOCUMENTO NAO NUMERICO'
                                           TO WADK-DESC-ERRO
                       PERFORM 0750-GRAVAR-SAIDA-NAOK
                       MOVE 'S' TO WRK-DADOS-INCONS
                   END-IF
               END-IF
           END-IF.

           IF WRK-DADOS-INCONS NOT EQUAL 'S'
               IF WADJ-RAZAO     NOT NUMERIC
                   MOVE 'RAZAO NAO NUMERICA' TO WADK-DESC-ERRO
                   PERFORM 0750-GRAVAR-SAIDA-NAOK
                   MOVE 'S' TO WRK-DADOS-INCONS
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       0360-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0400-BUSCAR-DATA-PROCM             SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC      OF GFCTB0A1.

           EXEC SQL
             SELECT
                   DPROCM_ANTER,
                   DPROCM_ATUAL,
                   DPROCM_PROX
             INTO
                   :GFCTB0A1.DPROCM-ANTER,
                   :GFCTB0A1.DPROCM-ATUAL,
                   :GFCTB0A1.DPROCM-PROX
             FROM   DB2PRD.PARM_DATA_PROCM
             WHERE
                   CSIST_PRINC         = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS) OR
               (SQLWARN0               EQUAL 'W')
                MOVE 'DB2'             TO ERR-TIPO-ACESSO
                MOVE 'PARM_DATA_PROCM' TO ERR-DBD-TAB
                MOVE 'SELECT'          TO ERR-FUN-COMANDO
                MOVE  SQLCODE          TO ERR-SQL-CODE
                MOVE '0005'            TO ERR-LOCAL
                MOVE  SPACES           TO ERR-SEGM
                PERFORM 9999-ROTINA-ERRO
           END-IF.

           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO WRK-DATA-DB2.
           MOVE 01                     TO WRK-DIA-FDMA.
           MOVE WRK-MES-DB2            TO WRK-MES-FDMA.
           MOVE WRK-ANO-DB2            TO WRK-ANO-FDMA.

           MOVE WRK-DATA-FDMA          TO WRK-0025-DATA-ENTRADA.
           MOVE -1                     TO WRK-0025-NUMERO-DIAS.

           CALL 'BRAD0025'             USING WRK-0025-DATA-ENTRADA
                                             WRK-0025-NUMERO-DIAS
                                             WRK-0025-DATA-SAIDA.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MSG0025        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           MOVE WRK-0025-DATA-SAIDA    TO WRK-DATA-FDMA.
           MOVE WRK-MES-FDMA           TO WRK-MES-AM.
           MOVE WRK-ANO-FDMA           TO WRK-ANO-AM.

      *----------------------------------------------------------------*
       0400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0600-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           IF WRK-DADOS-INCONS NOT EQUAL 'S'
               IF LNK-MES EQUAL 0
                   PERFORM 0610-ACESSAR-CLIEV007
                   PERFORM 0620-ACESSAR-CLIEV008
               ELSE
                   IF WRK-DATA-AM EQUAL WRK-DATA-AM-ARQ
                       PERFORM 0610-ACESSAR-CLIEV007
                       PERFORM 0620-ACESSAR-CLIEV008
                   ELSE
                       MOVE 'AAAAMM REG. DIFER. AAAAMM REFERENCIA'
                                               TO WADK-DESC-ERRO
                       PERFORM 0750-GRAVAR-SAIDA-NAOK
                   END-IF
               END-IF
           END-IF.

           PERFORM 0350-LER-EXTRAMES.

      *----------------------------------------------------------------*
       0600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0610-ACESSAR-CLIEV007           SECTION.
      *----------------------------------------------------------------*

           MOVE WADJ-COD-AGENCIA TO CJUNC-DEPDC OF CLIEV007.
           MOVE WADJ-NRO-CONTA   TO CCTA-CLI    OF CLIEV007.

           EXEC SQL
             SELECT
                   CJUNC_DEPDC,
                   CCTA_CLI
             INTO
                   :CLIEV007.CJUNC-DEPDC,
                   :CLIEV007.CCTA-CLI
             FROM   DB2PRD.V01CTA_POUPANCA
             WHERE
                   CJUNC_DEPDC = :CLIEV007.CJUNC-DEPDC
             AND   CCTA_CLI    = :CLIEV007.CCTA-CLI
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
                MOVE 'DB2'             TO ERR-TIPO-ACESSO
                MOVE 'V01CTA_POUPANCA' TO ERR-DBD-TAB
                MOVE 'SELECT'          TO ERR-FUN-COMANDO
                MOVE  SQLCODE          TO ERR-SQL-CODE
                MOVE '0010'            TO ERR-LOCAL
                MOVE  SPACES           TO ERR-SEGM
                PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'N'                        TO WRK-CLIEV
           ELSE
               MOVE 'S'                        TO WRK-CLIEV
           END-IF.

      *----------------------------------------------------------------*
       0610-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0620-ACESSAR-CLIEV008            SECTION.
      *----------------------------------------------------------------*

           MOVE WADJ-COD-AGENCIA TO CJUNC-DEPDC OF CLIEV008.
           MOVE WADJ-NRO-CONTA   TO CCTA-CLI    OF CLIEV008.

           EXEC SQL
             SELECT
                   CJUNC_DEPDC,
                   CCTA_CLI
             INTO
                   :CLIEV008.CJUNC-DEPDC,
                   :CLIEV008.CCTA-CLI
             FROM   DB2PRD.V01CTA_CORRENTE
             WHERE
                   CJUNC_DEPDC = :CLIEV008.CJUNC-DEPDC
             AND   CCTA_CLI    = :CLIEV008.CCTA-CLI
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
                MOVE 'DB2'             TO ERR-TIPO-ACESSO
                MOVE 'V01CTA_CORRENTE' TO ERR-DBD-TAB
                MOVE 'SELECT'          TO ERR-FUN-COMANDO
                MOVE  SQLCODE          TO ERR-SQL-CODE
                MOVE '0015'            TO ERR-LOCAL
                MOVE  SPACES           TO ERR-SEGM
                PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  WRK-CLIEV EQUAL 'N' AND SQLCODE EQUAL +100
               MOVE 'AG/CTA NAO ENCONTRADO NO CLIEV7 E CLIEV8'
                                               TO WADK-DESC-ERRO
               PERFORM 0750-GRAVAR-SAIDA-NAOK
           ELSE
               IF  WRK-CLIEV EQUAL 'S' AND SQLCODE EQUAL +100
                   PERFORM 0740-GRAVAR-SAIDA-OK
               ELSE
                   IF  WRK-CLIEV EQUAL 'S' AND SQLCODE EQUAL ZEROS
                       PERFORM 0740-GRAVAR-SAIDA-OK
                   ELSE
                       IF  WRK-CLIEV EQUAL 'N' AND SQLCODE EQUAL ZEROS
                           PERFORM 0740-GRAVAR-SAIDA-OK
                       END-IF
                   END-IF
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       0620-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0740-GRAVAR-SAIDA-OK            SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                     GFCTWADK-REG.

           WRITE FD-REG-EXTRAMOK   FROM   GFCTWADJ-REG.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 0220-TESTAR-FS-EXTRAMOK.

           ADD 1                       TO ACU-GRAVA-EXTRAMOK
                                          ACU-TOK-MESANO.

      *----------------------------------------------------------------*
       0740-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0750-GRAVAR-SAIDA-NAOK            SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                     GFCTWADJ-REG.

           WRITE FD-REG-EXTRANOK FROM    GFCTWADK-REG.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 0230-TESTAR-FS-EXTRANOK.

           ADD 1                       TO ACU-GRAVA-EXTRANOK
                                          ACU-TNOK-MESANO.

      *----------------------------------------------------------------*
       0750-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0760-GRAVAR-ARQTOTAL            SECTION.
      *----------------------------------------------------------------*

           WRITE FD-REG-ARQTOTAL  FROM    GFCTWAQ0-REG.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 0240-TESTAR-FS-ARQTOTAL.

           ADD 1                       TO ACU-GRAVA-ARQTOTAL.

      *----------------------------------------------------------------*
       0760-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA DE FINALIZACAO DO PROGRAMA                           *
      *----------------------------------------------------------------*
       1200-FINALIZAR-PROGRAMA         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MES-ANT         TO  WAQ0-MES-REPROC
           MOVE WRK-ANO-ANT         TO  WAQ0-ANO-REPROC
           MOVE ACU-TNOK-MESANO     TO  WAQ0-QTDE-REJEIT
           MOVE ACU-TOK-MESANO      TO  WAQ0-QTDE-REPROC

           PERFORM 0760-GRAVAR-ARQTOTAL.

           PERFORM 1300-EMITIR-DISPLAY.

           CLOSE EXTRAMES
                 EXTRAMOK
                 EXTRANOK
                 ARQTOTAL.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 0200-TESTAR-FILE-STATUS.

           STOP RUN.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA DE EMISSAO DE TOTAIS DE CONTROLE                     *
      *----------------------------------------------------------------*
       1300-EMITIR-DISPLAY             SECTION.
      *----------------------------------------------------------------*

               DISPLAY '*-----------------------------------------------
      -        '--------*'
               DISPLAY '*                   PROGRAMA GFCT2613
      -        '        *'
               DISPLAY '*                   -----------------
      -        '        *'
               DISPLAY '*
      -        '        *'
               DISPLAY '*          PROCESSAMENTO  EFETUADO COM SUCESSO
      -        '        *'
               DISPLAY '*-----------------------------------------------
      -        '--------*'
               DISPLAY '************************************************
      -        '*********'
               MOVE ACU-LIDOS-EXTRAMES   TO WRK-MASCARA
               DISPLAY '* TOTAL DE REGISTROS LIDOS              = '
                 WRK-MASCARA
                              '   *'
               MOVE ACU-GRAVA-EXTRAMOK   TO WRK-MASCARA
               DISPLAY '* TOTAL DE REGISTROS OK GRAVADOS        = '
                 WRK-MASCARA
                              '   *'
               MOVE ACU-GRAVA-EXTRANOK   TO WRK-MASCARA
               DISPLAY '* TOTAL DE REGISTROS NAO OK GRAVADOS    = '
                 WRK-MASCARA
                              '   *'
               MOVE ACU-GRAVA-ARQTOTAL   TO WRK-MASCARA
               DISPLAY '* TOTAL DE REGISTROS TOTAIS GRAVADOS    = '
                 WRK-MASCARA
                              '   *'
               DISPLAY '************************************************
      -        '*********'.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           IF  ERR-TIPO-ACESSO         NOT EQUAL 'DB2'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
           END-IF.

           MOVE 'GFCT2613'             TO ERR-PGM.

           CALL 'BRAD7100'             USING WRK-BATCH
                                             ERRO-AREA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
