      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID.                     GFCT2123.
       AUTHOR.                         MARCEL QUEIROZ.
      *================================================================*
      *                       C A P G E M I N I                        *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT2123                                    *
      *    PROGRAMADOR.:   MARCEL QUEIROZ          - SALVADOR          *
      *    ANALISTA....:   RODRIGO JOVANELI        - ALPHAVILE         *
      *    DATA........:   13/08/2019                                  *
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   INCLUSAO DE SOLICITACAO DE PERMISSAO PARA   *
      *                    ADESAO (CLIENTE)                            *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                        INCLUDE/BOOK     *
      *                   DB2PRD.PARM_DATA_PROCM        GFCTB0A1       *
      *                   DB2PRD.SERVC_TARIF_PRINC      GFCTB0D8       *
      *                   DB2PRD.TPRMSS_CLI             GFCTB0I0       *
      *                   DB2PRD.PRMSS_ADSAO_PCOT       GFCTB0C6       *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    GFCTWK7C - LAYOUT DO ARQUIVO DE ENTRADA EARQWK7C            *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    POOL0110 - CONSISTIR CLIENTE (CPF)                          *
      *    CALE2000 - VALIDACAO DE DATAS                               *
      *================================================================*
      *                    A L T E R A C A O                           *
      *----------------------------------------------------------------*
      *
      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*
      *
      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*
      *
       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.
      *
      *----------------------------------------------------------------*
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *
           SELECT EARQWK7C ASSIGN      TO UT-S-EARQWK7C
               FILE STATUS             IS WRK-FS-EARQWK7C.
      *
           SELECT SWK7INCO ASSIGN      TO UT-S-SWK7INCO
               FILE STATUS             IS WRK-FS-SWK7INCO.
      *
           SELECT SWK7CONS ASSIGN      TO UT-S-SWK7CONS
               FILE STATUS             IS WRK-FS-SWK7CONS.
      *
      *================================================================*
       DATA                            DIVISION.
      *================================================================*
      *
      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    INPUT:  ARQUIVO DE ENTRADA EARQWK7C                         *
      *            ORG. SEQUENCIAL  -  LRECL = 057                     *
      *----------------------------------------------------------------*
      *
       FD  EARQWK7C
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           LABEL RECORD IS STANDARD.
      *
       01  FD-EARQWK7C                 PIC X(057).
      *
      *----------------------------------------------------------------*
      *    INPUT:  ARQUIVO DE SAIDA CONSISTENTES (INSERIDOS)           *
      *            ORG. SEQUENCIAL  -  LRECL = 057                     *
      *----------------------------------------------------------------*
      *
       FD  SWK7CONS
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           LABEL RECORD IS STANDARD.
      *
       01  FD-SWK7CONS                 PIC X(057).
      *
      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE SAIDA INCONSISTENTES                     *
      *            ORG. SEQUENCIAL  -  LRECL = 107                     *
      *----------------------------------------------------------------*
      *
       FD  SWK7INCO
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           LABEL RECORD IS STANDARD.
      *
       01  FD-ENTRADA.
           03 FD-DADOS-RST.
              05 FD-FUNC-BDSCO                 PIC  X(007).
           03 FD-FIXO.
              05 FD-CPACOTE                    PIC  9(005).
              05 FD-CAGPTO                     PIC  9(003).
              05 FD-TOTAL                      PIC  X(001).
ST2506*        05 FD-CGC-CPF                    PIC  9(009).
ST2506*        05 FD-FILIAL                     PIC  9(005).
ST2506         05 FD-CGC-CPF                    PIC  X(009).
ST2506         05 FD-FILIAL                     PIC  X(004).
              05 FD-CONTROLE                   PIC  X(002).
              05 FD-DATA-INICIO                PIC  X(010).
              05 FD-DATA-FIM                   PIC  X(010).
              05 FD-TIPO-ADESAO                PIC  X(001).
              05 FD-SOLICITA-EXCECAO           PIC  9(001).
              05 FD-PRMSS-INDVD                PIC  9(001).
              05 FD-CPSSOA-TARIFA              PIC  X(001).
              05 FD-VISUAL-COMPULS             PIC  9(001).
           03 FD-MENSAGEM                      PIC  X(050).
      *
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* INICIO DA AREA DE WORKING *'.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA TO VARIAVEIS AUXILIARES *'.
      *----------------------------------------------------------------*
      *
       77  WRK-FIM                     PIC  X(001)         VALUE SPACES.
       77  WRK-ACHOU-DUPLICIDADE       PIC  X(001)         VALUE SPACES.
       77  WRK-ACHOU-FUTURO            PIC  X(001)         VALUE SPACES.
       77  WRK-SEQUENCIA               PIC  9(009)         COMP-3
                                                           VALUE ZEROS.
       77  WRK-LOCAL                   PIC  X(004)         VALUE SPACES.
       77  WRK-CHEKA                   PIC  X(001)         VALUE SPACES.

       77  WRK-INCONSISTENTE           PIC  X(001)         VALUE SPACES.
       77  WRK-PROGRAMA                PIC  X(008)         VALUE
                                                             'GFCT2123'.
       77  WRK-MENSAGEM                PIC  X(050)         VALUE SPACES.
       77  WRK-MASCARA-SQLCODE         PIC  ZZZ.ZZZ.ZZ9-   VALUE ZEROS.
       77  WRK-FRWK2999                PIC  X(008)         VALUE
                                                             'FRWK2999'.
       77  WRK-MASCARA                 PIC  ZZZ.ZZZ.ZZ9    VALUE ZEROS.
       77  WRK-CKRS0100                PIC  X(008)         VALUE
                                                             'CKRS0100'.
       77  WRK-CALE2000                PIC  X(008)         VALUE
                                                             'CALE2000'.
      *
       01  WRK-DATA-FIM.
           05  WRK-ANO-FIM             PIC  9(004)         VALUE ZEROS.
           05  WRK-MES-FIM             PIC  9(002)         VALUE ZEROS.
           05  WRK-DIA-FIM             PIC  9(002)         VALUE ZEROS.
      *
       01  WRK-DATA-INICIO.
           05  WRK-ANO-INI             PIC  9(004)         VALUE ZEROS.
           05  WRK-MES-INI             PIC  9(002)         VALUE ZEROS.
           05  WRK-DIA-INI             PIC  9(002)         VALUE ZEROS.
      *
       01  WRK-DATA-PROCM.
           05  WRK-ANO-PROCM           PIC  9(004)         VALUE ZEROS.
           05  WRK-MES-PROCM           PIC  9(002)         VALUE ZEROS.
           05  WRK-DIA-PROCM           PIC  9(002)         VALUE ZEROS.
      *
       01  WRK-S9-09                   PIC +9(009)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-S9-09.
           05 FILLER                   PIC  X(001).
           05 WRK-9-09                 PIC  9(009).
      *
       01  WRK-CCTRL-CPF-CGC-X.
           05 WRK-CCTRL-CPF-CGC-9      PIC  9(002)         VALUE ZEROS.
      *
       01  WRK-DATA-10.
           05 WRK-DIA                  PIC  9(002)         VALUE ZEROS.
           05 FILLER                   PIC  X(001)         VALUE '.'.
           05 WRK-MES                  PIC  9(002)         VALUE ZEROS.
           05 FILLER                   PIC  X(001)         VALUE '.'.
           05 WRK-ANO                  PIC  9(004)         VALUE ZEROS.
      *
       01  WRK-MSG-INCONSISTENTE.
           05 WRK-MENSAGEM-1           PIC  X(006)         VALUE
                                                               'CAMPO '.
           05 WRK-CAMPO                PIC  X(020)         VALUE SPACES.
           05 WRK-MESAGEM-2            PIC  X(014)         VALUE
                                                       ' INCONSISTENTE'.
           05 FILLER                   PIC  X(010)         VALUE SPACES.
      *
       01  WRK-MSG-DATA-INCO.
           05 WRK-MSG-1                PIC  X(006)         VALUE
                                                               'CAMPO '.
           05 WRK-CAMPO-DATA           PIC  X(011)         VALUE SPACES.
           05 WRK-MSG-2                PIC  X(033)          VALUE
                                    ' INCONSISTENTE NO MODULO CALE2000'.
      *
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           'AREA PARA ACUMULADORES'.
      *----------------------------------------------------------------*
      *
       01  WRK-AREA-RESTART.
           05  ACU-LIDOS-EARQWK7C      PIC  9(009) COMP-3  VALUE ZEROS.
           05  ACU-GRAVA-SWK7INCO      PIC  9(009) COMP-3  VALUE ZEROS.
           05  ACU-GRAVA-SWK7CONS      PIC  9(009) COMP-3  VALUE ZEROS.
           05  ACU-INSERIDOS-B0C6      PIC  9(009) COMP-3  VALUE ZEROS.
           05  ACU-INSERIDOS-B0I0      PIC  9(009) COMP-3  VALUE ZEROS.
      *
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA TESTE DE FILE-STATUS *'.
      *----------------------------------------------------------------*
      *
       77  WRK-FS-EARQWK7C             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-SWK7CONS             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-SWK7INCO             PIC  X(002)         VALUE SPACES.
      *
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050) VALUE
           '* AREA LAYOUT ARQUIVO EARQWK7C/CONS *'.
      *----------------------------------------------------------------*
      *
           COPY 'GFCTWK7C'.
      *
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA P/ FORMATACAO DE ERRO DE MODULO ***'.
      *----------------------------------------------------------------*
      *
       01  WRK-AREA-ERRO-MODULO.
           COPY 'I#FRWKMD'.
      *
      *----------------------------------------------------------------*
       77  FILLER                      PIC X(050)  VALUE
           'AREA DE TRATAMENTO DE ERRO DB2 (API-ERRO)'.
      *----------------------------------------------------------------*
      *
       01  WRK-AREA-ERRO-DB2.
           COPY 'I#FRWKDB'.
      *
      *----------------------------------------------------------------*
       77  FILLER                      PIC X(050)      VALUE
           '* AREA DE ACESSO AO CKRS0100 *'.
      *----------------------------------------------------------------*
      *                                                                *
       COPY 'I#CKRS01'.
      *
      *----------------------------------------------------------------*
       77  FILLER                      PIC X(050)  VALUE
           'AREA DE COMUNICACAO DO FRWK1999 (LOG DE ERRO)'.
      *----------------------------------------------------------------*
      *
       01  WRK-AREA-ERRO.
           COPY 'I#FRWKGE'.
           05 WRK-BLOCO-INFO-ERRO.
             10 WRK-CHAR-INFO-ERRO     PIC X(01) OCCURS 0 TO 600 TIMES
                                       DEPENDING ON FRWKGHEA-TAM-DADOS.
      *
ST2506*----------------------------------------------------------------*
ST2506*77  FILLER                      PIC  X(050)         VALUE
ST2506*    '*** AREAS DA POOL0110 ***'.
ST2506*----------------------------------------------------------------*
ST2506*
ST2506*01  WRK-CGC-CPF-0110.
ST2506*    03  WRK-NUMERO-0110         PIC  9(009)         VALUE ZEROS.
ST2506*    03  WRK-FILIAL-0110         PIC  9(005)         VALUE ZEROS.
ST2506*01  WRK-CONTROLE-0110           PIC  X(002)         VALUE SPACES.

ST2506*----------------------------------------------------------------*
ST2506 77  FILLER                      PIC  X(050)         VALUE
ST2506     '*** AREAS DA BRAD2000 ***'.
ST2506*----------------------------------------------------------------*
ST2506
ST2506 01  WRK-BRAD2000                 PIC  X(08)  VALUE
ST2506     'BRAD2000'.
ST2506 
ST2506 01 WRK-AREA-BRAD2000.
ST2506    05 WRK-CAD-CPFCGC.
ST2506       10 WRK-CAD-NUMERO         PIC X(09)           VALUE SPACES.
ST2506       10 WRK-CAD-FILIAL         PIC X(04)           VALUE SPACES.
ST2506 77  WRK-CAD-CONTROLE            PIC 9(02)           VALUE ZEROS.
ST2506 01  WRK-9-X-2.
ST2506     05 WRK-9-02                 PIC 9(02)           VALUE ZEROS.
      *
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           'AREA DE TRATAMENTO DE ERROS DE ARQUIVOS'.
      *----------------------------------------------------------------*
      *
       01  WRK-FRWKGARQ.
           COPY 'I#FRWKAR'.
      *
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA COMUNICACAO API BRAD0450 *'.
      *----------------------------------------------------------------*
      *
       01  WRK-ABEND                   PIC  X(001)         VALUE SPACES.
           88  WRK-ABENDAR                                 VALUE 'S'.
      *
       01  WRK-ABEND-0450              PIC S9(004) COMP    VALUE +1234.
       01  WRK-DUMP                    PIC  X(001)         VALUE 'S'.
      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA PARA CONSITIR DATA'.
      *----------------------------------------------------------------*
      *
       COPY 'I#CALE01'.
      *
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA TO TABELA DB2 *'.
      *----------------------------------------------------------------*
      *
           EXEC SQL
             INCLUDE SQLCA
           END-EXEC.
      *
      *    -> PRMSS_ADSAO_PCOTE
      *
           EXEC SQL
             INCLUDE GFCTB0C6
           END-EXEC.
      *
      *    -> SERVC_TARIF_PRINC
      *
           EXEC SQL
             INCLUDE GFCTB0D8
           END-EXEC.
      *
      *    -> PARM_DATA_PROCM
      *
           EXEC SQL
             INCLUDE GFCTB0A1
           END-EXEC.
      *
      *    -> TPRMSS_GRP_CLI
      *
           EXEC SQL
             INCLUDE GFCTB0I0
           END-EXEC.
      *
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA TO CURSORES *'.
      *----------------------------------------------------------------*
      *
           EXEC SQL
             DECLARE   CSR01-GFCTB0C6   CURSOR WITH HOLD FOR
               SELECT
                   CSERVC_TARIF,
                   CAGPTO_CTA,
                   DINIC_PRMSS_PCOTE,
                   CSEQ_AGPTO_CTA,
                   CINDCD_ADSAO_INDVD,
                   CINDCD_VSLAO_COMP,
                   CPSSOA_SERVC_TARIF
             FROM  DB2PRD.PRMSS_ADSAO_PCOTE
             WHERE
                   CAGPTO_CTA         = :GFCTB0C6.CAGPTO-CTA         AND
                   CSERVC_TARIF       = :GFCTB0C6.CSERVC-TARIF       AND
                   DFIM_PRMSS_PCOTE   > :GFCTB0C6.DFIM-PRMSS-PCOTE
           END-EXEC.
      *
           EXEC SQL
             DECLARE   CSR02-GFCTB0C6   CURSOR WITH HOLD FOR
               SELECT
                   CSERVC_TARIF,
                   CAGPTO_CTA,
                   DINIC_PRMSS_PCOTE,
                   CSEQ_AGPTO_CTA,
                   CINDCD_ADSAO_INDVD,
                   CINDCD_VSLAO_COMP,
                   CPSSOA_SERVC_TARIF
             FROM  DB2PRD.PRMSS_ADSAO_PCOTE
             WHERE
                   CAGPTO_CTA         = :GFCTB0C6.CAGPTO-CTA         AND
                   CSERVC_TARIF       = :GFCTB0C6.CSERVC-TARIF       AND
                   DINIC_PRMSS_PCOTE  > :GFCTB0C6.DINIC-PRMSS-PCOTE
           END-EXEC.
      *
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* FIM DA AREA DE WORKING *'.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*
      *
      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*
      *
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*
      *
           PERFORM 1000-INICIALIZAR
      *
           PERFORM 2000-PROCESSAR      UNTIL WRK-FS-EARQWK7C
                                       EQUAL '10'
      *
           PERFORM 3000-FINALIZAR
      *
           .
      *
      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*
      *
           INITIALIZE                  FRWKGHEA-REGISTRO
                                       FRWKGARQ-REGISTRO
                                       FRWKGDB2-REGISTRO
                                       FRWKGMOD-REGISTRO
      *
           CALL 'CKRS1000'
      *
           PERFORM 7000-CKRS-INICIALIZAR
      *
           OPEN INPUT                  EARQWK7C
               OUTPUT                  SWK7INCO
                                       SWK7CONS
      *
           SET ARQ-OPEN                TO TRUE
      *
           PERFORM 1100-TESTAR-FILE-STATUS
      *
           PERFORM 1200-VERIFICAR-VAZIO
      *
           .
      *
      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    ROTINA PARA TESTAR FILE STATUS DOS ARQUIVOS                 *
      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*
      *
           PERFORM 1110-TESTAR-FS-EARQWK7C
           PERFORM 1120-TESTAR-FS-SWK7INCO
           PERFORM 1130-TESTAR-FS-SWK7CONS
      *
           .
      *
      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    ROTINA PARA TESTAR FILE STATUS DO ARQUIVO EARQWK7C          *
      *----------------------------------------------------------------*
       1110-TESTAR-FS-EARQWK7C         SECTION.
      *----------------------------------------------------------------*
      *
           IF  WRK-FS-EARQWK7C         NOT EQUAL '00' AND '10'
               MOVE WRK-FS-EARQWK7C    TO FRWKGARQ-FILE-STATUS
               MOVE 'EARQWK7C'         TO FRWKGARQ-NOME-ARQUIVO
               SET WRK-ABENDAR         TO TRUE
               PERFORM 9100-ERRO-ARQ
           END-IF
      *
           .
      *
      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    ROTINA PARA TESTAR FILE STATUS DO ARQUIVO SWK7INCO          *
      *----------------------------------------------------------------*
       1120-TESTAR-FS-SWK7INCO         SECTION.
      *----------------------------------------------------------------*
      *
           IF  WRK-FS-SWK7INCO         NOT EQUAL '00'
               MOVE WRK-FS-SWK7INCO    TO FRWKGARQ-FILE-STATUS
               MOVE 'SWK7INCO'         TO FRWKGARQ-NOME-ARQUIVO
               SET WRK-ABENDAR         TO TRUE
               PERFORM 9100-ERRO-ARQ
           END-IF
      *
           .
      *
      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    ROTINA PARA TESTAR FILE STATUS DO ARQUIVO SWK7CONS          *
      *----------------------------------------------------------------*
       1130-TESTAR-FS-SWK7CONS         SECTION.
      *----------------------------------------------------------------*
      *
           IF  WRK-FS-SWK7CONS         NOT EQUAL '00'
               MOVE WRK-FS-SWK7CONS    TO FRWKGARQ-FILE-STATUS
               MOVE 'SWK7CONS'         TO FRWKGARQ-NOME-ARQUIVO
               SET WRK-ABENDAR         TO TRUE
               PERFORM 9100-ERRO-ARQ
           END-IF
      *
           .
      *
      *----------------------------------------------------------------*
       1130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    VERIFICA SE O ARQUIVO DE ENTRADA EARQWK7C ESTA VAZIO        *
      *----------------------------------------------------------------*
       1200-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*
      *
           PERFORM 1210-LER-EARQWK7C
      *
           IF  WRK-FS-EARQWK7C         EQUAL '10'
               DISPLAY '************* GFCT2123 *************'
               DISPLAY '*                                  *'
               DISPLAY '*      ARQUIVO EARQWK7C VAZIO      *'
               DISPLAY '*                                  *'
               DISPLAY '*      PROCESSAMENTO ENCERRADO     *'
               DISPLAY '*                                  *'
               DISPLAY '************* GFCT2123 *************'
               PERFORM 3000-FINALIZAR
           END-IF
      *
           .
      *
      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    LEITURA DO ARQUIVO DE ENTRADA EARQWK7C                      *
      *----------------------------------------------------------------*
       1210-LER-EARQWK7C               SECTION.
      *----------------------------------------------------------------*
      *
           INITIALIZE                  FD-EARQWK7C
      *
           MOVE '0001'                 TO WRK-LOCAL
           MOVE '2010-LER-EARQWK7C'    TO FRWKGHEA-IDEN-PARAGRAFO
      *
           READ EARQWK7C               INTO GFCTK7-ENTRADA

      *
           SET ARQ-READ                TO TRUE
      *
           PERFORM 1110-TESTAR-FS-EARQWK7C
      *
           IF  WRK-FS-EARQWK7C         EQUAL '10'
               GO                      TO 1210-99-FIM
           END-IF
      *
           ADD 1                       TO ACU-LIDOS-EARQWK7C
      *
           .
      *
      *----------------------------------------------------------------*
       1210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*
      *
           PERFORM 2100-CONSISTIR-DADOS
      *
           IF WRK-INCONSISTENTE        EQUAL 'N'
      *
              PERFORM 2200-CONSISTIR-CLIENTE
      *
              PERFORM 2300-CONSISTIR-DATA
      *
              PERFORM 2400-VER-NO-PACOTE

           IF WRK-CHEKA                 EQUAL 'S'
              PERFORM 2500-VERIFICAR-DUPLICIDADE
           END-IF
      *
              PERFORM 2600-INSERIR
      *
           END-IF
      *
           PERFORM 7200-CKRS-CHECKPOINT
      *
           PERFORM 1210-LER-EARQWK7C
      *
           .
      *
      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       2100-CONSISTIR-DADOS            SECTION.
      *----------------------------------------------------------------*
      *
           MOVE 'N'                    TO WRK-INCONSISTENTE
      *
           IF GFCTK7-CGC-CPF-R         NOT NUMERIC
              MOVE 'CGC-CPF'           TO WRK-CAMPO
              MOVE WRK-MSG-INCONSISTENTE
                                       TO WRK-MENSAGEM
              PERFORM 3100-GRAVAR-SWK7INCO
           END-IF.
      *
           IF GFCTK7-FILIAL-R          NOT NUMERIC
              MOVE 'FILIAL'            TO WRK-CAMPO
              MOVE WRK-MSG-INCONSISTENTE
                                       TO WRK-MENSAGEM
              PERFORM 3100-GRAVAR-SWK7INCO
           END-IF.
      *
           IF GFCTK7-CONTROLE          EQUAL LOW-VALUES
              MOVE 'CGC-CPF'           TO WRK-CAMPO
              MOVE WRK-MSG-INCONSISTENTE
                                       TO WRK-MENSAGEM
              PERFORM 3100-GRAVAR-SWK7INCO
           END-IF
      *
           IF GFCTK7-CPACOTE-R          NOT NUMERIC
              MOVE 'CPACOTE'           TO WRK-CAMPO
              MOVE WRK-MSG-INCONSISTENTE
                                       TO WRK-MENSAGEM
              PERFORM 3100-GRAVAR-SWK7INCO
           END-IF.
      *
           IF GFCTK7-CAGPTO-R          NOT NUMERIC
              MOVE 'CAGPTO'            TO WRK-CAMPO
              MOVE WRK-MSG-INCONSISTENTE
                                       TO WRK-MENSAGEM
              PERFORM 3100-GRAVAR-SWK7INCO
           END-IF.
      *
           IF GFCTK7-TIPO-ADESAO       EQUAL LOW-VALUES
              MOVE 'TIPO-ADESAO'       TO WRK-CAMPO
              MOVE WRK-MSG-INCONSISTENTE
                                       TO WRK-MENSAGEM
              PERFORM 3100-GRAVAR-SWK7INCO
           END-IF.
      *
           IF GFCTK7-VISUAL-COMPULS-R  NOT NUMERIC
              MOVE 'VISUAL-COMPULS'    TO WRK-CAMPO
              MOVE WRK-MSG-INCONSISTENTE
                                       TO WRK-MENSAGEM
              PERFORM 3100-GRAVAR-SWK7INCO
           END-IF.
      *
           IF GFCTK7-SOLICITA-EXCECAO-R
                                       NOT NUMERIC
              MOVE 'SOLICITA-EXCECAO'  TO WRK-CAMPO
              MOVE WRK-MSG-INCONSISTENTE
                                       TO WRK-MENSAGEM
              PERFORM 3100-GRAVAR-SWK7INCO
           END-IF.
      *
           IF GFCTK7-PRMSS-INDVD-R     NOT NUMERIC
              MOVE 'PRMSS-INDVD'       TO WRK-CAMPO
              MOVE WRK-MSG-INCONSISTENTE
                                       TO WRK-MENSAGEM
              PERFORM 3100-GRAVAR-SWK7INCO
           END-IF

           IF GFCTK7-DATA-INICIO       EQUAL LOW-VALUES
              MOVE 'DATA-INICIO'       TO WRK-CAMPO
              MOVE WRK-MSG-INCONSISTENTE
                                       TO WRK-MENSAGEM
              PERFORM 3100-GRAVAR-SWK7INCO
           ELSE
              MOVE GFCTK7-DATA-INICIO  TO WRK-DATA-10
              MOVE 'DATA-INICIO'       TO WRK-CAMPO-DATA
              PERFORM 7400-CONSISTIR-DATA
           END-IF.
      *
           IF GFCTK7-DATA-FIM          EQUAL LOW-VALUES
              MOVE 'DATA-FIM'          TO WRK-CAMPO
              MOVE WRK-MSG-INCONSISTENTE
                                       TO WRK-MENSAGEM
              PERFORM 3100-GRAVAR-SWK7INCO
           ELSE
              MOVE GFCTK7-DATA-FIM     TO WRK-DATA-10
              MOVE 'DATA-FIM'          TO WRK-CAMPO-DATA
              PERFORM 7400-CONSISTIR-DATA
           END-IF.

           IF GFCTK7-CPSSOA-TARIFA     EQUAL LOW-VALUES
              MOVE 'CPSSOA-TARIFA'     TO WRK-CAMPO
              MOVE WRK-MSG-INCONSISTENTE
                                       TO WRK-MENSAGEM
              PERFORM 3100-GRAVAR-SWK7INCO
           END-IF.
      *
      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       2200-CONSISTIR-CLIENTE          SECTION.
      *----------------------------------------------------------------*
      *
ST2506*    INITIALIZE                  WRK-CGC-CPF-0110
ST2506*                                WRK-CONTROLE-0110
ST2506*
ST2506*    MOVE SPACES                 TO WRK-CONTROLE-0110
ST2506*    MOVE GFCTK7-CGC-CPF         TO WRK-NUMERO-0110
ST2506*    MOVE GFCTK7-FILIAL          TO WRK-FILIAL-0110
ST2506*
ST2506*    CALL 'POOL0110'             USING WRK-CGC-CPF-0110
ST2506*                                      WRK-CONTROLE-0110

ST2506     MOVE ZEROS                 TO WRK-CAD-CONTROLE. 
ST2506     MOVE GFCTK7-CGC-CPF        TO WRK-CAD-NUMERO.
ST2506     MOVE GFCTK7-FILIAL         TO WRK-CAD-FILIAL.
ST2506
ST2506     CALL WRK-BRAD2000          USING WRK-CAD-CPFCGC
ST2506                                       WRK-CAD-CONTROLE.
      *
           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'CGC/CNPJ INVALIDO'
                                       TO WRK-MENSAGEM
               PERFORM 3100-GRAVAR-SWK7INCO
           END-IF
      *
ST2506*    IF  GFCTK7-CONTROLE         NOT EQUAL WRK-CONTROLE-0110
ST2506     MOVE  WRK-CAD-CONTROLE      TO WRK-9-02.
ST2506     IF  GFCTK7-CONTROLE         NOT EQUAL WRK-9-X-2 
               MOVE 'CONTROLE DO CGC-CPF INFORMADO NAO CONFERE'
                                       TO WRK-MENSAGEM
               PERFORM 3100-GRAVAR-SWK7INCO
           END-IF
      *
           .
      *
      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       2300-CONSISTIR-DATA             SECTION.
      *----------------------------------------------------------------*
      *
           IF WRK-INCONSISTENTE        EQUAL 'S'
              GO                       TO 2300-99-FIM
           END-IF
      *
           PERFORM 2310-OBTER-DATA-PROCM
      *
           STRING GFCTK7-DATA-INICIO(07:04)
                  GFCTK7-DATA-INICIO(04:02)
                  GFCTK7-DATA-INICIO(01:02)
           DELIMITED BY SIZE           INTO WRK-DATA-INICIO
      *
           STRING GFCTK7-DATA-FIM(07:04)
                  GFCTK7-DATA-FIM(04:02)
                  GFCTK7-DATA-FIM(01:02)
           DELIMITED BY SIZE           INTO WRK-DATA-FIM
      *
           STRING DPROCM-ATUAL         OF GFCTB0A1(07:04)
                  DPROCM-ATUAL         OF GFCTB0A1(04:02)
                  DPROCM-ATUAL         OF GFCTB0A1(01:02)
           DELIMITED BY SIZE           INTO WRK-DATA-PROCM
      *        01.01.2015              01.01.2014

           IF  WRK-DATA-FIM            LESS WRK-DATA-INICIO
             MOVE 'DATA INI DA PERMSS DEVE SER MAIOR QUE DATA ATUAL'
                                       TO WRK-MENSAGEM
               PERFORM 3100-GRAVAR-SWK7INCO
           END-IF
      *
           .
      *
      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       2310-OBTER-DATA-PROCM           SECTION.
      *----------------------------------------------------------------*
      *
           INITIALIZE                  GFCTB0A1
      *
           MOVE 'GFCT'                 TO CSIST-PRINC
                                       OF GFCTB0A1
      *
           EXEC SQL
             SELECT
                   DPROCM_ATUAL,
                   DPROCM_PROX
             INTO
                   :GFCTB0A1.DPROCM-ATUAL,
                   :GFCTB0A1.DPROCM-PROX
             FROM   DB2PRD.PARM_DATA_PROCM
             WHERE
                   CSIST_PRINC        = :GFCTB0A1.CSIST-PRINC
           END-EXEC
      *
           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               SET DB2-SELECT          TO TRUE
               SET WRK-ABENDAR         TO TRUE
               MOVE 'PARM_DATA_PROCM'  TO FRWKGDB2-NOME-TABELA
               MOVE SQLSTATE           TO FRWKGDB2-SQLSTATE
               MOVE SQLCA              TO FRWKGDB2-SQLCA
               MOVE SPACES             TO FRWKGDB2-STORED-PROC
               MOVE '010'              TO FRWKGDB2-LOCAL
               PERFORM 9000-ERRO-DB2
           END-IF
      *
           .
      *
      *----------------------------------------------------------------*
       2310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       2400-VER-NO-PACOTE              SECTION.
      *----------------------------------------------------------------*
      *
           IF WRK-INCONSISTENTE        EQUAL 'S'
              GO                       TO 2400-99-FIM
           END-IF
      *
           MOVE GFCTK7-CPACOTE         TO CSERVC-TARIF
                                       OF GFCTB0D8
      *
           EXEC SQL
             SELECT CPSSOA_SERVC_TARIF
             INTO  :GFCTB0D8.CPSSOA-SERVC-TARIF
             FROM   DB2PRD.SERVC_TARIF_PRINC
             WHERE  CSERVC_TARIF = :GFCTB0D8.CSERVC-TARIF
           END-EXEC
      *
           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               SET DB2-SELECT          TO TRUE
               SET WRK-ABENDAR         TO TRUE
               MOVE 'SERVC_TARIF_PRINC'
                                       TO FRWKGDB2-NOME-TABELA
               MOVE SQLSTATE           TO FRWKGDB2-SQLSTATE
               MOVE SQLCA              TO FRWKGDB2-SQLCA
               MOVE SPACES             TO FRWKGDB2-STORED-PROC
               MOVE '020'              TO FRWKGDB2-LOCAL
               PERFORM 9000-ERRO-DB2
           END-IF


           IF  SQLCODE                 EQUAL +100
              MOVE 'TARIFA INVALIDA '  TO WRK-MENSAGEM
              PERFORM 3100-GRAVAR-SWK7INCO
           END-IF

      *
           IF  CPSSOA-SERVC-TARIF      OF GFCTB0D8
                                       EQUAL '1'
           AND GFCTK7-FILIAL           NOT EQUAL ZEROS
              MOVE 'NAO PERMITIDO INCLUIR CNPJ PARA PACOTE DE PF'
                                      TO WRK-MENSAGEM
              PERFORM 3100-GRAVAR-SWK7INCO
           END-IF
      *
           IF  CPSSOA-SERVC-TARIF      OF GFCTB0D8
                                       EQUAL '2'
           AND GFCTK7-FILIAL           EQUAL ZEROS
              MOVE 'NAO PERMITIDO INCLUIR CNPJ PARA PACOTE DE PJ'
                                      TO WRK-MENSAGEM
              PERFORM 3100-GRAVAR-SWK7INCO
           END-IF
      *
           .
      *
      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       2500-VERIFICAR-DUPLICIDADE      SECTION.
      *----------------------------------------------------------------*
      *
           IF WRK-INCONSISTENTE        EQUAL 'S'
              GO                       TO 2500-99-FIM
           END-IF
      *
           PERFORM 2520-ABRIR-CSR01
      *
           PERFORM 2540-LER-CSR01
      *
           PERFORM 2560-PROCESSAR-CSR01
                                       UNTIL
              WRK-FIM                  EQUAL 'S' OR
              WRK-ACHOU-DUPLICIDADE    EQUAL 'S'
      *
           PERFORM 2580-FECHAR-CSR01
      *
           IF WRK-ACHOU-DUPLICIDADE    EQUAL 'S'
              MOVE 'JA EXISTE PERMISSAO PARA CHAVE SOLICITADA'
                                       TO WRK-MENSAGEM
              PERFORM 3100-GRAVAR-SWK7INCO
           END-IF
      *
           .
      *
      *----------------------------------------------------------------*
       2500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       2520-ABRIR-CSR01                SECTION.
      *----------------------------------------------------------------*
      *
           MOVE 'N'                    TO WRK-FIM
           MOVE 'N'                    TO WRK-ACHOU-DUPLICIDADE
           MOVE GFCTK7-CPACOTE         TO CSERVC-TARIF
                                       OF GFCTB0C6
           MOVE GFCTK7-CAGPTO          TO CAGPTO-CTA
                                       OF GFCTB0C6
           MOVE GFCTK7-DATA-INICIO     TO DFIM-PRMSS-PCOTE
                                       OF GFCTB0C6
      *
           EXEC SQL
               OPEN CSR01-GFCTB0C6
           END-EXEC
      *
           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               SET DB2-OPEN            TO TRUE
               SET WRK-ABENDAR         TO TRUE
               MOVE 'PRMSS_ADSAO_PCOTE'
                                       TO FRWKGDB2-NOME-TABELA
               MOVE SQLSTATE           TO FRWKGDB2-SQLSTATE
               MOVE SQLCA              TO FRWKGDB2-SQLCA
               MOVE SPACES             TO FRWKGDB2-STORED-PROC
               MOVE '030'              TO FRWKGDB2-LOCAL
               PERFORM 9000-ERRO-DB2
           END-IF
      *
           .
      *
      *----------------------------------------------------------------*
       2520-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       2540-LER-CSR01                  SECTION.
      *----------------------------------------------------------------*
      *
           EXEC SQL
               FETCH CSR01-GFCTB0C6
               INTO :GFCTB0C6.CSERVC-TARIF,
                    :GFCTB0C6.CAGPTO-CTA,
                    :GFCTB0C6.DINIC-PRMSS-PCOTE,
                    :GFCTB0C6.CSEQ-AGPTO-CTA,
                    :GFCTB0C6.CINDCD-ADSAO-INDVD,
                    :GFCTB0C6.CINDCD-VSLAO-COMP,
                    :GFCTB0C6.CPSSOA-SERVC-TARIF
           END-EXEC
      *
           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               SET DB2-FETCH           TO TRUE
               SET WRK-ABENDAR         TO TRUE
               MOVE 'PRMSS_ADSAO_PCOTE'
                                       TO FRWKGDB2-NOME-TABELA
               MOVE SQLSTATE           TO FRWKGDB2-SQLSTATE
               MOVE SQLCA              TO FRWKGDB2-SQLCA
               MOVE SPACES             TO FRWKGDB2-STORED-PROC
               MOVE '040'              TO FRWKGDB2-LOCAL
               PERFORM 9000-ERRO-DB2
           END-IF
      *
           IF  SQLCODE                  EQUAL +100
               MOVE 'S'                 TO WRK-FIM
           END-IF
      *
           .
      *
      *----------------------------------------------------------------*
       2540-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       2560-PROCESSAR-CSR01            SECTION.
      *----------------------------------------------------------------*
      *
           PERFORM 2565-VERIFICAR-CLIENTE
      *
           PERFORM 2540-LER-CSR01
      *
           .
      *
      *----------------------------------------------------------------*
       2560-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       2565-VERIFICAR-CLIENTE          SECTION.
      *----------------------------------------------------------------*
      *
           MOVE CAGPTO-CTA             OF GFCTB0C6
                                       TO CAGPTO-CTA
                                       OF GFCTB0I0
      *
           MOVE CSERVC-TARIF           OF GFCTB0C6
                                       TO CSERVC-TARIF
                                       OF GFCTB0I0
      *
           MOVE CSEQ-AGPTO-CTA         OF GFCTB0C6
                                       TO CSEQ-AGPTO-CTA
                                       OF GFCTB0I0
      *
           MOVE DINIC-PRMSS-PCOTE      OF GFCTB0C6
                                       TO DINIC-PRMSS-PCOTE
                                       OF GFCTB0I0
      *
           MOVE GFCTK7-CGC-CPF         TO CCGC-CPF
                                       OF GFCTB0I0
      *
           MOVE GFCTK7-FILIAL          TO CFLIAL-CGC
                                       OF GFCTB0I0
      *
           MOVE GFCTK7-CONTROLE        TO WRK-CCTRL-CPF-CGC-X
           MOVE WRK-CCTRL-CPF-CGC-9    TO CCTRL-CPF-CGC
                                       OF GFCTB0I0
      *
           EXEC SQL
               SELECT  CCGC_CPF,
                       CFLIAL_CGC,
                       CCTRL_CPF_CGC
               INTO   :GFCTB0I0.CCGC-CPF,
                      :GFCTB0I0.CFLIAL-CGC,
                      :GFCTB0I0.CCTRL-CPF-CGC
               FROM    DB2PRD.TPRMSS_GRP_CLI
               WHERE   CAGPTO_CTA         = :GFCTB0I0.CAGPTO-CTA
                 AND   CSERVC_TARIF       = :GFCTB0I0.CSERVC-TARIF
                 AND   CSEQ_AGPTO_CTA     = :GFCTB0I0.CSEQ-AGPTO-CTA
                 AND   DINIC_PRMSS_PCOTE  = :GFCTB0I0.DINIC-PRMSS-PCOTE
ST25X6*          AND   CCGC_CPF           = :GFCTB0I0.CCGC-CPF
ST25X6*          AND   CFLIAL_CGC         = :GFCTB0I0.CFLIAL-CGC
ST25X6*          AND   CCTRL_CPF_CGC      = :GFCTB0I0.CCTRL-CPF-CGC
ST25X6           AND   CCGC_CPF_ST        = :GFCTB0I0.CCGC-CPF-ST
ST25X6           AND   CFLIAL_CGC_ST      = :GFCTB0I0.CFLIAL-CGC-ST
ST25X6           AND   CCTRL_CPF_CGC_ST   = :GFCTB0I0.CCTRL-CPF-CGC-ST
           END-EXEC
      *
           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               SET DB2-SELECT          TO TRUE
               SET WRK-ABENDAR         TO TRUE
               MOVE 'TPRMSS_GRP_CLI'   TO FRWKGDB2-NOME-TABELA
               MOVE SQLSTATE           TO FRWKGDB2-SQLSTATE
               MOVE SQLCA              TO FRWKGDB2-SQLCA
               MOVE SPACES             TO FRWKGDB2-STORED-PROC
               MOVE '050'              TO FRWKGDB2-LOCAL
               PERFORM 9000-ERRO-DB2
           END-IF
      *
           IF  SQLCODE                 EQUAL ZEROS
               PERFORM 2566-VERIFICA-DUPLICADO
           END-IF
      *
           .
      *
      *----------------------------------------------------------------*
       2565-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       2566-VERIFICA-DUPLICADO         SECTION.
      *----------------------------------------------------------------*
      *
           IF GFCTK7-TIPO-ADESAO       EQUAL  'I'
           OR GFCTK7-TIPO-ADESAO       EQUAL  'A'
              MOVE 'S'                 TO WRK-ACHOU-DUPLICIDADE
           ELSE
              IF CINDCD-ADSAO-INDVD    OF GFCTB0C6
                                       EQUAL 'I'
              OR CINDCD-ADSAO-INDVD    OF GFCTB0C6
                                       EQUAL 'A'
                 MOVE 'S'              TO WRK-ACHOU-DUPLICIDADE
              ELSE
                  IF (CPSSOA-SERVC-TARIF
                                       OF GFCTB0C6
                                       EQUAL '3')
                  OR (GFCTK7-CPSSOA-TARIFA
                                       EQUAL '3')
                      MOVE 'S'         TO WRK-ACHOU-DUPLICIDADE
                  ELSE
                      IF GFCTK7-CPSSOA-TARIFA
                                       EQUAL CPSSOA-SERVC-TARIF
                                       OF GFCTB0C6
                         MOVE 'S'      TO WRK-ACHOU-DUPLICIDADE
                      END-IF
                  END-IF
              END-IF
           END-IF
      *
           .
      *
      *----------------------------------------------------------------*
       2566-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       2580-FECHAR-CSR01              SECTION.
      *----------------------------------------------------------------*
      *
           EXEC SQL
               CLOSE CSR01-GFCTB0C6
           END-EXEC
      *
           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               SET DB2-CLOSE           TO TRUE
               SET WRK-ABENDAR         TO TRUE
               MOVE 'PRMSS_ADSAO_PCOTE'
                                       TO FRWKGDB2-NOME-TABELA
               MOVE SQLSTATE           TO FRWKGDB2-SQLSTATE
               MOVE SQLCA              TO FRWKGDB2-SQLCA
               MOVE SPACES             TO FRWKGDB2-STORED-PROC
               MOVE '060'              TO FRWKGDB2-LOCAL
               PERFORM 9000-ERRO-DB2
           END-IF
      *
           .
      *
      *----------------------------------------------------------------*
       2580-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       2600-INSERIR                    SECTION.
      *----------------------------------------------------------------*
      *
           IF WRK-INCONSISTENTE        EQUAL 'S'
              GO                       TO 2600-99-FIM
           END-IF
      *

           IF WRK-CHEKA                 EQUAL 'S'
             PERFORM 2630-VE-FUTURO
           END-IF.
      *
           PERFORM 2660-INSERIR-C6
      *
           PERFORM 2690-GERAR-CLIENTE
      *
           PERFORM 3200-GRAVAR-SWK7CONS
      *
           .
      *
      *----------------------------------------------------------------*
       2600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       2630-VE-FUTURO                  SECTION.
      *----------------------------------------------------------------*
      *
           PERFORM 2635-ABRIR-CSR02
      *
           PERFORM 2640-LER-CSR02
      *
           PERFORM 2645-PROCESSAR-CSR02
                                       UNTIL
              WRK-FIM                  EQUAL 'S' OR
              WRK-ACHOU-FUTURO         EQUAL 'S'
      *
           PERFORM 2650-FECHAR-CSR02
      *
           IF  WRK-ACHOU-FUTURO        EQUAL 'S'
               MOVE 'JA EXISTE PERMISSAO FUTURA PARA ESTA CHAVE'
                                       TO WRK-MENSAGEM
               PERFORM 3100-GRAVAR-SWK7INCO
           END-IF
      *
           .
      *
      *----------------------------------------------------------------*
       2630-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       2635-ABRIR-CSR02                SECTION.
      *----------------------------------------------------------------*
      *
           MOVE 'N'                    TO WRK-FIM
           MOVE 'N'                    TO WRK-ACHOU-FUTURO
           MOVE GFCTK7-CPACOTE         TO CSERVC-TARIF
                                       OF GFCTB0C6
           MOVE GFCTK7-CAGPTO          TO CAGPTO-CTA
                                       OF GFCTB0C6
           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO DINIC-PRMSS-PCOTE
                                       OF GFCTB0C6
      *
           EXEC SQL
               OPEN CSR02-GFCTB0C6
           END-EXEC
      *
           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               SET DB2-OPEN            TO TRUE
               SET WRK-ABENDAR         TO TRUE
               MOVE 'PRMSS_ADSAO_PCOTE'
                                       TO FRWKGDB2-NOME-TABELA
               MOVE SQLSTATE           TO FRWKGDB2-SQLSTATE
               MOVE SQLCA              TO FRWKGDB2-SQLCA
               MOVE SPACES             TO FRWKGDB2-STORED-PROC
               MOVE '070'              TO FRWKGDB2-LOCAL
               PERFORM 9000-ERRO-DB2
           END-IF
      *
           .
      *
      *----------------------------------------------------------------*
       2635-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       2640-LER-CSR02                  SECTION.
      *----------------------------------------------------------------*
      *
           EXEC SQL
               FETCH CSR02-GFCTB0C6
               INTO :GFCTB0C6.CSERVC-TARIF,
                    :GFCTB0C6.CAGPTO-CTA,
                    :GFCTB0C6.DINIC-PRMSS-PCOTE,
                    :GFCTB0C6.CSEQ-AGPTO-CTA,
                    :GFCTB0C6.CINDCD-ADSAO-INDVD,
                    :GFCTB0C6.CINDCD-VSLAO-COMP,
                    :GFCTB0C6.CPSSOA-SERVC-TARIF
           END-EXEC
      *
           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               SET DB2-FETCH           TO TRUE
               SET WRK-ABENDAR         TO TRUE
               MOVE 'PRMSS_ADSAO_PCOTE'
                                       TO FRWKGDB2-NOME-TABELA
               MOVE SQLSTATE           TO FRWKGDB2-SQLSTATE
               MOVE SQLCA              TO FRWKGDB2-SQLCA
               MOVE SPACES             TO FRWKGDB2-STORED-PROC
               MOVE '080'              TO FRWKGDB2-LOCAL
               PERFORM 9000-ERRO-DB2
           END-IF
      *
           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM
           END-IF
      *
           .
      *
      *----------------------------------------------------------------*
       2640-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       2645-PROCESSAR-CSR02            SECTION.
      *----------------------------------------------------------------*
      *
           PERFORM 2646-VERIFICAR-CLIENTE
      *
           PERFORM 2640-LER-CSR02
      *
           .
      *
      *----------------------------------------------------------------*
       2645-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       2646-VERIFICAR-CLIENTE          SECTION.
      *----------------------------------------------------------------*
      *
           MOVE CAGPTO-CTA             OF GFCTB0C6
                                       TO CAGPTO-CTA
                                       OF GFCTB0I0
      *
           MOVE CSERVC-TARIF           OF GFCTB0C6
                                       TO CSERVC-TARIF
                                       OF GFCTB0I0
      *
           MOVE CSEQ-AGPTO-CTA         OF GFCTB0C6
                                       TO CSEQ-AGPTO-CTA
                                       OF GFCTB0I0
      *
           MOVE DINIC-PRMSS-PCOTE      OF GFCTB0C6
                                       TO DINIC-PRMSS-PCOTE
                                       OF GFCTB0I0
      *
           MOVE GFCTK7-CGC-CPF         TO CCGC-CPF
                                       OF GFCTB0I0
      *
           MOVE GFCTK7-FILIAL          TO CFLIAL-CGC
                                       OF GFCTB0I0
      *
           MOVE GFCTK7-CONTROLE        TO WRK-CCTRL-CPF-CGC-X
           MOVE WRK-CCTRL-CPF-CGC-9    TO CCTRL-CPF-CGC
                                       OF GFCTB0I0
      *
           EXEC SQL
               SELECT  CCGC_CPF,
                       CFLIAL_CGC,
                       CCTRL_CPF_CGC
               INTO   :GFCTB0I0.CCGC-CPF,
                      :GFCTB0I0.CFLIAL-CGC,
                      :GFCTB0I0.CCTRL-CPF-CGC
               FROM    DB2PRD.TPRMSS_GRP_CLI
               WHERE   CAGPTO_CTA         = :GFCTB0I0.CAGPTO-CTA
                 AND   CSERVC_TARIF       = :GFCTB0I0.CSERVC-TARIF
                 AND   CSEQ_AGPTO_CTA     = :GFCTB0I0.CSEQ-AGPTO-CTA
                 AND   DINIC_PRMSS_PCOTE  = :GFCTB0I0.DINIC-PRMSS-PCOTE
ST25X6*          AND   CCGC_CPF           = :GFCTB0I0.CCGC-CPF
ST25X6*          AND   CFLIAL_CGC         = :GFCTB0I0.CFLIAL-CGC
ST25X6*          AND   CCTRL_CPF_CGC      = :GFCTB0I0.CCTRL-CPF-CGC
ST25X6           AND   CCGC_CPF_ST        = :GFCTB0I0.CCGC-CPF-ST
ST25X6           AND   CFLIAL_CGC_ST      = :GFCTB0I0.CFLIAL-CGC-ST
ST25X6           AND   CCTRL_CPF_CGC_ST   = :GFCTB0I0.CCTRL-CPF-CGC-ST
           END-EXEC
      *
           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               SET DB2-SELECT          TO TRUE
               SET WRK-ABENDAR         TO TRUE
               MOVE 'TPRMSS_GRP_CLI'   TO FRWKGDB2-NOME-TABELA
               MOVE SQLSTATE           TO FRWKGDB2-SQLSTATE
               MOVE SQLCA              TO FRWKGDB2-SQLCA
               MOVE SPACES             TO FRWKGDB2-STORED-PROC
               MOVE '090'              TO FRWKGDB2-LOCAL
               PERFORM 9000-ERRO-DB2
           END-IF
      *
           IF  SQLCODE                 EQUAL ZEROS
               PERFORM 2647-VERIFICA-FUTURO
           END-IF
      *
           .
      *
      *----------------------------------------------------------------*
       2646-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       2647-VERIFICA-FUTURO            SECTION.
      *----------------------------------------------------------------*
      *
           IF GFCTK7-TIPO-ADESAO       EQUAL  'I'
           OR GFCTK7-TIPO-ADESAO       EQUAL  'A'
              MOVE 'S'                 TO WRK-ACHOU-FUTURO
           ELSE
              IF CINDCD-ADSAO-INDVD    OF GFCTB0C6
                                       EQUAL 'I'
              OR CINDCD-ADSAO-INDVD    OF GFCTB0C6
                                       EQUAL 'A'
                 MOVE 'S'              TO WRK-ACHOU-FUTURO
              ELSE
                  IF (CPSSOA-SERVC-TARIF
                                       OF GFCTB0C6 EQUAL '3') OR
                     (GFCTK7-CPSSOA-TARIFA
                                       EQUAL '3')
                      MOVE 'S'         TO WRK-ACHOU-FUTURO
                 ELSE
                     IF GFCTK7-CPSSOA-TARIFA
                                       EQUAL CPSSOA-SERVC-TARIF
                                       OF GFCTB0C6
                        MOVE 'S'       TO WRK-ACHOU-FUTURO
                     END-IF
                  END-IF
              END-IF
           END-IF
      *
           .
      *
      *----------------------------------------------------------------*
       2647-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       2650-FECHAR-CSR02               SECTION.
      *----------------------------------------------------------------*
      *
           EXEC SQL
               CLOSE CSR02-GFCTB0C6
           END-EXEC
      *
           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               SET DB2-CLOSE           TO TRUE
               SET WRK-ABENDAR         TO TRUE
               MOVE 'PRMSS_ADSAO_PCOTE'
                                       TO FRWKGDB2-NOME-TABELA
               MOVE SQLSTATE           TO FRWKGDB2-SQLSTATE
               MOVE SQLCA              TO FRWKGDB2-SQLCA
               MOVE SPACES             TO FRWKGDB2-STORED-PROC
               MOVE '100'              TO FRWKGDB2-LOCAL
               PERFORM 9000-ERRO-DB2
           END-IF
      *
           .
      *
      *----------------------------------------------------------------*
       2650-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       2660-INSERIR-C6                 SECTION.
      *----------------------------------------------------------------*
      *
           PERFORM 2665-OBTER-MAIOR-SEQ
      *
           MOVE GFCTK7-CPACOTE         TO CSERVC-TARIF
                                       OF GFCTB0C6
      *
           MOVE GFCTK7-CAGPTO          TO CAGPTO-CTA
                                       OF GFCTB0C6
      *
           MOVE WRK-SEQUENCIA          TO CSEQ-AGPTO-CTA
                                       OF GFCTB0C6
      *
           MOVE GFCTK7-DATA-INICIO     TO DINIC-PRMSS-PCOTE
                                       OF GFCTB0C6
      *
           MOVE 2123                   TO CFUNC-MANUT-INCL
                                       OF GFCTB0C6
      *
           MOVE ZEROS                  TO CFUNC-MANUT
                                       OF GFCTB0C6
      *
           MOVE GFCTK7-DATA-FIM        TO DFIM-PRMSS-PCOTE
                                       OF GFCTB0C6
      *
           MOVE GFCTK7-TIPO-ADESAO     TO CINDCD-ADSAO-INDVD
                                       OF GFCTB0C6
      *
           IF GFCTK7-TIPO-ADESAO       EQUAL 'C'
              IF GFCTK7-SOLICITA-EXCECAO
                                       EQUAL 1
                 MOVE 'S'              TO CINDCD-EXCEC-ADSAO
                                       OF GFCTB0C6
              ELSE
                 MOVE ' '              TO CINDCD-EXCEC-ADSAO
                                       OF GFCTB0C6
              END-IF
      *
              MOVE ZEROS               TO CIDTFD-PRMSS-INDVD
                                       OF GFCTB0C6
              MOVE GFCTK7-VISUAL-COMPULS
                                       TO CINDCD-VSLAO-COMP
                                       OF GFCTB0C6
              MOVE GFCTK7-CPSSOA-TARIFA
                                       TO CPSSOA-SERVC-TARIF
                                       OF GFCTB0C6
           END-IF
      *
           IF GFCTK7-TIPO-ADESAO       EQUAL 'I'
              MOVE ' '                 TO CINDCD-EXCEC-ADSAO
                                       OF GFCTB0C6
              MOVE GFCTK7-PRMSS-INDVD
                                       TO CIDTFD-PRMSS-INDVD
                                       OF GFCTB0C6
              MOVE ZEROS               TO CINDCD-VSLAO-COMP
                                       OF GFCTB0C6
           END-IF
      *
           IF GFCTK7-TIPO-ADESAO       EQUAL 'A'
              IF GFCTK7-SOLICITA-EXCECAO
                                       EQUAL  1
                 MOVE 'S'              TO CINDCD-EXCEC-ADSAO
                                       OF GFCTB0C6
              ELSE
                 MOVE ' '              TO CINDCD-EXCEC-ADSAO
                                       OF GFCTB0C6
              END-IF
      *
              MOVE GFCTK7-PRMSS-INDVD TO CIDTFD-PRMSS-INDVD
                                       OF GFCTB0C6
              MOVE GFCTK7-VISUAL-COMPULS
                                       TO CINDCD-VSLAO-COMP
                                       OF GFCTB0C6
              MOVE GFCTK7-CPSSOA-TARIFA
                                       TO CPSSOA-SERVC-TARIF
                                       OF GFCTB0C6
           END-IF
      *
           MOVE SPACES                 TO CINDCD-AGPTO-TOT
                                       OF GFCTB0C6
           MOVE '0001-01-01-01.01.01.000001'
                                       TO HMANUT-REG
                                       OF GFCTB0C6
      *
           EXEC SQL
             INSERT INTO DB2PRD.PRMSS_ADSAO_PCOTE
                  (CSERVC_TARIF,
                   CAGPTO_CTA,
                   DINIC_PRMSS_PCOTE,
                   CSEQ_AGPTO_CTA,
                   CFUNC_MANUT_INCL,
                   CFUNC_MANUT,
                   DFIM_PRMSS_PCOTE,
                   CINDCD_ADSAO_INDVD,
                   CINDCD_AGPTO_TOT,
                   HMANUT_REG,
                   HINCL_REG_SIST,
                   CINDCD_EXCEC_ADSAO,
                   CIDTFD_PRMSS_INDVD,
                   CINDCD_VSLAO_COMP,
                   CPSSOA_SERVC_TARIF)
               VALUES
                  (:GFCTB0C6.CSERVC-TARIF,
                   :GFCTB0C6.CAGPTO-CTA,
                   :GFCTB0C6.DINIC-PRMSS-PCOTE,
                   :GFCTB0C6.CSEQ-AGPTO-CTA,
                   :GFCTB0C6.CFUNC-MANUT-INCL,
                   :GFCTB0C6.CFUNC-MANUT,
                   :GFCTB0C6.DFIM-PRMSS-PCOTE,
                   :GFCTB0C6.CINDCD-ADSAO-INDVD,
                   :GFCTB0C6.CINDCD-AGPTO-TOT,
                   :GFCTB0C6.HMANUT-REG,
                   CURRENT TIMESTAMP,
                   :GFCTB0C6.CINDCD-EXCEC-ADSAO,
                   :GFCTB0C6.CIDTFD-PRMSS-INDVD,
                   :GFCTB0C6.CINDCD-VSLAO-COMP,
                   :GFCTB0C6.CPSSOA-SERVC-TARIF)
           END-EXEC
      *
           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               SET DB2-INSERT          TO TRUE
               SET WRK-ABENDAR         TO TRUE
               MOVE 'PRMSS_ADSAO_PCOTE'
                                       TO FRWKGDB2-NOME-TABELA
               MOVE SQLSTATE           TO FRWKGDB2-SQLSTATE
               MOVE SQLCA              TO FRWKGDB2-SQLCA
               MOVE SPACES             TO FRWKGDB2-STORED-PROC
               MOVE '110'              TO FRWKGDB2-LOCAL
               PERFORM 9000-ERRO-DB2
           END-IF
      *
           ADD 1                       TO ACU-INSERIDOS-B0C6
      *
           .
      *
      *----------------------------------------------------------------*
       2660-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       2665-OBTER-MAIOR-SEQ            SECTION.
      *----------------------------------------------------------------*
      *
           MOVE GFCTK7-CAGPTO          TO CAGPTO-CTA
                                       OF GFCTB0C6
      *
           MOVE GFCTK7-CPACOTE         TO CSERVC-TARIF
                                       OF GFCTB0C6
      *
           MOVE GFCTK7-DATA-INICIO     TO DINIC-PRMSS-PCOTE
                                       OF GFCTB0C6
      *
           EXEC SQL
               SELECT   MAX(CSEQ_AGPTO_CTA)
               INTO     :GFCTB0C6.CSEQ-AGPTO-CTA
               FROM     DB2PRD.PRMSS_ADSAO_PCOTE
               WHERE    CAGPTO_CTA        = :GFCTB0C6.CAGPTO-CTA
                 AND    CSERVC_TARIF      = :GFCTB0C6.CSERVC-TARIF
                 AND    DINIC_PRMSS_PCOTE = :GFCTB0C6.DINIC-PRMSS-PCOTE
           END-EXEC
      *
           IF (SQLCODE                 NOT EQUAL ZEROS AND -305) OR
              (SQLWARN0                EQUAL 'W')
               SET DB2-SELECT          TO TRUE
               SET WRK-ABENDAR         TO TRUE
               MOVE 'PRMSS_ADSAO_PCOTE'
                                       TO FRWKGDB2-NOME-TABELA
               MOVE SQLSTATE           TO FRWKGDB2-SQLSTATE
               MOVE SQLCA              TO FRWKGDB2-SQLCA
               MOVE SPACES             TO FRWKGDB2-STORED-PROC
               MOVE '120'              TO FRWKGDB2-LOCAL
               PERFORM 9000-ERRO-DB2
           END-IF
      *
           IF  SQLCODE                 EQUAL -305
               MOVE 1                  TO WRK-SEQUENCIA
           ELSE
               MOVE CSEQ-AGPTO-CTA     OF GFCTB0C6
                                       TO WRK-S9-09
               MOVE WRK-9-09           TO WRK-SEQUENCIA
               ADD 1                   TO WRK-SEQUENCIA
           END-IF
      *
           .
      *
      *----------------------------------------------------------------*
       2665-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       2690-GERAR-CLIENTE              SECTION.
      *----------------------------------------------------------------*
      *
           MOVE GFCTK7-CAGPTO          TO CAGPTO-CTA
                                       OF GFCTB0I0
      *
           MOVE GFCTK7-CPACOTE         TO CSERVC-TARIF
                                       OF GFCTB0I0
      *
           MOVE GFCTK7-DATA-INICIO     TO DINIC-PRMSS-PCOTE
                                       OF GFCTB0I0
      *
           MOVE WRK-SEQUENCIA          TO CSEQ-AGPTO-CTA
                                       OF GFCTB0I0
      *
           MOVE GFCTK7-CGC-CPF         TO CCGC-CPF
                                       OF GFCTB0I0
      *
           MOVE GFCTK7-FILIAL          TO CFLIAL-CGC
                                       OF GFCTB0I0
      *
           MOVE GFCTK7-CONTROLE        TO WRK-CCTRL-CPF-CGC-X
           MOVE WRK-CCTRL-CPF-CGC-9    TO CCTRL-CPF-CGC
                                       OF GFCTB0I0
      *
           EXEC SQL
             INSERT INTO DB2PRD.TPRMSS_GRP_CLI
                  (CSERVC_TARIF,
                   CAGPTO_CTA,
                   DINIC_PRMSS_PCOTE,
                   CSEQ_AGPTO_CTA,
                   CCGC_CPF,
                   CCTRL_CPF_CGC,
                   CFLIAL_CGC)
               VALUES
                  (:GFCTB0I0.CSERVC-TARIF,
                   :GFCTB0I0.CAGPTO-CTA,
                   :GFCTB0I0.DINIC-PRMSS-PCOTE,
                   :GFCTB0I0.CSEQ-AGPTO-CTA,
                   :GFCTB0I0.CCGC-CPF,
                   :GFCTB0I0.CCTRL-CPF-CGC,
                   :GFCTB0I0.CFLIAL-CGC)
           END-EXEC
      *
           IF (SQLCODE                 NOT EQUAL ZEROS AND -305) OR
              (SQLWARN0                EQUAL 'W')
               SET DB2-INSERT          TO TRUE
               SET WRK-ABENDAR         TO TRUE
               MOVE 'TPRMSS_GRP_CLI'   TO FRWKGDB2-NOME-TABELA
               MOVE SQLSTATE           TO FRWKGDB2-SQLSTATE
               MOVE SQLCA              TO FRWKGDB2-SQLCA
               MOVE SPACES             TO FRWKGDB2-STORED-PROC
               MOVE '130'              TO FRWKGDB2-LOCAL
               PERFORM 9000-ERRO-DB2
           END-IF
      *
           ADD 1                       TO ACU-INSERIDOS-B0I0
      *
           .
      *
      *----------------------------------------------------------------*
       2690-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*
      *
           CLOSE                       EARQWK7C
                                       SWK7INCO
                                       SWK7CONS
      *
           SET ARQ-CLOSE               TO TRUE
      *
           PERFORM 1100-TESTAR-FILE-STATUS
      *
           PERFORM 7300-CKRS-FINALIZAR
      *
           PERFORM 8200-DISPLAYS
      *
           DISPLAY '************ PROGRAMA GFCT2123 ************'
           DISPLAY '*                                         *'
           DISPLAY '* PROGRAMA GFCT2123 ENCERRADO COM SUCESSO *'
           DISPLAY '*                                         *'
           DISPLAY '************ PROGRAMA GFCT2123 ************'
      *
           MOVE ZEROS                  TO RETURN-CODE
      *
           PERFORM 6000-STOP-RUN
      *
           .
      *
      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    GRAVA ARQUIVO DE SAIDA SWK7INCO                             *
      *----------------------------------------------------------------*
       3100-GRAVAR-SWK7INCO            SECTION.
      *----------------------------------------------------------------*
      *
           INITIALIZE                  FD-ENTRADA
      *
           MOVE '0002'                 TO WRK-LOCAL
           MOVE '3100-GRAVAR-SWK7INCO' TO FRWKGHEA-IDEN-PARAGRAFO
      *
           MOVE GFCTK7-ENTRADA         TO FD-ENTRADA
           MOVE WRK-MENSAGEM           TO FD-MENSAGEM
      *
           WRITE FD-ENTRADA
      *
           SET ARQ-WRITE               TO TRUE
      *
           PERFORM 1120-TESTAR-FS-SWK7INCO
      *
           ADD 1                       TO ACU-GRAVA-SWK7INCO
      *
           MOVE 'S'                    TO WRK-INCONSISTENTE
      *
           .
      *
      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    GRAVA ARQUIVO DE SAIDA SWK7CONS                             *
      *----------------------------------------------------------------*
       3200-GRAVAR-SWK7CONS            SECTION.
      *----------------------------------------------------------------*
      *
           INITIALIZE                  FD-SWK7CONS
      *
           MOVE '0003'                 TO WRK-LOCAL
           MOVE '3200-GRAVAR-SWK7CONS' TO FRWKGHEA-IDEN-PARAGRAFO
      *
           WRITE FD-SWK7CONS           FROM GFCTK7-ENTRADA
      *
           SET ARQ-WRITE               TO TRUE
      *
           PERFORM 1130-TESTAR-FS-SWK7CONS
      *
           ADD 1                       TO ACU-GRAVA-SWK7CONS
      *
           .
      *
      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    FIM DE EXECUCAO                                             *
      *----------------------------------------------------------------*
       6000-STOP-RUN                   SECTION.
      *----------------------------------------------------------------*
      *
           STOP RUN
      *
           .
      *
      *----------------------------------------------------------------*
       6000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    CHAMAR MODULO PARA INCIAR CONEXAO                           *
      *----------------------------------------------------------------*
       7000-CKRS-INICIALIZAR           SECTION.
      *----------------------------------------------------------------*
      *                                                                *
           INITIALIZE                  CKRS01-INTERFACE
      *                                                                *
           MOVE WRK-PROGRAMA           TO CK01-PLAN
           SET  CK01-INICIALIZAR       TO TRUE
           MOVE 'DB2'                  TO CK01-ID-DB2
           MOVE LENGTH                 OF WRK-AREA-RESTART
                                       TO CK01-TAM-AREA-RESTART
           MOVE WRK-AREA-RESTART       TO CK01-AREA-RESTART
      *                                                                *
           PERFORM 7100-ACESSAR-CKRS0100
      *                                                                *
           IF  CK01-RESTART
               DISPLAY 'GFCT2123 - PROCESSAMENTO COM RESTART'
               MOVE CK01-AREA-RESTART(1:LENGTH
                                       OF WRK-AREA-RESTART)
                                       TO WRK-AREA-RESTART
           ELSE
               DISPLAY 'GFCT2123 - PROCESSAMENTO NORMAL (SEM RESTART)'
           END-IF
      *
           .
      *                                                                *
      *----------------------------------------------------------------*
       7000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
      *    CHAMADA AO MODULO DE CHECKPOINT/RESTART.                    *
      *----------------------------------------------------------------*
       7100-ACESSAR-CKRS0100           SECTION.
      *----------------------------------------------------------------*
      *                                                                *
           CALL WRK-CKRS0100           USING CKRS01-INTERFACE
      *                                                                *
           IF (RETURN-CODE             EQUAL 12) OR
              (CK01-CODIGO-RETORNO     EQUAL 12)
               DISPLAY '*************** GFCT2123 ***************'
               DISPLAY '*     RC=12 NA CHAMADA AO CKRS0100     *'
               DISPLAY '*        PROCESSAMENTO ENCERRADO       *'
               DISPLAY '*************** GFCT2123 ***************'
               GOBACK
           END-IF
      *                                                                *
           IF  CK01-CODIGO-RETORNO     NOT EQUAL ZEROS
               MOVE '7100-ACESSAR-CKRS0100'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               MOVE WRK-CKRS0100       TO FRWKGMOD-NOME-MODULO
      *
               MOVE CK01-CODIGO-RETORNO
                                       TO FRWKGMOD-COD-RETORNO
      *
               MOVE '0999'             TO FRWKGMOD-COD-ERRO
      *
               MOVE CK01-CODIGO-MENSAGEM
                                       TO FRWKGMOD-COD-MENSAGEM

      *
               SET WRK-ABENDAR         TO TRUE
      *
               PERFORM 9200-ERRO-MODULO
      *
           END-IF
      *
           .
      *                                                                *
      *----------------------------------------------------------------*
       7100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
      *    CHAMAR MODULO DE CHECKPOINT/RESTART PARA CHECKPOINT.        *
      *----------------------------------------------------------------*
       7200-CKRS-CHECKPOINT            SECTION.
      *----------------------------------------------------------------*
      *                                                                *
           INITIALIZE                  CKRS01-INTERFACE
      *                                                                *
           MOVE SPACES                 TO CK01-PLAN
           SET  CK01-CHECKPOINT        TO TRUE
           MOVE 'DB2'                  TO CK01-ID-DB2
           MOVE LENGTH                 OF WRK-AREA-RESTART
                                       TO CK01-TAM-AREA-RESTART
           MOVE WRK-AREA-RESTART       TO CK01-AREA-RESTART
      *                                                                *
           PERFORM 7100-ACESSAR-CKRS0100
      *
           .
      *                                                                *
      *----------------------------------------------------------------*
       7200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
      *    CHAMAR MODULO PARA DESCONECTAR                              *
      *----------------------------------------------------------------*
       7300-CKRS-FINALIZAR             SECTION.
      *----------------------------------------------------------------*
      *                                                                *
           INITIALIZE CKRS01-INTERFACE
      *                                                                *
           MOVE SPACES                 TO CK01-PLAN
           SET  CK01-FINALIZAR         TO TRUE
           MOVE 'DB2'                  TO CK01-ID-DB2
           MOVE LENGTH                 OF WRK-AREA-RESTART
                                       TO CK01-TAM-AREA-RESTART
           MOVE WRK-AREA-RESTART       TO CK01-AREA-RESTART
      *                                                                *
           PERFORM 7100-ACESSAR-CKRS0100
           .
      *                                                                *
      *----------------------------------------------------------------*
       7300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *    CONSISTENCIA DE DATA
      *----------------------------------------------------------------*
       7400-CONSISTIR-DATA             SECTION.
      *----------------------------------------------------------------*
      *
           INITIALIZE                  CALE01-REGISTRO
      *
           MOVE 'F2'                   TO CALE01-FUNCAO
           MOVE 'SF2001'               TO CALE01-SUB-FUNCAO(1:6)
           MOVE 'CALE0001'             TO CALE01-ID-BLOCO
           MOVE LENGTH                 OF CALE01-REGISTRO
                                       TO CALE01-TAM-BLOCO
           MOVE 07                     TO CALE01-FORMATO-ARGUMENTO-INI
           MOVE WRK-DATA-10            TO CALE01-VLR-ARGUMENTO-INI
      *
           CALL WRK-CALE2000           USING CALE01-REGISTRO
      *
           IF CALE01-COD-RETORNO       NOT EQUAL ZEROS
           OR CALE01-CONSIST-DT-HORA   EQUAL 'I'
              MOVE WRK-MSG-DATA-INCO   TO WRK-MENSAGEM
              PERFORM 3100-GRAVAR-SWK7INCO
           END-IF
      *
           .
      *
      *----------------------------------------------------------------*
       7400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    CHAMADA AO FRAMEWORK PARA REGISTRAR OCORRENCIA DE ERRO      *
      *----------------------------------------------------------------*
       8000-CHAMAR-FRWK2999            SECTION.
      *----------------------------------------------------------------*
      *
           DISPLAY ' '
           DISPLAY '*** FRWK2999 - PARAMETROS ENVIADOS ***'
           DISPLAY 'FRWKGHEA-TIPO-ERRO      = ' FRWKGHEA-TIPO-ERRO
           DISPLAY 'FRWKGHEA-NOME-PROGRAMA  = ' FRWKGHEA-NOME-PROGRAMA
           DISPLAY 'FRWKGHEA-IDEN-PARAGRAFO = ' FRWKGHEA-IDEN-PARAGRAFO
           DISPLAY 'FRWKGHEA-TAM-DADOS      = ' FRWKGHEA-TAM-DADOS
      *
           CALL WRK-FRWK2999           USING WRK-AREA-ERRO
      *
           DISPLAY ' '
           DISPLAY '*** FRWK2999 - PARAMETROS RETORNADOS ***'
           DISPLAY 'FRWKGERR-COD-RETORNO    = ' FRWKGERR-COD-RETORNO
           DISPLAY 'FRWKGERR-COD-ERRO       = ' FRWKGERR-COD-ERRO
           DISPLAY 'FRWKGERR-COD-MENSAGEM   = ' FRWKGERR-COD-MENSAGEM
      *
           .
      *
      *----------------------------------------------------------------*
       8000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    ROTINA PARA ABENDAR O PROGRAMA                              *
      *----------------------------------------------------------------*
       8100-ROTINA-ABEND               SECTION.
      *----------------------------------------------------------------*
      *
           DISPLAY '*** BRAD0450 CHAMADO PARA ABENDAR O PROGRAMA ***'
           DISPLAY ' '
      *
           CALL 'BRAD0450'             USING WRK-ABEND-0450
                                             WRK-DUMP
      *
           .
      *
      *----------------------------------------------------------------*
       8100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    DEMONSTRA ESTATISTICA DO PROCESSAMENTO                      *
      *----------------------------------------------------------------*
       8200-DISPLAYS                   SECTION.
      *----------------------------------------------------------------*
      *
           DISPLAY '************* PROGRAMA GFCT2123 *************'
           DISPLAY '*                                           *'
      *
           MOVE ACU-LIDOS-EARQWK7C     TO WRK-MASCARA
           DISPLAY '* REG. LIDOS EARQWK7C.........: ' WRK-MASCARA ' *'
           DISPLAY '*                                           *'
      *
           MOVE ACU-GRAVA-SWK7INCO     TO WRK-MASCARA
           DISPLAY '* REG. INCONSISTENTES.........: ' WRK-MASCARA ' *'
           DISPLAY '*                                           *'
      *
           MOVE ACU-GRAVA-SWK7CONS     TO WRK-MASCARA
           DISPLAY '* REG. CONSISTENTES/INSERIDOS.: ' WRK-MASCARA ' *'
           DISPLAY '*                                           *'
      *
           MOVE ACU-INSERIDOS-B0C6     TO WRK-MASCARA
           DISPLAY '* REG. INSERIDOS TABELA B0C6..: ' WRK-MASCARA ' *'
           DISPLAY '*                                           *'
      *
           MOVE ACU-INSERIDOS-B0I0     TO WRK-MASCARA
           DISPLAY '* REG. INSERIDOS TABELA B0I0..: ' WRK-MASCARA ' *'
           DISPLAY '*                                           *'
      *
           DISPLAY '*                                           *'
           DISPLAY '************* PROGRAMA GFCT2123 *************'
      *
           .
      *
      *----------------------------------------------------------------*
       8200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    TRATAMENTO PARA ERRO NO ACESSO AO DB2                       *
      *----------------------------------------------------------------*
       9000-ERRO-DB2                   SECTION.
      *----------------------------------------------------------------*
      *
           SET ERRO-DB2                TO TRUE
      *
           MOVE WRK-PROGRAMA           TO FRWKGHEA-NOME-PROGRAMA
           MOVE FRWKGDB2-TAM-LAYOUT    TO FRWKGHEA-TAM-DADOS
           MOVE WRK-AREA-ERRO-DB2      TO WRK-BLOCO-INFO-ERRO
                                          (1:FRWKGHEA-TAM-DADOS)
      *
           MOVE FRWKGDB2-SQLCODE       TO WRK-MASCARA-SQLCODE
      *
           DISPLAY ' '
           DISPLAY '** ERRO ACESSO DB2 **'.
           DISPLAY 'FRWKGHEA-IDEN-PARAGRAFO = ' FRWKGHEA-IDEN-PARAGRAFO
           DISPLAY 'FRWKGDB2-NOME-TABELA    = ' FRWKGDB2-NOME-TABELA
           DISPLAY 'FRWKGDB2-LOCAL          = ' FRWKGDB2-LOCAL
           DISPLAY 'FRWKGDB2-SQLCODE        = ' WRK-MASCARA-SQLCODE
           DISPLAY 'FRWKGDB2-SQLSTATE       = ' FRWKGDB2-SQLSTATE
           DISPLAY 'FRWKGDB2-SQLCA          = ' FRWKGDB2-SQLCA
      *
           PERFORM 9999-API-ERROS
      *
           .
      *
      *----------------------------------------------------------------*
       9000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      * PROCESSAMENTO DE ERRO DE ARQUIVO                               *
      *----------------------------------------------------------------*
       9100-ERRO-ARQ                   SECTION.
      *----------------------------------------------------------------*
      *
           SET ERRO-ARQUIVO            TO TRUE
      *
           MOVE WRK-PROGRAMA           TO FRWKGHEA-NOME-PROGRAMA
           MOVE FRWKGARQ-TAM-LAYOUT    TO FRWKGHEA-TAM-DADOS
           MOVE WRK-FRWKGARQ           TO WRK-BLOCO-INFO-ERRO
                                       (1:FRWKGHEA-TAM-DADOS)
      *
           DISPLAY ' '
           DISPLAY 'FRWKGHEA-IDEN-PARAGRAFO = ' FRWKGHEA-IDEN-PARAGRAFO
           DISPLAY 'FRWKGARQ-FILE-STATUS    = ' FRWKGARQ-FILE-STATUS
           DISPLAY 'FRWKGARQ-NOME-ARQUIVO   = ' FRWKGARQ-NOME-ARQUIVO
           DISPLAY 'FRWKGARQ-COMANDO        = ' FRWKGARQ-COMANDO
           DISPLAY 'WRK-LOCAL               = ' WRK-LOCAL
      *
           PERFORM 9999-API-ERROS
      *
           .
      *
      *----------------------------------------------------------------*
       9100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       9200-ERRO-MODULO                SECTION.
      *----------------------------------------------------------------*
      *
           SET ERRO-MODULO             TO TRUE
      *
           MOVE FRWKGMOD-TAM-LAYOUT    TO FRWKGHEA-TAM-DADOS
           MOVE WRK-AREA-ERRO-MODULO   TO WRK-BLOCO-INFO-ERRO(1:35)
      *
           PERFORM 9999-API-ERROS
      *
           .
      *
      *----------------------------------------------------------------*
       9200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *
      *----------------------------------------------------------------*
      *    ROTINA ENCERRAMENTO COM ERRO NO PROGRAMA                    *
      *----------------------------------------------------------------*
       9999-API-ERROS                  SECTION.
      *----------------------------------------------------------------*
      *
           PERFORM 8000-CHAMAR-FRWK2999
      *
           PERFORM 8200-DISPLAYS
      *
           IF WRK-ABENDAR
               PERFORM 8100-ROTINA-ABEND
           END-IF
      *
           PERFORM 6000-STOP-RUN
      *
           .
      *
      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
