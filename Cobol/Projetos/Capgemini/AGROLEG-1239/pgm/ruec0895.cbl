      *===============================================================*
       IDENTIFICATION DIVISION.
      *===============================================================*

       PROGRAM-ID. RUEC0895.
       AUTHOR.     EDUARDO WATANABE.

      ******************************************************************
      *    ANALISTA   :        EDUARDO WATANABE                        *
      *    DATA       :        JUNHO/2017                              *
      *----------------------------------------------------------------*
      *    PROJETO    :        BSI 99/9999 - CATALOGO 4.09             *
      *----------------------------------------------------------------*
      *    OBJETIVO :                                                  *
      *                                                                *
      *       ENVIAR COR0005 (ALTERACAO DE CAMPOS COMPLEMENTARES).     *
      *                                                                *
      *----------------------------------------------------------------*
      *    ENTRADA :                                                   *
      *                                                                *
      *      ARQUIVOS:                                                 *
      *         DDNAME    DESCRICAO                       INCLUDE/BOOK *
      *         ARQLIBER  ARQUIVO LIBERADOS NO DIA        --------     *
      *                                                                *
      *----------------------------------------------------------------*
      *    SAIDA :                                                     *
      *                                                                *
      *      ARQUIVOS:                                                 *
      *         DDNAME    DESCRICAO                       INCLUDE/BOOK *
      *         ARQOCORR  ARQUIVO OCORRENCIAS PROC        --------     *
      *                                                                *
      *----------------------------------------------------------------*
      *    DB2 :                                                       *
      *                                                                *
      *      TABELA:                                                   *
      *         DDNAME    DESCRICAO                  NOME DA TABELA    *
      *       RUMOV002    HIST_MOVTO                 V01HIST_MOVTO     *
      *                                                                *
      *----------------------------------------------------------------*
      *    MODULOS CHAMADOS :                                          *
      *                                                                *
      *       POOL7100 - TRATAMENTO DE ERROS DE APLICACAO.             *
      *       POOL7600 - OBTER DATA DO SISTEMA.                        *
      *                                                                *
      ******************************************************************
EG0117*================================================================*
EG0117*                      A L T E R A C A O                         *
EG0117*================================================================*
EG0117*    ANALISTA....:   EVANDRO GUIMARAES       - BSI TECNOLOGIA    *
EG0117*    DATA........:   JANEIRO/2017                                *
EG0117*    OBJETIVO....:   DESPREZAR LIBERACOES GERADAS PARA EFETIVAR  *
EG0117*                    PRORROGACAO DE PARCELAS QUE VIERAM VENCIDAS *
EG0117*                    DO HSBC (ESTORNO DE MORA INDEVIDA)          *
EG0117*================================================================*
NAV506*  ANALISTA......:   NAVA                                        *
NAV506*  DATA..........:   MAIO/2023                                   *
NAV506*----------------------------------------------------------------*
NAV506*  OBJETIVO......:   ADEQUACOES SICOR 5.06                       *
NAV506*                    FORMATACAO DOS CAMPOS                       *
NAV506*================================================================*
EG0117
           EJECT
      *===============================================================*
       ENVIRONMENT DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       CONFIGURATION SECTION.
      *---------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *---------------------------------------------------------------*
       INPUT-OUTPUT SECTION.
      *---------------------------------------------------------------*

       FILE-CONTROL.

           SELECT ARQLIBER ASSIGN TO UT-S-ARQLIBER
                      FILE STATUS IS WRK-FS-ARQLIBER.

           SELECT ARQOCORR ASSIGN TO UT-S-ARQOCORR
                      FILE STATUS IS WRK-FS-ARQOCORR.

      *===============================================================*
       DATA DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       FILE SECTION.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      *    INPUT:     ARQUIVO LIBERADOS NO DIA                        *
      *               ORG. SEQUENCIAL   -   LRECL = 750               *
      *---------------------------------------------------------------*

       FD  ARQLIBER
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

CAT408 01  FD-ARQLIBER                 PIC  X(1259).

      *---------------------------------------------------------------*
      *    OUTPUT:    ARQUIVO OCORRENCIAS DO PROCESSAMENTO            *
      *               ORG. SEQUENCIAL   -   LRECL = 100               *
      *---------------------------------------------------------------*

       FD  ARQOCORR
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-ARQOCORR                 PIC  X(100).

      *---------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** RUEC0895 - INICIO DA AREA DE WORKING ***'.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** ACUMULADORES ***'.
      *---------------------------------------------------------------*

       01  WRK-AREA-RESTART.
           05  ACU-LIDOS-ARQLIBER      PIC  9(009) COMP-3  VALUE ZEROS.
           05  ACU-GRAVS-ARQOCORR      PIC  9(009) COMP-3  VALUE ZEROS.
           05  ACU-RCOR-OK             PIC  9(009) COMP-3  VALUE ZEROS.
           05  ACU-RCOR-NOK            PIC  9(009) COMP-3  VALUE ZEROS.

       77  WRK-LIDOS-ARQLIBER-E        PIC  ZZZ.ZZZ.ZZ9    VALUE SPACES.
       77  WRK-GRAVS-ARQOCORR-E        PIC  ZZZ.ZZZ.ZZ9    VALUE SPACES.
       77  WRK-RCOR-OK-E               PIC  ZZZ.ZZZ.ZZ9    VALUE SPACES.
       77  WRK-RCOR-NOK-E              PIC  ZZZ.ZZZ.ZZ9    VALUE SPACES.

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA TESTES DE FILE STATUS ***'.
      *---------------------------------------------------------------*

       77  WRK-FS-ARQLIBER             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-ARQOCORR             PIC  X(002)         VALUE SPACES.

       77  WRK-ABERTURA                PIC  X(010)         VALUE
           ' ABERTURA '.
       77  WRK-LEITURA                 PIC  X(010)         VALUE
           ' LEITURA  '.
       77  WRK-GRAVACAO                PIC  X(010)         VALUE
           ' GRAVACAO '.
       77  WRK-FECHAMENTO              PIC  X(010)         VALUE
           'FECHAMENTO'.

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREAS AUXILIARES ***'.
      *---------------------------------------------------------------*

       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.

       01  WRK-CHAVE-ARQLIBER.
           05  WRK-CHV-LIB-CCONTR      PIC  9(09)          VALUE ZEROS.

EG0117*---------------------------------------------------------------*
EG0117 01  FILLER                      PIC  X(032)         VALUE
EG0117     '*** AUXILIARES PRORROGACAO   ***'.
EG0117*---------------------------------------------------------------*
EG0117
EG0117 01  FILLER.
EG0117     03 IND-1                    PIC S9(009) COMP-3  VALUE ZEROS.
EG0117     03 IND-2                    PIC S9(009) COMP-3  VALUE ZEROS.
EG0117     03 WRK-COUNT                PIC S9(009) COMP    VALUE ZEROS.
EG0117
EG0117 01  FILLER.
EG0117     03 WRK-DATA-ISO             PIC X(08)           VALUE SPACES.
EG0117     03 FILLER REDEFINES WRK-DATA-ISO.
EG0117        05 WRK-ANO-ISO           PIC X(04).
EG0117        05 WRK-MES-ISO           PIC X(02).
EG0117        05 WRK-DIA-ISO           PIC X(02).
EG0117
EG0117 01  FILLER.
EG0117     03 WRK-DATA-DB2-GRUPO.
EG0117        05 WRK-DIA-DB2           PIC X(02)           VALUE ZEROS.
EG0117        05 FILLER                PIC X(01)           VALUE '.'.
EG0117        05 WRK-MES-DB2           PIC X(02)           VALUE ZEROS.
EG0117        05 FILLER                PIC X(01)           VALUE '.'.
EG0117        05 WRK-ANO-DB2           PIC X(04)           VALUE ZEROS.
EG0117     03 WRK-DATA-DB2             REDEFINES
EG0117        WRK-DATA-DB2-GRUPO       PIC X(10).
EG0117
      *---------------------------------------------------------------*
       01  FILLER                      PIC X(079)          VALUE
           '*** AREA PARA CHAMADA MODULO RUEC8831 ***'.
      *---------------------------------------------------------------*

       77  WRK-RUEC8831                PIC X(08) VALUE 'RUEC8831'.

       COPY 'I#RUEC65'.

       01  WRK-8831-MSG.
           03  WRK-8831-COD-RET            PIC  9(02) VALUE ZEROS.
           03  FILLER                      PIC  X(01) VALUE '-'.
           03  WRK-8831-MENSAGEM           PIC  X(79) VALUE ZEROS.

      *----------------------------------------------------------------*
      *                      AREA DO MODULO CKRS0100                   *
      *----------------------------------------------------------------*

       77  WRK-CKRS0100                PIC X(08) VALUE 'CKRS0100'.

       COPY 'I#CKRS01'.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA COM LAYOUT DO ARQUIVO ARQLIBER ***'.
      *---------------------------------------------------------------*

CAT408 01  WRK-REG-ARQLIBER.
           03  WRK-LIB-CCONTR-X.
               07 WRK-LIB-CCONTR       PIC  9(009)         VALUE ZEROS.
CAT408     03  WRK-ARQLIBER.
CAT408         05 WRK-ARQ-CCONTR-X.
CAT408            07 WRK-ARQ-CCONTR    PIC  9(009)         VALUE ZEROS.
               05 WRK-LIB-IND          PIC  9(003) COMP-3  VALUE ZEROS.
               05 WRK-LIB-OCORRENCIAS  OCCURS  30 TIMES.
                  10  WRK-LIB-DATA     PIC  X(008)         VALUE ZEROS.
                  10  WRK-LIB-VALOR    PIC  X(015)         VALUE ZEROS.
CAT408         05 FILLER               PIC  X(049)         VALUE SPACES.
CAT408     03  WRK-ARQDESCL.
CAT408         05 WRK-DES-CCONTR-X.
CAT408            07 WRK-DES-CCONTR    PIC  9(009)         VALUE ZEROS.
CAT408         05 WRK-DES-VAZIO        PIC  X(001)         VALUE SPACES.
CAT408         05 WRK-DES-GRUPO        PIC  X(330)         VALUE SPACES.
CAT408         05 FILLER               PIC  X(160)         VALUE SPACES.
CAT408
      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA COM LAYOUT DO ARQUIVO ARQOCORR ***'.
      *---------------------------------------------------------------*

       01  WRK-ARQOCORR.
           05  WRK-ARQOCORR-CCONTR     PIC  9(09)        VALUE ZEROS.
           05  WRK-ARQOCORR-MSG        PIC  X(82)        VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** MENSAGEM DE ERRO DE FILE STATUS ***'.
      *---------------------------------------------------------------*

       01  WRK-ERRO-ARQUIVO.
           03  FILLER                  PIC  X(011)         VALUE
               '** ARQUIVO='.
           03  WRK-NOME-ARQUIVO        PIC  X(008)         VALUE SPACES.
           03  FILLER                  PIC  X(008)         VALUE
               ' FUNCAO='.
           03  WRK-OPERACAO            PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(013)         VALUE
               ' FILE-STATUS='.
           03  WRK-FILE-STATUS         PIC  X(002)         VALUE SPACES.
           03  FILLER                  PIC  X(003)         VALUE ' **'.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA BRAD7100 ***'.
      *---------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(36) VALUE
           '* AREA MENSAGEM DE DATA DO SISTEMA *'.
      *----------------------------------------------------------------*

       01  WRK-BRAD7600.
           03  WRK-B7600-DT-JULIANA      PIC 9(05) COMP-3 VALUE ZEROS.
           03  WRK-B7600-DT-AAMMDD       PIC 9(07) COMP-3 VALUE ZEROS.
           03  WRK-B7600-DT-AAAAMMDD     PIC 9(08) COMP-3 VALUE ZEROS.
           03  WRK-B7600-TI-HHMMSS       PIC 9(07) COMP-3 VALUE ZEROS.
           03  WRK-B7600-TI-HHMMSSMMMMMM PIC 9(13) COMP-3 VALUE ZEROS.
           03  WRK-B7600-TIMESTAMP       PIC X(20)        VALUE SPACES.

      *----------------------------------------------------------------*
      *   AREA DE INCLUDE DB2                                          *
      *----------------------------------------------------------------*

EG0117*---------------------------------------------------------------*
EG0117 01  FILLER                      PIC X(032)          VALUE
EG0117     '*** INCLUDE RUMOV002         ***'.
EG0117*---------------------------------------------------------------*
EG0117
EG0117     EXEC SQL
EG0117         INCLUDE RUMOV002
EG0117     END-EXEC.
EG0117
      *---------------------------------------------------------------*
       01  FILLER                      PIC X(032)          VALUE
           '*** INCLUDE SQLCA            ***'.
      *---------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** RUEC0895 - FIM DA AREA DE WORKING ***'.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       LINKAGE                         SECTION.
      *---------------------------------------------------------------*

       01  LNK-PARM.
           05  LNK-TAMANHO   PIC S9(04) COMP.
           05  LNK-PARAMETRO PIC  X(05).

      *===============================================================*
       PROCEDURE                       DIVISION USING LNK-PARM.
      *===============================================================*

      *---------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *---------------------------------------------------------------*

           CALL 'CKRS1000'.

           PERFORM 0100-INICIALIZAR-CKRS0100.

           PERFORM 1000-INICIALIZAR.

           PERFORM 3000-PROCESSAR      UNTIL
                   WRK-FS-ARQLIBER     EQUAL '10'.

           PERFORM 9000-FINALIZAR.

           PERFORM 0140-FINALIZAR-CKRS0100.

           STOP RUN.

      *---------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ROTINA DE INICIALIZACAO DO MODULO CKRS0100                    *
      *----------------------------------------------------------------*
       0100-INICIALIZAR-CKRS0100       SECTION.
      *----------------------------------------------------------------*

           INITIALIZE CKRS01-INTERFACE.

           MOVE  SPACES                    TO  CK01-PLAN.
           MOVE  'DB2'                     TO  CK01-ID-DB2.
           MOVE  'I'                       TO  CK01-FUNCAO.
           MOVE  LENGTH                    OF  WRK-AREA-RESTART
                                           TO  CK01-TAM-AREA-RESTART.
           MOVE  WRK-AREA-RESTART          TO  CK01-AREA-RESTART.

           PERFORM 0110-CHAMAR-CKRS01.

           IF  CK01-STATUS              EQUAL  'REST'
               MOVE  CK01-AREA-RESTART(1:CK01-TAM-AREA-RESTART)
                                           TO  WRK-AREA-RESTART
           END-IF.

      *----------------------------------------------------------------*
       0100-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ROTINA DE CHAMADA DO MODULO CKRS0100                          *
      *----------------------------------------------------------------*
       0110-CHAMAR-CKRS01                  SECTION.
      *----------------------------------------------------------------*

           CALL  WRK-CKRS0100           USING  CKRS01-INTERFACE.

           IF  CK01-CODIGO-RETORNO  NOT EQUAL  ZEROS
               PERFORM 0120-TRATAR-ERRO-CKRS0100
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0110-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ROTINA DE TRATAMENTO DE ERRO NO ACESSO AO MODULO CKRS0100     *
      *----------------------------------------------------------------*
       0120-TRATAR-ERRO-CKRS0100       SECTION.
      *----------------------------------------------------------------*

           MOVE 'APL'                          TO  ERR-TIPO-ACESSO.

           IF  CK01-CODIGO-RETORNO          EQUAL  8
               EVALUATE CK01-CODIGO-MENSAGEM
                   WHEN 'CKRS0001'
                        MOVE 'CKRS0100 - CODIGO DE FUNCAO INCORRETO'
                                               TO  ERR-TEXTO
                   WHEN 'CKRS0002'
                        MOVE 'CKRS0100 - JA FOI FEITA UMA CHAMADA EM MOD
      -                      'O INITIALIZE'
                                               TO  ERR-TEXTO
                   WHEN 'CKRS0003'
                        MOVE 'CKRS0100 - NAO FOI EFETUADA CHAMADA EM MOD
      -                      'O INITIALIZE'
                                               TO  ERR-TEXTO
                   WHEN 'CKRS0004'
                        MOVE 'CKRS0100 - CAMPO DB2-ID DEVE ESTAR INFORMA
      -                      'DO'              TO  ERR-TEXTO
                   WHEN 'CKRS0005'
                        MOVE 'CKRS0100 - O PROCESSO NAO SE ENCONTRA CADA
      -                      'STRADO'          TO  ERR-TEXTO
                   WHEN OTHER
                        MOVE 'CKRS0100 - ERRO NAO IDENTIFICADO'
                                               TO  ERR-TEXTO
               END-EVALUATE
           ELSE
               MOVE 'CKRS0100 - ERRO GRAVE - RETURN <> DE ZEROS E 8'
                                               TO  ERR-TEXTO
           END-IF.

           DISPLAY '************** RUEC0895 **************'
           DISPLAY '*                                    *'
           DISPLAY '* CK01-CODIGO-RETORNO : '
                      CK01-CODIGO-RETORNO    '           *'
           DISPLAY '* CK01-CODIGO-MENSAGEM: '
                      CK01-CODIGO-MENSAGEM         '     *'
           DISPLAY '*                                    *'
           DISPLAY '************** RUEC0895 **************'.

      *----------------------------------------------------------------*
       0120-99-FIM.   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ROTINA DE ATUALIZACAO DO MODULO CKRS0100                      *
      *----------------------------------------------------------------*
       0130-ATUALIZAR-CKRS0100             SECTION.
      *----------------------------------------------------------------*

           INITIALIZE CKRS01-INTERFACE.

           MOVE  'P'                       TO  CK01-FUNCAO.
           MOVE  'DB2 '                    TO  CK01-ID-DB2.

           MOVE  LENGTH                    OF  WRK-AREA-RESTART
                                           TO  CK01-TAM-AREA-RESTART.
           MOVE  WRK-AREA-RESTART          TO  CK01-AREA-RESTART.

           PERFORM 0110-CHAMAR-CKRS01.

      *----------------------------------------------------------------*
       0130-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ROTINA DE FINALIZACAO DO MODULO CKRS0100                      *
      *----------------------------------------------------------------*
       0140-FINALIZAR-CKRS0100             SECTION.
      *----------------------------------------------------------------*

           INITIALIZE CKRS01-INTERFACE.

           MOVE  'F'                       TO  CK01-FUNCAO.
           MOVE  'DB2'                     TO  CK01-ID-DB2.

           PERFORM 0110-CHAMAR-CKRS01.

      *----------------------------------------------------------------*
       0140-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *---------------------------------------------------------------*

           CALL 'BRAD7600'             USING WRK-BRAD7600.

           OPEN INPUT  ARQLIBER
                OUTPUT ARQOCORR.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

           PERFORM 2000-LER-ARQLIBER.

      *---------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *---------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-ARQLIBER.

           PERFORM 1130-TESTAR-FS-ARQOCORR.

      *---------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1110-TESTAR-FS-ARQLIBER         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-ARQLIBER         NOT EQUAL ZEROS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'ARQLIBER'         TO WRK-NOME-ARQUIVO
               MOVE  WRK-FS-ARQLIBER   TO WRK-FILE-STATUS
               MOVE  WRK-ERRO-ARQUIVO  TO ERR-TEXTO
               PERFORM  9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1130-TESTAR-FS-ARQOCORR         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-ARQOCORR         NOT EQUAL ZEROS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'ARQOCORR'         TO WRK-NOME-ARQUIVO
               MOVE  WRK-FS-ARQOCORR   TO WRK-FILE-STATUS
               MOVE  WRK-ERRO-ARQUIVO  TO ERR-TEXTO
               PERFORM  9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1130-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2000-LER-ARQLIBER               SECTION.
      *---------------------------------------------------------------*

CAT408     READ ARQLIBER               INTO WRK-REG-ARQLIBER.

           IF WRK-FS-ARQLIBER          EQUAL '10'
              MOVE HIGH-VALUES         TO WRK-CHAVE-ARQLIBER
              GO                       TO 2000-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1110-TESTAR-FS-ARQLIBER.

           MOVE WRK-LIB-CCONTR         TO WRK-CHV-LIB-CCONTR.

           ADD 1                       TO ACU-LIDOS-ARQLIBER.

      *---------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *---------------------------------------------------------------*

EG0117     PERFORM 3400-CHECAR-PRORROG-HSBC.
EG0117
EG0117     IF  WRK-LIB-IND             NUMERIC
EG0117     AND WRK-LIB-IND             EQUAL ZEROS
EG0117     AND WRK-DES-CCONTR          NOT NUMERIC
EG0117         PERFORM 2000-LER-ARQLIBER
EG0117         GO TO 3000-99-FIM
EG0117     END-IF.
EG0117
           PERFORM 3100-CHAMAR-RUEC8831

           PERFORM 0130-ATUALIZAR-CKRS0100.

           PERFORM 2000-LER-ARQLIBER.

      *---------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3100-CHAMAR-RUEC8831            SECTION.
      *---------------------------------------------------------------*

           INITIALIZE RUEC65-AREA.

           MOVE 'RUEC0895'             TO RUEC65-PGM-CHAMADOR
           MOVE WRK-LIB-CCONTR         TO RUEC65-CCONTR
           MOVE ZEROS                  TO RUEC65-NENVIO-RECOR
           MOVE 'F'                    TO RUEC65-FUNCAO
NAV506***
NAV506* -- MOVIMENTADO '*' PARA RUEC65-TP-DESCLASS PARA GRAVAR OS
NAV506* -- CAMPOS DE DESCLASSIFICACAO TOTAL E PARCIAL COM '*'
NAV506***
NAV506     MOVE '*'                    TO RUEC65-TP-DESCLASS

           CALL WRK-RUEC8831           USING RUEC65-AREA

           IF RUEC65-COD-RET           EQUAL 99
              MOVE RUEC65-SQLCA        TO SQLCA
              MOVE RUEC65-ERRO-AREA    TO ERRO-AREA
              PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF RUEC65-COD-RET           EQUAL ZEROS
              ADD  1                   TO ACU-RCOR-OK
           ELSE
              ADD  1                   TO ACU-RCOR-NOK
              MOVE RUEC65-COD-RET      TO WRK-8831-COD-RET
              MOVE RUEC65-MENSAGEM     TO WRK-8831-MENSAGEM
              MOVE WRK-LIB-CCONTR      TO WRK-ARQOCORR-CCONTR
              MOVE WRK-8831-MSG        TO WRK-ARQOCORR-MSG
              PERFORM 3900-GRAVAR-ARQOCORR
           END-IF.

      *---------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

EG0117*---------------------------------------------------------------*
EG0117 3400-CHECAR-PRORROG-HSBC        SECTION.
EG0117*---------------------------------------------------------------*
EG0117
EG0117     IF  WRK-ARQ-CCONTR          NOT NUMERIC
EG0117     OR  WRK-ARQ-CCONTR          EQUAL ZEROS
EG0117         GO TO 3400-99-FIM
EG0117     END-IF.
EG0117
EG0117     PERFORM VARYING IND-1       FROM 1 BY 1
EG0117       UNTIL IND-1               GREATER 30
EG0117         IF  WRK-LIB-DATA(IND-1) NUMERIC
EG0117         AND WRK-LIB-DATA(IND-1) NOT EQUAL ZEROS
EG0117             PERFORM 3410-ACESSAR-RUMOV002
EG0117         END-IF
EG0117     END-PERFORM.
EG0117
EG0117*---------------------------------------------------------------*
EG0117 3400-99-FIM.                    EXIT.
EG0117*---------------------------------------------------------------*
EG0117
EG0117*---------------------------------------------------------------*
EG0117 3410-ACESSAR-RUMOV002           SECTION.
EG0117*---------------------------------------------------------------*
EG0117
EG0117     MOVE WRK-ARQ-CCONTR         TO CCONTR            OF RUMOV002
EG0117     MOVE WRK-LIB-DATA(IND-1)    TO WRK-DATA-ISO
EG0117     MOVE WRK-DIA-ISO            TO WRK-DIA-DB2
EG0117     MOVE WRK-MES-ISO            TO WRK-MES-DB2
EG0117     MOVE WRK-ANO-ISO            TO WRK-ANO-DB2
EG0117     MOVE WRK-DATA-DB2           TO DCTBIL-ENCAR-MORA OF RUMOV002.
EG0117
EG0117     EXEC SQL
EG0117       SELECT COUNT(*)
EG0117       INTO   :WRK-COUNT
EG0117       FROM   DB2PRD.V01HIST_MOVTO
EG0117       WHERE  CCONTR             = :RUMOV002.CCONTR
EG0117       AND    DVCTO              < '10.10.2016'
EG0117       AND    DCTBIL_ENCAR_MORA  = :RUMOV002.DCTBIL-ENCAR-MORA
EG0117       AND    CTPO_EVNTO_RURAL   = 7070
EG0117     END-EXEC.
EG0117
EG0117     IF (SQLCODE                 NOT EQUAL ZEROS )
EG0117     OR (SQLWARN0                EQUAL 'W')
EG0117         MOVE 'DB2'              TO ERR-TIPO-ACESSO
EG0117         MOVE 'V01HIST_MOVTO'    TO ERR-DBD-TAB
EG0117         MOVE 'SELECT'           TO ERR-FUN-COMANDO
EG0117         MOVE SQLCODE            TO ERR-SQL-CODE
EG0117         MOVE '3410'             TO ERR-LOCAL
EG0117         MOVE SPACES             TO ERR-SEGM
EG0117         PERFORM 9999-ROTINA-ERRO
EG0117     END-IF.
EG0117
EG0117     IF  WRK-COUNT               EQUAL ZEROS
EG0117         GO TO 3410-99-FIM
EG0117     END-IF.
EG0117
EG0117     MOVE IND-1                  TO IND-2.
EG0117
EG0117     PERFORM VARYING IND-2       FROM IND-2 BY 1
EG0117       UNTIL IND-2               GREATER 29
EG0117        MOVE WRK-LIB-OCORRENCIAS (IND-2 + 1)
EG0117                                 TO WRK-LIB-OCORRENCIAS (IND-2)
EG0117     END-PERFORM.
EG0117
EG0117     MOVE ZEROS                  TO WRK-LIB-DATA(30).
EG0117     SUBTRACT 1                  FROM WRK-LIB-IND.
EG0117
EG0117*---------------------------------------------------------------*
EG0117 3410-99-FIM.                    EXIT.
EG0117*---------------------------------------------------------------*
EG0117
      *---------------------------------------------------------------*
       3900-GRAVAR-ARQOCORR            SECTION.
      *---------------------------------------------------------------*

           WRITE FD-ARQOCORR           FROM WRK-ARQOCORR.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 1130-TESTAR-FS-ARQOCORR.

           ADD 1                       TO ACU-GRAVS-ARQOCORR.

      *---------------------------------------------------------------*
       3900-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       9000-FINALIZAR                  SECTION.
      *---------------------------------------------------------------*

           MOVE ACU-LIDOS-ARQLIBER TO WRK-LIDOS-ARQLIBER-E.
           MOVE ACU-GRAVS-ARQOCORR TO WRK-GRAVS-ARQOCORR-E.
           MOVE ACU-RCOR-OK        TO WRK-RCOR-OK-E.
           MOVE ACU-RCOR-NOK       TO WRK-RCOR-NOK-E.

           DISPLAY '********************RUEC0895********************'.
           DISPLAY '*                                              *'.
           DISPLAY '*   TOTAL DE LIDOS EM ARQLIBER  = '
                      WRK-LIDOS-ARQLIBER-E '  *'.
           DISPLAY '*                                              *'.
           DISPLAY '*   TOTAL DE GRAVS. EM ARQOCORR = '
                      WRK-GRAVS-ARQOCORR-E '  *'.
           DISPLAY '*                                              *'.
           DISPLAY '*   TOTAL ENVIADOS RCOR OK      = '
                      WRK-RCOR-OK-E        '  *'.
           DISPLAY '*                                              *'.
           DISPLAY '*   TOTAL ENVIADOS RCOR NOK     = '
                      WRK-RCOR-NOK-E       '  *'.
           DISPLAY '*                                              *'.
           DISPLAY '********************RUEC0895********************'.

           CLOSE ARQLIBER
                 ARQOCORR.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

      *---------------------------------------------------------------*
       9000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *---------------------------------------------------------------*

           MOVE 'RUEC0895'             TO ERR-PGM.

           IF  ERR-TIPO-ACESSO         EQUAL  'APL'
               CALL 'BRAD7100'         USING  WRK-BATCH
                                              ERRO-AREA
           ELSE
               CALL 'BRAD7100'         USING  WRK-BATCH
                                              ERRO-AREA
                                              SQLCA
           END-IF.

           GOBACK.

      *---------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *---------------------------------------------------------------*
