      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT1785.
       AUTHOR.     WILLIAN CARARO.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT1785                                    *
      *    PROGRAMADOR.:   WILLIAN CARARO          - CPM PATO BRANCO   *
      *    ANALISTA CPM:   JOSSIANE CARLA GAZZONI  - CPM PATO BRANCO   *
      *    ANALISTA....:   MARIA V. DE S. TORQUATO - PROCWORK / GP.50  *
      *    DATA........:   06/07/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   EXCLUSAO NO ARQUIVO DAS FLEXIBILIZACOES POR *
      *      AGRUPAMENTO A VENCER NO PROXIMO  MES, QUE JA TENHA UM PE- *
      *      DIDO DEFERIDO.                                            *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                         INCLUDE/BOOK     *
      *                    FLEXAGPT                     I#GFCTT9       *
      *                    FLEXVENC                     I#GFCTT9       *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                        INCLUDE/BOOK     *
      *                    DB2PRD.PDIDO_FLEXZ_AGPTO     GFCTB0B2       *
      *                    DB2PRD.PDIDO_FLEXZ_CLI       GFCTB0B3       *
      *                    DB2PRD.PDIDO_FLEXZ_AG        GFCTB0B1       *
      *                    DB2PRD.PDIDO_FLEXZ_SGMTO     GFCTB0C0       *
      *                    DB2PRD.PDIDO_FLEXZ_PAB       GFCTB0B8       *
      *                    DB2PRD.PDIDO_FLEXZ_PSTAL     GFCTB0B9       *
      *                    DB2PRD.PDIDO_FLEXZ_EMPR      GFCTB0B6       *
      *                    DB2PRD.PDIDO_FLEXZ_MUN       GFCTB0B7       *
      *                    DB2PRD.PDIDO_FLEXZ_UF        GFCTB0C1       *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    POL7100C - AREA PARA TRATAMENTO DE ERROS PELA POOL7100.     *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    POOL1050 - MODULO PARA FAZER CONEXAO COM DB2.               *
      *    POOL7100 - TRATAMENTO DE ERROS PADRAO BRADESCO.             *
      *================================================================*
080828*----------------------------------------------------------------*
080828*    ULTIMA ALTERACAO - VINICIUS MADUREIRA                       *
080828*    INCLUSAO DE CAMPOS NA TABELA GFCTB0B2 E GFCTB049            *
080828*    E ALTERACAO DA INC GFCTT3                                   *
080828*================================================================*

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

           SELECT FLEXAGPT   ASSIGN    TO UT-S-FLEXAGPT
           FILE STATUS                 IS WRK-FS-FLEXAGPT.

           SELECT FLEXVENC   ASSIGN    TO UT-S-FLEXVENC
           FILE STATUS                 IS WRK-FS-FLEXVENC.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT : FLEXIBILIZACOES A VENCER                            *
      *            ORG. SEQUENCIAL     -   LRECL = 300                 *
      *----------------------------------------------------------------*

       FD  FLEXAGPT
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-ENTRADA              PIC  X(300).

      *----------------------------------------------------------------*
      *    OUTPUT: FLEXIBILIZACOES A VENCER                            *
      *            ORG. SEQUENCIAL     -   LRECL = 300                 *
      *----------------------------------------------------------------*

       FD  FLEXVENC
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-SAIDA                PIC  X(300).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* INICIO DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA DE ACUMULADORES *'.
      *----------------------------------------------------------------*

       77  ACU-LIDOS-FLEXAGPT          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-RENOV-FLEXAGPT          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-FLEXVENC          PIC  9(009) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA DE TESTES DE FILE STATUS *'.
      *----------------------------------------------------------------*

       77  WRK-FS-FLEXAGPT             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-FLEXVENC             PIC  X(002)         VALUE SPACES.

       77  WRK-ABERTURA                PIC  X(013)         VALUE
           ' NA ABERTURA '.
       77  WRK-LEITURA                 PIC  X(013)         VALUE
           ' NA  LEITURA '.
       77  WRK-GRAVACAO                PIC  X(013)         VALUE
           ' NA GRAVACAO '.
       77  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA VARIAVEIS AUXILIARES *'.
      *----------------------------------------------------------------*

       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.
       77  WRK-MASCARA                 PIC  Z.ZZZ.ZZZ.ZZ9  VALUE ZEROS.

LFM    01  WRK-DINIC-FLEXZ-T9         PIC  X(010)       VALUE SPACES.
LFM    01  FILLER                      REDEFINES   WRK-DINIC-FLEXZ-T9.
LFM        05  WRK-DIA-DINIC-T9        PIC  9(002).
LFM        05  FILLER                  PIC  X(001).
LFM        05  WRK-MES-DINIC-T9        PIC  9(002).
LFM        05  FILLER                  PIC  X(001).
LFM        05  WRK-ANO-DINIC-T9        PIC  9(004).
LFM    01  WRK-DINIC-FLEXZ-T9-INV      PIC  9(008)         VALUE ZEROS.
LFM    01  FILLER                    REDEFINES   WRK-DINIC-FLEXZ-T9-INV.
LFM        05  WRK-ANO-DINIC-T9-INV    PIC  9(004).
LFM        05  WRK-MES-DINIC-T9-INV    PIC  9(002).
LFM        05  WRK-DIA-DINIC-T9-INV    PIC  9(002).
LFM
LFM    01  WRK-DINIC-FLEXZ-AGPTO         PIC  X(010)       VALUE SPACES.
LFM    01  FILLER                      REDEFINES WRK-DINIC-FLEXZ-AGPTO.
LFM        05  WRK-DIA-DINIC           PIC  9(002).
LFM        05  FILLER                  PIC  X(001).
LFM        05  WRK-MES-DINIC           PIC  9(002).
LFM        05  FILLER                  PIC  X(001).
LFM        05  WRK-ANO-DINIC           PIC  9(004).
LFM    01  WRK-DINIC-FLEXZ-AGPTO-INV      PIC  9(008)    VALUE ZEROS.
LFM    01  FILLER             REDEFINES   WRK-DINIC-FLEXZ-AGPTO-INV.
LFM        05  WRK-ANO-DINIC-INV       PIC  9(004).
LFM        05  WRK-MES-DINIC-INV       PIC  9(002).
LFM        05  WRK-DIA-DINIC-INV       PIC  9(002).

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA MENSAGENS *'.
      *---------------------------------------------------------------*

       01  WRK-MENSAGENS.
           05  WRK-MSG001              PIC  X(075)         VALUE
               '** ARQUIVO FLEXAGPT ESTA VAZIO, PROGRAMA CANCELADO **'.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* MENSAGEM DE ERRO DE FILE-STATUS *'.
      *---------------------------------------------------------------*

       01  WRK-ERRO-TEXTO.
           05  FILLER                  PIC  X(008)         VALUE
               '** ERRO '.
           05  WRK-OPERACAO            PIC  X(013)         VALUE SPACES.
           05  FILLER                  PIC  X(012)         VALUE
               ' DO ARQUIVO '.
           05  WRK-NOME-ARQ            PIC  X(008)         VALUE SPACES.
           05  FILLER                  PIC  X(017)         VALUE
               ' - FILE STATUS = '.
           05  WRK-FILE-STATUS         PIC  X(002)         VALUE SPACES.
           05  FILLER                  PIC  X(003)         VALUE ' **'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA ARQUIVOS *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTT9'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA TRATAMENTO DE ERRO *'.
      *----------------------------------------------------------------*

       COPY POL7100C.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA DB2 *'.
      *----------------------------------------------------------------*

           EXEC SQL
             INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0B2
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0B3
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0B1
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0C0
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0B8
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0B9
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0B6
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0B7
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0C1
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0M7
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* FIM DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      ******************************************************************
      *    CHAMA AS ROTINAS A SEREM PROCESSADAS                        *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-VERIFICAR-VAZIO.

           PERFORM 3000-PROCESSAR      UNTIL
                   WRK-FS-FLEXAGPT     EQUAL '10'.

           PERFORM 4000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    CONEXAO COM DB2 E ABERTURA DE ARQUIVOS                      *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL1050'.

           OPEN INPUT  FLEXAGPT
                OUTPUT FLEXVENC.

           MOVE  WRK-ABERTURA          TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTE DE FILE STATUS DOS ARQUIVOS                           *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-FLEXAGPT.

           PERFORM 1120-TESTAR-FS-FLEXVENC.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTE DE FILE STATUS DO ARQUIVO FLEXAGPT                    *
      ******************************************************************
      *----------------------------------------------------------------*
       1110-TESTAR-FS-FLEXAGPT         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-FLEXAGPT         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-FLEXAGPT    TO WRK-FILE-STATUS
               MOVE 'FLEXAGPT'         TO WRK-NOME-ARQ
               MOVE WRK-ERRO-TEXTO     TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTE DE FILE STATUS DO ARQUIVO FLEXVENC                    *
      ******************************************************************
      *----------------------------------------------------------------*
       1120-TESTAR-FS-FLEXVENC         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-FLEXVENC         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-FLEXVENC    TO WRK-FILE-STATUS
               MOVE 'FLEXVENC'         TO WRK-NOME-ARQ
               MOVE WRK-ERRO-TEXTO     TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICA SE O ARQUIVO DE ENTRADA ESTA VAZIO                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-LER-FLEXAGPT.

           IF  WRK-FS-FLEXAGPT         EQUAL '10'
PROC****       PERFORM 2200-CANCELAR-PROGRAMA
               PERFORM 2200-DISPLAY-VAZIO
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    LEITURA DO ARQUIVO FLEXAGPT                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-LER-FLEXAGPT               SECTION.
      *----------------------------------------------------------------*

           READ FLEXAGPT               INTO GFCTT9-REGISTRO.

           IF  WRK-FS-FLEXAGPT         EQUAL '10'
               GO TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1110-TESTAR-FS-FLEXAGPT.

           ADD 1                       TO ACU-LIDOS-FLEXAGPT.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    EMITE DISPLAY DE ARQUIVO DE ENTRADA VAZIO E CANCELA O PRG   *
      ******************************************************************
      *----------------------------------------------------------------*
PROC***2200-CANCELAR-PROGRAMA          SECTION.
       2200-DISPLAY-VAZIO              SECTION.
      *----------------------------------------------------------------*

           DISPLAY '*-----------------------------------------------*'.
           DISPLAY '*               PROGRAMA GFCT1785               *'.
           DISPLAY '*               -----------------               *'.
           DISPLAY '*                                               *'.
           DISPLAY '*           ARQUIVO DE FLEXAGPT VAZIO           *'.
           DISPLAY '*                                               *'.
PROC****   DISPLAY '*            PROCESSAMENTO CANCELADO            *'.
           DISPLAY '*-----------------------------------------------*'.
PROC****   MOVE 'APL'                  TO ERR-TIPO-ACESSO.
PROC****   MOVE WRK-MSG001             TO ERR-TEXTO.
PROC****   PERFORM 9999-PROCESSAR-ROTINA-ERRO.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCESSAMENTO PRINCIPAL DO PROGRAMA                         *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           MOVE FD-REG-ENTRADA         TO GFCTT9-REGISTRO.

           EVALUATE GFCTT9-CAGPTO-CTA

               WHEN 4
                   PERFORM 3100-VERIFICAR-AGRUP-CLIENTE

               WHEN 14
                   PERFORM 3200-VERIFICAR-AGRUP-AGENCIA

               WHEN 9
                   PERFORM 3300-VERIFICAR-AGRUP-SEGMENTO

               WHEN 15
                   PERFORM 3400-VERIFICAR-AGRUP-PAB

               WHEN 16
                   PERFORM 3500-VERIFICAR-AGRUP-EMPRESA

               WHEN 17
                   PERFORM 3600-VERIFICAR-AGRUP-POSTAL

               WHEN 18
                   PERFORM 3700-VERIFICAR-AGRUP-MUN

               WHEN 19
                   PERFORM 3800-VERIFICAR-AGRUP-UF

LFM            WHEN 20
LFM                PERFORM 3600-VERIFICAR-AGRUP-POSTAL
LFM
LFM            WHEN 26
LFM                PERFORM 3910-VERIFICAR-AGRUP-RAZAO
LFM
080828         WHEN OTHER
080828             PERFORM 3900-VERIFICAR-OUTROS-AGRUP
           END-EVALUATE.

           PERFORM 2100-LER-FLEXAGPT.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICA AGRUPAMENTO CLIENTE                                *
      ******************************************************************
      *----------------------------------------------------------------*
       3100-VERIFICAR-AGRUP-CLIENTE    SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTT9-CAGPTO-CTA      TO CAGPTO-CTA        OF GFCTB0B2.
           MOVE GFCTT9-CSERVC-TARIF    TO CSERVC-TARIF      OF GFCTB0B2.
           MOVE GFCTT9-DINIC-FLEXZ     TO DINIC-FLEXZ-AGPTO OF GFCTB0B2.
           MOVE 02                     TO CSIT-SOLTC-FLEXZ  OF GFCTB0B2.
           MOVE ZEROS                  TO CFUNC-EXCL-SIST   OF GFCTB0B2
                                          RJUSTF-SOLTC-FLEXZ-LEN
                                                            OF GFCTB0B2
                                          RJUSTF-PAREC-FLEXZ-LEN
                                                            OF GFCTB0B2.
           MOVE SPACES                 TO RJUSTF-SOLTC-FLEXZ-TEXT
                                                            OF GFCTB0B2
                                          RJUSTF-PAREC-FLEXZ-TEXT
                                                            OF GFCTB0B2.
ST2506*    MOVE GFCTT9-CCGC-AGP04      TO CCGC-CPF          OF GFCTB0B3.
ST2506*    MOVE GFCTT9-CFLIAL-AGP04    TO CFLIAL-CGC        OF GFCTB0B3.
ST2506*    MOVE GFCTT9-CCTRL-AGP04     TO CCTRL-CPF-CGC     OF GFCTB0B3.
ST250      MOVE GFCTT9-CCGC-AGP04      TO CCGC-CPF-ST       OF GFCTB0B3.
ST2506     MOVE GFCTT9-CFLIAL-AGP04    TO CFLIAL-CGC-ST     OF GFCTB0B3.
ST2506     MOVE GFCTT9-CCTRL-AGP04     TO CCTRL-CPF-CGC-ST  OF GFCTB0B3.

           EXEC SQL
             SELECT
                   A.CSIT_SOLTC_FLEXZ
             INTO
                   :GFCTB0B2.CSIT-SOLTC-FLEXZ
             FROM  DB2PRD.PDIDO_FLEXZ_AGPTO   A,
                   DB2PRD.PDIDO_FLEXZ_CLI     B
             WHERE
                   A.CSERVC_TARIF      =  B.CSERVC_TARIF             AND
                   A.CAGPTO_CTA        =  B.CAGPTO_CTA               AND
                   A.CSERVC_TARIF      = :GFCTB0B2.CSERVC-TARIF      AND
                   A.CAGPTO_CTA        = :GFCTB0B2.CAGPTO-CTA        AND
                   A.DINIC_FLEXZ_AGPTO > :GFCTB0B2.DINIC-FLEXZ-AGPTO AND
ST2506*            B.CCGC_CPF          = :GFCTB0B3.CCGC-CPF          AND
ST2506*            B.CFLIAL_CGC        = :GFCTB0B3.CFLIAL-CGC        AND
ST2506*            B.CCTRL_CPF_CGC     = :GFCTB0B3.CCTRL-CPF-CGC     AND
ST2506             B.CCGC_CPF_ST       = :GFCTB0B3.CCGC-CPF-ST       AND
ST2506             B.CFLIAL_CGC_ST     = :GFCTB0B3.CFLIAL-CGC-ST     AND
ST2506             B.CCTRL_CPF_CGC_ST  = :GFCTB0B3.CCTRL-CPF-CGC-ST  AND
                   A.CSIT_SOLTC_FLEXZ  = :GFCTB0B2.CSIT-SOLTC-FLEXZ  AND
                   A.CFUNC_EXCL_SIST   = :GFCTB0B2.CFUNC-EXCL-SIST   AND
                   B.HIDTFD_FLEXZ_AGPTO   IN
                     (SELECT MAX(B.HIDTFD_FLEXZ_AGPTO)
                      FROM   DB2PRD.PDIDO_FLEXZ_AGPTO  A,
                             DB2PRD.PDIDO_FLEXZ_CLI    B
                      WHERE
                               A.CSERVC_TARIF  =  B.CSERVC_TARIF
                         AND   A.CAGPTO_CTA    =  B.CAGPTO_CTA
                         AND   A.CSERVC_TARIF  = :GFCTB0B2.CSERVC-TARIF
                         AND   A.CAGPTO_CTA    = :GFCTB0B2.CAGPTO-CTA
                         AND   A.DINIC_FLEXZ_AGPTO >
                                            :GFCTB0B2.DINIC-FLEXZ-AGPTO
ST2506*                  AND   B.CCGC_CPF      = :GFCTB0B3.CCGC-CPF
ST2506*                  AND   B.CFLIAL_CGC    = :GFCTB0B3.CFLIAL-CGC
ST2506*                  AND   B.CCTRL_CPF_CGC = :GFCTB0B3.CCTRL-CPF-CGC
ST2506                   AND   B.CCGC_CPF_ST   = :GFCTB0B3.CCGC-CPF-ST
ST2506                   AND   B.CFLIAL_CGC_ST = :GFCTB0B3.CFLIAL-CGC-ST
ST2506                   AND   B.CCTRL_CPF_CGC_ST 
                                            = :GFCTB0B3.CCTRL-CPF-CGC-ST
                         AND   A.CSIT_SOLTC_FLEXZ  =
                                            :GFCTB0B2.CSIT-SOLTC-FLEXZ
                         AND   A.CFUNC_EXCL_SIST   =
                                            :GFCTB0B2.CFUNC-EXCL-SIST)
           END-EXEC.

KVB        IF (SQLCODE        NOT EQUAL ZEROS AND +100 AND -811) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_AGPTO' TO ERR-DBD-TAB
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0010'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               WRITE FD-REG-SAIDA      FROM GFCTT9-REGISTRO
               MOVE WRK-GRAVACAO       TO WRK-OPERACAO
               PERFORM 1120-TESTAR-FS-FLEXVENC
               ADD 1                   TO ACU-GRAVA-FLEXVENC
           ELSE
               ADD 1                   TO ACU-RENOV-FLEXAGPT
           END-IF.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICA AGRUPAMENTO AGENCIA                                *
      ******************************************************************
      *----------------------------------------------------------------*
       3200-VERIFICAR-AGRUP-AGENCIA    SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTT9-CAGPTO-CTA      TO CAGPTO-CTA        OF GFCTB0B2.
           MOVE GFCTT9-CSERVC-TARIF    TO CSERVC-TARIF      OF GFCTB0B2.
           MOVE GFCTT9-DINIC-FLEXZ     TO DINIC-FLEXZ-AGPTO OF GFCTB0B2.
           MOVE 02                     TO CSIT-SOLTC-FLEXZ  OF GFCTB0B2.
           MOVE ZEROS                  TO CFUNC-EXCL-SIST   OF GFCTB0B2
                                          RJUSTF-SOLTC-FLEXZ-LEN
                                                            OF GFCTB0B2
                                          RJUSTF-PAREC-FLEXZ-LEN
                                                            OF GFCTB0B2.
           MOVE SPACES                 TO RJUSTF-SOLTC-FLEXZ-TEXT
                                                            OF GFCTB0B2
                                          RJUSTF-PAREC-FLEXZ-TEXT
                                                            OF GFCTB0B2.
           MOVE GFCTT9-CEMPR-AGP14     TO CEMPR-INC         OF GFCTB0B1.
           MOVE GFCTT9-CDEPDC-AGP14    TO CDEPDC            OF GFCTB0B1.

           EXEC SQL
             SELECT
                   A.CSIT_SOLTC_FLEXZ
             INTO
                   :GFCTB0B2.CSIT-SOLTC-FLEXZ
             FROM  DB2PRD.PDIDO_FLEXZ_AGPTO   A,
                   DB2PRD.PDIDO_FLEXZ_AG      B
             WHERE
                   A.CSERVC_TARIF      =  B.CSERVC_TARIF             AND
                   A.CAGPTO_CTA        =  B.CAGPTO_CTA               AND
                   A.CSERVC_TARIF      = :GFCTB0B2.CSERVC-TARIF      AND
                   A.CAGPTO_CTA        = :GFCTB0B2.CAGPTO-CTA        AND
                   A.DINIC_FLEXZ_AGPTO > :GFCTB0B2.DINIC-FLEXZ-AGPTO AND
                   B.CEMPR_INC         = :GFCTB0B1.CEMPR-INC         AND
                   B.CDEPDC            = :GFCTB0B1.CDEPDC            AND
                   A.CSIT_SOLTC_FLEXZ  = :GFCTB0B2.CSIT-SOLTC-FLEXZ  AND
                   A.CFUNC_EXCL_SIST   = :GFCTB0B2.CFUNC-EXCL-SIST   AND
                   B.HIDTFD_FLEXZ_AGPTO   IN
                     (SELECT MAX(B.HIDTFD_FLEXZ_AGPTO)
                      FROM   DB2PRD.PDIDO_FLEXZ_AGPTO  A,
                             DB2PRD.PDIDO_FLEXZ_AG     B
                      WHERE
                               A.CSERVC_TARIF  =  B.CSERVC_TARIF
                         AND   A.CAGPTO_CTA    =  B.CAGPTO_CTA
                         AND   A.CSERVC_TARIF  = :GFCTB0B2.CSERVC-TARIF
                         AND   A.CAGPTO_CTA    = :GFCTB0B2.CAGPTO-CTA
                         AND   A.DINIC_FLEXZ_AGPTO >
                                            :GFCTB0B2.DINIC-FLEXZ-AGPTO
                         AND   B.CEMPR_INC     = :GFCTB0B1.CEMPR-INC
                         AND   B.CDEPDC        = :GFCTB0B1.CDEPDC
                         AND   A.CSIT_SOLTC_FLEXZ  =
                                            :GFCTB0B2.CSIT-SOLTC-FLEXZ
                         AND   A.CFUNC_EXCL_SIST   =
                                            :GFCTB0B2.CFUNC-EXCL-SIST)
           END-EXEC.

KVB        IF (SQLCODE       NOT EQUAL ZEROS AND +100 AND -811) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_AGPTO' TO ERR-DBD-TAB
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0020'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               WRITE FD-REG-SAIDA      FROM GFCTT9-REGISTRO
               MOVE WRK-GRAVACAO       TO WRK-OPERACAO
               PERFORM 1120-TESTAR-FS-FLEXVENC
               ADD 1                   TO ACU-GRAVA-FLEXVENC
           ELSE
               ADD 1                   TO ACU-RENOV-FLEXAGPT
           END-IF.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICA AGRUPAMENTO SEGMENTO                               *
      ******************************************************************
      *----------------------------------------------------------------*
       3300-VERIFICAR-AGRUP-SEGMENTO   SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTT9-CAGPTO-CTA      TO CAGPTO-CTA        OF GFCTB0B2.
           MOVE GFCTT9-CSERVC-TARIF    TO CSERVC-TARIF      OF GFCTB0B2.
           MOVE GFCTT9-DINIC-FLEXZ     TO DINIC-FLEXZ-AGPTO OF GFCTB0B2.
           MOVE 02                     TO CSIT-SOLTC-FLEXZ  OF GFCTB0B2.
           MOVE ZEROS                  TO CFUNC-EXCL-SIST   OF GFCTB0B2
                                          RJUSTF-SOLTC-FLEXZ-LEN
                                                            OF GFCTB0B2
                                          RJUSTF-PAREC-FLEXZ-LEN
                                                            OF GFCTB0B2.
           MOVE SPACES                 TO RJUSTF-SOLTC-FLEXZ-TEXT
                                                            OF GFCTB0B2
                                          RJUSTF-PAREC-FLEXZ-TEXT
                                                            OF GFCTB0B2.
           MOVE GFCTT9-CSGMTO-AGP09    TO CSGMTO-GSTAO-TARIF
                                                            OF GFCTB0C0.

           EXEC SQL
             SELECT
                   A.CSIT_SOLTC_FLEXZ
             INTO
                   :GFCTB0B2.CSIT-SOLTC-FLEXZ
             FROM  DB2PRD.PDIDO_FLEXZ_AGPTO   A,
                   DB2PRD.PDIDO_FLEXZ_SGMTO   B
             WHERE
                   A.CSERVC_TARIF       =  B.CSERVC_TARIF            AND
                   A.CAGPTO_CTA         =  B.CAGPTO_CTA              AND
                   A.CSERVC_TARIF       = :GFCTB0B2.CSERVC-TARIF     AND
                   A.CAGPTO_CTA         = :GFCTB0B2.CAGPTO-CTA       AND
                   A.DINIC_FLEXZ_AGPTO  > :GFCTB0B2.DINIC-FLEXZ-AGPTO
              AND  B.CSGMTO_GSTAO_TARIF = :GFCTB0C0.CSGMTO-GSTAO-TARIF
              AND  A.CSIT_SOLTC_FLEXZ   = :GFCTB0B2.CSIT-SOLTC-FLEXZ AND
                   A.CFUNC_EXCL_SIST    = :GFCTB0B2.CFUNC-EXCL-SIST  AND
                   B.HIDTFD_FLEXZ_AGPTO   IN
                     (SELECT MAX(B.HIDTFD_FLEXZ_AGPTO)
                      FROM   DB2PRD.PDIDO_FLEXZ_AGPTO  A,
                             DB2PRD.PDIDO_FLEXZ_SGMTO  B
                      WHERE
                               A.CSERVC_TARIF  =  B.CSERVC_TARIF
                         AND   A.CAGPTO_CTA    =  B.CAGPTO_CTA
                         AND   A.CSERVC_TARIF  = :GFCTB0B2.CSERVC-TARIF
                         AND   A.CAGPTO_CTA    = :GFCTB0B2.CAGPTO-CTA
                         AND   A.DINIC_FLEXZ_AGPTO >
                                            :GFCTB0B2.DINIC-FLEXZ-AGPTO
                         AND   B.CSGMTO_GSTAO_TARIF =
                                            :GFCTB0C0.CSGMTO-GSTAO-TARIF
                         AND   A.CSIT_SOLTC_FLEXZ  =
                                            :GFCTB0B2.CSIT-SOLTC-FLEXZ
                         AND   A.CFUNC_EXCL_SIST   =
                                            :GFCTB0B2.CFUNC-EXCL-SIST)
           END-EXEC.

KVB        IF (SQLCODE       NOT EQUAL ZEROS AND +100 AND -811) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_AGPTO' TO ERR-DBD-TAB
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0030'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               WRITE FD-REG-SAIDA      FROM GFCTT9-REGISTRO
               MOVE WRK-GRAVACAO       TO WRK-OPERACAO
               PERFORM 1120-TESTAR-FS-FLEXVENC
               ADD 1                   TO ACU-GRAVA-FLEXVENC
           ELSE
               ADD 1                   TO ACU-RENOV-FLEXAGPT
           END-IF.

      *----------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICA AGRUPAMENTO PAB                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       3400-VERIFICAR-AGRUP-PAB        SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTT9-CAGPTO-CTA      TO CAGPTO-CTA        OF GFCTB0B2.
           MOVE GFCTT9-CSERVC-TARIF    TO CSERVC-TARIF      OF GFCTB0B2.
           MOVE GFCTT9-DINIC-FLEXZ     TO DINIC-FLEXZ-AGPTO OF GFCTB0B2.
           MOVE 02                     TO CSIT-SOLTC-FLEXZ  OF GFCTB0B2.
           MOVE ZEROS                  TO CFUNC-EXCL-SIST   OF GFCTB0B2
                                          RJUSTF-SOLTC-FLEXZ-LEN
                                                            OF GFCTB0B2
                                          RJUSTF-PAREC-FLEXZ-LEN
                                                            OF GFCTB0B2.
           MOVE SPACES                 TO RJUSTF-SOLTC-FLEXZ-TEXT
                                                            OF GFCTB0B2
                                          RJUSTF-PAREC-FLEXZ-TEXT
                                                            OF GFCTB0B2.
           MOVE GFCTT9-CEMPR-AGP15     TO CEMPR-INC         OF GFCTB0B8.
           MOVE GFCTT9-CDEPDC-AGP15    TO CDEPDC            OF GFCTB0B8.
           MOVE GFCTT9-CPOSTO-AGP15    TO CPOSTO-SERVC      OF GFCTB0B8.

           EXEC SQL
             SELECT
                   A.CSIT_SOLTC_FLEXZ
             INTO
                   :GFCTB0B2.CSIT-SOLTC-FLEXZ
             FROM  DB2PRD.PDIDO_FLEXZ_AGPTO   A,
                   DB2PRD.PDIDO_FLEXZ_PAB     B
             WHERE
                   A.CSERVC_TARIF      =  B.CSERVC_TARIF             AND
                   A.CAGPTO_CTA        =  B.CAGPTO_CTA               AND
                   A.CSERVC_TARIF      = :GFCTB0B2.CSERVC-TARIF      AND
                   A.CAGPTO_CTA        = :GFCTB0B2.CAGPTO-CTA        AND
                   A.DINIC_FLEXZ_AGPTO > :GFCTB0B2.DINIC-FLEXZ-AGPTO AND
                   B.CEMPR_INC         = :GFCTB0B8.CEMPR-INC         AND
                   B.CDEPDC            = :GFCTB0B8.CDEPDC            AND
                   B.CPOSTO_SERVC      = :GFCTB0B8.CPOSTO-SERVC      AND
                   A.CSIT_SOLTC_FLEXZ  = :GFCTB0B2.CSIT-SOLTC-FLEXZ  AND
                   A.CFUNC_EXCL_SIST   = :GFCTB0B2.CFUNC-EXCL-SIST   AND
                   B.HIDTFD_FLEXZ_AGPTO   IN
                     (SELECT MAX(B.HIDTFD_FLEXZ_AGPTO)
                      FROM   DB2PRD.PDIDO_FLEXZ_AGPTO  A,
                             DB2PRD.PDIDO_FLEXZ_PAB    B
                      WHERE
                               A.CSERVC_TARIF  =  B.CSERVC_TARIF
                         AND   A.CAGPTO_CTA    =  B.CAGPTO_CTA
                         AND   A.CSERVC_TARIF  = :GFCTB0B2.CSERVC-TARIF
                         AND   A.CAGPTO_CTA    = :GFCTB0B2.CAGPTO-CTA
                         AND   A.DINIC_FLEXZ_AGPTO >
                                             :GFCTB0B2.DINIC-FLEXZ-AGPTO
                         AND   B.CEMPR_INC     = :GFCTB0B8.CEMPR-INC
                         AND   B.CDEPDC        = :GFCTB0B8.CDEPDC
                         AND   B.CPOSTO_SERVC  = :GFCTB0B8.CPOSTO-SERVC
                         AND   A.CSIT_SOLTC_FLEXZ  =
                                             :GFCTB0B2.CSIT-SOLTC-FLEXZ
                         AND   A.CFUNC_EXCL_SIST   =
                                             :GFCTB0B2.CFUNC-EXCL-SIST)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100
CPMAFG                                                 AND -811) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_AGPTO' TO ERR-DBD-TAB
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0040'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               WRITE FD-REG-SAIDA      FROM GFCTT9-REGISTRO
               MOVE WRK-GRAVACAO       TO WRK-OPERACAO
               PERFORM 1120-TESTAR-FS-FLEXVENC
               ADD 1                   TO ACU-GRAVA-FLEXVENC
           ELSE
               ADD 1                   TO ACU-RENOV-FLEXAGPT
           END-IF.

      *----------------------------------------------------------------*
       3400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICA AGRUPAMENTO EMPRESA                                *
      ******************************************************************
      *----------------------------------------------------------------*
       3500-VERIFICAR-AGRUP-EMPRESA    SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTT9-CAGPTO-CTA      TO CAGPTO-CTA        OF GFCTB0B2.
           MOVE GFCTT9-CSERVC-TARIF    TO CSERVC-TARIF      OF GFCTB0B2.
           MOVE GFCTT9-DINIC-FLEXZ     TO DINIC-FLEXZ-AGPTO OF GFCTB0B2.
           MOVE 02                     TO CSIT-SOLTC-FLEXZ  OF GFCTB0B2.
           MOVE ZEROS                  TO CFUNC-EXCL-SIST   OF GFCTB0B2
                                          RJUSTF-SOLTC-FLEXZ-LEN
                                                            OF GFCTB0B2
                                          RJUSTF-PAREC-FLEXZ-LEN
                                                            OF GFCTB0B2.
           MOVE SPACES                 TO RJUSTF-SOLTC-FLEXZ-TEXT
                                                            OF GFCTB0B2
                                          RJUSTF-PAREC-FLEXZ-TEXT
                                                            OF GFCTB0B2.
           MOVE GFCTT9-CJUNC-AGP16     TO CJUNC-DEPDC       OF GFCTB0B6.
           MOVE GFCTT9-CCTA-AGP16      TO CCTA-CLI          OF GFCTB0B6.

           EXEC SQL
             SELECT
                   A.CSIT_SOLTC_FLEXZ
             INTO
                   :GFCTB0B2.CSIT-SOLTC-FLEXZ
             FROM  DB2PRD.PDIDO_FLEXZ_AGPTO   A,
                   DB2PRD.PDIDO_FLEXZ_EMPR    B
             WHERE
                   A.CSERVC_TARIF      =  B.CSERVC_TARIF             AND
                   A.CAGPTO_CTA        =  B.CAGPTO_CTA               AND
                   A.CSERVC_TARIF      = :GFCTB0B2.CSERVC-TARIF      AND
                   A.CAGPTO_CTA        = :GFCTB0B2.CAGPTO-CTA        AND
                   A.DINIC_FLEXZ_AGPTO > :GFCTB0B2.DINIC-FLEXZ-AGPTO AND
                   B.CJUNC_DEPDC       = :GFCTB0B6.CJUNC-DEPDC       AND
                   B.CCTA_CLI          = :GFCTB0B6.CCTA-CLI          AND
                   A.CSIT_SOLTC_FLEXZ  = :GFCTB0B2.CSIT-SOLTC-FLEXZ  AND
                   A.CFUNC_EXCL_SIST   = :GFCTB0B2.CFUNC-EXCL-SIST   AND
                   B.HIDTFD_FLEXZ_AGPTO   IN
                     (SELECT MAX(B.HIDTFD_FLEXZ_AGPTO)
                      FROM   DB2PRD.PDIDO_FLEXZ_AGPTO  A,
                             DB2PRD.PDIDO_FLEXZ_EMPR   B
                      WHERE
                               A.CSERVC_TARIF  =  B.CSERVC_TARIF
                         AND   A.CAGPTO_CTA    =  B.CAGPTO_CTA
                         AND   A.CSERVC_TARIF  = :GFCTB0B2.CSERVC-TARIF
                         AND   A.CAGPTO_CTA    = :GFCTB0B2.CAGPTO-CTA
                         AND   A.DINIC_FLEXZ_AGPTO >
                                             :GFCTB0B2.DINIC-FLEXZ-AGPTO
                         AND   B.CJUNC_DEPDC   = :GFCTB0B6.CJUNC-DEPDC
                         AND   B.CCTA_CLI      = :GFCTB0B6.CCTA-CLI
                         AND   A.CSIT_SOLTC_FLEXZ  =
                                             :GFCTB0B2.CSIT-SOLTC-FLEXZ
                         AND   A.CFUNC_EXCL_SIST   =
                                             :GFCTB0B2.CFUNC-EXCL-SIST)
           END-EXEC.

KVB        IF (SQLCODE       NOT EQUAL ZEROS AND +100 AND -811) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_AGPTO' TO ERR-DBD-TAB
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0050'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               WRITE FD-REG-SAIDA      FROM GFCTT9-REGISTRO
               MOVE WRK-GRAVACAO       TO WRK-OPERACAO
               PERFORM 1120-TESTAR-FS-FLEXVENC
               ADD 1                   TO ACU-GRAVA-FLEXVENC
           ELSE
               ADD 1                   TO ACU-RENOV-FLEXAGPT
           END-IF.

      *----------------------------------------------------------------*
       3500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICA AGRUPAMENTO POSTAL                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       3600-VERIFICAR-AGRUP-POSTAL     SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTT9-CAGPTO-CTA      TO CAGPTO-CTA        OF GFCTB0B2.
           MOVE GFCTT9-CSERVC-TARIF    TO CSERVC-TARIF      OF GFCTB0B2.
           MOVE GFCTT9-DINIC-FLEXZ     TO DINIC-FLEXZ-AGPTO OF GFCTB0B2.
           MOVE 02                     TO CSIT-SOLTC-FLEXZ  OF GFCTB0B2.
           MOVE ZEROS                  TO CFUNC-EXCL-SIST   OF GFCTB0B2
                                          RJUSTF-SOLTC-FLEXZ-LEN
                                                            OF GFCTB0B2
                                          RJUSTF-PAREC-FLEXZ-LEN
                                                            OF GFCTB0B2.
           MOVE SPACES                 TO RJUSTF-SOLTC-FLEXZ-TEXT
                                                            OF GFCTB0B2
                                          RJUSTF-PAREC-FLEXZ-TEXT
                                                            OF GFCTB0B2.
           MOVE GFCTT9-CEMPR-AGP17     TO CEMPR-INC         OF GFCTB0B9.
           MOVE GFCTT9-CDEPDC-AGP17    TO CDEPDC            OF GFCTB0B9.
           MOVE GFCTT9-CPOSTO-AGP17    TO CPOSTO-SERVC      OF GFCTB0B9.

           EXEC SQL
             SELECT
                   A.CSIT_SOLTC_FLEXZ
             INTO
                   :GFCTB0B2.CSIT-SOLTC-FLEXZ
             FROM  DB2PRD.PDIDO_FLEXZ_AGPTO   A,
                   DB2PRD.PDIDO_FLEXZ_PSTAL   B
             WHERE
                   A.CSERVC_TARIF      =  B.CSERVC_TARIF             AND
                   A.CAGPTO_CTA        =  B.CAGPTO_CTA               AND
                   A.CSERVC_TARIF      = :GFCTB0B2.CSERVC-TARIF      AND
                   A.CAGPTO_CTA        = :GFCTB0B2.CAGPTO-CTA        AND
                   A.DINIC_FLEXZ_AGPTO > :GFCTB0B2.DINIC-FLEXZ-AGPTO AND
                   B.CEMPR_INC         = :GFCTB0B9.CEMPR-INC         AND
                   B.CDEPDC            = :GFCTB0B9.CDEPDC            AND
                   B.CPOSTO_SERVC      = :GFCTB0B9.CPOSTO-SERVC      AND
                   A.CSIT_SOLTC_FLEXZ  = :GFCTB0B2.CSIT-SOLTC-FLEXZ  AND
                   A.CFUNC_EXCL_SIST   = :GFCTB0B2.CFUNC-EXCL-SIST   AND
                   B.HIDTFD_FLEXZ_AGPTO   IN
                     (SELECT MAX(B.HIDTFD_FLEXZ_AGPTO)
                      FROM   DB2PRD.PDIDO_FLEXZ_AGPTO  A,
                             DB2PRD.PDIDO_FLEXZ_PSTAL  B
                      WHERE
                               A.CSERVC_TARIF  =  B.CSERVC_TARIF
                         AND   A.CAGPTO_CTA    =  B.CAGPTO_CTA
                         AND   A.CSERVC_TARIF  = :GFCTB0B2.CSERVC-TARIF
                         AND   A.CAGPTO_CTA    = :GFCTB0B2.CAGPTO-CTA
                         AND   A.DINIC_FLEXZ_AGPTO >
                                             :GFCTB0B2.DINIC-FLEXZ-AGPTO
                         AND   B.CEMPR_INC     = :GFCTB0B9.CEMPR-INC
                         AND   B.CDEPDC        = :GFCTB0B9.CDEPDC
                         AND   B.CPOSTO_SERVC  = :GFCTB0B9.CPOSTO-SERVC
                         AND   A.CSIT_SOLTC_FLEXZ  =
                                             :GFCTB0B2.CSIT-SOLTC-FLEXZ
                         AND   A.CFUNC_EXCL_SIST   =
                                             :GFCTB0B2.CFUNC-EXCL-SIST)
           END-EXEC.

KVB        IF (SQLCODE       NOT EQUAL ZEROS AND +100 AND -811) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_AGPTO' TO ERR-DBD-TAB
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0060'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               WRITE FD-REG-SAIDA      FROM GFCTT9-REGISTRO
               MOVE WRK-GRAVACAO       TO WRK-OPERACAO
               PERFORM 1120-TESTAR-FS-FLEXVENC
               ADD 1                   TO ACU-GRAVA-FLEXVENC
           ELSE
               ADD 1                   TO ACU-RENOV-FLEXAGPT
           END-IF.

      *----------------------------------------------------------------*
       3600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICA AGRUPAMENTO MUNICIPIO                              *
      ******************************************************************
      *----------------------------------------------------------------*
       3700-VERIFICAR-AGRUP-MUN        SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTT9-CAGPTO-CTA      TO CAGPTO-CTA        OF GFCTB0B2.
           MOVE GFCTT9-CSERVC-TARIF    TO CSERVC-TARIF      OF GFCTB0B2.
           MOVE GFCTT9-DINIC-FLEXZ     TO DINIC-FLEXZ-AGPTO OF GFCTB0B2.
           MOVE 02                     TO CSIT-SOLTC-FLEXZ  OF GFCTB0B2.
           MOVE ZEROS                  TO CFUNC-EXCL-SIST   OF GFCTB0B2
                                          RJUSTF-SOLTC-FLEXZ-LEN
                                                            OF GFCTB0B2
                                          RJUSTF-PAREC-FLEXZ-LEN
                                                            OF GFCTB0B2.
           MOVE SPACES                 TO RJUSTF-SOLTC-FLEXZ-TEXT
                                                            OF GFCTB0B2
                                          RJUSTF-PAREC-FLEXZ-TEXT
                                                            OF GFCTB0B2.
           MOVE GFCTT9-CMUN-AGP18      TO CMUN-IBGE         OF GFCTB0B7.

           EXEC SQL
             SELECT
                   A.CSIT_SOLTC_FLEXZ
             INTO
                   :GFCTB0B2.CSIT-SOLTC-FLEXZ
             FROM  DB2PRD.PDIDO_FLEXZ_AGPTO   A,
                   DB2PRD.PDIDO_FLEXZ_MUN     B
             WHERE
                   A.CSERVC_TARIF      =  B.CSERVC_TARIF             AND
                   A.CAGPTO_CTA        =  B.CAGPTO_CTA               AND
                   A.CSERVC_TARIF      = :GFCTB0B2.CSERVC-TARIF      AND
                   A.CAGPTO_CTA        = :GFCTB0B2.CAGPTO-CTA        AND
                   A.DINIC_FLEXZ_AGPTO > :GFCTB0B2.DINIC-FLEXZ-AGPTO AND
                   B.CMUN_IBGE         = :GFCTB0B7.CMUN-IBGE         AND
                   A.CSIT_SOLTC_FLEXZ  = :GFCTB0B2.CSIT-SOLTC-FLEXZ  AND
                   A.CFUNC_EXCL_SIST   = :GFCTB0B2.CFUNC-EXCL-SIST   AND
                   B.HIDTFD_FLEXZ_AGPTO   IN
                     (SELECT MAX(B.HIDTFD_FLEXZ_AGPTO)
                      FROM   DB2PRD.PDIDO_FLEXZ_AGPTO  A,
                             DB2PRD.PDIDO_FLEXZ_MUN    B
                      WHERE
                               A.CSERVC_TARIF  =  B.CSERVC_TARIF
                         AND   A.CAGPTO_CTA    =  B.CAGPTO_CTA
                         AND   A.CSERVC_TARIF  = :GFCTB0B2.CSERVC-TARIF
                         AND   A.CAGPTO_CTA    = :GFCTB0B2.CAGPTO-CTA
                         AND   A.DINIC_FLEXZ_AGPTO >
                                             :GFCTB0B2.DINIC-FLEXZ-AGPTO
                         AND   B.CMUN_IBGE     = :GFCTB0B7.CMUN-IBGE
                         AND   A.CSIT_SOLTC_FLEXZ  =
                                             :GFCTB0B2.CSIT-SOLTC-FLEXZ
                         AND   A.CFUNC_EXCL_SIST   =
                                             :GFCTB0B2.CFUNC-EXCL-SIST)
           END-EXEC.

KVB        IF (SQLCODE       NOT EQUAL ZEROS AND +100 AND -811) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_AGPTO' TO ERR-DBD-TAB
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0070'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               WRITE FD-REG-SAIDA      FROM GFCTT9-REGISTRO
               MOVE WRK-GRAVACAO       TO WRK-OPERACAO
               PERFORM 1120-TESTAR-FS-FLEXVENC
               ADD 1                   TO ACU-GRAVA-FLEXVENC
           ELSE
               ADD 1                   TO ACU-RENOV-FLEXAGPT
           END-IF.

      *----------------------------------------------------------------*
       3700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICA AGRUPAMENTO UF                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       3800-VERIFICAR-AGRUP-UF         SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTT9-CAGPTO-CTA      TO CAGPTO-CTA        OF GFCTB0B2.
           MOVE GFCTT9-CSERVC-TARIF    TO CSERVC-TARIF      OF GFCTB0B2.
           MOVE GFCTT9-DINIC-FLEXZ     TO DINIC-FLEXZ-AGPTO OF GFCTB0B2.
           MOVE 02                     TO CSIT-SOLTC-FLEXZ  OF GFCTB0B2.
           MOVE ZEROS                  TO CFUNC-EXCL-SIST   OF GFCTB0B2
                                          RJUSTF-SOLTC-FLEXZ-LEN
                                                            OF GFCTB0B2
                                          RJUSTF-PAREC-FLEXZ-LEN
                                                            OF GFCTB0B2.
           MOVE SPACES                 TO RJUSTF-SOLTC-FLEXZ-TEXT
                                                            OF GFCTB0B2
                                          RJUSTF-PAREC-FLEXZ-TEXT
                                                            OF GFCTB0B2.
           MOVE GFCTT9-UF-AGP19        TO CSGL-UF           OF GFCTB0C1.

           EXEC SQL
             SELECT
                   A.CSIT_SOLTC_FLEXZ
             INTO
                   :GFCTB0B2.CSIT-SOLTC-FLEXZ
             FROM  DB2PRD.PDIDO_FLEXZ_AGPTO   A,
                   DB2PRD.PDIDO_FLEXZ_UF      B
             WHERE
                   A.CSERVC_TARIF      =  B.CSERVC_TARIF             AND
                   A.CAGPTO_CTA        =  B.CAGPTO_CTA               AND
                   A.CSERVC_TARIF      = :GFCTB0B2.CSERVC-TARIF      AND
                   A.CAGPTO_CTA        = :GFCTB0B2.CAGPTO-CTA        AND
                   A.DINIC_FLEXZ_AGPTO > :GFCTB0B2.DINIC-FLEXZ-AGPTO AND
                   B.CSGL_UF           = :GFCTB0C1.CSGL-UF           AND
                   A.CSIT_SOLTC_FLEXZ  = :GFCTB0B2.CSIT-SOLTC-FLEXZ  AND
                   A.CFUNC_EXCL_SIST   = :GFCTB0B2.CFUNC-EXCL-SIST   AND
                   B.HIDTFD_FLEXZ_AGPTO   IN
                     (SELECT MAX(B.HIDTFD_FLEXZ_AGPTO)
                      FROM   DB2PRD.PDIDO_FLEXZ_AGPTO  A,
                             DB2PRD.PDIDO_FLEXZ_UF     B
                      WHERE
                               A.CSERVC_TARIF  =  B.CSERVC_TARIF
                         AND   A.CAGPTO_CTA    =  B.CAGPTO_CTA
                         AND   A.CSERVC_TARIF  = :GFCTB0B2.CSERVC-TARIF
                         AND   A.CAGPTO_CTA    = :GFCTB0B2.CAGPTO-CTA
                         AND   A.DINIC_FLEXZ_AGPTO >
                                             :GFCTB0B2.DINIC-FLEXZ-AGPTO
                         AND   B.CSGL_UF       = :GFCTB0C1.CSGL-UF
                         AND   A.CSIT_SOLTC_FLEXZ  =
                                             :GFCTB0B2.CSIT-SOLTC-FLEXZ
                         AND   A.CFUNC_EXCL_SIST   =
                                             :GFCTB0B2.CFUNC-EXCL-SIST)
           END-EXEC.

KVB        IF (SQLCODE       NOT EQUAL ZEROS AND +100 AND -811) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_AGPTO' TO ERR-DBD-TAB
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0080'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               WRITE FD-REG-SAIDA      FROM GFCTT9-REGISTRO
               MOVE WRK-GRAVACAO       TO WRK-OPERACAO
               PERFORM 1120-TESTAR-FS-FLEXVENC
               ADD 1                   TO ACU-GRAVA-FLEXVENC
           ELSE
               ADD 1                   TO ACU-RENOV-FLEXAGPT
           END-IF.

      *----------------------------------------------------------------*
       3800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3900-VERIFICAR-OUTROS-AGRUP         SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTT9-CAGPTO-CTA      TO CAGPTO-CTA        OF GFCTB0B2.
           MOVE GFCTT9-CSERVC-TARIF    TO CSERVC-TARIF      OF GFCTB0B2.
           MOVE GFCTT9-DINIC-FLEXZ     TO DINIC-FLEXZ-AGPTO OF GFCTB0B2.
           MOVE 02                     TO CSIT-SOLTC-FLEXZ  OF GFCTB0B2.
           MOVE ZEROS                  TO CFUNC-EXCL-SIST   OF GFCTB0B2
                                          RJUSTF-SOLTC-FLEXZ-LEN
                                                            OF GFCTB0B2
                                          RJUSTF-PAREC-FLEXZ-LEN
                                                            OF GFCTB0B2.
           MOVE SPACES                 TO RJUSTF-SOLTC-FLEXZ-TEXT
                                                            OF GFCTB0B2
                                          RJUSTF-PAREC-FLEXZ-TEXT
                                                            OF GFCTB0B2.

           EXEC SQL
             SELECT
               CAGPTO_CTA,
               CSERVC_TARIF,
               HIDTFD_FLEXZ_AGPTO,
               CFUNC_SOLTC_FLEXZ,
               CFUNC_PAREC_FLEXZ,
               CTPO_OPER_MOTVO,
               CMOTVO_JUSTF,
               CEMPR_ORIGE_FLEXZ,
               CDEPDC_ORIGE_FLEXZ,
               CEMPR_PAREC_FLEXZ,
               CDEPDC_PAREC_FLEXZ,
               DINIC_FLEXZ_AGPTO,
               DFIM_FLEXZ_AGPTO,
               QFRANQ_FLEXZ_AGPTO,
               CPER_FRANQ_AGPTO,
               VNEGOC_FLEXZ_AGPTO,
               PDESC_FLEXZ_AGPTO,
               DDIA_AGNDA,
               QDIA_FLEXZ_AGPTO,
               HSOLTC_FLEXZ,
               RJUSTF_SOLTC_FLEXZ,
               HPAREC_FLEXZ,
               RJUSTF_PAREC_FLEXZ,
               CSIT_SOLTC_FLEXZ,
               CINDCD_AGPTO_TOT,
               CINIC_PER_FRANQ,
               CFUNC_EXCL_SIST,
               CPER_COBR_TARIF,
               CINIC_PER_COBR
             INTO
               :GFCTB0B2.CAGPTO-CTA,
               :GFCTB0B2.CSERVC-TARIF,
               :GFCTB0B2.HIDTFD-FLEXZ-AGPTO,
               :GFCTB0B2.CFUNC-SOLTC-FLEXZ,
               :GFCTB0B2.CFUNC-PAREC-FLEXZ,
               :GFCTB0B2.CTPO-OPER-MOTVO,
               :GFCTB0B2.CMOTVO-JUSTF,
               :GFCTB0B2.CEMPR-ORIGE-FLEXZ,
               :GFCTB0B2.CDEPDC-ORIGE-FLEXZ,
               :GFCTB0B2.CEMPR-PAREC-FLEXZ,
               :GFCTB0B2.CDEPDC-PAREC-FLEXZ,
               :GFCTB0B2.DINIC-FLEXZ-AGPTO,
               :GFCTB0B2.DFIM-FLEXZ-AGPTO,
               :GFCTB0B2.QFRANQ-FLEXZ-AGPTO,
               :GFCTB0B2.CPER-FRANQ-AGPTO,
               :GFCTB0B2.VNEGOC-FLEXZ-AGPTO,
               :GFCTB0B2.PDESC-FLEXZ-AGPTO,
               :GFCTB0B2.DDIA-AGNDA,
               :GFCTB0B2.QDIA-FLEXZ-AGPTO,
               :GFCTB0B2.HSOLTC-FLEXZ,
               :GFCTB0B2.RJUSTF-SOLTC-FLEXZ,
               :GFCTB0B2.HPAREC-FLEXZ,
               :GFCTB0B2.RJUSTF-PAREC-FLEXZ,
               :GFCTB0B2.CSIT-SOLTC-FLEXZ,
               :GFCTB0B2.CINDCD-AGPTO-TOT,
               :GFCTB0B2.CINIC-PER-FRANQ,
               :GFCTB0B2.CFUNC-EXCL-SIST,
               :GFCTB0B2.CPER-COBR-TARIF,
               :GFCTB0B2.CINIC-PER-COBR
             FROM  DB2PRD.PDIDO_FLEXZ_AGPTO
             WHERE
                   CSERVC_TARIF      = :GFCTB0B2.CSERVC-TARIF      AND
                   CAGPTO_CTA        = :GFCTB0B2.CAGPTO-CTA        AND
                   DINIC_FLEXZ_AGPTO > :GFCTB0B2.DINIC-FLEXZ-AGPTO AND
                   CSIT_SOLTC_FLEXZ  = :GFCTB0B2.CSIT-SOLTC-FLEXZ  AND
                   CFUNC_EXCL_SIST   = :GFCTB0B2.CFUNC-EXCL-SIST
           END-EXEC.

KVB        IF (SQLCODE       NOT EQUAL ZEROS AND +100 AND -811) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_AGPTO' TO ERR-DBD-TAB
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0095'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               WRITE FD-REG-SAIDA      FROM GFCTT9-REGISTRO
               MOVE WRK-GRAVACAO       TO WRK-OPERACAO
               PERFORM 1120-TESTAR-FS-FLEXVENC
               ADD 1                   TO ACU-GRAVA-FLEXVENC
           ELSE
               ADD 1                   TO ACU-RENOV-FLEXAGPT
           END-IF.

      *----------------------------------------------------------------*
       3900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
LFM   *----------------------------------------------------------------*
LFM    3910-VERIFICAR-AGRUP-RAZAO   SECTION.
LFM   *----------------------------------------------------------------*
LFM
LFM        PERFORM 3920-MAX-PED-RAZAO
LFM
LFM        EXEC SQL
LFM              SELECT A.CSIT_SOLTC_FLEXZ,
LFM                     A.DINIC_FLEXZ_AGPTO
LFM              INTO   :GFCTB0B2.CSIT-SOLTC-FLEXZ,
LFM                     :GFCTB0B2.DINIC-FLEXZ-AGPTO
LFM              FROM   DB2PRD.PDIDO_FLEXZ_AGPTO A,
LFM                     DB2PRD.TPDIDO_FLEXZ_RZ   B
LFM           WHERE  A.CAGPTO_CTA         =  B.CAGPTO_CTA
LFM           AND    A.CSERVC_TARIF       =  B.CSERVC_TARIF
LFM           AND    A.CSERVC_TARIF       = :GFCTB0B2.CSERVC-TARIF
LFM           AND    A.CAGPTO_CTA         = :GFCTB0B2.CAGPTO-CTA
LFM           AND    B.CGRP_CTBIL_ATUAL   = :GFCTB0M7.CGRP-CTBIL-ATUAL
LFM           AND    B.CSGRP_CTBIL        = :GFCTB0M7.CSGRP-CTBIL
LFM           AND    A.HIDTFD_FLEXZ_AGPTO = B.HIDTFD_FLEXZ_AGPTO
LFM           AND    A.CSIT_SOLTC_FLEXZ   = :GFCTB0B2.CSIT-SOLTC-FLEXZ
LFM           AND    A.CFUNC_EXCL_SIST    = :GFCTB0B2.CFUNC-EXCL-SIST
LFM           AND    B.HIDTFD_FLEXZ_AGPTO = :GFCTB0M7.HIDTFD-FLEXZ-AGPTO
LFM        END-EXEC.
LFM
LFM
LFM        IF (SQLCODE       NOT EQUAL ZEROS AND +100 AND -811) OR
LFM           (SQLWARN0                EQUAL 'W'      )
LFM           MOVE 'DB2'              TO ERR-TIPO-ACESSO
LFM           MOVE 'PDIDO_FLEXZ_AGPTO' TO ERR-DBD-TAB
LFM           MOVE 'SELECT  '         TO ERR-FUN-COMANDO
LFM           MOVE  SQLCODE           TO ERR-SQL-CODE
LFM           MOVE '0100'             TO ERR-LOCAL
LFM           MOVE  SPACES            TO ERR-SEGM
LFM           PERFORM 9999-PROCESSAR-ROTINA-ERRO
LFM        END-IF.
LFM
LFM        IF  SQLCODE                  EQUAL +100
LFM            WRITE FD-REG-SAIDA      FROM GFCTT9-REGISTRO
LFM            MOVE WRK-GRAVACAO       TO WRK-OPERACAO
LFM            PERFORM 1120-TESTAR-FS-FLEXVENC
LFM            ADD 1                   TO ACU-GRAVA-FLEXVENC
LFM        ELSE
LFM            MOVE DINIC-FLEXZ-AGPTO   OF GFCTB0B2
LFM                                     TO WRK-DINIC-FLEXZ-AGPTO
LFM            MOVE WRK-DIA-DINIC       TO WRK-DIA-DINIC-INV
LFM            MOVE WRK-MES-DINIC       TO WRK-MES-DINIC-INV
LFM            MOVE WRK-ANO-DINIC       TO WRK-ANO-DINIC-INV
LFM            IF   WRK-DINIC-FLEXZ-T9-INV > WRK-DINIC-FLEXZ-AGPTO-INV
LFM                 WRITE FD-REG-SAIDA      FROM GFCTT9-REGISTRO
LFM                 MOVE WRK-GRAVACAO       TO WRK-OPERACAO
LFM                 PERFORM 1120-TESTAR-FS-FLEXVENC
LFM                 ADD 1                   TO ACU-GRAVA-FLEXVENC
LFM            ELSE
LFM                 ADD 1                   TO ACU-RENOV-FLEXAGPT
LFM             END-IF
LFM        END-IF.
LFM
LFM   *----------------------------------------------------------------*
LFM    3910-99-FIM.                    EXIT.
LFM   *----------------------------------------------------------------*
LFM
LFM   *----------------------------------------------------------------*
LFM    3920-MAX-PED-RAZAO    SECTION.
LFM   *----------------------------------------------------------------*
LFM
LFM         MOVE GFCTT9-CAGPTO-CTA      TO CAGPTO-CTA     OF GFCTB0B2.
LFM         MOVE GFCTT9-CAGPTO-CTA      TO CAGPTO-CTA     OF GFCTB0M7.
LFM         MOVE GFCTT9-CSERVC-TARIF    TO CSERVC-TARIF   OF GFCTB0B2.
LFM         MOVE GFCTT9-CSERVC-TARIF    TO CSERVC-TARIF   OF GFCTB0M7.
LFM         MOVE GFCTT9-DINIC-FLEXZ  TO DINIC-FLEXZ-AGPTO OF GFCTB0B2
LFM                                        WRK-DINIC-FLEXZ-T9
LFM         MOVE WRK-DIA-DINIC-T9       TO WRK-DIA-DINIC-T9-INV
LFM         MOVE WRK-MES-DINIC-T9       TO WRK-MES-DINIC-T9-INV
LFM         MOVE WRK-ANO-DINIC-T9       TO WRK-ANO-DINIC-T9-INV
LFM         MOVE 02                     TO CSIT-SOLTC-FLEXZ OF GFCTB0B2.
LFM         MOVE ZEROS                  TO CFUNC-EXCL-SIST  OF GFCTB0B2
LFM                                        RJUSTF-SOLTC-FLEXZ-LEN
LFM                                                         OF GFCTB0B2
LFM                                        RJUSTF-PAREC-FLEXZ-LEN
LFM                                                         OF GFCTB0B2.
LFM         MOVE SPACES                 TO RJUSTF-SOLTC-FLEXZ-TEXT
LFM                                                      OF GFCTB0B2
LFM                                        RJUSTF-PAREC-FLEXZ-TEXT
LFM                                                      OF GFCTB0B2.
LFM         MOVE GFCTT9-GRP-CTBIL-AGP26 TO CGRP-CTBIL-ATUAL OF GFCTB0M7.
LFM         MOVE GFCTT9-SGRP-CTBIL-AGP26 TO CSGRP-CTBIL OF GFCTB0M7.
LFM
LFM
LFM        EXEC SQL
LFM        SELECT MAX(B.HIDTFD_FLEXZ_AGPTO)
LFM        INTO   :GFCTB0M7.HIDTFD-FLEXZ-AGPTO
LFM        FROM   DB2PRD.PDIDO_FLEXZ_AGPTO A,
LFM               DB2PRD.TPDIDO_FLEXZ_RZ    B
LFM        WHERE  A.CAGPTO_CTA         = B.CAGPTO_CTA
LFM          AND  A.CSERVC_TARIF       = B.CSERVC_TARIF
LFM          AND  A.HIDTFD_FLEXZ_AGPTO = B.HIDTFD_FLEXZ_AGPTO
LFM          AND  B.CAGPTO_CTA         = :GFCTB0M7.CAGPTO-CTA
LFM          AND  B.CSERVC_TARIF       = :GFCTB0M7.CSERVC-TARIF
LFM          AND  B.CGRP_CTBIL_ATUAL   = :GFCTB0M7.CGRP-CTBIL-ATUAL
LFM          AND  B.CSGRP_CTBIL        = :GFCTB0M7.CSGRP-CTBIL
LFM        END-EXEC.
LFM
LFM
LFM         IF (SQLCODE                NOT EQUAL ZEROS AND -305) OR
LFM           (SQLWARN0                EQUAL 'W'      )
LFM            MOVE 'DB2'              TO ERR-TIPO-ACESSO
LFM            MOVE 'PDIDO_FLEXZ_AGPTO' TO ERR-DBD-TAB
LFM            MOVE 'SELECT  '         TO ERR-FUN-COMANDO
LFM            MOVE  SQLCODE           TO ERR-SQL-CODE
LFM            MOVE '0110'             TO ERR-LOCAL
LFM            MOVE  SPACES            TO ERR-SEGM
LFM            PERFORM 9999-PROCESSAR-ROTINA-ERRO
LFM        END-IF.
LFM
LFM        IF  SQLCODE                 EQUAL -305
LFM            MOVE '0001-01-01-01.01.01.000001'
LFM                                    TO HIDTFD-FLEXZ-AGPTO
LFM                                                      OF GFCTB0M7
LFM        END-IF.
LFM
LFM   *----------------------------------------------------------------*
LFM    3920-99-FIM.                    EXIT.
LFM   *----------------------------------------------------------------*

      ******************************************************************
      *    PROCEDIMENTOS REALIZADOS NO FINAL DO PROCESSAMENTO          *
      ******************************************************************
      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           IF  ACU-GRAVA-FLEXVENC      GREATER ZEROS
               PERFORM 4100-EMITIR-DISPLAY-TOTAIS
           ELSE
               PERFORM 4200-EMITIR-DISPLAY-VAZIO
           END-IF.

           CLOSE FLEXAGPT
                 FLEXVENC.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

           STOP RUN.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    EMITE DISPLAY COM OS TOTAIS DO PROCESSAMENTO                *
      ******************************************************************
      *----------------------------------------------------------------*
       4100-EMITIR-DISPLAY-TOTAIS      SECTION.
      *----------------------------------------------------------------*

           DISPLAY '*-----------------------------------------------*'.
           DISPLAY '*               PROGRAMA GFCT1785               *'.
           DISPLAY '*               -----------------               *'.
           DISPLAY '*                                               *'.
           DISPLAY '*      PROCESSAMENTO  EFETUADO COM SUCESSO      *'.
           DISPLAY '*-----------------------------------------------*'.
           DISPLAY '*                                               *'.
           MOVE ACU-LIDOS-FLEXAGPT     TO WRK-MASCARA.
           DISPLAY '* TOT. REGS. LIDOS EM FLEXAGPT  = ' WRK-MASCARA
                                                                  ' *'.
           MOVE ACU-RENOV-FLEXAGPT     TO WRK-MASCARA.
           DISPLAY '* TOT. REGS. RENOVADOS FLEXAGPT = ' WRK-MASCARA
                                                                  ' *'.
           MOVE ACU-GRAVA-FLEXVENC     TO WRK-MASCARA.
           DISPLAY '* TOT. REGS. GRAVADOS FLEXAVENC = ' WRK-MASCARA
                                                                  ' *'.
           DISPLAY '*-----------------------------------------------*'.

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    EMITE DISPLAY QUANDO ARQUIVO DE SAIDA VAZIO                 *
      ******************************************************************
      *----------------------------------------------------------------*
       4200-EMITIR-DISPLAY-VAZIO       SECTION.
      *----------------------------------------------------------------*

           DISPLAY '*-----------------------------------------------*'.
           DISPLAY '*               PROGRAMA GFCT1785               *'.
           DISPLAY '*               -----------------               *'.
           DISPLAY '*                                               *'.
           DISPLAY '*           ARQUIVO DE FLEXVENC VAZIO           *'.
           DISPLAY '*-----------------------------------------------*'.

      *----------------------------------------------------------------*
       4200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    EXECUTA PROCEDIMENTO DE CANCELAMENTO DO PGM EM CASO DE ERRO *
      ******************************************************************
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT1785'             TO ERR-PGM.

           CALL 'POOL7100'             USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA.
           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

