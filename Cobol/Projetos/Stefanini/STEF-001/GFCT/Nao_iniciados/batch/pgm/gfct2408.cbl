      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. GFCT2408.
       AUTHOR.     VINICIUS CRISTIAN MADUREIRA.
      *================================================================*
      *      S O N D A  P R O C W O R K  -  C O N S U L T O R I A      *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT2408                                    *
      *    PROGRAMADOR.:   VINICIUS CRISTIAN MADUREIRA - SONDPROC/GP.50*
      *    ANALISTA....:   VINICIUS CRISTIAN MADUREIRA - SONDPROC/GP.50*
      *    DATA........:   23/03/2009                                  *
      *                                                                *
      *    OBJETIVO    :   SEPARA CONTAS DO FORNECEDOR SOLICITADO.     *
      *                                                                *
      *    ARQUIVOS:                                                   *
      *      DDNAME                                    INCLUDE/BOOK    *
      *      SOLICFOR                                  GFCTWAFZ        *
      *      CTASFOR1                                  GFCTWAF2        *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                       INCLUDE/BOOK      *
      *                   DB2PRD.LOG_ALT_PGTO        PFEBB010          *
      *----------------------------------------------------------------*
      *    MODULOS CHAMADOS:                                           *
      *      BRAD7100  -  MODULO DE TRATAMENTO DE ERRO.                *
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

           SELECT SOLICFOR ASSIGN      TO UT-S-SOLICFOR
                      FILE STATUS      IS WRK-FS-SOLICFOR.

           SELECT CTASFOR1 ASSIGN      TO UT-S-CTASFOR1
                      FILE STATUS      IS WRK-FS-CTASFOR1.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *================================================================*
       FILE                            SECTION.
      *================================================================*

      *----------------------------------------------------------------*
      *   INPUT: REGISTRO                                             *
      *           ORG.SEQUENCIAL       - LRECL = 700                   *
      *----------------------------------------------------------------*

       FD  SOLICFOR
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'GFCTWAFZ'.

      *----------------------------------------------------------------*
      *   OUTPUT: REGISTRO                                             *
      *           ORG.SEQUENCIAL       - LRECL = 150                   *
      *----------------------------------------------------------------*

       FD  CTASFOR1
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'GFCTWAF2'.

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)          VALUE
           '*   INICIO DA WORKING GFCT2408 *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*         ACUMULADORES         *'.
      *----------------------------------------------------------------*

       01  ACU-ACUMULADORES.
           05  ACU-LIDOS               PIC  9(11)  COMP-3  VALUE ZEROS.
           05  ACU-GRAVA-CTASFOR1      PIC  9(11)  COMP-3  VALUE ZEROS.
           05  ACU-DESPREZADO          PIC  9(11)  COMP-3  VALUE ZEROS.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '* VARIAVEIS AUXILIARES         *'.
      *---------------------------------------------------------------*

       01  WRK-AUXILIARES.
           05  WRK-BATCH               PIC  X(08)          VALUE
               'BATCH'.
           05 WRK-MASCARA              PIC ZZ.ZZZ.ZZZ.Z99  VALUE ZEROS.

       01  WRK-FIM-CSR-PFEB            PIC  X(001)         VALUE SPACES.
       01  WRK-FLAG                    PIC  X(001)         VALUE 'N'.
       01  WRK-VAR2-1                  PIC  X(002)         VALUE '02'.
       01  WRK-VAR2-2                  PIC  X(002)         VALUE ' 2'.
       01  WRK-VAR2-3                  PIC  X(002)         VALUE '2 '.

       01  FILLER.
           03 WRK-CAG-BCRIA-NULL       PIC S9(04) COMP     VALUE ZEROS.
           03 WRK-CCTA-CORR-EXTER-NULL PIC S9(04) COMP     VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*  AREA PARA FILE-STATUS       *'.
      *----------------------------------------------------------------*

       01  WRK-AREA-FILE-STATUS.
           05  WRK-FS-SOLICFOR         PIC  X(02)          VALUE SPACES.
           05  WRK-FS-CTASFOR1         PIC  X(02)          VALUE SPACES.
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
       01  FILLER                      PIC  X(32)          VALUE
           '*       AREA PARA BRAD7100     *'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*         AREA PARA DB2        *'.
      *----------------------------------------------------------------*

           EXEC SQL
             INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
             INCLUDE PFEBB010
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*DECLARACAO DE CURSORES*'.
      *----------------------------------------------------------------*

           EXEC SQL
               DECLARE CSR01-PFEBB010 CURSOR FOR
             SELECT
                   CAG_BCRIA,
                   CCTA_CORR_EXTER,
                   CIDTFD_LCTO
             FROM  DB2PRD.LOG_ALT_PGTO
             WHERE
                  (CCGC_CPF           = :PFEBB010.CCGC-CPF           AND
                   CFLIAL_CGC         = :PFEBB010.CFLIAL-CGC         AND
                   CCTRL_CGC          = :PFEBB010.CCTRL-CGC          AND
                   CIDTFD_INDCD_FORNC = 1                            AND
                   CSIT_PGTO_FORNC    = :WRK-VAR2-1)                  OR

                  (CCGC_CPF           = :PFEBB010.CCGC-CPF           AND
                   CFLIAL_CGC         = :PFEBB010.CFLIAL-CGC         AND
                   CCTRL_CGC          = :PFEBB010.CCTRL-CGC          AND
                   CIDTFD_INDCD_FORNC = 1                            AND
                   CSIT_PGTO_FORNC    = :WRK-VAR2-2)                  OR

                  (CCGC_CPF           = :PFEBB010.CCGC-CPF           AND
                   CFLIAL_CGC         = :PFEBB010.CFLIAL-CGC         AND
                   CCTRL_CGC          = :PFEBB010.CCTRL-CGC          AND
                   CIDTFD_INDCD_FORNC = 1                            AND
                   CSIT_PGTO_FORNC    = :WRK-VAR2-3)                  OR

                  (CCGC_CPF           = :PFEBB010.CCGC-CPF           AND
                   CFLIAL_CGC         = :PFEBB010.CFLIAL-CGC         AND
                   CCTRL_CGC          = :PFEBB010.CCTRL-CGC          AND
                   CIDTFD_INDCD_FORNC = 5                            AND
                   CSIT_PGTO_FORNC    = :WRK-VAR2-1)                  OR

                  (CCGC_CPF           = :PFEBB010.CCGC-CPF           AND
                   CFLIAL_CGC         = :PFEBB010.CFLIAL-CGC         AND
                   CCTRL_CGC          = :PFEBB010.CCTRL-CGC          AND
                   CIDTFD_INDCD_FORNC = 5                            AND
                   CSIT_PGTO_FORNC    = :WRK-VAR2-2)                  OR

                  (CCGC_CPF           = :PFEBB010.CCGC-CPF           AND
                   CFLIAL_CGC         = :PFEBB010.CFLIAL-CGC         AND
                   CCTRL_CGC          = :PFEBB010.CCTRL-CGC          AND
                   CIDTFD_INDCD_FORNC = 5                            AND
                   CSIT_PGTO_FORNC    = :WRK-VAR2-3)
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    FIM DA WORKING GFCT2408  *'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           CALL 'BRAD1050'.

           PERFORM 0100-INICIALIZAR-PROGRAMA.

           PERFORM 0300-VERIFICAR-VAZIO.

           PERFORM 0600-PROCESSAR      UNTIL
                       WRK-FS-SOLICFOR EQUAL '10'.

           PERFORM 1200-FINALIZAR-PROGRAMA.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0100-INICIALIZAR-PROGRAMA       SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT   SOLICFOR
                OUTPUT  CTASFOR1.

           MOVE  WRK-ABERTURA          TO WRK-OPERACAO.

           PERFORM 0200-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       0100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0200-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 0210-TESTAR-FS-SOLICFOR.

           PERFORM 0220-TESTAR-FS-CTASFOR1.

      *----------------------------------------------------------------*
       0200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0210-TESTAR-FS-SOLICFOR         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SOLICFOR         NOT EQUAL '00'
               MOVE 'SOLICFOR'         TO WRK-NOME-ARQ
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-SOLICFOR    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0220-TESTAR-FS-CTASFOR1         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-CTASFOR1         NOT EQUAL '00'
               MOVE 'CTASFOR1'         TO WRK-NOME-ARQ
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-CTASFOR1    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0300-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 0350-LER-SOLICFOR.

           IF  WRK-FS-SOLICFOR        EQUAL '10'
               DISPLAY '*********** GFCT2408 ***********'
               DISPLAY '*                              *'
               DISPLAY '*    ARQUIVO SOLICFOR VAZIO    *'
               DISPLAY '*    PROCESSAMENTO ENCERRADO   *'
               DISPLAY '*                              *'
               DISPLAY '*********** GFCT2408 ***********'
           END-IF.

      *----------------------------------------------------------------*
       0300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0350-LER-SOLICFOR               SECTION.
      *----------------------------------------------------------------*

           READ SOLICFOR.

           IF  WRK-FS-SOLICFOR         EQUAL '10'
               GO                      TO 0350-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 0210-TESTAR-FS-SOLICFOR.

           ADD 1                       TO ACU-LIDOS.

           MOVE 'N'                    TO WRK-FLAG
                                          WRK-FIM-CSR-PFEB.

      *----------------------------------------------------------------*
       0350-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0600-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 0610-ABRIR-CURSOR-PFEBB010.

           PERFORM 0620-FETCH-CURSOR-PFEBB010
                   UNTIL WRK-FIM-CSR-PFEB EQUAL 'S'.

           PERFORM 0630-FECHAR-CURSOR-PFEBB010.

           PERFORM 0350-LER-SOLICFOR.

      *----------------------------------------------------------------*
       0600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0610-ABRIR-CURSOR-PFEBB010                  SECTION.
      *----------------------------------------------------------------*

           MOVE WAFZ-CNPJ-NUM TO CCGC-CPF   OF PFEBB010.
           MOVE WAFZ-CNPJ-FIL TO CFLIAL-CGC OF PFEBB010.
           MOVE WAFZ-CNPJ-CTR TO CCTRL-CGC  OF PFEBB010.

           EXEC SQL
               OPEN  CSR01-PFEBB010
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
                MOVE SQLCODE           TO ERR-SQL-CODE
                MOVE 'DB2'             TO ERR-TIPO-ACESSO
                MOVE 'LOG_ALT_PGTO'
                                       TO ERR-DBD-TAB
                MOVE 'OPCURSOR'        TO ERR-FUN-COMANDO
                MOVE SQLCODE           TO ERR-SQL-CODE
                MOVE '0001'            TO ERR-LOCAL
                MOVE SPACES            TO ERR-SEGM
                PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0610-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0620-FETCH-CURSOR-PFEBB010                  SECTION.
      *----------------------------------------------------------------*

           EXEC SQL  FETCH CSR01-PFEBB010 INTO
               :PFEBB010.CAG-BCRIA
                  :WRK-CAG-BCRIA-NULL,
               :PFEBB010.CCTA-CORR-EXTER
                   :WRK-CCTA-CORR-EXTER-NULL,
               :PFEBB010.CIDTFD-LCTO
           END-EXEC.

           IF (SQLCODE NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
                MOVE 'DB2'             TO ERR-TIPO-ACESSO
                MOVE 'LOG_ALT_PGTO'
                                       TO ERR-DBD-TAB
                MOVE 'FETCH'           TO ERR-FUN-COMANDO
                MOVE SQLCODE           TO ERR-SQL-CODE
                MOVE '0002'            TO ERR-LOCAL
                MOVE SPACES            TO ERR-SEGM
                PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               IF WRK-FLAG EQUAL 'N'
                   ADD 1               TO ACU-DESPREZADO
               END-IF
               MOVE 'S'                TO WRK-FIM-CSR-PFEB
           ELSE
               IF WRK-CAG-BCRIA-NULL       LESS ZEROS
                   MOVE ZEROS TO CAG-BCRIA
                              OF PFEBB010
               END-IF
               IF WRK-CCTA-CORR-EXTER-NULL LESS ZEROS
                   MOVE ZEROS TO CCTA-CORR-EXTER
                              OF PFEBB010
               END-IF
               MOVE 'S'                TO WRK-FLAG
               PERFORM 0700-TRATAR-REGISTROS
           END-IF.

      *----------------------------------------------------------------*
       0620-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0630-FECHAR-CURSOR-PFEBB010                  SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR01-PFEBB010
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
                MOVE 'DB2'             TO ERR-TIPO-ACESSO
                MOVE 'LOG_ALT_PGTO'
                                       TO ERR-DBD-TAB
                MOVE 'CLOSE'           TO ERR-FUN-COMANDO
                MOVE SQLCODE           TO ERR-SQL-CODE
                MOVE '0003'            TO ERR-LOCAL
                MOVE SPACES            TO ERR-SEGM
                PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0630-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0700-TRATAR-REGISTROS                  SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                          REG-WAF2.
           MOVE WAFZ-CAGPTO-CTA             TO WAF2-CAGPTO-CTA-S.
           MOVE WAFZ-CSERVC-TARIF           TO WAF2-CSERVC-TARIF-S.
           MOVE WAFZ-HINCL-REG              TO WAF2-HINCL-REG-S.
           MOVE CAG-BCRIA       OF PFEBB010 TO WAF2-AGENCIA-FUNC.
           MOVE CCTA-CORR-EXTER OF PFEBB010 TO WAF2-CONTA-FUNC.
           MOVE CIDTFD-LCTO     OF PFEBB010 TO WAF2-CIDTFD-LCTO.

           PERFORM 0740-GRAVAR-SAIDA.

      *----------------------------------------------------------------*
       0700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0740-GRAVAR-SAIDA            SECTION.
      *----------------------------------------------------------------*

           WRITE REG-WAF2 OF CTASFOR1.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 0220-TESTAR-FS-CTASFOR1.

           ADD 1                       TO ACU-GRAVA-CTASFOR1.

      *----------------------------------------------------------------*
       0740-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA DE FINALIZACAO DO PROGRAMA                           *
      *----------------------------------------------------------------*
       1200-FINALIZAR-PROGRAMA         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1300-EMITIR-DISPLAY.

           CLOSE SOLICFOR
                 CTASFOR1.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 0200-TESTAR-FILE-STATUS.

           STOP RUN.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA DE EMISSAO DE TOTAIS DE CONTROLE                     *
      *----------------------------------------------------------------*
       1300-EMITIR-DISPLAY             SECTION.
      *----------------------------------------------------------------*

           DISPLAY '*------------------------------------------------*'.
           DISPLAY '*              PROGRAMA GFCT2408                 *'.
           DISPLAY '*              -----------------                 *'.
           DISPLAY '*                                                *'.
           DISPLAY '*      PROCESSAMENTO  EFETUADO COM SUCESSO       *'.
           DISPLAY '*----------------------------------------------- *'.
           DISPLAY '*                                                *'.
           MOVE ACU-LIDOS              TO WRK-MASCARA.
           DISPLAY '*  LIDOS EM SOLICFOR......: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-GRAVA-CTASFOR1     TO WRK-MASCARA.
           DISPLAY '*  GRAVADOS NO CTASFOR1...: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           DISPLAY '******************** GFCT2408 ********************'.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     RETORNO DE ERRO                                            *
      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           IF  ERR-TIPO-ACESSO         NOT EQUAL 'DB2'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
           END-IF.

           MOVE 'GFCT2408'             TO ERR-PGM.

           CALL 'BRAD7100'             USING WRK-BATCH
                                             ERRO-AREA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

