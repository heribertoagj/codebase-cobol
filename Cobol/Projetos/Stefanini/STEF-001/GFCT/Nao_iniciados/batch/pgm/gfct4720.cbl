      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT4720.
       AUTHOR.     KATIA PEGORELLI.
      *================================================================*
      *                        SONDA IT                                *
      *----------------------------------------------------------------*
      *    PROGRAMA....:  GFCT4720                                     *
      *    PROGRAMADOR.:  KATIA PEGORELLI         - SONDA IT           *
      *    ANALISTA    :  ANTONIO PAGNOCCA        - SONDA IT           *
      *    DATA........:  18/11/2015.                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:  INCLUIR CPF OU CNPJ NO ARQUIVO REDUZIDO PARA *
      *                   PESQUISA SIGB                                *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                      INCLUDE/BOOK        *
      *                  REDUZ23C                    I#GFCT51          *
      *                  REDUZ23W                    I#GFCT51          *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#GFCT51 - ARQUIVO REDUZIDO PARA PESQUISA SIGB.             *
      *    I#BRAD7C - AREA UTILIZADA PELA BRAD7100.                    *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    BRAD7100 - TRATAMENTO DE ERRO.                              *
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
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*

       FILE-CONTROL.

           SELECT REDUZ23F ASSIGN      TO UT-S-REDUZ23F
           FILE STATUS                 IS WRK-FS-REDUZ23F.

           SELECT REDUZ23W ASSIGN      TO UT-S-REDUZ23W
           FILE STATUS                 IS WRK-FS-REDUZ23W.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:  ARQUIVO DE ENTRADA  -   REDUZ23F                    *
      *            ORG. SEQUENCIAL     -   LRECL   =   050             *
      *----------------------------------------------------------------*

       FD  REDUZ23F
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS  0 RECORDS.

       01 FD-REDUZ23F                  PIC  X(050).

      *    BRAD7100 - TRATAMENTO DE ERRO.                              *
      *----------------------------------------------------------------*
      *   OUTPUT:  ARQUIVO DE SAIDA    -   REDUZ23W                    *
      *            ORG. SEQUENCIAL     -   LRECL   =   050             *
      *----------------------------------------------------------------*

       FD  REDUZ23W
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS  0 RECORDS.

       01 FD-REDUZ23W                  PIC  X(050).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           'INICIO DA WORKING SECTION STORAGE'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           'AREA PARA ACUMULADORES'.
      *----------------------------------------------------------------*

       77  ACU-LIDOS-REDUZ23F          PIC  9(013) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-REDUZ23W          PIC  9(013) COMP-3  VALUE ZEROS.
       77  WRK-ERRO                    PIC  9(001)         VALUE ZEROS.

       01  WRK-CID-CLI                 PIC  X(026)         VALUE SPACES.
       01  WRK-CTPO-CTA                PIC  9(002)         VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           'VARIAVEIS AUXILIARES'.
      *----------------------------------------------------------------*

       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.

       77  WRK-MASCARA                 PIC  Z.ZZZ.ZZZ.ZZZ.ZZ9
                                                           VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(031)         VALUE
           'AREA PARA TESTES DE FILE-STATUS'.
      *----------------------------------------------------------------*

       01  WRK-FS-REDUZ23F             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-REDUZ23W             PIC  X(002)         VALUE SPACES.

       01  WRK-ABERTURA                PIC  X(013)         VALUE
           ' NA ABERTURA '.
       01  WRK-LEITURA                 PIC  X(013)         VALUE
           ' NA LEITURA  '.
       01  WRK-GRAVACAO                PIC  X(013)         VALUE
           ' NA GRAVACAO '.
       01  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(031)         VALUE
           'MENSAGEM DE ERRO DE FILE-STATUS'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-BRAD7100.
           05 FILLER                   PIC  X(009)         VALUE
              '*** ERRO '.
           05 WRK-OPERACAO             PIC  X(013)         VALUE SPACES.
           05 FILLER                   PIC  X(012)         VALUE
              ' DO ARQUIVO '.
           05 WRK-NOME-ARQ             PIC  X(008)         VALUE SPACES.
           05 FILLER                   PIC  X(017)         VALUE
              ' - FILE-STATUS = '.
           05 WRK-FILE-STATUS          PIC  X(002)         VALUE SPACES.
           05 FILLER                   PIC  X(004)         VALUE ' ***'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA PARA LAYOUT DOS ARQUIVOS'.
      *----------------------------------------------------------------*

       COPY 'I#GFCT51'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA PARA BRAD7100'.
      *----------------------------------------------------------------*

       COPY 'POL7100C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA DB2 ***'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EJECT
           EXEC SQL
               INCLUDE CLIEV007
           END-EXEC.

           EJECT
           EXEC SQL
               INCLUDE CLIEV008
           END-EXEC.

           EJECT
           EXEC SQL
               INCLUDE CLIEV004
           END-EXEC.

      *---------------------------------------------------------------*
      * AREA DE DECLARACAO PARA SUPORTE A MULTIPLAS FAIXAS DE AGENCIA *
      *---------------------------------------------------------------*
      *
           COPY 'I#HEXA01'.
      *

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(030)         VALUE
           'FIM DA WORKING SECTION STORAGE'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      ******************************************************************
      *    PROCEDIMENTOS INICIAIS DO PROGRAMA.                         *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*
      *---------------------------------------------------------------*
      * AREA DE MOVIMENTACAO DA ROTINA MESU E DE ABEND                *
      *---------------------------------------------------------------*
      *
           MOVE        'MESU9410'  TO          WRK-MODULO-MESUX0.
           MOVE        'POOL7100'  TO          WRK-ABEND-MESUX0.
      *
           CALL 'BRAD1050'.

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-VERIFICAR-VAZIO.

           PERFORM 3000-PROCESSAR UNTIL
                                  WRK-FS-REDUZ23F EQUAL '10'.

           PERFORM 4000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ABERTURA DOS ARQUIVOS E TESTE DE FILE-STATUS.               *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT  REDUZ23F
                OUTPUT REDUZ23W.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTAR FILE-STATUS DOS ARQUIVOS.                            *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-REDUZ23F.

           PERFORM 1120-TESTAR-FS-REDUZ23W.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTAR FILE-STATUS DO ARQUIVO REDUZ23F.                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1110-TESTAR-FS-REDUZ23F         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-REDUZ23F         NOT EQUAL '00'
               MOVE 'REDUZ23F'         TO  WRK-NOME-ARQ
               MOVE  WRK-FS-REDUZ23F   TO  WRK-FILE-STATUS
               MOVE  WRK-ERRO-BRAD7100 TO  ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTAR FILE-STATUS DO ARQUIVO REDUZ23W.                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1120-TESTAR-FS-REDUZ23W         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-REDUZ23W         NOT EQUAL '00'
               MOVE 'REDUZ23W'         TO  WRK-NOME-ARQ
               MOVE  WRK-FS-REDUZ23W   TO  WRK-FILE-STATUS
               MOVE  WRK-ERRO-BRAD7100 TO  ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICAR SE OS ARQUIVOS DE ENTRADA ESTAO VAZIOS.           *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-LER-REDUZ23F.

           IF  WRK-FS-REDUZ23F         EQUAL '10'
               DISPLAY '***************** GFCT4720 *****************'
               DISPLAY '*                                          *'
               DISPLAY '*         ARQUIVO REDUZ23F VAZIO           *'
               DISPLAY '*                                          *'
               DISPLAY '***************** GFCT4720 *****************'
               PERFORM 4000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    LEITURA DO ARQUIVO REDUZ23F.                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-LER-REDUZ23F               SECTION.
      *----------------------------------------------------------------*

           READ REDUZ23F               INTO GFCT51-REDUZ23A.

           IF  WRK-FS-REDUZ23F         EQUAL '10'
               GO TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1110-TESTAR-FS-REDUZ23F.

           MOVE GFCT51-AGENCIA         TO CJUNC-DEPDC OF CLIEV008.
           MOVE GFCT51-CONTA           TO CCTA-CLI    OF CLIEV008.

           ADD 1                       TO ACU-LIDOS-REDUZ23F.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      ******************************************************************
      *    PROCESSAMENTO PRINCIPAL DO PROGRAMA.                        *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 3100-ACESSAR-CTA-CORRENTE.

           IF  WRK-ERRO                EQUAL ZEROS
               PERFORM 3300-ACESSAR-CLIEV004
           ELSE
               PERFORM 3200-ACESSAR-CTA-POUPANCA

               IF  WRK-ERRO            EQUAL ZEROS
                   PERFORM 3300-ACESSAR-CLIEV004
               ELSE
                   MOVE SPACES             TO GFCT51-TIPO-PSSOA
ST25X6*            MOVE ZEROS              TO GFCT51-TIPO-CTA
ST25X6*                                       GFCT51-CPF-CNPJ
ST25X6*                                       GFCT51-FILIAL
ST25X6*                                       GFCT51-CTRL
ST25X6             MOVE ZEROS              TO GFCT51-TIPO-CTA
ST25X6                                        GFCT51-CTRL
ST25X6             MOVE SPACES             TO GFCT51-CPF-CNPJ
ST25X6                                        GFCT51-FILIAL
               END-IF
           END-IF.

           PERFORM 3400-GRAVAR-REDUZ23W.

           PERFORM 2100-LER-REDUZ23F.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3100-ACESSAR-CTA-CORRENTE       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             SELECT
                    CCTA_CLI,
                    CID_CLI,
                    CTPO_CTA
             INTO
                   :CLIEV008.CCTA-CLI,
                   :CLIEV008.CID-CLI,
                   :CLIEV008.CTPO-CTA
             FROM   DB2PRD.V01CTA_CORRENTE
             WHERE
                    CJUNC_DEPDC         = :CLIEV008.CJUNC-DEPDC
             AND    CCTA_CLI            = :CLIEV008.CCTA-CLI
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'               )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'V01CTA_CORRENTE'  TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '0010'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 1                  TO WRK-ERRO
           ELSE
               MOVE CID-CLI OF CLIEV008
                                       TO WRK-CID-CLI
               MOVE CTPO-CTA OF CLIEV008
                                       TO WRK-CTPO-CTA
           END-IF.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3200-ACESSAR-CTA-POUPANCA       SECTION.
      *----------------------------------------------------------------*

           MOVE GFCT51-AGENCIA         TO CJUNC-DEPDC OF CLIEV007.
           MOVE GFCT51-CONTA           TO CCTA-CLI    OF CLIEV007.

           EXEC SQL
             SELECT
                    CCTA_CLI,
                    CID_CLI,
                    CTPO_CTA
             INTO
                   :CLIEV007.CCTA-CLI,
                   :CLIEV007.CID-CLI,
                   :CLIEV007.CTPO-CTA
             FROM   DB2PRD.V01CTA_POUPANCA
             WHERE
                    CJUNC_DEPDC         = :CLIEV007.CJUNC-DEPDC
             AND    CCTA_CLI            = :CLIEV007.CCTA-CLI
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'               )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'V01CTA_POUPANCA'  TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '0020'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 1                  TO WRK-ERRO
           ELSE
               MOVE CID-CLI OF CLIEV007
                                       TO WRK-CID-CLI
               MOVE CTPO-CTA OF CLIEV007
                                       TO WRK-CTPO-CTA
           END-IF.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3300-ACESSAR-CLIEV004           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-CID-CLI            TO CID-CLI OF CLIEV004.

           EXEC SQL
             SELECT
                    IPSSOA_COPLT,
                    CTPO_PSSOA,
                    CCGC_CPF,
                    CFLIAL_CGC,
                    CCTRL_CPF_CGC
             INTO
                   :CLIEV004.IPSSOA-COPLT,
                   :CLIEV004.CTPO-PSSOA,
                   :CLIEV004.CCGC-CPF,
                   :CLIEV004.CFLIAL-CGC,
                   :CLIEV004.CCTRL-CPF-CGC
             FROM   DB2PRD.V01CLIENTE_AGENCIA
             WHERE
                    CID_CLI = :CLIEV004.CID-CLI
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'               )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'V01CTA_POUPANCA'  TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '0020'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE SPACES             TO GFCT51-TIPO-PSSOA
ST25X6*         MOVE ZEROS              TO GFCT51-TIPO-CTA
ST25X6*                                    GFCT51-CPF-CNPJ
ST25X6*                                    GFCT51-FILIAL
ST25X6*                                    GFCT51-CTRL
ST25X6         MOVE ZEROS              TO GFCT51-TIPO-CTA
ST25X6                                    GFCT51-CTRL
ST25X6         MOVE SPACES             TO GFCT51-CPF-CNPJ
ST25X6                                    GFCT51-FILIAL
           ELSE
               MOVE CTPO-PSSOA OF CLIEV004
                                       TO GFCT51-TIPO-PSSOA
               MOVE WRK-CTPO-CTA       TO GFCT51-TIPO-CTA
               MOVE CCGC-CPF           OF CLIEV004
                                       TO GFCT51-CPF-CNPJ
               MOVE CFLIAL-CGC         OF CLIEV004
                                       TO GFCT51-FILIAL
               MOVE CCTRL-CPF-CGC      OF CLIEV004
                                       TO GFCT51-CTRL
           END-IF.

      *----------------------------------------------------------------*
       3300-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    GRAVAR REGISTROS NO ARQUIVO REDUZ23W                        *
      ******************************************************************
      *----------------------------------------------------------------*
       3400-GRAVAR-REDUZ23W            SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           WRITE FD-REDUZ23W           FROM GFCT51-REDUZ23A.

           PERFORM 1120-TESTAR-FS-REDUZ23W.

           ADD 1                       TO ACU-GRAVA-REDUZ23W.

      *----------------------------------------------------------------*
       3400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    FINALIZAR PROGRAMA.                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           IF  ACU-LIDOS-REDUZ23F      GREATER ZEROS
               PERFORM 4100-EMITIR-DISPLAY
           END-IF

           CLOSE REDUZ23F
                 REDUZ23W.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

           STOP RUN.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    EMISSAO DE TOTAIS DE LIDOS E GRAVADOS.                      *
      ******************************************************************
      *----------------------------------------------------------------*
       4100-EMITIR-DISPLAY            SECTION.
      *----------------------------------------------------------------*

           DISPLAY '*--------------------------------------------*'.
           DISPLAY '*              PROGRAMA GFCT4720             *'.
           DISPLAY '*              -----------------             *'.
           DISPLAY '*                                            *'.
           DISPLAY '*     PROCESSAMENTO EFETUADO COM SUCESSO     *'.
           DISPLAY '*--------------------------------------------*'.
           DISPLAY '*--------------------------------------------*'.
           DISPLAY '*                                            *'.

           MOVE ACU-LIDOS-REDUZ23F     TO WRK-MASCARA.

           DISPLAY '* REGS LIDOS REDUZ23F    = ' WRK-MASCARA   ' *'.
           DISPLAY '*                                            *'.

           MOVE ACU-GRAVA-REDUZ23W     TO WRK-MASCARA.

           DISPLAY '* REGS GRAVADOS REDUZ23W = ' WRK-MASCARA   ' *'.
           DISPLAY '*                                            *'.
           DISPLAY '*--------------------------------------------*'.

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TRATAMENTO PARA CANCELAMENTO DO PROGRAMA.                   *
      ******************************************************************
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           MOVE 'APL'                  TO ERR-TIPO-ACESSO.
           MOVE 'GFCT4720'             TO ERR-PGM.

           CALL 'POOL7100'             USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
