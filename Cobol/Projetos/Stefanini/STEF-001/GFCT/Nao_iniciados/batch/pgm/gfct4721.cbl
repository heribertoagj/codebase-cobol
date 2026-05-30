      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT4721.
       AUTHOR.     KATIA PEGORELLI.
      *================================================================*
      *                        SONDA IT                                *
      *----------------------------------------------------------------*
      *    PROGRAMA....:  GFCT4721                                     *
      *    PROGRAMADOR.:  KATIA PEGORELLI         - SONDA IT           *
      *    ANALISTA    :  ANTONIO PAGNOCCA        - SONDA IT           *
      *    DATA........:  18/11/2015.                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:  INCLUIR CPF OU CNPJ NO ARQUIVO APURACAO      *
      *                                                                *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                      INCLUDE/BOOK        *
      *                  REDUZ23C                    I#GFCTZY          *
      *                  APURA23W                    I#GFCTZY          *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#GFCTZY - ARQUIVO APURACAO.                                *
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

           SELECT APURA23A ASSIGN      TO UT-S-APURA23A
           FILE STATUS                 IS WRK-FS-APURA23A.

           SELECT APURA23W ASSIGN      TO UT-S-APURA23W
           FILE STATUS                 IS WRK-FS-APURA23W.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:  ARQUIVO DE ENTRADA  -   APURA23A                    *
      *            ORG. SEQUENCIAL     -   LRECL   =   320             *
      *----------------------------------------------------------------*

       FD  APURA23A
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS  0 RECORDS.

       01 FD-APURA23A                  PIC  X(320).

      *    BRAD7100 - TRATAMENTO DE ERRO.                              *
      *----------------------------------------------------------------*
      *   OUTPUT:  ARQUIVO DE SAIDA    -   APURA23W                    *
      *            ORG. SEQUENCIAL     -   LRECL   =   320             *
      *----------------------------------------------------------------*

       FD  APURA23W
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS  0 RECORDS.

       01 FD-APURA23W                  PIC  X(320).

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

       77  ACU-LIDOS-APURA23A          PIC  9(013) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-APURA23W          PIC  9(013) COMP-3  VALUE ZEROS.
       77  WRK-ERRO                    PIC  9(001)         VALUE ZEROS.

       01  WRK-CID-CLI                 PIC  X(026)         VALUE SPACES.
       01  WRK-CTPO-CTA                PIC  9(002)         VALUE ZEROS.

       01 WRK-CONTA-BRAD              PIC S9(13) COMP-3 VALUE ZEROS.
       01 WRK-CONTA-BRAD-R  REDEFINES  WRK-CONTA-BRAD.
          05 FILLER                   PIC  X(03).
          05 WRK-RESTO-CTA            PIC S9(07) COMP-3.

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

       01  WRK-FS-APURA23A             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-APURA23W             PIC  X(002)         VALUE SPACES.

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

       COPY 'I#GFCTZY'.

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
                                  WRK-FS-APURA23A EQUAL '10'.

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

           OPEN INPUT  APURA23A
                OUTPUT APURA23W.

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

           PERFORM 1110-TESTAR-FS-APURA23A.

           PERFORM 1120-TESTAR-FS-APURA23W.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTAR FILE-STATUS DO ARQUIVO APURA23A.                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1110-TESTAR-FS-APURA23A         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-APURA23A         NOT EQUAL '00'
               MOVE 'APURA23A'         TO  WRK-NOME-ARQ
               MOVE  WRK-FS-APURA23A   TO  WRK-FILE-STATUS
               MOVE  WRK-ERRO-BRAD7100 TO  ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTAR FILE-STATUS DO ARQUIVO APURA23W.                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1120-TESTAR-FS-APURA23W         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-APURA23W         NOT EQUAL '00'
               MOVE 'APURA23W'         TO  WRK-NOME-ARQ
               MOVE  WRK-FS-APURA23W   TO  WRK-FILE-STATUS
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

           PERFORM 2100-LER-APURA23A.

           IF  WRK-FS-APURA23A         EQUAL '10'
               DISPLAY '***************** GFCT4721 *****************'
               DISPLAY '*                                          *'
               DISPLAY '*         ARQUIVO APURA23A VAZIO           *'
               DISPLAY '*                                          *'
               DISPLAY '***************** GFCT4721 *****************'
               PERFORM 4000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    LEITURA DO ARQUIVO APURA23A.                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-LER-APURA23A               SECTION.
      *----------------------------------------------------------------*

           READ APURA23A               INTO GFCTZY-REG.

           IF  WRK-FS-APURA23A         EQUAL '10'
               GO TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1110-TESTAR-FS-APURA23A.

           MOVE ZY-CAG-DSTNO-MOVTO     TO CJUNC-DEPDC OF CLIEV008.
           MOVE ZY-CCTA-DSTNO-MOVTO    TO WRK-CONTA-BRAD
           MOVE WRK-RESTO-CTA          TO CCTA-CLI    OF CLIEV008.

           ADD 1                       TO ACU-LIDOS-APURA23A.

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
                   MOVE SPACES             TO ZY-TIPO-PSSOA
ST25X6*            MOVE ZEROS              TO ZY-TIPO-CTA
ST25X6*                                       ZY-CPF-CNPJ-CLI
ST25X6*                                       ZY-FILIAL-CLI
ST25X6*                                       ZY-CONTROLE-CLI
ST25X6            MOVE ZEROS              TO ZY-TIPO-CTA
ST25X6                                       ZY-CONTROLE-CLI
ST25X6            MOVE SPACES             TO ZY-CPF-CNPJ-CLI
ST25X6                                       ZY-FILIAL-CLI
               END-IF
           END-IF.

           PERFORM 3400-GRAVAR-APURA23W.

           PERFORM 2100-LER-APURA23A.

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

           MOVE ZY-CAG-DSTNO-MOVTO     TO CJUNC-DEPDC OF CLIEV007.
           MOVE ZY-CCTA-DSTNO-MOVTO    TO WRK-CONTA-BRAD
           MOVE WRK-RESTO-CTA          TO CCTA-CLI    OF CLIEV007.

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
               MOVE SPACES             TO ZY-TIPO-PSSOA
ST25X6*         MOVE ZEROS              TO ZY-TIPO-CTA
ST25X6*                                    ZY-CPF-CNPJ-CLI
ST25X6*                                    ZY-FILIAL-CLI
ST25X6*                                    ZY-CONTROLE-CLI
ST25X6         MOVE ZEROS              TO ZY-TIPO-CTA
ST25X6                                    ZY-CONTROLE-CLI
ST25X6         MOVE SPACES             TO ZY-CPF-CNPJ-CLI
ST25X6                                    ZY-FILIAL-CLI
           ELSE
               MOVE CTPO-PSSOA OF CLIEV004
                                       TO ZY-TIPO-PSSOA
               MOVE WRK-CTPO-CTA       TO ZY-TIPO-CTA
ST25X6*        MOVE CCGC-CPF           OF CLIEV004
ST25X6*                                    TO ZY-CPF-CNPJ-CLI
ST25X6         MOVE CCGC-CPF-ST        OF CLIEV004
ST25X6                                    TO ZY-CPF-CNPJ-CLI
ST25X6*        MOVE CFLIAL-CGC         OF CLIEV004
ST25X6*                                    TO ZY-FILIAL-CLI
ST25X6         MOVE CFLIAL-CGC-ST      OF CLIEV004
ST25X6                                    TO ZY-FILIAL-CLI
               MOVE CCTRL-CPF-CGC      OF CLIEV004
                                       TO ZY-CONTROLE-CLI
           END-IF.

      *----------------------------------------------------------------*
       3300-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    GRAVAR REGISTROS NO ARQUIVO APURA23W                        *
      ******************************************************************
      *----------------------------------------------------------------*
       3400-GRAVAR-APURA23W            SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           WRITE FD-APURA23W           FROM GFCTZY-REG.

           PERFORM 1120-TESTAR-FS-APURA23W.

           ADD 1                       TO ACU-GRAVA-APURA23W.

      *----------------------------------------------------------------*
       3400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    FINALIZAR PROGRAMA.                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           IF  ACU-LIDOS-APURA23A      GREATER ZEROS
               PERFORM 4100-EMITIR-DISPLAY
           END-IF

           CLOSE APURA23A
                 APURA23W.

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
           DISPLAY '*              PROGRAMA GFCT4721             *'.
           DISPLAY '*              -----------------             *'.
           DISPLAY '*                                            *'.
           DISPLAY '*     PROCESSAMENTO EFETUADO COM SUCESSO     *'.
           DISPLAY '*--------------------------------------------*'.
           DISPLAY '*--------------------------------------------*'.
           DISPLAY '*                                            *'.

           MOVE ACU-LIDOS-APURA23A     TO WRK-MASCARA.

           DISPLAY '* REGS LIDOS APURA23A    = ' WRK-MASCARA   ' *'.
           DISPLAY '*                                            *'.

           MOVE ACU-GRAVA-APURA23W     TO WRK-MASCARA.

           DISPLAY '* REGS GRAVADOS APURA23W = ' WRK-MASCARA   ' *'.
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
           MOVE 'GFCT4721'             TO ERR-PGM.

           CALL 'POOL7100'             USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
