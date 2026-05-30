      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT1666.
       AUTHOR.     BELCLEI FASOLO.
      *================================================================*
      *              C P M  B R A X I S  -  S I S T E M A S            *
      *----------------------------------------------------------------*
      *    PROGRAMA....:  GFCT1666                                     *
      *    PROGRAMADOR.:  BELCLEI FASOLO          - CPM BRAXIS - P BCO *
      *    ANALISTA CPM:  ELIAS BOSCATO           - CPM BRAXIS - P BCO *
      *    ANALISTA....:  RICARDO PEREIRA DA SILVA- PROCKWORK  - GP 50 *
      *    DATA........:  02/10/2007                                   *
      *----------------------------------------------------------------*
      *    OBJETIVO....:  GERAR UM REGISTRO DE SAIDA PARA CADA CARTEIRA*
      *      DO GRUPO INDICADO NO ARQUIVO DE ENTRADA.                  *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                      INCLUDE/BOOK        *
      *                  REDUZ23C                    I#GFCT51          *
      *                  REDUZ23D                    I#GFCT51          *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                     INCLUDE/BOOK        *
      *                    DB2PRD.TASSOC_GRP_CART    GFCTB0I5          *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#GFCT51 - ARQUIVO REDUZIDO PARA PESQUISA SIGB.             *
      *    I#BRAD7C - AREA UTILIZADA PELA BRAD7100.                    *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    BRAD1050 - FAZ CONEXAO COM DB2.                             *
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

           SELECT REDUZ23C ASSIGN      TO UT-S-REDUZ23C
           FILE STATUS                 IS WRK-FS-REDUZ23C.

           SELECT REDUZ23D ASSIGN      TO UT-S-REDUZ23D
           FILE STATUS                 IS WRK-FS-REDUZ23D.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:  ARQUIVO DE ENTRADA  -   REDUZ23C                    *
      *            ORG. SEQUENCIAL     -   LRECL   =   050             *
      *----------------------------------------------------------------*

       FD  REDUZ23C
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS  0 RECORDS.

       01 FD-REDUZ23C                  PIC  X(050).

      *----------------------------------------------------------------*
      *   OUTPUT:  ARQUIVO DE SAIDA    -   REDUZ23D                    *
      *            ORG. SEQUENCIAL     -   LRECL   =   050             *
      *----------------------------------------------------------------*

       FD  REDUZ23D
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS  0 RECORDS.

       01 FD-REDUZ23D                  PIC  X(050).

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

       77  ACU-LIDOS-REDUZ23C          PIC  9(013) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-REDUZ23D          PIC  9(013) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           'VARIAVEIS AUXILIARES'.
      *----------------------------------------------------------------*

       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.

       77  WRK-FIM-CSR01-GFCTB0I5      PIC  X(001)         VALUE 'N'.
       77  WRK-MASCARA                 PIC  Z.ZZZ.ZZZ.ZZZ.ZZ9
                                                           VALUE ZEROS.
       01  WRK-CHV-AUX                 PIC +99999          VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-CHV-AUX.
           05  FILLER                  PIC  X(001).
           05  WRK-CHV-AUX-INT         PIC  9(005).


      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(031)         VALUE
           'AREA PARA TESTES DE FILE-STATUS'.
      *----------------------------------------------------------------*

       01  WRK-FS-REDUZ23C             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-REDUZ23D             PIC  X(002)         VALUE SPACES.

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

       COPY I#GFCT51.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA PARA BRAD7100'.
      *----------------------------------------------------------------*

       COPY I#BRAD7C.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA PARA DB2'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0I5
           END-EXEC.

           EXEC SQL
             DECLARE CSR01-GFCTB0I5 CURSOR   FOR
               SELECT
                   CCART_ANLSE_GERC
             FROM  DB2PRD.TASSOC_GRP_CART
             WHERE
                   CGRP_CART_SIST     = :GFCTB0I5.CGRP-CART-SIST
           END-EXEC.

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

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-VERIFICAR-VAZIO.

           PERFORM 3000-PROCESSAR      UNTIL
                   WRK-FS-REDUZ23C     EQUAL '10'.

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

           CALL 'BRAD1050'.

           OPEN INPUT  REDUZ23C
                OUTPUT REDUZ23D.

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

           PERFORM 1110-TESTAR-FS-REDUZ23C.

           PERFORM 1120-TESTAR-FS-REDUZ23D.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTAR FILE-STATUS DO ARQUIVO REDUZ23C.                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1110-TESTAR-FS-REDUZ23C         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-REDUZ23C         NOT EQUAL '00'
               MOVE 'REDUZ23C'         TO  WRK-NOME-ARQ
               MOVE  WRK-FS-REDUZ23C   TO  WRK-FILE-STATUS
               MOVE  WRK-ERRO-BRAD7100 TO  ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTAR FILE-STATUS DO ARQUIVO REDUZ23D.                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1120-TESTAR-FS-REDUZ23D         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-REDUZ23D         NOT EQUAL '00'
               MOVE 'REDUZ23D'         TO  WRK-NOME-ARQ
               MOVE  WRK-FS-REDUZ23D   TO  WRK-FILE-STATUS
               MOVE  WRK-ERRO-BRAD7100 TO  ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICAR SE ARQUIVO DE ENTRADA ESTA VAZIO.                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-LER-REDUZ23C.

           IF  WRK-FS-REDUZ23C         EQUAL '10'
               DISPLAY '***************** GFCT1666 *****************'
               DISPLAY '*                                          *'
               DISPLAY '*         ARQUIVO REDUZ23C VAZIO           *'
               DISPLAY '*                                          *'
               DISPLAY '*         PROCESSAMENTO ENCERRADO          *'
               DISPLAY '*                                          *'
               DISPLAY '***************** GFCT1666 *****************'
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    LEITURA DO ARQUIVO REDUZ23C.                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-LER-REDUZ23C               SECTION.
      *----------------------------------------------------------------*

           READ REDUZ23C               INTO GFCT51-REDUZ23A.

           IF  WRK-FS-REDUZ23C         EQUAL '10'
               GO TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1110-TESTAR-FS-REDUZ23C.

           ADD 1                       TO ACU-LIDOS-REDUZ23C.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCESSAMENTO PRINCIPAL DO PROGRAMA.                        *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 3100-ABRIR-CSR01-GFCTB0I5.

           PERFORM 3200-LER-CSR01-GFCTB0I5.

           PERFORM 3300-PROCESSAR-CSR01-GFCTB0I5   UNTIL
               WRK-FIM-CSR01-GFCTB0I5              EQUAL 'S'.

           PERFORM 3400-FECHAR-CSR01-GFCTB0I5.

           PERFORM 2100-LER-REDUZ23C.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ABRIR O CURSOR CSR01-GFCTB0I5                               *
      ******************************************************************
      *----------------------------------------------------------------*
       3100-ABRIR-CSR01-GFCTB0I5       SECTION.
      *----------------------------------------------------------------*

           MOVE GFCT51-GRUPO           TO CGRP-CART-SIST   OF GFCTB0I5.

           EXEC SQL
               OPEN CSR01-GFCTB0I5
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS) OR
               (SQLWARN0               EQUAL 'W')
                MOVE 'DB2'             TO ERR-TIPO-ACESSO
                MOVE 'TASSOC_GRP_CART' TO ERR-DBD-TAB
                MOVE 'OPEN      '      TO ERR-FUN-COMANDO
                MOVE SQLCODE           TO ERR-SQL-CODE
                MOVE '0010'            TO ERR-LOCAL
                MOVE SPACES            TO ERR-SEGM
                PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE 'N'                    TO WRK-FIM-CSR01-GFCTB0I5.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    LER O CURSOR CSR01-GFCTB0I5                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       3200-LER-CSR01-GFCTB0I5         SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH CSR01-GFCTB0I5 INTO
                   :GFCTB0I5.CCART-ANLSE-GERC
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS AND +100) OR
               (SQLWARN0               EQUAL 'W')
                MOVE 'DB2'             TO ERR-TIPO-ACESSO
                MOVE 'TASSOC_GRP_CART' TO ERR-DBD-TAB
                MOVE 'FETCH     '      TO ERR-FUN-COMANDO
                MOVE SQLCODE           TO ERR-SQL-CODE
                MOVE '0020'            TO ERR-LOCAL
                MOVE SPACES            TO ERR-SEGM
                PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-CSR01-GFCTB0I5
           END-IF.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    OBTER CARTEIRA E PROCESSAR ATE O FINAL DO CURSOR            *
      ******************************************************************
      *----------------------------------------------------------------*
       3300-PROCESSAR-CSR01-GFCTB0I5   SECTION.
      *----------------------------------------------------------------*

           PERFORM 3330-GRAVAR-REDUZ23D.

           PERFORM 3200-LER-CSR01-GFCTB0I5.

      *----------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    GRAVAR REGISTROS NO ARQUIVO REDUZ23D                        *
      ******************************************************************
      *----------------------------------------------------------------*
       3330-GRAVAR-REDUZ23D            SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO GFCT51-DADO-CONS.
           MOVE CCART-ANLSE-GERC       OF GFCTB0I5
                                       TO WRK-CHV-AUX.
           MOVE WRK-CHV-AUX-INT        TO GFCT51-CARTEIRA.
           MOVE  WRK-GRAVACAO          TO WRK-OPERACAO.

           WRITE FD-REDUZ23D           FROM GFCT51-REDUZ23A.

           PERFORM 1120-TESTAR-FS-REDUZ23D.

           ADD 1                       TO ACU-GRAVA-REDUZ23D.

      *----------------------------------------------------------------*
       3330-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    FECHAR O CURSOR CSR01-GFCTB0I5                              *
      ******************************************************************
      *----------------------------------------------------------------*
       3400-FECHAR-CSR01-GFCTB0I5      SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR01-GFCTB0I5
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS) OR
               (SQLWARN0               EQUAL 'W')
                MOVE 'DB2'             TO ERR-TIPO-ACESSO
                MOVE 'TASSOC_GRP_CART' TO ERR-DBD-TAB
                MOVE 'CLOSE     '      TO ERR-FUN-COMANDO
                MOVE SQLCODE           TO ERR-SQL-CODE
                MOVE '0030'            TO ERR-LOCAL
                MOVE SPACES            TO ERR-SEGM
                PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       3400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    FINALIZAR PROGRAMA.                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           IF  ACU-LIDOS-REDUZ23C      GREATER ZEROS
               PERFORM 4100-EMITIR-DISPLAY
           END-IF

           CLOSE REDUZ23C
                 REDUZ23D.

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
           DISPLAY '*              PROGRAMA GFCT1666             *'.
           DISPLAY '*              -----------------             *'.
           DISPLAY '*                                            *'.
           DISPLAY '*     PROCESSAMENTO EFETUADO COM SUCESSO     *'.
           DISPLAY '*--------------------------------------------*'.
           DISPLAY '*--------------------------------------------*'.
           DISPLAY '*                                            *'.

           MOVE ACU-LIDOS-REDUZ23C     TO WRK-MASCARA.

           DISPLAY '* REGS LIDOS REDUZ23C    = ' WRK-MASCARA   ' *'.
           DISPLAY '*                                            *'.

           MOVE ACU-GRAVA-REDUZ23D     TO WRK-MASCARA.

           DISPLAY '* REGS GRAVADOS REDUZ23D = ' WRK-MASCARA   ' *'.
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

           IF  ERR-TIPO-ACESSO         NOT EQUAL 'DB2'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
           END-IF.

           MOVE 'GFCT1666'             TO ERR-PGM.

           CALL 'BRAD7100'             USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
