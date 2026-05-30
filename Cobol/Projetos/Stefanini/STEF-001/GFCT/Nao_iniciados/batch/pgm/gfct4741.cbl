      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT4741.
       AUTHOR.     KATIA PEGORELLI.
      *================================================================*
      *                        SONDA IT                                *
      *----------------------------------------------------------------*
      *    PROGRAMA....:  GFCT4741                                     *
      *    PROGRAMADOR.:  KATIA PEGORELLI         - SONDA IT           *
      *    ANALISTA    :  ANTONIO PAGNOCCA        - SONDA IT           *
      *    DATA........:  13/01/2016.                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:  INCLUIR CPF OU CNPJ NO ARQUIVO DE AG/CTA     *
      *                                                                *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                      INCLUDE/BOOK        *
      *                  BOBIFVLR                    GFCTWF40          *
      *                  BONIFCPF                    GFCTWF40          *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    GFCTWF40 - ARQUIVO APURACAO.                                *
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

           SELECT BONIFVLR ASSIGN      TO UT-S-BONIFVLR
           FILE STATUS                 IS WRK-FS-BONIFVLR.

           SELECT BONIFCPF ASSIGN      TO UT-S-BONIFCPF
           FILE STATUS                 IS WRK-FS-BONIFCPF.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:  ARQUIVO DE ENTRADA  -   BONIFVLR                    *
      *            ORG. SEQUENCIAL     -   LRECL   =   100             *
      *----------------------------------------------------------------*

       FD  BONIFVLR
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS  0 RECORDS.

       01 FD-BONIFVLR                  PIC  X(100).

      *    BRAD7100 - TRATAMENTO DE ERRO.                              *
      *----------------------------------------------------------------*
      *   OUTPUT:  ARQUIVO DE SAIDA    -   BONIFCPF                    *
      *            ORG. SEQUENCIAL     -   LRECL   =   100             *
      *----------------------------------------------------------------*

       FD  BONIFCPF
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS  0 RECORDS.

       01 FD-BONIFCPF                  PIC  X(100).

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

       77  ACU-LIDOS-BONIFVLR          PIC  9(013) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-BONIFCPF          PIC  9(013) COMP-3  VALUE ZEROS.
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

       01  WRK-FS-BONIFVLR             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-BONIFCPF             PIC  X(002)         VALUE SPACES.

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

       COPY 'GFCTWF40'.

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
                                  WRK-FS-BONIFVLR EQUAL '10'.

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

           OPEN INPUT  BONIFVLR
                OUTPUT BONIFCPF.

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

           PERFORM 1110-TESTAR-FS-BONIFVLR.

           PERFORM 1120-TESTAR-FS-BONIFCPF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTAR FILE-STATUS DO ARQUIVO BONIFVLR.                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1110-TESTAR-FS-BONIFVLR         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-BONIFVLR         NOT EQUAL '00'
               MOVE 'BONIFVLR'         TO  WRK-NOME-ARQ
               MOVE  WRK-FS-BONIFVLR   TO  WRK-FILE-STATUS
               MOVE  WRK-ERRO-BRAD7100 TO  ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTAR FILE-STATUS DO ARQUIVO BONIFCPF.                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1120-TESTAR-FS-BONIFCPF         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-BONIFCPF         NOT EQUAL '00'
               MOVE 'BONIFCPF'         TO  WRK-NOME-ARQ
               MOVE  WRK-FS-BONIFCPF   TO  WRK-FILE-STATUS
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

           PERFORM 2100-LER-BONIFVLR.

           IF  WRK-FS-BONIFVLR         EQUAL '10'
               DISPLAY '***************** GFCT4741 *****************'
               DISPLAY '*                                          *'
               DISPLAY '*         ARQUIVO BONIFVLR VAZIO           *'
               DISPLAY '*                                          *'
               DISPLAY '***************** GFCT4741 *****************'
               PERFORM 4000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    LEITURA DO ARQUIVO BONIFVLR.                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-LER-BONIFVLR               SECTION.
      *----------------------------------------------------------------*

           READ BONIFVLR               INTO GFCTWF40-REGISTRO.

           IF  WRK-FS-BONIFVLR         EQUAL '10'
               GO TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1110-TESTAR-FS-BONIFVLR.

           IF GFCTWF40-AGENCIA         EQUAL ZEROS OR
              GFCTWF40-CONTA           EQUAL ZEROS
              GO TO 2100-LER-BONIFVLR
           END-IF.

           MOVE GFCTWF40-AGENCIA       TO CJUNC-DEPDC OF CLIEV008.
           MOVE GFCTWF40-CONTA         TO WRK-CONTA-BRAD
           MOVE WRK-RESTO-CTA          TO CCTA-CLI    OF CLIEV008.

           ADD 1                       TO ACU-LIDOS-BONIFVLR.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      ******************************************************************
      *    PROCESSAMENTO PRINCIPAL DO PROGRAMA.                        *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS           TO SQLCODE.
MV0516     MOVE    ZEROS        TO   WRK-ERRO.
           PERFORM 3100-ACESSAR-CTA-CORRENTE.

           IF  WRK-ERRO                EQUAL ZEROS
               PERFORM 3300-ACESSAR-CLIEV004
           ELSE
MV0516         MOVE    ZEROS   TO   WRK-ERRO
               PERFORM 3200-ACESSAR-CTA-POUPANCA

               IF  WRK-ERRO            EQUAL ZEROS
                   PERFORM 3300-ACESSAR-CLIEV004
               ELSE
                   MOVE FD-BONIFVLR    TO FD-BONIFCPF
ST24X6*            MOVE ZEROS          TO GFCTWF40-CPF-CNPJ
ST24X6             MOVE SPACES         TO GFCTWF40-CPF-CNPJ
               END-IF
           END-IF.

           PERFORM 3400-GRAVAR-BONIFCPF.

           PERFORM 2100-LER-BONIFVLR.

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

           MOVE GFCTWF40-AGENCIA       TO CJUNC-DEPDC OF CLIEV007.
           MOVE GFCTWF40-CONTA         TO WRK-CONTA-BRAD
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
                    CCTRL_CPF_CGC,
ST25X6              CCGC_CPF_ST,
ST25X6              CFLIAL_CGC_ST,
ST25X6              CCTRL_CPF_CGC_ST                    
             INTO
                   :CLIEV004.IPSSOA-COPLT,
                   :CLIEV004.CTPO-PSSOA,
                   :CLIEV004.CCGC-CPF,
                   :CLIEV004.CFLIAL-CGC,
                   :CLIEV004.CCTRL-CPF-CGC,
ST25X6             :CLIEV004.CCGC-CPF-ST,
ST25X6             :CLIEV004.CFLIAL-CGC-ST,
ST25X6             :CLIEV004.CCTRL-CPF-CGC-ST
             FROM   DB2PRD.V01CLIENTE_AGENCIA
             WHERE
                    CID_CLI = :CLIEV004.CID-CLI
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'               )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'CLIENTE_AGENCIA'  TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '0025'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE FD-BONIFVLR        TO FD-BONIFCPF
ST25X6*        M0VE ZEROS              TO GFCTWF40-CPF-CNPJ
ST25X6         MOVE ZEROS              TO GFCTWF40-CPF-CNPJ
           ELSE
               MOVE FD-BONIFVLR        TO FD-BONIFCPF
ST25X6*        MOVE CCGC-CPF           OF CLIEV004
ST25X6*                                TO GFCTWF40-PRINCIPAL
ST25X6*        MOVE CFLIAL-CGC         OF CLIEV004
ST25X6*                                TO GFCTWF40-FILIAL
ST25X6*        MOVE CCTRL-CPF-CGC      OF CLIEV004
ST25X6*                                TO GFCTWF40-CONTROLE

ST25X6         MOVE CCGC-CPF-ST        OF CLIEV004
ST25X6                                 TO GFCTWF40-PRINCIPAL
ST25X6         MOVE CFLIAL-CGC-ST      OF CLIEV004
ST25X6                                 TO GFCTWF40-FILIAL
ST25X6         MOVE CCTRL-CPF-CGC-ST   OF CLIEV004
ST25X6                                 TO GFCTWF40-CONTROLE
           END-IF.

      *----------------------------------------------------------------*
       3300-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    GRAVAR REGISTROS NO ARQUIVO BONIFCPF                        *
      ******************************************************************
      *----------------------------------------------------------------*
       3400-GRAVAR-BONIFCPF            SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           WRITE FD-BONIFCPF           FROM GFCTWF40-REGISTRO.

           PERFORM 1120-TESTAR-FS-BONIFCPF.

           ADD 1                       TO ACU-GRAVA-BONIFCPF.

      *----------------------------------------------------------------*
       3400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    FINALIZAR PROGRAMA.                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           IF  ACU-LIDOS-BONIFVLR      GREATER ZEROS
               PERFORM 4100-EMITIR-DISPLAY
           END-IF

           CLOSE BONIFVLR
                 BONIFCPF.

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
           DISPLAY '*              PROGRAMA GFCT4741             *'.
           DISPLAY '*              -----------------             *'.
           DISPLAY '*                                            *'.
           DISPLAY '*     PROCESSAMENTO EFETUADO COM SUCESSO     *'.
           DISPLAY '*--------------------------------------------*'.
           DISPLAY '*--------------------------------------------*'.
           DISPLAY '*                                            *'.

           MOVE ACU-LIDOS-BONIFVLR     TO WRK-MASCARA.

           DISPLAY '* REGS LIDOS BONIFVLR    = ' WRK-MASCARA   ' *'.
           DISPLAY '*                                            *'.

           MOVE ACU-GRAVA-BONIFCPF     TO WRK-MASCARA.

           DISPLAY '* REGS GRAVADOS BONIFCPF = ' WRK-MASCARA   ' *'.
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
           MOVE 'GFCT4741'             TO ERR-PGM.

           CALL 'POOL7100'             USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
