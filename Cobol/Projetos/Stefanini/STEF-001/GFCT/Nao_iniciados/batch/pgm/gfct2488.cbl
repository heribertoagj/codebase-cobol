      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT2488.
       AUTHOR MARCUS VINICIUS.
      *================================================================*
      *                 W I P R O   I N F O S E R V E R                *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : GFCT2488                                     *
      *    ANALISTA     : MARCUS VINICIUS - INFOSERVER - GRUPO 38      *
      *    DATA         : FEV/2018                                     *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      IDENTIFICA ADESOES DAS CESTAS JA VENCIDAS.                *
      *       ATUALMENTE SOMENTE CESTAS NEXT - 1184 E 1187             *
      *       ACRESCENTANDO INFORMACAO DO CPF/CNPJ DO CLIENTE          *
      *                                                                *
      *    ARQUIVOS:                                                   *
      *      DDNAME                              INCLUDE/BOOK          *
      *      ARQDATAS                              I#GFCT06            *
      *      ADESOESE                              GFCTWE04            *
      *      ADESPAST                              --------            *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      BRAD7100 - MODULO TRATAMENTO DE ERRO.                     *
      *                                                                *
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

           SELECT ADESOESE ASSIGN      TO UT-S-ADESOESE
                      FILE STATUS      IS WRK-FS-ADESOESE.

           SELECT ARQDATAS ASSIGN      TO UT-S-ARQDATAS
                      FILE STATUS      IS WRK-FS-ARQDATAS.

           SELECT ADESPAST ASSIGN      TO UT-S-ADESPAST
                      FILE STATUS      IS WRK-FS-ADESPAST.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *================================================================*
       FILE                            SECTION.
      *================================================================*

      *----------------------------------------------------------------*
      *   INPUT:  ADESOES DAS CESTAS NEXT  - 1184 E 1187               *
      *           ORG.SEQUENCIAL  -  LRECL = 094                       *
      *----------------------------------------------------------------*

       FD  ADESOESE
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY GFCTWE04.

      *----------------------------------------------------------------*
      *    INPUT  :  ARQUIVO DE DATAS DA ROTINA DE TARIFAS             *
      *              ORG. SEQUENCIAL   -   LRECL = 100                 *
      *----------------------------------------------------------------*

       FD  ARQDATAS
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCT06'.

      *----------------------------------------------------------------*
      *   OUTPUT: ADESOES DAS CESTAS NEXT - SOMENTE DO PROPRIO MES     *
      *           ORG.SEQUENCIAL  -  LRECL = 110                       *
      *----------------------------------------------------------------*

       FD  ADESPAST
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-ADESPAST-REG.
           05  FD-ADESAO-REG           PIC X(094).
ST25X6*     05  FD-CPF-CNPJ             PIC 9(009).
ST25X6*     05  FD-FILIAL               PIC 9(005).
ST25X6*     05  FD-CONTROLE             PIC 9(002).
ST25X6      05  FD-CPF-CNPJ             PIC X(009).
ST25X6      05  FD-FILIAL               PIC X(004).
ST25X6      05  FD-CONTROLE             PIC 9(002).
ST25X6      05  FILLER                  PIC X(001).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(032)          VALUE
           '*   INICIO DA WORKING GFCT2488 *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(032)          VALUE
           '*         ACUMULADORES         *'.
      *----------------------------------------------------------------*

       01  ACU-ACUMULADORES.
           05 ACU-REG-LIDOS            PIC 9(012) COMP-3   VALUE ZEROS.
           05 ACU-REG-DATAS            PIC 9(012) COMP-3   VALUE ZEROS.
           05 ACU-REG-GRAVADOS         PIC 9(012) COMP-3   VALUE ZEROS.
           05 ACU-REG-DESPREZA         PIC 9(012) COMP-3   VALUE ZEROS.

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(032)          VALUE
           '* VARIAVEIS AUXILIARES         *'.
      *---------------------------------------------------------------*

       01  WRK-AUXILIARES.
           05  WRK-BATCH               PIC X(008)          VALUE
               'BATCH'.
           05  WRK-MASCARA             PIC ZZZ.ZZZ.ZZZ.ZZ9 VALUE ZEROS.

           05  WRK-PERIODO-X.
               10  WRK-AAAA-PER        PIC  9(004)         VALUE ZEROES.
               10  WRK-MM-PER          PIC  9(002)         VALUE ZEROES.
           05  WRK-PERIODO     REDEFINES  WRK-PERIODO-X
                                       PIC  9(006).

           05  WRK-PERIODO-AUX         PIC  9(006)         VALUE ZEROES.

           05  WRK-PERIODO-ARQ         PIC  9(006)         VALUE ZEROES.

           05  WRK-DATA-DB2.
               10  WRK-DIA-DB2         PIC  9(002)         VALUE ZEROES.
               10  FILLER              PIC  X(001)         VALUE '.'.
               10  WRK-MES-DB2         PIC  9(002)         VALUE ZEROES.
               10  FILLER              PIC  X(001)         VALUE '.'.
               10  WRK-ANO-DB2         PIC  9(004)         VALUE ZEROES.

           05  WRK-ANO                 PIC  9(004) COMP-3  VALUE ZEROS.
           05  WRK-MES                 PIC  9(002) COMP-3  VALUE ZEROS.

           05  WRK-DATA-AUX.
               10  WRK-DIA-AUX         PIC  9(002)         VALUE ZEROES.
               10  FILLER              PIC  X(001)         VALUE '.'.
               10  WRK-MES-AUX         PIC  9(002)         VALUE ZEROES.
               10  FILLER              PIC  X(001)         VALUE '.'.
               10  WRK-ANO-AUX         PIC  9(004)         VALUE ZEROES.

           05  WRK-CPF-CNPJ            PIC S9(009)V        VALUE ZEROS.
           05  WRK-CPF-CNPJ-AUX        REDEFINES WRK-CPF-CNPJ
                                       PIC  9(009).

           05  WRK-FILIAL              PIC S9(005)V        VALUE ZEROS.
           05  WRK-FILIAL-AUX          REDEFINES WRK-FILIAL
                                       PIC  9(005).

           05  WRK-CONTROLE            PIC S9(002)V        VALUE ZEROS.
           05  WRK-CONTROLE-AUX        REDEFINES WRK-CONTROLE
                                       PIC  9(002).

           05  WRK-ERRO                PIC  9(001)         VALUE ZEROS.

           05  WRK-CID-CLI             PIC  X(026)         VALUE SPACES.
           05  WRK-CTPO-CTA            PIC  9(002)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(032)          VALUE
           '*  AREA FILE-STATUS            *'.
      *----------------------------------------------------------------*

       01  WRK-FS-ADESOESE             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-ARQDATAS             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-ADESPAST             PIC  X(002)         VALUE SPACES.

       01  WRK-ABERTURA                PIC  X(013)         VALUE
           'NA ABERTURA'.
       01  WRK-LEITURA                 PIC  X(013)         VALUE
           ' NA LEITURA'.
       01  WRK-GRAVACAO                PIC  X(013)         VALUE
           'NA GRAVACAO'.
       01  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(032)          VALUE
           '*  MENSAGENS DE ERRO           *'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-FS.
           05  FILLER                  PIC X(007)          VALUE SPACES.
           05  FILLER                  PIC X(007)          VALUE
               '* ERRO '.
           05  WRK-OPERACAO            PIC X(013)          VALUE SPACES.
           05  FILLER                  PIC X(012)          VALUE
              ' DO ARQUIVO '.
           05  WRK-NOME-ARQ            PIC X(008)          VALUE SPACES.
           05  FILLER                  PIC X(017)          VALUE
              ' - FILE-STATUS = '.
           05  WRK-FS                  PIC X(002)          VALUE SPACES.
           05  FILLER                  PIC X(002)          VALUE ' *'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*       AREA DA BRAD7100       *'.
      *----------------------------------------------------------------*

       COPY I#BRAD7C.

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

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    FIM DA WORKING GFCT2488   *'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
      *    ROTINA PRINCIPAL                                            *
      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           CALL 'BRAD1050'.

           PERFORM 0100-INICIALIZAR-PROGRAMA.

           PERFORM 0500-VERIFICA-VAZIO.

           PERFORM 1000-PROCESSA-NEXT-PAST
                    UNTIL     WRK-FS-ADESOESE   EQUAL '10'.

           PERFORM 1800-PROCEDIMENTOS-FINAIS.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA INICIAL DO PROGRAMA                                  *
      *----------------------------------------------------------------*
       0100-INICIALIZAR-PROGRAMA       SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT  ADESOESE
                       ARQDATAS
                OUTPUT ADESPAST.

           MOVE  WRK-ABERTURA          TO WRK-OPERACAO.

           PERFORM 0200-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       0100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *      TESTE DE FILE STATUS DOS ARQUIVOS                         *
      *----------------------------------------------------------------*
       0200-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 0300-TESTAR-FS-ADESOESE.

           PERFORM 0350-TESTAR-FS-ARQDATAS.

           PERFORM 0400-TESTAR-FS-ADESPAST.

      *----------------------------------------------------------------*
       0200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTE DE FILE STATUS DO ARQUIVO ADESOESE                    *
      *----------------------------------------------------------------*
       0300-TESTAR-FS-ADESOESE         SECTION.
      *----------------------------------------------------------------*

           IF WRK-FS-ADESOESE          NOT EQUAL '00'
              MOVE 'ADESOESE'          TO WRK-NOME-ARQ
              MOVE 'APL'               TO ERR-TIPO-ACESSO
              MOVE WRK-FS-ADESOESE     TO WRK-FS
              MOVE WRK-ERRO-FS         TO ERR-TEXTO

              PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTE DE FILE STATUS DO ARQUIVO ARQDATAS                    *
      *----------------------------------------------------------------*
       0350-TESTAR-FS-ARQDATAS         SECTION.
      *----------------------------------------------------------------*

           IF WRK-FS-ARQDATAS          NOT EQUAL '00'
              MOVE 'ARQDATAS'          TO WRK-NOME-ARQ
              MOVE 'APL'               TO ERR-TIPO-ACESSO
              MOVE WRK-FS-ARQDATAS     TO WRK-FS
              MOVE WRK-ERRO-FS         TO ERR-TEXTO

              PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0350-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTE DE FILE STATUS DO ARQUIVO ADESPAST                    *
      *----------------------------------------------------------------*
       0400-TESTAR-FS-ADESPAST         SECTION.
      *----------------------------------------------------------------*

           IF WRK-FS-ADESPAST          NOT EQUAL '00'
              MOVE 'ADESPAST'          TO WRK-NOME-ARQ
              MOVE 'APL'               TO ERR-TIPO-ACESSO
              MOVE WRK-FS-ADESPAST     TO WRK-FS
              MOVE WRK-ERRO-FS         TO ERR-TEXTO

              PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     VERIFICA SE OS ARQUIVOS DE ENTRADA ESTAO VAZIOS            *
      *----------------------------------------------------------------*
       0500-VERIFICA-VAZIO             SECTION.
      *----------------------------------------------------------------*

           PERFORM 0600-LER-ARQUIVO-ADESOESE.

           IF  WRK-FS-ADESOESE         EQUAL '10'
               DISPLAY '***************** GFCT2488 *****************'
               DISPLAY '*                                          *'
               DISPLAY '*      ARQUIVO COM CESTAS NEXT ESTA        *'
               DISPLAY '*          VAZIO !!!                       *'
               DISPLAY '*                                          *'
               DISPLAY '***************** GFCT2488 *****************'
           END-IF.

           PERFORM 0650-LER-ARQUIVO-ARQDATAS

           IF WRK-FS-ARQDATAS          EQUAL '10'
               DISPLAY '**************** GFCT2488 *****************'
               DISPLAY '*                                         *'
               DISPLAY '*        ARQUIVO DIARIO DE DATAS DO       *'
               DISPLAY '*               TARIFAS VAZIO             *'
               DISPLAY '*                                         *'
               DISPLAY '**************** GFCT2488 *****************'
               MOVE 'ARQDATAS'         TO WRK-NOME-ARQ
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-ARQDATAS    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO

               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     LEITURA DO ARQUIVO ADESOESE                                *
      *----------------------------------------------------------------*
       0600-LER-ARQUIVO-ADESOESE       SECTION.
      *----------------------------------------------------------------*

           READ ADESOESE.

           IF  WRK-FS-ADESOESE         EQUAL '10'
               GO                      TO 0600-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 0300-TESTAR-FS-ADESOESE.

           ADD 1                       TO ACU-REG-LIDOS.

           MOVE  E04-DFIM-ADSAO-INDVD  TO WRK-DATA-AUX.

           MOVE  WRK-ANO-AUX           TO WRK-AAAA-PER.
           MOVE  WRK-MES-AUX           TO WRK-MM-PER.
           MOVE  WRK-PERIODO           TO WRK-PERIODO-ARQ.

      *----------------------------------------------------------------*
       0600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     LEITURA DO ARQUIVO ARQDATAS                                *
      *----------------------------------------------------------------*
       0650-LER-ARQUIVO-ARQDATAS       SECTION.
      *----------------------------------------------------------------*

           READ ARQDATAS.

           IF  WRK-FS-ARQDATAS         EQUAL '10'
               GO                      TO 0650-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 0350-TESTAR-FS-ARQDATAS.

           ADD 1                       TO ACU-REG-DATAS.

           MOVE  GFCT06-DPROCM-ATUAL   TO WRK-DATA-DB2.

           MOVE  WRK-ANO-DB2           TO WRK-ANO.
           MOVE  WRK-MES-DB2           TO WRK-MES.

           IF  WRK-MES EQUAL 01
               MOVE 12                 TO   WRK-MES
               COMPUTE  WRK-ANO  =  WRK-ANO - 1
           ELSE
               COMPUTE  WRK-MES  =  WRK-MES - 1
           END-IF.

           MOVE  WRK-ANO               TO WRK-AAAA-PER.
           MOVE  WRK-MES               TO WRK-MM-PER.
           MOVE  WRK-PERIODO           TO WRK-PERIODO-AUX.

      *----------------------------------------------------------------*
       0650-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     PROCESSAMENTO PRINCIPAL                                    *
      *----------------------------------------------------------------*
       1000-PROCESSA-NEXT-PAST         SECTION.
      *----------------------------------------------------------------*

           IF   WRK-PERIODO-ARQ        NOT GREATER  WRK-PERIODO-AUX   OR
                E04-CINDCD-EXCL-REG    EQUAL   1
               PERFORM 1500-GRAVA-REGISTRO
           ELSE
               ADD  1                  TO  ACU-REG-DESPREZA
           END-IF.

           PERFORM 0600-LER-ARQUIVO-ADESOESE.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     GRAVA REGISTRO SAIDA                                       *
      *----------------------------------------------------------------*
       1500-GRAVA-REGISTRO             SECTION.
      *----------------------------------------------------------------*

           INITIALIZE FD-ADESPAST-REG.

           MOVE E04-GFCTB009-REG       TO  FD-ADESAO-REG.

           PERFORM 1600-OBTER-DADOS-CLIENTE.
           MOVE WRK-CPF-CNPJ-AUX       TO  FD-CPF-CNPJ.
           MOVE WRK-FILIAL-AUX         TO  FD-FILIAL.
           MOVE WRK-CONTROLE-AUX       TO  FD-CONTROLE.

           WRITE FD-ADESPAST-REG.

           MOVE WRK-GRAVACAO           TO  WRK-OPERACAO.

           PERFORM 0400-TESTAR-FS-ADESPAST.

           ADD 1                       TO ACU-REG-GRAVADOS.

      *----------------------------------------------------------------*
       1500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA OBTER O CPJ / CNPJ                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       1600-OBTER-DADOS-CLIENTE        SECTION.
      *----------------------------------------------------------------*

           MOVE  ZEROS                 TO  WRK-ERRO

           PERFORM 1620-ACESSAR-CTA-CORRENTE

           IF  WRK-ERRO                EQUAL ZEROS
               PERFORM 1630-ACESSAR-CLIEV004
           ELSE
               MOVE    ZEROS           TO  WRK-ERRO
               PERFORM 1610-ACESSAR-CTA-POUPANCA

               IF  WRK-ERRO            EQUAL ZEROS
                   PERFORM 1630-ACESSAR-CLIEV004
               END-IF
           END-IF.

           IF  WRK-ERRO                EQUAL  1
               MOVE ZEROS              TO WRK-CPF-CNPJ
               MOVE ZEROS              TO WRK-FILIAL
               MOVE ZEROS              TO WRK-CONTROLE
           END-IF.

      *----------------------------------------------------------------*
       1600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1610-ACESSAR-CTA-POUPANCA       SECTION.
      *----------------------------------------------------------------*

           MOVE E04-CJUNC-DEPDC        TO CJUNC-DEPDC OF CLIEV007.
           MOVE E04-CCTA-CLI           TO CCTA-CLI    OF CLIEV007.

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
                MOVE 'DB2'             TO ERR-TIPO-ACESSO
                MOVE 'V01CTA_POUPANCA'
                                       TO ERR-DBD-TAB
                MOVE 'SELECT'          TO ERR-FUN-COMANDO
                MOVE SQLCODE           TO ERR-SQL-CODE
                MOVE '0010'            TO ERR-LOCAL
                MOVE SPACES            TO ERR-SEGM
                PERFORM 9999-ROTINA-ERRO
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
       1610-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1620-ACESSAR-CTA-CORRENTE       SECTION.
      *----------------------------------------------------------------*

           MOVE E04-CJUNC-DEPDC        TO CJUNC-DEPDC OF CLIEV008.
           MOVE E04-CCTA-CLI           TO CCTA-CLI    OF CLIEV008.

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
                MOVE 'DB2'             TO ERR-TIPO-ACESSO
                MOVE 'V01CTA_CORRENTE'
                                       TO ERR-DBD-TAB
                MOVE 'SELECT'          TO ERR-FUN-COMANDO
                MOVE SQLCODE           TO ERR-SQL-CODE
                MOVE '0020'            TO ERR-LOCAL
                MOVE SPACES            TO ERR-SEGM
                PERFORM 9999-ROTINA-ERRO
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
       1620-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1630-ACESSAR-CLIEV004           SECTION.
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
                MOVE 'DB2'             TO ERR-TIPO-ACESSO
                MOVE 'V01CLIENTE_AGENCIA'
                                       TO ERR-DBD-TAB
                MOVE 'SELECT'          TO ERR-FUN-COMANDO
                MOVE SQLCODE           TO ERR-SQL-CODE
                MOVE '0030'            TO ERR-LOCAL
                MOVE SPACES            TO ERR-SEGM
                PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 1                  TO WRK-ERRO
           ELSE
               MOVE CCGC-CPF           OF CLIEV004
                                       TO WRK-CPF-CNPJ
               MOVE CFLIAL-CGC         OF CLIEV004
                                       TO WRK-FILIAL
               MOVE CCTRL-CPF-CGC      OF CLIEV004
                                       TO WRK-CONTROLE
           END-IF.

      *----------------------------------------------------------------*
       1630-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     FINALIZACAO DO PROGRAMA                                    *
      *----------------------------------------------------------------*
       1800-PROCEDIMENTOS-FINAIS       SECTION.
      *----------------------------------------------------------------*

           PERFORM 1900-EMITIR-DISPLAY-TOTAIS.

           CLOSE ADESOESE
                 ARQDATAS
                 ADESPAST.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 0200-TESTAR-FILE-STATUS.

           STOP RUN.

      *----------------------------------------------------------------*
       1800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     EMITIR TOTAIS DE PROCESSAMENTO
      *----------------------------------------------------------------*
       1900-EMITIR-DISPLAY-TOTAIS      SECTION.
      *----------------------------------------------------------------*

           DISPLAY '                                               '.
           DISPLAY '***************** GFCT2488 ********************'.
           DISPLAY '*                                             *'.
           DISPLAY '*            PROGRAMA GFCT2488                *'.
           DISPLAY '*                                             *'.
           DISPLAY '*---------------------------------------------*'.

           IF  ACU-REG-LIDOS           EQUAL ZEROS
               DISPLAY '*                                             *'
               DISPLAY '*         PROCESSAMENTO ENCERRADO             *'
           ELSE
               DISPLAY '*                                             *'
               DISPLAY '*   PROCESSAMENTO ENCERRADO COM SUCESSO       *'
               DISPLAY '*                                             *'
               DISPLAY '*---------------------------------------------*'
               DISPLAY '*                                             *'

               MOVE ACU-REG-LIDOS          TO WRK-MASCARA

               DISPLAY '*  REGISTROS LIDOS ADESOESE.: ' WRK-MASCARA
                                                               ' *'

               MOVE ACU-REG-DATAS          TO WRK-MASCARA

               DISPLAY '*  REGISTROS LIDOS ARQDATAS.: ' WRK-MASCARA
                                                               ' *'

               MOVE ACU-REG-GRAVADOS       TO WRK-MASCARA

               DISPLAY '*  REGISTROS GRAV. ADESPAST.: ' WRK-MASCARA
                                                               ' *'

               MOVE ACU-REG-DESPREZA       TO WRK-MASCARA

               DISPLAY '*  REGISTROS DESPREZADOS....: ' WRK-MASCARA
                                                               ' *'
           END-IF.

           DISPLAY '*                                             *'.
           DISPLAY '***************** GFCT2488 ********************'.

      *----------------------------------------------------------------*
       1900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     RETORNO DE ERRO                                            *
      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT2488'             TO ERR-PGM.

           IF  ERR-TIPO-ACESSO         EQUAL 'APL'
               CALL 'BRAD7100'         USING WRK-BATCH
                                             ERRO-AREA
           ELSE
               CALL 'BRAD7100'         USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA
           END-IF.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
