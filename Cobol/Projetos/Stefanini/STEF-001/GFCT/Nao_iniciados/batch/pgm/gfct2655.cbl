      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. GFCT2655.
       AUTHOR.     VINICIUS CRISTIAN MADUREIRA.
      *================================================================*
      *      S O N D A  P R O C W O R K  -  C O N S U L T O R I A      *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT2655                                    *
      *    PROGRAMADOR.:   VINICIUS CRISTIAN MADUREIRA - SONDPROC/GP.50*
      *    ANALISTA....:   VINICIUS CRISTIAN MADUREIRA - SONDPROC/GP.50*
      *    DATA........:   13/12/2008                                  *
      *                                                                *
      *    OBJETIVO    :   OBTEM CNPJ/CPF E VALOR DA RENDA DO CLIENTE.
      *                                                                *
      *    ARQUIVOS:                                                   *
      *      DDNAME                                    INCLUDE/BOOK    *
      *      EXMSTC1A                                  GFCTWAFE        *
      *      EXMPREND                                  GFCTWAET        *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                       INCLUDE/BOOK      *
      *                   DB2PRD.V01EMPRG_CLI_AG     CLIEV019          *
      *                   DB2PRD.CLIENTE_AGENCIA     CLIEV004          *
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

      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*

       FILE-CONTROL.

           SELECT EXMSTC1A   ASSIGN    TO UT-S-EXMSTC1A
                      FILE STATUS      IS WRK-FS-EXMSTC1A.

           SELECT EXMPREND ASSIGN      TO UT-S-EXMPREND
                      FILE STATUS      IS WRK-FS-EXMPREND.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:                                                      *
      *            ORG. SEQUENTIAL     -  LRECL = 100                  *
      *----------------------------------------------------------------*

       FD  EXMSTC1A
           RECORDING MODE IS F
           LABEL RECORDS IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'GFCTWAFE'.

      *----------------------------------------------------------------*
      *    INPUT:                                                      *
      *            ORG. SEQUENTIAL     -  LRECL = 080                  *
      *----------------------------------------------------------------*

       FD  EXMPREND
           RECORDING MODE IS F
           LABEL RECORDS IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'GFCTWAET'.

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  INICIO DA WORKING GFCT2655  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*        ACUMULADORES          *'.
      *----------------------------------------------------------------*

       01  ACU-ACUMULADORES.
           05 ACU-LIDOS-EXMSTC1A       PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-GRAVADOS-EXMPREND    PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-ACHADOS-19           PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-ACHADOS-04           PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-ZERADOS              PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-NAO-ACHADOS-19       PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-NAO-ACHADOS-04       PIC 9(11) COMP-3    VALUE ZEROS.


      *----------------------------------------------------------------*
       01  FILLER                       PIC X(32)           VALUE
           '*    VARIAVEIS AUXILIARES      *'.
      *----------------------------------------------------------------*

       01  WRK-AUXILIARES.
           05 WRK-BATCH                PIC X(08)           VALUE
              'BATCH'.
           05 WRK-MASCARA              PIC ZZ.ZZZ.ZZZ.Z99  VALUE ZEROS.

       01  WRK-VALOR-RENDA             PIC S9(13)V99       VALUE ZEROS.

       ST25X6* Adequação ao padrão ST: tratar CPF/CNPJ e Filial como alfanuméricos.
       01  WRK-CCGC-CPF                PIC X(009)          VALUE SPACES.
       01  WRK-CFLIAL-CGC              PIC X(004)          VALUE SPACES.
       01  WRK-CCTRL-CPF-CGC           PIC 9(002)          VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    AREA PARA FILE STATUS     *'.
      *----------------------------------------------------------------*

       01  WRK-FS-EXMSTC1A             PIC X(02)           VALUE SPACES.
       01  WRK-FS-EXMPREND             PIC X(02)           VALUE SPACES.
       01  WRK-ABERTURA                PIC X(13)           VALUE
           'NA ABERTURA'.
       01  WRK-LEITURA                 PIC X(13)           VALUE
           'NA  LEITURA'.
       01  WRK-GRAVACAO                PIC X(13)           VALUE
           'NA GRAVACAO'.
       01  WRK-FECHAMENTO              PIC X(13)           VALUE
           'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  AREA  DE MENSAGEM  DE ERRO  *'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-FS.
           05  FILLER                  PIC X(07)           VALUE SPACES.
           05  FILLER                  PIC X(07)           VALUE
               '* ERRO '.
           05  WRK-OPERACAO            PIC X(13)           VALUE SPACES.
           05  FILLER                  PIC X(12)           VALUE
              ' DO ARQUIVO '.
           05  WRK-NOME-ARQ            PIC X(08)           VALUE SPACES.
           05  FILLER                  PIC X(17)           VALUE
              ' - FILE-STATUS = '.
           05  WRK-FS                  PIC X(02)           VALUE SPACES.
           05  FILLER                  PIC X(02)           VALUE ' *'.
           05  FILLER                  PIC X(07)           VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     AREA PARA BRAD7100       *'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA PARA TABELA DB2'.
      *---------------------------------------------------------------*

           EXEC SQL
             INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
             INCLUDE CLIEV019
           END-EXEC.

           EXEC SQL
             INCLUDE CLIEV004
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*   FIM  DA WORKING GFCT2655   *'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
      *    ROTINA PRINCIPAL DO PROGRAMA                                *
      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           CALL 'BRAD1050'.

           PERFORM 0100-INICIALIZAR-PROGRAMA.

           PERFORM 0700-VERIFICAR-VAZIO.

           PERFORM 1000-PROCESSAR      UNTIL
                       WRK-FS-EXMSTC1A   EQUAL '10'.

           PERFORM 1500-PROCEDIMENTOS-FINAIS.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA INICIALIZAR O PROGRAMA                          *
      *----------------------------------------------------------------*
       0100-INICIALIZAR-PROGRAMA       SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT EXMSTC1A
               OUTPUT EXMPREND.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 0200-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       0100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE STATUS DOS ARQUIVOS DE ENTRADA E SAIDA          *
      *----------------------------------------------------------------*
       0200-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 0300-TESTAR-FS-EXMSTC1A.

           PERFORM 0400-TESTAR-FS-EXMPREND.

      *----------------------------------------------------------------*
       0200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO EXMSTC1A                      *
      *----------------------------------------------------------------*
       0300-TESTAR-FS-EXMSTC1A         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-EXMSTC1A         NOT EQUAL '00'
               MOVE 'EXMSTC1A'         TO WRK-NOME-ARQ
               MOVE WRK-FS-EXMSTC1A    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO EXMPREND                      *
      *----------------------------------------------------------------*
       0400-TESTAR-FS-EXMPREND         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-EXMPREND         NOT EQUAL '00'
               MOVE 'EXMPREND'         TO WRK-NOME-ARQ
               MOVE WRK-FS-EXMPREND    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA P/ VERIFICAR SE OS ARQUIVOS DE ENTRADA ESTAO VAZIOS  *
      *----------------------------------------------------------------*
       0700-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 0800-LER-EXMSTC1A.

           IF  WRK-FS-EXMSTC1A        EQUAL '10'
               DISPLAY '*********** GFCT2655 ***********'
               DISPLAY '*                              *'
               DISPLAY '*    ARQUIVO  EXMSTC1A VAZIO     *'
               DISPLAY '*    PROCESSAMENTO ENCERRADO   *'
               DISPLAY '*                              *'
               DISPLAY '*********** GFCT2655 ***********'
           END-IF.

      *----------------------------------------------------------------*
       0700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LEITURA DO ARQUIVO EXMSTC1A                                 *
      *----------------------------------------------------------------*
       0800-LER-EXMSTC1A               SECTION.
      *----------------------------------------------------------------*

           READ EXMSTC1A.

           IF  WRK-FS-EXMSTC1A         EQUAL '10'
               GO                      TO 0800-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 0300-TESTAR-FS-EXMSTC1A.

           ADD 1                       TO ACU-LIDOS-EXMSTC1A.

      *----------------------------------------------------------------*
       0800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL DO PROGRAMA                         *
      *----------------------------------------------------------------*
       1000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           IF WAFE-CID-CLI EQUAL SPACES
               ADD 1      TO ACU-ZERADOS
               MOVE ZEROS  TO WRK-VALOR-RENDA
      * ST25X6* Campos alfanuméricos: usar SPACES quando informação ausente
               MOVE SPACES TO WRK-CCGC-CPF
               MOVE SPACES TO WRK-CFLIAL-CGC
               MOVE ZEROS  TO WRK-CCTRL-CPF-CGC
           ELSE
               PERFORM 1300-BUSCAR-VALORES-CLIEV019
               PERFORM 1350-BUSCAR-CPFCNPJ-CLIEV004
           END-IF.

           PERFORM 1400-GRAVAR-EXMPREND.

           PERFORM 0800-LER-EXMSTC1A.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    BUSCAR VALORES CLIEV019                                     *
      *----------------------------------------------------------------*
       1300-BUSCAR-VALORES-CLIEV019               SECTION.
      *----------------------------------------------------------------*

           MOVE WAFE-CID-CLI TO CID-CLI OF CLIEV019.

           EXEC SQL
             SELECT
                   CID_CLI,
                   VRENDA_MES_PF
             INTO
                   :CLIEV019.CID-CLI,
                   :CLIEV019.VRENDA-MES-PF
             FROM   DB2PRD.V01EMPRG_CLI_AG
             WHERE
                   CID_CLI        = :CLIEV019.CID-CLI
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
                MOVE 'DB2'             TO ERR-TIPO-ACESSO
                MOVE 'V01EMPRG_CLI_AG' TO ERR-DBD-TAB
                MOVE 'SELECT'          TO ERR-FUN-COMANDO
                MOVE  SQLCODE          TO ERR-SQL-CODE
                MOVE '0030'            TO ERR-LOCAL
                MOVE  SPACES           TO ERR-SEGM
                PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE ZEROS              TO WRK-VALOR-RENDA
               ADD 1                   TO ACU-NAO-ACHADOS-19
           ELSE
               MOVE VRENDA-MES-PF      OF CLIEV019
                                       TO WRK-VALOR-RENDA
               ADD 1                   TO ACU-ACHADOS-19
           END-IF.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    BUSCAR VALORES CLIEV004                                     *
      *----------------------------------------------------------------*
       1350-BUSCAR-CPFCNPJ-CLIEV004               SECTION.
      *----------------------------------------------------------------*

           MOVE WAFE-CID-CLI TO CID-CLI OF CLIEV004.

           EXEC SQL
             SELECT
                   CCGC_CPF,
                   CFLIAL_CGC,
                   CCTRL_CPF_CGC
             INTO
                   :CLIEV004.CCGC-CPF,
                   :CLIEV004.CFLIAL-CGC,
                   :CLIEV004.CCTRL-CPF-CGC
             FROM   DB2PRD.CLIENTE_AGENCIA
             WHERE
                   CID_CLI        = :CLIEV004.CID-CLI
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
                MOVE 'DB2'             TO ERR-TIPO-ACESSO
                MOVE 'CLIENTE_AGENCIA' TO ERR-DBD-TAB
                MOVE 'SELECT'          TO ERR-FUN-COMANDO
                MOVE  SQLCODE          TO ERR-SQL-CODE
                MOVE '0040'            TO ERR-LOCAL
                MOVE  SPACES           TO ERR-SEGM
                PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
      * ST25X6* Ausência de dados: preencher alfanuméricos com SPACES
               MOVE SPACES                   TO WRK-CCGC-CPF
               MOVE SPACES                   TO WRK-CFLIAL-CGC
               MOVE ZEROS                    TO WRK-CCTRL-CPF-CGC
               ADD 1                          TO ACU-NAO-ACHADOS-04
           ELSE
               MOVE CCGC-CPF      OF CLIEV004 TO WRK-CCGC-CPF
               MOVE CFLIAL-CGC    OF CLIEV004 TO WRK-CFLIAL-CGC
               MOVE CCTRL-CPF-CGC OF CLIEV004 TO WRK-CCTRL-CPF-CGC
               ADD 1                          TO ACU-ACHADOS-04
           END-IF.

      *----------------------------------------------------------------*
       1350-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    GRAVAR ARQUIVO  EXMPREND                                    *
      *----------------------------------------------------------------*
       1400-GRAVAR-EXMPREND            SECTION.
      *----------------------------------------------------------------*

           MOVE WAFE-ANO-PREV     TO WAET-ANO-PREV.
           MOVE WAFE-AGENCIA-PREV TO WAET-AGENCIA-PREV.
           MOVE WAFE-CONTA-PREV   TO WAET-CONTA-PREV.
           MOVE WAFE-QTDE-PREV    TO WAET-QTDE-PREV.
           MOVE WAFE-PAB          TO WAET-PAB.
           MOVE WAFE-SEGTO        TO WAET-SEGTO.
           MOVE WAFE-RENTB        TO WAET-RENTB.
           MOVE WAFE-RENDA        TO WAET-RENDA.
           MOVE WAFE-INDICA-GECT  TO WAET-INDICA-GECT.
           MOVE WAFE-TIPO-EMIS    TO WAET-TIPO-EMIS.
           MOVE WAFE-SEGTO-CLIE   TO WAET-SEGTO-CLIE.

           MOVE WRK-VALOR-RENDA   TO WAET-RENDA.

           MOVE WRK-CCGC-CPF      TO WAET-CCGC-CPF.
           MOVE WRK-CFLIAL-CGC    TO WAET-CFLIAL-CGC.
           MOVE WRK-CCTRL-CPF-CGC TO WAET-CCTRL-CPF-CGC.

           WRITE GFCTWAET-REG.

           MOVE WRK-GRAVACAO      TO WRK-OPERACAO.

           PERFORM 0400-TESTAR-FS-EXMPREND.

           ADD 1                  TO ACU-GRAVADOS-EXMPREND.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA FINALIZACAO DO PROGRAMA                         *
      *----------------------------------------------------------------*
       1500-PROCEDIMENTOS-FINAIS       SECTION.
      *----------------------------------------------------------------*

           PERFORM 1600-EMITIR-DISPLAY.

           CLOSE EXMSTC1A
                 EXMPREND.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 0200-TESTAR-FILE-STATUS.

           STOP RUN.

      *----------------------------------------------------------------*
       1500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA EMISSAO DOS DISPLAYS FINAIS DO PROGRAMA         *
      *----------------------------------------------------------------*
       1600-EMITIR-DISPLAY             SECTION.
      *----------------------------------------------------------------*

           DISPLAY '******************** GFCT2655 ********************'.
           DISPLAY '*                                                *'.
           MOVE ACU-LIDOS-EXMSTC1A        TO WRK-MASCARA.
           DISPLAY '*  LIDOS EM EXMSTC1A........: '
                                                  WRK-MASCARA  '     *'.
           DISPLAY '*                                                *'.
           MOVE ACU-ZERADOS               TO WRK-MASCARA.
           DISPLAY '*  LIDOS SEM CID-CLI........: '
                                                  WRK-MASCARA  '     *'.
           DISPLAY '*                                                *'.
           MOVE ACU-ACHADOS-19             TO WRK-MASCARA.
           DISPLAY '*  ENCONTRADOS NA CLIEV019..: '
                                                  WRK-MASCARA  '     *'.
           DISPLAY '*                                                *'.
           MOVE ACU-ACHADOS-04             TO WRK-MASCARA.
           DISPLAY '*  ENCONTRADOS NA CLIEV004..: '
                                                  WRK-MASCARA  '     *'.
           DISPLAY '*                                                *'.
           MOVE ACU-NAO-ACHADOS-19        TO WRK-MASCARA.
           DISPLAY '*  NAO ENCONTRAD. CLIEV019..: '
                                                  WRK-MASCARA  '     *'.
           DISPLAY '*                                                *'.
           MOVE ACU-NAO-ACHADOS-04        TO WRK-MASCARA.
           DISPLAY '*  NAO ENCONTRAD. CLIEV004..: '
                                                  WRK-MASCARA  '     *'.
           DISPLAY '*                                                *'.
           MOVE ACU-GRAVADOS-EXMPREND     TO WRK-MASCARA.
           DISPLAY '*  GRAVADOS EM EXMPREND.....: '
                                                  WRK-MASCARA  '     *'.
           DISPLAY '*                                                *'.
           DISPLAY '******************** GFCT2655 ********************'.

      *----------------------------------------------------------------*
       1600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAMENTO DE ERRO                              *
      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           IF  ERR-TIPO-ACESSO         NOT EQUAL 'DB2'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
           ELSE
               MOVE 'APL'              TO ERR-TIPO-ACESSO
           END-IF.

           MOVE 'GFCT2655'             TO ERR-PGM.

           CALL 'BRAD7100'             USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------

