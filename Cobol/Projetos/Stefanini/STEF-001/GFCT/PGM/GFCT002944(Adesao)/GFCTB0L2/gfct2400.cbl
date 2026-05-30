
      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. GFCT2400.
       AUTHOR.     VINICIUS CRISTIAN MADUREIRA.
      *================================================================*
      *      S O N D A  P R O C W O R K  -  C O N S U L T O R I A      *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT2400                                    *
      *    PROGRAMADOR.:   VINICIUS CRISTIAN MADUREIRA - SONDPROC/GP.50*
      *    ANALISTA....:   VINICIUS CRISTIAN MADUREIRA - SONDPROC/GP.50*
      *    DATA........:   17/03/2009                                  *
      *                                                                *
      *    OBJETIVO    :   COMPLETA DADOS DA SOLICITACAO DE            *
      *                    CANCELAMENTO.                               *
      *                                                                *
      *    ARQUIVOS:                                                   *
      *      DDNAME                                    INCLUDE/BOOK    *
      *      ARQSOLIC                                  GFCTWAFZ        *
      *      SOLICPAB                                  GFCTWAFZ        *
      *      SOLICEMP                                  GFCTWAFZ        *
      *      SOLICFOR                                  GFCTWAFZ        *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                       INCLUDE/BOOK      *
      *                   DB2PRD.TCANCT_ADSAO_POSTO  GFCTB0L3          *
      *                   DB2PRD.TCANCT_ADSAO_EMPR   GFCTB0L1          *
      *                   DB2PRD.TCANCT_ADSAO_FORNC  GFCTB0L2          *
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

           SELECT ARQSOLIC   ASSIGN    TO UT-S-ARQSOLIC
                      FILE STATUS      IS WRK-FS-ARQSOLIC.

           SELECT SOLICPAB ASSIGN      TO UT-S-SOLICPAB
                      FILE STATUS      IS WRK-FS-SOLICPAB.

           SELECT SOLICEMP ASSIGN      TO UT-S-SOLICEMP
                      FILE STATUS      IS WRK-FS-SOLICEMP.

           SELECT SOLICFOR ASSIGN      TO UT-S-SOLICFOR
                      FILE STATUS      IS WRK-FS-SOLICFOR.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:                                                      *
      *            ORG. SEQUENTIAL     -  LRECL = 0649                 *
      *----------------------------------------------------------------*

       FD  ARQSOLIC
           RECORDING MODE IS F
           LABEL RECORDS IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-ARQSOLIC             PIC X(649).

      *----------------------------------------------------------------*
      *    OUTPUT:                                                     *
      *            ORG. SEQUENTIAL     -  LRECL = 0700                 *
      *----------------------------------------------------------------*

       FD  SOLICPAB
           RECORDING MODE IS F
           LABEL RECORDS IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-SOLICPAB             PIC X(700).

      *----------------------------------------------------------------*
      *    OUTPUT:                                                     *
      *            ORG. SEQUENTIAL     -  LRECL = 0700                 *
      *----------------------------------------------------------------*

       FD  SOLICEMP
           RECORDING MODE IS F
           LABEL RECORDS IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-SOLICEMP             PIC X(700).

      *----------------------------------------------------------------*
      *    OUTPUT:                                                     *
      *            ORG. SEQUENTIAL     -  LRECL = 0700                 *
      *----------------------------------------------------------------*

       FD  SOLICFOR
           RECORDING MODE IS F
           LABEL RECORDS IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-SOLICFOR             PIC X(700).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  INICIO DA WORKING GFCT2400  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*        ACUMULADORES          *'.
      *----------------------------------------------------------------*

       01  ACU-ACUMULADORES.
           05 ACU-LIDOS                PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-GRAVADOS-SOLICPAB    PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-GRAVADOS-SOLICEMP    PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-GRAVADOS-SOLICFOR    PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-NAO-ENCONTRADOS      PIC 9(11) COMP-3    VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                       PIC X(32)           VALUE
           '*    VARIAVEIS AUXILIARES      *'.
      *----------------------------------------------------------------*

       01  WRK-AUXILIARES.
           05 WRK-BATCH                PIC X(08)           VALUE
              'BATCH'.
           05 WRK-MASCARA              PIC ZZ.ZZZ.ZZZ.Z99   VALUE ZEROS.

       01  WRK-AGRUPAMENTO-S           PIC S9(03)           VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-AGRUPAMENTO-S.
           05 WRK-AGRUPAMENTO          PIC  9(03).

       01  WRK-AGRUPAMENTO-ZZZ         PIC ZZ9              VALUE ZEROS.

       01  WRK-CAGPTO-CTA-S            PIC S9(03)           VALUE ZEROS.

       01  WRK-TARIFA-S                PIC S9(05)           VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-TARIFA-S.
           05 WRK-TARIFA               PIC  9(05).

       01  WRK-TARIFA-ZZZ              PIC ZZZZ9            VALUE ZEROS.

       01  WRK-CSERVC-TARIF-S          PIC S9(05)           VALUE ZEROS.

       01  WRK-REGISTRO                PIC X(700)          VALUE SPACES.
       01  FILLER                      REDEFINES WRK-REGISTRO.
           05 WRK-SOLICITA             PIC X(649).
           05 WRK-DETALHES             PIC X(023).
           05 FILLER                   PIC X(028).

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    AREA PARA FILE STATUS     *'.
      *----------------------------------------------------------------*

       01  WRK-FS-ARQSOLIC             PIC X(02)           VALUE SPACES.
       01  WRK-FS-SOLICPAB             PIC X(02)           VALUE SPACES.
       01  WRK-FS-SOLICEMP             PIC X(02)           VALUE SPACES.
       01  WRK-FS-SOLICFOR             PIC X(02)           VALUE SPACES.
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
       01  FILLER                      PIC X(32)           VALUE
           '*     AREA PARA INCLUDES       *'.
      *----------------------------------------------------------------*

       COPY 'GFCTWAFZ'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA PARA TABELA DB2'.
      *---------------------------------------------------------------*

           EXEC SQL
             INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0L1
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0L2
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0L3
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*   FIM  DA WORKING GFCT2400   *'.
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
                       WRK-FS-ARQSOLIC EQUAL '10'.

           PERFORM 1700-PROCEDIMENTOS-FINAIS.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA INICIALIZAR O PROGRAMA                          *
      *----------------------------------------------------------------*
       0100-INICIALIZAR-PROGRAMA       SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT ARQSOLIC
               OUTPUT SOLICPAB
                      SOLICEMP
                      SOLICFOR.

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

           PERFORM 0300-TESTAR-FS-ARQSOLIC.

           PERFORM 0400-TESTAR-FS-SOLICPAB.

           PERFORM 0500-TESTAR-FS-SOLICEMP.

           PERFORM 0600-TESTAR-FS-SOLICFOR.

      *----------------------------------------------------------------*
       0200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO ARQSOLIC                      *
      *----------------------------------------------------------------*
       0300-TESTAR-FS-ARQSOLIC         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ARQSOLIC         NOT EQUAL '00'
               MOVE 'ARQSOLIC'         TO WRK-NOME-ARQ
               MOVE WRK-FS-ARQSOLIC    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO SOLICPAB                      *
      *----------------------------------------------------------------*
       0400-TESTAR-FS-SOLICPAB         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SOLICPAB         NOT EQUAL '00'
               MOVE 'SOLICPAB'         TO WRK-NOME-ARQ
               MOVE WRK-FS-SOLICPAB    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO SOLICEMP                      *
      *----------------------------------------------------------------*
       0500-TESTAR-FS-SOLICEMP         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SOLICEMP         NOT EQUAL '00'
               MOVE 'SOLICEMP'         TO WRK-NOME-ARQ
               MOVE WRK-FS-SOLICEMP    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO SOLICFOR                      *
      *----------------------------------------------------------------*
       0600-TESTAR-FS-SOLICFOR         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SOLICFOR         NOT EQUAL '00'
               MOVE 'SOLICFOR'         TO WRK-NOME-ARQ
               MOVE WRK-FS-SOLICFOR    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA P/ VERIFICAR SE OS ARQUIVOS DE ENTRADA ESTAO VAZIOS  *
      *----------------------------------------------------------------*
       0700-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 0800-LER-ARQSOLIC.

           IF  WRK-FS-ARQSOLIC        EQUAL '10'
               DISPLAY '*********** GFCT2400 ***********'
               DISPLAY '*                              *'
               DISPLAY '*    ARQUIVO ARQSOLIC VAZIO    *'
               DISPLAY '*    PROCESSAMENTO ENCERRADO   *'
               DISPLAY '*                              *'
               DISPLAY '*********** GFCT2400 ***********'
           END-IF.

      *----------------------------------------------------------------*
       0700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LEITURA DO ARQUIVO ARQSOLIC                                 *
      *----------------------------------------------------------------*
       0800-LER-ARQSOLIC               SECTION.
      *----------------------------------------------------------------*

           READ ARQSOLIC.

           IF  WRK-FS-ARQSOLIC         EQUAL '10'
               GO                      TO 0800-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 0300-TESTAR-FS-ARQSOLIC.

           MOVE FD-ARQSOLIC            TO WRK-SOLICITA.

           MOVE WRK-REGISTRO           TO REG-GFCTWAFZ.
           INITIALIZE                     WAFZ-DETALHES.

           ADD 1                       TO ACU-LIDOS.

      *----------------------------------------------------------------*
       0800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL DO PROGRAMA                         *
      *----------------------------------------------------------------*
       1000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           MOVE WAFZ-CAGPTO-CTA    TO WRK-CAGPTO-CTA-S.
           MOVE WRK-CAGPTO-CTA-S   TO WRK-AGRUPAMENTO-S.
           MOVE WRK-AGRUPAMENTO    TO WRK-AGRUPAMENTO-ZZZ
           MOVE WAFZ-CSERVC-TARIF  TO WRK-CSERVC-TARIF-S.
           MOVE WRK-CSERVC-TARIF-S TO WRK-TARIFA-S.
           MOVE WRK-TARIFA         TO WRK-TARIFA-ZZZ

           IF WAFZ-CAGPTO-CTA         EQUAL 15 OR 17 OR 20
               PERFORM 1100-BUSCAR-DADOS-GFCTB0L3
           ELSE
               IF WAFZ-CAGPTO-CTA     EQUAL 16
                   PERFORM 1200-BUSCAR-DADOS-GFCTB0L1
               ELSE
                   IF WAFZ-CAGPTO-CTA EQUAL 22
                       PERFORM 1300-BUSCAR-DADOS-GFCTB0L2
                   ELSE
                       DISPLAY '************** GFCT2400 **************'
                       DISPLAY '*                                    *'
                       DISPLAY '*      AGRUPAMENTO DIFERENTE DE:     *'
                       DISPLAY '*        15, 17, 20, 16 E  22        *'
                       DISPLAY '*                                    *'
                       DISPLAY '* AGRUPAMENTO: 'WRK-AGRUPAMENTO-ZZZ
                                                 '                   *'
                       DISPLAY '* TARIFA: 'WRK-TARIFA-ZZZ
                                              '                      *'
                       DISPLAY '* TIMSTP: 'WAFZ-HINCL-REG
                                                                   ' *'
                       DISPLAY '*                                    *'
                       DISPLAY '************** GFCT2400 **************'
                   END-IF
               END-IF
           END-IF.

           PERFORM 0800-LER-ARQSOLIC.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    BUSCAR DADOS TABELA GFCTB0L3                                *
      *----------------------------------------------------------------*
       1100-BUSCAR-DADOS-GFCTB0L3               SECTION.
      *----------------------------------------------------------------*

           MOVE WAFZ-CAGPTO-CTA   TO CAGPTO-CTA   OF GFCTB0L3.
           MOVE WAFZ-CSERVC-TARIF TO CSERVC-TARIF OF GFCTB0L3.
           MOVE WAFZ-HINCL-REG    TO HINCL-REG    OF GFCTB0L3.

           EXEC SQL
             SELECT
                   CDEPDC,
                   CPOSTO_SERVC
             INTO
                   :GFCTB0L3.CDEPDC,
                   :GFCTB0L3.CPOSTO-SERVC
             FROM   DB2PRD.TCANCT_ADSAO_POSTO
             WHERE
                   CAGPTO_CTA   = :GFCTB0L3.CAGPTO-CTA   AND
                   CSERVC_TARIF = :GFCTB0L3.CSERVC-TARIF AND
                   HINCL_REG    = :GFCTB0L3.HINCL-REG
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
                MOVE 'DB2'                TO ERR-TIPO-ACESSO
                MOVE 'TCANCT_ADSAO_POSTO' TO ERR-DBD-TAB
                MOVE 'SELECT'             TO ERR-FUN-COMANDO
                MOVE  SQLCODE             TO ERR-SQL-CODE
                MOVE '0005'               TO ERR-LOCAL
                MOVE  SPACES              TO ERR-SEGM
                PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               DISPLAY '************** GFCT2400 **************'
               DISPLAY '*                                    *'
               DISPLAY '* REGISTRO NAO ENCONTRADO NA TABELA: *'
               DISPLAY '*   GFCTB0L3  TCANCT_ADSAO_POSTO     *'
               DISPLAY '* AGRUPAMENTO: 'WRK-AGRUPAMENTO-ZZZ
                                         '                   *'
               DISPLAY '* TARIFA: 'WRK-TARIFA-ZZZ
                                      '                      *'
               DISPLAY '* TIMSTP: 'WAFZ-HINCL-REG
                                                           ' *'
               DISPLAY '*                                    *'
               DISPLAY '************** GFCT2400 **************'
               ADD 1 TO ACU-NAO-ENCONTRADOS
           ELSE
               MOVE CDEPDC        OF GFCTB0L3 TO WAFZ-AGENCIA
               MOVE CPOSTO-SERVC  OF GFCTB0L3 TO WAFZ-POSTO
               PERFORM 1400-GRAVAR-SOLICPAB
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      *----------------------------------------------------------------*
      *    BUSCAR DADOS TABELA GFCTB0L1                                *
      *----------------------------------------------------------------*
       1200-BUSCAR-DADOS-GFCTB0L1               SECTION.
      *----------------------------------------------------------------*

           MOVE WAFZ-CAGPTO-CTA   TO CAGPTO-CTA   OF GFCTB0L1.
           MOVE WAFZ-CSERVC-TARIF TO CSERVC-TARIF OF GFCTB0L1.
           MOVE WAFZ-HINCL-REG    TO HINCL-REG    OF GFCTB0L1.

           EXEC SQL
             SELECT
                   CAG_BCRIA,
                   CCTA_BCRIA_CLI
             INTO
                   :GFCTB0L1.CAG-BCRIA,
                   :GFCTB0L1.CCTA-BCRIA-CLI
             FROM   DB2PRD.TCANCT_ADSAO_EMPR
             WHERE
                   CAGPTO_CTA   = :GFCTB0L1.CAGPTO-CTA   AND
                   CSERVC_TARIF = :GFCTB0L1.CSERVC-TARIF AND
                   HINCL_REG    = :GFCTB0L1.HINCL-REG
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
                MOVE 'DB2'               TO ERR-TIPO-ACESSO
                MOVE 'TCANCT_ADSAO_EMPR' TO ERR-DBD-TAB
                MOVE 'SELECT'            TO ERR-FUN-COMANDO
                MOVE  SQLCODE            TO ERR-SQL-CODE
                MOVE '0010'              TO ERR-LOCAL
                MOVE  SPACES             TO ERR-SEGM
                PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               DISPLAY '************** GFCT2400 **************'
               DISPLAY '*                                    *'
               DISPLAY '* REGISTRO NAO ENCONTRADO NA TABELA: *'
               DISPLAY '*   GFCTB0L1  TCANCT_ADSAO_EMPR      *'
               DISPLAY '* AGRUPAMENTO: 'WRK-AGRUPAMENTO-ZZZ
                                         '                   *'
               DISPLAY '* TARIFA: 'WRK-TARIFA-ZZZ
                                      '                      *'
               DISPLAY '* TIMSTP: 'WAFZ-HINCL-REG
                                                           ' *'
               DISPLAY '*                                    *'
               DISPLAY '************** GFCT2400 **************'
               ADD 1 TO ACU-NAO-ENCONTRADOS
           ELSE
               MOVE CAG-BCRIA       OF GFCTB0L1 TO  WAFZ-AGENCIA
               MOVE CCTA-BCRIA-CLI  OF GFCTB0L1 TO  WAFZ-CONTA
               PERFORM 1500-GRAVAR-SOLICEMP
           END-IF.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *    BUSCAR DADOS TABELA GFCTB0L2                                *
      *----------------------------------------------------------------*
       1300-BUSCAR-DADOS-GFCTB0L2               SECTION.
      *----------------------------------------------------------------*

           MOVE WAFZ-CAGPTO-CTA   TO CAGPTO-CTA   OF GFCTB0L2.
           MOVE WAFZ-CSERVC-TARIF TO CSERVC-TARIF OF GFCTB0L2.
           MOVE WAFZ-HINCL-REG    TO HINCL-REG    OF GFCTB0L2.

           EXEC SQL
             SELECT
ST2506*            CCNPJ_CPF,
ST2506*            CFLIAL_CNPJ,
ST2506*            CCTRL_CNPJ
ST2506             CCNPJ_CPF_ST,
ST2506             CFLIAL_CNPJ_ST,
ST2506             CCTRL_CNPJ_ST
             INTO
ST2506*            :GFCTB0L2.CCNPJ-CPF,
ST2506*            :GFCTB0L2.CFLIAL-CNPJ,
ST2506*            :GFCTB0L2.CCTRL-CNPJ
ST2506             :GFCTB0L2.CCNPJ-CPF-ST,
ST2506             :GFCTB0L2.CFLIAL-CNPJ-ST,
ST2506             :GFCTB0L2.CCTRL-CNPJ-ST
             FROM   DB2PRD.TCANCT_ADSAO_FORNC
             WHERE
                   CAGPTO_CTA   = :GFCTB0L2.CAGPTO-CTA   AND
                   CSERVC_TARIF = :GFCTB0L2.CSERVC-TARIF AND
                   HINCL_REG    = :GFCTB0L2.HINCL-REG
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
                MOVE 'DB2'                TO ERR-TIPO-ACESSO
                MOVE 'TCANCT_ADSAO_FORNC' TO ERR-DBD-TAB
                MOVE 'SELECT'             TO ERR-FUN-COMANDO
                MOVE  SQLCODE             TO ERR-SQL-CODE
                MOVE '0015'               TO ERR-LOCAL
                MOVE  SPACES              TO ERR-SEGM
                PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               DISPLAY '************** GFCT2400 **************'
               DISPLAY '*                                    *'
               DISPLAY '* REGISTRO NAO ENCONTRADO NA TABELA: *'
               DISPLAY '*   GFCTB0L2  TCANCT_ADSAO_FORNC     *'
               DISPLAY '* AGRUPAMENTO: 'WRK-AGRUPAMENTO-ZZZ
                                         '                   *'
               DISPLAY '* TARIFA: 'WRK-TARIFA-ZZZ
                                      '                      *'
               DISPLAY '* TIMSTP: 'WAFZ-HINCL-REG
                                                           ' *'
               DISPLAY '*                                    *'
               DISPLAY '************** GFCT2400 **************'
               ADD 1 TO ACU-NAO-ENCONTRADOS
           ELSE
ST2506*        MOVE CCNPJ-CPF   OF GFCTB0L2 TO WAFZ-CNPJ-NUM
ST2506*        MOVE CFLIAL-CNPJ OF GFCTB0L2 TO WAFZ-CNPJ-FIL
ST2506         MOVE CCNPJ-CPF-ST   OF GFCTB0L2 TO WAFZ-CNPJ-CGC
ST2506         MOVE CFLIAL-CNPJ-ST OF GFCTB0L2 TO WAFZ-CNPJ-FIL
               MOVE CCTRL-CNPJ  OF GFCTB0L2 TO WAFZ-CNPJ-CTR
               PERFORM 1600-GRAVAR-SOLICFOR
           END-IF.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    GRAVAR ARQUIVO  SOLICPAB                                    *
      *----------------------------------------------------------------*
       1400-GRAVAR-SOLICPAB            SECTION.
      *----------------------------------------------------------------*

           WRITE FD-SOLICPAB FROM REG-GFCTWAFZ.

           MOVE WRK-GRAVACAO      TO WRK-OPERACAO.

           PERFORM 0400-TESTAR-FS-SOLICPAB.

           ADD 1                  TO ACU-GRAVADOS-SOLICPAB.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    GRAVAR ARQUIVO  SOLICEMP                                    *
      *----------------------------------------------------------------*
       1500-GRAVAR-SOLICEMP            SECTION.
      *----------------------------------------------------------------*

           WRITE FD-SOLICEMP FROM REG-GFCTWAFZ.

           MOVE WRK-GRAVACAO      TO WRK-OPERACAO.

           PERFORM 0500-TESTAR-FS-SOLICEMP.

           ADD 1                  TO ACU-GRAVADOS-SOLICEMP.

      *----------------------------------------------------------------*
       1500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    GRAVAR ARQUIVO  SOLICFOR                                    *
      *----------------------------------------------------------------*
       1600-GRAVAR-SOLICFOR            SECTION.
      *----------------------------------------------------------------*

           WRITE FD-SOLICFOR FROM REG-GFCTWAFZ.

           MOVE WRK-GRAVACAO      TO WRK-OPERACAO.

           PERFORM 0600-TESTAR-FS-SOLICFOR.

           ADD 1                  TO ACU-GRAVADOS-SOLICFOR.

      *----------------------------------------------------------------*
       1600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA FINALIZACAO DO PROGRAMA                         *
      *----------------------------------------------------------------*
       1700-PROCEDIMENTOS-FINAIS       SECTION.
      *----------------------------------------------------------------*

           PERFORM 1800-EMITIR-DISPLAY.

           CLOSE ARQSOLIC
                 SOLICPAB
                 SOLICEMP
                 SOLICFOR.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 0200-TESTAR-FILE-STATUS.

           STOP RUN.

      *----------------------------------------------------------------*
       1700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA EMISSAO DOS DISPLAYS FINAIS DO PROGRAMA         *
      *----------------------------------------------------------------*
       1800-EMITIR-DISPLAY             SECTION.
      *----------------------------------------------------------------*

           DISPLAY '******************** GFCT2400 ********************'.
           DISPLAY '*                                                *'.
           MOVE ACU-LIDOS                 TO WRK-MASCARA.
           DISPLAY '*  LIDOS EM ARQSOLIC........: '
                                                  WRK-MASCARA  '     *'.
           DISPLAY '*                                                *'.
           MOVE ACU-GRAVADOS-SOLICPAB     TO WRK-MASCARA.
           DISPLAY '*  GRAVADOS EM SOLICPAB.....: '
                                                  WRK-MASCARA  '     *'.
           DISPLAY '*                                                *'.
           MOVE ACU-GRAVADOS-SOLICEMP     TO WRK-MASCARA.
           DISPLAY '*  GRAVADOS EM SOLICEMP.....: '
                                                  WRK-MASCARA  '     *'.
           DISPLAY '*                                                *'.
           MOVE ACU-GRAVADOS-SOLICFOR     TO WRK-MASCARA.
           DISPLAY '*  GRAVADOS EM SOLICFOR.....: '
                                                  WRK-MASCARA  '     *'.
           DISPLAY '*                                                *'.
           MOVE ACU-NAO-ENCONTRADOS       TO WRK-MASCARA.
           DISPLAY '*  NAO ENCONTRADOS..........: '
                                                  WRK-MASCARA  '     *'.
           DISPLAY '*                                                *'.
           DISPLAY '******************** GFCT2400 ********************'.

      *----------------------------------------------------------------*
       1800-99-FIM.                    EXIT.
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

           MOVE 'GFCT2400'             TO ERR-PGM.

           CALL 'BRAD7100'             USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------