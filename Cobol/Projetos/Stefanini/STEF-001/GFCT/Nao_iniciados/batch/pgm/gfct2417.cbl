      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. GFCT2417.
      *================================================================*
      *                                                                *
      *    PROGRAMA     : GFCT2417                                     *
      *    PROGRAMADOR  : ROGERIO PREVEDEL              -  PROCWORK    *
      *    ANALISTA     : WAGNER SILVA                  -  PROCWORK    *
      *    DATA         : 18/03/2009                                   *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      ATUALIZACAO DA TABELA TCANCT_ADSAO_AGPTO.                 *
      *                                                                *
      *    ARQUIVOS     :                                              *
      *      DDNAME                                INCLUDE/BOOK        *
      *      ATUTABEL  -  ENTRADA                    GFCTWAFZ          *
      *      ARQATUAL  -  SAIDA                      GFCTWAFZ          *
      *                                                                *
      *    BANCO DE DADOS :                                            *
      *     DB2                                                        *
      *      TABLE                                 INCLUDE/BOOK        *
      *      DB2PRD.PARM_DATA_PROCM                  GFCTB0A1          *
      *      DB2PRD.TCANCT_ADSAO_AGPTO               GFCTB0L0          *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      POOL7100 - TRATAMENTO DE ERROS.                           *
      *      CKRS0100 - TRATAMENTO DE COMMIT-RESTART.                  *
      *                                                                *
      *================================================================*
      *                        A L T E R A C A O                       *
      *----------------------------------------------------------------*
      *                                                                *
      *    ANALISTA     : XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
      *    DATA         : XX/XX/XXXX                                   *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
      *      XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
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

      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*

       FILE-CONTROL.

           SELECT ATUTABEL ASSIGN      TO UT-S-ATUTABEL
                      FILE STATUS      IS WRK-FS-ATUTABEL.

           SELECT ARQATUAL ASSIGN      TO UT-S-ARQATUAL
                      FILE STATUS      IS WRK-FS-ARQATUAL.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT  :  EVENTOS DE GRUPO DE SERVICO                       *
      *              ORG. SEQUENCIAL   -   LRECL = 0700                *
      *----------------------------------------------------------------*

       FD  ATUTABEL
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-ATUTABEL             PIC X(700).

      *----------------------------------------------------------------*
      *    OUTPUT :  ARQUIVO DE EVENTO SEGUEM COBRANCA                 *
      *              ORG. SEQUENCIAL   -   LRECL = 0700                *
      *----------------------------------------------------------------*

       FD  ARQATUAL
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-ARQATUAL             PIC X(700).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC X(32)           VALUE
           '*  INICIO DA WORKING GFCT2417  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*         ACUMULADORES         *'.
      *----------------------------------------------------------------*

       01  ACU-ACUMULADORES.
           05  ACU-LIDOS-ATUTABEL      PIC 9(11) COMP-3    VALUE ZEROS.
           05  ACU-GRAVA-ARQATUAL      PIC 9(11) COMP-3    VALUE ZEROS.
           05  ACU-ATUALIZADOS-OK      PIC 9(11) COMP-3    VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     TESTE DE FILE-STATUS     *'.
      *----------------------------------------------------------------*

       01  WRK-AREA-FS.
           05  WRK-FS-ATUTABEL         PIC X(02)           VALUE SPACES.
           05  WRK-FS-ARQATUAL         PIC X(02)           VALUE SPACES.
           05  WRK-ABERTURA            PIC X(13)           VALUE
               ' NA ABERTURA '.
           05  WRK-LEITURA             PIC X(13)           VALUE
               ' NA  LEITURA '.
           05  WRK-GRAVACAO            PIC X(13)           VALUE
               ' NA GRAVACAO '.
           05  WRK-FECHAMENTO          PIC X(13)           VALUE
               'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     VARIAVEIS AUXILIARES     *'.
      *----------------------------------------------------------------*

       01  WRK-AUXILIARES.
           05 WRK-BATCH                PIC X(08)           VALUE
              'BATCH'.
           05 WRK-MASCARA              PIC ZZ.ZZZ.ZZZ.ZZ9 VALUE ZEROS.
           05 WRK-FIM-ATUTABEL         PIC X(01)           VALUE 'N'.
           05 WRK-CKRS0100             PIC X(08)     VALUE 'CKRS0100'.

       01 WRK-AREA-RESTART.
           10  WRK-LIDOS-ATUTABEL  PIC 9(11)           VALUE ZEROS.
           10  WRK-GRAVA-ARQATUAL  PIC 9(11)           VALUE ZEROS.
           10  WRK-ATUALIZADOS-OK  PIC 9(11)           VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*      MENSAGEM DE ERRO        *'.
      *----------------------------------------------------------------*

       01  WRK-MSG-FS-ERRO.
           05  FILLER                  PIC X(07)           VALUE SPACES.
           05  FILLER                  PIC X(02)           VALUE '* '.
           05  FILLER                  PIC X(05)           VALUE
               'ERRO '.
           05  WRK-OPERACAO            PIC X(13)           VALUE SPACES.
           05  FILLER                  PIC X(12)           VALUE
               ' DO ARQUIVO '.
           05  WRK-NOME-ARQ            PIC X(08)           VALUE SPACES.
           05  FILLER                  PIC X(17)           VALUE
               ' - FILE-STATUS = '.
           05  WRK-FILE-STATUS         PIC X(02)           VALUE SPACES.
           05  FILLER                  PIC X(02)           VALUE ' *'.
           05  FILLER                  PIC X(07)           VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     AREA DE AGRUPAMENTOS     *'.
      *----------------------------------------------------------------*

       COPY 'GFCTWAFZ'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*       AREA DE RESTART        *'.
      *----------------------------------------------------------------*

       COPY 'I#CKRS01'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*       AREA DA POOL7100       *'.
      *----------------------------------------------------------------*

       COPY POL7100C.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*       AREA PARA DB2          *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0A1
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0L0
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    FIM DA WORKING GFCT2417   *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  LNK-PARM.
           05  LNK-TAM                 PIC S9(004)         COMP.
           05  LNK-PAR                 PIC  9(001).

      *================================================================*
       PROCEDURE                       DIVISION          USING LNK-PARM.
      *================================================================*

      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           PERFORM 0100-INICIALIZAR.

           PERFORM 0700-VERIFICAR-VAZIO.

           PERFORM 0900-ACESSAR-GFCTB0A1.

           PERFORM 1000-PROCESSAR-DADOS
             UNTIL WRK-FIM-ATUTABEL    EQUAL 'S'.

           PERFORM 1400-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0100-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           PERFORM 0150-CONSISTIR-PARM.

           CALL    'CKRS1000'.

           PERFORM 0200-START-RESTART.

           OPEN INPUT  ATUTABEL
                OUTPUT ARQATUAL.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 0400-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       0100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0150-CONSISTIR-PARM             SECTION.
      *----------------------------------------------------------------*

           IF  LNK-TAM EQUAL ZEROS OR
               LNK-PAR NOT NUMERIC OR
               LNK-PAR NOT EQUAL (1 AND 2)
               DISPLAY '************** GFCT2417 *************'
               DISPLAY '*        PARAMETRO INVALIDO:        *'
               DISPLAY '* LNK-PAR     : ' LNK-PAR
                                           '                *'
               DISPLAY '************** GFCT2417 *************'
               MOVE 'APL'               TO ERR-TIPO-ACESSO
               MOVE 'PARM'              TO WRK-NOME-ARQ
               MOVE 'INCONSISTENTE'     TO ERR-TEXTO
               PERFORM 9999-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0150-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0200-START-RESTART              SECTION.
      *----------------------------------------------------------------*

           INITIALIZE CKRS01-INTERFACE.

           MOVE SPACES                  TO CK01-PLAN.
           MOVE 'DB2 '                  TO CK01-ID-DB2.
           MOVE 'I'                     TO CK01-FUNCAO.
           MOVE SPACES                  TO CK01-AREA-RESTART.
           MOVE ZEROS                   TO CK01-TAM-AREA-RESTART.

           CALL WRK-CKRS0100         USING CKRS01-INTERFACE.

           IF   CK01-CODIGO-RETORNO    NOT EQUAL ZEROS
                DISPLAY '******** NA ABERTURA ********'
                PERFORM 0300-TRATAR-ERRO-RESTART
           END-IF.

           IF   CK01-RESTART
                MOVE CK01-AREA-RESTART(1:CK01-TAM-AREA-RESTART)
                                        TO WRK-AREA-RESTART
                MOVE WRK-LIDOS-ATUTABEL TO ACU-LIDOS-ATUTABEL
                MOVE WRK-GRAVA-ARQATUAL TO ACU-GRAVA-ARQATUAL
                MOVE WRK-ATUALIZADOS-OK TO ACU-ATUALIZADOS-OK
           END-IF.

      *----------------------------------------------------------------*
       0200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0300-TRATAR-ERRO-RESTART        SECTION.
      *----------------------------------------------------------------*

           MOVE 'APL'                  TO ERR-TIPO-ACESSO.

           IF  CK01-CODIGO-RETORNO     EQUAL 8
               EVALUATE CK01-CODIGO-MENSAGEM
                   WHEN 'CKRS0001'
                        MOVE 'CKRS0100 - CODIGO DE FUNCAO INCORRETO'
                                       TO ERR-TEXTO

                   WHEN 'CKRS0002'
                        MOVE 'CKRS0100 - JA FOI FEITA UMA CHAMADA EM MOD
      -                      'O INITIALIZE'
                                       TO ERR-TEXTO

                   WHEN 'CKRS0003'
                        MOVE 'CKRS0100 - NAO FOI EFETUADA CHAMADA EM MOD
      -                      'O INITIALIZE'
                                       TO ERR-TEXTO

                   WHEN 'CKRS0004'
                        MOVE 'CKRS0100 - CAMPO DB2-ID DEVE ESTAR INFORMA
      -                      'DO'      TO ERR-TEXTO

                   WHEN 'CKRS0005'
                        MOVE 'CKRS0100 - O PROCESSO NAO SE ENCONTRA CADA
      -                      'STRADO'  TO ERR-TEXTO

                   WHEN OTHER
                        MOVE 'CKRS0100 - ERRO NAO IDENTIFICADO'
                                       TO ERR-TEXTO
                        DISPLAY '************** GFCT2417 **************'
                        DISPLAY '*                                    *'
                        DISPLAY '* CK01-CODIGO-RETORNO : '
                                   CK01-CODIGO-RETORNO    '           *'
                        DISPLAY '* CK01-CODIGO-MENSAGEM: '
                                   CK01-CODIGO-MENSAGEM         '     *'
                        DISPLAY '*                                    *'
                        DISPLAY '************** GFCT2417 **************'
               END-EVALUATE
           ELSE
               MOVE 'CKRS0100 - ERRO GRAVE - RETURN <> DE ZEROS E 8'
                                       TO ERR-TEXTO
               DISPLAY '************** GFCT2417 **************'
               DISPLAY '*                                    *'
               DISPLAY '* CK01-CODIGO-RETORNO : '
                          CK01-CODIGO-RETORNO    '           *'
               DISPLAY '* CK01-CODIGO-MENSAGEM: '
                          CK01-CODIGO-MENSAGEM         '     *'
               DISPLAY '*                                    *'
               DISPLAY '************** GFCT2417 **************'
           END-IF.

           PERFORM 9999-TRATAR-ERRO.

      *----------------------------------------------------------------*
       0300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0400-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 0500-TESTAR-FS-ATUTABEL.

           PERFORM 0600-TESTAR-FS-ARQATUAL.

      *----------------------------------------------------------------*
       0400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0500-TESTAR-FS-ATUTABEL         SECTION.
      *----------------------------------------------------------------*

           IF WRK-FS-ATUTABEL          NOT EQUAL '00'
              MOVE 'APL'               TO ERR-TIPO-ACESSO
              MOVE 'ATUTABEL'          TO WRK-NOME-ARQ
              MOVE WRK-FS-ATUTABEL     TO WRK-FILE-STATUS
              MOVE WRK-MSG-FS-ERRO     TO ERR-TEXTO
              PERFORM 9999-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0600-TESTAR-FS-ARQATUAL         SECTION.
      *----------------------------------------------------------------*

           IF WRK-FS-ARQATUAL          NOT EQUAL '00'
              MOVE 'APL'               TO ERR-TIPO-ACESSO
              MOVE 'ARQATUAL'          TO WRK-NOME-ARQ
              MOVE WRK-FS-ARQATUAL     TO WRK-FILE-STATUS
              MOVE WRK-MSG-FS-ERRO     TO ERR-TEXTO
              PERFORM 9999-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0700-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           MOVE    'N'                  TO WRK-FIM-ATUTABEL.

           PERFORM 0800-LER-ATUTABEL.

           IF  WRK-FIM-ATUTABEL        EQUAL 'S'
               DISPLAY '*************** GFCT2417 ***************'
               DISPLAY '*                                      *'
               DISPLAY '*     NAO EXISTEM  AGRUPAMENTOS        *'
               DISPLAY '*                                      *'
               DISPLAY '*      PROCESSO TERMINA NORMAL         *'
               DISPLAY '*                                      *'
               DISPLAY '*************** GFCT2417 ***************'
           END-IF.

      *----------------------------------------------------------------*
       0700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0800-LER-ATUTABEL               SECTION.
      *----------------------------------------------------------------*

           READ ATUTABEL               INTO REG-GFCTWAFZ.

           IF WRK-FS-ATUTABEL          EQUAL '10'
              MOVE 'S'                 TO WRK-FIM-ATUTABEL
              GO                       TO 0800-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 0500-TESTAR-FS-ATUTABEL.

           ADD  1                      TO ACU-LIDOS-ATUTABEL.

      *----------------------------------------------------------------*
       0800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0900-ACESSAR-GFCTB0A1           SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC OF GFCTB0A1.

           EXEC SQL
               SELECT  DPROCM_ATUAL
               INTO   :GFCTB0A1.DPROCM-ATUAL
               FROM    DB2PRD.PARM_DATA_PROCM
               WHERE   CSIST_PRINC     = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PARM_DATA_PROCM'  TO ERR-DBD-TAB
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '0010'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-PROCESSAR-DADOS            SECTION.
      *----------------------------------------------------------------*

           PERFORM 1100-ATUALIZAR-GFCTB0L0.

           PERFORM 1200-GRAVAR-ARQATUAL.

           PERFORM 1300-CHECK-POINT.

           PERFORM 0800-LER-ATUTABEL.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1100-ATUALIZAR-GFCTB0L0         SECTION.
      *----------------------------------------------------------------*

           MOVE WAFZ-CAGPTO-CTA         TO CAGPTO-CTA
                                        OF GFCTB0L0.
           MOVE WAFZ-CSERVC-TARIF       TO CSERVC-TARIF
                                        OF GFCTB0L0.
           MOVE WAFZ-HINCL-REG          TO HINCL-REG
                                        OF GFCTB0L0.

           IF   LNK-PAR   EQUAL   1
               MOVE WAFZ-QADSAO-CANCD   TO QPRVIO-CANCT-ADSAO
                                        OF GFCTB0L0
               PERFORM   1110-UPDATE-CSIT-2
           ELSE
               MOVE WAFZ-QADSAO-CANCD   TO QADSAO-CANCD OF GFCTB0L0
               PERFORM   1120-UPDATE-CSIT-5
           END-IF.

           ADD  1                      TO ACU-ATUALIZADOS-OK.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1110-UPDATE-CSIT-2              SECTION.
      *----------------------------------------------------------------*

           MOVE    +02                 TO CSIT-SOLTC-CANCT
                                       OF GFCTB0L0.

           EXEC SQL
               UPDATE DB2PRD.TCANCT_ADSAO_AGPTO
                  SET QPRVIO_CANCT_ADSAO = :GFCTB0L0.QPRVIO-CANCT-ADSAO,
                      CSIT_SOLTC_CANCT   = :GFCTB0L0.CSIT-SOLTC-CANCT,
                      HEXCUC_CALC_PRVIO  = CURRENT TIMESTAMP
                WHERE CAGPTO_CTA         = :GFCTB0L0.CAGPTO-CTA
                  AND CSERVC_TARIF       = :GFCTB0L0.CSERVC-TARIF
                  AND HINCL_REG          = :GFCTB0L0.HINCL-REG
           END-EXEC.

           IF  (SQLCODE                   NOT EQUAL ZEROS) OR
               (SQLWARN0                  EQUAL 'W')
                MOVE 'DB2'                TO ERR-TIPO-ACESSO
                MOVE 'TCANCT_ADSAO_AGPTO' TO ERR-DBD-TAB
                MOVE 'UPDATE  '           TO ERR-FUN-COMANDO
                MOVE SQLCODE              TO ERR-SQL-CODE
                MOVE '0020'               TO ERR-LOCAL
                MOVE SPACES               TO ERR-SEGM
                PERFORM 9999-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1120-UPDATE-CSIT-5              SECTION.
      *----------------------------------------------------------------*

           MOVE    +05                 TO CSIT-SOLTC-CANCT
                                       OF GFCTB0L0.

           EXEC SQL
               UPDATE DB2PRD.TCANCT_ADSAO_AGPTO
                  SET QADSAO_CANCD     = :GFCTB0L0.QADSAO-CANCD,
                      CSIT_SOLTC_CANCT = :GFCTB0L0.CSIT-SOLTC-CANCT,
                      HEXCUC_PROCM     = CURRENT TIMESTAMP
                WHERE CAGPTO_CTA       = :GFCTB0L0.CAGPTO-CTA
                  AND CSERVC_TARIF     = :GFCTB0L0.CSERVC-TARIF
                  AND HINCL_REG        = :GFCTB0L0.HINCL-REG
           END-EXEC.

           IF  (SQLCODE                   NOT EQUAL ZEROS) OR
               (SQLWARN0                  EQUAL 'W')
                MOVE 'DB2'                TO ERR-TIPO-ACESSO
                MOVE 'TCANCT_ADSAO_AGPTO' TO ERR-DBD-TAB
                MOVE 'UPDATE  '           TO ERR-FUN-COMANDO
                MOVE SQLCODE              TO ERR-SQL-CODE
                MOVE '0030'               TO ERR-LOCAL
                MOVE SPACES               TO ERR-SEGM
                PERFORM 9999-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1200-GRAVAR-ARQATUAL            SECTION.
      *----------------------------------------------------------------*

           MOVE CSIT-SOLTC-CANCT       OF GFCTB0L0
                                       TO WAFZ-CSIT-SOLTC-CANCT.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           WRITE FD-REG-ARQATUAL       FROM REG-GFCTWAFZ.

           PERFORM 0600-TESTAR-FS-ARQATUAL.

           ADD 1                       TO ACU-GRAVA-ARQATUAL.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1300-CHECK-POINT                SECTION.
      *----------------------------------------------------------------*

           INITIALIZE CKRS01-INTERFACE.

           MOVE 'DB2 '                 TO CK01-ID-DB2.
           MOVE 'P'                    TO CK01-FUNCAO.
           MOVE ACU-LIDOS-ATUTABEL     TO WRK-LIDOS-ATUTABEL.
           MOVE ACU-GRAVA-ARQATUAL     TO WRK-GRAVA-ARQATUAL.
           MOVE ACU-ATUALIZADOS-OK     TO WRK-ATUALIZADOS-OK.
           MOVE WRK-AREA-RESTART       TO CK01-AREA-RESTART.
           MOVE LENGTH OF WRK-AREA-RESTART
                                       TO CK01-TAM-AREA-RESTART.

           CALL WRK-CKRS0100           USING CKRS01-INTERFACE.

           IF  CK01-CODIGO-RETORNO     NOT EQUAL ZEROS
               DISPLAY '****  NO PROCESSAMENTO  ****'
               PERFORM 0300-TRATAR-ERRO-RESTART
           END-IF.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1400-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 1500-EMITIR-DISPLAY.

           CLOSE ATUTABEL
                 ARQATUAL.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 0400-TESTAR-FILE-STATUS.

           PERFORM 1600-FINALIZA-CHECK-POINT.

           STOP RUN.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1500-EMITIR-DISPLAY             SECTION.
      *----------------------------------------------------------------*

           DISPLAY '******************** GFCT2417 '
                   '********************'.
           DISPLAY '*                             '
                   '                   *'.
           DISPLAY '*        PROGRAMA ENCERRADO CO'
                   'M SUCESSO          *'.
           DISPLAY '* ============================'
                   '===============    *'.
           DISPLAY '*   PROCESSAMENTO DE : ' DPROCM-ATUAL OF GFCTB0A1
                   '                *'.
           DISPLAY '* ============================'
                   '=====              *'.

           MOVE ACU-LIDOS-ATUTABEL     TO WRK-MASCARA.

           DISPLAY '*   TOTAL DE LIDOS EM ATUTABEL.: '
                   WRK-MASCARA '  *'.

           MOVE ACU-GRAVA-ARQATUAL     TO WRK-MASCARA.

           DISPLAY '*   TOTAL DE GRAVADOS ARQATUAL.: '
                   WRK-MASCARA '  *'.

           MOVE ACU-ATUALIZADOS-OK     TO WRK-MASCARA.

           DISPLAY '*   TOTAL DE ATUALIZADOS - OK..: '
                   WRK-MASCARA '  *'.

           DISPLAY '*                             '
                   '                   *'.
           DISPLAY '*                   TERMINO NO'
                   'RMAL               *'.
           DISPLAY '*                             '
                   '                   *'.
           DISPLAY '******************** GFCT2417 '
                   '********************'.

      *----------------------------------------------------------------*
       1500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1600-FINALIZA-CHECK-POINT       SECTION.
      *----------------------------------------------------------------*

           INITIALIZE CKRS01-INTERFACE.

           MOVE 'DB2 '                 TO CK01-ID-DB2.
           MOVE 'F'                    TO CK01-FUNCAO.

           CALL WRK-CKRS0100           USING CKRS01-INTERFACE.

           IF  CK01-CODIGO-RETORNO     NOT EQUAL ZEROS
               DISPLAY '****  NA FINALIZACAO  ****'
               PERFORM 0300-TRATAR-ERRO-RESTART
           END-IF.

      *----------------------------------------------------------------*
       1600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999-TRATAR-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT2417'             TO ERR-PGM.

           IF  ERR-TIPO-ACESSO         EQUAL 'DB2'
               CALL 'POOL7100'         USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA
           ELSE
               CALL 'POOL7100'         USING WRK-BATCH
                                             ERRO-AREA
           END-IF.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

