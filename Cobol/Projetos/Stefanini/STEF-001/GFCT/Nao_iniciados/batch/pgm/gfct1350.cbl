      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT1350.
       AUTHOR.     SONDA PROCWORK.
      *================================================================*
      *       S O N D A   P R O C W O R K   O U T S O U R C I N G      *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : GFCT1350                                     *
      *    ANALISTA     : VINICIUS - PROCWORK           - GRUPO 50     *
      *    DATA         : 17/04/2008                                   *
      *                                                                *
      *    OBJETIVO     : COMPLEMENTACAO DO ARQUIVO PREPARATORIO DA    *
      *                   FITA4, COM INFORMACAO DO TIPO DE PESSOA E    *
      *                   DO TIPO DE CONTA PARA DADOS DE ESTORNO       *
      *                                                                *
      *    ARQUIVOS:                                                   *
      *      DDNAME                              INCLUDE/BOOK          *
      *      ENTFITA4                              I#GFCTHF            *
      *      SAIFITA4                              I#GFCTHF            *
      *                                                                *
      *    BANCO DE DADOS:                                             *
      *     DB2                                                        *
      *      TABLE                               INCLUDE/BOOK          *
      *      DB2PRD.V01CLIENTE_AGENCIA             CLIEV004            *
      *      DB2PRD.V01CTA_POUPANCA                CLIEV007            *
      *      DB2PRD.V01CTA_CORRENTE                CLIEV008            *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      POOL1050 - MODULO PARA CONEXAO COM DB2.                   *
      *      POOL7100 - TRATAMENTO DE ERROS DA APLICACAO.              *
      *                                                                *
      *================================================================*

      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
        CONFIGURATION                  SECTION.
      *----------------------------------------------------------------*

        SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*

       FILE-CONTROL.

           SELECT ENTFITA4 ASSIGN      TO UT-S-ENTRADA
                      FILE STATUS      IS WRK-FS-ENTFITA4.

           SELECT SAIFITA4 ASSIGN      TO UT-S-SAIDA
                      FILE STATUS      IS WRK-FS-SAIFITA4.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT  :  ARQ GERADO - ESTORNO P/ CONTA. VIA TRIC           *
      *              ORG. SEQUENCIAL   -   LRECL = 0130                *
      *----------------------------------------------------------------*

       FD  ENTFITA4
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY I#GFCTHF.

      *----------------------------------------------------------------*
      *    OUTPUT :  ARQ GERADO - ESTORNO P/ CONTA. VIA TRIC           *
      *              ORG. SEQUENCIAL   -   LRECL = 0130                *
      *----------------------------------------------------------------*

       FD  SAIFITA4
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY I#GFCTHF.

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  INICIO DA WORKING GFCT1350  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*        ACUMULADORES          *'.
      *----------------------------------------------------------------*

       01  ACU-LIDOS-ENTFITA4          PIC 9(11)           VALUE ZEROS.
       01  ACU-GRAVA-SAIFITA4          PIC 9(11)           VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     VARIAVEIS AUXILIARES     *'.
      *----------------------------------------------------------------*

       01  WRK-BATCH                   PIC X(08)           VALUE
           'BATCH'.

       01  WRK-MASCARA                 PIC ZZZ.ZZZ.ZZZ.ZZ9.

       01  WRK-AGENCIA-ANT             PIC  9(005) COMP-3  VALUE ZEROS.
       01  WRK-CONTA-ANT               PIC  9(013) COMP-3  VALUE ZEROS.

       01  WRK-CID-CLI                 PIC  X(026)         VALUE SPACES.

       01  WRK-ACHOU-CONTA-POUP        PIC  X(001)         VALUE SPACES.
       01  WRK-ACHOU-CONTA-CORR        PIC  X(001)         VALUE SPACES.

       01  WRK-TIPO-CONTA              PIC  9(001)         VALUE ZEROS.
       01  WRK-TIPO-PESSOA             PIC  9(001)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     AREA PARA FILE-STATUS    *'.
      *----------------------------------------------------------------*

       01  WRK-FS-ENTFITA4             PIC X(02)           VALUE SPACES.
       01  WRK-FS-SAIFITA4             PIC X(02)           VALUE SPACES.

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
           '*  AREAS DE MENSAGENS DE ERRO  *'.
      *----------------------------------------------------------------*

       01  WRK-MSG-FS.
           03  FILLER                  PIC X(07)           VALUE SPACES.
           03  FILLER                  PIC X(02)           VALUE '* '.
           03  FILLER                  PIC X(05)           VALUE
               'ERRO '.
           03  WRK-OPERACAO            PIC X(13)           VALUE SPACES.
           03  FILLER                  PIC X(12)           VALUE
               ' DO ARQUIVO '.
           03  WRK-ARQUIVO             PIC X(08)           VALUE SPACES.
           03  FILLER                  PIC X(17)           VALUE
               ' - FILE-STATUS = '.
           03  WRK-FS                  PIC X(02)           VALUE SPACES.
           03  FILLER                  PIC X(02)           VALUE ' *'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*      AREA PARA POOL7100      *'.
      *----------------------------------------------------------------*

       COPY POL7100C.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     AREAS DE TABELAS DB2     *'.
      *----------------------------------------------------------------*

           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
                INCLUDE CLIEV004
           END-EXEC.

           EXEC SQL
                INCLUDE CLIEV007
           END-EXEC.

           EXEC SQL
                INCLUDE CLIEV008
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*    FIM DA WORKING GFCT1350   *'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL1050'.

           PERFORM 1000-INICIALIZAR.

           PERFORM 3000-PROCESSAR
             UNTIL WRK-FS-ENTFITA4     EQUAL '10'.

           PERFORM 9000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT  ENTFITA4
                OUTPUT SAIFITA4.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 9200-TESTAR-FILE-STATUS.

           PERFORM 1100-VERIFICAR-VAZIO.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1100-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 3100-LER-ENTFITA4.

           IF  WRK-FS-ENTFITA4         EQUAL '10'
               DISPLAY '***********************************************'
               DISPLAY '*                                             *'
               DISPLAY '*          PROGRAMA GFCT1350                  *'
               DISPLAY '*          -----------------                  *'
               DISPLAY '*                                             *'
               DISPLAY '*    ARQUIVO PRE-FITA4 ESTA SEM REGISTROS     *'
               DISPLAY '*   --------------------------------------    *'
               DISPLAY '*                                             *'
               DISPLAY '***********************************************'

               PERFORM 9000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           IF  GFCTHF-AGENCIA OF ENTFITA4
                                       NOT EQUAL WRK-AGENCIA-ANT OR
               GFCTHF-CONTA OF ENTFITA4
                                       NOT EQUAL WRK-CONTA-ANT
               PERFORM 3200-OBTER-INFORMACAO-CLIE

               MOVE GFCTHF-AGENCIA OF ENTFITA4
                                       TO WRK-AGENCIA-ANT
               MOVE GFCTHF-CONTA OF ENTFITA4
                                       TO WRK-CONTA-ANT
           END-IF.

           PERFORM 3300-MONTAR-SAIDA.

           PERFORM 3400-GRAVAR-SAIFITA4.

           PERFORM 3100-LER-ENTFITA4.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3100-LER-ENTFITA4               SECTION.
      *----------------------------------------------------------------*

           READ ENTFITA4.

           IF  WRK-FS-ENTFITA4         EQUAL '10'
               GO                      TO 3100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 9210-TESTAR-FS-ENTFITA4.

           ADD 1                       TO ACU-LIDOS-ENTFITA4.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3200-OBTER-INFORMACAO-CLIE      SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO WRK-TIPO-CONTA,
                                          WRK-TIPO-PESSOA.

           MOVE 'N'                    TO WRK-ACHOU-CONTA-POUP.

           PERFORM 3210-OBTER-RAZAO-CLIE-POUP.

           IF  WRK-ACHOU-CONTA-POUP    EQUAL 'S'
               PERFORM 3230-OBTER-TIPO-PESSOA-CLIE
           ELSE
               MOVE 'N'                TO WRK-ACHOU-CONTA-CORR

               PERFORM 3220-OBTER-RAZAO-CLIE-C-C

               IF  WRK-ACHOU-CONTA-CORR
                                       EQUAL 'S'
                   PERFORM 3230-OBTER-TIPO-PESSOA-CLIE
               ELSE
                   MOVE 3              TO WRK-TIPO-PESSOA

                   MOVE 3              TO WRK-TIPO-CONTA
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3210-OBTER-RAZAO-CLIE-POUP      SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTHF-AGENCIA OF ENTFITA4
                                       TO CJUNC-DEPDC OF CLIEV007.
           MOVE GFCTHF-CONTA OF ENTFITA4
                                       TO CCTA-CLI    OF CLIEV007.

           EXEC SQL
             SELECT  CID_CLI
               INTO :CLIEV007.CID-CLI
               FROM  DB2PRD.V01CTA_POUPANCA
              WHERE  CJUNC_DEPDC        = :CLIEV007.CJUNC-DEPDC
                AND  CCTA_CLI           = :CLIEV007.CCTA-CLI
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'V01CTA_POUPANCA'  TO ERR-DBD-TAB
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '0010'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM

               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               MOVE 'S'                TO WRK-ACHOU-CONTA-POUP

               MOVE 2                  TO WRK-TIPO-CONTA

               MOVE CID-CLI OF CLIEV007
                                       TO WRK-CID-CLI
           END-IF.

      *----------------------------------------------------------------*
       3210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3220-OBTER-RAZAO-CLIE-C-C       SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTHF-AGENCIA OF ENTFITA4
                                       TO CJUNC-DEPDC OF CLIEV008.
           MOVE GFCTHF-CONTA OF ENTFITA4
                                       TO CCTA-CLI    OF CLIEV008.

           EXEC SQL
             SELECT  CID_CLI,
                     CGRP_CTBIL_ATUAL,
                     CSGRP_CTBIL
               INTO :CLIEV008.CID-CLI,
                    :CLIEV008.CGRP-CTBIL-ATUAL,
                    :CLIEV008.CSGRP-CTBIL
               FROM  DB2PRD.V01CTA_CORRENTE
              WHERE  CJUNC_DEPDC        = :CLIEV008.CJUNC-DEPDC
                AND  CCTA_CLI           = :CLIEV008.CCTA-CLI
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'V01CTA_CORRENTE'  TO ERR-DBD-TAB
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '0020'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM

               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               MOVE 'S'                TO WRK-ACHOU-CONTA-CORR

               MOVE 1                  TO WRK-TIPO-CONTA

               MOVE CID-CLI OF CLIEV008
                                       TO WRK-CID-CLI
           END-IF.

      *----------------------------------------------------------------*
       3220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3230-OBTER-TIPO-PESSOA-CLIE     SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-CID-CLI            TO CID-CLI OF CLIEV004.

ST25X6*    EXEC SQL
ST25X6*      SELECT  CFLIAL_CGC
ST25X6*        INTO :CLIEV004.CFLIAL-CGC
ST25X6*        FROM  DB2PRD.V01CLIENTE_AGENCIA
ST25X6*       WHERE  CID_CLI            = :CLIEV004.CID-CLI
ST25X6*    END-EXEC.

           EXEC SQL
             SELECT  CFLIAL_CGC,
ST25X6               CFLIAL_CGC_ST
               INTO :CLIEV004.CFLIAL-CGC,
ST25X6              :CLIEV004.CFLIAL-CGC-ST
               FROM  DB2PRD.V01CLIENTE_AGENCIA
              WHERE  CID_CLI            = :CLIEV004.CID-CLI
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'V01CLIENTE_AGENCIA'
                                       TO ERR-DBD-TAB
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '0030'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM

               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
ST25X6*        IF  CFLIAL-CGC OF CLIEV004
ST25X6*                                    EQUAL ZEROS
ST25X6*            MOVE 1              TO WRK-TIPO-PESSOA
ST25X6*        ELSE
ST25X6*            MOVE 2              TO WRK-TIPO-PESSOA
ST25X6*        END-IF
               IF  CFLIAL-CGC-ST OF CLIEV004
                                       EQUAL SPACES
                   MOVE 1              TO WRK-TIPO-PESSOA
               ELSE
                   MOVE 2              TO WRK-TIPO-PESSOA
               END-IF
           ELSE
               MOVE 4                  TO WRK-TIPO-PESSOA
           END-IF.

      *----------------------------------------------------------------*
       3230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3300-MONTAR-SAIDA               SECTION.
      *----------------------------------------------------------------*

           INITIALIZE GFCTHF-ENTRADA                     OF SAIFITA4.

           MOVE GFCTHF-ENTRADA OF ENTFITA4
                                       TO GFCTHF-ENTRADA OF SAIFITA4.

           MOVE WRK-TIPO-PESSOA        TO GFCTHF-TIPO-PESSOA
                                                            OF SAIFITA4.
           MOVE WRK-TIPO-CONTA         TO GFCTHF-TIPO-CONTA OF SAIFITA4.

           IF  WRK-TIPO-CONTA          EQUAL 1
               MOVE CGRP-CTBIL-ATUAL OF CLIEV008
                                       TO GFCTHF-GRUPO-RAZAO
                                                            OF SAIFITA4
               MOVE CSGRP-CTBIL OF CLIEV008
                                       TO GFCTHF-SUB-GRUPO-RAZAO
                                                            OF SAIFITA4
           ELSE
               MOVE ZEROS              TO GFCTHF-GRUPO-RAZAO
                                                            OF SAIFITA4
                                          GFCTHF-SUB-GRUPO-RAZAO
                                                            OF SAIFITA4
           END-IF.

      *----------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3400-GRAVAR-SAIFITA4            SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           WRITE GFCTHF-ENTRADA OF SAIFITA4.

           PERFORM 9220-TESTAR-FS-SAIFITA4.

           ADD 1                       TO ACU-GRAVA-SAIFITA4.

      *----------------------------------------------------------------*
       3400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 9100-EMITIR-DISPLAY.

           CLOSE ENTFITA4
                 SAIFITA4.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 9200-TESTAR-FILE-STATUS.

           STOP RUN.

      *----------------------------------------------------------------*
       9000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9100-EMITIR-DISPLAY             SECTION.
      *----------------------------------------------------------------*

           DISPLAY '****************************************************
      -            '***'.
           DISPLAY '*
      -            '  *'.
           DISPLAY '*                 PROGRAMA GFCT1350
      -            '  *'.
           DISPLAY '*                 -----------------
      -            '  *'.
           DISPLAY '*
      -            '  *'.
           DISPLAY '*             PROCESSAMENTO EFETUADO COM SUCESSO
      -            '  *'.
           DISPLAY '*             ----------------------------------
      -            '  *'.
           DISPLAY '*
      -            '  *'.

           MOVE ACU-LIDOS-ENTFITA4     TO WRK-MASCARA.

           DISPLAY '* QTDE DE LIDOS DO ENTFITA4        = ' WRK-MASCARA
                   '  *'.

           MOVE ACU-GRAVA-SAIFITA4     TO WRK-MASCARA.

           DISPLAY '* QTDE DE GRAVADOS NO SAIFITA4     = ' WRK-MASCARA
                   '  *'.
           DISPLAY '****************************************************
      -            '***'.

      *----------------------------------------------------------------*
       9100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9200-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 9210-TESTAR-FS-ENTFITA4.

           PERFORM 9220-TESTAR-FS-SAIFITA4.

      *----------------------------------------------------------------*
       9200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9210-TESTAR-FS-ENTFITA4         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ENTFITA4         NOT EQUAL ZEROS
               MOVE 'ENTFITA4'         TO WRK-ARQUIVO
               MOVE WRK-FS-ENTFITA4    TO WRK-FS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-MSG-FS         TO ERR-TEXTO

               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       9210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9220-TESTAR-FS-SAIFITA4         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SAIFITA4         NOT EQUAL ZEROS
               MOVE 'SAIFITA4'         TO WRK-ARQUIVO
               MOVE WRK-FS-SAIFITA4    TO WRK-FS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-MSG-FS         TO ERR-TEXTO

               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       9220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT1350'             TO ERR-PGM.

           CALL 'POOL7100'             USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
