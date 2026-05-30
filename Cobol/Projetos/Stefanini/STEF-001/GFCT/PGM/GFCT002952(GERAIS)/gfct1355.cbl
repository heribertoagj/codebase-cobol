      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT1355.
       AUTHOR.     SONDA PROCWORK.
      *================================================================*
      *       S O N D A   P R O C W O R K   O U T S O U R C I N G      *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : GFCT1355                                     *
      *    ANALISTA     : VINICIUS - PROCWORK           - GRUPO 50     *
      *    DATA         : 17/04/2008                                   *
      *                                                                *
      *    OBJETIVO     : COMPLEMENTACAO DO ARQUIVO PREPARATORIO DA    *
      *                   FITA4, COM INFORMACAO DO TIPO DE PESSOA E    *
      *                   DO TIPO DE CONTA PARA DADOS DE COBRANCA      *
      *                                                                *
      *    ARQUIVOS:                                                   *
      *      DDNAME                              INCLUDE/BOOK          *
      *      ENTFITA4                              I#GFCTJH            *
LIE   *      CADCTAC                               I#CHEQ04            *
LIE   *      CADPOUP                               I#CHEQ04            *
      *      SAIFITA4                              I#GFCTJH            *
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

LIE        SELECT CADCTAC  ASSIGN      TO UT-S-ENTCTAC
                      FILE STATUS      IS WRK-FS-CADCTAC.

LIE        SELECT CADPOUP  ASSIGN      TO UT-S-ENTPOUP
                      FILE STATUS      IS WRK-FS-CADPOUP.

           SELECT SAIFITA4 ASSIGN      TO UT-S-SAIDA
                      FILE STATUS      IS WRK-FS-SAIFITA4.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT  :  ARQ GERADO - COBRANCA P/ CONTA. VIA TRIC          *
      *              ORG. SEQUENCIAL   -   LRECL = 0130                *
      *----------------------------------------------------------------*

       FD  ENTFITA4
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY I#GFCTJH.

      *----------------------------------------------------------------*
LIE'  *    INPUT  :  ARQ CADASTRO DE CLIENTE CONTA CORRENTE - CLIEV004 *
      *              ORG. SEQUENCIAL   -   LRECL = 025                 *
      *----------------------------------------------------------------*

       FD  CADCTAC
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

LIE''  COPY I#CHEQ04.

      *----------------------------------------------------------------*
LIE'  *    INPUT  :  ARQ CADASTRO DE CLIENTE POUPANCA - CLIEV004       *
      *              ORG. SEQUENCIAL   -   LRECL = 025                 *
      *----------------------------------------------------------------*

       FD  CADPOUP
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

LIE''  COPY I#CHEQ04.

      *----------------------------------------------------------------*
      *    OUTPUT :  ARQ GERADO - COBRANCA P/ CONTA. VIA TRIC          *
      *              ORG. SEQUENCIAL   -   LRECL = 0130                *
      *----------------------------------------------------------------*

       FD  SAIFITA4
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY I#GFCTJH.

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  INICIO DA WORKING GFCT1355  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*        ACUMULADORES          *'.
      *----------------------------------------------------------------*

       01  ACU-LIDOS-ENTFITA4          PIC 9(11)  COMP-3   VALUE ZEROS.
LIE    01  ACU-LIDOS-CADCTAC           PIC 9(11)  COMP-3   VALUE ZEROS.
LIE    01  ACU-LIDOS-CADPOUP           PIC 9(11)  COMP-3   VALUE ZEROS.
LIE    01  ACU-LIDOS-CLIEV004          PIC 9(11)  COMP-3   VALUE ZEROS.
       01  ACU-GRAVA-SAIFITA4          PIC 9(11)  COMP-3   VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     VARIAVEIS AUXILIARES     *'.
      *----------------------------------------------------------------*

       01  WRK-BATCH                   PIC X(08)           VALUE
           'BATCH'.

       01  WRK-MASCARA                 PIC ZZZ.ZZZ.ZZZ.ZZ9.
       01  WRK-CID-CLI                 PIC  X(026)         VALUE SPACES.
       01  WRK-TIPO-CONTA              PIC  9(001)         VALUE ZEROS.
       01  WRK-TIPO-PESSOA             PIC  9(001)         VALUE ZEROS.
       01  WRK-SAIR-CTA                PIC  X(001)         VALUE SPACES.
LIE    01  WRK-SAIR-POU                PIC  X(001)         VALUE SPACES.
LIE    01  WRK-FORA7E8                 PIC  9(001)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*            CHAVES            *'.
      *----------------------------------------------------------------*

LIE    01  WRK-CH-MOVTO.
           03  WRK-MVTO-AGENCIA        PIC 9(05)   VALUE ZEROS.
           03  WRK-MVTO-CONTA          PIC 9(07)   VALUE ZEROS.

LIE    01  WRK-CH-MOVTO-ANT.
           03  WRK-MVTO-AGE-ANT        PIC 9(05)   VALUE ZEROS.
           03  WRK-MVTO-CTA-ANT        PIC 9(07)   VALUE ZEROS.

LIE'   01  WRK-JH-AGENCIA              PIC S9(005)  VALUE ZEROS.
       01  WRK-JH-AGENCIA-R  REDEFINES WRK-JH-AGENCIA.
LIE''      03  WRK-JH-AGE              PIC  9(005).

LIE'   01  WRK-JH-CONTA                PIC S9(013)  VALUE ZEROS.
       01  WRK-JH-CONTA-R   REDEFINES  WRK-JH-CONTA.
           03  WRK-JH-CTA              PIC  9(006).
LIE''      03  WRK-JH-CTA1             PIC  9(007).

LIE    01  WRK-CH-CLIE-CTA.
           03  WRK-CLIE-AGE-CTA        PIC 9(05)   VALUE ZEROS.
           03  WRK-CLIE-CTA-CTA        PIC 9(07)   VALUE ZEROS.

LIE    01  WRK-CH-CLIE-POUP.
           03  WRK-CLIE-AGE-POU        PIC 9(05)   VALUE ZEROS.
           03  WRK-CLIE-CTA-POU        PIC 9(07)   VALUE ZEROS.


      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     AREA PARA FILE-STATUS    *'.
      *----------------------------------------------------------------*

       01  WRK-FS-ENTFITA4             PIC X(02)           VALUE SPACES.
LIE    01  WRK-FS-CADCTAC              PIC X(02)           VALUE SPACES.
LIE    01  WRK-FS-CADPOUP              PIC X(02)           VALUE SPACES.
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
           '*    FIM DA WORKING GFCT1355   *'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL1050'.

           PERFORM 1000-INICIALIZAR.

LIE        PERFORM 2100-LER-CADCTAC.

LIE        PERFORM 2200-LER-CADPOUP.

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
LIE                    CADCTAC
LIE                    CADPOUP
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

           PERFORM 2000-LER-ENTFITA4.

           IF  WRK-FS-ENTFITA4         EQUAL '10'
               DISPLAY '***********************************************'
               DISPLAY '*                                             *'
               DISPLAY '*          PROGRAMA GFCT1355                  *'
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
       2000-LER-ENTFITA4               SECTION.
      *----------------------------------------------------------------*

           READ ENTFITA4.

           IF  WRK-FS-ENTFITA4         EQUAL '10'
               GO                      TO 2000-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 9210-TESTAR-FS-ENTFITA4.

LIE'       MOVE GFCTJH-AGENCIA OF ENTFITA4 TO WRK-JH-AGENCIA.
           MOVE WRK-JH-AGE                 TO WRK-MVTO-AGENCIA.
           MOVE GFCTJH-CONTA OF ENTFITA4   TO WRK-JH-CONTA.
LIE''      MOVE WRK-JH-CTA1                TO WRK-MVTO-CONTA.

           ADD 1                       TO ACU-LIDOS-ENTFITA4.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
LIE'   2100-LER-CADCTAC                SECTION.
      *----------------------------------------------------------------*

           READ CADCTAC.

           IF WRK-FS-CADCTAC     EQUAL '10'
              MOVE HIGH-VALUES      TO WRK-CH-CLIE-CTA
              GO   TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA          TO WRK-OPERACAO.

           PERFORM 9220-TESTAR-FS-CADCTAC.

           MOVE CHEQ-AGE OF CADCTAC  TO WRK-CLIE-AGE-CTA
           MOVE CHEQ-CTA OF CADCTAC  TO WRK-CLIE-CTA-CTA.

           ADD  1                    TO ACU-LIDOS-CADCTAC.

      *----------------------------------------------------------------*
LIE''  2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
LIE'   2200-LER-CADPOUP                SECTION.
      *----------------------------------------------------------------*

           READ CADPOUP.

           IF   WRK-FS-CADPOUP     EQUAL '10'
                MOVE HIGH-VALUES      TO WRK-CH-CLIE-POUP
                GO   TO 2200-99-FIM
           END-IF.

           IF   CHEQ-RAZ OF CADPOUP  NOT EQUAL 10510
                GO                       TO    2200-LER-CADPOUP
           END-IF.

           MOVE WRK-LEITURA          TO WRK-OPERACAO.

           PERFORM 9240-TESTAR-FS-CADPOUP.

           MOVE CHEQ-AGE OF CADPOUP  TO WRK-CLIE-AGE-POU.
           MOVE CHEQ-CTA OF CADPOUP  TO WRK-CLIE-CTA-POU.

           ADD  1                    TO ACU-LIDOS-CADPOUP.

      *----------------------------------------------------------------*
LIE''  2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

LIE'       IF    GFCTJH-CONTA OF ENTFITA4  LESS    1000000
                 PERFORM 3100-PROCESSA-CORRENTE
           ELSE
                 PERFORM 3200-PROCESSA-POUPANCA
LIE''      END-IF

LIE'       MOVE GFCTJH-AGENCIA OF ENTFITA4 TO WRK-JH-AGENCIA.
           MOVE WRK-JH-AGE                 TO WRK-MVTO-AGE-ANT.
           MOVE GFCTJH-CONTA OF ENTFITA4   TO WRK-JH-CONTA.
LIE''      MOVE WRK-JH-CTA1                TO WRK-MVTO-CTA-ANT

           PERFORM 3500-MONTAR-SAIDA

           PERFORM 3600-GRAVAR-SAIFITA4.

           PERFORM 2000-LER-ENTFITA4.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3100-PROCESSA-CORRENTE         SECTION.
      *----------------------------------------------------------------*

LIE        MOVE SPACES               TO      WRK-SAIR-CTA.
LIE        MOVE ZEROS                TO      WRK-FORA7E8.

           PERFORM 3120-OBTER-CLIE008.

LIE        IF   WRK-FORA7E8       EQUAL      3
LIE             GO                   TO      3100-99-FIM
LIE        END-IF

LIE'       IF  WRK-CH-MOVTO     NOT EQUAL  WRK-CH-MOVTO-ANT
               MOVE ZEROS              TO  WRK-TIPO-PESSOA
               PERFORM 3110-TRATA-CTA-CORRENTE    UNTIL
                       WRK-SAIR-CTA  EQUAL  'S'
LIE''      END-IF.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
LIE'   3110-TRATA-CTA-CORRENTE         SECTION.
      *----------------------------------------------------------------*

           IF   WRK-CH-MOVTO        EQUAL   WRK-CH-CLIE-CTA
             IF CHEQ-CPF-FIL OF CADCTAC   EQUAL   ZEROS
                MOVE 1                 TO   WRK-TIPO-PESSOA
             ELSE
                MOVE 2                 TO   WRK-TIPO-PESSOA
             END-IF
                MOVE 'S'               TO   WRK-SAIR-CTA
           ELSE
             IF  WRK-CH-MOVTO        LESS   WRK-CH-CLIE-CTA
                 PERFORM 3420-OBTER-TIPO-PESSOA-CLIE
                 MOVE 'S'              TO   WRK-SAIR-CTA
             ELSE
                 PERFORM 2100-LER-CADCTAC    UNTIL
                         WRK-CH-CLIE-CTA  NOT LESS   WRK-CH-MOVTO    OR
                         WRK-CH-CLIE-CTA     EQUAL   HIGH-VALUES
             END-IF
           END-IF.

      *----------------------------------------------------------------*
LIE''  3110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3120-OBTER-CLIE008            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTJH-AGENCIA OF ENTFITA4
                                       TO CJUNC-DEPDC OF CLIEV008.
           MOVE GFCTJH-CONTA OF ENTFITA4
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
               MOVE 1                     TO WRK-TIPO-CONTA
               MOVE CID-CLI OF CLIEV008   TO WRK-CID-CLI
           ELSE
               MOVE 3                     TO WRK-TIPO-PESSOA
                                             WRK-TIPO-CONTA
                                             WRK-FORA7E8
           END-IF.

      *----------------------------------------------------------------*
       3120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3200-PROCESSA-POUPANCA         SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES               TO      WRK-SAIR-POU.
LIE        MOVE ZEROS                TO      WRK-FORA7E8.

           PERFORM 3220-OBTER-CLIE007

LIE        IF   WRK-FORA7E8       EQUAL      3
LIE             GO                   TO      3200-99-FIM
LIE        END-IF

LIE'       IF  WRK-CH-MOVTO     NOT EQUAL  WRK-CH-MOVTO-ANT
               MOVE ZEROS              TO  WRK-TIPO-PESSOA
               PERFORM 3210-TRATA-POUPANCA    UNTIL
                       WRK-SAIR-POU  EQUAL  'S'
LIE''      END-IF.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
LIE'   3210-TRATA-POUPANCA             SECTION.
      *----------------------------------------------------------------*

           IF   WRK-CH-MOVTO        EQUAL   WRK-CH-CLIE-POUP
             IF CHEQ-CPF-FIL OF CADPOUP   EQUAL   ZEROS
                MOVE 1                 TO   WRK-TIPO-PESSOA
             ELSE
                MOVE 2                 TO   WRK-TIPO-PESSOA
             END-IF
                MOVE 'S'               TO   WRK-SAIR-POU
           ELSE
             IF  WRK-CH-MOVTO        LESS   WRK-CH-CLIE-POUP
                 PERFORM 3420-OBTER-TIPO-PESSOA-CLIE
                 MOVE 'S'              TO   WRK-SAIR-POU
             ELSE
                 PERFORM 2200-LER-CADPOUP    UNTIL
                         WRK-CH-CLIE-POUP NOT LESS   WRK-CH-MOVTO    OR
                         WRK-CH-CLIE-POUP    EQUAL   HIGH-VALUES
             END-IF
           END-IF.

      *----------------------------------------------------------------*
LIE''  3210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3220-OBTER-CLIE007       SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTJH-AGENCIA OF ENTFITA4
                                       TO CJUNC-DEPDC OF CLIEV007.
           MOVE GFCTJH-CONTA OF ENTFITA4
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
               MOVE 2                     TO WRK-TIPO-CONTA
               MOVE CID-CLI OF CLIEV007   TO WRK-CID-CLI
           ELSE
               MOVE 3                     TO WRK-TIPO-PESSOA
                                             WRK-TIPO-CONTA
LIE                                          WRK-FORA7E8
           END-IF.

      *----------------------------------------------------------------*
       3220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3420-OBTER-TIPO-PESSOA-CLIE     SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-CID-CLI            TO CID-CLI OF CLIEV004.

           EXEC SQL
             SELECT  CFLIAL_CGC
               INTO :CLIEV004.CFLIAL-CGC
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
               IF  CFLIAL-CGC OF CLIEV004
                                       EQUAL ZEROS
                   MOVE 1              TO WRK-TIPO-PESSOA
               ELSE
                   MOVE 2              TO WRK-TIPO-PESSOA
               END-IF
           ELSE
               MOVE 4                  TO WRK-TIPO-PESSOA
           END-IF.

           ADD 1                       TO ACU-LIDOS-CLIEV004.

      *----------------------------------------------------------------*
       3420-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3500-MONTAR-SAIDA               SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTJH-ENTRADA OF ENTFITA4
                                       TO GFCTJH-ENTRADA OF SAIFITA4.

           MOVE WRK-TIPO-PESSOA        TO GFCTJH-TIPO-PESSOA OF SAIFITA4

           MOVE WRK-TIPO-CONTA         TO GFCTJH-TIPO-CONTA OF SAIFITA4.

           IF  WRK-TIPO-CONTA          EQUAL 1
               MOVE CGRP-CTBIL-ATUAL OF CLIEV008
                                       TO GFCTJH-GRUPO-RAZAO OF SAIFITA4
               MOVE CSGRP-CTBIL OF CLIEV008
                                       TO GFCTJH-SUB-GRUPO-RAZAO
                                                            OF SAIFITA4
           ELSE
               MOVE ZEROS              TO GFCTJH-GRUPO-RAZAO OF SAIFITA4
                                          GFCTJH-SUB-GRUPO-RAZAO
                                                            OF SAIFITA4
           END-IF.

      *----------------------------------------------------------------*
       3500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3600-GRAVAR-SAIFITA4            SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           WRITE GFCTJH-ENTRADA OF SAIFITA4.

           PERFORM 9230-TESTAR-FS-SAIFITA4.

           ADD 1                       TO ACU-GRAVA-SAIFITA4.

      *----------------------------------------------------------------*
       3600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 9100-EMITIR-DISPLAY.

           CLOSE ENTFITA4
LIE              CADCTAC
LIE              CADPOUP
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
           DISPLAY '*                 PROGRAMA GFCT1355
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

LIE        MOVE ACU-LIDOS-CADCTAC      TO WRK-MASCARA.

LIE        DISPLAY '* QTDE DE LIDOS DO CADCTAC         = ' WRK-MASCARA
                   '  *'.

LIE        MOVE ACU-LIDOS-CADPOUP      TO WRK-MASCARA.

LIE        DISPLAY '* QTDE DE LIDOS DO CADPOUP         = ' WRK-MASCARA
                   '  *'.

LIE        MOVE ACU-LIDOS-CLIEV004     TO WRK-MASCARA.

LIE        DISPLAY '* QTDE DE LIDOS DA CLIEV004        = ' WRK-MASCARA
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

 LIE       PERFORM 9220-TESTAR-FS-CADCTAC.

           PERFORM 9230-TESTAR-FS-SAIFITA4.

 LIE       PERFORM 9240-TESTAR-FS-CADPOUP.

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

LIE'  *----------------------------------------------------------------*
       9220-TESTAR-FS-CADCTAC          SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-CADCTAC          NOT EQUAL ZEROS
               MOVE 'CADCTAC'          TO WRK-ARQUIVO
               MOVE WRK-FS-CADCTAC     TO WRK-FS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-MSG-FS         TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
LIE''  9220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9230-TESTAR-FS-SAIFITA4         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SAIFITA4         NOT EQUAL ZEROS
               MOVE 'SAIFITA4'         TO WRK-ARQUIVO
               MOVE WRK-FS-SAIFITA4    TO WRK-FS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-MSG-FS         TO ERR-TEXTO

               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       9230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

LIE'  *----------------------------------------------------------------*
       9240-TESTAR-FS-CADPOUP          SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-CADPOUP          NOT EQUAL ZEROS
               MOVE 'CADPOUP'          TO WRK-ARQUIVO
               MOVE WRK-FS-CADPOUP     TO WRK-FS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-MSG-FS         TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
LIE''  9240-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT1355'             TO ERR-PGM.

           CALL 'POOL7100'             USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
