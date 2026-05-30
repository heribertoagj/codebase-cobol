      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. BVVE0001.
       AUTHOR.     FRANCISMARA.
      *================================================================*
      *              B S I                  S I S T E M A S            *
      *----------------------------------------------------------------*
      *    PROGRAMA....:  BVVE0001                                     *
      *    PROGRAMADOR.:  FRANCISMARA             - BSI                *
      *    DATA........:  JULHO-2012                                   *
      *----------------------------------------------------------------*
      *    OBJETIVO....:  FAZER UPDATE DOS CAMPOS NOVOS TELEFONE       *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#BRAD7C - AREA PARA TRATAMENTO DE ERRO PELA BRAD7100.      *
      *    I#CKRS01 - AREA PARA CKRS0100.                              *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    CKRS1000 - EFETUA PRE CONEXAO DB2.                          *
      *    CKRS0100 - MODULO PARA TRATAMENTO DE CHECKPOINT/RESTART     *
      *    BRAD7100 - TRATAMENTO DE ERROS.                             *
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

           SELECT ENTRADA ASSIGN       TO UT-S-ENTRADA
           FILE STATUS                 IS WRK-FS-ENTRADA.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT : LAYOUT ARQUIVO                                      *
      *            ORG. SEQUENCIAL     - LRECL = 165                   *
      *----------------------------------------------------------------*

       FD  ENTRADA
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-ENTRADA                 PIC X(165).


      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           'INICIO DA WORKING STORAGE SECTION'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA PARA VARIAVEIS AUXILIARES'.
      *----------------------------------------------------------------*

       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.
       77  WRK-MASCARA                 PIC  Z.ZZZ.ZZZ.ZZ9  VALUE ZEROS.
       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  ACU-LIDOS-ENTRADA           PIC  9(010) COMP-3  VALUE ZEROS.

       01  WRK-NLIN-TFONI-NULL         PIC S9(04) COMP VALUE ZEROS.

       01  WRK-FONE-11                 PIC  9(011)     VALUE ZEROS.
       01  FILLER REDEFINES WRK-FONE-11.
           03  WRK-ZEROS               PIC  9(003).
           03  WRK-FONE-8              PIC  X(008).

       01  WRK-BVVEB000.
           10 WRK-CAG-CTA              PIC S9(12)V  COMP-3 VALUE +0.
           10 WRK-CJUNC-DEPDC          PIC S9(5)V  COMP-3 VALUE +0.
           10 WRK-CCTA-CORR            PIC S9(7)V  COMP-3 VALUE +0.
           10 WRK-IVDDOR-VISA          PIC X(40)   VALUE SPACES.
           10 WRK-CCGC-CPF             PIC S9(9)V  COMP-3 VALUE +0.
           10 WRK-CFLIAL-CGC           PIC S9(4)V  COMP-3 VALUE +0.
           10 WRK-CCTRL-CPF-CGC        PIC S9(2)V  COMP-3 VALUE +0.
           10 WRK-ELOGDR-COPLT         PIC X(40)   VALUE SPACES.
           10 WRK-IMUN                 PIC X(30)   VALUE SPACES.
           10 WRK-CSGL-UF              PIC X(2)   VALUE SPACES.
           10 WRK-CCEP                 PIC S9(5)V  COMP-3 VALUE +0.
           10 WRK-CCOMPL-CEP           PIC X(3)   VALUE SPACES.
           10 WRK-CDDD                 PIC X(4)   VALUE SPACES.
           10 WRK-CFONE                PIC X(8)   VALUE SPACES.
           10 WRK-VLIM-ANTCP-VDDOR     PIC S9(13)V9(2) COMP-3 VALUE +0.
           10 WRK-PLIM-ANTEC-VDDOR     PIC S9(3)V9(2)  COMP-3 VALUE +0.
7C2511     10 WRK-CCGC-CPF-ST          PIC X(9)   VALUE SPACES.
7C2511     10 WRK-CFLIAL-CGC-ST        PIC X(4)   VALUE SPACES.
7C2511     10 WRK-CCTRL-CPF-CGC-ST     PIC X(2)   VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'MENSAGENS DE ERRO'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-ARQUIVO.
           05 FILLER                   PIC  X(008)         VALUE
              '** ERRO '.
           05 WRK-OPERACAO             PIC  X(013)         VALUE SPACES.
           05 FILLER                   PIC  X(012)         VALUE
              ' DO ARQUIVO '.
           05 WRK-NOME-ARQUIVO         PIC  X(008)         VALUE SPACES.
           05 FILLER                   PIC  X(018)         VALUE
              ' - FILE STATUS =  '.
           05 WRK-FILE-STATUS          PIC  X(002)         VALUE SPACES.
           05 FILLER                   PIC  X(003)         VALUE ' **'.
           05 FILLER                   PIC  X(011)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA PARA TESTE DE FILE-STATUS'.
      *----------------------------------------------------------------*

       01  WRK-FS-ENTRADA             PIC  X(002)         VALUE SPACES.

       01  WRK-ABERTURA                PIC  X(013)         VALUE
           ' NA ABERTURA '.
       01  WRK-LEITURA                 PIC  X(013)         VALUE
           ' NA  LEITURA '.
       01  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA PARA RESTART'.
      *----------------------------------------------------------------*

       01 WRK-AREA-RESTART.
          05  WRK-LIDOS-ENTRADA       PIC  9(010)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA DA BRAD7100'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE ACESSO AO CKRS0100 *'.
      *----------------------------------------------------------------*

       COPY 'I#CKRS01'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA DB2 *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE BVVEB000
           END-EXEC.

           EXEC SQL
               DECLARE CSR01-BVVEB000 CURSOR  WITH HOLD
               FOR SELECT  NLIN_TFONI
               FROM DB2PRD.INFORMACAO_EDBS
               WHERE   CAG_CTA       = :WRK-CAG-CTA
                 AND   CJUNC_DEPDC   = :WRK-CJUNC-DEPDC
                 AND   CCTA_CORR     = :WRK-CCTA-CORR
7C2511*           AND   CCGC_CPF      = :WRK-CCGC-CPF
7C2511*           AND   CFLIAL_CGC    = :WRK-CFLIAL-CGC
7C2511*           AND   CCTRL_CPF_CGC = :WRK-CCTRL-CPF-CGC
7C2511           AND   CCGC_CPF_ST      = :WRK-CCGC-CPF-ST
7C2511           AND   CFLIAL_CGC_ST    = :WRK-CFLIAL-CGC-ST
7C2511           AND   CCTRL_CPF_CGC_ST = :WRK-CCTRL-CPF-CGC-ST
                 AND   CDDD          = :WRK-CDDD
                 AND   CFONE         = :WRK-CFONE
               FOR UPDATE OF NLIN_TFONI
           END-EXEC.


      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'FIM DA WORKING STORAGE       SECTION'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      ******************************************************************
      * ROTINA PRINCIPAL DO PROGRAMA                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-PRINCIPAL                  SECTION.
      *----------------------------------------------------------------*

           CALL 'CKRS1000'.

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-VERIFICAR-VAZIO

           PERFORM 3000-PROCESSAR      UNTIL
               WRK-FS-ENTRADA         EQUAL '10'.

           PERFORM 4000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINAS DE INICIALIZACAO.                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           PERFORM 1200-INICIALIZAR-RESTART.

           IF CK01-STATUS              EQUAL 'REST'
              MOVE CK01-AREA-RESTART(1:LENGTH OF WRK-AREA-RESTART)
                                       TO WRK-AREA-RESTART
              MOVE WRK-LIDOS-ENTRADA  TO ACU-LIDOS-ENTRADA
           END-IF.

           OPEN INPUT  ENTRADA.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTES DE DE FILE STATUS.                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1120-TESTAR-FS-ENTRADA.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      ******************************************************************
      *    TESTE DE FILE STATUS DO ARQUIVO ENTRADA.                   *
      ******************************************************************
      *----------------------------------------------------------------*
       1120-TESTAR-FS-ENTRADA         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ENTRADA         NOT EQUAL '00'
               MOVE 'APL'             TO ERR-TIPO-ACESSO
               MOVE 'ENTRADA'         TO WRK-NOME-ARQUIVO
               MOVE WRK-FS-ENTRADA    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-ARQUIVO  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DO CHECK-POINT.                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       1200-INICIALIZAR-RESTART   SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO CKRS01-INTERFACE.

           INITIALIZE WRK-AREA-RESTART
                      CKRS01-INTERFACE.

           MOVE SPACES                 TO CK01-PLAN.
           MOVE 'DB2 '                 TO CK01-ID-DB2.
           MOVE 'I'                    TO CK01-FUNCAO.
           MOVE WRK-AREA-RESTART       TO CK01-AREA-RESTART.
           MOVE LENGTH OF WRK-AREA-RESTART
                                       TO CK01-TAM-AREA-RESTART.

           PERFORM 1210-CHAMAR-MODULO-CKRS0100.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA QUE CHAMA O MODULO CKRS0100.                         *
      ******************************************************************
      *----------------------------------------------------------------*
       1210-CHAMAR-MODULO-CKRS0100     SECTION.
      *----------------------------------------------------------------*

           MOVE 'CKRS0100'             TO WRK-MODULO.

           CALL  WRK-MODULO            USING CKRS01-INTERFACE.
           IF  CK01-CODIGO-RETORNO     EQUAL 12
               PERFORM 4200-RETORNAR-PGM
           ELSE
               IF  CK01-CODIGO-RETORNO
                                       NOT EQUAL ZEROS
                   MOVE CK01-TEXTO-MENSAGEM(1:75)
                                       TO ERR-TEXTO
                   MOVE 'APL'          TO ERR-TIPO-ACESSO
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO
               END-IF
           END-IF.

      *----------------------------------------------------------------
       1210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR VAZIO.                                               *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-LER-ENTRADA.

           IF  WRK-FS-ENTRADA         EQUAL '10'
               DISPLAY '***************** BVVE0001 **************'
               DISPLAY '***                                   ***'
               DISPLAY '***       ARQUIVO ENTRADA VAZIO      ***'
               DISPLAY '***         PROGRAMA ENCERRADO        ***'
               DISPLAY '***                                   ***'
               DISPLAY '*****************BVVE0001****************'
               PERFORM 4100-ENCERRAR
           END-IF.


      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * LE O ARQUIVO ENTRADA.                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-LER-ENTRADA               SECTION.
      *----------------------------------------------------------------*

           READ ENTRADA                INTO WRK-BVVEB000.

           IF  WRK-FS-ENTRADA         EQUAL '10'
               GO TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1120-TESTAR-FS-ENTRADA.

           ADD 1                       TO ACU-LIDOS-ENTRADA.

7C2511* Conversao para variaveis -ST usadas no SQL
           MOVE WRK-CCGC-CPF             TO WRK-CCGC-CPF-ST.
           MOVE WRK-CFLIAL-CGC           TO WRK-CFLIAL-CGC-ST.
           MOVE WRK-CCTRL-CPF-CGC        TO WRK-CCTRL-CPF-CGC-ST.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAMENTO PRINCIPAL.                                       *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-PROCESSAR                        SECTION.
      *----------------------------------------------------------------*

           IF  WRK-CFONE               IS NUMERIC
               PERFORM 3200-FAZER-UPDATE
               IF  SQLCODE             EQUAL +100
                   DISPLAY '******* BVVE0001 ***********'
                   DISPLAY '*  CHAVE NAO ENCONTRADA    *'
                   DISPLAY '*  PARA ATUALIZACAO DA     *'
                   DISPLAY '*  BVVEB000                *'
                   DISPLAY '*  CHAVE NAO ENCONTRADA    *'
                   DISPLAY '*  WRK-CAG-CTA '  WRK-CAG-CTA
                   DISPLAY '*  WRK-CJUNC-DEPDC ' WRK-CJUNC-DEPDC
                   DISPLAY '*  WRK-CCTA-CORR ' WRK-CCTA-CORR
                   DISPLAY '*  WRK-CCGC-CPF ' WRK-CCGC-CPF
                   DISPLAY '*  WRK-CFLIAL-CGC  ' WRK-CFLIAL-CGC
                   DISPLAY '*  WRK-CCTRL-CPF-CGC ' WRK-CCTRL-CPF-CGC
                   DISPLAY '*  WRK-CCGC-CPF-ST ' WRK-CCGC-CPF-ST
                   DISPLAY '*  WRK-CFLIAL-CGC-ST ' WRK-CFLIAL-CGC-ST
                   DISPLAY '*  WRK-CCTRL-CPF-CGC-ST ' 
                                                 WRK-CCTRL-CPF-CGC-ST
                   DISPLAY '*  WRK-CDDD ' WRK-CDDD
                   DISPLAY '*  WRK-CFONE ' WRK-CFONE
                   DISPLAY '******* BVVE0001 ***********'
               ELSE
                   PERFORM 3300-CHECKPOINT-RESTART
               END-IF
           END-IF.

           PERFORM 2100-LER-ENTRADA.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      ******************************************************************
      *--------------------------------------      --------------------------*
       3200-FAZER-UPDATE             SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               OPEN CSR01-BVVEB000
           END-EXEC.

           IF  SQLCODE                   NOT EQUAL ZEROS
               MOVE 'DB2'                TO ERR-TIPO-ACESSO
               MOVE 'INFORMACAO_EDBS'    TO ERR-DBD-TAB
               MOVE 'OPEN'               TO ERR-FUN-COMANDO
               MOVE SQLCODE              TO ERR-SQL-CODE
               MOVE '0010'               TO ERR-LOCAL
               MOVE SPACES               TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           EXEC SQL
               FETCH CSR01-BVVEB000
               INTO  :BVVEB000.NLIN-TFONI
                        :WRK-NLIN-TFONI-NULL
           END-EXEC.

           IF  SQLCODE                   NOT EQUAL ZEROS
               MOVE 'DB2'                TO ERR-TIPO-ACESSO
               MOVE 'INFORMACAO_EDBS'    TO ERR-DBD-TAB
               MOVE 'FETCH '             TO ERR-FUN-COMANDO
               MOVE SQLCODE              TO ERR-SQL-CODE
               MOVE '0020'               TO ERR-LOCAL
               MOVE SPACES               TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  WRK-NLIN-TFONI-NULL       EQUAL -1
               MOVE ZEROS                TO NLIN-TFONI   OF BVVEB000
           END-IF.

           MOVE WRK-CFONE                TO WRK-FONE-8
           MOVE ZEROS                    TO WRK-ZEROS
           MOVE WRK-FONE-11              TO NLIN-TFONI OF BVVEB000
           MOVE ZEROS                    TO WRK-NLIN-TFONI-NULL

           EXEC SQL
               UPDATE DB2PRD.INFORMACAO_EDBS
               SET  NLIN_TFONI       = :BVVEB000.NLIN-TFONI
                                         :WRK-NLIN-TFONI-NULL
               WHERE   CURRENT OF CSR01-BVVEB000
           END-EXEC.

           IF  SQLCODE                   NOT EQUAL ZEROS AND +100
               MOVE 'DB2'                TO ERR-TIPO-ACESSO
               MOVE 'INFORMACAO_EDBS'    TO ERR-DBD-TAB
               MOVE 'UPDATE'             TO ERR-FUN-COMANDO
               MOVE SQLCODE              TO ERR-SQL-CODE
               MOVE '0030'               TO ERR-LOCAL
               MOVE SPACES               TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           EXEC SQL
               CLOSE CSR01-BVVEB000
           END-EXEC.

           IF  SQLCODE                   NOT EQUAL ZEROS
               MOVE 'DB2'                TO ERR-TIPO-ACESSO
               MOVE 'INFORMACAO_EDBS'    TO ERR-DBD-TAB
               MOVE 'CLOSE'              TO ERR-FUN-COMANDO
               MOVE SQLCODE              TO ERR-SQL-CODE
               MOVE '0040'               TO ERR-LOCAL
               MOVE SPACES               TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    EFETUAR O CHECK-POINT.                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       3300-CHECKPOINT-RESTART         SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-AREA-RESTART
                      CKRS01-INTERFACE.

           MOVE 'DB2 '                 TO CK01-ID-DB2.
           MOVE 'P'                    TO CK01-FUNCAO.
           MOVE LENGTH OF WRK-AREA-RESTART
                                       TO CK01-TAM-AREA-RESTART.
           MOVE ACU-LIDOS-ENTRADA     TO WRK-LIDOS-ENTRADA.
           MOVE WRK-AREA-RESTART       TO CK01-AREA-RESTART.
           PERFORM 1210-CHAMAR-MODULO-CKRS0100.

      *----------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA DE FINALIZACAO                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           DISPLAY '*****************BSCG0009****************'.
           DISPLAY '***                                   ***'.
           DISPLAY '***         PROCESSAMENTO  OK         ***'.
           DISPLAY '***                                   ***'.
           MOVE ACU-LIDOS-ENTRADA     TO WRK-MASCARA.
           DISPLAY '*** LIDOS    ENTRADA : ' WRK-MASCARA ' ***'.
           DISPLAY '***                                   ***'.
           DISPLAY '*****************BSCG0009****************'.

           PERFORM 4100-ENCERRAR.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ENCERRA O PROGRAMA.                                           *
      ******************************************************************
      *----------------------------------------------------------------*
       4100-ENCERRAR                   SECTION.
      *----------------------------------------------------------------*

           CLOSE ENTRADA.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

           PERFORM 4110-FINALIZAR-RESTART.

           STOP RUN.

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    FINALIZAR CHECK-POINT.                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       4110-FINALIZAR-RESTART          SECTION.
      *----------------------------------------------------------------*

           INITIALIZE CKRS01-INTERFACE.

           MOVE 'F'                    TO CK01-FUNCAO.
           MOVE 'DB2'                  TO CK01-ID-DB2.

           PERFORM 1210-CHAMAR-MODULO-CKRS0100.

      *----------------------------------------------------------------*
       4110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    FINALIZAR PROGRAMA.                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       4200-RETORNAR-PGM               SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       4200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCESSAMENTO DA ROTINA DE ERRO.                            *
      ******************************************************************
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           MOVE 'BVVE0001'             TO ERR-PGM.

           CALL 'BRAD7100'             USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
