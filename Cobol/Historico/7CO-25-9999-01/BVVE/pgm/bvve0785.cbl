      *===============================================================*
       IDENTIFICATION                  DIVISION.
      *===============================================================*

       PROGRAM-ID. BVVE0785.
       AUTHOR.     LEANDRO RIBEIRO.
      *================================================================*
      *                  PRIME IT SOLUTIONS                            *
      *----------------------------------------------------------------*
      *    PROGRAMA....: BVVE0785                                      *
      *    PROGRAMADOR : LEANDRO RIBEIRO            PRIME IT SOLUTIONS *
      *    ANALISTA....: MARCIA BARRETO             PRIME IT SOLUTIONS *
      *    DATA........: JULHO/2019                                    *
      *----------------------------------------------------------------*
      *    OBJETIVO....: BATIMENTO                                     *
      *                  AUTORIZADOS X REDE                            *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *    ARQAUTOR - ARQUIVO DE CLIENTES PARA AUTORIZACAO             *
      *    ARQREDE  - ARQUIVO REDE                                     *
      *    SAIDAOK  - AUTORIZADOS COM DATA <   2019-04-08              *
      *    SAIDANOK - AUTORIZADOS COM DATA >=  2019-04-08              *
      *================================================================*

      *===============================================================*
       ENVIRONMENT                     DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *---------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *---------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       FILE-CONTROL.
      *---------------------------------------------------------------*

           SELECT  ARQAUTOR ASSIGN TO UT-S-ARQAUTOR
                      FILE STATUS IS WRK-FS-ARQAUTOR.

           SELECT  ARQREDE ASSIGN TO UT-S-ARQREDE
                      FILE STATUS IS WRK-FS-ARQREDE.

           SELECT  SAIDAOK ASSIGN TO UT-S-SAIDAOK
                      FILE STATUS IS WRK-FS-SAIDAOK.

           SELECT  SAIDANOK ASSIGN TO UT-S-SAIDANOK
                      FILE  STATUS IS WRK-FS-SAIDANOK.

      *===============================================================*
       DATA                            DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       FILE                            SECTION.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      *    INPUT :    ARQUIVO DE AUTORIZACOES                         *
      *               ORG. SEQUENCIAL   -   LRECL = 064               *
      *---------------------------------------------------------------*

       FD  ARQAUTOR
           RECORDING  MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-ARQAUTOR                PIC  X(064).

      *---------------------------------------------------------------*
      *    INPUT:     ARQUIVO REDE                                    *
      *               ORG. SEQUENCIAL   -   LRECL = 200               *
      *---------------------------------------------------------------*

       FD  ARQREDE
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-ARQREDE                  PIC  X(200).

      *---------------------------------------------------------------*
      *    SAIDA:     ARQUIVO SAIDA OK  < 2019-04-08                  *
      *               ORG. SEQUENCIAL   -   LRECL = 064               *
      *---------------------------------------------------------------*

       FD  SAIDAOK
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-SAIDAOK                  PIC  X(064).

      *---------------------------------------------------------------*
      *    SAIDA:     ARQUIVO SAIDA OK >= 2019-04-08                  *
      *               ORG. SEQUENCIAL   -   LRECL = 064               *
      *---------------------------------------------------------------*

       FD  SAIDANOK
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-SAIDANOK                 PIC  X(064).

      *---------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** BVVE0785 - INICIO DA AREA DE WORKING ***'.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ACUMULADORES ***'.
      *---------------------------------------------------------------*

       01  ACU-MULADORES.
           03  ACU-LID-ARQREDE         PIC  9(09) COMP-3   VALUE ZEROS.
           03  ACU-LID-ARQAUTOR        PIC  9(09) COMP-3   VALUE ZEROS.
           03  ACU-GRA-SAIDAOK         PIC  9(09) COMP-3   VALUE ZEROS.
           03  ACU-GRA-SAIDANOK        PIC  9(09) COMP-3   VALUE ZEROS.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** VARIAVEIS DE OPERACAO DE ARQUIVOS ***'.
      *---------------------------------------------------------------*

       01  WRK-FS-ARQAUTOR             PIC  X(02) VALUE SPACES.
       01  WRK-FS-ARQREDE              PIC  X(02) VALUE SPACES.
       01  WRK-FS-SAIDAOK              PIC  X(02) VALUE SPACES.
       01  WRK-FS-SAIDANOK             PIC  X(02) VALUE SPACES.

       01  WRK-ABERTURA                PIC  X(13) VALUE ' NA ABERTURA '.
       01  WRK-GRAVACAO                PIC  X(13) VALUE ' NA GRAVACAO '.
       01  WRK-FECHAMENTO              PIC  X(13) VALUE 'NO FECHAMENTO'.
       01  WRK-LEITURA                 PIC  X(13) VALUE ' NA LEITURA  '.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)      VALUE
           '*** AREAS AUXILIARES ***'.
      *---------------------------------------------------------------*
       01  WRK-BATCH                   PIC  X(008)     VALUE 'BATCH'.
       77  WRK-MASCARA                 PIC  ZZZ.ZZZ.ZZ9    VALUE ZEROS.

       01  WRK-DATA-CONDICAO         PIC  9(08)    VALUE  20190408.

       01  WRK-CHV-ARQREDE.
           10 WRK-AGEN-REDE          PIC  9(05)    VALUE ZEROS.
           10 WRK-CONTA-REDE         PIC  9(13)    VALUE ZEROS.
           10 WRK-BANDEIRA-REDE      PIC  9(03)    VALUE 003.
7C2511*    10 WRK-CPF-CNPF-REDE      PIC  9(15)    VALUE ZEROS.
7C2511     10 WRK-CPF-CNPF-REDE      PIC  X(15)    VALUE SPACES.

       01  WRK-CHV-ARQAUTOR.
           10 WRK-AGEN-DOMC          PIC  9(05)    VALUE ZEROS.
           10 WRK-CONTA-DOMC         PIC  9(13)    VALUE ZEROS.
           10 WRK-BANDEIRA-DOMC      PIC  9(03)    VALUE ZEROS.
7C2511*    10 WRK-CPF-CNPF-DOMC      PIC  9(15)    VALUE ZEROS.
7C2511     10 WRK-CPF-CNPF-DOMC      PIC  X(15)    VALUE SPACES.

7C2511*01  WRK-CNPJ                  PIC  9(15)    VALUE ZEROS.
7C2511 01  WRK-CNPJ                  PIC  X(15)    VALUE SPACES.
       01  WRK-CNPJ-R                REDEFINES WRK-CNPJ.
           10 WRK-CNPJ-Z             PIC  9(01).
           10 WRK-CNPJ-N             PIC  9(14).

7C2511*01  WRK-CPF                   PIC  9(15)    VALUE ZEROS.
7C2511 01  WRK-CPF                   PIC  X(15)    VALUE SPACES.
       01  WRK-CPF-R                 REDEFINES WRK-CPF.
           10 WRK-CPF-Z              PIC  9(04).
           10 WRK-CPF-N              PIC  9(11).

       01  WRK-S9-13A                 PIC +9(13)    VALUE ZEROS.
       01  FILLER                    REDEFINES WRK-S9-13A.
           03 FILLER                 PIC X(01).
           03 WRK-9-13A.
           10 WRK-9-6                PIC  9(06).
           10 WRK-9-7                PIC  9(07).

       01  WRK-CONTA                 PIC 9(13)    VALUE ZEROS.
       01  WRK-CONTA-R               REDEFINES WRK-CONTA.
           10 WRK-CONTA-NUM          PIC  9(12).
           10 WRK-CONTA-DIG          PIC  9(01).

       01  WRK-S9-5                   PIC +9(05)    VALUE ZEROS.
       01  FILLER                    REDEFINES WRK-S9-5.
           10 FILLER                 PIC  X(01).
           10 WRK-9-5                PIC  9(05).

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*   AREA MENSAGENS DE ERRO     *'.
      *---------------------------------------------------------------*

       01  WRK-MSG-ERRO.
          03  FILLER                   PIC X(07)   VALUE SPACES.
          03  FILLER                   PIC X(07)   VALUE
              '* ERRO '.
          03  WRK-OPERACAO             PIC X(13)   VALUE SPACES.
          03  FILLER                   PIC X(12)   VALUE
              ' DO ARQUIVO '.
          03  WRK-NOME-ARQ             PIC X(08)   VALUE SPACES.
          03  FILLER                   PIC X(17)   VALUE
              ' - FILE-STATUS = '.
          03  WRK-FILE-STATUS          PIC X(02)   VALUE SPACES.
          03  FILLER                   PIC X(02)   VALUE ' *'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(70)         VALUE
           '* LAYOUT PARA O ARQUIVO ARQREDE  *'.
      *----------------------------------------------------------------*

       COPY 'I#BVVELJ'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(70)         VALUE
           '* LAYOUT PARA O ARQUIVO SAIDAOK E SAIDANOK E ARQAUTOR *'.
      *----------------------------------------------------------------*

       COPY 'I#BVVELF'.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)      VALUE
           '*** AREA DA BRAD7100 ***'.
      *---------------------------------------------------------------*
       COPY 'I#BRAD7C'.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** FIM DA WORKING BVVE0785 ***'.
      *---------------------------------------------------------------*
           EJECT

      *===============================================================*
       PROCEDURE                   DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *---------------------------------------------------------------*

           PERFORM 1000-INICIAR-PROGRAMA.

           PERFORM 2000-VER-VAZIO.

           PERFORM 3000-PROCESSAR      UNTIL
                   WRK-FS-ARQAUTOR     EQUAL '10'.

           PERFORM 4000-FINALIZAR.

           STOP RUN.

      *---------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1000-INICIAR-PROGRAMA           SECTION.
      *---------------------------------------------------------------*

           OPEN INPUT  ARQAUTOR
                       ARQREDE
               OUTPUT  SAIDAOK
                       SAIDANOK.


           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 1200-TESTAR-FILE-STATUS.

      *---------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1200-TESTAR-FILE-STATUS         SECTION.
      *---------------------------------------------------------------*

           PERFORM 1220-TESTAR-FS-ARQAUTOR.

           PERFORM 1230-TESTAR-FS-ARQREDE.

           PERFORM 1240-TESTAR-FS-SAIDAOK.

           PERFORM 1260-TESTAR-FS-SAIDANOK.

      *---------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1220-TESTAR-FS-ARQAUTOR        SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-ARQAUTOR        NOT EQUAL '00'
               MOVE 'ARQAUTOR'        TO WRK-NOME-ARQ
               MOVE WRK-FS-ARQAUTOR   TO WRK-FILE-STATUS
               MOVE WRK-MSG-ERRO      TO ERR-TEXTO
               MOVE 'APL'             TO ERR-TIPO-ACESSO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1220-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1230-TESTAR-FS-ARQREDE          SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-ARQREDE          NOT EQUAL '00'
               MOVE 'ARQREDE'          TO WRK-NOME-ARQ
               MOVE WRK-FS-ARQREDE     TO WRK-FILE-STATUS
               MOVE WRK-MSG-ERRO       TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1230-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1240-TESTAR-FS-SAIDAOK          SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-SAIDAOK          NOT EQUAL '00'
               MOVE 'SAIDAOK'          TO WRK-NOME-ARQ
               MOVE WRK-FS-SAIDAOK     TO WRK-FILE-STATUS
               MOVE WRK-MSG-ERRO       TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1240-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1260-TESTAR-FS-SAIDANOK         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-SAIDANOK         NOT EQUAL '00'
               MOVE 'SAIDANOK'         TO WRK-NOME-ARQ
               MOVE WRK-FS-SAIDANOK    TO WRK-FILE-STATUS
               MOVE WRK-MSG-ERRO       TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1260-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2000-VER-VAZIO                  SECTION.
      *---------------------------------------------------------------*

           PERFORM 2200-LER-ARQAUTOR.

           IF  WRK-FS-ARQAUTOR         EQUAL '10'
               DISPLAY '*************** BVVE0785 ******************'
               DISPLAY '*                                         *'
               DISPLAY '*    ARQUIVO    ARQAUTOR                  *'
               DISPLAY '*              ESTA VAZIO                 *'
               DISPLAY '*                                         *'
               DISPLAY '*************** BVVE0785 ******************'
           END-IF.

           PERFORM 2100-LER-ARQREDE.

           IF  WRK-FS-ARQREDE          EQUAL '10'
               DISPLAY '*************** BVVE0785 ******************'
               DISPLAY '*                                         *'
               DISPLAY '*    ARQUIVO    ARQREDES                 *'
               DISPLAY '*              ESTA VAZIO                 *'
               DISPLAY '*                                         *'
               DISPLAY '*************** BVVE0785 ******************'
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2100-LER-ARQREDE                SECTION.
      *----------------------------------------------------------------*

           READ ARQREDE                INTO REG-BVVELJ.

           IF  WRK-FS-ARQREDE          EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHV-ARQREDE
               GO                      TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1230-TESTAR-FS-ARQREDE .

           PERFORM 2150-TRATAR-CHAVE-REDE  .

           ADD 1                           TO ACU-LID-ARQREDE.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2150-TRATAR-CHAVE-REDE          SECTION.
      *----------------------------------------------------------------*

           MOVE BVVELJ-AGENCIA-CRED        TO  WRK-AGEN-REDE

           MOVE BVVELJ-CONTA-CRED          TO  WRK-CONTA
           MOVE WRK-CONTA-NUM              TO  WRK-CONTA-REDE
           MOVE 003                        TO  WRK-BANDEIRA-REDE
           MOVE BVVELJ-CNPJ                TO  WRK-CPF-CNPF-REDE .

      *----------------------------------------------------------------*
       2150-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2200-LER-ARQAUTOR               SECTION.
      *----------------------------------------------------------------*

           READ ARQAUTOR               INTO BVVELF-REGISTRO.

           IF  WRK-FS-ARQAUTOR         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHV-ARQAUTOR
               GO                      TO 2200-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1220-TESTAR-FS-ARQAUTOR.

           PERFORM 2250-TRATAR-CHAVE-AUTOR.

           ADD 1                        TO ACU-LID-ARQAUTOR.

      *---------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *----------------------------------------------------------------*
       2250-TRATAR-CHAVE-AUTOR         SECTION.
      *----------------------------------------------------------------*

           MOVE BVVELF-AGEN            TO WRK-S9-5
           MOVE WRK-9-5                TO WRK-AGEN-DOMC

           MOVE BVVELF-CONTA           TO WRK-S9-13A
           MOVE WRK-9-7                TO WRK-CONTA-DOMC

           MOVE BVVELF-BANDEIRA        TO WRK-BANDEIRA-DOMC

7C2511*        IF   BVVELF-TP-PSSOA         EQUAL 'J'
7C2511*          MOVE ZEROS                          TO WRK-CNPJ-Z
7C2511*          MOVE BVVELF-CPF-CNPJ                TO WRK-CNPJ-N
7C2511*
7C2511*          MOVE WRK-CNPJ                       TO WRK-CPF-CNPF-DOMC
7C2511*         ELSE
7C2511*          MOVE ZEROS                          TO WRK-CPF-Z
7C2511*          MOVE BVVELF-CPF-NUM                 TO WRK-CPF-N
7C2511*
7C2511*          MOVE WRK-CPF                        TO WRK-CPF-CNPF-DOMC
7C2511*        END-IF.
7C2511         IF BVVELF-TP-PSSOA EQUAL 'J'
7C2511            MOVE WRK-CNPJ TO WRK-CPF-CNPF-DOMC
7C2511         ELSE
7C2511            MOVE ZEROS TO WRK-CPF-Z
7C2511            MOVE BVVELF-CPF-NUM TO WRK-CPF-N
7C2511            MOVE WRK-CPF TO WRK-CPF-CNPF-DOMC
7C2511         END-IF.

      *----------------------------------------------------------------*
       2250-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *---------------------------------------------------------------*

           IF  WRK-CHV-ARQAUTOR              EQUAL WRK-CHV-ARQREDE

               IF    BVVELJ-DATA-DE-FILIACAO LESS WRK-DATA-CONDICAO
                     PERFORM 3300-GRAVAR-SAIDAOK
                 ELSE
                     PERFORM 3200-GRAVAR-SAIDANOK
               END-IF

               PERFORM 2200-LER-ARQAUTOR
               PERFORM 2100-LER-ARQREDE

           ELSE
               IF  WRK-CHV-ARQAUTOR    GREATER  WRK-CHV-ARQREDE
                      PERFORM 2100-LER-ARQREDE
                ELSE
                   PERFORM 3200-GRAVAR-SAIDANOK
                   PERFORM 2200-LER-ARQAUTOR
               END-IF
           END-IF.

      *---------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3200-GRAVAR-SAIDANOK            SECTION.
      *---------------------------------------------------------------*

           MOVE  WRK-GRAVACAO           TO WRK-OPERACAO.

           WRITE FD-SAIDANOK            FROM  BVVELF-REGISTRO.

           PERFORM 1260-TESTAR-FS-SAIDANOK.
           ADD 1                        TO ACU-GRA-SAIDANOK.

      *---------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3300-GRAVAR-SAIDAOK            SECTION.
      *---------------------------------------------------------------*

           MOVE  WRK-GRAVACAO          TO WRK-OPERACAO.

           WRITE FD-SAIDAOK            FROM  BVVELF-REGISTRO.

           PERFORM 1240-TESTAR-FS-SAIDAOK.

           ADD 1                       TO ACU-GRA-SAIDAOK.

      *---------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *---------------------------------------------------------------*

           PERFORM 4100-EMITIR-DISPLAY.

           CLOSE ARQAUTOR
                 ARQREDE
                 SAIDAOK
                 SAIDANOK.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 1200-TESTAR-FILE-STATUS.

      *---------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       4100-EMITIR-DISPLAY             SECTION.
      *---------------------------------------------------------------*

           DISPLAY '*************** BVVE0785 ******************'
           DISPLAY '*                                         *'

           MOVE ACU-LID-ARQREDE       TO WRK-MASCARA.
           DISPLAY '*   REG. LIDOS ARQREDE  : ' WRK-MASCARA '     *'

           MOVE ACU-LID-ARQAUTOR     TO WRK-MASCARA.
           DISPLAY '*   REG. LIDOS ARQAUTOR : ' WRK-MASCARA '     *'

           MOVE ACU-GRA-SAIDAOK       TO WRK-MASCARA.
           DISPLAY '*   REG. GRAVA SAIDAOK  : ' WRK-MASCARA '     *'

           MOVE ACU-GRA-SAIDANOK      TO WRK-MASCARA.
           DISPLAY '*   REG. GRAVA SAIDANOK : ' WRK-MASCARA '     *'

           DISPLAY '*                                         *'
           DISPLAY '*************** BVVE0785 ******************'.

      *---------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *---------------------------------------------------------------*

           MOVE 'BVVE0785'             TO ERR-PGM.

           CALL 'BRAD7100'         USING WRK-BATCH
                                         ERRO-AREA.

           GOBACK.
      *---------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *---------------------------------------------------------------*
