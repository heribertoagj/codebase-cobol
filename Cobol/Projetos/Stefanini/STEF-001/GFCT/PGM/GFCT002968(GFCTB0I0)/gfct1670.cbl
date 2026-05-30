      *===============================================================*
       IDENTIFICATION                  DIVISION.
      *===============================================================*

       PROGRAM-ID. GFCT1670.
       AUTHOR.     DOUGLAS ALBINO KAMANSKI.
      *===============================================================*
      *              C P M  B R A X I S  -  S I S T E M A S           *
      *---------------------------------------------------------------*
      *    PROGRAMA....:  GFCT1670.                                   *
      *    PROGRAMADOR.:  DOUGLAS ALBINO KAMANSKI - CPM BRAXIS - P BCO*
      *    ANALISTA CPM:  KHARUZO INOCENCIO LEITE - CPM BRAXIS - P BCO*
      *    ANALISTA....:  RICARDO PEREIRA         - PROCKWORK  - GP 50*
      *    DATA........:  05/10/2007                                  *
      *---------------------------------------------------------------*
      *    OBJETIVO....:  BALANCE-LINE ENTRE O ARQUIVO DE EVENTOS E O *
      *    ARQUIVO REDUZIDO PARA ENCONTRAR FATOR PARA DESCONTO        *
      *    (DADOS-CONS) REFERENTE A CADA GRUPO DE CARTEIRAS.          *
      *---------------------------------------------------------------*
      *    ARQUIVOS....:                                              *
      *                DDNAME                      INCLUDE/BOOK       *
      *                  REDUZ23F                    I#GFCT51         *
      *                  APURA23B                    I#GFCTZY         *
      *                  APURA23C                    I#GFCTZY         *
      *---------------------------------------------------------------*
      *    BOOK'S......:                                              *
      *    I#BRAD7C - AREA UTILIZADA PELA BRAD7100.                   *
      *---------------------------------------------------------------*
      *    MODULOS.....:                                              *
      *    BRAD7100 - TRATAMENTO DE ERRO.                             *
      *===============================================================*

      *===============================================================*
       ENVIRONMENT                     DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *---------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *---------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *---------------------------------------------------------------*

       FILE-CONTROL.

           SELECT REDUZ23F ASSIGN      TO UT-S-REDUZ23F
           FILE STATUS                 IS WRK-FS-REDUZ23F.

           SELECT APURA23B ASSIGN      TO UT-S-APURA23B
           FILE STATUS                 IS WRK-FS-APURA23B.

           SELECT APURA23C ASSIGN      TO UT-S-APURA23C
           FILE STATUS                 IS WRK-FS-APURA23C.

      *===============================================================*
       DATA                            DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       FILE                            SECTION.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      *    INPUT:  ARQUIVO DE ENTRADA  -   REDUZ23F                   *
      *            ORG. SEQUENCIAL     -   LRECL   =   050            *
      *---------------------------------------------------------------*

       FD  REDUZ23F
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS  0 RECORDS.

       01 FD-REDUZ23F                  PIC  X(050).

      *---------------------------------------------------------------*
      *    INPUT:  ARQUIVO DE ENTRADA  -   APURA23B                   *
      *            ORG. SEQUENCIAL     -   LRECL   =   320            *
      *---------------------------------------------------------------*

       FD  APURA23B
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS  0 RECORDS.

       01 FD-APURA23B                  PIC  X(320).

      *---------------------------------------------------------------*
      *   OUTPUT:  ARQUIVO DE SAIDA    -   APURA23C                   *
      *            ORG. SEQUENCIAL     -   LRECL   =   320            *
      *---------------------------------------------------------------*

       FD  APURA23C
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS  0 RECORDS.

       01 FD-APURA23C                  PIC  X(320).

      *---------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           'INICIO DA WORKING SECTION STORAGE'.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           'AREA PARA ACUMULADORES'.
      *---------------------------------------------------------------*

       77  ACU-LIDOS-REDUZ23F          PIC  9(013) COMP-3  VALUE ZEROS.
       77  ACU-LIDOS-APURA23B          PIC  9(013) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-APURA23C          PIC  9(013) COMP-3  VALUE ZEROS.

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           'VARIAVEIS AUXILIARES'.
      *---------------------------------------------------------------*

       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.

       77  WRK-MASCARA                 PIC  Z.ZZZ.ZZZ.ZZZ.ZZ9
                                                           VALUE ZEROS.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA PARA REDEFINICAO DE VARIAVEIS'.
      *---------------------------------------------------------------*

       01  WRK-S9DE5                   PIC +9(005)         VALUE ZEROS.
       01  WRK-9DE5-R                  REDEFINES WRK-S9DE5.
           05  FILLER                  PIC  X(001).
           05  WRK-9DE5                PIC  9(005).

       01  WRK-S9DE13                  PIC +9(013)         VALUE ZEROS.
       01  WRK-9DE13-R                 REDEFINES WRK-S9DE13.
           05  FILLER                  PIC  X(001).
           05  WRK-9DE13               PIC  9(013).

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA PARA CHAVES DE ARQUIVOS'.
      *---------------------------------------------------------------*

       01  WRK-CHAVE-REDUZ23F.
           05  WRK-REDUZ-AGENCIA       PIC 9(005) COMP-3   VALUE ZEROS.
           05  WRK-REDUZ-CONTA         PIC 9(013) COMP-3   VALUE ZEROS.
           05  WRK-REDUZ-GRUPO         PIC 9(005) COMP-3   VALUE ZEROS.

       01  WRK-CHAVE-APURA23B.
           05  WRK-APURA-AGENCIA       PIC 9(005) COMP-3   VALUE ZEROS.
           05  WRK-APURA-CONTA         PIC 9(013) COMP-3   VALUE ZEROS.
           05  WRK-APURA-GRUPO         PIC 9(005) COMP-3   VALUE ZEROS.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA PARA TESTES DE FILE-STATUS'.
      *---------------------------------------------------------------*

       01  WRK-FS-REDUZ23F             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-APURA23B             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-APURA23C             PIC  X(002)         VALUE SPACES.

       01  WRK-ABERTURA                PIC  X(013)         VALUE
           ' NA ABERTURA '.
       01  WRK-LEITURA                 PIC  X(013)         VALUE
           ' NA LEITURA  '.
       01  WRK-GRAVACAO                PIC  X(013)         VALUE
           ' NA GRAVACAO '.
       01  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'MENSAGEM DE ERRO DE FILE-STATUS'.
      *---------------------------------------------------------------*

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

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA PARA LAYOUT DO ARQUIVO REDUZ23F'.
      *---------------------------------------------------------------*

       COPY I#GFCT51.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA PARA LAYOUT DOS ARQUIVOS APURA23B E APURA23C'.
      *---------------------------------------------------------------*

       COPY I#GFCTZY.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA PARA BRAD7100'.
      *---------------------------------------------------------------*

       COPY I#BRAD7C.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(030)         VALUE
           'FIM DA WORKING SECTION STORAGE'.
      *---------------------------------------------------------------*

      *===============================================================*
       PROCEDURE                       DIVISION.
      *===============================================================*

      *****************************************************************
      *    PROCEDIMENTOS INICIAIS DO PROGRAMA.                        *
      *****************************************************************
      *---------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *---------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-VERIFICAR-VAZIO.

           PERFORM 3000-PROCESSAR      UNTIL
                   WRK-CHAVE-APURA23B  EQUAL HIGH-VALUES.

           PERFORM 4000-FINALIZAR.

      *---------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *****************************************************************
      *    ABERTURA DOS ARQUIVOS E TESTE DE FILE-STATUS.              *
      *****************************************************************
      *---------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *---------------------------------------------------------------*

           OPEN INPUT  REDUZ23F
                       APURA23B
                OUTPUT APURA23C.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

           MOVE ZEROS                  TO WRK-APURA-GRUPO
                                          WRK-APURA-CONTA
                                          WRK-APURA-AGENCIA
                                          WRK-REDUZ-AGENCIA
                                          WRK-REDUZ-CONTA
                                          WRK-REDUZ-GRUPO.

      *---------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *****************************************************************
      *    TESTAR FILE-STATUS DOS ARQUIVOS.                           *
      *****************************************************************
      *---------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *---------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-REDUZ23F.

           PERFORM 1120-TESTAR-FS-APURA23B.

           PERFORM 1130-TESTAR-FS-APURA23C.

      *---------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *****************************************************************
      *    TESTAR FILE-STATUS DO ARQUIVO REDUZ23F.                    *
      *****************************************************************
      *---------------------------------------------------------------*
       1110-TESTAR-FS-REDUZ23F         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-REDUZ23F         NOT EQUAL '00'
               MOVE 'REDUZ23F'         TO  WRK-NOME-ARQ
               MOVE  WRK-FS-REDUZ23F   TO  WRK-FILE-STATUS
               MOVE  WRK-ERRO-BRAD7100 TO  ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *****************************************************************
      *    TESTAR FILE-STATUS DO ARQUIVO APURA23B.                    *
      *****************************************************************
      *---------------------------------------------------------------*
       1120-TESTAR-FS-APURA23B         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-APURA23B         NOT EQUAL '00'
               MOVE 'APURA23B'         TO  WRK-NOME-ARQ
               MOVE  WRK-FS-APURA23B   TO  WRK-FILE-STATUS
               MOVE  WRK-ERRO-BRAD7100 TO  ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *****************************************************************
      *    TESTAR FILE-STATUS DO ARQUIVO APURA23C.                    *
      *****************************************************************
      *---------------------------------------------------------------*
       1130-TESTAR-FS-APURA23C         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-APURA23C         NOT EQUAL '00'
               MOVE 'APURA23C'         TO  WRK-NOME-ARQ
               MOVE  WRK-FS-APURA23C   TO  WRK-FILE-STATUS
               MOVE  WRK-ERRO-BRAD7100 TO  ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1130-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *****************************************************************
      *    VERIFICAR SE OS ARQUIVOS DE ENTRADA ESTAO VAZIOS.          *
      *****************************************************************
      *---------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *---------------------------------------------------------------*

           PERFORM 2100-LER-REDUZ23F.

           IF  WRK-CHAVE-REDUZ23F      EQUAL HIGH-VALUES
               DISPLAY '***************** GFCT1670 *****************'
               DISPLAY '*                                          *'
               DISPLAY '*         ARQUIVO REDUZ23F VAZIO           *'
               DISPLAY '*                                          *'
               DISPLAY '***************** GFCT1670 *****************'
           END-IF.

           PERFORM 2200-LER-APURA23B.

           IF  WRK-CHAVE-APURA23B      EQUAL HIGH-VALUES
               DISPLAY '***************** GFCT1670 *****************'
               DISPLAY '*                                          *'
               DISPLAY '*         ARQUIVO APURA23B VAZIO           *'
               DISPLAY '*                                          *'
               DISPLAY '*         PROCESSAMENTO ENCERRADO          *'
               DISPLAY '*                                          *'
               DISPLAY '***************** GFCT1670 *****************'
               PERFORM 4000-FINALIZAR
           END-IF.

      *---------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *****************************************************************
      *    LEITURA DO ARQUIVO REDUZ23F.                               *
      *****************************************************************
      *---------------------------------------------------------------*
       2100-LER-REDUZ23F               SECTION.
      *---------------------------------------------------------------*

           READ REDUZ23F               INTO GFCT51-REDUZ23A.

           IF  WRK-FS-REDUZ23F         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHAVE-REDUZ23F
               GO TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1110-TESTAR-FS-REDUZ23F.

           MOVE GFCT51-AGENCIA         TO WRK-REDUZ-AGENCIA.
           MOVE GFCT51-CONTA           TO WRK-REDUZ-CONTA.
           MOVE GFCT51-GRUPO           TO WRK-REDUZ-GRUPO.

           ADD 1                       TO ACU-LIDOS-REDUZ23F.

      *---------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *****************************************************************
      *    LEITURA DO ARQUIVO APURA23B.                               *
      *****************************************************************
      *---------------------------------------------------------------*
       2200-LER-APURA23B               SECTION.
      *---------------------------------------------------------------*

           READ APURA23B               INTO GFCTZY-REG.

           IF  WRK-FS-APURA23B         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHAVE-APURA23B
               GO TO 2200-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1120-TESTAR-FS-APURA23B.

           MOVE ZY-CAG-DSTNO-MOVTO    TO WRK-S9DE5.
           MOVE WRK-9DE5              TO WRK-APURA-AGENCIA.
           MOVE ZY-CCTA-DSTNO-MOVTO   TO WRK-S9DE13.
           MOVE WRK-9DE13             TO WRK-APURA-CONTA.
           MOVE ZY-23-CGRP-CART-SIST  TO WRK-S9DE5.
           MOVE WRK-9DE5              TO WRK-APURA-GRUPO.

           ADD 1                      TO ACU-LIDOS-APURA23B.

      *---------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *****************************************************************
      *    PROCESSAMENTO PRINCIPAL DO PROGRAMA.                       *
      *****************************************************************
      *---------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *---------------------------------------------------------------*

           IF  WRK-CHAVE-APURA23B      EQUAL WRK-CHAVE-REDUZ23F
               MOVE GFCT51-DADO-CONS   TO ZY-CONSE-DADO-CONS
               PERFORM 3100-GRAVAR-APURA23C
               PERFORM 2200-LER-APURA23B
           ELSE
               IF  WRK-CHAVE-APURA23B  GREATER WRK-CHAVE-REDUZ23F
                   PERFORM 2100-LER-REDUZ23F
               ELSE
                   MOVE ZEROS          TO ZY-CONSE-DADO-CONS
                   PERFORM 3100-GRAVAR-APURA23C
                   PERFORM 2200-LER-APURA23B
               END-IF
           END-IF.

      *---------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *****************************************************************
      *    GRAVAR REGISTROS NO ARQUIVO APURA23C.                      *
      *****************************************************************
      *---------------------------------------------------------------*
       3100-GRAVAR-APURA23C            SECTION.
      *---------------------------------------------------------------*

           WRITE FD-APURA23C           FROM GFCTZY-REG.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 1130-TESTAR-FS-APURA23C.

           ADD 1                       TO ACU-GRAVA-APURA23C.

      *---------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *****************************************************************
      *    FINALIZAR PROGRAMA.                                        *
      *****************************************************************
      *---------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *---------------------------------------------------------------*

           CLOSE REDUZ23F
                 APURA23B
                 APURA23C.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

           IF  ACU-LIDOS-REDUZ23F      GREATER ZEROS AND
               ACU-LIDOS-APURA23B      GREATER ZEROS
               PERFORM 4100-EMITIR-DISPLAY
           END-IF.

           STOP RUN.

      *---------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *****************************************************************
      *    EMISSAO DE TOTAIS DE LIDOS E GRAVADOS.                     *
      *****************************************************************
      *---------------------------------------------------------------*
       4100-EMITIR-DISPLAY            SECTION.
      *---------------------------------------------------------------*

           DISPLAY '*--------------------------------------------*'.
           DISPLAY '*              PROGRAMA GFCT1670             *'.
           DISPLAY '*              -----------------             *'.
           DISPLAY '*                                            *'.
           DISPLAY '*     PROCESSAMENTO EFETUADO COM SUCESSO     *'.
           DISPLAY '*--------------------------------------------*'.
           DISPLAY '*--------------------------------------------*'.
           DISPLAY '*                                            *'.

           MOVE ACU-LIDOS-APURA23B     TO WRK-MASCARA.

           DISPLAY '* REGS LIDOS APURA23B    = ' WRK-MASCARA   ' *'.
           DISPLAY '*                                            *'.

           MOVE ACU-LIDOS-REDUZ23F     TO WRK-MASCARA.

           DISPLAY '* REGS LIDOS REDUZ23F    = ' WRK-MASCARA   ' *'.
           DISPLAY '*                                            *'.

           MOVE ACU-GRAVA-APURA23C     TO WRK-MASCARA.

           DISPLAY '* REGS GRAVADOS APURA23C = ' WRK-MASCARA   ' *'.
           DISPLAY '*                                            *'.
           DISPLAY '*--------------------------------------------*'.

      *---------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *****************************************************************
      *    TRATAMENTO PARA CANCELAMENTO DO PROGRAMA.                  *
      *****************************************************************
      *---------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *---------------------------------------------------------------*

           MOVE 'APL'                  TO ERR-TIPO-ACESSO.
           MOVE 'GFCT1670'             TO ERR-PGM.

           CALL 'BRAD7100'             USING WRK-BATCH
                                             ERRO-AREA.

           GOBACK.

      *---------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *---------------------------------------------------------------*
