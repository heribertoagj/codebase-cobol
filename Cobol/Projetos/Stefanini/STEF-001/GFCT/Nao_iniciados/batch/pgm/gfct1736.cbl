      *===============================================================*
       IDENTIFICATION                  DIVISION.
      *===============================================================*

       PROGRAM-ID. GFCT1736.
       AUTHOR.     RICARDO PEREIRA DA SILVA.
      *===============================================================*
      *                     P R O C W O R K                           *
      *---------------------------------------------------------------*
      *    PROGRAMA....:  GFCT1736                                    *
      *    PROGRAMADOR :  RICARDO PEREIRA DA SILVA - PROCWORK         *
      *    ANALISTA....:  RICARDO PEREIRA DA SILVA - PROCWORK         *
      *    DATA........:  11/06/2008                                  *
      *---------------------------------------------------------------*
      *    OBJETIVO....:  BALANCE-LINE ENTRE OS ARQUIVOS SELECAO5 E   *
      *                   ACPFREDZ.                                   *
      *                   DESPREZA REGISTROS COM QUANTIDADE DE ACOES  *
      *                   MAIOR QUE 1199.                             *
      *---------------------------------------------------------------*
      *    ARQUIVOS....:                                              *
      *                DDNAME                      INCLUDE/BOOK       *
      *                   SELECAO5                    I#GFCT61        *
      *                   ACPFREDZ                    I#GFCTTA        *
      *                   SELECAO6                    I#GFCT61        *
      *---------------------------------------------------------------*
      *    BOOK'S......:                                              *
      *    I#BRAD7C  - AREA DE COMUNICACAO COM DA POOL7100            *
      *---------------------------------------------------------------*
      *    MODULOS.....:                                              *
      *    BRAD7100 - TRATAMENTO DE ERROS QUANDO PROGRAMA INVALIDO    *
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

           SELECT  ACPFREDZ ASSIGN     TO UT-S-ACPFREDZ
                      FILE STATUS      IS WRK-FS-ACPFREDZ.

           SELECT  SELECAO5 ASSIGN     TO UT-S-SELECAO5
                      FILE STATUS      IS WRK-FS-SELECAO5.

           SELECT  SELECAO6 ASSIGN     TO UT-S-SELECAO6
                      FILE STATUS      IS WRK-FS-SELECAO6.

      *===============================================================*
       DATA                            DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       FILE                            SECTION.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      *    INPUT:  GERACAO MOVTO P/ COBRANCA DE FICHA CADASTRAL       *
      *            ORG. SEQUENCIAL     -   LRECL   =   030            *
      *---------------------------------------------------------------*

       FD  SELECAO5
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCT61'.

      *---------------------------------------------------------------*
      *    INPUT:  REGISTRO DE ACIONISTAS COM NUMERO DE CPF.          *
      *            ORG. SEQUENCIAL     -   LRECL   =   040            *
      *---------------------------------------------------------------*

       FD  ACPFREDZ
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTTA'.

      *---------------------------------------------------------------*
      *    OUTPUT: LAYOUT DE SAIDA - GERACAO MOVTO P/ COBR FICHA CAD. *
      *            ORG. SEQUENCIAL     -   LRECL   =   030            *
      *---------------------------------------------------------------*

       FD  SELECAO6
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-SELECAO6          PIC X(030).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** INICIO DA WORKING GFCT1736 ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           'AREA PARA ACUMULADORES'.
      *----------------------------------------------------------------*

       77  ACU-LIDOS-SELECAO5          PIC  9(012) COMP-3  VALUE ZEROS.
       77  ACU-LIDOS-ACPFREDZ          PIC  9(012) COMP-3  VALUE ZEROS.
       77  ACU-GRAVADOS-SELECAO6       PIC  9(012) COMP-3  VALUE ZEROS.
       77  ACU-DESPREZA-SELECAO5       PIC  9(012) COMP-3  VALUE ZEROS.
       77  ACU-MAIS1199-ACOES          PIC  9(012) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA VARIAVEIS AUXILIARES *'.
      *----------------------------------------------------------------*

       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.
       77  WRK-MASCARA                 PIC ZZZ.ZZZ.ZZZ.ZZ9 VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA TESTE DE FILE-STATUS *'.
      *----------------------------------------------------------------*

       01  WRK-FS-SELECAO5             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-ACPFREDZ             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-SELECAO6             PIC  X(002)         VALUE SPACES.

       01  WRK-ABERTURA                PIC  X(013)         VALUE
           ' NA ABERTURA '.
       01  WRK-LEITURA                 PIC  X(013)         VALUE
           ' NA  LEITURA '.
       01  WRK-GRAVACAO                PIC  X(013)         VALUE
           ' NA GRAVACAO '.
       01  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA MENSAGENS DE ERRO *'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-BRAD7100.
           05 FILLER                   PIC  X(008)         VALUE
              '** ERRO '.
           05 WRK-OPERACAO             PIC  X(013)         VALUE SPACES.
           05 FILLER                   PIC  X(012)         VALUE
              ' DO ARQUIVO '.
           05 WRK-NOME-ARQ             PIC  X(008)         VALUE SPACES.
           05 FILLER                   PIC  X(017)         VALUE
              ' - FILE STATUS = '.
           05 WRK-FILE-STATUS          PIC  X(002)         VALUE SPACES.
           05 FILLER                   PIC  X(003)         VALUE ' **'.
           05 FILLER                   PIC  X(012)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA BRAD7100 *'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA CHAVES *'.
      *----------------------------------------------------------------*

       01  WRK-CHAVE-SELECAO5.
           05  WRK-CPF-SELECAO5        PIC  9(009)         VALUE ZEROS.

       01  WRK-CHAVE-ACPFREDZ.
           05  WRK-CPF-ACPFREDZ        PIC  9(009)         VALUE ZEROS.

       01  WRK-CPF-SELECAO5-ANT        PIC  9(009)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '** FIM DA WORKING GFCT1736 **'.
      *----------------------------------------------------------------*

      *===============================================================*
       PROCEDURE                       DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
      *                    INICIO DO PROCESSAMENTO                    *
      *---------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *---------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-VERIFICAR-VAZIO.

           PERFORM 3000-PROCESSAR      UNTIL
                   WRK-CHAVE-SELECAO5  EQUAL HIGH-VALUES.

           PERFORM 4000-FINALIZAR.

      *---------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      *               ROTINA DE INICIALIZACAO                         *
      *---------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *---------------------------------------------------------------*

           OPEN INPUT  SELECAO5
                       ACPFREDZ
                OUTPUT SELECAO6.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

      *---------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      *             ROTINA PARA TESTAR FILE STATUS                    *
      *---------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *---------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-SELECAO5.

           PERFORM 1120-TESTAR-FS-ACPFREDZ.

           PERFORM 1130-TESTAR-FS-SELECAO6.

      *---------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      *          TESTE DE FILE-STATUS DO ARQUIVO SELECAO5             *
      *---------------------------------------------------------------*
       1110-TESTAR-FS-SELECAO5         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-SELECAO5         NOT EQUAL '00'
               MOVE 'SELECAO5'         TO WRK-NOME-ARQ
               MOVE WRK-FS-SELECAO5    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-BRAD7100  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      *          TESTE DE FILE-STATUS DO ARQUIVO ACPFREDZ             *
      *---------------------------------------------------------------*
       1120-TESTAR-FS-ACPFREDZ         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-ACPFREDZ         NOT EQUAL '00'
               MOVE 'ACPFREDZ'         TO WRK-NOME-ARQ
               MOVE WRK-FS-ACPFREDZ    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-BRAD7100  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      *          TESTE DE FILE-STATUS DO ARQUIVO SELECAO6.            *
      *---------------------------------------------------------------*
       1130-TESTAR-FS-SELECAO6         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-SELECAO6         NOT EQUAL '00'
               MOVE 'SELECAO6'         TO WRK-NOME-ARQ
               MOVE WRK-FS-SELECAO6    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-BRAD7100  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1130-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      *       VERIFICA SE OS ARQUIVOS DE ENTRADA ESTAO VAZIOS         *
      *---------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *---------------------------------------------------------------*

           PERFORM 2100-LER-SELECAO5.

           IF  WRK-CHAVE-SELECAO5     EQUAL HIGH-VALUES
               DISPLAY '**************** GFCT1736 ****************'
               DISPLAY '*                                        *'
               DISPLAY '*      ARQUIVO SELECAO5 VAZIO            *'
               DISPLAY '*                                        *'
               DISPLAY '**************** GFCT1736 ****************'
           END-IF.

           PERFORM 2200-LER-ACPFREDZ.

           IF  WRK-CHAVE-ACPFREDZ     EQUAL HIGH-VALUES
               DISPLAY '**************** GFCT1736 ****************'
               DISPLAY '*                                        *'
               DISPLAY '*      ARQUIVO ACPFREDZ VAZIO            *'
               DISPLAY '*                                        *'
               DISPLAY '**************** GFCT1736 ****************'
           END-IF.

      *---------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      *            LEITURA DO ARQUIVO DE ENTRADA SELECAO5             *
      *---------------------------------------------------------------*
       2100-LER-SELECAO5               SECTION.
      *---------------------------------------------------------------*

           READ SELECAO5.

           IF  WRK-FS-SELECAO5         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHAVE-SELECAO5
               GO TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1110-TESTAR-FS-SELECAO5.

           MOVE GFCT61-CPF-NUM OF SELECAO5  TO WRK-CPF-SELECAO5.

           ADD 1                       TO ACU-LIDOS-SELECAO5.

           IF WRK-CPF-SELECAO5-ANT     EQUAL WRK-CPF-SELECAO5
              ADD 1                    TO ACU-DESPREZA-SELECAO5
              GO TO 2100-LER-SELECAO5
           ELSE
              MOVE WRK-CPF-SELECAO5    TO WRK-CPF-SELECAO5-ANT
           END-IF.

      *---------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      *            LEITURA DO ARQUIVO DE ENTRADA ACPFREDZ             *
      *---------------------------------------------------------------*
       2200-LER-ACPFREDZ               SECTION.
      *---------------------------------------------------------------*

           READ ACPFREDZ.

           IF  WRK-FS-ACPFREDZ         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHAVE-ACPFREDZ
               GO TO 2200-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1120-TESTAR-FS-ACPFREDZ.

           MOVE GFCTTA-CPF-NUM         TO WRK-CPF-ACPFREDZ.

           ADD 01                      TO ACU-LIDOS-ACPFREDZ.

      *---------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      *                 PROCESSAMENTO PRINCIPAL                       *
      *---------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *---------------------------------------------------------------*

           IF  WRK-CPF-ACPFREDZ EQUAL WRK-CPF-SELECAO5
               IF GFCTTA-QT-ACOES GREATER 1199
                  ADD 01          TO ACU-MAIS1199-ACOES
               ELSE
                   MOVE REG-GFCT61 TO FD-REG-SELECAO6
                   MOVE WRK-GRAVACAO   TO WRK-OPERACAO
                   WRITE FD-REG-SELECAO6 OF SELECAO6
                   PERFORM 1130-TESTAR-FS-SELECAO6
                   ADD 01              TO ACU-GRAVADOS-SELECAO6
               END-IF
               PERFORM 2100-LER-SELECAO5
           ELSE
               IF  WRK-CPF-ACPFREDZ GREATER WRK-CPF-SELECAO5
                   MOVE REG-GFCT61 TO FD-REG-SELECAO6
                   MOVE WRK-GRAVACAO   TO WRK-OPERACAO
                   WRITE FD-REG-SELECAO6    OF SELECAO6
                   PERFORM 1130-TESTAR-FS-SELECAO6
                   ADD 01              TO ACU-GRAVADOS-SELECAO6
                   PERFORM 2100-LER-SELECAO5
               ELSE
                   PERFORM 2200-LER-ACPFREDZ
               END-IF
           END-IF.

      *---------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      *                 FINALIZACAO DE PROCEDIMENTOS                  *
      *---------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *---------------------------------------------------------------*

           CLOSE ACPFREDZ
                 SELECAO5
                 SELECAO6.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

           PERFORM 4100-EMITIR-DISPLAY.

           STOP RUN.

      *---------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      *  EMITIR ESTATISTICAS DE REGISTROS LIDOS E GRAVADOS.           *
      *---------------------------------------------------------------*
       4100-EMITIR-DISPLAY             SECTION.
      *---------------------------------------------------------------*

           DISPLAY '**************** GFCT1736 *********************'.
           DISPLAY '*                                             *'.
           DISPLAY '*           PROGRAMA GFCT1736                 *'.
           DISPLAY '*           -----------------                 *'.
           DISPLAY '*                                             *'.
           DISPLAY '*     PROCESSAMENTO EFETUADO COM SUCESSO      *'.
           DISPLAY '*---------------------------------------------*'.
           DISPLAY '*---------------------------------------------*'.
           DISPLAY '*                                             *'.
           MOVE ACU-LIDOS-ACPFREDZ     TO WRK-MASCARA.
           DISPLAY '* REGS LIDOS ACPFREDZ      =  ' WRK-MASCARA ' *'.
           DISPLAY '*                                             *'.
           MOVE ACU-LIDOS-SELECAO5     TO WRK-MASCARA.
           DISPLAY '* REGS LIDOS SELECAO5      =  ' WRK-MASCARA ' *'.
           DISPLAY '*                                             *'.
           MOVE ACU-GRAVADOS-SELECAO6  TO WRK-MASCARA.
           DISPLAY '* REGS GRAVADOS SELECAO6   =  ' WRK-MASCARA ' *'.
           DISPLAY '*                                             *'.
           MOVE ACU-DESPREZA-SELECAO5  TO WRK-MASCARA.
           DISPLAY '* REGS DUPLICADOS SELECAO5 =  ' WRK-MASCARA ' *'.
           DISPLAY '*                                             *'.
           MOVE ACU-MAIS1199-ACOES     TO WRK-MASCARA.
           DISPLAY '* REGS MAIORES 1199 ACOES  =  ' WRK-MASCARA ' *'.
           DISPLAY '*                                             *'.
           DISPLAY '**************** GFCT1736 *********************'.

      *---------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      *             ROTINA PARA PROCESSAMENTO DE ERRO                 *
      *---------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *---------------------------------------------------------------*

           MOVE 'APL'                  TO ERR-TIPO-ACESSO.
           MOVE 'GFCT1736'             TO ERR-PGM.

           CALL 'BRAD7100'             USING WRK-BATCH
                                             ERRO-AREA.

           GOBACK.

      *---------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *---------------------------------------------------------------*


