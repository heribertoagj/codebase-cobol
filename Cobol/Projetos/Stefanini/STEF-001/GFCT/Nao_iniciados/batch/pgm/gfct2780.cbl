      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT2780.
       AUTHOR. VINICIUS.
      *================================================================*
      *                        WIPRO INFOOSERVER                       *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : GFCT2780                                     *
      *    PROGRAMADOR  : VINICIUS                                     *
      *    ANALISTA     : VINICIUS                                     *
      *    DATA         : AGO-2018                                     *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      DO ARQUIVO UTILIZADO PARA PULAR A ISENCAO POR TEST DRIVE  *
      *      POR O CLIENTE JA TER CQANCELADO CESTA NEXT -              *
      *      RETIRAR AS CESTAS CANCELADA DO NEXT LIGTH - 1187.         *
      *                                                                *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                      INCLUDE/BOOK        *
      *                    NEXTCANC                  --------          *
      *                    SEGCANCE                  --------          *
      *                    CANC1187                  --------          *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                         INCLUDE/BOOK    *
      *                                                                *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    POL7100C - AREA PARA TRATAMENTO DE ERROS PELA POOL7100.     *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    POOL7100 - TRATAMENTO DE ERROS PADRAO BRADESCO.             *
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

           SELECT NEXTCANC ASSIGN      TO UT-S-NEXTCANC
           FILE STATUS                 IS WRK-FS-NEXTCANC.

           SELECT SEGCANCE ASSIGN      TO UT-S-SEGCANCE
           FILE STATUS                 IS WRK-FS-SEGCANCE.

           SELECT CANC1187 ASSIGN      TO UT-S-CANC1187
           FILE STATUS                 IS WRK-FS-CANC1187.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT : NEXT CANCELADOS PARA NAO ISENTAR MAIS POR TD.       *
      *            ORG. SEQUENCIAL     -   LRECL = 110                 *
      *----------------------------------------------------------------*

       FD  NEXTCANC
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-NEXTCANC-REG.
           05  FD-ADESAO-REG.
               15 FD-CJUNC-DEPDC       PIC S9(5)V  COMP-3.
               15 FD-CCTA-CLI          PIC S9(7)V  COMP-3.
               15 FD-CSERVC-TARIF      PIC S9(5)V  COMP-3.
               15 FD-HINCL-REG         PIC X(26).
               15 FILLER               PIC X(58).
ST25X6* Adequação ao padrão ST: comentar definição original (numérica)
ST25X6* e incluir nova definição alfanumérica para CPF/CNPJ e Filial,
ST25X6* mantendo Controle numérico.
ST25X6*           05  FD-CPF-CNPJ             PIC 9(009).
ST25X6*           05  FD-FILIAL               PIC 9(005).
ST25X6*           05  FD-CONTROLE             PIC 9(002).
           05  FD-CPF-CNPJ             PIC X(009).
           05  FD-FILIAL               PIC X(004).
           05  FD-CONTROLE             PIC 9(002).
ST25X6* FILLER adicionado ao fim do registro para manter LRECL
           05  FILLER                 PIC X(001).

      *----------------------------------------------------------------*
      *    OUTPUT: NEXT CANCELADOS PARA NAO ISENTAR MAIS POR TD.       *
      *            ORG. SEQUENCIAL     -   LRECL = 110                 *
      *----------------------------------------------------------------*

       FD  SEGCANCE
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-SEGCANCE-REG              PIC  X(110).

      *----------------------------------------------------------------*
      *    OUTPUT: NEXT LIGHT - CESTA 1187 RETIRADO DESTE PROCESSO     *
      *            ORG. SEQUENCIAL     -   LRECL = 110                 *
      *----------------------------------------------------------------*

       FD  CANC1187
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-CANC1187-REG              PIC  X(110).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* INICIO DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA DE ACUMULADORES *'.
      *----------------------------------------------------------------*

       77  ACU-LIDOS-NEXTCANC          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-SEGCANCE          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-CANC1187          PIC  9(009) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA DE TESTES DE FILE STATUS *'.
      *----------------------------------------------------------------*

       77  WRK-FS-NEXTCANC             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-SEGCANCE             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-CANC1187             PIC  X(002)         VALUE SPACES.

       77  WRK-ABERTURA                PIC  X(013)         VALUE
           ' NA ABERTURA '.
       77  WRK-LEITURA                 PIC  X(013)         VALUE
           ' NA  LEITURA '.
       77  WRK-GRAVACAO                PIC  X(013)         VALUE
           ' NA GRAVACAO '.
       77  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA VARIAVEIS AUXILIARES *'.
      *----------------------------------------------------------------*

       77  WRK-MASCARA                 PIC  ZZZZ.ZZZ.999   VALUE ZEROS.
       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* MENSAGEM DE ERRO DE FILE-STATUS *'.
      *---------------------------------------------------------------*

       01  WRK-ERRO-TEXTO.
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

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA TRATAMENTO DE ERRO *'.
      *----------------------------------------------------------------*

       COPY POL7100C.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* FIM DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      ******************************************************************
      *    CHAMA AS ROTINAS A SEREM PROCESSADAS                        *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-VERIFICAR-VAZIO.

           PERFORM 3000-PROCESSAR      UNTIL
                   WRK-FS-NEXTCANC     EQUAL '10'.

           PERFORM 4000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCEDIMENTOS INICIAIS.                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT  NEXTCANC
                OUTPUT SEGCANCE
                       CANC1187.

           MOVE  WRK-ABERTURA          TO WRK-OPERACAO.

           PERFORM 1200-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    BUSCAR DATA PROCESSAMENTO.                                  *
      ******************************************************************

      ******************************************************************
      *    TESTE DE FILE STATUS DOS ARQUIVOS.                          *
      ******************************************************************
      *----------------------------------------------------------------*
       1200-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1210-TESTAR-FS-NEXTCANC.

           PERFORM 1230-TESTAR-FS-SEGCANCE.

           PERFORM 1250-TESTAR-FS-CANC1187.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTE DE FILE STATUS DO ARQUIVO NEXTCANC.                   *
      ******************************************************************
      *----------------------------------------------------------------*
       1210-TESTAR-FS-NEXTCANC         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-NEXTCANC         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-NEXTCANC    TO WRK-FILE-STATUS
               MOVE 'NEXTCANC'         TO WRK-NOME-ARQ
               MOVE WRK-ERRO-TEXTO     TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTE DE FILE STATUS DO ARQUIVO SEGCANCE.                   *
      ******************************************************************
      *----------------------------------------------------------------*
       1230-TESTAR-FS-SEGCANCE         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SEGCANCE         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-SEGCANCE    TO WRK-FILE-STATUS
               MOVE 'SEGCANCE'         TO WRK-NOME-ARQ
               MOVE WRK-ERRO-TEXTO     TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTE DE FILE STATUS DO ARQUIVO CANC1187.                   *
      ******************************************************************
      *----------------------------------------------------------------*
       1250-TESTAR-FS-CANC1187         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-CANC1187         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-CANC1187    TO WRK-FILE-STATUS
               MOVE 'CANC1187'         TO WRK-NOME-ARQ
               MOVE WRK-ERRO-TEXTO     TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1250-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICA SE ARQUIVO ESTA VAZIO.                             *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-LER-NEXTCANC.

           IF WRK-FS-NEXTCANC          EQUAL '10'
              DISPLAY '******************** GFCT2780 ******************'
              DISPLAY '*                                              *'
              DISPLAY '*              PROGRAMA GFCT2780               *'
              DISPLAY '*              -----------------               *'
              DISPLAY '*                                              *'
              DISPLAY '*          ARQUIVO DE NEXTCANC VAZIO           *'
              DISPLAY '*                                              *'
              DISPLAY '******************** GFCT2780 ******************'
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    LEITURA DO ARQUIVO NEXTCANC                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-LER-NEXTCANC               SECTION.
      *----------------------------------------------------------------*

           READ NEXTCANC.

           IF  WRK-FS-NEXTCANC         EQUAL '10'
               GO TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1210-TESTAR-FS-NEXTCANC.

           ADD 1                       TO ACU-LIDOS-NEXTCANC.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCESSAMENTO PRINCIPAL.                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           IF  FD-CSERVC-TARIF         NOT EQUAL  1187
               PERFORM 3300-GRAVAR-SEGCANCE
           ELSE
               PERFORM 3400-GRAVAR-CANC1187
           END-IF.

           PERFORM 2100-LER-NEXTCANC.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    GRAVAR ARQUIVO SEGCANCE                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       3300-GRAVAR-SEGCANCE            SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO            TO WRK-OPERACAO.

           WRITE FD-SEGCANCE-REG  FROM  FD-NEXTCANC-REG.

           PERFORM 1230-TESTAR-FS-SEGCANCE.

           ADD 1                        TO ACU-GRAVA-SEGCANCE.

      *----------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    GRAVAR ARQUIVO CANC1187                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       3400-GRAVAR-CANC1187            SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO            TO WRK-OPERACAO.

           WRITE FD-CANC1187-REG  FROM  FD-NEXTCANC-REG.

           PERFORM 1250-TESTAR-FS-CANC1187.

           ADD 1                        TO ACU-GRAVA-CANC1187.

      *----------------------------------------------------------------*
       3400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCEDIMENTOS REALIZADOS NO FINAL DO PROCESSAMENTO.         *
      ******************************************************************
      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 4100-EMITIR-TOTAIS.

           CLOSE NEXTCANC
                 SEGCANCE
                 CANC1187.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 1200-TESTAR-FILE-STATUS.

           STOP RUN.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    EMITIR TOTAIS                                               *
      ******************************************************************
      *----------------------------------------------------------------*
       4100-EMITIR-TOTAIS              SECTION.
      *----------------------------------------------------------------*

           MOVE ACU-LIDOS-NEXTCANC     TO WRK-MASCARA.

           DISPLAY '******************** GFCT2780 ******************'.
           DISPLAY '*                                              *'.
           DISPLAY '*              PROGRAMA GFCT2780               *'.
           DISPLAY '*              -----------------               *'.
           DISPLAY '*                                              *'.
           DISPLAY '*  LIDOS NO ARQUIVO NEXTCANC...: ' WRK-MASCARA '  *'
           DISPLAY '*                                              *'.

           MOVE ACU-GRAVA-SEGCANCE     TO WRK-MASCARA.

           DISPLAY '*  GRAVADOS NO ARQUIVO SEGCANCE: ' WRK-MASCARA '  *'
           DISPLAY '*                                              *'.

           MOVE ACU-GRAVA-CANC1187     TO WRK-MASCARA.

           DISPLAY '*  GRAVADOS NO ARQUIVO CANC1187: ' WRK-MASCARA '  *'
           DISPLAY '*                                              *'.
           DISPLAY '******************** GFCT2780 ******************'.

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    EXECUTA PROCEDIMENTO DE CANCELAMENTO DO PGM EM CASO DE ERRO *
      ******************************************************************
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT2780'             TO ERR-PGM.

           CALL 'POOL7100'             USING WRK-BATCH
                                             ERRO-AREA.
           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

