      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT2486.
       AUTHOR. VINICIUS.
      *================================================================*
      *                        WIPRO INFOOSERVER                       *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : GFCT2486                                     *
      *    PROGRAMADOR  : VINICIUS                                     *
      *    ANALISTA     : VINICIUS                                     *
      *    DATA         : FEV-2018                                     *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      IDENTIFICA EVENTOS NEXT QUE NAO PASSAM PELA               *
      *      ISENCAO POR TEST DRIVE - POR JA TER TIDO UMA ADESAO       *
      *      NEXT ANTERIOR.                                            *
      *                                                                *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                      INCLUDE/BOOK        *
      *                    NEXTFRAN                  I#GFCTVE          *
      *                    NEXTPAST                                    *
      *                    SEGCOBRA                  I#GFCTVE          *
      *                    SEGNEXTF                  I#GFCTVE          *
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

           SELECT NEXTFRAN ASSIGN      TO UT-S-NEXTFRAN
           FILE STATUS                 IS WRK-FS-NEXTFRAN.

           SELECT NEXTPAST ASSIGN      TO UT-S-NEXTPAST
           FILE STATUS                 IS WRK-FS-NEXTPAST.

           SELECT SEGCOBRA ASSIGN      TO UT-S-SEGCOBRA
           FILE STATUS                 IS WRK-FS-SEGCOBRA.

           SELECT SEGNEXTF ASSIGN      TO UT-S-SEGNEXTF
           FILE STATUS                 IS WRK-FS-SEGNEXTF.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT : REGISTROS DAS CESTAS NEXT - PASSIVEIS DE FRANQUIA   *
      *            ORG. SEQUENCIAL     -   LRECL = 300                 *
      *----------------------------------------------------------------*

       FD  NEXTFRAN
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTVE'.

      *----------------------------------------------------------------*
      *    INPUT : ARQUIVO COM ADESOES JA VENCIDADS PARA CESTAS NEXT   *
      *            ORG. SEQUENCIAL     -   LRECL = 110                 *
      *----------------------------------------------------------------*

       FD  NEXTPAST
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-NEXTPAST-REG.
           05  FD-ADESAO-REG           PIC X(094).
ST25X6*     05  FD-CPF-CNPJ             PIC 9(009).
ST25X6*     05  FD-FILIAL               PIC 9(005).
ST25X6*     05  FD-CONTROLE             PIC 9(002).
ST25X6      05  FD-CPF-CNPJ             PIC X(009).
ST25X6      05  FD-FILIAL               PIC X(004).
ST25X6      05  FD-CONTROLE             PIC 9(002).
ST25X6      05  FILLER                  PIC X(001).

      *----------------------------------------------------------------*
      *    OUTPUT: FRANQUEADOS PARA GRAVAR NA TABELA GFCTB092.         *
      *            ORG. SEQUENCIAL     -   LRECL = 132                 *
      *----------------------------------------------------------------*

       FD  SEGCOBRA
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTVE'.

      *----------------------------------------------------------------*
      *    OUTPUT: SEGUEM PARA VERIFICAR CARENCIA.                     *
      *            ORG. SEQUENCIAL     -   LRECL = 300                 *
      *----------------------------------------------------------------*

       FD  SEGNEXTF
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTVE'.

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

       77  ACU-LIDOS-NEXTFRAN          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-LIDOS-NEXTPAST          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-SEGCOBRA          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-SEGNEXTF          PIC  9(009) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA DE TESTES DE FILE STATUS *'.
      *----------------------------------------------------------------*

       77  WRK-FS-NEXTFRAN             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-NEXTPAST             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-SEGCOBRA             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-SEGNEXTF             PIC  X(002)         VALUE SPACES.

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

ST25X6*01 WRK-CHAVE-NEXTFRAN.
ST25X6*    10 WRK-COB-CPF-CNPJ          PIC  9(009)        VALUE ZEROS.
ST25X6*    10 WRK-COB-FILIAL            PIC  9(005)        VALUE ZEROS.
ST25X6*    10 WRK-COB-CONTROLE          PIC  9(002)        VALUE ZEROS.
ST25X6 01 WRK-CHAVE-NEXTFRAN.
ST25X6     10 WRK-COB-CPF-CNPJ          PIC  X(009)        VALUE SPACES.
ST25X6     10 WRK-COB-FILIAL            PIC  X(004)        VALUE SPACES.
ST25X6     10 WRK-COB-CONTROLE          PIC  9(002)        VALUE ZEROS.
ST25X6     10 FILLER                    PIC  X(001)        VALUE SPACES.

ST25X6*  01 WRK-CHAVE-NEXTPAST.
ST25X6*     10 WRK-NEX-CPF-CNPJ          PIC  9(009)        VALUE ZEROS.
ST25X6*     10 WRK-NEX-FILIAL            PIC  9(005)        VALUE ZEROS.
ST25X6*     10 WRK-NEX-CONTROLE          PIC  9(002)        VALUE ZEROS.
ST25X6 01 WRK-CHAVE-NEXTPAST.
ST25X6     10 WRK-NEX-CPF-CNPJ        PIC  X(009)        VALUE SPACES.
ST25X6     10 WRK-NEX-FILIAL          PIC  X(004)        VALUE SPACES.
ST25X6     10 WRK-NEX-CONTROLE        PIC  9(002)        VALUE ZEROS.
ST25X6     10 FILLER                  PIC  X(001)        VALUE SPACES.

ST25X6*01  WRK-AUXILIARES.
ST25X6*    05  WRK-CPF-CNPJ            PIC S9(009)V        VALUE ZEROS.
ST25X6*    05  WRK-CPF-CNPJ-AUX        REDEFINES WRK-CPF-CNPJ
ST25X6*                                PIC  9(009).

ST25X6*    05  WRK-FILIAL              PIC S9(005)V        VALUE ZEROS.
ST25X6*    05  WRK-FILIAL-AUX          REDEFINES WRK-FILIAL
ST25X6*                                PIC  9(005).
ST25X6*
ST25X6*    05  WRK-CONTROLE-X.
ST25X6*        07  WRK-CONTROLE-N      PIC  9(002)         VALUE ZEROS.

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
                   WRK-FS-NEXTFRAN     EQUAL '10'.

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

           OPEN INPUT  NEXTFRAN
                       NEXTPAST
                OUTPUT SEGCOBRA
                       SEGNEXTF.

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

           PERFORM 1210-TESTAR-FS-NEXTFRAN.

           PERFORM 1220-TESTAR-FS-NEXTPAST.

           PERFORM 1230-TESTAR-FS-SEGCOBRA.

           PERFORM 1250-TESTAR-FS-SEGNEXTF.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTE DE FILE STATUS DO ARQUIVO NEXTFRAN.                   *
      ******************************************************************
      *----------------------------------------------------------------*
       1210-TESTAR-FS-NEXTFRAN         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-NEXTFRAN         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-NEXTFRAN    TO WRK-FILE-STATUS
               MOVE 'NEXTFRAN'         TO WRK-NOME-ARQ
               MOVE WRK-ERRO-TEXTO     TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTE DE FILE STATUS DO ARQUIVO NEXTPAST.                   *
      ******************************************************************
      *----------------------------------------------------------------*
       1220-TESTAR-FS-NEXTPAST         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-NEXTPAST         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-NEXTPAST    TO WRK-FILE-STATUS
               MOVE 'NEXTPAST'         TO WRK-NOME-ARQ
               MOVE WRK-ERRO-TEXTO     TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTE DE FILE STATUS DO ARQUIVO SEGCOBRA.                   *
      ******************************************************************
      *----------------------------------------------------------------*
       1230-TESTAR-FS-SEGCOBRA         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SEGCOBRA         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-SEGCOBRA    TO WRK-FILE-STATUS
               MOVE 'SEGCOBRA'         TO WRK-NOME-ARQ
               MOVE WRK-ERRO-TEXTO     TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTE DE FILE STATUS DO ARQUIVO SEGNEXTF.                   *
      ******************************************************************
      *----------------------------------------------------------------*
       1250-TESTAR-FS-SEGNEXTF         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SEGNEXTF         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-SEGNEXTF    TO WRK-FILE-STATUS
               MOVE 'SEGNEXTF'         TO WRK-NOME-ARQ
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

           PERFORM 2100-LER-NEXTFRAN.

           IF WRK-FS-NEXTFRAN          EQUAL '10'
              DISPLAY '******************** GFCT2486 ******************'
              DISPLAY '*                                              *'
              DISPLAY '*              PROGRAMA GFCT2486               *'
              DISPLAY '*              -----------------               *'
              DISPLAY '*                                              *'
              DISPLAY '*          ARQUIVO DE NEXTFRAN VAZIO           *'
              DISPLAY '*                                              *'
              DISPLAY '******************** GFCT2486 ******************'
           END-IF.

           PERFORM 2200-LER-NEXTPAST.

           IF WRK-FS-NEXTPAST          EQUAL '10'
              DISPLAY '******************** GFCT2486 ******************'
              DISPLAY '*                                              *'
              DISPLAY '*              PROGRAMA GFCT2486               *'
              DISPLAY '*              -----------------               *'
              DISPLAY '*                                              *'
              DISPLAY '*             ARQUIVO DE NEXTPAST              *'
              DISPLAY '*           COM DADOS DE CESTAS NEXT JA        *'
              DISPLAY '*          ENCERRADAS ESTA VAZIO!!             *'
              DISPLAY '*                                              *'
              DISPLAY '******************** GFCT2486 ******************'
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    LEITURA DO ARQUIVO NEXTFRAN                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-LER-NEXTFRAN               SECTION.
      *----------------------------------------------------------------*

           READ NEXTFRAN.

           IF  WRK-FS-NEXTFRAN         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHAVE-NEXTFRAN
               GO TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1210-TESTAR-FS-NEXTFRAN.

           ADD 1                       TO ACU-LIDOS-NEXTFRAN.

ST25X6*   XMOVE VE-CPF-CNPJ OF NEXTFRAN
ST25X6*                                TO  WRK-CPF-CNPJ.
ST25X6*   XMOVE WRK-CPF-CNPJ-AUX       TO  WRK-COB-CPF-CNPJ.
ST25X6*
ST25X6*   XMOVE VE-FILIAL OF NEXTFRAN  TO  WRK-FILIAL.
ST25X6*   XMOVE WRK-FILIAL-AUX         TO  WRK-COB-FILIAL.
ST25X6*
ST25X6*   XMOVE VE-CONTROLE OF NEXTFRAN
ST25X6*                                TO  WRK-CONTROLE-X.
ST25X6*   XMOVE WRK-CONTROLE-N         TO  WRK-COB-CONTROLE.

ST25X6     MOVE VE-CPF-CNPJ-ST         OF NEXTFRAN
ST25X6                                 TO WRK-COB-CPF-CNPJ.
ST25X6     MOVE VE-FILIAL-ST           OF NEXTFRAN  
ST25X6                                 TO WRK-COB-FILIAL.
ST25X6     MOVE VE-CONTROLE-ST         OF NEXTFRAN
ST25X6                                 TO  WRK-COB-CONTROLE.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    LEITURA DO ARQUIVO NEXTPAST                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-LER-NEXTPAST               SECTION.
      *----------------------------------------------------------------*

           READ NEXTPAST.

           IF  WRK-FS-NEXTPAST         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHAVE-NEXTPAST
               GO TO 2200-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1220-TESTAR-FS-NEXTPAST.

           ADD 1                       TO ACU-LIDOS-NEXTPAST.

           MOVE FD-CPF-CNPJ            TO  WRK-NEX-CPF-CNPJ.

           MOVE FD-FILIAL              TO  WRK-NEX-FILIAL.

           MOVE FD-CONTROLE            TO  WRK-NEX-CONTROLE.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCESSAMENTO PRINCIPAL.                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           IF  WRK-CHAVE-NEXTFRAN      EQUAL WRK-CHAVE-NEXTPAST
                 PERFORM 3300-GRAVAR-SEGCOBRA
                 PERFORM 2100-LER-NEXTFRAN
           ELSE
               IF  WRK-CHAVE-NEXTFRAN  GREATER WRK-CHAVE-NEXTPAST
                   PERFORM 2200-LER-NEXTPAST
               ELSE
                   PERFORM 3400-GRAVAR-SEGNEXTF
                   PERFORM 2100-LER-NEXTFRAN
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    GRAVAR ARQUIVO SEGCOBRA                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       3300-GRAVAR-SEGCOBRA            SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO            TO WRK-OPERACAO.
           MOVE REG-IGFCTVE OF NEXTFRAN TO REG-IGFCTVE OF SEGCOBRA.

           WRITE REG-IGFCTVE            OF SEGCOBRA.

           PERFORM 1230-TESTAR-FS-SEGCOBRA.

           ADD 1                        TO ACU-GRAVA-SEGCOBRA.

      *----------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    GRAVAR ARQUIVO SEGNEXTF                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       3400-GRAVAR-SEGNEXTF            SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO            TO WRK-OPERACAO.
           MOVE REG-IGFCTVE OF NEXTFRAN TO REG-IGFCTVE OF SEGNEXTF.

           WRITE REG-IGFCTVE            OF SEGNEXTF.

           PERFORM 1250-TESTAR-FS-SEGNEXTF.

           ADD 1                        TO ACU-GRAVA-SEGNEXTF.

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

           CLOSE NEXTFRAN
                 NEXTPAST
                 SEGCOBRA
                 SEGNEXTF.

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

           MOVE ACU-LIDOS-NEXTFRAN     TO WRK-MASCARA.

           DISPLAY '******************** GFCT2486 ******************'.
           DISPLAY '*                                              *'.
           DISPLAY '*              PROGRAMA GFCT2486               *'.
           DISPLAY '*              -----------------               *'.
           DISPLAY '*                                              *'.
           DISPLAY '*  LIDOS NO ARQUIVO NEXTFRAN...: ' WRK-MASCARA '  *'
           DISPLAY '*                                              *'.

           MOVE ACU-LIDOS-NEXTPAST     TO WRK-MASCARA.

           DISPLAY '*  LIDOS NO ARQUIVO NEXTPAST...: ' WRK-MASCARA '  *'
           DISPLAY '*                                              *'.

           MOVE ACU-GRAVA-SEGCOBRA     TO WRK-MASCARA.

           DISPLAY '*  GRAVADOS NO ARQUIVO SEGCOBRA: ' WRK-MASCARA '  *'
           DISPLAY '*                                              *'.

           MOVE ACU-GRAVA-SEGNEXTF     TO WRK-MASCARA.

           DISPLAY '*  GRAVADOS NO ARQUIVO SEGNEXTF: ' WRK-MASCARA '  *'
           DISPLAY '*                                              *'.
           DISPLAY '******************** GFCT2486 ******************'.

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    EXECUTA PROCEDIMENTO DE CANCELAMENTO DO PGM EM CASO DE ERRO *
      ******************************************************************
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT2486'             TO ERR-PGM.

           CALL 'POOL7100'             USING WRK-BATCH
                                             ERRO-AREA.
           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

