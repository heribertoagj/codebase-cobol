      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT2484.
       AUTHOR. VINICIUS.
      *================================================================*
      *                        WIPRO INFOOSERVER                       *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : GFCT2484                                     *
      *    PROGRAMADOR  : VINICIUS                                     *
      *    ANALISTA     : VINICIUS                                     *
      *    DATA         : FEV-2018                                     *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      IDENTIFICA EVENTOS NEXT DOS FUNCIONARIOS QUE POSSUEM      *
      *      ISENCAO PERMANENTE.                                       *
      *                                                                *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                      INCLUDE/BOOK        *
      *                    VERFRANQ                  I#GFCTVE          *
      *                    SARHFCPF                                    *
      *                    FRANQT92                  I#GFCTVA          *
      *                    FRANQT41                  I#GFCTV4          *
      *                    VERCAREN                  I#GFCTVE          *
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
      *    POOL7600 - OBTER DATA/HORA ATUAL DO SISTEMA.                *
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

           SELECT VERFRANQ ASSIGN      TO UT-S-VERFRANQ
           FILE STATUS                 IS WRK-FS-VERFRANQ.

           SELECT SARHFCPF ASSIGN      TO UT-S-SARHFCPF
           FILE STATUS                 IS WRK-FS-SARHFCPF.

           SELECT FRANQT92 ASSIGN      TO UT-S-FRANQT92
           FILE STATUS                 IS WRK-FS-FRANQT92.

           SELECT FRANQT41 ASSIGN      TO UT-S-FRANQT41
           FILE STATUS                 IS WRK-FS-FRANQT41.

           SELECT VERCAREN ASSIGN      TO UT-S-VERCAREN
           FILE STATUS                 IS WRK-FS-VERCAREN.

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

       FD  VERFRANQ
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTVE'.

      *----------------------------------------------------------------*
      *    INPUT : ARQUIVO COM CPF DOS FUNCIONARIOS - SARH             *
      *            ORG. SEQUENCIAL     -   LRECL = 025                 *
      *----------------------------------------------------------------*

       FD  SARHFCPF
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-SARHFCPF-REG.
           05  FD-COD-FUNC             PIC 9(009) COMP-3.
           05  FD-SITUACAO             PIC X(001).
           05  FD-CGC-CPF              PIC X(015).

      *----------------------------------------------------------------*
      *    OUTPUT: FRANQUEADOS PARA GRAVAR NA TABELA GFCTB092.         *
      *            ORG. SEQUENCIAL     -   LRECL = 132                 *
      *----------------------------------------------------------------*

       FD  FRANQT92
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTVA'.

      *----------------------------------------------------------------*
      *    OUTPUT: FRANQUEADOS PARA GRAVAR NA TABELA GFCTB041.         *
      *            ORG. SEQUENCIAL     -   LRECL = 150                 *
      *----------------------------------------------------------------*

       FD  FRANQT41
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTV4'.

      *----------------------------------------------------------------*
      *    OUTPUT: SEGUEM PARA VERIFICAR CARENCIA.                     *
      *            ORG. SEQUENCIAL     -   LRECL = 300                 *
      *----------------------------------------------------------------*

       FD  VERCAREN
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

       77  ACU-LIDOS-VERFRANQ          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-LIDOS-SARHFCPF          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-FRANQT92          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-FRANQT41          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-VERCAREN          PIC  9(009) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA DE TESTES DE FILE STATUS *'.
      *----------------------------------------------------------------*

       77  WRK-FS-VERFRANQ             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-SARHFCPF             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-FRANQT92             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-FRANQT41             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-VERCAREN             PIC  X(002)         VALUE SPACES.

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

ST25X6*  01 WRK-CHAVE-VERFRANQ.
ST25X6*     10 WRK-COB-CPF-CNPJ          PIC  9(009)        VALUE ZEROS.
ST25X6*     10 WRK-COB-FILIAL            PIC  9(005)        VALUE ZEROS.
ST25X6*     10 WRK-COB-CONTROLE          PIC  9(002)        VALUE ZEROS.
ST25X6   01 WRK-CHAVE-VERFRANQ.
ST25X6      10 WRK-COB-CPF-CNPJ         PIC  X(009)        VALUE SPACES.
ST25X6      10 WRK-COB-FILIAL           PIC  X(004)        VALUE SPACES.
ST25X6      10 WRK-COB-CONTROLE         PIC  9(002)        VALUE ZEROS.
ST25X6      10 FILLER                   PIC  X(001)        VALUE SPACES.

ST25X6*  01 WRK-CHAVE-SARHFCPF.
ST25X6*     10 WRK-FUN-CPF-CNPJ          PIC  9(009)        VALUE ZEROS.
ST25X6*     10 WRK-FUN-FILIAL            PIC  9(005)        VALUE ZEROS.
ST25X6*     10 WRK-FUN-CONTROLE          PIC  9(002)        VALUE ZEROS.
ST25X6   01 WRK-CHAVE-SARHFCPF.
ST25X6      10 WRK-FUN-CPF-CNPJ         PIC  X(009)        VALUE SPACES.
ST25X6      10 WRK-FUN-FILIAL           PIC  X(004)        VALUE SPACES.
ST25X6      10 WRK-FUN-CONTROLE         PIC  9(002)        VALUE ZEROS.
ST25X6      10 FILLER                   PIC  X(001)        VALUE SPACES.

       01  WRK-AUXILIARES.
ST25X6*    05  WRK-CPF-CNPJ            PIC S9(009)V        VALUE ZEROS.
ST25X6*    05  WRK-CPF-CNPJ-AUX        REDEFINES WRK-CPF-CNPJ
ST25X6*                                PIC  9(009).
ST25X6*
ST25X6*    05  WRK-FILIAL              PIC S9(005)V        VALUE ZEROS.
ST25X6*    05  WRK-FILIAL-AUX          REDEFINES WRK-FILIAL
ST25X6*                                PIC  9(005).
ST25X6*
ST25X6*    05  WRK-CONTROLE-X.
ST25X6*        07  WRK-CONTROLE-N      PIC  9(002)         VALUE ZEROS.

           05  WRK-CGC-CPF.
ST25X6*        07  WRK-PARTE-1         PIC  9(003)         VALUE ZEROS.
ST25X6*        07  WRK-PONTO-1         PIC  X(001)         VALUE SPACES.
ST25X6*        07  WRK-PARTE-2         PIC  9(003)         VALUE ZEROS.
ST25X6*        07  WRK-PONTO-2         PIC  X(001)         VALUE SPACES.
ST25X6*        07  WRK-PARTE-3         PIC  9(003)         VALUE ZEROS.
ST25X6*        07  WRK-TRACO-3         PIC  X(001)         VALUE SPACES.
ST25X6*        07  WRK-PARTE-CTRL      PIC  9(002)         VALUE ZEROS.
ST25X6*        07  WRK-BRANCO          PIC  X(001)         VALUE SPACES.
ST25X6         07  WRK-PARTE-1         PIC  X(003)         VALUE SPACES.
ST25X6         07  WRK-PONTO-1         PIC  X(001)         VALUE SPACES.
ST25X6         07  WRK-PARTE-2         PIC  X(003)         VALUE SPACES.
ST25X6         07  WRK-PONTO-2         PIC  X(001)         VALUE SPACES.
ST25X6         07  WRK-PARTE-3         PIC  X(003)         VALUE SPACES.
ST25X6         07  WRK-TRACO-3         PIC  X(001)         VALUE SPACES.
ST25X6         07  WRK-PARTE-CTRL      PIC  9(002)         VALUE ZEROS.
ST25X6         07  WRK-BRANCO          PIC  X(001)         VALUE SPACES.

ST25X6     05  WRK-CPF-CNPJ-N          PIC  X(009)         VALUE SPACES.
ST25X6     05  WRK-CPF-CNPJ-X          REDEFINES WRK-CPF-CNPJ-N.
ST25X6         07  WRK-CORPO-1         PIC  X(003).
ST25X6         07  WRK-CORPO-2         PIC  X(003).
ST25X6         07  WRK-CORPO-3         PIC  X(003).

       01  WRK-TIMESTAMP-AUX.
           05 WRK-ANO-TMS              PIC  9(004)         VALUE ZEROS.
           05 FILLER                   PIC  X(001)         VALUE '-'.
           05 WRK-MES-TMS              PIC  9(002)         VALUE ZEROS.
           05 FILLER                   PIC  X(001)         VALUE '-'.
           05 WRK-DIA-TMS              PIC  9(002)         VALUE ZEROS.
           05 FILLER                   PIC  X(001)         VALUE '-'.
           05 WRK-HOR-TMS              PIC  9(002)         VALUE ZEROS.
           05 FILLER                   PIC  X(001)         VALUE '.'.
           05 WRK-MIN-TMS              PIC  9(002)         VALUE ZEROS.
           05 FILLER                   PIC  X(001)         VALUE '.'.
           05 WRK-SEG-TMS              PIC  9(002)         VALUE ZEROS.
           05 FILLER                   PIC  X(001)         VALUE '.'.
           05 WRK-MIL-TMS              PIC  9(006)         VALUE ZEROS.

RU0125 01 WRK-CPTCAO-GFCTB041          PIC  X(010) VALUE SPACES.
RU0125 01 FILLER                       REDEFINES WRK-CPTCAO-GFCTB041.
           05 WRK-CPTCAO-B041          PIC 9(002).
RU0125     05 FILLER                   PIC X(008).
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA DA POOL7600'.
      *----------------------------------------------------------------*

       01  WRK-DATA-HORA.
           05 WRK-DATA-JULIANA         PIC  9(005) COMP-3  VALUE ZEROS.
           05 WRK-DATA-AAMMDD          PIC  9(007) COMP-3  VALUE ZEROS.
           05 WRK-DATA-AAAAMMDD        PIC  9(009) COMP-3  VALUE ZEROS.
           05 WRK-HORA-HHMMSS          PIC  9(007) COMP-3  VALUE ZEROS.
           05 WRK-HORA-HHMMSSMMMMMM    PIC  9(013) COMP-3  VALUE ZEROS.
           05 WRK-TIMESTAMP.
             10 WRK-ANO                PIC  9(004)         VALUE ZEROS.
             10 WRK-MES                PIC  9(002)         VALUE ZEROS.
             10 WRK-DIA                PIC  9(002)         VALUE ZEROS.
             10 WRK-HOR                PIC  9(002)         VALUE ZEROS.
             10 WRK-MIN                PIC  9(002)         VALUE ZEROS.
             10 WRK-SEG                PIC  9(002)         VALUE ZEROS.
             10 WRK-MIL                PIC  9(006)         VALUE ZEROS.

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
                   WRK-FS-VERFRANQ     EQUAL '10'.

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

           OPEN INPUT  VERFRANQ
                       SARHFCPF
                OUTPUT FRANQT92
                       FRANQT41
                       VERCAREN.

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

           PERFORM 1210-TESTAR-FS-VERFRANQ.

           PERFORM 1220-TESTAR-FS-SARHFCPF.

           PERFORM 1230-TESTAR-FS-FRANQT92.

           PERFORM 1240-TESTAR-FS-FRANQT41.

           PERFORM 1250-TESTAR-FS-VERCAREN.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTE DE FILE STATUS DO ARQUIVO VERFRANQ.                   *
      ******************************************************************
      *----------------------------------------------------------------*
       1210-TESTAR-FS-VERFRANQ         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-VERFRANQ         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-VERFRANQ    TO WRK-FILE-STATUS
               MOVE 'VERFRANQ'         TO WRK-NOME-ARQ
               MOVE WRK-ERRO-TEXTO     TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTE DE FILE STATUS DO ARQUIVO SARHFCPF.                   *
      ******************************************************************
      *----------------------------------------------------------------*
       1220-TESTAR-FS-SARHFCPF         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SARHFCPF         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-SARHFCPF    TO WRK-FILE-STATUS
               MOVE 'SARHFCPF'         TO WRK-NOME-ARQ
               MOVE WRK-ERRO-TEXTO     TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTE DE FILE STATUS DO ARQUIVO FRANQT92.                   *
      ******************************************************************
      *----------------------------------------------------------------*
       1230-TESTAR-FS-FRANQT92         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-FRANQT92         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-FRANQT92    TO WRK-FILE-STATUS
               MOVE 'FRANQT92'         TO WRK-NOME-ARQ
               MOVE WRK-ERRO-TEXTO     TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTE DE FILE STATUS DO ARQUIVO FRANQT41.                   *
      ******************************************************************
      *----------------------------------------------------------------*
       1240-TESTAR-FS-FRANQT41         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-FRANQT41         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-FRANQT41    TO WRK-FILE-STATUS
               MOVE 'FRANQT41'         TO WRK-NOME-ARQ
               MOVE WRK-ERRO-TEXTO     TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1240-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTE DE FILE STATUS DO ARQUIVO VERCAREN.                   *
      ******************************************************************
      *----------------------------------------------------------------*
       1250-TESTAR-FS-VERCAREN         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-VERCAREN         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-VERCAREN    TO WRK-FILE-STATUS
               MOVE 'VERCAREN'         TO WRK-NOME-ARQ
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

           PERFORM 2100-LER-VERFRANQ.

           IF WRK-FS-VERFRANQ          EQUAL '10'
              DISPLAY '******************** GFCT2484 ******************'
              DISPLAY '*                                              *'
              DISPLAY '*              PROGRAMA GFCT2484               *'
              DISPLAY '*              -----------------               *'
              DISPLAY '*                                              *'
              DISPLAY '*          ARQUIVO DE VERFRANQ VAZIO           *'
              DISPLAY '*                                              *'
              DISPLAY '******************** GFCT2484 ******************'
           END-IF.

           PERFORM 2200-LER-SARHFCPF.

           IF WRK-FS-SARHFCPF          EQUAL '10'
              DISPLAY '******************** GFCT2484 ******************'
              DISPLAY '*                                              *'
              DISPLAY '*              PROGRAMA GFCT2484               *'
              DISPLAY '*              -----------------               *'
              DISPLAY '*                                              *'
              DISPLAY '*             ARQUIVO DE SARHFCPF              *'
              DISPLAY '*           COM DADOS DE FUNCIONARIOS          *'
              DISPLAY '*            BRADESCO ESTA VAZIO!!             *'
              DISPLAY '*                                              *'
              DISPLAY '******************** GFCT2484 ******************'
              MOVE 'APL'               TO ERR-TIPO-ACESSO
              MOVE WRK-FS-SARHFCPF     TO WRK-FILE-STATUS
              MOVE 'SARHFCPF'          TO WRK-NOME-ARQ
              MOVE WRK-ERRO-TEXTO      TO ERR-TEXTO
              PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    LEITURA DO ARQUIVO VERFRANQ                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-LER-VERFRANQ               SECTION.
      *----------------------------------------------------------------*

           READ VERFRANQ.

           IF  WRK-FS-VERFRANQ         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHAVE-VERFRANQ
               GO TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1210-TESTAR-FS-VERFRANQ.

           ADD 1                       TO ACU-LIDOS-VERFRANQ.

ST25X6*   XMOVE VE-CPF-CNPJ OF VERFRANQ
ST25X6*                                TO  WRK-CPF-CNPJ.
ST25X6*   XMOVE WRK-CPF-CNPJ-AUX       TO  WRK-COB-CPF-CNPJ.
ST25X6     MOVE VE-CPF-CNPJ-ST         OF VERFRANQ
ST25X6                                 TO  WRK-COB-CPF-CNPJ.

ST25X6*   XMOVE VE-FILIAL OF VERFRANQ  TO  WRK-FILIAL.
ST25X6*   XMOVE WRK-FILIAL-AUX         TO  WRK-COB-FILIAL.
ST25X6     MOVE VE-FILIAL-ST           OF VERFRANQ  
ST25X6                                 TO  WRK-COB-FILIAL.

ST25X6*   XMOVE VE-CONTROLE            OF VERFRANQ
ST25X6*                                TO  WRK-CONTROLE-X.
ST25X6*   XMOVE WRK-CONTROLE-N         TO  WRK-COB-CONTROLE.
ST25X6     MOVE VE-CONTROLE-ST         OF VERFRANQ
                                       TO  WRK-COB-CONTROLE.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    LEITURA DO ARQUIVO SARHFCPF                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-LER-SARHFCPF               SECTION.
      *----------------------------------------------------------------*

           READ SARHFCPF.

           IF  WRK-FS-SARHFCPF         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHAVE-SARHFCPF
               GO TO 2200-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1220-TESTAR-FS-SARHFCPF.

           ADD 1                       TO ACU-LIDOS-SARHFCPF.

           MOVE FD-CGC-CPF             TO  WRK-CGC-CPF.

           IF  WRK-PARTE-1             NOT NUMERIC
               MOVE ZEROS              TO  WRK-PARTE-1
           END-IF.
           IF  WRK-PARTE-2             NOT NUMERIC
               MOVE ZEROS              TO  WRK-PARTE-2
           END-IF.
           IF  WRK-PARTE-3             NOT NUMERIC
               MOVE ZEROS              TO  WRK-PARTE-3
           END-IF.
           IF  WRK-PARTE-CTRL          NOT NUMERIC
               MOVE ZEROS              TO  WRK-PARTE-CTRL
           END-IF.

           MOVE WRK-PARTE-1            TO  WRK-CORPO-1.
           MOVE WRK-PARTE-2            TO  WRK-CORPO-2.
           MOVE WRK-PARTE-3            TO  WRK-CORPO-3.

           IF  WRK-CPF-CNPJ-N          EQUAL  ZEROS
               GO TO 2200-LER-SARHFCPF
           END-IF.

           MOVE WRK-CPF-CNPJ-N         TO  WRK-FUN-CPF-CNPJ.

           MOVE ZEROS                  TO  WRK-FUN-FILIAL.

           MOVE WRK-PARTE-CTRL         TO  WRK-FUN-CONTROLE.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCESSAMENTO PRINCIPAL.                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           IF  WRK-CHAVE-VERFRANQ      EQUAL WRK-CHAVE-SARHFCPF
                 PERFORM 3300-GRAVAR-FRANQUEADOS
                 PERFORM 2100-LER-VERFRANQ
           ELSE
               IF  WRK-CHAVE-VERFRANQ  GREATER WRK-CHAVE-SARHFCPF
                   PERFORM 2200-LER-SARHFCPF
               ELSE
                   PERFORM 3400-GRAVAR-VERCAREN
                   PERFORM 2100-LER-VERFRANQ
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    GRAVAR OS ARQUIVOS FRANQT92 E FRANQT41.                     *
      ******************************************************************
      *----------------------------------------------------------------*
       3300-GRAVAR-FRANQUEADOS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 3310-OBTER-TIMESTAMP.

           MOVE WRK-GRAVACAO            TO WRK-OPERACAO.

           INITIALIZE                      GFCTVA-GFCTB092.
           MOVE VE-GFCTB092 OF VERFRANQ TO GFCTVA-GFCTB092.
           MOVE ZEROS                   TO GFCTVA-VTARIF-LIQ-MOVTO
                                           GFCTVA-VTARIF-DEB-MOVTO.
           MOVE VE-DAGNDA-DEB-MOVTO     OF VERFRANQ
                                        TO GFCTVA-DEFETV-DEB-MOVTO.
           MOVE VE-CTRATO-MOVTO         OF VERFRANQ
                                        TO GFCTVA-CTRATO-MOVTO.
           MOVE 16                      TO GFCTVA-CSIT-EVNTO-RECBD.

           WRITE GFCTVA-GFCTB092.

           PERFORM 1230-TESTAR-FS-FRANQT92.

           ADD 1                       TO ACU-GRAVA-FRANQT92.


           MOVE 4                      TO GFCTV4-CIDTFD-TPO-DESC.
           MOVE 6                      TO GFCTV4-CIDTFD-MOTVO-DESC.
           MOVE ZEROS                  TO GFCTV4-CIDTFD-DESC.
           MOVE 3                      TO GFCTV4-CPARM-DESC.
           MOVE VE-QEVNTO-AGRUP        OF VERFRANQ
                                       TO GFCTV4-QEVNTO-DESC.
           MOVE VE-VTARIF-LIQ-MOVTO    OF VERFRANQ
                                       TO GFCTV4-VCONCS-DESC.
           MOVE SPACES                 TO GFCTV4-CCONTR-FLEXZ.
           MOVE VE-DENVIO-MOVTO-TARIF  OF VERFRANQ
                                       TO GFCTV4-DENVIO-MOVTO-TARIF.
RU0125     MOVE  GFCTV4-DENVIO-MOVTO-TARIF
RU0125                                 TO WRK-CPTCAO-GFCTB041.
RU0125     MOVE  WRK-CPTCAO-B041       TO GFCTV4-CPTCAO.
           MOVE VE-CROTNA-ORIGE-MOVTO  OF VERFRANQ
                                       TO GFCTV4-CROTNA-ORIGE-MOVTO.
           MOVE VE-CNRO-ARQ-MOVTO      OF VERFRANQ
                                       TO GFCTV4-CNRO-ARQ-MOVTO.
           MOVE VE-CSEQ-MOVTO          OF VERFRANQ
                                       TO GFCTV4-CSEQ-MOVTO.
           MOVE VE-CBCO-DSTNO-MOVTO    OF VERFRANQ
                                       TO GFCTV4-CBCO-DSTNO-MOVTO.
           MOVE VE-CAG-DSTNO-MOVTO     OF VERFRANQ
                                       TO GFCTV4-CAG-DSTNO-MOVTO.
           MOVE VE-CCTA-DSTNO-MOVTO    OF VERFRANQ
                                       TO GFCTV4-CCTA-DSTNO-MOVTO.
           MOVE VE-CPAB-DSTNO-MOVTO    OF VERFRANQ
                                       TO GFCTV4-CPAB-DSTNO-MOVTO.
           MOVE VE-CSERVC-TARIF        OF VERFRANQ
                                       TO GFCTV4-CSERVC-TARIF.
           MOVE VE-DOCOR-EVNTO         OF VERFRANQ
                                       TO GFCTV4-DOCOR-EVNTO.
           MOVE VE-HPREST-SERVC-MOVTO  OF VERFRANQ
                                       TO GFCTV4-HPREST-MOVTO.
           MOVE WRK-TIMESTAMP-AUX      TO GFCTV4-IDTFD-TIMESTAMP.

           WRITE GFCTV4-GFCTB041.

           PERFORM 1240-TESTAR-FS-FRANQT41.

           ADD 1                       TO ACU-GRAVA-FRANQT41.

      *----------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    OBTER O TIMESTAMP ATUAL DO SISTEMA PARA GRAVACAO DO ARQUIVO.*
      ******************************************************************
      *----------------------------------------------------------------*
       3310-OBTER-TIMESTAMP            SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL7600'             USING WRK-DATA-HORA.

           MOVE WRK-ANO                TO WRK-ANO-TMS.
           MOVE WRK-MES                TO WRK-MES-TMS.
           MOVE WRK-DIA                TO WRK-DIA-TMS.
           MOVE WRK-HOR                TO WRK-HOR-TMS.
           MOVE WRK-MIN                TO WRK-MIN-TMS.
           MOVE WRK-SEG                TO WRK-SEG-TMS.
           MOVE WRK-MIL                TO WRK-MIL-TMS.

      *----------------------------------------------------------------*
       3310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    GRAVAR ARQUIVO VERCAREN                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       3400-GRAVAR-VERCAREN            SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO            TO WRK-OPERACAO.
           MOVE REG-IGFCTVE OF VERFRANQ TO REG-IGFCTVE OF VERCAREN.

           WRITE REG-IGFCTVE            OF VERCAREN.

           PERFORM 1250-TESTAR-FS-VERCAREN.

           ADD 1                        TO ACU-GRAVA-VERCAREN.

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

           CLOSE VERFRANQ
                 SARHFCPF
                 FRANQT92
                 FRANQT41
                 VERCAREN.

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

           MOVE ACU-LIDOS-VERFRANQ     TO WRK-MASCARA.

           DISPLAY '******************** GFCT2484 ******************'.
           DISPLAY '*                                              *'.
           DISPLAY '*              PROGRAMA GFCT2484               *'.
           DISPLAY '*              -----------------               *'.
           DISPLAY '*                                              *'.
           DISPLAY '*  LIDOS NO ARQUIVO VERFRANQ...: ' WRK-MASCARA '  *'
           DISPLAY '*                                              *'.

           MOVE ACU-LIDOS-SARHFCPF     TO WRK-MASCARA.

           DISPLAY '*  LIDOS NO ARQUIVO SARHFCPF...: ' WRK-MASCARA '  *'
           DISPLAY '*                                              *'.

           MOVE ACU-GRAVA-FRANQT92     TO WRK-MASCARA.

           DISPLAY '*  GRAVADOS NO ARQUIVO FRANQT92: ' WRK-MASCARA '  *'
           DISPLAY '*                                              *'.

           MOVE ACU-GRAVA-FRANQT41     TO WRK-MASCARA.

           DISPLAY '*  GRAVADOS NO ARQUIVO FRANQT41: ' WRK-MASCARA '  *'
           DISPLAY '*                                              *'.

           MOVE ACU-GRAVA-VERCAREN     TO WRK-MASCARA.

           DISPLAY '*  GRAVADOS NO ARQUIVO VERCAREN: ' WRK-MASCARA '  *'
           DISPLAY '*                                              *'.
           DISPLAY '******************** GFCT2484 ******************'.

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    EXECUTA PROCEDIMENTO DE CANCELAMENTO DO PGM EM CASO DE ERRO *
      ******************************************************************
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT2484'             TO ERR-PGM.

           CALL 'POOL7100'             USING WRK-BATCH
                                             ERRO-AREA.
           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

