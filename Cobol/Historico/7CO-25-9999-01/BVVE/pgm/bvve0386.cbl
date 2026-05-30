      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID.     BVVE0386.
       AUTHOR.         FRANCISMARA PAGNONCELLI.

      *================================================================*
      *                   B S I   T E C N O L O G I A                  *
      *----------------------------------------------------------------*
      *    ANALISTA....: FRANCISMARA PAGNONCELI - BSI TECNOLOGIA       *
      *    DATA........: OUTUBRO/2017                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....: CARREGA ARQUIVO DE PERFIS PARA  PRFOR         *
      *                  COPIA DO BVVE0209                             *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                      INCLUDE/BOOK        *
      *               SYSCNPJ  -  INPUT              SYSIN JOB         *
      *               SYSNOME  -  INPUT              SYSIN JOB         *
      *---------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    I#BRAD7C -  AREA PARA TRATAMENTO DE ERRO PELA BRAD7100      *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    BRAD7100 -  TRATAMENTO DE ERROS QUANDO PROGRAMA INVALIDO    *
      *    BRAD7600 -  TRATAMENTO DE DATA/HORA CORRENTE                *
      *================================================================*

      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*

       FILE-CONTROL.

           SELECT SYSCNPJ ASSIGN       TO UT-S-SYSCNPJ
                      FILE STATUS      IS WRK-FS-SYSCNPJ.

           SELECT SYSNOME ASSIGN       TO UT-S-SYSNOME
                      FILE STATUS      IS WRK-FS-SYSNOME.

           SELECT CADPERF ASSIGN       TO UT-S-CADPERF
                      FILE STATUS      IS WRK-FS-CADPERF.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.

      *----------------------------------------------------------------*
      *    INPUT:  DADOS DO PERFIL                                     *
      *            SYSIN DO JOB        -   LRECL   =   080             *
      *----------------------------------------------------------------*
       FD  SYSCNPJ
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-SYSCNPJ.
           03  FD-PERF-PERFIL         PIC 9(008).
7C2511*    03  FD-PERF-CNPJ           PIC 9(008).
7C2511    03  FD-PERF-CNPJ           PIC X(008).
7C2511*    03  FD-PERF-FILIAL         PIC 9(004).
7C2511    03  FD-PERF-FILIAL         PIC X(004).
           03  FD-PERF-CTRL           PIC 9(002).
           03  FD-PERF-AGENCIA        PIC 9(005).
           03  FD-PERF-CONTA          PIC 9(007).
           03  FD-PERF-AGEN-COMPL     PIC 9(004).
           03  FD-PERF-CONT-COMPL     PIC 9(007).
           03  FD-PERF-CONS-SALDO     PIC X(001).
           03  FILLER                 PIC X(033).

      *----------------------------------------------------------------*
      *    INPUT : NOME PERFIL                                         *
      *            SYSIN DO JOB        -   LRECL   =   080             *
      *----------------------------------------------------------------*
       FD  SYSNOME
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-SYSNOME                 PIC  X(080).

      *----------------------------------------------------------------*
      *    OUTPUT:  CADASTRO DO PERFIL                                 *
      *----------------------------------------------------------------*
       FD  CADPERF
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01  FD-CADPERF                 PIC  X(150).

      *================================================================*
       WORKING-STORAGE                 SECTION.
      *================================================================*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)          VALUE
           '*** BVVE0386 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** ACUMULADORES ***'.
      *----------------------------------------------------------------*
       77  ACU-GRA-CADPERF             PIC  9(010)   COMP-3 VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREAS AUXILIARES ***'.
       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.
       77  WRK-MASCARA                 PIC  Z.ZZZ.ZZZ.ZZ9  VALUE ZEROS.
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREAS DE FILE-STATUS ***'.
      *----------------------------------------------------------------*
       77  WRK-FS-SYSCNPJ             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-CADPERF             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-SYSNOME             PIC  X(002)         VALUE SPACES.

       77  WRK-ABERTURA                PIC  X(013)         VALUE
           ' NA ABERTURA '.

       77  WRK-LEITURA                 PIC  X(013)         VALUE
           ' NA  LEITURA '.

       77  WRK-GRAVACAO                PIC  X(013)         VALUE
           ' NA GRAVACAO '.

           'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(50)         VALUE
           '*  LAYOUT ARQUIVO CADPERF   *'.
      *----------------------------------------------------------------*

       01  WRK-REG-CADPERF.
           03  WRK-PERF-PERFIL             PIC 9(008)  VALUE ZEROS.
7C2511*    03  WRK-PERF-CNPJ               PIC 9(008)  VALUE ZEROS.
7C2511     03  WRK-PERF-CNPJ               PIC X(008)  VALUE ZEROS.
7C2511*    03  WRK-PERF-FILIAL             PIC 9(004)  VALUE ZEROS.
7C2511     03  WRK-PERF-FILIAL             PIC X(004)  VALUE ZEROS.
           03  WRK-PERF-CTRL               PIC 9(002)  VALUE ZEROS.
           03  WRK-PERF-AGENCIA            PIC 9(005)  VALUE ZEROS.
           03  WRK-PERF-CONTA              PIC 9(007)  VALUE ZEROS.
           03  WRK-PERF-AGEN-COMPL         PIC 9(004)  VALUE ZEROS.
           03  WRK-PERF-CONT-COMPL         PIC 9(007)  VALUE ZEROS.
           03  WRK-PERF-CON-SALDO          PIC X(001)  VALUE SPACES.
           03  WRK-PERF-NRO-PER            PIC X(001)  VALUE SPACES.
           03  WRK-PERF-NOME               PIC X(040)  VALUE SPACES.

       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ERRO - FILE-STATUS ***'.
      *----------------------------------------------------------------*
       01  WRK-ERRO-ARQUIVO.
           03  FILLER                  PIC  X(008)         VALUE
               '** ERRO '.
           03  WRK-OPERACAO            PIC  X(013)         VALUE SPACES.
           03  FILLER                  PIC  X(012)         VALUE
               ' DO ARQUIVO '.
           03  WRK-NOME-ARQUIVO        PIC  X(008)         VALUE SPACES.
           03  FILLER                  PIC  X(018)         VALUE
               ' - FILE STATUS =  '.
           03  WRK-FILE-STATUS         PIC  X(002)         VALUE SPACES.
           03  FILLER                  PIC  X(003)         VALUE ' **'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(025)         VALUE
           '*** AREA DA BRAD7100 ***'.
      *----------------------------------------------------------------*
       COPY 'I#BRAD7C'.
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** BVVE0386 - FIM DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE  DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-VERIFICAR-VAZIO.

           PERFORM 3000-PROCESSAR
             UNTIL WRK-FS-SYSCNPJ     EQUAL '10' AND
                   WRK-FS-SYSNOME     EQUAL '10'.
           PERFORM 4000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-INICIALIZAR              SECTION.
      *----------------------------------------------------------------*

           OPEN    INPUT   SYSCNPJ
                           SYSNOME
                   OUTPUT  CADPERF.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.
           PERFORM 1100-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1120-TESTAR-FS-SYSCNPJ.

           PERFORM 1130-TESTAR-FS-CADPERF.

           PERFORM 1140-TESTAR-FS-SYSNOME.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1120-TESTAR-FS-SYSCNPJ         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SYSCNPJ          NOT EQUAL ZEROS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'SYSCNPJ'          TO WRK-NOME-ARQUIVO
               MOVE  WRK-ERRO-ARQUIVO  TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1130-TESTAR-FS-CADPERF         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-CADPERF          NOT EQUAL ZEROS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE  WRK-FS-CADPERF    TO WRK-FILE-STATUS
               MOVE 'CADPERF'          TO WRK-NOME-ARQUIVO
               MOVE  WRK-ERRO-ARQUIVO  TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.
      *----------------------------------------------------------------*
       1130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1140-TESTAR-FS-SYSNOME         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SYSNOME          NOT EQUAL ZEROS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE  WRK-FS-SYSNOME    TO WRK-FILE-STATUS
               MOVE 'SYSNOME'          TO WRK-NOME-ARQUIVO
               MOVE  WRK-ERRO-ARQUIVO  TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1140-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

       2000-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2200-LER-SYSCNPJ.

           IF  WRK-FS-SYSCNPJ         EQUAL       '10'
               DISPLAY '******************* BVVE0386 *****************'
               DISPLAY '*                                            *'
               DISPLAY '*           ARQUIVO SYSCNPJ VAZIO           *'
               DISPLAY '*                                            *'
               DISPLAY '******************* BVVE0386 *****************'
           END-IF.

           PERFORM 2300-LER-SYSNOME.

           IF  WRK-FS-SYSNOME         EQUAL '10'
               DISPLAY '******************* BVVE0386 *****************'
               DISPLAY '*                                            *'
               DISPLAY '*           ARQUIVO SYSNOME VAZIO           *'
               DISPLAY '*                                            *'
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2200-LER-SYSCNPJ               SECTION.
      *----------------------------------------------------------------*

           READ SYSCNPJ.

           IF  WRK-FS-SYSCNPJ         EQUAL '10'
               GO TO 2200-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.
           PERFORM 1120-TESTAR-FS-SYSCNPJ.

      *----------------------------------------------------------------*
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2300-LER-SYSNOME               SECTION.
      *----------------------------------------------------------------*

           READ SYSNOME.

           IF  WRK-FS-SYSNOME         EQUAL '10'
               GO TO 2300-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.
           PERFORM 1140-TESTAR-FS-SYSNOME.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *----------------------------------------------------------------*

           PERFORM 3100-GRAVAR-CADPERF.

           PERFORM 2200-LER-SYSCNPJ.
           PERFORM 2300-LER-SYSNOME.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3100-GRAVAR-CADPERF             SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-REG-CADPERF
           MOVE FD-PERF-PERFIL         TO   WRK-PERF-PERFIL
           MOVE FD-PERF-CNPJ           TO   WRK-PERF-CNPJ
           MOVE FD-PERF-FILIAL         TO   WRK-PERF-FILIAL
           MOVE FD-PERF-CTRL           TO   WRK-PERF-CTRL
           MOVE FD-PERF-CONTA          TO   WRK-PERF-CONTA
           MOVE FD-PERF-AGEN-COMPL     TO   WRK-PERF-AGEN-COMPL
           MOVE FD-PERF-CONT-COMPL     TO   WRK-PERF-CONT-COMPL
           MOVE FD-PERF-CONS-SALDO     TO   WRK-PERF-CON-SALDO
           MOVE FD-SYSNOME(1:40)       TO   WRK-PERF-NOME.
           MOVE FD-PERF-NRO-PER        TO   WRK-PERF-NRO-PER

           MOVE WRK-GRAVACAO           TO   WRK-OPERACAO

           WRITE FD-CADPERF            FROM   WRK-REG-CADPERF
           PERFORM 1130-TESTAR-FS-CADPERF
           ADD 1                       TO  ACU-GRA-CADPERF.


      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.

           PERFORM 4200-EMITIR-DISPLAY-FINAL.

           CLOSE SYSCNPJ
                 CADPERF
                 SYSNOME.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.
           PERFORM 1100-TESTAR-FILE-STATUS.

           STOP RUN.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4200-EMITIR-DISPLAY-FINAL       SECTION.
      *----------------------------------------------------------------*

           DISPLAY '*                                               *'.
           DISPLAY '*          ESTATISTICA DE PROCESSAMENTO         *'.
           DISPLAY '*                                               *'.
           DISPLAY '*-----------------------------------------------*'.
           DISPLAY '*                                               *'.

           MOVE ACU-GRA-CADPERF       TO WRK-MASCARA.
           DISPLAY '* REGISTROS GRAVADOS CADPERF:    ' WRK-MASCARA
                                                                  ' *'.
           DISPLAY '*                                               *'.
           DISPLAY '******************* BVVE0386 ********************'.


      *----------------------------------------------------------------*
       4200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*
           MOVE 'BVVE0386'             TO ERR-PGM.

           CALL 'BRAD7100'             USING WRK-BATCH
                                             ERRO-AREA

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
