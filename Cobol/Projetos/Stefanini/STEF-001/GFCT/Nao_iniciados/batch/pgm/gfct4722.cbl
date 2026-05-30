      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT4722.
       AUTHOR.     HOMI - SONDA.

      *================================================================*
      *.                SONDA                                          *
      *----------------------------------------------------------------*
      *    PROGRAMA....:  GFCT4722                                     *
      *    SISTEMA.....:  GFCT - GFCT                                  *
      *    PROGRAMADOR.:  HOMI - SONDA                                 *
      *    ANALISTA....:  ANTONIO PAGNOCCA - SONDA                     *
      *    GRUPO.......:  50                                           *
      *    DATA........:  DEZ 2015                                     *
      *----------------------------------------------------------------*
      *    OBJETIVO....:  INVESTIMENTOS ULTIMOS 03 MESES               *
      *                   BALANCE LINE ENTRE ARQUIVO CONTENDO AGENCIA, *
      *                   CONTA E TARIFAS A SEREM BONIFICADAS          *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME           I/O        INCLUDE/BOOK        *
      *                GFCTEHP0          I         GFCTWF25            *
      *                GFCTEF30          I         GFCTWF30            *
      *                GFCTS0P0          O         GFCTWF25            *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    GFCTEHP0-GFCTWF25 - ENTRADA IMAGEM GFCTB0P0 + CNPJ/CPF      *
      *    GFCTEF30-GFCTWF30 - ENTRADA CONTAS A SEREM BONIFICADAS      *
      *    GFCTS0P0-GFCTWF25 - SAIDA   IMAGEM GFCTB0P0 + CNPJ/CPF      *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    POOL7100 - MODULO PARA CANCELAMENTO DO PROGRAMA             *
      *================================================================*

      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS   COMMA.

      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*

       FILE-CONTROL.

           SELECT GFCTEHP0 ASSIGN      TO   UT-S-GFCTEHP0
                      FILE STATUS      IS   WRK-FS-GFCTEHP0.

           SELECT GFCTEF30 ASSIGN      TO   UT-S-GFCTEF30
                      FILE STATUS      IS   WRK-FS-GFCTEF30.

           SELECT GFCTS0P0 ASSIGN      TO   UT-S-GFCTS0P0
                      FILE STATUS      IS   WRK-FS-GFCTS0P0.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *   INPUT:   ARQUIVO DE ENTRADA - IMAGEM GFCTEHP0 - HSBC         *
      *            ORG. SEQUENCIAL    - LRECL   = 98                   *
      *----------------------------------------------------------------*
       FD  GFCTEHP0
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-GFCTEHP0                 PIC  X(98).

      *----------------------------------------------------------------*
      *   INPUT:   ARQUIVO DE ENTRADA - AGENCIA, CONTA P/BONIFICACAO   *
      *            ORG. SEQUENCIAL    - LRECL   = 090                  *
      *----------------------------------------------------------------*
       FD  GFCTEF30
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-GFCTEF30                 PIC  X(090).

      *----------------------------------------------------------------*
      *   OUTPUT:ARQUIVO DE SAIDA    -  ARQUIVO IMAGEM GFCTB0P0        *
      *          ORG. SEQUENCIAL     -   LRECL  = 98                   *
      *----------------------------------------------------------------*
       FD  GFCTS0P0
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-GFCTS0P0                 PIC  X(98).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       77  FILLER                      PIC  X(050)        VALUE
           '*** INICIO DA WORKING-STORAGE SECTION         ****'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       77  WRK-BATCH                   PIC  X(008)        VALUE 'BATCH'.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ACUMULADORES                       ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01  WRK-ACUMULADORES.

           03 WRK-LIDOS-GFCTEHP0       PIC  9(009) COMP-3  VALUE ZEROS.
           03 WRK-LIDOS-GFCTEF30       PIC  9(009) COMP-3  VALUE ZEROS.
           03 WRK-GRAVADOS-GFCTS0P0    PIC  9(009) COMP-3  VALUE ZEROS.
           03 WRK-DISPLAY              PIC +Z(008)9        VALUE ZEROS.

       01  WRK-CAG-BCRIA-CS            PIC +9(005)         VALUE ZEROS.
       01  REDEFINES WRK-CAG-BCRIA-CS.
           03                          PIC X.
           03 WRK-CAG-BCRIA-SS         PIC  9(005).

       01  WRK-CCTA-CLI-CS             PIC +9(007)         VALUE ZEROS.
       01  REDEFINES WRK-CCTA-CLI-CS.
           03                          PIC X.
           03  WRK-CCTA-CLI-SS         PIC  9(007).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE TESTE DE FILE-STATUS               ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01  WRK-FS-GFCTEHP0             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-GFCTEF30             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-GFCTS0P0             PIC  X(002)         VALUE SPACES.

       01  WRK-ABERTURA                PIC  X(013)         VALUE
           ' NA ABERTURA '.
       01  WRK-LEITURA                 PIC  X(013)         VALUE
           ' NA LEITURA  '.
       01  WRK-GRAVACAO                PIC  X(013)         VALUE
           ' NA GRAVACAO '.
       01  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** MENSAGEM DE ERRO DE FILE-STATUS ***'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-POOL7100.
           03  FILLER                  PIC  X(009)         VALUE
               '*** ERRO '.
           03  WRK-OPERACAO            PIC  X(013)         VALUE SPACES.
           03  FILLER                  PIC  X(012)         VALUE
               ' DO ARQUIVO '.
           03  WRK-NOME-ARQ            PIC  X(008)         VALUE SPACES.
           03  FILLER                  PIC  X(017)         VALUE
               ' - FILE-STATUS = '.
           03  WRK-FILE-STATUS         PIC  X(002)         VALUE SPACES.
           03  FILLER                  PIC  X(004)         VALUE ' ***'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA POOL7100 ***'.
      *----------------------------------------------------------------*

       COPY POL7100C.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE CHAVES                             ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01  WRK-CHAVE-GFCTEHP0.
           03 WRK-KEY1-AGENCIA         PIC S9(05)    VALUE ZEROS.
           03 WRK-KEY1-CONTA           PIC S9(13)    VALUE ZEROS.

       01  WRK-CHAVE-GFCTEHP0-ANT.
           03 WRK-K1-AGENCIA-ANT       PIC S9(05)    VALUE ZEROS.
           03 WRK-K1-CONTA-ANT         PIC S9(13)    VALUE ZEROS.

       01  WRK-CHAVE-GFCTEF30.
           03 WRK-KEY2-AGENCIA         PIC S9(05)    VALUE ZEROS.
           03 WRK-KEY2-CONTA           PIC S9(13)    VALUE ZEROS.

       01  WRK-CHAVE-GFCTEF30-ANT.
           03 WRK-K2-AGENCIA-ANT       PIC S9(05)    VALUE ZEROS.
           03 WRK-K2-CONTA-ANT         PIC S9(13)    VALUE ZEROS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ENTRADA GFCTEHP0                   ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01 WRK-GFCTEHP0-REGISTRO.
       COPY 'GFCTWF25'.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ENTRADA GFCTEF30                   ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       COPY 'GFCTWF30'.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                      PIC  X(050)         VALUE
           '*** FIM DA WORKING-STORAGE SECTION             ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  LNK-PARM.
           03  LNK-PARM-TAMANHO        PIC S9(004) COMP.
           03  LNK-BALANCE             PIC  X(01).

      *================================================================*
       PROCEDURE DIVISION              USING LNK-PARM.
      *================================================================*

      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIAR.

           PERFORM 2000-VERIFICAR-VAZIO.

           PERFORM 3000-PROCESSAR
             UNTIL WRK-CHAVE-GFCTEHP0   EQUAL     HIGH-VALUES
               AND WRK-CHAVE-GFCTEF30   EQUAL     HIGH-VALUES.

           PERFORM 9000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           IF LNK-PARM-TAMANHO         EQUAL ZEROS
              MOVE 'APL'               TO ERR-TIPO-ACESSO
              MOVE 'FALTOU INFORMAR PARAMETRO' TO ERR-TEXTO
              PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF LNK-BALANCE NOT EQUAL 'S' AND 'N'
              MOVE 'APL'               TO ERR-TIPO-ACESSO
              MOVE 'PARAMETRO BALANCE INVALIDO' TO ERR-TEXTO
              PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           OPEN INPUT   GFCTEHP0
                        GFCTEF30
                OUTPUT  GFCTS0P0.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-GFCTEF30.

           PERFORM 1120-TESTAR-FS-GFCTEHP0.

           PERFORM 1130-TESTAR-FS-GFCTS0P0.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1110-TESTAR-FS-GFCTEF30         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-GFCTEF30         NOT EQUAL  '00'
               MOVE 'GFCTEF30'         TO WRK-NOME-ARQ
               MOVE WRK-FS-GFCTEF30    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-POOL7100  TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1120-TESTAR-FS-GFCTEHP0         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-GFCTEHP0         NOT EQUAL  '00'
               MOVE 'GFCTEHP0'         TO WRK-NOME-ARQ
               MOVE WRK-FS-GFCTEHP0    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-POOL7100  TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1130-TESTAR-FS-GFCTS0P0         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-GFCTS0P0         NOT EQUAL  '00'
               MOVE 'GFCTS0P0'         TO WRK-NOME-ARQ
               MOVE WRK-FS-GFCTS0P0    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-POOL7100  TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2200-LER-GFCTEHP0.

           IF (WRK-LIDOS-GFCTEHP0      EQUAL     ZEROS)
               DISPLAY '************ GFCT4722 ************'
               DISPLAY '*                                *'
               DISPLAY '*     ARQUIVO GFCTEHP0 VAZIO     *'
               DISPLAY '*                                *'
               DISPLAY '************ GFCT4722 ************'
           END-IF.

           PERFORM 2100-LER-GFCTEF30.

           IF (WRK-LIDOS-GFCTEF30      EQUAL     ZEROS)
               DISPLAY '************ GFCT4722 ************'
               DISPLAY '*                                *'
               DISPLAY '*     ARQUIVO GFCTEF30 VAZIO     *'
               DISPLAY '*                                *'
               DISPLAY '************ GFCT4722 ************'
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2100-LER-GFCTEF30               SECTION.
      *----------------------------------------------------------------*

           IF LNK-BALANCE EQUAL TO 'S'
              READ GFCTEF30            INTO GFCTWF30-REGISTRO
           ELSE
              IF WRK-FS-GFCTEHP0       EQUAL ZEROS
                 MOVE WF25-CAG-BCRIA   OF WRK-GFCTEHP0-REGISTRO
                                       TO WRK-CAG-BCRIA-CS
                 MOVE WRK-CAG-BCRIA-SS TO GFCTWF30-AGENCIA
                 MOVE WF25-CCTA-CLI    OF WRK-GFCTEHP0-REGISTRO
                                       TO WRK-CCTA-CLI-CS
                 MOVE WRK-CCTA-CLI-SS  TO GFCTWF30-CONTA
               ELSE
                 MOVE WRK-FS-GFCTEHP0  TO WRK-FS-GFCTEF30
              END-IF
           END-IF.

           IF WRK-FS-GFCTEF30          EQUAL     '10'
               MOVE HIGH-VALUES        TO   WRK-CHAVE-GFCTEF30
               GO TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1110-TESTAR-FS-GFCTEF30.
           ADD  1  TO   WRK-LIDOS-GFCTEF30.
           PERFORM 2110-MONTAR-CHAVE-GFCTEF30.

           IF WRK-CHAVE-GFCTEF30 LESS THAN WRK-CHAVE-GFCTEF30-ANT
              DISPLAY '************ GFCT4722 ************'
              DISPLAY '*                                *'
              DISPLAY '*       ARQUIVO GFCTEF30         *'
              DISPLAY '*       NAO CLASSIFICADO         *'
              DISPLAY '*       PROGRAMA ENCERRADO       *'
              DISPLAY '* CHAVE GFCTEF30 ANTERIOR        *'
              MOVE WRK-K2-AGENCIA-ANT  TO WRK-DISPLAY
              DISPLAY '*   CNPJ-CPF-ANT.............= ' WRK-DISPLAY
              DISPLAY '*--------------------------------*'
              DISPLAY '* CHAVE GFCTEF30 LIDA            *'
              MOVE WRK-KEY2-AGENCIA TO WRK-DISPLAY
              DISPLAY '*   CNPJ-CPF.................= ' WRK-DISPLAY
              DISPLAY '************ GFCT4722 ************'
              MOVE 'GFCTEF30'         TO WRK-NOME-ARQ
              MOVE WRK-FS-GFCTEF30    TO WRK-FILE-STATUS
              MOVE WRK-ERRO-POOL7100  TO ERR-TEXTO
              MOVE 'APL'              TO ERR-TIPO-ACESSO
              PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2110-MONTAR-CHAVE-GFCTEF30      SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-CHAVE-GFCTEF30 TO WRK-CHAVE-GFCTEF30-ANT.

           MOVE GFCTWF30-AGENCIA   TO WRK-KEY2-AGENCIA.
           MOVE GFCTWF30-CONTA     TO WRK-KEY2-CONTA.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2200-LER-GFCTEHP0               SECTION.
      *----------------------------------------------------------------*

           READ GFCTEHP0               INTO WRK-GFCTEHP0-REGISTRO.

           IF WRK-FS-GFCTEHP0          EQUAL     '10'
               MOVE HIGH-VALUES        TO   WRK-CHAVE-GFCTEHP0
               GO TO 2200-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1120-TESTAR-FS-GFCTEHP0.
           ADD  1  TO   WRK-LIDOS-GFCTEHP0.
           PERFORM 2210-MONTAR-CHAVE-GFCTEHP0.

           IF WRK-CHAVE-GFCTEHP0 LESS THAN WRK-CHAVE-GFCTEHP0-ANT
              DISPLAY '************ GFCT4722 ************'
              DISPLAY '*                                *'
              DISPLAY '*       ARQUIVO GFCTEHP0         *'
              DISPLAY '*       NAO CLASSIFICADO         *'
              DISPLAY '*       PROGRAMA ENCERRADO       *'
              DISPLAY '* CHAVE GFCTEHP0 ANTERIOR        *'
              MOVE WRK-K1-AGENCIA-ANT  TO WRK-DISPLAY
              DISPLAY '*   AGENCIA-ANT..............= ' WRK-DISPLAY
              DISPLAY '*--------------------------------*'
              DISPLAY '* CHAVE GFCTEHP0 LIDA            *'
              MOVE WRK-KEY1-AGENCIA TO WRK-DISPLAY
              DISPLAY '*   AGENCIA..................= ' WRK-DISPLAY
              DISPLAY '************ GFCT4722 ************'
              MOVE 'GFCTEHP0'         TO WRK-NOME-ARQ
              MOVE WRK-FS-GFCTEHP0    TO WRK-FILE-STATUS
              MOVE WRK-ERRO-POOL7100  TO ERR-TEXTO
              MOVE 'APL'              TO ERR-TIPO-ACESSO
              PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2210-MONTAR-CHAVE-GFCTEHP0      SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-CHAVE-GFCTEHP0 TO WRK-CHAVE-GFCTEHP0-ANT.

           MOVE WF25-CAG-BCRIA     OF WRK-GFCTEHP0-REGISTRO
                                   TO WRK-KEY1-AGENCIA.
           MOVE WF25-CCTA-CLI      OF WRK-GFCTEHP0-REGISTRO
                                   TO WRK-KEY1-CONTA.

      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           IF      WRK-CHAVE-GFCTEHP0  LESS THAN WRK-CHAVE-GFCTEF30
                   PERFORM 3100-GFCTEHP0-MENOR-Q-GFCTEF30
           ELSE
             IF    WRK-CHAVE-GFCTEHP0  EQUAL TO  WRK-CHAVE-GFCTEF30
                   PERFORM 3200-GFCTEHP0-IGUAL-A-GFCTEF30
               ELSE
                   PERFORM 3300-GFCTEHP0-MAIOR-Q-GFCTEF30
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3100-GFCTEHP0-MENOR-Q-GFCTEF30  SECTION.
      *----------------------------------------------------------------*

           PERFORM 2200-LER-GFCTEHP0.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3200-GFCTEHP0-IGUAL-A-GFCTEF30  SECTION.
      *----------------------------------------------------------------*

           PERFORM 3500-GRAVAR-GFCTS0P0.

           PERFORM 2200-LER-GFCTEHP0.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3300-GFCTEHP0-MAIOR-Q-GFCTEF30  SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-LER-GFCTEF30.

      *----------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3500-GRAVAR-GFCTS0P0            SECTION.
      *----------------------------------------------------------------*

           WRITE FD-GFCTS0P0       FROM    WRK-GFCTEHP0-REGISTRO.

           MOVE WRK-GRAVACAO       TO WRK-OPERACAO.

           PERFORM 1130-TESTAR-FS-GFCTS0P0.

           ADD 1                   TO  WRK-GRAVADOS-GFCTS0P0.

      *----------------------------------------------------------------*
       3500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*
      *                                                                *
           DISPLAY '***************** GFCT4722 ****************'
           DISPLAY '*                                         *'
           DISPLAY '* INVESTIMENTOS ULTIMOS 03 MESES          *'
           DISPLAY '* BALANCE LINE ENTRE ARQUIVO COM AGENCIA, *'
           DISPLAY '* CONTA E TARIFAS A SEREM BONIFICADAS     *'
           DISPLAY '*                                         *'
           DISPLAY '*           RESUMO PROCESSAMENTO          *'
           DISPLAY '* --------------------------------------- *'
           DISPLAY '*                                         *'
           MOVE    WRK-LIDOS-GFCTEHP0  TO WRK-DISPLAY
           DISPLAY '* LIDOS    GFCTEHP0 : ' WRK-DISPLAY, ' *'
           DISPLAY '*                                         *'
           MOVE    WRK-LIDOS-GFCTEF30  TO WRK-DISPLAY
           DISPLAY '* LIDOS    GFCTEF30 : ' WRK-DISPLAY, ' *'
           DISPLAY '*                                         *'
           MOVE    WRK-GRAVADOS-GFCTS0P0  TO WRK-DISPLAY
           DISPLAY '* GRAVADOS GFCTS0P0 : ' WRK-DISPLAY, ' *'
           DISPLAY '*                                         *'
           DISPLAY '***************** GFCT4722 ****************'

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           CLOSE GFCTEHP0
                 GFCTEF30
                 GFCTS0P0.

           PERFORM 1100-TESTAR-FILE-STATUS.

           STOP RUN.

      *----------------------------------------------------------------*
       9000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO    SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT4722'             TO ERR-PGM.

           CALL 'POOL7100'         USING WRK-BATCH
                                         ERRO-AREA

           MOVE 4                      TO RETURN-CODE.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
      *================================================================*
