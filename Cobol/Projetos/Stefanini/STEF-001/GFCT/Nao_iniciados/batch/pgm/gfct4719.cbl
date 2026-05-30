      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT4719.
       AUTHOR.     HOMI - SONDA.

      *================================================================*
      *                 SONDA                                          *
      *----------------------------------------------------------------*
      *    PROGRAMA....:  GFCT4719                                     *
      *    SISTEMA.....:  GFCT - GFCT                                  *
      *    PROGRAMADOR.:  HOMI - SONDA                                 *
      *    ANALISTA....:  ANTONIO PAGNOCCA - SONDA                     *
      *    GRUPO.......:  50                                           *
      *    DATA........:  DEZ 2015                                     *
      *----------------------------------------------------------------*
      *    OBJETIVO....:  INVESTIMENTOS ULTIMOS 03 MESES               *
      *                   BALANCE LINE ENTRE HSBC E SUM DOS SALDOS     *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME           I/O        INCLUDE/BOOK        *
      *                GFCTEHP0          I         GFCTWF25            *
      *                GFCTESUM          I         GFCTWF25            *
      *                GFCTS0P0          O         GFCTWF25            *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    GFCTEHP0-GFCTWF25 - ENTRADA IMAGEM GFCTB0P0 + CNPJ/CPF      *
      *    GFCTESUM-GFCTWF25 - ENTRADA IMAGEM GFCTB0P0 + CNPJ/CPF SUM  *
      *    GFCTS0P0-GFCTWF25 - SAIDA   IMAGEM GFCTB0P0 CONSOLIDADO     *
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

           SELECT GFCTESUM ASSIGN      TO   UT-S-GFCTESUM
                      FILE STATUS      IS   WRK-FS-GFCTESUM.

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
      *   INPUT:   ARQUIVO DE ENTRADA - IMAGEM GFCTB0P0 - SUM     A    *
      *            ORG. SEQUENCIAL    - LRECL   = 98                   *
      *----------------------------------------------------------------*
       FD  GFCTESUM
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-GFCTESUM                 PIC  X(98).

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
           03 WRK-LIDOS-GFCTESUM       PIC  9(009) COMP-3  VALUE ZEROS.
           03 WRK-GRAVADOS-GFCTS0P0    PIC  9(009) COMP-3  VALUE ZEROS.
           03 WRK-DISPLAY              PIC +Z(008)9        VALUE ZEROS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE TESTE DE FILE-STATUS               ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01  WRK-FS-GFCTEHP0             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-GFCTESUM             PIC  X(002)         VALUE SPACES.
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
ST25X6*    03 WRK-KEY1-CNPJ-CPF         PIC S9(09)    VALUE ZEROS.
ST25X6     03 WRK-KEY1-CNPJ-CPF         PIC X(009)    VALUE SPACES.
           03 WRK-KEY1-DANO-MES-SDO     PIC S9(06)    VALUE ZEROS.

       01  WRK-CHAVE-GFCTEHP0-ANT.
ST25X6*    03 WRK-K1-CNPJ-CPF-ANT       PIC S9(09)    VALUE ZEROS.
ST25X6     03 WRK-K1-CNPJ-CPF-ANT       PIC X(009)    VALUE SPACES.
           03 WRK-K1-DANO-MES-SDO-ANT   PIC S9(06)    VALUE ZEROS.

       01  WRK-CHAVE-GFCTESUM.
ST25X6*    03 WRK-KEY2-CNPJ-CPF         PIC S9(09)    VALUE ZEROS.
ST25X6     03 WRK-KEY2-CNPJ-CPF         PIC X(009)    VALUE SPACES.
           03 WRK-KEY2-DANO-MES-SDO     PIC S9(06)    VALUE ZEROS.

       01  WRK-CHAVE-GFCTESUM-ANT.
ST25X6*    03 WRK-K2-CNPJ-CPF-ANT       PIC S9(09)    VALUE ZEROS.
ST25X6     03 WRK-K2-CNPJ-CPF-ANT       PIC X(009)    VALUE SPACES.
           03 WRK-K2-DANO-MES-SDO-ANT   PIC S9(06)    VALUE ZEROS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ENTRADA GFCTEHP0                   ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01 WRK-GFCTEHP0-REGISTRO.
       COPY 'GFCTWF25'.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ENTRADA GFCTESUM SUM               ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01 WRK-GFCTESUM-REGISTRO.
       COPY 'GFCTWF25'.

       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE SAIDA GFCTS0P0                     ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01 WRK-GFCTS0P0-REGISTRO.
       COPY 'GFCTWF25'.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                      PIC  X(050)         VALUE
           '*** FIM DA WORKING-STORAGE SECTION             ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIAR.

           PERFORM 2000-VERIFICAR-VAZIO.

           PERFORM 3000-PROCESSAR
             UNTIL WRK-CHAVE-GFCTEHP0   EQUAL     HIGH-VALUES
               AND WRK-CHAVE-GFCTESUM   EQUAL     HIGH-VALUES.

           PERFORM 9000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT   GFCTEHP0
                        GFCTESUM
                OUTPUT  GFCTS0P0.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-GFCTESUM.

           PERFORM 1120-TESTAR-FS-GFCTEHP0.

           PERFORM 1130-TESTAR-FS-GFCTS0P0.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1110-TESTAR-FS-GFCTESUM         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-GFCTESUM         NOT EQUAL  '00'
               MOVE 'GFCTESUM'         TO WRK-NOME-ARQ
               MOVE WRK-FS-GFCTESUM    TO WRK-FILE-STATUS
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

           PERFORM 2100-LER-GFCTESUM.

           IF (WRK-LIDOS-GFCTESUM      EQUAL     ZEROS)
               DISPLAY '************ GFCT4719 ************'
               DISPLAY '*                                *'
               DISPLAY '*     ARQUIVO GFCTESUM VAZIO     *'
               DISPLAY '*                                *'
               DISPLAY '************ GFCT4719 ************'
               MOVE 'GFCTESUM'         TO WRK-NOME-ARQ
               MOVE WRK-FS-GFCTESUM    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-POOL7100  TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 2200-LER-GFCTEHP0.

           IF (WRK-LIDOS-GFCTEHP0      EQUAL     ZEROS)
               DISPLAY '************ GFCT4719 ************'
               DISPLAY '*                                *'
               DISPLAY '*     ARQUIVO GFCTEHP0 VAZIO     *'
               DISPLAY '*                                *'
               DISPLAY '************ GFCT4719 ************'
               MOVE 'GFCTEHP0'         TO WRK-NOME-ARQ
               MOVE WRK-FS-GFCTEHP0    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-POOL7100  TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2100-LER-GFCTESUM               SECTION.
      *----------------------------------------------------------------*

           READ GFCTESUM               INTO WRK-GFCTESUM-REGISTRO.

           IF WRK-FS-GFCTESUM          EQUAL     '10'
               MOVE HIGH-VALUES        TO   WRK-CHAVE-GFCTESUM
               GO TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1110-TESTAR-FS-GFCTESUM.
           ADD  1  TO   WRK-LIDOS-GFCTESUM.
           PERFORM 2110-MONTAR-CHAVE-GFCTESUM.

           IF WRK-CHAVE-GFCTESUM LESS THAN WRK-CHAVE-GFCTESUM-ANT
              DISPLAY '************ GFCT4719 ************'
              DISPLAY '*                                *'
              DISPLAY '*       ARQUIVO GFCTESUM         *'
              DISPLAY '*       NAO CLASSIFICADO         *'
              DISPLAY '*       PROGRAMA ENCERRADO       *'
              DISPLAY '* CHAVE GFCTESUM ANTERIOR        *'
              MOVE WRK-K2-CNPJ-CPF-ANT  TO WRK-DISPLAY
              DISPLAY '*   CNPJ-CPF-ANT.............= ' WRK-DISPLAY
              DISPLAY '*--------------------------------*'
              DISPLAY '* CHAVE GFCTESUM LIDA            *'
              MOVE WRK-KEY2-CNPJ-CPF TO WRK-DISPLAY
              DISPLAY '*   CNPJ-CPF.................= ' WRK-DISPLAY
              DISPLAY '************ GFCT4719 ************'
              MOVE 'GFCTESUM'         TO WRK-NOME-ARQ
              MOVE WRK-FS-GFCTESUM    TO WRK-FILE-STATUS
              MOVE WRK-ERRO-POOL7100  TO ERR-TEXTO
              MOVE 'APL'              TO ERR-TIPO-ACESSO
              PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2110-MONTAR-CHAVE-GFCTESUM      SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-CHAVE-GFCTESUM TO WRK-CHAVE-GFCTESUM-ANT.

           MOVE WF25-CNPJ-CPF      OF WRK-GFCTESUM-REGISTRO
                                   TO WRK-KEY2-CNPJ-CPF.
           MOVE WF25-DANO-MES-SDO  OF WRK-GFCTESUM-REGISTRO
                                   TO WRK-KEY2-DANO-MES-SDO.

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
              DISPLAY '************ GFCT4719 ************'
              DISPLAY '*                                *'
              DISPLAY '*       ARQUIVO GFCTEHP0         *'
              DISPLAY '*       NAO CLASSIFICADO         *'
              DISPLAY '*       PROGRAMA ENCERRADO       *'
              DISPLAY '* CHAVE GFCTEHP0 ANTERIOR        *'
              MOVE WRK-K1-CNPJ-CPF-ANT  TO WRK-DISPLAY
              DISPLAY '*   CNPJ-CPF-ANT  ...........= ' WRK-DISPLAY
              DISPLAY '*--------------------------------*'
              DISPLAY '* CHAVE GFCTEHP0 LIDA            *'
              MOVE WRK-KEY1-CNPJ-CPF TO WRK-DISPLAY
              DISPLAY '*   CNPJ-CPF.................= ' WRK-DISPLAY
              DISPLAY '************ GFCT4719 ************'
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

           MOVE WF25-CNPJ-CPF      OF WRK-GFCTEHP0-REGISTRO
                                   TO WRK-KEY1-CNPJ-CPF.
           MOVE WF25-DANO-MES-SDO  OF WRK-GFCTEHP0-REGISTRO
                                   TO WRK-KEY1-DANO-MES-SDO.

      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           IF      WRK-CHAVE-GFCTEHP0  LESS THAN WRK-CHAVE-GFCTESUM
                   PERFORM 3100-GFCTEHP0-MENOR-Q-GFCTESUM
           ELSE
             IF    WRK-CHAVE-GFCTEHP0  EQUAL TO  WRK-CHAVE-GFCTESUM
                   PERFORM 3200-GFCTEHP0-IGUAL-A-GFCTESUM
               ELSE
                   PERFORM 3300-GFCTEHP0-MAIOR-Q-GFCTESUM
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3100-GFCTEHP0-MENOR-Q-GFCTESUM  SECTION.
      *----------------------------------------------------------------*

           DISPLAY '************ GFCT4719 ************'
           DISPLAY '*                                *'
           DISPLAY '*  REGISTRO GFCTEHP0 NAO EXISTE  *'
           DISPLAY '*        EM GFCTESUM/SUM         *'
           DISPLAY '*                                *'
           DISPLAY '************ GFCT4719 ************'
           MOVE 'GFCTEHP0'         TO WRK-NOME-ARQ
           MOVE WRK-FS-GFCTEHP0    TO WRK-FILE-STATUS
           MOVE WRK-ERRO-POOL7100  TO ERR-TEXTO
           MOVE 'APL'              TO ERR-TIPO-ACESSO
           PERFORM 9999-PROCESSAR-ROTINA-ERRO.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3200-GFCTEHP0-IGUAL-A-GFCTESUM  SECTION.
      *----------------------------------------------------------------*

           MOVE WF25-CBCO                 OF WRK-GFCTEHP0-REGISTRO
             TO WF25-CBCO                 OF WRK-GFCTS0P0-REGISTRO.
           MOVE WF25-CAG-BCRIA            OF WRK-GFCTEHP0-REGISTRO
             TO WF25-CAG-BCRIA            OF WRK-GFCTS0P0-REGISTRO.
           MOVE WF25-CCTA-CLI             OF WRK-GFCTEHP0-REGISTRO
             TO WF25-CCTA-CLI             OF WRK-GFCTS0P0-REGISTRO.
           MOVE WF25-DANO-MES-SDO         OF WRK-GFCTEHP0-REGISTRO
             TO WF25-DANO-MES-SDO         OF WRK-GFCTS0P0-REGISTRO.
           MOVE WF25-CSERVC-TARIF         OF WRK-GFCTEHP0-REGISTRO
             TO WF25-CSERVC-TARIF         OF WRK-GFCTS0P0-REGISTRO.

           MOVE WF25-VSDO-MED-FUNDO       OF WRK-GFCTESUM-REGISTRO
             TO WF25-VSDO-MED-FUNDO       OF WRK-GFCTS0P0-REGISTRO.
           MOVE WF25-VSDO-MED-POUP        OF WRK-GFCTESUM-REGISTRO
             TO WF25-VSDO-MED-POUP        OF WRK-GFCTS0P0-REGISTRO.
           MOVE WF25-VSDO-MED-CERTF       OF WRK-GFCTESUM-REGISTRO
             TO WF25-VSDO-MED-CERTF       OF WRK-GFCTS0P0-REGISTRO.
           MOVE WF25-VSDO-MED-PREVD       OF WRK-GFCTESUM-REGISTRO
             TO WF25-VSDO-MED-PREVD       OF WRK-GFCTS0P0-REGISTRO.
           MOVE WF25-VSDO-APLIC-ACOES     OF WRK-GFCTESUM-REGISTRO
             TO WF25-VSDO-APLIC-ACOES     OF WRK-GFCTS0P0-REGISTRO.
           MOVE WF25-VSDO-APLIC-TSORO     OF WRK-GFCTESUM-REGISTRO
             TO WF25-VSDO-APLIC-TSORO     OF WRK-GFCTS0P0-REGISTRO.
           MOVE WF25-VSDO-APLIC-DBTRS     OF WRK-GFCTESUM-REGISTRO
             TO WF25-VSDO-APLIC-DBTRS     OF WRK-GFCTS0P0-REGISTRO.
           MOVE WF25-VSDO-APLIC-DERIV     OF WRK-GFCTESUM-REGISTRO
             TO WF25-VSDO-APLIC-DERIV     OF WRK-GFCTS0P0-REGISTRO.

           MOVE WF25-CNPJ-CPF             OF WRK-GFCTEHP0-REGISTRO
             TO WF25-CNPJ-CPF             OF WRK-GFCTS0P0-REGISTRO.
           MOVE WF25-FILIAL               OF WRK-GFCTEHP0-REGISTRO
             TO WF25-FILIAL               OF WRK-GFCTS0P0-REGISTRO.
           MOVE WF25-DIGITO-FJ            OF WRK-GFCTEHP0-REGISTRO
             TO WF25-DIGITO-FJ            OF WRK-GFCTS0P0-REGISTRO.

           PERFORM 3500-GRAVAR-GFCTS0P0.

           PERFORM 2200-LER-GFCTEHP0.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3300-GFCTEHP0-MAIOR-Q-GFCTESUM  SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-LER-GFCTESUM.

      *----------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3500-GRAVAR-GFCTS0P0            SECTION.
      *----------------------------------------------------------------*

           WRITE FD-GFCTS0P0       FROM    WRK-GFCTS0P0-REGISTRO.

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
           DISPLAY '***************** GFCT4719 ****************'
           DISPLAY '*                                         *'
           DISPLAY '* INVESTIMENTOS ULTIMOS 03 MESES          *'
           DISPLAY '* BALANCE LINE ENTRE HSBC E SUM DOS SALDOS*'
           DISPLAY '*                                         *'
           DISPLAY '*           RESUMO PROCESSAMENTO          *'
           DISPLAY '* --------------------------------------- *'
           DISPLAY '*                                         *'
           MOVE    WRK-LIDOS-GFCTEHP0  TO WRK-DISPLAY
           DISPLAY '* LIDOS    GFCTEHP0 : ' WRK-DISPLAY, ' *'
           DISPLAY '*                                         *'
           MOVE    WRK-LIDOS-GFCTESUM  TO WRK-DISPLAY
           DISPLAY '* LIDOS    GFCTESUM : ' WRK-DISPLAY, ' *'
           DISPLAY '*                                         *'
           MOVE    WRK-GRAVADOS-GFCTS0P0  TO WRK-DISPLAY
           DISPLAY '* GRAVADOS GFCTS0P0 : ' WRK-DISPLAY, ' *'
           DISPLAY '*                                         *'
           DISPLAY '***************** GFCT4719 ****************'

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           CLOSE GFCTEHP0
                 GFCTESUM
                 GFCTS0P0.

           PERFORM 1100-TESTAR-FILE-STATUS.

           STOP RUN.

      *----------------------------------------------------------------*
       9000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO    SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT4719'             TO ERR-PGM.

           CALL 'POOL7100'         USING WRK-BATCH
                                         ERRO-AREA

           MOVE 4                      TO RETURN-CODE.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
      *================================================================*
