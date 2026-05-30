      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT4710.
       AUTHOR.     HOMI - SONDA.

      *================================================================*
      *                 SONDA                                          *
      *----------------------------------------------------------------*
      *    PROGRAMA....:  GFCT4710                                     *
      *    SISTEMA.....:  GFCT - GFCT                                  *
      *    PROGRAMADOR.:  HOMI - SONDA                                 *
      *    ANALISTA....:  ANTONIO PAGNOCCA - SONDA                     *
      *    GRUPO.......:  50                                           *
      *    DATA........:  DEZ 2015                                     *
      *----------------------------------------------------------------*
      *    OBJETIVO....:  INVESTIMENTOS ULTIMOS 03 MESES               *
      *                   BALANCE LINE ENTRE HSBC E CORRETORA          *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME           I/O        INCLUDE/BOOK        *
      *                GFCTEHP0          I         GFCTWF25            *
      *                GFCTECP0          I         GFCTWF25            *
      *                GFCTS0P0          O         GFCTWF25            *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    GFCTWF25 - ENTRADA E SAIDA - IMAGEM GFCTB0P0 + CNPJ/CPF     *
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

           SELECT GFCTECP0 ASSIGN      TO   UT-S-GFCTECP0
                      FILE STATUS      IS   WRK-FS-GFCTECP0.

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

       01  FD-GFCTEHP0.
       COPY 'GFCTWF25'.

      *----------------------------------------------------------------*
      *   INPUT:   ARQUIVO DE ENTRADA - IMAGEM GFCTB0P0 - CORRETORA    *
      *            ORG. SEQUENCIAL    - LRECL   = 98                   *
      *----------------------------------------------------------------*
       FD  GFCTECP0
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-GFCTECP0.
       COPY 'GFCTWF25'.

      *----------------------------------------------------------------*
      *   OUTPUT:ARQUIVO DE SAIDA    -  ARQUIVO IMAGEM GFCTB0P0        *
      *          ORG. SEQUENCIAL     -   LRECL  = 98                   *
      *----------------------------------------------------------------*
       FD  GFCTS0P0
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-GFCTS0P0.
       COPY GFCTWF25.

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       77  FILLER                      PIC  X(050)        VALUE
           '*** INICIO DA WORKING-STORAGE SECTION         ****'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01  WRK-ULT-DIAS-MESES          PIC  X(024)        VALUE
           '312831303130313130313031'.
       01  REDEFINES WRK-ULT-DIAS-MESES.
           03 OCCURS 12 TIMES.
              05 WRK-ULT-DIA-MES       PIC  9(002).

       77  WRK-BATCH                   PIC  X(008)        VALUE 'BATCH'.
       77  WRK-IDX                     PIC  9(009) COMP-3 VALUE ZEROS.
       77  WRK-YEAR                    PIC  9(005) COMP-3 VALUE ZEROS.
       77  WRK-GIVING                  PIC  9(005) COMP-3 VALUE ZEROS.
       77  WRK-REMAINDER               PIC  9(005) COMP-3 VALUE ZEROS.

       01  WRK-DATA-AM                 PIC +9(006)        VALUE ZEROS.
       01  REDEFINES WRK-DATA-AM.
           03                          PIC  X.
           03  WRK-ANO-AM              PIC  9(004).
           03  WRK-MES-AM              PIC  9(002).

       01  WRK-DATA-INI-G.
           03 WRK-INI-DIA              PIC  9(002)        VALUE 1.
           03                          PIC  X             VALUE '.'.
           03 WRK-INI-MES              PIC  9(002)        VALUE ZEROS.
           03                          PIC  X             VALUE '.'.
           03 WRK-INI-ANO              PIC  9(004)        VALUE ZEROS.
       01  WRK-DATA-INI
           REDEFINES WRK-DATA-INI-G    PIC  X(010).

       01  WRK-DATA-FIM-G.
           03 WRK-FIM-DIA              PIC  9(002)        VALUE ZEROS.
           03                          PIC  X             VALUE '.'.
           03 WRK-FIM-MES              PIC  9(002)        VALUE ZEROS.
           03                          PIC  X             VALUE '.'.
           03 WRK-FIM-ANO              PIC  9(004)        VALUE ZEROS.
       01  WRK-DATA-FIM
           REDEFINES WRK-DATA-FIM-G    PIC  X(010).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       77  FILLER                      PIC  X(050)        VALUE
           '*** AREA DE ACUMULADORES                       ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01  WRK-ACUMULADORES.

           03 WRK-LIDOS-GFCTEHP0       PIC  9(009) COMP-3 VALUE ZEROS.
           03 WRK-LIDOS-GFCTECP0       PIC  9(009) COMP-3 VALUE ZEROS.
           03 WRK-GRAVADOS-GFCTS0P0    PIC  9(009) COMP-3 VALUE ZEROS.
           03 WRK-DISPLAY              PIC +Z(008)9       VALUE ZEROS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                      PIC  X(050)        VALUE
           '*** AREA DE TESTE DE FILE-STATUS               ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01  WRK-FS-GFCTEHP0             PIC  X(002)        VALUE SPACES.
       01  WRK-FS-GFCTECP0             PIC  X(002)        VALUE SPACES.
       01  WRK-FS-GFCTS0P0             PIC  X(002)        VALUE SPACES.

       01  WRK-ABERTURA                PIC  X(013)        VALUE
           ' NA ABERTURA '.
       01  WRK-LEITURA                 PIC  X(013)        VALUE
           ' NA LEITURA  '.
       01  WRK-GRAVACAO                PIC  X(013)        VALUE
           ' NA GRAVACAO '.
       01  WRK-FECHAMENTO              PIC  X(013)        VALUE
           'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)        VALUE
           '*** MENSAGEM DE ERRO DE FILE-STATUS ***'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-POOL7100.
           03  FILLER                  PIC  X(009)        VALUE
               '*** ERRO '.
           03  WRK-OPERACAO            PIC  X(013)        VALUE SPACES.
           03  FILLER                  PIC  X(012)        VALUE
               ' DO ARQUIVO '.
           03  WRK-NOME-ARQ            PIC  X(008)        VALUE SPACES.
           03  FILLER                  PIC  X(017)        VALUE
               ' - FILE-STATUS = '.
           03  WRK-FILE-STATUS         PIC  X(002)        VALUE SPACES.
           03  FILLER                  PIC  X(004)        VALUE ' ***'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)        VALUE
           '*** AREA PARA POOL7100 ***'.
      *----------------------------------------------------------------*

       COPY POL7100C.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                      PIC  X(050)        VALUE
           '*** AREA DE CHAVES                             ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01  WRK-CHAVE-GFCTEHP0.
           03 WRK-KEY1-CAG-BCRIA        PIC S9(05)        VALUE ZEROS.
           03 WRK-KEY1-CCTA-CLI         PIC S9(07)        VALUE ZEROS.
           03 WRK-KEY1-DANO-MES-SDO     PIC S9(06)        VALUE ZEROS.

       01  WRK-CHAVE-GFCTEHP0-ANT.
           03 WRK-K1-CAG-BCRIA-ANT      PIC S9(05)        VALUE ZEROS.
           03 WRK-K1-CCTA-CLI-ANT       PIC S9(07)        VALUE ZEROS.
           03 WRK-K1-DANO-MES-SDO-ANT   PIC S9(06)        VALUE ZEROS.

       01  WRK-CHAVE-GFCTECP0.
           03 WRK-KEY2-CAG-BCRIA        PIC S9(05)        VALUE ZEROS.
           03 WRK-KEY2-CCTA-CLI         PIC S9(07)        VALUE ZEROS.
           03 WRK-KEY2-DANO-MES-SDO     PIC S9(06)        VALUE ZEROS.

       01  WRK-CHAVE-GFCTECP0-ANT.
           03 WRK-K2-CAG-BCRIA-ANT      PIC S9(05)        VALUE ZEROS.
           03 WRK-K2-CCTA-CLI-ANT       PIC S9(07)        VALUE ZEROS.
           03 WRK-K2-DANO-MES-SDO-ANT   PIC S9(06)        VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)        VALUE
           'AREA PARA TABELA DB2'.
      *---------------------------------------------------------------*

           EXEC SQL INCLUDE SQLCA    END-EXEC.

           EXEC SQL INCLUDE GFCTB009 END-EXEC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                      PIC  X(050)        VALUE
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
               AND WRK-CHAVE-GFCTECP0   EQUAL     HIGH-VALUES.

           PERFORM 9000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL1050'.

           OPEN INPUT   GFCTEHP0
                        GFCTECP0
                OUTPUT  GFCTS0P0.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-GFCTECP0.

           PERFORM 1120-TESTAR-FS-GFCTEHP0.

           PERFORM 1130-TESTAR-FS-GFCTS0P0.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1110-TESTAR-FS-GFCTECP0         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-GFCTECP0         NOT EQUAL  '00'
               MOVE 'GFCTECP0'         TO WRK-NOME-ARQ
               MOVE WRK-FS-GFCTECP0    TO WRK-FILE-STATUS
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

           PERFORM 2100-LER-GFCTECP0.

           IF (WRK-LIDOS-GFCTECP0      EQUAL     ZEROS)
               DISPLAY '************ GFCT4710 ************'
               DISPLAY '*                                *'
               DISPLAY '*     ARQUIVO GFCTECP0 VAZIO     *'
               DISPLAY '*                                *'
               DISPLAY '************ GFCT4710 ************'
           END-IF.

           PERFORM 2200-LER-GFCTEHP0.

           IF (WRK-LIDOS-GFCTEHP0      EQUAL     ZEROS)
               DISPLAY '************ GFCT4710 ************'
               DISPLAY '*                                *'
               DISPLAY '*     ARQUIVO GFCTEHP0 VAZIO     *'
               DISPLAY '*                                *'
               DISPLAY '************ GFCT4710 ************'
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2100-LER-GFCTECP0               SECTION.
      *----------------------------------------------------------------*

           READ GFCTECP0.

           IF WRK-FS-GFCTECP0          EQUAL     '10'
               MOVE HIGH-VALUES        TO   WRK-CHAVE-GFCTECP0
               GO TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1110-TESTAR-FS-GFCTECP0.
           ADD  1  TO   WRK-LIDOS-GFCTECP0.
           PERFORM 2110-MONTAR-CHAVE-GFCTECP0.

           IF WRK-CHAVE-GFCTECP0 LESS THAN WRK-CHAVE-GFCTECP0-ANT
              DISPLAY '************ GFCT4710 ************'
              DISPLAY '*                                *'
              DISPLAY '*       ARQUIVO GFCTECP0         *'
              DISPLAY '*       NAO CLASSIFICADO         *'
              DISPLAY '*       PROGRAMA ENCERRADO       *'
              DISPLAY '* CHAVE GFCTECP0 ANTERIOR        *'
              MOVE WRK-K2-CAG-BCRIA-ANT  TO WRK-DISPLAY
              DISPLAY '*   CAG-BCRIA-ANT............= ' WRK-DISPLAY
              DISPLAY '*--------------------------------*'
              DISPLAY '* CHAVE GFCTECP0 LIDA            *'
              MOVE WRK-KEY2-CAG-BCRIA TO WRK-DISPLAY
              DISPLAY '*   CAG-BCRIA................= ' WRK-DISPLAY
              DISPLAY '************ GFCT4710 ************'
              MOVE 'GFCTECP0'         TO WRK-NOME-ARQ
              MOVE WRK-FS-GFCTECP0    TO WRK-FILE-STATUS
              MOVE WRK-ERRO-POOL7100  TO ERR-TEXTO
              MOVE 'APL'              TO ERR-TIPO-ACESSO
              PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2110-MONTAR-CHAVE-GFCTECP0      SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-CHAVE-GFCTECP0 TO WRK-CHAVE-GFCTECP0-ANT.

           MOVE WF25-CAG-BCRIA  OF FD-GFCTECP0 TO WRK-KEY2-CAG-BCRIA.
           MOVE WF25-CCTA-CLI   OF FD-GFCTECP0 TO WRK-KEY2-CCTA-CLI.
           MOVE WF25-DANO-MES-SDO OF FD-GFCTECP0
                                               TO WRK-KEY2-DANO-MES-SDO.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2200-LER-GFCTEHP0               SECTION.
      *----------------------------------------------------------------*

           READ GFCTEHP0.

           IF WRK-FS-GFCTEHP0          EQUAL     '10'
               MOVE HIGH-VALUES        TO   WRK-CHAVE-GFCTEHP0
               GO TO 2200-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1120-TESTAR-FS-GFCTEHP0.
           ADD  1  TO   WRK-LIDOS-GFCTEHP0.
           PERFORM 2210-MONTAR-CHAVE-GFCTEHP0.

           IF WRK-CHAVE-GFCTEHP0 LESS THAN WRK-CHAVE-GFCTEHP0-ANT
              DISPLAY '************ GFCT4710 ************'
              DISPLAY '*                                *'
              DISPLAY '*       ARQUIVO GFCTEHP0         *'
              DISPLAY '*       NAO CLASSIFICADO         *'
              DISPLAY '*       PROGRAMA ENCERRADO       *'
              DISPLAY '* CHAVE GFCTEHP0 ANTERIOR        *'
              MOVE WRK-K1-CAG-BCRIA-ANT  TO WRK-DISPLAY
              DISPLAY '*   CAG-BCRIA-ANT ...........= ' WRK-DISPLAY
              DISPLAY '*--------------------------------*'
              DISPLAY '* CHAVE GFCTEHP0 LIDA            *'
              MOVE WRK-KEY1-CAG-BCRIA TO WRK-DISPLAY
              DISPLAY '*   CAG-BCRIA................= ' WRK-DISPLAY
              DISPLAY '************ GFCT4710 ************'
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

           MOVE WF25-CAG-BCRIA OF FD-GFCTEHP0 TO WRK-KEY1-CAG-BCRIA.
           MOVE WF25-CCTA-CLI  OF FD-GFCTEHP0 TO WRK-KEY1-CCTA-CLI.
           MOVE WF25-DANO-MES-SDO OF FD-GFCTEHP0
                                               TO WRK-KEY1-DANO-MES-SDO.

      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           IF      WRK-CHAVE-GFCTEHP0  LESS THAN WRK-CHAVE-GFCTECP0
                   PERFORM 3100-GFCTEHP0-MENOR-Q-GFCTECP0
           ELSE
             IF    WRK-CHAVE-GFCTEHP0  EQUAL TO  WRK-CHAVE-GFCTECP0
                   PERFORM 3200-GFCTEHP0-IGUAL-A-GFCTECP0
               ELSE
                   PERFORM 3300-GFCTEHP0-MAIOR-Q-GFCTECP0
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3100-GFCTEHP0-MENOR-Q-GFCTECP0  SECTION.
      *----------------------------------------------------------------*

           MOVE FD-GFCTEHP0 TO FD-GFCTS0P0.

           PERFORM 3500-GRAVAR-GFCTS0P0.

           PERFORM 2200-LER-GFCTEHP0.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3200-GFCTEHP0-IGUAL-A-GFCTECP0  SECTION.
      *----------------------------------------------------------------*

           MOVE FD-GFCTEHP0          TO FD-GFCTS0P0.
           MOVE WF25-VSDO-APLIC-ACOES OF FD-GFCTECP0
             TO WF25-VSDO-APLIC-ACOES OF FD-GFCTS0P0.
           MOVE WF25-VSDO-APLIC-TSORO OF FD-GFCTECP0
             TO WF25-VSDO-APLIC-TSORO OF FD-GFCTS0P0.

           PERFORM 3500-GRAVAR-GFCTS0P0.

           PERFORM 2200-LER-GFCTEHP0.
           PERFORM 2100-LER-GFCTECP0.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3300-GFCTEHP0-MAIOR-Q-GFCTECP0  SECTION.
      *----------------------------------------------------------------*

           PERFORM 3900-OBTER-TARIFA.

           MOVE CSERVC-TARIF OF GFCTB009
                                    TO WF25-CSERVC-TARIF OF FD-GFCTECP0.
           MOVE FD-GFCTECP0         TO FD-GFCTS0P0.

           PERFORM 3500-GRAVAR-GFCTS0P0.

           PERFORM 2100-LER-GFCTECP0.

      *----------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3500-GRAVAR-GFCTS0P0            SECTION.
      *----------------------------------------------------------------*

           WRITE FD-GFCTS0P0.

           MOVE WRK-GRAVACAO       TO WRK-OPERACAO.

           PERFORM 1130-TESTAR-FS-GFCTS0P0.

           ADD 1                   TO  WRK-GRAVADOS-GFCTS0P0.

      *----------------------------------------------------------------*
       3500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *     OBTER TARIFA                                               *
      ******************************************************************
      *----------------------------------------------------------------*
       3900-OBTER-TARIFA               SECTION.
      *----------------------------------------------------------------*
           MOVE WF25-CAG-BCRIA OF FD-GFCTECP0
                                       TO CJUNC-DEPDC       OF GFCTB009.
           MOVE WF25-CCTA-CLI OF FD-GFCTECP0
                                       TO CCTA-CLI          OF GFCTB009.
           MOVE ZEROS                  TO CINDCD-EXCL-REG   OF GFCTB009.
           MOVE WF25-DANO-MES-SDO OF FD-GFCTECP0
                                       TO WRK-DATA-AM.
           MOVE WRK-ANO-AM             TO WRK-INI-ANO
                                          WRK-FIM-ANO
                                          WRK-YEAR.

           DIVIDE WRK-YEAR BY 4 GIVING WRK-GIVING
                                REMAINDER  WRK-REMAINDER
           END-DIVIDE.
           IF WRK-REMAINDER EQUAL TO ZEROS
              MOVE 29 TO WRK-ULT-DIA-MES(2)
           END-IF.

           MOVE WRK-MES-AM             TO WRK-INI-MES
                                          WRK-FIM-MES
                                          WRK-IDX.
           MOVE WRK-ULT-DIA-MES(WRK-IDX) TO WRK-FIM-DIA.

           MOVE WRK-DATA-INI           TO DINIC-ADSAO-INDVD OF GFCTB009.
           MOVE WRK-DATA-FIM           TO DFIM-ADSAO-INDVD  OF GFCTB009.

           EXEC SQL
                SELECT CSERVC_TARIF
                  INTO :GFCTB009.CSERVC-TARIF
                  FROM DB2PRD.ADSAO_INDVD_PCOTE
                 WHERE CJUNC_DEPDC        = :GFCTB009.CJUNC-DEPDC
                   AND CCTA_CLI           = :GFCTB009.CCTA-CLI
                   AND CINDCD_EXCL_REG    = :GFCTB009.CINDCD-EXCL-REG
                   AND DINIC_ADSAO_INDVD <= :GFCTB009.DINIC-ADSAO-INDVD
                   AND DFIM_ADSAO_INDVD  >= :GFCTB009.DFIM-ADSAO-INDVD
                 FETCH FIRST 1 ROW ONLY
           END-EXEC.

           IF (SQLCODE NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
              MOVE 'DB2'                TO ERR-TIPO-ACESSO
              MOVE 'ADSAO_INDVD_PCOTE'  TO ERR-DBD-TAB
              MOVE 'SELECT'             TO ERR-FUN-COMANDO
              MOVE  SQLCODE             TO ERR-SQL-CODE
              MOVE '0004'               TO ERR-LOCAL
              MOVE  SPACES              TO ERR-SEGM
              PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF SQLCODE EQUAL +100
              MOVE ZEROS TO CSERVC-TARIF OF GFCTB009
           END-IF.

      *----------------------------------------------------------------*
       3900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       9000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*
      *                                                                *
           DISPLAY '***************** GFCT4710 ****************'
           DISPLAY '*                                         *'
           DISPLAY '*  INVESTIMENTOS ULTIMOS 03 MESES         *'
           DISPLAY '*  BALANCE LINE ENTRE HSBC E CORRETORA    *'
           DISPLAY '*                                         *'
           DISPLAY '*           RESUMO PROCESSAMENTO          *'
           DISPLAY '* --------------------------------------- *'
           DISPLAY '*                                         *'
           MOVE    WRK-LIDOS-GFCTEHP0  TO WRK-DISPLAY
           DISPLAY '* LIDOS    GFCTEHP0 : ' WRK-DISPLAY, ' *'
           DISPLAY '*                                         *'
           MOVE    WRK-LIDOS-GFCTECP0  TO WRK-DISPLAY
           DISPLAY '* LIDOS    GFCTECP0 : ' WRK-DISPLAY, ' *'
           DISPLAY '*                                         *'
           MOVE    WRK-GRAVADOS-GFCTS0P0  TO WRK-DISPLAY
           DISPLAY '* GRAVADOS GFCTS0P0 : ' WRK-DISPLAY, ' *'
           DISPLAY '*                                         *'
           DISPLAY '***************** GFCT4710 ****************'

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           CLOSE GFCTEHP0
                 GFCTECP0
                 GFCTS0P0.

           PERFORM 1100-TESTAR-FILE-STATUS.

           STOP RUN.

      *----------------------------------------------------------------*
       9000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO    SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT4710'             TO ERR-PGM.

           IF  ERR-TIPO-ACESSO         EQUAL 'APL'
               CALL 'POOL7100'         USING WRK-BATCH
                                             ERRO-AREA
           ELSE
               CALL 'POOL7100'         USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA
           END-IF.

           MOVE 4                      TO RETURN-CODE.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
      *================================================================*
