      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID.     BVVE0485.
       AUTHOR.         FERNANDA RODRIGUES.
      *================================================================*
      *                         W I P R O                              *
      *----------------------------------------------------------------*
      *    ANALISTA....: FERNANDA RODRIGUES  -  WIPRO                  *
      *    DATA........: MARCO/2019                                    *
      *----------------------------------------------------------------*
      *    OBJETIVO....: OBTER DATA FINAL DO CONTRATO                  *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME        IN/OUT      INCLUDE               *
      *                EARQDCOM      INPUT       I#BVVEBL              *
      *                EARQGRAN      INPUT       I#BVVEBV              *
      *                ARQDTFIM      OUTPUT      I#BVVEBV              *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *     I#BRAD7C - AREA PARA TRATAMENTO DE ERRO PELA BRAD7100      *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *     BRAD7100 - TRATAMENTO DE ERROS QUANDO PROGRAMA INVALIDO    *
      *     BRAD7600 - TRATAMENTO DE DATA E HORA CORRENTE              *
      *================================================================*

      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS          COMMA.

      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*

       FILE-CONTROL.

           SELECT  EARQDCOM    ASSIGN  TO          UT-S-EARQDCOM
                       FILE    STATUS  IS          WRK-FS-EARQDCOM.

           SELECT  EARQGRAN    ASSIGN  TO          UT-S-EARQGRAN
                       FILE    STATUS  IS          WRK-FS-EARQGRAN.

           SELECT  ARQDTFIM    ASSIGN  TO          UT-S-ARQDTFIM
                       FILE    STATUS  IS          WRK-FS-ARQDTFIM.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT : ARQUIVO DO DCOM COM VALORES DE ORPAGS + BANDEIRA    *
      *----------------------------------------------------------------*
       FD  EARQDCOM
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-EARQDCOM                 PIC  X(160).

      *----------------------------------------------------------------*
      *    INPUT : VALORES GARANTIA RECEBIDOS DO GRAN                  *
      *----------------------------------------------------------------*
       FD  EARQGRAN
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-EARQGRAN                 PIC  X(200).

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO IMAGEM DA TABEAL ARQDTFIM                   *
      *----------------------------------------------------------------*
       FD  ARQDTFIM
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-ARQDTFIM                 PIC  X(200).


      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)          VALUE
           '*** BVVE0485 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** ACUMULADORES ***'.
      *----------------------------------------------------------------*
       77  ACU-LIDOS-EARQDCOM          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-LIDOS-EARQGRAN          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-ARQDTFIM          PIC  9(009) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREAS DE FILE-STATUS ***'.
      *----------------------------------------------------------------*
       77  WRK-FS-EARQDCOM             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-EARQGRAN             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-ARQDTFIM             PIC  X(002)         VALUE SPACES.

       77  WRK-ABERTURA                PIC  X(13) VALUE 'NA ABERTURA'.
       77  WRK-LEITURA                 PIC  X(13) VALUE 'NA LEITURA'.
       77  WRK-GRAVACAO                PIC  X(13) VALUE 'NA GRAVACAO'.
       77  WRK-FECHAMENTO              PIC  X(13) VALUE 'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREAS AUXILIARES ***'.
      *----------------------------------------------------------------*
       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.
       77  WRK-MASCARA                 PIC  ZZZ.ZZZ.ZZ9    VALUE ZEROS.


       01  WRK-CHAVE-EARQDCOM-ANT      PIC  X(035)         VALUE SPACES.
       01  WRK-CHAVE-EARQGRAN-ANT      PIC  X(035)         VALUE SPACES.

       01  WRK-CHAVE-EARQDCOM.
           05 WRK-CH-DCOM-AGE          PIC  9(005)         VALUE ZEROS.
           05 WRK-CH-DCOM-CONTA        PIC  9(013)         VALUE ZEROS.
           05 WRK-CH-DCOM-BAND         PIC  X(004)         VALUE SPACES.
7C2511*    05 WRK-CH-DCOM-CNPJ         PIC  9(009)         VALUE ZEROS.
7C2511     05 WRK-CH-DCOM-CNPJ         PIC  X(009)         VALUE SPACES.
7C2511*    05 WRK-CH-DCOM-FILIAL       PIC  9(004)         VALUE ZEROS.
7C2511     05 WRK-CH-DCOM-FILIAL       PIC  X(004)         VALUE SPACES.

       01  WRK-CHAVE-EARQGRAN.
           05 WRK-CH-GRAN-AGE          PIC  9(005)         VALUE ZEROS.
           05 WRK-CH-GRAN-CONTA        PIC  9(013)         VALUE ZEROS.
           05 WRK-CH-GRAN-BAND         PIC  X(004)         VALUE SPACES.
7C2511*    05 WRK-CH-GRAN-CNPJ         PIC  9(009)         VALUE ZEROS.
7C2511     05 WRK-CH-GRAN-CNPJ         PIC  X(009)         VALUE SPACES.
7C2511*    05 WRK-CH-GRAN-FILIAL       PIC  9(004)         VALUE ZEROS.
7C2511     05 WRK-CH-GRAN-FILIAL       PIC  X(004)         VALUE SPACES.

      *----------------------------------------------------------------*
      *    '*** AREA DA BRAD7600 ***'.
      *----------------------------------------------------------------*

       01    WRK-DATA-HORA.
         05  WRK-DT-JULIANA            PIC  9(05)  COMP-3 VALUE ZEROS.
         05  WRK-DT-AAMMDD             PIC  9(07)  COMP-3 VALUE ZEROS.
         05  WRK-DT-AAAAMMDD-7600      PIC  9(09)  COMP-3 VALUE ZEROS.
         05  WRK-TI-HHMMSS             PIC  9(07)  COMP-3 VALUE ZEROS.
         05  WRK-TI-HHMMSSMMMMMM       PIC  9(13)  COMP-3 VALUE ZEROS.
         05  WRK-TIMESTAMP.
           10 WRK-TIMES-AAAA           PIC  9(04)   VALUE ZEROS.
           10 WRK-TIMES-MM             PIC  9(02)   VALUE ZEROS.
           10 WRK-TIMES-DD             PIC  9(02)   VALUE ZEROS.
           10 WRK-TIMES-HH             PIC  9(02)   VALUE ZEROS.
           10 WRK-TIMES-MN             PIC  9(02)   VALUE ZEROS.
           10 WRK-TIMES-SS             PIC  9(02)   VALUE ZEROS.
           10 WRK-TIMES-MIL            PIC  9(06)   VALUE ZEROS.

       01  WRK-TIMESTAMP-EDIT.
           03 WRK-TIMES-AAAA           PIC  9(04)   VALUE ZEROS.
           03 FILLER                   PIC  X(01)   VALUE '-'.
           03 WRK-TIMES-MM             PIC  9(02)   VALUE ZEROS.
           03 FILLER                   PIC  X(01)   VALUE '-'.
           03 WRK-TIMES-DD             PIC  9(02)   VALUE ZEROS.
           03 FILLER                   PIC  X(01)   VALUE '-'.
           03 WRK-TIMES-HH             PIC  9(02)   VALUE ZEROS.
           03 FILLER                   PIC  X(01)   VALUE '.'.
           03 WRK-TIMES-MN             PIC  9(02)   VALUE ZEROS.
           03 FILLER                   PIC  X(01)   VALUE '.'.
           03 WRK-TIMES-SS             PIC  9(02)   VALUE ZEROS.
           03 FILLER                   PIC  X(01)   VALUE '.'.
           03 WRK-TIMES-MIL            PIC  9(06)   VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(055)         VALUE
           '*** AREA DO ARQUIVO EARQDCOM ***'.
      *----------------------------------------------------------------*
       COPY I#BVVEBL.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DO ARQUIVO EARQGRAN '.
      *----------------------------------------------------------------*
       COPY 'I#BVVEBV'.

       01  WRK-BVVEBV-REGISTRO.
           03 WRK-BV-AGENCIA               PIC 9(005) VALUE ZEROS.
           03 WRK-BV-CONTA                 PIC 9(013) VALUE ZEROS.
           03 WRK-BV-CONTRATO              PIC 9(017) VALUE ZEROS.
           03 WRK-BV-CCUSTO                PIC X(004) VALUE SPACES.
           03 WRK-BV-COD-GARANTIA          PIC 9(003) VALUE ZEROS.
           03 WRK-BV-DT-VCTO-FIM           PIC 9(008) VALUE ZEROS.
           03 WRK-BV-VLR-GARNT             PIC 9(013)V99 VALUE 0.
           03 WRK-BV-BAND-BVVE             PIC X(015) VALUE SPACES.
           03 WRK-BV-AGENCIA-CONTRATO      PIC 9(005) VALUE ZEROS.
           03 WRK-BV-CONTA-CONTRATO        PIC 9(013) VALUE ZEROS.
           03 WRK-BV-CARTEIRA              PIC X(003) VALUE SPACES.
           03 WRK-BV-AREAS-RESERVADAS.
              05 WRK-BV-VLR-DCOM           PIC 9(013)V99 VALUE ZEROS.
              05 WRK-BV-DT-INI-CONTR       PIC 9(008) VALUE ZEROS.
7C2511*       05 WRK-BV-CNPJ               PIC 9(009) VALUE ZEROS.
7C2511        05 WRK-BV-CNPJ               PIC X(009) VALUE SPACES.
7C2511*       05 WRK-BV-FILIAL             PIC 9(004) VALUE ZEROS.
7C2511        05 WRK-BV-FILIAL             PIC X(004) VALUE SPACES.
              05 WRK-BV-CTRL               PIC 9(002) VALUE ZEROS.
           03 WRK-BV-SALDO             PIC S9(15)V99    VALUE ZEROS.
           03 WRK-BV-RAZAO             PIC 9(003) VALUE ZEROS.
           03 WRK-BV-SUB-GRUPO         PIC 9(003) VALUE ZEROS.
           03 WRK-BV-VLR-A-BLOQ        PIC 9(013)V99 VALUE ZEROS.
           03 WRK-BV-VLR-BLOQADO       PIC 9(013)V99 VALUE ZEROS.
           03 WRK-BV-COD-BAND          PIC 9(003) VALUE ZEROS.
           03 FILLER                   PIC X(005) VALUE SPACES.

      *    DATA: AAAAMMDD

      *----------------------------------------------------------------*
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
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA BRAD7100 ***'.
      *----------------------------------------------------------------*
       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** BVVE0485 - FIM DA AREA DE WORKING ***'.
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
             UNTIL WRK-FS-EARQGRAN     EQUAL '10' AND
                   WRK-FS-EARQDCOM     EQUAL '10'.

           PERFORM 4000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT EARQDCOM
                      EARQGRAN
               OUTPUT ARQDTFIM .

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.
           PERFORM 1100-TESTAR-FILE-STATUS.

           CALL 'BRAD7600'             USING WRK-DATA-HORA.
           MOVE  CORR WRK-TIMESTAMP    TO WRK-TIMESTAMP-EDIT.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-EARQDCOM

           PERFORM 1120-TESTAR-FS-EARQGRAN.

           PERFORM 1140-TESTAR-FS-ARQDTFIM.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1110-TESTAR-FS-EARQDCOM         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-EARQDCOM         NOT EQUAL   ZEROS
               MOVE 'APL'              TO  ERR-TIPO-ACESSO
               MOVE  WRK-FS-EARQDCOM   TO  WRK-FILE-STATUS
               MOVE 'EARQDCOM'         TO  WRK-NOME-ARQUIVO
               MOVE  WRK-ERRO-ARQUIVO  TO  ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1120-TESTAR-FS-EARQGRAN         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-EARQGRAN         NOT EQUAL   ZEROS
               MOVE 'APL'              TO  ERR-TIPO-ACESSO
               MOVE  WRK-FS-EARQGRAN   TO  WRK-FILE-STATUS
               MOVE 'EARQGRAN'         TO  WRK-NOME-ARQUIVO
               MOVE  WRK-ERRO-ARQUIVO  TO  ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1140-TESTAR-FS-ARQDTFIM         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ARQDTFIM         NOT EQUAL   ZEROS
               MOVE 'APL'              TO  ERR-TIPO-ACESSO
               MOVE  WRK-FS-ARQDTFIM   TO  WRK-FILE-STATUS
               MOVE 'ARQDTFIM'         TO  WRK-NOME-ARQUIVO
               MOVE  WRK-ERRO-ARQUIVO  TO  ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1140-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-LER-EARQDCOM.

           IF  WRK-FS-EARQDCOM         EQUAL '10'
               DISPLAY '**************** BVVE0485 ****************'
               DISPLAY '*                                        *'
               DISPLAY '*         ARQUIVO EARQDCOM VAZIO         *'
               DISPLAY '*                                        *'
               DISPLAY '**************** BVVE0485 ****************'
           END-IF.

           PERFORM 2200-LER-EARQGRAN.

           IF  WRK-FS-EARQGRAN         EQUAL '10'
               DISPLAY '**************** BVVE0485 ****************'
               DISPLAY '*                                        *'
               DISPLAY '*         ARQUIVO EARQGRAN VAZIO         *'
               DISPLAY '*                                        *'
               DISPLAY '**************** BVVE0485 ****************'
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2100-LER-EARQDCOM               SECTION.
      *----------------------------------------------------------------*

           READ EARQDCOM               INTO BVVEBL-REGISTRO.

           IF  WRK-FS-EARQDCOM         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHAVE-EARQDCOM
               GO TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.
           PERFORM 1110-TESTAR-FS-EARQDCOM.

           ADD  1                      TO ACU-LIDOS-EARQDCOM.

           MOVE BVVEBL-AGENCIA         TO WRK-CH-DCOM-AGE
           MOVE BVVEBL-CONTA           TO WRK-CH-DCOM-CONTA
           MOVE BVVEBL-BANDEIRA(1:4)   TO WRK-CH-DCOM-BAND
           MOVE BVVEBL-CNPJ            TO WRK-CH-DCOM-CNPJ
           MOVE BVVEBL-FILIAL          TO WRK-CH-DCOM-FILIAL.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2200-LER-EARQGRAN               SECTION.
      *----------------------------------------------------------------*

           READ EARQGRAN               INTO BVVEBV-REGISTRO.

           IF  WRK-FS-EARQGRAN         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHAVE-EARQGRAN
               GO TO 2200-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.
           PERFORM 1120-TESTAR-FS-EARQGRAN.

           ADD  1                      TO ACU-LIDOS-EARQGRAN.

           MOVE BVVEBV-AGENCIA         TO WRK-CH-GRAN-AGE
           MOVE BVVEBV-CONTA           TO WRK-CH-GRAN-CONTA
           MOVE BVVEBV-BAND-BVVE(1:4)  TO WRK-CH-GRAN-BAND
           MOVE BVVEBV-CNPJ            TO WRK-CH-GRAN-CNPJ
           MOVE BVVEBV-FILIAL          TO WRK-CH-GRAN-FILIAL.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           IF  WRK-CHAVE-EARQGRAN      EQUAL WRK-CHAVE-EARQDCOM
               PERFORM 3100-GRAVAR-ARQDTFIM-IGUAL
               MOVE WRK-CHAVE-EARQDCOM TO WRK-CHAVE-EARQDCOM-ANT
               MOVE WRK-CHAVE-EARQGRAN TO WRK-CHAVE-EARQGRAN-ANT
               PERFORM 2200-LER-EARQGRAN  UNTIL
                   WRK-CHAVE-EARQGRAN NOT EQUAL WRK-CHAVE-EARQGRAN-ANT
               PERFORM 2100-LER-EARQDCOM  UNTIL
                   WRK-CHAVE-EARQDCOM NOT EQUAL WRK-CHAVE-EARQDCOM-ANT
           ELSE
               IF  WRK-CHAVE-EARQGRAN  LESS WRK-CHAVE-EARQDCOM
                   PERFORM 3400-GRAVAR-ARQDTFIM-GRAN
                   MOVE WRK-CHAVE-EARQGRAN     TO WRK-CHAVE-EARQGRAN-ANT
                   PERFORM 2200-LER-EARQGRAN   UNTIL
                   WRK-CHAVE-EARQGRAN NOT EQUAL WRK-CHAVE-EARQGRAN-ANT
               ELSE
                   PERFORM 3300-GRAVAR-ARQDTFIM-DCOM
                   MOVE WRK-CHAVE-EARQDCOM     TO WRK-CHAVE-EARQDCOM-ANT
                   PERFORM 2100-LER-EARQDCOM   UNTIL
                   WRK-CHAVE-EARQDCOM NOT EQUAL WRK-CHAVE-EARQDCOM-ANT
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3100-GRAVAR-ARQDTFIM-IGUAL      SECTION.
      *----------------------------------------------------------------*

           MOVE BVVEBV-REGISTRO        TO WRK-BVVEBV-REGISTRO

           IF  BVVEBV-DT-VCTO-FIM       GREATER BVVEBL-DT-VCTO-PARC
               MOVE BVVEBV-DT-VCTO-FIM  TO WRK-BV-DT-VCTO-FIM
           ELSE
               MOVE BVVEBL-DT-VCTO-PARC TO WRK-BV-DT-VCTO-FIM
           END-IF.

           WRITE FD-ARQDTFIM           FROM WRK-BVVEBV-REGISTRO

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.
           PERFORM 1140-TESTAR-FS-ARQDTFIM.

           ADD 1                       TO ACU-GRAVA-ARQDTFIM.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3300-GRAVAR-ARQDTFIM-DCOM       SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-BVVEBV-REGISTRO.

           MOVE BVVEBL-AGENCIA         TO WRK-BV-AGENCIA
           MOVE BVVEBL-CONTA           TO WRK-BV-CONTA
           MOVE BVVEBL-BANDEIRA        TO WRK-BV-BAND-BVVE
           MOVE BVVEBL-DT-VCTO-PARC    TO WRK-BV-DT-VCTO-FIM
           MOVE BVVEBL-CNPJ            TO WRK-BV-CNPJ
           MOVE BVVEBL-FILIAL          TO WRK-BV-FILIAL
           MOVE BVVEBL-CNPJ-DIGITO     TO WRK-BV-CTRL

           WRITE FD-ARQDTFIM           FROM WRK-BVVEBV-REGISTRO.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.
           PERFORM 1140-TESTAR-FS-ARQDTFIM.

           ADD 1                       TO ACU-GRAVA-ARQDTFIM.

      *----------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3400-GRAVAR-ARQDTFIM-GRAN       SECTION.
      *----------------------------------------------------------------*

           WRITE FD-ARQDTFIM           FROM BVVEBV-REGISTRO

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.
           PERFORM 1140-TESTAR-FS-ARQDTFIM.

           ADD 1                       TO ACU-GRAVA-ARQDTFIM.

      *----------------------------------------------------------------*
       3400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 5000-EMITIR-DISPLAY-FINAL.

           CLOSE   EARQDCOM
                   EARQGRAN
                   ARQDTFIM

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.
           PERFORM 1100-TESTAR-FILE-STATUS

           STOP RUN.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5000-EMITIR-DISPLAY-FINAL       SECTION.
      *----------------------------------------------------------------*

           DISPLAY '******************* BVVE0485 ******************'.
           DISPLAY '*                                             *'.

           MOVE ACU-LIDOS-EARQDCOM     TO WRK-MASCARA.
           DISPLAY '* REGISTROS LIDOS EARQDCOM   :   ' WRK-MASCARA
                                                               '  *'.
           MOVE ACU-LIDOS-EARQGRAN     TO WRK-MASCARA.
           DISPLAY '* REGISTROS LIDOS EARQGRAN   :   ' WRK-MASCARA
                                                               '  *'.
           DISPLAY '*                                             *'.
           MOVE ACU-GRAVA-ARQDTFIM     TO WRK-MASCARA.
           DISPLAY '* REGISTROS GRAVADOS ARQDTFIM:   ' WRK-MASCARA
                                                               '  *'.
           DISPLAY '*                                             *'.
           DISPLAY '******************* BVVE0485 ******************'.

      *----------------------------------------------------------------*
       5000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'BVVE0485'             TO ERR-PGM.

           CALL 'BRAD7100'             USING WRK-BATCH
                                             ERRO-AREA.

           GOBACK.

      *---------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *---------------------------------------------------------------*
