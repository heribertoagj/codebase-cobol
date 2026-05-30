*================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
      * FAZER O TOMBMENTO DO CB.BVVE.S104243.RELBACK(0),
       PROGRAM-ID.     BVVEA086.
       AUTHOR.         FRANCISMARA.
      *================================================================*
      *                         W I P R O                              *
      *----------------------------------------------------------------*
      *    ANALISTA....: FRANCISMARA PAGNONCELLI -WIPRO                *
      *    DATA........: FEVEREIRO/2021                                *
      *----------------------------------------------------------------*
      *    OBJETIVO....: FAZER DE-PARA I#BVVEBU PARA I#BVVENZ          *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME        IN/OUT      INCLUDE               *
      *                ELAYOUBU      INPUT       I#BVVEBU              *
      *                SLAYOUNZ      OUTPUT      I#BVVENZ              *
      *----------------------------------------------------------------*
      *    INC'S.......:                                              *
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

           SELECT  ELAYOUBU    ASSIGN  TO          UT-S-ELAYOUBU
                       FILE    STATUS  IS          WRK-FS-ELAYOUBU.

           SELECT  SLAYOUNZ    ASSIGN  TO          UT-S-SLAYOUNZ
                       FILE    STATUS  IS          WRK-FS-SLAYOUNZ.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT : ARQUIVO LAYOUT I#BVVEBU                             *
      *----------------------------------------------------------------*
       FD  ELAYOUBU
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-ELAYOUBU                 PIC  X(700).

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO LAYOUT I#BVVENZ                             *
      *----------------------------------------------------------------*
       FD  SLAYOUNZ
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-SLAYOUNZ                 PIC  X(700).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)          VALUE
           '*** BVVEA086 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** ACUMULADORES ***'.
      *----------------------------------------------------------------*
       77  ACU-LIDOS-ELAYOUBU          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-SLAYOUNZ          PIC  9(009) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREAS DE FILE-STATUS ***'.
      *----------------------------------------------------------------*
       77  WRK-FS-ELAYOUBU             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-SLAYOUNZ             PIC  X(002)         VALUE SPACES.

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
       77  WRK-VALOR                   PIC  9(015)V9(002)  VALUE ZEROS.
       77  WRK-MASCARA                 PIC  ZZZ.ZZZ.ZZ9    VALUE ZEROS.

      *01 _WRK-DATA-DB2.
      *   _05  WRK-DIA                 PIC  9(02)          VALUE ZEROS.
      *   _05  FILLER                  PIC  X(01)          VALUE SPACES.
      *   _05  WRK-MES                 PIC  9(02)          VALUE ZEROS.
      *   _05  FILLER                  PIC  X(01)          VALUE SPACES.
      *   _05  WRK-ANO                 PIC  9(04)          VALUE ZEROS.

       01  WRK-DATA-DMA.
           05  WRK-DIA                 PIC  9(02)          VALUE ZEROS.
           05  WRK-MES                 PIC  9(02)          VALUE ZEROS.
           05  WRK-ANO                 PIC  9(04)          VALUE ZEROS.
       01  WRK-DATA-DMA-R REDEFINES
           WRK-DATA-DMA                PIC  9(08).

       01  WRK-DATA-INV.
           05  WRK-ANO                 PIC  9(04)          VALUE ZEROS.
           05  WRK-MES                 PIC  9(02)          VALUE ZEROS.
           05  WRK-DIA                 PIC  9(02)          VALUE ZEROS.
       01  WRK-DATA-INV-R REDEFINES
           WRK-DATA-INV                PIC  9(08).

      *01 _WRK-ANO-MES-AGNDA.
      *   _05  WRK-ANO                 PIC  9(04)          VALUE ZEROS.
      *   _05  WRK-MES                 PIC  9(02)          VALUE ZEROS.
      *01 _WRK-ANO-MES-AGNDA-R REDEFINES
      *   _WRK-ANO-MES-AGNDA           PIC  9(06).
      *
      *01 _WRK-VLR-S15V99              PIC +9(15)V99       VALUE ZEROS.
      *01 _FILLER                REDEFINES  WRK-VLR-S15V99.
      *   _05  FILLER                  PIC  X(03).
      *   _05  WRK-VLR-13V99           PIC  9(13)V99.
      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(080)         VALUE
           '*** AREA DO ARQUIVO ELAYOUBU *'.
      *----------------------------------------------------------------*

            COPY 'I#BVVEBU'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(055)         VALUE
           '*** AREA DO ARQUIVO SLAYOUNZ***'.
      *----------------------------------------------------------------*

            COPY 'I#BVVENZ'.

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
      *    AREA PARA POOL7600
      *
       01  WRK-DATA-HORA.
           10  WRK-DT-JULIANA          PIC 9(005) COMP-3 VALUE ZEROS.
           10  WRK-DT-AAMMDD           PIC 9(007) COMP-3 VALUE ZEROS.
           10  WRK-DT-SSAAMMDD         PIC 9(009) COMP-3 VALUE ZEROS.
           10  WRK-DT-HHMMSS           PIC 9(007) COMP-3 VALUE ZEROS.
           10  WRK-DT-HHMMSSMMMMMM
                                       PIC 9(013) COMP-3 VALUE ZEROS.
           10  WRK-DT-TIMESTAMP        PIC X(020)        VALUE SPACES.
           10  FILLER            REDEFINES WRK-DT-TIMESTAMP.
               15  WRK-TIME-ANO        PIC  9(004).
               15  WRK-TIME-MES        PIC  9(002).
               15  WRK-TIME-DIA        PIC  9(002).
               15  WRK-TIME-HH         PIC  9(002).
               15  WRK-TIME-MM         PIC  9(002).
               15  WRK-TIME-SS         PIC  9(002).
               15  WRK-TIME-MMMMMM     PIC  9(006).
      *
       01  WRK-DT-AAAAMM               PIC  9(006)       VALUE ZEROS.
       01  FILLER                REDEFINES WRK-DT-AAAAMM.
           05  WRK-DT-AAAA             PIC  9(004).
           05  WRK-DT-MM               PIC  9(002).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA BRAD7100 ***'.
      *----------------------------------------------------------------*
       COPY 'I#BRAD7C'.


      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** BVVEA086 - FIM DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE  DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR

           PERFORM 2000-VERIFICAR-VAZIO

           PERFORM 3000-PROCESSAR
             UNTIL WRK-FS-ELAYOUBU     EQUAL '10'

           PERFORM 4000-FINALIZAR
           .
      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT ELAYOUBU
               OUTPUT SLAYOUNZ

           MOVE WRK-ABERTURA           TO WRK-OPERACAO
           PERFORM 1100-TESTAR-FILE-STATUS

           INITIALIZE WRK-DATA-HORA
           CALL 'POOL7600'          USING WRK-DATA-HORA
           MOVE WRK-TIME-ANO           TO WRK-DT-AAAA
           MOVE WRK-TIME-MES           TO WRK-DT-MM
           .
      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-ELAYOUBU

           PERFORM 1120-TESTAR-FS-SLAYOUNZ
           .
      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1110-TESTAR-FS-ELAYOUBU         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ELAYOUBU         NOT EQUAL   ZEROS
               MOVE 'APL'              TO  ERR-TIPO-ACESSO
               MOVE  WRK-FS-ELAYOUBU   TO  WRK-FILE-STATUS
               MOVE 'ELAYOUBU'         TO  WRK-NOME-ARQUIVO
               MOVE  WRK-ERRO-ARQUIVO  TO  ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF
           .
      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1120-TESTAR-FS-SLAYOUNZ         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SLAYOUNZ         NOT EQUAL   ZEROS
               MOVE 'APL'              TO  ERR-TIPO-ACESSO
               MOVE  WRK-FS-SLAYOUNZ   TO  WRK-FILE-STATUS
               MOVE 'SLAYOUNZ'         TO  WRK-NOME-ARQUIVO
               MOVE  WRK-ERRO-ARQUIVO  TO  ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF
           .
      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-LER-ELAYOUBU

           IF  WRK-FS-ELAYOUBU         EQUAL '10'
               DISPLAY '**************** BVVEA086 ****************'
               DISPLAY '*                                        *'
               DISPLAY '*         ARQUIVO ELAYOUBU VAZIO          *'
               DISPLAY '*                                        *'
               DISPLAY '**************** BVVEA086 ****************'
           END-IF
           .
      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2100-LER-ELAYOUBU               SECTION.
      *----------------------------------------------------------------*

           READ ELAYOUBU               INTO BVVEBU-REGISTRO

           IF  WRK-FS-ELAYOUBU         EQUAL '10'
               GO TO 2100-99-FIM
           END-IF

           MOVE WRK-LEITURA            TO WRK-OPERACAO
           PERFORM 1110-TESTAR-FS-ELAYOUBU

           ADD  1                      TO ACU-LIDOS-ELAYOUBU
           .
      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 3100-GRAVAR-SLAYOUNZ

           PERFORM 2100-LER-ELAYOUBU
           .
      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3100-GRAVAR-SLAYOUNZ           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE BVVENZ-REGISTRO

           MOVE BVVEBU-DET-CENTRO-CUS TO  BVVENZ-CCUSTO

           IF  BVVEBU-DET-TP-PSSOA    EQUAL 'J'
               MOVE BVVEBU-DET-CNPJN  TO  BVVENZ-CNPJ-CPF
               MOVE BVVEBU-DET-FILIAL TO  BVVENZ-FILIAL
               MOVE BVVEBU-DET-DIGCN  TO  BVVENZ-DIGITO
           ELSE
               MOVE BVVEBU-DET-CPFN   TO  BVVENZ-CNPJ-CPF
7C2511         MOVE SPACES             TO  BVVENZ-FILIAL
               MOVE BVVEBU-DET-DIG    TO  BVVENZ-DIGITO
           END-IF.

           MOVE BVVEBU-DET-AGEN       TO  BVVENZ-AGENCIA
           MOVE BVVEBU-DET-CONTAN     TO  BVVENZ-CONTA

           MOVE BVVEBU-DET-AG-CTR     TO  BVVENZ-AG-CONTR
           MOVE BVVEBU-DET-CONTA-CTR  TO  BVVENZ-CT-CONTR
           MOVE BVVEBU-DET-CART-CTR   TO  BVVENZ-CARTEIRA

           IF   BVVEBU-DET-CONTRATO         IS NUMERIC AND
                BVVEBU-DET-CONTRATO         NOT EQUAL ZEROS
                MOVE BVVEBU-DET-CONTRATO    TO BVVENZ-CONTR-LEGADO
                MOVE BVVEBU-DET-CONTRATO-X  TO BVVENZ-CONTRATO-LOTE
                MOVE '1'                    TO BVVENZ-TP-CONTRATO
           ELSE
                MOVE '2'                    TO BVVENZ-TP-CONTRATO
                MOVE BVVEBU-DET-CPSSOA-JURID-CTR
                                            TO BVVENZ-CONTR-CPSSOA
                MOVE BVVEBU-DET-CTPO-CTR    TO BVVENZ-CONTR-CTPO
                MOVE BVVEBU-DET-NSEQ-CTR    TO BVVENZ-CONTR-NSEQ
           END-IF.

           MOVE BVVEBU-DET-CATALOGO    TO BVVENZ-COD-GARANTIA
           MOVE BVVEBU-DET-CBANDE      TO BVVENZ-BANDEIRA
           MOVE BVVEBU-DET-DT-INI-CTR  TO WRK-DATA-DMA-R
           MOVE CORR WRK-DATA-DMA      TO WRK-DATA-INV
           MOVE WRK-DATA-INV-R         TO BVVENZ-DT-INI-GRAV
           MOVE BVVEBU-DET-DT-VTO-CTR  TO WRK-DATA-DMA-R
           MOVE CORR WRK-DATA-DMA      TO WRK-DATA-INV
           MOVE WRK-DATA-INV-R         TO BVVENZ-DT-FIM-GRAV
                                          BVVENZ-DT-FIM-CONTR

           IF   BVVEBU-DET-VR-EXGDO    IS NUMERIC
                MOVE BVVEBU-DET-VR-EXGDO
                                       TO BVVENZ-VLR-EXIGIDO
           END-IF.

           IF  BVVEBU-DET-IND-VRLOR     IS NUMERIC AND
              (BVVEBU-DET-IND-VRLOR     EQUAL 1 OR 2)
               IF  BVVEBU-DET-IND-VRLOR EQUAL 1
                   IF  BVVEBU-DET-SLDO-DEVD NOT NUMERIC OR
                      (BVVEBU-DET-SLDO-DEVD EQUAL ZEROS AND
                      (BVVEBU-DET-CENTRO-CUS    NOT EQUAL 'EMPG' AND
                       BVVEBU-DET-CENTRO-CUS    NOT EQUAL 'CROT'))
                       MOVE ZEROS                TO WRK-VALOR
                   ELSE
                       MOVE BVVEBU-DET-SLDO-DEVD TO WRK-VALOR
                   END-IF
               ELSE
                   IF  BVVEBU-DET-VR-LIMITE  NOT NUMERIC OR
                      (BVVEBU-DET-VR-LIMITE  EQUAL ZEROS AND
                      (BVVEBU-DET-CENTRO-CUS NOT EQUAL 'EMPG' AND
                       BVVEBU-DET-CENTRO-CUS NOT EQUAL 'CROT'))
                       MOVE ZEROS            TO WRK-VALOR
                   ELSE
                       MOVE BVVEBU-DET-VR-LIMITE TO WRK-VALOR
                   END-IF
               END-IF
           ELSE
               IF  BVVEBU-DET-VR-EXGDO     NOT NUMERIC OR
                  (BVVEBU-DET-VR-EXGDO     EQUAL ZEROS AND
                  (BVVEBU-DET-CENTRO-CUS   NOT EQUAL 'EMPG' AND
                   BVVEBU-DET-CENTRO-CUS   NOT EQUAL 'CROT'))
                   MOVE ZEROS              TO WRK-VALOR
               ELSE
                   MOVE BVVEBU-DET-VR-EXGDO TO WRK-VALOR
               END-IF
           END-IF.

           MOVE WRK-VALOR              TO BVVENZ-VLR-TOT-CONTR

           WRITE FD-SLAYOUNZ           FROM BVVENZ-REGISTRO

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO
           PERFORM 1120-TESTAR-FS-SLAYOUNZ

           ADD 1                       TO ACU-GRAVA-SLAYOUNZ
           .
      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 5000-EMITIR-DISPLAY-FINAL

           CLOSE   ELAYOUBU
                   SLAYOUNZ

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO
           PERFORM 1100-TESTAR-FILE-STATUS

           STOP RUN
           .
      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5000-EMITIR-DISPLAY-FINAL       SECTION.
      *----------------------------------------------------------------*

           DISPLAY '******************* BVVEA086 ******************'.
           DISPLAY '*                                             *'.

           MOVE ACU-LIDOS-ELAYOUBU     TO WRK-MASCARA.
           DISPLAY '* REGISTROS LIDOS ELAYOUBU   :   ' WRK-MASCARA
                                                              '   *'.
           DISPLAY '*                                             *'.
           MOVE ACU-GRAVA-SLAYOUNZ     TO WRK-MASCARA.
           DISPLAY '* REGISTROS GRAVADOS SLAYOUNZ:   ' WRK-MASCARA
                                                              '   *'.
           DISPLAY '*                                             *'.
           DISPLAY '******************* BVVEA086 ******************'.

      *----------------------------------------------------------------*
       5000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'BVVEA086'             TO ERR-PGM.

           CALL 'BRAD7100'             USING WRK-BATCH
                                             ERRO-AREA

           GOBACK.

      *---------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *---------------------------------------------------------------*
