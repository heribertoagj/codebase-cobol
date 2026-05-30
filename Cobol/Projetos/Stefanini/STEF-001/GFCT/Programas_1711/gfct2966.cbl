      *===============================================================*
       IDENTIFICATION DIVISION.
      *===============================================================*
       PROGRAM-ID. GFCT2966.
       AUTHOR.     FERNANDO PINA.
      *===============================================================*
      *                    G P T I - S I S T E M A S                  *
      *---------------------------------------------------------------*
      *      PROGRAMA     : GFCT2966                                  *
      *      PROGRAMADOR  : FERNANDO PINA                             *
      *      ANALISTA     : EDMUNDO                                   *
      *      DATA         : 19/10/2009                                *
      *---------------------------------------------------------------*
      *                                                               *
      *      OBJETIVO     : EFETUAR GERACAO DO RELATORIO PARA RATEIO  *
      *                     DO CUSTO DE POSTAGEM POR AGENCIA NO EXTRA *
      *                     TO ANUAL DE TARIFAS - SPLIT POR AGENCIA   *
      *---------------------------------------------------------------*
      *      ARQUIVOS:                                                *
      *         DDNAME                           INCLUDE/BOOK         *
      *         RATEICLI                           GFCTWAEC           *
      *         ARQNTAX                            I#NTAX02           *
      *         RELAJUP                            I#AJAG04           *
      *===============================================================*
      *                    M A N U T E N C O E S                      *
      *===============================================================*
      *      PROGRAMADOR : XXXXXXXX -  GPTI                           *
      *      ANALISTA    : XXXXXXXX -  GPTI                           *
      *      DATA        : XX/XX/XXXX                                 *
      *---------------------------------------------------------------*
      *      OBJETIVO    : XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
      *                    XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX. *
      *===============================================================*

      *===============================================================*
       ENVIRONMENT DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       CONFIGURATION SECTION.
      *---------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *---------------------------------------------------------------*
       INPUT-OUTPUT SECTION.
      *---------------------------------------------------------------*

       FILE-CONTROL.

           SELECT RATEICLI ASSIGN TO UT-S-RATEICLI
                      FILE STATUS IS WRK-FS-RATEICLI.

           SELECT ARQNTAX  ASSIGN TO UT-S-ARQNTAX
                      FILE STATUS IS WRK-FS-ARQNTAX.

           SELECT RELAJUP  ASSIGN TO UT-S-RELAJUP
                      FILE STATUS IS WRK-FS-RELAJUP.

      *===============================================================*
       DATA DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       FILE SECTION.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      *    INPUT:     ORG. SEQUENCIAL   -   LRECL = 781 - FB          *
      *---------------------------------------------------------------*

       FD  RATEICLI
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY GFCTWAEC.

      *---------------------------------------------------------------*
      *    INPUT:     ORG. SEQUENCIAL   -   LRECL = 060               *
      *---------------------------------------------------------------*

       FD  ARQNTAX
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY I#NTAX02.

      *---------------------------------------------------------------*
      *    OUTPUT:    ORG. SEQUENCIAL   -   LRECL = 080               *
      *---------------------------------------------------------------*

       FD  RELAJUP
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-RELAJUP                      PIC X(080).

      *---------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *---------------------------------------------------------------*

       77  FILLER                      PIC X(32)        VALUE
                   '* INICIO DA WORKING GFCT2966 *'.

       77  WRK-ABEND                   PIC S9(04) COMP   VALUE +1111.
       77  WRK-AUX-04                  PIC  9(04) COMP-3 VALUE ZEROS.
       77  WRK-QUEBRA                  PIC  X(01)        VALUE 'N'.

      *---------------------------------------------------------------*
      *          AREAS AUXILIARES PARA VERIFICACAO DOS FILE-STATUS    *
      *---------------------------------------------------------------*

       77  WRK-FS-RATEICLI             PIC X(02)        VALUE  SPACES.
       77  WRK-FS-ARQNTAX              PIC X(02)        VALUE  SPACES.
       77  WRK-FS-RELAJUP              PIC X(02)        VALUE  SPACES.
       77  WRK-FIM-ARQNTAX             PIC X(01)        VALUE  SPACES.
       77  WRK-OPERACAO                PIC X(13) VALUE  SPACES.
       77  WRK-ABERTURA                PIC X(13) VALUE  'NA ABERTURA'.
       77  WRK-LEITURA                 PIC X(13) VALUE  'NA LEITURA'.
       77  WRK-GRAVACAO                PIC X(13) VALUE  'NA GRAVACAO'.
       77  WRK-FECHAMENTO              PIC X(13) VALUE  'NO FECHAMENTO'.

       01  WRK-POOL1260.
           05  WRK-1260-AGENCIA         PIC 9(005) COMP-3 VALUE ZEROS.
           05  WRK-1260-DIGITO          PIC X(001)        VALUE SPACES.
           05  WRK-1260-TAMANHO         PIC 9(002)        VALUE 3.

      *---------------------------------------------------------------*
      *          AREAS AUXILIARES PARA CHAVES                         *
      *---------------------------------------------------------------*

       77  WRK-AGENCIA-ATU             PIC 9(005) VALUE ZEROS.
       77  WRK-DIGITO-ATU              PIC X(001) VALUE SPACES.
       77  WRK-DISTR-ATU               PIC X(002) VALUE SPACES.

       77  WRK-AGENCIA-ANT             PIC 9(005) VALUE ZEROS.
       77  WRK-DIGITO-ANT              PIC X(001) VALUE SPACES.
       77  WRK-DISTR-ANT               PIC X(002) VALUE SPACES.

      *---------------------------------------------------------------*
      *          AREAS AUXILIARES PARA ACUMULADORES                   *
      *---------------------------------------------------------------*

       77  WRK-ACU-NTAX              PIC  9(09)    COMP-3 VALUE ZEROS.
       77  WRK-ACU-LIDOS             PIC  9(09)    COMP-3 VALUE ZEROS.
       77  WRK-ACU-IMPRESSOS         PIC  9(09)    COMP-3 VALUE ZEROS.
       77  WRK-ACU-GRAVADOS          PIC  9(09)    COMP-3 VALUE ZEROS.
       77  WRK-ACU-VALOR-IMPRE       PIC  9(13)V9999 COMP-3 VALUE ZEROS.
       77  WRK-ACU-VALOR-POSTA       PIC  9(13)V9999 COMP-3 VALUE ZEROS.
       77  WRK-TOT-VALOR-IMPRE       PIC  9(13)V9999 COMP-3 VALUE ZEROS.
       77  WRK-TOT-VALOR-POSTA       PIC  9(13)V9999 COMP-3 VALUE ZEROS.
       77  WRK-TOT-VALOR-GERAL       PIC  9(13)V9999 COMP-3 VALUE ZEROS.
       77  WRK-TOT-VLR-GER           PIC  9(13)V9999 COMP-3 VALUE ZEROS.
       77  WRK-TOT-VALOR-AUX         PIC  9(13)V9999 COMP-3 VALUE ZEROS.
       77  WRK-TOT-CLIENTE           PIC  9(15)    COMP-3 VALUE ZEROS.
       77  WRK-TOT-CLI               PIC  9(15)    COMP-3 VALUE ZEROS.
       77  WRK-ACU-CLIENTE           PIC  9(15)    COMP-3 VALUE ZEROS.
 H0502 77  WRK-ACU-PAG               PIC  9(03)  COMP-3  VALUE  ZEROS.
 H0502 77  WRK-ACU-LIN               PIC  9(03)  COMP-3  VALUE  99.
       01  WRK-VR-ATU-POSTA          PIC  9(13)     COMP-3 VALUE ZEROS.
       01  WRK-VR-ATU-POSTA-R        REDEFINES
                  WRK-VR-ATU-POSTA   PIC  9(05)V9(8) COMP-3.
       01  WRK-VR-ATU-IMPRE          PIC  9(13)     COMP-3 VALUE ZEROS.
       01  WRK-VR-ATU-IMPRE-R        REDEFINES
                  WRK-VR-ATU-IMPRE   PIC 9(05)V9(8) COMP-3.

H0602  01  WRK-AUX-CLIE15            PIC  9(15)       VALUE ZEROS.
H0602  01  FILLER REDEFINES          WRK-AUX-CLIE15.
H0602      03  FILLER                PIC  9(08).
H0602      03  WRK-AUX-CLIE7         PIC  9(07).
H0602  01  FILLER REDEFINES          WRK-AUX-CLIE15.
H0602      03  FILLER                PIC  9(07).
H0602      03  WRK-AUX-CLIE8         PIC  9(08).
H0602  01  FILLER REDEFINES          WRK-AUX-CLIE15.
H0602      03  FILLER                PIC  9(06).
H0602      03  WRK-AUX-CLIE9         PIC  9(09).
H0602  01  FILLER REDEFINES          WRK-AUX-CLIE15.
H0602      03  FILLER                PIC  9(03).
H0602      03  WRK-AUX-CLIE12        PIC  9(12).

H0602  01  WRK-AUX-VALR09 PIC  9(06)V9(03) VALUE ZEROS.
H0602  01  FILLER REDEFINES          WRK-AUX-VALR09.
H0602      03  FILLER                PIC  9(01).
H0602      03  WRK-AUX-VALR5         PIC  9(05).
H0602      03  WRK-AUX-VALR3         PIC  9(03).

H0602  01  WRK-AUX-VALO13 PIC  9(05)V9(08) VALUE ZEROS.
H0602  01  FILLER REDEFINES          WRK-AUX-VALO13.
H0602      03  WRK-AUX-VALO5         PIC  9(05).
H0602      03  WRK-AUX-VALO3         PIC  9(03).
H0602      03  FILLER  REDEFINES     WRK-AUX-VALO3.
H0602          05  WRK-AUX-VLOR2     PIC  9(02).
H0602          05  FILLER            PIC  9(01).
H0602      03  FILLER                PIC  9(05).

H0602  01  WRK-AUX-VAJ17 PIC  9(15)V9(02) VALUE ZEROS.
H0602  01  FILLER REDEFINES          WRK-AUX-VAJ17.
H0602      03  FILLER                PIC  9(02).
H0602      03  WRK-AUX-VAJ17-13      PIC  9(13).
H0602      03  WRK-AUX-VAJ17-02      PIC  9(02).

H0602  01  WRK-AUX-VAL17 PIC  9(13)V9(04) VALUE ZEROS.
H0602  01  FILLER REDEFINES          WRK-AUX-VAL17.
H0602      03  WRK-AUX-VAL17-13      PIC  9(13).
H0602      03  WRK-AUX-VAL17-02      PIC  9(02).
H0602      03  FILLER                PIC  9(02).
H0602  01      FILLER  REDEFINES     WRK-AUX-VAL17.
H0602          05  FILLER            PIC  9(04).
H0602          05  WRK-AUX-VAL17-12  PIC  9(09)V9(03).
H0602          05  FILLER            PIC  9(01).
H0602  01      FILLER  REDEFINES     WRK-AUX-VAL17.
H0602          05  FILLER            PIC  9(03).
H0602          05  WRK-AUX-VLOR17-13 PIC  9(10)V9(03).
H0602          05  FILLER            PIC  9(01).
H0602  01      FILLER  REDEFINES     WRK-AUX-VAL17.
H0602          05  FILLER            PIC  9(02).
H0602          05  WRK-AUX-VAL17-14  PIC  9(11)V9(03).
H0602          05  FILLER            PIC  9(01).

      *----------------------------------------------------------------*
      *            AREA DA INC I#AJAG04
      *----------------------------------------------------------------*

       COPY I#AJAG04.

      *----------------------------------------------------------------*
      *            AREA DISPLAY DE ESTATISTICA                         *
      *----------------------------------------------------------------*

       COPY GFCTWAD0.

      *----------------------------------------------------------------*
      *            AREA DA INC DE USO GERAL AJUP                       *
      *----------------------------------------------------------------*

       COPY GFCTWAED.

      *---------------------------------------------------------------*
      *          AREAS AUXILIARES PARA DATA                           *
      *---------------------------------------------------------------*

       01  WRK-DATA-1                  PIC 9(008)     VALUE ZEROS.
       01  FILLER                      REDEFINES      WRK-DATA-1.
           03  WRK-AAAA                PIC 9(004).
           03  WRK-MM                  PIC 9(002).
           03  WRK-DD                  PIC 9(002).

       01  WRK-AUX-DATA09              PIC 9(009) VALUE ZEROS.
       01  FILLER   REDEFINES   WRK-AUX-DATA09.
           03  FILLER          PIC 9(001).
           03  WRK-AUX-DATA    PIC 9(008).

      *---------------------------------------------------------------*
      *          AREAS AUXILIARES PARA DATA MOVTO AJUP                *
      *---------------------------------------------------------------*

       01  WRK-DATA-AJUP.
           03  WRK-AAAA-AJUP           PIC 9(004)  VALUE ZEROS.
           03  WRK-MM-AJUP             PIC 9(002)  VALUE ZEROS.
           03  WRK-DD-AJUP             PIC 9(002)  VALUE ZEROS.
       01  WRK-DATA-AJAG-R REDEFINES WRK-DATA-AJUP PIC 9(8).

      *---------------------------------------------------------------*
      *          AREAS AUXILIARES PARA HORA                           *
      *---------------------------------------------------------------*

       01  WRK-HORA                    PIC 9(006)     VALUE ZEROS.
       01  WRK-HORA-R                  REDEFINES      WRK-HORA.
           03  WRK-HORAS               PIC 9(002).
           03  WRK-MINUTOS             PIC 9(002).
           03  WRK-SEGUNDOS            PIC 9(002).

       01  WRK-AUX-HORA07              PIC 9(007) VALUE ZEROS.
       01  FILLER   REDEFINES   WRK-AUX-HORA07.
           03  FILLER          PIC 9(001).
           03  WRK-AUX-HORA    PIC 9(006).

      *----------------------------------------------------------------*
      *       AREA AUXILIAR  VARIAVEIS --> TAX2-DT-INIC / TAX2-DT-FIM  *
      *----------------------------------------------------------------*

       01  WRK-AUX-TAXI        PIC 9(09) VALUE ZEROS.
       01  FILLER  REDEFINES   WRK-AUX-TAXI.
           03  FILLER              PIC 9(01).
           03  WRK-AUX-TAXI-INIC   PIC 9(08).

       01  WRK-AUX-TAXF        PIC 9(09) VALUE ZEROS.
       01  FILLER  REDEFINES   WRK-AUX-TAXF.
           03  FILLER              PIC 9(01).
           03  WRK-AUX-TAXI-FIM    PIC 9(08).

      *---------------------------------------------------------------*
      *  AREAS AUXILIARES PARA OBTENCAO DO VALOR DO FAC.              *
      *---------------------------------------------------------------*

 ******77  WRK-JOB                  PIC X(08)            VALUE SPACES.
       77  WRK-VALORFAC-POSTA       PIC 9(04)V999 COMP-3 VALUE ZEROS.
       77  WRK-VALORFAC-IMPRE       PIC 9(04)V999 COMP-3 VALUE ZEROS.
       77  WRK-DEBCRED              PIC 9(01)            VALUE ZEROS.
       77  WRK-AJUP                 PIC 9(03)            VALUE ZEROS.

      *---------------------------------------------------------------*
      *  AREAS AUXILIARES PARA CABEC00 - SPLIT AGENCIA                *
      *---------------------------------------------------------------*

       01  WRK-CABEC00.
           03  WRK-CB0-CARRO       PIC X(01)            VALUE '0'.
           03  FILLER              PIC X(02)            VALUE  SPACES.
           03  WRK-CB0-AGENCIA     PIC 9(05)            VALUE  ZEROS.
           03  FILLER              PIC X(72)            VALUE  SPACES.

       01  FILLER                      PIC X(32)        VALUE
           '*  FIM DA WORKING GFCT2966 *'.
           EJECT

      *---------------------------------------------------------------*
       LINKAGE                    SECTION.
      *---------------------------------------------------------------*

       01 LNK-PARAMETRO.
          03 LNK-TAMANHO           PIC S9(04) COMP.
          03 LNK-CCUSTO            PIC  X(04).

      *===============================================================*
       PROCEDURE DIVISION   USING LNK-PARAMETRO.
      *===============================================================*

      *---------------------------------------------------------------*
       0000-INICIO-PROCESSAMENTO SECTION.
      *---------------------------------------------------------------*

           PERFORM 1000-INICIO.

           PERFORM 2000-PROCESSA UNTIL WRK-FS-RATEICLI EQUAL '10'.

           PERFORM 3000-FINALIZA.

      *---------------------------------------------------------------*
       0000-99-FIM.  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1000-INICIO SECTION.
      *---------------------------------------------------------------*

           OPEN INPUT   RATEICLI
                        ARQNTAX
                OUTPUT  RELAJUP.

           PERFORM 1400-OBTER-DATA-HORA.

           MOVE  'GFCT2966'            TO  WRK-PROGNAME
                                           CB1-PGM.

           MOVE    WRK-ABERTURA        TO    WRK-OPERACAO.
           PERFORM 1100-TESTAR-FILE-STATUS.

           PERFORM 1200-TESTA-RATEICLI-VAZIO.

           PERFORM 1220-LER-ARQNTAX.

           IF ((WRK-FIM-ARQNTAX  EQUAL 'S')      AND
               (WRK-VR-ATU-POSTA EQUAL  ZEROS  OR
                WRK-VR-ATU-IMPRE EQUAL  ZEROS))
                DISPLAY '****** PROGRAMA GFCT2966 CANCELADO ******'
                DISPLAY '*  REGISTRTO NAO ENCONTRADO NO ARQUIVO  *'
                DISPLAY '*       NTAX (1893 - POSTAGEM  OU       *'
                DISPLAY '*             4278 - IMPRESSAO)         *'
                DISPLAY '* > DATA DO PROCESSAMENTO ' WRK-DATA-1   '*'
                DISPLAY '*****************************************'
                CALL 'ILBOABN0'     USING WRK-ABEND
           ELSE
H0602           MOVE  WRK-VR-ATU-POSTA-R  TO  WRK-AUX-VALO13
H0602           MOVE  ZEROS               TO  WRK-AUX-VALR09
H0602           MOVE  WRK-AUX-VALO5       TO  WRK-AUX-VALR5
H0602           MOVE  WRK-AUX-VALO3       TO  WRK-AUX-VALR3
H0602           MOVE  WRK-AUX-VALR09      TO  WRK-TT2-VALOR

H0602           MOVE  WRK-VR-ATU-IMPRE-R  TO  WRK-AUX-VALO13
H0602           MOVE  ZEROS               TO  WRK-AUX-VALR09
H0602           MOVE  WRK-AUX-VALO5       TO  WRK-AUX-VALR5
H0602           MOVE  WRK-AUX-VALO3       TO  WRK-AUX-VALR3
H0602           MOVE  WRK-AUX-VALR09      TO  WRK-TT3-VALOR

           END-IF.

           PERFORM  1300-TRATA-TPO-DISTR.

           MOVE  WRK-AGENCIA-ATU       TO  WRK-AGENCIA-ANT.
           MOVE  WRK-DIGITO-ATU        TO  WRK-DIGITO-ANT.

      *---------------------------------------------------------------*
       1000-99-FIM.  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS SECTION.
      *---------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-RATEICLI.

           PERFORM 1130-TESTAR-FS-RELAJUP.

           PERFORM 1140-TESTAR-FS-ARQNTAX.

      *---------------------------------------------------------------*
       1100-99-FIM.  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1110-TESTAR-FS-RATEICLI SECTION.
      *---------------------------------------------------------------*

           IF WRK-FS-RATEICLI NOT EQUAL '00'
              DISPLAY '************** GFCT2966 *************'
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
              DISPLAY '*             RATEICLI              *'
              DISPLAY '*         FILE STATUS =  ' WRK-FS-RATEICLI
                                                 '         *'
              DISPLAY '************** GFCT2966 *************'
              CALL 'ILBOABN0'     USING WRK-ABEND
           END-IF.

      *---------------------------------------------------------------*
       1110-99-FIM.  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1130-TESTAR-FS-RELAJUP  SECTION.
      *---------------------------------------------------------------*

           IF WRK-FS-RELAJUP  NOT EQUAL '00'
              DISPLAY '************** GFCT2966 *************'
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
              DISPLAY '*            RELAJUP                *'
              DISPLAY '*         FILE STATUS =  ' WRK-FS-RELAJUP
                                                 '         *'
              DISPLAY '************** GFCT2966 *************'
              CALL 'ILBOABN0'     USING WRK-ABEND
           END-IF.

      *---------------------------------------------------------------*
       1130-99-FIM.  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1140-TESTAR-FS-ARQNTAX  SECTION.
      *---------------------------------------------------------------*

           IF WRK-FS-ARQNTAX  NOT EQUAL '00'
              DISPLAY '************** GFCT2966 *************'
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
              DISPLAY '*            ARQNTAX                *'
              DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQNTAX
                                                 '         *'
              DISPLAY '************** GFCT2966 *************'
              CALL 'ILBOABN0'     USING WRK-ABEND
           END-IF.

      *---------------------------------------------------------------*
       1140-99-FIM.  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1200-TESTA-RATEICLI-VAZIO  SECTION.
      *---------------------------------------------------------------*

           PERFORM  1210-LER-RATEICLI.

           IF       WRK-FS-RATEICLI     EQUAL   '10'
                    DISPLAY '************* GFCT2966 *************'
                    DISPLAY '*                                  *'
                    DISPLAY '*         SR. OPERADOR             *'
                    DISPLAY '*  O ARQUIVO RATEICLI ESTA VAZIO   *'
                    DISPLAY '*  O PROCESSAMENTO SERA ENCERRADO. *'
                    DISPLAY '*                                  *'
                    DISPLAY '************** GFCT2966 ************'
                    PERFORM  3000-FINALIZA
           END-IF.

      *---------------------------------------------------------------*
       1200-99-FIM.  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1210-LER-RATEICLI SECTION.
      *---------------------------------------------------------------*

           READ    RATEICLI.

           IF      WRK-FS-RATEICLI         EQUAL      '10'
                   GO  TO 1210-99-FIM
           END-IF.

           ADD     1                       TO  WRK-ACU-LIDOS.
           MOVE    WRK-LEITURA             TO  WRK-OPERACAO.
           PERFORM 1110-TESTAR-FS-RATEICLI.


           IF    RATCLI-TIPO-DISTR         EQUAL  'AI'  OR
                 RATCLI-TIPO-DISTR         EQUAL  'EI'
                 GO     TO   1210-LER-RATEICLI
           END-IF.

           MOVE  RATCLI-TIPO-DISTR         TO  WRK-DISTR-ATU.
           MOVE  RATCLI-AGENCIA            TO  WRK-AGENCIA-ATU.
           MOVE  RATCLI-DIG-AGE            TO  WRK-DIGITO-ATU.

      *---------------------------------------------------------------*
       1210-99-FIM.  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1220-LER-ARQNTAX  SECTION.
      *---------------------------------------------------------------*

       1220-LEITURA-NTAX.

           READ    ARQNTAX
                   AT END MOVE 'S' TO WRK-FIM-ARQNTAX
                   GO  TO 1220-99-FIM.

           ADD     1   TO WRK-ACU-NTAX.

           IF         (WRK-VR-ATU-POSTA     NOT EQUAL  ZEROS   AND
                       WRK-VR-ATU-IMPRE     NOT EQUAL  ZEROS)
                       GO     TO  1220-99-FIM
           END-IF.

           IF         (TAX2-TIPO    EQUAL   1893  OR
                       TAX2-TIPO    EQUAL   4278)
               MOVE    TAX2-DT-INIC    TO   WRK-AUX-TAXI
               MOVE    TAX2-DT-FIM     TO   WRK-AUX-TAXF
               IF      WRK-DATA-1 LESS WRK-AUX-TAXI-INIC
                       NEXT SENTENCE
               END-IF
                   IF  WRK-DATA-1 GREATER WRK-AUX-TAXI-FIM
                       NEXT SENTENCE
                   END-IF
                       IF  (TAX2-TIPO        EQUAL   1893   AND
                            WRK-VR-ATU-POSTA EQUAL   ZEROS)
                            MOVE TAX2-VR-ATU    TO   WRK-VR-ATU-POSTA
                       END-IF
                        IF (TAX2-TIPO        EQUAL   4278   AND
                            WRK-VR-ATU-IMPRE EQUAL   ZEROS)
                            MOVE TAX2-VR-ATU    TO   WRK-VR-ATU-IMPRE
                        END-IF
                    END-IF.


           GO  TO  1220-LEITURA-NTAX.

      *---------------------------------------------------------------*
       1220-99-FIM.  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1300-TRATA-TPO-DISTR     SECTION.
      *---------------------------------------------------------------*

           MOVE    SPACES                  TO  CB2A-TIPO.

           COMPUTE WRK-VALORFAC-POSTA   =  WRK-VR-ATU-POSTA-R +  0.
           COMPUTE WRK-VALORFAC-IMPRE   =  WRK-VR-ATU-IMPRE-R +  0.

      *---------------------------------------------------------------*
       1300-99-FIM.  EXIT.
      *---------------------------------------------------------------*

      *----------------------------------------------------------------*
       1400-OBTER-DATA-HORA SECTION.
      *----------------------------------------------------------------*

           CALL  'POOL7600'  USING  DATA-HORA.

           MOVE   DT-AAAAMMDD          TO    WRK-AUX-DATA09.
           MOVE   WRK-AUX-DATA         TO    WRK-DATA-1.

           MOVE   TI-HHMMSS            TO    WRK-AUX-HORA07.
           MOVE   WRK-AUX-HORA         TO    WRK-HORA.

           MOVE   WRK-AAAA             TO    WRK-AUX-04
                                             CB2A-AAAA.
           COMPUTE WRK-AUX-04          =     WRK-AUX-04  -  1.
           MOVE   WRK-AUX-04           TO    CB2A-ANO
           MOVE   WRK-MM               TO    CB2A-MM.
           MOVE   WRK-DD               TO    CB2A-DD.
           MOVE   WRK-HORAS            TO    CB3-HORAS.
           MOVE   WRK-MINUTOS          TO    CB3-MINUTOS.
           MOVE   WRK-SEGUNDOS         TO    CB3-SEGUNDOS.

           MOVE WRK-DIA-TIMESTAMP      TO  WRK-DIA-TIMESTAMP-DB2.
           MOVE WRK-HORA-TIMESTAMP     TO  WRK-HORA-TIMESTAMP-DB2.
           MOVE WRK-MIN-TIMESTAMP      TO  WRK-MIN-TIMESTAMP-DB2.
           MOVE WRK-SEG-TIMESTAMP      TO  WRK-SEG-TIMESTAMP-DB2.
           MOVE WRK-NANOSEG-TIMESTAMP  TO  WRK-NANOSEG-TIMESTAMP-DB2.

           STRING WRK-DIA-TIMESTAMP '/'
                  WRK-MES-TIMESTAMP '/'
                  WRK-ANO-TIMESTAMP
           DELIMITED BY SIZE      INTO  WRK-DATAMOV.

           STRING WRK-HORA-TIMESTAMP ':'
                  WRK-MIN-TIMESTAMP ':'
                  WRK-SEG-TIMESTAMP
           DELIMITED BY SIZE      INTO  WRK-HORAMOV.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       2000-PROCESSA SECTION.
      *---------------------------------------------------------------*

           MOVE    RATCLI-AGENCIA      TO     WRK-AGENCIA-ANT.
           MOVE    RATCLI-DIG-AGE      TO     WRK-DIGITO-ANT.
           MOVE    RATCLI-TIPO-DISTR   TO     WRK-DISTR-ANT.
           MOVE    ZEROS               TO     WRK-ACU-VALOR-IMPRE
                                              WRK-ACU-VALOR-POSTA
                                              WRK-ACU-CLIENTE.

           PERFORM 2100-ACUMULA        UNTIL
                    RATCLI-AGENCIA NOT EQUAL  WRK-AGENCIA-ANT OR
                    WRK-FS-RATEICLI    EQUAL  '10'.

           PERFORM 2200-GRAVA-RATEIAGE.

           PERFORM 2300-IMPRIME-RELAJUP.

           IF       WRK-FS-RATEICLI    EQUAL  '10'
                    PERFORM 2500-IMPRIME-TOTAL
           END-IF.

      *---------------------------------------------------------------*
       2000-99-FIM.  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2100-ACUMULA SECTION.
      *---------------------------------------------------------------*

           ADD 1                      TO     WRK-ACU-CLIENTE.

           PERFORM 1210-LER-RATEICLI.

      *---------------------------------------------------------------*
       2100-99-FIM.  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2200-GRAVA-RATEIAGE        SECTION.
      *---------------------------------------------------------------*

      *---------> AJUP IMPRESSAO

           MOVE  WRK-AGENCIA-ANT         TO  AJAG-JUNCAO.

           MOVE WRK-AGENCIA-ANT TO  WRK-1260-AGENCIA
           CALL 'POOL1260'  USING WRK-1260-AGENCIA
                                  WRK-1260-DIGITO
                                  WRK-1260-TAMANHO.

           MOVE  WRK-1260-DIGITO         TO  AJAG-DIGITO.

           MOVE  05802                   TO  AJAG-RAZAO.
           MOVE  ZEROS                   TO  AJAG-SEQ.

           COMPUTE WRK-ACU-VALOR-IMPRE
                                 = WRK-ACU-CLIENTE * WRK-VALORFAC-IMPRE.

H0602      MOVE  WRK-ACU-VALOR-IMPRE     TO  WRK-AUX-VAL17.
H0602      MOVE  ZEROS                   TO  WRK-AUX-VAJ17.
H0602      MOVE  WRK-AUX-VAL17-13        TO  WRK-AUX-VAJ17-13.
H0602      MOVE  WRK-AUX-VAL17-02        TO  WRK-AUX-VAJ17-02.
H0602      MOVE  WRK-AUX-VAJ17           TO  AJAG-VALOR.

H0602      MOVE  WRK-ACU-VALOR-IMPRE     TO  WRK-AUX-VAL17.
H0602      MOVE  ZEROS                   TO  WRK-AUX-VAJ17.
H0602      MOVE  WRK-AUX-VAL17-13        TO  WRK-AUX-VAJ17-13.
H0602      MOVE  WRK-AUX-VAL17-02        TO  WRK-AUX-VAJ17-02.
H0602      MOVE  WRK-AUX-VAJ17           TO  AJAG-VALOR.

           MOVE  004                     TO  AJAG-COD-SERV.

           MOVE  AJAG-COD-SERV           TO  WRK-AJUP.

           ADD   WRK-ACU-VALOR-IMPRE     TO  WRK-TOT-VALOR-IMPRE.
           MOVE  6                       TO  AJAG-DC
                                             WRK-DEBCRED.
           MOVE  LNK-CCUSTO              TO  AJAG-CENT-CUSTO.

           MOVE  WRK-DD                  TO  WRK-DD-AJUP.
           MOVE  WRK-MM                  TO  WRK-MM-AJUP.
           MOVE  WRK-AAAA                TO  WRK-AAAA-AJUP.
           MOVE  WRK-DATA-AJAG-R         TO  AJAG-DT-MOVTO.

      *-----> GRAVA REG-MOVTO-AJAG.
      *-----> MOVER WRK-GRAVACAO PARA  WRK-OPERACAO.
      *-----> CHAMA 1150-TESTAR-FS-RATEIAGE.

           ADD  1                        TO  WRK-ACU-GRAVADOS.

      *---------> AJUP POSTAGEM

           MOVE  WRK-AGENCIA-ANT         TO  AJAG-JUNCAO.

           MOVE  WRK-1260-DIGITO         TO  AJAG-DIGITO.

           MOVE  05802                   TO  AJAG-RAZAO.
           MOVE  ZEROS                   TO  AJAG-SEQ.

           COMPUTE WRK-ACU-VALOR-POSTA
                                 = WRK-ACU-CLIENTE * WRK-VALORFAC-POSTA.

H0602      MOVE  WRK-ACU-VALOR-POSTA     TO  WRK-AUX-VAL17.
H0602      MOVE  ZEROS                   TO  WRK-AUX-VAJ17.
H0602      MOVE  WRK-AUX-VAL17-13        TO  WRK-AUX-VAJ17-13.
H0602      MOVE  WRK-AUX-VAL17-02        TO  WRK-AUX-VAJ17-02.
H0602      MOVE  WRK-AUX-VAJ17           TO  AJAG-VALOR.

           MOVE  350                     TO  AJAG-COD-SERV.

           MOVE  AJAG-COD-SERV           TO  WRK-AJUP.

           ADD   WRK-ACU-VALOR-POSTA     TO  WRK-TOT-VALOR-POSTA.
           MOVE  6                       TO  AJAG-DC
                                             WRK-DEBCRED.
           MOVE  LNK-CCUSTO              TO  AJAG-CENT-CUSTO.

           MOVE  WRK-DD                  TO  WRK-DD-AJUP.
           MOVE  WRK-MM                  TO  WRK-MM-AJUP.
           MOVE  WRK-AAAA                TO  WRK-AAAA-AJUP.
           MOVE  WRK-DATA-AJAG-R         TO  AJAG-DT-MOVTO.

      *-----> GRAVA REG-MOVTO-AJAG.
      *-----> MOVER WRK-GRAVACAO PARA  WRK-OPERACAO.
      *-----> CHAMA 1150-TESTAR-FS-RATEIAGE.

           ADD  1                        TO  WRK-ACU-GRAVADOS.

      *---------------------------------------------------------------*
       2200-99-FIM.  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2300-IMPRIME-RELAJUP SECTION.
      *---------------------------------------------------------------*

           MOVE 'S'                      TO  WRK-QUEBRA.
           MOVE WRK-GRAVACAO             TO  WRK-OPERACAO.

           IF   WRK-ACU-LIN  GREATER  59     OR
                WRK-QUEBRA   EQUAL    'S'
                PERFORM 2310-IMPRIME-CABECS
           END-IF.

 H0502     MOVE    WRK-AGENCIA-ANT     TO   WRK-DT1-AGENCIA
                                            WRK-1260-AGENCIA.

           CALL  'POOL1260'  USING  WRK-1260-AGENCIA
                                    WRK-1260-DIGITO
                                    WRK-1260-TAMANHO.

           MOVE    WRK-1260-DIGITO     TO     DT1-DIG-AGE.
H0602      MOVE    WRK-ACU-CLIENTE     TO     WRK-AUX-CLIE15.
H0602      MOVE    WRK-AUX-CLIE7       TO     DT1-QTD-OBJ.

H0602      MOVE    WRK-ACU-VALOR-POSTA TO     WRK-AUX-VAL17.
H0602      MOVE    WRK-AUX-VAL17-12    TO     WRK-DT1-VALOR-POSTA.

H0602      MOVE    WRK-ACU-VALOR-IMPRE TO     WRK-AUX-VAL17.
H0602      MOVE    WRK-AUX-VAL17-12    TO     WRK-DT1-VALOR-IMPRE.

           IF      WRK-DEBCRED         EQUAL  6
             MOVE  'D'                 TO     DT1-DEB-CRED
           ELSE
             MOVE  'C'                 TO     DT1-DEB-CRED
           END-IF.

           COMPUTE WRK-TOT-VALOR-AUX   =      WRK-ACU-VALOR-POSTA  +
                                              WRK-ACU-VALOR-IMPRE.
H0602      MOVE    ZEROS               TO     WRK-AUX-VAL17-13.
H0602      MOVE    WRK-TOT-VALOR-AUX   TO     WRK-AUX-VAL17.
H0602      MOVE    WRK-AUX-VLOR17-13   TO     WRK-DT1-VALOR-TOTAL.


           ADD     WRK-TOT-VALOR-AUX   TO     WRK-TOT-VALOR-GERAL
                                              WRK-TOT-VLR-GER.

           WRITE   FD-REG-RELAJUP     FROM    DETAL01.
           PERFORM 1130-TESTAR-FS-RELAJUP.
           MOVE    ' '                 TO     DT1-CARRO.
           ADD     1                   TO     WRK-ACU-IMPRESSOS
                                              WRK-ACU-LIN.

           ADD     WRK-ACU-CLIENTE     TO     WRK-TOT-CLIENTE
                                              WRK-TOT-CLI.

           MOVE    'N'                 TO     WRK-QUEBRA.

           PERFORM  2600-IMPR-TOT-AGE.

      *---------------------------------------------------------------*
       2300-99-FIM.  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2310-IMPRIME-CABECS  SECTION.
      *---------------------------------------------------------------*

 H0502     IF      WRK-CB0-AGENCIA     NOT  EQUAL  04990
 H0502             MOVE    WRK-AGENCIA-ANT  TO     WRK-CB0-AGENCIA
 H0502     END-IF.

           MOVE WRK-GRAVACAO          TO      WRK-OPERACAO.

           ADD  1                     TO      WRK-ACU-PAG.
           MOVE WRK-ACU-PAG           TO      CB1-PAG.

           IF   WRK-ACU-LIN  GREATER  58
                MOVE    '0'  TO       WRK-CB0-CARRO
           ELSE
                MOVE    '1'  TO       WRK-CB0-CARRO
           END-IF.

           WRITE   FD-REG-RELAJUP     FROM    WRK-CABEC00.
           PERFORM 1130-TESTAR-FS-RELAJUP.

           WRITE   FD-REG-RELAJUP     FROM    CABEC01.
           PERFORM 1130-TESTAR-FS-RELAJUP.

           WRITE   FD-REG-RELAJUP     FROM    CABEC02A.
           PERFORM 1130-TESTAR-FS-RELAJUP.

           WRITE   FD-REG-RELAJUP     FROM    CABEC03.
           PERFORM 1130-TESTAR-FS-RELAJUP.

           WRITE   FD-REG-RELAJUP     FROM    CABEC03A.
           PERFORM 1130-TESTAR-FS-RELAJUP.

           WRITE   FD-REG-RELAJUP     FROM    CABEC04.
           PERFORM 1130-TESTAR-FS-RELAJUP.

           MOVE    '0'                 TO     DT1-CARRO.
           MOVE     8                  TO     WRK-ACU-LIN.

      *---------------------------------------------------------------*
       2310-99-FIM.  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2500-IMPRIME-TOTAL SECTION.
      *---------------------------------------------------------------*

           MOVE 'S'              TO       WRK-QUEBRA.

           MOVE WRK-GRAVACAO     TO       WRK-OPERACAO.

           IF   WRK-ACU-LIN      GREATER  54  OR
                WRK-QUEBRA       EQUAL    'S'
                MOVE  4990       TO       WRK-CB0-AGENCIA
                PERFORM 2310-IMPRIME-CABECS
241109*****ERA O COMANDO ELSE
           END-IF.
      *---------------------------------------------------------------*
      *    GRAVA CONTRA-PARTIDA DOS LANCAMENTOS
      *---------------------------------------------------------------*
 H0502          MOVE    4990                TO  WRK-DT1-AGENCIA
                MOVE    '5'                 TO     DT1-DIG-AGE
H0602           MOVE    WRK-TOT-CLIENTE     TO     WRK-AUX-CLIE15
H0602           MOVE    WRK-AUX-CLIE7       TO     DT1-QTD-OBJ.


H0602           MOVE    WRK-TOT-VALOR-IMPRE TO     WRK-AUX-VAL17
H0602           MOVE    WRK-AUX-VAL17-12    TO     WRK-DT1-VALOR-IMPRE.



H0602           MOVE    WRK-TOT-VALOR-POSTA TO     WRK-AUX-VAL17
H0602           MOVE    WRK-AUX-VAL17-12    TO     WRK-DT1-VALOR-POSTA.


H0602           MOVE    WRK-TOT-VALOR-GERAL TO     WRK-AUX-VAL17
H0602           MOVE    WRK-AUX-VLOR17-13   TO     WRK-DT1-VALOR-TOTAL.

                MOVE    'C'                 TO     DT1-DEB-CRED.

                WRITE   FD-REG-RELAJUP      FROM   DETAL01.
                PERFORM 1130-TESTAR-FS-RELAJUP.
                ADD     1                   TO     WRK-ACU-IMPRESSOS
                                                   WRK-ACU-LIN.
241109*****TINHA O COMANDO END-IF.

      *---------------------------------------------------------------*

           IF   WRK-ACU-LIN      GREATER  54
                PERFORM 2310-IMPRIME-CABECS
           END-IF.

H0602      MOVE    WRK-TOT-CLIENTE     TO     WRK-AUX-CLIE15.
H0602      MOVE    WRK-AUX-CLIE8       TO     TT1-QTD-OBJ.


H0602      MOVE    WRK-TOT-VALOR-IMPRE TO     WRK-AUX-VAL17.
H0602      MOVE    WRK-AUX-VAL17-14    TO     WRK-TT1-VLR-IMPRE.


H0602      MOVE    WRK-TOT-VALOR-POSTA TO     WRK-AUX-VAL17.
H0602      MOVE    WRK-AUX-VAL17-14    TO     WRK-TT1-VLR-POSTA.


H0602      MOVE    WRK-TOT-VALOR-GERAL TO     WRK-AUX-VAL17.
H0602      MOVE    WRK-AUX-VAL17-14    TO     WRK-TT1-VLR-TOTAL.

           WRITE   FD-REG-RELAJUP      FROM   TOTAL01.
           PERFORM 1130-TESTAR-FS-RELAJUP
           ADD     1                   TO     WRK-ACU-IMPRESSOS.

           WRITE   FD-REG-RELAJUP      FROM   TOTAL02.
           PERFORM 1130-TESTAR-FS-RELAJUP
           ADD     1                   TO     WRK-ACU-IMPRESSOS.

           WRITE   FD-REG-RELAJUP      FROM   TOTAL03.
           PERFORM 1130-TESTAR-FS-RELAJUP
           ADD     1                   TO     WRK-ACU-IMPRESSOS.

      *---------------------------------------------------------------*
       2500-99-FIM.  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2600-IMPR-TOT-AGE  SECTION.
      *---------------------------------------------------------------*

           IF   WRK-ACU-LIN      GREATER  54
                PERFORM 2310-IMPRIME-CABECS
           END-IF.

H0602      MOVE    WRK-TOT-CLI         TO     WRK-AUX-CLIE15.
H0602      MOVE    WRK-AUX-CLIE8       TO     TT1-QTD-OBJ.


H0602      MOVE    WRK-ACU-VALOR-IMPRE TO     WRK-AUX-VAL17.
H0602      MOVE    WRK-AUX-VAL17-14    TO     WRK-TT1-VLR-IMPRE.


H0602      MOVE    WRK-ACU-VALOR-POSTA TO     WRK-AUX-VAL17.
H0602      MOVE    WRK-AUX-VAL17-14    TO     WRK-TT1-VLR-POSTA.


H0602      MOVE    WRK-TOT-VLR-GER     TO     WRK-AUX-VAL17.
H0602      MOVE    WRK-AUX-VAL17-14    TO     WRK-TT1-VLR-TOTAL.



           WRITE   FD-REG-RELAJUP      FROM   TOTAL01.
           PERFORM 1130-TESTAR-FS-RELAJUP
           ADD     1                   TO     WRK-ACU-IMPRESSOS.

           WRITE   FD-REG-RELAJUP      FROM   TOTAL02.
           PERFORM 1130-TESTAR-FS-RELAJUP
           ADD     1                   TO     WRK-ACU-IMPRESSOS.

           WRITE   FD-REG-RELAJUP      FROM   TOTAL03.
           PERFORM 1130-TESTAR-FS-RELAJUP
           ADD     1                   TO     WRK-ACU-IMPRESSOS.

H0602      MOVE    ZEROS               TO     WRK-TOT-CLI
H0602                                         WRK-TOT-VLR-GER.

      *---------------------------------------------------------------*
       2600-99-FIM.  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3000-FINALIZA  SECTION.
      *---------------------------------------------------------------*

           PERFORM 4000-EXIBIR-DISP-ESTATISTICAS.

           CLOSE  RATEICLI
                  ARQNTAX
                  RELAJUP.

           MOVE    WRK-FECHAMENTO          TO   WRK-OPERACAO.
           PERFORM 1100-TESTAR-FILE-STATUS.

           STOP RUN.

      *---------------------------------------------------------------*
       3000-99-FIM.  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       4000-EXIBIR-DISP-ESTATISTICAS   SECTION.
      *---------------------------------------------------------------*

           DISPLAY WRK-DISP1.
           DISPLAY WRK-DISP2.
           DISPLAY WRK-DISP3.
           DISPLAY WRK-DISP4.
           DISPLAY WRK-DISP5.
           DISPLAY WRK-DISP6.
           DISPLAY WRK-DISP3.
           DISPLAY WRK-DISP7.
           DISPLAY WRK-DISP8.

           MOVE WRK-ACU-NTAX       TO WRK-QTDEARQ.
           MOVE 'ARQNTAX'          TO WRK-DDNAME.
           MOVE 'I'                TO WRK-I-O.
           MOVE 'ARQUIVO DE TAXAS'
                                       TO WRK-DESCARQ.
           DISPLAY WRK-DISP9.

           MOVE WRK-ACU-LIDOS      TO WRK-QTDEARQ.
           MOVE 'RATEICLI'         TO WRK-DDNAME.
           MOVE 'I'                TO WRK-I-O.
           MOVE 'QTDE. DE REGISTROS LIDOS'
                                       TO WRK-DESCARQ.
           DISPLAY WRK-DISP9.
           DISPLAY WRK-DISP1.

           MOVE WRK-ACU-GRAVADOS   TO WRK-QTDEARQ.
           MOVE 'RATEIAGE'         TO WRK-DDNAME.
           MOVE '0'                TO WRK-I-O.
           MOVE 'QTDE. DE REGS. GRAVADOS'
                                       TO WRK-DESCARQ.

           DISPLAY WRK-DISP9.
           DISPLAY WRK-DISP1.

           MOVE WRK-ACU-IMPRESSOS  TO WRK-QTDEARQ.
           MOVE 'RELAJUP'          TO WRK-DDNAME.
           MOVE '0'                TO WRK-I-O.
           MOVE 'QTDE. DE REGS. IMPRESSOS'
                                       TO WRK-DESCARQ.

           DISPLAY WRK-DISP9.
           DISPLAY WRK-DISP1.

H0602      MOVE WRK-TOT-CLIENTE    TO WRK-AUX-CLIE15.
H0602      MOVE WRK-AUX-CLIE12     TO WRK-QTDEARQ.


           MOVE SPACES             TO WRK-DDNAME.
           MOVE ' '                TO WRK-I-O.
           MOVE 'QTDE. OBJETOS FRANQUEADOS'
                                       TO WRK-DESCARQ.

           DISPLAY WRK-DISP9.

           MOVE WRK-VALORFAC-POSTA     TO WRK-VALOR-FATOR.
           MOVE SPACES                 TO WRK-DDNAME-FATOR.
           MOVE ' '                    TO WRK-I-O-FATOR.
           MOVE '1893 (TXA.POSTAGEM:- R$)'
                                       TO WRK-DESC-FATOR.

           DISPLAY WRK-DISP-FATOR.
           DISPLAY WRK-DISP1.

           MOVE WRK-VALORFAC-IMPRE     TO WRK-VALOR-FATOR.
           MOVE SPACES                 TO WRK-DDNAME-FATOR.
           MOVE ' '                    TO WRK-I-O-FATOR.
           MOVE '4278 (TXA.IMPRESSAO:-R$)'
                                       TO WRK-DESC-FATOR.

           DISPLAY WRK-DISP-FATOR.
           DISPLAY WRK-DISP1.

      *---------------------------------------------------------------*
       4000-99-FIM.  EXIT.
      *---------------------------------------------------------------*
