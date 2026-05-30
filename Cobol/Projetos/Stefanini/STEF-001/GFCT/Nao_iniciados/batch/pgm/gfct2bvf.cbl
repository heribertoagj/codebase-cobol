      *===============================================================*
       IDENTIFICATION  DIVISION.
      *===============================================================*
       PROGRAM-ID.     GFCT2BVF.
       AUTHOR.                         WILKE ARAUJO - SIS.
      *================================================================*
      *       S   I   S   -   C  O  N  S  U  L  T  O  R  I  A          *
      *----------------------------------------------------------------*
      *    PROGRAMA....: GFCT2BVF                                      *
      *    ANALISTA ...: WILKE ARAUJO - SIS                            *
      *    PROGRAMADOR.: WILKE ARAUJO - SIS CONSULTORIA                *
      *    DATA........: AGO_2023                                      *
      *----------------------------------------------------------------*
      *    OBJETIVO....: BATIMENTO DO ARQUIVO DO DIA ANTERIOR COM O DIA*
      *                  ATUAL, PARA ENVIAR AO CEPT A DIFERENCA        *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                              DE/BOOK          *
      *                        ARQNGATU      I        GFCTWBKE         *
      *                        ARQNGANT      I        GFCTWBKE         *
      *                        ARQSAIDA      O        GFCTWBKE         *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#FRWKGE - BOOK DE COMUNICACAO COM FRWK2999.                *
      *    I#FRWKAR - BOOK PARA TRATAMENTO DE ERROS DE ARQUIVOS.       *
      *================================================================*

      *===============================================================*
       ENVIRONMENT                     DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *---------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *---------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *---------------------------------------------------------------*
       FILE-CONTROL.

           SELECT ARQNGATU     ASSIGN  TO  UT-S-ARQNGATU
                  FILE         STATUS  IS  WRK-FS-ARQNGATU.

           SELECT ARQNGANT     ASSIGN  TO  UT-S-ARQNGANT
                  FILE         STATUS  IS  WRK-FS-ARQNGANT.

           SELECT ARQSAIDA     ASSIGN  TO  UT-S-ARQSAIDA
                  FILE         STATUS  IS  WRK-FS-ARQSAIDA.

      *===============================================================*
       DATA    DIVISION.
      *===============================================================*
       FILE                            SECTION.

      *----------------------------------------------------------------*
      *   INPUT : ADESOES CEPT ATUAL                                   *
      *           ORG.SEQUENCIAL  -  LRECL = 150                       *
      *----------------------------------------------------------------*

       FD  ARQNGATU
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-ARQNGATU              PIC  X(150).

      *----------------------------------------------------------------*
      *   INPUT : ADESOES CEPT ANTERIOR                                *
      *           ORG.SEQUENCIAL  -  LRECL = 150                       *
      *----------------------------------------------------------------*

       FD  ARQNGANT
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-ARQNGANT              PIC  X(150).

      *---------------------------------------------------------------*
      *  SAIDA     LRECL = 135  REGISTROS ATUALIZADOS A SEREM ENVIADOS
      *                         AO CEPT
      *---------------------------------------------------------------*
       FD  ARQSAIDA
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01  FD-ARQSAIDA                 PIC  X(150).

      *---------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** GFCT2BVF - INICIO DA AREA DE WORKING ***'.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA LAYOUT DOS ARQUIVOS ANTERIOR'.
      *---------------------------------------------------------------*
           COPY GFCTWBKE.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA LAYOUT DOS ARQUIVOS ANTERIOR'.
      *---------------------------------------------------------------*
       01 WRK-REGISTRO-ANT.
         03 WRK-REGISTRO-GFCTB0P7.
            05 WRK-CBCO-CLI                PIC S9(03) COMP-3  VALUE 0.
            05 WRK-CAG-BCRIA               PIC S9(05) COMP-3  VALUE 0.
            05 WRK-CCTA-BCRIA-CLI          PIC S9(13) COMP-3  VALUE 0.
            05 WRK-DMES-ANO-FRANQ          PIC S9(06) COMP-3  VALUE 0.
            05 WRK-NCTRL-FRANQ-FLEXZ       PIC S9(10) COMP-3  VALUE 0.
            05 WRK-CGRP-SERVC              PIC S9(05) COMP-3  VALUE 0.
            05 WRK-DINIC-VGCIA-SERVC       PIC  X(10)    VALUE SPACES.
            05 WRK-QFRANQ-TRF-CADTR        PIC S9(07) COMP-3  VALUE 0.
            05 WRK-QFRANQ-TRF-UTLZD        PIC S9(07) COMP-3  VALUE 0.
            05 WRK-QFRANQ-TRF-EXCED        PIC S9(07) COMP-3  VALUE 0.
         03 WRK-REGISTRO-GFCTB0P8.
            05 WRK-CSIST                   PIC  X(04)    VALUE SPACES.
            05 WRK-CBCO-SALRL              PIC S9(03) COMP-3  VALUE 0.
            05 WRK-CAG-BCRIA-SALRL         PIC S9(05) COMP-3  VALUE 0.
            05 WRK-CCTA-BCRIA-SALRL        PIC S9(13) COMP-3  VALUE 0.
            05 WRK-NFLEXZ-TARIF-AVULS      PIC S9(10) COMP-3  VALUE 0.
            05 WRK-CPSSOA-JURID-CONTR      PIC S9(10) COMP-3  VALUE 0.
            05 WRK-CTPO-CONTR-NEGOC        PIC S9(03) COMP-3  VALUE 0.
            05 WRK-NSEQ-CONTR-NEGOC        PIC S9(10) COMP-3  VALUE 0.
            05 WRK-DINIC-VGCIA-FLEXZ       PIC  X(10)    VALUE SPACES.
            05 WRK-DINIC-VGCIA-NUL         PIC  X(01)    VALUE SPACES.
            05 WRK-DFIM-VGCIA-FLEXZ        PIC  X(10)    VALUE SPACES.
            05 WRK-DFIM-VGCIA-NUL          PIC  X(01)    VALUE SPACES.
         03 WRK-REGISTRO-COMPLENTO.
            05 WRK-TRF-GFCT                PIC  9(05)         VALUE 0.
            05 WRK-TRF-LEGADO              PIC  9(05)         VALUE 0.
            05 WRK-CENTRO-CUSTO            PIC  X(04)    VALUE SPACES.
            05 WRK-DTA-INI-VIG             PIC  9(08)         VALUE 0.
            05 WRK-DTA-FIM-VIG             PIC  9(08)         VALUE 0.
            05 WRK-CGRUPO                  PIC  9(03) COMP-3  VALUE 0.
            05 WRK-CSUBGR                  PIC  9(03) COMP-3  VALUE 0.
            05 WRK-CPF-NUM                 PIC  9(09) COMP-3  VALUE 0.
            05 WRK-CPF-FIL                 PIC  9(04)         VALUE 0.
            05 WRK-CPF-CON                 PIC  9(02)         VALUE 0.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE TESTE DE FILE-STATUS *'.
      *---------------------------------------------------------------*
       01  WRK-STATUS-ARQS.
           05  WRK-FS-ARQNGATU         PIC  X(002)         VALUE SPACES.
           05  WRK-FS-ARQNGANT         PIC  X(002)         VALUE SPACES.
           05  WRK-FS-ARQSAIDA         PIC  X(002)         VALUE SPACES.
      *---------------------------------------------------------------*
      * AREA PARA ACUMULADORES
      *---------------------------------------------------------------*

       01  ACU-LIDOS-ARQNGATU         PIC 9(012) COMP-3 VALUE ZEROS.
       01  ACU-LIDOS-ARQNGANT         PIC 9(012) COMP-3 VALUE ZEROS.
       01  ACU-GRAVA-ARQSAIDA         PIC 9(012) COMP-3 VALUE ZEROS.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA CAMPOS AUXILIARES ***'.
      *---------------------------------------------------------------*

       01  WRK-CAMPOS-AUXILIARES.
           05  WRK-LOCAL               PIC  X(004)         VALUE SPACES.
           05  WRK-MASCARA             PIC ZZZ.ZZZ.ZZZ.ZZ9
                                                           VALUE ZEROS.
           05  WRK-FRWK2999            PIC  X(008)         VALUE
                                                           'FRWK2999'.
           05  WRK-PROGRAMA            PIC  X(008)         VALUE
                                                           'GFCT2BVF'.
           05  WRK-ABEND-BAT           PIC S9(004) COMP    VALUE +1111.
           05  WRK-DUMP-BAT            PIC  X(001)         VALUE 'S'.
           05  WRK-DDNAME              PIC  X(008)         VALUE SPACES.
           05  WRK-FILE-STATUS         PIC  X(002)         VALUE SPACES.
           05  WRK-ABEND               PIC  X(001)         VALUE SPACES.
               88 WRK-ABENDAR                              VALUE 'S'.
      *
      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA REDEFINES         ***'.
      *---------------------------------------------------------------*
       01  WRK-S9-05               PIC  +9(05)         VALUE ZEROS.
       01  FILLER                  REDEFINES WRK-S9-05.
           05 FILLER               PIC  X(001).
           05 WRK-9-05             PIC  9(005).

       01  WRK-S9-13               PIC  +9(13)         VALUE ZEROS.
       01  FILLER                  REDEFINES WRK-S9-13.
           05 FILLER               PIC  X(001).
           05 WRK-9-13             PIC  9(013).

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA CHAVE  AUXILIARES ***'.
      *---------------------------------------------------------------*
       01  WRK-CHAVE-ATU.
           03  WRK-AGENCIA-ATU             PIC 9(005) VALUE ZEROS.
           03  WRK-CONTA-ATU               PIC 9(013) VALUE ZEROS.
      *
       01  WRK-CHAVE-ANT.
           03  WRK-AGENCIA-ANT             PIC 9(005) VALUE ZEROS.
           03  WRK-CONTA-ANT               PIC 9(013) VALUE ZEROS.
      *
      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA FRAMEWORK ***'.
      *---------------------------------------------------------------*
      *
       01  WRK-AREA-ERRO.
           COPY  'I#FRWKGE'.
           05  WRK-BLOCO-INFO-ERRO                     VALUE SPACES.
               10  WRK-CHAR-INFO-ERRO  PIC X(001)  OCCURS
                   0 TO 30000 TIMES DEPENDING ON FRWKGHEA-TAM-DADOS.
      *
       01  WRK-FRWKGARQ.
           COPY  'I#FRWKAR'.
      *
      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** GFCT2BVF - FIM DA AREA DE WORKING ***'.
      *---------------------------------------------------------------*

      *===============================================================*
       PROCEDURE   DIVISION.
      *===============================================================*
      *
       0000-INICIAR                    SECTION.
      *---------------------------------------------------------------*

           CALL    'CKRS1000'.

           PERFORM 0100-ABRIR-ARQS.

           PERFORM 3000-PROCESSAR UNTIL WRK-FS-ARQNGATU     EQUAL '10'.

           PERFORM 4000-FINALIZA.

           STOP RUN.

      *---------------------------------------------------------------*
       0000-FIM-INICIALIZAR.           EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       0100-ABRIR-ARQS                 SECTION.
      *---------------------------------------------------------------*

           MOVE '0001'                 TO  WRK-LOCAL.
           MOVE '0100-ABRIR-ARQS'      TO  FRWKGHEA-IDEN-PARAGRAFO.
           SET  ARQ-OPEN               TO  TRUE.

           OPEN INPUT  ARQNGATU
                       ARQNGANT
                OUTPUT ARQSAIDA

           PERFORM 1000-TESTAR-FS.

           PERFORM 2000-LER-ARQNGATU.

           IF WRK-FS-ARQNGATU EQUAL '10'
              DISPLAY '******* GFCT2BVF *****************'
              DISPLAY '     ARQUIVO ARQNGATU VAZIO       '
              DISPLAY '**********************************'
           END-IF.

           PERFORM 2100-LER-ARQNGANT.

           IF WRK-FS-ARQNGANT EQUAL '10'
              DISPLAY '******* GFCT2BVF *****************'
              DISPLAY '     ARQUIVO ARQNGANT VAZIO       '
              DISPLAY '**********************************'
           END-IF.

      *---------------------------------------------------------------*
       0100-99-FIM.          EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1000-TESTAR-FS                  SECTION.
      *---------------------------------------------------------------*

           PERFORM 1100-TESTAR-FS-ARQNGATU.
           PERFORM 1200-TESTAR-FS-ARQNGANT.
           PERFORM 1300-TESTAR-FS-ARQSAIDA.

      *---------------------------------------------------------------*
       1000-99-FIM.          EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1100-TESTAR-FS-ARQNGATU         SECTION.
      *---------------------------------------------------------------*

           MOVE 'ARQNGATU'             TO  WRK-DDNAME.

           IF WRK-FS-ARQNGATU NOT EQUAL '00'
              SET WRK-ABENDAR          TO  TRUE
              SET ERRO-ARQUIVO         TO  TRUE
              MOVE WRK-FS-ARQNGATU     TO  WRK-FILE-STATUS
              PERFORM 8200-TRATAR-ERRO-ARQ
           END-IF.

      *---------------------------------------------------------------*
       1100-99-FIM.          EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1200-TESTAR-FS-ARQNGANT         SECTION.
      *---------------------------------------------------------------*

           MOVE 'ARQNGANT'             TO  WRK-DDNAME.

           IF WRK-FS-ARQNGANT NOT EQUAL '00'
              SET WRK-ABENDAR          TO  TRUE
              SET ERRO-ARQUIVO         TO  TRUE
              MOVE WRK-FS-ARQNGANT     TO  WRK-FILE-STATUS
              PERFORM 8200-TRATAR-ERRO-ARQ
           END-IF.

      *---------------------------------------------------------------*
       1200-99-FIM.          EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1300-TESTAR-FS-ARQSAIDA         SECTION.
      *---------------------------------------------------------------*

           MOVE 'ARQSAIDA'             TO  WRK-DDNAME.

           IF WRK-FS-ARQSAIDA NOT EQUAL '00'
              SET WRK-ABENDAR          TO  TRUE
              SET ERRO-ARQUIVO         TO  TRUE
              MOVE WRK-FS-ARQSAIDA     TO  WRK-FILE-STATUS
              PERFORM 8200-TRATAR-ERRO-ARQ
           END-IF.

      *---------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2000-LER-ARQNGATU               SECTION.
      *---------------------------------------------------------------*

           MOVE '0003'                 TO WRK-LOCAL.
           MOVE '2000-LER-ARQNGATU'    TO FRWKGHEA-IDEN-PARAGRAFO.
           SET ARQ-READ                TO TRUE.

           READ ARQNGATU             INTO GFCTWBKE-REGISTRO

           IF WRK-FS-ARQNGATU EQUAL '10'
              MOVE HIGH-VALUES         TO WRK-CHAVE-ATU
              GO                       TO 2000-99-FIM
           END-IF.

           PERFORM 1100-TESTAR-FS-ARQNGATU.

           ADD  1                      TO ACU-LIDOS-ARQNGATU.

           MOVE GFCTWBKE-CAG-BCRIA      TO WRK-S9-05
           MOVE WRK-9-05                TO WRK-AGENCIA-ATU

           MOVE GFCTWBKE-CCTA-BCRIA-CLI TO WRK-S9-13
           MOVE WRK-9-13                TO WRK-CONTA-ATU.

      *---------------------------------------------------------------*
       2000-99-FIM.          EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2100-LER-ARQNGANT               SECTION.
      *---------------------------------------------------------------*

           MOVE '0004'                 TO WRK-LOCAL.
           MOVE '2100-LER-ARQNGANT'    TO FRWKGHEA-IDEN-PARAGRAFO.
           SET ARQ-READ                TO TRUE.

           READ ARQNGANT             INTO WRK-REGISTRO-ANT

           IF WRK-FS-ARQNGANT EQUAL '10'
              MOVE HIGH-VALUES         TO WRK-CHAVE-ANT
              GO                       TO 2100-99-FIM
           END-IF.

           PERFORM 1200-TESTAR-FS-ARQNGANT.

           ADD  1                        TO ACU-LIDOS-ARQNGANT.

           MOVE WRK-CAG-BCRIA            TO WRK-S9-05
           MOVE WRK-9-05                 TO WRK-AGENCIA-ANT

           MOVE WRK-CCTA-BCRIA-CLI       TO WRK-S9-13
           MOVE WRK-9-13                 TO WRK-CONTA-ANT.

      *---------------------------------------------------------------*
       2100-99-FIM.          EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *---------------------------------------------------------------*

           IF WRK-CHAVE-ANT EQUAL WRK-CHAVE-ATU
              PERFORM 3100-GRAVAR-ARQSAIDA
              PERFORM 2100-LER-ARQNGANT
              PERFORM 2000-LER-ARQNGATU
           ELSE
              IF WRK-CHAVE-ANT GREATER WRK-CHAVE-ATU
                 PERFORM 3100-GRAVAR-ARQSAIDA
                 PERFORM 2000-LER-ARQNGATU
              ELSE
                 PERFORM 2100-LER-ARQNGANT
              END-IF
           END-IF.

      *---------------------------------------------------------------*
       3000-99-FIM.          EXIT.
      *---------------------------------------------------------------*


      *---------------------------------------------------------------*
       3100-GRAVAR-ARQSAIDA            SECTION.
      *---------------------------------------------------------------*

           MOVE '0005'                 TO WRK-LOCAL.
           MOVE '3100-GRAVAR-ARQSAIDA' TO FRWKGHEA-IDEN-PARAGRAFO.
           SET ARQ-WRITE               TO  TRUE.

           WRITE FD-ARQSAIDA           FROM GFCTWBKE-REGISTRO

           ADD    1                    TO ACU-GRAVA-ARQSAIDA.

           PERFORM 1300-TESTAR-FS-ARQSAIDA.

      *---------------------------------------------------------------*
       3100-99-FIM.          EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------
       4000-FINALIZA                   SECTION.
      *---------------------------------------------------------------

           SET ARQ-CLOSE               TO  TRUE.

           CLOSE ARQNGATU
                 ARQNGANT
                 ARQSAIDA

           PERFORM 1000-TESTAR-FS.

           PERFORM 4200-DISPLAYS.

           DISPLAY '************ PROGRAMA GFCT2BVF *************'
           DISPLAY '*                                          *'
           DISPLAY '* PROGRAMA GFCT2BVF ENCERRADO COM SUCESSO  *'
           DISPLAY '*                                          *'
           DISPLAY '************ PROGRAMA GFCT2BVF *************'
           MOVE ZEROS                  TO RETURN-CODE

           STOP RUN.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4200-DISPLAYS                   SECTION.
      *----------------------------------------------------------------*

           DISPLAY '************ PROGRAMA GFCT2BVF *****************'.
           MOVE ACU-LIDOS-ARQNGATU     TO WRK-MASCARA.
           DISPLAY '*                                              *'.
           DISPLAY '* REGISTROS LIDOS ARQNGATU....:' WRK-MASCARA.
           MOVE ACU-LIDOS-ARQNGANT     TO WRK-MASCARA.
           DISPLAY '*                                              *'.
           DISPLAY '* REGISTROS LIDOS ARQNGANT....:' WRK-MASCARA.
           MOVE ACU-GRAVA-ARQSAIDA     TO WRK-MASCARA.
           DISPLAY '*                                              *'.
           DISPLAY '* REGISTROS GRAVADOS ARQSAIDA.:' WRK-MASCARA.
           DISPLAY '*                                              *'.
           DISPLAY '************************************************'.

      *----------------------------------------------------------------*
       4200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       8200-TRATAR-ERRO-ARQ            SECTION.
      *---------------------------------------------------------------*

           SET WRK-ABENDAR             TO TRUE
           SET ERRO-ARQUIVO            TO TRUE
           MOVE WRK-FILE-STATUS        TO FRWKGARQ-FILE-STATUS
           MOVE WRK-DDNAME             TO FRWKGARQ-NOME-ARQUIVO
           MOVE WRK-PROGRAMA           TO FRWKGHEA-NOME-PROGRAMA
           MOVE FRWKGARQ-TAM-LAYOUT    TO FRWKGHEA-TAM-DADOS
           MOVE WRK-FRWKGARQ           TO WRK-BLOCO-INFO-ERRO
                                          (1:FRWKGHEA-TAM-DADOS).

           DISPLAY ' '.
           DISPLAY 'ERRO NO ACESSO AO ARQUIVO ' FRWKGARQ-NOME-ARQUIVO.
           DISPLAY ' '.
           DISPLAY 'FRWKGHEA-IDEN-PARAGRAFO= ' FRWKGHEA-IDEN-PARAGRAFO.
           DISPLAY 'FRWKGARQ-FILE-STAUTS   = ' FRWKGARQ-FILE-STATUS.
           DISPLAY 'FRWKGARQ-NOME-ARQUIVO  = ' FRWKGARQ-NOME-ARQUIVO.
           DISPLAY 'FRWKGARQ-COMANDO       = ' FRWKGARQ-COMANDO.
           DISPLAY 'WRK-LOCAL              = ' WRK-LOCAL.

           PERFORM 8300-CHAMAR-API.

      *----------------------------------------------------------------*
       8200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       8300-CHAMAR-API                 SECTION.
      *---------------------------------------------------------------*
           DISPLAY ' ************************* GFCT2BVF **************'.
           DISPLAY ' * FRWK2999 CHAMADO. SEGUEM SEUS PARAMETROS       '.
           DISPLAY ' *                                                '.
           DISPLAY ' * FRWKGHEA-TIPO-ERRO  = ' FRWKGHEA-TIPO-ERRO
           DISPLAY ' * FRWKGHEA-NOME-PROGR = ' FRWKGHEA-NOME-PROGRAMA
           DISPLAY ' * FRWKGHEA-IDEN-PARAG = ' FRWKGHEA-IDEN-PARAGRAFO
           DISPLAY ' * FRWKGHEA-TAM-DADOS  = ' FRWKGHEA-TAM-DADOS
           DISPLAY ' ************************* GFCT2BVF **************'.

           CALL WRK-FRWK2999           USING WRK-AREA-ERRO.

           DISPLAY ' ************************* GFCT2BVF **************'.
           DISPLAY ' * PARAMETROS RETORNADOS PELO FRWK2999            '.
           DISPLAY ' *                                                '.
           DISPLAY ' * FRWKGERR-COD-RETORNO    = ' FRWKGERR-COD-RETORNO
           DISPLAY ' * FRWKGERR-COD-ERRO       = ' FRWKGERR-COD-ERRO
           DISPLAY ' * FRWKGERR-COD-MENSAGEM   = ' FRWKGERR-COD-MENSAGEM
                   ' *                                                '.
           DISPLAY ' ************************* GFCT2BVF **************'
                   ' ***********'.

           IF WRK-ABENDAR
              PERFORM 8500-ABENDAR-PROGRAMA
           END-IF.

      *----------------------------------------------------------------*
       8300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       8500-ABENDAR-PROGRAMA           SECTION.
      *----------------------------------------------------------------*

           CALL 'BRAD0450' USING  WRK-ABEND-BAT  WRK-DUMP-BAT.

      *----------------------------------------------------------------*
       8500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

