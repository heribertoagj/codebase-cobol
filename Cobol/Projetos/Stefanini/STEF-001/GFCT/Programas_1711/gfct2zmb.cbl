      *===============================================================*
       IDENTIFICATION  DIVISION.
      *===============================================================*
       PROGRAM-ID.     GFCT2ZMB.
       AUTHOR.                         MAURICIO ROQUE DA SILVA.
      *================================================================*
      *       S   I   S   -   C  O  N  S  U  L  T  O  R  I  A          *
      *----------------------------------------------------------------*
      *    PROGRAMA....: GFCT2ZMB                                      *
      *    ANALISTA ...: MAURICIO ROQUE DA SILVA                       *
      *    PROGRAMADOR.: MAURICIO ROQUE DA SILVA                       *
      *    DATA........: 13-03-2024                                    *
      *----------------------------------------------------------------*
      *    OBJETIVO....: BUSCAR GRUPO E SUBGRUPO NO ARQUIVO DO CLIE E  *
      *                  ATUALIZAR NO ARQUIVO DE SAIDA - CONTA SALARIO *
      *---------------------------------------------------------------*
      *    ARQUIVOS....:                              DE/BOOK         *
      *                        MULTIPAE            GFCTWQHF           *
      *                        ECCTACLI            I#CHEQ04           *
      *                        MULTIPAS            GFCTWQHF           *
      *                        MULTIPAI            GFCTWQHF           *
      *---------------------------------------------------------------*
      *    BOOK'S......:                                              *
      *    I#FRWKGE - BOOK DE COMUNICACAO COM FRWK2999.               *
      *    I#FRWKAR - BOOK PARA TRATAMENTO DE ERROS DE ARQUIVOS.      *
      *===============================================================*

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

           SELECT MULTIPAE     ASSIGN  TO  UT-S-MULTIPAE
                  FILE         STATUS  IS  WRK-FS-MULTIPAE.

           SELECT ECCTACLI     ASSIGN  TO  UT-S-ECCTACLI
                  FILE         STATUS  IS  WRK-FS-ECCTACLI.

           SELECT MULTIPAS     ASSIGN  TO  UT-S-MULTIPAS
                  FILE         STATUS  IS  WRK-FS-MULTIPAS.

           SELECT MULTIPAI     ASSIGN  TO  UT-S-MULTIPAI
                  FILE         STATUS  IS  WRK-FS-MULTIPAI.

      *===============================================================*
       DATA    DIVISION.
      *===============================================================*
       FILE                            SECTION.

      *---------------------------------------------------------------*
      *    INPUT   LRECL = 0250
      *---------------------------------------------------------------*
       FD  MULTIPAE
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01  FD-MULTIPAE                 PIC  X(0250).

      *---------------------------------------------------------------*
      *    INPUT   LRECL = 025  ARQUIVO CLIE
      *---------------------------------------------------------------*
       FD  ECCTACLI
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01  FD-ECCTACLI                 PIC  X(025).

      *---------------------------------------------------------------*
      *    OUTUT   LRECL = 0250 ARQUIVO SAIDA
      *---------------------------------------------------------------*
       FD  MULTIPAS
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01  FD-MULTIPAS                 PIC  X(0250).

      *---------------------------------------------------------------*
      *    OUTPUT   LRECL = 0250
      *---------------------------------------------------------------*
       FD  MULTIPAI
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01  FD-MULTIPAI                 PIC  X(0250).

      *---------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** GFCT2ZMB - INICIO DA AREA DE WORKING ***'.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE TESTE DE FILE-STATUS *'.
      *---------------------------------------------------------------*
       01  WRK-STATUS-ARQS.
           05  WRK-FS-MULTIPAE         PIC  X(002)         VALUE SPACES.
           05  WRK-FS-ECCTACLI         PIC  X(002)         VALUE SPACES.
           05  WRK-FS-MULTIPAS         PIC  X(002)         VALUE SPACES.
           05  WRK-FS-MULTIPAI         PIC  X(002)         VALUE SPACES.

      *---------------------------------------------------------------*
      * AREA PARA ACUMULADORES
      *---------------------------------------------------------------*

       01  ACU-LIDOS-MULTIPAE         PIC 9(012) COMP-3 VALUE ZEROS.
       01  ACU-LIDOS-ECCTACLI         PIC 9(012) COMP-3 VALUE ZEROS.
       01  ACU-GRAVA-MULTIPAS         PIC 9(012) COMP-3 VALUE ZEROS.
       01  ACU-GRAVA-MULTIPAI         PIC 9(012) COMP-3 VALUE ZEROS.

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
                                                           'GFCT2ZMB'.
           05  WRK-ABEND-BAT           PIC S9(004) COMP    VALUE +1111.
           05  WRK-DUMP-BAT            PIC  X(001)         VALUE 'S'.
           05  WRK-DDNAME              PIC  X(008)         VALUE SPACES.
           05  WRK-FILE-STATUS         PIC  X(002)         VALUE SPACES.
           05  WRK-ABEND               PIC  X(001)         VALUE SPACES.
               88 WRK-ABENDAR                              VALUE 'S'.
      *
       01  WRK-CHAVE-CLI.
           03 WRK-AGENCIA-CLI          PIC 9(005) VALUE ZEROS.
           03 WRK-CONTA-CLI            PIC 9(007) VALUE ZEROS.
      *
       01  WRK-CHAVE-NEGVIGE.
           03 WRK-AGENCIA-FTCONTB       PIC 9(005) VALUE ZEROS.
           03 WRK-CONTA-FTCONTB         PIC 9(007) VALUE ZEROS.
      *
       01  WRK-GRUPO-RAZAO              PIC 9(003) VALUE ZEROS.
       01  REDEFINES WRK-GRUPO-RAZAO.
           03 FILLER                    PIC 9(001).
           03 WRK-GRUPO-RAZAO-02        PIC 9(002).

       01  WRK-RAZAO.
           03  WRK-RAZAO1               PIC 9(02)  VALUE ZEROS.
           03  WRK-RAZAO2               PIC 9(02)  VALUE ZEROS.
           03  WRK-RAZAO3               PIC 9(01)  VALUE ZEROS.
       01  WRK-RAZAO-R                  REDEFINES  WRK-RAZAO
                                        PIC 9(05).

       01  WRK-JH-AGENCIA               PIC S9(005)  VALUE ZEROS.
       01  WRK-JH-AGENCIA-R             REDEFINES WRK-JH-AGENCIA.
           03  WRK-JH-AGE               PIC  9(005).

       01  WRK-JH-CONTA                 PIC S9(013)  VALUE ZEROS.
       01  WRK-JH-CONTA-R               REDEFINES  WRK-JH-CONTA.
           03  WRK-JH-CTA               PIC  9(006).
           03  WRK-JH-CTA1              PIC  9(007).

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA DEFINICAO DE LAYOUT DE ARQUIVOS ***'.
      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)     VALUE
           'LAYOUT ENCERRAENTO'.
      *---------------------------------------------------------------*
           COPY 'I#CHEQ04'.
      *
      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)     VALUE
           'LAYOUT ENTRADA'.
      *---------------------------------------------------------------*
           COPY GFCTWQHF.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA FRAMEWORK ***'.
      *---------------------------------------------------------------*
      *
       01  WRK-AREA-ERRO.
           COPY  'I#FRWKGE'.
           05  WRK-BLOCO-INFO-ERRO                    VALUE SPACES.
               10  WRK-CHAR-INFO-ERRO  PIC X(001) OCCURS 0
                                       TO 00526 TIMES
                                       DEPENDING ON FRWKGHEA-TAM-DADOS.
      *
       01  WRK-FRWKGARQ.
           COPY  'I#FRWKAR'.
      *
      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** GFCT2ZMB - FIM DA AREA DE WORKING ***'.
      *---------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*
      *
      *---------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *---------------------------------------------------------------*

           CALL    'CKRS1000'.

           PERFORM 0100-ABRIR-ARQS.

           PERFORM 3000-PROCESSAR         UNTIL
                                          WRK-FS-MULTIPAE EQUAL '10'.

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

           OPEN INPUT  MULTIPAE
                       ECCTACLI
                OUTPUT MULTIPAS
                       MULTIPAI.

           PERFORM 1000-TESTAR-FS.

           PERFORM 2000-LER-MULTIPAE.

           IF WRK-FS-MULTIPAE EQUAL '10'
              DISPLAY '******* GFCT2ZMB *****************'
              DISPLAY '     ARQUIVO MULTIPAE VAZIO       '
              DISPLAY '**********************************'
           END-IF.

           PERFORM 2100-LER-ECCTACLI.

           IF WRK-FS-ECCTACLI EQUAL '10'
              DISPLAY '******* GFCT2ZMB *****************'
              DISPLAY '     ARQUIVO ECCTACLI VAZIO       '
              DISPLAY '**********************************'
           END-IF.

      *---------------------------------------------------------------*
       0100-99-FIM.          EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1000-TESTAR-FS                  SECTION.
      *---------------------------------------------------------------*

           PERFORM 1100-TESTAR-FS-MULTIPAE.
           PERFORM 1200-TESTAR-FS-ECCTACLI.
           PERFORM 1300-TESTAR-FS-MULTIPAS.
           PERFORM 1400-TESTAR-FS-MULTIPAI.

      *---------------------------------------------------------------*
       1000-99-FIM.          EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1100-TESTAR-FS-MULTIPAE         SECTION.
      *---------------------------------------------------------------*

           MOVE 'MULTIPAE'             TO  WRK-DDNAME.

           IF WRK-FS-MULTIPAE NOT EQUAL '00'
              SET WRK-ABENDAR          TO  TRUE
              SET ERRO-ARQUIVO         TO  TRUE
              MOVE WRK-FS-MULTIPAE     TO  WRK-FILE-STATUS
              PERFORM 8200-TRATAR-ERRO-ARQ
           END-IF.

      *---------------------------------------------------------------*
       1100-99-FIM.          EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1200-TESTAR-FS-ECCTACLI         SECTION.
      *---------------------------------------------------------------*

           MOVE 'ECCTACLI'             TO  WRK-DDNAME.

           IF WRK-FS-ECCTACLI NOT EQUAL '00'
              SET WRK-ABENDAR          TO  TRUE
              SET ERRO-ARQUIVO         TO  TRUE
              MOVE WRK-FS-ECCTACLI     TO  WRK-FILE-STATUS
              PERFORM 8200-TRATAR-ERRO-ARQ
           END-IF.

      *---------------------------------------------------------------*
       1200-99-FIM.          EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1300-TESTAR-FS-MULTIPAS         SECTION.
      *---------------------------------------------------------------*

           MOVE 'MULTIPAS'             TO  WRK-DDNAME.

           IF WRK-FS-MULTIPAS NOT EQUAL '00'
              SET WRK-ABENDAR          TO  TRUE
              SET ERRO-ARQUIVO         TO  TRUE
              MOVE WRK-FS-MULTIPAS     TO  WRK-FILE-STATUS
              PERFORM 8200-TRATAR-ERRO-ARQ
           END-IF.

      *---------------------------------------------------------------*
       1300-99-FIM.          EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1400-TESTAR-FS-MULTIPAI         SECTION.
      *---------------------------------------------------------------*

           MOVE 'MULTIPAI'             TO  WRK-DDNAME.

           IF WRK-FS-MULTIPAI NOT EQUAL '00'
              SET WRK-ABENDAR          TO  TRUE
              SET ERRO-ARQUIVO         TO  TRUE
              MOVE WRK-FS-MULTIPAI     TO  WRK-FILE-STATUS
              PERFORM 8200-TRATAR-ERRO-ARQ
           END-IF.

      *---------------------------------------------------------------*
       1400-99-FIM.          EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2000-LER-MULTIPAE               SECTION.
      *---------------------------------------------------------------*

           MOVE '0003'                 TO WRK-LOCAL.
           MOVE '2000-LER-MULTIPAE'    TO FRWKGHEA-IDEN-PARAGRAFO.
           SET ARQ-READ                TO TRUE.

           READ MULTIPAE               INTO GFCTWQHF-REGISTRO-PGTO-TRAG

           IF WRK-FS-MULTIPAE EQUAL '10'
              GO                         TO 2000-99-FIM
           END-IF.

           PERFORM 1100-TESTAR-FS-MULTIPAE.

           ADD  1                        TO ACU-LIDOS-MULTIPAE.

           MOVE GFCTWQHF-AGEN-SALRL      TO WRK-JH-AGENCIA
           MOVE WRK-JH-AGE               TO WRK-AGENCIA-FTCONTB

           MOVE GFCTWQHF-CTA-SALRL       TO GFCTWQHF-CTA-SALRL
           MOVE WRK-JH-CTA1              TO WRK-CONTA-FTCONTB.

      *---------------------------------------------------------------*
       2000-99-FIM.          EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2100-LER-ECCTACLI               SECTION.
      *---------------------------------------------------------------*

           MOVE '0004'                 TO WRK-LOCAL.
           MOVE '2100-LER-ECCTACLI'    TO FRWKGHEA-IDEN-PARAGRAFO.
           SET ARQ-READ                TO TRUE.

           READ ECCTACLI               INTO CHEQ-REG.

           IF WRK-FS-ECCTACLI EQUAL '10'
              MOVE HIGH-VALUES         TO WRK-CHAVE-CLI
              GO                       TO 2100-99-FIM
           END-IF.

           PERFORM 1200-TESTAR-FS-ECCTACLI.

           ADD  1                        TO ACU-LIDOS-ECCTACLI.

           MOVE CHEQ-AGE                 TO WRK-AGENCIA-CLI.
           MOVE CHEQ-CTA                 TO WRK-CONTA-CLI.

      *---------------------------------------------------------------*
       2100-99-FIM.          EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *---------------------------------------------------------------*

           IF WRK-CHAVE-CLI EQUAL WRK-CHAVE-NEGVIGE
              PERFORM 3100-GRAVAR-MULTIPAS
              PERFORM 2000-LER-MULTIPAE
           ELSE
              IF WRK-CHAVE-CLI LESS WRK-CHAVE-NEGVIGE
                 PERFORM 2100-LER-ECCTACLI
              ELSE
                 PERFORM 3300-GRAVAR-MULTIPAI
                 PERFORM 2000-LER-MULTIPAE
              END-IF
           END-IF.

      *---------------------------------------------------------------*
       3000-99-FIM.          EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3100-GRAVAR-MULTIPAS            SECTION.
      *---------------------------------------------------------------*

           PERFORM 3200-FORMATAR-MULTIPAS-OK

           MOVE '0005'                 TO WRK-LOCAL.
           MOVE '3100-GRAVAR-MULTIPAS' TO FRWKGHEA-IDEN-PARAGRAFO.
           SET ARQ-WRITE               TO  TRUE.

           WRITE FD-MULTIPAS           FROM GFCTWQHF-REGISTRO-PGTO-TRAG

           ADD    1                    TO ACU-GRAVA-MULTIPAS.

           PERFORM 1300-TESTAR-FS-MULTIPAS.

      *---------------------------------------------------------------*
       3100-99-FIM.          EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3200-FORMATAR-MULTIPAS-OK          SECTION.
      *---------------------------------------------------------------*

           MOVE CHEQ-RAZ                 TO WRK-RAZAO-R

           MOVE WRK-RAZAO1               TO WRK-GRUPO-RAZAO-02
           MOVE WRK-GRUPO-RAZAO          TO GFCTWQHF-CGRUPO-SALRL

           MOVE WRK-RAZAO2               TO WRK-GRUPO-RAZAO-02
           MOVE WRK-GRUPO-RAZAO          TO GFCTWQHF-CSUBGR-SALRL.


      *---------------------------------------------------------------*
       3200-99-FIM.          EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3300-GRAVAR-MULTIPAI            SECTION.
      *---------------------------------------------------------------*

           MOVE '0005'                 TO WRK-LOCAL.

           MOVE 'CONTA SALARIO NAO ENCONTRADA'
                                       TO GFCTWQHF-MSG-ERRO

           MOVE '3300-GRAVAR-MULTIPAI' TO FRWKGHEA-IDEN-PARAGRAFO.
           SET ARQ-WRITE               TO  TRUE.

           WRITE FD-MULTIPAI           FROM GFCTWQHF-REGISTRO-PGTO-TRAG

           ADD    1                    TO ACU-GRAVA-MULTIPAI.

           PERFORM 1400-TESTAR-FS-MULTIPAI.

      *---------------------------------------------------------------*
       3300-99-FIM.          EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------
       4000-FINALIZA                   SECTION.
      *---------------------------------------------------------------

           SET ARQ-CLOSE               TO  TRUE.

           CLOSE MULTIPAE
                 ECCTACLI
                 MULTIPAS
                 MULTIPAI.

           PERFORM 1000-TESTAR-FS.

           PERFORM 4200-DISPLAYS.

           DISPLAY '************ PROGRAMA GFCT2ZMB *************'
           DISPLAY '*                                          *'
           DISPLAY '* PROGRAMA GFCT2ZMB ENCERRADO COM SUCESSO  *'
           DISPLAY '*                                          *'
           DISPLAY '************ PROGRAMA GFCT2ZMB *************'
           MOVE ZEROS                  TO RETURN-CODE.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4200-DISPLAYS                   SECTION.
      *----------------------------------------------------------------*

           DISPLAY '************ PROGRAMA GFCT2ZMB *****************'.
           MOVE ACU-LIDOS-MULTIPAE     TO WRK-MASCARA.
           DISPLAY '*                                              *'.
           DISPLAY '* REGISTROS LIDOS MULTIPAE....:' WRK-MASCARA.
           MOVE ACU-LIDOS-ECCTACLI     TO WRK-MASCARA.
           DISPLAY '*                                              *'.
           DISPLAY '* REGISTROS LIDOS ECCTACLI....:' WRK-MASCARA.
           MOVE ACU-GRAVA-MULTIPAS     TO WRK-MASCARA.
           DISPLAY '*                                              *'.
           DISPLAY '* REGISTROS GRAVADOS MULTIPAS.:' WRK-MASCARA.
           MOVE ACU-GRAVA-MULTIPAI     TO WRK-MASCARA.
           DISPLAY '*                                              *'.
           DISPLAY '* REGISTROS GRAVADOS MULTIPAI.:' WRK-MASCARA.
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
           DISPLAY ' ************************* GFCT2ZMB **************'.
           DISPLAY ' * FRWK2999 CHAMADO. SEGUEM SEUS PARAMETROS       '.
           DISPLAY ' *                                                '.
           DISPLAY ' * FRWKGHEA-TIPO-ERRO  = ' FRWKGHEA-TIPO-ERRO
           DISPLAY ' * FRWKGHEA-NOME-PROGR = ' FRWKGHEA-NOME-PROGRAMA
           DISPLAY ' * FRWKGHEA-IDEN-PARAG = ' FRWKGHEA-IDEN-PARAGRAFO
           DISPLAY ' * FRWKGHEA-TAM-DADOS  = ' FRWKGHEA-TAM-DADOS
           DISPLAY ' ************************* GFCT2ZMB **************'.

           CALL WRK-FRWK2999           USING WRK-AREA-ERRO.

           DISPLAY ' ************************* GFCT2ZMB **************'.
           DISPLAY ' * PARAMETROS RETORNADOS PELO FRWK2999            '.
           DISPLAY ' *                                                '.
           DISPLAY ' * FRWKGERR-COD-RETORNO    = ' FRWKGERR-COD-RETORNO
           DISPLAY ' * FRWKGERR-COD-ERRO       = ' FRWKGERR-COD-ERRO
           DISPLAY ' * FRWKGERR-COD-MENSAGEM   = ' FRWKGERR-COD-MENSAGEM
                   ' *                                                '.
           DISPLAY ' ************************* GFCT2ZMB **************'
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

