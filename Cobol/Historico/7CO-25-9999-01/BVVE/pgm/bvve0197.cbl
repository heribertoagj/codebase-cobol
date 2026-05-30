      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. BVVE0197.
       AUTHOR.     FRANCISMARA PAGNONCELLI.
      *================================================================*
      *                  B S I  T E C N O L O G I A                    *
      *----------------------------------------------------------------*
      *    PROGRAMA....: BVVE0197                                      *
      *    PROGRAMADORA: FRANCISMARA PAGNONCELLI  - BSI                *
      *    ANALISTA....: FRANCISMARA PAGNONCELLI  - BSI                *
      *    DATA........: JANEIRO/2015                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....: RECEBER RETORNO DO ASCG025                    *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *    ASCG025R - ARQUIVO RECEBIDO DA CIP                          *
      *    ASCG025S - ARQUIVO EXTRAIDO                                 *
      *================================================================*

      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*

       FILE-CONTROL.

           SELECT ASCG025R ASSIGN      TO UT-S-ASCG025R
                    FILE STATUS        IS WRK-FS-ASCG025R.

           SELECT ASCG025S ASSIGN      TO UT-S-ASCG025S
                    FILE STATUS        IS WRK-FS-ASCG025S.


      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT - ARQUIVO ASCG025R                                    *
      *            ORG. SEQUENCIAL - LRECL = 32000                     *
      *----------------------------------------------------------------*

       FD  ASCG025R
           RECORDING  MODE IS V
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-ASCG025R.
           05  FILLER               PIC  X(32000).

      *----------------------------------------------------------------*
      *    INPUT - ARQUIVO SAIDA ASCG025S                              *
      *            ORG. FIXO - LRECL = 146                             *
      *----------------------------------------------------------------*

       FD  ASCG025S
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-ASCG025S            PIC  X(146).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** BVVE0197 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ACUMULADORES ***'.
      *----------------------------------------------------------------*

       01  ACU-MULADORES.
           03  ACU-LID-ASCG025R        PIC  9(09) COMP-3   VALUE ZEROS.
           03  ACU-GRA-ASCG025S        PIC  9(09) COMP-3   VALUE ZEROS.
           03  ACU-ACEIT               PIC  9(09) COMP-3   VALUE ZEROS.
           03  ACU-REJEI               PIC  9(09) COMP-3   VALUE ZEROS.
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** VARIAVEIS DE OPERACAO DE ARQUIVOS ***'.
      *----------------------------------------------------------------*

       77  WRK-FS-ASCG025R             PIC X(02)       VALUE SPACES.
       77  WRK-FS-ASCG025S             PIC X(02)       VALUE SPACES.

       01  WRK-ABERTURA                PIC X(13)       VALUE
           ' NA ABERTURA '.
       01  WRK-GRAVACAO                PIC X(13)       VALUE
           ' NA GRAVACAO '.
       01  WRK-FECHAMENTO              PIC X(13)       VALUE
           'NO FECHAMENTO'.
       01  WRK-LEITURA                 PIC X(13)       VALUE
           ' NA LEITURA  '.

       77  WRK-FIM-ASCG025R            PIC X(01)  VALUE  'N'.
       77  WRK-BATCH                   PIC X(08)  VALUE  'BATCH'.
       77  WRK-IND                     PIC 9(02)  COMP-3 VALUE ZEROS.
       77  WRK-POS                     PIC 9(03)  COMP-3 VALUE ZEROS.
       77  WRK-MASCARA                 PIC ZZZ.ZZZ.ZZ9   VALUE ZEROS.

       01  WRK-GRUPO-ACEITOS.
           03 WRK-AC-NUM-CTRL-IF       PIC X(020)  VALUE SPACES.
           03 WRK-AC-NUM-CTRL-CIP      PIC X(020)  VALUE SPACES.
           03 WRK-AC-NUM-LIQUID        PIC X(021)  VALUE SPACES.
           03 WRK-AC-NUM-LIQUID-ERR    PIC X(008)  VALUE SPACES.
           03 WRK-AC-DTHR-MANUT        PIC X(019)  VALUE SPACES.

       01  WRK-GRUPO-REJEITADOS.
           03 WRK-REJ-NUM-CTRL-IF      PIC X(020)  VALUE SPACES.
           03 WRK-REJ-NUM-CTRL-IF-E    PIC X(008)  VALUE SPACES.
           03 WRK-REJ-NUM-LIQUID       PIC X(021)  VALUE SPACES.
           03 WRK-REJ-NUM-LIQUID-E     PIC X(008)  VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*   AREA MENSAGENS DE ERRO     *'.
      *----------------------------------------------------------------*

       01  WRK-MSG-ERRO.
          03  FILLER                   PIC X(07)   VALUE SPACES.
          03  FILLER                   PIC X(07)   VALUE
              '* ERRO '.
          03  WRK-OPERACAO             PIC X(13)   VALUE SPACES.
          03  FILLER                   PIC X(12)   VALUE
              ' DO ARQUIVO '.
          03  WRK-NOME-ARQ             PIC X(08)   VALUE SPACES.
          03  FILLER                   PIC X(17)   VALUE
              ' - FILE-STATUS = '.
          03  WRK-FILE-STATUS          PIC X(02)   VALUE SPACES.
          03  FILLER                   PIC X(02)   VALUE ' *'.


      *----------------------------------------------------------------*
       01  FILLER                      PIC X(36)         VALUE
           '* LAYOUT GENERICO ARQ. DA CIP *'.
      *----------------------------------------------------------------*

           COPY  'I#BVVEDI'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(50)         VALUE
           '*     BOOK ARQUIVO SAIDA EXTRAIDO   *'.
      *----------------------------------------------------------------*

           COPY  'I#BVVEA8'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)          VALUE
           '*  AREA DA POOL7100  *'.
      *----------------------------------------------------------------*

           COPY  'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*   FIM DA WORKING BVVE0197    *'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIAR-PROGRAMA.

           PERFORM 2000-LER-ASCG025R.

           IF  WRK-FIM-ASCG025R  EQUAL 'S'
               DISPLAY   '***********BVVE0197******************* '
               DISPLAY   '*                                    * '
               DISPLAY   '*       ARQ    ASCG025R   VAZIO      * '
               DISPLAY   '*         PROGRAMA ENCERRADO         * '
               DISPLAY   '*  FILE STATUS  =  ' WRK-FS-ASCG025R
               DISPLAY   '************************************** '
           END-IF.

           PERFORM 3000-PROCESSAR      UNTIL
                   WRK-FIM-ASCG025R   EQUAL 'S'.

           PERFORM 4000-FINALIZAR.

           GOBACK.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-INICIAR-PROGRAMA           SECTION.
      *----------------------------------------------------------------*
      *
           OPEN INPUT  ASCG025R
               OUTPUT  ASCG025S.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 1200-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1200-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1210-TESTAR-FS-ASCG025R.

           PERFORM 1220-TESTAR-FS-ASCG025S.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1210-TESTAR-FS-ASCG025R         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ASCG025R         NOT EQUAL '00' AND '04'
               MOVE 'ASCG025R'         TO WRK-NOME-ARQ
               MOVE WRK-FS-ASCG025R    TO WRK-FILE-STATUS
               MOVE WRK-MSG-ERRO       TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1220-TESTAR-FS-ASCG025S         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ASCG025S         NOT EQUAL '00'
               MOVE 'ASCG025S'         TO WRK-NOME-ARQ
               MOVE WRK-FS-ASCG025S    TO WRK-FILE-STATUS
               MOVE WRK-MSG-ERRO       TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000-LER-ASCG025R               SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO  REG-BVVEDI.
           INITIALIZE    REG-BVVEDI.

           READ ASCG025R               INTO REG-BVVEDI.

           IF  WRK-FS-ASCG025R         EQUAL '10'
               MOVE 'S'                TO   WRK-FIM-ASCG025R
               GO TO 2000-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1210-TESTAR-FS-ASCG025R.

           ADD     1                   TO ACU-LID-ASCG025R.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           IF  BVVEDI-NOME-ARQ(1:4)    EQUAL  'ASCG'
               INITIALIZE BVVEA8-ASCG023
               MOVE '0'                  TO BVVEA8-ID-REG
               MOVE BVVEDI-HEADER(1:145) TO BVVEA8-HEADER
               PERFORM 3100-GRAVAR-ASCG025S
           ELSE
               IF  BVVEDI-REGISTRO(1:6) EQUAL ALL '*'
                   NEXT SENTENCE
               ELSE
                   PERFORM 3300-MONTA-REGISTRO
               END-IF
           END-IF.

           PERFORM 2000-LER-ASCG025R.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3100-GRAVAR-ASCG025S            SECTION.
      *----------------------------------------------------------------*

           WRITE FD-REG-ASCG025S       FROM   BVVEA8-ASCG023

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO

           PERFORM 1220-TESTAR-FS-ASCG025S

           ADD 1                       TO  ACU-GRA-ASCG025S.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      * EXTRAI ARQUIVO E MOVI PARA AREA DE WORKING, PARA DEPOIS MOVER
      * PARA A SAIDA, POIS O CAMPO NUM-LIQUID PRECISA FICAR NA MESMA
      * POSICAO PARA ACEITOS E REJEITADOS
      *----------------------------------------------------------------*
       3300-MONTA-REGISTRO             SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO BVVEA8-ASCG023
           INITIALIZE BVVEA8-ASCG023
           MOVE ZEROS                  TO WRK-IND
           MOVE '1'                    TO BVVEA8-ID-REG

           IF  BVVEDI-FILLER        EQUAL  '{1000}'
               INITIALIZE BVVEA8-GRUPO-ACEITOS
               MOVE BVVEDI-REGISTRO(1:88) TO WRK-GRUPO-ACEITOS
               MOVE WRK-AC-NUM-CTRL-IF    TO BVVEA8-AC-NUM-CTRL-IF
               MOVE WRK-AC-NUM-CTRL-CIP   TO BVVEA8-AC-NUM-CTRL-CIP
               MOVE WRK-AC-NUM-LIQUID     TO BVVEA8-AC-NUM-LIQUID
               MOVE WRK-AC-DTHR-MANUT     TO BVVEA8-AC-DTHR-MANUT
               MOVE 'A'                   TO BVVEA8-SITUACAO
               ADD 1                      TO ACU-ACEIT
           ELSE
               INITIALIZE BVVEA8-GRUPO-REJEITADOS
               MOVE BVVEDI-REGISTRO(1:57)  TO WRK-GRUPO-REJEITADOS
               MOVE WRK-REJ-NUM-CTRL-IF    TO BVVEA8-REJ-NUM-CTRL-IF
               MOVE WRK-REJ-NUM-CTRL-IF-E  TO BVVEA8-REJ-NUM-CTRL-IF-E
               MOVE WRK-REJ-NUM-LIQUID     TO BVVEA8-REJ-NUM-LIQUID
               MOVE WRK-REJ-NUM-LIQUID-E   TO BVVEA8-REJ-NUM-LIQUID-E
               MOVE 'R'                    TO BVVEA8-SITUACAO
               MOVE 58                     TO WRK-POS
               ADD 1                       TO ACU-REJEI
               PERFORM UNTIL WRK-IND                 EQUAL 5 OR
                    (BVVEDI-REGISTRO (WRK-POS : 6) NOT EQUAL '{2100}')
                   ADD 6                   TO WRK-POS
                   ADD 1                   TO WRK-IND
                   MOVE BVVEDI-REGISTRO(WRK-POS : 10)
                                           TO BVVEA8-REJ-OCORR(WRK-IND)
                   ADD 10                  TO WRK-POS
               END-PERFORM
           END-IF.

           PERFORM 3100-GRAVAR-ASCG025S.

      *---------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 4100-EMITIR-DISPLAY.

           CLOSE ASCG025R
                 ASCG025S.

           MOVE WRK-FECHAMENTO         TO  WRK-OPERACAO.

           PERFORM 1200-TESTAR-FILE-STATUS.

           GOBACK.
      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4100-EMITIR-DISPLAY             SECTION.
      *----------------------------------------------------------------*

           DISPLAY '*************** BVVE0197 ******************'
           DISPLAY '*                                         *'

           MOVE ACU-LID-ASCG025R      TO WRK-MASCARA.
           DISPLAY '*   REG. LIDOS ASCG025R: ' WRK-MASCARA '      *'

           MOVE ACU-REJEI             TO WRK-MASCARA.
           DISPLAY '*   REG. REJEITADOS    : ' WRK-MASCARA '      *'

           MOVE ACU-ACEIT             TO WRK-MASCARA.
           DISPLAY '*   REG. ACEITOS       : ' WRK-MASCARA '      *'

           MOVE ACU-GRA-ASCG025S     TO WRK-MASCARA.
           DISPLAY '*   REG. GRAVA ASCG025S: ' WRK-MASCARA '      *'

           DISPLAY '*                                         *'

           DISPLAY '*************** BVVE0197 ******************'.


      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'BVVE0197'             TO ERR-PGM.

           CALL 'BRAD7100'         USING WRK-BATCH
                                         ERRO-AREA.

           GOBACK.
      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
