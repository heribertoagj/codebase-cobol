      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. GFCT2013.
       AUTHOR.     INFOSERVER.
      *================================================================*
      *                    I N F O S E R V E R                         *
      *----------------------------------------------------------------*
      *                                                                *
      *      PROGRAMA     : GFCT2013                                 *
      *      PROGRAMADOR  : INFOSERVER                                 *
      *      ANALISTA     : INFOSERVER                                 *
      *      DATA         : 15/02/2017                                 *
      *      OBJETIVO     : BALANCE-LINE ENTRE OS ARQUIVOS PARA IDENTI-*
      *                     FICACAO DO TIPO DE PESSOA PF OU PJ.        *
      *                                                                *
      *      ARQUIVOS:                                                 *
      *         DDNAME                           INCLUDE/BOOK          *
      *         EGFCTAW2                           GFCTWAW2            *
      *         EGFCTCAD                           I#CLIE20            *
      *         SGFCTAW2                           GFCTWAW2            *
      *                                                                *
      *      MODULOS CHAMADOS:                                         *
      *         POOL7100  -  TRATAMENTO DE ERRO                        *
      *                                                                *
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
           SELECT  EGFCTAW2   ASSIGN   TO UT-S-EGFCTAW2
                       FILE   STATUS   IS WRK-FS-EGFCTAW2.

           SELECT  EGFCTCAD   ASSIGN   TO UT-S-EGFCTCAD
                       FILE   STATUS   IS WRK-FS-CLIE20.

           SELECT  SGFCTAW2   ASSIGN   TO UT-S-SGFCTAW2
                       FILE   STATUS   IS WRK-FS-SGFCTAW2.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  INPUT:       ORG. SEQUENCIAL   -   LRECL = 083 CTAS INATIVAS  *
      *----------------------------------------------------------------*
       FD  EGFCTAW2
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-EGFCTAW2  PIC X(083).

      *----------------------------------------------------------------*
      *  INPUT:       ORG. SEQUENCIAL   -   LRECL = 180 CTAS ATIVAS    *
      *----------------------------------------------------------------*

       FD  EGFCTCAD
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-EGFCTCAD  PIC X(180).

      *----------------------------------------------------------------*
      *  OUTPUT:      ORG. SEQUENCIAL   -   LRECL = 083                *
      *----------------------------------------------------------------*

       FD  SGFCTAW2
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-SGFCTAW2  PIC X(083).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       77  FILLER PIC X(32) VALUE  '* GFCT2013 - INICIO DA WORKING *'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       77  FILLER PIC X(32) VALUE  '*         ACUMULADORES         *'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01  WRK-ACUMULADORES.
           03  ACU-LIDOS-EGFCTAW2      PIC  9(10) COMP-3 VALUE ZEROS.
           03  ACU-LIDOS-EGFCTCAD      PIC  9(10) COMP-3 VALUE ZEROS.
           03  ACU-GRAVA-SGFCTAW2      PIC  9(10) COMP-3 VALUE ZEROS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       77  FILLER PIC X(32) VALUE  '*         DIVERSOS             *'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       77  WRK-BATCH                   PIC  X(08) VALUE 'BATCH'.
       77  WRK-DISP                    PIC  ZZZZ.ZZZ.ZZ9 VALUE SPACES.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       77  FILLER PIC X(32) VALUE  '*  AREAS P/ TESTE FILE STATUS  *'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       77  WRK-FS-EGFCTAW2             PIC  X(02) VALUE SPACES.
       77  WRK-FS-CLIE20               PIC  X(02) VALUE SPACES.
       77  WRK-FS-SGFCTAW2             PIC  X(02) VALUE SPACES.
       77  WRK-ABERTURA                PIC  X(13) VALUE 'NA ABERTURA'.
       77  WRK-LEITURA                 PIC  X(13) VALUE 'NA LEITURA'.
       77  WRK-FECHAMENTO              PIC  X(13) VALUE 'NO FECHAMENTO'.
       77  WRK-GRAVACAO                PIC  X(13) VALUE 'NA GRAVACAO  '.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       77  FILLER PIC X(32) VALUE  '*  REDEFINICAO DE AREAS *'.

VI0317 01  WRK-CONTA-13                PIC  9(013)         VALUE ZEROS.
VI0317 01  FILLER                      REDEFINES WRK-CONTA-13.
VI0317     03  FILLER                  PIC  X(006).
VI0317     03  WRK-CONTA-7             PIC  9(007).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       77  FILLER PIC X(32) VALUE  '*  CHAVE DE COMPARACAO   *'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01  WRK-GFCTWAW2-AGE-CTA.
VI0317  05  WRK-GFCT-AGENCIA           PIC  9(005)         VALUE ZEROS.
VI0317  05  WRK-GFCT-CONTA             PIC  9(007)         VALUE ZEROS.

       01  WRK-CLIE20-AGE-CTA.
VI0317  05  WRK-CLIE-AGENCIA           PIC  9(005)         VALUE ZEROS.
VI0317  05  WRK-CLIE-CONTA             PIC  9(007)         VALUE ZEROS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       77  FILLER PIC X(32) VALUE  '*      AREA DE MENSAGENS       *'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01  WRK-FS-ERRO.
           03  FILLER                  PIC  X(09) VALUE SPACES.
           03  FILLER                  PIC  X(05) VALUE 'ERRO'.
           03  WRK-OPERACAO            PIC  X(13) VALUE SPACES.
           03  FILLER                  PIC  X(12) VALUE ' DO ARQUIVO '.
           03  WRK-ARQUIVO             PIC  X(08) VALUE SPACES.
           03  FILLER                  PIC  X(17) VALUE
               ' - FILE STATUS = '.
           03  WRK-FILE-STATUS         PIC  X(02) VALUE SPACES.


      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       77  FILLER PIC X(32) VALUE  '*   AREA DO EGFCTAW2 E SGFCTAW2*'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

           COPY GFCTWAW2.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       77  FILLER PIC X(32) VALUE  '*   AREA DO LAYOUT CONTROLE    *'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

           COPY I#CLIE20.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       77  FILLER PIC X(32) VALUE  '*       AREA DA POOL7100       *'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

           COPY POL7100C.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       77  FILLER PIC X(32) VALUE  '*      AREA DO DRESSCODE       *'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       77  FILLER PIC X(32) VALUE  '*  GFCT2013 - FIM DA WORKING   *'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-VERIFICAR-VAZIO.

           PERFORM 3000-PROCESSAR UNTIL
                  (WRK-CLIE20-AGE-CTA     EQUAL '999999999999'
                   AND
                   WRK-GFCTWAW2-AGE-CTA   EQUAL '999999999999')

           PERFORM 4000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           OPEN    INPUT   EGFCTAW2
                           EGFCTCAD
                   OUTPUT  SGFCTAW2.

           MOVE    WRK-ABERTURA        TO      WRK-OPERACAO.
           PERFORM 1100-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-EGFCTAW2.

           PERFORM 1120-TESTAR-FS-EGFCTCAD.

           PERFORM 1130-TESTAR-FS-SGFCTAW2.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1110-TESTAR-FS-EGFCTAW2         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-EGFCTAW2         NOT EQUAL '00'
               MOVE 'EGFCTAW2'         TO WRK-ARQUIVO
               MOVE WRK-FS-EGFCTAW2    TO WRK-FILE-STATUS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-ERRO        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1120-TESTAR-FS-EGFCTCAD         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-CLIE20           NOT EQUAL '00'
               MOVE 'EGFCTCAD'         TO WRK-ARQUIVO
               MOVE WRK-FS-CLIE20      TO WRK-FILE-STATUS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-ERRO        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1130-TESTAR-FS-SGFCTAW2         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SGFCTAW2         NOT EQUAL '00'
               MOVE 'SGFCTAW2'         TO WRK-ARQUIVO
               MOVE WRK-FS-SGFCTAW2    TO WRK-FILE-STATUS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-ERRO        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-LER-EGFCTAW2.

           IF  ACU-LIDOS-EGFCTAW2 EQUAL ZEROS
               DISPLAY '*********** GFCT2013 ***********'
               DISPLAY '*                              *'
               DISPLAY '*    ARQUIVO EGFCTAW2 VAZIO    *'
               DISPLAY '*      PROGRAMA ENCERRADO      *'
               DISPLAY '*                              *'
               DISPLAY '*********** GFCT2013 ***********'
               STOP RUN
           END-IF.

           PERFORM 2200-LER-EGFCTCAD.

           IF  ACU-LIDOS-EGFCTCAD EQUAL ZEROS
               DISPLAY '*********** GFCT2013 ***********'
               DISPLAY '*                              *'
               DISPLAY '*    ARQUIVO EGFCTCAD VAZIO    *'
               DISPLAY '*      PROGRAMA ENCERRADO      *'
               DISPLAY '*                              *'
               DISPLAY '*********** GFCT2013 ***********'
               STOP RUN
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2100-LER-EGFCTAW2               SECTION.
      *----------------------------------------------------------------*

           READ EGFCTAW2               INTO   GFCTWAW2-REGISTRO.

           IF  WRK-FS-EGFCTAW2         EQUAL '10'
               MOVE 99999              TO  WRK-GFCT-AGENCIA
               MOVE 9999999            TO  WRK-GFCT-CONTA
               GO                      TO 2100-99-FIM
           END-IF.

           MOVE    WRK-LEITURA         TO WRK-OPERACAO.
           PERFORM 1110-TESTAR-FS-EGFCTAW2.
           ADD     1                   TO ACU-LIDOS-EGFCTAW2

VI0317     MOVE GFCTWAW2-AGE-DEB       TO WRK-GFCT-AGENCIA.
VI0317     MOVE GFCTWAW2-CTA-DEB       TO WRK-CONTA-13.
VI0317     MOVE WRK-CONTA-7            TO WRK-GFCT-CONTA.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2200-LER-EGFCTCAD               SECTION.
      *----------------------------------------------------------------*

           READ EGFCTCAD               INTO   REG-CADACLIE.

           IF  WRK-FS-CLIE20           EQUAL '10'
               MOVE 99999              TO  WRK-CLIE-AGENCIA
               MOVE 9999999            TO  WRK-CLIE-CONTA
               GO                      TO  2200-99-FIM
           END-IF.

           MOVE    WRK-LEITURA         TO WRK-OPERACAO.
           PERFORM 1120-TESTAR-FS-EGFCTCAD.
           ADD     1                   TO ACU-LIDOS-EGFCTCAD

VI0317     MOVE CAD-AGENCIA            TO WRK-CLIE-AGENCIA.
VI0317     MOVE CAD-CONTA              TO WRK-CLIE-CONTA.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           IF   WRK-CLIE20-AGE-CTA     EQUAL   WRK-GFCTWAW2-AGE-CTA
                IF  CAD-FILIAL         EQUAL   ZEROS
                    MOVE 'PF'             TO   GFCTWAW2-TIPO-PESSOA
                ELSE
                    MOVE 'PJ'             TO   GFCTWAW2-TIPO-PESSOA
                END-IF
                PERFORM 3100-GRAVA-SGFCTAW2
                PERFORM 2100-LER-EGFCTAW2
                PERFORM 2200-LER-EGFCTCAD
           ELSE
               IF   WRK-CLIE20-AGE-CTA     LESS   WRK-GFCTWAW2-AGE-CTA
                    PERFORM 2200-LER-EGFCTCAD
               ELSE
                    DISPLAY '* REGISTRO INATIVAS NAO EXISTE NO CLIE '
                    DISPLAY '* CHAVE DO CADASTRO ' WRK-CLIE20-AGE-CTA
                    DISPLAY '* CHAVE DO MOVTO    ' WRK-GFCTWAW2-AGE-CTA
VI0317              MOVE '  '             TO   GFCTWAW2-TIPO-PESSOA
VI0317              PERFORM 3100-GRAVA-SGFCTAW2
VI0317              PERFORM 2100-LER-EGFCTAW2
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3100-GRAVA-SGFCTAW2             SECTION.
      *----------------------------------------------------------------*

           MOVE    WRK-GRAVACAO        TO      WRK-OPERACAO.

           WRITE   FD-REG-SGFCTAW2     FROM    GFCTWAW2-REGISTRO

           PERFORM 1130-TESTAR-FS-SGFCTAW2.

           ADD     1                   TO      ACU-GRAVA-SGFCTAW2.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           DISPLAY '****************** GFCT2013 ******************'
           DISPLAY '*                                            *'

           MOVE ACU-LIDOS-EGFCTAW2     TO WRK-DISP
           DISPLAY '* REGISTROS LIDOS EGFCTAW2    - ' WRK-DISP ' *'

           MOVE ACU-LIDOS-EGFCTCAD     TO WRK-DISP
           DISPLAY '* REGISTROS LIDOS EGFCTCAD    - ' WRK-DISP ' *'

           MOVE ACU-GRAVA-SGFCTAW2     TO WRK-DISP
           DISPLAY '* REGISTROS GRAVADOS SGFCTAW2 - ' WRK-DISP ' *'

           DISPLAY '*                                            *'
           DISPLAY '****************** GFCT2013 ******************'.

           CLOSE   EGFCTAW2
                   EGFCTCAD
                   SGFCTAW2.

           MOVE    WRK-FECHAMENTO      TO      WRK-OPERACAO.
           PERFORM 1100-TESTAR-FILE-STATUS.

           STOP RUN.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT2013'             TO      ERR-PGM.

           CALL 'POOL7100'             USING   WRK-BATCH
                                               ERRO-AREA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
