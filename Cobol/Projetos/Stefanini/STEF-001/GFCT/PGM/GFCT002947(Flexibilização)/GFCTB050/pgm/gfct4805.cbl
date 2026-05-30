      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT4805.
       AUTHOR.     HOMI - SONDA.

      *================================================================*
      *                 SONDA                                          *
      *----------------------------------------------------------------*
      *    PROGRAMA....:  GFCT4805                                     *
      *    SISTEMA.....:  GFCT - GFCT                                  *
      *    PROGRAMADOR.:  HOMI - SONDA                                 *
      *    ANALISTA....:  RODRIGO RIOS - SONDA                         *
      *    GRUPO.......:  50                                           *
      *    DATA........:  11 DEC 2014 - 21:26:23                       *
      *----------------------------------------------------------------*
      *    OBJETIVO....:  BALANCE LINE ENTRE:                          *
      *                   ARQ. DE COMUNICACAO GFCT E TRPN              *
      *                   IMAGEM DA TABELA GFCTB050                    *
      *                   OBTEM FLEXIBILIZACAO POR CPF/CNPJ            *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME           I/O        INCLUDE/BOOK        *
      *                GFCTTRPN          I         GFCTWE01            *
      *                GFCTB050          I         GFCTWE02            *
      *                GFCTTRPS          O         GFCTWE01            *
      *----------------------------------------------------------------*
      *    TABELAS.....:                                               *
      *                TBNAME           I/O        INCLUDE/DCLGEN      *
      *                FLEXZ_AGPTO_CTA   I         GFCTB049            *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    GFCTWE01 - BOOK DE ENTRADA - ARQ. DE COMUNICACAO GFCT E TRPN*
      *    GFCTWE02 - BOOK DE ENTRADA - IMAGEM DA TABELA GFCTB050      *
      *    GFCTWE01 - BOOK DE SAIDA   - ARQ. DE COMUNICACAO GFCT E TRPN*
      *    I#FRWKGE - GRAVAR LOG DE ERRO P/ BATCH                      *
      *    I#FRWKAR - BOOK PARA TRATAMENTO DE ERROS DE ARQUIVOS        *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    CKRS1000 - MODULO PARA INDICAR PROCESSAMWENTO               *
      *    FRWK2999 - GRAVAR LOG DE ERRO P/ BATCH                      *
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

           SELECT GFCTTRPN ASSIGN      TO   UT-S-GFCTTRPN
                      FILE STATUS      IS   WRK-FS-GFCTTRPN.

           SELECT GFCTB050 ASSIGN      TO   UT-S-GFCTB050
                      FILE STATUS      IS   WRK-FS-GFCTB050.

           SELECT GFCTTRPS ASSIGN      TO   UT-S-GFCTTRPS
                      FILE STATUS      IS   WRK-FS-GFCTTRPS.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *   INPUT:   ARQUIVO DE ENTRADA - ARQ. DE COMUNICACAO GFCT E TRPN*
      *            ORG. SEQUENCIAL    - LRECL   = 146                  *
      *----------------------------------------------------------------*
       FD  GFCTTRPN
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-GFCTTRPN                 PIC  X(146).

      *----------------------------------------------------------------*
      *   INPUT:   ARQUIVO DE ENTRADA - IMAGEM DA TABELA GFCTB050      *
      *            ORG. SEQUENCIAL    - LRECL   = 41                   *
      *----------------------------------------------------------------*
       FD  GFCTB050
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-GFCTB050                 PIC  X(41).

      *----------------------------------------------------------------*
      *   OUTPUT:ARQUIVO DE SAIDA    - ARQ. DE COMUNICACAO GFCT E TRPN *
      *          ORG. SEQUENCIAL     -   LRECL  = 146                  *
      *----------------------------------------------------------------*
       FD  GFCTTRPS
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-GFCTTRPS                 PIC  X(146).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE TRABALHO                           ***'.
      *----------------------------------------------------------------*

       01  WRK-PER-CBR-TARIF-T         PIC  X(05) VALUE 'NDSQM'.
       01  REDEFINES WRK-PER-CBR-TARIF-T.
           03 OCCURS 5 TIMES.
              05 WRK-PER-CBR-TARIF     PIC  X(01).
       01  WRK-ADD                     PIC  9(02) COMP-3 VALUE ZEROS.
       01  WRK-SQLCODE-AUX             PIC S9(09) VALUE ZEROS.
       01  REDEFINES WRK-SQLCODE-AUX.
           03                          PIC  X(6).
           03 WRK-SQLCODE-9-3          PIC  9(3).

       01  WRK-VNEGOC-FLEXZ-CS         PIC +9(011)V9(02) VALUE ZEROS.
       01  REDEFINES WRK-VNEGOC-FLEXZ-CS.
           03                          PIC  X.
           03  WRK-VNEGOC-FLEXZ-SS     PIC  9(011)V9(02).

       01  WRK-PDESC-FLEXZ-CS          PIC +9(007)V9(04) VALUE ZEROS.
       01  REDEFINES WRK-PDESC-FLEXZ-CS.
           03                          PIC  X.
           03  WRK-PDESC-FLEXZ-SS      PIC  9(007)V9(04).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       77  FILLER                      PIC  X(050)        VALUE
           '*** INICIO DA WORKING-STORAGE SECTION         ****'.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ACUMULADORES                       ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01  WRK-ACUMULADORES.

           03 WRK-LIDOS-GFCTTRPN       PIC  9(009) COMP-3  VALUE ZEROS.
           03 WRK-LIDOS-GFCTB050       PIC  9(009) COMP-3  VALUE ZEROS.
           03 WRK-LIDOS-TB-GFCTB049    PIC  9(009) COMP-3  VALUE ZEROS.
           03 WRK-GRAVADOS-GFCTTRPS    PIC  9(009) COMP-3  VALUE ZEROS.
           03 WRK-GRAVADOS-GFCTTRPS-CF PIC  9(009) COMP-3  VALUE ZEROS.
           03 WRK-GRAVADOS-GFCTTRPS-SF PIC  9(009) COMP-3  VALUE ZEROS.
           03 WRK-DISPLAY              PIC +Z(017)9        VALUE ZEROS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE TESTE DE FILE-STATUS               ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01  WRK-FS-GFCTTRPN             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-GFCTB050             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-GFCTTRPS             PIC  X(002)         VALUE SPACES.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE CHAVES                             ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01  WRK-CHAVE-GFCTTRPN.
ST2506*    03 WRK-KEY1-CPF-CNPJ-PRINC      PIC S9(09)    VALUE ZEROS.
ST2506*    03 WRK-KEY1-CPF-CNPJ-FILIAL     PIC S9(05)    VALUE ZEROS.
ST2506     03 WRK-KEY1-CPF-CNPJ-PRINC      PIC X(09)    VALUE SPACES.
ST2506     03 WRK-KEY1-CPF-CNPJ-FILIAL     PIC X(04)    VALUE SPACES.
           03 WRK-KEY1-CPF-CNPJ-CONTR      PIC S9(03)    VALUE ZEROS.
           03 WRK-KEY1-CSERVC-TARIF-GFCT   PIC S9(05)    VALUE ZEROS.

       01  WRK-CHAVE-GFCTB050.
ST2508*    03 WRK-KEY2-CCGC-CPF            PIC S9(09)    VALUE ZEROS.
ST2508*    03 WRK-KEY2-CFLIAL-CGC          PIC S9(05)    VALUE ZEROS.
ST2508     03 WRK-KEY2-CCGC-CPF            PIC X(09)    VALUE SPACES.
ST2508     03 WRK-KEY2-CFLIAL-CGC          PIC X(04)    VALUE SPACES.
           03 WRK-KEY2-CCTRL-CPF-CGC       PIC S9(03)    VALUE ZEROS.
           03 WRK-KEY2-CSERVC-TARIF        PIC S9(05)    VALUE ZEROS.

       01  WRK-CHAVE-GFCTTRPN-ANT.
ST2506*    03 WRK-K1-CPF-CNPJ-PRINC-ANT    PIC S9(09)  VALUE ZEROS.
ST2506*    03 WRK-K1-CPF-CNPJ-FILIAL-ANT   PIC S9(05)  VALUE ZEROS.
ST2506     03 WRK-K1-CPF-CNPJ-PRINC-ANT    PIC X(09)  VALUE SPACES.
ST2506     03 WRK-K1-CPF-CNPJ-FILIAL-ANT   PIC X(04)  VALUE SPACES.
           03 WRK-K1-CPF-CNPJ-CONTR-ANT    PIC S9(03)  VALUE ZEROS.
           03 WRK-K1-CSERVC-TARIF-GFCT-ANT PIC S9(05)  VALUE ZEROS.

       01  WRK-CHAVE-GFCTB050-ANT.
ST2506*    03 WRK-K2-CCGC-CPF-ANT            PIC S9(09)  VALUE ZEROS.
ST2506*    03 WRK-K2-CFLIAL-CGC-ANT          PIC S9(05)  VALUE ZEROS.
ST2506     03 WRK-K2-CCGC-CPF-ANT            PIC X(09)  VALUE SPACES.
ST2506     03 WRK-K2-CFLIAL-CGC-ANT          PIC X(04)  VALUE SPACES.
           03 WRK-K2-CCTRL-CPF-CGC-ANT       PIC S9(03)  VALUE ZEROS.
           03 WRK-K2-CSERVC-TARIF-ANT        PIC S9(05)  VALUE ZEROS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ENTRADA GFCTTRPN                   ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE SAIDA GFCTTRPS                     ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       COPY 'GFCTWE01'.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ENTRADA GFCTB050                   ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       COPY 'GFCTWE02'.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DO FRWK2999                           ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01  WRK-FRWK2999                PIC  X(008)         VALUE
           'FRWK2999'.

       01  WRK-AREA-ERRO.
           COPY 'I#FRWKGE'.
           05 WRK-BLOCO-INFO-ERRO.
              10 WRK-CHAR-INFO-ERRO    PIC  X(001) OCCURS 0 TO 30000
                                       TIMES DEPENDING ON
                                       FRWKGHEA-TAM-DADOS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA P/ FORMATACAO DE ERRO DE ARQUIVO      ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01  WRK-AREA-ERRO-ARQUIVO.
           COPY 'I#FRWKAR'.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                      PIC  X(50)  VALUE
           '*** AREA P/ FORMATACAO DE ERRO DE DB2          ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01  WRK-AREA-ERRO-DB2.
           COPY 'I#FRWKDB'.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA BRAD0450                           ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01  WRK-AREA-BRAD0450.
           05 WRK-0450-ABEND-BAT       PIC S9(004) COMP    VALUE +1111.
           05 WRK-0450-DUMP-BAT        PIC  X(001)         VALUE 'S'.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO COM CKRS0105           ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       77  WRK-CKRS0105            PIC  X(008)         VALUE
           'CKRS0105'.

           COPY 'I#CKRS04'.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREAS DB2                                  ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

           EXEC SQL INCLUDE SQLCA      END-EXEC.

           EXEC SQL INCLUDE GFCTB049   END-EXEC.

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

           MOVE '0000-ROTINA-PRINCIPAL'   TO   FRWKGHEA-IDEN-PARAGRAFO.

           PERFORM 1000-INICIAR.

           PERFORM 2000-VERIFICAR-VAZIO.

           PERFORM 3000-PROCESSAR
             UNTIL WRK-CHAVE-GFCTTRPN   EQUAL     HIGH-VALUES.

           PERFORM 9000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           MOVE '1000-INICIAR'         TO FRWKGHEA-IDEN-PARAGRAFO.

           CALL 'CKRS1000'.

           PERFORM 7000-INICIAR-CKRS0105.

           INITIALIZE FRWKGHEA-REGISTRO
                      FRWKGARQ-REGISTRO.

           OPEN INPUT   GFCTTRPN
                        GFCTB050
                OUTPUT  GFCTTRPS.

           PERFORM 1100-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           MOVE '1100-TESTAR-FILE-STATUS' TO   FRWKGHEA-IDEN-PARAGRAFO.

           PERFORM 1110-TESTAR-FS-GFCTTRPN.

           PERFORM 1120-TESTAR-FS-GFCTB050.

           PERFORM 1130-TESTAR-FS-GFCTTRPS.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1110-TESTAR-FS-GFCTTRPN         SECTION.
      *----------------------------------------------------------------*

           MOVE '1110-TESTAR-FS-GFCTTRPN' TO   FRWKGHEA-IDEN-PARAGRAFO.

           IF  WRK-FS-GFCTTRPN         NOT EQUAL  '00'
               MOVE 'GFCTTRPN'         TO FRWKGARQ-NOME-ARQUIVO
               MOVE WRK-FS-GFCTTRPN    TO FRWKGARQ-FILE-STATUS
               PERFORM 9100-FORMATAR-ERRO-ARQUIVO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1120-TESTAR-FS-GFCTB050         SECTION.
      *----------------------------------------------------------------*

           MOVE '1120-TESTAR-FS-GFCTB050' TO   FRWKGHEA-IDEN-PARAGRAFO.

           IF  WRK-FS-GFCTB050         NOT EQUAL  '00'
               MOVE 'GFCTB050'         TO FRWKGARQ-NOME-ARQUIVO
               MOVE WRK-FS-GFCTB050    TO FRWKGARQ-FILE-STATUS
               PERFORM 9100-FORMATAR-ERRO-ARQUIVO
           END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1130-TESTAR-FS-GFCTTRPS         SECTION.
      *----------------------------------------------------------------*

           MOVE '1130-TESTAR-FS-GFCTTRPS' TO   FRWKGHEA-IDEN-PARAGRAFO.

           IF  WRK-FS-GFCTTRPS         NOT EQUAL  '00'
               MOVE 'GFCTTRPS'         TO FRWKGARQ-NOME-ARQUIVO
               MOVE WRK-FS-GFCTTRPS    TO FRWKGARQ-FILE-STATUS
               PERFORM 9100-FORMATAR-ERRO-ARQUIVO
           END-IF.

      *----------------------------------------------------------------*
       1130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           MOVE '2000-VERIFICAR-VAZIO' TO   FRWKGHEA-IDEN-PARAGRAFO.

           PERFORM 2100-LER-GFCTTRPN.

           IF (WRK-LIDOS-GFCTTRPN      EQUAL     ZEROS)
               DISPLAY '************ GFCT4805 ************'
               DISPLAY '*                                *'
               DISPLAY '*     ARQUIVO GFCTTRPN VAZIO     *'
               DISPLAY '*       PROGRAMA ENCERRADO       *'
               DISPLAY '*                                *'
               DISPLAY '************ GFCT4805 ************'
               MOVE 'GFCTTRPN'        TO FRWKGARQ-NOME-ARQUIVO
               MOVE WRK-FS-GFCTTRPN   TO FRWKGARQ-FILE-STATUS
               PERFORM 9100-FORMATAR-ERRO-ARQUIVO
               PERFORM 9000-FINALIZAR
           END-IF.

           PERFORM 2200-LER-GFCTB050.

           IF (WRK-LIDOS-GFCTB050      EQUAL     ZEROS)
               DISPLAY '************ GFCT4805 ************'
               DISPLAY '*                                *'
               DISPLAY '*     ARQUIVO GFCTB050 VAZIO     *'
               DISPLAY '*       PROGRAMA ENCERRADO       *'
               DISPLAY '*                                *'
               DISPLAY '************ GFCT4805 ************'
               MOVE 'GFCTB050'        TO FRWKGARQ-NOME-ARQUIVO
               MOVE WRK-FS-GFCTB050   TO FRWKGARQ-FILE-STATUS
               PERFORM 9100-FORMATAR-ERRO-ARQUIVO
               PERFORM 9000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2100-LER-GFCTTRPN               SECTION.
      *----------------------------------------------------------------*

           MOVE '2100-LER-GFCTTRPN'    TO   FRWKGHEA-IDEN-PARAGRAFO.

           READ GFCTTRPN               INTO GFCTWE01-REG.

           IF WRK-FS-GFCTTRPN          EQUAL     '10'
               MOVE HIGH-VALUES        TO   WRK-CHAVE-GFCTTRPN
               GO TO 2100-99-FIM
           END-IF.

           PERFORM 1110-TESTAR-FS-GFCTTRPN.
           ADD  1  TO   WRK-LIDOS-GFCTTRPN.
           PERFORM 2110-MONTAR-CHAVE-GFCTTRPN.

           IF WRK-CHAVE-GFCTTRPN LESS THAN WRK-CHAVE-GFCTTRPN-ANT
              DISPLAY '************ GFCT4805 ************'
              DISPLAY '*                                *'
              DISPLAY '*     ARQUIVO NAO CLASSIFICADO   *'
              DISPLAY '*       PROGRAMA ENCERRADO       *'
              DISPLAY '* CHAVE GFCTTRPN ANTERIOR        *'
              MOVE WRK-K1-CPF-CNPJ-PRINC-ANT TO WRK-DISPLAY
              DISPLAY '*   CPF-CNPJ-PRINC-ANT ..........= ' WRK-DISPLAY
              MOVE WRK-K1-CPF-CNPJ-FILIAL-ANT TO WRK-DISPLAY
              DISPLAY '*   CPF-CNPJ-FILIAL-ANT .........= ' WRK-DISPLAY
              MOVE WRK-K1-CPF-CNPJ-CONTR-ANT TO WRK-DISPLAY
              DISPLAY '*   CPF-CNPJ-CONTR-ANT ..........= ' WRK-DISPLAY
              DISPLAY '*--------------------------------*'
              DISPLAY '* CHAVE GFCTTRPN LIDA            *'
              MOVE WRK-KEY1-CPF-CNPJ-PRINC TO WRK-DISPLAY
              DISPLAY '*   CPF-CNPJ-PRINC...............= ' WRK-DISPLAY
              MOVE WRK-KEY1-CPF-CNPJ-FILIAL TO WRK-DISPLAY
              DISPLAY '*   CPF-CNPJ-FILIAL..............= ' WRK-DISPLAY
              MOVE WRK-KEY1-CPF-CNPJ-CONTR TO WRK-DISPLAY
              DISPLAY '*   CPF-CNPJ-CONTR...............= ' WRK-DISPLAY
              DISPLAY '************ GFCT4805 ************'
              MOVE 'GFCTTRPN'        TO FRWKGARQ-NOME-ARQUIVO
              MOVE WRK-FS-GFCTTRPN   TO FRWKGARQ-FILE-STATUS
              PERFORM 9100-FORMATAR-ERRO-ARQUIVO
              PERFORM 9000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2110-MONTAR-CHAVE-GFCTTRPN      SECTION.
      *----------------------------------------------------------------*

           MOVE '2110-MONTAR-CHAVE-GFCTTRPN' TO FRWKGHEA-IDEN-PARAGRAFO.

           MOVE WRK-CHAVE-GFCTTRPN TO WRK-CHAVE-GFCTTRPN-ANT.

           MOVE WE01-CPF-CNPJ-PRINC
             TO WRK-KEY1-CPF-CNPJ-PRINC.

           MOVE WE01-CPF-CNPJ-FILIAL
             TO WRK-KEY1-CPF-CNPJ-FILIAL.

           MOVE WE01-CPF-CNPJ-CONTR
             TO WRK-KEY1-CPF-CNPJ-CONTR.

           MOVE WE01-CSERVC-TARIF-GFCT
             TO WRK-KEY1-CSERVC-TARIF-GFCT.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2200-LER-GFCTB050               SECTION.
      *----------------------------------------------------------------*

           MOVE '2200-LER-GFCTB050'    TO   FRWKGHEA-IDEN-PARAGRAFO.

           READ GFCTB050               INTO GFCTWE02-REGISTRO.

           IF WRK-FS-GFCTB050          EQUAL     '10'
               MOVE HIGH-VALUES        TO   WRK-CHAVE-GFCTB050
               GO TO 2200-99-FIM
           END-IF.

           PERFORM 1120-TESTAR-FS-GFCTB050.
           ADD  1  TO   WRK-LIDOS-GFCTB050.
           PERFORM 2210-MONTAR-CHAVE-GFCTB050.

           IF WRK-CHAVE-GFCTB050 LESS THAN WRK-CHAVE-GFCTB050-ANT
              DISPLAY '************ GFCT4805 ************'
              DISPLAY '*                                *'
              DISPLAY '*     ARQUIVO NAO CLASSIFICADO   *'
              DISPLAY '*       PROGRAMA ENCERRADO       *'
              DISPLAY '* CHAVE GFCTB050 ANTERIOR        *'
ST2506*       DISPLAY '*   CCGC-CPF-ANT ................= ' WRK-DISPLAY
ST2506*       MOVE WRK-K2-CFLIAL-CGC-ANT TO WRK-DISPLAY
ST2506*       DISPLAY '*   CFLIAL-CGC-ANT ..............= ' WRK-DISPLAY
ST2506*       MOVE WRK-K2-CCTRL-CPF-CGC-ANT TO WRK-DISPLAY
ST2506*       DISPLAY '*   CCTRL-CPF-CGC-ANT ...........= ' WRK-DISPLAY
ST2506*       DISPLAY '*--------------------------------*'
ST2506*       DISPLAY '* CHAVE GFCTTRPN LIDA            *'
ST2506*       MOVE WRK-KEY2-CCGC-CPF TO WRK-DISPLAY
ST2506*       DISPLAY '*   CCGC-CPF.....................= ' WRK-DISPLAY
ST2506*       MOVE WRK-KEY2-CFLIAL-CGC TO WRK-DISPLAY
ST2506*       DISPLAY '*   CFLIAL-CGC...................= ' WRK-DISPLAY
ST2506*       MOVE WRK-KEY2-CCTRL-CPF-CGC TO WRK-DISPLAY
ST2506*       DISPLAY '*   CCTRL-CPF-CGC................= ' WRK-DISPLAY

ST2506        DISPLAY '*   CCGC-CPF-ANT ................= ' 
ST2506                                         WRK-K2-CCGC-CPF-ANT 
ST2506        DISPLAY '*   CFLIAL-CGC-ANT ..............= ' 
                                               WRK-K2-CFLIAL-CGC-ANT
ST2506        DISPLAY '*   CCTRL-CPF-CGC-ANT ...........= ' 
                                               WRK-K2-CCTRL-CPF-CGC-ANT
ST2506        DISPLAY '*--------------------------------*'
ST2506        DISPLAY '* CHAVE GFCTTRPN LIDA            *'
ST2506        DISPLAY '*   CCGC-CPF.....................= ' 
                                               WRK-KEY2-CCGC-CPF
ST2506        DISPLAY '*   CFLIAL-CGC...................= ' 
                                               WRK-KEY2-CFLIAL-CGC
ST2506        DISPLAY '*   CCTRL-CPF-CGC................= ' 
                                               WRK-KEY2-CCTRL-CPF-CGC
              DISPLAY '************ GFCT4805 ************'
              MOVE 'GFCTB050'        TO FRWKGARQ-NOME-ARQUIVO
              MOVE WRK-FS-GFCTB050   TO FRWKGARQ-FILE-STATUS
              PERFORM 9100-FORMATAR-ERRO-ARQUIVO
              PERFORM 9000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2210-MONTAR-CHAVE-GFCTB050      SECTION.
      *----------------------------------------------------------------*

           MOVE '2210-MONTAR-CHAVE-GFCTB050' TO FRWKGHEA-IDEN-PARAGRAFO.

           MOVE WRK-CHAVE-GFCTB050 TO WRK-CHAVE-GFCTB050-ANT.

           MOVE GFCTWE02-CCGC-CPF OF GFCTWE02-REGISTRO
             TO WRK-KEY2-CCGC-CPF.

           MOVE GFCTWE02-CFLIAL-CGC OF GFCTWE02-REGISTRO
             TO WRK-KEY2-CFLIAL-CGC.

           MOVE GFCTWE02-CCTRL-CPF-CGC OF GFCTWE02-REGISTRO
             TO WRK-KEY2-CCTRL-CPF-CGC.

           MOVE GFCTWE02-CSERVC-TARIF OF GFCTWE02-REGISTRO
             TO WRK-KEY2-CSERVC-TARIF.

      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           MOVE '3000-PROCESSAR' TO FRWKGHEA-IDEN-PARAGRAFO.

           IF      WRK-CHAVE-GFCTTRPN  LESS THAN WRK-CHAVE-GFCTB050
                   PERFORM 3100-GFCTTRPN-MENOR-Q-GFCTB050
           ELSE
             IF    WRK-CHAVE-GFCTTRPN  EQUAL TO  WRK-CHAVE-GFCTB050
                   PERFORM 3200-GFCTTRPN-IGUAL-A-GFCTB050
               ELSE
                   PERFORM 3300-GFCTTRPN-MAIOR-Q-GFCTB050
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3100-GFCTTRPN-MENOR-Q-GFCTB050  SECTION.
      *----------------------------------------------------------------*

           MOVE '3100-GFCTTRPN-MENOR-Q-GFCTB050'
                                             TO FRWKGHEA-IDEN-PARAGRAFO.
           MOVE 'N' TO WE01-CINDCD-FLEXZ-2.

           ADD  1   TO WRK-GRAVADOS-GFCTTRPS-SF.

           PERFORM 3500-GRAVAR-GFCTTRPS.

           PERFORM 2100-LER-GFCTTRPN.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3200-GFCTTRPN-IGUAL-A-GFCTB050  SECTION.
      *----------------------------------------------------------------*

           MOVE  '3200-GFCTTRPN-IGUAL-A-GFCTB050'
                                           TO   FRWKGHEA-IDEN-PARAGRAFO.
           PERFORM 3400-FORMAT-GFCTTRPS

           PERFORM 3500-GRAVAR-GFCTTRPS.

           PERFORM 2100-LER-GFCTTRPN.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3300-GFCTTRPN-MAIOR-Q-GFCTB050  SECTION.
      *----------------------------------------------------------------*

           MOVE  '3300-GFCTTRPN-MAIOR-Q-GFCTB050'
                                           TO   FRWKGHEA-IDEN-PARAGRAFO.

           PERFORM 2200-LER-GFCTB050.

      *----------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3400-FORMAT-GFCTTRPS            SECTION.
      *----------------------------------------------------------------*

           MOVE '3400-FORMAT-GFCTTRPS' TO   FRWKGHEA-IDEN-PARAGRAFO.


           MOVE GFCTWE02-CSERVC-TARIF
             TO          CSERVC-TARIF       OF GFCTB049.

           MOVE GFCTWE02-CAGPTO-CTA
             TO          CAGPTO-CTA         OF GFCTB049.

           MOVE GFCTWE02-HIDTFD-FLEXZ-AGPTO
             TO          HIDTFD-FLEXZ-AGPTO OF GFCTB049.

           PERFORM 5000-SELECT-FLEXZ-AGPTO-CTA.

           MOVE WE01-COD-RETORNO  TO WRK-ADD.

           ADD 1 TO WRK-ADD.

           MOVE WRK-ADD TO WE01-COD-RETORNO.

           MOVE 'S'     TO WE01-CINDCD-FLEXZ-2.

           MOVE WRK-PER-CBR-TARIF(CPER-COBR-TARIF OF GFCTB049 + 1)
             TO WE01-PER-CBR-TARIF-2.

           MOVE CPER-COBR-TARIF   OF GFCTB049
             TO WE01-QFRANQ-FLEXZ-2.

           MOVE VNEGOC-FLEXZ-AGPTO OF GFCTB049
             TO  WRK-VNEGOC-FLEXZ-CS.
           MOVE WRK-VNEGOC-FLEXZ-SS
             TO WE01-VNEGOC-FLEXZ-2.

           MOVE PDESC-FLEXZ-AGPTO  OF GFCTB049
             TO WRK-PDESC-FLEXZ-CS.
           MOVE WRK-PDESC-FLEXZ-SS
             TO WE01-PDESC-FLEXZ-2.

           ADD  1   TO WRK-GRAVADOS-GFCTTRPS-CF.

      *----------------------------------------------------------------*
       3400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3500-GRAVAR-GFCTTRPS            SECTION.
      *----------------------------------------------------------------*

           MOVE '3500-GRAVAR-GFCTTRPS' TO   FRWKGHEA-IDEN-PARAGRAFO.

           WRITE FD-GFCTTRPS       FROM    GFCTWE01-REG.

           PERFORM 1130-TESTAR-FS-GFCTTRPS.

           ADD 1                   TO  WRK-GRAVADOS-GFCTTRPS.

      *----------------------------------------------------------------*
       3500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5000-SELECT-FLEXZ-AGPTO-CTA     SECTION.
      *----------------------------------------------------------------*

           MOVE '5000-SELECT-FLEXZ-AGPTO-CTA'
                                           TO   FRWKGHEA-IDEN-PARAGRAFO.

           EXEC SQL SELECT CSERVC_TARIF
                         , CAGPTO_CTA
                         , HIDTFD_FLEXZ_AGPTO
                         , DINIC_FLEXZ_AGPTO
                         , DFIM_FLEXZ_AGPTO
                         , QFRANQ_FLEXZ_AGPTO
                         , CPER_FRANQ_AGPTO
                         , VNEGOC_FLEXZ_AGPTO
                         , PDESC_FLEXZ_AGPTO
                         , QDIA_FLEXZ_AGPTO
                         , CINDCD_AGPTO_TOT
                         , DDIA_AGNDA
                         , CINIC_PER_FRANQ
                         , CTPO_OPER_MOTVO
                         , CMOTVO_JUSTF
                         , CEMPR_INC
                         , CDEPDC_ENCRR_VGCIA
                         , CFUNC_ENCRR_VGCIA
                         , HVGCIA_REGRA
                         , CPER_COBR_TARIF
                         , CINIC_PER_COBR
                      INTO :GFCTB049.CSERVC-TARIF
                         , :GFCTB049.CAGPTO-CTA
                         , :GFCTB049.HIDTFD-FLEXZ-AGPTO
                         , :GFCTB049.DINIC-FLEXZ-AGPTO
                         , :GFCTB049.DFIM-FLEXZ-AGPTO
                         , :GFCTB049.QFRANQ-FLEXZ-AGPTO
                         , :GFCTB049.CPER-FRANQ-AGPTO
                         , :GFCTB049.VNEGOC-FLEXZ-AGPTO
                         , :GFCTB049.PDESC-FLEXZ-AGPTO
                         , :GFCTB049.QDIA-FLEXZ-AGPTO
                         , :GFCTB049.CINDCD-AGPTO-TOT
                         , :GFCTB049.DDIA-AGNDA
                         , :GFCTB049.CINIC-PER-FRANQ
                         , :GFCTB049.CTPO-OPER-MOTVO
                         , :GFCTB049.CMOTVO-JUSTF
                         , :GFCTB049.CEMPR-INC
                         , :GFCTB049.CDEPDC-ENCRR-VGCIA
                         , :GFCTB049.CFUNC-ENCRR-VGCIA
                         , :GFCTB049.HVGCIA-REGRA
                         , :GFCTB049.CPER-COBR-TARIF
                         , :GFCTB049.CINIC-PER-COBR
                      FROM DB2PRD.FLEXZ_AGPTO_CTA
                     WHERE CSERVC_TARIF       = :GFCTB049.CSERVC-TARIF
                       AND CAGPTO_CTA         = :GFCTB049.CAGPTO-CTA
                       AND HIDTFD_FLEXZ_AGPTO =
                                            :GFCTB049.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF SQLCODE              NOT EQUAL   ZEROS
              MOVE 'FLEXZ_AGPTO_CTA'   TO      FRWKGDB2-NOME-TABELA
              SET DB2-SELECT           TO      TRUE
              PERFORM 9200-TRATAR-ERRO-DB2
           END-IF.

           ADD 1                       TO  WRK-LIDOS-TB-GFCTB049.

      *----------------------------------------------------------------*
       5000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       7000-INICIAR-CKRS0105           SECTION.
      *----------------------------------------------------------------*

           MOVE '7000-INICIAR-CKRS0105' TO FRWKGHEA-IDEN-PARAGRAFO.

           MOVE 'C'                    TO PARM-OP.
           MOVE 'DB2'                  TO PARM-SSID.
           MOVE SPACES                 TO PARM-PLAN.

           CALL WRK-CKRS0105           USING PARM-CKRS0105.

      *----------------------------------------------------------------*
       7000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       7100-ENCERRAR-CKRS0105          SECTION.
      *----------------------------------------------------------------*

           MOVE '7100-ENCERRAR-CKRS0105' TO FRWKGHEA-IDEN-PARAGRAFO.

           MOVE 'D'                    TO PARM-OP.
           MOVE SPACES                 TO PARM-PLAN.

           CALL WRK-CKRS0105           USING PARM-CKRS0105.

      *----------------------------------------------------------------*
       7100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*
      *                                                                *
           MOVE '9000-FINALIZAR'       TO FRWKGHEA-IDEN-PARAGRAFO.
      *                                                                *
           DISPLAY '***************** GFCT4805 ****************'
           DISPLAY '*                                         *'
           DISPLAY '*     OBTEM FLEXIBILIZACAO POR CPF/CNPJ   *'
           DISPLAY '*                                         *'
           DISPLAY '*           RESUMO PROCESSAMENTO          *'
           DISPLAY '* --------------------------------------- *'
           MOVE    WRK-LIDOS-GFCTTRPN  TO WRK-DISPLAY
           DISPLAY '* LIDOS    GFCTTRPN : ' WRK-DISPLAY, ' *'
           DISPLAY '*                                         *'
           MOVE    WRK-LIDOS-GFCTB050  TO WRK-DISPLAY
           DISPLAY '* LIDOS    GFCTB050 : ' WRK-DISPLAY, ' *'
           DISPLAY '*                                         *'
           MOVE    WRK-LIDOS-TB-GFCTB049  TO WRK-DISPLAY
           DISPLAY '* LIDOS TB GFCTB049 : ' WRK-DISPLAY, ' *'
           DISPLAY '*                                         *'
           MOVE    WRK-GRAVADOS-GFCTTRPS  TO WRK-DISPLAY
           DISPLAY '* GRAVADOS GFCTTRPS : ' WRK-DISPLAY, ' *'
           DISPLAY '*                                         *'
           MOVE    WRK-GRAVADOS-GFCTTRPS-CF TO WRK-DISPLAY
           DISPLAY '*COM FLEXIBILIZACAO : ' WRK-DISPLAY, ' *'
           DISPLAY '*                                         *'
           MOVE    WRK-GRAVADOS-GFCTTRPS-SF TO WRK-DISPLAY
           DISPLAY '*SEM FLEXIBILIZACAO : ' WRK-DISPLAY, ' *'
           DISPLAY '*                              *'
           DISPLAY '*********** GFCT4805 ***********'

           PERFORM 7100-ENCERRAR-CKRS0105.

           CLOSE GFCTTRPN
                 GFCTB050
                 GFCTTRPS.

           PERFORM 1100-TESTAR-FILE-STATUS.

           STOP RUN.

      *----------------------------------------------------------------*
       9000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9100-FORMATAR-ERRO-ARQUIVO      SECTION.
      *----------------------------------------------------------------*

           SET ERRO-ARQUIVO            TO TRUE.

           MOVE FRWKGARQ-TAM-LAYOUT    TO FRWKGHEA-TAM-DADOS.
           MOVE WRK-AREA-ERRO-ARQUIVO  TO WRK-BLOCO-INFO-ERRO
                                          (1:FRWKGHEA-TAM-DADOS).

           PERFORM 9900-TRATAR-ERRO.

      *----------------------------------------------------------------*
       9100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9200-TRATAR-ERRO-DB2            SECTION.
      *----------------------------------------------------------------*

           SET  ERRO-DB2                TO      TRUE.

           MOVE FRWKGDB2-TAM-LAYOUT     TO      FRWKGHEA-TAM-DADOS.
           MOVE FRWKGHEA-IDEN-PARAGRAFO(1:16)
                                        TO      FRWKGDB2-LOCAL.
           MOVE SQLSTATE                TO      FRWKGDB2-SQLSTATE.
           MOVE SQLCA                   TO      FRWKGDB2-SQLCA.
           MOVE SQLCODE                 TO      WRK-SQLCODE-AUX.
           MOVE WRK-SQLCODE-9-3         TO      FRWKGDB2-SQLCODE2.
           MOVE WRK-AREA-ERRO-DB2       TO      WRK-BLOCO-INFO-ERRO
                                                (1:FRWKGHEA-TAM-DADOS).

           PERFORM 9900-TRATAR-ERRO.

      *----------------------------------------------------------------*
       9200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9900-TRATAR-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT4805'             TO FRWKGHEA-NOME-PROGRAMA.

           PERFORM 9990-GRAVAR-LOG-ERRO.

           PERFORM 9999-ABENDAR-PROGRAMA.

           GOBACK.

      *----------------------------------------------------------------*
       9900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9990-GRAVAR-LOG-ERRO            SECTION.
      *----------------------------------------------------------------*

           CALL WRK-FRWK2999           USING WRK-AREA-ERRO.

      *----------------------------------------------------------------*
       9990-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999-ABENDAR-PROGRAMA           SECTION.
      *----------------------------------------------------------------*

           DISPLAY '*** BRAD0450 CHAMADO PARA ABENDAR O PROGRAMA ***'.
           DISPLAY ' '.

           CALL 'BRAD0450'             USING WRK-0450-ABEND-BAT
                                             WRK-0450-DUMP-BAT.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *================================================================*
