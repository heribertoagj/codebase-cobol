      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. GFCT4430.
       AUTHOR.     SONDA IT.
      *================================================================*
      *                  SONDA IT INFORMATICA                          *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA.........: GFCT4430                                 *
      *                                                                *
      *    ANALISTA.........: UBIRAJARA                                *
      *    LIDER PROJETO....: RODRIGO                                  *
      *    GERENCIA / EQUIPE: GERENCIA-E / EQUIPE-1   - SONDA IT       *
      *                                                                *
      *    GRUPO NEGOCIO....: DDS-GP50                                 *
      *    ANALISTA.........:                                          *
      *                                                                *
      *    DATA.............: JUN/2013                                 *
      *                                                                *
      *    PROJETO..........: PWT-13-0116573-01-GFCT-CONTAS-SEM-CESTA  *
      *                                                                *
      *    OBJETIVO.........: BALANCE LINE ADESOES E RENTABILIDADE     *
      *                       POR CPF , GERA ARQUIVO DE ADESOES        *
      *                       COM AS INFORMACOES DE RENTABILIDADE.     *
      *                                                                *
      *    ARQUIVOS:                                                   *
      *      DDNAME                            INCLUDE/BOOK            *
      *        ADMESCOR                            GFCTWBD0            *
      *        ARQURENC                            I#RENC23            *
      *        ADESAOSC                            GFCTWBD0            *
      *                                                                *
      *    MODULOS CHAMADOS.:                                          *
      *      BRAD7100 - TRATAR ERRO                                    *
      *                                                                *
      *================================================================*

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

           SELECT ADMESCOR ASSIGN TO UT-S-ADMESCOR
                      FILE STATUS IS WRK-FS-ADMESCOR.

           SELECT ARQURENC ASSIGN TO UT-S-ARQURENC
                      FILE STATUS IS WRK-FS-ARQURENC.

           SELECT ADESAOSC ASSIGN TO UT-S-ADESAOSC
                      FILE STATUS IS WRK-FS-ADESAOSC.

      *===============================================================*
       DATA DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       FILE SECTION.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      *   INPUT :     ORG. SEQUENCIAL   -   LRECL = 030               *
      *---------------------------------------------------------------*

       FD  ADMESCOR
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-ADMESCOR           PIC  X(404).

      *---------------------------------------------------------------*
      *   INPUT :     ORG. SEQUENCIAL   -   LRECL = 080               *
      *---------------------------------------------------------------*

       FD  ARQURENC
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#RENC23'.

      *---------------------------------------------------------------*
      *   OUTPUT:     ORG. SEQUENCIAL   -   LRECL = 404               *
      *---------------------------------------------------------------*

       FD  ADESAOSC
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-ADESAOSC           PIC  X(404).

      *---------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *---------------------------------------------------------------*

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
       77  FILLER  PIC X(32) VALUE '*  INICIO DA WORKING GFCT4430  *'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*

      *---------------------------------------------------------------*
       77  FILLER  PIC X(32) VALUE '*  AREAS DE FILE STATUS        *'.
      *---------------------------------------------------------------*

       77  WRK-BATCH                   PIC X(08) VALUE 'BATCH   '.

       77  WRK-FS-ADMESCOR             PIC X(02) VALUE SPACES.
       77  WRK-FS-ARQURENC             PIC X(02) VALUE SPACES.
       77  WRK-FS-ADESAOSC             PIC X(02) VALUE SPACES.

       77  WRK-OPERACAO                PIC X(13) VALUE SPACES.
       77  WRK-ABERTURA                PIC X(13) VALUE 'NA ABERTURA'.
       77  WRK-LEITURA                 PIC X(13) VALUE 'NA LEITURA'.
       77  WRK-GRAVACAO                PIC X(13) VALUE 'NA GRAVACAO'.
       77  WRK-FECHAMENTO              PIC X(13) VALUE 'NO FECHAMENTO'.

      *---------------------------------------------------------------*
       77  FILLER  PIC X(32) VALUE '*  ACUMULADORES                *'.
      *---------------------------------------------------------------*

       77  ACU-LIDOS-ADMESCOR          PIC 9(09) COMP-3 VALUE ZEROS.
       77  ACU-LIDOS-ARQURENC          PIC 9(09) COMP-3 VALUE ZEROS.
       77  ACU-GRAVA-ADESAOSC          PIC 9(09) COMP-3 VALUE ZEROS.

      *---------------------------------------------------------------*
       01  FILLER  PIC X(32) VALUE '*  MASCARAS DE EDICAO DE TOTAIS*'.
      *---------------------------------------------------------------*

       01  FILLER.
           05  WRK-MASK-LIDOS-ADMESCOR PIC ZZZ.ZZZ.ZZ9 VALUE SPACES.
           05  WRK-MASK-LIDOS-ARQURENC PIC ZZZ.ZZZ.ZZ9 VALUE SPACES.
           05  WRK-MASK-GRAVA-ADESAOSC PIC ZZZ.ZZZ.ZZ9 VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER  PIC X(32) VALUE '*  CHAVES DOS ARQUIVOS         *'.
      *---------------------------------------------------------------*

       01  FILLER.
           05  WRK-CHAVE-ADMESCOR.
               10  WRK-CHV-CPF-COR     PIC 9(009) VALUE ZEROS.

           05  WRK-CHAVE-ARQURENC.
               10  WRK-CHV-CPF-RENT    PIC 9(009) VALUE ZEROS.

      *---------------------------------------------------------------*
       01  FILLER  PIC X(32) VALUE '*  BOOK GFCTWBD0               *'.
      *---------------------------------------------------------------*

       COPY 'GFCTWBD0'.

      *---------------------------------------------------------------*
       01  FILLER  PIC X(32) VALUE '*  AREAS TRATAMENTO ERRO       *'.
      *---------------------------------------------------------------*

       COPY 'I#BRAD7C'.

       01  WRK-TEXTO.
           05  FILLER                  PIC X(05) VALUE 'ERRO'.
           05  WRK-OPERACAO-TXT        PIC X(13) VALUE SPACES.
           05  FILLER                  PIC X(12) VALUE ' DO ARQUIVO '.
           05  WRK-ARQ-TXT             PIC X(08) VALUE SPACES.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
       01  FILLER  PIC X(32) VALUE '*  FIM  DA WORKING   GFCT4430  *'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*

      *===============================================================*
       PROCEDURE DIVISION.
      *===============================================================*

      *===============================================================*
      *    ROTINA INICIAR
      *===============================================================*
       0000-INICIAR SECTION.
      *---------------------------------------------------------------*

           OPEN    INPUT   ADMESCOR
                           ARQURENC
                   OUTPUT  ADESAOSC.

           MOVE    WRK-ABERTURA        TO  WRK-OPERACAO.
           PERFORM 1000-TESTAR-FILE-STATUS.

           PERFORM 2000-PRIMEIRA-LEITURA.

           IF  ACU-LIDOS-ADMESCOR      EQUAL   ZEROS
               PERFORM 4000-ROTINA-FINAL
               GO  TO 0000-FINALIZAR
           END-IF.

           PERFORM 3000-PROCESSA
             UNTIL WRK-FS-ADMESCOR     EQUAL '10'.

           PERFORM 4000-ROTINA-FINAL.

      *===============*
      * LABEL FINALIZAR
      *===============*
       0000-FINALIZAR.
      *---------------*

           CLOSE   ADMESCOR
                   ARQURENC
                   ADESAOSC.

           MOVE    WRK-FECHAMENTO      TO   WRK-OPERACAO.
           PERFORM 1000-TESTAR-FILE-STATUS.

           STOP    RUN.

      *---------------------------------------------------------------*
       0000-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA TESTAR-FILE-STATUS
      *===============================================================*
       1000-TESTAR-FILE-STATUS SECTION.
      *---------------------------------------------------------------*

           PERFORM 1100-TESTAR-FS-ADMESCOR.

           PERFORM 1200-TESTAR-FS-ARQURENC.

           PERFORM 1300-TESTAR-FS-ADESAOSC.

      *---------------------------------------------------------------*
       1000-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA TESTAR-FS-ADMESCOR
      *===============================================================*
       1100-TESTAR-FS-ADMESCOR SECTION.
      *---------------------------------------------------------------*

           IF WRK-FS-ADMESCOR NOT EQUAL '00'
              DISPLAY '************** GFCT4430 *************'
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
              DISPLAY '*              ADMESCOR             *'
              DISPLAY '*         FILE STATUS =  ' WRK-FS-ADMESCOR
                                                 '         *'
              DISPLAY '*        PROGRAMA  ENCERRADO        *'
              DISPLAY '************** GFCT4430 *************'
              MOVE WRK-OPERACAO        TO  WRK-OPERACAO-TXT
              MOVE 'ADMESCOR'          TO  WRK-ARQ-TXT
              MOVE WRK-TEXTO           TO  ERR-TEXTO
              PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1100-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA TESTAR-FS-ARQURENC
      *===============================================================*
       1200-TESTAR-FS-ARQURENC SECTION.
      *---------------------------------------------------------------*

           IF WRK-FS-ARQURENC NOT EQUAL '00'
              DISPLAY '************** GFCT4430 *************'
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
              DISPLAY '*              ARQURENC             *'
              DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQURENC
                                                 '         *'
              DISPLAY '*        PROGRAMA  ENCERRADO        *'
              DISPLAY '************** GFCT4430 *************'
              MOVE WRK-OPERACAO        TO  WRK-OPERACAO-TXT
              MOVE 'ARQURENC'          TO  WRK-ARQ-TXT
              MOVE WRK-TEXTO           TO  ERR-TEXTO
              PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1200-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA TESTAR-FS-ADESAOSC
      *===============================================================*
       1300-TESTAR-FS-ADESAOSC   SECTION.
      *---------------------------------------------------------------*

           IF WRK-FS-ADESAOSC NOT EQUAL '00'
              DISPLAY '************** GFCT4430 *************'
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
              DISPLAY '*               ADESAOSC            *'
              DISPLAY '*         FILE STATUS =  ' WRK-FS-ADESAOSC
                                                 '         *'
              DISPLAY '*        PROGRAMA  ENCERRADO        *'
              DISPLAY '************** GFCT4430 *************'
              MOVE WRK-OPERACAO        TO  WRK-OPERACAO-TXT
              MOVE 'ADESAOSC'          TO  WRK-ARQ-TXT
              MOVE WRK-TEXTO           TO  ERR-TEXTO
              PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1300-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA PRIMEIRA-LEITURA
      *===============================================================*
       2000-PRIMEIRA-LEITURA SECTION.
      *---------------------------------------------------------------*

           PERFORM 2100-LER-ADMESCOR.

           IF  ACU-LIDOS-ADMESCOR      EQUAL   ZEROS
               DISPLAY '************** GFCT4430 *************'
               DISPLAY '*                                   *'
               DISPLAY '*    ARQUIVO ADMESCOR ESTA VAZIO    *'
               DISPLAY '*                                   *'
               DISPLAY '*         PROGRAMA ENCERRADO        *'
               DISPLAY '*                                   *'
               DISPLAY '************** GFCT4430 *************'
           END-IF.

           PERFORM 2200-LER-ARQURENC.

           IF  ACU-LIDOS-ARQURENC      EQUAL   ZEROS
               DISPLAY '************** GFCT4430 *************'
               DISPLAY '*                                   *'
               DISPLAY '*    ARQUIVO ARQURENC ESTA VAZIO    *'
               DISPLAY '*                                   *'
               DISPLAY '************** GFCT4430 *************'
           END-IF.

      *---------------------------------------------------------------*
       2000-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA LER-ADMESCOR
      *===============================================================*
       2100-LER-ADMESCOR SECTION.
      *---------------------------------------------------------------*

           READ ADMESCOR               INTO GFCTWBD0-REG.

           IF  WRK-FS-ADMESCOR         EQUAL   '10'
               MOVE  HIGH-VALUES       TO  WRK-CHAVE-ADMESCOR
               GO                      TO  2100-99-FIM
           END-IF.

           MOVE    WRK-LEITURA         TO  WRK-OPERACAO.
           PERFORM 1100-TESTAR-FS-ADMESCOR.

           ADD 1                       TO  ACU-LIDOS-ADMESCOR

           MOVE BD0-CPF                TO  WRK-CHV-CPF-COR.

      *---------------------------------------------------------------*
       2100-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA LER-ARQURENC
      *===============================================================*
       2200-LER-ARQURENC SECTION.
      *---------------------------------------------------------------*

           READ ARQURENC.

           IF  WRK-FS-ARQURENC      EQUAL   '10'
               MOVE  HIGH-VALUES       TO  WRK-CHAVE-ARQURENC
               GO                      TO  2200-99-FIM
           END-IF.

           MOVE    WRK-LEITURA         TO  WRK-OPERACAO.
           PERFORM 1200-TESTAR-FS-ARQURENC.

           ADD 1                       TO  ACU-LIDOS-ARQURENC.

           MOVE CCGC-NU-CGCCPF         TO  WRK-CHV-CPF-RENT.

      *---------------------------------------------------------------*
       2200-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA PROCESSA
      *===============================================================*
       3000-PROCESSA SECTION.
      *---------------------------------------------------------------*

           IF  WRK-CHAVE-ADMESCOR      EQUAL WRK-CHAVE-ARQURENC
               PERFORM 3100-GRAVAR-ADESAOSC
               PERFORM 2100-LER-ADMESCOR
           ELSE
               IF  WRK-CHAVE-ADMESCOR LESS WRK-CHAVE-ARQURENC
                   PERFORM 3200-GRAVAR-ADESAOSC-2
                   PERFORM 2100-LER-ADMESCOR
               ELSE
                   PERFORM 2200-LER-ARQURENC
               END-IF
           END-IF.

      *---------------------------------------------------------------*
       3000-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA GRAVAR-ADESAOSC
      *===============================================================*
       3100-GRAVAR-ADESAOSC SECTION.
      *---------------------------------------------------------------*

           COMPUTE  BD0-VR-RENTAB  =  (CCGC-VLRAGDEP + CCGC-VLRDEDEP).

           WRITE FD-REG-ADESAOSC      FROM    GFCTWBD0-REG.
           MOVE WRK-GRAVACAO          TO      WRK-OPERACAO.
           PERFORM 1300-TESTAR-FS-ADESAOSC.

           ADD 1                      TO      ACU-GRAVA-ADESAOSC.

      *---------------------------------------------------------------*
       3100-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA GRAVAR-ADESAOSC
      *===============================================================*
       3200-GRAVAR-ADESAOSC-2 SECTION.
      *---------------------------------------------------------------*

           MOVE  ZEROS                TO      BD0-VR-RENTAB.

           WRITE FD-REG-ADESAOSC      FROM    GFCTWBD0-REG.
           MOVE WRK-GRAVACAO          TO      WRK-OPERACAO.
           PERFORM 1300-TESTAR-FS-ADESAOSC.

           ADD 1                      TO      ACU-GRAVA-ADESAOSC.

      *---------------------------------------------------------------*
       3200-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA FINAL
      *===============================================================*
       4000-ROTINA-FINAL   SECTION.
      *---------------------------------------------------------------*

           MOVE ACU-LIDOS-ADMESCOR     TO  WRK-MASK-LIDOS-ADMESCOR.
           MOVE ACU-LIDOS-ARQURENC     TO  WRK-MASK-LIDOS-ARQURENC.
           MOVE ACU-GRAVA-ADESAOSC     TO  WRK-MASK-GRAVA-ADESAOSC.

           DISPLAY '*********************** GFCT4430 *******************
      -                                                        '****'.
           DISPLAY '*
      -                                                        '   *'.
           DISPLAY '* QTDE DE REGISTROS LIDOS    ADMESCOR... : '
                                       WRK-MASK-LIDOS-ADMESCOR ' *'.
           DISPLAY '* QTDE DE REGISTROS LIDOS    ARQURENC... : '
                                       WRK-MASK-LIDOS-ARQURENC ' *'.
           DISPLAY '* QTDE DE REGISTROS GRAVADOS ADESAOSC... : '
                                       WRK-MASK-GRAVA-ADESAOSC ' *'.
           DISPLAY '*
      -                                                        '   *'.
           DISPLAY '*********************** GFCT4430 *******************
      -                                                        '****'.

      *---------------------------------------------------------------*
       4000-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA ERRO
      *===============================================================*
       9999-ROTINA-ERRO SECTION.
      *---------------------------------------------------------------*

           MOVE 'APL'                  TO  ERR-TIPO-ACESSO
           MOVE 'GFCT4430'             TO  ERR-PGM

           CALL 'BRAD7100'          USING  WRK-BATCH
                                           ERRO-AREA.

           GOBACK.

      *---------------------------------------------------------------*
       9999-99-FIM. EXIT.
      *---------------------------------------------------------------*
