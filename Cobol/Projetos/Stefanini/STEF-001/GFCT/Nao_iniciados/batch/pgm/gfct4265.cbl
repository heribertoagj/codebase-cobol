      *===============================================================*
       IDENTIFICATION DIVISION.
      *===============================================================*

       PROGRAM-ID. GFCT4265.

      *================================================================*
      *                       S O N D A   I T                          *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA.........: GFCT4265                                 *
      *    PROGRAMADOR......: FABRICA SOFTWARE        - SONDA PROCWORK *
      *                                                                *
      *    ANALISTA.........: ANTONIO PAGNOCCA NETO                    *
      *    LIDER PROJETO....: RODRIGO PRADO                            *
      *    GERENCIA / EQUIPE: GERENCIA-E / EQUIPE-1   - SONDA PROCWORK *
      *                                                                *
      *    GRUPO NEGOCIO....: DDS-GP50                                 *
      *                                                                *
      *    DATA.............: MAI/2012                                 *
      *                                                                *
      *    PROJETO..........: CONTROLE FERIADOS MORA                   *
      *                                                                *
      *    OBJETIVO.........: GERA DETALHES COBRANCA ZONADO PARA ENVIO *
      *                       VIA ISD.                                 *
      *                                                                *
      *    ARQUIVOS:                                                   *
      *      DDNAME                            INCLUDE/BOOK            *
      *        CONSOLID                          I#GFCTIL              *
      *        DETALHAD                          I#GFCTIL              *
      *        OCORRENC                          GFCTWBB4              *
      *        APURACAO                          GFCTWBB1              *
      *        SAIDAZON                          GFCTWBB0              *
      *                                                                *
      *    MODULOS CHAMADOS.:                                          *
      *      BRAD0560 - OBTER NOME DO JOB.                             *
      *      BRAD7100 - TRATAR ERRO                                    *
      *      BRAD7600 - OBTER DATA DO SISTEMA                          *
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

           SELECT CONSOLID ASSIGN TO UT-S-CONSOLID
                      FILE STATUS IS WRK-FS-CONSOLID.

           SELECT DETALHAD ASSIGN TO UT-S-DETALHAD
                      FILE STATUS IS WRK-FS-DETALHAD.

           SELECT OCORRENC ASSIGN TO UT-S-OCORRENC
                      FILE STATUS IS WRK-FS-OCORRENC.

           SELECT APURACAO ASSIGN TO UT-S-APURACAO
                      FILE STATUS IS WRK-FS-APURACAO.

           SELECT SAIDAZON ASSIGN TO UT-S-SAIDAZON
                      FILE STATUS IS WRK-FS-SAIDAZON.

      *===============================================================*
       DATA DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       FILE SECTION.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      *   INPUT :     ORG. SEQUENCIAL   -   LRECL = 300               *
      *---------------------------------------------------------------*

       FD  CONSOLID
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTIL'.

      *---------------------------------------------------------------*
      *   INPUT :     ORG. SEQUENCIAL   -   LRECL = 300               *
      *---------------------------------------------------------------*

       FD  DETALHAD
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTIL'.

      *---------------------------------------------------------------*
      *   OUTPUT:     ORG. SEQUENCIAL   -   LRECL = 304               *
      *---------------------------------------------------------------*

       FD  OCORRENC
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-OCORRENC             PIC X(304).

      *---------------------------------------------------------------*
      *   OUTPUT:     ORG. SEQUENCIAL   -   LRECL = 111               *
      *---------------------------------------------------------------*

       FD  APURACAO
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-APURACAO             PIC X(111).

      *---------------------------------------------------------------*
      *   OUTPUT:     ORG. SEQUENCIAL   -   LRECL = 134               *
      *---------------------------------------------------------------*

       FD  SAIDAZON
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-SAIDAZON             PIC X(134).

      *---------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *---------------------------------------------------------------*

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
       77  FILLER  PIC X(32) VALUE '*  INICIO DA WORKING GFCT4265  *'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*

      *---------------------------------------------------------------*
       77  FILLER  PIC X(32) VALUE '*  AREAS AUXILIARES            *'.
      *---------------------------------------------------------------*

       77  WRK-INCONSIS                PIC  X(001)         VALUE SPACES.
       01  WRK-TAM-11V2-COMP           PIC 9(11)V99 COMP-3 VALUE ZEROS.

       01  WRK-DATA-DMA-BARRA.
           05  WRK-DIA-DMA             PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '/'.
           05  WRK-MES-DMA             PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '/'.
           05  WRK-ANO-DMA             PIC  9(004)         VALUE ZEROS.

       01  WRK-HORA-HMS-2PTS.
           05  WRK-HOR-HMS             PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE ':'.
           05  WRK-MIN-HMS             PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE ':'.
           05  WRK-SEG-HMS             PIC  9(002)         VALUE ZEROS.


       01  WRK-MENSA-01.
           05  FILLER                  PIC  X(025)         VALUE
               'TIPO COBRANCA INVALIDA - '.
           05  WRK-MENSA-01-TIPO       PIC  9(002)         VALUE ZEROS.


       01  WRK-DISPLAYS.
           05  WRK-DISPL-01            PIC  X(053)         VALUE
               '******************* GFCT4265 ************************'.

           05  WRK-DISPL-02.
               10  FILLER              PIC  X(007)         VALUE
                   'DATA ='.
               10  WRK-DISPL-05-DATA   PIC  X(010)         VALUE SPACES.
               10  FILLER              PIC  X(001)         VALUE SPACES.
               10  WRK-DISPL-05-HORA   PIC  X(008)         VALUE SPACES.

           05  WRK-DISPL-03            PIC  X(037)         VALUE
               'TOTAIS APURADOS POR TIPO DE COBRANCA'.

           05  WRK-DISPL-04            PIC  X(051)         VALUE
               'TIPO               REGISTROS       VALOR A DEBITAR'.

           05  WRK-DISPL-05.
               10  WRK-DISPL-05-LITER  PIC  X(016)         VALUE SPACES.
               10  FILLER              PIC  X(003)         VALUE ' -'.
               10  WRK-DISPL-05-REG    PIC ZZZ.ZZZ.ZZ9     VALUE ZEROS.
               10  FILLER              PIC  X(005)         VALUE ' - $'.
               10  WRK-DISPL-05-VALOR  PIC ZZZ.ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.


       01  WRK-LAYOUT.
           05  WRK-LAY-01              PIC  X(053)        VALUE ALL '*'.

           05  WRK-LAY-02              PIC  X(053)        VALUE
               'TOTAIS APURADOS POR TIPO DE COBRANCA'.

           05  WRK-LAY-03              PIC  X(053)        VALUE
               'TIPO               REGISTROS       VALOR A DEBITAR'.

           05  WRK-LAY-04.
               10  FILLER              PIC  X(002)         VALUE '* '.
               10  WRK-LAY-04-LITER    PIC  X(016)         VALUE SPACES.
               10  FILLER              PIC  X(003)         VALUE ' -'.
               10  WRK-LAY-04-REG      PIC ZZZ.ZZZ.ZZ9     VALUE ZEROS.
               10  FILLER              PIC  X(005)         VALUE ' - $'.
               10  WRK-LAY-04-VALOR    PIC ZZZ.ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.

      *---------------------------------------------------------------*
       77  FILLER  PIC X(32) VALUE '*  AREAS DE FILE STATUS        *'.
      *---------------------------------------------------------------*

       77  WRK-BATCH                   PIC X(08) VALUE 'BATCH   '.

       77  WRK-FS-CONSOLID             PIC X(02) VALUE SPACES.
       77  WRK-FS-DETALHAD             PIC X(02) VALUE SPACES.
       77  WRK-FS-OCORRENC             PIC X(02) VALUE SPACES.
       77  WRK-FS-APURACAO             PIC X(02) VALUE SPACES.
       77  WRK-FS-SAIDAZON             PIC X(02) VALUE SPACES.

       77  WRK-OPERACAO                PIC X(13) VALUE SPACES.
       77  WRK-ABERTURA                PIC X(13) VALUE 'NA ABERTURA'.
       77  WRK-LEITURA                 PIC X(13) VALUE 'NA LEITURA'.
       77  WRK-GRAVACAO                PIC X(13) VALUE 'NA GRAVACAO'.
       77  WRK-FECHAMENTO              PIC X(13) VALUE 'NO FECHAMENTO'.

      *---------------------------------------------------------------*
       77  FILLER  PIC X(32) VALUE '*  ACUMULADORES                *'.
      *---------------------------------------------------------------*

       77  ACU-LIDOS-CONSOLID          PIC 9(009) COMP-3   VALUE ZEROS.
       77  ACU-LIDOS-DETALHAD          PIC 9(009) COMP-3   VALUE ZEROS.

       77  ACU-REG-TIPO-01             PIC 9(009) COMP-3   VALUE ZEROS.
       77  ACU-REG-TIPO-02             PIC 9(009) COMP-3   VALUE ZEROS.
       77  ACU-REG-TIPO-03             PIC 9(009) COMP-3   VALUE ZEROS.
       77  ACU-REG-TIPO-04             PIC 9(009) COMP-3   VALUE ZEROS.
       77  ACU-REG-TIPO-05             PIC 9(009) COMP-3   VALUE ZEROS.
       77  ACU-REG-TIPO-07             PIC 9(009) COMP-3   VALUE ZEROS.
       77  ACU-REG-TIPO-08             PIC 9(009) COMP-3   VALUE ZEROS.
       77  ACU-REG-TIPO-09             PIC 9(009) COMP-3   VALUE ZEROS.
       77  ACU-REG-TIPO-10             PIC 9(009) COMP-3   VALUE ZEROS.
       77  ACU-REG-TOT-ENV             PIC 9(009) COMP-3   VALUE ZEROS.
       77  ACU-REG-TOT-INV             PIC 9(009) COMP-3   VALUE ZEROS.

       77  ACU-TOT-VALOR-DEBITAR       PIC 9(011)V99 COMP-3 VALUE ZEROS.

       77  ACU-VLR-TIPO-01             PIC 9(012)V99 COMP-3 VALUE ZEROS.
       77  ACU-VLR-TIPO-02             PIC 9(012)V99 COMP-3 VALUE ZEROS.
       77  ACU-VLR-TIPO-03             PIC 9(012)V99 COMP-3 VALUE ZEROS.
       77  ACU-VLR-TIPO-04             PIC 9(012)V99 COMP-3 VALUE ZEROS.
       77  ACU-VLR-TIPO-05             PIC 9(012)V99 COMP-3 VALUE ZEROS.
       77  ACU-VLR-TIPO-07             PIC 9(012)V99 COMP-3 VALUE ZEROS.
       77  ACU-VLR-TIPO-08             PIC 9(012)V99 COMP-3 VALUE ZEROS.
       77  ACU-VLR-TIPO-09             PIC 9(012)V99 COMP-3 VALUE ZEROS.
       77  ACU-VLR-TIPO-10             PIC 9(012)V99 COMP-3 VALUE ZEROS.
       77  ACU-VLR-TOT-ENV             PIC 9(012)V99 COMP-3 VALUE ZEROS.
       77  ACU-VLR-TOT-INV             PIC 9(012)V99 COMP-3 VALUE ZEROS.

      *---------------------------------------------------------------*
       01  FILLER  PIC X(32) VALUE '*  CHAVES DOS ARQUIVOS         *'.
      *---------------------------------------------------------------*

       01  FILLER.
           05  WRK-CHAVE-CONSOLID.
               10  WRK-CHV-DIG-CONS    PIC X(001)          VALUE SPACES.
               10  WRK-CHV-NUM-CONS    PIC 9(007)          VALUE ZEROS.

           05  WRK-CHAVE-DETALHAD.
               10  WRK-CHV-DIG-DETA    PIC X(001)          VALUE SPACES.
               10  WRK-CHV-NUM-DETA    PIC 9(007)          VALUE ZEROS.

           05  WRK-CHAVE-DETALHAD-ANT  PIC X(008)          VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER  PIC X(32)  VALUE  '*  AREAS LAYOUT OCORRENC      *'.
      *---------------------------------------------------------------*

       COPY GFCTWBB4.

      *---------------------------------------------------------------*
       01  FILLER  PIC X(32)  VALUE  '*  AREAS LAYOUT APURACAO      *'.
      *---------------------------------------------------------------*

       COPY GFCTWBB1.

      *---------------------------------------------------------------*
       01  FILLER  PIC X(32)  VALUE  '*  AREAS LAYOUT SAIDAZON      *'.
      *---------------------------------------------------------------*

       COPY GFCTWBB0.

      *---------------------------------------------------------------*
       01  FILLER  PIC X(32)  VALUE  '*  AREAS BRAD0560             *'.
      *---------------------------------------------------------------*

       01  WRK-P0560-JOBNAME           PIC X(008)          VALUE SPACES.
       01  WRK-P0560-JOBNUMBER         PIC 9(005)          VALUE ZEROS.

      *---------------------------------------------------------------*
       01  FILLER  PIC X(32)  VALUE  '* AREAS BRAD7600               *'.
      *---------------------------------------------------------------*

       01  WRK-DATA-HORA-7600.
           05  WRK-DT-JULIANA          PIC 9(005) COMP-3   VALUE ZEROS.
           05  WRK-DT-AAMMDD           PIC 9(007) COMP-3   VALUE ZEROS.
           05  WRK-DT-AAAAMMDD         PIC 9(009) COMP-3   VALUE ZEROS.
           05  WRK-TI-HHMMSS           PIC 9(007) COMP-3   VALUE ZEROS.
           05  WRK-TI-HHMMSSMMMMMM     PIC 9(013) COMP-3   VALUE ZEROS.
           05  WRK-TIMESTAMP           PIC X(020)          VALUE SPACES.

       01  WRK-DT-SISTEMA              PIC 9(009)          VALUE ZEROS.
       01  FILLER      REDEFINES       WRK-DT-SISTEMA.
           05  FILLER                  PIC X(001).
           05  WRK-DT-SISTEMA-R.
               10  WRK-ANO-SIS         PIC 9(004).
               10  WRK-MES-SIS         PIC 9(002).
               10  WRK-DIA-SIS         PIC 9(002).


       01  WRK-HR-SISTEMA              PIC 9(007)          VALUE ZEROS.
       01  FILLER      REDEFINES       WRK-HR-SISTEMA.
           05  FILLER                  PIC X(001).
           05  WRK-HR-SISTEMA-R.
               10  WRK-HOR-SIS         PIC 9(002).
               10  WRK-MIN-SIS         PIC 9(002).
               10  WRK-SEG-SIS         PIC 9(002).

      *---------------------------------------------------------------*
       01  FILLER  PIC X(32) VALUE '*  AREAS TRATAMENTO ERRO       *'.
      *---------------------------------------------------------------*

       COPY 'I#BRAD7C'.

       01  WRK-TEXTO.
           05  FILLER                  PIC X(005) VALUE 'ERRO'.
           05  WRK-OPERACAO-TXT        PIC X(013) VALUE SPACES.
           05  FILLER                  PIC X(012) VALUE ' DO ARQUIVO '.
           05  WRK-ARQ-TXT             PIC X(008) VALUE SPACES.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
       01  FILLER  PIC X(32) VALUE '*  FIM  DA WORKING   GFCT4265  *'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*

      *===============================================================*
       PROCEDURE DIVISION.
      *===============================================================*

      *===============================================================*
      *    ROTINA INICIAR
      *===============================================================*
       0000-INICIAR SECTION.
      *---------------------------------------------------------------*

           OPEN    INPUT   CONSOLID
                           DETALHAD
                   OUTPUT  OCORRENC
                           APURACAO
                           SAIDAZON

           MOVE    WRK-ABERTURA        TO  WRK-OPERACAO.
           PERFORM 1000-TESTAR-FILE-STATUS.

           PERFORM 2000-ROTINA-INICIAL.

           PERFORM 3000-PROCESSA
             UNTIL WRK-FS-CONSOLID     EQUAL '10' AND
                   WRK-FS-DETALHAD     EQUAL '10'.

           PERFORM 4000-ROTINA-FINAL.

      *===============*
      * LABEL FINALIZAR
      *===============*
       0000-FINALIZAR.
      *---------------*

           CLOSE   CONSOLID
                   DETALHAD
                   OCORRENC
                   APURACAO
                   SAIDAZON

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

           PERFORM 1100-TESTAR-FS-CONSOLID.

           PERFORM 1200-TESTAR-FS-DETALHAD.

           PERFORM 1300-TESTAR-FS-OCORRENC.

           PERFORM 1400-TESTAR-FS-APURACAO.

           PERFORM 1500-TESTAR-FS-SAIDAZON.

      *---------------------------------------------------------------*
       1000-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA TESTAR-FS-CONSOLID
      *===============================================================*
       1100-TESTAR-FS-CONSOLID SECTION.
      *---------------------------------------------------------------*

           IF WRK-FS-CONSOLID NOT EQUAL '00'
              DISPLAY '************** GFCT4265 *************'
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
              DISPLAY '*              CONSOLID             *'
              DISPLAY '*         FILE STATUS =  ' WRK-FS-CONSOLID
                                                 '         *'
              DISPLAY '*        PROGRAMA  ENCERRADO        *'
              DISPLAY '************** GFCT4265 *************'
              MOVE WRK-OPERACAO        TO  WRK-OPERACAO-TXT
              MOVE 'CONSOLID'          TO  WRK-ARQ-TXT
              MOVE WRK-TEXTO           TO  ERR-TEXTO
              PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1100-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA TESTAR-FS-DETALHAD
      *===============================================================*
       1200-TESTAR-FS-DETALHAD SECTION.
      *---------------------------------------------------------------*

           IF WRK-FS-DETALHAD NOT EQUAL '00'
              DISPLAY '************** GFCT4265 *************'
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
              DISPLAY '*              DETALHAD             *'
              DISPLAY '*         FILE STATUS =  ' WRK-FS-DETALHAD
                                                 '         *'
              DISPLAY '*        PROGRAMA  ENCERRADO        *'
              DISPLAY '************** GFCT4265 *************'
              MOVE WRK-OPERACAO        TO  WRK-OPERACAO-TXT
              MOVE 'DETALHAD'          TO  WRK-ARQ-TXT
              MOVE WRK-TEXTO           TO  ERR-TEXTO
              PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1200-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA TESTAR-FS-OCORRENC
      *===============================================================*
       1300-TESTAR-FS-OCORRENC SECTION.
      *---------------------------------------------------------------*

           IF WRK-FS-OCORRENC NOT EQUAL '00'
              DISPLAY '************** GFCT4265 *************'
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
              DISPLAY '*              OCORRENC             *'
              DISPLAY '*         FILE STATUS =  ' WRK-FS-OCORRENC
                                                 '         *'
              DISPLAY '*        PROGRAMA  ENCERRADO        *'
              DISPLAY '************** GFCT4265 *************'
              MOVE WRK-OPERACAO        TO  WRK-OPERACAO-TXT
              MOVE 'OCORRENC'          TO  WRK-ARQ-TXT
              MOVE WRK-TEXTO           TO  ERR-TEXTO
              PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1300-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA TESTAR-FS-APURACAO
      *===============================================================*
       1400-TESTAR-FS-APURACAO SECTION.
      *---------------------------------------------------------------*

           IF WRK-FS-APURACAO NOT EQUAL '00'
              DISPLAY '************** GFCT4265 *************'
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
              DISPLAY '*              APURACAO             *'
              DISPLAY '*         FILE STATUS =  ' WRK-FS-APURACAO
                                                 '         *'
              DISPLAY '*        PROGRAMA  ENCERRADO        *'
              DISPLAY '************** GFCT4265 *************'
              MOVE WRK-OPERACAO        TO  WRK-OPERACAO-TXT
              MOVE 'APURACAO'          TO  WRK-ARQ-TXT
              MOVE WRK-TEXTO           TO  ERR-TEXTO
              PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1400-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA TESTAR-FS-SAIDAZON
      *===============================================================*
       1500-TESTAR-FS-SAIDAZON SECTION.
      *---------------------------------------------------------------*

           IF WRK-FS-SAIDAZON NOT EQUAL '00'
              DISPLAY '************** GFCT4265 *************'
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
              DISPLAY '*              SAIDAZON             *'
              DISPLAY '*         FILE STATUS =  ' WRK-FS-SAIDAZON
                                                 '         *'
              DISPLAY '*        PROGRAMA  ENCERRADO        *'
              DISPLAY '************** GFCT4265 *************'
              MOVE WRK-OPERACAO        TO  WRK-OPERACAO-TXT
              MOVE 'SAIDAZON'          TO  WRK-ARQ-TXT
              MOVE WRK-TEXTO           TO  ERR-TEXTO
              PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1500-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA ROTINA-INICIAL
      *===============================================================*
       2000-ROTINA-INICIAL SECTION.
      *---------------------------------------------------------------*

           PERFORM 2100-LER-CONSOLID.

           IF  ACU-LIDOS-CONSOLID      EQUAL   ZEROS
               DISPLAY '************** GFCT4265 *************'
               DISPLAY '*                                   *'
               DISPLAY '*    ARQUIVO CONSOLID ESTA VAZIO    *'
               DISPLAY '*                                   *'
               DISPLAY '************** GFCT4265 *************'
           END-IF.

           PERFORM 2200-LER-DETALHAD.

           IF  ACU-LIDOS-DETALHAD      EQUAL   ZEROS
               DISPLAY '************** GFCT4265 *************'
               DISPLAY '*                                   *'
               DISPLAY '*    ARQUIVO DETALHAD ESTA VAZIO    *'
               DISPLAY '*                                   *'
               DISPLAY '************** GFCT4265 *************'
           END-IF.


           CALL 'BRAD0560'             USING WRK-P0560-JOBNAME
                                             WRK-P0560-JOBNUMBER.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE SPACES             TO  WRK-P0560-JOBNAME
           END-IF.


      *--> OBTER DATA E HORA DO SISTEMA

           CALL 'BRAD7600'             USING  WRK-DATA-HORA-7600
           MOVE WRK-DT-AAAAMMDD        TO  WRK-DT-SISTEMA.
           MOVE WRK-TI-HHMMSS          TO  WRK-HR-SISTEMA.

           MOVE WRK-DT-SISTEMA-R       TO  BB1-DATA
           MOVE WRK-HR-SISTEMA-R       TO  BB1-HORA

           MOVE WRK-ANO-SIS            TO  WRK-ANO-DMA
           MOVE WRK-MES-SIS            TO  WRK-MES-DMA
           MOVE WRK-DIA-SIS            TO  WRK-DIA-DMA
           MOVE WRK-DATA-DMA-BARRA     TO  WRK-DISPL-05-DATA

           MOVE WRK-HOR-SIS            TO  WRK-HOR-HMS
           MOVE WRK-MIN-SIS            TO  WRK-MIN-HMS
           MOVE WRK-SEG-SIS            TO  WRK-SEG-HMS
           MOVE WRK-HORA-HMS-2PTS      TO  WRK-DISPL-05-HORA.

      *---------------------------------------------------------------*
       2000-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA LER-CONSOLID
      *===============================================================*
       2100-LER-CONSOLID SECTION.
      *---------------------------------------------------------------*

           READ CONSOLID.

           IF  WRK-FS-CONSOLID         EQUAL   '10'
               MOVE  HIGH-VALUES       TO  WRK-CHAVE-CONSOLID
               GO                      TO  2100-99-FIM
           END-IF.

           MOVE    WRK-LEITURA         TO  WRK-OPERACAO.
           PERFORM 1100-TESTAR-FS-CONSOLID.

           ADD 1                       TO  ACU-LIDOS-CONSOLID

           MOVE COBR-DIG-CONTRATO      OF  CONSOLID
                                       TO  WRK-CHV-DIG-CONS
           MOVE COBR-NUM-CONTRATO      OF  CONSOLID
                                       TO  WRK-CHV-NUM-CONS.

           EVALUATE COBR-TIPO-COBRANCA OF  CONSOLID
             WHEN 09
               ADD 1                   TO  ACU-REG-TIPO-09
               MOVE COBR-VALOR-DEBITAR OF  CONSOLID
                                       TO  WRK-TAM-11V2-COMP
               ADD WRK-TAM-11V2-COMP   TO  ACU-VLR-TIPO-09

             WHEN 02
               ADD 1                   TO  ACU-REG-TIPO-02
               MOVE COBR-VALOR-DEBITAR OF  CONSOLID
                                       TO  WRK-TAM-11V2-COMP
               ADD WRK-TAM-11V2-COMP   TO  ACU-VLR-TIPO-02

             WHEN 03
               ADD 1                   TO  ACU-REG-TIPO-03
               MOVE COBR-VALOR-DEBITAR OF  CONSOLID
                                       TO  WRK-TAM-11V2-COMP
               ADD WRK-TAM-11V2-COMP   TO  ACU-VLR-TIPO-03

             WHEN 07
               ADD 1                   TO  ACU-REG-TIPO-07
               MOVE COBR-VALOR-DEBITAR OF  CONSOLID
                                       TO  WRK-TAM-11V2-COMP
               ADD WRK-TAM-11V2-COMP   TO  ACU-VLR-TIPO-07

           END-EVALUATE.

      *---------------------------------------------------------------*
       2100-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA LER-DETALHAD
      *===============================================================*
       2200-LER-DETALHAD SECTION.
      *---------------------------------------------------------------*

           READ DETALHAD.

           IF  WRK-FS-DETALHAD         EQUAL   '10'
               MOVE  HIGH-VALUES       TO  WRK-CHAVE-DETALHAD
               GO                      TO  2200-99-FIM
           END-IF.

           MOVE    WRK-LEITURA         TO  WRK-OPERACAO.
           PERFORM 1200-TESTAR-FS-DETALHAD.

           ADD 1                       TO  ACU-LIDOS-DETALHAD.

           MOVE COBR-DIG-CONTRATO      OF  DETALHAD
                                       TO  WRK-CHV-DIG-DETA
           MOVE COBR-NUM-CONTRATO      OF  DETALHAD
                                       TO  WRK-CHV-NUM-DETA.

           EVALUATE COBR-TIPO-COBRANCA OF  DETALHAD
             WHEN 01
               ADD 1                   TO  ACU-REG-TIPO-01
               MOVE COBR-VALOR-DEBITAR OF  DETALHAD
                                       TO  WRK-TAM-11V2-COMP
               ADD WRK-TAM-11V2-COMP   TO  ACU-VLR-TIPO-01

             WHEN 10
               ADD 1                   TO  ACU-REG-TIPO-10
               MOVE COBR-VALOR-DEBITAR OF  DETALHAD
                                       TO  WRK-TAM-11V2-COMP
               ADD WRK-TAM-11V2-COMP   TO  ACU-VLR-TIPO-10

             WHEN 04
               ADD 1                   TO  ACU-REG-TIPO-04
               MOVE COBR-VALOR-DEBITAR OF  DETALHAD
                                       TO  WRK-TAM-11V2-COMP
               ADD WRK-TAM-11V2-COMP   TO  ACU-VLR-TIPO-04

             WHEN 05
               ADD 1                   TO  ACU-REG-TIPO-05
               MOVE COBR-VALOR-DEBITAR OF  DETALHAD
                                       TO  WRK-TAM-11V2-COMP
               ADD WRK-TAM-11V2-COMP   TO  ACU-VLR-TIPO-05

             WHEN 08
               ADD 1                   TO  ACU-REG-TIPO-08
               MOVE COBR-VALOR-DEBITAR OF  DETALHAD
                                       TO  WRK-TAM-11V2-COMP
               ADD WRK-TAM-11V2-COMP   TO  ACU-VLR-TIPO-08

           END-EVALUATE.

      *---------------------------------------------------------------*
       2200-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA PROCESSA
      *===============================================================*
       3000-PROCESSA SECTION.
      *---------------------------------------------------------------*

      *--> VALIDAR SE AS CHAVES POSSUEM CORRESPONDENTES

           IF  WRK-CHAVE-CONSOLID      EQUAL WRK-CHAVE-DETALHAD
               PERFORM 3300-TRATAR-IGUAL
           ELSE

               IF  WRK-CHAVE-CONSOLID  LESS WRK-CHAVE-DETALHAD
                   PERFORM 2100-LER-CONSOLID
               ELSE

                   IF  COBR-TIPO-COBRANCA OF DETALHAD EQUAL 1
                       PERFORM 3100-GRAVAR-SAIDAZON-DETA
                   END-IF
                   PERFORM 2200-LER-DETALHAD
               END-IF
           END-IF.

      *---------------------------------------------------------------*
       3000-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA GRAVAR-SAIDAZON-DETA
      *===============================================================*
       3100-GRAVAR-SAIDAZON-DETA  SECTION.
      *---------------------------------------------------------------*

220512     MOVE SPACES                 TO  BB0-REG
220512
           EVALUATE COBR-TIPO-COBRANCA OF  DETALHAD
             WHEN 01
               MOVE '01-D-INDIV'       TO  BB0-IDENT-ARQUIVO
             WHEN 10
               MOVE '10-D-EMPRE'       TO  BB0-IDENT-ARQUIVO
             WHEN 04
               MOVE '04-D-UNIFI'       TO  BB0-IDENT-ARQUIVO
             WHEN 05
               MOVE '05-D-CONSO'       TO  BB0-IDENT-ARQUIVO
             WHEN 08
               MOVE '08-D-AGRUP'       TO  BB0-IDENT-ARQUIVO
           END-EVALUATE.

           MOVE COBR-DATA-ENVIO        OF  DETALHAD
                                       TO  BB0-DATA-ENVIO
           MOVE COBR-DIG-CONTRATO      OF  DETALHAD
                                       TO  BB0-DIG-CONTRATO
           MOVE COBR-NUM-CONTRATO      OF  DETALHAD
                                       TO  BB0-NUM-CONTRATO
           MOVE COBR-DATA-ORIG         OF  DETALHAD
                                       TO  BB0-DATA-ORIG
           MOVE COBR-ROTINA-ORIG       OF  DETALHAD
                                       TO  BB0-ROTINA-ORIG
           MOVE COBR-NUM-ARQ-ORIG      OF  DETALHAD
                                       TO  BB0-NUM-ARQ-ORIG
           MOVE COBR-SEQ-ARQ-ORIG      OF  DETALHAD
                                       TO  BB0-SEQ-ARQ-ORIG
           MOVE COBR-TARIFA-ORIG       OF  DETALHAD
                                       TO  BB0-TARIFA-ORIG
           MOVE COBR-BANCO-DEB         OF  DETALHAD
                                       TO  BB0-BANCO-DEB
           MOVE COBR-AGENCIA-DEB       OF  DETALHAD
                                       TO  BB0-AGENCIA-DEB
           MOVE COBR-CONTA-DEB         OF  DETALHAD
                                       TO  BB0-CONTA-DEB
           MOVE COBR-VALOR-DEBITAR     OF  DETALHAD
                                       TO  BB0-VALOR-DEBITAR
           MOVE COBR-VALOR-DEBITADO    OF  DETALHAD
                                       TO  BB0-VALOR-DEBITADO
           MOVE COBR-TIPO-COBRANCA     OF  DETALHAD
                                       TO  BB0-TIPO-COBRANCA
           MOVE COBR-NUM-PARC          OF  DETALHAD
                                       TO  BB0-NUM-PARC

           MOVE WRK-GRAVACAO           TO  WRK-OPERACAO.

           WRITE FD-REG-SAIDAZON       FROM  BB0-REG
           PERFORM 1500-TESTAR-FS-SAIDAZON.

      *---------------------------------------------------------------*
       3100-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA GRAVAR-SAIDAZON-CONS
      *===============================================================*
       3200-GRAVAR-SAIDAZON-CONS  SECTION.
      *---------------------------------------------------------------*

220512     MOVE SPACES                 TO  BB0-REG
220512
           EVALUATE COBR-TIPO-COBRANCA OF  CONSOLID
             WHEN 09
               MOVE '09-C-EMPRE'       TO  BB0-IDENT-ARQUIVO
             WHEN 02
               MOVE '02-C-UNIFI'       TO  BB0-IDENT-ARQUIVO
             WHEN 03
               MOVE '03-C-CONSO'       TO  BB0-IDENT-ARQUIVO
             WHEN 07
               MOVE '07-C-AGRUP'       TO  BB0-IDENT-ARQUIVO
           END-EVALUATE.

           MOVE COBR-DATA-ENVIO        OF  CONSOLID
                                       TO  BB0-DATA-ENVIO
           MOVE COBR-DIG-CONTRATO      OF  CONSOLID
                                       TO  BB0-DIG-CONTRATO
           MOVE COBR-NUM-CONTRATO      OF  CONSOLID
                                       TO  BB0-NUM-CONTRATO
           MOVE COBR-DATA-ORIG         OF  CONSOLID
                                       TO  BB0-DATA-ORIG
           MOVE COBR-ROTINA-ORIG       OF  CONSOLID
                                       TO  BB0-ROTINA-ORIG
           MOVE COBR-NUM-ARQ-ORIG      OF  CONSOLID
                                       TO  BB0-NUM-ARQ-ORIG
           MOVE COBR-SEQ-ARQ-ORIG      OF  CONSOLID
                                       TO  BB0-SEQ-ARQ-ORIG
           MOVE COBR-TARIFA-ORIG       OF  CONSOLID
                                       TO  BB0-TARIFA-ORIG
           MOVE COBR-BANCO-DEB         OF  CONSOLID
                                       TO  BB0-BANCO-DEB
           MOVE COBR-AGENCIA-DEB       OF  CONSOLID
                                       TO  BB0-AGENCIA-DEB
           MOVE COBR-CONTA-DEB         OF  CONSOLID
                                       TO  BB0-CONTA-DEB
           MOVE COBR-VALOR-DEBITAR     OF  CONSOLID
                                       TO  BB0-VALOR-DEBITAR
           MOVE COBR-VALOR-DEBITADO    OF  CONSOLID
                                       TO  BB0-VALOR-DEBITADO
           MOVE COBR-TIPO-COBRANCA     OF  CONSOLID
                                       TO  BB0-TIPO-COBRANCA
           MOVE COBR-NUM-PARC          OF  CONSOLID
                                       TO  BB0-NUM-PARC

           MOVE WRK-GRAVACAO           TO  WRK-OPERACAO.

           WRITE FD-REG-SAIDAZON       FROM  BB0-REG
           PERFORM 1500-TESTAR-FS-SAIDAZON.

      *---------------------------------------------------------------*
       3200-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA TRATAR-IGUAL
      *===============================================================*
       3300-TRATAR-IGUAL  SECTION.
      *---------------------------------------------------------------*

           MOVE WRK-CHAVE-DETALHAD     TO  WRK-CHAVE-DETALHAD-ANT
           MOVE ZEROS                  TO  ACU-TOT-VALOR-DEBITAR

           PERFORM UNTIL WRK-FS-DETALHAD EQUAL '10'
               OR WRK-CHAVE-DETALHAD NOT EQUAL WRK-CHAVE-DETALHAD-ANT

               MOVE SPACES             TO  WRK-INCONSIS
               PERFORM 3330-CONSISTIR-TIPO-COBR

               IF  WRK-INCONSIS        EQUAL SPACES

                   MOVE COBR-VALOR-DEBITAR OF DETALHAD
                                       TO  WRK-TAM-11V2-COMP
                   COMPUTE ACU-TOT-VALOR-DEBITAR
                                       =   ACU-TOT-VALOR-DEBITAR +
                                           WRK-TAM-11V2-COMP
                   PERFORM 3100-GRAVAR-SAIDAZON-DETA
               ELSE
                   MOVE COBR-TIPO-COBRANCA OF  DETALHAD
                                       TO  WRK-MENSA-01-TIPO
                   MOVE WRK-MENSA-01   TO  BB4-DESCRICAO
                   PERFORM 3320-GRAVAR-OCORRENC-DETA
               END-IF

               PERFORM 2200-LER-DETALHAD

           END-PERFORM


           IF  ACU-TOT-VALOR-DEBITAR   NOT EQUAL
                                       COBR-VALOR-DEBITAR OF CONSOLID
               MOVE 'TOTAIS DETALHADO E CONSOLIDADO NAO BATEM'
                                       TO  BB4-DESCRICAO
               PERFORM 3310-GRAVAR-OCORRENC-CONS
           ELSE
               PERFORM 3200-GRAVAR-SAIDAZON-CONS
           END-IF

           PERFORM 2100-LER-CONSOLID.

      *---------------------------------------------------------------*
       3300-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA GRAVAR-OCORRENC-CONS
      *===============================================================*
       3310-GRAVAR-OCORRENC-CONS  SECTION.
      *---------------------------------------------------------------*

           MOVE COBR-CHAVE-COBRANCA    OF  CONSOLID
                                       TO  BB4-CHAVE-COBRANCA
           MOVE COBR-CHAVE-ORIGEM      OF  CONSOLID
                                       TO  BB4-CHAVE-ORIGEM
           MOVE COBR-CGC-CPF           OF  CONSOLID
                                       TO  BB4-CGC-CPF
           MOVE COBR-DADOS-DEBITO      OF  CONSOLID
                                       TO  BB4-DADOS-DEBITO
           MOVE COBR-DETALHES-COBRANCA OF  CONSOLID
                                       TO  BB4-DETALHES-COBRANCA

           MOVE WRK-P0560-JOBNAME      TO  BB4-JOB
           MOVE 'GFCT4265'             TO  BB4-PROGRAMA
           MOVE COBR-RECURSO           OF  CONSOLID
                                       TO  BB4-RECURSO
           MOVE COBR-COD-RETORNO       OF  CONSOLID
                                       TO  BB4-COD-RETORNO
           MOVE COBR-VALOR-DEBITADO    OF  CONSOLID
                                       TO  BB4-VALOR-DEBITADO

           MOVE COBR-TRAG-EXT          OF  CONSOLID
                                       TO  BB4-TRAG-EXT
           MOVE COBR-DOCTO-EXTRT       OF  CONSOLID
                                       TO  BB4-DOCTO-EXTRT
           MOVE COBR-NUM-PARC          OF  CONSOLID
                                       TO  BB4-NUM-PARC

           MOVE WRK-GRAVACAO           TO  WRK-OPERACAO.

           WRITE FD-REG-OCORRENC       FROM  REG-COBRANCA OF CONSOLID
           PERFORM 1300-TESTAR-FS-OCORRENC.

           ADD 1                       TO  ACU-REG-TOT-INV
           MOVE COBR-VALOR-DEBITAR     OF  CONSOLID
                                       TO  WRK-TAM-11V2-COMP
           ADD WRK-TAM-11V2-COMP       TO  ACU-VLR-TOT-INV.

      *---------------------------------------------------------------*
       3310-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA GRAVAR-OCORRENC-DETA
      *===============================================================*
       3320-GRAVAR-OCORRENC-DETA  SECTION.
      *---------------------------------------------------------------*

           MOVE COBR-CHAVE-COBRANCA    OF  DETALHAD
                                       TO  BB4-CHAVE-COBRANCA
           MOVE COBR-CHAVE-ORIGEM      OF  DETALHAD
                                       TO  BB4-CHAVE-ORIGEM
           MOVE COBR-CGC-CPF           OF  DETALHAD
                                       TO  BB4-CGC-CPF
           MOVE COBR-DADOS-DEBITO      OF  DETALHAD
                                       TO  BB4-DADOS-DEBITO
           MOVE COBR-DETALHES-COBRANCA OF  DETALHAD
                                       TO  BB4-DETALHES-COBRANCA

           MOVE WRK-P0560-JOBNAME      TO  BB4-JOB
           MOVE 'GFCT4265'             TO  BB4-PROGRAMA
           MOVE COBR-RECURSO           OF  DETALHAD
                                       TO  BB4-RECURSO
           MOVE COBR-COD-RETORNO       OF  DETALHAD
                                       TO  BB4-COD-RETORNO
           MOVE COBR-VALOR-DEBITADO    OF  DETALHAD
                                       TO  BB4-VALOR-DEBITADO

           MOVE COBR-TRAG-EXT          OF  DETALHAD
                                       TO  BB4-TRAG-EXT
           MOVE COBR-DOCTO-EXTRT       OF  DETALHAD
                                       TO  BB4-DOCTO-EXTRT
           MOVE COBR-NUM-PARC          OF  DETALHAD
                                       TO  BB4-NUM-PARC

           MOVE WRK-GRAVACAO           TO  WRK-OPERACAO.

           WRITE FD-REG-OCORRENC       FROM  REG-COBRANCA OF DETALHAD
           PERFORM 1300-TESTAR-FS-OCORRENC.

           ADD 1                       TO  ACU-REG-TOT-INV
           MOVE COBR-VALOR-DEBITAR     OF  DETALHAD
                                       TO  WRK-TAM-11V2-COMP
           ADD WRK-TAM-11V2-COMP       TO  ACU-VLR-TOT-INV.

      *---------------------------------------------------------------*
       3320-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA CONSISTIR-TIPO-COBR
      *===============================================================*
       3330-CONSISTIR-TIPO-COBR SECTION.
      *---------------------------------------------------------------*

      *--> CONSISTIR O TIPO DE COBRANCA ENTRE OS ARQUIVOS

           EVALUATE COBR-TIPO-COBRANCA OF  CONSOLID
             WHEN 09
               IF  COBR-TIPO-COBRANCA  OF  DETALHAD NOT EQUAL 10
                   MOVE 'S'            TO  WRK-INCONSIS
               END-IF

             WHEN 02
               IF  COBR-TIPO-COBRANCA  OF  DETALHAD NOT EQUAL 04
                   MOVE 'S'            TO  WRK-INCONSIS
               END-IF

             WHEN 03
               IF  COBR-TIPO-COBRANCA  OF  DETALHAD NOT EQUAL 05
                   MOVE 'S'            TO  WRK-INCONSIS
               END-IF

             WHEN 07
               IF  COBR-TIPO-COBRANCA  OF  DETALHAD NOT EQUAL 08
                   MOVE 'S'            TO  WRK-INCONSIS
               END-IF
           END-EVALUATE.

      *---------------------------------------------------------------*
       3330-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA FINAL
      *===============================================================*
       4000-ROTINA-FINAL   SECTION.
      *---------------------------------------------------------------*

           PERFORM 4100-GRAVAR-APURACAO

      *--> MOSTRAR TOTALIZACAO

           DISPLAY WRK-DISPL-01
           DISPLAY WRK-DISPL-02
           DISPLAY ' '
           DISPLAY WRK-DISPL-03
           DISPLAY WRK-DISPL-04

           MOVE 'INDIVIDUAL. (01)'     TO  WRK-DISPL-05-LITER
           MOVE ACU-REG-TIPO-01        TO  WRK-DISPL-05-REG
           MOVE ACU-VLR-TIPO-01        TO  WRK-DISPL-05-VALOR
           DISPLAY WRK-DISPL-05

           MOVE 'EMPR. INDIV (09)'     TO  WRK-DISPL-05-LITER
           MOVE ACU-REG-TIPO-09        TO  WRK-DISPL-05-REG
           MOVE ACU-VLR-TIPO-09        TO  WRK-DISPL-05-VALOR
           DISPLAY WRK-DISPL-05

           MOVE 'EMPR. AGRUP (10)'     TO  WRK-DISPL-05-LITER
           MOVE ACU-REG-TIPO-10        TO  WRK-DISPL-05-REG
           MOVE ACU-VLR-TIPO-10        TO  WRK-DISPL-05-VALOR
           DISPLAY WRK-DISPL-05

           MOVE 'UNIF. INDIV (02)'     TO  WRK-DISPL-05-LITER
           MOVE ACU-REG-TIPO-02        TO  WRK-DISPL-05-REG
           MOVE ACU-VLR-TIPO-02        TO  WRK-DISPL-05-VALOR
           DISPLAY WRK-DISPL-05

           MOVE 'UNIF. AGRUP (04)'     TO  WRK-DISPL-05-LITER
           MOVE ACU-REG-TIPO-04        TO  WRK-DISPL-05-REG
           MOVE ACU-VLR-TIPO-04        TO  WRK-DISPL-05-VALOR
           DISPLAY WRK-DISPL-05

           MOVE 'CONS. INDIV (03)'     TO  WRK-DISPL-05-LITER
           MOVE ACU-REG-TIPO-03        TO  WRK-DISPL-05-REG
           MOVE ACU-VLR-TIPO-03        TO  WRK-DISPL-05-VALOR
           DISPLAY WRK-DISPL-05

           MOVE 'CONS. AGRUP (05)'     TO  WRK-DISPL-05-LITER
           MOVE ACU-REG-TIPO-05        TO  WRK-DISPL-05-REG
           MOVE ACU-VLR-TIPO-05        TO  WRK-DISPL-05-VALOR
           DISPLAY WRK-DISPL-05

           MOVE 'AGR.  INDIV (07)'     TO  WRK-DISPL-05-LITER
           MOVE ACU-REG-TIPO-07        TO  WRK-DISPL-05-REG
           MOVE ACU-VLR-TIPO-07        TO  WRK-DISPL-05-VALOR
           DISPLAY WRK-DISPL-05

           MOVE 'AGR.  AGRUP (08)'     TO  WRK-DISPL-05-LITER
           MOVE ACU-REG-TIPO-08        TO  WRK-DISPL-05-REG
           MOVE ACU-VLR-TIPO-08        TO  WRK-DISPL-05-VALOR
           DISPLAY WRK-DISPL-05

           COMPUTE ACU-REG-TOT-ENV = ACU-REG-TIPO-01 +
230512                               ACU-REG-TIPO-09 +
230512                               ACU-REG-TIPO-02 +
230512                               ACU-REG-TIPO-03 +
230512                               ACU-REG-TIPO-07

           COMPUTE ACU-VLR-TOT-ENV = ACU-VLR-TIPO-01 +
230512                               ACU-VLR-TIPO-09 +
230512                               ACU-VLR-TIPO-02 +
230512                               ACU-VLR-TIPO-03 +
230512                               ACU-VLR-TIPO-07

           MOVE 'TOTAL ENVIADO   '     TO  WRK-DISPL-05-LITER
           MOVE ACU-REG-TOT-ENV        TO  WRK-DISPL-05-REG
           MOVE ACU-VLR-TOT-ENV        TO  WRK-DISPL-05-VALOR
           DISPLAY WRK-DISPL-05

           MOVE 'INVALIDOS       '     TO  WRK-DISPL-05-LITER
           MOVE ACU-REG-TOT-INV        TO  WRK-DISPL-05-REG
           MOVE ACU-VLR-TOT-INV        TO  WRK-DISPL-05-VALOR
           DISPLAY WRK-DISPL-05

           DISPLAY WRK-DISPL-01.

      *---------------------------------------------------------------*
       4000-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA GRAVAR-APURACAO
      *===============================================================*
       4100-GRAVAR-APURACAO SECTION.
      *---------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO  WRK-OPERACAO

           MOVE WRK-P0560-JOBNAME      TO  BB1-JOB
           MOVE 'GFCT4265'             TO  BB1-PGM
           MOVE '-'                    TO  BB1-SEPAR

           MOVE WRK-LAY-01             TO  BB1-LINHA
           WRITE FD-REG-APURACAO     FROM  BB1-REG
           PERFORM 1400-TESTAR-FS-APURACAO

           MOVE WRK-LAY-02             TO  BB1-LINHA
           WRITE FD-REG-APURACAO     FROM  BB1-REG
           PERFORM 1400-TESTAR-FS-APURACAO

           MOVE WRK-LAY-03             TO  BB1-LINHA
           WRITE FD-REG-APURACAO     FROM  BB1-REG
           PERFORM 1400-TESTAR-FS-APURACAO

           MOVE 'INDIVIDUAL. (01)'     TO  WRK-LAY-04-LITER
           MOVE ACU-REG-TIPO-01        TO  WRK-LAY-04-REG
           MOVE ACU-VLR-TIPO-01        TO  WRK-LAY-04-VALOR
           MOVE WRK-LAY-04             TO  BB1-LINHA
           WRITE FD-REG-APURACAO     FROM  BB1-REG
           PERFORM 1400-TESTAR-FS-APURACAO

           MOVE 'EMPR. INDIV (09)'     TO  WRK-LAY-04-LITER
           MOVE ACU-REG-TIPO-09        TO  WRK-LAY-04-REG
           MOVE ACU-VLR-TIPO-09        TO  WRK-LAY-04-VALOR
           MOVE WRK-LAY-04             TO  BB1-LINHA
           WRITE FD-REG-APURACAO     FROM  BB1-REG
           PERFORM 1400-TESTAR-FS-APURACAO

           MOVE 'EMPR. AGRUP (10)'     TO  WRK-LAY-04-LITER
           MOVE ACU-REG-TIPO-10        TO  WRK-LAY-04-REG
           MOVE ACU-VLR-TIPO-10        TO  WRK-LAY-04-VALOR
           MOVE WRK-LAY-04             TO  BB1-LINHA
           WRITE FD-REG-APURACAO     FROM  BB1-REG
           PERFORM 1400-TESTAR-FS-APURACAO

           MOVE 'UNIF. INDIV (02)'     TO  WRK-LAY-04-LITER
           MOVE ACU-REG-TIPO-02        TO  WRK-LAY-04-REG
           MOVE ACU-VLR-TIPO-02        TO  WRK-LAY-04-VALOR
           MOVE WRK-LAY-04             TO  BB1-LINHA
           WRITE FD-REG-APURACAO     FROM  BB1-REG
           PERFORM 1400-TESTAR-FS-APURACAO

           MOVE 'UNIF. AGRUP (04)'     TO  WRK-LAY-04-LITER
           MOVE ACU-REG-TIPO-04        TO  WRK-LAY-04-REG
           MOVE ACU-VLR-TIPO-04        TO  WRK-LAY-04-VALOR
           MOVE WRK-LAY-04             TO  BB1-LINHA
           WRITE FD-REG-APURACAO     FROM  BB1-REG
           PERFORM 1400-TESTAR-FS-APURACAO

           MOVE 'CONS. INDIV (03)'     TO  WRK-LAY-04-LITER
           MOVE ACU-REG-TIPO-03        TO  WRK-LAY-04-REG
           MOVE ACU-VLR-TIPO-03        TO  WRK-LAY-04-VALOR
           MOVE WRK-LAY-04             TO  BB1-LINHA
           WRITE FD-REG-APURACAO     FROM  BB1-REG
           PERFORM 1400-TESTAR-FS-APURACAO

           MOVE 'CONS. AGRUP (05)'     TO  WRK-LAY-04-LITER
           MOVE ACU-REG-TIPO-05        TO  WRK-LAY-04-REG
           MOVE ACU-VLR-TIPO-05        TO  WRK-LAY-04-VALOR
           MOVE WRK-LAY-04             TO  BB1-LINHA
           WRITE FD-REG-APURACAO     FROM  BB1-REG
           PERFORM 1400-TESTAR-FS-APURACAO

           MOVE 'AGR.  INDIV (07)'     TO  WRK-LAY-04-LITER
           MOVE ACU-REG-TIPO-07        TO  WRK-LAY-04-REG
           MOVE ACU-VLR-TIPO-07        TO  WRK-LAY-04-VALOR
           MOVE WRK-LAY-04             TO  BB1-LINHA
           WRITE FD-REG-APURACAO     FROM  BB1-REG
           PERFORM 1400-TESTAR-FS-APURACAO

           MOVE 'AGR.  AGRUP (08)'     TO  WRK-LAY-04-LITER
           MOVE ACU-REG-TIPO-08        TO  WRK-LAY-04-REG
           MOVE ACU-VLR-TIPO-08        TO  WRK-LAY-04-VALOR
           MOVE WRK-LAY-04             TO  BB1-LINHA
           WRITE FD-REG-APURACAO     FROM  BB1-REG
           PERFORM 1400-TESTAR-FS-APURACAO

           COMPUTE ACU-REG-TOT-ENV = ACU-REG-TIPO-01 +
230512                               ACU-REG-TIPO-09 +
230512                               ACU-REG-TIPO-02 +
230512                               ACU-REG-TIPO-03 +
230512                               ACU-REG-TIPO-07

           COMPUTE ACU-VLR-TOT-ENV = ACU-VLR-TIPO-01 +
230512                               ACU-VLR-TIPO-09 +
230512                               ACU-VLR-TIPO-02 +
230512                               ACU-VLR-TIPO-03 +
230512                               ACU-VLR-TIPO-07

           MOVE 'TOTAL ENVIADO   '     TO  WRK-LAY-04-LITER
           MOVE ACU-REG-TOT-ENV        TO  WRK-LAY-04-REG
           MOVE ACU-VLR-TOT-ENV        TO  WRK-LAY-04-VALOR
           MOVE WRK-LAY-04             TO  BB1-LINHA
           WRITE FD-REG-APURACAO     FROM  BB1-REG
           PERFORM 1400-TESTAR-FS-APURACAO

           MOVE 'INVALIDOS       '     TO  WRK-LAY-04-LITER
           MOVE ACU-REG-TOT-INV        TO  WRK-LAY-04-REG
           MOVE ACU-VLR-TOT-INV        TO  WRK-LAY-04-VALOR
           MOVE WRK-LAY-04             TO  BB1-LINHA
           WRITE FD-REG-APURACAO     FROM  BB1-REG
           PERFORM 1400-TESTAR-FS-APURACAO.

      *---------------------------------------------------------------*
       4100-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA ERRO
      *===============================================================*
       9999-ROTINA-ERRO SECTION.
      *---------------------------------------------------------------*

           MOVE 'APL'                  TO  ERR-TIPO-ACESSO
           MOVE 'GFCT4265'             TO  ERR-PGM

           CALL 'BRAD7100'          USING  WRK-BATCH
                                           ERRO-AREA.

           GOBACK.

      *---------------------------------------------------------------*
       9999-99-FIM. EXIT.
      *---------------------------------------------------------------*
