      *===============================================================*
       IDENTIFICATION DIVISION.
      *===============================================================*

       PROGRAM-ID. GFCT4260.

      *================================================================*
      *.                      S O N D A   I T                          *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA.........: GFCT4260                                 *
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
      *    OBJETIVO.........: SELECAO COBRANCA ACUMULADA - DETALHE E   *
      *                       AGRUPADO.                                *
      *                                                                *
      *    ARQUIVOS:                                                   *
      *      DDNAME                            INCLUDE/BOOK            *
      *        ENTRADA                           I#GFCTIL              *
      *        DETALHAD                          I#GFCTIL              *
      *        CONSOLID                          I#GFCTIL              *
      *        OCORRENC                          GFCTWBB4              *
      *                                                                *
      *    MODULOS CHAMADOS.:                                          *
      *      BRAD0560 - OBTER NOME DO JOB.                             *
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

           SELECT ENTRADA  ASSIGN TO UT-S-ENTRADA
                      FILE STATUS IS WRK-FS-ENTRADA.

           SELECT DETALHAD ASSIGN TO UT-S-DETALHAD
                      FILE STATUS IS WRK-FS-DETALHAD.

           SELECT CONSOLID ASSIGN TO UT-S-CONSOLID
                      FILE STATUS IS WRK-FS-CONSOLID.

           SELECT OCORRENC ASSIGN TO UT-S-OCORRENC
                      FILE STATUS IS WRK-FS-OCORRENC.

      *===============================================================*
       DATA DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       FILE SECTION.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      *   INPUT :     ORG. SEQUENCIAL   -   LRECL = 300               *
      *---------------------------------------------------------------*

       FD  ENTRADA
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-ENTRADA              PIC X(300).

      *---------------------------------------------------------------*
      *   OUTPUT:     ORG. SEQUENCIAL   -   LRECL = 300               *
      *---------------------------------------------------------------*

       FD  DETALHAD
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-DETALHAD             PIC X(300).

      *---------------------------------------------------------------*
      *   OUTPUT:     ORG. SEQUENCIAL   -   LRECL = 300               *
      *---------------------------------------------------------------*

       FD  CONSOLID
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-CONSOLID             PIC X(300).

      *---------------------------------------------------------------*
      *   OUTPUT:     ORG. SEQUENCIAL   -   LRECL = 304               *
      *---------------------------------------------------------------*

       FD  OCORRENC
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-OCORRENC             PIC X(304).

      *---------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *---------------------------------------------------------------*

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
       77  FILLER  PIC X(32)  VALUE  '*  INICIO DA WORKING GFCT4260  *'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*

      *---------------------------------------------------------------*
       01  FILLER  PIC X(32)  VALUE  '*  AREAS AUXILIARES           *'.
      *---------------------------------------------------------------*

       01  WRK-1-VEZ                   PIC  X(001)         VALUE 'S'.
       01  WRK-OCORREU                 PIC  X(001)         VALUE SPACES.
       01  WRK-FOUND                   PIC  X(001)         VALUE SPACES.
       01  WRK-CHV-DUPL                PIC  X(001)         VALUE SPACES.

       01  WRK-REG-COBRANCA-ANT        PIC  X(300)         VALUE SPACES.
       01  WRK-REG-COBRANCA-ATU        PIC  X(300)         VALUE SPACES.

       01  WRK-TAM-11V2-COMP           PIC 9(011)V99 COMP-3 VALUE ZEROS.

       01  FILLER.
           05  WRK-CHAVE-ATU.
               10  WRK-CHV-DAC-CTR-ATU PIC  X(001)         VALUE SPACES.
               10  WRK-CHV-NUM-CTR-ATU PIC  9(007)         VALUE ZEROS.

           05  WRK-CHAVE-ANT.
               10  WRK-CHV-DAC-CTR-ANT PIC  X(001)         VALUE SPACES.
               10  WRK-CHV-NUM-CTR-ANT PIC  9(007)         VALUE ZEROS.

       01  WRK-DISPLAYS.
           05  WRK-DISPL-01            PIC  X(070)         VALUE
               '************************** GFCT4260 ********************
      -        '******'.

           05  WRK-DISPL-02            PIC  X(070)         VALUE
               '*                   FINAL DE PROCESSAMENTO
      -        '     *'.

           05  WRK-DISPL-03            PIC  X(070)         VALUE
               '*
      -        '     *'.

           05  WRK-DISPL-04            PIC  X(070)         VALUE
               '* ARQUIVO     REG.           VALOR DEBITAR    VALOR DEBI
      -        'TADO *'.

           05  WRK-DISPL-05.
               10  FILLER              PIC  X(002)         VALUE '* '.
               10  WRK-DISPL-05-LITER.
                   15  WRK-DISPL-05-LIT-1
                                       PIC  X(006)         VALUE SPACES.
                   15  WRK-DISPL-05-LIT-2
                                       PIC  9(002)         VALUE ZEROS.
               10  FILLER              PIC  X(002)         VALUE SPACES.
               10  WRK-DISPL-05-REG    PIC ZZZZ.ZZZ.ZZ9    VALUE ZEROS.
               10  FILLER              PIC  X(002)         VALUE SPACES.
               10  WRK-DISPL-05-DBTAR  PIC ZZZZZ.ZZZ.ZZ9,99 VALUE ZEROS.
               10  FILLER              PIC  X(002)         VALUE SPACES.
               10  WRK-DISPL-05-DBTDO  PIC ZZZZZ.ZZZ.ZZ9,99 VALUE ZEROS.
               10  FILLER              PIC  X(002)         VALUE ' *'.

      *----------------------------------------------------------------*
       01  FILLER  PIC X(32)  VALUE  '*  AREAS TAB INTERNA          *'.
      *----------------------------------------------------------------*

       01  IND-1                       PIC  9(003) COMP-3  VALUE ZEROS.

       COPY 'GFCTWBB5'.

      *---------------------------------------------------------------*
       01  FILLER  PIC X(32)  VALUE  '*  AREAS DE FILE STATUS       *'.
      *---------------------------------------------------------------*

       77  WRK-BATCH                   PIC X(08) VALUE 'BATCH   '.

       77  WRK-FS-ENTRADA              PIC X(02) VALUE SPACES.
       77  WRK-FS-DETALHAD             PIC X(02) VALUE SPACES.
       77  WRK-FS-CONSOLID             PIC X(02) VALUE SPACES.
       77  WRK-FS-OCORRENC             PIC X(02) VALUE SPACES.

       77  WRK-OPERACAO                PIC X(13) VALUE SPACES.
       77  WRK-ABERTURA                PIC X(13) VALUE 'NA ABERTURA'.
       77  WRK-LEITURA                 PIC X(13) VALUE 'NA LEITURA'.
       77  WRK-GRAVACAO                PIC X(13) VALUE 'NA GRAVACAO'.
       77  WRK-FECHAMENTO              PIC X(13) VALUE 'NO FECHAMENTO'.

      *---------------------------------------------------------------*
       77  FILLER  PIC X(32)  VALUE  '*  ACUMULADORES               *'.
      *---------------------------------------------------------------*

       77  ACU-LIDOS-ENTRADA           PIC 9(10) COMP-3    VALUE ZEROS.
       77  ACU-GRAVA-DETALHAD          PIC 9(10) COMP-3    VALUE ZEROS.
       77  ACU-GRAVA-CONSOLID          PIC 9(10) COMP-3    VALUE ZEROS.
       77  ACU-GRAVA-OCORRENC          PIC 9(10) COMP-3    VALUE ZEROS.

       77  ACU-DBTAR-ENTRADA           PIC 9(11)V99 COMP-3 VALUE ZEROS.
       77  ACU-DBTDO-ENTRADA           PIC 9(11)V99 COMP-3 VALUE ZEROS.
       77  ACU-DBTAR-DETALHAD          PIC 9(11)V99 COMP-3 VALUE ZEROS.
       77  ACU-DBTDO-DETALHAD          PIC 9(11)V99 COMP-3 VALUE ZEROS.
       77  ACU-DBTAR-CONSOLID          PIC 9(11)V99 COMP-3 VALUE ZEROS.
       77  ACU-DBTDO-CONSOLID          PIC 9(11)V99 COMP-3 VALUE ZEROS.
       77  ACU-DBTAR-OCORRENC          PIC 9(11)V99 COMP-3 VALUE ZEROS.
       77  ACU-DBTDO-OCORRENC          PIC 9(11)V99 COMP-3 VALUE ZEROS.

      *---------------------------------------------------------------*
       01  FILLER  PIC X(32)  VALUE  '*  AREAS LAYOUT DOS ARQ.      *'.
      *---------------------------------------------------------------*

       COPY 'I#GFCTIL'.

      *---------------------------------------------------------------*
       01  FILLER  PIC X(32)  VALUE  '*  AREAS LAYOUT OCORRENC      *'.
      *---------------------------------------------------------------*

       COPY 'GFCTWBB4'.

      *---------------------------------------------------------------*
       01  FILLER  PIC X(32)  VALUE  '*  AREAS BRAD0560             *'.
      *---------------------------------------------------------------*

       01  WRK-P0560-JOBNAME           PIC X(008)          VALUE SPACES.
       01  WRK-P0560-JOBNUMBER         PIC 9(005)          VALUE ZEROS.

      *---------------------------------------------------------------*
       01  FILLER  PIC X(32)  VALUE  '*   AREAS TRATAMENTO ERRO    *'.
      *---------------------------------------------------------------*

       COPY 'POL7100C'.

       01  WRK-TEXTO.
           05  FILLER                  PIC X(05) VALUE 'ERRO'.
           05  WRK-OPERACAO-TXT        PIC X(13) VALUE SPACES.
           05  FILLER                  PIC X(12) VALUE ' DO ARQUIVO '.
           05  WRK-ARQ-TXT             PIC X(08) VALUE SPACES.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
       01  FILLER  PIC X(32)  VALUE  '*  FIM  DA WORKING   GFCT4260  *'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*

      *===============================================================*
       PROCEDURE DIVISION.
      *===============================================================*

      *===============================================================*
      *    ROTINA INICIAR
      *===============================================================*
       0000-INICIAR SECTION.
      *---------------------------------------------------------------*

           OPEN    INPUT   ENTRADA
                   OUTPUT  DETALHAD
                           CONSOLID
                           OCORRENC.

           MOVE    WRK-ABERTURA        TO  WRK-OPERACAO.
           PERFORM 1000-TESTAR-FILE-STATUS.

           PERFORM 2000-ROTINA-INICIAL.

           PERFORM 3000-PROCESSA
             UNTIL WRK-FS-ENTRADA      EQUAL '10'.

           PERFORM 4000-ROTINA-FINAL.


      *===============*
      * LABEL FINALIZAR
      *===============*
       0000-FINALIZAR.
      *---------------*

           CLOSE   ENTRADA
                   DETALHAD
                   CONSOLID
                   OCORRENC.

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

           PERFORM 1100-TESTAR-FS-ENTRADA.

           PERFORM 1200-TESTAR-FS-DETALHAD.

           PERFORM 1300-TESTAR-FS-CONSOLID.

           PERFORM 1400-TESTAR-FS-OCORRENC.

      *---------------------------------------------------------------*
       1000-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA TESTAR-FS-ENTRADA
      *===============================================================*
       1100-TESTAR-FS-ENTRADA  SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-ENTRADA          NOT EQUAL '00'
               DISPLAY '************** GFCT4260 *************'
               DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
               DISPLAY '*              ENTRADA              *'
               DISPLAY '*         FILE STATUS =  ' WRK-FS-ENTRADA
                                                  '         *'
               DISPLAY '*         PROGRAMA ENCERRADO        *'
               DISPLAY '************** GFCT4260 *************'
               MOVE WRK-OPERACAO       TO  WRK-OPERACAO-TXT
               MOVE 'ENTRADA'          TO  WRK-ARQ-TXT
               MOVE WRK-TEXTO          TO  ERR-TEXTO
               MOVE 'APL'              TO  ERR-TIPO-ACESSO
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

           IF  WRK-FS-DETALHAD         NOT EQUAL '00'
               DISPLAY '************** GFCT4260 *************'
               DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
               DISPLAY '*              DETALHAD             *'
               DISPLAY '*         FILE STATUS =  ' WRK-FS-DETALHAD
                                                  '         *'
               DISPLAY '*         PROGRAMA ENCERRADO        *'
               DISPLAY '************** GFCT4260 *************'
               MOVE WRK-OPERACAO       TO  WRK-OPERACAO-TXT
               MOVE 'DETALHAD'         TO  WRK-ARQ-TXT
               MOVE WRK-TEXTO          TO  ERR-TEXTO
               MOVE 'APL'              TO  ERR-TIPO-ACESSO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1200-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA TESTAR-FS-CONSOLID
      *===============================================================*
       1300-TESTAR-FS-CONSOLID SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-CONSOLID         NOT EQUAL '00'
               DISPLAY '************** GFCT4260 *************'
               DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
               DISPLAY '*              CONSOLID             *'
               DISPLAY '*         FILE STATUS =  ' WRK-FS-CONSOLID
                                                  '         *'
               DISPLAY '*         PROGRAMA ENCERRADO        *'
               DISPLAY '************** GFCT4260 *************'
               MOVE WRK-OPERACAO       TO  WRK-OPERACAO-TXT
               MOVE 'CONSOLID'         TO  WRK-ARQ-TXT
               MOVE WRK-TEXTO          TO  ERR-TEXTO
               MOVE 'APL'              TO  ERR-TIPO-ACESSO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1300-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA TESTAR-FS-OCORRENC
      *===============================================================*
       1400-TESTAR-FS-OCORRENC SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-OCORRENC         NOT EQUAL '00'
               DISPLAY '************** GFCT4260 *************'
               DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
               DISPLAY '*              OCORRENC             *'
               DISPLAY '*         FILE STATUS =  ' WRK-FS-OCORRENC
                                                  '         *'
               DISPLAY '*         PROGRAMA ENCERRADO        *'
               DISPLAY '************** GFCT4260 *************'
               MOVE WRK-OPERACAO       TO  WRK-OPERACAO-TXT
               MOVE 'OCORRENC'         TO  WRK-ARQ-TXT
               MOVE WRK-TEXTO          TO  ERR-TEXTO
               MOVE 'APL'              TO  ERR-TIPO-ACESSO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1400-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA ROTINA-INICIAL
      *===============================================================*
       2000-ROTINA-INICIAL SECTION.
      *---------------------------------------------------------------*

220512     MOVE ZEROS                  TO  COBR-TIPO-COBRANCA
           PERFORM 2100-LER-ENTRADA.

           IF  ACU-LIDOS-ENTRADA        EQUAL   ZEROS
               DISPLAY '************* GFCT4260 *************'
               DISPLAY '*                                  *'
               DISPLAY '*    ARQUIVO ENTRADA ESTA VAZIO    *'
               DISPLAY '*                                  *'
               DISPLAY '************* GFCT4260 *************'
           END-IF.


           CALL 'POOL0560'             USING WRK-P0560-JOBNAME
                                             WRK-P0560-JOBNUMBER.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE SPACES             TO  WRK-P0560-JOBNAME
           END-IF.

      *---------------------------------------------------------------*
       2000-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA LER-ENTRADA
      *===============================================================*
       2100-LER-ENTRADA   SECTION.
      *---------------------------------------------------------------*

220512     IF  COBR-TIPO-COBRANCA      NOT EQUAL 01
               MOVE REG-COBRANCA       TO  WRK-REG-COBRANCA-ANT
220512     END-IF.

           READ ENTRADA INTO REG-COBRANCA.

           IF  WRK-FS-ENTRADA          EQUAL  '10'
               MOVE HIGH-VALUES        TO  WRK-CHAVE-ATU
               GO                      TO  2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO  WRK-OPERACAO.
           PERFORM 1100-TESTAR-FS-ENTRADA.

           ADD  1                      TO  ACU-LIDOS-ENTRADA.

           MOVE COBR-VALOR-DEBITAR     TO  WRK-TAM-11V2-COMP
           ADD WRK-TAM-11V2-COMP       TO  ACU-DBTAR-ENTRADA

           MOVE COBR-VALOR-DEBITADO    TO  WRK-TAM-11V2-COMP
           ADD WRK-TAM-11V2-COMP       TO  ACU-DBTDO-ENTRADA

           MOVE COBR-DIG-CONTRATO      TO  WRK-CHV-DAC-CTR-ATU
           MOVE COBR-NUM-CONTRATO      TO  WRK-CHV-NUM-CTR-ATU.

      *---------------------------------------------------------------*
       2100-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA PROCESSA
      *===============================================================*
       3000-PROCESSA SECTION.
      *---------------------------------------------------------------*

           PERFORM 3400-ACUMULAR-VALORES

      *--> CONSISTIR O DIG E NRO DO CONTRATO

           IF  (COBR-DIG-CONTRATO       NOT ALPHABETIC AND
                COBR-DIG-CONTRATO       NOT NUMERIC)  OR
               COBR-DIG-CONTRATO       EQUAL SPACES   OR
               COBR-NUM-CONTRATO       NOT NUMERIC
               MOVE 'CHAVE COBRANCA INVALIDA'
                                       TO  BB4-DESCRICAO
               PERFORM 3100-GRAVAR-OCORRENC
               PERFORM 2100-LER-ENTRADA
               GO                      TO  3000-99-FIM
           END-IF.

      *--> SEPARAR OS DETALHES DO CONSOLIDADO

           IF  COBR-TIPO-COBRANCA      EQUAL 01
               PERFORM 3200-GRAVAR-DETALHAD
               PERFORM 2100-LER-ENTRADA
               GO                      TO  3000-99-FIM
           END-IF.

           PERFORM 3500-PROCESSAR-GRAVACAO

           PERFORM 2100-LER-ENTRADA.

      *---------------------------------------------------------------*
       3000-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA GRAVAR-OCORRENC
      *===============================================================*
       3100-GRAVAR-OCORRENC SECTION.
      *---------------------------------------------------------------*

           MOVE COBR-CHAVE-COBRANCA    TO  BB4-CHAVE-COBRANCA
           MOVE COBR-CHAVE-ORIGEM      TO  BB4-CHAVE-ORIGEM
           MOVE COBR-CGC-CPF           TO  BB4-CGC-CPF
           MOVE COBR-DADOS-DEBITO      TO  BB4-DADOS-DEBITO
           MOVE COBR-DETALHES-COBRANCA TO  BB4-DETALHES-COBRANCA

           MOVE WRK-P0560-JOBNAME      TO  BB4-JOB
           MOVE 'GFCT4260'             TO  BB4-PROGRAMA
           MOVE COBR-RECURSO           TO  BB4-RECURSO
           MOVE COBR-COD-RETORNO       TO  BB4-COD-RETORNO
           MOVE COBR-VALOR-DEBITADO    TO  BB4-VALOR-DEBITADO

           MOVE COBR-TRAG-EXT          TO  BB4-TRAG-EXT
           MOVE COBR-DOCTO-EXTRT       TO  BB4-DOCTO-EXTRT
           MOVE COBR-NUM-PARC          TO  BB4-NUM-PARC

           MOVE WRK-GRAVACAO           TO  WRK-OPERACAO

           WRITE FD-REG-OCORRENC     FROM  BB4-REG
           PERFORM 1400-TESTAR-FS-OCORRENC

           ADD 1                       TO  ACU-GRAVA-OCORRENC.

           MOVE COBR-VALOR-DEBITAR     TO  WRK-TAM-11V2-COMP
           ADD WRK-TAM-11V2-COMP       TO  ACU-DBTAR-OCORRENC

           MOVE COBR-VALOR-DEBITADO    TO  WRK-TAM-11V2-COMP
           ADD WRK-TAM-11V2-COMP       TO  ACU-DBTDO-OCORRENC.

      *---------------------------------------------------------------*
       3100-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA GRAVAR-DETALHAD
      *===============================================================*
       3200-GRAVAR-DETALHAD SECTION.
      *---------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO  WRK-OPERACAO

           WRITE FD-REG-DETALHAD     FROM  REG-COBRANCA
           PERFORM 1200-TESTAR-FS-DETALHAD

           ADD 1                       TO  ACU-GRAVA-DETALHAD.

           MOVE COBR-VALOR-DEBITAR     TO  WRK-TAM-11V2-COMP
           ADD WRK-TAM-11V2-COMP       TO  ACU-DBTAR-DETALHAD

           MOVE COBR-VALOR-DEBITADO    TO  WRK-TAM-11V2-COMP
           ADD WRK-TAM-11V2-COMP       TO  ACU-DBTDO-DETALHAD.

      *---------------------------------------------------------------*
       3200-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA GRAVAR-CONSOLID
      *===============================================================*
       3300-GRAVAR-CONSOLID SECTION.
      *---------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO  WRK-OPERACAO

           WRITE FD-REG-CONSOLID     FROM  REG-COBRANCA
           PERFORM 1300-TESTAR-FS-CONSOLID

           ADD 1                       TO  ACU-GRAVA-CONSOLID.

           MOVE COBR-VALOR-DEBITAR     TO  WRK-TAM-11V2-COMP
           ADD WRK-TAM-11V2-COMP       TO  ACU-DBTAR-CONSOLID

           MOVE COBR-VALOR-DEBITADO    TO  WRK-TAM-11V2-COMP
           ADD WRK-TAM-11V2-COMP       TO  ACU-DBTDO-CONSOLID.

      *---------------------------------------------------------------*
       3300-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA ACUMULAR-VALORES
      *===============================================================*
       3400-ACUMULAR-VALORES SECTION.
      *---------------------------------------------------------------*

           MOVE SPACES                 TO  WRK-FOUND
           PERFORM VARYING IND-1 FROM 1 BY 1
               UNTIL IND-1             GREATER 10 OR
                     WRK-FOUND         EQUAL 'S'

               IF  WRK-TAB-TIPO(IND-1) EQUAL ZEROS OR
                   WRK-TAB-TIPO(IND-1) EQUAL COBR-TIPO-COBRANCA

                   MOVE 'S'            TO  WRK-FOUND

                   MOVE COBR-TIPO-COBRANCA
                                       TO  WRK-TAB-TIPO(IND-1)
                   ADD 1               TO  WRK-TAB-QTDE(IND-1)

                   MOVE COBR-VALOR-DEBITAR
                                       TO  WRK-TAM-11V2-COMP
                   ADD WRK-TAM-11V2-COMP
                                       TO  WRK-TAB-VLR-DEBITAR(IND-1)

                   MOVE COBR-VALOR-DEBITADO
                                       TO  WRK-TAM-11V2-COMP
                   ADD WRK-TAM-11V2-COMP
                                       TO  WRK-TAB-VLR-DEBITADO(IND-1)
               END-IF
           END-PERFORM

           IF  WRK-FOUND               EQUAL SPACES
               IF  WRK-OCORREU         EQUAL SPACES
                   MOVE 'S'            TO  WRK-OCORREU
                   DISPLAY '************* GFCT4260 *************'
                   DISPLAY '*                                  *'
                   DISPLAY '*     EXCEDEU TABELA INTERNA DE    *'
                   DISPLAY '*         TIPOS DE COBRANCA        *'
                   DISPLAY '*                                  *'
                   DISPLAY '************* GFCT4260 *************'
               END-IF
           END-IF.

      *---------------------------------------------------------------*
       3400-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA PROCESSAR-GRAVACAO
      *===============================================================*
       3500-PROCESSAR-GRAVACAO SECTION.
      *---------------------------------------------------------------*

           IF  WRK-1-VEZ               EQUAL 'S'
               MOVE 'N'                TO  WRK-1-VEZ
               GO                      TO  3500-99-FIM
           END-IF.

           IF  WRK-CHAVE-ATU NOT EQUAL WRK-CHAVE-ANT
               MOVE WRK-CHAVE-ATU      TO  WRK-CHAVE-ANT

               IF  WRK-CHV-DUPL        EQUAL 'S'
                   MOVE SPACES         TO  WRK-CHV-DUPL

                   MOVE 'CHAVE COBRANCA DUPLICADA'
                                       TO  BB4-DESCRICAO
                   MOVE REG-COBRANCA   TO  WRK-REG-COBRANCA-ATU
                   MOVE WRK-REG-COBRANCA-ANT
                                       TO  REG-COBRANCA
                   PERFORM 3100-GRAVAR-OCORRENC
                   MOVE WRK-REG-COBRANCA-ATU
                                       TO  REG-COBRANCA
               ELSE
                   MOVE REG-COBRANCA   TO  WRK-REG-COBRANCA-ATU
                   MOVE WRK-REG-COBRANCA-ANT
                                       TO  REG-COBRANCA
                   PERFORM 3300-GRAVAR-CONSOLID
                   MOVE WRK-REG-COBRANCA-ATU
                                       TO  REG-COBRANCA
               END-IF
           ELSE
               MOVE 'S'                TO  WRK-CHV-DUPL

               MOVE 'CHAVE COBRANCA DUPLICADA'
                                       TO  BB4-DESCRICAO
               MOVE REG-COBRANCA       TO  WRK-REG-COBRANCA-ATU
               MOVE WRK-REG-COBRANCA-ANT
                                       TO  REG-COBRANCA
               PERFORM 3100-GRAVAR-OCORRENC
               MOVE WRK-REG-COBRANCA-ATU
                                       TO  REG-COBRANCA
           END-IF.

      *---------------------------------------------------------------*
       3500-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA FINAL
      *===============================================================*
       4000-ROTINA-FINAL   SECTION.
      *---------------------------------------------------------------*

           PERFORM 3500-PROCESSAR-GRAVACAO.

      *--> MOSTRAR TOTALIZACAO

           DISPLAY WRK-DISPL-01
           DISPLAY WRK-DISPL-02
           DISPLAY WRK-DISPL-03
           DISPLAY WRK-DISPL-04

           MOVE 'ENTRADA'              TO  WRK-DISPL-05-LITER
           MOVE ACU-LIDOS-ENTRADA      TO  WRK-DISPL-05-REG
           MOVE ACU-DBTAR-ENTRADA      TO  WRK-DISPL-05-DBTAR
           MOVE ACU-DBTDO-ENTRADA      TO  WRK-DISPL-05-DBTDO
           DISPLAY WRK-DISPL-05

           MOVE 'OCORRENC'             TO  WRK-DISPL-05-LITER
           MOVE ACU-GRAVA-OCORRENC     TO  WRK-DISPL-05-REG
           MOVE ACU-DBTAR-OCORRENC     TO  WRK-DISPL-05-DBTAR
           MOVE ACU-DBTDO-OCORRENC     TO  WRK-DISPL-05-DBTDO
           DISPLAY WRK-DISPL-05

           MOVE 'DETALHAD'             TO  WRK-DISPL-05-LITER
           MOVE ACU-GRAVA-DETALHAD     TO  WRK-DISPL-05-REG
           MOVE ACU-DBTAR-DETALHAD     TO  WRK-DISPL-05-DBTAR
           MOVE ACU-DBTDO-DETALHAD     TO  WRK-DISPL-05-DBTDO
           DISPLAY WRK-DISPL-05

           MOVE 'CONSOLID'             TO  WRK-DISPL-05-LITER
           MOVE ACU-GRAVA-CONSOLID     TO  WRK-DISPL-05-REG
           MOVE ACU-DBTAR-CONSOLID     TO  WRK-DISPL-05-DBTAR
           MOVE ACU-DBTDO-CONSOLID     TO  WRK-DISPL-05-DBTDO
           DISPLAY WRK-DISPL-05

           PERFORM VARYING IND-1 FROM 1 BY 1
               UNTIL IND-1             GREATER 10 OR
                     WRK-TAB-TIPO(IND-1) EQUAL ZEROS

               MOVE 'TIPO'             TO  WRK-DISPL-05-LIT-1
               MOVE WRK-TAB-TIPO(IND-1)
                                       TO  WRK-DISPL-05-LIT-2
               MOVE WRK-TAB-QTDE(IND-1)
                                       TO  WRK-DISPL-05-REG
               MOVE WRK-TAB-VLR-DEBITAR(IND-1)
                                       TO  WRK-DISPL-05-DBTAR
               MOVE WRK-TAB-VLR-DEBITADO(IND-1)
                                       TO  WRK-DISPL-05-DBTDO
               DISPLAY WRK-DISPL-05
           END-PERFORM.

           DISPLAY WRK-DISPL-01.

      *---------------------------------------------------------------*
       4000-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA ERRO
      *===============================================================*
       9999-ROTINA-ERRO SECTION.
      *---------------------------------------------------------------*

           MOVE 'GFCT4260'             TO  ERR-PGM

           CALL 'POOL7100'          USING  WRK-BATCH
                                           ERRO-AREA.

           GOBACK.

      *---------------------------------------------------------------*
       9999-99-FIM. EXIT.
      *---------------------------------------------------------------*
