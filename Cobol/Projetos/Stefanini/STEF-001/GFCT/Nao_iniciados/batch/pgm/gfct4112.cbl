      *===============================================================*
       IDENTIFICATION DIVISION.
      *===============================================================*

       PROGRAM-ID. GFCT4112.

      *================================================================*
      *                  SONDA PROCWORK INFORMATICA                    *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA.........: GFCT4112                                 *
      *    PROGRAMADOR......: FABRICA SOFTWARE        - SONDA PROCWORK *
      *                                                                *
      *    ANALISTA.........: ANTONIO PAGNOCCA NETO                    *
      *    LIDER PROJETO....: RODRIGO / MARCELO TEODORO                *
      *    GERENCIA / EQUIPE: GERENCIA-E / EQUIPE-1   - SONDA PROCWORK *
      *                                                                *
      *    GRUPO NEGOCIO....: DDS-GP50                                 *
      *    ANALISTA.........:                                          *
      *                                                                *
      *    DATA.............: DEZ/2010                                 *
      *                                                                *
      *    PROJETO..........: TARIFA PARCELADA                         *
      *                                                                *
      *    OBJETIVO.........: FORMATAR REGISTROS PARA ENVIO AO IRES.   *
      *                                                                *
      *    ARQUIVOS:                                                   *
      *      DDNAME                            INCLUDE/BOOK            *
      *      ENTRADA                             GFCTWAL2              *
      *      ARQDATAS                            I#GFCT06              *
      *      SAIDA                               GFCTWAOU              *
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

           SELECT ENTRADA  ASSIGN TO UT-S-ENTRADA
                      FILE STATUS IS WRK-FS-ENTRADA.

           SELECT ARQDATAS ASSIGN TO UT-S-ARQDATAS
                      FILE STATUS IS WRK-FS-ARQDATAS.

           SELECT SAIDA    ASSIGN TO UT-S-SAIDA
                      FILE STATUS IS WRK-FS-SAIDA.

      *===============================================================*
       DATA DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       FILE SECTION.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      *   INPUT :     ORG. SEQUENCIAL   -   LRECL = 187               *
      *---------------------------------------------------------------*

       FD  ENTRADA
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-ENTRADA              PIC X(187).

      *---------------------------------------------------------------*
      *   INPUT:      ORG. SEQUENCIAL   -   LRECL = 100               *
      *---------------------------------------------------------------*

       FD  ARQDATAS
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-ARQDATAS             PIC X(100).

      *---------------------------------------------------------------*
      *   OUTPUT:     ORG. SEQUENCIAL   -   LRECL = 130               *
      *---------------------------------------------------------------*

       FD  SAIDA
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-SAIDA                PIC X(130).

      *---------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *---------------------------------------------------------------*

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
       77  FILLER  PIC X(32)  VALUE  '*  INICIO DA WORKING GFCT4112  *'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*

      *---------------------------------------------------------------*
       01  FILLER  PIC X(32)  VALUE  '*   AREAS AUXILIARES           *'.
      *---------------------------------------------------------------*

       01  WRK-NAO-ENCONTROU           PIC  X(002)         VALUE SPACES.

       01  WRK-TAM-02-X.
           05  WRK-TAM-02-N            PIC  9(002)         VALUE ZEROS.

       01  WRK-TAM-04-X.
           05  WRK-TAM-04-N            PIC  9(004)         VALUE ZEROS.

       01  WRK-TAM-09-X.
           05  WRK-TAM-09-N            PIC  9(009)         VALUE ZEROS.

       01  WRK-TAM-02-COM-S            PIC -9(002)         VALUE ZEROS.
       01  FILLER REDEFINES WRK-TAM-02-COM-S.
           05  FILLER                  PIC  X(001).
           05  WRK-TAM-02-SEM-S        PIC  9(002).

       01  WRK-TAM-03-COM-S            PIC -9(003)         VALUE ZEROS.
       01  FILLER REDEFINES WRK-TAM-03-COM-S.
           05  FILLER                  PIC  X(001).
           05  WRK-TAM-03-SEM-S        PIC  9(003).

       01  WRK-TAM-05-COM-S            PIC -9(005)         VALUE ZEROS.
       01  FILLER REDEFINES WRK-TAM-05-COM-S.
           05  FILLER                  PIC  X(001).
           05  WRK-TAM-05-SEM-S        PIC  9(005).

       01  WRK-TAM-11-COM-S            PIC -9(011)         VALUE ZEROS.
       01  FILLER REDEFINES WRK-TAM-11-COM-S.
           05  FILLER                  PIC  X(001).
           05  WRK-TAM-11-SEM-S        PIC  9(011).

       01  WRK-TAM-13-COM-S            PIC -9(013)         VALUE ZEROS.
       01  FILLER REDEFINES WRK-TAM-13-COM-S.
           05  FILLER                  PIC  X(001).
           05  WRK-TAM-13-SEM-S        PIC  9(013).

      *---------------------------------------------------------------*
       01  FILLER  PIC X(32)  VALUE  '*   AREAS DE FILE STATUS       *'.
      *---------------------------------------------------------------*

       77  WRK-BATCH                   PIC X(08) VALUE 'BATCH   '.

       77  WRK-FS-ENTRADA              PIC X(02) VALUE SPACES.
       77  WRK-FS-ARQDATAS             PIC X(02) VALUE SPACES.
       77  WRK-FS-SAIDA                PIC X(02) VALUE SPACES.

       77  WRK-OPERACAO                PIC X(13) VALUE SPACES.
       77  WRK-ABERTURA                PIC X(13) VALUE 'NA ABERTURA'.
       77  WRK-LEITURA                 PIC X(13) VALUE 'NA LEITURA'.
       77  WRK-GRAVACAO                PIC X(13) VALUE 'NA GRAVACAO'.
       77  WRK-FECHAMENTO              PIC X(13) VALUE 'NO FECHAMENTO'.

      *---------------------------------------------------------------*
       77  FILLER  PIC X(32)  VALUE  '* ACUMULADORES                 *'.
      *---------------------------------------------------------------*

       77  ACU-LIDOS-ENTRADA           PIC 9(09) COMP-3    VALUE ZEROS.
       77  ACU-LIDOS-ARQDATAS          PIC 9(09) COMP-3    VALUE ZEROS.
       77  ACU-GRAVA-SAIDA             PIC 9(09) COMP-3    VALUE ZEROS.

      *---------------------------------------------------------------*
       01  FILLER  PIC X(32)  VALUE  '* MASCARAS DE EDICAO DE TOTAIS *'.
      *---------------------------------------------------------------*

       01  FILLER.
           05  WRK-MASK-LIDOS-ENTRADA  PIC ZZZ.ZZZ.ZZ9     VALUE ZEROS.
           05  WRK-MASK-LIDOS-ARQDATAS PIC ZZZ.ZZZ.ZZ9     VALUE ZEROS.
           05  WRK-MASK-GRAVA-SAIDA    PIC ZZZ.ZZZ.ZZ9     VALUE ZEROS.

      *---------------------------------------------------------------*
       01  FILLER    PIC X(32)   VALUE '*   AREAS ENTRADA           *'.
      *---------------------------------------------------------------*

       COPY 'GFCTWAL2'.

      *---------------------------------------------------------------*
       01  FILLER    PIC X(32)   VALUE '*   AREAS LAYOUT ARQDATAS   *'.
      *---------------------------------------------------------------*

       COPY 'I#GFCT06'.

      *---------------------------------------------------------------*
       01  FILLER    PIC X(32)   VALUE '*   AREAS LAYOUT SAIDA      *'.
      *---------------------------------------------------------------*

       COPY 'GFCTWAOU'.

      *---------------------------------------------------------------*
       01  FILLER    PIC X(32)   VALUE '*   AREAS TRATAMENTO ERRO    *'.
      *---------------------------------------------------------------*

       COPY 'I#BRAD7C'.

       01  WRK-TEXTO.
           05  FILLER                  PIC X(05) VALUE 'ERRO'.
           05  WRK-OPERACAO-TXT        PIC X(13) VALUE SPACES.
           05  FILLER                  PIC X(12) VALUE ' DO ARQUIVO '.
           05  WRK-ARQ-TXT             PIC X(08) VALUE SPACES.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
       01  FILLER  PIC X(32)  VALUE  '*  FIM  DA WORKING   GFCT4112  *'.
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
                           ARQDATAS
                   OUTPUT  SAIDA

           MOVE    WRK-ABERTURA        TO  WRK-OPERACAO.
           PERFORM 1000-TESTAR-FILE-STATUS.

           PERFORM 2000-ROTINA-INICIAL.

           IF  ACU-LIDOS-ENTRADA       EQUAL ZEROS OR
               ACU-LIDOS-ARQDATAS      EQUAL ZEROS OR
               WRK-NAO-ENCONTROU       EQUAL 'S'
               GO                      TO  0000-FINALIZAR
           END-IF.

           PERFORM 3000-PROCESSA
             UNTIL WRK-FS-ENTRADA  EQUAL '10'.

           PERFORM 4000-ROTINA-FINAL.


      *===============*
      * LABEL FINALIZAR
      *===============*
       0000-FINALIZAR.
      *---------------*

           CLOSE   ENTRADA
                   ARQDATAS
                   SAIDA.

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

           PERFORM 1200-TESTAR-FS-ARQDATAS.

           PERFORM 1300-TESTAR-FS-SAIDA.

      *---------------------------------------------------------------*
       1000-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA TESTAR-FS-ENTRADA
      *===============================================================*
       1100-TESTAR-FS-ENTRADA  SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-ENTRADA          NOT EQUAL '00'
               DISPLAY '************** GFCT4112 *************'
               DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
               DISPLAY '*              ENTRADA              *'
               DISPLAY '*         FILE STATUS =  ' WRK-FS-ENTRADA
                                                  '         *'
               DISPLAY '*         PROGRAMA ENCERRADO        *'
               DISPLAY '************** GFCT4112 *************'
               MOVE WRK-OPERACAO       TO  WRK-OPERACAO-TXT
               MOVE 'ENTRADA '         TO  WRK-ARQ-TXT
               MOVE WRK-TEXTO          TO  ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1100-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA TESTAR-FS-ARQDATAS
      *===============================================================*
       1200-TESTAR-FS-ARQDATAS   SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-ARQDATAS         NOT EQUAL '00'
               DISPLAY '************** GFCT4112 *************'
               DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
               DISPLAY '*              ARQDATAS             *'
               DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQDATAS
                                                  '         *'
               DISPLAY '*         PROGRAMA ENCERRADO        *'
               DISPLAY '************** GFCT4112 *************'
               MOVE WRK-OPERACAO       TO  WRK-OPERACAO-TXT
               MOVE 'ARQDATAS'         TO  WRK-ARQ-TXT
               MOVE WRK-TEXTO          TO  ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1200-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA TESTAR-FS-SAIDA
      *===============================================================*
       1300-TESTAR-FS-SAIDA SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-SAIDA            NOT EQUAL '00'
               DISPLAY '************** GFCT4112 *************'
               DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
               DISPLAY '*               SAIDA               *'
               DISPLAY '*         FILE STATUS =  ' WRK-FS-SAIDA
                                                  '         *'
               DISPLAY '*         PROGRAMA ENCERRADO        *'
               DISPLAY '************** GFCT4112 *************'
               MOVE WRK-OPERACAO       TO  WRK-OPERACAO-TXT
               MOVE 'SAIDA   '         TO  WRK-ARQ-TXT
               MOVE WRK-TEXTO          TO  ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1300-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA ROTINA-INICIAL
      *===============================================================*
       2000-ROTINA-INICIAL SECTION.
      *---------------------------------------------------------------*

           PERFORM 2100-LER-ENTRADA.

           IF  ACU-LIDOS-ENTRADA       EQUAL   ZEROS
               DISPLAY '************* GFCT4112 *************'
               DISPLAY '*                                  *'
               DISPLAY '*    ARQUIVO ENTRADA ESTA VAZIO    *'
               DISPLAY '*                                  *'
               DISPLAY '*        PROGRAMA ENCERRADO        *'
               DISPLAY '*                                  *'
               DISPLAY '************* GFCT4112 *************'
               GO                      TO  2000-99-FIM
           END-IF.

           PERFORM 2200-LER-ARQDATAS.

           IF  ACU-LIDOS-ARQDATAS      EQUAL   ZEROS
               DISPLAY '************* GFCT4112 *************'
               DISPLAY '*                                  *'
               DISPLAY '*   ARQUIVO ARQDATAS ESTA VAZIO    *'
               DISPLAY '*                                  *'
               DISPLAY '*        PROGRAMA ENCERRADO        *'
               DISPLAY '*                                  *'
               DISPLAY '************* GFCT4112 *************'
               GO                      TO  2000-99-FIM
           END-IF.

           PERFORM UNTIL GFCT06-CSIST-PRINC EQUAL 'GFCT' OR
                         WRK-FS-ARQDATAS    EQUAL '10'
               PERFORM 2200-LER-ARQDATAS
           END-PERFORM.

           IF  WRK-FS-ARQDATAS         EQUAL '10'
               DISPLAY '************* GFCT4112 *************'
               DISPLAY '*                                  *'
               DISPLAY '*  DATA PROC GFCT NAO LOCALIZADA   *'
               DISPLAY '*                                  *'
               DISPLAY '*        PROGRAMA ENCERRADO        *'
               DISPLAY '*                                  *'
               DISPLAY '************* GFCT4112 *************'
               MOVE 'S'                TO  WRK-NAO-ENCONTROU
           END-IF.

      *---------------------------------------------------------------*
       2000-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA LER-ENTRADA
      *===============================================================*
       2100-LER-ENTRADA  SECTION.
      *---------------------------------------------------------------*

           READ ENTRADA  INTO REG-GFCTWAL2.

           IF  WRK-FS-ENTRADA       EQUAL  '10'
               GO                      TO  2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO  WRK-OPERACAO.
           PERFORM 1100-TESTAR-FS-ENTRADA.

           ADD  1                      TO  ACU-LIDOS-ENTRADA.

      *---------------------------------------------------------------*
       2100-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA LER-ARQDATAS
      *===============================================================*
       2200-LER-ARQDATAS SECTION.
      *---------------------------------------------------------------*

           READ ARQDATAS INTO GFCT06-REG.

           IF  WRK-FS-ARQDATAS      EQUAL  '10'
               GO                      TO  2200-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO  WRK-OPERACAO.
           PERFORM 1200-TESTAR-FS-ARQDATAS.

           ADD  1                      TO  ACU-LIDOS-ARQDATAS.

      *---------------------------------------------------------------*
       2200-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA PROCESSA
      *===============================================================*
       3000-PROCESSA SECTION.
      *---------------------------------------------------------------*

           PERFORM 3100-GRAVAR-SAIDA

           PERFORM 2100-LER-ENTRADA.

      *---------------------------------------------------------------*
       3000-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA GRAVAR-SAIDA
      *===============================================================*
       3100-GRAVAR-SAIDA SECTION.
      *---------------------------------------------------------------*

ST25X6*    MOVE WAL2-RJUSTF-SOLTC-EXTRT(1:9)
ST25X6*                                    TO  WRK-TAM-09-X
ST25X6*    MOVE WRK-TAM-09-N               TO  WAOU-CCGC-CPF
ST25X6     MOVE WAL2-RJUSTF-SOLTC-EXTRT(1:9)
ST25X6                                     TO  WAOU-CCGC-CPF

ST25X6*    MOVE WAL2-RJUSTF-SOLTC-EXTRT(11:4)
ST25X6*                                    TO  WRK-TAM-04-X
ST25X6*    MOVE WRK-TAM-04-N               TO  WAOU-CFLIAL-CGC
ST25X6     MOVE WAL2-RJUSTF-SOLTC-EXTRT(11:4)
ST25X6                                     TO  WAOU-CFLIAL-CGC

           MOVE WAL2-RJUSTF-SOLTC-EXTRT(16:2)
                                           TO  WRK-TAM-02-X
           MOVE WRK-TAM-02-N               TO  WAOU-CCTRL-CPF-CGC

           MOVE GFCT06-DPROCM-ATUAL        TO  WAOU-DSOLTC-CNST

           MOVE WAL2-CBCO-DSTNO-MOVTO      TO  WRK-TAM-03-COM-S
           MOVE WRK-TAM-03-SEM-S           TO  WAOU-BANCO

           MOVE WAL2-CAG-DSTNO-MOVTO       TO  WRK-TAM-05-COM-S
           MOVE WRK-TAM-05-SEM-S           TO  WAOU-AGE-DEB

           MOVE WAL2-CCTA-DSTNO-MOVTO      TO  WRK-TAM-13-COM-S
           MOVE WRK-TAM-13-SEM-S           TO  WAOU-CTA-DEB

ST25X6*    MOVE ZEROS                      TO  WAOU-CPF-CGC-NUM
ST25X6*    MOVE ZEROS                      TO  WAOU-CPF-CGC-FILIAL
ST25X6     MOVE SPACES                     TO  WAOU-CPF-CGC-NUM
ST25X6     MOVE SPACES                     TO  WAOU-CPF-CGC-FILIAL
           MOVE ZEROS                      TO  WAOU-CPF-CTR
           MOVE SPACES                     TO  WAOU-DTA-CONSULTA
           MOVE SPACES                     TO  WAOU-RES-CONSULTA
           MOVE ZEROS                      TO  WAOU-TP-IMPED
           MOVE SPACES                     TO  WAOU-RS-IMPED
           MOVE WAL2-CROTNA-ORIGE-MOVTO    TO  WAOU-CROTNA-ORIGE-MOVTO
           MOVE WAL2-DENVIO-MOVTO-TARIF    TO  WAOU-DENVIO-MOVTO-TARIF

           MOVE WAL2-CNRO-ARQ-MOVTO        TO  WRK-TAM-02-COM-S
           MOVE WRK-TAM-02-SEM-S           TO  WAOU-CNRO-ARQ-MOVTO

           MOVE WAL2-CSEQ-MOVTO            TO  WRK-TAM-11-COM-S
           MOVE WRK-TAM-11-SEM-S           TO  WAOU-CSEQ-MOVTO

           MOVE WAL2-CSERVC-TARIF          TO  WRK-TAM-05-COM-S
           MOVE WRK-TAM-05-SEM-S           TO  WAOU-CSERVC-TARIF

           MOVE WRK-GRAVACAO               TO  WRK-OPERACAO.

           WRITE FD-REG-SAIDA FROM GFCTWAOU-REG
           PERFORM 1300-TESTAR-FS-SAIDA.

           ADD 1                           TO  ACU-GRAVA-SAIDA.

      *---------------------------------------------------------------*
       3100-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA FINAL
      *===============================================================*
       4000-ROTINA-FINAL   SECTION.
      *---------------------------------------------------------------*

      *--> MOSTRAR TOTALIZACAO

           MOVE ACU-LIDOS-ENTRADA      TO  WRK-MASK-LIDOS-ENTRADA .
           MOVE ACU-LIDOS-ARQDATAS     TO  WRK-MASK-LIDOS-ARQDATAS.
           MOVE ACU-GRAVA-SAIDA        TO  WRK-MASK-GRAVA-SAIDA   .

           DISPLAY '*********************** GFCT4112 *******************
      -                                                        '****'.
           DISPLAY '*
      -                                                        '   *'.
           DISPLAY '* QTDE DE REGISTROS LIDOS   ENTRADA..... : '
                                       WRK-MASK-LIDOS-ENTRADA  ' *'.
           DISPLAY '* QTDE DE REGISTROS LIDOS   ARQDATAS.... : '
                                       WRK-MASK-LIDOS-ARQDATAS ' *'.
           DISPLAY '* QTDE DE REGISTROS GRAVADOS SAIDA...... : '
                                       WRK-MASK-GRAVA-SAIDA    ' *'.
           DISPLAY '*
      -                                                        '   *'.
           DISPLAY '*********************** GFCT4112 *******************
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
           MOVE 'GFCT4112'             TO  ERR-PGM

           CALL 'BRAD7100'          USING  WRK-BATCH
                                           ERRO-AREA.

           GOBACK.

      *---------------------------------------------------------------*
       9999-99-FIM. EXIT.
      *---------------------------------------------------------------*
