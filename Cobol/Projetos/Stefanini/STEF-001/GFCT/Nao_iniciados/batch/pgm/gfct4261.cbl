      *===============================================================*
       IDENTIFICATION DIVISION.
      *===============================================================*

       PROGRAM-ID. GFCT4261.

      *================================================================*
      *                       S O N D A   I T                          *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA.........: GFCT4261                                 *
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
      *    OBJETIVO.........: SELECAO COBRANCA ACUMULADA - DETALHE     *
      *                       UNIFICADO.                               *
      *                                                                *
      *    ARQUIVOS:                                                   *
      *      DDNAME                            INCLUDE/BOOK            *
      *        CONSOLID                          I#GFCTIL              *
      *        DETALHAD                          I#GFCTIL              *
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

           SELECT CONSOLID ASSIGN TO UT-S-CONSOLID
                      FILE STATUS IS WRK-FS-CONSOLID.

           SELECT DETALHAD ASSIGN TO UT-S-DETALHAD
                      FILE STATUS IS WRK-FS-DETALHAD.

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
       WORKING-STORAGE SECTION.
      *---------------------------------------------------------------*

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
       77  FILLER  PIC X(32) VALUE '*  INICIO DA WORKING GFCT4261  *'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*

      *---------------------------------------------------------------*
       77  FILLER  PIC X(32) VALUE '*  AREAS AUXILIARES            *'.
      *---------------------------------------------------------------*

       77  WRK-INCONSIS                PIC  X(001)         VALUE SPACES.
       77  WRK-TIPO-COBRANCA           PIC  9(002)         VALUE ZEROS.

       01  WRK-TAM-11V2-COMP           PIC 9(11)V99 COMP-3 VALUE ZEROS.

       01  WRK-MENSA-01.
           05  FILLER                  PIC  X(025)         VALUE
               'TIPO COBRANCA INVALIDA - '.
           05  WRK-MENSA-01-TIPO       PIC  9(002)         VALUE ZEROS.

       01  WRK-DISPLAYS.
           05  WRK-DISPL-01            PIC  X(070)         VALUE
               '************************** GFCT4261 ********************
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

      *---------------------------------------------------------------*
       77  FILLER  PIC X(32) VALUE '*  AREAS DE FILE STATUS        *'.
      *---------------------------------------------------------------*

       77  WRK-BATCH                   PIC X(08) VALUE 'BATCH   '.

       77  WRK-FS-CONSOLID             PIC X(02) VALUE SPACES.
       77  WRK-FS-DETALHAD             PIC X(02) VALUE SPACES.
       77  WRK-FS-OCORRENC             PIC X(02) VALUE SPACES.

       77  WRK-OPERACAO                PIC X(13) VALUE SPACES.
       77  WRK-ABERTURA                PIC X(13) VALUE 'NA ABERTURA'.
       77  WRK-LEITURA                 PIC X(13) VALUE 'NA LEITURA'.
       77  WRK-GRAVACAO                PIC X(13) VALUE 'NA GRAVACAO'.
       77  WRK-FECHAMENTO              PIC X(13) VALUE 'NO FECHAMENTO'.

      *---------------------------------------------------------------*
       77  FILLER  PIC X(32) VALUE '*  ACUMULADORES                *'.
      *---------------------------------------------------------------*

       77  ACU-LIDOS-CONSOLID          PIC 9(010) COMP-3   VALUE ZEROS.
       77  ACU-LIDOS-DETALHAD          PIC 9(010) COMP-3   VALUE ZEROS.
       77  ACU-GRAVA-OCORRENC          PIC 9(010) COMP-3   VALUE ZEROS.
       77  ACU-LIDOS-DET-TIPO          PIC 9(010) COMP-3   VALUE ZEROS.

       77  ACU-TOT-VALOR-DEBITAR       PIC 9(011)V99 COMP-3 VALUE ZEROS.

       77  ACU-DBTAR-CONSOLID          PIC 9(011)V99 COMP-3 VALUE ZEROS.
       77  ACU-DBTDO-CONSOLID          PIC 9(011)V99 COMP-3 VALUE ZEROS.
       77  ACU-DBTAR-DETALHAD          PIC 9(011)V99 COMP-3 VALUE ZEROS.
       77  ACU-DBTDO-DETALHAD          PIC 9(011)V99 COMP-3 VALUE ZEROS.
       77  ACU-DBTAR-OCORRENC          PIC 9(011)V99 COMP-3 VALUE ZEROS.
       77  ACU-DBTDO-OCORRENC          PIC 9(011)V99 COMP-3 VALUE ZEROS.
       77  ACU-DBTAR-DET-TIPO          PIC 9(011)V99 COMP-3 VALUE ZEROS.
       77  ACU-DBTDO-DET-TIPO          PIC 9(011)V99 COMP-3 VALUE ZEROS.

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
       01  FILLER  PIC X(32)  VALUE  '*  AREAS BRAD0560             *'.
      *---------------------------------------------------------------*

       01  WRK-P0560-JOBNAME           PIC X(008)          VALUE SPACES.
       01  WRK-P0560-JOBNUMBER         PIC 9(005)          VALUE ZEROS.

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
       01  FILLER  PIC X(32) VALUE '*  FIM  DA WORKING   GFCT4261  *'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*

      *----------------------------------------------------------------*
       LINKAGE SECTION.
      *----------------------------------------------------------------*

       01  LNK-PARM.
           05  FILLER                  PIC S9(004) COMP    VALUE ZEROS.
           05  LNK-TIPO                PIC  9(002)         VALUE ZEROS.

      *===============================================================*
       PROCEDURE DIVISION USING LNK-PARM.
      *===============================================================*

      *===============================================================*
      *    ROTINA INICIAR
      *===============================================================*
       0000-INICIAR SECTION.
      *---------------------------------------------------------------*

           OPEN    INPUT   CONSOLID
                           DETALHAD
                   OUTPUT  OCORRENC.

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

      *---------------------------------------------------------------*
       1000-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA TESTAR-FS-CONSOLID
      *===============================================================*
       1100-TESTAR-FS-CONSOLID SECTION.
      *---------------------------------------------------------------*

           IF WRK-FS-CONSOLID NOT EQUAL '00'
              DISPLAY '************** GFCT4261 *************'
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
              DISPLAY '*              CONSOLID             *'
              DISPLAY '*         FILE STATUS =  ' WRK-FS-CONSOLID
                                                 '         *'
              DISPLAY '*        PROGRAMA  ENCERRADO        *'
              DISPLAY '************** GFCT4261 *************'
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
              DISPLAY '************** GFCT4261 *************'
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
              DISPLAY '*              DETALHAD             *'
              DISPLAY '*         FILE STATUS =  ' WRK-FS-DETALHAD
                                                 '         *'
              DISPLAY '*        PROGRAMA  ENCERRADO        *'
              DISPLAY '************** GFCT4261 *************'
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
              DISPLAY '************** GFCT4261 *************'
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
              DISPLAY '*              OCORRENC             *'
              DISPLAY '*         FILE STATUS =  ' WRK-FS-OCORRENC
                                                 '         *'
              DISPLAY '*        PROGRAMA  ENCERRADO        *'
              DISPLAY '************** GFCT4261 *************'
              MOVE WRK-OPERACAO        TO  WRK-OPERACAO-TXT
              MOVE 'OCORRENC'          TO  WRK-ARQ-TXT
              MOVE WRK-TEXTO           TO  ERR-TEXTO
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

           PERFORM 2300-CONSISTIR-PARM.

           PERFORM 2100-LER-CONSOLID.

           IF  ACU-LIDOS-CONSOLID      EQUAL   ZEROS
               DISPLAY '************** GFCT4261 *************'
               DISPLAY '*                                   *'
               DISPLAY '*    ARQUIVO CONSOLID ESTA VAZIO    *'
               DISPLAY '*                                   *'
               DISPLAY '************** GFCT4261 *************'
           END-IF.

           PERFORM 2200-LER-DETALHAD.

           IF  ACU-LIDOS-DETALHAD      EQUAL   ZEROS
               DISPLAY '************** GFCT4261 *************'
               DISPLAY '*                                   *'
               DISPLAY '*    ARQUIVO DETALHAD ESTA VAZIO    *'
               DISPLAY '*                                   *'
               DISPLAY '************** GFCT4261 *************'
           END-IF.


           CALL 'BRAD0560'             USING WRK-P0560-JOBNAME
                                             WRK-P0560-JOBNUMBER.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE SPACES             TO  WRK-P0560-JOBNAME
           END-IF.

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

      *--> DESPREZAR OS REGISTROS QUE POSSUEM TIPO DIFERENTE DO PARM
           IF  COBR-TIPO-COBRANCA      OF CONSOLID
                                       NOT EQUAL LNK-TIPO
               GO                      TO  2100-LER-CONSOLID
           END-IF


           ADD 1                       TO  ACU-LIDOS-CONSOLID

           MOVE COBR-VALOR-DEBITAR     OF  CONSOLID
                                       TO  WRK-TAM-11V2-COMP
           ADD WRK-TAM-11V2-COMP       TO  ACU-DBTAR-CONSOLID

           MOVE COBR-VALOR-DEBITADO    OF  CONSOLID
                                       TO  WRK-TAM-11V2-COMP
           ADD WRK-TAM-11V2-COMP       TO  ACU-DBTDO-CONSOLID


           MOVE COBR-DIG-CONTRATO      OF  CONSOLID
                                       TO  WRK-CHV-DIG-CONS
           MOVE COBR-NUM-CONTRATO      OF  CONSOLID
                                       TO  WRK-CHV-NUM-CONS.

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

           MOVE COBR-VALOR-DEBITAR     OF  DETALHAD
                                       TO  WRK-TAM-11V2-COMP
           ADD WRK-TAM-11V2-COMP       TO  ACU-DBTAR-DETALHAD

           MOVE COBR-VALOR-DEBITADO    OF  DETALHAD
                                       TO  WRK-TAM-11V2-COMP
           ADD WRK-TAM-11V2-COMP       TO  ACU-DBTDO-DETALHAD


           MOVE COBR-DIG-CONTRATO      OF  DETALHAD
                                       TO  WRK-CHV-DIG-DETA
           MOVE COBR-NUM-CONTRATO      OF  DETALHAD
                                       TO  WRK-CHV-NUM-DETA.

      *---------------------------------------------------------------*
       2200-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA CONSISTIR-PARM
      *===============================================================*
       2300-CONSISTIR-PARM SECTION.
      *---------------------------------------------------------------*

           IF  LNK-TIPO                NOT NUMERIC OR
              (LNK-TIPO                NOT EQUAL 2 AND 3 AND 7 AND 9)
               DISPLAY '************** GFCT4261 *************'
               DISPLAY '*                                   *'
               DISPLAY '*       PARAMETRO INVALIDO: ' LNK-TIPO
                                                     '      *'
               DISPLAY '*    SERA ASSUMIDO PARAMETRO = 02   *'
               DISPLAY '*                                   *'
               DISPLAY '************** 4FCT4261 *************'
               MOVE 2                  TO  LNK-TIPO
           ELSE
               DISPLAY '************** GFCT4261 *************'
               DISPLAY '*                                   *'
               DISPLAY '*  PARAMETRO RECEBIDO VIA JCL: ' LNK-TIPO
                                                        '   *'
               DISPLAY '*                                   *'
               DISPLAY '************** GFCT4261 *************'
           END-IF.

      *---------------------------------------------------------------*
       2300-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA PROCESSA
      *===============================================================*
       3000-PROCESSA SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-DETALHAD         EQUAL '00'
               PERFORM 3300-CONSISTIR-DETALHAD
               IF  WRK-INCONSIS        EQUAL 'S'
                   GO                  TO  3000-99-FIM
               END-IF
           END-IF.


      *--> VALIDAR SE AS CHAVES POSSUEM CORRESPONDENTES

           IF  WRK-CHAVE-CONSOLID      EQUAL WRK-CHAVE-DETALHAD

               MOVE WRK-CHAVE-DETALHAD TO  WRK-CHAVE-DETALHAD-ANT
               MOVE ZEROS              TO  ACU-TOT-VALOR-DEBITAR

               PERFORM UNTIL WRK-FS-DETALHAD EQUAL '10'
                 OR WRK-CHAVE-DETALHAD NOT EQUAL WRK-CHAVE-DETALHAD-ANT

                   MOVE COBR-VALOR-DEBITAR OF DETALHAD
                                       TO  WRK-TAM-11V2-COMP
                   COMPUTE ACU-TOT-VALOR-DEBITAR
                                       =   ACU-TOT-VALOR-DEBITAR +
                                           WRK-TAM-11V2-COMP

                   ADD 1                       TO  ACU-LIDOS-DET-TIPO
                   MOVE COBR-VALOR-DEBITAR     OF  DETALHAD
                                               TO  WRK-TAM-11V2-COMP
                   ADD WRK-TAM-11V2-COMP       TO  ACU-DBTAR-DET-TIPO

                   MOVE COBR-VALOR-DEBITADO    OF  DETALHAD
                                               TO  WRK-TAM-11V2-COMP
                   ADD WRK-TAM-11V2-COMP       TO  ACU-DBTDO-DET-TIPO

                   PERFORM 2200-LER-DETALHAD
               END-PERFORM

               IF  ACU-TOT-VALOR-DEBITAR NOT EQUAL
                                       COBR-VALOR-DEBITAR OF CONSOLID
                   MOVE 'TOTAIS DETALHADO E CONSOLIDADO NAO BATEM'
                                       TO  BB4-DESCRICAO
                   PERFORM 3200-GRAVAR-OCORRENC-CONS
               END-IF

               PERFORM 2100-LER-CONSOLID

           ELSE
               IF  WRK-CHAVE-CONSOLID LESS WRK-CHAVE-DETALHAD

                   MOVE 'REGISTRO DE CONSOLID NAO ENCONTRADO EM DETALHE'
                                       TO  BB4-DESCRICAO
                   PERFORM 3200-GRAVAR-OCORRENC-CONS
                   PERFORM 2100-LER-CONSOLID
               ELSE
                   MOVE 'REGISTRO DE DETALHE NAO ENCONTRADO EM CONSOLID'
                                       TO  BB4-DESCRICAO
                   PERFORM 3100-GRAVAR-OCORRENC-DETA

                   ADD 1                       TO  ACU-LIDOS-DET-TIPO
                   MOVE COBR-VALOR-DEBITAR     OF  DETALHAD
                                               TO  WRK-TAM-11V2-COMP
                   ADD WRK-TAM-11V2-COMP       TO  ACU-DBTAR-DET-TIPO

                   MOVE COBR-VALOR-DEBITADO    OF  DETALHAD
                                               TO  WRK-TAM-11V2-COMP
                   ADD WRK-TAM-11V2-COMP       TO  ACU-DBTDO-DET-TIPO

                   PERFORM 2200-LER-DETALHAD
               END-IF
           END-IF.

      *---------------------------------------------------------------*
       3000-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA GRAVAR-OCORRENC-DETA
      *===============================================================*
       3100-GRAVAR-OCORRENC-DETA  SECTION.
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
           MOVE 'GFCT4260'             TO  BB4-PROGRAMA
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

220512     WRITE FD-REG-OCORRENC       FROM BB4-REG
           PERFORM 1300-TESTAR-FS-OCORRENC.

           ADD 1                       TO  ACU-GRAVA-OCORRENC.

           MOVE COBR-VALOR-DEBITAR     OF  DETALHAD
                                       TO  WRK-TAM-11V2-COMP
           ADD WRK-TAM-11V2-COMP       TO  ACU-DBTAR-OCORRENC

           MOVE COBR-VALOR-DEBITADO    OF  DETALHAD
                                       TO  WRK-TAM-11V2-COMP
           ADD WRK-TAM-11V2-COMP       TO  ACU-DBTDO-OCORRENC.

      *---------------------------------------------------------------*
       3100-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA GRAVAR-OCORRENC-CONS
      *===============================================================*
       3200-GRAVAR-OCORRENC-CONS  SECTION.
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
           MOVE 'GFCT4260'             TO  BB4-PROGRAMA
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

220512     WRITE FD-REG-OCORRENC       FROM BB4-REG
           PERFORM 1300-TESTAR-FS-OCORRENC.

           ADD 1                       TO  ACU-GRAVA-OCORRENC.

           MOVE COBR-VALOR-DEBITAR     OF  CONSOLID
                                       TO  WRK-TAM-11V2-COMP
           ADD WRK-TAM-11V2-COMP       TO  ACU-DBTAR-OCORRENC

           MOVE COBR-VALOR-DEBITADO    OF  CONSOLID
                                       TO  WRK-TAM-11V2-COMP
           ADD WRK-TAM-11V2-COMP       TO  ACU-DBTDO-OCORRENC.

      *---------------------------------------------------------------*
       3200-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA CONSISTIR-DETALHAD
      *===============================================================*
       3300-CONSISTIR-DETALHAD SECTION.
      *---------------------------------------------------------------*

           MOVE SPACES                 TO  WRK-INCONSIS

      *--> CONSISTIR O DIG E NRO DO CONTRATO

           IF  COBR-DIG-CONTRATO OF DETALHAD
                                       NOT ALPHABETIC OR
               COBR-DIG-CONTRATO OF DETALHAD
                                       EQUAL SPACES   OR
               COBR-NUM-CONTRATO OF DETALHAD
                                       NOT NUMERIC
               MOVE 'CHAVE COBRANCA INVALIDA'
                                       TO  BB4-DESCRICAO
               PERFORM 3100-GRAVAR-OCORRENC-DETA
               PERFORM 2200-LER-DETALHAD

               MOVE 'S'                TO  WRK-INCONSIS
               GO                      TO  3300-99-FIM
           END-IF.

      *--> CONSISTIR O TIPO DO CONTRATO NO ARQ. DE DETALHE

           EVALUATE LNK-TIPO
             WHEN 09
               IF  COBR-TIPO-COBRANCA  OF DETALHAD NOT EQUAL 10
                   MOVE 'S'            TO  WRK-INCONSIS
               END-IF

             WHEN 02
               IF  COBR-TIPO-COBRANCA  OF DETALHAD NOT EQUAL 04
                   MOVE 'S'            TO  WRK-INCONSIS
               END-IF

             WHEN 03
               IF  COBR-TIPO-COBRANCA  OF DETALHAD NOT EQUAL 05
                   MOVE 'S'            TO  WRK-INCONSIS
               END-IF

             WHEN 07
               IF  COBR-TIPO-COBRANCA  OF DETALHAD NOT EQUAL 08
                   MOVE 'S'            TO  WRK-INCONSIS
               END-IF
           END-EVALUATE.

           IF  WRK-INCONSIS            EQUAL 'S'
               MOVE COBR-TIPO-COBRANCA OF DETALHAD
                                       TO  WRK-MENSA-01-TIPO
               MOVE WRK-MENSA-01       TO  BB4-DESCRICAO
               PERFORM 3100-GRAVAR-OCORRENC-DETA
               PERFORM 2200-LER-DETALHAD
               GO                      TO  3300-99-FIM
           END-IF.

           MOVE COBR-TIPO-COBRANCA     OF DETALHAD
                                       TO  WRK-TIPO-COBRANCA.

      *---------------------------------------------------------------*
       3300-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA FINAL
      *===============================================================*
       4000-ROTINA-FINAL   SECTION.
      *---------------------------------------------------------------*

      *--> MOSTRAR TOTALIZACAO

           DISPLAY WRK-DISPL-01
           DISPLAY WRK-DISPL-02
           DISPLAY WRK-DISPL-03
           DISPLAY WRK-DISPL-04

           MOVE 'DETALHAD'             TO  WRK-DISPL-05-LITER
           MOVE ACU-LIDOS-DETALHAD     TO  WRK-DISPL-05-REG
           MOVE ACU-DBTAR-DETALHAD     TO  WRK-DISPL-05-DBTAR
           MOVE ACU-DBTDO-DETALHAD     TO  WRK-DISPL-05-DBTDO
           DISPLAY WRK-DISPL-05

           MOVE 'CONSOLID'             TO  WRK-DISPL-05-LITER
           MOVE ACU-LIDOS-CONSOLID     TO  WRK-DISPL-05-REG
           MOVE ACU-DBTAR-CONSOLID     TO  WRK-DISPL-05-DBTAR
           MOVE ACU-DBTDO-CONSOLID     TO  WRK-DISPL-05-DBTDO
           DISPLAY WRK-DISPL-05

           MOVE 'OCORRENC'             TO  WRK-DISPL-05-LITER
           MOVE ACU-GRAVA-OCORRENC     TO  WRK-DISPL-05-REG
           MOVE ACU-DBTAR-OCORRENC     TO  WRK-DISPL-05-DBTAR
           MOVE ACU-DBTDO-OCORRENC     TO  WRK-DISPL-05-DBTDO
           DISPLAY WRK-DISPL-05

           MOVE 'DET'                  TO  WRK-DISPL-05-LIT-1
           MOVE WRK-TIPO-COBRANCA      TO  WRK-DISPL-05-LIT-2
           MOVE ACU-LIDOS-DET-TIPO     TO  WRK-DISPL-05-REG
           MOVE ACU-DBTAR-DET-TIPO     TO  WRK-DISPL-05-DBTAR
           MOVE ACU-DBTDO-DET-TIPO     TO  WRK-DISPL-05-DBTDO
           DISPLAY WRK-DISPL-05

           MOVE 'CONS'                 TO  WRK-DISPL-05-LIT-1
           MOVE LNK-TIPO               TO  WRK-DISPL-05-LIT-2
           MOVE ACU-LIDOS-CONSOLID     TO  WRK-DISPL-05-REG
           MOVE ACU-DBTAR-CONSOLID     TO  WRK-DISPL-05-DBTAR
           MOVE ACU-DBTDO-CONSOLID     TO  WRK-DISPL-05-DBTDO
           DISPLAY WRK-DISPL-05

           DISPLAY WRK-DISPL-01.

      *---------------------------------------------------------------*
       4000-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA ERRO
      *===============================================================*
       9999-ROTINA-ERRO SECTION.
      *---------------------------------------------------------------*

           MOVE 'APL'                  TO  ERR-TIPO-ACESSO
           MOVE 'GFCT4261'             TO  ERR-PGM

           CALL 'BRAD7100'          USING  WRK-BATCH
                                           ERRO-AREA.

           GOBACK.

      *---------------------------------------------------------------*
       9999-99-FIM. EXIT.
      *---------------------------------------------------------------*
