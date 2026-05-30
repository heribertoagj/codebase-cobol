      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. CMRU248C.
       AUTHOR.     RENATA AMORIM LEGARIO.

      *================================================================*
      *                            B R Q                               *
      *----------------------------------------------------------------*
      *    PROGRAMA....:  CMRU248C                                     *
      *    ANALISTA....:  RENATA AMORIM LEGARIO - BRQ                  *
      *    DATA........:  MAIO/2019                                    *
      *----------------------------------------------------------------*
      *    OBJETIVO....:  ATUALIZA A TABELA RUFIB002 COM OS DADOS      *
      *                   DA DAP                                       *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME           I/O           INCLUDE/BOOK     *
      *                CADRURAL          I              I#RURAPC       *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    POOL7100 - MODULO PARA TRATAMENTO DE ERROS                  *
      *    BRAD7600 - MODULO PRA OBTER DATA/HORA DO SISTEMA            *
      *    CKRS0100 - MODULO PARA INICIAR E FINALIZAR CONEXAO DB2      *
      *    CKRS1000 - MODULO PARA INDICAR PROCESSAMWENTO               *
      *----------------------------------------------------------------*
      *                                                                *
WP1021*================================================================*
.     *  ANALISTA......:   WIPRO                                       *
.     *  DATA..........:   OUTUBRO/2021                                *
.     *----------------------------------------------------------------*
.     *  OBJETIVO......:   ADEQUACOES SICOR 5.03                       *
.     *                    FORMATACAO DOS CAMPOS                       *
WP1021*================================================================*

      *================================================================*
B0923 * ANALISTA REPONSAVEL: BELGA JUNIOR - FOURSYS                    *
B0923 * DATA...............: SETEMBRO - 2023                           *
B0923 * OBJETIVOS..........: RESOLUCAO 5.078 - REDUTOR DE TAXA - CAR   *
      *================================================================*
      *================================================================*
WIPR01* ANALISTA REPONSAVEL: CATIA MARQUES - WIPRO                     *
WIPR01* DATA...............: MARCO         - 2025                      *
WIPR01* OBJETIVO...........: ATENDER INC0893116                        *
WIPR01*                      TRATAR ORIGEM DE RECURSO = 47, PASSANDO A *
WIPR01*                      GRAVAR NUMERO DE DAP                      *
      *================================================================*
      *================================================================*
DPS01 * ANALISTA REPONSAVEL: DESIREE PESSOA                            *
.     * DATA...............: SETEMBRO      - 2025                      *
.     * OBJETIVO...........: ATENDER INC1164092                        *
.     *                      TRATAR ORIGEM DE RECURSO = 43, PASSANDO A *
DPS01 *                      GRAVAR NUMERO DE DAP                      *
      *================================================================*

      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*

       FILE-CONTROL.

           SELECT CADRURAL ASSIGN      TO UT-S-CADRURAL
                      FILE STATUS      IS WRK-FS-CADRURAL.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  INPUT:    ARQUIVO COM DADOS DO CONTRATO TOMBADO               *
      *----------------------------------------------------------------*

       FD  CADRURAL
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#RURAPC'.

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       77 FILLER                       PIC  X(032)         VALUE
          '* INICIO WORKING-STORAGE *'.
      *----------------------------------------------------------------*

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01 FILLER                       PIC  X(032)         VALUE
          '* AREA AUXILIAR *'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       77  WRK-MASCARA                 PIC  ZZZZZZZ.ZZZ.ZZ9 VALUE ' '.
       77  WRK-RELAC                   PIC  X(008)         VALUE SPACES.
       77  WRK-BATCH                   PIC  X(008)          VALUE
           'BATCH'.

       01  WRK-HORA.
           05 WRK-HH                   PIC  9(002)         VALUE ZEROS.
           05 FILLER                   PIC  X(001)         VALUE ':'.
           05 WRK-MM                   PIC  9(002)         VALUE ZEROS.
           05 FILLER                   PIC  X(001)         VALUE ':'.
           05 WRK-SS                   PIC  9(002)         VALUE ZEROS.

       01  WRK-DATADMA.
           05 WRK-DD-DMA               PIC  9(002)         VALUE ZEROS.
           05 FILLER                   PIC  X(001)         VALUE '/'.
           05 WRK-MM-DMA               PIC  9(002)         VALUE ZEROS.
           05 FILLER                   PIC  X(001)         VALUE '/'.
           05 WRK-SA-DMA               PIC  9(004)         VALUE ZEROS.

       01  WRK-DATAAMD.
           05 FILLER                   PIC  X(001)         VALUE SPACES.
           05 WRK-SA-AMD               PIC  9(004)         VALUE ZEROS.
           05 WRK-MM-AMD               PIC  9(002)         VALUE ZEROS.
           05 WRK-DD-AMD               PIC  9(002)         VALUE ZEROS.
       01  WRK-DATAAMD-R        REDEFINES WRK-DATAAMD
                                       PIC  9(009).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01 FILLER                       PIC  X(032)         VALUE
          '* AREA DE TESTE DE FILE-STATUS *'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01 WRK-FS-CADRURAL              PIC  X(002)         VALUE SPACES.

       01  WRK-ABERTURA                PIC  X(013)         VALUE
           ' NA ABERTURA '.
       01  WRK-LEITURA                 PIC  X(013)         VALUE
           ' NA LEITURA  '.
       01  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01 FILLER                       PIC  X(032)         VALUE
          '* AREA DE ACUMULADORES *'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01 WRK-ACUMULADORES.
          05 ACU-LIDOS-CADRURAL        PIC  9(009) COMP-3  VALUE ZEROS.
          05 ACU-ATUALIZADOS           PIC  9(009) COMP-3  VALUE ZEROS.
          05 ACU-ATUALIZADOS-TB50      PIC  9(009) COMP-3  VALUE ZEROS.
          05 ACU-ATUALIZADOS-TB70      PIC  9(009) COMP-3  VALUE ZEROS.
          05 ACU-ATUALIZADOS-TB71      PIC  9(009) COMP-3  VALUE ZEROS.
          05 ACU-ATUALIZADOS-TB72      PIC  9(009) COMP-3  VALUE ZEROS.
B0923     05 ACU-ATUALIZADOS-TB20      PIC  9(009) COMP-3  VALUE ZEROS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01 FILLER                       PIC  X(032)         VALUE
          '* AREA DE CAMPOS NULOS *'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01 WRK-CRURB0MB-NULL.
           05 WRK-MB-CDECLR-NULL        PIC S9(004) COMP    VALUE ZEROS.
           05 WRK-MB-DVALDD-NULL        PIC S9(004) COMP    VALUE ZEROS.
           05 WRK-MB-CGRP-NULL          PIC S9(004) COMP    VALUE ZEROS.

WP1021 01  WRK-CCETFC-ORGNC-EMPTO-NULL  PIC S9(04)  COMP    VALUE ZEROS.
WP1021 01  WRK-EURL-CETFC-ORGNC-NULL    PIC S9(04)  COMP    VALUE ZEROS.
WP1021 01  WRK-CRSTRB-EMPTO-RURAL-NULL  PIC S9(04)  COMP    VALUE ZEROS.
WP1021 01  WRK-EURL-RSTRB-EMPTO-NULL    PIC S9(04)  COMP    VALUE ZEROS.
WP1021 01  WRK-PENERG-RENOV-EMPTO-NULL  PIC S9(04)  COMP    VALUE ZEROS.

WP1021 01  WRK-CTPO-SOLO-AGCLT-NULL     PIC S9(04)  COMP    VALUE ZEROS.
WP1021 01  WRK-CCICLO-CTVDA-NULL        PIC S9(04)  COMP    VALUE ZEROS.
WP1021 01  WRK-CMANUT-SOLO-NULL         PIC S9(04)  COMP    VALUE ZEROS.

B0923  01  WRK-PREDC-JURO-AMBTL-NULL    PIC S9(04)  COMP    VALUE ZEROS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                      PIC  X(32) VALUE
           '* COPYBOOK MODULO CMRU2AFT     *'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01  WRK-CMRU2AFT                PIC  X(008) VALUE 'CMRU2AFT'.

       01  WRK-AREA-CMRU2AFT.
           COPY 'CMRUWAFA'.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      ***  AREA  PARA  ACESSAR   DATA   DA   POOL7600
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01  WRK-DATA-HORA.
           03  WRK-DT-JULIANA          PIC 9(05)  COMP-3 VALUE ZEROS.
           03  WRK-DT-AAMMDD           PIC 9(07)  COMP-3 VALUE ZEROS.
           03  WRK-DT-AAAAMMDD         PIC 9(09)  COMP-3 VALUE ZEROS.
           03  WRK-TI-HHMMSS           PIC 9(07)  COMP-3 VALUE ZEROS.
           03  WRK-TI-HHMMSSMMMMMM     PIC 9(13)  COMP-3 VALUE ZEROS.
           03  WRK-TIMESTAMP.
               05  WRK-7600-ANO          PIC  9(04)        VALUE ZEROS.
               05  WRK-7600-MES          PIC  9(02)        VALUE ZEROS.
               05  WRK-7600-DIA          PIC  9(02)        VALUE ZEROS.
               05  WRK-7600-HORA         PIC  9(02)        VALUE ZEROS.
               05  WRK-7600-MINUTOS      PIC  9(02)        VALUE ZEROS.
               05  WRK-7600-SEGUNDOS     PIC  9(02)        VALUE ZEROS.
               05  WRK-7600-MICROSEG     PIC  9(06)        VALUE ZEROS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01 FILLER                       PIC  X(032)         VALUE
          '* AREA DE CHECKPOINT / RESTART *'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01 WRK-CKRS0100                 PIC  X(008)         VALUE
          'CKRS0100'.

          COPY 'I#CKRS01'.

       01  WRK-AREA-RESTART.
           05  WRK-REST-CADRURAL       PIC  9(009) COMP-3  VALUE ZEROS.
           05  WRK-REST-RUFIB002       PIC  9(009) COMP-3  VALUE ZEROS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ERRO - FILE-STATUS ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  WRK-ERRO-POOL7100.
           03  FILLER                  PIC  X(008)         VALUE
               '** ERRO '.
           03  WRK-OPERACAO            PIC  X(013)         VALUE SPACES.
           03  FILLER                  PIC  X(012)         VALUE
               ' DO ARQUIVO '.
           03  WRK-NOME-ARQUIVO        PIC  X(008)         VALUE SPACES.
           03  FILLER                  PIC  X(017)         VALUE
               ' - FILE STATUS = '.
           03  WRK-FILE-STATUS         PIC  X(002)         VALUE SPACES.
           03  FILLER                  PIC  X(003)         VALUE  ' **'.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA POOL7100 ***'.
      *----------------------------------------------------------------*

       COPY POL7100C.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01 FILLER                       PIC  X(012)         VALUE
          '* AREA DB2 *'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
                INCLUDE RUFIB002
           END-EXEC.

           EXEC SQL
                INCLUDE CRURB0L7
           END-EXEC.

           EXEC SQL
                INCLUDE CRURB0MB
           END-EXEC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01 FILLER                       PIC  X(032)         VALUE
          '* FIM DA WORKING-STORAGE *'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

      *================================================================*
       PROCEDURE DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           CALL 'CKRS1000'.

           PERFORM 7000-INICIAR-CKRS0100.

           PERFORM 1000-INICIAR.

           PERFORM 2000-VERIFICAR-VAZIO.

           PERFORM 3000-PROCESSAR      UNTIL WRK-FS-CADRURAL EQUAL '10'.

           PERFORM 4000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT CADRURAL.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FS-CADRURAL.

           CALL 'POOL7600' USING WRK-DATA-HORA.

           MOVE  WRK-DT-AAAAMMDD       TO WRK-DATAAMD-R
           MOVE  WRK-SA-AMD            TO WRK-SA-DMA
           MOVE  WRK-MM-AMD            TO WRK-MM-DMA
           MOVE  WRK-DD-AMD            TO WRK-DD-DMA

           MOVE  WRK-7600-HORA         TO WRK-HH
           MOVE  WRK-7600-MINUTOS      TO WRK-MM
           MOVE  WRK-7600-SEGUNDOS     TO WRK-SS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1100-TESTAR-FS-CADRURAL         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-CADRURAL         NOT EQUAL '00'
               MOVE WRK-FS-CADRURAL    TO WRK-FILE-STATUS
               MOVE 'CADRURAL'         TO WRK-NOME-ARQUIVO
               MOVE WRK-ERRO-POOL7100  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-LER-CADRURAL.

           IF (ACU-LIDOS-CADRURAL      EQUAL ZEROS)
               DISPLAY '************ CMRU248C ************'
               DISPLAY '*                                *'
               DISPLAY '*     NAO HA CONTRATO PRONAF     *'
               DISPLAY '*                                *'
               DISPLAY '************ CMRU248C ************'
               PERFORM 4000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2100-LER-CADRURAL               SECTION.
      *----------------------------------------------------------------*

           READ CADRURAL.

           IF  WRK-FS-CADRURAL         EQUAL '10'
               GO                      TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FS-CADRURAL.
WP1021
WP1021     IF N110TP00                 NOT EQUAL 50  AND  70  AND
WP1021                                           71  AND  72  AND 20
              GO TO                    2100-LER-CADRURAL
           END-IF.

WP1021***  I  N110C096              NOT EQUAL 35 AND 38 AND 39
WP1021***     G  TO                 2100-LER-CADRURAL
WP1021***  E D-IF

           ADD 1                       TO ACU-LIDOS-CADRURAL.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

WP1021     EVALUATE  N110TP00
WP1021         WHEN  50
                     PERFORM 3260-ATUALIZ-RUFIB002-TIPO-50
WP1021         WHEN  70
WP1021               PERFORM 3230-ATUALIZ-RUFIB002-TIPO-70
WP1021         WHEN  71
WP1021               PERFORM 3240-ATUALIZ-RUFIB002-TIPO-71
WP1021         WHEN  72
WP1021               PERFORM 3250-ATUALIZ-RUFIB002-TIPO-72
B0923          WHEN  20
B0923                PERFORM 3270-ATUALIZ-RUFIB002-TIPO-20
WP1021     END-EVALUATE.

WP1021     IF N110C096              EQUAL 35 OR  38 OR  39
DPS01                                        OR  43 OR  47
              PERFORM 3100-OBTER-CONTR-AGRO

              IF CMRUWAFA-S-NSEQ-CONTR     NOT EQUAL ZEROS
                 PERFORM 3200-OBTER-DAP
              END-IF

WP1021     END-IF.

           PERFORM 7100-CHECKPOINT-CKRS0100

           PERFORM 2100-LER-CADRURAL.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3100-OBTER-CONTR-AGRO            SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-AREA-CMRU2AFT.

           MOVE N110CHAV               TO CMRUWAFA-E-CCONTR-LGADO

           CALL WRK-CMRU2AFT           USING WRK-AREA-CMRU2AFT.

           IF  CMRUWAFA-COD-RETORNO    EQUAL 12
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  CMRUWAFA-COD-RETORNO    NOT EQUAL ZEROS
               DISPLAY 'CONTRATO NAS NAO ENCONTRADO'
               DISPLAY ' *** CONTRATO = ' N110CHAV
               MOVE ZEROS     TO CMRUWAFA-S-CPSSOA-JURID
                                 CMRUWAFA-S-CTPO-CONTR
                                 CMRUWAFA-S-NSEQ-CONTR
           END-IF.


      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3200-OBTER-DAP                   SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-CRURB0MB-NULL.

           MOVE CMRUWAFA-S-CPSSOA-JURID TO
                                         CPSSOA-JURID-CONTR OF CRURB0L7.
           MOVE CMRUWAFA-S-CTPO-CONTR   TO
                                         CTPO-CONTR-NEGOC   OF CRURB0L7.
           MOVE CMRUWAFA-S-NSEQ-CONTR   TO
                                         NSEQ-CONTR-NEGOC   OF CRURB0L7.

           EXEC SQL
              SELECT B.CDECLR_AGCLT_FAMLR
                    ,B.DVALDD_DECLR_FAMLR
                    ,B.CGRP_ESPCL_RURAL
               INTO  :CRURB0MB.CDECLR-AGCLT-FAMLR
                        :WRK-MB-CDECLR-NULL
                    ,:CRURB0MB.DVALDD-DECLR-FAMLR
                        :WRK-MB-DVALDD-NULL
                    ,:CRURB0MB.CGRP-ESPCL-RURAL
                        :WRK-MB-CGRP-NULL
                FROM DB2PRD.TCOMPO_CONTR_CREDT  A
                    ,DB2PRD.TORCAM_CONTR_CREDT  B
               WHERE A.CPSSOA_JURID_CONTR = :CRURB0L7.CPSSOA-JURID-CONTR
                 AND A.CTPO_CONTR_NEGOC   = :CRURB0L7.CTPO-CONTR-NEGOC
                 AND A.NSEQ_CONTR_NEGOC   = :CRURB0L7.NSEQ-CONTR-NEGOC
                 AND A.CORCAM_CONTR_RURAL = B.CORCAM_CONTR_RURAL
                 AND B.CDECLR_AGCLT_FAMLR > ' '
            ORDER BY A.CPSSOA_JURID_CONTR ASC
                    ,A.CTPO_CONTR_NEGOC   ASC
                    ,A.NSEQ_CONTR_NEGOC   ASC
           END-EXEC.

           IF (SQLCODE                NOT EQUAL ZEROS AND +100)
           OR (SQLWARN0               EQUAL 'W')
               MOVE 'DB2'             TO ERR-TIPO-ACESSO
               MOVE 'TORCAM_CONTR_CREDT'
                                      TO ERR-DBD-TAB
               MOVE 'SELECT'          TO ERR-FUN-COMANDO
               MOVE SQLCODE           TO ERR-SQL-CODE
               MOVE '3200'            TO ERR-LOCAL
               MOVE SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF SQLCODE                  EQUAL ZEROS
              PERFORM 3210-TRATAR-NULL
              PERFORM 3220-ATUALIZ-RUFIB002
           END-IF.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3210-TRATAR-NULL                 SECTION.
      *----------------------------------------------------------------*

           IF  WRK-MB-CDECLR-NULL      LESS ZEROS
               MOVE ZEROS              TO CDECLR-AGCLT-FAMLR OF CRURB0MB
           END-IF.

           IF  WRK-MB-DVALDD-NULL      LESS ZEROS
               MOVE ZEROS              TO DVALDD-DECLR-FAMLR OF CRURB0MB
           END-IF.

           IF  WRK-MB-CGRP-NULL        LESS ZEROS
               MOVE ZEROS              TO CGRP-ESPCL-RURAL   OF CRURB0MB
           END-IF.

      *----------------------------------------------------------------*
       3210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3220-ATUALIZ-RUFIB002            SECTION.
      *----------------------------------------------------------------*

           MOVE N110CHAV              TO CCONTR             OF RUFIB002.
           MOVE CDECLR-AGCLT-FAMLR    OF CRURB0MB
                                      TO CDECLR-AGCLT-FAMLR OF RUFIB002.
           MOVE DVALDD-DECLR-FAMLR    OF CRURB0MB
                                      TO DVALDD-DECLR-FAMLR OF RUFIB002.
           MOVE CGRP-ESPCL-RURAL      OF CRURB0MB
                                      TO CGRP-AGCLT-FAMLR   OF RUFIB002.

           EXEC SQL
                UPDATE DB2PRD.CONTRATO_RURAL
                  SET CDECLR_AGCLT_FAMLR = :RUFIB002.CDECLR-AGCLT-FAMLR
                    , DVALDD_DECLR_FAMLR = :RUFIB002.DVALDD-DECLR-FAMLR
                    , CGRP_AGCLT_FAMLR   = :RUFIB002.CGRP-AGCLT-FAMLR
                  WHERE CCONTR           = :RUFIB002.CCONTR
           END-EXEC.

           IF (SQLCODE        NOT EQUAL ZEROS AND +100 AND -530) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'CONTRATO_RURAL '  TO ERR-DBD-TAB
               MOVE 'UPDATE'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '3220'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF SQLCODE                  EQUAL ZEROS
              ADD 1                    TO ACU-ATUALIZADOS
           END-IF.

           IF SQLCODE                  EQUAL +100
              DISPLAY ' CONTR.NAO LOCALIZADO RUFIB002 =  ' N110CHAV
           END-IF.

           IF SQLCODE                  EQUAL -530
              MOVE SQLERRMC(1:8)       TO WRK-RELAC
              DISPLAY ' CONTR.C/PROBL.RELACIONAMENTO  =  ' N110CHAV
                      ' FK = ' WRK-RELAC
           END-IF.

      *----------------------------------------------------------------*
       3220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

WP1021*----------------------------------------------------------------*
WP1021 3230-ATUALIZ-RUFIB002-TIPO-70    SECTION.
WP1021*----------------------------------------------------------------*
WP1021
WP1021     MOVE N110CHAV              TO CCONTR             OF RUFIB002.
WP1021
WP1021     IF  N110CCOR               EQUAL  LOW-VALUES
WP1021         MOVE  -1               TO     WRK-CCETFC-ORGNC-EMPTO-NULL
WP1021         MOVE  ZEROS            TO     CCETFC-ORGNC-EMPTO
WP1021                                                      OF  RUFIB002
WP1021     ELSE
WP1021         MOVE  ZEROS            TO     WRK-CCETFC-ORGNC-EMPTO-NULL
WP1021         MOVE  N110CCOR         TO     CCETFC-ORGNC-EMPTO
WP1021                                                      OF  RUFIB002
WP1021     END-IF.
WP1021
WP1021     IF  N110CREM               EQUAL  LOW-VALUES
WP1021         MOVE  -1               TO     WRK-CRSTRB-EMPTO-RURAL-NULL
WP1021         MOVE  ZEROS            TO     CRSTRB-EMPTO-RURAL
WP1021                                                      OF  RUFIB002
WP1021     ELSE
WP1021         MOVE  ZEROS            TO     WRK-CRSTRB-EMPTO-RURAL-NULL
WP1021         MOVE  N110CREM         TO     CRSTRB-EMPTO-RURAL
WP1021                                                      OF  RUFIB002
WP1021     END-IF.
WP1021
WP1021     IF (WRK-CRSTRB-EMPTO-RURAL-NULL   LESS  ZEROS)  AND
WP1021        (WRK-CCETFC-ORGNC-EMPTO-NULL   LESS  ZEROS)
WP1021         GO TO  3230-99-FIM
WP1021     END-IF.
WP1021
WP1021     EXEC SQL
WP1021          UPDATE DB2PRD.CONTRATO_RURAL
WP1021            SET CCETFC_ORGNC_EMPTO = :RUFIB002.CCETFC-ORGNC-EMPTO
WP1021                                      :WRK-CCETFC-ORGNC-EMPTO-NULL
WP1021              , CRSTRB_EMPTO_RURAL = :RUFIB002.CRSTRB-EMPTO-RURAL
WP1021                                      :WRK-CRSTRB-EMPTO-RURAL-NULL
WP1021            WHERE CCONTR           = :RUFIB002.CCONTR
WP1021     END-EXEC.
WP1021
WP1021     IF (SQLCODE        NOT EQUAL ZEROS AND +100 AND -530) OR
WP1021        (SQLWARN0                EQUAL 'W')
WP1021         MOVE 'DB2'              TO ERR-TIPO-ACESSO
WP1021         MOVE 'CONTRATO_RURAL '  TO ERR-DBD-TAB
WP1021         MOVE 'UPDATE'           TO ERR-FUN-COMANDO
WP1021         MOVE SQLCODE            TO ERR-SQL-CODE
WP1021         MOVE '3230'             TO ERR-LOCAL
WP1021         MOVE SPACES             TO ERR-SEGM
WP1021         PERFORM 9999-PROCESSAR-ROTINA-ERRO
WP1021     END-IF.
WP1021
WP1021     IF SQLCODE                  EQUAL ZEROS
WP1021        ADD 1                    TO ACU-ATUALIZADOS-TB70
WP1021     END-IF.
WP1021
WP1021     IF SQLCODE                  EQUAL +100
WP1021        DISPLAY ' CONTR.NAO LOCALIZADO RUFIB002(70) = ' N110CHAV
WP1021     END-IF.
WP1021
WP1021     IF SQLCODE                  EQUAL -530
WP1021        MOVE SQLERRMC(1:8)       TO WRK-RELAC
WP1021        DISPLAY ' CONTR.C/PROBL.RELACIONAMENTO(70) = ' N110CHAV
WP1021                ' FK = ' WRK-RELAC
WP1021     END-IF.
WP1021
WP1021*----------------------------------------------------------------*
WP1021 3230-99-FIM.                    EXIT.
WP1021*----------------------------------------------------------------*

WP1021*----------------------------------------------------------------*
WP1021 3240-ATUALIZ-RUFIB002-TIPO-71    SECTION.
WP1021*----------------------------------------------------------------*
WP1021
WP1021     MOVE N110CHAV              TO CCONTR             OF RUFIB002.
WP1021
WP1021     IF  N110EUCL               EQUAL  ZEROS
WP1021         GO TO  3240-99-FIM
WP1021     END-IF.
WP1021
WP1021     MOVE  ZEROS                TO     WRK-EURL-CETFC-ORGNC-NULL
WP1021     MOVE  N110EUCO             TO     EURL-CETFC-ORGNC
WP1021                                                      OF  RUFIB002
WP1021     EXEC SQL
WP1021          UPDATE DB2PRD.CONTRATO_RURAL
WP1021            SET EURL_CETFC_ORGNC   = :RUFIB002.EURL-CETFC-ORGNC
WP1021                                      :WRK-EURL-CETFC-ORGNC-NULL
WP1021            WHERE CCONTR           = :RUFIB002.CCONTR
WP1021     END-EXEC.
WP1021
WP1021     IF (SQLCODE        NOT EQUAL ZEROS AND +100 AND -530) OR
WP1021        (SQLWARN0                EQUAL 'W')
WP1021         MOVE 'DB2'              TO ERR-TIPO-ACESSO
WP1021         MOVE 'CONTRATO_RURAL '  TO ERR-DBD-TAB
WP1021         MOVE 'UPDATE'           TO ERR-FUN-COMANDO
WP1021         MOVE SQLCODE            TO ERR-SQL-CODE
WP1021         MOVE '3240'             TO ERR-LOCAL
WP1021         MOVE SPACES             TO ERR-SEGM
WP1021         PERFORM 9999-PROCESSAR-ROTINA-ERRO
WP1021     END-IF.
WP1021
WP1021     IF SQLCODE                  EQUAL ZEROS
WP1021        ADD 1                    TO ACU-ATUALIZADOS-TB71
WP1021     END-IF.
WP1021
WP1021     IF SQLCODE                  EQUAL +100
WP1021        DISPLAY ' CONTR.NAO LOCALIZADO RUFIB002(71) = ' N110CHAV
WP1021     END-IF.
WP1021
WP1021     IF SQLCODE                  EQUAL -530
WP1021        MOVE SQLERRMC(1:8)       TO WRK-RELAC
WP1021        DISPLAY ' CONTR.C/PROBL.RELACIONAMENTO(71) = ' N110CHAV
WP1021                ' FK = ' WRK-RELAC
WP1021     END-IF.
WP1021
WP1021*----------------------------------------------------------------*
WP1021 3240-99-FIM.                    EXIT.
WP1021*----------------------------------------------------------------*

WP1021*----------------------------------------------------------------*
WP1021 3250-ATUALIZ-RUFIB002-TIPO-72    SECTION.
WP1021*----------------------------------------------------------------*
WP1021
WP1021     MOVE N110CHAV              TO CCONTR             OF RUFIB002.
WP1021
WP1021     IF  N110EURL               EQUAL  ZEROS
WP1021         GO TO  3250-99-FIM
WP1021     END-IF.
WP1021
WP1021     MOVE  ZEROS                TO     WRK-EURL-RSTRB-EMPTO-NULL
WP1021     MOVE  N110EURE             TO     EURL-RSTRB-EMPTO
WP1021                                                      OF  RUFIB002
WP1021     EXEC SQL
WP1021          UPDATE DB2PRD.CONTRATO_RURAL
WP1021            SET EURL_RSTRB_EMPTO   = :RUFIB002.EURL-RSTRB-EMPTO
WP1021                                      :WRK-EURL-RSTRB-EMPTO-NULL
WP1021            WHERE CCONTR           = :RUFIB002.CCONTR
WP1021     END-EXEC.
WP1021
WP1021     IF (SQLCODE        NOT EQUAL ZEROS AND +100 AND -530) OR
WP1021        (SQLWARN0                EQUAL 'W')
WP1021         MOVE 'DB2'              TO ERR-TIPO-ACESSO
WP1021         MOVE 'CONTRATO_RURAL '  TO ERR-DBD-TAB
WP1021         MOVE 'UPDATE'           TO ERR-FUN-COMANDO
WP1021         MOVE SQLCODE            TO ERR-SQL-CODE
WP1021         MOVE '3250'             TO ERR-LOCAL
WP1021         MOVE SPACES             TO ERR-SEGM
WP1021         PERFORM 9999-PROCESSAR-ROTINA-ERRO
WP1021     END-IF.
WP1021
WP1021     IF SQLCODE                  EQUAL ZEROS
WP1021        ADD 1                    TO ACU-ATUALIZADOS-TB72
WP1021     END-IF.
WP1021
WP1021     IF SQLCODE                  EQUAL +100
WP1021        DISPLAY ' CONTR.NAO LOCALIZADO RUFIB002(72) =  ' N110CHAV
WP1021     END-IF.
WP1021
WP1021     IF SQLCODE                  EQUAL -530
WP1021        MOVE SQLERRMC(1:8)       TO WRK-RELAC
WP1021        DISPLAY ' CONTR.C/PROBL.RELACIONAMENTO(72) =  ' N110CHAV
WP1021                ' FK = ' WRK-RELAC
WP1021     END-IF.
WP1021
WP1021*----------------------------------------------------------------*
WP1021 3250-99-FIM.                    EXIT.
WP1021*----------------------------------------------------------------*
WP1021
WP1021*----------------------------------------------------------------*
WP1021 3260-ATUALIZ-RUFIB002-TIPO-50    SECTION.
WP1021*----------------------------------------------------------------*
WP1021
WP1021     MOVE N110CHAV              TO CCONTR             OF RUFIB002.
WP1021
WP1021
WP1021     IF  N110PRRE           NOT NUMERIC
WP1021         MOVE  999,99           TO N110PRRE
WP1021     END-IF.
WP1021
WP1021     IF  N110CTSA           NOT NUMERIC
WP1021         MOVE  ZEROS            TO N110CTSA
WP1021     END-IF.
WP1021
WP1021     IF  N110CCTV           NOT NUMERIC
WP1021         MOVE  ZEROS            TO N110CCTV
WP1021     END-IF.
WP1021
WP1021     IF  N110CMSL           NOT NUMERIC
WP1021         MOVE  ZEROS            TO N110CMSL
WP1021     END-IF.
WP1021
WP1021     IF  N110PRRE               EQUAL  999,99
WP1021         MOVE  -1               TO     WRK-PENERG-RENOV-EMPTO-NULL
WP1021         MOVE  ZEROS            TO     PENERG-RENOV-EMPTO
WP1021                                                      OF  RUFIB002
WP1021     ELSE
WP1021         MOVE  ZEROS            TO     WRK-PENERG-RENOV-EMPTO-NULL
WP1021         MOVE  N110PRRE         TO     PENERG-RENOV-EMPTO
WP1021                                                      OF  RUFIB002
WP1021     END-IF.
WP1021
WP1021     IF  N110CTSA               EQUAL  ZEROS
WP1021         MOVE  -1               TO     WRK-CTPO-SOLO-AGCLT-NULL
WP1021         MOVE  ZEROS            TO     CTPO-SOLO-AGCLT
WP1021                                                      OF  RUFIB002
WP1021     ELSE
WP1021         MOVE  ZEROS            TO     WRK-CTPO-SOLO-AGCLT-NULL
WP1021         MOVE  N110CTSA         TO     CTPO-SOLO-AGCLT
WP1021                                                      OF  RUFIB002
WP1021     END-IF.
WP1021
WP1021     IF  N110CCTV               EQUAL  ZEROS
WP1021         MOVE  -1               TO     WRK-CCICLO-CTVDA-NULL
WP1021         MOVE  ZEROS            TO     CCICLO-CTVDA
WP1021                                                      OF  RUFIB002
WP1021     ELSE
WP1021         MOVE  ZEROS            TO     WRK-CCICLO-CTVDA-NULL
WP1021         MOVE  N110CCTV         TO     CCICLO-CTVDA
WP1021                                                      OF  RUFIB002
WP1021     END-IF.
WP1021
WP1021     IF  N110CMSL               EQUAL  ZEROS
WP1021         MOVE  -1               TO     WRK-CMANUT-SOLO-NULL
WP1021         MOVE  ZEROS            TO     CMANUT-SOLO
WP1021                                                      OF  RUFIB002
WP1021     ELSE
WP1021         MOVE  ZEROS            TO     WRK-CMANUT-SOLO-NULL
WP1021         MOVE  N110CMSL         TO     CMANUT-SOLO
WP1021                                                      OF  RUFIB002
WP1021     END-IF.
WP1021
WP1021     EXEC SQL
WP1021          UPDATE DB2PRD.CONTRATO_RURAL
WP1021            SET PENERG_RENOV_EMPTO = :RUFIB002.PENERG-RENOV-EMPTO
WP1021                                      :WRK-PENERG-RENOV-EMPTO-NULL
WP1021              , CTPO_SOLO_AGCLT    = :RUFIB002.CTPO-SOLO-AGCLT
WP1021                                      :WRK-CTPO-SOLO-AGCLT-NULL
WP1021              , CCICLO_CTVDA       = :RUFIB002.CCICLO-CTVDA
WP1021                                      :WRK-CCICLO-CTVDA-NULL
WP1021              , CMANUT_SOLO        = :RUFIB002.CMANUT-SOLO
WP1021                                      :WRK-CMANUT-SOLO-NULL
WP1021            WHERE CCONTR           = :RUFIB002.CCONTR
WP1021     END-EXEC.
WP1021
WP1021     IF (SQLCODE        NOT EQUAL ZEROS AND +100 AND -530) OR
WP1021        (SQLWARN0                EQUAL 'W')
WP1021         MOVE 'DB2'              TO ERR-TIPO-ACESSO
WP1021         MOVE 'CONTRATO_RURAL '  TO ERR-DBD-TAB
WP1021         MOVE 'UPDATE'           TO ERR-FUN-COMANDO
WP1021         MOVE SQLCODE            TO ERR-SQL-CODE
WP1021         MOVE '3260'             TO ERR-LOCAL
WP1021         MOVE SPACES             TO ERR-SEGM
WP1021         PERFORM 9999-PROCESSAR-ROTINA-ERRO
WP1021     END-IF.
WP1021
WP1021     IF SQLCODE                  EQUAL ZEROS
WP1021        ADD 1                    TO ACU-ATUALIZADOS-TB50
WP1021     END-IF.
WP1021
WP1021     IF SQLCODE                  EQUAL +100
WP1021        DISPLAY ' CONTR.NAO LOCALIZADO RUFIB002 =  ' N110CHAV
WP1021     END-IF.
WP1021
WP1021     IF SQLCODE                  EQUAL -530
WP1021        MOVE SQLERRMC(1:8)       TO WRK-RELAC
WP1021        DISPLAY ' CONTR.C/PROBL.RELACIONAMENTO  =  ' N110CHAV
WP1021                ' FK = ' WRK-RELAC
WP1021     END-IF.
WP1021
WP1021*----------------------------------------------------------------*
WP1021 3260-99-FIM.                    EXIT.
WP1021*----------------------------------------------------------------*

B0923 *----------------------------------------------------------------*
B0923  3270-ATUALIZ-RUFIB002-TIPO-20   SECTION.
B0923 *----------------------------------------------------------------*
B0923
B0923      MOVE N110CHAV               TO CCONTR
B0923                                  OF RUFIB002.
           IF  N110CPTXRED             NOT NUMERIC
               MOVE ZEROS              TO N110CPTXRED
           END-IF.
B0923
B0923      IF  N110CIREDT              EQUAL SPACES OR LOW-VALUES
B0923          MOVE -1                 TO WRK-PREDC-JURO-AMBTL-NULL
B0923      ELSE
B0923          MOVE ZEROS              TO WRK-PREDC-JURO-AMBTL-NULL
B0923          MOVE N110CPTXRED        TO PREDC-JURO-AMBTL
B0923                                  OF RUFIB002
B0923      END-IF.
B0923
B0923      EXEC SQL
B0923           UPDATE DB2PRD.CONTRATO_RURAL
B0923             SET PREDC_JURO_AMBTL   = :RUFIB002.PREDC-JURO-AMBTL
B0923                                       :WRK-PREDC-JURO-AMBTL-NULL
B0923             WHERE CCONTR           = :RUFIB002.CCONTR
B0923      END-EXEC.
B0923
B0923      IF (SQLCODE        NOT EQUAL ZEROS AND +100 AND -530) OR
B0923         (SQLWARN0                EQUAL 'W')
B0923          MOVE 'DB2'              TO ERR-TIPO-ACESSO
B0923          MOVE 'CONTRATO_RURAL '  TO ERR-DBD-TAB
B0923          MOVE 'UPDATE'           TO ERR-FUN-COMANDO
B0923          MOVE SQLCODE            TO ERR-SQL-CODE
B0923          MOVE '3270'             TO ERR-LOCAL
B0923          MOVE SPACES             TO ERR-SEGM
B0923          PERFORM 9999-PROCESSAR-ROTINA-ERRO
B0923      END-IF.
B0923
B0923      IF SQLCODE                  EQUAL ZEROS
B0923         ADD 1                    TO ACU-ATUALIZADOS-TB20
B0923      END-IF.
B0923
B0923      IF SQLCODE                  EQUAL +100
B0923         DISPLAY ' CONTR.NAO LOCALIZADO RUFIB002 =  ' N110CHAV
B0923      END-IF.
B0923
B0923      IF SQLCODE                  EQUAL -530
B0923         MOVE SQLERRMC(1:8)       TO WRK-RELAC
B0923         DISPLAY ' CONTR.C/PROBL.RELACIONAMENTO  =  ' N110CHAV
B0923                 ' FK = ' WRK-RELAC
B0923      END-IF.
B0923
B0923 *----------------------------------------------------------------*
B0923  3270-99-FIM.                    EXIT.
B0923 *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 4100-EMITIR-DISPLAY

           CLOSE CADRURAL.

           MOVE    WRK-FECHAMENTO      TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FS-CADRURAL.

           PERFORM 7200-ENCERRAR-CKRS0100

           GOBACK.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4100-EMITIR-DISPLAY           SECTION.
      *----------------------------------------------------------------*


           DISPLAY '******************* CMRU248C *******************'.
           MOVE ACU-LIDOS-CADRURAL     TO WRK-MASCARA.
           DISPLAY '*                                              *'.
           DISPLAY '* QTDE DE LIDOS EM CADRURAL = ' WRK-MASCARA
                                                                '  *'.
           MOVE ACU-ATUALIZADOS        TO WRK-MASCARA.
           DISPLAY '*                                              *'.
           DISPLAY '* REGS ATUALIZADOS RUFIB002 = ' WRK-MASCARA
                                                                '  *'.
           MOVE ACU-ATUALIZADOS-TB70   TO WRK-MASCARA.
           DISPLAY '*                                              *'.
           DISPLAY '* REGS ATUALIZADOS RUFIB002(70) = ' WRK-MASCARA
                                                                '  *'.
           MOVE ACU-ATUALIZADOS-TB71   TO WRK-MASCARA.
           DISPLAY '*                                              *'.
           DISPLAY '* REGS ATUALIZADOS RUFIB002(71) = ' WRK-MASCARA
                                                                '  *'.
           MOVE ACU-ATUALIZADOS-TB72   TO WRK-MASCARA.
           DISPLAY '*                                              *'.
           DISPLAY '* REGS ATUALIZADOS RUFIB002(72) = ' WRK-MASCARA
                                                                '  *'.
           MOVE ACU-ATUALIZADOS-TB50   TO WRK-MASCARA.
           DISPLAY '*                                              *'.
           DISPLAY '* REGS ATUALIZADOS RUFIB002(50) = ' WRK-MASCARA
                                                                '  *'.
B0923      MOVE ACU-ATUALIZADOS-TB20   TO WRK-MASCARA.
B0923      DISPLAY '*                                              *'.
B0923      DISPLAY '* REGS ATUALIZADOS RUFIB002(20) = ' WRK-MASCARA
B0923                                                           '  *'.
           DISPLAY '*                                              *'.
           DISPLAY '******************* CMRU248C *******************'.

      *----------------------------------------------------------------*
       4100-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       7000-INICIAR-CKRS0100           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE CKRS01-INTERFACE.

           MOVE 'I'                    TO CK01-FUNCAO.
           MOVE 'DB2'                  TO CK01-ID-DB2.
           MOVE WRK-AREA-RESTART       TO CK01-AREA-RESTART.
           MOVE LENGTH OF WRK-AREA-RESTART
                                       TO CK01-TAM-AREA-RESTART.

           CALL  WRK-CKRS0100          USING CKRS01-INTERFACE.

           IF  CK01-CODIGO-RETORNO     NOT EQUAL ZEROS
               PERFORM 7300-CKRS-TRATAR-RETORNO
           END-IF.

           IF  CK01-STATUS             EQUAL 'REST'
               MOVE CK01-AREA-RESTART  (1: LENGTH OF WRK-AREA-RESTART)
                                       TO  WRK-AREA-RESTART
               MOVE WRK-REST-CADRURAL  TO  ACU-LIDOS-CADRURAL
               MOVE WRK-REST-RUFIB002  TO  ACU-ATUALIZADOS
           ELSE
               DISPLAY 'CMRU248C - PROCESSAMENTO NORMAL (SEM RESTART).'
               MOVE ZEROS              TO   ACU-LIDOS-CADRURAL
                                            ACU-ATUALIZADOS
           END-IF.

      *----------------------------------------------------------------*
       7000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       7100-CHECKPOINT-CKRS0100        SECTION.
      *----------------------------------------------------------------*

           INITIALIZE CKRS01-INTERFACE.

           MOVE 'P'                    TO CK01-FUNCAO.
           MOVE 'DB2'                  TO CK01-ID-DB2.
           MOVE ACU-LIDOS-CADRURAL     TO WRK-REST-CADRURAL
           MOVE ACU-ATUALIZADOS        TO WRK-REST-RUFIB002

           MOVE WRK-AREA-RESTART       TO CK01-AREA-RESTART.
           MOVE LENGTH                 OF WRK-AREA-RESTART
                                       TO CK01-TAM-AREA-RESTART.

           CALL  WRK-CKRS0100          USING CKRS01-INTERFACE.

           IF  CK01-CODIGO-RETORNO     NOT EQUAL ZEROS
               PERFORM 7300-CKRS-TRATAR-RETORNO
           END-IF.

      *----------------------------------------------------------------*
       7100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       7200-ENCERRAR-CKRS0100          SECTION.
      *----------------------------------------------------------------*

           INITIALIZE CKRS01-INTERFACE.

           MOVE 'F'                    TO CK01-FUNCAO.
           MOVE 'DB2'                  TO CK01-ID-DB2.

           CALL  WRK-CKRS0100          USING CKRS01-INTERFACE.

           IF  CK01-CODIGO-RETORNO     NOT EQUAL ZEROS
               PERFORM 7300-CKRS-TRATAR-RETORNO
           END-IF.

      *----------------------------------------------------------------*
       7200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       7300-CKRS-TRATAR-RETORNO        SECTION.
      *----------------------------------------------------------------*

           IF  CK01-CODIGO-RETORNO     EQUAL ZEROS
               GO TO 7300-99-FIM
           END-IF.

           IF CK01-CODIGO-RETORNO      EQUAL 08
              DISPLAY '************** RURA3887 **************'
              DISPLAY '*'
              DISPLAY '* CK01-TEXTO-MENSAGEM: ' CK01-TEXTO-MENSAGEM
              DISPLAY '*'
              DISPLAY '************** RURA3887 **************'
              MOVE 'APL'                       TO ERR-TIPO-ACESSO
              PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE 'CKRS0100-ERRO NAO CONTROLADO' TO CK01-TEXTO-MENSAGEM.
           DISPLAY '************** RURA3887 **************'.
           DISPLAY '*'.
           DISPLAY '* CK01-CODIGO-RETORNO : ' CK01-CODIGO-RETORNO.
           DISPLAY '* CK01-CODIGO-MENSAGEM: ' CK01-CODIGO-MENSAGEM.
           DISPLAY '* CK01-CODIGO-RETORNO : ' CK01-CODIGO-RETORNO.
           DISPLAY '*'.
           DISPLAY '************** RURA3887 **************'.
           MOVE 'APL'                          TO ERR-TIPO-ACESSO
           PERFORM 9999-PROCESSAR-ROTINA-ERRO.

      *----------------------------------------------------------------*
       7300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO    SECTION.
      *----------------------------------------------------------------*

           MOVE 'CMRU248C'             TO ERR-PGM.

           CALL 'POOL7100'             USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA.
           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

