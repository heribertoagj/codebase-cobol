      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. RUCA24XC.
       AUTHOR.     HERIBERTO GIANNASI.

      *================================================================*
      *                          7 C O M M                             *
      *----------------------------------------------------------------*
      *    PROGRAMA....:  RUCA24XC                                     *
      *    ANALISTA....:  HERIBERTO GIANNASI - 7 COMM                  *
      *    DATA........:  MAR/2026                                     *
      *----------------------------------------------------------------*
      *    OBJETIVO....:  ATUALIZA A TABELA RUFIB002 COM OS DADOS      *
      *                   DO CRUR                                      *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME           I/O           INCLUDE/BOOK     *
      *                RELATO           O              -               *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    POOL7100 - MODULO PARA TRATAMENTO DE ERROS                  *
      *    BRAD7600 - MODULO PRA OBTER DATA/HORA DO SISTEMA            *
      *    CKRS0105 - MODULO PARA INICIAR/COMMIT/FINALIZAR CONEXAO DB2 *
      *    CKRS1000 - MODULO PARA INDICAR PROCESSAMWENTO               *
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

           SELECT RELATO               ASSIGN TO RELATO
                  ORGANIZATION         IS SEQUENTIAL
                  FILE STATUS          IS WRK-FS-RELATO.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

       FD  RELATO
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REG-RELATO                  PIC  X(080).

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
       01  WRK-S9-09                   PIC +9(009)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-S9-09.
           05 WRK-9-09                 PIC  9(009).

       01  WRK-S9-02                   PIC +9(002)        VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-S9-02.
           05 WRK-9-02                 PIC  9(002).

       77  ACU-LIDOS                   PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-ATUALIZADOS             PIC  9(009) COMP-3  VALUE ZEROS.

       01  WRK-FIM-CSR01               PIC  X(001)         VALUE 'N'.

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
       01  WRK-FS-RELATO               PIC  X(002)         VALUE SPACES.

       01  WRK-ABERTURA                PIC  X(013)         VALUE
           ' NA ABERTURA '.
       01  WRK-ESCRITA                 PIC  X(013)         VALUE
           ' NA ESCRITA  '.
       01  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01 FILLER                       PIC  X(032)         VALUE
          '* REGISTRO RELATORIO *'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01  WRK-REG-RELATO.
           05 WRK-REL-CCONTR           PIC  9(009).
           05 FILLER                   PIC  X(001)         VALUE ' '.
           05 WRK-REL-CDECLR           PIC  X(025).
           05 FILLER                   PIC  X(001)         VALUE ' '.
           05 WRK-REL-DVALDD           PIC  X(010).
           05 FILLER                   PIC  X(001)         VALUE ' '.
           05 WRK-REL-CGRP             PIC  9(002).
           05 FILLER                   PIC  X(031)         VALUE SPACES.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01 FILLER                       PIC  X(032)         VALUE
          '* AREA DE ACUMULADORES *'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01 FILLER                       PIC  X(032)         VALUE
          '* AREA DE CAMPOS NULOS *'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01 WRK-CRURB0MB-NULL.
           05 WRK-MB-CDECLR-NULL        PIC S9(004) COMP    VALUE ZEROS.
           05 WRK-MB-DVALDD-NULL        PIC S9(004) COMP    VALUE ZEROS.
           05 WRK-MB-CGRP-NULL          PIC S9(004) COMP    VALUE ZEROS.

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
          '* AREA CKRS0105 (CONEXAO DB2) *'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       77  WRK-CKRS0105                PIC  X(008)         VALUE
           'CKRS0105'.

           COPY 'I#CKRS04'.

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

           EXEC SQL
              DECLARE CSR01-RUFIB002    CURSOR FOR
              SELECT CCONTR
                FROM DB2PRD.CONTRATO_RURAL                
              WITH UR
           END-EXEC.
           
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01 FILLER                       PIC  X(032)         VALUE
          '* FIM DA WORKING-STORAGE *'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       LINKAGE                         SECTION.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  LNK-AREA.
           05 LNK-TAM-AREA             PIC S9(004) COMP.
           05 LNK-TIPO-PROCESSAMENTO   PIC  9(001).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01 FILLER                       PIC  X(032)         VALUE
          '* FIM DA LINKAGE SECTION *'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      *================================================================*
       PROCEDURE                        DIVISION USING LNK-AREA.
      *================================================================*

      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           CALL 'CKRS1000'.

           PERFORM 7000-INICIAR-CKRS0105.

           PERFORM 1000-INICIAR.

           PERFORM 1500-ABRIR-RELATO.

           PERFORM 2100-ABRIR-CSR01     

           PERFORM 2200-LER-CSR01.

           IF WRK-FIM-CSR01            EQUAL 'S'
              DISPLAY '******************RUCA24XC********************'
              DISPLAY '*                                            *'
              DISPLAY '*    NAO HA CONTRATO PRONAF PARA ATUALIZAR   *'
              DISPLAY '*                                            *'
              DISPLAY '******************RUCA24XC*******************'              
              PERFORM 4000-FINALIZAR
           END-IF.

           PERFORM                     UNTIL WRK-FIM-CSR01 
                                       EQUAL 'S'
              PERFORM 3000-PROCESSAR      
              PERFORM 2200-LER-CSR01
           END-PERFORM.

           PERFORM 2300-FECHAR-CSR01.

           PERFORM 4000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL7600' USING WRK-DATA-HORA.

           MOVE  WRK-DT-AAAAMMDD       TO WRK-DATAAMD-R
           MOVE  WRK-SA-AMD            TO WRK-SA-DMA
           MOVE  WRK-MM-AMD            TO WRK-MM-DMA
           MOVE  WRK-DD-AMD            TO WRK-DD-DMA

           MOVE  WRK-7600-HORA         TO WRK-HH
           MOVE  WRK-7600-MINUTOS      TO WRK-MM
           MOVE  WRK-7600-SEGUNDOS     TO WRK-SS.

           DISPLAY '******************RUCA24XC********************'
           DISPLAY '*                                            *'
           DISPLAY '*          INICIO DE PROCESSAMENTO           *'
           DISPLAY '*                                            *'
           DISPLAY '******************RUCA24XC********************'.              

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
       2100-ABRIR-CSR01                SECTION.
      *----------------------------------------------------------------*
           
           EXEC SQL
              OPEN CSR01-RUFIB002
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS)
           OR (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'CONTRATO_RURAL'   TO ERR-DBD-TAB
               MOVE 'OPEN'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '2100'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE 'N'                    TO WRK-FIM-CSR01.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2200-LER-CSR01                 SECTION.
      *----------------------------------------------------------------*
           
           EXEC SQL
              FETCH CSR01-RUFIB002
              INTO :RUFIB002.CCONTR
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100)
           OR (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'CONTRATO_RURAL'   TO ERR-DBD-TAB
               MOVE 'FETCH'            TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '2200'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF SQLCODE                  EQUAL +100
              ADD 1                    TO ACU-LIDOS 
           ELSE
              MOVE 'S'                 TO WRK-FIM-CSR01
           END-IF.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2300-FECHAR-CSR01                SECTION.
      *----------------------------------------------------------------*
           
           EXEC SQL
              CLOSE CSR01-RUFIB002
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS)
           OR (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'CONTRATO_RURAL'   TO ERR-DBD-TAB
               MOVE 'CLOSE'            TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '2300'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*      
      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*
      * REGRA DESATIVADA: FILTRAR ORIGEM DE RECURSO (35/38/39/43/47)
      * POR EXIGENCIA DE QUALITY CODE (NAO MANTER COMANDOS COBOL
      * EXECUTAVEIS COMENTADOS NO FONTE).
           PERFORM 3100-OBTER-CONTR-AGRO

           IF CMRUWAFA-S-NSEQ-CONTR    NOT EQUAL ZEROS
              PERFORM 3200-OBTER-DAP
           END-IF

           PERFORM 7100-COMMIT-CKRS0105.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3100-OBTER-CONTR-AGRO           SECTION.
      *----------------------------------------------------------------*
      *
      * OBTEM A CHAVE CRUR CORRESPONDENTE AO CONTRATO
           INITIALIZE WRK-AREA-CMRU2AFT.

           MOVE CCONTR                 OF RUFIB002               
                                       TO CMRUWAFA-E-CCONTR-LGADO

           CALL WRK-CMRU2AFT           USING WRK-AREA-CMRU2AFT.

           IF  CMRUWAFA-COD-RETORNO    EQUAL 12
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  CMRUWAFA-COD-RETORNO    NOT EQUAL ZEROS
               DISPLAY 'CONTRATO NAS NAO ENCONTRADO'
               DISPLAY ' *** CONTRATO = ' CCONTR OF RUFIB002
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
      *
      * OBTEM DADOS DO DAP / CAF UTILIZANDO OS DADOS DO UORG  
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

              
              IF LNK-TIPO-PROCESSAMENTO IS NUMERIC 
              AND 3220-ATUALIZ-RUFIB002 EQUAL 2
                  PERFORM 3220-ATUALIZ-RUFIB002
              END-IF
              PERFORM 3230-GRAVAR-RELATO
           END-IF.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3210-TRATAR-NULL                 SECTION.
      *----------------------------------------------------------------*

           IF  WRK-MB-CDECLR-NULL      LESS ZEROS
               MOVE SPACES             TO CDECLR-AGCLT-FAMLR OF CRURB0MB
           END-IF.

           IF  WRK-MB-DVALDD-NULL      LESS ZEROS
               MOVE SPACES             TO DVALDD-DECLR-FAMLR OF CRURB0MB
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
                 WHERE CCONTR             = :RUFIB002.CCONTR
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100 
                                                       AND -530)
           OR (SQLWARN0                EQUAL 'W')
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
              DISPLAY ' CONTR.NAO LOCALIZADO RUFIB002 =  ' 
                                       CCONTR OF RUFIB002
           END-IF.

           IF SQLCODE                  EQUAL -530
              MOVE SQLERRMC(1:8)       TO WRK-RELAC
              DISPLAY ' CONTR.C/PROBL.RELACIONAMENTO  =  ' 
                                       CCONTR OF RUFIB002                
           END-IF.

      *----------------------------------------------------------------*
       3220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3230-GRAVAR-RELATO              SECTION.
      *----------------------------------------------------------------*

           MOVE CCONTR                 OF RUFIB002
                                       TO WRK-S9-09
           MOVE WRK-9-09               TO WRK-REL-CCONTR.
           MOVE CDECLR-AGCLT-FAMLR     OF RUFIB002
                                       TO WRK-REL-CDECLR.
           MOVE DVALDD-DECLR-FAMLR     OF RUFIB002
                                       TO WRK-REL-DVALDD.
           MOVE CGRP-AGCLT-FAMLR       OF RUFIB002
                                       TO WRK-S9-02
           MOVE WRK-9-02               TO WRK-REL-CGRP.

           MOVE WRK-REG-RELATO         TO REG-RELATO.
           WRITE REG-RELATO.

           MOVE WRK-ESCRITA            TO WRK-OPERACAO.
           PERFORM 1510-TESTAR-FS-RELATO.
      *----------------------------------------------------------------*
       3230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1500-ABRIR-RELATO               SECTION.
      *----------------------------------------------------------------*

           OPEN OUTPUT RELATO.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 1510-TESTAR-FS-RELATO.

      *----------------------------------------------------------------*
       1500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1510-TESTAR-FS-RELATO           SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-RELATO           NOT EQUAL '00'
               MOVE WRK-FS-RELATO      TO WRK-FILE-STATUS
               MOVE 'RELATO'           TO WRK-NOME-ARQUIVO
               MOVE WRK-ERRO-POOL7100  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1510-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1600-FECHAR-RELATO              SECTION.
      *----------------------------------------------------------------*

           CLOSE RELATO.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 1510-TESTAR-FS-RELATO.

      *----------------------------------------------------------------*
       1600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 4100-EMITIR-DISPLAY

           PERFORM 1600-FECHAR-RELATO

           PERFORM 7200-ENCERRAR-CKRS0105

           GOBACK.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       4100-EMITIR-DISPLAY             SECTION.
      *----------------------------------------------------------------*

           DISPLAY '******************* RUCA24XC *******************'.
           MOVE ACU-LIDOS              TO WRK-MASCARA.
           DISPLAY '*                                              *'.
           DISPLAY '* QTDE DE CONTRATOS LIDOS EM CADRURAL = '
                                         WRK-MASCARA
                                                                '  *'.
           MOVE ACU-ATUALIZADOS        TO WRK-MASCARA.
           DISPLAY '*                                              *'.
           DISPLAY '* REGS ATUALIZADOS RUFIB002 = ' WRK-MASCARA
                                                                '  *'.
           DISPLAY '*                                              *'.
           DISPLAY '******************* RUCA24XC *******************'.

      *----------------------------------------------------------------*
       4100-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       7000-INICIAR-CKRS0105           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE PARM-CKRS0105.

           MOVE 'C'                    TO PARM-OP.
           MOVE 'DB2'                  TO PARM-SSID.
           MOVE 'RUCA24XC'             TO PARM-PLAN.
           MOVE SPACES                 TO PARM-CORRID.

           CALL WRK-CKRS0105           USING PARM-CKRS0105.

           IF RETURN-CODE              NOT EQUAL ZEROS
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'CKRS0105'         TO ERR-DBD-TAB
               MOVE 'CONNECT'          TO ERR-FUN-COMANDO
               MOVE RETURN-CODE        TO ERR-SQL-CODE
               MOVE '7000'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       7000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       7100-COMMIT-CKRS0105            SECTION.
      *----------------------------------------------------------------*

           INITIALIZE PARM-CKRS0105.

           MOVE 'P'                    TO PARM-OP.
           MOVE 'DB2'                  TO PARM-SSID.
           MOVE 'RUCA24XC'             TO PARM-PLAN.
           MOVE SPACES                 TO PARM-CORRID.

           CALL WRK-CKRS0105           USING PARM-CKRS0105.

           IF RETURN-CODE              NOT EQUAL ZEROS
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'CKRS0105'         TO ERR-DBD-TAB
               MOVE 'COMMIT'           TO ERR-FUN-COMANDO
               MOVE RETURN-CODE        TO ERR-SQL-CODE
               MOVE '7100'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       7100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       7200-ENCERRAR-CKRS0105          SECTION.
      *----------------------------------------------------------------*

           INITIALIZE PARM-CKRS0105.

           MOVE 'D'                    TO PARM-OP.
           MOVE 'DB2'                  TO PARM-SSID.
           MOVE 'RUCA24XC'             TO PARM-PLAN.
           MOVE SPACES                 TO PARM-CORRID.

           CALL WRK-CKRS0105           USING PARM-CKRS0105.

           IF RETURN-CODE              NOT EQUAL ZEROS
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'CKRS0105'         TO ERR-DBD-TAB
               MOVE 'DISCONN'          TO ERR-FUN-COMANDO
               MOVE RETURN-CODE        TO ERR-SQL-CODE
               MOVE '7200'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       7200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO    SECTION.
      *----------------------------------------------------------------*

           MOVE 'RUCA24XC'             TO ERR-PGM.

           CALL 'POOL7100'             USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA.
           GOBACK.
      *----------------------------------------------------------------*
       9999-99-FIM.                  EXIT.
      *----------------------------------------------------------------*