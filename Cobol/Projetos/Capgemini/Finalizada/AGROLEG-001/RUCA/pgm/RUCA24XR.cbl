      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. RUCA24XR.
       AUTHOR.     HERIBERTO GIANNASI.

      *================================================================*
      *                          7 C O M M                             *
      *----------------------------------------------------------------*
      *    PROGRAMA....:  RUCA24XR                                     *
      *    ANALISTA....:  HERIBERTO GIANNASI - 7 COMM                  *
      *    DATA........:  MAR/2026                                     *
      *----------------------------------------------------------------*
      *    OBJETIVO....:  GERA RELATORIO COM CONTRATO, DECLARACAO,     *
      *                   VALIDADE, GRUPO E CHAVE CRUR (CPSSOA_JURID_  *
      *                   CONTR, CTPO_CONTR_NEGOC, NSEQ_CONTR_NEGOC)   *
      *                   MESMA LOGICA DO RUCA24XC SEM UPDATE          *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME           I/O           DESCRICAO        *
      *                RELATO              O           RELATORIO SAIDA *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    POOL7100 - MODULO PARA TRATAMENTO DE ERROS                  *
      *    BRAD7600 - MODULO PRA OBTER DATA/HORA DO SISTEMA            *
      *    CKRS0100 - MODULO PARA INICIAR E FINALIZAR CONEXAO DB2      *
      *    CKRS1000 - MODULO PARA INDICAR PROCESSAMENTO                 *
      *    CMRU2AFT - MODULO PARA OBTER CHAVE CRUR DO CONTRATO         *
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
                  ORGANIZATION         IS LINE SEQUENTIAL
                  FILE STATUS          IS WRK-FS-RELATO.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

       FD  RELATO
           RECORDING MODE F
           BLOCK CONTAINS 0 RECORDS.
       01  REG-RELATO                  PIC X(080).

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

       77  ACU-LIDOS                   PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVADOS                PIC  9(009) COMP-3  VALUE ZEROS.

       01  WRK-FIM-CSR01               PIC  X(001)         VALUE 'N'.

       77  WRK-MASCARA                 PIC  ZZZZZZZ.ZZZ.ZZ9 VALUE ' '.
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
          '* AREA DE FILE-STATUS *'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01  WRK-FS-RELATO               PIC  X(002)         VALUE SPACES.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01 FILLER                       PIC  X(032)         VALUE
          '* REGISTRO DE RELATORIO *'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01  WRK-REG-RELATO.
           05 WRK-REL-CCONTR           PIC  9(009).
           05 WRK-REL-FILL1            PIC  X(001)         VALUE ';'.
           05 WRK-REL-CDECLR           PIC  X(025).
           05 WRK-REL-FILL2            PIC  X(001)         VALUE ';'.
           05 WRK-REL-DVALDD           PIC  X(010).
           05 WRK-REL-FILL3            PIC  X(001)         VALUE ';'.
           05 WRK-REL-CGRP             PIC  9(002).
           05 WRK-REL-FILL4            PIC  X(001)         VALUE ';'.
           05 WRK-REL-CPSSOA           PIC  9(010).
           05 WRK-REL-FILL5            PIC  X(001)         VALUE ';'.
           05 WRK-REL-CTPO             PIC  9(003).
           05 WRK-REL-FILL6            PIC  X(001)         VALUE ';'.
           05 WRK-REL-NSEQ             PIC  9(010).

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

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01 FILLER                       PIC  X(032)         VALUE
          '* AREA DE CHECKPOINT / RESTART *'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  WRK-CKRS0100                 PIC  X(008)         VALUE
           'CKRS0100'.
       01  WRK-AREA-RESTART.
           05 FILLER                    PIC  X(010)         VALUE SPACES.

       COPY 'I#CKRS01'.

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
      *================================================================*
       PROCEDURE DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           CALL 'CKRS1000'.

           PERFORM 7000-INICIAR-CKRS0100.

           PERFORM 1000-INICIAR.

           PERFORM 1500-ABRIR-RELATO.

           PERFORM 2100-ABRIR-CSR01.

           PERFORM 2200-LER-CSR01.

           IF WRK-FIM-CSR01            EQUAL 'S'
              DISPLAY '******************RUCA24XR********************'
              DISPLAY '*                                            *'
              DISPLAY '*    NAO HA CONTRATO PRONAF PARA RELATORIO   *'
              DISPLAY '*                                            *'
              DISPLAY '******************RUCA24XR*******************'
              PERFORM 4000-FINALIZAR
           END-IF.

           PERFORM                     UNTIL WRK-FIM-CSR01
                                       EQUAL 'S'
              PERFORM 3000-PROCESSAR
              PERFORM 2200-LER-CSR01
           END-PERFORM.

           PERFORM 2300-FECHAR-CSR01.

           PERFORM 1600-FECHAR-RELATO.

           PERFORM 4000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           DISPLAY '******************RUCA24XR********************'
           DISPLAY '*                                            *'
           DISPLAY '*          INICIO DE PROCESSAMENTO           *'
           DISPLAY '*          GERACAO DE RELATORIO             *'
           DISPLAY '*                                            *'
           DISPLAY '******************RUCA24XR********************'.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1500-ABRIR-RELATO               SECTION.
      *----------------------------------------------------------------*

           OPEN OUTPUT RELATO.

           IF WRK-FS-RELATO            NOT EQUAL '00'
              MOVE 'RELATO'            TO WRK-NOME-ARQUIVO
              MOVE WRK-FS-RELATO       TO WRK-FILE-STATUS
              MOVE 'ABERTURA'          TO WRK-OPERACAO
              DISPLAY WRK-ERRO-POOL7100
              MOVE 12                  TO RETURN-CODE
              GOBACK
           END-IF.
           
           WRITE REG-RELATO.
           IF WRK-FS-RELATO            NOT EQUAL '00'
              MOVE 'RELATO'            TO WRK-NOME-ARQUIVO
              MOVE WRK-FS-RELATO       TO WRK-FILE-STATUS
              MOVE 'ESCRITA'           TO WRK-OPERACAO
              DISPLAY WRK-ERRO-POOL7100
           END-IF.

      *----------------------------------------------------------------*
       1500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1600-FECHAR-RELATO              SECTION.
      *----------------------------------------------------------------*

           CLOSE RELATO.

           IF WRK-FS-RELATO            NOT EQUAL '00'
              MOVE 'RELATO'            TO WRK-NOME-ARQUIVO
              MOVE WRK-FS-RELATO       TO WRK-FILE-STATUS
              MOVE 'FECHAMENTO'         TO WRK-OPERACAO
              DISPLAY WRK-ERRO-POOL7100
           END-IF.

      *----------------------------------------------------------------*
       1600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

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
               MOVE 'OPEN'             TO ERR-FUN-COMANDO
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
       2200-LER-CSR01                  SECTION.
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
              MOVE 'S'                 TO WRK-FIM-CSR01
           ELSE
              ADD 1                    TO ACU-LIDOS
           END-IF.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2300-FECHAR-CSR01               SECTION.
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

           PERFORM 3100-OBTER-CONTR-AGRO.

           IF CMRUWAFA-S-NSEQ-CONTR    NOT EQUAL ZEROS
              PERFORM 3200-OBTER-DAP-GRAVAR-RELATO
           END-IF.

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
                                       TO CMRUWAFA-E-CCONTR-LGADO.

           CALL WRK-CMRU2AFT           USING WRK-AREA-CMRU2AFT.

           IF  CMRUWAFA-COD-RETORNO    EQUAL 12
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  CMRUWAFA-COD-RETORNO    NOT EQUAL ZEROS
               DISPLAY 'CONTRATO NAO ENCONTRADO'
               DISPLAY ' *** CONTRATO = ' CCONTR OF RUFIB002
               MOVE ZEROS     TO CMRUWAFA-S-CPSSOA-JURID
                                 CMRUWAFA-S-CTPO-CONTR
                                 CMRUWAFA-S-NSEQ-CONTR
           END-IF.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3200-OBTER-DAP-GRAVAR-RELATO    SECTION.
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
              PERFORM 3220-GRAVAR-RELATO
           END-IF.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3210-TRATAR-NULL                SECTION.
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
       3220-GRAVAR-RELATO              SECTION.
      *----------------------------------------------------------------*
      *
      * MONTA E GRAVA REGISTRO: CONTRATO, DECLARACAO, VALIDADE,
      * GRUPO, CHAVE CRUR (CPSSOA_JURID, CTPO_CONTR, NSEQ_CONTR)
           MOVE CCONTR                 OF RUFIB002
                                       TO WRK-REL-CCONTR.
           MOVE CDECLR-AGCLT-FAMLR     OF CRURB0MB
                                       TO WRK-REL-CDECLR.
           MOVE DVALDD-DECLR-FAMLR     OF CRURB0MB
                                       TO WRK-REL-DVALDD.
           MOVE CGRP-ESPCL-RURAL       OF CRURB0MB
                                       TO WRK-REL-CGRP.
           MOVE CPSSOA-JURID-CONTR     OF CRURB0L7
                                       TO WRK-REL-CPSSOA.
           MOVE CTPO-CONTR-NEGOC       OF CRURB0L7
                                       TO WRK-REL-CTPO.
           MOVE NSEQ-CONTR-NEGOC       OF CRURB0L7
                                       TO WRK-REL-NSEQ.

           MOVE WRK-REG-RELATO         TO REG-RELATO.
           WRITE REG-RELATO.

           IF WRK-FS-RELATO            NOT EQUAL '00'
              MOVE 'RELATO'            TO WRK-NOME-ARQUIVO
              MOVE WRK-FS-RELATO       TO WRK-FILE-STATUS
              MOVE 'ESCRITA'           TO WRK-OPERACAO
              DISPLAY WRK-ERRO-POOL7100
              PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           ADD 1                       TO ACU-GRAVADOS.

      *----------------------------------------------------------------*
       3220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 4100-EMITIR-DISPLAY.

           PERFORM 7200-ENCERRAR-CKRS0100.

           GOBACK.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4100-EMITIR-DISPLAY             SECTION.
      *----------------------------------------------------------------*

           DISPLAY '******************* RUCA24XR *******************'.
           MOVE ACU-LIDOS              TO WRK-MASCARA.
           DISPLAY '*                                              *'.
           DISPLAY '* QTDE DE CONTRATOS LIDOS (CONTRATO_RURAL) = '
                                         WRK-MASCARA
                                                                '  *'.
           MOVE ACU-GRAVADOS            TO WRK-MASCARA.
           DISPLAY '*                                              *'.
           DISPLAY '* REGS GRAVADOS NO RELATORIO = ' WRK-MASCARA
                                                                '  *'.
           DISPLAY '*                                              *'.
           DISPLAY '******************* RUCA24XR *******************'.

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
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

           MOVE ZEROS                  TO ACU-LIDOS
                                          ACU-GRAVADOS.

      *----------------------------------------------------------------*
       7000-99-FIM.                    EXIT.
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

           MOVE 'CKRS0100-ERRO NAO CONTROLADO'
                                       TO CK01-TEXTO-MENSAGEM.
           DISPLAY '************** RUCA24XR **************'.
           DISPLAY '*'.
           DISPLAY '* CK01-CODIGO-RETORNO : ' CK01-CODIGO-RETORNO.
           DISPLAY '* CK01-CODIGO-MENSAGEM: ' CK01-CODIGO-MENSAGEM.
           DISPLAY '*'.
           DISPLAY '************** RUCA24XR **************'.
           MOVE 'APL'                  TO ERR-TIPO-ACESSO
           PERFORM 9999-PROCESSAR-ROTINA-ERRO.

      *----------------------------------------------------------------*
       7300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           MOVE 'RUCA24XR'             TO ERR-PGM.

           CALL 'POOL7100'             USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA.
           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
