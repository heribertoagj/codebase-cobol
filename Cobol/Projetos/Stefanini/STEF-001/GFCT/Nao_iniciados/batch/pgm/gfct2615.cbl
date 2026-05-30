      *===============================================================*
       IDENTIFICATION DIVISION.
      *===============================================================*

       PROGRAM-ID. GFCT2615.

      *================================================================*
      *                  SONDA PROCWORK INFORMATICA                    *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA.........: GFCT2615                                 *
      *    PROGRAMADOR......: FABRICA SOFTWARE        - SONDA PROCWORK *
      *                                                                *
      *    ANALISTA.........: CIBELE BARBOSA                           *
      *    LIDER PROJETO....: RODRIGO / MARCELO TEODORO                *
      *    GERENCIA / EQUIPE: GERENCIA-E / EQUIPE-1   - SONDA PROCWORK *
      *                                                                *
      *    GRUPO NEGOCIO....: DDS-GP50                                 *
      *    ANALISTA.........:                                          *
      *                                                                *
      *    DATA.............: MAR/2011                                 *
      *                                                                *
      *    PROJETO..........: EXTRATO ANUAL DE TARIFAS                 *
      *                                                                *
      *    OBJETIVO.........: EFETUAR A CONSISTENCIA FISICA DO ARQUIVO *
      *                       DO CONTA CORRENTE.                       *
      *                       CLONE DO PGM GFCT2608, COM TOTAIS.       *
      *                                                                *
      *    ARQUIVOS:                                                   *
      *      DDNAME                            INCLUDE/BOOK            *
      *        ENTTOTAL                          GFCTWAQ0              *
      *        EXTRAMOK                          GFCTWADJ              *
      *        EXTRASOK                          GFCTWADM              *
      *        EXTRANOK                          GFCTWADK              *
      *        SAITOTAL                          GFCTWAQ0              *
      *                                                                *
      *    BANCO DE DADOS...:                                          *
      *      DB2                                                       *
      *      TABLE                             INCLUDE/BOOK            *
      *        DB2PRD.TINFO_COMPL_EXTRT          GFCTB0N3              *
      *        DB2PRD.TPARM_CALC_EXTRT           GFCTB0N4              *
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

           SELECT ENTTOTAL ASSIGN TO UT-S-ENTTOTAL
                      FILE STATUS IS WRK-FS-ENTTOTAL.

           SELECT EXTRAMOK ASSIGN TO UT-S-EXTRAMOK
                      FILE STATUS IS WRK-FS-EXTRAMOK.

           SELECT EXTRASOK ASSIGN TO UT-S-EXTRASOK
                      FILE STATUS IS WRK-FS-EXTRASOK.

           SELECT EXTRANOK ASSIGN TO UT-S-EXTRANOK
                      FILE STATUS IS WRK-FS-EXTRANOK.

           SELECT SAITOTAL ASSIGN TO UT-S-SAITOTAL
                      FILE STATUS IS WRK-FS-SAITOTAL.

      *===============================================================*
       DATA DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       FILE SECTION.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      *   INPUT :     ORG. SEQUENCIAL   -   LRECL = 030               *
      *---------------------------------------------------------------*

       FD  ENTTOTAL
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-ENTTOTAL             PIC X(030).

      *---------------------------------------------------------------*
      *   INPUT :     ORG. SEQUENCIAL   -   LRECL = 131               *
      *---------------------------------------------------------------*

       FD  EXTRAMOK
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-EXTRAMOK             PIC X(131).

      *---------------------------------------------------------------*
      *   OUTPUT:     ORG. SEQUENCIAL   -   LRECL = 150               *
      *---------------------------------------------------------------*

       FD  EXTRASOK
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-EXTRASOK             PIC X(150).

      *---------------------------------------------------------------*
      *   OUTPUT:     ORG. SEQUENCIAL   -   LRECL = 171               *
      *---------------------------------------------------------------*

       FD  EXTRANOK
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-EXTRANOK             PIC X(171).

      *---------------------------------------------------------------*
      *   OUTPUT:     ORG. SEQUENCIAL   -   LRECL = 030               *
      *---------------------------------------------------------------*

       FD  SAITOTAL
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-SAITOTAL             PIC X(030).

      *---------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *---------------------------------------------------------------*

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
       77  FILLER  PIC X(32)  VALUE  '*  INICIO DA WORKING GFCT2615  *'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*

      *---------------------------------------------------------------*
       01  FILLER  PIC X(32)  VALUE  '*   AREAS AUXILIARES           *'.
      *---------------------------------------------------------------*

       01  WRK-SQLCODE-CSR01           PIC S9(09) COMP     VALUE ZEROS.
       01  WRK-INCONSIS                PIC  X(001)         VALUE SPACES.

       01  WRK-QTDE-COMP              PIC  9(005)    COMP-3 VALUE ZEROS.
       01  WRK-VALOR-COMP             PIC  9(012)V99 COMP-3 VALUE ZEROS.
       01  WRK-VLR-LANC-COMP          PIC  9(012)V99 COMP-3 VALUE ZEROS.

       01  WRK-DATA-DB2.
           05  WRK-DIA-DB2             PIC 9(02)           VALUE ZEROS.
           05  FILLER                  PIC X(01)           VALUE '.'.
           05  WRK-MES-DB2             PIC 9(02)           VALUE ZEROS.
           05  FILLER                  PIC X(01)           VALUE '.'.
           05  WRK-ANO-DB2             PIC 9(04)           VALUE ZEROS.

      *---------------------------------------------------------------*
       01  FILLER  PIC X(32)  VALUE  '*   AREAS DE FILE STATUS       *'.
      *---------------------------------------------------------------*

       77  WRK-BATCH                   PIC X(08) VALUE 'BATCH   '.

       77  WRK-FS-EXTRAMOK             PIC X(02) VALUE SPACES.
       77  WRK-FS-EXTRASOK             PIC X(02) VALUE SPACES.
       77  WRK-FS-EXTRANOK             PIC X(02) VALUE SPACES.
       77  WRK-FS-ENTTOTAL             PIC X(02) VALUE SPACES.
       77  WRK-FS-SAITOTAL             PIC X(02) VALUE SPACES.

       77  WRK-OPERACAO                PIC X(13) VALUE SPACES.
       77  WRK-ABERTURA                PIC X(13) VALUE 'NA ABERTURA'.
       77  WRK-LEITURA                 PIC X(13) VALUE 'NA LEITURA'.
       77  WRK-GRAVACAO                PIC X(13) VALUE 'NA GRAVACAO'.
       77  WRK-FECHAMENTO              PIC X(13) VALUE 'NO FECHAMENTO'.

       77  WRK-TEM-CORREP              PIC X(01) VALUE 'N'.

       77  WRK-FIM-ENTTOTAL            PIC X(01) VALUE 'N'.

       77  WRK-MES-REPROC              PIC 9(002)          VALUE ZEROS.
       77  WRK-ANO-REPROC              PIC 9(004)          VALUE ZEROS.

      *---------------------------------------------------------------*
       77  FILLER  PIC X(32)  VALUE  '* ACUMULADORES                 *'.
      *---------------------------------------------------------------*

       77  ACU-LIDOS-ENTTOTAL          PIC 9(09) COMP-3    VALUE ZEROS.
       77  ACU-LIDOS-EXTRAMOK          PIC 9(09) COMP-3    VALUE ZEROS.
       77  ACU-GRAVA-EXTRASOK          PIC 9(09) COMP-3    VALUE ZEROS.
       77  ACU-GRAVA-EXTRANOK          PIC 9(09) COMP-3    VALUE ZEROS.
       77  ACU-GRAVA-SAITOTAL          PIC 9(09) COMP-3    VALUE ZEROS.
       77  ACU-TNOK-MESANO             PIC 9(11) COMP-3    VALUE ZEROS.
       77  ACU-TOK-MESANO              PIC 9(11) COMP-3    VALUE ZEROS.
       77  ACU-QTDE-REPROC             PIC 9(11) COMP-3    VALUE ZEROS.

      *---------------------------------------------------------------*
       01  FILLER  PIC X(32)  VALUE  '* MASCARAS DE EDICAO DE TOTAIS *'.
      *---------------------------------------------------------------*

       01  FILLER.
           05  WRK-MASK-LIDOS-ENTTOTAL PIC ZZZ.ZZZ.ZZ9     VALUE ZEROS.
           05  WRK-MASK-LIDOS-EXTRAMOK PIC ZZZ.ZZZ.ZZ9     VALUE ZEROS.
           05  WRK-MASK-GRAVA-EXTRASOK PIC ZZZ.ZZZ.ZZ9     VALUE ZEROS.
           05  WRK-MASK-GRAVA-EXTRANOK PIC ZZZ.ZZZ.ZZ9     VALUE ZEROS.
           05  WRK-MASK-GRAVA-SAITOTAL PIC ZZZ.ZZZ.ZZ9     VALUE ZEROS.

       01  WRK-CHAVE-ANT              PIC 9(006)           VALUE ZEROS.
       01  FILLER                     REDEFINES WRK-CHAVE-ANT.
           05  WRK-MES-ANT            PIC 9(002).
           05  WRK-ANO-ANT            PIC 9(004).

       01  WRK-CHAVE-ATU              PIC 9(006)           VALUE ZEROS.
       01  FILLER                     REDEFINES WRK-CHAVE-ATU.
           05  WRK-MES-ATU            PIC 9(002).
           05  WRK-ANO-ATU            PIC 9(004).

       01  WRK-CHAVE-EXTRAMOK         PIC 9(006)           VALUE ZEROS.
       01  FILLER                     REDEFINES WRK-CHAVE-EXTRAMOK.
           05  WRK-MES-EXTRAMOK       PIC 9(002).
           05  WRK-ANO-EXTRAMOK       PIC 9(004).

       01  WRK-CHAVE-ENTTOTAL         PIC 9(006)           VALUE ZEROS.
       01  FILLER                     REDEFINES WRK-CHAVE-ENTTOTAL.
           05  WRK-MES-ENTTOTAL       PIC 9(002).
           05  WRK-ANO-ENTTOTAL       PIC 9(004).


      *---------------------------------------------------------------*
       01  FILLER    PIC X(32)   VALUE '*   AREAS LAYOUT EXTRAMOK  *'.
      *---------------------------------------------------------------*

       COPY 'GFCTWADJ'.

      *---------------------------------------------------------------*
       01  FILLER    PIC X(32)   VALUE '*   AREAS LAYOUT EXTRASOK   *'.
      *---------------------------------------------------------------*

       COPY 'GFCTWADM'.

      *---------------------------------------------------------------*
       01  FILLER    PIC X(32)   VALUE '*   AREAS LAYOUT EXTRANOK   *'.
      *---------------------------------------------------------------*

       COPY 'GFCTWADK'.

      *---------------------------------------------------------------*
       01  FILLER    PIC X(32)   VALUE '*   AREAS LAYOUT TOTAL      *'.
      *---------------------------------------------------------------*

       COPY 'GFCTWAQ0'.

      *---------------------------------------------------------------*
       01  FILLER    PIC X(32)   VALUE '*   AREAS TRATAMENTO ERRO    *'.
      *---------------------------------------------------------------*

       COPY 'I#BRAD7C'.

       01  WRK-TEXTO.
           05  FILLER                  PIC X(05) VALUE 'ERRO'.
           05  WRK-OPERACAO-TXT        PIC X(13) VALUE SPACES.
           05  FILLER                  PIC X(12) VALUE ' DO ARQUIVO '.
           05  WRK-ARQ-TXT             PIC X(08) VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER  PIC X(32)  VALUE  '*  AREAS DB2                   *'.
      *---------------------------------------------------------------*

           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0N3
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0N4
           END-EXEC.

      *---------------------------------------------------------------*
      *                  DECLARE DO CURSOR CSR01-GFCTB0N3
      *---------------------------------------------------------------*

           EXEC SQL
               DECLARE CSR01-GFCTB0N3 CURSOR FOR

               SELECT
                   NSEQ_FILTR_SELEC,
                   DINIC_VGCIA_REG,
                   DFIM_VGCIA_REG,
                   CINDCD_EXTRT_GRP,
                   CINDCD_CRITR_LCTO
               FROM
                   DB2PRD.TINFO_COMPL_EXTRT
               WHERE
                   CBCO_CCREN            = :GFCTB0N3.CBCO-CCREN
               AND CIDTFD_LCTO           = :GFCTB0N3.CIDTFD-LCTO
               AND CIDTFD_SNAL_LCTO      = :GFCTB0N3.CIDTFD-SNAL-LCTO
               AND CTPO_CTA_MOVTO        = :GFCTB0N3.CTPO-CTA-MOVTO
               AND CPSSOA_SERVC_TARIF    = :GFCTB0N3.CPSSOA-SERVC-TARIF
               AND CINDCD_SEGDA_LIN      = :GFCTB0N3.CINDCD-SEGDA-LIN
               AND CIDTFD_SIT_PARM       = :GFCTB0N3.CIDTFD-SIT-PARM
               AND RFILTR_SELEC_EXTRT    = :GFCTB0N3.RFILTR-SELEC-EXTRT
           END-EXEC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
       01  FILLER  PIC X(32)  VALUE  '*  FIM  DA WORKING   GFCT2615  *'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*

      *===============================================================*
       PROCEDURE DIVISION.
      *===============================================================*

      *===============================================================*
      *    ROTINA INICIAR
      *===============================================================*
       0000-INICIAR SECTION.
      *---------------------------------------------------------------*

           CALL 'BRAD1050'.

           OPEN    INPUT   EXTRAMOK
                           ENTTOTAL
                   OUTPUT  EXTRASOK
                           EXTRANOK
                           SAITOTAL.

           MOVE    WRK-ABERTURA        TO  WRK-OPERACAO.
           PERFORM 1000-TESTAR-FILE-STATUS.

           PERFORM 2000-ROTINA-INICIAL.

           IF  ACU-LIDOS-EXTRAMOK      EQUAL ZEROS
               GO                      TO  0000-FINALIZAR
           END-IF.

      * ST25X6* Não aplicar checagem "NOT NUMERIC" ao CPF/CNPJ;
      *         consistir apenas BRANCO e ZEROS antes de gravar EXTRASOK.
           IF  WADJ-CPF-CNPJ          EQUAL SPACES
               MOVE GFCTWADJ-REG      TO  GFCTWADK-REG
               MOVE 'CPF/CNPJ EM BRANCO'
                                       TO  WADK-DESC-ERRO
               PERFORM 3300-GRAVAR-EXTRANOK
               GO              TO  3200-99-FIM
           ELSE
               IF  WADJ-CPF-CNPJ      EQUAL ZEROS
                   MOVE GFCTWADJ-REG  TO  GFCTWADK-REG
                   MOVE 'CPF/CNPJ ZERADO'
                                       TO  WADK-DESC-ERRO
                   PERFORM 3300-GRAVAR-EXTRANOK
                   GO          TO  3200-99-FIM
               END-IF
           END-IF.

           PERFORM 3000-PROCESSA
             UNTIL WRK-FS-EXTRAMOK     EQUAL '10'.

           PERFORM 4000-ROTINA-FINAL.


      *===============*
      * LABEL FINALIZAR
      *===============*
       0000-FINALIZAR.
      *---------------*

           CLOSE   ENTTOTAL
                   EXTRAMOK
                   EXTRASOK
                   EXTRANOK
                   SAITOTAL.

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

           PERFORM 1100-TESTAR-FS-EXTRAMOK.

           PERFORM 1200-TESTAR-FS-EXTRASOK.

           PERFORM 1300-TESTAR-FS-EXTRANOK.

           PERFORM 1400-TESTAR-FS-ENTTOTAL.

           PERFORM 1500-TESTAR-FS-SAITOTAL.

      *---------------------------------------------------------------*
       1000-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA TESTAR-FS-EXTRAMOK
      *===============================================================*
       1100-TESTAR-FS-EXTRAMOK SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-EXTRAMOK         NOT EQUAL '00'
               DISPLAY '************** GFCT2615 *************'
               DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
               DISPLAY '*              EXTRAMOK             *'
               DISPLAY '*         FILE STATUS =  ' WRK-FS-EXTRAMOK
                                                  '         *'
               DISPLAY '*         PROGRAMA ENCERRADO        *'
               DISPLAY '************** GFCT2615 *************'
               MOVE WRK-OPERACAO       TO  WRK-OPERACAO-TXT
               MOVE 'EXTRAMOK'         TO  WRK-ARQ-TXT
               MOVE WRK-TEXTO          TO  ERR-TEXTO
               MOVE 'APL'              TO  ERR-TIPO-ACESSO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1100-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA TESTAR-FS-EXTRASOK
      *===============================================================*
       1200-TESTAR-FS-EXTRASOK SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-EXTRASOK         NOT EQUAL '00'
               DISPLAY '************** GFCT2615 *************'
               DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
               DISPLAY '*              EXTRASOK             *'
               DISPLAY '*         FILE STATUS =  ' WRK-FS-EXTRASOK
                                                  '         *'
               DISPLAY '*         PROGRAMA ENCERRADO        *'
               DISPLAY '************** GFCT2615 *************'
               MOVE WRK-OPERACAO       TO  WRK-OPERACAO-TXT
               MOVE 'EXTRASOK'         TO  WRK-ARQ-TXT
               MOVE WRK-TEXTO          TO  ERR-TEXTO
               MOVE 'APL'              TO  ERR-TIPO-ACESSO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1200-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA TESTAR-FS-EXTRANOK
      *===============================================================*
       1300-TESTAR-FS-EXTRANOK SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-EXTRANOK         NOT EQUAL '00'
               DISPLAY '************** GFCT2615 *************'
               DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
               DISPLAY '*               EXTRANOK            *'
               DISPLAY '*         FILE STATUS =  ' WRK-FS-EXTRANOK
                                                  '         *'
               DISPLAY '*         PROGRAMA ENCERRADO        *'
               DISPLAY '************** GFCT2615 *************'
               MOVE WRK-OPERACAO       TO  WRK-OPERACAO-TXT
               MOVE 'EXTRANOK'         TO  WRK-ARQ-TXT
               MOVE WRK-TEXTO          TO  ERR-TEXTO
               MOVE 'APL'              TO  ERR-TIPO-ACESSO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1300-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA TESTAR-FS-ENTTOTAL
      *===============================================================*
       1400-TESTAR-FS-ENTTOTAL SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-ENTTOTAL         NOT EQUAL '00'
               DISPLAY '************** GFCT2615 *************'
               DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
               DISPLAY '*               ENTTOTAL            *'
               DISPLAY '*         FILE STATUS =  ' WRK-FS-ENTTOTAL
                                                  '         *'
               DISPLAY '*         PROGRAMA ENCERRADO        *'
               DISPLAY '************** GFCT2615 *************'
               MOVE WRK-OPERACAO       TO  WRK-OPERACAO-TXT
               MOVE 'ENTTOTAL'         TO  WRK-ARQ-TXT
               MOVE WRK-TEXTO          TO  ERR-TEXTO
               MOVE 'APL'              TO  ERR-TIPO-ACESSO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1400-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA TESTAR-FS-SAITOTAL
      *===============================================================*
       1500-TESTAR-FS-SAITOTAL SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-SAITOTAL         NOT EQUAL '00'
               DISPLAY '************** GFCT2615 *************'
               DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
               DISPLAY '*               SAITOTAL            *'
               DISPLAY '*         FILE STATUS =  ' WRK-FS-SAITOTAL
                                                  '         *'
               DISPLAY '*         PROGRAMA ENCERRADO        *'
               DISPLAY '************** GFCT2615 *************'
               MOVE WRK-OPERACAO       TO  WRK-OPERACAO-TXT
               MOVE 'SAITOTAL'         TO  WRK-ARQ-TXT
               MOVE WRK-TEXTO          TO  ERR-TEXTO
               MOVE 'APL'              TO  ERR-TIPO-ACESSO
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

           PERFORM 2100-LER-EXTRAMOK.

           IF  ACU-LIDOS-EXTRAMOK       EQUAL   ZEROS
               DISPLAY '************* GFCT2615 *************'
               DISPLAY '*                                  *'
               DISPLAY '*    ARQUIVO EXTRAMOK ESTA VAZIO   *'
               DISPLAY '*                                  *'
               DISPLAY '*        PROGRAMA ENCERRADO        *'
               DISPLAY '*                                  *'
               DISPLAY '************* GFCT2615 *************'
           END-IF.

      *---------------------------------------------------------------*
       2000-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA LER-EXTRAMOK
      *===============================================================*
       2100-LER-EXTRAMOK  SECTION.
      *---------------------------------------------------------------*

           READ EXTRAMOK  INTO GFCTWADJ-REG.

           IF  WRK-FS-EXTRAMOK         EQUAL  '10'
               GO                      TO  2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO  WRK-OPERACAO.
           PERFORM 1100-TESTAR-FS-EXTRAMOK.

           ADD  1                      TO  ACU-LIDOS-EXTRAMOK.

           IF ACU-LIDOS-EXTRAMOK  EQUAL 1
              MOVE ZEROS                 TO ACU-TOK-MESANO
                                            ACU-TNOK-MESANO
              MOVE WADJ-DT-LANCTO-AAAA   TO WRK-ANO-ANT
                                            WRK-ANO-REPROC
                                            WRK-ANO-ATU
                                            WRK-ANO-EXTRAMOK
              MOVE WADJ-DT-LANCTO-MM     TO WRK-MES-ANT
                                            WRK-MES-REPROC
                                            WRK-MES-ATU
                                            WRK-MES-EXTRAMOK
              PERFORM 2200-LER-ENTTOTAL
              PERFORM 3050-TRATAR-QUEBRA
           ELSE
              MOVE WRK-CHAVE-ATU       TO WRK-CHAVE-ANT
              MOVE WADJ-DT-LANCTO-MM   TO WRK-MES-ATU
                                          WRK-MES-EXTRAMOK
              MOVE WADJ-DT-LANCTO-AAAA TO WRK-ANO-ATU
                                          WRK-ANO-EXTRAMOK
           END-IF.

      *---------------------------------------------------------------*
       2100-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA LER-ENTTOTAL
      *===============================================================*
       2200-LER-ENTTOTAL  SECTION.
      *---------------------------------------------------------------*

           READ ENTTOTAL  INTO GFCTWAQ0-REG.

           IF  WRK-FS-ENTTOTAL         EQUAL  '10'
               MOVE 999999             TO  WRK-CHAVE-ENTTOTAL
               MOVE 'S'                TO  WRK-FIM-ENTTOTAL
               GO TO 2200-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO  WRK-OPERACAO.
           PERFORM 1400-TESTAR-FS-ENTTOTAL.

           ADD  1                      TO  ACU-LIDOS-ENTTOTAL.

           MOVE WAQ0-ANO-REPROC        TO WRK-ANO-ENTTOTAL.
           MOVE WAQ0-MES-REPROC        TO WRK-MES-ENTTOTAL.

      *---------------------------------------------------------------*
       2200-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA PROCESSA
      *===============================================================*
       3000-PROCESSA SECTION.
      *---------------------------------------------------------------*

           IF WRK-CHAVE-ATU         NOT EQUAL WRK-CHAVE-ANT
              MOVE 'N'              TO  WRK-TEM-CORREP
              MOVE WRK-MES-REPROC   TO  WAQ0-MES-REPROC
              MOVE WRK-ANO-REPROC   TO  WAQ0-ANO-REPROC
              MOVE ACU-TNOK-MESANO  TO  WAQ0-QTDE-REJEIT
              MOVE ACU-TOK-MESANO   TO  WAQ0-QTDE-REPROC
              PERFORM 3400-GRAVAR-ARQTOTAL
              PERFORM 2200-LER-ENTTOTAL
              PERFORM 3050-TRATAR-QUEBRA
           END-IF.

           MOVE 1                      TO CINDCD-SEGDA-LIN   OF GFCTB0N3
           MOVE WADJ-HIST-CPL          TO RFILTR-SELEC-EXTRT OF GFCTB0N3
           PERFORM 3100-ACESSAR-GFCTB0N3

           IF  WRK-SQLCODE-CSR01       EQUAL ZEROS
               PERFORM 3200-GRAVAR-EXTRASOK
           ELSE
               MOVE 2                  TO CINDCD-SEGDA-LIN   OF GFCTB0N3
               MOVE WADJ-DESC-LANCAMENTO
                                       TO RFILTR-SELEC-EXTRT OF GFCTB0N3
               PERFORM 3100-ACESSAR-GFCTB0N3

               IF  WRK-SQLCODE-CSR01   EQUAL ZEROS
                   PERFORM 3200-GRAVAR-EXTRASOK
               ELSE
                   MOVE GFCTWADJ-REG   TO  GFCTWADK-REG
                   MOVE 'NAO ENCONTRADO SEGUNDA LINHA'
                                       TO  WADK-DESC-ERRO
                   PERFORM 3300-GRAVAR-EXTRANOK
               END-IF
           END-IF.

           PERFORM 2100-LER-EXTRAMOK.

      *---------------------------------------------------------------*
       3000-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA PROCESSA
      *===============================================================*
       3050-TRATAR-QUEBRA SECTION.
      *---------------------------------------------------------------*

           IF WRK-CHAVE-EXTRAMOK       EQUAL WRK-CHAVE-ENTTOTAL
              MOVE WAQ0-QTDE-REPROC       TO  ACU-TOK-MESANO
                                              ACU-QTDE-REPROC
              MOVE WAQ0-QTDE-REJEIT       TO  ACU-TNOK-MESANO
              MOVE WAQ0-MES-REPROC        TO  WRK-MES-REPROC
              MOVE WAQ0-ANO-REPROC        TO  WRK-ANO-REPROC
              MOVE 'S'                    TO  WRK-TEM-CORREP
           ELSE
              IF WRK-CHAVE-EXTRAMOK    LESS WRK-CHAVE-ENTTOTAL
                 MOVE ZEROS               TO  ACU-TOK-MESANO
                                              ACU-TNOK-MESANO
                 MOVE WADJ-DT-LANCTO-MM   TO  WRK-MES-REPROC
                 MOVE WADJ-DT-LANCTO-AAAA TO  WRK-ANO-REPROC
              ELSE
                 PERFORM 3400-GRAVAR-ARQTOTAL
                 PERFORM 2200-LER-ENTTOTAL
                 GO TO 3050-TRATAR-QUEBRA
              END-IF
           END-IF.

      *---------------------------------------------------------------*
       3050-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA ACESSAR-GFCTB0N3
      *===============================================================*
       3100-ACESSAR-GFCTB0N3 SECTION.
      *---------------------------------------------------------------*

           PERFORM 3110-OPEN-CSR01-GFCTB0N3.

           PERFORM 3120-FETCH-CSR01-GFCTB0N3.

           PERFORM 3130-CLOSE-CSR01-GFCTB0N3.

      *---------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA ABRIR CURSOR
      *===============================================================*
       3110-OPEN-CSR01-GFCTB0N3 SECTION.
      *---------------------------------------------------------------*

           MOVE 237                    TO  CBCO-CCREN      OF GFCTB0N3
           MOVE WADJ-COD-LANCAMENTO    TO  CIDTFD-LCTO     OF GFCTB0N3
           MOVE 1                      TO  CIDTFD-SIT-PARM OF GFCTB0N3

           IF  WADJ-TIPO-REGISTRO      EQUAL  'E'
               MOVE 2                  TO  CIDTFD-SNAL-LCTO OF GFCTB0N3
           ELSE
               MOVE 1                  TO  CIDTFD-SNAL-LCTO OF GFCTB0N3
           END-IF.

           IF  WADJ-RAZAO              EQUAL '010051'
               MOVE 2                  TO  CTPO-CTA-MOVTO OF GFCTB0N3
           ELSE
               MOVE 1                  TO  CTPO-CTA-MOVTO OF GFCTB0N3
           END-IF.

           IF  WADJ-FILIAL             EQUAL ZEROS
               MOVE '1'                TO CPSSOA-SERVC-TARIF OF GFCTB0N3
           ELSE
               MOVE '2'                TO CPSSOA-SERVC-TARIF OF GFCTB0N3
           END-IF.

           EXEC SQL
                OPEN CSR01-GFCTB0N3
           END-EXEC.

           IF ( SQLCODE NOT EQUAL ZEROS ) OR
              ( SQLWARN0    EQUAL 'W' )
                MOVE 'DB2'                TO  ERR-TIPO-ACESSO
                MOVE 'TINFO_COMPL_EXTRT'  TO  ERR-DBD-TAB
                MOVE 'OPEN'               TO  ERR-FUN-COMANDO
                MOVE  SQLCODE             TO  ERR-SQL-CODE
                MOVE '0001'               TO  ERR-LOCAL
                MOVE  SPACES              TO  ERR-SEGM
                PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       3110-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA LER CURSOR
      *===============================================================*
       3120-FETCH-CSR01-GFCTB0N3 SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
                FETCH CSR01-GFCTB0N3
                 INTO :GFCTB0N3.NSEQ-FILTR-SELEC,
                      :GFCTB0N3.DINIC-VGCIA-REG,
                      :GFCTB0N3.DFIM-VGCIA-REG,
                      :GFCTB0N3.CINDCD-EXTRT-GRP,
                      :GFCTB0N3.CINDCD-CRITR-LCTO
           END-EXEC.

           IF ( SQLCODE NOT EQUAL ZEROS AND +100 ) OR
              ( SQLWARN0    EQUAL 'W' )
                MOVE 'DB2'                TO  ERR-TIPO-ACESSO
                MOVE 'TINFO_COMPL_EXTRT'  TO  ERR-DBD-TAB
                MOVE 'FETCH '             TO  ERR-FUN-COMANDO
                MOVE  SQLCODE             TO  ERR-SQL-CODE
                MOVE '0002'               TO  ERR-LOCAL
                MOVE  SPACES              TO  ERR-SEGM
                PERFORM 9999-ROTINA-ERRO
           END-IF.

           MOVE SQLCODE                TO  WRK-SQLCODE-CSR01.

      *---------------------------------------------------------------*
       3120-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA FECHAR CURSOR
      *===============================================================*
       3130-CLOSE-CSR01-GFCTB0N3 SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR01-GFCTB0N3
           END-EXEC.

           IF ( SQLCODE NOT EQUAL ZEROS  ) OR
              ( SQLWARN0    EQUAL 'W' )
                MOVE 'DB2'                TO  ERR-TIPO-ACESSO
                MOVE 'TINFO_COMPL_EXTRT'  TO  ERR-DBD-TAB
                MOVE 'CLOSE '             TO  ERR-FUN-COMANDO
                MOVE  SQLCODE             TO  ERR-SQL-CODE
                MOVE '0003'               TO  ERR-LOCAL
                MOVE  SPACES              TO  ERR-SEGM
                PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       3130-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA GRAVAR-EXTRASOK
      *===============================================================*
       3200-GRAVAR-EXTRASOK SECTION.
      *---------------------------------------------------------------*

           IF  WADJ-TIPO-REGISTRO        EQUAL 'E'
               MOVE 4                    TO  WADM-IDENT
           ELSE
               IF  CINDCD-EXTRT-GRP      EQUAL 1
                   MOVE 1                TO  WADM-IDENT
               ELSE
                   IF  CINDCD-EXTRT-GRP  EQUAL 2
                       MOVE 2            TO  WADM-IDENT
                   ELSE
                       MOVE 3            TO  WADM-IDENT
                   END-IF

                   MOVE SPACES         TO  WRK-INCONSIS
                   PERFORM 3210-TRATAR-QTDE

                   IF  WRK-INCONSIS    EQUAL 'S'
                       MOVE GFCTWADJ-REG
                                       TO  GFCTWADK-REG
                       MOVE 'NAO ENCONTRADO VALOR UNITARIO'
                                       TO  WADK-DESC-ERRO
                       PERFORM 3300-GRAVAR-EXTRANOK
                       GO              TO  3200-99-FIM
                   END-IF
               END-IF
           END-IF.

      * ST25X6* Não aplicar checagem "NOT NUMERIC" ao CPF/CNPJ;
      *         consistir apenas BRANCO e ZEROS antes de gravar EXTRASOK.
           IF  WADJ-CPF-CNPJ          EQUAL SPACES
               MOVE GFCTWADJ-REG      TO  GFCTWADK-REG
               MOVE 'CPF/CNPJ EM BRANCO'
                                       TO  WADK-DESC-ERRO
               PERFORM 3300-GRAVAR-EXTRANOK
               GO              TO  3200-99-FIM
           ELSE
               IF  WADJ-CPF-CNPJ      EQUAL ZEROS
                   MOVE GFCTWADJ-REG  TO  GFCTWADK-REG
                   MOVE 'CPF/CNPJ ZERADO'
                                       TO  WADK-DESC-ERRO
                   PERFORM 3300-GRAVAR-EXTRANOK
                   GO          TO  3200-99-FIM
               END-IF
           END-IF.

           MOVE WADJ-TIPO-REGISTRO     TO  WADM-TIPO-REGISTRO
           MOVE WADJ-COD-AGENCIA       TO  WADM-COD-AGENCIA
           MOVE WADJ-CPF-CNPJ          TO  WADM-CPF-CNPJ
           MOVE WADJ-RAZAO             TO  WADM-RAZAO
           MOVE WADJ-NRO-CONTA         TO  WADM-NRO-CONTA
           MOVE WADJ-NUMERO-DOCUMENTO  TO  WADM-NUMERO-DOCUMENTO
           MOVE WADJ-COD-LANCAMENTO    TO  WADM-COD-LANCAMENTO
           MOVE WADJ-DATA-LANCAMENTO   TO  WADM-DATA-LANCAMENTO
           MOVE WADJ-VLR-LANCAMENTO    TO  WADM-VLR-LANCAMENTO
           MOVE RFILTR-SELEC-EXTRT     OF  GFCTB0N3
                                       TO  WADM-SEGUNDA-LINHA

           MOVE WRK-GRAVACAO           TO  WRK-OPERACAO

           WRITE FD-REG-EXTRASOK FROM GFCTWADM-REG
           PERFORM 1200-TESTAR-FS-EXTRASOK

           ADD 1                       TO  ACU-GRAVA-EXTRASOK.

           IF WRK-TEM-CORREP = 'N'
              ADD   1                  TO  ACU-TOK-MESANO
           END-IF.

      *---------------------------------------------------------------*
       3200-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA TRATAR-QTDE
      *===============================================================*
       3210-TRATAR-QTDE  SECTION.
      *---------------------------------------------------------------*

           IF  CINDCD-CRITR-LCTO       EQUAL 1
               MOVE WADJ-NUMERO-DOCUMENTO(3:5)
                                       TO  WADM-QTDE-TAR
           ELSE
               IF  CINDCD-CRITR-LCTO   EQUAL 2
                   PERFORM 3211-ACESSAR-GFCTB0N4

                   IF  SQLCODE         EQUAL +100
                       MOVE 'S'        TO  WRK-INCONSIS
                   ELSE
                       MOVE WADJ-VLR-LANCAMENTO
                                       TO WRK-VLR-LANC-COMP
                       MOVE VTARIF-DEB-MOVTO OF GFCTB0N4
                                       TO  WRK-VALOR-COMP
                       COMPUTE WRK-QTDE-COMP ROUNDED =
                                   WRK-VLR-LANC-COMP / WRK-VALOR-COMP
                       MOVE WRK-QTDE-COMP
                                       TO  WADM-QTDE-TAR
                   END-IF
               ELSE
                   MOVE 1              TO  WADM-QTDE-TAR
               END-IF
           END-IF.


      *---------------------------------------------------------------*
       3210-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA ACESSAR-GFCTB0N4
      *===============================================================*
       3211-ACESSAR-GFCTB0N4  SECTION.
      *---------------------------------------------------------------*

           MOVE CBCO-CCREN             OF GFCTB0N3
             TO CBCO-CCREN             OF GFCTB0N4
           MOVE CIDTFD-LCTO            OF GFCTB0N3
             TO CIDTFD-LCTO            OF GFCTB0N4
           MOVE CIDTFD-SNAL-LCTO       OF GFCTB0N3
             TO CIDTFD-SNAL-LCTO       OF GFCTB0N4
           MOVE CTPO-CTA-MOVTO         OF GFCTB0N3
             TO CTPO-CTA-MOVTO         OF GFCTB0N4
           MOVE CPSSOA-SERVC-TARIF     OF GFCTB0N3
             TO CPSSOA-SERVC-TARIF     OF GFCTB0N4
           MOVE CINDCD-SEGDA-LIN       OF GFCTB0N3
             TO CINDCD-SEGDA-LIN       OF GFCTB0N4
           MOVE DINIC-VGCIA-REG        OF GFCTB0N3
             TO DINIC-VGCIA-REG        OF GFCTB0N4
           MOVE NSEQ-FILTR-SELEC       OF GFCTB0N3
             TO NSEQ-FILTR-SELEC       OF GFCTB0N4

           MOVE WADJ-DT-LANCTO-DD      TO WRK-DIA-DB2
           MOVE WADJ-DT-LANCTO-MM      TO WRK-MES-DB2
           MOVE WADJ-DT-LANCTO-AAAA    TO WRK-ANO-DB2
           MOVE WRK-DATA-DB2           TO DINIC-VGCIA-VLR    OF GFCTB0N4
                                          DFIM-VGCIA-VLR     OF GFCTB0N4

           EXEC SQL
               SELECT  VTARIF_DEB_MOVTO
                 INTO :GFCTB0N4.VTARIF-DEB-MOVTO

                 FROM DB2PRD.TPARM_CALC_EXTRT

               WHERE  CBCO_CCREN         = :GFCTB0N4.CBCO-CCREN
                 AND  CIDTFD_LCTO        = :GFCTB0N4.CIDTFD-LCTO
                 AND  CIDTFD_SNAL_LCTO   = :GFCTB0N4.CIDTFD-SNAL-LCTO
                 AND  CTPO_CTA_MOVTO     = :GFCTB0N4.CTPO-CTA-MOVTO
                 AND  CPSSOA_SERVC_TARIF = :GFCTB0N4.CPSSOA-SERVC-TARIF
                 AND  CINDCD_SEGDA_LIN   = :GFCTB0N4.CINDCD-SEGDA-LIN
                 AND  DINIC_VGCIA_REG    = :GFCTB0N4.DINIC-VGCIA-REG
                 AND  NSEQ_FILTR_SELEC   = :GFCTB0N4.NSEQ-FILTR-SELEC
                 AND  DINIC_VGCIA_VLR   <= :GFCTB0N4.DINIC-VGCIA-VLR
                 AND  DFIM_VGCIA_VLR    >= :GFCTB0N4.DFIM-VGCIA-VLR
           END-EXEC.

           IF ( SQLCODE NOT EQUAL ZEROS AND +100 AND -811 ) OR
              ( SQLWARN0    EQUAL 'W' )
                MOVE 'DB2'                TO  ERR-TIPO-ACESSO
                MOVE 'TPARM_CALC_EXTRT'   TO  ERR-DBD-TAB
                MOVE 'SELECT'             TO  ERR-FUN-COMANDO
                MOVE  SQLCODE             TO  ERR-SQL-CODE
                MOVE '0004'               TO  ERR-LOCAL
                MOVE  SPACES              TO  ERR-SEGM
                PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       3211-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA GRAVAR-EXTRANOK
      *===============================================================*
       3300-GRAVAR-EXTRANOK  SECTION.
      *---------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO  WRK-OPERACAO.

           WRITE FD-REG-EXTRANOK FROM GFCTWADK-REG
           PERFORM 1300-TESTAR-FS-EXTRANOK.

           ADD 1                       TO  ACU-GRAVA-EXTRANOK.

           IF WRK-TEM-CORREP = 'N'
              ADD   1                  TO  ACU-TNOK-MESANO
           ELSE
              COMPUTE ACU-TOK-MESANO = ACU-QTDE-REPROC - 1
              ADD   1                  TO  ACU-TNOK-MESANO
              MOVE    ACU-TOK-MESANO   TO  ACU-QTDE-REPROC
           END-IF.

      *---------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA GRAVAR-SAITOTAL
      *===============================================================*
       3400-GRAVAR-ARQTOTAL  SECTION.
      *---------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO  WRK-OPERACAO.

           WRITE FD-REG-SAITOTAL FROM GFCTWAQ0-REG.
           PERFORM 1500-TESTAR-FS-SAITOTAL.

           ADD 1                       TO  ACU-GRAVA-SAITOTAL.

      *---------------------------------------------------------------*
       3400-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA GRAVAR-SAITOTAL
      *===============================================================*
       3500-GRAVA-FIM-ARQTOTAL  SECTION.
      *---------------------------------------------------------------*

           PERFORM 2200-LER-ENTTOTAL.

           IF  WRK-FIM-ENTTOTAL         EQUAL  'N'
               PERFORM 3400-GRAVAR-ARQTOTAL
           END-IF.

      *---------------------------------------------------------------*
       3500-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA FINAL
      *===============================================================*
       4000-ROTINA-FINAL   SECTION.
      *---------------------------------------------------------------*

           MOVE WRK-MES-REPROC      TO  WAQ0-MES-REPROC.
           MOVE WRK-ANO-REPROC      TO  WAQ0-ANO-REPROC.
           MOVE ACU-TNOK-MESANO     TO  WAQ0-QTDE-REJEIT.
           MOVE ACU-TOK-MESANO      TO  WAQ0-QTDE-REPROC.
           PERFORM 3400-GRAVAR-ARQTOTAL.

           PERFORM 3500-GRAVA-FIM-ARQTOTAL UNTIL
                                  WRK-FIM-ENTTOTAL EQUAL 'S'.

      *--> MOSTRAR TOTALIZACAO

           MOVE ACU-LIDOS-EXTRAMOK     TO  WRK-MASK-LIDOS-EXTRAMOK
           MOVE ACU-LIDOS-ENTTOTAL     TO  WRK-MASK-LIDOS-ENTTOTAL
           MOVE ACU-GRAVA-EXTRASOK     TO  WRK-MASK-GRAVA-EXTRASOK
           MOVE ACU-GRAVA-EXTRANOK     TO  WRK-MASK-GRAVA-EXTRANOK
           MOVE ACU-GRAVA-SAITOTAL     TO  WRK-MASK-GRAVA-SAITOTAL

           DISPLAY '*********************** GFCT2615 *******************
      -                                                        '****'.
           DISPLAY '*
      -                                                        '   *'.
           DISPLAY '* QTDE DE REGISTROS LIDOS   EXTRAMOK.... : '
                                       WRK-MASK-LIDOS-EXTRAMOK   ' *'.
           DISPLAY '* QTDE DE REGISTROS LIDOS   ENTTOTAL.... : '
                                       WRK-MASK-LIDOS-ENTTOTAL   ' *'.
           DISPLAY '* QTDE DE REGISTROS GRAVADOS EXTRASOK... : '
                                       WRK-MASK-GRAVA-EXTRASOK   ' *'.
           DISPLAY '* QTDE DE REGISTROS GRAVADOS EXTRANOK... : '
                                       WRK-MASK-GRAVA-EXTRANOK   ' *'.
           DISPLAY '* QTDE DE REGISTROS GRAVADOS SAITOTAL... : '
                                       WRK-MASK-GRAVA-SAITOTAL   ' *'.
           DISPLAY '*
      -                                                        '   *'.
           DISPLAY '*********************** GFCT2615 *******************
      -                                                        '****'.

      *---------------------------------------------------------------*
       4000-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA ERRO
      *===============================================================*
       9999-ROTINA-ERRO SECTION.
      *---------------------------------------------------------------*

           MOVE 'GFCT2615'             TO  ERR-PGM

           CALL 'BRAD7100'          USING  WRK-BATCH
                                           ERRO-AREA
                                           SQLCA.

           GOBACK.

      *---------------------------------------------------------------*
       9999-99-FIM. EXIT.
      *---------------------------------------------------------------*
