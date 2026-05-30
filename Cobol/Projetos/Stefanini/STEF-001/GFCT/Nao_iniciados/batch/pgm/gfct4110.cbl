      *===============================================================*
       IDENTIFICATION DIVISION.
      *===============================================================*

       PROGRAM-ID. GFCT4110.

      *================================================================*
      *                  SONDA PROCWORK INFORMATICA                    *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA.........: GFCT4110                                 *
      *    PROGRAMADOR......: FABRICA SOFTWARE        - SONDA PROCWORK *
      *                                                                *
      *    ANALISTA.........: ANTONIO PAGNOCCA NETO                    *
      *    LIDER PROJETO....: RODRIGO / MARCELO TEODORO                *
      *    GERENCIA / EQUIPE: GERENCIA-E / EQUIPE-1   - SONDA PROCWORK *
      *                                                                *
      *    GRUPO NEGOCIO....: DDS-GP50                                 *
      *    ANALISTA.........: HELENA                                   *
      *                                                                *
      *    DATA.............: DEZ/2010                                 *
      *                                                                *
      *    PROJETO..........: TARIFA PARCELADA                         *
      *                                                                *
      *    OBJETIVO.........: TEM COMO OBJETIVO SELECIONAR EVENTOS     *
      *                       PARA CONSULTA AO IRES.                   *
      *                                                                *
      *    ARQUIVOS:                                                   *
      *      DDNAME                            INCLUDE/BOOK            *
      *        ENTRADA                           GFCTWAL2              *
      *        SAIDA                             GFCTWAL2              *
      *                                                                *
      *    BANCO DE DADOS...:                                          *
      *      DB2                                                       *
      *      TABLE                             INCLUDE/BOOK            *
      *        DB2PRD.SERVC_TARIF_PRINC          GFCTB0D8              *
      *                                                                *
      *    MODULOS CHAMADOS.:                                          *
      *        BRAD7100 - TRATAR ERRO                                  *
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

           SELECT ENTRADA ASSIGN TO UT-S-ENTRADA
                      FILE STATUS IS WRK-FS-ENTRADA.

           SELECT SAIDA ASSIGN TO UT-S-SAIDA
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

       01  FD-REG-ENTRADA             PIC X(187).

      *---------------------------------------------------------------*
      *   OUTPUT:     ORG. SEQUENCIAL   -   LRECL = 187               *
      *---------------------------------------------------------------*

       FD  SAIDA
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-SAIDA             PIC X(187).

      *---------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *---------------------------------------------------------------*

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
       77  FILLER  PIC X(32)  VALUE  '*  INICIO DA WORKING GFCT4110  *'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*

      *---------------------------------------------------------------*
       01  FILLER  PIC X(32)  VALUE  '*   AREAS AUXILIARES           *'.
      *---------------------------------------------------------------*

       01  WRK-CSERVC-TARIF-ANT        PIC S9(05) COMP-3   VALUE ZEROS.
       01  WRK-INCONSIS                PIC X(01)           VALUE SPACES.

       01  WRK-CNPJ.
ST25X6*     05  WRK-NRO-CNPJ           PIC  9(009)         VALUE ZEROS.
ST25X6     05  WRK-NRO-CNPJ            PIC  X(009)         VALUE SPACES.
           05  WRK-BAR-CNPJ            PIC X(01)           VALUE SPACES.
ST25X6*     05  WRK-FIL-CNPJ           PIC  9(004          VALUE ZEROS.
ST25X6      05  WRK-FIL-CNPJ           PIC  X(004)         VALUE SPACES.
           05  WRK-TRA-CNPJ            PIC X(01)           VALUE SPACES.
           05  WRK-CTR-CNPJ            PIC 9(02)           VALUE ZEROS.

       01  WRK-TAM-03-COM-S-E          PIC +9(003)         VALUE ZEROS.

       01  WRK-TAM-05-COM-S            PIC +9(005)         VALUE ZEROS.
       01  FILLER REDEFINES WRK-TAM-05-COM-S.
           05  FILLER                  PIC  X(001).
           05  WRK-TAM-05-SEM-S        PIC  9(005).

       01  WRK-TAM-09-COM-S            PIC S9(009)         VALUE ZEROS.
       01  FILLER REDEFINES WRK-TAM-09-COM-S.
           05  FILLER                  PIC  X(006).
           05  WRK-TAM-03-COM-S        PIC S9(003).

      *---------------------------------------------------------------*
       01  FILLER  PIC X(32)  VALUE  '*   AREAS DE FILE STATUS       *'.
      *---------------------------------------------------------------*

       77  WRK-BATCH                   PIC X(08) VALUE 'BATCH   '.

       77  WRK-FS-ENTRADA             PIC X(02) VALUE SPACES.
       77  WRK-FS-SAIDA             PIC X(02) VALUE SPACES.

       77  WRK-OPERACAO                PIC X(13) VALUE SPACES.
       77  WRK-ABERTURA                PIC X(13) VALUE 'NA ABERTURA'.
       77  WRK-LEITURA                 PIC X(13) VALUE 'NA LEITURA'.
       77  WRK-GRAVACAO                PIC X(13) VALUE 'NA GRAVACAO'.
       77  WRK-FECHAMENTO              PIC X(13) VALUE 'NO FECHAMENTO'.

      *---------------------------------------------------------------*
       77  FILLER  PIC X(32)  VALUE  '* ACUMULADORES                 *'.
      *---------------------------------------------------------------*

       77  ACU-LIDOS-ENTRADA          PIC 9(09) COMP-3    VALUE ZEROS.
       77  ACU-GRAVA-SAIDA            PIC 9(09) COMP-3    VALUE ZEROS.
       77  ACU-DESPR-LEITURA          PIC 9(09) COMP-3    VALUE ZEROS.
       77  ACU-DESPR-CONSIST          PIC 9(09) COMP-3    VALUE ZEROS.
       77  ACU-SELECT                 PIC 9(09) COMP-3    VALUE ZEROS.
       77  ACU-SELECT-COM-S           PIC 9(09) COMP-3    VALUE ZEROS.

      *---------------------------------------------------------------*
       01  FILLER  PIC X(32)  VALUE  '* MASCARAS DE EDICAO DE TOTAIS *'.
      *---------------------------------------------------------------*

       01  FILLER.
           05  WRK-MASK-LIDOS-ENTRADA PIC ZZZ.ZZZ.ZZ9     VALUE ZEROS.
           05  WRK-MASK-GRAVA-SAIDA   PIC ZZZ.ZZZ.ZZ9     VALUE ZEROS.
           05  WRK-MASK-DESPR-LEITURA PIC ZZZ.ZZZ.ZZ9     VALUE ZEROS.
           05  WRK-MASK-DESPR-CONSIST PIC ZZZ.ZZZ.ZZ9     VALUE ZEROS.
           05  WRK-MASK-SELECT        PIC ZZZ.ZZZ.ZZ9     VALUE ZEROS.
           05  WRK-MASK-SELECT-COM-S  PIC ZZZ.ZZZ.ZZ9     VALUE ZEROS.

      *---------------------------------------------------------------*
       01  FILLER  PIC X(32)  VALUE  '*   AREAS ENTRADA/SAIDA       *'.
      *---------------------------------------------------------------*

       COPY 'GFCTWAL2'.

      *---------------------------------------------------------------*
       01  FILLER  PIC X(32)  VALUE  '*   AREAS TRATAMENTO ERRO    *'.
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
                INCLUDE GFCTB0D8
           END-EXEC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
       01  FILLER  PIC X(32)  VALUE  '*  FIM  DA WORKING   GFCT4110  *'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*

      *===============================================================*
       PROCEDURE DIVISION.
      *===============================================================*

      *===============================================================*
      *    ROTINA INICIAR
      *===============================================================*
       0000-INICIAR SECTION.
      *---------------------------------------------------------------*

           CALL    'BRAD1050'.

           OPEN    INPUT   ENTRADA
                   OUTPUT  SAIDA

           MOVE    WRK-ABERTURA        TO  WRK-OPERACAO.
           PERFORM 1000-TESTAR-FILE-STATUS.

           PERFORM 2000-ROTINA-INICIAL.

           IF  ACU-LIDOS-ENTRADA       EQUAL ZEROS
               GO                      TO  0000-FINALIZAR
           END-IF.

           PERFORM 3000-PROCESSA
             UNTIL WRK-FS-ENTRADA EQUAL '10'.

           PERFORM 4000-ROTINA-FINAL.


      *===============*
      * LABEL FINALIZAR
      *===============*
       0000-FINALIZAR.
      *---------------*

           CLOSE   ENTRADA
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

           PERFORM 1200-TESTAR-FS-SAIDA.

      *---------------------------------------------------------------*
       1000-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA TESTAR-FS-ENTRADA
      *===============================================================*
       1100-TESTAR-FS-ENTRADA SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-ENTRADA          NOT EQUAL '00'
               DISPLAY '************** GFCT4110 *************'
               DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
               DISPLAY '*              ENTRADA              *'
               DISPLAY '*         FILE STATUS =  ' WRK-FS-ENTRADA
                                                  '         *'
               DISPLAY '*         PROGRAMA ENCERRADO        *'
               DISPLAY '************** GFCT4110 *************'
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
      *    ROTINA TESTAR-FS-SAIDA
      *===============================================================*
       1200-TESTAR-FS-SAIDA   SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-SAIDA            NOT EQUAL '00'
               DISPLAY '************** GFCT4110 *************'
               DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
               DISPLAY '*               SAIDA               *'
               DISPLAY '*         FILE STATUS =  ' WRK-FS-SAIDA
                                                  '         *'
               DISPLAY '*         PROGRAMA ENCERRADO        *'
               DISPLAY '************** GFCT4110 *************'
               MOVE WRK-OPERACAO       TO  WRK-OPERACAO-TXT
               MOVE 'SAIDA'            TO  WRK-ARQ-TXT
               MOVE WRK-TEXTO          TO  ERR-TEXTO
               MOVE 'APL'              TO  ERR-TIPO-ACESSO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1200-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA ROTINA-INICIAL
      *===============================================================*
       2000-ROTINA-INICIAL SECTION.
      *---------------------------------------------------------------*

           PERFORM 2100-LER-ENTRADA.

           IF  ACU-LIDOS-ENTRADA      EQUAL   ZEROS
               DISPLAY '************* GFCT4110 *************'
               DISPLAY '*                                  *'
               DISPLAY '*    ARQUIVO ENTRADA ESTA VAZIO    *'
               DISPLAY '*                                  *'
               DISPLAY '*        PROGRAMA ENCERRADO        *'
               DISPLAY '*                                  *'
               DISPLAY '************* GFCT4110 *************'
           END-IF.

      *---------------------------------------------------------------*
       2000-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA LER-ENTRADA
      *===============================================================*
       2100-LER-ENTRADA SECTION.
      *---------------------------------------------------------------*

           READ ENTRADA INTO REG-GFCTWAL2.

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
      *    ROTINA PROCESSA
      *===============================================================*
       3000-PROCESSA SECTION.
      *---------------------------------------------------------------*

           MOVE SPACES                 TO  WRK-INCONSIS

           PERFORM 3100-CONSISTIR-CAMPOS

           IF  WRK-INCONSIS            EQUAL 'S'
               ADD 1                   TO  ACU-DESPR-CONSIST
               PERFORM 2100-LER-ENTRADA
               GO                      TO  3000-99-FIM
           END-IF.

           IF  WAL2-CSERVC-TARIF       NOT EQUAL WRK-CSERVC-TARIF-ANT
               MOVE WAL2-CSERVC-TARIF  TO  WRK-CSERVC-TARIF-ANT
               PERFORM 3200-PESQUISAR-TARIFA
           END-IF.

           IF  CINDCD-CONS-IMPED       EQUAL 'S'
               PERFORM 3300-GRAVAR-SAIDA
           ELSE
               ADD 1                   TO  ACU-DESPR-LEITURA
           END-IF.

           PERFORM 2100-LER-ENTRADA.

      *---------------------------------------------------------------*
       3000-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA CONSISTIR-CAMPOS
      *===============================================================*
       3100-CONSISTIR-CAMPOS  SECTION.
      *---------------------------------------------------------------*

           IF  WAL2-CSIT-EVNTO-RECBD   EQUAL 2 AND
               WAL2-VTARIF-LIQ-MOVTO   GREATER 0,00

               MOVE WAL2-RJUSTF-SOLTC-EXTRT(1:17)
                                       TO  WRK-CNPJ

               IF  WRK-NRO-CNPJ        NOT NUMERIC OR
ST25X6             WRK-NRO-CNPJ        EQUAL SPACES OR
                   WRK-BAR-CNPJ        NOT EQUAL '/' OR
                   WRK-FIL-CNPJ        NOT NUMERIC OR
ST25X6             WRK-FIL-CNPJ        EQUAL SPACES OR
                   WRK-TRA-CNPJ        NOT EQUAL '-' OR
                   WRK-CTR-CNPJ        NOT NUMERIC
                   MOVE 'S'            TO  WRK-INCONSIS
               END-IF
           ELSE
               MOVE 'S'                TO  WRK-INCONSIS
           END-IF.

      *---------------------------------------------------------------*
       3100-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA PESQUISAR-TARIFA
      *===============================================================*
       3200-PESQUISAR-TARIFA SECTION.
      *---------------------------------------------------------------*

           ADD 1                       TO  ACU-SELECT.

           MOVE WAL2-CSERVC-TARIF      TO  CSERVC-TARIF

           EXEC SQL
               SELECT
                   CINDCD_CONS_IMPED
               INTO
                   :GFCTB0D8.CINDCD-CONS-IMPED
               FROM
                   DB2PRD.SERVC_TARIF_PRINC
               WHERE
                   CSERVC_TARIF        = :GFCTB0D8.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE NOT EQUAL ZEROS AND +100 ) OR
              (SQLWARN0    EQUAL 'W' )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'SERVC_TARIF_PRINC'
                                       TO  ERR-DBD-TAB
               MOVE 'SELECT'           TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0001'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'N'                TO  CINDCD-CONS-IMPED

               MOVE SQLCODE            TO  WRK-TAM-09-COM-S
               MOVE WRK-TAM-03-COM-S   TO  WRK-TAM-03-COM-S-E
               MOVE CSERVC-TARIF       TO  WRK-TAM-05-COM-S

               DISPLAY '************* GFCT4110 *************'
               DISPLAY '*                                  *'
               DISPLAY '*   RETORNO ACESSO TAB. GFCTB0D8   *'
               DISPLAY '*                                  *'
               DISPLAY '*   COMANDO: SELECT                *'
               DISPLAY '*   SQLCODE: ' WRK-TAM-03-COM-S-E
                                        '                  *'
               DISPLAY '*                                  *'
               DISPLAY '*   CHAVE:                         *'
               DISPLAY '*   CSERVC-TARIF: ' WRK-TAM-05-SEM-S
                                              '            *'
               DISPLAY '*                                  *'
               DISPLAY '************* GFCT4110 *************'
           END-IF.

           IF  CINDCD-CONS-IMPED       EQUAL 'S'
               ADD 1                   TO  ACU-SELECT-COM-S
           END-IF.

      *---------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA GRAVAR-SAIDA
      *===============================================================*
       3300-GRAVAR-SAIDA  SECTION.
      *---------------------------------------------------------------*

           WRITE FD-REG-SAIDA        FROM  REG-GFCTWAL2.

           MOVE WRK-GRAVACAO           TO  WRK-OPERACAO.
           PERFORM 1200-TESTAR-FS-SAIDA.

           ADD 1                       TO  ACU-GRAVA-SAIDA.

      *---------------------------------------------------------------*
       3300-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA FINAL
      *===============================================================*
       4000-ROTINA-FINAL   SECTION.
      *---------------------------------------------------------------*

      *--> MOSTRAR TOTALIZACAO

           MOVE ACU-LIDOS-ENTRADA     TO  WRK-MASK-LIDOS-ENTRADA
           MOVE ACU-GRAVA-SAIDA       TO  WRK-MASK-GRAVA-SAIDA
           MOVE ACU-DESPR-LEITURA     TO  WRK-MASK-DESPR-LEITURA
           MOVE ACU-DESPR-CONSIST     TO  WRK-MASK-DESPR-CONSIST
           MOVE ACU-SELECT            TO  WRK-MASK-SELECT
           MOVE ACU-SELECT-COM-S      TO  WRK-MASK-SELECT-COM-S

           DISPLAY '*********************** GFCT4110 *******************
      -                                                        '****'.
           DISPLAY '*
      -                                                        '   *'.
           DISPLAY '* QTDE DE REGISTROS LIDOS    ENTRADA.... : '
                                       WRK-MASK-LIDOS-ENTRADA ' *'.
           DISPLAY '* QTDE DE REG. DESPREZADOS NA LEITURA... : '
                                       WRK-MASK-DESPR-LEITURA ' *'.
           DISPLAY '* QTDE DE REG. DESPREZ. CONSIST. FISICA. : '
                                       WRK-MASK-DESPR-CONSIST ' *'.
           DISPLAY '* QTDE DE SELECTS NA TAB. GFCTB0D8...... : '
                                       WRK-MASK-SELECT        ' *'.
           DISPLAY '* QTDE DE SELECTS COM "S"............... : '
                                       WRK-MASK-SELECT-COM-S  ' *'.
           DISPLAY '* QTDE DE REGISTROS GRAVADOS SAIDA...... : '
                                       WRK-MASK-GRAVA-SAIDA ' *'.
           DISPLAY '*
      -                                                        '   *'.
           DISPLAY '*********************** GFCT4110 *******************
      -                                                        '****'.

      *---------------------------------------------------------------*
       4000-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA ERRO
      *===============================================================*
       9999-ROTINA-ERRO SECTION.
      *---------------------------------------------------------------*

           MOVE 'GFCT4110'             TO  ERR-PGM

           CALL 'BRAD7100'          USING  WRK-BATCH
                                           ERRO-AREA
                                           SQLCA.

           GOBACK.

      *---------------------------------------------------------------*
       9999-99-FIM. EXIT.
      *---------------------------------------------------------------*
