      *================================================================*
      * VERSAO 4.0 - IMPLANTACAO PREVISTA OUTUBRO/2013                 *
      *================================================================*
      * SUBSTITUICAO DAS COPYS CLONES RUEC PELAS COPYS OFICIAIS RCOR:  *
      * I#RUEC69 = I#RCOR04                                            *
      * I#RUEC70 = I#RCOR05                                            *
      * I#RUEC71 = I#RCOR07                                            *
      * I#RUEC72 = I#RCOR08                                            *
      * I#RUEC77 = I#RCOR03                                            *
      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. RUEC8804.
       AUTHOR.     MARINA FUJITA.
      *================================================================*
      *                    S O N D A  P R O C W O R K                  *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   RUEC8804                                    *
      *    ANALISTA....:   MARINA FUJITA           - SONDA PROCWORK    *
      *    DATA........:   21/06/2012                                  *
      *    PROJETO.....:   PWT 12/9931    2012/81536/5-01              *
      *                    REFORMULACAO MENSAGERIA BACEN - RECOR       *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   TRATAR RETORNO DA MENSAGEM BACEN            *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#RUEC60 - AREA DE RETORNO DO MODULO.                       *
      *    I#BRAD7C - AREA PARA TRATAMENTO DE ERRO DB2.                *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    BRAD7100 - VERIFICAR DISPONIBILIDADE                        *
      *================================================================*
      *                                                                *
      * BSI TECNOLOGIA.                                                *
      *                                                                *
      * ALTERACAO: BS0250  - ARQUIVO FORNECEDORES VERSAO 4.0           *
      *                      ALTERAR MOVAS CONDICOES DO CSIT-MSGEM     *
      * DATA.....: OUTUBRO/2013                                        *
      * ANALISTA.: DANILO KIOROGLO                                     *
      *                                                                *
BR0618*================================================================*
BR0618* BRQ.                                                           *
BR0618*                                                                *
BR0618* ALTERACAO: QUANDO COR0001R1 E RCOR04-COD-ERRO <> '****'        *
BR0618*            FORMATAR CONFORME COR0001E                          *
BR0618* DATA.....: JUNHO/2018                                          *
BR0618* ANALISTA.: PATRICK SILVEIRA                                    *
      *================================================================*
7C0120*================================================================*
.     *                      A L T E R A C A O                         *
.     *================================================================*
.     *    ANALISTA....:   EDSON MATSUMOTO         - 7COMM             *
.     *    DATA........:   JANEIRO/2020                                *
.     *    OBJETIVO....:   ALTERAR A FORMA DE ENVIO DE CONTINUACAO,    *
.     *                    DEVERA  AGUARDAR O RETORNO DO SICOR PARA    *
.     *                    ENVIO DA PROXIMA COR0001 SERA FEITO NO      *
.     *                    RUEC8804                                    *
7C0120*================================================================*
BRQ001*================================================================*
.     *                      A L T E R A C A O                         *
.     *================================================================*
.     *    ANALISTA....:   RODOLFO VIEIRA          - BRQ               *
.     *    DATA........:   JULHO/2021                                  *
.     *    OBJETIVO....:   ADICIONAR O TRATAMENTO DAS MENSAGENS        *
.     *                    COR0003E E COR0003R1.                       *
BRQ001*================================================================*
7COMMP*                   A L T E R A C A O                            *
.     *----------------------------------------------------------------*
.     *    ANALISTA....:  PRISCILA PEREIRA                             *
.     *    DATA........:  SETEMBRO/2023                                *
.     *----------------------------------------------------------------*
.     *    OBJETIVO....:  SICOR 5.07                                   *
.     *    PESQUISA....:  7COMMP                                       *
7COMMP*================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

       CONFIGURATION                   SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *---------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       77  FILLER                      PIC X(079)          VALUE
           '*** INICIO DA WORKING RUEC8804 ***'.
      *---------------------------------------------------------------*
       77      WRK-NMNTRC-MSGEM        PIC S9(015)  COMP-3 VALUE ZEROS.
       77      WRK-MODULO              PIC  X(008)  VALUE  SPACES.
      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(079)          VALUE
           '*** VARIAVEIS AUXILIARES ***'.
      *---------------------------------------------------------------*
       77  WRK-LIMITE                  PIC S9(03)  COMP-3  VALUE +4.
       77  WRK-COUNT                   PIC S9(03)  COMP-3  VALUE ZEROS.
7C0120 77  WRK-CONTA-ENVIO             PIC  9(03)  COMP-3  VALUE ZEROS.
7C0120 77  WRK-MODULO-RCOR5005         PIC  X(08)      VALUE 'RCOR5005'.
BS0281 77  WRK-CORIGE-REC-NULL         PIC S9(004) COMP    VALUE ZEROS.
BS0281 77  WRK-CTPO-INI                PIC S9(004) COMP-3  VALUE ZEROS.
BS0281 77  WRK-CTPO-FIM                PIC S9(004) COMP-3  VALUE ZEROS.
BS0281 77  WRK-FIM-RUECV010            PIC X(001)          VALUE SPACES.
BS0281 77  WRK-FIM-RUECV012            PIC X(001)          VALUE SPACES.
BR0618 77  WRK-FLAG                    PIC X(002)          VALUE SPACES.
7C0120 77  WRK-QTDREG-B09A             PIC S9(005) COMP-3  VALUE ZEROS.

BR0618 01  WRK-COD-RET.
BR0618     05  WRK-COD-5E              PIC X(003)          VALUE 'C5E'.
BR0618     05  WRK-COD-MO              PIC 9(005)          VALUE ZEROS.

BS0281 01  WRK-EVENTO                  PIC  9(004)          VALUE ZEROS.
BS0281 01  FILLER          REDEFINES   WRK-EVENTO.
BS0281     03  WRK-EVENT-TP            PIC  9(002).
BS0281     03  WRK-EVENT-CD            PIC  9(002).

BS0281 01  WRK-CFORMA-RURAL-CM-NULL    PIC S9(04) COMP   VALUE ZEROS.
BS0281 01  WRK-PUTILZ-INDCD-DESC-NULL  PIC S9(04) COMP   VALUE ZEROS.

BS0281 01  WRK-NUM-03                  PIC -9(03)           VALUE ZEROS.
BS0281 01  FILLER            REDEFINES WRK-NUM-03.
BS0281     03  FILLER                  PIC  X(002).
BS0281     03  WRK-NUM-02              PIC  9(002).

7C0120 01  WRK-ENVIO-LEN            PIC S9(004) COMP VALUE ZEROS.
.
.      01  WRK-ENVIO-SIS            PIC X(32000)    VALUE SPACES.
.      01  WRK-ENVIO-SIS-R          REDEFINES WRK-ENVIO-SIS.
.          05 FILLER                PIC X(00009).
.          05 WRK-ENVIO-NUMCTRLIF   PIC X(00020).
7C0120     05 FILLER                PIC X(31971).

7C0120*---------------------------------------------------------------*
.      01  FILLER                      PIC X(079)          VALUE
.          '*** AREA PARA CHAMADA MODULO RCOR5005 ***'.
.     *---------------------------------------------------------------*
.
.      COPY 'I#RCOR10'.
7C0120
      *---------------------------------------------------------------*
       01  FILLER                      PIC X(079)          VALUE
           '*** AREA PARA CHAMADA DO MODULO RUEC8806 ***'.
      *---------------------------------------------------------------*

       COPY 'I#RUEC61'.

       01  FILLER                      PIC X(079)          VALUE
           '*** LAYOUT DA MENSAGEM COR0001E          ***'.
      *---------------------------------------------------------------*

       COPY 'I#RUEC68'.

       01  FILLER                      PIC X(079)          VALUE
           '*** LAYOUT DA MENSAGEM COR0001R1         ***'.
      *---------------------------------------------------------------*

       COPY 'I#RUEC89'.

       01  FILLER                      PIC X(079)          VALUE
           '*** LAYOUT DA MENSAGEM GEN0004           ***'.
      *---------------------------------------------------------------*

       COPY 'I#RCOR05'.

       01  FILLER                      PIC X(079)          VALUE
           '*** LAYOUT DA MENSAGEM COR0002R1         ***'.
      *---------------------------------------------------------------*

       COPY 'I#RCOR07'.

       01  FILLER                      PIC X(079)          VALUE
           '*** LAYOUT DA MENSAGEM COR0002E          ***'.
      *---------------------------------------------------------------*

       COPY 'I#RCOR08'.

       01  FILLER                      PIC X(079)          VALUE
           '*** AREA PARA CHAMADA DO MODULO RUEC8816 ***'.
      *---------------------------------------------------------------*

       COPY 'I#RUEC73'.

BR1217 01  FILLER                      PIC X(079)          VALUE
BR1217     '*** AREA PARA CHAMADA DO MODULO RUEC8850 E RUEC8860 ***'.
BR1217*---------------------------------------------------------------*
BR1217
BR1217 COPY 'I#RUEC87'.

       01  FILLER                      PIC X(079)          VALUE
           '*** LAYOUT DA MENSAGEM TMNF              ***'.
      *---------------------------------------------------------------*

       COPY 'I#RCOR03'.

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(079)          VALUE
           '*** TRATAR CAMPOS NULOS                  ***'.
      *---------------------------------------------------------------*

       01  WRK-NULL-B09B.
           05 WRK-B09B-CCTRL-NULL  PIC S9(04) COMP VALUE +0.
           05 WRK-B09B-CSITM-NULL  PIC S9(04) COMP VALUE +0.

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(079)          VALUE
           '*** AREA PARA TABELAS DB2 ***'.
      *---------------------------------------------------------------*

           EXEC SQL INCLUDE SQLCA    END-EXEC.

7C0120     EXEC SQL INCLUDE RUECB09A END-EXEC.

           EXEC SQL INCLUDE RUECB09B END-EXEC.

           EXEC SQL INCLUDE RUECB09C END-EXEC.

EW0116     EXEC SQL INCLUDE RUFIB002 END-EXEC.

BS0281     EXEC SQL INCLUDE RUECB007 END-EXEC.

BS0281     EXEC SQL INCLUDE RURCB051 END-EXEC.

BS0281     EXEC SQL INCLUDE RUECV010 END-EXEC.

BS0281     EXEC SQL INCLUDE RUECV012 END-EXEC.

EG0617     EXEC SQL INCLUDE RUECB048 END-EXEC.

      *===============================================================*

BS0281     EXEC SQL
BS0281         DECLARE CSR01-RUECV010 CURSOR WITH HOLD FOR
BS0281         SELECT  CJUNC_DEPDC,
BS0281                 CCDULA,
BS0281                 CTPO_EVNTO_RURAL,
BS0281                 DEVNTO,
BS0281                 VEVNTO_RURAL
BS0281          FROM   DB2PRD.V01EVNTO_CDULA
BS0281          WHERE  CJUNC_DEPDC = :RUECV010.CJUNC-DEPDC
BS0281            AND  CCDULA      = :RUECV010.CCDULA
BS0281            AND (CTPO_EVNTO_RURAL BETWEEN :WRK-CTPO-INI AND
BS0281                                          :WRK-CTPO-FIM)
BS0281     END-EXEC.

BS0281     EXEC SQL
BS0281         DECLARE CSR01-RUECV012 CURSOR WITH HOLD FOR
BS0281         SELECT  CCONTR,
BS0281                 CTPO_EVNTO_RURAL,
BS0281                 DEVNTO,
BS0281                 VEVNTO_RURAL,
BS0281                 VPCELA_REC_PPRIO
BS0281          FROM   DB2PRD.V01EVNTO_RURAL
BS0281          WHERE  CCONTR      = :RUECV012.CCONTR
BS0281            AND (CTPO_EVNTO_RURAL BETWEEN :WRK-CTPO-INI AND
BS0281                                          :WRK-CTPO-FIM)
BS0281     END-EXEC.

      *---------------------------------------------------------------*
       01  FILLER   PIC X(32) VALUE '* FIM DA WORKING RUEC8804 *'.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       LINKAGE SECTION.
      *---------------------------------------------------------------*

       COPY 'I#RUEC60'.

       COPY I#BRAD7C.

       01  LNK-SQLCA                   PIC X(136).

      *================================================================*
       PROCEDURE DIVISION              USING RUEC60-REGISTRO
                                             ERRO-AREA
                                             LNK-SQLCA.
      *================================================================*

      *---------------------------------------------------------------*
       0000-INICIO                 SECTION.
      *---------------------------------------------------------------*

           CALL  'CKRS1000'.

           MOVE ZEROS              TO RUEC60-COD-RETORNO.

           PERFORM  1000-PROCESSAMENTO-PRINCIPAL.

           PERFORM  4000-ROTINA-FIM.

      *---------------------------------------------------------------*
       0000-99-FIM.                EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1000-PROCESSAMENTO-PRINCIPAL SECTION.
      *---------------------------------------------------------------*


           PERFORM 2000-VALIDAR-ARG-ENTRADA.


           IF  RUEC60-COD-RETORNO EQUAL ZEROS
               PERFORM 3000-CONSULTA
               IF  RUEC60-COD-RETORNO EQUAL ZEROS
                   PERFORM 3500-INCLUSAO
               END-IF
           END-IF.

      *---------------------------------------------------------------*
       1000-99-FIM.                EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2000-VALIDAR-ARG-ENTRADA    SECTION.
      *---------------------------------------------------------------*

           IF  RUEC60-COD-MSG      EQUAL SPACES
               MOVE 01             TO RUEC60-COD-RETORNO
               GO  TO  2000-99-FIM
           END-IF.

           IF  RUEC60-SISMSG       EQUAL SPACES
               MOVE 02             TO RUEC60-COD-RETORNO
               GO  TO  2000-99-FIM
           END-IF.

           EVALUATE  RUEC60-COD-MSG
               WHEN 'COR0001R1'
               WHEN 'COR0001E '
               WHEN 'COR0002R1'
               WHEN 'COR0002E '
BRQ001         WHEN 'COR0003R1'
BRQ001         WHEN 'COR0003E '
EG0617         WHEN 'COR0005R1'
EG0617         WHEN 'COR0005E '
BR1217         WHEN 'COR0006R1'
BR1217         WHEN 'COR0006E '
               WHEN 'GEN0004  '
               WHEN 'TMNF     '
                    MOVE ZEROS          TO RUEC60-COD-RETORNO
               WHEN OTHER
                    MOVE 03             TO RUEC60-COD-RETORNO
           END-EVALUATE.

      *---------------------------------------------------------------*
       2000-99-FIM.                EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3000-CONSULTA               SECTION.
      *---------------------------------------------------------------*

           IF RUEC60-COD-MSG        EQUAL  'COR0006E' OR 'COR0006R1' OR
BRQ001                                     'COR0003E' OR 'COR0003R1'
              GO TO                        3000-99-FIM
           END-IF.

           IF  RUEC60-COD-MSG  NOT  EQUAL  'GEN0004  '
               MOVE RUEC60-NRO-CTRL-IF TO
                                      CCTRL-RQUIS-FINCR  OF RUECB09B
           ELSE
               MOVE RUEC60-SISMSG(1: LENGTH OF RCOR05-REGISTRO)
                                       TO RCOR05-REGISTRO
               MOVE  RCOR05-NRO-OPE-ORIG   TO CUNIC-MSGEM OF RUECB09B
           END-IF.

           IF  RUEC60-COD-MSG  NOT  EQUAL  'GEN0004  '
               EXEC SQL
                   SELECT   CCONTR_CREDT_RURAL
                          , NENVIO_RECOR
                          , CCTRL_RQUIS_FINCR
                          , HULT_ATULZ
BS0250                    , CSIT_MSGEM
                   INTO :RUECB09B.CCONTR-CREDT-RURAL
                      , :RUECB09B.NENVIO-RECOR
                      , :RUECB09B.CCTRL-RQUIS-FINCR
                             :WRK-B09B-CCTRL-NULL
                      , :RUECB09B.HULT-ATULZ
BS0250                , :RUECB09B.CSIT-MSGEM
BS0250                       :WRK-B09B-CSITM-NULL
                     FROM DB2PRD.THIST_ENVIO_RECOR
                  WHERE CCTRL_RQUIS_FINCR  = :RUECB09B.CCTRL-RQUIS-FINCR
               END-EXEC
           ELSE
               EXEC SQL
                   SELECT   CCONTR_CREDT_RURAL
                          , NENVIO_RECOR
                          , CCTRL_RQUIS_FINCR
                          , HULT_ATULZ
BS0250                    , CSIT_MSGEM
                       INTO :RUECB09B.CCONTR-CREDT-RURAL
                          , :RUECB09B.NENVIO-RECOR
                          , :RUECB09B.CCTRL-RQUIS-FINCR
                                 :WRK-B09B-CCTRL-NULL
                          , :RUECB09B.HULT-ATULZ
BS0250                    , :RUECB09B.CSIT-MSGEM
BS0250                           :WRK-B09B-CSITM-NULL
                     FROM DB2PRD.THIST_ENVIO_RECOR
                    WHERE CUNIC_MSGEM        = :RUECB09B.CUNIC-MSGEM
               END-EXEC
           END-IF.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 04                  TO RUEC60-COD-RETORNO
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'THIST_ENVIO_RECOR' TO ERR-DBD-TAB
               MOVE 'SELECT'            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0010'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               MOVE SQLCA(1:136)        TO LNK-SQLCA
               PERFORM 4000-ROTINA-FIM
           END-IF.

           IF  SQLCODE                  EQUAL ZEROS
               PERFORM 3001-VER-NULOS-B09B
               GO  TO  3000-99-FIM
           END-IF.

           IF  SQLCODE                  EQUAL +100
               MOVE 05                  TO RUEC60-COD-RETORNO
               PERFORM 4000-ROTINA-FIM
           END-IF.

      *---------------------------------------------------------------*
       3000-99-FIM.                EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3001-VER-NULOS-B09B         SECTION.
      *---------------------------------------------------------------*

            IF WRK-B09B-CCTRL-NULL  LESS ZEROS
               MOVE SPACES  TO CCTRL-RQUIS-FINCR OF RUECB09B
            END-IF.

BS0250      IF WRK-B09B-CSITM-NULL  LESS ZEROS
BS0250         MOVE ZEROS   TO CSIT-MSGEM OF RUECB09B
BS0250      END-IF.

      *---------------------------------------------------------------*
       3001-99-FIM.                EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3500-INCLUSAO               SECTION.
      *---------------------------------------------------------------*


            MOVE RUEC60-TAM-SISMSG     TO WCONTD-MSGEM-RURAL-LEN
                                       OF RUECB09C
            EVALUATE  RUEC60-COD-MSG
                WHEN 'COR0001R1'
EG0617          WHEN 'COR0005R1'
BS0250                IF CSIT-MSGEM    OF RUECB09B EQUAL 6
BS0250                   MOVE 7        TO CSIT-MSGEM OF RUECB09C
BS0250                ELSE
                         MOVE 4        TO CSIT-MSGEM OF RUECB09C
BS0250                END-IF
BR0618                MOVE RUEC60-SISMSG
                                       TO RCOR04-REGISTRO
BR0618                MOVE SPACES      TO WRK-FLAG
BR0618                IF  RCOR04-COD-ERRO NOT EQUAL '********'
BR0618                    MOVE '1E'    TO WRK-FLAG
BR0618                    MOVE 3       TO CSIT-MSGEM OF RUECB09C
BR0618                    MOVE RUEC60-SISMSG
BR0618                                   (1: LENGTH OF RUEC68-COR0001-1)
BR0618                                 TO RUEC68-COR0001-1
BR0618                END-IF
BR1217          WHEN 'COR0006R1'
BRQ001          WHEN 'COR0003R1'
BR1217                MOVE 4           TO CSIT-MSGEM OF RUECB09C
                WHEN 'COR0001E '
EG0617          WHEN 'COR0005E '
RE1217          WHEN 'COR0006E '
                      MOVE 3           TO CSIT-MSGEM OF RUECB09C
                      MOVE RUEC60-SISMSG(1: LENGTH OF RUEC68-COR0001-1)
                                       TO RUEC68-COR0001-1
                WHEN 'COR0002R1'
BS0250                IF CSIT-MSGEM    OF RUECB09B EQUAL 6
BS0250                   MOVE 7        TO CSIT-MSGEM OF RUECB09C
BS0250                ELSE
                         MOVE 4        TO CSIT-MSGEM OF RUECB09C
BS0250                END-IF
                      MOVE RUEC60-SISMSG(1: LENGTH OF RCOR07-REGISTRO)
                                       TO RCOR07-REGISTRO
                WHEN 'COR0002E '
                      MOVE 3           TO CSIT-MSGEM OF RUECB09C
                      MOVE RUEC60-SISMSG(1: LENGTH OF RCOR08-REGISTRO)
                                       TO RCOR08-REGISTRO
BRQ001          WHEN 'COR0003E '
BRQ001                MOVE 3           TO CSIT-MSGEM OF RUECB09C
                WHEN 'GEN0004  '
                      MOVE 3           TO CSIT-MSGEM OF RUECB09C
                      MOVE RUEC60-SISMSG(1: LENGTH OF RCOR05-REGISTRO)
                                       TO RCOR05-REGISTRO
                WHEN 'TMNF     '
                      MOVE RUEC60-SISMSG(1: LENGTH OF RCOR03-REGISTRO)
                                       TO RCOR03-REGISTRO
                      IF  RCOR03-CRETOR-PRINC NUMERIC
                      AND RCOR03-CRETOR-PRINC EQUAL ZEROS
                          MOVE 2           TO CSIT-MSGEM OF RUECB09C
                      ELSE
                          MOVE 3           TO CSIT-MSGEM OF RUECB09C
                      END-IF

           END-EVALUATE.

BR0618     MOVE ZEROS                TO RUEC60-COD-RETORNO.
           MOVE RUEC60-COD-MSG       TO CMSGEM-RECOR OF RUECB09C.
           MOVE SPACES               TO CISPB-EMISR-MSGEM OF RUECB09C
           MOVE SPACES               TO CISPB-DSTNA-MSGEM OF RUECB09C
           MOVE RUEC60-NRO-CTRL-IF   TO CCTRL-RQUIS-FINCR OF RUECB09C.
           IF   RUEC60-COD-MSG       EQUAL  'GEN0004  '
                MOVE  RCOR05-NRO-OPE-ORIG TO CUNIC-MSGEM OF RUECB09C
                MOVE  CCTRL-RQUIS-FINCR OF RUECB09B
                                     TO CCTRL-RQUIS-FINCR OF RUECB09C
           ELSE
                MOVE SPACES               TO CUNIC-MSGEM OF RUECB09C
           END-IF.
           MOVE RUEC60-SISMSG        TO WCONTD-MSGEM-RURAL-TEXT
                                       OF RUECB09C
           MOVE ZEROS                TO NMNTRC-MSGEM-RURAL OF RUECB09C.
           MOVE ZEROS                TO WRK-NMNTRC-MSGEM.

           EXEC SQL
             SELECT MAX(NMNTRC_MSGEM_RURAL)
             INTO :WRK-NMNTRC-MSGEM
             FROM  DB2PRD.TMNTRC_MSGEM_RURAL
           END-EXEC.

           IF (SQLCODE   NOT EQUAL ZEROS AND +100 AND -310 AND -305) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 06                  TO RUEC60-COD-RETORNO
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'TMNTRC_MSGEM_RURAL' TO ERR-DBD-TAB
               MOVE 'MAX   '            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0020'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               MOVE SQLCA(1:136)        TO LNK-SQLCA
               PERFORM 4000-ROTINA-FIM
           END-IF.

           ADD 1                        TO WRK-NMNTRC-MSGEM.
           MOVE WRK-NMNTRC-MSGEM        TO NMNTRC-MSGEM-RURAL
                                        OF RUECB09C

           EXEC SQL
             INSERT INTO DB2PRD.TMNTRC_MSGEM_RURAL
                       (NMNTRC_MSGEM_RURAL,
                        CMSGEM_RECOR,
                        CISPB_DSTNA_MSGEM,
                        CISPB_EMISR_MSGEM,
                        CCTRL_RQUIS_FINCR,
                        CUNIC_MSGEM,
                        HMOVTO_MSGEM_RECOR,
                        HULT_ATULZ,
                        WCONTD_MSGEM_RURAL,
                        CSIT_MSGEM)
                VALUES (:RUECB09C.NMNTRC-MSGEM-RURAL,
                        :RUECB09C.CMSGEM-RECOR,
                        :RUECB09C.CISPB-DSTNA-MSGEM,
                        :RUECB09C.CISPB-EMISR-MSGEM,
                        :RUECB09C.CCTRL-RQUIS-FINCR,
                        :RUECB09C.CUNIC-MSGEM,
                         CURRENT TIMESTAMP,
                         CURRENT TIMESTAMP,
                        :RUECB09C.WCONTD-MSGEM-RURAL,
                        :RUECB09C.CSIT-MSGEM)
           END-EXEC.

RE0219     IF (SQLCODE               NOT EQUAL ZEROS ) OR
RE0219        (SQLWARN0              EQUAL  'W')
RE0219         MOVE 07                  TO RUEC60-COD-RETORNO
RE0219         MOVE 'DB2'                    TO  ERR-TIPO-ACESSO
RE0219         MOVE 'TMNTRC_MSGEM_RURAL'     TO  ERR-DBD-TAB
RE0219         MOVE 'INSERT  '               TO  ERR-FUN-COMANDO
RE0219         MOVE SQLCODE                  TO  ERR-SQL-CODE
RE0219         MOVE '3500'                   TO  ERR-LOCAL
RE0219         MOVE SPACES                   TO  ERR-SEGM
RE0219         MOVE SQLCA(1:136)        TO LNK-SQLCA
RE0219         PERFORM 4000-ROTINA-FIM
RE0219     END-IF.

EG0617****-IF  ERRO HEXA PARA NAO IMPLANTAR
BR0618     IF (RUEC60-COD-MSG    EQUAL 'COR0005R1' OR 'COR0006R1'  OR
BRQ001                                 'COR0003R1' )               OR
BR0618        (RUEC60-COD-MSG    EQUAL 'COR0001R1' AND
BR0618         WRK-FLAG      NOT EQUAL '1E')
EG0617         MOVE CMSGEM-RECOR OF RUECB09C (1:8) TO CTERM OF RUECB048
EG0617         MOVE WCONTD-MSGEM-RURAL-TEXT OF RUECB09C (1:300)
EG0617                           TO WRESUL-CONS-MDULO-TEXT OF RUECB048
EG0617         MOVE 300          TO WRESUL-CONS-MDULO-LEN  OF RUECB048
EG0617         MOVE CMSGEM-RECOR OF RUECB09C (1:8) TO CTERM OF RUECB048
EG0617         EXEC SQL
EG0617           INSERT INTO DB2PRD.TAPOIO_UTILZ_MDULO (
EG0617                  CTERM
EG0617           ,      CIDTFD_MDULO
EG0617           ,      CSEQ_CTRL_MDULO
EG0617           ,      WRESUL_CONS_MDULO
EG0617           ) VALUES (
EG0617             :RUECB048.CTERM
EG0617           , 0
EG0617           , 0
EG0617           , CHAR ( CURRENT TIMESTAMP ) ||
EG0617             :RUECB048.WRESUL-CONS-MDULO )
EG0617         END-EXEC
EG0617     END-IF.
           IF  RUEC60-COD-MSG  EQUAL  'COR0001R1'
BR0618     AND WRK-FLAG        NOT EQUAL '1E'
               PERFORM  3600-CHAMA-RUEC8806
               GO  TO  3500-99-FIM
           ELSE
           IF  RUEC60-COD-MSG  EQUAL  'COR0002R1'
               PERFORM  3700-CHAMA-RUEC8816
               GO  TO  3500-99-FIM
           END-IF
BR1217     IF  RUEC60-COD-MSG  EQUAL  'COR0006R1'
BR1217         PERFORM  3750-CHAMA-RUEC8850
BR1217         GO  TO  3500-99-FIM
BR1217     END-IF
BR0618     IF  RUEC60-COD-MSG  EQUAL  'COR0005E'
BR0618         PERFORM  3760-CHAMA-RUEC8860
BR0618         GO  TO  3500-99-FIM
BR0618     END-IF
           END-IF.
BRQ001     IF  RUEC60-COD-MSG  EQUAL  'COR0003R1' OR 'COR0003E'
BRQ001         GO  TO  3500-99-FIM
BRQ001     END-IF.

           IF  RUEC60-COD-MSG  EQUAL  'TMNF     '
           AND RCOR03-CRETOR-PRINC NUMERIC
           AND RCOR03-CRETOR-PRINC EQUAL ZEROS
7C0120         PERFORM  3650-TRATA-CONTINUACAO
               MOVE ZEROS             TO WRK-COUNT
               PERFORM 3900-ATUALIZA-NUOPER
               GO  TO  3500-99-FIM
           END-IF.

EW0116     IF  RUEC60-COD-MSG  EQUAL  'COR0001E '
BR0618     OR  WRK-FLAG        EQUAL  '1E'
EW0116         MOVE CCONTR-CREDT-RURAL OF RUECB09B
EW0116                                TO CCONTR             OF RUFIB002
BS0281         PERFORM 4600-SELECT-RUFIB002
BS0281         IF  CORIGE-REC OF RUFIB002 EQUAL 29 OR 31
BS0281             MOVE 13            TO CORIGE-REC         OF RUFIB002
BS0281                                   CORIGE-REC         OF RUECB007
EW0116             PERFORM 4200-ATUALIZA-RUFIB002
BS0281             PERFORM 4250-ATUALIZA-RUECB007
BS0281             PERFORM 4500-RETORNA-TAXA
BS0281         END-IF
EW0116     END-IF.

           MOVE ZEROS             TO WRK-COUNT
           PERFORM  3800-ATUALIZA-ERRO.

      *---------------------------------------------------------------*
       3500-99-FIM.                EXIT.
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
       3600-CHAMA-RUEC8806         SECTION.
      *---------------------------------------------------------------*

           IF  RCOR04-NRO-REF-BCCOR  EQUAL  SPACES  OR ZEROS
               GO  TO  3600-99-FIM
           END-IF.

           MOVE 'RUEC8806'              TO WRK-MODULO.
           INITIALIZE  RUEC61-AREA.
           MOVE 'RUEC8804'              TO RUEC61-PGM-CHAMADOR.
           MOVE RUEC60-NRO-CTRL-IF      TO RUEC61-NRO-CTRLIF.
           MOVE RCOR04-NRO-REF-BCCOR    TO RUEC61-CREFT-RECOR.
           CALL WRK-MODULO     USING  RUEC61-AREA.

           IF  RUEC61-COD-RET     NOT EQUAL ZEROS
               GO  TO  3600-99-FIM
           END-IF.
      *---------------------------------------------------------------*
       3600-99-FIM.                EXIT.
7C0120*---------------------------------------------------------------*
.     * TRATA CONTINUACAO DA COR0001
.     *---------------------------------------------------------------*
.      3650-TRATA-CONTINUACAO      SECTION.
.     *---------------------------------------------------------------*
.
.          PERFORM 3660-COUNT-RUECB09A
.          IF WRK-QTDREG-B09A NOT GREATER ZEROS
.              GO TO   3650-99-FIM
.          END-IF
.
.          PERFORM 3670-SELECT-RUECB09A.
.
.          PERFORM 3680-ENVIA-RCOR5005.
.
.     *---------------------------------------------------------------*
.      3650-99-FIM.                EXIT.
.     *---------------------------------------------------------------*
.     *---------------------------------------------------------------*
.      3660-COUNT-RUECB09A  SECTION.
.     *---------------------------------------------------------------*
.
.          MOVE RUEC60-NRO-CTRL-IF    TO CCTRL-RQUIS-FINCR  OF RUECB09B.
.          MOVE 1                     TO CSIT-MSGEM         OF RUECB09B.
.
.          EXEC SQL
.            SELECT  COUNT(*)
.            INTO  :WRK-QTDREG-B09A
.            FROM  DB2PRD.TENVIO_RECOR_TEMPR A
.                 ,DB2PRD.THIST_ENVIO_RECOR  B
.            WHERE B.CCTRL_RQUIS_FINCR    = :RUECB09B.CCTRL-RQUIS-FINCR
.            AND   A.CSIT_MSGEM           = :RUECB09B.CSIT-MSGEM
.            AND   A.CCONTR_CREDT_RURAL   = B.CCONTR_CREDT_RURAL
.            AND   A.NENVIO_RECOR         = B.NENVIO_RECOR
.            AND   B.CMSGEM_RECOR         = 'COR0001'
.          END-EXEC.
.
.          IF (SQLCODE                  NOT EQUAL ZEROS AND +100
.                                                      AND -305) OR
.             (SQLWARN0                 EQUAL 'W')
.              MOVE 08                   TO  RUEC60-COD-RETORNO
.              MOVE 'DB2'                TO  ERR-TIPO-ACESSO
.              MOVE 'TENVIO_RECOR_TEMPR' TO  ERR-DBD-TAB
.              MOVE 'COUNT'              TO  ERR-FUN-COMANDO
.              MOVE SQLCODE              TO  ERR-SQL-CODE
.              MOVE '3660'               TO  ERR-LOCAL
.              MOVE SPACES               TO  ERR-SEGM
               PERFORM 4000-ROTINA-FIM
.          END-IF.
.
.     *----------------------------------------------------------------*
.      3660-99-FIM.                    EXIT.
.     *----------------------------------------------------------------*
.
.     *---------------------------------------------------------------*
.      3670-SELECT-RUECB09A  SECTION.
.     *---------------------------------------------------------------*
.
.          MOVE CCONTR-CREDT-RURAL OF RUECB09B
.                                     TO CCONTR-CREDT-RURAL OF RUECB09A.
.          MOVE NENVIO-RECOR       OF RUECB09B
.                                     TO NENVIO-RECOR       OF RUECB09A.
.          MOVE 1                     TO CSIT-MSGEM         OF RUECB09A.
.
.          EXEC SQL
.            SELECT  CCONTR_CREDT_RURAL,
.                    NENVIO_RECOR,
.                    NREG_MSGEM_RECOR,
.                    WCONTD_MSGEM_RURAL
.             INTO  :RUECB09A.CCONTR-CREDT-RURAL,
.                   :RUECB09A.NENVIO-RECOR,
.                   :RUECB09A.NREG-MSGEM-RECOR,
.                   :RUECB09A.WCONTD-MSGEM-RURAL
.            FROM  DB2PRD.TENVIO_RECOR_TEMPR
.            WHERE CCONTR_CREDT_RURAL   = :RUECB09A.CCONTR-CREDT-RURAL
.            AND   NENVIO_RECOR         = :RUECB09A.NENVIO-RECOR
.            AND   CSIT_MSGEM           = :RUECB09A.CSIT-MSGEM
.            ORDER BY  CCONTR_CREDT_RURAL,
.                      NENVIO_RECOR,
.                      NREG_MSGEM_RECOR
.            FETCH FIRST 1 ROWS ONLY
.          END-EXEC.
.
.          IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
.             (SQLWARN0      EQUAL 'W')
.              MOVE 09                   TO  RUEC60-COD-RETORNO
.              MOVE 'DB2'                TO  ERR-TIPO-ACESSO
.              MOVE 'TENVIO_RECOR_TEMPR' TO  ERR-DBD-TAB
.              MOVE 'COUNT'              TO  ERR-FUN-COMANDO
.              MOVE SQLCODE              TO  ERR-SQL-CODE
.              MOVE '3660'               TO  ERR-LOCAL
.              MOVE SPACES               TO  ERR-SEGM
               PERFORM 4000-ROTINA-FIM
.          END-IF.
.
.     *----------------------------------------------------------------*
.      3670-99-FIM.                    EXIT.
.     *----------------------------------------------------------------*
.     *---------------------------------------------------------------*
.      3680-ENVIA-RCOR5005         SECTION.
.     *---------------------------------------------------------------*
.
.          INITIALIZE  RCOR10-REGISTRO.
.
.          IF WRK-QTDREG-B09A GREATER 1
.             MOVE 'S' TO  RCOR10-IND-CONTINUA
.          ELSE
.             MOVE 'N' TO  RCOR10-IND-CONTINUA
.          END-IF.
.
.          MOVE RUEC60-NRO-CTRL-IF  TO  RCOR10-NRO-CTRLIF
.
.          MOVE  'RUEC'                  TO RCOR10-CCUSTO.
.
.          MOVE ZEROS                    TO RCOR10-TAM-SISMSG.
.          MOVE SPACES                   TO RCOR10-SISMSG.
.          MOVE ZEROS                    TO WRK-ENVIO-LEN.
.          MOVE SPACES                   TO WRK-ENVIO-SIS.
.
.          MOVE  WCONTD-MSGEM-RURAL-LEN  OF RUECB09A
.                                        TO RCOR10-TAM-SISMSG.
.          MOVE  WCONTD-MSGEM-RURAL-TEXT
.                            OF RUECB09A(1: LENGTH OF RCOR10-SISMSG)
.                                        TO RCOR10-SISMSG.
.
.          MOVE  WCONTD-MSGEM-RURAL-LEN OF RUECB09A
.                                        TO WRK-ENVIO-LEN.
.          MOVE  WCONTD-MSGEM-RURAL-TEXT
.                            OF RUECB09A(1: LENGTH OF WRK-ENVIO-SIS)
.                                        TO WRK-ENVIO-SIS.
.
.          MOVE RUEC60-NRO-CTRL-IF  TO  WRK-ENVIO-NUMCTRLIF
.          MOVE RUEC60-SEQ-MSG  TO  WRK-CONTA-ENVIO
.          ADD   1              TO  WRK-CONTA-ENVIO
.          MOVE WRK-CONTA-ENVIO TO  RCOR10-SEQ-MSG
.
.          CALL WRK-MODULO-RCOR5005  USING  RCOR10-REGISTRO
.                                           ERRO-AREA.
.
.          IF RCOR10-COD-RETORNO EQUAL ZEROS
.             PERFORM 3690-UPDATE-B09A
.          ELSE
.             MOVE 10                   TO RUEC60-COD-RETORNO
.          END-IF.
.
.     *----------------------------------------------------------------*
.      3680-99-FIM.                    EXIT.
.     *----------------------------------------------------------------*
.
.     *---------------------------------------------------------------*
.      3690-UPDATE-B09A            SECTION.
.     *---------------------------------------------------------------*
.
.          MOVE WRK-ENVIO-LEN          TO WCONTD-MSGEM-RURAL-LEN
.                                         OF RUECB09A.
.
.          MOVE WRK-ENVIO-SIS          TO WCONTD-MSGEM-RURAL-TEXT
.                                         OF RUECB09A.
.
.          MOVE 2                      TO CSIT-MSGEM
.                                         OF RUECB09A.
.
.          EXEC SQL
.          UPDATE DB2PRD.TENVIO_RECOR_TEMPR
.          SET CSIT_MSGEM           = :RUECB09A.CSIT-MSGEM,
.              WCONTD_MSGEM_RURAL   = :RUECB09A.WCONTD-MSGEM-RURAL
.          WHERE CCONTR_CREDT_RURAL = :RUECB09A.CCONTR-CREDT-RURAL
.          AND   NENVIO_RECOR       = :RUECB09A.NENVIO-RECOR
.          AND   NREG_MSGEM_RECOR   = :RUECB09A.NREG-MSGEM-RECOR
.          END-EXEC.
.
.          IF (SQLCODE   NOT EQUAL ZEROS) OR
.             (SQLWARN0      EQUAL 'W')
.              MOVE 11                   TO RUEC60-COD-RETORNO
.              MOVE 'DB2'                TO ERR-TIPO-ACESSO
.              MOVE 'TENVIO_RECOR_TEMPR' TO ERR-DBD-TAB
.              MOVE 'UPDATE'             TO ERR-FUN-COMANDO
.              MOVE SQLCODE              TO ERR-SQL-CODE
.              MOVE '3690'               TO ERR-LOCAL
.              MOVE SPACES               TO ERR-SEGM
               PERFORM 4000-ROTINA-FIM
.          END-IF.
.
.     *----------------------------------------------------------------*
.      3690-99-FIM.                    EXIT.
7C0120*----------------------------------------------------------------*

      *---------------------------------------------------------------*
       3700-CHAMA-RUEC8816         SECTION.
      *---------------------------------------------------------------*

           MOVE 'RUEC8816'              TO WRK-MODULO.
           INITIALIZE  RUEC73-AREA.
           MOVE 'RUEC8804'              TO RUEC73-PGM-CHAMADOR.
           MOVE RUEC60-NRO-CTRL-IF      TO RUEC73-NRO-CTRLIF.
           CALL WRK-MODULO     USING  RUEC73-AREA.

           IF  RUEC73-COD-RET     NOT EQUAL ZEROS
               GO  TO  3700-99-FIM
           END-IF.

      *---------------------------------------------------------------*
       3700-99-FIM.                EXIT.
      *---------------------------------------------------------------*

BR1217*---------------------------------------------------------------*
BR1217 3750-CHAMA-RUEC8850         SECTION.
BR1217*---------------------------------------------------------------*
BR1217
BR1217     MOVE 'RUEC8850'              TO WRK-MODULO.
BR1217     INITIALIZE  RUEC87-AREA.
BR1217     MOVE 'RUEC8804'              TO RUEC87-PGM-CHAMADOR.
BR1217     MOVE RUEC60-NRO-CTRL-IF      TO RUEC87-NRO-CTRLIF.
BR1217     MOVE RUEC60-SISMSG           TO RUEC87-SISMSG.
BR1217     CALL WRK-MODULO     USING  RUEC87-AREA.
BR1217
BR1217     IF  RUEC87-COD-RET     NOT EQUAL ZEROS
BR1217         GO  TO  3750-99-FIM
BR1217     END-IF.
BR1217
BR1217*---------------------------------------------------------------*
BR1217 3750-99-FIM.                EXIT.
BR1217*---------------------------------------------------------------*

BR0618*---------------------------------------------------------------*
BR0618 3760-CHAMA-RUEC8860         SECTION.
BR0618*---------------------------------------------------------------*
BR0618
BR0618     MOVE 'RUEC8860'              TO WRK-MODULO.
BR0618     INITIALIZE  RUEC87-AREA.
BR0618     MOVE 'RUEC8804'              TO RUEC87-PGM-CHAMADOR.
BR0618     MOVE RUEC60-NRO-CTRL-IF      TO RUEC87-NRO-CTRLIF.
BR0618     MOVE RUEC60-SISMSG           TO RUEC87-SISMSG.

BR0618     CALL WRK-MODULO     USING  RUEC87-AREA.
BR0618
BR0618     IF  RUEC87-COD-RET     NOT EQUAL ZEROS
BR0618         MOVE RUEC87-COD-RET                 TO WRK-COD-MO
BR0618         MOVE WRK-COD-RET                    TO CTERM OF RUECB048
BR0618         MOVE RUEC87-RETORNO (1:300)
BR0618                           TO WRESUL-CONS-MDULO-TEXT OF RUECB048
BR0618         MOVE 300          TO WRESUL-CONS-MDULO-LEN  OF RUECB048
BR0618         EXEC SQL
BR0618           INSERT INTO DB2PRD.TAPOIO_UTILZ_MDULO (
BR0618                  CTERM
BR0618           ,      CIDTFD_MDULO
BR0618           ,      CSEQ_CTRL_MDULO
BR0618           ,      WRESUL_CONS_MDULO
BR0618           ) VALUES (
BR0618             :RUECB048.CTERM
BR0618           , 0
BR0618           , 0
BR0618           , CHAR ( CURRENT TIMESTAMP ) ||
BR0618             :RUECB048.WRESUL-CONS-MDULO )
BR0618         END-EXEC
BR0618         GO  TO  3760-99-FIM
BR0618     END-IF.
BR0618
BR0618*---------------------------------------------------------------*
BR0618 3760-99-FIM.                EXIT.
BR0618*---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3800-ATUALIZA-ERRO          SECTION.
      *---------------------------------------------------------------*
       3800-010.
      *---------------------------------------------------------------*

           ADD  1                  TO WRK-COUNT.
           MOVE 3                  TO CSIT-MSGEM OF RUECB09B.
           EXEC SQL
             UPDATE   DB2PRD.THIST_ENVIO_RECOR
                SET   CSIT_MSGEM = :RUECB09B.CSIT-MSGEM
                WHERE CCONTR_CREDT_RURAL = :RUECB09B.CCONTR-CREDT-RURAL
                  AND NENVIO_RECOR       = :RUECB09B.NENVIO-RECOR
                  AND HULT_ATULZ         = :RUECB09B.HULT-ATULZ
           END-EXEC.

           IF (SQLCODE              NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 12                  TO RUEC60-COD-RETORNO
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'THIST_ENVIO_RECOR ' TO ERR-DBD-TAB
               MOVE 'UPDATE'            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0040'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               MOVE SQLCA(1:136)        TO LNK-SQLCA
               PERFORM 4000-ROTINA-FIM
           END-IF.

           IF (SQLCODE   NOT EQUAL ZEROS AND -911 AND -913) OR
              (SQLWARN0                 EQUAL 'W')
               PERFORM 4000-ROTINA-FIM
           END-IF.

           IF  SQLCODE   EQUAL  -911 OR  -913
               IF  WRK-COUNT  LESS  WRK-LIMITE
                   GO  TO  3800-010
               ELSE
                   PERFORM 4000-ROTINA-FIM
               END-IF
           END-IF.

      *---------------------------------------------------------------*
       3800-99-FIM.                EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3900-ATUALIZA-NUOPER        SECTION.
      *---------------------------------------------------------------*
       3900-010.
      *---------------------------------------------------------------*

           ADD  1                  TO WRK-COUNT.
           MOVE RCOR03-CNRO-UNIC-OPER TO CUNIC-MSGEM OF RUECB09B
           EXEC SQL
             UPDATE   DB2PRD.THIST_ENVIO_RECOR
                SET   CUNIC_MSGEM = :RUECB09B.CUNIC-MSGEM
                WHERE CCONTR_CREDT_RURAL = :RUECB09B.CCONTR-CREDT-RURAL
                  AND NENVIO_RECOR       = :RUECB09B.NENVIO-RECOR
                  AND HULT_ATULZ         = :RUECB09B.HULT-ATULZ
           END-EXEC.

           IF (SQLCODE              NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 13                  TO RUEC60-COD-RETORNO
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'THIST_ENVIO_RECOR ' TO ERR-DBD-TAB
               MOVE 'UPDATE'            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0050'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               MOVE SQLCA(1:136)        TO LNK-SQLCA
               PERFORM 4000-ROTINA-FIM
           END-IF.

           IF (SQLCODE   NOT EQUAL ZEROS AND -911 AND -913) OR
              (SQLWARN0                 EQUAL 'W')
               PERFORM 4000-ROTINA-FIM
           END-IF.

           IF  SQLCODE   EQUAL  -911 OR  -913
               IF  WRK-COUNT  LESS  WRK-LIMITE
                   GO  TO  3900-010
               ELSE
                   PERFORM 4000-ROTINA-FIM
               END-IF
           END-IF.

      *---------------------------------------------------------------*
       3900-99-FIM.                EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       4000-ROTINA-FIM             SECTION.
      *---------------------------------------------------------------*

           GOBACK.

      *---------------------------------------------------------------*
       4000-99-FIM.                EXIT.
      *---------------------------------------------------------------**

      *---------------------------------------------------------------*
EW0116 4200-ATUALIZA-RUFIB002      SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
               UPDATE   DB2PRD.CONTRATO_RURAL
                 SET    CORIGE_REC  = :RUFIB002.CORIGE-REC
                 WHERE CCONTR             = :RUFIB002.CCONTR
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 14                  TO RUEC60-COD-RETORNO
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'CONTRATO_RURAL   ' TO ERR-DBD-TAB
               MOVE 'UPDATE'            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0060'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               MOVE SQLCA(1:136)        TO LNK-SQLCA
               PERFORM 4000-ROTINA-FIM
           END-IF.

      *---------------------------------------------------------------*
EW0116 4200-99-FIM.                EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
BS0281 4250-ATUALIZA-RUECB007      SECTION.
      *---------------------------------------------------------------*

           MOVE CJUNC-DEPDC OF RUFIB002 TO CJUNC-DEPDC OF RUECB007.
           MOVE CCDULA      OF RUFIB002 TO CCDULA      OF RUECB007.

           EXEC SQL
               UPDATE   DB2PRD.CEDULA_TEMPR
                 SET    CORIGE_REC  = :RUECB007.CORIGE-REC
                WHERE   CJUNC_DEPDC = :RUECB007.CJUNC-DEPDC
                 AND    CCDULA      = :RUECB007.CCDULA
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 15                  TO RUEC60-COD-RETORNO
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'CEDULA_TEMPR     ' TO ERR-DBD-TAB
               MOVE 'UPDATE'            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0070'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               MOVE SQLCA(1:136)        TO LNK-SQLCA
               PERFORM 4000-ROTINA-FIM
           END-IF.

      *---------------------------------------------------------------*
BS0281 4250-99-FIM.                EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
BS0281 4500-RETORNA-TAXA     SECTION.
      *---------------------------------------------------------------*

           PERFORM 4510-SELECT-RUECB007.

           PERFORM 4520-DELETE-RUECV010.

           PERFORM 4530-DELETE-RUECV012.

           PERFORM 4540-OBTER-TAXA.

           PERFORM 4550-INSERT-RUECV010.

           PERFORM 4560-INSERT-RUECV012.

      *----------------------------------------------------------------*
BS0281 4500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
BS0281 4510-SELECT-RUECB007 SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
               SELECT
                   CTPO_PROTR_RURAL,
                   CFNALD_RURAL,
                   CPRODT_RURAL
               INTO
                   :RUECB007.CTPO-PROTR-RURAL,
                   :RUECB007.CFNALD-RURAL,
                   :RUECB007.CPRODT-RURAL
               FROM DB2PRD.CEDULA_TEMPR
               WHERE CJUNC_DEPDC       = :RUECB007.CJUNC-DEPDC
                 AND CCDULA            = :RUECB007.CCDULA
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS ) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 16                  TO RUEC60-COD-RETORNO
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'CEDULA_TEMPR'     TO  ERR-DBD-TAB
               MOVE 'SELECT'           TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '0080'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               MOVE SQLCA(1:136)        TO LNK-SQLCA
               PERFORM 4000-ROTINA-FIM
           END-IF.

      *----------------------------------------------------------------*
BS0281 4510-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
BS0281 4520-DELETE-RUECV010 SECTION.
      *---------------------------------------------------------------*

           MOVE SPACES                 TO WRK-FIM-RUECV010.

           PERFORM 4521-ABRIR-CSR01-RUECV010.

           PERFORM 4522-FETCH-CSR01-RUECV010.

           PERFORM 4523-PROCESSA-CSR01-RUECV010
                        UNTIL WRK-FIM-RUECV010 EQUAL 'S'.

           PERFORM 4524-CLOSE-CSR01-RUECV010.

      *----------------------------------------------------------------*
BS0281 4520-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
BS0281 4521-ABRIR-CSR01-RUECV010 SECTION.
      *---------------------------------------------------------------*

           MOVE 8200                    TO WRK-CTPO-INI.
           MOVE 8399                    TO WRK-CTPO-FIM.
           MOVE CJUNC-DEPDC OF RUFIB002 TO CJUNC-DEPDC  OF RUECV010.
           MOVE CCDULA      OF RUFIB002 TO CCDULA       OF RUECV010.

           EXEC SQL
                OPEN CSR01-RUECV010
           END-EXEC.

           IF (SQLCODE           NOT EQUAL   +0 AND +100)    OR
              (SQLWARN0              EQUAL  'W')
               MOVE 17                  TO RUEC60-COD-RETORNO
               MOVE 'DB2'                    TO  ERR-TIPO-ACESSO
               MOVE 'V01EVNTO_CDULA'         TO  ERR-DBD-TAB
               MOVE 'OPEN    '               TO  ERR-FUN-COMANDO
               MOVE SQLCODE                  TO  ERR-SQL-CODE
               MOVE '0090'                   TO  ERR-LOCAL
               MOVE SPACES                   TO  ERR-SEGM
               MOVE SQLCA(1:136)        TO LNK-SQLCA
               PERFORM 4000-ROTINA-FIM
           END-IF.

      *---------------------------------------------------------------*
BS0281 4521-99-FIM.  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
BS0281 4522-FETCH-CSR01-RUECV010 SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
                FETCH CSR01-RUECV010 INTO
                     :RUECV010.CJUNC-DEPDC,
                     :RUECV010.CCDULA,
                     :RUECV010.CTPO-EVNTO-RURAL,
                     :RUECV010.DEVNTO,
                     :RUECV010.VEVNTO-RURAL
           END-EXEC.

           IF (SQLCODE           NOT EQUAL   +0 AND +100)    OR
              (SQLWARN0              EQUAL  'W')
               MOVE 18                  TO RUEC60-COD-RETORNO
               MOVE 'DB2'                    TO  ERR-TIPO-ACESSO
               MOVE 'V01EVNTO_CDULA'         TO  ERR-DBD-TAB
               MOVE 'FECTH   '               TO  ERR-FUN-COMANDO
               MOVE SQLCODE                  TO  ERR-SQL-CODE
               MOVE '0100'                   TO  ERR-LOCAL
               MOVE SPACES                   TO  ERR-SEGM
               MOVE SQLCA(1:136)        TO LNK-SQLCA
               PERFORM 4000-ROTINA-FIM
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-RUECV010
           END-IF.

      *---------------------------------------------------------------*
BS0281 4522-99-FIM.  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
BS0281 4523-PROCESSA-CSR01-RUECV010 SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
                DELETE FROM DB2PRD.V01EVNTO_CDULA
                   WHERE CURRENT OF CSR01-RUECV010
           END-EXEC.

           IF (SQLCODE           NOT EQUAL   +0 AND +100)    OR
              (SQLWARN0              EQUAL  'W')
               MOVE 19                  TO RUEC60-COD-RETORNO
               MOVE 'DB2'                    TO  ERR-TIPO-ACESSO
               MOVE 'V01EVNTO_CDULA'         TO  ERR-DBD-TAB
               MOVE 'DELETE  '               TO  ERR-FUN-COMANDO
               MOVE SQLCODE                  TO  ERR-SQL-CODE
               MOVE '0110'                   TO  ERR-LOCAL
               MOVE SPACES                   TO  ERR-SEGM
               MOVE SQLCA(1:136)        TO LNK-SQLCA
               PERFORM 4000-ROTINA-FIM
           END-IF.

           PERFORM 4522-FETCH-CSR01-RUECV010.

      *---------------------------------------------------------------*
BS0281 4523-99-FIM.  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
BS0281 4524-CLOSE-CSR01-RUECV010       SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
                CLOSE CSR01-RUECV010
           END-EXEC.

           IF (SQLCODE           NOT EQUAL   +0 AND +100)    OR
              (SQLWARN0              EQUAL  'W')
               MOVE 10                  TO RUEC60-COD-RETORNO
               MOVE 'DB2'                    TO  ERR-TIPO-ACESSO
               MOVE 'V01EVNTO_CDULA'         TO  ERR-DBD-TAB
               MOVE 'CLOSE   '               TO  ERR-FUN-COMANDO
               MOVE SQLCODE                  TO  ERR-SQL-CODE
               MOVE '0120'                   TO  ERR-LOCAL
               MOVE SPACES                   TO  ERR-SEGM
               MOVE SQLCA(1:136)        TO LNK-SQLCA
               PERFORM 4000-ROTINA-FIM
           END-IF.

      *---------------------------------------------------------------*
BS0281 4524-99-FIM.  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
BS0281 4530-DELETE-RUECV012 SECTION.
      *---------------------------------------------------------------*

           MOVE SPACES                 TO WRK-FIM-RUECV012.

           PERFORM 4531-ABRIR-CSR01-RUECV012.

           PERFORM 4532-FETCH-CSR01-RUECV012.

           PERFORM 4533-PROCESSA-CSR01-RUECV012
                        UNTIL WRK-FIM-RUECV012 EQUAL 'S'.

           PERFORM 4534-CLOSE-CSR01-RUECV012.

      *----------------------------------------------------------------*
BS0281 4530-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
BS0281 4531-ABRIR-CSR01-RUECV012 SECTION.
      *---------------------------------------------------------------*

           MOVE 8200                    TO WRK-CTPO-INI.
           MOVE 8399                    TO WRK-CTPO-FIM.
           MOVE CCONTR OF RUFIB002      TO CCONTR OF RUECV012.

           EXEC SQL
                OPEN CSR01-RUECV012
           END-EXEC.

           IF (SQLCODE           NOT EQUAL   +0 AND +100)    OR
              (SQLWARN0              EQUAL  'W')
               MOVE 21                  TO RUEC60-COD-RETORNO
               MOVE 'DB2'                    TO  ERR-TIPO-ACESSO
               MOVE 'V01EVNTO_RURAL'         TO  ERR-DBD-TAB
               MOVE 'OPEN    '               TO  ERR-FUN-COMANDO
               MOVE SQLCODE                  TO  ERR-SQL-CODE
               MOVE '0130'                   TO  ERR-LOCAL
               MOVE SPACES                   TO  ERR-SEGM
               MOVE SQLCA(1:136)        TO LNK-SQLCA
               PERFORM 4000-ROTINA-FIM
           END-IF.

      *---------------------------------------------------------------*
BS0281 4531-99-FIM.  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
BS0281 4532-FETCH-CSR01-RUECV012 SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
                FETCH CSR01-RUECV012 INTO
                     :RUECV012.CCONTR,
                     :RUECV012.CTPO-EVNTO-RURAL,
                     :RUECV012.DEVNTO,
                     :RUECV012.VEVNTO-RURAL,
                     :RUECV012.VPCELA-REC-PPRIO
           END-EXEC.

           IF (SQLCODE           NOT EQUAL   +0 AND +100)    OR
              (SQLWARN0              EQUAL  'W')
               MOVE 22                  TO RUEC60-COD-RETORNO
               MOVE 'DB2'                    TO  ERR-TIPO-ACESSO
               MOVE 'V01EVNTO_RURAL'         TO  ERR-DBD-TAB
               MOVE 'FECTH   '               TO  ERR-FUN-COMANDO
               MOVE SQLCODE                  TO  ERR-SQL-CODE
               MOVE '0140'                   TO  ERR-LOCAL
               MOVE SPACES                   TO  ERR-SEGM
               MOVE SQLCA(1:136)        TO LNK-SQLCA
               PERFORM 4000-ROTINA-FIM
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-RUECV012
           END-IF.

      *---------------------------------------------------------------*
BS0281 4532-99-FIM.  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
BS0281 4533-PROCESSA-CSR01-RUECV012 SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
                DELETE FROM DB2PRD.V01EVNTO_RURAL
                   WHERE CURRENT OF CSR01-RUECV012
           END-EXEC.

           IF (SQLCODE           NOT EQUAL   +0 AND +100)    OR
              (SQLWARN0              EQUAL  'W')
               MOVE 23                  TO RUEC60-COD-RETORNO
               MOVE 'DB2'                    TO  ERR-TIPO-ACESSO
               MOVE 'V01EVNTO_RURAL'         TO  ERR-DBD-TAB
               MOVE 'DELETE  '               TO  ERR-FUN-COMANDO
               MOVE SQLCODE                  TO  ERR-SQL-CODE
               MOVE '0150'                   TO  ERR-LOCAL
               MOVE SPACES                   TO  ERR-SEGM
               MOVE SQLCA(1:136)        TO LNK-SQLCA
               PERFORM 4000-ROTINA-FIM
           END-IF.

           PERFORM 4532-FETCH-CSR01-RUECV012.

      *---------------------------------------------------------------*
BS0281 4533-99-FIM.  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
BS0281 4534-CLOSE-CSR01-RUECV012       SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
                CLOSE CSR01-RUECV012
           END-EXEC.

           IF (SQLCODE           NOT EQUAL   +0 AND +100)    OR
              (SQLWARN0              EQUAL  'W')
               MOVE 24                  TO RUEC60-COD-RETORNO
               MOVE 'DB2'                    TO  ERR-TIPO-ACESSO
               MOVE 'V01EVNTO_RURAL'         TO  ERR-DBD-TAB
               MOVE 'CLOSE   '               TO  ERR-FUN-COMANDO
               MOVE SQLCODE                  TO  ERR-SQL-CODE
               MOVE '0160'                   TO  ERR-LOCAL
               MOVE SPACES                   TO  ERR-SEGM
               MOVE SQLCA(1:136)        TO LNK-SQLCA
               PERFORM 4000-ROTINA-FIM
           END-IF.

      *---------------------------------------------------------------*
BS0281 4534-99-FIM.  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
BS0281 4540-OBTER-TAXA                 SECTION.
      *---------------------------------------------------------------*

           MOVE 13                       TO CMIN-ORIGE-REC   OF RURCB051
                                            CMAX-ORIGE-REC   OF RURCB051
           MOVE CFNALD-RURAL     OF RUECB007
                                         TO CMIN-FNALD-RURAL OF RURCB051
                                            CMAX-FNALD-RURAL OF RURCB051
           MOVE CTPO-PROTR-RURAL OF RUECB007
                                         TO CMIN-TPO-PROTR   OF RURCB051
                                            CMAX-TPO-PROTR   OF RURCB051
           MOVE CPRODT-RURAL     OF RUECB007
                                         TO CMIN-PRODT-RURAL OF RURCB051
                                            CMAX-PRODT-RURAL OF RURCB051

           EXEC SQL
                SELECT CFORMA_RURAL_JURO,
                       PJURO,
                       CFORMA_RURAL_CM,
                       PUTILZ_INDCD_DESC
                INTO   :RURCB051.CFORMA-RURAL-JURO,
                       :RURCB051.PJURO,
                       :RURCB051.CFORMA-RURAL-CM
                           :WRK-CFORMA-RURAL-CM-NULL,
                       :RURCB051.PUTILZ-INDCD-DESC
                           :WRK-PUTILZ-INDCD-DESC-NULL
                FROM   DB2PRD.SEQ_TAXA_RURAL
                WHERE  CMIN_ORIGE_REC   <= :RURCB051.CMIN-ORIGE-REC
                  AND  CMAX_ORIGE_REC   >= :RURCB051.CMAX-ORIGE-REC
                  AND  CMIN_FNALD_RURAL <= :RURCB051.CMIN-FNALD-RURAL
                  AND  CMAX_FNALD_RURAL >= :RURCB051.CMAX-FNALD-RURAL
                  AND  CMIN_TPO_PROTR   <= :RURCB051.CMIN-TPO-PROTR
                  AND  CMAX_TPO_PROTR   >= :RURCB051.CMAX-TPO-PROTR
                  AND  CMIN_PRODT_RURAL <= :RURCB051.CMIN-PRODT-RURAL
                  AND  CMAX_PRODT_RURAL >= :RURCB051.CMAX-PRODT-RURAL
           END-EXEC.

           IF ( SQLCODE NOT EQUAL ZEROS AND +100) OR
              ( SQLWARN0    EQUAL 'W' )
               MOVE 25                  TO RUEC60-COD-RETORNO
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'SEQ_TAXA_RURAL'   TO  ERR-DBD-TAB
               MOVE 'SELECT'           TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0170'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE SQLCA(1:136)        TO LNK-SQLCA
               PERFORM 4000-ROTINA-FIM
           END-IF.

           IF  WRK-PUTILZ-INDCD-DESC-NULL NOT EQUAL ZEROS
               MOVE ZEROS            TO PUTILZ-INDCD-DESC OF RURCB051
           END-IF.
           IF  WRK-CFORMA-RURAL-CM-NULL NOT EQUAL ZEROS
               MOVE ZEROS            TO CFORMA-RURAL-CM   OF RURCB051
           END-IF.

      *---------------------------------------------------------------*
BS0281 4540-99-FIM.  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
BS0281 4550-INSERT-RUECV010           SECTION.
      *---------------------------------------------------------------*

      *** DADOS DE JUROS ***

           MOVE CJUNC-DEPDC OF RUFIB002 TO CJUNC-DEPDC  OF RUECV010.
           MOVE CCDULA      OF RUFIB002 TO CCDULA       OF RUECV010.
           MOVE PJURO       OF RURCB051 TO VEVNTO-RURAL OF RUECV010.
           MOVE 82                      TO WRK-EVENT-TP.
           MOVE CFORMA-RURAL-JURO OF RURCB051
                                        TO WRK-NUM-03.
           MOVE WRK-NUM-02              TO WRK-EVENT-CD.
           MOVE WRK-EVENTO              TO CTPO-EVNTO-RURAL OF RUECV010.

           EXEC SQL
                INSERT INTO DB2PRD.V01EVNTO_CDULA
                     (CJUNC_DEPDC, CCDULA,
                      CTPO_EVNTO_RURAL, DEVNTO,
                      VEVNTO_RURAL)
                VALUES
                     (:RUECV010.CJUNC-DEPDC, :RUECV010.CCDULA,
                     :RUECV010.CTPO-EVNTO-RURAL, CURRENT DATE,
                     :RUECV010.VEVNTO-RURAL)
           END-EXEC.

           IF (SQLCODE           NOT EQUAL   +0)    OR
              (SQLWARN0              EQUAL  'W')
               MOVE 26                  TO RUEC60-COD-RETORNO
               MOVE 'DB2'                    TO  ERR-TIPO-ACESSO
               MOVE 'V01EVNTO_CDULA'         TO  ERR-DBD-TAB
               MOVE 'INSERT  '               TO  ERR-FUN-COMANDO
               MOVE SQLCODE                  TO  ERR-SQL-CODE
               MOVE '0180'                   TO  ERR-LOCAL
               MOVE SPACES                   TO  ERR-SEGM
               MOVE SQLCA(1:136)        TO LNK-SQLCA
               PERFORM 4000-ROTINA-FIM
           END-IF.

      *** DADOS DE CORRECAO MONETARIA ***

           MOVE PUTILZ-INDCD-DESC OF RURCB051
                                        TO VEVNTO-RURAL OF RUECV010.
           MOVE 83                      TO WRK-EVENT-TP.
           MOVE CFORMA-RURAL-CM   OF RURCB051
                                        TO WRK-NUM-03.
           MOVE WRK-NUM-02              TO WRK-EVENT-CD.
           MOVE WRK-EVENTO              TO CTPO-EVNTO-RURAL OF RUECV010.

           EXEC SQL
                INSERT INTO DB2PRD.V01EVNTO_CDULA
                     (CJUNC_DEPDC, CCDULA,
                      CTPO_EVNTO_RURAL, DEVNTO,
                      VEVNTO_RURAL)
                VALUES
                     (:RUECV010.CJUNC-DEPDC, :RUECV010.CCDULA,
                     :RUECV010.CTPO-EVNTO-RURAL, CURRENT DATE,
                     :RUECV010.VEVNTO-RURAL)
           END-EXEC.

           IF (SQLCODE           NOT EQUAL   +0)    OR
              (SQLWARN0              EQUAL  'W')
               MOVE 27                  TO RUEC60-COD-RETORNO
               MOVE 'DB2'                    TO  ERR-TIPO-ACESSO
               MOVE 'V01EVNTO_CDULA'         TO  ERR-DBD-TAB
               MOVE 'INSERT  '               TO  ERR-FUN-COMANDO
               MOVE SQLCODE                  TO  ERR-SQL-CODE
               MOVE '0190'                   TO  ERR-LOCAL
               MOVE SPACES                   TO  ERR-SEGM
               MOVE SQLCA(1:136)        TO LNK-SQLCA
               PERFORM 4000-ROTINA-FIM
           END-IF.

      *---------------------------------------------------------------*
BS0281 4550-99-FIM.  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
BS0281 4560-INSERT-RUECV012           SECTION.
      *---------------------------------------------------------------*

      *** DADOS DE JUROS ***

           MOVE CCONTR OF RUFIB002      TO CCONTR       OF RUECV012.
           MOVE PJURO       OF RURCB051 TO VEVNTO-RURAL OF RUECV012.
           MOVE 82                      TO WRK-EVENT-TP.
           MOVE CFORMA-RURAL-JURO OF RURCB051
                                        TO WRK-NUM-03.
           MOVE WRK-NUM-02              TO WRK-EVENT-CD.
           MOVE WRK-EVENTO              TO CTPO-EVNTO-RURAL OF RUECV012.


           EXEC SQL
                INSERT INTO DB2PRD.V01EVNTO_RURAL
                     (CCONTR,
                      CTPO_EVNTO_RURAL, DEVNTO,
                      VEVNTO_RURAL)
                VALUES
                     (:RUECV012.CCONTR,
                     :RUECV012.CTPO-EVNTO-RURAL, CURRENT DATE,
                     :RUECV012.VEVNTO-RURAL)
           END-EXEC.

           IF (SQLCODE           NOT EQUAL   +0)    OR
              (SQLWARN0              EQUAL  'W')
               MOVE 28                  TO RUEC60-COD-RETORNO
              MOVE 'DB2'                    TO  ERR-TIPO-ACESSO
              MOVE 'V01EVNTO_RURAL'         TO  ERR-DBD-TAB
              MOVE 'INSERT  '               TO  ERR-FUN-COMANDO
              MOVE SQLCODE                  TO  ERR-SQL-CODE
              MOVE '0200'                   TO  ERR-LOCAL
              MOVE SPACES                   TO  ERR-SEGM
               MOVE SQLCA(1:136)        TO LNK-SQLCA
              PERFORM 4000-ROTINA-FIM
           END-IF.

      *** DADOS DE CORRECAO MONETARIA ***

           MOVE PUTILZ-INDCD-DESC OF RURCB051
                                        TO VEVNTO-RURAL     OF RUECV012.
           MOVE 83                      TO WRK-EVENT-TP.
           MOVE CFORMA-RURAL-CM   OF RURCB051
                                        TO WRK-NUM-03.
           MOVE WRK-NUM-02              TO WRK-EVENT-CD.
           MOVE WRK-EVENTO              TO CTPO-EVNTO-RURAL OF RUECV012.

           EXEC SQL
                INSERT INTO DB2PRD.V01EVNTO_RURAL
                     (CCONTR,
                      CTPO_EVNTO_RURAL, DEVNTO,
                      VEVNTO_RURAL)
                VALUES
                     (:RUECV012.CCONTR,
                     :RUECV012.CTPO-EVNTO-RURAL, CURRENT DATE,
                     :RUECV012.VEVNTO-RURAL)
           END-EXEC.

           IF (SQLCODE           NOT EQUAL   +0)    OR
              (SQLWARN0              EQUAL  'W')
               MOVE 29                  TO RUEC60-COD-RETORNO
               MOVE 'DB2'                    TO  ERR-TIPO-ACESSO
               MOVE 'V01EVNTO_RURAL'         TO  ERR-DBD-TAB
               MOVE 'INSERT  '               TO  ERR-FUN-COMANDO
               MOVE SQLCODE                  TO  ERR-SQL-CODE
               MOVE '0210'                   TO  ERR-LOCAL
               MOVE SPACES                   TO  ERR-SEGM
               MOVE SQLCA(1:136)        TO LNK-SQLCA
               PERFORM 4000-ROTINA-FIM
           END-IF.

      *---------------------------------------------------------------*
BS0281 4560-99-FIM.  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
BS0281 4600-SELECT-RUFIB002 SECTION.
      *---------------------------------------------------------------*

           EXEC SQL

               SELECT   CCDULA,
                        CJUNC_DEPDC,
                        CORIGE_REC
               INTO     :RUFIB002.CCDULA,
                        :RUFIB002.CJUNC-DEPDC,
                        :RUFIB002.CORIGE-REC
                          :WRK-CORIGE-REC-NULL
                 FROM DB2PRD.CONTRATO_RURAL
                WHERE CCONTR   = :RUFIB002.CCONTR
           END-EXEC.

           IF (SQLCODE             NOT EQUAL ZEROS AND +100 AND -811) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 30                  TO RUEC60-COD-RETORNO
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'CONTRATO_RURAL'    TO ERR-DBD-TAB
               MOVE 'SELECT'            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0220'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               MOVE SQLCA(1:136)        TO LNK-SQLCA
               PERFORM 4000-ROTINA-FIM
           END-IF.

           IF  SQLCODE                  EQUAL +100
               MOVE 31                  TO RUEC60-COD-RETORNO
               PERFORM 4000-ROTINA-FIM
           END-IF.

           IF WRK-CORIGE-REC-NULL       LESS ZEROS
              MOVE ZEROS                TO CORIGE-REC OF RUFIB002
           END-IF.


      *----------------------------------------------------------------*
BS0281 4600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
