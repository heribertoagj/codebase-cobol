      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID.     RUCA6895.
       AUTHOR.         EVANDRO GUIMARAES.
      *================================================================*
      *                   S O N D A   I T                              *
      *----------------------------------------------------------------*
      *     PROGRAMA....: RUCA6895                                     *
      *     ANALISTA....: EVANDRO GUIMARAES                            *
      *     DATA........: 09/01/2011                                   *
      *----------------------------------------------------------------*
      *     OBJETIVO....: CONSULTA INCONSISTENCIAS RETORNADAS NO RECOR *
      *                   OU TMNF                                      *
      *----------------------------------------------------------------*
      *     TELAS.......:                                              *
      *     RUCAT895....: CONSULTA INCONSISTENCIAS RETORNADAS NO RECOR *
      *                   OU TMNF                                      *
      *----------------------------------------------------------------*
      *     INC'S.......:                                              *
      *     I#BRAD7C - AREA DE ERRO DA BRAD7100                        *
      *----------------------------------------------------------------*
      *     DB2.........:                                              *
      *            COPYBOOK                 DCLGEN                     *
      *            RUECB084                 DB2PRD.TEMPTO_FINAN_RURAL  *
      *----------------------------------------------------------------*
      *     MODULOS.....:                                              *
      *     BRAD0660 - INSERIR CARACTERES DE NULL NA MENSAGEM DE SAIDA *
      *     BRAD7100 - TRATAMENTO DE ERROS QUANDO PROGRAMA INVALIDO    *
      *     POOL5005 - CONTROLE DO TRANSITO DE MENSAGENS NO IMS/DC     *
      *----------------------------------------------------------------*
      *     NAVEGACAO...:                                              *
      *     CHAMADO POR.:                    CHAMA:                    *
      *================================================================*
      *================================================================*
      *                                                                *
      * BSI TECNOLOGIA.                                                *
      *                                                                *
      * ALTERACAO: BS0000  - SICOR - ALTERACAO EXTRAORDINARIA NO       *
      *                      CATALOGO DE MENSAGEM.                     *
      * DATA.....: DEZEMBRO/2013                                       *
      * ANALISTA.: DANILO KIOROGLO                                     *
      *                                                                *
      *================================================================*
EG0615*================================================================*
EG0615*                                                                *
EG0615* BSI TECNOLOGIA.                                                *
EG0615*                                                                *
EG0615* ALTERACAO: INCLUSAO DO CAMPO SUBPROGRAMA                       *
EG0615* DATA.....: JUNHO/2015                                          *
EG0615* ANALISTA.: EVANDRO GUIMARAES                                   *
EG0615*                                                                *
EG0615*================================================================*
EG0615
BSI4.8*================================================================*
BSI4.8*             B S I   T E C N O L O G I A                      *
BSI4.8*================================================================*
BSI4.8* ALTERACAO: ADAPTACAO A VERSAO 4.08 DO CATALOGO SICOR           *
BSI4.8* DATA.....: SETEMBRO/2016                                       *
BSI4.8* ANALISTA.: LEONARDO MANÇO CAMARGO                              *
BSI4.8*================================================================*
BSI4.8
      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *================================================================*
       DATA                             DIVISION.
      *================================================================*
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** RUCA6895 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DE ATRIBUTOS ***'.
      *----------------------------------------------------------------*

       01  WRK-241                     PIC S9(008) COMP  VALUE +241.
       01  WRK-49369                   PIC S9(008) COMP  VALUE +49369.
       01  WRK-49345                   PIC S9(008) COMP  VALUE +49345.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*

       77  WRK-CMSGEM-RECOR            PIC  X(009)         VALUE SPACES.
       77  WRK-ERRO                    PIC  X(008)         VALUE SPACES.
       77  WRK-CSEQ-RUECB048           PIC S9(005) COMP-3  VALUE ZEROS.
       77  WRK-FIM-REPET               PIC  X(001)         VALUE SPACES.
       77  WRK-FIM-ERRO                PIC  X(001)         VALUE SPACES.
       77  WRK-FIM-RUECB09C            PIC  X(001)         VALUE SPACES.
       77  WRK-DESLOCA                 PIC S9(005) COMP-3  VALUE ZEROS.
       77  WRK-POS                     PIC S9(005) COMP-3  VALUE ZEROS.
       01  WRK-IND-1                   PIC  9(009) COMP-3  VALUE ZEROS.
       01  WRK-IND-2                   PIC  9(009) COMP-3  VALUE ZEROS.
EG0617 01  IND-4                       PIC  9(009) COMP-3  VALUE ZEROS.

       01  WRK-INCONSIS                PIC  X(001)         VALUE SPACES.
       01  WRK-MODULO                  PIC  X(008)         VALUE SPACES.

       01  WRK-PAGINA-AUX              PIC  9(005) COMP-3  VALUE ZEROS.
       01  WRK-DESPREZAR               PIC  9(009) COMP-3  VALUE ZEROS.

       01  WRK-MENSAGEM-AUX.
           05  WRK-MENSAGEM-AUX-1      PIC  X(068)         VALUE SPACES.
           05  WRK-MENSAGEM-AUX-2      PIC  X(012)         VALUE SPACES.

       01  WRK-RUECB048-NULL.
           05  WRK-WRESUL-CONS-MDULO-LEN-NULL
                                           PIC S9(004) COMP VALUE ZEROS.
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA POOL5000 ***'.
      *----------------------------------------------------------------*

       77  WRK-GU                      PIC  X(004)         VALUE 'GU'.
       77  WRK-CHNG                    PIC  X(004)         VALUE 'CHNG'.
       77  WRK-ISRT                    PIC  X(004)         VALUE 'ISRT'.
       01  WRK-COD-USER-X.
           05  WRK-COD-USER            PIC  9(007)         VALUE ZEROS.
       77  WRK-FUNCAO                  PIC  X(004)         VALUE SPACES.
       77  WRK-TELA                    PIC  X(008)         VALUE SPACES.
       77  WRK-NOVA-OPCAO              PIC  9(002)         VALUE ZEROS.
       77  WRK-OPCAO-ATUAL             PIC  9(002)         VALUE ZEROS.

       01  WRK-COD-DEPTO.
           05  FILLER                  PIC  X(002)         VALUE SPACES.
           05  WRK-COD-DEPTO-N         PIC  9(004)         VALUE ZEROS.

       01  WRK-MENSAGEM.
           05  FILLER                  PIC  X(004)         VALUE SPACES.
           05  WRK-TRANSACAO           PIC  X(008)         VALUE SPACES.
           05  FILLER                  PIC  X(1988)        VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DE ERRO DA BRAD0660 ***'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-BRAD0660.
           05  FILLER                  PIC  X(045)       VALUE
               '* RETURN-CODE DIFERENTE DE ZEROS NA BRAD0660'.
           05  FILLER                  PIC  X(012)       VALUE
               '- RET.CODE ='.
           05  WRK-RETURN-CODE         PIC  9(002)       VALUE ZEROS.
           05  FILLER                  PIC  X(011)       VALUE
               ' - LOCAL ='.
           05  WRK-LOCAL-ERRO          PIC  9(002)       VALUE ZEROS.
           05  FILLER                  PIC  X(002)       VALUE ' *'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DA TELA RUCAT895 - INPUT ***'.
      *----------------------------------------------------------------*

       01  WRK-INPUT-T895.
           05  FILLER                        PIC  X(018) VALUE SPACES.
           05  WRK-INP-PFK-T895              PIC  X(001) VALUE SPACES.
           05  WRK-INP-SENHAS-T895           PIC  X(037) VALUE SPACES.
           05  WRK-INP-NOPCAO-T895           PIC  X(002) VALUE SPACES.
           05  WRK-INP-OCULTO-T895.
               10  WRK-INP-FASE-T895         PIC  9(001) VALUE ZEROS.
               10  WRK-INP-PAGINA-T895       PIC  9(005) VALUE ZEROS.
               10  WRK-INP-FIMAMOS-T895      PIC  X(001) VALUE SPACES.
               10  WRK-INP-CONTROLE-T895     PIC  9(005) VALUE ZEROS.
               10  FILLER                    PIC  X(013) VALUE SPACES.
           05  WRK-INP-DADOS-T895.
               10  WRK-INP-AGENCIA-T895.
                   15  WRK-INP-AGENCIA-T895-N
                                             PIC  9(004) VALUE ZEROS.
               10  WRK-INP-CEDULA-T895.
                   15  WRK-INP-CEDULA-T895-N
                                             PIC  9(009) VALUE ZEROS.
               10  WRK-INP-CONTRATO-T895.
                   15  WRK-INP-CONTRATO-T895-N
                                             PIC  9(009) VALUE ZEROS.
               10  FILLER                    OCCURS 07 TIMES.
                   15  WRK-INP-ERRO-T895     PIC  X(008) VALUE SPACES.
                   15  WRK-INP-DESC1-T895    PIC  X(068) VALUE SPACES.
                   15  WRK-INP-DESC2-T895    PIC  X(068) VALUE SPACES.
               10  WRK-INP-MENSA-T895        PIC  X(079) VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(50)      VALUE
           '*** AREA DA TELA RUEC3AIM  - OUTPUT ***'.
      *----------------------------------------------------------------*

       01  WRK-OUTPUT-T895.
           05  WRK-OUT-LL-T895           PIC S9(004) COMP VALUE ZEROS.
           05  WRK-OUT-ZZ-T895           PIC S9(004) COMP VALUE ZEROS.
           05  WRK-OUT-SENHAS-T895       PIC  X(037)      VALUE SPACES.
           05  WRK-OUT-NOPCAO-ATTR-T895  PIC S9(004) COMP VALUE ZEROS.
           05  WRK-OUT-NOPCAO-T895       PIC  X(002)      VALUE SPACES.
           05  WRK-OUT-OCULTO-T895.
               10  WRK-OUT-FASE-T895     PIC  9(001)      VALUE ZEROS.
               10  WRK-OUT-PAGINA-T895   PIC  9(005)      VALUE ZEROS.
               10  WRK-OUT-FIMAMOS-T895  PIC  X(001)      VALUE SPACES.
               10  WRK-OUT-CONTROLE-T895 PIC  9(005)      VALUE ZEROS.
               10  FILLER                PIC  X(013)      VALUE SPACES.
           05  WRK-OUT-DADOS-T895.
               10  WRK-OUT-AGENCIA-ATTR-T895
                                         PIC S9(004) COMP VALUE ZEROS.
               10  WRK-OUT-AGENCIA-T895.
                   15  WRK-OUT-AGENCIA-T895-N
                                         PIC  9(004)      VALUE ZEROS.
               10  WRK-OUT-CEDULA-ATTR-T895
                                         PIC S9(004) COMP VALUE ZEROS.
               10  WRK-OUT-CEDULA-T895.
                   15  WRK-OUT-CEDULA-T895-N
                                         PIC  9(009)      VALUE ZEROS.
               10  WRK-OUT-CONTRATO-ATTR-T895
                                         PIC S9(004) COMP VALUE ZEROS.
               10  WRK-OUT-CONTRATO-T895.
                   15  WRK-OUT-CONTRATO-T895-N
                                             PIC  9(009) VALUE ZEROS.
               10  FILLER                    OCCURS 07 TIMES.
                   15  WRK-OUT-ERRO-T895     PIC  X(008) VALUE SPACES.
                   15  WRK-OUT-DESC1-T895    PIC  X(068) VALUE SPACES.
                   15  WRK-OUT-DESC2-T895    PIC  X(068) VALUE SPACES.
               10  WRK-OUT-MENSA-T895        PIC  X(079) VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DA TELA DCOM9GTM - BRAD0660 ***'.
      *----------------------------------------------------------------*

       01  WRK-660-RUCAT895.
           05  WRK-660-LL-AREA-T895      PIC  9(004) COMP  VALUE 0000.
           05  WRK-660-LL-MENSAGEM-T895  PIC  9(004) COMP  VALUE 0000.
           05  WRK-660-SENHAS-T895       PIC  9(004) COMP  VALUE 0037.
           05  WRK-660-NOPCAO-T895       PIC  9(004) COMP  VALUE 2004.
           05  WRK-660-OCULTO-T895       PIC  9(004) COMP  VALUE 0025.
           05  WRK-660-AGEN-T895         PIC  9(004) COMP  VALUE 2006.
           05  WRK-660-CEDU-T895         PIC  9(004) COMP  VALUE 2011.
           05  WRK-660-CONT-T895         PIC  9(004) COMP  VALUE 2011.
           05  FILLER                    OCCURS 07 TIMES.
               10  WRK-660-ERRO-T895     PIC  9(004) COMP  VALUE 0008.
               10  WRK-660-DESC1-T895    PIC  9(004) COMP  VALUE 0068.
               10  WRK-660-DESC2-T895    PIC  9(004) COMP  VALUE 0068.
           05  WRK-660-MENSA-T895        PIC  9(004) COMP  VALUE 0079.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DE COMUNICACAO ***'.
      *----------------------------------------------------------------*

       01  WRK-C-AREA.
           05  WRK-C-LL                PIC S9(004) COMP  VALUE ZEROS.
           05  WRK-C-ZZ                PIC S9(004) COMP  VALUE ZEROS.
           05  WRK-C-TRANCODE.
               10  WRK-C-TRANSACAO     PIC  X(008)       VALUE SPACES.
               10  FILLER              PIC  X(006)       VALUE SPACES.
               10  WRK-C-PFK           PIC  X(001)       VALUE SPACES.
           05  WRK-C-SENHAS            PIC  X(037)       VALUE SPACES.
           05  WRK-C-MENSAGEM          PIC  X(079)       VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA INDICADORES DE NULIDADE ***'.
      *----------------------------------------------------------------*

       01  WRK-RUECB09B-NULL.
           05 WRK-B09B-CSIT-MSGEM-N        PIC S9(004) COMP VALUE ZEROS.
           05 WRK-B09B-CMSGEM-RECOR-N      PIC S9(004) COMP VALUE ZEROS.
           05 WRK-B09B-CCTRL-RQUIS-FINCR-N PIC S9(004) COMP VALUE ZEROS.

       01  WRK-RUECB09C-NULL.
           05  WRK-CMSGEM-RECOR-NULL       PIC S9(004) COMP VALUE ZEROS.
           05  WRK-CISPB-EMISR-MSGEM-NULL  PIC S9(004) COMP VALUE ZEROS.
           05  WRK-CISPB-DSTNA-MSGEM-NULL  PIC S9(004) COMP VALUE ZEROS.
           05  WRK-CCTRL-RQUIS-FINCR-NULL  PIC S9(004) COMP VALUE ZEROS.
           05  WRK-CUNIC-MSGEM-NULL        PIC S9(004) COMP VALUE ZEROS.
           05  WRK-HULT-ATULZ-NULL         PIC S9(004) COMP VALUE ZEROS.
           05  WRK-CSIT-MSGEM-NULL         PIC S9(004) COMP VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DA BRAD7100 ***'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** BOOKS AUXILIARES ***'.
      *----------------------------------------------------------------*

       COPY 'I#RUEC31'.

       COPY 'I#RUEC70'.

       COPY 'I#RUEC72'.

       COPY 'I#RUEC74'.

      *----------------------------------------------------------------*
       01  FILLER                        PIC  X(050)         VALUE
           '*** AREA PARA DB2 ***'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL INCLUDE RUFIB002 END-EXEC.
           EXEC SQL INCLUDE RUECB048 END-EXEC.
           EXEC SQL INCLUDE RUECB09B END-EXEC.
           EXEC SQL INCLUDE RUECB09C END-EXEC.

           EXEC SQL
             DECLARE CSR01-RUECB048 CURSOR FOR
               SELECT
                   CTERM,
                   CIDTFD_MDULO,
                   CSEQ_CTRL_MDULO,
                   WRESUL_CONS_MDULO
             FROM  DB2PRD.TAPOIO_UTILZ_MDULO
             WHERE CTERM              = :RUECB048.CTERM
               AND CIDTFD_MDULO       = :RUECB048.CIDTFD-MDULO
             ORDER BY WRESUL_CONS_MDULO
           END-EXEC.

           EXEC SQL
             DECLARE CSR01-RUECB09C CURSOR FOR
               SELECT
                   NMNTRC_MSGEM_RURAL,
                   CMSGEM_RECOR,
                   CISPB_EMISR_MSGEM,
                   CISPB_DSTNA_MSGEM,
                   CCTRL_RQUIS_FINCR,
                   CUNIC_MSGEM,
                   HMOVTO_MSGEM_RECOR,
                   HULT_ATULZ,
                   WCONTD_MSGEM_RURAL,
                   CSIT_MSGEM
             FROM  DB2PRD.TMNTRC_MSGEM_RURAL
             WHERE CCTRL_RQUIS_FINCR  = :RUECB09C.CCTRL-RQUIS-FINCR
              AND (CMSGEM_RECOR       = :RUECB09C.CMSGEM-RECOR
                OR CMSGEM_RECOR       = 'GEN0004  ')
             ORDER BY NMNTRC_MSGEM_RURAL
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** RUCA6895 - FIM DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  LNK-IO-PCB.
           05  LNK-IO-LTERM            PIC  X(008).
           05  FILLER                  PIC  X(002).
           05  LNK-IO-STATUS           PIC  X(002).
           05  FILLER                  PIC  X(012).
           05  LNK-IO-MODNAME          PIC  X(008).

       01  LNK-ALT-PCB.
           05  LNK-ALT-LTERM           PIC  X(008).
           05  FILLER                  PIC  X(002).
           05  LNK-ALT-STATUS          PIC  X(002).
           05  FILLER                  PIC  X(012).
           05  LNK-ALT-MODNAME         PIC  X(008).


      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

           ENTRY  'DLITCBL'            USING  LNK-IO-PCB
                                              LNK-ALT-PCB.


           MOVE  WRK-GU                TO  WRK-FUNCAO.

      *----------------------------------------------------------------*
      *  ESSA ROTINA CONTEM OS PROCEDIMENTOS INICIAIS                  *
      *----------------------------------------------------------------*
       0000-INICIAR.
      *----------------------------------------------------------------*

           CALL 'POOL5000'             USING WRK-FUNCAO
                                             LNK-IO-PCB
                                             LNK-ALT-PCB
                                             WRK-MENSAGEM
                                             WRK-TELA
                                             WRK-NOVA-OPCAO
                                             WRK-COD-USER
                                             WRK-OPCAO-ATUAL
                                             WRK-COD-DEPTO.

           IF  RETURN-CODE          EQUAL  04 OR 12
               PERFORM 2000-FINALIZAR
           END-IF.

           MOVE SPACES                 TO WRK-OUT-DADOS-T895.

           PERFORM 1000-PROCESSAR-MENSAGEM.

           GO TO 0000-INICIAR.

      *----------------------------------------------------------------*
       0000-99-FIM.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA PROGRAMA A MENSAGEM                               *
      *----------------------------------------------------------------*
       1000-PROCESSAR-MENSAGEM         SECTION.
      *----------------------------------------------------------------*

           MOVE  'RUCAT895'            TO  WRK-TELA.

           IF WRK-TRANSACAO            NOT EQUAL  'RUCA6895'
              PERFORM 1050-MONTAR-TELA
           ELSE
              PERFORM 1100-PROCESSAR-RUCAT895
           END-IF.

           IF WRK-TELA              EQUAL  'RUCAT895'
              PERFORM 1200-PROCESSAR-BRAD0660
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1050-MONTAR-TELA                SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM           TO WRK-C-AREA.
           MOVE 'INFORME OS DADOS E TECLE <ENTER>'
                                       TO WRK-OUT-MENSA-T895.
           MOVE 1                      TO WRK-OUT-FASE-T895
           MOVE ZEROS                  TO WRK-OUT-PAGINA-T895.
           PERFORM  1060-LIMPAR-RUECB048.

      *----------------------------------------------------------------*
       1050-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA LIMPA RUECB048 - AREA DE TRABALHO                 *
      *----------------------------------------------------------------*
       1060-LIMPAR-RUECB048            SECTION.
      *----------------------------------------------------------------*

           PERFORM  1061-OPEN-CSR01-RUECB048.

      *----------------------------------------------------------------*
       1060-10-FETCH.
      *----------------------------------------------------------------*

           PERFORM  1062-FETCH-CSR01-RUECB048.
           IF  SQLCODE  EQUAL  +100
               GO  TO  1060-90-CLOSE
           END-IF.
           PERFORM  1063-DELETE-RUECB048.
           GO  TO  1060-10-FETCH.

      *----------------------------------------------------------------*
       1060-90-CLOSE.
      *----------------------------------------------------------------*

           PERFORM  1064-CLOSE-CSR01-RUECB048.

      *----------------------------------------------------------------*
       1060-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1061-OPEN-CSR01-RUECB048        SECTION.
      *----------------------------------------------------------------*

           MOVE LNK-IO-LTERM           TO CTERM OF RUECB048
           MOVE 33                     TO CIDTFD-MDULO OF RUECB048

           EXEC SQL
               OPEN CSR01-RUECB048
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'TAPOIO_UTILZ_MDULO' TO ERR-DBD-TAB
               MOVE 'OPEN  '            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0010'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1061-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1062-FETCH-CSR01-RUECB048       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH CSR01-RUECB048  INTO
                   :RUECB048.CTERM,
                   :RUECB048.CIDTFD-MDULO,
                   :RUECB048.CSEQ-CTRL-MDULO,
                   :RUECB048.WRESUL-CONS-MDULO
                     :WRK-WRESUL-CONS-MDULO-LEN-NULL
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'TAPOIO_UTILZ_MDULO' TO ERR-DBD-TAB
               MOVE 'FETCH '            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0020'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                  EQUAL  +100
               GO  TO  1062-99-FIM
           END-IF.

           IF  WRK-WRESUL-CONS-MDULO-LEN-NULL
                                       NOT EQUAL ZEROS
               MOVE SPACES             TO WRESUL-CONS-MDULO-TEXT
                                                           OF RUECB048
           END-IF.

      *----------------------------------------------------------------*
       1062-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1063-DELETE-RUECB048            SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             DELETE  FROM DB2PRD.TAPOIO_UTILZ_MDULO
                WHERE CTERM           =  :RUECB048.CTERM
                  AND CIDTFD_MDULO    =  :RUECB048.CIDTFD-MDULO
                  AND CSEQ_CTRL_MDULO =  :RUECB048.CSEQ-CTRL-MDULO
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'TAPOIO_UTILZ_MDULO' TO ERR-DBD-TAB
               MOVE 'DELETE'            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0030'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1063-99-FIM.      EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1064-CLOSE-CSR01-RUECB048       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR01-RUECB048
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'TAPOIO_UTILZ_MDULO' TO ERR-DBD-TAB
               MOVE 'CLOSE '            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0040'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1064-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA PROCESSA A MENSAGEM VINDA DA TELA RUCAT895        *
      *----------------------------------------------------------------*
       1100-PROCESSAR-RUCAT895         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM           TO  WRK-INPUT-T895.

           PERFORM 1110-EFETUAR-INSPECT.

           EVALUATE  TRUE

               WHEN  WRK-INP-PFK-T895     EQUAL  'C'
                     PERFORM  1120-DEVOLVER-TELA
                     PERFORM  1140-RETORNAR-CHAMADOR

               WHEN  WRK-INP-PFK-T895     EQUAL  '.' AND
                     WRK-INP-FASE-T895    EQUAL   1
                     PERFORM  1120-DEVOLVER-TELA
                     PERFORM  1150-TRATAR-ENTER-FASE-1

               WHEN  WRK-INP-PFK-T895     EQUAL  '.' AND
                     WRK-INP-FASE-T895    EQUAL   2
                     PERFORM  1120-DEVOLVER-TELA
                     PERFORM  1160-TRATAR-ENTER-FASE-2

               WHEN  WRK-INP-PFK-T895     EQUAL  '8' AND
                     WRK-INP-FASE-T895    EQUAL   2
                     PERFORM  1170-AVANCAR-PAGINA

               WHEN  WRK-INP-PFK-T895     EQUAL  '7' AND
                     WRK-INP-FASE-T895    EQUAL   2
                     PERFORM  1180-VOLTAR-PAGINA

               WHEN  OTHER
                     PERFORM  1120-DEVOLVER-TELA
                     MOVE 'PF INVALIDA'   TO WRK-OUT-MENSA-T895
                     IF  WRK-INP-FASE-T895 EQUAL 2
                         PERFORM 1153-PROTEGE-CAMPOS
                     END-IF

           END-EVALUATE.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  EFETUA A FORMATACAO DOS CAMPOS NUMERICOS                      *
      *----------------------------------------------------------------*
       1110-EFETUAR-INSPECT            SECTION.
      *----------------------------------------------------------------*

           IF WRK-INP-AGENCIA-T895     NOT EQUAL SPACES
              INSPECT WRK-INP-AGENCIA-T895
                                       REPLACING LEADING ' ' BY '0'
           END-IF.

           IF WRK-INP-CEDULA-T895      NOT EQUAL SPACES
              INSPECT WRK-INP-CEDULA-T895
                                       REPLACING LEADING ' ' BY '0'
           END-IF.

           IF WRK-INP-CONTRATO-T895    NOT EQUAL SPACES
              INSPECT WRK-INP-CONTRATO-T895
                                       REPLACING LEADING ' ' BY '0'
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA DEVOLVE OS DADOS DA TELA SEM ALTERACAO            *
      *----------------------------------------------------------------*
       1120-DEVOLVER-TELA              SECTION.
      *----------------------------------------------------------------*

           PERFORM 1121-DEVOLVER-FIXO.

           PERFORM 1122-DEVOLVER-VARIAVEL.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  DEVOLVE A PARTE FIXA DA TELA                                  *
      *----------------------------------------------------------------*
       1121-DEVOLVER-FIXO              SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-FASE-T895        TO WRK-OUT-FASE-T895.
           MOVE WRK-INP-PAGINA-T895      TO WRK-OUT-PAGINA-T895.
           MOVE WRK-INP-FIMAMOS-T895     TO WRK-OUT-FIMAMOS-T895.
           MOVE WRK-INP-CONTROLE-T895    TO WRK-OUT-CONTROLE-T895.
           MOVE WRK-INP-MENSA-T895       TO WRK-OUT-MENSA-T895.
           MOVE WRK-INP-AGENCIA-T895     TO WRK-OUT-AGENCIA-T895.
           MOVE WRK-INP-CEDULA-T895      TO WRK-OUT-CEDULA-T895.
           MOVE WRK-INP-CONTRATO-T895    TO WRK-OUT-CONTRATO-T895.

      *----------------------------------------------------------------*
       1121-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  DEVOLVE A PARTE VARIAVEL DA TELA                              *
      *----------------------------------------------------------------*
       1122-DEVOLVER-VARIAVEL          SECTION.
      *----------------------------------------------------------------*

           PERFORM VARYING WRK-IND-1   FROM 1 BY 1
                     UNTIL WRK-IND-1   GREATER 7

               MOVE WRK-INP-ERRO-T895     (WRK-IND-1)
                 TO WRK-OUT-ERRO-T895     (WRK-IND-1)
               MOVE WRK-INP-DESC1-T895    (WRK-IND-1)
                 TO WRK-OUT-DESC1-T895    (WRK-IND-1)
               MOVE WRK-INP-DESC2-T895    (WRK-IND-1)
                 TO WRK-OUT-DESC2-T895    (WRK-IND-1)

           END-PERFORM.

      *----------------------------------------------------------------*
       1122-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA RETORNA AO PROGRAMA CHAMADOR                      *
      *----------------------------------------------------------------*
       1140-RETORNAR-CHAMADOR          SECTION.
      *----------------------------------------------------------------*

           INITIALIZE  WRK-C-AREA.

           MOVE WRK-CHNG               TO WRK-FUNCAO
           MOVE  LENGTH  OF WRK-C-AREA TO WRK-C-LL.
           MOVE 'RUEC4590'             TO WRK-TELA
           MOVE 'RUCA6895'             TO WRK-C-TRANSACAO
           MOVE WRK-C-AREA             TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1140-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  EFETUA TRATAMENTO PARA VALIDACAO DOS CAMPOS                   *
      *----------------------------------------------------------------*
       1150-TRATAR-ENTER-FASE-1        SECTION.
      *----------------------------------------------------------------*

           PERFORM  1060-LIMPAR-RUECB048.
           MOVE SPACES                 TO WRK-INCONSIS.
           PERFORM VARYING WRK-IND-1   FROM 1 BY 1
                     UNTIL WRK-IND-1   GREATER 7
               MOVE SPACES             TO WRK-OUT-ERRO-T895(WRK-IND-1)
               MOVE SPACES             TO WRK-OUT-DESC1-T895(WRK-IND-1)
               MOVE SPACES             TO WRK-OUT-DESC2-T895(WRK-IND-1)
           END-PERFORM.

           PERFORM 1151-CONSISTIR-CAMPOS.

           IF  WRK-INCONSIS            NOT EQUAL SPACES
               GO TO 1150-99-FIM
           END-IF.

           PERFORM 1152-MONTAR-TELA.

           IF  WRK-INCONSIS            NOT EQUAL SPACES
               GO TO 1150-99-FIM
           END-IF.

           PERFORM 1153-PROTEGE-CAMPOS.

           MOVE ZEROS                  TO WRK-DESPREZAR.
           MOVE 2                      TO WRK-OUT-FASE-T895.
           MOVE 1                      TO WRK-OUT-PAGINA-T895.

      *----------------------------------------------------------------*
       1150-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  EFETUA CONSISTENCIAS FISICAS DOS CAMPOS DA TELA.              *
      *----------------------------------------------------------------*
       1151-CONSISTIR-CAMPOS           SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INP-AGENCIA-T895    EQUAL SPACES AND
               WRK-INP-CEDULA-T895     EQUAL SPACES AND
               WRK-INP-CONTRATO-T895   EQUAL SPACES
               MOVE 'S'                TO WRK-INCONSIS
               MOVE 'INFORMAR AGENCIA E CEDULA OU CONTRATO'
                                       TO WRK-OUT-MENSA-T895
               MOVE WRK-49369          TO WRK-OUT-AGENCIA-ATTR-T895
                                          WRK-OUT-CEDULA-ATTR-T895
               GO TO 1151-99-FIM
           END-IF.

           IF (WRK-INP-AGENCIA-T895    NOT EQUAL SPACES AND
               WRK-INP-CEDULA-T895     EQUAL SPACES) OR
              (WRK-INP-AGENCIA-T895    EQUAL SPACES AND
               WRK-INP-CEDULA-T895     NOT EQUAL SPACES)
               IF  WRK-INP-AGENCIA-T895    NOT NUMERIC OR
                   WRK-INP-AGENCIA-T895-N  EQUAL ZEROS
                   MOVE 'S'                TO WRK-INCONSIS
                   MOVE 'AGENCIA INVALIDA'
                                           TO WRK-OUT-MENSA-T895
                   MOVE WRK-49369          TO WRK-OUT-AGENCIA-ATTR-T895
                                              WRK-OUT-CEDULA-ATTR-T895
                   GO TO 1151-99-FIM
               END-IF
               IF  WRK-INP-CEDULA-T895     NOT NUMERIC OR
                   WRK-INP-CEDULA-T895-N   EQUAL ZEROS
                   MOVE 'S'                TO WRK-INCONSIS
                   MOVE 'CEDULA INVALIDA'
                                           TO WRK-OUT-MENSA-T895
                   MOVE WRK-49369          TO WRK-OUT-AGENCIA-ATTR-T895
                                              WRK-OUT-CEDULA-ATTR-T895
                   GO TO 1151-99-FIM
               END-IF
           END-IF.

           IF  WRK-INP-CONTRATO-T895   NOT EQUAL SPACES
               IF  WRK-INP-CONTRATO-T895   NOT NUMERIC
                   MOVE 'S'                TO WRK-INCONSIS
                   MOVE 'CONTRATO INVALIDO'
                                           TO WRK-OUT-MENSA-T895
                   MOVE WRK-49369          TO WRK-OUT-AGENCIA-ATTR-T895
                                              WRK-OUT-CEDULA-ATTR-T895
                   GO TO 1151-99-FIM
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1151-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  EFETUA A MONTAGEM DA TELA                                     *
      *----------------------------------------------------------------*
       1152-MONTAR-TELA                SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INP-AGENCIA-T895    NOT EQUAL SPACES
               PERFORM 1152-10-ACESSAR-RUFIB002
               IF  WRK-INCONSIS        NOT EQUAL SPACES
                   MOVE 'S'            TO WRK-INCONSIS
                   MOVE 'CONTRATO INEXISTENTE'
                                       TO WRK-OUT-MENSA-T895
                   MOVE WRK-49369      TO WRK-OUT-AGENCIA-ATTR-T895
                                          WRK-OUT-CEDULA-ATTR-T895
                   GO TO 1152-99-FIM
               END-IF
           END-IF.

           PERFORM 1152-20-ACESSAR-RUECB09B.

           IF  WRK-INCONSIS            NOT EQUAL SPACES
               MOVE 'S'                TO WRK-INCONSIS
               MOVE 'CONTRATO INEXISTENTE'
                                       TO WRK-OUT-MENSA-T895
               IF  WRK-INP-AGENCIA-T895  NOT EQUAL SPACES
                   MOVE WRK-49369      TO WRK-OUT-AGENCIA-ATTR-T895
                                          WRK-OUT-CEDULA-ATTR-T895
               ELSE
                   MOVE WRK-49369      TO WRK-OUT-CONTRATO-ATTR-T895
               END-IF
               GO TO 1152-99-FIM
           END-IF.

           IF  CSIT-MSGEM              OF RUECB09B
                                       NOT EQUAL 3
               MOVE 'S'                TO WRK-INCONSIS
               MOVE 'SEM REGISTRO DE INCONSISTENCIA TMNF/BACEN(1)'
                                       TO WRK-OUT-MENSA-T895
               IF  WRK-INP-AGENCIA-T895  NOT EQUAL SPACES
                   MOVE WRK-49369      TO WRK-OUT-AGENCIA-ATTR-T895
                                          WRK-OUT-CEDULA-ATTR-T895
               ELSE
                   MOVE WRK-49369      TO WRK-OUT-CONTRATO-ATTR-T895
               END-IF
               GO TO 1152-99-FIM
           END-IF.

           PERFORM 1152-30-ACESSAR-RUECB09C.

           IF  SQLCODE  EQUAL  +100
               MOVE 'S'                TO WRK-INCONSIS
               MOVE 'SEM REGISTRO DE INCONSISTENCIA TMNF/BACEN(2)'
                                       TO WRK-OUT-MENSA-T895
               IF  WRK-INP-AGENCIA-T895  NOT EQUAL SPACES
                   MOVE WRK-49369      TO WRK-OUT-AGENCIA-ATTR-T895
                                          WRK-OUT-CEDULA-ATTR-T895
               ELSE
                   MOVE WRK-49369      TO WRK-OUT-CONTRATO-ATTR-T895
               END-IF
               GO TO 1152-01-CLOSE
           END-IF.

           IF  WRK-INCONSIS            NOT EQUAL SPACES
               GO TO 1152-01-CLOSE
           END-IF.

           MOVE ZEROS                  TO WRK-CSEQ-RUECB048.
           IF  CMSGEM-RECOR OF RUECB09C  EQUAL 'COR0001E '
EG0617                                      OR 'COR0005E '
               PERFORM 1152-31-PESQ-ERROS-COR0001
                       UNTIL WRK-FIM-RUECB09C  EQUAL 'S'
           END-IF.
           IF  CMSGEM-RECOR OF RUECB09C  EQUAL 'COR0002E '
               PERFORM 1152-43-PESQ-ERROS-COR0002
           END-IF.
           IF  CMSGEM-RECOR OF RUECB09C  EQUAL 'GEN0004  '
               PERFORM 1152-44-PESQ-ERROS-GEN0004
           END-IF.

           IF  WRK-CSEQ-RUECB048       EQUAL ZEROS
               MOVE 'S'                TO WRK-INCONSIS
               MOVE 'SEM REGISTRO DE INCONSISTENCIA TMNF/BACEN(3)'
                                       TO WRK-OUT-MENSA-T895
               IF  WRK-INP-AGENCIA-T895  NOT EQUAL SPACES
                   MOVE WRK-49369      TO WRK-OUT-AGENCIA-ATTR-T895
                                          WRK-OUT-CEDULA-ATTR-T895
               ELSE
                   MOVE WRK-49369      TO WRK-OUT-CONTRATO-ATTR-T895
               END-IF
               GO TO 1152-01-CLOSE
           END-IF.

           MOVE WRK-DESPREZAR          TO WRK-IND-2.
           MOVE SPACES                 TO WRK-FIM-ERRO.

           PERFORM 1061-OPEN-CSR01-RUECB048.
           PERFORM 1152-50-MONTAR-OCORRENCIAS
               VARYING WRK-IND-1       FROM 1 BY 1
                 UNTIL WRK-IND-1       GREATER 7
                    OR WRK-FIM-ERRO    EQUAL 'S'.

           IF  WRK-FIM-ERRO  EQUAL  'S'
               MOVE '*'                TO WRK-OUT-FIMAMOS-T895
               MOVE 'FINAL DE AMOSTRAGEM'
                                       TO WRK-OUT-MENSA-T895
           END-IF.
           PERFORM  1064-CLOSE-CSR01-RUECB048.

      *----------------------------------------------------------------*
       1152-01-CLOSE.
      *----------------------------------------------------------------*

           PERFORM 1152-42-CLOSE-CSR01-RUECB09C.

      *----------------------------------------------------------------*
       1152-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  EFETUA ACESSO A TABELA RUFIB002                               *
      *----------------------------------------------------------------*
       1152-10-ACESSAR-RUFIB002        SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-AGENCIA-T895-N TO CJUNC-DEPDC      OF RUFIB002.
           MOVE WRK-INP-CEDULA-T895-N  TO CCDULA           OF RUFIB002.

           EXEC SQL
             SELECT CCONTR
             INTO   :RUFIB002.CCONTR
             FROM   DB2PRD.CONTRATO_RURAL
             WHERE  CJUNC_DEPDC        = :RUFIB002.CJUNC-DEPDC
              AND   CCDULA             = :RUFIB002.CCDULA
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'CONTRATO_RURAL'    TO ERR-DBD-TAB
               MOVE 'SELECT'            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0050'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-INCONSIS
           END-IF.

      *----------------------------------------------------------------*
       1152-10-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  EFETUA ACESSO A TABELA RUECB09B                               *
      *----------------------------------------------------------------*
       1152-20-ACESSAR-RUECB09B        SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INP-AGENCIA-T895    NOT EQUAL SPACES
               MOVE CCONTR             OF RUFIB002
                                       TO CCONTR-CREDT-RURAL OF RUECB09B
           ELSE
               MOVE WRK-INP-CONTRATO-T895-N
                                       TO CCONTR-CREDT-RURAL OF RUECB09B
           END-IF.

           EXEC SQL
             SELECT MAX(NENVIO_RECOR)
             INTO   :RUECB09B.NENVIO-RECOR
             FROM   DB2PRD.THIST_ENVIO_RECOR
             WHERE  CCONTR_CREDT_RURAL = :RUECB09B.CCONTR-CREDT-RURAL
           END-EXEC.

           IF (SQLCODE        NOT EQUAL ZEROS AND +100 AND -305) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'THIST_ENVIO_RECOR' TO ERR-DBD-TAB
               MOVE 'SELECT'            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0060'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100 OR -305
               MOVE 'S'                TO WRK-INCONSIS
               GO TO 1152-20-99-FIM
           END-IF.

           EXEC SQL
             SELECT  CSIT_MSGEM,
                     CMSGEM_RECOR,
                     CCTRL_RQUIS_FINCR
             INTO   :RUECB09B.CSIT-MSGEM
                      :WRK-B09B-CSIT-MSGEM-N,
                    :RUECB09B.CMSGEM-RECOR
                      :WRK-B09B-CMSGEM-RECOR-N,
                    :RUECB09B.CCTRL-RQUIS-FINCR
                      :WRK-B09B-CCTRL-RQUIS-FINCR-N
             FROM   DB2PRD.THIST_ENVIO_RECOR
             WHERE  CCONTR_CREDT_RURAL = :RUECB09B.CCONTR-CREDT-RURAL
               AND  NENVIO_RECOR       = :RUECB09B.NENVIO-RECOR
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'THIST_ENVIO_RECOR' TO ERR-DBD-TAB
               MOVE 'SELECT'            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0070'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  WRK-B09B-CSIT-MSGEM-N   NOT EQUAL ZEROS
               MOVE ZEROS          TO CSIT-MSGEM           OF RUECB09B
           END-IF.

           IF  WRK-B09B-CMSGEM-RECOR-N NOT EQUAL ZEROS
               MOVE SPACES         TO CMSGEM-RECOR         OF RUECB09B
           END-IF.

           IF  WRK-B09B-CCTRL-RQUIS-FINCR-N NOT EQUAL ZEROS
               MOVE SPACES         TO CCTRL-RQUIS-FINCR    OF RUECB09B
           END-IF.

      *----------------------------------------------------------------*
       1152-20-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  EFETUA A LEITURA  DO CURSOR.                                  *
      *----------------------------------------------------------------*
       1152-30-ACESSAR-RUECB09C        SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO NMNTRC-MSGEM-RURAL
           MOVE 'N'                    TO WRK-FIM-RUECB09C
           MOVE CMSGEM-RECOR           OF RUECB09B
                                       TO WRK-CMSGEM-RECOR
           MOVE 'E'                    TO WRK-CMSGEM-RECOR(8:1)
           MOVE WRK-CMSGEM-RECOR       TO
                                          CMSGEM-RECOR OF RUECB09C.
           MOVE CCTRL-RQUIS-FINCR      OF RUECB09B
                                       TO CCTRL-RQUIS-FINCR OF RUECB09C.

           PERFORM  1152-40-OPEN-CSR01-RUECB09C

           PERFORM  1152-41-FETCH-CSR01-RUECB09C.

      *----------------------------------------------------------------*
       1152-30-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  PESQUISAR ERROS DO COR0001E                                   *
      *----------------------------------------------------------------*
       1152-31-PESQ-ERROS-COR0001      SECTION.
      *----------------------------------------------------------------*

EG0617     PERFORM VARYING IND-4       FROM 1 BY 1
EG0617             UNTIL   IND-4       GREATER 32496
EG0617        IF  WCONTD-MSGEM-RURAL-TEXT OF RUECB09C(IND-4:4)
EG0617                                 EQUAL 'ECOR'
EG0617            MOVE WCONTD-MSGEM-RURAL-TEXT OF RUECB09C(IND-4:8)
EG0617                                 TO WRK-ERRO
EG0617            PERFORM 1152-45-ACESSAR-RCOR5000
EG0617        END-IF
EG0617     END-PERFORM
EG0617     .
           PERFORM  1152-41-FETCH-CSR01-RUECB09C.

      *----------------------------------------------------------------*
       1152-31-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1152-40-OPEN-CSR01-RUECB09C     SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               OPEN CSR01-RUECB09C
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'TMNTRC_MSGEM_RURAL' TO ERR-DBD-TAB
               MOVE 'OPEN  '            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0100'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1152-40-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1152-41-FETCH-CSR01-RUECB09C    SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH CSR01-RUECB09C  INTO
                     :RUECB09C.NMNTRC-MSGEM-RURAL,
                     :RUECB09C.CMSGEM-RECOR
                       :WRK-CMSGEM-RECOR-NULL,
                     :RUECB09C.CISPB-EMISR-MSGEM
                       :WRK-CISPB-EMISR-MSGEM-NULL,
                     :RUECB09C.CISPB-DSTNA-MSGEM
                       :WRK-CISPB-DSTNA-MSGEM-NULL,
                           :RUECB09C.CCTRL-RQUIS-FINCR
                             :WRK-CCTRL-RQUIS-FINCR-NULL,
                           :RUECB09C.CUNIC-MSGEM
                             :WRK-CUNIC-MSGEM-NULL,
                           :RUECB09C.HMOVTO-MSGEM-RECOR,
                           :RUECB09C.HULT-ATULZ
                             :WRK-HULT-ATULZ-NULL,
                           :RUECB09C.WCONTD-MSGEM-RURAL,
                           :RUECB09C.CSIT-MSGEM
                             :WRK-CSIT-MSGEM-NULL
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'TMNTRC_MSGEM_RURAL' TO ERR-DBD-TAB
               MOVE 'FETCH '            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0110'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                  EQUAL  +100
               MOVE 'S'                 TO WRK-FIM-RUECB09C
               GO  TO  1152-41-99-FIM
           END-IF.

           IF  WRK-CMSGEM-RECOR-NULL   NOT EQUAL ZEROS
               MOVE SPACES         TO CMSGEM-RECOR         OF RUECB09C
           END-IF.

           IF  WRK-CISPB-EMISR-MSGEM-NULL   NOT EQUAL ZEROS
               MOVE SPACES         TO CISPB-EMISR-MSGEM    OF RUECB09C
           END-IF.

           IF  WRK-CISPB-DSTNA-MSGEM-NULL   NOT EQUAL ZEROS
               MOVE SPACES         TO CISPB-DSTNA-MSGEM    OF RUECB09C
           END-IF.

           IF  WRK-CCTRL-RQUIS-FINCR-NULL   NOT EQUAL ZEROS
               MOVE SPACES         TO CCTRL-RQUIS-FINCR    OF RUECB09C
           END-IF.

           IF  WRK-CUNIC-MSGEM-NULL   NOT EQUAL ZEROS
               MOVE SPACES         TO CUNIC-MSGEM          OF RUECB09C
           END-IF.

           IF  WRK-HULT-ATULZ-NULL   NOT EQUAL ZEROS
               MOVE '0001-01-01-00.00.00.000000'
                                   TO HULT-ATULZ           OF RUECB09C
           END-IF.

           IF  WRK-CSIT-MSGEM-NULL   NOT EQUAL ZEROS
               MOVE ZEROS          TO WCONTD-MSGEM-RURAL   OF RUECB09C
           END-IF.

      *----------------------------------------------------------------*
       1152-41-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1152-42-CLOSE-CSR01-RUECB09C       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR01-RUECB09C
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'TMNTRC_MSGEM_RURAL' TO ERR-DBD-TAB
               MOVE 'CLOSE '            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0120'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1152-42-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1152-43-PESQ-ERROS-COR0002      SECTION.
      *----------------------------------------------------------------*

           MOVE WCONTD-MSGEM-RURAL-TEXT OF RUECB09C
                                       (1: LENGTH OF RUEC72-REGISTRO)
                                       TO RUEC72-REGISTRO
           IF  RUEC72-CODMSG-E           NOT  EQUAL  SPACES
               MOVE RUEC72-CODMSG-E           TO WRK-ERRO
               PERFORM 1152-45-ACESSAR-RCOR5000
           END-IF.
           IF  RUEC72-NRO-CTRL-IF-E       NOT  EQUAL  SPACES
               MOVE RUEC72-NRO-CTRL-IF-E       TO WRK-ERRO
               PERFORM 1152-45-ACESSAR-RCOR5000
           END-IF.
           IF  RUEC72-CNPJ-ENT-RESPONS-E  NOT  EQUAL  SPACES
               MOVE RUEC72-CNPJ-ENT-RESPONS-E  TO WRK-ERRO
               PERFORM 1152-45-ACESSAR-RCOR5000
           END-IF.
           IF  RUEC72-NUM-REF-BACNE-E     NOT  EQUAL  SPACES
               MOVE RUEC72-NUM-REF-BACNE-E     TO WRK-ERRO
               PERFORM 1152-45-ACESSAR-RCOR5000
           END-IF.
           IF  RUEC72-MTVO-EXCL-COR-E     NOT  EQUAL  SPACES
               MOVE RUEC72-MTVO-EXCL-COR-E     TO WRK-ERRO
               PERFORM 1152-45-ACESSAR-RCOR5000
           END-IF.
           IF  RUEC72-DATA-MOVTO-E        NOT  EQUAL  SPACES
               MOVE RUEC72-DATA-MOVTO-E        TO WRK-ERRO
               PERFORM 1152-45-ACESSAR-RCOR5000
           END-IF.

      *----------------------------------------------------------------*
       1152-43-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1152-44-PESQ-ERROS-GEN0004      SECTION.
      *----------------------------------------------------------------*

           MOVE WCONTD-MSGEM-RURAL-TEXT OF RUECB09C
                                       (1: LENGTH OF RUEC70-REGISTRO)
                                       TO RUEC70-REGISTRO
           IF  RUEC70-ERRO-GEN           NOT  EQUAL  SPACES
               MOVE RUEC70-ERRO-GEN           TO WRK-ERRO
               PERFORM 1152-45-ACESSAR-RCOR5000
           END-IF.
           MOVE '******  '             TO WRK-ERRO
           MOVE RUEC70-HISTORICO(1:80) TO RCOR02-S-DSCERRO(1)
           PERFORM 1152-45-ACESSAR-RCOR5000
           MOVE '******* '             TO WRK-ERRO
           MOVE RUEC70-HISTORICO(81:80) TO RCOR02-S-DSCERRO(1).
           PERFORM 1152-45-ACESSAR-RCOR5000.
           MOVE '********'             TO WRK-ERRO
           MOVE RUEC70-HISTORICO(161:40) TO RCOR02-S-DSCERRO(1).
           PERFORM 1152-45-ACESSAR-RCOR5000.

      *----------------------------------------------------------------*
       1152-44-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ACESSAR MODULO RCOR5000 E GRAVAR RUECB048                     *
      *----------------------------------------------------------------*
       1152-45-ACESSAR-RCOR5000        SECTION.
      *----------------------------------------------------------------*

           IF  WRK-ERRO  EQUAL SPACES OR LOW-VALUES
               GO  TO  1152-45-99-FIM
           END-IF.

           IF  WRK-ERRO(1:6) EQUAL '******'
               GO  TO  1152-45-10-GRAVA
           END-IF.

DK....     INITIALIZE RCOR02-REGISTRO.

           MOVE WRK-ERRO               TO RCOR02-E-CODERRO

      ***** REVER ESTE ACESSO COM 1, POIS 1 RETORNA SOMENTE 1 OCORRENCIA
           MOVE 1                      TO RCOR02-E-TPCONS
           MOVE 'RCOR5000'             TO WRK-MODULO.

           CALL WRK-MODULO             USING RCOR02-REGISTRO
                                             ERRO-AREA
                                             SQLCA

           IF  RCOR02-S-COD-RETORNO    EQUAL  100
               MOVE 'DESCRICAO DO ERRO NAO ENCONTRADA NO RCOR'
                                       TO RCOR02-S-DSCERRO(1)
           END-IF.
           IF  RCOR02-S-COD-RETORNO    EQUAL  10
               MOVE 'INCONSISTENCIA NOS DADOS ENVIADOS AO RCOR5000'
                                       TO RCOR02-S-DSCERRO(1)
           END-IF.
           IF  RCOR02-S-COD-RETORNO    EQUAL  99
               MOVE 'ERRO NO ACESSO DB2 TABELA RCOR'
                                       TO RCOR02-S-DSCERRO(1)
           END-IF.

      *----------------------------------------------------------------*
       1152-45-10-GRAVA.
      *----------------------------------------------------------------*

           ADD  1                      TO WRK-CSEQ-RUECB048.
           MOVE LNK-IO-LTERM           TO CTERM OF RUECB048
           MOVE 33                     TO CIDTFD-MDULO OF RUECB048
           MOVE WRK-CSEQ-RUECB048      TO CSEQ-CTRL-MDULO OF RUECB048
           MOVE WRK-ERRO               TO R31-CERRO-BACEN
           MOVE RCOR02-S-DSCERRO(1)    TO R31-IERRO-BACEN.
           MOVE 300                    TO WRESUL-CONS-MDULO-LEN
           MOVE R31-TEXTO              TO WRESUL-CONS-MDULO-TEXT

           EXEC SQL
             INSERT INTO DB2PRD.TAPOIO_UTILZ_MDULO
                  (CTERM,
                   CIDTFD_MDULO,
                   CSEQ_CTRL_MDULO,
                   WRESUL_CONS_MDULO)
               VALUES
                  (:RUECB048.CTERM,
                   :RUECB048.CIDTFD-MDULO,
                   :RUECB048.CSEQ-CTRL-MDULO,
                   :RUECB048.WRESUL-CONS-MDULO)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'TAPOIO_UTILZ_MDULO' TO ERR-DBD-TAB
               MOVE 'INSERT'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '0090'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1152-45-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  EFETUA A MONTAGEM DAS OCORRENCIAS.                            *
      *----------------------------------------------------------------*
       1152-50-MONTAR-OCORRENCIAS      SECTION.
      *----------------------------------------------------------------*

           PERFORM 1062-FETCH-CSR01-RUECB048.
           IF  SQLCODE  EQUAL  +100
               MOVE 'S'                TO WRK-FIM-ERRO
               GO  TO  1152-50-99-FIM
           END-IF.
           ADD 1                       TO WRK-IND-2.
           IF  WRK-IND-2 NOT GREATER  WRK-DESPREZAR
               GO  TO  1152-50-MONTAR-OCORRENCIAS
           END-IF.

           MOVE WRESUL-CONS-MDULO-TEXT TO R31-TEXTO
           MOVE R31-CERRO-BACEN        TO WRK-OUT-ERRO-T895(WRK-IND-1)
           MOVE R31-IERRO-BACEN        TO WRK-MENSAGEM-AUX.
           MOVE WRK-MENSAGEM-AUX-1     TO WRK-OUT-DESC1-T895(WRK-IND-1).
           MOVE WRK-MENSAGEM-AUX-2     TO WRK-OUT-DESC2-T895(WRK-IND-1).

      *----------------------------------------------------------------*
       1152-50-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  EFETUA CONSISTENCIAS FISICAS DOS CAMPOS DA TELA.              *
      *----------------------------------------------------------------*
       1153-PROTEGE-CAMPOS             SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-241                TO WRK-OUT-AGENCIA-ATTR-T895
                                          WRK-OUT-CEDULA-ATTR-T895
                                          WRK-OUT-CONTRATO-ATTR-T895.
           MOVE WRK-49345              TO WRK-OUT-NOPCAO-ATTR-T895.

      *----------------------------------------------------------------*
       1153-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  EFETUA TRATAMENTO DE <ENTRA> PARA FASE 2                      *
      *----------------------------------------------------------------*
       1160-TRATAR-ENTER-FASE-2        SECTION.
      *----------------------------------------------------------------*

           MOVE 'INFORME OS DADOS E TECLE <ENTER>'
                                       TO WRK-OUT-MENSA-T895.
           MOVE 1                      TO WRK-OUT-FASE-T895
           MOVE ZEROS                  TO WRK-OUT-PAGINA-T895.

      *----------------------------------------------------------------*
       1160-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  EFETUA TRATAMENTO PARA FASE 2                                 *
      *----------------------------------------------------------------*
       1170-AVANCAR-PAGINA             SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INP-PAGINA-T895     EQUAL 1
               PERFORM 1120-DEVOLVER-TELA
               MOVE 'NAO HA MAIS PAGINAS PARA AVANCAR'
                                       TO WRK-OUT-MENSA-T895
           ELSE
               PERFORM 1060-LIMPAR-RUECB048
               PERFORM 1121-DEVOLVER-FIXO
               MOVE SPACES             TO WRK-OUT-FIMAMOS-T895
               MOVE WRK-INP-PAGINA-T895
                                       TO WRK-PAGINA-AUX
               COMPUTE WRK-DESPREZAR   =  WRK-PAGINA-AUX  * 7

               ADD   1                 TO WRK-PAGINA-AUX

               MOVE WRK-PAGINA-AUX     TO WRK-OUT-PAGINA-T895
               PERFORM 1152-MONTAR-TELA
               MOVE 'AMOSTRAGEM CONTINUA'
                                       TO WRK-OUT-MENSA-T895
           END-IF.

           PERFORM 1153-PROTEGE-CAMPOS.

      *----------------------------------------------------------------*
       1170-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  EFETUA TRATAMENTO PARA FASE 2                                 *
      *----------------------------------------------------------------*
       1180-VOLTAR-PAGINA              SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INP-FIMAMOS-T895    EQUAL '*'
               PERFORM 1120-DEVOLVER-TELA
               MOVE 'NAO HA MAIS PAGINAS PARA VOLTAR'
                                       TO WRK-OUT-MENSA-T895
           ELSE
               PERFORM 1060-LIMPAR-RUECB048
               PERFORM 1121-DEVOLVER-FIXO
               MOVE WRK-INP-PAGINA-T895
                                       TO WRK-PAGINA-AUX
               COMPUTE WRK-DESPREZAR   =  (WRK-PAGINA-AUX - 2) * 7

               SUBTRACT 1           FROM  WRK-PAGINA-AUX

               MOVE WRK-PAGINA-AUX     TO WRK-OUT-PAGINA-T895
               PERFORM  1152-MONTAR-TELA
               MOVE 'AMOSTRAGEM CONTINUA'
                                       TO WRK-OUT-MENSA-T895
           END-IF.

           PERFORM 1153-PROTEGE-CAMPOS.

      *----------------------------------------------------------------*
       1180-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA CARREGA A TELA QUE SERA DEMONSTRADA PELO IMS.     *
      *----------------------------------------------------------------*
       1200-PROCESSAR-BRAD0660         SECTION.
      *----------------------------------------------------------------*

           MOVE LENGTH OF WRK-OUTPUT-T895
                                       TO WRK-660-LL-MENSAGEM-T895
                                          WRK-OUT-LL-T895
           MOVE LENGTH OF WRK-660-RUCAT895
                                       TO WRK-660-LL-AREA-T895

           CALL  'BRAD0660'            USING WRK-OUTPUT-T895
                                             WRK-660-RUCAT895.

           IF RETURN-CODE              NOT EQUAL  ZEROS
              MOVE  'APL'              TO ERR-TIPO-ACESSO
              MOVE  RETURN-CODE        TO WRK-RETURN-CODE
              MOVE  11                 TO WRK-LOCAL-ERRO
              MOVE  WRK-ERRO-BRAD0660  TO ERR-TEXTO
              PERFORM  9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE  WRK-ISRT              TO WRK-FUNCAO.
           MOVE  WRK-OUTPUT-T895       TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA FINALIZA O PROCESSAMENTO DO PROGRAMA              *
      *----------------------------------------------------------------*
       2000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA FORMATA E EMITE A MENSAGEM DE ERRO DO PROGRAMA    *
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           IF ERR-MODULO               NOT EQUAL  SPACES  AND
                                                  LOW-VALUES
              MOVE  'RUCA6895'         TO  ERR-PGM
           ELSE
              MOVE  'SENHAS02'         TO  ERR-PGM
              MOVE  'RUCA6895'         TO  ERR-MODULO
           END-IF.

           MOVE  WRK-COD-USER-X        TO  ERR-COD-USER.
           MOVE  WRK-COD-DEPTO         TO  ERR-COD-DEPTO.


           IF  ERR-TIPO-ACESSO         EQUAL 'DB2'
               CALL  'BRAD7100'        USING  LNK-IO-PCB
                                              ERRO-AREA
                                              LNK-ALT-PCB
                                              SQLCA
           ELSE
               CALL 'BRAD7100'         USING  LNK-IO-PCB
                                              ERRO-AREA
                                              LNK-ALT-PCB
           END-IF.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
