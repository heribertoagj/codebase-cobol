      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. RUCA6959.
       AUTHOR.     FELIPE S.
      *================================================================*
      *                   S I S   C O N S U L T O R I A                *
      *----------------------------------------------------------------*
      *      PROGRAMADOR  : FELIPE S.           - SIS                  *
      *      ANALISTA     : FELIPE S.           - SIS                  *
      *      DATA         : 21/09/2020                                 *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   CONSULTA INCONSISTENCIAS ENVIO SICOR        *
      *----------------------------------------------------------------*
      *    TELAS.......:   RUCAT959                                    *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    I#BRAD7C - AREA PARA TRATAMENTO DE ERROS PELA BRAD7100      *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    BRAD0660 - INSERIR CARACTERES DE NULL NA MENSAGEM DE SAIDA  *
      *    POOL5005 - ENVIA/RECEBE MENSAGENS DO IMS.                   *
      *    BRAD7100 - TRATAMENTO DE ERROS DA APLICACAO                 *
      *----------------------------------------------------------------*
      *    NAVEGACAO...:                                               *
      *    CHAMADO POR.: RUCA6950            CHAMA.:                   *
      *================================================================*
7COMMP*                   A L T E R A C A O                            *
.     *----------------------------------------------------------------*
.     *    ANALISTA....:  PRISCILA PEREIRA                             *
.     *    DATA........:  SETEMBRO/2023                                *
.     *----------------------------------------------------------------*
.     *    OBJETIVO....:  SICOR 5.07                                   *
.     *    PESQUISA....:  7COMMP                                       *
7COMMP*================================================================*
VI0725*================================================================*
VI0725*               7 C O M M  -  C O N S U L T O R I A              *
VI0725*----------------------------------------------------------------*
VI0725*    PROGRAMADOR.:   VINICIUS CRISTIAN MADUREIRA                 *
VI0725*    ANALISTA....:   VINICIUS CRISTIAN MADUREIRA                 *
VI0725*    DATA........:   29/07/2025                                  *
VI0725*    OBJETIVO....:   HISTORIA JIRA: AGRO7-1012                   *
VI0725*                    TRATAR GRUPOS DE REJEICAO COR0001R1 E       *
VI0725*                    COR0005R1, EXIBINDO NA TELA POR CONTRATO O  *
VI0725*                    CODIGO E A DESCRIÇÃO CORRESPONDENTE PARA    *
VI0725*                    CONSULTA DO USUARIO.                        *
VI0725*                    ARMAZENAR AS INFORMACOES DOS GRUPOS DE      *
VI0725*                    REJEICOES DA COR0001R1 E COR0005R1 PARA     *
VI0725*                    CONSULTA.                                   *
VI0725*================================================================*

      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** RUCA6959 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREAS AUXILIARES PARA POOL5005 ***'.
      *----------------------------------------------------------------*

       01  WRK-GU                      PIC  X(004)       VALUE 'GU'.
       01  WRK-CHNG                    PIC  X(004)       VALUE 'CHNG'.
       01  WRK-ISRT                    PIC  X(004)       VALUE 'ISRT'.
       01  WRK-VERSAO                  PIC  X(006)       VALUE 'VRS002'.
       01  WRK-FUNCAO                  PIC  X(004)       VALUE  SPACES.
       01  WRK-TELA                    PIC  X(008)       VALUE  SPACES.
       01  WRK-INCONSIS                PIC  X(001)       VALUE  SPACES.
       01  WRK-MODULO                  PIC  X(008)       VALUE  SPACES.

       01  WRK-COD-USER                PIC  X(007)       VALUE  SPACES.

       01  WRK-COD-DEPTO.
           05  FILLER                  PIC  X(002)       VALUE  SPACES.
           05  WRK-COD-DEPTO-N         PIC  9(004)       VALUE  ZEROS.

       01  WRK-MENSAGEM.
           05  FILLER                  PIC  X(004)       VALUE  SPACES.
           05  WRK-TRANSACAO           PIC  X(008)       VALUE  SPACES.
           05  FILLER                  PIC  X(1988)      VALUE  SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA BRAD7100 ***'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DO MODULO RCOR5000 ***'.
      *----------------------------------------------------------------*

       COPY 'I#RUEC74'.

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(050)          VALUE
           '*** AREA DE MENSAGEM COR0001R1 ***'.
      *---------------------------------------------------------------*

       COPY 'I#RUEC89'.

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(050)          VALUE
           '*** AREA DA MENSAGEM TMNF ***'.
      *---------------------------------------------------------------*

       COPY 'I#RCOR03'.

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(050)          VALUE
           '*** AREA DA MENSAGEM GEN0004 ***'.
      *---------------------------------------------------------------*

       COPY 'I#RCOR05'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ERRO DA BRAD0660 ***'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-BRAD0660.
           05  FILLER                  PIC  X(045)         VALUE
               '* RETURN-CODE DIFERENTE DE ZEROS NA BRAD0660 '.
           05  FILLER                  PIC  X(012)         VALUE
               '- RET.COD = '.
           05  WRK-RETURN-CODE         PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(011)         VALUE
               ' - LOCAL = '.
           05  WRK-LOCAL-ERRO          PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(002)         VALUE ' *'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)         VALUE
           '*** AREA DE ATRIBUTO  ***'.
      *----------------------------------------------------------------*

      * 1 | NUMERICO | NORMAL   | DESPROTEGIDO |                       *
       01  WRK-209                     PIC S9(008) COMP VALUE +209.

      * 2 | NUMERICO | NORMAL   | DESPROTEGIDO | POSICIONAMENTO CURSOR *
       01  WRK-49361                   PIC S9(008) COMP VALUE +49361.

      * 3 | NUMERICO | BRILHANTE| DESPROTEGIDO | POSICIONAMENTO CURSOR *
       01  WRK-49369                   PIC S9(08)  COMP VALUE +49369.

      * 4 | NUMERICO | ALFA     | PROTEGIDO    |                       *
       01  WRK-225                     PIC S9(008) COMP VALUE +225.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA MENSAGENS ***'.
      *----------------------------------------------------------------*

       01  WRK-MSG00                   PIC  X(079)         VALUE
           'ENTRE COM O ARGUMENTO DE PESQUISA E TECLE <ENTER>'.

       01  WRK-MSG01                   PIC  X(079)         VALUE
           'CAMPO COM CONTEUDO INVALIDO'.

       01  WRK-MSG02                   PIC  X(079)         VALUE
           'PF INVALIDA'.

       01  WRK-MSG03                   PIC  X(079)         VALUE
           'CONTRATO NAO ENCONTRADO'.

       01  WRK-MSG04                   PIC  X(079)         VALUE
           'NUMERO DE REFERENCIA BACEN NAO ENCONTRADO'.

       01  WRK-MSG05                   PIC  X(079)         VALUE
           'CONTRATO PESQUISADO NAO POSSUI INCONSISTENCIA'.

       01  WRK-MSG06                   PIC  X(079)         VALUE
           'CONSULTA CONTINUA'.

       01  WRK-MSG07                   PIC  X(079)         VALUE
           'FIM DE CONSULTA'.

       01  WRK-MSG08                   PIC  X(079)         VALUE
           'NAO HA MAIS PAGINAS PARA RETORNAR'.

       01  WRK-MSG09                   PIC  X(079)         VALUE
           'NAO HA MAIS PAGINAS PARA AVANCAR'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*

       01  IND-LIN                     PIC  9(05) COMP-3  VALUE ZEROS.
       01  IND-CHAR                    PIC  9(05) COMP-3  VALUE ZEROS.
       01  ACU-PAGINA                  PIC  9(03) COMP-3  VALUE ZEROS.
       01  ACU-DESPREZAR               PIC  9(05) COMP-3  VALUE ZEROS.
       01  ACU-REGISTRO                PIC  9(05) COMP-3  VALUE ZEROS.
       01  WRK-CSEQ-CTRL-MDULO         PIC S9(05) COMP-3  VALUE ZEROS.
       01  WRK-ENCONTROU               PIC  X(01)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)         VALUE
           '*** AREA PARA SALVAR DADOS DA LISTA ***'.
      *----------------------------------------------------------------*

       01  WRK-DADOS-NAV-T959.
           05  WRK-959-CERRO           PIC  X(08)          VALUE SPACES.
           05  FILLER                  REDEFINES  WRK-959-CERRO.
               07  WRK-959-CERRO-X4    PIC  X(04).
               07  WRK-959-CERRO-N4    PIC  9(04).
           05  WRK-959-DERRO           PIC  X(70)          VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO ***'.
      *----------------------------------------------------------------*

       01  WRK-COMU-AREA.
           05  WRK-COMU-LL             PIC S9(04) COMP     VALUE ZEROS.
           05  WRK-COMU-ZZ             PIC  9(04) COMP     VALUE ZEROS.
           05  WRK-COMU-TRANCODE       PIC  X(15)          VALUE SPACES.
           05  WRK-COMU-SENHAS         PIC  X(37)          VALUE SPACES.
           05  WRK-COMU-NOPCAO         PIC  X(02)          VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** TELA RUCAT959 - INPUT ***'.
      *----------------------------------------------------------------*

       01  INPUT-T959.
           03  INP-LL-T959             PIC  9(04)  COMP    VALUE 0.
           03  INP-ZZ-T959             PIC  9(04)  COMP    VALUE 0.
           03  INP-TRANCODE-T959       PIC  X(08)          VALUE SPACES.
           03  FILLER                  PIC  X(06)          VALUE SPACES.
           03  INP-DADOS-T959.
               05  INP-PFK-T959        PIC  X(01)          VALUE SPACES.
               05  INP-SENHAS-T959     PIC  X(37)          VALUE SPACES.
               05  INP-NOPCAO-T959     PIC  X(02)          VALUE SPACES.
               05  INP-FIMAMOS-T959    PIC  X(01)          VALUE SPACES.
               05  INP-FASE-T959       PIC  9(01)          VALUE ZEROS.
               05  INP-ATRIB-T959.
                   07  INP-ATRIB-NOPCAO
                                       PIC  9(01)          VALUE ZEROS.
                   07  INP-ATRIB-CHAVEI
                                       PIC  9(01)          VALUE ZEROS.
                   07  INP-ATRIB-BACENI
                                       PIC  9(01)          VALUE ZEROS.
               05  INP-PAGINA-T959     PIC  9(03)          VALUE ZEROS.
               05  INP-CPOANT-T959.
                   07  INP-CHAVE-ANT-T959
                                       PIC  X(09)          VALUE SPACES.
                   07  INP-BACEN-ANT-T959
                                       PIC  X(11)          VALUE SPACES.
               05  INP-CHAVEI-T959     PIC  X(09)          VALUE SPACES.
               05  INP-CHAVEI-T959-N   REDEFINES
                   INP-CHAVEI-T959     PIC  9(09).
               05  INP-BACENI-T959     PIC  X(11)          VALUE SPACES.
               05  INP-BACENI-T959-N   REDEFINES
                   INP-BACENI-T959     PIC  9(11).
               05  INP-LISTA-T959.
                   07  INP-LINHA-T959  OCCURS 14 TIMES.
                       10  INP-CERRO-T959
                                       PIC  X(08)          VALUE SPACES.
                       10  INP-DERRO-T959
                                       PIC  X(70)          VALUE SPACES.
               05  INP-MENSA-T959      PIC  X(79)          VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** TELA RUCAT959 - OUTPUT ***'.
      *----------------------------------------------------------------*

       01  OUTPUT-T959.
           03  OUT-LL-T959             PIC  9(04)  COMP    VALUE 0.
           03  OUT-ZZ-T959             PIC  9(04)  COMP    VALUE 0.
           03  OUT-DADOS-T959.
               05  OUT-SENHAS-T959     PIC  X(37)          VALUE SPACES.
               05  OUT-NOPCAO-ATTR-T959
                                       PIC S9(04) COMP    VALUE ZEROS.
               05  OUT-NOPCAO-T959     PIC  X(02)          VALUE SPACES.
               05  OUT-FIMAMOS-T959    PIC  X(01)          VALUE SPACES.
               05  OUT-FASE-T959       PIC  9(01)          VALUE ZEROS.
               05  OUT-ATRIB-T959      PIC  X(03)          VALUE SPACES.
               05  OUT-PAGINA-T959     PIC  9(03)          VALUE ZEROS.
               05  OUT-CPOANT-T959     PIC  X(20)          VALUE SPACES.
               05  OUT-CHAVEI-ATTR-T959
                                       PIC S9(04) COMP    VALUE ZEROS.
               05  OUT-CHAVEI-T959     PIC  X(09)          VALUE SPACES.
               05  OUT-CHAVEI-T959-N   REDEFINES
                   OUT-CHAVEI-T959     PIC  9(09).
               05  OUT-BACENI-ATTR-T959
                                       PIC S9(04) COMP    VALUE ZEROS.
               05  OUT-BACENI-T959     PIC  X(11)          VALUE SPACES.
               05  OUT-BACENI-T959-N   REDEFINES
                   OUT-BACENI-T959     PIC  9(11).
               05  OUT-LISTA-T959.
                   07  OUT-LINHA-T959  OCCURS 14 TIMES.
                       10  OUT-CERRO-T959
                                       PIC  X(08)          VALUE SPACES.
                       10  OUT-DERRO-T959
                                       PIC  X(70)          VALUE SPACES.
               05  OUT-MENSA-T959      PIC  X(79)          VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA BRAD0660 ***'.
      *---------------------------------------------------------------*

       01  660-RUCAT959-SEG00.
           05  660-LL-AREA-T959        PIC  9(04)  COMP    VALUE 0000.
           05  660-LL-MENSAGEM-T959    PIC  9(04)  COMP    VALUE 0000.
           05  660-SENHAS-T959         PIC  9(04)  COMP    VALUE 0037.
           05  660-NOPCAO-T959         PIC  9(04)  COMP    VALUE 2004.
           05  660-FIMAMOS-T959        PIC  9(04)  COMP    VALUE 0001.
           05  660-FASE-T959           PIC  9(04)  COMP    VALUE 0001.
           05  660-ATRIB-T959          PIC  9(04)  COMP    VALUE 0003.
           05  660-PAGINA-T959         PIC  9(04)  COMP    VALUE 0003.
           05  660-CPOANT-T959         PIC  9(04)  COMP    VALUE 0020.
           05  660-CHAVEI-T959         PIC  9(04)  COMP    VALUE 2011.
           05  660-BACENI-T959         PIC  9(04)  COMP    VALUE 2013.
           05  660-LINHA-T959          OCCURS 14 TIMES.
               07  660-CERRO-T959      PIC  9(04)  COMP    VALUE 0008.
               07  660-DERRO-T959      PIC  9(04)  COMP    VALUE 0070.
           05  660-MENSA-T959          PIC  9(04)  COMP    VALUE 0079.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** AREAS PARA TRATAR NULO ***'.
      *----------------------------------------------------------------*

       01  WRK-B019-CMSGEM-RECOR       PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B048-WRESUL-CONS-MDULO  PIC S9(04)  COMP    VALUE ZEROS.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE DB2 ***'.
      *---------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE RUCAB013
           END-EXEC.

           EXEC SQL
               INCLUDE RUCAB015
           END-EXEC.

           EXEC SQL
               INCLUDE RUCAB019
           END-EXEC.

           EXEC SQL
               INCLUDE RUFIB002
           END-EXEC.

           EXEC SQL
               INCLUDE RUECB048
           END-EXEC.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA DECLARACAO DE CURSORES ***'.
      *---------------------------------------------------------------*

           EXEC SQL
               DECLARE  CSR01-RUCAB013    CURSOR FOR
                SELECT  CCONTR              ,
                        HINCL_REG           ,
                        NENVIO_RECOR        ,
                        NREG_MSGEM_RECOR    ,
                        CTPO_CONTD_MSGEM    ,
                        WCONTD_MSGEM_RURAL
                  FROM  DB2PRD.TENVIO_RETOR_MANUT
                 WHERE  CCONTR            = :RUCAB013.CCONTR
                   AND  HINCL_REG         = :RUCAB013.HINCL-REG
                   AND  NENVIO_RECOR     >= :RUCAB013.NENVIO-RECOR
                   AND  NREG_MSGEM_RECOR >= :RUCAB013.NREG-MSGEM-RECOR
                   AND  CTPO_CONTD_MSGEM >= :RUCAB013.CTPO-CONTD-MSGEM
                 ORDER  BY  NENVIO_RECOR     ASC,
                            NREG_MSGEM_RECOR ASC,
                            CTPO_CONTD_MSGEM ASC
           END-EXEC.

           EXEC SQL
               DECLARE  CSR02-RUECB048 CURSOR WITH HOLD FOR
                SELECT  CTERM             ,
                        CIDTFD_MDULO      ,
                        CSEQ_CTRL_MDULO   ,
                        WRESUL_CONS_MDULO
                  FROM  DB2PRD.TAPOIO_UTILZ_MDULO
                 WHERE  CTERM              = :RUECB048.CTERM
                   AND  CIDTFD_MDULO       = :RUECB048.CIDTFD-MDULO
                   AND  CSEQ_CTRL_MDULO   >= :RUECB048.CSEQ-CTRL-MDULO
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** RUCA6959 - FIM DA AREA DE WORKING ***'.
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

           ENTRY 'DLITCBL'             USING LNK-IO-PCB
                                             LNK-ALT-PCB.

           MOVE WRK-GU                 TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       0000-PROCESSAR-TRANSACAO        SECTION.
      *----------------------------------------------------------------*

           CALL    'POOL5005'      USING   WRK-VERSAO
                                           WRK-FUNCAO
                                           LNK-IO-PCB
                                           LNK-ALT-PCB
                                           WRK-MENSAGEM
                                           WRK-TELA
                                           WRK-COD-USER
                                           WRK-COD-DEPTO.

           IF  RETURN-CODE         EQUAL 04 OR 12
               GOBACK
           END-IF.

           PERFORM 1000-PROCEDIMENTOS-INICIAIS.

           PERFORM 2000-PROCESSAR-MENSAGEM.

           GO   TO 0000-PROCESSAR-TRANSACAO.

      *----------------------------------------------------------------*
       0000-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-PROCEDIMENTOS-INICIAIS     SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO  INP-DADOS-T959
                                           OUT-DADOS-T959
                                           ERRO-AREA

           MOVE ZEROS                  TO  ACU-PAGINA
                                           ACU-DESPREZAR
                                           IND-LIN
                                           IND-CHAR

           MOVE  1000                  TO  ACU-REGISTRO

           MOVE 'RUCAT959'             TO  WRK-TELA.

      *----------------------------------------------------------------*
       1000-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000-PROCESSAR-MENSAGEM         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-TRANSACAO  NOT EQUAL 'RUCA6959'
               PERFORM  3000-RECEBER-CONTROLE
           ELSE
               PERFORM  4000-TRATAR-RUCAT959
           END-IF.

           IF  WRK-TELA           EQUAL 'RUCAT959'
               PERFORM  6000-DEVOLVER-TELA
               PERFORM  7000-PROCESSAR-BRAD0660
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3000-RECEBER-CONTROLE           SECTION.
      *----------------------------------------------------------------*

           MOVE  WRK-MENSAGEM(1:LENGTH OF  WRK-COMU-AREA)
                                       TO  WRK-COMU-AREA.

           PERFORM  3010-MONTA-TELA-INICIAL.

      *----------------------------------------------------------------*
       3000-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3010-MONTA-TELA-INICIAL         SECTION.
      *----------------------------------------------------------------*

           MOVE  WRK-COMU-SENHAS       TO  INP-SENHAS-T959
           MOVE  SPACES                TO  INP-FIMAMOS-T959
           MOVE  1                     TO  INP-FASE-T959
           MOVE  2                     TO  INP-ATRIB-CHAVEI
           MOVE  1                     TO  INP-ATRIB-BACENI
           MOVE  1                     TO  INP-PAGINA-T959
           MOVE  SPACES                TO  INP-CHAVEI-T959
                                           INP-BACENI-T959
                                           INP-CHAVE-ANT-T959
                                           INP-BACEN-ANT-T959
                                           INP-LISTA-T959
           MOVE  WRK-MSG00             TO  INP-MENSA-T959

           PERFORM  4400-EXCLUIR-DADOS-NAV.

      *----------------------------------------------------------------*
       3010-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4000-TRATAR-RUCAT959            SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM(1:LENGTH  OF  INPUT-T959)
                                       TO  INPUT-T959

           IF  INP-PFK-T959        EQUAL 'B' OR '9' OR 'H'
               PERFORM  4010-TRATAR-PFK-COMUNS
           ELSE
               PERFORM  4030-TRATAR-FASES
           END-IF.

      *----------------------------------------------------------------*
       4000-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4010-TRATAR-PFK-COMUNS          SECTION.
      *----------------------------------------------------------------*

           EVALUATE  INP-PFK-T959

               WHEN  'H'
                     CONTINUE

               WHEN  '9'
                     PERFORM  3010-MONTA-TELA-INICIAL

               WHEN  'B'
                     PERFORM  4400-EXCLUIR-DADOS-NAV
                     PERFORM  4020-RETORNAR-CHAMADOR

           END-EVALUATE.

      *----------------------------------------------------------------*
       4010-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4020-RETORNAR-CHAMADOR          SECTION.
      *----------------------------------------------------------------*

           MOVE  64                    TO  WRK-COMU-LL
           MOVE 'RUCA6959'             TO  WRK-COMU-TRANCODE
           MOVE 'RUCA6950'             TO  WRK-TELA
           MOVE  INP-SENHAS-T959       TO  WRK-COMU-SENHAS
           MOVE  INP-NOPCAO-T959       TO  WRK-COMU-NOPCAO
           MOVE  WRK-COMU-AREA         TO  WRK-MENSAGEM
           MOVE  WRK-CHNG              TO  WRK-FUNCAO.

      *----------------------------------------------------------------*
       4020-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4030-TRATAR-FASES               SECTION.
      *----------------------------------------------------------------*

           EVALUATE  INP-FASE-T959

               WHEN  1
                     PERFORM  4100-TRATAR-FASE-1

               WHEN  2
                     PERFORM  4300-TRATAR-FASE-2

           END-EVALUATE.

      *----------------------------------------------------------------*
       4030-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4100-TRATAR-FASE-1              SECTION.
      *----------------------------------------------------------------*

           EVALUATE  INP-PFK-T959

               WHEN  '.'
                     PERFORM  4105-TRATAR-ENTER-FASE-1

               WHEN  OTHER
                     MOVE  WRK-MSG02   TO  INP-MENSA-T959

           END-EVALUATE.

      *----------------------------------------------------------------*
       4100-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4105-TRATAR-ENTER-FASE-1        SECTION.
      *----------------------------------------------------------------*

           PERFORM  4110-CONSIST-FISICA-FASE-1

           IF  WRK-INCONSIS   EQUAL  'N'
               PERFORM  4115-CONSIST-LOGICA-FASE-1
           END-IF.

           IF  WRK-INCONSIS         EQUAL  'N'
               PERFORM  4140-PROCURAR-CONTR-REJEITADO
           END-IF.

           IF  WRK-INCONSIS   EQUAL  'N'
               PERFORM  4225-MONTAR-FASE-2
           END-IF.

      *----------------------------------------------------------------*
       4105-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4110-CONSIST-FISICA-FASE-1      SECTION.
      *----------------------------------------------------------------*

           MOVE  'N'                   TO  WRK-INCONSIS
           MOVE  1                     TO  INP-ATRIB-CHAVEI
           MOVE  1                     TO  INP-ATRIB-BACENI

           IF  INP-CHAVEI-T959      EQUAL  SPACES  OR  LOW-VALUES
               CONTINUE
           ELSE
               INSPECT INP-CHAVEI-T959 REPLACING  LEADING ' ' BY '0'

               IF  INP-CHAVEI-T959     IS     NUMERIC   AND
                   INP-CHAVEI-T959-N GREATER  ZEROS
                   CONTINUE
               ELSE
                   MOVE  'S'           TO  WRK-INCONSIS
                   MOVE  3             TO  INP-ATRIB-CHAVEI
                   MOVE  WRK-MSG01     TO  INP-MENSA-T959
               END-IF
           END-IF.

           IF  INP-BACENI-T959      EQUAL  SPACES  OR  LOW-VALUES
               CONTINUE
           ELSE
               INSPECT INP-BACENI-T959 REPLACING  LEADING ' ' BY '0'

               IF  INP-BACENI-T959     IS     NUMERIC   AND
                   INP-BACENI-T959-N GREATER  ZEROS
                   CONTINUE
               ELSE
                   MOVE  'S'           TO  WRK-INCONSIS
                   MOVE  3             TO  INP-ATRIB-BACENI
                   MOVE  WRK-MSG01     TO  INP-MENSA-T959
               END-IF
           END-IF.

           IF ( INP-CHAVEI-T959     EQUAL  SPACES  OR  LOW-VALUES ) AND
              ( INP-BACENI-T959     EQUAL  SPACES  OR  LOW-VALUES )
               MOVE  'S'               TO  WRK-INCONSIS
               MOVE  2                 TO  INP-ATRIB-CHAVEI
               MOVE  1                 TO  INP-ATRIB-BACENI
               MOVE  WRK-MSG00         TO  INP-MENSA-T959
           END-IF.

           IF  INP-CHAVEI-T959      EQUAL  INP-CHAVE-ANT-T959  AND
               INP-BACENI-T959      EQUAL  INP-BACEN-ANT-T959
               MOVE  'S'               TO  WRK-INCONSIS
           ELSE
               MOVE  1                 TO  INP-FASE-T959
               MOVE  SPACES            TO  INP-CHAVE-ANT-T959
                                           INP-BACEN-ANT-T959
                                           INP-LISTA-T959
               PERFORM  4400-EXCLUIR-DADOS-NAV
           END-IF.

           IF  INP-NOPCAO-T959      EQUAL  SPACES OR LOW-VALUES
               CONTINUE
           ELSE
               INSPECT  INP-NOPCAO-T959 REPLACING  LEADING ' ' BY '0'

               IF  INP-NOPCAO-T959  IS  NUMERIC
                   CONTINUE
               ELSE
                   MOVE  'S'           TO  WRK-INCONSIS
                   MOVE  3             TO  INP-ATRIB-NOPCAO
                   MOVE  WRK-MSG01     TO  INP-MENSA-T959
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       4110-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4115-CONSIST-LOGICA-FASE-1      SECTION.
      *----------------------------------------------------------------*

           IF  INP-CHAVEI-T959        IS  NUMERIC   AND
               INP-CHAVEI-T959-N GREATER  ZEROS
               PERFORM  4120-VALIDAR-CONTRATO
           ELSE
               PERFORM  4130-VALIDAR-REF-BACEN
           END-IF.

      *----------------------------------------------------------------*
       4115-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4120-VALIDAR-CONTRATO           SECTION.
      *----------------------------------------------------------------*

           PERFORM  4125-SELECT-RUFIB002

           IF  SQLCODE              EQUAL  +100
               MOVE  'S'               TO  WRK-INCONSIS
               MOVE  3                 TO  INP-ATRIB-CHAVEI
               MOVE  WRK-MSG03         TO  INP-MENSA-T959
           ELSE
               IF  INP-BACENI-T959     IS     NUMERIC   AND
                   INP-BACENI-T959-N GREATER  ZEROS

                   IF  INP-BACENI-T959 EQUAL CREFT-RECOR OF RUFIB002
                       CONTINUE
                   ELSE
                       MOVE  'S'       TO  WRK-INCONSIS
                       MOVE  3         TO  INP-ATRIB-BACENI
                       MOVE  WRK-MSG04 TO  INP-MENSA-T959
                   END-IF
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       4120-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4125-SELECT-RUFIB002            SECTION.
      *----------------------------------------------------------------*

           MOVE  INP-CHAVEI-T959-N     TO  CCONTR           OF RUFIB002

           EXEC SQL
                SELECT
                        CCONTR                        ,
                        CREFT_RECOR
                  INTO
                        :RUFIB002.CCONTR              ,
                        :RUFIB002.CREFT-RECOR
                  FROM  DB2PRD.CONTRATO_RURAL
                 WHERE  CCONTR            = :RUFIB002.CCONTR
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  AND  +100 )  OR
              ( SQLWARN0            EQUAL  'W'             )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'CONTRATO_RURAL'   TO  ERR-DBD-TAB
               MOVE 'SELECT'           TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0001'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       4125-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4130-VALIDAR-REF-BACEN          SECTION.
      *----------------------------------------------------------------*

           PERFORM  4135-SELECT-RUFIB002

           IF  SQLCODE              EQUAL  +100
               MOVE  'S'               TO  WRK-INCONSIS
               MOVE  3                 TO  INP-ATRIB-BACENI
               MOVE  WRK-MSG04         TO  INP-MENSA-T959
           END-IF.

      *----------------------------------------------------------------*
       4130-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4135-SELECT-RUFIB002            SECTION.
      *----------------------------------------------------------------*

           MOVE  INP-BACENI-T959       TO  CREFT-RECOR      OF RUFIB002
           MOVE  ZEROS                 TO  CCONTR           OF RUFIB002

           EXEC SQL
                SELECT
                        CCONTR                        ,
                        CREFT_RECOR
                  INTO
                        :RUFIB002.CCONTR              ,
                        :RUFIB002.CREFT-RECOR
                  FROM  DB2PRD.CONTRATO_RURAL
                 WHERE  CREFT_RECOR       = :RUFIB002.CREFT-RECOR
                   AND  CCONTR           >= :RUFIB002.CCONTR
                 FETCH  FIRST 1 ROW ONLY
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  AND  +100 )  OR
              ( SQLWARN0            EQUAL  'W'              )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'CONTRATO_RURAL'   TO  ERR-DBD-TAB
               MOVE 'SELECT'           TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0002'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       4135-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4140-PROCURAR-CONTR-REJEITADO   SECTION.
      *----------------------------------------------------------------*

           MOVE  'N'                   TO  WRK-ENCONTROU

           PERFORM  4145-SELECT-MAX-RUCAB015

           IF  SQLCODE              EQUAL  ZEROS
               PERFORM  4150-SELECT-RUCAB015
           END-IF.

           IF  CSIT-ALT-REG     OF RUCAB015  EQUAL  5
               PERFORM  4160-PROCURAR-MSG-ERRO
           END-IF.

           IF  WRK-ENCONTROU        EQUAL  'N'
               IF  INP-CHAVEI-T959     IS     NUMERIC   AND
                   INP-CHAVEI-T959-N GREATER  ZEROS
                   MOVE  'S'           TO  WRK-INCONSIS
                   MOVE  3             TO  INP-ATRIB-CHAVEI
                   MOVE  WRK-MSG05     TO  INP-MENSA-T959
               END-IF

               IF  INP-BACENI-T959     IS     NUMERIC   AND
                   INP-BACENI-T959-N GREATER  ZEROS
                   MOVE  'S'           TO  WRK-INCONSIS
                   MOVE  3             TO  INP-ATRIB-BACENI
                   MOVE  WRK-MSG05     TO  INP-MENSA-T959
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       4140-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4145-SELECT-MAX-RUCAB015        SECTION.
      *----------------------------------------------------------------*

           MOVE  CCONTR OF RUFIB002    TO  CCONTR           OF RUCAB015

           EXEC SQL
                SELECT
                        MAX(HINCL_REG)
                  INTO
                        :RUCAB015.HINCL-REG
                  FROM  DB2PRD.TMANUT_CONTR_RECOR
                 WHERE  CCONTR             = :RUCAB015.CCONTR
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS AND +100 AND -305 ) OR
              ( SQLWARN0            EQUAL  'W'                     )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TMANUT_CONTR_RECOR'
                                       TO  ERR-DBD-TAB
               MOVE 'SELECT'           TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0003'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE              EQUAL  +100 OR  -305
               MOVE  ZEROS             TO  CSIT-ALT-REG     OF RUCAB015
           END-IF.

      *----------------------------------------------------------------*
       4145-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4150-SELECT-RUCAB015            SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                SELECT
                        CSIT_ALT_REG
                  INTO
                        :RUCAB015.CSIT-ALT-REG
                  FROM  DB2PRD.TMANUT_CONTR_RECOR
                 WHERE  CCONTR       = :RUCAB015.CCONTR
                   AND  HINCL_REG    = :RUCAB015.HINCL-REG
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS AND +100 )  OR
              ( SQLWARN0            EQUAL  'W'            )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TMANUT_CONTR_RECOR'
                                       TO  ERR-DBD-TAB
               MOVE 'SELECT'           TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0004'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE              EQUAL  +100
               MOVE  ZEROS             TO  CSIT-ALT-REG     OF RUCAB015
           END-IF.

      *----------------------------------------------------------------*
       4150-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4160-PROCURAR-MSG-ERRO          SECTION.
      *----------------------------------------------------------------*

           PERFORM  4165-OPEN-CSR01-RUCAB013
           PERFORM  4170-FETCH-CSR01-RUCAB013

           PERFORM
             UNTIL  SQLCODE         EQUAL  +100

                    PERFORM  4180-OBTER-COD-ERRO
                    PERFORM  4170-FETCH-CSR01-RUCAB013
           END-PERFORM.

           PERFORM  4175-CLOSE-CSR01-RUCAB013.

      *----------------------------------------------------------------*
       4160-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4165-OPEN-CSR01-RUCAB013        SECTION.
      *----------------------------------------------------------------*

           MOVE  CCONTR    OF RUCAB015 TO  CCONTR           OF RUCAB013
           MOVE  HINCL-REG OF RUCAB015 TO  HINCL-REG        OF RUCAB013
           MOVE  ZEROS                 TO  NENVIO-RECOR     OF RUCAB013
           MOVE  ZEROS                 TO  NREG-MSGEM-RECOR OF RUCAB013
           MOVE  2                     TO  CTPO-CONTD-MSGEM OF RUCAB013

           EXEC SQL
                OPEN CSR01-RUCAB013
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
              ( SQLWARN0            EQUAL  'W'    )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TENVIO_RETOR_MANUT'
                                       TO  ERR-DBD-TAB
               MOVE 'OPEN'             TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0005'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       4165-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4170-FETCH-CSR01-RUCAB013       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                FETCH  CSR01-RUCAB013
                 INTO  :RUCAB013.CCONTR              ,
                       :RUCAB013.HINCL-REG           ,
                       :RUCAB013.NENVIO-RECOR        ,
                       :RUCAB013.NREG-MSGEM-RECOR    ,
                       :RUCAB013.CTPO-CONTD-MSGEM    ,
                       :RUCAB013.WCONTD-MSGEM-RURAL
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  AND +100 )  OR
              ( SQLWARN0            EQUAL  'W'             )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TENVIO_RETOR_MANUT'
                                       TO  ERR-DBD-TAB
               MOVE 'FETCH'            TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0006'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       4170-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4175-CLOSE-CSR01-RUCAB013       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                CLOSE CSR01-RUCAB013
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
              ( SQLWARN0            EQUAL  'W'    )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TENVIO_RETOR_MANUT'
                                       TO  ERR-DBD-TAB
               MOVE 'CLOSE'            TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0007'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       4175-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4180-OBTER-COD-ERRO             SECTION.
      *----------------------------------------------------------------*

           PERFORM  4185-SELECT-RUCAB019

           IF  CSIT-MSGEM OF RUCAB019  EQUAL  3
               PERFORM  4190-OBTER-MSG-ERRO
           END-IF.

      *----------------------------------------------------------------*
       4180-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4185-SELECT-RUCAB019            SECTION.
      *----------------------------------------------------------------*

           MOVE  CCONTR            OF RUCAB013
                                       TO  CCONTR           OF RUCAB019
           MOVE  HINCL-REG         OF RUCAB013
                                       TO  HINCL-REG        OF RUCAB019
           MOVE  NENVIO-RECOR      OF RUCAB013
                                       TO  NENVIO-RECOR     OF RUCAB019
           MOVE  NREG-MSGEM-RECOR  OF RUCAB013
                                       TO  NREG-MSGEM-RECOR OF RUCAB019

           EXEC SQL
                SELECT
                        CSIT_MSGEM             ,
                        CMSGEM_RECOR
                  INTO
                        :RUCAB019.CSIT-MSGEM   ,
                        :RUCAB019.CMSGEM-RECOR
                          :WRK-B019-CMSGEM-RECOR
                  FROM  DB2PRD.TSIT_MANUT_RECOR
                 WHERE  CCONTR           = :RUCAB019.CCONTR
                   AND  HINCL_REG        = :RUCAB019.HINCL-REG
                   AND  NENVIO_RECOR     = :RUCAB019.NENVIO-RECOR
                   AND  NREG_MSGEM_RECOR = :RUCAB019.NREG-MSGEM-RECOR
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS AND +100 )  OR
              ( SQLWARN0            EQUAL  'W'            )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TSIT_MANUT_RECOR' TO  ERR-DBD-TAB
               MOVE 'SELECT'           TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0008'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE              EQUAL  ZEROS
               IF  WRK-B019-CMSGEM-RECOR  EQUAL  -1
                   MOVE  SPACES        TO  CMSGEM-RECOR     OF RUCAB019
               END-IF
           ELSE
               MOVE  ZEROS             TO  CSIT-MSGEM       OF RUCAB019
               MOVE  SPACES            TO  CMSGEM-RECOR     OF RUCAB019
           END-IF.

      *----------------------------------------------------------------*
       4185-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4190-OBTER-MSG-ERRO             SECTION.
      *----------------------------------------------------------------*

           EVALUATE  CMSGEM-RECOR OF RUCAB019

           WHEN  'COR0001R1'
                  PERFORM  4195-OBTER-MSG-COR0001R1

           WHEN  'COR0001E '
           WHEN  'COR0005E '
VI0725     WHEN  'COR0005R1'

                  IF  CTPO-CONTD-MSGEM OF RUCAB013  EQUAL  3
                      PERFORM  4200-OBTER-MSG-COR0001E-5E
                  END-IF

           WHEN  'GEN0004  '
                  PERFORM  4205-OBTER-MSG-GEN0004

           WHEN  'TMNF     '

                  IF  CTPO-CONTD-MSGEM OF RUCAB013  EQUAL  2
                      PERFORM  4210-OBTER-MSG-TMNF
                  END-IF

           END-EVALUATE.

      *----------------------------------------------------------------*
       4190-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4195-OBTER-MSG-COR0001R1        SECTION.
      *----------------------------------------------------------------*

           MOVE  WCONTD-MSGEM-RURAL-TEXT OF RUCAB013
                                          (1: LENGTH OF RCOR04-REGISTRO)
                                       TO  RCOR04-REGISTRO

           IF  RCOR04-COD-ERRO      EQUAL '********'
               CONTINUE
           ELSE
               MOVE  RCOR04-COD-ERRO  (1:08)
                                       TO  WRK-959-CERRO
               MOVE  RCOR04-DESCR-ERRO(1: 70)
                                       TO  WRK-959-DERRO

               IF  RCOR04-DESCR-ERRO EQUAL  SPACES OR LOW-VALUES
                   PERFORM  4215-OBTER-DESC-ERRO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       4195-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4200-OBTER-MSG-COR0001E-5E      SECTION.
      *----------------------------------------------------------------*

           PERFORM
           VARYING  IND-CHAR         FROM  1 BY 1
             UNTIL  IND-CHAR      GREATER  32493

             IF  WCONTD-MSGEM-RURAL-TEXT OF RUCAB013(IND-CHAR:4)
                                    EQUAL  'ECOR'
                 MOVE  WCONTD-MSGEM-RURAL-TEXT OF RUCAB013(IND-CHAR:8)
                                       TO  WRK-959-CERRO
                 PERFORM  4215-OBTER-DESC-ERRO
             END-IF
           END-PERFORM.

           IF  WRK-ENCONTROU        EQUAL  'N'
               MOVE  CMSGEM-RECOR OF RUCAB019
                                       TO  WRK-959-CERRO
               MOVE  'RCOR NAO RETORNOU O MOTIVO DA INCONSISTENCIA'
                                       TO  WRK-959-DERRO
               PERFORM  4430-INSERT-RUECB048
           END-IF.

      *----------------------------------------------------------------*
       4200-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4205-OBTER-MSG-GEN0004          SECTION.
      *----------------------------------------------------------------*

           MOVE  WCONTD-MSGEM-RURAL-TEXT OF RUCAB013
                                          (1: LENGTH OF RCOR05-REGISTRO)
                                       TO  RCOR05-REGISTRO

           IF  RCOR05-COD-MSG       EQUAL  SPACES OR LOW-VALUES
               CONTINUE
           ELSE
               MOVE  RCOR05-COD-MSG(1:08)
                                       TO  WRK-959-CERRO

               PERFORM  4215-OBTER-DESC-ERRO

               IF  RCOR05-HISTORICO(01:70) EQUAL SPACES OR LOW-VALUES
                   CONTINUE
               ELSE
                   MOVE  RCOR05-HISTORICO(01:70)
                                       TO  WRK-959-DERRO
                   PERFORM  4430-INSERT-RUECB048
               END-IF

               IF  RCOR05-HISTORICO(71:70) EQUAL SPACES OR LOW-VALUES
                   CONTINUE
               ELSE
                   MOVE  RCOR05-HISTORICO(71:70)
                                       TO  WRK-959-DERRO
                   PERFORM  4430-INSERT-RUECB048
               END-IF

               IF  RCOR05-HISTORICO(141:60) EQUAL SPACES OR LOW-VALUES
                   CONTINUE
               ELSE
                   MOVE  RCOR05-HISTORICO(141:60)
                                       TO  WRK-959-DERRO
                   PERFORM  4430-INSERT-RUECB048
               END-IF
           END-IF.

           IF  WRK-ENCONTROU        EQUAL  'N'
               MOVE  CMSGEM-RECOR OF RUCAB019
                                       TO  WRK-959-CERRO
               MOVE  'RCOR NAO RETORNOU O MOTIVO DA INCONSISTENCIA'
                                       TO  WRK-959-DERRO
               PERFORM  4430-INSERT-RUECB048
           END-IF.

      *----------------------------------------------------------------*
       4205-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4210-OBTER-MSG-TMNF             SECTION.
      *----------------------------------------------------------------*

           MOVE  WCONTD-MSGEM-RURAL-TEXT OF RUCAB013
                                          (1: LENGTH OF RCOR03-REGISTRO)
                                       TO  RCOR03-REGISTRO

           IF  RCOR03-CRETOR-PRINC  IS   NUMERIC  AND
               RCOR03-CRETOR-PRINC EQUAL ZEROS
               CONTINUE
           ELSE
               MOVE  'TMNF'            TO  WRK-959-CERRO-X4
               MOVE  RCOR03-CRETOR-PRINC
                                       TO  WRK-959-CERRO-N4

               MOVE  RCOR03-RMSGEM-ERRO(01:31)
                                       TO  WRK-959-DERRO
               PERFORM  4430-INSERT-RUECB048

               MOVE  RCOR03-RMSGEM-ERRO(32:49)
                                       TO  WRK-959-DERRO
               PERFORM  4430-INSERT-RUECB048
            END-IF.

      *----------------------------------------------------------------*
       4210-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4215-OBTER-DESC-ERRO            SECTION.
      *----------------------------------------------------------------*

           PERFORM  4220-ACESSAR-RCOR5000

           MOVE  RCOR02-S-DSCERRO(1)(1:70)
                                       TO  WRK-959-DERRO

           PERFORM  4430-INSERT-RUECB048.

      *----------------------------------------------------------------*
       4215-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4220-ACESSAR-RCOR5000           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                      RCOR02-REGISTRO

           MOVE  1                     TO  RCOR02-E-TPCONS
           MOVE  WRK-959-CERRO         TO  RCOR02-E-CODERRO
           MOVE  'RCOR5000'            TO  WRK-MODULO

           CALL  WRK-MODULO         USING  RCOR02-REGISTRO
                                           ERRO-AREA
                                           SQLCA

           EVALUATE RCOR02-S-COD-RETORNO
               WHEN ZEROS
                    CONTINUE
               WHEN 100
                    MOVE 'DESCRICAO DO ERRO NAO ENCONTRADA NO RCOR'
                                       TO RCOR02-S-DSCERRO(1)
               WHEN 10
                    MOVE 'INCONSISTENCIA NOS DADOS ENVIADOS AO RCOR5000'
                                       TO RCOR02-S-DSCERRO(1)
               WHEN 99
                    MOVE 'ERRO NO ACESSO DB2 TABELA RCOR'
                                       TO RCOR02-S-DSCERRO(1)
               WHEN OTHER
                    MOVE 'ERRO NAO IDENTIFICADO'
                                       TO RCOR02-S-DSCERRO(1)
           END-EVALUATE.

      *----------------------------------------------------------------*
       4220-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4225-MONTAR-FASE-2              SECTION.
      *----------------------------------------------------------------*

           MOVE  SPACES                TO  INP-FIMAMOS-T959
           MOVE  2                     TO  INP-FASE-T959
           MOVE  2                     TO  INP-ATRIB-CHAVEI
           MOVE  1                     TO  INP-ATRIB-BACENI
           MOVE  1                     TO  INP-PAGINA-T959
           MOVE  INP-CHAVEI-T959       TO  INP-CHAVE-ANT-T959
           MOVE  INP-BACENI-T959       TO  INP-BACEN-ANT-T959
           MOVE  ZEROS                 TO  ACU-DESPREZAR

           PERFORM  4230-MONTAR-TELA.

      *----------------------------------------------------------------*
       4225-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4230-MONTAR-TELA                SECTION.
      *----------------------------------------------------------------*

           MOVE  SPACES                TO  INP-LISTA-T959
           MOVE  10000                 TO  WRK-CSEQ-CTRL-MDULO

           PERFORM  4410-OPEN-CSR02-RUECB048

           PERFORM  4415-FETCH-CSR02-RUECB048
           VARYING  IND-LIN          FROM  1 BY 1
             UNTIL  IND-LIN       GREATER  (ACU-DESPREZAR + 1)
                OR  SQLCODE         EQUAL  +100

           PERFORM
           VARYING  IND-LIN          FROM  1 BY 1
             UNTIL  IND-LIN       GREATER  14
                OR  SQLCODE         EQUAL  +100

                MOVE  WRK-DADOS-NAV-T959
                                       TO  INP-LINHA-T959(IND-LIN)

                PERFORM  4415-FETCH-CSR02-RUECB048
           END-PERFORM.

           IF  SQLCODE              EQUAL  +100
               MOVE  '*'               TO  INP-FIMAMOS-T959
               MOVE  WRK-MSG07         TO  INP-MENSA-T959

           ELSE
               MOVE  SPACES            TO  INP-FIMAMOS-T959
               MOVE  WRK-MSG06         TO  INP-MENSA-T959
           END-IF.

           PERFORM  4420-CLOSE-CSR02-RUECB048.

      *----------------------------------------------------------------*
       4230-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4300-TRATAR-FASE-2              SECTION.
      *----------------------------------------------------------------*

           EVALUATE  INP-PFK-T959

               WHEN  '.'
                     PERFORM  4305-TRATAR-ENTER-FASE-2

               WHEN  '7'
                     PERFORM  4310-TRATAR-PFK07

               WHEN  '8'
                     PERFORM  4320-TRATAR-PFK08

               WHEN  OTHER
                     MOVE  WRK-MSG02   TO  INP-MENSA-T959
           END-EVALUATE.

      *----------------------------------------------------------------*
       4300-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4305-TRATAR-ENTER-FASE-2        SECTION.
      *----------------------------------------------------------------*

           IF  INP-CHAVEI-T959      EQUAL  INP-CHAVE-ANT-T959  AND
               INP-BACENI-T959      EQUAL  INP-BACEN-ANT-T959
               CONTINUE
           ELSE
               MOVE  1                 TO  INP-FASE-T959
               MOVE  SPACES            TO  INP-CHAVE-ANT-T959
                                           INP-BACEN-ANT-T959
                                           INP-LISTA-T959
               PERFORM  4105-TRATAR-ENTER-FASE-1
           END-IF.

      *----------------------------------------------------------------*
       4305-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4310-TRATAR-PFK07               SECTION.
      *----------------------------------------------------------------*

           IF  INP-CHAVEI-T959      EQUAL  INP-CHAVE-ANT-T959  AND
               INP-BACENI-T959      EQUAL  INP-BACEN-ANT-T959
               PERFORM  4315-VOLTAR-PAGINA
           ELSE
               MOVE  WRK-MSG02         TO  INP-MENSA-T959
           END-IF.

      *----------------------------------------------------------------*
       4310-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4315-VOLTAR-PAGINA              SECTION.
      *----------------------------------------------------------------*

           IF  INP-PAGINA-T959      EQUAL  1
               MOVE  WRK-MSG08         TO  INP-MENSA-T959
           ELSE
               MOVE  INP-PAGINA-T959   TO  ACU-PAGINA
               COMPUTE ACU-DESPREZAR    =  (ACU-PAGINA - 2) * 14
               SUBTRACT  1           FROM  ACU-PAGINA
               MOVE  ACU-PAGINA        TO  INP-PAGINA-T959

               PERFORM  4230-MONTAR-TELA
           END-IF.

      *----------------------------------------------------------------*
       4315-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4320-TRATAR-PFK08               SECTION.
      *----------------------------------------------------------------*

           IF  INP-CHAVEI-T959      EQUAL  INP-CHAVE-ANT-T959  AND
               INP-BACENI-T959      EQUAL  INP-BACEN-ANT-T959
               PERFORM  4325-AVANCAR-PAGINA
           ELSE
               MOVE  WRK-MSG02         TO  INP-MENSA-T959
           END-IF.

      *----------------------------------------------------------------*
       4320-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4325-AVANCAR-PAGINA             SECTION.
      *----------------------------------------------------------------*

           IF  INP-FIMAMOS-T959     EQUAL  '*'
               MOVE  WRK-MSG09         TO  INP-MENSA-T959
           ELSE
               MOVE  INP-PAGINA-T959   TO  ACU-PAGINA
               COMPUTE ACU-DESPREZAR    =  ACU-PAGINA * 14
               ADD   1                 TO  ACU-PAGINA
               MOVE  ACU-PAGINA        TO  INP-PAGINA-T959

               PERFORM  4230-MONTAR-TELA
           END-IF.

      *----------------------------------------------------------------*
       4325-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4400-EXCLUIR-DADOS-NAV          SECTION.
      *----------------------------------------------------------------*

           PERFORM  4410-OPEN-CSR02-RUECB048
           PERFORM  4415-FETCH-CSR02-RUECB048

           PERFORM
             UNTIL  SQLCODE   EQUAL  +100

             PERFORM  4425-DELETE-RUECB048
             PERFORM  4415-FETCH-CSR02-RUECB048
           END-PERFORM.

           PERFORM  4420-CLOSE-CSR02-RUECB048.

      *----------------------------------------------------------------*
       4400-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4410-OPEN-CSR02-RUECB048        SECTION.
      *----------------------------------------------------------------*

           MOVE  LNK-IO-LTERM          TO  CTERM           OF RUECB048
           MOVE  959                   TO  CIDTFD-MDULO    OF RUECB048
           MOVE  ZEROS                 TO  CSEQ-CTRL-MDULO OF RUECB048

           EXEC SQL
                OPEN CSR02-RUECB048
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
              ( SQLWARN0            EQUAL  'W'    )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TAPOIO_UTILZ_MDULO'
                                       TO  ERR-DBD-TAB
               MOVE 'OPEN'             TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0009'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       4410-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4415-FETCH-CSR02-RUECB048       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                FETCH  CSR02-RUECB048
                 INTO  :RUECB048.CTERM            ,
                       :RUECB048.CIDTFD-MDULO     ,
                       :RUECB048.CSEQ-CTRL-MDULO  ,
                       :RUECB048.WRESUL-CONS-MDULO
                         :WRK-B048-WRESUL-CONS-MDULO
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  AND +100 )  OR
              ( SQLWARN0            EQUAL  'W'             )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TAPOIO_UTILZ_MDULO'
                                       TO  ERR-DBD-TAB
               MOVE 'FETCH'            TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0010'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE              EQUAL  ZEROS
              IF  WRK-B048-WRESUL-CONS-MDULO  EQUAL  -1
                  MOVE  ZEROS          TO  WRESUL-CONS-MDULO-LEN
                  MOVE  SPACES         TO  WRESUL-CONS-MDULO-TEXT
              END-IF

              MOVE  WRESUL-CONS-MDULO-TEXT(1: WRESUL-CONS-MDULO-LEN)
                                       TO  WRK-DADOS-NAV-T959
           ELSE
               MOVE  SPACES            TO  WRK-DADOS-NAV-T959
           END-IF.

      *----------------------------------------------------------------*
       4415-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4420-CLOSE-CSR02-RUECB048       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                CLOSE CSR02-RUECB048
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
              ( SQLWARN0            EQUAL  'W'    )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TAPOIO_UTILZ_MDULO'
                                       TO  ERR-DBD-TAB
               MOVE 'CLOSE'            TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0011'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       4420-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4425-DELETE-RUECB048            SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                DELETE
                  FROM  DB2PRD.TAPOIO_UTILZ_MDULO
                 WHERE  CTERM           = :RUECB048.CTERM
                   AND  CIDTFD_MDULO    = :RUECB048.CIDTFD-MDULO
                   AND  CSEQ_CTRL_MDULO = :RUECB048.CSEQ-CTRL-MDULO
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  AND  +100 )  OR
              ( SQLWARN0            EQUAL  'W'              )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TAPOIO_UTILZ_MDULO'
                                       TO  ERR-DBD-TAB
               MOVE 'DELETE'           TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0012'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       4425-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4430-INSERT-RUECB048            SECTION.
      *----------------------------------------------------------------*

           ADD   1                     TO  ACU-REGISTRO

           MOVE  ACU-REGISTRO          TO  WRK-CSEQ-CTRL-MDULO
           MOVE  LNK-IO-LTERM          TO  CTERM           OF RUECB048
           MOVE  959                   TO  CIDTFD-MDULO    OF RUECB048
           MOVE  WRK-CSEQ-CTRL-MDULO   TO  CSEQ-CTRL-MDULO OF RUECB048
           MOVE  LENGTH OF WRK-DADOS-NAV-T959
                                       TO  WRESUL-CONS-MDULO-LEN
                                                          OF  RUECB048
           MOVE  WRK-DADOS-NAV-T959    TO  WRESUL-CONS-MDULO-TEXT
                                                          OF  RUECB048
           MOVE  ZEROS                 TO  WRK-B048-WRESUL-CONS-MDULO
           MOVE  'S'                   TO  WRK-ENCONTROU

           EXEC SQL
                INSERT  INTO DB2PRD.TAPOIO_UTILZ_MDULO
                        (CTERM                         ,
                         CIDTFD_MDULO                  ,
                         CSEQ_CTRL_MDULO               ,
                         WRESUL_CONS_MDULO             )
                VALUES
                       (:RUECB048.CTERM                ,
                        :RUECB048.CIDTFD-MDULO         ,
                        :RUECB048.CSEQ-CTRL-MDULO      ,
                        :RUECB048.WRESUL-CONS-MDULO
                           :WRK-B048-WRESUL-CONS-MDULO )
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  AND  -803 )  OR
              ( SQLWARN0            EQUAL  'W'              )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TAPOIO_UTILZ_MDULO'
                                       TO  ERR-DBD-TAB
               MOVE 'INSERT'           TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0013'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       4430-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       6000-DEVOLVER-TELA              SECTION.
      *----------------------------------------------------------------*

           MOVE  INP-SENHAS-T959   TO  OUT-SENHAS-T959
           MOVE  INP-NOPCAO-T959   TO  OUT-NOPCAO-T959
           MOVE  INP-FIMAMOS-T959  TO  OUT-FIMAMOS-T959
           MOVE  INP-FASE-T959     TO  OUT-FASE-T959
           MOVE  INP-ATRIB-T959    TO  OUT-ATRIB-T959
           MOVE  INP-PAGINA-T959   TO  OUT-PAGINA-T959
           MOVE  INP-CPOANT-T959   TO  OUT-CPOANT-T959
           MOVE  INP-CHAVEI-T959   TO  OUT-CHAVEI-T959
           MOVE  INP-BACENI-T959   TO  OUT-BACENI-T959
           MOVE  INP-LISTA-T959    TO  OUT-LISTA-T959
           MOVE  INP-MENSA-T959    TO  OUT-MENSA-T959

           IF  INP-NOPCAO-T959  EQUAL  ZEROS
               MOVE  SPACES        TO  OUT-NOPCAO-T959
           ELSE
               MOVE  INP-NOPCAO-T959
                                   TO  OUT-NOPCAO-T959
           END-IF.

           EVALUATE  INP-ATRIB-CHAVEI
               WHEN  1
                     MOVE  WRK-209     TO  OUT-CHAVEI-ATTR-T959
               WHEN  2
                     MOVE  WRK-49361   TO  OUT-CHAVEI-ATTR-T959
               WHEN  3
                     MOVE  WRK-49369   TO  OUT-CHAVEI-ATTR-T959
               WHEN  4
                     MOVE  WRK-225     TO  OUT-CHAVEI-ATTR-T959
           END-EVALUATE.

           EVALUATE  INP-ATRIB-BACENI
               WHEN  1
                     MOVE  WRK-209     TO  OUT-BACENI-ATTR-T959
               WHEN  2
                     MOVE  WRK-49361   TO  OUT-BACENI-ATTR-T959
               WHEN  3
                     MOVE  WRK-49369   TO  OUT-BACENI-ATTR-T959
               WHEN  4
                     MOVE  WRK-225     TO  OUT-BACENI-ATTR-T959
           END-EVALUATE.

           IF  INP-ATRIB-NOPCAO EQUAL  3
               MOVE  WRK-49369         TO  OUT-NOPCAO-ATTR-T959
           ELSE
               MOVE  WRK-209           TO  OUT-NOPCAO-ATTR-T959
           END-IF.

      *----------------------------------------------------------------*
       6000-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       7000-PROCESSAR-BRAD0660         SECTION.
      *----------------------------------------------------------------*

           MOVE  LENGTH OF OUTPUT-T959 TO  OUT-LL-T959
                                           660-LL-MENSAGEM-T959
           MOVE  LENGTH OF 660-RUCAT959-SEG00
                                       TO  660-LL-AREA-T959

           CALL 'BRAD0660'          USING  OUTPUT-T959
                                           660-RUCAT959-SEG00

           IF ( RETURN-CODE     NOT EQUAL ZEROS )
               MOVE 'APL'              TO  ERR-TIPO-ACESSO
               MOVE  RETURN-CODE       TO  WRK-RETURN-CODE
               MOVE  14                TO  WRK-LOCAL-ERRO
               MOVE  WRK-ERRO-BRAD0660 TO  ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           MOVE  WRK-ISRT              TO  WRK-FUNCAO
           MOVE  OUTPUT-T959           TO  WRK-MENSAGEM.

      *----------------------------------------------------------------*
       7000-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'SENHAS02'             TO  ERR-PGM
           MOVE  WRK-COD-USER          TO  ERR-COD-USER
           MOVE  WRK-COD-DEPTO         TO  ERR-COD-DEPTO
           MOVE 'RUCA6959'             TO  ERR-MODULO

           IF  ERR-TIPO-ACESSO  EQUAL  'APL'
               CALL 'BRAD7100'      USING  LNK-IO-PCB
                                           ERRO-AREA
                                           LNK-ALT-PCB
           ELSE
               CALL 'BRAD7100'      USING  LNK-IO-PCB
                                           ERRO-AREA
                                           LNK-ALT-PCB
                                           SQLCA
           END-IF.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.  EXIT.
      *----------------------------------------------------------------*
