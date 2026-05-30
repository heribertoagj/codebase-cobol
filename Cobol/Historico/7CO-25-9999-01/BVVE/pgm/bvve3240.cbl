      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. BVVE3240.
       AUTHOR.     FERNANDA RODRIGUES.

      *================================================================*
      *                   B S I   T E C N O L O G I A                  *
      *----------------------------------------------------------------*
      *    ANALISTA   : FERNANDA RODRIGUES  -  BSI TECNOLOGIA          *
      *    DATA       : OUTUBRO/2015                                   *
      *----------------------------------------------------------------*
      *    OBJETIVO   : AFILIACAO ELETRONICA DE ESTABELECIMENTOS CIELO *
      *                 LISTA PROPOSTA                                 *
      *----------------------------------------------------------------*
      *    MAPA/MAPSET: BVVE324                                        *
      *----------------------------------------------------------------*
      *    TRANSACAO  : VV9I                                           *
      *----------------------------------------------------------------*
      *    TABELAS DB2: BVVEB011 - DB2PRD.PPSTA_AFLIA_ESTBL            *
      *----------------------------------------------------------------*
      *    NAVEGACAO  :                                                *
      *      CHAMADO POR: BVVE3225              CHAMA: BVVE3225        *
      *================================================================*

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
       77  FILLER                      PIC  X(32)        VALUE
           '*  INICIO DA WORKING BVVE3240  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)        VALUE
           '*         INDEXADORES          *'.
      *----------------------------------------------------------------*
       01  IND-1                       PIC  9(05) COMP-3 VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)        VALUE
           '*         ACUMULADORES         *'.
      *----------------------------------------------------------------*
       01  ACU-SEL                     PIC  9(03) COMP-3 VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)        VALUE
           '*     VARIAVEIS AUXILIARES     *'.
      *----------------------------------------------------------------*
       01  WRK-ABSTIME                 PIC S9(15) COMP-3 VALUE ZEROS.
       01  WRK-INCONSISTENTE           PIC  X(01)        VALUE SPACES.
       01  WRK-ACHOU                   PIC  X(01)        VALUE SPACES.

       01  WRK-DATA-INI                PIC X(10)         VALUE SPACES.
       01  WRK-DATA-FIM                PIC X(10)         VALUE SPACES.


       01  WRK-CONTA-AUX               PIC +9(13)        VALUE ZEROS.
       01  FILLER                  REDEFINES WRK-CONTA-AUX.
           03  FILLER                  PIC  X(01).
           03  FILLER                  PIC  X(06).
           03  WRK-CONTA-N             PIC  9(07).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*      AREA DE MENSAGENS       *'.
      *----------------------------------------------------------------*
       01  WRK-MENSAGENS.
           03  WRK-MSG01               PIC  X(79)        VALUE
               'PF Invalida'.

           03  WRK-MSG02               PIC  X(79)        VALUE
               'Transacao Encerrada. Entrada somente via Menu'.

           03  WRK-MSG03               PIC  X(79)        VALUE
               'Selecione a Proposta desejada com "X" e tecle <ENTER>'.

           03  WRK-MSG04               PIC  X(79)        VALUE
               'Selecione apenas uma Proposta'.

           03  WRK-MSG05               PIC  X(79)        VALUE
               'Nao ha dados a serem exibidos'.

           03  WRK-MSG09               PIC  X(79)        VALUE
               'Nao ha paginas para retornar'.

           03  WRK-MSG10               PIC  X(79)        VALUE
               'Nao ha paginas para avancar'.

           03  WRK-MSG98.
               05  FILLER              PIC  X(35)        VALUE
                   'ERRO NAO IDENTIF PELO PGM. SQLCODE='.
               05  WRK-SQLCODE         PIC  +999         VALUE ZEROS.
               05  FILLER              PIC  X(09)        VALUE
                   ' E LOCAL='.
               05  WRK-LOCAL-SQLCODE   PIC  9(02)        VALUE ZEROS.
               05  FILLER              PIC  X(20)        VALUE
                   '. CONTACTAR ANALISTA'.

           03  WRK-MSG99.
               05  FILLER              PIC  X(35)        VALUE
                   'ERRO NAO IDENTIF PELO PGM. EIBRESP='.
               05  WRK-EIBRESP         PIC  9(04)        VALUE ZEROS.
               05  FILLER              PIC  X(09)        VALUE
                   ' E LOCAL='.
               05  WRK-LOCAL-EIBRESP   PIC  9(02)        VALUE ZEROS.
               05  FILLER              PIC  X(20)        VALUE
                   '. CONTACTAR ANALISTA'.

           03  WRK-MSG00.
               05  FILLER              PIC  X(19)        VALUE
                   'ERRO CICS - ABCODE='.
               05  WRK-ABCODE-MSG      PIC  X(08)        VALUE SPACES.
               05  FILLER              PIC  X(09)        VALUE
                   ' E LOCAL='.
               05  WRK-LOCAL-ABCODE    PIC  9(02)        VALUE ZEROS.
               05  FILLER              PIC  X(30)        VALUE
                   '. CONTACTAR ANALISTA'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*  AREA COMUNICACAO SENH1005   *'.
      *----------------------------------------------------------------*
       01  WRK-AREA-SENH1005-PAS.
           03  WRK-VERSAO-PAS          PIC  9(02)        VALUE 01.
           03  WRK-MENSAGEM-PAS.
               05  WRK-COD-MSG-PAS     PIC  9(03)        VALUE ZEROS.
               05  FILLER              PIC  X(01)        VALUE SPACES.
               05  WRK-TEXTO-MSG-PAS   PIC  X(79)        VALUE SPACES.
           03  WRK-SISTEMA-PAS         PIC  X(04)        VALUE SPACES.
           03  WRK-NIVEL-PAS           PIC  X(01)        VALUE SPACES.
               88  WRK-PRINCIPAL       VALUE 'P'.
               88  WRK-SECUNDARIO      VALUE 'S'.
           03  WRK-ACAO-PAS            PIC  X(01)        VALUE SPACES.
               88  WRK-ENCERRA         VALUE 'E'.
               88  WRK-CONTINUA        VALUE 'C'.
               88  WRK-TRATA-ERRO      VALUE 'T'.
           03  WRK-LINHA-COMANDO-PAS   PIC  X(68)        VALUE SPACES.
           03  FILLER                  PIC  X(96)        VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*      COMMAREA AUXILIAR       *'.
      *----------------------------------------------------------------*
       01  WRK-COMMAREA.
           03 WRK-COMM-PADRAO.
              05 WRK-COMM-TRANSACAO    PIC  X(04)        VALUE SPACES.
              05 WRK-COMM-FASE         PIC  X(01)        VALUE SPACES.
              05 WRK-COMM-PAG          PIC  9(05) COMP-3 VALUE ZEROS.
              05 WRK-COMM-ULT-PAG      PIC  9(05) COMP-3 VALUE ZEROS.
              05 WRK-COMM-MENSAG       PIC  X(79)        VALUE SPACES.

           03 WRK-COMM-PGM-3200.
              05 WRK-COMM-AGEN-DEPTO   PIC  9(04)        VALUE ZEROS.

           03 WRK-COMM-PGM-3205.
              05 WRK-COMM-OPCAO        PIC  X(01)        VALUE SPACES.

           03 WRK-COMM-PGM-3235.
              05 WRK-COMM-AGENCIA      PIC  9(05)        VALUE ZEROS.
              05 WRK-COMM-DATA-DE      PIC  X(10)        VALUE SPACES.
              05 WRK-COMM-DATA-ATE     PIC  X(10)        VALUE SPACES.

7C2511     03 WRK-COMM-PGM-3240.
7C2511*       05 WRK-COMM-3240-CNPJ    PIC  9(09)        VALUE ZEROS.
7C2511        05 WRK-COMM-3240-CNPJ    PIC  X(09)        VALUE SPACES.
7C2511*       05 WRK-COMM-3240-FILIAL  PIC  9(04)        VALUE ZEROS.
7C2511        05 WRK-COMM-3240-FILIAL  PIC  X(04)        VALUE SPACES.
7C2511        05 WRK-COMM-3240-CTRL    PIC  9(02)        VALUE ZEROS.
7C2511        05 WRK-COMM-PAG-SEL      PIC  9(05) COMP-3 VALUE ZEROS.
7C2511        05 WRK-COMM-ULT-PAG-SEL  PIC  9(05) COMP-3 VALUE ZEROS.
7C2511
7C2511*----------------------------------------------------------------*
7C2511 01  FILLER                      PIC  X(32)        VALUE
7C2511     '*     AREA PARA FILA TS    *'.
7C2511*----------------------------------------------------------------*
7C2511 01  WRK-ITEM-TS                 PIC S9(04) COMP   VALUE ZEROS.
7C2511
7C2511 01  WRK-NOME-TS.
7C2511     05 WRK-TERMID               PIC  X(04)        VALUE SPACES.
7C2511     05 WRK-TRANID               PIC  X(04)        VALUE SPACES.
7C2511
7C2511 01  WRK-AREA-TS.
7C2511     05 WRK-DADOS-PROP       OCCURS 12.
7C2511        10 WRK-TS-SELEC          PIC  X(001)         VALUE SPACES.
7C2511*       10 WRK-TS-CNPJ           PIC  9(009)         VALUE ZEROS.
7C2511        10 WRK-TS-CNPJ           PIC  X(009)         VALUE SPACES.
7C2511*       10 WRK-TS-FILIAL         PIC  9(004)         VALUE ZEROS.
7C2511        10 WRK-TS-FILIAL         PIC  X(004)         VALUE SPACES.
7C2511        10 WRK-TS-CTRL           PIC  9(002)         VALUE ZEROS.
7C2511        10 WRK-TS-AGENCIA        PIC  9(005)         VALUE ZEROS.
7C2511        10 WRK-TS-CONTA          PIC  9(007)         VALUE ZEROS.
7C2511        10 WRK-TS-DIGITO         PIC  9(001)         VALUE ZEROS.
7C2511        10 WRK-TS-NOME           PIC  X(020)         VALUE SPACES.
7C2511        10 WRK-TS-DT-AFIL        PIC  X(010)         VALUE SPACES.
7C2511        10 WRK-TS-ESTAB          PIC  9(010)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*     AREA DO MAPA BVVE324     *'.
      *----------------------------------------------------------------*
       COPY BVVE324.
       01  WRK-BVVE324R REDEFINES BVVE324I.
           03  FILLER                  PIC  X(103).
           03  WRK-DFHG002I        OCCURS 12 TIMES.
               05  WRK-SELECL          PIC S9(04) COMP.
               05  WRK-SELECA          PIC  X(01).
               05  WRK-SELECI          PIC  X(01).
               05  WRK-CNPJL           PIC S9(04) COMP.
               05  WRK-CNPJA           PIC  X(01).
7C2511         05  WRK-CNPJI           PIC  X(09).
7C2511*        05  WRK-CNPJI-N     REDEFINES WRK-CNPJI
7C2511*                                PIC  9(09).
               05  WRK-FILIALL         PIC S9(04) COMP.
               05  WRK-FILIALA         PIC  X(01).
7C2511         05  WRK-FILIALI         PIC  X(04).
7C2511*        05  WRK-FILIALI-N     REDEFINES WRK-FILIALI
7C2511*                                PIC  9(04).
               05  WRK-CTRLL           PIC S9(04) COMP.
               05  WRK-CTRLA           PIC  X(01).
               05  WRK-CTRLI           PIC  X(02).
               05  WRK-CTRLI-N     REDEFINES WRK-CTRLI
                                       PIC  9(02).
               05  WRK-AGENCIAL        PIC S9(04) COMP.
               05  WRK-AGENCIAA        PIC  X(01).
               05  WRK-AGENCIAI        PIC  X(05).
               05  WRK-AGENCIAI-N     REDEFINES WRK-AGENCIAI
                                       PIC  9(05).
               05  WRK-CONTAL          PIC S9(04) COMP.
               05  WRK-CONTAA          PIC  X(01).
               05  WRK-CONTAI          PIC  X(07).
               05  WRK-CONTAI-N    REDEFINES WRK-CONTAI
                                       PIC  9(07).
               05  WRK-DIGL            PIC S9(04) COMP.
               05  WRK-DIGA            PIC  X(01).
               05  WRK-DIGI            PIC  X(01).
               05  WRK-DIGI-N      REDEFINES WRK-DIGI
                                       PIC  9(01).
               05  WRK-NOMEL           PIC S9(04) COMP.
               05  WRK-NOMEA           PIC  X(01).
               05  WRK-NOMEI           PIC  X(20).
               05  WRK-DTAFILL         PIC S9(04) COMP.
               05  WRK-DTAFILA         PIC  X(01).
               05  WRK-DTAFILI         PIC  X(10).
               05  WRK-ESTABL          PIC S9(04) COMP.
               05  WRK-ESTABA          PIC  X(01).
               05  WRK-ESTABI          PIC  X(10).
               05  WRK-ESTABI-N    REDEFINES WRK-ESTABI
                                       PIC  9(10).
           03  FILLER                  PIC  X(82).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*     AREA NULIDADE            *'.
      *----------------------------------------------------------------*
       01  WRK-BVVEB011-NULL.
           05 WRK-DRETOR-PPSTA-NULL    PIC S9(004) COMP VALUE ZEROS.
           05 WRK-NESTBL-AFLIA-NULL    PIC S9(004) COMP VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*     AREA PARA DB2            *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE BVVEB011
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*     AREA PARA CURSORES       *'.
      *----------------------------------------------------------------*

           EXEC SQL
               DECLARE CSR01-BVVEB011 CURSOR FOR
               SELECT
                    CCNPJ_CPF_ESTBL,
                    CFLIAL_CNPJ_ESTBL,
                    CCTRL_CNPJ_CPF,
7C2511              CCNPJ_CPF_ESTBL_ST,
7C2511              CFLIAL_CNPJ_ESTBL_ST,
7C2511              CCTRL_CNPJ_CPF_ST,
                    IESTBL_AFLIA,
                    DRETOR_PPSTA_AFLIA,
                    CAG_BCRIA,
                    CCTA_BCRIA_CLI,
                    CDIG_CCORR_ESTBL,
                    NESTBL_AFLIA
               FROM DB2PRD.PPSTA_AFLIA_ESTBL
               WHERE
                     CAG_BCRIA           = :BVVEB011.CAG-BCRIA
                 AND DRETOR_PPSTA_AFLIA >= :WRK-DATA-INI
                 AND DRETOR_PPSTA_AFLIA <= :WRK-DATA-FIM
               ORDER BY
                     CCNPJ_CPF_ESTBL,
                     CFLIAL_CNPJ_ESTBL,
                     CCTRL_CNPJ_CPF,
                     DRETOR_PPSTA_AFLIA
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*   FIM DA WORKING BVVE3240   *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  DFHCOMMAREA.
           03  FILLER                  PIC  X(750).

      *================================================================*
       PROCEDURE                       DIVISION USING DFHCOMMAREA.
      *================================================================*

      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           MOVE    01                  TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE

           EXEC CICS
                HANDLE ABEND
                       LABEL(9998-TRATAR-ERRO)
           END-EXEC

           IF EIBRESP                  NOT EQUAL DFHRESP(NORMAL)
              PERFORM 9998-TRATAR-ERRO
           END-IF

           MOVE    02                  TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE

           EXEC CICS
                HANDLE AID
                       PF2   (0100-TRATAR-PF2)
                       PF3   (0200-TRATAR-PF3)
                       PF5   (0250-TRATAR-PF5)
                       PF7   (0700-TRATAR-PF7)
                       PF8   (0800-TRATAR-PF8)
                       CLEAR (0200-TRATAR-PF3)
                       ENTER (1000-TRATAR-ENTER)
                       ANYKEY(1900-TRATAR-PF-INVALIDA)
           END-EXEC

           IF EIBRESP                  NOT EQUAL DFHRESP(NORMAL)
              PERFORM 9998-TRATAR-ERRO
           END-IF

           MOVE    EIBTRMID            TO WRK-TERMID
           MOVE   'VV9I'               TO WRK-TRANID

           IF (EIBCALEN                EQUAL ZEROS) OR
              (EIBTRNID                NOT EQUAL 'VV9I' AND 'VV9H' AND
                                                 'VV9F')
              PERFORM 0300-ENCERRAR-TRANSACAO
           END-IF

           MOVE    DFHCOMMAREA(1:EIBCALEN)
                                       TO WRK-COMMAREA

           IF EIBTRNID                 NOT EQUAL 'VV9I'
              PERFORM 0400-RECEBER-CONTROLE
           ELSE
              PERFORM 0600-RECEBER-MAPA
           END-IF.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0100-TRATAR-PF2                 SECTION.
      *----------------------------------------------------------------*

           MOVE    03                  TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE

           EXEC CICS
                ISSUE DISCONNECT
           END-EXEC.

           IF EIBRESP                  NOT EQUAL DFHRESP(NORMAL)
              PERFORM 9998-TRATAR-ERRO
           END-IF

           EXEC CICS
                RETURN
           END-EXEC.

      *----------------------------------------------------------------*
       0100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0200-TRATAR-PF3                 SECTION.
      *----------------------------------------------------------------*

           MOVE    04                  TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE

           EXEC CICS
                XCTL PROGRAM ('BVVE3235')
                     COMMAREA(WRK-COMMAREA)
           END-EXEC

           IF EIBRESP                  NOT EQUAL DFHRESP(NORMAL)
              PERFORM 9998-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0250-TRATAR-PF5                 SECTION.
      *----------------------------------------------------------------*

           MOVE   'BVVE'               TO WRK-SISTEMA-PAS
           MOVE   'P'                  TO WRK-NIVEL-PAS

           MOVE    05                  TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE

           EXEC CICS
                LINK PROGRAM ('SENH1005')
                     COMMAREA(WRK-AREA-SENH1005-PAS)
                     LENGTH  (LENGTH OF WRK-AREA-SENH1005-PAS)
           END-EXEC.

           IF EIBRESP                  NOT EQUAL DFHRESP(NORMAL)
              PERFORM 9998-TRATAR-ERRO
           END-IF

           EXEC CICS
                RETURN
           END-EXEC.

      *----------------------------------------------------------------*
       0250-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0300-ENCERRAR-TRANSACAO         SECTION.
      *----------------------------------------------------------------*

           EXEC CICS SEND
                FROM(WRK-MSG02)
                ERASE
           END-EXEC.

           EXEC CICS
                RETURN
           END-EXEC.

      *----------------------------------------------------------------*
       0300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0400-RECEBER-CONTROLE           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-COMM-PADRAO

           IF  EIBTRNID                EQUAL 'VV9F'
               PERFORM 0405-RECUPERAR-TELA
           ELSE
               PERFORM 0410-MONTAR-TELA
           END-IF.

           MOVE -1                     TO SELEC01L
           MOVE WRK-MSG03              TO MENSAGO

           PERFORM 0500-ENVIAR-MAPA.

      *----------------------------------------------------------------*
       0400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0405-RECUPERAR-TELA             SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-COMM-PAG-SEL       TO WRK-COMM-PAG
           MOVE WRK-COMM-ULT-PAG-SEL   TO WRK-COMM-ULT-PAG

           IF  WRK-COMM-AGENCIA        NOT EQUAL ZEROS
               MOVE 'Agencia...:'      TO LITAGENI
               MOVE WRK-COMM-AGENCIA   TO AGENCIAO
           END-IF.

           IF  WRK-COMM-DATA-DE        NOT EQUAL '01.01.0001'
               MOVE 'Periodo DE:'      TO LITDEI
               MOVE WRK-COMM-DATA-DE   TO DATADEI
           END-IF

           IF  WRK-COMM-DATA-ATE       NOT EQUAL '31.12.9999'
               MOVE 'ATE:'             TO LITATEI
               MOVE WRK-COMM-DATA-ATE  TO DATAATEI
           END-IF

           PERFORM 0470-LER-TS
           PERFORM 0480-MONTAR-OCORRENCIAS
           PERFORM 0510-DESPROT-SELECAO.

      *----------------------------------------------------------------*
       0405-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0410-MONTAR-TELA                SECTION.
      *----------------------------------------------------------------*

           MOVE  LOW-VALUES            TO BVVE324I
           MOVE ZEROS                  TO WRK-COMM-PAG
                                          WRK-COMM-ULT-PAG.

           IF  WRK-COMM-AGENCIA        NOT EQUAL ZEROS
               MOVE 'Agencia...:'      TO LITAGENI
               MOVE WRK-COMM-AGENCIA   TO AGENCIAO
           END-IF.

           IF  WRK-COMM-DATA-DE        NOT EQUAL '01.01.0001'
               MOVE 'Periodo DE:'      TO LITDEI
               MOVE WRK-COMM-DATA-DE   TO DATADEI
           END-IF

           IF  WRK-COMM-DATA-ATE       NOT EQUAL '31.12.9999'
               MOVE 'ATE:'             TO LITATEI
               MOVE WRK-COMM-DATA-ATE  TO DATAATEI
           END-IF

           PERFORM 0420-DELETAR-TS

           PERFORM 0440-LISTAR-SOLICITACOES.

      *----------------------------------------------------------------*
       0410-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0420-DELETAR-TS                 SECTION.
      *----------------------------------------------------------------*

           MOVE    06                  TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE

           EXEC CICS
                DELETEQ TS
                QUEUE  (WRK-NOME-TS)
                NOHANDLE
           END-EXEC.

           IF EIBRESP                  NOT EQUAL DFHRESP(NORMAL) AND
                                                 DFHRESP(QIDERR)
              PERFORM 9998-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0420-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0440-LISTAR-SOLICITACOES        SECTION.
      *----------------------------------------------------------------*

           PERFORM 0441-ABRIR-CSR01-BVVEB011

           PERFORM 0442-LER-CSR01-BVVEB011

           IF  SQLCODE                 EQUAL +100
               PERFORM 0443-FECHAR-CSR01-BVVEB011
           ELSE
               MOVE 1                  TO WRK-COMM-PAG
               PERFORM 0450-MONTAR-TS
               PERFORM 0443-FECHAR-CSR01-BVVEB011
               PERFORM 0470-LER-TS
               PERFORM 0480-MONTAR-OCORRENCIAS
               PERFORM 0510-DESPROT-SELECAO
           END-IF.

      *----------------------------------------------------------------*
       0440-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0441-ABRIR-CSR01-BVVEB011       SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-COMM-AGENCIA       TO CAG-BCRIA  OF BVVEB011
           MOVE WRK-COMM-DATA-DE       TO WRK-DATA-INI
           MOVE WRK-COMM-DATA-ATE      TO WRK-DATA-FIM

           EXEC SQL
               OPEN CSR01-BVVEB011
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE    10              TO WRK-LOCAL-SQLCODE
               MOVE    SQLCODE         TO WRK-SQLCODE
               PERFORM 9998-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0441-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0442-LER-CSR01-BVVEB011         SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                INTO :BVVEB011.CCNPJ-CPF-ESTBL,
                     :BVVEB011.CFLIAL-CNPJ-ESTBL,
                     :BVVEB011.CCTRL-CNPJ-CPF,
7C2511               :BVVEB011.CCNPJ-CPF-ESTBL-ST,
7C2511               :BVVEB011.CFLIAL-CNPJ-ESTBL-ST,
7C2511               :BVVEB011.CCTRL-CNPJ-CPF-ST,
                     :BVVEB011.IESTBL-AFLIA,
                     :BVVEB011.DRETOR-PPSTA-AFLIA
                       :WRK-DRETOR-PPSTA-NULL,
                     :BVVEB011.CAG-BCRIA,
                     :BVVEB011.CCTA-BCRIA-CLI,
                     :BVVEB011.CDIG-CCORR-ESTBL,
                     :BVVEB011.NESTBL-AFLIA
                        :WRK-NESTBL-AFLIA-NULL
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE    11              TO WRK-LOCAL-SQLCODE
               MOVE    SQLCODE         TO WRK-SQLCODE
               PERFORM 9998-TRATAR-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               GO TO 0442-99-FIM
           END-IF.

           IF  WRK-DRETOR-PPSTA-NULL   NOT EQUAL ZEROS
               MOVE ZEROS              TO DRETOR-PPSTA-AFLIA OF BVVEB011
           END-IF.

           IF  WRK-NESTBL-AFLIA-NULL   NOT EQUAL ZEROS
               MOVE ZEROS              TO NESTBL-AFLIA OF BVVEB011
           END-IF.

      *----------------------------------------------------------------*
       0442-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0443-FECHAR-CSR01-BVVEB011      SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR01-BVVEB011
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE    12              TO WRK-LOCAL-SQLCODE
               MOVE    SQLCODE         TO WRK-SQLCODE
               PERFORM 9998-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0443-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0450-MONTAR-TS                  SECTION.
      *----------------------------------------------------------------*

           PERFORM VARYING IND-1       FROM 1 BY 1
                     UNTIL SQLCODE     EQUAL +100

               IF  IND-1               GREATER 12
                   PERFORM 0460-GRAVAR-TS
                   MOVE 1              TO IND-1
               END-IF

7C2511*        MOVE CCNPJ-CPF-ESTBL   OF BVVEB011
7C2511*                                TO WRK-TS-CNPJ(IND-1)
7C2511         MOVE CCNPJ-CPF-ESTBL-ST OF BVVEB011
7C2511                                 TO WRK-TS-CNPJ(IND-1)

7C2511*        MOVE CFLIAL-CNPJ-ESTBL OF BVVEB011
7C2511*                                TO WRK-TS-FILIAL(IND-1)
7C2511         MOVE CFLIAL-CNPJ-ESTBL-ST OF BVVEB011
7C2511                                 TO WRK-TS-FILIAL(IND-1)

               MOVE CCTRL-CNPJ-CPF    OF BVVEB011
                                       TO WRK-TS-CTRL(IND-1)

               MOVE CAG-BCRIA         OF BVVEB011
                                       TO WRK-TS-AGENCIA(IND-1)

               MOVE CCTA-BCRIA-CLI    OF BVVEB011
                                       TO WRK-CONTA-AUX
               MOVE WRK-CONTA-N
                                       TO WRK-TS-CONTA(IND-1)

               MOVE CDIG-CCORR-ESTBL  OF BVVEB011
                                       TO WRK-TS-DIGITO(IND-1)

               MOVE IESTBL-AFLIA      OF BVVEB011
                                       TO WRK-TS-NOME(IND-1)

               MOVE DRETOR-PPSTA-AFLIA OF BVVEB011
                                       TO WRK-TS-DT-AFIL(IND-1)

               MOVE NESTBL-AFLIA      OF BVVEB011
                                       TO WRK-TS-ESTAB(IND-1)

               PERFORM 0442-LER-CSR01-BVVEB011

           END-PERFORM

           IF  IND-1                   NOT GREATER 13
               PERFORM 0460-GRAVAR-TS
           END-IF.

      *----------------------------------------------------------------*
       0450-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0460-GRAVAR-TS                  SECTION.
      *----------------------------------------------------------------*

           MOVE    13                  TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE

           ADD 1                       TO WRK-ITEM-TS
                                          WRK-COMM-ULT-PAG

           EXEC CICS
                WRITEQ TS QUEUE (WRK-NOME-TS)
                          FROM  (WRK-AREA-TS)
                          ITEM  (WRK-ITEM-TS)
           END-EXEC.

           IF  EIBRESP                 NOT EQUAL DFHRESP(NORMAL)
               PERFORM 9998-TRATAR-ERRO
           END-IF.

           INITIALIZE WRK-AREA-TS.

      *----------------------------------------------------------------*
       0460-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0470-LER-TS                     SECTION.
      *----------------------------------------------------------------*

           MOVE    14                  TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE

           MOVE WRK-COMM-PAG           TO WRK-ITEM-TS

           EXEC CICS
                READQ TS QUEUE (WRK-NOME-TS)
                         INTO  (WRK-AREA-TS)
                         ITEM  (WRK-ITEM-TS)
           END-EXEC.

           IF  EIBRESP                 NOT EQUAL DFHRESP(NORMAL)
               PERFORM 9998-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0470-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0480-MONTAR-OCORRENCIAS         SECTION.
      *----------------------------------------------------------------*

           PERFORM VARYING IND-1       FROM 1 BY 1
                     UNTIL IND-1       GREATER 12
            OR WRK-TS-NOME(IND-1)      EQUAL SPACES OR LOW-VALUES
               MOVE SPACES                 TO WRK-SELECI(IND-1)
7C2511*        MOVE WRK-TS-CNPJ(IND-1)     TO WRK-CNPJI-N(IND-1)
7C2511*        MOVE WRK-TS-FILIAL(IND-1)   TO WRK-FILIALI-N(IND-1)
               MOVE WRK-TS-CNPJ(IND-1)     TO WRK-CNPJI(IND-1)
               MOVE WRK-TS-FILIAL(IND-1)   TO WRK-FILIALI(IND-1)
               MOVE WRK-TS-CTRL(IND-1)     TO WRK-CTRLI-N(IND-1)
               MOVE WRK-TS-AGENCIA(IND-1)  TO WRK-AGENCIAI-N(IND-1)
               MOVE WRK-TS-CONTA(IND-1)    TO WRK-CONTAI-N(IND-1)
               MOVE WRK-TS-DIGITO(IND-1)   TO WRK-DIGI-N(IND-1)
               MOVE WRK-TS-NOME(IND-1)     TO WRK-NOMEI(IND-1)
               MOVE WRK-TS-DT-AFIL(IND-1)  TO WRK-DTAFILI(IND-1)
               MOVE WRK-TS-ESTAB(IND-1)    TO WRK-ESTABI-N(IND-1)
           END-PERFORM.

           IF  IND-1                   NOT GREATER 12
               PERFORM VARYING IND-1   FROM IND-1 BY 1
                         UNTIL IND-1   GREATER 12
                   MOVE SPACES         TO WRK-SELECI(IND-1)
                                          WRK-CNPJI(IND-1)
                                          WRK-FILIALI(IND-1)
                                          WRK-CTRLI(IND-1)
                                          WRK-AGENCIAI(IND-1)
                                          WRK-CONTAI(IND-1)
                                          WRK-DIGI(IND-1)
                                          WRK-NOMEI(IND-1)
                                          WRK-DTAFILI(IND-1)
                                          WRK-ESTABI(IND-1)
               END-PERFORM
           END-IF.

      *----------------------------------------------------------------*
       0480-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0500-ENVIAR-MAPA                SECTION.
      *----------------------------------------------------------------*

           EXEC CICS
                ASKTIME ABSTIME (WRK-ABSTIME)
           END-EXEC.

           EXEC CICS
                FORMATTIME ABSTIME (WRK-ABSTIME)
                           DATESEP ('/')
                           DDMMYY  (DTASISO)
                           TIMESEP (':')
                           TIME    (HORSISO)
           END-EXEC.

           MOVE    15                  TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE

           EXEC CICS
                SEND MAP   ('BVVE324')
                     MAPSET('BVVE324')
                     FROM  (BVVE324O)
                     CURSOR
                     ERASE
                     FREEKB
           END-EXEC.

           IF EIBRESP                  NOT EQUAL DFHRESP(NORMAL)
              MOVE    EIBRESP          TO WRK-EIBRESP
              EXEC CICS
                   SEND FROM(WRK-MSG99)
                        ERASE
              END-EXEC

              EXEC CICS
                   RETURN
              END-EXEC

           ELSE
              EXEC CICS
                   RETURN TRANSID ('VV9I')
                          COMMAREA(WRK-COMMAREA)
              END-EXEC
           END-IF.

      *----------------------------------------------------------------*
       0500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0510-DESPROT-SELECAO            SECTION.
      *----------------------------------------------------------------*

           MOVE   -1                   TO WRK-SELECL(1)

           PERFORM VARYING IND-1       FROM 1 BY 1
                   UNTIL   IND-1       GREATER 12
              OR WRK-NOMEI(IND-1)      EQUAL SPACES OR LOW-VALUES
               MOVE 'A'                TO WRK-SELECA(IND-1)
           END-PERFORM.

      *----------------------------------------------------------------*
       0510-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0600-RECEBER-MAPA               SECTION.
      *----------------------------------------------------------------*

           MOVE    16                  TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE

           EXEC CICS
                RECEIVE MAP   ('BVVE324')
                        MAPSET('BVVE324')
                        INTO  (BVVE324I)
           END-EXEC.

           IF EIBRESP                  NOT EQUAL DFHRESP(NORMAL)
              PERFORM 9998-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0700-TRATAR-PF7                 SECTION.
      *----------------------------------------------------------------*

           IF WRK-COMM-PAG             EQUAL 1
               MOVE    WRK-MSG09       TO MENSAGO

           ELSE
               SUBTRACT 1              FROM WRK-COMM-PAG
               PERFORM 0470-LER-TS
               PERFORM 0480-MONTAR-OCORRENCIAS

               MOVE   -1               TO WRK-SELECL(1)
               MOVE    WRK-MSG03       TO MENSAGO
           END-IF

           PERFORM 0510-DESPROT-SELECAO

           PERFORM 0500-ENVIAR-MAPA.

      *----------------------------------------------------------------*
       0700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0800-TRATAR-PF8                 SECTION.
      *----------------------------------------------------------------*

           IF WRK-COMM-PAG             EQUAL WRK-COMM-ULT-PAG
               MOVE WRK-MSG10          TO MENSAGO

           ELSE
               ADD     1               TO WRK-COMM-PAG
               PERFORM 0470-LER-TS
               PERFORM 0480-MONTAR-OCORRENCIAS

               MOVE   -1               TO WRK-SELECL(1)
               MOVE    WRK-MSG03       TO MENSAGO
           END-IF

           PERFORM 0510-DESPROT-SELECAO

           PERFORM 0500-ENVIAR-MAPA.

      *----------------------------------------------------------------*
       0800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-TRATAR-ENTER               SECTION.
      *----------------------------------------------------------------*

           IF  WRK-COMM-PAG            EQUAL ZEROS
               MOVE WRK-MSG05          TO MENSAGO
               PERFORM 0500-ENVIAR-MAPA
           END-IF

           PERFORM 1100-CONSISTIR-SELECAO

           PERFORM 1200-VERIFICAR-SELECAO

           PERFORM 1800-PASSAR-CONTROLE.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1100-CONSISTIR-SELECAO          SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO ACU-SEL

           PERFORM VARYING IND-1       FROM 1 BY 1
                     UNTIL IND-1       GREATER 12

             IF WRK-SELECI(IND-1)      EQUAL 'X'
                ADD     1              TO ACU-SEL
                MOVE   'A'             TO WRK-SELECA(IND-1)

             ELSE
               IF WRK-SELECI(IND-1)    NOT EQUAL SPACES AND LOW-VALUES
                  MOVE   -1            TO WRK-SELECL(IND-1)
                  MOVE   'I'           TO WRK-SELECA(IND-1)
                  MOVE   '*'           TO WRK-INCONSISTENTE

               ELSE
                   IF WRK-NOMEI(IND-1) NOT EQUAL SPACES AND LOW-VALUES
                      MOVE   'A'       TO WRK-SELECA(IND-1)
                   END-IF
               END-IF
             END-IF

           END-PERFORM.

           IF WRK-INCONSISTENTE        NOT EQUAL SPACES
              MOVE    WRK-MSG03        TO MENSAGO
              PERFORM 0500-ENVIAR-MAPA
           END-IF.

           IF ACU-SEL                  GREATER 1
              PERFORM VARYING IND-1    FROM 1 BY 1
                        UNTIL IND-1    GREATER 12 OR
                    WRK-NOMEI(IND-1)   EQUAL SPACES OR LOW-VALUES
                  MOVE -1              TO WRK-SELECL(IND-1)
                  MOVE 'I'             TO WRK-SELECA(IND-1)
              END-PERFORM
              MOVE WRK-MSG04           TO MENSAGO
              PERFORM 0500-ENVIAR-MAPA
           ELSE
              IF ACU-SEL               LESS 1
                 MOVE   -1             TO WRK-SELECL(1)
                 MOVE WRK-MSG03        TO MENSAGO
                 PERFORM 0500-ENVIAR-MAPA
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1200-VERIFICAR-SELECAO          SECTION.
      *----------------------------------------------------------------*

           PERFORM VARYING IND-1       FROM 1 BY 1
                     UNTIL IND-1       GREATER  12
                        OR WRK-ACHOU   EQUAL 'S'

               IF  WRK-SELECI(IND-1)   EQUAL 'X'
                   MOVE 'S'                  TO WRK-ACHOU
7C2511*        MOVE WRK-CNPJI-N(IND-1)   TO WRK-COMM-3240-CNPJ
7C2511*        MOVE WRK-FILIALI-N(IND-1) TO WRK-COMM-3240-FILIAL
                   MOVE WRK-CNPJI(IND-1)     TO WRK-COMM-3240-CNPJ
                   MOVE WRK-FILIALI(IND-1)   TO WRK-COMM-3240-FILIAL
                   MOVE WRK-CTRLI-N(IND-1)   TO WRK-COMM-3240-CTRL
               END-IF

           END-PERFORM.


           IF  WRK-ACHOU               NOT EQUAL 'S'
               MOVE   -1               TO WRK-SELECL(1)
               MOVE WRK-MSG03          TO MENSAGO
               PERFORM 0500-ENVIAR-MAPA
           END-IF.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1800-PASSAR-CONTROLE            SECTION.
      *----------------------------------------------------------------*

           MOVE    18                  TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE

           MOVE WRK-COMM-PAG           TO WRK-COMM-PAG-SEL
           MOVE WRK-COMM-ULT-PAG       TO WRK-COMM-ULT-PAG-SEL

           MOVE   'VV9I'               TO WRK-COMM-TRANSACAO

           EXEC CICS
                XCTL PROGRAM ('BVVE3225')
                     COMMAREA(WRK-COMMAREA)
           END-EXEC

           IF EIBRESP                  NOT EQUAL DFHRESP(NORMAL)
              PERFORM 9998-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1900-TRATAR-PF-INVALIDA         SECTION.
      *----------------------------------------------------------------*

           MOVE -1                     TO SELEC01L

           IF WRK-COMM-PAG             NOT EQUAL ZEROS
               PERFORM 0510-DESPROT-SELECAO
           END-IF

           MOVE    WRK-MSG01           TO MENSAGO
           PERFORM 0500-ENVIAR-MAPA.

      *----------------------------------------------------------------*
       1900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9998-TRATAR-ERRO                SECTION.
      *----------------------------------------------------------------*

           IF EIBRESP                  NOT EQUAL ZEROS
              MOVE    EIBRESP          TO WRK-EIBRESP
              MOVE    WRK-MSG99        TO MENSAGO
                                          WRK-MSG03
           ELSE
              IF SQLCODE               NOT EQUAL ZEROS
                 MOVE    WRK-MSG98     TO MENSAGO
                                          WRK-MSG03
              ELSE
                 PERFORM 9999-ERRO-ABCODE
                 MOVE    WRK-MSG00     TO MENSAGO
                                          WRK-MSG03
              END-IF
           END-IF.

           PERFORM 0500-ENVIAR-MAPA.

      *----------------------------------------------------------------*
       9998-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999-ERRO-ABCODE                SECTION.
      *----------------------------------------------------------------*

           EXEC CICS
                ASSIGN ABCODE(WRK-ABCODE-MSG)
           END-EXEC.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
