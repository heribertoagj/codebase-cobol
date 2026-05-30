      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. BVVE4005.
       AUTHOR.     FRANCISMARA.
      *================================================================*
      *                  B S I - T E C N O L O G I A                   *
      *----------------------------------------------------------------*
      *    PROGRAMA.....: BVVE4005                                     *
      *    PROGRAMADORA.: FRANCISMARA          - BSI                   *
      *    DATA.........: JUNHO/2012                                   *
      *----------------------------------------------------------------*
      *    OBJETIVO     :                                              *
      *      CONSULTA MANUTENCAO DE DOMICILIO BANCARIO (TRAVA)- CIP    *
      *----------------------------------------------------------------*
      *    MAPA/MAPSET  :                                              *
      *      BVVE405 - CONSULTA MANUTENCAO DOMICILIO BANCARIO - CIP    *
      *----------------------------------------------------------------*
      *    TRANSACAO    :                                              *
      *      VV4E                                                      *
      *----------------------------------------------------------------*
      *    NAVEGACAO    :                                              *
      *      CHAMADO POR: BVVE4001              CHAMA: BVVE4040        *
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
           '*  INICIO DA WORKING BVVE4005  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)        VALUE
           '*         INDEXADORES          *'.
      *----------------------------------------------------------------*
       01  WRK-INDEXADORES.
           03  WRK-IND                 PIC  9(03) COMP-3  VALUE ZEROS.
           03  WRK-IND-AUX             PIC  9(03) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)        VALUE
           '*     VARIAVEIS AUXILIARES     *'.
      *----------------------------------------------------------------*
       01  WRK-AUXILIARES.
           03  WRK-ABSTIME             PIC S9(15) COMP-3 VALUE ZEROS.
           03  WRK-LENGTH              PIC S9(04) COMP   VALUE ZEROS.
           03  WRK-PROGRAMA            PIC  X(08)        VALUE SPACES.
           03  WRK-INCONSISTENTE       PIC  X(01)        VALUE SPACES.
           03  ACU-PREENCHIDOS         PIC  9(02) COMP-3 VALUE ZEROS.

       01  WRK-EIBRESP-AUX             PIC  9(04)        VALUE ZEROS.

7C2511* 01  WRK-FORMAT-CGCCPF.
7C2511*     03  WRK-NROCNPJ             PIC  9(008)       VALUE ZEROS.
7C2511*     03  WRK-FILIAL              PIC  9(004)       VALUE ZEROS.
7C2511*     03  WRK-CTRL-CNPJ           PIC  9(002)       VALUE ZEROS.
7C2511 01  WRK-FORMAT-CGCCPF.
7C2511     03  WRK-NROCNPJ             PIC  X(08)        VALUE SPACES.
7C2511     03  WRK-FILIAL              PIC  X(04)        VALUE SPACES.
7C2511     03  WRK-CTRL-CNPJ           PIC  9(02)        VALUE ZEROS.
7C2511* 01  FILLER REDEFINES WRK-FORMAT-CGCCPF.
7C2511*     03  WRK-ZEROS               PIC  9(003).
7C2511*     03  WRK-CPF                 PIC  9(009).
7C2511*     03  WRK-CTRL-CPF            PIC  9(002).
7C2511 01  FILLER REDEFINES WRK-FORMAT-CGCCPF.
7C2511     03  WRK-ZEROS               PIC  X(03).
7C2511     03  WRK-CPF                 PIC  X(09).
7C2511     03  WRK-CTRL-CPF            PIC  9(02).
7C2511* 01  WRK-FORMAT-CGCCPF-R         REDEFINES
7C2511*     WRK-FORMAT-CGCCPF           PIC  9(014).
7C2511 01  WRK-FORMAT-CGCCPF-R         REDEFINES
7C2511     WRK-FORMAT-CGCCPF           PIC  X(14).

       01  WRK-DATA-10                 PIC  X(10)        VALUE SPACES.
       01  FILLER REDEFINES WRK-DATA-10.
           03  WRK-DATA-8              PIC  9(08).
           03  FILLER                  PIC  X(02).

       01  WRK-TIME-10                 PIC  X(10)        VALUE SPACES.
       01  FILLER REDEFINES WRK-TIME-10.
           03  WRK-TIME-2              PIC  9(02).
           03  FILLER                  PIC  9(08).

       01  WRK-X-8.
           03  WRK-9-8                 PIC  9(08)        VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*     AREA DA POOL0110     *'.
      *----------------------------------------------------------------*

       01  WRK-CONTROLE                PIC  X(02)          VALUE SPACES.
       01  WRK-CONTROLE-N              REDEFINES WRK-CONTROLE
                                       PIC  9(02).


7C2511* 01  WRK-CNPJ-CPF.
7C2511*     03  WRK-CNPJ-CPF-NUM        PIC  9(09)          VALUE ZEROS.
7C2511*     03  WRK-CNPJ-CPF-FILIAL     PIC  9(05)          VALUE ZEROS.
7C2511 01  WRK-CNPJ-CPF.
7C2511     03  WRK-CNPJ-CPF-NUM        PIC  X(09)          VALUE SPACES.
7C2511     03  WRK-CNPJ-CPF-FILIAL     PIC  X(04)          VALUE SPACES.
7C2511     03  FILLER                  PIC  X(01)          VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*      AREA DE MENSAGENS       *'.
      *----------------------------------------------------------------*

       01  WRK-MENSAGENS.
           03  WRK-MSG01               PIC  X(79)        VALUE
               'Pf invalida'.

           03  WRK-MSG02               PIC  X(79)        VALUE
               'Digite os dados desejados, selecione uma Bandeira e tecl
      -        'e <ENTER>'.

           03  WRK-MSG05               PIC  X(79)        VALUE
               'Campo invalido'.

           03  WRK-MSG03               PIC  X(79)        VALUE
               'Acesso permitido somente pelas transacoes VV4A, VV4E e V
      -        'V4N'.

           03  WRK-MSG06               PIC  X(79)        VALUE
               'Informe uma das chaves: CNPJ ou CPF'.

           03  WRK-MSG07               PIC X(79) VALUE
               'CNPJ/CPF invalido'.

           03  WRK-MSG08               PIC X(79) VALUE
               'Tecle <PF6> para confirmar'.

           03  WRK-MSG09               PIC  X(79)        VALUE
               'Campo(S) obrigatorio(s)'.

           03  WRK-MSG10               PIC  X(79)        VALUE
               'Solicitacao de consulta efetuada. Tecle <PF10> para moni
      -        'torar a sua efetivacao'.

           03  WRK-MSG12               PIC  X(79)        VALUE
               'Informe apenas uma das chaves: CPF ou CNPJ'.

           03  WRK-MSG14               PIC  X(79)        VALUE
               'Informe se deseja consultar todas as filiais do CNPJ'.

           03  WRK-MSG19               PIC  X(79)        VALUE
               'Solicitacao nao realizada. Fora da grade horaria (08:00
      -        'as 18:00 horas).'.

           03  WRK-MSG17               PIC  X(79)        VALUE
               'Selecione uma Bandeira'.

           03  WRK-MSG18               PIC  X(79)        VALUE
               'Selecione apenas uma Bandeira com ''X'' e tecle <ENTER>
      -        ' '.

           03  WRK-MSG99.
               05  FILLER              PIC  X(35)        VALUE
                   'ERRO NAO IDENTIF PELO PGM. EIBRESP='.
               05  WRK-EIBRESP-MSG     PIC  9(04)        VALUE ZEROS.
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

           03  WRK-MSG97.
               05  FILLER              PIC  X(12)        VALUE
                  'ERRO MODULO '.
               05  WRK-MODULO          PIC  X(08)        VALUE SPACES.
               05  WRK-TEXTO           PIC  X(30)        VALUE SPACES.
               05  FILLER              PIC  X(20)        VALUE
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
           03  WRK-COMM-PADRAO.
               05  WRK-COMM-TRANSACAO  PIC  X(04)        VALUE SPACES.
               05  WRK-COMM-FASE       PIC  X(01)        VALUE SPACES.
           03  WRK-COMM-PGM.
               05  WRK-COMM-DEPTO      PIC  9(04)        VALUE ZEROS.
               05  WRK-COMM-RAIZ       PIC  X(001)       VALUE SPACES.
               05  WRK-COMM-BAN-SELEC  PIC  9(03)      VALUE ZEROS.
               05  WRK-COMM-CCTRL-IF   PIC  X(23)      VALUE SPACES.
               05  WRK-COMM-BAN        PIC  9(03) OCCURS 6
                                                         VALUE ZEROS.


      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*      COMMAREA AUXILIAR 4025  *'.
      *----------------------------------------------------------------*

       01  WRK-COMMAREA-4025.
           03 WRK-COMM4025-CCTRL-IF    PIC X(023)          VALUE SPACES.
           03 WRK-COMM4025-TP-MANUT    PIC X(001)          VALUE SPACES.
           03 WRK-COMM4025-CHAMADOR    PIC X(008)          VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*     AREA DA SCG0005 *'.
      *----------------------------------------------------------------*

       COPY 'I#BVVEDA'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*     AREA DO ARQVUIO TRAVPBAN *'.
      *----------------------------------------------------------------*

       COPY 'I#BVVED1'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*     AREA DO MODULO BVVE2450 *'.
      *----------------------------------------------------------------*

       COPY 'I#BVVEDM'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*     AREA DO MAPA BVVE405     *'.
      *----------------------------------------------------------------*

       COPY 'BVVE405'.

       01  WRK-BVVE405I                REDEFINES BVVE405I.
           03  WRK-FIXO                PIC X(078).
           03  FILLER                  OCCURS 6.
               05  WRK-SELL            PIC S9(04) COMP.
               05  WRK-SELA            PIC  X(01)     .
               05  WRK-SELI            PIC  X(01)     .
               05  WRK-BANDL           PIC S9(04) COMP.
               05  WRK-BANDA           PIC  X(01)     .
               05  WRK-BANDI           PIC  X(20)     .

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*   FIM DA WORKING BVVE4005   *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  DFHCOMMAREA.
           03  FILLER                  PIC  X(054).

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
           END-EXEC.

           IF EIBRESP                  NOT EQUAL DFHRESP(NORMAL)
              PERFORM 9998-TRATAR-ERRO
           END-IF

           INITIALIZE WRK-INDEXADORES
                      WRK-AUXILIARES

           MOVE    02                  TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE

           EXEC CICS
                HANDLE AID
                       PF2   (1100-TRATAR-PF2)
                       PF3   (1200-TRATAR-PF3)
                       PF5   (1300-TRATAR-PF5)
                       PF6   (1400-TRATAR-PF6)
                       PF9   (1900-RECEBER-CONTROLE)
                       PF10  (1600-TRATAR-PF10)
                       CLEAR (9000-ENCERRAR-PGM)
                       ENTER (1700-TRATAR-ENTER)
                       ANYKEY(1800-TRATAR-PF-INVALIDA)

           END-EXEC.

           IF EIBRESP                  NOT EQUAL DFHRESP(NORMAL)
              PERFORM 9998-TRATAR-ERRO
           END-IF

           IF EIBCALEN                 EQUAL ZEROS OR
              EIBTRNID                 NOT EQUAL 'VV4A' AND 'VV4E' AND
                                                 'VV4N'
              MOVE    WRK-MSG03        TO MENSAGO
              PERFORM 9000-ENCERRAR-PGM
           END-IF

           MOVE DFHCOMMAREA(1:EIBCALEN)
                                       TO WRK-COMMAREA

           IF EIBTRNID                 NOT EQUAL 'VV4E'
              PERFORM 1900-RECEBER-CONTROLE
           ELSE
              PERFORM 2000-RECEBER-MAPA
           END-IF.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1100-TRATAR-PF2                 SECTION.
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
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1200-TRATAR-PF3                 SECTION.
      *----------------------------------------------------------------*

           MOVE    04                  TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE

           EXEC CICS
                XCTL PROGRAM ('BVVE4001')
                     COMMAREA(WRK-COMMAREA)
           END-EXEC

           IF EIBRESP                  NOT EQUAL DFHRESP(NORMAL)
              PERFORM 9998-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1300-TRATAR-PF5                 SECTION.
      *----------------------------------------------------------------*

           MOVE   'BVVE'               TO WRK-SISTEMA-PAS
           MOVE   'P'                  TO WRK-NIVEL-PAS

           MOVE    05                  TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE

           EXEC CICS
                LINK PROGRAM ('SENH1005')
                     COMMAREA(WRK-AREA-SENH1005-PAS)
                     LENGTH  (+255)
           END-EXEC.

           IF EIBRESP                  NOT EQUAL DFHRESP(NORMAL)
              PERFORM 9998-TRATAR-ERRO
           END-IF

           EXEC CICS
                RETURN
           END-EXEC.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1400-TRATAR-PF6                 SECTION.
      *----------------------------------------------------------------*

           IF  WRK-COMM-FASE           NOT EQUAL '2'
               MOVE WRK-MSG01          TO MENSAGO
               PERFORM 1950-ENVIAR-MAPA
           END-IF.

           EXEC CICS
                ASKTIME ABSTIME (WRK-ABSTIME)
           END-EXEC.

           EXEC CICS
                FORMATTIME ABSTIME  (WRK-ABSTIME)
                           TIME     (WRK-TIME-10)
                           YYYYMMDD (WRK-DATA-10)
           END-EXEC.

           IF  WRK-TIME-2              LESS 08 OR GREATER 17
               MOVE WRK-MSG19          TO MENSAGO
               PERFORM 1950-ENVIAR-MAPA
           END-IF.

           PERFORM 1410-CHAMAR-BVVE2450.

           MOVE LOW-VALUES             TO WRK-FIXO
                                          WRK-SELI(1) WRK-SELI(2)
                                          WRK-SELI(3) WRK-SELI(4)
                                          WRK-SELI(5) WRK-SELI(6)

           MOVE '1'                    TO WRK-COMM-FASE.
           MOVE WRK-MSG10              TO MENSAGO.

           PERFORM 1950-ENVIAR-MAPA.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1410-CHAMAR-BVVE2450            SECTION.
      *----------------------------------------------------------------*

           INITIALIZE BVVEDA-SCG0005
           INITIALIZE WRK-COMMAREA-2450

           EXEC CICS
                ASKTIME ABSTIME (WRK-ABSTIME)
           END-EXEC.

           EXEC CICS
                FORMATTIME ABSTIME  (WRK-ABSTIME)
                           YYYYMMDD (WRK-DATA-10)
           END-EXEC.

           PERFORM 1420-OBTER-COD-PROP
           MOVE 'SCG0005'              TO BVVEDA-COD-MSG
           MOVE ALL '*'                TO BVVEDA-NUM-IDENTC-SCG-X

7C2511*     IF  NROCNPJO            NUMERIC
7C2511     IF NROCNPJO NOT EQUAL SPACES AND NOT EQUAL LOW-VALUES
               IF  SIMNAOO         EQUAL 'S'
7C2511*             MOVE NROCNPJO   TO WRK-9-8
7C2511*                                WRK-2450-CNPJ
7C2511             MOVE NROCNPJO   TO WRK-9-8
7C2511                                WRK-2450-CNPJ-ST
                   MOVE WRK-X-8    TO BVVEDA-CNPJ-BASE-CLI
                   MOVE ALL '*'    TO BVVEDA-DADOS-CLI
                   MOVE 'S'        TO WRK-2450-RAIZ
               ELSE
                   MOVE 'N'        TO WRK-2450-RAIZ
                   MOVE ALL '*'    TO BVVEDA-CNPJ-BASE-CLI
7C2511*             MOVE NROCNPJO   TO WRK-NROCNPJ
7C2511*                                WRK-2450-CNPJ
7C2511             MOVE NROCNPJO   TO WRK-NROCNPJ
7C2511                                WRK-2450-CNPJ-ST
7C2511*             MOVE FILIALO    TO WRK-FILIAL
7C2511*                                WRK-2450-FILIAL
7C2511             MOVE FILIALO    TO WRK-FILIAL
7C2511                                WRK-2450-FILIAL-ST
                   MOVE CTRLCGCO   TO WRK-CTRL-CNPJ
                   MOVE 'J'        TO BVVEDA-TP-PESSOA-CLI
                   MOVE WRK-FORMAT-CGCCPF-R TO BVVEDA-CNPJ-CPF-CLI
               END-IF
           ELSE
               MOVE 'N'            TO WRK-2450-RAIZ
               MOVE ALL '*'        TO BVVEDA-CNPJ-BASE-CLI
7C2511*         MOVE NROCPFO        TO WRK-CPF
7C2511*                                WRK-2450-CNPJ
7C2511         MOVE NROCPFO        TO WRK-CPF
7C2511                                WRK-2450-CNPJ-ST
               MOVE CTRLCPFO       TO WRK-CTRL-CPF
7C2511*         MOVE ZEROS          TO WRK-ZEROS
7C2511*                                WRK-2450-FILIAL
7C2511         MOVE SPACES         TO WRK-ZEROS
7C2511                                WRK-2450-FILIAL-ST
               MOVE 'F'            TO BVVEDA-TP-PESSOA-CLI
               MOVE WRK-FORMAT-CGCCPF-R TO BVVEDA-CNPJ-CPF-CLI
           END-IF.

           MOVE WRK-DATA-8             TO BVVEDA-DT-MOVTO
           MOVE 'M'                    TO BVVEDA-TP-RET

           MOVE  WRK-COMM-BAN-SELEC    TO WRK-2450-BANDEIRA
           MOVE  89                    TO WRK-2450-TAM-DADOS
           MOVE  BVVEDA-SCG0005        TO WRK-2450-DADOS-MQ.
           MOVE 'SCG0005'              TO WRK-2450-COD-MSG.

           EXEC CICS
                LINK PROGRAM ('BVVE2450')
                     COMMAREA(WRK-COMMAREA-2450)
                     LENGTH  (LENGTH OF WRK-COMMAREA-2450 )
           END-EXEC.

           IF EIBRESP                  NOT EQUAL DFHRESP(NORMAL)
              PERFORM 9998-TRATAR-ERRO
           END-IF

           IF  WRK-2450-COD-RETORNO         NOT EQUAL ZEROS
               MOVE WRK-2450-ERR-MODULO     TO WRK-MODULO
               EVALUATE WRK-2450-COD-RETORNO
                  WHEN 96
                       MOVE WRK-2450-ERR-TEXTO TO WRK-TEXTO
                  WHEN 97
                       STRING 'DB2'
                              WRK-2450-ERR-DBD-TAB
                              WRK-2450-ERR-FUN-COMANDO
                              WRK-2450-ERR-SQL-CODE
                              WRK-2450-ERR-LOCAL
                       DELIMITED BY ' '
                       INTO WRK-TEXTO
                  WHEN 95
                       STRING 'ERRO CICS, EIBRESP='
                               WRK-2450-EIBRESP
                       DELIMITED BY SIZE
                       INTO       WRK-TEXTO
               END-EVALUATE
               MOVE  WRK-MSG97          TO MENSAGO
               MOVE   '0'               TO WRK-COMM-FASE
               PERFORM 1950-ENVIAR-MAPA
           END-IF.

           MOVE WRK-2450-NRO-CTRL-IF   TO WRK-COMM-CCTRL-IF.

      *----------------------------------------------------------------*
       1410-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1420-OBTER-COD-PROP             SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO BVVED1-CHAVE
           MOVE WRK-COMM-BAN-SELEC     TO BVVED1-COD-BAND
           MOVE    10                  TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE

           EXEC CICS
                READ     FILE  ('TRAVPBAN')
                         INTO  (BVVED1-TRAVPBAN)
                         RIDFLD(BVVED1-CHAVE)
                         NOHANDLE
           END-EXEC.

           IF  EIBRESP                 NOT EQUAL DFHRESP (NORMAL)
               MOVE EIBRESP            TO WRK-EIBRESP-AUX
               MOVE WRK-EIBRESP-AUX    TO EIBRESP
               PERFORM 9998-TRATAR-ERRO
           END-IF.
           MOVE  BVVED1-COD-PROP-ESQ-X TO BVVEDA-COD-PROPT-ESQM.

      *----------------------------------------------------------------*
       1420-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1600-TRATAR-PF10                SECTION.
      *----------------------------------------------------------------*

           IF  WRK-COMM-CCTRL-IF       EQUAL SPACES OR LOW-VALUES
               MOVE WRK-MSG01          TO MENSAGO
               PERFORM 1950-ENVIAR-MAPA
           END-IF.

           MOVE 'C'                    TO WRK-COMM4025-TP-MANUT
           MOVE 'BVVE4005'             TO WRK-COMM4025-CHAMADOR
           MOVE WRK-COMM-CCTRL-IF      TO WRK-COMM4025-CCTRL-IF

           EXEC CICS
                XCTL PROGRAM ('BVVE4025')
                     COMMAREA(WRK-COMMAREA-4025)
           END-EXEC.

           MOVE    08                  TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE

           IF EIBRESP                  NOT EQUAL DFHRESP(NORMAL)
              PERFORM 9998-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1700-TRATAR-ENTER               SECTION.
      *----------------------------------------------------------------*

           PERFORM 1710-CONSIST-FISICA.

           PERFORM 1750-CONSIST-PREENCH.

           IF  NROCNPJI                NOT EQUAL SPACES AND LOW-VALUES
               PERFORM 1720-CONSIST-CNPJ
           ELSE
               IF  NROCPFI             NOT EQUAL SPACES AND LOW-VALUES
                   PERFORM 1730-CONSIST-CPF
               END-IF
           END-IF.

           PERFORM VARYING WRK-IND     FROM 1 BY 1
                   UNTIL   WRK-IND     GREATER 6 OR
                   WRK-BANDI(WRK-IND)  EQUAL SPACES
                   IF  WRK-SELI(WRK-IND) NOT EQUAL SPACES AND LOW-VALUES
                       ADD 1             TO ACU-PREENCHIDOS
                       IF  WRK-SELI(WRK-IND)  NOT EQUAL 'X'
                           MOVE '*'           TO WRK-INCONSISTENTE
                       ELSE
                           MOVE WRK-IND       TO WRK-IND-AUX
                       END-IF
                   END-IF
           END-PERFORM.

           IF  ACU-PREENCHIDOS         EQUAL ZEROS
               MOVE -1                 TO WRK-SELL(1)
               MOVE '*'                TO WRK-INCONSISTENTE
               MOVE    WRK-MSG17       TO MENSAGO
               PERFORM 1950-ENVIAR-MAPA
           ELSE
               IF  ACU-PREENCHIDOS      GREATER  1 OR
                   WRK-INCONSISTENTE    EQUAL '*'
                   MOVE    WRK-MSG18    TO MENSAGO
                   MOVE '*'             TO WRK-INCONSISTENTE
                   PERFORM VARYING WRK-IND     FROM 1 BY 1
                           UNTIL   WRK-IND     GREATER 6 OR
                           WRK-BANDI(WRK-IND)  EQUAL SPACES
                           IF  WRK-SELI(WRK-IND) NOT EQUAL SPACES
                                   AND LOW-VALUES
                               MOVE -1           TO WRK-SELL(WRK-IND)
                               MOVE 'I'          TO WRK-SELA(WRK-IND)
                           END-IF
                   END-PERFORM
                   PERFORM 1950-ENVIAR-MAPA
               ELSE
                   PERFORM 1760-ENVIAR-FASE-2
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1710-CONSIST-FISICA             SECTION.
      *----------------------------------------------------------------*

           IF  (NROCPFI                NOT EQUAL SPACES AND LOW-VALUES)
                IF NROCPFO             NOT NUMERIC OR
                   NROCPFO             EQUAL ZEROS
                   MOVE  -1            TO NROCPFL
                   MOVE  'I'           TO NROCPFA
                   MOVE  '*'           TO WRK-INCONSISTENTE
               END-IF
           END-IF.

           IF  (CTRLCPFI               NOT EQUAL SPACES AND LOW-VALUES)
                IF CTRLCPFO            NOT NUMERIC
                   MOVE  -1            TO CTRLCPFL
                   MOVE  'I'           TO CTRLCPFA
                   MOVE  '*'           TO WRK-INCONSISTENTE
               END-IF
           END-IF.
           IF  (NROCNPJI               NOT EQUAL SPACES AND LOW-VALUES)
                IF NROCNPJO            NOT NUMERIC OR
                   NROCNPJO            EQUAL ZEROS
                   MOVE  -1            TO NROCNPJL
                   MOVE  'I'           TO NROCNPJA
                   MOVE  '*'           TO WRK-INCONSISTENTE
               END-IF
           END-IF.

           IF  (FILIALI                NOT EQUAL SPACES AND LOW-VALUES)
                IF FILIALO             NOT NUMERIC OR
                   FILIALO             EQUAL ZEROS
                   MOVE  -1            TO FILIALL
                   MOVE  'I'           TO FILIALA
                   MOVE  '*'           TO WRK-INCONSISTENTE
               END-IF
           END-IF.

           IF  (CTRLCGCI               NOT EQUAL SPACES AND LOW-VALUES)
                IF CTRLCGCO            NOT NUMERIC
                   MOVE  -1            TO CTRLCGCL
                   MOVE  'I'           TO CTRLCGCA
                   MOVE  '*'           TO WRK-INCONSISTENTE
               END-IF
           END-IF.

           IF  (SIMNAOO                NOT EQUAL SPACES AND LOW-VALUES)
                IF SIMNAOO             NOT EQUAL 'S' AND 'N'
                   MOVE  -1            TO SIMNAOL
                   MOVE  'I'           TO SIMNAOA
                   MOVE  '*'           TO WRK-INCONSISTENTE
               END-IF
           END-IF.

           IF WRK-INCONSISTENTE        EQUAL '*'
              MOVE    WRK-MSG05        TO MENSAGO
              PERFORM 1950-ENVIAR-MAPA
           END-IF.

      *----------------------------------------------------------------*
       1710-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1720-CONSIST-CNPJ               SECTION.
      *----------------------------------------------------------------*

           MOVE NROCNPJO               TO WRK-CNPJ-CPF-NUM
           MOVE FILIALO                TO WRK-CNPJ-CPF-FILIAL

           CALL 'POOL0112'             USING WRK-CNPJ-CPF
                                             WRK-CONTROLE.


           IF  WRK-CONTROLE-N       NOT EQUAL CTRLCGCO
               MOVE    WRK-MSG07    TO MENSAGO
               MOVE  -1             TO NROCNPJL
               MOVE  'I'            TO NROCNPJA
                                       FILIALA
                                       CTRLCGCA
               MOVE  '*'           TO WRK-INCONSISTENTE
               PERFORM 1950-ENVIAR-MAPA
           END-IF.

      *----------------------------------------------------------------*
       1720-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1730-CONSIST-CPF                SECTION.
      *----------------------------------------------------------------*

           MOVE NROCPFO                TO WRK-CNPJ-CPF-NUM
           MOVE ZEROS                  TO WRK-CNPJ-CPF-FILIAL

           CALL 'POOL0112'             USING WRK-CNPJ-CPF
                                             WRK-CONTROLE.

           IF  WRK-CONTROLE-N       NOT EQUAL CTRLCPFO
               MOVE    WRK-MSG07    TO MENSAGO
               MOVE  -1             TO NROCPFL
               MOVE  'I'            TO NROCPFA
                                       CTRLCPFA
               MOVE  '*'           TO WRK-INCONSISTENTE
               PERFORM 1950-ENVIAR-MAPA
           END-IF.

      *----------------------------------------------------------------*
       1730-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1750-CONSIST-PREENCH            SECTION.
      *----------------------------------------------------------------*

           IF (NROCNPJI          EQUAL SPACES OR  LOW-VALUES) AND
              (NROCPFI            EQUAL SPACES OR  LOW-VALUES)
               MOVE '*'                TO WRK-INCONSISTENTE
               MOVE -1                 TO NROCPFL
               MOVE    WRK-MSG06       TO MENSAGO
               PERFORM 1950-ENVIAR-MAPA
           END-IF.

           IF (NROCNPJI          NOT EQUAL SPACES AND LOW-VALUES) OR
              (FILIALI           NOT EQUAL SPACES AND LOW-VALUES) OR
              (CTRLCGCI          NOT EQUAL SPACES AND LOW-VALUES)
               IF (NROCPFI       NOT EQUAL SPACES AND LOW-VALUES) OR
                  (CTRLCPFI      NOT EQUAL SPACES AND LOW-VALUES)
                   MOVE '*'                TO WRK-INCONSISTENTE
                   MOVE -1                 TO NROCPFL
                   MOVE    WRK-MSG12        TO MENSAGO
                   PERFORM 1950-ENVIAR-MAPA
               END-IF
           END-IF

           IF  (NROCPFI            NOT EQUAL SPACES AND LOW-VALUES) AND
               (CTRLCPFI           EQUAL SPACES OR LOW-VALUES)
               MOVE '*'                TO WRK-INCONSISTENTE
               MOVE -1                 TO CTRLCPFL
               MOVE 'I'                TO CTRLCPFA
           END-IF.

           IF  (NROCNPJI          NOT EQUAL SPACES AND LOW-VALUES) AND
               (FILIALI           EQUAL SPACES OR LOW-VALUES)
               MOVE '*'                TO WRK-INCONSISTENTE
               MOVE -1                 TO FILIALL
               MOVE 'I'                TO FILIALA
           END-IF.

           IF  (NROCNPJI          NOT EQUAL SPACES AND LOW-VALUES) AND
               (CTRLCGCI          EQUAL SPACES OR LOW-VALUES)
               MOVE '*'                TO WRK-INCONSISTENTE
               MOVE -1                 TO CTRLCGCL
               MOVE 'I'                TO CTRLCGCA
           END-IF.

           IF  WRK-INCONSISTENTE       EQUAL '*'
               MOVE    WRK-MSG09       TO MENSAGO
               PERFORM 1950-ENVIAR-MAPA
           END-IF.

           IF  (NROCNPJI          NOT EQUAL SPACES AND LOW-VALUES) AND
               (SIMNAOO           EQUAL SPACES OR LOW-VALUES)
               MOVE '*'                TO WRK-INCONSISTENTE
               MOVE -1                 TO SIMNAOL
               MOVE 'I'                TO SIMNAOA
               MOVE    WRK-MSG14       TO MENSAGO
               PERFORM 1950-ENVIAR-MAPA
           END-IF.

      *----------------------------------------------------------------*
       1750-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1760-ENVIAR-FASE-2              SECTION.
      *----------------------------------------------------------------*

           MOVE '2'                       TO WRK-COMM-FASE.
           MOVE WRK-COMM-BAN(WRK-IND-AUX) TO WRK-COMM-BAN-SELEC

           MOVE    WRK-MSG08              TO MENSAGO

           PERFORM 1950-ENVIAR-MAPA.

      *----------------------------------------------------------------*
       1760-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1800-TRATAR-PF-INVALIDA         SECTION.
      *----------------------------------------------------------------*

           MOVE    WRK-MSG01           TO MENSAGO.

           PERFORM 1950-ENVIAR-MAPA.

      *----------------------------------------------------------------*
       1800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1900-RECEBER-CONTROLE           SECTION.
      *----------------------------------------------------------------*

           MOVE    LOW-VALUES          TO BVVE405I
           MOVE   '1'                  TO WRK-COMM-FASE
           MOVE    WRK-MSG02           TO MENSAGO

           PERFORM 1910-POSICIONAR-TRAVPBAN.
           PERFORM 1920-LER-TRAVPBAN.

           PERFORM 1940-MONTAR-BANDEIRAS
                   UNTIL WRK-IND       EQUAL 6 OR
                         EIBRESP       NOT EQUAL DFHRESP (NORMAL).

           PERFORM 1930-EXECUTAR-ENDBR.

           PERFORM 1950-ENVIAR-MAPA.

      *----------------------------------------------------------------*
       1900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1910-POSICIONAR-TRAVPBAN        SECTION.
      *----------------------------------------------------------------*

           MOVE LOW-VALUES             TO BVVED1-CHAVE
           MOVE    10                  TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE

           EXEC CICS
                STARTBR FILE  ('TRAVPBAN')
                        RIDFLD(BVVED1-CHAVE)
                        NOHANDLE
           END-EXEC.

           IF  EIBRESP                 NOT EQUAL DFHRESP (NORMAL) AND
               EIBRESP                 NOT EQUAL DFHRESP (NOTFND)
               PERFORM 9998-TRATAR-ERRO
           END-IF.

           IF  EIBRESP                 EQUAL DFHRESP (NOTFND)
               MOVE WRK-MSG02          TO MENSAGO
               PERFORM 1950-ENVIAR-MAPA
           END-IF.

      *----------------------------------------------------------------*
       1910-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1920-LER-TRAVPBAN               SECTION.
      *----------------------------------------------------------------*

           MOVE 11                      TO WRK-LOCAL-EIBRESP
                                           WRK-LOCAL-ABCODE.

           EXEC CICS
                READNEXT FILE  ('TRAVPBAN')
                         INTO  (BVVED1-TRAVPBAN)
                         RIDFLD(BVVED1-CHAVE)
                         NOHANDLE
           END-EXEC.

           IF  EIBRESP                 NOT EQUAL DFHRESP (NORMAL) AND
               EIBRESP                 NOT EQUAL DFHRESP (ENDFILE)
               MOVE EIBRESP            TO WRK-EIBRESP-AUX
               PERFORM 1930-EXECUTAR-ENDBR
               MOVE WRK-EIBRESP-AUX    TO EIBRESP
               PERFORM 9998-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1920-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1930-EXECUTAR-ENDBR             SECTION.
      *----------------------------------------------------------------*

           MOVE 12                     TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE.

           EXEC CICS
                ENDBR FILE ('TRAVPBAN')
           END-EXEC.

      *----------------------------------------------------------------*
       1930-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1940-MONTAR-BANDEIRAS           SECTION.
      *----------------------------------------------------------------*

           IF  BVVED1-IND-TRAVA        EQUAL 1
               ADD 1                   TO WRK-IND
               MOVE BVVED1-DESC-BAND   TO WRK-BANDI(WRK-IND)
               MOVE ' '                TO WRK-SELA (WRK-IND)
               MOVE BVVED1-COD-BAND    TO WRK-COMM-BAN(WRK-IND)
           END-IF.

           PERFORM 1920-LER-TRAVPBAN.

      *----------------------------------------------------------------*
       1940-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1950-ENVIAR-MAPA                SECTION.
      *----------------------------------------------------------------*

           IF  WRK-COMM-FASE           EQUAL '2' OR '0'
               MOVE '/'                TO   NROCNPJA SIMNAOA
                                           FILIALA  CTRLCGCA
                                           NROCPFA  CTRLCPFA
               PERFORM VARYING WRK-IND FROM 1 BY 1
                       UNTIL   WRK-IND GREATER 6
                       MOVE '/'           TO WRK-SELA (WRK-IND)
               END-PERFORM
           ELSE
               IF  WRK-INCONSISTENTE   EQUAL SPACES
                   MOVE  -1                TO NROCPFL
               END-IF
               PERFORM VARYING WRK-IND FROM 1 BY 1
                       UNTIL   WRK-IND GREATER 6
                  IF  WRK-BANDI(WRK-IND) EQUAL SPACES OR LOW-VALUES
                      MOVE '/'           TO WRK-SELA (WRK-IND)
                  END-IF
               END-PERFORM
           END-IF.

           EXEC CICS
                ASKTIME ABSTIME (WRK-ABSTIME)
           END-EXEC.

           EXEC CICS
                FORMATTIME ABSTIME  (WRK-ABSTIME)
                           DATESEP  ('/')
                           DDMMYYYY (DTASISO)
                           TIMESEP  (':')
                           TIME     (HORSISO)
           END-EXEC.

           MOVE    09                  TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE

           EXEC CICS
                SEND MAP   ('BVVE405')
                     MAPSET('BVVE405')
                     FROM  (BVVE405O)
                     CURSOR
                     ERASE
                     FREEKB
           END-EXEC

           IF EIBRESP                  NOT EQUAL DFHRESP(NORMAL)
              MOVE    EIBRESP          TO WRK-EIBRESP-MSG
              EXEC CICS
                   SEND FROM(WRK-MSG99)
                        ERASE
              END-EXEC
           END-IF.

           EXEC CICS
                RETURN TRANSID ('VV4E')
                       COMMAREA(WRK-COMMAREA)
           END-EXEC.

      *----------------------------------------------------------------*
       1950-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000-RECEBER-MAPA               SECTION.
      *----------------------------------------------------------------*

           MOVE    13                  TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE

           EXEC CICS
                RECEIVE MAP   ('BVVE405')
                        MAPSET('BVVE405')
                        INTO  (BVVE405I)
           END-EXEC.

           IF EIBRESP                  NOT EQUAL DFHRESP(NORMAL)
              PERFORM 9998-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9000-ENCERRAR-PGM               SECTION.
      *----------------------------------------------------------------*

           MOVE    14                  TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE

           EXEC CICS
                SEND FROM(WRK-MSG05)
                     ERASE
           END-EXEC.

           IF EIBRESP                  NOT EQUAL DFHRESP(NORMAL)
              PERFORM 9998-TRATAR-ERRO
           END-IF

           EXEC CICS
                RETURN
           END-EXEC.

      *----------------------------------------------------------------*
       9000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9998-TRATAR-ERRO                SECTION.
      *----------------------------------------------------------------*

           IF EIBRESP                  NOT EQUAL ZEROS
              MOVE    EIBRESP          TO WRK-EIBRESP-MSG
              MOVE    WRK-MSG99        TO MENSAGO

           ELSE
              PERFORM 9999-ERRO-ABCODE
              MOVE    WRK-MSG00        TO MENSAGO
           END-IF.

           MOVE   '0'                  TO WRK-COMM-FASE

           PERFORM 1950-ENVIAR-MAPA.

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
