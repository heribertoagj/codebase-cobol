      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. BVVE1036.
       AUTHOR. DEISI APARECIDA MARTINS.
      *================================================================*
      *                    C P M   S I S T E M A S                     *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : BVVE1036                                     *
      *    PROGRAMADORA : DEISI APARECIDA MARTINS    -  CPM/FPOLIS     *
      *    ANALISTA CPM : JOSIANE T. SOARES LUZ      -  CPM/FPOLIS     *
      *    ANALISTA     : LOURIVAL DAMASIO           -  GRUPO 87       *
      *    DATA         : 21/06/2004                                   *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      MENU DA PARAMETRIZACAO DE TAXAS.                          *
      *                                                                *
      *    MAPA/MAPSET  :                                              *
      *      BVVE036 - MENU DA PARAMETRIZACAO DE TAXAS.                *
      *                                                                *
      *    TRANSACAO    :                                              *
      *      TC36                                                      *
      *                                                                *
      *    NAVEGACAO    :                                              *
      *      CHAMADO POR: SENH8040              CHAMA: BVVE1037        *
      *                                                BVVE1038        *
      *                                                BVVE1039        *
      *                                                BVVE1040        *
      *                                                BVVE1041        *
      *                                                                *
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
           '*  INICIO DA WORKING BVVE1036  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)        VALUE
           '*     VARIAVEIS AUXILIARES     *'.
      *----------------------------------------------------------------*

       77  WRK-ABSTIME                 PIC S9(15) COMP-3 VALUE ZEROS.
       77  WRK-LENGTH                  PIC S9(04) COMP   VALUE ZEROS.
       77  WRK-PROGRAMA                PIC  X(08)        VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*      AREA DE MENSAGENS       *'.
      *----------------------------------------------------------------*

       01  WRK-MENSAGENS.
           03  WRK-MSG01               PIC  X(79)        VALUE
               'PF INVALIDA'.
           03  WRK-MSG02               PIC  X(79)        VALUE
               'POSICIONE O CURSOR NA OPCAO DESEJADA E TECLE <ENTER>'.
           03  WRK-MSG03               PIC  X(79)        VALUE
               'TC36 - FIM DA TRANSACAO'.
           03  WRK-MSG04               PIC  X(79)        VALUE
               'ACESSO PERMITIDO SOMENTE PELO SENHAS'.
           03  WRK-MSG05               PIC  X(79)        VALUE
               'OPCAO INVALIDA'.
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
           03  WRK-COMM-PADRAO.
               05  WRK-COMM-TRANSACAO  PIC  X(04)        VALUE SPACES.
               05  WRK-COMM-USUARIO    PIC  X(07)        VALUE SPACES.
           03  WRK-COMM-PGM.
               05  WRK-COMM-FASE       PIC  X(01)        VALUE SPACES.

       01  WRK-AREA-SENHAS-REC.
           03  WRK-SISTEMA-REC         PIC  X(04)        VALUE SPACES.
           03  WRK-FUNCAO-REC          PIC  9(02)        VALUE ZEROS.
           03  WRK-TIPO-USR-REC        PIC  X(01)        VALUE SPACES.
           03  WRK-FUNC-BRADESCO-REC.
               05  WRK-AGENCIA-DEPTO-REC
                                       PIC  9(04)        VALUE ZEROS.
               05  WRK-SECAO-REC       PIC  9(03)        VALUE ZEROS.
               05  FILLER              PIC  X(09)        VALUE SPACES.
           03  WRK-USR-EXTERNO-REC REDEFINES WRK-FUNC-BRADESCO-REC.
7C2511*        05  WRK-CGC-REC         PIC  9(09).
7C2511*        05  WRK-FILIAL-REC      PIC  9(05).
7C2511         05  WRK-CGC-REC         PIC  X(09).
7C2511         05  WRK-FILIAL-REC      PIC  X(04).
7C2511         05  FILLER              PIC  X(01).
               05  WRK-CONTROLE-REC    PIC  9(02).
           03  WRK-TERMIANL-REC        PIC  X(08)        VALUE SPACES.
           03  WRK-IMPRESSORA-REC      PIC  X(08)        VALUE SPACES.
           03  WRK-COD-USR-REC         PIC  X(07)        VALUE SPACES.
           03  FILLER                  PIC  X(209)       VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*    AREAS RESERVADAS CICS     *'.
      *----------------------------------------------------------------*

       COPY DFHAID.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*     AREA DO MAPA BVVE036     *'.
      *----------------------------------------------------------------*

       COPY BVVE036.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*   FIM DA WORKING BVVE1036   *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  DFHCOMMAREA.
           03  FILLER                  PIC  X(12).

      *================================================================*
       PROCEDURE                       DIVISION USING DFHCOMMAREA.
      *================================================================*

      *----------------------------------------------------------------*
       000000-ROTINA-PRINCIPAL         SECTION.
      *----------------------------------------------------------------*

           EXEC CICS
                HANDLE ABEND
                       LABEL(999998-TRATAR-ERRO)
           END-EXEC.

           EXEC CICS
                HANDLE CONDITION
                       MAPFAIL(999998-TRATAR-ERRO)
                       ERROR  (999998-TRATAR-ERRO)
           END-EXEC.

           EXEC CICS
                HANDLE AID
                       PF2   (500000-DESCONECTAR)
                       PF5   (600000-RETORNAR-MENU)
                       CLEAR (200000-ENCERRAR-PGM)
                       ENTER (700000-TRATAR-ENTER)
                       ANYKEY(800000-TRATAR-PF-INVALIDA)
           END-EXEC.

           IF  EIBCALEN                EQUAL ZEROS
               PERFORM 100000-EXECUTAR-RETRIEVE
               PERFORM 300000-RECEBER-CONTROLE
           ELSE
               IF  EIBTRNID            NOT EQUAL 'TC36' AND 'TC37' AND
                                                 'TC38' AND 'TC39' AND
                                                 'TC40' AND 'TC41'
                  MOVE WRK-MSG04       TO WRK-MSG03
                  PERFORM 200000-ENCERRAR-PGM
               ELSE
                  MOVE DFHCOMMAREA(1:EIBCALEN)
                                       TO WRK-COMMAREA
               END-IF
           END-IF.

           IF  EIBTRNID                NOT EQUAL 'TC36'
RICA  *    IF (EIBCALEN  EQUAL  ZEROS)                     OR
RICA  *       (WRK-COMM-TRANSACAO  NOT =  'TC36')
               PERFORM 300000-RECEBER-CONTROLE
           ELSE
               PERFORM 400000-RECEBER-MAPA
           END-IF.

      *----------------------------------------------------------------*
       000000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       100000-EXECUTAR-RETRIEVE        SECTION.
      *----------------------------------------------------------------*

           MOVE +255                   TO WRK-LENGTH.

           EXEC CICS
                RETRIEVE INTO   (WRK-AREA-SENHAS-REC)
                         LENGTH (LENGTH OF WRK-AREA-SENHAS-REC)
                         NOHANDLE
           END-EXEC.

           IF  EIBRESP                 NOT EQUAL DFHRESP(NORMAL)
               MOVE WRK-MSG04          TO WRK-MSG03
               PERFORM 200000-ENCERRAR-PGM
           ELSE
               MOVE WRK-COD-USR-REC    TO WRK-COMM-USUARIO
           END-IF.

      *----------------------------------------------------------------*
       100000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       200000-ENCERRAR-PGM             SECTION.
      *----------------------------------------------------------------*

           MOVE 01                     TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE.

           EXEC CICS
                SEND FROM(WRK-MSG03)
                     ERASE
           END-EXEC.

           EXEC CICS
                RETURN
           END-EXEC.

      *----------------------------------------------------------------*
       200000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       300000-RECEBER-CONTROLE         SECTION.
      *----------------------------------------------------------------*

           MOVE LOW-VALUES             TO BVVE036I.
           MOVE 'TC36'                 TO WRK-COMM-TRANSACAO.
           MOVE '1'                    TO WRK-COMM-FASE.
           MOVE WRK-MSG02              TO MENSAGO.

           PERFORM 310000-ENVIAR-MAPA.

      *----------------------------------------------------------------*
       300000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       310000-ENVIAR-MAPA              SECTION.
      *----------------------------------------------------------------*

           IF  WRK-COMM-FASE           EQUAL '1'
               MOVE -1                 TO SELECL
           END-IF.

           EXEC CICS
                ASKTIME ABSTIME (WRK-ABSTIME)
           END-EXEC.

           EXEC CICS
                FORMATTIME ABSTIME (WRK-ABSTIME)
                           DATESEP  ('/')
                           DDMMYYYY (DTASISO)
                           TIMESEP  (':')
                           TIME     (HORSISO)
           END-EXEC.

           MOVE 02                     TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE.

           EXEC CICS
                SEND MAP   ('BVVE036')
                     MAPSET('BVVE036')
                     FROM  (BVVE036O)
                     CURSOR
                     ERASE
                     FREEKB
           END-EXEC.

           MOVE 03                     TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE.

           EXEC CICS
                RETURN TRANSID ('TC36')
                       COMMAREA(WRK-COMMAREA)
           END-EXEC.

      *----------------------------------------------------------------*
       310000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       400000-RECEBER-MAPA             SECTION.
      *----------------------------------------------------------------*

           MOVE 04                     TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE.

           EXEC CICS
                RECEIVE MAP   ('BVVE036')
                        MAPSET('BVVE036')
                        INTO  (BVVE036I)
           END-EXEC.

      *----------------------------------------------------------------*
       400000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       500000-DESCONECTAR              SECTION.
      *----------------------------------------------------------------*

           MOVE 05                     TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE.

           EXEC CICS
                ISSUE DISCONNECT
           END-EXEC.

           EXEC CICS
                RETURN
           END-EXEC.

      *----------------------------------------------------------------*
       500000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       600000-RETORNAR-MENU            SECTION.
      *----------------------------------------------------------------*

           MOVE 'BVVE'                 TO WRK-SISTEMA-PAS.
           MOVE 'P'                    TO WRK-NIVEL-PAS.

           MOVE 06                     TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE.

           EXEC CICS
                LINK PROGRAM ('SENH1005')
                     COMMAREA(WRK-AREA-SENH1005-PAS)
                     LENGTH  (+255)
           END-EXEC.

           EXEC CICS
                RETURN
           END-EXEC.

      *----------------------------------------------------------------*
       600000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       700000-TRATAR-ENTER             SECTION.
      *----------------------------------------------------------------*

           IF  WRK-COMM-FASE           EQUAL '0'
               PERFORM 800000-TRATAR-PF-INVALIDA
           END-IF.

           IF  EIBCPOSN                NOT EQUAL 830 AND 910  AND
                                                 990 AND 1070 AND 01150
               MOVE WRK-MSG05          TO MENSAGO
               PERFORM 310000-ENVIAR-MAPA
           END-IF.

           PERFORM 710000-PASSAR-CONTROLE.

      *----------------------------------------------------------------*
       700000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       710000-PASSAR-CONTROLE          SECTION.
      *----------------------------------------------------------------*

           EVALUATE EIBCPOSN

               WHEN 830
                    MOVE 'BVVE1037'    TO WRK-PROGRAMA

               WHEN 910
                    MOVE 'BVVE1038'    TO WRK-PROGRAMA

               WHEN 990
                    MOVE 'BVVE1039'    TO WRK-PROGRAMA

               WHEN 1070
                    MOVE 'BVVE1040'    TO WRK-PROGRAMA

               WHEN 1150
                    MOVE 'BVVE1041'    TO WRK-PROGRAMA

           END-EVALUATE.

           EXEC CICS
                XCTL PROGRAM (WRK-PROGRAMA)
                     COMMAREA(WRK-COMM-PADRAO)
           END-EXEC.

      *----------------------------------------------------------------*
       710000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       800000-TRATAR-PF-INVALIDA       SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MSG01              TO MENSAGO.

           PERFORM 310000-ENVIAR-MAPA.

      *----------------------------------------------------------------*
       800000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       999998-TRATAR-ERRO              SECTION.
      *----------------------------------------------------------------*

           IF  EIBRESP                 NOT EQUAL ZEROS
               MOVE EIBRESP            TO WRK-EIBRESP
               MOVE WRK-MSG99          TO MENSAGO
           ELSE
               PERFORM 999999-ERRO-ABCODE
               MOVE WRK-MSG00          TO MENSAGO
           END-IF.

           MOVE '0'                    TO WRK-COMM-FASE.

           PERFORM 310000-ENVIAR-MAPA.

      *----------------------------------------------------------------*
       999998-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       999999-ERRO-ABCODE              SECTION.
      *----------------------------------------------------------------*

           EXEC CICS
                ASSIGN ABCODE(WRK-ABCODE-MSG)
           END-EXEC.

      *----------------------------------------------------------------*
       999999-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
