      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. BVVE4030.
       AUTHOR.     FRANCISMARA.
      *================================================================*
      *                  B S I - T E C N O L O G I A                   *
      *----------------------------------------------------------------*
      *    PROGRAMA.....: BVVE4030                                     *
      *    PROGRAMADORA.: FRANCISMARA          - BSI                   *
      *    DATA.........: JULHO/2012                                   *
      *----------------------------------------------------------------*
      *    OBJETIVO     :                                              *
      *      MENU - MANUTENCAO DE BANDEIRAS - CIP                      *
      *----------------------------------------------------------------*
      *    MAPA/MAPSET  :                                              *
      *      BVVE430 - MENU DE BANDEIRAS - CIP                         *
      *----------------------------------------------------------------*
      *    TRANSACAO    :                                              *
      *      VV4P                                                      *
      *----------------------------------------------------------------*
      *    NAVEGACAO    :                                              *
      *      CHAMADO POR: SENH8020              CHAMA: BVVE4031        *
      *                                                BVVE4032        *
      *                                                BVVE4033        *
      *                                                BVVE4034        *
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
           '*  INICIO DA WORKING BVVE4030  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)        VALUE
           '*         INDEXADORES          *'.
      *----------------------------------------------------------------*
       01  WRK-INDEXADORES.
           03  WRK-IND                 PIC  9(03) COMP-3  VALUE ZEROS.
           03  WRK-IND-2               PIC  9(03) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)        VALUE
           '*     VARIAVEIS AUXILIARES     *'.
      *----------------------------------------------------------------*
       01  WRK-AUXILIARES.
           03  WRK-ABSTIME             PIC S9(15) COMP-3 VALUE ZEROS.
           03  WRK-LENGTH              PIC S9(04) COMP   VALUE ZEROS.
           03  WRK-PROGRAMA            PIC  X(08)        VALUE SPACES.
           03  WRK-INCONSISTENTE       PIC  X(01)        VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*      AREA DE MENSAGENS       *'.
      *----------------------------------------------------------------*

       01  WRK-MENSAGENS.
           03  WRK-MSG01               PIC  X(79)        VALUE
               'PF invalida'.

           03  WRK-MSG02               PIC  X(79)        VALUE
               'Selecione uma opcao e tecle <ENTER>'.
           03  WRK-MSG03               PIC  X(79)        VALUE
               'VV4P - Fim da transacao'.

           03  WRK-MSG04               PIC  X(79)        VALUE
               'Acesso permitido somente pelo SENHAS'.

           03  WRK-MSG05               PIC  X(79)        VALUE
               'Opcao invalida'.

           03  WRK-MSG99.
               05  FILLER              PIC  X(35)        VALUE
                   'Erro nao identif pelo pgm. Eibresp='.
               05  WRK-EIBRESP         PIC  9(04)        VALUE ZEROS.
               05  FILLER              PIC  X(09)        VALUE
                   ' e Local='.
               05  WRK-LOCAL-EIBRESP   PIC  9(02)        VALUE ZEROS.
               05  FILLER              PIC  X(20)        VALUE
                   '. Contactar analista'.

           03  WRK-MSG00.
               05  FILLER              PIC  X(19)        VALUE
                   'Erro cics - Abcode='.
               05  WRK-ABCODE-MSG      PIC  X(08)        VALUE SPACES.
               05  FILLER              PIC  X(09)        VALUE
                   ' e Local='.
               05  WRK-LOCAL-ABCODE    PIC  9(02)        VALUE ZEROS.
               05  FILLER              PIC  X(30)        VALUE
                   '. Contactar analista'.

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

       01  WRK-COMMAREA-4031           PIC  X(54)        VALUE SPACES.

       01  WRK-AREA-SENHAS-REC.
           03  WRK-SISTEMA-REC         PIC  X(04)        VALUE SPACES.
           03  WRK-FUNCAO-REC          PIC  9(02)        VALUE ZEROS.
           03  WRK-TIPO-USR-REC        PIC  X(01)        VALUE SPACES.
           03  WRK-FUNC-BRADESCO-REC.
               05  WRK-AGENCIA-DEPTO-REC
                                       PIC  9(04)        VALUE ZEROS.
               05  WRK-SECAO-REC       PIC  9(03)        VALUE ZEROS.
               05  FILLER              PIC  X(09)        VALUE SPACES.
           03  WRK-FUNC-BRADESCO-R REDEFINES WRK-FUNC-BRADESCO-REC.
               05  WRK-FUNC-REC        PIC  9(07).
               05  FILLER              PIC  X(09).
           03  WRK-USR-EXTERNO-REC REDEFINES WRK-FUNC-BRADESCO-REC.
7C2511*        05  WRK-CGC-REC         PIC  9(09).
7C2511*        05  WRK-FILIAL-REC      PIC  9(05).
7C2511         05  WRK-CGC-REC         PIC  X(09).
7C2511         05  WRK-FILIAL-REC      PIC  X(04).
7C2511         05  FILLER              PIC  X(01).
               05  WRK-CONTROLE-REC    PIC  9(02).
           03  WRK-TERMINAL-REC        PIC  X(08)        VALUE SPACES.
           03  WRK-IMPRESSORA-REC      PIC  X(08)        VALUE SPACES.
           03  WRK-COD-USR-REC         PIC  X(07)        VALUE SPACES.
           03  FILLER                  PIC  X(209)       VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*     AREA DO MAPA BVVE430     *'.
      *----------------------------------------------------------------*

       COPY 'BVVE430'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*   FIM DA WORKING BVVE4030   *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  DFHCOMMAREA.
           03  FILLER                  PIC  X(009).

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
                       PF3   (1300-TRATAR-PF5)
                       PF5   (1300-TRATAR-PF5)
                       CLEAR (9000-ENCERRAR-PGM)
                       ENTER (1500-TRATAR-ENTER)
                       ANYKEY(1800-TRATAR-PF-INVALIDA)
           END-EXEC.

           IF EIBRESP                  NOT EQUAL DFHRESP(NORMAL)
              PERFORM 9998-TRATAR-ERRO
           END-IF

           IF EIBCALEN                 EQUAL ZEROS
              PERFORM 1900-EXECUTAR-RETRIEVE
           ELSE
              IF EIBTRNID              NOT EQUAL 'VV4P' AND 'VV4Q' AND
                                                 'VV4R' AND 'VV4S' AND
                                                 'VV4T' AND 'VV4W' AND
                                                 'VV4Y'
                 MOVE    WRK-MSG04     TO WRK-MSG03
                 PERFORM 9000-ENCERRAR-PGM

              ELSE
                 MOVE DFHCOMMAREA(1:EIBCALEN)
                                       TO WRK-COMMAREA
              END-IF
           END-IF.

           IF EIBTRNID                 NOT EQUAL 'VV4P' OR
              EIBCALEN                 EQUAL ZEROS
              PERFORM 1400-RECEBER-CONTROLE
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
       1400-RECEBER-CONTROLE           SECTION.
      *----------------------------------------------------------------*

           MOVE    LOW-VALUES          TO BVVE430I

           MOVE   '1'                  TO WRK-COMM-FASE

           MOVE    WRK-MSG02           TO MENSAGO

           PERFORM 1410-ENVIAR-MAPA.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1410-ENVIAR-MAPA                SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INCONSISTENTE       EQUAL SPACES
               MOVE -1                 TO OPCAOL
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

           MOVE    07                  TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE

           EXEC CICS
                SEND MAP   ('BVVE430')
                     MAPSET('BVVE430')
                     FROM  (BVVE430O)
                     CURSOR
                     ERASE
                     FREEKB
           END-EXEC

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
                   RETURN TRANSID ('VV4P')
                          COMMAREA(WRK-COMMAREA)
              END-EXEC
           END-IF.

      *----------------------------------------------------------------*
       1410-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1500-TRATAR-ENTER               SECTION.
      *----------------------------------------------------------------*

           IF OPCAOI                   EQUAL SPACES OR  LOW-VALUES
              MOVE    WRK-MSG02        TO MENSAGO
              PERFORM 1410-ENVIAR-MAPA
           END-IF.

           IF  OPCAOI                  NOT NUMERIC OR
               OPCAOO                  EQUAL ZEROS
               MOVE   -1               TO OPCAOL
               MOVE   'I'              TO OPCAOA
               MOVE   '*'              TO WRK-INCONSISTENTE
               MOVE    WRK-MSG05       TO MENSAGO
               PERFORM 1410-ENVIAR-MAPA
           END-IF.

           IF  OPCAOO                  LESS 7
               PERFORM 1510-PASSAR-CONTROLE
           ELSE
               MOVE   -1               TO OPCAOL
               MOVE   'I'              TO OPCAOA
               MOVE   '*'              TO WRK-INCONSISTENTE
               MOVE    WRK-MSG05       TO MENSAGO
               PERFORM 1410-ENVIAR-MAPA
           END-IF.

      *----------------------------------------------------------------*
       1500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1510-PASSAR-CONTROLE            SECTION.
      *----------------------------------------------------------------*

           EVALUATE OPCAOO
               WHEN 1
                  MOVE 'BVVE4031'      TO WRK-PROGRAMA
                   EXEC CICS
                   XCTL PROGRAM (WRK-PROGRAMA)
                         COMMAREA(WRK-COMMAREA-4031)
                   END-EXEC
               WHEN 2
                  MOVE 'BVVE4032'      TO WRK-PROGRAMA
                   EXEC CICS
                   XCTL PROGRAM (WRK-PROGRAMA)
                         COMMAREA(WRK-COMMAREA-4031)
                   END-EXEC
               WHEN 3
                  MOVE 'BVVE4033'      TO WRK-PROGRAMA
                   EXEC CICS
                   XCTL PROGRAM (WRK-PROGRAMA)
                         COMMAREA(WRK-COMMAREA-4031)
                   END-EXEC
               WHEN 4
                  MOVE 'BVVE4034'      TO WRK-PROGRAMA
                   EXEC CICS
                   XCTL PROGRAM (WRK-PROGRAMA)
                         COMMAREA(WRK-COMMAREA)
                   END-EXEC
               WHEN 5
                  MOVE 'BVVE3911'      TO WRK-PROGRAMA
                   EXEC CICS
                   XCTL PROGRAM (WRK-PROGRAMA)
                         COMMAREA(WRK-COMMAREA)
                   END-EXEC
               WHEN 6
                  MOVE 'BVVE3931'      TO WRK-PROGRAMA
                   EXEC CICS
                   XCTL PROGRAM (WRK-PROGRAMA)
                         COMMAREA(WRK-COMMAREA)
                   END-EXEC
           END-EVALUATE.


           MOVE    09                  TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE

           IF EIBRESP                  NOT EQUAL DFHRESP(NORMAL)
              PERFORM 9998-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1510-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1800-TRATAR-PF-INVALIDA         SECTION.
      *----------------------------------------------------------------*

           MOVE    WRK-MSG01           TO MENSAGO

           PERFORM 1410-ENVIAR-MAPA.

      *----------------------------------------------------------------*
       1800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1900-EXECUTAR-RETRIEVE          SECTION.
      *----------------------------------------------------------------*

           MOVE   +255                TO WRK-LENGTH

           EXEC CICS
                RETRIEVE INTO   (WRK-AREA-SENHAS-REC)
                         LENGTH (WRK-LENGTH)
                         NOHANDLE
           END-EXEC

           IF EIBRESP                  NOT EQUAL DFHRESP(NORMAL)
              MOVE    WRK-MSG04        TO WRK-MSG03
              PERFORM 9000-ENCERRAR-PGM
           END-IF.

      *----------------------------------------------------------------*
       1900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000-RECEBER-MAPA               SECTION.
      *----------------------------------------------------------------*

           MOVE    08                  TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE

           EXEC CICS
                RECEIVE MAP   ('BVVE430')
                        MAPSET('BVVE430')
                        INTO  (BVVE430I)
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

           MOVE    06                  TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE

           EXEC CICS
                SEND FROM(WRK-MSG04)
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
              MOVE    EIBRESP          TO WRK-EIBRESP
              MOVE    WRK-MSG99        TO MENSAGO

           ELSE
              PERFORM 9999-ERRO-ABCODE
              MOVE    WRK-MSG00        TO MENSAGO
           END-IF.

           MOVE   '0'                  TO WRK-COMM-FASE

           PERFORM 1410-ENVIAR-MAPA.

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
