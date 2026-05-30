      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. BVVE1978.
       AUTHOR.     MARCOS TADEU.
      *================================================================*
      *                        B S I                                   *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : BVVE1978                                     *
      *    PROGRAMADOR  : MARCOS TADEU         - B S I                 *
      *    ANALISTA     : SERGIO VIEIRA JUNIOR - B S I                 *
      *    DATA         : 10/02/2015                                   *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      CARTAO DE CREDITO - VISA VALE - SELECAO DE TIPOS DE       *
      *      EMPRESAS                                                  *
      *                                                                *
      *    MAPA/MAPSET  :                                              *
      *      BVVE978 - SELECAO DE TIPOS DE EMPRESAS                    *
      *                                                                *
      *    TRANSACAO    :                                              *
      *      VVA9                                                      *
      *                                                                *
      *    ARQUIVOS     :                                              *
      *      DDNAME                               INCLUDE/BOOK         *
      *      PPEMPRES                               I#BVVEEM           *
      *                                                                *
      *    NAVEGACAO    :                                              *
      *      CHAMADO POR: BVVE1977              CHAMA: BVVE1977        *
      *                         NAO SE APLICA          BVVE1817        *
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
           '*  INICIO DA WORKING BVVE1978  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)        VALUE
           '*         INDEXADORES          *'.
      *----------------------------------------------------------------*
       01  WRK-INDEXADORES.
           03  WRK-IND                 PIC  9(03) COMP-3 VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)        VALUE
           '*         ACUMULADORES         *'.
      *----------------------------------------------------------------*
       01  WRK-ACUMULADORES.
           03  WRK-AC-SEL              PIC  9(03) COMP-3 VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)        VALUE
           '*     VARIAVEIS AUXILIARES     *'.
      *----------------------------------------------------------------*
       01  WRK-AUXILIARES.
           03  WRK-ABSTIME             PIC S9(15) COMP-3 VALUE ZEROS.
           03  WRK-LENGTH              PIC S9(04) COMP   VALUE ZEROS.
           03  WRK-PROGRAMA            PIC  X(08)        VALUE SPACES.
           03  WRK-INCONSISTENTE       PIC  X(01)        VALUE SPACES.
           03  WRK-SEL                 PIC  9(03) COMP-3 VALUE ZEROS.
           03  WRK-LINHA-SEL           PIC  9(02) COMP-3 VALUE ZEROS.
           03  WRK-PAGINA-SEL          PIC  9(03) COMP-3 VALUE ZEROS.

           03  WRK-ITEM-TS             PIC S9(04) COMP   VALUE ZEROS.

           03  WRK-NOME-TS.
               05  WRK-TERMID          PIC  X(04)        VALUE SPACES.
               05  WRK-TRANID          PIC  X(04)        VALUE SPACES.

           03  WRK-AREA-TS.
               05  WRK-TAB-EMPRESAS    OCCURS 14.
                   07  WRK-TAB-COD     PIC  9(02).
                   07  WRK-TAB-DESC    PIC  X(70).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*      AREA DE MENSAGENS       *'.
      *----------------------------------------------------------------*

       01  WRK-MENSAGENS.
           03  WRK-MSG01               PIC  X(79)        VALUE
               'PF invalida'.

           03  WRK-MSG02               PIC  X(79)        VALUE
               'Transacao encerrada. Entrada somente via Menu'.

           03  WRK-MSG03               PIC  X(79)        VALUE
               'Nao ha dados a serem exibidos'.

           03  WRK-MSG04               PIC  X(79)        VALUE
               'Selecione uma opcao com "X" para alteracao na proposta e
      -        ' tecle <ENTER>'.

           03  WRK-MSG05               PIC  X(79)        VALUE
               'Nao ha paginas para retornar'.

           03  WRK-MSG06               PIC  X(79)        VALUE
               'Nao ha paginas para avancar'.

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
           '*      COMMAREA AUXILIAR       *'.
      *----------------------------------------------------------------*

       01  WRK-COMMAREA.
           03  WRK-COMM-PADRAO.
               05  WRK-COMM-TRANSACAO  PIC  X(04)        VALUE SPACES.
               05  WRK-COMM-FASE       PIC  X(01)        VALUE SPACES.
               05  WRK-COMM-PAG        PIC  9(05) COMP-3 VALUE ZEROS.
               05  WRK-COMM-ULT-PAG    PIC  9(05) COMP-3 VALUE ZEROS.
               05  WRK-COMM-MENSAG     PIC  X(79)        VALUE SPACES.
           03  WRK-COMM-PGM-2950.
               05  WRK-COMM-AG-ORIG    PIC  9(04)        VALUE ZEROS.
               05  WRK-COMM-OPCAO      PIC  X(01)        VALUE SPACES.
7C2511*        05  WRK-COMM-CNPJ       PIC  9(08)        VALUE ZEROS.
7C2511         05  WRK-COMM-CNPJ       PIC  X(08)        VALUE SPACES.
7C2511*        05  WRK-COMM-FILIAL     PIC  9(04)        VALUE ZEROS.
7C2511         05  WRK-COMM-FILIAL     PIC  X(04)        VALUE SPACES.
               05  WRK-COMM-CONTROL    PIC  9(02)        VALUE ZEROS.
           03  WRK-COMM-PGM-1815.
               05  WRK-COMM-PROD       PIC  9(03)        VALUE ZEROS.
               05  WRK-COMM-DESCR-PROD PIC  X(30)        VALUE SPACES.
               05  WRK-COMM-TIPOEMP    PIC  9(02)        VALUE ZEROS.
               05  WRK-COMM-TIPORES    PIC  9(02)        VALUE ZEROS.
               05  WRK-COMM-MEIO-PG    PIC  9(01)        VALUE ZEROS.
           03  WRK-COMM-PGM-1816.
               05  WRK-COMM-TIPOEMP-1816
                                       PIC  9(02)        VALUE ZEROS.
               05  WRK-COMM-DESCR-EMP  PIC  X(30)        VALUE SPACES.
           03  FILLER                  PIC  X(571)       VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*     AREA DO MAPA BVVE978     *'.
      *----------------------------------------------------------------*

           COPY BVVE978.
       01  WRK-BVVE978R REDEFINES BVVE978I.
           03  FILLER                  PIC  X(59).
           03  WRK-DFHG001I            OCCURS 14 TIMES.
               05  WRK-SELECL          PIC S9(04) COMP.
               05  WRK-SELECA          PIC  X(01).
               05  WRK-SELECI          PIC  X(01).
               05  WRK-CODIGL          PIC S9(04) COMP.
               05  WRK-CODIGA          PIC  X(01).
               05  WRK-CODIGI          PIC  X(02).
               05  WRK-CODIGI-R        REDEFINES WRK-CODIGI
                                       PIC  9(02).
               05  WRK-DESCRL          PIC S9(04) COMP.
               05  WRK-DESCRA          PIC  X(01).
               05  WRK-DESCRI          PIC  X(70).
           03  FILLER                  PIC  X(82).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*   AREA DO ARQUIVO PPEMPRES  *'.
      *----------------------------------------------------------------*

           COPY 'I#BVVEEM'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*   FIM DA WORKING BVVE1978   *'.
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
                       PF7   (0800-TRATAR-PF7)
                       PF8   (0900-TRATAR-PF8)
                       CLEAR (0200-TRATAR-PF3)
                       ENTER (1000-TRATAR-ENTER)
                       ANYKEY(1100-TRATAR-PF-INVALIDA)
           END-EXEC

           IF EIBRESP                  NOT EQUAL DFHRESP(NORMAL)
              PERFORM 9998-TRATAR-ERRO
           END-IF

           INITIALIZE WRK-INDEXADORES
                      WRK-AUXILIARES
                      WRK-ACUMULADORES

           MOVE    EIBTRMID            TO WRK-TERMID
           MOVE   'VVA9'               TO WRK-TRANID

           IF (EIBCALEN                EQUAL ZEROS) OR
              (EIBTRNID                NOT EQUAL 'VVA8' AND 'VVA9' AND
                                                 'VV17' and 'VVE1')
              PERFORM 0300-ENCERRAR-TRANSACAO
           END-IF


           MOVE    DFHCOMMAREA(1:EIBCALEN)
                                       TO WRK-COMMAREA

           IF EIBTRNID                 NOT EQUAL 'VVA9'
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
                XCTL PROGRAM ('BVVE1977')
                     COMMAREA(WRK-COMMAREA)
           END-EXEC

           IF EIBRESP                  NOT EQUAL DFHRESP(NORMAL)
              PERFORM 9998-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0200-99-FIM.                    EXIT.
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

           PERFORM 0410-MONTAR-TELA

           PERFORM 0510-ENVIAR-MAPA.

      *----------------------------------------------------------------*
       0400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0410-MONTAR-TELA                SECTION.
      *----------------------------------------------------------------*

           MOVE    LOW-VALUES          TO BVVE978I
           MOVE   '1'                  TO WRK-COMM-FASE
           MOVE    ZEROS               TO WRK-COMM-PAG
                                          WRK-COMM-ULT-PAG
           MOVE    WRK-COMM-CNPJ       TO CNPJO
           MOVE    WRK-COMM-FILIAL     TO FILIALO
           MOVE    WRK-COMM-CONTROL    TO CONTROLO

           PERFORM 0420-DELETAR-TS

           PERFORM 0430-STARTBR-PPEMPRES

           IF EIBRESP                  EQUAL DFHRESP(NORMAL)
              PERFORM 0440-LER-PPEMPRES

              IF EIBRESP               EQUAL DFHRESP(NORMAL)
                 MOVE    1             TO WRK-COMM-PAG

                 PERFORM 0460-MONTAR-TS

                 PERFORM 0450-ENDBR-PPEMPRES

      *          VERIFICA SE UM ITEM JA HAVIA SIDO ANTERIORMENTE
      *          SELECIONADO, LENDO NA TS A PAGINA CORRESPONDENTE
                 IF WRK-PAGINA-SEL     NOT EQUAL ZEROS
                    MOVE    WRK-PAGINA-SEL
                                       TO WRK-COMM-PAG
                 END-IF

                 PERFORM 0480-LER-TS

                 PERFORM 0490-MONTAR-OCORR

      *          VERIFICA SE UM ITEM JA HAVIA SIDO ANTERIORMENTE
      *          SELECIONADO, MARCANDO O MESMO NA PAGINA
                 IF WRK-PAGINA-SEL     NOT EQUAL ZEROS
                    MOVE   -1          TO WRK-SELECL(WRK-LINHA-SEL)
                    MOVE   'X'         TO WRK-SELECI(WRK-LINHA-SEL)
                 END-IF

                 MOVE    WRK-MSG04     TO MENSAGO
              END-IF

           ELSE
              MOVE    WRK-MSG03        TO MENSAGO
           END-IF.

      *----------------------------------------------------------------*
       0410-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0420-DELETAR-TS                 SECTION.
      *----------------------------------------------------------------*

           MOVE    05                  TO WRK-LOCAL-EIBRESP
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
       0430-STARTBR-PPEMPRES           SECTION.
      *----------------------------------------------------------------*

           MOVE    06                  TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE

           MOVE    ZEROS               TO PARM-EMPR-CH-CODIGO

           EXEC CICS
                STARTBR FILE      ('PPEMPRES')
                        RIDFLD    (PARM-EMPR-CH)
                        KEYLENGTH (LENGTH OF PARM-EMPR-CH)
                        GTEQ
                        NOHANDLE
           END-EXEC.

           IF EIBRESP                  NOT EQUAL DFHRESP(NORMAL) AND
                                                 DFHRESP(NOTFND)
              PERFORM 9998-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0430-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0440-LER-PPEMPRES               SECTION.
      *----------------------------------------------------------------*

           MOVE    07                  TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE

           EXEC CICS
                READNEXT DATASET   ('PPEMPRES')
                         INTO      (PARM-EMPRESA)
                         LENGTH    (LENGTH OF PARM-EMPRESA)
                         RIDFLD    (PARM-EMPR-CH)
                         KEYLENGTH (LENGTH OF PARM-EMPR-CH)
                         NOHANDLE
           END-EXEC.

           IF EIBRESP                  NOT EQUAL DFHRESP(NORMAL) AND
                                                 DFHRESP(ENDFILE)
              PERFORM 9998-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0440-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0450-ENDBR-PPEMPRES             SECTION.
      *----------------------------------------------------------------*

           MOVE    08                  TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE

           EXEC CICS
                ENDBR FILE ('PPEMPRES')
           END-EXEC

           IF EIBRESP                  NOT EQUAL DFHRESP(NORMAL) AND
                                                 DFHRESP(NOTFND)
              PERFORM 9998-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0450-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0460-MONTAR-TS                  SECTION.
      *----------------------------------------------------------------*

           PERFORM VARYING WRK-IND  FROM 1 BY 1
                   UNTIL   EIBRESP  EQUAL DFHRESP(ENDFILE)

              IF WRK-IND               GREATER 14
                 PERFORM 0470-GRAVAR-TS
                 MOVE    1             TO WRK-IND
              END-IF

              MOVE    PARM-EMPR-CH-CODIGO
                                       TO WRK-TAB-COD(WRK-IND)
              MOVE    PARM-EMPR-DESCRICAO
                                       TO WRK-TAB-DESC(WRK-IND)

      *       VERIFICA SE O ITEM LIDO EH IGUAL AO ITEM ANTERIORMENTE
      *       SELECIONADO, PARA MARCACAO NA TELA
      *       OBS: CASO ESTEJA VINDO DA TRANSACAO VVA8, A COMPARACAO EH
      *       EM RELACAO AO ARQUIVO, CASO CONTRARIO EM RELACAO AO QUE
      *       FOI SELECIONADO
              IF EIBTRNID              EQUAL 'VVA8'
                 IF PARM-EMPR-CH-CODIGO   EQUAL WRK-COMM-TIPOEMP
                    MOVE    WRK-IND    TO WRK-LINHA-SEL
                    COMPUTE WRK-PAGINA-SEL = WRK-ITEM-TS + 1
                 END-IF
              ELSE
                 IF PARM-EMPR-CH-CODIGO
                                       EQUAL WRK-COMM-TIPOEMP-1816
                    MOVE    WRK-IND    TO WRK-LINHA-SEL
                    COMPUTE WRK-PAGINA-SEL = WRK-ITEM-TS + 1
                 END-IF
              END-IF

              PERFORM 0440-LER-PPEMPRES
           END-PERFORM

           IF WRK-IND                  NOT GREATER 15
              PERFORM 0470-GRAVAR-TS
           END-IF.

      *----------------------------------------------------------------*
       0460-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0470-GRAVAR-TS                  SECTION.
      *----------------------------------------------------------------*

           MOVE    09                  TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE

           ADD     1                   TO WRK-ITEM-TS
                                          WRK-COMM-ULT-PAG

           EXEC CICS
                WRITEQ TS QUEUE (WRK-NOME-TS)
                          FROM  (WRK-AREA-TS)
                          ITEM  (WRK-ITEM-TS)
           END-EXEC.

           IF EIBRESP                  NOT EQUAL DFHRESP(NORMAL)
              PERFORM 9998-TRATAR-ERRO
           END-IF.

           INITIALIZE WRK-AREA-TS.

      *----------------------------------------------------------------*
       0470-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0480-LER-TS                     SECTION.
      *----------------------------------------------------------------*

           MOVE    10                  TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE

           MOVE    WRK-COMM-PAG        TO WRK-ITEM-TS

           EXEC CICS
                READQ TS QUEUE (WRK-NOME-TS)
                         INTO  (WRK-AREA-TS)
                         ITEM  (WRK-ITEM-TS)
           END-EXEC.

           IF EIBRESP                  NOT EQUAL DFHRESP(NORMAL)
              PERFORM 9998-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0480-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0490-MONTAR-OCORR               SECTION.
      *----------------------------------------------------------------*

           PERFORM VARYING WRK-IND  FROM 1 BY 1
                   UNTIL   WRK-IND  GREATER 14 OR
                           WRK-TAB-DESC(WRK-IND) EQUAL SPACES OR
                                                       LOW-VALUES
              MOVE    SPACES           TO WRK-SELECI(WRK-IND)
              MOVE    WRK-TAB-COD(WRK-IND)
                                       TO WRK-CODIGI-R(WRK-IND)
              MOVE    WRK-TAB-DESC(WRK-IND)
                                       TO WRK-DESCRI(WRK-IND)
              MOVE   -1                TO WRK-SELECL(WRK-IND)
              MOVE   'A'               TO WRK-SELECA(WRK-IND)
           END-PERFORM

           IF WRK-IND                  NOT GREATER 14
              PERFORM VARYING WRK-IND  FROM WRK-IND BY 1
                      UNTIL   WRK-IND  GREATER 14
                 MOVE SPACES           TO WRK-SELECI(WRK-IND)
                                          WRK-CODIGI(WRK-IND)
                                          WRK-DESCRI(WRK-IND)
              END-PERFORM
           END-IF.

      *----------------------------------------------------------------*
       0490-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0500-DESPR-SELECAO              SECTION.
      *----------------------------------------------------------------*

           MOVE   -1                   TO WRK-SELECL(1)

           PERFORM VARYING WRK-IND FROM 1 BY 1
                   UNTIL   WRK-IND GREATER 14 OR
                           WRK-CODIGI(WRK-IND) EQUAL SPACES OR
                                                     LOW-VALUES
                   MOVE 'A'            TO WRK-SELECA(WRK-IND)
           END-PERFORM.

      *----------------------------------------------------------------*
       0500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0510-ENVIAR-MAPA                SECTION.
      *----------------------------------------------------------------*

           EXEC CICS
                ASKTIME ABSTIME (WRK-ABSTIME)
           END-EXEC.

           EXEC CICS
                FORMATTIME ABSTIME   (WRK-ABSTIME)
                           DATESEP   ('/')
                           DDMMYYYY  (DTASISO)
                           TIMESEP   (':')
                           TIME      (HORSISO)
           END-EXEC.

           MOVE    11                  TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE

           EXEC CICS
                SEND MAP   ('BVVE978')
                     MAPSET('BVVE978')
                     FROM  (BVVE978O)
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
                   RETURN TRANSID ('VVA9')
                          COMMAREA(WRK-COMMAREA)
              END-EXEC
           END-IF.

      *----------------------------------------------------------------*
       0510-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0600-RECEBER-MAPA               SECTION.
      *----------------------------------------------------------------*

           MOVE    12                  TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE

           EXEC CICS
                RECEIVE MAP   ('BVVE978')
                        MAPSET('BVVE978')
                        INTO  (BVVE978I)
           END-EXEC.

           IF EIBRESP                  NOT EQUAL DFHRESP(NORMAL)
              PERFORM 9998-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0800-TRATAR-PF7                 SECTION.
      *----------------------------------------------------------------*

           IF WRK-COMM-PAG             EQUAL ZEROS
              MOVE    WRK-MSG03        TO MENSAGO

           ELSE
              IF WRK-COMM-PAG          EQUAL 1
                 PERFORM 0500-DESPR-SELECAO
                 MOVE    WRK-MSG05     TO MENSAGO

              ELSE
                 SUBTRACT 1          FROM WRK-COMM-PAG
                 PERFORM 0480-LER-TS
                 PERFORM 0490-MONTAR-OCORR
                 MOVE    WRK-MSG04     TO MENSAGO
              END-IF
           END-IF

           PERFORM 0510-ENVIAR-MAPA.

      *----------------------------------------------------------------*
       0800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0900-TRATAR-PF8                 SECTION.
      *----------------------------------------------------------------*

           IF WRK-COMM-PAG             EQUAL ZEROS
              MOVE    WRK-MSG03        TO MENSAGO

           ELSE
              IF WRK-COMM-PAG          EQUAL WRK-COMM-ULT-PAG
                 PERFORM 0500-DESPR-SELECAO
                 MOVE WRK-MSG06        TO MENSAGO

              ELSE
                 ADD     1             TO WRK-COMM-PAG
                 PERFORM 0480-LER-TS
                 PERFORM 0490-MONTAR-OCORR
                 MOVE    WRK-MSG04     TO MENSAGO
              END-IF
           END-IF

           PERFORM 0510-ENVIAR-MAPA.

      *----------------------------------------------------------------*
       0900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-TRATAR-ENTER               SECTION.
      *----------------------------------------------------------------*

           IF WRK-COMM-PAG             EQUAL ZEROS
              MOVE    WRK-MSG03        TO MENSAGO
              PERFORM 0510-ENVIAR-MAPA
           END-IF

           MOVE    ZEROS               TO WRK-AC-SEL

           PERFORM VARYING WRK-IND     FROM 1 BY 1
                     UNTIL WRK-IND     GREATER 14

                IF WRK-SELECI(WRK-IND) EQUAL 'X'
                   ADD     1           TO WRK-AC-SEL
                   MOVE    WRK-IND     TO WRK-SEL
                   MOVE   'A'          TO WRK-SELECA(WRK-IND)

                ELSE
                   IF WRK-SELECI(WRK-IND)
                                       NOT EQUAL SPACES AND LOW-VALUES
                      MOVE   -1        TO WRK-SELECL(WRK-IND)
                      MOVE   'I'       TO WRK-SELECA(WRK-IND)
                      MOVE   '*'       TO WRK-INCONSISTENTE

                   ELSE
                      IF WRK-CODIGI(WRK-IND)
                                       NOT EQUAL SPACES AND LOW-VALUES
                         MOVE   'A'    TO WRK-SELECA(WRK-IND)
                      END-IF
                   END-IF
                END-IF

           END-PERFORM.

           IF WRK-INCONSISTENTE        NOT EQUAL SPACES
              MOVE    WRK-MSG04        TO MENSAGO
              PERFORM 0510-ENVIAR-MAPA

           ELSE
              IF WRK-AC-SEL            EQUAL ZEROS
                 MOVE   -1             TO WRK-SELECL(1)
                 MOVE    WRK-MSG04     TO MENSAGO
                 PERFORM 0510-ENVIAR-MAPA
              END-IF
           END-IF.

           IF WRK-AC-SEL               GREATER 1
              PERFORM VARYING WRK-IND  FROM 1 BY 1
                        UNTIL WRK-IND  GREATER 14

                   IF WRK-SELECI(WRK-IND)  EQUAL 'X'
                      MOVE   -1        TO WRK-SELECL(WRK-IND)
                      MOVE   'I'       TO WRK-SELECA(WRK-IND)
                      MOVE   '*'       TO WRK-INCONSISTENTE

                   ELSE
                      IF WRK-CODIGI(WRK-IND)
                                       NOT EQUAL SPACES AND LOW-VALUES
                         MOVE   'A'    TO WRK-SELECA(WRK-IND)
                      END-IF
                   END-IF
              END-PERFORM
           END-IF

           IF WRK-INCONSISTENTE        EQUAL SPACES
              PERFORM VARYING WRK-IND  FROM 1 BY 1
                      UNTIL   WRK-SELECI(WRK-IND)
                                       EQUAL 'X'
              END-PERFORM

              MOVE    WRK-CODIGI-R(WRK-IND)
                                       TO WRK-COMM-TIPOEMP-1816
              MOVE    WRK-DESCRI(WRK-IND)
                                       TO WRK-COMM-DESCR-EMP

              MOVE    13               TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE

              EXEC CICS
                   XCTL PROGRAM ('BVVE1979')
                        COMMAREA(WRK-COMMAREA)
              END-EXEC

              IF EIBRESP               NOT EQUAL DFHRESP(NORMAL)
                 PERFORM 9998-TRATAR-ERRO
              END-IF

           ELSE
              MOVE    WRK-MSG04        TO MENSAGO
              PERFORM 0510-ENVIAR-MAPA
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1100-TRATAR-PF-INVALIDA         SECTION.
      *----------------------------------------------------------------*

           IF WRK-COMM-PAG    NOT EQUAL ZEROS
              PERFORM 0500-DESPR-SELECAO
           END-IF

           MOVE    WRK-MSG01           TO MENSAGO
           PERFORM 0510-ENVIAR-MAPA.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
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

           MOVE   '1'                  TO WRK-COMM-FASE

           MOVE   -1                   TO WRK-SELECL(1)
           PERFORM 0510-ENVIAR-MAPA.

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
