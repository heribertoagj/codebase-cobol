      *================================================================*
       IDENTIFICATION                  DIVISION.
      *-===============================================================*
       PROGRAM-ID. BVVE2741.
       AUTHOR.     FABIO SOARES CARREIRO.
      *================================================================*
      *         C D I - CENTRO DE DESENVOLVIMENTO INTERNO DDS          *
      *----------------------------------------------------------------*
      *    PROGRAMA.....: BVVE2741                                     *
      *    PROGRAMADORA.: FABIO SOARES CARREIRO  - CDI DDS             *
      *    DATA.........: SETEMBRO / 2018                              *
      *----------------------------------------------------------------*
      *    OBJETIVO     : CONSULTAR - DETALHAR RELACAO DE LANCAMENTOS  *
      *                   DE CONTA CORRENTE.                           *
      *----------------------------------------------------------------*
      *    MAPA/MAPSET  :                                              *
      *      BVVETL2 - DETALHAR LANCAMENTOS DE CONTA CORRENTE.         *
      *----------------------------------------------------------------*
      *    TRANSACAO    : VV0B                                         *
      *----------------------------------------------------------------*
      *    NAVEGACAO    :                                              *
      *      CHAMADO POR: BVVE2740              CHAMA: ---             *
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
           '*  INICIO DA WORKING BVVE2700  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)        VALUE
           '*     VARIAVEIS AUXILIARES     *'.
      *----------------------------------------------------------------*
       77  WRK-MODULO                  PIC  X(08)        VALUE SPACES.

       01  WRK-AUXILIARES.
           03  WRK-ABSTIME             PIC S9(15) COMP-3 VALUE ZEROS.
           03  WRK-PROGRAMA            PIC  X(08)        VALUE SPACES.
           03  WRK-INCONSISTENTE       PIC  X(01)        VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*      AREA DE MENSAGENS       *'.
      *----------------------------------------------------------------*
       01  WRK-MENSAGENS.
           03  WRK-MSG01               PIC  X(79)        VALUE
               'PF INVALIDA'.

           03  WRK-MSG03               PIC  X(79)        VALUE
               'VV0B - FIM DA TRANSACAO'.

           03  WRK-MSG04               PIC  X(79)        VALUE
               'ACESSO PERMITIDO SOMENTE PELO SENHAS'.

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
           '*  AREA COMUNICACAO SENHAS     *'.
      *----------------------------------------------------------------*
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
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO  ***'.
      *----------------------------------------------------------------*
       01  WRK-COMMAREA.
           03  WRK-CA-FASE             PIC  9(001)     VALUE ZERO.
           03  WRK-CA-FUNCAO           PIC  X(001)     VALUE SPACES.
           03  WRK-CA-CONT             PIC  X(001)     VALUE SPACE.
           03  WRK-CA-PAG              PIC  X(001)     VALUE SPACE.
           03  WRK-CA-CRE              PIC  9(004)     VALUE ZEROS.
           03  WRK-CA-ANJO             PIC  9(003)     VALUE ZEROS.
           03  WRK-CA-NR               PIC  9(001)     VALUE ZEROS.
           03  WRK-CA-LCTO             PIC  9(005)     VALUE ZEROS.
MY1018     03  WRK-CA-ANT-ESTBL-COMLI  PIC  9(004)     VALUE ZEROS.
MY1018     03  WRK-CA-ANT-CARRJO-PGTOI PIC  9(003)     VALUE ZEROS.
MY1018     03  WRK-CA-ANT-NPRFILI      PIC  9(001)     VALUE ZEROS.
MY1018     03  WRK-CA-ANT-ESTBL-COMLF  PIC  9(004)     VALUE ZEROS.
MY1018     03  WRK-CA-ANT-CARRJO-PGTOF PIC  9(003)     VALUE ZEROS.
MY1018     03  WRK-CA-ANT-NPRFILF      PIC  9(001)     VALUE ZEROS.
MY1018     03  WRK-CA-ESTBL-COMLI      PIC  9(004)     VALUE ZEROS.
MY1018     03  WRK-CA-CARRJO-PGTOI     PIC  9(003)     VALUE ZEROS.
MY1018     03  WRK-CA-NPRFILI          PIC  9(001)     VALUE ZEROS.
MY1018     03  WRK-CA-ESTBL-COMLF      PIC  9(004)     VALUE ZEROS.
MY1018     03  WRK-CA-CARRJO-PGTOF     PIC  9(003)     VALUE ZEROS.
MY1018     03  WRK-CA-NPRFILF          PIC  9(001)     VALUE ZEROS.

      *01  WRK-COMMAREA.
      *    03  WRK-CA-FASE             PIC  9(001)     VALUE ZERO.
      *    03  WRK-CA-FUNCAO           PIC  X(001)     VALUE SPACES.
      *    03  WRK-CA-PROGRAMA         PIC  X(008)     VALUE SPACES.
      *    03  WRK-CA-PAG              PIC  9(002)     VALUE ZERO.
      *    03  WRK-CA-CONT             PIC  X(001)     VALUE SPACE.
      *    03  WRK-CA-CRED             PIC  9(004)     VALUE ZEROS.
      *    03  WRK-CA-CREDA            PIC  9(004)     VALUE ZEROS.
      *    03  WRK-CA-CREDU            PIC  9(004)     VALUE ZEROS.
      *    03  WRK-CA-CARRJO           PIC  9(003)     VALUE ZEROS.
      *    03  WRK-CA-NPRFIL           PIC  9(001)     VALUE ZEROS.
      *    03  FILLER                  PIC  X(021)     VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA TECLAS CICS  ***'.
      *----------------------------------------------------------------*

       COPY 'DFHAID'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*     AREA DO MAPA BVVETL2     *'.
      *----------------------------------------------------------------*

       COPY BVVETL2.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DO MODULO BVVEO039  ***'.
      *----------------------------------------------------------------*

       01  WRK-AREA-BVVEO039.
           COPY BVVEW000.
           COPY BVVEW39I.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*   FIM DA WORKING BVVE2741   *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*
       01  DFHCOMMAREA.
           03  FILLER                  PIC X(01) OCCURS 1000
                                             DEPENDING ON EIBCALEN.

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

           INITIALIZE WRK-AUXILIARES.

           IF EIBCALEN                 EQUAL ZEROS  OR
              EIBTRNID                 NOT EQUAL 'VV0B'
              PERFORM 1400-RECEBER-CONTROLE
           ELSE
              PERFORM 2000-RECEBER-MAPA
           END-IF.

           MOVE    02                  TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE

           EVALUATE  EIBAID
               WHEN  DFHPF2
                     PERFORM 1100-TRATAR-PF2
               WHEN  DFHPF3
                     PERFORM 1200-TRATAR-PF3
               WHEN  DFHPF5
                     PERFORM 1300-TRATAR-PF5
               WHEN  DFHCLEAR
                     PERFORM 9000-ENCERRAR-PGM
               WHEN  OTHER
                     PERFORM 1800-TRATAR-PF-INVALIDA
           END-EVALUATE.

           IF EIBRESP                  NOT EQUAL DFHRESP(NORMAL)
              PERFORM 9998-TRATAR-ERRO
           END-IF

           IF EIBCALEN                 EQUAL ZEROS
              PERFORM 1900-EXECUTAR-RETRIEVE
           ELSE
              IF EIBTRNID              NOT EQUAL 'VV0B'
                 MOVE    WRK-MSG04     TO WRK-MSG03
                 PERFORM 9000-ENCERRAR-PGM
              ELSE
                 MOVE DFHCOMMAREA(1:EIBCALEN)
                                       TO WRK-COMMAREA
              END-IF
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

           MOVE 04                     TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE

           EXEC CICS
                XCTL PROGRAM ('BVVE2740')
                     COMMAREA(WRK-COMMAREA)
           END-EXEC

           IF  EIBRESP                 NOT EQUAL DFHRESP(NORMAL)
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

           MOVE    04                  TO WRK-LOCAL-EIBRESP
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
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1400-RECEBER-CONTROLE           SECTION.
      *----------------------------------------------------------------*

           MOVE DFHCOMMAREA(1:EIBCALEN)
                                       TO WRK-COMMAREA.

           MOVE 'C'                    TO BVVEW39I-E-C-ATIVIDADE
           MOVE '001'                  TO BVVEW39I-E-C-FUNCAO

           INITIALIZE BVVEW000-BLOCO-RETORNO
                      BVVEW39I-ENTRADA
                      BVVEW39I-SAIDA.

           MOVE WRK-CA-CRE             TO BVVEW39I-E-CCRDRA-ESTBL-COML
           MOVE WRK-CA-ANJO            TO BVVEW39I-E-CARRJO-PGTO
           MOVE WRK-CA-NR              TO BVVEW39I-E-NPRFIL-CRDRA-PGTO
           MOVE 'BVVEO039'             TO WRK-MODULO

           EXEC CICS
                LINK PROGRAM (WRK-MODULO)
                     COMMAREA(WRK-AREA-BVVEO039)
                     LENGTH  (LENGTH OF WRK-AREA-BVVEO039)
           END-EXEC.

           IF  EIBRESP                 NOT EQUAL DFHRESP(NORMAL)
               PERFORM 9998-TRATAR-ERRO
           END-IF.

           IF  BVVEW000-COD-RETORNO    NOT EQUAL ZEROS
               MOVE BVVEW39I-S-C-MENSAGEM
                                       TO MSGO
           ELSE
               PERFORM 1410-MONTA-TELA
           END-IF.

           PERFORM 1450-ENVIAR-MAPA.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1410-MONTA-TELA                 SECTION.
      *----------------------------------------------------------------*

MY1118     MOVE BVVEW39I-S-CCRDRA-ESTBL-COML
                                       TO CCREO.
           MOVE BVVEW39I-S-IREDZD-CRDRA-ESTBL(1:15)
                                       TO NABRO.

           MOVE BVVEW39I-S-CARRJO-PGTO TO CARJO.
           MOVE BVVEW39I-S-IARRJO-PGTO TO NARJO.

           MOVE BVVEW39I-S-NPRFIL-CRDRA-PGTO
                                       TO NPERO.
           MOVE BVVEW39I-S-CIDTFD-LCTO TO CLCTO.

MY1118     IF BVVEW39I-S-HINCL-REG     NOT EQUAL
.                                         '0001-01-01-01.01.01.000001'
              MOVE BVVEW39I-S-HINCL-REG(1:4)
                                       TO CDTIO(7:4)
              MOVE BVVEW39I-S-HINCL-REG(6:2)
                                       TO CDTIO(4:2)
              MOVE BVVEW39I-S-HINCL-REG(9:2)
                                       TO CDTIO(1:2)
              MOVE '/'                 TO CDTIO(3:1)
                                          CDTIO(6:1)
              MOVE BVVEW39I-S-CUSUAR-INCL
                                       TO CUSIO
MY1118     END-IF

MY1118     IF BVVEW39I-S-HMANUT-REG    NOT EQUAL
.                                         '0001-01-01-01.01.01.000001'
              MOVE BVVEW39I-S-HMANUT-REG(1:4)
                                       TO CDTAO(7:4)
              MOVE BVVEW39I-S-HMANUT-REG(6:2)
                                       TO CDTAO(4:2)
              MOVE BVVEW39I-S-HMANUT-REG(9:2)
                                       TO CDTAO(1:2)
              MOVE '/'                 TO CDTAO(3:1)
                                          CDTAO(6:1)
              MOVE BVVEW39I-S-CUSUAR-MANUT
                                       TO CUSAO
MY1118     END-IF
           .
      *----------------------------------------------------------------*
       1410-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1450-ENVIAR-MAPA                SECTION.
      *----------------------------------------------------------------*

           EXEC CICS
                ASKTIME ABSTIME (WRK-ABSTIME)
           END-EXEC.

           EXEC CICS
                FORMATTIME ABSTIME  (WRK-ABSTIME)
                           DATESEP  ('/')
                           DDMMYYYY (DATAO)
                           TIMESEP  (':')
                           TIME     (HORAO)
           END-EXEC.

           MOVE    05                  TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE

           EXEC CICS
                SEND MAPSET ('BVVETL2')
                     MAP    ('BVVETL2')
                     FROM   (BVVETL2O)
                     CURSOR
                     ERASE
                     FREEKB
                     NOHANDLE
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
                   RETURN TRANSID ('VV0B')
                          COMMAREA(WRK-COMMAREA)
              END-EXEC
           END-IF.

      *----------------------------------------------------------------*
       1450-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1800-TRATAR-PF-INVALIDA         SECTION.
      *----------------------------------------------------------------*

           MOVE    WRK-MSG01           TO MSGO

           PERFORM 1450-ENVIAR-MAPA.

      *----------------------------------------------------------------*
       1800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1900-EXECUTAR-RETRIEVE          SECTION.
      *----------------------------------------------------------------*

           EXEC CICS
                RETRIEVE INTO   (WRK-AREA-SENHAS-REC)
                         LENGTH (LENGTH OF WRK-AREA-SENHAS-REC)
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

           MOVE    07                  TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE

           EXEC CICS
                RECEIVE MAPSET ('BVVETL2')
                        MAP    ('BVVETL2')
                        INTO   (BVVETL2I)
                        NOHANDLE
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

           MOVE    08                  TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE

           EXEC CICS
                SEND FROM(WRK-MSG03)
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
              MOVE    WRK-MSG99        TO MSGO
           ELSE
              PERFORM 9999-ERRO-ABCODE
              MOVE    WRK-MSG00        TO MSGO
           END-IF.

           MOVE ZEROS                  TO WRK-CA-FASE

           PERFORM 1450-ENVIAR-MAPA.

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
