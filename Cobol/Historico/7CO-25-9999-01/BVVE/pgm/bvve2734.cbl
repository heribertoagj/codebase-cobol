      *-===============================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. BVVE2734.
       AUTHOR.     RANGEL VICTOR SANTOS
      *================================================================*
      *         C D I - CENTRO DE DESENVOLVIMENTO INTERNO DDS          *
      *----------------------------------------------------------------*
      *    PROGRAMA.....: BVVE2734                                     *
      *    PROGRAMADOR..: RANGEL VICTOR SANTOS   - CDI DDS             *
      *    DATA.........: 09/2018                                      *
      *----------------------------------------------------------------*
      *    OBJETIVO     : EXCLUIR - RELAC. CREDENCIADORA PERFIL        *
      *----------------------------------------------------------------*
      *    MAPA/MAPSET  :                                              *
      *      BVVETP5 - EXCLUIR RELAC. CREDENCIADORA PERFIL             *
      *----------------------------------------------------------------*
      *    TRANSACAO    : VV0A                                         *
      *----------------------------------------------------------------*
      *    NAVEGACAO    :                                              *
      *      CHAMADO POR: BVVE2730              CHAMA: BVVEO041        *
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
           '*  INICIO DA WORKING BVVE2734  *'.
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
               'VV0A - FIM DA TRANSACAO'.

           03  WRK-MSG04               PIC  X(79)        VALUE
               'ACESSO PERMITIDO SOMENTE PELO SENHAS'.

           03  WRK-MSG06               PIC  X(79)        VALUE
               'TECLA <PF6> PARA CONFIRMAR A EXCLUSAO DOS DADOS'.

           03  WRK-MSG11               PIC  X(79)        VALUE
               'EXCLUSAO EFETUADA COM SUCESSO'.

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
               05  WRK-CGC-REC         PIC  X(09).
               05  WRK-FILIAL-REC      PIC  X(04).
               05  FILLER              PIC  X(01).
               05  WRK-CONTROLE-REC    PIC  9(02).
           03  WRK-TERMINAL-REC        PIC  X(08)        VALUE SPACES.
           03  WRK-IMPRESSORA-REC      PIC  X(08)        VALUE SPACES.
           03  WRK-COD-USR-REC         PIC  X(07)        VALUE SPACES.
           03  FILLER                  PIC  X(209)       VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO  ***'.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO  ***'.
      *----------------------------------------------------------------*

       01  WRK-COMMAREA.
           03  WRK-CA-FASE             PIC  9(001)     VALUE ZERO.
           03  WRK-CA-FUNCAO           PIC  X(001)     VALUE SPACES.
           03  WRK-CA-CONT             PIC  X(001)     VALUE SPACE.
           03  WRK-CA-PAG              PIC  X(001)     VALUE SPACE.
           03  WRK-CA-ESTAB            PIC  9(004)     VALUE ZEROS.
           03  WRK-CA-PERFIL           PIC  9(001)     VALUE ZERO.
           03  WRK-CA-SEQ              PIC  9(010)     VALUE ZEROS.
           03  WRK-CA-INTERFASE        PIC  9(001)     VALUE ZERO.
           03  WRK-CA-ANT-ESTBL-COMLI  PIC  9(004)     VALUE ZERO.
           03  WRK-CA-ANT-NPRFILI      PIC  9(001)     VALUE ZERO.
           03  WRK-CA-ANT-SEQI         PIC  9(010)     VALUE ZEROS.
           03  WRK-CA-ANT-ESTBL-COMLF  PIC  9(004)     VALUE ZERO.
           03  WRK-CA-ANT-NPRFILF      PIC  9(001)     VALUE ZERO.
           03  WRK-CA-ANT-SEQF         PIC  9(010)     VALUE ZEROS.
           03  WRK-CA-ESTBL-COMLI      PIC  9(004)     VALUE ZERO.
           03  WRK-CA-NPRFILI          PIC  9(001)     VALUE ZERO.
           03  WRK-CA-SEQI             PIC  9(010)     VALUE ZEROS.
           03  WRK-CA-ESTBL-COMLF      PIC  9(004)     VALUE ZERO.
           03  WRK-CA-NPRFILF          PIC  9(001)     VALUE ZERO.
           03  WRK-CA-SEQF             PIC  9(010)     VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA TECLAS CICS  ***'.
      *----------------------------------------------------------------*

           COPY 'DFHAID'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*     AREA DO MAPA BVVETP5     *'.
      *----------------------------------------------------------------*

       COPY BVVETP5.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DO MODULO BVVEO040  ***'.
      *----------------------------------------------------------------*

       01  WRK-AREA-BVVEO040.
           COPY BVVEW000.
           COPY BVVEW40I.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DO MODULO BVVEO041  ***'.
      *----------------------------------------------------------------*

       01  WRK-AREA-BVVEO041.
           COPY BVVEW000.
           COPY BVVEW41I.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*   FIM DA WORKING BVVE2734   *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*
       01  DFHCOMMAREA.
           03  FILLER                  PIC X(01) OCCURS 10000
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
              EIBTRNID                 NOT EQUAL 'VV0A'
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
               WHEN  DFHPF6
                     PERFORM 1500-TRATAR-PF6
               WHEN  DFHENTER
                     PERFORM 1600-TRATAR-ENTER
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
              IF EIBTRNID              NOT EQUAL 'VV0A'
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
                XCTL PROGRAM ('BVVE2730')
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
           MOVE 1                      TO WRK-CA-INTERFASE.

           INITIALIZE BVVEW000-BLOCO-RETORNO OF WRK-AREA-BVVEO041
                      BVVEW41I-ENTRADA
                      BVVEW41I-SAIDA.

           INITIALIZE BVVEW000-BLOCO-RETORNO OF WRK-AREA-BVVEO040
                      BVVEW40I-ENTRADA
                      BVVEW40I-SAIDA.

           MOVE 'D'                    TO BVVEW41I-E-C-ATIVIDADE
           MOVE '001'                  TO BVVEW41I-E-C-FUNCAO
           MOVE WRK-CA-ESTAB           TO BVVEW41I-E-CCRDRA-ESTBL-COML
           MOVE WRK-CA-PERFIL          TO BVVEW41I-E-NPRFIL-CRDRA-PGTO
           MOVE ZEROS                  TO BVVEW41I-E-FASE

           PERFORM 1405-ACESSAR-BVVEO041.

           IF  BVVEW000-COD-RETORNO OF WRK-AREA-BVVEO041 NOT EQUAL ZEROS
               MOVE BVVEW41I-COD-MENSAGEM
                                       TO MSGO
           ELSE
               PERFORM 1406-ACESSA-BVVEO040
               PERFORM 1410-MONTA-TELA
               MOVE WRK-MSG06          TO MSGO
           END-IF.

           PERFORM 1450-ENVIAR-MAPA.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1405-ACESSAR-BVVEO041           SECTION.
      *----------------------------------------------------------------*

           MOVE 'BVVEO041'             TO WRK-MODULO

           EXEC CICS
                LINK PROGRAM (WRK-MODULO)
                     COMMAREA(WRK-AREA-BVVEO041)
                     LENGTH  (LENGTH OF WRK-AREA-BVVEO041)
           END-EXEC.

           IF  EIBRESP                 NOT EQUAL DFHRESP(NORMAL)
               PERFORM 9998-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1405-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1406-ACESSA-BVVEO040            SECTION.
      *----------------------------------------------------------------*

           MOVE 'C'                    TO BVVEW40I-E-C-ATIVIDADE
           MOVE '001'                  TO BVVEW40I-E-C-FUNCAO
           MOVE WRK-CA-ESTAB           TO BVVEW40I-E-CCRDRA-ESTBL-COML
           MOVE ZEROS                  TO BVVEW41I-E-FASE
           MOVE 'BVVEO040'             TO WRK-MODULO

           EXEC CICS
                LINK PROGRAM (WRK-MODULO)
                     COMMAREA(WRK-AREA-BVVEO040)
                     LENGTH  (LENGTH OF WRK-AREA-BVVEO040)
           END-EXEC.

           IF  EIBRESP                 NOT EQUAL DFHRESP(NORMAL)
               PERFORM 9998-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1406-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1410-MONTA-TELA                 SECTION.
      *----------------------------------------------------------------*

           MOVE BVVEW41I-S-CCRDRA-ESTBL-COML(2:3)
                                       TO CCREO.
0511       MOVE BVVEW40I-S-IREDZD-CRDRA-ESTBL(1:15)
0511                                   TO NABRO.
7C2511*    MOVE BVVEW40I-S-CCNPJ-CRDRA-ESTBL(2:8)
7C2511*                                   TO CNP1O.
7C2511*    MOVE BVVEW40I-S-CFLIAL-CRDRA-ESTBL
7C2511*                                   TO CNP2O.
7C2511*    MOVE BVVEW40I-S-CCTRL-CRDRA-ESTBL
7C2511*                                   TO CNP3O.
           MOVE BVVEW40I-S-CCNPJ-CRDRA-ESTBL-ST(2:8)
                                       TO CNP1O.
           MOVE BVVEW40I-S-CFLIAL-CRDRA-ESTBL-ST
                                       TO CNP2O.
           MOVE BVVEW40I-S-CCTRL-CRDRA-ESTBL-ST
                                       TO CNP3O.
           MOVE BVVEW41I-S-NPRFIL-CRDRA-PGTO
                                       TO NPERO.
           MOVE BVVEW41I-S-NSEQ-CONTR-NEGOC(3:8)
                                       TO CPERO.
           MOVE BVVEW41I-S-CAG-BCRIA-CLI
                                       TO CAGAO.
           MOVE BVVEW41I-S-NCTA-BCRIA(6:7)
                                       TO CCTAO.

           IF   BVVEW41I-S-HINCL-REG   EQUAL SPACES
                                       OR   '0001-01-01-01.01.01.000001'
                MOVE SPACES            TO CDTIO
           ELSE
                MOVE BVVEW41I-S-HINCL-REG(1:4)
                                       TO CDTIO(7:4)
                MOVE BVVEW41I-S-HINCL-REG(6:2)
                                       TO CDTIO(4:2)
                MOVE BVVEW41I-S-HINCL-REG(9:2)
                                       TO CDTIO(1:2)
                MOVE '/'               TO CDTIO(3:1)
                                          CDTIO(6:1)
           END-IF

           MOVE BVVEW41I-S-CUSUAR-INCL TO CUSIO.

           IF   BVVEW41I-S-HMANUT-REG  EQUAL SPACES
                                       OR   '0001-01-01-01.01.01.000001'
                MOVE SPACES            TO CDTAO
           ELSE
                MOVE BVVEW41I-S-HMANUT-REG(1:4)
                                       TO CDTAO(7:4)
                MOVE BVVEW41I-S-HMANUT-REG(6:2)
                                       TO CDTAO(4:2)
                MOVE BVVEW41I-S-HMANUT-REG(9:2)
                                       TO CDTAO(1:2)
                MOVE '/'               TO CDTAO(3:1)
                                          CDTAO(6:1)
           END-IF

           MOVE BVVEW41I-S-CUSUAR-MANUT
                                       TO CUSAO.

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
                SEND MAPSET ('BVVETP5')
                     MAP    ('BVVETP5')
                     FROM   (BVVETP5O)
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
                   RETURN TRANSID ('VV0A')
                          COMMAREA(WRK-COMMAREA)
              END-EXEC
           END-IF.

      *----------------------------------------------------------------*
       1450-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1500-TRATAR-PF6                 SECTION.
      *----------------------------------------------------------------*

           IF  WRK-CA-INTERFASE        EQUAL 1
               PERFORM 1800-TRATAR-PF-INVALIDA
           END-IF.

           INITIALIZE BVVEW000-BLOCO-RETORNO OF WRK-AREA-BVVEO041
                      BVVEW41I-ENTRADA
                      BVVEW41I-SAIDA.

           MOVE CCREI                  TO BVVEW41I-E-CCRDRA-ESTBL-COML
           MOVE NPERI                  TO BVVEW41I-E-NPRFIL-CRDRA-PGTO
           MOVE 'E'                    TO BVVEW41I-E-C-ATIVIDADE
           MOVE '001'                  TO BVVEW41I-E-C-FUNCAO
           MOVE ZEROS                  TO BVVEW41I-E-FASE

           PERFORM 1405-ACESSAR-BVVEO041.

           IF  BVVEW000-COD-RETORNO OF WRK-AREA-BVVEO041 EQUAL 55
               MOVE WRK-MSG11          TO MSGO
           ELSE
               MOVE BVVEW41I-COD-MENSAGEM
                                       TO MSGO
           END-IF.

           PERFORM 1450-ENVIAR-MAPA.

      *----------------------------------------------------------------*
       1500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1600-TRATAR-ENTER SECTION.
      *----------------------------------------------------------------*

           PERFORM 1450-ENVIAR-MAPA.

      *----------------------------------------------------------------*
       1600-99-FIM. EXIT.
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
                RECEIVE MAPSET ('BVVETP5')
                        MAP    ('BVVETP5')
                        INTO   (BVVETP5I)
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

           MOVE ZEROS                  TO WRK-CA-INTERFASE

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
