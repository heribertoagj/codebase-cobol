      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. BVVE0251.
       AUTHOR.     FERNANDA RODRIGUES.
      *================================================================*
      *                  B S I - T E C N O L O G I A                   *
      *----------------------------------------------------------------*
      *    PROGRAMA.....: BVVE0251                                     *
      *    PROGRAMADORA.: FERNANDA RODRIGUES  -  BSI TECNOLOGIA        *
      *    DATA.........: JULHO/2014                                   *
      *----------------------------------------------------------------*
      *    OBJETIVO     : PARAMETRIZACAO DO PARECER DO VAREJO          *
      *                   OPCAO 29 DO CICS                             *
      *----------------------------------------------------------------*
      *    MAPA/MAPSET  : BVVE251                                      *
      *----------------------------------------------------------------*
      *    TRANSACAO    : VV85                                         *
      *----------------------------------------------------------------*
      *    ARQUIVOS VSAM: DDNAME                COPYBOOK               *
      *                  PARECPVJ               I#BVVEDV               *
      *----------------------------------------------------------------*
      *    NAVEGACAO    :                                              *
      *      CHAMADO POR: SENH8020              CHAMA: --------        *
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
       77  FILLER                      PIC  X(32)          VALUE
           '*  INICIO DA WORKING BVVE0251  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)          VALUE
           '*     INDEXADORES    *'.
      *----------------------------------------------------------------*
       01  WRK-INDEXADORES.
           05 IND-1                    PIC  9(003) COMP-3  VALUE ZEROS.
           05 IND-2                    PIC  9(003) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)          VALUE
           '*     VARIAVEIS AUXILIARES     *'.
      *----------------------------------------------------------------*
       01  WRK-AUXILIARES.
           05 WRK-ABSTIME              PIC S9(015) COMP-3  VALUE ZEROS.

       01  WRK-TIMESTAMP-AUX.
           05 WRK-TIMES-DATA.
              10 WRK-DATA-ANO          PIC  9(04)        VALUE ZEROS.
              10 WRK-DATA-MES          PIC  9(02)        VALUE ZEROS.
              10 WRK-DATA-DIA          PIC  9(02)        VALUE ZEROS.
           05 WRK-TIMES-HORA.
              10 WRK-HORA-HR           PIC  9(02)        VALUE ZEROS.
              10 WRK-HORA-MN           PIC  9(02)        VALUE ZEROS.
              10 WRK-HORA-SG           PIC  9(02)        VALUE ZEROS.
           05 FILLER                   PIC  X(06)        VALUE SPACES.

       01  WRK-DATA-TELA.
           05 WRK-DATA-DIA             PIC  9(02)        VALUE ZEROS.
           05 FILLER                   PIC  X(01)        VALUE '.'.
           05 WRK-DATA-MES             PIC  9(02)        VALUE ZEROS.
           05 FILLER                   PIC  X(01)        VALUE '.'.
           05 WRK-DATA-ANO             PIC  9(04)        VALUE ZEROS.

       01  WRK-HORA-TELA.
           05 WRK-HORA-HR              PIC  9(02)        VALUE ZEROS.
           05 FILLER                   PIC  X(01)        VALUE ':'.
           05 WRK-HORA-MN              PIC  9(02)        VALUE ZEROS.
           05 FILLER                   PIC  X(01)        VALUE ':'.
           05 WRK-HORA-SG              PIC  9(02)        VALUE ZEROS.

      * AREA PARA CLASSIFICACAO DAS OCORRENCIAS DA TELA ---------------*
       01  WRK-AREA-CLASSIFICACAO.
           05 WRK-QTD-OCORR            PIC  9(003) COMP-3  VALUE ZEROS.
           05 WRK-TAB-REGS-ASC     OCCURS 0 TO 999
                                   DEPENDING ON WRK-QTD-OCORR.
              10 WRK-TAB-TIMES-A       PIC  X(020).
              10 WRK-TAB-PAREC-A       PIC  X(001).
              10 WRK-TAB-USER-A        PIC  X(009).
              10 WRK-TAB-TERM-A        PIC  X(008).

           05 WRK-TAB-REGS-DES     OCCURS 0 TO 999
                                   DEPENDING ON WRK-QTD-OCORR.
              10 WRK-TAB-TIMES-D       PIC  X(020).
              10 WRK-TAB-PAREC-D       PIC  X(001).
              10 WRK-TAB-USER-D        PIC  X(009).
              10 WRK-TAB-TERM-D        PIC  X(008).

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)          VALUE
           '*     AREA DA BRAD7600      *'.
      *----------------------------------------------------------------*
       01  WRK-7600-DATA-HORA.
           05 WRK-7600-DT-JULIANA      PIC  9(005) COMP-3  VALUE ZEROS.
           05 WRK-7600-DT-AAMMDD       PIC  9(007) COMP-3  VALUE ZEROS.
           05 WRK-7600-DT-AAAAMMDD     PIC  9(009) COMP-3  VALUE ZEROS.
           05 WRK-7600-TI-HHMMSS       PIC  9(007) COMP-3  VALUE ZEROS.
           05 WRK-7600-TI-HHMMSSMMMMMM PIC  9(013) COMP-3  VALUE ZEROS.
           05 WRK-7600-TIMESTAMP.
              10 WRK-7600-ANO          PIC  9(004)         VALUE ZEROS.
              10 WRK-7600-MES          PIC  9(002)         VALUE ZEROS.
              10 WRK-7600-DIA          PIC  9(002)         VALUE ZEROS.
              10 WRK-7600-HORA         PIC  9(002)         VALUE ZEROS.
              10 WRK-7600-MIN          PIC  9(002)         VALUE ZEROS.
              10 WRK-7600-SEG          PIC  9(002)         VALUE ZEROS.
              10 WRK-7600-MSEG         PIC  9(006)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*  AREA PARA FILA TS   *'.
      *----------------------------------------------------------------*
       01  WRK-ITEM-TS                 PIC S9(004) COMP    VALUE ZEROS.

       01  WRK-NOME-TS.
           05 WRK-TERMID               PIC  X(004)         VALUE SPACES.
           05 WRK-TRANID               PIC  X(004)         VALUE SPACES.

       01  WRK-AREA-TS.
           05 WRK-DADOS-PARECER    OCCURS 14.
              10 WRK-TS-TIMESTAMP      PIC  X(020)         VALUE SPACES.
              10 WRK-TS-PARECER        PIC  X(030)         VALUE SPACES.
              10 WRK-TS-USUARIO        PIC  X(009)         VALUE SPACES.
              10 WRK-TS-TERMINAL       PIC  X(008)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*      AREA DE MENSAGENS       *'.
      *----------------------------------------------------------------*
       01  WRK-MENSAGENS.
           03  WRK-MSG01               PIC  X(79)          VALUE
               'PF INVALIDA'.

           03  WRK-MSG02               PIC  X(79)          VALUE
               'INFORME O PARECER E TECLE <ENTER>'.

           03  WRK-MSG03               PIC  X(79)          VALUE
               'VV85 - FIM DA TRANSACAO'.

           03  WRK-MSG04               PIC  X(79)          VALUE
               'ACESSO PERMITIDO SOMENTE PELO SENHAS'.

           03  WRK-MSG05               PIC  X(79)          VALUE
               'PARECER INVALIDO, DIGITE S OU N'.

           03  WRK-MSG06               PIC  X(79)          VALUE
               'PARECER DEVE SER DIFERENTE DO PARECER CADASTRADO ANTERIO
      -        'RMENTE'.

           03  WRK-MSG07               PIC  X(79)          VALUE
               'TECLE <PF6> PARA CONFIRMAR OU <ENTER> PARA CANCELAR'.

           03  WRK-MSG08               PIC  X(79)          VALUE
               'NAO HA PAGINAS PARA RETORNAR'.

           03  WRK-MSG09               PIC  X(79)          VALUE
               'NAO HA PAGINAS PARA AVANCAR'.

           03  WRK-MSG10               PIC  X(79)          VALUE
               'PARECER INCLUIDO COM SUCESSO!'.

           03  WRK-MSG99.
               05  FILLER              PIC  X(35)          VALUE
                   'ERRO NAO IDENTIF PELO PGM. EIBRESP='.
               05  WRK-EIBRESP         PIC  9(04)          VALUE ZEROS.
               05  FILLER              PIC  X(09)          VALUE
                   ' E LOCAL='.
               05  WRK-LOCAL-EIBRESP   PIC  9(02)          VALUE ZEROS.
               05  FILLER              PIC  X(20)          VALUE
                   '. CONTACTAR ANALISTA'.

           03  WRK-MSG00.
               05  FILLER              PIC  X(19)          VALUE
                   'ERRO CICS - ABCODE='.
               05  WRK-ABCODE-MSG      PIC  X(08)          VALUE SPACES.
               05  FILLER              PIC  X(09)          VALUE
                   ' E LOCAL='.
               05  WRK-LOCAL-ABCODE    PIC  9(02)          VALUE ZEROS.
               05  FILLER              PIC  X(30)          VALUE
                   '. CONTACTAR ANALISTA'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*  AREA COMUNICACAO SENH1005   *'.
      *----------------------------------------------------------------*
       01  WRK-AREA-SENH1005-PAS.
           03  WRK-VERSAO-PAS          PIC  9(02)          VALUE 01.
           03  WRK-MENSAGEM-PAS.
               05  WRK-COD-MSG-PAS     PIC  9(03)          VALUE ZEROS.
               05  FILLER              PIC  X(01)          VALUE SPACES.
               05  WRK-TEXTO-MSG-PAS   PIC  X(79)          VALUE SPACES.
           03  WRK-SISTEMA-PAS         PIC  X(04)          VALUE SPACES.
           03  WRK-NIVEL-PAS           PIC  X(01)          VALUE SPACES.
               88  WRK-PRINCIPAL       VALUE 'P'.
               88  WRK-SECUNDARIO      VALUE 'S'.
           03  WRK-ACAO-PAS            PIC  X(01)          VALUE SPACES.
               88  WRK-ENCERRA         VALUE 'E'.
               88  WRK-CONTINUA        VALUE 'C'.
               88  WRK-TRATA-ERRO      VALUE 'T'.
           03  WRK-LINHA-COMANDO-PAS   PIC  X(68)          VALUE SPACES.
           03  FILLER                  PIC  X(96)          VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*  AREA COMUNICACAO SENHAS     *'.
      *----------------------------------------------------------------*
       01  WRK-AREA-SENHAS-REC.
           03  WRK-SISTEMA-REC         PIC  X(04)          VALUE SPACES.
           03  WRK-FUNCAO-REC          PIC  9(02)          VALUE ZEROS.
           03  WRK-TIPO-USR-REC        PIC  X(01)          VALUE SPACES.
           03  WRK-FUNC-BRADESCO-REC.
               05  WRK-AGENCIA-DEPTO-REC
                                       PIC  9(04)          VALUE ZEROS.
               05  WRK-SECAO-REC       PIC  9(03)          VALUE ZEROS.
               05  FILLER              PIC  X(09)          VALUE SPACES.
           03  WRK-FUNC-BRADESCO-R REDEFINES WRK-FUNC-BRADESCO-REC.
               05  WRK-FUNC-REC        PIC  9(07).
               05  FILLER              PIC  X(09).
           03  WRK-USR-EXTERNO-REC REDEFINES WRK-FUNC-BRADESCO-REC.
7C2511*        05  WRK-CGC-REC         PIC  9(09).
7C2511         05  WRK-CGC-REC         PIC  X(09).
7C2511*        05  WRK-FILIAL-REC      PIC  9(05).
7C2511         05  WRK-FILIAL-REC      PIC  X(04).
7C2511*        05  WRK-CONTROLE-REC    PIC  9(02).
7C2511         05  WRK-CONTROLE-REC    PIC  X(02).
           03  WRK-TERMINAL-REC        PIC  X(08)          VALUE SPACES.
           03  WRK-IMPRESSORA-REC      PIC  X(08)          VALUE SPACES.
           03  WRK-COD-USR-REC         PIC  X(07)          VALUE SPACES.
           03  FILLER                  PIC  X(209)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*      COMMAREA AUXILIAR       *'.
      *----------------------------------------------------------------*
       01  WRK-COMMAREA.
           05 WRK-COMM-PADRAO.
              10 WRK-COMM-TRANSACAO    PIC  X(004)         VALUE SPACES.
              10 WRK-COMM-TERMINAL     PIC  X(008)         VALUE SPACES.
           05 WRK-COMM-0251.
              10 WRK-COMM-DEPTO        PIC  9(004)         VALUE ZEROS.
              10 WRK-COMM-FASE         PIC  X(001)         VALUE SPACES.
              10 WRK-COMM-PAGINA       PIC  9(005) COMP-3  VALUE ZEROS.
              10 WRK-COMM-ULT-PAG      PIC  9(005) COMP-3  VALUE ZEROS.
              10 WRK-COMM-TIMESTAMP    PIC  X(020)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*     AREA DO ARQVUIO PARECPVJ *'.
      *----------------------------------------------------------------*
       COPY I#BVVEDV.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*     AREA DO MAPA BVVE251     *'.
      *----------------------------------------------------------------*
       COPY BVVE251.
       01  WRK-BVVE251R    REDEFINES BVVE251I.
           05 FILLER                   PIC  X(034).
           05 WRK-DFHG001I OCCURS 15 TIMES.
              10 WRK-PARECL            PIC S9(004) COMP.
              10 WRK-PARECA            PIC  X(001).
              10 WRK-PARECI            PIC  X(001).
              10 WRK-DATAL             PIC S9(004) COMP.
              10 WRK-DATAA             PIC  X(001).
              10 WRK-DATAI             PIC  X(010).
              10 FILLER    REDEFINES WRK-DATAI.
                 15 WRK-DIAI           PIC  9(002).
                 15 FILLER             PIC  X(001).
                 15 WRK-MESI           PIC  9(002).
                 15 FILLER             PIC  X(001).
                 15 WRK-ANOI           PIC  9(004).
              10 WRK-HORAL             PIC S9(004) COMP.
              10 WRK-HORAA             PIC  X(001).
              10 WRK-HORAI             PIC  X(008).
              10 WRK-USERL             PIC S9(004) COMP.
              10 WRK-USERA             PIC  X(001).
              10 WRK-USERI             PIC  X(009).
              10 WRK-TERML             PIC S9(004) COMP.
              10 WRK-TERMA             PIC  X(001).
              10 WRK-TERMI             PIC  X(008).
           05 FILLER                  PIC  X(82).


      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*   FIM DA WORKING BVVE0251   *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  DFHCOMMAREA.
           03  FILLER                  PIC  X(007).

      *================================================================*
       PROCEDURE                       DIVISION USING DFHCOMMAREA.
      *================================================================*

      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           MOVE 01                     TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE

           EXEC CICS
                HANDLE ABEND
                       LABEL(9998-TRATAR-ERRO)
           END-EXEC.

           IF  EIBRESP                 NOT EQUAL DFHRESP(NORMAL)
               PERFORM 9998-TRATAR-ERRO
           END-IF

           MOVE 02                     TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE

           EXEC CICS
                HANDLE AID
                       PF2   (1100-TRATAR-PF2)
                       PF3   (1300-TRATAR-PF5)
                       PF5   (1300-TRATAR-PF5)
                       PF6   (3000-TRATAR-PF6)
                       PF7   (4000-TRATAR-PF7)
                       PF8   (5000-TRATAR-PF8)
                       CLEAR (9000-ENCERRAR-PGM)
                       ENTER (2000-TRATAR-ENTER)
                       ANYKEY(1400-TRATAR-PF-INVALIDA)
           END-EXEC.

           IF  EIBRESP                 NOT EQUAL DFHRESP(NORMAL)
               PERFORM 9998-TRATAR-ERRO
           END-IF.

           INITIALIZE WRK-INDEXADORES
                      WRK-AUXILIARES.

           MOVE EIBTRMID               TO WRK-TERMID
           MOVE 'VV85'                 TO WRK-TRANID

           IF  EIBCALEN                EQUAL ZEROS
               PERFORM 7000-EXECUTAR-RETRIEVE
           ELSE
              IF  EIBTRNID             NOT EQUAL 'VV85'
                  MOVE WRK-MSG04       TO WRK-MSG03
                  PERFORM 9000-ENCERRAR-PGM
              ELSE
                  MOVE DFHCOMMAREA(1:EIBCALEN)
                                       TO WRK-COMMAREA
              END-IF
           END-IF.

           IF  EIBCALEN                EQUAL ZEROS  OR
               EIBTRNID                NOT EQUAL 'VV85'
               PERFORM 1500-RECEBER-CONTROLE
           ELSE
               PERFORM 6000-RECEBER-MAPA
           END-IF.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1100-TRATAR-PF2                 SECTION.
      *----------------------------------------------------------------*

           MOVE 03                     TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE

           EXEC CICS
                ISSUE DISCONNECT
           END-EXEC.

           IF  EIBRESP                 NOT EQUAL DFHRESP(NORMAL)
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

           MOVE 'BVVE'                 TO WRK-SISTEMA-PAS
           MOVE 'P'                    TO WRK-NIVEL-PAS

           MOVE 04                     TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE

           EXEC CICS
                LINK PROGRAM ('SENH1005')
                     COMMAREA(WRK-AREA-SENH1005-PAS)
                     LENGTH  (LENGTH OF WRK-AREA-SENH1005-PAS)
           END-EXEC.

           IF  EIBRESP                 NOT EQUAL DFHRESP(NORMAL)
               PERFORM 9998-TRATAR-ERRO
           END-IF

           EXEC CICS
                RETURN
           END-EXEC.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1400-TRATAR-PF-INVALIDA         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MSG01              TO MENSAGO

           PERFORM 8000-ENVIAR-MAPA.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1500-RECEBER-CONTROLE           SECTION.
      *----------------------------------------------------------------*

           PERFORM 1520-MONTAR-TELA

           MOVE WRK-MSG02              TO MENSAGO
           PERFORM 8000-ENVIAR-MAPA.

      *----------------------------------------------------------------*
       1500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1520-MONTAR-TELA                SECTION.
      *----------------------------------------------------------------*

           MOVE LOW-VALUES             TO BVVE251I
           MOVE '1'                    TO WRK-COMM-FASE
           MOVE ZEROS                  TO WRK-COMM-PAGINA
                                          WRK-COMM-ULT-PAG.

           PERFORM 1540-DELETAR-TS

           PERFORM 1560-POSICIONAR-PARECPVJ

           IF  EIBRESP                 EQUAL DFHRESP(NORMAL)
               PERFORM 1580-LER-PARECPVJ

               IF  EIBRESP             EQUAL DFHRESP(NORMAL)
                   MOVE 1              TO WRK-COMM-PAGINA
                   PERFORM 1600-CLASS-OCORRENCIAS
                   PERFORM 1620-MONTAR-TS
                   PERFORM 1660-EXECUTAR-ENDBR
                   PERFORM 1680-LER-TS
                   PERFORM 1700-MONTAR-OCORRENCIAS
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       1520-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1540-DELETAR-TS                 SECTION.
      *----------------------------------------------------------------*

           MOVE 05                     TO WRK-LOCAL-EIBRESP
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
       1540-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1560-POSICIONAR-PARECPVJ        SECTION.
      *----------------------------------------------------------------*

           MOVE 06                     TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE

           MOVE LOW-VALUES             TO BVVEDV-CH-TIMESTAMP

           EXEC CICS
                STARTBR FILE      ('PARECPVJ')
                        RIDFLD    (BVVEDV-CHAVE)
                        KEYLENGTH (LENGTH OF BVVEDV-CHAVE)
                        GTEQ
                        NOHANDLE
           END-EXEC.

           IF  EIBRESP                 NOT EQUAL DFHRESP(NORMAL) AND
                                                 DFHRESP(NOTFND)
               PERFORM 9998-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1560-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1580-LER-PARECPVJ               SECTION.
      *----------------------------------------------------------------*

           MOVE 07                     TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE.

           EXEC CICS
               READNEXT DATASET ('PARECPVJ')
                        INTO    (BVVEDV-PARECPVJ)
                        RIDFLD  (BVVEDV-CHAVE)
                        KEYLENGTH (LENGTH OF BVVEDV-CHAVE)
                        NOHANDLE
           END-EXEC.

           IF  EIBRESP                 NOT EQUAL DFHRESP (NORMAL)  AND
                                                 DFHRESP(ENDFILE)
               PERFORM 9998-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1580-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1600-CLASS-OCORRENCIAS        SECTION.
      *----------------------------------------------------------------*

      * CLASSIFICACAO ASCENDENTE DE TODOS OS REGS GRAVADOS NO ARQUIVO
           PERFORM VARYING IND-1       FROM 1 BY 1
                     UNTIL EIBRESP     EQUAL DFHRESP(ENDFILE)
               ADD 1                       TO WRK-QTD-OCORR
               MOVE BVVEDV-CH-TIMESTAMP    TO WRK-TAB-TIMES-A(IND-1)
               MOVE BVVEDV-PARECER         TO WRK-TAB-PAREC-A(IND-1)
               MOVE BVVEDV-USUARIO         TO WRK-TAB-USER-A(IND-1)
               MOVE BVVEDV-TERMINAL        TO WRK-TAB-TERM-A(IND-1)

               PERFORM 1580-LER-PARECPVJ
           END-PERFORM


      * CLASSIFICACAO DECRESCENTE DE TODOS OS REGS GRAVADOS NO ARQUIVO
           MOVE 1                      TO IND-2

           PERFORM VARYING IND-1       FROM WRK-QTD-OCORR BY -1
                     UNTIL IND-1       LESS 1
               MOVE WRK-TAB-TIMES-A(IND-1) TO WRK-TAB-TIMES-D(IND-2)
               MOVE WRK-TAB-PAREC-A(IND-1) TO WRK-TAB-PAREC-D(IND-2)
               MOVE WRK-TAB-USER-A(IND-1)  TO WRK-TAB-USER-D(IND-2)
               MOVE WRK-TAB-TERM-A(IND-1)  TO WRK-TAB-TERM-D(IND-2)
               ADD 1                       TO IND-2
           END-PERFORM.
      *
      *----------------------------------------------------------------*
       1600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1620-MONTAR-TS                  SECTION.
      *----------------------------------------------------------------*

           MOVE 1                      TO IND-2

           PERFORM VARYING IND-1       FROM 1 BY 1
                     UNTIL IND-1       GREATER WRK-QTD-OCORR

      * MAXIMO DE OCORRENCIAS BLOQUEADAS - GRAVAR UMA TS POR PAG CHEIA
               IF  IND-2               GREATER 14
                   PERFORM 1640-GRAVAR-TS
                   MOVE 1              TO IND-2
               END-IF

               MOVE WRK-TAB-TIMES-D(IND-1)
                                       TO WRK-TS-TIMESTAMP(IND-2)
               MOVE WRK-TAB-PAREC-D(IND-1)
                                       TO WRK-TS-PARECER(IND-2)
               MOVE WRK-TAB-USER-D(IND-1)
                                       TO WRK-TS-USUARIO(IND-2)
               MOVE WRK-TAB-TERM-D(IND-1)
                                       TO WRK-TS-TERMINAL(IND-2)

               ADD 1                   TO IND-2
           END-PERFORM

      * MAXIMO DE OCORRENCIAS INCLUINDO A LINHA PARA INSERIR
           IF  IND-2                   NOT GREATER 15
               PERFORM 1640-GRAVAR-TS
           END-IF.

      *----------------------------------------------------------------*
       1620-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1640-GRAVAR-TS                  SECTION.
      *----------------------------------------------------------------*

           MOVE 08                     TO WRK-LOCAL-EIBRESP
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
       1640-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1660-EXECUTAR-ENDBR             SECTION.
      *----------------------------------------------------------------*

           MOVE 09                     TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE

           EXEC CICS
                ENDBR FILE ('PARECPVJ')
           END-EXEC

           IF  EIBRESP                 NOT EQUAL DFHRESP(NORMAL) AND
                                                 DFHRESP(NOTFND)
               PERFORM 9998-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1660-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1680-LER-TS                     SECTION.
      *----------------------------------------------------------------*

           MOVE 10                     TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE

           MOVE WRK-COMM-PAGINA        TO WRK-ITEM-TS

           EXEC CICS
                READQ TS QUEUE (WRK-NOME-TS)
                         INTO  (WRK-AREA-TS)
                         ITEM  (WRK-ITEM-TS)
           END-EXEC.

           IF  EIBRESP                 NOT EQUAL DFHRESP(NORMAL)
               PERFORM 9998-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1680-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1700-MONTAR-OCORRENCIAS         SECTION.
      *----------------------------------------------------------------*

           MOVE 2                      TO IND-2

           PERFORM VARYING IND-1       FROM 1 BY 1
                     UNTIL IND-1       GREATER 14
           OR WRK-TS-TIMESTAMP(IND-1)  EQUAL SPACES OR LOW-VALUES

              MOVE WRK-TS-PARECER(IND-1)   TO WRK-PARECI(IND-2)

              MOVE WRK-TS-TIMESTAMP(IND-1) TO WRK-TIMESTAMP-AUX

              MOVE CORR WRK-TIMES-DATA     TO WRK-DATA-TELA
              MOVE WRK-DATA-TELA           TO WRK-DATAI(IND-2)

              MOVE CORR WRK-TIMES-HORA     TO WRK-HORA-TELA
              MOVE WRK-HORA-TELA           TO WRK-HORAI(IND-2)

              MOVE WRK-TS-USUARIO(IND-1)   TO WRK-USERI(IND-2)
              MOVE WRK-TS-TERMINAL(IND-1)  TO WRK-TERMI(IND-2)

              ADD  1                       TO IND-2
           END-PERFORM.

           IF  IND-2                   NOT GREATER 15
               PERFORM VARYING IND-2   FROM IND-2 BY 1
                         UNTIL IND-2   GREATER 15
                   MOVE SPACES         TO WRK-PARECI(IND-2)
                                          WRK-DATAI(IND-2)
                                          WRK-HORAI(IND-2)
                                          WRK-USERI(IND-2)
                                          WRK-TERMI(IND-2)
               END-PERFORM
           END-IF.

      *----------------------------------------------------------------*
       1700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000-TRATAR-ENTER               SECTION.
      *----------------------------------------------------------------*

           IF  WRK-COMM-FASE           EQUAL '1'
               PERFORM 2010-CONSIS-FISICA
               PERFORM 2020-PREENCHER-BLOQUEADOS
               MOVE '/'                TO WRK-PARECA(1)
               MOVE '2'                TO WRK-COMM-FASE
               MOVE WRK-MSG07          TO MENSAGO
           ELSE
               MOVE 'A'                TO WRK-PARECA(1)
               MOVE '1'                TO WRK-COMM-FASE
               MOVE WRK-MSG02          TO MENSAGO
           END-IF.

           PERFORM 8000-ENVIAR-MAPA.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2010-CONSIS-FISICA              SECTION.
      *----------------------------------------------------------------*

           IF  WRK-PARECI(1)           EQUAL SPACES OR LOW-VALUES
               MOVE -1                 TO WRK-PARECL(1)
               MOVE 'I'                TO WRK-PARECA(1)
               MOVE WRK-MSG05          TO MENSAGO
               PERFORM 8000-ENVIAR-MAPA
           END-IF.

           IF  WRK-PARECI(1)           NOT EQUAL 'S' AND 'N'
               MOVE -1                 TO WRK-PARECL(1)
               MOVE 'I'                TO WRK-PARECA(1)
               MOVE WRK-MSG05          TO MENSAGO
               PERFORM 8000-ENVIAR-MAPA
           END-IF.

           IF  WRK-PARECI(1)           EQUAL WRK-PARECI(2)
               MOVE -1                 TO WRK-PARECL(1)
               MOVE 'I'                TO WRK-PARECA(1)
               MOVE WRK-MSG06          TO MENSAGO
               PERFORM 8000-ENVIAR-MAPA
           END-IF.

      *----------------------------------------------------------------*
       2010-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2020-PREENCHER-BLOQUEADOS       SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL7601'             USING WRK-7600-DATA-HORA.
           MOVE WRK-7600-TIMESTAMP     TO WRK-COMM-TIMESTAMP
                                          WRK-TIMESTAMP-AUX

           MOVE CORR WRK-TIMES-DATA    TO WRK-DATA-TELA
           MOVE WRK-DATA-TELA          TO WRK-DATAI(1)

           MOVE CORR WRK-TIMES-HORA    TO WRK-HORA-TELA
           MOVE WRK-HORA-TELA          TO WRK-HORAI(1)

           EXEC CICS
                ASSIGN USERID (WRK-USERI(1))
           END-EXEC

           MOVE WRK-COMM-TERMINAL      TO WRK-TERMI(1).

      *----------------------------------------------------------------*
       2020-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3000-TRATAR-PF6                 SECTION.
      *----------------------------------------------------------------*

           IF  WRK-COMM-FASE           EQUAL '1'
               PERFORM 1400-TRATAR-PF-INVALIDA
           END-IF.

           MOVE 11                     TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE

           MOVE WRK-COMM-TIMESTAMP     TO BVVEDV-CH-TIMESTAMP
           MOVE WRK-PARECI(1)          TO BVVEDV-PARECER
           MOVE WRK-USERI(1)           TO BVVEDV-USUARIO
           MOVE WRK-TERMI(1)           TO BVVEDV-TERMINAL

           EXEC CICS
                WRITE DATASET  ('PARECPVJ')
                      FROM     (BVVEDV-PARECPVJ)
                      RIDFLD   (BVVEDV-CHAVE)
                      NOHANDLE
           END-EXEC

           IF  EIBRESP                 NOT EQUAL DFHRESP(NORMAL)
               PERFORM 9998-TRATAR-ERRO
           END-IF.

           PERFORM 1520-MONTAR-TELA

           MOVE '1'                    TO WRK-COMM-FASE
           MOVE WRK-MSG10              TO MENSAGO
           PERFORM 8000-ENVIAR-MAPA.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4000-TRATAR-PF7                 SECTION.
      *----------------------------------------------------------------*

           IF  WRK-COMM-PAGINA         EQUAL ZEROS
               IF  WRK-COMM-FASE       EQUAL '1'
                   MOVE WRK-MSG02      TO MENSAGO
               ELSE
                   MOVE WRK-MSG07      TO MENSAGO
               END-IF

           ELSE
               IF  WRK-COMM-PAGINA     EQUAL 1
                   MOVE WRK-MSG08      TO MENSAGO

               ELSE
                   SUBTRACT 1          FROM WRK-COMM-PAGINA
                   PERFORM 1680-LER-TS
                   PERFORM 1700-MONTAR-OCORRENCIAS

                   IF  WRK-COMM-FASE   EQUAL '1'
                       MOVE WRK-MSG02  TO MENSAGO
                   ELSE
                       MOVE WRK-MSG07  TO MENSAGO
                   END-IF
               END-IF
           END-IF

           IF  WRK-COMM-FASE           EQUAL '2'
               MOVE '/'                TO WRK-PARECI(1)
           END-IF

           PERFORM 8000-ENVIAR-MAPA.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5000-TRATAR-PF8                 SECTION.
      *----------------------------------------------------------------*

           IF  WRK-COMM-PAGINA         EQUAL ZEROS
               IF  WRK-COMM-FASE       EQUAL '1'
                   MOVE WRK-MSG02      TO MENSAGO
               ELSE
                   MOVE WRK-MSG07      TO MENSAGO
               END-IF

           ELSE
               IF  WRK-COMM-PAGINA     EQUAL WRK-COMM-ULT-PAG
                   MOVE WRK-MSG09      TO MENSAGO

               ELSE
                   ADD 1               TO WRK-COMM-PAGINA
                   PERFORM 1680-LER-TS
                   PERFORM 1700-MONTAR-OCORRENCIAS

                   IF  WRK-COMM-FASE   EQUAL '1'
                       MOVE WRK-MSG02  TO MENSAGO
                   ELSE
                       MOVE WRK-MSG07  TO MENSAGO
                   END-IF
               END-IF
           END-IF

           IF  WRK-COMM-FASE           EQUAL '2'
               MOVE '/'                TO WRK-PARECI(1)
           END-IF

           PERFORM 8000-ENVIAR-MAPA.

      *----------------------------------------------------------------*
       5000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       6000-RECEBER-MAPA               SECTION.
      *----------------------------------------------------------------*

           MOVE 12                     TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE

           EXEC CICS
                RECEIVE MAP   ('BVVE251')
                        MAPSET('BVVE251')
                        INTO  (BVVE251I)
           END-EXEC.

           IF  EIBRESP                 NOT EQUAL DFHRESP(NORMAL)
               PERFORM 9998-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       6000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       7000-EXECUTAR-RETRIEVE          SECTION.
      *----------------------------------------------------------------*

           EXEC CICS
                RETRIEVE INTO   (WRK-AREA-SENHAS-REC)
                         LENGTH (LENGTH OF WRK-AREA-SENHAS-REC)
                         NOHANDLE
           END-EXEC

           IF  EIBRESP                 NOT EQUAL DFHRESP(NORMAL)
               MOVE WRK-MSG04          TO WRK-MSG03
               PERFORM 9000-ENCERRAR-PGM
           END-IF.

           MOVE WRK-TERMINAL-REC       TO WRK-COMM-TERMINAL.

      *----------------------------------------------------------------*
       7000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       8000-ENVIAR-MAPA                SECTION.
      *----------------------------------------------------------------*

           MOVE -1                     TO WRK-PARECL(1)

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

           MOVE 13                     TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE

           EXEC CICS
                SEND MAP   ('BVVE251')
                     MAPSET('BVVE251')
                     FROM  (BVVE251O)
                     CURSOR
                     ERASE
                     FREEKB
           END-EXEC

           IF  EIBRESP                  NOT EQUAL DFHRESP(NORMAL)
               MOVE EIBRESP            TO WRK-EIBRESP
               EXEC CICS
                    SEND FROM(WRK-MSG99)
                         ERASE
               END-EXEC

               EXEC CICS
                    RETURN
               END-EXEC

           ELSE
               EXEC CICS
                    RETURN TRANSID ('VV85')
                           COMMAREA(WRK-COMMAREA)
               END-EXEC
           END-IF.

      *----------------------------------------------------------------*
       8000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9000-ENCERRAR-PGM               SECTION.
      *----------------------------------------------------------------*

           MOVE 14                     TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE

           EXEC CICS
                SEND FROM(WRK-MSG03)
                     ERASE
           END-EXEC.

           IF  EIBRESP                 NOT EQUAL DFHRESP(NORMAL)
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

           IF  EIBRESP                 NOT EQUAL ZEROS
               MOVE EIBRESP            TO WRK-EIBRESP
               MOVE WRK-MSG99          TO MENSAGO
           ELSE
               PERFORM 9999-ERRO-ABCODE
               MOVE WRK-MSG00          TO MENSAGO
           END-IF.

           PERFORM 8000-ENVIAR-MAPA.

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
