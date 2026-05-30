      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. BVVE4025.
       AUTHOR.     FRANCISMARA.
      *================================================================*
      *                  B S I - T E C N O L O G I A                   *
      *----------------------------------------------------------------*
      *    PROGRAMA.....: BVVE4025                                     *
      *    PROGRAMADORA.: FRANCISMARA          - BSI                   *
      *    DATA.........: JUNHO/2012                                   *
      *----------------------------------------------------------------*
      *    OBJETIVO     :                                              *
      *      MONITORACAO DAS MANUTENCAOES ENVIADAS A CIP               *
      *----------------------------------------------------------------*
      *    MAPA/MAPSET  :                                              *
      *      BVVE425 - MANUTENCOES ENVIADAS A CIP                      *
      *----------------------------------------------------------------*
      *    TRANSACAO    :                                              *
      *      VV4N                                                      *
      *----------------------------------------------------------------*
      *    NAVEGACAO    :                                              *
      *      CHAMADO POR: BVVE4011              CHAMA:                 *
      *      CHAMADO POR: BVVE4012              CHAMA:                 *
      *      CHAMADO POR: BVVE4013              CHAMA:                 *
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
           '*  INICIO DA WORKING BVVE4025  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)        VALUE
           '*         INDEXADORES          *'.
      *----------------------------------------------------------------*
       01  WRK-INDEXADORES.
           03  WRK-IND                 PIC  9(03) COMP-3  VALUE ZEROS.
           03  WRK-IND2                PIC  9(03) COMP-3  VALUE ZEROS.

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

7C2511 01  WRK-FORMAT-CGCCPF.
7C2511*    03  WRK-NROCNPJ             PIC  9(008)       VALUE ZEROS.
7C2511     03  WRK-NROCNPJ             PIC  X(008)       VALUE SPACES.
           03  WRK-BARRA               PIC  X(001)       VALUE SPACES.
7C2511*    03  WRK-FILIAL              PIC  9(004)       VALUE ZEROS.
7C2511     03  WRK-FILIAL              PIC  X(004)       VALUE SPACES.
           03  WRK-TRACO               PIC  X(001)       VALUE SPACES.
           03  WRK-CTRL-CNPJ           PIC  9(002)       VALUE ZEROS.
       01  WRK-FORNAT-CPF REDEFINES WRK-FORMAT-CGCCPF.
           03  WRK-CPF                 PIC  9(009).
           03  WRK-TRACO-2             PIC  X(001).
           03  WRK-CTRL-CPF            PIC  9(002).

7C2511 01  WRK-CGCCPF.
7C2511*    03  WRK-NROCNPJ             PIC  9(008)       VALUE ZEROS.
7C2511     03  WRK-NROCNPJ             PIC  X(008)       VALUE SPACES.
7C2511*    03  WRK-FILIAL              PIC  9(004)       VALUE ZEROS.
7C2511     03  WRK-FILIAL              PIC  X(004)       VALUE SPACES.
           03  WRK-CTRL-CNPJ           PIC  9(002)       VALUE ZEROS.
       01  WRK-SO-CPF REDEFINES WRK-CGCCPF.
           03  FILLER                  PIC  9(003).
           03  WRK-CPF                 PIC  9(009).
           03  WRK-CTRL-CPF            PIC  9(002).
       01  WRK-AMBOS-NUM  REDEFINES WRK-SO-CPF
                                       PIC  9(014).

       01  WRK-VLR-EDIT-X.
           03  WRK-VLR-EDIT            PIC  ZZZZZZZZZ.ZZ9,99
                                                         VALUE ZEROS.

       01  WRK-X-9.
           03  WRK-9-9                 PIC  9(09)      VALUE ZEROS.

       01  WRK-TIMESTAMP.
           03  WRK-ANO-TI              PIC  9(004)      VALUE ZEROS.
           03  WRK-MES-TI              PIC  9(002)      VALUE ZEROS.
           03  WRK-DIA-TI              PIC  9(002)      VALUE ZEROS.
           03  WRK-HH-TI               PIC  9(002)      VALUE ZEROS.
           03  WRK-MM-TI               PIC  9(002)      VALUE ZEROS.
           03  WRK-SS-TI               PIC  9(002)      VALUE ZEROS.
           03  WRK-MMMMMM-TI           PIC  9(006)      VALUE ZEROS.

       01  WRK-HORA-EDIT.
           03  WRK-HH-TI               PIC  9(002)      VALUE ZEROS.
           03  FILLER                  PIC  X(001)      VALUE ':'.
           03  WRK-MM-TI               PIC  9(002)      VALUE ZEROS.
           03  FILLER                  PIC  X(001)      VALUE ':'.
           03  WRK-SS-TI               PIC  9(002)      VALUE ZEROS.

       01  WRK-DATA-EDIT.
           03  WRK-DIA-TI              PIC  9(002)      VALUE ZEROS.
           03  FILLER                  PIC  X(001)      VALUE '/'.
           03  WRK-MES-TI              PIC  9(002)      VALUE ZEROS.
           03  FILLER                  PIC  X(001)      VALUE '/'.
           03  WRK-ANO-TI              PIC  9(004)      VALUE ZEROS.


       01  WRK-DATA-DB2.
           03  WRK-DIA                 PIC  9(02)        VALUE ZEROS.
           03  FILLER                  PIC  X(01)        VALUE '/'.
           03  WRK-MES                 PIC  9(02)        VALUE ZEROS.
           03  FILLER                  PIC  X(01)        VALUE '/'.
           03  WRK-ANO                 PIC  9(04)        VALUE ZEROS.

       01  WRK-DATA-INV.
           03  WRK-ANO                 PIC  9(04)        VALUE ZEROS.
           03  WRK-MES                 PIC  9(02)        VALUE ZEROS.
           03  WRK-DIA                 PIC  9(02)        VALUE ZEROS.
       01  WRK-DATA-INV-R REDEFINES    WRK-DATA-INV
                                       PIC  9(08).

       01  WRK-CONTA-13                PIC  9(13)        VALUE ZEROS.
       01  FILLER  REDEFINES WRK-CONTA-13.
           03  FILLER                  PIC  X(05).
           03  WRK-CONTA-7             PIC  9(07).
           03  WRK-DIG                 PIC  9(01).

       01  WRK-CONTA-EDT.
           03  WRK-7-EDT               PIC  9(07)        VALUE ZEROS.
           03  FILLER                  PIC  X(01)        VALUE '-'.
           03  WRK-DIG-EDT             PIC  9(01)        VALUE ZEROS.

       01  WRK-LITERAL-PF              PIC  X(079)       VALUE
           'PF2-DESC             PF3-ANTER            PF5-SENHAS
      -    '    PF10-ATUALIZAR'.

       01  WRK-LITERAL-CONS            PIC  X(079)       VALUE
           'PF2-DESC       PF3-ANTER      PF5-SENHAS     PF10-ATUALIZAR
      -    '    PF11-FILIAIS'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*      AREA DE MENSAGENS       *'.
      *----------------------------------------------------------------*

       01  WRK-MENSAGENS.
           03  WRK-MSG01               PIC  X(79)        VALUE
               'Pf invalida'.

           03  WRK-MSG02               PIC  X(79)        VALUE
               'Tecle <PF10> para acompanhar e atualizar o monitoramento
      -        ' '.

           03  WRK-MSG03               PIC  X(79)        VALUE
               'Solicitacao em processamento. Tecle <PF10> para atualiza
      -        'r'.

           03  WRK-MSG04               PIC  X(79)        VALUE
               'Consulta de filiais somente quando consulta por raiz'.

           03  WRK-MSG11               PIC  X(79)        VALUE
               'Acesso permitido somente pelas transacoes VV4G, VV4H, VV
      -        '4I, VV7I E VV4O.'.

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

           03  WRK-ERRO-MENSA-01.
               05  FILLER              PIC  X(44)        VALUE
                   'ERRO NO PROCESSAMENTO DA MENSAGEM. EIBRESP: '.
               05  WRK-EIBRESP-01      PIC  9(04)        VALUE ZEROS.

           03  WRK-ERRO-MENSA-02.
               05  FILLER              PIC  X(42)        VALUE
                   'ERRO NO PROCESSAMENTO DA MENSAGEM. C.COD: '.
               05  WRK-C-COD-01        PIC  9(04)        VALUE ZEROS.
               05  FILLER              PIC  X(07)        VALUE
                   'R.COD: '.
               05  WRK-R-COD-01        PIC  9(04)        VALUE ZEROS.

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
           03 WRK-COMM-CCTRL-IF        PIC X(023)        VALUE SPACES.
           03 WRK-COMM-TP-MANUT        PIC X(001)        VALUE SPACES.
           03 WRK-COMM-CHAMADOR        PIC X(008)        VALUE SPACES.
7C2511     03 WRK-COMM-PAG-NAV         PIC 9(005)        VALUE ZEROS.
7C2511*    03 WRK-COMM-CNPJ            PIC 9(009)        VALUE ZEROS.
7C2511     03 WRK-COMM-CNPJ            PIC X(009)        VALUE SPACES.
           03 WRK-COMM-SOLI-RAIZ       PIC X(001)        VALUE SPACES.

       01  WRK-COMMAREA-0241.
           03 WRK-COMM-241-CCTRL-IF    PIC X(023)        VALUE SPACES.
           03 WRK-COMM-241-CHAMADOR    PIC X(008)        VALUE SPACES.
7C2511     03 WRK-COMM-241-PAG-NAV     PIC 9(005)        VALUE ZEROS.
7C2511*    03 WRK-COMM-241-CNPJ        PIC 9(009)        VALUE ZEROS.
7C2511     03 WRK-COMM-241-CNPJ        PIC X(009)        VALUE SPACES.
           03 WRK-COMM-241-BAND        PIC X(010)        VALUE SPACES.

       01  WRK-COMMAREA-4026.
           03  WRK-COMM-TRANSACAO      PIC  X(04)        VALUE SPACES.
           03  WRK-COMM-FASE           PIC  X(01)        VALUE SPACES.
           03  WRK-COMM-FIM            PIC  X(01)        VALUE SPACES.
           03  WRK-COMM-PAG            PIC  9(05)        VALUE ZEROS.
           03  WRK-COMM-OCCURS.
               05  WRK-COMM-CTRL-IF    PIC  X(20)        VALUE SPACES
                                       OCCURS 15.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*     AREA DO ARQVUIO TRAVPMSG *'.
      *----------------------------------------------------------------*

       COPY 'I#BVVED2'.


      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*     AREA DE MENSAGENS '.
      *----------------------------------------------------------------*

       COPY 'I#BVVED9'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           ' AREA DO ARQUIVO TRAVPCON'.
      *----------------------------------------------------------------*

       COPY 'I#BVVED4'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*     AREA DO ARQVUIO TRAVPMAN *'.
      *----------------------------------------------------------------*

       COPY 'I#BVVED3'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*     AREA DO ARQVUIO TRAVPBAN *'.
      *----------------------------------------------------------------*

       COPY 'I#BVVED1'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*     AREA DO MAPA BVVE425     *'.
      *----------------------------------------------------------------*

       COPY 'BVVE425'.

       01  WRK-BVVE425I                REDEFINES BVVE425I.
           03  WRK-FIXO                PIC X(276).
           03  FILLER                  OCCURS 6.
               05  FILLER              PIC  X(03).
               05  WRK-MENSAO          PIC  X(75).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*   FIM DA WORKING BVVE4025   *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  DFHCOMMAREA.
           03  FILLER                  PIC  X(311).

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
                       PF10  (1900-RECEBER-CONTROLE)
                       PF11  (1400-CONSULTA-FILIAL)
                       CLEAR (9000-ENCERRAR-PGM)
                       ENTER (1900-RECEBER-CONTROLE)
                       ANYKEY(1800-TRATAR-PF-INVALIDA)
           END-EXEC.

           IF EIBRESP                  NOT EQUAL DFHRESP(NORMAL)
              PERFORM 9998-TRATAR-ERRO
           END-IF

           IF EIBCALEN                 EQUAL ZEROS OR
             (EIBTRNID                 NOT EQUAL 'VV4N'
                                       AND 'VV4G' AND 'VV4H' AND 'VV4I'
                                       AND 'VV4E' AND 'VV4O' AND 'VV7I')
              MOVE    WRK-MSG11        TO MENSAGO
              PERFORM 9000-ENCERRAR-PGM
           END-IF

           IF  EIBTRNID                 EQUAL 'VV7I'
               MOVE DFHCOMMAREA(1:EIBCALEN)
                                            TO WRK-COMMAREA-0241
               MOVE  WRK-COMM-241-CCTRL-IF  TO WRK-COMM-CCTRL-IF
               MOVE  WRK-COMM-241-CHAMADOR  TO WRK-COMM-CHAMADOR
               MOVE  WRK-COMM-241-PAG-NAV   TO WRK-COMM-PAG-NAV
               MOVE  WRK-COMM-241-CNPJ      TO WRK-COMM-CNPJ
               MOVE 'C'                     TO WRK-COMM-TP-MANUT
           ELSE
               MOVE DFHCOMMAREA(1:EIBCALEN)
                                       TO WRK-COMMAREA
           END-IF.

           IF EIBTRNID                 NOT EQUAL 'VV4N'
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

           IF  WRK-COMM-CHAMADOR       EQUAL 'BVVE4026'
               INITIALIZE  WRK-COMMAREA-4026
               MOVE WRK-COMM-PAG-NAV   TO WRK-COMM-PAG
               EXEC CICS
                XCTL PROGRAM (WRK-COMM-CHAMADOR)
                     COMMAREA(WRK-COMMAREA-4026)
               END-EXEC
           ELSE
               EXEC CICS
                XCTL PROGRAM (WRK-COMM-CHAMADOR)
                     COMMAREA(WRK-COMMAREA)
               END-EXEC
           END-IF.

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
       1400-CONSULTA-FILIAL            SECTION.
      *----------------------------------------------------------------*
      *-- VER PGM NOVO
           IF  WRK-COMM-TP-MANUT           NOT EQUAL 'C'
               PERFORM 1800-TRATAR-PF-INVALIDA
           END-IF.

           IF  WRK-COMM-SOLI-RAIZ          NOT EQUAL 'S'
               MOVE    WRK-MSG04           TO MENSAGO
               PERFORM 1940-ENVIAR-MAPA
           END-IF.

           MOVE  WRK-COMM-CCTRL-IF         TO WRK-COMM-241-CCTRL-IF
           MOVE  WRK-COMM-CHAMADOR         TO WRK-COMM-241-CHAMADOR
           MOVE  WRK-COMM-PAG-NAV          TO WRK-COMM-241-PAG-NAV
           MOVE  WRK-COMM-CNPJ             TO WRK-COMM-241-CNPJ
           MOVE  BANDO                     TO WRK-COMM-241-BAND

           MOVE    04                      TO WRK-LOCAL-EIBRESP
                                              WRK-LOCAL-ABCODE

           EXEC CICS
            XCTL PROGRAM ('BVVE0241')
                 COMMAREA(WRK-COMMAREA-0241)
           END-EXEC

           IF EIBRESP                  NOT EQUAL DFHRESP(NORMAL)
              PERFORM 9998-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1800-TRATAR-PF-INVALIDA         SECTION.
      *----------------------------------------------------------------*

           MOVE    WRK-MSG01           TO MENSAGO.

           PERFORM 1940-ENVIAR-MAPA.

      *----------------------------------------------------------------*
       1800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1900-RECEBER-CONTROLE           SECTION.
      *----------------------------------------------------------------*

           MOVE    LOW-VALUES          TO BVVE425I
           MOVE    WRK-MSG02           TO MENSAGO

           IF  WRK-COMM-TP-MANUT           EQUAL 'C'
               MOVE WRK-LITERAL-CONS       TO PFSO
           ELSE
               MOVE WRK-LITERAL-PF         TO PFSO
           END-IF.

           PERFORM 1922-LER-TRAVPMSG.

           IF  EIBRESP                     EQUAL DFHRESP (NOTFND)
               MOVE    WRK-MSG03           TO MENSAGO
               PERFORM 1940-ENVIAR-MAPA
           END-IF.

           MOVE BVVED2-NU-OP-OR        TO NROPERO.

           IF  WRK-COMM-TP-MANUT       EQUAL 'C'
               PERFORM 1930-ACESSAR-TRAVPCON
           ELSE
               PERFORM 1910-ACESSAR-TRAVPMAN
           END-IF.

           PERFORM 1950-LER-TRAVPBAN

           PERFORM 1940-ENVIAR-MAPA.

      *----------------------------------------------------------------*
       1900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1910-ACESSAR-TRAVPMAN           SECTION.
      *----------------------------------------------------------------*

           PERFORM 1912-LER-TRAVPMAN.

           EVALUATE WRK-COMM-TP-MANUT
               WHEN 'I'
                   MOVE 'INCLUSAO'     TO SOLICO
               WHEN 'A'
                   MOVE 'ALTERACAO'    TO SOLICO
               WHEN 'B'
                   MOVE 'BAIXA    '    TO SOLICO
           END-EVALUATE.

           IF  BVVED3-AG-CLI               NOT EQUAL ZEROS
               MOVE BVVED3-AG-CLI          TO AGENO
               MOVE BVVED3-CT-CLI          TO WRK-CONTA-13
               MOVE WRK-CONTA-7            TO WRK-7-EDT
               MOVE WRK-DIG                TO WRK-DIG-EDT
               MOVE WRK-CONTA-EDT          TO CONTAO
           END-IF.

           MOVE BVVED3-COD-BANDEIRA        TO BVVED1-COD-BAND
           MOVE BVVED3-TIMESTAMP           TO WRK-TIMESTAMP.
           MOVE CORR WRK-TIMESTAMP         TO WRK-HORA-EDIT.
           MOVE WRK-HORA-EDIT              TO HORASOLO.
           MOVE CORR WRK-TIMESTAMP         TO WRK-DATA-EDIT.
           MOVE WRK-DATA-EDIT              TO DATASO.
           MOVE BVVED3-NUM-CTRL-IF         TO NROIFO.
           MOVE BVVED3-VLR-MAX-RET         TO WRK-VLR-EDIT
           MOVE WRK-VLR-EDIT-x             TO VLRO

           IF   BVVED3-DTFIM-VALIDD        NOT EQUAL ZEROS
               MOVE BVVED3-DTFIM-VALIDD    TO WRK-DATA-INV-R
               MOVE CORR WRK-DATA-INV      TO WRK-DATA-DB2
               MOVE WRK-DATA-DB2           TO FIMTRAO
           ELSE
               MOVE SPACES                 TO FIMTRAO
           END-IF.

           IF  BVVED3-DTHR-SCG             NOT EQUAL SPACES
               MOVE BVVED3-DTHR-SCG        TO WRK-TIMESTAMP
               MOVE CORR WRK-TIMESTAMP     TO WRK-HORA-EDIT
               MOVE WRK-HORA-EDIT          TO HORASCGO
               MOVE CORR WRK-TIMESTAMP     TO WRK-DATA-EDIT
               MOVE WRK-DATA-EDIT          TO DATASCGO
           END-IF.

           MOVE SPACES                 TO WRK-FORMAT-CGCCPF.

           IF  BVVED3-SOLICT-RAIZ      EQUAL 'S'
               MOVE BVVED3-CNPJ        TO WRK-9-9
               MOVE WRK-X-9            TO CNPJCPFO
           ELSE
               IF  BVVED3-TP-PESSOA-CLI EQUAL 'F'
                   MOVE BVVED3-CNPJ-CPF-CLI     TO WRK-AMBOS-NUM
                   MOVE CORR WRK-SO-CPF         TO WRK-FORNAT-CPF
                   MOVE '-'                     TO WRK-TRACO-2
               ELSE
                   MOVE BVVED3-CNPJ-CPF-CLI     TO WRK-AMBOS-NUM
                   MOVE CORR WRK-CGCCPF         TO WRK-FORMAT-CGCCPF
                   MOVE '-'                     TO WRK-TRACO
                   MOVE '/'                     TO WRK-BARRA
               END-IF
               MOVE WRK-FORMAT-CGCCPF      TO CNPJCPFO
           END-IF.

           IF  BVVED2-ERRO             EQUAL 99
               MOVE 'ERRO'             TO STATUSO
               PERFORM 1924-TRATAR-ERRO-MAN
           ELSE
               IF BVVED2-STATUS        EQUAL '01' OR '02'
                   MOVE 'ENVIADO '     TO STATUSO
                   MOVE 'AGUARDANDO RESPOSTA DA CIP'
                                       TO MENSAO
               ELSE
                   MOVE 'OK      '     TO STATUSO
                   MOVE 'PROCESSADO COM SUCESSO PELA CIP'
                                       TO MENSAO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1910-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1912-LER-TRAVPMAN               SECTION.
      *----------------------------------------------------------------*

           MOVE 09                     TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE.
           MOVE SPACES                 TO BVVED3-FILLER-CHV
           MOVE BVVED2-CNPJ            TO BVVED3-CNPJ
           MOVE BVVED2-FILIAL          TO BVVED3-FILIAL
           MOVE WRK-COMM-CCTRL-IF      TO BVVED3-NUM-CTRL-IF

           EXEC CICS
                READ     FILE  ('TRAVPMAN')
                         INTO  (BVVED3-TRAVPMAN)
                         RIDFLD(BVVED3-CHAVE)
                         NOHANDLE
           END-EXEC.

           IF  EIBRESP                 NOT EQUAL DFHRESP (NORMAL) AND
               EIBRESP                 NOT EQUAL DFHRESP (ENDFILE)
               MOVE EIBRESP            TO WRK-EIBRESP-AUX
               MOVE WRK-EIBRESP-AUX    TO EIBRESP
               PERFORM 9998-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1912-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1922-LER-TRAVPMSG               SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO BVVED2-CHAVE
           MOVE WRK-COMM-CCTRL-IF      TO BVVED2-NUM-CTRL-IF
           MOVE SPACES                 TO BVVED2-NUOP

           MOVE 09                     TO WRK-LOCAL-EIBRESP
                                           WRK-LOCAL-ABCODE.

           EXEC CICS
                READ     FILE  ('TRAVPMSG')
                         INTO  (BVVED2-TRVPMSG)
                         RIDFLD(BVVED2-CHAVE)
                         NOHANDLE
           END-EXEC.

           IF  EIBRESP                 NOT EQUAL DFHRESP (NORMAL) AND
               EIBRESP                 NOT EQUAL DFHRESP (NOTFND)
               MOVE EIBRESP            TO WRK-EIBRESP-AUX
               MOVE WRK-EIBRESP-AUX    TO EIBRESP
               PERFORM 9998-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1922-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1924-TRATAR-ERRO-MAN            SECTION.
      *----------------------------------------------------------------*

           IF  BVVED2-EIBRESP          NOT EQUAL ZEROS
               MOVE BVVED2-EIBRESP     TO WRK-EIBRESP-01
               MOVE WRK-ERRO-MENSA-01  TO WRK-MENSAO(1)
               GO TO 1924-99-FIM
           END-IF.

           IF  BVVED2-COMPLETION-CODE      NOT EQUAL ZEROS
               MOVE BVVED2-COMPLETION-CODE TO WRK-C-COD-01
               MOVE BVVED2-REASON-CODE     TO WRK-R-COD-01
               MOVE WRK-ERRO-MENSA-02      TO WRK-MENSAO(1)
               GO TO 1924-99-FIM
           END-IF.

           IF  BVVED2-ERRO-GEN             NOT EQUAL SPACES AND ZEROS
               PERFORM VARYING WRK-IND FROM 1 BY 1
                       UNTIL WRK-IND   GREATER 99 OR
                             BVVED9-COD-MSG(WRK-IND) EQUAL
                                       BVVED2-ERRO-GEN
               END-PERFORM
               IF WRK-IND              GREATER 99
                  STRING BVVED2-ERRO-GEN
                         BVVED2-HIST (1:40)
                  DELIMITED BY SIZE
                  INTO WRK-MENSAO(1)
               ELSE
                  MOVE  BVVED9-TXT-MSG(WRK-IND) TO WRK-MENSAO(1)
               END-IF
               GO TO 1924-99-FIM
           END-IF.

           PERFORM VARYING WRK-IND FROM 1 BY 1
                   UNTIL WRK-IND   GREATER 6 OR
                  (BVVED3-COD-ERRO(WRK-IND) EQUAL ZEROS OR SPACES)

                   PERFORM VARYING WRK-IND2 FROM 1 BY 1
                           UNTIL   WRK-IND2  GREATER 99 OR
                           BVVED3-COD-ERRO(WRK-IND)   EQUAL
                                   BVVED9-COD-MSG(WRK-IND2)
                   END-PERFORM
                   IF  WRK-IND2    GREATER 99
                       MOVE BVVED3-COD-ERRO(WRK-IND)
                                           TO WRK-MENSAO(WRK-IND)
                   ELSE
                       MOVE BVVED9-TXT-MSG(WRK-IND2)
                                           TO WRK-MENSAO(WRK-IND)
                   END-IF
           END-PERFORM.

      *----------------------------------------------------------------*
       1924-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1925-TRATAR-ERRO-CON            SECTION.
      *----------------------------------------------------------------*

           IF  BVVED2-EIBRESP          NOT EQUAL ZEROS
               MOVE BVVED2-EIBRESP     TO WRK-EIBRESP-01
               MOVE WRK-ERRO-MENSA-01  TO WRK-MENSAO(1)
               GO TO 1925-99-FIM
           END-IF.

           IF  BVVED2-COMPLETION-CODE      NOT EQUAL ZEROS
               MOVE BVVED2-COMPLETION-CODE TO WRK-C-COD-01
               MOVE BVVED2-REASON-CODE     TO WRK-R-COD-01
               MOVE WRK-ERRO-MENSA-02      TO WRK-MENSAO(1)
               GO TO 1925-99-FIM
           END-IF.

           IF  BVVED2-ERRO-GEN             NOT EQUAL SPACES AND ZEROS
               PERFORM VARYING WRK-IND FROM 1 BY 1
                       UNTIL WRK-IND   GREATER 99 OR
                             BVVED9-COD-MSG(WRK-IND) EQUAL
                                       BVVED2-ERRO-GEN
               END-PERFORM
               IF WRK-IND              GREATER 99
                  STRING BVVED2-ERRO-GEN
                         BVVED2-HIST (1:40)
                  DELIMITED BY SIZE
                  INTO WRK-MENSAO(1)
               ELSE
                  MOVE  BVVED9-TXT-MSG(WRK-IND) TO WRK-MENSAO(1)
               END-IF
               GO TO 1925-99-FIM
           END-IF.

           PERFORM VARYING WRK-IND FROM 1 BY 1
                   UNTIL WRK-IND   GREATER 6 OR
                  (BVVED4-COD-ERRO(WRK-IND) EQUAL ZEROS OR SPACES)

                   PERFORM VARYING WRK-IND2 FROM 1 BY 1
                           UNTIL   WRK-IND2  GREATER 99 OR
                           BVVED4-COD-ERRO(WRK-IND)   EQUAL
                                   BVVED9-COD-MSG(WRK-IND2)
                   END-PERFORM
                   IF  WRK-IND2    GREATER 99
                       MOVE BVVED4-COD-ERRO(WRK-IND)
                                           TO WRK-MENSAO(WRK-IND)
                   ELSE
                       MOVE BVVED9-TXT-MSG(WRK-IND2)
                                           TO WRK-MENSAO(WRK-IND)
                   END-IF
           END-PERFORM.

      *----------------------------------------------------------------*
       1925-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1930-ACESSAR-TRAVPCON           SECTION.
      *----------------------------------------------------------------*

           PERFORM 1931-LER-TRAVPCON.

           MOVE  BVVED4-CNPJ           TO WRK-COMM-CNPJ
           MOVE  BVVED4-SOLICT-RAIZ    TO WRK-COMM-SOLI-RAIZ

           MOVE 'CONSULTA '            TO SOLICO

           MOVE BVVED4-COD-BANDEIRA    TO BVVED1-COD-BAND
           MOVE BVVED4-TIMESTAMP       TO WRK-TIMESTAMP.
           MOVE CORR WRK-TIMESTAMP     TO WRK-HORA-EDIT.
           MOVE WRK-HORA-EDIT          TO HORASOLO.
           MOVE CORR WRK-TIMESTAMP     TO WRK-DATA-EDIT.
           MOVE WRK-DATA-EDIT          TO DATASO.
           MOVE BVVED4-NUM-CTRL-IF     TO NROIFO.
           MOVE BVVED4-VLR-MAX-RET     TO WRK-VLR-EDIT
           MOVE WRK-VLR-EDIT-x         TO VLRO
           MOVE SPACES                 TO WRK-FORMAT-CGCCPF.

           IF  BVVED4-INDR-BLOQ-DOMCL EQUAL 'N' OR
               BVVED4-DTFIM-VALIDD    EQUAL ZEROS
               MOVE SPACES            TO FIMTRAO
           ELSE
               MOVE BVVED4-DTFIM-VALIDD    TO WRK-DATA-INV-R
               MOVE CORR WRK-DATA-INV      TO WRK-DATA-DB2
               MOVE WRK-DATA-DB2           TO FIMTRAO
           END-IF.

           IF  BVVED4-INDR-BLOQ-DOMCL      EQUAL 'S'
               IF  BVVED4-AG-CLI           NOT EQUAL ZEROS
                   MOVE BVVED4-AG-CLI      TO AGENO
                   MOVE BVVED4-CT-CLI      TO WRK-CONTA-13
                   MOVE WRK-CONTA-7        TO WRK-7-EDT
                   MOVE WRK-DIG            TO WRK-DIG-EDT
                   MOVE WRK-CONTA-EDT      TO CONTAO
               END-IF
               MOVE 'SIM'                  TO TRAVAO
               IF  BVVED4-TP-MANUT         EQUAL 'C'
                   MOVE '- COMPLETO'       TO TPTRAVO
               ELSE
                   MOVE '- RAIZ    '       TO TPTRAVO
               END-IF
               MOVE BVVED4-INST-DETENTORA  TO INSDETO
           ELSE
               IF  BVVED4-INDR-BLOQ-DOMCL  EQUAL 'N'
                   MOVE 'NAO'              TO TRAVAO
               END-IF
           END-IF.

           IF  BVVED4-SOLICT-RAIZ      EQUAL 'S'
               MOVE BVVED4-CNPJ        TO  WRK-9-9
               MOVE WRK-X-9            TO  CNPJCPFO
           ELSE
               IF  BVVED4-TP-PESSOA-CLI EQUAL 'F'
                   MOVE BVVED4-CNPJ-CPF-CLI     TO WRK-AMBOS-NUM
                   MOVE CORR WRK-SO-CPF         TO WRK-FORNAT-CPF
                   MOVE '-'                     TO WRK-TRACO-2
               ELSE
                   MOVE BVVED4-CNPJ-CPF-CLI     TO WRK-AMBOS-NUM
                   MOVE CORR WRK-CGCCPF         TO WRK-FORMAT-CGCCPF
                   MOVE '-'                     TO WRK-TRACO
                   MOVE '/'                     TO WRK-BARRA
               END-IF
               MOVE WRK-FORMAT-CGCCPF      TO CNPJCPFO
           END-IF.

           IF  BVVED2-ERRO             EQUAL 99
               MOVE 'ERRO'             TO STATUSO
               PERFORM 1925-TRATAR-ERRO-CON
           ELSE
               IF BVVED2-STATUS        EQUAL '01' OR '02'
                   MOVE 'ENVIADO '     TO STATUSO
                   MOVE 'AGUARDANDO RESPOSTA DA CIP'
                                       TO MENSAO
               ELSE
                   MOVE 'PROCESS.'     TO STATUSO
                   IF  BVVED4-SIT-CONS EQUAL 03
                       MOVE 'PESQUISA NAO RETORNOU DADOS PARA CNPJ/CPF E
      -                     ' BANDEIRA'
                                       TO MENSAO
                   ELSE
                       MOVE 'PROCESSADO COM SUCESSO PELA CIP'
                                       TO MENSAO
                   END-IF
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1930-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1931-LER-TRAVPCON               SECTION.
      *----------------------------------------------------------------*

           MOVE 09                     TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE.
           MOVE SPACES                 TO BVVED4-FILLER-CHV
           MOVE BVVED2-CNPJ            TO BVVED4-CNPJ
           MOVE BVVED2-FILIAL          TO BVVED4-FILIAL
           MOVE WRK-COMM-CCTRL-IF      TO BVVED4-NUM-CTRL-IF

           EXEC CICS
                READ     FILE  ('TRAVPCON')
                         INTO  (BVVED4-TRAVPCON)
                         RIDFLD(BVVED4-CHAVE)
                         NOHANDLE
           END-EXEC.

           IF  EIBRESP                 NOT EQUAL DFHRESP (NORMAL) AND
               EIBRESP                 NOT EQUAL DFHRESP (NOTFND)
               MOVE EIBRESP            TO WRK-EIBRESP-AUX
               MOVE WRK-EIBRESP-AUX    TO EIBRESP
               PERFORM 9998-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1931-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1940-ENVIAR-MAPA                SECTION.
      *----------------------------------------------------------------*

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

           MOVE    12                  TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE

           EXEC CICS
                SEND MAP   ('BVVE425')
                     MAPSET('BVVE425')
                     FROM  (BVVE425O)
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

              EXEC CICS
                   RETURN
              END-EXEC

           ELSE
              EXEC CICS
                   RETURN TRANSID ('VV4N')
                          COMMAREA(WRK-COMMAREA)
              END-EXEC
           END-IF.

      *----------------------------------------------------------------*
       1940-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1950-LER-TRAVPBAN               SECTION.
      *----------------------------------------------------------------*

           MOVE 09                     TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE.

           MOVE SPACES                 TO BVVED1-FILLER-CHV

           EXEC CICS
                READ     FILE  ('TRAVPBAN')
                         INTO  (BVVED1-TRAVPBAN)
                         RIDFLD(BVVED1-CHAVE)
                         NOHANDLE
           END-EXEC.

           IF  EIBRESP                 NOT EQUAL DFHRESP (NORMAL) AND
               EIBRESP                 NOT EQUAL DFHRESP (NOTFND)
               MOVE EIBRESP            TO WRK-EIBRESP-AUX
               MOVE WRK-EIBRESP-AUX    TO EIBRESP
               PERFORM 9998-TRATAR-ERRO
           END-IF.

           IF  EIBRESP                 EQUAL DFHRESP (NORMAL)
               MOVE BVVED1-DESC-BAND(1:10)   TO BANDO
           END-IF.

      *----------------------------------------------------------------*
       1950-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000-RECEBER-MAPA               SECTION.
      *----------------------------------------------------------------*

           MOVE    13                  TO WRK-LOCAL-EIBRESP
                                          WRK-LOCAL-ABCODE

           EXEC CICS
                RECEIVE MAP   ('BVVE425')
                        MAPSET('BVVE425')
                        INTO  (BVVE425I)
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
                SEND FROM(WRK-MSG11)
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

           PERFORM 1940-ENVIAR-MAPA.

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
