      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. BVVE4073.
       AUTHOR.     FRANCISMARA PAGNONCELLI.
      *================================================================*
      *                  B S I  T E C N O L O G I A                    *
      *----------------------------------------------------------------*
      *    PROGRAMA....: BVVE4073                                      *
      *    PROGRAMADORA: FRANCISMARA PAGNONCELLI  - BSI                *
      *    ANALISTA....: FRANCISMARA PAGNONCELLI  - BSI                *
      *    DATA........: JULHO/2012                                    *
      *----------------------------------------------------------------*
      *    OBJETIVO....: LER FILA MQ ENVIADA PELO TMNF                 *
      *                  BVVE.QL.RSP.TMNF.BVVE.03                      *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#SBATMV - AREA PARA CHAMADA DA SBAT3510                    *
      *================================================================*
      *                     A L T E R A C A O                          *
      *----------------------------------------------------------------*
      *    PROGRAMADORA: FRANCISMARA PAGNONCELLI  - BSI                *
      *    DATA........: JANEIRO/2017                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....: ALTERAR PARA GUARDAR EM TRAVPMAN A AGENCIA E  *
      *                  CONTA ANTIGA                                  *
      *================================================================*
      *                     A L T E R A C A O                          *
      *----------------------------------------------------------------*
      *    PROGRAMADORA: ADRIANA CAMARGO - PRIME                       *
      *    DATA........: ABRIL/2020                                    *
      *----------------------------------------------------------------*
      *    OBJETIVO....: GRAVAR VALOR ZERADO (BVVED4-VLR-MAX-RET)      *
      *                  NO ARQUIVO TRAVPCON QUANDO NAO TIVER TRAVA    *
      *                  EM NENHUM BANCO OU NAO TIVER TRAVA NO BRADESCO*
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
       77  FILLER                      PIC  X(032)         VALUE
           'INICIO DA AREA DE WORKING'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(032)          VALUE
           'VARIAVEIS AUXILIARES'.
      *----------------------------------------------------------------*

       01  WRK-POS-AUX                 PIC 9(008) COMP-3 VALUE ZEROS.

       01  WRK-SALVA-DADOS-COMPL.
           03 FILLER                   PIC X(006)        VALUE SPACES.
           03 WRK-DB-NUM-IDENTC-SCG    PIC X(017)        VALUE SPACES.
           03 WRK-DB-TP-PESSOA-CLI     PIC X(001)        VALUE SPACES.
7C2511     03 WRK-DB-CNPJ-CPF-CLI      PIC X(014)        VALUE SPACES.
7C2511*    03 WRK-DB-CNPJ-CPF-CLI      PIC 9(014)        VALUE ZEROS.
           03 WRK-DB-COD-PROPT-COMPL   PIC X(003)        VALUE SPACES.
           03 WRK-DB-TP-PRODT-COMPL    PIC X(001)        VALUE SPACES.
           03 WRK-DB-DTHR-MANUT        PIC X(014)        VALUE SPACES.
           03 WRK-DB-INDR-BLOQ-COMPL   PIC X(001)        VALUE SPACES.
           03 WRK-DB-CLI-PROPRIO.
              05 WRK-DB-AG-CLI         PIC 9(004)        VALUE ZEROS.
              05 WRK-DB-CT-CLI         PIC 9(013)        VALUE ZEROS.
              05 WRK-DB-PERC-GAR       PIC 9(005)        VALUE ZEROS.
              05 WRK-DB-DTINI-VALIDD   PIC 9(008)        VALUE ZEROS.
              05 WRK-DB-DTFIM-VALIDD   PIC 9(008)        VALUE ZEROS.
              05 WRK-DB-TP-NATUR-GAR   PIC X(001)        VALUE SPACES.
              05 WRK-DB-VLR-MAX-RET    PIC X(019)        VALUE SPACES.

       01  WRK-IND                     PIC 9(005) COMP-3 VALUE ZEROS.
       01  WRK-CHAVE                   PIC X(090)        VALUE SPACES.
       01  WRK-TRAVA-ATIVA             PIC X(001)        VALUE 'N'.
       01  WRK-OUTRA                   PIC X(001)        VALUE 'N'.
       01  WRK-BRAD                    PIC X(001)        VALUE 'N'.

       01  WRK-FORMAT-CGCCPF.
7C2511     03  WRK-NROCNPJ             PIC  X(008)       VALUE SPACES.
7C2511     03  WRK-FILIAL              PIC  X(004)       VALUE SPACES.
7C2511*    03  WRK-NROCNPJ             PIC  9(008)       VALUE ZEROS.
7C2511*    03  WRK-FILIAL              PIC  9(004)       VALUE ZEROS.
           03  WRK-CTRL-CNPJ           PIC  9(002)       VALUE ZEROS.
7C2511 01  FILLER REDEFINES WRK-FORMAT-CGCCPF.
7C2511     03  WRK-CGCCPF-X            PIC  X(014).
7C2511 01  FILLER REDEFINES WRK-CGCCPF-X.
7C2511*01  FILLER REDEFINES WRK-FORMAT-CGCCPF.
           03  WRK-ZEROS               PIC  9(003).
           03  WRK-CPF                 PIC  9(009).
           03  WRK-CTRL-CPF            PIC  9(002).
       01  WRK-FORMAT-CGCCPF-R         REDEFINES
           WRK-FORMAT-CGCCPF           PIC  9(014).

       01  WRK-X-19                    PIC X(019) VALUE SPACES.
       01  FILLER REDEFINES WRK-X-19.
           03 FILLER                   PIC X(04).
           03 WRK-9-13V99              PIC 9(13)V99.

       01  WRK-DT-HR-SIT.
           03  WRK-ANO                 PIC  9(004)       VALUE ZEROS.
           03  WRK-MES                 PIC  9(002)       VALUE ZEROS.
           03  WRK-DIA                 PIC  9(002)       VALUE ZEROS.
           03  WRK-HH                  PIC  9(002)       VALUE ZEROS.
           03  WRK-MM                  PIC  9(002)       VALUE ZEROS.
           03  WRK-SS                  PIC  9(002)       VALUE ZEROS.

       01  WRK-DT-HR-EDIT.
           03  WRK-ANO                 PIC  9(004)       VALUE ZEROS.
           03  FILLER                  PIC  X(001)       VALUE '-'.
           03  WRK-MES                 PIC  9(002)       VALUE ZEROS.
           03  FILLER                  PIC  X(001)       VALUE '-'.
           03  WRK-DIA                 PIC  9(002)       VALUE ZEROS.
           03  FILLER                  PIC  X(001)       VALUE '-'.
           03  WRK-HH                  PIC  9(002)       VALUE ZEROS.
           03  FILLER                  PIC  X(001)       VALUE ':'.
           03  WRK-MM                  PIC  9(002)       VALUE ZEROS.
           03  FILLER                  PIC  X(001)       VALUE ':'.
           03  WRK-SS                  PIC  9(002)       VALUE ZEROS.

       01  WRK-TIMESTAMP.
           03  WRK-ANO                 PIC  9(004)       VALUE ZEROS.
           03  WRK-MES                 PIC  9(002)       VALUE ZEROS.
           03  WRK-DIA                 PIC  9(002)       VALUE ZEROS.
           03  WRK-HORA                PIC  9(002)       VALUE ZEROS.
           03  WRK-MIN                 PIC  9(002)       VALUE ZEROS.
           03  WRK-SEG                 PIC  9(002)       VALUE ZEROS.
           03  FILLER                  PIC  9(006)       VALUE ZEROS.

       01  WRK-TIMESTAMP-EDIT.
           03  WRK-ANO                 PIC  9(004)       VALUE ZEROS.
           03  FILLER                  PIC  X(001)       VALUE '-'.
           03  WRK-MES                 PIC  9(002)       VALUE ZEROS.
           03  FILLER                  PIC  X(001)       VALUE '-'.
           03  WRK-DIA                 PIC  9(002)       VALUE ZEROS.
           03  FILLER                  PIC  X(001)       VALUE 'T'.
           03  WRK-HORA                PIC  9(002)       VALUE ZEROS.
           03  FILLER                  PIC  X(001)       VALUE ':'.
           03  WRK-MIN                 PIC  9(002)       VALUE ZEROS.
           03  FILLER                  PIC  X(001)       VALUE ':'.
           03  WRK-SEG                 PIC  9(002)       VALUE ZEROS.

       01  WRK-MSG99.
           03  FILLER              PIC  X(46)        VALUE
               'ERRO NA CHAMANDA DO SBAT3510. MODULO BVVE4073.'.
           03  FILLER              PIC  X(09)        VALUE
               'EIBRESP= '.
           03  WRK-EIBRESP         PIC  9(04)        VALUE ZEROS.
           03  FILLER              PIC  X(07)        VALUE
               'LOCAL= '.
           03  WRK-LOCAL-99        PIC  9(02)        VALUE ZEROS.
           03  FILLER              PIC  X(20)        VALUE
               '. CONTACTAR ANALISTA'.

       01  WRK-MSG98.
           03  FILLER              PIC  X(08)        VALUE
               'ERRO NO '.
           03  WRK-FUNCAO          PIC  X(08)        VALUE SPACES.
           03  FILLER              PIC  X(16)        VALUE
               'MODULO BVVE4073.'.
           03  FILLER              PIC  X(07)        VALUE
               'R.COD= '.
           03  WRK-REASON-CODE     PIC  9(04)        VALUE ZEROS.
           03  FILLER              PIC  X(07)        VALUE
               'C.COD= '.
           03  WRK-COM-CODE        PIC  9(04)        VALUE ZEROS.
           03  FILLER              PIC  X(07)        VALUE
               'LOCAL= '.
           03  WRK-LOCAL-98        PIC  9(02)        VALUE ZEROS.
           03  FILLER              PIC  X(20)        VALUE
               '. CONTACTAR ANALISTA'.


       01  WRK-MSG97.
           03  FILLER              PIC  X(08)        VALUE
               'ERRO NO '.
           03  WRK-FUNCAO-97       PIC  X(08)        VALUE SPACES.
           03  FILLER              PIC  X(07)        VALUE
               ' ARQ. '.
           03  WRK-ARQUIVO-97      PIC  X(09)        VALUE SPACES.
           03  FILLER              PIC  X(09)        VALUE
               'EIBRESP= '.
           03  WRK-EIBRESP-97      PIC  9(04)        VALUE ZEROS.
           03  FILLER              PIC  X(07)        VALUE
               'LOCAL= '.
           03  WRK-LOCAL-97        PIC  9(02)        VALUE ZEROS.
           03  FILLER              PIC  X(20)        VALUE
               '. CONTACTAR ANALISTA'.

       01  WRK-MSG-CDES.
           03  FILLER                  PIC X(25)           VALUE
               'ERRO NO ACESSO AO MODULO '.
           03  WRK-MODULO              PIC X(08)           VALUE SPACES.
           03  FILLER                  PIC X(10)           VALUE
               '. EIBRESP='.
           03  WRK-EIBRESP-CDES        PIC 9(04)           VALUE ZEROS.
           03  FILLER                  PIC X(08)           VALUE
               '. LOCAL='.
           03  WRK-LOCAL               PIC 9(02)           VALUE ZEROS.

       01  WRK-COMMAREA.
           03  WRK-COD-RETORNO         PIC 9(004) VALUE ZEROS.
           03  WRK-MSG                 PIC X(080) VALUE SPACES.


      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*     AREA DA BCMSG *'.
      *----------------------------------------------------------------*

       COPY 'I#BVVEDN'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*     AREA DE BOOKS *'.
      *----------------------------------------------------------------*

       COPY 'I#BVVEDD'.
       COPY 'I#BVVEDB'.
       COPY 'I#BVVEDF'.
       COPY 'I#BVVEDJ'.
       COPY 'I#BVVEDQ'.
       COPY 'I#BVVEDR'.
       COPY 'I#BVVEDS'.
       COPY 'I#BVVEDP'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*     AREA DO ARQVUIO TRAVPATI *'.
      *----------------------------------------------------------------*
       COPY 'I#BVVEDW'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*     AREA DO ARQVUIO TRAVPMAN *'.
      *----------------------------------------------------------------*
       COPY 'I#BVVED3'.

       COPY 'I#BVVED4'.
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*     AREA DO ARQVUIO TRAVPMSG *'.
      *----------------------------------------------------------------*

       COPY 'I#BVVED2'.

       01  WRK-BVVED2-TRVPMSG REDEFINES BVVED2-TRVPMSG.
           03 WRK-BVVED2-CHAVE.
              05 WRK-BVVED2-NUM-CTRL-IF    PIC X(020).
              05 WRK-BVVED2-NUOP           PIC X(023).
              05 WRK-BVVED2-FILLER-CHV     PIC X(057).
           03 WRK-DADOS                    PIC X(450).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*     AREA DO BLOCO DE RETORNO *'.
      *----------------------------------------------------------------*

       COPY 'I#SIMM03'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(032)          VALUE
           'AREA DE COMUNIC SBAT3501'.
      *----------------------------------------------------------------*

       COPY 'I#SBATMV'.
           05  WRK-AREA-MQ-LL              PIC S9(08) COMP  VALUE ZEROS.
           05  WRK-AREA-MQ.
               10  WRK-DADOS-ENVIO         PIC X(19000) VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(032)          VALUE
           'FIM DA AREA DE WORKING'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  DFHCOMMAREA.
           05  FILLER                  PIC  X(084).

      *================================================================*
       PROCEDURE                       DIVISION USING DFHCOMMAREA.
      *================================================================*

      ******************************************************************
      *    ROTINA PRINCIPAL DO PROGRAMA                                *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-PROCESSAR.

           PERFORM 3000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    INICIALIZA MODULO.                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           MOVE DFHCOMMAREA(1:EIBCALEN)    TO WRK-COMMAREA.
           MOVE ZEROS                      TO WRK-COD-RETORNO.

           EXEC CICS
                HANDLE ABEND
                       LABEL (9999-TRATAR-ERRO-CICS)
           END-EXEC.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000-PROCESSAR        SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-ABRIR-FILA-MQ.
           PERFORM 2200-LER-FILA-MQ.
           PERFORM 2300-TRATAR-RETORNO.
           PERFORM 2800-FECHAR-FILA-MQ.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2100-ABRIR-FILA-MQ              SECTION.
      *----------------------------------------------------------------*

           INITIALIZE MQ-MCA.

           MOVE 3                      TO MQ-FORMATO.
           MOVE 'OPEN'                 TO MQ-FUNCAO.
           MOVE 'GET'                  TO MQ-OPCAO.
           MOVE 'BVVE.QL.RSP.TMNF.BVVE.03'
                                       TO MQ-FILA.
           MOVE 16384                  TO MQ-OPCOES-DE-GET.

           EXEC    CICS   LINK
                          PROGRAM  ('SBAT3510')
                          COMMAREA (MQ-MCA)
                          LENGTH   (+252)
                          NOHANDLE
           END-EXEC.

           IF  EIBRESP                  NOT EQUAL DFHRESP(NORMAL)
               MOVE EIBRESP             TO WRK-EIBRESP
               MOVE 01                  TO WRK-LOCAL-99
               MOVE WRK-MSG99           TO WRK-MSG
               MOVE 99                  TO WRK-COD-RETORNO
               PERFORM 9999-TRATAR-ERRO-CICS
           END-IF

           IF  MQ-REASON-CODE          NOT EQUAL ZEROS
               MOVE 'OPEN '             TO WRK-FUNCAO
               MOVE MQ-COMPLETION-CODE  TO WRK-COM-CODE
               MOVE MQ-REASON-CODE      TO WRK-REASON-CODE
               MOVE 02                  TO WRK-LOCAL-98
               MOVE WRK-MSG98           TO WRK-MSG
               MOVE 98                  TO WRK-COD-RETORNO
               PERFORM 9999-TRATAR-ERRO-CICS
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2200-LER-FILA-MQ                SECTION.
      *----------------------------------------------------------------*

           MOVE 3                      TO MQ-FORMATO.
           MOVE 'GET'                  TO MQ-FUNCAO.
           MOVE SPACES                 TO MQ-OPCAO.
           MOVE 19000                  TO WRK-AREA-MQ-LL.
           MOVE 16384                  TO MQ-OPCOES-DE-GET.
           MOVE ZEROS                  TO MQ-COMPLETION-CODE
                                          MQ-REASON-CODE.

           EXEC    CICS   LINK
                          PROGRAM  ('SBAT3510')
                          COMMAREA (MQ-MCA)
                          LENGTH   (LENGTH OF MQ-MCA)
                          NOHANDLE
           END-EXEC.

           IF  EIBRESP                  NOT EQUAL DFHRESP(NORMAL)
               MOVE EIBRESP             TO WRK-EIBRESP
               MOVE 03                  TO WRK-LOCAL-99
               MOVE WRK-MSG99           TO WRK-MSG
               MOVE 99                  TO WRK-COD-RETORNO
               PERFORM 9999-TRATAR-ERRO-CICS
           END-IF

           IF (MQ-REASON-CODE           EQUAL 2033) OR
              (MQ-REASON-CODE           EQUAL 2080)
               PERFORM 2800-FECHAR-FILA-MQ
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  MQ-REASON-CODE          NOT EQUAL ZEROS
               MOVE 'GET  '             TO WRK-FUNCAO
               MOVE MQ-COMPLETION-CODE  TO WRK-COM-CODE
               MOVE MQ-REASON-CODE      TO WRK-REASON-CODE
               MOVE 04                  TO WRK-LOCAL-98
               MOVE WRK-MSG98           TO WRK-MSG
               MOVE 98                  TO WRK-COD-RETORNO
               PERFORM 2800-FECHAR-FILA-MQ
               PERFORM 9999-TRATAR-ERRO-CICS
           END-IF.


      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2300-TRATAR-RETORNO             SECTION.
      *----------------------------------------------------------------*

           MOVE 1                      TO WRK-POS-AUX.
           MOVE WRK-DADOS-ENVIO(WRK-POS-AUX : LENGTH OF SIMM03-REGISTRO)
                                           TO SIMM03-REGISTRO.
           ADD  LENGTH OF SIMM03-REGISTRO  TO WRK-POS-AUX.

           MOVE WRK-DADOS-ENVIO(WRK-POS-AUX: LENGTH OF BVVEDN-BCMSG)
                                           TO  BVVEDN-BCMSG.
           ADD LENGTH OF  BVVEDN-BCMSG     TO WRK-POS-AUX.

           IF   SIMM03-CNRO-CTRL-INSTC-X(1:8) NOT EQUAL '0237BVVE' AND
                                                        '0237GRAN'
                GO TO 2300-99-FIM
           END-IF.

           EVALUATE WRK-DADOS-ENVIO(WRK-POS-AUX : 9)
               WHEN 'GEN0004  '
                   PERFORM 2310-TRATAR-GEN0004
               WHEN 'SCG0005R1'
                   PERFORM 2320-TRATAR-SCG0005R1
               WHEN 'SCG0005E '
                   PERFORM 2330-TRATAR-SCG0005E
               WHEN 'SCG0011R1'
                   PERFORM 2340-TRATAR-SCG0011R1
               WHEN 'SCG0011E '
                   PERFORM 2350-TRATAR-SCG0011E
               WHEN 'SCG0012R1'
                   PERFORM 2360-TRATAR-SCG0012R1
               WHEN 'SCG0012E '
                   PERFORM 2370-TRATAR-SCG0012E
               WHEN 'SCG0013R1'
                   PERFORM 2380-TRATAR-SCG0013R1
               WHEN 'SCG0013E '
                   PERFORM 2390-TRATAR-SCG0013E
           END-EVALUATE.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2310-TRATAR-GEN0004             SECTION.
      *----------------------------------------------------------------*

           PERFORM 2400-LER-TRAVPMSG.

           MOVE WRK-DADOS-ENVIO(WRK-POS-AUX :LENGTH OF BVVEDJ-GEN0004)
                                       TO BVVEDJ-GEN0004.
           MOVE BVVEDJ-ERRO-GEN        TO BVVED2-ERRO-GEN
           MOVE BVVEDJ-ISPB-EMISSOR    TO BVVED2-ISPB-EMISSOR
           MOVE BVVEDJ-ISPB-DESTIN     TO BVVED2-ISPB-DESTIN
           MOVE BVVEDJ-NUM-MQ          TO BVVED2-NUM-MQ
           MOVE BVVEDJ-NU-OP-OR        TO BVVED2-NU-OP-OR
           MOVE BVVEDJ-NUM-PROT-PSTA-OR  TO BVVED2-NUM-PROT-PSTA-OR
           MOVE BVVEDJ-HIST            TO BVVED2-HIST
           MOVE BVVEDJ-DT-HR-PART      TO BVVED2-DT-HR-PART

           MOVE  '04'                  TO BVVED2-STATUS.
           MOVE  00                    TO BVVED2-ERRO.

           PERFORM 2410-REGRAVAR-TRAVPMSG.

           PERFORM 2500-LER-TRAVPMAN.

           MOVE SPACES                 TO BVVED3-ERROS.
           MOVE  '04'                  TO BVVED3-STATUS.

           PERFORM 2510-REGRAVAR-TRAVPMAN.

      *----------------------------------------------------------------*
       2310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2320-TRATAR-SCG0005R1           SECTION.
      *----------------------------------------------------------------*
      *    ATUALIZA STATUS DA MENSAGEM ENVIADA

           PERFORM 2400-LER-TRAVPMSG.

           MOVE  '03'                  TO BVVED2-STATUS.
           MOVE  00                    TO BVVED2-ERRO.

           PERFORM 2410-REGRAVAR-TRAVPMSG.

      *    RECUPERAR REGISTRO DE ENVIO DA CONSULTA

           PERFORM 2600-LER-TRAVPCON.
           MOVE  BVVED4-CHAVE             TO WRK-CHAVE.

           MOVE WRK-DADOS-ENVIO(WRK-POS-AUX :LENGTH OF BVVEDB-SCG0005R1)
                                       TO BVVEDB-SCG0005R1.

      *--- VERIFICAR SE O GRUPO DOMICILIO BASE OU COMPL ESTA PREENCHIDO

           MOVE 1                      TO WRK-POS-AUX.
           MOVE BVVEDB-VARIAVEL(WRK-POS-AUX
                                      : LENGTH OF BVVEDB-GRP-DOMCL-BASE)
                                      TO BVVEDB-GRP-DOMCL-BASE

           ADD LENGTH OF BVVEDB-GRP-DOMCL-BASE
                                      TO WRK-POS-AUX.

           MOVE BVVEDB-VARIAVEL(WRK-POS-AUX :
                                      LENGTH OF BVVEDB-GRP-DOMCL-COMPL)
                                      TO BVVEDB-GRP-DOMCL-COMPL

      *--- CONSULTA VAZIA
           IF  BVVEDB-GRP-DOMCL-BASE   EQUAL ALL '*' AND
               BVVEDB-DOMCL-DADOS     EQUAL ALL '*'
               MOVE '03'               TO BVVED4-STATUS
               MOVE  03                TO BVVED4-SIT-CONS
               PERFORM 2610-REGRAVAR-TRAVPCON
               GO TO 2320-99-FIM
           END-IF.

      *--- TRATA CONSULTA COMPLETO - RETORNA APENAS UMA OCORRENCIA

           IF  BVVED4-SOLICT-RAIZ      EQUAL 'N'
               PERFORM 2321-TRATA-CONSULTA-COMPLETO
               GO TO 2320-99-FIM
           END-IF.

      *--- APENAS BASE PREENCHIDO
           IF  BVVEDB-DOMCL-DADOS      EQUAL ALL '*' AND
              (BVVEDB-GRP-DOMCL-BASE   NOT EQUAL ALL '*')
               MOVE BVVEDB-INDR-BLOQ-BASE TO BVVED4-INDR-BLOQ-DOMCL
               MOVE BVVEDB-TP-PRODT-BASE  TO BVVED4-TP-PRODT
               MOVE 'NAO'                 TO BVVED4-INST-DETENTORA
               ADD LENGTH OF BVVEDB-GRP-DOMCL-COMPL
                                          TO WRK-POS-AUX
               PERFORM 2324-GRAVAR-FIXO-FINAL
               GO TO 2320-99-FIM
           END-IF.

           PERFORM 2322-GRUPO-DOMCL-COMPL
           UNTIL BVVEDB-VARIAVEL(WRK-POS-AUX:6)
                           NOT EQUAL  '{1000}'.

           IF (BVVEDB-GRP-DOMCL-BASE   NOT EQUAL ALL '*')
               MOVE BVVEDB-INDR-BLOQ-BASE TO BVVED4-INDR-BLOQ-DOMCL
               MOVE BVVEDB-TP-PRODT-BASE  TO BVVED4-TP-PRODT
           ELSE
               MOVE WRK-TRAVA-ATIVA       TO BVVED4-INDR-BLOQ-DOMCL
               MOVE SPACES                TO BVVED4-TP-PRODT
           END-IF.

           EVALUATE TRUE
               WHEN WRK-BRAD              EQUAL 'S' AND
                    WRK-OUTRA             EQUAL 'S'
                    MOVE 'PARTE'          TO BVVED4-INST-DETENTORA
               WHEN WRK-BRAD              EQUAL 'N' AND
                    WRK-OUTRA             EQUAL 'S'
                    MOVE 'NAO'            TO BVVED4-INST-DETENTORA
               WHEN WRK-BRAD              EQUAL 'S' AND
                    WRK-OUTRA             EQUAL 'N'
                    MOVE 'SIM'            TO BVVED4-INST-DETENTORA
           END-EVALUATE.

      **   TRATA MANUTENCAO POR RAIZ, OS DADOS DA TRAVA VEM NOS
      **   DADOS COMPL
           IF  BVVEDB-TP-MANUT        EQUAL 'R' AND
               BVVED4-INST-DETENTORA  EQUAL 'SIM'
               MOVE WRK-DB-CLI-PROPRIO  TO BVVED4-CLI-PROPRIO
               MOVE WRK-DB-TP-NATUR-GAR TO BVVED4-TP-NATUR-GAR
               MOVE WRK-DB-NUM-IDENTC-SCG  TO BVVED4-NUM-IDENTC-SCG-X
               MOVE WRK-DB-TP-PRODT-COMPL  TO BVVED4-TP-PRODT
               MOVE WRK-DB-DTHR-MANUT      TO WRK-DT-HR-SIT
               MOVE CORR WRK-DT-HR-SIT     TO WRK-DT-HR-EDIT
               MOVE WRK-DT-HR-EDIT         TO BVVED4-DTHR-MANUT
           END-IF.

           PERFORM 2324-GRAVAR-FIXO-FINAL

           MOVE 1                      TO WRK-POS-AUX.
           ADD LENGTH OF BVVEDB-GRP-DOMCL-BASE
                                      TO WRK-POS-AUX.
           PERFORM 2620-GRAVAR-TRAVPCON
           UNTIL BVVEDB-VARIAVEL(WRK-POS-AUX:6)
                           NOT EQUAL  '{1000}'.

      *----------------------------------------------------------------*
       2320-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2321-TRATA-CONSULTA-COMPLETO    SECTION.
      *----------------------------------------------------------------*

           IF  BVVEDB-NUM-IDENTC-SCG   EQUAL ALL '*'
               MOVE 'NAO'              TO BVVED4-INST-DETENTORA
               INITIALIZE BVVED4-CLI-PROPRIO BVVED4-TP-NATUR-GAR
                                             BVVED4-VLR-MAX-RET
           ELSE
               MOVE 'SIM'               TO BVVED4-INST-DETENTORA
               MOVE BVVEDB-CLI-PROPRIO  TO BVVED4-CLI-PROPRIO
               MOVE BVVEDB-TP-NATUR-GAR TO BVVED4-TP-NATUR-GAR
               MOVE BVVEDB-VLR-MAX-RET  TO WRK-X-19
               MOVE WRK-9-13V99         TO BVVED4-VLR-MAX-RET
               MOVE BVVEDB-NUM-IDENTC-SCG  TO BVVED4-NUM-IDENTC-SCG-X
           END-IF.

           IF   BVVEDB-INDR-BLOQ-COMPL EQUAL 'N'
                MOVE ZEROS  TO BVVED4-VLR-MAX-RET
           ELSE
                IF   BVVEDB-INDR-BLOQ-COMPL EQUAL 'S' AND
                     BVVEDB-NUM-IDENTC-SCG   EQUAL ALL '*'
                     MOVE ZEROS  TO BVVED4-VLR-MAX-RET
                END-IF
           END-IF.

           MOVE BVVEDB-INDR-BLOQ-COMPL TO BVVED4-INDR-BLOQ-DOMCL
           MOVE BVVEDB-TP-PRODT-COMPL  TO BVVED4-TP-PRODT.

           MOVE BVVEDB-DTHR-MANUT      TO WRK-DT-HR-SIT
           MOVE CORR WRK-DT-HR-SIT     TO WRK-DT-HR-EDIT
           MOVE WRK-DT-HR-EDIT         TO BVVED4-DTHR-MANUT.

           ADD LENGTH OF BVVEDB-GRP-DOMCL-COMPL
                                             TO WRK-POS-AUX.
           PERFORM 2324-GRAVAR-FIXO-FINAL.

      *----------------------------------------------------------------*
       2321-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2322-GRUPO-DOMCL-COMPL          SECTION.
      *----------------------------------------------------------------*

           MOVE BVVEDB-VARIAVEL(WRK-POS-AUX :
                                      LENGTH OF BVVEDB-GRP-DOMCL-COMPL)
                                      TO BVVEDB-GRP-DOMCL-COMPL
                                         WRK-SALVA-DADOS-COMPL

           ADD LENGTH OF BVVEDB-GRP-DOMCL-COMPL
                                      TO WRK-POS-AUX.

           IF  BVVEDB-INDR-BLOQ-COMPL EQUAL 'S'
               MOVE 'S'               TO WRK-TRAVA-ATIVA
           END-IF.

           IF BVVEDB-NUM-IDENTC-SCG EQUAL ALL '*'
              MOVE 'S'            TO WRK-OUTRA
           ELSE
              MOVE 'S'            TO WRK-BRAD
           END-IF.

      *----------------------------------------------------------------*
       2322-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2324-GRAVAR-FIXO-FINAL          SECTION.
      *----------------------------------------------------------------*

           MOVE BVVEDB-TP-MANUT        TO BVVED4-TP-MANUT.
           MOVE  00                    TO BVVED4-ERRO
           MOVE '03'                   TO BVVED4-STATUS

           MOVE BVVEDB-VARIAVEL(WRK-POS-AUX
                                       : LENGTH OF BVVEDB-FIXO-FINAL)
                                       TO BVVEDB-FIXO-FINAL

           IF  BVVEDB-INDR-INF-ADCL    EQUAL '*'
               MOVE SPACES             TO BVVED4-INDR-INF-ADCL
           ELSE
               MOVE  BVVEDB-INDR-INF-ADCL
                                       TO BVVED4-INDR-INF-ADCL
           END-IF.

           MOVE BVVEDB-SIT-CONS        TO BVVED4-SIT-CONS

           IF  BVVEDB-TAM-ARQ          EQUAL ALL '*'
               MOVE SPACES             TO BVVED4-TAM-ARQ
           ELSE
               MOVE BVVEDB-TAM-ARQ     TO BVVED4-TAM-ARQ
           END-IF.

           IF  BVVEDB-IDENTD-ARQ       EQUAL ALL '*'
               MOVE SPACES             TO BVVED4-IDENTD-ARQ
           ELSE
               MOVE BVVEDB-IDENTD-ARQ  TO BVVED4-IDENTD-ARQ
           END-IF.

           MOVE BVVEDB-DT-HR-SIT       TO WRK-DT-HR-SIT
           MOVE CORR WRK-DT-HR-SIT     TO WRK-DT-HR-EDIT
           MOVE WRK-DT-HR-EDIT         TO BVVED4-DT-HR-SIT
           MOVE BVVEDB-DT-MOVTO        TO BVVED4-DT-MOVTO

           PERFORM 2610-REGRAVAR-TRAVPCON.

      *----------------------------------------------------------------*
       2324-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2330-TRATAR-SCG0005E           SECTION.
      *----------------------------------------------------------------*

           PERFORM 2400-LER-TRAVPMSG.

           MOVE  '03'                  TO BVVED2-STATUS.
           MOVE  99                    TO BVVED2-ERRO.

           PERFORM 2410-REGRAVAR-TRAVPMSG.

           PERFORM 2600-LER-TRAVPCON.

           MOVE SPACES                 TO BVVED4-ERROS.

           MOVE WRK-DADOS-ENVIO(WRK-POS-AUX :LENGTH OF BVVEDP-SCG0005E)
                                       TO BVVEDP-SCG0005E.

           IF  BVVEDP-COD-MSG-E        NOT EQUAL SPACES
               ADD 1                   TO WRK-IND
               MOVE BVVEDP-COD-MSG-E   TO  BVVED4-COD-ERRO(WRK-IND)
           END-IF.

           IF  BVVEDP-NUM-CTRL-IF-E    NOT EQUAL SPACES
               ADD 1                   TO WRK-IND
               MOVE BVVEDP-NUM-CTRL-IF-E
                                       TO  BVVED4-COD-ERRO(WRK-IND)
           END-IF.

           IF  BVVEDP-ISPB-IF-E        NOT EQUAL SPACES
               ADD 1                   TO WRK-IND
               MOVE BVVEDP-ISPB-IF-E   TO  BVVED4-COD-ERRO(WRK-IND)
           END-IF.

           IF  BVVEDP-NUM-IDENTC-SCG-E  NOT EQUAL SPACES
               ADD 1                   TO WRK-IND
               MOVE BVVEDP-NUM-IDENTC-SCG-E
                                       TO  BVVED4-COD-ERRO(WRK-IND)
           END-IF.

           IF  BVVEDP-TP-PESSOA-CLI-E  NOT EQUAL SPACES
               ADD 1                   TO WRK-IND
               MOVE BVVEDP-TP-PESSOA-CLI-E
                                       TO  BVVED4-COD-ERRO(WRK-IND)
           END-IF.

           IF  BVVEDP-CNPJ-CPF-CLI-E   NOT EQUAL SPACES
               ADD 1                   TO WRK-IND
               MOVE BVVEDP-CNPJ-CPF-CLI-E
                                       TO  BVVED4-COD-ERRO(WRK-IND)
           END-IF.

           IF  BVVEDP-CNPJ-BASE-CLI-E  NOT EQUAL SPACES
               ADD 1                   TO WRK-IND
               MOVE BVVEDP-CNPJ-BASE-CLI-E
                                       TO BVVED4-COD-ERRO(WRK-IND)
           END-IF.

           IF  BVVEDP-TP-RET-E         NOT EQUAL SPACES
               ADD 1                   TO WRK-IND
               MOVE BVVEDP-TP-RET-E    TO  BVVED4-COD-ERRO(WRK-IND)
           END-IF.

           IF  BVVEDP-DT-MOVTO-E       NOT EQUAL SPACES
               ADD 1                   TO WRK-IND
               MOVE BVVEDP-DT-MOVTO-E  TO  BVVED4-COD-ERRO(WRK-IND)
           END-IF.

           MOVE  '03'                  TO BVVED4-STATUS.
           MOVE  99                    TO BVVED4-ERRO.

           PERFORM 2610-REGRAVAR-TRAVPCON.

      *----------------------------------------------------------------*
       2330-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2340-TRATAR-SCG0011R1           SECTION.
      *----------------------------------------------------------------*

           PERFORM 2400-LER-TRAVPMSG.

           MOVE  '03'                  TO BVVED2-STATUS.
           MOVE  00                    TO BVVED2-ERRO.

           PERFORM 2410-REGRAVAR-TRAVPMSG.

           PERFORM 2500-LER-TRAVPMAN.

           MOVE WRK-DADOS-ENVIO(WRK-POS-AUX :LENGTH OF BVVEDD-SCG0011R1)
                                       TO BVVEDD-SCG0011R1.

           MOVE BVVEDD-NUM-CTRL-CIP    TO BVVED3-NUM-CTRL-CIP
           MOVE BVVEDD-NUM-IDENTC-SCG  TO BVVED3-NUM-IDENTC-SCG
           MOVE BVVEDD-DT-HR-SCG       TO BVVED3-DTHR-SCG

           MOVE  '03'                  TO BVVED3-STATUS.
           MOVE  00                    TO BVVED3-ERRO.

           PERFORM 2510-REGRAVAR-TRAVPMAN.
           PERFORM 2520-GRAVAR-TRAVPATI.

      *----------------------------------------------------------------*
       2340-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2350-TRATAR-SCG0011E           SECTION.
      *----------------------------------------------------------------*

           PERFORM 2400-LER-TRAVPMSG.

           MOVE  '03'                  TO BVVED2-STATUS.
           MOVE  99                    TO BVVED2-ERRO.

           PERFORM 2410-REGRAVAR-TRAVPMSG.

           PERFORM 2500-LER-TRAVPMAN.

           MOVE SPACES                 TO BVVED3-ERROS.

           MOVE WRK-DADOS-ENVIO(WRK-POS-AUX :LENGTH OF BVVEDQ-SCG0011E)
                                       TO BVVEDQ-SCG0011E.

           IF  BVVEDQ-COD-MSG-E        NOT EQUAL SPACES
               ADD 1                   TO WRK-IND
               MOVE BVVEDQ-COD-MSG-E   TO  BVVED3-COD-ERRO(WRK-IND)
           END-IF.

           IF  BVVEDQ-NUM-CTRL-IF-E    NOT EQUAL SPACES
               ADD 1                   TO WRK-IND
               MOVE BVVEDQ-NUM-CTRL-IF-E
                                       TO  BVVED3-COD-ERRO(WRK-IND)
           END-IF.

           IF  BVVEDQ-ISPB-IF-E        NOT EQUAL SPACES
               ADD 1                   TO WRK-IND
               MOVE BVVEDQ-ISPB-IF-E   TO  BVVED3-COD-ERRO(WRK-IND)
           END-IF.

           IF  BVVEDQ-TP-PESSOA-CLI-E  NOT EQUAL SPACES
               ADD 1                   TO WRK-IND
               MOVE BVVEDQ-TP-PESSOA-CLI-E
                                       TO  BVVED3-COD-ERRO(WRK-IND)
           END-IF.

           IF  BVVEDQ-CNPJ-CPF-CLI-E   NOT EQUAL SPACES
               ADD 1                   TO WRK-IND
               MOVE BVVEDQ-CNPJ-CPF-CLI-E
                                       TO  BVVED3-COD-ERRO(WRK-IND)
           END-IF.

           IF  BVVEDQ-CNPJ-BASE-CLI-E  NOT EQUAL SPACES
               ADD 1                   TO WRK-IND
               MOVE BVVEDQ-CNPJ-BASE-CLI-E
                                       TO BVVED3-COD-ERRO(WRK-IND)
           END-IF.

           IF  BVVEDQ-COD-PROPT-ESQM-E NOT EQUAL SPACES
               ADD 1                   TO WRK-IND
               MOVE BVVEDQ-COD-PROPT-ESQM-E
                                       TO  BVVED3-COD-ERRO(WRK-IND)
           END-IF.

           IF  BVVEDQ-TP-PRODT-E       NOT EQUAL SPACES
               ADD 1                   TO WRK-IND
               MOVE BVVEDQ-TP-PRODT-E  TO  BVVED3-COD-ERRO(WRK-IND)
           END-IF.

           IF  BVVEDQ-NUM-COD-IF-E     NOT EQUAL SPACES
               ADD 1                   TO WRK-IND
               MOVE BVVEDQ-NUM-COD-IF-E
                                       TO  BVVED3-COD-ERRO(WRK-IND)
           END-IF.

           IF  BVVEDQ-AG-CLI-E         NOT EQUAL SPACES
               ADD 1                   TO WRK-IND
               MOVE BVVEDQ-AG-CLI-E    TO  BVVED3-COD-ERRO(WRK-IND)
           END-IF.

           IF  BVVEDQ-CT-CLI-E         NOT EQUAL SPACES
               ADD 1                   TO WRK-IND
               MOVE BVVEDQ-CT-CLI-E    TO  BVVED3-COD-ERRO(WRK-IND)
           END-IF.

           IF  BVVEDQ-PERC-GAR-E       NOT EQUAL SPACES
               ADD 1                   TO WRK-IND
               MOVE BVVEDQ-PERC-GAR-E  TO  BVVED3-COD-ERRO(WRK-IND)
           END-IF.

           IF  BVVEDQ-DTINI-VALIDD-E   NOT EQUAL SPACES
               ADD 1                   TO WRK-IND
               MOVE BVVEDQ-DTINI-VALIDD-E
                                       TO  BVVED3-COD-ERRO(WRK-IND)
           END-IF.

           IF  BVVEDQ-DTFIM-VALIDD-E   NOT EQUAL SPACES
               ADD 1                   TO WRK-IND
               MOVE BVVEDQ-DTFIM-VALIDD-E
                                       TO  BVVED3-COD-ERRO(WRK-IND)
           END-IF.

           IF  BVVEDQ-TP-NATU-GAR-E    NOT EQUAL SPACES
               ADD 1                   TO WRK-IND
               MOVE BVVEDQ-TP-NATU-GAR-E
                                       TO  BVVED3-COD-ERRO(WRK-IND)
           END-IF.

           IF  BVVEDQ-DT-MOVTO-E       NOT EQUAL SPACES
               ADD 1                   TO WRK-IND
               MOVE BVVEDQ-DT-MOVTO-E  TO  BVVED3-COD-ERRO(WRK-IND)
           END-IF.

           MOVE  '03'                  TO BVVED3-STATUS.
           MOVE  99                    TO BVVED3-ERRO.

           PERFORM 2510-REGRAVAR-TRAVPMAN.

      *----------------------------------------------------------------*
       2350-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2360-TRATAR-SCG0012R1           SECTION.
      *----------------------------------------------------------------*

           PERFORM 2400-LER-TRAVPMSG.

           MOVE  '03'                  TO BVVED2-STATUS.
           MOVE  00                    TO BVVED2-ERRO.

           PERFORM 2410-REGRAVAR-TRAVPMSG.

           PERFORM 2500-LER-TRAVPMAN.

           MOVE WRK-DADOS-ENVIO(WRK-POS-AUX :LENGTH OF BVVEDF-SCG0012R1)
                                       TO BVVEDF-SCG0012R1.

           MOVE BVVEDF-NUM-CTRL-CIP    TO BVVED3-NUM-CTRL-CIP
           MOVE BVVEDF-DT-HR-SCG       TO BVVED3-DTHR-SCG
           MOVE BVVEDF-DT-MOVTO        TO BVVED3-DT-MOVTO

           MOVE  '03'                  TO BVVED3-STATUS.
           MOVE  00                    TO BVVED3-ERRO.

           PERFORM 2361-LER-TRAVPATI.

      *    GRAVA TRAVPMAN COM A AGENCIA E CONTA ANTIGA
           MOVE BVVEDW-AG-CLI          TO BVVED3-AGENCIA-OLD
           MOVE BVVEDW-CTA-CLI         TO BVVED3-CONTA-OLD
           MOVE BVVEDW-COD-PROP-ESQM   TO BVVED3-COD-PROPT-ESQM

           PERFORM 2510-REGRAVAR-TRAVPMAN.

           MOVE BVVED3-AG-CLI          TO BVVEDW-AG-CLI
           MOVE BVVED3-CT-CLI          TO BVVEDW-CTA-CLI
           MOVE BVVED3-DTINI-VALIDD    TO BVVEDW-DTINI-VALIDD
           MOVE BVVED3-DTFIM-VALIDD    TO BVVEDW-DTFIM-VALIDD
           MOVE BVVED3-VLR-MAX-RET     TO BVVEDW-VLR-MAX-RET
           MOVE BVVED3-TIMESTAMP       TO WRK-TIMESTAMP
           MOVE CORR WRK-TIMESTAMP     TO WRK-TIMESTAMP-EDIT
           MOVE WRK-TIMESTAMP-EDIT     TO BVVEDW-TIMES-MANUT

           PERFORM 2362-REGRAVAR-TRAVPATI.

      *----------------------------------------------------------------*
       2360-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2361-LER-TRAVPATI               SECTION.
      *----------------------------------------------------------------*

           INITIALIZE BVVEDW-TRAVPATI.

           MOVE SPACES                 TO BVVEDW-CH-FILLER.
           MOVE BVVED3-CNPJ            TO BVVEDW-CH-CNPJ-CPF
           MOVE BVVED3-FILIAL          TO BVVEDW-CH-FILIAL
           MOVE BVVED3-COD-BANDEIRA    TO BVVEDW-CH-COD-BAND

           EXEC CICS
                READ     FILE   ('TRAVPATI')
                         INTO   (BVVEDW-TRAVPATI)
                         LENGTH (LENGTH OF BVVEDW-TRAVPATI)
                         RIDFLD (BVVEDW-CHAVE)
                         EQUAL
                         UPDATE
           END-EXEC.

           IF  EIBRESP                 NOT EQUAL DFHRESP (NORMAL)
               MOVE 'READ'             TO WRK-FUNCAO-97
               MOVE EIBRESP            TO WRK-EIBRESP-97
               MOVE 05                 TO WRK-LOCAL-97
               MOVE 'TRAVPATI'         TO WRK-ARQUIVO-97
               MOVE WRK-MSG97          TO WRK-MSG
               MOVE 97                 TO WRK-COD-RETORNO
               PERFORM 9999-TRATAR-ERRO-CICS
           END-IF.

      *----------------------------------------------------------------*
       2361-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2362-REGRAVAR-TRAVPATI          SECTION.
      *----------------------------------------------------------------*

           EXEC CICS
                REWRITE DATASET  ('TRAVPATI')
                        FROM     (BVVEDW-TRAVPATI)
                        LENGTH   (LENGTH OF BVVEDW-TRAVPATI)
                        NOHANDLE
           END-EXEC

           IF  EIBRESP                 NOT EQUAL DFHRESP (NORMAL)
               MOVE 'WRITE'            TO WRK-FUNCAO-97
               MOVE EIBRESP            TO WRK-EIBRESP-97
               MOVE 06                 TO WRK-LOCAL-97
               MOVE 'TRAVPATI'         TO WRK-ARQUIVO-97
               MOVE WRK-MSG97          TO WRK-MSG
               MOVE 97                 TO WRK-COD-RETORNO
               PERFORM 9999-TRATAR-ERRO-CICS
           END-IF.

      *----------------------------------------------------------------*
       2362-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2370-TRATAR-SCG0012E           SECTION.
      *----------------------------------------------------------------*

           PERFORM 2400-LER-TRAVPMSG.

           MOVE  '03'                  TO BVVED2-STATUS.
           MOVE  99                    TO BVVED2-ERRO.

           PERFORM 2410-REGRAVAR-TRAVPMSG.

           PERFORM 2500-LER-TRAVPMAN.

           MOVE SPACES                 TO BVVED3-ERROS.

           MOVE WRK-DADOS-ENVIO(WRK-POS-AUX :LENGTH OF BVVEDR-SCG0012E)
                                       TO BVVEDR-SCG0012E.

           IF  BVVEDR-COD-MSG-E        NOT EQUAL SPACES
               ADD 1                   TO WRK-IND
               MOVE BVVEDR-COD-MSG-E   TO  BVVED3-COD-ERRO(WRK-IND)
           END-IF.

           IF  BVVEDR-NUM-CTRL-IF-E    NOT EQUAL SPACES
               ADD 1                   TO WRK-IND
               MOVE BVVEDR-NUM-CTRL-IF-E
                                       TO  BVVED3-COD-ERRO(WRK-IND)
           END-IF.

           IF  BVVEDR-ISPB-IF-E        NOT EQUAL SPACES
               ADD 1                   TO WRK-IND
               MOVE BVVEDR-ISPB-IF-E   TO  BVVED3-COD-ERRO(WRK-IND)
           END-IF.

           IF  BVVEDR-NUM-COD-IF-E     NOT EQUAL SPACES
               ADD 1                   TO WRK-IND
               MOVE BVVEDR-NUM-COD-IF-E
                                       TO  BVVED3-COD-ERRO(WRK-IND)
           END-IF.

           IF  BVVEDR-NUM-IDENTC-SCG-E NOT EQUAL SPACES
               ADD 1                   TO WRK-IND
               MOVE BVVEDR-NUM-IDENTC-SCG-E
                                       TO  BVVED3-COD-ERRO(WRK-IND)
           END-IF.


           IF  BVVEDQ-AG-CLI-E         NOT EQUAL SPACES
               ADD 1                   TO WRK-IND
               MOVE BVVEDQ-AG-CLI-E    TO  BVVED3-COD-ERRO(WRK-IND)
           END-IF.

           IF  BVVEDQ-CT-CLI-E         NOT EQUAL SPACES
               ADD 1                   TO WRK-IND
               MOVE BVVEDQ-CT-CLI-E    TO  BVVED3-COD-ERRO(WRK-IND)
           END-IF.

           IF  BVVEDQ-PERC-GAR-E       NOT EQUAL SPACES
               ADD 1                   TO WRK-IND
               MOVE BVVEDQ-PERC-GAR-E  TO  BVVED3-COD-ERRO(WRK-IND)
           END-IF.

           IF  BVVEDQ-DTINI-VALIDD-E   NOT EQUAL SPACES
               ADD 1                   TO WRK-IND
               MOVE BVVEDQ-DTINI-VALIDD-E
                                       TO  BVVED3-COD-ERRO(WRK-IND)
           END-IF.

           IF  BVVEDQ-DTFIM-VALIDD-E   NOT EQUAL SPACES
               ADD 1                   TO WRK-IND
               MOVE BVVEDQ-DTFIM-VALIDD-E
                                       TO  BVVED3-COD-ERRO(WRK-IND)
           END-IF.

           IF  BVVEDQ-TP-NATU-GAR-E    NOT EQUAL SPACES
               ADD 1                   TO WRK-IND
               MOVE BVVEDQ-TP-NATU-GAR-E
                                       TO  BVVED3-COD-ERRO(WRK-IND)
           END-IF.

           IF  BVVEDQ-DT-MOVTO-E       NOT EQUAL SPACES
               ADD 1                   TO WRK-IND
               MOVE BVVEDQ-DT-MOVTO-E  TO  BVVED3-COD-ERRO(WRK-IND)
           END-IF.

           MOVE  '03'                  TO BVVED3-STATUS.
           MOVE  99                    TO BVVED3-ERRO.

           PERFORM 2510-REGRAVAR-TRAVPMAN.


      *----------------------------------------------------------------*
       2370-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2380-TRATAR-SCG0013R1           SECTION.
      *----------------------------------------------------------------*

           PERFORM 2400-LER-TRAVPMSG.

           MOVE  '03'                  TO BVVED2-STATUS.
           MOVE  00                    TO BVVED2-ERRO.

           PERFORM 2410-REGRAVAR-TRAVPMSG.

           PERFORM 2500-LER-TRAVPMAN.

           MOVE WRK-DADOS-ENVIO(WRK-POS-AUX :LENGTH OF BVVEDF-SCG0012R1)
                                       TO BVVEDF-SCG0012R1.

           MOVE BVVEDF-NUM-CTRL-CIP    TO BVVED3-NUM-CTRL-CIP
           MOVE BVVEDF-DT-HR-SCG       TO BVVED3-DTHR-SCG
           MOVE BVVEDF-DT-MOVTO        TO BVVED3-DT-MOVTO

           MOVE  '03'                  TO BVVED3-STATUS.
           MOVE  00                    TO BVVED3-ERRO.

           PERFORM 2510-REGRAVAR-TRAVPMAN.
           PERFORM 2381-DELETAR-TRAVPATI.

      *----------------------------------------------------------------*
       2380-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2381-DELETAR-TRAVPATI           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE BVVEDW-TRAVPATI.

           MOVE SPACES                 TO BVVEDW-CH-FILLER.
           MOVE BVVED3-CNPJ            TO BVVEDW-CH-CNPJ-CPF
           MOVE BVVED3-FILIAL          TO BVVEDW-CH-FILIAL
           MOVE BVVED3-COD-BANDEIRA    TO BVVEDW-CH-COD-BAND

           EXEC CICS
                DELETE   FILE   ('TRAVPATI')
                         RIDFLD (BVVEDW-CHAVE)
           END-EXEC.

           IF  EIBRESP                 NOT EQUAL DFHRESP (NORMAL)
               MOVE 'READ'             TO WRK-FUNCAO-97
               MOVE EIBRESP            TO WRK-EIBRESP-97
               MOVE 07                 TO WRK-LOCAL-97
               MOVE 'TRAVPATI'         TO WRK-ARQUIVO-97
               MOVE WRK-MSG97          TO WRK-MSG
               MOVE 97                 TO WRK-COD-RETORNO
               PERFORM 9999-TRATAR-ERRO-CICS
           END-IF.

      *----------------------------------------------------------------*
       2381-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2390-TRATAR-SCG0013E           SECTION.
      *----------------------------------------------------------------*

           PERFORM 2400-LER-TRAVPMSG.

           MOVE  '03'                  TO BVVED2-STATUS.
           MOVE  99                    TO BVVED2-ERRO.

           PERFORM 2410-REGRAVAR-TRAVPMSG.

           PERFORM 2500-LER-TRAVPMAN.

           MOVE SPACES                 TO BVVED3-ERROS.

           MOVE WRK-DADOS-ENVIO(WRK-POS-AUX :LENGTH OF BVVEDS-SCG0013E)
                                       TO BVVEDS-SCG0013E.

           IF  BVVEDS-COD-MSG-E        NOT EQUAL SPACES
               ADD 1                   TO WRK-IND
               MOVE BVVEDS-COD-MSG-E   TO BVVED3-COD-ERRO(WRK-IND)
           END-IF.

           IF  BVVEDS-NUM-CTRL-IF-E    NOT EQUAL SPACES
               ADD 1                   TO WRK-IND
               MOVE BVVEDS-NUM-CTRL-IF-E
                                       TO  BVVED3-COD-ERRO(WRK-IND)
           END-IF.

           IF  BVVEDS-ISPB-IF-E        NOT EQUAL SPACES
               ADD 1                   TO WRK-IND
               MOVE BVVEDS-ISPB-IF-E   TO  BVVED3-COD-ERRO(WRK-IND)
           END-IF.

           IF  BVVEDS-NUM-IDENTC-SCG-E NOT EQUAL SPACES
               ADD 1                   TO WRK-IND
               MOVE BVVEDS-NUM-IDENTC-SCG-E
                                       TO  BVVED3-COD-ERRO(WRK-IND)
           END-IF.

           IF  BVVEDS-TP-BAIXA-DOMCL-E  NOT EQUAL SPACES
               ADD 1                   TO WRK-IND
               MOVE BVVEDS-TP-BAIXA-DOMCL-E
                                       TO  BVVED3-COD-ERRO(WRK-IND)
           END-IF.

           IF  BVVEDS-DT-MOVTO-E       NOT EQUAL SPACES
               ADD 1                   TO WRK-IND
               MOVE BVVEDS-DT-MOVTO-E  TO  BVVED3-COD-ERRO(WRK-IND)
           END-IF.

           MOVE  '03'                  TO BVVED3-STATUS.
           MOVE  99                    TO BVVED3-ERRO.

           PERFORM 2510-REGRAVAR-TRAVPMAN.

      *----------------------------------------------------------------*
       2390-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2400-LER-TRAVPMSG               SECTION.
      *----------------------------------------------------------------*

           INITIALIZE BVVED2-TRVPMSG.

           MOVE SPACES                    TO BVVED2-FILLER-CHV.
           MOVE SIMM03-CNRO-CTRL-INSTC-X  TO BVVED2-NUM-CTRL-IF.

           EXEC CICS
                READ     FILE   ('TRAVPMSG')
                         INTO   (BVVED2-TRVPMSG)
                         LENGTH (LENGTH OF BVVED2-TRVPMSG)
                         RIDFLD (BVVED2-CHAVE)
                         EQUAL
                         UPDATE
           END-EXEC.

           IF  EIBRESP                 NOT EQUAL DFHRESP (NORMAL)
               MOVE 'READ'             TO WRK-FUNCAO-97
               MOVE EIBRESP            TO WRK-EIBRESP-97
               MOVE 08                 TO WRK-LOCAL-97
               MOVE 'TRAVPMSG'         TO WRK-ARQUIVO-97
               MOVE WRK-MSG97          TO WRK-MSG
               MOVE 97                 TO WRK-COD-RETORNO
               PERFORM 9999-TRATAR-ERRO-CICS
           END-IF.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2410-REGRAVAR-TRAVPMSG          SECTION.
      *----------------------------------------------------------------*

           EXEC CICS
                REWRITE DATASET  ('TRAVPMSG')
                        FROM     (BVVED2-TRVPMSG)
                        LENGTH   (LENGTH OF BVVED2-TRVPMSG)
                        NOHANDLE
           END-EXEC

           IF  EIBRESP                 NOT EQUAL DFHRESP (NORMAL)
               MOVE 'WRITE'            TO WRK-FUNCAO-97
               MOVE EIBRESP            TO WRK-EIBRESP-97
               MOVE 09                 TO WRK-LOCAL-97
               MOVE 'TRAVPMSG'         TO WRK-ARQUIVO-97
               MOVE WRK-MSG97          TO WRK-MSG
               MOVE 97                 TO WRK-COD-RETORNO
               PERFORM 9999-TRATAR-ERRO-CICS
           END-IF.

      *----------------------------------------------------------------*
       2410-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2500-LER-TRAVPMAN               SECTION.
      *----------------------------------------------------------------*

           INITIALIZE BVVED3-TRAVPMAN.

           MOVE SPACES                     TO BVVED3-FILLER-CHV.
           MOVE BVVED2-CNPJ                TO BVVED3-CNPJ
           MOVE BVVED2-FILIAL              TO BVVED3-FILIAL
           MOVE SIMM03-CNRO-CTRL-INSTC-X   TO BVVED3-NUM-CTRL-IF.

           EXEC CICS
                READ     FILE   ('TRAVPMAN')
                         INTO   (BVVED3-TRAVPMAN)
                         LENGTH (LENGTH OF BVVED3-TRAVPMAN)
                         RIDFLD (BVVED3-CHAVE)
                         EQUAL
                         UPDATE
           END-EXEC.

           IF  EIBRESP                 NOT EQUAL DFHRESP (NORMAL)
               MOVE 'READ'             TO WRK-FUNCAO-97
               MOVE EIBRESP            TO WRK-EIBRESP-97
               MOVE 10                 TO WRK-LOCAL-97
               MOVE 'TRAVPMAN'         TO WRK-ARQUIVO-97
               MOVE WRK-MSG97          TO WRK-MSG
               MOVE 97                 TO WRK-COD-RETORNO
               PERFORM 9999-TRATAR-ERRO-CICS
           END-IF.

      *----------------------------------------------------------------*
       2500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2510-REGRAVAR-TRAVPMAN          SECTION.
      *----------------------------------------------------------------*

           EXEC CICS
                REWRITE DATASET  ('TRAVPMAN')
                        FROM     (BVVED3-TRAVPMAN)
                        LENGTH   (LENGTH OF BVVED3-TRAVPMAN)
                        NOHANDLE
           END-EXEC

           IF  EIBRESP                 NOT EQUAL DFHRESP (NORMAL)
               MOVE 'WRITE'            TO WRK-FUNCAO-97
               MOVE EIBRESP            TO WRK-EIBRESP-97
               MOVE 11                 TO WRK-LOCAL-97
               MOVE 'TRAVPMAN'         TO WRK-ARQUIVO-97
               MOVE WRK-MSG97          TO WRK-MSG
               MOVE 97                 TO WRK-COD-RETORNO
               PERFORM 9999-TRATAR-ERRO-CICS
           END-IF.

      *----------------------------------------------------------------*
       2510-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2520-GRAVAR-TRAVPATI            SECTION.
      *----------------------------------------------------------------*

           INITIALIZE BVVEDW-TRAVPATI.

           MOVE BVVED3-CNPJ            TO BVVEDW-CH-CNPJ-CPF
           MOVE BVVED3-FILIAL          TO BVVEDW-CH-FILIAL
           MOVE BVVED3-COD-BANDEIRA    TO BVVEDW-CH-COD-BAND
           MOVE BVVED3-NUM-IDENTC-SCG  TO BVVEDW-NUM-IDENT-SCG

           IF   BVVED3-TP-PESSOA-CLI      EQUAL SPACES
                MOVE 'J'                  TO BVVEDW-TP-PSSOA
           ELSE
                MOVE BVVED3-TP-PESSOA-CLI TO BVVEDW-TP-PSSOA
                MOVE BVVED3-CNPJ-CPF-CLI(13:2)
                                          TO BVVEDW-CTRL-CNPJ-CPF
           END-IF.

           MOVE BVVED3-TP-PRODT        TO BVVEDW-TP-PRODT
           MOVE BVVED3-AG-CLI          TO BVVEDW-AG-CLI
           MOVE BVVED3-CT-CLI          TO BVVEDW-CTA-CLI
           MOVE BVVED3-PERC-GAR        TO BVVEDW-PERC-GAR
           MOVE BVVED3-DTINI-VALIDD    TO BVVEDW-DTINI-VALIDD
           MOVE BVVED3-DTFIM-VALIDD    TO BVVEDW-DTFIM-VALIDD
           MOVE BVVED3-TIMESTAMP       TO WRK-TIMESTAMP
           MOVE CORR WRK-TIMESTAMP     TO WRK-TIMESTAMP-EDIT
           MOVE WRK-TIMESTAMP-EDIT     TO BVVEDW-TIMES-MANUT
           MOVE BVVED3-COD-PROPT-ESQM  TO BVVEDW-COD-PROP-ESQM
           MOVE BVVED3-VLR-MAX-RET     TO BVVEDW-VLR-MAX-RET

           EXEC CICS
               WRITE   DATASET  ('TRAVPATI')
                       FROM     (BVVEDW-TRAVPATI)
                       RIDFLD   (BVVEDW-CHAVE)
                       NOHANDLE
           END-EXEC

           IF  EIBRESP                 NOT EQUAL DFHRESP (NORMAL)
               MOVE 'WRITE'            TO WRK-FUNCAO-97
               MOVE EIBRESP            TO WRK-EIBRESP-97
               MOVE 12                 TO WRK-LOCAL-97
               MOVE 'TRAVPATI'         TO WRK-ARQUIVO-97
               MOVE WRK-MSG97          TO WRK-MSG
               MOVE 97                 TO WRK-COD-RETORNO
               PERFORM 9999-TRATAR-ERRO-CICS
           END-IF.

      *----------------------------------------------------------------*
       2520-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2600-LER-TRAVPCON               SECTION.
      *----------------------------------------------------------------*

           INITIALIZE BVVED4-CHAVE.
           MOVE SPACES                     TO BVVED4-FILLER-CHV.
           MOVE BVVED2-CNPJ                TO BVVED4-CNPJ
           MOVE BVVED2-FILIAL              TO BVVED4-FILIAL
           MOVE SIMM03-CNRO-CTRL-INSTC-X   TO BVVED4-NUM-CTRL-IF.

           EXEC CICS
                READ     FILE   ('TRAVPCON')
                         INTO   (BVVED4-TRAVPCON)
                         LENGTH (LENGTH OF BVVED4-TRAVPCON)
                         RIDFLD (BVVED4-CHAVE)
                         EQUAL
                         UPDATE
           END-EXEC.

           IF  EIBRESP                 NOT EQUAL DFHRESP (NORMAL)
               MOVE 'READ'             TO WRK-FUNCAO-97
               MOVE EIBRESP            TO WRK-EIBRESP-97
               MOVE 13                 TO WRK-LOCAL-97
               MOVE 'TRAVPCON'         TO WRK-ARQUIVO-97
               MOVE WRK-MSG97          TO WRK-MSG
               MOVE 97                 TO WRK-COD-RETORNO
               PERFORM 9999-TRATAR-ERRO-CICS
           END-IF.

      *----------------------------------------------------------------*
       2600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2610-REGRAVAR-TRAVPCON          SECTION.
      *----------------------------------------------------------------*

           EXEC CICS
                REWRITE DATASET  ('TRAVPCON')
                        FROM     (BVVED4-TRAVPCON)
                        LENGTH   (LENGTH OF BVVED4-TRAVPCON)
                        NOHANDLE
           END-EXEC

           IF  EIBRESP                 NOT EQUAL DFHRESP (NORMAL) AND
               EIBRESP                 NOT EQUAL DFHRESP (DUPKEY)
               MOVE 'WRITE'            TO WRK-FUNCAO-97
               MOVE EIBRESP            TO WRK-EIBRESP-97
               MOVE 14                 TO WRK-LOCAL-97
               MOVE 'TRAVPCON'         TO WRK-ARQUIVO-97
               MOVE WRK-MSG97          TO WRK-MSG
               MOVE 97                 TO WRK-COD-RETORNO
               PERFORM 9999-TRATAR-ERRO-CICS
           END-IF.

      *----------------------------------------------------------------*
       2610-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2620-GRAVAR-TRAVPCON            SECTION.
      *----------------------------------------------------------------*

           MOVE BVVEDB-VARIAVEL(WRK-POS-AUX : LENGTH OF
                                BVVEDB-GRP-DOMCL-COMPL)
                                     TO BVVEDB-GRP-DOMCL-COMPL.

           MOVE ALL '*'                TO BVVED4-CNPJ-BASE-CLI

           IF BVVEDB-NUM-IDENTC-SCG    EQUAL ALL '*'
              MOVE 'NAO'               TO BVVED4-INST-DETENTORA
              INITIALIZE BVVED4-CLI-PROPRIO BVVED4-TP-NATUR-GAR
           ELSE
              MOVE 'SIM'               TO BVVED4-INST-DETENTORA
              MOVE BVVEDB-CLI-PROPRIO(1:38)
                                       TO BVVED4-CLI-PROPRIO
              MOVE BVVEDB-TP-NATUR-GAR TO BVVED4-TP-NATUR-GAR
              MOVE BVVEDB-NUM-IDENTC-SCG  TO BVVED4-NUM-IDENTC-SCG-X
           END-IF

           MOVE BVVEDB-TP-PESSOA-CLI   TO BVVED4-TP-PESSOA-CLI
           MOVE BVVEDB-CNPJ-CPF-CLI    TO BVVED4-CNPJ-CPF-CLI
                                          WRK-FORMAT-CGCCPF-R

           IF  BVVEDB-TP-PESSOA-CLI    EQUAL 'F'
               MOVE WRK-CPF            TO    BVVED4-CNPJ
               MOVE ZEROS              TO    BVVED4-FILIAL
           ELSE
               MOVE WRK-NROCNPJ        TO    BVVED4-CNPJ
               MOVE WRK-FILIAL         TO    BVVED4-FILIAL
           END-IF

           MOVE BVVEDB-TP-PRODT-COMPL  TO BVVED4-TP-PRODT
           MOVE BVVEDB-DTHR-MANUT      TO WRK-DT-HR-SIT
           MOVE CORR WRK-DT-HR-SIT     TO WRK-DT-HR-EDIT
           MOVE WRK-DT-HR-EDIT         TO BVVED4-DTHR-MANUT
           MOVE BVVEDB-INDR-BLOQ-COMPL TO BVVED4-INDR-BLOQ-DOMCL

           EXEC CICS
               WRITE   DATASET  ('TRAVPCON')
                       FROM     (BVVED4-TRAVPCON)
                       RIDFLD   (BVVED4-CHAVE)
                       NOHANDLE
           END-EXEC

           IF  EIBRESP               NOT EQUAL DFHRESP (NORMAL) AND
               EIBRESP               NOT EQUAL DFHRESP (DUPKEY)
               MOVE 'WRITE'          TO WRK-FUNCAO-97
               MOVE EIBRESP          TO WRK-EIBRESP-97
               MOVE 15               TO WRK-LOCAL-97
               MOVE 'TRAVPCON'         TO WRK-ARQUIVO-97
               MOVE WRK-MSG97        TO WRK-MSG
               MOVE 97               TO WRK-COD-RETORNO
               PERFORM 3000-FINALIZAR
           END-IF

           ADD  LENGTH OF BVVEDB-GRP-DOMCL-COMPL  TO WRK-POS-AUX.

      *----------------------------------------------------------------*
       2620-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2800-FECHAR-FILA-MQ             SECTION.
      *----------------------------------------------------------------*

           MOVE 3                      TO MQ-FORMATO.
           MOVE 'CLOSE'                TO MQ-FUNCAO.

           EXEC    CICS   LINK
                          PROGRAM  ('SBAT3510')
                          COMMAREA (MQ-MCA)
                          LENGTH   (LENGTH OF MQ-MCA)
                          NOHANDLE
           END-EXEC.

           IF  EIBRESP                  NOT EQUAL DFHRESP(NORMAL)
               MOVE EIBRESP             TO WRK-EIBRESP
               MOVE 16                  TO WRK-LOCAL-99
               MOVE WRK-MSG99           TO WRK-MSG
               MOVE 99                 TO WRK-COD-RETORNO
               PERFORM 9999-TRATAR-ERRO-CICS
           END-IF

           IF  MQ-REASON-CODE          NOT EQUAL ZEROS
               MOVE 'CLOSE'            TO WRK-FUNCAO
               MOVE MQ-COMPLETION-CODE TO WRK-COM-CODE
               MOVE MQ-REASON-CODE     TO WRK-REASON-CODE
               MOVE 17                 TO WRK-LOCAL-98
               MOVE WRK-MSG98          TO WRK-MSG
               MOVE 98                 TO WRK-COD-RETORNO
               PERFORM 9999-TRATAR-ERRO-CICS
           END-IF.

      *----------------------------------------------------------------*
       2800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    FINALIZACAO.                                                *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           MOVE LOW-VALUES             TO  DFHCOMMAREA(1:EIBCALEN).
           MOVE WRK-COMMAREA           TO  DFHCOMMAREA(1:EIBCALEN).

           EXEC CICS
               RETURN
           END-EXEC.

           IF  EIBRESP                 NOT EQUAL DFHRESP(NORMAL)
               MOVE 99                 TO  WRK-COD-RETORNO
           END-IF.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999-TRATAR-ERRO-CICS           SECTION.
      *----------------------------------------------------------------*

           MOVE 'HANDLE'               TO WRK-MODULO.
           MOVE EIBRESP                TO WRK-EIBRESP-CDES
           MOVE 18                     TO WRK-LOCAL.

           PERFORM 3000-FINALIZAR.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

