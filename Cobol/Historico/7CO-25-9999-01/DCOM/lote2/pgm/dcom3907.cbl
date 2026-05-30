      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID.     DCOM3907.
       AUTHOR.         FABRICA - ALTRAN.
      *================================================================*
      *                          A L T R A N                           *
      *----------------------------------------------------------------*
      *     PROGRAMA....: DCOM3907                                     *
      *     PROGRAMADOR.: FABRICA - ALTRAN.                            *
      *     ANALISTA....: MARCELINO - ALTRAN.                          *
      *     DATA........: 05/01/2007                                   *
      *----------------------------------------------------------------*
      *     OBJETIVO....: LISTA PARA SELECAO                           *
      *                   SELECAO DE CONVENIO CLIENTE                  *
      *----------------------------------------------------------------*
      *     TELAS.......:                                              *
      *     DCOM9GTM....: LISTA PARA SELECAO                           *
      *                   SELECAO DE CONVENIO CLIENTE                  *
      *----------------------------------------------------------------*

      *     INC'S.......:                                              *
      *     I#BRAD7C - AREA DE ERRO DA BRAD7100                        *
      *     I#DCOMSG - TABELA DE MENSAGENS DE RETORNO DO               *
      *                SISTEMA DCOM (ON-LINE)                          *
      *----------------------------------------------------------------*
      *     MODULOS.....:                                              *
      *     BRAD0660 - INSERIR CARACTERES DE NULL NA MENSAGEM DE SAIDA *
      *     POOL5005 - CONTROLE DO TRANSITO DE MENSAGENS NO IMS/DC     *
      *     BRAD7100 - TRATAMENTO DE ERROS QUANDO PROGRAMA INVALIDO    *
      *----------------------------------------------------------------*
      *     NAVEGACAO...:                                              *
      *     CHAMADO POR.: DCOM0103                                     *
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
       77  FILLER                      PIC  X(50)          VALUE
           '*** DCOM3907 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*
       77  WRK-IND                     PIC S9(09) COMP     VALUE ZEROS.
       77  WIND-SEL                    PIC S9(09) COMP     VALUE ZEROS.
       77  WCT-SELEC                   PIC S9(09) COMP     VALUE ZEROS.

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE

           '*** AREA DE ATRIBUTOS ***'.
      *---------------------------------------------------------------*
      *--- ALPHA    - NORMAL    - PROTEGIDO    - NAO POS CURSOR ---*
       77  WRK-225                     PIC S9(08) COMP     VALUE +241.
      *--- NUMERIC  - NORMAL    - PROTEGIDO    - NAO POS CURSOR ---*
       77  WRK-241                     PIC S9(08) COMP     VALUE +241.
      *--- BRILHANTE, NORMAL    - DEPROTEGIDO  - POS CURSOR - ALFA ---*
       77  WRK-49353                   PIC S9(08) COMP     VALUE +49353.
      *--- NUMERICO - NORMAL    - PROTEGIDO    - POS. CURSOR    ---*
       77  WRK-49361                   PIC S9(08) COMP     VALUE +49361.
      *--- NUMERICO - BRILHANTE - DESPROTEGIDO - POS. CURSOR    ---*
       77  WRK-49369                   PIC S9(08) COMP     VALUE +49369.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*
       01  WRK-AUXILIARES.
           05  WRK-MODULO              PIC  X(08)          VALUE SPACES.
           05  WRK-INCONSIS            PIC  X(01)          VALUE SPACES.


      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DE COMUNICACAO DCOM0103 ***'.
      *----------------------------------------------------------------*
       01  COMU-AREA.
           05  COMU-LL                 PIC S9(004)  COMP   VALUE +343.
           05  COMU-ZZ                 PIC S9(004)  COMP   VALUE ZEROS.
           05  COMU-TRANCODE.
               10  COMU-TRANSACAO      PIC  X(008)         VALUE SPACES.
               10  FILLER              PIC  X(006)         VALUE SPACES.
               10  COMU-PFK            PIC  X(001)         VALUE SPACES.
           05  COMU-SENHAS             PIC  X(037)         VALUE SPACES.
           05  COMU-COMANDO            PIC  X(068)         VALUE SPACES.
           05  COMU-DADOS.
               10  COMU-TPOPCAO        PIC  X(001)         VALUE SPACES.
               10  COMU-TIMESTAMP      PIC  X(026)         VALUE SPACES.
               10  COMU-CHAMADO        PIC  X(008)         VALUE SPACES.
               10  COMU-NOME           PIC  X(040)         VALUE ZEROS.
               10  COMU-CPFCNPJ.
4S2511*         15  COMU-CPF        PIC  9(009)         VALUE ZEROS.
                    15  COMU-CPF        PIC  X(09)         VALUE SPACES.
                    15  FILLER          PIC  X(001)        VALUE SPACES.
4S2511*         15  COMU-CNPJ       PIC  9(004)         VALUE ZEROS.
                    15  COMU-CNPJ       PIC  X(04)         VALUE SPACES.
                    15  FILLER          PIC  X(001)        VALUE SPACES.
                    15  COMU-CTRL       PIC  9(002)        VALUE ZEROS.
               10  COMU-BANCO-X.
                   15  COMU-BANCO      PIC  9(003)         VALUE ZEROS.
               10  COMU-AGENCIA.
                   15  COMU-COD-AGE    PIC  9(005)         VALUE ZEROS.
                   15  FILLER          PIC  X(001)         VALUE SPACES.
                   15  COMU-DES-AGE    PIC  X(020)         VALUE SPACES.
               10  COMU-CONTA-X.
                   15  COMU-CONTA      PIC  9(013)         VALUE ZEROS.
               10  COMU-COD-PROD-X.
                   15  COMU-COD-PROD   PIC  9(003)         VALUE ZEROS.
               10  COMU-COD-SUBP-X.
                   15  COMU-COD-SUBP   PIC  9(003)         VALUE ZEROS.
               10  COMU-MENSAGEM       PIC  X(079)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DE COMUNICACAO DCOM0103 ***'.
      *----------------------------------------------------------------*

       01  COMU-AREA-103.
           05  COMU-LL-103             PIC S9(004)  COMP   VALUE +241.
           05  COMU-ZZ-103             PIC S9(004)  COMP   VALUE ZEROS.
           05  COMU-TRANCODE-103.
               10  COMU-TRANSACAO-103  PIC  X(008)         VALUE SPACES.
               10  FILLER              PIC  X(006)         VALUE SPACES.
               10  COMU-PFK-103        PIC  X(001)         VALUE SPACES.
           05  COMU-SENHAS-103         PIC  X(037)         VALUE SPACES.
           05  COMU-COMANDO-103        PIC  X(068)         VALUE SPACES.
           05  COMU-DADOS-103.
               10  COMU-OPCAO-103      PIC  X(001)         VALUE SPACES.
               10  COMU-TIMESTAMP-103  PIC  X(026)         VALUE SPACES.
               10  COMU-COD-OPER-103-X.
                   15  COMU-COD-OPER-103
                                       PIC  9(009)         VALUE ZEROS.
               10  COMU-TPOPER-103     PIC  X(002)         VALUE SPACES.
               10  COMU-MENSA-103      PIC  X(079)         VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(50)      VALUE
           '*** AREAS AUXILIARES PARA POOL5005 ***'.

      *----------------------------------------------------------------*
       01  WRK-POOL5005.
           05  WRK-SQLCA               PIC  X(136)         VALUE SPACES.
           05  WRK-GU                  PIC  X(04)          VALUE 'GU  '.
           05  WRK-CHNG                PIC  X(04)          VALUE 'CHNG'.
           05  WRK-ISRT                PIC  X(04)          VALUE 'ISRT'.
           05  WRK-FUNCAO              PIC  X(04)          VALUE SPACES.
           05  WRK-TELA                PIC  X(08)          VALUE SPACES.
           05  WRK-VERSAO              PIC  X(06)          VALUE
                                                           'VRS002'.
           05  WRK-COD-USER            PIC  X(07)          VALUE SPACES.
           05  WRK-COD-USER-R          REDEFINES
               WRK-COD-USER            PIC  9(07).
           05  WRK-COD-DEPTO           PIC  9(06)          VALUE ZEROS.

           05  WRK-MENSAGEM.
               10  FILLER              PIC  X(04)          VALUE SPACES.
               10  WRK-TRANSACAO       PIC  X(08)          VALUE SPACES.
               10  FILLER              PIC  X(1988)        VALUE SPACES.

      *----------------------------------------------------------------*

       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DA TELA DCOM9GTM - INPUT ***'.
      *----------------------------------------------------------------*
       01  WRK-INPUT-9GTM.
           05  FILLER                  PIC  X(18)          VALUE SPACES.
           05  WRK-INP-PFK-9GTM        PIC  X(01)          VALUE SPACES.
           05  WRK-INP-SENHAS-9GTM     PIC  X(37)          VALUE SPACES.
           05  WRK-INP-COMANDO-9GTM    PIC  X(68)          VALUE SPACES.
           05  WRK-INP-DADOS-9GTM.
               10  WRK-INP-TMSTAMP-9GTM
                                       PIC  X(26)          VALUE SPACES.
               10  WRK-INP-CHAMADO-9GTM
                                       PIC  X(08)          VALUE SPACES.
               10  WRK-INP-CDPROD-9GTM-X.
                   15  WRK-INP-CDPROD-9GTM
                                       PIC  9(03)          VALUE ZEROS.
               10  WRK-INP-CDSUBP-9GTM-X.
                   15  WRK-INP-CDSUBP-9GTM
                                       PIC  9(03)          VALUE ZEROS.
               10  WRK-INP-TIPOOPC-9GTM
                                       PIC  X(01)          VALUE SPACES.

               10  WRK-INP-ULTPAG-9GTM PIC  X(01)          VALUE SPACES.
               10  WRK-INP-PAGINA-9GTM-X.
                   15  WRK-INP-PAGINA-9GTM
                                       PIC  9(03)          VALUE ZEROS.
               10  WRK-INP-NOMECL-9GTM PIC  X(30)          VALUE SPACES.
               10  WRK-INP-CGCCPF-9GTM.
4S2511*            15 WRK-INP-CGCCPF1-N PIC  9(03)         VALUE ZEROS.
4S2511             15 WRK-INP-CGCCPF1-N PIC  X(03)         VALUE SPACES.
                   15 WRK-INP-PONTO1   PIC  X(01)          VALUE '.'.
4S2511*            15 WRK-INP-CGCCPF2-N PIC  9(03)         VALUE ZEROS.
4S2511             15 WRK-INP-CGCCPF2-N PIC  9(03)         VALUE SPACES.
                   15 WRK-INP-PONTO2   PIC  X(01)          VALUE '.'.
4S2511*            15 WRK-INP-CGCCPF3-N PIC  9(03)         VALUE ZEROS.
4S2511             15 WRK-INP-CGCCPF3-N PIC  9(03)         VALUE SPACES.
                   15 WRK-INP-BARRA    PIC  X(01)          VALUE '/'.
4S2511*            15 WRK-INP-FIL-N    PIC  9(04)          VALUE ZEROS.
4S2511             15 WRK-INP-FIL-N    PIC  X(04)          VALUE SPACES.
                   15 WRK-INP-TRACO    PIC  X(01)          VALUE '-'.
                   15 WRK-INP-CTRL-N   PIC  9(02)          VALUE ZEROS.
               10  WRK-INP-BANCO-9GTM-X.
                   15  WRK-INP-BANCO-9GTM
                                       PIC  9(03)          VALUE ZEROS.
               10  WRK-INP-AGENCIA-9GTM
                                       PIC  X(26)          VALUE SPACES.
               10  WRK-INP-CONTA-9GTM-X.

                   15  WRK-INP-CONTA-9GTM
                                       PIC  9(13)          VALUE ZEROS.
               10  WRK-INP-LINHAS-9GTM OCCURS 05 TIMES.
                   15  WRK-INP-SELEC-9GTM
                                       PIC  X(01)          VALUE SPACES.
                   15  WRK-INP-NUMERO-9GTM-X.
                       20  WRK-INP-NUMERO-9GTM
                                       PIC  9(09)          VALUE ZEROS.
                   15  WRK-INP-VLRLIM-9GTM-X
                                       PIC  X(14)          VALUE SPACES.
                   15  WRK-INP-VLRDIS-9GTM-X
                                       PIC  X(14)          VALUE SPACES.
                   15  WRK-INP-DTINIC-9GTM
                                       PIC  X(10)          VALUE SPACES.
                   15  WRK-INP-DTVENC-9GTM
                                       PIC  X(10)          VALUE SPACES.
                   15  WRK-INP-NOME-9GTM
                                       PIC  X(40)          VALUE SPACES.
           05  WRK-INP-MENSA-9GTM      PIC  X(79)          VALUE SPACES.

      *----------------------------------------------------------------*

       01  FILLER                          PIC  X(50)      VALUE
           '*** AREA DA TELA DCOM9GTM  - OUTPUT ***'.
      *----------------------------------------------------------------*
       01  WRK-OUTPUT-9GTM.
           05  WRK-OUT-LL-9GTM         PIC S9(04) COMP     VALUE +000.
           05  WRK-OUT-ZZ-9GTM         PIC S9(04) COMP     VALUE ZEROS.
           05  WRK-OUT-SENHAS-9GTM     PIC  X(37)          VALUE SPACES.
           05  WRK-OUT-COMANDO-9GTM    PIC  X(68)          VALUE SPACES.
           05  WRK-OUT-DADOS-9GTM.
               10  WRK-OUT-TMSTAMP-9GTM
                                       PIC  X(26)          VALUE SPACES.
               10  WRK-OUT-CHAMADO-9GTM
                                       PIC  X(08)          VALUE SPACES.
               10  WRK-OUT-CDPROD-9GTM-X.
                   15  WRK-OUT-CDPROD-9GTM
                                       PIC  9(03)          VALUE ZEROS.
               10  WRK-OUT-CDSUBP-9GTM-X.
                   15  WRK-OUT-CDSUBP-9GTM
                                       PIC  9(03)          VALUE ZEROS.
               10  WRK-OUT-TIPOOPC-9GTM
                                       PIC  X(01)          VALUE SPACES.

               10  WRK-OUT-ULTPAG-9GTM PIC  X(01)          VALUE SPACE.
               10  WRK-OUT-PAGINA-9GTM-X.
                   15  WRK-OUT-PAGINA-9GTM
                                       PIC  9(03)          VALUE ZEROS.
               10  WRK-OUT-NOMECL-9GTM PIC  X(30)          VALUE SPACES.
               10  WRK-OUT-CGCCPF-9GTM.
                   15 WRK-OUT-CGCCPF1-N PIC  9(03)         VALUE ZEROS.
                   15 WRK-OUT-PONTO1   PIC  X(01)          VALUE '.'.
                   15 WRK-OUT-CGCCPF2-N PIC  9(03)         VALUE ZEROS.
                   15 WRK-OUT-PONTO2   PIC  X(01)          VALUE '.'.
                   15 WRK-OUT-CGCCPF3-N PIC  9(03)         VALUE ZEROS.
                   15 WRK-OUT-BARRA    PIC  X(01)          VALUE '/'.
                   15 WRK-OUT-FIL-N    PIC  9(04)          VALUE ZEROS.
                   15 WRK-OUT-TRACO    PIC  X(01)          VALUE '-'.
                   15 WRK-OUT-CTRL-N   PIC  9(02)          VALUE ZEROS.
               10  WRK-OUT-BANCO-9GTM-X.
                   15  WRK-OUT-BANCO-9GTM
                                       PIC  9(03)          VALUE ZEROS.
               10  WRK-OUT-AGENCIA-9GTM
                                       PIC  X(26)          VALUE SPACES.
               10  WRK-OUT-CONTA-9GTM-X.

                   15  WRK-OUT-CONTA-9GTM
                                       PIC  9(13)          VALUE ZEROS.
               10  WRK-OUT-LINHAS-9GTM OCCURS 05 TIMES.
                   15  WRK-OUT-SELEC-ATTR-9GTM
                                       PIC  9(004) COMP    VALUE ZEROS.
                   15  WRK-OUT-SELEC-9GTM
                                       PIC  X(01)          VALUE SPACES.
                   15  WRK-OUT-NUMERO-9GTM-X.
                       20  WRK-OUT-NUMERO-9GTM
                                       PIC  9(09)          VALUE ZEROS.
                   15  WRK-OUT-VLRLIM-9GTM-X.
                       20  WRK-OUT-VLRLIM-9GTM
                                       PIC  ZZZ.ZZZ.ZZ9,99.
                   15  WRK-OUT-VLRDIS-9GTM-X.
                       20  WRK-OUT-VLRDIS-9GTM
                                       PIC  ZZZ.ZZZ.ZZ9,99.
                   15  WRK-OUT-DTINIC-9GTM
                                       PIC  X(10)          VALUE SPACES.
                   15  WRK-OUT-DTVENC-9GTM
                                       PIC  X(10)          VALUE SPACES.
                   15  WRK-OUT-NOME-9GTM

                                       PIC  X(40)          VALUE SPACES.
           05  WRK-OUT-MENSA-9GTM      PIC  X(79)          VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DE ERRO DA BRAD0660 ***'.
      *----------------------------------------------------------------*
       01  WRK-ERRO-BRAD0660.
           05  FILLER                  PIC  X(45)          VALUE
               '* RETURN-CODE DIFERENTE DE ZEROS NA BRAD0660'.
           05  FILLER                  PIC  X(12)          VALUE
               '- RET.COD ='.
           05  WRK-RETURN-CODE         PIC  9(02)          VALUE ZEROS.
           05  FILLER                  PIC  X(11)          VALUE
               ' - LOCAL ='.
           05  WRK-LOCAL-ERRO          PIC  9(02)          VALUE ZEROS.
           05  FILLER                  PIC  X(02)          VALUE ' *'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DA TELA DCOM9GTM - BRAD0660 ***'.

      *----------------------------------------------------------------*
       01  WRK-660-DCOM9GTM.
           05  WRK-660-LL-AREA-9GTM    PIC  9(004) COMP    VALUE 0000.
           05  WRK-660-LL-MENSA-9GTM   PIC  9(004) COMP    VALUE 0000.
           05  WRK-660-SENHAS-9GTM     PIC  9(004) COMP    VALUE 0037.
           05  WRK-660-COMANDO-9GTM    PIC  9(004) COMP    VALUE 0068.
           05  WRK-660-TMSTAMP-9GTM    PIC  9(004) COMP    VALUE 0026.
           05  WRK-660-CHAMADO-9GTM    PIC  9(004) COMP    VALUE 0008.
           05  WRK-660-CDPROD-9GTM     PIC  9(004) COMP    VALUE 0003.
           05  WRK-660-CDSUBP-9GTM     PIC  9(004) COMP    VALUE 0003.
           05  WRK-660-TIPOOPC-9GTM    PIC  9(004) COMP    VALUE 0001.
           05  WRK-660-ULTPAG-9GTM     PIC  9(004) COMP    VALUE 0001.
           05  WRK-660-PAGINA-9GTM     PIC  9(004) COMP    VALUE 0003.
           05  WRK-660-NOMECL-9GTM     PIC  9(004) COMP    VALUE 0030.
           05  WRK-660-CGCCPF-9GTM     PIC  9(004) COMP    VALUE 0019.
           05  WRK-660-BANCO-9GTM      PIC  9(004) COMP    VALUE 0003.
           05  WRK-660-AGENCIA-9GTM    PIC  9(004) COMP    VALUE 0026.
           05  WRK-660-NUCONTA-9GTM    PIC  9(004) COMP    VALUE 0013.
           05  WRK-600-LISTA-9GTM      OCCURS  05.
               10 WRK-660-ESCOLH-9GTM  PIC  9(004) COMP    VALUE 2003.
               10 WRK-660-NUMERO-9GTM  PIC  9(004) COMP    VALUE 0009.

               10 WRK-660-VLRLIM-9GTM  PIC  9(004) COMP    VALUE 0014.
               10 WRK-660-VLRDIS-9GTM  PIC  9(004) COMP    VALUE 0014.
               10 WRK-660-DTINIC-9GTM  PIC  9(004) COMP    VALUE 0010.
               10 WRK-660-DTVENC-9GTM  PIC  9(004) COMP    VALUE 0010.
               10 WRK-660-NOME-9GTM    PIC  9(004) COMP    VALUE 0040.
           05  WRK-660-MENSA-9GTM      PIC  9(004) COMP    VALUE 0079.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DE COMUNICACAO DCOM4321 ***'.
      *----------------------------------------------------------------*
       01  WRK-4321E-ENTRADA.
           03  WRK-4321E-COD-RETORNO      PIC  X(04).
           03  WRK-4321E-MSG-RETORNO      PIC  X(79).
           03  WRK-4321E-RESTART          PIC  9(05).
           03  WRK-4321E-FLAG             PIC  X(01).
           03  WRK-4321E-START-COUNT      PIC  X(01).
           03  WRK-4321E-CFUNC-BDSCO      PIC  9(09).
           03  WRK-4321E-CTERM            PIC  X(08).
           03  WRK-4321E-CPROD            PIC  9(03).
           03  WRK-4321E-CSPROD-DESC-COML PIC  9(03).

           03  WRK-4321E-CBCO             PIC  9(03).
           03  WRK-4321E-CAG-BCRIA        PIC  9(05).
           03  WRK-4321E-CCTA-BCRIA-CLI   PIC  9(13).
4S2511*    03  WRK-4321E-CCNPJ-CPF        PIC  9(09).
           03  WRK-4321E-CCNPJ-CPF        PIC  X(09).
4S2511*    03  WRK-4321E-CFLIAL-CNPJ      PIC  9(04).
           03  WRK-4321E-CFLIAL-CNPJ      PIC  X(04).
           03  WRK-4321E-CCTRL-CNPJ-CPF   PIC  9(02).

       01  WRK-4321S-SAIDA.
           03  WRK-4321S-HEADER.
               05  WRK-4321S-COD-RETORNO
                                       PIC  X(04).
               05  WRK-4321S-MSG-RETORNO
                                       PIC  X(79).
               05  WRK-4321S-RESTART   PIC  9(05).
               05  WRK-4321S-FLAG      PIC  X(01).
               05  WRK-4321S-QTDE-RETORNADA
                                       PIC  9(03).
               05  WRK-4321S-COUNT     PIC  9(09).
           03  WRK-4321S-DADOS.
               05  WRK-4321S-TABELA-SAIDA  OCCURS  20  TIMES.
                   07 WRK-4321S-CCONVE-CLI-DESC   PIC  9(009).

                   07 WRK-4321S-CVRSAO-CONVE-CLI  PIC  9(003).
                   07 WRK-4321S-CPRODT            PIC  9(003).
                   07 WRK-4321S-IABREV-PRODT      PIC  X(012).
                   07 WRK-4321S-CSPROD-DESC-COML  PIC  9(003).
                   07 WRK-4321S-IRSUMO-SPROD-DESC PIC  X(010).
                   07 WRK-4321S-VLIM-CONVE-DESC   PIC  9(015)V99.
                   07 WRK-4321S-DINIC-VGCIA-CONVE PIC  X(010).
                   07 WRK-4321S-DVCTO-CONVE-DESC  PIC  X(010).
                   07 WRK-4321S-ICLIE             PIC  X(040).
                   07 WRK-4321S-VDISP-CONVE-DESC  PIC  9(015)V99.
           03  WRK-4321S-CONSISTENCIA  REDEFINES  WRK-4321S-DADOS.
               05  WRK-4321S-TAMANHO-ERRO         PIC  9(01).
               05  WRK-4321S-TABELA-ERROS  OCCURS 008  TIMES.
                   07  WRK-4321S-CAMPOS-ERRO      PIC  9(01).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA PARA MENSAGENS ***'.
      *----------------------------------------------------------------*
       COPY 'I#DCOMSY'.


      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DA BRAD7100 ***'.
      *----------------------------------------------------------------*
       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** DCOM3907 - FIM DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  LNK-IO-PCB.
           05 LNK-IO-LTERM             PIC  X(08).
           05 FILLER                   PIC  X(02).
           05 LNK-IO-STATUS            PIC  X(02).
           05 FILLER                   PIC  X(12).
           05 LNK-IO-MODNAME           PIC  X(08).


       01  LNK-ALT-PCB.
           05 LNK-ALT-LTERM            PIC  X(08).
           05 FILLER                   PIC  X(02).
           05 LNK-ALT-STATUS           PIC  X(02).
           05 FILLER                   PIC  X(12).
           05 LNK-ALT-MODNAME          PIC  X(08).

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

           ENTRY 'DLITCBL'             USING LNK-IO-PCB
                                             LNK-ALT-PCB.

           MOVE WRK-GU                 TO WRK-FUNCAO.

      *----------------------------------------------------------------*
      *     PROCEDIMENTOS INICIAIS                                     *
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.

      *----------------------------------------------------------------*

           CALL 'POOL5005'             USING WRK-VERSAO
                                             WRK-FUNCAO
                                             LNK-IO-PCB
                                             LNK-ALT-PCB
                                             WRK-MENSAGEM
                                             WRK-TELA
                                             WRK-COD-USER
                                             WRK-COD-DEPTO.

           IF  RETURN-CODE             EQUAL 04 OR 12
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE SPACES                 TO WRK-OUT-DADOS-9GTM
                                          WRK-OUT-SENHAS-9GTM
                                          WRK-OUT-COMANDO-9GTM.

           PERFORM 2000-PROCESSAR-MENSAGEM.


           GO TO 0000-INICIAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     PROCESSAR MENSAGEM                                         *
      *----------------------------------------------------------------*
       2000-PROCESSAR-MENSAGEM         SECTION.
      *----------------------------------------------------------------*

           MOVE 'DCOM9GTM'             TO WRK-TELA

           IF  WRK-TRANSACAO           NOT EQUAL 'DCOM3907'
               PERFORM 2100-RECEBER-CONTROLE
           ELSE
               PERFORM 2200-PROCESSAR-DCOM9GTM
           END-IF.

           IF  WRK-TELA                EQUAL 'DCOM9GTM'

               PERFORM 2300-PROCESSAR-BRAD0660
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     RECEBER CONTROLE                                           *
      *----------------------------------------------------------------*
       2100-RECEBER-CONTROLE           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM           TO COMU-AREA.
           MOVE COMU-TIMESTAMP         TO WRK-INP-TMSTAMP-9GTM
                                          WRK-OUT-TMSTAMP-9GTM.
           MOVE COMU-TPOPCAO           TO WRK-INP-TIPOOPC-9GTM
                                          WRK-OUT-TIPOOPC-9GTM.
           MOVE COMU-CHAMADO           TO WRK-INP-CHAMADO-9GTM
                                          WRK-OUT-CHAMADO-9GTM.
           MOVE COMU-NOME              TO WRK-INP-NOMECL-9GTM

                                          WRK-OUT-NOMECL-9GTM.

           MOVE COMU-CPFCNPJ(1:3)      TO WRK-INP-CGCCPF1-N.
           MOVE '.'                    TO WRK-INP-PONTO1.
           MOVE COMU-CPFCNPJ(4:3)      TO WRK-INP-CGCCPF2-N.
           MOVE '.'                    TO WRK-INP-PONTO2.
           MOVE COMU-CPFCNPJ(7:3)      TO WRK-INP-CGCCPF3-N.
           MOVE '/'                    TO WRK-INP-BARRA.
           MOVE COMU-CPFCNPJ(11:4)     TO WRK-INP-FIL-N.
           MOVE '-'                    TO WRK-INP-TRACO.
           MOVE COMU-CPFCNPJ(16:2)     TO WRK-INP-CTRL-N.
           MOVE WRK-INP-CGCCPF-9GTM    TO WRK-OUT-CGCCPF-9GTM.
           MOVE 237                    TO COMU-BANCO.
           MOVE COMU-BANCO             TO WRK-INP-BANCO-9GTM
                                          WRK-OUT-BANCO-9GTM.
           MOVE COMU-AGENCIA           TO WRK-INP-AGENCIA-9GTM
                                          WRK-OUT-AGENCIA-9GTM.
           MOVE COMU-CONTA             TO WRK-INP-CONTA-9GTM
                                          WRK-OUT-CONTA-9GTM.
           MOVE COMU-COD-PROD          TO WRK-INP-CDPROD-9GTM
                                          WRK-OUT-CDPROD-9GTM.

           MOVE COMU-COD-SUBP          TO WRK-INP-CDSUBP-9GTM
                                          WRK-OUT-CDSUBP-9GTM.
           MOVE 1                      TO WRK-INP-PAGINA-9GTM
                                          WRK-OUT-PAGINA-9GTM.

           INITIALIZE  WRK-4321E-ENTRADA
                       WRK-4321S-SAIDA
                       ERRO-AREA.

           MOVE 'N'                    TO WRK-4321E-FLAG
           MOVE 'S'                    TO WRK-4321E-START-COUNT
           MOVE ZEROS                  TO WRK-4321E-RESTART
           MOVE WRK-COD-USER           TO WRK-4321E-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO WRK-4321E-CTERM
           MOVE COMU-COD-PROD          TO WRK-4321E-CPROD
           MOVE COMU-COD-SUBP          TO WRK-4321E-CSPROD-DESC-COML
           MOVE COMU-BANCO             TO WRK-4321E-CBCO
           MOVE COMU-COD-AGE           TO WRK-4321E-CAG-BCRIA
           MOVE COMU-CONTA             TO WRK-4321E-CCTA-BCRIA-CLI
           MOVE COMU-CPF               TO WRK-4321E-CCNPJ-CPF
           MOVE COMU-CNPJ              TO WRK-4321E-CFLIAL-CNPJ

           MOVE COMU-CTRL              TO WRK-4321E-CCTRL-CNPJ-CPF.

           MOVE 'DCOM4321'             TO WRK-MODULO.

           CALL WRK-MODULO             USING WRK-4321E-ENTRADA
                                             WRK-4321S-SAIDA
                                             ERRO-AREA
                                             WRK-SQLCA.

           EVALUATE WRK-4321S-COD-RETORNO
               WHEN '0000'
                   IF (WRK-4321S-CCONVE-CLI-DESC(06)  NOT NUMERIC) OR
                      (WRK-4321S-CCONVE-CLI-DESC(06)  EQUAL ZEROS)
                       MOVE '*'        TO WRK-OUT-ULTPAG-9GTM
                       MOVE SG-MENSAGEM(61)
                                       TO WRK-OUT-MENSA-9GTM
                   ELSE
                       MOVE SPACES     TO WRK-OUT-ULTPAG-9GTM
                       MOVE SG-MENSAGEM(61)
                                       TO WRK-OUT-MENSA-9GTM
                   END-IF

                   PERFORM 2110-MONTAR-TELA VARYING WRK-IND FROM 1 BY 1
                                              UNTIL WRK-IND GREATER 05

               WHEN '0099'
                   MOVE 'DB2'          TO ERR-TIPO-ACESSO
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                   MOVE WRK-INP-CHAMADO-9GTM   TO WRK-TELA
                   MOVE LENGTH OF COMU-AREA    TO COMU-LL
                   MOVE WRK-INP-SENHAS-9GTM    TO COMU-SENHAS
                   MOVE WRK-INP-COMANDO-9GTM   TO COMU-COMANDO
                   MOVE WRK-INP-TIPOOPC-9GTM   TO COMU-TPOPCAO
                   MOVE WRK-INP-TMSTAMP-9GTM   TO COMU-TIMESTAMP
                   MOVE WRK-INP-CHAMADO-9GTM   TO COMU-CHAMADO
                   MOVE WRK-INP-NOMECL-9GTM    TO COMU-NOME

                   MOVE WRK-INP-CGCCPF1-N      TO COMU-CPFCNPJ(1:3)
                   MOVE WRK-INP-CGCCPF2-N      TO COMU-CPFCNPJ(4:3)
                   MOVE WRK-INP-CGCCPF3-N      TO COMU-CPFCNPJ(7:3)
                   MOVE '/'                    TO COMU-CPFCNPJ(10:1)

                   MOVE WRK-INP-FIL-N          TO COMU-CPFCNPJ(11:4)
                   MOVE '-'                    TO COMU-CPFCNPJ(15:1)
                   MOVE WRK-INP-CTRL-N         TO COMU-CPFCNPJ(16:2)
                   MOVE 237                    TO WRK-INP-BANCO-9GTM
                   MOVE WRK-INP-BANCO-9GTM     TO COMU-BANCO
                   MOVE WRK-INP-AGENCIA-9GTM   TO COMU-AGENCIA
                   MOVE WRK-INP-CONTA-9GTM     TO COMU-CONTA
                   MOVE WRK-INP-CDPROD-9GTM    TO COMU-COD-PROD
                   MOVE WRK-INP-CDSUBP-9GTM    TO COMU-COD-SUBP
                   MOVE WRK-4321S-MSG-RETORNO  TO COMU-MENSAGEM
                   MOVE 'DCOM3907'             TO COMU-TRANSACAO
                   MOVE WRK-CHNG               TO WRK-FUNCAO
                   MOVE COMU-AREA              TO WRK-MENSAGEM
           END-EVALUATE.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     FORMATACAO DAS OCORRENCIAS DA TELA                         *

      *----------------------------------------------------------------*
       2110-MONTAR-TELA                SECTION.
      *----------------------------------------------------------------*

           IF (WRK-4321S-CCONVE-CLI-DESC(WRK-IND) NOT NUMERIC) OR
              (WRK-4321S-CCONVE-CLI-DESC(WRK-IND) EQUAL ZEROS)
               MOVE WRK-225          TO WRK-OUT-SELEC-ATTR-9GTM(WRK-IND)
           ELSE
               MOVE WRK-4321S-CCONVE-CLI-DESC(WRK-IND)
                                     TO WRK-OUT-NUMERO-9GTM(WRK-IND)
               MOVE WRK-4321S-VLIM-CONVE-DESC(WRK-IND)
                                     TO WRK-OUT-VLRLIM-9GTM(WRK-IND)
               MOVE WRK-4321S-VDISP-CONVE-DESC(WRK-IND)
                                     TO WRK-OUT-VLRDIS-9GTM(WRK-IND)
               MOVE WRK-4321S-DINIC-VGCIA-CONVE(WRK-IND)
                                     TO WRK-OUT-DTINIC-9GTM(WRK-IND)
               INSPECT WRK-OUT-DTINIC-9GTM(WRK-IND)
                                        REPLACING ALL '.' BY '/'
               MOVE WRK-4321S-DVCTO-CONVE-DESC(WRK-IND)
                                     TO WRK-OUT-DTVENC-9GTM(WRK-IND)
               INSPECT WRK-OUT-DTVENC-9GTM(WRK-IND)

                                        REPLACING ALL '.' BY '/'
               MOVE WRK-4321S-ICLIE (WRK-IND)
                                     TO WRK-OUT-NOME-9GTM(WRK-IND)
           END-IF.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     PROCESSAR DCOM9GTM                                         *
      *----------------------------------------------------------------*
       2200-PROCESSAR-DCOM9GTM         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM           TO WRK-INPUT-9GTM.

           PERFORM 2205-DEVOLVER-TELA-FIXO.

           EVALUATE TRUE


               WHEN WRK-INP-PFK-9GTM      EQUAL 'H' OR
                   (WRK-INP-PFK-9GTM      EQUAL '.' AND
                    WRK-INP-COMANDO-9GTM  EQUAL 'PFK01')
                    PERFORM 2210-DEVOLVER-TELA-VARIA
                    MOVE WRK-INP-MENSA-9GTM TO  WRK-OUT-MENSA-9GTM

               WHEN WRK-INP-PFK-9GTM      EQUAL '3' OR
                   (WRK-INP-PFK-9GTM      EQUAL '.' AND
                    WRK-INP-COMANDO-9GTM  EQUAL 'PFK03')
                    PERFORM 2220-RETORNAR-CHAMADOR

               WHEN WRK-INP-PFK-9GTM      EQUAL '7' OR
                   (WRK-INP-PFK-9GTM      EQUAL '.' AND
                    WRK-INP-COMANDO-9GTM  EQUAL 'PFK07')
                    PERFORM 2240-TELA-ANTERIOR

               WHEN WRK-INP-PFK-9GTM      EQUAL '8' OR
                   (WRK-INP-PFK-9GTM      EQUAL '.' AND
                    WRK-INP-COMANDO-9GTM  EQUAL 'PFK08')
                    PERFORM 2250-TELA-POSTERIOR


               WHEN (WRK-INP-PFK-9GTM     EQUAL '.' AND
                     WRK-INP-COMANDO-9GTM EQUAL SPACES)
                     PERFORM 2270-TRATAR-ENTER
                     PERFORM 2210-DEVOLVER-TELA-VARIA

               WHEN  OTHER
                     MOVE SG-MENSAGEM(38)
                                       TO  WRK-OUT-MENSA-9GTM
                     PERFORM 2210-DEVOLVER-TELA-VARIA

           END-EVALUATE.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     DEVOLVER TELA - DADOS FIXOS                                *
      *----------------------------------------------------------------*
       2205-DEVOLVER-TELA-FIXO         SECTION.
      *----------------------------------------------------------------*


           MOVE WRK-INP-SENHAS-9GTM    TO WRK-OUT-SENHAS-9GTM
           MOVE WRK-INP-TMSTAMP-9GTM   TO WRK-OUT-TMSTAMP-9GTM
           MOVE WRK-INP-CHAMADO-9GTM   TO WRK-OUT-CHAMADO-9GTM
           MOVE WRK-INP-CDPROD-9GTM-X  TO WRK-OUT-CDPROD-9GTM-X
           MOVE WRK-INP-CDSUBP-9GTM-X  TO WRK-OUT-CDSUBP-9GTM-X
           MOVE WRK-INP-TIPOOPC-9GTM   TO WRK-OUT-TIPOOPC-9GTM
           MOVE WRK-INP-ULTPAG-9GTM    TO WRK-OUT-ULTPAG-9GTM
           MOVE WRK-INP-PAGINA-9GTM-X  TO WRK-OUT-PAGINA-9GTM-X
           MOVE WRK-INP-MENSA-9GTM     TO WRK-OUT-MENSA-9GTM
           MOVE 237                    TO WRK-INP-BANCO-9GTM-X
           MOVE WRK-INP-BANCO-9GTM-X   TO WRK-OUT-BANCO-9GTM-X
           MOVE WRK-INP-AGENCIA-9GTM   TO WRK-OUT-AGENCIA-9GTM
           MOVE WRK-INP-CONTA-9GTM-X   TO WRK-OUT-CONTA-9GTM-X
           MOVE WRK-INP-CGCCPF-9GTM    TO WRK-OUT-CGCCPF-9GTM
           MOVE WRK-INP-NOMECL-9GTM    TO WRK-OUT-NOMECL-9GTM.

      *----------------------------------------------------------------*
       2205-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      *----------------------------------------------------------------*
      *     DEVOLVER TELA - DADOS VARIAVEIS                            *
      *----------------------------------------------------------------*
       2210-DEVOLVER-TELA-VARIA        SECTION.
      *----------------------------------------------------------------*

           PERFORM  VARYING  WRK-IND  FROM 1 BY 1
                      UNTIL  WRK-IND  GREATER 05

               IF (WRK-INP-NUMERO-9GTM(WRK-IND) NOT NUMERIC) OR
                  (WRK-INP-NUMERO-9GTM(WRK-IND) EQUAL ZEROS)
                   MOVE WRK-225      TO WRK-OUT-SELEC-ATTR-9GTM(WRK-IND)
               ELSE
                   MOVE WRK-INP-SELEC-9GTM(WRK-IND)
                                     TO WRK-OUT-SELEC-9GTM(WRK-IND)
                   MOVE WRK-INP-NUMERO-9GTM-X(WRK-IND)
                                     TO WRK-OUT-NUMERO-9GTM-X(WRK-IND)
                   MOVE WRK-INP-VLRLIM-9GTM-X(WRK-IND)
                                     TO WRK-OUT-VLRLIM-9GTM-X(WRK-IND)
                   MOVE WRK-INP-VLRDIS-9GTM-X(WRK-IND)
                                     TO WRK-OUT-VLRDIS-9GTM-X(WRK-IND)

                   MOVE WRK-INP-DTINIC-9GTM(WRK-IND)
                                     TO WRK-OUT-DTINIC-9GTM(WRK-IND)
                   MOVE WRK-INP-DTVENC-9GTM(WRK-IND)
                                     TO WRK-OUT-DTVENC-9GTM(WRK-IND)
                   MOVE WRK-INP-NOME-9GTM(WRK-IND)
                                     TO WRK-OUT-NOME-9GTM(WRK-IND)
               END-IF
           END-PERFORM.

      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     RETORNAR AO PROGRAMA CHAMADOR - DCOM0102                   *
      *----------------------------------------------------------------*
       2220-RETORNAR-CHAMADOR          SECTION.
      *----------------------------------------------------------------*

           MOVE LENGTH OF COMU-AREA    TO COMU-LL
           MOVE WRK-CHNG               TO WRK-FUNCAO

           MOVE WRK-INP-CHAMADO-9GTM   TO WRK-TELA
           MOVE 'DCOM3907'             TO COMU-TRANSACAO
           MOVE '3'                    TO COMU-PFK
           MOVE WRK-INP-SENHAS-9GTM    TO COMU-SENHAS
           MOVE WRK-INP-COMANDO-9GTM   TO COMU-COMANDO
           MOVE WRK-INP-TIPOOPC-9GTM   TO COMU-TPOPCAO
           MOVE WRK-INP-TMSTAMP-9GTM   TO COMU-TIMESTAMP
           MOVE WRK-INP-CHAMADO-9GTM   TO COMU-CHAMADO
           MOVE WRK-INP-NOMECL-9GTM    TO COMU-NOME

           MOVE WRK-INP-CGCCPF1-N      TO COMU-CPFCNPJ(1:3)
           MOVE WRK-INP-CGCCPF2-N      TO COMU-CPFCNPJ(4:3)
           MOVE WRK-INP-CGCCPF3-N      TO COMU-CPFCNPJ(7:3)
           MOVE '/'                    TO COMU-CPFCNPJ(10:1)
           MOVE WRK-INP-FIL-N          TO COMU-CPFCNPJ(11:4)
           MOVE '-'                    TO COMU-CPFCNPJ(15:1)
           MOVE WRK-INP-CTRL-N         TO COMU-CPFCNPJ(16:2)
           MOVE 237                    TO WRK-INP-BANCO-9GTM
           MOVE WRK-INP-BANCO-9GTM     TO COMU-BANCO
           MOVE WRK-INP-AGENCIA-9GTM   TO COMU-AGENCIA
           MOVE WRK-INP-CONTA-9GTM     TO COMU-CONTA

           MOVE WRK-INP-CDPROD-9GTM    TO COMU-COD-PROD
           MOVE WRK-INP-CDSUBP-9GTM    TO COMU-COD-SUBP
           MOVE COMU-AREA              TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     RETORNAR AO MENU ROTINAS                                   *
      *----------------------------------------------------------------*
      *     RETORNAR TELA ANTERIOR                                     *
      *----------------------------------------------------------------*
       2240-TELA-ANTERIOR              SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INP-PAGINA-9GTM  EQUAL 1
               PERFORM 2210-DEVOLVER-TELA-VARIA
               MOVE SG-MENSAGEM(28)    TO WRK-OUT-MENSA-9GTM
               GO TO 2240-99-FIM
           END-IF.


           MOVE 'N'                    TO WRK-4321E-FLAG
           MOVE 'N'                    TO WRK-4321E-START-COUNT
           MOVE WRK-COD-USER           TO WRK-4321E-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO WRK-4321E-CTERM
           MOVE WRK-INP-CDPROD-9GTM    TO WRK-4321E-CPROD
           MOVE WRK-INP-CDSUBP-9GTM    TO WRK-4321E-CSPROD-DESC-COML.
           MOVE 237                    TO WRK-INP-BANCO-9GTM
           MOVE WRK-INP-BANCO-9GTM     TO WRK-4321E-CBCO
           MOVE WRK-INP-AGENCIA-9GTM(1:5) TO WRK-4321E-CAG-BCRIA
           MOVE WRK-INP-CONTA-9GTM        TO WRK-4321E-CCTA-BCRIA-CLI
           MOVE WRK-INP-CGCCPF1-N         TO WRK-4321E-CCNPJ-CPF(1:3)
           MOVE WRK-INP-CGCCPF2-N         TO WRK-4321E-CCNPJ-CPF(4:3)
           MOVE WRK-INP-CGCCPF3-N         TO WRK-4321E-CCNPJ-CPF(7:3)
           MOVE WRK-INP-FIL-N             TO WRK-4321E-CFLIAL-CNPJ
           MOVE WRK-INP-CTRL-N            TO WRK-4321E-CCTRL-CNPJ-CPF

           COMPUTE WRK-4321E-RESTART  = (WRK-INP-PAGINA-9GTM - 2) * 05

           MOVE 'DCOM4321'             TO WRK-MODULO.
           CALL WRK-MODULO             USING WRK-4321E-ENTRADA

                                             WRK-4321S-SAIDA
                                             ERRO-AREA
                                             WRK-SQLCA

           EVALUATE WRK-4321S-COD-RETORNO
               WHEN '0000'
                   IF (WRK-4321S-CCONVE-CLI-DESC(11)  NOT NUMERIC) OR
                      (WRK-4321S-CCONVE-CLI-DESC(06)  EQUAL ZEROS)
                       MOVE '*'        TO WRK-OUT-ULTPAG-9GTM
                       MOVE SG-MENSAGEM(17)
                                       TO WRK-OUT-MENSA-9GTM
                   ELSE
                       MOVE SPACES     TO WRK-OUT-ULTPAG-9GTM
                       MOVE SG-MENSAGEM(01)
                                       TO WRK-OUT-MENSA-9GTM
                   END-IF
                   PERFORM 2110-MONTAR-TELA VARYING WRK-IND FROM 1 BY 1
                                              UNTIL WRK-IND GREATER 05
                   COMPUTE WRK-OUT-PAGINA-9GTM = WRK-INP-PAGINA-9GTM - 1

               WHEN '0099'

                   MOVE 'DB2'          TO ERR-TIPO-ACESSO
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                   PERFORM 2210-DEVOLVER-TELA-VARIA
                   MOVE WRK-4321S-MSG-RETORNO
                                       TO WRK-OUT-MENSA-9GTM
           END-EVALUATE.

      *----------------------------------------------------------------*
       2240-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     AVANCAR PARA TELA POSTERIOR                                *
      *----------------------------------------------------------------*
       2250-TELA-POSTERIOR             SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INP-ULTPAG-9GTM     EQUAL '*'
               PERFORM 2210-DEVOLVER-TELA-VARIA

               MOVE SG-MENSAGEM(29)    TO WRK-OUT-MENSA-9GTM
               GO TO 2250-99-FIM
           END-IF.

           MOVE 'N'                    TO WRK-4321E-FLAG
           MOVE 'N'                    TO WRK-4321E-START-COUNT
           MOVE WRK-COD-USER           TO WRK-4321E-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO WRK-4321E-CTERM
           MOVE WRK-INP-CDPROD-9GTM    TO WRK-4321E-CPROD
           MOVE WRK-INP-CDSUBP-9GTM    TO WRK-4321E-CSPROD-DESC-COML.
           MOVE 237                    TO WRK-INP-BANCO-9GTM
           MOVE WRK-INP-BANCO-9GTM     TO WRK-4321E-CBCO
           MOVE WRK-INP-AGENCIA-9GTM(1:5) TO WRK-4321E-CAG-BCRIA
           MOVE WRK-INP-CONTA-9GTM        TO WRK-4321E-CCTA-BCRIA-CLI
           MOVE WRK-INP-CGCCPF1-N         TO WRK-4321E-CCNPJ-CPF(1:3)
           MOVE WRK-INP-CGCCPF2-N         TO WRK-4321E-CCNPJ-CPF(4:3)
           MOVE WRK-INP-CGCCPF3-N         TO WRK-4321E-CCNPJ-CPF(7:3)
           MOVE WRK-INP-FIL-N             TO WRK-4321E-CFLIAL-CNPJ
           MOVE WRK-INP-CTRL-N            TO WRK-4321E-CCTRL-CNPJ-CPF

           COMPUTE WRK-4321E-RESTART  = (WRK-INP-PAGINA-9GTM) * 05


           MOVE 'DCOM4321'             TO WRK-MODULO.
           CALL WRK-MODULO             USING WRK-4321E-ENTRADA
                                             WRK-4321S-SAIDA
                                             ERRO-AREA
                                             WRK-SQLCA

           EVALUATE WRK-4321S-COD-RETORNO
               WHEN '0000'
                   IF (WRK-4321S-CCONVE-CLI-DESC(11)  NOT NUMERIC) OR
                      (WRK-4321S-CCONVE-CLI-DESC(06)  EQUAL ZEROS)
                       MOVE '*'        TO WRK-OUT-ULTPAG-9GTM
                       MOVE SG-MENSAGEM(17)
                                       TO WRK-OUT-MENSA-9GTM
                   ELSE
                       MOVE SPACES     TO WRK-OUT-ULTPAG-9GTM
                       MOVE SG-MENSAGEM(01)
                                       TO WRK-OUT-MENSA-9GTM
                   END-IF
                   PERFORM 2110-MONTAR-TELA VARYING WRK-IND FROM 1 BY 1
                                              UNTIL WRK-IND GREATER 05

                   COMPUTE WRK-OUT-PAGINA-9GTM = WRK-INP-PAGINA-9GTM + 1

               WHEN '0099'
                   MOVE 'DB2'          TO ERR-TIPO-ACESSO
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                   PERFORM 2210-DEVOLVER-TELA-VARIA
                   MOVE WRK-4321S-MSG-RETORNO
                                       TO WRK-OUT-MENSA-9GTM
           END-EVALUATE.

      *----------------------------------------------------------------*
       2250-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2270-TRATAR-ENTER               SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO WRK-INCONSIS.


           PERFORM 2271-CONSISTIR-FISICA.

           IF  WRK-INCONSIS            EQUAL '*'
               GO TO 2270-99-FIM
           END-IF.

           PERFORM 2272-TRANSFERIR-CONTROLE-0103.

      *----------------------------------------------------------------*
       2270-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2271-CONSISTIR-FISICA           SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO WCT-SELEC

           PERFORM VARYING WRK-IND FROM 1 BY 1 UNTIL WRK-IND GREATER 05


               IF  WRK-INP-SELEC-9GTM(WRK-IND) NOT EQUAL SPACE AND 'X'
                   MOVE '*'          TO WRK-INCONSIS
                   MOVE WRK-49369    TO WRK-OUT-SELEC-ATTR-9GTM(WRK-IND)
                   MOVE SG-MENSAGEM(41)
                                     TO WRK-OUT-MENSA-9GTM
               ELSE
                   IF  WRK-INP-SELEC-9GTM(WRK-IND) EQUAL 'X'
                       MOVE WRK-49369
                                     TO WRK-OUT-SELEC-ATTR-9GTM(WRK-IND)
                       ADD 1         TO WCT-SELEC
                       MOVE WRK-IND  TO WIND-SEL
                   END-IF
               END-IF
           END-PERFORM.

           IF WRK-INCONSIS  EQUAL  '*'
              GO TO 2271-99-FIM
           END-IF.

           IF  WCT-SELEC               EQUAL 0  OR  GREATER 1
               MOVE '*'                TO WRK-INCONSIS

               IF  WCT-SELEC           EQUAL 0
                   MOVE SG-MENSAGEM(53)
                                       TO WRK-OUT-MENSA-9GTM
               ELSE
                 IF  WCT-SELEC         GREATER 1
                     MOVE SG-MENSAGEM(39)
                                       TO WRK-OUT-MENSA-9GTM
                 END-IF
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2271-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2272-TRANSFERIR-CONTROLE-0103   SECTION.
      *----------------------------------------------------------------*

           INITIALIZE COMU-AREA-103
           MOVE WRK-CHNG               TO WRK-FUNCAO


           MOVE LENGTH OF COMU-AREA-103
                                       TO COMU-LL-103

           MOVE WRK-INP-CHAMADO-9GTM   TO WRK-TELA
           MOVE WRK-INP-SENHAS-9GTM    TO COMU-SENHAS-103
           MOVE WRK-INP-COMANDO-9GTM   TO COMU-COMANDO-103
           MOVE WRK-INP-TIPOOPC-9GTM   TO COMU-OPCAO-103
           MOVE WRK-INP-TMSTAMP-9GTM   TO COMU-TIMESTAMP-103
           MOVE WRK-INP-PFK-9GTM       TO COMU-PFK-103
           MOVE WRK-INP-NUMERO-9GTM(WIND-SEL)
                                       TO COMU-COD-OPER-103
           MOVE 'C1'                   TO COMU-TPOPER-103
           MOVE 'DCOM3907'             TO COMU-TRANSACAO-103
           MOVE COMU-AREA-103          TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2272-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*

       2300-PROCESSAR-BRAD0660         SECTION.
      *----------------------------------------------------------------*

           MOVE LENGTH OF WRK-660-DCOM9GTM TO WRK-660-LL-AREA-9GTM.
           MOVE LENGTH OF WRK-OUTPUT-9GTM  TO WRK-OUT-LL-9GTM
                                              WRK-660-LL-MENSA-9GTM.

           CALL 'BRAD0660'             USING WRK-OUTPUT-9GTM
                                             WRK-660-DCOM9GTM.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE RETURN-CODE        TO WRK-RETURN-CODE
               MOVE 10                 TO WRK-LOCAL-ERRO
               MOVE WRK-ERRO-BRAD0660  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-ISRT               TO WRK-FUNCAO.
           MOVE WRK-OUTPUT-9GTM        TO WRK-MENSAGEM.


      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           IF  ERR-MODULO              NOT EQUAL SPACES AND LOW-VALUES
               MOVE 'DCOM3907'         TO ERR-PGM
           ELSE

               MOVE 'SENHAS02'         TO ERR-PGM
               MOVE 'DCOM3907'         TO ERR-MODULO
           END-IF.

           MOVE WRK-COD-USER           TO ERR-COD-USER
           MOVE WRK-COD-DEPTO          TO ERR-COD-DEPTO

           IF  ERR-TIPO-ACESSO         EQUAL 'DB2'
               CALL 'BRAD7100'         USING LNK-IO-PCB
                                             ERRO-AREA
                                             LNK-ALT-PCB
                                             WRK-SQLCA
           ELSE
               CALL 'BRAD7100'         USING LNK-IO-PCB
                                             ERRO-AREA
                                             LNK-ALT-PCB
           END-IF.

           PERFORM 3000-FINALIZAR.

      *----------------------------------------------------------------*

       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
