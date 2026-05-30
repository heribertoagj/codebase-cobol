      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. DCOM0410.
       AUTHOR.     MARCELO MORINA.
      *================================================================*
      *                          A L T R A N                           *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   DCOM0410                                    *
      *    PROGRAMADOR.:   MARCELO MORINA MARQUES                      *
      *    ANALISTA....:   MARCELO MORINA MARQUES                      *
      *    DATA........:   05/02/2007                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   CONSULTAR OPERACAO - OPERACOES POR CLIENTE. *
      *----------------------------------------------------------------*
      *    TELAS.......:                                               *
      *    DCOMDJTM - CONSULTAR OPERACAO - OPERACOES POR CLIENTE.      *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    I#BRAD7C - AREA DE ERRO DA BRAD7100.                        *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    BRAD0660 - INSERIR CARACTERES DE NULL NA MENSAGEM DE SAIDA  *
      *    BRAD7100 - TRATAMENTO DE ERROS QUANDO PROGRAMA INVALIDO     *
      *    POOL5005 - CONTROLE DO TRANSITO DE MENSAGENS NO IMS/DC      *
      *----------------------------------------------------------------*
      *    NAVEGACAO...:                                               *
      *    CHAMADO POR.: DCOM0402           CHAMA: DCOM0411            *
      *================================================================*
BRQ001*                   BRQ DIGITAL SOLUTIONS - ALTERACAO            *
BRQ001*----------------------------------------------------------------*
BRQ001*     ANALISTA....: LUCIANDRA SILVEIRA                           *
BRQ001*     ANALISTA DS.: RICARDO JAMMAL                               *
BRQ001*     DATA........: 11/02/2019                                   *
BRQ001*----------------------------------------------------------------*
BRQ001*     OBJETIVO....: INCLUIR OPCAO 4 - CONSULTAR PENDENTE         *
BRQ001*                   PROJETO 18/0456 - BORDERO ELETRONICO         *
BRQ001*================================================================*

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
       01  FILLER                      PIC  X(050)         VALUE
           '*** DCOM0410 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ATRIBUTOS ***'.
      *---------------------------------------------------------------*
      *--- NUMERIC  - NORMAL    - PROTEGIDO    - NAO CURSOR - ALF  ---*
       01  WRK-225                     PIC S9(008) COMP    VALUE +255.
      *--- BRILHANTE, NORMAL    - DEPROTEGIDO  - POS CURSOR - ALFA ---*
       01  WRK-49353                   PIC S9(008) COMP    VALUE +49353.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*
       01  WRK-AUXILIARES.
           05  WRK-IND                 PIC  9(002)         VALUE ZEROS.
           05  WRK-QTD-OPCAO-D         PIC  9(001)         VALUE ZEROS.
           05  WRK-SQLCA               PIC  X(136)         VALUE SPACES.
           05  WRK-MODULO              PIC  X(008)         VALUE SPACES.
           05  WRK-INCONSISTENCIA      PIC  X(001)         VALUE SPACES.
           05  WRK-FLAG-ERRO           PIC  X(001)         VALUE 'N'.
           05  WRK-MENSAGEM-ERRO       PIC  X(079)         VALUE SPACES.
           05  WRK-AGENCIA-AUX.
               10  WRK-AGENCIA-NUM     PIC  9(005)         VALUE ZEROS.
           05  WRK-MENSA-PF7           PIC  X(079) VALUE
               'PF7 DESPREZARA AS ATUALIZACOES EFETUADAS - TECLE NOVAMEN
      -        'TE PARA VOLTAR'.
           05  WRK-MENSA-PF8           PIC  X(079) VALUE
               'PF8 DESPREZARA AS ATUALIZACOES EFETUADAS - TECLE NOVAMEN
      -        'TE PARA AVANCAR'.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREAS AUXILIARES PARA POOL5005 ***'.
      *----------------------------------------------------------------*
       01  WRK-POOL5005.
           05  WRK-GU                  PIC  X(004)         VALUE 'GU  '.
           05  WRK-CHNG                PIC  X(004)         VALUE 'CHNG'.
           05  WRK-ISRT                PIC  X(004)         VALUE 'ISRT'.
           05  WRK-FUNCAO              PIC  X(004)         VALUE SPACES.
           05  WRK-TELA                PIC  X(008)         VALUE SPACES.
           05  WRK-VERSAO              PIC  X(006)         VALUE
               'VRS002'.
           05  WRK-COD-USER            PIC  X(007)         VALUE SPACES.
           05  WRK-COD-USER-R          REDEFINES
               WRK-COD-USER            PIC  9(007).
           05  WRK-COD-DEPTO.
               10  FILLER              PIC  X(002)         VALUE SPACES.
               10  WRK-COD-DEPTO-N     PIC  9(004)         VALUE ZEROS.

           05  WRK-MENSAGEM.
               10  FILLER              PIC  X(004)         VALUE SPACES.
               10  WRK-TRANSACAO       PIC  X(008)         VALUE SPACES.
               10  FILLER              PIC  X(1988)        VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ERRO DA BRAD0660 ***'.
      *----------------------------------------------------------------*
       01 WRK-ERRO-BRAD0660.
           05 FILLER                   PIC  X(045)         VALUE
              '* RETURN-CODE DIFERENTE DE ZEROS NA BRAD0660'.
           05 FILLER                   PIC  X(012)         VALUE
              '- RET.COD ='.
           05 WRK-RETURN-CODE          PIC  9(002)         VALUE ZEROS.
           05 FILLER                   PIC  X(011)         VALUE
              ' - LOCAL ='.
           05 WRK-LOCAL-ERRO           PIC  9(002)         VALUE ZEROS.
           05 FILLER                   PIC  X(002)         VALUE ' *'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA TELA DCOMDJTM  - INPUT ***'.
      *----------------------------------------------------------------*
       01  WRK-INPUT-DJTM.
           05  FILLER                  PIC  X(018) VALUE SPACES.
           05  WRK-INP-PFK-DJTM        PIC  X(001) VALUE SPACES.
           05  WRK-INP-SENHAS-DJTM     PIC  X(037) VALUE SPACES.
           05  WRK-INP-COMANDO-DJTM    PIC  X(068) VALUE SPACES.
           05  WRK-INP-DADOS-DJTM.
BRQ001         10  WRK-INP-TITULO-DJTM  PIC  X(054) VALUE SPACES.
               10  WRK-INP-FIMAMOS-DJTM PIC  X(001) VALUE SPACES.
               10  WRK-INP-FLAGENT-DJTM PIC  X(001) VALUE SPACES.
BRQ001         10  WRK-INP-OPCAO-DJTM   PIC  9(001) VALUE ZEROS.
               10  WRK-INP-TMSTAMP-DJTM PIC  X(026) VALUE SPACES.
               10  WRK-INP-PAGINA-DJTM-X.
                   15  WRK-INP-PAGINA-DJTM
                                        PIC  9(003) VALUE ZEROS.
               10  WRK-INP-NOME-DJTM    PIC  X(040) VALUE SPACES.
               10  WRK-INP-LITCPF-DJTM  PIC  X(009) VALUE SPACES.
FS2511         10  WRK-INP-CNPJCPF-DJTM.
FS2511*            15  WRK-INP-NUMERO-DJTM
FS2511*                                       PIC  9(009) VALUE ZEROS.
FS2511             15  WRK-INP-NUMERO-DJTM    PIC  X(009) VALUE SPACES.
FS2511             15  WRK-INP-FILLER-1       PIC  X(001) VALUE SPACES.
FS2511             15  WRK-INP-FILIAL-DJTM
FS2511                                        PIC  X(004) VALUE SPACES.
FS2511             15  WRK-INP-FILLER-2       PIC  X(001) VALUE SPACES.
                   15  WRK-INP-CONTROLE-DJTM
                                              PIC  9(002) VALUE ZEROS.
               10  WRK-INP-LITBCO-DJTM  PIC  X(006) VALUE SPACES.
               10  WRK-INP-BANCO-DJTM-X.
                   15  WRK-INP-BANCO-DJTM
                                        PIC  9(003) VALUE ZEROS.
               10  WRK-INP-LITAGE-DJTM  PIC  X(008) VALUE SPACES.
               10  WRK-INP-AGENCIA-DJTM.
                   15  WRK-INP-CODAGE-DJTM
                                        PIC  9(004) VALUE ZEROS.
                   15  WRK-INP-FILLER-3 PIC  X(001) VALUE SPACES.
                   15  WRK-INP-DESCAGE-DJTM
                                        PIC  X(021) VALUE SPACES.
               10  WRK-INP-LITCTA-DJTM  PIC  X(009) VALUE SPACES.
               10  WRK-INP-CONTA-DJTM-X.
                   15  WRK-INP-CONTA-DJTM
                                        PIC  9(013) VALUE ZEROS.
               10  WRK-INP-TABELA-DJTM    OCCURS  04  TIMES.
                   15  WRK-INP-SELEC-DJTM     PIC  X(001) VALUE SPACES.
                   15  WRK-INP-OPERA-DJTM-X.
                       20  WRK-INP-OPERA-DJTM PIC  9(013) VALUE ZEROS.
                   15  WRK-INP-PROD-DJTM      PIC  X(012) VALUE SPACES.
                   15  WRK-INP-SUBPROD-DJTM   PIC  X(010) VALUE SPACES.
                   15  WRK-INP-DTOPER-DJTM    PIC  X(010) VALUE SPACES.
                   15  WRK-INP-VALOR-DJTM     PIC  X(017) VALUE SPACES.
                   15  WRK-INP-MEIOENT-DJTM   PIC  X(015) VALUE SPACES.
                   15  WRK-INP-SITANAL-DJTM   PIC  X(010) VALUE SPACES.
                   15  WRK-INP-PARECER-DJTM   PIC  X(003) VALUE SPACES.
           05  WRK-INP-MENSA-DJTM       PIC  X(079) VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                       PIC  X(050)         VALUE
           '*** AREA DA TELA DCOMDJTM  - OUTPUT ***'.
      *----------------------------------------------------------------*
       01  WRK-OUTPUT-DJTM.
BRQ001     05  WRK-OUT-LL-DJTM          PIC S9(004) COMP VALUE ZEROS.
           05  WRK-OUT-ZZ-DJTM          PIC  9(004) COMP VALUE ZEROS.
           05  WRK-OUT-SENHAS-DJTM      PIC  X(037) VALUE SPACES.
           05  WRK-OUT-COMANDO-DJTM     PIC  X(068) VALUE SPACES.
           05  WRK-OUT-DADOS-DJTM.
BRQ001         10  WRK-OUT-TITULO-DJTM  PIC  X(054) VALUE SPACES.
               10  WRK-OUT-FIMAMOS-DJTM PIC  X(001) VALUE SPACES.
               10  WRK-OUT-FLAGENT-DJTM PIC  X(001) VALUE SPACES.
BRQ001         10  WRK-OUT-OPCAO-DJTM   PIC  9(001) VALUE ZEROS.
               10  WRK-OUT-TMSTAMP-DJTM PIC  X(026) VALUE SPACES.
               10  WRK-OUT-PAGINA-DJTM-X.
                   15  WRK-OUT-PAGINA-DJTM
                                        PIC  9(003) VALUE ZEROS.
               10  WRK-OUT-NOME-DJTM    PIC  X(040) VALUE SPACES.
               10  WRK-OUT-LITCPF-DJTM  PIC  X(009) VALUE SPACES.
FS2511         10  WRK-OUT-CNPJCPF-DJTM.
FS2511*            15  WRK-OUT-NUMERO-DJTM
FS2511*                                       PIC  9(009) VALUE ZEROS.
FS2511             15  WRK-OUT-NUMERO-DJTM    PIC  X(009) VALUE SPACES.
FS2511             15  WRK-OUT-FILLER-1 PIC  X(001) VALUE SPACES.
FS2511             15  WRK-OUT-FILIAL-DJTM
FS2511                                       PIC  X(004) VALUE SPACES.
FS2511             15  WRK-OUT-FILLER-2 PIC  X(001) VALUE SPACES.
                   15  WRK-OUT-CONTROLE-DJTM
                                        PIC  9(002) VALUE ZEROS.
               10  WRK-OUT-LITBCO-DJTM  PIC  X(006) VALUE SPACES.
               10  WRK-OUT-BANCO-DJTM-X.
                   15  WRK-OUT-BANCO-DJTM
                                        PIC  9(003) VALUE ZEROS.
               10  WRK-OUT-LITAGE-DJTM  PIC  X(008) VALUE SPACES.
               10  WRK-OUT-AGENCIA-DJTM.
                   15  WRK-OUT-CODAGE-DJTM
                                        PIC  9(004) VALUE ZEROS.
                   15  WRK-OUT-FILLER-3 PIC  X(001) VALUE SPACES.
                   15  WRK-OUT-DESCAGE-DJTM
                                        PIC  X(021) VALUE SPACES.
               10  WRK-OUT-LITCTA-DJTM  PIC  X(009) VALUE SPACES.
               10  WRK-OUT-CONTA-DJTM-X.
                   15  WRK-OUT-CONTA-DJTM
                                        PIC  9(013) VALUE ZEROS.
               10  WRK-OUT-TABELA-DJTM    OCCURS  04  TIMES.
                   15  WRK-OUT-SELEC-ATTR-DJTM PIC  9(04) COMP.
                   15  WRK-OUT-SELEC-DJTM      PIC  X(001) VALUE SPACES.
                   15  WRK-OUT-OPERA-DJTM-X.
                       20  WRK-OUT-OPERA-DJTM  PIC  9(013) VALUE ZEROS.
                   15  WRK-OUT-PROD-DJTM       PIC  X(012) VALUE SPACES.
                   15  WRK-OUT-SUBPROD-DJTM    PIC  X(010) VALUE SPACES.
                   15  WRK-OUT-DTOPER-DJTM     PIC  X(010) VALUE SPACES.
                   15  WRK-OUT-VALOR-DJTM-X.
                       20  WRK-OUT-VALOR-DJTM  PIC  ZZ.ZZZ.ZZZ.ZZ9,99.
                   15  WRK-OUT-MEIOENT-DJTM    PIC  X(015) VALUE SPACES.
                   15  WRK-OUT-SITANAL-DJTM    PIC  X(010) VALUE SPACES.
                   15  WRK-OUT-PARECER-DJTM    PIC  X(003) VALUE SPACES.
               10  WRK-OUT-MENSA-DJTM   PIC  X(079) VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA TELA DCOMDJTM  - BRAD0660 ***'.
      *----------------------------------------------------------------*
       01  WRK-660-DCOMDJTM.
BRQ001     05  WRK-660-LL-AREA-DJTM     PIC  9(004) COMP   VALUE 0000.
BRQ001     05  WRK-660-LL-MENSAGEM-DJTM PIC  9(004) COMP   VALUE 0000.
           05  WRK-660-SENHAS-DJTM      PIC  9(004) COMP   VALUE 0037.
           05  WRK-660-COMANDO-DJTM     PIC  9(004) COMP   VALUE 0068.
BRQ001     05  WRK-660-TITULO-DJTM      PIC  9(004) COMP   VALUE 0054.
           05  WRK-660-FIMAMOS-DJTM     PIC  9(004) COMP   VALUE 0001.
           05  WRK-660-FLAGENT-DJTM     PIC  9(004) COMP   VALUE 0001.
BRQ001     05  WRK-660-OPCAO-DJTM       PIC  9(004) COMP   VALUE 0001.
           05  WRK-660-TMSTAMP-DJTM     PIC  9(004) COMP   VALUE 0026.
           05  WRK-660-PAGINA-DJTM      PIC  9(004) COMP   VALUE 0003.
           05  WRK-660-NOME-DJTM        PIC  9(004) COMP   VALUE 0040.
           05  WRK-660-LITCPF-DJTM      PIC  9(004) COMP   VALUE 0009.
           05  WRK-660-CNPJCPF-DJTM     PIC  9(004) COMP   VALUE 0017.
           05  WRK-660-LITBCO-DJTM      PIC  9(004) COMP   VALUE 0006.
           05  WRK-660-BANCO-DJTM       PIC  9(004) COMP   VALUE 0003.
           05  WRK-660-LITAGE-DJTM      PIC  9(004) COMP   VALUE 0008.
           05  WRK-660-AGENCIA-DJTM     PIC  9(004) COMP   VALUE 0026.
           05  WRK-660-LITCTA-DJTM      PIC  9(004) COMP   VALUE 0009.
           05  WRK-660-CONTA-DJTM       PIC  9(004) COMP   VALUE 0013.
           05  WRK-660-TABELA-DJTM    OCCURS  04  TIMES.
               10  WRK-660-SELEC-DJTM   PIC  9(004) COMP   VALUE 2003.
               10  WRK-660-OPERA-DJTM   PIC  9(004) COMP   VALUE 0013.
               10  WRK-660-PROD-DJTM    PIC  9(004) COMP   VALUE 0012.
               10  WRK-660-SUBPROD-DJTM PIC  9(004) COMP   VALUE 0010.
               10  WRK-660-DTOPER-DJTM  PIC  9(004) COMP   VALUE 0010.
               10  WRK-660-VALOR-DJTM   PIC  9(004) COMP   VALUE 0017.
               10  WRK-660-MEIOENT-DJTM PIC  9(004) COMP   VALUE 0015.
               10  WRK-660-SITANAL-DJTM PIC  9(004) COMP   VALUE 0010.
               10  WRK-660-PARECER-DJTM PIC  9(004) COMP   VALUE 0003.
           05  WRK-660-MENSA-DJTM       PIC  9(004) COMP   VALUE 0079.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(080)         VALUE
           '*** AREA DE RECEBIMENTO E RETORNO DO DCOM0402 ***'.
      *----------------------------------------------------------------*
       01  COMU-AREA-UN .
           05  COMU-LL-UN              PIC S9(004) COMP  VALUE +297.
           05  COMU-ZZ-UN              PIC S9(004) COMP  VALUE +0.
           05  COMU-TRANCODE-UN .
               10  COMU-TRANSACAO-UN   PIC  X(008)       VALUE SPACES.
               10  FILLER-UN           PIC  X(006)       VALUE SPACES.
               10  COMU-PFK-UN         PIC  X(001)       VALUE SPACES.
           05  COMU-SENHAS-UN          PIC  X(037)       VALUE SPACES.
           05  COMU-COMANDO-UN         PIC  X(068)       VALUE SPACES.
           05  COMU-DADOS-UN .
               10  COMU-OPCAO-UN       PIC  9(001)       VALUE ZEROS.
               10  COMU-OPER-UN        PIC  9(013)       VALUE ZEROS.
               10  COMU-AGENCIA-UN     PIC  9(005)       VALUE ZEROS.
               10  COMU-AGENC-UN-X.
                   15  COMU-AGENC-UN   PIC  9(005)       VALUE ZEROS.
               10  COMU-CONTA-UN       PIC  9(013)       VALUE ZEROS.
FS2511         10  COMU-CNPJ-UN        PIC  X(009)       VALUE SPACES.
FS2511         10  COMU-FILIAL-UN      PIC  X(004)       VALUE SPACES.
               10  COMU-DIG-UN         PIC  9(002)       VALUE ZEROS.
               10  COMU-OPERACAO-UN.
                 15 COMU-DANO-OPER-UN  PIC  9(004)       VALUE ZEROS.
                 15 COMU-NSEQ-OPER-UN  PIC  9(009)       VALUE ZEROS.
               10  COMU-TIMESTAMP-UN   PIC  X(026)       VALUE ZEROS.
               10  COMU-MENSAGEM-UN    PIC  X(079)       VALUE SPACES.
               10  COMU-PAGINA-UN      PIC  9(003)       VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO COM O MODULO DCOM4711 **'.
      *----------------------------------------------------------------*
       01  WRK-4711E-ENTRADA.
           03  WRK-4711E-COD-RETORNO             PIC  X(04).
           03  WRK-4711E-MSG-RETORNO             PIC  X(79).
           03  WRK-4711E-RESTART                 PIC  9(05).
           03  WRK-4711E-FLAG                    PIC  X(01).
           03  WRK-4711E-START-COUNT             PIC  X(01).
           03  WRK-4711E-CFUNC-BDSCO             PIC  9(09).
           03  WRK-4711E-CTERM                   PIC  X(08).
           03  WRK-4711E-FLAG-FILTRO             PIC  X(02).
           03  WRK-4711E-CBCO                    PIC  9(03).
           03  WRK-4711E-CAG-BCRIA               PIC  9(05).
           03  WRK-4711E-CAG-OPER                PIC  9(05).
           03  WRK-4711E-CCTA-BCRIA-CLI          PIC  9(13).
           03  WRK-4711E-CPRODT                  PIC  9(03).
           03  WRK-4711E-CSPROD-DESC-COML        PIC  9(03).
           03  WRK-4711E-DINIC-OPER-DESC         PIC  X(10).
           03  WRK-4711E-CELMTO-DESC-COML        PIC  9(03).
           03  WRK-4711E-CCNPJ-CPF               PIC  X(09).
           03  WRK-4711E-CFLIAL-CNPJ             PIC  X(04).
           03  WRK-4711E-CCTRL-CNPJ-CPF          PIC  9(02).
           03  WRK-4711E-CSIT-DESC-COML          PIC  9(03).
           03  WRK-4711E-CINDCD-EVNTO-WF         PIC  9(03).
           03  WRK-4711E-CINDCD-SIT-F-EVNTO      PIC  9(03).

       01  WRK-4711S-SAIDA.
           03  WRK-4711S-COD-RETORNO                PIC  X(04).
           03  WRK-4711S-MSG-RETORNO                PIC  X(79).
           03  WRK-4711S-RESTART                    PIC  9(05).
           03  WRK-4711S-FLAG                       PIC  X(01).
           03  WRK-4711S-QTDE-RETORNADA             PIC  9(03).
           03  WRK-4711S-COUNT                      PIC  9(09).
           03  WRK-4711S-DADOS.
               05  WRK-4711S-TABELA-SAIDA    OCCURS 10 TIMES.
                   10  WRK-4711S-DANO-OPER-DESC     PIC  9(04).
                   10  WRK-4711S-NSEQ-OPER-DESC     PIC  9(09).
                   10  WRK-4711S-IPSSOA-DESC-COML   PIC  X(60).
                   10  WRK-4711S-CBCO               PIC  9(03).
                   10  WRK-4711S-CAG-BCRIA          PIC  9(05).
                   10  WRK-4711S-CAG-BCRIA-DESC     PIC  X(40).
                   10  WRK-4711S-CCTA-BCRIA-CLI     PIC  9(13).
                   10  WRK-4711S-CBCO-OPER-OPER     PIC  9(03).
                   10  WRK-4711S-CAG-OPER-OPER      PIC  9(05).
                   10  WRK-4711S-CSIT-DESC-COML     PIC  9(03).
                   10  WRK-4711S-IRSUMO-SIT-DESC    PIC  X(15).
                   10  WRK-4711S-CSGL-SIT-DESC      PIC  X(06).
                   10  WRK-4711S-CPRODT             PIC  9(03).
                   10  WRK-4711S-IABREV-PRODT       PIC  X(12).
                   10  WRK-4711S-CSGL-PRODT-DESC    PIC  X(06).
                   10  WRK-4711S-CSPROD-DESC-COML   PIC  9(03).
                   10  WRK-4711S-CTPO-DESC-COML     PIC  9(03).
                   10  WRK-4711S-IRSUMO-SPROD-DESC  PIC  X(10).
                   10  WRK-4711S-CSGL-SPROD-DESC    PIC  X(06).
                   10  WRK-4711S-VOPER-DESC-COML    PIC  9(15)V9(2).
                   10  WRK-4711S-DINIC-OPER-DESC    PIC  X(10).
                   10  WRK-4711S-DVCTO-FNAL-OPER    PIC  X(10).
                   10  WRK-4711S-CMEIO-ENTRD-DESC   PIC  9(03).
                   10  WRK-4711S-IMEIO-ENTRD-DESC   PIC  X(25).
                   10  WRK-4711S-IRSUMO-MEIO-ENTRD  PIC  X(15).
                   10  WRK-4711S-CSGL-MEIO-ENTRD    PIC  X(06).
                   10  WRK-4711S-CINDCD-LIBRC-ANTCP PIC  X(01).
                   10  WRK-4711S-PARECER-FORML      PIC  X(01).
                   10  WRK-4711S-VLIQ-OPER-DESC     PIC  9(15)V9(2).

           03  WRK-4711S-CONSISTENCIA   REDEFINES   WRK-4711S-DADOS.
               05  WRK-4711S-TAMANHO-ERRO           PIC  9(01).
               05  WRK-4711S-TABELA-ERRO   OCCURS   2    TIMES.
                   10  WRK-4711S-CAMPOS-ERRO        PIC  9(01).
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO COM O MODULO DCOM4712 **'.
      *----------------------------------------------------------------*
       01  WRK-4712E-ENTRADA.
           03  WRK-4712E-COD-RETORNO             PIC  X(04).
           03  WRK-4712E-MSG-RETORNO             PIC  X(79).
           03  WRK-4712E-RESTART                 PIC  9(05).
           03  WRK-4712E-FLAG                    PIC  X(01).
           03  WRK-4712E-START-COUNT             PIC  X(01).
           03  WRK-4712E-CFUNC-BDSCO             PIC  9(09).
           03  WRK-4712E-CTERM                   PIC  X(08).
           03  WRK-4712E-FLAG-FILTRO             PIC  X(002).
           03  WRK-4712E-CBCO                    PIC  9(003).
           03  WRK-4712E-CAG-BCRIA               PIC  9(005).
           03  WRK-4712E-CAG-OPER                PIC  9(005).
           03  WRK-4712E-CCTA-BCRIA-CLI          PIC  9(013).
           03  WRK-4712E-CPRODT                  PIC  9(003).
           03  WRK-4712E-CSPROD-DESC-COML        PIC  9(003).
           03  WRK-4712E-DINIC-OPER-DESC         PIC  X(010).
           03  WRK-4712E-CELMTO-DESC-COML        PIC  9(003).
FS2511*    03  WRK-4712E-CCNPJ-CPF               PIC  9(009).
FS2511*    03  WRK-4712E-CFLIAL-CNPJ             PIC  9(004).
FS2511     03  WRK-4712E-CCNPJ-CPF               PIC  X(009).
FS2511     03  WRK-4712E-CFLIAL-CNPJ             PIC  X(004).
           03  WRK-4712E-CCTRL-CNPJ-CPF          PIC  9(002).
           03  WRK-4712E-CSIT-DESC-COML          PIC  9(003).
           03  WRK-4712E-CINDCD-EVNTO-WF         PIC  9(003).
           03  WRK-4712E-CINDCD-SIT-F-EVNTO      PIC  9(003).

       01  WRK-4712S-SAIDA.
           03  WRK-4712S-COD-RETORNO                PIC  X(04).
           03  WRK-4712S-MSG-RETORNO                PIC  X(79).
           03  WRK-4712S-RESTART                    PIC  9(05).
           03  WRK-4712S-FLAG                       PIC  X(01).
           03  WRK-4712S-QTDE-RETORNADA             PIC  9(03).
           03  WRK-4712S-COUNT                      PIC  9(09).
           03  WRK-4712S-DADOS.
               05  WRK-4712S-TABELA-SAIDA    OCCURS 10 TIMES.
                   10  WRK-4712S-DANO-OPER-DESC     PIC  9(04).
                   10  WRK-4712S-NSEQ-OPER-DESC     PIC  9(009).
                   10  WRK-4712S-IPSSOA-DESC-COML   PIC  X(060).
                   10  WRK-4712S-CBCO               PIC  9(003).
                   10  WRK-4712S-CAG-BCRIA          PIC  9(005).
                   10  WRK-4712S-CAG-BCRIA-DESC     PIC  X(040).
                   10  WRK-4712S-CCTA-BCRIA-CLI     PIC  9(013).
                   10  WRK-4712S-CBCO-OPER-OPER     PIC  9(003).
                   10  WRK-4712S-CAG-OPER-OPER      PIC  9(005).
                   10  WRK-4712S-CSIT-DESC-COML     PIC  9(003).
                   10  WRK-4712S-IRSUMO-SIT-DESC    PIC  X(015).
                   10  WRK-4712S-CSGL-SIT-DESC      PIC  X(006).
                   10  WRK-4712S-CPRODT             PIC  9(003).
                   10  WRK-4712S-IABREV-PRODT       PIC  X(012).
                   10  WRK-4712S-CSGL-PRODT-DESC    PIC  X(006).
                   10  WRK-4712S-CSPROD-DESC-COML   PIC  9(003).
                   10  WRK-4712S-CTPO-DESC-COML     PIC  9(003).
                   10  WRK-4712S-IRSUMO-SPROD-DESC  PIC  X(010).
                   10  WRK-4712S-CSGL-SPROD-DESC    PIC  X(006).
                   10  WRK-4712S-VOPER-DESC-COML    PIC  9(015)V9(2).
                   10  WRK-4712S-DINIC-OPER-DESC    PIC  X(010).
                   10  WRK-4712S-DVCTO-FNAL-OPER    PIC  X(010).
                   10  WRK-4712S-CMEIO-ENTRD-DESC   PIC  9(03).
                   10  WRK-4712S-IMEIO-ENTRD-DESC   PIC  X(25).
                   10  WRK-4712S-IRSUMO-MEIO-ENTRD  PIC  X(15).
                   10  WRK-4712S-CSGL-MEIO-ENTRD    PIC  X(06).
                   10  WRK-4712S-CINDCD-LIBRC-ANTCP PIC  X(01).
                   10  WRK-4712S-PARECER-FORML      PIC  X(01).
                   10  WRK-4712S-VLIQ-OPER-DESCL    PIC  9(015)V9(2).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA BRAD7100 ***'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** DCOM0410 - FIM DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01 LNK-IO-PCB.
           05 LNK-IO-LTERM             PIC  X(008).
           05 FILLER                   PIC  X(002).
           05 LNK-IO-STATUS            PIC  X(002).
           05 FILLER                   PIC  X(012).
           05 LNK-IO-MODNAME           PIC  X(008).

       01 LNK-ALT-PCB.
           05 LNK-ALT-LTERM            PIC  X(008).
           05 FILLER                   PIC  X(002).
           05 LNK-ALT-STATUS           PIC  X(002).
           05 FILLER                   PIC  X(012).
           05 LNK-ALT-MODNAME          PIC  X(008).

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

           ENTRY 'DLITCBL'             USING LNK-IO-PCB
                                             LNK-ALT-PCB.

           MOVE WRK-GU                 TO WRK-FUNCAO.

      *----------------------------------------------------------------*
      *  ESSA ROTINA CONTEM OS PROCEDIMENTOS INICIAIS                  *
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

           IF  RETURN-CODE          EQUAL  04 OR 12
               PERFORM 2000-FINALIZAR
           END-IF.

           MOVE SPACES                 TO  WRK-OUT-DADOS-DJTM
                                           WRK-OUT-SENHAS-DJTM
                                           WRK-OUT-COMANDO-DJTM
                                           WRK-INCONSISTENCIA.

           PERFORM 1000-PROCESSAR-MENSAGEM.

           GO TO 0000-INICIAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA PROGRAMA A MENSAGEM                               *
      *----------------------------------------------------------------*
       1000-PROCESSAR-MENSAGEM         SECTION.
      *----------------------------------------------------------------*

           MOVE 'DCOMDJTM'             TO  WRK-TELA.

           IF  WRK-TRANSACAO   NOT EQUAL 'DCOM0410'
               PERFORM 1100-RECEBER-CONTROLE
           ELSE
               PERFORM 1300-PROCESSAR-DCOMDJTM
           END-IF.

           IF  WRK-TELA             EQUAL  'DCOMDJTM'
               PERFORM 1400-PROCESSAR-BRAD0660
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA RECEBE O CONTROLE DO DCOM0402                     *
      *----------------------------------------------------------------*
       1100-RECEBER-CONTROLE           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-4711E-ENTRADA
                      WRK-4711S-SAIDA
                      WRK-4712E-ENTRADA
                      WRK-4712S-SAIDA.

           MOVE WRK-MENSAGEM           TO  COMU-AREA-UN.

BRQ001     IF  COMU-OPCAO-UN           EQUAL 3
BRQ001         MOVE '   CONSULTAR LIBERACAO - OPERACOES POR CLIENTE
BRQ001-             '   '              TO WRK-OUT-TITULO-DJTM
BRQ001     ELSE
BRQ001         IF  COMU-OPCAO-UN       EQUAL 4
BRQ001             MOVE 'CONSULTAR PENDENTES NOS CANAIS - OPERACOES POR
BRQ001-                 'CLIENTE'      TO WRK-OUT-TITULO-DJTM
BRQ001         END-IF
BRQ001     END-IF.
BRQ001
BRQ001     MOVE COMU-OPCAO-UN          TO  WRK-INP-OPCAO-DJTM
BRQ001                                     WRK-OUT-OPCAO-DJTM
BRQ001
           MOVE 1                      TO  WRK-OUT-PAGINA-DJTM.
           MOVE COMU-TIMESTAMP-UN      TO WRK-INP-TMSTAMP-DJTM
                                          WRK-OUT-TMSTAMP-DJTM

           IF COMU-AGENC-UN            NOT EQUAL ZEROS
           AND COMU-AGENC-UN-X         NOT EQUAL SPACES AND LOW-VALUES
              MOVE '1'                 TO  WRK-OUT-FLAGENT-DJTM
              MOVE COMU-AGENC-UN       TO  WRK-OUT-CODAGE-DJTM
                                           WRK-4711E-CAG-BCRIA
              MOVE '-'                 TO  WRK-OUT-FILLER-3
              MOVE COMU-CONTA-UN       TO  WRK-OUT-CONTA-DJTM
                                           WRK-4711E-CCTA-BCRIA-CLI
              MOVE SPACES              TO  WRK-OUT-NOME-DJTM
                                           WRK-OUT-CNPJCPF-DJTM
                                           WRK-OUT-DESCAGE-DJTM
                                           WRK-OUT-FILLER-1
                                           WRK-OUT-FILLER-2

              IF  WRK-TRANSACAO     EQUAL 'DCOM0411'
                  MOVE COMU-PAGINA-UN
                                    TO WRK-OUT-PAGINA-DJTM
                  COMPUTE WRK-4711E-RESTART = (COMU-PAGINA-UN - 1) * 4
                  PERFORM 1500-ACESSAR-DCOM4711

                  IF  WRK-4711S-COD-RETORNO EQUAL '0003'
                  AND COMU-PAGINA-UN GREATER 1
                      INITIALIZE WRK-4711E-ENTRADA
                                 WRK-4711S-SAIDA
                      MOVE 1        TO WRK-OUT-PAGINA-DJTM
                      MOVE COMU-AGENC-UN
                                    TO WRK-4711E-CAG-BCRIA
                      MOVE COMU-CONTA-UN
                                    TO WRK-4711E-CCTA-BCRIA-CLI
                      MOVE ZEROS    TO  WRK-4711E-RESTART
                      PERFORM 1500-ACESSAR-DCOM4711
                  END-IF
              ELSE
                  MOVE ZEROS        TO  WRK-4711E-RESTART
                  PERFORM 1500-ACESSAR-DCOM4711
              END-IF

              MOVE WRK-4711S-IPSSOA-DESC-COML(1)
                                    TO WRK-OUT-NOME-DJTM
              MOVE WRK-4711S-CAG-BCRIA-DESC(1)
                                    TO WRK-OUT-DESCAGE-DJTM
              MOVE 237              TO  WRK-OUT-BANCO-DJTM
              MOVE 'BANCO:'         TO WRK-OUT-LITBCO-DJTM
              MOVE 'AGENCIA:'       TO WRK-OUT-LITAGE-DJTM
              MOVE 'CONTA...:'      TO WRK-OUT-LITCTA-DJTM

           ELSE
              MOVE 'CNPJ/CPF:'         TO  WRK-OUT-LITCPF-DJTM
              MOVE '2'                 TO  WRK-OUT-FLAGENT-DJTM
              MOVE COMU-CNPJ-UN        TO  WRK-OUT-NUMERO-DJTM
                                           WRK-4712E-CCNPJ-CPF
              MOVE '/'                 TO  WRK-OUT-FILLER-1
              MOVE COMU-FILIAL-UN      TO  WRK-OUT-FILIAL-DJTM
                                           WRK-4712E-CFLIAL-CNPJ
              MOVE '-'                 TO  WRK-OUT-FILLER-2
              MOVE COMU-DIG-UN         TO  WRK-OUT-CONTROLE-DJTM
                                           WRK-4712E-CCTRL-CNPJ-CPF
              MOVE SPACES              TO  WRK-OUT-NOME-DJTM
                                           WRK-OUT-AGENCIA-DJTM
                                           WRK-OUT-CONTA-DJTM-X
                                           WRK-OUT-FILLER-3
                                           WRK-OUT-LITBCO-DJTM
                                           WRK-OUT-LITAGE-DJTM
                                           WRK-OUT-LITCTA-DJTM

              IF  WRK-TRANSACAO        EQUAL 'DCOM0411'
                  MOVE COMU-PAGINA-UN  TO WRK-OUT-PAGINA-DJTM
                  COMPUTE WRK-4712E-RESTART = (COMU-PAGINA-UN - 1) * 4
                  PERFORM 1600-ACESSAR-DCOM4712

                  IF  WRK-4712S-COD-RETORNO EQUAL '0003'
                  AND COMU-PAGINA-UN GREATER 1
                      INITIALIZE WRK-4712E-ENTRADA
                                 WRK-4712S-SAIDA
                      MOVE 1           TO WRK-OUT-PAGINA-DJTM
                      MOVE COMU-CNPJ-UN TO WRK-4712E-CCNPJ-CPF
                      MOVE COMU-FILIAL-UN TO WRK-4712E-CFLIAL-CNPJ
                      MOVE COMU-DIG-UN TO  WRK-4712E-CCTRL-CNPJ-CPF
                      MOVE ZEROS       TO  WRK-4712E-RESTART
                      PERFORM 1600-ACESSAR-DCOM4712
                  END-IF
              ELSE
                  MOVE ZEROS           TO  WRK-4712E-RESTART
                  PERFORM 1600-ACESSAR-DCOM4712
              END-IF

              MOVE WRK-4712S-IPSSOA-DESC-COML(1)
                                    TO WRK-OUT-NOME-DJTM

           END-IF.

           IF  WRK-4711S-COD-RETORNO   EQUAL '0003'
           OR (WRK-4712S-COD-RETORNO   EQUAL '0003' OR '0915')
               MOVE 'S'                TO WRK-FLAG-ERRO
               MOVE 'NAO EXISTE(M) OPERACAO(OES) PARA EFETUAR CONSULTA'
                                       TO WRK-MENSAGEM-ERRO
               PERFORM 1320-RETORNAR-CHAMADOR
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA PROCESSA A MENSAGEM VINDA DA TELA DCOMDJTM        *
      *----------------------------------------------------------------*
       1300-PROCESSAR-DCOMDJTM         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM             TO WRK-INPUT-DJTM.
           PERFORM 1310-DEVOLVER-TELA

           EVALUATE TRUE
               WHEN WRK-INP-PFK-DJTM     EQUAL 'H' OR
                   (WRK-INP-PFK-DJTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DJTM EQUAL 'PFK1')
                    PERFORM VARYING WRK-IND FROM 1 BY 1
                              UNTIL WRK-IND GREATER 4
                       IF  WRK-INP-OPERA-DJTM-X(WRK-IND) EQUAL SPACES
                                                          OR LOW-VALUES
                            MOVE WRK-225 TO WRK-OUT-SELEC-ATTR-DJTM
                                                          (WRK-IND)
                        END-IF
                    END-PERFORM
                    MOVE WRK-INP-MENSA-DJTM TO WRK-OUT-MENSA-DJTM

               WHEN WRK-INP-PFK-DJTM     EQUAL '3' OR
                   (WRK-INP-PFK-DJTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DJTM EQUAL 'PFK3')
                    PERFORM 1320-RETORNAR-CHAMADOR

               WHEN WRK-INP-PFK-DJTM     EQUAL 'X' OR
                   (WRK-INP-PFK-DJTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DJTM EQUAL 'PFK5')
                    PERFORM 1330-RETORNAR-MENU-ROTINAS

               WHEN WRK-INP-PFK-DJTM     EQUAL 'Z' OR
                   (WRK-INP-PFK-DJTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DJTM EQUAL 'PFK10')
                    PERFORM 1340-RETORNAR-MENU-DCOM

               WHEN WRK-INP-PFK-DJTM     EQUAL '7' OR
                   (WRK-INP-PFK-DJTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DJTM EQUAL 'PFK7')
                    PERFORM 1360-TRATAR-VOLTA-PAG

               WHEN WRK-INP-PFK-DJTM     EQUAL '8' OR
                   (WRK-INP-PFK-DJTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DJTM EQUAL 'PFK8')
                    PERFORM 1370-TRATAR-AVANCA-PAG

               WHEN WRK-INP-PFK-DJTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DJTM EQUAL  SPACES OR LOW-VALUES
                    PERFORM 1380-TRATAR-ENTER

                    IF  WRK-INCONSISTENCIA NOT EQUAL SPACES
                        PERFORM VARYING WRK-IND FROM 1 BY 1
                                  UNTIL WRK-IND GREATER 4
                            IF  WRK-INP-OPERA-DJTM-X(WRK-IND) EQUAL
                                                SPACES OR LOW-VALUES
                                MOVE WRK-225
                                        TO WRK-OUT-SELEC-ATTR-DJTM
                                                         (WRK-IND)
                            END-IF
                        END-PERFORM
                    END-IF

               WHEN OTHER
                    MOVE 'PF INVALIDA'   TO WRK-OUT-MENSA-DJTM
                    PERFORM VARYING WRK-IND FROM 1 BY 1
                              UNTIL WRK-IND GREATER 4
                       IF  WRK-INP-OPERA-DJTM-X(WRK-IND) EQUAL SPACES
                                                          OR LOW-VALUES
                            MOVE WRK-225 TO WRK-OUT-SELEC-ATTR-DJTM
                                                          (WRK-IND)
                        END-IF
                    END-PERFORM
           END-EVALUATE.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA DEVOLVE OS DADOS DA TELA SEM ALTERACAO            *
      *----------------------------------------------------------------*
       1310-DEVOLVER-TELA              SECTION.
      *----------------------------------------------------------------*

BRQ01      MOVE WRK-INP-TITULO-DJTM    TO WRK-OUT-TITULO-DJTM.
           MOVE WRK-INP-FIMAMOS-DJTM   TO WRK-OUT-FIMAMOS-DJTM.
           MOVE WRK-INP-SENHAS-DJTM    TO WRK-OUT-SENHAS-DJTM.
           MOVE WRK-INP-FLAGENT-DJTM   TO WRK-OUT-FLAGENT-DJTM.
BRQ001     MOVE WRK-INP-OPCAO-DJTM     TO WRK-OUT-OPCAO-DJTM.
           MOVE WRK-INP-TMSTAMP-DJTM   TO WRK-OUT-TMSTAMP-DJTM.
           MOVE WRK-INP-PAGINA-DJTM-X  TO WRK-OUT-PAGINA-DJTM-X.
           MOVE WRK-INP-NOME-DJTM      TO WRK-OUT-NOME-DJTM.
           MOVE WRK-INP-LITCPF-DJTM    TO WRK-OUT-LITCPF-DJTM.
           MOVE WRK-INP-CNPJCPF-DJTM   TO WRK-OUT-CNPJCPF-DJTM.
           MOVE WRK-INP-BANCO-DJTM-X   TO WRK-OUT-BANCO-DJTM-X.
           MOVE WRK-INP-AGENCIA-DJTM   TO WRK-OUT-AGENCIA-DJTM.
           MOVE WRK-INP-CONTA-DJTM-X   TO WRK-OUT-CONTA-DJTM-X.
           MOVE WRK-INP-LITBCO-DJTM    TO WRK-OUT-LITBCO-DJTM
           MOVE WRK-INP-LITAGE-DJTM    TO WRK-OUT-LITAGE-DJTM
           MOVE WRK-INP-LITCTA-DJTM    TO WRK-OUT-LITCTA-DJTM

           PERFORM VARYING WRK-IND FROM 1 BY 1 UNTIL WRK-IND GREATER 4
              MOVE WRK-INP-SELEC-DJTM(WRK-IND)
                                      TO WRK-OUT-SELEC-DJTM(WRK-IND)
              MOVE WRK-INP-OPERA-DJTM-X(WRK-IND)
                                      TO WRK-OUT-OPERA-DJTM-X(WRK-IND)
              MOVE WRK-INP-PROD-DJTM(WRK-IND)
                                      TO WRK-OUT-PROD-DJTM(WRK-IND)
              MOVE WRK-INP-SUBPROD-DJTM(WRK-IND)
                                      TO WRK-OUT-SUBPROD-DJTM(WRK-IND)
              MOVE WRK-INP-DTOPER-DJTM(WRK-IND)
                                      TO WRK-OUT-DTOPER-DJTM(WRK-IND)
              MOVE WRK-INP-VALOR-DJTM(WRK-IND)
                                      TO WRK-OUT-VALOR-DJTM-X(WRK-IND)
              MOVE WRK-INP-MEIOENT-DJTM(WRK-IND)
                                      TO WRK-OUT-MEIOENT-DJTM(WRK-IND)
              MOVE WRK-INP-SITANAL-DJTM(WRK-IND)
                                      TO WRK-OUT-SITANAL-DJTM(WRK-IND)
              MOVE WRK-INP-PARECER-DJTM(WRK-IND)
                                      TO WRK-OUT-PARECER-DJTM(WRK-IND)
           END-PERFORM.

      *----------------------------------------------------------------*
       1310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA RETORNA AO PROGRAMA CHAMADOR                      *
      *----------------------------------------------------------------*
       1320-RETORNAR-CHAMADOR          SECTION.
      *----------------------------------------------------------------*

           INITIALIZE COMU-AREA-UN.

           IF  WRK-FLAG-ERRO         EQUAL 'S'
               MOVE WRK-MENSAGEM-ERRO
                                     TO COMU-MENSAGEM-UN
           END-IF.

           MOVE LENGTH OF COMU-AREA-UN TO COMU-LL-UN
           MOVE ZEROS                TO COMU-ZZ-UN.

           MOVE WRK-CHNG             TO WRK-FUNCAO.
           MOVE 'DCOM0402'           TO WRK-TELA.
           MOVE 'DCOM0410'           TO COMU-TRANSACAO-UN.
           MOVE WRK-INP-SENHAS-DJTM  TO COMU-SENHAS-UN
           MOVE WRK-INP-TMSTAMP-DJTM TO COMU-TIMESTAMP-UN

           MOVE COMU-AREA-UN         TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1320-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRATAMENTO DE RETORNO AO MENU ROTINAS    *
      *----------------------------------------------------------------*
       1330-RETORNAR-MENU-ROTINAS      SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-CHNG            TO WRK-FUNCAO.
           MOVE 'DCOM0410'          TO WRK-TELA
                                       COMU-TRANSACAO-UN.
           MOVE '5'                 TO COMU-PFK-UN.
           MOVE COMU-AREA-UN        TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1330-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRATAMENTO DE RETORNO AO MENU DCOM       *
      *----------------------------------------------------------------*
       1340-RETORNAR-MENU-DCOM         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-CHNG           TO WRK-FUNCAO.
           MOVE 'DCOM0410'         TO WRK-TELA
                                      COMU-TRANSACAO-UN.
           MOVE 'A'                TO COMU-PFK-UN.
           MOVE COMU-AREA-UN       TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1340-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA PASSA O CONTROLE PARA TRAN. DCOM0411              *
      *----------------------------------------------------------------*
       1351-CHAMAR-TRAN-DCOM0411       SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-CHNG                    TO WRK-FUNCAO.
           MOVE 'DCOM0411'                  TO WRK-TELA.
           MOVE 'DCOM0410'                  TO COMU-TRANSACAO-UN
           MOVE WRK-INP-SENHAS-DJTM         TO COMU-SENHAS-UN
           MOVE WRK-INP-TMSTAMP-DJTM        TO COMU-TIMESTAMP-UN
           MOVE WRK-INP-PAGINA-DJTM         TO COMU-PAGINA-UN
BRQ001     MOVE WRK-INP-OPCAO-DJTM          TO COMU-OPCAO-UN.

           IF WRK-INP-FLAGENT-DJTM     EQUAL '1'
              MOVE WRK-INP-CODAGE-DJTM      TO COMU-AGENC-UN
              MOVE WRK-INP-CONTA-DJTM       TO COMU-CONTA-UN
FS2511        MOVE SPACES                   TO COMU-CNPJ-UN
                                               COMU-FILIAL-UN
FS2511        MOVE ZEROS                    TO COMU-DIG-UN
           ELSE
              MOVE ZEROS                    TO COMU-AGENC-UN
                                               COMU-CONTA-UN
              MOVE WRK-INP-NUMERO-DJTM      TO COMU-CNPJ-UN
              MOVE WRK-INP-FILIAL-DJTM      TO COMU-FILIAL-UN
              MOVE WRK-INP-CONTROLE-DJTM    TO COMU-DIG-UN
           END-IF.
           PERFORM VARYING WRK-IND FROM 1 BY 1 UNTIL WRK-IND GREATER 4
               IF  WRK-INP-SELEC-DJTM(WRK-IND) EQUAL 'D'
                   MOVE WRK-INP-OPERA-DJTM(WRK-IND)
                                            TO COMU-OPERACAO-UN
               END-IF
           END-PERFORM.

           MOVE COMU-AREA-UN                TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1351-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRAMAMENTO DO COMANDO PFK7 - VOLTA PAG.  *
      *----------------------------------------------------------------*
       1360-TRATAR-VOLTA-PAG           SECTION.
      *----------------------------------------------------------------*

           IF WRK-INP-PAGINA-DJTM  EQUAL  1
              MOVE 'NAO EXISTE PAGINA PARA VOLTAR'
                                       TO WRK-OUT-MENSA-DJTM
              PERFORM VARYING WRK-IND FROM 1 BY 1
                        UNTIL WRK-IND GREATER 4
                  IF  WRK-INP-OPERA-DJTM-X(WRK-IND)
                                       EQUAL SPACES OR LOW-VALUES
                      MOVE WRK-225 TO WRK-OUT-SELEC-ATTR-DJTM(WRK-IND)
                  END-IF
              END-PERFORM
              GO TO 1360-99-FIM
           END-IF.

           IF  WRK-INP-MENSA-DJTM      NOT EQUAL WRK-MENSA-PF7
               IF  WRK-INP-SELEC-DJTM(1) EQUAL 'D'
               OR  WRK-INP-SELEC-DJTM(2) EQUAL 'D'
               OR  WRK-INP-SELEC-DJTM(3) EQUAL 'D'
               OR  WRK-INP-SELEC-DJTM(4) EQUAL 'D'
                   MOVE WRK-MENSA-PF7  TO WRK-OUT-MENSA-DJTM
                   GO TO 1360-99-FIM
               END-IF
           END-IF.

           INITIALIZE WRK-4711E-ENTRADA
                      WRK-4711S-SAIDA
                      WRK-4712E-ENTRADA
                      WRK-4712S-SAIDA.

           MOVE SPACES                 TO WRK-OUT-FIMAMOS-DJTM
                                          WRK-OUT-SELEC-DJTM(1)
                                          WRK-OUT-SELEC-DJTM(2)
                                          WRK-OUT-SELEC-DJTM(3)
                                          WRK-OUT-SELEC-DJTM(4).

           IF WRK-INP-FLAGENT-DJTM  EQUAL '1'
              MOVE WRK-INP-CODAGE-DJTM TO  WRK-4711E-CAG-BCRIA
              MOVE WRK-INP-CONTA-DJTM  TO  WRK-4711E-CCTA-BCRIA-CLI
              COMPUTE WRK-4711E-RESTART = (WRK-INP-PAGINA-DJTM - 2) * 4
              PERFORM 1500-ACESSAR-DCOM4711
           ELSE
              MOVE WRK-INP-NUMERO-DJTM   TO WRK-4712E-CCNPJ-CPF
              MOVE WRK-INP-FILIAL-DJTM   TO WRK-4712E-CFLIAL-CNPJ
              MOVE WRK-INP-CONTROLE-DJTM TO WRK-4712E-CCTRL-CNPJ-CPF
              COMPUTE WRK-4712E-RESTART = (WRK-INP-PAGINA-DJTM - 2) * 4
              PERFORM 1600-ACESSAR-DCOM4712
           END-IF.

           COMPUTE WRK-OUT-PAGINA-DJTM = WRK-INP-PAGINA-DJTM - 1.

           MOVE 'AMOSTRAGEM CONTINUA'  TO WRK-OUT-MENSA-DJTM.

      *----------------------------------------------------------------*
       1360-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRAMAMENTO DO COMANDO PFK8 - AVANCA PAG. *
      *----------------------------------------------------------------*
       1370-TRATAR-AVANCA-PAG          SECTION.
      *----------------------------------------------------------------*

           IF WRK-INP-FIMAMOS-DJTM  EQUAL  '*'
              MOVE 'NAO EXISTE PAGINA PARA AVANCAR'
                                       TO WRK-OUT-MENSA-DJTM
              PERFORM VARYING WRK-IND FROM 1 BY 1
                        UNTIL WRK-IND GREATER 4
                  IF  WRK-INP-OPERA-DJTM-X(WRK-IND)
                                       EQUAL SPACES OR LOW-VALUES
                      MOVE WRK-225 TO WRK-OUT-SELEC-ATTR-DJTM(WRK-IND)
                  END-IF
              END-PERFORM
              GO TO 1370-99-FIM
           END-IF.

           IF  WRK-INP-MENSA-DJTM      NOT EQUAL WRK-MENSA-PF8
               IF  WRK-INP-SELEC-DJTM(1) EQUAL 'D'
               OR  WRK-INP-SELEC-DJTM(2) EQUAL 'D'
               OR  WRK-INP-SELEC-DJTM(3) EQUAL 'D'
               OR  WRK-INP-SELEC-DJTM(4) EQUAL 'D'
                   MOVE WRK-MENSA-PF8  TO WRK-OUT-MENSA-DJTM
                   GO TO 1370-99-FIM
               END-IF
           END-IF.

           INITIALIZE WRK-4711E-ENTRADA
                      WRK-4711S-SAIDA
                      WRK-4712E-ENTRADA
                      WRK-4712S-SAIDA.

           PERFORM VARYING WRK-IND FROM 1 BY 1
                     UNTIL WRK-IND GREATER 4
               INITIALIZE WRK-OUT-TABELA-DJTM(WRK-IND)
               MOVE SPACES             TO WRK-OUT-OPERA-DJTM-X(WRK-IND)
                                          WRK-OUT-VALOR-DJTM-X(WRK-IND)
           END-PERFORM.

           IF WRK-INP-FLAGENT-DJTM  EQUAL '1'
              MOVE WRK-INP-CODAGE-DJTM TO  WRK-4711E-CAG-BCRIA
              MOVE WRK-INP-CONTA-DJTM  TO  WRK-4711E-CCTA-BCRIA-CLI
              COMPUTE WRK-4711E-RESTART = (WRK-INP-PAGINA-DJTM) * 4
              PERFORM 1500-ACESSAR-DCOM4711
           ELSE
              MOVE WRK-INP-NUMERO-DJTM   TO WRK-4712E-CCNPJ-CPF
              MOVE WRK-INP-FILIAL-DJTM   TO WRK-4712E-CFLIAL-CNPJ
              MOVE WRK-INP-CONTROLE-DJTM TO WRK-4712E-CCTRL-CNPJ-CPF
              COMPUTE WRK-4712E-RESTART = (WRK-INP-PAGINA-DJTM) * 4
              PERFORM 1600-ACESSAR-DCOM4712
           END-IF.

           COMPUTE WRK-OUT-PAGINA-DJTM = WRK-INP-PAGINA-DJTM + 1.

           IF  WRK-OUT-FIMAMOS-DJTM      EQUAL SPACES
               MOVE 'AMOSTRAGEM CONTINUA'TO WRK-OUT-MENSA-DJTM
           ELSE
               MOVE 'FIM DE AMOSTRAGEM'  TO WRK-OUT-MENSA-DJTM
           END-IF.

      *----------------------------------------------------------------*
       1370-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRAMAMENTO DO COMANDO  NTER              *
      *----------------------------------------------------------------*
       1380-TRATAR-ENTER               SECTION.
      *----------------------------------------------------------------*

           IF (WRK-INP-SELEC-DJTM(1)   EQUAL SPACES OR LOW-VALUES)
           AND(WRK-INP-SELEC-DJTM(2)   EQUAL SPACES OR LOW-VALUES)
           AND(WRK-INP-SELEC-DJTM(3)   EQUAL SPACES OR LOW-VALUES)
           AND(WRK-INP-SELEC-DJTM(4)   EQUAL SPACES OR LOW-VALUES)
               MOVE 'NENHUMA OPCAO FOI SELECIONADA'
                                       TO WRK-OUT-MENSA-DJTM
               MOVE '*'                TO WRK-INCONSISTENCIA
               GO TO 1380-99-FIM
           END-IF.

           PERFORM VARYING WRK-IND FROM 1 BY 1 UNTIL WRK-IND GREATER 4
             IF  WRK-INP-SELEC-DJTM(WRK-IND) NOT EQUAL 'D' AND
                                             SPACES AND LOW-VALUES
                 MOVE '*'        TO WRK-INCONSISTENCIA
                 MOVE WRK-49353  TO WRK-OUT-SELEC-ATTR-DJTM(WRK-IND)
                 MOVE 'OPCAO INVALIDA - SELECIONAR APENAS: "D"'
                                 TO  WRK-OUT-MENSA-DJTM
             END-IF
           END-PERFORM.

           IF WRK-INCONSISTENCIA  EQUAL  '*'
              GO TO 1380-99-FIM
           END-IF.

           MOVE    ZEROS         TO WRK-QTD-OPCAO-D

           PERFORM VARYING WRK-IND FROM 1 BY 1 UNTIL WRK-IND GREATER 4
             IF  WRK-INP-SELEC-DJTM(WRK-IND) EQUAL 'D'
                 ADD  1          TO WRK-QTD-OPCAO-D
             END-IF
           END-PERFORM.

           IF WRK-QTD-OPCAO-D EQUAL  ZEROS
              MOVE 'SELECIONE COM "D" PARA DETALHAR' TO
                                        WRK-OUT-MENSA-DJTM
           ELSE
              IF WRK-QTD-OPCAO-D  GREATER  1
                 PERFORM VARYING WRK-IND FROM 1 BY 1
                         UNTIL WRK-IND GREATER  4
                   IF WRK-INP-SELEC-DJTM(WRK-IND) EQUAL 'D'
                      MOVE WRK-49353  TO
                                 WRK-OUT-SELEC-ATTR-DJTM(WRK-IND)
                   END-IF
                 END-PERFORM
                 MOVE 'SELECIONAR APENAS UMA OCORRENCIA PARA OPCAO "D"'
                                            TO  WRK-OUT-MENSA-DJTM
              ELSE
                 PERFORM 1351-CHAMAR-TRAN-DCOM0411
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       1380-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA CARREGA A TELA QUE SERA DEMONSTRADA PELO IMS.     *
      *----------------------------------------------------------------*
       1400-PROCESSAR-BRAD0660         SECTION.
      *----------------------------------------------------------------*

BRQ001     MOVE LENGTH                 OF WRK-OUTPUT-DJTM
BRQ001                                 TO WRK-660-LL-MENSAGEM-DJTM
BRQ001                                    WRK-OUT-LL-DJTM
BRQ001     MOVE LENGTH                 OF WRK-660-DCOMDJTM
BRQ001                                 TO WRK-660-LL-AREA-DJTM

           CALL 'BRAD0660'             USING WRK-OUTPUT-DJTM
                                             WRK-660-DCOMDJTM.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE RETURN-CODE        TO WRK-RETURN-CODE
               MOVE 10                 TO WRK-LOCAL-ERRO
               MOVE WRK-ERRO-BRAD0660  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-ISRT               TO WRK-FUNCAO.
           MOVE WRK-OUTPUT-DJTM        TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA CHAMA O MODULO DCOM4711                           *
      *----------------------------------------------------------------*
       1500-ACESSAR-DCOM4711           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-COD-USER           TO  WRK-4711E-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO  WRK-4711E-CTERM
           MOVE 'N'                    TO  WRK-4711E-START-COUNT
           MOVE SPACES                 TO  WRK-4711E-FLAG

BRQ001     IF  WRK-INP-OPCAO-DJTM      EQUAL 3
               MOVE 'CL'               TO  WRK-4711E-FLAG-FILTRO
BRQ001     ELSE
BRQ001         IF  WRK-INP-OPCAO-DJTM  EQUAL 4
BRQ001             MOVE 'CM'           TO  WRK-4711E-FLAG-FILTRO
BRQ001         END-IF
BRQ001     END-IF.

           MOVE ZEROS                  TO  WRK-4711E-CAG-OPER
           MOVE 237                    TO  WRK-4711E-CBCO
           MOVE 'DCOM4711'             TO  WRK-MODULO

           CALL WRK-MODULO    USING    WRK-4711E-ENTRADA
                                       WRK-4711S-SAIDA
                                       ERRO-AREA
                                       WRK-SQLCA.

           EVALUATE WRK-4711S-COD-RETORNO
               WHEN '0000'
                    IF WRK-4711S-DANO-OPER-DESC(6) EQUAL ZEROS
                       MOVE '*'     TO  WRK-OUT-FIMAMOS-DJTM
                    ELSE
                       MOVE SPACES  TO  WRK-OUT-FIMAMOS-DJTM
                    END-IF
                    MOVE 'SELECIONE COM "D" PARA DETALHAR'
                                    TO  WRK-OUT-MENSA-DJTM

                    PERFORM 1510-MONTAR-TELA VARYING WRK-IND FROM
                            1 BY 1 UNTIL WRK-IND GREATER 4

               WHEN '0003'
                    GO TO 1500-99-FIM

               WHEN '0099'
                    MOVE  'DB2'     TO     ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                    MOVE  'APL'     TO     ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-EVALUATE.

      *----------------------------------------------------------------*
       1500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA CARREGA OS CAMPOS DA TELA A PARTIR DO DCOM4711    *
      *----------------------------------------------------------------*
       1510-MONTAR-TELA                SECTION.
      *----------------------------------------------------------------*

           IF WRK-4711S-DANO-OPER-DESC(WRK-IND) NUMERIC
           AND WRK-4711S-DANO-OPER-DESC(WRK-IND) NOT EQUAL ZEROS
              MOVE WRK-4711S-DANO-OPER-DESC(WRK-IND)
                                  TO WRK-OUT-OPERA-DJTM(WRK-IND)(1:4)
              MOVE WRK-4711S-NSEQ-OPER-DESC(WRK-IND)
                                  TO WRK-OUT-OPERA-DJTM(WRK-IND)(5:9)
              MOVE WRK-4711S-IABREV-PRODT(WRK-IND)
                                  TO WRK-OUT-PROD-DJTM(WRK-IND)
              MOVE WRK-4711S-IRSUMO-SPROD-DESC(WRK-IND)
                                  TO WRK-OUT-SUBPROD-DJTM(WRK-IND)

              STRING WRK-4711S-DINIC-OPER-DESC(WRK-IND)(1:2)
              '/'  WRK-4711S-DINIC-OPER-DESC  (WRK-IND)(4:2)
              '/'  WRK-4711S-DINIC-OPER-DESC  (WRK-IND)(7:4)
              DELIMITED BY SIZE  INTO WRK-OUT-DTOPER-DJTM(WRK-IND)
              MOVE WRK-4711S-VOPER-DESC-COML(WRK-IND)
                                  TO WRK-OUT-VALOR-DJTM(WRK-IND)
              MOVE WRK-4711S-IRSUMO-MEIO-ENTRD(WRK-IND)
                                  TO WRK-OUT-MEIOENT-DJTM(WRK-IND)
              MOVE WRK-4711S-IRSUMO-SIT-DESC(WRK-IND)
                                  TO WRK-OUT-SITANAL-DJTM(WRK-IND)
              IF  WRK-4711S-PARECER-FORML(WRK-IND) EQUAL 'S'
                  MOVE 'SIM'      TO WRK-OUT-PARECER-DJTM(WRK-IND)
              ELSE
                  MOVE 'NAO'      TO WRK-OUT-PARECER-DJTM(WRK-IND)
              END-IF
           ELSE
              MOVE WRK-225        TO WRK-OUT-SELEC-ATTR-DJTM(WRK-IND)
           END-IF.

      *----------------------------------------------------------------*
       1510-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA CHAMA O MODULO DCOM4712                           *
      *----------------------------------------------------------------*
       1600-ACESSAR-DCOM4712           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-COD-USER           TO  WRK-4712E-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO  WRK-4712E-CTERM
           MOVE 'N'                    TO  WRK-4712E-START-COUNT
           MOVE SPACES                 TO  WRK-4712E-FLAG

BRQ001     IF  WRK-INP-OPCAO-DJTM      EQUAL 3
               MOVE 'CL'               TO  WRK-4712E-FLAG-FILTRO
BRQ001     ELSE
BRQ001         IF  WRK-INP-OPCAO-DJTM  EQUAL 4
BRQ001             MOVE 'CM'           TO  WRK-4712E-FLAG-FILTRO
BRQ001         END-IF
BRQ001     END-IF.

           MOVE 237                    TO  WRK-4712E-CBCO
           MOVE WRK-COD-DEPTO-N        TO  WRK-AGENCIA-NUM
           MOVE WRK-AGENCIA-AUX        TO  WRK-4712E-MSG-RETORNO(1:5)
           MOVE 'DCOM4712'             TO  WRK-MODULO

           CALL WRK-MODULO    USING    WRK-4712E-ENTRADA
                                       WRK-4712S-SAIDA
                                       ERRO-AREA
                                       WRK-SQLCA.

           EVALUATE WRK-4712S-COD-RETORNO
               WHEN '0000'
                    IF WRK-4712S-DANO-OPER-DESC(6) EQUAL ZEROS
                       MOVE '*'     TO  WRK-OUT-FIMAMOS-DJTM
                    ELSE
                       MOVE SPACES  TO  WRK-OUT-FIMAMOS-DJTM
                    END-IF
                    MOVE 'SELECIONE COM "D" PARA DETALHAR'
                                    TO  WRK-OUT-MENSA-DJTM

                    PERFORM 1610-MONTAR-TELA VARYING WRK-IND FROM
                            1 BY 1 UNTIL WRK-IND GREATER 4

               WHEN '0003'
               WHEN '0915'
                    GO TO 1600-99-FIM

               WHEN '0099'
                    MOVE  'DB2'     TO     ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                    MOVE 'S'                    TO WRK-FLAG-ERRO
                    MOVE WRK-4712S-MSG-RETORNO  TO WRK-MENSAGEM-ERRO
                    PERFORM 1320-RETORNAR-CHAMADOR
           END-EVALUATE.

      *----------------------------------------------------------------*
       1600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA CARREGA OS CAMPOS DA TELA A PARTIR DO DCOM4712    *
      *----------------------------------------------------------------*
       1610-MONTAR-TELA                SECTION.
      *----------------------------------------------------------------*

           IF WRK-4712S-DANO-OPER-DESC(WRK-IND) NUMERIC
           AND WRK-4712S-DANO-OPER-DESC(WRK-IND) NOT EQUAL ZEROS
              MOVE WRK-4712S-DANO-OPER-DESC(WRK-IND)
                                  TO WRK-OUT-OPERA-DJTM(WRK-IND)(1:4)
              MOVE WRK-4712S-NSEQ-OPER-DESC(WRK-IND)
                                  TO WRK-OUT-OPERA-DJTM(WRK-IND)(5:9)
              MOVE WRK-4712S-IABREV-PRODT(WRK-IND)
                                  TO WRK-OUT-PROD-DJTM(WRK-IND)
              MOVE WRK-4712S-IRSUMO-SPROD-DESC(WRK-IND)
                                  TO WRK-OUT-SUBPROD-DJTM(WRK-IND)
              STRING WRK-4712S-DINIC-OPER-DESC(WRK-IND)(1:2)
              '/'    WRK-4712S-DINIC-OPER-DESC(WRK-IND)(4:2)
              '/'    WRK-4712S-DINIC-OPER-DESC(WRK-IND)(7:4)
              DELIMITED BY SIZE  INTO WRK-OUT-DTOPER-DJTM(WRK-IND)
              MOVE WRK-4712S-VOPER-DESC-COML(WRK-IND)
                                  TO WRK-OUT-VALOR-DJTM(WRK-IND)
              MOVE WRK-4712S-IRSUMO-MEIO-ENTRD(WRK-IND)
                                  TO WRK-OUT-MEIOENT-DJTM(WRK-IND)
              MOVE WRK-4712S-IRSUMO-SIT-DESC(WRK-IND)
                                  TO WRK-OUT-SITANAL-DJTM(WRK-IND)
              IF  WRK-4712S-PARECER-FORML(WRK-IND) EQUAL 'S'
                  MOVE 'SIM'      TO WRK-OUT-PARECER-DJTM(WRK-IND)
              ELSE
                  MOVE 'NAO'      TO WRK-OUT-PARECER-DJTM(WRK-IND)
              END-IF
           ELSE
              MOVE WRK-225        TO WRK-OUT-SELEC-ATTR-DJTM(WRK-IND)
           END-IF.

      *----------------------------------------------------------------*
       1610-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA FINALIZA O PROCESSAMENTO DO PROGRAMA              *
      *----------------------------------------------------------------*
       2000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA FORMATA E EMITE A MENSAGEM DE ERRO DO PROGRAMA    *
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           IF  ERR-MODULO              EQUAL SPACES AND LOW-VALUES
               MOVE 'SENHAS02'         TO ERR-PGM
               MOVE 'DCOM0410'         TO ERR-MODULO
           ELSE
               MOVE 'DCOM0410'         TO ERR-PGM
           END-IF.


           MOVE WRK-COD-USER           TO ERR-COD-USER.
           MOVE WRK-COD-DEPTO          TO ERR-COD-DEPTO.


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


           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
