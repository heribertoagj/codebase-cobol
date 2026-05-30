      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID.     DCOM3924.
       AUTHOR.         ODIRLEI COSTA.
      *================================================================*
      *                          A L T R A N                           *
      *----------------------------------------------------------------*
      *     PROGRAMA....: DCOM3924                                     *
      *     PROGRAMADOR.: ODIRLEI COSTA    - SONDA PROCWORK            *
      *     ANALISTA....: ODIRLEI COSTA    - SONDA PROCWORK            *
      *     DATA........: 17/03/2011                                   *
      *----------------------------------------------------------------*
      *     OBJETIVO....: CONSULTA CET.                                *
      *----------------------------------------------------------------*
      *     TELAS.......:                                              *
      *     DCOM9YTM....: CONSULTA CET.                                *
      *----------------------------------------------------------------*
      *     INC'S.......:                                              *
      *     I#BRAD7C - AREA DE ERRO DA BRAD7100                        *
      *----------------------------------------------------------------*
      *     MODULOS.....:                                              *
      *     BRAD0660 - INSERIR CARACTERES DE NULL NA MENSAGEM DE SAIDA.*
      *     BRAD7100 - TRATAMENTO DE ERROS QUANDO PROGRAMA INVALIDO.   *
      *     DCOM4174 - CONSULTA TODOS OS DADOS DE UMA OPERACAO.        *
      *----------------------------------------------------------------*
      *     NAVEGACAO...:                                              *
      *     CHAMADO POR.:                    CHAMA:    -               *
      *================================================================*
      *             ALTERACAO                                          *
      *----------------------------------------------------------------*
      *     ANALISTA....: JOSE ROBERTO A FERRERIA   - BRADESCO         *
      *     DATA........: 30/05/2018                                   *
      *     OBJETIVO....: TRATAMENTO PARA RETORNO LIB OPERACOES ORPAG  *
      *----------------------------------------------------------------*
WIP001*                   A L T E R A C A O                            *
WIP001*----------------------------------------------------------------*
WIP001*     ANALISTA....: FERNANDO LUIZ DE SANTI                       *
WIP001*     ANALISTA DS.: PAULO ROBERTO        - GRUPO 39              *
WIP001*     DATA........: 06/10/2020                                   *
WIP001*----------------------------------------------------------------*
WIP001*     OBJETIVO....: INCLUIR TRATAMENTO PFK12.                    *
WIP001*================================================================*

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
       77  FILLER                      PIC  X(050)         VALUE
           '*** DCOM3924 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*

       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-BUSCA-MOVIMENTO         PIC  X(001)         VALUE SPACES.
       77  WRK-MSG-AUX                 PIC  X(079)         VALUE SPACES.

       01  WRK-BCOAGECTA-AUX.
           05  WRK-BANCO-AUX           PIC  9(03)          VALUE ZEROS.
           05  FILLER                  PIC  X(01)          VALUE '/'.
           05  WRK-AGENC-AUX           PIC  9(05)          VALUE ZEROS.
           05  FILLER                  PIC  X(01)          VALUE '/'.
           05  WRK-CONTA-AUX           PIC  9(13)          VALUE ZEROS.

VANS   01  WRK-PERC-AUX.
VANS       05  WRK-PERC-AUX-N          PIC  ZZ9,99         VALUE ZEROS.
VANS       05  WRK-PERC-AUX-SIMB       PIC  X(01)          VALUE '%'.
VANS
       01  WRK-COD-CET.
           05  WRK-COD-CET-N           PIC  ZZZZ9,99       VALUE ZEROS.

       01  WRK-VALOR-TELA.
VANS       05  WRK-VALOR-TELA-N        PIC  ZZZ.ZZZ.ZZ9,99 VALUE ZEROS.

VANS   01  WRK-PERC                PIC  9(015)V9(05) COMP-3 VALUE ZEROS.
VANS   01  WRK-PERC-R              PIC  9(003)V9(02) COMP-3 VALUE ZEROS.
VANS
VANS   01  WRK-VALOR-LIQUI            PIC  9(015)V99 COMP-3 VALUE ZEROS.
VANS   01  WRK-VALOR-LIBER            PIC  9(015)V99 COMP-3 VALUE ZEROS.
       01  WRK-VALOR-IOF              PIC  9(015)V99 COMP-3 VALUE ZEROS.
       01  WRK-VALOR-TAC-TITULOS      PIC  9(015)V99 COMP-3 VALUE ZEROS.
       01  WRK-VALOR-TAC-OPERACAO     PIC  9(015)V99 COMP-3 VALUE ZEROS.
       01  WRK-VALOR-TOT-TARIFAS      PIC  9(015)V99 COMP-3 VALUE ZEROS.
       01  WRK-VALOR-TOTAL            PIC  9(015)V99 COMP-3 VALUE ZEROS.

VANS   01  WRK-VALOR-DESCOMP          PIC  9(015)V99        VALUE ZEROS.
VANS   01  FILLER                     REDEFINES WRK-VALOR-DESCOMP.
VANS       05  FILLER                 PIC  9(006).
VANS       05  WRK-VALOR-DESCOMP-R    PIC  9(009)V99.
VANS
VANS   01  WRK-COD-CET-DIMINUI        PIC  9(008)V9(007)    VALUE ZEROS.
VANS   01  FILLER                     REDEFINES WRK-COD-CET-DIMINUI.
VANS       05  FILLER                 PIC  9(003).
VANS       05  WRK-COD-CET-DIMINUI-R  PIC  9(005)V99.
VANS       05  FILLER                 PIC  9(005).
VANS
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA MENSAGENS ***'.
      *----------------------------------------------------------------*

       77  WRK-MSG001                  PIC  X(079)         VALUE
           'CONSULTA EFETUADA COM SUCESSO. TECLE <PF3> PARA RETORNAR'.
       77  WRK-MSG002                  PIC  X(079)         VALUE
           'PF INVALIDA'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREAS AUXILIARES PARA POOL5005 ***'.
      *----------------------------------------------------------------*

       01  WRK-POOL5005.
           05  WRK-SQLCA               PIC  X(136)         VALUE SPACES.
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
           05  WRK-COD-DEPTO           PIC  X(006)         VALUE SPACES.
           05  WRK-COD-DEPTO-R         REDEFINES
               WRK-COD-DEPTO           PIC  9(006).

           05  WRK-MENSAGEM.
               10  FILLER              PIC  X(004)         VALUE SPACES.
               10  WRK-TRANSACAO       PIC  X(008)         VALUE SPACES.
               10  FILLER              PIC  X(1988)        VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA TELA DCOM9YTM - INPUT ***'.
      *----------------------------------------------------------------*

       01  WRK-INPUT-9YTM.
           05 FILLER                       PIC  X(018)     VALUE SPACES.
           05 WRK-INP-PFK-9YTM             PIC  X(001)     VALUE SPACES.
           05 WRK-INP-SENHAS-9YTM          PIC  X(037)     VALUE SPACES.
           05 WRK-INP-COMANDO-9YTM         PIC  X(068)     VALUE SPACES.
           05 WRK-INP-DADOS-9YTM.
              10 WRK-INP-OCULTOS-9YTM.
                 15 WRK-INP-OCULTO1-9YTM   PIC  X(079)     VALUE SPACES.
VANS             15 WRK-INP-OCULTO2-9YTM   PIC  X(012)     VALUE SPACES.
                 15 WRK-INP-OCULTO3-9YTM   PIC  X(079)     VALUE SPACES.
VANS             15 WRK-INP-OCULTO4-9YTM   PIC  X(041)     VALUE SPACES.
VANS             15 WRK-INP-OCULTO5-9YTM   PIC  X(006)     VALUE SPACES.
VANS             15 WRK-INP-OCULTO6-9YTM   PIC  X(011)     VALUE SPACES.
VANS             15 WRK-INP-OCULTO7-9YTM   PIC  X(010)     VALUE SPACES.
VANS             15 WRK-INP-OCULTO8-9YTM   PIC  X(018)     VALUE SPACES.
VANS             15 WRK-INP-OCULTO9-9YTM   PIC  X(011)     VALUE SPACES.
VANS             15 WRK-INP-OCULT10-9YTM   PIC  X(009)     VALUE SPACES.
VANS             15 WRK-INP-OCULT11-9YTM   PIC  X(007)     VALUE SPACES.
VANS             15 WRK-INP-OCULT12-9YTM   PIC  X(009)     VALUE SPACES.
VANS             15 WRK-INP-OCULT13-9YTM   PIC  X(002)     VALUE SPACES.
VANS             15 WRK-INP-OCULT14-9YTM   PIC  X(010)     VALUE SPACES.
VANS             15 WRK-INP-OCULT15-9YTM   PIC  X(009)     VALUE SPACES.
VANS             15 WRK-INP-OCULT16-9YTM   PIC  X(001)     VALUE SPACES.
VANS             15 WRK-INP-OCULT17-9YTM   PIC  X(001)     VALUE SPACES.
VANS             15 WRK-INP-OCULT18-9YTM   PIC  X(001)     VALUE SPACES.
              10 WRK-INP-NOMECL-9YTM       PIC  X(040)     VALUE SPACES.
              10 WRK-INP-CNPJCPF-9YTM      PIC  X(019)     VALUE SPACES.
              10 WRK-INP-BCAGCT-9YTM       PIC  X(023)     VALUE SPACES.
              10 WRK-INP-CODPRD-9YTM       PIC  9(003)     VALUE ZEROS.
              10 WRK-INP-DESPRD-9YTM       PIC  X(012)     VALUE SPACES.
              10 WRK-INP-CODSUB-9YTM       PIC  9(003)     VALUE ZEROS.
              10 WRK-INP-DESSUB-9YTM       PIC  X(012)     VALUE SPACES.
              10 WRK-INP-OPERAC-9YTM       PIC  9(013)     VALUE ZEROS.
              10 FILLER REDEFINES          WRK-INP-OPERAC-9YTM.
                 15 WRK-INP-DANO-9YTM      PIC  9(004).
                 15 WRK-INP-NSEQ-9YTM      PIC  9(009).
              10 WRK-INP-SITUAC-9YTM       PIC  X(015)     VALUE SPACES.
              10 WRK-INP-DESIDE-9YTM       PIC  X(013)     VALUE SPACES.
              10 WRK-INP-NUMERO-9YTM.
                 15 WRK-INP-NUMERO-9YTM-N  PIC  9(009)     VALUE ZEROS.
              10 WRK-INP-SIT3-9YTM         PIC  X(013)     VALUE SPACES.
              10 WRK-INP-SIT4-9YTM.
                15 WRK-INP-SIT4-9YTM-N     PIC  9(009)     VALUE ZEROS.
VANS          10 WRK-INP-VLRLIQU-9YTM      PIC  X(014)     VALUE SPACES.
VANS          10 WRK-INP-VLRLIBE-9YTM      PIC  X(014)     VALUE SPACES.
VANS          10 WRK-INP-PERLIBE-9YTM      PIC  X(007)     VALUE SPACES.
VANS          10 WRK-INP-TRIBUTOS-9YTM     PIC  X(014)     VALUE SPACES.
VANS          10 WRK-INP-PERTRIB-9YTM      PIC  X(007)     VALUE SPACES.
VANS          10 WRK-INP-SEGUROS-9YTM      PIC  X(014)     VALUE SPACES.
VANS          10 WRK-INP-PERSEGU-9YTM      PIC  X(007)     VALUE SPACES.
VANS          10 WRK-INP-TARIFAS-9YTM      PIC  X(014)     VALUE SPACES.
VANS          10 WRK-INP-PERTARI-9YTM      PIC  X(007)     VALUE SPACES.
VANS          10 WRK-INP-PGTOSERV-9YTM     PIC  X(014)     VALUE SPACES.
VANS          10 WRK-INP-PERPGTO-9YTM      PIC  X(007)     VALUE SPACES.
VANS          10 WRK-INP-REGISTROS-9YTM    PIC  X(014)     VALUE SPACES.
VANS          10 WRK-INP-PERREGI-9YTM      PIC  X(007)     VALUE SPACES.
VANS          10 WRK-INP-TOTAL-9YTM        PIC  X(014)     VALUE SPACES.
VANS          10 WRK-INP-PERTOTA-9YTM      PIC  X(007)     VALUE SPACES.
              10 WRK-INP-CETAM-9YTM        PIC  X(008)     VALUE SPACES.
              10 WRK-INP-CETAA-9YTM        PIC  X(008)     VALUE SPACES.
              10 WRK-INP-CHAMADOR-9YTM     PIC  X(008)     VALUE SPACES.
           05 WRK-INP-MENSA-9YTM           PIC  X(079)     VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA TELA DCOM9YTM  - OUTPUT ***'.
      *----------------------------------------------------------------*

       01  WRK-OUTPUT-9YTM.
           05 WRK-OUT-LL-9YTM             PIC S9(004) COMP VALUE +0000.
           05 WRK-OUT-ZZ-9YTM             PIC S9(004) COMP VALUE ZEROS.
           05 WRK-OUT-SENHAS-9YTM          PIC  X(037)     VALUE SPACES.
           05 WRK-OUT-COMANDO-9YTM         PIC  X(068)     VALUE SPACES.
           05 WRK-OUT-DADOS-9YTM.
              10 WRK-OUT-OCULTOS-9YTM.
                 15 WRK-OUT-OCULTO1-9YTM   PIC  X(079)     VALUE SPACES.
VANS             15 WRK-OUT-OCULTO2-9YTM   PIC  X(012)     VALUE SPACES.
                 15 WRK-OUT-OCULTO3-9YTM   PIC  X(079)     VALUE SPACES.
VANS             15 WRK-OUT-OCULTO4-9YTM   PIC  X(041)     VALUE SPACES.
VANS             15 WRK-OUT-OCULTO5-9YTM   PIC  X(006)     VALUE SPACES.
VANS             15 WRK-OUT-OCULTO6-9YTM   PIC  X(011)     VALUE SPACES.
VANS             15 WRK-OUT-OCULTO7-9YTM   PIC  X(010)     VALUE SPACES.
VANS             15 WRK-OUT-OCULTO8-9YTM   PIC  X(018)     VALUE SPACES.
VANS             15 WRK-OUT-OCULTO9-9YTM   PIC  X(011)     VALUE SPACES.
VANS             15 WRK-OUT-OCULT10-9YTM   PIC  X(009)     VALUE SPACES.
VANS             15 WRK-OUT-OCULT11-9YTM   PIC  X(007)     VALUE SPACES.
VANS             15 WRK-OUT-OCULT12-9YTM   PIC  X(009)     VALUE SPACES.
VANS             15 WRK-OUT-OCULT13-9YTM   PIC  X(002)     VALUE SPACES.
VANS             15 WRK-OUT-OCULT14-9YTM   PIC  X(010)     VALUE SPACES.
VANS             15 WRK-OUT-OCULT15-9YTM   PIC  X(009)     VALUE SPACES.
VANS             15 WRK-OUT-OCULT16-9YTM   PIC  X(001)     VALUE SPACES.
VANS             15 WRK-OUT-OCULT17-9YTM   PIC  X(001)     VALUE SPACES.
VANS             15 WRK-OUT-OCULT18-9YTM   PIC  X(001)     VALUE SPACES.
              10 WRK-OUT-NOMECL-9YTM       PIC  X(040)     VALUE SPACES.
              10 WRK-OUT-CNPJCPF-9YTM      PIC  X(019)     VALUE SPACES.
              10 WRK-OUT-BCAGCT-9YTM       PIC  X(023)     VALUE SPACES.
              10 WRK-OUT-CODPRD-9YTM       PIC  9(003)     VALUE ZEROS.
              10 WRK-OUT-DESPRD-9YTM       PIC  X(012)     VALUE SPACES.
              10 WRK-OUT-CODSUB-9YTM       PIC  9(003)     VALUE ZEROS.
              10 WRK-OUT-DESSUB-9YTM       PIC  X(012)     VALUE SPACES.
              10 WRK-OUT-OPERAC-9YTM       PIC  9(013)     VALUE ZEROS.
              10 FILLER REDEFINES          WRK-OUT-OPERAC-9YTM.
                 15 WRK-OUT-DANO-9YTM      PIC  9(004).
                 15 WRK-OUT-NSEQ-9YTM      PIC  9(009).
              10 WRK-OUT-SITUAC-9YTM       PIC  X(015)     VALUE SPACES.
              10 WRK-OUT-DESIDE-9YTM       PIC  X(013)     VALUE SPACES.
              10 WRK-OUT-NUMERO-9YTM.
                 15 WRK-OUT-NUMERO-9YTM-N  PIC  9(009)     VALUE ZEROS.
              10 WRK-OUT-SIT3-9YTM         PIC  X(013)     VALUE SPACES.
              10 WRK-OUT-SIT4-9YTM.
                15 WRK-OUT-SIT4-9YTM-N     PIC  9(009)     VALUE ZEROS.
VANS          10 WRK-OUT-VLRLIQU-9YTM      PIC  X(014)     VALUE SPACES.
VANS          10 WRK-OUT-VLRLIBE-9YTM      PIC  X(014)     VALUE SPACES.
VANS          10 WRK-OUT-PERLIBE-9YTM      PIC  X(007)     VALUE SPACES.
VANS          10 WRK-OUT-TRIBUTOS-9YTM     PIC  X(014)     VALUE SPACES.
VANS          10 WRK-OUT-PERTRIB-9YTM      PIC  X(007)     VALUE SPACES.
VANS          10 WRK-OUT-SEGUROS-9YTM      PIC  X(014)     VALUE SPACES.
VANS          10 WRK-OUT-PERSEGU-9YTM      PIC  X(007)     VALUE SPACES.
VANS          10 WRK-OUT-TARIFAS-9YTM      PIC  X(014)     VALUE SPACES.
VANS          10 WRK-OUT-PERTARI-9YTM      PIC  X(007)     VALUE SPACES.
VANS          10 WRK-OUT-PGTOSERV-9YTM     PIC  X(014)     VALUE SPACES.
VANS          10 WRK-OUT-PERPGTO-9YTM      PIC  X(007)     VALUE SPACES.
VANS          10 WRK-OUT-REGISTROS-9YTM    PIC  X(014)     VALUE SPACES.
VANS          10 WRK-OUT-PERREGI-9YTM      PIC  X(007)     VALUE SPACES.
VANS          10 WRK-OUT-TOTAL-9YTM        PIC  X(014)     VALUE SPACES.
VANS          10 WRK-OUT-PERTOTA-9YTM      PIC  X(007)     VALUE SPACES.
              10 WRK-OUT-CETAM-9YTM        PIC  X(008)     VALUE SPACES.
              10 WRK-OUT-CETAA-9YTM        PIC  X(008)     VALUE SPACES.
              10 WRK-OUT-CHAMADOR-9YTM     PIC  X(008)     VALUE SPACES.
           05 WRK-OUT-MENSA-9YTM           PIC  X(079)     VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA TELA DCOM9YTM - BRAD0660 ***'.
      *----------------------------------------------------------------*

       01  WRK-660-DCOM9YTM.
           05  WRK-660-LL-AREA-9YTM    PIC  9(004) COMP    VALUE 0000.
           05  WRK-660-LL-MENSAG-9YTM  PIC  9(004) COMP    VALUE 0000.
           05  WRK-660-SENHAS-9YTM     PIC  9(004) COMP    VALUE 0037.
           05  WRK-660-COMANDO-9YTM    PIC  9(004) COMP    VALUE 0068.
           05  WRK-660-OCULTOS1-9YTM   PIC  9(004) COMP    VALUE 0079.
VANS       05  WRK-660-OCULTOS2-9YTM   PIC  9(004) COMP    VALUE 0012.
           05  WRK-660-OCULTOS3-9YTM   PIC  9(004) COMP    VALUE 0079.
VANS       05  WRK-660-OCULTOS4-9YTM   PIC  9(004) COMP    VALUE 0041.
VANS       05  WRK-660-OCULTOS5-9YTM   PIC  9(004) COMP    VALUE 0006.
VANS       05  WRK-660-OCULTOS6-9YTM   PIC  9(004) COMP    VALUE 0011.
VANS       05  WRK-660-OCULTOS7-9YTM   PIC  9(004) COMP    VALUE 0010.
VANS       05  WRK-660-OCULTOS8-9YTM   PIC  9(004) COMP    VALUE 0018.
VANS       05  WRK-660-OCULTOS9-9YTM   PIC  9(004) COMP    VALUE 0011.
VANS       05  WRK-660-OCULTO10-9YTM   PIC  9(004) COMP    VALUE 0009.
VANS       05  WRK-660-OCULTO11-9YTM   PIC  9(004) COMP    VALUE 0007.
VANS       05  WRK-660-OCULTO12-9YTM   PIC  9(004) COMP    VALUE 0009.
VANS       05  WRK-660-OCULTO13-9YTM   PIC  9(004) COMP    VALUE 0002.
VANS       05  WRK-660-OCULTO14-9YTM   PIC  9(004) COMP    VALUE 0010.
VANS       05  WRK-660-OCULTO15-9YTM   PIC  9(004) COMP    VALUE 0009.
VANS       05  WRK-660-OCULTO16-9YTM   PIC  9(004) COMP    VALUE 0001.
VANS       05  WRK-660-OCULTO17-9YTM   PIC  9(004) COMP    VALUE 0001.
VANS       05  WRK-660-OCULTO18-9YTM   PIC  9(004) COMP    VALUE 0001.
           05  WRK-660-NOMECL-9YTM     PIC  9(004) COMP    VALUE 0040.
           05  WRK-660-CNPJCPF-9YTM    PIC  9(004) COMP    VALUE 0019.
           05  WRK-660-BCAGCT-9YTM     PIC  9(004) COMP    VALUE 0023.
           05  WRK-660-CODPRD-9YTM     PIC  9(004) COMP    VALUE 0003.
           05  WRK-660-DESPRD-9YTM     PIC  9(004) COMP    VALUE 0012.
           05  WRK-660-CODSUB-9YTM     PIC  9(004) COMP    VALUE 0003.
           05  WRK-660-DESSUB-9YTM     PIC  9(004) COMP    VALUE 0012.
           05  WRK-660-OPERAC-9YTM     PIC  9(004) COMP    VALUE 0013.
           05  WRK-660-SITUAC-9YTM     PIC  9(004) COMP    VALUE 0015.
           05  WRK-660-DESIDE-9YTM     PIC  9(004) COMP    VALUE 0013.
           05  WRK-660-NUMERO-9YTM     PIC  9(004) COMP    VALUE 0009.
           05  WRK-660-SIT3-9YMT       PIC  9(004) COMP    VALUE 0013.
           05  WRK-660-SIT4-9YTM       PIC  9(004) COMP    VALUE 0009.
VANS       05  WRK-660-VLRLIQU-9YTM    PIC  9(004) COMP    VALUE 0014.
VANS       05  WRK-660-VLRLIBE-9YTM    PIC  9(004) COMP    VALUE 0014.
VANS       05  WRK-660-PERLIBE-9YTM    PIC  9(004) COMP    VALUE 0007.
VANS       05  WRK-660-TRIBUTOS-9YTM   PIC  9(004) COMP    VALUE 0014.
VANS       05  WRK-660-PERTRIB-9YTM    PIC  9(004) COMP    VALUE 0007.
VANS       05  WRK-660-SEGUROS-9YTM    PIC  9(004) COMP    VALUE 0014.
VANS       05  WRK-660-PERSEGU-9YTM    PIC  9(004) COMP    VALUE 0007.
VANS       05  WRK-660-TARIFAS-9YTM    PIC  9(004) COMP    VALUE 0014.
VANS       05  WRK-660-PERTARI-9YTM    PIC  9(004) COMP    VALUE 0007.
VANS       05  WRK-660-PGTOSERV-9YTM   PIC  9(004) COMP    VALUE 0014.
VANS       05  WRK-660-PERPGTO-9YTM    PIC  9(004) COMP    VALUE 0007.
VANS       05  WRK-660-REGISTROS-9YTM  PIC  9(004) COMP    VALUE 0014.
VANS       05  WRK-660-PERREGI-9YTM    PIC  9(004) COMP    VALUE 0007.
VANS       05  WRK-660-TOTAL-9YTM      PIC  9(004) COMP    VALUE 0014.
VANS       05  WRK-660-PERTOTA-9YTM    PIC  9(004) COMP    VALUE 0007.
           05  WRK-660-CETAM-9YTM      PIC  9(004) COMP    VALUE 0008.
           05  WRK-660-CETAA-9YTM      PIC  9(004) COMP    VALUE 0008.
           05  WRK-660-CHAMADOR-9YTM   PIC  9(004) COMP    VALUE 0008.
           05  WRK-660-MENSA-9YTM      PIC  9(004) COMP    VALUE 0079.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ERRO DA BRAD0660 ***'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-BRAD0660.
           05  FILLER                  PIC  X(045)         VALUE
               '* RETURN-CODE DIFERENTE DE ZEROS NA BRAD0660'.
           05  FILLER                  PIC  X(012)         VALUE
               '- RET.COD ='.
           05  WRK-RETURN-CODE         PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(011)         VALUE
               ' - LOCAL ='.
           05  WRK-LOCAL-ERRO          PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(002)         VALUE ' *'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO CADASTRAMENTO ***'.
      *----------------------------------------------------------------*

       01  WRK-COMU-AREA.
           05  WRK-COMU-LL               PIC S9(004) COMP  VALUE ZEROS.
           05  WRK-COMU-ZZ               PIC S9(004) COMP  VALUE ZEROS.
           05  WRK-COMU-TRANCODE.
               10  WRK-COMU-TRANSACAO    PIC  X(008)       VALUE SPACES.
               10  FILLER                PIC  X(006)       VALUE SPACES.
               10  WRK-COMU-PFK          PIC  X(001)       VALUE SPACES.
           05  WRK-COMU-SENHAS           PIC  X(037)       VALUE SPACES.
           05  WRK-COMU-COMANDO          PIC  X(068)       VALUE SPACES.
           05  WRK-COMU-DADOS.
               10  WRK-COMU-OPCAO        PIC  X(001)       VALUE SPACES.
               10  WRK-COMU-TIMESTAMP    PIC  X(026)       VALUE SPACES.
               10  WRK-COMU-NOME         PIC  X(040)       VALUE SPACES.
               10  WRK-COMU-CPFCNPJ.
4S2511             15  WRK-COMU-CNPJCPF  PIC  X(009)       VALUE SPACES.
                   15  FILLER            PIC  X(001)       VALUE SPACES.
4S2511             15  WRK-COMU-FILIAL   PIC  X(004)       VALUE SPACES.
                   15  FILLER            PIC  X(001)       VALUE SPACES.
                   15  WRK-COMU-CONTROL  PIC  9(002)       VALUE ZEROS.
               10  WRK-COMU-BCO-AGE-CTA.
                   15  WRK-COMU-BANCO    PIC  9(003)       VALUE ZEROS.
                   15  FILLER            PIC  X(001)       VALUE SPACES.
                   15  WRK-COMU-AGENC    PIC  9(005)       VALUE ZEROS.
                   15  FILLER            PIC  X(001)       VALUE SPACES.
                   15  WRK-COMU-CONTA    PIC  9(013)       VALUE ZEROS.
               10  WRK-COMU-PROD         PIC  9(003)       VALUE ZEROS.
               10  WRK-COMU-DPROD        PIC  X(012)       VALUE SPACES.
               10  WRK-COMU-SUBPROD      PIC  9(003)       VALUE ZEROS.
               10  WRK-COMU-DSPROD       PIC  X(012)       VALUE SPACES.
               10  WRK-COMU-OPER         PIC  9(013)       VALUE ZEROS.
               10  FILLER REDEFINES WRK-COMU-OPER.
                   15  WRK-COMU-DANO     PIC  9(004).
                   15  WRK-COMU-NSEQ     PIC  9(009).
               10  WRK-COMU-SIT1         PIC  X(015)       VALUE SPACES.
               10  WRK-COMU-SIT2         PIC  X(013)       VALUE SPACES.
               10  WRK-COMU-SIT3         PIC  9(009)       VALUE ZEROS.
               10  WRK-COMU-SIT4         PIC  X(013)       VALUE SPACES.
               10  WRK-COMU-SIT5         PIC  9(009)       VALUE ZEROS.
               10  WRK-COMU-MEIENT       PIC  9(003)       VALUE ZEROS.
               10  WRK-COMU-TIPDES       PIC  9(003)       VALUE ZEROS.
               10  WRK-COMU-MENSAGEM     PIC  X(079)       VALUE SPACES.
               10  WRK-COMU-CHAMADOR     PIC  X(008)       VALUE SPACES.
               10  WRK-COMU-PAG          PIC  9(004)       VALUE ZEROS.
               10  WRK-COMU-OCORR        PIC  9(008)       VALUE ZEROS.
BRQ001         10  WRK-COMU-LIBMID       PIC  X(001)       VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO DCOM0305/0308 ***'.
      *----------------------------------------------------------------*

       01  WRK-COMU-AREA-0305.
           05 WRK-COMU-LL-0305        PIC S9(004) COMP     VALUE ZEROS.
           05 WRK-COMU-ZZ-0305        PIC S9(004) COMP     VALUE ZEROS.
           05 WRK-COMU-TRANCODE-0305.
             10 WRK-COMU-TRANSACAO-0305
                                      PIC  X(008)          VALUE SPACES.
             10 FILLER                PIC  X(006)          VALUE SPACES.
             10 WRK-COMU-PFK-0305     PIC  X(001)          VALUE SPACES.
           05 WRK-COMU-SENHAS-0305    PIC  X(037)          VALUE SPACES.
           05 WRK-COMU-COMANDO-0305   PIC  X(068)          VALUE SPACES.
           05 WRK-COMU-DADOS-0305.
               10 WRK-COMU-OPCAO-0305     PIC  9(001)      VALUE ZEROS.
               10 WRK-COMU-OPERACAO-0305  PIC  9(013)      VALUE ZEROS.
               10 FILLER               REDEFINES WRK-COMU-OPERACAO-0305.
                  15 WRK-COMU-DANO-0305   PIC  9(004).
                  15 WRK-COMU-NSEQ-0305   PIC  9(009).
               10 WRK-COMU-TMSTAMP-0305   PIC  X(026)      VALUE SPACES.
               10 WRK-COMU-DADOS-0302.
                 15 WRK-COMU-MENSA-0302    PIC  X(079)     VALUE SPACES.
                 15 WRK-COMU-FILLER        PIC  X(043)     VALUE SPACES.
               10 FILLER  REDEFINES WRK-COMU-DADOS-0302.
                 15 WRK-COMU-AGENCIA-0305  PIC  9(005).
                 15 WRK-COMU-PAG-0305      PIC  9(004).
                 15 WRK-COMU-MENSA-0305    PIC  X(079).
                 15 WRK-COMU-BANCO-0305    PIC  9(003).
                 15 WRK-COMU-CONTA-0305    PIC  9(013).
4S2511*         15 WRK-COMU-CCPFCNPJ-0305 PIC  9(009).
4S2511          15 WRK-COMU-CCPFCNPJ-0305 PIC  X(09).
4S2511*         15 WRK-COMU-CFLIAL-0305   PIC  9(004).
4S2511          15 WRK-COMU-CFLIAL-0305   PIC  X(04).
                 15 WRK-COMU-CCTRL-0305    PIC  9(002).
                 15 WRK-COMU-OCORR-0305    PIC  9(002).
                 15 WRK-COMU-FORMAL-0305   PIC  X(001).
               10 WRK-COMU-CHAMADOR-0305   PIC  X(008)     VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(079)         VALUE
           '*** AREA DE COMUNICACAO DCOM0402/DCOM0403/DCOM0404 ***'.
      *----------------------------------------------------------------*

       01  WRK-COMU-AREA-UN.
           05  WRK-COMU-LL-UN          PIC S9(004) COMP    VALUE ZEROS.
           05  WRK-COMU-ZZ-UN          PIC S9(004) COMP    VALUE ZEROS.
           05  WRK-COMU-TRANCODE-UN.
               10  WRK-COMU-TRANSACAO-UN PIC  X(008)       VALUE SPACES.
               10  FILLER                PIC  X(006)       VALUE SPACES.
               10  WRK-COMU-PFK-UN       PIC  X(001)       VALUE SPACES.
           05  WRK-COMU-SENHAS-UN        PIC  X(037)       VALUE SPACES.
           05  WRK-COMU-COMANDO-UN       PIC  X(068)       VALUE SPACES.
           05  WRK-COMU-DADOS-UN.
               10  WRK-COMU-OPCAO-UN-X.
                   15  WRK-COMU-OPCAO-UN PIC  9(001)       VALUE ZEROS.
               10  WRK-COMU-OPER-UN      PIC  9(013)       VALUE ZEROS.
               10  FILLER                REDEFINES WRK-COMU-OPER-UN.
                   15  WRK-COMU-DANO-UN  PIC  9(004).
                   15  WRK-COMU-NSEQ-UN  PIC  9(009).
               10  WRK-COMU-AGENCIA-UN   PIC  9(005)       VALUE ZEROS.
               10  WRK-COMU-AGENC-UN-X.
                   15  WRK-COMU-AGENC-UN PIC  9(005)       VALUE ZEROS.
               10  WRK-COMU-CONTA-UN     PIC  9(013)       VALUE ZEROS.
               10  WRK-COMU-CNPJ-UN-X.
4S2511*            15  WRK-COMU-CNPJ-UN  PIC  9(009)       VALUE ZEROS.
4S2511             15  WRK-COMU-CNPJ-UN  PIC  X(09)        VALUE SPACES.
4S2511*        10  WRK-COMU-FILIAL-UN    PIC  9(004)       VALUE ZEROS.
4S2511         10  WRK-COMU-FILIAL-UN    PIC  X(04)        VALUE SPACES.
               10  WRK-COMU-DIG-UN       PIC  9(002)       VALUE ZEROS.
               10  WRK-COMU-DANO-OPER-UN PIC  9(004)       VALUE ZEROS.
               10  WRK-COMU-NSEQ-OPER-UN PIC  9(009)       VALUE ZEROS.
               10  WRK-COMU-TIMESTAMP-UN PIC  X(026)       VALUE ZEROS.
               10  WRK-COMU-MENSAGEM-UN  PIC  X(079)       VALUE SPACES.
               10  WRK-COMU-PAGINA-UN    PIC  9(003)       VALUE ZEROS.
               10  WRK-COMU-CHAMADOR-UN  PIC  X(008)       VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(070)         VALUE
           'AREA DE COMUNICACAO COM DCOM0804/0823/0822/0813'.
      *----------------------------------------------------------------*

       01  WRK-COMU-AREA-0804.
           05  WRK-COMU-LL-0804           PIC S9(004) COMP  VALUE ZEROS.
           05  WRK-COMU-ZZ-0804           PIC S9(004) COMP  VALUE ZEROS.
           05  WRK-COMU-TRANCODE-0804.
               10  WRK-COMU-TRANSACAO-0804 PIC  X(008)     VALUE SPACES.
               10  FILLER                 PIC  X(006)      VALUE SPACES.
               10  WRK-COMU-PFK-0804      PIC  X(001)      VALUE SPACES.
           05  WRK-COMU-SENHAS-0804       PIC  X(037)      VALUE SPACES.
           05  WRK-COMU-COMANDO-0804      PIC  X(068)      VALUE SPACES.
           05  WRK-COMU-DADOS-0804.
               10  WRK-COMU-TPMANUT-0804       PIC  X(001) VALUE SPACES.
               10  WRK-COMU-TIMESTAMP-0804     PIC  X(026) VALUE SPACES.
               10  WRK-COMU-OPERACAO-0804      PIC  X(013) VALUE ZEROS.
               10  FILLER REDEFINES WRK-COMU-OPERACAO-0804.
                   15  WRK-COMU-DANO-OPER-0804 PIC  9(004).
                   15  WRK-COMU-NSEQ-OPER-0804 PIC  9(009).
               10  WRK-COMU-CBCO-0804          PIC  9(003) VALUE ZEROS.
               10  WRK-COMU-CAG-BCRIA-0804     PIC  9(005) VALUE ZEROS.
               10  WRK-COMU-CTA-BCRIA-CLI-0804 PIC  9(013) VALUE ZEROS.
4S2511         10  WRK-COMU-CCPF-CNPJ-0804     PIC  X(009) VALUE SPACES.
4S2511         10  WRK-COMU-CFLIAL-CNPJ-0804   PIC  X(004) VALUE SPACES.
               10  WRK-COMU-CCTRL-CNPJ-CPF-0804
                                               PIC  9(002) VALUE ZEROS.
               10  WRK-COMU-PAGINA-0804        PIC  9(003) VALUE ZEROS.
               10  WRK-COMU-MENSAGEM-0804      PIC  X(079) VALUE SPACES.
           05  REDEFINES WRK-COMU-DADOS-0804.
               10  WRK-COMU-TIMESTAMP-2-0804       PIC  X(026).
               10  WRK-COMU-OPERACAO-2-0804        PIC  X(013).
               10  FILLER REDEFINES WRK-COMU-OPERACAO-2-0804.
                   15  WRK-COMU-DANO-OPER-2-0804   PIC  9(004).
                   15  WRK-COMU-NSEQ-OPER-2-0804   PIC  9(009).
               10  WRK-COMU-MENSAGEM-2-0804        PIC  X(079).
               10  FILLER                          PIC  X(040).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(080)         VALUE
           '*** AREA DE COMUNICACAO PROGRAMAS CONS. OPERACAO ***'.
      *----------------------------------------------------------------*

       01  WRK-COMU-AREA-0612.
           05  WRK-COMU-LL-0612          PIC S9(004) COMP  VALUE ZEROS.
           05  WRK-COMU-ZZ-0612          PIC S9(004) COMP  VALUE ZEROS.
           05  WRK-COMU-TRANCODE-0612.
               10  WRK-COMU-TRANSACAO-0612
                                         PIC  X(008)       VALUE SPACES.
               10  FILLER                PIC  X(006)       VALUE SPACES.
               10  WRK-COMU-PFK-0612     PIC  X(001)       VALUE SPACES.
           05  WRK-COMU-SENHAS-0612      PIC  X(037)       VALUE SPACES.
           05  WRK-COMU-COMANDO-0612     PIC  X(068)       VALUE SPACES.
           05  WRK-COMU-DADOS-0612.
               10  WRK-COMU-TIMESTAMP-0612   PIC  X(026)   VALUE SPACES.
               10  WRK-COMU-MENSAGEM-0612    PIC  X(079)   VALUE SPACES.
               10  WRK-COMU-DANO-0612        PIC  9(004)   VALUE ZEROS.
               10  WRK-COMU-NSEQ-0612        PIC  9(009)   VALUE ZEROS.
               10  WRK-COMU-TIMES-AUX-0612   PIC  X(026)   VALUE SPACES.
               10  WRK-COMU-OPCAO-0612       PIC  X(001)   VALUE SPACES.

WIP001*----------------------------------------------------------------*
WIP001 01  FILLER                      PIC  X(080)         VALUE
WIP001     '*** AREA DE COMUNICACAO PROGRAMA CONSULTA OPTIN ***'.
WIP001*----------------------------------------------------------------*
WIP001
WIP001 01  WRK-COMU-AREA-3935.
WIP001     05  WRK-COMU-LL-3935          PIC S9(004) COMP  VALUE ZEROS.
WIP001     05  WRK-COMU-ZZ-3935          PIC S9(004) COMP  VALUE ZEROS.
WIP001     05  WRK-COMU-TRANCODE-3935.
WIP001         10  WRK-COMU-TRANS-3935   PIC  X(008)       VALUE SPACES.
WIP001         10  FILLER                PIC  X(006)       VALUE SPACES.
WIP001         10  WRK-COMU-PFK-3935     PIC  X(001)       VALUE SPACES.
WIP001     05  WRK-COMU-SENHAS-3935      PIC  X(037)       VALUE SPACES.
WIP001     05  WRK-COMU-COMANDO-3935     PIC  X(068)       VALUE SPACES.
WIP001     05  WRK-COMU-MENSAGEM-3935    PIC  X(079)       VALUE SPACES.
WIP001     05  WRK-COMU-DADOS-3935.
WIP001         10  WRK-COMU-OPER-3935    PIC  9(013)       VALUE ZEROS.
WIP001         10  FILLER REDEFINES WRK-COMU-OPER-3935.
WIP001             15  WRK-COMU-DANO-3935 PIC  9(004).
WIP001             15  WRK-COMU-NSEQ-3935 PIC  9(009).
WIP001         10  WRK-COMU-CHAMADOR-3935 PIC  X(008)      VALUE SPACES.
WIP001         10  WRK-COMU-DADOS-CHAMADOR-3935.
WIP001             15  WRK-COMU-OCU1-3935 PIC  X(079)      VALUE SPACES.
WIP001             15  WRK-COMU-OCU2-3935 PIC  X(079)      VALUE SPACES.
WIP001             15  WRK-COMU-OCU3-3935 PIC  X(079)      VALUE SPACES.
WIP001             15  WRK-COMU-OCU4-3935 PIC  X(079)      VALUE SPACES.
WIP001
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(070)         VALUE
           '*** AREA DE COMUNICACAO MODULO DE IMPRESSAO ***'.
      *----------------------------------------------------------------*

       01  COMU-AREA-P3925.
           05 WRK-COMU-RETORNO-P3925      PIC  9(002)      VALUE ZEROS.
           05 WRK-COMU-IMPRESSORA-P3925   PIC  X(008)      VALUE SPACES.
           05 WRK-COMU-DANO-OPER-DESC-P3925
                                          PIC  9(004)      VALUE ZEROS.
           05 WRK-COMU-NSEQ-OPER-DESC-P3925
                                          PIC  9(009)      VALUE ZEROS.
           05 WRK-COMU-TIMESTAMP-P3925    PIC  X(026)      VALUE SPACES.
VANS       05 WRK-COMU-OPCAO-P3925        PIC  9(001)      VALUE ZEROS.
           05 WRK-COMU-CHAMADOR-P3925     PIC  X(008)      VALUE SPACES.
           05 WRK-COMU-USUARIO-P3925      PIC  X(007)      VALUE SPACES.
           05 WRK-COMU-DEPTO-P3925        PIC  9(006)      VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA ACESSO DCOM5575 ***'.
      *----------------------------------------------------------------*

       01  WRK-5575-ARGUMENTOS-ENTRADA.

       03  WRK-5575-AREA-ENVIO.
           05  WRK-5575-CPRODT           PIC  9(003)       VALUE ZEROS.
           05  WRK-5575-CSPROD-DESC-COML PIC  9(003)       VALUE ZEROS.
4S2511     05  WRK-5575-CCNPJ-CPF        PIC  X(009)       VALUE SPACES.
4S2511     05  WRK-5575-CFLIAL-CNPJ      PIC  X(004)       VALUE SPACES.
           05  WRK-5575-CCTRL-CNPJ-CPF   PIC  9(002)       VALUE ZEROS.

       01  WRK-5575-AREA-RETORNO.
           05  WRK-5575-CD-RETORNO       PIC  9(004)       VALUE ZEROS.
           05  WRK-5575-MENSAGEM         PIC  X(079)       VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(080)         VALUE
           '*** AREA DE COMUNICACAO PROGRAMA DCOM5577              ***'.
      *----------------------------------------------------------------*

       01  WRK-5577-ARGUMENTOS-ENTRADA.
           05 WRK-5577-DANO-OPER-DESC    PIC 9(004)        VALUE ZEROS.
           05 WRK-5577-NSEQ-OPER-DESC    PIC 9(009)        VALUE ZEROS.
           05 WRK-5577-CELMTO-DESC-COML  PIC 9(003)        VALUE ZEROS.
           05 WRK-5577-CEVNTO-DESC-COML  PIC 9(003)        VALUE ZEROS.

       01  WRK-5577-RETORNO.
           05 WRK-5577-COD-RETORNO       PIC 9(004)        VALUE ZEROS.
           05 WRK-5577-MSG-RETORNO       PIC X(079)        VALUE SPACES.
           05 WRK-5577-CET-DIA           PIC 9(08)V9(7)    VALUE ZEROS.
           05 WRK-5577-CET-MES           PIC 9(08)V9(7)    VALUE ZEROS.
           05 WRK-5577-CET-ANO           PIC 9(08)V9(7)    VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(080)         VALUE
           '*** AREA DE COMUNICACAO PROGRAMA DCOM5537              ***'.
      *----------------------------------------------------------------*

       COPY 'I#DCOMJ4'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO DCOM4174 ***'.
      *----------------------------------------------------------------*

       COPY 'I#DCOMXJ'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA BRAD7100 ***'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** DCOM3924 - FIM DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  LNK-IO-PCB.
           05 LNK-IO-LTERM             PIC  X(008).
           05 FILLER                   PIC  X(002).
           05 LNK-IO-STATUS            PIC  X(002).
           05 FILLER                   PIC  X(012).
           05 LNK-IO-MODNAME           PIC  X(008).

       01  LNK-ALT-PCB.
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
               GOBACK
           END-IF.

           MOVE SPACES                 TO WRK-OUT-DADOS-9YTM
                                          WRK-MSG-AUX.

           PERFORM 2000-PROCESSAR-MENSAGEM.

           GO TO 0000-INICIAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     PROCESSAMENTO DA MENSAGEM RECEBIDA                         *
      *----------------------------------------------------------------*
       2000-PROCESSAR-MENSAGEM         SECTION.
      *----------------------------------------------------------------*

           MOVE 'DCOM9YTM'             TO WRK-TELA.

           IF  WRK-TRANSACAO           NOT EQUAL 'DCOM3924'
               PERFORM 2100-RECEBER-CONTROLE
           ELSE
               PERFORM 2200-PROCESSAR-DCOM9YTM
           END-IF.

           IF  WRK-TELA                EQUAL 'DCOM9YTM'
               PERFORM 2300-PROCESSAR-BRAD0660
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     RECEBE CONTROLE DE OUTRA TRANSACAO IMS                     *
      *----------------------------------------------------------------*
       2100-RECEBER-CONTROLE           SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO WRK-BUSCA-MOVIMENTO.

           EVALUATE WRK-TRANSACAO
               WHEN 'DCOM0305'
               WHEN 'DCOM0308'
               WHEN 'DCOM0311'
                  MOVE WRK-MENSAGEM        TO WRK-COMU-AREA-0305
                  MOVE WRK-COMU-DANO-0305  TO WRK-COMU-DANO
                  MOVE WRK-COMU-NSEQ-0305  TO WRK-COMU-NSEQ
                  MOVE WRK-COMU-DADOS-0305 TO WRK-INP-OCULTOS-9YTM
                                              WRK-OUT-OCULTOS-9YTM
                  IF  WRK-COMU-OPCAO-0305  EQUAL 1
                      MOVE 'N'             TO WRK-BUSCA-MOVIMENTO
                  ELSE
                      MOVE 'S'             TO WRK-BUSCA-MOVIMENTO
                  END-IF

               WHEN 'DCOM0405'
               WHEN 'DCOM0408'
               WHEN 'DCOM0411'
BRA001         WHEN 'DCOM1004'
BRA001         WHEN 'DCOM1005'
BRA001         WHEN 'DCOM1008'
                  MOVE WRK-MENSAGEM          TO WRK-COMU-AREA-UN
                  MOVE WRK-COMU-DANO-OPER-UN TO WRK-COMU-DANO
                  MOVE WRK-COMU-NSEQ-OPER-UN TO WRK-COMU-NSEQ
                  MOVE WRK-COMU-DADOS-UN     TO WRK-INP-OCULTOS-9YTM
                                                WRK-OUT-OCULTOS-9YTM

               WHEN 'DCOM0612'
               WHEN 'DCOM0618'
                  MOVE WRK-MENSAGEM        TO WRK-COMU-AREA-0612
                  MOVE WRK-COMU-DANO-0612  TO WRK-COMU-DANO
                  MOVE WRK-COMU-NSEQ-0612  TO WRK-COMU-NSEQ
                  MOVE WRK-COMU-DADOS-0612 TO WRK-OUT-OCULTOS-9YTM

               WHEN 'DCOM0315'
               WHEN 'DCOM0336'
               WHEN 'DCOM0337'
                  MOVE WRK-MENSAGEM        TO WRK-COMU-AREA-0612
                  MOVE WRK-COMU-DANO-0612  TO WRK-COMU-DANO
                  MOVE WRK-COMU-NSEQ-0612  TO WRK-COMU-NSEQ
                  MOVE WRK-COMU-DADOS-0612 TO WRK-OUT-OCULTOS-9YTM
                  MOVE 'S'                 TO WRK-BUSCA-MOVIMENTO

               WHEN 'DCOM0804'
               WHEN 'DCOM0813'
               WHEN 'DCOM0822'
               WHEN 'DCOM0823'
                  MOVE WRK-MENSAGEM              TO WRK-COMU-AREA-0804
                  MOVE WRK-COMU-DANO-OPER-2-0804 TO WRK-COMU-DANO
                  MOVE WRK-COMU-NSEQ-OPER-2-0804 TO WRK-COMU-NSEQ
                  MOVE WRK-COMU-DADOS-0804       TO WRK-OUT-OCULTOS-9YTM

WIP001         WHEN 'DCOM3935'
WIP001            PERFORM 2140-RECEBER-CTRL-3935
WIP001
               WHEN OTHER
                  MOVE WRK-MENSAGEM        TO WRK-COMU-AREA
                  MOVE WRK-COMU-DADOS      TO WRK-INP-OCULTOS-9YTM
                                              WRK-OUT-OCULTOS-9YTM
           END-EVALUATE.

           MOVE WRK-COMU-DANO              TO WRK-INP-DANO-9YTM
                                              WRK-OUT-DANO-9YTM
           MOVE WRK-COMU-NSEQ              TO WRK-INP-NSEQ-9YTM
                                              WRK-OUT-NSEQ-9YTM
           MOVE WRK-TRANSACAO              TO WRK-OUT-CHAMADOR-9YTM.

           MOVE WRK-MSG001                 TO WRK-OUT-MENSA-9YTM.

           INITIALIZE WRK-5575-ARGUMENTOS-ENTRADA
                      WRK-5575-AREA-RETORNO.

           IF  WRK-TRANSACAO               EQUAL 'DCOM0612' OR
               WRK-TRANSACAO               EQUAL 'DCOM0618'
               PERFORM 2110-OBTER-OPER-HIST-OFIC
           ELSE
               PERFORM 2120-OBTER-OPERACAO
           END-IF.

           IF  WRK-BUSCA-MOVIMENTO     EQUAL 'S'
               PERFORM 2130-BUSCAR-CET-MOVIMENTO
           END-IF.

WIP001     IF  WRK-COMU-MENSAGEM-3935  NOT EQUAL SPACES AND LOW-VALUES
WIP001         MOVE WRK-COMU-MENSAGEM-3935
WIP001                                 TO WRK-OUT-MENSA-9YTM
WIP001     END-IF.
WIP001
      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  OBTER OPERACAO DO HISTORICO                                   *
      *----------------------------------------------------------------*
       2110-OBTER-OPER-HIST-OFIC       SECTION.
      *----------------------------------------------------------------*

           INITIALIZE 5537-ENTRADA-ROTEADOR

           IF  WRK-TRANSACAO            EQUAL 'DCOM0612'
               MOVE 'O'                 TO  5537-ENT-FLAG
           ELSE
               MOVE 'H'                 TO  5537-ENT-FLAG
           END-IF.

           MOVE WRK-COD-USER-R          TO 5537-ENT-CFUNC-BDSCO.
           MOVE LNK-IO-LTERM            TO 5537-ENT-CTERM.
           MOVE WRK-COMU-DANO           TO 5537-ENT-DANO-OPER-DESC.
           MOVE WRK-COMU-NSEQ           TO 5537-ENT-NSEQ-OPER-DESC.
           MOVE WRK-COMU-TIMES-AUX-0612 TO 5537-ENT-HULT-ATULZ.
           MOVE 'DCOM5537'              TO WRK-MODULO.

           CALL WRK-MODULO    USING     5537-ENTRADA-ROTEADOR
                                        5537-SAIDA-ROTEADOR
                                        ERRO-AREA
                                        WRK-SQLCA.

           EVALUATE 5537-SAI-COD-RETORNO

               WHEN '0000'
                    PERFORM 2111-MONTAR-TELA

               WHEN '0003'
               WHEN '0100'
                    CONTINUE

               WHEN '0099'
                    MOVE  'DB2'     TO     ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                    MOVE  'APL'     TO     ERR-TIPO-ACESSO
VANS                MOVE  5537-SAI-MSG-RETORNO(1:75)
VANS                                TO     ERR-TEXTO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO

           END-EVALUATE.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA CARREGA OS CAMPOS DA TELA                         *
      *----------------------------------------------------------------*
       2111-MONTAR-TELA                SECTION.
      *----------------------------------------------------------------*

VANS       MOVE 5537-SAI-IPSSOA-DESC-COML(1:40) TO WRK-INP-NOMECL-9YTM
                                                   WRK-OUT-NOMECL-9YTM

4S2511     IF 5537-SAI-CCNPJ-CPF    EQUAL SPACES OR
4S2511        5537-SAI-CCNPJ-CPF    EQUAL LOW-VALUES OR
4S2511        5537-SAI-CFLIAL-CNPJ  EQUAL SPACES OR
4S2511        5537-SAI-CFLIAL-CNPJ  EQUAL LOW-VALUES
4S2511         MOVE SPACES          TO WRK-INP-CNPJCPF-9YTM
4S2511         MOVE SPACES          TO WRK-OUT-CNPJCPF-9YTM
4S2511     ELSE
4S2511         STRING 5537-SAI-CCNPJ-CPF(1:3)  '.'
4S2511                5537-SAI-CCNPJ-CPF(4:3)  '.'
4S2511                5537-SAI-CCNPJ-CPF(7:3)  '/'
4S2511                5537-SAI-CFLIAL-CNPJ      '-'
4S2511                5537-SAI-CCTRL-CNPJ-CPF
4S2511                                    DELIMITED BY SIZE
4S2511                                    INTO WRK-INP-CNPJCPF-9YTM
4S2511         MOVE WRK-INP-CNPJCPF-9YTM    TO WRK-OUT-CNPJCPF-9YTM
4S2511     END-IF
           MOVE 5537-SAI-CBCO           TO WRK-BANCO-AUX.
           MOVE 5537-SAI-CAG-BCRIA      TO WRK-AGENC-AUX.
           MOVE 5537-SAI-CCTA-BCRIA-CLI TO WRK-CONTA-AUX.
           MOVE WRK-BCOAGECTA-AUX       TO WRK-OUT-BCAGCT-9YTM.

           MOVE 5537-SAI-CPRODT         TO WRK-INP-CODPRD-9YTM
                                           WRK-OUT-CODPRD-9YTM
           MOVE 5537-SAI-IABREV-PRODT   TO WRK-INP-DESPRD-9YTM
                                           WRK-OUT-DESPRD-9YTM
           MOVE 5537-SAI-CSPROD-DESC-COML TO WRK-INP-CODSUB-9YTM
                                             WRK-OUT-CODSUB-9YTM
           MOVE 5537-SAI-IRSUMO-SPROD-DESC TO WRK-INP-DESSUB-9YTM
                                              WRK-OUT-DESSUB-9YTM
           MOVE 5537-SAI-IRSUMO-SIT-DESC  TO WRK-INP-SITUAC-9YTM
                                             WRK-OUT-SITUAC-9YTM

VANS***    SE  5537-SAI-CCONTR-CONVE-DESC NOT EQUAL ZEROS
VANS***        SE  5537-SAI-CELMTO-DESC-COML  EQUAL 8
VANS***            MOVER 'CONV GERAL..:'       TO WRK-OUT-SIT3-9YTM
VANS***        SENAO
VANS***            MOVER 'CONV CLIENTE:'       TO WRK-OUT-SIT3-9YTM
VANS***        FIM-SE
VANS***        MOVER 5537-SAI-CCONTR-CONVE-DESC
VANS***                                   TO WRK-OUT-SIT4-9YTM-N
VANS***    SENAO
VANS***        SE  5537-SAI-CCONTR-LIM-DESC NOT EQUAL ZEROS
VANS***            MOVER 'CONTR LIMITE:'   TO WRK-OUT-SIT3-9YTM
VANS***            MOVER 5537-SAI-CCONTR-LIM-DESC
VANS***                                   TO WRK-OUT-SIT4-9YTM-N
VANS***        FIM-SE
VANS***    FIM-SE.
VANS***
VANS       MOVE 5537-SAI-VLIQ-OPER-DESC    TO WRK-VALOR-LIBER.
VANS       MOVE WRK-VALOR-LIBER            TO WRK-VALOR-DESCOMP.
VANS       MOVE WRK-VALOR-DESCOMP-R        TO WRK-VALOR-TELA-N.
VANS       MOVE WRK-VALOR-TELA             TO WRK-OUT-VLRLIBE-9YTM.
VANS
           MOVE 5537-SAI-VIOF-OPER-DESC    TO WRK-VALOR-IOF.
VANS       MOVE WRK-VALOR-IOF              TO WRK-VALOR-DESCOMP.
VANS       MOVE WRK-VALOR-DESCOMP-R        TO WRK-VALOR-TELA-N.
           MOVE WRK-VALOR-TELA             TO WRK-OUT-TRIBUTOS-9YTM.

           MOVE 5537-SAI-VTARIF-REG-TITLO  TO WRK-VALOR-TAC-TITULOS.
           MOVE 5537-SAI-VTAC-OPER-DESC    TO WRK-VALOR-TAC-OPERACAO.

           COMPUTE WRK-VALOR-TOT-TARIFAS   = WRK-VALOR-TAC-TITULOS
                                           + WRK-VALOR-TAC-OPERACAO.

VANS       MOVE WRK-VALOR-TOT-TARIFAS      TO WRK-VALOR-DESCOMP.
VANS       MOVE WRK-VALOR-DESCOMP-R        TO WRK-VALOR-TELA-N.
           MOVE WRK-VALOR-TELA             TO WRK-OUT-TARIFAS-9YTM.

           COMPUTE WRK-VALOR-TOTAL         = WRK-VALOR-TOT-TARIFAS
                                           + WRK-VALOR-IOF.

VANS       MOVE WRK-VALOR-TOTAL            TO WRK-VALOR-DESCOMP.
VANS       MOVE WRK-VALOR-DESCOMP-R        TO WRK-VALOR-TELA-N.
           MOVE WRK-VALOR-TELA             TO WRK-OUT-TOTAL-9YTM.

VANS       MOVE 5537-SAI-VLIQ-OPER-DESC    TO WRK-VALOR-LIQUI.
VANS       COMPUTE WRK-VALOR-LIQUI         = WRK-VALOR-LIQUI
VANS                                       + WRK-VALOR-TOTAL.
VANS       MOVE WRK-VALOR-LIQUI            TO WRK-VALOR-DESCOMP.
VANS       MOVE WRK-VALOR-DESCOMP-R        TO WRK-VALOR-TELA-N.
VANS       MOVE WRK-VALOR-TELA             TO WRK-OUT-VLRLIQU-9YTM.
VANS
VANS       IF  WRK-VALOR-LIQUI             GREATER ZEROS
VANS           COMPUTE WRK-PERC            = (WRK-VALOR-LIBER
VANS                                       / WRK-VALOR-LIQUI) * 100
VANS       ELSE
VANS           MOVE ZEROS                  TO WRK-PERC
VANS       END-IF.
VANS       MOVE ZEROS                      TO WRK-PERC-R.
VANS       COMPUTE WRK-PERC-R ROUNDED      = WRK-PERC-R
VANS                                       + WRK-PERC.
VANS       MOVE WRK-PERC-R                 TO WRK-PERC-AUX-N.
VANS       MOVE WRK-PERC-AUX               TO WRK-OUT-PERLIBE-9YTM.
VANS
VANS       IF  WRK-VALOR-LIQUI             GREATER ZEROS
VANS           COMPUTE WRK-PERC            = (WRK-VALOR-IOF
VANS                                       / WRK-VALOR-LIQUI) * 100
VANS       ELSE
VANS           MOVE ZEROS                  TO WRK-PERC
VANS       END-IF.
VANS       MOVE ZEROS                      TO WRK-PERC-R.
VANS       COMPUTE WRK-PERC-R ROUNDED      = WRK-PERC-R
VANS                                       + WRK-PERC.
VANS       MOVE WRK-PERC-R                 TO WRK-PERC-AUX-N.
VANS       MOVE WRK-PERC-AUX               TO WRK-OUT-PERTRIB-9YTM.
VANS
VANS       IF  WRK-VALOR-LIQUI             GREATER ZEROS
VANS           COMPUTE WRK-PERC            = (WRK-VALOR-TOT-TARIFAS
VANS                                       / WRK-VALOR-LIQUI) * 100
VANS       ELSE
VANS           MOVE ZEROS                  TO WRK-PERC
VANS       END-IF.
VANS       MOVE ZEROS                      TO WRK-PERC-R.
VANS       COMPUTE WRK-PERC-R ROUNDED      = WRK-PERC-R
VANS                                       + WRK-PERC.
VANS       MOVE WRK-PERC-R                 TO WRK-PERC-AUX-N.
VANS       MOVE WRK-PERC-AUX               TO WRK-OUT-PERTARI-9YTM.
VANS
VANS       IF  WRK-VALOR-LIQUI             GREATER ZEROS
VANS           COMPUTE WRK-PERC            = (WRK-VALOR-TOTAL
VANS                                       / WRK-VALOR-LIQUI) * 100
VANS       ELSE
VANS           MOVE ZEROS                  TO WRK-PERC
VANS       END-IF.
VANS       MOVE ZEROS                      TO WRK-PERC-R.
VANS       COMPUTE WRK-PERC-R ROUNDED      = WRK-PERC-R
VANS                                       + WRK-PERC.
VANS       MOVE WRK-PERC-R                 TO WRK-PERC-AUX-N.
VANS       MOVE WRK-PERC-AUX               TO WRK-OUT-PERTOTA-9YTM.
VANS
           MOVE ZEROS                      TO WRK-VALOR-TELA-N.
VANS       MOVE ZEROS                      TO WRK-PERC-AUX-N.
           MOVE WRK-VALOR-TELA             TO WRK-OUT-SEGUROS-9YTM
                                              WRK-OUT-PGTOSERV-9YTM
                                              WRK-OUT-REGISTROS-9YTM.
VANS       MOVE WRK-PERC-AUX               TO WRK-OUT-PERSEGU-9YTM
VANS                                          WRK-OUT-PERPGTO-9YTM
VANS                                          WRK-OUT-PERREGI-9YTM.

           MOVE 5537-SAI-CET-AM            TO WRK-COD-CET-N.
           MOVE WRK-COD-CET                TO WRK-OUT-CETAM-9YTM.
           MOVE 5537-SAI-CET-AA            TO WRK-COD-CET-N.
           MOVE WRK-COD-CET                TO WRK-OUT-CETAA-9YTM.

           MOVE 5537-SAI-CPRODT         TO WRK-5575-CPRODT.
           MOVE 5537-SAI-CSPROD-DESC-COML
                                        TO WRK-5575-CSPROD-DESC-COML.
           MOVE 5537-SAI-CCNPJ-CPF      TO WRK-5575-CCNPJ-CPF.
           MOVE 5537-SAI-CFLIAL-CNPJ    TO WRK-5575-CFLIAL-CNPJ.
           MOVE 5537-SAI-CCTRL-CNPJ-CPF TO WRK-5575-CCTRL-CNPJ-CPF.

           PERFORM 2111-10-VERIFICAR-CET.

      *----------------------------------------------------------------*
       2111-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     VERIFICA A EXIBICAO DO VALOR DO CET                        *
      *----------------------------------------------------------------*
       2111-10-VERIFICAR-CET           SECTION.
      *----------------------------------------------------------------*

           MOVE 'DCOM5575'             TO WRK-MODULO.

           CALL WRK-MODULO             USING WRK-5575-ARGUMENTOS-ENTRADA
                                             WRK-5575-AREA-RETORNO
                                             ERRO-AREA
                                             WRK-SQLCA.

           IF  WRK-5575-CD-RETORNO     NOT EQUAL ZEROS
               MOVE WRK-5575-MENSAGEM  TO WRK-OUT-MENSA-9YTM
           END-IF.

      *----------------------------------------------------------------*
       2111-10-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     CONSULTA DADOS DA OPERACAO                                 *
      *----------------------------------------------------------------*
       2120-OBTER-OPERACAO             SECTION.
      *----------------------------------------------------------------*

           INITIALIZE 4174-ENTRADA-ROTEADOR
                      4174-SAIDA-ROTEADOR.

           MOVE WRK-COMU-DANO          TO 4174-ENT-DANO-OPER-DESC.
           MOVE WRK-COMU-NSEQ          TO 4174-ENT-NSEQ-OPER-DESC.
           MOVE 'DCOM4174'             TO WRK-MODULO.

           CALL WRK-MODULO             USING 4174-ENTRADA-ROTEADOR
                                             4174-SAIDA-ROTEADOR
                                             ERRO-AREA
                                             WRK-SQLCA.

           EVALUATE 4174-SAI-COD-RETORNO
               WHEN '0000'
                    PERFORM 2121-FORMATAR-TELA-INICIAL

               WHEN '0099'
                    MOVE 'DB2'                TO ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                    MOVE 4174-SAI-MSG-RETORNO TO WRK-MSG-AUX
                    PERFORM 2122-RETORNAR-CHAMADOR
           END-EVALUATE.

      *----------------------------------------------------------------*
       2120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     FORMATA TELA INICIAL COM DADOS RETORNADOS DE DCOM4174      *
      *----------------------------------------------------------------*
       2121-FORMATAR-TELA-INICIAL      SECTION.
      *----------------------------------------------------------------*

VANS       MOVE 4174-SAI-IPSSOA-DESC-COML(1:40) TO WRK-OUT-NOMECL-9YTM.

4S2511     IF 4174-SAI-CCNPJ-CPF    EQUAL SPACES OR
4S2511        4174-SAI-CCNPJ-CPF    EQUAL LOW-VALUES OR
4S2511        4174-SAI-CFLIAL-CNPJ  EQUAL SPACES OR
4S2511        4174-SAI-CFLIAL-CNPJ  EQUAL LOW-VALUES
4S2511         MOVE SPACES          TO WRK-OUT-CNPJCPF-9YTM
4S2511     ELSE
4S2511         STRING 4174-SAI-CCNPJ-CPF(1:3)
4S2511         '.'    4174-SAI-CCNPJ-CPF(4:3)
4S2511         '.'    4174-SAI-CCNPJ-CPF(7:3)
4S2511         '/'    4174-SAI-CFLIAL-CNPJ
4S2511         '-'    4174-SAI-CCTRL-CNPJ-CPF
4S2511         DELIMITED BY SIZE              INTO WRK-OUT-CNPJCPF-9YTM
4S2511     END-IF

           MOVE 4174-SAI-CBCO                 TO WRK-BANCO-AUX.
           MOVE 4174-SAI-CAG-BCRIA            TO WRK-AGENC-AUX.
           MOVE 4174-SAI-CCTA-BCRIA-CLI       TO WRK-CONTA-AUX.
           MOVE WRK-BCOAGECTA-AUX             TO WRK-OUT-BCAGCT-9YTM.
           MOVE 4174-SAI-CPRODT               TO WRK-INP-CODPRD-9YTM
                                                 WRK-OUT-CODPRD-9YTM.
           MOVE 4174-SAI-IABREV-PRODT         TO WRK-OUT-DESPRD-9YTM.

           MOVE 4174-SAI-CSPROD-DESC-COML     TO WRK-OUT-CODSUB-9YTM.
           MOVE 4174-SAI-IRSUMO-SPROD-DESC    TO WRK-OUT-DESSUB-9YTM.
           MOVE 4174-SAI-IRSUMO-SIT-DESC      TO WRK-OUT-SITUAC-9YTM.

           IF 4174-SAI-CELMTO-DESC-COML       EQUAL 08
              MOVE 'CONV. GERAL.:'            TO WRK-OUT-DESIDE-9YTM
              MOVE 4174-SAI-CCONTR-CONVE-DESC TO WRK-OUT-NUMERO-9YTM-N
           END-IF.

           IF 4174-SAI-CELMTO-DESC-COML       EQUAL 09
              MOVE 'CONV.CLIENTE:'            TO WRK-OUT-DESIDE-9YTM
              MOVE 4174-SAI-CCONTR-CONVE-DESC TO WRK-OUT-NUMERO-9YTM-N
           END-IF.

           IF 4174-SAI-CELMTO-DESC-COML       EQUAL 10
              MOVE 'CONTR.LIMITE:'            TO WRK-OUT-DESIDE-9YTM
              MOVE 4174-SAI-CCONTR-LIM-DESC   TO WRK-OUT-NUMERO-9YTM-N
           END-IF.

           IF  4174-SAI-NSMULA-OPER-DESC      GREATER ZEROS
               IF  WRK-OUT-DESIDE-9YTM        EQUAL SPACES OR LOW-VALUES
                   MOVE 'SIMULACAO...:'       TO WRK-OUT-DESIDE-9YTM
                   MOVE 4174-SAI-NSMULA-OPER-DESC
                                              TO WRK-OUT-NUMERO-9YTM-N
VANS***        SENAO
VANS***            MOVER 'SIMULACAO...:'       TO WRK-OUT-SIT3-9YTM
VANS***            MOVER 4174-SAI-NSMULA-OPER-DESC
VANS***                                       TO WRK-OUT-SIT4-9YTM-N
               END-IF
           END-IF.

VANS       MOVE 4174-SAI-VLIQ-OPER-DESC    TO WRK-VALOR-LIBER.
VANS       MOVE WRK-VALOR-LIBER            TO WRK-VALOR-DESCOMP.
VANS       MOVE WRK-VALOR-DESCOMP-R        TO WRK-VALOR-TELA-N.
VANS       MOVE WRK-VALOR-TELA             TO WRK-OUT-VLRLIBE-9YTM.
VANS
           MOVE 4174-SAI-VIOF-OPER-DESC    TO WRK-VALOR-IOF.
VANS       MOVE WRK-VALOR-IOF              TO WRK-VALOR-DESCOMP.
VANS       MOVE WRK-VALOR-DESCOMP-R        TO WRK-VALOR-TELA-N.
           MOVE WRK-VALOR-TELA             TO WRK-OUT-TRIBUTOS-9YTM.

           MOVE 4174-SAI-VTARIF-REG-TITLO  TO WRK-VALOR-TAC-TITULOS.
           MOVE 4174-SAI-VTAC-OPER-DESC    TO WRK-VALOR-TAC-OPERACAO.

           COMPUTE WRK-VALOR-TOT-TARIFAS   = WRK-VALOR-TAC-TITULOS
                                           + WRK-VALOR-TAC-OPERACAO.

VANS       MOVE WRK-VALOR-TOT-TARIFAS      TO WRK-VALOR-DESCOMP.
VANS       MOVE WRK-VALOR-DESCOMP-R        TO WRK-VALOR-TELA-N.
           MOVE WRK-VALOR-TELA             TO WRK-OUT-TARIFAS-9YTM.

           COMPUTE WRK-VALOR-TOTAL         = WRK-VALOR-TOT-TARIFAS
                                           + WRK-VALOR-IOF.

VANS       MOVE WRK-VALOR-TOTAL            TO WRK-VALOR-DESCOMP.
VANS       MOVE WRK-VALOR-DESCOMP-R        TO WRK-VALOR-TELA-N.
           MOVE WRK-VALOR-TELA             TO WRK-OUT-TOTAL-9YTM.

VANS       MOVE 4174-SAI-VLIQ-OPER-DESC    TO WRK-VALOR-LIQUI.
VANS       COMPUTE WRK-VALOR-LIQUI         = WRK-VALOR-LIQUI
VANS                                       + WRK-VALOR-TOTAL.
VANS       MOVE WRK-VALOR-LIQUI            TO WRK-VALOR-DESCOMP.
VANS       MOVE WRK-VALOR-DESCOMP-R        TO WRK-VALOR-TELA-N.
VANS       MOVE WRK-VALOR-TELA             TO WRK-OUT-VLRLIQU-9YTM.
VANS
VANS       IF  WRK-VALOR-LIQUI             GREATER ZEROS
VANS           COMPUTE WRK-PERC            = (WRK-VALOR-LIBER
VANS                                       / WRK-VALOR-LIQUI) * 100
VANS       ELSE
VANS           MOVE ZEROS                  TO WRK-PERC
VANS       END-IF.
VANS       MOVE ZEROS                      TO WRK-PERC-R.
VANS       COMPUTE WRK-PERC-R ROUNDED      = WRK-PERC-R
VANS                                       + WRK-PERC.
VANS       MOVE WRK-PERC-R                 TO WRK-PERC-AUX-N.
VANS       MOVE WRK-PERC-AUX               TO WRK-OUT-PERLIBE-9YTM.
VANS
VANS       IF  WRK-VALOR-LIQUI             GREATER ZEROS
VANS           COMPUTE WRK-PERC            = (WRK-VALOR-IOF
VANS                                       / WRK-VALOR-LIQUI) * 100
VANS       ELSE
VANS           MOVE ZEROS                  TO WRK-PERC
VANS       END-IF.
VANS       MOVE ZEROS                      TO WRK-PERC-R.
VANS       COMPUTE WRK-PERC-R ROUNDED      = WRK-PERC-R
VANS                                       + WRK-PERC.
VANS       MOVE WRK-PERC-R                 TO WRK-PERC-AUX-N.
VANS       MOVE WRK-PERC-AUX               TO WRK-OUT-PERTRIB-9YTM.
VANS
VANS       IF  WRK-VALOR-LIQUI             GREATER ZEROS
VANS           COMPUTE WRK-PERC            = (WRK-VALOR-TOT-TARIFAS
VANS                                       / WRK-VALOR-LIQUI) * 100
VANS       ELSE
VANS           MOVE ZEROS                  TO WRK-PERC
VANS       END-IF.
VANS       MOVE ZEROS                      TO WRK-PERC-R.
VANS       COMPUTE WRK-PERC-R ROUNDED      = WRK-PERC-R
VANS                                       + WRK-PERC.
VANS       MOVE WRK-PERC-R                 TO WRK-PERC-AUX-N.
VANS       MOVE WRK-PERC-AUX               TO WRK-OUT-PERTARI-9YTM.
VANS
VANS       IF  WRK-VALOR-LIQUI             GREATER ZEROS
VANS           COMPUTE WRK-PERC            = (WRK-VALOR-TOTAL
VANS                                       / WRK-VALOR-LIQUI) * 100
VANS       ELSE
VANS           MOVE ZEROS                  TO WRK-PERC
VANS       END-IF.
VANS       MOVE ZEROS                      TO WRK-PERC-R.
VANS       COMPUTE WRK-PERC-R ROUNDED      = WRK-PERC-R
VANS                                       + WRK-PERC.
VANS       MOVE WRK-PERC-R                 TO WRK-PERC-AUX-N.
VANS       MOVE WRK-PERC-AUX               TO WRK-OUT-PERTOTA-9YTM.
VANS
           MOVE ZEROS                      TO WRK-VALOR-TELA-N.
VANS       MOVE ZEROS                      TO WRK-PERC-AUX-N.
           MOVE WRK-VALOR-TELA             TO WRK-OUT-SEGUROS-9YTM
                                              WRK-OUT-PGTOSERV-9YTM
                                              WRK-OUT-REGISTROS-9YTM.
VANS       MOVE WRK-PERC-AUX               TO WRK-OUT-PERSEGU-9YTM
VANS                                          WRK-OUT-PERPGTO-9YTM
VANS                                          WRK-OUT-PERREGI-9YTM.

           MOVE 4174-SAI-CET-AM            TO WRK-COD-CET-N.
           MOVE WRK-COD-CET                TO WRK-OUT-CETAM-9YTM.
           MOVE 4174-SAI-CET-AA            TO WRK-COD-CET-N.
           MOVE WRK-COD-CET                TO WRK-OUT-CETAA-9YTM.

           MOVE 4174-SAI-CPRODT         TO WRK-5575-CPRODT.
           MOVE 4174-SAI-CSPROD-DESC-COML
                                        TO WRK-5575-CSPROD-DESC-COML.
           MOVE 4174-SAI-CCNPJ-CPF      TO WRK-5575-CCNPJ-CPF.
           MOVE 4174-SAI-CFLIAL-CNPJ    TO WRK-5575-CFLIAL-CNPJ.
           MOVE 4174-SAI-CCTRL-CNPJ-CPF TO WRK-5575-CCTRL-CNPJ-CPF.

           PERFORM 2111-10-VERIFICAR-CET.

      *----------------------------------------------------------------*
       2121-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     DEVOLVE CONTROLE AO PROGRAMA CHAMADOR                      *
      *----------------------------------------------------------------*
       2122-RETORNAR-CHAMADOR          SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-CHAMADOR-9YTM      TO WRK-TELA.
           MOVE WRK-CHNG                   TO WRK-FUNCAO.

           EVALUATE WRK-INP-CHAMADOR-9YTM
               WHEN 'DCOM0305'
               WHEN 'DCOM0308'
               WHEN 'DCOM0311'
                   INITIALIZE WRK-COMU-AREA-0305

                   MOVE LENGTH OF WRK-COMU-AREA-0305
                                              TO WRK-COMU-LL-0305
                   MOVE 'DCOM3924'            TO WRK-COMU-TRANSACAO-0305

                   MOVE WRK-INP-OCULTOS-9YTM  TO WRK-COMU-DADOS-0305
                   MOVE WRK-COMU-AREA-0305    TO WRK-MENSAGEM

               WHEN 'DCOM0405'
               WHEN 'DCOM0408'
               WHEN 'DCOM0411'
BRA001         WHEN 'DCOM1004'
BRA001         WHEN 'DCOM1005'
BRA001         WHEN 'DCOM1008'
                   INITIALIZE WRK-COMU-AREA-UN

                   MOVE LENGTH OF WRK-COMU-AREA-UN
                                              TO WRK-COMU-LL-UN
                   MOVE 'DCOM3924'            TO WRK-COMU-TRANSACAO-UN

                   MOVE WRK-INP-OCULTOS-9YTM  TO WRK-COMU-DADOS-UN
                   MOVE WRK-COMU-AREA-UN      TO WRK-MENSAGEM


               WHEN 'DCOM0612'
               WHEN 'DCOM0618'
               WHEN 'DCOM0315'
               WHEN 'DCOM0336'
               WHEN 'DCOM0337'
                   INITIALIZE WRK-COMU-AREA-0612
                   MOVE LENGTH OF WRK-COMU-AREA-0612
                                              TO WRK-COMU-LL-0612
                   MOVE 'DCOM3924'            TO WRK-COMU-TRANSACAO-0612
                   MOVE WRK-INP-OCULTOS-9YTM  TO WRK-COMU-DADOS-0612
                   MOVE WRK-COMU-AREA-0612    TO WRK-MENSAGEM

               WHEN 'DCOM0804'
               WHEN 'DCOM0813'
               WHEN 'DCOM0822'
               WHEN 'DCOM0823'

                   INITIALIZE WRK-COMU-AREA-0804
                   MOVE LENGTH OF WRK-COMU-AREA-0804
                                             TO WRK-COMU-LL-0804
                   MOVE 'DCOM3924'           TO WRK-COMU-TRANSACAO-0804
                   MOVE WRK-INP-OCULTOS-9YTM TO WRK-COMU-DADOS-0804
                   MOVE SPACES               TO WRK-COMU-MENSAGEM-2-0804
                   MOVE WRK-COMU-AREA-0804   TO WRK-MENSAGEM

               WHEN OTHER
                   INITIALIZE WRK-COMU-AREA

                   MOVE LENGTH OF WRK-COMU-AREA
                                              TO WRK-COMU-LL
                   MOVE 'DCOM3924'            TO WRK-COMU-TRANSACAO

                   MOVE WRK-INP-OCULTOS-9YTM  TO WRK-COMU-DADOS
                   MOVE WRK-MSG-AUX           TO WRK-COMU-MENSAGEM
                   MOVE WRK-COMU-AREA         TO WRK-MENSAGEM

           END-EVALUATE.

      *----------------------------------------------------------------*
       2122-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     BUSCAR O VALOR DO CET CALCULADO NAS BASES DO MOVIMENTO C3  *
      *----------------------------------------------------------------*
       2130-BUSCAR-CET-MOVIMENTO       SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-5577-ARGUMENTOS-ENTRADA
                      WRK-5577-RETORNO.

           MOVE WRK-COMU-DANO        TO WRK-5577-DANO-OPER-DESC.
           MOVE WRK-COMU-NSEQ        TO WRK-5577-NSEQ-OPER-DESC.

           IF  WRK-TRANSACAO           EQUAL 'DCOM0315'
                                          OR 'DCOM0336'
                                          OR 'DCOM0337'
               IF  WRK-COMU-OPCAO-0612 EQUAL '3'
      * -->   FORMALIZACAO DE ADITAMENTO DE PARCELA
                   MOVE 7              TO WRK-5577-CELMTO-DESC-COML
                   MOVE 25             TO WRK-5577-CEVNTO-DESC-COML
               ELSE
      * -->   FORMALIZACAO DE PRORROGACAO DE PARCELA
                   MOVE 7              TO WRK-5577-CELMTO-DESC-COML
                   MOVE 08             TO WRK-5577-CEVNTO-DESC-COML
               END-IF
           ELSE
               IF  WRK-COMU-OPCAO-0305 NOT EQUAL 1
                   MOVE 6              TO WRK-5577-CELMTO-DESC-COML
                   MOVE 26             TO WRK-5577-CEVNTO-DESC-COML
               END-IF
           END-IF.

           MOVE 'DCOM5577'             TO WRK-MODULO.

           CALL WRK-MODULO             USING WRK-5577-ARGUMENTOS-ENTRADA
                                             WRK-5577-RETORNO
                                             ERRO-AREA
                                             WRK-SQLCA.

           EVALUATE WRK-5577-COD-RETORNO
               WHEN ZEROS
VANS                MOVE WRK-5577-CET-MES   TO WRK-COD-CET-DIMINUI
VANS                MOVE WRK-COD-CET-DIMINUI-R
VANS                                        TO WRK-COD-CET-N
                    MOVE WRK-COD-CET        TO WRK-OUT-CETAM-9YTM
VANS                MOVE WRK-5577-CET-ANO   TO WRK-COD-CET-DIMINUI
VANS                MOVE WRK-COD-CET-DIMINUI-R
VANS                                        TO WRK-COD-CET-N
                    MOVE WRK-COD-CET        TO WRK-OUT-CETAA-9YTM

               WHEN 99
                    MOVE 'DB2'                TO ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                    MOVE WRK-5577-MSG-RETORNO TO WRK-MSG-AUX
                    PERFORM 2122-RETORNAR-CHAMADOR
           END-EVALUATE.

      *----------------------------------------------------------------*
       2130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

WIP001*----------------------------------------------------------------*
WIP001*     RECEBER CONTROLE DCOM3935                                  *
WIP001*----------------------------------------------------------------*
WIP001 2140-RECEBER-CTRL-3935          SECTION.
WIP001*----------------------------------------------------------------*
WIP001
WIP001     MOVE WRK-MENSAGEM           TO WRK-COMU-AREA-3935.
WIP001     MOVE WRK-COMU-CHAMADOR-3935 TO WRK-TRANSACAO.
WIP001
WIP001     IF  WRK-TRANSACAO           EQUAL 'DCOM0305'  OR
WIP001                                       'DCOM0308'  OR
WIP001                                       'DCOM0311'
WIP001         MOVE WRK-COMU-DADOS-CHAMADOR-3935
WIP001                                  TO WRK-COMU-DADOS-0305
WIP001         MOVE WRK-COMU-DANO-0305  TO WRK-COMU-DANO
WIP001         MOVE WRK-COMU-NSEQ-0305  TO WRK-COMU-NSEQ
WIP001         MOVE WRK-COMU-DADOS-0305 TO WRK-INP-OCULTOS-9YTM
WIP001                                     WRK-OUT-OCULTOS-9YTM
WIP001         IF  WRK-COMU-OPCAO-0305  EQUAL 1
WIP001             MOVE 'N'             TO WRK-BUSCA-MOVIMENTO
WIP001         ELSE
WIP001             MOVE 'S'             TO WRK-BUSCA-MOVIMENTO
WIP001         END-IF
WIP001         GO TO 2140-99-FIM
WIP001     END-IF.
WIP001
WIP001     IF  WRK-TRANSACAO           EQUAL 'DCOM0405' OR
WIP001                                       'DCOM0408' OR
WIP001                                       'DCOM0411' OR
WIP001                                       'DCOM1004' OR
WIP001                                       'DCOM1005' OR
WIP001                                       'DCOM1008'
WIP001         MOVE WRK-COMU-DADOS-CHAMADOR-3935
WIP001                                    TO WRK-COMU-DADOS-UN
WIP001         MOVE WRK-COMU-DANO-OPER-UN TO WRK-COMU-DANO
WIP001         MOVE WRK-COMU-NSEQ-OPER-UN TO WRK-COMU-NSEQ
WIP001         MOVE WRK-COMU-DADOS-UN     TO WRK-INP-OCULTOS-9YTM
WIP001                                       WRK-OUT-OCULTOS-9YTM
WIP001         GO TO 2140-99-FIM
WIP001     END-IF.
WIP001
WIP001     IF  WRK-TRANSACAO               EQUAL 'DCOM0612' OR
WIP001                                           'DCOM0618'
WIP001         MOVE WRK-COMU-DADOS-CHAMADOR-3935
WIP001                                     TO WRK-COMU-DADOS-0612
WIP001         MOVE WRK-COMU-DANO-0612     TO WRK-COMU-DANO
WIP001         MOVE WRK-COMU-NSEQ-0612     TO WRK-COMU-NSEQ
WIP001         MOVE WRK-COMU-DADOS-0612    TO WRK-OUT-OCULTOS-9YTM
WIP001         GO TO 2140-99-FIM
WIP001     END-IF.
WIP001
WIP001     IF  WRK-TRANSACAO               EQUAL 'DCOM0315' OR
WIP001                                           'DCOM0336' OR
WIP001                                           'DCOM0337'
WIP001         MOVE WRK-COMU-DADOS-CHAMADOR-3935
WIP001                                     TO WRK-COMU-DADOS-0612
WIP001         MOVE WRK-COMU-DANO-0612     TO WRK-COMU-DANO
WIP001         MOVE WRK-COMU-NSEQ-0612     TO WRK-COMU-NSEQ
WIP001         MOVE WRK-COMU-DADOS-0612    TO WRK-OUT-OCULTOS-9YTM
WIP001         MOVE 'S'                    TO WRK-BUSCA-MOVIMENTO
WIP001         GO TO 2140-99-FIM
WIP001     END-IF.
WIP001
WIP001     IF  WRK-TRANSACAO               EQUAL 'DCOM0804' OR
WIP001                                           'DCOM0813' OR
WIP001                                           'DCOM0822' OR
WIP001                                           'DCOM0823'
WIP001         MOVE WRK-COMU-DADOS-CHAMADOR-3935
WIP001                                        TO WRK-COMU-DADOS-0804
WIP001         MOVE WRK-COMU-DANO-OPER-2-0804 TO WRK-COMU-DANO
WIP001         MOVE WRK-COMU-NSEQ-OPER-2-0804 TO WRK-COMU-NSEQ
WIP001         MOVE WRK-COMU-DADOS-0804       TO WRK-OUT-OCULTOS-9YTM
WIP001         GO TO 2140-99-FIM
WIP001     END-IF.
WIP001
WIP001     MOVE WRK-COMU-DADOS-CHAMADOR-3935
WIP001                                     TO WRK-COMU-DADOS.
WIP001     MOVE WRK-COMU-DADOS             TO WRK-INP-OCULTOS-9YTM
WIP001                                        WRK-OUT-OCULTOS-9YTM.
WIP001
WIP001*----------------------------------------------------------------*
WIP001 2140-99-FIM.                    EXIT.
WIP001*----------------------------------------------------------------*
WIP001
      *----------------------------------------------------------------*
      *     PROCESSAMENTO DA MENSAGEM ENVIADA PELA TELA DCOM9YTM       *
      *----------------------------------------------------------------*
       2200-PROCESSAR-DCOM9YTM         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM                TO WRK-INPUT-9YTM.

           PERFORM 2210-DEVOLVER-TELA

           EVALUATE TRUE
               WHEN WRK-INP-PFK-9YTM        EQUAL 'H' OR
                   (WRK-INP-PFK-9YTM        EQUAL '.' AND
                    WRK-INP-COMANDO-9YTM    EQUAL 'PFK01')
                    MOVE WRK-INP-MENSA-9YTM TO WRK-OUT-MENSA-9YTM

               WHEN WRK-INP-PFK-9YTM        EQUAL '3' OR
                   (WRK-INP-PFK-9YTM        EQUAL '.' AND
                    WRK-INP-COMANDO-9YTM    EQUAL 'PFK03')
                    PERFORM 2122-RETORNAR-CHAMADOR

             WHEN  (WRK-INP-PFK-9YTM       EQUAL '.'       AND
                    WRK-INP-COMANDO-9YTM   EQUAL 'PFK04')  OR
                    WRK-INP-PFK-9YTM       EQUAL '4'
                    PERFORM 2215-TRATAR-IMPRESSAO

               WHEN WRK-INP-PFK-9YTM        EQUAL '5' OR
                   (WRK-INP-PFK-9YTM        EQUAL '.' AND
                    WRK-INP-COMANDO-9YTM    EQUAL 'PFK05')
                    PERFORM 2220-RETORNAR-MENU-ROTINAS

               WHEN WRK-INP-PFK-9YTM        EQUAL 'A' OR
                   (WRK-INP-PFK-9YTM        EQUAL '.' AND
                    WRK-INP-COMANDO-9YTM    EQUAL 'PFK10')
                    PERFORM 2240-RETORNAR-MENU-DCOM

WIP001         WHEN WRK-INP-PFK-9YTM        EQUAL 'D' OR
WIP001             (WRK-INP-PFK-9YTM        EQUAL '.' AND
WIP001              WRK-INP-COMANDO-9YTM    EQUAL 'PFK12')
WIP001              PERFORM 2250-TRATAR-PFK12
WIP001
               WHEN WRK-INP-PFK-9YTM        EQUAL '.' AND
                    WRK-INP-COMANDO-9YTM    EQUAL SPACES
                    MOVE WRK-MSG001         TO WRK-OUT-MENSA-9YTM

               WHEN OTHER
                    MOVE WRK-MSG002         TO WRK-OUT-MENSA-9YTM

           END-EVALUATE.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     MANTEM TELA DEVOLVENDO DADOS DE INPUT PARA OUTPUT          *
      *----------------------------------------------------------------*
       2210-DEVOLVER-TELA              SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-OCULTOS-9YTM    TO WRK-OUT-OCULTOS-9YTM.
           MOVE WRK-INP-NOMECL-9YTM     TO WRK-OUT-NOMECL-9YTM.
           MOVE WRK-INP-CNPJCPF-9YTM    TO WRK-OUT-CNPJCPF-9YTM.
           MOVE WRK-INP-BCAGCT-9YTM     TO WRK-OUT-BCAGCT-9YTM.
           MOVE WRK-INP-CODPRD-9YTM     TO WRK-OUT-CODPRD-9YTM.
           MOVE WRK-INP-DESPRD-9YTM     TO WRK-OUT-DESPRD-9YTM.
           MOVE WRK-INP-CODSUB-9YTM     TO WRK-OUT-CODSUB-9YTM.
           MOVE WRK-INP-DESSUB-9YTM     TO WRK-OUT-DESSUB-9YTM.
           MOVE WRK-INP-OPERAC-9YTM     TO WRK-OUT-OPERAC-9YTM.
           MOVE WRK-INP-SITUAC-9YTM     TO WRK-OUT-SITUAC-9YTM.
           MOVE WRK-INP-DESIDE-9YTM     TO WRK-OUT-DESIDE-9YTM.
           MOVE WRK-INP-NUMERO-9YTM     TO WRK-OUT-NUMERO-9YTM.
           MOVE WRK-INP-SIT3-9YTM       TO WRK-OUT-SIT3-9YTM.
           MOVE WRK-INP-SIT4-9YTM       TO WRK-OUT-SIT4-9YTM.
           MOVE WRK-INP-CHAMADOR-9YTM   TO WRK-OUT-CHAMADOR-9YTM.
VANS       MOVE WRK-INP-VLRLIQU-9YTM    TO WRK-OUT-VLRLIQU-9YTM.
VANS       MOVE WRK-INP-VLRLIBE-9YTM    TO WRK-OUT-VLRLIBE-9YTM.
VANS       MOVE WRK-INP-PERLIBE-9YTM    TO WRK-OUT-PERLIBE-9YTM.
           MOVE WRK-INP-TRIBUTOS-9YTM   TO WRK-OUT-TRIBUTOS-9YTM.
VANS       MOVE WRK-INP-PERTRIB-9YTM    TO WRK-OUT-PERTRIB-9YTM.
           MOVE WRK-INP-SEGUROS-9YTM    TO WRK-OUT-SEGUROS-9YTM.
VANS       MOVE WRK-INP-PERSEGU-9YTM    TO WRK-OUT-PERSEGU-9YTM.
           MOVE WRK-INP-TARIFAS-9YTM    TO WRK-OUT-TARIFAS-9YTM.
VANS       MOVE WRK-INP-PERTARI-9YTM    TO WRK-OUT-PERTARI-9YTM.
           MOVE WRK-INP-PGTOSERV-9YTM   TO WRK-OUT-PGTOSERV-9YTM.
VANS       MOVE WRK-INP-PERPGTO-9YTM    TO WRK-OUT-PERPGTO-9YTM.
           MOVE WRK-INP-REGISTROS-9YTM  TO WRK-OUT-REGISTROS-9YTM.
VANS       MOVE WRK-INP-PERREGI-9YTM    TO WRK-OUT-PERREGI-9YTM.
           MOVE WRK-INP-TOTAL-9YTM      TO WRK-OUT-TOTAL-9YTM.
VANS       MOVE WRK-INP-PERTOTA-9YTM    TO WRK-OUT-PERTOTA-9YTM.
           MOVE WRK-INP-CETAM-9YTM      TO WRK-OUT-CETAM-9YTM.
           MOVE WRK-INP-CETAA-9YTM      TO WRK-OUT-CETAA-9YTM.

      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     TRATA A IMPRESSAO VIA PF4                                  *
      *----------------------------------------------------------------*
       2215-TRATAR-IMPRESSAO           SECTION.
      *----------------------------------------------------------------*

           CALL  'BRAD1080'            USING LNK-IO-PCB
                                             WRK-INP-SENHAS-9YTM
                                             WRK-COMU-IMPRESSORA-P3925.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE RETURN-CODE        TO WRK-RETURN-CODE
               MOVE 05                 TO WRK-LOCAL-ERRO
               MOVE WRK-ERRO-BRAD0660  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE ZEROS                  TO WRK-COMU-RETORNO-P3925.
           MOVE WRK-INP-DANO-9YTM      TO WRK-COMU-DANO-OPER-DESC-P3925.
           MOVE WRK-INP-NSEQ-9YTM      TO WRK-COMU-NSEQ-OPER-DESC-P3925.

           IF  WRK-INP-CHAMADOR-9YTM   EQUAL 'DCOM0618'
               MOVE WRK-INP-OCULTOS-9YTM TO WRK-COMU-AREA-0612
               MOVE WRK-COMU-TIMES-AUX-0612 TO WRK-COMU-TIMESTAMP-P3925
           END-IF.

           MOVE WRK-INP-CHAMADOR-9YTM  TO WRK-COMU-CHAMADOR-P3925.

           IF  WRK-INP-CHAMADOR-9YTM   EQUAL 'DCOM0305' OR
               WRK-INP-CHAMADOR-9YTM   EQUAL 'DCOM0308' OR
               WRK-INP-CHAMADOR-9YTM   EQUAL 'DCOM0311'
               MOVE WRK-INP-OCULTOS-9YTM   TO WRK-COMU-AREA-0305
               MOVE WRK-COMU-OPCAO-0305    TO WRK-COMU-OPCAO-P3925
           END-IF.

           MOVE WRK-COD-USER           TO WRK-COMU-USUARIO-P3925.
VANS       MOVE WRK-COD-DEPTO-R        TO WRK-COMU-DEPTO-P3925.

           MOVE 'DCOM3925'             TO WRK-MODULO.

           CALL WRK-MODULO             USING LNK-IO-PCB
                                             LNK-ALT-PCB
                                             COMU-AREA-P3925.

           IF  WRK-COMU-RETORNO-P3925      EQUAL ZEROS
               MOVE 'IMPRESSAO EFETUADA'   TO WRK-OUT-MENSA-9YTM
           ELSE
               IF  WRK-COMU-RETORNO-P3925  EQUAL 2
                   MOVE 'IMPRESSORA INDISPONIVEL'
                                           TO WRK-OUT-MENSA-9YTM
               ELSE
                   MOVE 'APL'          TO ERR-TIPO-ACESSO
                   MOVE 'IMPRESSAO NAO EFETUADA'
                                       TO ERR-TEXTO
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO
               END-IF
           END-IF.

           PERFORM 2210-DEVOLVER-TELA.

      *----------------------------------------------------------------*
       2215-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     RETORNA AO MENU DE ROTINAS DO SENHAS                       *
      *----------------------------------------------------------------*
       2220-RETORNAR-MENU-ROTINAS      SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-COMU-AREA.

           MOVE LENGTH OF WRK-COMU-AREA TO WRK-COMU-LL.
           MOVE WRK-CHNG                TO WRK-FUNCAO.
           MOVE 'DCOM3924'              TO WRK-TELA
                                           WRK-COMU-TRANSACAO.
           MOVE '5'                     TO WRK-COMU-PFK.
           MOVE WRK-COMU-AREA           TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     RETORNA AO MENU DCOM                                       *
      *----------------------------------------------------------------*
       2240-RETORNAR-MENU-DCOM         SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-COMU-AREA.

           MOVE LENGTH OF WRK-COMU-AREA TO WRK-COMU-LL.
           MOVE WRK-CHNG                TO WRK-FUNCAO.
           MOVE 'DCOM3924'              TO WRK-TELA
                                           WRK-COMU-TRANSACAO.
           MOVE 'A'                     TO WRK-COMU-PFK.
           MOVE WRK-COMU-AREA           TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2240-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

WIP001*----------------------------------------------------------------*
WIP001*     TRATAR PFK12                                               *
WIP001*----------------------------------------------------------------*
WIP001 2250-TRATAR-PFK12               SECTION.
WIP001*----------------------------------------------------------------*
WIP001
WIP001     INITIALIZE WRK-COMU-AREA-3935.
WIP001
WIP001     MOVE LENGTH OF WRK-COMU-AREA-3935
WIP001                                 TO WRK-COMU-LL-3935.
WIP001     MOVE 'DCOM3924'             TO WRK-COMU-TRANS-3935.
WIP001
WIP001     MOVE WRK-INP-DANO-9YTM      TO WRK-COMU-DANO-3935.
WIP001     MOVE WRK-INP-NSEQ-9YTM      TO WRK-COMU-NSEQ-3935.
WIP001     MOVE WRK-INP-CHAMADOR-9YTM  TO WRK-COMU-CHAMADOR-3935.
WIP001     MOVE SPACES                 TO WRK-COMU-MENSAGEM-3935.
WIP001     MOVE WRK-INP-OCULTOS-9YTM   TO WRK-COMU-DADOS-CHAMADOR-3935.
WIP001
WIP001     MOVE 'DCOM3935'             TO WRK-TELA.
WIP001     MOVE WRK-CHNG               TO WRK-FUNCAO.
WIP001     MOVE WRK-COMU-AREA-3935     TO WRK-MENSAGEM.
WIP001
WIP001*----------------------------------------------------------------*
WIP001 2250-99-FIM.                    EXIT.
WIP001*----------------------------------------------------------------*
WIP001
      *----------------------------------------------------------------*
      *     PROCESSAMENTO DA BRAD0660 - INSERCAO DA TELA DCOM9YTM      *
      *----------------------------------------------------------------*
       2300-PROCESSAR-BRAD0660         SECTION.
      *----------------------------------------------------------------*

           MOVE LENGTH                 OF WRK-OUTPUT-9YTM
                                       TO WRK-OUT-LL-9YTM
                                          WRK-660-LL-MENSAG-9YTM.
           MOVE LENGTH                 OF WRK-660-DCOM9YTM
                                       TO WRK-660-LL-AREA-9YTM.

           CALL 'BRAD0660'             USING WRK-OUTPUT-9YTM
                                             WRK-660-DCOM9YTM.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE RETURN-CODE        TO WRK-RETURN-CODE
               MOVE 10                 TO WRK-LOCAL-ERRO
               MOVE WRK-ERRO-BRAD0660  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-ISRT               TO WRK-FUNCAO.
           MOVE WRK-OUTPUT-9YTM        TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     PROCESSAMENTO DE ERRO                                      *
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           IF  ERR-MODULO              NOT EQUAL SPACES AND LOW-VALUES
               MOVE 'DCOM3924'         TO ERR-PGM
           ELSE
               MOVE 'SENHAS02'         TO ERR-PGM
               MOVE 'DCOM3924'         TO ERR-MODULO
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
