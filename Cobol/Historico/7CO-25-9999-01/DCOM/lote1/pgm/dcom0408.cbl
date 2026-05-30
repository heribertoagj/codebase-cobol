      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID.     DCOM0408.
       AUTHOR.         ADRIANO RICARDO BELMONT.
      *================================================================*
      *                          A L T R A N                           *
      *----------------------------------------------------------------*
      *     PROGRAMA....: DCOM0408                                     *
      *     PROGRAMADORA: ADRIANO RICARDO BELMONT                      *
      *     ANALISTA....: DANILO                                       *
      *     DATA........: 01/02/2007                                   *
      *----------------------------------------------------------------*
      *     OBJETIVO....: CANCELAR LIBERACAO - DEMONSTRATIVO           *
      *----------------------------------------------------------------*
      *     TELAS.......:                                              *
      *     DCOMDHTM  -   LIBERAR OPERACAO - DEMONSTRATIVO             *
      *----------------------------------------------------------------*
      *     INC'S.......:                                              *
      *     I#BRAD7C - AREA DE ERRO DA BRAD7100                        *
      *----------------------------------------------------------------*
      *     MODULOS:                                                   *
      *     BRAD0660 - INSERIR CARACTERES DE NULL NA MENSAGEM DE SAIDA *
      *     BRAD7100 - TRATAMENTO DE ERROS QUANDO PROGRAMA INVALIDO    *
      *     POOL5005 - CONTROLE DO TRANSITO DE MENSAGENS NO IMS/DC     *
      *     BRAD7600 - FORNECE DATA E HORA CORRENTE DO SISTEMA         *
      *                                                                *
      *     CHAMA:                                                     *
      *     DCOM0214 - CONSULTA LIBERACAO - RESUMO DA ANALISE          *
      *     DCOM0222 - CONSULTA LIBERACAO - RESUMO DA ANALISE          *
      *                                                                *
      *----------------------------------------------------------------*
      *     NAVEGACAO...:                                              *
      *     CHAMADO POR.: DCOM0402           CHAMA: DCOM0214           *
      *                   DCOM0406                  DCOM0222           *
      *                   DCOM0407                                     *
      *================================================================*
DTS001*                    ALTERACAO - DTS CONSULTING                  *
DTS001*----------------------------------------------------------------*
DTS001*    PROGRAMADORA:   LUCIANDRA SILVEIRA - DTS CONSULTING         *
DTS001*    ANALISTA....:   LUCIANDRA SILVEIRA - DTS CONSULTING         *
DTS001*    ANALISTA DTS:   VERA MARIA POLLINI - BRADESCO - GRUPO 70    *
DTS001*    DATA........:   03/02/2010                                  *
DTS001*----------------------------------------------------------------*
DTS001*    OBJETIVO....:   EXECUTAR ROLLBACK CASO OCORRA ERRO NO MOMEN-*
DTS001*                    TO DO CANCELAMENTO DA LIBERACAO DA OPERACAO *
DTS001*================================================================*
DTS002*                    ALTERACAO  -  DTS  CONSULTING               *
DTS002*----------------------------------------------------------------*
DTS002*    PROGRAMADORA:   LUCIANDRA SILVEIRA     - DTS CONSULTING     *
DTS002*    ANALISTA....:   LUCIANDRA SILVEIRA     - DTS CONSULTING     *
DTS002*    ANALISTA DDS:   LUIS EDUARDO DE MORAES - GRUPO 70           *
DTS002*    DATA........:   06/09/2011                                  *
DTS002*----------------------------------------------------------------*
DTS002*     OBJETIVO....: CORRIGIR AREA DE COMUNICACAO NA CHAMADA AO   *
DTS002*                   DCOM0214 (RO 3089375).                       *
DTS002*================================================================*
BRQ001*                   ALTERACAO  -  BRQ DIGITAL SOLUTIONS          *
BRQ001*----------------------------------------------------------------*
BRQ001*    PROGRAMADOR.:  FABIO AUGUSTO FINK - BRQ DIGITAL SOLUTIONS   *
BRQ001*    ANALISTA BRQ:  FABIO AUGUSTO FINK - BRQ DIGITAL SOLUTIONS   *
BRQ001*    ANALISTA DS.:  RICARDO JAMMAL     - BRADESCO / GP.39        *
BRQ001*    DATA........:  26/11/2018                                   *
BRQ001*----------------------------------------------------------------*
BRQ001*    OBJETIVO....:  BRQ 18/0456 - BORDERO ELETRONICO.            *
BRQ001*      INCLUIR TRATAMENTO PARA CAMPO INDICADOR DE LIBERACAO      *
BRQ001*      NO CANAL.                                                 *
BRQ001*================================================================*
7C0922*================================================================*
7C0922*                   ALTERACAO  -  7COMM                          *
7C0922*----------------------------------------------------------------*
7C0922*    PROGRAMADOR.:  EDSON MATSUMOTO    - 7COMM                   *
7C0922*    ANALISTA    :  EDSON MATSUMOTO    - 7COMM                   *
7C0922*    ANALISTA DS.:  PAULO ROBERTO SARTORATO - BRADESCO           *
7C0922*    DATA........:  12/09/2022                                   *
7C0922*----------------------------------------------------------------*
7C0922*    OBJETIVO....:  PROJETO 794794 - CANCELAR CONTRATO (COR002)  *
7C0922*                   NAO PERMTIR CANCELAR CONTRATO NPR SE JA TIVER*
7C0922*                   RETORNO DA COR0005R1                         *
7C0922*================================================================*

      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *================================================================*
       DATA                             DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *           *** DCOM0408 - INICIO DA AREA DE WORKING ***         *
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *               *** VARIAVEIS AUXILIARES ***                     *
      *----------------------------------------------------------------*

       01  WRK-AUXILIARES.
           05  WRK-SQLCA               PIC  X(136)       VALUE SPACES.
           05  WRK-MODULO              PIC  X(008)       VALUE SPACES.
           05  WRK-INCONSIS            PIC  X(001)       VALUE SPACES.
           05  WRK-FLAG-ERRO           PIC  X(001)       VALUE 'N'.
           05  WRK-MENSAGEM-ERRO       PIC  X(079)       VALUE SPACES.

      *----------------------------------------------------------------*
      *             *** AREAS AUXILIARES PARA POOL5005 ***             *
      *----------------------------------------------------------------*

       01  WRK-POOL5005.
           05  WRK-GU                  PIC  X(004)    VALUE 'GU  '.
           05  WRK-CHNG                PIC  X(004)    VALUE 'CHNG'.
           05  WRK-ISRT                PIC  X(004)    VALUE 'ISRT'.
DTS001     05  WRK-ROLB                PIC  X(004)    VALUE 'ROLB'.
           05  WRK-FUNCAO              PIC  X(004)    VALUE SPACES.
           05  WRK-TELA                PIC  X(008)    VALUE SPACES.
           05  WRK-VERSAO              PIC  X(006)    VALUE 'VRS002'.
           05  WRK-COD-USER            PIC  X(007)    VALUE SPACES.
           05  WRK-COD-USER-R          REDEFINES
               WRK-COD-USER            PIC  9(007).
           05  WRK-COD-DEPTO.
               10  FILLER              PIC  X(002)    VALUE SPACES.
               10  WRK-COD-DEPTO-N     PIC  9(004)    VALUE ZEROS.

           05  WRK-MENSAGEM.
               10  FILLER              PIC  X(004)    VALUE SPACES.
               10  WRK-TRANSACAO       PIC  X(008)    VALUE SPACES.
               10  FILLER              PIC  X(1988)   VALUE SPACES.

      *---------------------------------------------------------------*
      *              *** AREA DE ERRO DA BRAD0660 *****
      *---------------------------------------------------------------*

       01  WRK-ERRO-BRAD0660.
           05  FILLER                  PIC  X(045)    VALUE
               '* RETURN-CODE DIFERENTE DE ZEROS NA BRAD0660'.
           05  FILLER                  PIC  X(012)    VALUE
               '- RET.CODE ='.
           05  WRK-RETURN-CODE         PIC  9(002)    VALUE ZEROS.
           05  FILLER                  PIC  X(011)    VALUE
               ' - LOCAL ='.
           05  WRK-LOCAL-ERRO          PIC  9(002)    VALUE ZEROS.
           05  FILLER                  PIC  X(002)    VALUE '*'.

      *---------------------------------------------------------------*
      *        *** AREA DA TELA DCOMDHTM  -  INPUT ***                *
      *---------------------------------------------------------------*

       01  INPUT-DHTM.
           05  FILLER                    PIC  X(018)    VALUE SPACES.
           05  INP-PFK                   PIC  X(001)    VALUE SPACES.
           05  INP-SENHAS                PIC  X(037)    VALUE SPACES.
           05  INP-COMANDO               PIC  X(068)    VALUE SPACES.
           05  INP-DADOS.
               10 INP-FLAGENT            PIC  X(001)    VALUE SPACES.
               10 INP-TIPODES            PIC  9(003)    VALUE ZEROS.
               10 INP-TRANSAC            PIC  X(008)    VALUE SPACES.
               10 INP-TMSTAMP            PIC  X(026)    VALUE SPACES.
               10 INP-NOME               PIC  X(040)    VALUE SPACES.
               10 INP-CNPJCPF            PIC  X(019)    VALUE SPACES.
               10 INP-CAMPO2             PIC  X(023)    VALUE SPACES.
               10 INP-PRODUTO            PIC  9(003)    VALUE ZEROS.
               10 INP-DPRODUT            PIC  X(012)    VALUE SPACES.
               10 INP-SPRODUT            PIC  9(003)    VALUE ZEROS.
               10 INP-DSPRODU            PIC  X(012)    VALUE SPACES.
7C0922         10 WRK-INP-OPERAC-X.
.                 15 WRK-INP-OPERAC      PIC  9(013)    VALUE ZEROS.
.              10 WRK-INP-OPERAC-R     REDEFINES WRK-INP-OPERAC-X.
.                 15  WRK-INP-OPERA-DANO PIC  9(04).
7C0922            15  WRK-INP-OPERA-NSEQ PIC  9(09).
               10 INP-SITUAC             PIC  X(015)    VALUE SPACES.
               10 INP-CAMPO3             PIC  X(013)    VALUE SPACES.
               10 INP-CAMPO4             PIC  9(009)    VALUE ZEROS.
               10 INP-DTOPER             PIC  X(010)    VALUE SPACES.
               10 INP-DTVCTOF            PIC  X(010)    VALUE SPACES.
               10 INP-VLROPER-X          PIC  X(014).
               10 INP-VLROPER-E REDEFINES INP-VLROPER-X
                                         PIC   ZZZ.ZZZ.ZZ9,99.
               10 INP-TPOTAXA            PIC  X(015)    VALUE SPACES.
               10 INP-VLTAROP.
                  15 INP-VLTAROP-X       PIC  X(014).
                  15 INP-VLTAROP-E REDEFINES INP-VLTAROP-X
                                         PIC   ZZZ.ZZZ.ZZ9,99.
               10 INP-TAXAJUR.
                  15 INP-TAXAJUR-X       PIC  X(009).
                  15 INP-TAXAJUR-E REDEFINES INP-TAXAJUR-X
                                         PIC  ZZ9,99999.
               10 INP-NMJUROS            PIC  X(004)    VALUE SPACES.
               10 INP-VLTRFTT.
                  15 INP-VLTRFTT-X       PIC  X(014).
                  15 INP-VLTRFTT-E REDEFINES INP-VLTRFTT-X
                                         PIC  ZZZ.ZZZ.ZZ9,99.
               10 INP-VLRJUR.
                  15 INP-VLRJUR-X        PIC  X(014).
                  15 INP-VLRJUR-E REDEFINES INP-VLRJUR-X
                                         PIC  ZZZ.ZZZ.ZZ9,99.
               10 INP-PRZMED             PIC  9(003)    VALUE ZEROS.
               10 INP-VLRIOF.
                  15 INP-VLRIOF-X        PIC  X(014).
                  15 INP-VLRIOF-E  REDEFINES  INP-VLRIOF-X
                                         PIC  ZZZ.ZZZ.ZZ9,99.
               10 INP-QTDETIT.
                  15 INP-QTDETIT-N       PIC  ZZ.ZZ9.
               10 INP-VLRLIQD.
                  15 INP-VLRLIQD-X       PIC  X(014).
                  15 INP-VLRLIQD-E  REDEFINES  INP-VLRLIQD-X
                                         PIC  ZZZ.ZZZ.ZZ9,99.
BRQ001         10 INP-LIBMID             PIC  X(003)    VALUE SPACES.
               10 INP-GARANTI            PIC  X(020)    VALUE SPACES.
               10 INP-AUTRZTE            PIC  X(050)    VALUE SPACES.
               10 INP-DESCR.
                   15 INP-DESCR1         PIC  X(053)    VALUE SPACES.
                   15 INP-DESCR2         PIC  X(075)    VALUE SPACES.
               10 INP-JURMAXI.
                  15 INP-JURMAXI-X       PIC  X(006).
                  15 INP-JURMAXI-E  REDEFINES  INP-JURMAXI-X
                                         PIC  999,99.
               10 INP-VLRMAXI.
                  15 INP-VLRMAXI-X       PIC  X(017).
                  15 INP-VLRMAXI-E  REDEFINES  INP-VLRMAXI-X
                                         PIC  ZZ.ZZZ.ZZZ.ZZ9,99.
               10 INP-JRLIBER.
                  15 INP-JRLIBER-X       PIC  X(006).
                  15 INP-JRLIBER-E  REDEFINES  INP-JRLIBER-X
                                         PIC  999,99.
               10 INP-VLLIBER.
                  15 INP-VLLIBER-X       PIC  X(017).
                  15 INP-VLLIBER-E  REDEFINES  INP-VLLIBER-X
                                         PIC  ZZ.ZZZ.ZZZ.ZZ9,99.
               10 INP-TIPOLIB            PIC  X(001)    VALUE SPACES.
           05  INP-MENSAG                PIC  X(079)    VALUE SPACES.

      *----------------------------------------------------------------*
      *        *** AREA DA TELA DCOMDHTM  -  OUTPUT ***                *
      *----------------------------------------------------------------*

       01  OUTPUT-DHTM.
           05  OUT-LL                    PIC S9(004) COMP VALUE +774.
           05  OUT-ZZ                    PIC S9(004) COMP VALUE +0.
           05  OUT-SENHAS                PIC  X(037)      VALUE SPACES.
           05  OUT-COMANDO               PIC  X(068)      VALUE SPACES.
           05  OUT-DADOS.
               10 OUT-FLAGENT            PIC  X(001)    VALUE SPACES.
               10 OUT-TIPODES            PIC  9(003)    VALUE ZEROS.
               10 OUT-TRANSAC            PIC  X(008)    VALUE SPACES.
               10 OUT-TMSTAMP            PIC  X(026)    VALUE SPACES.
               10 OUT-NOME               PIC  X(040)    VALUE SPACES.
               10 OUT-CNPJCPF            PIC  X(019)    VALUE SPACES.
               10 OUT-CAMPO2             PIC  X(023)    VALUE SPACES.
               10 OUT-PRODUTO            PIC  9(003)    VALUE ZEROS.
               10 OUT-DPRODUT            PIC  X(012)    VALUE SPACES.
               10 OUT-SPRODUT            PIC  9(003)    VALUE ZEROS.
               10 OUT-DSPRODU            PIC  X(012)    VALUE SPACES.
               10 OUT-OPERAC             PIC  9(013)    VALUE ZEROS.
               10 OUT-SITUAC             PIC  X(015)    VALUE SPACES.
               10 OUT-CAMPO3             PIC  X(013)    VALUE SPACES.
               10 OUT-CAMPO4-X.
                  15 OUT-CAMPO4          PIC  9(009)    VALUE ZEROS.
               10 OUT-DTOPER             PIC  X(010)    VALUE SPACES.
               10 OUT-DTVCTOF            PIC  X(010)    VALUE SPACES.
               10 OUT-VLROPER.
                  15 OUT-VLROPER-N       PIC   ZZZ.ZZZ.ZZ9,99.
               10 OUT-TPOTAXA            PIC  X(015)    VALUE SPACES.
               10 OUT-VLTAROP.
                  15 OUT-VLTAROP-N       PIC   ZZZ.ZZZ.ZZ9,99.
               10 OUT-TAXAJUR.
                  15 OUT-TAXAJUR-N       PIC  ZZ9,99999.
               10 OUT-NMJUROS            PIC  X(004)    VALUE SPACES.
               10 OUT-VLTRFTT.
                  15 OUT-VLTRFTT-N       PIC  ZZZ.ZZZ.ZZ9,99.
               10 OUT-VLRJUR.
                  15 OUT-VLRJUR-N        PIC  ZZZ.ZZZ.ZZ9,99.
               10 OUT-PRZMED             PIC  9(003)    VALUE ZEROS.
               10 OUT-VLRIOF.
                  15 OUT-VLRIOF-N        PIC  ZZZ.ZZZ.ZZ9,99.
               10 OUT-QTDETIT.
                  15 OUT-QTDETIT-N       PIC  ZZ.ZZ9.
               10 OUT-VLRLIQD.
                  15 OUT-VLRLIQD-N       PIC  ZZZ.ZZZ.ZZ9,99.
BRQ001         10 OUT-LIBMID             PIC  X(003)    VALUE SPACES.
               10 OUT-GARANTI            PIC  X(020)    VALUE SPACES.
               10 OUT-AUTRZTE            PIC  X(050)    VALUE SPACES.
               10 OUT-DESCR.
                   15 OUT-DESCR1         PIC  X(053)    VALUE SPACES.
                   15 OUT-DESCR2         PIC  X(075)    VALUE SPACES.
               10 OUT-JURMAXI.
                  15 OUT-JURMAXI-N       PIC  ZZ9,99.
               10 OUT-VLRMAXI.
                  15 OUT-VLRMAXI-N       PIC  ZZ.ZZZ.ZZZ.ZZ9,99.
               10 OUT-JRLIBER.
                  15 OUT-JRLIBER-N       PIC  ZZ9,99.
               10 OUT-VLLIBER.
                  15 OUT-VLLIBER-N       PIC  ZZ.ZZZ.ZZZ.ZZ9,99.
               10 OUT-TIPOLIB            PIC  X(001)    VALUE SPACES.
           05  OUT-MENSAG                PIC  X(079)    VALUE SPACES.

      /---------------------------------------------------------------*
      *  AREA DA BRAD0660 COM MASCARA DA TELA DCOMDHTM - SEGMENTO 00  *
      *---------------------------------------------------------------*

       01  660-DCOMDHTM.
           05  660-LL-AREA-DHTM        PIC  9(04)  COMP  VALUE 0086.
           05  660-LL-MENSAGEM-DHTM    PIC  9(04)  COMP  VALUE 0777.
           05  660-SENHAS-DHTM         PIC  9(04)  COMP  VALUE 0037.
           05  660-COMANDO-DHTM        PIC  9(04)  COMP  VALUE 0068.
           05  660-FLAGENT-DHTM        PIC  9(04)  COMP  VALUE 0001.
           05  660-TIPODES             PIC  9(04)  COMP  VALUE 0003.
           05  660-TRANSAC-DHTM        PIC  9(04)  COMP  VALUE 0008.
           05  660-TMSTAMP-DHTM        PIC  9(04)  COMP  VALUE 0026.
           05  660-NOME-DHTM           PIC  9(04)  COMP  VALUE 0040.
           05  660-CNPJCPF-DHTM        PIC  9(04)  COMP  VALUE 0019.
           05  660-CAMPO2-DHTM         PIC  9(04)  COMP  VALUE 0023.
           05  660-PRODUTO-DHTM        PIC  9(04)  COMP  VALUE 0003.
           05  660-DPRODUT-DHTM        PIC  9(04)  COMP  VALUE 0012.
           05  660-SPRODUT-DHTM        PIC  9(04)  COMP  VALUE 0003.
           05  660-DSPRODU-DHTM        PIC  9(04)  COMP  VALUE 0012.
           05  660-OPERAC-DHTM         PIC  9(04)  COMP  VALUE 0013.
           05  660-SITUAC-DHTM         PIC  9(04)  COMP  VALUE 0015.
           05  660-CAMPO3-DHTM         PIC  9(04)  COMP  VALUE 0013.
           05  660-CAMPO4-DHTM         PIC  9(04)  COMP  VALUE 0009.
           05  660-DTOPER-DHTM         PIC  9(04)  COMP  VALUE 0010.
           05  660-DTVCTOF-DHTM        PIC  9(04)  COMP  VALUE 0010.
           05  660-VLROPER-DHTM        PIC  9(04)  COMP  VALUE 0014.
           05  660-TPOTAXA-DHTM        PIC  9(04)  COMP  VALUE 0015.
           05  660-VLTAROP-DHTM        PIC  9(04)  COMP  VALUE 0014.
           05  660-TAXAJUR-DHTM        PIC  9(04)  COMP  VALUE 0009.
           05  660-NMJUROS-DHTM        PIC  9(04)  COMP  VALUE 0004.
           05  660-VLTRFTT-DHTM        PIC  9(04)  COMP  VALUE 0014.
           05  660-VLRJUR-DHTM         PIC  9(04)  COMP  VALUE 0014.
           05  660-PRZMED-DHTM         PIC  9(04)  COMP  VALUE 0003.
           05  660-VLRIOF-DHTM         PIC  9(04)  COMP  VALUE 0014.
           05  660-QTDETIT-DHTM        PIC  9(04)  COMP  VALUE 0006.
           05  660-VLRLIQD-DHTM        PIC  9(04)  COMP  VALUE 0014.
BRQ001     05  660-LIBMID-DHTM         PIC  9(04)  COMP  VALUE 0003.
           05  660-GARANTI-DHTM        PIC  9(04)  COMP  VALUE 0020.
           05  660-AUTRZTE-DHTM        PIC  9(04)  COMP  VALUE 0050.
           05  660-DESCR1-DHTM         PIC  9(04)  COMP  VALUE 0053.
           05  660-DESCR2-DHTM         PIC  9(04)  COMP  VALUE 0075.
           05  660-JURMAXI-DHTM        PIC  9(04)  COMP  VALUE 0006.
           05  660-VLRMAXI-DHTM        PIC  9(04)  COMP  VALUE 0017.
           05  660-JRLIBER-DHTM        PIC  9(04)  COMP  VALUE 0006.
           05  660-VLLIBER-DHTM        PIC  9(04)  COMP  VALUE 0017.
           05  660-TIPOLIB-DHTM        PIC  9(04)  COMP  VALUE 0001.
           05  660-MENSA-DHTM          PIC  9(04)  COMP  VALUE 0079.

      *---------------------------------------------------------------*
      *    AREA DE COMUNICACAO CHAMADO DCOM0402/DCOM0406/DCOM0407     *
      *---------------------------------------------------------------*

       01  COMU-AREA-UN.
           05  COMU-LL-UN              PIC S9(004) COMP  VALUE +297.
           05  COMU-ZZ-UN              PIC S9(004) COMP  VALUE +0.
           05  COMU-TRANCODE-UN.
               10  COMU-TRANSACAO-UN   PIC  X(008)       VALUE SPACES.
               10  FILLER-UN           PIC  X(006)       VALUE SPACES.
               10  COMU-PFK-UN         PIC  X(001)       VALUE SPACES.
           05  COMU-SENHAS-UN          PIC  X(037)       VALUE SPACES.
           05  COMU-COMANDO-UN         PIC  X(068)       VALUE SPACES.
           05  COMU-DADOS-UN.
               10  COMU-OPCAO-UN-X.
                   15  COMU-OPCAO-UN   PIC  9(001)       VALUE ZEROS.
               10  COMU-OPER-UN        PIC  9(013)       VALUE ZEROS.
               10  COMU-AGENCIA-UN     PIC  9(005)       VALUE ZEROS.
               10  COMU-AGENC-UN-X.
                   15  COMU-AGENC-UN   PIC  9(005)       VALUE ZEROS.
               10  COMU-CONTA-UN       PIC  9(013)       VALUE ZEROS.
               10  COMU-CNPJ-UN-X.
FS2511*            15  COMU-CNPJ-UN    PIC  9(009)       VALUE ZEROS.
FS2511             15  COMU-CNPJ-UN    PIC  X(009)       VALUE SPACES.
FS2511*        10  COMU-FILIAL-UN      PIC  9(004)       VALUE ZEROS.
FS2511         10  COMU-FILIAL-UN      PIC  X(004)       VALUE SPACES.
               10  COMU-DIG-UN         PIC  9(002)       VALUE ZEROS.
               10  COMU-DANO-OPER-UN   PIC  9(004)       VALUE ZEROS.
               10  COMU-NSEQ-OPER-UN   PIC  9(009)       VALUE ZEROS.
               10  COMU-TIMESTAMP-UN   PIC  X(026)       VALUE ZEROS.
               10  COMU-MENSAGEM-UN    PIC  X(079)       VALUE SPACES.
               10  COMU-PAGINA-UN      PIC  9(003)       VALUE ZEROS.
PROC01         10  COMU-CHAMADOR-UN    PIC  X(008)       VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(50)      VALUE
           '*** AREA DE COMUNICACAO COM DCOM0222 ***'.
      *----------------------------------------------------------------*

       01  COMU-AREA-0215.
           05  COMU-LL-0215            PIC S9(004) COMP   VALUE ZEROS.
           05  COMU-ZZ-0215            PIC S9(004) COMP   VALUE ZEROS.
           05  COMU-TRANCODE-0215.
               10  COMU-TRANSACAO-0215 PIC  X(008)        VALUE SPACES.
               10  FILLER              PIC  X(006)        VALUE SPACES.
               10  COMU-PFK-0215       PIC  X(001)        VALUE SPACES.
           05  COMU-SENHAS-0215        PIC  X(037)        VALUE SPACES.
           05  COMU-COMANDO-0215       PIC  X(068)        VALUE SPACES.
           05  COMU-DADOS-0215.
               10  COMU-TPOPCAO-0215   PIC  X(001)        VALUE SPACES.
               10  COMU-TIMESTAMP-0215 PIC  X(026)        VALUE SPACES.
               10  COMU-TPDESC-0215    PIC  9(003)        VALUE ZEROS.
               10  COMU-SALVOS-0215.
FS2511*            15  COMU-CNPJCPF-0215   PIC  9(009)    VALUE ZEROS.
FS2511             15  COMU-CNPJCPF-0215   PIC  X(009)    VALUE SPACES.
FS2511*             15  COMU-FILIAL-0215    PIC  9(004)    VALUE ZEROS.
FS2511              15  COMU-FILIAL-0215    PIC  X(004)    VALUE SPACES.
                   15  COMU-CONTROLE-0215  PIC  9(002)    VALUE ZEROS.
                   15  COMU-AGENCIA-0215   PIC  9(005)    VALUE ZEROS.
                   15  COMU-CONTA-0215     PIC  9(013)    VALUE ZEROS.
                   15  COMU-OPERACAO-0215  PIC  9(013)    VALUE ZEROS.
                   15  FILLER REDEFINES    COMU-OPERACAO-0215.
                       20 COMU-DANO-0215   PIC  9(004).
                       20 COMU-NSEQ-0215   PIC  9(009).
                   15  COMU-BORDERO-0215   PIC  9(007)    VALUE ZEROS.
                   15  COMU-LOTE-0215      PIC  9(007)    VALUE ZEROS.
                   15  COMU-CHEQUE-0215    PIC  9(007)    VALUE ZEROS.
                   15  COMU-COMUNICACAO-0215.
                       20  COMU-PAGINA1-0215  PIC  9(004) VALUE ZEROS.
                       20  COMU-TRANSA1-0215  PIC  X(008) VALUE SPACES.
                       20  COMU-PAGINA2-0215  PIC  9(004) VALUE ZEROS.
                       20  COMU-TRANSA2-0215  PIC  X(008) VALUE SPACES.
                       20  COMU-PAGINA3-0215  PIC  9(004) VALUE ZEROS.
                       20  COMU-TRANSA3-0215  PIC  X(008) VALUE SPACES.
                       20  COMU-PAGINA4-0215  PIC  9(004) VALUE ZEROS.
                       20  COMU-TRANSA4-0215  PIC  X(008) VALUE SPACES.
                   15  COMU-COD204-0215       PIC  X(004) VALUE SPACES.
               10  COMU-MENSAGEM-0215         PIC  X(079) VALUE SPACES.

      *----------------------------------------------------------------*
      *        *** AREA DE COMUNICACAO 214 ***                         *
      *----------------------------------------------------------------*
       01  COMU-AREA-0214.
           05  COMU-LL-0214               PIC S9(004) COMP VALUE ZEROS.
           05  COMU-ZZ-0214               PIC S9(004) COMP VALUE ZEROS.
           05  COMU-TRANCODE-0214.
               10  COMU-TRANSACAO-0214      PIC  X(008)   VALUE SPACES.
               10  FILLER                   PIC  X(006)   VALUE SPACES.
               10  COMU-PFK-0214            PIC  X(001)   VALUE SPACES.
           05  COMU-SENHAS-0214             PIC  X(037)   VALUE SPACES.
           05  COMU-COMANDO-0214            PIC  X(068)   VALUE SPACES.
           05  COMU-DADOS-0214.
               10  COMU-TIPO-OPERACAO-0214  PIC  9(01)    VALUE ZEROS.
DTS002         10  COMU-TPDESC-0214         PIC  9(03)    VALUE ZEROS.
               10  COMU-AGENCIA-0214        PIC  9(05)    VALUE ZEROS.
               10  COMU-CONTA-0214          PIC  9(13)    VALUE ZEROS.
FS2511*        10  COMU-CPFCNPJ-0214        PIC  9(09)    VALUE ZEROS.
FS2511         10  COMU-CPFCNPJ-0214        PIC  X(09)    VALUE SPACES.
FS2511*        10  COMU-FILIAL-0214         PIC  9(04)    VALUE ZEROS.
FS2511         10  COMU-FILIAL-0214         PIC  X(04)    VALUE SPACES.
               10  COMU-CONTROLE-0214       PIC  9(02)    VALUE ZEROS.
               10  COMU-OPERACAO-0214       PIC  9(013)     VALUE ZEROS.
               10  FILLER REDEFINES         COMU-OPERACAO-0214.
                   15 COMU-DANO-0214        PIC  9(004).
                   15 COMU-NSEQ-0214        PIC  9(009).
               10  COMU-PAGINA-OPER-0214    PIC  9(05)    VALUE ZEROS.
               10  COMU-TIMESTAMP-0214      PIC  X(26)    VALUE SPACES.
               10  COMU-MENSAGEM-0214       PIC  X(79)    VALUE SPACES.
               10  COMU-TRANSA-ANTE-0214    PIC  X(08)    VALUE SPACES.
               10  COMU-PAGINA-ANTE-0214    PIC  9(05)    VALUE ZEROS.
DTS002         10  COMU-NRTITLO-0214        PIC  9(05)    VALUE ZEROS.

      *----------------------------------------------------------------*
      *        *** AREA DE COMUNICACAO PFK ***                         *
      *----------------------------------------------------------------*

       01  COMU-AREA-PFK.
           05  COMU-LL-PFK             PIC S9(004) COMP    VALUE +124.
           05  COMU-ZZ-PFK             PIC S9(004) COMP    VALUE ZEROS.
           05  COMU-TRANCODE.
               10  COMU-TRANSACAO-PFK  PIC  X(008)         VALUE SPACES.
               10  FILLER              PIC  X(006)         VALUE SPACES.
               10  COMU-PFK1           PIC  X(001)         VALUE SPACES.
           05  COMU-SENHAS-PFK         PIC  X(037)         VALUE SPACES.
           05  COMU-COMANDO-PFK        PIC  X(068)         VALUE SPACES.

BRQ001*----------------------------------------------------------------*
BRQ001 01  FILLER                      PIC  X(050)         VALUE
BRQ001     '*** AREA DO MODULO DCOM5490 ***'.
BRQ001*----------------------------------------------------------------*
BRQ001
BRQ001 01  WRK-5490-ENTRADA.
BRQ001     05  WRK-E5490-COD-RETORNO      PIC  X(004)      VALUE SPACES.
BRQ001     05  WRK-E5490-MSG-RETORNO      PIC  X(079)      VALUE SPACES.
BRQ001     05  WRK-E5490-RESTART          PIC  9(005)      VALUE ZEROS.
BRQ001     05  WRK-E5490-FLAG             PIC  X(001)      VALUE SPACES.
BRQ001     05  WRK-E5490-START-COUNT      PIC  X(001)      VALUE SPACES.
BRQ001     05  WRK-E5490-CFUNC-BDSCO      PIC  9(009)      VALUE ZEROS.
BRQ001     05  WRK-E5490-CTERM            PIC  X(008)      VALUE SPACES.
BRQ001     05  WRK-E5490-CPRODT           PIC  9(003)      VALUE ZEROS.
BRQ001     05  WRK-E5490-CSPROD-DESC-COML PIC  9(003)      VALUE ZEROS.
BRQ001     05  WRK-E5490-CTPO-DESC-COML   PIC  9(003)      VALUE ZEROS.
BRQ001     05  WRK-E5490-NRO-OPERACAO.
BRQ001         10  WRK-E5490-DANO-OPER    PIC  9(004)      VALUE ZEROS.
BRQ001         10  WRK-E5490-NSEQ-OPER    PIC  9(009)      VALUE ZEROS.
BRQ001     05  WRK-E5490-VOPER-DESC-COML  PIC  9(015)V99   VALUE ZEROS.
BRQ001     05  WRK-E5490-FUNCAO           PIC  X(002)      VALUE SPACES.
BRQ001     05  WRK-E5490-LIBRC-MIDIA      PIC  X(001)      VALUE SPACES.
BRQ001
BRQ001 01  WRK-5490-SAIDA.
BRQ001     05  WRK-S5490-COD-RETORNO      PIC  9(004)      VALUE ZEROS.
BRQ001     05  WRK-S5490-MSG-RETORNO      PIC  X(079)      VALUE SPACES.
BRQ001     05  WRK-S5490-RESTART          PIC  9(005)      VALUE ZEROS.
BRQ001     05  WRK-S5490-FLAG             PIC  X(001)      VALUE SPACES.
BRQ001     05  WRK-S5490-LIBRC-MIDIA      PIC  X(001)      VALUE SPACES.
BRQ001     05  WRK-S5490-CADASTRO-BROB    PIC  X(001)      VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(080)         VALUE
           '*** AREA DE COMUNICACAO PROGRAMA DCOM4266              ***'.
      *----------------------------------------------------------------*

       01  4266E-ENTRADA.
           03  4266E-COD-RETORNO           PIC  X(004).
           03  4266E-MSG-RETORNO           PIC  X(079).
           03  4266E-RESTART               PIC  9(005).
           03  4266E-FLAG                  PIC  X(001).
           03  4266E-CFUNC-BDSCO           PIC  9(009).
           03  4266E-CTERM                 PIC  X(008).
           03  4266E-DANO-OPER-DESC        PIC  9(004).
           03  4266E-NSEQ-OPER-DESC        PIC  9(009).
FS2511*    03  4266E-CCNPJ-CPF             PIC  9(009).
FS2511*    03  4266E-CFLIAL-CNPJ           PIC  9(004).
FS2511     03  4266E-CCNPJ-CPF             PIC  X(009).
FS2511     03  4266E-CFLIAL-CNPJ           PIC  X(004).

       01  4266S-SAIDA.
           03  4266S-HEADER.
               05  4266S-COD-RETORNO       PIC  X(004).
               05  4266S-MSG-RETORNO       PIC  X(079).
               05  4266S-RESTART           PIC  9(005).
               05  4266S-FLAG              PIC  X(001).
           03  4266S-DADOS.
               05 4266S-CINDCD-FREQ-TX      PIC  X(02).
               05 4266S-CTPO-TX-JURO-DESC   PIC  9(03).
               05 4266S-CSGL-TPO-TX-JURO    PIC  X(06).
               05 4266S-IRSUMO-TPO-TX-JURO  PIC  X(15).
               05 4266S-ITPO-TX-JURO-DESC   PIC  X(30).

           03  4266S-CONSISTENCIA          REDEFINES 4266S-DADOS.
               05  4266S-TAMANHO-ERRO      PIC  9(001).
               05  4266S-TABELA-ERROS      OCCURS 02 TIMES.
                   07  4266S-CAMPOS-ERRO   PIC  9(001).
      *----------------------------------------------------------------*
      *        *** AREA DA BRAD7100 ***                                *
      *----------------------------------------------------------------*

       COPY  'I#BRAD7C'.

       COPY  'I#DCOMXE'.

      *----------------------------------------------------------------*
      *--- OBTER DADOS BASICOS      ---*

       COPY I#DCOMXJ.

7C0922*----------------------------------------------------------------*
.     *     *** AREA DE ACESSO AO MODULO DCOM7565 ***
.     *----------------------------------------------------------------*
.
.      COPY I#DCOML7.
7C0922
      *----------------------------------------------------------------*
      *---           AREA DE ACESSO AO MODULO DCOM5111              ---*
      *----------------------------------------------------------------*

       01  5111E-ROTEADOR.
           03  5111E-COD-RETORNO      PIC  X(004).
           03  5111E-MSG-RETORNO      PIC  X(079).
           03  5111E-RESTART          PIC  9(005).
           03  5111E-FLAG             PIC  X(001).
           03  5111E-CFUNC-BDSCO      PIC  9(009).
           03  5111E-CTERM            PIC  X(008).
           03  5111E-DANO-OPER-DESC   PIC  9(004).
           03  5111E-NSEQ-OPER-DESC   PIC  9(009).

      *--- AREA DE SAIDA DO ROTEADOR   --> TAM.   97 BYTES          ---*

       01  5111S-ROTEADOR.
           03  5111S-COD-RETORNO      PIC  X(004).
           03  5111S-MSG-RETORNO      PIC  X(079).
           03  5111S-RESTART          PIC  9(005).
           03  5111S-FLAG             PIC  9(001).
           03  5111S-CONSISTENCIA.
               05  5111S-TAMANHO-ERRO PIC  9(001).
               05  5111S-TABELA-ERROS OCCURS  4 TIMES.
                   07  5111S-CAMPOS-ERRO
                                       PIC 9(001).

      *----------------------------------------------------------------*
      *---           AREA DE ACESSO AO MODULO DCOM4134              ---*
      *----------------------------------------------------------------*

       01  4134E-ROTEADOR.
           05  4134E-COD-RETORNO                 PIC  X(004).
           05  4134E-MSG-RETORNO                 PIC  X(079).
           05  4134E-RESTART                     PIC  9(005).
           05  4134E-FLAG                        PIC  X(001).
           05  4134E-CFUNC-BDSCO                 PIC  9(009).
           05  4134E-CTERM                       PIC  X(008).
           05  4134E-DANO-OPER-DESC              PIC  9(004).
           05  4134E-NSEQ-OPER-DESC              PIC  9(009).
BRQ001     05  WRK-4134E-OPCAO-MENU              PIC  9(001)
BRQ001                                           VALUE ZEROS.
BRQ001*        3 - CONSULTAR LIBERACAO
BRQ001*        4 - CONSULTAR PENDENTES NOS CANAIS

       01  4134S-ROTEADOR.
           05  4134S-COD-RETORNO                 PIC  X(004).
           05  4134S-MSG-RETORNO                 PIC  X(079).
           05  4134S-RESTART                     PIC  9(005).
           05  4134S-FLAG                        PIC  X(001).
           05  4134S-DADOS.
             10  4134S-TPO-LIBRC                 PIC  X(001).
             10  4134S-CFUNC-AUTRZ-PRAZO         PIC  9(009).
             10  4134S-CFUNC-AUTRZ-TAXA          PIC  9(009).
             10  4134S-CFUNC-AUTRZ-CONE          PIC  9(009).
             10  4134S-PARECER-FORML-DESC        PIC  X(240).
             10  4134S-VLIBRC-MAX                PIC  9(015)V9(2).
             10  4134S-PMAX-LIBRC-ANTCP          PIC  9(003)V9(3).
             10  4134S-PLIBRC-LIBERADO           PIC  9(003)V9(3).
             10  4134S-VLIBRC-LIBERADO           PIC  9(015)V9(2).
           05  4134S-CONSISTENCIA     REDEFINES  4134S-DADOS.
             10  4134S-TAMANHO-ERRO               PIC  9(001).
             10  4134S-TABELA-ERROS         OCCURS 2 TIMES.
               15  4134S-CAMPOS-ERRO             PIC  9(001).

      *----------------------------------------------------------------*
      *---             AREA DE ACESSO AO MODULO DCOM7995            ---*
      *----------------------------------------------------------------*
       01  WRK-ENTRADA-DCOM7995.
           05  WRK-ENT-OPCAO           PIC  X(02)          VALUE SPACES.
           05  WRK-ENT-COD-FUNC        PIC  9(09)          VALUE ZEROS.
           05  WRK-ENT-COD-TERMINAL    PIC  X(08)          VALUE SPACES.
           05  WRK-ENT-TIMESTAMP       PIC  X(26)          VALUE SPACES.
           05  WRK-ENT-TRANS           PIC  X(08)          VALUE SPACES.
           05  WRK-ENT-TRANS-ANT       PIC  X(08)          VALUE SPACES.
           05  WRK-ENT-PAGINA          PIC  9(03)          VALUE ZEROS.
           05  WRK-ENT-DADOS           PIC  X(1600)        VALUE SPACES.

       01  WRK-SAIDA-DCOM7995.
           05  WRK-SAI-COD-RETORNO     PIC  9(02)          VALUE ZEROS.
           05  WRK-SAI-MSG-RETORNO     PIC  X(79)          VALUE SPACES.
           05  WRK-SAI-TIMESTAMP       PIC  X(26)          VALUE SPACES.
           05  WRK-SAI-TRANS           PIC  X(08)          VALUE SPACES.
           05  WRK-SAI-TRANS-ANT       PIC  X(08)          VALUE SPACES.
           05  WRK-SAI-PAGINA          PIC  9(03)          VALUE ZEROS.
           05  WRK-SAI-DADOS           PIC  X(1600)        VALUE SPACES.

       01  WRK-AREA-DBTEMP.
           05  WRK-DADOS-DBTEMP        PIC  X(173)         VALUE SPACES.
           05  WRK-CHAMADOR-DBTEMP     PIC  X(008)         VALUE SPACES.

      *----------------------------------------------------------------*
      *        *** DCOM0408 - FIM DA AREA DE WORKING ***               *
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  LNK-IO-PCB.
           05  LNK-IO-LTERM            PIC  X(008).
           05  FILLER                  PIC  X(002).
           05  LNK-IO-STATUS           PIC  X(002).
           05  FILLER                  PIC  X(012).
           05  LNK-IO-MODNAME          PIC  X(008).

       01  LNK-ALT-PCB.
           05  LNK-ALT-LTERM           PIC  X(008).
           05  FILLER                  PIC  X(002).
           05  LNK-ALT-STATUS          PIC  X(002).
           05  FILLER                  PIC  X(012).
           05  LNK-ALT-MODNAME         PIC  X(008).

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

           ENTRY  'DLITCBL'            USING  LNK-IO-PCB
                                              LNK-ALT-PCB.

           MOVE  WRK-GU                TO     WRK-FUNCAO.

      *----------------------------------------------------------------*
      *        PROCEDIMENTOS INICIAIS                                  *
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           CALL  'POOL5005'            USING  WRK-VERSAO
                                              WRK-FUNCAO
                                              LNK-IO-PCB
                                              LNK-ALT-PCB
                                              WRK-MENSAGEM
                                              WRK-TELA
                                              WRK-COD-USER
                                              WRK-COD-DEPTO.

           IF RETURN-CODE    EQUAL  04  OR  12
              GOBACK
           END-IF.

           MOVE SPACES          TO  OUT-DADOS.

           PERFORM  1000-PROCESSAR-MENSAGEM.

           GO   TO  0000-INICIAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-PROCESSAR-MENSAGEM         SECTION.
      *----------------------------------------------------------------*

           MOVE  'DCOMDHTM'             TO WRK-TELA.

           IF WRK-TRANSACAO            NOT EQUAL  'DCOM0408'
              PERFORM  1100-RECEBER-CONTROLE
           ELSE
              PERFORM  2000-PROCESSAR-DCOMDHTM
           END-IF.

           IF WRK-TELA                 EQUAL  'DCOMDHTM'
              PERFORM  3000-PROCESSAR-BRAD0660
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1100-RECEBER-CONTROLE           SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO ERRO-AREA WRK-SQLCA
                                          OUT-MENSAG.

           INITIALIZE ROTENT-6437-ENVIO
                      ROTSAI-6437-RETORNO
                      ERRO-AREA
                      4174-ENTRADA-ROTEADOR
                      4174-SAIDA-ROTEADOR
                      4134E-ROTEADOR
                      4134S-ROTEADOR.

           IF  WRK-TRANSACAO    NOT EQUAL 'DCOM0214' AND 'DCOM0222'
PROC01         MOVE WRK-MENSAGEM           TO COMU-AREA-UN
PROC01
PROC01         IF  WRK-TRANSACAO           EQUAL 'DCOM3924'
PROC01             MOVE COMU-CHAMADOR-UN   TO INP-TRANSAC
PROC01                                        OUT-TRANSAC
PROC01         ELSE
PROC01             MOVE WRK-TRANSACAO      TO INP-TRANSAC
PROC01                                        OUT-TRANSAC
PROC01         END-IF
PROC01
               PERFORM 1101-GRAVAR-DBTEMP

               MOVE COMU-TIMESTAMP-UN  TO INP-TMSTAMP
                                          OUT-TMSTAMP
               MOVE COMU-SENHAS-UN     TO INP-SENHAS
                                              OUT-SENHAS
               MOVE COMU-COMANDO-UN    TO INP-COMANDO
                                              OUT-COMANDO

               MOVE COMU-DANO-OPER-UN  TO 4174-ENT-DANO-OPER-DESC
                                          4134E-DANO-OPER-DESC
                                      WRK-INP-OPERAC(1:4)
               MOVE COMU-NSEQ-OPER-UN  TO 4174-ENT-NSEQ-OPER-DESC
                                          4134E-NSEQ-OPER-DESC
                                      WRK-INP-OPERAC(5:9)
           ELSE
               IF  WRK-TRANSACAO              EQUAL 'DCOM0214'
                   MOVE WRK-MENSAGEM          TO COMU-AREA-0214
                   MOVE 'DCOM0402'            TO INP-TRANSAC
                                                 OUT-TRANSAC
                   MOVE COMU-TIMESTAMP-0214   TO INP-TMSTAMP
                                                 OUT-TMSTAMP
                   MOVE COMU-SENHAS-0214      TO INP-SENHAS
                                                 OUT-SENHAS
                   MOVE ' '                   TO INP-FLAGENT
                                                 OUT-FLAGENT
                   MOVE COMU-DANO-0214        TO 4174-ENT-DANO-OPER-DESC
                                                 4134E-DANO-OPER-DESC
                                             WRK-INP-OPERAC(1:4)
                   MOVE COMU-NSEQ-0214        TO 4174-ENT-NSEQ-OPER-DESC
                                                 4134E-NSEQ-OPER-DESC
                                             WRK-INP-OPERAC(5:9)
                   IF COMU-MENSAGEM-0214 NOT EQUAL SPACES
                       MOVE COMU-MENSAGEM-0214 TO OUT-MENSAG
                   END-IF
               ELSE
                   MOVE WRK-MENSAGEM          TO COMU-AREA-0215
                   MOVE 'DCOM0402'            TO INP-TRANSAC
                                                 OUT-TRANSAC
                   MOVE COMU-TIMESTAMP-0215   TO INP-TMSTAMP
                                                 OUT-TMSTAMP
                   MOVE COMU-SENHAS-0215      TO INP-SENHAS
                                                 OUT-SENHAS
                   MOVE ' '                   TO INP-FLAGENT
                                                 OUT-FLAGENT
                   MOVE COMU-DANO-0215        TO 4174-ENT-DANO-OPER-DESC
                                                 4134E-DANO-OPER-DESC
                                             WRK-INP-OPERAC(1:4)
                   MOVE COMU-NSEQ-0215        TO 4174-ENT-NSEQ-OPER-DESC
                                                 4134E-NSEQ-OPER-DESC
                                             WRK-INP-OPERAC(5:9)
                   IF COMU-MENSAGEM-0215 NOT EQUAL SPACES
                       MOVE COMU-MENSAGEM-0215 TO OUT-MENSAG
                   END-IF
               END-IF
           END-IF.

           MOVE SPACES                 TO OUT-CAMPO3
                                          OUT-CAMPO4-X.

           PERFORM 1110-ACESSAR-DCOM4174.

           IF  4174-SAI-COD-RETORNO    EQUAL '0000' AND
               WRK-INCONSIS            EQUAL SPACES
               PERFORM 1130-ACESSAR-DCOM4134
               PERFORM 1111-ACESSAR-DCOM4266
               IF  OUT-MENSAG             EQUAL SPACES
                   MOVE 'TECLE <PF6> PARA CONFIRMAR CANCELAMENTO DA LIBE
      -             'RACAO'             TO OUT-MENSAG
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1111-ACESSAR-DCOM4266           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE 4266E-ENTRADA
                      4266S-SAIDA

           MOVE 'DCOM4266'              TO  WRK-MODULO.

           MOVE  WRK-COD-USER           TO  4266E-CFUNC-BDSCO.
           MOVE  LNK-IO-LTERM           TO  4266E-CTERM.
           MOVE  4174-ENT-DANO-OPER-DESC TO  4266E-DANO-OPER-DESC.
           MOVE  4174-ENT-NSEQ-OPER-DESC TO  4266E-NSEQ-OPER-DESC.
           MOVE  INP-CNPJCPF       (1:3) TO  4266E-CCNPJ-CPF(1:3).
           MOVE  INP-CNPJCPF       (5:3) TO  4266E-CCNPJ-CPF(4:3).
           MOVE  INP-CNPJCPF       (9:3) TO  4266E-CCNPJ-CPF(7:3).
           MOVE  INP-CNPJCPF      (13:4) TO  4266E-CFLIAL-CNPJ.

           CALL  WRK-MODULO          USING  4266E-ENTRADA
                                            4266S-SAIDA
                                            ERRO-AREA
                                            WRK-SQLCA.

           EVALUATE 4266S-COD-RETORNO

              WHEN '0000'
                   MOVE 4266S-IRSUMO-TPO-TX-JURO    TO
                        OUT-TPOTAXA
                   IF  4266S-CINDCD-FREQ-TX  EQUAL 'AA'
                       MOVE 'A.A.'             TO   OUT-NMJUROS
                       MOVE 4174-SAI-PTX-JURO-ANO
                                               TO   OUT-TAXAJUR-N
                   ELSE
                       MOVE 'A.M.'             TO   OUT-NMJUROS
                       MOVE 4174-SAI-PTX-JURO-MES
                                               TO   OUT-TAXAJUR-N
                   END-IF

              WHEN '0099'
                    MOVE   'DB2'               TO   ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO

              WHEN  OTHER
                    MOVE 'A.M.'                TO  OUT-NMJUROS
                    MOVE 4174-SAI-PTX-JURO-MES
                                               TO  OUT-TAXAJUR-N
           END-EVALUATE.

      *----------------------------------------------------------------*
       1111-99-4266-FIM.               EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1101-GRAVAR-DBTEMP              SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-ENTRADA-DCOM7995
                      WRK-SAIDA-DCOM7995.

           MOVE 'I '                   TO WRK-ENT-OPCAO
           MOVE WRK-COD-USER-R         TO WRK-ENT-COD-FUNC
           MOVE LNK-IO-LTERM           TO WRK-ENT-COD-TERMINAL
           MOVE COMU-TIMESTAMP-UN      TO WRK-ENT-TIMESTAMP
           MOVE 'DCOM0408'             TO WRK-ENT-TRANS
           MOVE 1                      TO WRK-ENT-PAGINA
           MOVE COMU-DADOS-UN          TO WRK-DADOS-DBTEMP

PROC01     IF  WRK-TRANSACAO           EQUAL 'DCOM3924'
PROC01         MOVE COMU-CHAMADOR-UN   TO WRK-CHAMADOR-DBTEMP
PROC01     ELSE
PROC01         MOVE WRK-TRANSACAO      TO WRK-CHAMADOR-DBTEMP
PROC01     END-IF.
PROC01
           MOVE WRK-AREA-DBTEMP        TO WRK-ENT-DADOS

           MOVE 'DCOM7995'             TO WRK-MODULO

           CALL WRK-MODULO             USING WRK-ENTRADA-DCOM7995
                                             WRK-SAIDA-DCOM7995
                                             ERRO-AREA
                                             WRK-SQLCA.

           IF  WRK-SAI-COD-RETORNO     EQUAL 01
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-SAI-MSG-RETORNO
                                       TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           ELSE
               IF  WRK-SAI-COD-RETORNO EQUAL 99
                   MOVE 'DB2'          TO ERR-TIPO-ACESSO
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1101-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1102-LER-DBTEMP                 SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-ENTRADA-DCOM7995
                      WRK-SAIDA-DCOM7995.

           MOVE 'CS'                   TO WRK-ENT-OPCAO
           MOVE WRK-COD-USER-R         TO WRK-ENT-COD-FUNC
           MOVE LNK-IO-LTERM           TO WRK-ENT-COD-TERMINAL
           MOVE INP-TMSTAMP            TO WRK-ENT-TIMESTAMP
           MOVE 'DCOM0408'             TO WRK-ENT-TRANS
           MOVE 1                      TO WRK-ENT-PAGINA

           MOVE 'DCOM7995'             TO WRK-MODULO

           CALL WRK-MODULO             USING WRK-ENTRADA-DCOM7995
                                             WRK-SAIDA-DCOM7995
                                             ERRO-AREA
                                             WRK-SQLCA.

           IF  WRK-SAI-COD-RETORNO     EQUAL 01
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-SAI-MSG-RETORNO
                                       TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           ELSE
               IF  WRK-SAI-COD-RETORNO EQUAL 99
                   MOVE 'DB2'          TO ERR-TIPO-ACESSO
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO
               END-IF
           END-IF.

           MOVE WRK-SAI-DADOS          TO WRK-AREA-DBTEMP.

      *----------------------------------------------------------------*
       1102-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1103-APAGAR-DBTEMP              SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-ENTRADA-DCOM7995
                      WRK-SAIDA-DCOM7995.

           MOVE 'ET'                   TO WRK-ENT-OPCAO
           MOVE WRK-COD-USER-R         TO WRK-ENT-COD-FUNC
           MOVE LNK-IO-LTERM           TO WRK-ENT-COD-TERMINAL
           MOVE INP-TMSTAMP            TO WRK-ENT-TIMESTAMP
           MOVE 'DCOM0408'             TO WRK-ENT-TRANS
           MOVE 1                      TO WRK-ENT-PAGINA

           MOVE 'DCOM7995'             TO WRK-MODULO

           CALL WRK-MODULO             USING WRK-ENTRADA-DCOM7995
                                             WRK-SAIDA-DCOM7995
                                             ERRO-AREA
                                             WRK-SQLCA.

           IF  WRK-SAI-COD-RETORNO     EQUAL 01
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-SAI-MSG-RETORNO
                                       TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           ELSE
               IF  WRK-SAI-COD-RETORNO EQUAL 99
                   MOVE 'DB2'          TO ERR-TIPO-ACESSO
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1103-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1110-ACESSAR-DCOM4174           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-COD-USER           TO 4174-ENT-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO 4174-ENT-CTERM
           MOVE 'DCOM4174'             TO WRK-MODULO.

           CALL WRK-MODULO             USING 4174-ENTRADA-ROTEADOR
                                             4174-SAIDA-ROTEADOR
                                             ERRO-AREA
                                             WRK-SQLCA.

           EVALUATE 4174-SAI-COD-RETORNO
               WHEN '0000'
                   MOVE SPACES         TO WRK-INCONSIS
                   MOVE 4174-SAI-CAG-BCRIA
                                       TO ROTENT-6437-AGEPESQ-ENV
                   PERFORM 1111-1-VALIDAR-PERFIL
                   IF  WRK-INCONSIS    EQUAL SPACES
                       PERFORM 1111-CARREGAR-DADOS-TELA
                   ELSE
                       MOVE SPACES     TO WRK-INCONSIS
                       MOVE 4174-SAI-CAG-OPER-OPER
                                       TO ROTENT-6437-AGEPESQ-ENV
                       PERFORM 1111-1-VALIDAR-PERFIL
                       IF  WRK-INCONSIS    EQUAL SPACES
                           PERFORM 1111-CARREGAR-DADOS-TELA
                       ELSE
                           MOVE 'S'    TO WRK-FLAG-ERRO
                           MOVE 'USUARIO NAO AUTORIZADO'
                                       TO WRK-MENSAGEM-ERRO
                           PERFORM 2300-RETORNAR-CHAMADOR
                       END-IF
                   END-IF


                   IF  WRK-INCONSIS            EQUAL SPACES  AND
                       4174-SAI-CSIT-DESC-COML EQUAL 9
                       MOVE 'OPERACAO NAO PERMITE A OPCAO SELECIONADA, O
      -                     'PERACAO ADITADA' TO WRK-MENSAGEM-ERRO
                       MOVE '*'    TO WRK-INCONSIS
                       MOVE 'S'    TO WRK-FLAG-ERRO
                       PERFORM 2300-RETORNAR-CHAMADOR
                   END-IF

               WHEN '0003'
                   MOVE 'S'            TO WRK-FLAG-ERRO
                   MOVE 4174-SAI-MSG-RETORNO
                                       TO WRK-MENSAGEM-ERRO
                   PERFORM 2300-RETORNAR-CHAMADOR

               WHEN '0099'
                   MOVE 'DB2'          TO ERR-TIPO-ACESSO
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                   MOVE 'APL'          TO ERR-TIPO-ACESSO
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO

           END-EVALUATE.


       1110-99-FIM.                    EXIT.

      *----------------------------------------------------------------*
       1111-1-VALIDAR-PERFIL           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-COD-USER           TO ROTENT-6437-CFUNC-ENV.
           MOVE LNK-IO-LTERM           TO ROTENT-6437-CTERM.
           MOVE WRK-COD-DEPTO-N        TO ROTENT-6437-AGEUSU-ENV.
           MOVE 'A'                    TO ROTENT-6437-FUNCAO-ENV.
           MOVE 'DCOM6437'             TO WRK-MODULO.

           CALL WRK-MODULO             USING ROTENT-6437-ENVIO
                                             ROTSAI-6437-RETORNO
                                             ERRO-AREA
                                             WRK-SQLCA.

           IF  ROTSAI-6437-COD-RET     NOT EQUAL ZEROS OR
               ROTSAI-6437-RESUL-RET   GREATER 3
               MOVE '*'                TO WRK-INCONSIS
           END-IF.

       1111-1-99-FIM.                    EXIT.

      *----------------------------------------------------------------*
       1111-CARREGAR-DADOS-TELA        SECTION.
      *----------------------------------------------------------------*

           MOVE  4174-SAI-DANO-OPER-DESC
                                           TO OUT-OPERAC(1:4)
           MOVE  4174-SAI-NSEQ-OPER-DESC
                                           TO OUT-OPERAC(5:9)
           MOVE  4174-SAI-CPRODT           TO OUT-PRODUTO
           MOVE  4174-SAI-IABREV-PRODT     TO OUT-DPRODUT
           MOVE  4174-SAI-CSPROD-DESC-COML
                                           TO OUT-SPRODUT
           MOVE  4174-SAI-IRSUMO-SPROD-DESC
                                           TO OUT-DSPRODU
           MOVE  4174-SAI-IRSUMO-SIT-DESC  TO OUT-SITUAC
           MOVE  4174-SAI-IPSSOA-DESC-COML TO OUT-NOME
           MOVE  4174-SAI-CCNPJ-CPF(1:3)   TO OUT-CNPJCPF(1:3)
                                              INP-CNPJCPF(1:3)
           MOVE  '.'                       TO OUT-CNPJCPF(4:1)
           MOVE  4174-SAI-CCNPJ-CPF(4:3)   TO OUT-CNPJCPF(5:3)
                                              INP-CNPJCPF(5:3)
           MOVE  '.'                       TO OUT-CNPJCPF(8:1)
           MOVE  4174-SAI-CCNPJ-CPF(7:3)   TO OUT-CNPJCPF(9:3)
                                              INP-CNPJCPF(9:3)
           MOVE  '/'                       TO OUT-CNPJCPF(12:1)
           MOVE  4174-SAI-CFLIAL-CNPJ      TO OUT-CNPJCPF(13:4)
                                              INP-CNPJCPF(13:4)
           MOVE  '-'                       TO OUT-CNPJCPF(17:1)
           MOVE  4174-SAI-CCTRL-CNPJ-CPF   TO OUT-CNPJCPF(18:2)
                                              INP-CNPJCPF(18:2)
           MOVE  4174-SAI-CBCO             TO OUT-CAMPO2(1:3)
           MOVE  '/'                       TO OUT-CAMPO2(4:1)
           MOVE  4174-SAI-CAG-BCRIA        TO OUT-CAMPO2(5:5)
                                              INP-CAMPO2(5:5)
           MOVE  '/'                       TO OUT-CAMPO2(10:1)
           MOVE  4174-SAI-CCTA-BCRIA-CLI   TO OUT-CAMPO2(11:13)
                                              INP-CAMPO2(11:13)

           IF 4174-SAI-CCONTR-LIM-DESC      GREATER  ZEROS
              MOVE 'CONTR LIMITE:'          TO  OUT-CAMPO3
              MOVE 4174-SAI-CCONTR-LIM-DESC TO  OUT-CAMPO4
           ELSE
             IF 4174-SAI-CCONTR-CONVE-DESC  GREATER  ZEROS AND
                4174-SAI-CELMTO-DESC-COML   EQUAL 008
                MOVE 'CONV GERAL..:'            TO OUT-CAMPO3
                MOVE 4174-SAI-CCONTR-CONVE-DESC TO OUT-CAMPO4
             ELSE
             IF 4174-SAI-CCONTR-CONVE-DESC  GREATER  ZEROS AND
                4174-SAI-CELMTO-DESC-COML   EQUAL 009
                MOVE 'CONV CLIENTE:'            TO OUT-CAMPO3
                MOVE 4174-SAI-CCONTR-CONVE-DESC TO OUT-CAMPO4
             END-IF
           END-IF.

           MOVE  4174-SAI-VOPER-DESC-COML  TO OUT-VLROPER-N
           MOVE  4174-SAI-VTAC-OPER-DESC   TO OUT-VLTAROP-N
           MOVE  4174-SAI-VTARIF-REG-TITLO TO OUT-VLTRFTT-N
           MOVE  4174-SAI-VJURO-OPER-DESC  TO OUT-VLRJUR-N
           MOVE  4174-SAI-VIOF-OPER-DESC   TO OUT-VLRIOF-N
           MOVE  4174-SAI-VLIQ-OPER-DESC   TO OUT-VLRLIQD-N
           MOVE  4174-SAI-ITPO-TX-JURO-DESC TO OUT-TPOTAXA
           MOVE  4174-SAI-DVCTO-FNAL-OPER  TO OUT-DTVCTOF
           INSPECT OUT-DTVCTOF REPLACING ALL '.' BY '/'.
           MOVE  4174-SAI-DINIC-OPER-DESC  TO OUT-DTOPER
           INSPECT OUT-DTOPER  REPLACING ALL '.' BY '/'.
           MOVE  4174-SAI-TMED-PONDE-OPER  TO OUT-PRZMED
           MOVE  4174-SAI-QTITLO-OPER-APURA TO OUT-QTDETIT-N.
           MOVE  4174-SAI-ITPO-GARNT       TO OUT-GARANTI.
           MOVE  4174-SAI-CTPO-DESC-COML   TO OUT-TIPODES.

BRQ001     PERFORM 1112-OBTER-INFO-LIBRC-MIDIA.

      *----------------------------------------------------------------*
       1111-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

BRQ001*----------------------------------------------------------------*
BRQ001*     OBTER PARAMETRO DE LIBERACAO NAS MIDIAS
BRQ001*----------------------------------------------------------------*
BRQ001 1112-OBTER-INFO-LIBRC-MIDIA     SECTION.
BRQ001*----------------------------------------------------------------*
BRQ001
BRQ001     INITIALIZE WRK-5490-ENTRADA
BRQ001                WRK-5490-SAIDA
BRQ001                ERRO-AREA
BRQ001                WRK-SQLCA.
BRQ001
BRQ001     MOVE 4174-ENT-DANO-OPER-DESC   TO WRK-E5490-DANO-OPER.
BRQ001     MOVE 4174-ENT-NSEQ-OPER-DESC   TO WRK-E5490-NSEQ-OPER.
BRQ001     MOVE 'CO'                      TO WRK-E5490-FUNCAO.
BRQ001     MOVE 'DCOM5490'                TO WRK-MODULO.
BRQ001
BRQ001     CALL  WRK-MODULO            USING WRK-5490-ENTRADA
BRQ001                                       WRK-5490-SAIDA
BRQ001                                       ERRO-AREA
BRQ001                                       WRK-SQLCA.
BRQ001
BRQ001     IF  WRK-S5490-COD-RETORNO   EQUAL 99
BRQ001         MOVE 'DB2'              TO ERR-TIPO-ACESSO
BRQ001         PERFORM 9999-PROCESSAR-ROTINA-ERRO
BRQ001     END-IF.
BRQ001
BRQ001     IF  WRK-S5490-LIBRC-MIDIA   EQUAL 'S'
BRQ001         MOVE 'SIM'              TO OUT-LIBMID
BRQ001     ELSE
BRQ001         MOVE 'NAO'              TO OUT-LIBMID
BRQ001     END-IF.
BRQ001
BRQ001*----------------------------------------------------------------*
BRQ001 1112-99-FIM.                    EXIT.
BRQ001*----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1130-ACESSAR-DCOM4134           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-COD-USER           TO 4134E-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO 4134E-CTERM
BRQ001     MOVE 3                      TO WRK-4134E-OPCAO-MENU.

           MOVE 'DCOM4134'             TO WRK-MODULO.

           CALL WRK-MODULO             USING 4134E-ROTEADOR
                                             4134S-ROTEADOR
                                             ERRO-AREA
                                             WRK-SQLCA.

           EVALUATE 4134S-COD-RETORNO
               WHEN '0000'
                   STRING 'PRAZO: '4134S-CFUNC-AUTRZ-PRAZO
                   ' TAXA: '4134S-CFUNC-AUTRZ-TAXA
                   ' CONE: '4134S-CFUNC-AUTRZ-CONE
                   DELIMITED BY SIZE           INTO OUT-AUTRZTE
                   MOVE 4134S-PARECER-FORML-DESC
                                       TO OUT-DESCR
                   MOVE 4134S-PMAX-LIBRC-ANTCP  TO OUT-JURMAXI-N
                   MOVE 4134S-VLIBRC-MAX        TO OUT-VLRMAXI-N
                   MOVE 4134S-VLIBRC-LIBERADO   TO OUT-VLLIBER-N
                   MOVE 4134S-PLIBRC-LIBERADO   TO OUT-JRLIBER-N

               WHEN '0099'
                   MOVE 'DB2'          TO ERR-TIPO-ACESSO
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                   MOVE 4134S-MSG-RETORNO
                                       TO WRK-MENSAGEM-ERRO
                   MOVE 'S'            TO WRK-FLAG-ERRO
                   PERFORM 2300-RETORNAR-CHAMADOR

           END-EVALUATE.

      *----------------------------------------------------------------*
       1130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000-PROCESSAR-DCOMDHTM         SECTION.
      *----------------------------------------------------------------*

           MOVE  WRK-MENSAGEM          TO  INPUT-DHTM.
           PERFORM 2200-DEVOLVER-TELA.

           EVALUATE  TRUE
               WHEN  INP-PFK          EQUAL  'H'  OR
                    (INP-PFK          EQUAL  '.'  AND
                     INP-COMANDO      EQUAL  'PFK01')
                     MOVE INP-MENSAG  TO OUT-MENSAG

               WHEN  INP-PFK          EQUAL  '3'  OR
                    (INP-PFK          EQUAL  '.'  AND
                     INP-COMANDO      EQUAL 'PFK03')
                     PERFORM  2300-RETORNAR-CHAMADOR

               WHEN  INP-PFK          EQUAL  '5'  OR
                    (INP-PFK          EQUAL  '.'  AND
                     INP-COMANDO      EQUAL  'PFK05')
                     PERFORM  2400-MENU-ROTINAS

               WHEN  INP-PFK          EQUAL  'B'  OR
                    (INP-PFK          EQUAL  '.'  AND
                     INP-COMANDO      EQUAL  'PFK10')
                     PERFORM  2500-MENU-DCOM

               WHEN  INP-PFK          EQUAL  '6'  OR
                    (INP-PFK          EQUAL  '.'  AND
                     INP-COMANDO      EQUAL  'PFK06')

7C0922               IF ( INP-SPRODUT            EQUAL 007 OR
.                         INP-SPRODUT            EQUAL 008)
.                      PERFORM 2100-OBTER-MSGEM-COR0005R1
.                    END-IF
.
7C0922               IF WRK-INCONSIS NOT EQUAL '*'
                        PERFORM 2110-ACESSA-DCOM5111
7C0922               END-IF

                     IF  WRK-INCONSIS EQUAL '*'
                         GO TO 2000-99-FIM
                     END-IF
                     MOVE 'CANCELAMENTO EFETUADO COM SUCESSO - C/C DEBIT
      -                   'ADA'
                                      TO WRK-MENSAGEM-ERRO
                     MOVE 'S'         TO WRK-FLAG-ERRO
                     PERFORM 2300-RETORNAR-CHAMADOR

               WHEN  INP-PFK          EQUAL  'C' OR
                    (INP-PFK          EQUAL  '.'  AND
                     INP-COMANDO      EQUAL  'PFK11') AND
                    (INP-TIPODES      EQUAL 2 OR 3)
                     IF  INP-TIPODES  EQUAL 2
                         PERFORM  2800-ACESSA-DCOM0214
                     ELSE
                         PERFORM  2900-ACESSA-DCOM0222
                     END-IF

PROC01         WHEN  INP-PFK          EQUAL  'D' OR
PROC01              (INP-PFK          EQUAL  '.'  AND
PROC01               INP-COMANDO      EQUAL  'PFK12')
PROC01               PERFORM 2950-ACESSAR-CET
PROC01
               WHEN  INP-PFK          EQUAL  '.'  AND
                     INP-COMANDO      EQUAL  SPACES
                     MOVE 'TECLE <PF6> PARA CONFIRMAR CANCELAMENTO DA LI
      -                   'BERACAO'   TO OUT-MENSAG

               WHEN  OTHER
                     MOVE  'PF INVALIDA'   TO  OUT-MENSAG

           END-EVALUATE.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

7C0922*----------------------------------------------------------------*
.      2100-OBTER-MSGEM-COR0005R1      SECTION.
.     *----------------------------------------------------------------*
.
.          INITIALIZE L7-ARGUMENTOS-ENTRADA
.                     L7-RETORNO
.                     ERRO-AREA
.                     WRK-SQLCA.
.
.          MOVE WRK-INP-OPERA-DANO           TO L7-DANO-OPER-DESC.
.          MOVE WRK-INP-OPERA-NSEQ           TO L7-NSEQ-OPER-DESC.
.          MOVE 'COR0005R1'                  TO L7-CMSGEM-RECOR.
.          MOVE 'PARCIAL-01'                 TO L7-INSTRUCAO.
.          MOVE 'DCOM7565'                   TO WRK-MODULO.
.
.          CALL WRK-MODULO             USING L7-ARGUMENTOS-ENTRADA
.                                            L7-RETORNO
.                                            ERRO-AREA
.                                            WRK-SQLCA.
.
.          EVALUATE L7-COD-RETORNO
.             WHEN  0000
.                  MOVE
.                       'CANCELAMENTO NAO AUTORIZADO - COR0005 ENVIADO'
.                                             TO  WRK-MENSAGEM-ERRO
.                  MOVE '*'    TO WRK-INCONSIS
.                  MOVE 'S'    TO WRK-FLAG-ERRO
.                  PERFORM 2300-RETORNAR-CHAMADOR
.             WHEN  0003
.                  CONTINUE
.             WHEN OTHER
.                  MOVE L7-MENSAGEM
.                                      TO WRK-MENSAGEM-ERRO
.                  MOVE 'S'            TO WRK-FLAG-ERRO
.
.          END-EVALUATE.
.
.     *----------------------------------------------------------------*
.      2100-99-FIM.                    EXIT.
7C0922*----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2110-ACESSA-DCOM5111            SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO 5111E-COD-RETORNO
           MOVE SPACES                 TO 5111E-MSG-RETORNO
           MOVE ZEROS                  TO 5111E-RESTART
           MOVE SPACES                 TO 5111E-FLAG
           MOVE WRK-COD-USER           TO 5111E-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO 5111E-CTERM
           MOVE WRK-INP-OPERAC(1:4)    TO 5111E-DANO-OPER-DESC
           MOVE WRK-INP-OPERAC(5:9)    TO 5111E-NSEQ-OPER-DESC

           MOVE 'DCOM5111'             TO WRK-MODULO

           CALL WRK-MODULO             USING 5111E-ROTEADOR
                                             5111S-ROTEADOR
                                             ERRO-AREA
                                             WRK-SQLCA.

           IF 5111S-COD-RETORNO    NOT EQUAL '0000'
               IF 5111S-COD-RETORNO    EQUAL '0099'
                   MOVE 'DB2'          TO ERR-TIPO-ACESSO
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO
               ELSE
                  IF 5111S-COD-RETORNO    EQUAL '7777'
                      MOVE 'APL'          TO ERR-TIPO-ACESSO
                      PERFORM 9999-PROCESSAR-ROTINA-ERRO
                  ELSE
DTS001                PERFORM 8888-EFETUAR-ROLLBACK

                      MOVE '*'            TO WRK-INCONSIS
                      MOVE 5111S-MSG-RETORNO TO OUT-MENSAG
                  END-IF
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2200-DEVOLVER-TELA              SECTION.
      *----------------------------------------------------------------*

           MOVE  INP-TRANSAC                    TO OUT-TRANSAC
           MOVE  INP-FLAGENT                    TO OUT-FLAGENT
           MOVE  INP-SENHAS                     TO OUT-SENHAS
           MOVE  INP-VLROPER-X                  TO OUT-VLROPER
           MOVE  INP-TPOTAXA                    TO OUT-TPOTAXA
           MOVE  INP-VLTAROP-X                  TO OUT-VLTAROP
           MOVE  INP-TAXAJUR                    TO OUT-TAXAJUR
           MOVE  INP-NMJUROS                    TO OUT-NMJUROS
           MOVE  INP-VLTRFTT-X                  TO OUT-VLTRFTT
           MOVE  INP-DTOPER                     TO OUT-DTOPER
           MOVE  INP-DTVCTOF                    TO OUT-DTVCTOF
           MOVE  INP-VLRJUR-X                   TO OUT-VLRJUR
           MOVE  INP-PRZMED                     TO OUT-PRZMED
           MOVE  INP-VLRIOF-X                   TO OUT-VLRIOF
           MOVE  INP-QTDETIT                    TO OUT-QTDETIT
           MOVE  INP-VLRLIQD-X                  TO OUT-VLRLIQD
BRQ001     MOVE  INP-LIBMID                     TO OUT-LIBMID
           MOVE  INP-GARANTI                    TO OUT-GARANTI
           MOVE  INP-AUTRZTE                    TO OUT-AUTRZTE
           MOVE  INP-DESCR1                     TO OUT-DESCR1
           MOVE  INP-DESCR2                     TO OUT-DESCR2
           MOVE  INP-JURMAXI-E                  TO OUT-JURMAXI-N
           MOVE  INP-VLRMAXI-E                  TO OUT-VLRMAXI-N
           MOVE  INP-JRLIBER-E                  TO OUT-JRLIBER
           MOVE  INP-VLLIBER-E                  TO OUT-VLLIBER
           MOVE  INP-TMSTAMP                    TO OUT-TMSTAMP
           MOVE  INP-NOME                       TO OUT-NOME
           MOVE  INP-CNPJCPF                    TO OUT-CNPJCPF
           MOVE  INP-CAMPO2                     TO OUT-CAMPO2
           MOVE  INP-PRODUTO                    TO OUT-PRODUTO
           MOVE  INP-DPRODUT                    TO OUT-DPRODUT
           MOVE  INP-SPRODUT                    TO OUT-SPRODUT
           MOVE  INP-DSPRODU                    TO OUT-DSPRODU
           MOVE  WRK-INP-OPERAC                 TO OUT-OPERAC
           MOVE  INP-SITUAC                     TO OUT-SITUAC
           MOVE  INP-CAMPO3                     TO OUT-CAMPO3
           MOVE  INP-CAMPO4                     TO OUT-CAMPO4-X
           MOVE  INP-TIPOLIB                    TO OUT-TIPOLIB.
           MOVE  INP-TIPODES                    TO OUT-TIPODES.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2300-RETORNAR-CHAMADOR          SECTION.
      *----------------------------------------------------------------*

           INITIALIZE COMU-AREA-UN.

           MOVE LENGTH OF COMU-AREA-UN TO  COMU-LL-UN.
           MOVE ZEROS                  TO  COMU-ZZ-UN.

           PERFORM 1102-LER-DBTEMP.

           MOVE WRK-DADOS-DBTEMP       TO COMU-DADOS-UN

           PERFORM 1103-APAGAR-DBTEMP.

           IF  WRK-FLAG-ERRO           EQUAL 'S'
               MOVE WRK-MENSAGEM-ERRO  TO COMU-MENSAGEM-UN
               IF  INP-TRANSAC         EQUAL 'DCOM0402'
               AND INP-PFK             NOT EQUAL '6'
                   MOVE WRK-INP-OPERAC TO COMU-OPER-UN
                   MOVE 2              TO COMU-OPCAO-UN
               END-IF
           ELSE
               MOVE ZEROS              TO COMU-OPER-UN
                                          COMU-OPCAO-UN
           END-IF.

           MOVE  INP-SENHAS            TO  COMU-SENHAS-UN.
           MOVE  'DCOM0408'            TO  COMU-TRANSACAO-UN
           MOVE WRK-CHAMADOR-DBTEMP    TO WRK-TELA
           MOVE WRK-CHNG               TO WRK-FUNCAO.
           MOVE COMU-AREA-UN           TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2400-MENU-ROTINAS               SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                      COMU-AREA-PFK.
           MOVE  +124                  TO  COMU-LL-PFK.
           MOVE  ZEROS                 TO  COMU-ZZ-PFK.
           MOVE  WRK-CHNG              TO  WRK-FUNCAO.
           MOVE  'DCOM0408'            TO  WRK-TELA
                                           COMU-TRANSACAO-PFK.
           MOVE  '5'                   TO  COMU-PFK1.
           MOVE  COMU-AREA-PFK         TO  WRK-MENSAGEM.

           PERFORM 1103-APAGAR-DBTEMP.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2500-MENU-DCOM                  SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                      COMU-AREA-PFK.

           MOVE  +124                  TO  COMU-LL-PFK.
           MOVE  ZEROS                 TO  COMU-ZZ-PFK.
           MOVE  WRK-CHNG              TO  WRK-FUNCAO.
           MOVE  'DCOM0408'            TO  WRK-TELA
                                           COMU-TRANSACAO-PFK.
           MOVE  'A'                   TO  COMU-PFK1.
           MOVE  COMU-AREA-PFK         TO  WRK-MENSAGEM.

           PERFORM 1103-APAGAR-DBTEMP.

      *----------------------------------------------------------------*
       2500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2800-ACESSA-DCOM0214            SECTION.
      *----------------------------------------------------------------*

           INITIALIZE COMU-AREA-0214.

           MOVE LENGTH OF COMU-AREA-0214    TO COMU-LL-0214
           MOVE ZEROS                       TO COMU-ZZ-0214
           MOVE WRK-CHNG                    TO WRK-FUNCAO

           MOVE INP-TIPODES                 TO COMU-TPDESC-0214
           MOVE INP-TMSTAMP                 TO COMU-TIMESTAMP-0214
           MOVE WRK-INP-OPERAC              TO COMU-OPERACAO-0214
           MOVE INP-CAMPO2(5:5)             TO COMU-AGENCIA-0214
           MOVE INP-CAMPO2(11:13)           TO COMU-CONTA-0214
           STRING INP-CNPJCPF(1:3)
                  INP-CNPJCPF(5:3)
                  INP-CNPJCPF(9:3)        DELIMITED BY SIZE
                                          INTO COMU-CPFCNPJ-0214
           MOVE INP-CNPJCPF(13:4)           TO COMU-FILIAL-0214
           MOVE INP-CNPJCPF(18:2)           TO COMU-CONTROLE-0214
           MOVE 'DCOM0408'                  TO COMU-TRANSACAO-0214
           MOVE 'DCOM0214'                  TO WRK-TELA
           MOVE WRK-CHNG                    TO WRK-FUNCAO
           MOVE COMU-AREA-0214              TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2900-ACESSA-DCOM0222            SECTION.
      *----------------------------------------------------------------*

           INITIALIZE COMU-AREA-0215.

           MOVE LENGTH OF COMU-AREA-0215    TO COMU-LL-0215
           MOVE INP-TIPODES                 TO COMU-TPDESC-0215
           MOVE INP-TMSTAMP                 TO COMU-TIMESTAMP-0215
           MOVE WRK-INP-OPERAC              TO COMU-OPERACAO-0215
           MOVE 'DCOM0408'                  TO COMU-TRANSACAO-0215
           MOVE 'DCOM0222'                  TO WRK-TELA
           MOVE WRK-CHNG                    TO WRK-FUNCAO
           MOVE COMU-AREA-0215              TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

PROC01*----------------------------------------------------------------*
PROC01 2950-ACESSAR-CET                SECTION.
PROC01*----------------------------------------------------------------*
PROC01
PROC01     INITIALIZE COMU-AREA-UN.
PROC01
PROC01     MOVE LENGTH OF COMU-AREA-UN TO  COMU-LL-UN.
PROC01     MOVE ZEROS                  TO  COMU-ZZ-UN.
PROC01
PROC01     PERFORM 1102-LER-DBTEMP.
PROC01
PROC01     MOVE WRK-DADOS-DBTEMP       TO  COMU-DADOS-UN
PROC01
PROC01     PERFORM 1103-APAGAR-DBTEMP.
PROC01
PROC01     MOVE WRK-CHNG               TO  WRK-FUNCAO.
PROC01     MOVE 'DCOM3924'             TO  WRK-TELA
PROC01     MOVE 'DCOM0408'             TO  COMU-TRANSACAO-UN
PROC01     MOVE WRK-CHAMADOR-DBTEMP    TO  COMU-CHAMADOR-UN.
PROC01
PROC01     MOVE  COMU-AREA-UN          TO  WRK-MENSAGEM.
PROC01
PROC01*----------------------------------------------------------------*
PROC01 2950-99-FIM.                    EXIT.
PROC01*----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3000-PROCESSAR-BRAD0660         SECTION.
      *----------------------------------------------------------------*

           CALL  'BRAD0660'            USING  OUTPUT-DHTM
                                              660-DCOMDHTM.

           IF RETURN-CODE              NOT EQUAL  ZEROS
              MOVE  'APL'              TO  ERR-TIPO-ACESSO
              MOVE  RETURN-CODE        TO  WRK-RETURN-CODE
              MOVE  10                 TO  WRK-LOCAL-ERRO
              MOVE  WRK-ERRO-BRAD0660  TO  ERR-TEXTO
              PERFORM  9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE  WRK-ISRT              TO  WRK-FUNCAO.
           MOVE  OUTPUT-DHTM           TO  WRK-MENSAGEM.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

DTS001*----------------------------------------------------------------*
DTS001 8888-EFETUAR-ROLLBACK           SECTION.
DTS001*----------------------------------------------------------------*
DTS001
DTS001      CALL 'CBLTDLI'             USING WRK-ROLB
DTS001                                       LNK-IO-PCB.
DTS001
DTS001*----------------------------------------------------------------*
DTS001 8888-99-FIM.                    EXIT.
DTS001*----------------------------------------------------------------*
DTS001
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           IF ERR-MODULO    NOT EQUAL  SPACES  AND  LOW-VALUES
              MOVE  'DCOM0408'         TO  ERR-PGM
           ELSE
              MOVE  'SENHAS02'         TO  ERR-PGM
              MOVE  'DCOM0408'         TO  ERR-MODULO
           END-IF.

           MOVE  WRK-COD-USER          TO  ERR-COD-USER.
           MOVE  WRK-COD-DEPTO         TO  ERR-COD-DEPTO.

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
