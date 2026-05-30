      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID.     DCOM0411.
       AUTHOR.         ADRIANO RICARDO BELMONT.
      *================================================================*
      *                          A L T R A N                           *
      *----------------------------------------------------------------*
      *     PROGRAMA....: DCOM0411                                     *
      *     PROGRAMADORA: ADRIANO RICARDO BELMONT                      *
      *     ANALISTA....: DANILO                                       *
      *     DATA........: 01/02/2007                                   *
      *----------------------------------------------------------------*
      *     OBJETIVO....: CONSULTA LIBERACAO - DEMONSTRATIVO           *
      *----------------------------------------------------------------*
      *     TELAS.......:                                              *
      *     DCOMDKTM  -   LIBERAR OPERACAO - DEMONSTRATIVO             *
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
      *                   DCOM0409                  DCOM0222           *
      *                   DCOM0410                                     *
      *================================================================*
DTS001*                   ALTERACAO  -  DTS  CONSULTING                *
DTS001*----------------------------------------------------------------*
DTS001*     PROGRAMADORA: LUCIANDRA SILVEIRA     - DTS CONSULTING      *
DTS001*     ANALISTA....: LUCIANDRA SILVEIRA     - DTS CONSULTING      *
DTS001*     ANALISTA DDS: LUIS EDUARDO DE MORAES - GRUPO 70            *
DTS001*     DATA........: 06/09/2011                                   *
DTS001*----------------------------------------------------------------*
DTS001*     OBJETIVO....: CORRIGIR AREA DE COMUNICACAO NA CHAMADA AO   *
DTS001*                   DCOM0214 (RO 3089375).                       *
DTS001*================================================================*
BRQ001*                    A L T E R A C A O  - B R Q                  *
      *----------------------------------------------------------------*
      *    ANALISTA.....:  LAERTES / LUIS EDUARDO      - BRQ           *
      *    DATA.........:  AGOSTO / 2016                               *
      *----------------------------------------------------------------*
      *    OBJETIVO.....:  TRATAMENTO PARAMETRO LIMITE NETEMPRESA.     *
      *       INCLUIR O CAMPO DE CANAL NA TELA                         *
      *----------------------------------------------------------------*
      *     MODULOS:                                                   *
      *     DCOM5505 - BUSCAR DADOS DO WORK-FLOW                       *
BRQ001*================================================================*
BRQ002*                   ALTERACAO  -  BRQ DIGITAL SOLUTIONS          *
BRQ002*----------------------------------------------------------------*
BRQ002*    PROGRAMADOR.:  FABIO AUGUSTO FINK - BRQ DIGITAL SOLUTIONS   *
BRQ002*    ANALISTA BRQ:  FABIO AUGUSTO FINK - BRQ DIGITAL SOLUTIONS   *
BRQ002*    ANALISTA DS.:  RICARDO JAMMAL     - BRADESCO / GP.39        *
BRQ002*    DATA........:  26/11/2018                                   *
BRQ002*----------------------------------------------------------------*
BRQ002*    OBJETIVO....:  BRQ 18/0456 - BORDERO ELETRONICO.            *
BRQ002*      INCLUIR TRATAMENTO PARA CAMPO INDICADOR DE LIBERACAO      *
BRQ002*      NO CANAL.                                                 *
BRQ002*----------------------------------------------------------------*
BRQ002*    MODULO......:                                               *
BRQ002*        DCOM5490 - CONSISTIR OPERACAO PENDENTE DE LIBERACAO     *
BRQ002*                   NAS MIDIAS.                                  *
BRQ002*================================================================*

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
      *           *** DCOM0411 - INICIO DA AREA DE WORKING ***         *
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
BRQ002     05  WRK-FUNCAO-5490         PIC  X(002)       VALUE SPACES.

      *----------------------------------------------------------------*
      *         *** AREAS DE ACESSO AO MODULO DCOM7995 ***             *
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
BRQ002     05  WRK-DADOS-DBTEMP        PIC  X(181)         VALUE SPACES.
           05  WRK-CHAMADOR-DBTEMP     PIC  X(008)         VALUE SPACES.

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
FS2511     03  4266E-CCNPJ-CPF             PIC  X(009).
FS2511*    03  4266E-CFLIAL-CNPJ           PIC  9(004).
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

BRQ001*----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DO MODULO DCOM5505 ***'.
      *----------------------------------------------------------------*

       01  WRK-5505E-ENTRADA.
           05 WRK-5505E-COD-RETORNO    PIC  X(004)         VALUE SPACES.
           05 WRK-5505E-MSG-RETORNO    PIC  X(079)         VALUE SPACES.
           05 WRK-5505E-RESTART        PIC  9(005)         VALUE ZEROS.
           05 WRK-5505E-FLAG           PIC  X(001)         VALUE SPACES.
           05 WRK-5505E-CFUNC-BDSCO    PIC  9(009)         VALUE ZEROS.
           05 WRK-5505E-CTERM          PIC  X(008)         VALUE SPACES.
           05 WRK-5505E-DANO-OPER-DESC PIC  9(004)         VALUE ZEROS.
           05 WRK-5505E-NSEQ-OPER-DESC PIC  9(009)         VALUE ZEROS.
           05 WRK-5505E-CEVNTO-DESC-COML
                                       PIC  9(003)         VALUE ZEROS.

       01  WRK-5505E-RETORNO.
           05 WRK-5505S-COD-RETORNO    PIC  9(004)         VALUE ZEROS.
           05 WRK-5505S-MSG-RETORNO    PIC  X(079)         VALUE SPACES.
           05 WRK-5505S-RESTART        PIC  9(005)         VALUE ZEROS.
           05 WRK-5505S-FLAG           PIC  X(001)         VALUE SPACES.
           05 WRK-5505S-CSIT-INIC-DESC PIC  9(003)         VALUE ZEROS.
           05 WRK-5505S-CSIT-FNAL-DESC PIC  9(003)         VALUE ZEROS.
           05 WRK-5505S-TAMANHO-ERRO   PIC  9(001)         VALUE ZEROS.
           05 WRK-5505S-TABELA-ERROS   OCCURS 012 TIMES.
BRQ001       10 WRK-5505S-CAMPOS-ERRO  PIC  9(002)         VALUE ZEROS.

BRQ002*----------------------------------------------------------------*
BRQ002 01  FILLER                      PIC  X(050)         VALUE
BRQ002     '*** AREA DO MODULO DCOM5490 ***'.
BRQ002*----------------------------------------------------------------*
BRQ002
BRQ002 01  WRK-5490-ENTRADA.
BRQ002     05  WRK-E5490-COD-RETORNO      PIC  X(004)      VALUE SPACES.
BRQ002     05  WRK-E5490-MSG-RETORNO      PIC  X(079)      VALUE SPACES.
BRQ002     05  WRK-E5490-RESTART          PIC  9(005)      VALUE ZEROS.
BRQ002     05  WRK-E5490-FLAG             PIC  X(001)      VALUE SPACES.
BRQ002     05  WRK-E5490-START-COUNT      PIC  X(001)      VALUE SPACES.
BRQ002     05  WRK-E5490-CFUNC-BDSCO      PIC  9(009)      VALUE ZEROS.
BRQ002     05  WRK-E5490-CTERM            PIC  X(008)      VALUE SPACES.
BRQ002     05  WRK-E5490-CPRODT           PIC  9(003)      VALUE ZEROS.
BRQ002     05  WRK-E5490-CSPROD-DESC-COML PIC  9(003)      VALUE ZEROS.
BRQ002     05  WRK-E5490-CTPO-DESC-COML   PIC  9(003)      VALUE ZEROS.
BRQ002     05  WRK-E5490-NRO-OPERACAO.
BRQ002         10  WRK-E5490-DANO-OPER    PIC  9(004)      VALUE ZEROS.
BRQ002         10  WRK-E5490-NSEQ-OPER    PIC  9(009)      VALUE ZEROS.
BRQ002     05  WRK-E5490-VOPER-DESC-COML  PIC  9(015)V99   VALUE ZEROS.
BRQ002     05  WRK-E5490-FUNCAO           PIC  X(002)      VALUE SPACES.
BRQ002     05  WRK-E5490-LIBRC-MIDIA      PIC  X(001)      VALUE SPACES.
BRQ002
BRQ002 01  WRK-5490-SAIDA.
BRQ002     05  WRK-S5490-COD-RETORNO      PIC  9(004)      VALUE ZEROS.
BRQ002     05  WRK-S5490-MSG-RETORNO      PIC  X(079)      VALUE SPACES.
BRQ002     05  WRK-S5490-RESTART          PIC  9(005)      VALUE ZEROS.
BRQ002     05  WRK-S5490-FLAG             PIC  X(001)      VALUE SPACES.
BRQ002     05  WRK-S5490-LIBRC-MIDIA      PIC  X(001)      VALUE SPACES.
BRQ002     05  WRK-S5490-CADASTRO-BROB    PIC  X(001)      VALUE SPACES.

      *----------------------------------------------------------------*
      *             *** AREAS AUXILIARES PARA POOL5005 ***             *
      *----------------------------------------------------------------*

       01  WRK-POOL5005.
           05  WRK-GU                  PIC  X(004)    VALUE 'GU  '.
           05  WRK-CHNG                PIC  X(004)    VALUE 'CHNG'.
           05  WRK-ISRT                PIC  X(004)    VALUE 'ISRT'.
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
      *        *** AREA DA TELA DCOMDKTM  -  INPUT ***                *
      *---------------------------------------------------------------*

       01  INPUT-DKTM.
           05  FILLER                    PIC  X(018)    VALUE SPACES.
           05  INP-PFK                   PIC  X(001)    VALUE SPACES.
           05  INP-SENHAS                PIC  X(037)    VALUE SPACES.
           05  INP-COMANDO               PIC  X(068)    VALUE SPACES.
           05  INP-DADOS.
BRQ002         10 WRK-INP-TITULO-DKTM    PIC  X(046)    VALUE SPACES.
               10 INP-FLAGENT            PIC  X(001)    VALUE SPACES.
               10 INP-TIPODES            PIC  9(003)    VALUE ZEROS.
BRQ002         10 WRK-INP-OPCAO-DKTM.
BRQ002            15 WRK-INP-OPCAO-DKTM-N
BRQ002                                   PIC  9(001)    VALUE ZEROS.
               10 INP-TRANSAC            PIC  X(008)    VALUE SPACES.
               10 INP-TMSTAMP            PIC  X(026)    VALUE SPACES.
               10 INP-NOME               PIC  X(040)    VALUE SPACES.
BRQ001         10 WRK-INP-CANAL          PIC  X(010)    VALUE SPACES.
               10 INP-CNPJCPF            PIC  X(019)    VALUE SPACES.
               10 INP-CAMPO2             PIC  X(023)    VALUE SPACES.
               10 INP-PRODUTO            PIC  9(003)    VALUE ZEROS.
               10 INP-DPRODUT            PIC  X(012)    VALUE SPACES.
               10 INP-SPRODUT            PIC  9(003)    VALUE ZEROS.
               10 INP-DSPRODU            PIC  X(012)    VALUE SPACES.
               10 INP-OPERAC             PIC  9(013)    VALUE ZEROS.
BRQ001         10 FILLER REDEFINES INP-OPERAC.
BRQ001            15 WRK-INP-DANO        PIC  9(004).
BRQ001            15 WRK-INP-NSEQ        PIC  9(009).
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
BRQ002         10 INP-LIBMID             PIC  X(003)    VALUE SPACES.
               10 INP-GARANTI            PIC  X(020)    VALUE SPACES.
               10 INP-AUTRZTE            PIC  X(050)    VALUE SPACES.
               10 INP-DESCR1             PIC  X(053)    VALUE SPACES.
               10 INP-DESCR2             PIC  X(075)    VALUE SPACES.
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
      *        *** AREA DA TELA DCOMDKTM  -  OUTPUT ***                *
      *----------------------------------------------------------------*

       01  OUTPUT-DKTM.
BRQ002     05  OUT-LL                    PIC S9(004) COMP VALUE +0.
           05  OUT-ZZ                    PIC S9(004) COMP VALUE +0.
           05  OUT-SENHAS                PIC  X(037)      VALUE SPACES.
           05  OUT-COMANDO               PIC  X(068)      VALUE SPACES.
           05  OUT-DADOS.
BRQ002         10 WRK-OUT-TITULO-DKTM    PIC  X(046)    VALUE SPACES.
               10 OUT-FLAGENT            PIC  X(001)    VALUE SPACES.
               10 OUT-TIPODES            PIC  9(003)    VALUE ZEROS.
BRQ002         10 WRK-OUT-OPCAO-DKTM.
BRQ002            15 WRK-OUT-OPCAO-DKTM-N
BRQ002                                   PIC  9(001)    VALUE ZEROS.
               10 OUT-TRANSAC            PIC  X(008)    VALUE SPACES.
               10 OUT-TMSTAMP            PIC  X(026)    VALUE SPACES.
               10 OUT-NOME               PIC  X(040)    VALUE SPACES.
BRQ001         10 WRK-OUT-CANAL          PIC  X(010)    VALUE SPACES.
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
                  15 OUT-VLTAROP-N       PIC ZZZ.ZZZ.ZZ9,99
                                                        VALUE ZEROS.
               10 OUT-TAXAJUR.
                  15 OUT-TAXAJUR-N       PIC ZZ9,99999  VALUE ZEROS.
               10 OUT-NMJUROS            PIC  X(004)    VALUE SPACES.
               10 OUT-VLTRFTT.
                  15 OUT-VLTRFTT-N       PIC  ZZZ.ZZZ.ZZ9,99
                                                        VALUE ZEROS.
               10 OUT-VLRJUR.
                  15 OUT-VLRJUR-N        PIC  ZZZ.ZZZ.ZZ9,99
                                                        VALUE ZEROS.
               10 OUT-PRZMED             PIC  9(003)    VALUE ZEROS.
               10 OUT-VLRIOF.
                  15 OUT-VLRIOF-N        PIC  ZZZ.ZZZ.ZZ9,99
                                                        VALUE ZEROS.
               10 OUT-QTDETIT.
                  15 OUT-QTDETIT-N       PIC  ZZ.ZZ9    VALUE ZEROS.
               10 OUT-VLRLIQD.
                  15 OUT-VLRLIQD-N       PIC  ZZZ.ZZZ.ZZ9,99
                                                        VALUE ZEROS.
BRQ002         10 OUT-LIBMID             PIC  X(003)    VALUE SPACES.
               10 OUT-GARANTI            PIC  X(020)    VALUE SPACES.
               10 OUT-AUTRZTE            PIC  X(050)    VALUE SPACES.
               10 OUT-DESCR1             PIC  X(053)    VALUE SPACES.
               10 OUT-DESCR2             PIC  X(075)    VALUE SPACES.
               10 OUT-JURMAXI.
                  15 OUT-JURMAXI-N       PIC  ZZ9,99    VALUE ZEROS.
               10 OUT-VLRMAXI.
                  15 OUT-VLRMAXI-N       PIC  ZZ.ZZZ.ZZZ.ZZ9,99
                                                        VALUE ZEROS.
               10 OUT-JRLIBER.
                  15 OUT-JRLIBER-N       PIC  ZZ9,99    VALUE ZEROS.
               10 OUT-VLLIBER.
                  15 OUT-VLLIBER-N       PIC  ZZ.ZZZ.ZZZ.ZZ9,99
                                                        VALUE ZEROS.
               10 OUT-TIPOLIB            PIC  X(001)    VALUE SPACES.
           05  OUT-MENSAG                PIC  X(079)    VALUE SPACES.

      *---------------------------------------------------------------*
      *  AREA DA BRAD0660 COM MASCARA DA TELA DCOMDKTM - SEGMENTO 00  *
      *---------------------------------------------------------------*

       01  660-DCOMDKTM.
BRQ002     05  660-LL-AREA-DKTM        PIC  9(04)  COMP  VALUE 0000.
BRQ002     05  660-LL-MENSAGEM-DKTM    PIC  9(04)  COMP  VALUE 0000.
           05  660-SENHAS-DKTM         PIC  9(04)  COMP  VALUE 0037.
           05  660-COMANDO-DKTM        PIC  9(04)  COMP  VALUE 0068.
BRQ002     05  WRK-660-TITULO-DKTM     PIC  9(04)  COMP  VALUE 0046.
           05  660-FLAGENT-DKTM        PIC  9(04)  COMP  VALUE 0001.
           05  660-TIPODES-DKTM        PIC  9(04)  COMP  VALUE 0003.
BRQ002     05  WRK-660-OPCAO-DKTM      PIC  9(04)  COMP  VALUE 0001.
           05  660-TRANSAC-DKTM        PIC  9(04)  COMP  VALUE 0008.
           05  660-TMSTAMP-DKTM        PIC  9(04)  COMP  VALUE 0026.
           05  660-NOME-DKTM           PIC  9(04)  COMP  VALUE 0040.
BRQ001     05  WRK-660-CANAL-DKTM      PIC  9(04)  COMP  VALUE 0010.
           05  660-CNPJCPF-DKTM        PIC  9(04)  COMP  VALUE 0019.
           05  660-CAMPO2-DKTM         PIC  9(04)  COMP  VALUE 0023.
           05  660-PRODUTO-DKTM        PIC  9(04)  COMP  VALUE 0003.
           05  660-DPRODUT-DKTM        PIC  9(04)  COMP  VALUE 0012.
           05  660-SPRODUT-DKTM        PIC  9(04)  COMP  VALUE 0003.
           05  660-DSPRODU-DKTM        PIC  9(04)  COMP  VALUE 0012.
           05  660-OPERAC-DKTM         PIC  9(04)  COMP  VALUE 0013.
           05  660-SITUAC-DKTM         PIC  9(04)  COMP  VALUE 0015.
           05  660-CAMPO3-DKTM         PIC  9(04)  COMP  VALUE 0013.
           05  660-CAMPO4-DKTM         PIC  9(04)  COMP  VALUE 0009.
           05  660-DTOPER-DKTM         PIC  9(04)  COMP  VALUE 0010.
           05  660-DTVCTOF-DKTM        PIC  9(04)  COMP  VALUE 0010.
           05  660-VLROPER-DKTM        PIC  9(04)  COMP  VALUE 0014.
           05  660-TPOTAXA-DKTM        PIC  9(04)  COMP  VALUE 0015.
           05  660-VLTAROP-DKTM        PIC  9(04)  COMP  VALUE 0014.
           05  660-TAXAJUR-DKTM        PIC  9(04)  COMP  VALUE 0009.
           05  660-NMJUROS-DKTM        PIC  9(04)  COMP  VALUE 0004.
           05  660-VLTRFTT-DKTM        PIC  9(04)  COMP  VALUE 0014.
           05  660-VLRJUR-DKTM         PIC  9(04)  COMP  VALUE 0014.
           05  660-PRZMED-DKTM         PIC  9(04)  COMP  VALUE 0003.
           05  660-VLRIOF-DKTM         PIC  9(04)  COMP  VALUE 0014.
           05  660-QTDETIT-DKTM        PIC  9(04)  COMP  VALUE 0006.
           05  660-VLRLIQD-DKTM        PIC  9(04)  COMP  VALUE 0014.
BRQ002     05  660-LIBMID-DKTM         PIC  9(04)  COMP  VALUE 0003.
           05  660-GARANTI-DKTM        PIC  9(04)  COMP  VALUE 0020.
           05  660-AUTRZTE-DKTM        PIC  9(04)  COMP  VALUE 0050.
           05  660-DESCR1-DKTM         PIC  9(04)  COMP  VALUE 0053.
           05  660-DESCR2-DKTM         PIC  9(04)  COMP  VALUE 0075.
           05  660-JURMAXI-DKTM        PIC  9(04)  COMP  VALUE 0006.
           05  660-VLRMAXI-DKTM        PIC  9(04)  COMP  VALUE 0017.
           05  660-JRLIBER-DKTM        PIC  9(04)  COMP  VALUE 0006.
           05  660-VLLIBER-DKTM        PIC  9(04)  COMP  VALUE 0017.
           05  660-TIPOLIB-DKTM        PIC  9(04)  COMP  VALUE 0001.
           05  660-MENSA-DKTM          PIC  9(04)  COMP  VALUE 0079.

      *---------------------------------------------------------------*
      *    AREA DE COMUNICACAO CHAMADO DCOM0402/DCOM0409/DCOM0410     *
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
FS2511         10  COMU-CNPJ-UN-X.
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
DTS001         10  COMU-TPDESC-0214         PIC  9(03)    VALUE ZEROS.
               10  COMU-AGENCIA-0214        PIC  9(05)    VALUE ZEROS.
               10  COMU-CONTA-0214          PIC  9(13)    VALUE ZEROS.
FS2511*        10  COMU-CPFCNPJ-0214        PIC  9(09)    VALUE ZEROS.
FS2511         10  COMU-CPFCNPJ-0214        PIC  X(09)    VALUE SPACES.
FS2511*        10  COMU-FILIAL-0214         PIC  9(004)    VALUE ZEROS.
FS2511         10  COMU-FILIAL-0214         PIC  X(04)    VALUE SPACES.
FS2511         10  FILLER                   PIC  X(01)    VALUE SPACES.
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
DTS001         10  COMU-NRTITLO-0214        PIC  9(05)    VALUE ZEROS.

      *----------------------------------------------------------------*
      *        *** AREA DE COMUNICACAO COM DCOM0222 ***                *
      *----------------------------------------------------------------*

       01  COMU-AREA-0215.
           05  COMU-LL-0215            PIC S9(004) COMP    VALUE ZEROS.
           05  COMU-ZZ-0215            PIC S9(004) COMP    VALUE ZEROS.
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
FS2511*            15  COMU-FILIAL-0215    PIC  9(004)    VALUE ZEROS.
FS2511             15  COMU-FILIAL-0215    PIC  X(004)    VALUE SPACES.
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

      *----------------------------------------------------------------*
      *        *** AREA DA BRAD7100 ***                                *
      *----------------------------------------------------------------*

       COPY  'I#BRAD7C'.

       COPY  'I#DCOMXE'.

      *----------------------------------------------------------------*
      *    AREA DE ACESSO AO MODULO DCOM4134                           *
      *----------------------------------------------------------------*
      *--- AREA DE ENTRADA DO ROTEADOR --> TAM. 0119 BYTES          ---*

       01  WRK-4134-ENTRADA-ROTEADOR.
           05  WRK-4134E-COD-RETORNO                 PIC  X(004).
           05  WRK-4134E-MSG-RETORNO                 PIC  X(079).
           05  WRK-4134E-RESTART                     PIC  9(005).
           05  WRK-4134E-FLAG                        PIC  X(001).
           05  WRK-4134E-CFUNC-BDSCO                 PIC  9(009).
           05  WRK-4134E-CTERM                       PIC  X(008).
           05  WRK-4134E-DANO-OPER-DESC              PIC  9(004).
           05  WRK-4134E-NSEQ-OPER-DESC              PIC  9(009).
BRQ002     05  WRK-4134E-OPCAO-MENU                  PIC  9(001)
BRQ002                                               VALUE ZEROS.
BRQ002*        3 - CONSULTAR LIBERACAO
BRQ002*        4 - CONSULTAR PENDENTES NOS CANAIS

       01  WRK-4134-SAIDA-ROTEADOR.
           05  WRK-4134S-COD-RETORNO                 PIC  X(004).
           05  WRK-4134S-MSG-RETORNO                 PIC  X(079).
           05  WRK-4134S-RESTART                     PIC  9(005).
           05  WRK-4134S-FLAG                        PIC  X(001).
           05  WRK-4134S-DADOS.
             10  WRK-4134S-TPO-LIBRC                 PIC  X(001).
             10  WRK-4134S-CFUNC-AUTRZ-PRAZO         PIC  9(009).
             10  WRK-4134S-CFUNC-AUTRZ-TAXA          PIC  9(009).
             10  WRK-4134S-CFUNC-AUTRZ-CONE          PIC  9(009).
             10  WRK-4134S-PARECER-FORML-DESC        PIC  X(240).
             10  WRK-4134S-VLIBRC-MAX                PIC  9(015)V9(2).
             10  WRK-4134S-PMAX-LIBRC-ANTCP          PIC  9(003)V9(3).
             10  WRK-4134S-PLIBRC-LIBERADO           PIC  9(003)V9(3).
             10  WRK-4134S-VLIBRC-LIBERADO           PIC  9(015)V9(2).
           05  WRK-4134S-CONSISTENCIA     REDEFINES  WRK-4134S-DADOS.
             10  WRK-4134S-TAMANHO-ERRO               PIC  9(001).
             10  WRK-4134S-TABELA-ERROS         OCCURS 2 TIMES.
               15  WRK-4134S-CAMPOS-ERRO             PIC  9(001).

      *----------------------------------------------------------------*
      *--- OBTER DADOS BASICOS      ---*
      *----------------------------------------------------------------*

       COPY 'I#DCOMXJ'.


      *----------------------------------------------------------------*
      *        *** DCOM0411 - FIM DA AREA DE WORKING ***               *
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

           MOVE  'DCOMDKTM'             TO WRK-TELA.

           IF WRK-TRANSACAO            NOT EQUAL  'DCOM0411'
              PERFORM  1100-RECEBER-CONTROLE
           ELSE
              PERFORM  2000-PROCESSAR-DCOMDKTM
           END-IF.

           IF WRK-TELA                 EQUAL  'DCOMDKTM'
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
                      WRK-4134-ENTRADA-ROTEADOR
                      WRK-4134-SAIDA-ROTEADOR.


           IF  WRK-TRANSACAO       NOT EQUAL 'DCOM0214' AND 'DCOM0222'
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
BRQ002         MOVE COMU-OPCAO-UN      TO WRK-INP-OPCAO-DKTM-N
BRQ002                                    WRK-OUT-OPCAO-DKTM-N
BRQ002
BRQ002         IF  WRK-INP-OPCAO-DKTM-N   EQUAL 4
BRQ002             MOVE 'CONSULTAR PENDENTES NOS CANAIS - DEMONSTRATIVO'
BRQ002                                 TO WRK-OUT-TITULO-DKTM
BRQ002         ELSE
BRQ002             MOVE '      CONSULTAR LIBERACAO - DEMONSTRATIVO     '
BRQ002                                 TO WRK-OUT-TITULO-DKTM
BRQ002         END-IF

               IF WRK-TRANSACAO        EQUAL 'DCOM0410'
                   IF COMU-AGENC-UN-X  NOT EQUAL SPACES AND LOW-VALUES
                                                        AND ZEROS
                       MOVE '1'        TO OUT-FLAGENT
                   ELSE
                       MOVE '2'        TO OUT-FLAGENT
                   END-IF
               END-IF
               MOVE COMU-DANO-OPER-UN  TO 4174-ENT-DANO-OPER-DESC
                                          WRK-4134E-DANO-OPER-DESC
                                          INP-OPERAC(1:4)
               MOVE COMU-NSEQ-OPER-UN  TO 4174-ENT-NSEQ-OPER-DESC
                                          WRK-4134E-NSEQ-OPER-DESC
                                          INP-OPERAC(5:9)
           ELSE
               IF  WRK-TRANSACAO            EQUAL 'DCOM0214'
                    MOVE WRK-MENSAGEM       TO COMU-AREA-0214
                    MOVE 'DCOM0402'         TO INP-TRANSAC
                                               OUT-TRANSAC
                    MOVE COMU-TIMESTAMP-0214   TO INP-TMSTAMP
                                                  OUT-TMSTAMP
BRQ002              MOVE COMU-TIPO-OPERACAO-0214
BRQ002                                      TO WRK-INP-OPCAO-DKTM-N
BRQ002                                         WRK-OUT-OPCAO-DKTM-N
                    MOVE ' '                TO INP-FLAGENT
                                               OUT-FLAGENT
                    MOVE COMU-DANO-0214     TO  4174-ENT-DANO-OPER-DESC
                                                WRK-4134E-DANO-OPER-DESC
                                                INP-OPERAC(1:4)
                    MOVE COMU-NSEQ-0214     TO  4174-ENT-NSEQ-OPER-DESC
                                                WRK-4134E-NSEQ-OPER-DESC
                                                INP-OPERAC(5:9)
                    IF  COMU-MENSAGEM-0214      NOT EQUAL SPACES
                        MOVE COMU-MENSAGEM-0214 TO OUT-MENSAG
                    END-IF
               ELSE
                    MOVE WRK-MENSAGEM           TO COMU-AREA-0215
                    MOVE 'DCOM0402'             TO INP-TRANSAC
                                                   OUT-TRANSAC
                    MOVE COMU-TIMESTAMP-0215    TO INP-TMSTAMP
                                                   OUT-TMSTAMP
BRQ002              MOVE COMU-TPOPCAO-0215      TO WRK-INP-OPCAO-DKTM
BRQ002                                             WRK-OUT-OPCAO-DKTM
                    MOVE ' '                    TO INP-FLAGENT
                                                   OUT-FLAGENT
                    MOVE COMU-DANO-0215         TO
                                                4174-ENT-DANO-OPER-DESC
                                              WRK-4134E-DANO-OPER-DESC
                                                INP-OPERAC(1:4)
                    MOVE COMU-NSEQ-0215         TO
                                                4174-ENT-NSEQ-OPER-DESC
                                              WRK-4134E-NSEQ-OPER-DESC
                                                INP-OPERAC(5:9)
                    IF  COMU-MENSAGEM-0215      NOT EQUAL SPACES
                        MOVE COMU-MENSAGEM-0215 TO OUT-MENSAG
                    END-IF
               END-IF
           END-IF.

           MOVE SPACES                 TO OUT-CAMPO3
                                          OUT-CAMPO4-X.

           PERFORM 1110-ACESSAR-DCOM4174.

           IF  4174-SAI-COD-RETORNO    EQUAL '0000' AND
               WRK-INCONSIS            EQUAL SPACES
               PERFORM 1120-ACESSAR-DCOM4134
               PERFORM 1111-ACESSAR-DCOM4266
           END-IF.

           IF  OUT-MENSAG             EQUAL SPACES
               MOVE 'TECLE <PF3> PARA VOLTAR'
                                       TO OUT-MENSAG
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------
       1101-GRAVAR-DBTEMP              SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-ENTRADA-DCOM7995
                      WRK-SAIDA-DCOM7995.

           MOVE 'I '                   TO WRK-ENT-OPCAO
           MOVE WRK-COD-USER-R         TO WRK-ENT-COD-FUNC
           MOVE LNK-IO-LTERM           TO WRK-ENT-COD-TERMINAL
           MOVE COMU-TIMESTAMP-UN      TO WRK-ENT-TIMESTAMP
           MOVE 'DCOM0411'             TO WRK-ENT-TRANS
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

      *----------------------------------------------------------------
       1102-LER-DBTEMP                 SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-ENTRADA-DCOM7995
                      WRK-SAIDA-DCOM7995.

           MOVE 'CS'                   TO WRK-ENT-OPCAO
           MOVE WRK-COD-USER-R         TO WRK-ENT-COD-FUNC
           MOVE LNK-IO-LTERM           TO WRK-ENT-COD-TERMINAL
           MOVE INP-TMSTAMP            TO WRK-ENT-TIMESTAMP
           MOVE 'DCOM0411'             TO WRK-ENT-TRANS
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

      *----------------------------------------------------------------
       1103-APAGAR-DBTEMP              SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-ENTRADA-DCOM7995
                      WRK-SAIDA-DCOM7995.

           MOVE 'ET'                   TO WRK-ENT-OPCAO
           MOVE WRK-COD-USER-R         TO WRK-ENT-COD-FUNC
           MOVE LNK-IO-LTERM           TO WRK-ENT-COD-TERMINAL
           MOVE INP-TMSTAMP            TO WRK-ENT-TIMESTAMP
           MOVE 'DCOM0411'             TO WRK-ENT-TRANS
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

      *----------------------------------------------------------------
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
                   MOVE 4174-SAI-CAG-BCRIA TO ROTENT-6437-AGEPESQ-ENV
                   PERFORM 1111-1-VALIDAR-PERFIL
                   IF  WRK-INCONSIS    EQUAL SPACES
                       PERFORM 1111-CARREGAR-DADOS-TELA
                   ELSE
                       MOVE SPACES         TO WRK-INCONSIS
                       MOVE 4174-SAI-CAG-OPER-OPER
                                           TO ROTENT-6437-AGEPESQ-ENV
                       PERFORM 1111-1-VALIDAR-PERFIL
                       IF  WRK-INCONSIS    EQUAL SPACES
                           PERFORM 1111-CARREGAR-DADOS-TELA
                       ELSE
                           MOVE 'S'        TO WRK-FLAG-ERRO
                           MOVE 'USUARIO NAO AUTORIZADO'
                                           TO WRK-MENSAGEM-ERRO
                           PERFORM 2300-RETORNAR-CHAMADOR
                       END-IF
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

           MOVE  4174-SAI-DANO-OPER-DESC   TO INP-OPERAC(1:4)
                                              OUT-OPERAC(1:4)
           MOVE  4174-SAI-NSEQ-OPER-DESC   TO INP-OPERAC(5:9)
                                              OUT-OPERAC(5:9)
           MOVE  4174-SAI-CPRODT           TO OUT-PRODUTO
           MOVE  4174-SAI-IABREV-PRODT     TO OUT-DPRODUT
           MOVE  4174-SAI-CSPROD-DESC-COML TO OUT-SPRODUT
           MOVE  4174-SAI-IRSUMO-SPROD-DESC
                                           TO OUT-DSPRODU
           MOVE  4174-SAI-IRSUMO-SIT-DESC  TO OUT-SITUAC
           MOVE  4174-SAI-IPSSOA-DESC-COML TO OUT-NOME
           MOVE  4174-SAI-CCNPJ-CPF(1:3)   TO OUT-CNPJCPF(1:3)
           MOVE  '.'                       TO OUT-CNPJCPF(4:1)
           MOVE  4174-SAI-CCNPJ-CPF(4:3)   TO OUT-CNPJCPF(5:3)
           MOVE  '.'                       TO OUT-CNPJCPF(8:1)
           MOVE  4174-SAI-CCNPJ-CPF(7:3)   TO OUT-CNPJCPF(9:3)
           MOVE  '/'                       TO OUT-CNPJCPF(12:1)
           MOVE  4174-SAI-CFLIAL-CNPJ      TO OUT-CNPJCPF(13:4)
           MOVE  '-'                       TO OUT-CNPJCPF(17:1)
           MOVE  4174-SAI-CCTRL-CNPJ-CPF   TO OUT-CNPJCPF(18:2)
           MOVE  4174-SAI-CBCO             TO OUT-CAMPO2(1:3)
           MOVE  '/'                       TO OUT-CAMPO2(4:1)
           MOVE  4174-SAI-CAG-BCRIA        TO OUT-CAMPO2(5:5)
           MOVE  '/'                       TO OUT-CAMPO2(10:1)
           MOVE  4174-SAI-CCTA-BCRIA-CLI   TO OUT-CAMPO2(11:13)

           IF  4174-SAI-CCONTR-CONVE-DESC  NOT EQUAL SPACES AND
                                           LOW-VALUES AND ZEROS
               MOVE 'CONVENIO....:'        TO OUT-CAMPO3
               MOVE 4174-SAI-CCONTR-CONVE-DESC
                                           TO OUT-CAMPO4
           END-IF.

           IF  4174-SAI-CCONTR-LIM-DESC    NOT EQUAL SPACES AND
                                           LOW-VALUES AND ZEROS
               MOVE 'CONTR LIMITE:'        TO OUT-CAMPO3
               MOVE 4174-SAI-CCONTR-LIM-DESC
                                           TO OUT-CAMPO4
           END-IF.

           MOVE  4174-SAI-VOPER-DESC-COML  TO OUT-VLROPER-N
           MOVE  4174-SAI-VTAC-OPER-DESC   TO OUT-VLTAROP-N
           MOVE  4174-SAI-VTARIF-REG-TITLO TO OUT-VLTRFTT-N
           MOVE  4174-SAI-VJURO-OPER-DESC  TO OUT-VLRJUR-N
           MOVE  4174-SAI-VIOF-OPER-DESC   TO OUT-VLRIOF-N
           MOVE  4174-SAI-VLIQ-OPER-DESC   TO OUT-VLRLIQD-N
           MOVE  4174-SAI-PTX-JURO-MES     TO OUT-TAXAJUR-N
           MOVE  'A.M.'                    TO OUT-NMJUROS
           MOVE  4174-SAI-ITPO-TX-JURO-DESC TO OUT-TPOTAXA
           MOVE  4174-SAI-DVCTO-FNAL-OPER  TO OUT-DTVCTOF
           INSPECT OUT-DTVCTOF REPLACING ALL '.' BY '/'.
           MOVE  4174-SAI-DINIC-OPER-DESC  TO OUT-DTOPER
           INSPECT OUT-DTOPER  REPLACING ALL '.' BY '/'.
           MOVE  4174-SAI-TMED-PONDE-OPER  TO OUT-PRZMED
           MOVE  4174-SAI-QTITLO-OPER-APURA TO OUT-QTDETIT-N.
           MOVE  4174-SAI-ITPO-GARNT       TO OUT-GARANTI.
           MOVE  4174-SAI-CTPO-DESC-COML   TO OUT-TIPODES.

BRQ001     EVALUATE 4174-SAI-CTERM
             WHEN 'NETEMPR'
             WHEN 'MOBILE '
               INITIALIZE WRK-5505E-ENTRADA
               MOVE 150             TO WRK-5505E-CEVNTO-DESC-COML
               PERFORM 1112-ACESSAR-DCOM5505
               IF  WRK-5505S-COD-RETORNO EQUAL ZEROS
                   MOVE 'MOBILE '   TO WRK-OUT-CANAL
               ELSE
                   MOVE 'NETEMPR'   TO WRK-OUT-CANAL
               END-IF

             WHEN 'TELEBCO'
               MOVE 'TELEBCO'       TO WRK-OUT-CANAL

             WHEN OTHER
               MOVE 'AGENCIA'       TO WRK-OUT-CANAL
BRQ001     END-EVALUATE.

BRQ002     IF  WRK-INP-OPCAO-DKTM-N    EQUAL 4
BRQ002         MOVE 'DP'               TO WRK-FUNCAO-5490
BRQ002         PERFORM 1113-OBTER-INFO-LIBRC-MIDIA
BRQ002     END-IF.
BRQ002
BRQ002     MOVE 'CO'                   TO WRK-FUNCAO-5490.
BRQ002
BRQ002     PERFORM 1113-OBTER-INFO-LIBRC-MIDIA.

      *----------------------------------------------------------------*
       1111-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA OBTEM A PERIODICIDADE DA TAXA                     *
      *----------------------------------------------------------------*
       1111-ACESSAR-DCOM4266           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE 4266E-ENTRADA
                      4266S-SAIDA

           MOVE 'DCOM4266'              TO  WRK-MODULO.

           MOVE  WRK-COD-USER           TO  4266E-CFUNC-BDSCO.
           MOVE  LNK-IO-LTERM           TO  4266E-CTERM.
           MOVE  INP-OPERAC          (1:4)
                                        TO  4266E-DANO-OPER-DESC.
           MOVE  INP-OPERAC          (5:9)
                                        TO  4266E-NSEQ-OPER-DESC.
           MOVE  OUT-CNPJCPF         (1:3)
                                        TO  4266E-CCNPJ-CPF(1:3).
           MOVE  OUT-CNPJCPF         (5:3)
                                        TO  4266E-CCNPJ-CPF(4:3).
           MOVE  OUT-CNPJCPF         (9:3)
                                        TO  4266E-CCNPJ-CPF(7:3).
           MOVE  OUT-CNPJCPF         (13:4)
                                        TO  4266E-CFLIAL-CNPJ.

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

BRQ001*----------------------------------------------------------------*
       1112-ACESSAR-DCOM5505           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-5505E-RETORNO.

           MOVE WRK-COD-USER-R         TO WRK-5505E-CFUNC-BDSCO.
           MOVE LNK-IO-LTERM           TO WRK-5505E-CTERM.
           MOVE WRK-INP-DANO           TO WRK-5505E-DANO-OPER-DESC.
           MOVE WRK-INP-NSEQ           TO WRK-5505E-NSEQ-OPER-DESC.
           MOVE 'DCOM5505'             TO WRK-MODULO.

           CALL WRK-MODULO             USING WRK-5505E-ENTRADA
                                             WRK-5505E-RETORNO
                                             ERRO-AREA
                                             WRK-SQLCA.

           EVALUATE  WRK-5505S-COD-RETORNO
              WHEN  0099
                    MOVE 'DB2'         TO ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-EVALUATE.

      *----------------------------------------------------------------*
       1112-99-FIM.                  EXIT.
BRQ001*----------------------------------------------------------------*

BRQ002*----------------------------------------------------------------*
BRQ002*     OBTER PARAMETRO DE LIBERACAO NAS MIDIAS
BRQ002*----------------------------------------------------------------*
BRQ002 1113-OBTER-INFO-LIBRC-MIDIA     SECTION.
BRQ002*----------------------------------------------------------------*
BRQ002
BRQ002     INITIALIZE WRK-5490-ENTRADA
BRQ002                WRK-5490-SAIDA
BRQ002                ERRO-AREA
BRQ002                WRK-SQLCA.
BRQ002
BRQ002     MOVE 4174-ENT-DANO-OPER-DESC   TO WRK-E5490-DANO-OPER.
BRQ002     MOVE 4174-ENT-NSEQ-OPER-DESC   TO WRK-E5490-NSEQ-OPER.
BRQ002     MOVE 4174-SAI-CTPO-DESC-COML   TO WRK-E5490-CTPO-DESC-COML.
BRQ002     MOVE WRK-FUNCAO-5490           TO WRK-E5490-FUNCAO.
BRQ002     MOVE 'DCOM5490'                TO WRK-MODULO.
BRQ002
BRQ002     CALL  WRK-MODULO            USING WRK-5490-ENTRADA
BRQ002                                       WRK-5490-SAIDA
BRQ002                                       ERRO-AREA
BRQ002                                       WRK-SQLCA.
BRQ002
BRQ002     IF  WRK-S5490-COD-RETORNO   EQUAL 99
BRQ002         MOVE 'DB2'              TO ERR-TIPO-ACESSO
BRQ002         PERFORM 9999-PROCESSAR-ROTINA-ERRO
BRQ002     END-IF.
BRQ002
BRQ002     IF  WRK-S5490-LIBRC-MIDIA   EQUAL 'S'
BRQ002         MOVE 'SIM'              TO OUT-LIBMID
BRQ002     ELSE
BRQ002         MOVE 'NAO'              TO OUT-LIBMID
BRQ002     END-IF.
BRQ002
BRQ002     IF  WRK-FUNCAO-5490         EQUAL 'DP' AND
BRQ002         WRK-S5490-COD-RETORNO   NOT EQUAL ZEROS
BRQ002         MOVE 'S'                TO WRK-FLAG-ERRO
BRQ002         MOVE WRK-S5490-MSG-RETORNO
BRQ002                                 TO WRK-MENSAGEM-ERRO
BRQ002         PERFORM 2300-RETORNAR-CHAMADOR
BRQ002     END-IF.
BRQ002
BRQ002*----------------------------------------------------------------*
BRQ002 1113-99-FIM.                    EXIT.
BRQ002*----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1120-ACESSAR-DCOM4134           SECTION.
      *----------------------------------------------------------------*

BRQ002     MOVE WRK-INP-OPCAO-DKTM-N   TO WRK-4134E-OPCAO-MENU
           MOVE WRK-COD-USER           TO WRK-4134E-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO WRK-4134E-CTERM

           MOVE 'DCOM4134'             TO WRK-MODULO.

           CALL WRK-MODULO             USING WRK-4134-ENTRADA-ROTEADOR
                                             WRK-4134-SAIDA-ROTEADOR
                                             ERRO-AREA
                                             WRK-SQLCA.

           EVALUATE WRK-4134S-COD-RETORNO
               WHEN '0000'
                   STRING 'PRAZO: 'WRK-4134S-CFUNC-AUTRZ-PRAZO
                          ' TAXA: 'WRK-4134S-CFUNC-AUTRZ-TAXA
                          ' CONE: 'WRK-4134S-CFUNC-AUTRZ-CONE
                   DELIMITED BY SIZE   INTO OUT-AUTRZTE
                   MOVE WRK-4134S-PARECER-FORML-DESC(1:53)
                                       TO OUT-DESCR1
                   MOVE WRK-4134S-PARECER-FORML-DESC(54:75)
                                       TO OUT-DESCR2
                   MOVE WRK-4134S-PMAX-LIBRC-ANTCP
                                       TO OUT-JURMAXI-N
                   MOVE WRK-4134S-VLIBRC-MAX
                                       TO OUT-VLRMAXI-N

                   IF  WRK-4134S-PLIBRC-LIBERADO NOT EQUAL ZEROS OR
                       WRK-4134S-VLIBRC-LIBERADO NOT EQUAL ZEROS
                       MOVE WRK-4134S-PLIBRC-LIBERADO
                                       TO OUT-JRLIBER-N
                       MOVE WRK-4134S-VLIBRC-LIBERADO
                                       TO OUT-VLLIBER-N
                   ELSE
                       MOVE SPACES     TO OUT-JRLIBER
                       MOVE SPACES     TO OUT-VLLIBER
                   END-IF

               WHEN '0099'
                   MOVE 'DB2'          TO ERR-TIPO-ACESSO
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                   MOVE 'S'            TO WRK-FLAG-ERRO
                   MOVE WRK-4134S-MSG-RETORNO
                                       TO WRK-MENSAGEM-ERRO
                   PERFORM 2300-RETORNAR-CHAMADOR

           END-EVALUATE.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000-PROCESSAR-DCOMDKTM         SECTION.
      *----------------------------------------------------------------*

           MOVE  WRK-MENSAGEM          TO  INPUT-DKTM.
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

               WHEN (INP-PFK          EQUAL  'C'  OR
                    (INP-PFK          EQUAL  '.'  AND
                     INP-COMANDO      EQUAL  'PFK11'))AND
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
                     MOVE 'TECLE <PF3> PARA VOLTAR'
                                      TO OUT-MENSAG

               WHEN  OTHER
                     MOVE  'PF INVALIDA'   TO  OUT-MENSAG

           END-EVALUATE.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2200-DEVOLVER-TELA              SECTION.
      *----------------------------------------------------------------*

           MOVE  INP-TRANSAC                    TO OUT-TRANSAC
           MOVE  INP-FLAGENT                    TO OUT-FLAGENT
BRQ002     MOVE  WRK-INP-TITULO-DKTM            TO WRK-OUT-TITULO-DKTM
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
BRQ002     MOVE  INP-LIBMID                     TO OUT-LIBMID
           MOVE  INP-GARANTI                    TO OUT-GARANTI
           MOVE  INP-AUTRZTE                    TO OUT-AUTRZTE
           MOVE  INP-DESCR1                     TO OUT-DESCR1
           MOVE  INP-DESCR2                     TO OUT-DESCR2
           MOVE  INP-JURMAXI-X                  TO OUT-JURMAXI
           MOVE  INP-VLRMAXI-X                  TO OUT-VLRMAXI
           MOVE  INP-JRLIBER-X                  TO OUT-JRLIBER
           MOVE  INP-VLLIBER-X                  TO OUT-VLLIBER
           MOVE  INP-TMSTAMP                    TO OUT-TMSTAMP
           MOVE  INP-NOME                       TO OUT-NOME
BRQ001     MOVE  WRK-INP-CANAL                  TO WRK-OUT-CANAL
           MOVE  INP-CNPJCPF                    TO OUT-CNPJCPF
           MOVE  INP-CAMPO2                     TO OUT-CAMPO2
           MOVE  INP-PRODUTO                    TO OUT-PRODUTO
           MOVE  INP-DPRODUT                    TO OUT-DPRODUT
           MOVE  INP-SPRODUT                    TO OUT-SPRODUT
           MOVE  INP-DSPRODU                    TO OUT-DSPRODU
           MOVE  INP-OPERAC                     TO OUT-OPERAC
           MOVE  INP-SITUAC                     TO OUT-SITUAC
           MOVE  INP-CAMPO3                     TO OUT-CAMPO3
           MOVE  INP-CAMPO4                     TO OUT-CAMPO4-X
           MOVE  INP-TIPOLIB                    TO OUT-TIPOLIB.
           MOVE  INP-TIPODES                    TO OUT-TIPODES.
BRQ002     MOVE  WRK-INP-OPCAO-DKTM             TO WRK-OUT-OPCAO-DKTM.

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

           MOVE WRK-DADOS-DBTEMP       TO COMU-DADOS-UN.

           PERFORM 1103-APAGAR-DBTEMP.

           IF  WRK-FLAG-ERRO           EQUAL 'S'
               MOVE WRK-MENSAGEM-ERRO  TO COMU-MENSAGEM-UN
               IF  INP-TRANSAC         EQUAL 'DCOM0402'
                   MOVE INP-OPERAC     TO COMU-OPER-UN
               END-IF
           END-IF.

           MOVE  INP-SENHAS            TO  COMU-SENHAS-UN.
           MOVE  'DCOM0411'            TO  COMU-TRANSACAO-UN
           MOVE  WRK-CHNG              TO  WRK-FUNCAO.
           MOVE  WRK-CHAMADOR-DBTEMP   TO  WRK-TELA
           MOVE  COMU-AREA-UN          TO  WRK-MENSAGEM.

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
           MOVE  'DCOM0411'            TO  WRK-TELA
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
           MOVE  'DCOM0411'            TO  WRK-TELA
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

BRQ002     MOVE WRK-INP-OPCAO-DKTM-N        TO COMU-TIPO-OPERACAO-0214
           MOVE INP-TIPODES                 TO COMU-TPDESC-0214
           MOVE INP-TMSTAMP                 TO COMU-TIMESTAMP-0214
           MOVE INP-OPERAC                  TO COMU-OPERACAO-0214
           MOVE INP-CAMPO2(5:5)             TO COMU-AGENCIA-0214
           MOVE INP-CAMPO2(11:13)           TO COMU-CONTA-0214
           STRING INP-CNPJCPF(1:3)
                  INP-CNPJCPF(5:3)
                  INP-CNPJCPF(9:3)        DELIMITED BY SIZE
                                          INTO COMU-CPFCNPJ-0214
           MOVE INP-CNPJCPF(13:4)           TO COMU-FILIAL-0214
           MOVE INP-CNPJCPF(18:2)           TO COMU-CONTROLE-0214
           MOVE 'DCOM0411'                  TO COMU-TRANSACAO-0214
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
           MOVE INP-OPERAC                  TO COMU-OPERACAO-0215
           MOVE 'DCOM0411'                  TO COMU-TRANSACAO-0215
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
PROC01     MOVE 'DCOM0411'             TO  COMU-TRANSACAO-UN
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

BRQ002     MOVE LENGTH                 OF OUTPUT-DKTM
BRQ002                                 TO 660-LL-MENSAGEM-DKTM
BRQ002                                    OUT-LL
BRQ002     MOVE LENGTH                 OF 660-DCOMDKTM
BRQ002                                 TO 660-LL-AREA-DKTM
BRQ002
           CALL  'BRAD0660'            USING  OUTPUT-DKTM
                                              660-DCOMDKTM.

           IF RETURN-CODE              NOT EQUAL  ZEROS
              MOVE  'APL'              TO  ERR-TIPO-ACESSO
              MOVE  RETURN-CODE        TO  WRK-RETURN-CODE
              MOVE  10                 TO  WRK-LOCAL-ERRO
              MOVE  WRK-ERRO-BRAD0660  TO  ERR-TEXTO
              PERFORM  9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE  WRK-ISRT              TO  WRK-FUNCAO.
           MOVE  OUTPUT-DKTM           TO  WRK-MENSAGEM.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           IF ERR-MODULO    NOT EQUAL  SPACES  AND  LOW-VALUES
              MOVE  'DCOM0411'         TO  ERR-PGM
           ELSE
              MOVE  'SENHAS02'         TO  ERR-PGM
              MOVE  'DCOM0411'         TO  ERR-MODULO
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
