      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID.     DCOM0513.
       AUTHOR.         FABRICA - ALTRAN.
      *================================================================*
      *                          A L T R A N                           *
      *----------------------------------------------------------------*
      *     PROGRAMA....: DCOM0513                                     *
      *     PROGRAMADOR.: FABRICA - ALTRAN                             *
      *     ANALISTA ...: EMERSON ARTUR FRANCO                         *
      *     DATA........: 20/09/2007                                   *
      *----------------------------------------------------------------*
      *     OBJETIVO....: CONSULTAR ESTORNO DE CONTRATO LIMITE         *
      *----------------------------------------------------------------*
      *     TELAS.......:                                              *
      *     DCOMEMTM....: ESTORNO DE CONTRATO LIMITE - DADOS BASICOS   *
      *----------------------------------------------------------------*
      *     INC'S.......:                                              *
      *     I#DCOM11 - AREA PARA ACESSAR CNPJ/CPF                      *
      *     I#BRAD7C - AREA DE ERRO DA BRAD7100                        *
      *     I#DCOMSG - TABELA DE MENSAGENS DE RETORNO DO               *
      *                SISTEMA DCOM (ON-LINE)                          *
      *----------------------------------------------------------------*
      *     MODULOS.....:                                              *
      *     BRAD0660 - INSERIR CARACTERES DE NULL NA MENSAGEM DE SAIDA *
      *     POOL5005 - CONTROLE DO TRANSITO DE MENSAGENS NO IMS/DC     *
      *     BRAD7100 - TRATAMENTO DE ERROS QUANDO PROGRAMA INVALIDO    *
      *     BRAD7600 - FORNECE DATA E HORA CORRENTE DO SISTEMA         *
      *     DCOM6437 - VALIDA PERFIL DO USUARIO                        *
      *     DCOM4128 - BUSCA DADOS BASICOS                             *
      *     DCOM4170 - BUSCA NOME DO CLIENTE NAO CORRENTISTA           *
      *     DCOM6408 - BUSCA NOME DO CLIENTE CORRENTISTA               *
      *     DCOM6416 - VALIDA E BUSCA DADOS DA AGENCIA                 *
      *----------------------------------------------------------------*
      *     NAVEGACAO...:                                              *
      *     CHAMADO POR.: DCOM0508           CHAMA: -                  *
      *                   DCOM0509                                     *
      *                   DCOM0510                                     *
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
       01  FILLER                      PIC  X(50)          VALUE
           '*** DCOM0513 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*
       77  WIND                        PIC  9(09)          VALUE ZEROS.
       77  WIND2                       PIC  9(09)          VALUE ZEROS.
       77  WRK-MSG003                  PIC  X(079)         VALUE
           'CONSULTA EFETUADA COM SUCESSO. TECLE <PF3> PARA RETORNAR'.

       01     WRK-CHAVE-4170.
         05   WRK-CONTRATO-4170        PIC  9(09)          VALUE ZEROS.
         05   WRK-FILLER               PIC  X(36)          VALUE SPACES.

       01     WRK-MENSAG-4170.
         05   WRK-FILLER-4170          PIC  X(13) VALUE 'BASE < DCOM >'.
         05   WRK-RESTO-4170           PIC  X(66) VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** LINHA DA TELA DE SUBPRODUTOS ***'.
      *---------------------------------------------------------------*
       01  WRK-AUXILIAR-TELA.
           05  WRK-LIN-SUBPROD.
               10  WRK-LIN-SUBP-NRO1    PIC  X(003)     VALUE SPACES.
               10  WRK-LIN-HIFEM1       PIC  X(1)       VALUE '-'.
               10  WRK-LIN-SUBP-DESC1   PIC  X(006)     VALUE SPACES.
               10  WRK-LIN-BARRA1       PIC  X(3)       VALUE ' / '.
               10  WRK-LIN-SUBP-NRO2    PIC  X(003)     VALUE SPACES.
               10  WRK-LIN-HIFEM2       PIC  X(1)       VALUE '-'.
               10  WRK-LIN-SUBP-DESC2   PIC  X(006)     VALUE SPACES.
               10  WRK-LIN-BARRA2       PIC  X(3)       VALUE ' / '.
               10  WRK-LIN-SUBP-NRO3    PIC  X(003)     VALUE SPACES.
               10  WRK-LIN-HIFEM3       PIC  X(1)       VALUE '-'.
               10  WRK-LIN-SUBP-DESC3   PIC  X(006)     VALUE SPACES.
               10  WRK-LIN-BARRA3       PIC  X(3)       VALUE ' / '.
               10  WRK-LIN-SUBP-NRO4    PIC  X(003)     VALUE SPACES.
               10  WRK-LIN-HIFEM4       PIC  X(1)       VALUE '-'.
               10  WRK-LIN-SUBP-DESC4   PIC  X(006)     VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DE ATRIBUTOS ***'.
      *---------------------------------------------------------------*
      *--- ALPHA    - NORMAL    - PROTEGIDO    - NAO POS CURSOR ---*
       01  WRK-225                     PIC S9(08) COMP     VALUE +225.
      *--- ALPHA    - NORMAL    - DESPROTEGIDO - POS. CURSOR    ---*
       01  WRK-49345                   PIC S9(08) COMP     VALUE +49345.
      *--- NUMERICO - BRILHANTE - DESPROTEGIDO - POS. CURSOR    ---*
       01  WRK-49369                   PIC S9(08) COMP     VALUE +49369.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*
       01  WRK-AUXILIARES.
           05  WRK-MODULO              PIC  X(08)          VALUE SPACES.
           05  WRK-INCONSIS            PIC  X(01)          VALUE SPACES.
           05  WRK-ERRO-MOD            PIC  X(01)          VALUE 'N'.
           05  WRK-PFK06               PIC  X(01)          VALUE 'N'.
           05  WRK-ERRO-ACESSO         PIC  X(01)          VALUE SPACES.

      *----------------------------------------------------------------
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DE COMUNICACAO DCOM4170 ***'.
      *----------------------------------------------------------------

       01     4170-ENTRADA.
          05   4170-ENT-COD-RET           PIC  X(04)       VALUE SPACES.
          05   4170-ENT-MSG-RET           PIC  X(79)       VALUE SPACES.
          05   4170-ENT-RESTART           PIC  9(05)       VALUE ZEROS.
          05   4170-ENT-FLAG              PIC  X(01)       VALUE ZEROS.
          05   4170-ENT-START-COUNT       PIC  X(01)       VALUE SPACES.
          05   4170-ENT-CFUNC-BDSCO       PIC  9(09)       VALUE ZEROS.
          05   4170-ENT-CTERM             PIC  X(08)       VALUE SPACES.
          05   4170-ENT-CCHAVE-ELMTO-DESC PIC  X(45)       VALUE SPACES.
          05   4170-ENT-CINDCD-PSSOA-DESC PIC  X(02)       VALUE SPACES.
          05   4170-ENT-CELMTO-DESC-COML  PIC  9(03)       VALUE ZEROS.

       01     4170-SAIDA.
         05   4170-SAI-COD-RET           PIC  X(004)       VALUE SPACES.
         05   4170-SAI-MSG-RET           PIC  X(079)       VALUE SPACES.
         05   4170-SAI-RESTART           PIC  9(005)       VALUE ZEROS.
         05   4170-SAI-FLAG              PIC  X(001)       VALUE ZEROS.
         05   4170-SAI-QTD-RETORNA       PIC  9(003)       VALUE ZEROS.
         05   4170-SAI-COUNT             PIC  9(009)       VALUE ZEROS.
         05   4170-SAI-DADOS.
FS2511*    10 4170-SAI-CCNPJ-CPF         PIC  9(009)       VALUE ZEROS.
FS2511     10 4170-SAI-CCNPJ-CPF         PIC  X(09)        VALUE SPACES.
FS2511*    10 4170-SAI-CFLIAL-CNPJ       PIC  9(005)       VALUE ZEROS.
FS2511     10 4170-SAI-CFLIAL-CNPJ       PIC  X(004)       VALUE SPACES.
FS2511     10 4170-SAI-CFLIAL-CNPJ       PIC  X(04)        VALUE SPACES.
           10 4170-SAI-CCTRL-CNPJ-CPF    PIC  9(002)       VALUE ZEROS.
           10 4170-SAI-CDOCTO-ID         PIC  X(015)       VALUE SPACES.
           10 4170-SAI-IPSSOA-DESC-COML  PIC  X(060)       VALUE SPACES.
           10 4170-SAI-CCLUB             PIC  9(010)       VALUE ZEROS.
           10 4170-SAI-CBCO              PIC  9(003)       VALUE ZEROS.
           10 4170-SAI-NOME-BANCO        PIC  X(040)       VALUE SPACES.
           10 4170-SAI-CAG-BCRIA         PIC  9(005)       VALUE ZEROS.
           10 4170-SAI-NOME-AGENCIA      PIC  X(040)       VALUE SPACES.
           10 4170-SAI-CCTA-BCRIA-CLI    PIC  9(013)       VALUE ZEROS.
           10 4170-SAI-CPOSTO-SERVC      PIC  9(003)       VALUE ZEROS.
           10 4170-SAI-IPSSOA-CONJG      PIC  X(040)       VALUE SPACES.
           10 4170-SAI-CCPF-CONJG        PIC  9(009)       VALUE ZEROS.
           10 4170-SAI-CCTRL-CPF-CONJG   PIC  9(002)       VALUE ZEROS.
           10 4170-SAI-CDOCTO-ID-CONJG   PIC  X(015)       VALUE SPACES.
           10 4170-SAI-CINDCD-CONJG-AUTRZ
                                         PIC  X(001)       VALUE SPACES.
           10 4170-SAI-CCEP              PIC  9(005)       VALUE ZEROS.
           10 4170-SAI-CCEP-COMPL        PIC  9(003)       VALUE ZEROS.
           10 4170-SAI-ELOGDR            PIC  X(040)       VALUE SPACES.
           10 4170-SAI-CSGL-UF           PIC  X(002)       VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DE COMUNICACAO PADRAO             ***'.
      *----------------------------------------------------------------*
       01  WRK-COMU-AREA.
           05  WRK-COMU-LL             PIC S9(004) COMP    VALUE ZEROS.
           05  WRK-COMU-ZZ             PIC S9(004) COMP    VALUE ZEROS.
           05  WRK-COMU-TRANCODE.
               10 WRK-COMU-TRANSACAO   PIC  X(008)         VALUE SPACES.
               10 FILLER               PIC  X(006)         VALUE SPACES.
               10 WRK-COMU-PFK1        PIC  X(001)         VALUE SPACES.
           05  WRK-COMU-SENHAS         PIC  X(037)         VALUE SPACES.
           05  WRK-COMU-COMANDO        PIC  X(068)         VALUE SPACES.
           05  WRK-COMU-DADOS.
               10  WRK-COMU-TPOPCAO        PIC  X(001)    VALUE SPACES.
               10  WRK-COMU-TMSTAMP        PIC  X(026)    VALUE SPACES.
               10  WRK-COMU-CCPFCNPJ       PIC  9(009)    VALUE ZEROS.
FS2511*        10  WRK-COMU-CFLIAL         PIC  9(004)    VALUE ZEROS.
FS2511         10  WRK-COMU-CFLIAL         PIC  X(04)     VALUE SPACES.
               10  WRK-COMU-CCTRL          PIC  9(002)    VALUE ZEROS.
               10  WRK-COMU-BANCO          PIC  9(003)    VALUE ZEROS.
               10  WRK-COMU-AGENCIA        PIC  9(005)    VALUE ZEROS.
               10  WRK-COMU-CONTA          PIC  9(013)    VALUE ZEROS.
               10  WRK-COMU-OPERACAO       PIC  9(013)    VALUE ZEROS.
               10  FILLER REDEFINES    WRK-COMU-OPERACAO.
                   15 WRK-COMU-DANO        PIC  9(004).
                   15 WRK-COMU-NSEQ        PIC  9(009).
               10  WRK-COMU-CONTRATO-LIM   PIC  9(009)    VALUE ZEROS.
               10  WRK-COMU-VERSAO         PIC  9(003)    VALUE ZEROS.
               10  WRK-COMU-PAG            PIC  9(004)    VALUE ZEROS.
               10  WRK-COMU-OCORR          PIC  9(008)    VALUE ZEROS.
           05  WRK-COMU-MENSA              PIC  X(079)    VALUE SPACES.
           05  WRK-COMU-ACAO               PIC  X(001)    VALUE SPACES.


      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DE COMUNICACAO PFK ***'.
      *----------------------------------------------------------------*
       01  WRK-COMU-AREA-PFK.
           05  WRK-COMU-LL-PFK             PIC S9(04) COMP  VALUE +124.
           05  WRK-COMU-ZZ-PFK             PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-COMU-TRANCODE.
               10  WRK-COMU-TRANSACAO-PFK  PIC  X(08)      VALUE SPACES.
               10  FILLER              PIC  X(06)          VALUE SPACES.
               10  WRK-COMU-PFK            PIC  X(01)      VALUE SPACES.
           05  WRK-COMU-SENHAS-PFK     PIC  X(37)          VALUE SPACES.
           05  WRK-COMU-COMANDO-PFK    PIC  X(68)          VALUE SPACES.

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
           05  FILLER                  REDEFINES WRK-COD-DEPTO.
               10  FILLER              PIC  X(02).
               10  WRK-COD-DEPTO-N     PIC  9(04).

           05  WRK-MENSAGEM.
               10  FILLER              PIC  X(04)          VALUE SPACES.
               10  WRK-TRANSACAO       PIC  X(08)          VALUE SPACES.
               10  FILLER              PIC  X(1988)        VALUE SPACES.

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
           '*** AREA DO MODULO DCOM4128 ***'.
      *----------------------------------------------------------------*

       01  ENTRADA-ROTEADOR.
           03  ROTENT-COD-RETORNO                  PIC  X(04).
           03  ROTENT-MSG-RETORNO                  PIC  X(79).
           03  ROTENT-RESTART                      PIC  9(05).
           03  ROTENT-FLAG                         PIC  X(01).
           03  ROTENT-CFUNC-BDSCO                  PIC  9(09).
           03  ROTENT-CTERM                        PIC  X(08).
           03  ROTENT-CCONTR-LIM-DESC              PIC  9(09).
           03  ROTENT-CVRSAO-CONTR-LIM             PIC  9(03).
           03  ROTENT-FLAG-FILTRO                  PIC  X(01).

       01  SAIDA-ROTEADOR.
           03  ROTSAI-HEADER.
               05  ROTSAI-COD-RETORNO              PIC  X(04).
               05  ROTSAI-MSG-RETORNO              PIC  X(79).
               05  ROTSAI-RESTART                  PIC  9(05).
               05  ROTSAI-FLAG                     PIC  X(01).
           03  ROTSAI-DADOS.
               05  ROTSAI-CPRODT                   PIC  9(03).
               05  ROTSAI-ISIT-DESC-COML           PIC  X(30).
               05  ROTSAI-IRSUMO-SIT-DESC          PIC  X(15).
FS2511*        05  ROTSAI-CCNPJ-CPF                PIC  9(09).
FS2511         05  ROTSAI-CCNPJ-CPF                PIC  X(09).
               05  ROTSAI-CFLIAL-CNPJ              PIC  9(04).
               05  ROTSAI-CCTRL-CNPJ-CPF           PIC  9(02).
               05  ROTSAI-IPSSOA-DESC-COML         PIC  X(60).
               05  ROTSAI-CBCO                     PIC  9(03).
               05  ROTSAI-CBCO-DESC                PIC  X(20).
               05  ROTSAI-CAG-BCRIA                PIC  9(05).
               05  ROTSAI-CAG-BCRIA-DESC           PIC  X(30).
               05  ROTSAI-CCTA-BCRIA-CLI           PIC  9(13).
               05  ROTSAI-DINIC-VGCIA-CONTR        PIC  X(10).
               05  ROTSAI-CINDCD-RENOV-AUTOM       PIC  X(01).
               05  ROTSAI-DVCTO-CONTR-LIM          PIC  X(10).
               05  ROTSAI-TCONTR-LIM-DESC          PIC  9(05).
               05  ROTSAI-VCONTR-LIM-DESC          PIC  9(15)V99.
               05  ROTSAI-VDISPN-CONTR-LIM         PIC  9(15)V99.
               05  ROTSAI-CTPO-COBR-TAC            PIC  9(03).
               05  ROTSAI-ITPO-COBR-TAC            PIC  X(30).
               05  ROTSAI-VTAC-CONTR-LIM           PIC  9(15)V99.
               05  ROTSAI-PARECER                  PIC  X(240).
               05  ROTSAI-CGARAN                   PIC  9(003).
               05  ROTSAI-IGARAN                   PIC  X(040).
               05  ROTSAI-VUTILZ-LIM-DESC          PIC  9(15)V99.

           03  ROTSAI-CONSISTENCIA REDEFINES ROTSAI-DADOS.
               05  ROTSAI-TAMANHO-ERRO             PIC  9(01).
               05  ROTSAI-TABELA-ERROS             OCCURS 01 TIMES.
                   07  ROTSAI-CAMPOS-ERRO          PIC  9(01).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DO MODULO DCOM4113 **'.
      *----------------------------------------------------------------*
       01  ENTRADA-ROTEADOR-4113.
           03  ROTENT-HEADER-4113.
               05  ROTENT-COD-RETORNO-4113     PIC  X(004).
               05  ROTENT-MSG-RETORNO-4113     PIC  X(079).
               05  ROTENT-RESTART-4113         PIC  9(005).
               05  ROTENT-FLAG-4113            PIC  9(001).
               05  ROTENT-START-COUNT-4113     PIC  X(001).
               05  ROTENT-CFUNC-BDSCO-4113     PIC  9(009).
               05  ROTENT-CTERM-4113           PIC  X(008).
           03  ROTENT-DADOS-4113.
               05  ROTENT-CPRODT-4113          PIC  9(003).
               05  ROTENT-CCONTR-LIM-DESC-4113 PIC  9(009).

       01  SAIDA-ROTEADOR-4113.
           03  ROTSAI-HEADER-4113.
               05  ROTSAI-COD-RETORNO-4113      PIC  X(004).
               05  ROTSAI-MSG-RETORNO-4113      PIC  X(079).
               05  ROTSAI-RESTART-4113          PIC  9(005).
               05  ROTSAI-FLAG-4113             PIC  X(001).
               05  ROTSAI-QTDE-RETORNADA-4113   PIC  9(003).
               05  ROTSAI-COUNT-4113            PIC  9(009).

           03  ROTSAI-DADOS-4113.
               05  ROTSAI-CPRODT-4113           PIC  9(003).
               05  ROTSAI-IPRODT-4113           PIC  X(060).
               05  ROTSAI-IABREV-PRODT-4113     PIC  X(012).
               05  ROTSAI-CSGL-PRODT-4113       PIC  X(006).
               05  ROTSAI-TABELA-SAIDA-4113  OCCURS 10  TIMES.
                   07  ROTSAI-CSPROD-DESC-COML-4113   PIC 9(003).
                   07  ROTSAI-ISPROD-DESC-COML-4113   PIC X(040).
                   07  ROTSAI-IRSUMO-SPROD-4113       PIC X(010).
                   07  ROTSAI-CSGL-SPROD-4113         PIC X(006).
           03  ROTSAI-CONSISTENCIA-4113  REDEFINES  ROTSAI-DADOS-4113.
               05  ROTSAI-TAMANHO-ERRO-4113     PIC  9(001).
               05  ROTSAI-TABELA-ERROS-4113  OCCURS  2 TIMES.
                   07  ROTSAI-CAMPOS-ERRO-4113  PIC  9(001).


      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '**  AREA DO MODULO DCOM6408 **'.
      *----------------------------------------------------------------*
       COPY 'I#DCOMX3'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DO MODULO DCOM6437 **'.
      *----------------------------------------------------------------*
       COPY 'I#DCOMXE'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DO MODULO DCOM6416 **'.
      *----------------------------------------------------------------*
       01  ENTRADA-ROTEADOR-6416.
           03  ROTENT-COD-RETORNO-6416 PIC  X(004).
           03  ROTENT-MSG-RETORNO-6416 PIC  X(079).
           03  ROTENT-RESTART-6416     PIC  9(005).
           03  ROTENT-FLAG-6416        PIC  X(001).
           03  ROTENT-CFUNC-BDSCO-6416 PIC  9(009).
           03  ROTENT-CTERM-6416       PIC  X(008).
           03  ROTENT-BANCO-6416       PIC  9(003).
           03  ROTENT-DEPDC-6416       PIC  9(005).

       01  SAIDA-ROTEADOR-6416.
           03  ROTSAI-HEADER-6416.
               05  ROTSAI-COD-RETORNO-6416  PIC  X(004).
               05  ROTSAI-MSG-RETORNO-6416  PIC  X(079).
               05  ROTSAI-RESTART-6416      PIC  9(005).
               05  ROTSAI-FLAG-6416         PIC  X(001).
           03  ROTSAI-DADOS-6416.
               05  ROTSAI-TABELA-SAIDA-6416.
                   07  ROTSAI-AG-DEPDC-6416       PIC 9(05).
                   07  ROTSAI-DG-DEPDC-6416       PIC X(01).
                   07  ROTSAI-NOME-DEPDC-6416     PIC X(40).
                   07  ROTSAI-END-DEPDC-6416      PIC X(30).
                   07  ROTSAI-DIRETORIA-REG-6416  PIC 9(05).
                   07  ROTSAI-DIRETORIA-NOME-6416 PIC X(40).
                   07  ROTSAI-NOME-BANCO-6416     PIC X(40).
                   07  ROTSAI-POLO-SERVICO-6416   PIC 9(05).
                   07  ROTSAI-MUNIC-DEPDC-6416    PIC X(25).
                   07  ROTSAI-UF-6416             PIC X(02).
                   07  ROTSAI-CEP-6416            PIC 9(05).
                   07  ROTSAI-CEP-COMPL-6416      PIC 9(03).
                   07  ROTSAI-INDIC-DEPDC-AT-6416 PIC X(01).
                   07  ROTSAI-NATUREZA-DEPDC-6416 PIC X(01).
                   07  ROTSAI-COD-DISTR-CORR-6416 PIC 9(05).
                   07  ROTSAI-DENCRR-DEPDC-BDSCO-6416 PIC X(10).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DA TELA DCOMEMTM - INPUT ***'.
      *----------------------------------------------------------------*
       01  WRK-INPUT-EMTM.
           05  FILLER                          PIC  X(18)  VALUE SPACES.
           05  WRK-INP-PFK-EMTM                PIC  X(01)  VALUE SPACES.
           05  WRK-INP-SENHAS-EMTM             PIC  X(37)  VALUE SPACES.
           05  WRK-INP-COMANDO-EMTM            PIC  X(68)  VALUE SPACES.
           05  WRK-INP-DADOS-EMTM.
               10  WRK-INP-TIPOOPC-EMTM        PIC  X(01)  VALUE SPACES.
               10  WRK-INP-TPPESQ-EMTM         PIC  X(03)  VALUE SPACES.
               10  WRK-INP-OCORR-EMTM          PIC  9(03)  VALUE ZEROS.
               10  WRK-INP-TMSTAMP-EMTM        PIC  X(26)  VALUE SPACES.
               10  WRK-INP-CHAMADO-EMTM        PIC  X(08)  VALUE SPACES.
               10  WRK-INP-PAGINA-EMTM-X.
                   15  WRK-INP-PAGINA-EMTM     PIC  9(03)  VALUE ZEROS.
               10  WRK-INP-FASE-EMTM           PIC  X(01)  VALUE SPACES.
               10  WRK-INP-NOME-EMTM           PIC  X(40)  VALUE SPACES.
               10  WRK-INP-CNPJCPF-EMTM.
FS2511*            15  WRK-INP-NUMERO1-EMTM    PIC  9(03)  VALUE ZEROS.
FS2511             15  WRK-INP-NUMERO1-EMTM    PIC  X(03)  VALUE SPACES.
                   15  WRK-INP-PONTO-1-EMTM    PIC  X(01)  VALUE SPACES.
FS2511*            15  WRK-INP-NUMERO2-EMTM    PIC  9(03)  VALUE ZEROS.
FS2511             15  WRK-INP-NUMERO2-EMTM    PIC  X(03)  VALUE SPACES.
                   15  WRK-INP-PONTO-2-EMTM    PIC  X(01)  VALUE SPACES.
FS2511*            15  WRK-INP-NUMERO3-EMTM    PIC  9(03)  VALUE ZEROS.
FS2511             15  WRK-INP-NUMERO3-EMTM    PIC  X(03)  VALUE SPACES.
                   15  WRK-INP-BARRA-1-EMTM    PIC  X(01)  VALUE SPACES.
FS2511*            15  WRK-INP-FILIAL-EMTM     PIC  9(04)  VALUE ZEROS.
FS2511             15  WRK-INP-FILIAL-EMTM     PIC  X(04)  VALUE SPACES.
                   15  WRK-INP-BARRA-2-EMTM    PIC  X(01)  VALUE SPACES.
                   15  WRK-INP-CONTROLE-EMTM   PIC  9(02)  VALUE ZEROS.
               10  WRK-INP-BANCO-EMTM.
                   15  WRK-INP-BANCO-EMTM-N    PIC  9(03)  VALUE ZEROS.
               10  WRK-INP-AGENCIA-GR-EMTM.
                   15  WRK-INP-AGENCIA-EMTM    PIC  9(05)  VALUE ZEROS.
                   15  WRK-INP-HIFEN-AG-EMTM   PIC  X(01)  VALUE ZEROS.
                   15  WRK-INP-NMAGENC-EMTM    PIC  X(23)  VALUE SPACES.
               10  WRK-INP-CONTA-EMTM.
                   15  WRK-INP-CONTA-EMTM-N    PIC  9(13)  VALUE ZEROS.
               10  WRK-INP-PRODUTO-EMTM.
                   15  WRK-INP-CODPROD-EMTM-N  PIC  9(03)  VALUE ZEROS.
               10  WRK-INP-DESPROD-EMTM        PIC  X(10)  VALUE SPACES.
               10  WRK-INP-CONTR-EMTM          PIC  X(09)  VALUE SPACES.
               10  WRK-INP-TAB-SUBP-EMTM       OCCURS 3 TIMES.
                   15  WRK-INP-LIN-SUBP-EMTM   PIC X(050).
               10  WRK-INP-SITUAC-EMTM         PIC  X(15)  VALUE SPACES.
               10  WRK-INP-DTCONTR-EMTM        PIC  X(10)  VALUE SPACES.
               10  WRK-INP-NUMGAR-EMTM.
                   15  WRK-INP-NUMGAR-EMTM-N   PIC  9(03)  VALUE ZEROS.
               10  WRK-INP-DESGAR-EMTM         PIC  X(20)  VALUE SPACES.
               10  WRK-INP-DTVENC-EMTM         PIC  X(10)  VALUE SPACES.
               10  WRK-INP-PRZDIAS-EMTM        PIC  X(03)  VALUE SPACES.
               10  WRK-INP-VLLIM-EMTM          PIC  X(18)  VALUE SPACES.
               10  WRK-INP-RENAUTO-EMTM        PIC  X(01)  VALUE SPACES.
               10  WRK-INP-TPSUB-EMTM          PIC  X(01)  VALUE SPACES.
               10  WRK-INP-TPCTLIM-EMTM        PIC  X(01)  VALUE SPACES.
               10  WRK-INP-TPISENT-EMTM        PIC  X(01)  VALUE SPACES.
               10  WRK-INP-TPVLFIX-EMTM        PIC  X(01)  VALUE SPACES.
               10  WRK-INP-TPPCENT-EMTM        PIC  X(01)  VALUE SPACES.
               10  WRK-INP-VLTAC-EMTM          PIC  X(18)  VALUE SPACES.
           05  WRK-INP-MENSA-EMTM              PIC  X(79)  VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                            PIC  X(50)    VALUE
           '*** AREA DA TELA DCOMEMTM  - OUTPUT ***'.
      *----------------------------------------------------------------*
       01  WRK-OUTPUT-EMTM.
           05  WRK-OUT-LL-EMTM         PIC S9(04) COMP     VALUE +0000.
           05  WRK-OUT-ZZ-EMTM         PIC S9(04) COMP     VALUE ZEROS.
           05  WRK-OUT-SENHAS-EMTM             PIC  X(37)  VALUE SPACES.
           05  WRK-OUT-COMANDO-EMTM            PIC  X(68)  VALUE SPACES.
           05  WRK-OUT-DADOS-EMTM.
               10  WRK-OUT-TIPOOPC-EMTM        PIC  X(01)  VALUE SPACES.
               10  WRK-OUT-TPPESQ-EMTM         PIC  X(03)  VALUE SPACES.
               10  WRK-OUT-OCORR-EMTM          PIC  9(03)  VALUE ZEROS.
               10  WRK-OUT-TMSTAMP-EMTM        PIC  X(26)  VALUE SPACES.
               10  WRK-OUT-CHAMADO-EMTM        PIC  X(08)  VALUE SPACES.
               10  WRK-OUT-PAGINA-EMTM-X.
                   15  WRK-OUT-PAGINA-EMTM     PIC  9(03)  VALUE ZEROS.
               10  WRK-OUT-FASE-EMTM           PIC  X(01)  VALUE SPACES.
               10  WRK-OUT-NOME-EMTM           PIC  X(40)  VALUE SPACES.
               10  WRK-OUT-CNPJCPF-EMTM.
FS2511             15  WRK-OUT-NUMERO1-EMTM    PIC  X(03)  VALUE SPACES.
                   15  WRK-OUT-PONTO-1-EMTM    PIC  X(01)  VALUE SPACES.
FS2511             15  WRK-OUT-NUMERO2-EMTM    PIC  X(03)  VALUE SPACES.
                   15  WRK-OUT-PONTO-2-EMTM    PIC  X(01)  VALUE SPACES.
FS2511             15  WRK-OUT-NUMERO3-EMTM    PIC  X(03)  VALUE SPACES.
                   15  WRK-OUT-BARRA-1-EMTM    PIC  X(01)  VALUE SPACES.
FS2511*            15  WRK-OUT-FILIAL-EMTM     PIC  9(04)  VALUE ZEROS.
FS2511             15  WRK-OUT-FILIAL-EMTM     PIC  X(04)  VALUE SPACES.
                   15  WRK-OUT-BARRA-2-EMTM    PIC  X(01)  VALUE SPACES.
                   15  WRK-OUT-CONTROLE-EMTM   PIC  9(02)  VALUE ZEROS.
               10  WRK-OUT-BANCO-EMTM.
                   15  WRK-OUT-BANCO-EMTM-N    PIC  9(03)  VALUE ZEROS.
               10  WRK-OUT-AGENCIA-GR-EMTM.
                   15  WRK-OUT-AGENCIA-EMTM    PIC  9(05)  VALUE ZEROS.
                   15  WRK-OUT-HIFEN-AG-EMTM   PIC  X(01)  VALUE ZEROS.
                   15  WRK-OUT-NMAGENC-EMTM    PIC  X(23)  VALUE SPACES.
               10  WRK-OUT-CONTA-EMTM.
                   15  WRK-OUT-CONTA-EMTM-N    PIC  9(13)  VALUE ZEROS.
               10  WRK-OUT-PRODUTO-EMTM.
                   15  WRK-OUT-CODPROD-EMTM-N  PIC  9(03)  VALUE ZEROS.
               10  WRK-OUT-DESPROD-EMTM        PIC  X(10)  VALUE SPACES.
               10  WRK-OUT-CONTR-EMTM          PIC  X(09)  VALUE SPACES.
               10  WRK-OUT-TAB-SUBP-EMTM       OCCURS 3 TIMES.
                   15  WRK-OUT-LIN-SUBP-EMTM   PIC X(050).
               10  WRK-OUT-SITUAC-EMTM         PIC  X(15)  VALUE SPACES.
               10  WRK-OUT-DTCONTR-EMTM        PIC  X(10)  VALUE SPACES.
               10  WRK-OUT-NUMGAR-EMTM.
                   15  WRK-OUT-NUMGAR-EMTM-N   PIC  9(03)  VALUE ZEROS.
               10  WRK-OUT-DESGAR-EMTM         PIC  X(20)  VALUE SPACES.
               10  WRK-OUT-DTVENC-EMTM         PIC  X(10)  VALUE SPACES.
               10  WRK-OUT-PRZDIAS-EMTM.
                   15  WRK-OUT-PRZDIAS-EMTM-N  PIC  9(03)  VALUE ZEROS.
               10  WRK-OUT-VLLIM-EMTM.
                   15  WRK-OUT-VLLIM-EMTM-N    PIC  ZZZ.ZZZ.ZZZ.ZZ9,99.
               10  WRK-OUT-RENAUTO-EMTM        PIC  X(01)  VALUE SPACES.
               10  WRK-OUT-TIPO-TAC-EMTM.
                   15  WRK-OUT-TPSUB-EMTM      PIC  X(01)  VALUE SPACES.
                   15  WRK-OUT-TPCTLIM-EMTM    PIC  X(01)  VALUE SPACES.
                   15  WRK-OUT-TPISENT-EMTM    PIC  X(01)  VALUE SPACES.
                   15  WRK-OUT-TPVLFIX-EMTM    PIC  X(01)  VALUE SPACES.
                   15  WRK-OUT-TPPCENT-EMTM    PIC  X(01)  VALUE SPACES.
               10  WRK-OUT-VLTAC-EMTM.
                   15  WRK-OUT-VLTAC-EMTM-N    PIC  ZZZ.ZZZ.ZZZ.ZZ9,99.
           05  WRK-OUT-MENSA-EMTM              PIC  X(79)  VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                            PIC  X(50)   VALUE
           '*** AREA DA TELA DCOMEMTM - BRAD0660 ***'.
      *----------------------------------------------------------------*
       01  WRK-660-DCOMEMTM.
           05  WRK-660-LL-AREA-EMTM      PIC  9(004) COMP    VALUE 0000.
           05  WRK-660-LL-MENSA-EMTM     PIC  9(004) COMP    VALUE 0000.
           05  WRK-660-SENHAS-EMTM       PIC  9(004) COMP    VALUE 0037.
           05  WRK-660-COMANDO-EMTM      PIC  9(004) COMP    VALUE 0068.
           05  WRK-660-TIPOOPC-EMTM      PIC  9(004) COMP    VALUE 0001.
           05  WRK-660-TPPESQ-EMTM       PIC  9(004) COMP    VALUE 0003.
           05  WRK-660-OCORR-EMTM        PIC  9(004) COMP    VALUE 0003.
           05  WRK-660-TMSTAMP-EMTM      PIC  9(004) COMP    VALUE 0026.
           05  WRK-660-CHAMADO-EMTM      PIC  9(004) COMP    VALUE 0008.
           05  WRK-660-PAGINA-EMTM       PIC  9(004) COMP    VALUE 0003.
           05  WRK-660-FASE-EMTM         PIC  9(004) COMP    VALUE 0001.
           05  WRK-660-NOME-EMTM         PIC  9(004) COMP    VALUE 0040.
           05  WRK-660-CNPJCPF-EMTM      PIC  9(004) COMP    VALUE 0019.
           05  WRK-660-BANCO-EMTM        PIC  9(004) COMP    VALUE 0003.
           05  WRK-660-AGENCIA-EMTM      PIC  9(004) COMP    VALUE 0029.
           05  WRK-660-CONTA-EMTM        PIC  9(004) COMP    VALUE 0013.
           05  WRK-660-CODPROD-EMTM      PIC  9(004) COMP    VALUE 0003.
           05  WRK-660-DESPROD-EMTM      PIC  9(004) COMP    VALUE 0010.
           05  WRK-660-CONTR-EMTM        PIC  9(004) COMP    VALUE 0009.
           05  WRK-660-TAB-SUBP-EMTM     OCCURS 3 TIMES.
               10  WRK-660-LIN-SUBP-EMTM PIC  9(004) COMP    VALUE 0050.
           05  WRK-660-SITUAC-EMTM       PIC  9(004) COMP    VALUE 0015.
           05  WRK-660-DTCONTR-EMTM      PIC  9(004) COMP    VALUE 0010.
           05  WRK-660-NUMGAR-EMTM       PIC  9(004) COMP    VALUE 0003.
           05  WRK-660-DESGAR-EMTM       PIC  9(004) COMP    VALUE 0020.
           05  WRK-660-DTVENC-EMTM       PIC  9(004) COMP    VALUE 0010.
           05  WRK-660-PRZDIAS-EMTM      PIC  9(004) COMP    VALUE 0003.
           05  WRK-660-VLLIM-EMTM        PIC  9(004) COMP    VALUE 0018.
           05  WRK-660-RENAUTO-EMTM      PIC  9(004) COMP    VALUE 0001.
           05  WRK-660-TPSUB-EMTM        PIC  9(004) COMP    VALUE 0001.
           05  WRK-660-TPCTLIM-EMTM      PIC  9(004) COMP    VALUE 0001.
           05  WRK-660-TPISENT-EMTM      PIC  9(004) COMP    VALUE 0001.
           05  WRK-660-TPVLFIX-EMTM      PIC  9(004) COMP    VALUE 0001.
           05  WRK-660-TPPCENT-EMTM      PIC  9(004) COMP    VALUE 0001.
           05  WRK-660-VLTAC-EMTM        PIC  9(004) COMP    VALUE 0018.
           05  WRK-660-MENSA-EMTM        PIC  9(004) COMP    VALUE 0079.

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
           '*** DCOM0513 - FIM DA AREA DE WORKING ***'.
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

           MOVE SPACES                 TO WRK-OUT-DADOS-EMTM
                                          WRK-OUT-SENHAS-EMTM
                                          WRK-OUT-COMANDO-EMTM.

           INITIALIZE                     ENTRADA-ROTEADOR
                                          SAIDA-ROTEADOR
                                          ENTRADA-ROTEADOR-4113
                                          SAIDA-ROTEADOR-4113
                                          ENTRADA-ROTEADOR-6416
                                          SAIDA-ROTEADOR-6416
                                          WRK-AUXILIARES.

           PERFORM 2000-PROCESSAR-MENSAGEM.

           GO TO 0000-INICIAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000-PROCESSAR-MENSAGEM         SECTION.
      *----------------------------------------------------------------*

           MOVE 'DCOMEMTM'             TO WRK-TELA

           IF  WRK-TRANSACAO           NOT EQUAL 'DCOM0513'
               PERFORM 2100-RECEBER-CONTROLE
           ELSE
               PERFORM 2200-PROCESSAR-DCOMEMTM
           END-IF.

           IF  WRK-TELA                EQUAL 'DCOMEMTM'
               PERFORM 2300-PROCESSAR-BRAD0660
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2100-RECEBER-CONTROLE           SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO WRK-INCONSIS

           IF  WRK-TRANSACAO           EQUAL   'DCOM0508'
               MOVE  WRK-MENSAGEM      TO WRK-COMU-AREA
               MOVE  WRK-COMU-TPOPCAO  TO WRK-OUT-TIPOOPC-EMTM
                                          WRK-INP-TIPOOPC-EMTM
               MOVE  WRK-COMU-TMSTAMP  TO WRK-OUT-TMSTAMP-EMTM
                                          WRK-INP-TMSTAMP-EMTM
               MOVE  ZEROS             TO WRK-OUT-AGENCIA-EMTM
                                          WRK-OUT-PAGINA-EMTM
                                          WRK-INP-PAGINA-EMTM
               MOVE  WRK-COMU-CONTRATO-LIM
                                       TO WRK-INP-CONTR-EMTM
                                          WRK-OUT-CONTR-EMTM
               MOVE 'CTR'              TO WRK-INP-TPPESQ-EMTM
                                          WRK-OUT-TPPESQ-EMTM

           ELSE
               MOVE  WRK-MENSAGEM      TO WRK-COMU-AREA
               MOVE  WRK-COMU-TPOPCAO  TO WRK-OUT-TIPOOPC-EMTM
                                          WRK-INP-TIPOOPC-EMTM
               MOVE  WRK-COMU-TMSTAMP
                                       TO WRK-OUT-TMSTAMP-EMTM
                                          WRK-INP-TMSTAMP-EMTM
               MOVE  WRK-COMU-CONTRATO-LIM
                                       TO WRK-INP-CONTR-EMTM
                                          WRK-OUT-CONTR-EMTM
               MOVE  WRK-COMU-PAG      TO WRK-INP-PAGINA-EMTM
                                          WRK-OUT-PAGINA-EMTM
               MOVE  WRK-COMU-OCORR    TO WRK-INP-OCORR-EMTM
                                          WRK-OUT-OCORR-EMTM
               IF WRK-COMU-AGENCIA     NOT EQUAL  ZEROS
                  MOVE 'AGE'           TO WRK-INP-TPPESQ-EMTM
                                          WRK-OUT-TPPESQ-EMTM
               ELSE
                  MOVE 'CPF'           TO WRK-INP-TPPESQ-EMTM
                                          WRK-OUT-TPPESQ-EMTM
               END-IF
           END-IF.


           MOVE  WRK-TRANSACAO         TO WRK-INP-CHAMADO-EMTM
                                          WRK-OUT-CHAMADO-EMTM.

           MOVE  SPACES                TO ROTSAI-FLAG

           PERFORM 2110-ACESSAR-DCOM4128.

           IF  WRK-ERRO-MOD          NOT EQUAL SPACES
               MOVE SPACES             TO WRK-ERRO-MOD
               GO TO 2100-99-FIM
           END-IF.

           PERFORM 2199-VALIDAR-PERFIL.

           IF  WRK-ERRO-MOD          NOT EQUAL SPACES
               MOVE SPACES             TO WRK-ERRO-MOD
               GO TO 2100-99-FIM
           END-IF.

           PERFORM 2120-ACESSAR-DCOM6416.

           MOVE 'N'                    TO WRK-ERRO-ACESSO.

           PERFORM  4170-ACESSAR-DCOM4170
           IF 4170-SAI-CCLUB   GREATER ZEROS
              PERFORM  2130-ACESSAR-DCOM6408
              IF  WRK-INCONSIS      EQUAL  '*'
                  GO TO 2100-99-FIM
              END-IF
           END-IF.

           PERFORM 2140-ACESSAR-DCOM4113.

           IF WRK-INCONSIS             NOT EQUAL '*' AND
              WRK-ERRO-ACESSO          EQUAL 'N'
              MOVE WRK-MSG003          TO WRK-OUT-MENSA-EMTM
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2110-ACESSAR-DCOM4128           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-COD-USER           TO ROTENT-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO ROTENT-CTERM
           MOVE 'T'                    TO ROTENT-FLAG-FILTRO
           MOVE SPACES                 TO ROTENT-FLAG
           MOVE ZEROS                  TO ROTENT-RESTART
           MOVE WRK-COMU-CONTRATO-LIM  TO ROTENT-CCONTR-LIM-DESC
           MOVE 'DCOM4128'             TO WRK-MODULO

           CALL WRK-MODULO             USING ENTRADA-ROTEADOR
                                             SAIDA-ROTEADOR
                                             ERRO-AREA
                                             WRK-SQLCA.

           EVALUATE ROTSAI-COD-RETORNO
              WHEN '0000'
                   PERFORM 2111-MONTAR-TELA
                   MOVE  WRK-MSG003
                                       TO  WRK-OUT-MENSA-EMTM

              WHEN '0099'
                   MOVE 'DB2'          TO ERR-TIPO-ACESSO
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO

              WHEN '0003'
                   MOVE ROTSAI-MSG-RETORNO
                                       TO ROTSAI-MSG-RETORNO
                   MOVE 'S'            TO WRK-ERRO-MOD
                   PERFORM 2220-RETORNAR-CHAMADOR

              WHEN OTHER
                   MOVE 'S'            TO WRK-ERRO-MOD
                   PERFORM 2220-RETORNAR-CHAMADOR
           END-EVALUATE.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2199-VALIDAR-PERFIL             SECTION.
      *----------------------------------------------------------------*

           INITIALIZE ROTENT-6437-ENVIO
                      ROTSAI-6437-RETORNO.

           MOVE WRK-COD-USER           TO ROTENT-6437-CFUNC-ENV.
           MOVE LNK-IO-LTERM           TO ROTENT-6437-CTERM.
           MOVE WRK-COD-DEPTO-N        TO ROTENT-6437-AGEUSU-ENV.
           MOVE ROTSAI-CAG-BCRIA       TO ROTENT-6437-AGEPESQ-ENV.
           MOVE 'A'                    TO ROTENT-6437-FUNCAO-ENV.
           MOVE 'DCOM6437'             TO WRK-MODULO.

           CALL WRK-MODULO             USING ROTENT-6437-ENVIO
                                             ROTSAI-6437-RETORNO
                                             ERRO-AREA
                                             WRK-SQLCA.

           IF  ROTSAI-6437-COD-RET     NOT EQUAL ZEROS OR
               ROTSAI-6437-RESUL-RET   GREATER 3
               MOVE SPACES         TO WRK-ERRO-MOD
                                      WRK-PFK06
               PERFORM 2220-RETORNAR-CHAMADOR
               MOVE 'USUARIO NAO AUTORIZADO'
                                   TO WRK-COMU-MENSA
               MOVE 'S'            TO WRK-ERRO-MOD
               MOVE WRK-COMU-AREA      TO WRK-MENSAGEM
           END-IF.

      *----------------------------------------------------------------*
       2199-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2111-MONTAR-TELA                SECTION.
      *----------------------------------------------------------------*

           MOVE  ROTSAI-CCNPJ-CPF(1:3)     TO WRK-OUT-NUMERO1-EMTM
           MOVE  ROTSAI-CCNPJ-CPF(4:3)     TO WRK-OUT-NUMERO2-EMTM
           MOVE  ROTSAI-CCNPJ-CPF(7:3)     TO WRK-OUT-NUMERO3-EMTM
           MOVE  ROTSAI-CFLIAL-CNPJ        TO WRK-OUT-FILIAL-EMTM
           MOVE  ROTSAI-CCTRL-CNPJ-CPF     TO WRK-OUT-CONTROLE-EMTM
           MOVE  '.'                       TO WRK-OUT-PONTO-1-EMTM
                                              WRK-OUT-PONTO-2-EMTM
           MOVE  '/'                       TO WRK-OUT-BARRA-1-EMTM
           MOVE  '-'                       TO WRK-OUT-BARRA-2-EMTM
           MOVE  ROTSAI-IRSUMO-SIT-DESC    TO WRK-OUT-SITUAC-EMTM
           MOVE  ROTSAI-CBCO               TO WRK-OUT-BANCO-EMTM-N
           MOVE  ROTSAI-CAG-BCRIA          TO WRK-OUT-AGENCIA-EMTM
           MOVE  '-'                       TO WRK-OUT-HIFEN-AG-EMTM
           MOVE  ROTSAI-CCTA-BCRIA-CLI     TO WRK-OUT-CONTA-EMTM-N
           MOVE  ROTSAI-DINIC-VGCIA-CONTR  TO WRK-OUT-DTCONTR-EMTM
           INSPECT WRK-OUT-DTCONTR-EMTM REPLACING ALL '.' BY '/'
           MOVE  ROTSAI-CINDCD-RENOV-AUTOM TO WRK-OUT-RENAUTO-EMTM
           MOVE  ROTSAI-DVCTO-CONTR-LIM    TO WRK-OUT-DTVENC-EMTM
           INSPECT WRK-OUT-DTVENC-EMTM REPLACING ALL '.' BY '/'
           MOVE  ROTSAI-VCONTR-LIM-DESC    TO WRK-OUT-VLLIM-EMTM-N
           MOVE  ROTSAI-VTAC-CONTR-LIM     TO WRK-OUT-VLTAC-EMTM-N
           MOVE  ROTSAI-TCONTR-LIM-DESC    TO WRK-OUT-PRZDIAS-EMTM-N
           MOVE  SPACES                    TO WRK-OUT-TIPO-TAC-EMTM

           IF ROTSAI-CTPO-COBR-TAC  EQUAL  999 OR ZEROS OR 004
              MOVE 'X'                     TO WRK-OUT-TPSUB-EMTM
           ELSE
              IF ROTSAI-CTPO-COBR-TAC  EQUAL  01
                 MOVE 'X'                  TO WRK-OUT-TPCTLIM-EMTM
                                              WRK-OUT-TPISENT-EMTM
              ELSE
                 IF ROTSAI-CTPO-COBR-TAC  EQUAL  02
                    MOVE 'X'               TO WRK-OUT-TPCTLIM-EMTM
                                              WRK-OUT-TPVLFIX-EMTM
                 ELSE
                    IF ROTSAI-CTPO-COBR-TAC  EQUAL  03
                       MOVE 'X'            TO WRK-OUT-TPCTLIM-EMTM
                                              WRK-OUT-TPPCENT-EMTM
                    END-IF
                 END-IF
              END-IF
           END-IF.
           IF    ROTSAI-CGARAN         EQUAL ZEROS
                 MOVE SPACES           TO  WRK-OUT-NUMGAR-EMTM
                                           WRK-OUT-DESGAR-EMTM
           ELSE
                 MOVE ROTSAI-CGARAN    TO  WRK-OUT-NUMGAR-EMTM-N
                 MOVE ROTSAI-IGARAN    TO  WRK-OUT-DESGAR-EMTM
           END-IF.

      *----------------------------------------------------------------*
       2111-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2120-ACESSAR-DCOM6416           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-COD-USER           TO ROTENT-CFUNC-BDSCO-6416
           MOVE LNK-IO-LTERM           TO ROTENT-CTERM-6416
           MOVE ZEROS                  TO ROTENT-RESTART-6416
           MOVE SPACES                 TO ROTENT-FLAG-6416
           MOVE ROTSAI-CBCO            TO ROTENT-BANCO-6416
           MOVE ROTSAI-CAG-BCRIA       TO ROTENT-DEPDC-6416
           MOVE 'DCOM6416'             TO WRK-MODULO

           CALL WRK-MODULO             USING ENTRADA-ROTEADOR-6416
                                             SAIDA-ROTEADOR-6416
                                             ERRO-AREA
                                             WRK-SQLCA.

           EVALUATE ROTSAI-COD-RETORNO-6416
              WHEN '0000'
                   MOVE ROTSAI-NOME-DEPDC-6416
                                       TO WRK-OUT-NMAGENC-EMTM

              WHEN '0099'
                   MOVE 'DB2'          TO ERR-TIPO-ACESSO
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO

              WHEN OTHER
                   MOVE ROTSAI-MSG-RETORNO-6416
                                       TO WRK-OUT-MENSA-EMTM
           END-EVALUATE.

      *----------------------------------------------------------------*
       2120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4170-ACESSAR-DCOM4170           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                      4170-ENTRADA
                                           4170-SAIDA.

           MOVE   ROTENT-CCONTR-LIM-DESC TO  WRK-CONTRATO-4170
           MOVE   WRK-CHAVE-4170         TO  4170-ENT-CCHAVE-ELMTO-DESC
           MOVE  'CP'                    TO  4170-ENT-CINDCD-PSSOA-DESC
           MOVE  'N'                     TO  WRK-ERRO-ACESSO
           MOVE  'DCOM4170'            TO  WRK-MODULO

           CALL  WRK-MODULO         USING  4170-ENTRADA
                                           4170-SAIDA
                                           ERRO-AREA
                                           WRK-SQLCA.

           EVALUATE 4170-SAI-COD-RET
               WHEN  '0000'

                   MOVE 'N'            TO WRK-ERRO-ACESSO
                   MOVE 4170-SAI-IPSSOA-DESC-COML TO WRK-OUT-NOME-EMTM
               WHEN '0099'

                   MOVE 'DB2'          TO ERR-TIPO-ACESSO
                   MOVE '0010'         TO ERR-LOCAL
                   MOVE '2132-ACESSAR-DCOM4170'
                                       TO ERR-TEXTO
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO
               WHEN OTHER

                  MOVE 'S'                       TO WRK-ERRO-ACESSO
                  MOVE 4170-SAI-MSG-RET          TO  WRK-RESTO-4170
                  MOVE WRK-MENSAG-4170           TO  WRK-OUT-MENSA-EMTM

           END-EVALUATE.

      *----------------------------------------------------------------*
       2130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2130-ACESSAR-DCOM6408           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-COD-USER           TO ROTENT-6408-CFUNC-ENV
           MOVE LNK-IO-LTERM           TO ROTENT-6408-CTERM-ENV
           MOVE ZEROS                  TO ROTENT-6408-REST-ENV
                                          ROTENT-6408-CJUNC-ENV
                                          ROTENT-6408-CTAC-ENV
                                          ROTENT-6408-CCLUB-ENV
           MOVE 'N'                    TO ROTENT-6408-FLAG-ENV
                                          ROTENT-6408-START-ENV
           MOVE ROTSAI-CCNPJ-CPF       TO ROTENT-6408-NUM-CNPJ-ENV
           MOVE ROTSAI-CFLIAL-CNPJ     TO ROTENT-6408-FIL-CNPJ-ENV
           MOVE ROTSAI-CCTRL-CNPJ-CPF  TO ROTENT-6408-CTR-CNPJ-ENV

           MOVE 'DCOM6408'             TO WRK-MODULO
           CALL WRK-MODULO             USING ROTENT-6408-AREA
                                             ROTSAI-6408-RETORNO
                                             ERRO-AREA
                                             WRK-SQLCA.

           EVALUATE ROTSAI-6408-COD-RET
              WHEN '0000'
                   MOVE ROTSAI-6408-NM-RZSOC-RET(1)
                                       TO WRK-OUT-NOME-EMTM

              WHEN '0099'
                   MOVE 'DB2'          TO ERR-TIPO-ACESSO
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO

              WHEN OTHER
                   MOVE ROTSAI-6408-MENS-RET
                                       TO WRK-OUT-MENSA-EMTM
           END-EVALUATE.

      *----------------------------------------------------------------*
       2130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2140-ACESSAR-DCOM4113           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-COD-USER           TO ROTENT-CFUNC-BDSCO-4113
           MOVE LNK-IO-LTERM           TO ROTENT-CTERM-4113
           MOVE ZEROS                  TO ROTENT-FLAG-4113
                                          ROTENT-RESTART-4113
           MOVE SPACES                 TO ROTENT-START-COUNT-4113
           MOVE ROTSAI-CPRODT          TO ROTENT-CPRODT-4113
           MOVE ROTENT-CCONTR-LIM-DESC TO ROTENT-CCONTR-LIM-DESC-4113
           MOVE 'DCOM4113'             TO WRK-MODULO

           CALL WRK-MODULO             USING ENTRADA-ROTEADOR-4113
                                             SAIDA-ROTEADOR-4113
                                             ERRO-AREA
                                             WRK-SQLCA.

           EVALUATE ROTSAI-COD-RETORNO-4113
              WHEN '0000'
                   MOVE ROTSAI-CPRODT-4113  TO WRK-OUT-CODPROD-EMTM-N
                   MOVE ROTSAI-IABREV-PRODT-4113
                                            TO WRK-OUT-DESPROD-EMTM
                   MOVE 1                   TO WIND
                                               WIND2
                   PERFORM 2150-CARREGA-SUBPROD
                           UNTIL   WIND2    GREATER 3 OR
                                   WIND     GREATER 12

              WHEN '0099'
                   MOVE 'DB2'          TO ERR-TIPO-ACESSO
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO

              WHEN OTHER
                   MOVE '*'            TO WRK-INCONSIS
                   MOVE ROTSAI-MSG-RETORNO-4113
                                       TO WRK-OUT-MENSA-EMTM
           END-EVALUATE.

      *----------------------------------------------------------------*
       2140-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2150-CARREGA-SUBPROD            SECTION.
      *----------------------------------------------------------------*

           INITIALIZE  WRK-LIN-SUBPROD.

           IF (ROTSAI-CSPROD-DESC-COML-4113(WIND)    IS NUMERIC)  AND
              (ROTSAI-CSPROD-DESC-COML-4113(WIND) GREATER ZEROS)
               MOVE ROTSAI-CSPROD-DESC-COML-4113(WIND)
                                       TO WRK-LIN-SUBP-NRO1
               MOVE ROTSAI-IRSUMO-SPROD-4113  (WIND)
                                       TO WRK-LIN-SUBP-DESC1
               MOVE '-'                TO WRK-LIN-HIFEM1
           ELSE
               MOVE SPACES             TO WRK-LIN-SUBP-NRO1
                                          WRK-LIN-HIFEM1
           END-IF.

           ADD 1           TO WIND

           IF (ROTSAI-CSPROD-DESC-COML-4113(WIND)    IS NUMERIC)  AND
              (ROTSAI-CSPROD-DESC-COML-4113(WIND) GREATER ZEROS)
               MOVE ROTSAI-CSPROD-DESC-COML-4113(WIND)
                                       TO WRK-LIN-SUBP-NRO2
               MOVE ROTSAI-IRSUMO-SPROD-4113  (WIND)
                                       TO WRK-LIN-SUBP-DESC2
               MOVE '-'                TO WRK-LIN-HIFEM2
               MOVE ' / '              TO WRK-LIN-BARRA1
           ELSE
               MOVE SPACES             TO WRK-LIN-SUBP-NRO2
                                          WRK-LIN-HIFEM2
           END-IF.

           IF WIND NOT LESS 12
              MOVE  13 TO WIND
              GO TO 2150-ACABOU
           END-IF.

           ADD 1           TO WIND


           IF WIND NOT LESS 12
              MOVE  13 TO WIND
              GO TO 2150-ACABOU
           END-IF.

           IF (ROTSAI-CSPROD-DESC-COML-4113(WIND)    IS NUMERIC)  AND
              (ROTSAI-CSPROD-DESC-COML-4113(WIND) GREATER ZEROS)
               MOVE ROTSAI-CSPROD-DESC-COML-4113(WIND)
                                       TO WRK-LIN-SUBP-NRO3
               MOVE ROTSAI-IRSUMO-SPROD-4113  (WIND)
                                       TO WRK-LIN-SUBP-DESC3
               MOVE '-'                TO WRK-LIN-HIFEM3
               MOVE ' / '              TO WRK-LIN-BARRA2
           ELSE
               MOVE SPACES             TO WRK-LIN-SUBP-NRO3
                                          WRK-LIN-HIFEM3
           END-IF.

           ADD 1           TO WIND

           IF (ROTSAI-CSPROD-DESC-COML-4113(WIND)    IS NUMERIC)  AND
              (ROTSAI-CSPROD-DESC-COML-4113(WIND) GREATER ZEROS)
               MOVE ROTSAI-CSPROD-DESC-COML-4113(WIND)
                                       TO WRK-LIN-SUBP-NRO4
               MOVE ROTSAI-IRSUMO-SPROD-4113  (WIND)
                                       TO WRK-LIN-SUBP-DESC4
               MOVE '-'                TO WRK-LIN-HIFEM4
               MOVE ' / '              TO WRK-LIN-BARRA3
           ELSE
               MOVE SPACES             TO WRK-LIN-SUBP-NRO4
                                          WRK-LIN-HIFEM4
           END-IF.

       2150-ACABOU.

           MOVE WRK-LIN-SUBPROD
                                   TO  WRK-OUT-LIN-SUBP-EMTM(WIND2)

           ADD     1               TO  WIND
                                       WIND2.

      *----------------------------------------------------------------*
       2150-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2200-PROCESSAR-DCOMEMTM         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM           TO WRK-INPUT-EMTM.

           PERFORM 2210-DEVOLVER-TELA

           EVALUATE TRUE
               WHEN WRK-INP-PFK-EMTM     EQUAL 'H' OR
                   (WRK-INP-PFK-EMTM     EQUAL '.' AND
                    WRK-INP-COMANDO-EMTM EQUAL 'PFK01')
                    PERFORM 2210-DEVOLVER-TELA
                    MOVE    WRK-INP-MENSA-EMTM  TO WRK-OUT-MENSA-EMTM

               WHEN WRK-INP-PFK-EMTM     EQUAL '3' OR
                   (WRK-INP-PFK-EMTM     EQUAL '.' AND
                    WRK-INP-COMANDO-EMTM EQUAL 'PFK03')
                    PERFORM 2220-RETORNAR-CHAMADOR

               WHEN WRK-INP-PFK-EMTM     EQUAL 'X' OR
                   (WRK-INP-PFK-EMTM     EQUAL '.' AND
                    WRK-INP-COMANDO-EMTM EQUAL 'PFK05')
                    PERFORM 2230-MENU-ROTINAS

               WHEN WRK-INP-PFK-EMTM     EQUAL 'V' OR
                   (WRK-INP-PFK-EMTM     EQUAL '.' AND
                    WRK-INP-COMANDO-EMTM EQUAL 'PFK10')
                    PERFORM 2270-MENU-DCOM

               WHEN (WRK-INP-PFK-EMTM          EQUAL '.' AND
                     WRK-INP-COMANDO-EMTM      EQUAL SPACES)
                     PERFORM 2280-TRATAR-ENTER

               WHEN OTHER
                     MOVE    SG-MENSAGEM(38)     TO WRK-OUT-MENSA-EMTM

           END-EVALUATE.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2210-DEVOLVER-TELA              SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-SENHAS-EMTM    TO WRK-OUT-SENHAS-EMTM
           MOVE WRK-INP-TIPOOPC-EMTM   TO WRK-OUT-TIPOOPC-EMTM
           MOVE WRK-INP-TPPESQ-EMTM    TO WRK-OUT-TPPESQ-EMTM
           MOVE WRK-INP-OCORR-EMTM     TO WRK-OUT-OCORR-EMTM
           MOVE WRK-INP-TMSTAMP-EMTM   TO WRK-OUT-TMSTAMP-EMTM
           MOVE WRK-INP-CHAMADO-EMTM   TO WRK-OUT-CHAMADO-EMTM
           MOVE WRK-INP-PAGINA-EMTM-X  TO WRK-OUT-PAGINA-EMTM-X
           MOVE WRK-INP-FASE-EMTM      TO WRK-OUT-FASE-EMTM
           MOVE WRK-INP-NOME-EMTM      TO WRK-OUT-NOME-EMTM
           MOVE WRK-INP-CNPJCPF-EMTM   TO WRK-OUT-CNPJCPF-EMTM
           MOVE WRK-INP-SITUAC-EMTM    TO WRK-OUT-SITUAC-EMTM
           MOVE WRK-INP-BANCO-EMTM     TO WRK-OUT-BANCO-EMTM
           MOVE WRK-INP-AGENCIA-GR-EMTM
                                       TO WRK-OUT-AGENCIA-GR-EMTM
           MOVE WRK-INP-CONTA-EMTM     TO WRK-OUT-CONTA-EMTM
           MOVE WRK-INP-PRODUTO-EMTM   TO WRK-OUT-PRODUTO-EMTM
           MOVE WRK-INP-DESPROD-EMTM   TO WRK-OUT-DESPROD-EMTM
           MOVE WRK-INP-CONTR-EMTM     TO WRK-OUT-CONTR-EMTM
           MOVE WRK-INP-SITUAC-EMTM    TO WRK-OUT-SITUAC-EMTM
           MOVE WRK-INP-DTCONTR-EMTM   TO WRK-OUT-DTCONTR-EMTM
           MOVE WRK-INP-NUMGAR-EMTM    TO WRK-OUT-NUMGAR-EMTM
           MOVE WRK-INP-DESGAR-EMTM    TO WRK-OUT-DESGAR-EMTM
           MOVE WRK-INP-DTVENC-EMTM    TO WRK-OUT-DTVENC-EMTM
           MOVE WRK-INP-PRZDIAS-EMTM   TO WRK-OUT-PRZDIAS-EMTM
           MOVE WRK-INP-VLLIM-EMTM     TO WRK-OUT-VLLIM-EMTM
           MOVE WRK-INP-RENAUTO-EMTM   TO WRK-OUT-RENAUTO-EMTM
           MOVE WRK-INP-TPSUB-EMTM     TO WRK-OUT-TPSUB-EMTM
           MOVE WRK-INP-TPCTLIM-EMTM   TO WRK-OUT-TPCTLIM-EMTM
           MOVE WRK-INP-TPISENT-EMTM   TO WRK-OUT-TPISENT-EMTM
           MOVE WRK-INP-TPVLFIX-EMTM   TO WRK-OUT-TPVLFIX-EMTM
           MOVE WRK-INP-TPPCENT-EMTM   TO WRK-OUT-TPPCENT-EMTM
           MOVE WRK-INP-VLTAC-EMTM     TO WRK-OUT-VLTAC-EMTM

           PERFORM  VARYING  WIND  FROM 1 BY 1
                      UNTIL  WIND  GREATER 03

               MOVE WRK-INP-LIN-SUBP-EMTM(WIND)
                                       TO WRK-OUT-LIN-SUBP-EMTM(WIND)

           END-PERFORM.

      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2220-RETORNAR-CHAMADOR          SECTION.
      *----------------------------------------------------------------*

           INITIALIZE      WRK-COMU-AREA
           MOVE LENGTH OF WRK-COMU-AREA  TO WRK-COMU-LL
           MOVE ZEROS                    TO WRK-COMU-ZZ
           MOVE WRK-CHNG                 TO WRK-FUNCAO
           MOVE WRK-INP-CHAMADO-EMTM     TO WRK-TELA
           MOVE 'DCOM0513'               TO WRK-COMU-TRANSACAO
           MOVE '3'                      TO WRK-COMU-PFK1
           MOVE WRK-INP-SENHAS-EMTM      TO WRK-COMU-SENHAS
           MOVE WRK-INP-COMANDO-EMTM     TO WRK-COMU-COMANDO
           MOVE WRK-INP-TIPOOPC-EMTM     TO WRK-COMU-TPOPCAO
           MOVE WRK-INP-TMSTAMP-EMTM     TO WRK-COMU-TMSTAMP
           IF  WRK-ERRO-MOD  EQUAL  'S'
               MOVE ROTSAI-MSG-RETORNO   TO WRK-COMU-MENSA
           END-IF

           IF  WRK-INP-CHAMADO-EMTM   EQUAL   'DCOM0508'
               MOVE WRK-INP-CONTR-EMTM       TO WRK-COMU-CONTRATO-LIM

               MOVE WRK-COMU-AREA                TO WRK-MENSAGEM
           ELSE
               MOVE WRK-INP-PAGINA-EMTM      TO WRK-COMU-PAG
               MOVE WRK-INP-OCORR-EMTM       TO WRK-COMU-OCORR
               IF WRK-INP-TPPESQ-EMTM  EQUAL  'AGE'
                  MOVE WRK-INP-AGENCIA-EMTM  TO WRK-COMU-AGENCIA
                  MOVE WRK-INP-BANCO-EMTM-N  TO WRK-COMU-BANCO
                  MOVE WRK-INP-CONTA-EMTM-N  TO WRK-COMU-CONTA
                  MOVE ZEROS                 TO WRK-COMU-CCPFCNPJ
                                                WRK-COMU-CFLIAL
                                                WRK-COMU-CCTRL
               ELSE
                  MOVE ZEROS                 TO WRK-COMU-AGENCIA
                                                WRK-COMU-BANCO
                                                WRK-COMU-CONTA
                  MOVE WRK-INP-NUMERO1-EMTM  TO WRK-COMU-CCPFCNPJ(1:3)
                  MOVE WRK-INP-NUMERO2-EMTM  TO WRK-COMU-CCPFCNPJ(4:3)
                  MOVE WRK-INP-NUMERO3-EMTM  TO WRK-COMU-CCPFCNPJ(7:3)
                  MOVE WRK-OUT-FILIAL-EMTM   TO WRK-COMU-CFLIAL
                  MOVE WRK-OUT-CONTROLE-EMTM TO WRK-COMU-CCTRL
               END-IF

               MOVE WRK-COMU-AREA            TO WRK-MENSAGEM
           END-IF.

      *----------------------------------------------------------------*
       2220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2230-MENU-ROTINAS               SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-COMU-AREA-PFK

           MOVE LENGTH OF WRK-COMU-AREA-PFK
                                       TO WRK-COMU-LL-PFK
           MOVE WRK-CHNG               TO WRK-FUNCAO
           MOVE 'DCOM0513'             TO WRK-TELA
                                          WRK-COMU-TRANSACAO-PFK
           MOVE '5'                    TO WRK-COMU-PFK
           MOVE WRK-COMU-AREA-PFK          TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2270-MENU-DCOM                  SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-COMU-AREA-PFK

           MOVE LENGTH OF WRK-COMU-AREA-PFK
                                       TO WRK-COMU-LL-PFK
           MOVE WRK-CHNG               TO WRK-FUNCAO
           MOVE 'DCOM0513'             TO WRK-TELA
                                          WRK-COMU-TRANSACAO-PFK
           MOVE 'A'                    TO WRK-COMU-PFK
           MOVE WRK-COMU-AREA-PFK      TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2270-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2280-TRATAR-ENTER               SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MSG003             TO      WRK-OUT-MENSA-EMTM.

      *----------------------------------------------------------------*
       2280-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2300-PROCESSAR-BRAD0660         SECTION.
      *----------------------------------------------------------------*
           MOVE LENGTH OF WRK-OUTPUT-EMTM
                                       TO  WRK-660-LL-MENSA-EMTM
                                           WRK-OUT-LL-EMTM

           MOVE LENGTH OF WRK-660-DCOMEMTM
                                       TO  WRK-660-LL-AREA-EMTM

           CALL 'BRAD0660'             USING WRK-OUTPUT-EMTM
                                             WRK-660-DCOMEMTM.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE RETURN-CODE        TO WRK-RETURN-CODE
               MOVE 10                 TO WRK-LOCAL-ERRO
               MOVE WRK-ERRO-BRAD0660  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-ISRT               TO WRK-FUNCAO.
           MOVE WRK-OUTPUT-EMTM        TO WRK-MENSAGEM.

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
               MOVE 'DCOM0513'         TO ERR-PGM
           ELSE
               MOVE 'SENHAS02'         TO ERR-PGM
               MOVE 'DCOM0513'         TO ERR-MODULO
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
