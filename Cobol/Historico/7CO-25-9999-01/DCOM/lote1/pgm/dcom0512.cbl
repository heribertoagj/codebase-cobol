      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID.     DCOM0512.
       AUTHOR.         FABRICA - ALTRAN.
      *================================================================*
      *                          A L T R A N                           *
      *----------------------------------------------------------------*
      *     PROGRAMA....: DCOM0512                                     *
      *     PROGRAMADOR.: FABRICA - ALTRAN.                            *
      *     ANALISTA ...: EMERSON ARTUR FRANCO                         *
      *     DATA........: 20/09/2007                                   *
      *----------------------------------------------------------------*
      *     OBJETIVO....: EXCLUIR ESTORNO DE CONTRATO - DADOS BASICOS  *
      *----------------------------------------------------------------*
      *     TELAS.......:                                              *
      *     DCOMELTM....: ESTORNO DO CONTRATO LIMITE - DADOS BASICOS   *
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
      *     DCOM5496 - EFETIVA EXCLUSAO DO ESTORNO                     *
      *----------------------------------------------------------------*
      *     NAVEGACAO...:                                              *
      *     CHAMADO POR.: DCOM0508           CHAMA: -                  *
      *                   DCOM0509                                     *
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
       01  FILLER                      PIC  X(50)          VALUE
           '*** DCOM0512 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*
       77  WIND                        PIC  9(09)          VALUE ZEROS.
       77  WIND2                       PIC  9(09)          VALUE ZEROS.
       77  WRK-MSG004                  PIC  X(079)         VALUE
           'ESTORNO DE CONTRATO LIMITE EXCLUIDO COM SUCESSO'.

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
               10  WRK-COMU-CCPFCNPJ       PIC  X(09)     VALUE SPACES.
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
FS2511*        05  ROTSAI-CFLIAL-CNPJ              PIC  9(04).
FS2511         05  ROTSAI-CFLIAL-CNPJ              PIC  X(04).
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
           '*** AREA DO MODULO DCOM5496 ***'.
      *----------------------------------------------------------------*

       01  ENTRADA-ROTEADOR-5496.
           03  RE5496-COD-RETORNO                 PIC  X(04).
           03  RE5496-MSG-RETORNO                 PIC  X(79).
           03  RE5496-RESTART                     PIC  9(05).
           03  RE5496-FLAG                        PIC  X(01).
           03  RE5496-CFUNC-BDSCO                 PIC  9(09).
           03  RE5496-CTERM                       PIC  X(08).
           03  RE5496-CCONTR-LIM-DESC             PIC  9(09).

       01  SAIDA-ROTEADOR-5496.
           03  RS5496-HEADER.
               05  RS5496-COD-RETORNO             PIC  X(04).
               05  RS5496-MSG-RETORNO             PIC  X(79).
               05  RS5496-RESTART                 PIC  9(05).
               05  RS5496-FLAG                    PIC  X(01).
               05  RS5496-COUNT                   PIC  9(09).
           03  RS5496-CONSISTENCIA.
               05  RS5496-TAMANHO-ERRO            PIC  9(001).
               05  RS5496-TABELA-ERROS            OCCURS 1 TIMES.
                   07  RS5496-CAMPOS-ERRO         PIC  9(001).

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
           '*** AREA DA TELA DCOMELTM - INPUT ***'.
      *----------------------------------------------------------------*
       01  WRK-INPUT-ELTM.
           05  FILLER                          PIC  X(18)  VALUE SPACES.
           05  WRK-INP-PFK-ELTM                PIC  X(01)  VALUE SPACES.
           05  WRK-INP-SENHAS-ELTM             PIC  X(37)  VALUE SPACES.
           05  WRK-INP-COMANDO-ELTM            PIC  X(68)  VALUE SPACES.
           05  WRK-INP-DADOS-ELTM.
               10  WRK-INP-TIPOOPC-ELTM        PIC  X(01)  VALUE SPACES.
               10  WRK-INP-TPPESQ-ELTM         PIC  X(03)  VALUE SPACES.
               10  WRK-INP-OCORR-ELTM          PIC  9(03)  VALUE ZEROS.
               10  WRK-INP-TMSTAMP-ELTM        PIC  X(26)  VALUE SPACES.
               10  WRK-INP-CHAMADO-ELTM        PIC  X(08)  VALUE SPACES.
               10  WRK-INP-PAGINA-ELTM-X.
                   15  WRK-INP-PAGINA-ELTM     PIC  9(03)  VALUE ZEROS.
               10  WRK-INP-FASE-ELTM           PIC  X(01)  VALUE SPACES.
               10  WRK-INP-NOME-ELTM           PIC  X(40)  VALUE SPACES.
               10  WRK-INP-CNPJCPF-ELTM.
FS2511*            15  WRK-INP-NUMERO1-ELTM    PIC  9(03)  VALUE ZEROS.
FS2511             15  WRK-INP-NUMERO1-ELTM    PIC  X(03)  VALUE SPACES.
                   15  WRK-INP-PONTO-1-ELTM    PIC  X(01)  VALUE SPACES.
FS2511*            15  WRK-INP-NUMERO2-ELTM    PIC  9(03)  VALUE ZEROS.
FS2511             15  WRK-INP-NUMERO2-ELTM    PIC  X(03)  VALUE SPACES.
                   15  WRK-INP-PONTO-2-ELTM    PIC  X(01)  VALUE SPACES.
FS2511*            15  WRK-INP-NUMERO3-ELTM    PIC  9(03)  VALUE ZEROS.
FS2511             15  WRK-INP-NUMERO3-ELTM    PIC  X(03)  VALUE SPACES.
                   15  WRK-INP-BARRA-1-ELTM    PIC  X(01)  VALUE SPACES.
FS2511*            15  WRK-INP-FILIAL-ELTM     PIC  9(04)  VALUE ZEROS.
FS2511             15  WRK-INP-FILIAL-ELTM     PIC  X(04)  VALUE SPACES.
                   15  WRK-INP-BARRA-2-ELTM    PIC  X(01)  VALUE SPACES.
                   15  WRK-INP-CONTROLE-ELTM   PIC  9(02)  VALUE ZEROS.
               10  WRK-INP-BANCO-ELTM.
                   15  WRK-INP-BANCO-ELTM-N    PIC  9(03)  VALUE ZEROS.
               10  WRK-INP-AGENCIA-GR-ELTM.
                   15  WRK-INP-AGENCIA-ELTM    PIC  9(05)  VALUE ZEROS.
                   15  WRK-INP-HIFEN-AG-ELTM   PIC  X(01)  VALUE ZEROS.
                   15  WRK-INP-NMAGENC-ELTM    PIC  X(23)  VALUE SPACES.
               10  WRK-INP-CONTA-ELTM.
                   15  WRK-INP-CONTA-ELTM-N    PIC  9(13)  VALUE ZEROS.
               10  WRK-INP-PRODUTO-ELTM.
                   15  WRK-INP-CODPROD-ELTM-N  PIC  9(03)  VALUE ZEROS.
               10  WRK-INP-DESPROD-ELTM        PIC  X(10)  VALUE SPACES.
               10  WRK-INP-CONTR-ELTM          PIC  X(09)  VALUE SPACES.
               10  WRK-INP-TAB-SUBP-ELTM       OCCURS 3 TIMES.
                   15  WRK-INP-LIN-SUBP-ELTM   PIC X(050).
               10  WRK-INP-SITUAC-ELTM         PIC  X(15)  VALUE SPACES.
               10  WRK-INP-DTCONTR-ELTM        PIC  X(10)  VALUE SPACES.
               10  WRK-INP-NUMGAR-ELTM.
                   15  WRK-INP-NUMGAR-ELTM-N   PIC  9(03)  VALUE ZEROS.
               10  WRK-INP-DESGAR-ELTM         PIC  X(20)  VALUE SPACES.
               10  WRK-INP-DTVENC-ELTM         PIC  X(10)  VALUE SPACES.
               10  WRK-INP-PRZDIAS-ELTM        PIC  X(03)  VALUE SPACES.
               10  WRK-INP-VLLIM-ELTM          PIC  X(18)  VALUE SPACES.
               10  WRK-INP-RENAUTO-ELTM        PIC  X(01)  VALUE SPACES.
               10  WRK-INP-TPSUB-ELTM          PIC  X(01)  VALUE SPACES.
               10  WRK-INP-TPCTLIM-ELTM        PIC  X(01)  VALUE SPACES.
               10  WRK-INP-TPISENT-ELTM        PIC  X(01)  VALUE SPACES.
               10  WRK-INP-TPVLFIX-ELTM        PIC  X(01)  VALUE SPACES.
               10  WRK-INP-TPPCENT-ELTM        PIC  X(01)  VALUE SPACES.
               10  WRK-INP-VLTAC-ELTM          PIC  X(18)  VALUE SPACES.
           05  WRK-INP-MENSA-ELTM              PIC  X(79)  VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                            PIC  X(50)    VALUE
           '*** AREA DA TELA DCOMELTM  - OUTPUT ***'.
      *----------------------------------------------------------------*
       01  WRK-OUTPUT-ELTM.
           05  WRK-OUT-LL-ELTM         PIC S9(04) COMP     VALUE +0000.
           05  WRK-OUT-ZZ-ELTM         PIC S9(04) COMP     VALUE ZEROS.
           05  WRK-OUT-SENHAS-ELTM             PIC  X(37)  VALUE SPACES.
           05  WRK-OUT-COMANDO-ELTM            PIC  X(68)  VALUE SPACES.
           05  WRK-OUT-DADOS-ELTM.
               10  WRK-OUT-TIPOOPC-ELTM        PIC  X(01)  VALUE SPACES.
               10  WRK-OUT-TPPESQ-ELTM         PIC  X(03)  VALUE SPACES.
               10  WRK-OUT-OCORR-ELTM          PIC  9(03)  VALUE ZEROS.
               10  WRK-OUT-TMSTAMP-ELTM        PIC  X(26)  VALUE SPACES.
               10  WRK-OUT-CHAMADO-ELTM        PIC  X(08)  VALUE SPACES.
               10  WRK-OUT-PAGINA-ELTM-X.
                   15  WRK-OUT-PAGINA-ELTM     PIC  9(03)  VALUE ZEROS.
               10  WRK-OUT-FASE-ELTM           PIC  X(01)  VALUE SPACES.
               10  WRK-OUT-NOME-ELTM           PIC  X(40)  VALUE SPACES.
               10  WRK-OUT-CNPJCPF-ELTM.
FS2511             15  WRK-OUT-NUMERO1-ELTM    PIC  X(03)  VALUE SPACES.
                   15  WRK-OUT-PONTO-1-ELTM    PIC  X(01)  VALUE SPACES.
FS2511             15  WRK-OUT-NUMERO2-ELTM    PIC  X(03)  VALUE SPACES.
                   15  WRK-OUT-PONTO-2-ELTM    PIC  X(01)  VALUE SPACES.
FS2511             15  WRK-OUT-NUMERO3-ELTM    PIC  X(03)  VALUE SPACES.
                   15  WRK-OUT-BARRA-1-ELTM    PIC  X(01)  VALUE SPACES.
FS2511             15  WRK-OUT-FILIAL-ELTM     PIC  X(04)  VALUE SPACES.
                   15  WRK-OUT-BARRA-2-ELTM    PIC  X(01)  VALUE SPACES.
                   15  WRK-OUT-CONTROLE-ELTM   PIC  9(02)  VALUE ZEROS.
               10  WRK-OUT-BANCO-ELTM.
                   15  WRK-OUT-BANCO-ELTM-N    PIC  9(03)  VALUE ZEROS.
               10  WRK-OUT-AGENCIA-GR-ELTM.
                   15  WRK-OUT-AGENCIA-ELTM    PIC  9(05)  VALUE ZEROS.
                   15  WRK-OUT-HIFEN-AG-ELTM   PIC  X(01)  VALUE ZEROS.
                   15  WRK-OUT-NMAGENC-ELTM    PIC  X(23)  VALUE SPACES.
               10  WRK-OUT-CONTA-ELTM.
                   15  WRK-OUT-CONTA-ELTM-N    PIC  9(13)  VALUE ZEROS.
               10  WRK-OUT-PRODUTO-ELTM.
                   15  WRK-OUT-CODPROD-ELTM-N  PIC  9(03)  VALUE ZEROS.
               10  WRK-OUT-DESPROD-ELTM        PIC  X(10)  VALUE SPACES.
               10  WRK-OUT-CONTR-ELTM          PIC  X(09)  VALUE SPACES.
               10  WRK-OUT-TAB-SUBP-ELTM       OCCURS 3 TIMES.
                   15  WRK-OUT-LIN-SUBP-ELTM   PIC X(050).
               10  WRK-OUT-SITUAC-ELTM         PIC  X(15)  VALUE SPACES.
               10  WRK-OUT-DTCONTR-ELTM        PIC  X(10)  VALUE SPACES.
               10  WRK-OUT-NUMGAR-ELTM.
                   15  WRK-OUT-NUMGAR-ELTM-N   PIC  9(03)  VALUE ZEROS.
               10  WRK-OUT-DESGAR-ELTM         PIC  X(20)  VALUE SPACES.
               10  WRK-OUT-DTVENC-ELTM         PIC  X(10)  VALUE SPACES.
               10  WRK-OUT-PRZDIAS-ELTM.
                   15  WRK-OUT-PRZDIAS-ELTM-N  PIC  9(03)  VALUE ZEROS.
               10  WRK-OUT-VLLIM-ELTM.
                   15  WRK-OUT-VLLIM-ELTM-N    PIC  ZZZ.ZZZ.ZZZ.ZZ9,99.
               10  WRK-OUT-RENAUTO-ELTM        PIC  X(01)  VALUE SPACES.
               10  WRK-OUT-TIPO-TAC-ELTM.
                   15  WRK-OUT-TPSUB-ELTM      PIC  X(01)  VALUE SPACES.
                   15  WRK-OUT-TPCTLIM-ELTM    PIC  X(01)  VALUE SPACES.
                   15  WRK-OUT-TPISENT-ELTM    PIC  X(01)  VALUE SPACES.
                   15  WRK-OUT-TPVLFIX-ELTM    PIC  X(01)  VALUE SPACES.
                   15  WRK-OUT-TPPCENT-ELTM    PIC  X(01)  VALUE SPACES.
               10  WRK-OUT-VLTAC-ELTM.
                   15  WRK-OUT-VLTAC-ELTM-N    PIC  ZZZ.ZZZ.ZZZ.ZZ9,99.
           05  WRK-OUT-MENSA-ELTM              PIC  X(79)  VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                            PIC  X(50)   VALUE
           '*** AREA DA TELA DCOMELTM - BRAD0660 ***'.
      *----------------------------------------------------------------*
       01  WRK-660-DCOMELTM.
           05  WRK-660-LL-AREA-ELTM      PIC  9(004) COMP    VALUE 0000.
           05  WRK-660-LL-MENSA-ELTM     PIC  9(004) COMP    VALUE 0000.
           05  WRK-660-SENHAS-ELTM       PIC  9(004) COMP    VALUE 0037.
           05  WRK-660-COMANDO-ELTM      PIC  9(004) COMP    VALUE 0068.
           05  WRK-660-TIPOOPC-ELTM      PIC  9(004) COMP    VALUE 0001.
           05  WRK-660-TPPESQ-ELTM       PIC  9(004) COMP    VALUE 0003.
           05  WRK-660-OCORR-ELTM        PIC  9(004) COMP    VALUE 0003.
           05  WRK-660-TMSTAMP-ELTM      PIC  9(004) COMP    VALUE 0026.
           05  WRK-660-CHAMADO-ELTM      PIC  9(004) COMP    VALUE 0008.
           05  WRK-660-PAGINA-ELTM       PIC  9(004) COMP    VALUE 0003.
           05  WRK-660-FASE-ELTM         PIC  9(004) COMP    VALUE 0001.
           05  WRK-660-NOME-ELTM         PIC  9(004) COMP    VALUE 0040.
           05  WRK-660-CNPJCPF-ELTM      PIC  9(004) COMP    VALUE 0019.
           05  WRK-660-BANCO-ELTM        PIC  9(004) COMP    VALUE 0003.
           05  WRK-660-AGENCIA-ELTM      PIC  9(004) COMP    VALUE 0029.
           05  WRK-660-CONTA-ELTM        PIC  9(004) COMP    VALUE 0013.
           05  WRK-660-CODPROD-ELTM      PIC  9(004) COMP    VALUE 0003.
           05  WRK-660-DESPROD-ELTM      PIC  9(004) COMP    VALUE 0010.
           05  WRK-660-CONTR-ELTM        PIC  9(004) COMP    VALUE 0009.
           05  WRK-660-TAB-SUBP-ELTM     OCCURS 3 TIMES.
               10  WRK-660-LIN-SUBP-ELTM PIC  9(004) COMP    VALUE 0050.
           05  WRK-660-SITUAC-ELTM       PIC  9(004) COMP    VALUE 0015.
           05  WRK-660-DTCONTR-ELTM      PIC  9(004) COMP    VALUE 0010.
           05  WRK-660-NUMGAR-ELTM       PIC  9(004) COMP    VALUE 0003.
           05  WRK-660-DESGAR-ELTM       PIC  9(004) COMP    VALUE 0020.
           05  WRK-660-DTVENC-ELTM       PIC  9(004) COMP    VALUE 0010.
           05  WRK-660-PRZDIAS-ELTM      PIC  9(004) COMP    VALUE 0003.
           05  WRK-660-VLLIM-ELTM        PIC  9(004) COMP    VALUE 0018.
           05  WRK-660-RENAUTO-ELTM      PIC  9(004) COMP    VALUE 0001.
           05  WRK-660-TPSUB-ELTM        PIC  9(004) COMP    VALUE 0001.
           05  WRK-660-TPCTLIM-ELTM      PIC  9(004) COMP    VALUE 0001.
           05  WRK-660-TPISENT-ELTM      PIC  9(004) COMP    VALUE 0001.
           05  WRK-660-TPVLFIX-ELTM      PIC  9(004) COMP    VALUE 0001.
           05  WRK-660-TPPCENT-ELTM      PIC  9(004) COMP    VALUE 0001.
           05  WRK-660-VLTAC-ELTM        PIC  9(004) COMP    VALUE 0018.
           05  WRK-660-MENSA-ELTM        PIC  9(004) COMP    VALUE 0079.

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
           '*** DCOM0512 - FIM DA AREA DE WORKING ***'.
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

           MOVE SPACES                 TO WRK-OUT-DADOS-ELTM
                                          WRK-OUT-SENHAS-ELTM
                                          WRK-OUT-COMANDO-ELTM.

           INITIALIZE                     ENTRADA-ROTEADOR
                                          SAIDA-ROTEADOR
                                          ENTRADA-ROTEADOR-4113
                                          SAIDA-ROTEADOR-4113
                                          ENTRADA-ROTEADOR-5496
                                          SAIDA-ROTEADOR-5496
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

           MOVE 'DCOMELTM'             TO WRK-TELA

           IF  WRK-TRANSACAO           NOT EQUAL 'DCOM0512'
               PERFORM 2100-RECEBER-CONTROLE
           ELSE
               PERFORM 2200-PROCESSAR-DCOMELTM
           END-IF.

           IF  WRK-TELA                EQUAL 'DCOMELTM'
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
               MOVE  WRK-COMU-TPOPCAO  TO WRK-OUT-TIPOOPC-ELTM
                                          WRK-INP-TIPOOPC-ELTM
               MOVE  WRK-COMU-TMSTAMP  TO WRK-OUT-TMSTAMP-ELTM
                                          WRK-INP-TMSTAMP-ELTM
               MOVE  ZEROS             TO WRK-OUT-AGENCIA-ELTM
                                          WRK-OUT-PAGINA-ELTM
                                          WRK-INP-PAGINA-ELTM
               MOVE  WRK-COMU-CONTRATO-LIM
                                       TO WRK-INP-CONTR-ELTM
                                          WRK-OUT-CONTR-ELTM
               MOVE 'CTR'              TO WRK-INP-TPPESQ-ELTM
                                          WRK-OUT-TPPESQ-ELTM

           ELSE
               MOVE  WRK-MENSAGEM      TO WRK-COMU-AREA
               MOVE  WRK-COMU-TPOPCAO  TO WRK-OUT-TIPOOPC-ELTM
                                          WRK-INP-TIPOOPC-ELTM
               MOVE  WRK-COMU-TMSTAMP
                                       TO WRK-OUT-TMSTAMP-ELTM
                                          WRK-INP-TMSTAMP-ELTM
               MOVE  WRK-COMU-CONTRATO-LIM
                                       TO WRK-INP-CONTR-ELTM
                                          WRK-OUT-CONTR-ELTM
               MOVE  WRK-COMU-PAG      TO WRK-INP-PAGINA-ELTM
                                          WRK-OUT-PAGINA-ELTM
               MOVE  WRK-COMU-OCORR    TO WRK-INP-OCORR-ELTM
                                          WRK-OUT-OCORR-ELTM
               IF WRK-COMU-AGENCIA     NOT EQUAL  ZEROS
                  MOVE 'AGE'           TO WRK-INP-TPPESQ-ELTM
                                          WRK-OUT-TPPESQ-ELTM
               ELSE
                  MOVE 'CPF'           TO WRK-INP-TPPESQ-ELTM
                                          WRK-OUT-TPPESQ-ELTM
               END-IF
           END-IF.

           MOVE  WRK-TRANSACAO         TO WRK-INP-CHAMADO-ELTM
                                          WRK-OUT-CHAMADO-ELTM.
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
              MOVE SG-MENSAGEM(110)    TO WRK-OUT-MENSA-ELTM
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
                   MOVE  SG-MENSAGEM(110)
                                       TO  WRK-OUT-MENSA-ELTM

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

           MOVE  ROTSAI-CCNPJ-CPF(1:3)     TO WRK-OUT-NUMERO1-ELTM
           MOVE  ROTSAI-CCNPJ-CPF(4:3)     TO WRK-OUT-NUMERO2-ELTM
           MOVE  ROTSAI-CCNPJ-CPF(7:3)     TO WRK-OUT-NUMERO3-ELTM
           MOVE  ROTSAI-CFLIAL-CNPJ        TO WRK-OUT-FILIAL-ELTM
           MOVE  ROTSAI-CCTRL-CNPJ-CPF     TO WRK-OUT-CONTROLE-ELTM
           MOVE  '.'                       TO WRK-OUT-PONTO-1-ELTM
                                              WRK-OUT-PONTO-2-ELTM
           MOVE  '/'                       TO WRK-OUT-BARRA-1-ELTM
           MOVE  '-'                       TO WRK-OUT-BARRA-2-ELTM
           MOVE  ROTSAI-IRSUMO-SIT-DESC    TO WRK-OUT-SITUAC-ELTM
           MOVE  ROTSAI-CBCO               TO WRK-OUT-BANCO-ELTM-N
           MOVE  ROTSAI-CAG-BCRIA          TO WRK-OUT-AGENCIA-ELTM
           MOVE  '-'                       TO WRK-OUT-HIFEN-AG-ELTM
           MOVE  ROTSAI-CCTA-BCRIA-CLI     TO WRK-OUT-CONTA-ELTM-N
           MOVE  ROTSAI-DINIC-VGCIA-CONTR  TO WRK-OUT-DTCONTR-ELTM
           INSPECT WRK-OUT-DTCONTR-ELTM REPLACING ALL '.' BY '/'
           MOVE  ROTSAI-CINDCD-RENOV-AUTOM TO WRK-OUT-RENAUTO-ELTM
           MOVE  ROTSAI-DVCTO-CONTR-LIM    TO WRK-OUT-DTVENC-ELTM
           INSPECT WRK-OUT-DTVENC-ELTM REPLACING ALL '.' BY '/'
           MOVE  ROTSAI-VCONTR-LIM-DESC    TO WRK-OUT-VLLIM-ELTM-N
           MOVE  ROTSAI-VTAC-CONTR-LIM     TO WRK-OUT-VLTAC-ELTM-N
           MOVE  ROTSAI-TCONTR-LIM-DESC    TO WRK-OUT-PRZDIAS-ELTM-N
           MOVE  SPACES                    TO WRK-OUT-TIPO-TAC-ELTM

           IF ROTSAI-CTPO-COBR-TAC  EQUAL  999 OR ZEROS OR 004
              MOVE 'X'                     TO WRK-OUT-TPSUB-ELTM
           ELSE
              IF ROTSAI-CTPO-COBR-TAC  EQUAL  01
                 MOVE 'X'                  TO WRK-OUT-TPCTLIM-ELTM
                                              WRK-OUT-TPISENT-ELTM
              ELSE
                 IF ROTSAI-CTPO-COBR-TAC  EQUAL  02
                    MOVE 'X'               TO WRK-OUT-TPCTLIM-ELTM
                                              WRK-OUT-TPVLFIX-ELTM
                 ELSE
                    IF ROTSAI-CTPO-COBR-TAC  EQUAL  03
                       MOVE 'X'            TO WRK-OUT-TPCTLIM-ELTM
                                              WRK-OUT-TPPCENT-ELTM
                    END-IF
                 END-IF
              END-IF
           END-IF.
           IF    ROTSAI-CGARAN         EQUAL ZEROS
                 MOVE SPACES           TO  WRK-OUT-NUMGAR-ELTM
                                           WRK-OUT-DESGAR-ELTM
           ELSE
                 MOVE ROTSAI-CGARAN    TO  WRK-OUT-NUMGAR-ELTM-N
                 MOVE ROTSAI-IGARAN    TO  WRK-OUT-DESGAR-ELTM
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
                                       TO WRK-OUT-NMAGENC-ELTM

              WHEN '0099'
                   MOVE 'DB2'          TO ERR-TIPO-ACESSO
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO

              WHEN OTHER
                   MOVE ROTSAI-MSG-RETORNO-6416
                                       TO WRK-OUT-MENSA-ELTM
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
                   MOVE 4170-SAI-IPSSOA-DESC-COML TO WRK-OUT-NOME-ELTM
               WHEN '0099'

                   MOVE 'DB2'          TO ERR-TIPO-ACESSO
                   MOVE '0010'         TO ERR-LOCAL
                   MOVE '2132-ACESSAR-DCOM4170'
                                       TO ERR-TEXTO
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO
               WHEN OTHER

                  MOVE 'S'                       TO WRK-ERRO-ACESSO
                  MOVE 4170-SAI-MSG-RET          TO  WRK-RESTO-4170
                  MOVE WRK-MENSAG-4170           TO  WRK-OUT-MENSA-ELTM

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
                                       TO WRK-OUT-NOME-ELTM

              WHEN '0099'
                   MOVE 'DB2'          TO ERR-TIPO-ACESSO
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO

              WHEN OTHER
                   MOVE ROTSAI-6408-MENS-RET
                                       TO WRK-OUT-MENSA-ELTM
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
                   MOVE ROTSAI-CPRODT-4113  TO WRK-OUT-CODPROD-ELTM-N
                   MOVE ROTSAI-IABREV-PRODT-4113
                                            TO WRK-OUT-DESPROD-ELTM
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
                                       TO WRK-OUT-MENSA-ELTM
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
               MOVE ROTSAI-IRSUMO-SPROD-4113 (WIND)
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

           MOVE WRK-LIN-SUBPROD    TO  WRK-OUT-LIN-SUBP-ELTM(WIND2)

           ADD     1               TO  WIND
                                       WIND2.

      *----------------------------------------------------------------*
       2150-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2200-PROCESSAR-DCOMELTM         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM           TO WRK-INPUT-ELTM.

           PERFORM 2210-DEVOLVER-TELA

           EVALUATE TRUE
               WHEN WRK-INP-PFK-ELTM     EQUAL 'H' OR
                   (WRK-INP-PFK-ELTM     EQUAL '.' AND
                    WRK-INP-COMANDO-ELTM EQUAL 'PFK01')
                    PERFORM 2210-DEVOLVER-TELA
                    MOVE    WRK-INP-MENSA-ELTM  TO WRK-OUT-MENSA-ELTM

               WHEN WRK-INP-PFK-ELTM     EQUAL '3' OR
                   (WRK-INP-PFK-ELTM     EQUAL '.' AND
                    WRK-INP-COMANDO-ELTM EQUAL 'PFK03')
                    PERFORM 2220-RETORNAR-CHAMADOR

               WHEN WRK-INP-PFK-ELTM     EQUAL 'X' OR
                   (WRK-INP-PFK-ELTM     EQUAL '.' AND
                    WRK-INP-COMANDO-ELTM EQUAL 'PFK05')
                    PERFORM 2230-MENU-ROTINAS

               WHEN WRK-INP-PFK-ELTM     EQUAL 'V' OR
                   (WRK-INP-PFK-ELTM     EQUAL '.' AND
                    WRK-INP-COMANDO-ELTM EQUAL 'PFK10')
                    PERFORM 2270-MENU-DCOM

               WHEN WRK-INP-PFK-ELTM     EQUAL '6' OR
                   (WRK-INP-PFK-ELTM     EQUAL '.' AND
                    WRK-INP-COMANDO-ELTM EQUAL 'PFK06')
                    PERFORM 2240-PF6-CONFIRMA

               WHEN (WRK-INP-PFK-ELTM          EQUAL '.' AND
                     WRK-INP-COMANDO-ELTM      EQUAL SPACES)
                     PERFORM 2280-TRATAR-ENTER

               WHEN OTHER
                     MOVE    SG-MENSAGEM(38)     TO WRK-OUT-MENSA-ELTM

           END-EVALUATE.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2210-DEVOLVER-TELA              SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-SENHAS-ELTM    TO WRK-OUT-SENHAS-ELTM
           MOVE WRK-INP-TIPOOPC-ELTM   TO WRK-OUT-TIPOOPC-ELTM
           MOVE WRK-INP-TPPESQ-ELTM    TO WRK-OUT-TPPESQ-ELTM
           MOVE WRK-INP-OCORR-ELTM     TO WRK-OUT-OCORR-ELTM
           MOVE WRK-INP-TMSTAMP-ELTM   TO WRK-OUT-TMSTAMP-ELTM
           MOVE WRK-INP-CHAMADO-ELTM   TO WRK-OUT-CHAMADO-ELTM
           MOVE WRK-INP-PAGINA-ELTM-X  TO WRK-OUT-PAGINA-ELTM-X
           MOVE WRK-INP-FASE-ELTM      TO WRK-OUT-FASE-ELTM
           MOVE WRK-INP-NOME-ELTM      TO WRK-OUT-NOME-ELTM
           MOVE WRK-INP-CNPJCPF-ELTM   TO WRK-OUT-CNPJCPF-ELTM
           MOVE WRK-INP-SITUAC-ELTM    TO WRK-OUT-SITUAC-ELTM
           MOVE WRK-INP-BANCO-ELTM     TO WRK-OUT-BANCO-ELTM
           MOVE WRK-INP-AGENCIA-GR-ELTM
                                       TO WRK-OUT-AGENCIA-GR-ELTM
           MOVE WRK-INP-CONTA-ELTM     TO WRK-OUT-CONTA-ELTM
           MOVE WRK-INP-PRODUTO-ELTM   TO WRK-OUT-PRODUTO-ELTM
           MOVE WRK-INP-DESPROD-ELTM   TO WRK-OUT-DESPROD-ELTM
           MOVE WRK-INP-CONTR-ELTM     TO WRK-OUT-CONTR-ELTM
           MOVE WRK-INP-SITUAC-ELTM    TO WRK-OUT-SITUAC-ELTM
           MOVE WRK-INP-DTCONTR-ELTM   TO WRK-OUT-DTCONTR-ELTM
           MOVE WRK-INP-NUMGAR-ELTM    TO WRK-OUT-NUMGAR-ELTM
           MOVE WRK-INP-DESGAR-ELTM    TO WRK-OUT-DESGAR-ELTM
           MOVE WRK-INP-DTVENC-ELTM    TO WRK-OUT-DTVENC-ELTM
           MOVE WRK-INP-PRZDIAS-ELTM   TO WRK-OUT-PRZDIAS-ELTM
           MOVE WRK-INP-VLLIM-ELTM     TO WRK-OUT-VLLIM-ELTM
           MOVE WRK-INP-RENAUTO-ELTM   TO WRK-OUT-RENAUTO-ELTM
           MOVE WRK-INP-TPSUB-ELTM     TO WRK-OUT-TPSUB-ELTM
           MOVE WRK-INP-TPCTLIM-ELTM   TO WRK-OUT-TPCTLIM-ELTM
           MOVE WRK-INP-TPISENT-ELTM   TO WRK-OUT-TPISENT-ELTM
           MOVE WRK-INP-TPVLFIX-ELTM   TO WRK-OUT-TPVLFIX-ELTM
           MOVE WRK-INP-TPPCENT-ELTM   TO WRK-OUT-TPPCENT-ELTM
           MOVE WRK-INP-VLTAC-ELTM     TO WRK-OUT-VLTAC-ELTM

           PERFORM  VARYING  WIND  FROM 1 BY 1
                      UNTIL  WIND  GREATER 03

               MOVE WRK-INP-LIN-SUBP-ELTM(WIND)
                                       TO WRK-OUT-LIN-SUBP-ELTM(WIND)

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
           MOVE WRK-INP-CHAMADO-ELTM     TO WRK-TELA
           MOVE 'DCOM0512'               TO WRK-COMU-TRANSACAO
           MOVE '3'                      TO WRK-COMU-PFK1
           MOVE WRK-INP-SENHAS-ELTM      TO WRK-COMU-SENHAS
           MOVE WRK-INP-COMANDO-ELTM     TO WRK-COMU-COMANDO
           MOVE WRK-INP-TIPOOPC-ELTM     TO WRK-COMU-TPOPCAO
           MOVE WRK-INP-TMSTAMP-ELTM     TO WRK-COMU-TMSTAMP
           IF  WRK-ERRO-MOD  EQUAL  'S'
               MOVE ROTSAI-MSG-RETORNO   TO WRK-COMU-MENSA
           END-IF
           IF  WRK-PFK06     EQUAL  'S'
               MOVE '6'                  TO WRK-COMU-PFK1
               MOVE 'S'                  TO WRK-COMU-ACAO
               MOVE WRK-MSG004           TO WRK-COMU-MENSA
           END-IF

           IF  WRK-INP-CHAMADO-ELTM   EQUAL   'DCOM0508'
               MOVE WRK-INP-CONTR-ELTM       TO WRK-COMU-CONTRATO-LIM
               MOVE WRK-COMU-AREA                TO WRK-MENSAGEM
           ELSE
               MOVE WRK-INP-PAGINA-ELTM      TO WRK-COMU-PAG
               MOVE WRK-INP-OCORR-ELTM       TO WRK-COMU-OCORR
               IF WRK-INP-TPPESQ-ELTM  EQUAL  'AGE'
                  MOVE WRK-INP-AGENCIA-ELTM  TO WRK-COMU-AGENCIA
                  MOVE WRK-INP-BANCO-ELTM-N  TO WRK-COMU-BANCO
                  MOVE WRK-INP-CONTA-ELTM-N  TO WRK-COMU-CONTA
                  MOVE ZEROS                 TO WRK-COMU-CCPFCNPJ
                                                WRK-COMU-CFLIAL
                                                WRK-COMU-CCTRL
               ELSE
                  MOVE ZEROS                 TO WRK-COMU-AGENCIA
                                                WRK-COMU-BANCO
                                                WRK-COMU-CONTA
                  MOVE WRK-INP-NUMERO1-ELTM  TO WRK-COMU-CCPFCNPJ(1:3)
                  MOVE WRK-INP-NUMERO2-ELTM  TO WRK-COMU-CCPFCNPJ(4:3)
                  MOVE WRK-INP-NUMERO3-ELTM  TO WRK-COMU-CCPFCNPJ(7:3)
                  MOVE WRK-OUT-FILIAL-ELTM   TO WRK-COMU-CFLIAL
                  MOVE WRK-OUT-CONTROLE-ELTM TO WRK-COMU-CCTRL
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
           MOVE 'DCOM0512'             TO WRK-TELA
                                          WRK-COMU-TRANSACAO-PFK
           MOVE '5'                    TO WRK-COMU-PFK
           MOVE WRK-COMU-AREA-PFK          TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2240-PF6-CONFIRMA               SECTION.
      *----------------------------------------------------------------*
           MOVE 'N'                    TO  WRK-PFK06
           MOVE WRK-INP-CONTR-ELTM     TO  RE5496-CCONTR-LIM-DESC
           MOVE LNK-IO-LTERM           TO  RE5496-CTERM
           MOVE WRK-COD-USER           TO  RE5496-CFUNC-BDSCO
           MOVE ZEROS                  TO  RE5496-FLAG
           MOVE 'DCOM5496'             TO  WRK-MODULO

           CALL WRK-MODULO             USING   ENTRADA-ROTEADOR-5496
                                               SAIDA-ROTEADOR-5496
                                               ERRO-AREA
                                               WRK-SQLCA

           EVALUATE RS5496-COD-RETORNO
               WHEN '0000'
                       MOVE 'S'   TO WRK-PFK06
                       PERFORM 2220-RETORNAR-CHAMADOR
               WHEN '0099'
                       MOVE 'DB2' TO ERR-TIPO-ACESSO
                       PERFORM 9999-PROCESSAR-ROTINA-ERRO
               WHEN OTHER
                       MOVE '*'        TO WRK-INCONSIS
                       MOVE RS5496-MSG-RETORNO
                                       TO WRK-OUT-MENSA-ELTM
           END-EVALUATE.

      *----------------------------------------------------------------*
       2240-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2270-MENU-DCOM                  SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-COMU-AREA-PFK

           MOVE LENGTH OF WRK-COMU-AREA-PFK
                                       TO WRK-COMU-LL-PFK
           MOVE WRK-CHNG               TO WRK-FUNCAO
           MOVE 'DCOM0512'             TO WRK-TELA
                                          WRK-COMU-TRANSACAO-PFK
           MOVE 'A'                    TO WRK-COMU-PFK
           MOVE WRK-COMU-AREA-PFK      TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2270-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2280-TRATAR-ENTER               SECTION.
      *----------------------------------------------------------------*

           MOVE SG-MENSAGEM(110)       TO      WRK-OUT-MENSA-ELTM.

      *----------------------------------------------------------------*
       2280-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2300-PROCESSAR-BRAD0660         SECTION.
      *----------------------------------------------------------------*
           MOVE LENGTH OF WRK-OUTPUT-ELTM
                                       TO  WRK-660-LL-MENSA-ELTM
                                           WRK-OUT-LL-ELTM

           MOVE LENGTH OF WRK-660-DCOMELTM
                                       TO  WRK-660-LL-AREA-ELTM

           CALL 'BRAD0660'             USING WRK-OUTPUT-ELTM
                                             WRK-660-DCOMELTM.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE RETURN-CODE        TO WRK-RETURN-CODE
               MOVE 10                 TO WRK-LOCAL-ERRO
               MOVE WRK-ERRO-BRAD0660  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-ISRT               TO WRK-FUNCAO.
           MOVE WRK-OUTPUT-ELTM        TO WRK-MENSAGEM.

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
               MOVE 'DCOM0512'         TO ERR-PGM
           ELSE
               MOVE 'SENHAS02'         TO ERR-PGM
               MOVE 'DCOM0512'         TO ERR-MODULO
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
