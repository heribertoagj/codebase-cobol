      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
      *
       PROGRAM-ID.     DCOM0610.
       AUTHOR.         FABRICA - ALTRAN.
      *================================================================*
      *                          A L T R A N                           *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   DCOM0610                                    *
      *    PROGRAMADOR.:   FABRICA - ALTRAN                            *
      *    ANALISTA....:   EMERSON ARTUR FRANCO                        *
      *    DATA........:   21/05/2007                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   CONSULTA OPERACOES - POR CONTRATO           *
      *                                                                *
      *----------------------------------------------------------------*
      *    TELAS.......:                                               *
      *    DCOMFJTM - CONSULTA OPERACOES - POR CONTRATO                *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    I#BRAD7C - AREA DE ERRO DA BRAD7100.                        *
      *    I#DCOMSY - TABELA DE MSG DE RETORNO DO SISTEMA DCOM(ONLINE).*
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    BRAD0660 - INSERIR CARACTERES DE NULL NA MENSAGEM DE SAIDA  *
      *    BRAD7100 - TRATAMENTO DE ERROS QUANDO PROGRAMA INVALIDO     *
      *    POOL5005 - CONTROLE DO TRANSITO DE MENSAGENS NO IMS/DC      *
      *    DCOM6416 - MODULO DE NEGOCIO - OBTER NOME DA AGENCIA        *
      *    DCOM4001 - MODULO DE NEGOCIO - OBTER NOME DO PRODUTO        *
      *    DCOM4049 - MODULO DE NEGOCIO - OBTER NOME DO SUB-PRODUTO    *
      *    DCOM4309 - MODULO DE NEGOCIO - OBTER NOME DA SITUACAO       *
      *----------------------------------------------------------------*
      *    NAVEGACAO...:                                               *
      *    CHAMADO POR.: DCOM0602           CHAMA: DCOM0611            *
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
       01  FILLER                          PIC  X(050)     VALUE
           '*** DCOM0610 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       01  FILLER                          PIC  X(050)     VALUE
           '*** AREA DE ATRIBUTOS ***'.
      *---------------------------------------------------------------*
      *--- NUMERIC  - NORMAL    - PROTEGIDO    - NAO POS CURSOR ---*
       01  WRK-241                         PIC S9(08) COMP VALUE +241.
      *--- NUMERICO - NORMAL    - PROTEGIDO    - POS. CURSOR    ---*
       01  WRK-49361                       PIC S9(08) COMP VALUE +49361.
      *--- NUMERICO - BRILHANTE - DESPROTEGIDO - POS. CURSOR    ---*
       01  WRK-49369                       PIC S9(08) COMP VALUE +49369.
      *--- ALPHA    - NORMAL    - DESPROTEGIDO - POS. CURSOR    ---*
       01  WRK-49345                       PIC S9(08) COMP VALUE +49345.
      *
       01  WRK-UNNS                        PIC S9(08) COMP VALUE +209.
       01  WRK-UNNS-C                      PIC S9(08) COMP VALUE +49361.
       01  WRK-PNNS                        PIC S9(08) COMP VALUE +225.
       01  WRK-PNNS-C                      PIC S9(08) COMP VALUE +49377.
      *
      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(050)     VALUE
           '*** VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*
       01  WRK-AUXILIARES.
           05  IND-1                       PIC  9(009)     VALUE ZEROS.
           05  WRK-MODULO                  PIC  X(008)     VALUE SPACES.
           05  WRK-INCONSIS                PIC  X(001)     VALUE SPACES.
           05  WRK-MENS01                  PIC  X(072)     VALUE
               'DIGITE A OPCAO DESEJADA'.
           05  WRK-MENS02                  PIC  X(072)     VALUE
               'PF INVALIDA'.
           05  WRK-MENS03                  PIC  X(072)     VALUE
               'OPCAO INVALIDA'.
           05  WRK-MENS04                  PIC  X(072)     VALUE
               'CAMPO BRILHANTE INCONSISTENTE'.
           05  WRK-MENS05                  PIC  X(072)     VALUE
               'TECLE <PF6> PARA CONFIRMAR OU <ENTER> PARA ALTERAR'.
           05  WRK-MENS06                  PIC  X(072)     VALUE
               'NUMERO DO CONTRATO OBRIGATORIO'.
           05  WRK-MENS07                  PIC  X(072)     VALUE
               'NUMERO DO CONTRATO INVALIDO'.
           05  WRK-MENS08                  PIC  X(072)     VALUE
               'PRODUTO INVALIDO'.
           05  WRK-MENS09                  PIC  X(072)     VALUE
               'SUBPRODUTO INVALIDO'.
           05  WRK-MENS10                  PIC  X(072)     VALUE
               'AGENCIA INVALIDA'.
           05  WRK-MENS11                  PIC  X(072)     VALUE
               'SITUACAO INVALIDA'.
           05  WRK-MENS12                  PIC  X(072)     VALUE
               'OBRIGATORIO INFORMAR PRODUTO'.

      *
      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(50)      VALUE
           '*** AREA DE COMUNICACAO DCOM0610 / DCOM6209 ***'.
      *----------------------------------------------------------------*
      *
       01          COMU-AREA.
         05        COMU-LL                 PIC S9(04) COMP VALUE +406.
         05        COMU-ZZ                 PIC S9(04) COMP VALUE ZEROS.
         05        COMU-TRANCODE.
            10     COMU-TRANSACAO          PIC X(008)      VALUE SPACES.
            10     FILLER                  PIC X(006)      VALUE SPACES.
            10     COMU-PFK                PIC X(001)      VALUE SPACES.
         05        COMU-SENHAS             PIC X(037)      VALUE SPACES.
         05        COMU-COMANDO            PIC X(068)      VALUE SPACES.
         05        COMU-DADOS.
            10     COMU-TIMESTAMP          PIC X(026)      VALUE SPACES.
      *
      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(50)      VALUE
           '*** AREA DE COMUNICACAO DCOM6416 ***'.
      *
      *----------------------------------------------------------------*
       01          ENTRADA-ROT6416.
         05        ENT6416-COD-RETORNO     PIC X(004)      VALUE SPACES.
         05        ENT6416-MSG-RETORNO     PIC X(079)      VALUE SPACES.
         05        ENT6416-RESTART         PIC 9(005)      VALUE ZEROS.
         05        ENT6416-FLAG            PIC X(001)      VALUE SPACES.
         05        ENT6416-CFUNC-BDSCO     PIC 9(009)      VALUE ZEROS.
         05        ENT6416-CTERM           PIC X(008)      VALUE SPACES.
         05        ENT6416-BANCO           PIC 9(003)      VALUE ZEROS.
         05        ENT6416-DEPDC           PIC 9(005)      VALUE ZEROS.
      *
      *--- AREA DE SAIDA   DO ROTEADOR --> TAM. 0089 + 0107 = 0196  ---*
      *
       01          SAIDA-ROT6416.
         05        SAI6416-HEADER.
            10     SAI6416-COD-RETORNO     PIC X(004)      VALUE SPACES.
            10     SAI6416-MSG-RETORNO     PIC X(079)      VALUE SPACES.
            10     SAI6416-RESTART         PIC 9(005)      VALUE ZEROS.
            10     SAI6416-FLAG            PIC X(001)      VALUE SPACES.
         05        SAI6416-DADOS.
               10  SAI6416-TABELA-SAIDA.
               15  SAI6416-AG-DEPDC        PIC 9(05)       VALUE ZEROS.
               15  SAI6416-DG-DEPDC        PIC X(01)       VALUE SPACES.
               15  SAI6416-NOME-DEPDC      PIC X(40)       VALUE SPACES.
               15  SAI6416-END-DEPDC       PIC X(30)       VALUE SPACES.
               15  SAI6416-DIRETORIA-REG   PIC 9(05)       VALUE ZEROS.
               15  SAI6416-DIRETORIA-NOME  PIC X(40)       VALUE SPACES.
               15  SAI6416-NOME-BANCO      PIC X(40)       VALUE SPACES.
               15  SAI6416-POLO-SERVICO    PIC 9(05)       VALUE ZEROS.
               15  SAI6416-MUNIC-DEPDC     PIC X(25)       VALUE SPACES.
               15  SAI6416-UF              PIC X(02)       VALUE SPACES.
               15  SAI6416-CEP             PIC 9(05)       VALUE ZEROS.
               15  SAI6416-CEP-COMPL       PIC 9(03)       VALUE ZEROS.
               15  SAI6416-INDIC-DEPDC-AT  PIC X(01)       VALUE SPACES.
               15  SAI6416-NATUREZA-DEPDC  PIC X(01)       VALUE SPACES.
               15  SAI6416-COD-DISTR-CORR  PIC 9(05)       VALUE ZEROS.
               15  SAI6416-DENCRR-DEPDC-BDSCO
                                           PIC X(10)       VALUE SPACES.
      *

      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(50)      VALUE
           '*** AREA DE COMUNICACAO DCOM4001 ***'.
      *
      *----------------------------------------------------------------*
      *--- AREA DE ENTRADA DO ROTEADOR --> TAM. 0106 + 0003 = 0109  ---*
      *
       01          ENTRADA-ROT4001.
         05        ENT4001-COD-RETORNO     PIC X(004)      VALUE SPACES.
         05        ENT4001-MSG-RETORNO     PIC X(079)      VALUE SPACES.
         05        ENT4001-RESTART         PIC 9(005)      VALUE ZEROS.
         05        ENT4001-FLAG            PIC X(001)      VALUE SPACES.
         05        ENT4001-CFUNC-BDSCO     PIC 9(009)      VALUE ZEROS.
         05        ENT4001-CTERM           PIC X(008)      VALUE SPACES.
         05        ENT4001-CPRODT          PIC 9(003)      VALUE ZEROS.
         05        ENT4001-DT-MOVTO        PIC X(010)      VALUE SPACES.
      *
      *--- AREA DE SAIDA   DO ROTEADOR --> TAM. 0089 + 0107 = 0196  ---*
      *
       01          SAIDA-ROT4001.
         05        SAI4001-HEADER.
            10     SAI4001-COD-RETORNO     PIC X(004)      VALUE SPACES.
            10     SAI4001-MSG-RETORNO     PIC X(079)      VALUE SPACES.
            10     SAI4001-RESTART         PIC 9(005)      VALUE ZEROS.
            10     SAI4001-FLAG            PIC X(001)      VALUE SPACES.
         05        SAI4001-DADOS.
               10  SAI4001-TABELA-SAIDA.
               15  SAI4001-CPRODT          PIC 9(003)      VALUE ZEROS.
               15  SAI4001-IPRODT          PIC X(060)      VALUE SPACES.
               15  SAI4001-IABREV-PRODT    PIC X(012)      VALUE SPACES.
               15  SAI4001-CSGL-PRODT-DESC PIC X(006)      VALUE SPACES.
               15  SAI4001-CIDTFD-TX-MIN-JURO
                                           PIC 9(005)      VALUE ZEROS.
               15  SAI4001-CIDTFD-TX-MIN-VLR
                                           PIC 9(015)      VALUE ZEROS.
               15  SAI4001-CIDTFD-TX-MIN-DEC
                                           PIC 9(003)      VALUE ZEROS.
               15  SAI4001-CIDTFD-TX-MAX-JURO
                                           PIC 9(005)      VALUE ZEROS.
               15  SAI4001-CIDTFD-TX-MAX-VLR
                                           PIC 9(015)      VALUE ZEROS.
               15  SAI4001-CIDTFD-TX-MAX-DEC
                                           PIC 9(003)      VALUE ZEROS.
               15  SAI4001-HULT-ATULZ      PIC X(022).
         05        SAI4001-CONSISTENCIA    REDEFINES SAI4001-DADOS.
            10     SAI4001-TAMANHO-ERRO    PIC 9(001).
            10     SAI4001-TABELA-ERROS OCCURS  002  TIMES.
               15  SAI4001-CAMPOS-ERRO     PIC 9(001).
      *
      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(50)      VALUE
           '*** AREA DE COMUNICACAO DCOM4049 ***'.
      *
      *----------------------------------------------------------------*
       01          ENTRADA-ROT4049.
         05        ENT4049-COD-RETORNO     PIC X(04)       VALUE SPACES.
         05        ENT4049-MSG-RETORNO     PIC X(79)       VALUE SPACES.
         05        ENT4049-RESTART         PIC 9(05)       VALUE ZEROS.
         05        ENT4049-FLAG            PIC X(01)       VALUE SPACES.
         05        ENT4049-CFUNC-BDSCO     PIC 9(09)       VALUE ZEROS.
         05        ENT4049-CTERM           PIC X(08)       VALUE SPACES.
         05        ENT4049-CPRODT          PIC 9(03)       VALUE ZEROS.
         05        ENT4049-CSPROD-DESC-COML
                                           PIC 9(03)       VALUE ZEROS.
      *
       01          SAIDA-ROT4049.
         05        SAI4049-HEADER.
            10     SAI4049-COD-RETORNO     PIC X(04)       VALUE SPACES.
            10     SAI4049-MSG-RETORNO     PIC X(79)       VALUE SPACES.
            10     SAI4049-RESTART         PIC 9(05)       VALUE ZEROS.
            10     SAI4049-FLAG            PIC X(01)       VALUE SPACES.
         05        SAI4049-DADOS.
            10     SAI4049-TABELA-SAIDA.
               15  SAI4049-CPRODT          PIC 9(003)      VALUE ZEROS.
               15  SAI4049-IPRODT          PIC X(060)      VALUE SPACES.
               15  SAI4049-CSPROD-DESC-COML
                                           PIC 9(003)      VALUE ZEROS.
               15  SAI4049-ID-DUPLICATA    PIC X(001)      VALUE SPACES.
               15  SAI4049-DINIC-VGCIA-SPROD
                                           PIC X(010)      VALUE SPACES.
               15  SAI4049-DFIM-VGCIA-SPROD
                                           PIC X(010)      VALUE SPACES.
               15  SAI4049-CTPO-DESC-COML  PIC 9(003)      VALUE ZEROS.
               15  SAI4049-IRSUMO-TPO-DESC PIC X(010)      VALUE SPACES.
               15  SAI4049-ISPROD-DESC-COML
                                           PIC X(040)      VALUE SPACES.
               15  SAI4049-IRSUMO-SPROD-DESC
                                           PIC X(010)      VALUE SPACES.
               15  SAI4049-RSPROD-DESC-COML
                                           PIC X(240)      VALUE SPACES.
               15  SAI4049-CSGL-SPROD-DESC PIC X(006)      VALUE SPACES.
               15  SAI4049-CSIT-DESC-COML  PIC 9(003)      VALUE ZEROS.
               15  SAI4049-HSIT-DESC-COML  PIC X(022)      VALUE SPACES.
               15  SAI4049-IRSUMO-SIT-DESC PIC X(015)      VALUE SPACES.
               15  SAI4049-CFUNC-BDSCO-ALT PIC 9(009)      VALUE ZEROS.
               15  SAI4049-NFUNC-BDSCO-ALT PIC X(040)      VALUE SPACES.
               15  SAI4049-CFUNC-BDSCO-INC PIC 9(009)      VALUE ZEROS.
               15  SAI4049-NFUNC-BDSCO-INC PIC X(040)      VALUE SPACES.
               15  SAI4049-HULT-ATULZ-ALT  PIC X(022)      VALUE SPACES.
               15  SAI4049-HULT-ATULZ-INC  PIC X(022)      VALUE SPACES.
               15  SAI4049-HULT-ATULZ      PIC X(022)      VALUE SPACES.
               15  SAI4049-CFUNC-BDSCO     PIC 9(009)      VALUE ZEROS.
               15  SAI4049-NFUNC-BDSCO     PIC X(040)      VALUE SPACES.
      *
         05        SAI4049-CONSISTENCIA    REDEFINES SAI4049-DADOS.
            10     SAI4049-TAMANHO-ERRO    PIC 9(01).
            10     SAI4049-TABELA-ERROS    OCCURS  2  TIMES.
               15  SAI4049-CAMPOS-ERRO     PIC 9(01).
      *
      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(50)      VALUE
           '*** AREA DE COMUNICACAO DCOM4309 ***'.
      *
      *----------------------------------------------------------------*

      *--- AREA DE ENTRADA DO ROTEADOR --> TAM. 0106 BYTES       ---*
      *
       01          ENTRADA-ROT4309.
         05        ENT4309-COD-RETORNO     PIC X(04)       VALUE SPACES.
         05        ENT4309-MSG-RETORNO     PIC X(79)       VALUE SPACES.
         05        ENT4309-RESTART         PIC 9(05)       VALUE ZEROS.
         05        ENT4309-FLAG            PIC X(01)       VALUE SPACES.
         05        ENT4309-CFUNC-BDSCO     PIC 9(09)       VALUE ZEROS.
         05        ENT4309-CTERM           PIC X(08)       VALUE SPACES.
         05        ENT4309-DADOS.
            10     ENT4309-CSIT-DESC-COML  PIC 9(03)       VALUE ZEROS.
            10     ENT4309-CELMTO-DESC-COML
                                           PIC 9(03)       VALUE ZEROS.
      *
      *--- AREA DE SAIDA   DO ROTEADOR --> TAM. 143 BYTES          ---*
      *
       01          SAIDA-ROT4309.
         05        SAI4309-COD-RETORNO     PIC X(04)       VALUE SPACES.
         05        SAI4309-MSG-RETORNO     PIC X(79)       VALUE SPACES.
         05        SAI4309-RESTART         PIC 9(05)       VALUE ZEROS.
         05        SAI4309-FLAG            PIC X(01)       VALUE SPACES.
         05        SAI4309-DADOS.
            10     SAI4309-CSIT-DESC-COML  PIC 9(03)       VALUE ZEROS.
            10     SAI4309-IRSUMO-SIT-DESC PIC X(15)       VALUE SPACES.
            10     SAI4309-ISIT-DESC-COML  PIC X(30)       VALUE SPACES.
            10     SAI4309-CSGL-SIT-DESC   PIC X(06)       VALUE SPACES.
      *
      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(50)      VALUE
           '*** AREA DE COMUNICACAO DCOM4200 ***'.
      *
      *----------------------------------------------------------------*
       01          ENTRADA-ROT4200.
         05        ENT4200-COD-RETORNO     PIC X(004)      VALUE SPACES.
         05        ENT4200-MSG-RETORNO     PIC X(079)      VALUE SPACES.
         05        ENT4200-RESTART         PIC 9(005)      VALUE ZEROS.
         05        ENT4200-FLAG            PIC X(001)      VALUE SPACES.
         05        ENT4200-CFUNC-BDSCO     PIC 9(009)      VALUE ZEROS.
         05        ENT4200-CTERM           PIC X(008)      VALUE SPACES.
         05        ENT4200-CCONVE-GRAL-DESC
                                           PIC 9(009)      VALUE ZEROS.
         05        ENT4200-CVRSAO-CONVE-GRAL
                                           PIC 9(003)      VALUE ZEROS.
         05        ENT4200-FLAG-FUNCAO     PIC X(001)      VALUE SPACES.
      *
       01          SAIDA-ROT4200.
         05        SAI4200-HEADER.
            10     SAI4200-COD-RETORNO     PIC X(004)      VALUE SPACES.
            10     SAI4200-MSG-RETORNO     PIC X(079)      VALUE SPACES.
            10     SAI4200-RESTART         PIC 9(005)      VALUE ZEROS.
            10     SAI4200-FLAG            PIC X(001)      VALUE SPACES.
         05        SAI4200-DADOS.
            10     SAI4200-CCONVE-GRAL-DESC
                                           PIC 9(009)      VALUE ZEROS.
            10     SAI4200-CVRSAO-CONVE-GRAL
                                           PIC 9(003)      VALUE ZEROS.
            10     SAI4200-ICONVE-GRAL-DESC
                                           PIC X(040)      VALUE SPACES.
            10     SAI4200-IRSUMO-CONVE-GRAL
                                           PIC X(015)      VALUE SPACES.
            10     SAI4200-CPRODT          PIC 9(003)      VALUE ZEROS.
            10     SAI4200-IPRODT          PIC X(060)      VALUE SPACES.
            10     SAI4200-CSPROD-DESC-COML
                                           PIC 9(003)      VALUE ZEROS.
            10     SAI4200-ISPROD-DESC-COML
                                           PIC X(040)      VALUE SPACES.
            10     SAI4200-CBCO-OPER-CONVE PIC 9(003)      VALUE ZEROS.
            10     SAI4200-IBCO-OPER-CONVE PIC X(010)      VALUE SPACES.
            10     SAI4200-DINCL-CONVE-GRAL
                                           PIC X(010)      VALUE SPACES.
            10     SAI4200-DINIC-VGCIA-CONVE
                                           PIC X(010)      VALUE SPACES.
            10     SAI4200-DVCTO-CONVE-DESC
                                           PIC X(010)      VALUE SPACES.
            10     SAI4200-VLIM-CONVE-GRAL PIC 9(015)V9(2) VALUE ZEROS.
            10     SAI4200-CTPO-DEPDC-EXCVD
                                           PIC X(001)      VALUE SPACES.
            10     SAI4200-FLAG-NAVEGACAO-AUX
                                           PIC X(003)      VALUE SPACES.
      *
      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(50)      VALUE
           '*** AREA DE COMUNICACAO DCOM3903 ***'.
      *----------------------------------------------------------------*
       01  COMU-AREA-3903.
           05  COMU-LL-3903                PIC S9(04) COMP VALUE +273.
           05  COMU-ZZ-3903                PIC S9(04) COMP VALUE ZEROS.
           05  COMU-TRANCODE-3903.
               10  COMU-TRANSACAO-3903     PIC  X(008)     VALUE SPACES.
               10  FILLER                  PIC  X(006)     VALUE SPACES.
               10  COMU-PFK-3903           PIC  X(001)     VALUE SPACES.
           05  COMU-SENHAS-3903            PIC  X(037)     VALUE SPACES.
           05  COMU-COMANDO-3903           PIC  X(068)     VALUE SPACES.
           05  COMU-DADOS-3903.
               10  COMU-OPCAO-3903         PIC  9(001)     VALUE ZEROS.
               10  COMU-TIMESTA-3903       PIC  X(026)     VALUE SPACES.
               10  COMU-COD-PROD-X-3903.
                   15  COMU-COD-PROD-3903  PIC  9(003)     VALUE ZEROS.
               10  COMU-DES-PROD-3903      PIC  X(040)     VALUE SPACES.
               10  COMU-MENSA-3903         PIC  X(079)     VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(50)      VALUE
           '*** AREA DE COMUNICACAO DCOM3904 ***'.
      *----------------------------------------------------------------*
       01  COMU-AREA-3904.
           05  COMU-LL-3904                PIC S9(04) COMP VALUE +293.
           05  COMU-ZZ-3904                PIC S9(04) COMP VALUE ZEROS.
           05  COMU-TRANCODE-3904.
               10  COMU-TRANSACAO-3904     PIC  X(008)     VALUE SPACES.
               10  FILLER                  PIC  X(006)     VALUE SPACES.
               10  COMU-PFK-3904           PIC  X(001)     VALUE SPACES.
           05  COMU-SENHAS-3904            PIC  X(037)     VALUE SPACES.
           05  COMU-COMANDO-3904           PIC  X(068)     VALUE SPACES.
           05  COMU-DADOS-3904.
               10  COMU-OPCAO-3904         PIC  9(001)     VALUE ZEROS.
               10  COMU-TIMESTA-3904       PIC  X(026)     VALUE SPACES.
               10  COMU-CNPJCPF-3904       PIC  X(017)     VALUE SPACES.
               10  COMU-COD-PROD-X-3904.
                   15  COMU-COD-PROD-3904  PIC  9(003)     VALUE ZEROS.
               10  COMU-COD-SPROD-X-3904.
                   15  COMU-COD-SPROD-3904 PIC  9(003)     VALUE ZEROS.
               10  COMU-DES-SPROD-3904     PIC  X(040)     VALUE SPACES.
               10  COMU-MENSA-3904         PIC  X(079)     VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(50)      VALUE
           '*** AREA DE COMUNICACAO DCOM3913 ***'.
      *----------------------------------------------------------------*
       01  COMU-AREA-3913.
           05  COMU-LL-3913                PIC S9(04) COMP VALUE +180.
           05  COMU-ZZ-3913                PIC S9(04) COMP VALUE ZEROS.
           05  COMU-TRANCODE-3913.
               10  COMU-TRANSACAO-3913     PIC  X(008)     VALUE SPACES.
               10  FILLER                  PIC  X(006)     VALUE SPACES.
               10  COMU-PFK-3913           PIC  X(001)     VALUE SPACES.
           05  COMU-SENHAS-3913            PIC  X(037)     VALUE SPACES.
           05  COMU-COMANDO-3913           PIC  X(068)     VALUE SPACES.
           05  COMU-DADOS-3913.
               10  COMU-TIMESTA-3913       PIC  X(026)     VALUE SPACES.
               10  COMU-TPOPCAO-3913       PIC  X(001)     VALUE SPACES.
               10  COMU-CELMTO-DESC-3913   PIC  9(003)     VALUE ZEROS.
               10  COMU-CSIT-DESC-3913     PIC  9(003)     VALUE ZEROS.
               10  COMU-COD-SITUA-3913     PIC  9(003)     VALUE ZEROS.
               10  COMU-DES-SITUA-3913     PIC  X(020)     VALUE SPACES.
      *
      *---------------------------------------------------------------*
       01  FILLER                          PIC  X(050)     VALUE
           '*** AREAS AUXILIARES PARA POOL5005 ***'.
      *----------------------------------------------------------------*
       01  WRK-POOL5005.
           05  WRK-SQLCA                   PIC  X(136)     VALUE SPACES.
           05  WRK-GU                      PIC  X(004)     VALUE 'GU  '.
           05  WRK-CHNG                    PIC  X(004)     VALUE 'CHNG'.
           05  WRK-ISRT                    PIC  X(004)     VALUE 'ISRT'.
           05  WRK-FUNCAO                  PIC  X(004)     VALUE SPACES.
           05  WRK-TELA                    PIC  X(008)     VALUE SPACES.
           05  WRK-VERSAO                  PIC  X(006)     VALUE
                                                           'VRS002'.
           05  WRK-COD-USER                PIC  X(007)     VALUE SPACES.
           05  WRK-COD-USER-R              REDEFINES
               WRK-COD-USER                PIC  9(007).
           05  WRK-COD-DEPTO.
               10  FILLER                  PIC  X(002)     VALUE SPACES.
               10  WRK-COD-DEPTO-N         PIC  9(004)     VALUE ZEROS.

           05  WRK-MENSAGEM.
               10  FILLER                  PIC  X(004)     VALUE SPACES.
               10  WRK-TRANSACAO           PIC  X(008)     VALUE SPACES.
               10  FILLER                  PIC  X(1988)    VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(050)     VALUE
           '*** AREA DA TELA DCOMFJTM  - BRAD0660 ***'.
      *----------------------------------------------------------------*
       01  WRK-660-DCOMFJTM.
           05  WRK-660-LL-AREA-FJTM        PIC  9(04) COMP VALUE 0042.
           05  WRK-660-LL-MENSAGEM-FJTM    PIC  9(04) COMP VALUE 0348.
           05  WRK-660-SENHAS-FJTM         PIC  9(04) COMP VALUE 0037.
           05  WRK-660-COMANDO-FJTM        PIC  9(04) COMP VALUE 2070.
           05  WRK-660-TIMESTA-FJTM        PIC  9(04) COMP VALUE 0026.
           05  WRK-660-FASE-FJTM           PIC  9(04) COMP VALUE 0001.
           05  WRK-660-CCONTR-FJTM         PIC  9(04) COMP VALUE 2011.
           05  WRK-660-PROD-FJTM           PIC  9(04) COMP VALUE 2005.
           05  WRK-660-DPROD-FJTM          PIC  9(04) COMP VALUE 0012.
           05  WRK-660-SPROD-FJTM          PIC  9(04) COMP VALUE 2005.
           05  WRK-660-DSPROD-FJTM         PIC  9(04) COMP VALUE 0010.
           05  WRK-660-CAGEN-FJTM          PIC  9(04) COMP VALUE 2007.
           05  WRK-660-NAGEN-FJTM          PIC  9(04) COMP VALUE 0040.
           05  WRK-660-SITUA-FJTM          PIC  9(04) COMP VALUE 2005.
           05  WRK-660-DSITUA-FJTM         PIC  9(04) COMP VALUE 0015.
           05  WRK-660-MENSA-FJTM          PIC  9(04) COMP VALUE 0079.
      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(050)     VALUE
           '*** AREA DE ERRO DA BRAD0660 ***'.
      *----------------------------------------------------------------*
       01  WRK-ERRO-BRAD0660.
           05  FILLER                      PIC  X(045)     VALUE
               '* RETURN-CODE DIFERENTE DE ZEROS NA BRAD0660'.
           05  FILLER                      PIC  X(012)     VALUE
               '- RET.COD ='.
           05  WRK-RETURN-CODE             PIC  9(002)     VALUE ZEROS.
           05  FILLER                      PIC  X(011)     VALUE
               ' - LOCAL ='.
           05  WRK-LOCAL-ERRO              PIC  9(002)     VALUE ZEROS.
           05  FILLER                      PIC  X(002)     VALUE ' *'.

      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(050)     VALUE
           '*** AREA DA TELA DCOMFJTM - INPUT ***'.
      *----------------------------------------------------------------*
       01  WRK-INPUT-FJTM.
           05  FILLER                      PIC  X(018)     VALUE SPACES.
           05  WRK-INP-PFK-FJTM            PIC  X(001)     VALUE SPACES.
           05  WRK-INP-SENHAS-FJTM         PIC  X(037)     VALUE SPACES.
           05  WRK-INP-COMANDO-FJTM        PIC  X(068)     VALUE SPACES.
           05  WRK-INP-TIMESTA-FJTM        PIC  X(026)     VALUE SPACES.
           05  WRK-INP-FASE-FJTM           PIC  X(001)     VALUE SPACES.
           05  WRK-INP-DADOS-FJTM.
               10  WRK-INP-CCONTR-FJTM.
                   15  WRK-INP-CCONTR-FJTM-N
                                           PIC  9(009)     VALUE ZEROS.
      *
               10  WRK-INP-PROD-FJTM.
                   15  WRK-INP-PROD-FJTM-N
                                           PIC  9(003)     VALUE ZEROS.
               10  WRK-INP-DPROD-FJTM.
                   15  WRK-INP-DPROD-FJTM-N
                                           PIC  X(012)     VALUE ZEROS.
               10  WRK-INP-SPROD-FJTM.
                   15  WRK-INP-SPROD-FJTM-N
                                           PIC  9(003)     VALUE ZEROS.
               10  WRK-INP-DSPROD-FJTM.
                   15  WRK-INP-DSPROD-FJTM-N
                                           PIC  X(010)     VALUE ZEROS.
               10  WRK-INP-CAGEN-FJTM.
                   15  WRK-INP-CAGEN-FJTM-N
                                           PIC  9(005)     VALUE ZEROS.
               10  WRK-INP-NAGEN-FJTM.
                   15  WRK-INP-NAGEN-FJTM-N
                                           PIC  X(040)     VALUE ZEROS.
               10  WRK-INP-SITUA-FJTM.
                   15  WRK-INP-SITUA-FJTM-N
                                           PIC  9(003)     VALUE ZEROS.
               10  WRK-INP-DSITUA-FJTM.
                   15  WRK-INP-DSITUA-FJTM-N
                                           PIC  X(015)     VALUE ZEROS.
           05  WRK-INP-MENSA-FJTM          PIC  X(079)     VALUE SPACES.
           05  WRK-INP-POSI-FJTM.
               10  WRK-INP-LINHA-FJTM     PIC S9(004) COMP VALUE ZEROS.
               10  WRK-INP-COLUNA-FJTM    PIC S9(004) COMP VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(050)     VALUE
           '*** AREA DA TELA DCOMFJTM  - OUTPUT ***'.
      *----------------------------------------------------------------*
       01  WRK-OUTPUT-FJTM.
           05  WRK-OUT-LL-FJTM             PIC S9(04) COMP VALUE +0348.
           05  WRK-OUT-ZZ-FJTM             PIC  9(04) COMP VALUE ZEROS.
           05  WRK-OUT-SENHAS-FJTM         PIC  X(37)      VALUE SPACES.
           05  WRK-OUT-COMANDO-ATTR-FJTM   PIC S9(04) COMP VALUE ZEROS.
           05  WRK-OUT-COMANDO-FJTM        PIC  X(68)      VALUE SPACES.
           05  WRK-OUT-TIMESTA-FJTM        PIC  X(26)      VALUE SPACES.
           05  WRK-OUT-FASE-FJTM           PIC  X(01)      VALUE SPACES.
           05  WRK-OUT-DADOS-FJTM.
               10  WRK-OUT-CCONV-ATTR-FJTM PIC S9(04) COMP VALUE ZEROS.
               10  WRK-OUT-CCONTR-FJTM.
                   15  WRK-OUT-CCONTR-FJTM-N
                                           PIC  9(009)     VALUE ZEROS.
      *
               10  WRK-OUT-PROD-ATTR-FJTM PIC S9(04) COMP VALUE ZEROS.
               10  WRK-OUT-PROD-FJTM.
                   15  WRK-OUT-PROD-FJTM-N
                                           PIC  9(003)     VALUE ZEROS.
               10  WRK-OUT-DPROD-FJTM      PIC  X(012)     VALUE SPACES.
      *
               10  WRK-OUT-CSPROD-ATTR-FJTM PIC S9(04) COMP VALUE ZEROS.
               10  WRK-OUT-SPROD-FJTM.
                   15  WRK-OUT-SPROD-FJTM-N
                                           PIC  9(003)     VALUE ZEROS.
               10  WRK-OUT-DSPROD-FJTM     PIC  X(010)     VALUE SPACES.
      *
               10  WRK-OUT-CAGEN-ATTR-FJTM PIC S9(04) COMP VALUE ZEROS.
               10  WRK-OUT-CAGEN-FJTM.
                   15  WRK-OUT-CAGEN-FJTM-N
                                           PIC  9(005)     VALUE ZEROS.
               10  WRK-OUT-NAGEN-FJTM      PIC  X(040)     VALUE SPACES.
      *
               10  WRK-OUT-SITUA-ATTR-FJTM PIC S9(04) COMP VALUE ZEROS.
               10  WRK-OUT-SITUA-FJTM.
                   15  WRK-OUT-SITUA-FJTM-N
                                           PIC  9(003)     VALUE ZEROS.
               10  WRK-OUT-DSITUA-FJTM     PIC  X(015)     VALUE SPACES.
      *
           05  WRK-OUT-MENSA-FJTM          PIC  X(079)     VALUE SPACES.
      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(050)     VALUE
           '*** AREA DE COMUNICACAO PFK ***'.
      *----------------------------------------------------------------*
       01  COMU-AREA-PFK.
           05  COMU-LL-PFK                 PIC S9(04) COMP VALUE +124.
           05  COMU-ZZ-PFK                 PIC S9(04) COMP VALUE ZEROS.
           05  COMU-TRANCODE-PFK.
               10  COMU-TRANSACAO-PFK      PIC  X(008)     VALUE SPACES.
               10  FILLER                  PIC  X(006)     VALUE SPACES.
               10  COMU-PFK-PFK            PIC  X(001)     VALUE SPACES.
           05  COMU-SENHAS-PFK             PIC  X(037)     VALUE SPACES.
           05  COMU-COMANDO-PFK            PIC  X(068)     VALUE SPACES.

      *----------------------------------------------------------------*
      *        *** AREA DE COMUNICACAO DO MODULO DCOM7995 - CHAMA      *
      *----------------------------------------------------------------*

       01  WRK-ENTRADA-DCOM7995.
           05  WRK-ENT-OPCAO           PIC  X(0002)    VALUE SPACES.
           05  WRK-ENT-COD-FUNC        PIC  9(0009)    VALUE ZEROS.
           05  WRK-ENT-COD-TERMINAL    PIC  X(0008)    VALUE SPACES.
           05  WRK-ENT-TIMESTA         PIC  X(0026)    VALUE SPACES.
           05  WRK-ENT-TRANS           PIC  X(0008)    VALUE SPACES.
           05  WRK-ENT-TRANS-ANT       PIC  X(0008)    VALUE SPACES.
           05  WRK-ENT-PAGINA          PIC  9(0003)    VALUE ZEROS.
           05  WRK-ENT-DADOS           PIC  X(1600)    VALUE SPACES.

       01  WRK-SAIDA-DCOM7995.
           05  WRK-SAI-COD-RETORNO     PIC  9(0002)    VALUE ZEROS.
           05  WRK-SAI-MSG-RET7995     PIC  X(0079)    VALUE SPACES.
           05  WRK-SAI-TIMESTA         PIC  X(0026)    VALUE SPACES.
           05  WRK-SAI-TRANS           PIC  X(0008)    VALUE SPACES.
           05  WRK-SAI-TRANS-ANT       PIC  X(0008)    VALUE SPACES.
           05  WRK-SAI-PAGINA          PIC  9(0003)    VALUE ZEROS.
           05  WRK-SAI-DADOS           PIC  X(1600)    VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(050)     VALUE
           '*** AREA PARA MENSAGENS ***'.
      *----------------------------------------------------------------*

       COPY 'I#DCOMSY'.

      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(079)     VALUE
           '*** AREA PARA COMUNICACAO PADRAO ***'.
      *----------------------------------------------------------------*

       COPY 'I#DCOMJ2'.

      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(050)     VALUE
           '*** AREA DA BRAD7100 ***'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(050)     VALUE
           '*** DCOM0610 - FIM DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                             SECTION.
      *----------------------------------------------------------------*

       01 LNK-IO-PCB.
           05 LNK-IO-LTERM                 PIC  X(008).
           05 FILLER                       PIC  X(002).
           05 LNK-IO-STATUS                PIC  X(002).
           05 FILLER                       PIC  X(012).
           05 LNK-IO-MODNAME               PIC  X(008).

       01 LNK-ALT-PCB.
           05 LNK-ALT-LTERM                PIC  X(008).
           05 FILLER                       PIC  X(002).
           05 LNK-ALT-STATUS               PIC  X(002).
           05 FILLER                       PIC  X(012).
           05 LNK-ALT-MODNAME              PIC  X(008).

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

           ENTRY 'DLITCBL'             USING  LNK-IO-PCB
                                              LNK-ALT-PCB.

           MOVE WRK-GU                     TO WRK-FUNCAO.

      *----------------------------------------------------------------*
      *     PROCEDIMENTOS INICIAIS                                     *
      *----------------------------------------------------------------*
       0000-INICIAR                        SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL5005'             USING  WRK-VERSAO
                                              WRK-FUNCAO
                                              LNK-IO-PCB
                                              LNK-ALT-PCB
                                              WRK-MENSAGEM
                                              WRK-TELA
                                              WRK-COD-USER
                                              WRK-COD-DEPTO.

           IF  RETURN-CODE                 EQUAL 04 OR 12
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE SPACES                     TO WRK-OUT-DADOS-FJTM
                                              WRK-OUT-SENHAS-FJTM
                                              WRK-OUT-COMANDO-FJTM.

           PERFORM 2000-PROCESSAR-MENSAGEM.

           GO TO 0000-INICIAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                        EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *     PROCESSAR MENSAGEM                                         *
      *----------------------------------------------------------------*
       2000-PROCESSAR-MENSAGEM             SECTION.
      *----------------------------------------------------------------*

           MOVE 'DCOMFJTM'                 TO WRK-TELA.

           IF  WRK-TRANSACAO           NOT EQUAL 'DCOM0610'
               PERFORM 2100-RECEBER-CONTROLE
           ELSE
               PERFORM 2200-PROCESSAR-DCOMFJTM
           END-IF.

           IF  WRK-TELA                    EQUAL 'DCOMFJTM'
               PERFORM 2300-PROCESSAR-BRAD0660
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     RECEBER CONTROLE                                           *
      *----------------------------------------------------------------*
       2100-RECEBER-CONTROLE               SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM               TO COMU-AREA

           IF   WRK-TRANSACAO              EQUAL 'DCOM3903'
                MOVE WRK-MENSAGEM          TO COMU-AREA-3903
                MOVE COMU-TIMESTA-3903     TO COMU-TIMESTAMP
           END-IF

           IF   WRK-TRANSACAO              EQUAL 'DCOM3904'
                MOVE WRK-MENSAGEM          TO COMU-AREA-3904
                MOVE COMU-TIMESTA-3904     TO COMU-TIMESTAMP
           END-IF

           IF   WRK-TRANSACAO              EQUAL 'DCOM3913'
                MOVE WRK-MENSAGEM          TO COMU-AREA-3913
                MOVE COMU-TIMESTA-3913     TO COMU-TIMESTAMP
           END-IF

           MOVE COMU-TIMESTAMP             TO WRK-INP-TIMESTA-FJTM
                                              WRK-OUT-TIMESTA-FJTM
                                              WRK-ENT-TIMESTA.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE 'CP'                       TO WRK-ENT-OPCAO

           PERFORM 8200-ACESSAR-DCOM7995

           MOVE WRK-SAI-DADOS              TO WRK-AREA-COMUN-TOTAL


           IF  WRK-TRANSACAO               EQUAL 'DCOM3903' OR
               WRK-TRANSACAO               EQUAL 'DCOM3904' OR
               WRK-TRANSACAO               EQUAL 'DCOM3913'
               IF  WRK-CONTRATO                GREATER ZEROS
                   MOVE WRK-CONTRATO           TO WRK-OUT-CCONTR-FJTM-N
               END-IF
               IF  WRK-AGENCIA                 GREATER ZEROS
                   MOVE WRK-AGENCIA            TO WRK-OUT-CAGEN-FJTM
                   MOVE WRK-DESC-AGENCIA       TO WRK-OUT-NAGEN-FJTM
               END-IF
               IF  WRK-PRODUTO-LISTA           GREATER ZEROS
                   MOVE WRK-PRODUTO-LISTA      TO WRK-OUT-PROD-FJTM
               END-IF
               IF  WRK-SUBPRODUTO-LISTA        GREATER ZEROS
                   MOVE WRK-SUBPRODUTO-LISTA   TO WRK-OUT-SPROD-FJTM
               END-IF
               IF  WRK-SITUACAO-LISTA          GREATER ZEROS
                   MOVE WRK-SITUACAO-LISTA     TO WRK-OUT-SITUA-FJTM
               END-IF
           END-IF.

           MOVE 'DIGITE OS DADOS E TECLE <ENTER>'
                                           TO WRK-OUT-MENSA-FJTM.

           IF   WRK-TRANSACAO                EQUAL 'DCOM3903'
                MOVE WRK-MENSAGEM            TO COMU-AREA-3903
                IF   COMU-COD-PROD-3903      GREATER ZEROS
                     MOVE COMU-COD-PROD-3903 TO WRK-OUT-PROD-FJTM
                     MOVE COMU-DES-PROD-3903 TO WRK-OUT-DPROD-FJTM
                ELSE
                     MOVE SPACES             TO WRK-OUT-PROD-FJTM
                                                WRK-OUT-DPROD-FJTM
                END-IF
                MOVE COMU-MENSA-3903         TO WRK-OUT-MENSA-FJTM
           END-IF

           IF   WRK-TRANSACAO                EQUAL 'DCOM3904'
                MOVE WRK-MENSAGEM            TO COMU-AREA-3904
                IF   COMU-COD-SPROD-3904     GREATER ZEROS
                     MOVE COMU-COD-SPROD-3904 TO WRK-OUT-SPROD-FJTM
                     MOVE COMU-DES-SPROD-3904 TO WRK-OUT-DSPROD-FJTM
                ELSE
                     MOVE SPACES             TO WRK-OUT-SPROD-FJTM
                                                WRK-OUT-DSPROD-FJTM
                END-IF
                MOVE COMU-MENSA-3904         TO WRK-OUT-MENSA-FJTM
           END-IF

           IF   WRK-TRANSACAO                 EQUAL 'DCOM3913'
                MOVE WRK-MENSAGEM             TO COMU-AREA-3913
                IF   COMU-COD-SITUA-3913      GREATER ZEROS
                     MOVE COMU-COD-SITUA-3913 TO WRK-OUT-SITUA-FJTM
                     MOVE COMU-DES-SITUA-3913 TO WRK-OUT-DSITUA-FJTM
                ELSE
                     MOVE SPACES              TO WRK-OUT-SITUA-FJTM
                                                 WRK-OUT-DSITUA-FJTM
                END-IF
           END-IF

           IF   WRK-TRANSACAO               EQUAL 'DCOM0611' AND
                WRK-MENSAGEM-COMUM      NOT EQUAL SPACES
                MOVE WRK-MENSAGEM-COMUM     TO WRK-OUT-MENSA-FJTM
           END-IF.

           PERFORM VARYING IND-1               FROM 1 BY 1
                     UNTIL IND-1               GREATER 15
                        OR WRK-CHAMADOR(IND-1) EQUAL SPACES
               IF WRK-CHAMADOR(IND-1)          EQUAL 'DCOM0610'
                  MOVE SPACES                  TO WRK-CHAMADOR(IND-1)
                  MOVE ZEROS                   TO WRK-PAGINA(IND-1)
                  MOVE 99                      TO IND-1
               END-IF
           END-PERFORM.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE WRK-INP-TIMESTA-FJTM       TO WRK-ENT-TIMESTA
           MOVE 'I '                       TO WRK-ENT-OPCAO
           MOVE WRK-AREA-COMUN-TOTAL       TO WRK-ENT-DADOS

           PERFORM 8200-ACESSAR-DCOM7995.

      *----------------------------------------------------------------*
       2100-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       8200-ACESSAR-DCOM7995           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-COD-USER-R         TO WRK-ENT-COD-FUNC
           MOVE LNK-IO-LTERM           TO WRK-ENT-COD-TERMINAL
           MOVE 'DCOM0601'             TO WRK-ENT-TRANS

           MOVE 'DCOM7995'             TO WRK-MODULO.

           CALL WRK-MODULO          USING WRK-ENTRADA-DCOM7995
                                          WRK-SAIDA-DCOM7995
                                          ERRO-AREA
                                          WRK-SQLCA.

           IF  WRK-SAI-COD-RETORNO     EQUAL 01
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-SAI-MSG-RET7995
                                       TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           ELSE
               IF  WRK-SAI-COD-RETORNO EQUAL 99
                   MOVE 'DB2'          TO ERR-TIPO-ACESSO
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       8200-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     PROCESSAR DCOMFJTM                                         *
      *----------------------------------------------------------------*
       2200-PROCESSAR-DCOMFJTM             SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM               TO WRK-INPUT-FJTM.

           PERFORM 2210-DEVOLVER-TELA

           MOVE SPACES                     TO WRK-OUT-DPROD-FJTM
                                              WRK-OUT-DSPROD-FJTM
                                              WRK-OUT-NAGEN-FJTM
                                              WRK-OUT-DSITUA-FJTM.

           EVALUATE TRUE

               WHEN WRK-INP-PFK-FJTM       EQUAL '1' OR
                   (WRK-INP-PFK-FJTM       EQUAL '.' AND
                    WRK-INP-COMANDO-FJTM   EQUAL 'PFK01')
                  IF  WRK-INP-FASE-FJTM    EQUAL '2'
                      MOVE WRK-49345       TO  WRK-OUT-COMANDO-ATTR-FJTM
                      MOVE WRK-241         TO  WRK-OUT-CCONV-ATTR-FJTM
                                               WRK-OUT-CAGEN-ATTR-FJTM
                                               WRK-OUT-PROD-ATTR-FJTM
                                               WRK-OUT-CSPROD-ATTR-FJTM
                                               WRK-OUT-SITUA-ATTR-FJTM
                  END-IF
                  MOVE WRK-INP-MENSA-FJTM TO WRK-OUT-MENSA-FJTM

               WHEN WRK-INP-PFK-FJTM       EQUAL '3' OR
                   (WRK-INP-PFK-FJTM       EQUAL '.' AND
                    WRK-INP-COMANDO-FJTM   EQUAL 'PFK03')
                  PERFORM 2240-RETORNAR-CHAMADOR

               WHEN WRK-INP-PFK-FJTM       EQUAL 'X' OR
                   (WRK-INP-PFK-FJTM       EQUAL '.' AND
                    WRK-INP-COMANDO-FJTM   EQUAL 'PFK05')
                  PERFORM 2250-MENU-ROTINAS

               WHEN WRK-INP-PFK-FJTM       EQUAL 'Z' OR
                   (WRK-INP-PFK-FJTM       EQUAL '.' AND
                    WRK-INP-COMANDO-FJTM   EQUAL 'PFK10')
                  PERFORM 2260-MENU-DCOM

               WHEN (WRK-INP-PFK-FJTM      EQUAL '.' AND
                     WRK-INP-COMANDO-FJTM  EQUAL SPACES)
                  PERFORM 2270-TRATAR-ENTER

               WHEN WRK-INP-PFK-FJTM       EQUAL '9' OR
                   (WRK-INP-PFK-FJTM       EQUAL '.' AND
                    WRK-INP-COMANDO-FJTM   EQUAL 'PFK09')
                  PERFORM 2280-CHAMAR-LISTA

               WHEN WRK-INP-PFK-FJTM       EQUAL '6' OR
                   (WRK-INP-PFK-FJTM       EQUAL '.' AND
                    WRK-INP-COMANDO-FJTM   EQUAL 'PFK06') AND
                   WRK-INP-FASE-FJTM       EQUAL '2'
                  PERFORM 2290-TRANSFERIR-PGM

               WHEN OTHER
                  MOVE WRK-MENS02          TO  WRK-OUT-MENSA-FJTM
                  IF  WRK-INP-FASE-FJTM    EQUAL '2'
                      MOVE WRK-49345       TO  WRK-OUT-COMANDO-ATTR-FJTM
                      MOVE WRK-241         TO  WRK-OUT-CCONV-ATTR-FJTM
                                               WRK-OUT-CAGEN-ATTR-FJTM
                                               WRK-OUT-PROD-ATTR-FJTM
                                               WRK-OUT-CSPROD-ATTR-FJTM
                                               WRK-OUT-SITUA-ATTR-FJTM
                  END-IF

           END-EVALUATE.

      *----------------------------------------------------------------*
       2200-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     DEVOLVER TELA                                              *
      *----------------------------------------------------------------*
       2210-DEVOLVER-TELA                  SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INP-CCONTR-FJTM     NOT EQUAL SPACES AND LOW-VALUES
               INSPECT WRK-INP-CCONTR-FJTM-N
                                       REPLACING LEADING ' ' BY '0'
           END-IF.

           IF  WRK-INP-CAGEN-FJTM      NOT EQUAL SPACES AND LOW-VALUES
               INSPECT WRK-INP-CAGEN-FJTM-N
                                       REPLACING LEADING ' ' BY '0'
           END-IF.

           IF  WRK-INP-PROD-FJTM       NOT EQUAL SPACES AND LOW-VALUES
               INSPECT WRK-INP-PROD-FJTM-N
                                       REPLACING LEADING ' ' BY '0'
           END-IF.

           IF  WRK-INP-SPROD-FJTM      NOT EQUAL SPACES AND LOW-VALUES
               INSPECT WRK-INP-SPROD-FJTM-N
                                       REPLACING LEADING ' ' BY '0'
           END-IF.

           IF  WRK-INP-SITUA-FJTM      NOT EQUAL SPACES AND LOW-VALUES
               INSPECT WRK-INP-SITUA-FJTM-N
                                       REPLACING LEADING ' ' BY '0'
           END-IF.

           MOVE  WRK-INP-CCONTR-FJTM       TO WRK-OUT-CCONTR-FJTM
           MOVE  WRK-INP-CAGEN-FJTM        TO WRK-OUT-CAGEN-FJTM
           MOVE  WRK-INP-NAGEN-FJTM        TO WRK-OUT-NAGEN-FJTM
           MOVE  WRK-INP-PROD-FJTM         TO WRK-OUT-PROD-FJTM
           MOVE  WRK-INP-DPROD-FJTM        TO WRK-OUT-DPROD-FJTM
           MOVE  WRK-INP-SPROD-FJTM        TO WRK-OUT-SPROD-FJTM
           MOVE  WRK-INP-DSPROD-FJTM       TO WRK-OUT-DSPROD-FJTM
           MOVE  WRK-INP-SITUA-FJTM        TO WRK-OUT-SITUA-FJTM
           MOVE  WRK-INP-DSITUA-FJTM       TO WRK-OUT-DSITUA-FJTM
           MOVE  WRK-INP-TIMESTA-FJTM      TO WRK-OUT-TIMESTA-FJTM
           MOVE  WRK-INP-FASE-FJTM         TO WRK-OUT-FASE-FJTM.

      *----------------------------------------------------------------*
       2210-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     RETORNAR AO PROGRAMA CHAMADOR                              *
      *----------------------------------------------------------------*
       2240-RETORNAR-CHAMADOR              SECTION.
      *----------------------------------------------------------------*

           INITIALIZE COMU-AREA

           MOVE WRK-INP-TIMESTA-FJTM       TO WRK-ENT-TIMESTA
           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE 'CP'                       TO WRK-ENT-OPCAO

           PERFORM 8200-ACESSAR-DCOM7995

           MOVE WRK-SAI-DADOS              TO WRK-AREA-COMUN-TOTAL

           PERFORM VARYING IND-1               FROM 1 BY 1
                     UNTIL IND-1               GREATER 15
                        OR WRK-CHAMADOR(IND-1) EQUAL SPACES
               MOVE WRK-CHAMADOR(IND-1)    TO WRK-TELA
           END-PERFORM.

           INITIALIZE WRK-DADOS-TEMP.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE WRK-INP-TIMESTA-FJTM       TO WRK-ENT-TIMESTA
                                              COMU-TIMESTAMP
           MOVE 'I '                       TO WRK-ENT-OPCAO
           MOVE WRK-AREA-COMUN-TOTAL       TO WRK-ENT-DADOS

           PERFORM 8200-ACESSAR-DCOM7995.

           MOVE LENGTH COMU-AREA           TO COMU-LL
           MOVE WRK-CHNG                   TO WRK-FUNCAO
           MOVE 'DCOM0610'                 TO COMU-TRANSACAO
           MOVE '3'                        TO COMU-PFK
           MOVE COMU-AREA                  TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2240-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     RETORNAR AO MENU ROTINAS                                   *
      *----------------------------------------------------------------*
       2250-MENU-ROTINAS                   SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-TIMESTA-FJTM       TO WRK-ENT-TIMESTA.
           MOVE 1                          TO WRK-ENT-PAGINA.
           MOVE 'ET'                       TO WRK-ENT-OPCAO.

           PERFORM 8200-ACESSAR-DCOM7995.

           INITIALIZE COMU-AREA-PFK
           MOVE LENGTH OF COMU-AREA-PFK    TO COMU-LL-PFK
           MOVE WRK-CHNG                   TO WRK-FUNCAO
           MOVE 'DCOM0610'                 TO WRK-TELA
                                              COMU-TRANSACAO-PFK
           MOVE '5'                        TO COMU-PFK-PFK
           MOVE COMU-AREA-PFK              TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2250-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     RETORNAR AO MENU DCOM                                      *
      *----------------------------------------------------------------*
       2260-MENU-DCOM                      SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-TIMESTA-FJTM       TO WRK-ENT-TIMESTA.
           MOVE 1                          TO WRK-ENT-PAGINA.
           MOVE 'ET'                       TO WRK-ENT-OPCAO.

           PERFORM 8200-ACESSAR-DCOM7995.

           INITIALIZE COMU-AREA-PFK
           MOVE LENGTH OF COMU-AREA-PFK    TO COMU-LL-PFK
           MOVE WRK-CHNG                   TO WRK-FUNCAO
           MOVE 'DCOM0610'                 TO WRK-TELA
                                              COMU-TRANSACAO-PFK
           MOVE 'A'                        TO COMU-PFK-PFK
           MOVE COMU-AREA-PFK              TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2260-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2270-TRATAR-ENTER                   SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                     TO  WRK-INCONSIS

           IF  WRK-OUT-FASE-FJTM           EQUAL '2'
               MOVE WRK-MENS01             TO  WRK-OUT-MENSA-FJTM
               MOVE '1'                    TO  WRK-OUT-FASE-FJTM
               GO TO 2270-99-FIM
           END-IF

           PERFORM 2271-CONSISTIR-FISICA.

           IF  WRK-INCONSIS                EQUAL  '*'
               GO TO 2270-99-FIM
           END-IF.

           PERFORM 2272-OBTER-NOMES.

           IF  WRK-INCONSIS                EQUAL  '*'
               GO TO 2270-99-FIM
           END-IF.

           MOVE WRK-MENS05                 TO  WRK-OUT-MENSA-FJTM
           MOVE WRK-49345                  TO  WRK-OUT-COMANDO-ATTR-FJTM
           MOVE WRK-241                    TO  WRK-OUT-CCONV-ATTR-FJTM
                                               WRK-OUT-CAGEN-ATTR-FJTM
                                               WRK-OUT-PROD-ATTR-FJTM
                                               WRK-OUT-CSPROD-ATTR-FJTM
                                               WRK-OUT-SITUA-ATTR-FJTM
           MOVE '2'                        TO  WRK-OUT-FASE-FJTM.

      *----------------------------------------------------------------*
       2270-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2271-CONSISTIR-FISICA               SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INP-CCONTR-FJTM         EQUAL SPACES OR LOW-VALUES
               MOVE WRK-49361              TO WRK-OUT-CCONV-ATTR-FJTM
               MOVE WRK-MENS06             TO WRK-OUT-MENSA-FJTM
               MOVE '*'                    TO WRK-INCONSIS
               GO TO 2271-99-FIM
           ELSE
               IF  WRK-INP-CCONTR-FJTM     NOT NUMERIC OR
                   WRK-INP-CCONTR-FJTM     EQUAL ZEROS
                   MOVE WRK-49369          TO WRK-OUT-CCONV-ATTR-FJTM
                   MOVE WRK-MENS07         TO WRK-OUT-MENSA-FJTM
                   MOVE '*'                TO WRK-INCONSIS
                   GO TO 2271-99-FIM
               END-IF
           END-IF.

           IF  WRK-INP-PROD-FJTM           EQUAL SPACES OR  LOW-VALUES
             IF  WRK-INP-SPROD-FJTM    NOT EQUAL SPACES AND LOW-VALUES
                 MOVE WRK-49361            TO WRK-OUT-PROD-ATTR-FJTM
                 MOVE WRK-MENS12           TO WRK-OUT-MENSA-FJTM
                 MOVE '*'                  TO WRK-INCONSIS
                 GO TO 2271-99-FIM
             END-IF
           END-IF.

           IF  WRK-INP-PROD-FJTM       NOT EQUAL SPACES AND LOW-VALUES
             IF WRK-INP-PROD-FJTM      NOT NUMERIC  OR
                WRK-INP-PROD-FJTM-N        EQUAL ZEROS
                MOVE WRK-49369             TO WRK-OUT-PROD-ATTR-FJTM
                MOVE WRK-MENS08            TO WRK-OUT-MENSA-FJTM
                MOVE '*'                   TO WRK-INCONSIS
                GO TO 2271-99-FIM
             END-IF
           END-IF.

           IF  WRK-INP-SPROD-FJTM      NOT EQUAL SPACES AND LOW-VALUES
             IF WRK-INP-SPROD-FJTM     NOT NUMERIC  OR
                WRK-INP-SPROD-FJTM-N       EQUAL ZEROS
                MOVE WRK-49369             TO WRK-OUT-CSPROD-ATTR-FJTM
                MOVE WRK-MENS09            TO WRK-OUT-MENSA-FJTM
                MOVE '*'                   TO WRK-INCONSIS
                GO TO 2271-99-FIM
             END-IF
           END-IF.

           IF  WRK-INP-CAGEN-FJTM      NOT EQUAL SPACES AND LOW-VALUES
               IF  WRK-INP-CAGEN-FJTM  NOT NUMERIC
                   MOVE WRK-49369      TO WRK-OUT-CAGEN-ATTR-FJTM
                   MOVE WRK-MENS10     TO WRK-OUT-MENSA-FJTM
                   MOVE '*'            TO WRK-INCONSIS
                   GO TO 2271-99-FIM
               END-IF
           END-IF.

           IF  WRK-INP-SITUA-FJTM      NOT EQUAL SPACES AND LOW-VALUES
             IF WRK-INP-SITUA-FJTM     NOT NUMERIC  OR
                WRK-INP-SITUA-FJTM-N       EQUAL ZEROS
                MOVE WRK-49369             TO WRK-OUT-SITUA-ATTR-FJTM
                MOVE WRK-MENS11            TO WRK-OUT-MENSA-FJTM
                MOVE '*'                   TO WRK-INCONSIS
                GO TO 2271-99-FIM
             END-IF
           END-IF.

      *----------------------------------------------------------------*
       2271-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2272-OBTER-NOMES                    SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INP-PROD-FJTM       NOT EQUAL SPACES AND LOW-VALUES
               PERFORM 7100-OBTER-NOME-PROD
               IF  WRK-INCONSIS        EQUAL '*'
                   GO TO 2272-99-FIM
               END-IF
           ELSE
               MOVE SPACES                 TO  WRK-OUT-DPROD-FJTM
           END-IF

           IF  WRK-INP-SPROD-FJTM      NOT EQUAL SPACES AND LOW-VALUES
               PERFORM 7200-OBTER-NOME-SUBP
               IF  WRK-INCONSIS        EQUAL '*'
                   GO TO 2272-99-FIM
               END-IF
           ELSE
               MOVE SPACES                 TO  WRK-OUT-DSPROD-FJTM
           END-IF

           IF  WRK-INP-CAGEN-FJTM      NOT EQUAL SPACES AND LOW-VALUES
               PERFORM 7000-OBTER-NOME-AGEN
               IF  WRK-INCONSIS        EQUAL '*'
                   GO TO 2272-99-FIM
               END-IF
           ELSE
               MOVE SPACES                 TO  WRK-OUT-NAGEN-FJTM
           END-IF

           IF  WRK-INP-SITUA-FJTM      NOT EQUAL SPACES AND LOW-VALUES
               PERFORM 7300-OBTER-NOME-SITU
               IF  WRK-INCONSIS        EQUAL '*'
                   GO TO 2272-99-FIM
               END-IF
           ELSE
               MOVE SPACES                 TO  WRK-OUT-DSITUA-FJTM
           END-IF.

      *----------------------------------------------------------------*
       2272-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2280-CHAMAR-LISTA                   SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INP-LINHA-FJTM          EQUAL 10 AND
              (WRK-INP-COLUNA-FJTM         EQUAL 23 OR 24 OR 25)
               PERFORM 8100-OBTER-LISTA-PROD
           ELSE
               IF  WRK-INP-LINHA-FJTM      EQUAL 12 AND
                  (WRK-INP-COLUNA-FJTM     EQUAL 23 OR 24 OR 25)
                   PERFORM 8200-OBTER-LISTA-SUBP
               ELSE
                   IF  WRK-INP-LINHA-FJTM  EQUAL 16 AND
                      (WRK-INP-COLUNA-FJTM EQUAL 23 OR 24 OR 25)
                       PERFORM 8300-OBTER-LISTA-SITU
                   ELSE
                       MOVE 'POSICIONAMENTO DO CURSOR INVALIDO'
                                           TO WRK-OUT-MENSA-FJTM
                       MOVE '*'            TO WRK-INCONSIS
                   END-IF
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2280-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2290-TRANSFERIR-PGM                 SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-AREA-COMUN-TOTAL
                      COMU-AREA.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE WRK-INP-TIMESTA-FJTM       TO WRK-ENT-TIMESTA
                                              COMU-TIMESTAMP

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE 'CP'                       TO WRK-ENT-OPCAO

           PERFORM 8200-ACESSAR-DCOM7995
           MOVE WRK-SAI-DADOS              TO WRK-AREA-COMUN-TOTAL.

           MOVE 'I '                       TO WRK-ENT-OPCAO

           PERFORM VARYING IND-1               FROM 1 BY 1
                     UNTIL IND-1               GREATER 15


               IF  WRK-CHAMADOR(IND-1)         EQUAL SPACES
                   MOVE 'DCOM0610'             TO WRK-CHAMADOR(IND-1)
                                                  COMU-TRANSACAO
                   MOVE 99                     TO IND-1
               END-IF
           END-PERFORM.

           IF  WRK-INP-CAGEN-FJTM          NOT EQUAL SPACES
               MOVE WRK-INP-CAGEN-FJTM     TO WRK-AGENCIA-LISTA
               MOVE WRK-INP-NAGEN-FJTM     TO WRK-DESC-AGENCIA
           ELSE
               MOVE ZEROS                  TO WRK-AGENCIA-LISTA
               MOVE SPACES                 TO WRK-DESC-AGENCIA
           END-IF.

           IF  WRK-INP-CCONTR-FJTM-N       GREATER ZEROS
               MOVE WRK-INP-CCONTR-FJTM-N  TO WRK-CONTRATO
           ELSE
               MOVE ZEROS                  TO WRK-CONTRATO
           END-IF.

           IF  WRK-INP-PROD-FJTM           NOT EQUAL SPACES
               MOVE WRK-INP-PROD-FJTM      TO WRK-PRODUTO-LISTA
               MOVE WRK-INP-DPROD-FJTM     TO WRK-DESC-PRODUTO
           ELSE
               MOVE ZEROS                  TO WRK-PRODUTO-LISTA
               MOVE SPACES                 TO WRK-DESC-PRODUTO
           END-IF.

           IF  WRK-INP-SPROD-FJTM          NOT EQUAL SPACES
               MOVE WRK-INP-SPROD-FJTM     TO WRK-SUBPRODUTO-LISTA
               MOVE WRK-INP-DSPROD-FJTM    TO WRK-DESC-SPROD
           ELSE
               MOVE ZEROS                  TO WRK-SUBPRODUTO-LISTA
               MOVE SPACES                 TO WRK-DESC-SPROD
           END-IF.

           IF  WRK-INP-SITUA-FJTM          NOT EQUAL SPACES
               MOVE WRK-INP-SITUA-FJTM     TO WRK-SITUACAO-LISTA
               MOVE WRK-INP-DSITUA-FJTM    TO WRK-DESC-SITU
           ELSE
               MOVE ZEROS                  TO WRK-SITUACAO-LISTA
               MOVE SPACES                 TO WRK-DESC-SITU
           END-IF.

           MOVE WRK-AREA-COMUN-TOTAL       TO WRK-ENT-DADOS.
           PERFORM 8200-ACESSAR-DCOM7995

           MOVE LENGTH OF COMU-AREA        TO COMU-LL

           MOVE  'DCOM0610'                TO COMU-TRANSACAO
           MOVE  'DCOM0611'                TO WRK-TELA
           MOVE  COMU-AREA                 TO WRK-MENSAGEM
           MOVE  WRK-CHNG                  TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       2290-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2300-PROCESSAR-BRAD0660             SECTION.
      *----------------------------------------------------------------*

           MOVE LENGTH OF WRK-OUTPUT-FJTM
                                       TO  WRK-660-LL-MENSAGEM-FJTM
                                           WRK-OUT-LL-FJTM

           MOVE LENGTH OF WRK-660-DCOMFJTM
                                       TO  WRK-660-LL-AREA-FJTM

           CALL 'BRAD0660'             USING  WRK-OUTPUT-FJTM
                                              WRK-660-DCOMFJTM.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'                  TO ERR-TIPO-ACESSO
               MOVE RETURN-CODE            TO WRK-RETURN-CODE
               MOVE 10                     TO WRK-LOCAL-ERRO
               MOVE WRK-ERRO-BRAD0660      TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-ISRT                   TO WRK-FUNCAO
           MOVE WRK-OUTPUT-FJTM            TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2300-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3000-FINALIZAR                      SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       3000-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       7000-OBTER-NOME-AGEN                SECTION.
      *----------------------------------------------------------------*

           INITIALIZE ENTRADA-ROT6416
                      SAIDA-ROT6416

           MOVE 237                        TO  ENT6416-BANCO
           MOVE WRK-INP-CAGEN-FJTM-N       TO  ENT6416-DEPDC
           MOVE 'DCOM6416'                 TO  WRK-MODULO
           CALL WRK-MODULO              USING  ENTRADA-ROT6416
                                               SAIDA-ROT6416
                                               ERRO-AREA
                                               WRK-SQLCA

           EVALUATE SAI6416-COD-RETORNO
             WHEN '0000'
               MOVE SAI6416-NOME-DEPDC     TO WRK-OUT-NAGEN-FJTM

             WHEN '0099'
               MOVE 'DB2'                  TO ERR-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO

             WHEN OTHER
               MOVE 'AGENCIA INEXISTENTE'
                                           TO WRK-OUT-MENSA-FJTM
               MOVE '*'                    TO WRK-INCONSIS
               MOVE WRK-49369              TO WRK-OUT-CAGEN-ATTR-FJTM

           END-EVALUATE.

      *----------------------------------------------------------------*
       7000-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       7100-OBTER-NOME-PROD                SECTION.
      *----------------------------------------------------------------*

           INITIALIZE ENTRADA-ROT4001
                      SAIDA-ROT4001

           MOVE WRK-COD-USER               TO  ENT4001-CFUNC-BDSCO
           MOVE LNK-IO-LTERM               TO  ENT4001-CTERM
           MOVE WRK-INP-PROD-FJTM-N        TO  ENT4001-CPRODT
           MOVE WRK-INP-TIMESTA-FJTM(9:2)  TO  ENT4001-DT-MOVTO(1:2)
           MOVE WRK-INP-TIMESTA-FJTM(6:2)  TO  ENT4001-DT-MOVTO(4:2)
           MOVE WRK-INP-TIMESTA-FJTM(1:4)  TO  ENT4001-DT-MOVTO(7:4)

           INSPECT ENT4001-DT-MOVTO REPLACING ALL ' ' BY '.'.

           MOVE 'DCOM4001'                 TO  WRK-MODULO
           CALL WRK-MODULO              USING  ENTRADA-ROT4001
                                               SAIDA-ROT4001
                                               ERRO-AREA
                                               WRK-SQLCA

           EVALUATE SAI4001-COD-RETORNO
             WHEN '0000'
               MOVE SAI4001-IABREV-PRODT   TO  WRK-OUT-DPROD-FJTM

             WHEN '0099'
               MOVE 'DB2'                  TO  ERR-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO

             WHEN OTHER
               MOVE 'PRODUTO INEXISTENTE'
                                           TO WRK-OUT-MENSA-FJTM
               MOVE '*'                    TO WRK-INCONSIS
               MOVE WRK-49369              TO WRK-OUT-PROD-ATTR-FJTM

           END-EVALUATE.

      *----------------------------------------------------------------*
       7100-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       7200-OBTER-NOME-SUBP                SECTION.
      *----------------------------------------------------------------*

           INITIALIZE ENTRADA-ROT4049
                      SAIDA-ROT4049

           MOVE WRK-COD-USER               TO  ENT4049-CFUNC-BDSCO
           MOVE LNK-IO-LTERM               TO  ENT4049-CTERM
           MOVE WRK-INP-PROD-FJTM-N        TO  ENT4049-CPRODT
           MOVE WRK-INP-SPROD-FJTM-N       TO  ENT4049-CSPROD-DESC-COML
           MOVE 'DCOM4049'                 TO  WRK-MODULO
           CALL WRK-MODULO              USING  ENTRADA-ROT4049
                                               SAIDA-ROT4049
                                               ERRO-AREA
                                               WRK-SQLCA

           EVALUATE SAI4049-COD-RETORNO
             WHEN '0000'
               MOVE SAI4049-ISPROD-DESC-COML
                                           TO  WRK-OUT-DSPROD-FJTM

             WHEN '0099'
               MOVE 'DB2'                  TO  ERR-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO

             WHEN OTHER
               MOVE 'SUBPRODUTO INEXISTENTE'
                                           TO WRK-OUT-MENSA-FJTM
               MOVE '*'                    TO WRK-INCONSIS
               MOVE WRK-49369              TO WRK-OUT-CSPROD-ATTR-FJTM

           END-EVALUATE.

      *----------------------------------------------------------------*
       7200-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       7300-OBTER-NOME-SITU                SECTION.
      *----------------------------------------------------------------*

           INITIALIZE ENTRADA-ROT4309
                      SAIDA-ROT4309

           MOVE WRK-COD-USER               TO  ENT4309-CFUNC-BDSCO
           MOVE LNK-IO-LTERM               TO  ENT4309-CTERM
           MOVE 6                          TO  ENT4309-CELMTO-DESC-COML
           MOVE WRK-INP-SITUA-FJTM-N       TO  ENT4309-CSIT-DESC-COML
           MOVE 'DCOM4309'                 TO  WRK-MODULO
           CALL WRK-MODULO              USING  ENTRADA-ROT4309
                                               SAIDA-ROT4309
                                               ERRO-AREA
                                               WRK-SQLCA

           EVALUATE SAI4309-COD-RETORNO
             WHEN '0000'
               MOVE SAI4309-IRSUMO-SIT-DESC
                                           TO  WRK-OUT-DSITUA-FJTM

             WHEN '0099'
               MOVE 'DB2'                  TO  ERR-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO

             WHEN OTHER
               MOVE 'SITUACAO INEXISTENTE'
                                           TO WRK-OUT-MENSA-FJTM
               MOVE '*'                    TO  WRK-INCONSIS
               MOVE WRK-49369              TO WRK-OUT-SITUA-ATTR-FJTM

           END-EVALUATE.

      *----------------------------------------------------------------*
       7300-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       8100-OBTER-LISTA-PROD               SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-AREA-COMUN-TOTAL
                      COMU-AREA-3903.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE WRK-INP-TIMESTA-FJTM       TO WRK-ENT-TIMESTA
                                              COMU-TIMESTAMP

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE 'CP'                       TO WRK-ENT-OPCAO

           PERFORM 8200-ACESSAR-DCOM7995
           MOVE WRK-SAI-DADOS              TO WRK-AREA-COMUN-TOTAL.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE WRK-INP-TIMESTA-FJTM       TO WRK-ENT-TIMESTA
                                              COMU-TIMESTAMP
           MOVE 'EP'                       TO WRK-ENT-OPCAO

           PERFORM 8200-ACESSAR-DCOM7995
           MOVE LENGTH OF COMU-AREA-3903   TO COMU-LL-3903
           MOVE WRK-INP-TIMESTA-FJTM       TO COMU-TIMESTA-3903
           MOVE WRK-INP-PROD-FJTM          TO COMU-COD-PROD-3903
           MOVE 'DCOM0610'                 TO COMU-TRANSACAO-3903
           MOVE 'DCOM3903'                 TO WRK-TELA
           MOVE  WRK-CHNG                  TO WRK-FUNCAO.

           PERFORM VARYING IND-1               FROM 1 BY 1
                     UNTIL IND-1               GREATER 15


               IF  WRK-CHAMADOR(IND-1)         EQUAL SPACES
                   MOVE 'DCOM0610'             TO WRK-CHAMADOR(IND-1)
                                                  COMU-TRANSACAO
                   MOVE 99                     TO IND-1
               END-IF
           END-PERFORM.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE 'I '                       TO WRK-ENT-OPCAO

           IF  WRK-INP-CAGEN-FJTM          NOT EQUAL SPACES
               MOVE WRK-INP-CAGEN-FJTM     TO WRK-AGENCIA
               MOVE WRK-INP-NAGEN-FJTM     TO WRK-DESC-AGENCIA
           ELSE
               MOVE ZEROS                  TO WRK-AGENCIA
               MOVE SPACES                 TO WRK-DESC-AGENCIA
           END-IF.

           IF  WRK-INP-CCONTR-FJTM-N       GREATER ZEROS
               MOVE WRK-INP-CCONTR-FJTM-N  TO WRK-CONTRATO
           ELSE
               MOVE ZEROS                  TO WRK-CONTRATO
           END-IF.

           IF  WRK-INP-PROD-FJTM           NOT EQUAL SPACES
               MOVE WRK-INP-PROD-FJTM      TO WRK-PRODUTO-LISTA
           ELSE
               MOVE ZEROS                  TO WRK-PRODUTO-LISTA
           END-IF.

           IF  WRK-INP-SPROD-FJTM          NOT EQUAL SPACES
               MOVE WRK-INP-SPROD-FJTM     TO WRK-SUBPRODUTO-LISTA
           ELSE
               MOVE ZEROS                  TO WRK-SUBPRODUTO-LISTA
           END-IF.

           IF  WRK-INP-SITUA-FJTM          NOT EQUAL SPACES
               MOVE WRK-INP-SITUA-FJTM     TO WRK-SITUACAO-LISTA
           ELSE
               MOVE ZEROS                  TO WRK-SITUACAO-LISTA
           END-IF.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE 'I '                       TO WRK-ENT-OPCAO
           MOVE WRK-AREA-COMUN-TOTAL       TO WRK-ENT-DADOS.
           PERFORM 8200-ACESSAR-DCOM7995.

           MOVE  COMU-AREA-3903            TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       8100-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       8200-OBTER-LISTA-SUBP               SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INP-PROD-FJTM       EQUAL SPACES
               MOVE 'LISTA DE SUBPRODUTO OBRIGATORIO INFORMAR O PRODUTO'
                                       TO WRK-OUT-MENSA-FJTM
               MOVE WRK-49361          TO WRK-OUT-PROD-ATTR-FJTM
               MOVE '*'                TO WRK-INCONSIS
               GO TO 8200-99-FIM
           END-IF.

           IF  WRK-INP-PROD-FJTM       NOT NUMERIC OR
               WRK-INP-PROD-FJTM-N     EQUAL ZEROS
               MOVE WRK-MENS08         TO WRK-OUT-MENSA-FJTM
               MOVE WRK-49369          TO WRK-OUT-PROD-ATTR-FJTM
               MOVE '*'                TO WRK-INCONSIS
               GO TO 8200-99-FIM
           END-IF.

           PERFORM 7100-OBTER-NOME-PROD.

           IF  WRK-INCONSIS            EQUAL '*'
               GO TO 8200-99-FIM
           END-IF.

           INITIALIZE WRK-AREA-COMUN-TOTAL
                      COMU-AREA-3904.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE WRK-INP-TIMESTA-FJTM       TO WRK-ENT-TIMESTA
                                              COMU-TIMESTAMP

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE 'CP'                       TO WRK-ENT-OPCAO

           PERFORM 8200-ACESSAR-DCOM7995
           MOVE WRK-SAI-DADOS              TO WRK-AREA-COMUN-TOTAL.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE WRK-INP-TIMESTA-FJTM       TO WRK-ENT-TIMESTA
                                              COMU-TIMESTAMP
           MOVE 'EP'                       TO WRK-ENT-OPCAO

           PERFORM 8200-ACESSAR-DCOM7995
           MOVE LENGTH OF COMU-AREA-3904   TO COMU-LL-3904

           MOVE WRK-INP-TIMESTA-FJTM       TO COMU-TIMESTA-3904
           MOVE WRK-INP-PROD-FJTM          TO COMU-COD-PROD-3904
           MOVE WRK-INP-SPROD-FJTM         TO COMU-COD-SPROD-3904
           MOVE ZEROS                      TO COMU-CNPJCPF-3904
           MOVE 'DCOM0610'                 TO COMU-TRANSACAO-3904
           MOVE 'DCOM3904'                 TO WRK-TELA
           MOVE  COMU-AREA-3904            TO WRK-MENSAGEM
           MOVE  WRK-CHNG                  TO WRK-FUNCAO.

           PERFORM VARYING IND-1               FROM 1 BY 1
                     UNTIL IND-1               GREATER 15


               IF  WRK-CHAMADOR(IND-1)         EQUAL SPACES
                   MOVE 'DCOM0610'             TO WRK-CHAMADOR(IND-1)
                                                  COMU-TRANSACAO
                   MOVE 99                     TO IND-1
               END-IF
           END-PERFORM.

           IF  WRK-INP-CAGEN-FJTM          NOT EQUAL SPACES
               MOVE WRK-INP-CAGEN-FJTM     TO WRK-AGENCIA
               MOVE WRK-INP-NAGEN-FJTM     TO WRK-DESC-AGENCIA
           ELSE
               MOVE ZEROS                  TO WRK-AGENCIA
               MOVE SPACES                 TO WRK-DESC-AGENCIA
           END-IF.

           IF  WRK-INP-CCONTR-FJTM-N       GREATER ZEROS
               MOVE WRK-INP-CCONTR-FJTM-N  TO WRK-CONTRATO
           ELSE
               MOVE ZEROS                  TO WRK-CONTRATO
           END-IF

           IF  WRK-INP-PROD-FJTM           NOT EQUAL SPACES
               MOVE WRK-INP-PROD-FJTM      TO WRK-PRODUTO-LISTA
           ELSE
               MOVE ZEROS                  TO WRK-PRODUTO-LISTA
           END-IF.

           IF  WRK-INP-SPROD-FJTM          NOT EQUAL SPACES
               MOVE WRK-INP-SPROD-FJTM     TO WRK-SUBPRODUTO-LISTA
           ELSE
               MOVE ZEROS                  TO WRK-SUBPRODUTO-LISTA
           END-IF.

           IF  WRK-INP-SITUA-FJTM          NOT EQUAL SPACES
               MOVE WRK-INP-SITUA-FJTM     TO WRK-SITUACAO-LISTA
           ELSE
               MOVE ZEROS                  TO WRK-SITUACAO-LISTA
           END-IF.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE 'I '                       TO WRK-ENT-OPCAO
           MOVE WRK-AREA-COMUN-TOTAL       TO WRK-ENT-DADOS.
           PERFORM 8200-ACESSAR-DCOM7995.


      *----------------------------------------------------------------*
       8200-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       8300-OBTER-LISTA-SITU               SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-AREA-COMUN-TOTAL.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE WRK-INP-TIMESTA-FJTM       TO WRK-ENT-TIMESTA
                                              COMU-TIMESTAMP

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE 'CP'                       TO WRK-ENT-OPCAO

           PERFORM 8200-ACESSAR-DCOM7995
           MOVE WRK-SAI-DADOS              TO WRK-AREA-COMUN-TOTAL.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE WRK-INP-TIMESTA-FJTM       TO WRK-ENT-TIMESTA
                                              COMU-TIMESTAMP
           MOVE 'EP'                       TO WRK-ENT-OPCAO

           PERFORM 8200-ACESSAR-DCOM7995
           INITIALIZE COMU-AREA-3913
           MOVE LENGTH OF COMU-AREA-3913   TO COMU-LL-3913
           MOVE WRK-INP-TIMESTA-FJTM       TO COMU-TIMESTA-3913

           MOVE ZEROS                      TO COMU-CSIT-DESC-3913
           MOVE 006                        TO COMU-CELMTO-DESC-3913
           MOVE 'DCOM0610'                 TO COMU-TRANSACAO-3913
           MOVE 'DCOM3913'                 TO WRK-TELA
           MOVE  COMU-AREA-3913            TO WRK-MENSAGEM
           MOVE  WRK-CHNG                  TO WRK-FUNCAO.

           PERFORM VARYING IND-1               FROM 1 BY 1
                     UNTIL IND-1               GREATER 15


               IF  WRK-CHAMADOR(IND-1)         EQUAL SPACES
                   MOVE 'DCOM0610'             TO WRK-CHAMADOR(IND-1)
                                                  COMU-TRANSACAO
                   MOVE 99                     TO IND-1
               END-IF
           END-PERFORM.

           IF  WRK-INP-CAGEN-FJTM          NOT EQUAL SPACES
               MOVE WRK-INP-CAGEN-FJTM     TO WRK-AGENCIA
               MOVE WRK-INP-NAGEN-FJTM     TO WRK-DESC-AGENCIA
           ELSE
               MOVE ZEROS                  TO WRK-AGENCIA
               MOVE SPACES                 TO WRK-DESC-AGENCIA
           END-IF.

           IF  WRK-INP-CCONTR-FJTM-N       GREATER ZEROS
               MOVE WRK-INP-CCONTR-FJTM-N  TO WRK-CONTRATO
           ELSE
               MOVE ZEROS                  TO WRK-CONTRATO
           END-IF.

           IF  WRK-INP-PROD-FJTM           NOT EQUAL SPACES
               MOVE WRK-INP-PROD-FJTM      TO WRK-PRODUTO-LISTA
           ELSE
               MOVE ZEROS                  TO WRK-PRODUTO-LISTA
           END-IF.

           IF  WRK-INP-SPROD-FJTM          NOT EQUAL SPACES
               MOVE WRK-INP-SPROD-FJTM     TO WRK-SUBPRODUTO-LISTA
           ELSE
               MOVE ZEROS                  TO WRK-SUBPRODUTO-LISTA
           END-IF.

           IF  WRK-INP-SITUA-FJTM          NOT EQUAL SPACES
               MOVE WRK-INP-SITUA-FJTM     TO WRK-SITUACAO-LISTA
           ELSE
               MOVE ZEROS                  TO WRK-SITUACAO-LISTA
           END-IF.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE 'I '                       TO WRK-ENT-OPCAO
           MOVE WRK-AREA-COMUN-TOTAL       TO WRK-ENT-DADOS.
           PERFORM 8200-ACESSAR-DCOM7995.

      *----------------------------------------------------------------*
       8300-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO          SECTION.
      *----------------------------------------------------------------*

           IF  ERR-MODULO              NOT EQUAL SPACES AND LOW-VALUES
               MOVE 'DCOM0610'             TO ERR-PGM
           ELSE
               MOVE 'SENHAS02'             TO ERR-PGM
               MOVE 'DCOM0610'             TO ERR-MODULO
           END-IF.

           MOVE WRK-COD-USER               TO ERR-COD-USER.
           MOVE WRK-COD-DEPTO              TO ERR-COD-DEPTO.

           IF  ERR-TIPO-ACESSO             EQUAL 'DB2'
               CALL 'BRAD7100'         USING  LNK-IO-PCB
                                              ERRO-AREA
                                              LNK-ALT-PCB
                                              WRK-SQLCA
           ELSE
               CALL 'BRAD7100'         USING  LNK-IO-PCB
                                              ERRO-AREA
                                              LNK-ALT-PCB
           END-IF

           PERFORM 3000-FINALIZAR.

      *----------------------------------------------------------------*
       9999-99-FIM.                        EXIT.
      *----------------------------------------------------------------*
