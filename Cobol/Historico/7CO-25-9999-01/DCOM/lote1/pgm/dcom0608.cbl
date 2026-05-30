      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
      *
       PROGRAM-ID.     DCOM0608.
       AUTHOR.         FABRICA - ALTRAN.
      *================================================================*
      *                          A L T R A N                           *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   DCOM0608                                    *
      *    PROGRAMADOR.:   FABRICA - ALTRAN                            *
      *    ANALISTA....:   EMERSON ARTUR FRANCO                        *
      *    DATA........:   21/05/2007                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   CONSULTA OPERACOES - POR CONVENIO           *
      *                                                                *
      *----------------------------------------------------------------*
      *    TELAS.......:                                               *
      *    DCOMFHTM - CONSULTA OPERACOES - POR CONVENIO                *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    I#BRAD7C - AREA DE ERRO DA BRAD7100.                        *
      *    I#DCOMSY - TABELA DE MSG DE RETORNO DO SISTEMA DCOM(ONLINE).*
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    BRAD0660 - INSERIR CARACTERES DE NULL NA MENSAGEM DE SAIDA  *
      *    BRAD7100 - TRATAMENTO DE ERROS QUANDO PROGRAMA INVALIDO     *
      *    POOL5005 - CONTROLE DO TRANSITO DE MENSAGENS NO IMS/DC      *
      *    DCOM4200 - MODULO DE NEGOCIO - OBTER NOME DO CONVENIO       *
      *    DCOM6416 - MODULO DE NEGOCIO - OBTER NOME DA AGENCIA        *
      *    DCOM4001 - MODULO DE NEGOCIO - OBTER NOME DO PRODUTO        *
      *    DCOM4049 - MODULO DE NEGOCIO - OBTER NOME DO SUB-PRODUTO    *
      *    DCOM4309 - MODULO DE NEGOCIO - OBTER NOME DA SITUACAO       *
      *----------------------------------------------------------------*
      *    NAVEGACAO...:                                               *
      *    CHAMADO POR.: DCOM0602           CHAMA: DCOM0609            *
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
       01  FILLER                      PIC  X(050)         VALUE
           '*** DCOM0608 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ATRIBUTOS ***'.
      *---------------------------------------------------------------*

      *--- NUMERIC  - NORMAL    - PROTEGIDO    - NAO POS CURSOR ---*
       01  WRK-241                     PIC S9(008) COMP    VALUE +241.

      *--- NUMERICO - BRILHANTE - DESPROTEGIDO - POS. CURSOR    ---*
       01  WRK-49369                   PIC S9(008) COMP    VALUE +49369.

      *--- ALPHA    - NORMAL    - DESPROTEGIDO - POS. CURSOR    ---*
       01  WRK-49345                   PIC S9(008) COMP    VALUE +49345.

      *--- NUMERICO - NORMAL    - DESPROTEGIDO - POS. CURSOR    ---*
       01  WRK-49361                   PIC S9(008) COMP    VALUE +49361.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*

       01  WRK-AUXILIARES.
           05  IND-1                   PIC  9(009) COMP-3  VALUE ZEROS.
           05  WRK-MODULO              PIC  X(008)         VALUE SPACES.
           05  WRK-INCONSIS            PIC  X(001)         VALUE SPACES.
           05  WRK-MENS01              PIC  X(079)         VALUE
               'DIGITE A OPCAO DESEJADA'.
           05  WRK-MENS02              PIC  X(079)         VALUE
               'PF INVALIDA'.
           05  WRK-MENS05              PIC  X(079)         VALUE
               'TECLE <PF6> PARA CONFIRMAR OU <ENTER> PARA ALTERAR'.
           05  WRK-MENS06              PIC  X(079)         VALUE
               'CONVENIO OBRIGATORIO'.
           05  WRK-MENS07              PIC  X(079)         VALUE
               'CONVENIO INVALIDO'.
           05  WRK-MENS08              PIC  X(079)         VALUE
               'SELECIONE TIPO CONVENIO GERAL OU CLIENTE'.
           05  WRK-MENS09              PIC  X(079)         VALUE
               'INFORME CONVENIO COM "X"'.
           05  WRK-MENS10              PIC  X(079)         VALUE
               'PRODUTO INVALIDO'.
           05  WRK-MENS11              PIC  X(079)         VALUE
               'SUBPRODUTO INVALIDO'.
           05  WRK-MENS12              PIC  X(079)         VALUE
               'AGENCIA INVALIDA'.
           05  WRK-MENS13              PIC  X(079)         VALUE
               'SITUACAO INVALIDA'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO DCOM0608 / DCOM6209 ***'.
      *----------------------------------------------------------------*

       01  WRK-COMU-AREA.
           05 WRK-COMU-LL              PIC S9(004) COMP    VALUE +406.
           05 WRK-COMU-ZZ              PIC S9(004) COMP    VALUE ZEROS.
           05 WRK-COMU-TRANCODE.
             10 WRK-COMU-TRANSACAO     PIC  X(008)         VALUE SPACES.
             10 FILLER                 PIC  X(006)         VALUE SPACES.
             10 WRK-COMU-PFK           PIC  X(001)         VALUE SPACES.
           05 WRK-COMU-SENHAS          PIC  X(037)         VALUE SPACES.
           05 WRK-COMU-COMANDO         PIC  X(068)         VALUE SPACES.
           05 WRK-COMU-DADOS.
             10 WRK-COMU-TIMESTAMP     PIC  X(026)         VALUE SPACES.
             10 WRK-COMU-MENSAGEM      PIC  X(079)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO DCOM6416 ***'.
      *----------------------------------------------------------------*

       01  ENTRADA-ROT6416.
           05 ENT6416-COD-RETORNO      PIC  X(004)         VALUE SPACES.
           05 ENT6416-MSG-RETORNO      PIC  X(079)         VALUE SPACES.
           05 ENT6416-RESTART          PIC  9(005)         VALUE ZEROS.
           05 ENT6416-FLAG             PIC  X(001)         VALUE SPACES.
           05 ENT6416-CFUNC-BDSCO      PIC  9(009)         VALUE ZEROS.
           05 ENT6416-CTERM            PIC  X(008)         VALUE SPACES.
           05 ENT6416-BANCO            PIC  9(003)         VALUE ZEROS.
           05 ENT6416-DEPDC            PIC  9(005)         VALUE ZEROS.

       01  SAIDA-ROT6416.
           10 SAI6416-COD-RETORNO      PIC  X(004)         VALUE SPACES.
           10 SAI6416-MSG-RETORNO      PIC  X(079)         VALUE SPACES.
           10 SAI6416-RESTART          PIC  9(005)         VALUE ZEROS.
           10 SAI6416-FLAG             PIC  X(001)         VALUE SPACES.
           05 SAI6416-DADOS.
             10 SAI6416-TABELA-SAIDA.
             15 SAI6416-AG-DEPDC       PIC  9(005)         VALUE ZEROS.
             15 SAI6416-DG-DEPDC       PIC  X(001)         VALUE SPACES.
             15 SAI6416-NOME-DEPDC     PIC  X(040)         VALUE SPACES.
             15 SAI6416-END-DEPDC      PIC  X(030)         VALUE SPACES.
             15 SAI6416-DIRETORIA-REG  PIC  9(005)         VALUE ZEROS.
             15 SAI6416-DIRETORIA-NOME PIC  X(040)         VALUE SPACES.
             15 SAI6416-NOME-BANCO     PIC  X(040)         VALUE SPACES.
             15 SAI6416-POLO-SERVICO   PIC  9(005)         VALUE ZEROS.
             15 SAI6416-MUNIC-DEPDC    PIC  X(025)         VALUE SPACES.
             15 SAI6416-UF             PIC  X(002)         VALUE SPACES.
             15 SAI6416-CEP            PIC  9(005)         VALUE ZEROS.
             15 SAI6416-CEP-COMPL      PIC  9(003)         VALUE ZEROS.
             15 SAI6416-INDIC-DEPDC-AT PIC  X(001)         VALUE SPACES.
             15 SAI6416-NATUREZA-DEPDC PIC  X(001)         VALUE SPACES.
             15 SAI6416-COD-DISTR-CORR PIC  9(005)         VALUE ZEROS.
             15 SAI6416-DENCRR-DEPDC-BDSCO
                                       PIC  X(010)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO DCOM4001 ***'.
      *----------------------------------------------------------------*

       01 ENTRADA-ROT4001.
          05 ENT4001-COD-RETORNO       PIC  X(004)         VALUE SPACES.
          05 ENT4001-MSG-RETORNO       PIC  X(079)         VALUE SPACES.
          05 ENT4001-RESTART           PIC  9(005)         VALUE ZEROS.
          05 ENT4001-FLAG              PIC  X(001)         VALUE SPACES.
          05 ENT4001-CFUNC-BDSCO       PIC  9(009)         VALUE ZEROS.
          05 ENT4001-CTERM             PIC  X(008)         VALUE SPACES.
          05 ENT4001-CPRODT            PIC  9(003)         VALUE ZEROS.
          05 ENT4001-DT-MOVTO          PIC  X(010)         VALUE SPACES.

       01 SAIDA-ROT4001.
          05 SAI4001-COD-RETORNO       PIC  X(004)         VALUE SPACES.
          05 SAI4001-MSG-RETORNO       PIC  X(079)         VALUE SPACES.
          05 SAI4001-RESTART           PIC  9(005)         VALUE ZEROS.
          05 SAI4001-FLAG              PIC  X(001)         VALUE SPACES.
          05 SAI4001-DADOS.
            10 SAI4001-CPRODT          PIC 9(003)          VALUE ZEROS.
            10 SAI4001-IPRODT          PIC X(060)          VALUE SPACES.
            10 SAI4001-IABREV-PRODT    PIC X(012)          VALUE SPACES.
            10 SAI4001-CSGL-PRODT-DESC PIC X(006)          VALUE SPACES.
            10 SAI4001-CIDTFD-TX-MIN-JURO
                                       PIC 9(005)          VALUE ZEROS.
            10 SAI4001-CIDTFD-TX-MIN-VLR
                                       PIC 9(015)          VALUE ZEROS.
            10 SAI4001-CIDTFD-TX-MIN-DEC
                                       PIC 9(003)          VALUE ZEROS.
            10 SAI4001-CIDTFD-TX-MAX-JURO
                                       PIC 9(005)          VALUE ZEROS.
            10 SAI4001-CIDTFD-TX-MAX-VLR
                                       PIC 9(015)          VALUE ZEROS.
            10 SAI4001-CIDTFD-TX-MAX-DEC
                                       PIC 9(003)          VALUE ZEROS.
            10 SAI4001-HULT-ATULZ      PIC X(022)          VALUE SPACES.
         05 SAI4001-CONSISTENCIA       REDEFINES   SAI4001-DADOS.
            10 SAI4001-TAMANHO-ERRO    PIC  9(001).
            10 SAI4001-TABELA-ERROS    OCCURS 002  TIMES.
              15 SAI4001-CAMPOS-ERR    PIC  9(001).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO DCOM4049 ***'.
      *----------------------------------------------------------------*

       01  ENTRADA-ROT4049.
           05 WRK-4049E-COD-RETORNO    PIC  X(004)         VALUE SPACES.
           05 WRK-4049E-MSG-RETORNO    PIC  X(079)         VALUE SPACES.
           05 WRK-4049E-RESTART        PIC  9(005)         VALUE ZEROS.
           05 WRK-4049E-FLAG           PIC  X(001)         VALUE SPACES.
           05 WRK-4049E-CFUNC-BDSCO    PIC  9(009)         VALUE ZEROS.
           05 WRK-4049E-CTERM          PIC  X(008)         VALUE SPACES.
           05 WRK-4049E-CPRODT         PIC  9(003)         VALUE ZEROS.
           05 WRK-4049E-CSPROD-DESC-COML
                                       PIC  9(003)         VALUE ZEROS.

       01  SAIDA-ROT4049.
           05 WRK-4049S-COD-RETORNO    PIC  X(004)         VALUE SPACES.
           05 WRK-4049S-MSG-RETORNO    PIC  X(079)         VALUE SPACES.
           05 WRK-4049S-RESTART        PIC  9(005)         VALUE ZEROS.
           05 WRK-4049S-FLAG           PIC  X(001)         VALUE SPACES.
           05 WRK-4049S-DADOS.
             10 WRK-4049S-CPRODT       PIC  9(003)         VALUE ZEROS.
             10 WRK-4049S-IPRODT       PIC  X(060)         VALUE SPACES.
             10 WRK-4049S-CSPROD-DESC-COML
                                       PIC  9(003)         VALUE ZEROS.
             10 WRK-4049S-ID-DUPLICATA PIC  X(001)         VALUE SPACES.
             10 WRK-4049S-DINIC-VGCIA-SPROD
                                       PIC  X(010)         VALUE SPACES.
             10 WRK-4049S-DFIM-VGCIA-SPROD
                                       PIC  X(010)         VALUE SPACES.
             10 WRK-4049S-CTPO-DESC-COML
                                       PIC  9(003)         VALUE ZEROS.
             10 WRK-4049S-IRSUMO-TPO-DESC
                                       PIC  X(010)         VALUE SPACES.
             10 WRK-4049S-ISPROD-DESC-COML
                                       PIC  X(040)         VALUE SPACES.
             10 WRK-4049S-IRSUMO-SPROD-DESC
                                       PIC  X(010)         VALUE SPACES.
             10 WRK-4049S-RSPROD-DESC-COML
                                       PIC  X(240)         VALUE SPACES.
             10 WRK-4049S-CSGL-SPROD-DESC
                                       PIC  X(006)         VALUE SPACES.
             10 WRK-4049S-CSIT-DESC-COML
                                       PIC  9(003)         VALUE ZEROS.
             10 WRK-4049S-HSIT-DESC-COML
                                       PIC  X(022)         VALUE SPACES.
             10 WRK-4049S-IRSUMO-SIT-DESC
                                       PIC  X(015)         VALUE SPACES.
             10 WRK-4049S-CFUNC-BDSCO-ALT
                                       PIC  9(009)         VALUE ZEROS.
             10 WRK-4049S-NFUNC-BDSCO-ALT
                                       PIC  X(040)         VALUE SPACES.
             10 WRK-4049S-CFUNC-BDSCO-INC
                                       PIC  9(009)         VALUE ZEROS.
             10 WRK-4049S-NFUNC-BDSCO-INC
                                       PIC  X(040)         VALUE SPACES.
             10 WRK-4049S-HULT-ATULZ-ALT
                                       PIC  X(022)         VALUE SPACES.
             10 WRK-4049S-HULT-ATULZ-INC
                                       PIC  X(022)         VALUE SPACES.
             10 WRK-4049S-HULT-ATULZ   PIC  X(022)         VALUE SPACES.
             10 WRK-4049S-CFUNC-BDSCO  PIC  9(009)         VALUE ZEROS.
             10 WRK-4049S-NFUNC-BDSCO  PIC  X(040)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO DCOM4309 ***'.
      *----------------------------------------------------------------*

       01  ENTRADA-ROT4309.
           05 WRK-4309E-COD-RETORNO    PIC  X(004)         VALUE SPACES.
           05 WRK-4309E-MSG-RETORNO    PIC  X(079)         VALUE SPACES.
           05 WRK-4309E-RESTART        PIC  9(005)         VALUE ZEROS.
           05 WRK-4309E-FLAG           PIC  X(001)         VALUE SPACES.
           05 WRK-4309E-CFUNC-BDSCO    PIC  9(009)         VALUE ZEROS.
           05 WRK-4309E-CTERM          PIC  X(008)         VALUE SPACES.
           05 WRK-4309E-DADOS.
             10 WRK-4309E-CSIT-DESC-COML
                                       PIC  9(003)         VALUE ZEROS.
             10 WRK-4309E-CELMTO-DESC-COML
                                       PIC  9(003)         VALUE ZEROS.

       01  SAIDA-ROT4309.
           05 WRK-4309S-COD-RETORNO    PIC  X(004)         VALUE SPACES.
           05 WRK-4309S-MSG-RETORNO    PIC  X(079)         VALUE SPACES.
           05 WRK-4309S-RESTART        PIC  9(005)         VALUE ZEROS.
           05 WRK-4309S-FLAG           PIC  X(001)         VALUE SPACES.
           05 WRK-4309S-DADOS.
             10 WRK-4309S-CSIT-DESC-COML
                                       PIC  9(003)         VALUE ZEROS.
             10 WRK-4309S-IRSUMO-SIT-DESC
                                       PIC  X(015)         VALUE SPACES.
             10 WRK-4309S-ISIT-DESC-COML
                                       PIC  X(030)         VALUE SPACES.
             10 WRK-4309S-CSGL-SIT-DESC
                                       PIC  X(006)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO DCOM3903 ***'.
      *----------------------------------------------------------------*

       01  WRK-COMU-AREA-3903.
           05  WRK-COMU-LL-3903            PIC S9(04) COMP VALUE +273.
           05  WRK-COMU-ZZ-3903            PIC S9(04) COMP VALUE ZEROS.
           05  WRK-COMU-TRANCODE-3903.
               10  WRK-COMU-TRANSACAO-3903 PIC  X(008)     VALUE SPACES.
               10  FILLER                  PIC  X(006)     VALUE SPACES.
               10  WRK-COMU-PFK-3903       PIC  X(001)     VALUE SPACES.
           05  WRK-COMU-SENHAS-3903        PIC  X(037)     VALUE SPACES.
           05  WRK-COMU-COMANDO-3903       PIC  X(068)     VALUE SPACES.
           05  WRK-COMU-DADOS-3903.
               10  WRK-COMU-OPCAO-3903     PIC  9(001)     VALUE ZEROS.
               10  WRK-COMU-TIMESTA-3903   PIC  X(026)     VALUE SPACES.
               10  WRK-COMU-COD-PROD-X-3903.
                   15  WRK-COMU-COD-PROD-3903
                                           PIC  9(003)     VALUE ZEROS.
               10  WRK-COMU-DES-PROD-3903  PIC  X(040)     VALUE SPACES.
               10  WRK-COMU-MENSA-3903     PIC  X(079)     VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(50)      VALUE
           '*** AREA DE COMUNICACAO DCOM3904 ***'.
      *----------------------------------------------------------------*

       01  WRK-COMU-AREA-3904.
           05  WRK-COMU-LL-3904            PIC S9(04) COMP VALUE +293.
           05  WRK-COMU-ZZ-3904            PIC S9(04) COMP VALUE ZEROS.
           05  WRK-COMU-TRANCODE-3904.
               10  WRK-COMU-TRANSACAO-3904 PIC  X(008)     VALUE SPACES.
               10  FILLER                  PIC  X(006)     VALUE SPACES.
               10  WRK-COMU-PFK-3904       PIC  X(001)     VALUE SPACES.
           05  WRK-COMU-SENHAS-3904        PIC  X(037)     VALUE SPACES.
           05  WRK-COMU-COMANDO-3904       PIC  X(068)     VALUE SPACES.
           05  WRK-COMU-DADOS-3904.
               10  WRK-COMU-OPCAO-3904     PIC  9(001)     VALUE ZEROS.
               10  WRK-COMU-TIMESTA-3904   PIC  X(026)     VALUE SPACES.
               10  WRK-COMU-CNPJCPF-3904   PIC  X(017)     VALUE SPACES.
               10  WRK-COMU-COD-PROD-X-3904.
                   15  WRK-COMU-COD-PROD-3904
                                           PIC 9(003)      VALUE ZEROS.
               10  WRK-COMU-COD-SPROD-X-3904.
                   15  WRK-COMU-COD-SPROD-3904
                                           PIC  9(003)     VALUE ZEROS.
               10  WRK-COMU-DES-SPROD-3904 PIC  X(040)     VALUE SPACES.
               10  WRK-COMU-MENSA-3904     PIC  X(079)     VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(50)      VALUE
           '*** AREA DE COMUNICACAO DCOM3913 ***'.
      *----------------------------------------------------------------*

       01  WRK-COMU-AREA-3913.
           05  WRK-COMU-LL-3913            PIC S9(04) COMP VALUE +180.
           05  WRK-COMU-ZZ-3913            PIC S9(04) COMP VALUE ZEROS.
           05  WRK-COMU-TRANCODE-3913.
               10  WRK-COMU-TRANSACAO-3913 PIC  X(008)     VALUE SPACES.
               10  FILLER                  PIC  X(006)     VALUE SPACES.
               10  WRK-COMU-PFK-3913       PIC  X(001)     VALUE SPACES.
           05  WRK-COMU-SENHAS-3913        PIC  X(037)     VALUE SPACES.
           05  WRK-COMU-COMANDO-3913       PIC  X(068)     VALUE SPACES.
           05  WRK-COMU-DADOS-3913.
               10  WRK-COMU-TIMESTA-3913   PIC  X(026)     VALUE SPACES.
               10  WRK-COMU-TPOPCAO-3913   PIC  X(001)     VALUE SPACES.
               10  WRK-COMU-CELMTO-DESC-3913 PIC 9(003)    VALUE ZEROS.
               10  WRK-COMU-CSIT-DESC-3913 PIC  9(003)     VALUE ZEROS.
               10  WRK-COMU-COD-SITUA-3913 PIC  9(003)     VALUE ZEROS.
               10  WRK-COMU-DES-SITUA-3913 PIC  X(020)     VALUE SPACES.
      *
      *---------------------------------------------------------------*
       01  FILLER                          PIC  X(050)     VALUE
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
           05  WRK-COD-DEPTO.
               10  FILLER              PIC  X(002)         VALUE SPACES.
               10  WRK-COD-DEPTO-N     PIC  9(004)         VALUE ZEROS.

           05  WRK-MENSAGEM.
               10  FILLER              PIC  X(004)         VALUE SPACES.
               10  WRK-TRANSACAO       PIC  X(008)         VALUE SPACES.
               10  FILLER              PIC  X(1988)        VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(050)     VALUE
           '*** AREA DA TELA DCOMFHTM  - BRAD0660 ***'.
      *----------------------------------------------------------------*

       01  WRK-660-DCOMFHTM.
           05  WRK-660-LL-AREA-FHTM        PIC  9(04) COMP VALUE 0042.
           05  WRK-660-LL-MENSAGEM-FHTM    PIC  9(04) COMP VALUE 0348.
           05  WRK-660-SENHAS-FHTM         PIC  9(04) COMP VALUE 0037.
           05  WRK-660-COMANDO-FHTM        PIC  9(04) COMP VALUE 2070.
           05  WRK-660-TIMESTA-FHTM        PIC  9(04) COMP VALUE 0026.
           05  WRK-660-FASE-FHTM           PIC  9(04) COMP VALUE 0001.
           05  WRK-660-CCONV-FHTM          PIC  9(04) COMP VALUE 2011.
           05  WRK-660-SLGER-FHTM          PIC  9(04) COMP VALUE 2003.
           05  WRK-660-SLCLI-FHTM          PIC  9(04) COMP VALUE 2003.
           05  WRK-660-NCONV-FHTM          PIC  9(04) COMP VALUE 0015.
           05  WRK-660-PROD-FHTM           PIC  9(04) COMP VALUE 2005.
           05  WRK-660-DPROD-FHTM          PIC  9(04) COMP VALUE 0012.
           05  WRK-660-SPROD-FHTM          PIC  9(04) COMP VALUE 2005.
           05  WRK-660-DSPROD-FHTM         PIC  9(04) COMP VALUE 0010.
           05  WRK-660-CAGEN-FHTM          PIC  9(04) COMP VALUE 2007.
           05  WRK-660-NAGEN-FHTM          PIC  9(04) COMP VALUE 0040.
           05  WRK-660-SITUA-FHTM          PIC  9(04) COMP VALUE 2005.
           05  WRK-660-DSITUA-FHTM         PIC  9(04) COMP VALUE 0015.
           05  WRK-660-MENSA-FHTM          PIC  9(04) COMP VALUE 0079.

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
           '*** AREA DA TELA DCOMFHTM - INPUT ***'.
      *----------------------------------------------------------------*

       01  WRK-INPUT-FHTM.
           05  FILLER                  PIC  X(018)         VALUE SPACES.
           05  WRK-INP-PFK-FHTM        PIC  X(001)         VALUE SPACES.
           05  WRK-INP-SENHAS-FHTM     PIC  X(037)         VALUE SPACES.
           05  WRK-INP-COMANDO-FHTM    PIC  X(068)         VALUE SPACES.
           05  WRK-INP-TIMESTA-FHTM    PIC  X(026)         VALUE SPACES.
           05  WRK-INP-FASE-FHTM       PIC  X(001)         VALUE SPACES.
           05  WRK-INP-DADOS-FHTM.
               10  WRK-INP-CCONV-FHTM.
                   15  WRK-INP-CCONV-FHTM-N
                                       PIC  9(009)         VALUE ZEROS.
               10  WRK-INP-SLGER-FHTM  PIC  X(001)         VALUE SPACES.
               10  WRK-INP-SLCLI-FHTM  PIC  X(001)         VALUE SPACES.
               10  WRK-INP-NCONV-FHTM  PIC  X(015)         VALUE SPACES.
               10  WRK-INP-PROD-FHTM.
                   15  WRK-INP-PROD-FHTM-N
                                       PIC  9(003)         VALUE ZEROS.
               10  WRK-INP-DPROD-FHTM  PIC  X(012)         VALUE ZEROS.
               10  WRK-INP-SPROD-FHTM.
                   15  WRK-INP-SPROD-FHTM-N
                                       PIC  9(003)         VALUE ZEROS.
               10  WRK-INP-DSPROD-FHTM PIC  X(010)         VALUE ZEROS.
               10  WRK-INP-CAGEN-FHTM.
                   15  WRK-INP-CAGEN-FHTM-N
                                       PIC  9(005)         VALUE ZEROS.
               10  WRK-INP-NAGEN-FHTM  PIC  X(040)         VALUE ZEROS.
               10  WRK-INP-SITUA-FHTM.
                   15  WRK-INP-SITUA-FHTM-N
                                       PIC  9(003)         VALUE ZEROS.
               10  WRK-INP-DSITUA-FHTM PIC  X(015)         VALUE ZEROS.
           05  WRK-INP-MENSA-FHTM      PIC  X(079)         VALUE SPACES.
           05  WRK-INP-POSI-FHTM.
               10  WRK-INP-LINHA-FHTM  PIC S9(004) COMP    VALUE ZEROS.
               10  WRK-INP-COLUNA-FHTM PIC S9(004) COMP    VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(050)     VALUE
           '*** AREA DA TELA DCOMFHTM  - OUTPUT ***'.
      *----------------------------------------------------------------*

       01  WRK-OUTPUT-FHTM.
           05  WRK-OUT-LL-FHTM             PIC S9(04) COMP VALUE +0348.
           05  WRK-OUT-ZZ-FHTM             PIC  9(04) COMP VALUE ZEROS.
           05  WRK-OUT-SENHAS-FHTM         PIC  X(37)      VALUE SPACES.
           05  WRK-OUT-COMANDO-ATTR-FHTM   PIC S9(04) COMP VALUE ZEROS.
           05  WRK-OUT-COMANDO-FHTM        PIC  X(68)      VALUE SPACES.
           05  WRK-OUT-TIMESTA-FHTM        PIC  X(26)      VALUE SPACES.
           05  WRK-OUT-FASE-FHTM           PIC  X(01)      VALUE SPACES.
           05  WRK-OUT-DADOS-FHTM.
               10  WRK-OUT-CCONV-ATTR-FHTM PIC S9(04) COMP VALUE ZEROS.
               10  WRK-OUT-CCONV-FHTM.
                   15  WRK-OUT-CCONV-FHTM-N
                                           PIC  9(009)     VALUE ZEROS.
               10  WRK-OUT-SLGER-ATTR-FHTM PIC S9(04) COMP VALUE ZEROS.
               10  WRK-OUT-SLGER-FHTM      PIC  X(001)     VALUE SPACES.
               10  WRK-OUT-SLCLI-ATTR-FHTM PIC S9(04) COMP VALUE ZEROS.
               10  WRK-OUT-SLCLI-FHTM      PIC  X(001)     VALUE SPACES.
               10  WRK-OUT-NCONV-FHTM      PIC  X(015)     VALUE SPACES.
               10  WRK-OUT-PROD-ATTR-FHTM PIC S9(04) COMP VALUE ZEROS.
               10  WRK-OUT-PROD-FHTM.
                   15  WRK-OUT-PROD-FHTM-N
                                           PIC  9(003)     VALUE ZEROS.
               10  WRK-OUT-DPROD-FHTM      PIC  X(012)     VALUE SPACES.
               10  WRK-OUT-CSPROD-ATTR-FHTM PIC S9(04) COMP VALUE ZEROS.
               10  WRK-OUT-SPROD-FHTM.
                   15  WRK-OUT-SPROD-FHTM-N
                                           PIC  9(003)     VALUE ZEROS.
               10  WRK-OUT-DSPROD-FHTM     PIC  X(010)     VALUE SPACES.
               10  WRK-OUT-CAGEN-ATTR-FHTM PIC S9(04) COMP VALUE ZEROS.
               10  WRK-OUT-CAGEN-FHTM.
                   15  WRK-OUT-CAGEN-FHTM-N
                                           PIC  9(005)     VALUE ZEROS.
               10  WRK-OUT-NAGEN-FHTM      PIC  X(040)     VALUE SPACES.
               10  WRK-OUT-SITUA-ATTR-FHTM PIC S9(04) COMP VALUE ZEROS.
               10  WRK-OUT-SITUA-FHTM.
                   15  WRK-OUT-SITUA-FHTM-N
                                           PIC  9(003)     VALUE ZEROS.
               10  WRK-OUT-DSITUA-FHTM     PIC  X(015)     VALUE SPACES.
           05  WRK-OUT-MENSA-FHTM          PIC  X(079)     VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(050)     VALUE
           '*** AREA DE COMUNICACAO PFK ***'.
      *----------------------------------------------------------------*

       01  WRK-COMU-AREA-PFK.
           05  WRK-COMU-LL-PFK             PIC S9(04) COMP VALUE +124.
           05  WRK-COMU-ZZ-PFK             PIC S9(04) COMP VALUE ZEROS.
           05  WRK-COMU-TRANCODE-PFK.
               10  WRK-COMU-TRANSACAO-PFK  PIC  X(008)     VALUE SPACES.
               10  FILLER                  PIC  X(006)     VALUE SPACES.
               10  WRK-COMU-PFK-PFK        PIC  X(001)     VALUE SPACES.
           05  WRK-COMU-SENHAS-PFK         PIC  X(037)     VALUE SPACES.
           05  WRK-COMU-COMANDO-PFK        PIC  X(068)     VALUE SPACES.

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
           '*** DCOM0608 - FIM DA AREA DE WORKING ***'.
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

           MOVE SPACES                     TO WRK-OUT-DADOS-FHTM
                                              WRK-OUT-SENHAS-FHTM
                                              WRK-OUT-COMANDO-FHTM.

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

           MOVE 'DCOMFHTM'                 TO WRK-TELA.

           IF  WRK-TRANSACAO           NOT EQUAL 'DCOM0608'
               PERFORM 2100-RECEBER-CONTROLE
           ELSE
               PERFORM 2200-PROCESSAR-DCOMFHTM
           END-IF.

           IF  WRK-TELA                    EQUAL 'DCOMFHTM'
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

           MOVE WRK-MENSAGEM               TO WRK-COMU-AREA

           IF   WRK-TRANSACAO              EQUAL 'DCOM3903'
                MOVE WRK-MENSAGEM          TO WRK-COMU-AREA-3903
                MOVE WRK-COMU-TIMESTA-3903 TO WRK-COMU-TIMESTAMP
           END-IF

           IF   WRK-TRANSACAO              EQUAL 'DCOM3904'
                MOVE WRK-MENSAGEM          TO WRK-COMU-AREA-3904
                MOVE WRK-COMU-TIMESTA-3904 TO WRK-COMU-TIMESTAMP
           END-IF

           IF   WRK-TRANSACAO              EQUAL 'DCOM3913'
                MOVE WRK-MENSAGEM          TO WRK-COMU-AREA-3913
                MOVE WRK-COMU-TIMESTA-3913 TO WRK-COMU-TIMESTAMP
           END-IF

           MOVE WRK-COMU-TIMESTAMP         TO WRK-INP-TIMESTA-FHTM
                                              WRK-OUT-TIMESTA-FHTM
                                              WRK-ENT-TIMESTA.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE 'CP'                       TO WRK-ENT-OPCAO

           PERFORM 8200-ACESSAR-DCOM7995

           MOVE WRK-SAI-DADOS              TO WRK-AREA-COMUN-TOTAL

           IF  WRK-TRANSACAO               EQUAL 'DCOM3903' OR
               WRK-TRANSACAO               EQUAL 'DCOM3904' OR
               WRK-TRANSACAO               EQUAL 'DCOM3913'
               IF  WRK-CONVENIO-GERAL          GREATER ZEROS
                   MOVE WRK-CONVENIO-GERAL     TO WRK-OUT-CCONV-FHTM-N
                   MOVE 'X'                    TO WRK-OUT-SLGER-FHTM
               END-IF
               IF  WRK-CONVENIO-CLIENTE        GREATER ZEROS
                   MOVE WRK-CONVENIO-CLIENTE   TO WRK-OUT-CCONV-FHTM-N
                   MOVE 'X'                    TO WRK-OUT-SLCLI-FHTM
               END-IF
               IF  WRK-AGENCIA                 GREATER ZEROS
                   MOVE WRK-AGENCIA            TO WRK-OUT-CAGEN-FHTM-N
                   MOVE WRK-DESC-AGENCIA       TO WRK-OUT-NAGEN-FHTM
               END-IF
               IF  WRK-PRODUTO-LISTA         GREATER ZEROS
                   MOVE WRK-PRODUTO-LISTA    TO WRK-INP-PROD-FHTM-N
                                                WRK-OUT-PROD-FHTM-N
                   PERFORM 7100-OBTER-NOME-PROD
               END-IF
               IF  WRK-SUBPRODUTO-LISTA      GREATER ZEROS
                   MOVE WRK-PRODUTO-LISTA    TO WRK-INP-PROD-FHTM-N
                                                WRK-OUT-PROD-FHTM-N
                   MOVE WRK-SUBPRODUTO-LISTA TO WRK-INP-SPROD-FHTM
                                                WRK-OUT-SPROD-FHTM
                   PERFORM 7200-OBTER-NOME-SUBP
               END-IF
               IF  WRK-SITUACAO-LISTA          GREATER ZEROS
                   MOVE WRK-SITUACAO-LISTA     TO WRK-INP-SITUA-FHTM
                                                  WRK-OUT-SITUA-FHTM
                   PERFORM 7300-OBTER-NOME-SITU
               END-IF
           ELSE
               IF  WRK-COMU-MENSAGEM         NOT EQUAL SPACES
                   MOVE WRK-COMU-MENSAGEM    TO  WRK-OUT-MENSA-FHTM
               ELSE
                   MOVE 'DIGITE OS DADOS E TECLE <ENTER>'
                                             TO WRK-OUT-MENSA-FHTM
               END-IF
           END-IF.

           IF   WRK-TRANSACAO                EQUAL 'DCOM3903'
                MOVE WRK-MENSAGEM            TO WRK-COMU-AREA-3903
                IF   WRK-COMU-COD-PROD-3903  GREATER ZEROS
                     MOVE WRK-COMU-COD-PROD-3903 TO WRK-OUT-PROD-FHTM-N
                     MOVE WRK-COMU-DES-PROD-3903 TO WRK-OUT-DPROD-FHTM
                     MOVE WRK-49361         TO WRK-OUT-CSPROD-ATTR-FHTM
                ELSE
                     MOVE SPACES             TO WRK-OUT-PROD-FHTM
                                                WRK-OUT-DPROD-FHTM
                END-IF
                MOVE WRK-COMU-MENSA-3903     TO WRK-OUT-MENSA-FHTM
           END-IF

           IF   WRK-TRANSACAO                EQUAL 'DCOM3904'
                MOVE WRK-MENSAGEM            TO WRK-COMU-AREA-3904
                IF   WRK-COMU-COD-SPROD-3904 GREATER ZEROS
                     MOVE WRK-COMU-COD-SPROD-3904 TO WRK-OUT-SPROD-FHTM
                     MOVE WRK-COMU-DES-SPROD-3904 TO WRK-OUT-DSPROD-FHTM
                     MOVE WRK-49361           TO WRK-OUT-CAGEN-ATTR-FHTM
                ELSE
                     MOVE SPACES             TO WRK-OUT-SPROD-FHTM
                                                WRK-OUT-DSPROD-FHTM
                END-IF
                MOVE WRK-COMU-MENSA-3904     TO WRK-OUT-MENSA-FHTM
           END-IF

           IF   WRK-TRANSACAO                 EQUAL 'DCOM3913'
                MOVE WRK-MENSAGEM             TO WRK-COMU-AREA-3913
                IF   WRK-COMU-COD-SITUA-3913  GREATER ZEROS
                     MOVE WRK-COMU-COD-SITUA-3913 TO WRK-OUT-SITUA-FHTM
                     MOVE WRK-COMU-DES-SITUA-3913 TO WRK-OUT-DSITUA-FHTM
                     MOVE WRK-49361           TO WRK-OUT-CCONV-ATTR-FHTM
                ELSE
                     MOVE SPACES              TO WRK-OUT-SITUA-FHTM
                                                 WRK-OUT-DSITUA-FHTM
                END-IF
           END-IF.

           PERFORM VARYING IND-1               FROM 1 BY 1
                     UNTIL IND-1               GREATER 15
                        OR WRK-CHAMADOR(IND-1) EQUAL SPACES
               IF WRK-CHAMADOR(IND-1)          EQUAL 'DCOM0608'
                  MOVE SPACES                  TO WRK-CHAMADOR(IND-1)
                  MOVE ZEROS                   TO WRK-PAGINA(IND-1)
                  MOVE 99                      TO IND-1
               END-IF
           END-PERFORM.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE WRK-INP-TIMESTA-FHTM       TO WRK-ENT-TIMESTA
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
      *     PROCESSAR DCOMFHTM                                         *
      *----------------------------------------------------------------*
       2200-PROCESSAR-DCOMFHTM             SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM               TO WRK-INPUT-FHTM.

           PERFORM 2210-DEVOLVER-TELA

           MOVE SPACES                     TO WRK-OUT-NCONV-FHTM
                                              WRK-OUT-DPROD-FHTM
                                              WRK-OUT-DSPROD-FHTM
                                              WRK-OUT-NAGEN-FHTM
                                              WRK-OUT-DSITUA-FHTM.

           EVALUATE TRUE

               WHEN WRK-INP-PFK-FHTM       EQUAL '1' OR
                   (WRK-INP-PFK-FHTM       EQUAL '.' AND
                    WRK-INP-COMANDO-FHTM   EQUAL 'PFK01')
                  IF  WRK-INP-FASE-FHTM    EQUAL '2'
                      MOVE WRK-49345       TO  WRK-OUT-COMANDO-ATTR-FHTM
                      MOVE WRK-241         TO  WRK-OUT-CCONV-ATTR-FHTM
                                               WRK-OUT-SLCLI-ATTR-FHTM
                                               WRK-OUT-SLGER-ATTR-FHTM
                                               WRK-OUT-CAGEN-ATTR-FHTM
                                               WRK-OUT-PROD-ATTR-FHTM
                                               WRK-OUT-CSPROD-ATTR-FHTM
                                               WRK-OUT-SITUA-ATTR-FHTM
                  END-IF
                  MOVE WRK-INP-MENSA-FHTM TO WRK-OUT-MENSA-FHTM

               WHEN WRK-INP-PFK-FHTM       EQUAL '3' OR
                   (WRK-INP-PFK-FHTM       EQUAL '.' AND
                    WRK-INP-COMANDO-FHTM   EQUAL 'PFK03')
                  PERFORM 2240-RETORNAR-CHAMADOR

               WHEN WRK-INP-PFK-FHTM       EQUAL 'X' OR
                   (WRK-INP-PFK-FHTM       EQUAL '.' AND
                    WRK-INP-COMANDO-FHTM   EQUAL 'PFK05')
                  PERFORM 2250-MENU-ROTINAS

               WHEN WRK-INP-PFK-FHTM       EQUAL 'Z' OR
                   (WRK-INP-PFK-FHTM       EQUAL '.' AND
                    WRK-INP-COMANDO-FHTM   EQUAL 'PFK10')
                  PERFORM 2260-MENU-DCOM

               WHEN (WRK-INP-PFK-FHTM      EQUAL '.' AND
                     WRK-INP-COMANDO-FHTM  EQUAL SPACES)
                  PERFORM 2270-TRATAR-ENTER

               WHEN WRK-INP-PFK-FHTM       EQUAL '9' OR
                   (WRK-INP-PFK-FHTM       EQUAL '.' AND
                    WRK-INP-COMANDO-FHTM   EQUAL 'PFK09')
                  PERFORM 2280-CHAMAR-LISTA

               WHEN WRK-INP-PFK-FHTM       EQUAL '6' OR
                   (WRK-INP-PFK-FHTM       EQUAL '.' AND
                    WRK-INP-COMANDO-FHTM   EQUAL 'PFK06') AND
                   WRK-INP-FASE-FHTM       EQUAL '2'
                  PERFORM 2290-TRANSFERIR-PGM

               WHEN OTHER
                  MOVE WRK-MENS02          TO  WRK-OUT-MENSA-FHTM
                  IF  WRK-INP-FASE-FHTM    EQUAL '2'
                      MOVE WRK-49345       TO  WRK-OUT-COMANDO-ATTR-FHTM
                      MOVE WRK-241         TO  WRK-OUT-CCONV-ATTR-FHTM
                                               WRK-OUT-SLCLI-ATTR-FHTM
                                               WRK-OUT-SLGER-ATTR-FHTM
                                               WRK-OUT-CAGEN-ATTR-FHTM
                                               WRK-OUT-PROD-ATTR-FHTM
                                               WRK-OUT-CSPROD-ATTR-FHTM
                                               WRK-OUT-SITUA-ATTR-FHTM
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

           IF  WRK-INP-CCONV-FHTM      NOT EQUAL SPACES AND LOW-VALUES
               INSPECT WRK-INP-CCONV-FHTM-N
                                       REPLACING LEADING ' ' BY '0'
           END-IF.

           IF  WRK-INP-CAGEN-FHTM      NOT EQUAL SPACES AND LOW-VALUES
               INSPECT WRK-INP-CAGEN-FHTM-N
                                       REPLACING LEADING ' ' BY '0'
           END-IF.

           IF  WRK-INP-PROD-FHTM       NOT EQUAL SPACES AND LOW-VALUES
               INSPECT WRK-INP-PROD-FHTM-N
                                       REPLACING LEADING ' ' BY '0'
           END-IF.

           IF  WRK-INP-SPROD-FHTM      NOT EQUAL SPACES AND LOW-VALUES
               INSPECT WRK-INP-SPROD-FHTM-N
                                       REPLACING LEADING ' ' BY '0'
           END-IF.

           IF  WRK-INP-SITUA-FHTM      NOT EQUAL SPACES AND LOW-VALUES
               INSPECT WRK-INP-SITUA-FHTM-N
                                       REPLACING LEADING ' ' BY '0'
           END-IF.

           MOVE  WRK-INP-CCONV-FHTM        TO WRK-OUT-CCONV-FHTM
           MOVE  WRK-INP-SLCLI-FHTM        TO WRK-OUT-SLCLI-FHTM
           MOVE  WRK-INP-SLGER-FHTM        TO WRK-OUT-SLGER-FHTM
           MOVE  WRK-INP-CAGEN-FHTM        TO WRK-OUT-CAGEN-FHTM
           MOVE  WRK-INP-NAGEN-FHTM        TO WRK-OUT-NAGEN-FHTM
           MOVE  WRK-INP-PROD-FHTM         TO WRK-OUT-PROD-FHTM
           MOVE  WRK-INP-DPROD-FHTM        TO WRK-OUT-DPROD-FHTM
           MOVE  WRK-INP-SPROD-FHTM        TO WRK-OUT-SPROD-FHTM
           MOVE  WRK-INP-DSPROD-FHTM       TO WRK-OUT-DSPROD-FHTM
           MOVE  WRK-INP-SITUA-FHTM        TO WRK-OUT-SITUA-FHTM
           MOVE  WRK-INP-DSITUA-FHTM       TO WRK-OUT-DSITUA-FHTM
           MOVE  WRK-INP-TIMESTA-FHTM      TO WRK-OUT-TIMESTA-FHTM
           MOVE  WRK-INP-FASE-FHTM         TO WRK-OUT-FASE-FHTM.

      *----------------------------------------------------------------*
       2210-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     RETORNAR AO PROGRAMA CHAMADOR                              *
      *----------------------------------------------------------------*
       2240-RETORNAR-CHAMADOR              SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-COMU-AREA

           MOVE WRK-INP-TIMESTA-FHTM       TO WRK-ENT-TIMESTA
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
           MOVE WRK-INP-TIMESTA-FHTM       TO WRK-ENT-TIMESTA
                                              WRK-COMU-TIMESTAMP
           MOVE 'I '                       TO WRK-ENT-OPCAO
           MOVE WRK-AREA-COMUN-TOTAL       TO WRK-ENT-DADOS

           PERFORM 8200-ACESSAR-DCOM7995.

           MOVE LENGTH WRK-COMU-AREA       TO WRK-COMU-LL
           MOVE WRK-CHNG                   TO WRK-FUNCAO
           MOVE 'DCOM0608'                 TO WRK-COMU-TRANSACAO
           MOVE '3'                        TO WRK-COMU-PFK
           MOVE WRK-COMU-AREA              TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2240-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     RETORNAR AO MENU ROTINAS                                   *
      *----------------------------------------------------------------*
       2250-MENU-ROTINAS                   SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-TIMESTA-FHTM       TO WRK-ENT-TIMESTA.
           MOVE 1                          TO WRK-ENT-PAGINA.
           MOVE 'ET'                       TO WRK-ENT-OPCAO.

           PERFORM 8200-ACESSAR-DCOM7995.

           INITIALIZE WRK-COMU-AREA-PFK
           MOVE LENGTH OF WRK-COMU-AREA-PFK TO WRK-COMU-LL-PFK
           MOVE WRK-CHNG                   TO WRK-FUNCAO
           MOVE 'DCOM0608'                 TO WRK-TELA
                                              WRK-COMU-TRANSACAO-PFK
           MOVE '5'                        TO WRK-COMU-PFK-PFK
           MOVE WRK-COMU-AREA-PFK          TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2250-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     RETORNAR AO MENU DCOM                                      *
      *----------------------------------------------------------------*
       2260-MENU-DCOM                      SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-TIMESTA-FHTM       TO WRK-ENT-TIMESTA.
           MOVE 1                          TO WRK-ENT-PAGINA.
           MOVE 'ET'                       TO WRK-ENT-OPCAO.

           PERFORM 8200-ACESSAR-DCOM7995.

           INITIALIZE WRK-COMU-AREA-PFK
           MOVE LENGTH OF WRK-COMU-AREA-PFK TO WRK-COMU-LL-PFK
           MOVE WRK-CHNG                   TO WRK-FUNCAO
           MOVE 'DCOM0608'                 TO WRK-TELA
                                              WRK-COMU-TRANSACAO-PFK
           MOVE 'A'                        TO WRK-COMU-PFK-PFK
           MOVE WRK-COMU-AREA-PFK          TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2260-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2270-TRATAR-ENTER                   SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                     TO  WRK-INCONSIS

           IF  WRK-OUT-FASE-FHTM           EQUAL '2'
               MOVE WRK-MENS01             TO  WRK-OUT-MENSA-FHTM
               MOVE '1'                    TO  WRK-OUT-FASE-FHTM
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

           MOVE WRK-MENS05                 TO  WRK-OUT-MENSA-FHTM
           MOVE WRK-49345                  TO  WRK-OUT-COMANDO-ATTR-FHTM
           MOVE WRK-241                    TO  WRK-OUT-CCONV-ATTR-FHTM
                                               WRK-OUT-SLCLI-ATTR-FHTM
                                               WRK-OUT-SLGER-ATTR-FHTM
                                               WRK-OUT-CAGEN-ATTR-FHTM
                                               WRK-OUT-PROD-ATTR-FHTM
                                               WRK-OUT-CSPROD-ATTR-FHTM
                                               WRK-OUT-SITUA-ATTR-FHTM
           MOVE '2'                        TO  WRK-OUT-FASE-FHTM.

      *----------------------------------------------------------------*
       2270-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2271-CONSISTIR-FISICA               SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INP-CCONV-FHTM          EQUAL SPACES OR LOW-VALUES
               MOVE WRK-49361              TO WRK-OUT-CCONV-ATTR-FHTM
               MOVE WRK-MENS06             TO WRK-OUT-MENSA-FHTM
               MOVE '*'                    TO WRK-INCONSIS
               GO TO 2271-99-FIM
           ELSE
               IF  WRK-INP-CCONV-FHTM      NOT NUMERIC
                   MOVE WRK-49369          TO WRK-OUT-CCONV-ATTR-FHTM
                   MOVE WRK-MENS07         TO WRK-OUT-MENSA-FHTM
                   MOVE '*'                TO WRK-INCONSIS
                   GO TO 2271-99-FIM
               END-IF
           END-IF.

           IF  WRK-INP-SLCLI-FHTM          EQUAL SPACES AND
               WRK-INP-SLGER-FHTM          EQUAL SPACES
               MOVE WRK-49361              TO WRK-OUT-SLGER-ATTR-FHTM
               MOVE WRK-MENS08             TO WRK-OUT-MENSA-FHTM
               MOVE '*'                    TO WRK-INCONSIS
               GO TO 2271-99-FIM
           END-IF.

           IF  WRK-INP-SLCLI-FHTM          NOT EQUAL SPACES AND
               WRK-INP-SLGER-FHTM          NOT EQUAL SPACES
               MOVE WRK-49369              TO WRK-OUT-SLGER-ATTR-FHTM
                                              WRK-OUT-SLCLI-ATTR-FHTM
               MOVE WRK-MENS08             TO WRK-OUT-MENSA-FHTM
               MOVE '*'                    TO WRK-INCONSIS
               GO TO 2271-99-FIM
           END-IF.

           IF  WRK-INP-SLGER-FHTM          NOT EQUAL SPACES AND
               WRK-INP-SLGER-FHTM          NOT EQUAL 'X'
               MOVE WRK-49369              TO WRK-OUT-SLGER-ATTR-FHTM
               MOVE WRK-MENS09             TO WRK-OUT-MENSA-FHTM
               MOVE '*'                    TO WRK-INCONSIS
               GO TO 2271-99-FIM
           END-IF.

           IF  WRK-INP-SLCLI-FHTM          NOT EQUAL SPACES AND
               WRK-INP-SLCLI-FHTM          NOT EQUAL 'X'
               MOVE WRK-49369              TO WRK-OUT-SLCLI-ATTR-FHTM
               MOVE WRK-MENS09             TO WRK-OUT-MENSA-FHTM
               MOVE '*'                    TO WRK-INCONSIS
               GO TO 2271-99-FIM
           END-IF.

           IF  WRK-INP-PROD-FHTM       NOT EQUAL SPACES AND LOW-VALUES
               IF  WRK-INP-PROD-FHTM   NOT NUMERIC  OR
                   WRK-INP-PROD-FHTM-N EQUAL ZEROS
                   MOVE WRK-49369      TO WRK-OUT-PROD-ATTR-FHTM
                   MOVE WRK-MENS10     TO WRK-OUT-MENSA-FHTM
                   MOVE '*'            TO WRK-INCONSIS
                   GO TO 2271-99-FIM
               END-IF
           END-IF.

           IF  WRK-INP-SPROD-FHTM      NOT EQUAL SPACES AND LOW-VALUES
               IF  WRK-INP-SPROD-FHTM  NOT NUMERIC  OR
                   WRK-INP-SPROD-FHTM-N
                                       EQUAL ZEROS
                   MOVE WRK-49369      TO WRK-OUT-CSPROD-ATTR-FHTM
                   MOVE WRK-MENS11     TO WRK-OUT-MENSA-FHTM
                   MOVE '*'            TO WRK-INCONSIS
                   GO TO 2271-99-FIM
               END-IF
           END-IF.

           IF  WRK-INP-PROD-FHTM       EQUAL SPACES OR  LOW-VALUES
               IF  WRK-INP-SPROD-FHTM  NOT EQUAL SPACES AND LOW-VALUES
                   MOVE WRK-49369      TO WRK-OUT-PROD-ATTR-FHTM
                                          WRK-OUT-CSPROD-ATTR-FHTM
                   MOVE 'CODIGOS DO PRODUTO E SUBPRODUTO SAO OBRIGATORIO
      -                 'S'            TO WRK-OUT-MENSA-FHTM
                   MOVE '*'            TO WRK-INCONSIS
                   GO TO 2271-99-FIM
               END-IF
           END-IF.

           IF  WRK-INP-CAGEN-FHTM      NOT NUMERIC
               MOVE WRK-49369          TO WRK-OUT-CAGEN-ATTR-FHTM
               MOVE WRK-MENS12         TO WRK-OUT-MENSA-FHTM
               MOVE '*'                TO WRK-INCONSIS
               GO TO 2271-99-FIM
           END-IF.

           IF  WRK-INP-SITUA-FHTM      NOT EQUAL SPACES AND LOW-VALUES
               IF  WRK-INP-SITUA-FHTM  NOT NUMERIC OR
                   WRK-INP-SITUA-FHTM-N
                                       EQUAL ZEROS
                   MOVE WRK-49369      TO WRK-OUT-SITUA-ATTR-FHTM
                   MOVE WRK-MENS13     TO WRK-OUT-MENSA-FHTM
                   MOVE '*'            TO WRK-INCONSIS
                   GO TO 2271-99-FIM
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2271-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2272-OBTER-NOMES                SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INP-CAGEN-FHTM      NOT EQUAL SPACES AND LOW-VALUES
               PERFORM 7000-OBTER-NOME-AGEN
           ELSE
               MOVE SPACES             TO  WRK-OUT-NAGEN-FHTM
           END-IF

           IF  WRK-INP-PROD-FHTM       NOT EQUAL SPACES AND LOW-VALUES
               PERFORM 7100-OBTER-NOME-PROD
           ELSE
               MOVE SPACES             TO  WRK-OUT-DPROD-FHTM
           END-IF

           IF  WRK-INP-SPROD-FHTM      NOT EQUAL SPACES AND LOW-VALUES
               PERFORM 7200-OBTER-NOME-SUBP
           ELSE
               MOVE SPACES             TO  WRK-OUT-DSPROD-FHTM
           END-IF

           IF  WRK-INP-SITUA-FHTM      NOT EQUAL SPACES AND LOW-VALUES
               PERFORM 7300-OBTER-NOME-SITU
           ELSE
               MOVE SPACES             TO  WRK-OUT-DSITUA-FHTM
           END-IF.

      *----------------------------------------------------------------*
       2272-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2280-CHAMAR-LISTA                   SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INP-LINHA-FHTM          EQUAL 12 AND
              (WRK-INP-COLUNA-FHTM         EQUAL 23 OR 24 OR 25)
               PERFORM 8100-OBTER-LISTA-PROD
           ELSE
               IF  WRK-INP-LINHA-FHTM      EQUAL 14 AND
                  (WRK-INP-COLUNA-FHTM     EQUAL 23 OR 24 OR 25)
                   PERFORM 8200-OBTER-LISTA-SUBP
               ELSE
                   IF  WRK-INP-LINHA-FHTM  EQUAL 18 AND
                      (WRK-INP-COLUNA-FHTM EQUAL 23 OR 24 OR 25)
                       PERFORM 8300-OBTER-LISTA-SITU
                   ELSE
                       MOVE 'LISTA DISPONIVEL PARA PRODUTO/SUBPROD/SITUA
      -                     'CAO - POSICIONAMENTO ERRADO'
                                           TO WRK-OUT-MENSA-FHTM
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
                      WRK-COMU-AREA.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE WRK-INP-TIMESTA-FHTM       TO WRK-ENT-TIMESTA
                                              WRK-COMU-TIMESTAMP

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE 'CP'                       TO WRK-ENT-OPCAO

           PERFORM 8200-ACESSAR-DCOM7995
           MOVE WRK-SAI-DADOS              TO WRK-AREA-COMUN-TOTAL.

           MOVE 'I '                       TO WRK-ENT-OPCAO

           PERFORM VARYING IND-1               FROM 1 BY 1
                     UNTIL IND-1               GREATER 15


               IF  WRK-CHAMADOR(IND-1)         EQUAL SPACES
                   MOVE 'DCOM0608'             TO WRK-CHAMADOR(IND-1)
                                                  WRK-COMU-TRANSACAO
                   MOVE 99                     TO IND-1
               END-IF
           END-PERFORM.

           IF  WRK-INP-CAGEN-FHTM          NOT EQUAL SPACES
               MOVE WRK-INP-CAGEN-FHTM     TO WRK-AGENCIA-LISTA
           ELSE
               MOVE ZEROS                  TO WRK-AGENCIA-LISTA
           END-IF.

           IF  WRK-INP-SLCLI-FHTM            EQUAL 'X'
               IF  WRK-INP-CCONV-FHTM-N      GREATER ZEROS
                   MOVE WRK-INP-CCONV-FHTM-N TO WRK-CONVENIO-CLIENTE
                   MOVE WRK-INP-NCONV-FHTM   TO WRK-DESC-CONVE
               END-IF
           ELSE
               IF  WRK-INP-CCONV-FHTM-N      GREATER ZEROS
                   MOVE WRK-INP-CCONV-FHTM-N TO WRK-CONVENIO-GERAL
                   MOVE WRK-INP-NCONV-FHTM   TO WRK-DESC-CONVE
               END-IF
           END-IF.

           IF  WRK-INP-PROD-FHTM           NOT EQUAL SPACES
               MOVE WRK-INP-PROD-FHTM      TO WRK-PRODUTO-LISTA
           ELSE
               MOVE ZEROS                  TO WRK-PRODUTO-LISTA
           END-IF.

           IF  WRK-INP-SPROD-FHTM          NOT EQUAL SPACES
               MOVE WRK-INP-SPROD-FHTM     TO WRK-SUBPRODUTO-LISTA
           ELSE
               MOVE ZEROS                  TO WRK-SUBPRODUTO-LISTA
           END-IF.

           IF  WRK-INP-SITUA-FHTM          NOT EQUAL SPACES
               MOVE WRK-INP-SITUA-FHTM     TO WRK-SITUACAO-LISTA
           ELSE
               MOVE ZEROS                  TO WRK-SITUACAO-LISTA
           END-IF.

           MOVE WRK-AREA-COMUN-TOTAL       TO WRK-ENT-DADOS.
           PERFORM 8200-ACESSAR-DCOM7995

           MOVE LENGTH OF WRK-COMU-AREA    TO WRK-COMU-LL

           MOVE  'DCOM0608'                TO WRK-COMU-TRANSACAO
           MOVE  'DCOM0609'                TO WRK-TELA
           MOVE  WRK-COMU-AREA             TO WRK-MENSAGEM
           MOVE  WRK-CHNG                  TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       2290-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2300-PROCESSAR-BRAD0660             SECTION.
      *----------------------------------------------------------------*

           MOVE LENGTH OF WRK-OUTPUT-FHTM
                                       TO  WRK-660-LL-MENSAGEM-FHTM
                                           WRK-OUT-LL-FHTM

           MOVE LENGTH OF WRK-660-DCOMFHTM
                                       TO  WRK-660-LL-AREA-FHTM

           CALL 'BRAD0660'             USING  WRK-OUTPUT-FHTM
                                              WRK-660-DCOMFHTM.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'                  TO ERR-TIPO-ACESSO
               MOVE RETURN-CODE            TO WRK-RETURN-CODE
               MOVE 10                     TO WRK-LOCAL-ERRO
               MOVE WRK-ERRO-BRAD0660      TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-ISRT                   TO WRK-FUNCAO
           MOVE WRK-OUTPUT-FHTM            TO WRK-MENSAGEM.

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
           MOVE WRK-INP-CAGEN-FHTM-N       TO  ENT6416-DEPDC
           MOVE 'DCOM6416'                 TO  WRK-MODULO
           CALL WRK-MODULO              USING  ENTRADA-ROT6416
                                               SAIDA-ROT6416
                                               ERRO-AREA
                                               WRK-SQLCA

           EVALUATE SAI6416-COD-RETORNO
             WHEN '0000'
               MOVE SAI6416-NOME-DEPDC     TO  WRK-OUT-NAGEN-FHTM

             WHEN '0099'
               MOVE 'DB2'                  TO  ERR-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO

             WHEN OTHER
               MOVE 'AGENCIA INEXISTENTE'  TO WRK-OUT-MENSA-FHTM
               MOVE '*'                    TO WRK-INCONSIS
               MOVE WRK-49369              TO WRK-OUT-CAGEN-ATTR-FHTM

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
           MOVE WRK-INP-PROD-FHTM-N        TO  ENT4001-CPRODT
           MOVE WRK-INP-TIMESTA-FHTM(9:2)  TO  ENT4001-DT-MOVTO(1:2)
           MOVE WRK-INP-TIMESTA-FHTM(6:2)  TO  ENT4001-DT-MOVTO(4:2)
           MOVE WRK-INP-TIMESTA-FHTM(1:4)  TO  ENT4001-DT-MOVTO(7:4)

           INSPECT ENT4001-DT-MOVTO REPLACING ALL ' ' BY '.'.

           MOVE 'DCOM4001'                 TO  WRK-MODULO
           CALL WRK-MODULO              USING  ENTRADA-ROT4001
                                               SAIDA-ROT4001
                                               ERRO-AREA
                                               WRK-SQLCA

           EVALUATE SAI4001-COD-RETORNO
             WHEN '0000'
               MOVE SAI4001-IABREV-PRODT   TO  WRK-OUT-DPROD-FHTM

             WHEN '0099'
               MOVE 'DB2'                  TO  ERR-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO

             WHEN OTHER
               MOVE 'PRODUTO INEXISTENTE'  TO WRK-OUT-MENSA-FHTM
               MOVE '*'                    TO  WRK-INCONSIS
               MOVE WRK-49369              TO WRK-OUT-PROD-ATTR-FHTM

           END-EVALUATE.

      *----------------------------------------------------------------*
       7100-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       7200-OBTER-NOME-SUBP                SECTION.
      *----------------------------------------------------------------*

           INITIALIZE ENTRADA-ROT4049
                      SAIDA-ROT4049

           MOVE WRK-COD-USER               TO  WRK-4049E-CFUNC-BDSCO
           MOVE LNK-IO-LTERM               TO  WRK-4049E-CTERM
           MOVE WRK-INP-PROD-FHTM-N        TO  WRK-4049E-CPRODT
           MOVE WRK-INP-SPROD-FHTM-N       TO WRK-4049E-CSPROD-DESC-COML
           MOVE 'DCOM4049'                 TO  WRK-MODULO
           CALL WRK-MODULO              USING  ENTRADA-ROT4049
                                               SAIDA-ROT4049
                                               ERRO-AREA
                                               WRK-SQLCA

           EVALUATE WRK-4049S-COD-RETORNO
             WHEN '0000'
               MOVE WRK-4049S-ISPROD-DESC-COML
                                           TO  WRK-OUT-DSPROD-FHTM

             WHEN '0099'
               MOVE 'DB2'                  TO  ERR-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO

             WHEN OTHER
               MOVE 'SUBPRODUTO INEXISTENTE'
                                           TO WRK-OUT-MENSA-FHTM
               MOVE '*'                    TO  WRK-INCONSIS
               MOVE WRK-49369              TO WRK-OUT-CSPROD-ATTR-FHTM

           END-EVALUATE

           .
      *----------------------------------------------------------------*
       7200-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       7300-OBTER-NOME-SITU                SECTION.
      *----------------------------------------------------------------*

           INITIALIZE ENTRADA-ROT4309
                      SAIDA-ROT4309

           MOVE WRK-COD-USER               TO WRK-4309E-CFUNC-BDSCO
           MOVE LNK-IO-LTERM               TO WRK-4309E-CTERM
           MOVE 6                          TO WRK-4309E-CELMTO-DESC-COML
           MOVE WRK-INP-SITUA-FHTM-N       TO WRK-4309E-CSIT-DESC-COML
           MOVE 'DCOM4309'                 TO WRK-MODULO
           CALL WRK-MODULO              USING ENTRADA-ROT4309
                                              SAIDA-ROT4309
                                              ERRO-AREA
                                              WRK-SQLCA

           EVALUATE WRK-4309S-COD-RETORNO
             WHEN '0000'
               MOVE WRK-4309S-IRSUMO-SIT-DESC
                                           TO  WRK-OUT-DSITUA-FHTM

             WHEN '0099'
               MOVE 'DB2'                  TO  ERR-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO

             WHEN OTHER
               MOVE 'SITUACAO INEXISTENTE'
                                           TO WRK-OUT-MENSA-FHTM
               MOVE '*'                    TO WRK-INCONSIS
               MOVE WRK-49369              TO WRK-OUT-SITUA-ATTR-FHTM

           END-EVALUATE.

      *----------------------------------------------------------------*
       7300-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       8100-OBTER-LISTA-PROD               SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-AREA-COMUN-TOTAL
                      WRK-COMU-AREA-3903.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE WRK-INP-TIMESTA-FHTM       TO WRK-ENT-TIMESTA
                                              WRK-COMU-TIMESTAMP

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE 'CP'                       TO WRK-ENT-OPCAO

           PERFORM 8200-ACESSAR-DCOM7995
           MOVE WRK-SAI-DADOS              TO WRK-AREA-COMUN-TOTAL.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE WRK-INP-TIMESTA-FHTM       TO WRK-ENT-TIMESTA
                                              WRK-COMU-TIMESTAMP
           MOVE 'EP'                       TO WRK-ENT-OPCAO

           PERFORM 8200-ACESSAR-DCOM7995
           MOVE LENGTH OF WRK-COMU-AREA-3903 TO WRK-COMU-LL-3903
           MOVE WRK-INP-TIMESTA-FHTM       TO WRK-COMU-TIMESTA-3903
           MOVE WRK-INP-PROD-FHTM          TO WRK-COMU-COD-PROD-3903
           MOVE 'DCOM0608'                 TO WRK-COMU-TRANSACAO-3903
           MOVE 'DCOM3903'                 TO WRK-TELA
           MOVE  WRK-CHNG                  TO WRK-FUNCAO.

           PERFORM VARYING IND-1               FROM 1 BY 1
                     UNTIL IND-1               GREATER 15


               IF  WRK-CHAMADOR(IND-1)         EQUAL SPACES
                   MOVE 'DCOM0608'             TO WRK-CHAMADOR(IND-1)
                                                  WRK-COMU-TRANSACAO
                   MOVE 99                     TO IND-1
               END-IF
           END-PERFORM.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE 'I '                       TO WRK-ENT-OPCAO

           IF  WRK-INP-CAGEN-FHTM          NOT EQUAL SPACES
               MOVE WRK-INP-CAGEN-FHTM     TO WRK-AGENCIA
               MOVE WRK-INP-NAGEN-FHTM     TO WRK-DESC-AGENCIA
           ELSE
               MOVE ZEROS                  TO WRK-AGENCIA
               MOVE SPACES                 TO WRK-DESC-AGENCIA
           END-IF.

           IF  WRK-INP-SLCLI-FHTM            EQUAL 'X'
               IF  WRK-INP-CCONV-FHTM-N      GREATER ZEROS
                   MOVE WRK-INP-CCONV-FHTM-N TO WRK-CONVENIO-CLIENTE
               ELSE
                   MOVE ZEROS                TO WRK-CONVENIO-CLIENTE
               END-IF
           ELSE
               IF  WRK-INP-CCONV-FHTM-N      GREATER ZEROS
                   MOVE WRK-INP-CCONV-FHTM-N TO WRK-CONVENIO-GERAL
               ELSE
                   MOVE ZEROS                TO WRK-CONVENIO-GERAL
               END-IF
           END-IF.

           IF  WRK-INP-PROD-FHTM           NOT EQUAL SPACES
               MOVE WRK-INP-PROD-FHTM      TO WRK-PRODUTO-LISTA
               MOVE WRK-INP-DPROD-FHTM     TO WRK-DESC-PRODUTO
           ELSE
               MOVE ZEROS                  TO WRK-PRODUTO-LISTA
               MOVE SPACES                 TO WRK-DESC-PRODUTO
           END-IF.

           IF  WRK-INP-SPROD-FHTM          NOT EQUAL SPACES
               MOVE WRK-INP-SPROD-FHTM     TO WRK-SUBPRODUTO-LISTA
               MOVE WRK-INP-DSPROD-FHTM    TO WRK-DESC-SPROD
           ELSE
               MOVE ZEROS                  TO WRK-SUBPRODUTO-LISTA
               MOVE SPACES                 TO WRK-DESC-SPROD
           END-IF.

           IF  WRK-INP-SITUA-FHTM          NOT EQUAL SPACES
               MOVE WRK-INP-SITUA-FHTM     TO WRK-SITUACAO-LISTA
               MOVE WRK-INP-DSITUA-FHTM    TO WRK-DESC-SITU
           ELSE
               MOVE ZEROS                  TO WRK-SITUACAO-LISTA
               MOVE SPACES                 TO WRK-DESC-SITU
           END-IF.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE 'I '                       TO WRK-ENT-OPCAO
           MOVE WRK-AREA-COMUN-TOTAL       TO WRK-ENT-DADOS.
           PERFORM 8200-ACESSAR-DCOM7995.

           MOVE  WRK-COMU-AREA-3903        TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       8100-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       8200-OBTER-LISTA-SUBP               SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INP-PROD-FHTM           EQUAL SPACES
               MOVE 'LISTA DE SUBPRODUTO OBRIGATORIO INFORMAR O PRODUTO'
                                         TO WRK-OUT-MENSA-FHTM
               MOVE WRK-49369            TO WRK-OUT-CSPROD-ATTR-FHTM
               MOVE '*'                  TO WRK-INCONSIS
               GO TO 8200-99-FIM
           END-IF.

           INITIALIZE WRK-AREA-COMUN-TOTAL
                      WRK-COMU-AREA-3904.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE WRK-INP-TIMESTA-FHTM       TO WRK-ENT-TIMESTA
                                              WRK-COMU-TIMESTAMP

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE 'CP'                       TO WRK-ENT-OPCAO

           PERFORM 8200-ACESSAR-DCOM7995
           MOVE WRK-SAI-DADOS              TO WRK-AREA-COMUN-TOTAL.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE WRK-INP-TIMESTA-FHTM       TO WRK-ENT-TIMESTA
                                              WRK-COMU-TIMESTAMP
           MOVE 'EP'                       TO WRK-ENT-OPCAO

           PERFORM 8200-ACESSAR-DCOM7995
           MOVE LENGTH OF WRK-COMU-AREA-3904 TO WRK-COMU-LL-3904

           MOVE WRK-INP-TIMESTA-FHTM       TO WRK-COMU-TIMESTA-3904
           MOVE WRK-INP-PROD-FHTM          TO WRK-COMU-COD-PROD-3904
           MOVE WRK-INP-SPROD-FHTM         TO WRK-COMU-COD-SPROD-3904
           MOVE ZEROS                      TO WRK-COMU-CNPJCPF-3904
           MOVE 'DCOM0608'                 TO WRK-COMU-TRANSACAO-3904
           MOVE 'DCOM3904'                 TO WRK-TELA
           MOVE  WRK-COMU-AREA-3904        TO WRK-MENSAGEM
           MOVE  WRK-CHNG                  TO WRK-FUNCAO.

           PERFORM VARYING IND-1               FROM 1 BY 1
                     UNTIL IND-1               GREATER 15

               IF  WRK-CHAMADOR(IND-1)         EQUAL SPACES
                   MOVE 'DCOM0608'             TO WRK-CHAMADOR(IND-1)
                                                  WRK-COMU-TRANSACAO
                   MOVE 99                     TO IND-1
               END-IF
           END-PERFORM.

           IF  WRK-INP-CAGEN-FHTM          NOT EQUAL SPACES
               MOVE WRK-INP-CAGEN-FHTM     TO WRK-AGENCIA
               MOVE WRK-INP-NAGEN-FHTM     TO WRK-DESC-AGENCIA
           ELSE
               MOVE ZEROS                  TO WRK-AGENCIA
               MOVE SPACES                 TO WRK-DESC-AGENCIA
           END-IF.

           IF  WRK-INP-SLCLI-FHTM            EQUAL 'X'
               IF  WRK-INP-CCONV-FHTM-N      GREATER ZEROS
                   MOVE WRK-INP-CCONV-FHTM-N TO WRK-CONVENIO-CLIENTE
               ELSE
                   MOVE ZEROS                TO WRK-CONVENIO-CLIENTE
               END-IF
           ELSE
               IF  WRK-INP-CCONV-FHTM-N      GREATER ZEROS
                   MOVE WRK-INP-CCONV-FHTM-N TO WRK-CONVENIO-GERAL
               ELSE
                   MOVE ZEROS                TO WRK-CONVENIO-GERAL
               END-IF
           END-IF.

           IF  WRK-INP-PROD-FHTM           NOT EQUAL SPACES
               MOVE WRK-INP-PROD-FHTM      TO WRK-PRODUTO-LISTA
           ELSE
               MOVE ZEROS                  TO WRK-PRODUTO-LISTA
           END-IF.

           IF  WRK-INP-SPROD-FHTM          NOT EQUAL SPACES
               MOVE WRK-INP-SPROD-FHTM     TO WRK-SUBPRODUTO-LISTA
           ELSE
               MOVE ZEROS                  TO WRK-SUBPRODUTO-LISTA
           END-IF.

           IF  WRK-INP-SITUA-FHTM          NOT EQUAL SPACES
               MOVE WRK-INP-SITUA-FHTM     TO WRK-SITUACAO-LISTA
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
           MOVE WRK-INP-TIMESTA-FHTM       TO WRK-ENT-TIMESTA
                                              WRK-COMU-TIMESTAMP

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE 'CP'                       TO WRK-ENT-OPCAO

           PERFORM 8200-ACESSAR-DCOM7995
           MOVE WRK-SAI-DADOS              TO WRK-AREA-COMUN-TOTAL.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE WRK-INP-TIMESTA-FHTM       TO WRK-ENT-TIMESTA
                                              WRK-COMU-TIMESTAMP
           MOVE 'EP'                       TO WRK-ENT-OPCAO

           PERFORM 8200-ACESSAR-DCOM7995
           INITIALIZE WRK-COMU-AREA-3913
           MOVE LENGTH OF WRK-COMU-AREA-3913 TO WRK-COMU-LL-3913
           MOVE WRK-INP-TIMESTA-FHTM       TO WRK-COMU-TIMESTA-3913

           MOVE ZEROS                      TO WRK-COMU-CSIT-DESC-3913
           MOVE 006                        TO WRK-COMU-CELMTO-DESC-3913
           MOVE 'DCOM0608'                 TO WRK-COMU-TRANSACAO-3913
           MOVE 'DCOM3913'                 TO WRK-TELA
           MOVE  WRK-COMU-AREA-3913        TO WRK-MENSAGEM
           MOVE  WRK-CHNG                  TO WRK-FUNCAO.

           PERFORM VARYING IND-1               FROM 1 BY 1
                     UNTIL IND-1               GREATER 15


               IF  WRK-CHAMADOR(IND-1)         EQUAL SPACES
                   MOVE 'DCOM0608'             TO WRK-CHAMADOR(IND-1)
                                                  WRK-COMU-TRANSACAO
                   MOVE 99                     TO IND-1
               END-IF
           END-PERFORM.

           IF  WRK-INP-CAGEN-FHTM          NOT EQUAL SPACES
               MOVE WRK-INP-CAGEN-FHTM     TO WRK-AGENCIA
               MOVE WRK-INP-NAGEN-FHTM     TO WRK-DESC-AGENCIA
           ELSE
               MOVE ZEROS                  TO WRK-AGENCIA
               MOVE SPACES                 TO WRK-DESC-AGENCIA
           END-IF.

           IF  WRK-INP-SLCLI-FHTM            EQUAL 'X'
               IF  WRK-INP-CCONV-FHTM-N      GREATER ZEROS
                   MOVE WRK-INP-CCONV-FHTM-N TO WRK-CONVENIO-CLIENTE
               ELSE
                   MOVE ZEROS                TO WRK-CONVENIO-CLIENTE
               END-IF
           ELSE
               IF  WRK-INP-CCONV-FHTM-N      GREATER ZEROS
                   MOVE WRK-INP-CCONV-FHTM-N TO WRK-CONVENIO-GERAL
               ELSE
                   MOVE ZEROS                TO WRK-CONVENIO-GERAL
               END-IF
           END-IF.

           IF  WRK-INP-PROD-FHTM           NOT EQUAL SPACES
               MOVE WRK-INP-PROD-FHTM      TO WRK-PRODUTO-LISTA
           ELSE
               MOVE ZEROS                  TO WRK-PRODUTO-LISTA
           END-IF.

           IF  WRK-INP-SPROD-FHTM          NOT EQUAL SPACES
               MOVE WRK-INP-SPROD-FHTM     TO WRK-SUBPRODUTO-LISTA
           ELSE
               MOVE ZEROS                  TO WRK-SUBPRODUTO-LISTA
           END-IF.

           IF  WRK-INP-SITUA-FHTM          NOT EQUAL SPACES
               MOVE WRK-INP-SITUA-FHTM     TO WRK-SITUACAO-LISTA
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
               MOVE 'DCOM0608'             TO ERR-PGM
           ELSE
               MOVE 'SENHAS02'             TO ERR-PGM
               MOVE 'DCOM0608'             TO ERR-MODULO
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
