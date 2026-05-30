      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
      *
       PROGRAM-ID.     DCOM0603.
       AUTHOR.         FABRICA - ALTRAN.
      *================================================================*
      *                          A L T R A N                           *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   DCOM0603                                    *
      *    PROGRAMADOR.:   FABRICA - ALTRAN                            *
      *    ANALISTA....:   EMERSON ARTUR FRANCO                        *
      *    DATA........:   21/05/2007                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   CONSULTA OPERACOES - POR AGENCIA            *
      *----------------------------------------------------------------*
      *    TELAS.......:                                               *
      *    DCOMFCTM - CONSULTA OPERACOES - POR AGENCIA                 *
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
      *    CHAMADO POR.: DCOM0602           CHAMA: DCOM0604            *
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
           '*** DCOM0603 - INICIO DA AREA DE WORKING ***'.
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
           05  WRK-DATA-INICIAL            PIC  9(008)     VALUE ZEROS.
           05  WRK-DATA-FINAL              PIC  9(008)     VALUE ZEROS.
           05  WRK-DATA-ATUAL              PIC  9(008)     VALUE ZEROS.
           05  WRK-DATA-AUX-NUM            PIC  9(008)     VALUE ZEROS.
           05  FILLER                      REDEFINES WRK-DATA-AUX-NUM.
               10   WRK-ANO-AUX            PIC  9(004).
               10   WRK-MES-AUX            PIC  9(002).
               10   WRK-DIA-AUX            PIC  9(002).

           05  WRK-TIMESTAMP-ATU.
             10  WRK-TMS-AAAA               PIC  9(004).
             10  FILLER                     PIC  X(001).
             10  WRK-TMS-MM                 PIC  9(002).
             10  FILLER                     PIC  X(001).
             10  WRK-TMS-DD                 PIC  9(002).
             10  WRK-TMS-HORAS              PIC  X(016).

           05  WRK-MENS01                  PIC  X(072)     VALUE
               'DIGITE A OPCAO DESEJADA'.
           05  WRK-MENS02                  PIC  X(072)     VALUE
               'PF INVALIDA'.
           05  WRK-MENS03                  PIC  X(072)     VALUE
               'OPCAO INVALIDA'.
           05  WRK-MENS04                  PIC  X(072)     VALUE
               'AGENCIA OBRIGATORIA'.
           05  WRK-MENS05                  PIC  X(072)     VALUE
               'TECLE <PF6> PARA CONFIRMAR OU <ENTER> PARA ALTERAR'.
           05  WRK-MENS06                  PIC  X(072)     VALUE
               'AGENCIA INVALIDA'.

      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(079)     VALUE
           '*** AREA DE VALIDACAO DE PERFIL ***'.
      *----------------------------------------------------------------*

       COPY 'I#DCOMXE'.

      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(050)     VALUE
           '*** AREA DE COMUNICACAO ***'.
      *----------------------------------------------------------------*

       COPY 'I#DCOMJ2'.

      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(50)      VALUE
           '*** AREA DE COMUNICACAO DCOM0602 / DCOM6204 ***'.
      *----------------------------------------------------------------*
      *
       01          COMU-DADOS.
         05        COMU-LL                 PIC S9(04) COMP VALUE +406.
         05        COMU-ZZ                 PIC S9(04) COMP VALUE ZEROS.
         05        COMU-TRANCODE.
            10     COMU-TRANSACAO          PIC X(008)      VALUE SPACES.
            10     FILLER                  PIC X(006)      VALUE SPACES.
            10     COMU-PFK                PIC X(001)      VALUE SPACES.
         05        COMU-SENHAS             PIC X(037)      VALUE SPACES.
         05        COMU-COMANDO            PIC X(068)      VALUE SPACES.
         05        COMU-TIMESTAMP          PIC X(026)      VALUE SPACES.
         05        COMU-MENSAGEM           PIC X(079)      VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DO MODULO DCOM7995 **'.
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

      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(50)      VALUE
           '*** AREA DE COMUNICACAO DCOM6416 ***'.
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
               10  COMU-TIMESTAMP-3903     PIC  X(026)     VALUE SPACES.
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
               10  COMU-TIMESTAMP-3904     PIC  X(026)     VALUE SPACES.
               10  COMU-CNPJCPF-3904       PIC  X(017)     VALUE SPACES.
               10  COMU-COD-PROD-X-3904.
                   15  COMU-COD-PROD-3904  PIC  9(003)     VALUE ZEROS.
               10  COMU-COD-SUBP-X-3904.
                   15  COMU-COD-SUBP-3904  PIC  9(003)     VALUE ZEROS.
               10  COMU-DES-SUBP-3904      PIC  X(040)     VALUE SPACES.
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
               10  COMU-TIMESTAMP-3913     PIC  X(026)     VALUE SPACES.
               10  COMU-TPOPCAO-3913       PIC  X(001)     VALUE SPACES.
               10  COMU-CELMTO-DESC-3913   PIC  9(003)     VALUE ZEROS.
               10  COMU-CSIT-DESC-3913     PIC  9(003)     VALUE ZEROS.
               10  COMU-COD-SITUA-3913     PIC  9(003)     VALUE ZEROS.
               10  COMU-DES-SITUA-3913     PIC  X(020)     VALUE SPACES.
               10  COMU-MENSAGEM-3913      PIC  X(079)     VALUE SPACES.

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
           '*** AREA DA TELA DCOMFCTM - INPUT ***'.
      *----------------------------------------------------------------*
       01  WRK-INPUT-FCTM.
           05  FILLER                      PIC  X(018)     VALUE SPACES.
           05  WRK-INP-PFK-FCTM            PIC  X(001)     VALUE SPACES.
           05  WRK-INP-SENHAS-FCTM         PIC  X(037)     VALUE SPACES.
           05  WRK-INP-COMANDO-FCTM        PIC  X(068)     VALUE SPACES.
           05  WRK-INP-TIMESTAMP-FCTM      PIC  X(026)     VALUE SPACES.
           05  WRK-INP-FASE-FCTM           PIC  X(001)     VALUE SPACES.
           05  WRK-INP-DADOS-FCTM.
               10  WRK-INP-BANCO-FCTM.
                   15  WRK-INP-BANCO-FCTM-N
                                           PIC  9(003)     VALUE ZEROS.
               10  WRK-INP-CAGEN-FCTM.
                   15  WRK-INP-CAGEN-FCTM-N
                                           PIC  9(005)     VALUE ZEROS.
               10  WRK-INP-NAGEN-FCTM.
                   15  WRK-INP-NAGEN-FCTM-N
                                           PIC  X(040)     VALUE ZEROS.
               10  WRK-INP-CPROD-FCTM.
                   15  WRK-INP-CPROD-FCTM-N
                                           PIC  9(003)     VALUE ZEROS.
               10  WRK-INP-NPROD-FCTM.
                   15  WRK-INP-NPROD-FCTM-N
                                           PIC  X(030)     VALUE ZEROS.
               10  WRK-INP-CSUBP-FCTM.
                   15  WRK-INP-CSUBP-FCTM-N
                                           PIC  9(003)     VALUE ZEROS.
               10  WRK-INP-NSUBP-FCTM.
                   15  WRK-INP-NSUBP-FCTM-N
                                           PIC  X(030)     VALUE ZEROS.
               10  WRK-INP-CSITU-FCTM.
                   15  WRK-INP-CSITU-FCTM-N
                                           PIC  9(003)     VALUE ZEROS.
               10  WRK-INP-NSITU-FCTM.
                   15  WRK-INP-NSITU-FCTM-N
                                           PIC  X(030)     VALUE SPACES.
               10  WRK-INP-DT-INICIAL-FCTM.
                   15  WRK-INP-DIAINI-FCTM.
                       20  WRK-INP-DIAINI-FCTM-N
                                           PIC  9(002)     VALUE ZEROS.
                   15  WRK-INP-MESINI-FCTM.
                       20  WRK-INP-MESINI-FCTM-N
                                           PIC  9(002)     VALUE ZEROS.
                   15  WRK-INP-ANOINI-FCTM.
                       20  WRK-INP-ANOINI-FCTM-N
                                           PIC  9(004)     VALUE ZEROS.
               10  WRK-INP-DT-FINAL-FCTM.
                   15  WRK-INP-DIAFIM-FCTM.
                       20  WRK-INP-DIAFIM-FCTM-N
                                           PIC  9(002)     VALUE ZEROS.
                   15  WRK-INP-MESFIM-FCTM.
                       20  WRK-INP-MESFIM-FCTM-N
                                           PIC  9(002)     VALUE ZEROS.
                   15  WRK-INP-ANOFIM-FCTM.
                       20  WRK-INP-ANOFIM-FCTM-N
                                           PIC  9(004)     VALUE ZEROS.
           05  WRK-INP-MENSA-FCTM          PIC  X(079)     VALUE SPACES.
           05  WRK-INP-POSI-FCTM.
               10  WRK-INP-LINHA-FCTM      PIC S9(004) COMP VALUE ZEROS.
               10  WRK-INP-COLUNA-FCTM     PIC S9(004) COMP VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(050)     VALUE
           '*** AREA DA TELA DCOMFCTM  - OUTPUT ***'.
      *----------------------------------------------------------------*
       01  WRK-OUTPUT-FCTM.
           05  WRK-OUT-LL-FCTM             PIC S9(04) COMP VALUE +0344.
           05  WRK-OUT-ZZ-FCTM             PIC  9(04) COMP VALUE ZEROS.
           05  WRK-OUT-DADOS-FCTM.
               10  WRK-OUT-SENHAS-FCTM    PIC  X(37)      VALUE SPACES.
               10  WRK-OUT-COMANDO-ATTR-FCTM
                                           PIC S9(04) COMP VALUE ZEROS.
               10  WRK-OUT-COMANDO-FCTM   PIC  X(68)      VALUE SPACES.
               10  WRK-OUT-TIMESTAMP-FCTM PIC  X(26)      VALUE SPACES.
               10  WRK-OUT-FASE-FCTM      PIC  X(01)      VALUE SPACES.
               10  WRK-OUT-BANCO-FCTM.
                   15  WRK-OUT-BANCO-FCTM-N
                                           PIC  9(003)     VALUE ZEROS.
               10  WRK-OUT-CAGEN-ATTR-FCTM PIC S9(04) COMP VALUE ZEROS.
               10  WRK-OUT-CAGEN-FCTM.
                   15  WRK-OUT-CAGEN-FCTM-N
                                           PIC  9(005)     VALUE ZEROS.
               10  WRK-OUT-NAGEN-FCTM      PIC  X(040)     VALUE SPACES.
      *
               10  WRK-OUT-CPROD-ATTR-FCTM PIC S9(04) COMP VALUE ZEROS.
               10  WRK-OUT-CPROD-FCTM.
                   15  WRK-OUT-CPROD-FCTM-N
                                           PIC  9(003)     VALUE ZEROS.
               10  WRK-OUT-NPROD-FCTM      PIC  X(030)     VALUE SPACES.
      *
               10  WRK-OUT-CSUBP-ATTR-FCTM PIC S9(04) COMP VALUE ZEROS.
               10  WRK-OUT-CSUBP-FCTM.
                   15  WRK-OUT-CSUBP-FCTM-N
                                           PIC  9(003)     VALUE ZEROS.
               10  WRK-OUT-NSUBP-FCTM      PIC  X(030)     VALUE SPACES.
      *
               10  WRK-OUT-CSITU-ATTR-FCTM PIC S9(04) COMP VALUE ZEROS.
               10  WRK-OUT-CSITU-FCTM.
                   15  WRK-OUT-CSITU-FCTM-N
                                           PIC  9(003)     VALUE ZEROS.
               10  WRK-OUT-NSITU-FCTM      PIC  X(030)     VALUE SPACES.
               10  WRK-OUT-DIAINI-ATTR-FCTM
                                           PIC S9(04) COMP VALUE ZEROS.
               10  WRK-OUT-DIAINI-FCTM.
                   15  WRK-OUT-DIAINI-FCTM-N
                                           PIC  9(002)     VALUE ZEROS.
               10  WRK-OUT-MESINI-ATTR-FCTM
                                           PIC S9(04) COMP VALUE ZEROS.
               10  WRK-OUT-MESINI-FCTM.
                   15  WRK-OUT-MESINI-FCTM-N
                                           PIC  9(002)     VALUE ZEROS.
               10  WRK-OUT-ANOINI-ATTR-FCTM
                                           PIC S9(04) COMP VALUE ZEROS.
               10  WRK-OUT-ANOINI-FCTM.
                   15  WRK-OUT-ANOINI-FCTM-N
                                           PIC  9(004)     VALUE ZEROS.
               10  WRK-OUT-DIAFIM-ATTR-FCTM
                                           PIC S9(04) COMP VALUE ZEROS.
               10  WRK-OUT-DIAFIM-FCTM.
                   15  WRK-OUT-DIAFIM-FCTM-N
                                           PIC  9(002)     VALUE ZEROS.
               10  WRK-OUT-MESFIM-ATTR-FCTM
                                           PIC S9(04) COMP VALUE ZEROS.
               10  WRK-OUT-MESFIM-FCTM.
                   15  WRK-OUT-MESFIM-FCTM-N
                                           PIC  9(002)     VALUE ZEROS.
               10  WRK-OUT-ANOFIM-ATTR-FCTM
                                           PIC S9(04) COMP VALUE ZEROS.
               10  WRK-OUT-ANOFIM-FCTM.
                   15  WRK-OUT-ANOFIM-FCTM-N
                                           PIC  9(004)     VALUE ZEROS.
      *
               10  WRK-OUT-MENSA-FCTM      PIC  X(079)     VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(050)     VALUE
           '*** AREA DA TELA DCOMFCTM  - BRAD0660 ***'.
      *----------------------------------------------------------------*
       01  WRK-660-DCOMFCTM.
           05  WRK-660-LL-AREA-FCTM        PIC  9(04) COMP VALUE 0044.
           05  WRK-660-LL-MENSAGEM-FCTM    PIC  9(04) COMP VALUE 0346.
           05  WRK-660-SENHAS-FCTM         PIC  9(04) COMP VALUE 0037.
           05  WRK-660-COMANDO-FCTM        PIC  9(04) COMP VALUE 2070.
           05  WRK-660-TIMESTAMP-FCTM      PIC  9(04) COMP VALUE 0026.
           05  WRK-660-FASE-FCTM           PIC  9(04) COMP VALUE 0001.
           05  WRK-660-BANCO-FCTM          PIC  9(04) COMP VALUE 0003.
           05  WRK-660-CAGEN-FCTM          PIC  9(04) COMP VALUE 2007.
           05  WRK-660-NAGEN-FCTM          PIC  9(04) COMP VALUE 0040.
           05  WRK-660-CPROD-FCTM          PIC  9(04) COMP VALUE 2005.
           05  WRK-660-NPROD-FCTM          PIC  9(04) COMP VALUE 0030.
           05  WRK-660-CSUBP-FCTM          PIC  9(04) COMP VALUE 2005.
           05  WRK-660-NSUBP-FCTM          PIC  9(04) COMP VALUE 0030.
           05  WRK-660-CSITU-FCTM          PIC  9(04) COMP VALUE 2005.
           05  WRK-660-NSITU-FCTM          PIC  9(04) COMP VALUE 0030.
           05  WRK-660-DIAINI-FCTM         PIC  9(04) COMP VALUE 2004.
           05  WRK-660-MESINI-FCTM         PIC  9(04) COMP VALUE 2004.
           05  WRK-660-ANOINI-FCTM         PIC  9(04) COMP VALUE 2006.
           05  WRK-660-DIAFIM-FCTM         PIC  9(04) COMP VALUE 2004.
           05  WRK-660-MESFIM-FCTM         PIC  9(04) COMP VALUE 2004.
           05  WRK-660-ANOFIM-FCTM         PIC  9(04) COMP VALUE 2006.
           05  WRK-660-MENSA-FCTM          PIC  9(04) COMP VALUE 0079.

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
       01  FILLER                          PIC  X(050)     VALUE
           '*** AREA PARA MENSAGENS ***'.
      *----------------------------------------------------------------*

       COPY 'I#DCOMSY'.

      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(050)     VALUE
           '*** AREA DA BRAD7100 ***'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(050)     VALUE
           '*** DCOM0603 - FIM DA AREA DE WORKING ***'.
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
      *
           ENTRY 'DLITCBL'             USING  LNK-IO-PCB
                                              LNK-ALT-PCB.
      *
           MOVE WRK-GU                     TO WRK-FUNCAO.
      *
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

           MOVE SPACES                     TO WRK-OUT-DADOS-FCTM
                                              WRK-OUT-SENHAS-FCTM
                                              WRK-OUT-COMANDO-FCTM.

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

           MOVE 'DCOMFCTM'                 TO WRK-TELA.

           IF  WRK-TRANSACAO           NOT EQUAL 'DCOM0603'
               PERFORM 2100-RECEBER-CONTROLE
           ELSE
               PERFORM 2200-PROCESSAR-DCOMFCTM
           END-IF.


           IF  WRK-TELA                    EQUAL 'DCOMFCTM'
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

           MOVE WRK-MENSAGEM               TO COMU-DADOS.

           IF   WRK-TRANSACAO              EQUAL 'DCOM3903'
                MOVE WRK-MENSAGEM          TO COMU-AREA-3903
                MOVE COMU-TIMESTAMP-3903   TO COMU-TIMESTAMP
           END-IF

           IF   WRK-TRANSACAO              EQUAL 'DCOM3904'
                MOVE WRK-MENSAGEM          TO COMU-AREA-3904
                MOVE COMU-TIMESTAMP-3904   TO COMU-TIMESTAMP
           END-IF

           IF   WRK-TRANSACAO              EQUAL 'DCOM3913'
                MOVE WRK-MENSAGEM          TO COMU-AREA-3913
                MOVE COMU-TIMESTAMP-3913   TO COMU-TIMESTAMP
           END-IF

           MOVE COMU-TIMESTAMP             TO WRK-OUT-TIMESTAMP-FCTM
                                              WRK-INP-TIMESTAMP-FCTM
                                              WRK-ENT-TIMESTAMP

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE 'CP'                       TO WRK-ENT-OPCAO
           PERFORM 8200-ACESSAR-DCOM7995
           MOVE WRK-SAI-DADOS              TO WRK-AREA-COMUN-TOTAL

           IF   WRK-TRANSACAO                   EQUAL 'DCOM3903' OR
                WRK-TRANSACAO                   EQUAL 'DCOM3904' OR
                WRK-TRANSACAO                   EQUAL 'DCOM3913'
                IF  WRK-AGENCIA                 GREATER ZEROS
                    MOVE WRK-AGENCIA            TO WRK-OUT-CAGEN-FCTM-N
                    MOVE WRK-DESC-AGENCIA       TO WRK-OUT-NAGEN-FCTM
                END-IF
                IF  WRK-PRODUTO-LISTA           NOT EQUAL ZEROS AND
                    WRK-PRODUTO-LISTA           NOT EQUAL SPACES
                    MOVE WRK-PRODUTO-LISTA      TO WRK-OUT-CPROD-FCTM-N
                ELSE
                    MOVE SPACES                 TO WRK-OUT-CPROD-FCTM
                END-IF
                IF  WRK-SUBPRODUTO-LISTA        NOT EQUAL ZEROS AND
                    WRK-SUBPRODUTO-LISTA        NOT EQUAL SPACES
                    MOVE WRK-SUBPRODUTO-LISTA   TO WRK-OUT-CSUBP-FCTM-N
                ELSE
                    MOVE SPACES                 TO WRK-OUT-CSUBP-FCTM
                END-IF
                IF  WRK-SITUACAO-LISTA          NOT EQUAL ZEROS AND
                    WRK-SITUACAO-LISTA          NOT EQUAL SPACES
                    MOVE WRK-SITUACAO           TO WRK-OUT-CSITU-FCTM-N
                ELSE
                    MOVE SPACES                 TO WRK-OUT-CSITU-FCTM
                END-IF
                IF  WRK-DATA-INI-PESQ            NOT EQUAL SPACES
                    MOVE WRK-DATA-INI-PESQ(1:2)  TO WRK-OUT-DIAINI-FCTM
                    MOVE WRK-DATA-INI-PESQ(4:2)  TO WRK-OUT-MESINI-FCTM
                    MOVE WRK-DATA-INI-PESQ(7:4)  TO WRK-OUT-ANOINI-FCTM
                END-IF
                IF  WRK-DATA-FIM-PESQ            NOT EQUAL SPACES
                    MOVE WRK-DATA-FIM-PESQ(1:2)  TO WRK-OUT-DIAFIM-FCTM
                    MOVE WRK-DATA-FIM-PESQ(4:2)  TO WRK-OUT-MESFIM-FCTM
                    MOVE WRK-DATA-FIM-PESQ(7:4)  TO WRK-OUT-ANOFIM-FCTM
                END-IF
           END-IF.

           IF   WRK-TRANSACAO                 EQUAL 'DCOM0604'
                IF   COMU-MENSAGEM            NOT EQUAL SPACES
                     MOVE COMU-MENSAGEM       TO WRK-OUT-MENSA-FCTM
                ELSE
                     MOVE 'PREENCHA OS DADOS E TECLE <ENTER>'
                                              TO WRK-OUT-MENSA-FCTM
                END-IF
           END-IF.

           IF   WRK-TRANSACAO                EQUAL 'DCOM3903'
                MOVE WRK-MENSAGEM            TO COMU-AREA-3903
                IF   COMU-COD-PROD-3903      GREATER ZEROS
                     MOVE COMU-COD-PROD-3903 TO WRK-OUT-CPROD-FCTM-N
                     MOVE COMU-DES-PROD-3903 TO WRK-OUT-NPROD-FCTM
                ELSE
                     MOVE SPACES             TO WRK-OUT-CPROD-FCTM
                                                WRK-OUT-NPROD-FCTM
                END-IF
                MOVE COMU-MENSA-3903         TO WRK-OUT-MENSA-FCTM
           END-IF

           IF   WRK-TRANSACAO                EQUAL 'DCOM3904'
                MOVE WRK-MENSAGEM            TO COMU-AREA-3904
                IF   COMU-COD-SUBP-3904      GREATER ZEROS
                     MOVE COMU-COD-SUBP-3904 TO WRK-OUT-CSUBP-FCTM-N
                     MOVE COMU-DES-SUBP-3904 TO WRK-OUT-NSUBP-FCTM
                ELSE
                     MOVE SPACES             TO WRK-OUT-CSUBP-FCTM
                                                WRK-OUT-NSUBP-FCTM
                END-IF
                MOVE COMU-MENSA-3904         TO WRK-OUT-MENSA-FCTM
           END-IF

           IF   WRK-TRANSACAO                 EQUAL 'DCOM3913'
                MOVE WRK-MENSAGEM             TO COMU-AREA-3913
                IF   COMU-COD-SITUA-3913      GREATER ZEROS
                     MOVE COMU-COD-SITUA-3913 TO WRK-OUT-CSITU-FCTM-N
                     MOVE COMU-DES-SITUA-3913 TO WRK-OUT-NSITU-FCTM
                ELSE
                     MOVE SPACES              TO WRK-OUT-CSITU-FCTM
                                                 WRK-OUT-NSITU-FCTM
                END-IF
                MOVE COMU-MENSAGEM-3913      TO WRK-OUT-MENSA-FCTM
           END-IF.

           PERFORM VARYING IND-1               FROM 1 BY 1
                     UNTIL IND-1               GREATER 15
                        OR WRK-CHAMADOR(IND-1) EQUAL SPACES
               IF WRK-CHAMADOR(IND-1)          EQUAL 'DCOM0603'
                  MOVE SPACES                  TO WRK-CHAMADOR(IND-1)
                  MOVE ZEROS                   TO WRK-PAGINA(IND-1)
                  MOVE 99                      TO IND-1
               END-IF
           END-PERFORM.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE WRK-INP-TIMESTAMP-FCTM     TO WRK-ENT-TIMESTAMP
           MOVE 'I '                       TO WRK-ENT-OPCAO
           MOVE WRK-AREA-COMUN-TOTAL       TO WRK-ENT-DADOS

           PERFORM 8200-ACESSAR-DCOM7995.

           MOVE 237                        TO WRK-OUT-BANCO-FCTM.
           MOVE '1'                        TO WRK-OUT-FASE-FCTM.

      *----------------------------------------------------------------*
       2100-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     PROCESSAR DCOMFCTM                                         *
      *----------------------------------------------------------------*
       2200-PROCESSAR-DCOMFCTM             SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM               TO WRK-INPUT-FCTM.

           PERFORM 2210-DEVOLVER-TELA

           EVALUATE TRUE

               WHEN WRK-INP-PFK-FCTM       EQUAL 'H' OR
                   (WRK-INP-PFK-FCTM       EQUAL '.' AND
                    WRK-INP-COMANDO-FCTM   EQUAL 'PFK01')
                  MOVE WRK-INP-MENSA-FCTM  TO WRK-OUT-MENSA-FCTM
                  IF  WRK-INP-FASE-FCTM       EQUAL '2'
                      MOVE WRK-241        TO WRK-OUT-CAGEN-ATTR-FCTM
                                             WRK-OUT-CPROD-ATTR-FCTM
                                             WRK-OUT-CSUBP-ATTR-FCTM
                                             WRK-OUT-CSITU-ATTR-FCTM
                                             WRK-OUT-DIAINI-ATTR-FCTM
                                             WRK-OUT-MESINI-ATTR-FCTM
                                             WRK-OUT-ANOINI-ATTR-FCTM
                                             WRK-OUT-DIAFIM-ATTR-FCTM
                                             WRK-OUT-MESFIM-ATTR-FCTM
                                             WRK-OUT-ANOFIM-ATTR-FCTM
                       MOVE WRK-49361     TO WRK-OUT-COMANDO-ATTR-FCTM
                  END-IF

               WHEN WRK-INP-PFK-FCTM       EQUAL '3' OR
                   (WRK-INP-PFK-FCTM       EQUAL '.' AND
                    WRK-INP-COMANDO-FCTM   EQUAL 'PFK03')
                  PERFORM 2240-RETORNAR-CHAMADOR

               WHEN WRK-INP-PFK-FCTM       EQUAL 'X' OR
                   (WRK-INP-PFK-FCTM       EQUAL '.' AND
                    WRK-INP-COMANDO-FCTM   EQUAL 'PFK05')
                  PERFORM 2250-MENU-ROTINAS

               WHEN WRK-INP-PFK-FCTM       EQUAL 'Z' OR
                   (WRK-INP-PFK-FCTM       EQUAL '.' AND
                    WRK-INP-COMANDO-FCTM   EQUAL 'PFK10')
                  PERFORM 2260-MENU-DCOM

               WHEN (WRK-INP-PFK-FCTM      EQUAL '.' AND
                     WRK-INP-COMANDO-FCTM  EQUAL SPACES)
                  PERFORM 2270-TRATAR-ENTER

               WHEN (WRK-INP-PFK-FCTM       EQUAL '9' OR
                    (WRK-INP-PFK-FCTM       EQUAL '.' AND
                     WRK-INP-COMANDO-FCTM   EQUAL 'PFK09')) AND
                     WRK-INP-FASE-FCTM      EQUAL '1'
                     PERFORM 2280-CHAMAR-LISTA

               WHEN  WRK-INP-PFK-FCTM       EQUAL '6' OR
                    (WRK-INP-PFK-FCTM       EQUAL '.' AND
                     WRK-INP-COMANDO-FCTM   EQUAL 'PFK06')
                     IF WRK-INP-FASE-FCTM   EQUAL '2'
                        PERFORM 2290-TRANSFERIR-PGM
                     ELSE
                        MOVE WRK-MENS02          TO  WRK-OUT-MENSA-FCTM
                     END-IF

               WHEN OTHER
                  IF  WRK-INP-FASE-FCTM       EQUAL '2'
                      MOVE WRK-241        TO WRK-OUT-CAGEN-ATTR-FCTM
                                             WRK-OUT-CPROD-ATTR-FCTM
                                             WRK-OUT-CSUBP-ATTR-FCTM
                                             WRK-OUT-CSITU-ATTR-FCTM
                                             WRK-OUT-DIAINI-ATTR-FCTM
                                             WRK-OUT-MESINI-ATTR-FCTM
                                             WRK-OUT-ANOINI-ATTR-FCTM
                                             WRK-OUT-DIAFIM-ATTR-FCTM
                                             WRK-OUT-MESFIM-ATTR-FCTM
                                             WRK-OUT-ANOFIM-ATTR-FCTM
                       MOVE WRK-49361     TO WRK-OUT-COMANDO-ATTR-FCTM
                  END-IF
                  MOVE WRK-MENS02          TO  WRK-OUT-MENSA-FCTM

           END-EVALUATE.

      *----------------------------------------------------------------*
       2200-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     DEVOLVER TELA                                              *
      *----------------------------------------------------------------*
       2210-DEVOLVER-TELA                  SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INP-CAGEN-FCTM      NOT EQUAL SPACES AND LOW-VALUES
               INSPECT WRK-INP-CAGEN-FCTM-N
                                       REPLACING LEADING ' ' BY '0'
           END-IF.

           IF  WRK-INP-CPROD-FCTM      NOT EQUAL SPACES AND LOW-VALUES
               INSPECT WRK-INP-CPROD-FCTM-N
                                       REPLACING LEADING ' ' BY '0'
           END-IF.

           IF  WRK-INP-CSUBP-FCTM      NOT EQUAL SPACES AND LOW-VALUES
               INSPECT WRK-INP-CSUBP-FCTM-N
                                       REPLACING LEADING ' ' BY '0'
           END-IF.

           IF  WRK-INP-CSITU-FCTM      NOT EQUAL SPACES AND LOW-VALUES
               INSPECT WRK-INP-CSITU-FCTM-N
                                       REPLACING LEADING ' ' BY '0'
           END-IF.

           IF  WRK-INP-DIAINI-FCTM     NOT EQUAL SPACES AND LOW-VALUES
               INSPECT WRK-INP-DIAINI-FCTM
                                       REPLACING LEADING ' ' BY '0'
           END-IF.

           IF  WRK-INP-MESINI-FCTM     NOT EQUAL SPACES AND LOW-VALUES
               INSPECT WRK-INP-MESINI-FCTM
                                       REPLACING LEADING ' ' BY '0'
           END-IF.

           IF  WRK-INP-ANOINI-FCTM     NOT EQUAL SPACES AND LOW-VALUES
               INSPECT WRK-INP-ANOINI-FCTM
                                       REPLACING LEADING ' ' BY '0'
           END-IF.

           IF  WRK-INP-DIAFIM-FCTM     NOT EQUAL SPACES AND LOW-VALUES
               INSPECT WRK-INP-DIAFIM-FCTM
                                       REPLACING LEADING ' ' BY '0'
           END-IF.

           IF  WRK-INP-MESFIM-FCTM     NOT EQUAL SPACES AND LOW-VALUES
               INSPECT WRK-INP-MESFIM-FCTM
                                       REPLACING LEADING ' ' BY '0'
           END-IF.

           IF  WRK-INP-ANOFIM-FCTM     NOT EQUAL SPACES AND LOW-VALUES
               INSPECT WRK-INP-ANOFIM-FCTM
                                       REPLACING LEADING ' ' BY '0'
           END-IF.

           MOVE  237                       TO WRK-OUT-BANCO-FCTM
           MOVE  WRK-INP-CAGEN-FCTM        TO WRK-OUT-CAGEN-FCTM
           MOVE  WRK-INP-NAGEN-FCTM        TO WRK-OUT-NAGEN-FCTM
           MOVE  WRK-INP-CPROD-FCTM        TO WRK-OUT-CPROD-FCTM
           MOVE  WRK-INP-NPROD-FCTM        TO WRK-OUT-NPROD-FCTM
           MOVE  WRK-INP-CSUBP-FCTM        TO WRK-OUT-CSUBP-FCTM
           MOVE  WRK-INP-NSUBP-FCTM        TO WRK-OUT-NSUBP-FCTM
           MOVE  WRK-INP-CSITU-FCTM        TO WRK-OUT-CSITU-FCTM
           MOVE  WRK-INP-NSITU-FCTM        TO WRK-OUT-NSITU-FCTM
           MOVE  WRK-INP-DIAINI-FCTM       TO WRK-OUT-DIAINI-FCTM
           MOVE  WRK-INP-MESINI-FCTM       TO WRK-OUT-MESINI-FCTM
           MOVE  WRK-INP-ANOINI-FCTM       TO WRK-OUT-ANOINI-FCTM
           MOVE  WRK-INP-DIAFIM-FCTM       TO WRK-OUT-DIAFIM-FCTM
           MOVE  WRK-INP-MESFIM-FCTM       TO WRK-OUT-MESFIM-FCTM
           MOVE  WRK-INP-ANOFIM-FCTM       TO WRK-OUT-ANOFIM-FCTM
           MOVE  WRK-INP-TIMESTAMP-FCTM    TO WRK-OUT-TIMESTAMP-FCTM
           MOVE  WRK-INP-FASE-FCTM         TO WRK-OUT-FASE-FCTM.


      *----------------------------------------------------------------*
       2210-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     RETORNAR AO PROGRAMA CHAMADOR                              *
      *----------------------------------------------------------------*
       2240-RETORNAR-CHAMADOR              SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                     TO COMU-DADOS
           INITIALIZE COMU-DADOS

           MOVE WRK-INP-TIMESTAMP-FCTM     TO WRK-ENT-TIMESTAMP
           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE 'CP'                       TO WRK-ENT-OPCAO

           PERFORM 8200-ACESSAR-DCOM7995

           MOVE WRK-SAI-DADOS              TO WRK-AREA-COMUN-TOTAL

           PERFORM VARYING IND-1               FROM 1 BY 1
                     UNTIL IND-1               GREATER 15
                        OR WRK-CHAMADOR(IND-1) EQUAL SPACES
MILEO                IF    WRK-CHAMADOR(IND-1) EQUAL 'DCOM0603'
  .                      MOVE SPACES TO WRK-CHAMADOR(IND-1)
  .                      MOVE ZEROS  TO WRK-PAGINA(IND-1)
  .                  ELSE
  .                      MOVE WRK-CHAMADOR(IND-1)    TO WRK-TELA
  .                  END-IF
MILEO      END-PERFORM.

           INITIALIZE WRK-DADOS-TEMP.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE WRK-INP-TIMESTAMP-FCTM     TO WRK-ENT-TIMESTAMP
                                              COMU-TIMESTAMP
           MOVE 'I '                       TO WRK-ENT-OPCAO

           MOVE WRK-AREA-COMUN-TOTAL       TO WRK-ENT-DADOS

           PERFORM 8200-ACESSAR-DCOM7995.

           MOVE LENGTH COMU-DADOS          TO COMU-LL
           MOVE WRK-CHNG                   TO WRK-FUNCAO
           MOVE 'DCOM0603'                 TO COMU-TRANSACAO
           MOVE '3'                        TO COMU-PFK
           MOVE COMU-DADOS                 TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2240-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     RETORNAR AO MENU ROTINAS                                   *
      *----------------------------------------------------------------*
       2250-MENU-ROTINAS                   SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-TIMESTAMP-FCTM     TO WRK-ENT-TIMESTAMP.
           MOVE 1                          TO WRK-ENT-PAGINA.
           MOVE 'ET'                       TO WRK-ENT-OPCAO.

           PERFORM 8200-ACESSAR-DCOM7995.

           INITIALIZE COMU-AREA-PFK
           MOVE LENGTH OF COMU-AREA-PFK    TO COMU-LL-PFK
           MOVE WRK-CHNG                   TO WRK-FUNCAO
           MOVE 'DCOM0603'                 TO WRK-TELA
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

           MOVE WRK-INP-TIMESTAMP-FCTM     TO WRK-ENT-TIMESTAMP.
           MOVE 1                          TO WRK-ENT-PAGINA.
           MOVE 'ET'                       TO WRK-ENT-OPCAO.

           PERFORM 8200-ACESSAR-DCOM7995.

           INITIALIZE COMU-AREA-PFK
           MOVE LENGTH OF COMU-AREA-PFK    TO COMU-LL-PFK
           MOVE WRK-CHNG                   TO WRK-FUNCAO
           MOVE 'DCOM0603'                 TO WRK-TELA
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
                                               WRK-OUT-MENSA-FCTM.

           IF  WRK-OUT-FASE-FCTM           EQUAL '2'
               MOVE WRK-MENS01             TO  WRK-OUT-MENSA-FCTM
               MOVE '1'                    TO  WRK-OUT-FASE-FCTM
               GO TO 2270-99-FIM
           ELSE
               MOVE '1'                    TO  WRK-OUT-FASE-FCTM
           END-IF.

           PERFORM 2271-CONSISTIR-FISICA

           IF  WRK-INCONSIS                EQUAL SPACES
               PERFORM 2272-OBTER-NOMES
           END-IF.

           IF  WRK-INCONSIS                EQUAL SPACES
               MOVE WRK-241                TO WRK-OUT-CAGEN-ATTR-FCTM
                                              WRK-OUT-CPROD-ATTR-FCTM
                                              WRK-OUT-CSUBP-ATTR-FCTM
                                              WRK-OUT-CSITU-ATTR-FCTM
                                              WRK-OUT-DIAINI-ATTR-FCTM
                                              WRK-OUT-MESINI-ATTR-FCTM
                                              WRK-OUT-ANOINI-ATTR-FCTM
                                              WRK-OUT-DIAFIM-ATTR-FCTM
                                              WRK-OUT-MESFIM-ATTR-FCTM
                                              WRK-OUT-ANOFIM-ATTR-FCTM
               MOVE WRK-49361              TO WRK-OUT-COMANDO-ATTR-FCTM
               IF  WRK-OUT-MENSA-FCTM      EQUAL SPACES
                   MOVE WRK-MENS05         TO  WRK-OUT-MENSA-FCTM
               END-IF
               MOVE '2'                    TO  WRK-OUT-FASE-FCTM
           END-IF.


      *----------------------------------------------------------------*
       2270-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2271-CONSISTIR-FISICA           SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INP-CAGEN-FCTM      EQUAL SPACES OR LOW-VALUES
               MOVE WRK-49361          TO WRK-OUT-CAGEN-ATTR-FCTM
               MOVE WRK-MENS04         TO WRK-OUT-MENSA-FCTM
               MOVE '*'                TO WRK-INCONSIS
               MOVE SPACES             TO WRK-OUT-NAGEN-FCTM
                                          WRK-OUT-NPROD-FCTM
                                          WRK-OUT-NSUBP-FCTM
                                          WRK-OUT-NSITU-FCTM
               GO TO 2271-99-FIM
           END-IF.


           IF  WRK-INP-CAGEN-FCTM      NOT NUMERIC OR
               WRK-INP-CAGEN-FCTM      EQUAL ZEROS
               MOVE WRK-49369          TO WRK-OUT-CAGEN-ATTR-FCTM
               MOVE WRK-MENS06         TO WRK-OUT-MENSA-FCTM
               MOVE '*'                TO WRK-INCONSIS
               MOVE SPACES             TO WRK-OUT-NAGEN-FCTM
                                          WRK-OUT-NPROD-FCTM
                                          WRK-OUT-NSUBP-FCTM
                                          WRK-OUT-NSITU-FCTM
               GO TO 2271-99-FIM
           END-IF.

           PERFORM 1253-10-VALIDAR-PERFIL.

           IF  WRK-INCONSIS                NOT EQUAL SPACES
               GO TO 2271-99-FIM
           END-IF.

           IF  WRK-INP-CPROD-FCTM          NOT EQUAL SPACES
               IF  WRK-INP-CPROD-FCTM      NOT NUMERIC  OR
                   WRK-INP-CPROD-FCTM-N    EQUAL ZEROS
                   MOVE WRK-49369          TO WRK-OUT-CPROD-ATTR-FCTM
                   MOVE 'CODIGO DO PRODUTO INVALIDO'
                                           TO WRK-OUT-MENSA-FCTM
                   MOVE '*'                TO WRK-INCONSIS
                   MOVE SPACES             TO WRK-OUT-NAGEN-FCTM
                                              WRK-OUT-NPROD-FCTM
                                              WRK-OUT-NSUBP-FCTM
                                              WRK-OUT-NSITU-FCTM
                   GO TO 2271-99-FIM
               END-IF
           END-IF.

           IF  WRK-INP-CSUBP-FCTM          NOT EQUAL SPACES
               IF  WRK-INP-CSUBP-FCTM      NOT NUMERIC OR
                   WRK-INP-CSUBP-FCTM-N    EQUAL ZEROS
                   MOVE WRK-49369          TO WRK-OUT-CSUBP-ATTR-FCTM
                   MOVE 'CODIGO DO SUBPRODUTO INVALIDO'
                                           TO WRK-OUT-MENSA-FCTM
                   MOVE SPACES             TO WRK-OUT-NAGEN-FCTM
                                              WRK-OUT-NPROD-FCTM
                                              WRK-OUT-NSUBP-FCTM
                                              WRK-OUT-NSITU-FCTM
                   MOVE '*'                TO WRK-INCONSIS
                   GO TO 2271-99-FIM
               END-IF
           END-IF.

           IF  WRK-INP-CSITU-FCTM          NOT EQUAL SPACES
               IF  WRK-INP-CSITU-FCTM      NOT NUMERIC OR
                   WRK-INP-CSITU-FCTM-N    EQUAL ZEROS
                   MOVE WRK-49369          TO WRK-OUT-CSITU-ATTR-FCTM
                   MOVE 'CODIGO DA SITUACAO INVALIDO'
                                           TO WRK-OUT-MENSA-FCTM
                   MOVE '*'                TO WRK-INCONSIS
                   MOVE SPACES             TO WRK-OUT-NAGEN-FCTM
                                              WRK-OUT-NPROD-FCTM
                                              WRK-OUT-NSUBP-FCTM
                                              WRK-OUT-NSITU-FCTM
                   GO TO 2271-99-FIM
               END-IF
           END-IF.

           IF  (WRK-INP-DT-INICIAL-FCTM     EQUAL SPACES AND
                WRK-INP-DT-FINAL-FCTM   NOT EQUAL SPACES) OR
               (WRK-INP-DT-INICIAL-FCTM NOT EQUAL SPACES AND
                WRK-INP-DT-FINAL-FCTM       EQUAL SPACES)
                IF  WRK-INP-DT-INICIAL-FCTM EQUAL SPACES
                    MOVE WRK-49361          TO WRK-OUT-DIAINI-ATTR-FCTM
                END-IF

                IF  WRK-INP-DT-FINAL-FCTM   EQUAL SPACES
                    MOVE WRK-49361          TO WRK-OUT-DIAFIM-ATTR-FCTM
                END-IF

                MOVE 'PREENCHER DATA INICIAL E DATA FINAL PARA A PESQUIS
      -              'A'                   TO WRK-OUT-MENSA-FCTM
                MOVE '*'                   TO WRK-INCONSIS
                GO TO 2271-99-FIM
           END-IF.

           IF   WRK-INP-DT-INICIAL-FCTM     NOT EQUAL SPACES
                CALL 'BRAD1470'       USING WRK-INP-DT-INICIAL-FCTM

                IF  RETURN-CODE             NOT EQUAL ZEROS
                    MOVE '*'                TO WRK-INCONSIS
                    MOVE 'DATA INICIAL INVALIDA'
                                            TO WRK-OUT-MENSA-FCTM
                    MOVE WRK-49369          TO WRK-OUT-DIAINI-ATTR-FCTM
                                               WRK-OUT-MESINI-ATTR-FCTM
                                               WRK-OUT-ANOINI-ATTR-FCTM
                    GO TO 2271-99-FIM
                END-IF
           END-IF.

           IF   WRK-INP-DT-FINAL-FCTM       NOT EQUAL SPACES
                CALL 'BRAD1470'       USING WRK-INP-DT-FINAL-FCTM

                IF  RETURN-CODE             NOT EQUAL ZEROS
                    MOVE '*'                TO WRK-INCONSIS
                    MOVE 'DATA FINAL INVALIDA'
                                            TO WRK-OUT-MENSA-FCTM
                    MOVE WRK-49369          TO WRK-OUT-DIAFIM-ATTR-FCTM
                                               WRK-OUT-MESFIM-ATTR-FCTM
                                               WRK-OUT-ANOFIM-ATTR-FCTM
                    GO TO 2271-99-FIM
                END-IF
           END-IF.

           MOVE WRK-INP-DIAINI-FCTM-N         TO WRK-DIA-AUX.
           MOVE WRK-INP-MESINI-FCTM-N         TO WRK-MES-AUX.
           MOVE WRK-INP-ANOINI-FCTM-N         TO WRK-ANO-AUX.
           MOVE WRK-DATA-AUX-NUM              TO WRK-DATA-INICIAL.


           MOVE WRK-INP-DIAFIM-FCTM-N         TO WRK-DIA-AUX.
           MOVE WRK-INP-MESFIM-FCTM-N         TO WRK-MES-AUX.
           MOVE WRK-INP-ANOFIM-FCTM-N         TO WRK-ANO-AUX.
           MOVE WRK-DATA-AUX-NUM              TO WRK-DATA-FINAL.

           IF   WRK-DATA-INICIAL           GREATER WRK-DATA-FINAL
                MOVE WRK-49369             TO WRK-OUT-DIAINI-ATTR-FCTM
                                              WRK-OUT-MESINI-ATTR-FCTM
                                              WRK-OUT-ANOINI-ATTR-FCTM
                MOVE 'DATA INICIAL DEVE SER MAIOR/IGUAL QUE A DATA FINAL
      -              ''
                                           TO WRK-OUT-MENSA-FCTM
                MOVE '*'              TO WRK-INCONSIS
                GO TO 2271-99-FIM
           END-IF.

           PERFORM 2271-1-OBTER-DATA-ATUAL.

           IF   WRK-DATA-FINAL             GREATER WRK-DATA-ATUAL
                MOVE WRK-49369             TO WRK-OUT-DIAFIM-ATTR-FCTM
                                              WRK-OUT-MESFIM-ATTR-FCTM
                                              WRK-OUT-ANOFIM-ATTR-FCTM
                MOVE 'DATA FINAL NAO PODE SER MAIOR QUE DATA ATUAL'
                                           TO WRK-OUT-MENSA-FCTM
                MOVE '*'              TO WRK-INCONSIS
                GO TO 2271-99-FIM
           END-IF.

      *----------------------------------------------------------------*
       2271-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2271-1-OBTER-DATA-ATUAL             SECTION.
      *----------------------------------------------------------------*

           MOVE   'DCOM7999'       TO    WRK-MODULO.

           CALL   WRK-MODULO       USING WRK-TIMESTAMP-ATU
                                         ERRO-AREA
                                         WRK-SQLCA.

           MOVE WRK-TMS-AAAA       TO    WRK-ANO-AUX.
           MOVE WRK-TMS-MM         TO    WRK-MES-AUX.
           MOVE WRK-TMS-DD         TO    WRK-DIA-AUX.
           MOVE WRK-DATA-AUX-NUM   TO    WRK-DATA-ATUAL.

      *----------------------------------------------------------------*
       2271-1-99-FIM.                      EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1253-10-VALIDAR-PERFIL          SECTION.
      *----------------------------------------------------------------*

           INITIALIZE ROTENT-6437-ENVIO
                      ROTSAI-6437-RETORNO.

           MOVE 'DCOM6437'             TO WRK-MODULO.
           MOVE WRK-COD-USER-R         TO ROTENT-6437-CFUNC-ENV.
           MOVE LNK-IO-LTERM           TO ROTENT-6437-CTERM.
           MOVE WRK-COD-DEPTO-N        TO ROTENT-6437-AGEUSU-ENV.
           MOVE WRK-INP-CAGEN-FCTM-N   TO ROTENT-6437-AGEPESQ-ENV.
           MOVE 'A'                    TO ROTENT-6437-FUNCAO-ENV.

           CALL WRK-MODULO          USING ROTENT-6437-ENVIO
                                          ROTSAI-6437-RETORNO
                                          ERRO-AREA
                                          WRK-SQLCA.

           IF  ROTSAI-6437-COD-RET    EQUAL 99
               MOVE  'DB2'            TO ERR-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  ROTSAI-6437-COD-RET        NOT EQUAL ZEROS OR
               ROTSAI-6437-RESUL-RET      EQUAL 05 OR
               ROTSAI-6437-RESUL-RET      EQUAL 04
               MOVE '*'                   TO WRK-INCONSIS
               MOVE 'USUARIO NAO AUTORIZADO'
                                          TO WRK-OUT-MENSA-FCTM
           END-IF.

      *----------------------------------------------------------------*
       1253-10-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2272-OBTER-NOMES                    SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INP-CAGEN-FCTM      NOT EQUAL SPACES AND LOW-VALUES
               PERFORM 7000-OBTER-NOME-AGEN
           ELSE
               MOVE SPACES                 TO  WRK-OUT-NAGEN-FCTM
           END-IF.

           IF  WRK-INP-CPROD-FCTM      NOT EQUAL SPACES AND LOW-VALUES
               PERFORM 7100-OBTER-NOME-PROD
           ELSE
               MOVE SPACES                 TO  WRK-OUT-NPROD-FCTM
           END-IF.

           IF  WRK-INP-CSUBP-FCTM      NOT EQUAL SPACES AND LOW-VALUES
               PERFORM 7200-OBTER-NOME-SUBP
           ELSE
               MOVE SPACES                 TO  WRK-OUT-NSUBP-FCTM
           END-IF.

           IF  WRK-INP-CSITU-FCTM      NOT EQUAL SPACES AND LOW-VALUES
               PERFORM 7300-OBTER-NOME-SITU
           ELSE
               MOVE SPACES                 TO  WRK-OUT-NSITU-FCTM
           END-IF.


      *----------------------------------------------------------------*
       2272-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2280-CHAMAR-LISTA                   SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INP-LINHA-FCTM          EQUAL 12 AND
              (WRK-INP-COLUNA-FCTM         EQUAL 27 OR 28 OR 29)
               PERFORM 8100-OBTER-LISTA-PROD
           ELSE
              IF  WRK-INP-LINHA-FCTM       EQUAL 14 AND
                 (WRK-INP-COLUNA-FCTM      EQUAL 27 OR 28 OR 29)
                  IF  WRK-INP-CPROD-FCTM   EQUAL SPACES OR LOW-VALUES
                     MOVE 'LISTA SOMENTE DISPONIVEL SE PRODUTO PREENCHID
      -                   'O'              TO WRK-OUT-MENSA-FCTM
                  ELSE
                     PERFORM 8200-OBTER-LISTA-SUBP
                  END-IF
              ELSE
                  IF  WRK-INP-LINHA-FCTM   EQUAL 16 AND
                     (WRK-INP-COLUNA-FCTM  EQUAL 27 OR 28 OR 29)
                      PERFORM 8300-OBTER-LISTA-SITU
                  ELSE
                      MOVE 'LISTA DISPONIVEL PARA PRODUTO/SUBPROD/SITUAC
      -                    'AO - POSICIONAMENTO ERRADO'
                                           TO WRK-OUT-MENSA-FCTM
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
                      COMU-DADOS.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE WRK-INP-TIMESTAMP-FCTM     TO WRK-ENT-TIMESTAMP
                                              COMU-TIMESTAMP

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE 'CP'                       TO WRK-ENT-OPCAO

           PERFORM 8200-ACESSAR-DCOM7995
           MOVE WRK-SAI-DADOS              TO WRK-AREA-COMUN-TOTAL.

           MOVE 'I '                       TO WRK-ENT-OPCAO

           PERFORM VARYING IND-1               FROM 1 BY 1
                     UNTIL IND-1               GREATER 15

               IF  WRK-CHAMADOR(IND-1)         EQUAL SPACES
                   MOVE 'DCOM0603'             TO WRK-CHAMADOR(IND-1)
                                                  COMU-TRANSACAO
                   MOVE 99                     TO IND-1
               END-IF
           END-PERFORM.

           MOVE WRK-INP-CAGEN-FCTM-N       TO WRK-AGENCIA.
           MOVE WRK-INP-NAGEN-FCTM         TO WRK-DESC-AGENCIA.

           IF  WRK-INP-CPROD-FCTM          NOT EQUAL SPACES
               MOVE WRK-INP-CPROD-FCTM-N   TO WRK-PRODUTO-LISTA
           ELSE
               MOVE ZEROS                  TO WRK-PRODUTO-LISTA
           END-IF.

           IF  WRK-INP-CSUBP-FCTM          NOT EQUAL SPACES
               MOVE WRK-INP-CSUBP-FCTM-N   TO WRK-SUBPRODUTO-LISTA
           ELSE
               MOVE ZEROS                  TO WRK-SUBPRODUTO-LISTA
           END-IF.

           IF  WRK-INP-CSITU-FCTM          NOT EQUAL SPACES
               MOVE WRK-INP-CSITU-FCTM-N   TO WRK-SITUACAO-LISTA
           ELSE
               MOVE ZEROS                  TO WRK-SITUACAO-LISTA
           END-IF.

           IF  WRK-INP-DT-INICIAL-FCTM     NOT EQUAL SPACES
               MOVE WRK-INP-DIAINI-FCTM    TO WRK-DATA-INI-PESQ(1:2)
               MOVE WRK-INP-MESINI-FCTM    TO WRK-DATA-INI-PESQ(4:2)
               MOVE WRK-INP-ANOINI-FCTM    TO WRK-DATA-INI-PESQ(7:4)
               INSPECT WRK-DATA-INI-PESQ   REPLACING ALL ' ' BY '.'
           ELSE
               MOVE SPACES                 TO WRK-DATA-INI-PESQ
           END-IF.

           IF  WRK-INP-DT-FINAL-FCTM       NOT EQUAL SPACES
               MOVE WRK-INP-DIAFIM-FCTM    TO WRK-DATA-FIM-PESQ(1:2)
               MOVE WRK-INP-MESFIM-FCTM    TO WRK-DATA-FIM-PESQ(4:2)
               MOVE WRK-INP-ANOFIM-FCTM    TO WRK-DATA-FIM-PESQ(7:4)
               INSPECT WRK-DATA-FIM-PESQ   REPLACING ALL ' ' BY '.'
           ELSE
               MOVE SPACES                 TO WRK-DATA-FIM-PESQ
           END-IF.

           MOVE WRK-AREA-COMUN-TOTAL       TO WRK-ENT-DADOS.
           PERFORM 8200-ACESSAR-DCOM7995

           MOVE LENGTH OF COMU-DADOS       TO COMU-LL

           MOVE  'DCOM0603'                TO COMU-TRANSACAO
           MOVE  'DCOM0604'                TO WRK-TELA
           MOVE  COMU-DADOS                TO WRK-MENSAGEM
           MOVE  WRK-CHNG                  TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       2290-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2300-PROCESSAR-BRAD0660             SECTION.
      *----------------------------------------------------------------*

           MOVE LENGTH OF WRK-OUTPUT-FCTM
                                       TO  WRK-660-LL-MENSAGEM-FCTM
                                           WRK-OUT-LL-FCTM
           MOVE LENGTH OF WRK-660-DCOMFCTM
                                       TO  WRK-660-LL-AREA-FCTM

           CALL 'BRAD0660'             USING  WRK-OUTPUT-FCTM
                                              WRK-660-DCOMFCTM.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'                  TO ERR-TIPO-ACESSO
               MOVE RETURN-CODE            TO WRK-RETURN-CODE
               MOVE 10                     TO WRK-LOCAL-ERRO
               MOVE WRK-ERRO-BRAD0660      TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-ISRT                   TO WRK-FUNCAO
           MOVE WRK-OUTPUT-FCTM            TO WRK-MENSAGEM.

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
           MOVE WRK-INP-CAGEN-FCTM-N       TO  ENT6416-DEPDC
           MOVE 'DCOM6416'                 TO  WRK-MODULO
           CALL WRK-MODULO              USING  ENTRADA-ROT6416
                                               SAIDA-ROT6416
                                               ERRO-AREA
                                               WRK-SQLCA

           EVALUATE SAI6416-COD-RETORNO
             WHEN '0000'
               MOVE SAI6416-NOME-DEPDC     TO  WRK-OUT-NAGEN-FCTM

             WHEN '0099'
               MOVE 'DB2'                  TO  ERR-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO

             WHEN OTHER
               MOVE '*'                    TO  WRK-INCONSIS
               MOVE WRK-49369              TO  WRK-OUT-CAGEN-ATTR-FCTM
               MOVE 'AGENCIA INEXISTENTE'
                                           TO  WRK-OUT-MENSA-FCTM

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
           MOVE WRK-INP-CPROD-FCTM-N       TO  ENT4001-CPRODT

           MOVE WRK-INP-TIMESTAMP-FCTM(9:2) TO ENT4001-DT-MOVTO(1:2)
           MOVE WRK-INP-TIMESTAMP-FCTM(6:2) TO ENT4001-DT-MOVTO(4:2)
           MOVE WRK-INP-TIMESTAMP-FCTM(1:4) TO ENT4001-DT-MOVTO(7:4)

           INSPECT ENT4001-DT-MOVTO REPLACING ALL ' ' BY '.'.

           MOVE 'DCOM4001'                 TO  WRK-MODULO
           CALL WRK-MODULO              USING  ENTRADA-ROT4001
                                               SAIDA-ROT4001
                                               ERRO-AREA
                                               WRK-SQLCA

           EVALUATE SAI4001-COD-RETORNO
             WHEN '0000'
               MOVE SAI4001-IPRODT         TO  WRK-OUT-NPROD-FCTM

             WHEN '0099'
               MOVE 'DB2'                  TO  ERR-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO

             WHEN OTHER
               MOVE '*'                    TO  WRK-INCONSIS
               MOVE WRK-49369              TO  WRK-OUT-CPROD-ATTR-FCTM
               MOVE 'SUBPRODUTO INEXISTENTE'
                                           TO  WRK-OUT-MENSA-FCTM

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
           MOVE WRK-INP-CPROD-FCTM-N       TO  ENT4049-CPRODT
           MOVE WRK-INP-CSUBP-FCTM-N       TO  ENT4049-CSPROD-DESC-COML
           MOVE 'DCOM4049'                 TO  WRK-MODULO
           CALL WRK-MODULO              USING  ENTRADA-ROT4049
                                               SAIDA-ROT4049
                                               ERRO-AREA
                                               WRK-SQLCA

           EVALUATE SAI4049-COD-RETORNO
             WHEN '0000'
               MOVE SAI4049-ISPROD-DESC-COML
                                           TO  WRK-OUT-NSUBP-FCTM

             WHEN '0099'
               MOVE 'DB2'                  TO  ERR-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO

             WHEN OTHER
               MOVE '*'                    TO  WRK-INCONSIS
               MOVE WRK-49369              TO  WRK-OUT-CSUBP-ATTR-FCTM
               MOVE 'SUBPRODUTO INEXISTENTE'
                                           TO  WRK-OUT-MENSA-FCTM

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
           MOVE WRK-INP-CSITU-FCTM-N       TO  ENT4309-CSIT-DESC-COML
           MOVE 'DCOM4309'                 TO  WRK-MODULO
           CALL WRK-MODULO              USING  ENTRADA-ROT4309
                                               SAIDA-ROT4309
                                               ERRO-AREA
                                               WRK-SQLCA

           EVALUATE SAI4309-COD-RETORNO
             WHEN '0000'
               MOVE SAI4309-ISIT-DESC-COML
                                           TO  WRK-OUT-NSITU-FCTM

             WHEN '0099'
               MOVE 'DB2'                  TO  ERR-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO

             WHEN OTHER
               MOVE '*'                    TO  WRK-INCONSIS
               MOVE SPACES                 TO  WRK-OUT-NSITU-FCTM
               MOVE WRK-49369              TO  WRK-OUT-CSITU-ATTR-FCTM
               MOVE 'SITUACAO INEXISTENTE'
                                           TO  WRK-OUT-MENSA-FCTM

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
           MOVE WRK-INP-TIMESTAMP-FCTM     TO WRK-ENT-TIMESTAMP
                                              COMU-TIMESTAMP

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE 'CP'                       TO WRK-ENT-OPCAO

           PERFORM 8200-ACESSAR-DCOM7995
           MOVE WRK-SAI-DADOS              TO WRK-AREA-COMUN-TOTAL.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE WRK-INP-TIMESTAMP-FCTM     TO WRK-ENT-TIMESTAMP
                                              COMU-TIMESTAMP
           MOVE 'EP'                       TO WRK-ENT-OPCAO

           PERFORM 8200-ACESSAR-DCOM7995
           MOVE LENGTH OF COMU-AREA-3903   TO COMU-LL-3903
           MOVE WRK-INP-TIMESTAMP-FCTM     TO COMU-TIMESTAMP-3903
           MOVE WRK-INP-CPROD-FCTM         TO COMU-COD-PROD-3903
           MOVE 'DCOM0603'                 TO COMU-TRANSACAO-3903
           MOVE 'DCOM3903'                 TO WRK-TELA
           MOVE  WRK-CHNG                  TO WRK-FUNCAO.

           PERFORM VARYING IND-1               FROM 1 BY 1
                     UNTIL IND-1               GREATER 15


               IF  WRK-CHAMADOR(IND-1)         EQUAL SPACES
                   MOVE 'DCOM0603'             TO WRK-CHAMADOR(IND-1)
                                                  COMU-TRANSACAO
                   MOVE 99                     TO IND-1
               END-IF
           END-PERFORM.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE 'I '                       TO WRK-ENT-OPCAO
           MOVE WRK-INP-CAGEN-FCTM-N       TO WRK-AGENCIA.
           MOVE WRK-INP-NAGEN-FCTM         TO WRK-DESC-AGENCIA.

           IF  WRK-INP-CPROD-FCTM          NOT EQUAL SPACES
               MOVE WRK-INP-CPROD-FCTM-N   TO WRK-PRODUTO-LISTA
           ELSE
               MOVE ZEROS                  TO WRK-PRODUTO-LISTA
           END-IF.

           IF  WRK-INP-CSUBP-FCTM          NOT EQUAL SPACES
               MOVE WRK-INP-CSUBP-FCTM-N   TO WRK-SUBPRODUTO-LISTA
           ELSE
               MOVE ZEROS                  TO WRK-SUBPRODUTO-LISTA
           END-IF.

           IF  WRK-INP-CSITU-FCTM          NOT EQUAL SPACES
               MOVE WRK-INP-CSITU-FCTM-N   TO WRK-SITUACAO-LISTA
           ELSE
               MOVE ZEROS                  TO WRK-SITUACAO-LISTA
           END-IF.

           IF  WRK-INP-DT-INICIAL-FCTM     NOT EQUAL SPACES
               MOVE WRK-INP-DIAINI-FCTM    TO WRK-DATA-INI-PESQ(1:2)
               MOVE WRK-INP-MESINI-FCTM    TO WRK-DATA-INI-PESQ(4:2)
               MOVE WRK-INP-ANOINI-FCTM    TO WRK-DATA-INI-PESQ(7:4)
               INSPECT WRK-DATA-INI-PESQ   REPLACING ALL ' ' BY '.'
           ELSE
               MOVE SPACES                 TO WRK-DATA-INI-PESQ
           END-IF.

           IF  WRK-INP-DT-FINAL-FCTM       NOT EQUAL SPACES
               MOVE WRK-INP-DIAFIM-FCTM    TO WRK-DATA-FIM-PESQ(1:2)
               MOVE WRK-INP-MESFIM-FCTM    TO WRK-DATA-FIM-PESQ(4:2)
               MOVE WRK-INP-ANOFIM-FCTM    TO WRK-DATA-FIM-PESQ(7:4)
               INSPECT WRK-DATA-FIM-PESQ   REPLACING ALL ' ' BY '.'
           ELSE
               MOVE SPACES                 TO WRK-DATA-FIM-PESQ
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

           IF  WRK-INP-CPROD-FCTM      NOT EQUAL SPACES AND LOW-VALUES
               INSPECT WRK-INP-CPROD-FCTM-N
                                       REPLACING LEADING ' ' BY '0'
           END-IF.

           IF  WRK-INP-CPROD-FCTM      EQUAL SPACES OR LOW-VALUES
               MOVE WRK-49361          TO WRK-OUT-CPROD-ATTR-FCTM
               MOVE 'PRODUTO OBRIGATORIO'
                                       TO WRK-OUT-MENSA-FCTM
               MOVE '*'                TO WRK-INCONSIS
               MOVE SPACES             TO WRK-OUT-NPROD-FCTM
                                          WRK-OUT-NSUBP-FCTM
               GO TO 8200-99-FIM
           END-IF.

           IF  WRK-INP-CPROD-FCTM      NOT NUMERIC  OR
               WRK-INP-CPROD-FCTM-N    EQUAL ZEROS
               MOVE WRK-49369          TO WRK-OUT-CPROD-ATTR-FCTM
               MOVE 'CODIGO DO PRODUTO INVALIDO'
                                       TO WRK-OUT-MENSA-FCTM
               MOVE '*'                TO WRK-INCONSIS
               MOVE SPACES             TO WRK-OUT-NPROD-FCTM
                                          WRK-OUT-NSUBP-FCTM
               GO TO 8200-99-FIM
           END-IF.

           INITIALIZE WRK-AREA-COMUN-TOTAL
                      COMU-AREA-3904.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE WRK-INP-TIMESTAMP-FCTM     TO WRK-ENT-TIMESTAMP
                                              COMU-TIMESTAMP

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE 'CP'                       TO WRK-ENT-OPCAO

           PERFORM 8200-ACESSAR-DCOM7995
           MOVE WRK-SAI-DADOS              TO WRK-AREA-COMUN-TOTAL.

           MOVE WRK-SAI-DADOS              TO WRK-AREA-COMUN-TOTAL.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE WRK-INP-TIMESTAMP-FCTM     TO WRK-ENT-TIMESTAMP
                                              COMU-TIMESTAMP
           MOVE 'EP'                       TO WRK-ENT-OPCAO

           PERFORM 8200-ACESSAR-DCOM7995
           MOVE LENGTH OF COMU-AREA-3904   TO COMU-LL-3904

           MOVE WRK-INP-TIMESTAMP-FCTM     TO COMU-TIMESTAMP-3904


           MOVE WRK-INP-CPROD-FCTM-N       TO COMU-COD-PROD-3904
           MOVE WRK-INP-CSUBP-FCTM-N       TO COMU-COD-SUBP-3904
           MOVE ZEROS                      TO COMU-CNPJCPF-3904
           MOVE 'DCOM0603'                 TO COMU-TRANSACAO-3904
           MOVE 'DCOM3904'                 TO WRK-TELA
           MOVE  COMU-AREA-3904            TO WRK-MENSAGEM
           MOVE  WRK-CHNG                  TO WRK-FUNCAO.

           PERFORM VARYING IND-1               FROM 1 BY 1
                     UNTIL IND-1               GREATER 15


               IF  WRK-CHAMADOR(IND-1)         EQUAL SPACES
                   MOVE 'DCOM0603'             TO WRK-CHAMADOR(IND-1)
                                                  COMU-TRANSACAO
                   MOVE 99                     TO IND-1
               END-IF
           END-PERFORM.

           MOVE WRK-INP-CAGEN-FCTM-N       TO WRK-AGENCIA.
           MOVE WRK-INP-NAGEN-FCTM         TO WRK-DESC-AGENCIA.

           IF  WRK-INP-CPROD-FCTM          NOT EQUAL SPACES
               MOVE WRK-INP-CPROD-FCTM-N   TO WRK-PRODUTO-LISTA
           ELSE
               MOVE ZEROS                  TO WRK-PRODUTO-LISTA
           END-IF.

           IF  WRK-INP-CSUBP-FCTM          NOT EQUAL SPACES
               MOVE WRK-INP-CSUBP-FCTM-N   TO WRK-SUBPRODUTO-LISTA
           ELSE
               MOVE ZEROS                  TO WRK-SUBPRODUTO-LISTA
           END-IF.

           IF  WRK-INP-CSITU-FCTM          NOT EQUAL SPACES
               MOVE WRK-INP-CSITU-FCTM-N   TO WRK-SITUACAO-LISTA
           ELSE
               MOVE ZEROS                  TO WRK-SITUACAO-LISTA
           END-IF.

           IF  WRK-INP-DT-INICIAL-FCTM     NOT EQUAL SPACES
               MOVE WRK-INP-DIAINI-FCTM    TO WRK-DATA-INI-PESQ(1:2)
               MOVE WRK-INP-MESINI-FCTM    TO WRK-DATA-INI-PESQ(4:2)
               MOVE WRK-INP-ANOINI-FCTM    TO WRK-DATA-INI-PESQ(7:4)
               INSPECT WRK-DATA-INI-PESQ   REPLACING ALL ' ' BY '.'
           ELSE
               MOVE SPACES                 TO WRK-DATA-INI-PESQ
           END-IF.

           IF  WRK-INP-DT-FINAL-FCTM       NOT EQUAL SPACES
               MOVE WRK-INP-DIAFIM-FCTM    TO WRK-DATA-FIM-PESQ(1:2)
               MOVE WRK-INP-MESFIM-FCTM    TO WRK-DATA-FIM-PESQ(4:2)
               MOVE WRK-INP-ANOFIM-FCTM    TO WRK-DATA-FIM-PESQ(7:4)
               INSPECT WRK-DATA-FIM-PESQ   REPLACING ALL ' ' BY '.'
           ELSE
               MOVE SPACES                 TO WRK-DATA-FIM-PESQ
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
           MOVE WRK-INP-TIMESTAMP-FCTM     TO WRK-ENT-TIMESTAMP
                                              COMU-TIMESTAMP

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE 'CP'                       TO WRK-ENT-OPCAO

           PERFORM 8200-ACESSAR-DCOM7995
           MOVE WRK-SAI-DADOS              TO WRK-AREA-COMUN-TOTAL.

           MOVE WRK-SAI-DADOS              TO WRK-AREA-COMUN-TOTAL.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE WRK-INP-TIMESTAMP-FCTM     TO WRK-ENT-TIMESTAMP
                                              COMU-TIMESTAMP
           MOVE 'EP'                       TO WRK-ENT-OPCAO

           PERFORM 8200-ACESSAR-DCOM7995
           INITIALIZE COMU-AREA-3913
           MOVE LENGTH OF COMU-AREA-3913   TO COMU-LL-3913
           MOVE WRK-INP-TIMESTAMP-FCTM     TO COMU-TIMESTAMP-3913

           MOVE ZEROS                      TO COMU-CSIT-DESC-3913
           MOVE 006                        TO COMU-CELMTO-DESC-3913
           MOVE 'DCOM0603'                 TO COMU-TRANSACAO-3913
           MOVE 'DCOM3913'                 TO WRK-TELA
           MOVE  COMU-AREA-3913            TO WRK-MENSAGEM
           MOVE  WRK-CHNG                  TO WRK-FUNCAO.

           PERFORM VARYING IND-1               FROM 1 BY 1
                     UNTIL IND-1               GREATER 15


               IF  WRK-CHAMADOR(IND-1)         EQUAL SPACES
                   MOVE 'DCOM0603'             TO WRK-CHAMADOR(IND-1)
                                                  COMU-TRANSACAO
                   MOVE 99                     TO IND-1
               END-IF
           END-PERFORM.

           MOVE WRK-INP-CAGEN-FCTM-N       TO WRK-AGENCIA.
           MOVE WRK-INP-NAGEN-FCTM         TO WRK-DESC-AGENCIA.

           IF  WRK-INP-CPROD-FCTM          NOT EQUAL SPACES
               MOVE WRK-INP-CPROD-FCTM-N   TO WRK-PRODUTO-LISTA
           ELSE
               MOVE ZEROS                  TO WRK-PRODUTO-LISTA
           END-IF.

           IF  WRK-INP-CSUBP-FCTM          NOT EQUAL SPACES
               MOVE WRK-INP-CSUBP-FCTM-N   TO WRK-SUBPRODUTO-LISTA
           ELSE
               MOVE ZEROS                  TO WRK-SUBPRODUTO-LISTA
           END-IF.

           IF  WRK-INP-CSITU-FCTM          NOT EQUAL SPACES
               MOVE WRK-INP-CSITU-FCTM-N   TO WRK-SITUACAO-LISTA
           ELSE
               MOVE ZEROS                  TO WRK-SITUACAO-LISTA
           END-IF.

           IF  WRK-INP-DT-INICIAL-FCTM     NOT EQUAL SPACES
               MOVE WRK-INP-DIAINI-FCTM    TO WRK-DATA-INI-PESQ(1:2)
               MOVE WRK-INP-MESINI-FCTM    TO WRK-DATA-INI-PESQ(4:2)
               MOVE WRK-INP-ANOINI-FCTM    TO WRK-DATA-INI-PESQ(7:4)
               INSPECT WRK-DATA-INI-PESQ   REPLACING ALL ' ' BY '.'
           ELSE
               MOVE SPACES                 TO WRK-DATA-INI-PESQ
           END-IF.

           IF  WRK-INP-DT-FINAL-FCTM       NOT EQUAL SPACES
               MOVE WRK-INP-DIAFIM-FCTM    TO WRK-DATA-FIM-PESQ(1:2)
               MOVE WRK-INP-MESFIM-FCTM    TO WRK-DATA-FIM-PESQ(4:2)
               MOVE WRK-INP-ANOFIM-FCTM    TO WRK-DATA-FIM-PESQ(7:4)
               INSPECT WRK-DATA-FIM-PESQ   REPLACING ALL ' ' BY '.'
           ELSE
               MOVE SPACES                 TO WRK-DATA-FIM-PESQ
           END-IF.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE 'I '                       TO WRK-ENT-OPCAO
           MOVE WRK-AREA-COMUN-TOTAL       TO WRK-ENT-DADOS.
           PERFORM 8200-ACESSAR-DCOM7995.

      *----------------------------------------------------------------*
       8300-99-FIM.                        EXIT.
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
       8200-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO          SECTION.
      *----------------------------------------------------------------*

           IF  ERR-MODULO              NOT EQUAL SPACES AND LOW-VALUES
               MOVE 'DCOM0603'             TO ERR-PGM
           ELSE
               MOVE 'SENHAS02'             TO ERR-PGM
               MOVE 'DCOM0603'             TO ERR-MODULO
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
