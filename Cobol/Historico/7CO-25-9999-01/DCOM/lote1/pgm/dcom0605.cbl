      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. DCOM0605.
       AUTHOR.     ROMULO.
      *================================================================*
      *                          A L T R A N                           *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   DCOM0605                                    *
      *    PROGRAMADOR.:   ROMULO                                      *
      *    ANALISTA....:   MARCELO MORINA                              *
      *    DATA........:   22/03/2007                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   CONSULTA  OPERACAO - MENU FASE              *
      *----------------------------------------------------------------*
      *    TELAS.......:                                               *
      *    DCOMFETM - CONSULTA  OPERACAO                               *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    I#BRAD7C - AREA DE ERRO DA BRAD7100.                        *
      *    I#DCOMSY - TABELA DE MSG DE RETORNO DO SISTEMA DCOM(ONLINE).*
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    BRAD0660 - INSERIR CARACTERES DE NULL NA MENSAGEM DE SAIDA  *
      *    BRAD7100 - TRATAMENTO DE ERROS QUANDO PROGRAMA INVALIDO     *
      *    POOL5005 - CONTROLE DO TRANSITO DE MENSAGENS NO IMS/DC      *
      *----------------------------------------------------------------*
      *    NAVEGACAO...:                                               *
      *    CHAMADO POR.: SENH7020           CHAMA: DCOM0141            *
      *                                     CHAMA: DCOM0142            *
      *                                     CHAMA: DCOM01XX            *
      *                                     CHAMA: DCOM0153            *
      *                                     CHAMA: DCOM0154            *
      *                                     CHAMA: DCOM0156            *
      *                                     CHAMA: DCOM0157            *
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
           '*** DCOM0605 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
      ***                  DEFINICAO DE ATRIBUTOS
      *---------------------------------------------------------------*

       01  FILLER.
           03  TAB-ATRIBUTOS.

      *****************************************************************
      *    PROTEGIDO, NUM                                             *
      *                                         ---                   *
               05  WRK-241             PIC S9(008) COMP VALUE +241.
      *****************************************************************
      *    BRILHANTE, DESPROT, COM POS. CURSOR, NUM                   *
      *                                         ---                   *
               05  WRK-49369           PIC S9(008) COMP VALUE +49369.
      *****************************************************************
      *    NORMAL, DESPROT, COM POS. CURSOR, NUM                      *
      *                                      ---                      *
               05  WRK-49361           PIC S9(008) COMP VALUE +49361.
      *****************************************************************
      *    PROTEGIDO (NUMERICO OU ALFA)                               *
      *               -------- -- ----                                *
               05  WRK-225             PIC S9(008) COMP VALUE +225.
      *****************************************************************
      *    BRILHANTE, DESPROT, COM POS. CURSOR, ALFA                  *
      *                                         ----                  *
               05  WRK-49353           PIC S9(008) COMP VALUE +49353.
      *****************************************************************
      *    PROTEGIDO, BRILHANTE (NUMERICO OU ALFA)                    *
      *                          -------- -- ----                     *
               05  WRK-169             PIC S9(008) COMP VALUE +169.
      *****************************************************************
      *    PROTEGIDO, BRILHANTE, CURSOR (NUMERICO OU ALFA)            *
      *                                  -------- -- ----             *
               05  WRK-49385           PIC S9(008) COMP VALUE +49385.
      *****************************************************************
      *    NORMAL, DESPROT, CURSOR, ALFA                              *
      *                             ----                              *
               05  WRK-49345           PIC S9(008) COMP VALUE +49345.
      *****************************************************************
      *    NORMAL, DESPROT, NUM                                       *
      *                                                               *
               05  WRK-209             PIC S9(008) COMP VALUE +209.
      *****************************************************************
      *    NORMAL, DESPROT, ALFA                                      *
      *                                                               *
               05  WRK-193             PIC S9(008) COMP VALUE +193.
      *****************************************************************

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*
       01  WRK-AUXILIARES.
           05  IND-1                   PIC  9(009)         VALUE ZEROS.
           05  WRK-SQLCA               PIC  X(139)         VALUE SPACES.
           05  WRK-MODULO              PIC  X(008)         VALUE SPACES.
           05  WRK-ENCONTROU           PIC  X(001)         VALUE SPACES.
           05  WRK-INCONSIS            PIC  X(001)         VALUE SPACES.

ROMULO 01  WRK-ERR-TEXTO.
ROMULO     03  FILLER           PIC X(10) VALUE 'ERRO PFK3-'.
ROMULO     03  WRK-ERR-PARTE   OCCURS 7 TIMES
ROMULO                          PIC X(09).

ROMULO 01  WRK-IND-ERR          PIC 9(02) VALUE ZEROS.
ROMULO 01  WRK-IND-2            PIC 9(02) VALUE ZEROS.

       01  WRK-MENSAGENS.
           05  WRK-MENS05                  PIC  X(072)     VALUE
               'TECLE <PF6> PARA CONFIRMAR OU <ENTER> PARA ALTERAR'.

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

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREAS AUXILIARES PARA BRAD2000 ***'.
      *----------------------------------------------------------------*
       01  WRK-CPFCGC.
FS2511*    05  WRK-NUMERO              PIC  9(009)         VALUE ZEROS.
FS2511     05  WRK-NUMERO              PIC  X(09)          VALUE SPACES.
FS2511*    05  WRK-FILIAL              PIC  9(005)         VALUE ZEROS.
FS2511     05  WRK-FILIAL              PIC  X(04)          VALUE SPACES.

       77  WRK-CONTROLE                PIC  X(002)         VALUE ZEROS.

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
               10  COMU-TIMESTA-3913       PIC  X(026)     VALUE SPACES.
               10  COMU-TPOPCAO-3913       PIC  X(001)     VALUE SPACES.
               10  COMU-CELMTO-DESC-3913   PIC  9(003)     VALUE ZEROS.
               10  COMU-CSIT-DESC-3913     PIC  9(003)     VALUE ZEROS.
               10  COMU-COD-SITUA-3913     PIC  9(003)     VALUE ZEROS.
               10  COMU-DES-SITUA-3913     PIC  X(020)     VALUE SPACES.
               10  COMU-MENSAGEM-3913      PIC  X(079)     VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(050)     VALUE
           '*** AREA DE COMUNICACAO ***'.
      *----------------------------------------------------------------*
      *
       COPY 'I#DCOMJ2'.

      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(050)     VALUE
           '*** AREA DE VALIDACAO DO PERFIL ***'.
      *----------------------------------------------------------------*
      *
       COPY 'I#DCOMXE'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA TELA DCOMFETM  - INPUT ***'.
      *----------------------------------------------------------------*
       01  WRK-INPUT-FETM.
           05  FILLER                  PIC  X(018) VALUE SPACES.
           05  WRK-INP-PFK-FETM        PIC  X(001) VALUE SPACES.
           05  WRK-INP-SENHAS-FETM     PIC  X(037) VALUE SPACES.
           05  WRK-INP-COMANDO-FETM    PIC  X(068) VALUE SPACES.
           05  WRK-INP-DADOS-FETM.
             07 WRK-INP-FIXO-FETM.
               10  WRK-INP-TIMESTA-FETM   PIC  X(026) VALUE SPACES.
               10  WRK-INP-FASE-FETM      PIC  X(001) VALUE SPACES.
               10  WRK-INP-AGENC-FETM.
                   15  WRK-INP-AGENC-FETM-N PIC  9(005) VALUE ZEROS.
               10  WRK-INP-CONTA-FETM     PIC  X(013) VALUE SPACES.
               10  WRK-INP-CNPJ1-FETM     PIC  X(009) VALUE SPACES.
               10  WRK-INP-FILI1-FETM     PIC  X(004) VALUE SPACES.
               10  WRK-INP-CTRL1-FETM     PIC  X(002) VALUE SPACES.
               10  WRK-INP-CNPJ2-FETM     PIC  X(009) VALUE SPACES.
               10  WRK-INP-FILI2-FETM     PIC  X(004) VALUE SPACES.
               10  WRK-INP-CTRL2-FETM     PIC  X(002) VALUE SPACES.
               10  WRK-INP-PROD-FETM      PIC  X(003) VALUE SPACES.
               10  WRK-INP-DPROD-FETM     PIC  X(012) VALUE SPACES.
               10  WRK-INP-SPROD-FETM     PIC  X(003) VALUE SPACES.
               10  WRK-INP-DSPROD-FETM    PIC  X(010) VALUE SPACES.
               10  WRK-INP-SITUA-FETM     PIC  X(003) VALUE SPACES.
               10  WRK-INP-DSITUA-FETM    PIC  X(015) VALUE SPACES.
             07  WRK-INP-MENSAG-FETM      PIC  X(079) VALUE SPACES.
             07  WRK-INP-POSI-FETM.
               10  WRK-INP-LINHA-FETM     PIC S9(004) COMP VALUE ZEROS.
               10  WRK-INP-COLUNA-FETM    PIC S9(004) COMP VALUE ZEROS.


      *----------------------------------------------------------------*
       01  FILLER                       PIC  X(050)         VALUE
           '*** AREA DA TELA DCOMFETM  - OUTPUT ***'.
      *----------------------------------------------------------------*
       01  WRK-OUTPUT-FETM.
           05  WRK-OUT-LL-FETM          PIC S9(004) COMP VALUE ZEROS.
           05  WRK-OUT-ZZ-FETM          PIC  9(004) COMP VALUE ZEROS.
           05  WRK-OUT-SENHAS-FETM      PIC  X(037) VALUE SPACES.
           05  WRK-OUT-COMANDO-ATTR-FETM
                                        PIC S9(004) COMP VALUE ZEROS.
           05  WRK-OUT-COMANDO-FETM     PIC  X(068) VALUE SPACES.
           05  WRK-OUT-DADOS-FETM.
             07 WRK-OUT-FIXO-FETM.
               10  WRK-OUT-TIMESTA-FETM   PIC  X(026) VALUE SPACES.
               10  WRK-OUT-FASE-FETM      PIC  X(001) VALUE SPACES.
               10  WRK-OUT-AGENC-ATTR-FETM
                                          PIC S9(004) COMP VALUE ZEROS.
               10  WRK-OUT-AGENC-FETM     PIC  X(005) VALUE SPACES.
               10  WRK-OUT-CONTA-ATTR-FETM
                                          PIC S9(004) COMP VALUE ZEROS.
               10  WRK-OUT-CONTA-FETM     PIC  X(013) VALUE SPACES.
               10  WRK-OUT-CNPJ1-ATTR-FETM
                                          PIC S9(004) COMP VALUE ZEROS.
               10  WRK-OUT-CNPJ1-FETM     PIC  X(009) VALUE SPACES.
               10  WRK-OUT-FILI1-ATTR-FETM
                                          PIC S9(004) COMP VALUE ZEROS.
               10  WRK-OUT-FILI1-FETM     PIC  X(004) VALUE SPACES.
               10  WRK-OUT-CTRL1-ATTR-FETM
                                          PIC S9(004) COMP VALUE ZEROS.
               10  WRK-OUT-CTRL1-FETM     PIC  X(002) VALUE SPACES.
               10  WRK-OUT-CNPJ2-ATTR-FETM
                                          PIC S9(004) COMP VALUE ZEROS.
               10  WRK-OUT-CNPJ2-FETM     PIC  X(009) VALUE SPACES.
               10  WRK-OUT-FILI2-ATTR-FETM
                                          PIC S9(004) COMP VALUE ZEROS.
               10  WRK-OUT-FILI2-FETM     PIC  X(004) VALUE SPACES.
               10  WRK-OUT-CTRL2-ATTR-FETM
                                          PIC S9(004) COMP VALUE ZEROS.
               10  WRK-OUT-CTRL2-FETM     PIC  X(002) VALUE SPACES.
               10  WRK-OUT-PROD-ATTR-FETM
                                          PIC S9(004) COMP VALUE ZEROS.
               10  WRK-OUT-PROD-FETM      PIC  X(003) VALUE SPACES.
               10  WRK-OUT-DPROD-FETM     PIC  X(012) VALUE SPACES.
               10  WRK-OUT-SPROD-ATTR-FETM
                                          PIC S9(004) COMP VALUE ZEROS.
               10  WRK-OUT-SPROD-FETM     PIC  X(003) VALUE SPACES.
               10  WRK-OUT-DSPROD-FETM    PIC  X(010) VALUE SPACES.
               10  WRK-OUT-SITUA-ATTR-FETM
                                          PIC S9(004) COMP VALUE ZEROS.
               10  WRK-OUT-SITUA-FETM     PIC  X(003) VALUE SPACES.
               10  WRK-OUT-DSITUA-FETM    PIC  X(015) VALUE SPACES.
             07  WRK-OUT-MENSAG-FETM      PIC  X(079) VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA TELA DCOMFETM  - BRAD0660 ***'.
      *----------------------------------------------------------------*
       01  WRK-660-DCOMFETM.
           05  WRK-660-LL-AREA-FETM     PIC  9(004) COMP   VALUE 0042.
           05  WRK-660-LL-MENS-FETM     PIC  9(004) COMP   VALUE 0331.
           05  WRK-660-SENHAS-FETM      PIC  9(004) COMP   VALUE 0037.
           05  WRK-660-COMANDO-FETM     PIC  9(004) COMP   VALUE 2070.
           05  WRK-660-TIMESTA          PIC  9(004) COMP   VALUE 0026.
           05  WRK-660-FASE             PIC  9(004) COMP   VALUE 0001.
           05  WRK-660-AGENC            PIC  9(004) COMP   VALUE 2007.
           05  WRK-660-CONTA            PIC  9(004) COMP   VALUE 2015.
           05  WRK-660-CNPJ1            PIC  9(004) COMP   VALUE 2011.
           05  WRK-660-FILI1            PIC  9(004) COMP   VALUE 2006.
           05  WRK-660-CTRL1            PIC  9(004) COMP   VALUE 2004.
           05  WRK-660-CNPJ2            PIC  9(004) COMP   VALUE 2011.
           05  WRK-660-FILI2            PIC  9(004) COMP   VALUE 2006.
           05  WRK-660-CTRL2            PIC  9(004) COMP   VALUE 2004.
           05  WRK-660-PROD             PIC  9(004) COMP   VALUE 2005.
           05  WRK-660-DPROD            PIC  9(004) COMP   VALUE 0012.
           05  WRK-660-SPROD            PIC  9(004) COMP   VALUE 2005.
           05  WRK-660-DSPROD           PIC  9(004) COMP   VALUE 0010.
           05  WRK-660-SITUA            PIC  9(004) COMP   VALUE 2005.
           05  WRK-660-DSITUA           PIC  9(004) COMP   VALUE 0015.
           05  WRK-660-MENSAG           PIC  9(004) COMP   VALUE 0079.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(070)         VALUE
                'AREA DE COMUNICACAO'.
      *----------------------------------------------------------------*

       01  COMU-AREA.
           05  COMU-LL                PIC S9(004) COMP  VALUE ZEROS.
           05  COMU-ZZ                PIC S9(004) COMP  VALUE ZEROS.
           05  COMU-TRANCODE.
               10  COMU-TRANSACAO     PIC  X(008)     VALUE SPACES.
               10  FILLER             PIC  X(006)     VALUE SPACES.
               10  COMU-PFK           PIC  X(001)     VALUE SPACES.
           05  COMU-SENHAS            PIC  X(037)     VALUE SPACES.
           05  COMU-COMANDO           PIC  X(068)     VALUE SPACES.
           05  COMU-DADOS.
               10  COMU-TIMESTA       PIC  X(026)     VALUE SPACES.
               10  COMU-MENSAGEM      PIC  X(079)     VALUE SPACES.

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
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA MENSAGENS ***'.
      *----------------------------------------------------------------*

       COPY 'I#DCOMSY'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA BRAD7100 ***'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** DCOM0605 - FIM DA AREA DE WORKING ***'.
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

           MOVE SPACES                 TO  WRK-OUT-DADOS-FETM
                                           WRK-OUT-SENHAS-FETM
                                           WRK-OUT-COMANDO-FETM
                                           WRK-OUT-MENSAG-FETM.


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

           MOVE 'DCOMFETM'             TO  WRK-TELA.

           IF  WRK-TRANSACAO    NOT EQUAL  'DCOM0605'
               PERFORM 1100-RECEBER-CONTROLE
           ELSE
               PERFORM 1200-PROCESSAR-DCOMFETM
           END-IF.

           IF  WRK-TELA             EQUAL  'DCOMFETM'
               PERFORM 1300-PROCESSAR-BRAD0660
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA RECEBE O CONTROLE DE OUTRAS TRANSACOES            *
      *----------------------------------------------------------------*
       1100-RECEBER-CONTROLE           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM               TO COMU-AREA

           IF   WRK-TRANSACAO              EQUAL 'DCOM3903'
                MOVE WRK-MENSAGEM          TO COMU-AREA-3903
                MOVE COMU-TIMESTA-3903     TO COMU-TIMESTA
           END-IF

           IF   WRK-TRANSACAO              EQUAL 'DCOM3904'
                MOVE WRK-MENSAGEM          TO COMU-AREA-3904
                MOVE COMU-TIMESTA-3904     TO COMU-TIMESTA
           END-IF

           IF   WRK-TRANSACAO              EQUAL 'DCOM3913'
                MOVE WRK-MENSAGEM          TO COMU-AREA-3913
                MOVE COMU-TIMESTA-3913     TO COMU-TIMESTA
           END-IF

           MOVE COMU-TIMESTA               TO WRK-INP-TIMESTA-FETM
                                              WRK-OUT-TIMESTA-FETM
                                              WRK-ENT-TIMESTA.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE 'CP'                       TO WRK-ENT-OPCAO

           PERFORM 8200-ACESSAR-DCOM7995

           MOVE WRK-SAI-DADOS              TO WRK-AREA-COMUN-TOTAL


           IF   WRK-TRANSACAO                   EQUAL 'DCOM3903' OR
                WRK-TRANSACAO                   EQUAL 'DCOM3904' OR
                WRK-TRANSACAO                   EQUAL 'DCOM3913'
                IF  WRK-AGENCIA                 GREATER ZEROS
                    MOVE WRK-AGENCIA            TO WRK-OUT-AGENC-FETM
                END-IF
                IF  WRK-CONTA                   GREATER ZEROS
                    MOVE WRK-CONTA              TO WRK-OUT-CONTA-FETM
                END-IF
                IF  WRK-CCNPJ-CEDENTE           GREATER ZEROS
                    MOVE WRK-CCNPJ-CEDENTE      TO WRK-OUT-CNPJ1-FETM
                END-IF
                IF  WRK-FILIAL-CEDENTE          NOT EQUAL SPACES
                    MOVE WRK-FILIAL-CEDENTE     TO WRK-OUT-FILI1-FETM
                END-IF
                IF  WRK-CCTRL-CEDENTE           GREATER ZEROS
                   MOVE WRK-CCTRL-CEDENTE       TO WRK-OUT-CTRL1-FETM
                END-IF
                IF  WRK-CCNPJ-SACADO            GREATER ZEROS
                    MOVE WRK-CCNPJ-SACADO       TO WRK-OUT-CNPJ2-FETM
                END-IF
                IF  WRK-FILIAL-SACADO           NOT EQUAL SPACES
                    MOVE WRK-FILIAL-SACADO      TO WRK-OUT-FILI2-FETM
                END-IF
                IF  WRK-CCTRL-SACADO            GREATER ZEROS
                    MOVE WRK-CCTRL-SACADO       TO WRK-OUT-CTRL2-FETM
                END-IF
                IF  WRK-PRODUTO-LISTA           GREATER ZEROS
                    MOVE WRK-PRODUTO-LISTA      TO WRK-OUT-PROD-FETM
                END-IF
                IF  WRK-SUBPRODUTO-LISTA        GREATER ZEROS
                    MOVE WRK-SUBPRODUTO-LISTA   TO WRK-OUT-SPROD-FETM
                END-IF
                IF  WRK-SITUACAO-LISTA          GREATER ZEROS
                    MOVE WRK-SITUACAO-LISTA     TO WRK-OUT-SITUA-FETM
                END-IF
           END-IF.


           IF   WRK-TRANSACAO                EQUAL 'DCOM3903'
                MOVE WRK-MENSAGEM            TO COMU-AREA-3903
                IF   COMU-COD-PROD-3903      GREATER ZEROS
                     MOVE COMU-COD-PROD-3903 TO WRK-OUT-PROD-FETM
                     MOVE COMU-DES-PROD-3903 TO WRK-OUT-DPROD-FETM
                ELSE
                     MOVE SPACES             TO WRK-OUT-PROD-FETM
                                                WRK-OUT-DPROD-FETM
                END-IF
                MOVE COMU-MENSA-3903         TO WRK-OUT-MENSAG-FETM
           END-IF

           IF   WRK-TRANSACAO                EQUAL 'DCOM3904'
                MOVE WRK-MENSAGEM            TO COMU-AREA-3904
                IF   COMU-COD-SUBP-3904      GREATER ZEROS
                     MOVE COMU-COD-SUBP-3904 TO WRK-OUT-SPROD-FETM
                     MOVE COMU-DES-SUBP-3904 TO WRK-OUT-DSPROD-FETM
                ELSE
                     MOVE SPACES             TO WRK-OUT-SPROD-FETM
                                                WRK-OUT-DSPROD-FETM
                END-IF
                MOVE COMU-MENSA-3904         TO WRK-OUT-MENSAG-FETM
           END-IF

           IF   WRK-TRANSACAO                 EQUAL 'DCOM3913'
                MOVE WRK-MENSAGEM             TO COMU-AREA-3913
                IF   COMU-COD-SITUA-3913      GREATER ZEROS
                     MOVE COMU-COD-SITUA-3913 TO WRK-OUT-SITUA-FETM
                     MOVE COMU-DES-SITUA-3913 TO WRK-OUT-DSITUA-FETM
                ELSE
                     MOVE SPACES              TO WRK-OUT-SITUA-FETM
                                                 WRK-OUT-DSITUA-FETM
                END-IF
                MOVE COMU-MENSAGEM-3913      TO WRK-OUT-MENSAG-FETM
           END-IF.

           PERFORM VARYING IND-1               FROM 1 BY 1
                     UNTIL IND-1               GREATER 15
                        OR WRK-CHAMADOR(IND-1) EQUAL SPACES
               IF WRK-CHAMADOR(IND-1)          EQUAL 'DCOM0605'
                  MOVE SPACES                  TO WRK-CHAMADOR(IND-1)
                  MOVE ZEROS                   TO WRK-PAGINA(IND-1)
                  MOVE 99                      TO IND-1
               END-IF
           END-PERFORM.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE WRK-INP-TIMESTA-FETM       TO WRK-ENT-TIMESTA
           MOVE 'I '                       TO WRK-ENT-OPCAO
           MOVE WRK-AREA-COMUN-TOTAL       TO WRK-ENT-DADOS

           PERFORM 8200-ACESSAR-DCOM7995.

           IF   WRK-TRANSACAO              EQUAL 'DCOM3903' OR
                WRK-TRANSACAO              EQUAL 'DCOM3904' OR
                WRK-TRANSACAO              EQUAL 'DCOM3913'
                IF  WRK-OUT-MENSAG-FETM    EQUAL SPACES
                    MOVE 'DIGITE OS DADOS E TECLE <ENTER>'
                                           TO WRK-OUT-MENSAG-FETM
                END-IF
           ELSE
                IF  COMU-MENSAGEM          EQUAL SPACES
                    MOVE 'DIGITE OS DADOS E TECLE <ENTER>'
                                           TO WRK-OUT-MENSAG-FETM
                ELSE
                    MOVE COMU-MENSAGEM     TO WRK-OUT-MENSAG-FETM
                END-IF
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
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
      *  ESSA ROTINA PROCESSA A MENSAGEM VINDA DA TELA DCOMDCTM        *
      *----------------------------------------------------------------*
       1200-PROCESSAR-DCOMFETM         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM             TO WRK-INPUT-FETM.

           PERFORM 1205-TRATAR-INSPECT.

           PERFORM 1210-DEVOLVER-TELA.

           EVALUATE TRUE
               WHEN WRK-INP-PFK-FETM     EQUAL 'H' OR
                   (WRK-INP-PFK-FETM     EQUAL '.' AND
                    WRK-INP-COMANDO-FETM EQUAL 'PFK01')
                    MOVE WRK-INP-MENSAG-FETM TO WRK-OUT-MENSAG-FETM

               WHEN WRK-INP-PFK-FETM     EQUAL '3' OR
                   (WRK-INP-PFK-FETM     EQUAL '.' AND
                    WRK-INP-COMANDO-FETM EQUAL 'PFK03')
                    PERFORM 1220-RETORNAR-CHAMADOR

               WHEN WRK-INP-PFK-FETM     EQUAL 'X' OR
                   (WRK-INP-PFK-FETM     EQUAL '.' AND
                    WRK-INP-COMANDO-FETM EQUAL 'PFK05')
                    PERFORM 1230-RETORNAR-MENU-ROTINAS

               WHEN (WRK-INP-PFK-FETM     EQUAL '6' OR
                    (WRK-INP-PFK-FETM     EQUAL '.' AND
                     WRK-INP-COMANDO-FETM EQUAL 'PFK06')) AND
                     WRK-INP-FASE-FETM    EQUAL '2'
                     PERFORM 1250-TRATAR-CONFIRMACAO

               WHEN WRK-INP-PFK-FETM     EQUAL '9' OR
                   (WRK-INP-PFK-FETM     EQUAL '.' AND
                    WRK-INP-COMANDO-FETM EQUAL 'PFK09') AND
                    WRK-INP-FASE-FETM    EQUAL '1'
                    PERFORM 1260-TRATAR-LISTA

               WHEN WRK-INP-PFK-FETM     EQUAL 'Z' OR
                   (WRK-INP-PFK-FETM     EQUAL '.' AND
                    WRK-INP-COMANDO-FETM EQUAL 'PFK10')
                    PERFORM 1240-RETORNAR-MENU-DCOM

               WHEN WRK-INP-PFK-FETM     EQUAL '.' AND
                    WRK-INP-COMANDO-FETM EQUAL  SPACES OR LOW-VALUES
                    PERFORM 1280-TRATAR-ENTER

               WHEN OTHER
                    IF  WRK-INP-FASE-FETM  EQUAL '2'
                        MOVE WRK-241       TO WRK-OUT-AGENC-ATTR-FETM
                                              WRK-OUT-CONTA-ATTR-FETM
                                              WRK-OUT-CNPJ1-ATTR-FETM
                                              WRK-OUT-FILI1-ATTR-FETM
                                              WRK-OUT-CTRL1-ATTR-FETM
                                              WRK-OUT-CNPJ2-ATTR-FETM
                                              WRK-OUT-FILI2-ATTR-FETM
                                              WRK-OUT-CTRL2-ATTR-FETM
                                              WRK-OUT-PROD-ATTR-FETM
                                              WRK-OUT-SPROD-ATTR-FETM
                        MOVE WRK-49361     TO WRK-OUT-COMANDO-ATTR-FETM
                    END-IF
                    MOVE 'PF INVALIDA'   TO WRK-OUT-MENSAG-FETM

           END-EVALUATE.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA INSPECT NOS CAMPOS NUMERICOS               *
      *----------------------------------------------------------------*
       1205-TRATAR-INSPECT             SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INP-AGENC-FETM         NOT EQUAL SPACES
               INSPECT WRK-INP-AGENC-FETM REPLACING LEADING ' ' BY '0'
           END-IF.

           IF  WRK-INP-CONTA-FETM         NOT EQUAL SPACES
               INSPECT WRK-INP-CONTA-FETM REPLACING LEADING ' ' BY '0'
           END-IF.

           IF  WRK-INP-CNPJ1-FETM         NOT EQUAL SPACES
               INSPECT WRK-INP-CNPJ1-FETM REPLACING LEADING ' ' BY '0'
           END-IF.

           IF  WRK-INP-FILI1-FETM         NOT EQUAL SPACES
               INSPECT WRK-INP-FILI1-FETM REPLACING LEADING ' ' BY '0'
           END-IF.

           IF  WRK-INP-CTRL1-FETM         NOT EQUAL SPACES
               INSPECT WRK-INP-CTRL1-FETM REPLACING LEADING ' ' BY '0'
           END-IF.

           IF  WRK-INP-CNPJ2-FETM         NOT EQUAL SPACES
               INSPECT WRK-INP-CNPJ2-FETM REPLACING LEADING ' ' BY '0'
           END-IF.

           IF  WRK-INP-FILI2-FETM         NOT EQUAL SPACES
               INSPECT WRK-INP-FILI2-FETM REPLACING LEADING ' ' BY '0'
           END-IF.

           IF  WRK-INP-CTRL2-FETM         NOT EQUAL SPACES
               INSPECT WRK-INP-CTRL2-FETM REPLACING LEADING ' ' BY '0'
           END-IF.

           IF  WRK-INP-PROD-FETM          NOT EQUAL SPACES
               INSPECT WRK-INP-PROD-FETM  REPLACING LEADING ' ' BY '0'
           END-IF.

           IF  WRK-INP-SPROD-FETM         NOT EQUAL SPACES
               INSPECT WRK-INP-SPROD-FETM REPLACING LEADING ' ' BY '0'
           END-IF.

           IF  WRK-INP-SITUA-FETM         NOT EQUAL SPACES
               INSPECT WRK-INP-SITUA-FETM REPLACING LEADING ' ' BY '0'
           END-IF.

      *----------------------------------------------------------------*
       1205-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA DEVOLVE OS DADOS DA TELA SEM ALTERACAO            *
      *----------------------------------------------------------------*
       1210-DEVOLVER-TELA              SECTION.
      *----------------------------------------------------------------*

           MOVE  WRK-INP-TIMESTA-FETM TO WRK-OUT-TIMESTA-FETM.
           MOVE  WRK-INP-FASE-FETM    TO WRK-OUT-FASE-FETM.
           MOVE  WRK-INP-AGENC-FETM   TO WRK-OUT-AGENC-FETM.
           MOVE  WRK-INP-CONTA-FETM   TO WRK-OUT-CONTA-FETM.
           MOVE  WRK-INP-CNPJ1-FETM   TO WRK-OUT-CNPJ1-FETM.
           MOVE  WRK-INP-FILI1-FETM   TO WRK-OUT-FILI1-FETM.
           MOVE  WRK-INP-CTRL1-FETM   TO WRK-OUT-CTRL1-FETM.
           MOVE  WRK-INP-CNPJ2-FETM   TO WRK-OUT-CNPJ2-FETM.
           MOVE  WRK-INP-FILI2-FETM   TO WRK-OUT-FILI2-FETM.
           MOVE  WRK-INP-CTRL2-FETM   TO WRK-OUT-CTRL2-FETM.
           MOVE  WRK-INP-PROD-FETM    TO WRK-OUT-PROD-FETM.
           MOVE  WRK-INP-DPROD-FETM   TO WRK-OUT-DPROD-FETM.
           MOVE  WRK-INP-SPROD-FETM   TO WRK-OUT-SPROD-FETM.
           MOVE  WRK-INP-DSPROD-FETM  TO WRK-OUT-DSPROD-FETM.
           MOVE  WRK-INP-SITUA-FETM   TO WRK-OUT-SITUA-FETM.
           MOVE  WRK-INP-DSITUA-FETM  TO WRK-OUT-DSITUA-FETM.

      *----------------------------------------------------------------*
       1210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1220-RETORNAR-CHAMADOR              SECTION.
      *----------------------------------------------------------------*

           INITIALIZE COMU-AREA

           MOVE WRK-INP-TIMESTA-FETM       TO WRK-ENT-TIMESTA
           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE 'CP'                       TO WRK-ENT-OPCAO

           PERFORM 8200-ACESSAR-DCOM7995

           MOVE WRK-SAI-DADOS              TO WRK-AREA-COMUN-TOTAL

           PERFORM VARYING IND-1               FROM 1 BY 1
                     UNTIL IND-1               GREATER 15
                        OR WRK-CHAMADOR(IND-1) EQUAL SPACES
ATEND          IF  WRK-CHAMADOR(IND-1) EQUAL 'DCOM0605'
  .                MOVE SPACES TO WRK-CHAMADOR(IND-1)
  .                MOVE ZEROS  TO WRK-PAGINA(IND-1)
  .            ELSE
  .                MOVE WRK-CHAMADOR(IND-1)    TO WRK-TELA
ATEND          END-IF
           END-PERFORM.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE WRK-INP-TIMESTA-FETM       TO WRK-ENT-TIMESTA
                                              COMU-TIMESTA

           INITIALIZE WRK-DADOS-TEMP.

           MOVE 'I '                       TO WRK-ENT-OPCAO
           MOVE WRK-AREA-COMUN-TOTAL       TO WRK-ENT-DADOS

           PERFORM 8200-ACESSAR-DCOM7995.

           MOVE LENGTH COMU-AREA           TO COMU-LL
           MOVE WRK-CHNG                   TO WRK-FUNCAO
           MOVE 'DCOM0605'                 TO COMU-TRANSACAO
           MOVE '3'                        TO COMU-PFK
           MOVE COMU-AREA                  TO WRK-MENSAGEM.

ROMULO     IF  WRK-TELA   EQUAL  'DCOM0605'
ROMULO         MOVE 'DCOM0605'         TO ERR-PGM
ROMULO                                    ERR-MODULO
ROMULO         MOVE 'APL'              TO ERR-TIPO-ACESSO

ROMULO         COMPUTE WRK-IND-2 = IND-1 - 1
ROMULO         MOVE 1  TO WRK-IND-ERR
ROMULO         PERFORM VARYING WRK-IND-2 FROM WRK-IND-2 BY -1
ROMULO                 UNTIL ( WRK-IND-2     LESS    1 )
ROMULO                  OR   ( WRK-IND-ERR   GREATER 7 )
ROMULO             MOVE    WRK-CHAMADOR(WRK-IND-2)
ROMULO                     TO  WRK-ERR-PARTE(WRK-IND-ERR)
ROMULO             ADD   1 TO  WRK-IND-ERR
ROMULO         END-PERFORM

ROMULO         MOVE WRK-ERR-TEXTO      TO ERR-TEXTO
ROMULO         PERFORM 9999-PROCESSAR-ROTINA-ERRO
ROMULO     END-IF.

      *----------------------------------------------------------------*
       1220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRATAMENTO DE RETORNO AO MENU ROTINAS    *
      *----------------------------------------------------------------*
       1230-RETORNAR-MENU-ROTINAS      SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-TIMESTA-FETM       TO WRK-ENT-TIMESTA.
           MOVE 1                          TO WRK-ENT-PAGINA.
           MOVE 'ET'                       TO WRK-ENT-OPCAO.

           PERFORM 8200-ACESSAR-DCOM7995.

           INITIALIZE COMU-AREA-PFK
           MOVE LENGTH OF COMU-AREA-PFK    TO COMU-LL-PFK
           MOVE WRK-CHNG                   TO WRK-FUNCAO
           MOVE 'DCOM0605'                 TO WRK-TELA
                                              COMU-TRANSACAO-PFK
           MOVE '5'                        TO COMU-PFK-PFK
           MOVE COMU-AREA-PFK              TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRATAMENTO DE RETORNO AO MENU DCOM       *
      *----------------------------------------------------------------*
       1240-RETORNAR-MENU-DCOM         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-TIMESTA-FETM       TO WRK-ENT-TIMESTA.
           MOVE 1                          TO WRK-ENT-PAGINA.
           MOVE 'ET'                       TO WRK-ENT-OPCAO.

           PERFORM 8200-ACESSAR-DCOM7995.

           INITIALIZE COMU-AREA-PFK
           MOVE LENGTH OF COMU-AREA-PFK    TO COMU-LL-PFK
           MOVE WRK-CHNG                   TO WRK-FUNCAO
           MOVE 'DCOM0605'                 TO WRK-TELA
                                              COMU-TRANSACAO-PFK
           MOVE 'A'                        TO COMU-PFK-PFK
           MOVE COMU-AREA-PFK              TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1240-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1280-TRATAR-ENTER          SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES            TO WRK-OUT-DPROD-FETM
                                     WRK-OUT-DSPROD-FETM
                                     WRK-OUT-DSITUA-FETM.

           IF  WRK-INP-FASE-FETM   EQUAL '2'
               MOVE '1'            TO WRK-INP-FASE-FETM
                                      WRK-OUT-FASE-FETM
               MOVE 'DIGITE OS DADOS E TECLE <ENTER>'
                                   TO WRK-OUT-MENSAG-FETM
               GO TO 1280-99-FIM
           END-IF.

           IF  WRK-INP-AGENC-FETM     EQUAL SPACES AND
               WRK-INP-CONTA-FETM     EQUAL SPACES AND
               WRK-INP-CNPJ1-FETM     EQUAL SPACES AND
               WRK-INP-FILI1-FETM     EQUAL SPACES AND
               WRK-INP-CTRL1-FETM     EQUAL SPACES
               MOVE 'DADOS DO CEDENTE OBRIGATORIOS'
                                      TO  WRK-OUT-MENSAG-FETM
               GO   TO 1280-99-FIM
           END-IF.

           IF (WRK-INP-CNPJ1-FETM     NOT EQUAL SPACES OR
               WRK-INP-FILI1-FETM     NOT EQUAL SPACES OR
               WRK-INP-CTRL1-FETM     NOT EQUAL SPACES) AND
              (WRK-INP-CNPJ2-FETM     NOT EQUAL SPACES OR
               WRK-INP-FILI2-FETM     NOT EQUAL SPACES OR
               WRK-INP-CTRL2-FETM     NOT EQUAL SPACES)
               MOVE 'NAO HA PESQUISA PARA CNPJ/CPF DO CEDENTE E SACADO'
                                      TO  WRK-OUT-MENSAG-FETM
               GO   TO 1280-99-FIM
           END-IF.

           IF  WRK-INP-AGENC-FETM     NOT EQUAL SPACES AND
              (WRK-INP-CNPJ1-FETM     NOT EQUAL SPACES OR
               WRK-INP-FILI1-FETM     NOT EQUAL SPACES OR
               WRK-INP-CTRL1-FETM     NOT EQUAL SPACES)
               MOVE 'NAO HA PESQUISA PARA CNPJ/CPF DO CEDENTE E AGENCIA
      -             ' E CONTA'
                                      TO  WRK-OUT-MENSAG-FETM
               GO   TO 1280-99-FIM
           END-IF.

           IF  WRK-INP-AGENC-FETM  NOT EQUAL SPACES
               IF  WRK-INP-AGENC-FETM NOT NUMERIC OR
                   WRK-INP-AGENC-FETM EQUAL ZEROS
                   MOVE 'AGENCIA INVALIDA'
                                        TO  WRK-OUT-MENSAG-FETM
                   MOVE WRK-49369       TO  WRK-OUT-AGENC-ATTR-FETM
                                            WRK-OUT-CONTA-ATTR-FETM
                   GO   TO 1280-99-FIM
               ELSE
                   PERFORM 1253-10-VALIDAR-PERFIL
                   IF  WRK-INCONSIS     NOT EQUAL SPACES
                       GO TO 1280-99-FIM
                   END-IF
               END-IF
           END-IF.

           IF  WRK-INP-CONTA-FETM  NOT EQUAL SPACES
               IF  WRK-INP-CONTA-FETM NOT NUMERIC OR
                   WRK-INP-CONTA-FETM EQUAL ZEROS
                   MOVE 'CONTA INVALIDA'
                                        TO  WRK-OUT-MENSAG-FETM
                   MOVE WRK-49369       TO  WRK-OUT-AGENC-ATTR-FETM
                                            WRK-OUT-CONTA-ATTR-FETM
                   GO   TO 1280-99-FIM
               END-IF
           END-IF.

           IF (WRK-INP-AGENC-FETM  NOT EQUAL SPACES AND
               WRK-INP-CONTA-FETM  EQUAL SPACES) OR
              (WRK-INP-AGENC-FETM  EQUAL SPACES AND
               WRK-INP-CONTA-FETM  NOT EQUAL SPACES)
               MOVE 'OBRIGATORIO INFORMAR AGENCIA E CONTA'
                                        TO  WRK-OUT-MENSAG-FETM
               MOVE WRK-49369           TO  WRK-OUT-AGENC-ATTR-FETM
                                            WRK-OUT-CONTA-ATTR-FETM
               GO   TO 1280-99-FIM
           END-IF.

           IF  WRK-INP-CNPJ1-FETM       NOT EQUAL SPACES
               IF  WRK-INP-CNPJ1-FETM   NOT NUMERIC OR
                   WRK-INP-CNPJ1-FETM   EQUAL ZEROS
                   MOVE 'CNPJ/CPF CEDENTE INVALIDO'
                                        TO  WRK-OUT-MENSAG-FETM
                   MOVE WRK-49369       TO  WRK-OUT-CNPJ1-ATTR-FETM
                   GO   TO 1280-99-FIM
               END-IF
           END-IF.

           IF  WRK-INP-FILI1-FETM       NOT EQUAL SPACES
               IF  WRK-INP-FILI1-FETM   NOT NUMERIC
                   MOVE 'CNPJ/CPF CEDENTE INVALIDO'
                                        TO  WRK-OUT-MENSAG-FETM
                   MOVE WRK-49369       TO  WRK-OUT-FILI1-ATTR-FETM
                   GO   TO 1280-99-FIM
               END-IF
           END-IF.

           IF  WRK-INP-CTRL1-FETM       NOT EQUAL SPACES
               IF  WRK-INP-CTRL1-FETM   NOT NUMERIC
                   MOVE 'CNPJ/CPF CEDENTE INVALIDO'
                                        TO  WRK-OUT-MENSAG-FETM
                   MOVE WRK-49369       TO  WRK-OUT-CTRL1-ATTR-FETM
                   GO   TO 1280-99-FIM
               END-IF
           END-IF.

           IF  WRK-INP-CNPJ2-FETM       NOT EQUAL SPACES
               IF  WRK-INP-CNPJ2-FETM   NOT NUMERIC OR
                   WRK-INP-CNPJ2-FETM   EQUAL ZEROS
                   MOVE 'CNPJ/CPF SACADO INVALIDO'
                                        TO  WRK-OUT-MENSAG-FETM
                   MOVE WRK-49369       TO  WRK-OUT-CNPJ2-ATTR-FETM
                   GO   TO 1280-99-FIM
               END-IF
           END-IF.

           IF  WRK-INP-FILI2-FETM       NOT EQUAL SPACES
               IF  WRK-INP-FILI2-FETM   NOT NUMERIC
                   MOVE 'CNPJ/CPF SACADO INVALIDO'
                                        TO  WRK-OUT-MENSAG-FETM
                   MOVE WRK-49369       TO  WRK-OUT-FILI2-ATTR-FETM
                   GO   TO 1280-99-FIM
               END-IF
           END-IF.

           IF  WRK-INP-CTRL2-FETM       NOT EQUAL SPACES
               IF  WRK-INP-CTRL2-FETM   NOT NUMERIC
                   MOVE 'CNPJ/CPF SACADO INVALIDO'
                                        TO  WRK-OUT-MENSAG-FETM
                   MOVE WRK-49369       TO  WRK-OUT-CTRL2-ATTR-FETM
                   GO   TO 1280-99-FIM
               END-IF
           END-IF.

           IF (WRK-INP-CNPJ1-FETM       NOT EQUAL SPACES AND
               WRK-INP-CTRL1-FETM       EQUAL SPACES) OR
              (WRK-INP-CNPJ1-FETM       EQUAL SPACES AND
               WRK-INP-CTRL1-FETM       NOT EQUAL SPACES)
               MOVE 'CNPJ/CPF CEDENTE INVALIDO'
                                        TO  WRK-OUT-MENSAG-FETM
               MOVE WRK-49369           TO  WRK-OUT-CNPJ1-ATTR-FETM
                                            WRK-OUT-FILI1-ATTR-FETM
                                            WRK-OUT-CTRL1-ATTR-FETM
               GO   TO 1280-99-FIM
           END-IF.

           IF (WRK-INP-CNPJ2-FETM       NOT EQUAL SPACES AND
               WRK-INP-CTRL2-FETM       EQUAL SPACES) OR
              (WRK-INP-CNPJ2-FETM       EQUAL SPACES AND
               WRK-INP-CTRL2-FETM       NOT EQUAL SPACES)
               MOVE 'CNPJ/CPF SACADO INVALIDO'
                                        TO  WRK-OUT-MENSAG-FETM
               MOVE WRK-49369           TO  WRK-OUT-CNPJ2-ATTR-FETM
                                            WRK-OUT-FILI2-ATTR-FETM
                                            WRK-OUT-CTRL2-ATTR-FETM
               GO   TO 1280-99-FIM
           END-IF.

           IF  WRK-INP-PROD-FETM        NOT EQUAL SPACES
               IF  WRK-INP-PROD-FETM    NOT NUMERIC OR
                   WRK-INP-PROD-FETM    EQUAL ZEROS
                   MOVE 'PRODUTO INVALIDO'
                                        TO  WRK-OUT-MENSAG-FETM
                   MOVE WRK-49369       TO  WRK-OUT-PROD-ATTR-FETM
                   GO   TO 1280-99-FIM
               END-IF
           ELSE
               MOVE SPACES              TO WRK-OUT-DPROD-FETM
           END-IF.

           IF  WRK-INP-SPROD-FETM       NOT EQUAL SPACES
               IF  WRK-INP-SPROD-FETM   NOT NUMERIC OR
                   WRK-INP-SPROD-FETM   EQUAL ZEROS
                   MOVE 'SUBPRODUTO INVALIDO'
                                        TO  WRK-OUT-MENSAG-FETM
                   MOVE WRK-49369       TO  WRK-OUT-SPROD-ATTR-FETM
                   GO   TO 1280-99-FIM
               END-IF
           ELSE
               MOVE SPACES              TO WRK-OUT-DSPROD-FETM
           END-IF.

           IF  WRK-INP-SITUA-FETM       NOT EQUAL SPACES
               IF  WRK-INP-SITUA-FETM   NOT NUMERIC OR
                   WRK-INP-SITUA-FETM   EQUAL ZEROS
                   MOVE 'SITUACAO INVALIDA'
                                        TO  WRK-OUT-MENSAG-FETM
                   MOVE WRK-49369       TO  WRK-OUT-SITUA-ATTR-FETM
                   GO   TO 1280-99-FIM
               END-IF
           ELSE
               MOVE SPACES              TO WRK-OUT-DSITUA-FETM
           END-IF.

           IF  WRK-INP-CNPJ2-FETM       NOT EQUAL SPACES AND
              (WRK-INP-AGENC-FETM       EQUAL SPACES OR
               WRK-INP-CONTA-FETM       EQUAL SPACES)
               MOVE 'CONSULTA SACADO NECESSITA AGENCIA E CONTA'
                                        TO  WRK-OUT-MENSAG-FETM
               MOVE WRK-49369           TO  WRK-OUT-AGENC-ATTR-FETM
                                            WRK-OUT-CONTA-ATTR-FETM
               GO   TO 1280-99-FIM
           END-IF.

           IF  WRK-INP-CNPJ1-FETM       NOT EQUAL SPACES AND
               WRK-INP-CNPJ2-FETM       NOT EQUAL SPACES
               MOVE 'INFORMAR SOMENTE CPF/CNPJ DO CEDENTE OU SACADO'
                                        TO  WRK-OUT-MENSAG-FETM
               MOVE WRK-49369           TO  WRK-OUT-CNPJ1-ATTR-FETM
                                            WRK-OUT-FILI1-ATTR-FETM
                                            WRK-OUT-CTRL1-ATTR-FETM
                                            WRK-OUT-CNPJ2-ATTR-FETM
                                            WRK-OUT-FILI2-ATTR-FETM
                                            WRK-OUT-CTRL2-ATTR-FETM
               GO   TO 1280-99-FIM
           END-IF.

           IF   WRK-INP-CNPJ1-FETM     NOT EQUAL SPACES OR
                WRK-INP-FILI1-FETM     NOT EQUAL SPACES OR
                WRK-INP-CTRL1-FETM     NOT EQUAL SPACES
                MOVE WRK-INP-CNPJ1-FETM     TO  WRK-NUMERO
                IF  WRK-INP-FILI1-FETM      EQUAL SPACES
                    MOVE SPACES             TO WRK-FILIAL
                ELSE
                    MOVE WRK-INP-FILI1-FETM TO WRK-FILIAL
                END-IF
                MOVE SPACES                 TO WRK-CONTROLE

FS2511*         CALL 'BRAD0110'     USING  WRK-CPFCGC
FS2511*                                      WRK-CONTROLE
FS2511          CALL 'BRAD2000'     USING  WRK-CPFCGC
                                           WRK-CONTROLE

                IF  RETURN-CODE      NOT EQUAL  ZEROS
                    MOVE WRK-49369      TO WRK-OUT-CNPJ1-ATTR-FETM
                                           WRK-OUT-FILI1-ATTR-FETM
                                           WRK-OUT-CTRL1-ATTR-FETM
                    MOVE 'CNPJ/CPF INCONSISTENTE'
                                   TO WRK-OUT-MENSAG-FETM
                    GO   TO 1280-99-FIM
               ELSE
                    IF WRK-INP-CTRL1-FETM  NOT EQUAL  WRK-CONTROLE
                       MOVE WRK-49369   TO WRK-OUT-CTRL1-ATTR-FETM
                       MOVE 'DIGITO DO CPF/CNPJ INVALIDO'
                                   TO WRK-OUT-MENSAG-FETM
                       GO   TO 1280-99-FIM
                    END-IF
               END-IF
           END-IF.

           IF   WRK-INP-CNPJ2-FETM     NOT EQUAL SPACES OR
                WRK-INP-FILI2-FETM     NOT EQUAL SPACES OR
                WRK-INP-CTRL2-FETM     NOT EQUAL SPACES
                MOVE WRK-INP-CNPJ2-FETM     TO  WRK-NUMERO
                IF  WRK-INP-FILI2-FETM      EQUAL SPACES
                    MOVE SPACES             TO  WRK-FILIAL
                ELSE
                    MOVE WRK-INP-FILI2-FETM TO  WRK-FILIAL
                END-IF
                MOVE SPACES                 TO  WRK-CONTROLE

FS2511*         CALL 'BRAD0110'     USING  WRK-CPFCGC
FS2511*                                      WRK-CONTROLE
FS2511          CALL 'BRAD2000'     USING  WRK-CPFCGC
                                           WRK-CONTROLE

                IF  RETURN-CODE      NOT EQUAL  ZEROS
                    MOVE WRK-49369      TO WRK-OUT-CNPJ2-ATTR-FETM
                                           WRK-OUT-FILI2-ATTR-FETM
                                           WRK-OUT-CTRL2-ATTR-FETM
                    MOVE 'CNPJ/CPF INCONSISTENTE'
                                   TO WRK-OUT-MENSAG-FETM
                    GO   TO 1280-99-FIM
               ELSE
                    IF WRK-INP-CTRL2-FETM  NOT EQUAL  WRK-CONTROLE
                       MOVE WRK-49369   TO WRK-OUT-CTRL2-ATTR-FETM
                       MOVE 'DIGITO DO CPF/CNPJ INVALIDO'
                                   TO WRK-OUT-MENSAG-FETM
                       GO   TO 1280-99-FIM
                    END-IF
               END-IF
           END-IF.

           IF  WRK-INP-PROD-FETM        EQUAL SPACES AND
               WRK-INP-SPROD-FETM       NOT EQUAL SPACES
               MOVE 'CODIGOS DO PRODUTO E SUBPRODUTO SAO OBRIGATORIOS'
                                        TO WRK-OUT-MENSAG-FETM
               MOVE WRK-49369           TO WRK-OUT-PROD-ATTR-FETM
               GO   TO 1280-99-FIM
           END-IF

           IF  WRK-INP-AGENC-FETM          NOT EQUAL SPACES
               PERFORM 2272-1-VALIDAR-AGENCIA
           END-IF.

           IF  WRK-INCONSIS                EQUAL SPACES
               PERFORM 2272-OBTER-NOMES
           END-IF.

           IF  WRK-INCONSIS                EQUAL SPACES
               MOVE WRK-241                TO WRK-OUT-AGENC-ATTR-FETM
                                              WRK-OUT-CONTA-ATTR-FETM
                                              WRK-OUT-CNPJ1-ATTR-FETM
                                              WRK-OUT-FILI1-ATTR-FETM
                                              WRK-OUT-CTRL1-ATTR-FETM
                                              WRK-OUT-CNPJ2-ATTR-FETM
                                              WRK-OUT-FILI2-ATTR-FETM
                                              WRK-OUT-CTRL2-ATTR-FETM
                                              WRK-OUT-PROD-ATTR-FETM
                                              WRK-OUT-SPROD-ATTR-FETM
                                              WRK-OUT-SITUA-ATTR-FETM
               MOVE WRK-49361              TO WRK-OUT-COMANDO-ATTR-FETM
               IF  WRK-OUT-MENSAG-FETM     EQUAL SPACES
                   MOVE WRK-MENS05         TO  WRK-OUT-MENSAG-FETM
               END-IF
               MOVE '2'                    TO  WRK-OUT-FASE-FETM
           END-IF.

      *----------------------------------------------------------------*
       1280-99-FIM.                    EXIT.
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
           MOVE WRK-INP-AGENC-FETM-N   TO ROTENT-6437-AGEPESQ-ENV.
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
                                          TO WRK-OUT-MENSAG-FETM
           END-IF.

      *----------------------------------------------------------------*
       1253-10-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2272-1-VALIDAR-AGENCIA          SECTION.
      *----------------------------------------------------------------*

           INITIALIZE ENTRADA-ROT6416
                      SAIDA-ROT6416

           MOVE 237                        TO  ENT6416-BANCO
           MOVE WRK-INP-AGENC-FETM         TO  ENT6416-DEPDC
           MOVE 'DCOM6416'                 TO  WRK-MODULO
           CALL WRK-MODULO              USING  ENTRADA-ROT6416
                                               SAIDA-ROT6416
                                               ERRO-AREA
                                               WRK-SQLCA

           EVALUATE SAI6416-COD-RETORNO
             WHEN '0000'
               CONTINUE

             WHEN '0099'
               MOVE 'DB2'                  TO  ERR-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO

             WHEN OTHER
               MOVE '*'                    TO  WRK-INCONSIS
               MOVE WRK-49369              TO  WRK-OUT-AGENC-ATTR-FETM
               MOVE SAI6416-MSG-RETORNO    TO  WRK-OUT-MENSAG-FETM

           END-EVALUATE.

      *----------------------------------------------------------------*
       1272-1-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2272-OBTER-NOMES                SECTION.
      *----------------------------------------------------------------*


           IF  WRK-INP-PROD-FETM       NOT EQUAL SPACES AND LOW-VALUES
               PERFORM 7100-OBTER-NOME-PROD
           ELSE
               MOVE SPACES                 TO  WRK-OUT-DPROD-FETM
           END-IF.

           IF  WRK-INP-SPROD-FETM      NOT EQUAL SPACES AND LOW-VALUES
               PERFORM 7200-OBTER-NOME-SUBP
           ELSE
               MOVE SPACES                 TO  WRK-OUT-DSPROD-FETM
           END-IF.

           IF  WRK-INP-SITUA-FETM      NOT EQUAL SPACES AND LOW-VALUES
               PERFORM 7300-OBTER-NOME-SITU
           ELSE
               MOVE SPACES                 TO  WRK-OUT-SITUA-FETM
           END-IF.

      *----------------------------------------------------------------*
       2272-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       7100-OBTER-NOME-PROD                SECTION.
      *----------------------------------------------------------------*

           INITIALIZE ENTRADA-ROT4001
                      SAIDA-ROT4001

           MOVE WRK-COD-USER               TO  ENT4001-CFUNC-BDSCO
           MOVE LNK-IO-LTERM               TO  ENT4001-CTERM
           MOVE WRK-INP-PROD-FETM          TO  ENT4001-CPRODT

           MOVE WRK-INP-TIMESTA-FETM(9:2) TO ENT4001-DT-MOVTO(1:2)
           MOVE WRK-INP-TIMESTA-FETM(6:2) TO ENT4001-DT-MOVTO(4:2)
           MOVE WRK-INP-TIMESTA-FETM(1:4) TO ENT4001-DT-MOVTO(7:4)

           INSPECT ENT4001-DT-MOVTO REPLACING ALL ' ' BY '.'.

           MOVE 'DCOM4001'                 TO  WRK-MODULO
           CALL WRK-MODULO              USING  ENTRADA-ROT4001
                                               SAIDA-ROT4001
                                               ERRO-AREA
                                               WRK-SQLCA

           EVALUATE SAI4001-COD-RETORNO
             WHEN '0000'
               MOVE SAI4001-IPRODT         TO  WRK-OUT-DPROD-FETM

             WHEN '0099'
               MOVE 'DB2'                  TO  ERR-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO

             WHEN OTHER
               MOVE '*'                    TO  WRK-INCONSIS
               MOVE WRK-49369              TO  WRK-OUT-PROD-ATTR-FETM
               MOVE 'PRODUTO INEXISTENTE'  TO  WRK-OUT-MENSAG-FETM

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
           MOVE WRK-INP-PROD-FETM          TO  ENT4049-CPRODT
           MOVE WRK-INP-SPROD-FETM         TO  ENT4049-CSPROD-DESC-COML
           MOVE 'DCOM4049'                 TO  WRK-MODULO
           CALL WRK-MODULO              USING  ENTRADA-ROT4049
                                               SAIDA-ROT4049
                                               ERRO-AREA
                                               WRK-SQLCA

           EVALUATE SAI4049-COD-RETORNO
             WHEN '0000'
               MOVE SAI4049-IRSUMO-SPROD-DESC
                                           TO  WRK-OUT-DSPROD-FETM

             WHEN '0099'
               MOVE 'DB2'                  TO  ERR-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO

             WHEN OTHER
               MOVE '*'                    TO  WRK-INCONSIS
               MOVE WRK-49369              TO  WRK-OUT-SPROD-ATTR-FETM
               MOVE 'SUBPRODUTO INEXISTENTE'
                                           TO  WRK-OUT-MENSAG-FETM

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
           MOVE WRK-INP-SITUA-FETM         TO  ENT4309-CSIT-DESC-COML
           MOVE 'DCOM4309'                 TO  WRK-MODULO
           CALL WRK-MODULO              USING  ENTRADA-ROT4309
                                               SAIDA-ROT4309
                                               ERRO-AREA
                                               WRK-SQLCA

           EVALUATE SAI4309-COD-RETORNO
             WHEN '0000'
               MOVE SAI4309-ISIT-DESC-COML
                                           TO  WRK-OUT-DSITUA-FETM

             WHEN '0099'
               MOVE 'DB2'                  TO  ERR-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO

             WHEN OTHER
               MOVE '*'                    TO  WRK-INCONSIS
               MOVE WRK-49369              TO  WRK-OUT-SITUA-ATTR-FETM
               MOVE 'SITUACAO INEXISTENTE' TO  WRK-OUT-MENSAG-FETM

           END-EVALUATE.

      *----------------------------------------------------------------*
       7300-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA TRATA A PFK06 PARA REDIRECINAR PARA LISTA .       *
      *----------------------------------------------------------------*
       1250-TRATAR-CONFIRMACAO         SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-AREA-COMUN-TOTAL
                      COMU-AREA.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE WRK-INP-TIMESTA-FETM       TO WRK-ENT-TIMESTA
                                              COMU-TIMESTA

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE 'CP'                       TO WRK-ENT-OPCAO

           PERFORM 8200-ACESSAR-DCOM7995
           MOVE WRK-SAI-DADOS              TO WRK-AREA-COMUN-TOTAL.

           MOVE 'I '                       TO WRK-ENT-OPCAO

           PERFORM VARYING IND-1               FROM 1 BY 1
                     UNTIL IND-1               GREATER 15


               IF  WRK-CHAMADOR(IND-1)         EQUAL SPACES
                   MOVE 'DCOM0605'             TO WRK-CHAMADOR(IND-1)
                                                  COMU-TRANSACAO
                   MOVE 99                     TO IND-1
               END-IF
           END-PERFORM.

           IF  WRK-INP-AGENC-FETM          NOT EQUAL SPACES
               MOVE WRK-INP-AGENC-FETM     TO WRK-AGENCIA
           ELSE
               MOVE ZEROS                  TO WRK-AGENCIA
           END-IF.

           IF  WRK-INP-CONTA-FETM          NOT EQUAL SPACES
               MOVE WRK-INP-CONTA-FETM     TO WRK-CONTA
           ELSE
               MOVE ZEROS                  TO WRK-CONTA
           END-IF.

           IF  WRK-INP-CNPJ1-FETM          NOT EQUAL SPACES
               MOVE WRK-INP-CNPJ1-FETM     TO WRK-CCNPJ-CEDENTE
           ELSE
               MOVE ZEROS                  TO WRK-CCNPJ-CEDENTE
           END-IF.

           IF  WRK-INP-FILI1-FETM          NOT EQUAL SPACES
               MOVE WRK-INP-FILI1-FETM     TO WRK-FILIAL-CEDENTE
           ELSE
               MOVE SPACES                 TO WRK-FILIAL-CEDENTE
           END-IF.

           IF  WRK-INP-CTRL1-FETM          NOT EQUAL SPACES
               MOVE WRK-INP-CTRL1-FETM     TO WRK-CCTRL-CEDENTE
           ELSE
               MOVE ZEROS                  TO WRK-CCTRL-CEDENTE
           END-IF.

           IF  WRK-INP-CNPJ2-FETM          NOT EQUAL SPACES
               MOVE WRK-INP-CNPJ2-FETM     TO WRK-CCNPJ-SACADO
           ELSE
               MOVE ZEROS                  TO WRK-CCNPJ-SACADO
           END-IF.

           IF  WRK-INP-FILI2-FETM          NOT EQUAL SPACES
               MOVE WRK-INP-FILI2-FETM     TO WRK-FILIAL-SACADO
           ELSE
               MOVE SPACES                 TO WRK-FILIAL-SACADO
           END-IF.

           IF  WRK-INP-CTRL2-FETM          NOT EQUAL SPACES
               MOVE WRK-INP-CTRL2-FETM     TO WRK-CCTRL-SACADO
           ELSE
               MOVE ZEROS                  TO WRK-CCNPJ-SACADO
           END-IF.

           IF  WRK-INP-PROD-FETM           NOT EQUAL SPACES
               MOVE WRK-INP-PROD-FETM      TO WRK-PRODUTO-LISTA
           ELSE
               MOVE ZEROS                  TO WRK-PRODUTO-LISTA
           END-IF.

           IF  WRK-INP-SPROD-FETM          NOT EQUAL SPACES
               MOVE WRK-INP-SPROD-FETM     TO WRK-SUBPRODUTO-LISTA
           ELSE
               MOVE ZEROS                  TO WRK-SUBPRODUTO-LISTA
           END-IF.

           IF  WRK-INP-SITUA-FETM          NOT EQUAL SPACES
               MOVE WRK-INP-SITUA-FETM     TO WRK-SITUACAO-LISTA
           ELSE
               MOVE ZEROS                  TO WRK-SITUACAO-LISTA
           END-IF.

           MOVE WRK-AREA-COMUN-TOTAL       TO WRK-ENT-DADOS.
           PERFORM 8200-ACESSAR-DCOM7995

           MOVE LENGTH OF COMU-AREA        TO COMU-LL

           MOVE  'DCOM0605'                TO COMU-TRANSACAO
           MOVE  'DCOM0606'                TO WRK-TELA
           MOVE  COMU-AREA                 TO WRK-MENSAGEM
           MOVE  WRK-CHNG                  TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       1250-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA CARREGA A LISTA PARA AS OPCOES.                   *
      *----------------------------------------------------------------*
       1260-TRATAR-LISTA               SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INP-LINHA-FETM          EQUAL 17 AND
              (WRK-INP-COLUNA-FETM         EQUAL 33 OR 34 OR 35)
               PERFORM 8100-OBTER-LISTA-PROD
           ELSE
              IF  WRK-INP-LINHA-FETM       EQUAL 18 AND
                 (WRK-INP-COLUNA-FETM      EQUAL 33 OR 34 OR 35)
                  IF  WRK-INP-PROD-FETM    EQUAL SPACES OR LOW-VALUES
                     MOVE 'LISTA SOMENTE DISPONIVEL SE PRODUTO PREENCHID
      -                   'O'              TO WRK-OUT-MENSAG-FETM
                  ELSE
                     PERFORM 8200-OBTER-LISTA-SUBP
                  END-IF
              ELSE
                  IF  WRK-INP-LINHA-FETM   EQUAL 20 AND
                     (WRK-INP-COLUNA-FETM  EQUAL 33 OR 34 OR 35)
                      PERFORM 8300-OBTER-LISTA-SITU
                  ELSE
                      MOVE 'LISTA DISPONIVEL PARA PRODUTO/SUBPROD/SITUAC
      -                    'AO - POSICIONAMENTO ERRADO'
                                           TO WRK-OUT-MENSAG-FETM
                  END-IF
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       1250-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       8100-OBTER-LISTA-PROD               SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-AREA-COMUN-TOTAL
                      COMU-AREA-3903.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE WRK-INP-TIMESTA-FETM       TO WRK-ENT-TIMESTA
                                              COMU-TIMESTA

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE 'CP'                       TO WRK-ENT-OPCAO

           PERFORM 8200-ACESSAR-DCOM7995
           MOVE WRK-SAI-DADOS              TO WRK-AREA-COMUN-TOTAL.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE WRK-INP-TIMESTA-FETM       TO WRK-ENT-TIMESTA
                                              COMU-TIMESTA
           MOVE 'EP'                       TO WRK-ENT-OPCAO

           PERFORM 8200-ACESSAR-DCOM7995
           MOVE LENGTH OF COMU-AREA-3903   TO COMU-LL-3903
           MOVE WRK-INP-TIMESTA-FETM       TO COMU-TIMESTA-3903
           MOVE WRK-INP-PROD-FETM          TO COMU-COD-PROD-3903
           MOVE 'DCOM0605'                 TO COMU-TRANSACAO-3903
           MOVE 'DCOM3903'                 TO WRK-TELA
           MOVE  WRK-CHNG                  TO WRK-FUNCAO.

           PERFORM VARYING IND-1               FROM 1 BY 1
                     UNTIL IND-1               GREATER 15

               IF  WRK-CHAMADOR(IND-1)         EQUAL SPACES
                   MOVE 'DCOM0605'             TO WRK-CHAMADOR(IND-1)
                                                  COMU-TRANSACAO
                   MOVE 99                     TO IND-1
               END-IF
           END-PERFORM.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE 'I '                       TO WRK-ENT-OPCAO

           IF  WRK-INP-AGENC-FETM          NOT EQUAL SPACES
               MOVE WRK-INP-AGENC-FETM     TO WRK-AGENCIA
           END-IF.

           IF  WRK-INP-CONTA-FETM          NOT EQUAL SPACES
               MOVE WRK-INP-CONTA-FETM     TO WRK-CONTA
           END-IF.

           IF  WRK-INP-CNPJ1-FETM          NOT EQUAL SPACES
               MOVE WRK-INP-CNPJ1-FETM     TO WRK-CCNPJ-CEDENTE
           END-IF.

           IF  WRK-INP-FILI1-FETM          NOT EQUAL SPACES
               MOVE WRK-INP-FILI1-FETM     TO WRK-FILIAL-CEDENTE
           END-IF.

           IF  WRK-INP-CTRL1-FETM          NOT EQUAL SPACES
               MOVE WRK-INP-CTRL1-FETM     TO WRK-CCTRL-CEDENTE
           END-IF.

           IF  WRK-INP-CNPJ2-FETM          NOT EQUAL SPACES
               MOVE WRK-INP-CNPJ2-FETM     TO WRK-CCNPJ-SACADO
           END-IF.

           IF  WRK-INP-FILI2-FETM          NOT EQUAL SPACES
               MOVE WRK-INP-FILI2-FETM     TO WRK-FILIAL-SACADO
           END-IF.

           IF  WRK-INP-CTRL2-FETM          NOT EQUAL SPACES
               MOVE WRK-INP-CTRL2-FETM     TO WRK-CCTRL-SACADO
           END-IF.

           IF  WRK-INP-PROD-FETM           NOT EQUAL SPACES
               MOVE WRK-INP-PROD-FETM      TO WRK-PRODUTO-LISTA
           ELSE
               MOVE ZEROS                  TO WRK-PRODUTO-LISTA
           END-IF.

           IF  WRK-INP-SPROD-FETM          NOT EQUAL SPACES
               MOVE WRK-INP-SPROD-FETM     TO WRK-SUBPRODUTO-LISTA
           ELSE
               MOVE ZEROS                  TO WRK-SUBPRODUTO-LISTA
           END-IF.

           IF  WRK-INP-SITUA-FETM          NOT EQUAL SPACES
               MOVE WRK-INP-SITUA-FETM     TO WRK-SITUACAO-LISTA
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

           INITIALIZE WRK-AREA-COMUN-TOTAL
                      COMU-AREA-3904.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE WRK-INP-TIMESTA-FETM       TO WRK-ENT-TIMESTA
                                              COMU-TIMESTA

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE 'CP'                       TO WRK-ENT-OPCAO

           PERFORM 8200-ACESSAR-DCOM7995
           MOVE WRK-SAI-DADOS              TO WRK-AREA-COMUN-TOTAL.

           MOVE WRK-SAI-DADOS              TO WRK-AREA-COMUN-TOTAL.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE WRK-INP-TIMESTA-FETM       TO WRK-ENT-TIMESTA
                                              COMU-TIMESTA
           MOVE 'EP'                       TO WRK-ENT-OPCAO

           PERFORM 8200-ACESSAR-DCOM7995
           MOVE LENGTH OF COMU-AREA-3904   TO COMU-LL-3904

           MOVE WRK-INP-TIMESTA-FETM       TO COMU-TIMESTA-3904
           MOVE WRK-INP-PROD-FETM          TO COMU-COD-PROD-3904
           MOVE WRK-INP-SPROD-FETM         TO COMU-COD-SUBP-3904
           MOVE ZEROS                      TO COMU-CNPJCPF-3904
           MOVE 'DCOM0605'                 TO COMU-TRANSACAO-3904
           MOVE 'DCOM3904'                 TO WRK-TELA
           MOVE  COMU-AREA-3904            TO WRK-MENSAGEM
           MOVE  WRK-CHNG                  TO WRK-FUNCAO.

           PERFORM VARYING IND-1               FROM 1 BY 1
                     UNTIL IND-1               GREATER 15


               IF  WRK-CHAMADOR(IND-1)         EQUAL SPACES
                   MOVE 'DCOM0605'             TO WRK-CHAMADOR(IND-1)
                                                  COMU-TRANSACAO
                   MOVE 99                     TO IND-1
               END-IF
           END-PERFORM.

           IF  WRK-INP-AGENC-FETM          NOT EQUAL SPACES
               MOVE WRK-INP-AGENC-FETM     TO WRK-AGENCIA
           END-IF.

           IF  WRK-INP-CONTA-FETM          NOT EQUAL SPACES
               MOVE WRK-INP-CONTA-FETM     TO WRK-CONTA
           END-IF.

           IF  WRK-INP-CNPJ1-FETM          NOT EQUAL SPACES
               MOVE WRK-INP-CNPJ1-FETM     TO WRK-CCNPJ-CEDENTE
           END-IF.

           IF  WRK-INP-FILI1-FETM          NOT EQUAL SPACES
               MOVE WRK-INP-FILI1-FETM     TO WRK-FILIAL-CEDENTE
           END-IF.

           IF  WRK-INP-CTRL1-FETM          NOT EQUAL SPACES
               MOVE WRK-INP-CTRL1-FETM     TO WRK-CCTRL-CEDENTE
           END-IF.

           IF  WRK-INP-CNPJ2-FETM          NOT EQUAL SPACES
               MOVE WRK-INP-CNPJ2-FETM     TO WRK-CCNPJ-SACADO
           END-IF.

           IF  WRK-INP-FILI2-FETM          NOT EQUAL SPACES
               MOVE WRK-INP-FILI2-FETM     TO WRK-FILIAL-SACADO
           END-IF.

           IF  WRK-INP-CTRL2-FETM          NOT EQUAL SPACES
               MOVE WRK-INP-CTRL2-FETM     TO WRK-CCTRL-SACADO
           END-IF.

           IF  WRK-INP-PROD-FETM           NOT EQUAL SPACES
               MOVE WRK-INP-PROD-FETM      TO WRK-PRODUTO-LISTA
           ELSE
               MOVE ZEROS                  TO WRK-PRODUTO-LISTA
           END-IF.

           IF  WRK-INP-SPROD-FETM          NOT EQUAL SPACES
               MOVE WRK-INP-SPROD-FETM     TO WRK-SUBPRODUTO-LISTA
           ELSE
               MOVE ZEROS                  TO WRK-SUBPRODUTO-LISTA
           END-IF.

           IF  WRK-INP-SITUA-FETM          NOT EQUAL SPACES
               MOVE WRK-INP-SITUA-FETM     TO WRK-SITUACAO-LISTA
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
           MOVE WRK-INP-TIMESTA-FETM       TO WRK-ENT-TIMESTA
                                              COMU-TIMESTA

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE 'CP'                       TO WRK-ENT-OPCAO

           PERFORM 8200-ACESSAR-DCOM7995
           MOVE WRK-SAI-DADOS              TO WRK-AREA-COMUN-TOTAL.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE WRK-INP-TIMESTA-FETM       TO WRK-ENT-TIMESTA
                                              COMU-TIMESTA
           MOVE 'EP'                       TO WRK-ENT-OPCAO

           PERFORM 8200-ACESSAR-DCOM7995
           INITIALIZE COMU-AREA-3913
           MOVE LENGTH OF COMU-AREA-3913   TO COMU-LL-3913
           MOVE WRK-INP-TIMESTA-FETM       TO COMU-TIMESTA-3913

           MOVE ZEROS                      TO COMU-CSIT-DESC-3913
           MOVE 006                        TO COMU-CELMTO-DESC-3913
           MOVE 'DCOM0605'                 TO COMU-TRANSACAO-3913
           MOVE 'DCOM3913'                 TO WRK-TELA
           MOVE  COMU-AREA-3913            TO WRK-MENSAGEM
           MOVE  WRK-CHNG                  TO WRK-FUNCAO.

           PERFORM VARYING IND-1               FROM 1 BY 1
                     UNTIL IND-1               GREATER 15


               IF  WRK-CHAMADOR(IND-1)         EQUAL SPACES
                   MOVE 'DCOM0605'             TO WRK-CHAMADOR(IND-1)
                                                  COMU-TRANSACAO
                   MOVE 99                     TO IND-1
               END-IF
           END-PERFORM.

           IF  WRK-INP-AGENC-FETM          NOT EQUAL SPACES
               MOVE WRK-INP-AGENC-FETM     TO WRK-AGENCIA
           END-IF.

           IF  WRK-INP-CONTA-FETM          NOT EQUAL SPACES
               MOVE WRK-INP-CONTA-FETM     TO WRK-CONTA
           END-IF.

           IF  WRK-INP-CNPJ1-FETM          NOT EQUAL SPACES
               MOVE WRK-INP-CNPJ1-FETM     TO WRK-CCNPJ-CEDENTE
           END-IF.

           IF  WRK-INP-FILI1-FETM          NOT EQUAL SPACES
               MOVE WRK-INP-FILI1-FETM     TO WRK-FILIAL-CEDENTE
           END-IF.

           IF  WRK-INP-CTRL1-FETM          NOT EQUAL SPACES
               MOVE WRK-INP-CTRL1-FETM     TO WRK-CCTRL-CEDENTE
           END-IF.

           IF  WRK-INP-CNPJ2-FETM          NOT EQUAL SPACES
               MOVE WRK-INP-CNPJ2-FETM     TO WRK-CCNPJ-SACADO
           END-IF.

           IF  WRK-INP-FILI2-FETM          NOT EQUAL SPACES
               MOVE WRK-INP-FILI2-FETM     TO WRK-FILIAL-SACADO
           END-IF.

           IF  WRK-INP-CTRL2-FETM          NOT EQUAL SPACES
               MOVE WRK-INP-CTRL2-FETM     TO WRK-CCTRL-SACADO
           END-IF.

           IF  WRK-INP-PROD-FETM           NOT EQUAL SPACES
               MOVE WRK-INP-PROD-FETM      TO WRK-PRODUTO-LISTA
           ELSE
               MOVE ZEROS                  TO WRK-PRODUTO-LISTA
           END-IF.

           IF  WRK-INP-SPROD-FETM          NOT EQUAL SPACES
               MOVE WRK-INP-SPROD-FETM     TO WRK-SUBPRODUTO-LISTA
           ELSE
               MOVE ZEROS                  TO WRK-SUBPRODUTO-LISTA
           END-IF.

           IF  WRK-INP-SITUA-FETM          NOT EQUAL SPACES
               MOVE WRK-INP-SITUA-FETM     TO WRK-SITUACAO-LISTA
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
      *  ESSA ROTINA CARREGA A TELA QUE SERA DEMONSTRADA PELO IMS.     *
      *----------------------------------------------------------------*
       1300-PROCESSAR-BRAD0660         SECTION.
      *----------------------------------------------------------------*

           MOVE LENGTH OF WRK-OUTPUT-FETM
                                       TO  WRK-660-LL-MENS-FETM
                                           WRK-OUT-LL-FETM

           MOVE LENGTH OF WRK-660-DCOMFETM
                                       TO  WRK-660-LL-AREA-FETM

           CALL 'BRAD0660'             USING WRK-OUTPUT-FETM
                                             WRK-660-DCOMFETM.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE RETURN-CODE        TO WRK-RETURN-CODE
               MOVE 10                 TO WRK-LOCAL-ERRO
               MOVE WRK-ERRO-BRAD0660  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-ISRT               TO WRK-FUNCAO.
           MOVE WRK-OUTPUT-FETM        TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
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

           IF  ERR-MODULO       NOT    EQUAL SPACES AND LOW-VALUES
               MOVE 'DCOM0605'         TO ERR-PGM
           ELSE
               MOVE 'SENHAS02'         TO ERR-PGM
               MOVE 'DCOM0605'         TO ERR-MODULO
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
