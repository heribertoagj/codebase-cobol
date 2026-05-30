      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. DCOM3935.
       AUTHOR.     FERNANDO LUIZ DE SANTI.
      *================================================================*
      *                      W  I  P  R  O                             *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   DCOM3935                                    *
      *    ANALISTA....:   FERNANDO LUIZ DE SANTI                      *
      *    ANALISTA DS.:   PAULO ROBERTO        - GRUPO 39             *
      *    DATA........:   06/10/2020                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   CONSULTAR OPTIN.                            *
      *----------------------------------------------------------------*
      *    TELAS.......:                                               *
      *    DCOMIXTM - CONSULTAR OPTIN.                                 *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    I#BRAD7C - AREA DE ERRO DA BRAD7100                         *
      *    I#DCOMSG - AREA DE MENSAGENS SISTEMA DCOM ONLINE            *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    BRAD0660 - INSERIR CARACTERES DE NULL NA MENSAGEM DE SAIDA  *
      *    BRAD7100 - TRATAMENTO DE ERROS QUANDO PROGRAMA INVALIDO     *
      *    DCOM5537 - OBTEM DADOS GERAL DA OPERACAO                    *
      *    DCOM5460 - LISTAR DETALHE DE BAIXA POR PARCELA              *
      *    POOL5005 - CONTROLE DO TRANSITO DE MENSAGENS NO IMS/DC      *
      *----------------------------------------------------------------*
      *    NAVEGACAO...:                                               *
      *    CHAMADO POR.: DCOM3924           CHAMA:    -                *
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
       77  FILLER                      PIC  X(050)         VALUE
           '*** DCOM3935 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

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
           '*** VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*

       01  WRK-AUXILIARES-GERAIS.
           05  IND-1                   PIC  9(002) COMP-3  VALUE ZEROS.
           05  IND-2                   PIC  9(002) COMP-3  VALUE ZEROS.
           05  WRK-RESTART             PIC  9(005) COMP-3  VALUE ZEROS.
           05  WRK-PAGINA              PIC  9(004) COMP-3  VALUE ZEROS.
           05  WRK-SQLCA               PIC  X(136)         VALUE SPACES.
           05  WRK-MODULO              PIC  X(008)         VALUE SPACES.
           05  WRK-INCONSISTENCIA      PIC  X(001)         VALUE SPACES.
           05  WRK-MSG-ERRO            PIC  X(079)         VALUE SPACES.

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

       01  WRK-ERRO-BRAD0660.
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
           '*** AREA DA TELA DCOMIXTM  - INPUT ***'.
      *----------------------------------------------------------------*

       01  WRK-INPUT-IXTM.
           05 FILLER                    PIC  X(018)        VALUE SPACES.
           05 WRK-INP-PFK-IXTM          PIC  X(001)        VALUE SPACES.
           05 WRK-INP-DADOS-IXTM.
            10 WRK-INP-SENHAS-IXTM      PIC  X(037)        VALUE SPACES.
            10 WRK-INP-COMANDO-IXTM     PIC  X(068)        VALUE SPACES.
            10 WRK-INP-OCULTOS-IXTM.
              15 WRK-INP-FIMAMOS-IXTM   PIC  X(001)        VALUE SPACES.
              15 WRK-INP-OPCAO-IXTM     PIC  9(001)        VALUE ZEROS.
              15 WRK-INP-TPSELECAO-IXTM PIC  9(001)        VALUE ZEROS.
              15 WRK-INP-TMSTAMP-IXTM   PIC  X(026)        VALUE SPACES.
              15 WRK-INP-PARMENU-IXTM   PIC  9(005)        VALUE ZEROS.
              15 WRK-INP-XAMADOR1-IXTM  PIC  X(008)        VALUE SPACES.
              15 WRK-INP-PAGINA1-IXTM   PIC  9(004)        VALUE ZEROS.
              15 WRK-INP-XAMADOR2-IXTM  PIC  X(008)        VALUE SPACES.
              15 WRK-INP-PAGINA2-IXTM   PIC  9(004)        VALUE ZEROS.
              15 WRK-INP-XAMADOR3-IXTM  PIC  X(008)        VALUE SPACES.
              15 WRK-INP-PAGINA3-IXTM   PIC  9(004)        VALUE ZEROS.
              15 WRK-INP-XAMADOR4-IXTM  PIC  X(008)        VALUE SPACES.
              15 FILLER                 PIC  X(001)        VALUE SPACES.
           10 WRK-INP-NOMECLI-IXTM      PIC  X(040)        VALUE SPACES.
           10 WRK-INP-CNPJCPF-IXTM.
4S2511*       15 WRK-INP-NCNPJCPF-IXTM  PIC  ZZZ.ZZZ.ZZ9   VALUE ZEROS.
4S2511        15 WRK-INP-NCNPJCPF-IXTM  PIC  X(11)         VALUE SPACES.
              15 FILLER                 PIC  X(001)        VALUE SPACES.
4S2511*       15 WRK-INP-FILIAL-IXTM    PIC  9(004)        VALUE ZEROS.
4S2511        15 WRK-INP-FILIAL-IXTM    PIC  X(04)         VALUE SPACES.
              15 FILLER                 PIC  X(001)        VALUE SPACES.
              15 WRK-INP-CONTROLE-IXTM  PIC  9(002)        VALUE ZEROS.
           10 WRK-INP-DADOSBC-IXTM.
              15 WRK-INP-BANCO-IXTM     PIC  9(003)        VALUE ZEROS.
              15 WRK-INP-BARRA1-IXTM    PIC  X(001)        VALUE SPACES.
              15 WRK-INP-AGENCIA-IXTM   PIC  9(005)        VALUE ZEROS.
              15 WRK-INP-BARRA2-IXTM    PIC  X(001)        VALUE SPACES.
              15 WRK-INP-CTA-IXTM       PIC  9(013)        VALUE ZEROS.
           10 WRK-INP-CODPROD-IXTM      PIC  9(003)        VALUE ZEROS.
           10 WRK-INP-DESPROD-IXTM      PIC  X(012)        VALUE SPACES.
           10 WRK-INP-CODSUBP-IXTM      PIC  9(003)        VALUE ZEROS.
           10 WRK-INP-DESSUBP-IXTM      PIC  X(012)        VALUE SPACES.
           10 WRK-INP-OPERACAO-IXTM     PIC  9(013)        VALUE ZEROS.
           10 FILLER REDEFINES          WRK-INP-OPERACAO-IXTM.
              15 WRK-INP-DANO-IXTM      PIC  9(004).
              15 WRK-INP-NSEQ-IXTM      PIC  9(009).
           10 WRK-INP-SITU1-IXTM        PIC  X(015)        VALUE SPACES.
           10 WRK-INP-SITU2-IXTM        PIC  X(013)        VALUE SPACES.
           10 WRK-INP-SITU3-IXTM        PIC  X(009)        VALUE SPACES.
           10 WRK-INP-AGENOCU-IXTM      PIC  9(005)        VALUE ZEROS.
           10 WRK-INP-OCULTAR-IXTM.
              15 WRK-INP-OCULT1-IXTM    PIC  X(079)        VALUE SPACES.
              15 WRK-INP-OCULT2-IXTM    PIC  X(079)        VALUE SPACES.
              15 WRK-INP-OCULT3-IXTM    PIC  X(079)        VALUE SPACES.
              15 WRK-INP-OCULT4-IXTM    PIC  X(079)        VALUE SPACES.
           10 WRK-INP-FLIQDC-IXTM       PIC  X(001)        VALUE SPACES.
           10 WRK-INP-DLIQDC-IXTM       PIC  X(010)        VALUE SPACES.
           10 WRK-INP-TXTLIQ-IXTM       PIC  X(079)        VALUE SPACES.
           10 WRK-INP-UTILIM-IXTM       PIC  X(001)        VALUE SPACES.
           10 WRK-INP-DLIMIT-IXTM       PIC  X(003)        VALUE SPACES.
           10 WRK-INP-TXTLIM-IXTM       PIC  X(079)        VALUE SPACES.
           10 WRK-INP-CDEBIT-IXTM       PIC  X(001)        VALUE SPACES.
           10 WRK-INP-DDEBIT-IXTM       PIC  X(003)        VALUE SPACES.
           10 WRK-INP-TXTDEB-IXTM       PIC  X(079)        VALUE SPACES.
           10 WRK-INP-CTEIMO-IXTM       PIC  X(001)        VALUE SPACES.
           10 WRK-INP-DTEIMO-IXTM       PIC  X(003)        VALUE SPACES.
           10 WRK-INP-TXTTEI-IXTM       PIC  X(079)        VALUE SPACES.
           10 WRK-INP-MENSA-IXTM        PIC  X(079)        VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                       PIC  X(050)         VALUE
           '*** AREA DA TELA DCOMIXTM  - OUTPUT ***'.
      *----------------------------------------------------------------*

       01  WRK-OUTPUT-IXTM.
           05 WRK-OUT-LL-IXTM           PIC  9(004) COMP   VALUE ZEROS.
           05 WRK-OUT-ZZ-IXTM           PIC  9(004) COMP   VALUE ZEROS.
           05 WRK-OUT-DADOS-IXTM.
            10 WRK-OUT-SENHAS-IXTM      PIC  X(037)        VALUE SPACES.
            10 WRK-OUT-COMANDO-IXTM     PIC  X(068)        VALUE SPACES.
            10 WRK-OUT-OCULTOS-IXTM.
              15 WRK-OUT-FIMAMOS-IXTM   PIC  X(001)        VALUE SPACES.
              15 WRK-OUT-OPCAO-IXTM     PIC  9(001)        VALUE ZEROS.
              15 WRK-OUT-TPSELECAO-IXTM PIC  9(001)        VALUE ZEROS.
              15 WRK-OUT-TMSTAMP-IXTM   PIC  X(026)        VALUE SPACES.
              15 WRK-OUT-PARMENU-IXTM   PIC  9(005)        VALUE ZEROS.
              15 WRK-OUT-XAMADOR1-IXTM  PIC  X(008)        VALUE SPACES.
              15 WRK-OUT-PAGINA1-IXTM   PIC  9(004)        VALUE ZEROS.
              15 WRK-OUT-XAMADOR2-IXTM  PIC  X(008)        VALUE SPACES.
              15 WRK-OUT-PAGINA2-IXTM   PIC  9(004)        VALUE ZEROS.
              15 WRK-OUT-XAMADOR3-IXTM  PIC  X(008)        VALUE SPACES.
              15 WRK-OUT-PAGINA3-IXTM   PIC  9(004)        VALUE ZEROS.
              15 WRK-OUT-XAMADOR4-IXTM  PIC  X(008)        VALUE SPACES.
              15 FILLER                 PIC  X(001)        VALUE SPACES.
           10 WRK-OUT-NOMECLI-IXTM      PIC  X(040)        VALUE SPACES.
           10 WRK-OUT-CNPJCPF-IXTM.
4S2511*       15 WRK-OUT-NCNPJCPF-IXTM  PIC  ZZZ.ZZZ.ZZ9   VALUE ZEROS.
4S2511        15 WRK-OUT-NCNPJCPF-IXTM  PIC  X(11)         VALUE SPACES.
              15 FILLER                 PIC  X(001)        VALUE SPACES.
4S2511*       15 WRK-OUT-FILIAL-IXTM    PIC  9(004)        VALUE ZEROS.
4S2511        15 WRK-OUT-FILIAL-IXTM    PIC  X(04)         VALUE SPACES.
              15 FILLER                 PIC  X(001)        VALUE SPACES.
              15 WRK-OUT-CONTROLE-IXTM  PIC  9(002)        VALUE ZEROS.
           10 WRK-OUT-DADOSBC-IXTM.
              15 WRK-OUT-BANCO-IXTM     PIC  9(003)        VALUE ZEROS.
              15 WRK-OUT-BARRA1-IXTM    PIC  X(001)        VALUE SPACES.
              15 WRK-OUT-AGENCIA-IXTM   PIC  9(005)        VALUE ZEROS.
              15 WRK-OUT-BARRA2-IXTM    PIC  X(001)        VALUE SPACES.
              15 WRK-OUT-CTA-IXTM       PIC  9(013)        VALUE ZEROS.
           10 WRK-OUT-CODPROD-IXTM      PIC  9(003)        VALUE ZEROS.
           10 WRK-OUT-DESPROD-IXTM      PIC  X(012)        VALUE SPACES.
           10 WRK-OUT-CODSUBP-IXTM      PIC  9(003)        VALUE ZEROS.
           10 WRK-OUT-DESSUBP-IXTM      PIC  X(012)        VALUE SPACES.
           10 WRK-OUT-OPERACAO-IXTM     PIC  9(013)        VALUE ZEROS.
           10 FILLER REDEFINES          WRK-OUT-OPERACAO-IXTM.
              15 WRK-OUT-DANO-IXTM      PIC  9(004).
              15 WRK-OUT-NSEQ-IXTM      PIC  9(009).
           10 WRK-OUT-SITU1-IXTM        PIC  X(015)        VALUE SPACES.
           10 WRK-OUT-SITU2-IXTM        PIC  X(013)        VALUE SPACES.
           10 WRK-OUT-SITU3-IXTM-X.
             15 WRK-OUT-SITU3-IXTM      PIC  9(009)        VALUE ZEROS.
           10 WRK-OUT-AGENOCU-IXTM      PIC  9(005)        VALUE ZEROS.
           10 WRK-OUT-OCULTAR-IXTM.
              15 WRK-OUT-OCULT1-IXTM    PIC  X(079)        VALUE SPACES.
              15 WRK-OUT-OCULT2-IXTM    PIC  X(079)        VALUE SPACES.
              15 WRK-OUT-OCULT3-IXTM    PIC  X(079)        VALUE SPACES.
              15 WRK-OUT-OCULT4-IXTM    PIC  X(079)        VALUE SPACES.
           10 WRK-OUT-FLIQDC-IXTM       PIC  X(001)        VALUE SPACES.
           10 WRK-OUT-DLIQDC-IXTM       PIC  X(010)        VALUE SPACES.
           10 WRK-OUT-TXTLIQ-IXTM       PIC  X(079)        VALUE SPACES.
           10 WRK-OUT-UTILIM-IXTM       PIC  X(001)        VALUE SPACES.
           10 WRK-OUT-DLIMIT-IXTM       PIC  X(003)        VALUE SPACES.
           10 WRK-OUT-TXTLIM-IXTM       PIC  X(079)        VALUE SPACES.
           10 WRK-OUT-CDEBIT-IXTM       PIC  X(001)        VALUE SPACES.
           10 WRK-OUT-DDEBIT-IXTM       PIC  X(003)        VALUE SPACES.
           10 WRK-OUT-TXTDEB-IXTM       PIC  X(079)        VALUE SPACES.
           10 WRK-OUT-CTEIMO-IXTM       PIC  X(001)        VALUE SPACES.
           10 WRK-OUT-DTEIMO-IXTM       PIC  X(003)        VALUE SPACES.
           10 WRK-OUT-TXTTEI-IXTM       PIC  X(079)        VALUE SPACES.
           10 WRK-OUT-MENSA-IXTM        PIC  X(079)        VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA TELA DCOMIXTM  - BRAD0660 ***'.
      *----------------------------------------------------------------

       01  WRK-660-DCOMIXTM.
           05  WRK-660-LL-IXTM          PIC  9(004) COMP   VALUE 0000.
           05  WRK-660-ZZ-IXTM          PIC  9(004) COMP   VALUE 0000.
           05  WRK-660-SENHAS-IXTM      PIC  9(004) COMP   VALUE 0037.
           05  WRK-660-COMANDO-IXTM     PIC  9(004) COMP   VALUE 0068.
           05  WRK-660-OCULTOS-IXTM     PIC  9(004) COMP   VALUE 0079.
           05  WRK-660-NOMECLI-IXTM     PIC  9(004) COMP   VALUE 0040.
           05  WRK-660-CNPJCPF-IXTM     PIC  9(004) COMP   VALUE 0019.
           05  WRK-660-DADOSBC-IXTM     PIC  9(004) COMP   VALUE 0023.
           05  WRK-660-CODPROD-IXTM     PIC  9(004) COMP   VALUE 0003.
           05  WRK-660-DESPROD-IXTM     PIC  9(004) COMP   VALUE 0012.
           05  WRK-660-CODSUBP-IXTM     PIC  9(004) COMP   VALUE 0003.
           05  WRK-660-DESSUBP-IXTM     PIC  9(004) COMP   VALUE 0012.
           05  WRK-660-OPERACAO-IXTM    PIC  9(004) COMP   VALUE 0013.
           05  WRK-660-SITU1-IXTM       PIC  9(004) COMP   VALUE 0015.
           05  WRK-660-SITU2-IXTM       PIC  9(004) COMP   VALUE 0013.
           05  WRK-660-SITU3-IXTM       PIC  9(004) COMP   VALUE 0009.
           05  WRK-660-AGENOCU-IXTM     PIC  9(004) COMP   VALUE 0005.
           05  WRK-660-OCULT1-IXTM      PIC  9(004) COMP   VALUE 0079.
           05  WRK-660-OCULT2-IXTM      PIC  9(004) COMP   VALUE 0079.
           05  WRK-660-OCULT3-IXTM      PIC  9(004) COMP   VALUE 0079.
           05  WRK-660-OCULT4-IXTM      PIC  9(004) COMP   VALUE 0079.
           05  WRK-660-FLIQDC-IXTM      PIC  9(004) COMP   VALUE 0001.
           05  WRK-660-DLIQDC-IXTM      PIC  9(004) COMP   VALUE 0010.
           05  WRK-660-TXTLIQ-IXTM      PIC  9(004) COMP   VALUE 0079.
           05  WRK-660-UTILIM-IXTM      PIC  9(004) COMP   VALUE 0001.
           05  WRK-660-DLIMIT-IXTM      PIC  9(004) COMP   VALUE 0003.
           05  WRK-660-TXTLIM-IXTM      PIC  9(004) COMP   VALUE 0079.
           05  WRK-660-CDEBIT-IXTM      PIC  9(004) COMP   VALUE 0001.
           05  WRK-660-DDEBIT-IXTM      PIC  9(004) COMP   VALUE 0003.
           05  WRK-660-TXTDEB-IXTM      PIC  9(004) COMP   VALUE 0079.
           05  WRK-660-CTEIMO-IXTM      PIC  9(004) COMP   VALUE 0001.
           05  WRK-660-DTEIMO-IXTM      PIC  9(004) COMP   VALUE 0003.
           05  WRK-660-TXTTEI-IXTM      PIC  9(004) COMP   VALUE 0079.
           05  WRK-660-MENSA-IXTM       PIC  9(004) COMP   VALUE 0079.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO PROGRAMAS CHAMADOS ***'.
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
           05  WRK-COMU-MENSAGEM         PIC  X(079)       VALUE SPACES.
           05  WRK-COMU-DADOS.
               10  WRK-COMU-OPER         PIC  9(013)       VALUE ZEROS.
               10  FILLER REDEFINES WRK-COMU-OPER.
                   15  WRK-COMU-DANO     PIC  9(004).
                   15  WRK-COMU-NSEQ     PIC  9(009).
               10  WRK-COMU-CHAMADOR     PIC  X(008)       VALUE SPACES.
               10  WRK-COMU-DADOS-CHAMADOR.
                   15  WRK-COMU-OCULTO1  PIC  X(079)       VALUE SPACES.
                   15  WRK-COMU-OCULTO2  PIC  X(079)       VALUE SPACES.
                   15  WRK-COMU-OCULTO3  PIC  X(079)       VALUE SPACES.
                   15  WRK-COMU-OCULTO4  PIC  X(079)       VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ACESSO AO MODULO DCOM5120 ***'.
      *----------------------------------------------------------------*

       01  WRK-5120-ENTRADA.
           05 WRK-5120E-COD-RETORNO    PIC  X(004)         VALUE SPACES.
           05 WRK-5120E-MSG-RETORNO    PIC  X(079)         VALUE SPACES.
           05 WRK-5120E-RESTART        PIC  9(005)         VALUE ZEROS.
           05 WRK-5120E-FLAG           PIC  X(001)         VALUE SPACES.
           05 WRK-5120E-CFUNC-BDSCO    PIC  9(009)         VALUE ZEROS.
           05 WRK-5120E-CTERM          PIC  X(008)         VALUE SPACES.
           05 WRK-5120E-DADOS.
              10 WRK-5120E-LOCAL       PIC  X(001)         VALUE SPACES.
              10 WRK-5120E-DANO-OPER-DESC
                                       PIC  9(004)         VALUE ZEROS.
              10 WRK-5120E-NSEQ-OPER-DESC
                                       PIC  9(009)         VALUE ZEROS.
              10 WRK-5120E-HULT-ATULZ  PIC  X(026)         VALUE SPACES.
              10 WRK-5120E-CVAR-PRODT-RURAL
                                       PIC  9(005)         VALUE ZEROS.
              10 WRK-5120E-CNRO-REFT-BACEN
                                       PIC  9(018)         VALUE ZEROS.
              10 WRK-5120E-CCESTA-SAFRA-RURAL
                                       PIC  9(005)         VALUE ZEROS.
              10 WRK-5120E-CINDCD-ZNMTO
                                       PIC  9(001)         VALUE ZEROS.
              10 WRK-5120E-NBASE-LEGAL-RENEG
                                       PIC  9(005)         VALUE ZEROS.
              10 WRK-5120E-CREG-AMBTL-RURAL
                                       PIC  X(041)         VALUE ZEROS.
              10 WRK-5120E-CORIGE-ANTEC-PGMD
                                       PIC  9(001)         VALUE ZEROS.
              10 WRK-5120E-CCONTR-LIM-DESC
                                       PIC  9(009)         VALUE ZEROS.
              10 WRK-5120E-CVRSAO-CONTR-LIM
                                       PIC  9(003)         VALUE ZEROS.
              10 WRK-5120E-CINDCD-FORMA-LIQDC
                                       PIC  X(001)         VALUE SPACES.
              10 WRK-5120E-CINDCD-UTILZ-LIM
                                       PIC  X(001)         VALUE SPACES.
              10 WRK-5120E-CINDCD-DEB-PCIAL
                                       PIC  X(001)         VALUE SPACES.
              10 WRK-5120E-CINDCD-TENTV-DEB
                                       PIC  X(001)         VALUE SPACES.
              10 FILLER                PIC  X(731)         VALUE SPACES.

       01  WRK-5120-SAIDA.
           05 WRK-5120S-COD-RETORNO    PIC  9(004)         VALUE ZEROS.
           05 WRK-5120S-MSG-RETORNO    PIC  X(079)         VALUE SPACES.
           05 WRK-5120S-RESTART        PIC  9(005)         VALUE ZEROS.
           05 WRK-5120S-FLAG           PIC  X(001)         VALUE SPACES.
           05 WRK-5120S-DADOS.
              10 WRK-5120S-LOCAL       PIC  X(001)         VALUE SPACES.
              10 WRK-5120S-DANO-OPER-DESC
                                       PIC  9(004)         VALUE ZEROS.
              10 WRK-5120S-NSEQ-OPER-DESC
                                       PIC  9(009)         VALUE ZEROS.
              10 WRK-5120S-HULT-ATULZ  PIC  X(026)         VALUE SPACES.
              10 WRK-5120S-CVAR-PRODT-RURAL
                                       PIC  9(005)         VALUE ZEROS.
              10 WRK-5120S-CNRO-REFT-BACEN
                                       PIC  9(018)         VALUE ZEROS.
              10 WRK-5120S-CCESTA-SAFRA-RURAL
                                       PIC  9(005)         VALUE ZEROS.
              10 WRK-5120S-CINDCD-ZNMTO
                                       PIC  9(001)         VALUE ZEROS.
              10 WRK-5120S-NBASE-LEGAL-RENEG
                                       PIC  9(005)         VALUE ZEROS.
              10 WRK-5120S-CREG-AMBTL-RURAL
                                       PIC  X(041)         VALUE ZEROS.
              10 WRK-5120S-CORIGE-ANTEC-PGMD
                                       PIC  9(001)         VALUE ZEROS.
              10 WRK-5120S-CCONTR-LIM-DESC
                                       PIC  9(009)         VALUE ZEROS.
              10 WRK-5120S-CVRSAO-CONTR-LIM
                                       PIC  9(003)         VALUE ZEROS.
              10 WRK-5120S-CINDCD-FORMA-LIQDC
                                       PIC  X(001)         VALUE SPACES.
              10 WRK-5120S-CINDCD-UTILZ-LIM
                                       PIC  X(001)         VALUE SPACES.
              10 WRK-5120S-CINDCD-DEB-PCIAL
                                       PIC  X(001)         VALUE SPACES.
              10 WRK-5120S-CINDCD-TENTV-DEB
                                       PIC  X(001)         VALUE SPACES.
              10 FILLER                PIC  X(731)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO PROGRAMA DCOM5537 ***'.
      *----------------------------------------------------------------*

       COPY 'I#DCOMJ4'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA BRAD7100 ***'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** DCOM3935 - FIM DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01 LNK-IO-PGB.
           05 LNK-IO-LTERM             PIC  X(008).
           05 FILLER                   PIC  X(002).
           05 LNK-IO-STATUS            PIC  X(002).
           05 FILLER                   PIC  X(012).
           05 LNK-IO-MODNAME           PIC  X(008).

       01 LNK-ALT-PGB.
           05 LNK-ALT-LTERM            PIC  X(008).
           05 FILLER                   PIC  X(002).
           05 LNK-ALT-STATUS           PIC  X(002).
           05 FILLER                   PIC  X(012).
           05 LNK-ALT-MODNAME          PIC  X(008).

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

           ENTRY 'DLITCBL'             USING LNK-IO-PGB
                                             LNK-ALT-PGB.

           MOVE WRK-GU                 TO WRK-FUNCAO.

      *----------------------------------------------------------------*
      *  ESSA ROTINA CONTEM OS PROCEDIMENTOS INICIAIS                  *
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL5005'             USING WRK-VERSAO
                                             WRK-FUNCAO
                                             LNK-IO-PGB
                                             LNK-ALT-PGB
                                             WRK-MENSAGEM
                                             WRK-TELA
                                             WRK-COD-USER
                                             WRK-COD-DEPTO.

           IF  RETURN-CODE             EQUAL 04 OR 12
               PERFORM 2000-FINALIZAR
           END-IF.

           MOVE SPACES                 TO WRK-OUT-DADOS-IXTM
                                          WRK-INCONSISTENCIA.

           INITIALIZE WRK-AUXILIARES-GERAIS.

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

           MOVE 'DCOMIXTM'             TO WRK-TELA.

           IF  WRK-TRANSACAO           NOT EQUAL 'DCOM3935'
               PERFORM 1100-RECEBER-CONTROLE
           ELSE
               PERFORM 1200-PROCESSAR-DCOMIXTM
           END-IF.

           IF  WRK-TELA                EQUAL 'DCOMIXTM'
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

           MOVE WRK-MENSAGEM           TO WRK-COMU-AREA.

           MOVE WRK-COMU-DADOS-CHAMADOR
                                       TO WRK-INP-OCULTAR-IXTM
                                          WRK-OUT-OCULTAR-IXTM.
           MOVE WRK-COMU-CHAMADOR      TO WRK-INP-XAMADOR1-IXTM
                                          WRK-OUT-XAMADOR1-IXTM.

           PERFORM 1110-OBTER-CABECALHO.

           IF  WRK-INCONSISTENCIA      EQUAL SPACES
               PERFORM 1130-OBTER-DADOS-OPTIN
           END-IF.

           IF  WRK-INCONSISTENCIA      NOT EQUAL SPACES
               PERFORM 1150-RETORNAR-CHAMADOR
               EVALUATE WRK-INCONSISTENCIA
                 WHEN '@'
                   MOVE 5537-SAI-MSG-RETORNO
                                       TO WRK-COMU-MENSAGEM
                 WHEN '*'
                   MOVE WRK-5120S-MSG-RETORNO
                                       TO WRK-COMU-MENSAGEM
                 WHEN '/'
                   MOVE 'OPERACAO NAO POSSUI OPTIN'
                                       TO WRK-COMU-MENSAGEM
               END-EVALUATE
               MOVE WRK-COMU-AREA      TO WRK-MENSAGEM
           ELSE
               MOVE WRK-MSG001         TO WRK-OUT-MENSA-IXTM
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  OBTER DADOS PARA O CABECALHO ATRAVES DE ACESSO AO DCOM5537    *
      *----------------------------------------------------------------*
       1110-OBTER-CABECALHO            SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO WRK-SQLCA
                                          ERRO-AREA.

           INITIALIZE 5537-ENTRADA-ROTEADOR
                      5537-SAIDA-ROTEADOR
                      ERRO-AREA.

           MOVE WRK-COD-USER-R         TO 5537-ENT-CFUNC-BDSCO.
           MOVE LNK-IO-LTERM           TO 5537-ENT-CTERM.
           MOVE WRK-COMU-DANO          TO 5537-ENT-DANO-OPER-DESC.
           MOVE WRK-COMU-NSEQ          TO 5537-ENT-NSEQ-OPER-DESC.
           MOVE 'R'                    TO 5537-ENT-FLAG.
           MOVE 'DCOM5537'             TO WRK-MODULO

           CALL WRK-MODULO             USING 5537-ENTRADA-ROTEADOR
                                             5537-SAIDA-ROTEADOR
                                             ERRO-AREA
                                             WRK-SQLCA.

           EVALUATE 5537-SAI-COD-RETORNO
               WHEN '0000'
                     PERFORM 1120-FORMATAR-CABECALHO

               WHEN '0099'
                     PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                    MOVE '@'            TO WRK-INCONSISTENCIA
           END-EVALUATE.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  FORMATACAO DE CABECALHO COM DADOS RETORNADOS DO DCOM5537      *
      *----------------------------------------------------------------*
       1120-FORMATAR-CABECALHO         SECTION.
      *----------------------------------------------------------------*

           MOVE 5537-SAI-IPSSOA-DESC-COML  TO WRK-OUT-NOMECLI-IXTM.

4S2511     IF 5537-SAI-CCNPJ-CPF    EQUAL SPACES OR
4S2511        5537-SAI-CCNPJ-CPF    EQUAL LOW-VALUES OR
4S2511        5537-SAI-CFLIAL-CNPJ  EQUAL SPACES OR
4S2511        5537-SAI-CFLIAL-CNPJ  EQUAL LOW-VALUES
4S2511         MOVE SPACES          TO WRK-OUT-CNPJCPF-IXTM
4S2511     ELSE
4S2511         STRING 5537-SAI-CCNPJ-CPF(1:3) '.'
4S2511                5537-SAI-CCNPJ-CPF(4:3) '.'
4S2511                5537-SAI-CCNPJ-CPF(7:3) '/'
4S2511                5537-SAI-CFLIAL-CNPJ    '-'
4S2511                5537-SAI-CCTRL-CNPJ-CPF
4S2511         DELIMITED BY SIZE               INTO WRK-OUT-CNPJCPF-IXTM
4S2511     END-IF

           STRING 5537-SAI-CBCO      '/'
                  5537-SAI-CAG-BCRIA '/'
                  5537-SAI-CCTA-BCRIA-CLI
           DELIMITED BY SIZE               INTO WRK-OUT-DADOSBC-IXTM.

           MOVE 5537-SAI-CPRODT            TO WRK-OUT-CODPROD-IXTM.
           MOVE 5537-SAI-IABREV-PRODT      TO WRK-OUT-DESPROD-IXTM.
           MOVE 5537-SAI-CSPROD-DESC-COML  TO WRK-OUT-CODSUBP-IXTM.
           MOVE 5537-SAI-IRSUMO-SPROD-DESC TO WRK-OUT-DESSUBP-IXTM.
           MOVE WRK-COMU-DANO              TO WRK-OUT-DANO-IXTM.
           MOVE WRK-COMU-NSEQ              TO WRK-OUT-NSEQ-IXTM.
           MOVE 5537-SAI-IRSUMO-SIT-DESC   TO WRK-OUT-SITU1-IXTM.

           EVALUATE TRUE
               WHEN 5537-SAI-CCONTR-LIM-DESC      GREATER ZEROS
                    MOVE 'CONTR LIMITE:'          TO WRK-OUT-SITU2-IXTM
                    MOVE 5537-SAI-CCONTR-LIM-DESC TO WRK-OUT-SITU3-IXTM

               WHEN 5537-SAI-CCONTR-CONVE-DESC    GREATER ZEROS AND
                    5537-SAI-CELMTO-DESC-COML     EQUAL 8
                    MOVE 'CONV GERAL..:'          TO WRK-OUT-SITU2-IXTM
                    MOVE 5537-SAI-CCONTR-CONVE-DESC
                                                  TO WRK-OUT-SITU3-IXTM

               WHEN 5537-SAI-CCONTR-CONVE-DESC    GREATER ZEROS AND
                    5537-SAI-CELMTO-DESC-COML     EQUAL 9
                    MOVE 'CONV CLIENTE:'          TO WRK-OUT-SITU2-IXTM
                    MOVE 5537-SAI-CCONTR-CONVE-DESC
                                                  TO WRK-OUT-SITU3-IXTM
           END-EVALUATE.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * OBTEM DADOS DO OPTIN                                           *
      *----------------------------------------------------------------*
       1130-OBTER-DADOS-OPTIN          SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-5120-ENTRADA
                      WRK-5120-SAIDA
                      ERRO-AREA
                      WRK-SQLCA.

           MOVE WRK-COD-USER-R         TO WRK-5120E-CFUNC-BDSCO.
           MOVE LNK-IO-LTERM           TO WRK-5120E-CTERM.
           MOVE SPACES                 TO WRK-5120E-LOCAL.
           MOVE WRK-COMU-DANO          TO WRK-5120E-DANO-OPER-DESC.
           MOVE WRK-COMU-NSEQ          TO WRK-5120E-NSEQ-OPER-DESC.
           MOVE 'DCOM5120'             TO WRK-MODULO.

           CALL WRK-MODULO             USING WRK-5120-ENTRADA
                                             WRK-5120-SAIDA
                                             ERRO-AREA
                                             WRK-SQLCA.

           EVALUATE WRK-5120S-COD-RETORNO
               WHEN 0000
               WHEN 0003
                    PERFORM 1131-FORMATAR-TELA

               WHEN OTHER
                    MOVE '*'           TO WRK-INCONSISTENCIA
           END-EVALUATE.

      *----------------------------------------------------------------*
       1130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * OBTEM DADOS DO OPTIN                                           *
      *----------------------------------------------------------------*
       1131-FORMATAR-TELA              SECTION.
      *----------------------------------------------------------------*

           IF  WRK-5120S-COD-RETORNO   EQUAL 0003   OR
              (WRK-5120S-CINDCD-FORMA-LIQDC
                                       NOT EQUAL 'B' AND 'D')
               MOVE '/'                TO WRK-INCONSISTENCIA
               GO TO 1131-99-FIM
           END-IF.

           MOVE WRK-5120S-CINDCD-FORMA-LIQDC
                                       TO WRK-OUT-FLIQDC-IXTM.

           IF  WRK-OUT-FLIQDC-IXTM     EQUAL 'D'
               MOVE 'DEBITO CTA'       TO WRK-OUT-DLIQDC-IXTM
           ELSE
               MOVE 'BOLETO'           TO WRK-OUT-DLIQDC-IXTM
           END-IF.

           MOVE SPACES                 TO WRK-OUT-TXTLIQ-IXTM.

           MOVE WRK-5120S-CINDCD-UTILZ-LIM
                                       TO WRK-OUT-UTILIM-IXTM.

           IF  WRK-OUT-UTILIM-IXTM     EQUAL 'S'
               MOVE 'SIM'              TO WRK-OUT-DLIMIT-IXTM
           ELSE
               MOVE 'NAO'              TO WRK-OUT-DLIMIT-IXTM
           END-IF.

           MOVE SPACES                 TO WRK-OUT-TXTLIM-IXTM.

           MOVE WRK-5120S-CINDCD-DEB-PCIAL
                                       TO WRK-OUT-CDEBIT-IXTM.

           IF  WRK-OUT-CDEBIT-IXTM     EQUAL 'S'
               MOVE 'SIM'              TO WRK-OUT-DDEBIT-IXTM
           ELSE
               MOVE 'NAO'              TO WRK-OUT-DDEBIT-IXTM
           END-IF.

           MOVE SPACES                 TO WRK-OUT-TXTDEB-IXTM.

           MOVE WRK-5120S-CINDCD-TENTV-DEB
                                       TO WRK-OUT-CTEIMO-IXTM.

           IF  WRK-OUT-CDEBIT-IXTM     EQUAL 'S'
               MOVE 'SIM'              TO WRK-OUT-DTEIMO-IXTM
           ELSE
               MOVE 'NAO'              TO WRK-OUT-DTEIMO-IXTM
           END-IF.

           MOVE SPACES                 TO WRK-OUT-TXTTEI-IXTM.

      *----------------------------------------------------------------*
       1131-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * DEVOLVE CONTROLE AO PROGRAMA CHAMADOR                          *
      *----------------------------------------------------------------*
       1150-RETORNAR-CHAMADOR          SECTION.
      *----------------------------------------------------------------*

           PERFORM 1155-FORMATAR-COMU-AREA.

           MOVE 'DCOM3924'             TO WRK-TELA.

           MOVE WRK-CHNG               TO WRK-FUNCAO.
           MOVE WRK-COMU-AREA          TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1150-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  FORMATACAO DA AREA DE COMUNICACAO                             *
      *----------------------------------------------------------------*
       1155-FORMATAR-COMU-AREA         SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-COMU-AREA.

           MOVE LENGTH                 OF WRK-COMU-AREA
                                       TO WRK-COMU-LL.
           MOVE 'DCOM3935'             TO WRK-COMU-TRANSACAO.
           MOVE WRK-INP-DANO-IXTM      TO WRK-COMU-DANO.
           MOVE WRK-INP-NSEQ-IXTM      TO WRK-COMU-NSEQ.
           MOVE WRK-INP-XAMADOR1-IXTM  TO WRK-COMU-CHAMADOR.
           MOVE WRK-INP-OCULTAR-IXTM   TO WRK-COMU-DADOS-CHAMADOR.

      *----------------------------------------------------------------*
       1155-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA PROCESSA A MENSAGEM VINDA DA TELA DCOMIXTM        *
      *----------------------------------------------------------------*
       1200-PROCESSAR-DCOMIXTM         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM           TO WRK-INPUT-IXTM.

           EVALUATE TRUE
               WHEN WRK-INP-PFK-IXTM     EQUAL 'H' OR
                   (WRK-INP-PFK-IXTM     EQUAL '.' AND
                    WRK-INP-COMANDO-IXTM EQUAL 'PFK01')
                    PERFORM 1210-DEVOLVER-TELA
                    MOVE WRK-INP-MENSA-IXTM  TO WRK-OUT-MENSA-IXTM

               WHEN WRK-INP-PFK-IXTM     EQUAL '3' OR
                   (WRK-INP-PFK-IXTM     EQUAL '.' AND
                    WRK-INP-COMANDO-IXTM EQUAL 'PFK03')
                    PERFORM 1150-RETORNAR-CHAMADOR

               WHEN WRK-INP-PFK-IXTM     EQUAL '5' OR
                   (WRK-INP-PFK-IXTM     EQUAL '.' AND
                    WRK-INP-COMANDO-IXTM EQUAL 'PFK05')
                    PERFORM 1220-RETORNAR-MENU-ROTINAS

               WHEN WRK-INP-PFK-IXTM     EQUAL 'A' OR
                   (WRK-INP-PFK-IXTM     EQUAL '.' AND
                    WRK-INP-COMANDO-IXTM EQUAL 'PFK10')
                    PERFORM 1225-RETORNAR-MENU-DCOM

               WHEN OTHER
                    PERFORM 1210-DEVOLVER-TELA
                    MOVE WRK-MSG002      TO WRK-OUT-MENSA-IXTM
           END-EVALUATE.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA DEVOLVE OS DADOS DA TELA SEM ALTERACAO            *
      *----------------------------------------------------------------*
       1210-DEVOLVER-TELA              SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-OCULTOS-IXTM     TO WRK-OUT-OCULTOS-IXTM.
           MOVE WRK-INP-NOMECLI-IXTM     TO WRK-OUT-NOMECLI-IXTM.
           MOVE WRK-INP-CNPJCPF-IXTM     TO WRK-OUT-CNPJCPF-IXTM.
           MOVE WRK-INP-DADOSBC-IXTM     TO WRK-OUT-DADOSBC-IXTM.
           MOVE WRK-INP-CODPROD-IXTM     TO WRK-OUT-CODPROD-IXTM.
           MOVE WRK-INP-DESPROD-IXTM     TO WRK-OUT-DESPROD-IXTM.
           MOVE WRK-INP-CODSUBP-IXTM     TO WRK-OUT-CODSUBP-IXTM.
           MOVE WRK-INP-DESSUBP-IXTM     TO WRK-OUT-DESSUBP-IXTM.
           MOVE WRK-INP-OPERACAO-IXTM    TO WRK-OUT-OPERACAO-IXTM.
           MOVE WRK-INP-SITU1-IXTM       TO WRK-OUT-SITU1-IXTM.
           MOVE WRK-INP-SITU2-IXTM       TO WRK-OUT-SITU2-IXTM.
           MOVE WRK-INP-SITU3-IXTM       TO WRK-OUT-SITU3-IXTM-X.
           MOVE WRK-INP-AGENOCU-IXTM     TO WRK-OUT-AGENOCU-IXTM.
           MOVE WRK-INP-OCULTAR-IXTM     TO WRK-OUT-OCULTAR-IXTM.
           MOVE WRK-INP-FLIQDC-IXTM      TO WRK-OUT-FLIQDC-IXTM.
           MOVE WRK-INP-DLIQDC-IXTM      TO WRK-OUT-DLIQDC-IXTM.
           MOVE WRK-INP-TXTLIQ-IXTM      TO WRK-OUT-TXTLIQ-IXTM.
           MOVE WRK-INP-UTILIM-IXTM      TO WRK-OUT-UTILIM-IXTM.
           MOVE WRK-INP-DLIMIT-IXTM      TO WRK-OUT-DLIMIT-IXTM.
           MOVE WRK-INP-TXTLIM-IXTM      TO WRK-OUT-TXTLIM-IXTM.
           MOVE WRK-INP-CDEBIT-IXTM      TO WRK-OUT-CDEBIT-IXTM.
           MOVE WRK-INP-DDEBIT-IXTM      TO WRK-OUT-DDEBIT-IXTM.
           MOVE WRK-INP-TXTDEB-IXTM      TO WRK-OUT-TXTDEB-IXTM.
           MOVE WRK-INP-CTEIMO-IXTM      TO WRK-OUT-CTEIMO-IXTM.
           MOVE WRK-INP-DTEIMO-IXTM      TO WRK-OUT-DTEIMO-IXTM.
           MOVE WRK-INP-TXTTEI-IXTM      TO WRK-OUT-TXTTEI-IXTM.
           MOVE WRK-INP-MENSA-IXTM       TO WRK-OUT-MENSA-IXTM.

      *----------------------------------------------------------------*
       1210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRATAMENTO DE RETORNO AO MENU ROTINAS    *
      *----------------------------------------------------------------*
       1220-RETORNAR-MENU-ROTINAS      SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-COMU-AREA.

           MOVE LENGTH                 OF WRK-COMU-AREA
                                       TO WRK-COMU-LL.
           MOVE WRK-CHNG               TO WRK-FUNCAO.
           MOVE 'DCOM3935'             TO WRK-TELA
                                          WRK-COMU-TRANSACAO.
           MOVE '5'                    TO WRK-COMU-PFK.
           MOVE WRK-COMU-AREA          TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1225-RETORNAR-MENU-DCOM         SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-COMU-AREA.

           MOVE LENGTH                 OF WRK-COMU-AREA
                                       TO WRK-COMU-LL.
           MOVE WRK-CHNG               TO WRK-FUNCAO.
           MOVE 'DCOM3935'             TO WRK-TELA
                                          WRK-COMU-TRANSACAO.
           MOVE 'A'                    TO WRK-COMU-PFK.
           MOVE WRK-COMU-AREA          TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1225-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA COMPACTA A AREA DE MENSAGENS DO IMS               *
      *----------------------------------------------------------------*
       1300-PROCESSAR-BRAD0660         SECTION.
      *----------------------------------------------------------------*

           MOVE LENGTH                 OF WRK-OUTPUT-IXTM
                                       TO WRK-OUT-LL-IXTM
                                          WRK-660-ZZ-IXTM.
           MOVE LENGTH                 OF WRK-660-DCOMIXTM
                                       TO WRK-660-LL-IXTM.

           CALL 'BRAD0660'             USING WRK-OUTPUT-IXTM
                                             WRK-660-DCOMIXTM.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE RETURN-CODE        TO WRK-RETURN-CODE
               MOVE 10                 TO WRK-LOCAL-ERRO
               MOVE WRK-ERRO-BRAD0660  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-ISRT               TO WRK-FUNCAO.
           MOVE WRK-OUTPUT-IXTM        TO WRK-MENSAGEM.

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

           IF  ERR-MODULO              EQUAL SPACES AND LOW-VALUES
               MOVE 'SENHAS02'         TO ERR-PGM
               MOVE 'DCOM3935'         TO ERR-MODULO
           ELSE
               MOVE 'DCOM3935'         TO ERR-PGM
           END-IF.

           MOVE WRK-COD-USER           TO ERR-COD-USER.
           MOVE WRK-COD-DEPTO          TO ERR-COD-DEPTO.

           IF  ERR-TIPO-ACESSO         EQUAL 'DB2'
               CALL 'BRAD7100'         USING LNK-IO-PGB
                                             ERRO-AREA
                                             LNK-ALT-PGB
                                             WRK-SQLCA
           ELSE
               CALL 'BRAD7100'         USING LNK-IO-PGB
                                             ERRO-AREA
                                             LNK-ALT-PGB
           END-IF.

           PERFORM 2000-FINALIZAR.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
