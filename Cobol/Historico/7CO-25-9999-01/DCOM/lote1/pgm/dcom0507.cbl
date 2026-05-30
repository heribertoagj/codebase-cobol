      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID.     DCOM0507.
       AUTHOR.         FABRICA - ALTRAN.
      *================================================================*
      *                          A L T R A N                           *
      *----------------------------------------------------------------*
      *     PROGRAMA....: DCOM0507                                     *
      *     PROGRAMADOR.: FABRICA - ALTRAN.                            *
      *     DATA........: 03/08/2007                                   *
      *----------------------------------------------------------------*
      *     OBJETIVO....: CONSULTA ESTORNO DA OPERACAO                 *
      *----------------------------------------------------------------*
      *     TELAS.......:                                              *
      *     DCOMEGTM....: CONSULTA ESTORNO DA OPERACAO                 *
      *----------------------------------------------------------------*
      *     INC'S.......:                                              *
      *     I#BRAD7C - AREA DE ERRO DA BRAD7100.                       *
      *     I#DCOMXJ - AREA DE COMUNICACAO DO MODULO DCOM4174.         *
      *----------------------------------------------------------------*
      *     MODULOS.....:                                              *
      *     BRAD0660 - INSERIR CARACTERES DE NULL NA MENSAGEM DE SAIDA.*
      *     BRAD7100 - TRATAMENTO DE ERROS QUANDO PROGRAMA INVALIDO.   *
      *     DCOM4174 - OBTER DADOS DA OPERACAO.                        *
      *     DCOM5420 - ESTORNA OPERACAO.                            *
      *     POOL5005 - CONTROLE DO TRANSITO DE MENSAGENS NO IMS/DC.    *
      *----------------------------------------------------------------*
      *     NAVEGACAO...:                                              *
      *     CHAMADO POR.: DCOM0502           CHAMA:    --              *
      *                   DCOM0503                                     *
      *                   DCOM0504                                     *
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
           '*** DCOM0507 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** MENSAGENS ***'.
      *----------------------------------------------------------------*

       77  WRK-MSG002                  PIC  X(079)         VALUE
           'PF INVALIDA'.
       77  WRK-MSG003                  PIC  X(079)         VALUE
           'CONSULTA EFETUADA COM SUCESSO. TECLE <PF3> PARA RETORNAR'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** VARIAVEIS AUXILIAREAS ***'.
      *----------------------------------------------------------------*

       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-INCONSIS                PIC  X(001)         VALUE SPACES.
       77  WRK-SQLCA                   PIC  X(136)         VALUE SPACES.
      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** AREA DE ATRIBUTOS ***'.
      *---------------------------------------------------------------*

      *--- ALPHA    - NORMAL    - PROTEGIDO    - NAO POS CURSOR ---*
       77  WRK-225                     PIC S9(08) COMP     VALUE +225.

      *--- ALPHA    - NORMAL    - DESPROTEGIDO - POS. CURSOR    ---*
       77  WRK-49345                   PIC S9(08) COMP     VALUE +49345.

      *--- NUMERICO - BRILHANTE - DESPROTEGIDO - POS. CURSOR    ---*
       77  WRK-49369                   PIC S9(08) COMP     VALUE +49369.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA AUXILARES  **'.
      *----------------------------------------------------------------*

       01  WRK-AUXILIARES.
           05  WRK-JUSTIFIC.
             10  WRK-JUSTIFIC1               PIC X(60) VALUE SPACES.
             10  WRK-JUSTIFIC2               PIC X(60) VALUE SPACES.
           05  WRK-TEXTO                     PIC X(10) VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO COM O MODULO DCOM5355 **'.
      *----------------------------------------------------------------*

       01  WRK-5355E-ENTRADA.
           05 WRK-5355E-COD-RETORNO      PIC  X(004)       VALUE SPACES.
           05 WRK-5355E-MSG-RETORNO      PIC  X(079)       VALUE SPACES.
           05 WRK-5355E-RESTART          PIC  9(005)       VALUE ZEROS.
           05 WRK-5355E-FLAG             PIC  X(001)       VALUE SPACES.
           05 WRK-5355E-CFUNC-BDSCO      PIC  9(009)       VALUE ZEROS.
           05 WRK-5355E-CTERM            PIC  X(008)       VALUE SPACES.
           05 WRK-5355E-TIPO-CONVENIO    PIC  X(001)       VALUE SPACES.
           05 WRK-5355E-CCONVE-GRAL-DESC PIC  9(009)       VALUE ZEROS.
           05 WRK-5355E-CPRODT           PIC  9(003)       VALUE ZEROS.
           05 WRK-5355E-CSPROD-DESC-COML PIC  9(003)       VALUE ZEROS.
FS2511*    05 WRK-5355E-CCNPJ-CPF        PIC  9(009)       VALUE ZEROS.
FS2511     05 WRK-5355E-CCNPJ-CPF        PIC  X(09)        VALUE SPACES.
FS2511*    05 WRK-5355E-CFLIAL-CNPJ      PIC  9(005)       VALUE ZEROS.
FS2511     05 WRK-5355E-CFLIAL-CNPJ      PIC  X(004)       VALUE SPACES.
           05 WRK-5355E-CCTRL-CNPJ-CPF   PIC  9(002)       VALUE ZEROS.
           05 WRK-5355E-ROTENT-BANCO     PIC  9(003)       VALUE ZEROS.
           05 WRK-5355E-ROTENT-AGENCIA   PIC  9(005)       VALUE ZEROS.
           05 WRK-5355E-ROTENT-FLAG-FORM PIC  X(001)       VALUE SPACES.

       01  WRK-5355S-SAIDA.
           05 WRK-5355S-COD-RETORNO      PIC  X(004)       VALUE SPACES.
           05 WRK-5355S-MSG-RETORNO      PIC  X(079)       VALUE SPACES.
           05 WRK-5355S-RESTART          PIC  9(005)       VALUE ZEROS.
           05 WRK-5355S-FLAG             PIC  X(001)       VALUE SPACES.
           05 WRK-5355S-DADOS.
             10 WRK-5355S-CCONVE         PIC  9(009)       VALUE ZEROS.
             10 WRK-5355S-CVRSAO         PIC  9(003)       VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO ***'.
      *----------------------------------------------------------------*

       01  WRK-COMU-AREA.
           05  WRK-COMU-LL             PIC S9(004) COMP    VALUE ZEROS.
           05  WRK-COMU-ZZ             PIC S9(004) COMP    VALUE ZEROS.
           05  WRK-COMU-TRANCODE.
               10 WRK-COMU-TRANSACAO   PIC  X(008)         VALUE SPACES.
               10 FILLER               PIC  X(006)         VALUE SPACES.
               10 WRK-COMU-PFK         PIC  X(001)         VALUE SPACES.
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
               10  WRK-COMU-PAG            PIC  9(004)    VALUE ZEROS.
               10  WRK-COMU-OCORR          PIC  9(008)    VALUE ZEROS.
           05  WRK-COMU-MENSA              PIC  X(079)    VALUE SPACES.
           05  WRK-COMU-ACAO               PIC  X(001)    VALUE SPACES.

PROC01*----------------------------------------------------------------*
PROC01 01  FILLER                      PIC  X(050)         VALUE
PROC01     '*** AREA DE COMUNICACAO DCOM3924 ***'.
PROC01*----------------------------------------------------------------*
PROC01
PROC01 01  WRK-COMU-AREA-3924.
PROC01     05  WRK-COMU-LL-3924          PIC S9(004) COMP  VALUE ZEROS.
PROC01     05  WRK-COMU-ZZ-3924          PIC S9(004) COMP  VALUE ZEROS.
PROC01     05  WRK-COMU-TRANCODE-3924.
PROC01         10  WRK-COMU-TRANSACAO-3924
PROC01                                   PIC  X(008)       VALUE SPACES.
PROC01         10  FILLER                PIC  X(006)       VALUE SPACES.
PROC01         10  WRK-COMU-PFK-3924     PIC  X(001)       VALUE SPACES.
PROC01     05  WRK-COMU-SENHAS-3924      PIC  X(037)       VALUE SPACES.
PROC01     05  WRK-COMU-COMANDO-3924     PIC  X(068)       VALUE SPACES.
PROC01     05  WRK-COMU-DADOS-3924.
PROC01         10  WRK-COMU-OPCAO-3924   PIC  X(001)       VALUE SPACES.
PROC01         10  WRK-COMU-TIMESTAMP-3924
PROC01                                   PIC  X(026)       VALUE SPACES.
PROC01         10  WRK-COMU-NOME-3924    PIC  X(040)       VALUE SPACES.
PROC01         10  WRK-COMU-CPFCNPJ-3924.
PROC01             15  WRK-COMU-CNPJCPF-3924
PROC01                                   PIC  9(009)       VALUE ZEROS.
PROC01             15  FILLER            PIC  X(001)       VALUE SPACES.
PROC01             15  WRK-COMU-FILIAL-3924
FS2511*                                  PIC  9(004)       VALUE ZEROS.
FS2511                                   PIC  X(04)        VALUE SPACES.
PROC01             15  FILLER            PIC  X(001)       VALUE SPACES.
PROC01             15  WRK-COMU-CONTROL-3924
PROC01                                   PIC  9(002)       VALUE ZEROS.
PROC01         10  WRK-COMU-BCO-AGE-CTA-3924.
PROC01             15  WRK-COMU-BANCO-3924
PROC01                                   PIC  9(003)       VALUE ZEROS.
PROC01             15  FILLER            PIC  X(001)       VALUE SPACES.
PROC01             15  WRK-COMU-AGENC-3924
PROC01                                   PIC  9(005)       VALUE ZEROS.
PROC01             15  FILLER            PIC  X(001)       VALUE SPACES.
PROC01             15  WRK-COMU-CONTA-3924
PROC01                                   PIC  9(013)       VALUE ZEROS.
PROC01         10  WRK-COMU-PROD-3924    PIC  9(003)       VALUE ZEROS.
PROC01         10  WRK-COMU-DPROD-3924   PIC  X(012)       VALUE SPACES.
PROC01         10  WRK-COMU-SUBPROD-3924 PIC  9(003)       VALUE ZEROS.
PROC01         10  WRK-COMU-DSPROD-3924  PIC  X(012)       VALUE SPACES.
PROC01         10  WRK-COMU-OPER-3924    PIC  9(013)       VALUE ZEROS.
PROC01         10  FILLER REDEFINES WRK-COMU-OPER-3924.
PROC01             15  WRK-COMU-DANO-3924     PIC  9(004).
PROC01             15  WRK-COMU-NSEQ-3924     PIC  9(009).
PROC01         10  WRK-COMU-SIT1-3924    PIC  X(015)       VALUE SPACES.
PROC01         10  WRK-COMU-SIT2-3924    PIC  X(013)       VALUE SPACES.
PROC01         10  WRK-COMU-SIT3-3924    PIC  9(009)       VALUE ZEROS.
PROC01         10  WRK-COMU-SIT4-3924    PIC  X(013)       VALUE SPACES.
PROC01         10  WRK-COMU-SIT5-3924    PIC  9(009)       VALUE ZEROS.
PROC01         10  WRK-COMU-MEIENT-3924  PIC  9(003)       VALUE ZEROS.
PROC01         10  WRK-COMU-TIPDES-3924  PIC  9(003)       VALUE ZEROS.
PROC01         10  WRK-COMU-MENSAGEM-3924
PROC01                                   PIC  X(079)       VALUE SPACES.
PROC01         10  WRK-COMU-CHAMADOR-3924
PROC01                                   PIC  X(008)       VALUE SPACES.
PROC01         10  WRK-COMU-PAG-3924     PIC  9(004)       VALUE ZEROS.
PROC01         10  WRK-COMU-OCORR-3924   PIC  9(008)       VALUE ZEROS.

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
           '*** AREA DA TELA DCOMEGTM - INPUT ***'.
      *----------------------------------------------------------------*

       01  WRK-INPUT-EGTM.
           05 FILLER                   PIC  X(018)         VALUE SPACES.
           05 WRK-INP-PFK-EGTM         PIC  X(001)         VALUE SPACES.
           05 WRK-INP-DADOS-EGTM.
             10 WRK-INP-SENHAS-EGTM    PIC  X(037)         VALUE SPACES.
             10 WRK-INP-COMANDO-EGTM   PIC  X(068)         VALUE SPACES.
             10 WRK-INP-OCULTOS-EGTM.
               15 WRK-INP-TIPOOPC-EGTM PIC  X(001)         VALUE SPACES.
               15 WRK-INP-TPPESQ-EGTM  PIC  9(001)         VALUE ZEROS.
               15 WRK-INP-TMSTAMP-EGTM PIC  X(026)         VALUE SPACES.
               15 WRK-INP-CHAMADOR-EGTM PIC X(008)         VALUE SPACES.
               15 WRK-INP-PAGINA-EGTM  PIC  9(004)         VALUE ZEROS.
               15 WRK-INP-OCORR-EGTM   PIC  9(002)         VALUE ZEROS.
               15 WRK-INP-CNPJOC-EGTM  PIC  9(009)         VALUE ZEROS.
               15 WRK-INP-FASE-EGTM    PIC  9(001)         VALUE ZEROS.
               15 FILLER               PIC  X(027)         VALUE SPACES.
             10 WRK-INP-NOME-EGTM      PIC  X(040)         VALUE SPACES.
             10 WRK-INP-CNPJCPF-EGTM.
               15 WRK-INP-CCNPJCPF-EGTM PIC  ZZZ.ZZZ.ZZ9   VALUE ZEROS.
               15 FILLER                PIC  X(001)        VALUE SPACES.
FS2511*        15 WRK-INP-FILIAL-EGTM   PIC  9(004)        VALUE ZEROS.
FS2511         15 WRK-INP-FILIAL-EGTM   PIC  X(04)         VALUE SPACES.
               15 FILLER                PIC  X(001)        VALUE SPACES.
               15 WRK-INP-CONTROLE-EGTM PIC  9(002)        VALUE ZEROS.
             10 WRK-INP-BCAGCTA-EGTM.
               15 WRK-INP-BANCO-EGTM    PIC  9(003)        VALUE ZEROS.
               15 FILLER                PIC  X(001)        VALUE SPACES.
               15 WRK-INP-AGENCIA-EGTM  PIC  9(005)        VALUE ZEROS.
               15 FILLER                PIC  X(001)        VALUE SPACES.
               15 WRK-INP-CONTA-EGTM    PIC  9(013)        VALUE ZEROS.
             10 WRK-INP-PRODUTO-EGTM.
               15 WRK-INP-CODPROD-EGTM  PIC  9(003)        VALUE ZEROS.
               15 WRK-INP-DESPROD-EGTM  PIC  X(012)        VALUE SPACES.
             10 WRK-INP-SUBPRODUTO-EGTM.
               15 WRK-INP-CODSUBP-EGTM  PIC  9(003)        VALUE ZEROS.
               15 WRK-INP-DESSUBP-EGTM  PIC  X(012)        VALUE SPACES.
             10 WRK-INP-OPERAC-EGTM     PIC  9(013)        VALUE ZEROS.
             10 FILLER REDEFINES        WRK-INP-OPERAC-EGTM.
               15 WRK-INP-DANO-EGTM     PIC  9(004).
               15 WRK-INP-NSEQ-EGTM     PIC  9(009).
             10 WRK-INP-SITUAC-EGTM     PIC  X(015)        VALUE SPACES.
             10 WRK-INP-SIT1-EGTM       PIC  X(013)        VALUE SPACES.
             10 WRK-INP-SIT2-EGTM.
               15 WRK-INP-SIT2-EGTM-N   PIC  9(009)        VALUE ZEROS.
             10 WRK-INP-SIT3-EGTM       PIC  X(013)        VALUE SPACES.
             10 WRK-INP-SIT4-EGTM.
               15 WRK-INP-SIT4-EGTM-N   PIC  9(009)        VALUE ZEROS.
             10 WRK-INP-CARTCONS-EGTM   PIC  X(003)        VALUE SPACES.
             10 WRK-INP-NUM1CONS-EGTM   PIC  X(004)        VALUE SPACES.
             10 WRK-INP-NUM2CONS-EGTM   PIC  X(005)        VALUE SPACES.
             10 WRK-INP-NUM3CONS-EGTM   PIC  X(009)        VALUE SPACES.
             10 WRK-INP-VLOPER-EGTM     PIC  X(014)        VALUE SPACES.
             10 WRK-INP-TPTAXA-EGTM     PIC  X(015)        VALUE SPACES.
             10 WRK-INP-VLTAROP-EGTM    PIC  X(014)        VALUE SPACES.
             10 WRK-INP-TXJURO-EGTM     PIC  X(009)        VALUE SPACES.
             10 WRK-INP-DEJURO-EGTM     PIC  X(004)        VALUE SPACES.
             10 WRK-INP-VLTARTI-EGTM    PIC  X(014)        VALUE SPACES.
             10 WRK-INP-DTVENC-EGTM     PIC  X(010)        VALUE SPACES.
             10 WRK-INP-VLJURO-EGTM     PIC  X(014)        VALUE SPACES.
             10 WRK-INP-PRZMED-EGTM.
               15 WRK-INP-PRZMED-EGTM-N PIC  9(003)        VALUE ZEROS.
             10 WRK-INP-VLIOF-EGTM      PIC  X(014)        VALUE SPACES.
             10 WRK-INP-QTDTIT-EGTM     PIC  X(006)        VALUE SPACES.
             10 WRK-INP-VLLIQU-EGTM     PIC  X(014)        VALUE SPACES.
             10 WRK-INP-GARANT-EGTM     PIC  X(020)        VALUE SPACES.
             10 WRK-INP-VLLIBE-EGTM     PIC  X(014)        VALUE SPACES.
             10 WRK-INP-MEIENT-EGTM     PIC  X(015)        VALUE SPACES.
             10 WRK-INP-DTOPER-EGTM     PIC  X(010)        VALUE SPACES.
             10 WRK-INP-MENSA-EGTM      PIC  X(079)        VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA TELA DCOMEGTM  - OUTPUT ***'.
      *----------------------------------------------------------------*

       01  WRK-OUTPUT-EGTM.
           05 WRK-OUT-LL-EGTM           PIC S9(004) COMP   VALUE +000.
           05 WRK-OUT-ZZ-EGTM           PIC S9(004) COMP   VALUE ZEROS.
           05 WRK-OUT-DADOS-EGTM.
             10 WRK-OUT-SENHAS-EGTM     PIC  X(037)        VALUE SPACES.
             10 WRK-OUT-COMANDO-ATTR-EGTM
                                        PIC S9(004) COMP   VALUE ZEROS.
             10 WRK-OUT-COMANDO-EGTM    PIC  X(068)        VALUE SPACES.
             10 WRK-OUT-OCULTOS-EGTM.
               15 WRK-OUT-TIPOOPC-EGTM  PIC  X(001)        VALUE SPACES.
               15 WRK-OUT-TPPESQ-EGTM   PIC  9(001)        VALUE ZEROS.
               15 WRK-OUT-TMSTAMP-EGTM  PIC  X(026)        VALUE SPACES.
               15 WRK-OUT-CHAMADOR-EGTM PIC  X(008)        VALUE SPACES.
               15 WRK-OUT-PAGINA-EGTM   PIC  9(004)        VALUE ZEROS.
               15 WRK-OUT-OCORR-EGTM    PIC  9(002)        VALUE ZEROS.
               15 WRK-OUT-CNPJOC-EGTM   PIC  9(009)        VALUE ZEROS.
               15 WRK-OUT-FASE-EGTM     PIC  9(001)        VALUE ZEROS.
               15 FILLER                PIC  X(027)        VALUE SPACES.
             10 WRK-OUT-NOME-EGTM       PIC  X(040)        VALUE SPACES.
             10 WRK-OUT-CNPJCPF-EGTM    PIC  X(019)        VALUE SPACES.
             10 WRK-OUT-BCAGCTA-EGTM    PIC  X(023)        VALUE SPACES.
             10 WRK-OUT-PRODUTO-EGTM.
               15 WRK-OUT-CODPROD-EGTM  PIC  9(003)        VALUE ZEROS.
               15 WRK-OUT-DESPROD-EGTM  PIC  X(012)        VALUE SPACES.
             10 WRK-OUT-SUBPRODUTO-EGTM.
               15 WRK-OUT-CODSUBP-EGTM  PIC  9(003)        VALUE ZEROS.
               15 WRK-OUT-DESSUBP-EGTM  PIC  X(012)        VALUE SPACES.
             10 WRK-OUT-OPERAC-EGTM     PIC  9(013)        VALUE ZEROS.
             10 WRK-OUT-SITUAC-EGTM     PIC  X(015)        VALUE SPACES.
             10 WRK-OUT-SIT1-EGTM       PIC  X(013)        VALUE SPACES.
             10 WRK-OUT-SIT2-EGTM.
               15 WRK-OUT-SIT2-EGTM-N   PIC  9(009)        VALUE ZEROS.
             10 WRK-OUT-SIT3-EGTM       PIC  X(013)        VALUE SPACES.
             10 WRK-OUT-SIT4-EGTM.
               15 WRK-OUT-SIT4-EGTM-N   PIC  9(009)        VALUE ZEROS.
             10 WRK-OUT-CARTCONS-EGTM-X.
               15  WRK-OUT-CARTCONS-EGTM    PIC X(03)      VALUE SPACES.
             10 WRK-OUT-NUM1CONS-EGTM-X.
               15  WRK-OUT-NUM1CONS-EGTM    PIC 9(04)      VALUE ZEROS.
             10 WRK-OUT-NUM2CONS-EGTM-X.
               15  WRK-OUT-NUM2CONS-EGTM    PIC 9(05)      VALUE ZEROS.
             10 WRK-OUT-NUM3CONS-EGTM-X.
               15  WRK-OUT-NUM3CONS-EGTM    PIC 9(09)      VALUE ZEROS.
             10 WRK-OUT-VLOPER-EGTM.
               15 WRK-OUT-VLOPER-EGTM-N PIC  ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
             10 WRK-OUT-TPTAXA-EGTM     PIC  X(015)        VALUE SPACES.
             10 WRK-OUT-VLTAROP-EGTM.
               15 WRK-OUT-VLTAROP-EGTM-N PIC ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
             10 WRK-OUT-TXJURO-EGTM.
               15 WRK-OUT-TXJURO-EGTM-N PIC  ZZ9,99999     VALUE ZEROS.
             10 WRK-OUT-DEJURO-EGTM     PIC  X(004)        VALUE SPACES.
             10 WRK-OUT-VLTARTI-EGTM.
               15 WRK-OUT-VLTARTI-EGTM-N PIC ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
             10 WRK-OUT-DTVENC-EGTM     PIC  X(010)        VALUE SPACES.
             10 WRK-OUT-VLJURO-EGTM.
               15 WRK-OUT-VLJURO-EGTM-N PIC  ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
             10 WRK-OUT-PRZMED-EGTM.
               15 WRK-OUT-PRZMED-EGTM-N PIC  9(003)        VALUE ZEROS.
             10 WRK-OUT-VLIOF-EGTM.
               15 WRK-OUT-VLIOF-EGTM-N  PIC  ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
             10 WRK-OUT-QTDTIT-EGTM.
               15 WRK-OUT-QTDTIT-EGTM-N PIC  9(006)        VALUE ZEROS.
             10 WRK-OUT-VLLIQU-EGTM.
               15 WRK-OUT-VLLIQU-EGTM-N PIC  ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
             10 WRK-OUT-GARANT-EGTM     PIC  X(020)        VALUE SPACES.
             10 WRK-OUT-VLLIBE-EGTM.
               15 WRK-OUT-VLLIBE-EGTM-N PIC  ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
             10 WRK-OUT-MEIENT-EGTM     PIC  X(015)        VALUE SPACES.
             10 WRK-OUT-DTOPER-EGTM     PIC  X(010)        VALUE SPACES.
             10 WRK-OUT-MENSA-EGTM      PIC  X(079)        VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA TELA DCOMEGTM - BRAD0660 ***'.
      *----------------------------------------------------------------*

       01  WRK-660-DCOMEGTM.
           05  WRK-660-LL-AREA-EGTM    PIC  9(004) COMP    VALUE 0000.
           05  WRK-660-LL-MENSA-EGTM   PIC  9(004) COMP    VALUE 0000.
           05  WRK-660-SENHAS-EGTM     PIC  9(004) COMP    VALUE 0037.
           05  WRK-660-COMANDO-EGTM    PIC  9(004) COMP    VALUE 2070.
           05  WRK-660-OCULTOS-EGTM    PIC  9(004) COMP    VALUE 0079.
           05  WRK-660-NOME-EGTM       PIC  9(004) COMP    VALUE 0040.
           05  WRK-660-CNPJCPF-EGTM    PIC  9(004) COMP    VALUE 0019.
           05  WRK-660-BCAGCTA-EGTM    PIC  9(004) COMP    VALUE 0023.
           05  WRK-660-CODPROD-EGTM    PIC  9(004) COMP    VALUE 0003.
           05  WRK-660-DESPROD-EGTM    PIC  9(004) COMP    VALUE 0012.
           05  WRK-660-CODSUBP-EGTM    PIC  9(004) COMP    VALUE 0003.
           05  WRK-660-DESSUBP-EGTM    PIC  9(004) COMP    VALUE 0012.
           05  WRK-660-OPERAC-EGTM     PIC  9(004) COMP    VALUE 0013.
           05  WRK-660-SITUAC-EGTM     PIC  9(004) COMP    VALUE 0015.
           05  WRK-660-SIT1-EGTM       PIC  9(004) COMP    VALUE 0013.
           05  WRK-660-SIT2-EGTM       PIC  9(004) COMP    VALUE 0009.
           05  WRK-660-SIT3-EGTM       PIC  9(004) COMP    VALUE 0013.
           05  WRK-660-SIT4-EGTM       PIC  9(004) COMP    VALUE 0009.
           05  WRK-660-CARTCONS-EGTM   PIC  9(004) COMP    VALUE 0003.
           05  WRK-660-NUM1CONS-EGTM   PIC  9(004) COMP    VALUE 0004.
           05  WRK-660-NUM2CONS-EGTM   PIC  9(004) COMP    VALUE 0005.
           05  WRK-660-NUM3CONS-EGTM   PIC  9(004) COMP    VALUE 0009.
           05  WRK-660-VLOPER-EGTM     PIC  9(004) COMP    VALUE 0014.
           05  WRK-660-TPTAXA-EGTM     PIC  9(004) COMP    VALUE 0015.
           05  WRK-660-VLTAROP-EGTM    PIC  9(004) COMP    VALUE 0014.
           05  WRK-660-TXJURO-EGTM     PIC  9(004) COMP    VALUE 0009.
           05  WRK-660-DEJURO-EGTM     PIC  9(004) COMP    VALUE 0004.
           05  WRK-660-VLTARTI-EGTM    PIC  9(004) COMP    VALUE 0014.
           05  WRK-660-DTVENC-EGTM     PIC  9(004) COMP    VALUE 0010.
           05  WRK-660-VLJURO-EGTM     PIC  9(004) COMP    VALUE 0014.
           05  WRK-660-PRZMED-EGTM     PIC  9(004) COMP    VALUE 0003.
           05  WRK-660-VLIOF-EGTM      PIC  9(004) COMP    VALUE 0014.
           05  WRK-660-QTDTIT-EGTM     PIC  9(004) COMP    VALUE 0006.
           05  WRK-660-VLLIQU-EGTM     PIC  9(004) COMP    VALUE 0014.
           05  WRK-660-GARANT-EGTM     PIC  9(004) COMP    VALUE 0020.
           05  WRK-660-VLLIBE-EGTM     PIC  9(004) COMP    VALUE 0014.
           05  WRK-660-MEIENT-EGTM     PIC  9(004) COMP    VALUE 0015.
           05  WRK-660-DTOPER-EGTM     PIC  9(004) COMP    VALUE 0010.
           05  WRK-660-MENSA-EGTM      PIC  9(004) COMP    VALUE 0079.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(080)         VALUE
           '*** AREA DE COMUNICACAO PROGRAMA DCOM4266              ***'.
      *----------------------------------------------------------------*

       01  4266E-ENTRADA.
           03  4266E-COD-RETORNO             PIC  X(004).
           03  4266E-MSG-RETORNO             PIC  X(079).
           03  4266E-RESTART                 PIC  9(005).
           03  4266E-FLAG                    PIC  X(001).
           03  4266E-CFUNC-BDSCO             PIC  9(009).
           03  4266E-CTERM                   PIC  X(008).
           03  4266E-DANO-OPER-DESC          PIC  9(004).
           03  4266E-NSEQ-OPER-DESC          PIC  9(009).
           03  4266E-CCNPJ-CPF               PIC  9(009).
           03  4266E-CFLIAL-CNPJ             PIC  9(004).

       01  4266S-SAIDA.
           03  4266S-HEADER.
               05  4266S-COD-RETORNO         PIC  X(004).
               05  4266S-MSG-RETORNO         PIC  X(079).
               05  4266S-RESTART             PIC  9(005).
               05  4266S-FLAG                PIC  X(001).
           03  4266S-DADOS.
               05  4266S-CINDCD-FREQ-TX      PIC  X(02).
               05  4266S-CTPO-TX-JURO-DESC   PIC  9(03).
               05  4266S-CSGL-TPO-TX-JURO    PIC  X(06).
               05  4266S-IRSUMO-TPO-TX-JURO  PIC  X(15).
               05  4266S-ITPO-TX-JURO-DESC   PIC  X(30).

           03  4266S-CONSISTENCIA          REDEFINES 4266S-DADOS.
               05  4266S-TAMANHO-ERRO      PIC  9(001).
               05  4266S-TABELA-ERROS      OCCURS 02 TIMES.
                   07  4266S-CAMPOS-ERRO   PIC  9(001).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DO DCOM5420                 ***'.
      *----------------------------------------------------------------*

       01  5420-ENTRADA.
           05  5420-ENT-HEADER.
               10   WRK-5420E-COD-RETORNO         PIC  X(004).
               10   WRK-5420E-MSG-RETORNO         PIC  X(079).
               10   WRK-5420E-RESTART             PIC  9(005).
               10   WRK-5420E-FLAG                PIC  X(001).
               10   WRK-5420E-CFUNC-BDSCO         PIC  9(009).
               10   WRK-5420E-CTERM               PIC  X(08).
           05  WRK-5420E-CONSISTENCIA.
               10   WRK-5420E-DANO-OPER-DESC      PIC  9(04).
               10   WRK-5420E-NSEQ-OPER-DESC      PIC  9(09).
       01  5420-SAIDA.
           05  5420-SAI-HEADER.
               10  WRK-5420S-COD-RETORNO     PIC  X(04).
               10  WRK-5420S-MSG-RETORNO     PIC  X(79).
               10  WRK-5420S-RESTART         PIC  9(05).
               10  WRK-5420S-FLAG            PIC  X(01).
               10  WRK-5420S-CONSISTENCIA.
                 15   WRK-5420S-TAMANHO-ERRO     PIC 9(01).
                 15   WRK-5420S-TABELA-ERROS     OCCURS  02 TIMES.
                  20  WRK-5420S-CAMPOS-ERROS     PIC 9(01).
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DO MODULO DCOM4174 ***'.
      *----------------------------------------------------------------*

       COPY 'I#DCOMXJ'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA BRAD7100 ***'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** DCOM0507 - FIM DA AREA DE WORKING ***'.
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

           MOVE SPACES                 TO WRK-OUT-DADOS-EGTM.

           PERFORM 2000-PROCESSAR-MENSAGEM.

           GO TO 0000-INICIAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000-PROCESSAR-MENSAGEM         SECTION.
      *----------------------------------------------------------------*

           MOVE 'DCOMEGTM'             TO WRK-TELA

           IF  WRK-TRANSACAO           NOT EQUAL 'DCOM0507'
               PERFORM 2100-RECEBER-CONTROLE
           ELSE
               PERFORM 2200-PROCESSAR-DCOMEGTM
           END-IF.

           IF  WRK-TELA                EQUAL 'DCOMEGTM'
               PERFORM 2300-PROCESSAR-BRAD0660
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2100-RECEBER-CONTROLE           SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO WRK-INCONSIS.

PROC01     IF  WRK-TRANSACAO           EQUAL 'DCOM3924'
PROC01         MOVE WRK-MENSAGEM          TO WRK-COMU-AREA-3924
PROC01         MOVE WRK-COMU-CHAMADOR-3924
PROC01                                    TO WRK-INP-CHAMADOR-EGTM
PROC01                                       WRK-OUT-CHAMADOR-EGTM
PROC01                                       WRK-TRANSACAO
PROC01         MOVE WRK-COMU-OPCAO-3924
PROC01                                    TO WRK-COMU-TPOPCAO
PROC01         MOVE WRK-COMU-OPER-3924    TO WRK-COMU-OPERACAO
PROC01         MOVE WRK-COMU-BANCO-3924   TO WRK-COMU-BANCO
PROC01         MOVE WRK-COMU-TIMESTAMP-3924
PROC01                                    TO WRK-COMU-TMSTAMP
PROC01         MOVE WRK-COMU-PAG-3924     TO WRK-COMU-PAG
PROC01         MOVE WRK-COMU-OCORR-3924   TO WRK-COMU-OCORR
PROC01         MOVE WRK-COMU-CNPJCPF-3924 TO WRK-COMU-CCPFCNPJ
PROC01         MOVE WRK-COMU-FILIAL-3924  TO WRK-COMU-CFLIAL
PROC01         MOVE WRK-COMU-CONTROL-3924 TO WRK-COMU-CCTRL
PROC01     ELSE
PROC01         MOVE WRK-MENSAGEM          TO WRK-COMU-AREA
PROC01         MOVE WRK-TRANSACAO      TO WRK-INP-CHAMADOR-EGTM
PROC01                                    WRK-OUT-CHAMADOR-EGTM
PROC01     END-IF.

           MOVE WRK-COMU-TPOPCAO       TO WRK-OUT-TIPOOPC-EGTM
                                          WRK-INP-TIPOOPC-EGTM.
           MOVE WRK-COMU-OPERACAO      TO WRK-INP-OPERAC-EGTM
                                          WRK-OUT-OPERAC-EGTM.
           MOVE WRK-COMU-BANCO         TO WRK-INP-BANCO-EGTM
           MOVE WRK-COMU-TMSTAMP       TO WRK-OUT-TMSTAMP-EGTM
                                          WRK-INP-TMSTAMP-EGTM.

           IF  WRK-TRANSACAO            EQUAL 'DCOM0502'
               MOVE 1                      TO WRK-INP-TPPESQ-EGTM
                                              WRK-OUT-TPPESQ-EGTM
           ELSE
               MOVE WRK-COMU-PAG           TO WRK-INP-PAGINA-EGTM
                                              WRK-OUT-PAGINA-EGTM
               MOVE WRK-COMU-OCORR         TO WRK-INP-OCORR-EGTM
                                              WRK-OUT-OCORR-EGTM
               IF  WRK-TRANSACAO         EQUAL 'DCOM0503'
                   MOVE 2                  TO WRK-INP-TPPESQ-EGTM
                                              WRK-OUT-TPPESQ-EGTM
               ELSE
                   IF  WRK-COMU-CCPFCNPJ       IS NUMERIC AND
                       WRK-COMU-CCPFCNPJ          GREATER ZEROS
                       MOVE 3                  TO WRK-INP-TPPESQ-EGTM
                                                  WRK-OUT-TPPESQ-EGTM
                   ELSE
                       MOVE 2                  TO WRK-INP-TPPESQ-EGTM
                                                  WRK-OUT-TPPESQ-EGTM
                   END-IF
               END-IF
           END-IF.

           PERFORM 2110-ACESSAR-DCOM4174

           IF  WRK-INCONSIS             EQUAL '*'
               GO TO 2100-99-FIM
           END-IF

           PERFORM 2111-MONTAR-TELA

           IF  WRK-INCONSIS             NOT EQUAL '*'
               MOVE WRK-MSG003          TO  WRK-OUT-MENSA-EGTM
               MOVE '1'                 TO  WRK-INP-FASE-EGTM
                                            WRK-OUT-FASE-EGTM
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2110-ACESSAR-DCOM4174           SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO WRK-SQLCA
                                          ERRO-AREA.

           INITIALIZE 4174-ENTRADA-ROTEADOR
                      4174-SAIDA-ROTEADOR
                      ERRO-AREA.

           MOVE WRK-COD-USER           TO 4174-ENT-CFUNC-BDSCO.
           MOVE LNK-IO-LTERM           TO 4174-ENT-CTERM.
           MOVE WRK-INP-DANO-EGTM      TO 4174-ENT-DANO-OPER-DESC
           MOVE WRK-INP-NSEQ-EGTM      TO 4174-ENT-NSEQ-OPER-DESC.
           MOVE 'DCOM4174'             TO WRK-MODULO

           CALL WRK-MODULO             USING 4174-ENTRADA-ROTEADOR
                                             4174-SAIDA-ROTEADOR
                                             ERRO-AREA
                                             WRK-SQLCA.

           EVALUATE 4174-SAI-COD-RETORNO
              WHEN '0000'
                    CONTINUE

              WHEN '0099'
                    MOVE 'DB2'         TO ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO

              WHEN OTHER
                   PERFORM 2220-RETORNAR-CHAMADOR
                   MOVE 4174-SAI-MSG-RETORNO     TO WRK-COMU-MENSA
                   MOVE WRK-COMU-AREA            TO WRK-MENSAGEM
                   MOVE 4174-SAI-MSG-RETORNO     TO WRK-OUT-MENSA-EGTM
                   MOVE '*'                      TO WRK-INCONSIS
           END-EVALUATE.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2111-MONTAR-TELA                SECTION.
      *----------------------------------------------------------------*

           MOVE 4174-SAI-DANO-BASE            TO WRK-OUT-NUM1CONS-EGTM
           MOVE 4174-SAI-CDEPDC               TO WRK-OUT-NUM2CONS-EGTM
           MOVE 4174-SAI-CSEQ-STUDO           TO WRK-OUT-NUM3CONS-EGTM
           MOVE 4174-SAI-CCART-LIM-CREDT      TO WRK-OUT-CARTCONS-EGTM.

           MOVE 4174-SAI-IPSSOA-DESC-COML     TO WRK-OUT-NOME-EGTM.
           MOVE 4174-SAI-CCNPJ-CPF            TO WRK-INP-CNPJOC-EGTM
                                                 WRK-OUT-CNPJOC-EGTM.

           STRING 4174-SAI-CCNPJ-CPF(1:3) '.'
                  4174-SAI-CCNPJ-CPF(4:3) '.'
                  4174-SAI-CCNPJ-CPF(7:3) '/'
                  4174-SAI-CFLIAL-CNPJ    '-'
                  4174-SAI-CCTRL-CNPJ-CPF
           DELIMITED BY SIZE                INTO WRK-OUT-CNPJCPF-EGTM.

           STRING 4174-SAI-CBCO           '/'
                 4174-SAI-CAG-BCRIA       '/'
                 4174-SAI-CCTA-BCRIA-CLI
           DELIMITED BY SIZE                INTO WRK-OUT-BCAGCTA-EGTM.

           MOVE 4174-SAI-CPRODT               TO WRK-OUT-CODPROD-EGTM.
           MOVE 4174-SAI-IABREV-PRODT         TO WRK-OUT-DESPROD-EGTM.
           MOVE 4174-SAI-CSPROD-DESC-COML     TO WRK-OUT-CODSUBP-EGTM.
           MOVE 4174-SAI-IRSUMO-SPROD-DESC    TO WRK-OUT-DESSUBP-EGTM.
           MOVE 4174-SAI-IRSUMO-SIT-DESC      TO WRK-OUT-SITUAC-EGTM.

           IF 4174-SAI-CCONTR-LIM-DESC        GREATER ZEROS
              MOVE 'CONTR LIMITE:'            TO WRK-OUT-SIT1-EGTM
              MOVE 4174-SAI-CCONTR-LIM-DESC   TO WRK-OUT-SIT2-EGTM-N
           ELSE
              IF 4174-SAI-CCONTR-CONVE-DESC   GREATER ZEROS AND
                (4174-SAI-CELMTO-DESC-COML    EQUAL 8 OR 9 )
                 IF  4174-SAI-CELMTO-DESC-COML   EQUAL 8
                     MOVE 'CONV GERAL..:'     TO WRK-OUT-SIT1-EGTM
                     MOVE 4174-SAI-CCONTR-CONVE-DESC
                                              TO WRK-OUT-SIT2-EGTM-N
                 ELSE
                     MOVE 'CONV CLIENTE:'     TO WRK-OUT-SIT1-EGTM
                     MOVE 4174-SAI-CCONTR-CONVE-DESC
                                              TO WRK-OUT-SIT2-EGTM-N
                 END-IF
                 PERFORM 2112-VERIFICA-CONVENIO
              ELSE
                IF 4174-SAI-NSMULA-OPER-DESC  GREATER  ZEROS
                   MOVE 'SIMULACAO...:'       TO WRK-OUT-SIT3-EGTM
                   MOVE 4174-SAI-NSMULA-OPER-DESC
                                              TO WRK-OUT-SIT4-EGTM-N
                END-IF
             END-IF
           END-IF.

           MOVE 4174-SAI-VOPER-DESC-APURA    TO WRK-OUT-VLOPER-EGTM-N.
           MOVE 4174-SAI-VTAC-OPER-DESC      TO WRK-OUT-VLTAROP-EGTM-N

           MOVE 4174-SAI-VTARIF-REG-TITLO    TO WRK-OUT-VLTARTI-EGTM-N.
           MOVE 4174-SAI-DVCTO-FNAL-OPER     TO WRK-OUT-DTVENC-EGTM.

           INSPECT WRK-OUT-DTVENC-EGTM       REPLACING ALL '.' BY '/'.

           MOVE 4174-SAI-VJURO-OPER-DESC     TO WRK-OUT-VLJURO-EGTM-N.
           MOVE 4174-SAI-TMED-PONDE-OPER     TO WRK-OUT-PRZMED-EGTM-N.
           MOVE 4174-SAI-VIOF-OPER-DESC      TO WRK-OUT-VLIOF-EGTM-N.
           MOVE 4174-SAI-QTITLO-CHEQ-PCELA   TO WRK-OUT-QTDTIT-EGTM-N.
           MOVE 4174-SAI-VLIQ-OPER-DESC      TO WRK-OUT-VLLIQU-EGTM-N.
           MOVE 4174-SAI-ITPO-GARNT          TO WRK-OUT-GARANT-EGTM.

           IF  4174-SAI-VLIBRC-DESC-COML     EQUAL ZEROS
               MOVE 4174-SAI-VLIBRC-ANTCP-OPER
                                              TO WRK-OUT-VLLIBE-EGTM-N
           ELSE
               MOVE 4174-SAI-VLIBRC-DESC-COML TO WRK-OUT-VLLIBE-EGTM-N
           END-IF.

           MOVE 4174-SAI-IRSUMO-MEIO-ENTRD   TO WRK-OUT-MEIENT-EGTM.
           MOVE 4174-SAI-DINIC-OPER-DESC     TO WRK-OUT-DTOPER-EGTM.

           INSPECT WRK-OUT-DTOPER-EGTM       REPLACING ALL '.' BY '/'.

           MOVE 4174-SAI-IRSUMO-TPO-TX-JURO  TO  WRK-OUT-TPTAXA-EGTM
           MOVE 'A.M.'                       TO  WRK-OUT-DEJURO-EGTM
           MOVE 4174-SAI-PTX-JURO-MES        TO  WRK-OUT-TXJURO-EGTM-N.

      *----------------------------------------------------------------*
       2111-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2112-VERIFICA-CONVENIO          SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO WRK-SQLCA
                                          ERRO-AREA.

           INITIALIZE WRK-5355E-ENTRADA
                      WRK-5355S-SAIDA
                      ERRO-AREA.

           MOVE WRK-COD-USER-R             TO WRK-5355E-CFUNC-BDSCO.
           MOVE LNK-IO-LTERM               TO WRK-5355E-CTERM.
           MOVE 4174-SAI-CCONTR-CONVE-DESC TO WRK-5355E-CCONVE-GRAL-DESC
           MOVE 4174-SAI-CPRODT            TO WRK-5355E-CPRODT.
           MOVE 4174-SAI-CSPROD-DESC-COML  TO WRK-5355E-CSPROD-DESC-COML
           MOVE 4174-SAI-CCNPJ-CPF         TO WRK-5355E-CCNPJ-CPF.
           MOVE 4174-SAI-CFLIAL-CNPJ       TO WRK-5355E-CFLIAL-CNPJ.
           MOVE 4174-SAI-CCTRL-CNPJ-CPF    TO WRK-5355E-CCTRL-CNPJ-CPF.
           MOVE 'S'                        TO WRK-5355E-ROTENT-FLAG-FORM
           IF   4174-SAI-CELMTO-DESC-COML  EQUAL 8
                MOVE 'G'                   TO WRK-5355E-TIPO-CONVENIO
           ELSE
                MOVE 'C'                   TO WRK-5355E-TIPO-CONVENIO
           END-IF

           MOVE 'DCOM5355'                 TO WRK-MODULO.

           CALL WRK-MODULO                 USING WRK-5355E-ENTRADA
                                                 WRK-5355S-SAIDA
                                                 ERRO-AREA
                                                 WRK-SQLCA.

           EVALUATE  WRK-5355S-COD-RETORNO
              WHEN '0000'
                    IF  4174-SAI-CELMTO-DESC-COML   EQUAL 8
                        MOVE 'CONV GERAL..:'   TO WRK-OUT-SIT1-EGTM
                        MOVE 4174-SAI-CCONTR-CONVE-DESC
                                                TO WRK-OUT-SIT2-EGTM-N
                    ELSE
                        MOVE 'CONV CLIENTE:'    TO WRK-OUT-SIT1-EGTM
                        MOVE 4174-SAI-CCONTR-CONVE-DESC
                                                TO WRK-OUT-SIT2-EGTM-N
                    END-IF
              WHEN '0003'
                    CONTINUE

              WHEN '1075'
              WHEN '1125'
                    MOVE '0003'       TO WRK-5355S-COD-RETORNO

              WHEN '0099'
                    MOVE 'DB2'        TO ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO

              WHEN  OTHER
                    MOVE WRK-5355S-COD-RETORNO
                                      TO WRK-OUT-MENSA-EGTM
           END-EVALUATE.

      *----------------------------------------------------------------*
       2112-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *  ESSA ROTINA OBTEM A PERIODICIDADE DA TAXA                     *
      *----------------------------------------------------------------*
       2200-PROCESSAR-DCOMEGTM         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM             TO WRK-INPUT-EGTM.

           PERFORM 2215-DEVOLVER-TELA

           EVALUATE TRUE
               WHEN WRK-INP-PFK-EGTM     EQUAL 'H' OR
                   (WRK-INP-PFK-EGTM     EQUAL '.' AND
                    WRK-INP-COMANDO-EGTM EQUAL 'PFK01')
                    MOVE WRK-INP-MENSA-EGTM TO WRK-OUT-MENSA-EGTM

               WHEN WRK-INP-PFK-EGTM     EQUAL '3' OR
                   (WRK-INP-PFK-EGTM     EQUAL '.' AND
                    WRK-INP-COMANDO-EGTM EQUAL 'PFK03')
                    PERFORM 2220-RETORNAR-CHAMADOR

               WHEN WRK-INP-PFK-EGTM     EQUAL '5' OR
                   (WRK-INP-PFK-EGTM     EQUAL '.' AND
                    WRK-INP-COMANDO-EGTM EQUAL 'PFK05')
                    PERFORM 2230-RETORNAR-MENU-ROTINAS

               WHEN WRK-INP-PFK-EGTM     EQUAL 'A' OR
                   (WRK-INP-PFK-EGTM     EQUAL '.' AND
                    WRK-INP-COMANDO-EGTM EQUAL 'PFK10')
                    PERFORM 2240-RETORNAR-MENU-DCOM

PROC01         WHEN WRK-INP-PFK-EGTM     EQUAL  'B' OR
PROC01             (WRK-INP-PFK-EGTM     EQUAL  '.'  AND
PROC01              WRK-INP-COMANDO-EGTM EQUAL  'PFK11')
PROC01              PERFORM 2250-ACESSAR-CET
PROC01
               WHEN (WRK-INP-PFK-EGTM     EQUAL '.' AND
                     WRK-INP-COMANDO-EGTM EQUAL SPACES)
                     PERFORM 2255-TRATAR-ENTER

               WHEN  OTHER
                     MOVE WRK-MSG002      TO WRK-OUT-MENSA-EGTM
           END-EVALUATE.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2215-DEVOLVER-TELA              SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-TPPESQ-EGTM     TO WRK-OUT-TPPESQ-EGTM.
           MOVE WRK-INP-OCULTOS-EGTM    TO WRK-OUT-OCULTOS-EGTM.
           MOVE WRK-INP-NOME-EGTM       TO WRK-OUT-NOME-EGTM.
           MOVE WRK-INP-CNPJCPF-EGTM    TO WRK-OUT-CNPJCPF-EGTM.
           MOVE WRK-INP-BCAGCTA-EGTM    TO WRK-OUT-BCAGCTA-EGTM.
           MOVE WRK-INP-PRODUTO-EGTM    TO WRK-OUT-PRODUTO-EGTM.
           MOVE WRK-INP-SUBPRODUTO-EGTM TO WRK-OUT-SUBPRODUTO-EGTM.
           MOVE WRK-INP-OPERAC-EGTM     TO WRK-OUT-OPERAC-EGTM.
           MOVE WRK-INP-SITUAC-EGTM     TO WRK-OUT-SITUAC-EGTM.
           MOVE WRK-INP-SIT1-EGTM       TO WRK-OUT-SIT1-EGTM.
           MOVE WRK-INP-SIT2-EGTM       TO WRK-OUT-SIT2-EGTM.
           MOVE WRK-INP-SIT3-EGTM       TO WRK-OUT-SIT3-EGTM.
           MOVE WRK-INP-SIT4-EGTM       TO WRK-OUT-SIT4-EGTM.
           MOVE WRK-INP-NUM1CONS-EGTM   TO WRK-OUT-NUM1CONS-EGTM
           MOVE WRK-INP-NUM2CONS-EGTM   TO WRK-OUT-NUM2CONS-EGTM
           MOVE WRK-INP-NUM3CONS-EGTM   TO WRK-OUT-NUM3CONS-EGTM
           MOVE WRK-INP-CARTCONS-EGTM   TO WRK-OUT-CARTCONS-EGTM
           MOVE WRK-INP-VLOPER-EGTM     TO WRK-OUT-VLOPER-EGTM.
           MOVE WRK-INP-TPTAXA-EGTM     TO WRK-OUT-TPTAXA-EGTM.
           MOVE WRK-INP-VLTAROP-EGTM    TO WRK-OUT-VLTAROP-EGTM.
           MOVE WRK-INP-TXJURO-EGTM     TO WRK-OUT-TXJURO-EGTM.
           MOVE WRK-INP-DEJURO-EGTM     TO WRK-OUT-DEJURO-EGTM.
           MOVE WRK-INP-VLTARTI-EGTM    TO WRK-OUT-VLTARTI-EGTM.
           MOVE WRK-INP-DTVENC-EGTM     TO WRK-OUT-DTVENC-EGTM.
           MOVE WRK-INP-VLJURO-EGTM     TO WRK-OUT-VLJURO-EGTM.
           MOVE WRK-INP-PRZMED-EGTM     TO WRK-OUT-PRZMED-EGTM.
           MOVE WRK-INP-VLIOF-EGTM      TO WRK-OUT-VLIOF-EGTM.
           MOVE WRK-INP-QTDTIT-EGTM     TO WRK-OUT-QTDTIT-EGTM.
           MOVE WRK-INP-VLLIQU-EGTM     TO WRK-OUT-VLLIQU-EGTM.
           MOVE WRK-INP-GARANT-EGTM     TO WRK-OUT-GARANT-EGTM.
           MOVE WRK-INP-VLLIBE-EGTM     TO WRK-OUT-VLLIBE-EGTM.
           MOVE WRK-INP-MEIENT-EGTM     TO WRK-OUT-MEIENT-EGTM.
           MOVE WRK-INP-DTOPER-EGTM     TO WRK-OUT-DTOPER-EGTM.

      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2220-RETORNAR-CHAMADOR          SECTION.
      *----------------------------------------------------------------*

           PERFORM 2225-FORMATAR-COMU-AREA.

           MOVE WRK-INP-CHAMADOR-EGTM  TO WRK-TELA.
           MOVE WRK-CHNG               TO WRK-FUNCAO.
           MOVE WRK-COMU-AREA          TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2225-FORMATAR-COMU-AREA         SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  WRK-COMU-AREA.

           MOVE LENGTH                 OF WRK-COMU-AREA
                                       TO WRK-COMU-LL.

           MOVE WRK-INP-TIPOOPC-EGTM   TO WRK-COMU-TPOPCAO
           MOVE WRK-INP-TMSTAMP-EGTM   TO WRK-COMU-TMSTAMP.
           MOVE WRK-INP-OPERAC-EGTM    TO WRK-COMU-OPERACAO.
           MOVE WRK-INP-BANCO-EGTM     TO WRK-COMU-BANCO.
           MOVE WRK-INP-PAGINA-EGTM    TO WRK-COMU-PAG
           MOVE WRK-INP-OCORR-EGTM     TO WRK-COMU-OCORR
           MOVE 'DCOM0507'             TO WRK-COMU-TRANSACAO.

           EVALUATE WRK-INP-TPPESQ-EGTM
               WHEN 2
               WHEN 3
                    IF  WRK-INP-TPPESQ-EGTM     EQUAL 2
                        MOVE WRK-INP-AGENCIA-EGTM
                                                TO WRK-COMU-AGENCIA
                        MOVE WRK-INP-CONTA-EGTM TO WRK-COMU-CONTA
                    ELSE
                        MOVE WRK-INP-CNPJOC-EGTM
                                              TO WRK-COMU-CCPFCNPJ
                        MOVE WRK-INP-FILIAL-EGTM
                                              TO WRK-COMU-CFLIAL
                        MOVE WRK-INP-CONTROLE-EGTM
                                              TO WRK-COMU-CCTRL
                    END-IF
           END-EVALUATE.

      *----------------------------------------------------------------*
       2225-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2230-RETORNAR-MENU-ROTINAS      SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-COMU-AREA.

           MOVE LENGTH                 OF WRK-COMU-AREA
                                       TO WRK-COMU-LL.
           MOVE '5'                    TO WRK-COMU-PFK.
           MOVE WRK-CHNG               TO WRK-FUNCAO.
           MOVE 'DCOM0507'             TO WRK-TELA
                                          WRK-COMU-TRANSACAO.
           MOVE WRK-COMU-AREA          TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2240-RETORNAR-MENU-DCOM         SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-COMU-AREA.

           MOVE LENGTH                 OF WRK-COMU-AREA
                                       TO WRK-COMU-LL.
           MOVE 'A'                    TO WRK-COMU-PFK.
           MOVE WRK-CHNG               TO WRK-FUNCAO.
           MOVE 'DCOM0507'             TO WRK-TELA
                                          WRK-COMU-TRANSACAO.
           MOVE WRK-COMU-AREA          TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2240-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

PROC01*----------------------------------------------------------------*
PROC01 2250-ACESSAR-CET                SECTION.
PROC01*----------------------------------------------------------------*
PROC01
PROC01     INITIALIZE                  WRK-COMU-AREA-3924.
PROC01
PROC01     MOVE LENGTH                 OF WRK-COMU-AREA-3924
PROC01                                 TO WRK-COMU-LL-3924.
PROC01
PROC01     MOVE WRK-INP-TIPOOPC-EGTM   TO WRK-COMU-OPCAO-3924
PROC01     MOVE WRK-INP-TMSTAMP-EGTM   TO WRK-COMU-TIMESTAMP-3924.
PROC01     MOVE WRK-INP-OPERAC-EGTM    TO WRK-COMU-OPER-3924.
PROC01     MOVE WRK-INP-BANCO-EGTM     TO WRK-COMU-BANCO-3924.
PROC01     MOVE WRK-INP-PAGINA-EGTM    TO WRK-COMU-PAG-3924
PROC01     MOVE WRK-INP-OCORR-EGTM     TO WRK-COMU-OCORR-3924
PROC01     MOVE 'DCOM0507'             TO WRK-COMU-TRANSACAO-3924.
PROC01
PROC01     EVALUATE WRK-INP-TPPESQ-EGTM
PROC01         WHEN 2
PROC01         WHEN 3
PROC01              IF  WRK-INP-TPPESQ-EGTM     EQUAL 2
PROC01                  MOVE WRK-INP-AGENCIA-EGTM
PROC01                                          TO WRK-COMU-AGENC-3924
PROC01                  MOVE WRK-INP-CONTA-EGTM TO WRK-COMU-CONTA-3924
PROC01              ELSE
PROC01                  MOVE WRK-INP-CNPJOC-EGTM
PROC01                                        TO WRK-COMU-CNPJCPF-3924
PROC01                  MOVE WRK-INP-FILIAL-EGTM
PROC01                                        TO WRK-COMU-FILIAL-3924
PROC01                  MOVE WRK-INP-CONTROLE-EGTM
PROC01                                        TO WRK-COMU-CONTROL-3924
PROC01              END-IF
PROC01     END-EVALUATE.
PROC01
PROC01     MOVE 'DCOM3924'             TO WRK-TELA.
PROC01     MOVE WRK-INP-CHAMADOR-EGTM  TO WRK-COMU-CHAMADOR-3924.
PROC01
PROC01     MOVE WRK-CHNG               TO WRK-FUNCAO.
PROC01     MOVE WRK-COMU-AREA-3924     TO WRK-MENSAGEM.
PROC01
PROC01*----------------------------------------------------------------*
PROC01 2250-99-FIM.                    EXIT.
PROC01*----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2255-TRATAR-ENTER               SECTION.
      *----------------------------------------------------------------*
           MOVE '1'                    TO WRK-INP-FASE-EGTM
                                          WRK-OUT-FASE-EGTM
           MOVE WRK-MSG003             TO WRK-OUT-MENSA-EGTM
           .
      *----------------------------------------------------------------*
       2255-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2300-PROCESSAR-BRAD0660         SECTION.
      *----------------------------------------------------------------*

            MOVE LENGTH OF WRK-OUTPUT-EGTM
                                       TO  WRK-660-LL-MENSA-EGTM
                                           WRK-OUT-LL-EGTM
            MOVE LENGTH OF WRK-660-DCOMEGTM
                                       TO  WRK-660-LL-AREA-EGTM

           CALL 'BRAD0660'             USING WRK-OUTPUT-EGTM
                                             WRK-660-DCOMEGTM.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE RETURN-CODE        TO WRK-RETURN-CODE
               MOVE 10                 TO WRK-LOCAL-ERRO
               MOVE WRK-ERRO-BRAD0660  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-ISRT               TO WRK-FUNCAO.
           MOVE WRK-OUTPUT-EGTM        TO WRK-MENSAGEM.

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
               MOVE 'DCOM0507'         TO ERR-PGM
           ELSE
               MOVE 'SENHAS02'         TO ERR-PGM
               MOVE 'DCOM0507'         TO ERR-MODULO
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

           PERFORM 3000-FINALIZAR.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
