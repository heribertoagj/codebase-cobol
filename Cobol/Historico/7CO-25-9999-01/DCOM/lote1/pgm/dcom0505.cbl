      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID.     DCOM0505.
       AUTHOR.         FABRICA - ALTRAN.
      *================================================================*
      *                          A L T R A N                           *
      *----------------------------------------------------------------*
      *     PROGRAMA....: DCOM0505                                     *
      *     PROGRAMADOR.: FABRICA - ALTRAN.                            *
      *     DATA........: 03/08/2007                                   *
      *----------------------------------------------------------------*
      *     OBJETIVO....: EFETIVACAO DO ESTORNO DA OPERACAO            *
      *                                                                *
      *----------------------------------------------------------------*
      *     TELAS.......:                                              *
      *     DCOMEETM....: EFETIVACAO DO ESTORNO DA OPERACAO COM        *
      *                   JUSTIFICATIVA                                *
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
CAP001*                   ALTERACAO - CAPGEMINI                        *
.     *----------------------------------------------------------------*
.     *   PROGRAMADOR :   ANDREA             - CAPGEMINI               *
.     *   ANALISTA CAP:   ANDREA             - CAPGEMINI               *
.     *   ANALISTA DS.:   RICARDO JAMMAL     - BRADESCO - GRUPO 39     *
.     *   DATA........:   05/2021                                      *
.     *----------------------------------------------------------------*
.     *   OBJETIVO....: NAO PERMITIR ESTORNO TIPO DESCONTO 4 ENTRE OS  *
CAP001*                 DIAS 03 E 06.06.2021 - NORMAT 4734             *
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
           '*** DCOM0505 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** MENSAGENS ***'.
      *----------------------------------------------------------------*

       77  WRK-MSG004                  PIC  X(079)         VALUE
           'ESTORNO DA OPERACAO EFETUADA COM SUCESSO'.

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
           '*** AREA DE COMUNICACAO COM O MODULO DCOM5335 **'.
      *----------------------------------------------------------------*

       01  5335-ENTRADA.
           05 5335E-COD-RETORNO        PIC  X(004)         VALUE SPACES.
           05 5335E-MSG-RETORNO        PIC  X(079)         VALUE SPACES.
           05 5335E-RESTART            PIC  9(005)         VALUE ZEROS.
           05 5335E-FLAG               PIC  9(001)         VALUE ZEROS.
           05 5335E-CFUNC-BDSCO        PIC  9(009)         VALUE ZEROS.
           05 5335E-CTERM              PIC  X(008)         VALUE SPACES.
           05 5335E-OPERACAO.
              10 5335E-DANO-OPER-DESC  PIC  9(004)         VALUE ZEROS.
              10 5335E-NSEQ-OPER-DESC  PIC  9(009)         VALUE ZEROS.
           05 5335E-NPCELA-DESC-COML   PIC  9(005)         VALUE ZEROS.
           05 5335E-CELMTO-DESC-COML   PIC  9(003)         VALUE ZEROS.
           05 5335E-CEVNTO-DESC-COML   PIC  9(003)         VALUE ZEROS.
           05 5335E-ACAO               PIC  X(001)         VALUE SPACES.

       01  5335-SAIDA.
           05 5335S-COD-RETORNO        PIC  9(004)         VALUE ZEROS.
           05 5335S-MSG-RETORNO        PIC  X(079)         VALUE SPACES.

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
           05 WRK-5355E-CFLIAL-CNPJ      PIC  9(005)       VALUE ZEROS.
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
FS2511             15  WRK-COMU-CNPJCPF-3924
FS2511*                                  PIC  9(009)       VALUE ZEROS.
FS2511                                   PIC  X(009)       VALUE SPACES.
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

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREAS AUXILIARES PARA POOL5005 ***'.
      *----------------------------------------------------------------*

       01  WRK-POOL5005.
           05  WRK-GU                  PIC  X(004)         VALUE 'GU  '.
           05  WRK-CHNG                PIC  X(004)         VALUE 'CHNG'.
           05  WRK-ISRT                PIC  X(004)         VALUE 'ISRT'.
           05  WRK-ROLB                PIC  X(004)         VALUE 'ROLB'.
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
           '*** AREA DA TELA DCOMEETM - INPUT ***'.
      *----------------------------------------------------------------*

       01  WRK-INPUT-EETM.
           05 FILLER                   PIC  X(018)         VALUE SPACES.
           05 WRK-INP-PFK-EETM         PIC  X(001)         VALUE SPACES.
           05 WRK-INP-DADOS-EETM.
             10 WRK-INP-SENHAS-EETM    PIC  X(037)         VALUE SPACES.
             10 WRK-INP-COMANDO-EETM   PIC  X(068)         VALUE SPACES.
             10 WRK-INP-OCULTOS-EETM.
               15 WRK-INP-TIPOOPC-EETM PIC  X(001)         VALUE SPACES.
               15 WRK-INP-TPPESQ-EETM  PIC  9(001)         VALUE ZEROS.
               15 WRK-INP-TMSTAMP-EETM PIC  X(026)         VALUE SPACES.
               15 WRK-INP-CHAMADOR-EETM PIC X(008)         VALUE SPACES.
               15 WRK-INP-PAGINA-EETM  PIC  9(004)         VALUE ZEROS.
               15 WRK-INP-OCORR-EETM   PIC  9(002)         VALUE ZEROS.
               15 WRK-INP-CNPJOC-EETM  PIC  9(009)         VALUE ZEROS.
               15 WRK-INP-FASE-EETM    PIC  9(001)         VALUE ZEROS.
               15 FILLER               PIC  X(027)         VALUE SPACES.
             10 WRK-INP-NOME-EETM      PIC  X(040)         VALUE SPACES.
             10 WRK-INP-CNPJCPF-EETM.
FS2511*        15 WRK-INP-CCNPJCPF-EETM PIC  ZZZ.ZZZ.ZZ9   VALUE ZEROS.
FS2511         15 WRK-INP-CCNPJCPF-EETM PIC  X(011)        VALUE SPACES.
               15 FILLER                PIC  X(001)        VALUE SPACES.
FS2511*        15 WRK-INP-FILIAL-EETM   PIC  9(004)        VALUE ZEROS.
FS2511         15 WRK-INP-FILIAL-EETM   PIC  X(04)         VALUE SPACES.
               15 FILLER                PIC  X(001)        VALUE SPACES.
               15 WRK-INP-CONTROLE-EETM PIC  9(002)        VALUE ZEROS.
             10 WRK-INP-BCAGCTA-EETM.
               15 WRK-INP-BANCO-EETM    PIC  9(003)        VALUE ZEROS.
               15 FILLER                PIC  X(001)        VALUE SPACES.
               15 WRK-INP-AGENCIA-EETM  PIC  9(005)        VALUE ZEROS.
               15 FILLER                PIC  X(001)        VALUE SPACES.
               15 WRK-INP-CONTA-EETM    PIC  9(013)        VALUE ZEROS.
             10 WRK-INP-PRODUTO-EETM.
               15 WRK-INP-CODPROD-EETM  PIC  9(003)        VALUE ZEROS.
               15 WRK-INP-DESPROD-EETM  PIC  X(012)        VALUE SPACES.
             10 WRK-INP-SUBPRODUTO-EETM.
               15 WRK-INP-CODSUBP-EETM  PIC  9(003)        VALUE ZEROS.
               15 WRK-INP-DESSUBP-EETM  PIC  X(012)        VALUE SPACES.
             10 WRK-INP-OPERAC-EETM     PIC  9(013)        VALUE ZEROS.
             10 FILLER REDEFINES        WRK-INP-OPERAC-EETM.
               15 WRK-INP-DANO-EETM     PIC  9(004).
               15 WRK-INP-NSEQ-EETM     PIC  9(009).
             10 WRK-INP-SITUAC-EETM     PIC  X(015)        VALUE SPACES.
             10 WRK-INP-SIT1-EETM       PIC  X(013)        VALUE SPACES.
             10 WRK-INP-SIT2-EETM.
               15 WRK-INP-SIT2-EETM-N   PIC  9(009)        VALUE ZEROS.
             10 WRK-INP-SIT3-EETM       PIC  X(013)        VALUE SPACES.
             10 WRK-INP-SIT4-EETM.
               15 WRK-INP-SIT4-EETM-N   PIC  9(009)        VALUE ZEROS.
             10 WRK-INP-CARTCONS-EETM   PIC  X(003)        VALUE SPACES.
             10 WRK-INP-NUM1CONS-EETM   PIC  X(004)        VALUE SPACES.
             10 WRK-INP-NUM2CONS-EETM   PIC  X(005)        VALUE SPACES.
             10 WRK-INP-NUM3CONS-EETM   PIC  X(009)        VALUE SPACES.
             10 WRK-INP-VLOPER-EETM     PIC  X(014)        VALUE SPACES.
             10 WRK-INP-TPTAXA-EETM     PIC  X(015)        VALUE SPACES.
             10 WRK-INP-VLTAROP-EETM    PIC  X(014)        VALUE SPACES.
             10 WRK-INP-TXJURO-EETM     PIC  X(009)        VALUE SPACES.
             10 WRK-INP-DEJURO-EETM     PIC  X(004)        VALUE SPACES.
             10 WRK-INP-VLTARTI-EETM    PIC  X(014)        VALUE SPACES.
             10 WRK-INP-DTVENC-EETM     PIC  X(010)        VALUE SPACES.
             10 WRK-INP-VLJURO-EETM     PIC  X(014)        VALUE SPACES.
             10 WRK-INP-PRZMED-EETM.
               15 WRK-INP-PRZMED-EETM-N PIC  9(003)        VALUE ZEROS.
             10 WRK-INP-VLIOF-EETM      PIC  X(014)        VALUE SPACES.
             10 WRK-INP-QTDTIT-EETM     PIC  X(006)        VALUE SPACES.
             10 WRK-INP-VLLIQU-EETM     PIC  X(014)        VALUE SPACES.
             10 WRK-INP-GARANT-EETM     PIC  X(020)        VALUE SPACES.
             10 WRK-INP-VLLIBE-EETM     PIC  X(014)        VALUE SPACES.
             10 WRK-INP-MEIENT-EETM     PIC  X(015)        VALUE SPACES.
             10 WRK-INP-DTOPER-EETM     PIC  X(010)        VALUE SPACES.
             10 WRK-INP-MENSA-EETM      PIC  X(079)        VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA TELA DCOMEETM  - OUTPUT ***'.
      *----------------------------------------------------------------*

       01  WRK-OUTPUT-EETM.
           05 WRK-OUT-LL-EETM           PIC S9(004) COMP   VALUE +000.
           05 WRK-OUT-ZZ-EETM           PIC S9(004) COMP   VALUE ZEROS.
           05 WRK-OUT-DADOS-EETM.
             10 WRK-OUT-SENHAS-EETM     PIC  X(037)        VALUE SPACES.
             10 WRK-OUT-COMANDO-ATTR-EETM
                                        PIC S9(004) COMP   VALUE ZEROS.
             10 WRK-OUT-COMANDO-EETM    PIC  X(068)        VALUE SPACES.
             10 WRK-OUT-OCULTOS-EETM.
               15 WRK-OUT-TIPOOPC-EETM  PIC  X(001)        VALUE SPACES.
               15 WRK-OUT-TPPESQ-EETM   PIC  9(001)        VALUE ZEROS.
               15 WRK-OUT-TMSTAMP-EETM  PIC  X(026)        VALUE SPACES.
               15 WRK-OUT-CHAMADOR-EETM PIC  X(008)        VALUE SPACES.
               15 WRK-OUT-PAGINA-EETM   PIC  9(004)        VALUE ZEROS.
               15 WRK-OUT-OCORR-EETM    PIC  9(002)        VALUE ZEROS.
               15 WRK-OUT-CNPJOC-EETM   PIC  9(009)        VALUE ZEROS.
               15 WRK-OUT-FASE-EETM     PIC  9(001)        VALUE ZEROS.
               15 FILLER                PIC  X(027)        VALUE SPACES.
             10 WRK-OUT-NOME-EETM       PIC  X(040)        VALUE SPACES.
             10 WRK-OUT-CNPJCPF-EETM    PIC  X(019)        VALUE SPACES.
             10 WRK-OUT-BCAGCTA-EETM    PIC  X(023)        VALUE SPACES.
             10 WRK-OUT-PRODUTO-EETM.
               15 WRK-OUT-CODPROD-EETM  PIC  9(003)        VALUE ZEROS.
               15 WRK-OUT-DESPROD-EETM  PIC  X(012)        VALUE SPACES.
             10 WRK-OUT-SUBPRODUTO-EETM.
               15 WRK-OUT-CODSUBP-EETM  PIC  9(003)        VALUE ZEROS.
               15 WRK-OUT-DESSUBP-EETM  PIC  X(012)        VALUE SPACES.
             10 WRK-OUT-OPERAC-EETM     PIC  9(013)        VALUE ZEROS.
             10 WRK-OUT-SITUAC-EETM     PIC  X(015)        VALUE SPACES.
             10 WRK-OUT-SIT1-EETM       PIC  X(013)        VALUE SPACES.
             10 WRK-OUT-SIT2-EETM.
               15 WRK-OUT-SIT2-EETM-N   PIC  9(009)        VALUE ZEROS.
             10 WRK-OUT-SIT3-EETM       PIC  X(013)        VALUE SPACES.
             10 WRK-OUT-SIT4-EETM.
               15 WRK-OUT-SIT4-EETM-N   PIC  9(009)        VALUE ZEROS.
             10 WRK-OUT-CARTCONS-EETM-X.
               15  WRK-OUT-CARTCONS-EETM    PIC X(03)      VALUE SPACES.
             10 WRK-OUT-NUM1CONS-EETM-X.
               15  WRK-OUT-NUM1CONS-EETM    PIC 9(04)      VALUE ZEROS.
             10 WRK-OUT-NUM2CONS-EETM-X.
               15  WRK-OUT-NUM2CONS-EETM    PIC 9(05)      VALUE ZEROS.
             10 WRK-OUT-NUM3CONS-EETM-X.
               15  WRK-OUT-NUM3CONS-EETM    PIC 9(09)      VALUE ZEROS.
             10 WRK-OUT-VLOPER-EETM.
               15 WRK-OUT-VLOPER-EETM-N PIC  ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
             10 WRK-OUT-TPTAXA-EETM     PIC  X(015)        VALUE SPACES.
             10 WRK-OUT-VLTAROP-EETM.
               15 WRK-OUT-VLTAROP-EETM-N PIC ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
             10 WRK-OUT-TXJURO-EETM.
               15 WRK-OUT-TXJURO-EETM-N PIC  ZZ9,99999     VALUE ZEROS.
             10 WRK-OUT-DEJURO-EETM     PIC  X(004)        VALUE SPACES.
             10 WRK-OUT-VLTARTI-EETM.
               15 WRK-OUT-VLTARTI-EETM-N PIC ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
             10 WRK-OUT-DTVENC-EETM     PIC  X(010)        VALUE SPACES.
             10 WRK-OUT-VLJURO-EETM.
               15 WRK-OUT-VLJURO-EETM-N PIC  ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
             10 WRK-OUT-PRZMED-EETM.
               15 WRK-OUT-PRZMED-EETM-N PIC  9(003)        VALUE ZEROS.
             10 WRK-OUT-VLIOF-EETM.
               15 WRK-OUT-VLIOF-EETM-N  PIC  ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
             10 WRK-OUT-QTDTIT-EETM.
               15 WRK-OUT-QTDTIT-EETM-N PIC  9(006)        VALUE ZEROS.
             10 WRK-OUT-VLLIQU-EETM.
               15 WRK-OUT-VLLIQU-EETM-N PIC  ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
             10 WRK-OUT-GARANT-EETM     PIC  X(020)        VALUE SPACES.
             10 WRK-OUT-VLLIBE-EETM.
               15 WRK-OUT-VLLIBE-EETM-N PIC  ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
             10 WRK-OUT-MEIENT-EETM     PIC  X(015)        VALUE SPACES.
             10 WRK-OUT-DTOPER-EETM     PIC  X(010)        VALUE SPACES.
             10 WRK-OUT-MENSA-EETM      PIC  X(079)        VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA TELA DCOMEETM - BRAD0660 ***'.
      *----------------------------------------------------------------*

       01  WRK-660-DCOMEETM.
           05  WRK-660-LL-AREA-EETM    PIC  9(004) COMP    VALUE 0000.
           05  WRK-660-LL-MENSA-EETM   PIC  9(004) COMP    VALUE 0000.
           05  WRK-660-SENHAS-EETM     PIC  9(004) COMP    VALUE 0037.
           05  WRK-660-COMANDO-EETM    PIC  9(004) COMP    VALUE 2070.
           05  WRK-660-OCULTOS-EETM    PIC  9(004) COMP    VALUE 0079.
           05  WRK-660-NOME-EETM       PIC  9(004) COMP    VALUE 0040.
           05  WRK-660-CNPJCPF-EETM    PIC  9(004) COMP    VALUE 0019.
           05  WRK-660-BCAGCTA-EETM    PIC  9(004) COMP    VALUE 0023.
           05  WRK-660-CODPROD-EETM    PIC  9(004) COMP    VALUE 0003.
           05  WRK-660-DESPROD-EETM    PIC  9(004) COMP    VALUE 0012.
           05  WRK-660-CODSUBP-EETM    PIC  9(004) COMP    VALUE 0003.
           05  WRK-660-DESSUBP-EETM    PIC  9(004) COMP    VALUE 0012.
           05  WRK-660-OPERAC-EETM     PIC  9(004) COMP    VALUE 0013.
           05  WRK-660-SITUAC-EETM     PIC  9(004) COMP    VALUE 0015.
           05  WRK-660-SIT1-EETM       PIC  9(004) COMP    VALUE 0013.
           05  WRK-660-SIT2-EETM       PIC  9(004) COMP    VALUE 0009.
           05  WRK-660-SIT3-EETM       PIC  9(004) COMP    VALUE 0013.
           05  WRK-660-SIT4-EETM       PIC  9(004) COMP    VALUE 0009.
           05  WRK-660-CARTCONS-EETM   PIC  9(004) COMP    VALUE 0003.
           05  WRK-660-NUM1CONS-EETM   PIC  9(004) COMP    VALUE 0004.
           05  WRK-660-NUM2CONS-EETM   PIC  9(004) COMP    VALUE 0005.
           05  WRK-660-NUM3CONS-EETM   PIC  9(004) COMP    VALUE 0009.
           05  WRK-660-VLOPER-EETM     PIC  9(004) COMP    VALUE 0014.
           05  WRK-660-TPTAXA-EETM     PIC  9(004) COMP    VALUE 0015.
           05  WRK-660-VLTAROP-EETM    PIC  9(004) COMP    VALUE 0014.
           05  WRK-660-TXJURO-EETM     PIC  9(004) COMP    VALUE 0009.
           05  WRK-660-DEJURO-EETM     PIC  9(004) COMP    VALUE 0004.
           05  WRK-660-VLTARTI-EETM    PIC  9(004) COMP    VALUE 0014.
           05  WRK-660-DTVENC-EETM     PIC  9(004) COMP    VALUE 0010.
           05  WRK-660-VLJURO-EETM     PIC  9(004) COMP    VALUE 0014.
           05  WRK-660-PRZMED-EETM     PIC  9(004) COMP    VALUE 0003.
           05  WRK-660-VLIOF-EETM      PIC  9(004) COMP    VALUE 0014.
           05  WRK-660-QTDTIT-EETM     PIC  9(004) COMP    VALUE 0006.
           05  WRK-660-VLLIQU-EETM     PIC  9(004) COMP    VALUE 0014.
           05  WRK-660-GARANT-EETM     PIC  9(004) COMP    VALUE 0020.
           05  WRK-660-VLLIBE-EETM     PIC  9(004) COMP    VALUE 0014.
           05  WRK-660-MEIENT-EETM     PIC  9(004) COMP    VALUE 0015.
           05  WRK-660-DTOPER-EETM     PIC  9(004) COMP    VALUE 0010.
           05  WRK-660-MENSA-EETM      PIC  9(004) COMP    VALUE 0079.

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
       COPY I#DCOMSY.

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
           '*** DCOM0505 - FIM DA AREA DE WORKING ***'.
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

           MOVE SPACES                 TO WRK-OUT-DADOS-EETM.

           PERFORM 2000-PROCESSAR-MENSAGEM.

           GO TO 0000-INICIAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000-PROCESSAR-MENSAGEM         SECTION.
      *----------------------------------------------------------------*

           MOVE 'DCOMEETM'             TO WRK-TELA

           IF  WRK-TRANSACAO           NOT EQUAL 'DCOM0505'
               PERFORM 2100-RECEBER-CONTROLE
           ELSE
               PERFORM 2200-PROCESSAR-DCOMEETM
           END-IF.

           IF  WRK-TELA                EQUAL 'DCOMEETM'
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
PROC01                                    TO WRK-INP-CHAMADOR-EETM
PROC01                                       WRK-OUT-CHAMADOR-EETM
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
PROC01         MOVE WRK-TRANSACAO         TO WRK-INP-CHAMADOR-EETM
PROC01                                       WRK-OUT-CHAMADOR-EETM
PROC01     END-IF.

           MOVE WRK-COMU-TPOPCAO       TO WRK-OUT-TIPOOPC-EETM
                                          WRK-INP-TIPOOPC-EETM.
           MOVE WRK-COMU-OPERACAO      TO WRK-INP-OPERAC-EETM
                                          WRK-OUT-OPERAC-EETM.
           MOVE WRK-COMU-BANCO         TO WRK-INP-BANCO-EETM
           MOVE WRK-COMU-TMSTAMP       TO WRK-OUT-TMSTAMP-EETM
                                          WRK-INP-TMSTAMP-EETM.

           IF  WRK-TRANSACAO            EQUAL 'DCOM0502'
               MOVE 1                      TO WRK-INP-TPPESQ-EETM
                                              WRK-OUT-TPPESQ-EETM
           ELSE
               MOVE WRK-COMU-PAG           TO WRK-INP-PAGINA-EETM
                                              WRK-OUT-PAGINA-EETM
               MOVE WRK-COMU-OCORR         TO WRK-INP-OCORR-EETM
                                              WRK-OUT-OCORR-EETM
               IF  WRK-TRANSACAO         EQUAL 'DCOM0503'
                   MOVE 2                  TO WRK-INP-TPPESQ-EETM
                                              WRK-OUT-TPPESQ-EETM
               ELSE
                   IF  WRK-COMU-CCPFCNPJ       IS NUMERIC AND
                       WRK-COMU-CCPFCNPJ          GREATER ZEROS
                       MOVE 3                  TO WRK-INP-TPPESQ-EETM
                                                  WRK-OUT-TPPESQ-EETM
                   ELSE
                       MOVE 2                  TO WRK-INP-TPPESQ-EETM
                                                  WRK-OUT-TPPESQ-EETM
                   END-IF
               END-IF
           END-IF.

           PERFORM 2110-ACESSAR-DCOM4174

           IF  WRK-INCONSIS             EQUAL '*'
               GO TO 2100-99-FIM
           END-IF

           PERFORM 2111-MONTAR-TELA

           PERFORM 1111-ACESSAR-DCOM4266

           IF  WRK-INCONSIS             NOT EQUAL '*'
               MOVE SG-MENSAGEM(110)    TO  WRK-OUT-MENSA-EETM
               MOVE '1'                 TO  WRK-INP-FASE-EETM
                                            WRK-OUT-FASE-EETM
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
           MOVE WRK-INP-DANO-EETM      TO 4174-ENT-DANO-OPER-DESC
           MOVE WRK-INP-NSEQ-EETM      TO 4174-ENT-NSEQ-OPER-DESC.
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
                   MOVE 4174-SAI-MSG-RETORNO     TO WRK-OUT-MENSA-EETM
                   MOVE '*'                      TO WRK-INCONSIS
           END-EVALUATE.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2111-MONTAR-TELA                SECTION.
      *----------------------------------------------------------------*

           MOVE 4174-SAI-DANO-BASE            TO WRK-OUT-NUM1CONS-EETM
           MOVE 4174-SAI-CDEPDC               TO WRK-OUT-NUM2CONS-EETM
           MOVE 4174-SAI-CSEQ-STUDO           TO WRK-OUT-NUM3CONS-EETM
           MOVE 4174-SAI-CCART-LIM-CREDT      TO WRK-OUT-CARTCONS-EETM

           MOVE 4174-SAI-IPSSOA-DESC-COML     TO WRK-OUT-NOME-EETM.
           MOVE 4174-SAI-CCNPJ-CPF            TO WRK-INP-CNPJOC-EETM
                                                 WRK-OUT-CNPJOC-EETM.

           STRING 4174-SAI-CCNPJ-CPF(1:3) '.'
                  4174-SAI-CCNPJ-CPF(4:3) '.'
                  4174-SAI-CCNPJ-CPF(7:3) '/'
                  4174-SAI-CFLIAL-CNPJ    '-'
                  4174-SAI-CCTRL-CNPJ-CPF
           DELIMITED BY SIZE                INTO WRK-OUT-CNPJCPF-EETM.

           STRING 4174-SAI-CBCO           '/'
                 4174-SAI-CAG-BCRIA       '/'
                 4174-SAI-CCTA-BCRIA-CLI
           DELIMITED BY SIZE                INTO WRK-OUT-BCAGCTA-EETM.

           MOVE 4174-SAI-CPRODT               TO WRK-OUT-CODPROD-EETM.
           MOVE 4174-SAI-IABREV-PRODT         TO WRK-OUT-DESPROD-EETM.
           MOVE 4174-SAI-CSPROD-DESC-COML     TO WRK-OUT-CODSUBP-EETM.
           MOVE 4174-SAI-IRSUMO-SPROD-DESC    TO WRK-OUT-DESSUBP-EETM.
           MOVE 4174-SAI-IRSUMO-SIT-DESC      TO WRK-OUT-SITUAC-EETM.

           IF 4174-SAI-CCONTR-LIM-DESC        GREATER ZEROS
              MOVE 'CONTR LIMITE:'            TO WRK-OUT-SIT1-EETM
              MOVE 4174-SAI-CCONTR-LIM-DESC   TO WRK-OUT-SIT2-EETM-N
           ELSE
              IF 4174-SAI-CCONTR-CONVE-DESC   GREATER ZEROS AND
                (4174-SAI-CELMTO-DESC-COML    EQUAL 8 OR 9 )
                 IF  4174-SAI-CELMTO-DESC-COML   EQUAL 8
                     MOVE 'CONV GERAL..:'     TO WRK-OUT-SIT1-EETM
                     MOVE 4174-SAI-CCONTR-CONVE-DESC
                                              TO WRK-OUT-SIT2-EETM-N
                 ELSE
                     MOVE 'CONV CLIENTE:'     TO WRK-OUT-SIT1-EETM
                     MOVE 4174-SAI-CCONTR-CONVE-DESC
                                              TO WRK-OUT-SIT2-EETM-N
                 END-IF
                 PERFORM 2112-VERIFICA-CONVENIO
              ELSE
                IF 4174-SAI-NSMULA-OPER-DESC  GREATER  ZEROS
                   MOVE 'SIMULACAO...:'       TO WRK-OUT-SIT3-EETM
                   MOVE 4174-SAI-NSMULA-OPER-DESC
                                              TO WRK-OUT-SIT4-EETM-N
                END-IF
             END-IF
           END-IF.

           MOVE 4174-SAI-VOPER-DESC-APURA    TO WRK-OUT-VLOPER-EETM-N.
           MOVE 4174-SAI-VTAC-OPER-DESC      TO WRK-OUT-VLTAROP-EETM-N

           MOVE 4174-SAI-VTARIF-REG-TITLO    TO WRK-OUT-VLTARTI-EETM-N.
           MOVE 4174-SAI-DVCTO-FNAL-OPER     TO WRK-OUT-DTVENC-EETM.

           INSPECT WRK-OUT-DTVENC-EETM       REPLACING ALL '.' BY '/'.

           MOVE 4174-SAI-VJURO-OPER-DESC     TO WRK-OUT-VLJURO-EETM-N.
           MOVE 4174-SAI-TMED-PONDE-OPER     TO WRK-OUT-PRZMED-EETM-N.
           MOVE 4174-SAI-VIOF-OPER-DESC      TO WRK-OUT-VLIOF-EETM-N.
           MOVE 4174-SAI-QTITLO-CHEQ-PCELA   TO WRK-OUT-QTDTIT-EETM-N.
           MOVE 4174-SAI-VLIQ-OPER-DESC      TO WRK-OUT-VLLIQU-EETM-N.
           MOVE 4174-SAI-ITPO-GARNT          TO WRK-OUT-GARANT-EETM.

           IF  4174-SAI-VLIBRC-DESC-COML     EQUAL ZEROS
               MOVE 4174-SAI-VLIBRC-ANTCP-OPER  TO WRK-OUT-VLLIBE-EETM-N
           ELSE
               MOVE 4174-SAI-VLIBRC-DESC-COML   TO WRK-OUT-VLLIBE-EETM-N
           END-IF.

           MOVE 4174-SAI-IRSUMO-MEIO-ENTRD   TO WRK-OUT-MEIENT-EETM.
           MOVE 4174-SAI-DINIC-OPER-DESC     TO WRK-OUT-DTOPER-EETM.

           INSPECT WRK-OUT-DTOPER-EETM       REPLACING ALL '.' BY '/'.

      *----------------------------------------------------------------*
       2111-99-FIM.                    EXIT.
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
           MOVE  WRK-INP-DANO-EETM      TO  4266E-DANO-OPER-DESC.
           MOVE  WRK-INP-NSEQ-EETM      TO  4266E-NSEQ-OPER-DESC.
           MOVE  4174-SAI-CCNPJ-CPF     (1:3)
                                        TO  4266E-CCNPJ-CPF(1:3).
           MOVE  4174-SAI-CCNPJ-CPF     (4:3)
                                        TO  4266E-CCNPJ-CPF(4:3).
           MOVE  4174-SAI-CCNPJ-CPF     (7:3)
                                        TO  4266E-CCNPJ-CPF(7:3).
           MOVE  4174-SAI-CFLIAL-CNPJ
                                        TO  4266E-CFLIAL-CNPJ.

           CALL  WRK-MODULO          USING  4266E-ENTRADA
                                            4266S-SAIDA
                                            ERRO-AREA
                                            WRK-SQLCA.

           EVALUATE 4266S-COD-RETORNO

              WHEN '0000'

                   MOVE 4266S-IRSUMO-TPO-TX-JURO    TO
                        WRK-OUT-TPTAXA-EETM
                   IF  4266S-CINDCD-FREQ-TX  EQUAL 'AA'
                       MOVE 'A.A.'             TO WRK-OUT-DEJURO-EETM
                       MOVE 4174-SAI-PTX-JURO-ANO
                                               TO WRK-OUT-TXJURO-EETM-N
                   ELSE
                       MOVE 'A.M.'             TO WRK-OUT-DEJURO-EETM
                       MOVE 4174-SAI-PTX-JURO-MES
                                               TO WRK-OUT-TXJURO-EETM-N
                   END-IF

              WHEN '0099'
                    MOVE 'DB2'         TO ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO

              WHEN  OTHER
                    MOVE 'A.M.'                TO WRK-OUT-DEJURO-EETM
                    MOVE 4174-SAI-PTX-JURO-MES
                                               TO WRK-OUT-TXJURO-EETM-N
           END-EVALUATE.

      *----------------------------------------------------------------*
       1111-99-FIM.                    EXIT.
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
                        MOVE 'CONV GERAL..:'   TO WRK-OUT-SIT1-EETM
                        MOVE 4174-SAI-CCONTR-CONVE-DESC
                                                TO WRK-OUT-SIT2-EETM-N
                    ELSE
                        MOVE 'CONV CLIENTE:'    TO WRK-OUT-SIT1-EETM
                        MOVE 4174-SAI-CCONTR-CONVE-DESC
                                                TO WRK-OUT-SIT2-EETM-N
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
                                      TO WRK-OUT-MENSA-EETM
           END-EVALUATE.

      *----------------------------------------------------------------*
       2112-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA OBTEM A PERIODICIDADE DA TAXA                     *
      *----------------------------------------------------------------*
       2200-PROCESSAR-DCOMEETM         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM             TO WRK-INPUT-EETM.

           PERFORM 2215-DEVOLVER-TELA

           EVALUATE TRUE
               WHEN WRK-INP-PFK-EETM     EQUAL 'H' OR
                   (WRK-INP-PFK-EETM     EQUAL '.' AND
                    WRK-INP-COMANDO-EETM EQUAL 'PFK01')
                    MOVE WRK-INP-MENSA-EETM     TO WRK-OUT-MENSA-EETM

               WHEN WRK-INP-PFK-EETM     EQUAL '3' OR
                   (WRK-INP-PFK-EETM     EQUAL '.' AND
                    WRK-INP-COMANDO-EETM EQUAL 'PFK03')
                    PERFORM 2220-RETORNAR-CHAMADOR

               WHEN WRK-INP-PFK-EETM     EQUAL '5' OR
                   (WRK-INP-PFK-EETM     EQUAL '.' AND
                    WRK-INP-COMANDO-EETM EQUAL 'PFK05')
                    PERFORM 2230-RETORNAR-MENU-ROTINAS

               WHEN WRK-INP-PFK-EETM     EQUAL 'A' OR
                   (WRK-INP-PFK-EETM     EQUAL '.' AND
                    WRK-INP-COMANDO-EETM EQUAL 'PFK10')
                    PERFORM 2240-RETORNAR-MENU-DCOM

               WHEN (WRK-INP-PFK-EETM     EQUAL '6'     AND
                     WRK-INP-FASE-EETM    EQUAL  1  )
                                                            OR
                    (WRK-INP-PFK-EETM     EQUAL '.'     AND
                     WRK-INP-COMANDO-EETM EQUAL 'PFK06' AND
                     WRK-INP-FASE-EETM    EQUAL  1  )
CAP001               PERFORM 2270-NAUTZ-ORPAGS-4734
.                    IF  WRK-INCONSIS     EQUAL SPACES
.                        PERFORM 2245-ACESSAR-DCOM5335
CAP001               END-IF
                     IF  WRK-INCONSIS     EQUAL SPACES
                         PERFORM 2250-TRATAR-CONFIRMACAO
                     END-IF

               WHEN (WRK-INP-PFK-EETM     EQUAL '.' AND
                     WRK-INP-COMANDO-EETM EQUAL SPACES)
                     PERFORM 2255-TRATAR-ENTER

PROC01         WHEN (WRK-INP-PFK-EETM     EQUAL  'B' OR
PROC01              (WRK-INP-PFK-EETM     EQUAL  '.'  AND
PROC01               WRK-INP-COMANDO-EETM EQUAL  'PFK11')) AND
PROC01               WRK-INP-FASE-EETM    EQUAL  1
PROC01              PERFORM 2260-ACESSAR-CET
PROC01
               WHEN  OTHER
                     MOVE SG-MENSAGEM(38)       TO WRK-OUT-MENSA-EETM
           END-EVALUATE.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2215-DEVOLVER-TELA              SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-TPPESQ-EETM     TO WRK-OUT-TPPESQ-EETM.
           MOVE WRK-INP-OCULTOS-EETM    TO WRK-OUT-OCULTOS-EETM.
           MOVE WRK-INP-NOME-EETM       TO WRK-OUT-NOME-EETM.
           MOVE WRK-INP-CNPJCPF-EETM    TO WRK-OUT-CNPJCPF-EETM.
           MOVE WRK-INP-BCAGCTA-EETM    TO WRK-OUT-BCAGCTA-EETM.
           MOVE WRK-INP-PRODUTO-EETM    TO WRK-OUT-PRODUTO-EETM.
           MOVE WRK-INP-SUBPRODUTO-EETM TO WRK-OUT-SUBPRODUTO-EETM.
           MOVE WRK-INP-OPERAC-EETM     TO WRK-OUT-OPERAC-EETM.
           MOVE WRK-INP-SITUAC-EETM     TO WRK-OUT-SITUAC-EETM.
           MOVE WRK-INP-SIT1-EETM       TO WRK-OUT-SIT1-EETM.
           MOVE WRK-INP-SIT2-EETM       TO WRK-OUT-SIT2-EETM.
           MOVE WRK-INP-SIT3-EETM       TO WRK-OUT-SIT3-EETM.
           MOVE WRK-INP-SIT4-EETM       TO WRK-OUT-SIT4-EETM.
           MOVE WRK-INP-NUM1CONS-EETM   TO WRK-OUT-NUM1CONS-EETM
           MOVE WRK-INP-NUM2CONS-EETM   TO WRK-OUT-NUM2CONS-EETM
           MOVE WRK-INP-NUM3CONS-EETM   TO WRK-OUT-NUM3CONS-EETM
           MOVE WRK-INP-CARTCONS-EETM   TO WRK-OUT-CARTCONS-EETM
           MOVE WRK-INP-VLOPER-EETM     TO WRK-OUT-VLOPER-EETM.
           MOVE WRK-INP-TPTAXA-EETM     TO WRK-OUT-TPTAXA-EETM.
           MOVE WRK-INP-VLTAROP-EETM    TO WRK-OUT-VLTAROP-EETM.
           MOVE WRK-INP-TXJURO-EETM     TO WRK-OUT-TXJURO-EETM.
           MOVE WRK-INP-DEJURO-EETM     TO WRK-OUT-DEJURO-EETM.
           MOVE WRK-INP-VLTARTI-EETM    TO WRK-OUT-VLTARTI-EETM.
           MOVE WRK-INP-DTVENC-EETM     TO WRK-OUT-DTVENC-EETM.
           MOVE WRK-INP-VLJURO-EETM     TO WRK-OUT-VLJURO-EETM.
           MOVE WRK-INP-PRZMED-EETM     TO WRK-OUT-PRZMED-EETM.
           MOVE WRK-INP-VLIOF-EETM      TO WRK-OUT-VLIOF-EETM.
           MOVE WRK-INP-QTDTIT-EETM     TO WRK-OUT-QTDTIT-EETM.
           MOVE WRK-INP-VLLIQU-EETM     TO WRK-OUT-VLLIQU-EETM.
           MOVE WRK-INP-GARANT-EETM     TO WRK-OUT-GARANT-EETM.
           MOVE WRK-INP-VLLIBE-EETM     TO WRK-OUT-VLLIBE-EETM.
           MOVE WRK-INP-MEIENT-EETM     TO WRK-OUT-MEIENT-EETM.
           MOVE WRK-INP-DTOPER-EETM     TO WRK-OUT-DTOPER-EETM.

      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2220-RETORNAR-CHAMADOR          SECTION.
      *----------------------------------------------------------------*

           PERFORM 2225-FORMATAR-COMU-AREA.

           MOVE WRK-INP-CHAMADOR-EETM  TO WRK-TELA.
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

           MOVE WRK-INP-TIPOOPC-EETM   TO WRK-COMU-TPOPCAO
           MOVE WRK-INP-TMSTAMP-EETM   TO WRK-COMU-TMSTAMP.
           MOVE WRK-INP-OPERAC-EETM    TO WRK-COMU-OPERACAO.
           MOVE WRK-INP-BANCO-EETM     TO WRK-COMU-BANCO.
           MOVE WRK-INP-PAGINA-EETM    TO WRK-COMU-PAG
           MOVE WRK-INP-OCORR-EETM     TO WRK-COMU-OCORR
           MOVE 'DCOM0505'             TO WRK-COMU-TRANSACAO.

           EVALUATE WRK-INP-TPPESQ-EETM
               WHEN 2
               WHEN 3
                    IF  WRK-INP-TPPESQ-EETM     EQUAL 2
                        MOVE WRK-INP-AGENCIA-EETM
                                                TO WRK-COMU-AGENCIA
                        MOVE WRK-INP-CONTA-EETM TO WRK-COMU-CONTA
                    ELSE
                        MOVE WRK-INP-CNPJOC-EETM
                                              TO WRK-COMU-CCPFCNPJ
                        MOVE WRK-INP-FILIAL-EETM
                                              TO WRK-COMU-CFLIAL
                        MOVE WRK-INP-CONTROLE-EETM
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
           MOVE 'DCOM0505'             TO WRK-TELA
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
           MOVE 'DCOM0505'             TO WRK-TELA
                                          WRK-COMU-TRANSACAO.
           MOVE WRK-COMU-AREA          TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2240-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2245-ACESSAR-DCOM5335           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE 5335-ENTRADA
                      5335-SAIDA.

           MOVE WRK-INP-OPERAC-EETM    TO 5335E-OPERACAO.
           MOVE 006                    TO 5335E-CELMTO-DESC-COML.
           MOVE 009                    TO 5335E-CEVNTO-DESC-COML.
           MOVE 'I'                    TO 5335E-ACAO.
           MOVE 'DCOM5335'             TO WRK-MODULO.

           CALL WRK-MODULO             USING 5335-ENTRADA
                                             5335-SAIDA
                                             ERRO-AREA
                                             WRK-SQLCA.

           EVALUATE  5335S-COD-RETORNO
               WHEN '0000'
                     CONTINUE

               WHEN '0099'
                     MOVE 'DB2'        TO ERR-TIPO-ACESSO
                     PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN  OTHER
                     MOVE 5335S-MSG-RETORNO
                                       TO WRK-OUT-MENSA-EETM
                     MOVE '*'          TO WRK-INCONSIS
           END-EVALUATE.

      *----------------------------------------------------------------*
       2245-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2250-TRATAR-CONFIRMACAO         SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO WRK-SQLCA ERRO-AREA.

           PERFORM 2110-ACESSAR-DCOM4174

           INITIALIZE 5420-ENTRADA
                      5420-SAIDA
                      ERRO-AREA.

           MOVE WRK-COD-USER             TO  WRK-5420E-CFUNC-BDSCO.
           MOVE LNK-IO-LTERM             TO  WRK-5420E-CTERM.
           MOVE WRK-INP-DANO-EETM        TO  WRK-5420E-DANO-OPER-DESC
           MOVE WRK-INP-NSEQ-EETM        TO  WRK-5420E-NSEQ-OPER-DESC
           MOVE 'DCOM5420'             TO WRK-MODULO.

           CALL WRK-MODULO             USING 5420-ENTRADA
                                             5420-SAIDA
                                             ERRO-AREA
                                             WRK-SQLCA.

           EVALUATE WRK-5420S-COD-RETORNO
               WHEN '0000'
                     PERFORM 2220-RETORNAR-CHAMADOR
                     MOVE WRK-MSG004       TO WRK-COMU-MENSA
                     MOVE 'S'              TO WRK-COMU-ACAO
                     MOVE WRK-COMU-AREA    TO WRK-MENSAGEM

               WHEN '0099'
                     MOVE 'DB2'        TO ERR-TIPO-ACESSO
                     PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN '7777'
                     MOVE 'APL'        TO ERR-TIPO-ACESSO
                     PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN  OTHER
                     PERFORM 8888-EFETUAR-ROLLBACK
                     MOVE WRK-5420S-MSG-RETORNO
                                       TO WRK-OUT-MENSA-EETM
           END-EVALUATE.

      *----------------------------------------------------------------*
       2250-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2255-TRATAR-ENTER               SECTION.
      *----------------------------------------------------------------*

           MOVE '1'               TO WRK-INP-FASE-EETM
                                     WRK-OUT-FASE-EETM
           MOVE SG-MENSAGEM(110)  TO WRK-OUT-MENSA-EETM.

      *----------------------------------------------------------------*
       2255-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

PROC01*----------------------------------------------------------------*
PROC01 2260-ACESSAR-CET                SECTION.
PROC01*----------------------------------------------------------------*
PROC01
PROC01     INITIALIZE                  WRK-COMU-AREA-3924.
PROC01
PROC01     MOVE LENGTH                 OF WRK-COMU-AREA-3924
PROC01                                 TO WRK-COMU-LL-3924.
PROC01
PROC01     MOVE WRK-INP-TIPOOPC-EETM   TO WRK-COMU-OPCAO-3924
PROC01     MOVE WRK-INP-TMSTAMP-EETM   TO WRK-COMU-TIMESTAMP-3924.
PROC01     MOVE WRK-INP-OPERAC-EETM    TO WRK-COMU-OPER-3924.
PROC01     MOVE WRK-INP-BANCO-EETM     TO WRK-COMU-BANCO-3924.
PROC01     MOVE WRK-INP-PAGINA-EETM    TO WRK-COMU-PAG-3924
PROC01     MOVE WRK-INP-OCORR-EETM     TO WRK-COMU-OCORR-3924
PROC01     MOVE 'DCOM0507'             TO WRK-COMU-TRANSACAO-3924.
PROC01
PROC01     EVALUATE WRK-INP-TPPESQ-EETM
PROC01         WHEN 2
PROC01         WHEN 3
PROC01              IF  WRK-INP-TPPESQ-EETM     EQUAL 2
PROC01                  MOVE WRK-INP-AGENCIA-EETM
PROC01                                          TO WRK-COMU-AGENC-3924
PROC01                  MOVE WRK-INP-CONTA-EETM TO WRK-COMU-CONTA-3924
PROC01              ELSE
PROC01                  MOVE WRK-INP-CNPJOC-EETM
PROC01                                        TO WRK-COMU-CNPJCPF-3924
PROC01                  MOVE WRK-INP-FILIAL-EETM
PROC01                                        TO WRK-COMU-FILIAL-3924
PROC01                  MOVE WRK-INP-CONTROLE-EETM
PROC01                                        TO WRK-COMU-CONTROL-3924
PROC01              END-IF
PROC01     END-EVALUATE.
PROC01
PROC01     MOVE 'DCOM3924'             TO WRK-TELA.
PROC01     MOVE WRK-INP-CHAMADOR-EETM  TO WRK-COMU-CHAMADOR-3924.
PROC01
PROC01     MOVE WRK-CHNG               TO WRK-FUNCAO.
PROC01     MOVE WRK-COMU-AREA-3924     TO WRK-MENSAGEM.
PROC01
PROC01*----------------------------------------------------------------*
PROC01 2260-99-FIM.                    EXIT.
PROC01*----------------------------------------------------------------*

      *----------------------------------------------------------------*
CAP001 2270-NAUTZ-ORPAGS-4734          SECTION.
.     *----------------------------------------------------------------*
.
.          PERFORM 2110-ACESSAR-DCOM4174
.
.          IF (4174-SAI-CTPO-DESC-COML EQUAL 4) AND
.             (WRK-INP-TMSTAMP-EETM(1:10)
.                                      EQUAL '2021-06-03'
.                                         OR '2021-06-04'
.                                         OR '2021-06-05'
.                                         OR '2021-06-06')
.              MOVE 'NAO PERMITIDO ESTORNO DE ORPAGS ENTRE OS DIAS 03 E
.     -             '06/06/2021 - NORMAT.4734'
.                                      TO WRK-OUT-MENSA-EETM
.              MOVE '*'                TO WRK-INCONSIS
.          END-IF.
.
.     *----------------------------------------------------------------*
CAP001 2270-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2300-PROCESSAR-BRAD0660         SECTION.
      *----------------------------------------------------------------*

            MOVE LENGTH                OF  WRK-OUTPUT-EETM
                                       TO  WRK-660-LL-MENSA-EETM
                                           WRK-OUT-LL-EETM
            MOVE LENGTH                OF  WRK-660-DCOMEETM
                                       TO  WRK-660-LL-AREA-EETM

           CALL 'BRAD0660'             USING WRK-OUTPUT-EETM
                                             WRK-660-DCOMEETM.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE RETURN-CODE        TO WRK-RETURN-CODE
               MOVE 10                 TO WRK-LOCAL-ERRO
               MOVE WRK-ERRO-BRAD0660  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-ISRT               TO WRK-FUNCAO.
           MOVE WRK-OUTPUT-EETM        TO WRK-MENSAGEM.

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
               MOVE 'DCOM0505'         TO ERR-PGM
           ELSE
               MOVE 'SENHAS02'         TO ERR-PGM
               MOVE 'DCOM0505'         TO ERR-MODULO
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

      *----------------------------------------------------------------*
       8888-EFETUAR-ROLLBACK           SECTION.
      *----------------------------------------------------------------*

            CALL 'CBLTDLI'             USING WRK-ROLB
                                             LNK-IO-PCB.

      *----------------------------------------------------------------*
       8888-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
