      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID.     DCOM0506.
       AUTHOR.         FABRICA - ALTRAN.
      *================================================================*
      *                          A L T R A N                           *
      *----------------------------------------------------------------*
      *     PROGRAMA....: DCOM0506                                     *
      *     PROGRAMADOR.: FABRICA - ALTRAN.                            *
      *     DATA........: 03/08/2007                                   *
      *----------------------------------------------------------------*
      *     OBJETIVO....: EXCLUSAO DO ESTORNO DA OPERACAO              *
      *                                                                *
      *----------------------------------------------------------------*
      *     TELAS.......:                                              *
      *     DCOMEFTM....: EXCLUSAO   DO ESTORNO DA OPERACAO            *
      *----------------------------------------------------------------*
      *     INC'S.......:                                              *
      *     I#BRAD7C - AREA DE ERRO DA BRAD7100.                       *
      *     I#DCOMXJ - AREA DE COMUNICACAO DO MODULO DCOM4174.         *
      *----------------------------------------------------------------*
      *     MODULOS.....:                                              *
      *     BRAD0660 - INSERIR CARACTERES DE NULL NA MENSAGEM DE SAIDA.*
      *     BRAD7100 - TRATAMENTO DE ERROS QUANDO PROGRAMA INVALIDO.   *
      *     DCOM4174 - OBTER DADOS DA OPERACAO.                        *
      *     DCOM5421 - EXCLUIR ESTORNO DA OPERACAO                     *
      *     DCOM6437 - VALIDA PERFIL                                   *
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
           '*** DCOM0506 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** MENSAGENS ***'.
      *----------------------------------------------------------------*

       77  WRK-MSG002                  PIC  X(079)         VALUE
           'PF INVALIDA'.
       77  WRK-MSG003                  PIC  X(079)         VALUE
           'TECLE <PF6> PARA CONFIRMAR '.
       77  WRK-MSG004                  PIC  X(079)         VALUE
           'ESTORNO DA OPERACAO EXCLUIDO COM SUCESSO'.

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
FS2511*        10  WRK-COMU-CCPFCNPJ       PIC  9(009)    VALUE ZEROS.
FS2511         10  WRK-COMU-CCPFCNPJ       PIC  X(009)    VALUE SPACES.
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

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREAS AUXILIARES PARA POOL5005 ***'.
      *----------------------------------------------------------------*

       01  WRK-POOL5005.
           05  WRK-GU                  PIC  X(004)         VALUE 'GU  '.
           05  WRK-CHNG                PIC  X(004)         VALUE 'CHNG'.
           05  WRK-ROLB                PIC  X(004)         VALUE 'ROLB'.
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
           '*** AREA DA TELA DCOMEFTM - INPUT ***'.
      *----------------------------------------------------------------*

       01  WRK-INPUT-EFTM.
           05 FILLER                   PIC  X(018)         VALUE SPACES.
           05 WRK-INP-PFK-EFTM         PIC  X(001)         VALUE SPACES.
           05 WRK-INP-DADOS-EFTM.
             10 WRK-INP-SENHAS-EFTM    PIC  X(037)         VALUE SPACES.
             10 WRK-INP-COMANDO-EFTM   PIC  X(068)         VALUE SPACES.
             10 WRK-INP-OCULTOS-EFTM.
               15 WRK-INP-TIPOOPC-EFTM PIC  X(001)         VALUE SPACES.
               15 WRK-INP-TPPESQ-EFTM  PIC  9(001)         VALUE ZEROS.
               15 WRK-INP-TMSTAMP-EFTM PIC  X(026)         VALUE SPACES.
               15 WRK-INP-CHAMADOR-EFTM PIC X(008)         VALUE SPACES.
               15 WRK-INP-PAGINA-EFTM  PIC  9(004)         VALUE ZEROS.
               15 WRK-INP-OCORR-EFTM   PIC  9(002)         VALUE ZEROS.
               15 WRK-INP-CNPJOC-EFTM  PIC  9(009)         VALUE ZEROS.
               15 WRK-INP-FASE-EFTM    PIC  9(001)         VALUE ZEROS.
               15 FILLER               PIC  X(027)         VALUE SPACES.
             10 WRK-INP-NOME-EFTM      PIC  X(040)         VALUE SPACES.
             10 WRK-INP-CNPJCPF-EFTM.
FS2511*        15 WRK-INP-CCNPJCPF-EFTM PIC  ZZZ.ZZZ.ZZ9   VALUE ZEROS.
FS2511         15 WRK-INP-CCNPJCPF-EFTM PIC  X(11)         VALUE SPACES.
               15 FILLER                PIC  X(001)        VALUE SPACES.
FS2511*        15 WRK-INP-FILIAL-EFTM   PIC  9(004)        VALUE ZEROS.
FS2511         15 WRK-INP-FILIAL-EFTM   PIC  X(04)         VALUE SPACES.
               15 FILLER                PIC  X(001)        VALUE SPACES.
               15 WRK-INP-CONTROLE-EFTM PIC  9(002)        VALUE ZEROS.
             10 WRK-INP-BCAGCTA-EFTM.
               15 WRK-INP-BANCO-EFTM    PIC  9(003)        VALUE ZEROS.
               15 FILLER                PIC  X(001)        VALUE SPACES.
               15 WRK-INP-AGENCIA-EFTM  PIC  9(005)        VALUE ZEROS.
               15 FILLER                PIC  X(001)        VALUE SPACES.
               15 WRK-INP-CONTA-EFTM    PIC  9(013)        VALUE ZEROS.
             10 WRK-INP-PRODUTO-EFTM.
               15 WRK-INP-CODPROD-EFTM  PIC  9(003)        VALUE ZEROS.
               15 WRK-INP-DESPROD-EFTM  PIC  X(012)        VALUE SPACES.
             10 WRK-INP-SUBPRODUTO-EFTM.
               15 WRK-INP-CODSUBP-EFTM  PIC  9(003)        VALUE ZEROS.
               15 WRK-INP-DESSUBP-EFTM  PIC  X(012)        VALUE SPACES.
             10 WRK-INP-OPERAC-EFTM     PIC  9(013)        VALUE ZEROS.
             10 FILLER REDEFINES        WRK-INP-OPERAC-EFTM.
               15 WRK-INP-DANO-EFTM     PIC  9(004).
               15 WRK-INP-NSEQ-EFTM     PIC  9(009).
             10 WRK-INP-SITUAC-EFTM     PIC  X(015)        VALUE SPACES.
             10 WRK-INP-SIT1-EFTM       PIC  X(013)        VALUE SPACES.
             10 WRK-INP-SIT2-EFTM.
               15 WRK-INP-SIT2-EFTM-N   PIC  9(009)        VALUE ZEROS.
             10 WRK-INP-SIT3-EFTM       PIC  X(013)        VALUE SPACES.
             10 WRK-INP-SIT4-EFTM.
               15 WRK-INP-SIT4-EFTM-N   PIC  9(009)        VALUE ZEROS.
             10 WRK-INP-CARTCONS-EFTM   PIC  X(003)        VALUE SPACES.
             10 WRK-INP-NUM1CONS-EFTM   PIC  X(004)        VALUE SPACES.
             10 WRK-INP-NUM2CONS-EFTM   PIC  X(005)        VALUE SPACES.
             10 WRK-INP-NUM3CONS-EFTM   PIC  X(009)        VALUE SPACES.
             10 WRK-INP-VLOPER-EFTM     PIC  X(014)        VALUE SPACES.
             10 WRK-INP-TPTAXA-EFTM     PIC  X(015)        VALUE SPACES.
             10 WRK-INP-VLTAROP-EFTM    PIC  X(014)        VALUE SPACES.
             10 WRK-INP-TXJURO-EFTM     PIC  X(009)        VALUE SPACES.
             10 WRK-INP-DEJURO-EFTM     PIC  X(004)        VALUE SPACES.
             10 WRK-INP-VLTARTI-EFTM    PIC  X(014)        VALUE SPACES.
             10 WRK-INP-DTVENC-EFTM     PIC  X(010)        VALUE SPACES.
             10 WRK-INP-VLJURO-EFTM     PIC  X(014)        VALUE SPACES.
             10 WRK-INP-PRZMED-EFTM.
               15 WRK-INP-PRZMED-EFTM-N PIC  9(003)        VALUE ZEROS.
             10 WRK-INP-VLIOF-EFTM      PIC  X(014)        VALUE SPACES.
             10 WRK-INP-QTDTIT-EFTM     PIC  X(006)        VALUE SPACES.
             10 WRK-INP-VLLIQU-EFTM     PIC  X(014)        VALUE SPACES.
             10 WRK-INP-GARANT-EFTM     PIC  X(020)        VALUE SPACES.
             10 WRK-INP-VLLIBE-EFTM     PIC  X(014)        VALUE SPACES.
             10 WRK-INP-MEIENT-EFTM     PIC  X(015)        VALUE SPACES.
             10 WRK-INP-DTOPER-EFTM     PIC  X(010)        VALUE SPACES.
             10 WRK-INP-MENSA-EFTM      PIC  X(079)        VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA TELA DCOMEFTM  - OUTPUT ***'.
      *----------------------------------------------------------------*

       01  WRK-OUTPUT-EFTM.
           05 WRK-OUT-LL-EFTM           PIC S9(004) COMP   VALUE +000.
           05 WRK-OUT-ZZ-EFTM           PIC S9(004) COMP   VALUE ZEROS.
           05 WRK-OUT-DADOS-EFTM.
             10 WRK-OUT-SENHAS-EFTM     PIC  X(037)        VALUE SPACES.
             10 WRK-OUT-COMANDO-ATTR-EFTM
                                        PIC S9(004) COMP   VALUE ZEROS.
             10 WRK-OUT-COMANDO-EFTM    PIC  X(068)        VALUE SPACES.
             10 WRK-OUT-OCULTOS-EFTM.
               15 WRK-OUT-TIPOOPC-EFTM  PIC  X(001)        VALUE SPACES.
               15 WRK-OUT-TPPESQ-EFTM   PIC  9(001)        VALUE ZEROS.
               15 WRK-OUT-TMSTAMP-EFTM  PIC  X(026)        VALUE SPACES.
               15 WRK-OUT-CHAMADOR-EFTM PIC  X(008)        VALUE SPACES.
               15 WRK-OUT-PAGINA-EFTM   PIC  9(004)        VALUE ZEROS.
               15 WRK-OUT-OCORR-EFTM    PIC  9(002)        VALUE ZEROS.
               15 WRK-OUT-CNPJOC-EFTM   PIC  9(009)        VALUE ZEROS.
               15 WRK-OUT-FASE-EFTM     PIC  9(001)        VALUE ZEROS.
               15 FILLER                PIC  X(027)        VALUE SPACES.
             10 WRK-OUT-NOME-EFTM       PIC  X(040)        VALUE SPACES.
             10 WRK-OUT-CNPJCPF-EFTM    PIC  X(019)        VALUE SPACES.
             10 WRK-OUT-BCAGCTA-EFTM    PIC  X(023)        VALUE SPACES.
             10 WRK-OUT-PRODUTO-EFTM.
               15 WRK-OUT-CODPROD-EFTM  PIC  9(003)        VALUE ZEROS.
               15 WRK-OUT-DESPROD-EFTM  PIC  X(012)        VALUE SPACES.
             10 WRK-OUT-SUBPRODUTO-EFTM.
               15 WRK-OUT-CODSUBP-EFTM  PIC  9(003)        VALUE ZEROS.
               15 WRK-OUT-DESSUBP-EFTM  PIC  X(012)        VALUE SPACES.
             10 WRK-OUT-OPERAC-EFTM     PIC  9(013)        VALUE ZEROS.
             10 WRK-OUT-SITUAC-EFTM     PIC  X(015)        VALUE SPACES.
             10 WRK-OUT-SIT1-EFTM       PIC  X(013)        VALUE SPACES.
             10 WRK-OUT-SIT2-EFTM.
               15 WRK-OUT-SIT2-EFTM-N   PIC  9(009)        VALUE ZEROS.
             10 WRK-OUT-SIT3-EFTM       PIC  X(013)        VALUE SPACES.
             10 WRK-OUT-SIT4-EFTM.
               15 WRK-OUT-SIT4-EFTM-N   PIC  9(009)        VALUE ZEROS.
             10 WRK-OUT-CARTCONS-EFTM-X.
               15  WRK-OUT-CARTCONS-EFTM    PIC X(03)      VALUE SPACES.
             10 WRK-OUT-NUM1CONS-EFTM-X.
               15  WRK-OUT-NUM1CONS-EFTM    PIC 9(04)      VALUE ZEROS.
             10 WRK-OUT-NUM2CONS-EFTM-X.
               15  WRK-OUT-NUM2CONS-EFTM    PIC 9(05)      VALUE ZEROS.
             10 WRK-OUT-NUM3CONS-EFTM-X.
               15  WRK-OUT-NUM3CONS-EFTM    PIC 9(09)      VALUE ZEROS.
             10 WRK-OUT-VLOPER-EFTM.
               15 WRK-OUT-VLOPER-EFTM-N PIC  ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
             10 WRK-OUT-TPTAXA-EFTM     PIC  X(015)        VALUE SPACES.
             10 WRK-OUT-VLTAROP-EFTM.
               15 WRK-OUT-VLTAROP-EFTM-N PIC ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
             10 WRK-OUT-TXJURO-EFTM.
               15 WRK-OUT-TXJURO-EFTM-N PIC  ZZ9,99999     VALUE ZEROS.
             10 WRK-OUT-DEJURO-EFTM     PIC  X(004)        VALUE SPACES.
             10 WRK-OUT-VLTARTI-EFTM.
               15 WRK-OUT-VLTARTI-EFTM-N PIC ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
             10 WRK-OUT-DTVENC-EFTM     PIC  X(010)        VALUE SPACES.
             10 WRK-OUT-VLJURO-EFTM.
               15 WRK-OUT-VLJURO-EFTM-N PIC  ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
             10 WRK-OUT-PRZMED-EFTM.
               15 WRK-OUT-PRZMED-EFTM-N PIC  9(003)        VALUE ZEROS.
             10 WRK-OUT-VLIOF-EFTM.
               15 WRK-OUT-VLIOF-EFTM-N  PIC  ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
             10 WRK-OUT-QTDTIT-EFTM.
               15 WRK-OUT-QTDTIT-EFTM-N PIC  9(006)        VALUE ZEROS.
             10 WRK-OUT-VLLIQU-EFTM.
               15 WRK-OUT-VLLIQU-EFTM-N PIC  ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
             10 WRK-OUT-GARANT-EFTM     PIC  X(020)        VALUE SPACES.
             10 WRK-OUT-VLLIBE-EFTM.
               15 WRK-OUT-VLLIBE-EFTM-N PIC  ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
             10 WRK-OUT-MEIENT-EFTM     PIC  X(015)        VALUE SPACES.
             10 WRK-OUT-DTOPER-EFTM     PIC  X(010)        VALUE SPACES.
             10 WRK-OUT-MENSA-EFTM      PIC  X(079)        VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA TELA DCOMEFTM - BRAD0660 ***'.
      *----------------------------------------------------------------*

       01  WRK-660-DCOMEFTM.
           05  WRK-660-LL-AREA-EFTM    PIC  9(004) COMP    VALUE 0000.
           05  WRK-660-LL-MENSA-EFTM   PIC  9(004) COMP    VALUE 0000.
           05  WRK-660-SENHAS-EFTM     PIC  9(004) COMP    VALUE 0037.
           05  WRK-660-COMANDO-EFTM    PIC  9(004) COMP    VALUE 2070.
           05  WRK-660-OCULTOS-EFTM    PIC  9(004) COMP    VALUE 0079.
           05  WRK-660-NOME-EFTM       PIC  9(004) COMP    VALUE 0040.
           05  WRK-660-CNPJCPF-EFTM    PIC  9(004) COMP    VALUE 0019.
           05  WRK-660-BCAGCTA-EFTM    PIC  9(004) COMP    VALUE 0023.
           05  WRK-660-CODPROD-EFTM    PIC  9(004) COMP    VALUE 0003.
           05  WRK-660-DESPROD-EFTM    PIC  9(004) COMP    VALUE 0012.
           05  WRK-660-CODSUBP-EFTM    PIC  9(004) COMP    VALUE 0003.
           05  WRK-660-DESSUBP-EFTM    PIC  9(004) COMP    VALUE 0012.
           05  WRK-660-OPERAC-EFTM     PIC  9(004) COMP    VALUE 0013.
           05  WRK-660-SITUAC-EFTM     PIC  9(004) COMP    VALUE 0015.
           05  WRK-660-SIT1-EFTM       PIC  9(004) COMP    VALUE 0013.
           05  WRK-660-SIT2-EFTM       PIC  9(004) COMP    VALUE 0009.
           05  WRK-660-SIT3-EFTM       PIC  9(004) COMP    VALUE 0013.
           05  WRK-660-SIT4-EFTM       PIC  9(004) COMP    VALUE 0009.
           05  WRK-660-CARTCONS-EFTM   PIC  9(004) COMP    VALUE 0003.
           05  WRK-660-NUM1CONS-EFTM   PIC  9(004) COMP    VALUE 0004.
           05  WRK-660-NUM2CONS-EFTM   PIC  9(004) COMP    VALUE 0005.
           05  WRK-660-NUM3CONS-EFTM   PIC  9(004) COMP    VALUE 0009.
           05  WRK-660-VLOPER-EFTM     PIC  9(004) COMP    VALUE 0014.
           05  WRK-660-TPTAXA-EFTM     PIC  9(004) COMP    VALUE 0015.
           05  WRK-660-VLTAROP-EFTM    PIC  9(004) COMP    VALUE 0014.
           05  WRK-660-TXJURO-EFTM     PIC  9(004) COMP    VALUE 0009.
           05  WRK-660-DEJURO-EFTM     PIC  9(004) COMP    VALUE 0004.
           05  WRK-660-VLTARTI-EFTM    PIC  9(004) COMP    VALUE 0014.
           05  WRK-660-DTVENC-EFTM     PIC  9(004) COMP    VALUE 0010.
           05  WRK-660-VLJURO-EFTM     PIC  9(004) COMP    VALUE 0014.
           05  WRK-660-PRZMED-EFTM     PIC  9(004) COMP    VALUE 0003.
           05  WRK-660-VLIOF-EFTM      PIC  9(004) COMP    VALUE 0014.
           05  WRK-660-QTDTIT-EFTM     PIC  9(004) COMP    VALUE 0006.
           05  WRK-660-VLLIQU-EFTM     PIC  9(004) COMP    VALUE 0014.
           05  WRK-660-GARANT-EFTM     PIC  9(004) COMP    VALUE 0020.
           05  WRK-660-VLLIBE-EFTM     PIC  9(004) COMP    VALUE 0014.
           05  WRK-660-MEIENT-EFTM     PIC  9(004) COMP    VALUE 0015.
           05  WRK-660-DTOPER-EFTM     PIC  9(004) COMP    VALUE 0010.
           05  WRK-660-MENSA-EFTM      PIC  9(004) COMP    VALUE 0079.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DO DCOM5421                 ***'.
      *----------------------------------------------------------------*

       01  5421-ENTRADA.
           05  5421-ENT-HEADER.
               10   WRK-5421E-COD-RETORNO         PIC  X(004).
               10   WRK-5421E-MSG-RETORNO         PIC  X(079).
               10   WRK-5421E-RESTART             PIC  9(005).
               10   WRK-5421E-FLAG                PIC  X(001).
               10   WRK-5421E-CFUNC-BDSCO         PIC  9(009).
               10   WRK-5421E-CTERM               PIC  X(08).
           05  WRK-5421E-CONSISTENCIA.
               10   WRK-5421E-DANO-OPER-DESC      PIC  9(04).
               10   WRK-5421E-NSEQ-OPER-DESC      PIC  9(09).

       01  5421-SAIDA.
           05  5421-SAI-HEADER.
               10  WRK-5421S-COD-RETORNO     PIC  X(04).
               10  WRK-5421S-MSG-RETORNO     PIC  X(79).
               10  WRK-5421S-RESTART         PIC  9(05).
               10  WRK-5421S-FLAG            PIC  X(01).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** MENSAGEM DE ERRO IMS    ***'.
      *----------------------------------------------------------------*
       COPY 'I#DCOMSY'.

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
           '*** DCOM0506 - FIM DA AREA DE WORKING ***'.
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

           MOVE SPACES                 TO WRK-OUT-DADOS-EFTM.

           PERFORM 2000-PROCESSAR-MENSAGEM.

           GO TO 0000-INICIAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000-PROCESSAR-MENSAGEM         SECTION.
      *----------------------------------------------------------------*

           MOVE 'DCOMEFTM'             TO WRK-TELA

           IF  WRK-TRANSACAO           NOT EQUAL 'DCOM0506'
               PERFORM 2100-RECEBER-CONTROLE
           ELSE
               PERFORM 2200-PROCESSAR-DCOMEFTM
           END-IF.

           IF  WRK-TELA                EQUAL 'DCOMEFTM'
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
PROC01                                    TO WRK-INP-CHAMADOR-EFTM
PROC01                                       WRK-OUT-CHAMADOR-EFTM
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
PROC01         MOVE WRK-TRANSACAO         TO WRK-INP-CHAMADOR-EFTM
PROC01                                       WRK-OUT-CHAMADOR-EFTM
PROC01     END-IF.

           MOVE WRK-COMU-TPOPCAO       TO WRK-OUT-TIPOOPC-EFTM
                                          WRK-INP-TIPOOPC-EFTM.
           MOVE WRK-COMU-OPERACAO      TO WRK-INP-OPERAC-EFTM
                                          WRK-OUT-OPERAC-EFTM.
           MOVE WRK-COMU-BANCO         TO WRK-INP-BANCO-EFTM
           MOVE WRK-COMU-TMSTAMP       TO WRK-OUT-TMSTAMP-EFTM
                                          WRK-INP-TMSTAMP-EFTM.

           IF  WRK-TRANSACAO            EQUAL 'DCOM0502'
               MOVE 1                      TO WRK-INP-TPPESQ-EFTM
                                              WRK-OUT-TPPESQ-EFTM
           ELSE
               MOVE WRK-COMU-PAG           TO WRK-INP-PAGINA-EFTM
                                              WRK-OUT-PAGINA-EFTM
               MOVE WRK-COMU-OCORR         TO WRK-INP-OCORR-EFTM
                                              WRK-OUT-OCORR-EFTM
               IF  WRK-TRANSACAO         EQUAL 'DCOM0503'
                   MOVE 2                  TO WRK-INP-TPPESQ-EFTM
                                              WRK-OUT-TPPESQ-EFTM
               ELSE
                   IF  WRK-COMU-CCPFCNPJ       IS NUMERIC AND
                       WRK-COMU-CCPFCNPJ          GREATER ZEROS
                       MOVE 3                  TO WRK-INP-TPPESQ-EFTM
                                                  WRK-OUT-TPPESQ-EFTM
                   ELSE
                       MOVE 2                  TO WRK-INP-TPPESQ-EFTM
                                                  WRK-OUT-TPPESQ-EFTM
                   END-IF
               END-IF
           END-IF.

           PERFORM 2110-ACESSAR-DCOM4174

           IF  WRK-INCONSIS             EQUAL '*'
               GO TO 2100-99-FIM
           END-IF

           PERFORM 2111-MONTAR-TELA

           PERFORM 1111-ACESSAR-DCOM4266.

           IF  WRK-INCONSIS             NOT EQUAL '*'
               MOVE SG-MENSAGEM(110)    TO  WRK-OUT-MENSA-EFTM
               MOVE '1'                 TO  WRK-INP-FASE-EFTM
                                            WRK-OUT-FASE-EFTM
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
           MOVE WRK-INP-DANO-EFTM      TO 4174-ENT-DANO-OPER-DESC
           MOVE WRK-INP-NSEQ-EFTM      TO 4174-ENT-NSEQ-OPER-DESC.
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
                   MOVE 4174-SAI-MSG-RETORNO     TO WRK-OUT-MENSA-EFTM
                   MOVE '*'                      TO WRK-INCONSIS
           END-EVALUATE.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2111-MONTAR-TELA                SECTION.
      *----------------------------------------------------------------*

           MOVE 4174-SAI-DANO-BASE            TO WRK-OUT-NUM1CONS-EFTM
           MOVE 4174-SAI-CDEPDC               TO WRK-OUT-NUM2CONS-EFTM
           MOVE 4174-SAI-CSEQ-STUDO           TO WRK-OUT-NUM3CONS-EFTM
           MOVE 4174-SAI-CCART-LIM-CREDT      TO WRK-OUT-CARTCONS-EFTM

           MOVE 4174-SAI-IPSSOA-DESC-COML     TO WRK-OUT-NOME-EFTM.
           MOVE 4174-SAI-CCNPJ-CPF            TO WRK-INP-CNPJOC-EFTM
                                                 WRK-OUT-CNPJOC-EFTM.

           STRING 4174-SAI-CCNPJ-CPF(1:3) '.'
                  4174-SAI-CCNPJ-CPF(4:3) '.'
                  4174-SAI-CCNPJ-CPF(7:3) '/'
                  4174-SAI-CFLIAL-CNPJ    '-'
                  4174-SAI-CCTRL-CNPJ-CPF
           DELIMITED BY SIZE                INTO WRK-OUT-CNPJCPF-EFTM.

           STRING 4174-SAI-CBCO           '/'
                 4174-SAI-CAG-BCRIA       '/'
                 4174-SAI-CCTA-BCRIA-CLI
           DELIMITED BY SIZE                INTO WRK-OUT-BCAGCTA-EFTM.

           MOVE 4174-SAI-CPRODT               TO WRK-OUT-CODPROD-EFTM.
           MOVE 4174-SAI-IABREV-PRODT         TO WRK-OUT-DESPROD-EFTM.
           MOVE 4174-SAI-CSPROD-DESC-COML     TO WRK-OUT-CODSUBP-EFTM.
           MOVE 4174-SAI-IRSUMO-SPROD-DESC    TO WRK-OUT-DESSUBP-EFTM.
           MOVE 4174-SAI-IRSUMO-SIT-DESC      TO WRK-OUT-SITUAC-EFTM.

           IF 4174-SAI-CCONTR-LIM-DESC        GREATER ZEROS
              MOVE 'CONTR LIMITE:'            TO WRK-OUT-SIT1-EFTM
              MOVE 4174-SAI-CCONTR-LIM-DESC   TO WRK-OUT-SIT2-EFTM-N
           ELSE
              IF 4174-SAI-CCONTR-CONVE-DESC   GREATER ZEROS AND
                (4174-SAI-CELMTO-DESC-COML    EQUAL 8 OR 9 )
                 IF  4174-SAI-CELMTO-DESC-COML   EQUAL 8
                     MOVE 'CONV GERAL..:'     TO WRK-OUT-SIT1-EFTM
                     MOVE 4174-SAI-CCONTR-CONVE-DESC
                                              TO WRK-OUT-SIT2-EFTM-N
                 ELSE
                     MOVE 'CONV CLIENTE:'     TO WRK-OUT-SIT1-EFTM
                     MOVE 4174-SAI-CCONTR-CONVE-DESC
                                              TO WRK-OUT-SIT2-EFTM-N
                 END-IF
                 PERFORM 2112-VERIFICA-CONVENIO
              ELSE
                IF 4174-SAI-NSMULA-OPER-DESC  GREATER  ZEROS
                   MOVE 'SIMULACAO...:'       TO WRK-OUT-SIT3-EFTM
                   MOVE 4174-SAI-NSMULA-OPER-DESC
                                              TO WRK-OUT-SIT4-EFTM-N
                END-IF
             END-IF
           END-IF.

           MOVE 4174-SAI-VOPER-DESC-APURA    TO WRK-OUT-VLOPER-EFTM-N.
           MOVE 4174-SAI-VTAC-OPER-DESC      TO WRK-OUT-VLTAROP-EFTM-N

           MOVE 4174-SAI-VTARIF-REG-TITLO    TO WRK-OUT-VLTARTI-EFTM-N.
           MOVE 4174-SAI-DVCTO-FNAL-OPER     TO WRK-OUT-DTVENC-EFTM.

           INSPECT WRK-OUT-DTVENC-EFTM       REPLACING ALL '.' BY '/'.

           MOVE 4174-SAI-VJURO-OPER-DESC     TO WRK-OUT-VLJURO-EFTM-N.
           MOVE 4174-SAI-TMED-PONDE-OPER     TO WRK-OUT-PRZMED-EFTM-N.
           MOVE 4174-SAI-VIOF-OPER-DESC      TO WRK-OUT-VLIOF-EFTM-N.
           MOVE 4174-SAI-QTITLO-CHEQ-PCELA   TO WRK-OUT-QTDTIT-EFTM-N.
           MOVE 4174-SAI-VLIQ-OPER-DESC      TO WRK-OUT-VLLIQU-EFTM-N.
           MOVE 4174-SAI-ITPO-GARNT          TO WRK-OUT-GARANT-EFTM.
           MOVE 4174-SAI-VLIBRC-DESC-COML    TO WRK-OUT-VLLIBE-EFTM-N.
           MOVE 4174-SAI-IRSUMO-MEIO-ENTRD   TO WRK-OUT-MEIENT-EFTM.
           MOVE 4174-SAI-DINIC-OPER-DESC     TO WRK-OUT-DTOPER-EFTM.
           INSPECT WRK-OUT-DTOPER-EFTM       REPLACING ALL '.' BY '/'.

           MOVE 4174-SAI-IRSUMO-TPO-TX-JURO  TO  WRK-OUT-TPTAXA-EFTM
           MOVE 'A.M.'                       TO  WRK-OUT-DEJURO-EFTM
           MOVE 4174-SAI-PTX-JURO-MES        TO  WRK-OUT-TXJURO-EFTM-N.

      *----------------------------------------------------------------*
       2111-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1111-ACESSAR-DCOM4266           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE 4266E-ENTRADA
                      4266S-SAIDA

           MOVE 'DCOM4266'              TO  WRK-MODULO.

           MOVE  WRK-COD-USER           TO  4266E-CFUNC-BDSCO.
           MOVE  LNK-IO-LTERM           TO  4266E-CTERM.
           MOVE  WRK-INP-DANO-EFTM      TO  4266E-DANO-OPER-DESC.
           MOVE  WRK-INP-NSEQ-EFTM      TO  4266E-NSEQ-OPER-DESC.
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
                        WRK-OUT-TPTAXA-EFTM
                   IF  4266S-CINDCD-FREQ-TX  EQUAL 'AA'
                       MOVE 'A.A.'             TO WRK-OUT-DEJURO-EFTM
                       MOVE 4174-SAI-PTX-JURO-ANO
                                               TO WRK-OUT-TXJURO-EFTM-N
                   ELSE
                       MOVE 'A.M.'             TO WRK-OUT-DEJURO-EFTM
                       MOVE 4174-SAI-PTX-JURO-MES
                                               TO WRK-OUT-TXJURO-EFTM-N
                   END-IF

              WHEN '0099'
                    MOVE 'DB2'         TO ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO

              WHEN  OTHER
                    MOVE 'A.M.'                TO WRK-OUT-DEJURO-EFTM
                    MOVE 4174-SAI-PTX-JURO-MES
                                               TO WRK-OUT-TXJURO-EFTM-N
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
                        MOVE 'CONV GERAL..:'   TO WRK-OUT-SIT1-EFTM
                        MOVE 4174-SAI-CCONTR-CONVE-DESC
                                                TO WRK-OUT-SIT2-EFTM-N
                    ELSE
                        MOVE 'CONV CLIENTE:'    TO WRK-OUT-SIT1-EFTM
                        MOVE 4174-SAI-CCONTR-CONVE-DESC
                                                TO WRK-OUT-SIT2-EFTM-N
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
                                      TO WRK-OUT-MENSA-EFTM
           END-EVALUATE.

      *----------------------------------------------------------------*
       2112-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *  ESSA ROTINA OBTEM A PERIODICIDADE DA TAXA                     *
      *----------------------------------------------------------------*
       2200-PROCESSAR-DCOMEFTM         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM             TO WRK-INPUT-EFTM.

           PERFORM 2215-DEVOLVER-TELA

           EVALUATE TRUE
               WHEN WRK-INP-PFK-EFTM     EQUAL 'H' OR
                   (WRK-INP-PFK-EFTM     EQUAL '.' AND
                    WRK-INP-COMANDO-EFTM EQUAL 'PFK01')
                    MOVE WRK-INP-MENSA-EFTM TO WRK-OUT-MENSA-EFTM

               WHEN WRK-INP-PFK-EFTM     EQUAL '3' OR
                   (WRK-INP-PFK-EFTM     EQUAL '.' AND
                    WRK-INP-COMANDO-EFTM EQUAL 'PFK03')
                    PERFORM 2220-RETORNAR-CHAMADOR

               WHEN WRK-INP-PFK-EFTM     EQUAL '5' OR
                   (WRK-INP-PFK-EFTM     EQUAL '.' AND
                    WRK-INP-COMANDO-EFTM EQUAL 'PFK05')
                    PERFORM 2230-RETORNAR-MENU-ROTINAS

               WHEN WRK-INP-PFK-EFTM     EQUAL 'A' OR
                   (WRK-INP-PFK-EFTM     EQUAL '.' AND
                    WRK-INP-COMANDO-EFTM EQUAL 'PFK10')
                    PERFORM 2240-RETORNAR-MENU-DCOM

               WHEN (WRK-INP-PFK-EFTM     EQUAL '6'     AND
                     WRK-INP-FASE-EFTM    EQUAL  1  )
                                                            OR
                    (WRK-INP-PFK-EFTM     EQUAL '.'     AND
                     WRK-INP-COMANDO-EFTM EQUAL 'PFK06' AND
                     WRK-INP-FASE-EFTM    EQUAL  1  )
                     PERFORM 2245-ACESSAR-DCOM5335
                     IF  WRK-INCONSIS     EQUAL SPACES
                         PERFORM 2250-TRATAR-CONFIRMACAO
                     END-IF

               WHEN (WRK-INP-PFK-EFTM     EQUAL '.' AND
                     WRK-INP-COMANDO-EFTM EQUAL SPACES)
                     PERFORM 2255-TRATAR-ENTER

PROC01         WHEN (WRK-INP-PFK-EFTM     EQUAL  'B' OR
PROC01              (WRK-INP-PFK-EFTM     EQUAL  '.'  AND
PROC01               WRK-INP-COMANDO-EFTM EQUAL  'PFK11')) AND
PROC01               WRK-INP-FASE-EFTM    EQUAL  1
PROC01              PERFORM 2260-ACESSAR-CET
PROC01
               WHEN  OTHER
                     MOVE SG-MENSAGEM(38) TO WRK-OUT-MENSA-EFTM
           END-EVALUATE.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2215-DEVOLVER-TELA              SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-TPPESQ-EFTM     TO WRK-OUT-TPPESQ-EFTM.
           MOVE WRK-INP-OCULTOS-EFTM    TO WRK-OUT-OCULTOS-EFTM.
           MOVE WRK-INP-NOME-EFTM       TO WRK-OUT-NOME-EFTM.
           MOVE WRK-INP-CNPJCPF-EFTM    TO WRK-OUT-CNPJCPF-EFTM.
           MOVE WRK-INP-BCAGCTA-EFTM    TO WRK-OUT-BCAGCTA-EFTM.
           MOVE WRK-INP-PRODUTO-EFTM    TO WRK-OUT-PRODUTO-EFTM.
           MOVE WRK-INP-SUBPRODUTO-EFTM TO WRK-OUT-SUBPRODUTO-EFTM.
           MOVE WRK-INP-OPERAC-EFTM     TO WRK-OUT-OPERAC-EFTM.
           MOVE WRK-INP-SITUAC-EFTM     TO WRK-OUT-SITUAC-EFTM.
           MOVE WRK-INP-SIT1-EFTM       TO WRK-OUT-SIT1-EFTM.
           MOVE WRK-INP-SIT2-EFTM       TO WRK-OUT-SIT2-EFTM.
           MOVE WRK-INP-SIT3-EFTM       TO WRK-OUT-SIT3-EFTM.
           MOVE WRK-INP-SIT4-EFTM       TO WRK-OUT-SIT4-EFTM.
           MOVE WRK-INP-NUM1CONS-EFTM   TO WRK-OUT-NUM1CONS-EFTM
           MOVE WRK-INP-NUM2CONS-EFTM   TO WRK-OUT-NUM2CONS-EFTM
           MOVE WRK-INP-NUM3CONS-EFTM   TO WRK-OUT-NUM3CONS-EFTM
           MOVE WRK-INP-CARTCONS-EFTM   TO WRK-OUT-CARTCONS-EFTM
           MOVE WRK-INP-VLOPER-EFTM     TO WRK-OUT-VLOPER-EFTM.
           MOVE WRK-INP-TPTAXA-EFTM     TO WRK-OUT-TPTAXA-EFTM.
           MOVE WRK-INP-VLTAROP-EFTM    TO WRK-OUT-VLTAROP-EFTM.
           MOVE WRK-INP-TXJURO-EFTM     TO WRK-OUT-TXJURO-EFTM.
           MOVE WRK-INP-DEJURO-EFTM     TO WRK-OUT-DEJURO-EFTM.
           MOVE WRK-INP-VLTARTI-EFTM    TO WRK-OUT-VLTARTI-EFTM.
           MOVE WRK-INP-DTVENC-EFTM     TO WRK-OUT-DTVENC-EFTM.
           MOVE WRK-INP-VLJURO-EFTM     TO WRK-OUT-VLJURO-EFTM.
           MOVE WRK-INP-PRZMED-EFTM     TO WRK-OUT-PRZMED-EFTM.
           MOVE WRK-INP-VLIOF-EFTM      TO WRK-OUT-VLIOF-EFTM.
           MOVE WRK-INP-QTDTIT-EFTM     TO WRK-OUT-QTDTIT-EFTM.
           MOVE WRK-INP-VLLIQU-EFTM     TO WRK-OUT-VLLIQU-EFTM.
           MOVE WRK-INP-GARANT-EFTM     TO WRK-OUT-GARANT-EFTM.
           MOVE WRK-INP-VLLIBE-EFTM     TO WRK-OUT-VLLIBE-EFTM.
           MOVE WRK-INP-MEIENT-EFTM     TO WRK-OUT-MEIENT-EFTM.
           MOVE WRK-INP-DTOPER-EFTM     TO WRK-OUT-DTOPER-EFTM.

      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2220-RETORNAR-CHAMADOR          SECTION.
      *----------------------------------------------------------------*

           PERFORM 2225-FORMATAR-COMU-AREA.

           MOVE WRK-INP-CHAMADOR-EFTM  TO WRK-TELA.
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

           MOVE WRK-INP-TIPOOPC-EFTM   TO WRK-COMU-TPOPCAO
           MOVE WRK-INP-TMSTAMP-EFTM   TO WRK-COMU-TMSTAMP.
           MOVE WRK-INP-OPERAC-EFTM    TO WRK-COMU-OPERACAO.
           MOVE WRK-INP-BANCO-EFTM     TO WRK-COMU-BANCO.
           MOVE WRK-INP-PAGINA-EFTM    TO WRK-COMU-PAG
           MOVE WRK-INP-OCORR-EFTM     TO WRK-COMU-OCORR
           MOVE 'DCOM0506'             TO WRK-COMU-TRANSACAO.

           EVALUATE WRK-INP-TPPESQ-EFTM
               WHEN 2
               WHEN 3
                    IF  WRK-INP-TPPESQ-EFTM     EQUAL 2
                        MOVE WRK-INP-AGENCIA-EFTM
                                                TO WRK-COMU-AGENCIA
                        MOVE WRK-INP-CONTA-EFTM TO WRK-COMU-CONTA
                    ELSE
                        MOVE WRK-INP-CNPJOC-EFTM
                                              TO WRK-COMU-CCPFCNPJ
                        MOVE WRK-INP-FILIAL-EFTM
                                              TO WRK-COMU-CFLIAL
                        MOVE WRK-INP-CONTROLE-EFTM
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
           MOVE 'DCOM0506'             TO WRK-TELA
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
           MOVE 'DCOM0506'             TO WRK-TELA
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

           MOVE WRK-INP-OPERAC-EFTM    TO 5335E-OPERACAO.
           MOVE 006                    TO 5335E-CELMTO-DESC-COML.
           MOVE 009                    TO 5335E-CEVNTO-DESC-COML.
           MOVE 'E'                    TO 5335E-ACAO.
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
                                       TO WRK-OUT-MENSA-EFTM
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

           INITIALIZE 5421-ENTRADA
                      5421-SAIDA
                      ERRO-AREA.

           MOVE WRK-COD-USER             TO  WRK-5421E-CFUNC-BDSCO.
           MOVE LNK-IO-LTERM             TO  WRK-5421E-CTERM.
           MOVE WRK-INP-DANO-EFTM        TO  WRK-5421E-DANO-OPER-DESC
           MOVE WRK-INP-NSEQ-EFTM        TO  WRK-5421E-NSEQ-OPER-DESC

           MOVE 'DCOM5421'             TO WRK-MODULO.

           CALL WRK-MODULO             USING 5421-ENTRADA
                                             5421-SAIDA
                                             ERRO-AREA
                                             WRK-SQLCA.

           EVALUATE WRK-5421S-COD-RETORNO
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
                     MOVE WRK-5421S-MSG-RETORNO
                                       TO WRK-OUT-MENSA-EFTM
           END-EVALUATE.

      *----------------------------------------------------------------*
       2250-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2255-TRATAR-ENTER               SECTION.
      *----------------------------------------------------------------*

           MOVE '1'               TO WRK-INP-FASE-EFTM
                                     WRK-OUT-FASE-EFTM
           MOVE SG-MENSAGEM(110)  TO WRK-OUT-MENSA-EFTM.

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
PROC01     MOVE WRK-INP-TIPOOPC-EFTM   TO WRK-COMU-OPCAO-3924
PROC01     MOVE WRK-INP-TMSTAMP-EFTM   TO WRK-COMU-TIMESTAMP-3924.
PROC01     MOVE WRK-INP-OPERAC-EFTM    TO WRK-COMU-OPER-3924.
PROC01     MOVE WRK-INP-BANCO-EFTM     TO WRK-COMU-BANCO-3924.
PROC01     MOVE WRK-INP-PAGINA-EFTM    TO WRK-COMU-PAG-3924
PROC01     MOVE WRK-INP-OCORR-EFTM     TO WRK-COMU-OCORR-3924
PROC01     MOVE 'DCOM0507'             TO WRK-COMU-TRANSACAO-3924.
PROC01
PROC01     EVALUATE WRK-INP-TPPESQ-EFTM
PROC01         WHEN 2
PROC01         WHEN 3
PROC01              IF  WRK-INP-TPPESQ-EFTM     EQUAL 2
PROC01                  MOVE WRK-INP-AGENCIA-EFTM
PROC01                                          TO WRK-COMU-AGENC-3924
PROC01                  MOVE WRK-INP-CONTA-EFTM TO WRK-COMU-CONTA-3924
PROC01              ELSE
PROC01                  MOVE WRK-INP-CNPJOC-EFTM
PROC01                                        TO WRK-COMU-CNPJCPF-3924
PROC01                  MOVE WRK-INP-FILIAL-EFTM
PROC01                                        TO WRK-COMU-FILIAL-3924
PROC01                  MOVE WRK-INP-CONTROLE-EFTM
PROC01                                        TO WRK-COMU-CONTROL-3924
PROC01              END-IF
PROC01     END-EVALUATE.
PROC01
PROC01     MOVE 'DCOM3924'             TO WRK-TELA.
PROC01     MOVE WRK-INP-CHAMADOR-EFTM  TO WRK-COMU-CHAMADOR-3924.
PROC01
PROC01     MOVE WRK-CHNG               TO WRK-FUNCAO.
PROC01     MOVE WRK-COMU-AREA-3924     TO WRK-MENSAGEM.
PROC01
PROC01*----------------------------------------------------------------*
PROC01 2260-99-FIM.                    EXIT.
PROC01*----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2300-PROCESSAR-BRAD0660         SECTION.
      *----------------------------------------------------------------*

            MOVE LENGTH OF WRK-OUTPUT-EFTM
                                       TO  WRK-660-LL-MENSA-EFTM
                                           WRK-OUT-LL-EFTM
            MOVE LENGTH OF WRK-660-DCOMEFTM
                                       TO  WRK-660-LL-AREA-EFTM

           CALL 'BRAD0660'             USING WRK-OUTPUT-EFTM
                                             WRK-660-DCOMEFTM.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE RETURN-CODE        TO WRK-RETURN-CODE
               MOVE 10                 TO WRK-LOCAL-ERRO
               MOVE WRK-ERRO-BRAD0660  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-ISRT               TO WRK-FUNCAO.
           MOVE WRK-OUTPUT-EFTM        TO WRK-MENSAGEM.

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
               MOVE 'DCOM0506'         TO ERR-PGM
           ELSE
               MOVE 'SENHAS02'         TO ERR-PGM
               MOVE 'DCOM0506'         TO ERR-MODULO
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
