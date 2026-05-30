      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. DCOM0403.
       AUTHOR.     MARCELO MORINA.
      *================================================================*
      *                          A L T R A N                           *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   DCOM0403                                    *
      *    PROGRAMADOR.:   MARCELO MORINA MARQUES                      *
      *    ANALISTA....:   MARCELO MORINA MARQUES                      *
      *    DATA........:   01/02/2007                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   LIBERAR OPERACAO - OPERACOES POR AGENCIA.   *
      *----------------------------------------------------------------*
      *    TELAS.......:                                               *
      *    DCOMDCTM - LIBERAR OPERACAO - OPERACOES POR AGENCIA.        *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    I#BRAD7C - AREA DE ERRO DA BRAD7100.                        *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    BRAD0660 - INSERIR CARACTERES DE NULL NA MENSAGEM DE SAIDA  *
      *    BRAD7100 - TRATAMENTO DE ERROS QUANDO PROGRAMA INVALIDO     *
      *    POOL5005 - CONTROLE DO TRANSITO DE MENSAGENS NO IMS/DC      *
      *----------------------------------------------------------------*
      *    NAVEGACAO...:                                               *
      *    CHAMADO POR.: DCOM0402           CHAMA: DCOM0405            *
DTS001*================================================================*
DTS001*                    ALTERACAO - DTS CONSULTING                  *
DTS001*----------------------------------------------------------------*
DTS001*    PROGRAMADORA:   LUCIANDRA SILVEIRA - DTS CONSULTING         *
DTS001*    ANALISTA....:   LUCIANDRA SILVEIRA - DTS CONSULTING         *
DTS001*    ANALISTA DTS:   VERA MARIA POLLINI - BRADESCO - GRUPO 70    *
DTS001*    DATA........:   03/02/2010                                  *
DTS001*----------------------------------------------------------------*
DTS001*    OBJETIVO....:   EXECUTAR ROLLBACK CASO OCORRA ERRO NO MOMEN-*
DTS001*                    TO DA LIBERACAO DA OPERACAO (DCOM5110).     *
DTS001*================================================================*
DTS002*                   ALTERACAO  -  DTS  CONSULTING                *
DTS002*----------------------------------------------------------------*
DTS002*     PROGRAMADORA: LUCIANDRA SILVEIRA  -  DTS CONSULTING        *
DTS002*     ANALISTA....: LUCIANDRA SILVEIRA  -  DTS CONSULTING        *
DTS002*     ANALISTA DDS: VERA/EDGARD         -  GRUPO 70              *
DTS002*     DATA........: 06/05/2010                                   *
DTS002*----------------------------------------------------------------*
DTS002*     OBJETIVO....: NAO PERMITIR LIBERACAO DE OPERACAO CADASTRA- *
DTS002*                   DA PELO NET EMPRESA.                         *
DTS002*----------------------------------------------------------------*
DTS002*     MODULOS.....:                                              *
DTS002*     DCOM5505 - OBTEM RETORNO DAS SITUACOES DO WORKFLOW ENVIADO *
DTS002*================================================================*
DTS003*                   ALTERACAO  -  DTS  CONSULTING                *
DTS003*----------------------------------------------------------------*
DTS003*     PROGRAMADORA: LUCIANDRA SILVEIRA  -  DTS CONSULTING        *
DTS003*     ANALISTA....: LUCIANDRA SILVEIRA  -  DTS CONSULTING        *
DTS003*     ANALISTA DDS: EDGARD R. DE SOUZA  -  GRUPO 70              *
DTS003*     DATA........: 18/03/2011                                   *
DTS003*----------------------------------------------------------------*
DTS003*     OBJETIVO....: CORRECAO PARA EFETUAR COMMIT A CADA OPERACAO *
DTS003*                   LIBERADA PELA LISTA.                         *
DTS003*----------------------------------------------------------------*
DTS003*    NAVEGACAO...:                                               *
DTS003*    CHAMADO POR.:  DCOM0499                                     *
DTS003*================================================================*
CAP001*                   ALTERACAO - CAPGEMINI                        *
|     *----------------------------------------------------------------*
|     *   PROGRAMADOR.:   AMANDA BELTOSO     - CAPGEMINI               *
|     *   ANALISTA CAP:   POLIANA SILVA      - CAPGEMINI               *
|     *   ANALISTA DDS:   RICARDO JAMMAL     - BRADESCO - GRUPO 39     *
|     *   DATA........:   09/2020                                      *
|     *----------------------------------------------------------------*
|     *   OBJETIVO....: SOLICITAR PROTOCOLO DE EFETIVACAO NA CIP       *
|     *                 ACESSAR BVVEA504                               *
CAP001*================================================================*
FM0510*                   ALTERACAO - CAPGEMINI                        *
|     *----------------------------------------------------------------*
|     *   PROGRAMADOR.:   FELIPE MENDES      - CAPGEMINI               *
|     *   ANALISTA CAP:   FELIPE MENDES      - CAPGEMINI               *
|     *   ANALISTA DDS:   RICARDO JAMMAL     - BRADESCO - GRUPO 39     *
|     *   DATA........:   05/10/2020                                   *
|     *----------------------------------------------------------------*
|     *   OBJETIVO....: INCLUIR TRATAMENTO PARA DEMONSTRAR SITUACAO    *
|     *                 DAS OPERACOES CONFORME SITUACAO DO PROTOCOLO   *
FM0510*================================================================*
CAP002*                   ALTERACAO - CAPGEMINI                        *
|     *----------------------------------------------------------------*
|     *   PROGRAMADOR :   ANDREA             - CAPGEMINI               *
|     *   ANALISTA CAP:   ANDREA             - CAPGEMINI               *
|     *   ANALISTA DDS:   RICARDO JAMMAL     - BRADESCO - GRUPO 39     *
|     *   DATA........:   05/2021                                      *
|     *----------------------------------------------------------------*
|     *   OBJETIVO....: NAO PERMITIR LIBERACAO TIPO DESCONTO 4 ENTRE OS*
CAP002*                 DIAS 03 E 06.06.2021 - NORMAT 4734             *
CAP003*================================================================*
|     *                   ALTERACAO - CAPGEMINI                        *
|     *----------------------------------------------------------------*
|     *   PROGRAMADOR :   ANDREA             - CAPGEMINI               *
|     *   ANALISTA CAP:   ANDREA             - CAPGEMINI               *
|     *   ANALISTA DDS:   RICARDO JAMMAL     - BRADESCO - GRUPO 39     *
|     *   DATA........:   06/2021                                      *
|     *----------------------------------------------------------------*
|     *   OBJETIVO....: ALTERAR OCORRENCIAS NA CHAMADA DO BVVEA504     *
|     *                 DE 60 PARA 40.                                 *
|     *                 CORRIGIR PARA REALIZAR A INCLUSAO DO PROTOCOLO *
CAP003*                 SOMENTE UMA VEZ                                *
CAP004*================================================================*
|     *                   ALTERACAO - CAPGEMINI                        *
|     *----------------------------------------------------------------*
|     *   PROGRAMADOR :   AMANDA BELTOSO     - CAPGEMINI               *
|     *   ANALISTA CAP:   AMANDA BELTOSO     - CAPGEMINI               *
|     *   ANALISTA DDS:   RICARDO JAMMAL     - BRADESCO - GRUPO 39     *
|     *   DATA........:   06/2021                                      *
|     *----------------------------------------------------------------*
|     *   OBJETIVO....: ALTERAR MENSAGEM DE ERRO DE CONSISTENIA NO     *
|     *                 CODIGO 08 DO BVVEA504 PARA MENSAGEM RETORNADA  *
|     *                 PELO BVVE                                    O *
CAP004*================================================================*
WIP001*                     A L T E R A C A O                          *
WIP001*----------------------------------------------------------------*
WIP001*    PROGRAMADOR.:  DIRCEU RENATO MACAN                          *
WIP001*    ANALISTA....:  DIRCEU RENATO MACAN                          *
WIP001*    ANALISTA DS.:  DESIREE PESSOA DA SILVA                      *
WIP001*    DATA........:  26/11/2021                                   *
WIP001*----------------------------------------------------------------*
WIP001*    OBJETIVO....: .REMOVER MOVIMENTACAO DE TEXTO PARA ITEM      *
WIP001*                   GRUPO DA MENSAGEM DO DCOM5922                *
WIP001*                  .CORRIGIR TESTE DE CODIGO DE RETORNO ERRO DB2 *
WIP001*                   NA CHAMADA DO PROGRAMA DCOM5923 (DE 08 P/ 99)*
WIP001*                  .CORRIGIR TAMANHO DE CAMPO NA MOVIMENTACAO DE *
WIP001*                   MENSAGEM.                                    *
WIP001*                  .EXCLUIR LINHA DUPLICADA                      *
WIP001*                   IN6256068                                    *
WIP001*================================================================*
      *================================================================*
      *                   B  R  A  D  E  S  C  O                       *
      *----------------------------------------------------------------*
      *   ANALISTA....:  FERNANDA RODRIGUES  - BRADESCO - GRUPO 39     *
      *   DATA........:  25/04/2024                                    *
      *----------------------------------------------------------------*
F2404 *   OBJETIVO....:  INCLUIR TRATAMENTO PARA CLIENTES MASSIVOS     *
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
           '*** DCOM0403 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ATRIBUTOS ***'.
      *---------------------------------------------------------------*

      *--- ALPHA    - NORMAL    - PROTEGIDO    - NAO CURSOR ---*
       01  WRK-225                     PIC S9(008) COMP    VALUE +225.
      *--- NUMERICO - BRILHANTE - DESPROTEGIDO - POS CURSOR - NUM  ---*
       01  WRK-49369                   PIC S9(008) COMP    VALUE +49369.
      *--- BRILHANTE, NORMAL    - DEPROTEGIDO  - POS CURSOR - ALFA ---*
       01  WRK-49353                   PIC S9(008) COMP    VALUE +49353.
      *--- NUMERIC  - NORMAL    - DEPROTEGIDO  - POS CURSOR - ALFA ---*
       01  WRK-49345                   PIC S9(008) COMP    VALUE +49345.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*
       01  WRK-AUXILIARES.
DTS002     05  IND-1                   PIC  9(002) COMP-3  VALUE ZEROS.
CAP001     05  IND-2                   PIC  9(002) COMP-3  VALUE ZEROS.
CAP001     05  IND-3                   PIC  9(003) COMP-3  VALUE ZEROS.
           05  WRK-IND                 PIC  9(002) COMP-3  VALUE ZEROS.
           05  WRK-QTD-OPCAO-D         PIC  9(001)         VALUE ZEROS.
           05  WRK-QTD-OPCAO-C         PIC  9(001)         VALUE ZEROS.
           05  WRK-SQLCA               PIC  X(136)         VALUE SPACES.
           05  WRK-MODULO              PIC  X(008)         VALUE SPACES.
           05  WRK-SELEC-ANT           PIC  X(001)         VALUE SPACES.
           05  WRK-INCONSISTENCIA      PIC  X(001)         VALUE SPACES.
           05  WRK-FLAG-ERRO           PIC  X(001)         VALUE 'N'.
           05  WRK-MENSAGEM-ERRO       PIC  X(079)         VALUE SPACES.
           05  WRK-MENSA-PF7           PIC  X(079) VALUE
               'PF7 DESPREZARA AS ATUALIZACOES EFETUADAS - TECLE NOVAMEN
      -        'TE PARA VOLTAR'.
           05  WRK-MENSA-PF8           PIC  X(079) VALUE
               'PF8 DESPREZARA AS ATUALIZACOES EFETUADAS - TECLE NOVAMEN
      -        'TE PARA AVANCAR'.

CAP001     05  WRK-ACHOU-SOLTC         PIC  X(001)         VALUE SPACES.
|          05  WRK-SAIR                PIC  X(001)         VALUE SPACES.
|          05  WRK-ACAO                PIC  X(001)         VALUE SPACES.
|          05  WRK-TEMPO-ESPERA        PIC  9(003) COMP-3  VALUE ZEROS.
|          05  WRK-OK                  PIC  X(001)         VALUE SPACES.
|          05  WRK-PROCESSADO          PIC  X(001)         VALUE 'N'.
|          05  WRK-DESPREZAR           PIC  9(005) COMP-3  VALUE ZEROS.
|          05  WRK-FIM                 PIC  X(001)         VALUE SPACES.
|          05  WRK-PROTOCOLO           PIC  X(050)         VALUE SPACES.
|          05  WRK-TEMPO-ESPERA-X      PIC  X(008)         VALUE SPACES.
|          05  WRK-SITUACAO            PIC  9(001)         VALUE ZEROS.
CAP001*
      *
FM0510 01  WRK-CONTRATO-BVVE.
.          05 WRK-SIGLA-CCUSTO         PIC  X(004)         VALUE SPACES.
.          05 WRK-OPER-BVVE            PIC  X(013)         VALUE SPACES.
FM0510     05 FILLER                   PIC  X(002)         VALUE SPACES.
      *
CAP002 01 WRK-FLAG                     PIC  X(001)         VALUE SPACES.
      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREAS AUXILIARES PARA POOL5005 ***'.
      *----------------------------------------------------------------*
       01  WRK-POOL5005.
           05  WRK-GU                  PIC  X(004)         VALUE 'GU  '.
           05  WRK-CHNG                PIC  X(004)         VALUE 'CHNG'.
           05  WRK-ISRT                PIC  X(004)         VALUE 'ISRT'.
DTS001     05  WRK-ROLB                PIC  X(004)         VALUE 'ROLB'.
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

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA TELA DCOMDCTM  - INPUT ***'.
      *----------------------------------------------------------------*
       01  WRK-INPUT-DCTM.
           05  FILLER                  PIC  X(018) VALUE SPACES.
           05  WRK-INP-PFK-DCTM        PIC  X(001) VALUE SPACES.
           05  WRK-INP-SENHAS-DCTM     PIC  X(037) VALUE SPACES.
           05  WRK-INP-COMANDO-DCTM    PIC  X(068) VALUE SPACES.
           05  WRK-INP-DADOS-DCTM.
               10  WRK-INP-OCULTO-DCTM.
                   15  WRK-INP-TPOPCAO-DCTM
                                        PIC  X(001) VALUE SPACES.
                   15  WRK-INP-TMSTAMP-DCTM
                                        PIC  X(026) VALUE SPACES.
                   15  FILLER           PIC  X(041) VALUE SPACES.
               10  WRK-INP-FIMAMOS-DCTM PIC  X(001) VALUE SPACES.
               10  WRK-INP-FASE-DCTM    PIC  X(001) VALUE SPACES.
               10  WRK-INP-PAGINA-DCTM-X.
                   15  WRK-INP-PAGINA-DCTM
                                        PIC  9(003) VALUE ZEROS.
               10  WRK-INP-CODAGE-DCTM-X.
                   15  WRK-INP-CODAGE-DCTM
                                        PIC  9(005) VALUE ZEROS.
               10  WRK-INP-DESCAGE-DCTM PIC  X(020) VALUE SPACES.
               10  WRK-INP-TABELA-DCTM    OCCURS  05  TIMES.
                   15  WRK-INP-SELEC-DCTM     PIC  X(001) VALUE SPACES.
                   15  WRK-INP-OPERA-DCTM-X.
                       20  WRK-INP-OPERA-DCTM PIC  9(013) VALUE ZEROS.
DTS002                 20  FILLER REDEFINES   WRK-INP-OPERA-DCTM.
DTS002                     25 WRK-INP-DANO-DCTM    PIC  9(004).
DTS002                     25 WRK-INP-NSEQ-DCTM    PIC  9(009).
                   15  WRK-INP-PROD-DCTM      PIC  X(012) VALUE SPACES.
                   15  WRK-INP-SUBPROD-DCTM   PIC  X(010) VALUE SPACES.
                   15  WRK-INP-DTOPER-DCTM    PIC  X(010) VALUE SPACES.
                   15  WRK-INP-VALOR-DCTM.
                       20  WRK-INP-VALOR-DCTM-N PIC  ZZ.ZZZ.ZZZ.ZZ9,99.
                   15  WRK-INP-MEIOENT-DCTM   PIC  X(015) VALUE SPACES.
                   15  WRK-INP-SITANAL-DCTM   PIC  X(010) VALUE SPACES.
                   15  WRK-INP-PARECER-DCTM   PIC  X(003) VALUE SPACES.
                   15  WRK-INP-TIPOLIB-DCTM   PIC  X(001) VALUE SPACES.
                   15  WRK-INP-VLRLIQ-DCTM    PIC  9(15)V99 VALUE ZEROS.
           05  WRK-INP-MENSA-DCTM       PIC  X(079) VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                       PIC  X(050)         VALUE
           '*** AREA DA TELA DCOMDCTM  - OUTPUT ***'.
      *----------------------------------------------------------------*
       01  WRK-OUTPUT-DCTM.
           05  WRK-OUT-LL-DCTM          PIC S9(004) COMP VALUE +0843.
           05  WRK-OUT-ZZ-DCTM          PIC  9(004) COMP VALUE ZEROS.
           05  WRK-OUT-SENHAS-DCTM      PIC  X(037) VALUE SPACES.
           05  WRK-OUT-COMANDO-ATTR-DCTM PIC 9(004) COMP VALUE ZEROS.
           05  WRK-OUT-COMANDO-DCTM     PIC  X(068) VALUE SPACES.
           05  WRK-OUT-DADOS-DCTM.
               10  WRK-OUT-OCULTO-DCTM.
                   15  WRK-OUT-TPOPCAO-DCTM
                                        PIC  X(001) VALUE SPACES.
                   15  WRK-OUT-TMSTAMP-DCTM
                                        PIC  X(026) VALUE SPACES.
                   15  FILLER           PIC  X(041) VALUE SPACES.
               10  WRK-OUT-FIMAMOS-DCTM PIC  X(001) VALUE SPACES.
               10  WRK-OUT-FASE-DCTM    PIC  X(001) VALUE SPACES.
               10  WRK-OUT-PAGINA-DCTM-X.
                   15  WRK-OUT-PAGINA-DCTM
                                        PIC  9(003) VALUE ZEROS.
               10  WRK-OUT-CODAGE-DCTM-X.
                   15  WRK-OUT-CODAGE-DCTM
                                        PIC  9(005) VALUE ZEROS.
               10  WRK-OUT-DESCAGE-DCTM PIC  X(020) VALUE SPACES.
               10  WRK-OUT-TABELA-DCTM    OCCURS  05  TIMES.
                   15  WRK-OUT-SELEC-ATTR-DCTM PIC  9(04) COMP.
                   15  WRK-OUT-SELEC-DCTM      PIC  X(001) VALUE SPACES.
                   15  WRK-OUT-OPERA-DCTM-X.
                       20  WRK-OUT-OPERA-DCTM  PIC  9(013) VALUE ZEROS.
                   15  WRK-OUT-PROD-DCTM       PIC  X(012) VALUE SPACES.
                   15  WRK-OUT-SUBPROD-DCTM    PIC  X(010) VALUE SPACES.
                   15  WRK-OUT-DTOPER-DCTM     PIC  X(010) VALUE SPACES.
                   15  WRK-OUT-VALOR-DCTM-X.
                       20  WRK-OUT-VALOR-DCTM  PIC  ZZ.ZZZ.ZZZ.ZZ9,99.
                   15  WRK-OUT-MEIOENT-DCTM    PIC  X(015) VALUE SPACES.
                   15  WRK-OUT-SITANAL-DCTM    PIC  X(010) VALUE SPACES.
                   15  WRK-OUT-PARECER-DCTM    PIC  X(003) VALUE SPACES.
                   15  WRK-OUT-TIPOLIB-DCTM    PIC  X(001) VALUE SPACES.
                   15  WRK-OUT-VLRLIQ-DCTM    PIC  9(15)V99 VALUE ZEROS.
               10  WRK-OUT-MENSA-DCTM   PIC  X(079) VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA TELA DCOMDCTM  - BRAD0660 ***'.
      *----------------------------------------------------------------*
       01  WRK-660-DCOMDCTM.
           05  WRK-660-LL-AREA-DCTM     PIC  9(004) COMP   VALUE 0132.
           05  WRK-660-LL-MENSAGEM-DCTM PIC  9(004) COMP   VALUE 0843.
           05  WRK-660-SENHAS-DCTM      PIC  9(004) COMP   VALUE 0037.
           05  WRK-660-COMANDO-DCTM     PIC  9(004) COMP   VALUE 2070.
           05  WRK-660-OCULTO-DCTM      PIC  9(004) COMP   VALUE 0068.
           05  WRK-660-FIMAMOS-DCTM     PIC  9(004) COMP   VALUE 0001.
           05  WRK-660-FASE-DCTM        PIC  9(004) COMP   VALUE 0001.
           05  WRK-660-PAGINA-DCTM      PIC  9(004) COMP   VALUE 0003.
           05  WRK-660-CODAGE-DCTM      PIC  9(004) COMP   VALUE 0005.
           05  WRK-660-DESCAGE-DCTM     PIC  9(004) COMP   VALUE 0020.
           05  WRK-660-TABELA-DCTM    OCCURS  05  TIMES.
               10  WRK-660-SELEC-DCTM   PIC  9(004) COMP   VALUE 2003.
               10  WRK-660-OPERA-DCTM   PIC  9(004) COMP   VALUE 0013.
               10  WRK-660-PROD-DCTM    PIC  9(004) COMP   VALUE 0012.
               10  WRK-660-SUBPROD-DCTM PIC  9(004) COMP   VALUE 0010.
               10  WRK-660-DTOPER-DCTM  PIC  9(004) COMP   VALUE 0010.
               10  WRK-660-VALOR-DCTM   PIC  9(004) COMP   VALUE 0017.
               10  WRK-660-MEIOENT-DCTM PIC  9(004) COMP   VALUE 0015.
               10  WRK-660-SITANAL-DCTM PIC  9(004) COMP   VALUE 0010.
               10  WRK-660-PARECER-DCTM PIC  9(004) COMP   VALUE 0003.
               10  WRK-660-TIPOLIB-DCTM PIC  9(004) COMP   VALUE 0001.
               10  WRK-660-VLRLIQ-DCTM  PIC  9(004) COMP   VALUE 0017.
           05  WRK-660-MENSA-DCTM       PIC  9(004) COMP   VALUE 0079.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(080)         VALUE
           '*** AREA DE RECEBIMENTO E RETORNO DO DCOM0402 ***'.
      *----------------------------------------------------------------*

       01  COMU-AREA-UN .
           05  COMU-LL-UN              PIC S9(004) COMP  VALUE +297.
           05  COMU-ZZ-UN              PIC S9(004) COMP  VALUE +0.
           05  COMU-TRANCODE-UN .
               10  COMU-TRANSACAO-UN   PIC  X(008)       VALUE SPACES.
               10  FILLER-UN           PIC  X(006)       VALUE SPACES.
               10  COMU-PFK-UN         PIC  X(001)       VALUE SPACES.
           05  COMU-SENHAS-UN          PIC  X(037)       VALUE SPACES.
           05  COMU-COMANDO-UN         PIC  X(068)       VALUE SPACES.
           05  COMU-DADOS-UN .
               10  COMU-OPCAO-UN-X.
                   15  COMU-OPCAO-UN   PIC  9(001)       VALUE ZEROS.
               10  COMU-OPER-UN        PIC  9(013)       VALUE ZEROS.
               10  COMU-AGENCIA-UN     PIC  9(005)       VALUE ZEROS.
               10  COMU-AGENC-UN       PIC  9(005)       VALUE ZEROS.
               10  COMU-CONTA-UN       PIC  9(013)       VALUE ZEROS.
FS2511*        10  COMU-CNPJ-UN        PIC  9(009)       VALUE ZEROS.
FS2511         10  COMU-CNPJ-UN        PIC  X(009)       VALUE SPACES.
FS2511*        10  COMU-FILIAL-UN      PIC  9(004)       VALUE ZEROS.
FS2511         10  COMU-FILIAL-UN      PIC  X(004)       VALUE SPACES.
               10  COMU-DIG-UN         PIC  9(002)       VALUE ZEROS.
               10  COMU-OPERACAO-UN.
                 15 COMU-DANO-OPER-UN  PIC  9(004)       VALUE ZEROS.
                 15 COMU-NSEQ-OPER-UN  PIC  9(009)       VALUE ZEROS.
               10  COMU-TIMESTAMP-UN   PIC  X(026)       VALUE ZEROS.
               10  COMU-MENSAGEM-UN    PIC  X(079)       VALUE SPACES.
               10  COMU-PAGINA-UN      PIC  9(003)       VALUE ZEROS.
DTS003     05  WRK-COMU-DADOS-0499.
DTS003         10 WRK-COMU-OCORR-0499  PIC  9(002)       VALUE ZEROS.
DTS003         10 WRK-COMU-INPUT-0499  PIC  X(1000)      VALUE SPACES.

      *----------------------------------------------------------------*
      *---           AREA DE ACESSO AO MODULO DCOM4131              ---*
      *----------------------------------------------------------------*

       01  4131E-ROTEADOR.
           05  4131E-COD-RETORNO                 PIC  X(004).
           05  4131E-MSG-RETORNO                 PIC  X(079).
           05  4131E-RESTART                     PIC  9(005).
           05  4131E-FLAG                        PIC  X(001).
           05  4131E-CFUNC-BDSCO                 PIC  9(009).
           05  4131E-CTERM                       PIC  X(008).
           05  4131E-DANO-OPER-DESC              PIC  9(004).
           05  4131E-NSEQ-OPER-DESC              PIC  9(009).
           05  4131E-FLAG-FILTRO                 PIC  X(002).

       01  4131S-ROTEADOR.
           05  4131S-COD-RETORNO                 PIC  X(004).
           05  4131S-MSG-RETORNO                 PIC  X(079).
           05  4131S-RESTART                     PIC  9(005).
           05  4131S-FLAG                        PIC  X(001).
           05  4131S-DADOS.
             10  4131S-TPO-LIBRC                 PIC  X(001).
             10  4131S-CFUNC-AUTRZ-PRAZO         PIC  9(009).
             10  4131S-CFUNC-AUTRZ-TAXA          PIC  9(009).
             10  4131S-CFUNC-AUTRZ-CONE          PIC  9(009).
             10  4131S-PARECER-FORML-DESC        PIC  X(240).
             10  4131S-VLIBRC-MAX                PIC  9(015)V9(2).
             10  4131S-PMAX-LIBRC-ANTCP          PIC  9(003)V9(3).
             10  4131S-VLIBRC-A-LIBERAR          PIC  9(015)V9(2).
           05  4131S-CONSISTENCIA     REDEFINES  4131S-DADOS.
             10  4131S-TAMANHO-ERRO               PIC  9(001).
             10  4131S-TABELA-ERROS         OCCURS 2 TIMES.
               15  4131S-CAMPOS-ERRO             PIC  9(001).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO COM O MODULO DCOM5110 **'.
      *----------------------------------------------------------------*

       01  WRK-5110E-ENTRADA.
           03  WRK-5110E-COD-RETORNO      PIC  X(004).
           03  WRK-5110E-MSG-RETORNO      PIC  X(079).
           03  WRK-5110E-RESTART          PIC  9(005).
           03  WRK-5110E-FLAG             PIC  X(001).
           03  WRK-5110E-CFUNC-BDSCO      PIC  9(009).
           03  WRK-5110E-CTERM            PIC  X(008).
           03  WRK-5110E-DANO-OPER-DESC   PIC  9(004).
           03  WRK-5110E-NSEQ-OPER-DESC   PIC  9(009).
           03  WRK-5110E-TPO-LIBRC        PIC  X(001).
           03  WRK-5110E-VLIBRC           PIC  9(015)V9(02).

       01  WRK-5110S-SAIDA.
           03  WRK-5110S-COD-RETORNO      PIC  X(004).
           03  WRK-5110S-MSG-RETORNO      PIC  X(079).
           03  WRK-5110S-RESTART          PIC  9(005).
           03  WRK-5110S-FLAG             PIC  X(001).
           03  WRK-5110S-CONSISTENCIA.
               05  WRK-5110S-TAMANHO-ERRO PIC  9(001).
               05  WRK-5110S-TABELA-ERROS OCCURS  4 TIMES.
                   07  WRK-5110S-CAMPOS-ERRO
                                          PIC 9(001).

DTS002*----------------------------------------------------------------*
DTS002 01  FILLER                      PIC  X(50)          VALUE
DTS002     '*** AREA DO MODULO DCOM5505 ***'.
DTS002*----------------------------------------------------------------*
DTS002
DTS002 01  WRK-5505E-ENTRADA.
DTS002     05 WRK-5505E-COD-RETORNO    PIC  X(004)         VALUE SPACES.
DTS002     05 WRK-5505E-MSG-RETORNO    PIC  X(079)         VALUE SPACES.
DTS002     05 WRK-5505E-RESTART        PIC  9(005)         VALUE ZEROS.
DTS002     05 WRK-5505E-FLAG           PIC  X(001)         VALUE SPACES.
DTS002     05 WRK-5505E-CFUNC-BDSCO    PIC  9(009)         VALUE ZEROS.
DTS002     05 WRK-5505E-CTERM          PIC  X(008)         VALUE SPACES.
DTS002     05 WRK-5505E-DANO-OPER-DESC PIC  9(004)         VALUE ZEROS.
DTS002     05 WRK-5505E-NSEQ-OPER-DESC PIC  9(009)         VALUE ZEROS.
DTS002     05 WRK-5505E-CEVNTO-DESC-COML
DTS002                                 PIC  9(003)         VALUE ZEROS.
DTS002
DTS002 01  WRK-5505E-RETORNO.
DTS002     05 WRK-5505S-COD-RETORNO    PIC  9(004)         VALUE ZEROS.
DTS002     05 WRK-5505S-MSG-RETORNO    PIC  X(079)         VALUE SPACES.
DTS002     05 WRK-5505S-RESTART        PIC  9(005)         VALUE ZEROS.
DTS002     05 WRK-5505S-FLAG           PIC  X(001)         VALUE SPACES.
DTS002     05 WRK-5505S-CSIT-INIC-DESC PIC  9(003)         VALUE ZEROS.
DTS002     05 WRK-5505S-CSIT-FNAL-DESC PIC  9(003)         VALUE ZEROS.
DTS002     05 WRK-5505S-TAMANHO-ERRO   PIC  9(001)         VALUE ZEROS.
DTS002     05 WRK-5505S-TABELA-ERROS   OCCURS 012 TIMES.
DTS002       10 WRK-5505S-CAMPOS-ERRO  PIC  9(002)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO COM O MODULO DCOM4710 **'.
      *----------------------------------------------------------------*

       01  4710-ENTRADA-ROTEADOR.
           03  4710E-HEADER.
               05  4710E-COD-RETORNO              PIC  X(004).
               05  4710E-MSG-RETORNO              PIC  X(079).
               05  4710E-RESTART                  PIC  9(005).
               05  4710E-FLAG                     PIC  9(001).
               05  4710E-START-COUNT              PIC  X(001).
               05  4710E-CFUNC-BDSCO              PIC  9(009).
               05  4710E-CTERM                    PIC  X(008).
           03  4710E-DADOS.
               10  4710E-FLAG-FILTRO              PIC  X(002).
               10  4710E-CBCO                     PIC  9(003).
               10  4710E-CAG-BCRIA                PIC  9(005).
               10  4710E-CAG-OPER                 PIC  9(005).
               10  4710E-CCTA-BCRIA-CLI           PIC  9(013).
               10  4710E-CPRODT                   PIC  9(003).
               10  4710E-CSPROD-DESC-COML         PIC  9(003).
               10  4710E-DINIC-OPER-DESC          PIC  X(010).
               10  4710E-CELMTO-DESC-COML         PIC  9(003).
FS2511*        10  4710E-CCNPJ-CPF                PIC  9(009).
FS2511         10  4710E-CCNPJ-CPF                PIC  X(009).
FS2511*        10  4710E-CFLIAL-CNPJ              PIC  9(005).
FS2511         10  4710E-CFLIAL-CNPJ              PIC  X(004).
               10  4710E-CCTRL-CNPJ-CPF           PIC  9(002).
               10  4710E-CSIT-DESC-COML           PIC  9(003).
               10  4710E-CINDCD-EVNTO-WORKFLOW    PIC  9(003).
               10  4710E-CINDCD-SIT-FINAL-EVNTO   PIC  9(003).

       01  4710-SAIDA-ROTEADOR.
           03  4710S-HEADER.
               05  4710S-COD-RETORNO       PIC  X(004).
               05  4710S-MSG-RETORNO       PIC  X(079).
               05  4710S-RESTART           PIC  9(005).
               05  4710S-FLAG              PIC  X(001).
               05  4710S-QTDE-RETORNADA    PIC  9(003).
               05  4710S-COUNT             PIC  9(009).

           03  4710S-DADOS.
               05  4710S-TABELA-SAIDA      OCCURS 10  TIMES.
                   10  4710S-DANO-OPER-DESC         PIC  9(004).
                   10  4710S-NSEQ-OPER-DESC         PIC  9(009).
                   10  4710S-IPSSOA-DESC-COML       PIC  X(060).
                   10  4710S-CBCO                   PIC  9(003).
                   10  4710S-CAG-BCRIA              PIC  9(005).
                   10  4710S-CAG-BCRIA-DESC         PIC  X(040).
                   10  4710S-CCTA-BCRIA-CLI         PIC  9(013).
                   10  4710S-CBCO-OPER-OPER         PIC  9(003).
                   10  4710S-CAG-OPER-OPER          PIC  9(005).
                   10  4710S-CSIT-DESC-COML         PIC  9(003).
                   10  4710S-IRSUMO-SIT-DESC        PIC  X(015).
                   10  4710S-CSGL-SIT-DESC          PIC  X(006).
                   10  4710S-CPRODT                 PIC  9(003).
                   10  4710S-IABREV-PRODT           PIC  X(012).
                   10  4710S-CSGL-PRODT-DESC        PIC  X(006).
                   10  4710S-CSPROD-DESC-COML       PIC  9(003).
                   10  4710S-CTPO-DESC-COML         PIC  9(003).
                   10  4710S-IRSUMO-SPROD-DESC      PIC  X(010).
                   10  4710S-CSGL-SPROD-DESC        PIC  X(006).
                   10  4710S-VOPER-DESC-COML        PIC  9(015)V9(2).
                   10  4710S-DINIC-OPER-DESC        PIC  X(010).
                   10  4710S-DVCTO-FNAL-OPER        PIC  X(010).
                   10  4710S-CMEIO-ENTRD-DESC       PIC  9(03).
                   10  4710S-IMEIO-ENTRD-DESC       PIC  X(25).
                   10  4710S-IRSUMO-MEIO-ENTRD      PIC  X(15).
                   10  4710S-CSGL-MEIO-ENTRD        PIC  X(06).
                   10  4710S-CINDCD-LIBRC-ANTCP     PIC  X(01).
                   10  4710S-PARECER-FORML          PIC  X(01).
                   10  4710S-VLIQ-OPER-DESC         PIC  9(015)V9(2).

           03  4710S-CONSISTENCIA REDEFINES 4710S-DADOS.
               05  4710S-TAMANHO-ERRO      PIC  9(001).
               05  4710S-TABELA-ERROS      OCCURS  2 TIMES.
                   07  4710S-CAMPOS-ERRO   PIC  9(001).

CAP001*----------------------------------------------------------------*
|      01  FILLER                      PIC  X(050)         VALUE
|          '*** AREA DO BVVEA504 ***'.
|     *----------------------------------------------------------------*
|
|      COPY 'I#BVVEND'.

F2404 *----------------------------------------------------------------*
F2404  01  FILLER                      PIC  X(050)         VALUE
F2404      '*** AREA DO BVVEA520 ***'.
F2404 *----------------------------------------------------------------*
F2404
F2404  COPY 'I#BVVEFX'.

|     *----------------------------------------------------------------*
|      01  FILLER                      PIC  X(050)         VALUE
|          '*** AREA DE COMUNICACAO COM O MODULO DCOM4174 **'.
|     *----------------------------------------------------------------*
|
|      COPY 'I#DCOMXJ'.
|
|     *----------------------------------------------------------------*
|      01  FILLER                      PIC  X(050)         VALUE
|          '*** AREA DE COMUNICACAO COM O MODULO DCOM5922 **'.
|     *----------------------------------------------------------------*
|
|      COPY 'I#DCOMZD'.
|
|     *----------------------------------------------------------------*
|      01  FILLER                      PIC  X(050)         VALUE
|          '*** AREA DE COMUNICACAO COM O MODULO DCOM5923 **'.
|     *----------------------------------------------------------------*
|
|      COPY 'I#DCOMZH'.
|     *----------------------------------------------------------------*
|      01  FILLER                      PIC  X(050)         VALUE
|          '*** AREA DE COMUNICACAO COM O MODULO DCOM7341 **'.
|     *----------------------------------------------------------------*
|
|      COPY 'I#DCOMG8'.
|     *----------------------------------------------------------------*
|      01  FILLER                      PIC  X(050)         VALUE
|          '*** AREA PARA VARIAVEIS DE HEXAVISION ***'.
|     *----------------------------------------------------------------*
|     *
|      COPY 'I#DCOMHX'.
CAP001
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA BRAD7100 ***'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** DCOM0403 - FIM DA AREA DE WORKING ***'.
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

           MOVE SPACES                 TO  WRK-OUT-DADOS-DCTM
                                           WRK-OUT-SENHAS-DCTM
                                           WRK-OUT-COMANDO-DCTM
                                           WRK-INCONSISTENCIA.

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

           MOVE 'DCOMDCTM'             TO  WRK-TELA.

           IF  WRK-TRANSACAO    NOT EQUAL  'DCOM0403'
               PERFORM 1100-RECEBER-CONTROLE
           ELSE
               PERFORM 1200-PROCESSAR-DCOMDCTM
           END-IF.

           IF  WRK-TELA             EQUAL  'DCOMDCTM'
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

           INITIALIZE 4710-ENTRADA-ROTEADOR
                      4710-SAIDA-ROTEADOR.

           MOVE WRK-MENSAGEM           TO  COMU-AREA-UN .
           MOVE COMU-OPCAO-UN          TO  WRK-INP-TPOPCAO-DCTM
           MOVE COMU-TIMESTAMP-UN      TO  WRK-INP-TMSTAMP-DCTM
                                           WRK-OUT-TMSTAMP-DCTM.
           MOVE COMU-AGENCIA-UN        TO  WRK-INP-CODAGE-DCTM
                                           WRK-OUT-CODAGE-DCTM
                                           4710E-CAG-BCRIA.

DTS003     IF  WRK-TRANSACAO           EQUAL 'DCOM0499'
DTS003         MOVE WRK-COMU-INPUT-0499   TO WRK-INPUT-DCTM
DTS003         PERFORM 1210-DEVOLVER-TELA
DTS003
DTS003         ADD  1                     TO WRK-COMU-OCORR-0499
DTS003         PERFORM 1250-TRATAR-CONFIRMA
DTS003         GO TO 1100-99-FIM
DTS003     END-IF.
DTS003
           MOVE '1'                    TO  WRK-OUT-FASE-DCTM.

           IF  WRK-TRANSACAO           EQUAL 'DCOM0405'
               MOVE COMU-PAGINA-UN     TO  WRK-OUT-PAGINA-DCTM
               COMPUTE 4710E-RESTART = (COMU-PAGINA-UN - 1) * 5
               PERFORM 1120-ACESSAR-DCOM4710
               IF  4710S-COD-RETORNO   EQUAL '0003'
               AND COMU-PAGINA-UN      GREATER 1
                   MOVE 1              TO  WRK-OUT-PAGINA-DCTM
                   MOVE ZEROS          TO 4710E-RESTART
                   PERFORM 1120-ACESSAR-DCOM4710
               END-IF
           ELSE
               MOVE 1                  TO  WRK-OUT-PAGINA-DCTM
               MOVE ZEROS              TO  4710E-RESTART
               PERFORM 1120-ACESSAR-DCOM4710
           END-IF.

           IF  4710S-COD-RETORNO       EQUAL '0003'
               IF  WRK-TRANSACAO       EQUAL 'DCOM0405'
               AND COMU-MENSAGEM-UN    NOT EQUAL SPACES
                    MOVE COMU-MENSAGEM-UN TO WRK-MENSAGEM-ERRO
               ELSE
                    MOVE 'NAO EXISTE(M) OPERACAO(OES) PARA LIBERACAO'
                                          TO  WRK-MENSAGEM-ERRO
               END-IF
               MOVE 'S'                   TO  WRK-FLAG-ERRO
               PERFORM 1220-RETORNAR-CHAMADOR
           END-IF.


           MOVE 'SELECIONE COM "L" PARA LIBERAR, "D" PARA DETALHAR OU "C
      -         '" PARA CANCELAR'       TO  WRK-OUT-MENSA-DCTM

           IF  COMU-MENSAGEM-UN        NOT EQUAL SPACES
               MOVE COMU-MENSAGEM-UN   TO WRK-OUT-MENSA-DCTM
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA CARREGA OS CAMPOS DA TELA                         *
      *----------------------------------------------------------------*
       1110-MONTAR-TELA                SECTION.
      *----------------------------------------------------------------*

           IF 4710S-DANO-OPER-DESC(WRK-IND) IS NUMERIC
           AND 4710S-DANO-OPER-DESC(WRK-IND) NOT EQUAL ZEROS
              MOVE 4710S-DANO-OPER-DESC(WRK-IND)
                                  TO WRK-OUT-OPERA-DCTM(WRK-IND)(1:4)
              MOVE 4710S-NSEQ-OPER-DESC(WRK-IND)
                                  TO WRK-OUT-OPERA-DCTM(WRK-IND)(5:9)
              MOVE 4710S-IABREV-PRODT(WRK-IND)
                                  TO WRK-OUT-PROD-DCTM(WRK-IND)
              MOVE 4710S-IRSUMO-SPROD-DESC(WRK-IND)
                                  TO WRK-OUT-SUBPROD-DCTM(WRK-IND)
              STRING 4710S-DINIC-OPER-DESC(WRK-IND)(1:2)
              '/'    4710S-DINIC-OPER-DESC(WRK-IND)(4:2)
              '/'    4710S-DINIC-OPER-DESC(WRK-IND)(7:4)
              DELIMITED BY SIZE   INTO WRK-OUT-DTOPER-DCTM(WRK-IND)
              MOVE 4710S-VOPER-DESC-COML(WRK-IND)
                                  TO WRK-OUT-VALOR-DCTM(WRK-IND)
              MOVE 4710S-IRSUMO-MEIO-ENTRD(WRK-IND)
                                  TO WRK-OUT-MEIOENT-DCTM(WRK-IND)
              MOVE 4710S-IRSUMO-SIT-DESC(WRK-IND)
                                  TO WRK-OUT-SITANAL-DCTM(WRK-IND)
FM0510        PERFORM 1115-OBTER-SIT-PROTOC
              IF  4710S-PARECER-FORML(WRK-IND) EQUAL 'S'
                  MOVE 'SIM'      TO WRK-OUT-PARECER-DCTM(WRK-IND)
              ELSE
                  MOVE 'NAO'      TO WRK-OUT-PARECER-DCTM(WRK-IND)
              END-IF
              MOVE 4710S-CINDCD-LIBRC-ANTCP(WRK-IND)
                                  TO WRK-OUT-TIPOLIB-DCTM(WRK-IND)
              MOVE 4710S-VLIQ-OPER-DESC(WRK-IND)
                                  TO WRK-OUT-VLRLIQ-DCTM(WRK-IND)
           ELSE

              MOVE WRK-225        TO WRK-OUT-SELEC-ATTR-DCTM(WRK-IND)

           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

FM0510*----------------------------------------------------------------*
.     *    OBTER SITUACAO DO PROTOCOLO                                 *
.     *----------------------------------------------------------------*
.      1115-OBTER-SIT-PROTOC           SECTION.
.     *----------------------------------------------------------------*
.
.          INITIALIZE DCOMZD-BLOCO-ENTRADA
.                     DCOMZD-RETORNO
.
.          MOVE 'E'                    TO DCOMZD-E-CTPO-PROT-AGNDA.
.          MOVE 4710S-DANO-OPER-DESC(WRK-IND)
.                                      TO DCOMZD-E-DANO-OPER-DESC.
.          MOVE 4710S-NSEQ-OPER-DESC(WRK-IND)
.                                      TO DCOMZD-E-NSEQ-OPER-DESC.
.
.          MOVE 'DCOM5922'             TO WRK-MODULO.
.
.          CALL WRK-MODULO             USING DCOMZD-BLOCO-ENTRADA
.                                            DCOMZD-RETORNO
.                                            ERRO-AREA
.                                            WRK-SQLCA.
.
.          EVALUATE DCOMZD-S-COD-RETORNO
.              WHEN ZEROS
.                   EVALUATE DCOMZD-S-CSIT-PROT-AGNDA
.                       WHEN 1
.                       WHEN 2
.                            MOVE 'EM LIBER'
.                                      TO WRK-OUT-SITANAL-DCTM(WRK-IND)
.                       WHEN 3
.                            CONTINUE
.                       WHEN 4
.                            MOVE 'VLR INDISP'
.                                      TO WRK-OUT-SITANAL-DCTM(WRK-IND)
.                       WHEN OTHER
.                            MOVE 'REJEIT CIP'
.                                      TO WRK-OUT-SITANAL-DCTM(WRK-IND)
.                   END-EVALUATE
.
.              WHEN 08
.                   CONTINUE
.
.              WHEN 99
.                   MOVE 'DB2'         TO ERR-TIPO-ACESSO
.                   PERFORM 9999-PROCESSAR-ROTINA-ERRO
.              WHEN OTHER
.                   MOVE 'DCOM5922'    TO ERR-MODULO
.                   MOVE 'APL'         TO ERR-TIPO-ACESSO
WIP001              MOVE DCOMZD-S-MSG-RETORNO(1:79)
WIP001                                 TO ERR-TEXTO
.                   PERFORM 9999-PROCESSAR-ROTINA-ERRO
.          END-EVALUATE.
.
.     *----------------------------------------------------------------*
.      1115-99-FIM.                    EXIT.
FM0510*----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA PROCESSA A MENSAGEM VINDA DA TELA DCOMDCTM        *
      *----------------------------------------------------------------*
       1120-ACESSAR-DCOM4710           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-COD-USER           TO  4710E-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO  4710E-CTERM
           MOVE ZEROS                  TO  4710E-FLAG
           MOVE 'N'                    TO  4710E-START-COUNT
           MOVE 237                    TO  4710E-CBCO
           MOVE 'LI'                   TO  4710E-FLAG-FILTRO.
           MOVE 'DCOM4710'             TO  WRK-MODULO

           CALL WRK-MODULO    USING    4710-ENTRADA-ROTEADOR
                                       4710-SAIDA-ROTEADOR
                                       ERRO-AREA
                                       WRK-SQLCA.

           EVALUATE 4710S-COD-RETORNO
               WHEN '0000'
                    IF 4710S-DANO-OPER-DESC(6) EQUAL ZEROS
                    OR 4710S-DANO-OPER-DESC(6) NOT NUMERIC
                       MOVE '*'     TO  WRK-OUT-FIMAMOS-DCTM
                    ELSE
                       MOVE SPACES  TO  WRK-OUT-FIMAMOS-DCTM
                    END-IF

                    MOVE 'AMOSTRAGEM CONTINUA'  TO  WRK-OUT-MENSA-DCTM

                    PERFORM VARYING WRK-IND FROM 1 BY 1
                              UNTIL WRK-IND GREATER 5
                        INITIALIZE WRK-OUT-TABELA-DCTM(WRK-IND)
                        MOVE SPACES    TO WRK-OUT-OPERA-DCTM-X(WRK-IND)
                                          WRK-OUT-VALOR-DCTM-X(WRK-IND)
                    END-PERFORM

                    PERFORM 1110-MONTAR-TELA VARYING WRK-IND FROM
                            1 BY 1 UNTIL WRK-IND GREATER 5

                    MOVE 4710S-CAG-BCRIA-DESC(1)
                                    TO WRK-OUT-DESCAGE-DCTM

               WHEN '0003'
                   GO TO 1120-99-FIM

               WHEN '0099'
                    MOVE  'DB2'     TO     ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                    MOVE  'APL'     TO     ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-EVALUATE.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA PROCESSA A MENSAGEM VINDA DA TELA DCOMDCTM        *
      *----------------------------------------------------------------*
       1200-PROCESSAR-DCOMDCTM         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM             TO WRK-INPUT-DCTM.
           PERFORM 1210-DEVOLVER-TELA

           EVALUATE TRUE
               WHEN WRK-INP-PFK-DCTM     EQUAL 'H' OR
                   (WRK-INP-PFK-DCTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DCTM EQUAL 'PFK01')
                    IF   WRK-INP-FASE-DCTM    EQUAL '1'
                        PERFORM VARYING WRK-IND FROM 1 BY 1
                                  UNTIL WRK-IND GREATER 5
                            IF WRK-INP-OPERA-DCTM-X(WRK-IND)
                                             EQUAL SPACES OR LOW-VALUES
                                MOVE WRK-225 TO WRK-OUT-SELEC-ATTR-DCTM
                                                              (WRK-IND)
                            END-IF
                        END-PERFORM
                        MOVE ZEROS    TO WRK-OUT-COMANDO-ATTR-DCTM
                    ELSE
                        PERFORM VARYING WRK-IND FROM 1 BY 1
                                  UNTIL WRK-IND GREATER 5
                          MOVE WRK-225 TO WRK-OUT-SELEC-ATTR-DCTM
                                                              (WRK-IND)
                        END-PERFORM
                        MOVE WRK-49353 TO WRK-OUT-COMANDO-ATTR-DCTM
                    END-IF
                    MOVE WRK-INP-MENSA-DCTM TO WRK-OUT-MENSA-DCTM

               WHEN WRK-INP-PFK-DCTM     EQUAL '3' OR
                   (WRK-INP-PFK-DCTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DCTM EQUAL 'PFK03')
                    PERFORM 1220-RETORNAR-CHAMADOR

               WHEN WRK-INP-PFK-DCTM     EQUAL 'X' OR
                   (WRK-INP-PFK-DCTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DCTM EQUAL 'PFK05')
                    PERFORM 1230-RETORNAR-MENU-ROTINAS

               WHEN WRK-INP-PFK-DCTM     EQUAL 'Z' OR
                   (WRK-INP-PFK-DCTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DCTM EQUAL 'PFK10')
                    PERFORM 1240-RETORNAR-MENU-DCOM

               WHEN WRK-INP-FASE-DCTM    EQUAL '1'
                    PERFORM 1201-TRATAR-FASE-1

               WHEN WRK-INP-FASE-DCTM    EQUAL '2'
                    PERFORM 1202-TRATAR-FASE-2

               WHEN OTHER
                    MOVE 'PF INVALIDA'   TO WRK-OUT-MENSA-DCTM
                    PERFORM VARYING WRK-IND FROM 1 BY 1
                              UNTIL WRK-IND GREATER 5
                        IF  WRK-INP-OPERA-DCTM-X(WRK-IND)
                                         EQUAL SPACES OR LOW-VALUES
                            MOVE WRK-225 TO WRK-OUT-SELEC-ATTR-DCTM
                                                               (WRK-IND)
                        END-IF
                    END-PERFORM
           END-EVALUATE.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  TRATA SELECAO E BLOQUEIA A TELA                               *
      *----------------------------------------------------------------*
       1201-TRATAR-FASE-1              SECTION.
      *----------------------------------------------------------------*
           EVALUATE TRUE
               WHEN WRK-INP-PFK-DCTM     EQUAL '7' OR
                   (WRK-INP-PFK-DCTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DCTM EQUAL 'PFK07')
                    PERFORM 1260-TRATAR-VOLTA-PAG

               WHEN WRK-INP-PFK-DCTM     EQUAL '8' OR
                   (WRK-INP-PFK-DCTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DCTM EQUAL 'PFK08')
                    PERFORM 1270-TRATAR-AVANCA-PAG

               WHEN WRK-INP-PFK-DCTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DCTM EQUAL  SPACES OR LOW-VALUES
                    PERFORM 1280-TRATAR-ENTER
                    IF  WRK-INCONSISTENCIA EQUAL SPACES
                        IF  WRK-QTD-OPCAO-D GREATER ZEROS
                        OR  WRK-QTD-OPCAO-C GREATER ZEROS
                          PERFORM 1251-CHAMAR-TRAN-DCOM0405
                        ELSE
                          PERFORM VARYING WRK-IND FROM 1 BY 1
                              UNTIL WRK-IND GREATER 5
                            MOVE WRK-225 TO WRK-OUT-SELEC-ATTR-DCTM
                                                          (WRK-IND)
                          END-PERFORM
                          MOVE WRK-49345 TO WRK-OUT-COMANDO-ATTR-DCTM
                          MOVE '2'       TO WRK-OUT-FASE-DCTM
                          MOVE 'TECLE <PF6> PARA CONFIRMAR OU TECLE <ENT
      -                        'ER> PARA ALTERAR'
                                         TO WRK-OUT-MENSA-DCTM
                        END-IF
                    ELSE
                        PERFORM VARYING WRK-IND FROM 1 BY 1
                              UNTIL WRK-IND GREATER 5
                          IF  WRK-INP-OPERA-DCTM-X(WRK-IND)
                                         EQUAL SPACES OR LOW-VALUES
                            MOVE WRK-225 TO WRK-OUT-SELEC-ATTR-DCTM
                                                               (WRK-IND)
                          END-IF
                        END-PERFORM
                    END-IF

               WHEN OTHER
                    MOVE 'PF INVALIDA'   TO WRK-OUT-MENSA-DCTM
                    PERFORM VARYING WRK-IND FROM 1 BY 1
                              UNTIL WRK-IND GREATER 5
                        IF  WRK-INP-OPERA-DCTM-X(WRK-IND)
                                         EQUAL SPACES OR LOW-VALUES
                            MOVE WRK-225 TO WRK-OUT-SELEC-ATTR-DCTM
                                                               (WRK-IND)
                        END-IF
                    END-PERFORM
           END-EVALUATE.
      *----------------------------------------------------------------*
       1201-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  TRATA CONFIRMACAO DE LIBERACAO OU CANCELAMENTO                *
      *----------------------------------------------------------------*
       1202-TRATAR-FASE-2              SECTION.
      *----------------------------------------------------------------*
           EVALUATE TRUE
               WHEN WRK-INP-PFK-DCTM     EQUAL '6' OR
                   (WRK-INP-PFK-DCTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DCTM EQUAL 'PFK06')
DTS003              MOVE 1               TO WRK-COMU-OCORR-0499
                    PERFORM 1250-TRATAR-CONFIRMA
                    MOVE ZEROS           TO WRK-OUT-COMANDO-ATTR-DCTM

               WHEN WRK-INP-PFK-DCTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DCTM EQUAL  SPACES OR LOW-VALUES
                    PERFORM VARYING WRK-IND FROM 1 BY 1
                              UNTIL WRK-IND GREATER 5
                        IF  WRK-INP-OPERA-DCTM-X(WRK-IND)
                                         EQUAL SPACES OR LOW-VALUES
                            MOVE WRK-225 TO WRK-OUT-SELEC-ATTR-DCTM
                                                               (WRK-IND)
                        END-IF
                    END-PERFORM
                    MOVE '1'             TO WRK-OUT-FASE-DCTM
                    MOVE 'SELECIONE COM "L" PARA LIBERAR, "D" PARA DETAL
      -                  'HAR OU "C" PARA CANCELAR'
                                         TO  WRK-OUT-MENSA-DCTM
                    MOVE ZEROS           TO WRK-OUT-COMANDO-ATTR-DCTM

               WHEN OTHER
                    MOVE 'PF INVALIDA'   TO WRK-OUT-MENSA-DCTM
                    PERFORM VARYING WRK-IND FROM 1 BY 1
                              UNTIL WRK-IND GREATER 5
                        MOVE WRK-225     TO WRK-OUT-SELEC-ATTR-DCTM
                                                              (WRK-IND)
                    END-PERFORM
                    MOVE WRK-49345       TO WRK-OUT-COMANDO-ATTR-DCTM
           END-EVALUATE.
      *----------------------------------------------------------------*
       1201-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA DEVOLVE OS DADOS DA TELA SEM ALTERACAO            *
      *----------------------------------------------------------------*
       1210-DEVOLVER-TELA              SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-FIMAMOS-DCTM   TO WRK-OUT-FIMAMOS-DCTM.
           MOVE WRK-INP-FASE-DCTM      TO WRK-OUT-FASE-DCTM.
           MOVE WRK-INP-PAGINA-DCTM-X  TO WRK-OUT-PAGINA-DCTM-X.
           MOVE WRK-INP-CODAGE-DCTM-X  TO WRK-OUT-CODAGE-DCTM-X.
           MOVE WRK-INP-DESCAGE-DCTM   TO WRK-OUT-DESCAGE-DCTM.
           MOVE WRK-INP-TMSTAMP-DCTM   TO WRK-OUT-TMSTAMP-DCTM.

           PERFORM VARYING WRK-IND FROM 1 BY 1 UNTIL WRK-IND GREATER 5
              MOVE WRK-INP-SELEC-DCTM(WRK-IND)
                                      TO WRK-OUT-SELEC-DCTM(WRK-IND)

              MOVE WRK-INP-OPERA-DCTM-X(WRK-IND)
                                      TO WRK-OUT-OPERA-DCTM-X(WRK-IND)
              MOVE WRK-INP-PROD-DCTM(WRK-IND)
                                      TO WRK-OUT-PROD-DCTM(WRK-IND)
              MOVE WRK-INP-SUBPROD-DCTM(WRK-IND)
                                      TO WRK-OUT-SUBPROD-DCTM(WRK-IND)
              MOVE WRK-INP-DTOPER-DCTM(WRK-IND)
                                      TO WRK-OUT-DTOPER-DCTM(WRK-IND)
              MOVE WRK-INP-VALOR-DCTM(WRK-IND)
                                      TO WRK-OUT-VALOR-DCTM-X(WRK-IND)
              MOVE WRK-INP-MEIOENT-DCTM(WRK-IND)
                                      TO WRK-OUT-MEIOENT-DCTM(WRK-IND)
              MOVE WRK-INP-SITANAL-DCTM(WRK-IND)
                                      TO WRK-OUT-SITANAL-DCTM(WRK-IND)
              MOVE WRK-INP-PARECER-DCTM(WRK-IND)
                                      TO WRK-OUT-PARECER-DCTM(WRK-IND)
              MOVE WRK-INP-TIPOLIB-DCTM(WRK-IND)
                                      TO WRK-OUT-TIPOLIB-DCTM(WRK-IND)
              MOVE WRK-INP-VLRLIQ-DCTM(WRK-IND)
                                      TO WRK-OUT-VLRLIQ-DCTM(WRK-IND)
           END-PERFORM.

      *----------------------------------------------------------------*
       1210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA RETORNA AO PROGRAMA CHAMADOR                      *
      *----------------------------------------------------------------*
       1220-RETORNAR-CHAMADOR          SECTION.
      *----------------------------------------------------------------*

           INITIALIZE  COMU-AREA-UN .

           IF  WRK-FLAG-ERRO         EQUAL 'S'
               MOVE WRK-MENSAGEM-ERRO       TO COMU-MENSAGEM-UN
           END-IF.

           MOVE LENGTH OF COMU-AREA-UN
                                       TO COMU-LL-UN
           MOVE ZEROS                  TO COMU-ZZ-UN

           MOVE '3'                    TO COMU-PFK-UN
           MOVE WRK-CHNG               TO WRK-FUNCAO.
           MOVE 'DCOM0402'             TO WRK-TELA.
           MOVE 'DCOM0403'             TO COMU-TRANSACAO-UN .
           MOVE WRK-INP-SENHAS-DCTM    TO COMU-SENHAS-UN .
           MOVE WRK-INP-TMSTAMP-DCTM   TO COMU-TIMESTAMP-UN.
           MOVE COMU-AREA-UN           TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRATAMENTO DE RETORNO AO MENU ROTINAS    *
      *----------------------------------------------------------------*
       1230-RETORNAR-MENU-ROTINAS      SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-CHNG            TO WRK-FUNCAO.
           MOVE 'DCOM0403'          TO WRK-TELA
                                       COMU-TRANSACAO-UN .
           MOVE '5'                 TO COMU-PFK-UN .
           MOVE COMU-AREA-UN       TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRATAMENTO DE RETORNO AO MENU DCOM       *
      *----------------------------------------------------------------*
       1240-RETORNAR-MENU-DCOM         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-CHNG           TO WRK-FUNCAO.
           MOVE 'DCOM0403'         TO WRK-TELA
                                      COMU-TRANSACAO-UN .
           MOVE 'A'                TO COMU-PFK-UN .
           MOVE COMU-AREA-UN       TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1240-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRAMAMENTO DO COMANDO CONFIRMA(PFK6)     *
      *----------------------------------------------------------------*
       1250-TRATAR-CONFIRMA            SECTION.
      *----------------------------------------------------------------*

DTS003     PERFORM
DTS003     VARYING WRK-IND             FROM WRK-COMU-OCORR-0499 BY 1
DTS003       UNTIL WRK-IND             GREATER 5

             IF  WRK-INP-SELEC-DCTM(WRK-IND) EQUAL 'L'
CAP001           INITIALIZE          WRK-SAIR
CAP001                               WRK-OK
CAP001                               WRK-PROCESSADO
CAP001                               WRK-FIM
CAP001                               WRK-SITUACAO
CAP001                               WRK-PROTOCOLO
CAP001
CAP001           MOVE WRK-IND          TO IND-2

                 PERFORM 1910-ACESSAR-DCOM4174

CAP002           IF (WRK-FLAG          EQUAL 'S')
|                AND (WRK-INP-TMSTAMP-DCTM(1:10)
|                                      EQUAL '2021-06-03'
|                                         OR '2021-06-04'
|                                         OR '2021-06-05'
|                                         OR '2021-06-06')
|                   MOVE 'NAO PERMITIDO LIBERACAO DE ORPAGS ENTRE OS DIA
|     -                   'S 03 E 06/06/2021 - NORMAT.4734'
|                                       TO WRK-MENSAGEM-ERRO
|                                          WRK-OUT-MENSA-DCTM
|                    MOVE 'S'           TO WRK-FLAG-ERRO
|                    GO TO 1250-99-FIM
CAP002           END-IF

CAP001           PERFORM 1800-TRATA-OPERACOES

CAP001           IF WRK-OK             EQUAL 'S'
                    PERFORM 1252-1-CHAMAR-MOD-DCOM4131

                    IF WRK-INCONSISTENCIA   EQUAL SPACES
                       PERFORM 1252-CHAMAR-MOD-DCOM5110

DTS003                 IF WRK-INCONSISTENCIA  EQUAL SPACES
DTS003                    PERFORM 1253-CHAMAR-TRANS-COMMIT
DTS003                    GO TO 1250-99-FIM
DTS003                 END-IF
                    END-IF
DTS003              IF  WRK-INCONSISTENCIA    EQUAL '*'
DTS003                  GO TO 1250-99-FIM
DTS003              END-IF
CAP001           END-IF
             END-IF
           END-PERFORM.

           MOVE SPACES                 TO WRK-OUT-FIMAMOS-DCTM.

           MOVE '1'                    TO  WRK-OUT-FASE-DCTM.

CAP001     IF WRK-OK                   NOT EQUAL 'S'
CAP001        GO TO 1250-99-FIM
CAP001     END-IF

           INITIALIZE 4710-ENTRADA-ROTEADOR
                      4710-SAIDA-ROTEADOR.

           MOVE WRK-INP-CODAGE-DCTM    TO  4710E-CAG-BCRIA
           COMPUTE 4710E-RESTART = (WRK-INP-PAGINA-DCTM - 1) * 5

           PERFORM 1120-ACESSAR-DCOM4710.

           IF  4710S-COD-RETORNO       EQUAL '0003'
           AND WRK-INP-PAGINA-DCTM     GREATER 1
               INITIALIZE 4710-ENTRADA-ROTEADOR
                          4710-SAIDA-ROTEADOR

               MOVE WRK-INP-CODAGE-DCTM
                                       TO 4710E-CAG-BCRIA
               MOVE ZEROS              TO 4710E-RESTART
               MOVE 1                  TO WRK-OUT-PAGINA-DCTM
               PERFORM 1120-ACESSAR-DCOM4710
           END-IF.

           IF  4710S-COD-RETORNO       EQUAL '0003'
               MOVE 'LIBERACAO(OES) EFETUADA(S) COM SUCESSO - C/C CREDIT
      -             'ADA'
                                       TO WRK-MENSAGEM-ERRO
               MOVE 'S'                TO WRK-FLAG-ERRO
               PERFORM 1220-RETORNAR-CHAMADOR
           ELSE
DTS003         IF  WRK-INCONSISTENCIA  EQUAL '?'
                   MOVE WRK-MENSAGEM-ERRO
                                       TO WRK-OUT-MENSA-DCTM
               ELSE
                   MOVE 'LIBERACAO(OES) EFETUADA(S) COM SUCESSO - C/C CR
      -                 'EDITADA'
                                       TO WRK-OUT-MENSA-DCTM
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1250-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  OBTEM VALOR A SER LIBERADO                                    *
      *----------------------------------------------------------------*
       1252-1-CHAMAR-MOD-DCOM4131      SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-OPERA-DCTM(WRK-IND)(1:4)
                                       TO 4131E-DANO-OPER-DESC
           MOVE WRK-INP-OPERA-DCTM(WRK-IND)(5:9)
                                       TO 4131E-NSEQ-OPER-DESC
           MOVE WRK-COD-USER           TO 4131E-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO 4131E-CTERM
           MOVE 'LI'                   TO 4131E-FLAG-FILTRO
           MOVE 'DCOM4131'             TO WRK-MODULO.

           CALL WRK-MODULO             USING 4131E-ROTEADOR
                                             4131S-ROTEADOR
                                             ERRO-AREA
                                             WRK-SQLCA.

           EVALUATE 4131S-COD-RETORNO
               WHEN '0000'
                   MOVE 4131S-VLIBRC-A-LIBERAR
                                  TO WRK-INP-VLRLIQ-DCTM(WRK-IND)
                   IF  4131S-PMAX-LIBRC-ANTCP NOT EQUAL 100
                       MOVE 'TIPO DE LIBERACAO ANTECIPADA EXIGE DETALHAM
      -                     'ENTO'
                         TO WRK-OUT-MENSA-DCTM
                       MOVE WRK-49369
                                     TO WRK-OUT-SELEC-ATTR-DCTM(WRK-IND)
                       MOVE '*'      TO WRK-INCONSISTENCIA
                       MOVE '1'      TO  WRK-OUT-FASE-DCTM
                       PERFORM VARYING WRK-IND FROM 1 BY 1
                                 UNTIL WRK-IND GREATER 5
                         IF  WRK-INP-OPERA-DCTM-X(WRK-IND) EQUAL SPACES
                             MOVE WRK-225
                                  TO WRK-OUT-SELEC-ATTR-DCTM(WRK-IND)
                         END-IF
                       END-PERFORM
                   END-IF

               WHEN '0099'
                   MOVE 'DB2'          TO ERR-TIPO-ACESSO
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                   MOVE 4131S-MSG-RETORNO
                                       TO WRK-OUT-MENSA-DCTM
                   MOVE '*'            TO WRK-INCONSISTENCIA

           END-EVALUATE.

      *----------------------------------------------------------------*
       1252-1-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA PASSA O CONTROLE PARA TRAN. DCOM0405              *
      *----------------------------------------------------------------*
       1251-CHAMAR-TRAN-DCOM0405       SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-CHNG                    TO WRK-FUNCAO.
           MOVE 'DCOM0405'                  TO WRK-TELA.
           MOVE 'DCOM0403'                  TO COMU-TRANSACAO-UN
           MOVE WRK-INP-SENHAS-DCTM         TO COMU-SENHAS-UN
           MOVE WRK-INP-TMSTAMP-DCTM        TO COMU-TIMESTAMP-UN
           MOVE WRK-INP-PAGINA-DCTM         TO COMU-PAGINA-UN

           PERFORM VARYING WRK-IND FROM 1 BY 1
                     UNTIL WRK-IND GREATER 5
               IF  WRK-INP-SELEC-DCTM(WRK-IND) EQUAL 'D' OR 'C'
                   MOVE WRK-INP-SELEC-DCTM(WRK-IND)
                                            TO COMU-OPCAO-UN-X
                   MOVE WRK-INP-OPERA-DCTM-X(WRK-IND)
                                            TO COMU-OPERACAO-UN
               END-IF
           END-PERFORM.
           MOVE WRK-INP-CODAGE-DCTM    TO COMU-AGENCIA-UN.
           MOVE COMU-AREA-UN                TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1251-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA PASSA O CONTROLE PARA MODULO DCOM5110             *
      *----------------------------------------------------------------*
       1252-CHAMAR-MOD-DCOM5110        SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-5110E-ENTRADA
                      WRK-5110S-SAIDA.

           MOVE ZEROS                  TO WRK-5110E-COD-RETORNO
           MOVE SPACES                 TO WRK-5110E-MSG-RETORNO
           MOVE ZEROS                  TO WRK-5110E-RESTART
           MOVE SPACES                 TO WRK-5110E-FLAG
           MOVE WRK-COD-USER           TO WRK-5110E-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO WRK-5110E-CTERM
           MOVE WRK-INP-OPERA-DCTM(WRK-IND)(1:4)
                                       TO WRK-5110E-DANO-OPER-DESC
           MOVE WRK-INP-OPERA-DCTM(WRK-IND)(5:9)
                                       TO WRK-5110E-NSEQ-OPER-DESC
           MOVE WRK-INP-VLRLIQ-DCTM(WRK-IND)
                                       TO WRK-5110E-VLIBRC
           MOVE 4131S-TPO-LIBRC
                                       TO WRK-5110E-TPO-LIBRC
           MOVE 'DCOM5110'             TO WRK-MODULO

           CALL WRK-MODULO    USING    WRK-5110E-ENTRADA
                                       WRK-5110S-SAIDA
                                       ERRO-AREA
                                       WRK-SQLCA.

           IF  WRK-5110S-COD-RETORNO EQUAL '0099'
               MOVE  'DB2'       TO     ERR-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           ELSE
           IF  WRK-5110S-COD-RETORNO EQUAL '7777' OR
               WRK-5110S-COD-RETORNO EQUAL '7779'
               MOVE  'APL'       TO     ERR-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           ELSE
               IF  WRK-5110S-COD-RETORNO NOT EQUAL '0000'
DTS001             PERFORM 8888-EXECUTAR-ROLLBACK
                   STRING WRK-INP-OPERA-DCTM(WRK-IND)
                       '-' WRK-5110S-MSG-RETORNO
                   DELIMITED BY SIZE INTO WRK-MENSAGEM-ERRO
                   MOVE WRK-MENSAGEM-ERRO TO WRK-OUT-MENSA-DCTM
                   MOVE WRK-49369    TO WRK-OUT-SELEC-ATTR-DCTM(WRK-IND)
                   MOVE 6            TO WRK-IND
DTS003             MOVE '?'          TO WRK-INCONSISTENCIA
               END-IF
           END-IF
           END-IF.

      *----------------------------------------------------------------*
       1252-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
DTS003
DTS003*----------------------------------------------------------------*
DTS003*  ESSA ROTINA CHAMA A TRANSACAO DCOM0499 PARA EFETUAR COMMIT    *
DTS003*----------------------------------------------------------------*
DTS003 1253-CHAMAR-TRANS-COMMIT        SECTION.
DTS003*----------------------------------------------------------------*
DTS003
DTS003     INITIALIZE COMU-AREA-UN.
DTS003
DTS003     MOVE LENGTH                 OF COMU-AREA-UN
DTS003                                 TO COMU-LL-UN.
DTS003     MOVE WRK-INP-TPOPCAO-DCTM   TO COMU-OPCAO-UN.
DTS003     MOVE WRK-INP-TMSTAMP-DCTM   TO COMU-TIMESTAMP-UN.
DTS003     MOVE WRK-INP-PAGINA-DCTM    TO COMU-PAGINA-UN.
DTS003     MOVE WRK-INP-CODAGE-DCTM    TO COMU-AGENCIA-UN.
DTS003     MOVE WRK-INPUT-DCTM         TO WRK-COMU-INPUT-0499.
DTS003     MOVE WRK-IND                TO WRK-COMU-OCORR-0499.
DTS003
DTS003     MOVE 'DCOM0403'             TO COMU-TRANSACAO-UN.
DTS003     MOVE 'DCOM0499'             TO WRK-TELA.
DTS003     MOVE WRK-CHNG               TO WRK-FUNCAO.
DTS003     MOVE COMU-AREA-UN           TO WRK-MENSAGEM.
DTS003
DTS003*----------------------------------------------------------------*
DTS003 1253-99-FIM.                    EXIT.
DTS003*----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRAMAMENTO DO COMANDO PFK7 - VOLTA PAG.  *
      *----------------------------------------------------------------*
       1260-TRATAR-VOLTA-PAG           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE 4710-ENTRADA-ROTEADOR
                      4710-SAIDA-ROTEADOR.

           IF WRK-INP-PAGINA-DCTM  EQUAL  1
              MOVE 'NAO EXISTE PAGINA PARA VOLTAR'
                                       TO WRK-OUT-MENSA-DCTM
              PERFORM VARYING WRK-IND FROM 1 BY 1
                        UNTIL WRK-IND GREATER 5
                  IF  WRK-INP-OPERA-DCTM-X(WRK-IND)
                                       EQUAL SPACES OR LOW-VALUES
                      MOVE WRK-225 TO WRK-OUT-SELEC-ATTR-DCTM(WRK-IND)
                  END-IF
              END-PERFORM
              GO TO 1260-99-FIM
           END-IF.

           IF  WRK-INP-MENSA-DCTM      NOT EQUAL WRK-MENSA-PF7
               IF (WRK-INP-SELEC-DCTM(1) EQUAL 'L' OR 'D' OR 'C')
               OR (WRK-INP-SELEC-DCTM(2) EQUAL 'L' OR 'D' OR 'C')
               OR (WRK-INP-SELEC-DCTM(3) EQUAL 'L' OR 'D' OR 'C')
               OR (WRK-INP-SELEC-DCTM(4) EQUAL 'L' OR 'D' OR 'C')
               OR (WRK-INP-SELEC-DCTM(5) EQUAL 'L' OR 'D' OR 'C')
                   MOVE WRK-MENSA-PF7  TO WRK-OUT-MENSA-DCTM
                   GO TO 1260-99-FIM
               END-IF
           END-IF.

           MOVE SPACES                 TO WRK-OUT-FIMAMOS-DCTM
                                          WRK-OUT-SELEC-DCTM(1)
                                          WRK-OUT-SELEC-DCTM(2)
                                          WRK-OUT-SELEC-DCTM(3)
                                          WRK-OUT-SELEC-DCTM(4)
                                          WRK-OUT-SELEC-DCTM(5).

           MOVE WRK-COD-USER           TO  4710E-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO  4710E-CTERM
           MOVE 237                    TO  4710E-CBCO
           MOVE 'N'                    TO  4710E-START-COUNT
           MOVE ZEROS                  TO  4710E-FLAG
           MOVE 'LI'                   TO  4710E-FLAG-FILTRO
           MOVE WRK-INP-CODAGE-DCTM    TO  4710E-CAG-BCRIA
           COMPUTE 4710E-RESTART = (WRK-INP-PAGINA-DCTM - 2) * 5

           PERFORM 1120-ACESSAR-DCOM4710.

           COMPUTE WRK-OUT-PAGINA-DCTM = WRK-INP-PAGINA-DCTM - 1.

      *----------------------------------------------------------------*
       1260-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRAMAMENTO DO COMANDO PFK8 - AVANCA PAG. *
      *----------------------------------------------------------------*
       1270-TRATAR-AVANCA-PAG          SECTION.
      *----------------------------------------------------------------*

           INITIALIZE 4710-ENTRADA-ROTEADOR
                      4710-SAIDA-ROTEADOR.

           IF WRK-INP-FIMAMOS-DCTM  EQUAL  '*'
              MOVE 'NAO EXISTE PAGINA PARA AVANCAR'
                                       TO WRK-OUT-MENSA-DCTM
              PERFORM VARYING WRK-IND FROM 1 BY 1
                        UNTIL WRK-IND GREATER 5
                  IF  WRK-INP-OPERA-DCTM-X(WRK-IND)
                                       EQUAL SPACES OR LOW-VALUES
                      MOVE WRK-225 TO WRK-OUT-SELEC-ATTR-DCTM(WRK-IND)
                  END-IF
              END-PERFORM
              GO TO 1270-99-FIM
           END-IF.

           IF  WRK-INP-MENSA-DCTM      NOT EQUAL WRK-MENSA-PF8
               IF (WRK-INP-SELEC-DCTM(1) EQUAL 'L' OR 'D' OR 'C')
               OR (WRK-INP-SELEC-DCTM(2) EQUAL 'L' OR 'D' OR 'C')
               OR (WRK-INP-SELEC-DCTM(3) EQUAL 'L' OR 'D' OR 'C')
               OR (WRK-INP-SELEC-DCTM(4) EQUAL 'L' OR 'D' OR 'C')
               OR (WRK-INP-SELEC-DCTM(5) EQUAL 'L' OR 'D' OR 'C')
                   MOVE WRK-MENSA-PF8  TO WRK-OUT-MENSA-DCTM
                   GO TO 1270-99-FIM
               END-IF
           END-IF.

           MOVE WRK-COD-USER           TO  4710E-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO  4710E-CTERM
           MOVE 237                    TO  4710E-CBCO
           MOVE 'N'                    TO  4710E-START-COUNT
           MOVE ZEROS                  TO  4710E-FLAG
           MOVE 'LI'                   TO  4710E-FLAG-FILTRO
           MOVE WRK-INP-CODAGE-DCTM    TO  4710E-CAG-BCRIA
           COMPUTE 4710E-RESTART = (WRK-INP-PAGINA-DCTM) * 5

           PERFORM 1120-ACESSAR-DCOM4710.
           IF  WRK-OUT-FIMAMOS-DCTM    NOT EQUAL SPACES
               MOVE 'FIM DE AMOSTRAGEM' TO WRK-OUT-MENSA-DCTM
           END-IF.

           COMPUTE WRK-OUT-PAGINA-DCTM = WRK-INP-PAGINA-DCTM + 1.

      *----------------------------------------------------------------*
       1270-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRAMAMENTO DO COMANDO ENTER              *
      *----------------------------------------------------------------*
       1280-TRATAR-ENTER               SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INP-SELEC-DCTM(1)   EQUAL SPACES
           AND WRK-INP-SELEC-DCTM(2)   EQUAL SPACES
           AND WRK-INP-SELEC-DCTM(3)   EQUAL SPACES
           AND WRK-INP-SELEC-DCTM(4)   EQUAL SPACES
           AND WRK-INP-SELEC-DCTM(5)   EQUAL SPACES
               MOVE 'NENHUMA OPCAO FOI SELECIONADA'
                                       TO  WRK-OUT-MENSA-DCTM
               MOVE '*'                TO WRK-INCONSISTENCIA
               GO TO 1280-99-FIM
           END-IF.

           PERFORM VARYING WRK-IND FROM 1 BY 1 UNTIL WRK-IND GREATER 5
             IF  WRK-INP-SELEC-DCTM(WRK-IND) NOT EQUAL 'L' AND 'C'
                                                   AND 'D' AND SPACES
                                                   AND LOW-VALUES
                 MOVE '*'        TO WRK-INCONSISTENCIA
                 MOVE WRK-49353  TO WRK-OUT-SELEC-ATTR-DCTM(WRK-IND)
                 MOVE 'OPCAO INVALIDA - SELECIONAR APENAS: L/D/C'
                                 TO  WRK-OUT-MENSA-DCTM
             END-IF
           END-PERFORM.

           IF WRK-INCONSISTENCIA  EQUAL  '*'
              GO TO 1280-99-FIM
           END-IF.

           MOVE SPACES           TO WRK-SELEC-ANT

           PERFORM VARYING WRK-IND FROM 1 BY 1 UNTIL WRK-IND GREATER 5
             IF  WRK-INP-SELEC-DCTM(WRK-IND) NOT EQUAL WRK-SELEC-ANT AND
                 WRK-SELEC-ANT               NOT EQUAL SPACES
                 IF  WRK-INP-SELEC-DCTM(WRK-IND)  NOT EQUAL  SPACES
                     MOVE '*'        TO WRK-INCONSISTENCIA
                 END-IF
             ELSE
                 MOVE WRK-INP-SELEC-DCTM(WRK-IND) TO WRK-SELEC-ANT
             END-IF
           END-PERFORM.

           IF WRK-INCONSISTENCIA  EQUAL  '*'
              PERFORM VARYING WRK-IND FROM 1 BY 1
                        UNTIL WRK-IND GREATER 5
                 IF WRK-INP-SELEC-DCTM(WRK-IND) NOT EQUAL SPACES
                                                      AND LOW-VALUES
                    MOVE WRK-49353   TO WRK-OUT-SELEC-ATTR-DCTM(WRK-IND)
                 END-IF
              END-PERFORM
              MOVE 'SELECIONAR APENAS UMA OPCAO: L/D/C'
                                     TO  WRK-OUT-MENSA-DCTM
              GO TO 1280-99-FIM
           END-IF.


           MOVE    ZEROS         TO WRK-QTD-OPCAO-D
                                    WRK-QTD-OPCAO-C

           PERFORM VARYING WRK-IND FROM 1 BY 1 UNTIL WRK-IND GREATER 5
             IF  WRK-INP-SELEC-DCTM(WRK-IND) EQUAL 'D'
                 ADD  1          TO WRK-QTD-OPCAO-D
             END-IF
           END-PERFORM.

           IF WRK-QTD-OPCAO-D  GREATER  1
              PERFORM VARYING WRK-IND FROM 1 BY 1
                        UNTIL WRK-IND GREATER 5
                 IF WRK-INP-SELEC-DCTM(WRK-IND) NOT EQUAL SPACES
                                                      AND LOW-VALUES
                    MOVE WRK-49353   TO WRK-OUT-SELEC-ATTR-DCTM(WRK-IND)
                 END-IF
              END-PERFORM
              MOVE 'SELECIONAR APENAS UMA OCORRENCIA PARA OPCAO "D"'
                              TO  WRK-OUT-MENSA-DCTM
              MOVE '*'        TO WRK-INCONSISTENCIA
           END-IF.

           PERFORM VARYING WRK-IND FROM 1 BY 1 UNTIL WRK-IND GREATER 5
             IF  WRK-INP-SELEC-DCTM(WRK-IND) EQUAL 'C'
                 ADD  1          TO WRK-QTD-OPCAO-C
             END-IF
           END-PERFORM.

           IF WRK-QTD-OPCAO-C  GREATER  1
              PERFORM VARYING WRK-IND FROM 1 BY 1
                        UNTIL WRK-IND GREATER 5
                 IF WRK-INP-SELEC-DCTM(WRK-IND) NOT EQUAL SPACES
                                                      AND LOW-VALUES
                    MOVE WRK-49353   TO WRK-OUT-SELEC-ATTR-DCTM(WRK-IND)
                 END-IF
              END-PERFORM
              MOVE 'SELECIONAR APENAS UMA OCORRENCIA PARA OPCAO "C"'
                              TO  WRK-OUT-MENSA-DCTM
              MOVE '*'        TO WRK-INCONSISTENCIA
           END-IF.

DTS002     IF  WRK-INCONSISTENCIA      NOT EQUAL '*'
DTS002         PERFORM VARYING IND-1   FROM 1 BY 1
DTS002           UNTIL IND-1           GREATER 5
DTS002              IF WRK-INP-SELEC-DCTM(IND-1)
DTS002                                 NOT EQUAL SPACES
DTS002                 PERFORM 1285-VERIFICAR-OPER-NETEMPR
DTS002              END-IF
DTS002         END-PERFORM
DTS002     END-IF.

           IF  WRK-INCONSISTENCIA EQUAL '*'
               GO TO 1280-99-FIM
           END-IF.

      *----------------------------------------------------------------*
       1280-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

DTS002*----------------------------------------------------------------*
DTS002*    VERIFICA SE OPERACAO CADASTRADA PELO NET EMPRESA:           *
DTS002*  - OPERACAO CADASTRADA PELO NET EMPRESA, PODERA SER ALTERADA   *
DTS002*    PELA AGENCIA  SOMENTE  SE FOI CANCELADA A  LIBERACAO PELA   *
DTS002*    AGENCIA (SITUACAO FINAL = 21)                               *
DTS002*----------------------------------------------------------------*
DTS002 1285-VERIFICAR-OPER-NETEMPR     SECTION.
DTS002*----------------------------------------------------------------*
DTS002
DTS002     INITIALIZE WRK-5505E-ENTRADA
DTS002                WRK-5505E-RETORNO.
DTS002
DTS002     MOVE WRK-COD-USER-R         TO WRK-5505E-CFUNC-BDSCO.
DTS002     MOVE LNK-IO-LTERM           TO WRK-5505E-CTERM.
DTS002     MOVE WRK-INP-DANO-DCTM     (IND-1)
DTS002                                 TO WRK-5505E-DANO-OPER-DESC
DTS002     MOVE WRK-INP-NSEQ-DCTM     (IND-1)
DTS002                                 TO WRK-5505E-NSEQ-OPER-DESC
DTS002     MOVE 140                    TO WRK-5505E-CEVNTO-DESC-COML.
DTS002     MOVE 'DCOM5505'             TO WRK-MODULO.
DTS002
DTS002     CALL WRK-MODULO             USING WRK-5505E-ENTRADA
DTS002                                       WRK-5505E-RETORNO
DTS002                                       ERRO-AREA
DTS002                                       WRK-SQLCA.
DTS002
DTS002     EVALUATE  WRK-5505S-COD-RETORNO
DTS002         WHEN  0000
DTS002           IF  WRK-5505S-CSIT-FNAL-DESC EQUAL 22
DTS002               MOVE '*'          TO WRK-INCONSISTENCIA
DTS002               MOVE 'OPERACAO NAO PERMITE A OPCAO SELECIONADA, OPE
DTS002-                   'RACAO CADASTRADA PELO NETEMPRESA'
DTS002                                 TO WRK-OUT-MENSA-DCTM
DTS002               MOVE WRK-49353    TO
DTS002                                 WRK-OUT-SELEC-ATTR-DCTM(IND-1)
DTS002           END-IF
DTS002
DTS002        WHEN  0099
DTS002              MOVE 'DB2'         TO ERR-TIPO-ACESSO
DTS002              PERFORM 9999-PROCESSAR-ROTINA-ERRO
DTS002     END-EVALUATE.
DTS002
DTS002*----------------------------------------------------------------*
DTS002 1285-99-FIM.                    EXIT.
DTS002*----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA CARREGA A TELA QUE SERA DEMONSTRADA PELO IMS.     *
      *----------------------------------------------------------------*
       1300-PROCESSAR-BRAD0660         SECTION.
      *----------------------------------------------------------------*

           CALL 'BRAD0660'             USING WRK-OUTPUT-DCTM
                                             WRK-660-DCOMDCTM.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE RETURN-CODE        TO WRK-RETURN-CODE
               MOVE 10                 TO WRK-LOCAL-ERRO
               MOVE WRK-ERRO-BRAD0660  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-ISRT               TO WRK-FUNCAO.
           MOVE WRK-OUTPUT-DCTM        TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

CAP001*----------------------------------------------------------------*
|     *  ESSA ROTINA IRA TRATAR LIBERACAO DE VARIAS OPERACOES          *
|     *----------------------------------------------------------------*
|      1800-TRATA-OPERACOES            SECTION.
|     *----------------------------------------------------------------*
|
|          IF WRK-FLAG                 EQUAL 'N'
|             MOVE 'S'                 TO WRK-OK
|             GO TO 1800-99-FIM
|          END-IF
|
|     *    VERIFICAR SE A OPERCAO JA FOI ENVIADA
|          PERFORM 1900-VERIFICAR-PROTOCOLO
|
|          EVALUATE WRK-ACHOU-SOLTC
|              WHEN 'S'
|     *             LER PROXIMO REGISTRO
|     *             MENSAGEM DE PROTOCOLO EM PROCESSAMENTO NA CIP
|                   MOVE 'PROTOCOLO EM PROCESSAMENTO NA REGISTRADORA'
|                                      TO WRK-OUT-MENSA-DCTM
|                                         WRK-MENSAGEM-ERRO
|              WHEN 'N'
F2404               PERFORM 1950-ACESSAR-BVVEA520
F2404
F2404               IF  BVVEFX-S-IND-CLI-MASSV  EQUAL 'S'
F2404                   PERFORM 1925-ACESSAR-BVVEA504
F2404               ELSE
|                       PERFORM UNTIL WRK-FIM
|                                          EQUAL 'S'
|                          PERFORM 1920-INCLUIR-TITULO-DESPR
|                          PERFORM 1925-ACESSAR-BVVEA504
|                          IF BVVEND-S-COD-RETORNO  NOT EQUAL ZEROS
|                             GO TO 1800-99-FIM
|                          END-IF
|                       END-PERFORM
F2404               END-IF
|     *             MENSAGEM DE PROTOCOLO EM PROCESSAMENTO NA CIP
|                   MOVE 'PROTOCOLO EM PROCESSAMENTO NA REGISTRADORA'
|                                      TO WRK-OUT-MENSA-DCTM
|                                         WRK-MENSAGEM-ERRO
|          END-EVALUATE.
|
|     *----------------------------------------------------------------*
|      1800-99-FIM.                    EXIT.
|     *----------------------------------------------------------------*
|
|     *----------------------------------------------------------------*
|     *        VERIFICAR SE JA EXISTE PROTOCOLO PARA OPERACAO          *
|     *----------------------------------------------------------------*
|      1900-VERIFICAR-PROTOCOLO        SECTION.
|     *----------------------------------------------------------------*
|     *
|          INITIALIZE                  DCOMZD-REGISTRO
|     *
|          MOVE 'E'                    TO DCOMZD-E-CTPO-PROT-AGNDA
|          MOVE WRK-INP-DANO-DCTM(IND-2)
|                                      TO DCOMZD-E-DANO-OPER-DESC
|          MOVE WRK-INP-NSEQ-DCTM(IND-2)
|                                      TO DCOMZD-E-NSEQ-OPER-DESC
|          MOVE ZEROS                  TO DCOMZD-E-NSMULA-OPER-DESC
|     *
|          MOVE 'DCOM5922'             TO WRK-MODULO
|     *
|          CALL WRK-MODULO             USING DCOMZD-REGISTRO
|                                            ERRO-AREA
|                                            WRK-SQLCA
|     *
|          EVALUATE DCOMZD-S-COD-RETORNO
|             WHEN 00
|                  IF (DCOMZD-S-CSIT-PROT-AGNDA
|                                      NOT GREATER 5)
|                      MOVE 'S'        TO WRK-ACHOU-SOLTC
|                  ELSE
|                      MOVE 'N'        TO WRK-ACHOU-SOLTC
|                  END-IF
|             WHEN 08
|                  MOVE 'N'            TO WRK-ACHOU-SOLTC
|             WHEN 99
|                  MOVE 'DB2'          TO ERR-TIPO-ACESSO
|                  MOVE DCOMZD-S-MSG-RETORNO(1:79)
|                                      TO WRK-OUT-MENSA-DCTM
|                                         WRK-MENSAGEM-ERRO
|                  PERFORM 9999-PROCESSAR-ROTINA-ERRO
|             WHEN OTHER
|                  MOVE 'APL'          TO ERR-TIPO-ACESSO
|                  MOVE DCOMZD-S-MSG-RETORNO(1:79)
|                                      TO WRK-OUT-MENSA-DCTM
|                                         WRK-MENSAGEM-ERRO
WIP001                                    ERR-TEXTO
|                  PERFORM 9999-PROCESSAR-ROTINA-ERRO
|          END-EVALUATE
|          .
|     *
|     *----------------------------------------------------------------*
|      1900-99-FIM.                    EXIT.
|     *----------------------------------------------------------------*
|     *
|     *----------------------------------------------------------------*
|      1910-ACESSAR-DCOM4174           SECTION.
|     *----------------------------------------------------------------*
|     *
|          INITIALIZE                  4174-ENTRADA-ROTEADOR
|                                      4174-SAIDA-ROTEADOR
|     *
|          MOVE WRK-COD-USER-R         TO 4174-ENT-CFUNC-BDSCO
|          MOVE LNK-IO-LTERM           TO 4174-ENT-CTERM
|          MOVE WRK-INP-DANO-DCTM(IND-2)
|                                      TO 4174-ENT-DANO-OPER-DESC
|          MOVE WRK-INP-NSEQ-DCTM(IND-2)
|                                      TO 4174-ENT-NSEQ-OPER-DESC
|     *
|          MOVE 'DCOM4174'             TO WRK-MODULO
|     *
|          CALL WRK-MODULO             USING 4174-ENTRADA-ROTEADOR
|                                            4174-SAIDA-ROTEADOR
|                                            ERRO-AREA
|                                            WRK-SQLCA
|
|          EVALUATE 4174-SAI-COD-RETORNO
|              WHEN '0000'
|                  MOVE SPACES         TO WRK-INCONSISTENCIA
|              WHEN '0099'
|                  MOVE 'DB2'          TO ERR-TIPO-ACESSO
|                  MOVE 4174-SAI-MSG-RETORNO
|                                      TO WRK-OUT-MENSA-DCTM
|                                         WRK-MENSAGEM-ERRO
|                  PERFORM 9999-PROCESSAR-ROTINA-ERRO
|              WHEN OTHER
|                  MOVE 4174-SAI-MSG-RETORNO
|                                      TO WRK-OUT-MENSA-DCTM
|                                         WRK-MENSAGEM-ERRO
|                  MOVE 'APL'          TO ERR-TIPO-ACESSO
|                  PERFORM 9999-PROCESSAR-ROTINA-ERRO
|          END-EVALUATE
|     *
CAP002     IF 4174-SAI-CTPO-DESC-COML  NOT EQUAL 4
|             MOVE 'N'                 TO WRK-FLAG
|          ELSE
|             MOVE 'S'                 TO WRK-FLAG
CAP002     END-IF
|          .
|     *
|     *----------------------------------------------------------------*
|      1910-99-FIM.                    EXIT.
|     *----------------------------------------------------------------*
|
|     *----------------------------------------------------------------*
|     *     TRATAMENTO PARA TITULOS DESPREZADOS
|     *----------------------------------------------------------------*
|      1920-INCLUIR-TITULO-DESPR       SECTION.
|     *----------------------------------------------------------------*
|
|          INITIALIZE G8-ARGUMENTOS-ENTRADA
|                     G8-RETORNO.
|
|          MOVE WRK-INP-DANO-DCTM(IND-2)
|                                      TO G8-DANO-OPER-DESC
|          MOVE WRK-INP-NSEQ-DCTM(IND-2)
|                                      TO G8-NSEQ-OPER-DESC
|          MOVE WRK-DESPREZAR          TO G8-QTDE-A-DESPREZAR
|          MOVE 'PARCIAL-02'           TO G8-INSTRUCAO
|          MOVE 'DCOM7341'             TO WRK-MODULO
|
|          CALL  WRK-MODULO            USING G8-ARGUMENTOS-ENTRADA
|                                            G8-RETORNO
|                                            ERRO-AREA
|                                            WRK-SQLCA
|
|          IF G8-COD-RETORNO           NOT EQUAL ZEROS AND 2 AND 3
|             MOVE G8-MENSAGEM         TO WRK-OUT-MENSA-DCTM
|                                         WRK-MENSAGEM-ERRO
|             PERFORM 2000-FINALIZAR
|          ELSE
|             IF G8-COD-RETORNO        EQUAL  2
CAP003        OR G8-QTDE-RETORNADA     GREATER 40
CAP003           ADD 40                TO WRK-DESPREZAR
|                MOVE 'N'              TO WRK-FIM
|             ELSE
|                MOVE ZEROS            TO WRK-DESPREZAR
|                MOVE 'S'              TO WRK-FIM
|             END-IF
|          END-IF.
|
|     *----------------------------------------------------------------*
|      1920-99-FIM.                    EXIT.
|     *----------------------------------------------------------------*
|
|     *----------------------------------------------------------------*
|     *           ACESAR BVVEA504 PARA SOLICITAR EFETIVACAO            *
|     *----------------------------------------------------------------*
|      1925-ACESSAR-BVVEA504           SECTION.
|     *----------------------------------------------------------------*
|     *
|          INITIALIZE                  BVVEND-COMMAREA
|                                      WRK-CONTRATO-BVVE
|     *
|          MOVE WRK-PROTOCOLO          TO BVVEND-E-PROTOCOLO
|          MOVE 4174-SAI-CAG-BCRIA     TO BVVEND-E-AGENCIA
|          MOVE 4174-SAI-CCTA-BCRIA-CLI
|                                      TO BVVEND-E-CONTA
|
|          EVALUATE LNK-IO-LTERM
|             WHEN 'TELEBCO'
|                  MOVE 'TELE'         TO BVVEND-E-CANAL
|             WHEN 'NETEMPR'
|                  MOVE 'NETE'         TO BVVEND-E-CANAL
|             WHEN 'MOBILE'
|                  MOVE 'MOBI'         TO BVVEND-E-CANAL
|             WHEN OTHER
|                  MOVE 'AGEN'         TO BVVEND-E-CANAL
|          END-EVALUATE
|     *
|          MOVE 'DCOM'                 TO WRK-SIGLA-CCUSTO
|          MOVE WRK-INP-OPERA-DCTM-X(IND-2)
|                                      TO WRK-OPER-BVVE
|          MOVE WRK-CONTRATO-BVVE      TO BVVEND-E-CONTRATO
|          MOVE 4174-SAI-DVCTO-FNAL-OPER
|                                      TO BVVEND-E-DT-CONT
|          MOVE 4174-SAI-VOPER-DESC-COML
|                                      TO BVVEND-E-VLR-TOT
|          MOVE 'I'                    TO BVVEND-E-IND-I-A

F2404 * ---------------------------------------------------------------*
F2404 * CHAMADA PARA CLIENTES MASSIVOS SERA UNICA E SEM PREENCHIMENTO
F2404 * DAS OCORRENCIAS - IND-CLI-MASSV 'G' = TERMINAL GERENCIAL
F2404
F2404      IF  BVVEFX-S-IND-CLI-MASSV  EQUAL 'S'
F2404          MOVE 'S'                TO BVVEND-E-FIM-OCOR
F2404          MOVE 'G'                TO BVVEND-E-IND-ORIG-MASSV
F2404          MOVE WRK-INP-DANO-DCTM(IND-2)
F2404                                  TO BVVEND-E-OPER-ANO
F2404          MOVE WRK-INP-NSEQ-DCTM(IND-2)
F2404                                  TO BVVEND-E-OPER-SEQ
F2404      ELSE
               IF WRK-DESPREZAR        EQUAL ZEROS
                  MOVE 'S'             TO BVVEND-E-FIM-OCOR
               ELSE
                  MOVE SPACES          TO BVVEND-E-FIM-OCOR
               END-IF

               PERFORM VARYING IND-3   FROM 1 BY 1
CAP003                   UNTIL IND-3   GREATER 40
                            OR IND-3   GREATER G8-QTDE-RETORNADA
                  MOVE G8RT-DVCTO-TITLO-DESC(IND-3)
                                       TO BVVEND-E-DATA(IND-3)
                  MOVE G8RT-CADM-CATAO-CREDT(IND-3)
                                       TO HX-NUM-09-CS
                  MOVE HX-NUM-09-SS    TO HX-NRO-18
                  MOVE HX-NRO-05       TO BVVEND-E-PRODUTO(IND-3)
                  MOVE G8RT-VTITLO-DESC-COML(IND-3)
                                       TO BVVEND-E-VLR-DESCONTO(IND-3)
               END-PERFORM
F2404      END-IF.
|     *
|          MOVE 'BVVEA504'             TO WRK-MODULO
|     *
|          CALL WRK-MODULO             USING BVVEND-COMMAREA
|     *
|          EVALUATE BVVEND-S-COD-RETORNO
|              WHEN 00
CAP003              IF WRK-DESPREZAR   EQUAL ZEROS
 |                     IF BVVEND-S-PROTOCOLO
 |                                     EQUAL SPACES OR LOW-VALUES
 |                        MOVE SPACES  TO BVVEND-S-PROTOCOLO
 |                        MOVE 'S'     TO WRK-FLAG-ERRO
 |                        MOVE 'PROTOCOLO RETORNADO VAZIO DO BVVEA504'
 |                                     TO WRK-OUT-MENSA-DCTM
 |                                        WRK-MENSAGEM-ERRO
 |                        MOVE 08      TO BVVEND-S-COD-RETORNO
 |                     END-IF
 |                     MOVE BVVEND-S-PROTOCOLO
 |                                     TO WRK-PROTOCOLO
 |                     MOVE BVVEND-S-TMP-ESPERA
 |                                     TO WRK-TEMPO-ESPERA-X
 |                     MOVE 'I'        TO WRK-ACAO
 |                     MOVE 2          TO WRK-SITUACAO
 |                     MOVE 1          TO WRK-TEMPO-ESPERA
 |                     PERFORM 1930-INSERIR-CPROT-CANAL
 |                  END-IF
 |                  MOVE BVVEND-S-PROTOCOLO
CAP003                                 TO WRK-PROTOCOLO
|              WHEN 08
CAP004*        ERRO DE CONSISTENCIA
|                   MOVE 'S'            TO  WRK-FLAG-ERRO
CAP004              MOVE BVVEND-S-MSG-RETORNO
|                                      TO WRK-OUT-MENSA-DCTM
|                                         WRK-MENSAGEM-ERRO
|              WHEN 10
|                   MOVE 'S'            TO  WRK-FLAG-ERRO
|                   MOVE 'EFETIVACAO DE AGENDA NA REGISTRADORA FORA DO H
|     -                  'ORARIO'
|                                      TO WRK-OUT-MENSA-DCTM
|                                         WRK-MENSAGEM-ERRO
|              WHEN 16
|                   MOVE 'S'            TO  WRK-FLAG-ERRO
|                   MOVE BVVEND-S-MSG-RETORNO
|                                      TO WRK-OUT-MENSA-DCTM
|                                         WRK-MENSAGEM-ERRO
|              WHEN OTHER
|                   MOVE 'S'            TO  WRK-FLAG-ERRO
|                   MOVE BVVEND-S-MSG-RETORNO
|                                      TO WRK-OUT-MENSA-DCTM
|                                         WRK-MENSAGEM-ERRO
|                                         ERR-TEXTO
|                   MOVE 'APL'         TO ERR-TIPO-ACESSO
|                   PERFORM 9999-PROCESSAR-ROTINA-ERRO
|          END-EVALUATE
|          .
|     *
|     *----------------------------------------------------------------*
|      1925-99-FIM.                    EXIT.
|     *----------------------------------------------------------------*
|
|     *----------------------------------------------------------------*
|     *    INSERIR OU ATUALIZAR A SITUACAO DO PROTOCOLO NA BASE        *
|     *----------------------------------------------------------------*
|      1930-INSERIR-CPROT-CANAL        SECTION.
|     *----------------------------------------------------------------*
|     *
|          INITIALIZE                  DCOMZH-REGISTRO
|     *
|     **-->(1)EM PROCESSAMENTO
|     **-->(2)PENDENTE
|     **-->(3)PROCESSADO
|     **-->(4)SALDO INDISPONIVEL
|     **-->(5)REJEITADO
|     **-->(6)ERRO DE COMUNICACAO
|     **-->(7)FORA DA GRADE
|     **-->(8)CANCELADO
|     *
|          EVALUATE WRK-SITUACAO
|              WHEN 1
|              WHEN 2
|                   MOVE 'EM LIBER'    TO WRK-OUT-SITANAL-DCTM(IND-2)
|              WHEN 3
|                   CONTINUE
|              WHEN 4
|                   MOVE 'VLR INDISP'  TO WRK-OUT-SITANAL-DCTM(IND-2)
|              WHEN OTHER
|                   MOVE 'REJEIT CIP'  TO WRK-OUT-SITANAL-DCTM(IND-2)
|          END-EVALUATE
|     *
|          MOVE WRK-SITUACAO           TO DCOMZH-E-CSIT-PROT-AGNDA
|          MOVE WRK-ACAO               TO DCOMZH-E-ACAO
|          MOVE 4174-SAI-CBCO          TO DCOMZH-E-CBCO
|          MOVE 4174-SAI-CAG-BCRIA     TO DCOMZH-E-CAG-BCRIA
|          MOVE 4174-SAI-CCTA-BCRIA-CLI
|                                      TO DCOMZH-E-CCTA-BCRIA-CLI
|          MOVE WRK-PROTOCOLO          TO DCOMZH-E-CPROT-SOLTC-AGNDA
|          MOVE 'E'                    TO DCOMZH-E-CTPO-PROT-AGNDA
|          MOVE 1                      TO DCOMZH-E-QTEMPO-VALDD-PROCM
|          MOVE 4174-SAI-NSMULA-OPER-DESC
|                                      TO DCOMZH-E-NSMULA-OPER-DESC
|          MOVE WRK-INP-DANO-DCTM(IND-2)
|                                      TO DCOMZH-E-DANO-OPER-DESC
|          MOVE WRK-INP-NSEQ-DCTM(IND-2)
|                                      TO DCOMZH-E-NSEQ-OPER-DESC
|     *
|          EVALUATE LNK-IO-LTERM
|              WHEN 'TELEBCO'
|              WHEN 'NETEMPR'
|              WHEN 'MOBILE'
|                   MOVE LNK-IO-LTERM  TO DCOMZH-E-CCANAL-DESC
|              WHEN OTHER
|                   MOVE 'AGENCIA'     TO DCOMZH-E-CCANAL-DESC
|          END-EVALUATE
|     *
|          MOVE WRK-TEMPO-ESPERA       TO DCOMZH-E-QTEMPO-PROCM-CANAL
|     *
|          MOVE 'DCOM5923'             TO WRK-MODULO
|     *
|          CALL WRK-MODULO             USING DCOMZH-REGISTRO
|                                            ERRO-AREA
|                                            WRK-SQLCA
|     *
|          EVALUATE DCOMZH-S-COD-RETORNO
|             WHEN ZEROS
|                  CONTINUE
WIP001        WHEN 99
|                  MOVE 'DB2'          TO ERR-TIPO-ACESSO
WIP001             MOVE DCOMZH-S-MSG-RETORNO(1:79)
|                                      TO WRK-OUT-MENSA-DCTM
|                                         WRK-MENSAGEM-ERRO
|                  PERFORM 9999-PROCESSAR-ROTINA-ERRO
|             WHEN OTHER
|                  MOVE 'APL'          TO ERR-TIPO-ACESSO
WIP001             MOVE DCOMZH-S-MSG-RETORNO(1:79)
|                                      TO WRK-OUT-MENSA-DCTM
|                                         WRK-MENSAGEM-ERRO
|                                         ERR-TEXTO
|                  PERFORM 9999-PROCESSAR-ROTINA-ERRO
|          END-EVALUATE
|          .
|     *
|     *----------------------------------------------------------------*
|      1930-99-FIM.                    EXIT.
CAP001*----------------------------------------------------------------*

F2404 *----------------------------------------------------------------*
INICIO 1950-ACESSAR-BVVEA520           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE BVVEFX-COMMAREA

           MOVE 4174-SAI-CAG-BCRIA     TO BVVEFX-E-AGENCIA
           MOVE 4174-SAI-CCTA-BCRIA-CLI
                                       TO BVVEFX-E-CONTA

           MOVE 'BVVEA520'             TO WRK-MODULO
           CALL WRK-MODULO             USING BVVEFX-COMMAREA

           EVALUATE BVVEFX-S-COD-RETORNO
               WHEN 00
                    CONTINUE

               WHEN OTHER
                    MOVE 'N'           TO BVVEFX-S-IND-CLI-MASSV
                    MOVE BVVEFX-S-MSG-RETORNO
                                       TO WRK-OUT-MENSA-DCTM
                                          WRK-MENSAGEM-ERRO
           END-EVALUATE.

      *----------------------------------------------------------------*
FIM    1950-99-FIM.                    EXIT.
F2404 *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA FINALIZA O PROCESSAMENTO DO PROGRAMA              *
      *----------------------------------------------------------------*
       2000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

DTS001*----------------------------------------------------------------*
DTS001 8888-EXECUTAR-ROLLBACK          SECTION.
DTS001*----------------------------------------------------------------*
DTS001
DTS001      CALL 'CBLTDLI'             USING WRK-ROLB
DTS001                                       LNK-IO-PCB.
DTS001
DTS001*----------------------------------------------------------------*
DTS001 8888-99-FIM.                    EXIT.
DTS001*----------------------------------------------------------------*
DTS001
      *----------------------------------------------------------------*
      *  ESSA ROTINA FORMATA E EMITE A MENSAGEM DE ERRO DO PROGRAMA    *
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           IF  ERR-MODULO              EQUAL SPACES AND LOW-VALUES
               MOVE 'SENHAS02'         TO ERR-PGM
               MOVE 'DCOM0403'         TO ERR-MODULO
           ELSE
               MOVE 'DCOM0403'         TO ERR-PGM
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
