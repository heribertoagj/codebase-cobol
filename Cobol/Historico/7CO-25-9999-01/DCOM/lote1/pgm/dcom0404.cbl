      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. DCOM0404.
       AUTHOR.     MARCELO MORINA.
      *================================================================*
      *                          A L T R A N                           *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   DCOM0404                                    *
      *    PROGRAMADOR.:   MARCELO MORINA MARQUES                      *
      *    ANALISTA....:   MARCELO MORINA MARQUES                      *
      *    DATA........:   05/02/2007                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   LIBERAR OPERACAO - OPERACOES POR CLIENTE.   *
      *----------------------------------------------------------------*
      *    TELAS.......:                                               *
      *    DCOMDDTM - LIBERAR OPERACAO - OPERACOES POR CLIENTE.        *
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
      *================================================================*
DTS001*                    ALTERACAO - DTS CONSULTING                  *
DTS001*----------------------------------------------------------------*
DTS001*    PROGRAMADORA:   LUCIANDRA SILVEIRA - DTS CONSULTING         *
DTS001*    ANALISTA....:   LUCIANDRA SILVEIRA - DTS CONSULTING         *
DTS001*    ANALISTA DTS:   VERA MARIA POLLINI - BRADESCO - GRUPO 70    *
DTS001*    DATA........:   05/02/2010                                  *
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
|     *                 ACESSAR BVVEA504 E BVVEA505                    *
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
CAP002*                 DIAS 03 E 06/06/2021 - NORMAT 4734             *
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
|     *                 PELO BVVE                                      *
CAP004*================================================================*
CAP005*================================================================*
|     *                   ALTERACAO - CAPGEMINI                        *
|     *----------------------------------------------------------------*
|     *   PROGRAMADOR :   ANDREA             - CAPGEMINI               *
|     *   ANALISTA DDS:   RICARDO JAMMAL     - BRADESCO - GRUPO 39     *
|     *   DATA........:   21/07/2021                                   *
|     *----------------------------------------------------------------*
|     *   OBJETIVO....: ALTERAR MENSAGEM DE ERRO DE CONSISTENIA NO     *
|     *                 'PROTOCOLO EM PROCESSAMENTO NA REGISTRADORA'   *
|     *                 PARA 'OPERACAO INDISPONIVEL, LIBERACAO EM      *
|     *                 PROCESSAMENTO NA CIP'                          *
CAP005*================================================================*
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
           '*** DCOM0404 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ATRIBUTOS ***'.
      *---------------------------------------------------------------*

      *--- ALPHA    - NORMAL    - PROTEGIDO    - NAO CURSOR - ALFA ---*
       01  WRK-225                     PIC S9(008) COMP    VALUE +225.
      *--- NUMERICO - BRILHANTE - DESPROTEGIDO - POS CURSOR - NUM  ---*
       01  WRK-49369                   PIC S9(008) COMP    VALUE +49369.
      *--- BRILHANTE, NORMAL    - DEPROTEGIDO  - POS CURSOR - ALFA ---*
       01  WRK-49353                   PIC S9(008) COMP    VALUE +49353.
      *--- ALPHA    - NORMAL    - DEPROTEGIDO  - POS CURSOR - ALFA ---*
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
           05  WRK-AGENCIA-AUX.
               10  WRK-AGENCIA-NUM     PIC  9(005)         VALUE ZEROS.
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
FM0510 01  WRK-CONTRATO-BVVE.
.          05 WRK-SIGLA-CCUSTO         PIC  X(004)         VALUE SPACES.
.          05 WRK-OPER-BVVE            PIC  X(013)         VALUE SPACES.
FM0510     05 FILLER                   PIC  X(002)         VALUE SPACES.
      *
CAP002 01 WRK-FLAG                     PIC  X(001)         VALUE SPACES.
      *
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
           '*** AREA DA TELA DCOMDDTM  - INPUT ***'.
      *----------------------------------------------------------------*
       01  WRK-INPUT-DDTM.
           05  FILLER                  PIC  X(018) VALUE SPACES.
           05  WRK-INP-PFK-DDTM        PIC  X(001) VALUE SPACES.
           05  WRK-INP-SENHAS-DDTM     PIC  X(037) VALUE SPACES.
           05  WRK-INP-COMANDO-DDTM    PIC  X(068) VALUE SPACES.
           05  WRK-INP-DADOS-DDTM.
               10  WRK-INP-FIMAMOS-DDTM PIC  X(001) VALUE SPACES.
               10  WRK-INP-FASE-DDTM    PIC  X(001) VALUE SPACES.
               10  WRK-INP-TMSTAMP-DDTM PIC  X(026) VALUE SPACES.
               10  WRK-INP-PAGINA-DDTM-X.
                   15  WRK-INP-PAGINA-DDTM
                                        PIC  9(003) VALUE ZEROS.
               10  WRK-INP-NOME-DDTM    PIC  X(040) VALUE SPACES.
               10  WRK-INP-LITCPF-DDTM  PIC  X(009) VALUE SPACES.
FS2511         10  WRK-INP-CNPJCPF-DDTM.
FS2511*            15  WRK-INP-NUMERO-DDTM
FS2511*                                 PIC  9(009) VALUE ZEROS.
FS2511             15  WRK-INP-NUMERO-DDTM
FS2511                                  PIC  X(009) VALUE SPACES.
FS2511             15  WRK-INP-FILLER-1 PIC  X(001) VALUE SPACES.
FS2511*            15  WRK-INP-FILIAL-DDTM
FS2511*                                 PIC  9(004) VALUE ZEROS.
FS2511             15  WRK-INP-FILIAL-DDTM
FS2511                                        PIC  X(004) VALUE SPACES.
                   15  WRK-INP-FILLER-2 PIC  X(001) VALUE SPACES.
                   15  WRK-INP-CONTROLE-DDTM
                                        PIC  9(002) VALUE ZEROS.
               10  WRK-INP-LITBCO-DDTM  PIC  X(006) VALUE SPACES.
               10  WRK-INP-BANCO-DDTM-X.
                   15  WRK-INP-BANCO-DDTM
                                        PIC  9(003) VALUE ZEROS.
               10  WRK-INP-LITAGE-DDTM  PIC  X(008) VALUE SPACES.
               10  WRK-INP-AGENCIA-DDTM.
                   15  WRK-INP-CODAGE-DDTM
                                        PIC  9(004) VALUE ZEROS.
                   15  WRK-INP-FILLER-3 PIC  X(001) VALUE SPACES.
                   15  WRK-INP-DESCAGE-DDTM
                                        PIC  X(021) VALUE SPACES.
               10  WRK-INP-LITCTA-DDTM  PIC  X(009) VALUE SPACES.
               10  WRK-INP-CONTA-DDTM-X.
                   15  WRK-INP-CONTA-DDTM
                                        PIC  9(013) VALUE ZEROS.
               10  WRK-INP-TABELA-DDTM    OCCURS  04  TIMES.
                   15  WRK-INP-SELEC-DDTM     PIC  X(001) VALUE SPACES.
                   15  WRK-INP-OPERA-DDTM-X.
                       20  WRK-INP-OPERA-DDTM PIC  9(013) VALUE ZEROS.
DTS002                 20  FILLER REDEFINES   WRK-INP-OPERA-DDTM.
DTS002                     25  WRK-INP-DANO-DDTM   PIC  9(004).
DTS002                     25  WRK-INP-NSEQ-DDTM   PIC  9(009).
                   15  WRK-INP-PROD-DDTM      PIC  X(012) VALUE SPACES.
                   15  WRK-INP-SUBPROD-DDTM   PIC  X(010) VALUE SPACES.
                   15  WRK-INP-DTOPER-DDTM    PIC  X(010) VALUE SPACES.
                   15  WRK-INP-VALOR-DDTM.
                       20  WRK-INP-VALOR-DDTM-N PIC  ZZ.ZZZ.ZZZ.ZZ9,99.
                   15  WRK-INP-MEIOENT-DDTM   PIC  X(015) VALUE SPACES.
                   15  WRK-INP-SITANAL-DDTM   PIC  X(010) VALUE SPACES.
                   15  WRK-INP-PARECER-DDTM   PIC  X(003) VALUE SPACES.
                   15  WRK-INP-TIPOLIB-DDTM   PIC  X(001) VALUE SPACES.
                   15  WRK-INP-VLRLIQ-DDTM    PIC  9(15)V99 VALUE ZEROS.
           05  WRK-INP-MENSA-DDTM       PIC  X(079) VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                       PIC  X(050)         VALUE
           '*** AREA DA TELA DCOMDDTM  - OUTPUT ***'.
      *----------------------------------------------------------------*
       01  WRK-OUTPUT-DDTM.
           05  WRK-OUT-LL-DDTM          PIC S9(004) COMP VALUE +0796.
           05  WRK-OUT-ZZ-DDTM          PIC  9(004) COMP VALUE ZEROS.
           05  WRK-OUT-SENHAS-DDTM      PIC  X(037) VALUE SPACES.
           05  WRK-OUT-COMANDO-ATTR-DDTM PIC 9(004) COMP VALUE ZEROS.
           05  WRK-OUT-COMANDO-DDTM     PIC  X(068) VALUE SPACES.
           05  WRK-OUT-DADOS-DDTM.
               10  WRK-OUT-FIMAMOS-DDTM PIC  X(001) VALUE SPACES.
               10  WRK-OUT-FASE-DDTM    PIC  X(001) VALUE SPACES.
               10  WRK-OUT-TMSTAMP-DDTM PIC  X(026) VALUE SPACES.
               10  WRK-OUT-PAGINA-DDTM-X.
                   15  WRK-OUT-PAGINA-DDTM
                                        PIC  9(003) VALUE ZEROS.
               10  WRK-OUT-NOME-DDTM    PIC  X(040) VALUE SPACES.
               10  WRK-OUT-LITCPF-DDTM  PIC  X(009) VALUE SPACES.
FS2511         10  WRK-OUT-CNPJCPF-DDTM.
FS2511*            15  WRK-OUT-NUMERO-DDTM
FS2511*                                       PIC  9(009) VALUE ZEROS.
FS2511             15  WRK-OUT-NUMERO-DDTM
FS2511                                       PIC  X(009) VALUE SPACES.
FS2511             15  WRK-OUT-FILLER-1 PIC  X(001) VALUE SPACES.
FS2511*            15  WRK-OUT-FILIAL-DDTM
FS2511*                                       PIC  9(004) VALUE ZEROS.
FS2511             15  WRK-OUT-FILIAL-DDTM
FS2511                                       PIC  X(004) VALUE SPACES.
                   15  WRK-OUT-FILLER-2 PIC  X(001) VALUE SPACES.
                   15  WRK-OUT-CONTROLE-DDTM
                                        PIC  9(002) VALUE ZEROS.
               10  WRK-OUT-LITBCO-DDTM  PIC  X(006) VALUE SPACES.
               10  WRK-OUT-BANCO-DDTM-X.
                   15  WRK-OUT-BANCO-DDTM
                                        PIC  9(003) VALUE ZEROS.
               10  WRK-OUT-LITAGE-DDTM  PIC  X(008) VALUE SPACES.
               10  WRK-OUT-AGENCIA-DDTM.
                   15  WRK-OUT-CODAGE-DDTM
                                        PIC  9(004) VALUE ZEROS.
                   15  WRK-OUT-FILLER-3 PIC  X(001) VALUE SPACES.
                   15  WRK-OUT-DESCAGE-DDTM
                                        PIC  X(021) VALUE SPACES.
               10  WRK-OUT-LITCTA-DDTM  PIC  X(009) VALUE SPACES.
               10  WRK-OUT-CONTA-DDTM-X.
                   15  WRK-OUT-CONTA-DDTM
                                        PIC  9(013) VALUE ZEROS.
               10  WRK-OUT-TABELA-DDTM    OCCURS  04  TIMES.
                   15  WRK-OUT-SELEC-ATTR-DDTM PIC  9(04) COMP.
                   15  WRK-OUT-SELEC-DDTM      PIC  X(001) VALUE SPACES.
                   15  WRK-OUT-OPERA-DDTM-X.
                       20  WRK-OUT-OPERA-DDTM  PIC  9(013) VALUE ZEROS.
                   15  WRK-OUT-PROD-DDTM       PIC  X(012) VALUE SPACES.
                   15  WRK-OUT-SUBPROD-DDTM    PIC  X(010) VALUE SPACES.
                   15  WRK-OUT-DTOPER-DDTM     PIC  X(010) VALUE SPACES.
                   15  WRK-OUT-VALOR-DDTM-X.
                       20  WRK-OUT-VALOR-DDTM  PIC  ZZ.ZZZ.ZZZ.ZZ9,99.
                   15  WRK-OUT-MEIOENT-DDTM    PIC  X(015) VALUE SPACES.
                   15  WRK-OUT-SITANAL-DDTM    PIC  X(010) VALUE SPACES.
                   15  WRK-OUT-PARECER-DDTM    PIC  X(003) VALUE SPACES.
                   15  WRK-OUT-TIPOLIB-DDTM    PIC  X(001) VALUE SPACES.
                   15  WRK-OUT-VLRLIQ-DDTM     PIC 9(15)V99 VALUE ZEROS.
               10  WRK-OUT-MENSA-DDTM   PIC  X(079) VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA TELA DCOMDDTM  - BRAD0660 ***'.
      *----------------------------------------------------------------*
       01  WRK-660-DCOMDDTM.
           05  WRK-660-LL-AREA-DDTM     PIC  9(004) COMP   VALUE 0124.
           05  WRK-660-LL-MENSAGEM-DDTM PIC  9(004) COMP   VALUE 0796.
           05  WRK-660-SENHAS-DDTM      PIC  9(004) COMP   VALUE 0037.
           05  WRK-660-COMANDO-DDTM     PIC  9(004) COMP   VALUE 2070.
           05  WRK-660-FIMAMOS-DDTM     PIC  9(004) COMP   VALUE 0001.
           05  WRK-660-FASE-DDTM        PIC  9(004) COMP   VALUE 0001.
           05  WRK-660-TMSTAMP-DDTM     PIC  9(004) COMP   VALUE 0026.
           05  WRK-660-PAGINA-DDTM      PIC  9(004) COMP   VALUE 0003.
           05  WRK-660-NOME-DDTM        PIC  9(004) COMP   VALUE 0040.
           05  WRK-660-LITCPF-DDTM      PIC  9(004) COMP   VALUE 0009.
           05  WRK-660-CNPJCPF-DDTM     PIC  9(004) COMP   VALUE 0017.
           05  WRK-660-LITBCO-DDTM      PIC  9(004) COMP   VALUE 0006.
           05  WRK-660-BANCO-DDTM       PIC  9(004) COMP   VALUE 0003.
           05  WRK-660-LITAGE-DDTM      PIC  9(004) COMP   VALUE 0008.
           05  WRK-660-AGENCIA-DDTM     PIC  9(004) COMP   VALUE 0026.
           05  WRK-660-LITCTA-DDTM      PIC  9(004) COMP   VALUE 0009.
           05  WRK-660-CONTA-DDTM       PIC  9(004) COMP   VALUE 0013.
           05  WRK-660-TABELA-DDTM    OCCURS  04  TIMES.
               10  WRK-660-SELEC-DDTM   PIC  9(004) COMP   VALUE 2003.
               10  WRK-660-OPERA-DDTM   PIC  9(004) COMP   VALUE 0013.
               10  WRK-660-PROD-DDTM    PIC  9(004) COMP   VALUE 0012.
               10  WRK-660-SUBPROD-DDTM PIC  9(004) COMP   VALUE 0010.
               10  WRK-660-DTOPER-DDTM  PIC  9(004) COMP   VALUE 0010.
               10  WRK-660-VALOR-DDTM   PIC  9(004) COMP   VALUE 0017.
               10  WRK-660-MEIOENT-DDTM PIC  9(004) COMP   VALUE 0015.
               10  WRK-660-SITANAL-DDTM PIC  9(004) COMP   VALUE 0010.
               10  WRK-660-PARECER-DDTM PIC  9(004) COMP   VALUE 0003.
               10  WRK-660-TIPOLIB-DDTM PIC  9(004) COMP   VALUE 0001.
               10  WRK-660-VLRLIQ-DDTM  PIC  9(004) COMP   VALUE 0017.
           05  WRK-660-MENSA-DDTM       PIC  9(004) COMP   VALUE 0079.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(080)         VALUE
           '*** AREA DE RECEBIMENTO E RETORNO DO DCOM0402 ***'.
      *----------------------------------------------------------------*
       01  COMU-AREA-UN.
           05  COMU-LL-UN             PIC S9(004) COMP  VALUE +297.
           05  COMU-ZZ-UN             PIC S9(004) COMP  VALUE +0.
           05  COMU-TRANCODE-UN.
               10  COMU-TRANSACAO-UN  PIC  X(008)       VALUE SPACES.
               10  FILLER-UN          PIC  X(006)       VALUE SPACES.
               10  COMU-PFK-UN        PIC  X(001)       VALUE SPACES.
           05  COMU-SENHAS-UN         PIC  X(037)       VALUE SPACES.
           05  COMU-COMANDO-UN        PIC  X(068)       VALUE SPACES.
           05  COMU-DADOS-UN.
               10  COMU-OPCAO-UN-X.
                   15  COMU-OPCAO-UN  PIC  9(001)       VALUE ZEROS.
               10  COMU-OPER-UN       PIC  9(013)       VALUE ZEROS.
               10  COMU-AGENCIA-UN    PIC  9(005)       VALUE ZEROS.
               10  COMU-AGENC-UN-X.
                   15 COMU-AGENC-UN   PIC  9(005)       VALUE ZEROS.
               10  COMU-CONTA-UN      PIC  9(013)       VALUE ZEROS.
FS2511*        10  COMU-CNPJ-UN       PIC  9(009)       VALUE ZEROS.
FS2511         10  COMU-CNPJ-UN       PIC  X(009)       VALUE SPACES.
FS2511*        10  COMU-FILIAL-UN     PIC  9(004)       VALUE ZEROS.
FS2511         10  COMU-FILIAL-UN     PIC  X(004)       VALUE SPACES.
               10  COMU-DIG-UN        PIC  9(002)       VALUE ZEROS.
               10  COMU-OPERACAO-UN.
                 15 COMU-DANO-OPER-UN PIC  9(004)       VALUE ZEROS.
                 15 COMU-NSEQ-OPER-UN PIC  9(009)       VALUE ZEROS.
               10  COMU-TIMESTAMP-UN  PIC  X(026)       VALUE ZEROS.
               10  COMU-MENSAGEM-UN   PIC  X(079)       VALUE SPACES.
               10  COMU-PAGINA-UN     PIC  9(003)       VALUE ZEROS.
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
           03  WRK-5110E-TPO-LIBR         PIC  X(001).
           03  WRK-5110E-VLIBRC           PIC  9(015)V9(02).

       01  WRK-5110S-SAIDA.
           03  WRK-5110S-COD-RETORNO      PIC  X(004).
           03  WRK-5110S-MSG-RETORNO      PIC  X(079).
           03  WRK-5110S-RESTART          PIC  9(005).
           03  WRK-5110S-FLAG             PIC  9(001).
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
           '*** AREA DE COMUNICACAO COM O MODULO DCOM4711 **'.
      *----------------------------------------------------------------*
       01  WRK-4711E-ENTRADA.
           03  WRK-4711E-COD-RETORNO             PIC  X(04).
           03  WRK-4711E-MSG-RETORNO             PIC  X(79).
           03  WRK-4711E-RESTART                 PIC  9(05).
           03  WRK-4711E-FLAG                    PIC  X(01).
           03  WRK-4711E-START-COUNT             PIC  X(01).
           03  WRK-4711E-CFUNC-BDSCO             PIC  9(09).
           03  WRK-4711E-CTERM                   PIC  X(08).
           03  WRK-4711E-FLAG-FILTRO             PIC  X(02).
           03  WRK-4711E-CBCO                    PIC  9(03).
           03  WRK-4711E-CAG-BCRIA               PIC  9(05).
           03  WRK-4711E-CAG-OPER                PIC  9(05).
           03  WRK-4711E-CCTA-BCRIA-CLI          PIC  9(13).
           03  WRK-4711E-CPRODT                  PIC  9(03).
           03  WRK-4711E-CSPROD-DESC-COML        PIC  9(03).
           03  WRK-4711E-DINIC-OPER-DESC         PIC  X(10).
           03  WRK-4711E-CELMTO-DESC-COML        PIC  9(03).
FS2511*    03  WRK-4711E-CCNPJ-CPF               PIC  9(09).
FS2511     03  WRK-4711E-CCNPJ-CPF               PIC  X(09).
FS2511*    03  WRK-4711E-CFLIAL-CNPJ             PIC  9(04).
FS2511     03  WRK-4711E-CFLIAL-CNPJ             PIC  X(04).
           03  WRK-4711E-CCTRL-CNPJ-CPF          PIC  9(02).
           03  WRK-4711E-CSIT-DESC-COML          PIC  9(03).
           03  WRK-4711E-CINDCD-EVNTO-WF         PIC  9(03).
           03  WRK-4711E-CINDCD-SIT-F-EVNTO      PIC  9(03).

       01  WRK-4711S-SAIDA.
           03  WRK-4711S-COD-RETORNO                PIC  X(04).
           03  WRK-4711S-MSG-RETORNO                PIC  X(79).
           03  WRK-4711S-RESTART                    PIC  9(05).
           03  WRK-4711S-FLAG                       PIC  X(01).
           03  WRK-4711S-QTDE-RETORNADA             PIC  9(03).
           03  WRK-4711S-COUNT                      PIC  9(09).
           03  WRK-4711S-DADOS.
               05  WRK-4711S-TABELA-SAIDA    OCCURS 10 TIMES.
                   10  WRK-4711S-DANO-OPER-DESC     PIC  9(04).
                   10  WRK-4711S-NSEQ-OPER-DESC     PIC  9(09).
                   10  WRK-4711S-IPSSOA-DESC-COML   PIC  X(60).
                   10  WRK-4711S-CBCO               PIC  9(03).
                   10  WRK-4711S-CAG-BCRIA          PIC  9(05).
                   10  WRK-4711S-CAG-BCRIA-DESC     PIC  X(40).
                   10  WRK-4711S-CCTA-BCRIA-CLI     PIC  9(13).
                   10  WRK-4711S-CBCO-OPER-OPER     PIC  9(03).
                   10  WRK-4711S-CAG-OPER-OPER      PIC  9(05).
                   10  WRK-4711S-CSIT-DESC-COML     PIC  9(03).
                   10  WRK-4711S-IRSUMO-SIT-DESC    PIC  X(15).
                   10  WRK-4711S-CSGL-SIT-DESC      PIC  X(06).
                   10  WRK-4711S-CPRODT             PIC  9(03).
                   10  WRK-4711S-IABREV-PRODT       PIC  X(12).
                   10  WRK-4711S-CSGL-PRODT-DESC    PIC  X(06).
                   10  WRK-4711S-CSPROD-DESC-COML   PIC  9(03).
                   10  WRK-4711S-CTPO-DESC-COML     PIC  9(03).
                   10  WRK-4711S-IRSUMO-SPROD-DESC  PIC  X(10).
                   10  WRK-4711S-CSGL-SPROD-DESC    PIC  X(06).
                   10  WRK-4711S-VOPER-DESC-COML    PIC  9(15)V9(2).
                   10  WRK-4711S-DINIC-OPER-DESC    PIC  X(10).
                   10  WRK-4711S-DVCTO-FNAL-OPER    PIC  X(10).
                   10  WRK-4711S-CMEIO-ENTRD-DESC   PIC  9(03).
                   10  WRK-4711S-IMEIO-ENTRD-DESC   PIC  X(25).
                   10  WRK-4711S-IRSUMO-MEIO-ENTRD  PIC  X(15).
                   10  WRK-4711S-CSGL-MEIO-ENTRD    PIC  X(06).
                   10  WRK-4711S-CINDCD-LIBRC-ANTCP PIC  X(01).
                   10  WRK-4711S-PARECER-FORML      PIC  X(01).
                   10  WRK-4711S-VLIQ-OPER-DESC     PIC  9(15)V9(2).

           03  WRK-4711S-CONSISTENCIA   REDEFINES   WRK-4711S-DADOS.
               05  WRK-4711S-TAMANHO-ERRO           PIC  9(01).
               05  WRK-4711S-TABELA-ERRO   OCCURS   2    TIMES.
                   10  WRK-4711S-CAMPOS-ERRO        PIC  9(01).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO COM O MODULO DCOM4712 **'.
      *----------------------------------------------------------------*
       01  WRK-4712E-ENTRADA.
           03  WRK-4712E-COD-RETORNO             PIC  X(04).
           03  WRK-4712E-MSG-RETORNO             PIC  X(79).
           03  WRK-4712E-RESTART                 PIC  9(05).
           03  WRK-4712E-FLAG                    PIC  X(01).
           03  WRK-4712E-START-COUNT             PIC  X(01).
           03  WRK-4712E-CFUNC-BDSCO             PIC  9(09).
           03  WRK-4712E-CTERM                   PIC  X(08).
           03  WRK-4712E-FLAG-FILTRO             PIC  X(002).
           03  WRK-4712E-CBCO                    PIC  9(003).
           03  WRK-4712E-CAG-BCRIA               PIC  9(005).
           03  WRK-4712E-CAG-OPER                PIC  9(005).
           03  WRK-4712E-CCTA-BCRIA-CLI          PIC  9(013).
           03  WRK-4712E-CPRODT                  PIC  9(003).
           03  WRK-4712E-CSPROD-DESC-COML        PIC  9(003).
           03  WRK-4712E-DINIC-OPER-DESC         PIC  X(010).
           03  WRK-4712E-CELMTO-DESC-COML        PIC  9(003).
FS2511*    03  WRK-4712E-CCNPJ-CPF               PIC  9(009).
FS2511     03  WRK-4712E-CCNPJ-CPF               PIC  X(009).
FS2511*    03  WRK-4712E-CFLIAL-CNPJ             PIC  9(004).
FS2511     03  WRK-4712E-CFLIAL-CNPJ             PIC  X(004).
           03  WRK-4712E-CCTRL-CNPJ-CPF          PIC  9(002).
           03  WRK-4712E-CSIT-DESC-COML          PIC  9(003).
           03  WRK-4712E-CINDCD-EVNTO-WF         PIC  9(003).
           03  WRK-4712E-CINDCD-SIT-F-EVNTO      PIC  9(003).

       01  WRK-4712S-SAIDA.
           03  WRK-4712S-COD-RETORNO                PIC  X(04).
           03  WRK-4712S-MSG-RETORNO                PIC  X(79).
           03  WRK-4712S-RESTART                    PIC  9(05).
           03  WRK-4712S-FLAG                       PIC  X(01).
           03  WRK-4712S-QTDE-RETORNADA             PIC  9(03).
           03  WRK-4712S-COUNT                      PIC  9(09).
           03  WRK-4712S-DADOS.
               05  WRK-4712S-TABELA-SAIDA    OCCURS 10 TIMES.
                   10  WRK-4712S-DANO-OPER-DESC     PIC  9(04).
                   10  WRK-4712S-NSEQ-OPER-DESC     PIC  9(009).
                   10  WRK-4712S-IPSSOA-DESC-COML   PIC  X(060).
                   10  WRK-4712S-CBCO               PIC  9(003).
                   10  WRK-4712S-CAG-BCRIA          PIC  9(005).
                   10  WRK-4712S-CAG-BCRIA-DESC     PIC  X(040).
                   10  WRK-4712S-CCTA-BCRIA-CLI     PIC  9(013).
                   10  WRK-4712S-CBCO-OPER-OPER     PIC  9(003).
                   10  WRK-4712S-CAG-OPER-OPER      PIC  9(005).
                   10  WRK-4712S-CSIT-DESC-COML     PIC  9(003).
                   10  WRK-4712S-IRSUMO-SIT-DESC    PIC  X(015).
                   10  WRK-4712S-CSGL-SIT-DESC      PIC  X(006).
                   10  WRK-4712S-CPRODT             PIC  9(003).
                   10  WRK-4712S-IABREV-PRODT       PIC  X(012).
                   10  WRK-4712S-CSGL-PRODT-DESC    PIC  X(006).
                   10  WRK-4712S-CSPROD-DESC-COML   PIC  9(003).
                   10  WRK-4712S-CTPO-DESC-COML     PIC  9(003).
                   10  WRK-4712S-IRSUMO-SPROD-DESC  PIC  X(010).
                   10  WRK-4712S-CSGL-SPROD-DESC    PIC  X(006).
                   10  WRK-4712S-VOPER-DESC-COML    PIC  9(015)V9(2).
                   10  WRK-4712S-DINIC-OPER-DESC    PIC  X(010).
                   10  WRK-4712S-DVCTO-FNAL-OPER    PIC  X(010).
                   10  WRK-4712S-CMEIO-ENTRD-DESC   PIC  9(03).
                   10  WRK-4712S-IMEIO-ENTRD-DESC   PIC  X(25).
                   10  WRK-4712S-IRSUMO-MEIO-ENTRD  PIC  X(15).
                   10  WRK-4712S-CSGL-MEIO-ENTRD    PIC  X(06).
                   10  WRK-4712S-CINDCD-LIBRC-ANTCP PIC  X(01).
                   10  WRK-4712S-PARECER-FORML      PIC  X(01).
                   10  WRK-4712S-VLIQ-OPER-DESC     PIC  9(015)V9(2).

           03  WRK-4712S-CONSISTENCIA   REDEFINES   WRK-4712S-DADOS.
               05  WRK-4712S-TAMANHO-ERRO           PIC  9(01).
               05  WRK-4712S-TABELA-ERRO   OCCURS   2    TIMES.
                   10  WRK-4712S-CAMPOS-ERRO        PIC  9(01).

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
           '*** DCOM0404 - FIM DA AREA DE WORKING ***'.
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

           MOVE SPACES                 TO  WRK-OUT-DADOS-DDTM
                                           WRK-OUT-SENHAS-DDTM
                                           WRK-OUT-COMANDO-DDTM
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

           MOVE 'DCOMDDTM'             TO  WRK-TELA.

           IF  WRK-TRANSACAO           NOT EQUAL  'DCOM0404'
               PERFORM 1100-RECEBER-CONTROLE
           ELSE
               PERFORM 1300-PROCESSAR-DCOMDDTM
           END-IF.

           IF  WRK-TELA             EQUAL  'DCOMDDTM'
               PERFORM 1400-PROCESSAR-BRAD0660
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA RECEBE O CONTROLE DO DCOM0402                     *
      *----------------------------------------------------------------*
       1100-RECEBER-CONTROLE           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-4711E-ENTRADA
                      WRK-4711S-SAIDA
                      WRK-4712E-ENTRADA
                      WRK-4712S-SAIDA.

           MOVE WRK-MENSAGEM           TO  COMU-AREA-UN.
           MOVE 1                      TO  WRK-OUT-PAGINA-DDTM.

           MOVE COMU-TIMESTAMP-UN     TO WRK-INP-TMSTAMP-DDTM
                                         WRK-OUT-TMSTAMP-DDTM.

           IF COMU-AGENC-UN           NOT EQUAL  ZEROS
           AND COMU-AGENC-UN-X        NOT EQUAL SPACES AND LOW-VALUES
              MOVE COMU-AGENC-UN      TO  WRK-OUT-CODAGE-DDTM
                                           WRK-4711E-CAG-BCRIA
              MOVE '-'                 TO  WRK-OUT-FILLER-3
              MOVE COMU-CONTA-UN      TO  WRK-OUT-CONTA-DDTM
                                           WRK-4711E-CCTA-BCRIA-CLI
              MOVE SPACES              TO  WRK-OUT-NOME-DDTM
                                           WRK-OUT-CNPJCPF-DDTM
                                           WRK-OUT-DESCAGE-DDTM
                                           WRK-OUT-FILLER-1
                                           WRK-OUT-FILLER-2

DTS003        IF  WRK-TRANSACAO        EQUAL 'DCOM0499'
DTS003            MOVE WRK-COMU-INPUT-0499
DTS003                                 TO WRK-INPUT-DDTM
DTS003            PERFORM 1310-DEVOLVER-TELA
DTS003
DTS003            ADD  1               TO WRK-COMU-OCORR-0499
DTS003            PERFORM 1350-TRATAR-CONFIRMA
DTS003            GO TO 1100-99-FIM
DTS003        END-IF
DTS003
              IF  WRK-TRANSACAO        EQUAL 'DCOM0405'
                  MOVE COMU-PAGINA-UN  TO WRK-OUT-PAGINA-DDTM
                  COMPUTE WRK-4711E-RESTART = (COMU-PAGINA-UN - 1) * 4
                  PERFORM 1500-ACESSAR-DCOM4711

                  IF  WRK-4711S-COD-RETORNO EQUAL '0003'
                  AND COMU-PAGINA-UN        GREATER 1
                      INITIALIZE WRK-4711E-ENTRADA
                                 WRK-4711S-SAIDA
                      MOVE 1           TO WRK-OUT-PAGINA-DDTM
                      MOVE COMU-AGENC-UN
                                       TO  WRK-4711E-CAG-BCRIA
                      MOVE COMU-CONTA-UN
                                       TO  WRK-4711E-CCTA-BCRIA-CLI
                      MOVE ZEROS       TO  WRK-4711E-RESTART
                      PERFORM 1500-ACESSAR-DCOM4711
                  END-IF
              ELSE
                  MOVE ZEROS           TO  WRK-4711E-RESTART
                  PERFORM 1500-ACESSAR-DCOM4711
              END-IF

              MOVE WRK-4711S-IPSSOA-DESC-COML(1)
                                       TO WRK-OUT-NOME-DDTM
               MOVE WRK-4711S-CAG-BCRIA-DESC(1)
                                       TO WRK-OUT-DESCAGE-DDTM
               MOVE 237                TO  WRK-OUT-BANCO-DDTM
               MOVE 'BANCO:'           TO WRK-OUT-LITBCO-DDTM
               MOVE 'AGENCIA:'         TO WRK-OUT-LITAGE-DDTM
               MOVE 'CONTA...:'        TO WRK-OUT-LITCTA-DDTM

           ELSE
              MOVE 'CNPJ/CPF:'        TO WRK-OUT-LITCPF-DDTM
              MOVE COMU-CNPJ-UN       TO  WRK-OUT-NUMERO-DDTM
                                           WRK-4712E-CCNPJ-CPF
              MOVE '/'                 TO  WRK-OUT-FILLER-1
              MOVE COMU-FILIAL-UN     TO  WRK-OUT-FILIAL-DDTM
                                           WRK-4712E-CFLIAL-CNPJ
              MOVE '-'                 TO  WRK-OUT-FILLER-2
              MOVE COMU-DIG-UN        TO  WRK-OUT-CONTROLE-DDTM
                                           WRK-4712E-CCTRL-CNPJ-CPF
              MOVE SPACES              TO  WRK-OUT-NOME-DDTM
                                           WRK-OUT-AGENCIA-DDTM
                                           WRK-OUT-CONTA-DDTM-X
                                           WRK-OUT-FILLER-3
                                           WRK-OUT-LITBCO-DDTM
                                           WRK-OUT-LITAGE-DDTM
                                           WRK-OUT-LITCTA-DDTM

DTS003        IF  WRK-TRANSACAO        EQUAL 'DCOM0499'
DTS003            MOVE WRK-COMU-INPUT-0499
DTS003                                 TO WRK-INPUT-DDTM
DTS003            PERFORM 1310-DEVOLVER-TELA
DTS003
DTS003            ADD  1               TO WRK-COMU-OCORR-0499
DTS003            PERFORM 1350-TRATAR-CONFIRMA
DTS003            GO TO 1100-99-FIM
DTS003        END-IF
DTS003
              IF  WRK-TRANSACAO        EQUAL 'DCOM0405'
                  MOVE COMU-PAGINA-UN  TO WRK-OUT-PAGINA-DDTM
                  COMPUTE WRK-4712E-RESTART = (COMU-PAGINA-UN - 1) * 4
                  PERFORM 1600-ACESSAR-DCOM4712

                  IF  WRK-4712S-COD-RETORNO EQUAL '0003'
                  AND COMU-PAGINA-UN        GREATER 1
                      INITIALIZE WRK-4712E-ENTRADA
                                 WRK-4712S-SAIDA
                      MOVE 1           TO WRK-OUT-PAGINA-DDTM
                      MOVE COMU-CNPJ-UN
                                       TO  WRK-4712E-CCNPJ-CPF
                      MOVE COMU-FILIAL-UN
                                       TO  WRK-4712E-CFLIAL-CNPJ
                      MOVE COMU-DIG-UN TO  WRK-4712E-CCTRL-CNPJ-CPF
                      MOVE ZEROS       TO  WRK-4712E-RESTART
                      PERFORM 1600-ACESSAR-DCOM4712
                  END-IF
              ELSE
                  MOVE ZEROS           TO  WRK-4712E-RESTART
                  PERFORM 1600-ACESSAR-DCOM4712
              END-IF

              MOVE WRK-4712S-IPSSOA-DESC-COML(1)
                                      TO WRK-OUT-NOME-DDTM
           END-IF.

           IF  WRK-4711S-COD-RETORNO   EQUAL '0003'
           OR (WRK-4712S-COD-RETORNO   EQUAL '0003' OR '0915')
               MOVE 'S'                   TO WRK-FLAG-ERRO
               IF  WRK-TRANSACAO       EQUAL 'DCOM0405'
               AND COMU-MENSAGEM-UN    NOT EQUAL SPACES
                   MOVE COMU-MENSAGEM-UN TO WRK-MENSAGEM-ERRO
               ELSE
                   MOVE 'NAO EXISTE(M) OPERACAO(OES) PARA LIBERACAO'
                                          TO WRK-MENSAGEM-ERRO
               END-IF
               PERFORM 1320-RETORNAR-CHAMADOR
           END-IF.

           IF  COMU-MENSAGEM-UN        NOT EQUAL SPACES
               MOVE COMU-MENSAGEM-UN   TO WRK-OUT-MENSA-DDTM
           END-IF.

           MOVE '1'                    TO WRK-OUT-FASE-DDTM.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *  ESSA ROTINA PROCESSA A MENSAGEM VINDA DA TELA DCOMDDTM        *
      *----------------------------------------------------------------*
       1300-PROCESSAR-DCOMDDTM         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM             TO WRK-INPUT-DDTM.
           PERFORM 1310-DEVOLVER-TELA

           EVALUATE TRUE
               WHEN WRK-INP-PFK-DDTM     EQUAL 'H' OR
                   (WRK-INP-PFK-DDTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DDTM EQUAL 'PFK01')
                    IF   WRK-INP-FASE-DDTM    EQUAL '1'
                         PERFORM VARYING WRK-IND FROM 1 BY 1
                              UNTIL WRK-IND GREATER 4
                           IF  WRK-INP-OPERA-DDTM-X (WRK-IND)
                                        EQUAL SPACES OR LOW-VALUES
                               MOVE WRK-225 TO WRK-OUT-SELEC-ATTR-DDTM
                                                         (WRK-IND)
                           END-IF
                         END-PERFORM
                         MOVE ZEROS    TO WRK-OUT-COMANDO-ATTR-DDTM
                    ELSE
                         PERFORM VARYING WRK-IND FROM 1 BY 1
                              UNTIL WRK-IND GREATER 4
                           MOVE WRK-225 TO WRK-OUT-SELEC-ATTR-DDTM
                                                       (WRK-IND)
                         END-PERFORM
                         MOVE WRK-49353 TO WRK-OUT-COMANDO-ATTR-DDTM
                    END-IF
                    MOVE WRK-INP-MENSA-DDTM TO WRK-OUT-MENSA-DDTM

               WHEN WRK-INP-PFK-DDTM     EQUAL '3' OR
                   (WRK-INP-PFK-DDTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DDTM EQUAL 'PFK03')
                    PERFORM 1320-RETORNAR-CHAMADOR

               WHEN WRK-INP-PFK-DDTM     EQUAL 'X' OR
                   (WRK-INP-PFK-DDTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DDTM EQUAL 'PFK05')
                    PERFORM 1330-RETORNAR-MENU-ROTINAS

               WHEN WRK-INP-PFK-DDTM     EQUAL 'Z' OR
                   (WRK-INP-PFK-DDTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DDTM EQUAL 'PFK10')
                    PERFORM 1340-RETORNAR-MENU-DCOM

               WHEN WRK-INP-FASE-DDTM    EQUAL '1'
                    PERFORM 1301-TRATAR-FASE-1

               WHEN WRK-INP-FASE-DDTM    EQUAL '2'
                    PERFORM 1302-TRATAR-FASE-2

               WHEN OTHER
                    MOVE 'PF INVALIDA'   TO WRK-OUT-MENSA-DDTM
                    PERFORM VARYING WRK-IND FROM 1 BY 1
                              UNTIL WRK-IND GREATER 4
                        IF  WRK-INP-OPERA-DDTM-X(WRK-IND)
                                         EQUAL SPACES OR LOW-VALUES
                            MOVE WRK-225 TO WRK-OUT-SELEC-ATTR-DDTM
                                                          (WRK-IND)
                        END-IF
                    END-PERFORM
           END-EVALUATE.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1301-TRATAR-FASE-1              SECTION.
      *----------------------------------------------------------------*
           EVALUATE TRUE
               WHEN WRK-INP-PFK-DDTM     EQUAL '7' OR
                   (WRK-INP-PFK-DDTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DDTM EQUAL 'PFK07')
                    PERFORM 1360-TRATAR-VOLTA-PAG

               WHEN WRK-INP-PFK-DDTM     EQUAL '8' OR
                   (WRK-INP-PFK-DDTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DDTM EQUAL 'PFK08')
                    PERFORM 1370-TRATAR-AVANCA-PAG

               WHEN WRK-INP-PFK-DDTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DDTM EQUAL  SPACES OR LOW-VALUES
                    PERFORM 1380-TRATAR-ENTER
                    IF  WRK-INCONSISTENCIA EQUAL SPACES
                        IF  WRK-QTD-OPCAO-D GREATER ZEROS
                        OR  WRK-QTD-OPCAO-C GREATER ZEROS
                            PERFORM 1351-CHAMAR-TRAN-DCOM0405
                        ELSE
                            PERFORM VARYING WRK-IND FROM 1 BY 1 UNTIL
                                            WRK-IND GREATER 4
                                MOVE WRK-225 TO WRK-OUT-SELEC-ATTR-DDTM
                                                              (WRK-IND)
                            END-PERFORM
                            MOVE 'TECLE <PF6> PARA CONFIRMAR OU TECLE <E
      -                      'NTER> PARA ALTERAR'
                                             TO WRK-OUT-MENSA-DDTM
                            MOVE '2'     TO WRK-OUT-FASE-DDTM
                            MOVE WRK-49345 TO WRK-OUT-COMANDO-ATTR-DDTM
                        END-IF
                    ELSE
                        PERFORM VARYING WRK-IND FROM 1 BY 1
                                  UNTIL WRK-IND GREATER 4
                            IF  WRK-INP-OPERA-DDTM-X(WRK-IND)
                                         EQUAL SPACES OR LOW-VALUES
                                MOVE WRK-225
                                  TO WRK-OUT-SELEC-ATTR-DDTM(WRK-IND)
                            END-IF
                        END-PERFORM
                    END-IF
               WHEN OTHER
                    MOVE 'PF INVALIDA'   TO WRK-OUT-MENSA-DDTM
                    PERFORM VARYING WRK-IND FROM 1 BY 1
                      UNTIL WRK-IND GREATER 4
                        IF  WRK-INP-OPERA-DDTM-X(WRK-IND)
                                       EQUAL SPACES OR LOW-VALUES
                            MOVE WRK-225 TO WRK-OUT-SELEC-ATTR-DDTM
                                                      (WRK-IND)
                        END-IF
                    END-PERFORM
           END-EVALUATE.
      *----------------------------------------------------------------*
       1301-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1302-TRATAR-FASE-2              SECTION.
      *----------------------------------------------------------------*
           EVALUATE TRUE
               WHEN WRK-INP-PFK-DDTM     EQUAL '6' OR
                   (WRK-INP-PFK-DDTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DDTM EQUAL 'PFK06')
DTS003              MOVE 1               TO WRK-COMU-OCORR-0499
                    PERFORM 1350-TRATAR-CONFIRMA

               WHEN WRK-INP-PFK-DDTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DDTM EQUAL  SPACES OR LOW-VALUES
                    MOVE '1'             TO WRK-OUT-FASE-DDTM
                    MOVE ZEROS           TO WRK-OUT-COMANDO-ATTR-DDTM
                    PERFORM VARYING WRK-IND FROM 1 BY 1
                              UNTIL WRK-IND GREATER 4
                        IF  WRK-INP-OPERA-DDTM-X(WRK-IND) EQUAL SPACES
                                                          OR LOW-VALUES
                            MOVE WRK-225 TO WRK-OUT-SELEC-ATTR-DDTM
                                                          (WRK-IND)
                        END-IF
                    END-PERFORM
                    MOVE 'SELECIONE COM "L" PARA LIBERAR, "D" PARA DETAL
      -                  'HAR OU "C" PARA CANCELAR'
                                    TO  WRK-OUT-MENSA-DDTM

               WHEN OTHER
                    MOVE 'PF INVALIDA'   TO WRK-OUT-MENSA-DDTM
                    PERFORM VARYING WRK-IND FROM 1 BY 1 UNTIL
                                    WRK-IND GREATER 4
                        MOVE WRK-225 TO WRK-OUT-SELEC-ATTR-DDTM
                                                       (WRK-IND)
                    END-PERFORM
                    MOVE WRK-49345   TO WRK-OUT-COMANDO-ATTR-DDTM
           END-EVALUATE.
      *----------------------------------------------------------------*
       1302-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA DEVOLVE OS DADOS DA TELA SEM ALTERACAO            *
      *----------------------------------------------------------------*
       1310-DEVOLVER-TELA              SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-FIMAMOS-DDTM   TO WRK-OUT-FIMAMOS-DDTM
           MOVE WRK-INP-FASE-DDTM      TO WRK-OUT-FASE-DDTM
           MOVE WRK-INP-TMSTAMP-DDTM   TO WRK-OUT-TMSTAMP-DDTM
           MOVE WRK-INP-PAGINA-DDTM-X  TO WRK-OUT-PAGINA-DDTM-X
           MOVE WRK-INP-NOME-DDTM      TO WRK-OUT-NOME-DDTM
           MOVE WRK-INP-LITCPF-DDTM    TO WRK-OUT-LITCPF-DDTM
           MOVE WRK-INP-CNPJCPF-DDTM   TO WRK-OUT-CNPJCPF-DDTM
           MOVE WRK-INP-BANCO-DDTM-X   TO WRK-OUT-BANCO-DDTM-X
           MOVE WRK-INP-AGENCIA-DDTM   TO WRK-OUT-AGENCIA-DDTM
           MOVE WRK-INP-CONTA-DDTM-X   TO WRK-OUT-CONTA-DDTM-X
           MOVE WRK-INP-LITBCO-DDTM    TO WRK-OUT-LITBCO-DDTM
           MOVE WRK-INP-LITAGE-DDTM    TO WRK-OUT-LITAGE-DDTM
           MOVE WRK-INP-LITCTA-DDTM    TO WRK-OUT-LITCTA-DDTM.

           PERFORM VARYING WRK-IND FROM 1 BY 1 UNTIL WRK-IND GREATER 4
              MOVE WRK-INP-SELEC-DDTM(WRK-IND)
                                      TO WRK-OUT-SELEC-DDTM(WRK-IND)

              MOVE WRK-INP-OPERA-DDTM-X(WRK-IND)
                                      TO WRK-OUT-OPERA-DDTM-X(WRK-IND)
              MOVE WRK-INP-PROD-DDTM(WRK-IND)
                                      TO WRK-OUT-PROD-DDTM(WRK-IND)
              MOVE WRK-INP-SUBPROD-DDTM(WRK-IND)
                                      TO WRK-OUT-SUBPROD-DDTM(WRK-IND)
              MOVE WRK-INP-DTOPER-DDTM(WRK-IND)
                                      TO WRK-OUT-DTOPER-DDTM(WRK-IND)
              MOVE WRK-INP-VALOR-DDTM(WRK-IND)
                                      TO WRK-OUT-VALOR-DDTM-X(WRK-IND)
              MOVE WRK-INP-MEIOENT-DDTM(WRK-IND)
                                      TO WRK-OUT-MEIOENT-DDTM(WRK-IND)
              MOVE WRK-INP-SITANAL-DDTM(WRK-IND)
                                      TO WRK-OUT-SITANAL-DDTM(WRK-IND)
              MOVE WRK-INP-PARECER-DDTM(WRK-IND)
                                      TO WRK-OUT-PARECER-DDTM(WRK-IND)
              MOVE WRK-INP-TIPOLIB-DDTM(WRK-IND)
                                      TO WRK-OUT-TIPOLIB-DDTM(WRK-IND)
              MOVE WRK-INP-VLRLIQ-DDTM(WRK-IND)
                                      TO WRK-OUT-VLRLIQ-DDTM(WRK-IND)
           END-PERFORM.

      *----------------------------------------------------------------*
       1310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA RETORNA AO PROGRAMA CHAMADOR                      *
      *----------------------------------------------------------------*
       1320-RETORNAR-CHAMADOR          SECTION.
      *----------------------------------------------------------------*

           INITIALIZE COMU-AREA-UN.

           MOVE LENGTH OF COMU-AREA-UN
                                     TO COMU-LL-UN
           MOVE ZEROS                TO COMU-ZZ-UN.

           MOVE WRK-CHNG             TO WRK-FUNCAO.
           MOVE 'DCOM0402'           TO WRK-TELA.
           MOVE 'DCOM0404'           TO COMU-TRANSACAO-UN.
           MOVE WRK-INP-SENHAS-DDTM  TO COMU-SENHAS-UN
           MOVE WRK-INP-COMANDO-DDTM TO COMU-COMANDO-UN
           MOVE WRK-INP-TMSTAMP-DDTM TO COMU-TIMESTAMP-UN

           IF  WRK-FLAG-ERRO         EQUAL 'S'
               MOVE WRK-MENSAGEM-ERRO
                                     TO COMU-MENSAGEM-UN
               IF  WRK-TRANSACAO       EQUAL 'DCOM0402'
                   MOVE 1              TO COMU-OPCAO-UN
                   IF  WRK-INP-LITCPF-DDTM EQUAL SPACES OR LOW-VALUES
                       MOVE WRK-INP-CODAGE-DDTM   TO COMU-AGENC-UN
                       MOVE WRK-INP-CONTA-DDTM    TO COMU-CONTA-UN
                   ELSE
                       MOVE WRK-INP-NUMERO-DDTM   TO COMU-CNPJ-UN
                       MOVE WRK-INP-FILIAL-DDTM   TO COMU-FILIAL-UN
                       MOVE WRK-INP-CONTROLE-DDTM TO COMU-DIG-UN
                   END-IF
               END-IF
           END-IF.

           MOVE COMU-AREA-UN        TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1320-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRATAMENTO DE RETORNO AO MENU ROTINAS    *
      *----------------------------------------------------------------*
       1330-RETORNAR-MENU-ROTINAS      SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-CHNG            TO WRK-FUNCAO.
           MOVE 'DCOM0404'          TO WRK-TELA
                                       COMU-TRANSACAO-UN.
           MOVE '5'                 TO COMU-PFK-UN.
           MOVE COMU-AREA-UN       TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1330-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRATAMENTO DE RETORNO AO MENU DCOM       *
      *----------------------------------------------------------------*
       1340-RETORNAR-MENU-DCOM         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-CHNG           TO WRK-FUNCAO.
           MOVE 'DCOM0404'         TO WRK-TELA
                                      COMU-TRANSACAO-UN.
           MOVE 'A'                TO COMU-PFK-UN.
           MOVE COMU-AREA-UN      TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1340-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRAMAMENTO DO COMANDO CONFIRMA(PFK6)     *
      *----------------------------------------------------------------*
       1350-TRATAR-CONFIRMA            SECTION.
      *----------------------------------------------------------------*

DTS003     PERFORM VARYING WRK-IND     FROM WRK-COMU-OCORR-0499 BY 01
                     UNTIL WRK-IND     GREATER 04
                IF WRK-INP-SELEC-DDTM(WRK-IND) EQUAL 'L'
CAP001             INITIALIZE          WRK-SAIR
CAP001                                 WRK-OK
CAP001                                 WRK-PROCESSADO
CAP001                                 WRK-FIM
CAP001                                 WRK-SITUACAO
CAP001                                 WRK-PROTOCOLO
CAP001
CAP001             MOVE WRK-IND        TO IND-2

                   PERFORM 1910-ACESSAR-DCOM4174

CAP002             IF (WRK-FLAG        EQUAL 'S')
|                     AND (WRK-INP-TMSTAMP-DDTM(1:10)
|                                      EQUAL  '2021-06-03'
|                                         OR  '2021-06-04'
|                                         OR  '2021-06-05'
|                                         OR  '2021-06-06')
|                     MOVE 'NAO PERMITIDO LIBERACAO DE ORPAGS ENTRE OS D
|     -                    'IAS 03 E 06/06/2021 - NORMAT.4734'
|                                      TO WRK-MENSAGEM-ERRO
|                                         WRK-OUT-MENSA-DDTM
|                     MOVE 'S'           TO WRK-FLAG-ERRO
|                     GO TO 1350-99-FIM
CAP002             END-IF

CAP001             PERFORM 1800-TRATA-OPERACOES

CAP001             IF WRK-OK           EQUAL 'S'
                      PERFORM 1252-1-CHAMAR-MOD-DCOM4131
                      IF  WRK-INCONSISTENCIA      EQUAL SPACES
                          PERFORM 1352-CHAMAR-MOD-DCOM5110
DTS003
DTS003                    IF WRK-INCONSISTENCIA  EQUAL SPACES
DTS003                       PERFORM 1353-CHAMAR-TRANS-COMMIT
DTS003                       GO TO 1350-99-FIM
DTS003                    END-IF
                      END-IF
                      IF  WRK-INCONSISTENCIA      EQUAL '*'
                          GO TO 1350-99-FIM
                      END-IF
CAP001             END-IF
                END-IF
           END-PERFORM.

           MOVE SPACES                 TO WRK-OUT-FIMAMOS-DDTM
                                          WRK-OUT-SELEC-DDTM(1)
                                          WRK-OUT-SELEC-DDTM(2)
                                          WRK-OUT-SELEC-DDTM(3)
                                          WRK-OUT-SELEC-DDTM(4)

CAP001     IF WRK-OK                   NOT EQUAL 'S'
CAP001        GO TO 1350-99-FIM
CAP001     END-IF

           INITIALIZE WRK-4711E-ENTRADA
                      WRK-4711S-SAIDA
                      WRK-4712E-ENTRADA
                      WRK-4712S-SAIDA

           MOVE '1'                    TO WRK-OUT-FASE-DDTM

           IF WRK-INP-LITCPF-DDTM      EQUAL SPACES OR LOW-VALUES
              MOVE WRK-INP-CODAGE-DDTM TO  WRK-4711E-CAG-BCRIA
              MOVE WRK-INP-CONTA-DDTM  TO  WRK-4711E-CCTA-BCRIA-CLI
              COMPUTE WRK-4711E-RESTART = (WRK-INP-PAGINA-DDTM - 1) * 4
              PERFORM 1500-ACESSAR-DCOM4711
           ELSE
              MOVE WRK-INP-NUMERO-DDTM   TO WRK-4712E-CCNPJ-CPF
              MOVE WRK-INP-FILIAL-DDTM   TO WRK-4712E-CFLIAL-CNPJ
              MOVE WRK-INP-CONTROLE-DDTM TO WRK-4712E-CCTRL-CNPJ-CPF
              COMPUTE WRK-4712E-RESTART = (WRK-INP-PAGINA-DDTM - 1) * 4
              PERFORM 1600-ACESSAR-DCOM4712
           END-IF

           IF  WRK-4711S-COD-RETORNO   EQUAL '0003'
           OR  WRK-4712S-COD-RETORNO   EQUAL '0003'
               INITIALIZE WRK-4711E-ENTRADA
                          WRK-4711S-SAIDA
                          WRK-4712E-ENTRADA
                          WRK-4712S-SAIDA
               IF WRK-INP-LITCPF-DDTM     EQUAL SPACES OR LOW-VALUES
                  MOVE WRK-INP-CODAGE-DDTM TO WRK-4711E-CAG-BCRIA
                  MOVE WRK-INP-CONTA-DDTM TO WRK-4711E-CCTA-BCRIA-CLI
                  MOVE ZEROS              TO WRK-4711E-RESTART
                  PERFORM 1500-ACESSAR-DCOM4711
               ELSE
                  MOVE WRK-INP-NUMERO-DDTM TO WRK-4712E-CCNPJ-CPF
                  MOVE WRK-INP-FILIAL-DDTM TO WRK-4712E-CFLIAL-CNPJ
                  MOVE WRK-INP-CONTROLE-DDTM TO WRK-4712E-CCTRL-CNPJ-CPF
                  MOVE ZEROS                 TO WRK-4712E-RESTART
                  PERFORM 1600-ACESSAR-DCOM4712
               END-IF
           END-IF

           IF  WRK-4711S-COD-RETORNO   EQUAL '0003'
           OR (WRK-4712S-COD-RETORNO   EQUAL '0003' OR '0915')
               MOVE 'S'                TO  WRK-FLAG-ERRO
               MOVE 'LIBERACAO(OES) EFETUADA(S) COM SUCESSO - C/C CREDIT
      -             'ADA'
                                       TO  WRK-MENSAGEM-ERRO
               PERFORM 1320-RETORNAR-CHAMADOR
           ELSE
DTS003         IF  WRK-INCONSISTENCIA  EQUAL '?'
                   MOVE WRK-MENSAGEM-ERRO
                                       TO WRK-OUT-MENSA-DDTM
               ELSE
                   MOVE 'LIBERACAO(OES) EFETUADA(S) COM SUCESSO - C/C CR
      -                 'EDITADA'
                                       TO WRK-OUT-MENSA-DDTM
               END-IF
           END-IF
           .

      *----------------------------------------------------------------*
       1350-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA PASSA O CONTROLE PARA TRAN. DCOM0405              *
      *----------------------------------------------------------------*
       1351-CHAMAR-TRAN-DCOM0405       SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-CHNG                    TO WRK-FUNCAO.
           MOVE 'DCOM0405'                  TO WRK-TELA.
           MOVE 'DCOM0404'                  TO COMU-TRANSACAO-UN
           MOVE WRK-INP-SENHAS-DDTM         TO COMU-SENHAS-UN
           MOVE WRK-INP-TMSTAMP-DDTM        TO COMU-TIMESTAMP-UN
           MOVE WRK-INP-PAGINA-DDTM         TO COMU-PAGINA-UN

           PERFORM VARYING WRK-IND FROM 1 BY 1
                     UNTIL WRK-IND GREATER 4
               IF  WRK-INP-SELEC-DDTM(WRK-IND) EQUAL 'D' OR 'C'
                   MOVE WRK-INP-OPERA-DDTM-X(WRK-IND)
                                                    TO COMU-OPERACAO-UN
                   MOVE WRK-INP-SELEC-DDTM(WRK-IND) TO COMU-OPCAO-UN-X
               END-IF
           END-PERFORM.

           IF WRK-INP-LITCPF-DDTM      EQUAL SPACES OR LOW-VALUES
              MOVE WRK-INP-CODAGE-DDTM      TO COMU-AGENC-UN
              MOVE WRK-INP-CONTA-DDTM       TO COMU-CONTA-UN
FS2511        MOVE SPACES                   TO COMU-CNPJ-UN
                                               COMU-FILIAL-UN
FS2511        MOVE ZEROS                    TO COMU-DIG-UN
           ELSE
              MOVE ZEROS                    TO COMU-AGENC-UN
                                               COMU-CONTA-UN
              MOVE WRK-INP-NUMERO-DDTM      TO COMU-CNPJ-UN
              MOVE WRK-INP-FILIAL-DDTM      TO COMU-FILIAL-UN
              MOVE WRK-INP-CONTROLE-DDTM    TO COMU-DIG-UN
           END-IF.

           MOVE COMU-AREA-UN               TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1351-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  OBTEM VALOR A SER LIBERADO                                    *
      *----------------------------------------------------------------*
       1252-1-CHAMAR-MOD-DCOM4131      SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-OPERA-DDTM    (WRK-IND)(1:4)
                                       TO 4131E-DANO-OPER-DESC
           MOVE WRK-INP-OPERA-DDTM    (WRK-IND)(5:9)
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
                                  TO WRK-INP-VLRLIQ-DDTM(WRK-IND)
                   IF  4131S-PMAX-LIBRC-ANTCP NOT EQUAL 100
                       MOVE 'TIPO DE LIBERACAO ANTECIPADA EXIGE DETALHAM
      -                     'ENTO'
                         TO WRK-OUT-MENSA-DDTM
                       MOVE WRK-49369
                                     TO WRK-OUT-SELEC-ATTR-DDTM(WRK-IND)
                       MOVE '*'      TO WRK-INCONSISTENCIA
                       MOVE '1'      TO  WRK-OUT-FASE-DDTM
                       PERFORM VARYING WRK-IND FROM 1 BY 1
                                 UNTIL WRK-IND GREATER 5
                         IF  WRK-INP-OPERA-DDTM-X(WRK-IND) EQUAL SPACES
                             MOVE WRK-225
                                  TO WRK-OUT-SELEC-ATTR-DDTM(WRK-IND)
                         END-IF
                       END-PERFORM
                   END-IF

               WHEN '0099'
                   MOVE 'DB2'          TO ERR-TIPO-ACESSO
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                   MOVE 4131S-MSG-RETORNO
                                       TO WRK-OUT-MENSA-DDTM
                   MOVE '*'            TO WRK-INCONSISTENCIA

           END-EVALUATE.

      *----------------------------------------------------------------*
       1252-1-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA PASSA O CONTROLE PARA MODULO DCOM5110             *
      *----------------------------------------------------------------*
       1352-CHAMAR-MOD-DCOM5110        SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-5110E-ENTRADA WRK-5110S-SAIDA.

           MOVE ZEROS                  TO WRK-5110E-COD-RETORNO
           MOVE SPACES                 TO WRK-5110E-MSG-RETORNO
           MOVE ZEROS                  TO WRK-5110E-RESTART
           MOVE SPACES                 TO WRK-5110E-FLAG
           MOVE WRK-COD-USER           TO WRK-5110E-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO WRK-5110E-CTERM
           MOVE WRK-INP-OPERA-DDTM(WRK-IND)(1:4)
                                       TO WRK-5110E-DANO-OPER-DESC
           MOVE WRK-INP-OPERA-DDTM(WRK-IND)(5:9)
                                       TO WRK-5110E-NSEQ-OPER-DESC
           MOVE 4131S-TPO-LIBRC
                                       TO WRK-5110E-TPO-LIBR
           MOVE WRK-INP-VLRLIQ-DDTM(WRK-IND)
                                       TO WRK-5110E-VLIBRC
           MOVE 'DCOM5110'             TO WRK-MODULO

           CALL WRK-MODULO    USING    WRK-5110E-ENTRADA
                                       WRK-5110S-SAIDA
                                       ERRO-AREA
                                       WRK-SQLCA.

           IF  WRK-5110S-COD-RETORNO EQUAL '0099'
               MOVE  'DB2'       TO     ERR-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           ELSE
           IF  WRK-5110S-COD-RETORNO EQUAL '7777'
               MOVE  'APL'       TO     ERR-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           ELSE
               IF  WRK-5110S-COD-RETORNO NOT EQUAL '0000'
DTS001             PERFORM 8888-EXECUTAR-ROLLBACK
                   STRING WRK-INP-OPERA-DDTM(WRK-IND)
                       '-'WRK-5110S-MSG-RETORNO
                   DELIMITED BY SIZE INTO WRK-MENSAGEM-ERRO
                   MOVE WRK-MENSAGEM-ERRO TO WRK-OUT-MENSA-DDTM
                   MOVE WRK-49369    TO WRK-OUT-SELEC-ATTR-DDTM(WRK-IND)
                   MOVE 6            TO WRK-IND
DTS003             MOVE '?'          TO WRK-INCONSISTENCIA
               END-IF
           END-IF
           END-IF.

      *----------------------------------------------------------------*
       1352-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
DTS003
DTS003*----------------------------------------------------------------*
DTS003*  ESSA ROTINA CHAMA A TRANSACAO DCOM0499 PARA EFETUAR COMMIT    *
DTS003*----------------------------------------------------------------*
DTS003 1353-CHAMAR-TRANS-COMMIT        SECTION.
DTS003*----------------------------------------------------------------*
DTS003
DTS003     INITIALIZE COMU-AREA-UN.
DTS003
DTS003     MOVE LENGTH                 OF COMU-AREA-UN
DTS003                                 TO COMU-LL-UN.
DTS003     MOVE WRK-INP-TMSTAMP-DDTM   TO COMU-TIMESTAMP-UN.
DTS003     MOVE WRK-INP-PAGINA-DDTM    TO COMU-PAGINA-UN.
DTS003
DTS003     IF WRK-INP-LITCPF-DDTM      EQUAL SPACES OR LOW-VALUES
DTS003        MOVE WRK-INP-CODAGE-DDTM TO COMU-AGENC-UN
DTS003        MOVE WRK-INP-CONTA-DDTM  TO COMU-CONTA-UN
FS2511        MOVE SPACES              TO COMU-CNPJ-UN
DTS003                                    COMU-FILIAL-UN
DTS003        MOVE ZEROS               TO COMU-DIG-UN
DTS003     ELSE
DTS003        MOVE ZEROS               TO COMU-AGENC-UN
DTS003                                    COMU-CONTA-UN
DTS003        MOVE WRK-INP-NUMERO-DDTM TO COMU-CNPJ-UN
DTS003        MOVE WRK-INP-FILIAL-DDTM TO COMU-FILIAL-UN
DTS003        MOVE WRK-INP-CONTROLE-DDTM
DTS003                                 TO COMU-DIG-UN
DTS003     END-IF.
DTS003
DTS003     MOVE WRK-INPUT-DDTM         TO WRK-COMU-INPUT-0499.
DTS003     MOVE WRK-IND                TO WRK-COMU-OCORR-0499.
DTS003
DTS003     MOVE 'DCOM0404'             TO COMU-TRANSACAO-UN.
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
       1360-TRATAR-VOLTA-PAG           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-4711E-ENTRADA
                      WRK-4711S-SAIDA
                      WRK-4712E-ENTRADA
                      WRK-4712S-SAIDA.

           IF WRK-INP-PAGINA-DDTM  EQUAL  1
              MOVE 'NAO EXISTE PAGINA PARA VOLTAR'
                                       TO WRK-OUT-MENSA-DDTM
              PERFORM VARYING WRK-IND FROM 1 BY 1
                        UNTIL WRK-IND GREATER 4
                  IF  WRK-INP-OPERA-DDTM-X(WRK-IND)
                                       EQUAL SPACES OR LOW-VALUES
                      MOVE WRK-225 TO WRK-OUT-SELEC-ATTR-DDTM(WRK-IND)
                  END-IF
              END-PERFORM
              GO TO 1360-99-FIM
           END-IF.

           IF  WRK-INP-MENSA-DDTM      NOT EQUAL WRK-MENSA-PF7
               IF (WRK-INP-SELEC-DDTM(1) EQUAL 'L' OR 'D' OR 'C')
               OR (WRK-INP-SELEC-DDTM(2) EQUAL 'L' OR 'D' OR 'C')
               OR (WRK-INP-SELEC-DDTM(3) EQUAL 'L' OR 'D' OR 'C')
               OR (WRK-INP-SELEC-DDTM(4) EQUAL 'L' OR 'D' OR 'C')
                   MOVE WRK-MENSA-PF7  TO WRK-OUT-MENSA-DDTM
                   GO TO 1360-99-FIM
               END-IF
           END-IF.

           MOVE SPACES                 TO WRK-OUT-FIMAMOS-DDTM.

           IF WRK-INP-LITCPF-DDTM      EQUAL SPACES OR LOW-VALUES
              MOVE WRK-INP-CODAGE-DDTM TO  WRK-4711E-CAG-BCRIA
              MOVE WRK-INP-CONTA-DDTM  TO  WRK-4711E-CCTA-BCRIA-CLI
              COMPUTE WRK-4711E-RESTART = (WRK-INP-PAGINA-DDTM - 2) * 4
              PERFORM 1500-ACESSAR-DCOM4711
           ELSE
              MOVE WRK-INP-NUMERO-DDTM   TO WRK-4712E-CCNPJ-CPF
              MOVE WRK-INP-FILIAL-DDTM   TO WRK-4712E-CFLIAL-CNPJ
              MOVE WRK-INP-CONTROLE-DDTM TO WRK-4712E-CCTRL-CNPJ-CPF
              COMPUTE WRK-4712E-RESTART = (WRK-INP-PAGINA-DDTM - 2) * 4
              PERFORM 1600-ACESSAR-DCOM4712
           END-IF.

           MOVE  'AMOSTRAGEM CONTINUA'    TO  WRK-OUT-MENSA-DDTM
           COMPUTE WRK-OUT-PAGINA-DDTM = WRK-INP-PAGINA-DDTM - 1.

      *----------------------------------------------------------------*
       1360-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRAMAMENTO DO COMANDO PFK8 - AVANCA PAG. *
      *----------------------------------------------------------------*
       1370-TRATAR-AVANCA-PAG          SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-4711E-ENTRADA
                      WRK-4711S-SAIDA
                      WRK-4712E-ENTRADA
                      WRK-4712S-SAIDA.

           IF WRK-INP-FIMAMOS-DDTM  EQUAL  '*'
              MOVE 'NAO EXISTE PAGINA PARA AVANCAR'
                                       TO WRK-OUT-MENSA-DDTM
              PERFORM VARYING WRK-IND FROM 1 BY 1
                        UNTIL WRK-IND GREATER 4
                  IF  WRK-INP-OPERA-DDTM-X(WRK-IND)
                                       EQUAL SPACES OR LOW-VALUES
                      MOVE WRK-225 TO WRK-OUT-SELEC-ATTR-DDTM(WRK-IND)
                  END-IF
              END-PERFORM
              GO TO 1370-99-FIM
           END-IF.

           IF  WRK-INP-MENSA-DDTM      NOT EQUAL WRK-MENSA-PF8
               IF (WRK-INP-SELEC-DDTM(1) EQUAL 'L' OR 'D' OR 'C')
               OR (WRK-INP-SELEC-DDTM(2) EQUAL 'L' OR 'D' OR 'C')
               OR (WRK-INP-SELEC-DDTM(3) EQUAL 'L' OR 'D' OR 'C')
               OR (WRK-INP-SELEC-DDTM(4) EQUAL 'L' OR 'D' OR 'C')
                   MOVE WRK-MENSA-PF8  TO WRK-OUT-MENSA-DDTM
                   GO TO 1370-99-FIM
               END-IF
           END-IF.

           IF WRK-INP-LITCPF-DDTM      EQUAL SPACES OR LOW-VALUES
              MOVE WRK-INP-CODAGE-DDTM TO  WRK-4711E-CAG-BCRIA
              MOVE WRK-INP-CONTA-DDTM  TO  WRK-4711E-CCTA-BCRIA-CLI
              COMPUTE WRK-4711E-RESTART = (WRK-INP-PAGINA-DDTM) * 4
              PERFORM 1500-ACESSAR-DCOM4711
           ELSE
              MOVE WRK-INP-NUMERO-DDTM   TO WRK-4712E-CCNPJ-CPF
              MOVE WRK-INP-FILIAL-DDTM   TO WRK-4712E-CFLIAL-CNPJ
              MOVE WRK-INP-CONTROLE-DDTM TO WRK-4712E-CCTRL-CNPJ-CPF
              COMPUTE WRK-4712E-RESTART = (WRK-INP-PAGINA-DDTM) * 4
              PERFORM 1600-ACESSAR-DCOM4712
           END-IF.

           COMPUTE WRK-OUT-PAGINA-DDTM = WRK-INP-PAGINA-DDTM + 1.

           IF WRK-OUT-FIMAMOS-DDTM     EQUAL '*'
              MOVE 'FIM DE AMOSTRAGEM  '  TO  WRK-OUT-MENSA-DDTM
           ELSE
              MOVE 'AMOSTRAGEM CONTINUA'  TO  WRK-OUT-MENSA-DDTM.

      *----------------------------------------------------------------*
       1370-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRAMAMENTO DO COMANDO ENTER              *
      *----------------------------------------------------------------*
       1380-TRATAR-ENTER               SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INP-SELEC-DDTM(1)   EQUAL SPACES
           AND WRK-INP-SELEC-DDTM(2)   EQUAL SPACES
           AND WRK-INP-SELEC-DDTM(3)   EQUAL SPACES
           AND WRK-INP-SELEC-DDTM(4)   EQUAL SPACES
               MOVE 'NENHUMA OPCAO FOI SELECIONADA'
                                       TO  WRK-OUT-MENSA-DDTM
               MOVE '*'                TO WRK-INCONSISTENCIA
               GO TO 1380-99-FIM
           END-IF.

           PERFORM VARYING WRK-IND FROM 1 BY 1 UNTIL WRK-IND GREATER 4
             IF  WRK-INP-SELEC-DDTM(WRK-IND) NOT EQUAL 'L' AND 'C'
                                                   AND 'D' AND SPACES
                                                   AND LOW-VALUES
                 MOVE '*'        TO WRK-INCONSISTENCIA
                 MOVE WRK-49353  TO WRK-OUT-SELEC-ATTR-DDTM(WRK-IND)
                 MOVE 'OPCAO INVALIDA - SELECIONAR APENAS: L/D/C'
                                 TO  WRK-OUT-MENSA-DDTM
             END-IF
           END-PERFORM.

           IF WRK-INCONSISTENCIA  EQUAL  '*'
              GO TO 1380-99-FIM
           END-IF.

           MOVE SPACES           TO WRK-SELEC-ANT.

           PERFORM VARYING WRK-IND FROM 1 BY 1 UNTIL WRK-IND GREATER 4
             IF  WRK-INP-SELEC-DDTM(WRK-IND) NOT EQUAL WRK-SELEC-ANT AND
                 WRK-SELEC-ANT               NOT EQUAL SPACES
                 IF  WRK-INP-SELEC-DDTM(WRK-IND)  NOT EQUAL  SPACES
                     MOVE '*'        TO WRK-INCONSISTENCIA
                 END-IF
             ELSE
                 MOVE WRK-INP-SELEC-DDTM(WRK-IND) TO WRK-SELEC-ANT
             END-IF
           END-PERFORM.

           IF WRK-INCONSISTENCIA  EQUAL  '*'
              PERFORM VARYING WRK-IND FROM 1 BY 1
                        UNTIL WRK-IND GREATER 4
                 IF WRK-INP-SELEC-DDTM(WRK-IND) NOT EQUAL SPACES
                                                      AND LOW-VALUES
                    MOVE WRK-49353  TO WRK-OUT-SELEC-ATTR-DDTM(WRK-IND)
                 END-IF
              END-PERFORM
              MOVE 'SELECIONAR APENAS UM OPCAO: L/D/C'
                              TO  WRK-OUT-MENSA-DDTM
              GO TO 1380-99-FIM
           END-IF.


           MOVE    ZEROS         TO WRK-QTD-OPCAO-D WRK-QTD-OPCAO-C

           PERFORM VARYING WRK-IND FROM 1 BY 1 UNTIL WRK-IND GREATER 4
             IF  WRK-INP-SELEC-DDTM(WRK-IND) EQUAL 'D'
                 ADD  1          TO WRK-QTD-OPCAO-D
             END-IF
           END-PERFORM.

           IF WRK-QTD-OPCAO-D  GREATER  1
              PERFORM VARYING WRK-IND FROM 1 BY 1
                        UNTIL WRK-IND GREATER 4
                 IF WRK-INP-SELEC-DDTM(WRK-IND) NOT EQUAL SPACES
                                                      AND LOW-VALUES
                    MOVE WRK-49353  TO WRK-OUT-SELEC-ATTR-DDTM(WRK-IND)
                 END-IF
              END-PERFORM
              MOVE 'SELECIONAR APENAS UMA OCORRENCIA PARA OPCAO "D"'
                              TO  WRK-OUT-MENSA-DDTM
              MOVE '*'        TO WRK-INCONSISTENCIA
           END-IF.

           PERFORM VARYING WRK-IND FROM 1 BY 1 UNTIL WRK-IND GREATER 4
             IF  WRK-INP-SELEC-DDTM(WRK-IND) EQUAL 'C'
                 ADD  1          TO WRK-QTD-OPCAO-C
             END-IF
           END-PERFORM.

           IF WRK-QTD-OPCAO-C  GREATER  1
              PERFORM VARYING WRK-IND FROM 1 BY 1
                        UNTIL WRK-IND GREATER 4
                 IF WRK-INP-SELEC-DDTM(WRK-IND) NOT EQUAL SPACES
                                                      AND LOW-VALUES
                    MOVE WRK-49353  TO WRK-OUT-SELEC-ATTR-DDTM(WRK-IND)
                 END-IF
              END-PERFORM
              MOVE 'SELECIONAR APENAS UMA OCORRENCIA PARA OPCAO "C"'
                              TO  WRK-OUT-MENSA-DDTM
              MOVE '*'        TO WRK-INCONSISTENCIA
           END-IF.

DTS002     IF  WRK-INCONSISTENCIA      NOT EQUAL '*'
DTS002         PERFORM VARYING IND-1   FROM 1 BY 1
DTS002           UNTIL IND-1           GREATER 5
DTS002              IF WRK-INP-SELEC-DDTM(IND-1)
DTS002                                 NOT EQUAL SPACES
DTS002                 PERFORM 1385-VERIFICAR-OPER-NETEMPR
DTS002              END-IF
DTS002         END-PERFORM
DTS002     END-IF.

      *----------------------------------------------------------------*
       1380-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

DTS002*----------------------------------------------------------------*
DTS002*    VERIFICA SE OPERACAO CADASTRADA PELO NET EMPRESA:           *
DTS002*  - OPERACAO CADASTRADA PELO NET EMPRESA, PODERA SER ALTERADA   *
DTS002*    PELA AGENCIA  SOMENTE  SE FOI CANCELADA A  LIBERACAO PELA   *
DTS002*    AGENCIA (SITUACAO FINAL = 21)                               *
DTS002*----------------------------------------------------------------*
DTS002 1385-VERIFICAR-OPER-NETEMPR     SECTION.
DTS002*----------------------------------------------------------------*
DTS002
DTS002     INITIALIZE WRK-5505E-ENTRADA
DTS002                WRK-5505E-RETORNO.
DTS002
DTS002     MOVE WRK-COD-USER-R         TO WRK-5505E-CFUNC-BDSCO.
DTS002     MOVE LNK-IO-LTERM           TO WRK-5505E-CTERM.
DTS002     MOVE WRK-INP-DANO-DDTM     (IND-1)
DTS002                                 TO WRK-5505E-DANO-OPER-DESC
DTS002     MOVE WRK-INP-NSEQ-DDTM     (IND-1)
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
DTS002                                 TO WRK-OUT-MENSA-DDTM
DTS002               MOVE WRK-49353    TO
DTS002                                 WRK-OUT-SELEC-ATTR-DDTM(IND-1)
DTS002           END-IF
DTS002
DTS002        WHEN  0099
DTS002              MOVE 'DB2'         TO ERR-TIPO-ACESSO
DTS002              PERFORM 9999-PROCESSAR-ROTINA-ERRO
DTS002     END-EVALUATE.
DTS002
DTS002*----------------------------------------------------------------*
DTS002 1385-99-FIM.                    EXIT.
DTS002*----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA CARREGA A TELA QUE SERA DEMONSTRADA PELO IMS.     *
      *----------------------------------------------------------------*
       1400-PROCESSAR-BRAD0660         SECTION.
      *----------------------------------------------------------------*

           CALL 'BRAD0660'             USING WRK-OUTPUT-DDTM
                                             WRK-660-DCOMDDTM.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE RETURN-CODE        TO WRK-RETURN-CODE
               MOVE 10                 TO WRK-LOCAL-ERRO
               MOVE WRK-ERRO-BRAD0660  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-ISRT               TO WRK-FUNCAO.
           MOVE WRK-OUTPUT-DDTM        TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA CHAMA O MODULO DCOM4711                           *
      *----------------------------------------------------------------*
       1500-ACESSAR-DCOM4711           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-COD-USER           TO  WRK-4711E-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO  WRK-4711E-CTERM
           MOVE 'N'                    TO  WRK-4711E-START-COUNT
           MOVE SPACES                 TO  WRK-4711E-FLAG
           MOVE 237                    TO  WRK-4711E-CBCO
           MOVE ZEROS                  TO  WRK-4711E-CAG-OPER
           MOVE 'LI'                   TO  WRK-4711E-FLAG-FILTRO
           MOVE 'DCOM4711'             TO  WRK-MODULO

           CALL WRK-MODULO    USING    WRK-4711E-ENTRADA
                                       WRK-4711S-SAIDA
                                       ERRO-AREA
                                       WRK-SQLCA.


           EVALUATE WRK-4711S-COD-RETORNO
               WHEN '0000'
                    IF WRK-4711S-DANO-OPER-DESC(5) EQUAL ZEROS
                       MOVE '*'     TO  WRK-OUT-FIMAMOS-DDTM
                    ELSE
                       MOVE SPACES  TO  WRK-OUT-FIMAMOS-DDTM
                    END-IF

                    MOVE 'SELECIONE COM "L" PARA LIBERAR, "D" PARA DETAL
      -                  'HAR OU "C" PARA CANCELAR'
                                    TO  WRK-OUT-MENSA-DDTM

                    PERFORM VARYING WRK-IND FROM 1 BY 1
                              UNTIL WRK-IND GREATER 4
                        INITIALIZE WRK-OUT-TABELA-DDTM(WRK-IND)
                        MOVE SPACES    TO WRK-OUT-OPERA-DDTM-X(WRK-IND)
                                          WRK-OUT-VALOR-DDTM-X(WRK-IND)
                    END-PERFORM

                    PERFORM 1510-MONTAR-TELA VARYING WRK-IND FROM
                            1 BY 1 UNTIL WRK-IND GREATER 4

               WHEN '0099'
                    MOVE  'DB2'     TO     ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN '0003'
                    GO TO 1500-99-FIM

               WHEN OTHER
                    MOVE  'APL'     TO     ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-EVALUATE.

      *----------------------------------------------------------------*
       1500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA CARREGA OS CAMPOS DA TELA A PARTIR DO DCOM4711    *
      *----------------------------------------------------------------*
       1510-MONTAR-TELA                SECTION.
      *----------------------------------------------------------------*

           IF WRK-4711S-DANO-OPER-DESC(WRK-IND) NUMERIC
           AND WRK-4711S-DANO-OPER-DESC(WRK-IND) NOT EQUAL ZEROS
              MOVE WRK-4711S-DANO-OPER-DESC(WRK-IND)
                                  TO WRK-OUT-OPERA-DDTM(WRK-IND)(1:4)
              MOVE WRK-4711S-NSEQ-OPER-DESC(WRK-IND)
                                  TO WRK-OUT-OPERA-DDTM(WRK-IND)(5:9)
              MOVE WRK-4711S-IABREV-PRODT(WRK-IND)
                                  TO WRK-OUT-PROD-DDTM(WRK-IND)
              MOVE WRK-4711S-IRSUMO-SPROD-DESC(WRK-IND)
                                  TO WRK-OUT-SUBPROD-DDTM(WRK-IND)
              STRING WRK-4711S-DINIC-OPER-DESC(WRK-IND)(1:2)
              '/'    WRK-4711S-DINIC-OPER-DESC(WRK-IND)(4:2)
              '/'    WRK-4711S-DINIC-OPER-DESC(WRK-IND)(7:4)
              DELIMITED BY SIZE        INTO WRK-OUT-DTOPER-DDTM(WRK-IND)
              MOVE WRK-4711S-VOPER-DESC-COML(WRK-IND)
                                  TO WRK-OUT-VALOR-DDTM(WRK-IND)
              MOVE WRK-4711S-IRSUMO-MEIO-ENTRD(WRK-IND)
                                  TO WRK-OUT-MEIOENT-DDTM(WRK-IND)
              MOVE WRK-4711S-IRSUMO-SIT-DESC(WRK-IND)
                                  TO WRK-OUT-SITANAL-DDTM(WRK-IND)
FM0510        PERFORM 1515-OBTER-SIT-PROTOC
              IF  WRK-4711S-PARECER-FORML(WRK-IND) EQUAL 'S'
                  MOVE 'SIM'      TO WRK-OUT-PARECER-DDTM(WRK-IND)
              ELSE
                  MOVE 'NAO'      TO WRK-OUT-PARECER-DDTM(WRK-IND)
              END-IF
              MOVE WRK-4711S-CINDCD-LIBRC-ANTCP(WRK-IND)
                                  TO WRK-OUT-TIPOLIB-DDTM(WRK-IND)
              MOVE WRK-4711S-VLIQ-OPER-DESC(WRK-IND)
                                  TO WRK-OUT-VLRLIQ-DDTM(WRK-IND)
           ELSE
              MOVE WRK-225        TO WRK-OUT-SELEC-ATTR-DDTM(WRK-IND)
           END-IF.

      *----------------------------------------------------------------*
       1510-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

FM0510*----------------------------------------------------------------*
.     *    OBTER SITUACAO DO PROTOCOLO                                 *
.     *----------------------------------------------------------------*
.      1515-OBTER-SIT-PROTOC           SECTION.
.     *----------------------------------------------------------------*
.
.          INITIALIZE DCOMZD-BLOCO-ENTRADA
.                     DCOMZD-RETORNO
.
.          MOVE 'E'                    TO DCOMZD-E-CTPO-PROT-AGNDA.
.          MOVE WRK-4711S-DANO-OPER-DESC(WRK-IND)
.                                      TO DCOMZD-E-DANO-OPER-DESC.
.          MOVE WRK-4711S-NSEQ-OPER-DESC(WRK-IND)
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
.                                      TO WRK-OUT-SITANAL-DDTM(WRK-IND)
.                       WHEN 3
.                            CONTINUE
.                       WHEN 4
.                            MOVE 'VLR INDISP'
.                                      TO WRK-OUT-SITANAL-DDTM(WRK-IND)
.                       WHEN OTHER
.                            MOVE 'REJEIT CIP'
.                                      TO WRK-OUT-SITANAL-DDTM(WRK-IND)
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
.      1515-99-FIM.                    EXIT.
FM0510*----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA CHAMA O MODULO DCOM4712                           *
      *----------------------------------------------------------------*
       1600-ACESSAR-DCOM4712           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-COD-USER           TO  WRK-4712E-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO  WRK-4712E-CTERM
           MOVE 'N'                    TO  WRK-4712E-START-COUNT
           MOVE SPACES                 TO  WRK-4712E-FLAG
           MOVE 'LI'                   TO  WRK-4712E-FLAG-FILTRO
           MOVE WRK-COD-DEPTO-N        TO  WRK-AGENCIA-NUM
           MOVE WRK-AGENCIA-AUX        TO  WRK-4712E-MSG-RETORNO(1:5)
           MOVE 'DCOM4712'             TO  WRK-MODULO

           CALL WRK-MODULO    USING    WRK-4712E-ENTRADA
                                       WRK-4712S-SAIDA
                                       ERRO-AREA
                                       WRK-SQLCA.

           EVALUATE WRK-4712S-COD-RETORNO
               WHEN '0000'
                    IF WRK-4712S-DANO-OPER-DESC(5) EQUAL ZEROS
                       MOVE '*'     TO  WRK-OUT-FIMAMOS-DDTM
                    ELSE
                       MOVE SPACES  TO  WRK-OUT-FIMAMOS-DDTM
                    END-IF

                    MOVE 'SELECIONE COM "L" PARA LIBERAR, "D" PARA DETAL
      -                  'HAR OU "C" PARA CANCELAR'
                                    TO  WRK-OUT-MENSA-DDTM

                    PERFORM VARYING WRK-IND FROM 1 BY 1
                              UNTIL WRK-IND GREATER 4
                        INITIALIZE WRK-OUT-TABELA-DDTM(WRK-IND)
                        MOVE SPACES    TO WRK-OUT-OPERA-DDTM-X(WRK-IND)
                                          WRK-OUT-VALOR-DDTM-X(WRK-IND)
                    END-PERFORM

                    PERFORM 1610-MONTAR-TELA VARYING WRK-IND FROM
                            1 BY 1 UNTIL WRK-IND GREATER 4

               WHEN '0099'
                    MOVE  'DB2'     TO     ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN '0003'
               WHEN '0915'
                    GO TO 1600-99-FIM

               WHEN OTHER
                    MOVE 'S'                    TO WRK-FLAG-ERRO
                    MOVE WRK-4712S-MSG-RETORNO  TO WRK-MENSAGEM-ERRO
                    PERFORM 1320-RETORNAR-CHAMADOR
           END-EVALUATE.

      *----------------------------------------------------------------*
       1600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA CARREGA OS CAMPOS DA TELA A PARTIR DO DCOM4711    *
      *----------------------------------------------------------------*
       1610-MONTAR-TELA                SECTION.
      *----------------------------------------------------------------*

           IF WRK-4712S-DANO-OPER-DESC(WRK-IND) NUMERIC
           AND WRK-4712S-DANO-OPER-DESC(WRK-IND) NOT EQUAL ZEROS
              MOVE WRK-4712S-DANO-OPER-DESC(WRK-IND)
                                  TO WRK-OUT-OPERA-DDTM(WRK-IND)(1:4)
              MOVE WRK-4712S-NSEQ-OPER-DESC(WRK-IND)
                                  TO WRK-OUT-OPERA-DDTM(WRK-IND)(5:9)
              MOVE WRK-4712S-IABREV-PRODT(WRK-IND)
                                  TO WRK-OUT-PROD-DDTM(WRK-IND)
              MOVE WRK-4712S-IRSUMO-SPROD-DESC(WRK-IND)
                                  TO WRK-OUT-SUBPROD-DDTM(WRK-IND)
              MOVE WRK-4712S-DINIC-OPER-DESC(WRK-IND)
                                  TO WRK-OUT-DTOPER-DDTM(WRK-IND)
              MOVE WRK-4712S-VOPER-DESC-COML(WRK-IND)
                                  TO WRK-OUT-VALOR-DDTM(WRK-IND)
              MOVE WRK-4712S-IRSUMO-MEIO-ENTRD(WRK-IND)
                                  TO WRK-OUT-MEIOENT-DDTM(WRK-IND)
              MOVE WRK-4712S-IRSUMO-SIT-DESC(WRK-IND)
                                  TO WRK-OUT-SITANAL-DDTM(WRK-IND)
              IF  WRK-4712S-PARECER-FORML(WRK-IND) EQUAL 'S'
                  MOVE 'SIM'      TO WRK-OUT-PARECER-DDTM(WRK-IND)
              ELSE
                  MOVE 'NAO'      TO WRK-OUT-PARECER-DDTM(WRK-IND)
              END-IF
              MOVE WRK-4712S-CINDCD-LIBRC-ANTCP(WRK-IND)
                                  TO WRK-OUT-TIPOLIB-DDTM(WRK-IND)
              MOVE WRK-4712S-VLIQ-OPER-DESC(WRK-IND)
                                  TO WRK-OUT-VLRLIQ-DDTM(WRK-IND)
           ELSE
              MOVE WRK-225        TO WRK-OUT-SELEC-ATTR-DDTM(WRK-IND)
           END-IF.

      *----------------------------------------------------------------*
       1610-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

CAP002*----------------------------------------------------------------*
|      1800-TRATA-OPERACOES            SECTION.
|     *----------------------------------------------------------------*
|     *
|          IF WRK-FLAG                 EQUAL 'N'
|             MOVE 'S'                 TO WRK-OK
|             GO TO 1800-99-FIM
|          END-IF
|
|     *    VERIFICAR SE A OPERCAO JA FOI ENVIADA
|          PERFORM 1900-VERIFICAR-PROTOCOLO
|     *
|          EVALUATE WRK-ACHOU-SOLTC
|              WHEN 'S'
|     *             LER PROXIMO REGISTRO
|     *             MENSAGEM DE PROTOCOLO EM PROCESSAMENTO NA CIP
CAP005              MOVE 'OPERACAO INDISPONIVEL. LIBERACAO EM PROCESSAME
CAP005-                  'NTO NA CIP'
|                                      TO WRK-OUT-MENSA-DDTM
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
|     *
|                          IF BVVEND-S-COD-RETORNO  NOT EQUAL ZEROS
|                             GO TO 1800-99-FIM
|                          END-IF
|                       END-PERFORM
F2404               END-IF
|     *             MENSAGEM DE PROTOCOLO EM PROCESSAMENTO NA CIP
CAP005              MOVE 'OPERACAO EM PROCESSAMENTO NA CIP'
|                                      TO WRK-OUT-MENSA-DDTM
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
|          MOVE WRK-INP-DANO-DDTM(IND-2)
|                                      TO DCOMZD-E-DANO-OPER-DESC
|          MOVE WRK-INP-NSEQ-DDTM(IND-2)
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
|                                      TO WRK-OUT-MENSA-DDTM
|                                         WRK-MENSAGEM-ERRO
|                  PERFORM 9999-PROCESSAR-ROTINA-ERRO
|             WHEN OTHER
|                  MOVE 'APL'          TO ERR-TIPO-ACESSO
|                  MOVE DCOMZD-S-MSG-RETORNO(1:79)
|                                      TO WRK-OUT-MENSA-DDTM
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
|          MOVE WRK-INP-DANO-DDTM(IND-2)
|                                      TO 4174-ENT-DANO-OPER-DESC
|          MOVE WRK-INP-NSEQ-DDTM(IND-2)
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
|                                      TO WRK-OUT-MENSA-DDTM
|                                         WRK-MENSAGEM-ERRO
|                  PERFORM 9999-PROCESSAR-ROTINA-ERRO
|              WHEN OTHER
|                  MOVE 4174-SAI-MSG-RETORNO
|                                      TO WRK-OUT-MENSA-DDTM
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
|          MOVE WRK-INP-DANO-DDTM(IND-2)
|                                      TO G8-DANO-OPER-DESC
|          MOVE WRK-INP-NSEQ-DDTM(IND-2)
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
|             MOVE G8-MENSAGEM         TO WRK-OUT-MENSA-DDTM
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
|          MOVE WRK-INP-CODAGE-DDTM    TO BVVEND-E-AGENCIA
|          MOVE WRK-INP-CONTA-DDTM     TO BVVEND-E-CONTA
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
|          MOVE WRK-INP-OPERA-DDTM-X(IND-2)
|                                      TO WRK-OPER-BVVE
|          MOVE WRK-CONTRATO-BVVE      TO BVVEND-E-CONTRATO
|     *
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
F2404          MOVE WRK-INP-DANO-DDTM(IND-2)
F2404                                  TO BVVEND-E-OPER-ANO
F2404          MOVE WRK-INP-NSEQ-DDTM(IND-2)
F2404                                  TO BVVEND-E-OPER-SEQ
F2404      ELSE
|              IF WRK-DESPREZAR        EQUAL ZEROS
|                 MOVE 'S'             TO BVVEND-E-FIM-OCOR
|              ELSE
|                 MOVE SPACES          TO BVVEND-E-FIM-OCOR
|              END-IF
|     *
|              PERFORM VARYING IND-3   FROM 1 BY 1
CAP003                   UNTIL IND-3   GREATER 40
|                           OR IND-3   GREATER G8-QTDE-RETORNADA
|                 MOVE G8RT-DVCTO-TITLO-DESC(IND-3)
|                                      TO BVVEND-E-DATA(IND-3)
|                 MOVE G8RT-CADM-CATAO-CREDT(IND-3)
|                                      TO HX-NUM-09-CS
|                 MOVE HX-NUM-09-SS    TO HX-NRO-18
|                 MOVE HX-NRO-05       TO BVVEND-E-PRODUTO(IND-3)
|                 MOVE G8RT-VTITLO-DESC-COML(IND-3)
|                                      TO BVVEND-E-VLR-DESCONTO(IND-3)
|              END-PERFORM
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
 |                        MOVE 08      TO BVVEND-S-COD-RETORNO
 |                        MOVE 'S'     TO WRK-FLAG-ERRO
 |                        MOVE 'PROTOCOLO RETORNADO VAZIO DO BVVEA504'
 |                                     TO WRK-OUT-MENSA-DDTM
 |                                        WRK-MENSAGEM-ERRO
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
|              WHEN 08
CAP004*        ERRO DE CONSISTENCIA
|                   MOVE 'S'           TO WRK-FLAG-ERRO
CAP004              MOVE BVVEND-S-MSG-RETORNO
|                                      TO WRK-OUT-MENSA-DDTM
|                                         WRK-MENSAGEM-ERRO
|              WHEN 10
|                   MOVE 'S'           TO WRK-FLAG-ERRO
|                   MOVE 'EFETIVACAO DE AGENDA NA REGISTRADORA FORA DO H
|     -                  'ORARIO'
|                                      TO WRK-OUT-MENSA-DDTM
|                                         WRK-MENSAGEM-ERRO
|              WHEN 16
|                   MOVE 'S'           TO WRK-FLAG-ERRO
|                   MOVE BVVEND-S-MSG-RETORNO
|                                      TO WRK-OUT-MENSA-DDTM
|                                         WRK-MENSAGEM-ERRO
|              WHEN OTHER
|                   MOVE BVVEND-S-MSG-RETORNO
|                                      TO WRK-OUT-MENSA-DDTM
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
|                   MOVE 'EM LIBER'    TO WRK-OUT-SITANAL-DDTM(IND-2)
|              WHEN 3
|                   CONTINUE
|              WHEN 4
|                   MOVE 'VLR INDISP'  TO WRK-OUT-SITANAL-DDTM(IND-2)
|              WHEN OTHER
|                   MOVE 'REJEIT CIP'  TO WRK-OUT-SITANAL-DDTM(IND-2)
|          END-EVALUATE
|     *
|          MOVE WRK-SITUACAO           TO DCOMZH-E-CSIT-PROT-AGNDA
|          MOVE WRK-ACAO               TO DCOMZH-E-ACAO
|          MOVE WRK-INP-BANCO-DDTM     TO DCOMZH-E-CBCO
|          MOVE WRK-INP-CODAGE-DDTM    TO DCOMZH-E-CAG-BCRIA
|          MOVE WRK-INP-CONTA-DDTM     TO DCOMZH-E-CCTA-BCRIA-CLI
|          MOVE WRK-PROTOCOLO          TO DCOMZH-E-CPROT-SOLTC-AGNDA
|          MOVE 'E'                    TO DCOMZH-E-CTPO-PROT-AGNDA
|          MOVE 1                      TO DCOMZH-E-QTEMPO-VALDD-PROCM
|          MOVE 4174-SAI-NSMULA-OPER-DESC
|                                      TO DCOMZH-E-NSMULA-OPER-DESC
|          MOVE WRK-INP-DANO-DDTM(IND-2)
|                                      TO DCOMZH-E-DANO-OPER-DESC
|          MOVE WRK-INP-NSEQ-DDTM(IND-2)
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
|                                      TO WRK-OUT-MENSA-DDTM
|                                         WRK-MENSAGEM-ERRO
|                  PERFORM 9999-PROCESSAR-ROTINA-ERRO
|             WHEN OTHER
|                  MOVE 'APL'          TO ERR-TIPO-ACESSO
WIP001             MOVE DCOMZH-S-MSG-RETORNO(1:79)
|                                      TO WRK-OUT-MENSA-DDTM
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

           MOVE WRK-INP-CODAGE-DDTM    TO BVVEFX-E-AGENCIA
           MOVE WRK-INP-CONTA-DDTM     TO BVVEFX-E-CONTA

           MOVE 'BVVEA520'             TO WRK-MODULO
           CALL WRK-MODULO             USING BVVEFX-COMMAREA

           EVALUATE BVVEFX-S-COD-RETORNO
               WHEN 00
                    CONTINUE

               WHEN OTHER
                    MOVE 'N'           TO BVVEFX-S-IND-CLI-MASSV
                    MOVE BVVEFX-S-MSG-RETORNO
                                       TO WRK-OUT-MENSA-DDTM
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
               MOVE 'DCOM0404'         TO ERR-MODULO
           ELSE
               MOVE 'DCOM0404'         TO ERR-PGM
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
