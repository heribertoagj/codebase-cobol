      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. DCOM3933.
       AUTHOR.     MATHEUS SANTOS.
      *================================================================*
      *                         CAPGEMINI                              *
      *----------------------------------------------------------------*
      *    PROGRAMA......:  DCOM3933                                   *
      *    PROGRAMADOR...:  MATHEUS SANTOS    - CAPGEMINI              *
      *    ANALISTA......:  POLIANA SILVA     - CAPGEMINI              *
      *    ANALISTA DDS..:  RICARDO JAMMAL    - BRADESCO - GRUPO 39    *
      *    DATA..........:  17/08/2020                                 *
      *----------------------------------------------------------------*
      *    OBJETIVO....: EFETIVA/VERIFICA PROTOCOLO                    *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *       I#BRAD7C - AREA PARA TRATAMENTO DE ERROS DB2.            *
      *       I#DCOMSG - BOOK DE MENSAGENS.                            *
      *       I#DCOMHX - AREA PARA VARIAVEIS DE HEXAVISION.            *
      *       I#DCOMX3 - AREA DE COMUNICACAO DCOM6408                  *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *       BVVEA504 - SOLICITACAO EFETIVACAO                        *
      *       DCOM6408 - MODULO DE PESQUISA DADOS NO CADU              *
BRQ002*       DCOM5594 - MODULO DE SENSIBILIZACAO/ESTORNO DO CONE      *
CAP001*================================================================*
.     *                    ALTERACAO - CAPGEMINI                       *
.     *----------------------------------------------------------------*
.     *    ANALISTA....:   AMANDA BELTOSO                              *
.     *    DATA........:   07/05/2021                                  *
.     *----------------------------------------------------------------*
.     *    OBJETIVO....: - ALTERAR CODIGO RE RETORNO PARA 09 QUANDO    *
.     *                    FOR SALDO INSUFICIENTE                      *
CAP001*----------------------------------------------------------------*
CAP002*================================================================*
|     *                   ALTERACAO - CAPGEMINI                        *
|     *----------------------------------------------------------------*
|     *   PROGRAMADOR :   ANDREA             - CAPGEMINI               *
|     *   ANALISTA CAP:   ANDREA             - CAPGEMINI               *
|     *   ANALISTA DDS:   RICARDO JAMMAL     - BRADESCO - GRUPO 39     *
|     *   DATA........:   06/2021                                      *
|     *----------------------------------------------------------------*
|     *   OBJETIVO....: ALTERAR OCORRENCIAS NA CHAMADA DO BVVEA504     *
CAP002*                 DE 60 PARA 40                                  *
CAP003*================================================================*
|     *                   ALTERACAO - CAPGEMINI                        *
|     *----------------------------------------------------------------*
|     *   PROGRAMADOR :   AMANDA BELTOSO     - CAPGEMINI               *
|     *   ANALISTA CAP:   AMANDA BELTOSO     - CAPGEMINI               *
|     *   ANALISTA DDS:   RICARDO JAMMAL     - BRADESCO - GRUPO 39     *
|     *   DATA........:   06/2021                                      *
|     *----------------------------------------------------------------*
|     *   OBJETIVO....: FAZER TRATAMENTO PARA PROTOCOLO VAZIO RETORNADO*
CAP003*                 D0 BVVE                                        *
CAP004*================================================================*
|     *                   ALTERACAO - CAPGEMINI                        *
|     *----------------------------------------------------------------*
|     *   PROGRAMADOR :   AMANDA BELTOSO     - CAPGEMINI               *
|     *   ANALISTA CAP:   AMANDA BELTOSO     - CAPGEMINI               *
|     *   ANALISTA DDS:   RICARDO JAMMAL     - BRADESCO - GRUPO 39     *
|     *   DATA........:   06/2021                                      *
|     *----------------------------------------------------------------*
|     *   OBJETIVO....: FAZER VERIFICACAO DE CONE ANTES DE ENVIAR      *
CAP004*                 OPERACAO PARA O BVVE                           *
CAP005*================================================================*
|     *                   ALTERACAO - CAPGEMINI                        *
|     *----------------------------------------------------------------*
|     *   PROGRAMADOR :   AMANDA BELTOSO     - CAPGEMINI               *
|     *   ANALISTA CAP:   AMANDA BELTOSO     - CAPGEMINI               *
|     *   ANALISTA DDS:   RICARDO JAMMAL     - BRADESCO - GRUPO 39     *
|     *   DATA........:   07/2021                                      *
|     *----------------------------------------------------------------*
|     *   OBJETIVO....: FAZER VERIFICACAO DE LIMITE ANTES DE ENVIAR    *
CAP005*                 OPERACAO PARA O BVVE                           *
CAP006*================================================================*
|     *                   ALTERACAO - CAPGEMINI                        *
|     *----------------------------------------------------------------*
|     *   PROGRAMADOR :   ANDREA COUTINHO    - CAPGEMINI               *
|     *   ANALISTA CAP:   ANDREA COUTINHO    - CAPGEMINI               *
|     *   ANALISTA DDS:   RICARDO JAMMAL     - BRADESCO - GRUPO 39     *
|     *   DATA........:   08/2021                                      *
|     *----------------------------------------------------------------*
|     *   OBJETIVO....: ENVIAR MENSAGEM QDO TENTAR INSERIR PROTOCOLO DE*
CAP006*                 EFETIVACAO JA EXISTENTE NAS TABELAS DE PROTOCOL*
CAP007*================================================================*
|     *                   ALTERACAO - CAPGEMINI                        *
|     *----------------------------------------------------------------*
|     *   PROGRAMADOR :   RAFAEL CORSI       - CAPGEMINI               *
|     *   ANALISTA CAP:   ANDREA COUTINHO    - CAPGEMINI               *
|     *   ANALISTA DDS:   RICARDO JAMMAL     - BRADESCO - GRUPO 39     *
|     *   DATA........:   08/2021                                      *
|     *----------------------------------------------------------------*
|     *   OBJETIVO....: APOS ENQUADRAMENTO DE CONE, ATUALIZAR TABELA   *
CAP007*                 DE SIMULACAO COM A CARTEIRA CORRETA            *
      *================================================================*
WIP001*                     A L T E R A C A O                          *
WIP001*----------------------------------------------------------------*
WIP001*    PROGRAMADOR.:  DIRCEU RENATO MACAN                          *
WIP001*    ANALISTA....:  DIRCEU RENATO MACAN                          *
WIP001*    ANALISTA DS.:  DESIREE PESSOA DA SILVA                      *
WIP001*    DATA........:  01/12/2021                                   *
WIP001*----------------------------------------------------------------*
WIP001*    OBJETIVO....:  INCLUIR PROCESSO DE CONSULTA SALDO DO CONE.  *
WIP001*                   (MODULO CHAMADO: CONE0451)                   *
WIP001*                   IN6242129                                    *
WIP001*================================================================*
WIP002*                     A L T E R A C A O                          *
WIP002*----------------------------------------------------------------*
WIP002*    PROGRAMADOR.:  FABIO AUGUSTO FINK        - WIPRO            *
WIP002*    ANALISTA....:  FABIO AUGUSTO FINK        - WIPRO            *
WIP002*    ANALISTA DS.:  DESIREE PESSOA DA SILVA   - BRADESCO / RTB   *
WIP002*    DATA........:  15/12/2021                                   *
WIP002*----------------------------------------------------------------*
WIP002*    OBJETIVO....:  IN6242129 - OBTER A CARTEIRA PARAMETRIZADA   *
WIP002*      NO DCOM PARA ACESSO A ROTINA CONE.                        *
WIP002*================================================================*
BRQ001*                     A L T E R A C A O                          *
.     *----------------------------------------------------------------*
.     *    PROGRAMADOR.:  LUIS EDUARDO              - BRQ              *
.     *    ANALISTA....:  LUIS EDUARDO              - BRQ              *
.     *    ANALISTA DS.:  BETO / JAMMAL             - G439             *
.     *    DATA........:  20/04/2021                                   *
.     *----------------------------------------------------------------*
.     *    OBJETIVO.: 1 - AJUSTE NA CONSULTA DO PROTOCOLO EXISTENTE    *
.     *               2 - ATUALIZA SIT PROTOCOLO P/ 3-PROCESSADO NO    *
.     *                   RETORNO DO DCOM3934                          *
BRQ001*================================================================*
BRQ002*                     A L T E R A C A O                          *
.     *----------------------------------------------------------------*
.     *    PROGRAMADOR.:  LUIS EDUARDO              - BRQ              *
.     *    ANALISTA....:  LUIS EDUARDO              - BRQ              *
.     *    ANALISTA DS.:  BETO / JAMMAL             - G439             *
.     *    DATA........:  19/05/2021                                   *
.     *----------------------------------------------------------------*
.     *    OBJETIVO.:     SENSIBILIZAR CONE EM TEMPO DE SIMULACAO,     *
.     *                   APOS GERACAO DO PROTOCOLO (DCOMB0P5)         *
BRQ002*================================================================*
BRQ003*                     A L T E R A C A O                          *
.     *----------------------------------------------------------------*
.     *    PROGRAMADOR.:  GABRIEL SALIM             - BRQ              *
.     *    ANALISTA....:  LUIS EDUARDO              - BRQ              *
.     *    ANALISTA DS.:  BETO / JAMMAL             - G439             *
.     *    DATA........:  AGOSTO/2022                                  *
.     *----------------------------------------------------------------*
.     *    OBJETIVO.:     CORRIGIR ENVIO DA CARTEIRA E GRAVACAO DE LOG *
BRQ003*================================================================*
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
       77  FILLER                      PIC  X(050)         VALUE
           '*** INICIO DA WORKING DCOM3933 ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA INDEXADORES ***'.
      *----------------------------------------------------------------*

       77  IND-1                       PIC  9(003) COMP-3  VALUE ZEROS.


      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*

       77  WRK-SQLCA                   PIC  X(136)         VALUE SPACES.
       77  WRK-CSIT-PROT-AGNDA         PIC  9(001)         VALUE ZEROS.
       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-TIPO-LOG                PIC  X(004)         VALUE SPACES.
       77  WRK-PROTOCOLO               PIC  X(050)         VALUE SPACES.
       77  WRK-TEMPO-ESPERA            PIC  9(003) COMP-3  VALUE ZEROS.
       77  WRK-SEGUNDOS-COMP           PIC  9(003) COMP-3  VALUE ZEROS.
       77  WRK-MINUTO-COMP             PIC  9(003) COMP-3  VALUE ZEROS.
       77  WRK-NTITLO-DESC-COML        PIC  9(005)         VALUE ZEROS.
CAP002 77  WRK-DESPREZAR               PIC  9(005) COMP-3  VALUE ZEROS.
CAP002 77  WRK-FIM                     PIC  X(001)         VALUE SPACES.
BRQ002 77  WRK-FUNC-CONE               PIC  X(001)         VALUE SPACES.
TESTE  77  WRK-LIMITE                  PIC  X(001)         VALUE 'N'.
       01  WRK-CPF-X                   PIC  X(015)         VALUE SPACES.
       01  FILLER REDEFINES            WRK-CPF-X.
4S2511*    05 WRK-CPF                  PIC  9(009).
4S2511     05 WRK-CPF                  PIC  X(09).
4S2511*    05 WRK-CFLIAL-CPF           PIC  9(004).
4S2511     05 WRK-CFLIAL-CPF           PIC  X(04).
           05 WRK-CCTRL-CPF            PIC  9(002).
      *
CAP306 01  WRK-CCHAVE-B0E5.
.          03  WRK-NUM-SIMULACAO       PIC  9(009)         VALUE ZEROS.
.          03  WRK-ANO-ATUAL-B0E5      PIC  9(004)         VALUE ZEROS.
.          03  WRK-NUM-OPERACAO        PIC  9(009)         VALUE ZEROS.
CAP306     03  FILLER                  PIC  X(023)         VALUE SPACES.

       01  WRK-OPERACAO                PIC 9(013)          VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-OPERACAO.
           05 WRK-DANO                 PIC  9(004).
           05 WRK-NSEQ                 PIC  9(009).
      *
       01  WRK-CONTRATO-BVVE.
           05 WRK-SIGLA-CCUSTO         PIC  X(004)         VALUE SPACES.
           05 WRK-DANO-OPER            PIC  9(004)         VALUE ZEROS.
           05 WRK-NSEQ-OPER            PIC  9(009)         VALUE ZEROS.
           05 FILLER                   PIC  X(002)         VALUE SPACES.
      *
       01  WRK-TEMPO-BVVE              PIC  X(008)         VALUE SPACES.
       01  FILLER                      REDEFINES WRK-TEMPO-BVVE.
           05 WRK-HORA                 PIC  9(002).
           05 FILLER                   PIC  X(001).
           05 WRK-MINUTO               PIC  9(002).
           05 FILLER                   PIC  X(001).
           05 WRK-SEGUNDOS             PIC  9(002).
      *
      *
       01  WRK-NSEQ-OPER-15            PIC  9(015)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-NSEQ-OPER-15.
           05 FILLER                   PIC  9(006).
           05 WRK-NSEQ-OPER-09         PIC  9(009).
      *
       01  WRK-TIMESTAMP-ATUAL.
           05  WRK-ANO-TMSTAMP         PIC  9(004)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  WRK-MES-TMSTAMP         PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  WRK-DIA-TMSTAMP         PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(016)         VALUE SPACES.

       01  WRK-DATA-ATUAL              PIC  X(010)         VALUE SPACES.

       01  WRK-NULIDADE.
           05  WRK-DANO-OPER-DESC-N     PIC S9(004) COMP    VALUE ZEROS.
           05  WRK-NSEQ-OPER-DESC-N     PIC S9(004) COMP    VALUE ZEROS.

       01 WRK-COD-RETORNO              PIC X(004)          VALUE SPACES.
       01 FILLER                       REDEFINES WRK-COD-RETORNO.
          05 WRK-COD-RET               PIC 9(004).
      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA GRAVACAO DE LOG ***'.
      *---------------------------------------------------------------*

       01  WRK-LOG-CONE.
           03  WRK-DCOMBA-AREA.
               05 WRK-LOG-COD-RETORNO         PIC  9(004)   VALUE ZEROS.
               05 WRK-LOG-MSG-RETORNO         PIC  X(075)  VALUE SPACES.
               05 WRK-LOG-RESTART             PIC  9(005)   VALUE ZEROS.
               05 WRK-LOG-FLAG                PIC  X(001)  VALUE SPACES.
               05 WRK-LOG-CFUNC-BDSCO         PIC  9(009)   VALUE ZEROS.
               05 WRK-LOG-CTERM               PIC  X(008)  VALUE SPACES.
               05 WRK-LOG-DADOS.
                  10 WRK-LOG-NUM-SIMULACAO    PIC  9(009)   VALUE ZEROS.
                  10 WRK-LOG-COD-FUNCAO       PIC  X(001)  VALUE SPACES.
                  10 WRK-LOG-DANO-OPER-DESC   PIC  9(004)   VALUE ZEROS.
                  10 WRK-LOG-NSEQ-OPER-DESC   PIC  9(009)   VALUE ZEROS.
      *- CHAVE DO CONE
                  10 WRK-LOG-DANO-BASE        PIC  9(004)   VALUE ZEROS.
                  10 WRK-LOG-CDEPDC           PIC  9(005)   VALUE ZEROS.
                  10 WRK-LOG-CSEQ-STUDO       PIC  9(009)   VALUE ZEROS.
                  10 WRK-LOG-CCART-LIM        PIC  X(005)  VALUE SPACES.
           03  WRK-LOG-TXT                    PIC  X(004)  VALUE SPACES.

       01  WRK-LOG-CCHAVE-ELMTO.
           05 WRK-LOG-DANO-OPER               PIC  9(004)   VALUE ZEROS.
           05 WRK-LOG-NSEQ-OPER               PIC  9(009)   VALUE ZEROS.
           05 FILLER                          PIC  X(032)  VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DO SBAT3300 ***'.
      *---------------------------------------------------------------*

       01  WRK-GU                      PIC  X(004)         VALUE 'GU'.
       01  WRK-MSGF                    PIC  X(004)         VALUE 'MSGF'.
       01  WRK-ROLB                    PIC  X(004)         VALUE 'ROLB'.
       01  WRK-FUNCAO                  PIC  X(004)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA MENSAGENS ***'.
      *----------------------------------------------------------------*

       01 WRK-MENSAGENS.
           05  WRK-MSG02.
               10 FILLER               PIC  X(044)         VALUE
                  'ERRO NA CHAMADA DO MOD. SBAT3300 - FUNCAO = '.
               10 WRK-MSG02-FUNCAO     PIC  X(004)         VALUE SPACES.
               10 FILLER               PIC  X(012)         VALUE
                  ' - STATUS = '.
               10 WRK-STATUS           PIC  X(002)         VALUE SPACES.

           05  WRK-MSG084              PIC  X(079)         VALUE
               'AGUARDE, AGENDA EM PROCESSAMENTO'.
           05  WRK-MSG086              PIC  X(079)         VALUE
               'ERRO AO OBTER O NUMERO DA OPERACAO'.
           05  WRK-MSG090              PIC  X(079)         VALUE
               'AGENDA DE RECEBIVEIS DISPONIVEL PARA CONSULTA'.
           05  WRK-MSG091              PIC  X(079)         VALUE
               'ERRO DE CONSISTENCIA'.
           05  WRK-MSG092              PIC  X(079)         VALUE
               'OPERACAO REJEITADA PELA REGISTRADORA'.
           05  WRK-MSG095              PIC  X(079)         VALUE
               'ERRO DE ACESSO'.
           05  WRK-MSG096              PIC  X(079)         VALUE
               'GRADE FECHADA'.
           05  WRK-MSG097              PIC  X(079)         VALUE
               'NAO FOI POSSIVEL CONSULTAR A AGENDA NA REGISTRADORA'.
           05  WRK-MSG098              PIC  X(079)         VALUE
               'OPERACAO INDISPONIVEL, LIBERACAO EM PROCESSAMENTO'.
           05  WRK-MSG099              PIC  X(079)         VALUE
               'SALDO INSUFICIENTE, FAVOR REFAZER A OPERACAO'.
           05  WRK-MSG100              PIC  X(079)         VALUE
               'OPERACAO REJEITADA, REFACA OPERACAO'.
TESTE      05  WRK-MSG1183             PIC  X(079)         VALUE
               'VALOR ACIMA DO LIMITE. PENDENTE DE LIBERACAO PELO GERENT
      -        'E AGENCIA'.
CAP006     05  WRK-MSG1184             PIC  X(079)         VALUE
               'OPERACAO JA SOLICITADA A LIBERACAO. VERIFICAR TRANSACOES
      -        ' DO DIA'.
WIP001     05  WRK-MSG1185             PIC  X(079)         VALUE
WIP001         'CONSULTA CONE PENDENTE DE AUTORIZACAO OU INEXISTENTE'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA VARIAVEIS DE HEXAVISION ***'.
      *----------------------------------------------------------------*

       COPY 'I#DCOMHX'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA MENSAGENS DO SISTEMA DCOM ***'.
      *----------------------------------------------------------------*

       COPY 'I#DCOMSG'.
      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA BRAD7100 ***'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ACESSO AO SBAT3300 ***'.
      *----------------------------------------------------------------*

       COPY 'I#ISDDPC'.
      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ACESSO AO MODULO DCOM6408 **'.
      *----------------------------------------------------------------*
      *
       COPY 'I#DCOMX3'.
      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ACESSO AO MODULO DCOM7225 **'.
      *----------------------------------------------------------------*
      *
       COPY 'I#DCOME5'.
      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ACESSO AO MODULO DCOM7236 **'.
      *----------------------------------------------------------------*
      *
       COPY 'I#DCOME7'.
      *
WIP001*----------------------------------------------------------------*
WIP001 01  FILLER                      PIC  X(050)         VALUE
WIP001     '*** AREA DE ACESSO AO MODULO CONE0451 ***'.
WIP001*----------------------------------------------------------------*
WIP001*
WIP001 COPY 'I#CONECQ'.
WIP001*
WIP002*----------------------------------------------------------------*
WIP002 01  FILLER                      PIC  X(050)         VALUE
WIP002     '*** AREA DE ACESSO A TABELA DCOMB064 ***'.
WIP002*----------------------------------------------------------------*
WIP002
WIP002 COPY 'I#DCOM64'.
WIP002
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA ACESSO AO MODULO BVVEA501 ***'.
      *----------------------------------------------------------------*

       COPY I#BVVENA.
      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA ACESSO AO MODULO BVVEA504 ***'.
      *----------------------------------------------------------------*

       COPY I#BVVEND.
      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA ACESSO AO MODULO BVVEA505 ***'.
      *----------------------------------------------------------------*

       COPY I#BVVENE.

F2404 *----------------------------------------------------------------*
F2404  01  FILLER                      PIC  X(050)         VALUE
F2404      '*** AREA DO BVVEA520 ***'.
F2404 *----------------------------------------------------------------*
F2404
F2404  COPY 'I#BVVEFX'.

BRQ002*----------------------------------------------------------------*
.      01  FILLER                      PIC  X(050)         VALUE
.          '*** AREA PARA ACESSO AO MODULO DCOM5594 ***'.
.     *----------------------------------------------------------------*
.
.      COPY 'I#DCOMBA'.
BRQ002*
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ACESSO AO MODULO DCOM5075 ***'.
      *----------------------------------------------------------------*

       COPY 'I#DCOMX1'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO DCOM3934 *'.
      *----------------------------------------------------------------*
      *
       01  WRK-3934-ENTRADA.
           05 WRK-3934E-CANAL          PIC  X(008)         VALUE SPACES.
           05 WRK-3934E-CBCO           PIC  9(003)         VALUE ZEROS.
           05 WRK-3934E-CAG-BCRIA      PIC  9(005)         VALUE ZEROS.
           05 WRK-3934E-CCTA-BCRIA-CLI
                                       PIC  9(013)         VALUE ZEROS.
4S2511*    05 WRK-3934E-CCNPJ-CPF      PIC  9(009)         VALUE ZEROS.
4S2511     05 WRK-3934E-CCNPJ-CPF      PIC  X(09)           VALUE SPACES.
4S2511*    05 WRK-3934E-CFLIAL-CNPJ    PIC  9(004)         VALUE ZEROS.
4S2511     05 WRK-3934E-CFLIAL-CNPJ    PIC  X(04)           VALUE SPACES.
           05 WRK-3934E-CCTRL-CNPJ-CPF
                                       PIC  9(002)         VALUE ZEROS.
           05 WRK-3934E-NRO-SIMULA     PIC  9(009)         VALUE ZEROS.
           05 WRK-3934E-MIDIA          PIC  9(001)         VALUE ZEROS.
      *--->  (3=NET EMPRESA,5=MOBILE)
           05 WRK-3934E-DANO-OPER      PIC  9(004)         VALUE ZEROS.
           05 WRK-3934E-NSEQ-OPER      PIC  9(009)        VALUE ZEROS.
      *
       01  WRK-3934-SAIDA.
           05 WRK-3934S-COD-RETORNO    PIC  9(004)         VALUE ZEROS.
           05 WRK-3934S-MSG-RETORNO    PIC  X(079)         VALUE SPACES.
           05 WRK-3934S-CBCO           PIC  9(003)         VALUE ZEROS.
           05 WRK-3934S-CAG-BCRIA      PIC  9(005)         VALUE ZEROS.
           05 WRK-3934S-CCTA-BCRIA-CLI
                                       PIC  9(013)         VALUE ZEROS.
           05 WRK-3934S-DIG-CONTA      PIC  X(002)         VALUE SPACES.
4S2511*    05 WRK-3934S-CCNPJ-CPF      PIC  9(009)         VALUE ZEROS.
4S2511     05 WRK-3934S-CCNPJ-CPF      PIC  X(09)          VALUE SPACES.
4S2511*    05 WRK-3934S-CFLIAL-CNPJ    PIC  9(004)         VALUE ZEROS.
4S2511     05 WRK-3934S-CFLIAL-CNPJ    PIC  X(04)          VALUE SPACES.
           05 WRK-3934S-CCTRL-CNPJ-CPF
                                       PIC  9(002)         VALUE ZEROS.
           05 WRK-3934S-DANO-OPER      PIC  9(004)         VALUE ZEROS.
           05 WRK-3934S-NSEQ-OPER      PIC  9(009)         VALUE ZEROS.
           05 WRK-3934S-DINIC-OPER     PIC  X(010)         VALUE SPACES.
           05 WRK-3934S-VOPER-DESC     PIC  9(015)V9(002)  VALUE ZEROS.
           05 WRK-3934S-VIOF-OPER      PIC  9(015)V9(002)  VALUE ZEROS.
           05 WRK-3934S-VTAC-OPER      PIC  9(015)V9(002)  VALUE ZEROS.
           05 WRK-3934S-VJURO-OPER     PIC  9(015)V9(002)  VALUE ZEROS.
           05 WRK-3934S-PTX-JURO-MES   PIC 9(003)V9(006)  VALUE ZEROS.
           05 WRK-3934S-CINDCD-FREQ-TX
                                       PIC  X(002)         VALUE SPACES.
           05 WRK-3934S-VLIQ-OPER      PIC  9(015)V9(002)  VALUE ZEROS.
           05 WRK-3934S-DVCTO-PRI-ORP  PIC X(010)        VALUE SPACES.
           05 WRK-3934S-DVCTO-ULT-ORP  PIC X(010)        VALUE SPACES.
           05 WRK-3934S-QTDE-ORPAGS    PIC  9(005)         VALUE ZEROS.
           05 WRK-3934S-TMED-PONDE     PIC  9(003)         VALUE ZEROS.
           05 WRK-3934S-PTX-CET-MES    PIC  9(003)V9(006)  VALUE ZEROS.
           05 WRK-3934S-PTX-CET-ANO    PIC  9(003)V9(006)  VALUE ZEROS.
           05 WRK-3934S-PEND-GERENTE   PIC X(001)         VALUE SPACES.
      *--->  ('S'-PENDENTE DE APROVACAO DO GERENTE)
      *--->  ('N'-OPERACAO LIBERADA )

      *
CAP004*----------------------------------------------------------------*
.      01  FILLER                      PIC  X(050)         VALUE
.          '*** AREA PARA ACESSO AO MODULO DCOM5872 ***'.
.     *----------------------------------------------------------------*
.
.      01  WRK-5872-ENTRADA.
.          05 WRK-5872E-COD-RETORNO    PIC  X(004)         VALUE SPACES.
.          05 WRK-5872E-MSG-RETORNO    PIC  X(079)         VALUE SPACES.
.          05 WRK-5872E-RESTART        PIC  9(005)         VALUE ZEROS.
.          05 WRK-5872E-FLAG           PIC  X(001)         VALUE SPACES.
.          05 WRK-5872E-CFUNC-BDSCO    PIC  9(009)         VALUE ZEROS.
.          05 WRK-5872E-CTERM          PIC  X(008)         VALUE SPACES.
.          05 WRK-5872E-CPRODT         PIC  9(003)         VALUE ZEROS.
.          05 WRK-5872E-CSPROD-DESC    PIC  9(003)         VALUE ZEROS.
.          05 WRK-5872E-CMEIO-ENTRD    PIC  9(003)         VALUE ZEROS.
.          05 WRK-5872E-CAG-BCRIA      PIC  9(005)         VALUE ZEROS.
.          05 WRK-5872E-CCNPJ-CPF      PIC  9(009)         VALUE ZEROS.
.          05 WRK-5872E-CFLIAL-CNPJ    PIC  9(004)         VALUE ZEROS.
.          05 WRK-5872E-CCTRL-CNPJ-CPF PIC  9(002)         VALUE ZEROS.
.          05 WRK-5872E-VOPER-DESC-COML
.                                      PIC  9(015)V9(002)  VALUE ZEROS.
.          05 WRK-5872E-DINIC-OPER-DESC
.                                      PIC  X(010)         VALUE SPACES.
.
.      01  WRK-5872-SAIDA.
.          05 WRK-5872S-COD-RETORNO    PIC  9(004)         VALUE ZEROS.
.          05 WRK-5872S-MSG-RETORNO    PIC  X(079)         VALUE SPACES.
.          05 WRK-5872S-RESTART        PIC  9(005)         VALUE ZEROS.
.          05 WRK-5872S-FLAG           PIC  X(001)         VALUE SPACES.
.          05 WRK-5872S-CINDCD-CONS-CREDT
.                                      PIC  X(001)         VALUE SPACES.
.          05 WRK-5872S-DANO-BASE      PIC  9(004)         VALUE ZEROS.
.          05 WRK-5872S-CDEPDC         PIC  9(005)         VALUE ZEROS.
.          05 WRK-5872S-CSEQ-STUDO     PIC  9(009)         VALUE ZEROS.
.          05 WRK-5872S-CCART-LIM      PIC  X(005)         VALUE SPACES.
.          05 WRK-5872S-CGARNT         PIC  9(003)         VALUE ZEROS.
.          05 WRK-5872S-IGARNT         PIC  X(030)         VALUE SPACES.
.          05 WRK-5872S-VALOR-CONSULTA PIC  9(015)V9(002)  VALUE ZEROS.
.          05 WRK-5872S-SALDO-LIMITE   PIC  9(015)V9(002)  VALUE ZEROS.
.          05 WRK-5872S-SALDO-PLUS     PIC  9(015)V9(002)  VALUE ZEROS.
.          05 WRK-5872S-DVCTO-LIMITE   PIC  X(010)         VALUE SPACES.
.          05 WRK-5872S-CSIT-CONS-NEG  PIC  9(003)         VALUE ZEROS.
.          05 WRK-5872S-ISIT-CONS-NEG  PIC  X(030)         VALUE SPACES.
CAP004
CAP005*----------------------------------------------------------------*
.      01  FILLER                      PIC  X(050)         VALUE
.          '*** AREA PARA ACESSO AO MODULO DCOM6439 ***'.
.     *----------------------------------------------------------------*
.      01  WRK-6439-ENTRADA.
.          05 WRK-E6439-COD-RETORNO        PIC  X(004)     VALUE SPACES.
.          05 WRK-E6439-MSG-RETORNO        PIC  X(079)     VALUE SPACES.
.     *---- C - CONSULTA LIMITE
.     *---- V - CONSULTA LIMITE E VALIDA VALOR INFORMADO
.          05 WRK-E6439-TP-PESQUISA        PIC  X(001)     VALUE SPACES.
.          05 WRK-E6439-CPRODT             PIC  9(003)     VALUE ZEROS.
.          05 WRK-E6439-CSPROD-DESC-COML   PIC  9(003)     VALUE ZEROS.
.          05 WRK-E6439-CTPO-DESC-COML     PIC  9(003)     VALUE ZEROS.
.          05 WRK-E6439-CORIGE-OPER-DESC   PIC  9(001)     VALUE ZEROS.
.          05 WRK-E6439-CCNPJ-CPF          PIC  9(009)     VALUE ZEROS.
.          05 WRK-E6439-CFLIAL-CNPJ        PIC  9(004)     VALUE ZEROS.
.          05 WRK-E6439-CCTRL-CNPJ-CPF     PIC  9(002)     VALUE ZEROS.
.          05 WRK-E6439-CBCO               PIC  9(003)     VALUE ZEROS.
.          05 WRK-E6439-CAG-BCRIA          PIC  9(005)     VALUE ZEROS.
.          05 WRK-E6439-CCTA-BCRIA-CLI     PIC  9(013)     VALUE ZEROS.
.     *---- CAMPO PARA VERIFICAR SE CLIENTE POSSUI LIMITE PARA OPERAR
.          05 WRK-E6439-VLR-OPERACAO       PIC S9(015)V99  VALUE ZEROS.
.
.      01  WRK-6439-SAIDA.
.          05 WRK-S6439-COD-RETORNO        PIC  9(004)     VALUE ZEROS.
.          05 WRK-S6439-MSG-RETORNO        PIC  X(079)     VALUE SPACES.
.          05 WRK-S6439-TP-LIMITE          PIC  X(001)     VALUE ZEROS.
.     *----   'C' - POSSUI LIMITE CLIENTE
.     *----   'P' - POSSUI LIMITE DO PRODUTO/SEGMENTO
.     *----   ' ' - LIMITE NAO PARAMETRIZADO
.          05 WRK-S6439-IND-TEM-LIMITE     PIC  X(001)     VALUE ZEROS.
.     *----   'S' - POSSUI LIMITE DISPONIVEL PARA O VALOR INFORMADO
.     *----   'N' - SEM LIMITE DISPONIVEL PARA O VALOR INFORMADO
.     *----   ' ' - NAO POSSUI LIMITE CADASTRADO
.          05 WRK-S6439-VLR-LIM-PROD       PIC S9(015)V99  VALUE ZEROS.
.          05 WRK-S6439-VLR-LIM-CLI        PIC S9(015)V99  VALUE ZEROS.
.          05 WRK-S6439-VLR-UTILIZADO      PIC S9(015)V99  VALUE ZEROS.
.          05 WRK-S6439-VLR-DISPONIVEL     PIC S9(015)V99  VALUE ZEROS.
.          05 WRK-S6439-VLR-OPERACAO       PIC S9(015)V99  VALUE ZEROS.
CAP005
      *----------------------------------------------------------------*
       01  FILLER                        PIC  X(050)         VALUE
           '*** AREA DE RECEPCAO/ENVIO DE MSGS FRONT-END ***'.
      *----------------------------------------------------------------*
       01 WRK-AREA-ENTRADA.
           05 WRK-ENT-LL                 PIC S9(004) COMP  VALUE ZEROS.
           05 WRK-ENT-ZZ                 PIC S9(004) COMP  VALUE ZEROS.
           05 WRK-ENT-TRANSACAO          PIC  X(009)       VALUE SPACES.
           05 WRK-ENT-DADOS.
             10 WRK-ENT-CANAL            PIC  X(008)       VALUE SPACES.
             10 WRK-ENT-BANCO            PIC  9(003)       VALUE ZEROS.
             10 WRK-ENT-CAG-BCRIA        PIC  9(005)       VALUE ZEROS.
             10 WRK-ENT-CCTA-BCRIA-CLI   PIC  9(013)       VALUE ZEROS.
             10 WRK-ENT-NSMULA-OPER      PIC  9(009)       VALUE ZEROS.
             10 WRK-ENT-PROTOCOLO        PIC  X(050)       VALUE SPACES.
       01 WRK-AREA-SAIDA.
           05 WRK-SAI-LL                 PIC S9(004) COMP  VALUE ZEROS.
           05 WRK-SAI-ZZ                 PIC S9(004) COMP  VALUE ZEROS.
           05 WRK-SAI-DADOS.
             10 WRK-SAI-COD-RETORNO      PIC  9(004)       VALUE ZEROS.
             10 WRK-SAI-MSG-RETORNO      PIC  X(079)       VALUE SPACES.
             10 WRK-SAI-COD-BNCO         PIC  9(003)       VALUE ZEROS.
             10 WRK-SAI-CAG-BCRIA        PIC  9(005)       VALUE ZEROS.
             10 WRK-SAI-CCTA-BCRIA-CLI   PIC  9(013)       VALUE ZEROS.
4S2511*      10 WRK-SAI-CPF              PIC  9(009)       VALUE ZEROS.
4S2511       10 WRK-SAI-CPF              PIC  X(09)        VALUE SPACES.
4S2511*      10 WRK-SAI-CFLIAL-CPF       PIC  9(004)       VALUE ZEROS.
4S2511       10 WRK-SAI-CFLIAL-CPF       PIC  X(04)        VALUE SPACES.
            10 WRK-SAI-CCTRL-CPF        PIC  9(002)       VALUE ZEROS.
             10 WRK-SAI-OPERACAO         PIC  9(013)       VALUE ZEROS.
             10 WRK-SAI-PROTOCOLO        PIC  X(050)       VALUE SPACES.
             10 WRK-SAI-SIT-PROTOCOLO    PIC  X(001)       VALUE SPACES.
             10 WRK-SAI-TEMPO-ESPERA     PIC  X(008)       VALUE SPACES.
      *
      *----------------------------------------------------------------*
       01  FILLER   PIC X(32) VALUE 'INCLUDES'.
      *----------------------------------------------------------------*
           EXEC SQL
             INCLUDE SQLCA
           END-EXEC.
      *
      *--> TPROT_SOLTC_AGNDA
           EXEC SQL
             INCLUDE DCOMB0P5
           END-EXEC.
      *
      *--> TPROT_SOLTC_CANAL
           EXEC SQL
             INCLUDE DCOMB0P6
           END-EXEC.
      *
CAP306*--> TSMULA_OPER_DESC
.          EXEC SQL
.            INCLUDE DCOMB0E5
CAP306     END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** FIM DA WORKING DCOM3933 ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*
      *
       01  LNK-IO-PCB.
           05 LNK-IO-LTERM           PIC  X(008).
           05 FILLER                 PIC  X(002).
           05 LNK-IO-STATUS          PIC  X(002).
           05 FILLER                 PIC  X(012).
           05 LNK-IO-MODNAME         PIC  X(008).

       01  LNK-ALT-PCB.
           05 LNK-ALT-LTERM          PIC  X(008).
           05 FILLER                 PIC  X(002).
           05 LNK-ALT-STATUS         PIC  X(002).
           05 FILLER                 PIC  X(012).
           05 LNK-ALT-MODNAME        PIC  X(008).
      *
      *================================================================*
       PROCEDURE                      DIVISION.
      *================================================================*

           ENTRY 'DLITCBL'           USING LNK-IO-PCB
                                           LNK-ALT-PCB.
      *
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-RECEBER-MENSAGEM.

           PERFORM 1100-OBTER-TIMESTAMP

           PERFORM 2000-CONSISTIR-ENTRADA.

           PERFORM 3000-PROCESSAR.

           PERFORM 4000-ENVIAR-MENSAGEM.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
       1000-RECEBER-MENSAGEM           SECTION.
      *---------------------------------------------------------------*

           INITIALIZE                  WRK-AREA-ENTRADA
                                       WRK-AREA-SAIDA
                                       ERRO-AREA
                                       WRK-SQLCA

           CALL 'SBAT3300'             USING WRK-GU
                                             LNK-IO-PCB
                                             LNK-ALT-PCB
                                             MCA-ISDPA
                                             WRK-AREA-ENTRADA.

           IF  MPA-RCODE-API           EQUAL SPACES
               NEXT SENTENCE
           ELSE
               IF  MPA-RCODE-API       EQUAL 'QC'
                   MOVE WRK-MSGF       TO WRK-FUNCAO
                   PERFORM 5000-FINALIZAR
               ELSE
                   MOVE '02'           TO MPA-RCODE-APLICACAO
                   MOVE '0010'         TO ERR-LOCAL
                   MOVE MPA-RCODE-API  TO ERR-STA-CODE
                                          WRK-STATUS
                   MOVE WRK-GU         TO WRK-MSG02-FUNCAO
                   MOVE WRK-MSG02      TO WRK-SAI-MSG-RETORNO
                   MOVE 0100           TO WRK-SAI-COD-RETORNO
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO
               END-IF
           END-IF.

           MOVE LENGTH                 OF WRK-AREA-SAIDA
                                       TO WRK-SAI-LL.
           MOVE ZEROS                  TO WRK-SAI-COD-RETORNO.
           MOVE SG-MENSAGEM(1)         TO WRK-SAI-MSG-RETORNO.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *     OBTER DATA E HORA DO SISTEMA                               *
      *----------------------------------------------------------------*
       1100-OBTER-TIMESTAMP            SECTION.
      *----------------------------------------------------------------*

           MOVE 'DCOM7999'             TO WRK-MODULO.

           CALL WRK-MODULO             USING WRK-TIMESTAMP-ATUAL
                                             ERRO-AREA
                                             WRK-SQLCA.

           STRING WRK-TIMESTAMP-ATUAL(9:2) '.'
                  WRK-TIMESTAMP-ATUAL(6:2) '.'
                  WRK-TIMESTAMP-ATUAL(1:4)
           DELIMITED BY SIZE           INTO WRK-DATA-ATUAL.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       2000-CONSISTIR-ENTRADA          SECTION.
      *----------------------------------------------------------------*

           IF  WRK-ENT-CANAL           EQUAL SPACES
           OR  WRK-ENT-CANAL           EQUAL LOW-VALUES
               MOVE 08                 TO WRK-SAI-COD-RETORNO
               MOVE WRK-MSG091         TO WRK-SAI-MSG-RETORNO
               MOVE WRK-MSGF           TO WRK-FUNCAO
               PERFORM 4000-ENVIAR-MENSAGEM
           END-IF.

           IF  WRK-ENT-CAG-BCRIA       NOT NUMERIC OR
               WRK-ENT-CAG-BCRIA       EQUAL ZEROS
               MOVE 08                 TO WRK-SAI-COD-RETORNO
               MOVE WRK-MSG091         TO WRK-SAI-MSG-RETORNO
               MOVE WRK-MSGF           TO WRK-FUNCAO
               PERFORM 4000-ENVIAR-MENSAGEM
           END-IF.

           IF  WRK-ENT-CCTA-BCRIA-CLI  NOT NUMERIC OR
               WRK-ENT-CCTA-BCRIA-CLI  EQUAL ZEROS
               MOVE 08                 TO WRK-SAI-COD-RETORNO
               MOVE WRK-MSG091         TO WRK-SAI-MSG-RETORNO
               MOVE WRK-MSGF           TO WRK-FUNCAO
               PERFORM 4000-ENVIAR-MENSAGEM
           END-IF.
      *
      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 3050-MOVER-DADOS-FIXOS

           IF WRK-ENT-PROTOCOLO        EQUAL SPACES
             PERFORM 3100-VALIDA-OPERACAO
             PERFORM 3200-OBTER-DADOS-ADICIONAIS
           ELSE
BRQ002       PERFORM 3210-ACESSAR-DCOMB0E5
BRQ003       PERFORM 3220-OBTER-CPF
BRQ003       PERFORM 3230-OBTER-DATA-VALOR
BRQ003       PERFORM 3238-OBTER-CARTEIRA-DCOM
             PERFORM 3350-ACESSA-BVVEA501
           END-IF
           .
      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       3050-MOVER-DADOS-FIXOS          SECTION.
      *----------------------------------------------------------------*

            MOVE WRK-ENT-BANCO         TO WRK-SAI-COD-BNCO
            MOVE WRK-ENT-CAG-BCRIA     TO WRK-SAI-CAG-BCRIA
            MOVE WRK-ENT-CCTA-BCRIA-CLI
                                       TO WRK-SAI-CCTA-BCRIA-CLI
           .
      *----------------------------------------------------------------*
       3050-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       3100-VALIDA-OPERACAO            SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  DCOMB0P5

           MOVE 237                    TO CBCO
                                       OF DCOMB0P5
      *
           MOVE WRK-ENT-CAG-BCRIA      TO CAG-BCRIA
                                       OF DCOMB0P5
      *
           MOVE WRK-ENT-CCTA-BCRIA-CLI
                                       TO CCTA-BCRIA-CLI
                                       OF DCOMB0P5
      *
           MOVE 'E'                    TO CTPO-PROT-AGNDA
                                       OF DCOMB0P5
      *
           MOVE WRK-ENT-NSMULA-OPER    TO NSMULA-OPER-DESC
                                       OF DCOMB0P5
      *
           EXEC SQL
                SELECT
                      CPROT_SOLTC_AGNDA,
                      CSIT_PROT_AGNDA
                INTO  :DCOMB0P5.CPROT-SOLTC-AGNDA
                     ,:DCOMB0P5.CSIT-PROT-AGNDA
                FROM DB2PRD.TPROT_SOLTC_AGNDA
                WHERE CBCO             = :DCOMB0P5.CBCO
                AND   CAG_BCRIA        = :DCOMB0P5.CAG-BCRIA
                AND   CCTA_BCRIA_CLI   = :DCOMB0P5.CCTA-BCRIA-CLI
                AND   CTPO_PROT_AGNDA  = :DCOMB0P5.CTPO-PROT-AGNDA
                AND   NSMULA_OPER_DESC = :DCOMB0P5.NSMULA-OPER-DESC
                AND   DATE(HINCL_REG)  = CURRENT_DATE
                ORDER BY DATE(HINCL_REG) DESC
                FETCH FIRST 1 ROW ONLY
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100)
           OR (SQLWARN0                EQUAL 'W')
               MOVE SQLCODE            TO HX-SQLCODE-09
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE '0020'             TO ERR-LOCAL
               MOVE 'TPROT_SOLTC_AGNDA'
                                       TO ERR-DBD-TAB
               MOVE SQLCODE            TO ERR-SQL-CODE
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
              EVALUATE CSIT-PROT-AGNDA OF DCOMB0P5
BRQ001*---- OPERACAO EM PROCESSAMENTO / PENDENTE
                WHEN 1
                WHEN 2
BRQ002             MOVE 1176           TO WRK-SAI-COD-RETORNO
                   MOVE WRK-MSG098     TO WRK-SAI-MSG-RETORNO
                   MOVE WRK-MSGF       TO WRK-FUNCAO
                   PERFORM 4000-ENVIAR-MENSAGEM
BRQ001*---- OPERACAO PROCESSADA
.               WHEN 3
BRQ002             MOVE 1184           TO WRK-SAI-COD-RETORNO
.                  MOVE WRK-MSG1184    TO WRK-SAI-MSG-RETORNO
.                  MOVE WRK-MSGF       TO WRK-FUNCAO
BRQ001             PERFORM 4000-ENVIAR-MENSAGEM
BRQ001*---- OPERACAO REJEITADA - SALDO INDISPONIVEL
                WHEN 4
CAP001*           -MOVE 08             TO WRK-SAI-COD-RETORNO
CAP001             MOVE 09             TO WRK-SAI-COD-RETORNO
                   MOVE WRK-MSG099     TO WRK-SAI-MSG-RETORNO
                   MOVE WRK-MSGF       TO WRK-FUNCAO
                   PERFORM 4000-ENVIAR-MENSAGEM
BRQ001*---- OPERACAO REJEITADA (SIT >= 5)
.               WHEN OTHER
.                  MOVE 08             TO WRK-SAI-COD-RETORNO
.                  MOVE WRK-MSG100     TO WRK-SAI-MSG-RETORNO
.                  MOVE WRK-MSGF       TO WRK-FUNCAO
BRQ001             PERFORM 4000-ENVIAR-MENSAGEM
              END-EVALUATE
           END-IF

           .
      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       3200-OBTER-DADOS-ADICIONAIS     SECTION.
      *----------------------------------------------------------------*
      *
      ******ERFORM 3210-ACESSAR-DCOM5022
CAP306     PERFORM 3210-ACESSAR-DCOMB0E5
      *
           PERFORM 3220-OBTER-CPF
      *
           PERFORM 3230-OBTER-DATA-VALOR
      *
BRQ003     PERFORM 3238-OBTER-CARTEIRA-DCOM
      *
CAP005     PERFORM 3236-VERIFICAR-LIMITE
      *
           PERFORM 3240-OBTER-LISTA-PRODUTO
           .
      *
      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
CAP306*           ACESSA TABELA DCOMB0E5 PARA OBTER NSEQ
      *----------------------------------------------------------------*
       3210-ACESSAR-DCOMB0E5           SECTION.
      *----------------------------------------------------------------*

      *
           MOVE WRK-ENT-NSMULA-OPER    TO NSMULA-OPER-DESC OF DCOMB0E5.

           EXEC SQL
                SELECT
                      CCHAVE_ELMTO_DESC
                INTO  :DCOMB0E5.CCHAVE-ELMTO-DESC
                FROM DB2PRD.TSMULA_OPER_DESC
                WHERE NSMULA_OPER_DESC = :DCOMB0E5.NSMULA-OPER-DESC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100)
           OR (SQLWARN0                EQUAL 'W')
               MOVE SQLCODE            TO HX-SQLCODE-09
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE '0030'             TO ERR-LOCAL
               MOVE 'TSMULA_OPER_DESC' TO ERR-DBD-TAB
               MOVE SQLCODE            TO ERR-SQL-CODE
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF (SQLCODE                 EQUAL +100)
           OR (SQLWARN0                EQUAL 'W')
               MOVE SQLCODE            TO HX-SQLCODE-09
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE '0040'             TO ERR-LOCAL
               MOVE 'TSMULA_OPER_DESC' TO ERR-DBD-TAB
               MOVE SQLCODE            TO ERR-SQL-CODE
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE CCHAVE-ELMTO-DESC OF DCOMB0E5
                                       TO WRK-CCHAVE-B0E5.
           MOVE WRK-NUM-OPERACAO       TO WRK-NSEQ-OPER-15.
CAP306
      *----------------------------------------------------------------*
       3210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *     OBTER DADOS DO CLIENTE NO SISTEMA CADU                     *
      *----------------------------------------------------------------*
       3220-OBTER-CPF                  SECTION.
      *----------------------------------------------------------------*
      *
           INITIALIZE ROTENT-6408-AREA
                      ROTSAI-6408-RETORNO
                      ERRO-AREA
                      WRK-SQLCA.
      *
           MOVE WRK-ENT-CAG-BCRIA      TO ROTENT-6408-CJUNC-ENV
           MOVE WRK-ENT-CCTA-BCRIA-CLI TO ROTENT-6408-CCTAC-ENV
      *
           MOVE 'DCOM6408'             TO WRK-MODULO.
           CALL WRK-MODULO             USING ROTENT-6408-AREA
                                             ROTSAI-6408-RETORNO
                                             ERRO-AREA
                                             WRK-SQLCA.
      *
           IF ROTSAI-6408-COD-RET      NOT EQUAL '0000'

BRQ002*--- DEVOLUCAO DO CONE NA 2A CHAMADA C/ ERRO
.              IF WRK-LIMITE           EQUAL 'N' AND
.                 WRK-ENT-PROTOCOLO    NOT EQUAL SPACES
.                 MOVE 'D'             TO WRK-FUNC-CONE
.                 PERFORM 3320-SENSIBILIZA-CONE
.              END-IF
BRQ002
              MOVE ROTSAI-6408-COD-RET TO WRK-COD-RETORNO
              MOVE WRK-COD-RET         TO WRK-SAI-COD-RETORNO
              MOVE ROTSAI-6408-MENS-RET
                                       TO WRK-SAI-MSG-RETORNO
              MOVE WRK-MSGF            TO WRK-FUNCAO
              PERFORM 4000-ENVIAR-MENSAGEM
           END-IF
      *
4S2511     MOVE ROTSAI-6408-CNPJ-CADU(1)
4S2511                                 TO WRK-CPF-X
4S2511     IF WRK-CPF-X            EQUAL SPACES OR
4S2511        WRK-CPF-X            EQUAL LOW-VALUES
4S2511         MOVE SPACES         TO WRK-SAI-CPF
4S2511         MOVE SPACES         TO WRK-SAI-CFLIAL-CPF
4S2511         MOVE ZEROS          TO WRK-SAI-CCTRL-CPF
4S2511     ELSE
4S2511         MOVE WRK-CPF        TO WRK-SAI-CPF
4S2511         MOVE WRK-CFLIAL-CPF TO WRK-SAI-CFLIAL-CPF
4S2511         MOVE WRK-CCTRL-CPF  TO WRK-SAI-CCTRL-CPF
4S2511     END-IF
           .
      *
      *----------------------------------------------------------------*
       3220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       3230-OBTER-DATA-VALOR           SECTION.
      *----------------------------------------------------------------*
      *
           INITIALIZE                  E5-ARGUMENTOS-ENTRADA
                                       E5-RETORNO
                                       ERRO-AREA
                                       WRK-SQLCA.

           MOVE WRK-ENT-NSMULA-OPER    TO E5-NSMULA-OPER-DESC.
           MOVE 'TOTAL'                TO E5-INSTRUCAO.

           MOVE 'DCOM7225'             TO WRK-MODULO.
           CALL WRK-MODULO             USING E5-ARGUMENTOS-ENTRADA
                                             E5-RETORNO
                                             ERRO-AREA
                                             WRK-SQLCA.

           IF E5-COD-RETORNO           NOT EQUAL ZEROS
              MOVE E5-COD-RETORNO      TO WRK-SAI-COD-RETORNO
              MOVE E5-MENSAGEM         TO WRK-SAI-MSG-RETORNO
              MOVE WRK-MSGF           TO WRK-FUNCAO
              PERFORM 4000-ENVIAR-MENSAGEM
           ELSE
CAP004        PERFORM 3235-OBTER-CONSULTA-CONE
           END-IF
           .
      *
      *----------------------------------------------------------------*
       3230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
CAP004*----------------------------------------------------------------*
.     *     OBTER A PARTIR DA PARAMETRIZACAO DE CARTEIRAS A CONSULTA   *
.     *     DE CREDITO PARA A OPERACAO. A ISENCAO DE CONE EH VERIFICADA*
.     *     NO PROPRIO MODULO DCOM5872.                                *
.     *----------------------------------------------------------------*
.      3235-OBTER-CONSULTA-CONE      SECTION.
.     *----------------------------------------------------------------*
.
.          INITIALIZE WRK-5872-ENTRADA
.                     WRK-5872-SAIDA
.                     ERRO-AREA
.                     WRK-SQLCA
.
.          MOVE 1                        TO WRK-5872E-CFUNC-BDSCO
.          MOVE WRK-ENT-CANAL            TO WRK-5872E-CTERM
.          MOVE E5RT-CPRODT(1)           TO HX-NUM-03-CS
.          MOVE HX-NUM-03-SS             TO WRK-5872E-CPRODT
.          MOVE E5RT-CSPROD-DESC-COML(1) TO HX-NUM-03-CS
.          MOVE HX-NUM-03-SS             TO WRK-5872E-CSPROD-DESC
.          MOVE 005                      TO WRK-5872E-CMEIO-ENTRD
.          MOVE WRK-ENT-CAG-BCRIA        TO WRK-5872E-CAG-BCRIA
.     *
.          MOVE WRK-CPF                TO WRK-5872E-CCNPJ-CPF
.     *
.          MOVE WRK-CFLIAL-CPF         TO WRK-5872E-CFLIAL-CNPJ
.     *
.          MOVE WRK-CCTRL-CPF          TO WRK-5872E-CCTRL-CNPJ-CPF
.          MOVE E5RT-VOPER-DESC-COML(1)
.                                      TO HX-NUM-15V02-CS
.          MOVE HX-NUM-15V02-SS        TO WRK-5872E-VOPER-DESC-COML
.          MOVE E5RT-DINIC-OPER-DESC(1)
.                                      TO WRK-5872E-DINIC-OPER-DESC
.          MOVE 'DCOM5872'             TO WRK-MODULO
.
.          CALL WRK-MODULO             USING WRK-5872-ENTRADA
.                                            WRK-5872-SAIDA
.                                            ERRO-AREA
.                                            WRK-SQLCA
.     *
.          IF  WRK-5872S-COD-RETORNO   NOT EQUAL ZEROS
.              MOVE 1182               TO WRK-SAI-COD-RETORNO
.              MOVE 'VALOR DA OPERACAO SUPERIOR AO VALOR DO LIMITE DO CL
      -             'IENTE'
.                                      TO WRK-SAI-MSG-RETORNO
.              MOVE WRK-MSGF           TO WRK-FUNCAO
.              PERFORM 4000-ENVIAR-MENSAGEM
WIP001     ELSE
WIP001         PERFORM 3237-CONSULTA-SALDO-CONE
.          END-IF.
.
.     *----------------------------------------------------------------*
.      3235-99-FIM.                    EXIT.
CAP004*----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *     VERIFICAR SE CLIENTE POSSUI LIMITE DIARIO DISPONIVEL       *
      *     PARA REALIZAR OPERACAO :                                   *
      *     DCOMB0N2 - LIMITE CLIENTE                                  *
      *     DCOMB0N3 - LIMITE PRODUTO                                  *
      *     DCOMB0N4 - OPERACOES NET EMPRESA (PENDENTE / ATIVAS)       *
CAP005*----------------------------------------------------------------*
.      3236-VERIFICAR-LIMITE          SECTION.
.     *----------------------------------------------------------------*
.
.          INITIALIZE WRK-6439-ENTRADA
.                     WRK-6439-SAIDA
.
.          MOVE 'V'                      TO WRK-E6439-TP-PESQUISA
.          MOVE E5RT-CPRODT(1)           TO HX-NUM-03-CS
.          MOVE HX-NUM-03-SS             TO WRK-E6439-CPRODT
.          MOVE E5RT-CSPROD-DESC-COML(1) TO HX-NUM-03-CS
.          MOVE HX-NUM-03-SS             TO WRK-E6439-CSPROD-DESC-COML
.          MOVE E5RT-CTPO-DESC-COML(1)   TO HX-NUM-03-CS
.          MOVE HX-NUM-03-SS             TO WRK-E6439-CTPO-DESC-COML
.    ***-- SEMPRE PESQUISAR LIMITE COM PARAMETROS DO NET EMPRESA
.          MOVE 3                        TO WRK-E6439-CORIGE-OPER-DESC
.          MOVE WRK-CPF                  TO WRK-E6439-CCNPJ-CPF
.          MOVE WRK-CFLIAL-CPF           TO WRK-E6439-CFLIAL-CNPJ
.          MOVE WRK-CCTRL-CPF            TO WRK-E6439-CCTRL-CNPJ-CPF
.          MOVE WRK-ENT-BANCO            TO WRK-E6439-CBCO
.          MOVE WRK-ENT-CAG-BCRIA        TO WRK-E6439-CAG-BCRIA
.          MOVE WRK-ENT-CCTA-BCRIA-CLI   TO WRK-E6439-CCTA-BCRIA-CLI
.          MOVE E5RT-VOPER-DESC-COML(1)  TO HX-NUM-15V02-CS
.          MOVE HX-NUM-15V02-SS          TO WRK-E6439-VLR-OPERACAO
.
.          MOVE 'DCOM6439'               TO WRK-MODULO
.
.          CALL WRK-MODULO               USING WRK-6439-ENTRADA
.                                              WRK-6439-SAIDA
.                                              ERRO-AREA
.                                              WRK-SQLCA
.
.          IF  WRK-S6439-COD-RETORNO   NOT EQUAL ZEROS AND 03
.              MOVE WRK-S6439-COD-RETORNO TO WRK-SAI-COD-RETORNO
.              MOVE WRK-S6439-MSG-RETORNO TO WRK-SAI-MSG-RETORNO
               MOVE WRK-MSGF           TO WRK-FUNCAO
.              PERFORM 4000-ENVIAR-MENSAGEM
.          END-IF
.
.          IF  WRK-S6439-COD-RETORNO   EQUAL 0003
.              MOVE SPACES             TO WRK-S6439-TP-LIMITE
.          END-IF
.     *
.          IF  WRK-S6439-COD-RETORNO   EQUAL ZEROS
.              IF WRK-S6439-IND-TEM-LIMITE
.                                      NOT EQUAL 'S'
.              OR E5RT-VOPER-DESC-COML(1)
.                                      GREATER WRK-S6439-VLR-DISPONIVEL
TESTE             MOVE 'S'             TO WRK-LIMITE
TESTE             PERFORM 3500-ACESSAR-DCOM3934
TESTE             IF WRK-3934S-COD-RETORNO
TESTE                                  EQUAL ZEROS
TESTE *             -MOVE WRK-3934S-PEND-GERENTE
TESTE *                               -TO WRK-SAI-PEND-GERENTE
TESTE                MOVE 1183         TO WRK-SAI-COD-RETORNO
TESTE                MOVE WRK-MSG1183  TO WRK-SAI-MSG-RETORNO
TESTE                MOVE WRK-MSGF     TO WRK-FUNCAO
TESTE             END-IF
.                 PERFORM 4000-ENVIAR-MENSAGEM
.              END-IF
.          END-IF.
.
.     *----------------------------------------------------------------*
CAP005 3236-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
WIP001*----------------------------------------------------------------*
WIP001 3237-CONSULTA-SALDO-CONE            SECTION.
WIP001*----------------------------------------------------------------*
WIP001
WIP002     PERFORM 3238-OBTER-CARTEIRA-DCOM.
WIP001
WIP001     INITIALIZE LCQ-ENVIA
WIP001                LCQ-RETORNA
WIP001                LCQ-ERRO-AREA
WIP001                LCQ-SQLCA
WIP001
WIP001     MOVE 'DCOM3933'                 TO LCQ-COD-PGM
WIP001     MOVE 'V'                        TO LCQ-FUNCAO
WIP001
WIP001     MOVE WRK-ENT-CAG-BCRIA          TO LCQ-AGEN-CONTR
WIP001     MOVE WRK-ENT-CCTA-BCRIA-CLI     TO LCQ-CCTA-CONTR
WIP001     MOVE WRK-CPF                    TO LCQ-CGCCPF-CONTR
WIP001     MOVE WRK-CFLIAL-CPF             TO LCQ-CFLIAL-CONTR
WIP001     MOVE WRK-CCTRL-CPF              TO LCQ-CCTRL-CONTR
WIP001
WIP001     MOVE WRK-5872S-DANO-BASE        TO LCQ-ANO-CONS
WIP001     MOVE WRK-5872S-CDEPDC           TO LCQ-AGEN-CONS
WIP001     MOVE WRK-5872S-CSEQ-STUDO       TO LCQ-NUM-CONS
WIP002     MOVE 64RT-CCART-NORML-DESC(1)(1:3)
WIP002                                     TO LCQ-CCART
WIP001     MOVE WRK-5872S-CGARNT           TO LCQ-CGARNT
WIP001
WIP001     MOVE E5RT-VOPER-DESC-COML(1)    TO HX-NUM-15V02-CS
WIP001     MOVE HX-NUM-15V02-SS            TO LCQ-VCONTR
WIP001
WIP001     MOVE 'CONE0451'                 TO WRK-MODULO
WIP001
WIP001     CALL WRK-MODULO                 USING LCQ-AREA
WIP001                                           LNK-IO-PCB
WIP001                                           LNK-ALT-PCB
WIP001
WIP001     IF  LCQ-STATUS-CONSULTA         EQUAL 03
WIP001         NEXT SENTENCE
WIP001     ELSE
WIP001         MOVE 1185                   TO WRK-SAI-COD-RETORNO
WIP001         MOVE WRK-MSG1185            TO WRK-SAI-MSG-RETORNO
WIP001         MOVE WRK-MSGF               TO WRK-FUNCAO
WIP001         PERFORM 4000-ENVIAR-MENSAGEM
WIP001     END-IF
WIP001     .
WIP001*
WIP001*----------------------------------------------------------------*
WIP001 3237-99-FIM.                        EXIT.
WIP001*----------------------------------------------------------------*

WIP002*----------------------------------------------------------------*
WIP002*     OBTER CARTEIRA PARAMETRIZADA NO DCOM                       *
WIP002*----------------------------------------------------------------*
WIP002 3238-OBTER-CARTEIRA-DCOM       SECTION.
WIP002*----------------------------------------------------------------*
WIP002
WIP002     INITIALIZE 64-ARGUMENTOS-ENTRADA
WIP002                64-RETORNO
WIP002                ERRO-AREA
WIP002                WRK-SQLCA.
WIP002
WIP002     MOVE E5RT-CPRODT(1)           TO 64-CPRODT.
WIP002     MOVE E5RT-CSPROD-DESC-COML(1) TO 64-CSPROD-DESC-COML.
WIP002     MOVE E5RT-CMEIO-ENTRD-DESC(1) TO 64-CMEIO-ENTRD-DESC.
WIP002
WIP002     STRING E5RT-DINIC-OPER-DESC(1)(07:04) '-'
WIP002            E5RT-DINIC-OPER-DESC(1)(04:02) '-'
WIP002            E5RT-DINIC-OPER-DESC(1)(01:02) '-00.00.00.000000'
WIP002     DELIMITED BY SIZE           INTO 64-HINIC-MEIO-ENTRD.
WIP002
WIP002     MOVE 'PARCIAL-07'           TO 64-INSTRUCAO.
WIP002     MOVE 'DCOM6820'             TO WRK-MODULO
WIP002
WIP002     CALL WRK-MODULO             USING 64-ARGUMENTOS-ENTRADA
WIP002                                       64-RETORNO
WIP002                                       ERRO-AREA
WIP002                                       WRK-SQLCA.
WIP002
WIP002     IF  64-COD-RETORNO          NOT EQUAL ZEROS
WIP002         MOVE 64-COD-RETORNO     TO WRK-SAI-COD-RETORNO
WIP002         MOVE 64-MENSAGEM        TO WRK-SAI-MSG-RETORNO
WIP002         MOVE WRK-MSGF           TO WRK-FUNCAO
WIP002         PERFORM 4000-ENVIAR-MENSAGEM
WIP002     END-IF.
WIP002
WIP002*----------------------------------------------------------------*
WIP002 3238-99-FIM.                    EXIT.
WIP002*----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3240-OBTER-LISTA-PRODUTO        SECTION.
      *----------------------------------------------------------------*
      *
           INITIALIZE                  BVVEND-COMMAREA

F2404      PERFORM 3450-ACESSAR-BVVEA520

F2404      IF  BVVEFX-S-IND-CLI-MASSV  EQUAL 'S'
F2404          MOVE 'S'                TO WRK-FIM
F2404      ELSE
               PERFORM 3250-ACESSAR-DCOM7236
               PERFORM VARYING IND-1   FROM 1 BY 1
CAP002                   UNTIL IND-1   GREATER 40
                            OR IND-1   GREATER E7-QTDE-RETORNADA

                   MOVE E7RT-DVCTO-TITLO-DESC(IND-1)
                                       TO BVVEND-E-DATA(IND-1)
                   MOVE E7RT-CADM-CATAO-CREDT(IND-1)
                                       TO HX-NUM-09-CS
                   MOVE HX-NUM-09-SS   TO HX-NRO-18
                   MOVE HX-NRO-05      TO BVVEND-E-PRODUTO(IND-1)
                   MOVE E7RT-VTITLO-DESC-COML(IND-1)
                                       TO HX-NUM-15V02-CS
                   MOVE HX-NUM-15V02-SS
                                       TO BVVEND-E-VLR-DESCONTO(IND-1)
CAP002             MOVE E7RT-NTITLO-DESC-COML(IND-1)
|                                      TO HX-NUM-05-CS
CAP002             MOVE HX-NUM-05-SS   TO WRK-NTITLO-DESC-COML
               END-PERFORM
F2404      END-IF.
      *
           PERFORM 3260-ACESSA-BVVEA504

CAP002     IF WRK-FIM                  EQUAL 'N'
|              GO TO 3240-OBTER-LISTA-PRODUTO
|          ELSE
|              PERFORM 3270-CALCULA-TEMPO
|              PERFORM 3280-INSERIR-DADOS
CAP002     END-IF
           .
      *
      *----------------------------------------------------------------*
       3240-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *      OBTER LISTA DE PRODUTOS ANTECIPADOS (BASE SIMULACAO)      *
      *----------------------------------------------------------------*
       3250-ACESSAR-DCOM7236          SECTION.
      *----------------------------------------------------------------*
      *
           INITIALIZE                  E7-ARGUMENTOS-ENTRADA
                                       E7-RETORNO
                                       ERRO-AREA
                                       WRK-SQLCA.

           MOVE WRK-ENT-NSMULA-OPER    TO E7-NSMULA-OPER-DESC.
           MOVE WRK-NTITLO-DESC-COML   TO E7-PROXIMO-RESTART.
           MOVE 'PARCIAL-06'           TO E7-INSTRUCAO.

           MOVE 'DCOM7236'             TO WRK-MODULO
CAP002     MOVE WRK-DESPREZAR          TO E7-QTDE-A-DESPREZAR
           CALL WRK-MODULO             USING E7-ARGUMENTOS-ENTRADA
                                             E7-RETORNO
                                             ERRO-AREA
                                             WRK-SQLCA.

           IF E7-COD-RETORNO           NOT EQUAL ZEROS AND 2
              MOVE E7-COD-RETORNO      TO WRK-SAI-COD-RETORNO
              MOVE E7-MENSAGEM         TO WRK-SAI-MSG-RETORNO
              MOVE WRK-MSGF            TO WRK-FUNCAO
              PERFORM 4000-ENVIAR-MENSAGEM
           END-IF
      *
CAP002     IF E7-COD-RETORNO        EQUAL  2
|          OR E7-QTDE-RETORNADA     GREATER 40
|             ADD 40                TO WRK-DESPREZAR
|             MOVE 'N'              TO WRK-FIM
|          ELSE
|             MOVE ZEROS            TO WRK-DESPREZAR
|             MOVE 'S'              TO WRK-FIM
CAP002     END-IF
           .
      *
      *----------------------------------------------------------------*
       3250-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *                   GERA PROTOCOLO CIP                           *
      *----------------------------------------------------------------*
       3260-ACESSA-BVVEA504           SECTION.
      *----------------------------------------------------------------*
      *
           MOVE WRK-ANO-TMSTAMP        TO WRK-DANO
                                          WRK-DANO-OPER
           MOVE WRK-NSEQ-OPER-09       TO WRK-NSEQ
                                          WRK-NSEQ-OPER
           MOVE WRK-OPERACAO           TO WRK-SAI-OPERACAO

           MOVE WRK-PROTOCOLO          TO BVVEND-E-PROTOCOLO

           MOVE WRK-ENT-CAG-BCRIA      TO BVVEND-E-AGENCIA
           MOVE WRK-ENT-CCTA-BCRIA-CLI
                                       TO BVVEND-E-CONTA
           MOVE WRK-ENT-CANAL(1:4)     TO BVVEND-E-CANAL
           MOVE 'DCOM'                 TO WRK-SIGLA-CCUSTO
           MOVE WRK-CONTRATO-BVVE      TO BVVEND-E-CONTRATO
           MOVE 'I'                    TO BVVEND-E-IND-I-A
           MOVE E5RT-DVCTO-FNAL-APURA(1)
                                       TO BVVEND-E-DT-CONT
           MOVE E5RT-VOPER-DESC-COML(1)
                                       TO BVVEND-E-VLR-TOT

F2404 * ---------------------------------------------------------------*
F2404 * CHAMADA PARA CLIENTES MASSIVOS SERA UNICA E SEM PREENCHIMENTO
F2404 * DAS OCORRENCIAS - IND-CLI-MASSV 'C' = CANAL
F2404
F2404      IF  BVVEFX-S-IND-CLI-MASSV  EQUAL 'S'
F2404          MOVE 'S'                TO BVVEND-E-FIM-OCOR
F2404          MOVE 'C'                TO BVVEND-E-IND-ORIG-MASSV
F2404          MOVE WRK-ENT-NSMULA-OPER
F2404                                  TO BVVEND-E-NSMULA-OPER
F2404      ELSE
CAP002         IF WRK-DESPREZAR        EQUAL ZEROS
|                 MOVE 'S'             TO BVVEND-E-FIM-OCOR
|              ELSE
|                 MOVE 'N'             TO BVVEND-E-FIM-OCOR
CAP002         END-IF
F2404      END-IF.
      *
           MOVE 'BVVEA504'             TO WRK-MODULO
      *
           CALL WRK-MODULO             USING BVVEND-COMMAREA
      *
           EVALUATE  BVVEND-S-COD-RETORNO
              WHEN 00
      *--> PROCESSAMENTO OK
                IF BVVEND-S-PROTOCOLO  NOT EQUAL SPACES
                   MOVE BVVEND-S-PROTOCOLO
                                       TO WRK-SAI-PROTOCOLO
                                          WRK-PROTOCOLO
                   MOVE BVVEND-S-TMP-ESPERA
                                       TO WRK-TEMPO-BVVE
                   MOVE ZEROS          TO WRK-SAI-COD-RETORNO
                   MOVE WRK-MSGF       TO WRK-FUNCAO
                   MOVE WRK-MSG090     TO WRK-SAI-MSG-RETORNO
                ELSE
CAP003*--> ERRO DE CONSISTENCIA
.                  MOVE 08             TO WRK-SAI-COD-RETORNO
.                  MOVE 'PROTOCOLO RETORNADO VAZIO DO BVVEA504'
                                       TO WRK-SAI-MSG-RETORNO
.                  MOVE WRK-MSGF       TO WRK-FUNCAO
.                  PERFORM 4000-ENVIAR-MENSAGEM
CAP003          END-IF
              WHEN 08
      *--> ERRO DE CONSISTENCIA
                   MOVE 08             TO WRK-SAI-COD-RETORNO
                   MOVE WRK-MSG091     TO WRK-SAI-MSG-RETORNO
                   MOVE WRK-MSGF       TO WRK-FUNCAO
                   PERFORM 4000-ENVIAR-MENSAGEM
              WHEN 10
      *--> ERRO DE GRADE FECHADA
                   MOVE 10             TO WRK-SAI-COD-RETORNO
                   MOVE WRK-MSG096     TO WRK-SAI-MSG-RETORNO
                   MOVE WRK-MSGF       TO WRK-FUNCAO
                   PERFORM 4000-ENVIAR-MENSAGEM
              WHEN 12
      *--> ERRO DE COMUNICACAO COM A CIP
                   MOVE 12             TO WRK-SAI-COD-RETORNO
                   MOVE WRK-MSG097     TO WRK-SAI-MSG-RETORNO
                   MOVE WRK-MSGF       TO WRK-FUNCAO
                   PERFORM 4000-ENVIAR-MENSAGEM
              WHEN 16
      *--> ERRO DB2
                   MOVE 16             TO WRK-SAI-COD-RETORNO
                   MOVE WRK-MSG095     TO WRK-SAI-MSG-RETORNO
                   MOVE WRK-MSGF       TO WRK-FUNCAO
                   PERFORM 4000-ENVIAR-MENSAGEM
           END-EVALUATE
           .
      *----------------------------------------------------------------*
       3260-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *                   CALCULA TEMPO DE ESPERA                      *
      *----------------------------------------------------------------*
       3270-CALCULA-TEMPO              SECTION.
      *----------------------------------------------------------------*


           MOVE WRK-SEGUNDOS           TO WRK-SEGUNDOS-COMP
           MOVE WRK-MINUTO             TO WRK-MINUTO-COMP

           IF WRK-HORA                 GREATER ZEROS
           OR WRK-MINUTO               GREATER 16

            MOVE 999                   TO WRK-TEMPO-ESPERA
           ELSE
                COMPUTE WRK-TEMPO-ESPERA = WRK-MINUTO-COMP * 60
                IF  WRK-SEGUNDOS       GREATER 40
                AND WRK-MINUTO         EQUAL 16
                    MOVE 999           TO WRK-TEMPO-ESPERA
                ELSE
                    ADD WRK-SEGUNDOS-COMP
                                       TO WRK-TEMPO-ESPERA
                END-IF
           END-IF

           MOVE BVVEND-S-TMP-ESPERA    TO WRK-SAI-TEMPO-ESPERA
           .

      *----------------------------------------------------------------*
       3270-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       3280-INSERIR-DADOS              SECTION.
      *----------------------------------------------------------------*

CAP006     PERFORM 3285-VALIDAR-PROT-EXISTENTE
           PERFORM 3290-INSERIR-SOLTC-AGNDA
           PERFORM 3300-INSERIR-SOLTC-CANAL
CAP007     PERFORM 3310-ATUALIZAR-SIMULACAO
BRQ002*
.     *--- SENSIBILIZA CONE EM TEMPO DE SIMULACAO
.     *
.          MOVE 'S'                    TO WRK-FUNC-CONE
BRQ002     PERFORM 3320-SENSIBILIZA-CONE
           .

      *----------------------------------------------------------------*
       3280-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      * VALIDAR SE PROTOCOLO DE EFETIVACAO JA EXISTE NAS BASES DE PROT *
      *----------------------------------------------------------------*
CAP006 3285-VALIDAR-PROT-EXISTENTE     SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  DCOMB0P5
      *                                DCOMB0P6

           MOVE WRK-DANO               TO DANO-OPER-DESC
                                       OF DCOMB0P5
      *                                   DANO-OPER-DESC
      *                                OF DCOMB0P6
      *
           MOVE WRK-NSEQ               TO NSEQ-OPER-DESC
                                       OF DCOMB0P5
      *
           MOVE 'E'                    TO CTPO-PROT-AGNDA
                                       OF DCOMB0P5
      *
           EXEC SQL
                SELECT CPROT_SOLTC_AGNDA
                      ,CTPO_PROT_AGNDA
                INTO  :DCOMB0P5.CPROT-SOLTC-AGNDA
                     ,:DCOMB0P5.CTPO-PROT-AGNDA
                FROM DB2PRD.TPROT_SOLTC_AGNDA
                WHERE DANO_OPER_DESC    = :DCOMB0P5.DANO-OPER-DESC
                AND   NSEQ_OPER_DESC    = :DCOMB0P5.NSEQ-OPER-DESC
                FETCH FIRST 1 ROW ONLY
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100)
           OR (SQLWARN0                EQUAL 'W')
               MOVE SQLCODE            TO HX-SQLCODE-09
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE '0050'             TO ERR-LOCAL
               MOVE 'TPROT_SOLTC_AGNDA'
                                       TO ERR-DBD-TAB
               MOVE SQLCODE            TO ERR-SQL-CODE
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               MOVE 1184               TO WRK-SAI-COD-RETORNO
               MOVE WRK-MSG1184        TO WRK-SAI-MSG-RETORNO
               MOVE WRK-MSGF           TO WRK-FUNCAO
               PERFORM 4000-ENVIAR-MENSAGEM
           END-IF
           .
      *----------------------------------------------------------------*
       3285-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *        INSERE DADOS NA TAELA DE SOLICITACAO DE AGENDA
      *----------------------------------------------------------------*
       3290-INSERIR-SOLTC-AGNDA        SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  DCOMB0P5
                                       DCOMB0P5

           MOVE 237                    TO CBCO
                                       OF DCOMB0P5
      *
           MOVE WRK-ENT-CAG-BCRIA      TO CAG-BCRIA
                                       OF DCOMB0P5
      *
           MOVE WRK-ENT-CCTA-BCRIA-CLI
                                       TO CCTA-BCRIA-CLI
                                       OF DCOMB0P5
      *
           MOVE WRK-PROTOCOLO          TO CPROT-SOLTC-AGNDA
                                       OF DCOMB0P5
      *
           MOVE 'E'                    TO CTPO-PROT-AGNDA
                                       OF DCOMB0P5
      *
           MOVE ZEROS                  TO QTEMPO-VALDD-PROCM
                                       OF DCOMB0P5
      *
           MOVE 1                      TO CSIT-PROT-AGNDA
                                       OF DCOMB0P5
      *
           MOVE WRK-ENT-NSMULA-OPER    TO NSMULA-OPER-DESC
                                       OF DCOMB0P5
      *
           MOVE WRK-DANO               TO DANO-OPER-DESC
                                       OF DCOMB0P5
      *
           MOVE WRK-NSEQ               TO NSEQ-OPER-DESC
                                       OF DCOMB0P5

           EXEC SQL
               INSERT INTO DB2PRD.TPROT_SOLTC_AGNDA
                     (CBCO
                     ,CAG_BCRIA
                     ,CCTA_BCRIA_CLI
                     ,CPROT_SOLTC_AGNDA
                     ,CTPO_PROT_AGNDA
                     ,QTEMPO_VALDD_PROCM
                     ,CSIT_PROT_AGNDA
                     ,HINCL_REG
                     ,NSMULA_OPER_DESC
                     ,DANO_OPER_DESC
                     ,NSEQ_OPER_DESC)
                VALUES
                     (:DCOMB0P5.CBCO
                     ,:DCOMB0P5.CAG-BCRIA
                     ,:DCOMB0P5.CCTA-BCRIA-CLI
                     ,:DCOMB0P5.CPROT-SOLTC-AGNDA
                     ,:DCOMB0P5.CTPO-PROT-AGNDA
                     ,:DCOMB0P5.QTEMPO-VALDD-PROCM
                     ,:DCOMB0P5.CSIT-PROT-AGNDA
                     , CURRENT_TIMESTAMP
                     ,:DCOMB0P5.NSMULA-OPER-DESC
                     ,:DCOMB0P5.DANO-OPER-DESC
                     ,:DCOMB0P5.NSEQ-OPER-DESC)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND -803)
           OR (SQLWARN0                EQUAL 'W')
               MOVE SQLCODE            TO HX-SQLCODE-09
               MOVE 'INSERT'           TO ERR-FUN-COMANDO
               MOVE '0060'             TO ERR-LOCAL
               MOVE 'TPROT_SOLTC_AGNDA'
                                       TO ERR-DBD-TAB
               MOVE SQLCODE            TO ERR-SQL-CODE
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       3290-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *        INSERE DADOS NA TAELA DE SOLICITACAO DE CANAL
      *----------------------------------------------------------------*
       3300-INSERIR-SOLTC-CANAL        SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  DCOMB0P6

           MOVE 237                    TO CBCO
                                       OF DCOMB0P6
      *
           MOVE WRK-ENT-CAG-BCRIA      TO CAG-BCRIA
                                       OF DCOMB0P6
      *
           MOVE WRK-ENT-CCTA-BCRIA-CLI
                                       TO CCTA-BCRIA-CLI
                                       OF DCOMB0P6
      *
           MOVE WRK-PROTOCOLO          TO CPROT-SOLTC-AGNDA
                                       OF DCOMB0P6
      *
           MOVE 'E'                    TO CTPO-PROT-AGNDA
                                       OF DCOMB0P6
      *
           IF WRK-ENT-CANAL            EQUAL 'NETEMPR'
              MOVE 35                  TO CTPO-CANAL
                                       OF DCOMB0P6
           ELSE
              MOVE 66                  TO CTPO-CANAL
                                       OF DCOMB0P6
           END-IF
      *
           MOVE WRK-TEMPO-ESPERA       TO QTEMPO-PROCM-CANAL
                                       OF DCOMB0P6
      *

           EXEC SQL
               INSERT INTO DB2PRD.TPROT_SOLTC_CANAL
                     (CBCO
                     ,CAG_BCRIA
                     ,CCTA_BCRIA_CLI
                     ,CPROT_SOLTC_AGNDA
                     ,CTPO_PROT_AGNDA
                     ,CTPO_CANAL
                     ,QTEMPO_PROCM_CANAL)
                VALUES
                     (:DCOMB0P6.CBCO
                     ,:DCOMB0P6.CAG-BCRIA
                     ,:DCOMB0P6.CCTA-BCRIA-CLI
                     ,:DCOMB0P6.CPROT-SOLTC-AGNDA
                     ,:DCOMB0P6.CTPO-PROT-AGNDA
                     ,:DCOMB0P6.CTPO-CANAL
                     ,:DCOMB0P6.QTEMPO-PROCM-CANAL)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND -803)
           OR (SQLWARN0                EQUAL 'W')
               MOVE SQLCODE            TO HX-SQLCODE-09
               MOVE 'INSERT'           TO ERR-FUN-COMANDO
               MOVE '0070'             TO ERR-LOCAL
               MOVE 'TPROT_SOLTC_CANAL'
                                       TO ERR-DBD-TAB
               MOVE SQLCODE            TO ERR-SQL-CODE
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF
           .

      *----------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
CAP007*  ATUALIZAR A TABELA DE SIMULACAO COM O CONE ENQUADRADO
.     *----------------------------------------------------------------*
.      3310-ATUALIZAR-SIMULACAO        SECTION.
.     *----------------------------------------------------------------*
.     *
.          INITIALIZE DCOMB0E5
.     *
.          MOVE WRK-ENT-NSMULA-OPER    TO NSMULA-OPER-DESC OF DCOMB0E5.
.          MOVE WRK-5872S-DANO-BASE    TO DANO-BASE        OF DCOMB0E5.
.          MOVE WRK-5872S-CDEPDC       TO CDEPDC           OF DCOMB0E5.
.          MOVE WRK-5872S-CSEQ-STUDO   TO CSEQ-STUDO       OF DCOMB0E5.
.          MOVE WRK-5872S-CCART-LIM    TO CCART-LIM-CREDT  OF DCOMB0E5.
.
.          EXEC SQL
.               UPDATE DB2PRD.TSMULA_OPER_DESC
.                  SET DANO_BASE       = :DCOMB0E5.DANO-BASE
.                     ,CDEPDC          = :DCOMB0E5.CDEPDC
.                     ,CSEQ_STUDO      = :DCOMB0E5.CSEQ-STUDO
.                     ,CCART_LIM_CREDT = :DCOMB0E5.CCART-LIM-CREDT
.               WHERE NSMULA_OPER_DESC = :DCOMB0E5.NSMULA-OPER-DESC
.          END-EXEC.
.
.          IF (SQLCODE                 NOT EQUAL ZEROS)
.          OR (SQLWARN0                EQUAL 'W')
.              MOVE SQLCODE            TO HX-SQLCODE-09
.              MOVE 'UPDATE'           TO ERR-FUN-COMANDO
.              MOVE '0080'             TO ERR-LOCAL
.              MOVE 'TSMULA_OPER_DESC' TO ERR-DBD-TAB
.              MOVE SQLCODE            TO ERR-SQL-CODE
.              PERFORM 9999-PROCESSAR-ROTINA-ERRO
.          END-IF.
.
.     *----------------------------------------------------------------*
CAP007 3310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      * SENSIBILIZA OU DEVOLVE VALOR NA CONSULTA CONE ARMAZENADA
      * NA BASE DE SIMULACAO - DCOMB0E5
      *----------------------------------------------------------------*
BRQ002 3320-SENSIBILIZA-CONE           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  DCOMBA-ENTRADA.

           MOVE WRK-FUNC-CONE          TO DCOMBA-COD-FUNCAO
           MOVE WRK-ANO-ATUAL-B0E5     TO DCOMBA-DANO-OPER-DESC
           MOVE WRK-NUM-OPERACAO       TO DCOMBA-NSEQ-OPER-DESC
           MOVE WRK-ENT-NSMULA-OPER    TO DCOMBA-NUM-SIMULACAO
BRQ003     MOVE 64RT-CCART-NORML-DESC(1)(1:3)
                                       TO DCOMBA-CCART-LIM

      *--- CONE GERADO EM TEMPO DE EXECUCAO
           IF   WRK-FUNC-CONE          EQUAL 'S'
                MOVE WRK-5872S-DANO-BASE    TO DCOMBA-DANO-BASE
                MOVE WRK-5872S-CDEPDC       TO DCOMBA-CDEPDC
                MOVE WRK-5872S-CSEQ-STUDO   TO DCOMBA-CSEQ-STUDO
           ELSE
                MOVE ZEROS                  TO DCOMBA-DANO-BASE
                                               DCOMBA-CDEPDC
                                               DCOMBA-CSEQ-STUDO
           END-IF.

           MOVE 1                      TO DCOMBA-CFUNC-BDSCO
           MOVE WRK-ENT-CANAL          TO DCOMBA-CTERM

           MOVE 'DCOM5594'             TO WRK-MODULO
           CALL WRK-MODULO             USING DCOMBA-ENTRADA
                                             ERRO-AREA
                                             WRK-SQLCA.

           MOVE 'CONE'                 TO WRK-TIPO-LOG
           PERFORM 3700-INCLUIR-LOG

      * ATUALIZA PROTOCOLO - INCONSISTENCIA
           IF  DCOMBA-COD-RETORNO      NOT EQUAL ZEROS
               MOVE 9                  TO WRK-CSIT-PROT-AGNDA
               PERFORM 3600-ATUALIZAR-SITUACAO
           END-IF

           EVALUATE  DCOMBA-COD-RETORNO
              WHEN 00
                   CONTINUE
              WHEN 08
      *--> ERRO DE CONSISTENCIA
                   MOVE 08             TO WRK-SAI-COD-RETORNO
                   MOVE DCOMBA-MSG-RETORNO
                                       TO WRK-SAI-MSG-RETORNO
                   MOVE WRK-MSGF       TO WRK-FUNCAO
                   PERFORM 4000-ENVIAR-MENSAGEM
              WHEN OTHER
      *--> GENERICO
                   MOVE DCOMBA-COD-RETORNO
                                       TO WRK-SAI-COD-RETORNO
                   MOVE DCOMBA-MSG-RETORNO
                                       TO WRK-SAI-MSG-RETORNO
                   MOVE WRK-MSGF       TO WRK-FUNCAO
                   PERFORM 4000-ENVIAR-MENSAGEM
           END-EVALUATE
           .

      *----------------------------------------------------------------*
BRQ002 3320-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       3350-ACESSA-BVVEA501            SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  BVVENA-COMMAREA

           MOVE WRK-ENT-PROTOCOLO      TO BVVENA-E-PROTOCOLO

           MOVE 'BVVEA501'             TO WRK-MODULO

           CALL WRK-MODULO             USING BVVENA-COMMAREA

BRQ002*--- DEVOLUCAO DO CONE NA 2A CHAMADA C/ ERRO
.          IF  BVVENA-S-COD-RETORNO    NOT EQUAL ZEROS AND 01
.              MOVE 'D'                TO WRK-FUNC-CONE
.              PERFORM 3320-SENSIBILIZA-CONE
BRQ002     END-IF

           EVALUATE  BVVENA-S-COD-RETORNO
               WHEN 00
      *--> PROCESSAMENTO OK - CONE TOMADO
                   PERFORM 3400-ACESSA-BVVEA505
                   MOVE ZEROS          TO WRK-SAI-COD-RETORNO
                   MOVE WRK-MSG090     TO WRK-SAI-MSG-RETORNO
BRQ001             MOVE 3              TO WRK-CSIT-PROT-AGNDA
BRQ001             PERFORM 3600-ATUALIZAR-SITUACAO
               WHEN 01
      *--> EM PROCESSAMENTO - CONE TOMADO
                   MOVE 00             TO WRK-SAI-COD-RETORNO
                   MOVE WRK-MSG084     TO WRK-SAI-MSG-RETORNO
                   MOVE WRK-MSGF       TO WRK-FUNCAO
                   MOVE 2              TO WRK-CSIT-PROT-AGNDA
                   PERFORM 3600-ATUALIZAR-SITUACAO
                   PERFORM 4000-ENVIAR-MENSAGEM
              WHEN 08
      *--> ERRO DE CONSISTENCIA
                   MOVE 08             TO WRK-SAI-COD-RETORNO
                   MOVE WRK-MSG091     TO WRK-SAI-MSG-RETORNO
                   MOVE WRK-MSGF       TO WRK-FUNCAO
                   PERFORM 4000-ENVIAR-MENSAGEM
              WHEN 16
      *--> ERRO DB2
                   MOVE 16             TO WRK-SAI-COD-RETORNO
                   MOVE WRK-MSG095     TO WRK-SAI-MSG-RETORNO
                   MOVE WRK-MSGF       TO WRK-FUNCAO
                   PERFORM 4000-ENVIAR-MENSAGEM
           END-EVALUATE
           .

      *----------------------------------------------------------------*
       3350-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       3400-ACESSA-BVVEA505            SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  BVVENE-COMMAREA

           MOVE WRK-ENT-PROTOCOLO      TO BVVENE-E-PROTOCOLO
           MOVE 'BVVEA505'             TO WRK-MODULO

           CALL WRK-MODULO             USING BVVENE-COMMAREA

           MOVE BVVENE-S-COD-SIT       TO WRK-SAI-SIT-PROTOCOLO

BRQ002*--- DEVOLUCAO DO CONE NA 2A CHAMADA C/ ERRO
.          IF  BVVENE-S-COD-RETORNO    NOT EQUAL ZEROS
.              MOVE 'D'                TO WRK-FUNC-CONE
.              PERFORM 3320-SENSIBILIZA-CONE
BRQ002     END-IF

           EVALUATE  BVVENE-S-COD-RETORNO
              WHEN 00
      *--> PROCESSAMENTO OK
                   PERFORM 3500-ACESSAR-DCOM3934
                   MOVE ZEROS          TO WRK-SAI-COD-RETORNO
                   MOVE BVVENE-S-MSG-RETORNO
                                       TO WRK-SAI-MSG-RETORNO
              WHEN 08
      *--> ERRO DE CONSISTENCIA
                   MOVE 08             TO WRK-SAI-COD-RETORNO
                   MOVE WRK-MSG091     TO WRK-SAI-MSG-RETORNO
                   PERFORM 4000-ENVIAR-MENSAGEM
              WHEN 09
      *--> SALDO INDISPONIVEL
CAP001*           -MOVE 08             TO WRK-SAI-COD-RETORNO
CAP001             MOVE 09             TO WRK-SAI-COD-RETORNO
                   MOVE WRK-MSG099     TO WRK-SAI-MSG-RETORNO
      * REJEITADO POR SALDO INDISPONIVEL - NOVA SITUA��O SOLICITADA
                   MOVE 9              TO WRK-CSIT-PROT-AGNDA
                   PERFORM 3600-ATUALIZAR-SITUACAO
                   PERFORM 4000-ENVIAR-MENSAGEM
              WHEN 11
      *--> ERRO DE COMUNICACAO COM A CIP
                   MOVE 12             TO WRK-SAI-COD-RETORNO
                   MOVE WRK-MSG097     TO WRK-SAI-MSG-RETORNO
                   MOVE 6              TO WRK-CSIT-PROT-AGNDA
                   PERFORM 3600-ATUALIZAR-SITUACAO
                   PERFORM 4000-ENVIAR-MENSAGEM
              WHEN 13
      *--> REJEITADO NA CIP
                   MOVE 13             TO WRK-SAI-COD-RETORNO
                   MOVE WRK-MSG092     TO WRK-SAI-MSG-RETORNO
                   MOVE 5              TO WRK-CSIT-PROT-AGNDA
                   PERFORM 3600-ATUALIZAR-SITUACAO
                   PERFORM 4000-ENVIAR-MENSAGEM
              WHEN 16
      *--> ERRO DB2
                   MOVE 16             TO WRK-SAI-COD-RETORNO
                   MOVE WRK-MSG095     TO WRK-SAI-MSG-RETORNO
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-EVALUATE
      *
           .

      *----------------------------------------------------------------*
       3400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

F2404 *----------------------------------------------------------------*
INICIO 3450-ACESSAR-BVVEA520           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE BVVEFX-COMMAREA

           MOVE WRK-ENT-CAG-BCRIA      TO BVVEFX-E-AGENCIA
           MOVE WRK-ENT-CCTA-BCRIA-CLI TO BVVEFX-E-CONTA

           MOVE 'BVVEA520'             TO WRK-MODULO
           CALL WRK-MODULO             USING BVVEFX-COMMAREA

           EVALUATE BVVEFX-S-COD-RETORNO
               WHEN 00
                    CONTINUE

               WHEN OTHER
                    MOVE 'N'           TO BVVEFX-S-IND-CLI-MASSV
                    MOVE BVVEFX-S-MSG-RETORNO
                                       TO WRK-SAI-MSG-RETORNO
           END-EVALUATE.

      *----------------------------------------------------------------*
FIM    3450-99-FIM.                    EXIT.
F2404 *----------------------------------------------------------------*

      * GERA E EFETIVA OPERACAO                                        *
BRQ002* ALTERADO INTERNAMENTE PARA NAO CONSULTAR O CONE                *
      *----------------------------------------------------------------*
       3500-ACESSAR-DCOM3934           SECTION.
      *----------------------------------------------------------------*
      *
           INITIALIZE                  WRK-3934-ENTRADA
                                       WRK-3934-SAIDA
      *
           PERFORM 3220-OBTER-CPF

BRQ002*- GERA OPERACAO NA 2A CHAMADA COM PROTOCOLO
BRQ002*- NAO SENSIBILIZA CONE, JA FEZ NA 1A CHAMADA
           IF WRK-LIMITE               EQUAL 'N'
              PERFORM 3550-OBTER-OPERACAO
           ELSE
BRQ002*- GERA OPERACAO NA 1A CHAMADA SEM PROTOCOLO
BRQ002*- SENSIBILIZA CONE NORMALMENTE
              MOVE ZEROS               TO WRK-DANO
                                          WRK-NSEQ
           END-IF
      *
           MOVE 'NETEMPR'              TO WRK-3934E-CANAL

           IF WRK-ENT-CANAL            EQUAL 'NETEMPR'
              MOVE 3                   TO WRK-3934E-MIDIA
           ELSE
              MOVE 5                   TO WRK-3934E-MIDIA
           END-IF
      *
           MOVE 237                    TO WRK-3934E-CBCO
      *
           MOVE WRK-ENT-CAG-BCRIA      TO WRK-3934E-CAG-BCRIA
      *
           MOVE WRK-ENT-CCTA-BCRIA-CLI TO WRK-3934E-CCTA-BCRIA-CLI
      *
           MOVE WRK-CPF                TO WRK-3934E-CCNPJ-CPF
      *
           MOVE WRK-CFLIAL-CPF         TO WRK-3934E-CFLIAL-CNPJ
      *
           MOVE WRK-CCTRL-CPF          TO WRK-3934E-CCTRL-CNPJ-CPF
      *
           MOVE WRK-ENT-NSMULA-OPER    TO WRK-3934E-NRO-SIMULA
      *
           MOVE WRK-DANO               TO WRK-3934E-DANO-OPER
      *
           MOVE WRK-NSEQ               TO WRK-3934E-NSEQ-OPER
      *
           MOVE 'DCOM3934'             TO WRK-MODULO
      *
           CALL WRK-MODULO             USING WRK-3934-ENTRADA
                                             WRK-3934-SAIDA
                                             ERRO-AREA
                                             WRK-SQLCA.
      *
           IF  WRK-3934S-COD-RETORNO   NOT EQUAL ZEROS

BRQ002*--- DEVOLUCAO DO CONE NA 2A CHAMADA C/ ERRO
.              IF WRK-LIMITE           EQUAL 'N' AND
.                 WRK-ENT-PROTOCOLO    NOT EQUAL SPACES
.                 MOVE 'D'             TO WRK-FUNC-CONE
.                 PERFORM 3320-SENSIBILIZA-CONE
BRQ002         END-IF
      *
               MOVE WRK-3934S-COD-RETORNO
                                       TO WRK-SAI-COD-RETORNO
               MOVE WRK-3934S-MSG-RETORNO
                                       TO WRK-SAI-MSG-RETORNO
               MOVE WRK-MSGF           TO WRK-FUNCAO
               PERFORM 4000-ENVIAR-MENSAGEM
           END-IF.
      *
      *----------------------------------------------------------------*
       3500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       3550-OBTER-OPERACAO             SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  DCOMB0P5

           MOVE 237                    TO CBCO
                                       OF DCOMB0P5
      *
           MOVE WRK-ENT-CAG-BCRIA      TO CAG-BCRIA
                                       OF DCOMB0P5
      *
           MOVE WRK-ENT-CCTA-BCRIA-CLI
                                       TO CCTA-BCRIA-CLI
                                       OF DCOMB0P5
      *
           MOVE WRK-ENT-PROTOCOLO      TO CPROT-SOLTC-AGNDA
                                       OF DCOMB0P5
      *
           MOVE 'E'                    TO CTPO-PROT-AGNDA
                                       OF DCOMB0P5
      *
           EXEC SQL
                SELECT
                      DANO_OPER_DESC
                     ,NSEQ_OPER_DESC
                INTO  :DCOMB0P5.DANO-OPER-DESC
                           :WRK-DANO-OPER-DESC-N
                     ,:DCOMB0P5.NSEQ-OPER-DESC
                           :WRK-NSEQ-OPER-DESC-N
                FROM DB2PRD.TPROT_SOLTC_AGNDA
                WHERE CBCO              = :DCOMB0P5.CBCO
                AND   CAG_BCRIA         = :DCOMB0P5.CAG-BCRIA
                AND   CCTA_BCRIA_CLI    = :DCOMB0P5.CCTA-BCRIA-CLI
                AND   CTPO_PROT_AGNDA   = :DCOMB0P5.CTPO-PROT-AGNDA
                AND   CPROT_SOLTC_AGNDA = :DCOMB0P5.CPROT-SOLTC-AGNDA
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100)
           OR (SQLWARN0                EQUAL 'W')

BRQ002*--- DEVOLUCAO DO CONE NA 2A CHAMADA C/ ERRO - ERRO DB2
.              IF WRK-LIMITE           EQUAL 'N' AND
.                 WRK-ENT-PROTOCOLO    NOT EQUAL SPACES
.                 MOVE 'D'             TO WRK-FUNC-CONE
.                 PERFORM 3320-SENSIBILIZA-CONE
BRQ002         END-IF

               MOVE SQLCODE            TO HX-SQLCODE-09
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE '0090'             TO ERR-LOCAL
               MOVE 'TPROT_SOLTC_AGNDA'
                                       TO ERR-DBD-TAB
               MOVE SQLCODE            TO ERR-SQL-CODE
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.
      *
           IF (WRK-DANO-OPER-DESC-N    LESS ZEROS)
               MOVE ZEROS              TO DANO-OPER-DESC
                                       OF DCOMB0P5
           END-IF
      *
           IF (WRK-NSEQ-OPER-DESC-N    LESS ZEROS)
               MOVE ZEROS              TO NSEQ-OPER-DESC
                                       OF DCOMB0P5
           END-IF

           IF  SQLCODE                 NOT EQUAL ZEROS

BRQ002*--- DEVOLUCAO DO CONE NA 2A CHAMADA C/ ERRO - NAO ACHOU PROTOCOLO
.              IF WRK-LIMITE           EQUAL 'N' AND
.                 WRK-ENT-PROTOCOLO    NOT EQUAL SPACES
.                 MOVE 'D'             TO WRK-FUNC-CONE
.                 PERFORM 3320-SENSIBILIZA-CONE
BRQ002         END-IF

               MOVE 08                 TO WRK-SAI-COD-RETORNO
               MOVE WRK-MSG086         TO WRK-SAI-MSG-RETORNO
               MOVE WRK-MSGF           TO WRK-FUNCAO
               PERFORM 4000-ENVIAR-MENSAGEM
           END-IF

           MOVE DANO-OPER-DESC         OF DCOMB0P5
                                       TO HX-NUM-04-CS
           MOVE HX-NUM-04-SS           TO WRK-DANO
           MOVE NSEQ-OPER-DESC         OF DCOMB0P5
                                       TO HX-NUM-09-CS
           MOVE HX-NUM-09-SS           TO WRK-NSEQ
           .
      *----------------------------------------------------------------*
       3550-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       3600-ATUALIZAR-SITUACAO         SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  DCOMB0P5
      *
           MOVE 237                    TO CBCO
                                       OF DCOMB0P5
      *
           MOVE WRK-ENT-CAG-BCRIA      TO CAG-BCRIA
                                       OF DCOMB0P5
      *
           MOVE WRK-ENT-CCTA-BCRIA-CLI TO CCTA-BCRIA-CLI
                                       OF DCOMB0P5
      *
           MOVE WRK-ENT-PROTOCOLO      TO CPROT-SOLTC-AGNDA
                                       OF DCOMB0P5
      *
           MOVE WRK-CSIT-PROT-AGNDA    TO CSIT-PROT-AGNDA
                                       OF DCOMB0P5
      *
           EXEC SQL
                UPDATE DB2PRD.TPROT_SOLTC_AGNDA
                SET   CSIT_PROT_AGNDA   = :DCOMB0P5.CSIT-PROT-AGNDA
                WHERE CBCO              = :DCOMB0P5.CBCO
                AND   CAG_BCRIA         = :DCOMB0P5.CAG-BCRIA
                AND   CCTA_BCRIA_CLI    = :DCOMB0P5.CCTA-BCRIA-CLI
                AND   CPROT_SOLTC_AGNDA = :DCOMB0P5.CPROT-SOLTC-AGNDA
                AND   CTPO_PROT_AGNDA   = 'E'
           END-EXEC.
      *
           IF (SQLCODE                 NOT EQUAL ZEROS)
           OR (SQLWARN0                EQUAL 'W')
               MOVE SQLCODE            TO HX-SQLCODE-09
               MOVE 'UPDATE'           TO ERR-FUN-COMANDO
               MOVE '0100'             TO ERR-LOCAL
               MOVE 'TPROT_SOLTC_AGNDA'
                                       TO ERR-DBD-TAB
               MOVE SQLCODE            TO ERR-SQL-CODE
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF
           .
      *
      *----------------------------------------------------------------*
       3600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       3700-INCLUIR-LOG                SECTION.
      *----------------------------------------------------------------*

           MOVE 'DCOM7999'             TO WRK-MODULO.

           CALL  WRK-MODULO            USING WRK-TIMESTAMP-ATUAL
                                             ERRO-AREA
                                             WRK-SQLCA.

           INITIALIZE X1-ARGUMENTOS-ENTRADA
                      X1-RETORNO
                      WRK-LOG-CONE.

      *--- AREA SENSIBILIZA CONE
           MOVE DCOMBA-ENTRADA         TO WRK-DCOMBA-AREA
           MOVE WRK-TIPO-LOG           TO WRK-LOG-TXT

BRQ003*--  FORMATA RETORNO DCOM3933 E SIMULACAO
BRQ003     IF  WRK-TIPO-LOG EQUAL 'FIM'
BRQ003         MOVE WRK-SAI-COD-RETORNO   TO WRK-LOG-COD-RETORNO
BRQ003         MOVE WRK-SAI-MSG-RETORNO(1:75)
BRQ003                                 TO WRK-LOG-MSG-RETORNO
BRQ003         IF DCOMBA-NSEQ-OPER-DESC EQUAL ZEROS
BRQ003            MOVE WRK-ENT-NSMULA-OPER
BRQ003                                 TO DCOMBA-NSEQ-OPER-DESC
BRQ003         END-IF
BRQ003     END-IF

           MOVE 012                    TO X1-CELMTO-DESC-COML.
           MOVE 036                    TO X1-CEVNTO-LOG-DESC.
           MOVE WRK-TIMESTAMP-ATUAL    TO X1-HULT-ATULZ
           MOVE 'I'                    TO X1-CINDCD-MANUT-REG
           MOVE DCOMBA-DANO-OPER-DESC  TO WRK-LOG-DANO-OPER.
           MOVE DCOMBA-NSEQ-OPER-DESC  TO WRK-LOG-NSEQ-OPER.
           MOVE WRK-LOG-CCHAVE-ELMTO   TO X1-CCHAVE-ELMTO-DESC.

           MOVE LENGTH                 OF WRK-LOG-CONE
                                       TO X1-WREG-LOG-APOS-LEN.
           MOVE WRK-LOG-CONE           TO X1-WREG-LOG-APOS-TEXT
                                         (1:LENGTH OF WRK-LOG-CONE).

           MOVE 'DCOM3933'             TO X1-CTRANS-PROG-ATULZ.
           MOVE 'O'                    TO X1-CINDCD-MODLD-PROCM.
           MOVE 237                    TO X1-CEMPR-INC.
           MOVE 4130                   TO X1-CDEPDC.
BRQ003     MOVE 1                      TO X1-CFUNC-BDSCO.
BRQ003     MOVE 'NETEMPR'              TO X1-CTERM.
           MOVE DCOMBA-CFUNC-BDSCO     TO X1-CFUNC-BDSCO.
           MOVE DCOMBA-CTERM           TO X1-CTERM.
           MOVE 'DCOM5075'             TO WRK-MODULO.

           CALL WRK-MODULO             USING X1-REGISTRO
                                             ERRO-AREA
                                             WRK-SQLCA.

           IF  X1-COD-RETORNO          NOT EQUAL ZEROS
               MOVE X1-COD-RETORNO     TO WRK-SAI-COD-RETORNO
               MOVE X1-MENSAGEM        TO WRK-SAI-MSG-RETORNO
               PERFORM 5000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       3700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     FINALIZAR PROCESSO ENVIANDO MENSAGEM AO SBAT3300           *
      *----------------------------------------------------------------*
       4000-ENVIAR-MENSAGEM            SECTION.
      *----------------------------------------------------------------*

TESTE      MOVE WRK-SAI-COD-RETORNO    TO WRK-LOG-COD-RETORNO
.          MOVE WRK-SAI-MSG-RETORNO(1:75)
.                                      TO WRK-LOG-MSG-RETORNO
.
.          MOVE 'FIM '                 TO WRK-TIPO-LOG
TESTE      PERFORM 3700-INCLUIR-LOG

           MOVE WRK-MSGF               TO WRK-FUNCAO

           IF  WRK-FUNCAO              EQUAL WRK-MSGF
               CALL 'SBAT3300'         USING WRK-FUNCAO
                                             LNK-IO-PCB
                                             LNK-ALT-PCB
                                             MCA-ISDPA
                                             WRK-AREA-SAIDA
           END-IF.

           IF  MPA-RCODE-API           NOT EQUAL SPACES
               MOVE '0110'             TO ERR-LOCAL
               MOVE MPA-RCODE-API      TO ERR-STA-CODE
                                          WRK-STATUS
               MOVE WRK-MSGF           TO WRK-MSG02-FUNCAO
               MOVE WRK-MSG02          TO WRK-SAI-MSG-RETORNO
               MOVE 0100               TO WRK-SAI-COD-RETORNO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  WRK-FUNCAO              EQUAL WRK-MSGF
               PERFORM 5000-FINALIZAR
           END-IF
           .

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       5000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *---------------------------------------------------------------*

           MOVE MPA-CODIGO-USUARIO     TO ERR-COD-USER.
           MOVE ZEROS                  TO HX-COD-DEPTO-06.
           MOVE MPA-AGEN-DEPTO         TO HX-COD-DEPTO-04.
           MOVE HX-COD-DEPTO-06        TO ERR-COD-DEPTO.

           IF  WRK-SAI-COD-RETORNO   NOT EQUAL 0099
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'DCOM3933'         TO ERR-PGM
               MOVE WRK-SAI-MSG-RETORNO(1:75)
                                       TO ERR-TEXTO

               CALL 'BRAD7106'         USING LNK-IO-PCB
                                             ERRO-AREA
                                             LNK-ALT-PCB
           ELSE
               MOVE WRK-MODULO         TO WRK-PROGRAM
               MOVE ERR-SQL-CODE       TO HX-SQLCODE-09
               MOVE HX-SQLCODE-04      TO WRK-COD-ID
               MOVE SG-MENSAGEM(0099)  TO WRK-SAI-MSG-RETORNO
               MOVE HX-SQLCODE-SINAL   TO WRK-SAI-MSG-RETORNO(56:01)

               CALL 'BRAD7106'         USING LNK-IO-PCB
                                             ERRO-AREA
                                             LNK-ALT-PCB
                                             WRK-SQLCA
           END-IF.

           CALL 'SBAT3300'             USING WRK-ROLB
                                             LNK-IO-PCB
                                             LNK-ALT-PCB
                                             MCA-ISDPA.

           CALL 'SBAT3300'             USING WRK-MSGF
                                             LNK-IO-PCB
                                             LNK-ALT-PCB
                                             MCA-ISDPA
                                             WRK-AREA-SAIDA.

           PERFORM 5000-FINALIZAR.

      *---------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *---------------------------------------------------------------*
