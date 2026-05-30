      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID.     DCOM0405.
       AUTHOR.         ADRIANO RICARDO BELMONT.
      *================================================================*
      *                          A L T R A N                           *
      *----------------------------------------------------------------*
      *     PROGRAMA....: DCOM0405                                     *
      *     PROGRAMADOR.: ADRIANO RICARDO BELMONT                      *
      *     ANALISTA....: DANILO                                       *
      *     DATA........: 01/02/2007                                   *
      *----------------------------------------------------------------*
      *     OBJETIVO....: LIBERAR OPERACAO - DEMONSTRATIVO             *
      *----------------------------------------------------------------*
      *     TELAS.......:                                              *
      *     DCOMDETM  -   LIBERAR OPERACAO - DEMONSTRATIVO             *
      *----------------------------------------------------------------*
      *     INC'S.......:                                              *
      *     I#BRAD7C - AREA DE ERRO DA BRAD7100                        *
      *----------------------------------------------------------------*
      *     MODULOS:                                                   *
      *     BRAD0660 - INSERIR CARACTERES DE NULL NA MENSAGEM DE SAIDA *
      *     BRAD7100 - TRATAMENTO DE ERROS QUANDO PROGRAMA INVALIDO    *
      *     POOL5005 - CONTROLE DO TRANSITO DE MENSAGENS NO IMS/DC     *
      *     BRAD7600 - FORNECE DATA E HORA CORRENTE DO SISTEMA         *
      *                                                                *
      *     CHAMA:                                                     *
      *     DCOM0214 - CONSULTA LIBERACAO - RESUMO DA ANALISE          *
      *     DCOM0222 - CONSULTA LIBERACAO - RESUMO DA ANALISE          *
      *                                                                *
      *----------------------------------------------------------------*
      *     NAVEGACAO...:                                              *
      *     CHAMADO POR.: DCOM0402           CHAMA: DCOM0214           *
      *                   DCOM0403                  DCOM0222           *
      *                   DCOM0404                                     *
      *================================================================*
DTS001*                    ALTERACAO - DTS CONSULTING                  *
DTS001*----------------------------------------------------------------*
DTS001*    PROGRAMADORA:   LUCIANDRA SILVEIRA - DTS CONSULTING         *
DTS001*    ANALISTA....:   LUCIANDRA SILVEIRA - DTS CONSULTING         *
DTS001*    ANALISTA DTS:   VERA MARIA POLLINI - BRADESCO - GRUPO 70    *
DTS001*    DATA........:   05/02/2010                                  *
DTS001*----------------------------------------------------------------*
DTS001*    OBJETIVO....:   EXECUTAR ROLLBACK CASO OCORRA ERRO NO MOMEN-*
DTS001*                    TO DO CANCELAMENTO DA OPERACAO (DCOM5113).  *
DTS001*================================================================*
DTS002*                   ALTERACAO  -  DTS  CONSULTING                *
DTS002*----------------------------------------------------------------*
DTS002*     PROGRAMADORA: LUCIANDRA SILVEIRA  -  DTS CONSULTING        *
DTS002*     ANALISTA....: LUCIANDRA SILVEIRA  -  DTS CONSULTING        *
DTS002*     ANALISTA DDS: VERA/EDGARD         -  GRUPO 70              *
DTS002*     DATA........: 06/05/2010                                   *
DTS002*----------------------------------------------------------------*
DTS002*     OBJETIVO....: NAO PERMITIR VALIDACAO DE OPERACAO CADASTRA- *
DTS002*                   DA PELO NET EMPRESA.                         *
DTS002*----------------------------------------------------------------*
DTS002*     MODULOS.....:                                              *
DTS002*     DCOM5505 - OBTEM RETORNO DAS SITUACOES DO WORKFLOW ENVIADO *
DTS002*================================================================*
DTS003*                   ALTERACAO  -  DTS  CONSULTING                *
DTS003*----------------------------------------------------------------*
DTS003*     PROGRAMADORA: LUCIANDRA SILVEIRA     - DTS CONSULTING      *
DTS003*     ANALISTA....: LUCIANDRA SILVEIRA     - DTS CONSULTING      *
DTS003*     ANALISTA DDS: LUIS EDUARDO DE MORAES - GRUPO 70            *
DTS003*     DATA........: 06/09/2011                                   *
DTS003*----------------------------------------------------------------*
DTS003*     OBJETIVO....: CORRIGIR AREA DE COMUNICACAO NA CHAMADA AO   *
DTS003*                   DCOM0215 (RO 3089375).                       *
DTS003*================================================================*
BRQ001*                    A L T E R A C A O  - B R Q                  *
      *----------------------------------------------------------------*
      *    ANALISTA.....:  LAERTES / LUIS EDUARDO      - BRQ           *
      *    DATA.........:  AGOSTO / 2016                               *
      *----------------------------------------------------------------*
      *    OBJETIVO.....:  TRATAMENTO PARAMETRO LIMITE NETEMPRESA.     *
      *       INCLUIR O CAMPO DE CANAL NA TELA                         *
BRQ001*================================================================*
BRQ002*                   ALTERACAO  -  BRQ DIGITAL SOLUTIONS          *
BRQ002*----------------------------------------------------------------*
BRQ002*    PROGRAMADOR.:  LUCIANDRA SILVEIRA - BRQ DIGITAL SOLUTIONS   *
BRQ002*    ANALISTA BRQ:  FABIO AUGUSTO FINK - BRQ DIGITAL SOLUTIONS   *
BRQ002*    ANALISTA DS.:  RICARDO JAMMAL     - BRADESCO / GP.39        *
BRQ002*    DATA........:  26/11/2018                                   *
BRQ002*----------------------------------------------------------------*
BRQ002*    OBJETIVO....:  BRQ 18/0456 - BORDERO ELETRONICO.            *
BRQ002*      INCLUIR TRATAMENTO PARA CAMPO INDICADOR DE LIBERACAO      *
BRQ002*      NO CANAL.                                                 *
BRQ002*----------------------------------------------------------------*
BRQ002*    MODULO......:                                               *
BRQ002*        DCOM5490 - CONSISTIR OPERACAO PENDENTE DE LIBERACAO     *
BRQ002*                   NAS MIDIAS.                                  *
BRQ002*================================================================*
CAP001*                   ALTERACAO - CAPGEMINI                        *
|     *----------------------------------------------------------------*
|     *   PROGRAMADOR.:   AMANDA BELTOSO     - CAPGEMINI               *
|     *   ANALISTA CAP:   POLIANA SILVA      - CAPGEMINI               *
|     *   ANALISTA DDS:   RICARDO JAMMAL     - BRADESCO - GRUPO 39     *
|     *   DATA........:   09/2020                                      *
|     *----------------------------------------------------------------*
|     *   OBJETIVO....: SOLICITAR PROTOCOLO DE EFETIVACAO NA CIP       *
|     *                 ACESSAR BVVEA504 E BVVEA505 E CRIAR O PROTOCOLO*
|     *                 DE LIBERACAO COM O PF6 E VERIFICAR NO BVVE COM *
|     *                 O PF11                                         *
CAP001*================================================================*
FM1510*                   ALTERACAO - CAPGEMINI                        *
.     *----------------------------------------------------------------*
.     *   PROGRAMADOR.:   FELIPE MENDES      - CAPGEMINI               *
.     *   ANALISTA CAP:   FELIPE MENDES      - CAPGEMINI               *
.     *   ANALISTA DDS:   RICARDO JAMMAL     - BRADESCO - GRUPO 39     *
.     *   DATA........:   15/10/2020                                   *
.     *----------------------------------------------------------------*
.     *   OBJETIVO....: NAO PERMITIR LIBERACAO DE OPERCAO COM DATA DE  *
.     *                 30.0.2020                                      *
FM1510*================================================================*
CAP002*                   ALTERACAO - CAPGEMINI                        *
.     *----------------------------------------------------------------*
.     *   PROGRAMADOR.:   ANDREA             - CAPGEMINI               *
.     *   ANALISTA CAP:   ANDREA             - CAPGEMINI               *
.     *   ANALISTA DDS:   RICARDO JAMMAL     - BRADESCO - GRUPO 39     *
.     *   DATA........:   05/2021                                      *
.     *----------------------------------------------------------------*
.     *   OBJETIVO....: NAO PERMITIR LIBERACAO DE ORPAGS ENTRE OS DIAS *
.     *                 03 E 06.06.2021 - NORMAT.4734                  *
CAP002*================================================================*
CAP003*                   ALTERACAO - CAPGEMINI                        *
.     *----------------------------------------------------------------*
.     *   PROGRAMADOR.:   AMANDA BELTOSO     - CAPGEMINI               *
.     *   ANALISTA CAP:   AMANDA BELTOSO     - CAPGEMINI               *
.     *   ANALISTA DDS:   RICARDO JAMMAL     - BRADESCO - GRUPO 39     *
.     *   DATA........:   06/2021                                      *
.     *----------------------------------------------------------------*
.     *   OBJETIVO....: CORRECAO PARA MENSAGEM DE REGISTRO NA CIP      *
CAP003*                 APARECER SOMENTE PARA TIPO DE DESCONTO 4       *
CAP004*================================================================*
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
CAP004*                 SOMENTE UMA VEZ                                *
CAP005*================================================================*
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
CAP005*================================================================*
CAP006*================================================================*
|     *                   ALTERACAO - CAPGEMINI                        *
|     *----------------------------------------------------------------*
|     *   PROGRAMADOR :   AMANDA BELTOSO     - CAPGEMINI               *
|     *   ANALISTA CAP:   AMANDA BELTOSO     - CAPGEMINI               *
|     *   ANALISTA DDS:   RICARDO JAMMAL     - BRADESCO - GRUPO 39     *
|     *   DATA........:   07/2021                                      *
|     *----------------------------------------------------------------*
|     *   OBJETIVO....: ADEQUAR SITUACAO DA OPERACAO                   *
CAP006*================================================================*
CAP007*================================================================*
|     *                   ALTERACAO - CAPGEMINI                        *
|     *----------------------------------------------------------------*
|     *   PROGRAMADOR :   ANDREA             - CAPGEMINI               *
|     *   ANALISTA CAP:   ANDREA             - CAPGEMINI               *
|     *   ANALISTA DDS:   RICARDO JAMMAL     - BRADESCO - GRUPO 39     *
|     *   DATA........:   10/2021                                      *
|     *----------------------------------------------------------------*
|     *   OBJETIVO....: SOLICITADO PELO JAMMAL ALTERACAO DA MSG        *
|     *              DE 'EFETIVACAO EM PROCESSAMENTO NA ADMINISTRADORA'*
|     *            PARA 'EFETIVACAO EM PROCESSAMENTO NA REGISTRADORA'  *
CAP007*================================================================*
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
      *           *** DCOM0405 - INICIO DA AREA DE WORKING ***         *
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *                ***     AREA DE ATRIBUTOS ***                   *
      *----------------------------------------------------------------*

      ****  NUMERICO - NORMAL    -    PROTEGIDO  - NAO POS. CURSOR  ---*
       01  WRK-241                     PIC S9(008) COMP  VALUE +241.
      ****  NUMERICO - NORMAL    -    PROTEGIDO  -     POS. CURSOR  ---*
       01  WRK-49361                   PIC S9(008) COMP  VALUE +49361.
      ****  NUMERICO - BRILHANTE - DESPROTEGIDO  -     POS. CURSOR  ---*
       01  WRK-49369                   PIC S9(008) COMP  VALUE +49369.

      *----------------------------------------------------------------*
      *               *** VARIAVEIS AUXILIARES ***                     *
      *----------------------------------------------------------------*

       01  WRK-AUXILIARES.
           05  WRK-SQLCA               PIC  X(136)       VALUE SPACES.
           05  WRK-MODULO              PIC  X(008)       VALUE SPACES.
           05  WRK-INCONSIS            PIC  X(001)       VALUE SPACES.
           05  WRK-FLAG-ERRO           PIC  X(001)       VALUE 'N'.
           05  WRK-MENSAGEM-ERRO       PIC  X(079)       VALUE SPACES.
           05  WRK-VLRMAXI-AUX         PIC  9(013)V99    COMP-3
                                                         VALUE ZEROS.
           05  WRK-VLRLIBR-AUX         PIC  9(013)V99    COMP-3
                                                         VALUE ZEROS.
BRQ002     05  WRK-FUNCAO-5490         PIC  X(002)       VALUE SPACES.
CAP001     05  WRK-ACHOU-SOLTC         PIC  X(001)         VALUE SPACES.
|          05  WRK-SAIR                PIC  X(001)         VALUE SPACES.
|          05  WRK-SEGUNDOS-COMP       PIC  9(002) COMP-3  VALUE ZEROS.
|          05  WRK-MINUTOS-COMP        PIC  9(002) COMP-3  VALUE ZEROS.
|          05  WRK-TEMPO-ESPERA        PIC  9(003) COMP-3  VALUE ZEROS.
|          05  IND-1                   PIC  9(003) COMP-3  VALUE ZEROS.
|          05  WRK-ACAO                PIC  X(001)         VALUE SPACES.
|          05  WRK-PROCESSADO          PIC  X(001)         VALUE 'N'.
|          05  WRK-OK                  PIC  X(001)         VALUE SPACES.
|          05  WRK-DESPREZAR           PIC  9(005) COMP-3  VALUE ZEROS.
|          05  WRK-FIM                 PIC  X(001)         VALUE SPACES.
|          05  WRK-PROTOCOLO           PIC  X(050)         VALUE SPACES.
|          05  WRK-TEMPO-ESPERA-X      PIC  X(008)         VALUE SPACES.
|          05  WRK-MIN-ATUAL           PIC  9(004) COMP-3  VALUE ZEROS.
|          05  WRK-MIN-INI             PIC  9(004) COMP-3  VALUE ZEROS.
|          05  WRK-SEG-ATUAL           PIC  9(004) COMP-3  VALUE ZEROS.
|          05  WRK-SEG-INI             PIC  9(004) COMP-3  VALUE ZEROS.
|          05  WRK-SITUACAO            PIC  9(001)         VALUE ZEROS.
CAP001     05  WRK-CALC-QTEMPO         PIC  9(004) COMP-3  VALUE ZEROS.


       01  WRK-AREA-DBTEMP.
           10  WRK-DADOS-DBTEMP    PIC  X(173)         VALUE SPACES.
           10  WRK-CHAMADOR-DBTEMP PIC  X(008)         VALUE SPACES.

       01  WRK-AREA-BRAD0496.
           10  WRK-VALOR-0496      PIC  X(006)         VALUE SPACES.
           10  WRK-SAIDA-0496.
               15 WRK-SAIDA-0496-N PIC  9(003)V9(002)  VALUE ZEROS.
           10  WRK-TAMANHO-0496    PIC  9(008)         VALUE ZEROS.

       01  WRK-AREA-BRAD0496-2.
           10  WRK-VALOR-0496-2    PIC  X(017)         VALUE SPACES.
           10  WRK-SAIDA-0496-2.
               15 WRK-SAIDA-0496-2-N PIC  9(014)V9(002)  VALUE ZEROS.
           10  WRK-TAMANHO-0496-2  PIC  9(008)         VALUE ZEROS.
CAP001
|     *
|      01  WRK-TEMPO                   PIC  X(008)         VALUE ZEROS.
|      01  FILLER                      REDEFINES WRK-TEMPO.
|          05 WRK-HORA                 PIC  9(002).
|          05 FILLER                   PIC  X(001).
|          05 WRK-MINUTOS              PIC  9(002).
|          05 FILLER                   PIC  X(001).
|          05 WRK-SEGUNDOS             PIC  9(002).
|     *
|      01  WRK-TIME-INI                PIC  X(008)         VALUE ZEROS.
|      01  FILLER                      REDEFINES WRK-TIME-INI.
|          05 WRK-HORA-INI             PIC  9(002).
|          05 WRK-MINUTO-INI           PIC  9(002).
|          05 WRK-SEGUNDOS-INI         PIC  9(002).
|          05 WRK-MILSEG-INI           PIC  9(002).
|     *
|      01  WRK-TIME-ATUAL              PIC  X(008)         VALUE ZEROS.
|      01  FILLER                      REDEFINES WRK-TIME-ATUAL.
|          05 WRK-HORA-ATUAL           PIC  9(002).
|          05 WRK-MINUTO-ATUAL         PIC  9(002).
|          05 WRK-SEGUNDOS-ATUAL       PIC  9(002).
|          05 WRK-MILSEG-ATUAL         PIC  9(002).
|     *
|      01  WRK-QTEMPO                  PIC  9(004)         VALUE ZEROS.
|      01  FILLER                      REDEFINES WRK-QTEMPO.
|          05 FILLER                   PIC  X(001).
|          05 WRK-QTEMPO-03            PIC  9(003).
|     *
|      01  WRK-CAMPO-AG-CC.
|          05 WRK-BANCO                PIC  9(003)         VALUE ZEROS.
|          05 FILLER                   PIC  X(001)         VALUE '/'.
|          05 WRK-AGENCIA              PIC  9(005)         VALUE ZEROS.
|          05 FILLER                   PIC  X(001)         VALUE '/'.
|          05 WRK-CONTA                PIC  9(013)         VALUE ZEROS.
CAP001*

FM0510 01  WRK-CONTRATO-BVVE.
.          05 WRK-SIGLA-CCUSTO         PIC  X(004)         VALUE SPACES.
.          05 WRK-DANO-OPER            PIC  9(004)         VALUE ZEROS.
.          05 WRK-NSEQ-OPER            PIC  9(009)         VALUE ZEROS.
FM0510     05 FILLER                   PIC  X(002)         VALUE SPACES.

      *----------------------------------------------------------------*
      *             *** AREAS AUXILIARES PARA POOL5005 ***             *
      *----------------------------------------------------------------*

       01  WRK-POOL5005.
           05  WRK-GU                  PIC  X(004)    VALUE 'GU  '.
           05  WRK-CHNG                PIC  X(004)    VALUE 'CHNG'.
           05  WRK-ISRT                PIC  X(004)    VALUE 'ISRT'.
           05  WRK-ROLB                PIC  X(004)    VALUE 'ROLB'.
           05  WRK-FUNCAO              PIC  X(004)    VALUE SPACES.
           05  WRK-TELA                PIC  X(008)    VALUE SPACES.
           05  WRK-VERSAO              PIC  X(006)    VALUE 'VRS002'.
           05  WRK-COD-USER            PIC  X(007)    VALUE SPACES.
           05  WRK-COD-USER-R          REDEFINES
               WRK-COD-USER            PIC  9(007).
           05  WRK-COD-DEPTO.
               10  FILLER              PIC  X(002)    VALUE SPACES.
               10  WRK-COD-DEPTO-N     PIC  9(004)    VALUE ZEROS.

           05  WRK-MENSAGEM.
               10  FILLER              PIC  X(004)    VALUE SPACES.
               10  WRK-TRANSACAO       PIC  X(008)    VALUE SPACES.
               10  FILLER              PIC  X(1988)   VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(080)         VALUE
           '*** AREA DE COMUNICACAO PROGRAMA DCOM4266              ***'.
      *----------------------------------------------------------------*

       01  4266E-ENTRADA.
           03  4266E-COD-RETORNO           PIC  X(004).
           03  4266E-MSG-RETORNO           PIC  X(079).
           03  4266E-RESTART               PIC  9(005).
           03  4266E-FLAG                  PIC  X(001).
           03  4266E-CFUNC-BDSCO           PIC  9(009).
           03  4266E-CTERM                 PIC  X(008).
           03  4266E-DANO-OPER-DESC        PIC  9(004).
           03  4266E-NSEQ-OPER-DESC        PIC  9(009).
FS2511     03  4266E-CCNPJ-CPF             PIC  X(009).
FS2511     03  4266E-CFLIAL-CNPJ           PIC  X(004).
      *    03  4266E-CCNPJ-CPF             PIC  9(009).
      *    03  4266E-CFLIAL-CNPJ           PIC  9(004).

       01  4266S-SAIDA.
           03  4266S-HEADER.
               05  4266S-COD-RETORNO       PIC  X(004).
               05  4266S-MSG-RETORNO       PIC  X(079).
               05  4266S-RESTART           PIC  9(005).
               05  4266S-FLAG              PIC  X(001).
           03  4266S-DADOS.
               05 4266S-CINDCD-FREQ-TX      PIC  X(02).
               05 4266S-CTPO-TX-JURO-DESC   PIC  9(03).
               05 4266S-CSGL-TPO-TX-JURO    PIC  X(06).
               05 4266S-IRSUMO-TPO-TX-JURO  PIC  X(15).
               05 4266S-ITPO-TX-JURO-DESC   PIC  X(30).

           03  4266S-CONSISTENCIA          REDEFINES 4266S-DADOS.
               05  4266S-TAMANHO-ERRO      PIC  9(001).
               05  4266S-TABELA-ERROS      OCCURS 02 TIMES.
                   07  4266S-CAMPOS-ERRO   PIC  9(001).

      *---------------------------------------------------------------*
      *              *** AREA DE ERRO DA BRAD0660 *****
      *---------------------------------------------------------------*

       01  WRK-ERRO-BRAD0660.
           05  FILLER                  PIC  X(045)    VALUE
               '* RETURN-CODE DIFERENTE DE ZEROS NA BRAD0660'.
           05  FILLER                  PIC  X(012)    VALUE
               '- RET.CODE ='.
           05  WRK-RETURN-CODE         PIC  9(002)    VALUE ZEROS.
           05  FILLER                  PIC  X(011)    VALUE
               ' - LOCAL ='.
           05  WRK-LOCAL-ERRO          PIC  9(002)    VALUE ZEROS.
           05  FILLER                  PIC  X(002)    VALUE '*'.


      *---------------------------------------------------------------*
      *        *** AREA DA TELA DCOMDETM  -  INPUT ***                *
      *---------------------------------------------------------------*

       01  INPUT-DETM.
           05  FILLER                    PIC  X(018)    VALUE SPACES.
           05  INP-PFK                   PIC  X(001)    VALUE SPACES.
           05  INP-SENHAS                PIC  X(037)    VALUE SPACES.
           05  INP-COMANDO               PIC  X(068)    VALUE SPACES.
           05  INP-DADOS.
               10 INP-TITULO             PIC  X(008)    VALUE SPACES.
               10 INP-FASE               PIC  X(001)    VALUE SPACES.
               10 INP-FLAGENT            PIC  X(001)    VALUE SPACES.
               10 INP-TIPODES            PIC  9(003)    VALUE ZEROS.
               10 INP-TRANSAC            PIC  X(008)    VALUE ZEROS.
               10 INP-TMSTAMP            PIC  X(026)    VALUE SPACES.
               10 INP-NOME               PIC  X(040)    VALUE SPACES.
BRQ001         10 WRK-INP-CANAL          PIC  X(010)    VALUE SPACES.
               10 INP-CNPJCPF            PIC  X(019)    VALUE SPACES.
               10 INP-CAMPO2             PIC  X(023)    VALUE SPACES.
               10 INP-PRODUTO            PIC  9(003)    VALUE ZEROS.
               10 INP-DPRODUT            PIC  X(012)    VALUE SPACES.
               10 INP-SPRODUT            PIC  9(003)    VALUE ZEROS.
               10 INP-DSPRODU            PIC  X(012)    VALUE SPACES.
               10 INP-OPERAC             PIC  9(013)    VALUE ZEROS.
DTS002         10 FILLER REDEFINES       INP-OPERAC.
DTS002            15 WRK-INP-DANO        PIC  9(004).
DTS002            15 WRK-INP-NSEQ        PIC  9(009).
               10 INP-SITUAC             PIC  X(015)    VALUE SPACES.
               10 INP-CAMPO3             PIC  X(013)    VALUE SPACES.
               10 INP-CAMPO4             PIC  9(009)    VALUE ZEROS.
               10 INP-DTOPER             PIC  X(010)    VALUE SPACES.
               10 INP-DTVCTOF            PIC  X(010)    VALUE SPACES.
               10 INP-VLROPER-X          PIC  X(014).
               10 INP-VLROPER-E REDEFINES INP-VLROPER-X
                                         PIC   ZZZ.ZZZ.ZZ9,99.
               10 INP-TPOTAXA            PIC  X(015)    VALUE SPACES.
               10 INP-VLTAROP.
                  15 INP-VLTAROP-X       PIC  X(014).
                  15 INP-VLTAROP-E REDEFINES INP-VLTAROP-X
                                         PIC   ZZZ.ZZZ.ZZ9,99.
               10 INP-TAXAJUR.
                  15 INP-TAXAJUR-X       PIC  X(009).
                  15 INP-TAXAJUR-E REDEFINES INP-TAXAJUR-X
                                         PIC  ZZ9,99999.
               10 INP-NMJUROS            PIC  X(004)    VALUE SPACES.
               10 INP-VLTRFTT.
                  15 INP-VLTRFTT-X       PIC  X(014).
                  15 INP-VLTRFTT-E REDEFINES INP-VLTRFTT-X
                                         PIC  ZZZ.ZZZ.ZZ9,99.
               10 INP-VLRJUR.
                  15 INP-VLRJUR-X        PIC  X(014).
                  15 INP-VLRJUR-E REDEFINES INP-VLRJUR-X
                                         PIC  ZZZ.ZZZ.ZZ9,99.
               10 INP-PRZMED             PIC  9(003)    VALUE ZEROS.
               10 INP-VLRIOF.
                  15 INP-VLRIOF-X        PIC  X(014).
                  15 INP-VLRIOF-E  REDEFINES  INP-VLRIOF-X
                                         PIC  ZZZ.ZZZ.ZZ9,99.
               10 INP-QTDETIT.
                  15 INP-QTDETIT-N       PIC  ZZ.ZZ9.
               10 INP-VLRLIQD.
                  15 INP-VLRLIQD-X       PIC  X(014).
                  15 INP-VLRLIQD-E  REDEFINES  INP-VLRLIQD-X
                                         PIC  ZZZ.ZZZ.ZZ9,99.
BRQ002         10 INP-LIBMID             PIC  X(003)    VALUE SPACES.
               10 INP-GARANTI            PIC  X(020)    VALUE SPACES.
               10 INP-AUTRZTE            PIC  X(050)    VALUE SPACES.
               10 INP-DESCR1             PIC  X(053)    VALUE SPACES.
               10 INP-DESCR2             PIC  X(075)    VALUE SPACES.
               10 INP-JURMAXI.
                  15 INP-JURMAXI-X       PIC  X(006).
                  15 INP-JURMAXI-E  REDEFINES  INP-JURMAXI-X
                                         PIC  999,99.
               10 INP-VLRMAXI.
                  15 INP-VLRMAXI-X       PIC  X(017).
                  15 INP-VLRMAXI-E  REDEFINES  INP-VLRMAXI-X
                                         PIC  ZZ.ZZZ.ZZZ.ZZ9,99.
               10 INP-JRLIBER.
                  15 INP-JRLIBER-X       PIC  X(006).
                  15 INP-JRLIBER-E  REDEFINES  INP-JRLIBER-X
                                         PIC  999,99.
               10 INP-VLLIBER.
                  15 INP-VLLIBER-X       PIC  X(017).
                  15 INP-VLLIBER-E  REDEFINES  INP-VLLIBER-X
                                         PIC  ZZ.ZZZ.ZZZ.ZZ9,99.
               10 INP-TIPOLIB            PIC  X(001)    VALUE SPACES.
           05  INP-MENSAG                PIC  X(079)    VALUE SPACES.
           05  INP-LITPF9A               PIC  X(002)    VALUE SPACES.
           05  INP-LITPF9B               PIC  X(007)    VALUE SPACES.
           05  INP-LITPF11A              PIC  X(003)    VALUE SPACES.
           05  INP-LITPF11B              PIC  X(017)    VALUE SPACES.

      *----------------------------------------------------------------*
      *        *** AREA DA TELA DCOMDETM  -  OUTPUT ***                *
      *----------------------------------------------------------------*

       01  OUTPUT-DETM.
           05  OUT-LL                    PIC S9(004) COMP VALUE +826.
           05  OUT-ZZ                    PIC S9(004) COMP VALUE +0.
           05  OUT-SENHAS                PIC  X(037)      VALUE SPACES.
           05  OUT-COMANDO               PIC  X(068)      VALUE SPACES.
           05  OUT-DADOS.
               10 OUT-TITULO             PIC  X(008)    VALUE SPACES.
               10 OUT-FASE               PIC  X(001)    VALUE SPACES.
               10 OUT-FLAGENT            PIC  X(001)    VALUE SPACES.
               10 OUT-TIPODES            PIC  9(003)    VALUE ZEROS.
               10 OUT-TRANSAC            PIC  X(008)    VALUE SPACES.
               10 OUT-TMSTAMP            PIC  X(026)    VALUE SPACES.
               10 OUT-NOME               PIC  X(040)    VALUE SPACES.
BRQ001         10 WRK-OUT-CANAL          PIC  X(010)    VALUE SPACES.
               10 OUT-CNPJCPF            PIC  X(019)    VALUE SPACES.
               10 OUT-CAMPO2             PIC  X(023)    VALUE SPACES.
               10 OUT-PRODUTO            PIC  9(003)    VALUE ZEROS.
               10 OUT-DPRODUT            PIC  X(012)    VALUE SPACES.
               10 OUT-SPRODUT            PIC  9(003)    VALUE ZEROS.
               10 OUT-DSPRODU            PIC  X(012)    VALUE SPACES.
               10 OUT-OPERAC             PIC  9(013)    VALUE ZEROS.
               10 OUT-SITUAC             PIC  X(015)    VALUE SPACES.
               10 OUT-CAMPO3             PIC  X(013)    VALUE SPACES.
               10 OUT-CAMPO4-X.
                  15 OUT-CAMPO4          PIC  9(009)    VALUE ZEROS.
               10 OUT-DTOPER             PIC  X(010)    VALUE SPACES.
               10 OUT-DTVCTOF            PIC  X(010)    VALUE SPACES.
               10 OUT-VLROPER.
                  15 OUT-VLROPER-N       PIC   ZZZ.ZZZ.ZZ9,99.
               10 OUT-TPOTAXA            PIC  X(015)    VALUE SPACES.
               10 OUT-VLTAROP.
                  15 OUT-VLTAROP-N       PIC   ZZZ.ZZZ.ZZ9,99.
               10 OUT-TAXAJUR.
                  15 OUT-TAXAJUR-N       PIC  ZZ9,99999.
               10 OUT-NMJUROS            PIC  X(004)    VALUE SPACES.
               10 OUT-VLTRFTT.
                  15 OUT-VLTRFTT-N       PIC  ZZZ.ZZZ.ZZ9,99.
               10 OUT-VLRJUR.
                  15 OUT-VLRJUR-N        PIC  ZZZ.ZZZ.ZZ9,99.
               10 OUT-PRZMED             PIC  9(003)    VALUE ZEROS.
               10 OUT-VLRIOF.
                  15 OUT-VLRIOF-N        PIC  ZZZ.ZZZ.ZZ9,99.
               10 OUT-QTDETIT.
                  15 OUT-QTDETIT-N       PIC  ZZ.ZZ9.
               10 OUT-VLRLIQD.
                  15 OUT-VLRLIQD-N       PIC  ZZZ.ZZZ.ZZ9,99.
BRQ002         10 OUT-LIBMID             PIC  X(003)    VALUE SPACES.
               10 OUT-GARANTI            PIC  X(020)    VALUE SPACES.
               10 OUT-AUTRZTE            PIC  X(050)    VALUE SPACES.
               10 OUT-DESCR1             PIC  X(053)    VALUE SPACES.
               10 OUT-DESCR2             PIC  X(075)    VALUE SPACES.
               10 OUT-JURMAXI.
                  15 OUT-JURMAXI-N       PIC  ZZ9,99.
               10 OUT-VLRMAXI.
                  15 OUT-VLRMAXI-N       PIC  ZZ.ZZZ.ZZZ.ZZ9,99.
               10 OUT-ATTR-JRLIBER       PIC S9(004) COMP VALUE ZEROS.
               10 OUT-JRLIBER.
                  15 OUT-JRLIBER-N       PIC  ZZ9,99.
               10 OUT-ATTR-VLLIBER       PIC S9(004) COMP VALUE ZEROS.
               10 OUT-VLLIBER.
                  15 OUT-VLLIBER-N       PIC  ZZ.ZZZ.ZZZ.ZZ9,99.
               10 OUT-TIPOLIB            PIC  X(001)    VALUE SPACES.
           05  OUT-MENSAG                PIC  X(079)    VALUE SPACES.
           05  OUT-LITPF9A               PIC  X(002)    VALUE SPACES.
           05  OUT-LITPF9B               PIC  X(007)    VALUE SPACES.
           05  OUT-LITPF11A              PIC  X(003)    VALUE SPACES.
           05  OUT-LITPF11B              PIC  X(017)    VALUE SPACES.

      /---------------------------------------------------------------*
      *  AREA DA BRAD0660 COM MASCARA DA TELA DCOMDETM - SEGMENTO 00  *
      *---------------------------------------------------------------*

       01  660-DCOMDETM.
           05  660-LL-AREA-DETM        PIC  9(04)  COMP  VALUE 0100.
           05  660-LL-MENSAGEM-DETM    PIC  9(04)  COMP  VALUE 0829.
           05  660-SENHAS-DETM         PIC  9(04)  COMP  VALUE 0037.
           05  660-COMANDO-DETM        PIC  9(04)  COMP  VALUE 0068.
           05  660-TITULO-DETM         PIC  9(04)  COMP  VALUE 0008.
           05  660-FASE-DETM           PIC  9(04)  COMP  VALUE 0001.
           05  660-FLAGENT-DETM        PIC  9(04)  COMP  VALUE 0001.
           05  660-TIPODES-DETM        PIC  9(04)  COMP  VALUE 0003.
           05  660-TRANSAC-DETM        PIC  9(04)  COMP  VALUE 0008.
           05  660-TMSTAMP-DETM        PIC  9(04)  COMP  VALUE 0026.
           05  660-NOME-DETM           PIC  9(04)  COMP  VALUE 0040.
BRQ001     05  WRK-660-CANAL-DETM      PIC  9(04)  COMP  VALUE 0010.
           05  660-CNPJCPF-DETM        PIC  9(04)  COMP  VALUE 0019.
           05  660-CAMPO2-DETM         PIC  9(04)  COMP  VALUE 0023.
           05  660-PRODUTO-DETM        PIC  9(04)  COMP  VALUE 0003.
           05  660-DPRODUT-DETM        PIC  9(04)  COMP  VALUE 0012.
           05  660-SPRODUT-DETM        PIC  9(04)  COMP  VALUE 0003.
           05  660-DSPRODU-DETM        PIC  9(04)  COMP  VALUE 0012.
           05  660-OPERAC-DETM         PIC  9(04)  COMP  VALUE 0013.
           05  660-SITUAC-DETM         PIC  9(04)  COMP  VALUE 0015.
           05  660-CAMPO3-DETM         PIC  9(04)  COMP  VALUE 0013.
           05  660-CAMPO4-DETM         PIC  9(04)  COMP  VALUE 0009.
           05  660-DTOPER-DETM         PIC  9(04)  COMP  VALUE 0010.
           05  660-DTVCTOF-DETM        PIC  9(04)  COMP  VALUE 0010.
           05  660-VLROPER-DETM        PIC  9(04)  COMP  VALUE 0014.
           05  660-TPOTAXA-DETM        PIC  9(04)  COMP  VALUE 0015.
           05  660-VLTAROP-DETM        PIC  9(04)  COMP  VALUE 0014.
           05  660-TAXAJUR-DETM        PIC  9(04)  COMP  VALUE 0009.
           05  660-NMJUROS-DETM        PIC  9(04)  COMP  VALUE 0004.
           05  660-VLTRFTT-DETM        PIC  9(04)  COMP  VALUE 0014.
           05  660-VLRJUR-DETM         PIC  9(04)  COMP  VALUE 0014.
           05  660-PRZMED-DETM         PIC  9(04)  COMP  VALUE 0003.
           05  660-VLRIOF-DETM         PIC  9(04)  COMP  VALUE 0014.
           05  660-QTDETIT-DETM        PIC  9(04)  COMP  VALUE 0006.
           05  660-VLRLIQD-DETM        PIC  9(04)  COMP  VALUE 0014.
BRQ002     05  660-LIBMID-DETM         PIC  9(04)  COMP  VALUE 0003.
           05  660-GARANTI-DETM        PIC  9(04)  COMP  VALUE 0020.
           05  660-AUTRZTE-DETM        PIC  9(04)  COMP  VALUE 0050.
           05  660-DESCR1-DETM         PIC  9(04)  COMP  VALUE 0053.
           05  660-DESCR2-DETM         PIC  9(04)  COMP  VALUE 0075.
           05  660-JURMAXI-DETM        PIC  9(04)  COMP  VALUE 0006.
           05  660-VLRMAXI-DETM        PIC  9(04)  COMP  VALUE 0017.
           05  660-JRLIBER-DETM        PIC  9(04)  COMP  VALUE 2008.
           05  660-VLLIBER-DETM        PIC  9(04)  COMP  VALUE 2019.
           05  660-TIPOLIB-DETM        PIC  9(04)  COMP  VALUE 0001.
           05  660-MENSA-DETM          PIC  9(04)  COMP  VALUE 0079.
           05  660-LITPF9A-DETM        PIC  9(04)  COMP  VALUE 0002.
           05  660-LITPF9B-DETM        PIC  9(04)  COMP  VALUE 0007.
           05  660-LITPF11A-DETM       PIC  9(04)  COMP  VALUE 0003.
           05  660-LITPF11B-DETM       PIC  9(04)  COMP  VALUE 0017.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DE COMUNICACAO DCOM6425 ***'.
      *----------------------------------------------------------------*

       01  ENTRADA-6425-ROTEADOR.
           05  ROTENT-6425-COD-RETORNO      PIC  X(004)   VALUE SPACES.
           05  ROTENT-6425-MSG-RETORNO      PIC  X(079)   VALUE SPACES.
           05  ROTENT-6425-RESTART          PIC  9(005)   VALUE ZEROS.
           05  ROTENT-6425-FLAG             PIC  X(001)   VALUE SPACES.
           05  ROTENT-6425-START-COUNT      PIC  X(001)   VALUE SPACES.
           05  ROTENT-6425-CFUNC-BDSCO      PIC  9(009)   VALUE ZEROS.
           05  ROTENT-6425-CTERM            PIC  X(008)   VALUE SPACES.
           05  ROTENT-6425-CGARAN           PIC  9(003)   VALUE ZEROS.

       01  SAIDA-6425-ROTEADOR.
           05  ROTSAI-6425-COD-RETORNO    PIC  X(004)     VALUE SPACES.
           05  ROTSAI-6425-MSG-RETORNO    PIC  X(079)     VALUE SPACES.
           05  ROTSAI-6425-RESTART        PIC  9(005)     VALUE ZEROS.
           05  ROTSAI-6425-FLAG           PIC  X(001)     VALUE SPACES.
           05  ROTSAI-6425-QTDE-RETORNADA PIC  9(003)     VALUE ZEROS.
           05  ROTSAI-6425-COUNT          PIC  9(009)     VALUE ZEROS.
           05  ROTSAI-6425-CGARAN         PIC  9(003)     VALUE ZEROS.
           05  ROTSAI-6425-IGARAN         PIC  X(040)     VALUE SPACES.

BRQ002*----------------------------------------------------------------*
BRQ002 01  FILLER                      PIC  X(050)         VALUE
BRQ002     '*** AREA DO MODULO DCOM5490 ***'.
BRQ002*----------------------------------------------------------------*
BRQ002
BRQ002 01  WRK-5490-ENTRADA.
BRQ002     05  WRK-E5490-COD-RETORNO      PIC  X(004)      VALUE SPACES.
BRQ002     05  WRK-E5490-MSG-RETORNO      PIC  X(079)      VALUE SPACES.
BRQ002     05  WRK-E5490-RESTART          PIC  9(005)      VALUE ZEROS.
BRQ002     05  WRK-E5490-FLAG             PIC  X(001)      VALUE SPACES.
BRQ002     05  WRK-E5490-START-COUNT      PIC  X(001)      VALUE SPACES.
BRQ002     05  WRK-E5490-CFUNC-BDSCO      PIC  9(009)      VALUE ZEROS.
BRQ002     05  WRK-E5490-CTERM            PIC  X(008)      VALUE SPACES.
BRQ002     05  WRK-E5490-CPRODT           PIC  9(003)      VALUE ZEROS.
BRQ002     05  WRK-E5490-CSPROD-DESC-COML PIC  9(003)      VALUE ZEROS.
BRQ002     05  WRK-E5490-CTPO-DESC-COML   PIC  9(003)      VALUE ZEROS.
BRQ002     05  WRK-E5490-NRO-OPERACAO.
BRQ002         10  WRK-E5490-DANO-OPER    PIC  9(004)      VALUE ZEROS.
BRQ002         10  WRK-E5490-NSEQ-OPER    PIC  9(009)      VALUE ZEROS.
BRQ002     05  WRK-E5490-VOPER-DESC-COML  PIC  9(015)V99   VALUE ZEROS.
BRQ002     05  WRK-E5490-FUNCAO           PIC  X(002)      VALUE SPACES.
BRQ002     05  WRK-E5490-LIBRC-MIDIA      PIC  X(001)      VALUE SPACES.
BRQ002
BRQ002 01  WRK-5490-SAIDA.
BRQ002     05  WRK-S5490-COD-RETORNO      PIC  9(004)      VALUE ZEROS.
BRQ002     05  WRK-S5490-MSG-RETORNO      PIC  X(079)      VALUE SPACES.
BRQ002     05  WRK-S5490-RESTART          PIC  9(005)      VALUE ZEROS.
BRQ002     05  WRK-S5490-FLAG             PIC  X(001)      VALUE SPACES.
BRQ002     05  WRK-S5490-LIBRC-MIDIA      PIC  X(001)      VALUE SPACES.
BRQ002     05  WRK-S5490-CADASTRO-BROB    PIC  X(001)      VALUE SPACES.

DTS001*----------------------------------------------------------------*
DTS001 01  FILLER                      PIC  X(50)          VALUE
DTS001     '*** AREA DO MODULO DCOM5505 ***'.
DTS001*----------------------------------------------------------------*
DTS001
DTS001 01  WRK-5505E-ENTRADA.
DTS001     05 WRK-5505E-COD-RETORNO    PIC  X(004)         VALUE SPACES.
DTS001     05 WRK-5505E-MSG-RETORNO    PIC  X(079)         VALUE SPACES.
DTS001     05 WRK-5505E-RESTART        PIC  9(005)         VALUE ZEROS.
DTS001     05 WRK-5505E-FLAG           PIC  X(001)         VALUE SPACES.
DTS001     05 WRK-5505E-CFUNC-BDSCO    PIC  9(009)         VALUE ZEROS.
DTS001     05 WRK-5505E-CTERM          PIC  X(008)         VALUE SPACES.
DTS001     05 WRK-5505E-DANO-OPER-DESC PIC  9(004)         VALUE ZEROS.
DTS001     05 WRK-5505E-NSEQ-OPER-DESC PIC  9(009)         VALUE ZEROS.
DTS001     05 WRK-5505E-CEVNTO-DESC-COML
DTS001                                 PIC  9(003)         VALUE ZEROS.
DTS001
DTS001 01  WRK-5505E-RETORNO.
DTS001     05 WRK-5505S-COD-RETORNO    PIC  9(004)         VALUE ZEROS.
DTS001     05 WRK-5505S-MSG-RETORNO    PIC  X(079)         VALUE SPACES.
DTS001     05 WRK-5505S-RESTART        PIC  9(005)         VALUE ZEROS.
DTS001     05 WRK-5505S-FLAG           PIC  X(001)         VALUE SPACES.
DTS001     05 WRK-5505S-CSIT-INIC-DESC PIC  9(003)         VALUE ZEROS.
DTS001     05 WRK-5505S-CSIT-FNAL-DESC PIC  9(003)         VALUE ZEROS.
DTS001     05 WRK-5505S-TAMANHO-ERRO   PIC  9(001)         VALUE ZEROS.
DTS001     05 WRK-5505S-TABELA-ERROS   OCCURS 012 TIMES.
DTS001       10 WRK-5505S-CAMPOS-ERRO  PIC  9(002)         VALUE ZEROS.

      *---------------------------------------------------------------*
      *    AREA DE COMUNICACAO CHAMADO DCOM0402/DCOM0403/DCOM0404     *
      *---------------------------------------------------------------*

       01  COMU-AREA-UN.
           05  COMU-LL-UN              PIC S9(004) COMP  VALUE +297.
           05  COMU-ZZ-UN              PIC S9(004) COMP  VALUE +0.
           05  COMU-TRANCODE-UN.
               10  COMU-TRANSACAO-UN   PIC  X(008)       VALUE SPACES.
               10  FILLER-UN           PIC  X(006)       VALUE SPACES.
               10  COMU-PFK-UN         PIC  X(001)       VALUE SPACES.
           05  COMU-SENHAS-UN          PIC  X(037)       VALUE SPACES.
           05  COMU-COMANDO-UN         PIC  X(068)       VALUE SPACES.
           05  COMU-DADOS-UN.
               10  COMU-OPCAO-UN-X.
                   15  COMU-OPCAO-UN   PIC  9(001)       VALUE ZEROS.
               10  COMU-OPER-UN        PIC  9(013)       VALUE ZEROS.
               10  COMU-AGENCIA-UN     PIC  9(005)       VALUE ZEROS.
               10  COMU-AGENC-UN-X.
                   15  COMU-AGENC-UN   PIC  9(005)       VALUE ZEROS.
               10  COMU-CONTA-UN       PIC  9(013)       VALUE ZEROS.
               10  COMU-CNPJ-UN-X.
      *            15  COMU-CNPJ-UN    PIC  9(009)       VALUE ZEROS.
FS2511             15  COMU-CNPJ-UN    PIC  X(009)       VALUE SPACES.
      *        10  COMU-FILIAL-UN      PIC  9(004)       VALUE ZEROS.
FS2511         10  COMU-FILIAL-UN      PIC  X(004)       VALUE SPACES.
               10  COMU-DIG-UN         PIC  9(002)       VALUE ZEROS.
               10  COMU-DANO-OPER-UN   PIC  9(004)       VALUE ZEROS.
               10  COMU-NSEQ-OPER-UN   PIC  9(009)       VALUE ZEROS.
               10  COMU-TIMESTAMP-UN   PIC  X(026)       VALUE ZEROS.
               10  COMU-MENSAGEM-UN    PIC  X(079)       VALUE SPACES.
               10  COMU-PAGINA-UN      PIC  9(003)       VALUE ZEROS.
PROC01         10  COMU-CHAMADOR-UN    PIC  X(008)       VALUE SPACES.

      *----------------------------------------------------------------*
      *        *** AREA DE COMUNICACAO 214 ***                         *
      *----------------------------------------------------------------*
       01  COMU-AREA-0214.
           05  COMU-LL-0214               PIC S9(004) COMP VALUE ZEROS.
           05  COMU-ZZ-0214               PIC S9(004) COMP VALUE ZEROS.
           05  COMU-TRANCODE-0214.
               10  COMU-TRANSACAO-0214      PIC  X(008)   VALUE SPACES.
               10  FILLER                   PIC  X(006)   VALUE SPACES.
               10  COMU-PFK-0214            PIC  X(001)   VALUE SPACES.
           05  COMU-SENHAS-0214             PIC  X(037)   VALUE SPACES.
           05  COMU-COMANDO-0214            PIC  X(068)   VALUE SPACES.
           05  COMU-DADOS-0214.
               10  COMU-TIPO-OPERACAO-0214  PIC  9(01)    VALUE ZEROS.
DTS003         10  COMU-TPDESC-0214         PIC  9(03)    VALUE ZEROS.
               10  COMU-AGENCIA-0214        PIC  9(05)    VALUE ZEROS.
               10  COMU-CONTA-0214          PIC  9(13)    VALUE ZEROS.
      *        10  COMU-CPFCNPJ-0214        PIC  9(09)    VALUE ZEROS.
FS2511         10  COMU-CPFCNPJ-0214        PIC  X(09)    VALUE SPACES.
      *        10  COMU-FILIAL-0214         PIC  9(05)    VALUE ZEROS.
FS2511         10  COMU-FILIAL-0214         PIC  X(04)    VALUE SPACES.
               10  COMU-CONTROLE-0214       PIC  9(02)    VALUE ZEROS.
               10  COMU-OPERACAO-0214       PIC  9(013)     VALUE ZEROS.
               10  FILLER REDEFINES         COMU-OPERACAO-0214.
                   15 COMU-DANO-0214        PIC  9(004).
                   15 COMU-NSEQ-0214        PIC  9(009).
               10  COMU-PAGINA-OPER-0214    PIC  9(05)    VALUE ZEROS.
               10  COMU-TIMESTAMP-0214      PIC  X(26)    VALUE SPACES.
               10  COMU-MENSAGEM-0214       PIC  X(79)    VALUE SPACES.
               10  COMU-TRANSA-ANTE-0214    PIC  X(08)    VALUE SPACES.
               10  COMU-PAGINA-ANTE-0214    PIC  9(05)    VALUE ZEROS.
DTS003         10  COMU-NRTITLO-0214        PIC  9(05)    VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(50)      VALUE
           '*** AREA DE COMUNICACAO COM DCOM0222 ***'.
      *----------------------------------------------------------------*

       01  COMU-AREA-0215.
           05  COMU-LL-0215            PIC S9(004) COMP    VALUE ZEROS.
           05  COMU-ZZ-0215            PIC S9(004) COMP    VALUE ZEROS.
           05  COMU-TRANCODE-0215.
               10  COMU-TRANSACAO-0215 PIC  X(008)        VALUE SPACES.
               10  FILLER              PIC  X(006)        VALUE SPACES.
               10  COMU-PFK-0215       PIC  X(001)        VALUE SPACES.
           05  COMU-SENHAS-0215        PIC  X(037)        VALUE SPACES.
           05  COMU-COMANDO-0215       PIC  X(068)        VALUE SPACES.
           05  COMU-DADOS-0215.
               10  COMU-TPOPCAO-0215   PIC  X(001)        VALUE SPACES.
               10  COMU-TIMESTAMP-0215 PIC  X(026)        VALUE SPACES.
               10  COMU-TPDESC-0215    PIC  9(003)        VALUE ZEROS.
               10  COMU-SALVOS-0215.
      *            15  COMU-CNPJCPF-0215   PIC  9(009)     VALUE ZEROS.
FS2511             15  COMU-CNPJCPF-0215   PIC  X(009)     VALUE SPACES.
      *            15  COMU-FILIAL-0215    PIC  9(004)     VALUE ZEROS.
FS2511             15  COMU-FILIAL-0215    PIC  X(004)     VALUE SPACES.
                   15  COMU-CONTROLE-0215  PIC  9(002)     VALUE ZEROS.
                   15  COMU-AGENCIA-0215   PIC  9(005)     VALUE ZEROS.
                   15  COMU-CONTA-0215     PIC  9(013)     VALUE ZEROS.
                   15  COMU-OPERACAO-0215  PIC  9(013)     VALUE ZEROS.
                   15  FILLER REDEFINES    COMU-OPERACAO-0215.
                       20 COMU-DANO-0215   PIC  9(004).
                       20 COMU-NSEQ-0215   PIC  9(009).
                   15  COMU-BORDERO-0215   PIC  9(007)     VALUE ZEROS.
                   15  COMU-LOTE-0215      PIC  9(007)     VALUE ZEROS.
                   15  COMU-CHEQUE-0215    PIC  9(007)     VALUE ZEROS.
                   15  COMU-COMUNICACAO-0215.
                       20  COMU-PAGINA1-0215   PIC  9(004) VALUE ZEROS.
                       20  COMU-TRANSA1-0215  PIC  X(008) VALUE SPACES.
                       20  COMU-PAGINA2-0215   PIC  9(004) VALUE ZEROS.
                       20  COMU-TRANSA2-0215  PIC  X(008) VALUE SPACES.
                       20  COMU-PAGINA3-0215   PIC  9(004) VALUE ZEROS.
                       20  COMU-TRANSA3-0215  PIC  X(008) VALUE SPACES.
                       20  COMU-PAGINA4-0215   PIC  9(004) VALUE ZEROS.
                       20  COMU-TRANSA4-0215  PIC  X(008) VALUE SPACES.
                   15  COMU-COD204-0215    PIC  X(004)    VALUE SPACES.
               10  COMU-MENSAGEM-0215  PIC  X(079)        VALUE SPACES.

      *----------------------------------------------------------------*
      *        *** AREA DE COMUNICACAO PFK ***                         *
      *----------------------------------------------------------------*

       01  COMU-AREA-PFK.
           05  COMU-LL-PFK             PIC S9(004) COMP    VALUE +124.
           05  COMU-ZZ-PFK             PIC S9(004) COMP    VALUE ZEROS.
           05  COMU-TRANCODE.
               10  COMU-TRANSACAO-PFK  PIC  X(008)         VALUE SPACES.
               10  FILLER              PIC  X(006)         VALUE SPACES.
               10  COMU-PFK1           PIC  X(001)         VALUE SPACES.
           05  COMU-SENHAS-PFK         PIC  X(037)         VALUE SPACES.
           05  COMU-COMANDO-PFK        PIC  X(068)         VALUE SPACES.

      *----------------------------------------------------------------*
      *        *** AREA DA BRAD7100 ***                                *
      *----------------------------------------------------------------*

       COPY  'I#BRAD7C'.

       COPY  'I#DCOMXE'.

      *----------------------------------------------------------------*
      *--- AREA DE ENTRADA DO ROTEADOR - DCOM4161 - TAM. 0126 BYTES
      *--- OBTER DADOS BASICOS      ---*
      *----------------------------------------------------------------*

       COPY 'I#DCOMXJ'.

CAP001*----------------------------------------------------------------*
|      01  FILLER                      PIC  X(050)         VALUE
|          '*** AREA DO BVVEA501 ***'.
|     *----------------------------------------------------------------*
|
|      COPY 'I#BVVENA'.
|
|     *----------------------------------------------------------------*
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
|          '*** AREA DO BVVEA505 ***'.
|     *----------------------------------------------------------------*
|
|      COPY 'I#BVVENE'.
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
      *---           AREA DE ACESSO AO MODULO DCOM5110              ---*
      *----------------------------------------------------------------*

       01  5110E-ROTEADOR.
           03  5110E-COD-RETORNO      PIC  X(004).
           03  5110E-MSG-RETORNO      PIC  X(079).
           03  5110E-RESTART          PIC  9(005).
           03  5110E-FLAG             PIC  X(001).
           03  5110E-CFUNC-BDSCO      PIC  9(009).
           03  5110E-CTERM            PIC  X(008).
           03  5110E-DANO-OPER-DESC   PIC  9(004).
           03  5110E-NSEQ-OPER-DESC   PIC  9(009).
           03  5110E-TPO-LIBRC        PIC  X(001).
           03  5110E-VLIBRC           PIC  9(015)V9(02).

       01  5110S-ROTEADOR.
           03  5110S-COD-RETORNO      PIC  X(004).
           03  5110S-MSG-RETORNO      PIC  X(079).
           03  5110S-RESTART          PIC  9(005).
           03  5110S-FLAG             PIC  9(001).
           03  5110S-CONSISTENCIA.
               05  5110S-TAMANHO-ERRO PIC  9(001).
               05  5110S-TABELA-ERROS OCCURS  4 TIMES.
                   07  5110S-CAMPOS-ERRO
                                       PIC 9(001).

      *----------------------------------------------------------------*
      *---           AREA DE ACESSO AO MODULO DCOM5113              ---*
      *----------------------------------------------------------------*

       01  5113E-ROTEADOR.
           03  5113E-COD-RETORNO      PIC  X(004).
           03  5113E-MSG-RETORNO      PIC  X(079).
           03  5113E-RESTART          PIC  9(005).
           03  5113E-FLAG             PIC  X(001).
           03  5113E-CFUNC-BDSCO      PIC  9(009).
           03  5113E-CTERM            PIC  X(008).
           03  5113E-DANO-OPER-DESC   PIC  9(004).
           03  5113E-NSEQ-OPER-DESC   PIC  9(009).
      *    03  5113E-CPFCNPJ          PIC  9(009).
FS2511     03  5113E-CPFCNPJ          PIC  X(009).
           03  5113E-CONTROLE         PIC  9(004).
      *    03  5113E-FILIAL           PIC  9(002).
FS2511     03  5113E-FILIAL           PIC  X(004).

       01  5113S-ROTEADOR.
           03  5113S-COD-RETORNO      PIC  X(004).
           03  5113S-MSG-RETORNO      PIC  X(079).
           03  5113S-RESTART          PIC  9(005).
           03  5113S-FLAG             PIC  9(001).
           03  5113S-CONSISTENCIA.
               05  5113S-TAMANHO-ERRO PIC  9(001).
               05  5113S-TABELA-ERROS OCCURS  4 TIMES.
                   07  5113S-CAMPOS-ERRO
                                       PIC 9(001).

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
      *---           AREA DE ENTRADA DO ROTEADOR - DCOM4132         ---*
      *----------------------------------------------------------------*

       01  4132E-ROTEADOR.
           05  4132E-COD-RETORNO                 PIC  X(004).
           05  4132E-MSG-RETORNO                 PIC  X(079).
           05  4132E-RESTART                     PIC  9(005).
           05  4132E-FLAG                        PIC  X(001).
           05  4132E-CFUNC-BDSCO                 PIC  9(009).
           05  4132E-CTERM                       PIC  X(008).
           05  4132E-DANO-OPER-DESC              PIC  9(004).
           05  4132E-NSEQ-OPER-DESC              PIC  9(009).
           05  4132E-PERC-INFORMADO              PIC  9(003)V9(03).
           05  4132E-VALOR-INFORMADO             PIC  9(015)V9(02).

       01  4132S-ROTEADOR.
           05  4132S-COD-RETORNO                 PIC  X(004).
           05  4132S-MSG-RETORNO                 PIC  X(079).
           05  4132S-RESTART                     PIC  9(005).
           05  4132S-FLAG                        PIC  X(001).
           05  4132S-DADOS.
             10  4132S-PLIBRC-ANTCP-OPER         PIC  9(003)V9(03).
             10  4132S-VLIBRC-ANTCP-OPER         PIC  9(015)V9(02).

           05  4132S-CONSISTENCIA     REDEFINES  4132S-DADOS.
             10  4132S-TAMANHO-ERRO              PIC  9(001).
             10  4132S-TABELA-ERROS              OCCURS 2 TIMES.
               15  4132S-CAMPOS-ERRO             PIC  9(001).

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
      *        *** DCOM0405 - FIM DA AREA DE WORKING ***               *
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  LNK-IO-PCB.
           05  LNK-IO-LTERM            PIC  X(008).
           05  FILLER                  PIC  X(002).
           05  LNK-IO-STATUS           PIC  X(002).
           05  FILLER                  PIC  X(012).
           05  LNK-IO-MODNAME          PIC  X(008).

       01  LNK-ALT-PCB.
           05  LNK-ALT-LTERM           PIC  X(008).
           05  FILLER                  PIC  X(002).
           05  LNK-ALT-STATUS          PIC  X(002).
           05  FILLER                  PIC  X(012).
           05  LNK-ALT-MODNAME         PIC  X(008).

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

           ENTRY  'DLITCBL'            USING  LNK-IO-PCB
                                              LNK-ALT-PCB.

           MOVE  WRK-GU                TO     WRK-FUNCAO.

      *----------------------------------------------------------------*
      *        PROCEDIMENTOS INICIAIS                                  *
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           CALL  'POOL5005'            USING  WRK-VERSAO
                                              WRK-FUNCAO
                                              LNK-IO-PCB
                                              LNK-ALT-PCB
                                              WRK-MENSAGEM
                                              WRK-TELA
                                              WRK-COD-USER
                                              WRK-COD-DEPTO.

           IF RETURN-CODE    EQUAL  04  OR  12
              GOBACK
           END-IF.

           MOVE SPACES          TO  OUT-DADOS.

           PERFORM  1000-PROCESSAR-MENSAGEM.

           GO   TO  0000-INICIAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-PROCESSAR-MENSAGEM         SECTION.
      *----------------------------------------------------------------*

           MOVE  'DCOMDETM'             TO WRK-TELA.

           IF WRK-TRANSACAO            NOT EQUAL  'DCOM0405'
              PERFORM  1100-RECEBER-CONTROLE
           ELSE
              PERFORM  2000-PROCESSAR-DCOMDETM
           END-IF.

           IF WRK-TELA                 EQUAL  'DCOMDETM'
              PERFORM  3000-PROCESSAR-BRAD0660
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1100-RECEBER-CONTROLE           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE COMU-AREA-UN
                      COMU-AREA-0214
                      4174-ENTRADA-ROTEADOR
                      4174-SAIDA-ROTEADOR
                      4131E-ROTEADOR
                      4131S-ROTEADOR.

           IF  WRK-TRANSACAO       NOT EQUAL 'DCOM0214' AND 'DCOM0222'
               PERFORM 1101-RECEBER-CONTROLE-1
           ELSE
               IF  WRK-TRANSACAO       EQUAL 'DCOM0214'
                   PERFORM 1102-RECEBER-CONTROLE-2
               ELSE
                   PERFORM 1103-RECEBER-CONTROLE-3
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1101-RECEBER-CONTROLE-1         SECTION.
      *----------------------------------------------------------------*

           INITIALIZE ROTENT-6437-ENVIO
                      ROTSAI-6437-RETORNO
                      ERRO-AREA.

PROC01     MOVE WRK-MENSAGEM           TO COMU-AREA-UN
PROC01
PROC01     IF  WRK-TRANSACAO           EQUAL 'DCOM3924'
PROC01         MOVE COMU-CHAMADOR-UN   TO INP-TRANSAC
PROC01                                    OUT-TRANSAC
PROC01     ELSE
PROC01         MOVE WRK-TRANSACAO      TO INP-TRANSAC
PROC01                                    OUT-TRANSAC
PROC01     END-IF.
PROC01
           MOVE COMU-TIMESTAMP-UN      TO INP-TMSTAMP
                                          OUT-TMSTAMP
           MOVE COMU-SENHAS-UN         TO INP-SENHAS
                                          OUT-SENHAS

           IF COMU-OPCAO-UN-X          EQUAL 'C'
               MOVE 'CANCELAR'         TO INP-TITULO
                                          OUT-TITULO
           ELSE
               MOVE ' LIBERAR'         TO INP-TITULO
                                          OUT-TITULO
           END-IF
           MOVE COMU-DANO-OPER-UN      TO 4174-ENT-DANO-OPER-DESC
                                          4131E-DANO-OPER-DESC
                                          INP-OPERAC(1:4)
           MOVE COMU-NSEQ-OPER-UN      TO 4174-ENT-NSEQ-OPER-DESC
                                          4131E-NSEQ-OPER-DESC
                                          INP-OPERAC(5:9)

           PERFORM 1104-GRAVAR-DBTEMP.

           MOVE SPACES                 TO OUT-CAMPO3
                                          OUT-CAMPO4-X.

           MOVE WRK-49361              TO OUT-ATTR-JRLIBER

           PERFORM 1110-ACESSAR-DCOM4174.

           IF  4174-SAI-COD-RETORNO    EQUAL '0000' AND
               WRK-INCONSIS            EQUAL SPACES
               PERFORM 1120-ACESSAR-DCOM4131
               IF  WRK-FLAG-ERRO       EQUAL 'S'
                   GO TO  1101-99-FIM
               END-IF
BRQ002         MOVE 'VL'               TO WRK-FUNCAO-5490
BRQ002         PERFORM 1113-OBTER-INFO-LIBRC-MIDIA
BRQ002         IF  WRK-FLAG-ERRO       EQUAL 'S'
BRQ002             GO TO  1101-99-FIM
BRQ002         END-IF
               IF  COMU-OPCAO-UN-X     EQUAL 'C'
                   MOVE '2'            TO OUT-FASE
                   MOVE 'TECLE <PF6> PARA CONFIRMAR CANCELAMENTO DA OPER
      -                 'ACAO'         TO OUT-MENSAG
                   MOVE WRK-241        TO OUT-ATTR-JRLIBER
                                          OUT-ATTR-VLLIBER
               ELSE
                   IF 4131S-PMAX-LIBRC-ANTCP EQUAL 100 AND
                      4131S-TPO-LIBRC        EQUAL 'T'
                       MOVE 100        TO OUT-JRLIBER-N
                       MOVE '2'        TO OUT-FASE
                       MOVE 'TECLE <PF6> PARA CONFIRMAR LIBERACAO'
                                       TO OUT-MENSAG
                       MOVE WRK-241    TO OUT-ATTR-JRLIBER
                                          OUT-ATTR-VLLIBER
                   ELSE
                       MOVE '1'        TO OUT-FASE
                       MOVE 'DIGITE OS DADOS E TECLE ENTER'
                                           TO OUT-MENSAG
                       MOVE WRK-49361  TO OUT-ATTR-JRLIBER
                   END-IF
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1101-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1102-RECEBER-CONTROLE-2         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM           TO COMU-AREA-0214
           MOVE COMU-TIMESTAMP-0214    TO INP-TMSTAMP
                                          OUT-TMSTAMP
           MOVE 'DCOM0402'             TO INP-TRANSAC
                                          OUT-TRANSAC
           MOVE ' '                    TO INP-FLAGENT
                                          OUT-FLAGENT
           MOVE ' LIBERAR'             TO INP-TITULO
                                          OUT-TITULO
           MOVE COMU-DANO-0214         TO 4174-ENT-DANO-OPER-DESC
                                          4131E-DANO-OPER-DESC
                                          INP-OPERAC(1:4)
           MOVE COMU-NSEQ-0214         TO 4174-ENT-NSEQ-OPER-DESC
                                          4131E-NSEQ-OPER-DESC
                                          INP-OPERAC(5:9)

           MOVE SPACES                 TO OUT-CAMPO3
                                          OUT-CAMPO4-X
                                          WRK-INCONSIS.

           MOVE WRK-49361              TO OUT-ATTR-JRLIBER

           PERFORM 1110-ACESSAR-DCOM4174.

           IF  4174-SAI-COD-RETORNO    EQUAL '0000' AND
               WRK-INCONSIS            EQUAL SPACES
               PERFORM 1120-ACESSAR-DCOM4131
               IF 4131S-PMAX-LIBRC-ANTCP EQUAL 100 AND
                  4131S-TPO-LIBRC        EQUAL 'T'
                  MOVE 100             TO OUT-JRLIBER-N
                  MOVE '2'             TO OUT-FASE
                  MOVE 'TECLE <PF6> PARA CONFIRMAR LIBERACAO'
                                       TO OUT-MENSAG
                  MOVE WRK-241         TO OUT-ATTR-JRLIBER
                                          OUT-ATTR-VLLIBER
               ELSE
                  MOVE '1'             TO OUT-FASE
                  MOVE 'DIGITE OS DADOS E TECLE ENTER'
                                       TO OUT-MENSAG
                  MOVE WRK-49361       TO OUT-ATTR-JRLIBER
               END-IF
           END-IF.

           IF  COMU-MENSAGEM-0214       NOT EQUAL SPACES AND LOW-VALUES
               MOVE COMU-MENSAGEM-0214  TO OUT-MENSAG
           END-IF.

      *----------------------------------------------------------------*
       1102-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1103-RECEBER-CONTROLE-3         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM           TO COMU-AREA-0215

           MOVE COMU-TIMESTAMP-0215    TO INP-TMSTAMP
                                          OUT-TMSTAMP
           MOVE 'DCOM0402'             TO INP-TRANSAC
                                          OUT-TRANSAC
           MOVE COMU-SENHAS-0215       TO INP-SENHAS
                                          OUT-SENHAS
           MOVE ' '                    TO INP-FLAGENT
                                          OUT-FLAGENT
           MOVE ' LIBERAR'             TO INP-TITULO
                                          OUT-TITULO
           MOVE COMU-DANO-0215         TO 4174-ENT-DANO-OPER-DESC
                                          4131E-DANO-OPER-DESC
                                          INP-OPERAC(1:4)
           MOVE COMU-NSEQ-0215         TO 4174-ENT-NSEQ-OPER-DESC
                                          4131E-NSEQ-OPER-DESC
                                          INP-OPERAC(5:9)
           MOVE SPACES                 TO OUT-CAMPO3
                                          OUT-CAMPO4-X
                                          WRK-INCONSIS.

           MOVE WRK-49361              TO OUT-ATTR-JRLIBER

           PERFORM 1110-ACESSAR-DCOM4174.

           IF  4174-SAI-COD-RETORNO    EQUAL '0000' AND
               WRK-INCONSIS            EQUAL SPACES
               PERFORM 1120-ACESSAR-DCOM4131
               IF 4131S-PMAX-LIBRC-ANTCP EQUAL 100 AND
                  4131S-TPO-LIBRC        EQUAL 'T'
                   MOVE 100              TO OUT-JRLIBER-N
                   MOVE '2'              TO OUT-FASE
                   MOVE 'TECLE <PF6> PARA CONFIRMAR LIBERACAO'
                                         TO OUT-MENSAG
                   MOVE WRK-241          TO OUT-ATTR-JRLIBER
                                            OUT-ATTR-VLLIBER
               ELSE
                   MOVE '1'        TO OUT-FASE
                   MOVE 'DIGITE OS DADOS E TECLE ENTER'
                                   TO OUT-MENSAG
                   MOVE WRK-49361  TO OUT-ATTR-JRLIBER
               END-IF
           END-IF.

           IF  COMU-MENSAGEM-0215      NOT EQUAL SPACES
               MOVE COMU-MENSAGEM-0215 TO OUT-MENSAG
           END-IF.

      *----------------------------------------------------------------*
       1103-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1104-GRAVAR-DBTEMP              SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-ENTRADA-DCOM7995
                      WRK-SAIDA-DCOM7995.

           MOVE 'I '                   TO WRK-ENT-OPCAO
           MOVE WRK-COD-USER-R         TO WRK-ENT-COD-FUNC
           MOVE LNK-IO-LTERM           TO WRK-ENT-COD-TERMINAL
           MOVE COMU-TIMESTAMP-UN      TO WRK-ENT-TIMESTAMP
           MOVE 'DCOM0405'             TO WRK-ENT-TRANS
           MOVE SPACES                 TO WRK-ENT-TRANS-ANT
           MOVE 1                      TO WRK-ENT-PAGINA
           MOVE COMU-DADOS-UN          TO WRK-DADOS-DBTEMP

PROC01     IF  WRK-TRANSACAO           EQUAL 'DCOM3924'
PROC01         MOVE COMU-CHAMADOR-UN   TO WRK-CHAMADOR-DBTEMP
PROC01     ELSE
PROC01         MOVE WRK-TRANSACAO      TO WRK-CHAMADOR-DBTEMP
PROC01     END-IF.
PROC01
           MOVE WRK-AREA-DBTEMP        TO WRK-ENT-DADOS

           MOVE 'DCOM7995'             TO WRK-MODULO

           CALL WRK-MODULO             USING WRK-ENTRADA-DCOM7995
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
       1104-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1105-LER-DBTEMP                 SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-ENTRADA-DCOM7995
                      WRK-SAIDA-DCOM7995.

           MOVE 'CS'                   TO WRK-ENT-OPCAO
           MOVE WRK-COD-USER-R         TO WRK-ENT-COD-FUNC
           MOVE LNK-IO-LTERM           TO WRK-ENT-COD-TERMINAL
           MOVE INP-TMSTAMP            TO WRK-ENT-TIMESTAMP
           MOVE 'DCOM0405'             TO WRK-ENT-TRANS
           MOVE 1                      TO WRK-ENT-PAGINA

           MOVE 'DCOM7995'             TO WRK-MODULO

           CALL WRK-MODULO             USING WRK-ENTRADA-DCOM7995
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

           MOVE WRK-SAI-DADOS          TO WRK-AREA-DBTEMP.

      *----------------------------------------------------------------*
       1105-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1106-APAGAR-DBTEMP              SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-ENTRADA-DCOM7995
                      WRK-SAIDA-DCOM7995.

           MOVE 'ET'                   TO WRK-ENT-OPCAO
           MOVE WRK-COD-USER-R         TO WRK-ENT-COD-FUNC
           MOVE LNK-IO-LTERM           TO WRK-ENT-COD-TERMINAL
           MOVE INP-TMSTAMP            TO WRK-ENT-TIMESTAMP
           MOVE 'DCOM0405'             TO WRK-ENT-TRANS
           MOVE 1                      TO WRK-ENT-PAGINA

           MOVE 'DCOM7995'             TO WRK-MODULO

           CALL WRK-MODULO             USING WRK-ENTRADA-DCOM7995
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
       1106-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1110-ACESSAR-DCOM4174           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-COD-USER           TO 4174-ENT-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO 4174-ENT-CTERM
           MOVE 'DCOM4174'             TO WRK-MODULO

           CALL WRK-MODULO             USING 4174-ENTRADA-ROTEADOR
                                             4174-SAIDA-ROTEADOR
                                             ERRO-AREA
                                             WRK-SQLCA.

           EVALUATE 4174-SAI-COD-RETORNO
               WHEN '0000'
                   MOVE SPACES         TO WRK-INCONSIS
                   MOVE 4174-SAI-CAG-BCRIA TO ROTENT-6437-AGEPESQ-ENV
                   PERFORM 1111-1-VALIDAR-PERFIL
                   IF  WRK-INCONSIS    EQUAL SPACES
DTS002                 PERFORM 1111-2-VERIFICAR-OPER-NETEMPR
DTS002
DTS002                 IF WRK-INCONSIS EQUAL SPACES
                          PERFORM 1111-CARREGAR-DADOS-TELA
DTS002                 END-IF
                   ELSE
                       MOVE SPACES         TO WRK-INCONSIS
                       MOVE 4174-SAI-CAG-OPER-OPER
                                           TO ROTENT-6437-AGEPESQ-ENV
                       PERFORM 1111-1-VALIDAR-PERFIL
                       IF  WRK-INCONSIS    EQUAL SPACES
                           PERFORM 1111-CARREGAR-DADOS-TELA
                       ELSE
                           MOVE 'S'        TO WRK-FLAG-ERRO
                           MOVE 'USUARIO NAO AUTORIZADO'
                                           TO WRK-MENSAGEM-ERRO
                           PERFORM 2300-RETORNAR-CHAMADOR
                       END-IF
                   END-IF

               WHEN '0003'
PRM02          WHEN '0129'
                   MOVE '*'            TO WRK-INCONSIS
                   MOVE 'S'            TO WRK-FLAG-ERRO
                   MOVE 4174-SAI-MSG-RETORNO
                                       TO WRK-MENSAGEM-ERRO
                   PERFORM 2300-RETORNAR-CHAMADOR

               WHEN '0099'
                   MOVE 'DB2'          TO ERR-TIPO-ACESSO
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                   MOVE 4174-SAI-MSG-RETORNO
                                       TO OUT-MENSAG
                   MOVE '*'            TO WRK-INCONSIS
           END-EVALUATE.

           IF  4174-SAI-COD-RETORNO    EQUAL ZEROS AND
               4174-SAI-CSIT-DESC-COML EQUAL 16    AND
               WRK-INCONSIS            NOT EQUAL '*'
               MOVE '*'            TO WRK-INCONSIS
               MOVE 'S'            TO WRK-FLAG-ERRO
               MOVE 'OPERACAO NAO PERMITE A OPCAO SELECIONADA, OPERACAO
      -             'CANCELADA'    TO WRK-MENSAGEM-ERRO
               PERFORM 2300-RETORNAR-CHAMADOR
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1111-1-VALIDAR-PERFIL           SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO ERRO-AREA WRK-SQLCA.
           MOVE ZEROS                  TO ROTENT-6437-PROD-ENV
                                          ROTENT-6437-SUBPRO-ENV
           MOVE WRK-COD-USER           TO ROTENT-6437-CFUNC-ENV.
           MOVE LNK-IO-LTERM           TO ROTENT-6437-CTERM.
           MOVE WRK-COD-DEPTO-N        TO ROTENT-6437-AGEUSU-ENV.
           MOVE 'A'                    TO ROTENT-6437-FUNCAO-ENV.
           MOVE 'DCOM6437'             TO WRK-MODULO

           CALL WRK-MODULO             USING ROTENT-6437-ENVIO
                                             ROTSAI-6437-RETORNO
                                             ERRO-AREA
                                             WRK-SQLCA.

           IF  ROTSAI-6437-COD-RET     NOT EQUAL ZEROS OR
               ROTSAI-6437-RESUL-RET   GREATER 3
               MOVE '*'                TO WRK-INCONSIS
           END-IF.

      *----------------------------------------------------------------*
       1111-1-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

DTS002*----------------------------------------------------------------*
DTS002*    VERIFICA SE OPERACAO CADASTRADA PELO NET EMPRESA:           *
DTS002*  - OPERACAO CADASTRADA PELO NET EMPRESA, PODERA SER ALTERADA   *
DTS002*    PELA AGENCIA  SOMENTE  SE FOI CANCELADA A  LIBERACAO PELA   *
DTS002*    AGENCIA (SITUACAO FINAL = 21)                               *
DTS002*----------------------------------------------------------------*
DTS002 1111-2-VERIFICAR-OPER-NETEMPR   SECTION.
DTS002*----------------------------------------------------------------*
DTS002
DTS002     INITIALIZE WRK-5505E-ENTRADA
DTS002                WRK-5505E-RETORNO.
DTS002
DTS002     MOVE WRK-COD-USER-R         TO WRK-5505E-CFUNC-BDSCO.
DTS002     MOVE LNK-IO-LTERM           TO WRK-5505E-CTERM.
DTS002     MOVE WRK-INP-DANO           TO WRK-5505E-DANO-OPER-DESC.
DTS002     MOVE WRK-INP-NSEQ           TO WRK-5505E-NSEQ-OPER-DESC.
DTS002     MOVE 140                    TO WRK-5505E-CEVNTO-DESC-COML.
DTS002     MOVE 'DCOM5505'             TO WRK-MODULO
DTS002
DTS002     CALL WRK-MODULO             USING WRK-5505E-ENTRADA
DTS002                                       WRK-5505E-RETORNO
DTS002                                       ERRO-AREA
DTS002                                       WRK-SQLCA.
DTS002
DTS002     EVALUATE  WRK-5505S-COD-RETORNO
DTS002         WHEN  0000
DTS002           IF  WRK-5505S-CSIT-FNAL-DESC EQUAL 22
DTS002               MOVE '*'          TO WRK-INCONSIS
DTS002               MOVE 'S'          TO WRK-FLAG-ERRO
DTS002               MOVE 'OPERACAO NAO PERMITE A OPCAO SELECIONADA, OPE
DTS002-                   'RACAO CADASTRADA PELO NETEMPRESA'
DTS002                                 TO WRK-MENSAGEM-ERRO
DTS002               PERFORM 2300-RETORNAR-CHAMADOR
DTS002           END-IF
DTS002
DTS002        WHEN  0099
DTS002              MOVE 'DB2'         TO ERR-TIPO-ACESSO
DTS002              PERFORM 9999-PROCESSAR-ROTINA-ERRO
DTS002     END-EVALUATE.
DTS002
DTS002*----------------------------------------------------------------*
DTS002 1111-1-99-FIM.                  EXIT.
DTS002*----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1111-CARREGAR-DADOS-TELA        SECTION.
      *----------------------------------------------------------------*

           MOVE  4174-SAI-DANO-OPER-DESC
                                           TO OUT-OPERAC(1:4)
           MOVE  4174-SAI-NSEQ-OPER-DESC
                                           TO OUT-OPERAC(5:9)
           MOVE  4174-SAI-CPRODT           TO OUT-PRODUTO
           MOVE  4174-SAI-IABREV-PRODT     TO OUT-DPRODUT
           MOVE  4174-SAI-CSPROD-DESC-COML
                                           TO OUT-SPRODUT
           MOVE  4174-SAI-IRSUMO-SPROD-DESC
                                           TO OUT-DSPRODU
CAP006     IF 4174-SAI-CTPO-DESC-COML      EQUAL 4
.             PERFORM 2955-OBTER-SIT-PROTOC
.          ELSE
.             MOVE  4174-SAI-IRSUMO-SIT-DESC
.                                          TO OUT-SITUAC
CAP006     END-IF
      *
           MOVE  4174-SAI-IPSSOA-DESC-COML TO OUT-NOME
           MOVE  4174-SAI-CCNPJ-CPF(1:3)   TO OUT-CNPJCPF(1:3)
                                              INP-CNPJCPF(1:3)
           MOVE  '.'                       TO OUT-CNPJCPF(4:1)
           MOVE  4174-SAI-CCNPJ-CPF(4:3)   TO OUT-CNPJCPF(5:3)
                                              INP-CNPJCPF(5:3)
           MOVE  '.'                       TO OUT-CNPJCPF(8:1)
           MOVE  4174-SAI-CCNPJ-CPF(7:3)   TO OUT-CNPJCPF(9:3)
                                              INP-CNPJCPF(9:3)
           MOVE  '/'                       TO OUT-CNPJCPF(12:1)
           MOVE  4174-SAI-CFLIAL-CNPJ      TO OUT-CNPJCPF(13:4)
                                              INP-CNPJCPF(13:4)
           MOVE  '-'                       TO OUT-CNPJCPF(17:1)
           MOVE  4174-SAI-CCTRL-CNPJ-CPF   TO OUT-CNPJCPF(18:2)
                                              INP-CNPJCPF(18:2)
           MOVE  4174-SAI-CBCO             TO OUT-CAMPO2(1:3)
           MOVE  '/'                       TO OUT-CAMPO2(4:1)
           MOVE  4174-SAI-CAG-BCRIA        TO OUT-CAMPO2(5:5)
                                              INP-CAMPO2(5:5)
           MOVE  '/'                       TO OUT-CAMPO2(10:1)
           MOVE  4174-SAI-CCTA-BCRIA-CLI   TO OUT-CAMPO2(11:13)
                                              INP-CAMPO2(11:13)

           IF 4174-SAI-CCONTR-LIM-DESC      GREATER  ZEROS
              MOVE 'CONTR LIMITE:'          TO  OUT-CAMPO3
              MOVE 4174-SAI-CCONTR-LIM-DESC TO  OUT-CAMPO4
           ELSE
             IF 4174-SAI-CCONTR-CONVE-DESC  GREATER  ZEROS AND
                4174-SAI-CELMTO-DESC-COML   EQUAL 008
                MOVE 'CONV GERAL..:'            TO OUT-CAMPO3
                MOVE 4174-SAI-CCONTR-CONVE-DESC TO OUT-CAMPO4
             ELSE
             IF 4174-SAI-CCONTR-CONVE-DESC  GREATER  ZEROS AND
                4174-SAI-CELMTO-DESC-COML   EQUAL 009
                MOVE 'CONV CLIENTE:'            TO OUT-CAMPO3
                MOVE 4174-SAI-CCONTR-CONVE-DESC TO OUT-CAMPO4
             END-IF
           END-IF.

           MOVE  4174-SAI-VOPER-DESC-COML  TO OUT-VLROPER-N
           MOVE  4174-SAI-VTAC-OPER-DESC   TO OUT-VLTAROP-N
           MOVE  4174-SAI-VTARIF-REG-TITLO TO OUT-VLTRFTT-N
           MOVE  4174-SAI-VJURO-OPER-DESC  TO OUT-VLRJUR-N
           MOVE  4174-SAI-VIOF-OPER-DESC   TO OUT-VLRIOF-N
           MOVE  4174-SAI-VLIQ-OPER-DESC   TO OUT-VLRLIQD-N

           PERFORM 1111-ACESSAR-DCOM4266.

           MOVE  4174-SAI-DVCTO-FNAL-OPER  TO OUT-DTVCTOF
           INSPECT OUT-DTVCTOF REPLACING ALL '.' BY '/'.
           MOVE  4174-SAI-DINIC-OPER-DESC  TO OUT-DTOPER
           INSPECT OUT-DTOPER  REPLACING ALL '.' BY '/'.
           MOVE  4174-SAI-TMED-PONDE-OPER  TO OUT-PRZMED
           MOVE  4174-SAI-QTITLO-OPER-APURA TO OUT-QTDETIT-N
           MOVE  4174-SAI-ITPO-GARNT       TO OUT-GARANTI

           MOVE 4174-SAI-CTPO-DESC-COML    TO OUT-TIPODES
           IF  4174-SAI-CTPO-DESC-COML     EQUAL 2 OR 3
               MOVE '11-'                  TO OUT-LITPF11A
               MOVE 'RESUMO DA ANALISE'    TO OUT-LITPF11B
           ELSE
               MOVE SPACES                 TO OUT-LITPF11A
                                              OUT-LITPF11B

               IF INP-FASE                 EQUAL '3'
                  MOVE '11-'               TO OUT-LITPF11A
                  MOVE 'CONFIRMA LIBERAR ' TO OUT-LITPF11B
               END-IF
           END-IF.

BRQ001     EVALUATE 4174-SAI-CTERM
             WHEN 'NETEMPR'
             WHEN 'MOBILE '
               INITIALIZE WRK-5505E-ENTRADA
               MOVE 150             TO WRK-5505E-CEVNTO-DESC-COML
               PERFORM 1112-ACESSAR-DCOM5505
               IF  WRK-5505S-COD-RETORNO EQUAL ZEROS
                   MOVE 'MOBILE '   TO WRK-OUT-CANAL
               ELSE
                   MOVE 'NETEMPR'   TO WRK-OUT-CANAL
               END-IF

             WHEN 'TELEBCO'
               MOVE 'TELEBCO'       TO WRK-OUT-CANAL

             WHEN OTHER
               MOVE 'AGENCIA'       TO WRK-OUT-CANAL
BRQ001     END-EVALUATE.

BRQ002     MOVE 'CO'                   TO WRK-FUNCAO-5490
BRQ002
BRQ002     PERFORM 1113-OBTER-INFO-LIBRC-MIDIA.

      *----------------------------------------------------------------*
       1111-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA OBTEM A PERIODICIDADE DA TAXA                     *
      *----------------------------------------------------------------*
       1111-ACESSAR-DCOM4266           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE 4266E-ENTRADA
                      4266S-SAIDA

           MOVE 'DCOM4266'              TO  WRK-MODULO

           MOVE  WRK-COD-USER           TO  4266E-CFUNC-BDSCO.
           MOVE  LNK-IO-LTERM           TO  4266E-CTERM.
           MOVE  OUT-OPERAC             (1:4)
                                        TO  4266E-DANO-OPER-DESC.
           MOVE  OUT-OPERAC             (5:9)
                                        TO  4266E-NSEQ-OPER-DESC.
           MOVE  OUT-CNPJCPF            (1:3)
                                        TO  4266E-CCNPJ-CPF(1:3).
           MOVE  OUT-CNPJCPF            (5:3)
                                        TO  4266E-CCNPJ-CPF(4:3).
           MOVE  OUT-CNPJCPF            (9:3)
                                        TO  4266E-CCNPJ-CPF(7:3).
           MOVE  OUT-CNPJCPF            (13:4)
                                        TO  4266E-CFLIAL-CNPJ.

           MOVE 'DCOM4266'              TO  WRK-MODULO

           CALL  WRK-MODULO          USING  4266E-ENTRADA
                                            4266S-SAIDA
                                            ERRO-AREA
                                            WRK-SQLCA.

           EVALUATE 4266S-COD-RETORNO

              WHEN '0000'

                   MOVE 4266S-IRSUMO-TPO-TX-JURO    TO
                        OUT-TPOTAXA
                   IF  4266S-CINDCD-FREQ-TX  EQUAL 'AA'
                       MOVE 'A.A.'             TO   OUT-NMJUROS
                       MOVE 4174-SAI-PTX-JURO-ANO
                                               TO   OUT-TAXAJUR-N
                   ELSE
                       MOVE 'A.M.'             TO   OUT-NMJUROS
                       MOVE 4174-SAI-PTX-JURO-MES
                                               TO   OUT-TAXAJUR-N
                   END-IF

              WHEN '0099'
                    MOVE   'DB2'               TO   ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO

              WHEN  OTHER
                    MOVE 'A.M.'                TO  OUT-NMJUROS
                    MOVE 4174-SAI-PTX-JURO-MES
                                               TO  OUT-TAXAJUR-N
           END-EVALUATE.

      *----------------------------------------------------------------*
       1111-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

BRQ001*----------------------------------------------------------------*
       1112-ACESSAR-DCOM5505           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-5505E-RETORNO.

           MOVE WRK-COD-USER-R         TO WRK-5505E-CFUNC-BDSCO.
           MOVE LNK-IO-LTERM           TO WRK-5505E-CTERM.
           MOVE WRK-INP-DANO           TO WRK-5505E-DANO-OPER-DESC.
           MOVE WRK-INP-NSEQ           TO WRK-5505E-NSEQ-OPER-DESC.
           MOVE 'DCOM5505'             TO WRK-MODULO

           CALL WRK-MODULO             USING WRK-5505E-ENTRADA
                                             WRK-5505E-RETORNO
                                             ERRO-AREA
                                             WRK-SQLCA.

           EVALUATE  WRK-5505S-COD-RETORNO
              WHEN  0099
                    MOVE 'DB2'         TO ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-EVALUATE.

      *----------------------------------------------------------------*
       1112-99-FIM.                  EXIT.
BRQ001*----------------------------------------------------------------*

BRQ002*----------------------------------------------------------------*
BRQ002*     OBTER PARAMETRO DE LIBERACAO NAS MIDIAS
BRQ002*----------------------------------------------------------------*
BRQ002 1113-OBTER-INFO-LIBRC-MIDIA     SECTION.
BRQ002*----------------------------------------------------------------*
BRQ002
BRQ002     INITIALIZE WRK-5490-ENTRADA
BRQ002                WRK-5490-SAIDA
BRQ002                ERRO-AREA
BRQ002                WRK-SQLCA.
BRQ002
BRQ002     MOVE WRK-INP-DANO           TO WRK-E5490-DANO-OPER.
BRQ002     MOVE WRK-INP-NSEQ           TO WRK-E5490-NSEQ-OPER.
BRQ002     MOVE WRK-FUNCAO-5490        TO WRK-E5490-FUNCAO.
BRQ002     MOVE 'DCOM5490'             TO WRK-MODULO
BRQ002
BRQ002     CALL  WRK-MODULO            USING WRK-5490-ENTRADA
BRQ002                                       WRK-5490-SAIDA
BRQ002                                       ERRO-AREA
BRQ002                                       WRK-SQLCA.
BRQ002
BRQ002     IF  WRK-S5490-COD-RETORNO   EQUAL 99
BRQ002         MOVE 'DB2'              TO ERR-TIPO-ACESSO
BRQ002         PERFORM 9999-PROCESSAR-ROTINA-ERRO
BRQ002     END-IF.
BRQ002
BRQ002     IF  WRK-S5490-LIBRC-MIDIA   EQUAL 'S'
BRQ002         MOVE 'SIM'              TO OUT-LIBMID
BRQ002     ELSE
BRQ002         MOVE 'NAO'              TO OUT-LIBMID
BRQ002     END-IF.
BRQ002
BRQ002     IF  WRK-FUNCAO-5490         EQUAL 'VL' AND
BRQ002         WRK-S5490-COD-RETORNO   NOT EQUAL ZEROS
BRQ002         MOVE WRK-S5490-MSG-RETORNO
BRQ002                                 TO WRK-MENSAGEM-ERRO
BRQ002         MOVE 'S'                TO WRK-FLAG-ERRO
BRQ002         PERFORM 2300-RETORNAR-CHAMADOR
BRQ002     END-IF.
BRQ002
BRQ002*----------------------------------------------------------------*
BRQ002 1113-99-FIM.                    EXIT.
BRQ002*----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1120-ACESSAR-DCOM4131           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-COD-USER           TO 4131E-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO 4131E-CTERM

           IF  INP-TITULO              EQUAL 'CANCELAR'
               MOVE 'CA'               TO 4131E-FLAG-FILTRO
           ELSE
               MOVE 'LI'               TO 4131E-FLAG-FILTRO
           END-IF.


           MOVE 'DCOM4131'             TO WRK-MODULO

           CALL WRK-MODULO             USING 4131E-ROTEADOR
                                             4131S-ROTEADOR
                                             ERRO-AREA
                                             WRK-SQLCA.

           EVALUATE 4131S-COD-RETORNO
               WHEN '0100'
               WHEN '0000'
                   STRING 'PRAZO: '4131S-CFUNC-AUTRZ-PRAZO
                   ' TAXA: '4131S-CFUNC-AUTRZ-TAXA
                   ' CONE: '4131S-CFUNC-AUTRZ-CONE
                   DELIMITED BY SIZE           INTO OUT-AUTRZTE
                   MOVE 4131S-PARECER-FORML-DESC(1:53)
                                       TO OUT-DESCR1
                   MOVE 4131S-PARECER-FORML-DESC(54:75)
                                       TO OUT-DESCR2
                   MOVE 4131S-PMAX-LIBRC-ANTCP TO OUT-JURMAXI-N
                                                  OUT-JRLIBER-N
                   MOVE 4131S-VLIBRC-MAX       TO OUT-VLRMAXI-N
                   MOVE 4131S-VLIBRC-A-LIBERAR TO OUT-VLLIBER-N
                   MOVE 4131S-TPO-LIBRC        TO OUT-TIPOLIB

               WHEN '0099'
                   MOVE 'DB2'          TO ERR-TIPO-ACESSO
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                   MOVE 4131S-MSG-RETORNO
                                       TO WRK-MENSAGEM-ERRO
                   MOVE 'S'            TO WRK-FLAG-ERRO
                   PERFORM 2300-RETORNAR-CHAMADOR

           END-EVALUATE.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1131-BUSCAR-DESC-GARANTIA       SECTION.
      *----------------------------------------------------------------*

           INITIALIZE ENTRADA-6425-ROTEADOR
                      SAIDA-6425-ROTEADOR.

           MOVE 23                     TO ROTENT-6425-CGARAN.
           MOVE 'DCOM6425'             TO WRK-MODULO

           CALL WRK-MODULO             USING ENTRADA-6425-ROTEADOR
                                             SAIDA-6425-ROTEADOR
                                             ERRO-AREA
                                             WRK-SQLCA.

           EVALUATE  ROTSAI-6425-COD-RETORNO
               WHEN '0000'
                     MOVE ROTSAI-6425-IGARAN  TO OUT-GARANTI

               WHEN  '0099'
                     PERFORM 9999-PROCESSAR-ROTINA-ERRO

            END-EVALUATE.

      *----------------------------------------------------------------*
       1131-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000-PROCESSAR-DCOMDETM         SECTION.
      *----------------------------------------------------------------*

           MOVE  WRK-MENSAGEM          TO  INPUT-DETM.
           PERFORM 2200-DEVOLVER-TELA.

           EVALUATE  TRUE
               WHEN  INP-PFK          EQUAL  'H'  OR
                    (INP-PFK          EQUAL  '.'  AND
                     INP-COMANDO      EQUAL  'PFK01')
                     MOVE INP-MENSAG  TO OUT-MENSAG

               WHEN  INP-PFK          EQUAL  '3'  OR
                    (INP-PFK          EQUAL  '.'  AND
                     INP-COMANDO      EQUAL 'PFK03')
                     PERFORM  2300-RETORNAR-CHAMADOR

               WHEN  INP-PFK          EQUAL  '5'  OR
                    (INP-PFK          EQUAL  '.'  AND
                     INP-COMANDO      EQUAL  'PFK05')
                     PERFORM  2400-MENU-ROTINAS

               WHEN  INP-PFK          EQUAL  'B'  OR
                    (INP-PFK          EQUAL  '.'  AND
                     INP-COMANDO      EQUAL  'PFK10')
                     PERFORM  2500-MENU-DCOM

PROC01         WHEN  INP-PFK          EQUAL  'D' OR
PROC01              (INP-PFK          EQUAL  '.'  AND
PROC01               INP-COMANDO      EQUAL  'PFK12')
PROC01               PERFORM 2950-ACESSAR-CET
PROC01
               WHEN  INP-FASE         EQUAL '1'
                     PERFORM  2050-TRATAR-FASE-1

               WHEN  INP-FASE         EQUAL '2'
                     PERFORM  2100-TRATAR-FASE-2

               WHEN  INP-FASE         EQUAL '3'
                     PERFORM  2103-TRATAR-FASE-3

               WHEN  OTHER
                     MOVE  'PF INVALIDA'   TO  OUT-MENSAG
           END-EVALUATE.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2050-TRATAR-FASE-1              SECTION.
      *----------------------------------------------------------------*

           EVALUATE TRUE
               WHEN (INP-PFK          EQUAL  'C' OR
                    (INP-PFK          EQUAL  '.'  AND
                     INP-COMANDO      EQUAL  'PFK11')) AND
                    (INP-TIPODES      EQUAL 2 OR 3)
                         IF  INP-TIPODES  EQUAL 2
                             PERFORM  2800-ACESSA-DCOM0214
                         ELSE
                             PERFORM  2900-ACESSA-DCOM0222
                         END-IF

               WHEN  INP-PFK          EQUAL  '.'  AND
                     INP-COMANDO      EQUAL  SPACES
                     PERFORM  2600-TRATAR-ENTER
                     IF  WRK-INCONSIS EQUAL SPACES
                         MOVE 'TECLE <PF6> PARA CONFIRMAR OU TECLE <ENTE
      -                       'R> PARA ALTERAR'
                                      TO OUT-MENSAG
                         MOVE WRK-241 TO OUT-ATTR-JRLIBER
                                         OUT-ATTR-VLLIBER
                         MOVE '2'     TO OUT-FASE
                     END-IF

               WHEN  OTHER
                     MOVE  'PF INVALIDA'   TO  OUT-MENSAG

           END-EVALUATE.

      *----------------------------------------------------------------*
       2050-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2100-TRATAR-FASE-2              SECTION.
      *----------------------------------------------------------------*

           EVALUATE TRUE
               WHEN  INP-PFK          EQUAL  '6'  OR
                    (INP-PFK          EQUAL  '.'  AND
                     INP-COMANDO      EQUAL  'PFK06')
                     IF  INP-TITULO   EQUAL ' LIBERAR'
FM1510                   IF INP-TIPODES
.                                     EQUAL 4
.CAP02                      IF INP-TMSTAMP(1:10) EQUAL '2021-06-03'
.|                                                  OR '2021-06-04'
.|                                                  OR '2021-06-05'
.|                                                  OR '2021-06-06'
.|                             MOVE WRK-241  TO OUT-ATTR-JRLIBER
.|                                              OUT-ATTR-VLLIBER
.|            MOVE 'NAO PERMITIDO LIBERACAO DE ORPAGS ENTRE OS DIAS 03 E
.|    -              ' 06/06/2021 - NORMAT.4734'
.|                                           TO OUT-MENSAG
.|                             GO TO 2100-99-FIM
.CAP02                      END-IF
.                           PERFORM 2105-TRATAR-TPDESC-04
.                           MOVE '3'  TO OUT-FASE
.                           MOVE 'TECLE <PF11> PARA LIBERAR E AGUARDE LI
.     -                          'BERACAO DA OPERACAO'
.                                     TO OUT-MENSAG
.                           MOVE WRK-241
.                                     TO OUT-ATTR-JRLIBER
.                                        OUT-ATTR-VLLIBER
.                           MOVE '11-'               TO OUT-LITPF11A
.                           MOVE 'CONFIRMA LIBERAR ' TO OUT-LITPF11B
.                           IF WRK-INCONSIS EQUAL '*'
.                              GO TO 2100-99-FIM
.                           END-IF
FM1510                   ELSE
                            MOVE 'OPERACAO LIBERADA COM SUCESSO - C/C CR
      -                          'EDITADA'
                                      TO WRK-MENSAGEM-ERRO
CAP003                      MOVE 'S'  TO WRK-FLAG-ERRO
                            PERFORM 2110-ACESSA-DCOM5110
FM1510                   END-IF
                     ELSE
                         PERFORM 2120-ACESSA-DCOM5113
                         MOVE 'OPERACAO CANCELADA COM SUCESSO'
                                      TO WRK-MENSAGEM-ERRO
                     END-IF
                     IF  WRK-INCONSIS EQUAL '*'
                         MOVE WRK-241 TO OUT-ATTR-JRLIBER
                                         OUT-ATTR-VLLIBER
                         GO TO 2100-99-FIM
                     END-IF
CAP001               IF  INP-TITULO   NOT EQUAL ' LIBERAR'
                         MOVE 'S'     TO WRK-FLAG-ERRO
                         PERFORM 2300-RETORNAR-CHAMADOR
CAP001               ELSE
CAP001                   IF WRK-FLAG-ERRO
CAP001                                EQUAL 'S'
CAP001                      PERFORM 2300-RETORNAR-CHAMADOR
CAP001                   END-IF
CAP001               END-IF

               WHEN (INP-PFK          EQUAL  'C' OR
                    (INP-PFK          EQUAL  '.'  AND
                     INP-COMANDO      EQUAL  'PFK11')) AND
                    (INP-JURMAXI      EQUAL '100,00'   AND
                     INP-TITULO   NOT EQUAL 'CANCELAR')AND
                    (INP-TIPODES      EQUAL 2 OR 3)
                         IF  INP-TIPODES  EQUAL 2
                             PERFORM  2800-ACESSA-DCOM0214
                         ELSE
                             PERFORM  2900-ACESSA-DCOM0222
                         END-IF

               WHEN  INP-PFK          EQUAL  '.'  AND
                     INP-COMANDO      EQUAL  SPACES

                     IF INP-TITULO     EQUAL 'CANCELAR'
                       MOVE 'TECLE <PF6> PARA CANCELAR LIBERACAO OU <PF3
      -             '> PARA VOLTAR'            TO OUT-MENSAG
                       MOVE WRK-241 TO OUT-ATTR-JRLIBER
                                       OUT-ATTR-VLLIBER
                     ELSE
                       IF INP-JURMAXI  EQUAL '100,00' AND
                          INP-TIPOLIB  EQUAL 'T' AND
CAP003                    INP-TIPODES  NOT EQUAL 4
                           MOVE 'TECLE <PF6> PARA CONFIRMAR LIBERACAO'
                                        TO OUT-MENSAG
                           MOVE WRK-241 TO OUT-ATTR-JRLIBER
                                           OUT-ATTR-VLLIBER
                       ELSE
      *
CAP003                     IF INP-JURMAXI  EQUAL '100,00' AND
CAP003                        INP-TIPOLIB  EQUAL 'T' AND
CAP003                        INP-TIPODES  EQUAL 4
CAP001                     MOVE 'TECLE <PF6> PARA REGISTRAR A OPERACAO N
CAP001-                         'A CIP'
CAP003                                     TO OUT-MENSAG
CAP003                        MOVE WRK-241 TO OUT-ATTR-JRLIBER
CAP003                                        OUT-ATTR-VLLIBER
                           ELSE
                              MOVE 'DIGITE OS DADOS E TECLE ENTER'
                                           TO OUT-MENSAG
                              MOVE '1'     TO OUT-FASE
                              MOVE WRK-49361 TO OUT-ATTR-JRLIBER
                           END-IF
                       END-IF
                     END-IF

               WHEN  OTHER
                     MOVE  'PF INVALIDA'   TO  OUT-MENSAG
                     MOVE WRK-241     TO OUT-ATTR-JRLIBER
                                         OUT-ATTR-VLLIBER

           END-EVALUATE.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

CAP001*----------------------------------------------------------------*
|      2103-TRATAR-FASE-3              SECTION.
|     *----------------------------------------------------------------*
|
|          EVALUATE TRUE
|              WHEN  INP-PFK          EQUAL  'C' OR
|                   (INP-PFK          EQUAL  '.'  AND
|                    INP-COMANDO       EQUAL  'PFK11')
|                     IF INP-TITULO    EQUAL ' LIBERAR'
|                        IF INP-TIPODES
|                                      EQUAL 4
|                           PERFORM 2105-TRATAR-TPDESC-04
|                           IF WRK-INCONSIS EQUAL '*'
|                              GO TO 2103-99-FIM
|                           END-IF
|                        ELSE
|                           MOVE 'OPERACAO LIBERADA COM SUCESSO - C/C CR
|     -                       'EDITADA'
|                                      TO WRK-MENSAGEM-ERRO
|                           PERFORM 2110-ACESSA-DCOM5110
|                        END-IF
|                        MOVE 'S'      TO WRK-FLAG-ERRO
|                        PERFORM 2300-RETORNAR-CHAMADOR
|                     END-IF
|     *
|              WHEN  OTHER
|                    MOVE  'PF INVALIDA'
|                                      TO OUT-MENSAG
|                    MOVE WRK-241      TO OUT-ATTR-JRLIBER
|                                         OUT-ATTR-VLLIBER
|
|          END-EVALUATE.
|
|     *----------------------------------------------------------------*
|      2103-99-FIM.                    EXIT.
CAP001*----------------------------------------------------------------*
      *
FM1510*----------------------------------------------------------------*
.      2105-TRATAR-TPDESC-04           SECTION.
.     *----------------------------------------------------------------*
.
CAP001     IF INP-FASE                 EQUAL '2'
|             PERFORM 2130-TRATAR-LIBERACAO-PFK06
|          END-IF
|
|          IF INP-FASE                 EQUAL '3'
CAP001        PERFORM 2130-1-TRATAR-LIBERACAO-PFK11
.             IF WRK-OK                EQUAL 'S'
.                MOVE 'OPERACAO LIBERADA COM SUCESSO - C/C CREDITADA'
.                                      TO WRK-MENSAGEM-ERRO
.                PERFORM 2110-ACESSA-DCOM5110
.             END-IF
.          END-IF.
.
.     *----------------------------------------------------------------*
.      2105-99-FIM.                    EXIT.
FM1510*----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2110-ACESSA-DCOM5110            SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-COD-USER           TO 5110E-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO 5110E-CTERM
           MOVE INP-OPERAC(1:4)        TO 5110E-DANO-OPER-DESC
           MOVE INP-OPERAC(5:9)        TO 5110E-NSEQ-OPER-DESC
           MOVE INP-TIPOLIB            TO 5110E-TPO-LIBRC
           MOVE INP-VLLIBER-E          TO WRK-VLRLIBR-AUX
           MOVE WRK-VLRLIBR-AUX        TO 5110E-VLIBRC
           MOVE 'DCOM5110'             TO WRK-MODULO

           CALL WRK-MODULO             USING 5110E-ROTEADOR
                                             5110S-ROTEADOR
                                             ERRO-AREA
                                             WRK-SQLCA.


           IF  5110S-COD-RETORNO       NOT EQUAL '0000'
               IF 5110S-COD-RETORNO    EQUAL '0099' OR '9999'
                   MOVE 'DB2'          TO ERR-TIPO-ACESSO
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO
               ELSE
                   IF 5110S-COD-RETORNO          EQUAL '7777' OR
                      5110S-COD-RETORNO          EQUAL '7779'
                       MOVE 'APL'                TO ERR-TIPO-ACESSO
                       PERFORM 9999-PROCESSAR-ROTINA-ERRO
                   ELSE
                      IF  5110S-COD-RETORNO      EQUAL '7778'
                          MOVE SPACES            TO WRK-INCONSIS
                          MOVE 5110S-MSG-RETORNO TO WRK-MENSAGEM-ERRO
                      ELSE
                          MOVE '*'               TO WRK-INCONSIS
                          MOVE 5110S-MSG-RETORNO TO OUT-MENSAG
                          PERFORM 8888-EFETUAR-ROLLBACK
                      END-IF
                   END-IF
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2120-ACESSA-DCOM5113            SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO 5113E-COD-RETORNO
           MOVE SPACES                 TO 5113E-MSG-RETORNO
           MOVE ZEROS                  TO 5113E-RESTART
           MOVE SPACES                 TO 5113E-FLAG
           MOVE WRK-COD-USER           TO 5113E-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO 5113E-CTERM
           MOVE INP-OPERAC(1:4)        TO 5113E-DANO-OPER-DESC
           MOVE INP-OPERAC(5:9)        TO 5113E-NSEQ-OPER-DESC

           STRING INP-CNPJCPF(1:3)
                  INP-CNPJCPF(5:3)
                  INP-CNPJCPF(9:3)   DELIMITED BY SIZE
                                     INTO 5113E-CPFCNPJ
           MOVE INP-CNPJCPF(13:4)      TO 5113E-CONTROLE
           MOVE INP-CNPJCPF(18:2)      TO 5113E-FILIAL
           MOVE 'DCOM5113'             TO WRK-MODULO

           CALL WRK-MODULO             USING 5113E-ROTEADOR
                                             5113S-ROTEADOR
                                             ERRO-AREA
                                             WRK-SQLCA.

           IF 5113S-COD-RETORNO    NOT EQUAL '0000'
               IF 5113S-COD-RETORNO    EQUAL '0099'
                   MOVE 'DB2'          TO ERR-TIPO-ACESSO
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO
               ELSE
DTS001             PERFORM 8888-EFETUAR-ROLLBACK
                   MOVE '*'            TO WRK-INCONSIS
                   MOVE 5113S-MSG-RETORNO TO OUT-MENSAG
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

CAP001*----------------------------------------------------------------*
|     *             TRATAR LIBERACAO DE APENAS UMA OPERACAO            *
|     *----------------------------------------------------------------*
|      2130-TRATAR-LIBERACAO-PFK06     SECTION.
|     *----------------------------------------------------------------*
|     *
|          INITIALIZE                  WRK-OK
|     *
|          PERFORM 2131-VERIFICAR-PROTOCOLO
|     *
|          IF WRK-ACHOU-SOLTC          EQUAL 'S'
|             EVALUATE DCOMZD-S-CSIT-PROT-AGNDA
|                  WHEN 1
|                  WHEN 2
|                       MOVE 'OPERACAO INDISPONIVEL, LIBERACAO EM PROCES
|     -                 'SAMENTO'
|                                      TO OUT-MENSAG
|                                         WRK-MENSAGEM-ERRO
|                       MOVE 'S'       TO WRK-FLAG-ERRO
|                  WHEN 3
|                       MOVE 'OPERACAO INDISPONIVEL, EFETUE NOVA OPERACA
|     -                 'O'
|                                      TO OUT-MENSAG
|                                         WRK-MENSAGEM-ERRO
|                       MOVE 'S'       TO WRK-FLAG-ERRO
|                  WHEN 4
|                       MOVE 'OPERACAO INDISPONIVEL, SALDO INDISPONIVEL
|     -                 '- REFACA OPERACAO'
|                                      TO OUT-MENSAG
|                                         WRK-MENSAGEM-ERRO
|                       MOVE 'S'       TO WRK-FLAG-ERRO
|                  WHEN OTHER
|                       MOVE 'OPERACAO REJEITADA, REFACA OPERACAO'
|                                      TO OUT-MENSAG
|                                         WRK-MENSAGEM-ERRO
|                       MOVE 'S'       TO WRK-FLAG-ERRO
|             END-EVALUATE
|          ELSE
F2404         PERFORM 2150-ACESSAR-BVVEA520
F2404
F2404         IF  BVVEFX-S-IND-CLI-MASSV  EQUAL 'S'
F2404             PERFORM 2132-ACESSAR-BVVEA504
F2404         ELSE
|                 PERFORM UNTIL WRK-FIM    EQUAL 'S'
|                    PERFORM 2132-1-INCLUIR-TITULO-DESPR
|                    PERFORM 2132-ACESSAR-BVVEA504
|     *
CAP004               IF BVVEND-S-COD-RETORNO  NOT EQUAL ZEROS
 |                      GO TO 2130-99-FIM
CAP004               END-IF
|     *
|                 END-PERFORM
F2404         END-IF
|          END-IF
|          .
|     *
|     *----------------------------------------------------------------*
|      2130-99-FIM.                    EXIT.
CAP001*----------------------------------------------------------------*

CAP001*----------------------------------------------------------------*
|     *             TRATAR LIBERACAO DE APENAS UMA OPERACAO            *
|     *----------------------------------------------------------------*
|      2130-1-TRATAR-LIBERACAO-PFK11   SECTION.
|     *----------------------------------------------------------------*
|     *
|          INITIALIZE                  WRK-OK
|
|          PERFORM 2131-VERIFICAR-PROTOCOLO
|     *
|          MOVE DCOMZD-S-CPROT-SOLTC-AGNDA
|                                   TO WRK-PROTOCOLO
|          MOVE DCOMZD-S-QTEMPO-PROCM-CANAL
|                                   TO HX-NUM-03-CS
|          MOVE HX-NUM-03-SS        TO WRK-TEMPO-ESPERA
|     *
|          ACCEPT WRK-TIME-INI      FROM TIME
|          MOVE WRK-MINUTO-INI      TO WRK-MIN-INI
|          MOVE WRK-SEGUNDOS-INI    TO WRK-SEG-INI
|     *
|          PERFORM UNTIL WRK-SAIR   EQUAL 'S'
|     *
|            ACCEPT WRK-TIME-ATUAL FROM TIME
|            MOVE WRK-MINUTO-ATUAL TO WRK-MIN-ATUAL
|            MOVE WRK-SEGUNDOS-ATUAL
|                                  TO WRK-SEG-ATUAL
|     *
|            COMPUTE WRK-CALC-QTEMPO =
|                                  ((WRK-MIN-ATUAL * 60 + WRK-SEG-ATUAL)
|                                  - (WRK-MIN-INI * 60 + WRK-SEG-INI))
|     *
|            IF WRK-CALC-QTEMPO    GREATER 999
|               MOVE 999           TO WRK-CALC-QTEMPO
|            END-IF
|     *
|            MOVE WRK-CALC-QTEMPO  TO WRK-QTEMPO
|
|            IF WRK-QTEMPO-03      GREATER WRK-TEMPO-ESPERA
|               MOVE 'S'           TO WRK-SAIR
|            END-IF
|          END-PERFORM
|     *
|          PERFORM 2135-ACESSAR-BVVEA501
|     *
|            IF WRK-PROCESSADO        EQUAL 'S'
|               PERFORM 2136-ACESSAR-BVVEA505
|            END-IF
|          .
|     *
|     *----------------------------------------------------------------*
|      2130-1-99-FIM.                    EXIT.
CAP001*----------------------------------------------------------------*

CAP001*----------------------------------------------------------------*
|     *        VERIFICAR SE JA EXISTE PROTOCOLO PARA OPERACAO          *
|     *----------------------------------------------------------------*
|      2131-VERIFICAR-PROTOCOLO        SECTION.
|     *----------------------------------------------------------------*
|     *
|          INITIALIZE                  DCOMZD-REGISTRO
|     *
|          MOVE 'E'                    TO DCOMZD-E-CTPO-PROT-AGNDA
|          MOVE WRK-INP-DANO           TO DCOMZD-E-DANO-OPER-DESC
|          MOVE WRK-INP-NSEQ           TO DCOMZD-E-NSEQ-OPER-DESC
|          MOVE ZEROS                  TO DCOMZD-E-NSMULA-OPER-DESC
|     *
|          MOVE 'DCOM5922'             TO WRK-MODULO
|     *
|          CALL WRK-MODULO             USING DCOMZD-REGISTRO
|                                            ERRO-AREA
|                                            WRK-SQLCA.
|     *
|          EVALUATE DCOMZD-S-COD-RETORNO
|              WHEN 00
|                   IF (DCOMZD-S-CSIT-PROT-AGNDA
|                                      NOT GREATER 5)
|                       MOVE 'S'       TO WRK-ACHOU-SOLTC
|                   ELSE
|                       MOVE 'N'       TO WRK-ACHOU-SOLTC
|                   END-IF
|              WHEN 08
|                   MOVE 'N'           TO WRK-ACHOU-SOLTC
|              WHEN 99
|                   MOVE DCOMZD-S-MSG-RETORNO(1:79)
|                                      TO ERR-TEXTO
|                                         OUT-MENSAG
|                                         WRK-MENSAGEM-ERRO
|                   MOVE 'DB2'         TO ERR-TIPO-ACESSO
|                   PERFORM 9999-PROCESSAR-ROTINA-ERRO
|              WHEN OTHER
|                   MOVE DCOMZD-S-MSG-RETORNO(1:79)
|                                      TO ERR-TEXTO
|                                         OUT-MENSAG
|                                         WRK-MENSAGEM-ERRO
|                   MOVE 'APL'         TO ERR-TIPO-ACESSO
|                   PERFORM 9999-PROCESSAR-ROTINA-ERRO
|          END-EVALUATE
|          .
|     *
|     *----------------------------------------------------------------*
|      2131-99-FIM.                    EXIT.
CAP001*----------------------------------------------------------------*

CAP001*----------------------------------------------------------------*
|     *           ACESAR BVVEA504 PARA SOLICITAR EFETIVACAO            *
|     *----------------------------------------------------------------*
|      2132-ACESSAR-BVVEA504           SECTION.
|     *----------------------------------------------------------------*
|     *
|          INITIALIZE                  BVVEND-COMMAREA
|                                      WRK-CONTRATO-BVVE
|     *
|          MOVE WRK-PROTOCOLO          TO BVVEND-E-PROTOCOLO
|          MOVE OUT-CAMPO2             TO WRK-CAMPO-AG-CC
|          MOVE WRK-AGENCIA            TO BVVEND-E-AGENCIA
|          MOVE WRK-CONTA              TO BVVEND-E-CONTA
|     *
|          EVALUATE LNK-IO-LTERM
|              WHEN 'TELEBCO'
|                   MOVE 'TELE'        TO BVVEND-E-CANAL
|              WHEN 'NETEMPR'
|                   MOVE 'NETE'        TO BVVEND-E-CANAL
|              WHEN 'MOBILE'
|                   MOVE 'MOBI'        TO BVVEND-E-CANAL
|              WHEN OTHER
|                   MOVE 'AGEN'        TO BVVEND-E-CANAL
|          END-EVALUATE
|     *
|          MOVE 'DCOM'                 TO WRK-SIGLA-CCUSTO
|          MOVE WRK-INP-DANO           TO WRK-DANO-OPER
|          MOVE WRK-INP-NSEQ           TO WRK-NSEQ-OPER
|          MOVE WRK-CONTRATO-BVVE      TO BVVEND-E-CONTRATO
|     *
|          MOVE 'I'                    TO BVVEND-E-IND-I-A
|          MOVE OUT-DTVCTOF            TO BVVEND-E-DT-CONT
|          MOVE '.'                    TO BVVEND-E-DT-CONT(3:1)
|                                         BVVEND-E-DT-CONT(6:1)
|          MOVE INP-VLROPER-E          TO BVVEND-E-VLR-TOT
F2404 * ---------------------------------------------------------------*
F2404 * CHAMADA PARA CLIENTES MASSIVOS SERA UNICA E SEM PREENCHIMENTO
F2404 * DAS OCORRENCIAS - IND-CLI-MASSV 'G' = TERMINAL GERENCIAL
F2404
F2404      IF  BVVEFX-S-IND-CLI-MASSV  EQUAL 'S'
F2404          MOVE 'S'                TO BVVEND-E-FIM-OCOR
F2404          MOVE 'G'                TO BVVEND-E-IND-ORIG-MASSV
F2404          MOVE WRK-INP-DANO       TO BVVEND-E-OPER-ANO
F2404          MOVE  WRK-INP-NSEQ      TO BVVEND-E-OPER-SEQ
F2404      ELSE
|              IF WRK-DESPREZAR        EQUAL ZEROS
|                 MOVE 'S'             TO BVVEND-E-FIM-OCOR
|              ELSE
|                 MOVE 'N'             TO BVVEND-E-FIM-OCOR
|              END-IF
|     *
|              PERFORM VARYING IND-1   FROM 1 BY 1
CAP004                   UNTIL IND-1   GREATER 40
|                           OR IND-1   GREATER G8-QTDE-RETORNADA
|                 MOVE G8RT-DVCTO-TITLO-DESC(IND-1)
|                                      TO BVVEND-E-DATA(IND-1)
|                 MOVE G8RT-CADM-CATAO-CREDT(IND-1)
|                                      TO HX-NUM-09-CS
|                 MOVE HX-NUM-09-SS    TO HX-NRO-18
|                 MOVE HX-NRO-05       TO BVVEND-E-PRODUTO(IND-1)
|                 MOVE G8RT-VTITLO-DESC-COML(IND-1)
|                                      TO BVVEND-E-VLR-DESCONTO(IND-1)
|              END-PERFORM
F2404      END-IF.
|     *
|          MOVE 'BVVEA504'             TO WRK-MODULO
|     *
|          CALL WRK-MODULO             USING BVVEND-COMMAREA
|     *
|          EVALUATE BVVEND-S-COD-RETORNO
|              WHEN 00
CAP004              IF WRK-DESPREZAR   EQUAL ZEROS
 |                     IF BVVEND-S-PROTOCOLO
 |                                     EQUAL LOW-VALUES OR SPACES
 |                        MOVE SPACES  TO BVVEND-S-PROTOCOLO
 |                        MOVE 'S'     TO WRK-FLAG-ERRO
 |                        MOVE 'PROTOCOLO RETORNADO EM BRANCO PELO BVVEA
 |    -                        '504'
 |                                     TO OUT-MENSAG
 |                                        WRK-MENSAGEM-ERRO
 |                        MOVE 08      TO BVVEND-S-COD-RETORNO
 |                     END-IF
 |                     MOVE BVVEND-S-PROTOCOLO
 |                                     TO WRK-PROTOCOLO
 |                     MOVE BVVEND-S-TMP-ESPERA
 |                                     TO WRK-TEMPO-ESPERA-X
 |                     PERFORM 2134-CALCULA-TEMPO-CANAL
 |                     MOVE 'I'        TO WRK-ACAO
 |                     MOVE 1          TO WRK-SITUACAO
 |                     PERFORM 2133-INSERIR-CPROT-CANAL
 |                  END-IF
 |                  MOVE BVVEND-S-PROTOCOLO
CAP004                                 TO WRK-PROTOCOLO
|              WHEN 08
CAP005*        ERRO DE CONSISTENCIA
|                   MOVE 'S'           TO WRK-FLAG-ERRO
CAP005              MOVE BVVEND-S-MSG-RETORNO
|                                      TO OUT-MENSAG
|                                         WRK-MENSAGEM-ERRO
|              WHEN 10
|                   MOVE 'S'           TO WRK-FLAG-ERRO
CAP007              MOVE 'EFETIVACAO DE AGENDA NA REGISTRADORA FORA DO H
|     -                  'ORARIO'
|                                      TO OUT-MENSAG
|                                         WRK-MENSAGEM-ERRO
|              WHEN 16
|                   MOVE 'S'           TO WRK-FLAG-ERRO
|                   MOVE 'DB2'         TO ERR-TIPO-ACESSO
|                   MOVE BVVEND-S-MSG-RETORNO
|                                      TO OUT-MENSAG
|                                         WRK-MENSAGEM-ERRO
|              WHEN OTHER
|                   MOVE BVVEND-S-MSG-RETORNO
|                                      TO OUT-MENSAG
|                                         WRK-MENSAGEM-ERRO
|                                         ERR-TEXTO
|                   MOVE 'APL'         TO ERR-TIPO-ACESSO
|                   PERFORM 9999-PROCESSAR-ROTINA-ERRO
|          END-EVALUATE
|          .
|     *
|     *----------------------------------------------------------------*
|      2132-99-FIM.                    EXIT.
CAP001*----------------------------------------------------------------*

CAP001*----------------------------------------------------------------*
|     *     TRATAMENTO PARA TITULOS DESPREZADOS
|     *----------------------------------------------------------------*
|      2132-1-INCLUIR-TITULO-DESPR     SECTION.
|     *----------------------------------------------------------------*
|
|          INITIALIZE G8-ARGUMENTOS-ENTRADA
|                     G8-RETORNO.
|
|          MOVE WRK-INP-DANO           TO G8-DANO-OPER-DESC
|          MOVE WRK-INP-NSEQ           TO G8-NSEQ-OPER-DESC
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
|             MOVE G8-MENSAGEM         TO ERR-TEXTO
|             MOVE 'APL'               TO ERR-TIPO-ACESSO
|             PERFORM 9999-PROCESSAR-ROTINA-ERRO
|          ELSE
|             IF G8-COD-RETORNO        EQUAL  2
CAP004        OR G8-QTDE-RETORNADA     GREATER 40
CAP004           ADD 40                TO WRK-DESPREZAR
|                MOVE 'N'              TO WRK-FIM
|             ELSE
|                MOVE ZEROS            TO WRK-DESPREZAR
|                MOVE 'S'              TO WRK-FIM
|             END-IF
|          END-IF.
|
|     *----------------------------------------------------------------*
|      2132-1-99-FIM.                    EXIT.
CAP001*----------------------------------------------------------------*

CAP001*----------------------------------------------------------------*
|     *    INSERIR OU ATUALIZAR A SITUACAO DO PROTOCOLO NA BASE        *
|     *----------------------------------------------------------------*
|      2133-INSERIR-CPROT-CANAL        SECTION.
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
|          IF WRK-ACAO                 EQUAL 'I'
|     *
|             MOVE 1                   TO DCOMZH-E-QTEMPO-VALDD-PROCM
|             MOVE ZEROS               TO DCOMZH-E-NSMULA-OPER-DESC
|             EVALUATE LNK-IO-LTERM
|                WHEN 'TELEBCO'
|                WHEN 'NETEMPR'
|                WHEN 'MOBILE'
|                     MOVE LNK-IO-LTERM
|                                      TO DCOMZH-E-CCANAL-DESC
|                WHEN OTHER
|                     MOVE 'AGENCIA'   TO DCOMZH-E-CCANAL-DESC
|             END-EVALUATE
|     *
|             MOVE WRK-TEMPO-ESPERA    TO DCOMZH-E-QTEMPO-PROCM-CANAL
|          END-IF
|     *
|          MOVE 237                    TO DCOMZH-E-CBCO
|          MOVE OUT-CAMPO2             TO WRK-CAMPO-AG-CC
|          MOVE WRK-AGENCIA            TO DCOMZH-E-CAG-BCRIA
|          MOVE WRK-CONTA              TO DCOMZH-E-CCTA-BCRIA-CLI
|          MOVE WRK-PROTOCOLO          TO DCOMZH-E-CPROT-SOLTC-AGNDA
|          MOVE 'E'                    TO DCOMZH-E-CTPO-PROT-AGNDA
|          MOVE WRK-INP-DANO           TO DCOMZH-E-DANO-OPER-DESC
|          MOVE WRK-INP-NSEQ           TO DCOMZH-E-NSEQ-OPER-DESC
|          MOVE WRK-ACAO               TO DCOMZH-E-ACAO
|          MOVE WRK-SITUACAO           TO DCOMZH-E-CSIT-PROT-AGNDA
|     *
|          MOVE 'DCOM5923'             TO WRK-MODULO
|     *
|          CALL WRK-MODULO             USING DCOMZH-REGISTRO
|                                            ERRO-AREA
|                                            WRK-SQLCA.
|     *
|          EVALUATE DCOMZH-S-COD-RETORNO
|              WHEN ZEROS
|                   CONTINUE
|              WHEN 99
|                   MOVE DCOMZH-S-MSG-RETORNO(1:79)
|                                      TO ERR-TEXTO
|                                         WRK-MENSAGEM-ERRO
|                   MOVE 'DB2'         TO ERR-TIPO-ACESSO
|                   PERFORM 9999-PROCESSAR-ROTINA-ERRO
|              WHEN OTHER
|                   MOVE DCOMZH-S-MSG-RETORNO(1:79)
|                                      TO ERR-TEXTO
|                                         WRK-MENSAGEM-ERRO
|                   MOVE 'APL'         TO ERR-TIPO-ACESSO
|                   PERFORM 9999-PROCESSAR-ROTINA-ERRO
|          END-EVALUATE
|          .
|     *
|     *----------------------------------------------------------------*
|      2133-99-FIM.                    EXIT.
CAP001*----------------------------------------------------------------*

CAP001*----------------------------------------------------------------*
|     *                   CALCULA TEMPO DE ESPERA                      *
|     *----------------------------------------------------------------*
|      2134-CALCULA-TEMPO-CANAL        SECTION.
|     *----------------------------------------------------------------*
|     *
|          INITIALIZE                  WRK-TEMPO
|     *
|          MOVE WRK-TEMPO-ESPERA-X     TO WRK-TEMPO
|          MOVE WRK-MINUTOS            TO WRK-MINUTOS-COMP
|          MOVE WRK-SEGUNDOS           TO WRK-SEGUNDOS-COMP
|
|          IF WRK-HORA                 GREATER ZEROS
|          OR WRK-MINUTOS              GREATER 16
|             MOVE 999                 TO WRK-TEMPO-ESPERA
|          ELSE
|             IF  WRK-SEGUNDOS         GREATER 40
|             AND WRK-MINUTOS          EQUAL 16
|                 MOVE 999             TO WRK-TEMPO-ESPERA
|             ELSE
|                 COMPUTE WRK-TEMPO-ESPERA = WRK-MINUTOS-COMP * 60
|                 ADD WRK-SEGUNDOS-COMP
|                                      TO WRK-TEMPO-ESPERA
|             END-IF
|          END-IF
|          .
|     *
|     *----------------------------------------------------------------*
|      2134-99-FIM.                    EXIT.
CAP001*----------------------------------------------------------------*

CAP001*----------------------------------------------------------------*
|     *           ACESSAR BVVEA501 PARA OBTER RETORNO                  *
|     *----------------------------------------------------------------*
|      2135-ACESSAR-BVVEA501           SECTION.
|     *----------------------------------------------------------------*
|     *
|          INITIALIZE                  BVVENA-COMMAREA
|                                      WRK-PROCESSADO
|                                      WRK-ACAO
|                                      WRK-SITUACAO
|     *
|          MOVE WRK-PROTOCOLO          TO BVVENA-E-PROTOCOLO
|     *
|          MOVE 'BVVEA501'             TO WRK-MODULO
|     *
|          CALL WRK-MODULO             USING BVVENA-COMMAREA
|     *
|     *TESTAR COMUNICACAO COM A CIP
|     *
|          EVALUATE BVVENA-S-COD-RETORNO
|             WHEN 00
|     *            PROCESSAMENTO OK
|                  MOVE 'S'            TO WRK-SAIR
|                  MOVE 'S'            TO WRK-PROCESSADO
|             WHEN 01
|     *            EM PROCESSAMENTO
|                  MOVE 'S'            TO WRK-FLAG-ERRO
CAP001             MOVE 'U'            TO WRK-ACAO
|                  MOVE 2              TO WRK-SITUACAO
|                  PERFORM 2133-INSERIR-CPROT-CANAL
|                  MOVE 'EFETIVACAO EM PROCESSAMENTO NA REGISTRADORA'
|                                      TO OUT-MENSAG
|                                         WRK-MENSAGEM-ERRO
|             WHEN 08
|     *            ERRO CONSISTENCIA
|                  MOVE 'S'            TO WRK-FLAG-ERRO
CAP001             MOVE 'U'            TO WRK-ACAO
|                  MOVE 6              TO WRK-SITUACAO
|                  PERFORM 2133-INSERIR-CPROT-CANAL
|                  MOVE BVVENA-S-MSG-RETORNO
|                                      TO OUT-MENSAG
|                                         WRK-MENSAGEM-ERRO
|             WHEN 16
|     *            ERRO DB2
|                  MOVE 'S'            TO WRK-FLAG-ERRO
CAP001             MOVE 'U'            TO WRK-ACAO
|                  MOVE 6              TO WRK-SITUACAO
|                  PERFORM 2133-INSERIR-CPROT-CANAL
|                  MOVE BVVENA-S-MSG-RETORNO
|                                      TO OUT-MENSAG
|                                         WRK-MENSAGEM-ERRO
|             WHEN OTHER
CAP001             MOVE 'U'            TO WRK-ACAO
|                  MOVE 5              TO WRK-SITUACAO
|                  PERFORM 2133-INSERIR-CPROT-CANAL
|                  MOVE BVVENA-S-MSG-RETORNO
|                                      TO OUT-MENSAG
|                                         WRK-MENSAGEM-ERRO
|                                         ERR-TEXTO
|                  PERFORM 9999-PROCESSAR-ROTINA-ERRO
|          END-EVALUATE
|          .
|     *
|     *----------------------------------------------------------------*
|      2135-99-FIM.                    EXIT.
CAP001*----------------------------------------------------------------*

CAP001*----------------------------------------------------------------*
|     *           ACESSAR BVVEA505 PARA OBTER RETORNO                  *
|     *----------------------------------------------------------------*
|      2136-ACESSAR-BVVEA505           SECTION.
|     *----------------------------------------------------------------*
|     *
|          INITIALIZE                  BVVENE-COMMAREA
|     *
|          MOVE WRK-PROTOCOLO          TO BVVENE-E-PROTOCOLO
|     *
|          MOVE 'BVVEA505'             TO WRK-MODULO
|     *
|          CALL WRK-MODULO             USING BVVENE-COMMAREA
|     *
|          EVALUATE BVVENE-S-COD-RETORNO
|             WHEN 00
|     *            PROCESSAMENTO OK
CAP001             MOVE 'U'            TO WRK-ACAO
|                  MOVE 1              TO WRK-SITUACAO
|                  MOVE 'S'            TO WRK-OK
|                  PERFORM 2133-INSERIR-CPROT-CANAL
|             WHEN 08
|     *            ERRO CONSISTENCIA
|                  MOVE 'S'            TO WRK-FLAG-ERRO
CAP001             MOVE 'U'            TO WRK-ACAO
|                  MOVE 6              TO WRK-SITUACAO
|                  PERFORM 2133-INSERIR-CPROT-CANAL
CAP005             MOVE BVVENE-S-MSG-RETORNO
|                                      TO OUT-MENSAG
|                                         WRK-MENSAGEM-ERRO
|             WHEN 09
|     *            SALDO INDISPONIVEL
|                  MOVE 'S'            TO WRK-FLAG-ERRO
CAP001             MOVE 'U'            TO WRK-ACAO
|                  MOVE 4              TO WRK-SITUACAO
|                  PERFORM 2133-INSERIR-CPROT-CANAL
|                  MOVE 'OPERACAO REJEITADA - SALDO INDISPONIVEL'
|                                      TO OUT-MENSAG
|                                         WRK-MENSAGEM-ERRO
|             WHEN 11
|     *            ERRO DE COMUNICACAO COM A CIP
|                  MOVE 'S'            TO WRK-FLAG-ERRO
CAP001             MOVE 'U'            TO WRK-ACAO
|                  MOVE 6              TO WRK-SITUACAO
|                  PERFORM 2133-INSERIR-CPROT-CANAL
|                  MOVE 'OPERACAO REJEITADA - ERRO DE COMUNICACAO COM A
|     -                 'REGISTRADORA'
|                                      TO OUT-MENSAG
|                                         WRK-MENSAGEM-ERRO
|             WHEN 13
|     *            SOLICITACAO REJEITADA
|                  MOVE 'S'            TO WRK-FLAG-ERRO
CAP001             MOVE 'U'            TO WRK-ACAO
|                  MOVE 5              TO WRK-SITUACAO
|                  PERFORM 2133-INSERIR-CPROT-CANAL
|                  MOVE 'OPERACAO REJEITADA PELA REGISTRADORA'
|                                      TO OUT-MENSAG
|                                         WRK-MENSAGEM-ERRO
|             WHEN 16
|     *            ERRO DB2
|                  MOVE 'S'            TO WRK-FLAG-ERRO
CAP001             MOVE 'U'            TO WRK-ACAO
|                  MOVE 6              TO WRK-SITUACAO
|                  PERFORM 2133-INSERIR-CPROT-CANAL
|                  MOVE BVVENE-S-MSG-RETORNO
|                                      TO OUT-MENSAG
|                                         WRK-MENSAGEM-ERRO
|             WHEN OTHER
CAP001             MOVE 'U'            TO WRK-ACAO
|                  MOVE 5              TO WRK-SITUACAO
|                  PERFORM 2133-INSERIR-CPROT-CANAL
|                  MOVE BVVENE-S-MSG-RETORNO
|                                      TO OUT-MENSAG
|                                         WRK-MENSAGEM-ERRO
|                                         ERR-TEXTO
|                  MOVE 'APL'          TO ERR-TIPO-ACESSO
|                  PERFORM 9999-PROCESSAR-ROTINA-ERRO
|          END-EVALUATE
|          .
|     *
|     *----------------------------------------------------------------*
|      2136-99-FIM.                    EXIT.
CAP001*----------------------------------------------------------------*

F2404 *----------------------------------------------------------------*
INICIO 2150-ACESSAR-BVVEA520           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE BVVEFX-COMMAREA

           MOVE OUT-CAMPO2             TO WRK-CAMPO-AG-CC
           MOVE WRK-AGENCIA            TO BVVEFX-E-AGENCIA
           MOVE WRK-CONTA              TO BVVEFX-E-CONTA

           MOVE 'BVVEA520'             TO WRK-MODULO
           CALL WRK-MODULO             USING BVVEFX-COMMAREA

           EVALUATE BVVEFX-S-COD-RETORNO
               WHEN 00
                    CONTINUE

               WHEN OTHER
                    MOVE 'N'           TO BVVEFX-S-IND-CLI-MASSV
                    MOVE BVVEFX-S-MSG-RETORNO
                                       TO OUT-MENSAG
                                          WRK-MENSAGEM-ERRO
           END-EVALUATE.

      *----------------------------------------------------------------*
FIM    2150-99-FIM.                    EXIT.
F2404 *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2200-DEVOLVER-TELA              SECTION.
      *----------------------------------------------------------------*

           MOVE  INP-TRANSAC                    TO OUT-TRANSAC
           MOVE  INP-TITULO                     TO OUT-TITULO
           MOVE  INP-FLAGENT                    TO OUT-FLAGENT
           MOVE  INP-FASE                       TO OUT-FASE
           MOVE  INP-SENHAS                     TO OUT-SENHAS
           MOVE  INP-VLROPER-X                  TO OUT-VLROPER
           MOVE  INP-TPOTAXA                    TO OUT-TPOTAXA
           MOVE  INP-VLTAROP-X                  TO OUT-VLTAROP
           MOVE  INP-TAXAJUR                    TO OUT-TAXAJUR
           MOVE  INP-NMJUROS                    TO OUT-NMJUROS
           MOVE  INP-VLTRFTT-X                  TO OUT-VLTRFTT
           MOVE  INP-DTOPER                     TO OUT-DTOPER
           MOVE  INP-DTVCTOF                    TO OUT-DTVCTOF
           MOVE  INP-VLRJUR-X                   TO OUT-VLRJUR
           MOVE  INP-PRZMED                     TO OUT-PRZMED
           MOVE  INP-VLRIOF-X                   TO OUT-VLRIOF
           MOVE  INP-QTDETIT                    TO OUT-QTDETIT
           MOVE  INP-VLRLIQD-X                  TO OUT-VLRLIQD
BRQ002     MOVE  INP-LIBMID                     TO OUT-LIBMID
           MOVE  INP-GARANTI                    TO OUT-GARANTI
           MOVE  INP-AUTRZTE                    TO OUT-AUTRZTE
           MOVE  INP-DESCR1                     TO OUT-DESCR1
           MOVE  INP-DESCR2                     TO OUT-DESCR2
           MOVE  INP-JURMAXI-E                  TO OUT-JURMAXI-N
           MOVE  INP-VLRMAXI-E                  TO OUT-VLRMAXI-N
           IF  INP-JRLIBER-X     EQUAL SPACES OR LOW-VALUES
               MOVE SPACES                      TO OUT-JRLIBER
           ELSE
               MOVE INP-JRLIBER-X               TO OUT-JRLIBER
           END-IF
           IF  INP-VLLIBER-X     EQUAL SPACES OR LOW-VALUES
               MOVE SPACES                      TO OUT-VLLIBER
           ELSE
               MOVE INP-VLLIBER-X               TO OUT-VLLIBER
           END-IF
           MOVE  INP-TMSTAMP                    TO OUT-TMSTAMP
           MOVE  INP-NOME                       TO OUT-NOME
BRQ001     MOVE  WRK-INP-CANAL                  TO WRK-OUT-CANAL
           MOVE  INP-CNPJCPF                    TO OUT-CNPJCPF
           MOVE  INP-CAMPO2                     TO OUT-CAMPO2
           MOVE  INP-PRODUTO                    TO OUT-PRODUTO
           MOVE  INP-DPRODUT                    TO OUT-DPRODUT
           MOVE  INP-SPRODUT                    TO OUT-SPRODUT
           MOVE  INP-DSPRODU                    TO OUT-DSPRODU
           MOVE  INP-OPERAC                     TO OUT-OPERAC
           MOVE  INP-SITUAC                     TO OUT-SITUAC
           MOVE  INP-CAMPO3                     TO OUT-CAMPO3
           MOVE  INP-CAMPO4                     TO OUT-CAMPO4-X
           MOVE  INP-TIPOLIB                    TO OUT-TIPOLIB
           MOVE  INP-TIPODES                    TO OUT-TIPODES
           MOVE  INP-LITPF9A                    TO OUT-LITPF9A
           MOVE  INP-LITPF9B                    TO OUT-LITPF9B
           MOVE  INP-LITPF11A                   TO OUT-LITPF11A
           MOVE  INP-LITPF11B                   TO OUT-LITPF11B.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2300-RETORNAR-CHAMADOR          SECTION.
      *----------------------------------------------------------------*

           INITIALIZE COMU-AREA-UN.

           MOVE LENGTH OF COMU-AREA-UN TO  COMU-LL-UN.
           MOVE ZEROS                  TO  COMU-ZZ-UN.

           PERFORM 1105-LER-DBTEMP.

           MOVE WRK-DADOS-DBTEMP       TO COMU-DADOS-UN

           PERFORM 1106-APAGAR-DBTEMP.

           MOVE WRK-CHNG               TO  WRK-FUNCAO.
           MOVE 'DCOM0405'             TO COMU-TRANSACAO-UN

           MOVE WRK-CHAMADOR-DBTEMP    TO  WRK-TELA

           IF  WRK-FLAG-ERRO           EQUAL 'S'
               MOVE WRK-MENSAGEM-ERRO  TO COMU-MENSAGEM-UN
               IF  WRK-CHAMADOR-DBTEMP EQUAL 'DCOM0402'
               AND INP-PFK             NOT EQUAL '6'
                   MOVE INP-OPERAC     TO COMU-OPER-UN
                   MOVE 1              TO COMU-OPCAO-UN
               END-IF
           ELSE
               INITIALIZE COMU-OPER-UN
                          COMU-OPCAO-UN
           END-IF

           MOVE  COMU-AREA-UN          TO  WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2400-MENU-ROTINAS               SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                      COMU-AREA-PFK.
           MOVE  +124                  TO  COMU-LL-PFK.
           MOVE  ZEROS                 TO  COMU-ZZ-PFK.
           MOVE  WRK-CHNG              TO  WRK-FUNCAO.
           MOVE  'DCOM0405'            TO  WRK-TELA
                                           COMU-TRANSACAO-PFK.
           MOVE  '5'                   TO  COMU-PFK1.
           MOVE  COMU-AREA-PFK         TO  WRK-MENSAGEM.

           PERFORM 1106-APAGAR-DBTEMP.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2500-MENU-DCOM                  SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                      COMU-AREA-PFK.

           MOVE  +124                  TO  COMU-LL-PFK.
           MOVE  ZEROS                 TO  COMU-ZZ-PFK.
           MOVE  WRK-CHNG              TO  WRK-FUNCAO.
           MOVE  'DCOM0405'            TO  WRK-TELA
                                           COMU-TRANSACAO-PFK.
           MOVE  'A'                   TO  COMU-PFK1.
           MOVE  COMU-AREA-PFK         TO  WRK-MENSAGEM.

           PERFORM 1106-APAGAR-DBTEMP.

      *----------------------------------------------------------------*
       2500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2600-TRATAR-ENTER               SECTION.
      *----------------------------------------------------------------*

           INITIALIZE 4132E-ROTEADOR
                      4132S-ROTEADOR.

           MOVE  SPACES                TO WRK-INCONSIS.

           IF  (INP-JRLIBER-X      EQUAL SPACES  OR  LOW-VALUES)AND
              (INP-VLLIBER-X       EQUAL SPACES  OR  LOW-VALUES)
               MOVE 'DIGITAR UMA DAS OPCOES E TECLE ENTER'
                                       TO OUT-MENSAG
               MOVE '*'                TO WRK-INCONSIS
               MOVE WRK-49369          TO OUT-ATTR-VLLIBER
                                          OUT-ATTR-JRLIBER
               GO                      TO 2600-99-FIM
           END-IF.

           IF  (INP-JRLIBER-X     NOT EQUAL SPACES AND LOW-VALUES)
           AND  INP-JRLIBER-E     NOT EQUAL ZEROS
               MOVE INP-JRLIBER-E     TO WRK-VALOR-0496
               MOVE 06050302          TO WRK-TAMANHO-0496
               MOVE SPACES            TO WRK-SAIDA-0496
               CALL 'BRAD0496'        USING WRK-VALOR-0496
                                            WRK-SAIDA-0496
                                            WRK-TAMANHO-0496
               IF  RETURN-CODE        NOT EQUAL ZEROS
                   MOVE '*'           TO WRK-INCONSIS
                   MOVE 'PERCENTUAL A LIBERAR INVALIDO'
                                      TO OUT-MENSAG
                   MOVE WRK-49369     TO OUT-ATTR-JRLIBER
                   GO TO 2600-99-FIM
               END-IF
               MOVE WRK-SAIDA-0496-N  TO OUT-JRLIBER-N
                                         4132E-PERC-INFORMADO
               PERFORM 2610-CALCULAR-VALOR
               MOVE 4132S-VLIBRC-ANTCP-OPER    TO OUT-VLLIBER-N
           ELSE
               MOVE INP-VLLIBER-E  TO WRK-VALOR-0496-2
               MOVE '17161402'     TO WRK-TAMANHO-0496-2
               MOVE SPACES         TO WRK-SAIDA-0496-2
               CALL 'BRAD0496'             USING WRK-VALOR-0496-2
                                                 WRK-SAIDA-0496-2
                                                 WRK-TAMANHO-0496-2
               IF  RETURN-CODE        NOT EQUAL ZEROS
                   MOVE '*'           TO WRK-INCONSIS
                   MOVE 'VALOR A LIBERAR INVALIDO'
                                      TO OUT-MENSAG
                   MOVE WRK-49369     TO OUT-ATTR-VLLIBER
                   GO TO 2600-99-FIM
               END-IF
               MOVE WRK-SAIDA-0496-2-N TO OUT-VLLIBER-N
                                          WRK-VLRLIBR-AUX
                                          4132E-VALOR-INFORMADO
               MOVE INP-VLRMAXI-E      TO WRK-VLRMAXI-AUX
               PERFORM 2610-CALCULAR-VALOR
               MOVE 4132S-PLIBRC-ANTCP-OPER    TO OUT-JRLIBER-N
               IF  WRK-VLRMAXI-AUX     LESS WRK-VLRLIBR-AUX
                   MOVE 'VALOR A LIBERAR NAO PODE SER MAIOR QUE VALOR MA
      -             'XIMO'             TO OUT-MENSAG
                   MOVE '*'            TO WRK-INCONSIS
                   MOVE WRK-49369      TO OUT-ATTR-VLLIBER
                   GO                  TO 2600-99-FIM
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2610-CALCULAR-VALOR             SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-COD-USER           TO 4132E-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO 4132E-CTERM
           MOVE INP-OPERAC(1:4)        TO 4132E-DANO-OPER-DESC
           MOVE INP-OPERAC(5:9)        TO 4132E-NSEQ-OPER-DESC
           MOVE 'DCOM4132'             TO WRK-MODULO
           CALL WRK-MODULO             USING 4132E-ROTEADOR
                                             4132S-ROTEADOR
                                             ERRO-AREA
                                             WRK-SQLCA.

           IF  4132S-COD-RETORNO       NOT EQUAL '0000'
               IF  4132S-COD-RETORNO   EQUAL '0099'
                   MOVE 'DB2'          TO ERR-TIPO-ACESSO
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO
               ELSE
                   MOVE WRK-49369      TO OUT-ATTR-JRLIBER
                   MOVE '*'            TO WRK-INCONSIS
                   MOVE 4132S-MSG-RETORNO
                                       TO OUT-MENSAG
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2610-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2800-ACESSA-DCOM0214            SECTION.
      *----------------------------------------------------------------*

           INITIALIZE COMU-AREA-0214.

           MOVE LENGTH OF COMU-AREA-0214    TO COMU-LL-0214
           MOVE ZEROS                       TO COMU-ZZ-0214
           MOVE WRK-CHNG                    TO WRK-FUNCAO

           MOVE INP-TIPODES                 TO COMU-TPDESC-0214
           MOVE INP-TMSTAMP                 TO COMU-TIMESTAMP-0214
           MOVE INP-OPERAC                  TO COMU-OPERACAO-0214
           MOVE INP-CAMPO2(5:5)             TO COMU-AGENCIA-0214
           MOVE INP-CAMPO2(11:13)           TO COMU-CONTA-0214
           STRING INP-CNPJCPF(1:3)
                  INP-CNPJCPF(5:3)
                  INP-CNPJCPF(9:3)        DELIMITED BY SIZE
                                          INTO COMU-CPFCNPJ-0214
           MOVE INP-CNPJCPF(13:4)           TO COMU-FILIAL-0214
           MOVE INP-CNPJCPF(18:2)           TO COMU-CONTROLE-0214
           MOVE 'DCOM0405'                  TO COMU-TRANSACAO-0214
           MOVE 'DCOM0214'                  TO WRK-TELA
           MOVE WRK-CHNG                    TO WRK-FUNCAO
           MOVE COMU-AREA-0214              TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2900-ACESSA-DCOM0222            SECTION.
      *----------------------------------------------------------------*

           INITIALIZE COMU-AREA-0215.

           MOVE LENGTH OF COMU-AREA-0215    TO COMU-LL-0215
           MOVE INP-TIPODES                 TO COMU-TPDESC-0215
           MOVE INP-TMSTAMP                 TO COMU-TIMESTAMP-0215
           MOVE INP-OPERAC                  TO COMU-OPERACAO-0215
           MOVE 'DCOM0405'                  TO COMU-TRANSACAO-0215
           MOVE 'DCOM0222'                  TO WRK-TELA
           MOVE WRK-CHNG                    TO WRK-FUNCAO
           MOVE COMU-AREA-0215              TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

PROC01*----------------------------------------------------------------*
PROC01 2950-ACESSAR-CET                SECTION.
PROC01*----------------------------------------------------------------*
PROC01
PROC01     INITIALIZE COMU-AREA-UN.
PROC01
PROC01     MOVE LENGTH OF COMU-AREA-UN TO  COMU-LL-UN.
PROC01     MOVE ZEROS                  TO  COMU-ZZ-UN.
PROC01
PROC01     PERFORM 1105-LER-DBTEMP.
PROC01
PROC01     MOVE WRK-DADOS-DBTEMP       TO  COMU-DADOS-UN
PROC01
PROC01     PERFORM 1106-APAGAR-DBTEMP.
PROC01
PROC01     MOVE WRK-CHNG               TO  WRK-FUNCAO.
PROC01     MOVE 'DCOM3924'             TO  WRK-TELA
PROC01     MOVE 'DCOM0405'             TO  COMU-TRANSACAO-UN
PROC01     MOVE WRK-CHAMADOR-DBTEMP    TO  COMU-CHAMADOR-UN.
PROC01
PROC01     MOVE  COMU-AREA-UN          TO  WRK-MENSAGEM.
PROC01
PROC01*----------------------------------------------------------------*
PROC01 2950-99-FIM.                    EXIT.
PROC01*----------------------------------------------------------------*

CAP006*----------------------------------------------------------------*
.     *    OBTER SITUACAO DO PROTOCOLO                                 *
.     *----------------------------------------------------------------*
.      2955-OBTER-SIT-PROTOC           SECTION.
.     *----------------------------------------------------------------*
.
.          INITIALIZE DCOMZD-BLOCO-ENTRADA
.                     DCOMZD-RETORNO
.
.          MOVE 'E'                    TO DCOMZD-E-CTPO-PROT-AGNDA.
.          MOVE WRK-INP-DANO           TO DCOMZD-E-DANO-OPER-DESC.
.          MOVE WRK-INP-NSEQ           TO DCOMZD-E-NSEQ-OPER-DESC.
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
.                            MOVE 'EM LIBERAC. CIP'
.                                      TO OUT-SITUAC
.                       WHEN 3
.                            MOVE  4174-SAI-IRSUMO-SIT-DESC
.                                      TO OUT-SITUAC
.                       WHEN 4
.                            MOVE 'VLR. INDISP.'
.                                      TO OUT-SITUAC
.                       WHEN OTHER
.                            MOVE 'REJEITADO CIP'
.                                      TO OUT-SITUAC
.                   END-EVALUATE
.
.              WHEN 08
.                   MOVE  4174-SAI-IRSUMO-SIT-DESC
.                                      TO OUT-SITUAC
.              WHEN 99
.                   MOVE DCOMZD-S-MSG-RETORNO(1:79)
.                                      TO ERR-TEXTO
.                                         OUT-MENSAG
.                                         WRK-MENSAGEM-ERRO
.                   MOVE 'DB2'         TO ERR-TIPO-ACESSO
.                   PERFORM 9999-PROCESSAR-ROTINA-ERRO
.              WHEN OTHER
.                   MOVE DCOMZD-S-MSG-RETORNO(1:79)
.                                      TO ERR-TEXTO
.                                         OUT-MENSAG
.                                         WRK-MENSAGEM-ERRO
.                   MOVE 'APL'         TO ERR-TIPO-ACESSO
.                   PERFORM 9999-PROCESSAR-ROTINA-ERRO
.          END-EVALUATE.
.
.     *----------------------------------------------------------------*
.      2955-99-FIM.                    EXIT.
CAP006*----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       3000-PROCESSAR-BRAD0660         SECTION.
      *----------------------------------------------------------------*

           CALL  'BRAD0660'            USING  OUTPUT-DETM
                                              660-DCOMDETM.

           IF RETURN-CODE              NOT EQUAL  ZEROS
              MOVE  'APL'              TO  ERR-TIPO-ACESSO
              MOVE  RETURN-CODE        TO  WRK-RETURN-CODE
              MOVE  10                 TO  WRK-LOCAL-ERRO
              MOVE  WRK-ERRO-BRAD0660  TO  ERR-TEXTO
              PERFORM  9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE  WRK-ISRT              TO  WRK-FUNCAO.
           MOVE  OUTPUT-DETM           TO  WRK-MENSAGEM.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       8888-EFETUAR-ROLLBACK           SECTION.
      *----------------------------------------------------------------*

            CALL 'CBLTDLI'             USING WRK-ROLB
                                             LNK-IO-PCB.

      *----------------------------------------------------------------*
       8888-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           IF ERR-MODULO    NOT EQUAL  SPACES  AND  LOW-VALUES
              MOVE  'DCOM0405'         TO  ERR-PGM
           ELSE
              MOVE  'SENHAS02'         TO  ERR-PGM
              MOVE  'DCOM0405'         TO  ERR-MODULO
           END-IF.

           MOVE  WRK-COD-USER          TO  ERR-COD-USER.
           MOVE  WRK-COD-DEPTO         TO  ERR-COD-DEPTO.

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
