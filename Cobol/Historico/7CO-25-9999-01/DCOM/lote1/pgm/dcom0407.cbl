      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. DCOM0407.
       AUTHOR.     DIEGO MARCEL ZANOTTO
      *================================================================*
      *                          A L T R A N                           *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   DCOM0407                                    *
      *    PROGRAMADOR.:   DIEGO MARCEL ZANOTTO                        *
      *    ANALISTA....:   AQUIRA H.                                   *
      *    DATA........:   01/02/2007                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   CANCELAR LIBERACAO DA OPERACAO - OPERACOES  *
      *                    POR CLIENTES.                               *
      *----------------------------------------------------------------*
      *    TELAS.......:                                               *
      *    DCOMDGTM - CANCELAR LIBERACAO DA OPERACAO - OPERACOES       *
      *               POR CLIENTES.                                   *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    I#BRAD7C - AREA DE ERRO DA BRAD7100.                        *
      *    I#DCOMSG - TABELA DE MSG DE RETORNO DO SISTEMA DCOM(ONLINE).*
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    BRAD0660 - INSERIR CARACTERES DE NULL NA MENSAGEM DE SAIDA  *
      *    BRAD7100 - TRATAMENTO DE ERROS QUANDO PROGRAMA INVALIDO     *
      *    POOL5005 - CONTROLE DO TRANSITO DE MENSAGENS NO IMS/DC      *
      *----------------------------------------------------------------*
      *    NAVEGACAO...:                                               *
      *    CHAMADO POR.: DCOM0402           CHAMA: DCOM0408            *
      *================================================================*
DTS001*                    ALTERACAO - DTS CONSULTING                  *
DTS001*----------------------------------------------------------------*
DTS001*    PROGRAMADORA:   LUCIANDRA SILVEIRA - DTS CONSULTING         *
DTS001*    ANALISTA....:   LUCIANDRA SILVEIRA - DTS CONSULTING         *
DTS001*    ANALISTA DTS:   VERA MARIA POLLINI - BRADESCO - GRUPO 70    *
DTS001*    DATA........:   05/02/2010                                  *
DTS001*----------------------------------------------------------------*
DTS001*    OBJETIVO....:   EXECUTAR ROLLBACK CASO OCORRA ERRO NO MOMEN-*
DTS001*                    TO DO CANCELAMENTO DA LIBERACAO DA OPERACAO *
DTS001*================================================================*
DTS002*                   ALTERACAO  -  DTS  CONSULTING                *
DTS002*----------------------------------------------------------------*
DTS002*     PROGRAMADORA: LUCIANDRA SILVEIRA  -  DTS CONSULTING        *
DTS002*     ANALISTA....: LUCIANDRA SILVEIRA  -  DTS CONSULTING        *
DTS002*     ANALISTA DDS: EDGARD R. DE SOUZA  -  GRUPO 70              *
DTS002*     DATA........: 18/03/2011                                   *
DTS002*----------------------------------------------------------------*
DTS002*     OBJETIVO....: CORRECAO PARA EFETUAR COMMIT A CADA OPERACAO *
DTS002*                   LIBERADA PELA LISTA.                         *
DTS002*----------------------------------------------------------------*
DTS002*    NAVEGACAO...:                                               *
DTS002*    CHAMADO POR.:  DCOM0499                                     *
DTS002*================================================================*
7C0922*================================================================*
7C0922*                   ALTERACAO  -  7COMM                          *
7C0922*----------------------------------------------------------------*
7C0922*    PROGRAMADOR.:  EDSON MATSUMOTO    - 7COMM                   *
7C0922*    ANALISTA    :  EDSON MATSUMOTO    - 7COMM                   *
7C0922*    ANALISTA DS.:  PAULO ROBERTO SARTORATO - BRADESCO           *
7C0922*    DATA........:  12/09/2022                                   *
7C0922*----------------------------------------------------------------*
7C0922*    OBJETIVO....:  PROJETO 794794 - CANCELAR CONTRATO (COR002)  *
7C0922*                   NAO PERMTIR CANCELAR CONTRATO NPR SE JA TIVER*
7C0922*                   RETORNO DA COR0005R1                         *
7C0922*================================================================*


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
           '*** DCOM0407 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ATRIBUTOS ***'.
      *---------------------------------------------------------------*
      *--- NUMERICO - NORMAL    - PROTEGIDO    - NAO CURSOR - NUM  ---*
       01  WRK-241                     PIC S9(008) COMP    VALUE +241.
      *--- ALPHA    - NORMAL    - PROTEGIDO    - NAO CURSOR - ALFA ---*
       01  WRK-225                     PIC S9(008) COMP    VALUE +225.
      *--- NUMERICO - NORMAL    - PROTEGIDO    - POS CURSOR - NUM  ---*
       01  WRK-49361                   PIC S9(008) COMP    VALUE +49361.
      *--- NUMERICO - BRILHANTE - DESPROTEGIDO - POS CURSOR - NUM  ---*
       01  WRK-49369                   PIC S9(008) COMP    VALUE +49369.
      *--- BRILHANTE, NORMAL    - DEPROTEGIDO  - POS CURSOR - ALFA ---*
       01  WRK-49353                   PIC S9(008) COMP    VALUE +49353.
      *--- NUMERICO - NORMAL    - DEPROTEGIDO  - POS CURSOR - ALFA ---*
       01  WRK-49345                   PIC S9(008) COMP    VALUE +49345.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*
       01  WRK-AUXILIARES.
           05  WRK-SELEC-ANT           PIC  X(001)         VALUE SPACES.
           05  WRK-IND                 PIC  9(002)         VALUE ZEROS.
7C0922     05  WRK-IND1                PIC  9(002) COMP-3  VALUE ZEROS.
           05  WRK-SQLCA               PIC  X(136)         VALUE SPACES.
           05  WRK-MODULO              PIC  X(008)         VALUE SPACES.
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

       01  WRK-CNPJCPF.
FS2511*    05  WRK-NUMERO              PIC  9(009)         VALUE ZEROS.
FS2511     05  WRK-NUMERO              PIC  X(009)         VALUE SPACES.
           05  WRK-FILLER1             PIC  X(001)         VALUE SPACES.
FS2511*    05  WRK-FILIAL              PIC  9(004)         VALUE ZEROS.
FS2511     05  WRK-FILIAL              PIC  X(004)         VALUE SPACES.
           05  WRK-FILLER2             PIC  X(001)         VALUE SPACES.
           05  WRK-CONTROLE            PIC  9(002)         VALUE ZEROS.

       01  WRK-AGENCIA-NOME.
           05  WRK-COD-AGE             PIC  9(005)         VALUE ZEROS.
           05  WRK-FILLER3             PIC  X(001)         VALUE SPACES.
           05  WRK-DESC-AGE            PIC  X(020)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA ACUMULADORES ***'.
      *----------------------------------------------------------------*

       01  ACU-ACUMULADORES.
           05  ACU-SEL-D               PIC  9(002)         VALUE ZEROS.

      *----------------------------------------------------------------*
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
           '*** AREA DA TELA DCOMDGTM  - INPUT ***'.
      *----------------------------------------------------------------*
       01  WRK-INPUT-DGTM.
           05  FILLER                      PIC X(018)  VALUE SPACES.
           05  WRK-INP-PFK-DGTM            PIC X(001)  VALUE SPACES.
           05  WRK-INP-SENHAS-DGTM         PIC X(037)  VALUE SPACES.
           05  WRK-INP-COMANDO-DGTM        PIC X(068)  VALUE SPACES.
           05  WRK-INP-DADOS-DGTM.
               10  WRK-INP-FIMAMOS-DGTM    PIC X(001)  VALUE SPACES.
               10  WRK-INP-FASE-DGTM       PIC X(001)  VALUE SPACES.
               10  WRK-INP-TMSTAMP-DGTM    PIC X(026)  VALUE SPACES.
               10  WRK-INP-PAGINA-DGTM-X.
                   15  WRK-INP-PAGINA-DGTM
                                           PIC 9(003)  VALUE ZEROS.
               10  WRK-INP-NOME-DGTM       PIC X(040)  VALUE SPACES.
               10  WRK-INP-LITCPF-DGTM     PIC X(009)  VALUE SPACES.
               10  WRK-INP-CNPJCPF-DGTM.
FS2511*             15  WRK-INP-NUMERO-DGTM PIC 9(09) VALUE ZEROS.
FS2511             15  WRK-INP-NUMERO-DGTM PIC X(09) VALUE SPACES.
                   15  WRK-INP-BARRA-CNPJ  PIC X(01) VALUE SPACES.
FS2511*             15  WRK-INP-FILIAL-DGTM PIC 9(04) VALUE ZEROS.
FS2511             15  WRK-INP-FILIAL-DGTM PIC X(04) VALUE SPACES.
                   15  WRK-INP-TRACO-CNPJ  PIC X(01) VALUE SPACES.
                   15  WRK-INP-CONTROLE-DGTM
                                           PIC 9(02) VALUE ZEROS.
               10  WRK-INP-LITBCO-DGTM     PIC X(006) VALUE SPACES.
               10  WRK-INP-BANCO-DGTM-X.
                   15  WRK-INP-BANCO-DGTM  PIC 9(003)  VALUE ZEROS.
               10  WRK-INP-LITAGE-DGTM     PIC X(008) VALUE SPACES.
               10  WRK-INP-AGENCIA-DGTM-X.
                   15  WRK-INP-CODAGE-DGTM PIC 9(004)  VALUE ZEROS.
                   15  FILLER              PIC X(001)  VALUE SPACES.
                   15  WRK-INP-DESCAGE-DGTM
                                           PIC X(021)  VALUE SPACES.
               10  WRK-INP-LITCTA-DGTM     PIC X(009) VALUE SPACES.
               10  WRK-INP-CONTA-DGTM-X.
                   15  WRK-INP-CONTA-DGTM  PIC 9(013)  VALUE ZEROS.
               10  WRK-INP-TABELA-DGTM     OCCURS  04  TIMES.
                   15  WRK-INP-SELEC-DGTM     PIC  X(001) VALUE SPACES.
                   15  WRK-INP-OPERA-DGTM-X.
                       20  WRK-INP-OPERA-DGTM PIC  9(013) VALUE ZEROS.
                   15  WRK-INP-OPERA-DGTM-R REDEFINES
                                  WRK-INP-OPERA-DGTM-X.
                       20  WRK-INP-OPERA-DANO PIC  9(04).
                       20  WRK-INP-OPERA-NSEQ PIC  9(09).
                   15  WRK-INP-PROD-DGTM      PIC  X(012) VALUE SPACES.
                   15  WRK-INP-SUBPROD-DGTM   PIC  X(010) VALUE SPACES.
                   15  WRK-INP-DTOPER-DGTM    PIC  X(010) VALUE SPACES.
                   15  WRK-INP-VALOR-DGTM     PIC  X(017) VALUE SPACES.
                   15  WRK-INP-MEIENT-DGTM    PIC  X(015) VALUE SPACES.
                   15  WRK-INP-SITANAL-DGTM   PIC  X(010) VALUE SPACES.
                   15  WRK-INP-PARECE-DGTM    PIC  X(003) VALUE SPACES.
           05  WRK-INP-MENSA-DGTM       PIC  X(079) VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                       PIC  X(050)         VALUE
           '*** AREA DA TELA DCOMDGTM  - OUTPUT ***'.
      *----------------------------------------------------------------*
       01  WRK-OUTPUT-DGTM.
           05  WRK-OUT-LL-DGTM             PIC S9(004) COMP VALUE +0724.
           05  WRK-OUT-ZZ-DGTM             PIC 9(004)  COMP VALUE ZEROS.
           05  WRK-OUT-SENHAS-DGTM         PIC X(037)  VALUE SPACES.
           05  WRK-OUT-COMANDO-ATTR-DGTM   PIC 9(004) COMP VALUE ZEROS.
           05  WRK-OUT-COMANDO-DGTM        PIC X(068)  VALUE SPACES.
           05  WRK-OUT-DADOS-DGTM.
               10  WRK-OUT-FIMAMOS-DGTM    PIC X(001)  VALUE SPACES.
               10  WRK-OUT-FASE-DGTM       PIC X(001)  VALUE SPACES.
               10  WRK-OUT-TMSTAMP-DGTM    PIC X(026)  VALUE SPACES.
               10  WRK-OUT-PAGINA-DGTM-X.
                   15  WRK-OUT-PAGINA-DGTM
                                           PIC 9(003)  VALUE ZEROS.
               10  WRK-OUT-NOME-DGTM       PIC X(040)  VALUE SPACES.
               10  WRK-OUT-LITCPF-DGTM     PIC X(009)  VALUE SPACES.
               10  WRK-OUT-CNPJCPF-DGTM    PIC X(017)  VALUE SPACES.
               10  WRK-OUT-LITBCO-DGTM     PIC X(006) VALUE SPACES.
               10  WRK-OUT-BANCO-DGTM-X.
                   15  WRK-OUT-BANCO-DGTM  PIC 9(003)  VALUE ZEROS.
               10  WRK-OUT-LITAGE-DGTM     PIC X(008) VALUE SPACES.
               10  WRK-OUT-AGENCIA-DGTM-X.
                   15  WRK-OUT-CODAGE-DGTM PIC 9(004)  VALUE ZEROS.
                   15  FILLER              PIC X(001)  VALUE SPACES.
                   15  WRK-OUT-DESCAGE-DGTM
                                           PIC X(021)  VALUE SPACES.
               10  WRK-OUT-LITCTA-DGTM     PIC X(009) VALUE SPACES.
               10  WRK-OUT-CONTA-DGTM-X.
                   15  WRK-OUT-CONTA-DGTM  PIC 9(013)  VALUE ZEROS.
               10  WRK-OUT-TABELA-DGTM    OCCURS  04  TIMES.
                   15  WRK-OUT-SELEC-ATTR-DGTM
                                              PIC  9(004) COMP.
                   15  WRK-OUT-SELEC-DGTM     PIC  X(001) VALUE SPACES.
                   15  WRK-OUT-OPERA-DGTM-X.
                       20  WRK-OUT-OPERA-DGTM PIC  9(013) VALUE ZEROS.
                   15  WRK-OUT-PROD-DGTM      PIC  X(012) VALUE SPACES.
                   15  WRK-OUT-SUBPROD-DGTM   PIC  X(010) VALUE SPACES.
                   15  WRK-OUT-DTOPER-DGTM    PIC  X(010) VALUE SPACES.
                   15  WRK-OUT-VALOR-DGTM-X.
                       20  WRK-OUT-VALOR-DGTM PIC  ZZ.ZZZ.ZZZ.ZZ9,99.
                   15  WRK-OUT-MEIOENT-DGTM   PIC  X(015) VALUE SPACES.
                   15  WRK-OUT-SITANAL-DGTM   PIC  X(010) VALUE SPACES.
                   15  WRK-OUT-PARECER-DGTM   PIC  X(003) VALUE SPACES.
               10  WRK-OUT-MENSA-DGTM   PIC  X(079) VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA TELA DCOMDGTM  - BRAD0660 ***'.
      *----------------------------------------------------------------*
       01  WRK-660-DCOMDGTM.
           05  660-LL-AREA-DGTM        PIC  9(04)  COMP  VALUE 0108.
           05  660-LL-MENSAGEM-DGTM    PIC  9(04)  COMP  VALUE 0724.
           05  660-SENHAS-DGTM         PIC  9(04)  COMP  VALUE 0037.
           05  660-COMANDO-DGTM        PIC  9(04)  COMP  VALUE 2070.
           05  660-FIMAMOS-DGTM        PIC  9(04)  COMP  VALUE 0001.
           05  660-FASE-DGTM           PIC  9(04)  COMP  VALUE 0001.
           05  660-TMSTAMP-DGTM        PIC  9(04)  COMP  VALUE 0026.
           05  660-PAGINA-DGTM         PIC  9(04)  COMP  VALUE 0003.
           05  660-NOME-DGTM           PIC  9(04)  COMP  VALUE 0040.
           05  660-LITCPF-DGTM         PIC  9(04)  COMP  VALUE 0009.
           05  660-CPFCNPJ-DGTM        PIC  9(04)  COMP  VALUE 0017.
           05  660-LITBCO-DGTM         PIC  9(04)  COMP  VALUE 0006.
           05  660-BANCO-DGTM          PIC  9(04)  COMP  VALUE 0003.
           05  660-LITAGE-DGTM         PIC  9(04)  COMP  VALUE 0008.
           05  660-AGENCI-DGTM         PIC  9(04)  COMP  VALUE 0026.
           05  660-LITCTA-DGTM         PIC  9(04)  COMP  VALUE 0009.
           05  660-CCTA-DGTM           PIC  9(04)  COMP  VALUE 0013.
           05  660-OCOR-DGTM           OCCURS 4 TIMES.
               10 660-SELEC-DGTM       PIC  9(04)  COMP  VALUE 2003.
               10 660-OPERAC-DGTM      PIC  9(04)  COMP  VALUE 0013.
               10 660-PRODUT-DGTM      PIC  9(04)  COMP  VALUE 0012.
               10 660-SPRODT-DGTM      PIC  9(04)  COMP  VALUE 0010.
               10 660-DTOPER-DGTM      PIC  9(04)  COMP  VALUE 0010.
               10 660-VALOR-DGTM       PIC  9(04)  COMP  VALUE 0017.
               10 660-MEIENT-DGTM      PIC  9(04)  COMP  VALUE 0015.
               10 660-SITANA-DGTM      PIC  9(04)  COMP  VALUE 0010.
               10 660-PARECE-DGTM      PIC  9(04)  COMP  VALUE 0003.
           05  660-MENSA-DGTM          PIC  9(04)  COMP  VALUE 0079.

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
               10  COMU-OPCAO-UN       PIC  9(001)       VALUE ZEROS.
               10  COMU-OPER-UN        PIC  9(013)       VALUE ZEROS.
               10  COMU-AGENCIA-UN     PIC  9(005)       VALUE ZEROS.
               10  COMU-AGENC-UN-X.
                   15  COMU-AGENC-UN   PIC  9(005)       VALUE ZEROS.
               10  COMU-CONTA-UN-X.
                   15  COMU-CONTA-UN   PIC  9(013)       VALUE ZEROS.
FS2511*        10  COMU-CNPJ-UN        PIC  9(009)       VALUE ZEROS.
FS2511         10  COMU-CNPJ-UN        PIC  X(009)       VALUE SPACES.
FS2511*        10  COMU-FILIAL-UN      PIC  9(004)       VALUE ZEROS.
FS2511         10  COMU-FILIAL-UN      PIC  X(004)       VALUE SPACES.
               10  COMU-DIG-UN         PIC  9(002)       VALUE ZEROS.
               10  COMU-DANO-OPER-UN   PIC  9(004)       VALUE ZEROS.
               10  COMU-NSEQ-OPER-UN   PIC  9(009)       VALUE ZEROS.
               10  COMU-TIMESTAMP-UN   PIC  X(026)       VALUE ZEROS.
               10  COMU-MENSAGEM-UN    PIC  X(079)       VALUE SPACES.
               10  COMU-PAGINA-UN      PIC  9(003)       VALUE ZEROS.
DTS002     05  WRK-COMU-DADOS-0499.
DTS002         10 WRK-COMU-OCORR-0499 PIC  9(002)       VALUE ZEROS.
DTS002         10 WRK-COMU-INPUT-0499 PIC  X(1000)      VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO COM O MODULO DCOM5111 **'.
      *----------------------------------------------------------------*

       01  WRK-5111E-ENTRADA.
           03  WRK-5111E-COD-RETORNO      PIC  X(004).
           03  WRK-5111E-MSG-RETORNO      PIC  X(079).
           03  WRK-5111E-RESTART          PIC  9(005).
           03  WRK-5111E-FLAG             PIC  X(001).
           03  WRK-5111E-CFUNC-BDSCO      PIC  9(009).
           03  WRK-5111E-CTERM            PIC  X(008).
           03  WRK-5111E-DANO-OPER-DESC   PIC  9(004).
           03  WRK-5111E-NSEQ-OPER-DESC   PIC  9(009).

       01  WRK-5111S-SAIDA.
           03  WRK-5111S-COD-RETORNO      PIC  X(004).
           03  WRK-5111S-MSG-RETORNO      PIC  X(079).
           03  WRK-5111S-RESTART          PIC  9(005).
           03  WRK-5111S-FLAG             PIC  9(001).
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
                   10  WRK-4712S-VLIQ-OPER-DESCL    PIC  9(015)V9(2).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA MENSAGENS ***'.
      *----------------------------------------------------------------*

       COPY I#DCOMSG.

7C0922*----------------------------------------------------------------*
.     *     *** AREA DE ACESSO AO MODULO DCOM7565 ***
.     *----------------------------------------------------------------*
.
.      COPY I#DCOML7.
7C0922
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA BRAD7100 ***'.
      *----------------------------------------------------------------*

       COPY I#BRAD7C.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** DCOM0407 - FIM DA AREA DE WORKING ***'.
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

           IF  RETURN-CODE             EQUAL  04 OR 12
               PERFORM 2000-FINALIZAR
           END-IF.

           MOVE SPACES                 TO  WRK-OUT-DADOS-DGTM
                                           WRK-OUT-SENHAS-DGTM
                                           WRK-OUT-COMANDO-DGTM
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

           MOVE 'DCOMDGTM'             TO  WRK-TELA.

           IF  WRK-TRANSACAO           NOT EQUAL  'DCOM0407'
               PERFORM 1100-RECEBER-CONTROLE
           ELSE
               PERFORM 1200-PROCESSAR-DCOMDGTM
           END-IF.

           IF  WRK-TELA                EQUAL  'DCOMDGTM'
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

           MOVE WRK-MENSAGEM           TO COMU-AREA-UN .
           MOVE SPACES                 TO WRK-INCONSISTENCIA.
           MOVE COMU-SENHAS-UN         TO WRK-INP-SENHAS-DGTM
                                          WRK-OUT-SENHAS-DGTM.
           MOVE COMU-TIMESTAMP-UN      TO WRK-INP-TMSTAMP-DGTM
                                          WRK-OUT-TMSTAMP-DGTM.

           INITIALIZE WRK-4711E-ENTRADA
                      WRK-4711S-SAIDA
                      WRK-4712E-ENTRADA
                      WRK-4712S-SAIDA.

           IF (COMU-AGENC-UN           NOT EQUAL ZEROS
           AND COMU-CONTA-UN           NOT EQUAL ZEROS)
           AND(COMU-AGENC-UN-X         NOT EQUAL SPACES AND LOW-VALUES
           AND COMU-CONTA-UN-X         NOT EQUAL SPACES AND LOW-VALUES)
               MOVE COMU-AGENC-UN      TO WRK-OUT-CODAGE-DGTM
                                          WRK-4711E-CAG-BCRIA
               MOVE COMU-CONTA-UN      TO WRK-OUT-CONTA-DGTM
                                          WRK-4711E-CCTA-BCRIA-CLI

DTS003         IF WRK-TRANSACAO        EQUAL 'DCOM0499'
DTS003            MOVE WRK-COMU-INPUT-0499
DTS003                                 TO WRK-INPUT-DGTM
DTS003            PERFORM 1210-DEVOLVER-TELA
DTS003
DTS003            ADD  1               TO WRK-COMU-OCORR-0499
DTS003            PERFORM 1250-TRATAR-PFK06
DTS003            GO TO 1100-99-FIM
DTS003        END-IF
DTS003
               IF  WRK-TRANSACAO       EQUAL 'DCOM0408'
                   MOVE COMU-PAGINA-UN TO WRK-OUT-PAGINA-DGTM
                   COMPUTE WRK-4711E-RESTART = (COMU-PAGINA-UN - 1) * 4
                   PERFORM 1110-PROCESSAR-DCOM4711

                   IF  WRK-4711S-COD-RETORNO EQUAL '0003'
                   AND COMU-PAGINA-UN  GREATER 1
                       INITIALIZE WRK-4711E-ENTRADA
                                  WRK-4711S-SAIDA
                       MOVE 1          TO WRK-OUT-PAGINA-DGTM
                       MOVE ZEROS      TO WRK-4711S-RESTART
                       MOVE COMU-AGENC-UN TO WRK-4711E-CAG-BCRIA
                       MOVE COMU-CONTA-UN TO WRK-4711E-CCTA-BCRIA-CLI
                       PERFORM 1110-PROCESSAR-DCOM4711
                   END-IF

               ELSE
                   MOVE 1              TO WRK-OUT-PAGINA-DGTM
                   MOVE ZEROS          TO WRK-4711S-RESTART
                   PERFORM 1110-PROCESSAR-DCOM4711
               END-IF

               MOVE WRK-4711S-IPSSOA-DESC-COML(1)
                                       TO WRK-OUT-NOME-DGTM
               MOVE WRK-4711S-CAG-BCRIA-DESC(1)
                                       TO WRK-OUT-DESCAGE-DGTM

               MOVE 237                TO WRK-OUT-BANCO-DGTM
               MOVE 'BANCO:'           TO WRK-OUT-LITBCO-DGTM
               MOVE 'AGENCIA:'         TO WRK-OUT-LITAGE-DGTM
               MOVE 'CONTA...:'        TO WRK-OUT-LITCTA-DGTM

           ELSE
               MOVE 'CNPJ/CPF:'        TO WRK-OUT-LITCPF-DGTM
               MOVE COMU-CNPJ-UN       TO WRK-4712E-CCNPJ-CPF
               MOVE COMU-FILIAL-UN     TO WRK-4712E-CFLIAL-CNPJ
               MOVE COMU-DIG-UN        TO WRK-4712E-CCTRL-CNPJ-CPF
               STRING COMU-CNPJ-UN
               '/'  COMU-FILIAL-UN
               '-'  COMU-DIG-UN
               DELIMITED BY SIZE       INTO WRK-OUT-CNPJCPF-DGTM
               MOVE SPACES             TO WRK-OUT-AGENCIA-DGTM-X
                                          WRK-OUT-BANCO-DGTM-X
                                          WRK-OUT-CONTA-DGTM-X
                                          WRK-OUT-LITBCO-DGTM
                                          WRK-OUT-LITAGE-DGTM
                                          WRK-OUT-LITCTA-DGTM

DTS003         IF WRK-TRANSACAO        EQUAL 'DCOM0499'
DTS003            MOVE WRK-COMU-INPUT-0499
DTS003                                 TO WRK-INPUT-DGTM
DTS003            PERFORM 1210-DEVOLVER-TELA
DTS003
DTS003            ADD  1               TO WRK-COMU-OCORR-0499
DTS003            PERFORM 1250-TRATAR-PFK06
DTS003            GO TO 1100-99-FIM
DTS003        END-IF
DTS003
               IF  WRK-TRANSACAO       EQUAL 'DCOM0408'
                   MOVE COMU-PAGINA-UN TO WRK-OUT-PAGINA-DGTM
                   COMPUTE WRK-4712E-RESTART = (COMU-PAGINA-UN - 1) * 4
                   PERFORM 1120-PROCESSAR-DCOM4712

                   IF  WRK-4712S-COD-RETORNO EQUAL '0003'
                   AND COMU-PAGINA-UN  GREATER 1
                       INITIALIZE WRK-4712E-ENTRADA
                                  WRK-4712S-SAIDA
                       MOVE 1          TO WRK-OUT-PAGINA-DGTM
                       MOVE ZEROS      TO WRK-4712S-RESTART
                       MOVE COMU-CNPJ-UN TO WRK-4712E-CCNPJ-CPF
                       MOVE COMU-FILIAL-UN TO WRK-4712E-CFLIAL-CNPJ
                       MOVE COMU-DIG-UN TO WRK-4712E-CCTRL-CNPJ-CPF
                       PERFORM 1120-PROCESSAR-DCOM4712
                   END-IF

               ELSE
                   MOVE 1              TO WRK-OUT-PAGINA-DGTM
                   MOVE ZEROS          TO WRK-4712S-RESTART
                   PERFORM 1120-PROCESSAR-DCOM4712
               END-IF

               MOVE WRK-4712S-IPSSOA-DESC-COML(1)
                                       TO WRK-OUT-NOME-DGTM

           END-IF.

           IF  WRK-4711S-COD-RETORNO   EQUAL '0003'
           OR (WRK-4712S-COD-RETORNO   EQUAL '0003' OR '0915')
               MOVE 'S'                   TO WRK-FLAG-ERRO
               IF  WRK-TRANSACAO       EQUAL 'DCOM0408'
               AND COMU-MENSAGEM-UN    NOT EQUAL SPACES
                   MOVE COMU-MENSAGEM-UN  TO WRK-MENSAGEM-ERRO
               ELSE
                   MOVE 'NAO EXISTE(M) OPERACAO(OES) PARA CANCELAR LIBER
      -                 'ACAO'            TO WRK-MENSAGEM-ERRO
               END-IF
               PERFORM 1220-RETORNAR-CHAMADOR
           END-IF.

           IF COMU-MENSAGEM-UN         NOT EQUAL SPACES
              MOVE COMU-MENSAGEM-UN    TO WRK-OUT-MENSA-DGTM
           END-IF.

           MOVE '1'                    TO WRK-OUT-FASE-DGTM.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA CHAMA O MODULO DCOM4711                           *
      *----------------------------------------------------------------*
       1110-PROCESSAR-DCOM4711         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-COD-USER           TO WRK-4711E-CFUNC-BDSCO.
           MOVE LNK-IO-LTERM           TO WRK-4711E-CTERM.
           MOVE 'N'                    TO WRK-4711E-START-COUNT.
           MOVE 'CL'                   TO WRK-4711E-FLAG-FILTRO
           MOVE 237                    TO WRK-4711E-CBCO
           MOVE ZEROS                  TO WRK-4711E-CAG-OPER
           MOVE 'DCOM4711'             TO WRK-MODULO.

           CALL WRK-MODULO             USING  WRK-4711E-ENTRADA
                                              WRK-4711S-SAIDA
                                              ERRO-AREA
                                              WRK-SQLCA.

           EVALUATE WRK-4711S-COD-RETORNO
               WHEN '0000'
                    IF WRK-4711S-DANO-OPER-DESC(5) EQUAL ZEROS
                       MOVE '*'        TO WRK-OUT-FIMAMOS-DGTM
                    ELSE
                       MOVE SPACES     TO WRK-OUT-FIMAMOS-DGTM
                    END-IF

                    PERFORM VARYING WRK-IND FROM 1 BY 1
                              UNTIL WRK-IND GREATER 4
                        INITIALIZE WRK-OUT-TABELA-DGTM(WRK-IND)
                        MOVE SPACES    TO WRK-OUT-OPERA-DGTM-X(WRK-IND)
                                          WRK-OUT-VALOR-DGTM-X(WRK-IND)
                    END-PERFORM

                    MOVE 'SELECIONE COM "D" PARA DETALHAR OU "C" PARA CA
      -                  'NCELAR'   TO  WRK-OUT-MENSA-DGTM

                    PERFORM 1111-MONTAR-TELA-DCOM4711
                                       VARYING WRK-IND FROM
                            1 BY 1 UNTIL WRK-IND GREATER 4

               WHEN '0003'
                    GO TO 1110-99-FIM

               WHEN '0099'
                    MOVE  'DB2'        TO ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                    MOVE  'APL'        TO ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO

           END-EVALUATE.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA CARREGA OS CAMPOS DA TELA                         *
      *----------------------------------------------------------------*
       1111-MONTAR-TELA-DCOM4711       SECTION.
      *----------------------------------------------------------------*

           IF WRK-4711S-DANO-OPER-DESC(WRK-IND) NUMERIC
           AND WRK-4711S-DANO-OPER-DESC(WRK-IND) NOT EQUAL ZEROS
              MOVE WRK-4711S-DANO-OPER-DESC(WRK-IND)
                                     TO WRK-OUT-OPERA-DGTM(WRK-IND)(1:4)
              MOVE WRK-4711S-NSEQ-OPER-DESC(WRK-IND)
                                     TO WRK-OUT-OPERA-DGTM(WRK-IND)(5:9)
              MOVE WRK-4711S-IABREV-PRODT(WRK-IND)
                                       TO WRK-OUT-PROD-DGTM(WRK-IND)
              MOVE WRK-4711S-IRSUMO-SPROD-DESC(WRK-IND)
                                       TO WRK-OUT-SUBPROD-DGTM(WRK-IND)
              STRING WRK-4711S-DINIC-OPER-DESC(WRK-IND)(1:2)
              '/'    WRK-4711S-DINIC-OPER-DESC(WRK-IND)(4:2)
              '/'    WRK-4711S-DINIC-OPER-DESC(WRK-IND)(7:4)
              DELIMITED BY SIZE        INTO WRK-OUT-DTOPER-DGTM(WRK-IND)
              MOVE WRK-4711S-VOPER-DESC-COML(WRK-IND)
                                       TO WRK-OUT-VALOR-DGTM(WRK-IND)
              EVALUATE WRK-4711S-CMEIO-ENTRD-DESC(WRK-IND)
                WHEN 1
                 MOVE 'TODOS          ' TO WRK-OUT-MEIOENT-DGTM(WRK-IND)
                WHEN 2
                 MOVE 'DIG.NA AGENCIA ' TO WRK-OUT-MEIOENT-DGTM(WRK-IND)
                WHEN 3
                 MOVE 'DIG.NO POLO    ' TO WRK-OUT-MEIOENT-DGTM(WRK-IND)
                WHEN 4
                 MOVE 'TRANS. ARQUIVOS' TO WRK-OUT-MEIOENT-DGTM(WRK-IND)
                WHEN 5
                 MOVE 'SEL SIS PRODUTO' TO WRK-OUT-MEIOENT-DGTM(WRK-IND)
                WHEN 6
                 MOVE 'SEL CHEQUE NOVO' TO WRK-OUT-MEIOENT-DGTM(WRK-IND)
                WHEN 7
                 MOVE 'SEL CHEQUE CUST' TO WRK-OUT-MEIOENT-DGTM(WRK-IND)
              END-EVALUATE

              MOVE WRK-4711S-IRSUMO-SIT-DESC(WRK-IND)
                                       TO WRK-OUT-SITANAL-DGTM(WRK-IND)
              MOVE 'SIM'
                                       TO WRK-OUT-PARECER-DGTM(WRK-IND)
           ELSE
              MOVE WRK-225             TO WRK-OUT-SELEC-ATTR-DGTM
                                                              (WRK-IND)
           END-IF.

      *----------------------------------------------------------------*
       1111-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA CHAMA O MODULO DCOM4712                           *
      *----------------------------------------------------------------*
       1120-PROCESSAR-DCOM4712         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-COD-USER           TO WRK-4712E-CFUNC-BDSCO.
           MOVE LNK-IO-LTERM           TO WRK-4712E-CTERM.
           MOVE 'N'                    TO WRK-4712E-START-COUNT.
           MOVE SPACES                 TO WRK-4712E-FLAG.
           MOVE 'CL'                   TO WRK-4712E-FLAG-FILTRO.
           MOVE WRK-COD-DEPTO-N        TO WRK-AGENCIA-NUM
           MOVE WRK-AGENCIA-AUX        TO WRK-4712E-MSG-RETORNO(1:5)
           MOVE 'DCOM4712'             TO WRK-MODULO.

           CALL WRK-MODULO             USING  WRK-4712E-ENTRADA
                                              WRK-4712S-SAIDA
                                              ERRO-AREA
                                              WRK-SQLCA.

           EVALUATE WRK-4712S-COD-RETORNO
               WHEN '0000'
                    IF WRK-4712S-DANO-OPER-DESC(5) EQUAL ZEROS
                       MOVE '*'        TO WRK-OUT-FIMAMOS-DGTM
                    ELSE
                       MOVE SPACES     TO WRK-OUT-FIMAMOS-DGTM
                    END-IF

                    MOVE 'SELECIONE COM "D" PARA DETALHAR OU "C" PARA CA
      -                  'NCELAR'   TO  WRK-OUT-MENSA-DGTM

                    PERFORM VARYING WRK-IND FROM 1 BY 1
                              UNTIL WRK-IND GREATER 4
                        INITIALIZE WRK-OUT-TABELA-DGTM(WRK-IND)
                        MOVE SPACES    TO WRK-OUT-OPERA-DGTM-X(WRK-IND)
                                          WRK-OUT-VALOR-DGTM-X(WRK-IND)
                    END-PERFORM

                    PERFORM 1121-MONTAR-TELA-DCOM4712
                       VARYING WRK-IND FROM 1 BY 1
                           UNTIL WRK-IND GREATER 4

               WHEN '0915'
               WHEN '0003'
                    GO TO 1120-99-FIM

               WHEN '0099'
                    MOVE  'DB2'        TO ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                    MOVE 'S'                    TO WRK-FLAG-ERRO
                    MOVE WRK-4712S-MSG-RETORNO  TO WRK-MENSAGEM-ERRO
                    PERFORM 1220-RETORNAR-CHAMADOR

           END-EVALUATE.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA CARREGA OS CAMPOS DA TELA                         *
      *----------------------------------------------------------------*
       1121-MONTAR-TELA-DCOM4712       SECTION.
      *----------------------------------------------------------------*

           IF WRK-4712S-DANO-OPER-DESC(WRK-IND) NUMERIC
           AND WRK-4712S-DANO-OPER-DESC(WRK-IND) NOT EQUAL ZEROS
              MOVE WRK-4712S-DANO-OPER-DESC(WRK-IND)
                                     TO WRK-OUT-OPERA-DGTM(WRK-IND)(1:4)
              MOVE WRK-4712S-NSEQ-OPER-DESC(WRK-IND)
                                     TO WRK-OUT-OPERA-DGTM(WRK-IND)(5:9)
              MOVE WRK-4712S-IABREV-PRODT(WRK-IND)
                                       TO WRK-OUT-PROD-DGTM(WRK-IND)
              MOVE WRK-4712S-IRSUMO-SPROD-DESC(WRK-IND)
                                       TO WRK-OUT-SUBPROD-DGTM(WRK-IND)
              STRING WRK-4712S-DINIC-OPER-DESC(WRK-IND)(1:2)
              '/'    WRK-4712S-DINIC-OPER-DESC(WRK-IND)(4:2)
              '/'    WRK-4712S-DINIC-OPER-DESC(WRK-IND)(7:4)
              DELIMITED BY SIZE        INTO WRK-OUT-DTOPER-DGTM(WRK-IND)
              MOVE WRK-4712S-VOPER-DESC-COML(WRK-IND)
                                       TO WRK-OUT-VALOR-DGTM(WRK-IND)
              EVALUATE WRK-4712S-CMEIO-ENTRD-DESC(WRK-IND)
                WHEN 1
                 MOVE 'TODOS          ' TO WRK-OUT-MEIOENT-DGTM(WRK-IND)
                WHEN 2
                 MOVE 'DIG.NA AGENCIA ' TO WRK-OUT-MEIOENT-DGTM(WRK-IND)
                WHEN 3
                 MOVE 'DIG.NO POLO    ' TO WRK-OUT-MEIOENT-DGTM(WRK-IND)
                WHEN 4
                 MOVE 'TRANS. ARQUIVOS' TO WRK-OUT-MEIOENT-DGTM(WRK-IND)
                WHEN 5
                 MOVE 'SEL SIS PRODUTO' TO WRK-OUT-MEIOENT-DGTM(WRK-IND)
                WHEN 6
                 MOVE 'SEL CHEQUE NOVO' TO WRK-OUT-MEIOENT-DGTM(WRK-IND)
                WHEN 7
                 MOVE 'SEL CHEQUE CUST' TO WRK-OUT-MEIOENT-DGTM(WRK-IND)
              END-EVALUATE

              MOVE WRK-4712S-IRSUMO-SIT-DESC(WRK-IND)
                                       TO WRK-OUT-SITANAL-DGTM(WRK-IND)
              MOVE 'SIM'
                                       TO WRK-OUT-PARECER-DGTM(WRK-IND)
           ELSE
              MOVE WRK-225             TO WRK-OUT-SELEC-ATTR-DGTM
                                                              (WRK-IND)
           END-IF.

      *----------------------------------------------------------------*
       1121-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA PROCESSA A MENSAGEM VINDA DA TELA DCOMDGTM        *
      *----------------------------------------------------------------*
       1200-PROCESSAR-DCOMDGTM         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM             TO WRK-INPUT-DGTM.

           PERFORM 1210-DEVOLVER-TELA.

           EVALUATE TRUE

               WHEN WRK-INP-PFK-DGTM     EQUAL 'H' OR
                   (WRK-INP-PFK-DGTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DGTM EQUAL 'PFK01')
                    IF  WRK-INP-FASE-DGTM    EQUAL '1'
                        PERFORM VARYING WRK-IND FROM 1 BY 1
                                UNTIL WRK-IND GREATER 4
                           IF  WRK-INP-OPERA-DGTM-X(WRK-IND)
                                      EQUAL SPACES OR LOW-VALUES
                               MOVE WRK-225        TO
                                    WRK-OUT-SELEC-ATTR-DGTM(WRK-IND)
                           END-IF
                        END-PERFORM
                        MOVE ZEROS    TO WRK-OUT-COMANDO-ATTR-DGTM
                    ELSE
                        PERFORM VARYING WRK-IND FROM 1 BY 1
                                UNTIL WRK-IND GREATER 4
                          MOVE WRK-225        TO
                               WRK-OUT-SELEC-ATTR-DGTM(WRK-IND)
                       END-PERFORM
                       MOVE WRK-49353 TO WRK-OUT-COMANDO-ATTR-DGTM
                    END-IF
                    MOVE WRK-INP-MENSA-DGTM TO WRK-OUT-MENSA-DGTM

               WHEN WRK-INP-PFK-DGTM     EQUAL '3' OR
                   (WRK-INP-PFK-DGTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DGTM EQUAL 'PFK03')
                    PERFORM 1220-RETORNAR-CHAMADOR

               WHEN WRK-INP-PFK-DGTM     EQUAL 'X' OR
                   (WRK-INP-PFK-DGTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DGTM EQUAL 'PFK05')
                    PERFORM 1230-RETORNAR-MENU-ROTINAS

               WHEN WRK-INP-PFK-DGTM     EQUAL 'Z' OR
                   (WRK-INP-PFK-DGTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DGTM EQUAL 'PFK10')
                    PERFORM 1240-RETORNAR-MENU-DCOM

               WHEN WRK-INP-FASE-DGTM    EQUAL '1'
                    PERFORM 1201-TRATAR-FASE-1

               WHEN WRK-INP-FASE-DGTM    EQUAL '2'
                    PERFORM 1202-TRATAR-FASE-2

              WHEN OTHER
                   MOVE 'PF INVALIDA'   TO WRK-OUT-MENSA-DGTM
                   PERFORM VARYING WRK-IND FROM 1 BY 1
                             UNTIL WRK-IND GREATER 4
                       IF WRK-INP-OPERA-DGTM-X(WRK-IND)
                                        EQUAL SPACES OR LOW-VALUES
                          MOVE WRK-225
                                     TO WRK-OUT-SELEC-ATTR-DGTM(WRK-IND)
                       END-IF
                   END-PERFORM

           END-EVALUATE.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1201-TRATAR-FASE-1              SECTION.
      *----------------------------------------------------------------*
           EVALUATE TRUE
               WHEN WRK-INP-PFK-DGTM     EQUAL '7' OR
                   (WRK-INP-PFK-DGTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DGTM EQUAL 'PFK7')
                    PERFORM 1260-TRATAR-PFK07

               WHEN WRK-INP-PFK-DGTM     EQUAL '8' OR
                   (WRK-INP-PFK-DGTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DGTM EQUAL 'PFK8')
                    PERFORM 1270-TRATAR-PFK08

               WHEN WRK-INP-PFK-DGTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DGTM EQUAL  SPACES OR LOW-VALUES
                    PERFORM 1280-TRATAR-ENTER
                    IF  WRK-INCONSISTENCIA EQUAL SPACES
                        IF  ACU-SEL-D   GREATER ZEROS
                            PERFORM 1283-TRANSFERIR-CONTROLE
                        ELSE
                            PERFORM VARYING WRK-IND FROM 1 BY 1
                                      UNTIL WRK-IND GREATER 4
                              MOVE WRK-225 TO WRK-OUT-SELEC-ATTR-DGTM
                                            (WRK-IND)
                            END-PERFORM
                            MOVE 'TECLE <PF6> PARA CONFIRMAR OU TECLE <E
      -                      'NTER> PARA ALTERAR'
                                        TO WRK-OUT-MENSA-DGTM
                            MOVE '2'    TO WRK-OUT-FASE-DGTM
                            MOVE WRK-49345
                                        TO WRK-OUT-COMANDO-ATTR-DGTM
                        END-IF
                    ELSE
                        PERFORM VARYING WRK-IND FROM 1 BY 1
                                  UNTIL WRK-IND GREATER 4
                            IF WRK-INP-OPERA-DGTM-X(WRK-IND)
                                        EQUAL SPACES OR LOW-VALUES
                                MOVE WRK-225
                                  TO WRK-OUT-SELEC-ATTR-DGTM(WRK-IND)
                            END-IF
                        END-PERFORM
                    END-IF

              WHEN OTHER
                   MOVE 'PF INVALIDA'   TO WRK-OUT-MENSA-DGTM
                   PERFORM VARYING WRK-IND FROM 1 BY 1
                             UNTIL WRK-IND GREATER 4
                       IF WRK-INP-OPERA-DGTM-X(WRK-IND)
                                        EQUAL SPACES OR LOW-VALUES
                          MOVE WRK-225
                                     TO WRK-OUT-SELEC-ATTR-DGTM(WRK-IND)
                       END-IF
                   END-PERFORM

           END-EVALUATE.

      *----------------------------------------------------------------*
       1201-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1202-TRATAR-FASE-2              SECTION.
      *----------------------------------------------------------------*
           EVALUATE TRUE
               WHEN WRK-INP-PFK-DGTM     EQUAL '6' OR
                   (WRK-INP-PFK-DGTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DGTM EQUAL 'PFK6')
DTS002              MOVE 1               TO WRK-COMU-OCORR-0499
                    PERFORM 1250-TRATAR-PFK06
                   MOVE ZEROS        TO WRK-OUT-COMANDO-ATTR-DGTM

               WHEN WRK-INP-PFK-DGTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DGTM EQUAL  SPACES OR LOW-VALUES
                    MOVE '1'             TO WRK-OUT-FASE-DGTM
                    MOVE 'SELECIONE COM "D" PARA DETALHAR OU "C" PARA CA
      -                  'NCELAR'   TO  WRK-OUT-MENSA-DGTM
                   PERFORM VARYING WRK-IND FROM 1 BY 1
                             UNTIL WRK-IND GREATER 4
                       IF WRK-INP-OPERA-DGTM-X(WRK-IND)
                                        EQUAL SPACES OR LOW-VALUES
                          MOVE WRK-225
                                     TO WRK-OUT-SELEC-ATTR-DGTM(WRK-IND)
                       END-IF
                   END-PERFORM
                   MOVE ZEROS        TO WRK-OUT-COMANDO-ATTR-DGTM

              WHEN OTHER
                   MOVE 'PF INVALIDA'   TO WRK-OUT-MENSA-DGTM
                   PERFORM VARYING WRK-IND FROM 1 BY 1
                             UNTIL WRK-IND GREATER 4
                       MOVE WRK-225    TO WRK-OUT-SELEC-ATTR-DGTM
                                                        (WRK-IND)
                   END-PERFORM
                   MOVE WRK-49345      TO WRK-OUT-COMANDO-ATTR-DGTM

           END-EVALUATE.

      *----------------------------------------------------------------*
       1202-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA DEVOLVE OS DADOS DA TELA SEM ALTERACAO            *
      *----------------------------------------------------------------*
       1210-DEVOLVER-TELA              SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-FIMAMOS-DGTM   TO WRK-OUT-FIMAMOS-DGTM.
           MOVE WRK-INP-FASE-DGTM      TO WRK-OUT-FASE-DGTM.
           MOVE WRK-INP-TMSTAMP-DGTM   TO WRK-OUT-TMSTAMP-DGTM.
           MOVE WRK-INP-PAGINA-DGTM-X  TO WRK-OUT-PAGINA-DGTM-X.
           MOVE WRK-INP-AGENCIA-DGTM-X TO WRK-OUT-AGENCIA-DGTM-X
           MOVE WRK-INP-CNPJCPF-DGTM   TO WRK-OUT-CNPJCPF-DGTM
           MOVE WRK-INP-LITCPF-DGTM    TO WRK-OUT-LITCPF-DGTM
           MOVE WRK-INP-NOME-DGTM      TO WRK-OUT-NOME-DGTM
           MOVE WRK-INP-BANCO-DGTM-X   TO WRK-OUT-BANCO-DGTM-X
           MOVE WRK-INP-CONTA-DGTM-X   TO WRK-OUT-CONTA-DGTM-X
           MOVE WRK-INP-LITBCO-DGTM    TO WRK-OUT-LITBCO-DGTM
           MOVE WRK-INP-LITAGE-DGTM    TO WRK-OUT-LITAGE-DGTM
           MOVE WRK-INP-LITCTA-DGTM    TO WRK-OUT-LITCTA-DGTM

           PERFORM VARYING WRK-IND     FROM 1 BY 1
                   UNTIL WRK-IND       GREATER 4
              MOVE WRK-INP-SELEC-DGTM(WRK-IND)
                                       TO WRK-OUT-SELEC-DGTM(WRK-IND)
              MOVE WRK-INP-OPERA-DGTM-X(WRK-IND)
                                       TO WRK-OUT-OPERA-DGTM-X(WRK-IND)
              MOVE WRK-INP-PROD-DGTM(WRK-IND)
                                       TO WRK-OUT-PROD-DGTM(WRK-IND)
              MOVE WRK-INP-SUBPROD-DGTM(WRK-IND)
                                       TO WRK-OUT-SUBPROD-DGTM(WRK-IND)
              MOVE WRK-INP-DTOPER-DGTM(WRK-IND)
                                       TO WRK-OUT-DTOPER-DGTM(WRK-IND)
              MOVE WRK-INP-VALOR-DGTM(WRK-IND)
                                       TO WRK-OUT-VALOR-DGTM-X(WRK-IND)
              MOVE WRK-INP-MEIENT-DGTM(WRK-IND)
                                       TO WRK-OUT-MEIOENT-DGTM(WRK-IND)
              MOVE WRK-INP-SITANAL-DGTM(WRK-IND)
                                       TO WRK-OUT-SITANAL-DGTM(WRK-IND)
              MOVE WRK-INP-PARECE-DGTM(WRK-IND)
                                       TO WRK-OUT-PARECER-DGTM(WRK-IND)
           END-PERFORM.

      *----------------------------------------------------------------*
       1210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA RETORNA AO PROGRAMA CHAMADOR                      *
      *----------------------------------------------------------------*
       1220-RETORNAR-CHAMADOR          SECTION.
      *----------------------------------------------------------------*

           INITIALIZE COMU-AREA-UN .

           MOVE LENGTH OF COMU-AREA-UN
                                       TO COMU-LL-UN
           MOVE ZEROS                  TO COMU-ZZ-UN

           IF  WRK-FLAG-ERRO           EQUAL 'S'
               MOVE WRK-MENSAGEM-ERRO  TO COMU-MENSAGEM-UN
               IF  WRK-TRANSACAO       EQUAL 'DCOM0402'
                   IF WRK-INP-LITCPF-DGTM EQUAL SPACES OR LOW-VALUES
                       MOVE WRK-INP-CODAGE-DGTM
                                       TO COMU-AGENC-UN
                       MOVE WRK-INP-CONTA-DGTM
                                       TO COMU-CONTA-UN
                   ELSE
                       MOVE WRK-INP-NUMERO-DGTM
                                       TO COMU-CNPJ-UN
                       MOVE WRK-INP-FILIAL-DGTM
                                       TO COMU-FILIAL-UN
                       MOVE WRK-INP-CONTROLE-DGTM
                                       TO COMU-DIG-UN
                   END-IF
               END-IF
           END-IF.

           MOVE WRK-INP-SENHAS-DGTM    TO COMU-SENHAS-UN
           MOVE WRK-INP-TMSTAMP-DGTM   TO COMU-TIMESTAMP-UN

           MOVE WRK-CHNG               TO WRK-FUNCAO.
           MOVE 'DCOM0402'             TO WRK-TELA.
           MOVE 'DCOM0407'             TO COMU-TRANSACAO-UN .
           MOVE COMU-AREA-UN           TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRATAMENTO DE RETORNO AO MENU ROTINAS    *
      *----------------------------------------------------------------*
       1230-RETORNAR-MENU-ROTINAS      SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-CHNG               TO WRK-FUNCAO.
           MOVE 'DCOM0407'             TO WRK-TELA
                                          COMU-TRANSACAO-UN .
           MOVE '5'                    TO COMU-PFK-UN .
           MOVE COMU-AREA-UN           TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1230-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRATAMENTO DE RETORNO AO MENU DCOM       *
      *----------------------------------------------------------------*
       1240-RETORNAR-MENU-DCOM         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-CHNG               TO WRK-FUNCAO.
           MOVE 'DCOM0407'             TO WRK-TELA
                                          COMU-TRANSACAO-UN .
           MOVE 'A'                    TO COMU-PFK-UN .
           MOVE COMU-AREA-UN           TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1240-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRATAMENTO DO COMANDO PFK06              *
      *----------------------------------------------------------------*
       1250-TRATAR-PFK06               SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-4711E-ENTRADA
                      WRK-4711S-SAIDA
                      WRK-4712E-ENTRADA
                      WRK-4712S-SAIDA.

DTS002     PERFORM VARYING WRK-IND FROM WRK-COMU-OCORR-0499 BY 1
                     UNTIL WRK-IND GREATER 4
               IF  WRK-INP-SELEC-DGTM(WRK-IND) EQUAL 'C'
                   PERFORM 1284-ACESSAR-DCOM5111

DTS002              IF WRK-INCONSISTENCIA  EQUAL SPACES
DTS002                 PERFORM 1253-CHAMAR-TRANS-COMMIT
DTS002                 GO TO 1250-99-FIM
DTS002              END-IF
               END-IF
           END-PERFORM.

           MOVE SPACES                 TO WRK-OUT-FIMAMOS-DGTM.

           MOVE '1'                    TO WRK-OUT-FASE-DGTM

           IF  WRK-INP-LITCPF-DGTM      EQUAL SPACES OR LOW-VALUES
               MOVE WRK-INP-CODAGE-DGTM TO  WRK-4711E-CAG-BCRIA
               MOVE WRK-INP-CONTA-DGTM  TO  WRK-4711E-CCTA-BCRIA-CLI
               COMPUTE WRK-4711E-RESTART = (WRK-INP-PAGINA-DGTM - 1) * 4
               PERFORM 1110-PROCESSAR-DCOM4711
           ELSE
               MOVE WRK-INP-NUMERO-DGTM   TO WRK-4712E-CCNPJ-CPF
               MOVE WRK-INP-FILIAL-DGTM   TO WRK-4712E-CFLIAL-CNPJ
               MOVE WRK-INP-CONTROLE-DGTM TO WRK-4712E-CCTRL-CNPJ-CPF
               COMPUTE WRK-4712E-RESTART = (WRK-INP-PAGINA-DGTM - 1) * 4
               PERFORM 1120-PROCESSAR-DCOM4712
           END-IF.

           IF  WRK-4711S-COD-RETORNO   EQUAL '0003'
           OR  WRK-4712S-COD-RETORNO   EQUAL '0003'
               MOVE 1                  TO WRK-OUT-PAGINA-DGTM
               INITIALIZE WRK-4711E-ENTRADA
                          WRK-4711S-SAIDA
                          WRK-4712E-ENTRADA
                          WRK-4712S-SAIDA
               IF  WRK-INP-LITCPF-DGTM      EQUAL SPACES OR LOW-VALUES
                   MOVE WRK-INP-CODAGE-DGTM
                                        TO WRK-4711E-CAG-BCRIA
                   MOVE WRK-INP-CONTA-DGTM
                                        TO WRK-4711E-CCTA-BCRIA-CLI
                   MOVE ZEROS           TO WRK-4711E-RESTART
                   PERFORM 1110-PROCESSAR-DCOM4711
               ELSE
                   MOVE WRK-INP-NUMERO-DGTM
                                        TO WRK-4712E-CCNPJ-CPF
                   MOVE WRK-INP-FILIAL-DGTM
                                        TO WRK-4712E-CFLIAL-CNPJ
                   MOVE WRK-INP-CONTROLE-DGTM
                                        TO WRK-4712E-CCTRL-CNPJ-CPF
                   MOVE ZEROS           TO WRK-4712E-RESTART
                   PERFORM 1120-PROCESSAR-DCOM4712
               END-IF
           END-IF.

           IF  WRK-4711S-COD-RETORNO   EQUAL '0003'
           OR (WRK-4712S-COD-RETORNO   EQUAL '0003' OR '0915')
               MOVE 'S'                TO WRK-FLAG-ERRO
               MOVE 'CANCELAMENTO(S) EFETUADO(S) COM SUCESSO - C/C DEBIT
      -             'ADA'
                                       TO WRK-MENSAGEM-ERRO
               PERFORM 1220-RETORNAR-CHAMADOR
               GO TO 1250-99-FIM
           END-IF.

           IF  WRK-INCONSISTENCIA      NOT EQUAL '*'
               MOVE 'CANCELAMENTO(S) EFETUADO(S) COM SUCESSO - C/C DEBIT
      -             'ADA'
                                       TO WRK-OUT-MENSA-DGTM
           ELSE
               MOVE WRK-MENSAGEM-ERRO  TO WRK-OUT-MENSA-DGTM
           END-IF.

      *----------------------------------------------------------------*
       1250-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
DTS002
DTS002*----------------------------------------------------------------*
DTS002*  ESSA ROTINA CHAMA A TRANSACAO DCOM0499 PARA EFETUAR COMMIT    *
DTS002*----------------------------------------------------------------*
DTS002 1253-CHAMAR-TRANS-COMMIT        SECTION.
DTS002*----------------------------------------------------------------*
DTS002
DTS002     INITIALIZE COMU-AREA-UN.
DTS002
DTS002     MOVE LENGTH                 OF COMU-AREA-UN
DTS002                                 TO COMU-LL-UN.
DTS002     MOVE WRK-INP-TMSTAMP-DGTM   TO COMU-TIMESTAMP-UN.
DTS002     MOVE WRK-INP-PAGINA-DGTM    TO COMU-PAGINA-UN.
DTS002
DTS002     IF WRK-INP-LITCPF-DGTM      EQUAL SPACES OR LOW-VALUES
DTS002        MOVE WRK-INP-CODAGE-DGTM TO COMU-AGENC-UN
DTS002        MOVE WRK-INP-CONTA-DGTM  TO COMU-CONTA-UN
FS2511        MOVE SPACES              TO COMU-CNPJ-UN
DTS002                                    COMU-FILIAL-UN
FS2511        MOVE ZEROS               TO COMU-DIG-UN
DTS002     ELSE
DTS002        MOVE ZEROS               TO COMU-AGENC-UN
DTS002                                    COMU-CONTA-UN
DTS002        MOVE WRK-INP-NUMERO-DGTM TO COMU-CNPJ-UN
DTS002        MOVE WRK-INP-FILIAL-DGTM TO COMU-FILIAL-UN
DTS002        MOVE WRK-INP-CONTROLE-DGTM
DTS002                                 TO COMU-DIG-UN
DTS002     END-IF.
DTS002
DTS002     MOVE WRK-INPUT-DGTM         TO WRK-COMU-INPUT-0499.
DTS002     MOVE WRK-IND                TO WRK-COMU-OCORR-0499.
DTS002
DTS002     MOVE 'DCOM0407'             TO COMU-TRANSACAO-UN.
DTS002     MOVE 'DCOM0499'             TO WRK-TELA.
DTS002     MOVE WRK-CHNG               TO WRK-FUNCAO.
DTS002     MOVE COMU-AREA-UN           TO WRK-MENSAGEM.
DTS002
DTS002*----------------------------------------------------------------*
DTS002 1253-99-FIM.                    EXIT.
DTS002*----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRAMAMENTO DO COMANDO PFK07              *
      *----------------------------------------------------------------*
       1260-TRATAR-PFK07               SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-4711E-ENTRADA
                      WRK-4711S-SAIDA
                      WRK-4712E-ENTRADA
                      WRK-4712S-SAIDA.

           IF WRK-INP-PAGINA-DGTM  EQUAL  1
              MOVE 'NAO EXISTE PAGINA PARA VOLTAR'
                                       TO WRK-OUT-MENSA-DGTM
              PERFORM VARYING WRK-IND FROM 1 BY 1
                        UNTIL WRK-IND GREATER 4
                  IF  WRK-INP-OPERA-DGTM-X(WRK-IND)
                                       EQUAL SPACES OR LOW-VALUES
                      MOVE WRK-225 TO WRK-OUT-SELEC-ATTR-DGTM(WRK-IND)
                  END-IF
              END-PERFORM
              GO TO 1260-99-FIM
           END-IF.

           IF  WRK-INP-MENSA-DGTM      NOT EQUAL WRK-MENSA-PF7
               IF (WRK-INP-SELEC-DGTM(1) EQUAL 'D' OR 'C')
               OR (WRK-INP-SELEC-DGTM(2) EQUAL 'D' OR 'C')
               OR (WRK-INP-SELEC-DGTM(3) EQUAL 'D' OR 'C')
               OR (WRK-INP-SELEC-DGTM(4) EQUAL 'D' OR 'C')
                   MOVE WRK-MENSA-PF7 TO WRK-OUT-MENSA-DGTM
                   GO TO 1260-99-FIM
               END-IF
           END-IF

           MOVE SPACES                 TO WRK-OUT-FIMAMOS-DGTM


            IF  WRK-INP-LITCPF-DGTM      EQUAL SPACES OR LOW-VALUES
               MOVE WRK-INP-CODAGE-DGTM TO  WRK-4711E-CAG-BCRIA
               MOVE WRK-INP-CONTA-DGTM  TO  WRK-4711E-CCTA-BCRIA-CLI
               COMPUTE WRK-4711E-RESTART = (WRK-INP-PAGINA-DGTM - 2) * 4

               PERFORM 1110-PROCESSAR-DCOM4711
            ELSE
               MOVE WRK-INP-NUMERO-DGTM   TO WRK-4712E-CCNPJ-CPF
               MOVE WRK-INP-FILIAL-DGTM   TO WRK-4712E-CFLIAL-CNPJ
               MOVE WRK-INP-CONTROLE-DGTM TO WRK-4712E-CCTRL-CNPJ-CPF
               COMPUTE WRK-4712E-RESTART = (WRK-INP-PAGINA-DGTM - 2) * 4
               PERFORM 1120-PROCESSAR-DCOM4712
            END-IF.

            SUBTRACT 1 FROM WRK-INP-PAGINA-DGTM
                                       GIVING WRK-OUT-PAGINA-DGTM.
           MOVE 'AMOSTRAGEM CONTINUA'  TO WRK-OUT-MENSA-DGTM.

      *----------------------------------------------------------------*
       1260-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRAMAMENTO DO COMANDO PFK08              *
      *----------------------------------------------------------------*
       1270-TRATAR-PFK08               SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-4711E-ENTRADA
                      WRK-4711S-SAIDA
                      WRK-4712E-ENTRADA
                      WRK-4712S-SAIDA.

           IF WRK-INP-FIMAMOS-DGTM     EQUAL  '*'
              MOVE 'NAO EXISTE PAGINA PARA AVANCAR'
                                       TO WRK-OUT-MENSA-DGTM
              PERFORM VARYING WRK-IND FROM 1 BY 1
                        UNTIL WRK-IND GREATER 4
                  IF  WRK-INP-OPERA-DGTM-X(WRK-IND)
                                       EQUAL SPACES OR LOW-VALUES
                      MOVE WRK-225 TO WRK-OUT-SELEC-ATTR-DGTM(WRK-IND)
                  END-IF
              END-PERFORM
              GO TO 1270-99-FIM
           END-IF.

           IF  WRK-INP-MENSA-DGTM      NOT EQUAL WRK-MENSA-PF8
               IF (WRK-INP-SELEC-DGTM(1) EQUAL 'D' OR 'C')
               OR (WRK-INP-SELEC-DGTM(2) EQUAL 'D' OR 'C')
               OR (WRK-INP-SELEC-DGTM(3) EQUAL 'D' OR 'C')
               OR (WRK-INP-SELEC-DGTM(4) EQUAL 'D' OR 'C')
                   MOVE WRK-MENSA-PF8 TO WRK-OUT-MENSA-DGTM
                   GO TO 1270-99-FIM
               END-IF
           END-IF

           IF  WRK-INP-LITCPF-DGTM      EQUAL SPACES OR LOW-VALUES
              MOVE WRK-INP-CODAGE-DGTM TO  WRK-4711E-CAG-BCRIA
              MOVE WRK-INP-CONTA-DGTM  TO  WRK-4711E-CCTA-BCRIA-CLI
              COMPUTE WRK-4711E-RESTART = (WRK-INP-PAGINA-DGTM) * 4
              PERFORM 1110-PROCESSAR-DCOM4711
           ELSE
              MOVE WRK-INP-NUMERO-DGTM   TO WRK-4712E-CCNPJ-CPF
              MOVE WRK-INP-FILIAL-DGTM   TO WRK-4712E-CFLIAL-CNPJ
              MOVE WRK-INP-CONTROLE-DGTM TO WRK-4712E-CCTRL-CNPJ-CPF
              COMPUTE WRK-4712E-RESTART = (WRK-INP-PAGINA-DGTM) * 4
              PERFORM 1120-PROCESSAR-DCOM4712
           END-IF.

           ADD 1 TO WRK-INP-PAGINA-DGTM
                                       GIVING WRK-OUT-PAGINA-DGTM.

           IF  WRK-OUT-FIMAMOS-DGTM      EQUAL SPACES
               MOVE 'AMOSTRAGEM CONTINUA'TO WRK-OUT-MENSA-DGTM
           ELSE
               MOVE 'FIM DE AMOSTRAGEM'  TO WRK-OUT-MENSA-DGTM
           END-IF.

      *----------------------------------------------------------------*
       1270-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRAMAMENTO DO COMANDO ENTER              *
      *----------------------------------------------------------------*
       1280-TRATAR-ENTER               SECTION.
      *----------------------------------------------------------------*

           IF (WRK-INP-SELEC-DGTM(1) EQUAL SPACES OR LOW-VALUES)
           AND(WRK-INP-SELEC-DGTM(2) EQUAL SPACES OR LOW-VALUES)
           AND(WRK-INP-SELEC-DGTM(3) EQUAL SPACES OR LOW-VALUES)
           AND(WRK-INP-SELEC-DGTM(4) EQUAL SPACES OR LOW-VALUES)
               MOVE 'NENHUMA OPCAO FOI SELECIONADA'
                                     TO  WRK-OUT-MENSA-DGTM
               MOVE '*'              TO WRK-INCONSISTENCIA
               GO TO 1280-99-FIM
           END-IF.

           PERFORM VARYING WRK-IND         FROM 1 BY 1
                                           UNTIL WRK-IND GREATER 4
               IF  WRK-INP-SELEC-DGTM(WRK-IND)
                                           NOT EQUAL 'D' AND 'C' AND
                                           SPACES AND LOW-VALUES
                 MOVE '*'                  TO WRK-INCONSISTENCIA
                 MOVE WRK-49353            TO WRK-OUT-SELEC-ATTR-DGTM
                                              (WRK-IND)
                 MOVE 'OPCAO INVALIDA - SELECIONAR APENAS: D/C'
                                           TO  WRK-OUT-MENSA-DGTM
7C0922         ELSE
.                MOVE WRK-IND TO WRK-IND1
.                IF  WRK-INP-SELEC-DGTM(WRK-IND1)   EQUAL  'C' AND
.                   (WRK-INP-SUBPROD-DGTM(WRK-IND1) EQUAL 'NPR - RO ' OR
.                    WRK-INP-SUBPROD-DGTM(WRK-IND1) EQUAL 'NPR - RPL')
.                   PERFORM 1400-OBTER-MSGEM-COR0005R1
.                END-IF
7C0922         END-IF
           END-PERFORM.

           IF  WRK-INCONSISTENCIA          NOT EQUAL SPACES
               GO TO 1280-99-FIM
           END-IF.

           MOVE ZEROS                      TO ACU-SEL-D

           PERFORM VARYING WRK-IND     FROM 1 BY 1
                     UNTIL WRK-IND     GREATER 4
               IF  WRK-INP-SELEC-DGTM(WRK-IND) EQUAL 'D'
                   ADD 1               TO ACU-SEL-D
               END-IF
           END-PERFORM.

           IF  ACU-SEL-D   GREATER 1
               PERFORM VARYING WRK-IND FROM 1 BY 1
                         UNTIL WRK-IND GREATER 4
                   IF  WRK-INP-SELEC-DGTM(WRK-IND) EQUAL 'D'
                       MOVE WRK-49353  TO WRK-OUT-SELEC-ATTR-DGTM
                                          (WRK-IND)
                   END-IF
               END-PERFORM
               MOVE 'SELECIONAR APENAS UMA OCORRENCIA PARA OPCAO "D"'
                                       TO  WRK-OUT-MENSA-DGTM
               MOVE '*'                TO WRK-INCONSISTENCIA
               GO TO 1280-99-FIM
           END-IF

           MOVE SPACES                 TO WRK-SELEC-ANT.
           PERFORM VARYING WRK-IND     FROM 1 BY 1
                                       UNTIL WRK-IND GREATER 4
             IF  WRK-INP-SELEC-DGTM(WRK-IND)
                                       NOT EQUAL WRK-SELEC-ANT AND
                 WRK-SELEC-ANT         NOT EQUAL SPACES
                 IF  WRK-INP-SELEC-DGTM(WRK-IND)  NOT EQUAL  SPACES
                     MOVE '*'          TO WRK-INCONSISTENCIA
                 END-IF
             ELSE
                 MOVE WRK-INP-SELEC-DGTM(WRK-IND) TO WRK-SELEC-ANT
             END-IF
           END-PERFORM.

           IF  WRK-INCONSISTENCIA      NOT EQUAL SPACES
               PERFORM VARYING WRK-IND FROM 1 BY 1
                         UNTIL WRK-IND GREATER 4
                   IF  WRK-INP-SELEC-DGTM(WRK-IND) NOT EQUAL SPACES
                                                     AND LOW-VALUES
                       MOVE WRK-49353  TO WRK-OUT-SELEC-ATTR-DGTM
                                          (WRK-IND)
                   END-IF
               END-PERFORM
               MOVE 'SELECIONAR APENAS UMA OPCAO: D/C'
                                       TO  WRK-OUT-MENSA-DGTM
               GO TO 1280-99-FIM
           END-IF.

      *----------------------------------------------------------------*
       1280-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA TRANFERE CONTROLE DE ACORDO COM A SELECAO.        *
      *----------------------------------------------------------------*
       1283-TRANSFERIR-CONTROLE        SECTION.
      *----------------------------------------------------------------*

           PERFORM VARYING WRK-IND     FROM 1 BY 1
                                       UNTIL WRK-IND GREATER 4
             IF  WRK-INP-SELEC-DGTM(WRK-IND)
                                       EQUAL 'D'
                 PERFORM 1285-CHAMAR-DCOM0408
             END-IF
           END-PERFORM.

      *----------------------------------------------------------------*
      *  ESSA ROTINA CHAMA O MODULO DCOM5111 PARA CANCELAR LIBERACAO.  *
      *----------------------------------------------------------------*
       1284-ACESSAR-DCOM5111            SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-5111E-ENTRADA
                      WRK-5111S-SAIDA.

           MOVE SPACES                 TO WRK-5111E-COD-RETORNO.
           MOVE SPACES                 TO WRK-5111E-MSG-RETORNO.
           MOVE ZEROS                  TO WRK-5111E-RESTART.
           MOVE SPACES                 TO WRK-5111E-FLAG.
           MOVE WRK-COD-USER           TO WRK-5111E-CFUNC-BDSCO.
           MOVE LNK-IO-LTERM           TO WRK-5111E-CTERM.
           MOVE WRK-INP-OPERA-DGTM(WRK-IND)(1:4)
                                       TO WRK-5111E-DANO-OPER-DESC.
           MOVE WRK-INP-OPERA-DGTM(WRK-IND)(5:9)
                                       TO WRK-5111E-NSEQ-OPER-DESC.

           MOVE 'DCOM5111'             TO WRK-MODULO.

           CALL WRK-MODULO             USING WRK-5111E-ENTRADA
                                             WRK-5111S-SAIDA
                                             ERRO-AREA
                                             WRK-SQLCA.

           EVALUATE WRK-5111S-COD-RETORNO
               WHEN '0000'
                    GO TO 1284-99-FIM
               WHEN '0099'
                    MOVE  'DB2'        TO ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN '7777'
                    MOVE  'APL'        TO ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
DTS001              PERFORM 8888-EXECUTAR-ROLLBACK
                    STRING WRK-INP-OPERA-DGTM(WRK-IND)
                        '-'WRK-5111S-MSG-RETORNO
                    DELIMITED BY SIZE INTO WRK-MENSAGEM-ERRO
                    MOVE WRK-49369    TO
                    WRK-OUT-SELEC-ATTR-DGTM(WRK-IND)
                    MOVE 5            TO WRK-IND
                    MOVE '*'          TO WRK-INCONSISTENCIA

           END-EVALUATE.

      *----------------------------------------------------------------*
       1284-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA CHAMA O MODULO DCOM5111 PARA CANCELAR LIBERACAO.  *
      *----------------------------------------------------------------*
       1285-CHAMAR-DCOM0408            SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-CHNG               TO WRK-FUNCAO.
           MOVE 'DCOM0408'             TO WRK-TELA.
           MOVE 'DCOM0407'             TO COMU-TRANSACAO-UN .
           MOVE WRK-INP-SENHAS-DGTM    TO COMU-SENHAS-UN .
           MOVE WRK-INP-TMSTAMP-DGTM   TO COMU-TIMESTAMP-UN.
           MOVE WRK-INP-PAGINA-DGTM    TO COMU-PAGINA-UN

           IF  WRK-INP-LITCPF-DGTM EQUAL SPACES OR LOW-VALUES
               MOVE WRK-INP-CODAGE-DGTM
                                   TO COMU-AGENC-UN
               MOVE WRK-INP-CONTA-DGTM
                                   TO COMU-CONTA-UN
           ELSE
               MOVE WRK-INP-NUMERO-DGTM
                                   TO COMU-CNPJ-UN
               MOVE WRK-INP-FILIAL-DGTM
                                   TO COMU-FILIAL-UN
               MOVE WRK-INP-CONTROLE-DGTM
                                   TO COMU-DIG-UN
           END-IF.

           MOVE WRK-INP-OPERA-DGTM(WRK-IND)(1:4)
                                       TO COMU-DANO-OPER-UN
           MOVE WRK-INP-OPERA-DGTM(WRK-IND)(5:9)
                                       TO COMU-NSEQ-OPER-UN
           MOVE COMU-AREA-UN           TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1284-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA CARREGA A TELA QUE SERA DEMONSTRADA PELO IMS.     *
      *----------------------------------------------------------------*
       1300-PROCESSAR-BRAD0660         SECTION.
      *----------------------------------------------------------------*

           CALL 'BRAD0660'             USING WRK-OUTPUT-DGTM
                                             WRK-660-DCOMDGTM.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE RETURN-CODE        TO WRK-RETURN-CODE
               MOVE 10                 TO WRK-LOCAL-ERRO
               MOVE WRK-ERRO-BRAD0660  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-ISRT               TO WRK-FUNCAO.
           MOVE WRK-OUTPUT-DGTM        TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

7C0922*----------------------------------------------------------------*
.      1400-OBTER-MSGEM-COR0005R1      SECTION.
.     *----------------------------------------------------------------*
.
.          INITIALIZE L7-ARGUMENTOS-ENTRADA
.                     L7-RETORNO
.                     ERRO-AREA
.                     WRK-SQLCA.
.
.          MOVE WRK-INP-OPERA-DANO(WRK-IND1) TO L7-DANO-OPER-DESC.
.          MOVE WRK-INP-OPERA-NSEQ(WRK-IND1) TO L7-NSEQ-OPER-DESC.
.          MOVE 'COR0005R1'                  TO L7-CMSGEM-RECOR.
.          MOVE 'PARCIAL-01'                 TO L7-INSTRUCAO.
.          MOVE 'DCOM7565'                   TO WRK-MODULO.
.
.          CALL WRK-MODULO             USING L7-ARGUMENTOS-ENTRADA
.                                            L7-RETORNO
.                                            ERRO-AREA
.                                            WRK-SQLCA.
.
.          EVALUATE L7-COD-RETORNO
.             WHEN  0000
.                  MOVE '*'                  TO WRK-INCONSISTENCIA
.                  MOVE WRK-49353            TO WRK-OUT-SELEC-ATTR-DGTM
.                                              (WRK-IND1)
.                  MOVE 'CANCELAMENTO NAO AUTORIZADO - COR0005 ENVIADO'
.                                            TO  WRK-OUT-MENSA-DGTM
.             WHEN  0003
.                  CONTINUE
.             WHEN OTHER
.                  MOVE L7-MENSAGEM
.                                      TO WRK-MENSAGEM-ERRO
.                  MOVE 'S'            TO WRK-FLAG-ERRO
.
.          END-EVALUATE.
.
.     *----------------------------------------------------------------*
.      1400-99-FIM.                    EXIT.
7C0922*----------------------------------------------------------------*
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
               MOVE 'DCOM0407'         TO ERR-MODULO
           ELSE
               MOVE 'DCOM0407'         TO ERR-PGM
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
