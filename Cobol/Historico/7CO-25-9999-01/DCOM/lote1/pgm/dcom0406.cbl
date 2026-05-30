      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. DCOM0406.
       AUTHOR.     DIEGO MARCEL ZANOTTO
      *================================================================*
      *                          A L T R A N                           *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   DCOM0406                                    *
      *    PROGRAMADOR.:   DIEGO MARCEL ZANOTTO                        *
      *    ANALISTA....:   AQUIRA H.                                   *
      *    DATA........:   01/02/2007                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   CANCELAR LIBERACAO DA OPERACAO - OPERACOES  *
      *                    POR AGENCIA.                                *
      *----------------------------------------------------------------*
      *    TELAS.......:                                               *
      *    DCOMDFTM - CANCELAR LIBERACAO DA OPERACAO - OPERACOES       *
      *               POR AGENCIA.                                     *
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
DTS001*    DATA........:   03/02/2010                                  *
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
           '*** DCOM0406 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ATRIBUTOS ***'.
      *---------------------------------------------------------------*
      *--- NUMERICO - NORMAL    - PROTEGIDO    - NAO CURSOR - NUM  ---*
       01  WRK-241                     PIC S9(008) COMP    VALUE +241.
      *--- NUMERICO - NORMAL    - PROTEGIDO    - POS CURSOR - NUM  ---*
       01  WRK-49361                   PIC S9(008) COMP    VALUE +49361.
      *--- NUMERICO - BRILHANTE - DESPROTEGIDO - POS CURSOR - NUM  ---*
       01  WRK-49369                   PIC S9(008) COMP    VALUE +49369.
      *--- ALPHA    - NORMAL    - DESPROTEGIDO - POS CURSOR - ALFA ---*
       01  WRK-49345                   PIC S9(008) COMP    VALUE +49345.
      *--- ALPHA    - NORMAL    - PROTEGIDO    - N POS CUR. - ALPHA---*
       01  WRK-225                     PIC S9(008) COMP    VALUE +225.
      *--- BRILHANTE, NORMAL    - DEPROTEGIDO  - POS CURSOR - ALFA ---*
       01  WRK-49353                   PIC S9(008) COMP    VALUE +49353.

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
           05  ACU-SEL-D               PIC  9(002)         VALUE ZEROS.
           05  WRK-MENSA-PF7           PIC  X(079)         VALUE
               'PF7 DESPREZARA AS ATUALIZACOES EFETUADAS - TECLE NOVAMEN
      -        'TE PARA VOLTAR'.
           05  WRK-MENSA-PF8           PIC  X(079)         VALUE
               'PF8 DESPREZARA AS ATUALIZACOES EFETUADAS - TECLE NOVAMEN
      -        'TE PARA AVANCAR'.

FS2511 01  WRK-CNPJCPF.
FS2511*    05  WRK-NUMERO              PIC  9(009)         VALUE ZEROS.
FS2511     05  WRK-NUMERO              PIC  X(009)         VALUE SPACES.
FS2511     05  WRK-FILLER1             PIC  X(001)         VALUE SPACES.
FS2511*    05  WRK-FILIAL              PIC  9(004)         VALUE ZEROS.
FS2511     05  WRK-FILIAL              PIC  X(004)         VALUE SPACES.
FS2511     05  WRK-FILLER2             PIC  X(001)         VALUE SPACES.
FS2511     05  WRK-CONTROLE            PIC  9(002)         VALUE ZEROS.

       01  WRK-AGENCIA-NOME.
           05  WRK-COD-AGE             PIC  9(005)         VALUE ZEROS.
           05  WRK-FILLER3             PIC  X(001)         VALUE SPACES.
           05  WRK-DESC-AGE            PIC  X(020)         VALUE SPACES.

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
           '*** AREA DA TELA DCOMDFTM  - INPUT ***'.
      *----------------------------------------------------------------*
       01  WRK-INPUT-DFTM.
           05  FILLER                  PIC  X(018) VALUE SPACES.
           05  WRK-INP-PFK-DFTM        PIC  X(001) VALUE SPACES.
           05  WRK-INP-SENHAS-DFTM     PIC  X(037) VALUE SPACES.
           05  WRK-INP-COMANDO-DFTM    PIC  X(068) VALUE SPACES.
           05  WRK-INP-DADOS-DFTM.
               10  WRK-INP-FIMAMOS-DFTM
                                       PIC  X(001) VALUE SPACES.
               10  WRK-INP-FASE-DFTM   PIC  X(001) VALUE SPACES.
               10  WRK-INP-TMSTAMP-DFTM
                                       PIC  X(026) VALUE SPACES.
               10  WRK-INP-PAGINA-DFTM-X.
                   15  WRK-INP-PAGINA-DFTM
                                       PIC  9(003) VALUE ZEROS.
               10  WRK-INP-CODAGE-DFTM-X.
                   15  WRK-INP-CODAGE-DFTM
                                        PIC  9(005) VALUE ZEROS.
               10  WRK-INP-DESCAGE-DFTM PIC  X(020) VALUE SPACES.
               10  WRK-INP-TABELA-DFTM  OCCURS  05  TIMES.
                   15  WRK-INP-SELEC-DFTM     PIC  X(001) VALUE SPACES.
                   15  WRK-INP-OPERA-DFTM-X.
                       20  WRK-INP-OPERA-DFTM PIC  9(013) VALUE ZEROS.
                   15  WRK-INP-OPERA-DFTM-R REDEFINES
                                  WRK-INP-OPERA-DFTM-X.
                       20  WRK-INP-OPERA-DANO PIC  9(04).
                       20  WRK-INP-OPERA-NSEQ PIC  9(09).
                   15  WRK-INP-PROD-DFTM      PIC  X(012) VALUE SPACES.
                   15  WRK-INP-SUBPROD-DFTM   PIC  X(010) VALUE SPACES.
                   15  WRK-INP-DTOPER-DFTM    PIC  X(010) VALUE SPACES.
                   15  WRK-INP-VALOR-DFTM     PIC  X(017) VALUE SPACES.
                   15  WRK-INP-MEIOENT-DFTM   PIC  X(015) VALUE SPACES.
                   15  WRK-INP-SITANAL-DFTM   PIC  X(010) VALUE SPACES.
                   15  WRK-INP-PARECER-DFTM   PIC  X(003) VALUE SPACES.
           05  WRK-INP-MENSA-DFTM       PIC  X(079) VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                       PIC  X(050)         VALUE
           '*** AREA DA TELA DCOMDFTM  - OUTPUT ***'.
      *----------------------------------------------------------------*
       01  WRK-OUTPUT-DFTM.
           05  WRK-OUT-LL-DFTM          PIC S9(004) COMP VALUE +0711.
           05  WRK-OUT-ZZ-DFTM          PIC S9(004) COMP VALUE ZEROS.
           05  WRK-OUT-SENHAS-DFTM      PIC  X(037) VALUE SPACES.
           05  WRK-OUT-COMANDO-ATTR-DFTM PIC S9(004) COMP VALUE ZEROS.
           05  WRK-OUT-COMANDO-DFTM     PIC  X(068) VALUE SPACES.
           05  WRK-OUT-DADOS-DFTM.
               10  WRK-OUT-FIMAMOS-DFTM PIC  X(001) VALUE SPACES.
               10  WRK-OUT-FASE-DFTM    PIC  X(001) VALUE SPACES.
               10  WRK-OUT-TMSTAMP-DFTM
                                       PIC  X(026) VALUE SPACES.
               10  WRK-OUT-PAGINA-DFTM-X.
                   15  WRK-OUT-PAGINA-DFTM
                                        PIC  9(003) VALUE ZEROS.
               10  WRK-OUT-CODAGE-DFTM-X.
                   15  WRK-OUT-CODAGE-DFTM
                                        PIC  9(005) VALUE ZEROS.
               10  WRK-OUT-DESCAGE-DFTM PIC  X(020) VALUE SPACES.
               10  WRK-OUT-TABELA-DFTM    OCCURS  05  TIMES.
                   15  WRK-OUT-SELEC-ATTR-DFTM PIC S9(004) COMP
                                                    VALUE ZEROS.
                   15  WRK-OUT-SELEC-DFTM     PIC  X(001) VALUE SPACES.
                   15  WRK-OUT-OPERA-DFTM-X.
                       20  WRK-OUT-OPERA-DFTM PIC  9(013) VALUE ZEROS.
                   15  WRK-OUT-OPERA-DFTM-R REDEFINES
                                  WRK-OUT-OPERA-DFTM-X.
                       20  WRK-OUT-OPERA-DANO PIC  9(04).
                       20  WRK-OUT-OPERA-NSEQ PIC  9(09).
                   15  WRK-OUT-PROD-DFTM      PIC  X(012) VALUE SPACES.
                   15  WRK-OUT-SUBPROD-DFTM   PIC  X(010) VALUE SPACES.
                   15  WRK-OUT-DTOPER-DFTM    PIC  X(010) VALUE SPACES.
                   15  WRK-OUT-VALOR-DFTM-X.
                       20  WRK-OUT-VALOR-DFTM PIC  ZZ.ZZZ.ZZZ.ZZ9,99.
                   15  WRK-OUT-MEIOENT-DFTM   PIC  X(015) VALUE SPACES.
                   15  WRK-OUT-SITANAL-DFTM   PIC  X(010) VALUE SPACES.
                   15  WRK-OUT-PARECER-DFTM   PIC  X(003) VALUE SPACES.
               10  WRK-OUT-MENSA-DFTM   PIC  X(079) VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA TELA DCOMDFTM  - BRAD0660 ***'.
      *----------------------------------------------------------------*
       01  WRK-660-DCOMDFTM.
           05  660-LL-AREA-DFTM        PIC  9(04)  COMP  VALUE 0112.
           05  660-LL-MENSAGEM-DFTM    PIC  9(04)  COMP  VALUE 0711.
           05  660-SENHAS-DFTM         PIC  9(04)  COMP  VALUE 0037.
           05  660-COMANDO-DFTM        PIC  9(04)  COMP  VALUE 2070.
           05  660-FIMAMOS-DFTM        PIC  9(04)  COMP  VALUE 0001.
           05  660-FASE-DFTM           PIC  9(04)  COMP  VALUE 0001.
           05  660-TMSTAMP-DFTM        PIC  9(04)  COMP  VALUE 0026.
           05  660-PAGINA-DFTM         PIC  9(04)  COMP  VALUE 0003.
           05  660-CODAGE-DFTM         PIC  9(04)  COMP  VALUE 0005.
           05  660-DSCAGE-DFTM         PIC  9(04)  COMP  VALUE 0020.
           05  660-OCOR-DFTM OCCURS 5 TIMES.
               10 660-SELEC-DFTM       PIC  9(04)  COMP  VALUE 2003.
               10 660-OPERAC-DFTM      PIC  9(04)  COMP  VALUE 0013.
               10 660-PRODUT-DFTM      PIC  9(04)  COMP  VALUE 0012.
               10 660-SPRODT-DFTM      PIC  9(04)  COMP  VALUE 0010.
               10 660-DTOPER-DFTM      PIC  9(04)  COMP  VALUE 0010.
               10 660-VALOR-DFTM       PIC  9(04)  COMP  VALUE 0017.
               10 660-MEIENT-DFTM      PIC  9(04)  COMP  VALUE 0015.
               10 660-SITANA-DFTM      PIC  9(04)  COMP  VALUE 0010.
               10 660-PARECE-DFTM      PIC  9(04)  COMP  VALUE 0003.
           05  660-MENSA-DFTM          PIC  9(04)  COMP  VALUE 0079.

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
               10  COMU-OPCAO-UN      PIC  9(001)       VALUE ZEROS.
               10  COMU-OPER-UN       PIC  9(013)       VALUE ZEROS.
               10  COMU-AGENCIA-UN    PIC  9(005)       VALUE ZEROS.
               10  COMU-AGENC-UN      PIC  9(005)       VALUE ZEROS.
               10  COMU-CONTA-UN      PIC  9(013)       VALUE ZEROS.
FS2511*        10  COMU-CNPJ-UN       PIC  9(009)       VALUE ZEROS.
FS2511         10  COMU-CNPJ-UN       PIC  X(009)       VALUE SPACES.
FS2511*        10  COMU-FILIAL-UN     PIC  9(004)       VALUE ZEROS.
FS2511         10  COMU-FILIAL-UN     PIC  X(004)       VALUE SPACES.
               10  COMU-DIG-UN        PIC  9(002)       VALUE ZEROS.
               10  COMU-DANO-OPER-UN  PIC  9(004)       VALUE ZEROS.
               10  COMU-NSEQ-OPER-UN  PIC  9(009)       VALUE ZEROS.
               10  COMU-TIMESTAMP-UN  PIC  X(026)       VALUE ZEROS.
               10  COMU-MENSAGEM-UN   PIC  X(079)       VALUE SPACES.
               10  COMU-PAGINA-UN     PIC  9(003)       VALUE ZEROS.
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
           03  WRK-5111S-CONSISTENCIA.
               05  WRK-5111S-TAMANHO-ERRO PIC  9(001).
               05  WRK-5111S-TABELA-ERROS OCCURS  2 TIMES.
                   07  WRK-5111S-CAMPOS-ERRO
                                          PIC 9(001).

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
FS2511*        10  4710E-CFLIAL-CNPJ              PIC  9(004).
FS2511         10  4710E-CFLIAL-CNPJ              PIC  X(004).
FS2511         10  FILLER                         PIC  X(001).
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

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA MENSAGENS ***'.
      *----------------------------------------------------------------*

       COPY 'I#DCOMSG'.

7C0922*----------------------------------------------------------------*
.     *     *** AREA DE ACESSO AO MODULO DCOM7565 ***
.     *----------------------------------------------------------------*
.
.      COPY 'I#DCOML7'.
7C0922
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA BRAD7100 ***'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** DCOM0406 - FIM DA AREA DE WORKING ***'.
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

           MOVE SPACES                 TO  WRK-OUT-DADOS-DFTM
                                           WRK-OUT-SENHAS-DFTM
                                           WRK-OUT-COMANDO-DFTM
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

           MOVE 'DCOMDFTM'             TO  WRK-TELA.

           IF  WRK-TRANSACAO           NOT EQUAL  'DCOM0406'
               PERFORM 1100-RECEBER-CONTROLE
           ELSE
               PERFORM 1200-PROCESSAR-DCOMDFTM
           END-IF.

           IF  WRK-TELA                EQUAL  'DCOMDFTM'
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

           MOVE WRK-MENSAGEM           TO COMU-AREA-UN.
           MOVE COMU-AGENCIA-UN        TO WRK-INP-CODAGE-DFTM
                                          WRK-OUT-CODAGE-DFTM.
           MOVE COMU-TIMESTAMP-UN      TO WRK-INP-TMSTAMP-DFTM
                                          WRK-OUT-TMSTAMP-DFTM.

DTS002     IF  WRK-TRANSACAO           EQUAL 'DCOM0499'
DTS002         MOVE WRK-COMU-INPUT-0499   TO WRK-INPUT-DFTM
DTS002         PERFORM 1210-DEVOLVER-TELA
DTS002
DTS002         ADD  1                  TO WRK-COMU-OCORR-0499
DTS002         PERFORM 1250-TRATAR-CONFIRMA
DTS002         GO TO 1100-99-FIM
DTS002     END-IF.
DTS002
           MOVE 1                      TO WRK-OUT-PAGINA-DFTM.
           MOVE '1'                    TO WRK-OUT-FASE-DFTM.

           IF  WRK-TRANSACAO           EQUAL 'DCOM0408'
               MOVE COMU-PAGINA-UN     TO WRK-OUT-PAGINA-DFTM
               COMPUTE 4710E-RESTART = (COMU-PAGINA-UN - 1) * 5
               MOVE COMU-AGENCIA-UN    TO 4710E-CAG-BCRIA
               PERFORM 1120-ACESSAR-DCOM4710
               IF  4710S-COD-RETORNO   EQUAL '0003'
               AND COMU-PAGINA-UN      GREATER 1
                   MOVE 1              TO WRK-OUT-PAGINA-DFTM
                   INITIALIZE 4710-ENTRADA-ROTEADOR
                              4710-SAIDA-ROTEADOR
                   MOVE ZEROS          TO 4710E-RESTART
                   MOVE COMU-AGENCIA-UN
                                       TO 4710E-CAG-BCRIA
                   PERFORM 1120-ACESSAR-DCOM4710
               END-IF
           ELSE
               MOVE COMU-AGENCIA-UN    TO 4710E-CAG-BCRIA
               MOVE ZEROS              TO 4710E-RESTART
               PERFORM 1120-ACESSAR-DCOM4710
           END-IF.

           IF  4710S-COD-RETORNO       EQUAL '0003'
               IF  WRK-TRANSACAO       EQUAL 'DCOM0408'
               AND COMU-MENSAGEM-UN    NOT EQUAL SPACES
                   MOVE COMU-MENSAGEM-UN
                                       TO WRK-MENSAGEM-ERRO
               ELSE
                   MOVE 'NAO EXISTE(M) OPERACAO(OES) PARA CANCELAR LIBER
      -             'ACAO'             TO WRK-MENSAGEM-ERRO
               END-IF
               MOVE 'S'                TO WRK-FLAG-ERRO
               PERFORM 1220-RETORNAR-CHAMADOR
           END-IF.

           IF  COMU-MENSAGEM-UN        NOT EQUAL SPACES
               MOVE COMU-MENSAGEM-UN   TO WRK-OUT-MENSA-DFTM
           ELSE
               MOVE 'SELECIONE COM "D" PARA DETALHAR OU "C" PARA CANCELA
      -        'R'                     TO WRK-OUT-MENSA-DFTM
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA CARREGA OS CAMPOS DA TELA                         *
      *----------------------------------------------------------------*
       1110-MONTAR-TELA                SECTION.
      *----------------------------------------------------------------*

           IF  4710S-DANO-OPER-DESC(WRK-IND) NUMERIC
           AND 4710S-DANO-OPER-DESC(WRK-IND) NOT EQUAL ZEROS
              MOVE 4710S-DANO-OPER-DESC(WRK-IND)
                                       TO WRK-OUT-OPERA-DANO(WRK-IND)
              MOVE 4710S-NSEQ-OPER-DESC(WRK-IND)
                                       TO WRK-OUT-OPERA-NSEQ(WRK-IND)
              MOVE 4710S-IABREV-PRODT(WRK-IND)
                                       TO WRK-OUT-PROD-DFTM(WRK-IND)
              MOVE 4710S-IRSUMO-SPROD-DESC(WRK-IND)
                                       TO WRK-OUT-SUBPROD-DFTM(WRK-IND)
              STRING 4710S-DINIC-OPER-DESC(WRK-IND)(1:2)
              '/'    4710S-DINIC-OPER-DESC(WRK-IND)(4:2)
              '/'    4710S-DINIC-OPER-DESC(WRK-IND)(7:4)
              DELIMITED BY SIZE        INTO WRK-OUT-DTOPER-DFTM(WRK-IND)
              MOVE 4710S-VOPER-DESC-COML(WRK-IND)
                                       TO WRK-OUT-VALOR-DFTM(WRK-IND)
              MOVE 4710S-IRSUMO-MEIO-ENTRD(WRK-IND)
                                       TO WRK-OUT-MEIOENT-DFTM(WRK-IND)
              MOVE 4710S-IRSUMO-SIT-DESC(WRK-IND)
                                       TO WRK-OUT-SITANAL-DFTM(WRK-IND)
              IF  4710S-PARECER-FORML(WRK-IND) EQUAL 'S'
                  MOVE 'SIM'           TO WRK-OUT-PARECER-DFTM(WRK-IND)
              ELSE
                  MOVE 'NAO'           TO WRK-OUT-PARECER-DFTM(WRK-IND)
              END-IF
           ELSE
              MOVE WRK-225          TO WRK-OUT-SELEC-ATTR-DFTM(WRK-IND)
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA ACESSA O MODULO QUE TRAZ A LISTA DE OPERACOES     *
      *----------------------------------------------------------------*
       1120-ACESSAR-DCOM4710           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-COD-USER           TO 4710E-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO 4710E-CTERM
           MOVE 237                    TO 4710E-CBCO
           MOVE 'N'                    TO 4710E-START-COUNT
           MOVE ZEROS                  TO 4710E-FLAG
           MOVE 'CL'                   TO 4710E-FLAG-FILTRO
           MOVE 'DCOM4710'             TO WRK-MODULO.

           CALL WRK-MODULO             USING  4710-ENTRADA-ROTEADOR
                                              4710-SAIDA-ROTEADOR
                                              ERRO-AREA
                                              WRK-SQLCA.

           EVALUATE 4710S-COD-RETORNO
               WHEN '0000'
                    IF 4710S-DANO-OPER-DESC(6)
                                       EQUAL ZEROS
                       MOVE '*'        TO WRK-OUT-FIMAMOS-DFTM
                    ELSE
                       MOVE SPACES     TO WRK-OUT-FIMAMOS-DFTM
                    END-IF

                    PERFORM VARYING WRK-IND FROM 1 BY 1
                              UNTIL WRK-IND GREATER 5
                        INITIALIZE WRK-OUT-TABELA-DFTM(WRK-IND)
                        MOVE SPACES    TO WRK-OUT-OPERA-DFTM-X(WRK-IND)
                                          WRK-OUT-VALOR-DFTM-X(WRK-IND)
                    END-PERFORM

                    PERFORM 1110-MONTAR-TELA VARYING WRK-IND FROM
                            1 BY 1 UNTIL WRK-IND GREATER 5

                    MOVE 4710S-CAG-BCRIA-DESC(1) TO WRK-OUT-DESCAGE-DFTM

               WHEN '0003'
                    GO TO 1120-99-FIM

               WHEN '0099'
                    MOVE  'DB2'        TO ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                    MOVE  'APL'        TO ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-EVALUATE.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA PROCESSA A MENSAGEM VINDA DA TELA DCOMDFTM        *
      *----------------------------------------------------------------*
       1200-PROCESSAR-DCOMDFTM         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM             TO WRK-INPUT-DFTM.
           PERFORM 1210-DEVOLVER-TELA

           EVALUATE TRUE

               WHEN WRK-INP-PFK-DFTM     EQUAL 'H' OR
                   (WRK-INP-PFK-DFTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DFTM EQUAL 'PFK01')
                    IF   WRK-INP-FASE-DFTM    EQUAL '1'
                         PERFORM VARYING WRK-IND FROM 1 BY 1
                                 UNTIL WRK-IND GREATER 5
                             IF  WRK-INP-OPERA-DFTM-X(WRK-IND)
                                         EQUAL SPACES OR LOW-VALUES
                                MOVE WRK-225 TO WRK-OUT-SELEC-ATTR-DFTM
                                                              (WRK-IND)
                             END-IF
                         END-PERFORM
                         MOVE ZEROS    TO WRK-OUT-COMANDO-ATTR-DFTM
                    ELSE
                         PERFORM VARYING WRK-IND FROM 1 BY 1
                                 UNTIL WRK-IND GREATER 5
                           MOVE WRK-225 TO WRK-OUT-SELEC-ATTR-DFTM
                                                             (WRK-IND)
                         END-PERFORM
                         MOVE WRK-49353 TO WRK-OUT-COMANDO-ATTR-DFTM
                    END-IF
                    MOVE WRK-INP-MENSA-DFTM TO WRK-OUT-MENSA-DFTM

               WHEN WRK-INP-PFK-DFTM     EQUAL '3' OR
                   (WRK-INP-PFK-DFTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DFTM EQUAL 'PFK03')
                    PERFORM 1220-RETORNAR-CHAMADOR

               WHEN WRK-INP-PFK-DFTM     EQUAL 'X' OR
                   (WRK-INP-PFK-DFTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DFTM EQUAL 'PFK05')
                    PERFORM 1230-RETORNAR-MENU-ROTINAS

               WHEN WRK-INP-PFK-DFTM     EQUAL 'Z' OR
                   (WRK-INP-PFK-DFTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DFTM EQUAL 'PFK10')
                    PERFORM 1240-RETORNAR-MENU-DCOM

               WHEN WRK-INP-FASE-DFTM    EQUAL '1'
                    PERFORM 1201-TRATAR-FASE-1

               WHEN WRK-INP-FASE-DFTM    EQUAL '2'
                    PERFORM 1202-TRATAR-FASE-2
               WHEN OTHER
                    MOVE 'PF INVALIDA'   TO WRK-OUT-MENSA-DFTM
                    PERFORM VARYING WRK-IND FROM 1 BY 1
                              UNTIL WRK-IND GREATER 5
                        IF  WRK-INP-OPERA-DFTM-X(WRK-IND)
                                       EQUAL SPACES OR LOW-VALUES
                            MOVE WRK-225 TO WRK-OUT-SELEC-ATTR-DFTM
                                            (WRK-IND)
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
               WHEN WRK-INP-PFK-DFTM     EQUAL '7' OR
                   (WRK-INP-PFK-DFTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DFTM EQUAL 'PFK7')
                    PERFORM 1260-TRATAR-VOLTA-PAG

               WHEN WRK-INP-PFK-DFTM     EQUAL '8' OR
                   (WRK-INP-PFK-DFTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DFTM EQUAL 'PFK8')
                    PERFORM 1270-TRATAR-AVANCA-PAG

               WHEN WRK-INP-PFK-DFTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DFTM EQUAL  SPACES OR LOW-VALUES
                    PERFORM 1280-TRATAR-ENTER
                    IF  WRK-INCONSISTENCIA EQUAL SPACES
                        IF  ACU-SEL-D GREATER ZEROS
                            PERFORM 1285-CHAMAR-DCOM0408
                        ELSE
                            PERFORM VARYING WRK-IND FROM 1 BY 1
                              UNTIL WRK-IND GREATER 5
                                MOVE WRK-225 TO WRK-OUT-SELEC-ATTR-DFTM
                                                (WRK-IND)
                            END-PERFORM
                            MOVE WRK-49345 TO WRK-OUT-COMANDO-ATTR-DFTM
                            MOVE '2'       TO WRK-OUT-FASE-DFTM
                            MOVE 'TECLE <PF6> PARA CONFIRMAR OU TECLE <E
      -                          'NTER PARA ALTERAR'
                                           TO WRK-OUT-MENSA-DFTM
                        END-IF
                    ELSE
                        PERFORM VARYING WRK-IND FROM 1 BY 1
                                  UNTIL WRK-IND GREATER 5
                            IF  WRK-INP-OPERA-DFTM-X(WRK-IND)
                                           EQUAL SPACES OR LOW-VALUES
                                MOVE WRK-225 TO WRK-OUT-SELEC-ATTR-DFTM
                                                (WRK-IND)
                            END-IF
                        END-PERFORM
                    END-IF

               WHEN OTHER
                    MOVE 'PF INVALIDA'   TO WRK-OUT-MENSA-DFTM
                    PERFORM VARYING WRK-IND FROM 1 BY 1
                              UNTIL WRK-IND GREATER 5
                        IF  WRK-INP-OPERA-DFTM-X(WRK-IND)
                                       EQUAL SPACES OR LOW-VALUES
                            MOVE WRK-225 TO WRK-OUT-SELEC-ATTR-DFTM
                                            (WRK-IND)
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
               WHEN WRK-INP-PFK-DFTM     EQUAL '6' OR
                   (WRK-INP-PFK-DFTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DFTM EQUAL 'PFK6')
DTS002              MOVE 1               TO WRK-COMU-OCORR-0499
                    PERFORM 1250-TRATAR-CONFIRMA
                    MOVE ZEROS           TO WRK-OUT-COMANDO-ATTR-DFTM

               WHEN WRK-INP-PFK-DFTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DFTM EQUAL  SPACES OR LOW-VALUES
                    PERFORM VARYING WRK-IND FROM 1 BY 1
                              UNTIL WRK-IND GREATER 5
                        IF  WRK-INP-OPERA-DFTM-X(WRK-IND)
                                         EQUAL SPACES OR LOW-VALUES
                            MOVE WRK-225 TO WRK-OUT-SELEC-ATTR-DFTM
                                            (WRK-IND)
                        END-IF
                    END-PERFORM
                    MOVE '1'             TO WRK-OUT-FASE-DFTM
                    MOVE 'SELECIONE COM "D" PARA DETALHAR OU COM "C" PAR
      -                  'A CANCELAR'
                                         TO WRK-OUT-MENSA-DFTM
                    MOVE ZEROS           TO WRK-OUT-COMANDO-ATTR-DFTM

              WHEN OTHER
                    MOVE 'PF INVALIDA'   TO WRK-OUT-MENSA-DFTM
                    PERFORM VARYING WRK-IND FROM 1 BY 1
                              UNTIL WRK-IND GREATER 5
                        MOVE WRK-225     TO WRK-OUT-SELEC-ATTR-DFTM
                                            (WRK-IND)
                    END-PERFORM
                    MOVE WRK-49345       TO WRK-OUT-COMANDO-ATTR-DFTM

           END-EVALUATE.

      *----------------------------------------------------------------*
       1202-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA DEVOLVE OS DADOS DA TELA SEM ALTERACAO            *
      *----------------------------------------------------------------*
       1210-DEVOLVER-TELA              SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-FIMAMOS-DFTM   TO WRK-OUT-FIMAMOS-DFTM.
           MOVE WRK-INP-FASE-DFTM      TO WRK-OUT-FASE-DFTM.
           MOVE WRK-INP-PAGINA-DFTM-X  TO WRK-OUT-PAGINA-DFTM-X.
           MOVE WRK-INP-CODAGE-DFTM-X  TO WRK-OUT-CODAGE-DFTM-X.
           MOVE WRK-INP-DESCAGE-DFTM   TO WRK-OUT-DESCAGE-DFTM.
           MOVE WRK-INP-TMSTAMP-DFTM   TO WRK-OUT-TMSTAMP-DFTM.

           PERFORM VARYING WRK-IND     FROM 1 BY 1
                   UNTIL WRK-IND GREATER 5
              MOVE WRK-INP-SELEC-DFTM(WRK-IND)
                                       TO WRK-OUT-SELEC-DFTM(WRK-IND)
              MOVE WRK-INP-OPERA-DFTM-X(WRK-IND)
                                       TO WRK-OUT-OPERA-DFTM-X(WRK-IND)
              MOVE WRK-INP-PROD-DFTM   (WRK-IND)
                                       TO WRK-OUT-PROD-DFTM   (WRK-IND)
              MOVE WRK-INP-SUBPROD-DFTM(WRK-IND)
                                       TO WRK-OUT-SUBPROD-DFTM(WRK-IND)
              MOVE WRK-INP-DTOPER-DFTM (WRK-IND)
                                       TO WRK-OUT-DTOPER-DFTM (WRK-IND)
              MOVE WRK-INP-VALOR-DFTM   (WRK-IND)
                                       TO WRK-OUT-VALOR-DFTM-X(WRK-IND)
              MOVE WRK-INP-MEIOENT-DFTM(WRK-IND)
                                       TO WRK-OUT-MEIOENT-DFTM(WRK-IND)
              MOVE WRK-INP-SITANAL-DFTM(WRK-IND)
                                       TO WRK-OUT-SITANAL-DFTM(WRK-IND)
              MOVE WRK-INP-PARECER-DFTM(WRK-IND)
                                       TO WRK-OUT-PARECER-DFTM(WRK-IND)
           END-PERFORM.


      *----------------------------------------------------------------*
       1210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA RETORNA AO PROGRAMA CHAMADOR                      *
      *----------------------------------------------------------------*
       1220-RETORNAR-CHAMADOR          SECTION.
      *----------------------------------------------------------------*

           INITIALIZE COMU-AREA-UN.

           MOVE LENGTH OF COMU-AREA-UN
                                     TO COMU-LL-UN
           MOVE ZEROS                TO COMU-ZZ-UN.

           IF  WRK-FLAG-ERRO         EQUAL 'S'
               MOVE WRK-MENSAGEM-ERRO
                                     TO COMU-MENSAGEM-UN
               IF  WRK-TRANSACAO     EQUAL 'DCOM0402'
                   MOVE WRK-INP-CODAGE-DFTM
                                     TO COMU-AGENCIA-UN
                   MOVE 2            TO COMU-OPCAO-UN
               END-IF
           END-IF.

           MOVE WRK-CHNG             TO WRK-FUNCAO
           MOVE 'DCOM0402'           TO WRK-TELA
           MOVE 'DCOM0406'           TO COMU-TRANSACAO-UN
           MOVE WRK-INP-TMSTAMP-DFTM TO COMU-TIMESTAMP-UN
           MOVE COMU-AREA-UN         TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRATAMENTO DE RETORNO AO MENU ROTINAS    *
      *----------------------------------------------------------------*
       1230-RETORNAR-MENU-ROTINAS      SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-CHNG            TO WRK-FUNCAO
           MOVE 'DCOM0406'          TO WRK-TELA
                                       COMU-TRANSACAO-UN
           MOVE '5'                 TO COMU-PFK-UN
           MOVE COMU-AREA-UN        TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRATAMENTO DE RETORNO AO MENU DCOM       *
      *----------------------------------------------------------------*
       1240-RETORNAR-MENU-DCOM         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-CHNG               TO WRK-FUNCAO
           MOVE 'DCOM0406'             TO WRK-TELA
                                          COMU-TRANSACAO-UN
           MOVE 'A'                    TO COMU-PFK-UN
           MOVE COMU-AREA-UN           TO WRK-MENSAGEM.
      *----------------------------------------------------------------*
       1240-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRAMAMENTO DO COMANDO PFK06              *
      *----------------------------------------------------------------*
       1250-TRATAR-CONFIRMA            SECTION.
      *----------------------------------------------------------------*

DTS002     PERFORM
DTS002     VARYING WRK-IND             FROM WRK-COMU-OCORR-0499 BY 1
DTS002       UNTIL WRK-IND             GREATER 5
               IF  WRK-INP-SELEC-DFTM(WRK-IND) EQUAL 'C'
                   PERFORM 1284-CHAMAR-DCOM5111

DTS002              IF WRK-INCONSISTENCIA  EQUAL SPACES
DTS002                 PERFORM 1253-CHAMAR-TRANS-COMMIT
DTS002                 GO TO 1250-99-FIM
DTS002              END-IF
               END-IF
           END-PERFORM.

           MOVE SPACES                 TO WRK-OUT-FIMAMOS-DFTM
                                          WRK-OUT-SELEC-DFTM(1)
                                          WRK-OUT-SELEC-DFTM(2)
                                          WRK-OUT-SELEC-DFTM(3)
                                          WRK-OUT-SELEC-DFTM(4)
                                          WRK-OUT-SELEC-DFTM(5).

           MOVE '1'                    TO WRK-OUT-FASE-DFTM

           INITIALIZE 4710-ENTRADA-ROTEADOR
                      4710-SAIDA-ROTEADOR.

           MOVE WRK-INP-CODAGE-DFTM    TO 4710E-CAG-BCRIA
           COMPUTE 4710E-RESTART = (WRK-INP-PAGINA-DFTM - 1) * 5

           PERFORM 1120-ACESSAR-DCOM4710.

           IF  4710S-COD-RETORNO       EQUAL '0003'
           AND WRK-INP-PAGINA-DFTM     GREATER 1
               INITIALIZE 4710-ENTRADA-ROTEADOR
                          4710-SAIDA-ROTEADOR

               MOVE WRK-INP-CODAGE-DFTM
                                       TO 4710E-CAG-BCRIA
               MOVE ZEROS              TO 4710E-RESTART
               MOVE 1                  TO WRK-OUT-PAGINA-DFTM
               PERFORM 1120-ACESSAR-DCOM4710
           END-IF.

           IF  4710S-COD-RETORNO       EQUAL '0003'
               MOVE 'CANCELAMENTO(S) EFETUADO(S) COM SUCESSO - C/C DEBIT
      -             'ADA'
                                       TO WRK-MENSAGEM-ERRO
               MOVE 'S'                TO WRK-FLAG-ERRO
               PERFORM 1220-RETORNAR-CHAMADOR
           ELSE
               IF  WRK-INCONSISTENCIA  EQUAL SPACES
                   MOVE 'CANCELAMENTO(S) EFETUADO(S) COM SUCESSO - C/C D
      -                 'EBITADA'
                                       TO WRK-OUT-MENSA-DFTM
               ELSE
                   MOVE WRK-MENSAGEM-ERRO
                                       TO WRK-OUT-MENSA-DFTM
               END-IF
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
DTS002     MOVE WRK-INP-TMSTAMP-DFTM   TO COMU-TIMESTAMP-UN.
DTS002     MOVE WRK-INP-PAGINA-DFTM    TO COMU-PAGINA-UN.
DTS002     MOVE WRK-INP-CODAGE-DFTM    TO COMU-AGENCIA-UN.
DTS002     MOVE WRK-INPUT-DFTM         TO WRK-COMU-INPUT-0499.
DTS002     MOVE WRK-IND                TO WRK-COMU-OCORR-0499.
DTS002
DTS002     MOVE 'DCOM0406'             TO COMU-TRANSACAO-UN.
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
       1260-TRATAR-VOLTA-PAG           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE 4710-ENTRADA-ROTEADOR
                      4710-SAIDA-ROTEADOR.

           IF WRK-INP-PAGINA-DFTM      EQUAL  1
              MOVE 'NAO EXISTE PAGINA PARA VOLTAR'
                                       TO WRK-OUT-MENSA-DFTM
              PERFORM VARYING WRK-IND FROM 1 BY 1
                        UNTIL WRK-IND GREATER 5
                  IF  WRK-INP-OPERA-DFTM-X(WRK-IND)
                                       EQUAL SPACES OR LOW-VALUES
                      MOVE WRK-225 TO WRK-OUT-SELEC-ATTR-DFTM(WRK-IND)
                  END-IF
              END-PERFORM
              GO TO 1260-99-FIM
           END-IF.

           IF  WRK-INP-MENSA-DFTM      NOT EQUAL WRK-MENSA-PF7
               IF (WRK-INP-SELEC-DFTM(1) EQUAL 'D' OR 'C')
               OR (WRK-INP-SELEC-DFTM(2) EQUAL 'D' OR 'C')
               OR (WRK-INP-SELEC-DFTM(3) EQUAL 'D' OR 'C')
               OR (WRK-INP-SELEC-DFTM(4) EQUAL 'D' OR 'C')
               OR (WRK-INP-SELEC-DFTM(5) EQUAL 'D' OR 'C')
                   MOVE WRK-MENSA-PF7 TO WRK-OUT-MENSA-DFTM
                   GO TO 1260-99-FIM
               END-IF
           END-IF

           MOVE SPACES                 TO WRK-OUT-FIMAMOS-DFTM

           COMPUTE 4710E-RESTART   = (WRK-INP-PAGINA-DFTM - 2) * 5
           MOVE WRK-INP-CODAGE-DFTM    TO 4710E-CAG-BCRIA

           PERFORM 1120-ACESSAR-DCOM4710.

           COMPUTE WRK-OUT-PAGINA-DFTM = WRK-OUT-PAGINA-DFTM - 1
           MOVE 'AMOSTRAGEM CONTINUA'  TO  WRK-OUT-MENSA-DFTM.

      *----------------------------------------------------------------*
       1260-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRAMAMENTO DO COMANDO PFK08              *
      *----------------------------------------------------------------*
       1270-TRATAR-AVANCA-PAG          SECTION.
      *----------------------------------------------------------------*

           INITIALIZE 4710-ENTRADA-ROTEADOR
                      4710-SAIDA-ROTEADOR.

           IF WRK-INP-FIMAMOS-DFTM  EQUAL  '*'
              MOVE 'NAO EXISTE PAGINA PARA AVANCAR'
                                       TO WRK-OUT-MENSA-DFTM
              PERFORM VARYING WRK-IND FROM 1 BY 1
                        UNTIL WRK-IND GREATER 5
                  IF  WRK-INP-OPERA-DFTM-X(WRK-IND)
                                       EQUAL SPACES OR LOW-VALUES
                      MOVE WRK-225 TO WRK-OUT-SELEC-ATTR-DFTM(WRK-IND)
                  END-IF
              END-PERFORM
              GO TO 1270-99-FIM
           END-IF.

           IF  WRK-INP-MENSA-DFTM      NOT EQUAL WRK-MENSA-PF8
               IF (WRK-INP-SELEC-DFTM(1) EQUAL 'D' OR 'C')
               OR (WRK-INP-SELEC-DFTM(2) EQUAL 'D' OR 'C')
               OR (WRK-INP-SELEC-DFTM(3) EQUAL 'D' OR 'C')
               OR (WRK-INP-SELEC-DFTM(4) EQUAL 'D' OR 'C')
               OR (WRK-INP-SELEC-DFTM(5) EQUAL 'D' OR 'C')
                   MOVE WRK-MENSA-PF8 TO WRK-OUT-MENSA-DFTM
                   GO TO 1270-99-FIM
               END-IF
           END-IF

           COMPUTE 4710E-RESTART = (WRK-INP-PAGINA-DFTM * 5).
           MOVE WRK-INP-CODAGE-DFTM    TO 4710E-CAG-BCRIA

           PERFORM 1120-ACESSAR-DCOM4710.

           IF  WRK-OUT-FIMAMOS-DFTM    NOT EQUAL SPACES
               MOVE 'FIM DE AMOSTRAGEM' TO WRK-OUT-MENSA-DFTM
           ELSE
               MOVE 'AMOSTRAGEM CONTINUA'
                                       TO WRK-OUT-MENSA-DFTM
           END-IF.

           ADD 1                       TO WRK-OUT-PAGINA-DFTM.

      *----------------------------------------------------------------*
       1270-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRAMAMENTO DO COMANDO ENTER              *
      *----------------------------------------------------------------*
       1280-TRATAR-ENTER               SECTION.
      *----------------------------------------------------------------*

           IF (WRK-INP-SELEC-DFTM(1) EQUAL SPACES OR LOW-VALUES)
           AND(WRK-INP-SELEC-DFTM(2) EQUAL SPACES OR LOW-VALUES)
           AND(WRK-INP-SELEC-DFTM(3) EQUAL SPACES OR LOW-VALUES)
           AND(WRK-INP-SELEC-DFTM(4) EQUAL SPACES OR LOW-VALUES)
           AND(WRK-INP-SELEC-DFTM(5) EQUAL SPACES OR LOW-VALUES)
               MOVE 'NENHUMA OPCAO FOI SELECIONADA'
                                       TO  WRK-OUT-MENSA-DFTM
               MOVE '*'                TO WRK-INCONSISTENCIA
               GO TO 1280-99-FIM
           END-IF.

           PERFORM VARYING WRK-IND     FROM 1 BY 1
                                       UNTIL WRK-IND GREATER 5
             IF  WRK-INP-SELEC-DFTM(WRK-IND)
                                       NOT EQUAL 'D' AND 'C' AND
                                                 SPACES AND LOW-VALUES
                 MOVE '*'              TO WRK-INCONSISTENCIA
                 MOVE WRK-49353     TO WRK-OUT-SELEC-ATTR-DFTM(WRK-IND)
                 MOVE 'OPCAO INVALIDA - SELECIONAR APENAS: D/C'
                                       TO  WRK-OUT-MENSA-DFTM
7C0922       ELSE
.                MOVE WRK-IND TO WRK-IND1
.                IF  WRK-INP-SELEC-DFTM(WRK-IND1)   EQUAL  'C' AND
.                   (WRK-INP-SUBPROD-DFTM(WRK-IND1) EQUAL 'NPR - RO ' OR
.                    WRK-INP-SUBPROD-DFTM(WRK-IND1) EQUAL 'NPR - RPL')
.                   PERFORM 1900-OBTER-MSGEM-COR0005R1
.                END-IF
7C0922       END-IF
           END-PERFORM.

           IF  WRK-INCONSISTENCIA      NOT EQUAL SPACES
               GO TO 1280-99-FIM
           END-IF.

           MOVE ZEROS                  TO ACU-SEL-D

           PERFORM VARYING WRK-IND FROM 1 BY 1
                                   UNTIL WRK-IND GREATER 5
               IF  WRK-INP-SELEC-DFTM(WRK-IND) EQUAL 'D'
                   ADD 1               TO ACU-SEL-D
               END-IF
           END-PERFORM.

           IF  ACU-SEL-D               GREATER 1
               MOVE '*'                TO WRK-INCONSISTENCIA
               PERFORM VARYING WRK-IND FROM 1 BY 1
                                      UNTIL WRK-IND GREATER 5
                   IF  WRK-INP-SELEC-DFTM(WRK-IND) NOT EQUAL SPACES
                                                         AND LOW-VALUES

                       MOVE WRK-49353
                               TO WRK-OUT-SELEC-ATTR-DFTM(WRK-IND)
                   END-IF
               END-PERFORM
               MOVE 'SELECIONAR APENAS UMA OCORRENCIA PARA OPCAO "D"'
                               TO          WRK-OUT-MENSA-DFTM
               GO TO 1280-99-FIM
           END-IF.

           MOVE SPACES                 TO WRK-SELEC-ANT

           PERFORM VARYING WRK-IND     FROM 1 BY 1
                                       UNTIL WRK-IND GREATER 5
             IF  WRK-INP-SELEC-DFTM(WRK-IND)
                                       NOT EQUAL WRK-SELEC-ANT AND
                 WRK-SELEC-ANT         NOT EQUAL SPACES
                 IF  WRK-INP-SELEC-DFTM(WRK-IND)  NOT EQUAL  SPACES
                     MOVE '*'          TO WRK-INCONSISTENCIA
                 END-IF
             ELSE
                 MOVE WRK-INP-SELEC-DFTM(WRK-IND) TO WRK-SELEC-ANT
             END-IF
           END-PERFORM.

           IF  WRK-INCONSISTENCIA      NOT EQUAL SPACES
               PERFORM VARYING WRK-IND FROM 1 BY 1
                                      UNTIL WRK-IND GREATER 5
                   IF  WRK-INP-SELEC-DFTM(WRK-IND) NOT EQUAL SPACES
                                                         AND LOW-VALUES

                       MOVE WRK-49353
                               TO WRK-OUT-SELEC-ATTR-DFTM(WRK-IND)
                   END-IF
               END-PERFORM
               MOVE 'SELECIONAR APENAS UMA OPCAO: D/C'
                                       TO  WRK-OUT-MENSA-DFTM
               GO TO 1280-99-FIM
           END-IF.

      *----------------------------------------------------------------*
       1280-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA CHAMA O MODULO DCOM5111 PARA CANCELAR LIBERACAO.  *
      *----------------------------------------------------------------*
       1284-CHAMAR-DCOM5111            SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-5111E-ENTRADA.

           MOVE 'DCOM5111'             TO WRK-MODULO.
           MOVE SPACES                 TO WRK-5111E-COD-RETORNO.
           MOVE SPACES                 TO WRK-5111E-MSG-RETORNO.
           MOVE ZEROS                  TO WRK-5111E-RESTART.
           MOVE SPACES                 TO WRK-5111E-FLAG.
           MOVE WRK-COD-USER           TO WRK-5111E-CFUNC-BDSCO.
           MOVE LNK-IO-LTERM           TO WRK-5111E-CTERM.
           MOVE WRK-INP-OPERA-DFTM(WRK-IND)(1:4)
                                       TO WRK-5111E-DANO-OPER-DESC
           MOVE WRK-INP-OPERA-DFTM(WRK-IND)(5:9)
                                       TO WRK-5111E-NSEQ-OPER-DESC
           INITIALIZE WRK-5111S-SAIDA.

           CALL WRK-MODULO             USING WRK-5111E-ENTRADA
                                             WRK-5111S-SAIDA
                                             ERRO-AREA
                                             WRK-SQLCA.

           IF  WRK-5111S-COD-RETORNO   NOT EQUAL '0000'
               IF  WRK-5111S-COD-RETORNO EQUAL '0099'
                   MOVE  'DB2'         TO ERR-TIPO-ACESSO
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO
               ELSE
                   IF  WRK-5111S-COD-RETORNO EQUAL '7777'
                       MOVE 'APL'            TO ERR-TIPO-ACESSO
                       PERFORM 9999-PROCESSAR-ROTINA-ERRO
                   ELSE
DTS001                 PERFORM 8888-EXECUTAR-ROLLBACK
DTS001
                       STRING WRK-INP-OPERA-DFTM(WRK-IND)
                          '-' WRK-5111S-MSG-RETORNO
                       DELIMITED BY SIZE INTO WRK-MENSAGEM-ERRO
                       MOVE WRK-49369
                               TO WRK-OUT-SELEC-ATTR-DFTM(WRK-IND)
                       MOVE 6            TO WRK-IND
                       MOVE '*'          TO WRK-INCONSISTENCIA
                   END-IF
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1284-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1285-CHAMAR-DCOM0408            SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-CHNG               TO WRK-FUNCAO.
           MOVE 'DCOM0408'             TO WRK-TELA.
           MOVE 'DCOM0406'             TO COMU-TRANSACAO-UN
           MOVE WRK-INP-SENHAS-DFTM    TO COMU-SENHAS-UN
           MOVE WRK-INP-TMSTAMP-DFTM   TO COMU-TIMESTAMP-UN
           MOVE WRK-INP-CODAGE-DFTM    TO COMU-AGENCIA-UN
           MOVE WRK-INP-PAGINA-DFTM    TO COMU-PAGINA-UN

           PERFORM VARYING WRK-IND FROM 1 BY 1
                     UNTIL WRK-IND GREATER 5
               IF  WRK-INP-SELEC-DFTM(WRK-IND) EQUAL 'D'
                   MOVE WRK-INP-OPERA-DFTM(WRK-IND)(1:4)
                                       TO COMU-DANO-OPER-UN
                   MOVE WRK-INP-OPERA-DFTM(WRK-IND)(5:9)
                                       TO COMU-NSEQ-OPER-UN
               END-IF
           END-PERFORM.

           MOVE COMU-AREA-UN           TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1285-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA CARREGA A TELA QUE SERA DEMONSTRADA PELO IMS.     *
      *----------------------------------------------------------------*
       1300-PROCESSAR-BRAD0660         SECTION.
      *----------------------------------------------------------------*

           CALL 'BRAD0660'             USING WRK-OUTPUT-DFTM
                                             WRK-660-DCOMDFTM.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE RETURN-CODE        TO WRK-RETURN-CODE
               MOVE 10                 TO WRK-LOCAL-ERRO
               MOVE WRK-ERRO-BRAD0660  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-ISRT               TO WRK-FUNCAO.
           MOVE WRK-OUTPUT-DFTM        TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

7C0922*----------------------------------------------------------------*
.      1900-OBTER-MSGEM-COR0005R1      SECTION.
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
.                  MOVE WRK-49353            TO WRK-OUT-SELEC-ATTR-DFTM
.                                              (WRK-IND1)
.                  MOVE 'CANCELAMENTO NAO AUTORIZADO - COR0005 ENVIADO'
.                                            TO  WRK-OUT-MENSA-DFTM
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
.      1900-99-FIM.                    EXIT.
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
               MOVE 'DCOM0406'         TO ERR-MODULO
           ELSE
               MOVE 'DCOM0406'         TO ERR-PGM
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
