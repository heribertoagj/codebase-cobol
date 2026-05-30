      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. DCOM0417.
       AUTHOR.     MARCELO MORINA.
      *================================================================*
      *                          A L T R A N                           *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   DCOM0417                                    *
      *    PROGRAMADOR.:   MARCELO MORINA MARQUES                      *
      *    ANALISTA....:   MARCELO MORINA MARQUES                      *
      *    DATA........:   07/02/2007                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   CANCELAR LIBERACAO DE CONTRATO -            *
      *                    CONTRATOS POR AGENCIA.                      *
      *----------------------------------------------------------------*
      *    TELAS.......:                                               *
      *    DCOMDQTM - CANCELAR LIBERACAO DE CONTRATO -                 *
      *               CONTRATOS POR AGENCIA.                           *
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
      *    CHAMADO POR.: DCOM0413           CHAMA: DCOM0419            *
      *================================================================*
WIP001*                     A L T E R A C A O                          *
WIP001*----------------------------------------------------------------*
WIP001*  ANALISTA......:   FERNANDO LUIZ DE SANTI                      *
WIP001*  ANALISTA DS...:   PAULO ROBERTO        - GRUPO 39             *
WIP001*  DATA..........:   25/02/2021                                  *
WIP001*----------------------------------------------------------------*
WIP001*  OBJETIVO......:   VALIDAR SE CONTRATO LIMITE ESTA LIGADO A    *
WIP001*    ALGUMA OPERACAO, POR CAUSA DO OPTIN.                        *
WIP001*----------------------------------------------------------------*
WIP001*  MODULO........:                                               *
WIP001*  DCOM5124 - VER SE CONTRATO LIMITE TEM OPERACAO.               *
WIP001*================================================================*
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
           '*** DCOM0417 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ATRIBUTOS ***'.
      *---------------------------------------------------------------*
      *--- NUMERIC  - NORMAL    - PROTEGIDO    - NAO CURSOR - NUM  ---*
       01  WRK-225                     PIC S9(008) COMP    VALUE +225.
      *--- NUMERIC  - NORMAL    - PROTEGIDO    - NAO CURSOR - NUM  ---*
       01  WRK-241                     PIC S9(008) COMP    VALUE +241.
      *--- NUMERICO - NORMAL    - PROTEGIDO    - POS CURSOR - NUM  ---*
       01  WRK-49361                   PIC S9(008) COMP    VALUE +49361.
      *--- NUMERICO - BRILHANTE - DESPROTEGIDO - POS CURSOR - NUM  ---*
       01  WRK-49369                   PIC S9(008) COMP    VALUE +49369.
      *--- BRILHANTE, NORMAL    - DEPROTEGIDO  - POS CURSOR - ALFA ---*
       01  WRK-49353                   PIC S9(008) COMP    VALUE +49353.
      *--- BRILHANTE, NORMAL    - DEPROTEGIDO  - POS CURSOR - ALFA ---*
       01  WRK-49345                   PIC S9(008) COMP    VALUE +49345.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*
       01  WRK-AUXILIARES.
WIP001     05  WRK-IND                 PIC  9(002) COMP-3  VALUE ZEROS.
           05  WRK-QTD-OPCAO-D         PIC  9(001)         VALUE ZEROS.
           05  WRK-QTD-SPACES          PIC  9(001)         VALUE ZEROS.
           05  WRK-SQLCA               PIC  X(136)         VALUE SPACES.
           05  WRK-MODULO              PIC  X(008)         VALUE SPACES.
           05  WRK-SELEC-ANT           PIC  X(001)         VALUE SPACES.
           05  WRK-QTD-OPCAO-L         PIC  9(003)         VALUE ZEROS.
           05  WRK-QTD-OPCAO-C         PIC  9(003)         VALUE ZEROS.
           05  WRK-INCONSISTENCIA      PIC  X(001)         VALUE SPACES.
           05  WRK-CANCELADOS          PIC  X(001)         VALUE 'N'.

           05  WRK-MENSA-PF7           PIC  X(079) VALUE
               'PF7 DESPREZARA AS ATUALIZACOES EFETUADAS - TECLE NOVAMEN
      -        'TE PARA VOLTAR'.

           05  WRK-MENSA-PF8           PIC  X(079) VALUE
               'PF8 DESPREZARA AS ATUALIZACOES EFETUADAS - TECLE NOVAMEN
      -        'TE PARA AVANCAR'.

        01  WRK-MENS-4128.
            05  WRK-RET-4128            PIC  X(004)       VALUE SPACES.
            05  WRK-TRACO-4128          PIC  X(001)       VALUE '-'.
            05  WRK-MENS-4128           PIC  X(074)       VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DE COMUNICACAO DCOM4128 ***'.
      *----------------------------------------------------------------*

       01  WRK-4128E-ENTRADA.
           03  WRK-4128E-COD-RETORNO            PIC  X(004).
           03  WRK-4128E-MSG-RETORNO            PIC  X(079).
           03  WRK-4128E-RESTART                PIC  9(005).
           03  WRK-4128E-FLAG                   PIC  X(001).
           03  WRK-4128E-CFUNC-BDSCO            PIC  9(009).
           03  WRK-4128E-CTERM                  PIC  X(008).

           03  WRK-4128E-CCONTR-LIM-DESC        PIC  9(009).
           03  WRK-4128E-CVRSAO-CONTR-LIM       PIC  9(003).
           03  WRK-4128E-FLAG-FILTRO            PIC  X(001).

       01  WRK-4128S-SAIDA.
           03  WRK-4128S-HEADER.
               05  WRK-4128S-COD-RETORNO        PIC  X(004).
               05  WRK-4128S-MSG-RETORNO        PIC  X(079).
               05  WRK-4128S-RESTART            PIC  9(005).
               05  WRK-4128S-FLAG               PIC  X(001).
           03  WRK-4128S-DADOS.
               05  WRK-4128S-CPRODT             PIC  9(003).
               05  WRK-4128S-ISIT-DESC-COML     PIC  X(030).
               05  WRK-4128S-IRSUMO-SIT-DESC    PIC  X(015).
FS2511*        05  WRK-4128S-CCNPJ-CPF          PIC  9(009).
FS2511*        05  WRK-4128S-CFLIAL-CNPJ        PIC  9(004).
FS2511         05  WRK-4128S-CCNPJ-CPF          PIC  X(009).
FS2511         05  WRK-4128S-CFLIAL-CNPJ        PIC  X(004).
               05  WRK-4128S-CCTRL-CNPJ         PIC  9(002).
               05  WRK-4128S-IPSSOA-DESC-COML   PIC  X(060).
               05  WRK-4128S-CBCO               PIC  9(003).
               05  WRK-4128S-CBCO-DESC          PIC  X(020).
               05  WRK-4128S-CAG-BCRIA          PIC  9(005).
               05  WRK-4128S-CAG-BCRIA-DESC     PIC  X(030).
               05  WRK-4128S-CCTA-BCRIA-CLI     PIC  9(013).
               05  WRK-4128S-DINIC-VGCIA-CONTR  PIC  X(010).
               05  WRK-4128S-CINDCD-RENOV-AUTOM PIC  X(001).
               05  WRK-4128S-DVCTO-CONTR-LIM    PIC  X(010).
               05  WRK-4128S-TCONTR-LIM-DESC    PIC  9(005).
               05  WRK-4128S-VCONTR-LIM-DESC    PIC  9(015)V99.
               05  WRK-4128S-VDISPN-CONTR-LIM   PIC  9(015)V99.
               05  WRK-4128S-CTPO-COBR-TAC      PIC  9(003).
               05  WRK-4128S-ITPO-COBR-TAC      PIC  X(030).
               05  WRK-4128S-VTAC-CONTR-LIM     PIC  9(015)V99.
               05  WRK-4128S-PARECER            PIC  X(240).
               05  WRK-4128S-CGARAN             PIC  9(003).
               05  WRK-4128S-IGARAN             PIC  X(040).
               05  WRK-4128S-VUTILZ-LIM-DESC    PIC  9(15)V99.

           03  WRK-4128S-CONSISTENCIA REDEFINES WRK-4128S-DADOS.

               05  WRK-4128S-TAMANHO-ERRO       PIC  9(001).
               05  WRK-4128S-TABELA-ERROS       OCCURS 01 TIMES.
                   07  WRK-4128S-CAMPOS-ERRO    PIC  9(001).

       01  WRK-4128S-CSPROD-DESC-COML     PIC  9(003) VALUE ZEROS.

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
           '*** AREA DA TELA DCOMDQTM  - INPUT ***'.
      *----------------------------------------------------------------*
       01  WRK-INPUT-DQTM.
           05  FILLER                  PIC  X(018) VALUE SPACES.
           05  WRK-INP-PFK-DQTM        PIC  X(001) VALUE SPACES.
           05  WRK-INP-SENHAS-DQTM     PIC  X(037) VALUE SPACES.
           05  WRK-INP-COMANDO-DQTM    PIC  X(068) VALUE SPACES.
           05  WRK-INP-DADOS-DQTM.
               10  WRK-INP-FASE-DQTM-X.
                   15  WRK-INP-FASE-DQTM
                                        PIC  9(001) VALUE ZEROS.
               10  WRK-INP-TPOOPC-DQTM  PIC  X(001) VALUE SPACES.
               10  WRK-INP-TMSTAMP-DQTM PIC  X(026) VALUE SPACES.
               10  WRK-INP-FIMAMOS-DQTM PIC  X(001) VALUE SPACES.
               10  WRK-INP-PAGINA-DQTM-X.
                   15  WRK-INP-PAGINA-DQTM
                                        PIC  9(003) VALUE ZEROS.
               10  WRK-INP-CODAGE-DQTM-X.
                   15  WRK-INP-CODAGE-DQTM
                                        PIC  9(005) VALUE ZEROS.
               10  WRK-INP-DESCAGE-DQTM PIC  X(020) VALUE SPACES.
               10  WRK-INP-TABELA-DQTM    OCCURS  05  TIMES.
                   15  WRK-INP-SELEC-DQTM     PIC  X(001) VALUE SPACES.
                   15  WRK-INP-CONTR-DQTM-X.
                       20  WRK-INP-CONTR-DQTM PIC  9(009) VALUE ZEROS.
                   15  WRK-INP-NOME-DQTM      PIC  X(030) VALUE SPACES.
                   15  WRK-INP-DTINIC-DQTM    PIC  X(010) VALUE SPACES.
                   15  WRK-INP-DTVENCT-DQTM   PIC  X(010) VALUE SPACES.
                   15  WRK-INP-LIMITE-DQTM    PIC  X(017) VALUE SPACES.
                   15  WRK-INP-SITUAC-DQTM    PIC  X(020) VALUE SPACES.
           05  WRK-INP-MENSA-DQTM       PIC  X(079) VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                       PIC  X(050)         VALUE
           '*** AREA DA TELA DCOMDQTM  - OUTPUT ***'.
      *----------------------------------------------------------------*
       01  WRK-OUTPUT-DQTM.
           05  WRK-OUT-LL-DQTM          PIC S9(004) COMP VALUE +0740.
           05  WRK-OUT-ZZ-DQTM          PIC  9(004) COMP VALUE ZEROS.
           05  WRK-OUT-SENHAS-DQTM      PIC  X(037) VALUE SPACES.
           05  WRK-OUT-COMANDO-DQTM     PIC  X(068) VALUE SPACES.
           05  WRK-OUT-DADOS-DQTM.
               10  WRK-OUT-FASE-DQTM-X.
                   15  WRK-OUT-FASE-DQTM
                                        PIC  9(001) VALUE ZEROS.
               10  WRK-OUT-TPOOPC-DQTM  PIC  X(001) VALUE SPACES.
               10  WRK-OUT-TMSTAMP-DQTM PIC  X(026) VALUE SPACES.
               10  WRK-OUT-FIMAMOS-DQTM PIC  X(001) VALUE SPACES.
               10  WRK-OUT-PAGINA-DQTM-X.
                   15  WRK-OUT-PAGINA-DQTM
                                        PIC  9(003) VALUE ZEROS.
               10  WRK-OUT-CODAGE-DQTM-X.
                   15  WRK-OUT-CODAGE-DQTM
                                        PIC  9(005) VALUE ZEROS.
               10  WRK-OUT-DESCAGE-DQTM PIC  X(020) VALUE SPACES.
               10  WRK-OUT-TABE-DQTM.
                   15  WRK-OUT-TABELA-DQTM    OCCURS  05  TIMES.
                       20  WRK-OUT-SELEC-ATTR-DQTM PIC  S9(04) COMP.
                       20  WRK-OUT-SELEC-DQTM
                                               PIC  X(001) VALUE SPACES.
                       20  WRK-OUT-CONTR-DQTM-X.
                           25  WRK-OUT-CONTR-DQTM
                                               PIC  9(009) VALUE ZEROS.
                       20  WRK-OUT-NOME-DQTM
                                               PIC  X(030) VALUE SPACES.
                       20  WRK-OUT-DTINIC-DQTM
                                               PIC  X(010) VALUE SPACES.
                       20  WRK-OUT-DTVENCT-DQTM
                                               PIC  X(010) VALUE SPACES.
                       20  WRK-OUT-LIMITE-DQTM-X.
                           25  WRK-OUT-LIMITE-DQTM
                                               PIC  ZZ.ZZZ.ZZZ.ZZ9,99.
                       20  WRK-OUT-SITUAC-DQTM
                                               PIC  X(020) VALUE SPACES.
               10  WRK-OUT-MENSA-DQTM   PIC  X(079) VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA TELA DCOMDQTM  - BRAD0660 ***'.
      *----------------------------------------------------------------*
       01  WRK-660-DCOMDQTM.
           05  WRK-660-LL-AREA-DQTM     PIC  9(004) COMP   VALUE 0094.
           05  WRK-660-LL-MENSAGEM-DQTM PIC  9(004) COMP   VALUE 0740.
           05  WRK-660-SENHAS-DQTM      PIC  9(004) COMP   VALUE 0037.
           05  WRK-660-COMANDO-DQTM     PIC  9(004) COMP   VALUE 0068.
           05  WRK-660-FASE-DQTM        PIC  9(004) COMP   VALUE 0001.
           05  WRK-660-TPOOPC-DQTM      PIC  9(004) COMP   VALUE 0001.
           05  WRK-660-TMSTAMP-DQTM     PIC  9(004) COMP   VALUE 0026.
           05  WRK-660-FIMAMOS-DQTM     PIC  9(004) COMP   VALUE 0001.
           05  WRK-660-PAGINA-DQTM      PIC  9(004) COMP   VALUE 0003.
           05  WRK-660-CODAGE-DQTM      PIC  9(004) COMP   VALUE 0005.
           05  WRK-660-DESCAGE-DQTM     PIC  9(004) COMP   VALUE 0020.
           05  WRK-660-TABELA-DQTM    OCCURS  05  TIMES.
               10  WRK-660-SELEC-DQTM   PIC  9(004) COMP   VALUE 2003.
               10  WRK-660-CONTR-DQTM   PIC  9(004) COMP   VALUE 0009.
               10  WRK-660-NOME-DQTM    PIC  9(004) COMP   VALUE 0030.
               10  WRK-660-DTINIC-DQTM  PIC  9(004) COMP   VALUE 0010.
               10  WRK-660-DTVENCT-DQTM PIC  9(004) COMP   VALUE 0010.
               10  WRK-660-LIMITE-DQTM  PIC  9(004) COMP   VALUE 0017.
               10  WRK-660-SITUAC-DQTM  PIC  9(004) COMP   VALUE 0020.
           05  WRK-660-MENSA-DQTM       PIC  9(004) COMP   VALUE 0079.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(080)         VALUE
           '*** AREA DE PASSAGEM DE CONTROLE PARA DCOM0413 ***'.
      *----------------------------------------------------------------*
       01  COMU-AREA.
           05  COMU-LL            PIC S9(004) COMP    VALUE +165.
           05  COMU-ZZ            PIC S9(004) COMP    VALUE ZEROS.
           05  COMU-TRANCODE.
               10  COMU-TRANSACAO PIC  X(008)         VALUE SPACES.
               10  FILLER         PIC  X(006)         VALUE SPACES.
               10  COMU-PFK       PIC  X(001)         VALUE SPACES.
           05  COMU-SENHAS        PIC  X(037)         VALUE SPACES.
           05  COMU-COMANDO       PIC  X(068)         VALUE SPACES.
           05  COMU-OPCAO         PIC  X(001)         VALUE SPACES.
           05  COMU-TMSTAMP       PIC  X(026)         VALUE SPACES.
           05  COMU-CONTRATO      PIC  9(009)         VALUE ZEROS.
           05  COMU-AGENCIA       PIC  9(005)         VALUE ZEROS.
           05  COMU-CONTA         PIC  9(013)         VALUE ZEROS.
FS2511*    05  COMU-CPF           PIC  9(009)         VALUE ZEROS.
FS2511     05  COMU-CPF           PIC  X(009)         VALUE SPACES.
FS2511*    05  COMU-FILIAL        PIC  9(004)         VALUE ZEROS.
FS2511     05  COMU-FILIAL        PIC  X(04)          VALUE SPACES.
           05  COMU-DIGITO        PIC  9(002)         VALUE ZEROS.
           05  COMU-MENSAG        PIC  X(079)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(080)         VALUE
           '*** AREA DE RECEBIMENTO E RETORNO DO DCOM0413 ***'.
      *----------------------------------------------------------------*
       01  COMU-AREA-413.
           05  COMU-LL-413            PIC S9(004) COMP    VALUE +165.
           05  COMU-ZZ-413            PIC S9(004) COMP    VALUE ZEROS.
           05  COMU-TRANCODE-413.
               10  COMU-TRANSACAO-413 PIC  X(008)         VALUE SPACES.
               10  FILLER             PIC  X(006)         VALUE SPACES.
               10  COMU-PFK-413       PIC  X(001)         VALUE SPACES.
           05  COMU-SENHAS-413        PIC  X(037)         VALUE SPACES.
           05  COMU-COMANDO-413       PIC  X(068)         VALUE SPACES.
           05  COMU-OPCAO-413         PIC  X(001)         VALUE SPACES.
           05  COMU-TMSTAMP-413       PIC  X(026)         VALUE SPACES.
           05  COMU-CODAGE-413        PIC  9(005)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO COM TRANSACAO DCOM0419 **'.
      *----------------------------------------------------------------*
       01  COMU-AREA-419.
           05  COMU-LL-419            PIC S9(004) COMP    VALUE +244.
           05  COMU-ZZ-419            PIC S9(004) COMP    VALUE ZEROS.
           05  COMU-TRANCODE-419.
               10  COMU-TRANSACAO-419 PIC  X(008)         VALUE SPACES.
               10  FILLER             PIC  X(007)         VALUE SPACES.
           05  COMU-SENHAS-419        PIC  X(037)         VALUE SPACES.
           05  COMU-COMANDO-419       PIC  X(068)         VALUE SPACES.
           05  COMU-OPCAO-419         PIC  X(001)         VALUE SPACES.
           05  COMU-TMSTAMP-419       PIC  X(026)         VALUE SPACES.
           05  COMU-CONTRATO-419      PIC  9(009)         VALUE ZEROS.
           05  COMU-CODAGE-419        PIC  9(005)         VALUE ZEROS.
           05  COMU-MENSA-419         PIC  X(079)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO COM O MODULO DCOM6416 **'.
      *----------------------------------------------------------------*

       01  WRK-6416E-ENTRADA.
           03  WRK-6416E-COD-RETORNO      PIC  X(004).
           03  WRK-6416E-MSG-RETORNO      PIC  X(079).
           03  WRK-6416E-RESTART          PIC  9(005).
           03  WRK-6416E-FLAG             PIC  X(001).
           03  WRK-6416E-CFUNC-BDSCO      PIC  9(009).
           03  WRK-6416E-CTERM            PIC  X(008).
           03  WRK-6416E-BANCO            PIC  9(003).
           03  WRK-6416E-DEPDC            PIC  9(005).

       01  WRK-6416S-SAIDA.
           03  WRK-6416S-HEADER.
               05  WRK-6416S-COD-RETORNO  PIC  X(004).
               05  WRK-6416S-MSG-RETORNO  PIC  X(079).
               05  WRK-6416S-RESTART      PIC  9(005).
               05  WRK-6416S-FLAG         PIC  X(001).
           03  WRK-6416S-DADOS.
               05  WRK-6416S-TABELA-SAIDA.
                   07  WRK-6416S-AG-DEPDC            PIC 9(05).
                   07  WRK-6416S-DG-DEPDC            PIC X(01).
                   07  WRK-6416S-NOME-DEPDC          PIC X(40).
                   07  WRK-6416S-END-DEPDC           PIC X(30).
                   07  WRK-6416S-DIRETORIA-REG       PIC 9(05).
                   07  WRK-6416S-DIRETORIA-NOME      PIC X(40).
                   07  WRK-6416S-NOME-BANCO          PIC X(40).
                   07  WRK-6416S-POLO-SERVICO        PIC 9(05).
                   07  WRK-6416S-MUNIC-DEPDC         PIC X(25).
                   07  WRK-6416S-UF                  PIC X(02).
                   07  WRK-6416S-CEP                 PIC 9(05).
                   07  WRK-6416S-CEP-COMPL           PIC 9(03).
                   07  WRK-6416S-INDIC-DEPDC-AT      PIC X(01).
                   07  WRK-6416S-NATUREZA-DEPDC      PIC X(01).
                   07  WRK-6416S-COD-DISTR-CORR      PIC 9(05).
                   07  WRK-6416S-DENCRR-DEPDC-BDSCO  PIC X(10).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO COM O MODULO DCOM5143 **'.
      *----------------------------------------------------------------*

       01  WRK-5143E-ENTRADA.
           03  WRK-5143E-COD-RETORNO      PIC  X(004).
           03  WRK-5143E-MSG-RETORNO      PIC  X(079).
           03  WRK-5143E-RESTART          PIC  9(005).
           03  WRK-5143E-FLAG             PIC  X(001).
           03  WRK-5143E-CFUNC-BDSCO      PIC  9(009).
           03  WRK-5143E-CTERM            PIC  X(008).
           03  WRK-5143E-CCONTR-LIM-DESC  PIC  9(009).

       01  WRK-5143S-SAIDA.
           03  WRK-5143S-HEADER.
               05  WRK-5143S-COD-RETORNO      PIC  X(004).
               05  WRK-5143S-MSG-RETORNO      PIC  X(079).
               05  WRK-5143S-RESTART          PIC  9(005).
               05  WRK-5143S-FLAG             PIC  X(001).
               05  WRK-5143S-COUNT            PIC  9(009).
               05  WRK-5143S-TAMANHO-ERRO     PIC  9(001).
               05  WRK-5143S-CAMPOS-ERRO      PIC  9(001).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO COM O MODULO DCOM4110 **'.
      *----------------------------------------------------------------*
       01  WRK-4110E-ENTRADA.
           03  WRK-4110E-HEADER.
               05  WRK-4110E-COD-RETORNO             PIC  X(04).
               05  WRK-4110E-MSG-RETORNO             PIC  X(79).
               05  WRK-4110E-RESTART                 PIC  9(05).
               05  WRK-4110E-FLAG                    PIC  X(01).
               05  WRK-4110E-START-COUNT             PIC  X(01).
               05  WRK-4110E-CFUNC-BDSCO             PIC  9(09).
               05  WRK-4110E-CTERM                   PIC  X(08).
           03  WRK-4110E-DADOS.
               05  WRK-4110E-CAG-BCRIA               PIC  9(05).
               05  WRK-4110E-FLAG-FILTRO             PIC  X(01).

       01  WRK-4110S-SAIDA.
           03  WRK-4110S-HEADER.
               05  WRK-4110S-COD-RETORNO            PIC  X(04).
               05  WRK-4110S-MSG-RETORNO            PIC  X(79).
               05  WRK-4110S-RESTART                PIC  9(05).
               05  WRK-4110S-FLAG                   PIC  X(01).
               05  WRK-4110S-QTDE-RETORNADA         PIC  9(03).
               05  WRK-4110S-COUNT                  PIC  9(09).
           03  WRK-4110S-DADOS.
               05  WRK-4110S-TABELA-SAIDA    OCCURS 20 TIMES.
                   10  WRK-4110S-CCONTR-LIM-DESC    PIC  9(09).
                   10  WRK-4110S-CVRSAO-CONTR-LIM   PIC  9(03).
FS2511             10  WRK-4110S-CCNPJ-CPF          PIC  X(09).
FS2511             10  WRK-4110S-CFLIAL-CNPJ        PIC  X(04).
                   10  WRK-4110S-CCTRL-CNPJ-CPF     PIC  9(02).
                   10  WRK-4110S-NM-CLIE            PIC  X(25).
                   10  WRK-4110S-DINIC-VGCIA-CONTR  PIC  X(10).
                   10  WRK-4110S-DVCTO-CONTR-LIM    PIC  X(10).
                   10  WRK-4110S-VCONTR-LIM         PIC  9(15)V99.
                   10  WRK-4110S-IRSUMO-SIT-DESC    PIC  X(15).
           03  WRK-4110S-CONSISTENCIA   REDEFINES   WRK-4110S-DADOS.
               05  WRK-4110S-TAMANHO-ERRO           PIC  9(01).
               05  WRK-4110S-TABELA-ERRO   OCCURS   1    TIMES.
                   10  WRK-4110S-CAMPOS-ERRO        PIC  9(01).

WIP001*----------------------------------------------------------------*
WIP001 01  FILLER                      PIC  X(050)         VALUE
WIP001     '*** AREA PARA ACESSO AO DCOM5124 ***'.
WIP001*----------------------------------------------------------------*
WIP001
WIP001 01  WRK-DCOM5124-ENTRADA.
WIP001     05  WRK-5124E-COD-RETORNO    PIC  X(004)        VALUE SPACES.
WIP001     05  WRK-5124E-MSG-RETORNO    PIC  X(079)        VALUE SPACES.
WIP001     05  WRK-5124E-TRANSACAO      PIC  X(009)        VALUE SPACES.
WIP001     05  WRK-5124E-DADOS.
WIP001         10  WRK-5124E-CCONTR-LIM PIC  9(009)        VALUE ZEROS.
WIP001         10  WRK-5124E-CVRSAO-CON PIC  9(003)        VALUE ZEROS.
WIP001
WIP001 01  WRK-DCOM5124-SAIDA.
WIP001     05  WRK-5124S-COD-RETORNO    PIC  9(004)        VALUE ZEROS.
WIP001     05  WRK-5124S-MSG-RETORNO    PIC  X(079)        VALUE SPACES.
WIP001
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA MENSAGENS ***'.
      *----------------------------------------------------------------*

       COPY 'I#DCOMSY'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA BRAD7100 ***'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** DCOM0417 - FIM DA AREA DE WORKING ***'.
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

           MOVE 'N'                    TO  WRK-CANCELADOS
           MOVE SPACES                 TO  WRK-OUT-DADOS-DQTM
                                           WRK-OUT-SENHAS-DQTM
                                           WRK-OUT-COMANDO-DQTM
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

           MOVE 'DCOMDQTM'             TO  WRK-TELA.

           IF  WRK-TRANSACAO    NOT EQUAL  'DCOM0417'
               PERFORM 1100-RECEBER-CONTROLE
           ELSE
               PERFORM 1200-PROCESSAR-DCOMDQTM
           END-IF.

           IF  WRK-TELA             EQUAL  'DCOMDQTM'
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

           IF  WRK-TRANSACAO  EQUAL  'DCOM0413'
               MOVE WRK-MENSAGEM       TO  COMU-AREA-413
               MOVE COMU-OPCAO-413     TO  WRK-INP-TPOOPC-DQTM
                                           WRK-OUT-TPOOPC-DQTM
               MOVE COMU-TMSTAMP-413   TO  WRK-INP-TMSTAMP-DQTM
                                           WRK-OUT-TMSTAMP-DQTM
               MOVE COMU-CODAGE-413    TO  WRK-INP-CODAGE-DQTM
                                           WRK-OUT-CODAGE-DQTM
           ELSE
             IF  WRK-TRANSACAO  EQUAL  'DCOM0419'
                 MOVE WRK-MENSAGEM     TO  COMU-AREA-419
                 MOVE COMU-OPCAO-419   TO  WRK-INP-TPOOPC-DQTM
                                           WRK-OUT-TPOOPC-DQTM
                 MOVE COMU-TMSTAMP-419 TO  WRK-INP-TMSTAMP-DQTM
                                           WRK-OUT-TMSTAMP-DQTM
                 MOVE COMU-CODAGE-419  TO  WRK-INP-CODAGE-DQTM
                                           WRK-OUT-CODAGE-DQTM
             END-IF
           END-IF.

           PERFORM 1110-ACESSAR-DCOM6416

           MOVE ZEROS                  TO  WRK-4110E-RESTART

           PERFORM 1120-ACESSAR-DCOM4110

           MOVE 1                      TO  WRK-INP-PAGINA-DQTM
                                           WRK-OUT-PAGINA-DQTM
                                           WRK-INP-FASE-DQTM
                                           WRK-OUT-FASE-DQTM.

           MOVE 'SELECIONE UM REGISTRO COM "D" OU VARIOS COM "C" E TECLE
      -         ' <ENTER>'             TO  WRK-OUT-MENSA-DQTM.

           IF  WRK-TRANSACAO  EQUAL  'DCOM0419'
               IF COMU-MENSA-419      NOT EQUAL SPACES
                  MOVE COMU-MENSA-419  TO  WRK-OUT-MENSA-DQTM
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA ACESSA O MODULO DCOM6416                          *
      *----------------------------------------------------------------*
       1110-ACESSAR-DCOM6416           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-COD-USER           TO  WRK-6416E-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO  WRK-6416E-CTERM
           MOVE 237                    TO  WRK-6416E-BANCO
           MOVE WRK-INP-CODAGE-DQTM    TO  WRK-6416E-DEPDC
           MOVE 'DCOM6416'             TO  WRK-MODULO

           CALL WRK-MODULO    USING    WRK-6416E-ENTRADA
                                       WRK-6416S-SAIDA
                                       ERRO-AREA
                                       WRK-SQLCA.

           EVALUATE WRK-6416S-COD-RETORNO
               WHEN '0000'
                    MOVE WRK-6416S-NOME-DEPDC   TO WRK-INP-DESCAGE-DQTM
                                                   WRK-OUT-DESCAGE-DQTM

               WHEN '0003'
                    MOVE  ALL '*'               TO WRK-INP-DESCAGE-DQTM
                                                   WRK-OUT-DESCAGE-DQTM

               WHEN '0099'
                    MOVE  'DB2'     TO     ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                    MOVE  WRK-6416S-MSG-RETORNO
                                        TO WRK-OUT-MENSA-DQTM
           END-EVALUATE.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA ACESSA O MODULO DCOM4110                          *
      *----------------------------------------------------------------*
       1120-ACESSAR-DCOM4110           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-COD-USER           TO  WRK-4110E-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO  WRK-4110E-CTERM
           MOVE 'M'                    TO  WRK-4110E-FLAG-FILTRO
           MOVE 'S'                    TO  WRK-4110E-START-COUNT
           MOVE SPACES                 TO  WRK-4110E-FLAG
           MOVE WRK-INP-CODAGE-DQTM    TO  WRK-4110E-CAG-BCRIA
           MOVE 'DCOM4110'             TO  WRK-MODULO

           CALL WRK-MODULO    USING    WRK-4110E-ENTRADA
                                       WRK-4110S-SAIDA
                                       ERRO-AREA
                                       WRK-SQLCA.

           EVALUATE WRK-4110S-COD-RETORNO
               WHEN '0000'
                    IF (WRK-4110S-CCONTR-LIM-DESC(6)  NOT NUMERIC) OR
                       (WRK-4110S-CCONTR-LIM-DESC(6)  EQUAL ZEROS)
                       MOVE '*'     TO  WRK-OUT-FIMAMOS-DQTM
                       MOVE 'FINAL DE AMOSTRAGEM'
                                    TO  WRK-OUT-MENSA-DQTM
                    ELSE
                       MOVE SPACES  TO  WRK-OUT-FIMAMOS-DQTM
                       MOVE 'AMOSTRAGEM CONTINUA'
                                    TO  WRK-OUT-MENSA-DQTM
                    END-IF
                    PERFORM 1121-MONTAR-TELA VARYING WRK-IND FROM
                            1 BY 1 UNTIL WRK-IND GREATER 5

               WHEN '0003'
                    INITIALIZE                       COMU-AREA
                    IF  WRK-CANCELADOS        EQUAL 'S'
                        MOVE  SG-MENSAGEM(0203)       TO COMU-MENSAG
                        PERFORM 1220-RETORNAR-CHAMADOR
                    ELSE
                        MOVE  SG-MENSAGEM(0198)       TO COMU-MENSAG
                        PERFORM 1220-RETORNAR-CHAMADOR
                    END-IF

               WHEN '0099'
                    MOVE  'DB2'     TO     ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                    INITIALIZE                       COMU-AREA
                    MOVE  WRK-4110S-MSG-RETORNO   TO COMU-MENSAG
                    PERFORM 1220-RETORNAR-CHAMADOR

           END-EVALUATE.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA CARREGA OS CAMPOS DA TELA                         *
      *----------------------------------------------------------------*
       1121-MONTAR-TELA                SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES            TO WRK-OUT-SELEC-DQTM(WRK-IND)

           IF (WRK-4110S-CCONTR-LIM-DESC(WRK-IND) IS NUMERIC) AND
              (WRK-4110S-CCONTR-LIM-DESC(WRK-IND) GREATER ZEROS)
              MOVE SPACES         TO WRK-OUT-SELEC-DQTM(WRK-IND)
              MOVE WRK-4110S-CCONTR-LIM-DESC(WRK-IND)
                                  TO WRK-OUT-CONTR-DQTM(WRK-IND)
              MOVE WRK-4110S-NM-CLIE(WRK-IND)
                                  TO WRK-OUT-NOME-DQTM(WRK-IND)
              MOVE WRK-4110S-DINIC-VGCIA-CONTR(WRK-IND)
                                  TO WRK-OUT-DTINIC-DQTM(WRK-IND)
              INSPECT WRK-OUT-DTINIC-DQTM(WRK-IND)  REPLACING
                                                    ALL '.' BY '/'
              MOVE WRK-4110S-DVCTO-CONTR-LIM(WRK-IND)
                                  TO WRK-OUT-DTVENCT-DQTM(WRK-IND)
              INSPECT WRK-OUT-DTVENCT-DQTM(WRK-IND) REPLACING
                                                    ALL '.' BY '/'
              MOVE WRK-4110S-VCONTR-LIM(WRK-IND)
                                  TO WRK-OUT-LIMITE-DQTM(WRK-IND)
              MOVE WRK-4110S-IRSUMO-SIT-DESC(WRK-IND)
                                  TO WRK-OUT-SITUAC-DQTM(WRK-IND)
           ELSE
              MOVE WRK-241        TO WRK-OUT-SELEC-ATTR-DQTM(WRK-IND)
           END-IF.

      *----------------------------------------------------------------*
       1121-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA PROCESSA A MENSAGEM VINDA DA TELA DCOMDQTM        *
      *----------------------------------------------------------------*
       1200-PROCESSAR-DCOMDQTM         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM             TO WRK-INPUT-DQTM.
           PERFORM 1210-DEVOLVER-TELA-FIXA

           EVALUATE TRUE
               WHEN WRK-INP-PFK-DQTM     EQUAL 'H' OR
                   (WRK-INP-PFK-DQTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DQTM EQUAL 'PFK1')
                    PERFORM 1211-DEVOLVER-TELA-VARIA
                    MOVE WRK-INP-MENSA-DQTM TO WRK-OUT-MENSA-DQTM

               WHEN WRK-INP-PFK-DQTM     EQUAL '3' OR
                   (WRK-INP-PFK-DQTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DQTM EQUAL 'PFK3')
                    INITIALIZE           COMU-AREA
                    PERFORM 1220-RETORNAR-CHAMADOR

               WHEN WRK-INP-PFK-DQTM     EQUAL 'X' OR
                   (WRK-INP-PFK-DQTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DQTM EQUAL 'PFK5')
                    PERFORM 1230-RETORNAR-MENU-ROTINAS

               WHEN WRK-INP-PFK-DQTM     EQUAL 'Z' OR
                   (WRK-INP-PFK-DQTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DQTM EQUAL 'PFK10')
                    PERFORM 1240-RETORNAR-MENU-DCOM

               WHEN WRK-INP-FASE-DQTM    EQUAL 1
                    PERFORM 1250-TRATAR-FASE-1

               WHEN WRK-INP-FASE-DQTM    EQUAL 2
                    PERFORM 1260-TRATAR-FASE-2

               WHEN OTHER
                    PERFORM 1211-DEVOLVER-TELA-VARIA
                    MOVE SG-MENSAGEM(38)  TO  WRK-OUT-MENSA-DQTM
           END-EVALUATE.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA DEVOLVE OS DADOS FIXOS DA TELA SEM ALTERACAO      *
      *----------------------------------------------------------------*
       1210-DEVOLVER-TELA-FIXA         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-FIMAMOS-DQTM   TO WRK-OUT-FIMAMOS-DQTM.
           MOVE WRK-INP-FASE-DQTM      TO WRK-OUT-FASE-DQTM.
           MOVE WRK-INP-TPOOPC-DQTM    TO WRK-OUT-TPOOPC-DQTM.
           MOVE WRK-INP-TMSTAMP-DQTM   TO WRK-OUT-TMSTAMP-DQTM.
           MOVE WRK-INP-PAGINA-DQTM-X  TO WRK-OUT-PAGINA-DQTM-X.
           MOVE WRK-INP-CODAGE-DQTM-X  TO WRK-OUT-CODAGE-DQTM-X.
           MOVE WRK-INP-DESCAGE-DQTM   TO WRK-OUT-DESCAGE-DQTM.

      *----------------------------------------------------------------*
       1210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA DEVOLVE OS DADOS VARIAVEIS DA TELA S/ ALTERACAO   *
      *----------------------------------------------------------------*
       1211-DEVOLVER-TELA-VARIA        SECTION.
      *----------------------------------------------------------------*

           PERFORM VARYING WRK-IND FROM 1 BY 1 UNTIL WRK-IND GREATER 5
              MOVE SPACES             TO WRK-OUT-SELEC-DQTM(WRK-IND)
              IF  (WRK-INP-CONTR-DQTM(WRK-IND)  IS NUMERIC) AND
                  (WRK-INP-CONTR-DQTM(WRK-IND)  GREATER ZEROS)
                   MOVE WRK-INP-SELEC-DQTM(WRK-IND)
                                      TO WRK-OUT-SELEC-DQTM(WRK-IND)
                   MOVE WRK-INP-CONTR-DQTM-X(WRK-IND)
                                      TO WRK-OUT-CONTR-DQTM-X(WRK-IND)
                   MOVE WRK-INP-NOME-DQTM(WRK-IND)
                                      TO WRK-OUT-NOME-DQTM(WRK-IND)
                   MOVE WRK-INP-DTINIC-DQTM(WRK-IND)
                                      TO WRK-OUT-DTINIC-DQTM(WRK-IND)
                   MOVE WRK-INP-DTVENCT-DQTM(WRK-IND)
                                      TO WRK-OUT-DTVENCT-DQTM(WRK-IND)
                   MOVE WRK-INP-LIMITE-DQTM(WRK-IND)
                                      TO WRK-OUT-LIMITE-DQTM-X(WRK-IND)
                   MOVE WRK-INP-SITUAC-DQTM(WRK-IND)
                                      TO WRK-OUT-SITUAC-DQTM(WRK-IND)
              ELSE
                   MOVE WRK-241   TO WRK-OUT-SELEC-ATTR-DQTM(WRK-IND)
              END-IF
           END-PERFORM.

      *----------------------------------------------------------------*
       1211-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA RETORNA AO PROGRAMA CHAMADOR                      *
      *----------------------------------------------------------------*
       1220-RETORNAR-CHAMADOR          SECTION.
      *----------------------------------------------------------------*

           MOVE LENGTH COMU-AREA     TO COMU-LL
           MOVE WRK-CHNG             TO WRK-FUNCAO.
           MOVE 'DCOM0413'           TO WRK-TELA.
           MOVE 'DCOM0417'           TO COMU-TRANSACAO.
           MOVE WRK-INP-TPOOPC-DQTM  TO COMU-OPCAO.
           MOVE WRK-INP-TMSTAMP-DQTM TO COMU-TMSTAMP.
           MOVE COMU-AREA            TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRATAMENTO DE RETORNO AO MENU ROTINAS    *
      *----------------------------------------------------------------*
       1230-RETORNAR-MENU-ROTINAS      SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-CHNG            TO WRK-FUNCAO.
           MOVE 'DCOM0417'          TO WRK-TELA
                                       COMU-TRANSACAO-413.
           MOVE '5'                 TO COMU-PFK-413.
           MOVE COMU-AREA-413       TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRATAMENTO DE RETORNO AO MENU DCOM       *
      *----------------------------------------------------------------*
       1240-RETORNAR-MENU-DCOM         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-CHNG           TO WRK-FUNCAO.
           MOVE 'DCOM0417'         TO WRK-TELA
                                      COMU-TRANSACAO-413.
           MOVE 'A'                TO COMU-PFK-413.
           MOVE COMU-AREA-413      TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1240-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1250-TRATAR-FASE-1              SECTION.
      *----------------------------------------------------------------*

           EVALUATE TRUE
               WHEN  WRK-INP-PFK-DQTM     EQUAL '7' OR
                    (WRK-INP-PFK-DQTM    EQUAL '.' AND
                     WRK-INP-COMANDO-DQTM EQUAL 'PFK7')
                     PERFORM 1251-TRATAR-VOLTA-PAG

               WHEN  WRK-INP-PFK-DQTM     EQUAL '8' OR
                    (WRK-INP-PFK-DQTM    EQUAL '.' AND
                     WRK-INP-COMANDO-DQTM EQUAL 'PFK8')
                     PERFORM 1252-TRATAR-AVANCA-PAG

               WHEN  WRK-INP-PFK-DQTM     EQUAL '.' AND
                     WRK-INP-COMANDO-DQTM EQUAL  SPACES OR LOW-VALUES
                     PERFORM 1211-DEVOLVER-TELA-VARIA
                     PERFORM 1253-TRATAR-ENTER

               WHEN OTHER
                     PERFORM 1211-DEVOLVER-TELA-VARIA
                     MOVE SG-MENSAGEM(38)  TO  WRK-OUT-MENSA-DQTM
           END-EVALUATE.

      *----------------------------------------------------------------*
       1250-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRAMAMENTO DO COMANDO PFK7 - VOLTA PAG.  *
      *----------------------------------------------------------------*
       1251-TRATAR-VOLTA-PAG           SECTION.
      *----------------------------------------------------------------*

           IF WRK-INP-PAGINA-DQTM  EQUAL  1
              PERFORM 1211-DEVOLVER-TELA-VARIA
              MOVE 'NAO EXISTE PAGINA PARA VOLTAR'
                                       TO WRK-OUT-MENSA-DQTM
              GO TO 1251-99-FIM
           END-IF.

           IF  WRK-INP-MENSA-DQTM      NOT EQUAL WRK-MENSA-PF7

               MOVE ZEROS            TO WRK-QTD-OPCAO-D
                                        WRK-QTD-OPCAO-L
                                        WRK-QTD-OPCAO-C

               PERFORM VARYING WRK-IND FROM 1 BY 1
                       UNTIL   WRK-IND GREATER 5

                   IF  WRK-INP-SELEC-DQTM(WRK-IND) EQUAL 'D'
                       ADD  1          TO WRK-QTD-OPCAO-D
                   ELSE
                       IF  WRK-INP-SELEC-DQTM(WRK-IND) EQUAL 'L'
                           ADD  1          TO WRK-QTD-OPCAO-L
                       ELSE
                           IF  WRK-INP-SELEC-DQTM(WRK-IND) EQUAL 'C'
                               ADD  1          TO WRK-QTD-OPCAO-C
                           END-IF
                       END-IF
                   END-IF

               END-PERFORM

               IF  WRK-QTD-OPCAO-D      GREATER ZEROS OR
                   WRK-QTD-OPCAO-L      GREATER ZEROS OR
                   WRK-QTD-OPCAO-C      GREATER ZEROS
                   PERFORM 1211-DEVOLVER-TELA-VARIA
                   MOVE WRK-MENSA-PF7           TO  WRK-OUT-MENSA-DQTM
                   GO  TO 1251-99-FIM
               END-IF

           END-IF.

           MOVE  SPACES                TO WRK-OUT-TABE-DQTM

           COMPUTE WRK-4110E-RESTART = (WRK-INP-PAGINA-DQTM - 2) * 5

           PERFORM 1120-ACESSAR-DCOM4110

           COMPUTE WRK-OUT-PAGINA-DQTM = WRK-INP-PAGINA-DQTM - 1.

      *----------------------------------------------------------------*
       1251-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRAMAMENTO DO COMANDO PFK8 - AVANCA PAG. *
      *----------------------------------------------------------------*
       1252-TRATAR-AVANCA-PAG          SECTION.
      *----------------------------------------------------------------*

           IF WRK-INP-FIMAMOS-DQTM  EQUAL  '*'
              PERFORM 1211-DEVOLVER-TELA-VARIA
              MOVE 'NAO EXISTE PAGINA PARA AVANCAR'
                                       TO WRK-OUT-MENSA-DQTM
              GO TO 1252-99-FIM
           END-IF.

           IF  WRK-INP-MENSA-DQTM      NOT EQUAL WRK-MENSA-PF8

               MOVE ZEROS            TO WRK-QTD-OPCAO-D
                                        WRK-QTD-OPCAO-L
                                        WRK-QTD-OPCAO-C

               PERFORM VARYING WRK-IND FROM 1 BY 1
                       UNTIL   WRK-IND GREATER 5

                   IF  WRK-INP-SELEC-DQTM(WRK-IND) EQUAL 'D'
                       ADD  1          TO WRK-QTD-OPCAO-D
                   ELSE
                       IF  WRK-INP-SELEC-DQTM(WRK-IND) EQUAL 'L'
                           ADD  1          TO WRK-QTD-OPCAO-L
                       ELSE
                           IF  WRK-INP-SELEC-DQTM(WRK-IND) EQUAL 'C'
                               ADD  1          TO WRK-QTD-OPCAO-C
                           END-IF
                       END-IF
                   END-IF

               END-PERFORM

               IF  WRK-QTD-OPCAO-D      GREATER ZEROS OR
                   WRK-QTD-OPCAO-L      GREATER ZEROS OR
                   WRK-QTD-OPCAO-C      GREATER ZEROS
                   PERFORM 1211-DEVOLVER-TELA-VARIA
                   MOVE WRK-MENSA-PF8           TO  WRK-OUT-MENSA-DQTM
                   GO  TO 1252-99-FIM
               END-IF

           END-IF.
           MOVE  SPACES                TO WRK-OUT-TABE-DQTM

           COMPUTE WRK-4110E-RESTART = (WRK-INP-PAGINA-DQTM) * 5

           PERFORM 1120-ACESSAR-DCOM4110

           COMPUTE WRK-OUT-PAGINA-DQTM = WRK-INP-PAGINA-DQTM + 1.

      *----------------------------------------------------------------*
       1252-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRAMAMENTO DO COMANDO ENTER              *
      *----------------------------------------------------------------*
       1253-TRATAR-ENTER               SECTION.
      *----------------------------------------------------------------*

           MOVE  SPACES                TO WRK-INCONSISTENCIA

           PERFORM 1254-CONSISTIR-FISICA

           IF WRK-INCONSISTENCIA  EQUAL  '*'
              GO TO 1253-99-FIM
           END-IF.

WIP001     PERFORM
WIP001       VARYING WRK-IND           FROM 1 BY 1
WIP001         UNTIL WRK-IND           GREATER 5
WIP001           IF  WRK-INP-SELEC-DQTM(WRK-IND)
WIP001                                 NOT EQUAL SPACES
WIP001               PERFORM 1255-VALIDAR-OPTIN
WIP001           END-IF
WIP001     END-PERFORM.
WIP001
WIP001     IF  WRK-INCONSISTENCIA      EQUAL  '*'
WIP001         GO TO 1253-99-FIM
WIP001     END-IF.
WIP001
           PERFORM VARYING WRK-IND FROM 1 BY 1 UNTIL WRK-IND GREATER 5
                IF WRK-INP-SELEC-DQTM(WRK-IND) EQUAL 'D'
                   PERFORM 1262-CHAMAR-TRAN-DCOM0419
                   GO TO 1253-99-FIM
                END-IF
           END-PERFORM.

           PERFORM VARYING WRK-IND FROM 1 BY 1 UNTIL WRK-IND > 5
                   MOVE WRK-225    TO WRK-OUT-SELEC-ATTR-DQTM(WRK-IND)
           END-PERFORM.

           MOVE 2                      TO WRK-OUT-FASE-DQTM.
           MOVE 'TECLE <PF6> PARA EFETIVAR O CANCELAMENTO OU <ENTER> PAR
      -         'A ALTERAR'            TO WRK-OUT-MENSA-DQTM.

      *----------------------------------------------------------------*
       1253-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRAMAMENTO DA CONSISTENCIA FISICA        *
      *----------------------------------------------------------------*
       1254-CONSISTIR-FISICA           SECTION.
      *----------------------------------------------------------------*

           PERFORM VARYING WRK-IND FROM 1 BY 1 UNTIL WRK-IND GREATER 5
             IF  WRK-INP-SELEC-DQTM(WRK-IND) NOT EQUAL 'C' AND 'D' AND
                                                 SPACES AND LOW-VALUES
                 MOVE '*'        TO WRK-INCONSISTENCIA
                 MOVE WRK-49353  TO WRK-OUT-SELEC-ATTR-DQTM(WRK-IND)
                 MOVE 'OPCAO INVALIDA - SELECIONAR APENAS: C/D'
                                 TO  WRK-OUT-MENSA-DQTM
             END-IF
           END-PERFORM.

           IF WRK-INCONSISTENCIA  EQUAL  '*'
              GO TO 1254-99-FIM
           END-IF.

           MOVE    ZEROS         TO WRK-QTD-OPCAO-D
                                    WRK-QTD-OPCAO-C

           PERFORM VARYING WRK-IND FROM 1 BY 1 UNTIL WRK-IND GREATER 5
             IF  WRK-INP-SELEC-DQTM(WRK-IND) EQUAL 'D'
                 ADD  1          TO WRK-QTD-OPCAO-D
             ELSE
                 IF  WRK-INP-SELEC-DQTM(WRK-IND) EQUAL 'C'
                     ADD  1          TO WRK-QTD-OPCAO-C
                 END-IF
             END-IF
           END-PERFORM.

           IF WRK-QTD-OPCAO-D  GREATER  1
              MOVE '*'             TO WRK-INCONSISTENCIA
              MOVE 'SELECIONAR APENAS UMA OCORRENCIA PARA OPCAO "D"'
                                   TO WRK-OUT-MENSA-DQTM
              PERFORM VARYING WRK-IND FROM 1 BY 1
                                               UNTIL WRK-IND GREATER 5
                IF WRK-INP-SELEC-DQTM(WRK-IND) EQUAL 'D'
                   MOVE WRK-49353  TO WRK-OUT-SELEC-ATTR-DQTM(WRK-IND)
                END-IF
              END-PERFORM
           END-IF.

           IF WRK-INCONSISTENCIA  EQUAL  '*'
              GO TO 1254-99-FIM
           END-IF.

           IF WRK-QTD-OPCAO-D  GREATER ZEROS  AND
              WRK-QTD-OPCAO-C  GREATER ZEROS
              MOVE '*'             TO WRK-INCONSISTENCIA
              MOVE 'SELECIONAR APENAS UMA OPCAO: C/D'
                                   TO WRK-OUT-MENSA-DQTM
              PERFORM VARYING WRK-IND FROM 1 BY 1
                                               UNTIL WRK-IND GREATER 5
                IF WRK-INP-SELEC-DQTM(WRK-IND) EQUAL 'D'  OR  'C'
                   MOVE WRK-49353  TO WRK-OUT-SELEC-ATTR-DQTM(WRK-IND)
                END-IF
              END-PERFORM
           END-IF.

           IF WRK-INCONSISTENCIA  EQUAL  '*'
              GO TO 1254-99-FIM
           END-IF.

           IF WRK-QTD-OPCAO-C    EQUAL  ZEROS AND
              WRK-QTD-OPCAO-D    EQUAL  ZEROS
              MOVE '*'        TO WRK-INCONSISTENCIA
              MOVE WRK-49345  TO WRK-OUT-SELEC-ATTR-DQTM(1)
              MOVE 'NENHUMA OPCAO SELECIONADA'
                              TO WRK-OUT-MENSA-DQTM
           END-IF.

      *----------------------------------------------------------------*
       1254-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

WIP001*----------------------------------------------------------------*
WIP001 1255-VALIDAR-OPTIN              SECTION.
WIP001*----------------------------------------------------------------*
WIP001*
WIP001     INITIALIZE WRK-DCOM5124-ENTRADA
WIP001                WRK-DCOM5124-SAIDA
WIP001                ERRO-AREA
WIP001                WRK-SQLCA.
WIP001*
WIP001     MOVE WRK-INP-CONTR-DQTM(WRK-IND)
WIP001                                 TO WRK-5124E-CCONTR-LIM.
WIP001     MOVE ZEROS                  TO WRK-5124E-CVRSAO-CON.
WIP001     MOVE 'DCOM5124'             TO WRK-MODULO
WIP001*
WIP001     CALL WRK-MODULO             USING WRK-DCOM5124-ENTRADA
WIP001                                       WRK-DCOM5124-SAIDA
WIP001                                       ERRO-AREA
WIP001                                       WRK-SQLCA.
WIP001*
WIP001     EVALUATE WRK-5124S-COD-RETORNO
WIP001       WHEN 0000
WIP001         CONTINUE
WIP001*
WIP001       WHEN 0099
WIP001         MOVE 'DB2'              TO ERR-TIPO-ACESSO
WIP001         PERFORM 9999-PROCESSAR-ROTINA-ERRO
WIP001*
WIP001       WHEN OTHER
WIP001         MOVE WRK-5124S-MSG-RETORNO
WIP001                                 TO WRK-OUT-MENSA-DQTM
WIP001         MOVE WRK-49353          TO
WIP001                                 WRK-OUT-SELEC-ATTR-DQTM(WRK-IND)
WIP001         MOVE '*'                TO WRK-INCONSISTENCIA
WIP001     END-EVALUATE.
WIP001*
WIP001*----------------------------------------------------------------*
WIP001 1255-99-FIM.                    EXIT.
WIP001*----------------------------------------------------------------*
WIP001
      *----------------------------------------------------------------*
       1260-TRATAR-FASE-2              SECTION.
      *----------------------------------------------------------------*

           EVALUATE TRUE

               WHEN  WRK-INP-PFK-DQTM     EQUAL '6' OR
                    (WRK-INP-PFK-DQTM    EQUAL '.' AND
                     WRK-INP-COMANDO-DQTM EQUAL 'PFK06')
                     PERFORM 1261-TRATAR-CONFIRMA

               WHEN  WRK-INP-PFK-DQTM     EQUAL '.' AND
                     WRK-INP-COMANDO-DQTM EQUAL  SPACES OR LOW-VALUES
                     PERFORM VARYING WRK-IND FROM 1 BY 1
                                                      UNTIL WRK-IND > 5
                           MOVE WRK-49345
                                     TO WRK-OUT-SELEC-ATTR-DQTM(WRK-IND)
                     END-PERFORM
                     PERFORM 1211-DEVOLVER-TELA-VARIA
           MOVE 'SELECIONE UM REGISTRO COM "D" OU VARIOS COM "C" E TECLE
      -         ' <ENTER>'             TO  WRK-OUT-MENSA-DQTM
                     MOVE '1'        TO WRK-OUT-FASE-DQTM

               WHEN  OTHER
                     PERFORM 1211-DEVOLVER-TELA-VARIA
                     PERFORM VARYING WRK-IND FROM 1 BY 1
                                                      UNTIL WRK-IND > 5
                           MOVE WRK-225
                                     TO WRK-OUT-SELEC-ATTR-DQTM(WRK-IND)
                     END-PERFORM
                     MOVE SG-MENSAGEM(38)
                                     TO  WRK-OUT-MENSA-DQTM
           END-EVALUATE.

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRAMAMENTO DO COMANDO CONFIRMA(PFK6)     *
      *----------------------------------------------------------------*
       1261-TRATAR-CONFIRMA            SECTION.
      *----------------------------------------------------------------*

           PERFORM VARYING WRK-IND FROM 1 BY 1 UNTIL WRK-IND GREATER 5
                IF WRK-INP-SELEC-DQTM(WRK-IND) EQUAL 'C'
                   MOVE SPACES            TO WRK-INCONSISTENCIA
                   PERFORM 2110-ACESSAR-DCOM4128
                   IF  WRK-INCONSISTENCIA EQUAL SPACES
                       PERFORM 1263-CHAMAR-MOD-DCOM5143
                   END-IF
                END-IF
           END-PERFORM.

           IF  WRK-5143S-COD-RETORNO  EQUAL '0000'
               MOVE 'S'                  TO WRK-CANCELADOS
               MOVE ZEROS                TO WRK-4110E-RESTART
               PERFORM 1120-ACESSAR-DCOM4110
               MOVE 1                    TO WRK-INP-PAGINA-DQTM
                                            WRK-OUT-PAGINA-DQTM
                                            WRK-INP-FASE-DQTM
                                            WRK-OUT-FASE-DQTM
               MOVE 'CANCELAMENTO(S) EFETUADO COM SUCESSO'
                                         TO WRK-OUT-MENSA-DQTM
               PERFORM VARYING WRK-IND FROM 1 BY 1
                               UNTIL WRK-IND > 5
                      MOVE WRK-49345
                                     TO WRK-OUT-SELEC-ATTR-DQTM(WRK-IND)
               END-PERFORM
           END-IF.

      *----------------------------------------------------------------*
       1261-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2110-ACESSAR-DCOM4128           SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO WRK-4128E-RESTART
           MOVE SPACES                 TO WRK-4128E-FLAG
           MOVE WRK-COD-USER           TO WRK-4128E-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO WRK-4128E-CTERM
           MOVE WRK-INP-CONTR-DQTM(WRK-IND)
                                       TO WRK-4128E-CCONTR-LIM-DESC
           MOVE 'M'                    TO WRK-4128E-FLAG-FILTRO
           MOVE 'DCOM4128'             TO WRK-MODULO

           CALL WRK-MODULO    USING    WRK-4128E-ENTRADA
                                       WRK-4128S-SAIDA
                                       ERRO-AREA
                                       WRK-SQLCA.

           EVALUATE WRK-4128S-COD-RETORNO
               WHEN  '0000'
                   CONTINUE

               WHEN '0099'
                   MOVE 'DB2'          TO ERR-TIPO-ACESSO
                   MOVE '0010'         TO ERR-LOCAL
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                    MOVE '*'       TO WRK-INCONSISTENCIA
                    PERFORM 1211-DEVOLVER-TELA-VARIA
                    PERFORM VARYING WRK-IND FROM 1 BY 1
                                                     UNTIL WRK-IND > 5
                          MOVE WRK-225
                                   TO WRK-OUT-SELEC-ATTR-DQTM(WRK-IND)
                    END-PERFORM
                    MOVE  WRK-4128S-MSG-RETORNO
                                    TO WRK-OUT-MENSA-DQTM
           END-EVALUATE.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA PASSA O CONTROLE PARA TRAN. DCOM0419              *
      *----------------------------------------------------------------*
       1262-CHAMAR-TRAN-DCOM0419       SECTION.
      *----------------------------------------------------------------*

           MOVE LENGTH COMU-AREA-419         TO COMU-LL-419
           MOVE WRK-CHNG                     TO WRK-FUNCAO.
           MOVE 'DCOM0419'                   TO WRK-TELA.
           MOVE 'DCOM0417'                   TO COMU-TRANSACAO-419
           MOVE WRK-INP-SENHAS-DQTM          TO COMU-SENHAS-419
           MOVE WRK-INP-CONTR-DQTM(WRK-IND)  TO COMU-CONTRATO-419
           MOVE WRK-INP-CODAGE-DQTM          TO COMU-CODAGE-419
           MOVE COMU-AREA-419                TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1262-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA PASSA O CONTROLE PARA MODULO DCOM5143             *
      *----------------------------------------------------------------*
       1263-CHAMAR-MOD-DCOM5143        SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO WRK-5143E-COD-RETORNO
           MOVE SPACES                 TO WRK-5143E-MSG-RETORNO
           MOVE ZEROS                  TO WRK-5143E-RESTART
           MOVE SPACES                 TO WRK-5143E-FLAG
           MOVE WRK-COD-USER           TO WRK-5143E-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO WRK-5143E-CTERM
           MOVE WRK-INP-CONTR-DQTM(WRK-IND)
                                       TO WRK-5143E-CCONTR-LIM-DESC
           MOVE 'DCOM5143'             TO WRK-MODULO

           CALL WRK-MODULO    USING    WRK-5143E-ENTRADA
                                       WRK-5143S-SAIDA
                                       ERRO-AREA
                                       WRK-SQLCA.

           EVALUATE WRK-5143S-COD-RETORNO
               WHEN '0000'
                    CONTINUE

               WHEN '0099'
                    MOVE  'DB2'     TO     ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                    PERFORM 1211-DEVOLVER-TELA-VARIA
                    PERFORM VARYING WRK-IND FROM 1 BY 1
                                                     UNTIL WRK-IND > 5
                          MOVE WRK-225
                                   TO WRK-OUT-SELEC-ATTR-DQTM(WRK-IND)
                    END-PERFORM
                    MOVE  WRK-5143S-MSG-RETORNO
                                    TO WRK-OUT-MENSA-DQTM
           END-EVALUATE.

      *----------------------------------------------------------------*
       1252-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA CARREGA A TELA QUE SERA DEMONSTRADA PELO IMS.     *
      *----------------------------------------------------------------*
       1300-PROCESSAR-BRAD0660         SECTION.
      *----------------------------------------------------------------*

           CALL 'BRAD0660'             USING WRK-OUTPUT-DQTM
                                             WRK-660-DCOMDQTM.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE RETURN-CODE        TO WRK-RETURN-CODE
               MOVE 10                 TO WRK-LOCAL-ERRO
               MOVE WRK-ERRO-BRAD0660  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-ISRT               TO WRK-FUNCAO.
           MOVE WRK-OUTPUT-DQTM        TO WRK-MENSAGEM.

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
               MOVE 'DCOM0417'         TO ERR-MODULO
           ELSE
               MOVE 'DCOM0417'         TO ERR-PGM
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
