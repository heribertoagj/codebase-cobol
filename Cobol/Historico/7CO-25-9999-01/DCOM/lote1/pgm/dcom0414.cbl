      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. DCOM0414.
       AUTHOR.     MARCELO MORINA.
      *================================================================*
      *                          A L T R A N                           *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   DCOM0414                                    *
      *    PROGRAMADOR.:   MARCELO MORINA MARQUES                      *
      *    ANALISTA....:   MARCELO MORINA MARQUES                      *
      *    DATA........:   07/02/2007                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   LIBERAR CONTRATO - CONTRATOS POR AGENCIA.   *
      *----------------------------------------------------------------*
      *    TELAS.......:                                               *
      *    DCOMDNTM - LIBERAR CONTRATO - CONTRATOS POR AGENCIA.        *
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
      *    CHAMADO POR.: DCOM0413           CHAMA: DCOM0416            *
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
           '*** DCOM0414 - INICIO DA AREA DE WORKING ***'.
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
           05  WRK-IND                 PIC  9(002)         VALUE ZEROS.
           05  WRK-QTD-OPCAO-D         PIC  9(001)         VALUE ZEROS.
           05  WRK-QTD-OPCAO-L         PIC  9(001)         VALUE ZEROS.
           05  WRK-SQLCA               PIC  X(136)         VALUE SPACES.
           05  WRK-MODULO              PIC  X(008)         VALUE SPACES.
           05  WRK-SELEC-ANT           PIC  X(001)         VALUE SPACES.
           05  WRK-INCONSISTENCIA      PIC  X(001)         VALUE SPACES.
           05  WRK-LIBERADOS           PIC  X(001)         VALUE 'N'.

           05  WRK-MENSA-PF7           PIC  X(079) VALUE
               'PF7 DESPREZARA AS ATUALIZACOES EFETUADAS - TECLE NOVAMEN
      -        'TE PARA VOLTAR'.

           05  WRK-MENSA-PF8           PIC  X(079) VALUE
               'PF8 DESPREZARA AS ATUALIZACOES EFETUADAS - TECLE NOVAMEN
      -        'TE PARA AVANCAR'.

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
           '*** AREA DA TELA DCOMDNTM  - INPUT ***'.
      *----------------------------------------------------------------*
       01  WRK-INPUT-DNTM.
           05  FILLER                  PIC  X(018) VALUE SPACES.
           05  WRK-INP-PFK-DNTM        PIC  X(001) VALUE SPACES.
           05  WRK-INP-SENHAS-DNTM     PIC  X(037) VALUE SPACES.
           05  WRK-INP-COMANDO-DNTM    PIC  X(068) VALUE SPACES.
           05  WRK-INP-DADOS-DNTM.
               10  WRK-INP-FASE-DNTM-X.
                   15  WRK-INP-FASE-DNTM
                                        PIC  9(001) VALUE ZEROS.
               10  WRK-INP-TPOOPC-DNTM-X.
                   15  WRK-INP-TPOOPC-DNTM
                                        PIC  9(001) VALUE ZEROS.
               10  WRK-INP-TMSTAMP-DNTM PIC  X(026) VALUE SPACES.
               10  WRK-INP-FIMAMOS-DNTM PIC  X(001) VALUE SPACES.
               10  WRK-INP-PAGINA-DNTM-X.
                   15  WRK-INP-PAGINA-DNTM
                                        PIC  9(003) VALUE ZEROS.
               10  WRK-INP-CODAGE-DNTM-X.
                   15  WRK-INP-CODAGE-DNTM
                                        PIC  9(005) VALUE ZEROS.
               10  WRK-INP-DESCAGE-DNTM PIC  X(020) VALUE SPACES.
               10  WRK-INP-TABE-DNTM     OCCURS  05  TIMES.
                   15  WRK-INP-SELEC-DNTM     PIC  X(001) VALUE SPACES.
                   15  WRK-INP-CONTR-DNTM-X.
                       20  WRK-INP-CONTR-DNTM PIC  9(009) VALUE ZEROS.
                   15  WRK-INP-NOME-DNTM      PIC  X(030) VALUE SPACES.
                   15  WRK-INP-DTINIC-DNTM    PIC  X(010) VALUE SPACES.
                   15  WRK-INP-DTVENCT-DNTM   PIC  X(010) VALUE SPACES.
                   15  WRK-INP-LIMITE-DNTM    PIC  X(017) VALUE SPACES.
                   15  WRK-INP-SITUAC-DNTM    PIC  X(020) VALUE SPACES.
           05  WRK-INP-MENSA-DNTM       PIC  X(079) VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                       PIC  X(050)         VALUE
           '*** AREA DA TELA DCOMDNTM  - OUTPUT ***'.
      *----------------------------------------------------------------*
       01  WRK-OUTPUT-DNTM.
           05  WRK-OUT-LL-DNTM          PIC S9(004) COMP VALUE +0740.
           05  WRK-OUT-ZZ-DNTM          PIC  9(004) COMP VALUE ZEROS.
           05  WRK-OUT-SENHAS-DNTM      PIC  X(037) VALUE SPACES.
           05  WRK-OUT-COMANDO-DNTM     PIC  X(068) VALUE SPACES.
           05  WRK-OUT-DADOS-DNTM.
               10  WRK-OUT-FASE-DNTM-X.
                   15  WRK-OUT-FASE-DNTM
                                        PIC  9(001) VALUE ZEROS.
               10  WRK-OUT-TPOOPC-DNTM-X.
                   15  WRK-OUT-TPOOPC-DNTM
                                        PIC  9(001) VALUE ZEROS.
               10  WRK-OUT-TMSTAMP-DNTM PIC  X(026) VALUE SPACES.
               10  WRK-OUT-FIMAMOS-DNTM PIC  X(001) VALUE SPACES.
               10  WRK-OUT-PAGINA-DNTM-X.
                   15  WRK-OUT-PAGINA-DNTM
                                        PIC  9(003) VALUE ZEROS.
               10  WRK-OUT-CODAGE-DNTM-X.
                   15  WRK-OUT-CODAGE-DNTM
                                        PIC  9(005) VALUE ZEROS.
               10  WRK-OUT-DESCAGE-DNTM PIC  X(020) VALUE SPACES.
               10  WRK-OUT-TABELA-DNTM.
                   15  WRK-OUT-TABE-DNTM     OCCURS  05  TIMES.
                       20  WRK-OUT-SELEC-ATTR-DNTM
                                          PIC  S9(04) COMP VALUE +0.
                       20  WRK-OUT-SELEC-DNTM
                                          PIC  X(001) VALUE SPACES.
                       20  WRK-OUT-CONTR-DNTM-X.
                           25  WRK-OUT-CONTR-DNTM
                                          PIC  9(009) VALUE ZEROS.
                       20  WRK-OUT-NOME-DNTM
                                          PIC  X(030) VALUE SPACES.
                       20  WRK-OUT-DTINIC-DNTM
                                          PIC  X(010) VALUE SPACES.
                       20  WRK-OUT-DTVENCT-DNTM
                                          PIC  X(010) VALUE SPACES.
                       20  WRK-OUT-LIMITE-DNTM-X.
                           25  WRK-OUT-LIMITE-DNTM
                                          PIC  ZZ.ZZZ.ZZZ.ZZ9,99.
                       20  WRK-OUT-SITUAC-DNTM
                                          PIC  X(020) VALUE SPACES.
               10  WRK-OUT-MENSA-DNTM     PIC  X(079) VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA TELA DCOMDNTM  - BRAD0660 ***'.
      *----------------------------------------------------------------*
       01  WRK-660-DCOMDNTM.
           05  660-LL-AREA-DNTM        PIC  9(04)  COMP  VALUE 0094.
           05  660-LL-MENSAGEM-DNTM    PIC  9(04)  COMP  VALUE 0740.
           05  660-SENHAS-DNTM         PIC  9(04)  COMP  VALUE 0037.
           05  660-COMANDO-DNTM        PIC  9(04)  COMP  VALUE 0068.
           05  660-FASE-DNTM           PIC  9(04)  COMP  VALUE 0001.
           05  660-TPOOPC-DNTM         PIC  9(04)  COMP  VALUE 0001.
           05  660-TMSTAMP-DNTM        PIC  9(04)  COMP  VALUE 0026.
           05  660-FIMAMOS-DNTM        PIC  9(04)  COMP  VALUE 0001.
           05  660-PAGINA-DNTM         PIC  9(04)  COMP  VALUE 0003.
           05  660-CODAGE-DNTM         PIC  9(04)  COMP  VALUE 0005.
           05  660-DESCAGE-DNTM        PIC  9(04)  COMP  VALUE 0020.
           05  660-TABE-DNTM           OCCURS  05  TIMES.
               10  660-SELEC-DNTM      PIC  9(04)  COMP  VALUE 2003.
               10  660-CONTR-DNTM      PIC  9(04)  COMP  VALUE 0009.
               10  660-NOME-DNTM       PIC  9(04)  COMP  VALUE 0030.
               10  660-DTINIC-DNTM     PIC  9(04)  COMP  VALUE 0010.
               10  660-DTVENCT-DNTM    PIC  9(04)  COMP  VALUE 0010.
               10  660-LIMITE-DNTM     PIC  9(04)  COMP  VALUE 0017.
               10  660-SITUAC-DNTM     PIC  9(04)  COMP  VALUE 0020.
           05  660-MENSA-DNTM          PIC  9(04)  COMP  VALUE 0079.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(080)         VALUE
           '*** AREA DE PASSAGEM DE CONTROLE PARA DCOM0413 ***'.
      *----------------------------------------------------------------*
       01  COMU-AREA.
           05  COMU-LL            PIC S9(004) COMP    VALUE +230.
           05  COMU-ZZ            PIC S9(004) COMP    VALUE ZEROS.
           05  COMU-TRANCODE.
               10  COMU-TRANSACAO PIC  X(008)         VALUE SPACES.
               10  FILLER         PIC  X(006)         VALUE SPACES.
               10  COMU-PFK       PIC  X(001)         VALUE SPACES.
           05  COMU-SENHAS        PIC  X(037)         VALUE SPACES.
           05  COMU-COMANDO       PIC  X(068)         VALUE SPACES.
           05  COMU-DADOS.
               10  COMU-OPCAO     PIC  9(001)         VALUE ZEROS.
               10  COMU-TMSTAMP   PIC  X(026)         VALUE SPACES.
               10  COMU-CONTRATO  PIC  9(009)         VALUE ZEROS.
               10  COMU-AGENCIA   PIC  9(005)         VALUE ZEROS.
               10  COMU-CONTA     PIC  9(013)         VALUE ZEROS.
FS2511*        10  COMU-CPF       PIC  9(009)         VALUE ZEROS.
FS2511*        10  COMU-FILIAL    PIC  9(004)         VALUE ZEROS.
FS2511         10  COMU-CPF       PIC  X(009)         VALUE SPACES.
FS2511         10  COMU-FILIAL    PIC  X(004)         VALUE SPACES.
               10  COMU-DIGITO    PIC  9(002)         VALUE ZEROS.
               10  COMU-MENSAG    PIC  X(079)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(080)         VALUE
           '*** AREA DE RECEBIMENTO E RETORNO DO DCOM0413 ***'.
      *----------------------------------------------------------------*
       01  COMU-AREA-413.
           05  COMU-LL-413            PIC S9(004) COMP    VALUE +155.
           05  COMU-ZZ-413            PIC S9(004) COMP    VALUE ZEROS.
           05  COMU-TRANCODE-413.
               10  COMU-TRANSACAO-413 PIC  X(008)         VALUE SPACES.
               10  FILLER             PIC  X(006)         VALUE SPACES.
               10  COMU-PFK-413       PIC  X(001)         VALUE SPACES.
           05  COMU-SENHAS-413        PIC  X(037)         VALUE SPACES.
           05  COMU-COMANDO-413       PIC  X(068)         VALUE SPACES.
           05  COMU-DADOS-413.
               10  COMU-OPCAO-413     PIC  9(001)         VALUE ZEROS.
               10  COMU-TMSTAMP-413   PIC  X(026)         VALUE SPACES.
               10  COMU-CODAGE-413    PIC  9(005)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO COM TRANSACAO DCOM0416 **'.
      *----------------------------------------------------------------*
       01  COMU-AREA-416.
           05  COMU-LL-416            PIC S9(004) COMP    VALUE +244.
           05  COMU-ZZ-416            PIC S9(004) COMP    VALUE ZEROS.
           05  COMU-TRANCODE-416.
               10  COMU-TRANSACAO-416 PIC  X(008)         VALUE SPACES.
               10  FILLER             PIC  X(007)         VALUE SPACES.
           05  COMU-SENHAS-416        PIC  X(037)         VALUE SPACES.
           05  COMU-COMANDO-416       PIC  X(068)         VALUE SPACES.
           05  COMU-OPCAO-416         PIC  9(001)         VALUE ZEROS.
           05  COMU-TMSTAMP-416       PIC  X(026)         VALUE SPACES.
           05  COMU-CONTRATO-416      PIC  9(009)         VALUE ZEROS.
           05  COMU-CODAGE-416        PIC  9(005)         VALUE ZEROS.
           05  COMU-MENSA-416         PIC  X(079)         VALUE ZEROS.

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
           '*** AREA DE COMUNICACAO COM O MODULO DCOM5142 **'.
      *----------------------------------------------------------------*

       01  WRK-5142E-ENTRADA.
           03  WRK-5142E-COD-RETORNO      PIC  X(004).
           03  WRK-5142E-MSG-RETORNO      PIC  X(079).
           03  WRK-5142E-RESTART          PIC  9(005).
           03  WRK-5142E-FLAG             PIC  X(001).
           03  WRK-5142E-CFUNC-BDSCO      PIC  9(009).
           03  WRK-5142E-CTERM            PIC  X(008).
           03  WRK-5142E-CCONTR-LIM-DESC  PIC  9(009).

       01  WRK-5142S-SAIDA.
           03  WRK-5142S-HEADER.
               05  WRK-5142S-COD-RETORNO      PIC  X(004).
               05  WRK-5142S-MSG-RETORNO      PIC  X(079).
               05  WRK-5142S-RESTART          PIC  9(005).
               05  WRK-5142S-FLAG             PIC  X(001).
               05  WRK-5142S-COUNT            PIC  9(009).
               05  WRK-5142S-TAMANHO-ERRO     PIC  9(001).
               05  WRK-5142S-CAMPOS-ERRO      PIC  9(001).

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
FS2511*            10  WRK-4110S-CCNPJ-CPF          PIC  9(09).
FS2511*            10  WRK-4110S-CFLIAL-CNPJ        PIC  9(04).
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
           '*** DCOM0414 - FIM DA AREA DE WORKING ***'.
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

           MOVE 'N'                    TO  WRK-LIBERADOS
           MOVE SPACES                 TO  WRK-OUT-DADOS-DNTM
                                           WRK-OUT-SENHAS-DNTM
                                           WRK-OUT-COMANDO-DNTM
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

           MOVE 'DCOMDNTM'             TO  WRK-TELA.

           IF  WRK-TRANSACAO    NOT EQUAL  'DCOM0414'
               PERFORM 1100-RECEBER-CONTROLE
           ELSE
               PERFORM 1200-PROCESSAR-DCOMDNTM
           END-IF.

           IF  WRK-TELA             EQUAL  'DCOMDNTM'
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
               MOVE COMU-OPCAO-413     TO  WRK-INP-TPOOPC-DNTM
                                           WRK-OUT-TPOOPC-DNTM
               MOVE COMU-TMSTAMP-413   TO  WRK-INP-TMSTAMP-DNTM
                                           WRK-OUT-TMSTAMP-DNTM
               MOVE COMU-CODAGE-413    TO  WRK-INP-CODAGE-DNTM
                                           WRK-OUT-CODAGE-DNTM
           ELSE
             IF  WRK-TRANSACAO  EQUAL  'DCOM0416'
                 MOVE WRK-MENSAGEM     TO  COMU-AREA-416
                 MOVE COMU-OPCAO-416   TO  WRK-INP-TPOOPC-DNTM
                                           WRK-OUT-TPOOPC-DNTM
                 MOVE COMU-TMSTAMP-416 TO  WRK-INP-TMSTAMP-DNTM
                                           WRK-OUT-TMSTAMP-DNTM
                 MOVE COMU-CODAGE-416  TO  WRK-INP-CODAGE-DNTM
                                           WRK-OUT-CODAGE-DNTM
             END-IF
           END-IF.

           PERFORM 1110-ACESSAR-DCOM6416

           MOVE ZEROS                  TO  WRK-4110E-RESTART

           PERFORM 1120-ACESSAR-DCOM4110

           MOVE 1                      TO  WRK-INP-PAGINA-DNTM
                                           WRK-OUT-PAGINA-DNTM
                                           WRK-INP-FASE-DNTM
                                           WRK-OUT-FASE-DNTM.

           MOVE 'SELECIONE UM REGISTRO COM "D" OU VARIOS COM "L" E TECLE
      -         ' <ENTER>'             TO  WRK-OUT-MENSA-DNTM.

           IF  WRK-TRANSACAO       EQUAL  'DCOM0416'
               IF COMU-MENSA-416   NOT EQUAL SPACES
                  MOVE COMU-MENSA-416  TO  WRK-OUT-MENSA-DNTM
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
           MOVE WRK-INP-CODAGE-DNTM    TO  WRK-6416E-DEPDC
           MOVE 'DCOM6416'             TO  WRK-MODULO

           CALL WRK-MODULO    USING    WRK-6416E-ENTRADA
                                       WRK-6416S-SAIDA
                                       ERRO-AREA
                                       WRK-SQLCA.

           EVALUATE WRK-6416S-COD-RETORNO
               WHEN '0000'
                    MOVE WRK-6416S-NOME-DEPDC   TO WRK-INP-DESCAGE-DNTM
                                                   WRK-OUT-DESCAGE-DNTM

               WHEN '0003'
                    MOVE  ALL '*'               TO WRK-INP-DESCAGE-DNTM
                                                   WRK-OUT-DESCAGE-DNTM

               WHEN '0099'
                    MOVE  'DB2'     TO     ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                    MOVE  WRK-6416S-MSG-RETORNO
                                        TO WRK-OUT-MENSA-DNTM
           END-EVALUATE.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA ACESSA O MODULO 6COM4110                          *
      *----------------------------------------------------------------*
       1120-ACESSAR-DCOM4110           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-COD-USER           TO  WRK-4110E-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO  WRK-4110E-CTERM
           MOVE 'L'                    TO  WRK-4110E-FLAG-FILTRO
           MOVE 'S'                    TO  WRK-4110E-START-COUNT
           MOVE SPACES                 TO  WRK-4110E-FLAG
           MOVE WRK-INP-CODAGE-DNTM    TO  WRK-4110E-CAG-BCRIA
           MOVE 'DCOM4110'             TO  WRK-MODULO

           CALL WRK-MODULO    USING    WRK-4110E-ENTRADA
                                       WRK-4110S-SAIDA
                                       ERRO-AREA
                                       WRK-SQLCA.

           EVALUATE WRK-4110S-COD-RETORNO
               WHEN '0000'
                    IF (WRK-4110S-CCONTR-LIM-DESC(6)  NOT NUMERIC) OR
                       (WRK-4110S-CCONTR-LIM-DESC(6)  EQUAL ZEROS)
                       MOVE '*'     TO  WRK-OUT-FIMAMOS-DNTM
                       MOVE 'FINAL DE AMOSTRAGEM'
                                    TO  WRK-OUT-MENSA-DNTM
                    ELSE
                       MOVE SPACES  TO  WRK-OUT-FIMAMOS-DNTM
                       MOVE 'AMOSTRAGEM CONTINUA'
                                    TO  WRK-OUT-MENSA-DNTM
                    END-IF
                    PERFORM 1121-MONTAR-TELA VARYING WRK-IND FROM
                            1 BY 1 UNTIL WRK-IND GREATER 5

               WHEN '0003'
                    IF  WRK-LIBERADOS          EQUAL 'N'
                        MOVE  SG-MENSAGEM(0197)       TO COMU-MENSAG
                        PERFORM 1220-RETORNAR-CHAMADOR
                    ELSE
                        MOVE  SG-MENSAGEM(0202)       TO COMU-MENSAG
                        PERFORM 1220-RETORNAR-CHAMADOR
                    END-IF

               WHEN '0088'
                    MOVE  'DB2'     TO     ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
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

           MOVE  SPACES           TO WRK-OUT-SELEC-DNTM(WRK-IND)

           IF (WRK-4110S-CCONTR-LIM-DESC(WRK-IND) IS NUMERIC) AND
              (WRK-4110S-CCONTR-LIM-DESC(WRK-IND) GREATER ZEROS)
              MOVE WRK-4110S-CCONTR-LIM-DESC(WRK-IND)
                                  TO WRK-OUT-CONTR-DNTM(WRK-IND)
              MOVE WRK-4110S-NM-CLIE(WRK-IND)
                                  TO WRK-OUT-NOME-DNTM(WRK-IND)
              MOVE WRK-4110S-DINIC-VGCIA-CONTR(WRK-IND)
                                  TO WRK-OUT-DTINIC-DNTM(WRK-IND)
              INSPECT WRK-OUT-DTINIC-DNTM(WRK-IND) REPLACING
                                                    ALL '.' BY '/'
              MOVE WRK-4110S-DVCTO-CONTR-LIM(WRK-IND)
                                  TO WRK-OUT-DTVENCT-DNTM(WRK-IND)
              INSPECT WRK-OUT-DTVENCT-DNTM(WRK-IND) REPLACING
                                                    ALL '.' BY '/'
              MOVE WRK-4110S-VCONTR-LIM(WRK-IND)
                                  TO WRK-OUT-LIMITE-DNTM(WRK-IND)
              MOVE WRK-4110S-IRSUMO-SIT-DESC(WRK-IND)
                                  TO WRK-OUT-SITUAC-DNTM(WRK-IND)
           ELSE
              MOVE  WRK-241       TO WRK-OUT-SELEC-ATTR-DNTM(WRK-IND)
           END-IF.

      *----------------------------------------------------------------*
       1121-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA PROCESSA A MENSAGEM VINDA DA TELA DCOMDNTM        *
      *----------------------------------------------------------------*
       1200-PROCESSAR-DCOMDNTM         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM             TO WRK-INPUT-DNTM.
           PERFORM 1210-DEVOLVER-TELA-FIXA

           EVALUATE TRUE
               WHEN WRK-INP-PFK-DNTM     EQUAL 'H' OR
                   (WRK-INP-PFK-DNTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DNTM EQUAL 'PFK1')
                    PERFORM 1211-DEVOLVER-TELA-VARIA
                    MOVE WRK-INP-MENSA-DNTM TO WRK-OUT-MENSA-DNTM

               WHEN WRK-INP-PFK-DNTM     EQUAL '3' OR
                   (WRK-INP-PFK-DNTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DNTM EQUAL 'PFK3')
                    PERFORM 1220-RETORNAR-CHAMADOR

               WHEN WRK-INP-PFK-DNTM     EQUAL 'X' OR
                   (WRK-INP-PFK-DNTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DNTM EQUAL 'PFK5')
                    PERFORM 1230-RETORNAR-MENU-ROTINAS

               WHEN WRK-INP-PFK-DNTM     EQUAL 'Z' OR
                   (WRK-INP-PFK-DNTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DNTM EQUAL 'PFK10')
                    PERFORM 1240-RETORNAR-MENU-DCOM

               WHEN WRK-INP-FASE-DNTM    EQUAL '1'
                    PERFORM 1250-TRATAR-FASE-1

               WHEN WRK-INP-FASE-DNTM    EQUAL '2'
                    PERFORM 1260-TRATAR-FASE-2

               WHEN OTHER
                     MOVE SG-MENSAGEM(38)  TO  WRK-OUT-MENSA-DNTM
           END-EVALUATE.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA DEVOLVE OS DADOS FIXOS DA TELA SEM ALTERACAO      *
      *----------------------------------------------------------------*
       1210-DEVOLVER-TELA-FIXA         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-FIMAMOS-DNTM   TO WRK-OUT-FIMAMOS-DNTM.
           MOVE WRK-INP-TMSTAMP-DNTM   TO WRK-OUT-TMSTAMP-DNTM.
           MOVE WRK-INP-FASE-DNTM-X    TO WRK-OUT-FASE-DNTM-X.
           MOVE WRK-INP-TPOOPC-DNTM-X  TO WRK-OUT-TPOOPC-DNTM-X.
           MOVE WRK-INP-PAGINA-DNTM-X  TO WRK-OUT-PAGINA-DNTM-X.
           MOVE WRK-INP-CODAGE-DNTM-X  TO WRK-OUT-CODAGE-DNTM-X.
           MOVE WRK-INP-DESCAGE-DNTM   TO WRK-OUT-DESCAGE-DNTM.

      *----------------------------------------------------------------*
       1210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA DEVOLVE OS DADOS VARIAVEIS DA TELA S/ ALTERACAO   *
      *----------------------------------------------------------------*
       1211-DEVOLVER-TELA-VARIA        SECTION.
      *----------------------------------------------------------------*

           PERFORM VARYING WRK-IND FROM 1 BY 1 UNTIL WRK-IND GREATER 5
              IF  (WRK-INP-CONTR-DNTM(WRK-IND)  IS NUMERIC) AND
                  (WRK-INP-CONTR-DNTM(WRK-IND)  GREATER ZEROS)
                   MOVE WRK-INP-SELEC-DNTM(WRK-IND)
                                      TO WRK-OUT-SELEC-DNTM(WRK-IND)
                   MOVE WRK-INP-CONTR-DNTM-X(WRK-IND)
                                      TO WRK-OUT-CONTR-DNTM-X(WRK-IND)
                   MOVE WRK-INP-NOME-DNTM(WRK-IND)
                                      TO WRK-OUT-NOME-DNTM(WRK-IND)
                   MOVE WRK-INP-DTINIC-DNTM(WRK-IND)
                                      TO WRK-OUT-DTINIC-DNTM(WRK-IND)
                   MOVE WRK-INP-DTVENCT-DNTM(WRK-IND)
                                      TO WRK-OUT-DTVENCT-DNTM(WRK-IND)
                   MOVE WRK-INP-LIMITE-DNTM(WRK-IND)
                                      TO WRK-OUT-LIMITE-DNTM-X(WRK-IND)
                   MOVE WRK-INP-SITUAC-DNTM(WRK-IND)
                                      TO WRK-OUT-SITUAC-DNTM(WRK-IND)
              ELSE
                   MOVE  WRK-241  TO WRK-OUT-SELEC-ATTR-DNTM(WRK-IND)
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

           MOVE LENGTH OF COMU-AREA  TO COMU-LL.
           MOVE 'DCOM0413'           TO WRK-TELA.
           MOVE 'DCOM0414'           TO COMU-TRANSACAO.
           MOVE WRK-INP-TPOOPC-DNTM  TO COMU-OPCAO.
           MOVE WRK-INP-TMSTAMP-DNTM TO COMU-TMSTAMP.
           MOVE WRK-CHNG             TO WRK-FUNCAO.
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
           MOVE 'DCOM0414'          TO WRK-TELA
                                       COMU-TRANSACAO-413.
           MOVE '5'                 TO COMU-PFK-413.
           MOVE COMU-AREA-413      TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRATAMENTO DE RETORNO AO MENU DCOM       *
      *----------------------------------------------------------------*
       1240-RETORNAR-MENU-DCOM         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-CHNG           TO WRK-FUNCAO.
           MOVE 'DCOM0414'         TO WRK-TELA
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
               WHEN WRK-INP-PFK-DNTM     EQUAL '7' OR
                   (WRK-INP-PFK-DNTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DNTM EQUAL 'PFK7')
                    PERFORM 1251-TRATAR-VOLTA-PAG

               WHEN WRK-INP-PFK-DNTM     EQUAL '8' OR
                   (WRK-INP-PFK-DNTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DNTM EQUAL 'PFK8')
                    PERFORM 1252-TRATAR-AVANCA-PAG

               WHEN  WRK-INP-PFK-DNTM     EQUAL '.' AND
                     WRK-INP-COMANDO-DNTM EQUAL  SPACES OR LOW-VALUES
                     PERFORM 1211-DEVOLVER-TELA-VARIA
                     PERFORM 1253-TRATAR-ENTER
             WHEN  OTHER
                   PERFORM 1211-DEVOLVER-TELA-VARIA
                   MOVE SG-MENSAGEM(38)
                                     TO  WRK-OUT-MENSA-DNTM
           END-EVALUATE.

      *----------------------------------------------------------------*
       1250-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRAMAMENTO DO COMANDO PFK7 - VOLTA PAG.  *
      *----------------------------------------------------------------*
       1251-TRATAR-VOLTA-PAG           SECTION.
      *----------------------------------------------------------------*

           IF WRK-INP-PAGINA-DNTM  EQUAL  1
              PERFORM 1211-DEVOLVER-TELA-VARIA
              MOVE 'NAO EXISTE PAGINA PARA VOLTAR'
                                       TO WRK-OUT-MENSA-DNTM
              GO TO 1251-99-FIM
           END-IF.

           IF  WRK-INP-MENSA-DNTM      NOT EQUAL WRK-MENSA-PF7

               MOVE ZEROS            TO WRK-QTD-OPCAO-D
                                        WRK-QTD-OPCAO-L

               PERFORM VARYING WRK-IND FROM 1 BY 1
                       UNTIL   WRK-IND GREATER 5

                   IF  WRK-INP-SELEC-DNTM(WRK-IND) EQUAL 'D'
                       ADD  1          TO WRK-QTD-OPCAO-D
                   ELSE
                       IF  WRK-INP-SELEC-DNTM(WRK-IND) EQUAL 'L'
                           ADD  1          TO WRK-QTD-OPCAO-L
                       END-IF
                   END-IF

               END-PERFORM

               IF  WRK-QTD-OPCAO-D      GREATER ZEROS OR
                   WRK-QTD-OPCAO-L      GREATER ZEROS
                   PERFORM 1211-DEVOLVER-TELA-VARIA
                   MOVE WRK-MENSA-PF7           TO  WRK-OUT-MENSA-DNTM
                   GO  TO 1251-99-FIM
               END-IF

           END-IF.

           MOVE  SPACES                TO WRK-OUT-TABELA-DNTM
           COMPUTE WRK-4110E-RESTART = (WRK-INP-PAGINA-DNTM - 2) * 5

           PERFORM 1120-ACESSAR-DCOM4110

           COMPUTE WRK-OUT-PAGINA-DNTM = WRK-INP-PAGINA-DNTM - 1.

      *----------------------------------------------------------------*
       1251-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRAMAMENTO DO COMANDO PFK8 - AVANCA PAG. *
      *----------------------------------------------------------------*
       1252-TRATAR-AVANCA-PAG          SECTION.
      *----------------------------------------------------------------*

           IF WRK-INP-FIMAMOS-DNTM  EQUAL  '*'
              PERFORM 1211-DEVOLVER-TELA-VARIA
              MOVE 'NAO EXISTE PAGINA PARA AVANCAR'
                                       TO WRK-OUT-MENSA-DNTM
              GO TO 1252-99-FIM
           END-IF.

           IF  WRK-INP-MENSA-DNTM      NOT EQUAL WRK-MENSA-PF8

               MOVE ZEROS            TO WRK-QTD-OPCAO-D
                                        WRK-QTD-OPCAO-L

               PERFORM VARYING WRK-IND FROM 1 BY 1
                       UNTIL   WRK-IND GREATER 5

                   IF  WRK-INP-SELEC-DNTM(WRK-IND) EQUAL 'D'
                       ADD  1          TO WRK-QTD-OPCAO-D
                   ELSE
                       IF  WRK-INP-SELEC-DNTM(WRK-IND) EQUAL 'L'
                           ADD  1          TO WRK-QTD-OPCAO-L
                       END-IF
                   END-IF

               END-PERFORM

               IF  WRK-QTD-OPCAO-D      GREATER ZEROS OR
                   WRK-QTD-OPCAO-L      GREATER ZEROS
                   PERFORM 1211-DEVOLVER-TELA-VARIA
                   MOVE WRK-MENSA-PF8           TO  WRK-OUT-MENSA-DNTM
                   GO  TO 1252-99-FIM
               END-IF

           END-IF.

           MOVE  SPACES                TO WRK-OUT-TABELA-DNTM
           COMPUTE WRK-4110E-RESTART = (WRK-INP-PAGINA-DNTM) * 5

           PERFORM 1120-ACESSAR-DCOM4110

           COMPUTE WRK-OUT-PAGINA-DNTM = WRK-INP-PAGINA-DNTM + 1.

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

           PERFORM VARYING WRK-IND FROM 1 BY 1  UNTIL WRK-IND GREATER 5
               IF WRK-INP-SELEC-DNTM(WRK-IND) EQUAL 'D'
                  PERFORM 1262-CHAMAR-TRAN-DCOM0416
                  GO TO 1253-99-FIM
               END-IF
           END-PERFORM.

           PERFORM VARYING WRK-IND FROM 1 BY 1 UNTIL WRK-IND > 5
                   MOVE WRK-225    TO WRK-OUT-SELEC-ATTR-DNTM(WRK-IND)
           END-PERFORM.

           MOVE 2                      TO WRK-OUT-FASE-DNTM.
           MOVE 'TECLE <PF6> PARA EFETIVAR A LIBERACAO OU <ENTER> PARA A
      -         'LTERAR'               TO  WRK-OUT-MENSA-DNTM.

      *----------------------------------------------------------------*
       1253-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA A CONSISTENCIA FISICA                      *
      *----------------------------------------------------------------*
       1254-CONSISTIR-FISICA           SECTION.
      *----------------------------------------------------------------*

           PERFORM VARYING WRK-IND FROM 1 BY 1 UNTIL WRK-IND GREATER 5
             IF  WRK-INP-SELEC-DNTM(WRK-IND) NOT EQUAL 'L'       AND
                 WRK-INP-SELEC-DNTM(WRK-IND) NOT EQUAL 'D'       AND
                 WRK-INP-SELEC-DNTM(WRK-IND) NOT EQUAL SPACES    AND
                 WRK-INP-SELEC-DNTM(WRK-IND) NOT EQUAL LOW-VALUES
                 MOVE '*'        TO WRK-INCONSISTENCIA
                 MOVE WRK-49353  TO WRK-OUT-SELEC-ATTR-DNTM(WRK-IND)
                 MOVE 'OPCAO INVALIDA - SELECIONAR APENAS: L/D'
                                 TO  WRK-OUT-MENSA-DNTM
             END-IF
           END-PERFORM.

           IF WRK-INCONSISTENCIA  EQUAL  '*'
              GO TO 1254-99-FIM
           END-IF.

           MOVE    ZEROS         TO WRK-QTD-OPCAO-D
                                    WRK-QTD-OPCAO-L.

           PERFORM VARYING WRK-IND FROM 1 BY 1 UNTIL WRK-IND GREATER 5
             IF  WRK-INP-SELEC-DNTM(WRK-IND) EQUAL 'D'
                 ADD  1          TO WRK-QTD-OPCAO-D
             ELSE
                 IF  WRK-INP-SELEC-DNTM(WRK-IND) EQUAL 'L'
                     ADD  1          TO WRK-QTD-OPCAO-L
                 END-IF
             END-IF
           END-PERFORM.

           IF WRK-QTD-OPCAO-D  GREATER  1
              MOVE '*'             TO WRK-INCONSISTENCIA
              MOVE 'SELECIONAR APENAS UMA OCORRENCIA PARA OPCAO "D"'
                                   TO  WRK-OUT-MENSA-DNTM
              PERFORM VARYING WRK-IND FROM 1 BY 1
                                               UNTIL WRK-IND GREATER 5
                IF WRK-INP-SELEC-DNTM(WRK-IND) EQUAL 'D'
                   MOVE WRK-49353  TO WRK-OUT-SELEC-ATTR-DNTM(WRK-IND)
                END-IF
              END-PERFORM
           END-IF.

           IF WRK-INCONSISTENCIA  EQUAL  '*'
              GO TO 1254-99-FIM
           END-IF.

           IF WRK-QTD-OPCAO-L  GREATER ZEROS  AND
              WRK-QTD-OPCAO-D  GREATER ZEROS
              MOVE '*'           TO WRK-INCONSISTENCIA
              MOVE 'SELECIONAR APENAS UMA OPCAO: L/D'
                                 TO  WRK-OUT-MENSA-DNTM
              PERFORM VARYING WRK-IND FROM 1 BY 1
                                               UNTIL WRK-IND GREATER 5
                   IF (WRK-INP-SELEC-DNTM(WRK-IND) EQUAL 'L'  OR
                       WRK-INP-SELEC-DNTM(WRK-IND) EQUAL 'D')
                       MOVE WRK-49353
                                    TO WRK-OUT-SELEC-ATTR-DNTM(WRK-IND)
                   END-IF
              END-PERFORM
           END-IF.

           IF WRK-INCONSISTENCIA  EQUAL  '*'
              GO TO 1254-99-FIM
           END-IF.

           IF WRK-QTD-OPCAO-L    EQUAL  ZEROS AND
              WRK-QTD-OPCAO-D    EQUAL  ZEROS
              MOVE '*'        TO WRK-INCONSISTENCIA
              MOVE WRK-49345  TO WRK-OUT-SELEC-ATTR-DNTM(1)
              MOVE 'NENHUMA OPCAO SELECIONADA'
                              TO  WRK-OUT-MENSA-DNTM
           END-IF.

      *----------------------------------------------------------------*
       1254-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1260-TRATAR-FASE-2              SECTION.
      *----------------------------------------------------------------*

           EVALUATE TRUE

               WHEN  WRK-INP-PFK-DNTM     EQUAL '6' OR
                    (WRK-INP-PFK-DNTM     EQUAL '.' AND
                     WRK-INP-COMANDO-DNTM EQUAL 'PFK6')
                     PERFORM 1261-TRATAR-CONFIRMA

               WHEN  WRK-INP-PFK-DNTM     EQUAL '.' AND
                     WRK-INP-COMANDO-DNTM EQUAL  SPACES OR LOW-VALUES
                     PERFORM VARYING WRK-IND FROM 1 BY 1
                                                      UNTIL WRK-IND > 5
                           MOVE WRK-49345
                                     TO WRK-OUT-SELEC-ATTR-DNTM(WRK-IND)
                     END-PERFORM
                     PERFORM 1211-DEVOLVER-TELA-VARIA
                     MOVE '1'        TO WRK-OUT-FASE-DNTM
           MOVE 'SELECIONE UM REGISTRO COM "D" OU VARIOS COM "L" E TECLE
      -         ' <ENTER>'             TO  WRK-OUT-MENSA-DNTM

               WHEN  OTHER
                     PERFORM 1211-DEVOLVER-TELA-VARIA
                     PERFORM VARYING WRK-IND FROM 1 BY 1
                                                      UNTIL WRK-IND > 5
                           MOVE WRK-225
                                     TO WRK-OUT-SELEC-ATTR-DNTM(WRK-IND)
                     END-PERFORM
                     MOVE SG-MENSAGEM(38)
                                     TO  WRK-OUT-MENSA-DNTM
           END-EVALUATE.

      *----------------------------------------------------------------*
       1260-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRAMAMENTO DO COMANDO CONFIRMA(PFK6)     *
      *----------------------------------------------------------------*
       1261-TRATAR-CONFIRMA            SECTION.
      *----------------------------------------------------------------*

           PERFORM VARYING WRK-IND FROM 1 BY 1 UNTIL WRK-IND GREATER 5
             IF  WRK-INP-SELEC-DNTM(WRK-IND) EQUAL 'L'
                 PERFORM 1263-CHAMAR-MOD-DCOM5142
             END-IF
           END-PERFORM.

           IF  WRK-5142S-COD-RETORNO    EQUAL '0000'
               MOVE ZEROS      TO  WRK-4110E-RESTART
               MOVE 'S'        TO  WRK-LIBERADOS
               PERFORM 1120-ACESSAR-DCOM4110
               MOVE 1          TO  WRK-INP-PAGINA-DNTM
                                   WRK-OUT-PAGINA-DNTM
                                   WRK-INP-FASE-DNTM
                                   WRK-OUT-FASE-DNTM
               MOVE 'LIBERACOE(S) EFETUADA COM SUCESSO'
                                TO WRK-OUT-MENSA-DNTM

               PERFORM VARYING WRK-IND FROM 1 BY 1
                               UNTIL WRK-IND > 5
                       MOVE WRK-49345
                                TO WRK-OUT-SELEC-ATTR-DNTM(WRK-IND)
               END-PERFORM
           END-IF.

      *----------------------------------------------------------------*
       1261-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA PASSA O CONTROLE PARA TRAN. DCOM0416              *
      *----------------------------------------------------------------*
       1262-CHAMAR-TRAN-DCOM0416       SECTION.
      *----------------------------------------------------------------*

           MOVE LENGTH COMU-AREA-416        TO COMU-LL-416
           MOVE WRK-CHNG                    TO WRK-FUNCAO.
           MOVE 'DCOM0416'                  TO WRK-TELA.
           MOVE 'DCOM0414'                  TO COMU-TRANSACAO-416
           MOVE WRK-INP-TPOOPC-DNTM         TO COMU-OPCAO-416
           MOVE WRK-INP-TMSTAMP-DNTM        TO COMU-TMSTAMP-416
           MOVE WRK-INP-SENHAS-DNTM         TO COMU-SENHAS-416
           MOVE WRK-INP-CONTR-DNTM(WRK-IND) TO COMU-CONTRATO-416
           MOVE WRK-INP-CODAGE-DNTM         TO COMU-CODAGE-416
           MOVE COMU-AREA-416               TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1262-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA PASSA O CONTROLE PARA MODULO DCOM5142             *
      *----------------------------------------------------------------*
       1263-CHAMAR-MOD-DCOM5142        SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO WRK-5142E-COD-RETORNO
           MOVE SPACES                 TO WRK-5142E-MSG-RETORNO
           MOVE ZEROS                  TO WRK-5142E-RESTART
           MOVE SPACES                 TO WRK-5142E-FLAG
           MOVE WRK-COD-USER           TO WRK-5142E-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO WRK-5142E-CTERM
           MOVE WRK-INP-CONTR-DNTM(WRK-IND)
                                       TO WRK-5142E-CCONTR-LIM-DESC
           MOVE 'DCOM5142'             TO WRK-MODULO

           CALL WRK-MODULO    USING    WRK-5142E-ENTRADA
                                       WRK-5142S-SAIDA
                                       ERRO-AREA
                                       WRK-SQLCA.

           EVALUATE WRK-5142S-COD-RETORNO
               WHEN '0099'
                    MOVE  'DB2'     TO     ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN '0000'
                    CONTINUE

               WHEN OTHER
                    PERFORM 1210-DEVOLVER-TELA-FIXA
                    PERFORM 1211-DEVOLVER-TELA-VARIA
                    PERFORM VARYING WRK-IND FROM 1 BY 1
                                                      UNTIL WRK-IND > 5
                          MOVE WRK-225
                                    TO WRK-OUT-SELEC-ATTR-DNTM(WRK-IND)
                    END-PERFORM
                    MOVE  WRK-5142S-MSG-RETORNO TO WRK-OUT-MENSA-DNTM
           END-EVALUATE.

      *----------------------------------------------------------------*
       1252-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA CARREGA A TELA QUE SERA DEMONSTRADA PELO IMS.     *
      *----------------------------------------------------------------*
       1300-PROCESSAR-BRAD0660         SECTION.
      *----------------------------------------------------------------*

           CALL 'BRAD0660'             USING WRK-OUTPUT-DNTM
                                             WRK-660-DCOMDNTM.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE RETURN-CODE        TO WRK-RETURN-CODE
               MOVE 10                 TO WRK-LOCAL-ERRO
               MOVE WRK-ERRO-BRAD0660  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-ISRT               TO WRK-FUNCAO.
           MOVE WRK-OUTPUT-DNTM        TO WRK-MENSAGEM.

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
               MOVE 'DCOM0414'         TO ERR-MODULO
           ELSE
               MOVE 'DCOM0414'         TO ERR-PGM
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
