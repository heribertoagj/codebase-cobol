      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. DCOM0421.
       AUTHOR.     MARCELO MORINA.
      *================================================================*
      *                          A L T R A N                           *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   DCOM0421                                    *
      *    PROGRAMADOR.:   MARCELO MORINA MARQUES                      *
      *    ANALISTA....:   MARCELO MORINA MARQUES                      *
      *    DATA........:   16/03/2007                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   CONSULTAR LIBERACAO CONTRATOS POR CLIENTE   *
      *----------------------------------------------------------------*
      *    TELAS.......:                                               *
      *    DCOMDUTM - CONSULTAR LIBERACAO CONTRATOS POR CIENTES        *
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
      *    CHAMADO POR.: DCOM0413             CHAMA: DCOM0422          *
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
           '*** DCOM0421 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ATRIBUTOS ***'.
      *---------------------------------------------------------------*
      *--- NUMERIC  - NORMAL    - PROTEGIDO    - NAO CURSOR - NUM  ---*
       01  WRK-241                     PIC S9(008) COMP    VALUE +241.
      *--- NUMERICO - NORMAL    - PROTEGIDO    - POS CURSOR - NUM  ---*
       01  WRK-49361                   PIC S9(008) COMP    VALUE +49361.
      *--- NUMERICO - BRILHANTE - DESPROTEGIDO - POS CURSOR - NUM  ---*
       01  WRK-49369                   PIC S9(008) COMP    VALUE +49369.
      *--- BRILHANTE, NORMAL    - DEPROTEGIDO  - POS CURSOR - ALFA ---*
       01  WRK-49353                   PIC S9(008) COMP    VALUE +49353.
      *--- ALFA       NORMAL    - DEPROTEGIDO  - POS CURSOR - ALFA ---*
       01  WRK-49345                   PIC S9(008) COMP    VALUE +49345.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*
       01  WRK-AUXILIARES.
           05  WRK-IND                 PIC  9(002)         VALUE ZEROS.
           05  WRK-QTD-D               PIC  9(001)         VALUE ZEROS.
           05  WRK-QTD-SPACES          PIC  9(001)         VALUE ZEROS.
           05  WRK-SQLCA               PIC  X(136)         VALUE SPACES.
           05  WRK-MODULO              PIC  X(008)         VALUE SPACES.
           05  WRK-SELEC-ANT           PIC  X(001)         VALUE SPACES.
           05  WRK-INCONSIS            PIC  X(001)         VALUE SPACES.
           05  WRK-FLAG-MENSA          PIC  X(001)         VALUE 'N'.

FS2511*01  WRK-CNPJCPF-COMPL.
FS2511*    05  WRK-CNPJ.
FS2511*        07  WRK-CNPJ-N          PIC  999.999.999.
FS2511*    05  WRK-BARRA               PIC  X(001)         VALUE '/'.
FS2511*    05  WRK-FILIAL.
FS2511*        07  WRK-FILIAL-N        PIC  9(004)         VALUE ZEROS.
FS2511*    05  WRK-TRACO               PIC  X(001)         VALUE '-'.
FS2511*    05  WRK-CONTROLE.
FS2511*        07  WRK-CONTROLE-N      PIC  9(002)         VALUE ZEROS.
FS2511 01  WRK-CNPJCPF-COMPL-ST.
FS2511     05  WRK-CNPJ-ST.
FS2511         07  WRK-CNPJ-N-ST       PIC  X(009).
FS2511     05  WRK-BARRA-ST            PIC  X(001)         VALUE '/'.
FS2511     05  WRK-FILIAL-ST.
FS2511         07  WRK-FILIAL-N-ST     PIC  X(004)         VALUE SPACES.
FS2511     05  WRK-TRACO-ST            PIC  X(001)         VALUE '-'.
FS2511     05  WRK-CONTROLE-ST.
FS2511         07  WRK-CONTROLE-N-ST   PIC  9(002)         VALUE ZEROS.

       01  WRK-AGENCIA-AUX.
           05  WRK-AGENCIA-NUM         PIC  9(005)         VALUE ZEROS.

      *----------------------------------------------------------------*
      *   AREA DA BRAD0431                                             *
      *----------------------------------------------------------------*

       01  AREA-BRAD0431.
           03  WRK-CONTA-0431          PIC  9(13) COMP-3.
           03  WRK-CONTA-0431-R        PIC  9(13)          VALUE ZEROS.
           03  WRK-DIGITO-0431         PIC  X(01)          VALUE SPACES.
           03  WRK-DIGITO-0431-R REDEFINES WRK-DIGITO-0431 PIC 9(01).
           03  WRK-TAMANHO-0431        PIC  9(02)          VALUE 7.

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
           '*** AREA DA TELA DCOMDUTM  - INPUT ***'.
      *----------------------------------------------------------------*

       01  WRK-INPUT-DUTM.
           05  FILLER                         PIC  X(018) VALUE SPACES.
           05  WRK-INP-PFK-DUTM               PIC  X(001) VALUE SPACES.
           05  WRK-INP-SENHAS-DUTM            PIC  X(037) VALUE SPACES.
           05  WRK-INP-COMANDO-DUTM           PIC  X(068) VALUE SPACES.
           05  WRK-INP-DADOS-DUTM.
               10  WRK-INP-FIMAMOS-DUTM       PIC  X(001) VALUE SPACES.
               10  WRK-INP-PAGINA-DUTM-X.
                   15  WRK-INP-PAGINA-DUTM    PIC  9(003) VALUE ZEROS.
               10  WRK-INP-TPOOPC-DUTM        PIC  X(001) VALUE SPACES.
               10  WRK-INP-TIPO-DUTM.
                   15  WRK-INP-TIPO-DUTM-N    PIC  9(001) VALUE ZEROS.
               10  WRK-INP-TMSTAMP-DUTM       PIC  X(026) VALUE SPACES.
               10  WRK-INP-CLIENTE-DUTM       PIC  X(040) VALUE SPACES.
FS2511*        10  WRK-INP-CNPJCPF-DUTM.
FS2511*            15  WRK-INP-NUMERO-DUTM    PIC  999.999.999.
FS2511*            15  WRK-INP-BARRA-1-DUTM   PIC  X(01)  VALUE SPACES.
FS2511*            15  WRK-INP-FILIAL-DUTM    PIC  9(04)  VALUE ZEROS.
FS2511*            15  WRK-INP-HIFEN-2-DUTM   PIC  X(01)  VALUE SPACES.
FS2511*            15  WRK-INP-CONTROLE-DUTM  PIC  9(02)  VALUE ZEROS.
FS2511         10  WRK-INP-CNPJCPF-DUTM-ST.
FS2511             15  WRK-INP-NUMERO-DUTM-ST   PIC  X(11) VALUE SPACES.
FS2511             15  WRK-INP-BARRA-1-DUTM-ST  PIC  X(01) VALUE SPACES.
FS2511             15  WRK-INP-FILIAL-DUTM-ST   PIC  X(04) VALUE SPACES.
FS2511             15  WRK-INP-HIFEN-2-DUTM-ST  PIC  X(01) VALUE SPACES.
FS2511             15  WRK-INP-CONTROLE-DUTM-ST PIC  9(02) VALUE ZEROS.
               10  WRK-INP-BANCO-X            PIC  X(006) VALUE SPACES.
               10  WRK-INP-BANCO-DUTM-X.
                   15  WRK-INP-BANCO-DUTM     PIC  9(003) VALUE ZEROS.
               10  WRK-INP-AGENX-X            PIC  X(008) VALUE SPACES.
               10  WRK-INP-CODAGE-DUTM-X.
                   15  WRK-INP-CODAGE-DUTM    PIC  9(005) VALUE ZEROS.
               10  WRK-INP-TRACO-X            PIC  X(001) VALUE SPACES.
               10  WRK-INP-DESCAGE-DUTM       PIC  X(020) VALUE SPACES.
               10  WRK-INP-CONTA-X            PIC  X(009) VALUE SPACES.
               10  WRK-INP-CONTA-DUTM-X.
                   15  WRK-INP-CONTA-DUTM     PIC  9(013) VALUE ZEROS.
               10  WRK-INP-TABELA-DUTM     OCCURS  05  TIMES.
                   15  WRK-INP-SELEC-DUTM     PIC  X(001) VALUE SPACES.
                   15  WRK-INP-CONTR-DUTM-X.
                       20  WRK-INP-CONTR-DUTM PIC  9(009) VALUE ZEROS.
                   15  WRK-INP-NOME-DUTM      PIC  X(030) VALUE SPACES.
                   15  WRK-INP-DTINIC-DUTM    PIC  X(010) VALUE SPACES.
                   15  WRK-INP-DTVENCT-DUTM   PIC  X(010) VALUE SPACES.
                   15  WRK-INP-LIMITE-DUTM    PIC  X(017) VALUE SPACES.
                   15  WRK-INP-SITUAC-DUTM    PIC  X(020) VALUE SPACES.
           05  WRK-INP-MENSA-DUTM             PIC  X(079) VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                       PIC  X(050)         VALUE
           '*** AREA DA TELA DCOMDUTM  - OUTPUT ***'.
      *----------------------------------------------------------------*

       01  WRK-OUTPUT-DUTM.
           05  WRK-OUT-LL-DUTM             PIC S9(004) COMP VALUE +0848.
           05  WRK-OUT-ZZ-DUTM             PIC  9(004) COMP VALUE ZEROS.
           05  WRK-OUT-SENHAS-DUTM            PIC  X(037) VALUE SPACES.
           05  WRK-OUT-COMANDO-DUTM           PIC  X(068) VALUE SPACES.
           05  WRK-OUT-DADOS-DUTM.
               10  WRK-OUT-FIMAMOS-DUTM       PIC  X(001) VALUE SPACES.
               10  WRK-OUT-PAGINA-DUTM-X.
                   15  WRK-OUT-PAGINA-DUTM    PIC  9(003) VALUE ZEROS.
               10  WRK-OUT-TPOOPC-DUTM        PIC  X(001) VALUE SPACES.
               10  WRK-OUT-TIPO-DUTM.
                   15  WRK-OUT-TIPO-DUTM-N    PIC  9(001) VALUE ZEROS.
               10  WRK-OUT-TMSTAMP-DUTM       PIC  X(026) VALUE SPACES.
               10  WRK-OUT-CLIENTE-DUTM       PIC  X(040) VALUE SPACES.
               10  WRK-OUT-CNPJ-X             PIC  X(009) VALUE SPACES.
               10  WRK-OUT-CNPJCPF-DUTM       PIC  X(019) VALUE SPACES.
               10  WRK-OUT-BANCO-X            PIC  X(006) VALUE SPACES.
               10  WRK-OUT-BANCO-DUTM-X.
                   15  WRK-OUT-BANCO-DUTM     PIC  9(003) VALUE ZEROS.
               10  WRK-OUT-AGENX-X            PIC  X(008) VALUE SPACES.
               10  WRK-OUT-CODAGE-DUTM-X.
                   15  WRK-OUT-CODAGE-DUTM    PIC  9(005) VALUE ZEROS.
               10  WRK-OUT-TRACO-X            PIC  X(001) VALUE SPACES.
               10  WRK-OUT-DESCAGE-DUTM       PIC  X(020) VALUE SPACES.
               10  WRK-OUT-CONTA-X            PIC  X(009) VALUE SPACES.
               10  WRK-OUT-CONTA-DUTM-X.
                   15  WRK-OUT-CONTA-DUTM     PIC  9(013) VALUE ZEROS.
               10  WRK-OUT-TABE-DUTM.
                 13 WRK-OUT-TABELA-DUTM    OCCURS  05  TIMES.
                    15 WRK-OUT-SELEC-ATTR-DUTM PIC S9(04) COMP.
                    15 WRK-OUT-SELEC-DUTM      PIC  X(001) VALUE SPACES.
                    15 WRK-OUT-CONTR-DUTM-X.
                       17  WRK-OUT-CONTR-DUTM  PIC  9(009) VALUE ZEROS.
                    15 WRK-OUT-NOME-DUTM       PIC  X(030) VALUE SPACES.
                    15 WRK-OUT-DTINIC-DUTM     PIC  X(010) VALUE SPACES.
                    15 WRK-OUT-DTVENCT-DUTM    PIC  X(010) VALUE SPACES.
                    15 WRK-OUT-LIMITE-DUTM-X.
                       17  WRK-OUT-LIMITE-DUTM PIC  ZZ.ZZZ.ZZZ.ZZ9,99.
                    15 WRK-OUT-SITUAC-DUTM     PIC  X(020) VALUE SPACES.
               10  WRK-OUT-MENSA-DUTM          PIC  X(079) VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA TELA DCOMDUTM  - BRAD0660 ***'.
      *----------------------------------------------------------------*

       01  WRK-660-DCOMDUTM.
           05  WRK-660-LL-AREA-DUTM     PIC  9(004) COMP   VALUE 0112.
           05  WRK-660-LL-MENSAGEM-DUTM PIC  9(004) COMP   VALUE 0848.
           05  WRK-660-SENHAS-DUTM      PIC  9(004) COMP   VALUE 0037.
           05  WRK-660-COMANDO-DUTM     PIC  9(004) COMP   VALUE 0068.
           05  WRK-660-FIMAMOS-DUTM     PIC  9(004) COMP   VALUE 0001.
           05  WRK-660-PAGINA-DUTM      PIC  9(004) COMP   VALUE 0003.
           05  WRK-660-TPOOPC-DUTM      PIC  9(004) COMP   VALUE 0001.
           05  WRK-660-TIPO-DUTM        PIC  9(004) COMP   VALUE 0001.
           05  WRK-660-TMSTAMP-DUTM     PIC  9(004) COMP   VALUE 0026.
           05  WRK-660-CLIENTE-DUTM     PIC  9(004) COMP   VALUE 0040.
           05  WRK-660-CNPJX-DUTM       PIC  9(004) COMP   VALUE 0009.
           05  WRK-660-CNPJCPF-DUTM     PIC  9(004) COMP   VALUE 0019.
           05  WRK-660-BANCOX-DUTM      PIC  9(004) COMP   VALUE 0006.
           05  WRK-660-BANCO-DUTM       PIC  9(004) COMP   VALUE 0003.
           05  WRK-660-AGENX-DUTM       PIC  9(004) COMP   VALUE 0008.
           05  WRK-660-CODAGE-DUTM      PIC  9(004) COMP   VALUE 0005.
           05  WRK-660-TRACO-DUTM       PIC  9(004) COMP   VALUE 0001.
           05  WRK-660-DESCAGE-DUTM     PIC  9(004) COMP   VALUE 0020.
           05  WRK-660-CONTAX-DUTM      PIC  9(004) COMP   VALUE 0009.
           05  WRK-660-CONTA-DUTM       PIC  9(004) COMP   VALUE 0013.
           05  WRK-660-TABELA-DUTM    OCCURS  05  TIMES.
               10  WRK-660-SELEC-DUTM   PIC  9(004) COMP   VALUE 2003.
               10  WRK-660-CONTR-DUTM   PIC  9(004) COMP   VALUE 0009.
               10  WRK-660-NOME-DUTM    PIC  9(004) COMP   VALUE 0030.
               10  WRK-660-DTINIC-DUTM  PIC  9(004) COMP   VALUE 0010.
               10  WRK-660-DTVENCT-DUTM PIC  9(004) COMP   VALUE 0010.
               10  WRK-660-LIMITE-DUTM  PIC  9(004) COMP   VALUE 0017.
               10  WRK-660-SITUAC-DUTM  PIC  9(004) COMP   VALUE 0020.
           05  WRK-660-MENSA-DUTM       PIC  9(004) COMP   VALUE 0079.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(080)         VALUE
           '*** AREA DE RECEBIMENTO E RETORNO DO DCOM0413 ***'.
      *----------------------------------------------------------------*

       01  COMU-AREA-413.
           05  COMU-LL-413            PIC S9(004) COMP    VALUE +184.
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
               10  COMU-CONTA-413     PIC  9(013)         VALUE ZEROS.
FS2511*        10  COMU-NUMERO-413    PIC  9(009)         VALUE ZEROS.
FS2511*        10  COMU-FILIAL-413    PIC  9(004)         VALUE ZEROS.
FS2511         10  COMU-NUMERO-413-ST PIC  X(009)         VALUE SPACES.
FS2511         10  COMU-FILIAL-413-ST PIC  X(004)         VALUE SPACES.
               10  COMU-CONTROLE-413  PIC  9(002)         VALUE ZEROS.

       01  COMU-AREA.
           05  COMU-LL                 PIC S9(004) COMP  VALUE +272.
           05  COMU-ZZ                 PIC S9(004) COMP  VALUE +0.
           05  COMU-TRANCODE.
               10  COMU-TRANSACAO      PIC  X(008)       VALUE SPACES.
               10  FILLER              PIC  X(006)       VALUE SPACES.
               10  COMU-PFK            PIC  X(001)       VALUE SPACES.
           05  COMU-SENHAS             PIC  X(037)       VALUE SPACES.
           05  COMU-COMANDO            PIC  X(068)       VALUE SPACES.
           05  COMU-DADOS.
               10  COMU-OPCAO          PIC  9(001)       VALUE ZEROS.
               10  COMU-TIMESTAMP      PIC  X(026)       VALUE SPACES.
               10  COMU-CONTRATO       PIC  9(009)       VALUE ZEROS.
               10  COMU-AGENCIA        PIC  9(005)       VALUE ZEROS.
               10  COMU-CONTA          PIC  9(013)       VALUE ZEROS.
FS2511*        10  COMU-CPF-X.
FS2511*            15  COMU-CPF        PIC  9(009)       VALUE ZEROS.
FS2511         10  COMU-CPF-X-ST.
FS2511             15  COMU-CPF-ST     PIC  X(009)       VALUE SPACES.
FS2511*        10  COMU-FILIAL         PIC  9(004)       VALUE ZEROS.
FS2511         10  COMU-FILIAL-ST      PIC  X(004)       VALUE SPACES.
               10  COMU-DIGITO         PIC  9(002)       VALUE ZEROS.
               10  COMU-MENSAG         PIC  X(079)       VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(080)         VALUE
           '*** AREA DE ENVIO PARA  DCOM0422  ***'.
      *----------------------------------------------------------------*

       01  COMU-AREA-422.
           05  COMU-LL-422            PIC S9(004) COMP    VALUE +193.
           05  COMU-ZZ-422            PIC S9(004) COMP    VALUE ZEROS.
           05  COMU-TRANCODE-422.
               10  COMU-TRANSACAO-422 PIC  X(008)         VALUE SPACES.
               10  FILLER             PIC  X(007)         VALUE SPACES.
           05  COMU-SENHAS-422        PIC  X(037)         VALUE SPACES.
           05  COMU-COMANDO-422       PIC  X(068)         VALUE SPACES.
           05  COMU-TIPO-422          PIC  9(001)         VALUE ZEROS.
           05  COMU-TMSTAMP-422       PIC  X(026)         VALUE SPACES.
           05  COMU-CONTRATO-422      PIC  9(009)         VALUE ZEROS.
           05  COMU-CODAGE-422        PIC  9(005)         VALUE ZEROS.
           05  COMU-CONTA-422         PIC  9(013)         VALUE ZEROS.
FS2511*    05  COMU-NUMERO-422        PIC  9(009)         VALUE ZEROS.
FS2511*    05  COMU-FILIAL-422        PIC  9(004)         VALUE ZEROS.
FS2511     05  COMU-NUMERO-422-ST     PIC  X(009)         VALUE SPACES.
FS2511     05  COMU-FILIAL-422-ST     PIC  X(004)         VALUE SPACES.
           05  COMU-CONTROLE-422      PIC  9(002)         VALUE ZEROS.

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
           '*** AREA DE COMUNICACAO COM O MODULO DCOM4111 **'.
      *----------------------------------------------------------------*
       01  WRK-4111E-ENTRADA.
           03  WRK-4111E-HEADER.
               05  WRK-4111E-COD-RETORNO             PIC  X(04).
               05  WRK-4111E-MSG-RETORNO             PIC  X(79).
               05  WRK-4111E-RESTART                 PIC  9(05).
               05  WRK-4111E-FLAG                    PIC  X(01).
               05  WRK-4111E-START-COUNT             PIC  X(01).
               05  WRK-4111E-CFUNC-BDSCO             PIC  9(09).
               05  WRK-4111E-CTERM                   PIC  X(08).
           03  WRK-4111E-DADOS.
               05  WRK-4111E-CAG-BCRIA               PIC  9(05).
               05  WRK-4111E-CCTA-BCRIA              PIC  9(13).
               05  WRK-4111E-FLAG-FILTRO             PIC  X(01).

       01  WRK-4111S-SAIDA.
           03  WRK-4111S-HEADER.
               05  WRK-4111S-COD-RETORNO            PIC  X(04).
               05  WRK-4111S-MSG-RETORNO            PIC  X(79).
               05  WRK-4111S-RESTART                PIC  9(05).
               05  WRK-4111S-FLAG                   PIC  X(01).
               05  WRK-4111S-QTDE-RETORNADA         PIC  9(03).
               05  WRK-4111S-COUNT                  PIC  9(09).
           03  WRK-4111S-DADOS.
               05  WRK-4111S-TABELA-SAIDA    OCCURS 20 TIMES.
                   10  WRK-4111S-CCONTR-LIM-DESC    PIC  9(09).
                   10  WRK-4111S-VERSAO-CCONTR-LIM  PIC  9(03).
FS2511*            10  WRK-4111S-CCNPJ-CPF          PIC  9(09).
FS2511*            10  WRK-4111S-CFLIAL-CNPJ        PIC  9(04).
FS2511             10  WRK-4111S-CCNPJ-CPF-ST       PIC  X(09).
FS2511             10  WRK-4111S-CFLIAL-CNPJ-ST     PIC  X(04).
                   10  WRK-4111S-CCTRL-CNPJ-CPF     PIC  9(02).
                   10  WRK-4111S-NM-CLIE            PIC  X(25).
                   10  WRK-4111S-DINIC-VGCIA-CONTR  PIC  X(10).
                   10  WRK-4111S-DVCTO-CONTR-LIM    PIC  X(10).
                   10  WRK-4111S-VCONTR-LIM         PIC  9(15)V99.
                   10  WRK-4111S-IRSUMO-SIT-DESC    PIC  X(15).
           03  WRK-4111S-CONSISTENCIA   REDEFINES   WRK-4111S-DADOS.
               05  WRK-4111S-TAMANHO-ERRO           PIC  9(01).
               05  WRK-4111S-TABELA-ERRO   OCCURS   2    TIMES.
                   10  WRK-4111S-CAMPOS-ERRO        PIC  9(01).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO COM O MODULO DCOM4112 **'.
      *----------------------------------------------------------------*

       01  WRK-4112E-ENTRADA.
           03  WRK-4112E-HEADER.
               05  WRK-4112E-COD-RETORNO             PIC  X(04).
               05  WRK-4112E-MSG-RETORNO             PIC  X(79).
               05  WRK-4112E-RESTART                 PIC  9(05).
               05  WRK-4112E-FLAG                    PIC  X(01).
               05  WRK-4112E-START-COUNT             PIC  X(01).
               05  WRK-4112E-CFUNC-BDSCO             PIC  9(09).
               05  WRK-4112E-CTERM                   PIC  X(08).
           03  WRK-4112E-DADOS.
FS2511*        05  WRK-4112E-CCNPJ-CPF               PIC  9(09).
FS2511*        05  WRK-4112E-CFLIAL-CNPJ             PIC  9(04).
FS2511         05  WRK-4112E-CCNPJ-CPF-ST            PIC  X(09).
FS2511         05  WRK-4112E-CFLIAL-CNPJ-ST          PIC  X(04).
               05  WRK-4112E-CCTRL-CNPJ-CPF          PIC  9(02).
               05  WRK-4112E-FLAG-FILTRO             PIC  X(01).

       01  WRK-4112S-SAIDA.
           03  WRK-4112S-HEADER.
               05  WRK-4112S-COD-RETORNO            PIC  X(04).
               05  WRK-4112S-MSG-RETORNO            PIC  X(79).
               05  WRK-4112S-RESTART                PIC  9(05).
               05  WRK-4112S-FLAG                   PIC  X(01).
               05  WRK-4112S-QTDE-RETORNADA         PIC  9(03).
               05  WRK-4112S-COUNT                  PIC  9(09).
           03  WRK-4112S-DADOS.
               05  WRK-4112S-TABELA-SAIDA    OCCURS 30 TIMES.
                   10  WRK-4112S-NM-CLIE            PIC  X(25).
                   10  WRK-4112S-CCONTR-LIM-DESC    PIC  9(09).
                   10  WRK-4112S-CVRSAO-CONTR-LIM   PIC  9(03).
                   10  WRK-4112S-CAG-BCRIA          PIC  9(05).
                   10  WRK-4112S-CCTA-BCRIA         PIC  9(13).
                   10  WRK-4112S-DINIC-VGCIA-CONTR  PIC  X(10).
                   10  WRK-4112S-DVCTO-CONTR-LIM    PIC  X(10).
                   10  WRK-4112S-VCONTR-LIM         PIC  9(15)V99.
                   10  WRK-4112S-IRSUMO-SIT-DESC    PIC  X(15).
           03  WRK-4112S-CONSISTENCIA   REDEFINES   WRK-4112S-DADOS.
               05  WRK-4112S-TAMANHO-ERRO           PIC  9(01).
               05  WRK-4112S-TABELA-ERRO   OCCURS   1    TIMES.
                   10  WRK-4112S-CAMPOS-ERRO        PIC  9(01).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA MENSAGENS ***'.
      *----------------------------------------------------------------*

       COPY I#DCOMSY.

      *----------------------------------------------------------------*
      * I#DCOMX3                                                       *
      * AREA DE COMUNICACAO COM O  MODULO  DCOMXXXX P/ SELECAO DE      *
      *         CONTAS E  PESSOAS CADASTRADAS NO CADU                  *
      *----------------------------------------------------------------*
       COPY I#DCOMX3.

       01  ROTSAI-6408-ERRO-AREA       PIC  X(107).
       01  ROTSAI-6408-SQLCA           PIC  X(136).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA BRAD7100 ***'.
      *----------------------------------------------------------------*

       COPY I#BRAD7C.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** DCOM0421 - FIM DA AREA DE WORKING ***'.
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

           MOVE SPACES                 TO  WRK-INP-DADOS-DUTM
                                           WRK-OUT-DADOS-DUTM
                                           WRK-OUT-SENHAS-DUTM
                                           WRK-OUT-COMANDO-DUTM
                                           WRK-6416E-ENTRADA
                                           WRK-6416S-SAIDA
                                           WRK-5143E-ENTRADA
                                           WRK-5143S-SAIDA
                                           WRK-4111E-ENTRADA
                                           WRK-4111S-SAIDA
                                           WRK-4112E-ENTRADA
                                           WRK-4112S-SAIDA
                                           WRK-AUXILIARES.

           PERFORM 1000-PROCESSAR-MENSAGEM.

           GO TO 0000-INICIAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-PROCESSAR-MENSAGEM         SECTION.
      *----------------------------------------------------------------*

           MOVE 'DCOMDUTM'             TO  WRK-TELA.

           IF  WRK-TRANSACAO    NOT EQUAL  'DCOM0421'
               PERFORM 1100-RECEBER-CONTROLE-DCOM0413
           ELSE
               PERFORM 1200-PROCESSAR-DCOMDUTM
           END-IF.

           IF  WRK-TELA             EQUAL  'DCOMDUTM'
               PERFORM 1300-PROCESSAR-BRAD0660
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1100-RECEBER-CONTROLE-DCOM0413  SECTION.
      *----------------------------------------------------------------*

           IF  WRK-TRANSACAO        EQUAL  'DCOM0413'
               MOVE WRK-MENSAGEM           TO  COMU-AREA-413
               MOVE COMU-OPCAO-413         TO  WRK-INP-TPOOPC-DUTM
                                               WRK-OUT-TPOOPC-DUTM
               MOVE COMU-TMSTAMP-413       TO  WRK-INP-TMSTAMP-DUTM
               IF   COMU-CODAGE-413        GREATER ZEROS
                    MOVE 1                 TO  WRK-INP-TIPO-DUTM
                                               WRK-OUT-TIPO-DUTM
               END-IF
           ELSE
            IF WRK-TRANSACAO        EQUAL  'DCOM0422'
               MOVE WRK-MENSAGEM           TO  COMU-AREA-422
               MOVE COMU-TMSTAMP-422       TO  WRK-INP-TMSTAMP-DUTM
                                               WRK-OUT-TMSTAMP-DUTM
               IF   COMU-TIPO-422          EQUAL 1
                    MOVE COMU-CODAGE-422        TO  COMU-CODAGE-413
                    MOVE COMU-CONTA-422         TO  COMU-CONTA-413
                    MOVE ZEROS                  TO  COMU-NUMERO-413
                                                    COMU-FILIAL-413
                                                    COMU-CONTROLE-413
               ELSE
                    MOVE COMU-NUMERO-422        TO  COMU-NUMERO-413
                    MOVE COMU-FILIAL-422        TO  COMU-FILIAL-413
                    MOVE COMU-CONTROLE-422      TO  COMU-CONTROLE-413
                    MOVE ZEROS                  TO  COMU-CODAGE-413
                                                    COMU-CONTA-413
               END-IF
            END-IF
           END-IF.

           MOVE ZEROS                           TO  WRK-4111E-RESTART
           PERFORM  1110-MONTA-CABECALHO.

           MOVE 1                               TO  WRK-INP-PAGINA-DUTM
                                                    WRK-OUT-PAGINA-DUTM.

           MOVE 'SELECIONE UM REGISTRO COM "D" E TECLE <ENTER>'
                                                 TO WRK-OUT-MENSA-DUTM.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1110-MONTA-CABECALHO            SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                           ROTENT-6408-ENVIO.

           MOVE   ZEROS                      TO ROTENT-6408-COD-ENV
                                                ROTENT-6408-REST-ENV
           MOVE   SPACES                     TO ROTENT-6408-MSG-ENV
           MOVE   'N'                        TO ROTENT-6408-FLAG-ENV
                                                ROTENT-6408-START-ENV
           MOVE  WRK-COD-USER                TO ROTENT-6408-CFUNC-ENV
           MOVE  LNK-IO-LTERM                TO ROTENT-6408-CTERM-ENV

           IF  COMU-CODAGE-413               GREATER ZEROS
               MOVE  'BANCO:'                TO WRK-INP-BANCO-X
                                                WRK-OUT-BANCO-X
               MOVE  237                     TO WRK-INP-BANCO-DUTM
                                                WRK-OUT-BANCO-DUTM
               MOVE  'AGENCIA:'              TO WRK-INP-AGENX-X
                                                WRK-OUT-AGENX-X
               MOVE  COMU-CODAGE-413         TO ROTENT-6408-CJUNC-ENV
                                                WRK-INP-CODAGE-DUTM-X
                                                WRK-OUT-CODAGE-DUTM-X
               MOVE  '-'                     TO WRK-INP-TRACO-X
                                                WRK-OUT-TRACO-X
               PERFORM  1111-OBTER-DESC-AGENC
               MOVE  'CONTA..:'              TO WRK-INP-CONTA-X
                                                WRK-OUT-CONTA-X
               MOVE  COMU-CONTA-413          TO ROTENT-6408-CCTAC-ENV
                                                WRK-INP-CONTA-DUTM-X
                                                WRK-OUT-CONTA-DUTM-X
                                                WRK-CONTA-0431
               PERFORM 1112-CALCULAR-DIG
               MOVE  WRK-DIGITO-0431-R       TO ROTENT-6408-DCTAC-ENV
               MOVE  ZEROS                   TO ROTENT-6408-NUM-CNPJ-ENV
                                                ROTENT-6408-FIL-CNPJ-ENV
                                                ROTENT-6408-CTR-CNPJ-ENV
               PERFORM 1120-ACESSAR-DCOM4111

           ELSE
FS2511*      MOVE COMU-NUMERO-413          TO ROTENT-6408-NUM-CNPJ-ENV
FS2511*                                       WRK-CNPJ-N
FS2511       MOVE COMU-NUMERO-413-ST       TO ROTENT-6408-NUM-CNPJ-ENV
FS2511       MOVE COMU-NUMERO-413-ST       TO WRK-CNPJ-N-ST
FS2511*      MOVE COMU-FILIAL-413          TO ROTENT-6408-FIL-CNPJ-ENV
FS2511*                                       WRK-FILIAL-N
FS2511       MOVE COMU-FILIAL-413-ST       TO ROTENT-6408-FIL-CNPJ-ENV
FS2511       MOVE COMU-FILIAL-413-ST       TO WRK-FILIAL-N-ST
             MOVE COMU-CONTROLE-413        TO ROTENT-6408-CTR-CNPJ-ENV
                                                WRK-CONTROLE-N-ST
               MOVE 'CNPJ/CPF:'              TO WRK-INP-CNPJ-X
                                                WRK-OUT-CNPJ-X
               MOVE WRK-CNPJCPF-COMPL        TO WRK-INP-CNPJCPF-DUTM
                                                WRK-OUT-CNPJCPF-DUTM
               MOVE ZEROS                    TO ROTENT-6408-CJUNC-ENV
                                                ROTENT-6408-CCTAC-ENV
                                                ROTENT-6408-DCTAC-ENV
               PERFORM 1130-ACESSAR-DCOM4112
           END-IF.

           IF  WRK-FLAG-MENSA          EQUAL 'S'
               GO TO 1110-99-FIM
           END-IF

           MOVE  'DCOM6408'            TO WRK-MODULO.

           CALL  WRK-MODULO  USING     ROTENT-6408-AREA
                                       ROTSAI-6408-RETORNO
                                       ERRO-AREA
                                       WRK-SQLCA.

           EVALUATE ROTSAI-6408-COD-RET
               WHEN '0000'
                   MOVE ROTSAI-6408-NM-RZSOC-RET(1)
                                       TO WRK-INP-CLIENTE-DUTM
                                          WRK-OUT-CLIENTE-DUTM

               WHEN '0003'
                   INITIALIZE COMU-AREA
                   MOVE SG-MENSAGEM(0152)    TO COMU-MENSAG
                   PERFORM 1115-RETORNA-CHAMADOR-DCOM0413

               WHEN '0099'
                   MOVE 'DB2'          TO ERR-TIPO-ACESSO
                   MOVE '0070'         TO ERR-LOCAL
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                   INITIALIZE COMU-AREA
                   MOVE ROTSAI-6408-MENS-RET TO COMU-MENSAG
                   PERFORM 1115-RETORNA-CHAMADOR-DCOM0413
           END-EVALUATE.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1111-OBTER-DESC-AGENC           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-COD-USER           TO  WRK-6416E-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO  WRK-6416E-CTERM
           MOVE 237                    TO  WRK-6416E-BANCO
           MOVE COMU-CODAGE-413        TO  WRK-6416E-DEPDC
           MOVE 'DCOM6416'             TO  WRK-MODULO

           CALL WRK-MODULO    USING    WRK-6416E-ENTRADA
                                       WRK-6416S-SAIDA
                                       ERRO-AREA
                                       WRK-SQLCA.

           EVALUATE WRK-6416S-COD-RETORNO
               WHEN '0000'
                    MOVE WRK-6416S-NOME-DEPDC   TO WRK-INP-DESCAGE-DUTM
                                                   WRK-OUT-DESCAGE-DUTM

               WHEN '0003'
                    MOVE  ALL '*'               TO WRK-INP-DESCAGE-DUTM
                                                   WRK-OUT-DESCAGE-DUTM

               WHEN '0099'
                    MOVE  'DB2'     TO     ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                    MOVE  WRK-6416S-COD-RETORNO
                                        TO WRK-OUT-MENSA-DUTM(1:4)
                    MOVE  '-'           TO WRK-OUT-MENSA-DUTM(6:1)
                    MOVE  WRK-6416S-MSG-RETORNO
                                        TO WRK-OUT-MENSA-DUTM(8:72)
           END-EVALUATE.

      *----------------------------------------------------------------*
       1111-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1112-CALCULAR-DIG               SECTION.
      *----------------------------------------------------------------*

           CALL 'BRAD0431'   USING WRK-CONTA-0431
                                   WRK-DIGITO-0431
                                   WRK-TAMANHO-0431.

           IF  WRK-DIGITO-0431     EQUAL  '.'
               MOVE 'APL'          TO ERR-TIPO-ACESSO
               MOVE 'ERRO NO CALCULO DO DIGITO - BRAD0431 - CONTA = '
                                   TO ERR-TEXTO(1:47)
               MOVE WRK-CONTA-0431      TO WRK-CONTA-0431-R
               MOVE WRK-CONTA-0431-R    TO ERR-TEXTO(48:13)
                    MOVE ' 1112-CALCULAR-DIGITO -ERRO'
                                        TO WRK-OUT-MENSA-DUTM
           ELSE
               IF   WRK-DIGITO-0431     EQUAL  'P'
                    MOVE ZEROS          TO WRK-DIGITO-0431-R
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1112-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1115-RETORNA-CHAMADOR-DCOM0413  SECTION.
      *----------------------------------------------------------------*

           MOVE LENGTH COMU-AREA     TO COMU-LL
           MOVE WRK-CHNG             TO WRK-FUNCAO
           MOVE 'DCOM0413'           TO WRK-TELA
           MOVE 'DCOM0421'           TO COMU-TRANSACAO
           MOVE WRK-INP-TPOOPC-DUTM  TO COMU-OPCAO
           MOVE WRK-INP-TMSTAMP-DUTM TO COMU-TIMESTAMP
           MOVE COMU-AREA            TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1115-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1120-ACESSAR-DCOM4111           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-COD-USER           TO  WRK-4111E-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO  WRK-4111E-CTERM
           MOVE 'M'                    TO  WRK-4111E-FLAG-FILTRO
           MOVE 'S'                    TO  WRK-4111E-START-COUNT
           MOVE SPACES                 TO  WRK-4111E-FLAG
           MOVE WRK-INP-CODAGE-DUTM    TO  WRK-4111E-CAG-BCRIA
           MOVE WRK-INP-CONTA-DUTM     TO  WRK-4111E-CCTA-BCRIA

           MOVE 'DCOM4111'             TO  WRK-MODULO

           CALL WRK-MODULO    USING    WRK-4111E-ENTRADA
                                       WRK-4111S-SAIDA
                                       ERRO-AREA
                                       WRK-SQLCA.

           EVALUATE WRK-4111S-COD-RETORNO
               WHEN '0000'
                    IF (WRK-4111S-CCONTR-LIM-DESC(6)  NOT NUMERIC) OR
                       (WRK-4111S-CCONTR-LIM-DESC(6)  EQUAL ZEROS)
                       MOVE '*'     TO  WRK-OUT-FIMAMOS-DUTM
                       MOVE 'FINAL DE AMOSTRAGEM'
                                    TO  WRK-OUT-MENSA-DUTM
                    ELSE
                       MOVE SPACES  TO  WRK-OUT-FIMAMOS-DUTM
                       MOVE 'AMOSTRAGEM CONTINUA'
                                    TO  WRK-OUT-MENSA-DUTM
                    END-IF
                    PERFORM 1121-MONTAR-TELA VARYING WRK-IND FROM
                            1 BY 1 UNTIL WRK-IND GREATER 5

               WHEN '0003'
                    INITIALIZE    COMU-AREA
                    MOVE  SG-MENSAGEM(0152)       TO COMU-MENSAG
                    PERFORM 1115-RETORNA-CHAMADOR-DCOM0413
                    MOVE 'S'                      TO WRK-FLAG-MENSA

               WHEN '0099'
                    MOVE  'DB2'     TO     ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                    INITIALIZE    COMU-AREA
                    MOVE  WRK-4111S-MSG-RETORNO   TO COMU-MENSAG
                    MOVE  WRK-INP-CODAGE-DUTM     TO COMU-AGENCIA
                    MOVE  WRK-INP-CONTA-DUTM      TO COMU-CONTA
                    PERFORM 1115-RETORNA-CHAMADOR-DCOM0413
                    MOVE 'S'                      TO WRK-FLAG-MENSA
           END-EVALUATE.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1121-MONTAR-TELA                SECTION.
      *----------------------------------------------------------------*

           IF (WRK-4111S-CCONTR-LIM-DESC(WRK-IND) IS NUMERIC) AND
              (WRK-4111S-CCONTR-LIM-DESC(WRK-IND) GREATER ZEROS)
              MOVE WRK-4111S-CCONTR-LIM-DESC(WRK-IND)
                                  TO WRK-OUT-CONTR-DUTM(WRK-IND)
              MOVE WRK-4111S-NM-CLIE(WRK-IND)
                                  TO WRK-OUT-NOME-DUTM(WRK-IND)
              MOVE WRK-4111S-DINIC-VGCIA-CONTR(WRK-IND)
                                  TO WRK-OUT-DTINIC-DUTM(WRK-IND)
              INSPECT WRK-OUT-DTINIC-DUTM(WRK-IND) REPLACING ALL
                                                   '.' BY '/'
              MOVE WRK-4111S-DVCTO-CONTR-LIM(WRK-IND)
                                  TO WRK-OUT-DTVENCT-DUTM(WRK-IND)
              INSPECT WRK-OUT-DTVENCT-DUTM(WRK-IND) REPLACING ALL
                                                    '.' BY '/'
              MOVE WRK-4111S-VCONTR-LIM(WRK-IND)
                                  TO WRK-OUT-LIMITE-DUTM(WRK-IND)
              MOVE WRK-4111S-IRSUMO-SIT-DESC(WRK-IND)
                                  TO WRK-OUT-SITUAC-DUTM(WRK-IND)
           ELSE
              MOVE WRK-241        TO WRK-OUT-SELEC-ATTR-DUTM(WRK-IND)
           END-IF.

      *----------------------------------------------------------------*
       1121-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1130-ACESSAR-DCOM4112           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-COD-DEPTO-N        TO  WRK-4112E-MSG-RETORNO
           MOVE WRK-COD-USER           TO  WRK-4112E-CFUNC-BDSCO.
           MOVE LNK-IO-LTERM           TO  WRK-4112E-CTERM.
           MOVE WRK-INP-CNPJCPF-DUTM(01:09) TO WRK-CNPJ-N-ST.
           MOVE WRK-INP-CNPJCPF-DUTM(11:04) TO WRK-FILIAL-N-ST.
           MOVE WRK-INP-CNPJCPF-DUTM(16:02) TO WRK-CONTROLE-N-ST.

FS2511*    MOVE WRK-CNPJ-N             TO  WRK-4112E-CCNPJ-CPF.
FS2511     MOVE WRK-CNPJ-N-ST          TO  WRK-4112E-CCNPJ-CPF-ST.

FS2511*    MOVE WRK-FILIAL-N           TO  WRK-4112E-CFLIAL-CNPJ.
FS2511     MOVE WRK-FILIAL-N-ST        TO  WRK-4112E-CFLIAL-CNPJ-ST.
           MOVE WRK-CONTROLE           TO  WRK-4112E-CCTRL-CNPJ-CPF
           MOVE WRK-COD-DEPTO-N        TO  WRK-AGENCIA-NUM.
           MOVE WRK-AGENCIA-AUX        TO  WRK-4112E-MSG-RETORNO(1:5).
           MOVE 'M'                    TO  WRK-4112E-FLAG-FILTRO

           MOVE 'DCOM4112'             TO  WRK-MODULO.

           CALL WRK-MODULO    USING    WRK-4112E-ENTRADA
                                       WRK-4112S-SAIDA
                                       ERRO-AREA
                                       WRK-SQLCA.

           EVALUATE WRK-4112S-COD-RETORNO
               WHEN '0000'
                    IF (WRK-4112S-CCONTR-LIM-DESC(6)  NOT NUMERIC) OR
                       (WRK-4112S-CCONTR-LIM-DESC(6)  EQUAL ZEROS)
                       MOVE '*'     TO  WRK-OUT-FIMAMOS-DUTM
                       MOVE 'FINAL DE AMOSTRAGEM'
                                    TO  WRK-OUT-MENSA-DUTM
                    ELSE
                       MOVE SPACES  TO  WRK-OUT-FIMAMOS-DUTM
                       MOVE 'AMOSTRAGEM CONTINUA'
                                    TO  WRK-OUT-MENSA-DUTM
                    END-IF
                    PERFORM 1135-MONTAR-TELA-CNPJCPF

               WHEN '0003'
                   INITIALIZE    COMU-AREA
                   MOVE  SG-MENSAGEM(0152)       TO COMU-MENSAG
                   PERFORM 1115-RETORNA-CHAMADOR-DCOM0413
                   MOVE 'S'                      TO WRK-FLAG-MENSA

               WHEN '0099'
                    MOVE  'DB2'     TO     ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO


               WHEN '0915'
                   INITIALIZE    COMU-AREA
                   MOVE  WRK-4112S-MSG-RETORNO     TO COMU-MENSAG

                   IF  WRK-INP-TIPO-DUTM    EQUAL  1
                       MOVE WRK-INP-CODAGE-DUTM    TO COMU-AGENCIA
                       MOVE WRK-INP-CONTA-DUTM     TO COMU-CONTA
                   ELSE
                       MOVE WRK-INP-CNPJCPF-DUTM(1:3)
                                                   TO COMU-CPF-X(1:3)
                       MOVE WRK-INP-CNPJCPF-DUTM(5:3)
                                                   TO COMU-CPF-X(4:3)
                       MOVE WRK-INP-CNPJCPF-DUTM(9:3)
                                                   TO COMU-CPF-X(7:3)
                       MOVE WRK-INP-FILIAL-DUTM    TO COMU-FILIAL
                       MOVE WRK-INP-CONTROLE-DUTM  TO COMU-DIGITO
                   END-IF

                   PERFORM 1115-RETORNA-CHAMADOR-DCOM0413
                   MOVE 'S'                      TO WRK-FLAG-MENSA

               WHEN OTHER
                   INITIALIZE    COMU-AREA
                   MOVE  WRK-4112S-MSG-RETORNO      TO COMU-MENSAG
                   MOVE  WRK-4112E-CCNPJ-CPF        TO COMU-CPF
                   MOVE  WRK-4112E-CFLIAL-CNPJ      TO COMU-FILIAL
                   MOVE  WRK-4112E-CCTRL-CNPJ-CPF   TO COMU-DIGITO
                   PERFORM 1115-RETORNA-CHAMADOR-DCOM0413
                   MOVE 'S'                         TO WRK-FLAG-MENSA

           END-EVALUATE.


      *----------------------------------------------------------------*
       1130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1135-MONTAR-TELA-CNPJCPF        SECTION.
      *----------------------------------------------------------------*

           PERFORM  VARYING  WRK-IND  FROM  1 BY 1
                      UNTIL  WRK-IND  GREATER 05

               MOVE SPACE              TO WRK-OUT-SELEC-DUTM(WRK-IND)
               IF  WRK-4112S-CCONTR-LIM-DESC(WRK-IND) IS NUMERIC AND
                   WRK-4112S-CCONTR-LIM-DESC(WRK-IND) GREATER ZERO
                   MOVE SPACES    TO WRK-OUT-SELEC-DUTM(WRK-IND)
                   MOVE WRK-4112S-CCONTR-LIM-DESC(WRK-IND)
                                  TO WRK-OUT-CONTR-DUTM(WRK-IND)
                   MOVE WRK-4112S-NM-CLIE(WRK-IND)
                                  TO WRK-OUT-NOME-DUTM(WRK-IND)
                   MOVE WRK-4112S-DINIC-VGCIA-CONTR(WRK-IND)
                                  TO WRK-OUT-DTINIC-DUTM(WRK-IND)
                   MOVE WRK-4112S-DVCTO-CONTR-LIM(WRK-IND)
                                  TO WRK-OUT-DTVENCT-DUTM(WRK-IND)
                   MOVE WRK-4112S-VCONTR-LIM(WRK-IND)
                                  TO WRK-OUT-LIMITE-DUTM(WRK-IND)
                   MOVE WRK-4112S-IRSUMO-SIT-DESC(WRK-IND)
                                  TO WRK-OUT-SITUAC-DUTM(WRK-IND)
               ELSE
                   MOVE  SPACES   TO  WRK-OUT-TABELA-DUTM(WRK-IND)
               END-IF
           END-PERFORM.

           PERFORM  1137-PROTEGER-SELECAO.

           IF  WRK-4112S-CCONTR-LIM-DESC(6) IS NUMERIC AND
               WRK-4112S-CCONTR-LIM-DESC(6) GREATER ZERO
               MOVE  'AMOSTRAGEM CONTINUA' TO WRK-OUT-MENSA-DUTM
           ELSE
               MOVE 'FIM DA AMOSTRAGEM'    TO WRK-OUT-MENSA-DUTM
               MOVE '*'                    TO WRK-OUT-FIMAMOS-DUTM
               PERFORM  1137-PROTEGER-SELECAO
           END-IF.

      *----------------------------------------------------------------*
       1135-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1137-PROTEGER-SELECAO           SECTION.
      *----------------------------------------------------------------*

           PERFORM  VARYING  WRK-IND  FROM  1 BY 1
                      UNTIL  WRK-IND  GREATER 05
               IF  WRK-OUT-CONTR-DUTM(WRK-IND)  NOT NUMERIC
                   MOVE WRK-241    TO WRK-OUT-SELEC-ATTR-DUTM(WRK-IND)
               END-IF
           END-PERFORM.

      *----------------------------------------------------------------*
       1137-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1200-PROCESSAR-DCOMDUTM         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM             TO WRK-INPUT-DUTM.
           PERFORM 1210-DEVOLVER-TELA

           EVALUATE TRUE
               WHEN WRK-INP-PFK-DUTM     EQUAL 'H' OR
                   (WRK-INP-PFK-DUTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DUTM EQUAL 'PFK1')
                    MOVE WRK-INP-MENSA-DUTM TO WRK-OUT-MENSA-DUTM

               WHEN WRK-INP-PFK-DUTM     EQUAL '3' OR
                   (WRK-INP-PFK-DUTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DUTM EQUAL 'PFK3')
                    PERFORM 1220-RETORNAR-CHAMADOR

               WHEN WRK-INP-PFK-DUTM     EQUAL 'X' OR
                   (WRK-INP-PFK-DUTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DUTM EQUAL 'PFK5')
                    PERFORM 1230-RETORNAR-MENU-ROTINAS

               WHEN WRK-INP-PFK-DUTM     EQUAL 'Z' OR
                   (WRK-INP-PFK-DUTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DUTM EQUAL 'PFK10')
                    PERFORM 1240-RETORNAR-MENU-DCOM

               WHEN WRK-INP-PFK-DUTM     EQUAL '7' OR
                    (WRK-INP-PFK-DUTM    EQUAL '.' AND
                    WRK-INP-COMANDO-DUTM EQUAL 'PFK7')
                    IF (WRK-INP-CONTR-DUTM(1) NOT NUMERIC) OR
                       (WRK-INP-CONTR-DUTM(1) EQUAL ZEROS)
                       MOVE 'NAO HA OCORRENCIA A SEREM MOSTRADAS'
                                              TO WRK-OUT-MENSA-DUTM
                   ELSE
                       PERFORM 1260-TRATAR-VOLTA-PAG
                   END-IF

               WHEN WRK-INP-PFK-DUTM     EQUAL '8' OR
                    (WRK-INP-PFK-DUTM    EQUAL '.' AND
                    WRK-INP-COMANDO-DUTM EQUAL 'PFK8')
                    IF (WRK-INP-CONTR-DUTM(1) NOT NUMERIC) OR
                       (WRK-INP-CONTR-DUTM(1) EQUAL ZEROS)
                       MOVE 'NAO HA OCORRENCIA A SEREM MOSTRADAS'
                                              TO WRK-OUT-MENSA-DUTM
                   ELSE
                       PERFORM 1270-TRATAR-AVANCA-PAG
                   END-IF

               WHEN WRK-INP-PFK-DUTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DUTM EQUAL  SPACES OR LOW-VALUES
                    IF (WRK-INP-CONTR-DUTM(1) NOT NUMERIC) OR
                       (WRK-INP-CONTR-DUTM(1) EQUAL ZEROS)
                       MOVE 'NAO HA OCORRENCIA A SEREM MOSTRADAS'
                                              TO WRK-OUT-MENSA-DUTM
                   ELSE
                       PERFORM 1280-TRATAR-ENTER
                   END-IF

               WHEN OTHER
                    MOVE 'PF INVALIDA'   TO WRK-OUT-MENSA-DUTM
           END-EVALUATE.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1210-DEVOLVER-TELA              SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-FIMAMOS-DUTM   TO WRK-OUT-FIMAMOS-DUTM
           MOVE WRK-INP-PAGINA-DUTM-X  TO WRK-OUT-PAGINA-DUTM
           MOVE WRK-INP-TPOOPC-DUTM    TO WRK-OUT-TPOOPC-DUTM
           MOVE WRK-INP-TIPO-DUTM      TO WRK-OUT-TIPO-DUTM
           MOVE WRK-INP-TMSTAMP-DUTM   TO WRK-OUT-TMSTAMP-DUTM
           MOVE WRK-INP-CLIENTE-DUTM   TO WRK-OUT-CLIENTE-DUTM
           MOVE WRK-INP-CNPJ-X         TO WRK-OUT-CNPJ-X
FS2511*    MOVE WRK-INP-CNPJCPF-DUTM   TO WRK-OUT-CNPJCPF-DUTM
FS2511     MOVE WRK-INP-CNPJCPF-DUTM-ST TO WRK-OUT-CNPJCPF-DUTM
           MOVE WRK-INP-BANCO-X        TO WRK-OUT-BANCO-X
           MOVE WRK-INP-BANCO-DUTM-X   TO WRK-OUT-BANCO-DUTM-X
           MOVE WRK-INP-AGENX-X        TO WRK-OUT-AGENX-X
           MOVE WRK-INP-CODAGE-DUTM-X  TO WRK-OUT-CODAGE-DUTM-X
           MOVE WRK-INP-TRACO-X        TO WRK-OUT-TRACO-X
           MOVE WRK-INP-DESCAGE-DUTM   TO WRK-OUT-DESCAGE-DUTM
           MOVE WRK-INP-CONTA-X        TO WRK-OUT-CONTA-X
           MOVE WRK-INP-CONTA-DUTM-X   TO WRK-OUT-CONTA-DUTM-X.

           PERFORM VARYING WRK-IND FROM 1 BY 1 UNTIL WRK-IND GREATER 5
              MOVE SPACES             TO WRK-OUT-SELEC-DUTM(WRK-IND)
              IF  (WRK-INP-CONTR-DUTM(WRK-IND)  IS NUMERIC) AND
                  (WRK-INP-CONTR-DUTM(WRK-IND)  GREATER ZEROS)
                   MOVE WRK-INP-SELEC-DUTM(WRK-IND)
                                      TO WRK-OUT-SELEC-DUTM(WRK-IND)
                   MOVE WRK-INP-CONTR-DUTM-X(WRK-IND)
                                      TO WRK-OUT-CONTR-DUTM-X(WRK-IND)
                   MOVE WRK-INP-NOME-DUTM(WRK-IND)
                                      TO WRK-OUT-NOME-DUTM(WRK-IND)
                   MOVE WRK-INP-DTINIC-DUTM(WRK-IND)
                                      TO WRK-OUT-DTINIC-DUTM(WRK-IND)
                   MOVE WRK-INP-DTVENCT-DUTM(WRK-IND)
                                      TO WRK-OUT-DTVENCT-DUTM(WRK-IND)
                   MOVE WRK-INP-LIMITE-DUTM(WRK-IND)
                                      TO WRK-OUT-LIMITE-DUTM-X(WRK-IND)
                   MOVE WRK-INP-SITUAC-DUTM(WRK-IND)
                                      TO WRK-OUT-SITUAC-DUTM(WRK-IND)
              ELSE
                   MOVE WRK-241   TO WRK-OUT-SELEC-ATTR-DUTM(WRK-IND)
              END-IF
           END-PERFORM.

      *----------------------------------------------------------------*
       1210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1220-RETORNAR-CHAMADOR          SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                   COMU-AREA.

           MOVE LENGTH COMU-AREA     TO COMU-LL
           MOVE WRK-CHNG             TO WRK-FUNCAO
           MOVE 'DCOM0413'           TO WRK-TELA
           MOVE 'DCOM0421'           TO COMU-TRANSACAO
           MOVE WRK-INP-TPOOPC-DUTM  TO COMU-OPCAO
           MOVE WRK-INP-TMSTAMP-DUTM TO COMU-TIMESTAMP
           MOVE SPACES               TO COMU-MENSAG
           MOVE COMU-AREA            TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1230-RETORNAR-MENU-ROTINAS      SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-CHNG            TO WRK-FUNCAO.
           MOVE 'DCOM0421'          TO WRK-TELA
                                       COMU-TRANSACAO-413.
           MOVE '5'                 TO COMU-PFK-413.
           MOVE COMU-AREA-413       TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1240-RETORNAR-MENU-DCOM         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-CHNG           TO WRK-FUNCAO.
           MOVE 'DCOM0421'         TO WRK-TELA
                                      COMU-TRANSACAO-413.
           MOVE 'A'                TO COMU-PFK-413.
           MOVE COMU-AREA-413      TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1240-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1250-TRATAR-CONFIRMA            SECTION.
      *----------------------------------------------------------------*

           MOVE  ZEROS                    TO   WRK-IND

           PERFORM VARYING WRK-IND FROM 1 BY 1 UNTIL WRK-IND GREATER 5
             IF  WRK-INP-SELEC-DUTM(WRK-IND) EQUAL 'D'
                 PERFORM 1251-CHAMAR-TRAN-DCOM0422
                 GO                       TO   1250-99-FIM
             END-IF
           END-PERFORM.

      *----------------------------------------------------------------*
       1250-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1251-CHAMAR-TRAN-DCOM0422       SECTION.
      *----------------------------------------------------------------*

           MOVE LENGTH COMU-AREA-422         TO  COMU-LL-422
           MOVE WRK-CHNG                     TO  WRK-FUNCAO
           MOVE 'DCOM0422'                   TO  WRK-TELA
           MOVE 'DCOM0421'                   TO  COMU-TRANSACAO-422
           MOVE WRK-INP-SENHAS-DUTM          TO  COMU-SENHAS-422
           MOVE WRK-INP-TMSTAMP-DUTM         TO  COMU-TMSTAMP-422
           MOVE WRK-INP-CONTR-DUTM(WRK-IND)  TO  COMU-CONTRATO-422
           MOVE WRK-INP-TIPO-DUTM            TO  COMU-TIPO-422
           MOVE WRK-INP-CODAGE-DUTM          TO  COMU-CODAGE-422
           MOVE WRK-INP-CONTA-DUTM           TO  COMU-CONTA-422

FS2511*    MOVE WRK-CNPJ-N                   TO COMU-NUMERO-422-ST
FS2511*    MOVE WRK-FILIAL-N                 TO COMU-FILIAL-422-ST
FS2511     MOVE WRK-CNPJ-N-ST                TO COMU-NUMERO-422-ST
FS2511     MOVE WRK-FILIAL-N-ST              TO COMU-FILIAL-422-ST

           MOVE WRK-INP-CONTROLE-DUTM        TO COMU-CONTROLE-422
           MOVE COMU-AREA-422                TO  WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1251-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1260-TRATAR-VOLTA-PAG           SECTION.
      *----------------------------------------------------------------*

           IF WRK-INP-PAGINA-DUTM  EQUAL  1
              MOVE 'NAO EXISTE PAGINA PARA VOLTAR'
                                       TO WRK-OUT-MENSA-DUTM
              GO TO 1260-99-FIM
           END-IF.

           MOVE  SPACES                TO WRK-OUT-TABE-DUTM

           IF  WRK-INP-CNPJ-X   EQUAL SPACES
               COMPUTE WRK-4111E-RESTART = (WRK-INP-PAGINA-DUTM - 2) * 5
               PERFORM 1120-ACESSAR-DCOM4111
           ELSE
               COMPUTE WRK-4112E-RESTART = (WRK-INP-PAGINA-DUTM - 2) * 5
               PERFORM 1130-ACESSAR-DCOM4112
           END-IF.

           COMPUTE WRK-OUT-PAGINA-DUTM = WRK-INP-PAGINA-DUTM - 1.

      *----------------------------------------------------------------*
       1260-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1270-TRATAR-AVANCA-PAG          SECTION.
      *----------------------------------------------------------------*

           IF WRK-INP-FIMAMOS-DUTM  EQUAL  '*'
              MOVE 'NAO EXISTE PAGINA PARA AVANCAR'
                                       TO WRK-OUT-MENSA-DUTM
              GO TO 1270-99-FIM
           END-IF.

           MOVE  SPACES                TO WRK-OUT-TABE-DUTM


           IF  WRK-INP-CNPJ-X   EQUAL SPACES
               COMPUTE WRK-4111E-RESTART = (WRK-INP-PAGINA-DUTM) * 5
               PERFORM 1120-ACESSAR-DCOM4111
           ELSE
               COMPUTE WRK-4112E-RESTART = (WRK-INP-PAGINA-DUTM) * 5
               PERFORM 1130-ACESSAR-DCOM4112
           END-IF.


           COMPUTE WRK-OUT-PAGINA-DUTM = WRK-INP-PAGINA-DUTM + 1.

      *----------------------------------------------------------------*
       1270-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1280-TRATAR-ENTER               SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                    TO    WRK-IND

           PERFORM VARYING WRK-IND FROM 1 BY 1 UNTIL WRK-IND GREATER 5
             IF  WRK-INP-SELEC-DUTM(WRK-IND) NOT EQUAL  'D' AND
                                                 SPACES AND LOW-VALUES
                 MOVE '*'        TO WRK-INCONSIS
                 MOVE WRK-49353  TO WRK-OUT-SELEC-ATTR-DUTM(WRK-IND)
                 MOVE 'OPCAO INVALIDA - SELECIONAR "D" PARA DETALHAR'
                                 TO  WRK-OUT-MENSA-DUTM
              GO TO 1280-99-FIM
             END-IF
           END-PERFORM.

           MOVE ZEROS              TO    WRK-IND WRK-QTD-SPACES

           PERFORM VARYING WRK-IND FROM 1 BY 1 UNTIL WRK-IND GREATER 5
             IF  WRK-INP-SELEC-DUTM(WRK-IND)   EQUAL SPACES
                 ADD 1             TO WRK-QTD-SPACES
             END-IF
           END-PERFORM.

           IF  WRK-QTD-SPACES      EQUAL      5
               MOVE '*'            TO WRK-INCONSIS
               MOVE 'PARA DETALHAR SELECIONAR UMA OCORRENCIA COM: "D"'
                                   TO  WRK-OUT-MENSA-DUTM
              GO TO 1280-99-FIM
           END-IF.

           MOVE  ZERO              TO  WRK-QTD-D.

           PERFORM VARYING WRK-IND FROM 1 BY 1 UNTIL WRK-IND GREATER 05
             IF  WRK-INP-SELEC-DUTM(WRK-IND) EQUAL 'D'
                 ADD 1             TO  WRK-QTD-D
             END-IF
           END-PERFORM.

           IF  WRK-QTD-D           EQUAL    ZERO  OR
               WRK-QTD-D           GREATER  1
               MOVE '*'            TO WRK-INCONSIS
               MOVE WRK-49353      TO WRK-OUT-SELEC-ATTR-DUTM(WRK-IND)
               MOVE 'PARA DETALHAR SELECIONAR UMA OCORRENCIA COM: "D"'
                                   TO  WRK-OUT-MENSA-DUTM
              GO TO 1280-99-FIM
           END-IF.

           PERFORM 1250-TRATAR-CONFIRMA.

      *----------------------------------------------------------------*
       1280-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1300-PROCESSAR-BRAD0660         SECTION.
      *----------------------------------------------------------------*

           CALL 'BRAD0660'             USING WRK-OUTPUT-DUTM
                                             WRK-660-DCOMDUTM.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE RETURN-CODE        TO WRK-RETURN-CODE
               MOVE 10                 TO WRK-LOCAL-ERRO
               MOVE WRK-ERRO-BRAD0660  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-ISRT               TO WRK-FUNCAO.
           MOVE WRK-OUTPUT-DUTM        TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           IF  ERR-MODULO              EQUAL SPACES AND LOW-VALUES
               MOVE 'SENHAS02'         TO ERR-PGM
               MOVE 'DCOM0421'         TO ERR-MODULO
           ELSE
               MOVE 'DCOM0421'         TO ERR-PGM
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
