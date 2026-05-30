      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID.     DCOM1808.
       AUTHOR.         FABRICA - ALTRAN.
      *================================================================*
      *                          A L T R A N                           *
      *----------------------------------------------------------------*
      *     PROGRAMA....: DCOM1808                                     *
      *     PROGRAMADOR.: FABRICA - ALTRAN.                            *
      *     DATA........: 23/08/2007                                   *
      *----------------------------------------------------------------*
      *     OBJETIVO....: INCLUIR ADITAMENTO DA PRORROG - LISTA DE     *
      *       OPERACOES POR AGENCIA.                                   *
      *----------------------------------------------------------------*
      *     TELAS.......:                                              *
      *     DCOMRHTM....: LISTA DE OPERACAO OU ADITAMENTO POR AGENCIA. *
      *----------------------------------------------------------------*
      *     INC'S.......:                                              *

      *     I#BRAD7C - AREA DE ERRO DA BRAD7100                        *
      *----------------------------------------------------------------*
      *     MODULOS.....:                                              *
      *     BRAD0660 - INSERIR CARACTERES DE NULL NA MENSAGEM DE SAIDA *
      *     BRAD7100 - TRATAMENTO DE ERROS QUANDO PROGRAMA INVALIDO    *
      *     DCOM4731 - OBTER LISTA DE OPERACOES POR AGENCIA E FILTRO   *
      *     DCOM6416 - OBTER NOME DA AGENCIA                           *
      *     POOL5005 - CONTROLE DO TRANSITO DE MENSAGENS NO IMS/DC     *
      *----------------------------------------------------------------*
      *     NAVEGACAO...:                                              *
      *     CHAMADO POR.: DCOM1801           CHAMA: DCOM1803           *
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
           '*** DCOM1808 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*


       77  IND-1                       PIC  9(003) COMP-3  VALUE ZEROS.

       77  WRK-RESTART                 PIC  9(005) COMP-3  VALUE ZEROS.
       77  WRK-SELECAO                 PIC  9(002) COMP-3  VALUE ZEROS.
       77  WRK-SEL-X                   PIC  9(001) COMP-3  VALUE ZEROS.
       77  WRK-PAGINA                  PIC  9(005) COMP-3  VALUE ZEROS.
       77  WRK-SQLCA                   PIC  X(136)         VALUE SPACES.
       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-INCONSIS                PIC  X(001)         VALUE SPACES.
       77  WRK-ERRO-MENSAGEM           PIC  X(079)         VALUE SPACES.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** MENSAGENS ***'.
      *----------------------------------------------------------------*

       77  WRK-MSG001                  PIC  X(079)         VALUE
           'SELECIONE UMA OPERACAO COM "X" E TECLE <ENTER>'.
       77  WRK-MSG002                  PIC  X(079)         VALUE
           'FIM DA AMOSTRAGEM'.
       77  WRK-MSG003                  PIC  X(079)         VALUE

           'AMOSTRAGEM CONTINUA'.
       77  WRK-MSG004                  PIC  X(079)         VALUE
           'PF INVALIDA'.
       77  WRK-MSG006                  PIC  X(079)         VALUE
           'NAO EXISTE PAGINA PARA RETORNAR'.
       77  WRK-MSG007                  PIC  X(079)         VALUE
           'NAO EXISTE PAGINA PARA AVANCAR'.
       77  WRK-MSG008                  PIC  X(079)         VALUE
           'SELECIONE APENAS UMA OPERACAO COM "X" E TECLE <ENTER>'.
       77  WRK-MSG009                  PIC  X(079)         VALUE
           'SELECIONE OPERACAO APENAS COM "X"'.
       77  WRK-MSG010                  PIC  X(079)         VALUE
           'NAO HA OPERACAO(OES) PARA O(S) PARAMETRO(S) INFORMADO(S)'.
       77  WRK-MSG011                  PIC  X(079)         VALUE
           'OPERACAO(OES) ADITADA(S) COM SUCESSO'.

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** AREA DE ATRIBUTOS ***'.
      *---------------------------------------------------------------*


      *--- ALPHA    - NORMAL    - PROTEGIDO    - NAO POS CURSOR ---*
       77  WRK-225                     PIC S9(08) COMP     VALUE +225.

      *--- ALPHA    - BRILHANTE - DESPROTEGIDO - POS. CURSOR    ---*
       77  WRK-49353                   PIC S9(08) COMP     VALUE +49353.

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREAS AUXILIARES PARA POOL5005 ***'.
      *----------------------------------------------------------------*

       77  WRK-GU                      PIC  X(004)         VALUE 'GU  '.
       77  WRK-CHNG                    PIC  X(004)         VALUE 'CHNG'.
       77  WRK-ISRT                    PIC  X(004)         VALUE 'ISRT'.
       77  WRK-FUNCAO                  PIC  X(004)         VALUE SPACES.
       77  WRK-TELA                    PIC  X(008)         VALUE SPACES.
       77  WRK-VERSAO                  PIC  X(006)         VALUE
           'VRS002'.

       01  WRK-COD-USER                PIC  X(007)         VALUE SPACES.
       01  WRK-COD-USER-R              REDEFINES

           WRK-COD-USER                PIC  9(007).

       01  WRK-COD-DEPTO               PIC  X(006)         VALUE SPACES.
       01  WRK-COD-DEPTO-R             REDEFINES
           WRK-COD-DEPTO               PIC  9(006).

       01  WRK-MENSAGEM.
           05  FILLER                  PIC  X(004)         VALUE SPACES.
           05  WRK-TRANSACAO           PIC  X(008)         VALUE SPACES.
           05  FILLER                  PIC  X(1988)        VALUE SPACES.

       01  WRK-CFUNC-BDSCO             PIC  9(009)         VALUE ZEROS.
       01  FILLER REDEFINES            WRK-CFUNC-BDSCO.
           05 FILLER                   PIC  9(002).
           05 WRK-CFUNC-BDSCO-7        PIC  9(007).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE WRK-COMUNICACAO DCOM1801/DCOM1803 ***'.
      *----------------------------------------------------------------*


       01  WRK-COMU-AREA.
           05 WRK-COMU-LL              PIC S9(004) COMP    VALUE ZEROS.
           05 WRK-COMU-ZZ              PIC S9(004) COMP    VALUE ZEROS.
           05 WRK-COMU-TRANCODE.
             10 WRK-COMU-TRANSACAO     PIC  X(008)         VALUE SPACES.
             10 FILLER                 PIC  X(006)         VALUE SPACES.
             10 WRK-COMU-PFK           PIC  X(001)         VALUE SPACES.
           05 WRK-COMU-SENHAS          PIC  X(037)         VALUE SPACES.
           05 WRK-COMU-COMANDO         PIC  X(068)         VALUE SPACES.
           05 WRK-COMU-OPCAO           PIC  9(001)         VALUE ZEROS.
           05 WRK-COMU-TIMESTAMP       PIC  X(026)         VALUE SPACES.
           05 WRK-COMU-DADOS.
             10 WRK-COMU-OPERACAO      PIC  9(013)         VALUE ZEROS.
             10 FILLER REDEFINES       WRK-COMU-OPERACAO.
               15 WRK-COMU-DANO-OPER   PIC  9(004).
               15 WRK-COMU-NSEQ-OPER   PIC  9(009).
             10 WRK-COMU-PARCELA       PIC  9(005)         VALUE ZEROS.
             10 WRK-COMU-BANCO         PIC  9(003)         VALUE ZEROS.
             10 WRK-COMU-AGENCIA       PIC  9(005)         VALUE ZEROS.
             10 WRK-COMU-CONTA         PIC  9(013)         VALUE ZEROS.
4S2511       10 WRK-COMU-CCNPJ-CPF     PIC  X(09)          VALUE SPACES.
4S2511       10 WRK-COMU-CFLIAL-CNPJ   PIC  X(04)          VALUE SPACES.
             10 WRK-COMU-CCTRL-CNPJCPF PIC  9(002)         VALUE ZEROS.
             10 WRK-COMU-CPRODUTO      PIC  9(003)         VALUE ZEROS.
             10 WRK-COMU-IPRODUTO      PIC  X(020)         VALUE SPACES.
             10 FILLER                 PIC  X(005)         VALUE SPACES.
             10 WRK-COMU-CSUBPROD      PIC  9(003)         VALUE ZEROS.
             10 WRK-COMU-ISUBPROD      PIC  X(020)         VALUE SPACES.
             10 FILLER                 PIC  X(005)         VALUE SPACES.
             10 WRK-COMU-PAGINA-1      PIC  9(004)         VALUE ZEROS.
             10 WRK-COMU-PAGINA-2      PIC  9(004)         VALUE ZEROS.
             10 WRK-COMU-TP-SELECAO    PIC  9(001)         VALUE ZEROS.
             10 WRK-COMU-MENSAGEM      PIC  X(079)         VALUE SPACES.

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
           '*** AREA DA TELA DCOMRHTM - INPUT ***'.
      *----------------------------------------------------------------*

       01  WRK-INPUT-RHTM.
           05 FILLER                     PIC  X(018)       VALUE SPACES.
           05 WRK-INP-PFK-RHTM           PIC  X(001)       VALUE SPACES.
           05 WRK-INP-SENHAS-RHTM        PIC  X(037)       VALUE SPACES.
           05 WRK-INP-COMANDO-RHTM       PIC  X(068)       VALUE SPACES.
           05 WRK-INP-DADOS-RHTM.
             10 WRK-INP-LITE-RHTM        PIC  X(009)       VALUE SPACES.
             10 WRK-INP-PAG-RHTM.

               15 WRK-INP-PAG-RHTM-N     PIC  9(004)       VALUE ZEROS.
             10 WRK-INP-OCULTOS-RHTM.
               15 WRK-INP-OPCAO-RHTM     PIC  9(001)       VALUE ZEROS.
               15 WRK-INP-TMSTAMP-RHTM   PIC  X(026)       VALUE SPACES.
               15 WRK-INP-FIMAMOS-RHTM   PIC  X(001)       VALUE SPACES.
               15 WRK-INP-RESERVA-RHTM   PIC  X(051)       VALUE SPACES.
             10 WRK-INP-BANCO-RHTM.
               15 WRK-INP-BANCO-RHTM-N   PIC  9(003)       VALUE ZEROS.
             10 WRK-INP-AGENCIA-RHTM.
               15 WRK-INP-AGENCIA-RHTM-N PIC  9(005)       VALUE ZEROS.
             10 WRK-INP-NMAGENC-RHTM     PIC  X(020)       VALUE SPACES.
             10 WRK-INP-LITPROD-RHTM     PIC  X(009)       VALUE SPACES.
             10 WRK-INP-CODPROD-RHTM.
               15 WRK-INP-CODPROD-RHTM-N PIC  9(003)       VALUE ZEROS.
             10 WRK-INP-HIFEN1-RHTM      PIC  X(001)       VALUE SPACES.
             10 WRK-INP-DESPROD-RHTM     PIC  X(020)       VALUE SPACES.
             10 WRK-INP-LITSUBP-RHTM     PIC  X(012)       VALUE SPACES.
             10 WRK-INP-CODSUBP-RHTM.
               15 WRK-INP-CODSUBP-RHTM-N PIC  9(003)       VALUE ZEROS.
             10 WRK-INP-HIFEN2-RHTM      PIC  X(001)       VALUE SPACES.
             10 WRK-INP-DESSUBP-RHTM     PIC  X(020)       VALUE SPACES.

             10 WRK-INP-LINHAS-RHTM      OCCURS 010 TIMES.
               15 WRK-INP-SELEC-RHTM     PIC  X(001)       VALUE SPACES.
               15 WRK-INP-OPERAC-RHTM.
                 20 WRK-INP-OPERAC-RHTM-N
                                         PIC  9(013)       VALUE ZEROS.
               15 WRK-INP-QTPARC-RHTM    PIC  X(005)       VALUE SPACES.
               15 WRK-INP-NOME-RHTM      PIC  X(022)       VALUE SPACES.
               15 WRK-INP-VALOR-RHTM     PIC  X(014)       VALUE SPACES.
               15 WRK-INP-DTVCTO-RHTM    PIC  X(010)       VALUE SPACES.
               15 WRK-INP-SITUAC-RHTM    PIC  X(007)       VALUE SPACES.
             10 WRK-INP-MENSA-RHTM       PIC  X(079)       VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)        VALUE
           '*** AREA DA TELA DCOMRHTM  - OUTPUT ***'.
      *----------------------------------------------------------------*

       01  WRK-OUTPUT-RHTM.
           05 WRK-OUT-LL-RHTM            PIC S9(004) COMP  VALUE +1117.
           05 WRK-OUT-ZZ-RHTM            PIC S9(004) COMP  VALUE ZEROS.
           05 WRK-OUT-SENHAS-RHTM        PIC  X(037)       VALUE SPACES.

           05 WRK-OUT-DADOS-RHTM.
             10 WRK-OUT-COMANDO-RHTM     PIC  X(068)       VALUE SPACES.
             10 WRK-OUT-LITE-RHTM        PIC  X(009)      VALUE SPACES.
             10 WRK-OUT-PAG-RHTM.
               15 WRK-OUT-PAG-RHTM-N     PIC  9(004)       VALUE ZEROS.
             10 WRK-OUT-OCULTOS-RHTM.
               15 WRK-OUT-OPCAO-RHTM     PIC  9(001)       VALUE ZEROS.
               15 WRK-OUT-TMSTAMP-RHTM   PIC  X(026)       VALUE SPACES.
               15 WRK-OUT-FIMAMOS-RHTM   PIC  X(001)       VALUE SPACES.
               15 WRK-OUT-RESERVA-RHTM   PIC  X(051)       VALUE SPACES.
             10 WRK-OUT-BANCO-RHTM.
               15 WRK-OUT-BANCO-RHTM-N   PIC  9(003)       VALUE ZEROS.
             10 WRK-OUT-AGENCIA-RHTM.
               15 WRK-OUT-AGENCIA-RHTM-N PIC  9(005)       VALUE ZEROS.
             10 WRK-OUT-NMAGENC-RHTM     PIC  X(020)       VALUE SPACES.
             10 WRK-OUT-LITPROD-RHTM     PIC  X(009)       VALUE SPACES.
             10 WRK-OUT-CODPROD-RHTM.
               15 WRK-OUT-CODPROD-RHTM-N PIC  9(003)       VALUE ZEROS.
             10 WRK-OUT-HIFEN1-RHTM      PIC  X(001)       VALUE SPACES.
             10 WRK-OUT-DESPROD-RHTM     PIC  X(020)       VALUE SPACES.
             10 WRK-OUT-LITSUBP-RHTM     PIC  X(012)       VALUE SPACES.

             10 WRK-OUT-CODSUBP-RHTM.
               15 WRK-OUT-CODSUBP-RHTM-N PIC  9(003)       VALUE ZEROS.
             10 WRK-OUT-HIFEN2-RHTM      PIC  X(001)       VALUE SPACES.
             10 WRK-OUT-DESSUBP-RHTM     PIC  X(020)       VALUE SPACES.
             10 WRK-OUT-LINHAS-RHTM      OCCURS 010 TIMES.
               15 WRK-OUT-SELEC-ATTR-RHTM
                                         PIC S9(004) COMP   VALUE ZEROS.
               15 WRK-OUT-SELEC-RHTM     PIC  X(001)       VALUE SPACES.
               15 WRK-OUT-OPERAC-RHTM.
                 20 WRK-OUT-OPERAC-RHTM-N
                                         PIC  9(013)       VALUE ZEROS.
               15 WRK-OUT-QTPARC-RHTM.
                 20 WRK-OUT-QTPARC-RHTM-N
                                         PIC  9(005)       VALUE ZEROS.
               15 WRK-OUT-NOME-RHTM      PIC  X(022)       VALUE SPACES.
               15 WRK-OUT-VALOR-RHTM.
                 20 WRK-OUT-VALOR-RHTM-N
                                        PIC ZZZ.ZZZ.ZZ9,99 VALUE SPACES.
               15 WRK-OUT-DTVCTO-RHTM    PIC  X(010)       VALUE SPACES.
               15 WRK-OUT-SITUAC-RHTM    PIC  X(007)       VALUE SPACES.
             10 WRK-OUT-MENSA-RHTM       PIC  X(079)       VALUE SPACES.


      *----------------------------------------------------------------*
       01  FILLER                       PIC  X(050)        VALUE
           '*** AREA DA TELA DCOMRHTM - BRAD0660 ***'.
      *----------------------------------------------------------------*

       01  WRK-660-DCOMRHTM.
           05 WRK-660-LL-AREA-RHTM     PIC  9(004) COMP    VALUE 0178.
           05 WRK-660-LL-MENSA-RHTM    PIC  9(004) COMP    VALUE 1117.
           05 WRK-660-SENHAS-RHTM      PIC  9(004) COMP    VALUE 0037.
           05 WRK-660-COMANDO-RHTM     PIC  9(004) COMP    VALUE 0068.
           05 WRK-660-LITE-RHTM        PIC  9(004) COMP    VALUE 0009.
           05 WRK-660-PAG-RHTM         PIC  9(004) COMP    VALUE 0004.
           05 WRK-660-OCULTOS-RHTM     PIC  9(004) COMP    VALUE 0079.
           05 WRK-660-BANCO-RHTM       PIC  9(004) COMP    VALUE 0003.
           05 WRK-660-AGENCIA-RHTM     PIC  9(004) COMP    VALUE 0005.
           05 WRK-660-NMAGENC-RHTM     PIC  9(004) COMP    VALUE 0020.
           05 WRK-660-LITPROD-RHTM     PIC  9(004) COMP    VALUE 0009.
           05 WRK-660-CODPROD-RHTM     PIC  9(004) COMP    VALUE 0003.
           05 WRK-660-HIFEN1-RHTM      PIC  9(004) COMP    VALUE 0001.
           05 WRK-660-DESPROD-RHTM     PIC  9(004) COMP    VALUE 0020.

           05 WRK-660-LITSUBP-RHTM     PIC  9(004) COMP    VALUE 0012.
           05 WRK-660-CODSUBP-RHTM     PIC  9(004) COMP    VALUE 0003.
           05 WRK-660-HIFEN2-RHTM      PIC  9(004) COMP    VALUE 0001.
           05 WRK-660-DESSUBP-RHTM     PIC  9(004) COMP    VALUE 0020.
           05 WRK-600-LISTA            OCCURS 010  TIMES.
             10 WRK-660-SELEC-RHTM     PIC  9(004) COMP    VALUE 2003.
             10 WRK-660-OPERAC-RHTM    PIC  9(004) COMP    VALUE 0013.
             10 WRK-660-QTPARC-RHTM    PIC  9(004) COMP    VALUE 0005.
             10 WRK-660-NOME-RHTM      PIC  9(004) COMP    VALUE 0022.
             10 WRK-660-VALOR-RHTM     PIC  9(004) COMP    VALUE 0014.
             10 WRK-660-DTVCTO-RHTM    PIC  9(004) COMP    VALUE 0010.
             10 WRK-660-SITUAC-RHTM    PIC  9(004) COMP    VALUE 0007.
           05 WRK-660-MENSA-RHTM       PIC  9(004) COMP    VALUE 0079.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DO MODULO DCOM4731 ***'.
      *----------------------------------------------------------------*

       01  WRK-4731-ENTRADA.
           05 WRK-4731E-COD-RETORNO      PIC  X(004)       VALUE SPACES.

           05 WRK-4731E-MSG-RETORNO      PIC  X(079)       VALUE SPACES.
           05 WRK-4731E-RESTART          PIC  9(005)       VALUE ZEROS.
           05 WRK-4731E-FLAG             PIC  9(001)       VALUE ZEROS.
           05 WRK-4731E-START-COUNT      PIC  X(001)       VALUE SPACES.
           05 WRK-4731E-CFUNC-BDSCO      PIC  9(009)       VALUE ZEROS.
           05 WRK-4731E-CTERM            PIC  X(008)       VALUE SPACES.
           05 WRK-4731E-FLAG-FILTRO      PIC  X(002)       VALUE SPACES.
           05 WRK-4731E-CBCO             PIC  9(003)       VALUE ZEROS.
           05 WRK-4731E-CAG-BCRIA        PIC  9(005)       VALUE ZEROS.
           05 WRK-4731E-CPRODT           PIC  9(003)       VALUE ZEROS.
           05 WRK-4731E-CSPROD-DESC      PIC  9(003)       VALUE ZEROS.

       01  WRK-4731-SAIDA.
           05 WRK-4731S-COD-RETORNO      PIC  X(004)       VALUE SPACES.
           05 WRK-4731S-MSG-RETORNO      PIC  X(079)       VALUE SPACES.
           05 WRK-4731S-RESTART          PIC  9(005)       VALUE ZEROS.
           05 WRK-4731S-FLAG             PIC  X(001)       VALUE SPACES.
           05 WRK-4731S-QTDE-RETORNADA   PIC  9(003)       VALUE ZEROS.
           05 WRK-4731S-COUNT            PIC  9(009)       VALUE ZEROS.
           05 WRK-4731S-DADOS.
             10 WRK-4731S-TABELA-SAIDA   OCCURS 010 TIMES.

               15 WRK-4731S-OPERACAO     PIC  9(013)       VALUE ZEROS.
               15 WRK-4731S-QTDE-PARCELA PIC  9(005)       VALUE ZEROS.
               15 WRK-4731S-IPSSOA-DESC.
                 20 WRK-4731S-IPSSOA-30  PIC  X(030)       VALUE SPACES.
                 20 FILLER               PIC  X(030)       VALUE SPACES.
               15 WRK-4731S-VOPER-DESC   PIC  9(015)V99    VALUE ZEROS.
               15 FILLER REDEFINES       WRK-4731S-VOPER-DESC.
                 20 FILLER               PIC  9(006).
                 20 WRK-4731S-VOPER-9V99 PIC  9(009)V99.
               15 WRK-4731S-DVCTO-FNAL   PIC  X(010)       VALUE SPACES.
               15 WRK-4731S-SITUAC-OPER  PIC  X(010)       VALUE SPACES.
               15 WRK-4731S-CPRODT       PIC  9(003)       VALUE ZEROS.
               15 WRK-4731S-CSPROD-DESC  PIC  9(003)       VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DO MODULO DCOM6416 **'.
      *----------------------------------------------------------------*

       01  WRK-6416-ENTRADA.
           05 WRK-6416E-COD-RETORNO    PIC  X(004)         VALUE SPACES.

           05 WRK-6416E-MSG-RETORNO    PIC  X(079)         VALUE SPACES.
           05 WRK-6416E-RESTART        PIC  9(005)         VALUE ZEROS.
           05 WRK-6416E-FLAG           PIC  X(001)         VALUE SPACES.
           05 WRK-6416E-CFUNC-BDSCO    PIC  9(009)         VALUE ZEROS.
           05 WRK-6416E-CTERM          PIC  X(008)         VALUE SPACES.
           05 WRK-6416E-BANCO          PIC  9(003)         VALUE ZEROS.
           05 WRK-6416E-DEPDC          PIC  9(005)         VALUE ZEROS.

       01  WRK-6416-SAIDA.
           05 WRK-6416S-COD-RETORNO    PIC  X(004)         VALUE SPACES.
           05 WRK-6416S-MSG-RETORNO    PIC  X(079)         VALUE SPACES.
           05 WRK-6416S-RESTART        PIC  9(005)         VALUE ZEROS.
           05 WRK-6416S-FLAG           PIC  X(001)         VALUE SPACES.
           05 WRK-6416S-DADOS.
             10 WRK-6416S-AG-DEPDC     PIC  9(005)         VALUE ZEROS.
             10 WRK-6416S-DG-DEPDC     PIC  X(001)         VALUE SPACES.
             10 WRK-6416S-NOME-DEPDC.
               15 WRK-6416S-NM-DEPDC   PIC  X(020)         VALUE SPACES.
               15 FILLER               PIC  X(020)         VALUE SPACES.
             10 WRK-6416S-END-DEPDC    PIC  X(030)         VALUE SPACES.
             10 WRK-6416S-DIRETORIA-RG PIC  9(005)         VALUE ZEROS.

             10 WRK-6416S-DIRETORIA-NM PIC  X(040)         VALUE SPACES.
             10 WRK-6416S-NOME-BANCO   PIC  X(040)         VALUE SPACES.
             10 WRK-6416S-POLO-SERVICO PIC  9(005)         VALUE ZEROS.
             10 WRK-6416S-MUNIC-DEPDC  PIC  X(025)         VALUE SPACES.
             10 WRK-6416S-UF           PIC  X(002)         VALUE SPACES.
             10 WRK-6416S-CEP          PIC  9(005)         VALUE ZEROS.
             10 WRK-6416S-CEP-COMPL    PIC  9(003)         VALUE ZEROS.
             10 WRK-6416S-INDIC-DEPDC  PIC  X(001)         VALUE SPACES.
             10 WRK-6416S-NATUREZA     PIC  X(001)         VALUE SPACES.
             10 WRK-6416S-COD-DISTR    PIC  9(005)         VALUE ZEROS.
             10 WRK-6416S-DENCRR-DEPDC PIC  X(010)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA BRAD7100 ***'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE

           '*** DCOM1808 - FIM DA AREA DE WORKING ***'.
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

           MOVE SPACES                 TO WRK-OUT-DADOS-RHTM
                                          WRK-INCONSIS.
           MOVE ZEROS                  TO WRK-PAGINA.

           PERFORM 2000-PROCESSAR-MENSAGEM.

           GO TO 0000-INICIAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000-PROCESSAR-MENSAGEM         SECTION.
      *----------------------------------------------------------------*



           MOVE 'DCOMRHTM'             TO WRK-TELA.

           IF  WRK-TRANSACAO           NOT EQUAL 'DCOM1808'
               PERFORM 2100-RECEBER-CONTROLE
           ELSE
               PERFORM 2200-PROCESSAR-DCOMRHTM
           END-IF.

           IF  WRK-TELA                EQUAL 'DCOMRHTM'
               PERFORM 2300-PROCESSAR-BRAD0660
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2100-RECEBER-CONTROLE           SECTION.
      *----------------------------------------------------------------*



           MOVE WRK-MENSAGEM(1:LENGTH  OF WRK-COMU-AREA)
                                       TO WRK-COMU-AREA.
           MOVE WRK-COMU-OPCAO         TO WRK-OUT-OPCAO-RHTM
                                          WRK-INP-OPCAO-RHTM.
           MOVE WRK-COMU-TIMESTAMP     TO WRK-OUT-TMSTAMP-RHTM
                                          WRK-INP-TMSTAMP-RHTM.
           MOVE WRK-COMU-BANCO         TO WRK-INP-BANCO-RHTM-N
                                          WRK-OUT-BANCO-RHTM-N.
           MOVE WRK-COMU-AGENCIA       TO WRK-INP-AGENCIA-RHTM-N
                                          WRK-OUT-AGENCIA-RHTM-N.

           IF  WRK-COMU-CPRODUTO       NOT EQUAL ZEROS
               MOVE 'PRODUTO.:'        TO WRK-OUT-LITPROD-RHTM
               MOVE WRK-COMU-CPRODUTO  TO WRK-INP-CODPROD-RHTM-N
                                          WRK-OUT-CODPROD-RHTM-N
               MOVE '-'                TO WRK-OUT-HIFEN1-RHTM
               MOVE WRK-COMU-IPRODUTO  TO WRK-INP-DESPROD-RHTM
                                          WRK-OUT-DESPROD-RHTM
           END-IF.


           IF  WRK-COMU-CSUBPROD       NOT EQUAL ZEROS
               MOVE 'SUBPRODUTO.:'     TO WRK-OUT-LITSUBP-RHTM
               MOVE WRK-COMU-CSUBPROD  TO WRK-INP-CODSUBP-RHTM-N
                                          WRK-OUT-CODSUBP-RHTM-N
               MOVE '-'                TO WRK-OUT-HIFEN2-RHTM
               MOVE WRK-COMU-ISUBPROD  TO WRK-INP-DESSUBP-RHTM
                                          WRK-OUT-DESSUBP-RHTM
           END-IF.

           IF  WRK-COMU-TRANSACAO      EQUAL 'DCOM1801'
               MOVE 1                  TO WRK-OUT-PAG-RHTM-N
                                          WRK-INP-PAG-RHTM-N
               MOVE ZEROS              TO WRK-RESTART
           ELSE
               MOVE WRK-COMU-PAGINA-1  TO WRK-PAGINA
               MOVE WRK-PAGINA         TO WRK-OUT-PAG-RHTM-N
                                          WRK-INP-PAG-RHTM-N
               COMPUTE WRK-RESTART     = (WRK-PAGINA - 1) * 10
           END-IF.

           PERFORM 2110-ACESSAR-DCOM6416.


           IF  WRK-INCONSIS                     EQUAL SPACES
               PERFORM 2120-ACESSAR-DCOM4731

               IF WRK-4731S-COD-RETORNO         EQUAL '0003'
                  PERFORM 2130-TRATAR-RETORNO-PAG
               END-IF

               IF  WRK-INCONSIS                 EQUAL '*'
                   MOVE WRK-4731S-MSG-RETORNO   TO WRK-ERRO-MENSAGEM
                   PERFORM 2220-RETORNAR-CHAMADOR
               ELSE
                   IF  WRK-COMU-MENSAGEM        EQUAL SPACES
                       MOVE WRK-MSG001          TO WRK-OUT-MENSA-RHTM
                   ELSE
                       MOVE WRK-COMU-MENSAGEM   TO WRK-OUT-MENSA-RHTM
                   END-IF
               END-IF
           END-IF.

      *----------------------------------------------------------------*

       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2110-ACESSAR-DCOM6416           SECTION.
      *----------------------------------------------------------------*


           MOVE SPACES                 TO WRK-SQLCA ERRO-AREA.

           INITIALIZE WRK-6416-ENTRADA
                      WRK-6416-SAIDA
                      ERRO-AREA.

           MOVE WRK-COD-USER-R         TO WRK-CFUNC-BDSCO-7.
           MOVE WRK-CFUNC-BDSCO        TO WRK-6416E-CFUNC-BDSCO.
           MOVE LNK-IO-LTERM           TO WRK-6416E-CTERM.
           MOVE WRK-INP-BANCO-RHTM-N   TO WRK-6416E-BANCO.
           MOVE WRK-INP-AGENCIA-RHTM-N TO WRK-6416E-DEPDC.

           MOVE 'DCOM6416'             TO WRK-MODULO.


           CALL WRK-MODULO             USING WRK-6416-ENTRADA
                                             WRK-6416-SAIDA
                                             ERRO-AREA
                                             WRK-SQLCA.

           EVALUATE WRK-6416S-COD-RETORNO
               WHEN '0000'
                    MOVE WRK-6416S-NM-DEPDC
                                       TO WRK-OUT-NMAGENC-RHTM

               WHEN '0099'
                    MOVE 'DB2'         TO ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                    MOVE WRK-6416S-MSG-RETORNO
                                       TO WRK-ERRO-MENSAGEM
                    MOVE '*'           TO WRK-INCONSIS
                    PERFORM 2220-RETORNAR-CHAMADOR
           END-EVALUATE.


      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2120-ACESSAR-DCOM4731           SECTION.
      *----------------------------------------------------------------*


           MOVE SPACES                 TO WRK-SQLCA ERRO-AREA.

           INITIALIZE  WRK-4731-ENTRADA
                       WRK-4731-SAIDA
                       ERRO-AREA.

           MOVE WRK-COD-USER-R         TO WRK-CFUNC-BDSCO-7.
           MOVE WRK-CFUNC-BDSCO        TO WRK-4731E-CFUNC-BDSCO.
           MOVE LNK-IO-LTERM           TO WRK-4731E-CTERM.
           MOVE WRK-RESTART            TO WRK-4731E-RESTART.
           MOVE WRK-INP-BANCO-RHTM-N   TO WRK-4731E-CBCO.

           MOVE WRK-INP-AGENCIA-RHTM-N TO WRK-4731E-CAG-BCRIA.

           IF  WRK-INP-OPCAO-RHTM      EQUAL '1'
               MOVE '  INCLUIR'              TO WRK-OUT-LITE-RHTM
               MOVE 'IP'                   TO WRK-4731E-FLAG-FILTRO
           ELSE
              IF WRK-INP-OPCAO-RHTM    EQUAL '2'
                 MOVE '  EXCLUIR'            TO WRK-OUT-LITE-RHTM
                 MOVE 'EP'                   TO WRK-4731E-FLAG-FILTRO
              ELSE
                IF WRK-INP-OPCAO-RHTM    EQUAL '3'
                   MOVE 'CONSULTAR'          TO WRK-OUT-LITE-RHTM
                   MOVE 'CP'                 TO WRK-4731E-FLAG-FILTRO
                END-IF
              END-IF
           END-IF

           IF  WRK-INP-CODPROD-RHTM    NOT EQUAL SPACES
               MOVE WRK-INP-CODPROD-RHTM-N
                                       TO WRK-4731E-CPRODT
           END-IF.


           IF  WRK-INP-CODSUBP-RHTM    NOT EQUAL SPACES
               MOVE WRK-INP-CODSUBP-RHTM-N
                                       TO WRK-4731E-CSPROD-DESC
           END-IF.

           MOVE 'DCOM4731'             TO WRK-MODULO


           CALL WRK-MODULO             USING WRK-4731-ENTRADA
                                             WRK-4731-SAIDA
                                             ERRO-AREA
                                             WRK-SQLCA.


           EVALUATE  WRK-4731S-COD-RETORNO
               WHEN '0000'
                     PERFORM 2125-MONTAR-TELA

               WHEN '0003'
                     CONTINUE


               WHEN '0099'
                     MOVE 'DB2'          TO ERR-TIPO-ACESSO
                     PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN  OTHER
                     MOVE WRK-4731S-MSG-RETORNO
                                        TO WRK-OUT-MENSA-RHTM
                     MOVE '*'           TO WRK-INCONSIS
           END-EVALUATE.

      *----------------------------------------------------------------*
       2120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     FORMATACAO DAS OCORRENCIAS DA TELA                         *
      *----------------------------------------------------------------*
       2125-MONTAR-TELA                SECTION.
      *----------------------------------------------------------------*


           PERFORM VARYING IND-1                FROM 1 BY 1
                     UNTIL IND-1                GREATER 10

                IF WRK-4731S-OPERACAO(IND-1)    EQUAL ZEROS
                   MOVE WRK-225
                     TO WRK-OUT-SELEC-ATTR-RHTM(IND-1)
                ELSE
                   MOVE WRK-4731S-OPERACAO     (IND-1)
                     TO WRK-OUT-OPERAC-RHTM-N  (IND-1)
                   MOVE WRK-4731S-QTDE-PARCELA (IND-1)
                     TO WRK-OUT-QTPARC-RHTM    (IND-1)
                   MOVE WRK-4731S-IPSSOA-30    (IND-1)
                     TO WRK-OUT-NOME-RHTM      (IND-1)
                   MOVE WRK-4731S-VOPER-9V99   (IND-1)
                     TO WRK-OUT-VALOR-RHTM-N   (IND-1)
                   MOVE WRK-4731S-DVCTO-FNAL   (IND-1)
                     TO WRK-OUT-DTVCTO-RHTM    (IND-1)
                   MOVE WRK-4731S-SITUAC-OPER  (IND-1)
                     TO WRK-OUT-SITUAC-RHTM    (IND-1)
                   INSPECT WRK-OUT-DTVCTO-RHTM (IND-1)
                           REPLACING ALL '.' BY '/'

                END-IF
           END-PERFORM.

           IF  WRK-4731S-FLAG                 EQUAL 'S'
               MOVE '*'                       TO WRK-OUT-FIMAMOS-RHTM
               MOVE WRK-MSG002                TO WRK-OUT-MENSA-RHTM
           ELSE
               MOVE SPACES                    TO WRK-OUT-FIMAMOS-RHTM
               MOVE WRK-MSG003                TO WRK-OUT-MENSA-RHTM
           END-IF.

      *----------------------------------------------------------------*
       2125-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2130-TRATAR-RETORNO-PAG         SECTION.
      *----------------------------------------------------------------*

           IF WRK-COMU-TRANSACAO         EQUAL 'DCOM1801'
              MOVE WRK-MSG010            TO WRK-4731S-MSG-RETORNO

              MOVE '*'                   TO WRK-INCONSIS
           END-IF.

      *----------------------------------------------------------------*
       2130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2200-PROCESSAR-DCOMRHTM         SECTION.
      *----------------------------------------------------------------*


           MOVE WRK-MENSAGEM(1:LENGTH  OF WRK-INPUT-RHTM)
                                       TO WRK-INPUT-RHTM.

           EVALUATE TRUE
               WHEN WRK-INP-PFK-RHTM     EQUAL 'H' OR
                   (WRK-INP-PFK-RHTM     EQUAL '.' AND
                    WRK-INP-COMANDO-RHTM EQUAL 'PFK01')
                    PERFORM 2210-DEVOLVER-TELA
                    MOVE WRK-INP-MENSA-RHTM TO WRK-OUT-MENSA-RHTM


               WHEN WRK-INP-PFK-RHTM     EQUAL '3' OR
                   (WRK-INP-PFK-RHTM     EQUAL '.' AND
                    WRK-INP-COMANDO-RHTM EQUAL 'PFK03')
                    PERFORM 2220-RETORNAR-CHAMADOR

               WHEN WRK-INP-PFK-RHTM     EQUAL '5' OR
                   (WRK-INP-PFK-RHTM     EQUAL '.' AND
                    WRK-INP-COMANDO-RHTM EQUAL 'PFK05')
                    PERFORM 2230-RETORNAR-MENU-ROTINAS

               WHEN WRK-INP-PFK-RHTM      EQUAL '7' OR
                   (WRK-INP-PFK-RHTM      EQUAL '.' AND
                    WRK-INP-COMANDO-RHTM  EQUAL 'PFK07')
                    PERFORM 2240-RETORNAR-PAGINA

               WHEN WRK-INP-PFK-RHTM      EQUAL '8' OR
                   (WRK-INP-PFK-RHTM      EQUAL '.' AND
                    WRK-INP-COMANDO-RHTM  EQUAL 'PFK08')
                    PERFORM 2250-AVANCAR-PAGINA


               WHEN WRK-INP-PFK-RHTM     EQUAL 'A' OR
                   (WRK-INP-PFK-RHTM     EQUAL '.' AND
                    WRK-INP-COMANDO-RHTM EQUAL 'PFK10')
                    PERFORM 2260-RETORNAR-MENU-DCOM

               WHEN WRK-INP-PFK-RHTM     EQUAL '.' AND
                    WRK-INP-COMANDO-RHTM EQUAL SPACES
                    PERFORM 2270-TRATAR-ENTER

               WHEN OTHER
                    PERFORM 2210-DEVOLVER-TELA
                    MOVE WRK-MSG004      TO WRK-OUT-MENSA-RHTM

           END-EVALUATE.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2210-DEVOLVER-TELA              SECTION.

      *----------------------------------------------------------------*

           PERFORM 2215-DEVOLVER-TELA-FIXA.

           PERFORM VARYING IND-1       FROM 1 BY 1
                     UNTIL IND-1       GREATER 10
                IF WRK-INP-LINHAS-RHTM(IND-1) EQUAL SPACES
                   MOVE WRK-225        TO WRK-OUT-SELEC-ATTR-RHTM(IND-1)
                ELSE
                   MOVE WRK-INP-SELEC-RHTM  (IND-1)
                     TO WRK-OUT-SELEC-RHTM  (IND-1)
                   MOVE WRK-INP-OPERAC-RHTM (IND-1)
                     TO WRK-OUT-OPERAC-RHTM (IND-1)
                   MOVE WRK-INP-QTPARC-RHTM (IND-1)
                     TO WRK-OUT-QTPARC-RHTM (IND-1)
                   MOVE WRK-INP-NOME-RHTM   (IND-1)
                     TO WRK-OUT-NOME-RHTM   (IND-1)
                   MOVE WRK-INP-VALOR-RHTM  (IND-1)
                     TO WRK-OUT-VALOR-RHTM  (IND-1)
                   MOVE WRK-INP-DTVCTO-RHTM (IND-1)
                     TO WRK-OUT-DTVCTO-RHTM (IND-1)

                   MOVE WRK-INP-SITUAC-RHTM (IND-1)
                     TO WRK-OUT-SITUAC-RHTM (IND-1)
                END-IF
           END-PERFORM.

      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2215-DEVOLVER-TELA-FIXA         SECTION.
      *----------------------------------------------------------------*


           MOVE WRK-INP-PAG-RHTM       TO WRK-OUT-PAG-RHTM.
           MOVE WRK-INP-OPCAO-RHTM     TO WRK-OUT-OPCAO-RHTM.
           MOVE WRK-INP-LITE-RHTM      TO WRK-OUT-LITE-RHTM.
           MOVE WRK-INP-OCULTOS-RHTM   TO WRK-OUT-OCULTOS-RHTM.
           MOVE WRK-INP-BANCO-RHTM     TO WRK-OUT-BANCO-RHTM.
           MOVE WRK-INP-AGENCIA-RHTM   TO WRK-OUT-AGENCIA-RHTM.
           MOVE WRK-INP-NMAGENC-RHTM   TO WRK-OUT-NMAGENC-RHTM.

           MOVE WRK-INP-LITPROD-RHTM   TO WRK-OUT-LITPROD-RHTM.
           MOVE WRK-INP-CODPROD-RHTM   TO WRK-OUT-CODPROD-RHTM.
           MOVE WRK-INP-HIFEN1-RHTM    TO WRK-OUT-HIFEN1-RHTM.
           MOVE WRK-INP-DESPROD-RHTM   TO WRK-OUT-DESPROD-RHTM.
           MOVE WRK-INP-LITSUBP-RHTM   TO WRK-OUT-LITSUBP-RHTM.
           MOVE WRK-INP-CODSUBP-RHTM   TO WRK-OUT-CODSUBP-RHTM.
           MOVE WRK-INP-HIFEN2-RHTM    TO WRK-OUT-HIFEN2-RHTM.
           MOVE WRK-INP-DESSUBP-RHTM   TO WRK-OUT-DESSUBP-RHTM.

      *----------------------------------------------------------------*
       2215-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2220-RETORNAR-CHAMADOR          SECTION.
      *----------------------------------------------------------------*


           INITIALIZE WRK-COMU-AREA.

           MOVE LENGTH                 OF WRK-COMU-AREA

                                       TO WRK-COMU-LL.
           MOVE WRK-INP-OPCAO-RHTM     TO WRK-COMU-OPCAO.
           MOVE WRK-INP-AGENCIA-RHTM-N TO WRK-COMU-AGENCIA.
           MOVE WRK-INP-TMSTAMP-RHTM   TO WRK-COMU-TIMESTAMP.
           MOVE WRK-ERRO-MENSAGEM      TO WRK-COMU-MENSAGEM.
           MOVE 'DCOM1808'             TO WRK-COMU-TRANSACAO.
           MOVE 'DCOM1801'             TO WRK-TELA.
           MOVE WRK-CHNG               TO WRK-FUNCAO.
           MOVE WRK-COMU-AREA          TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2230-RETORNAR-MENU-ROTINAS      SECTION.
      *----------------------------------------------------------------*


           INITIALIZE WRK-COMU-AREA.


           MOVE LENGTH                 OF WRK-COMU-AREA
                                       TO WRK-COMU-LL.
           MOVE WRK-CHNG               TO WRK-FUNCAO.
           MOVE 'DCOM1808'             TO WRK-TELA
                                          WRK-COMU-TRANSACAO.
           MOVE '5'                    TO WRK-COMU-PFK.
           MOVE WRK-COMU-AREA          TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2240-RETORNAR-PAGINA            SECTION.
      *----------------------------------------------------------------*


           IF  WRK-INP-PAG-RHTM-N      EQUAL 1
               PERFORM 2210-DEVOLVER-TELA
               MOVE WRK-MSG006         TO WRK-OUT-MENSA-RHTM
               GO TO 2240-99-FIM

           END-IF.

           PERFORM 2215-DEVOLVER-TELA-FIXA.

           MOVE WRK-INP-PAG-RHTM-N     TO WRK-PAGINA.

           COMPUTE WRK-RESTART      = (WRK-PAGINA - 2) * 10.

           PERFORM 2120-ACESSAR-DCOM4731.

           IF  WRK-4731S-COD-RETORNO   EQUAL '0003'
               PERFORM 2210-DEVOLVER-TELA
               MOVE WRK-4731S-MSG-RETORNO
                                       TO WRK-OUT-MENSA-RHTM
           ELSE
               SUBTRACT 1              FROM WRK-PAGINA
               MOVE WRK-PAGINA         TO WRK-OUT-PAG-RHTM-N
           END-IF.

      *----------------------------------------------------------------*
       2240-99-FIM.                    EXIT.

      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2250-AVANCAR-PAGINA             SECTION.
      *----------------------------------------------------------------*


           IF  WRK-INP-FIMAMOS-RHTM    EQUAL '*'
               PERFORM 2210-DEVOLVER-TELA
               MOVE WRK-MSG007         TO WRK-OUT-MENSA-RHTM
               GO TO 2250-99-FIM
           END-IF.

           PERFORM 2215-DEVOLVER-TELA-FIXA.

           MOVE WRK-INP-PAG-RHTM-N     TO WRK-PAGINA.

           COMPUTE WRK-RESTART      =  WRK-PAGINA * 10.

           PERFORM 2120-ACESSAR-DCOM4731.


           IF  WRK-4731S-COD-RETORNO   EQUAL '0003'
               PERFORM 2210-DEVOLVER-TELA
               MOVE WRK-4731S-MSG-RETORNO
                                       TO WRK-OUT-MENSA-RHTM
           ELSE
               ADD 1                   TO WRK-PAGINA
               MOVE WRK-PAGINA         TO WRK-OUT-PAG-RHTM-N
           END-IF.

      *----------------------------------------------------------------*
       2250-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2260-RETORNAR-MENU-DCOM         SECTION.
      *----------------------------------------------------------------*


           INITIALIZE WRK-COMU-AREA.

           MOVE LENGTH                 OF WRK-COMU-AREA

                                       TO WRK-COMU-LL.
           MOVE WRK-CHNG               TO WRK-FUNCAO.
           MOVE 'DCOM1808'             TO WRK-TELA
                                          WRK-COMU-TRANSACAO.
           MOVE 'A'                    TO WRK-COMU-PFK.
           MOVE WRK-COMU-AREA          TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2260-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2270-TRATAR-ENTER               SECTION.
      *----------------------------------------------------------------*

           PERFORM 2210-DEVOLVER-TELA.

           MOVE ZEROS                  TO WRK-SELECAO
                                          WRK-SEL-X.
           MOVE SPACES                 TO WRK-INCONSIS.


           PERFORM VARYING IND-1       FROM 1 BY 1
             UNTIL IND-1               GREATER 10 OR
                   WRK-INP-LINHAS-RHTM(IND-1) EQUAL SPACES
                IF WRK-INP-SELEC-RHTM (IND-1) NOT EQUAL 'X' AND SPACES
                   MOVE '*'            TO WRK-INCONSIS
                   MOVE WRK-49353      TO WRK-OUT-SELEC-ATTR-RHTM(IND-1)
                END-IF
                IF WRK-INP-SELEC-RHTM (IND-1) EQUAL 'X'
                   ADD 1               TO WRK-SEL-X
                   MOVE IND-1          TO WRK-SELECAO
                END-IF
           END-PERFORM.

           IF WRK-INCONSIS             EQUAL '*'
              MOVE WRK-MSG009          TO WRK-OUT-MENSA-RHTM
           ELSE
              IF WRK-SEL-X             EQUAL ZEROS
                 MOVE WRK-MSG001       TO WRK-OUT-MENSA-RHTM
              ELSE
                 IF WRK-SEL-X          GREATER 1
                    MOVE WRK-MSG008    TO WRK-OUT-MENSA-RHTM

                    PERFORM VARYING    IND-1 FROM 1 BY 1
                      UNTIL IND-1      GREATER 10 OR
                            WRK-INP-LINHAS-RHTM(IND-1) EQUAL SPACES
                         IF WRK-INP-SELEC-RHTM (IND-1) EQUAL 'X'
                            MOVE WRK-49353
                              TO WRK-OUT-SELEC-ATTR-RHTM(IND-1)
                         END-IF
                    END-PERFORM
                  ELSE
                    PERFORM 2275-DETALHAR-OPERACAO
                END-IF
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       2270-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2275-DETALHAR-OPERACAO          SECTION.
      *----------------------------------------------------------------*


           INITIALIZE WRK-COMU-AREA.

           MOVE LENGTH                 OF WRK-COMU-AREA
                                       TO WRK-COMU-LL.
           MOVE WRK-INP-OPCAO-RHTM     TO WRK-COMU-OPCAO.
           MOVE WRK-INP-TMSTAMP-RHTM   TO WRK-COMU-TIMESTAMP.
           MOVE WRK-INP-OPERAC-RHTM-N (WRK-SELECAO)
                                       TO WRK-COMU-OPERACAO.
           MOVE WRK-INP-BANCO-RHTM-N   TO WRK-COMU-BANCO.
           MOVE WRK-INP-AGENCIA-RHTM-N TO WRK-COMU-AGENCIA.

           IF  WRK-INP-CODPROD-RHTM    NOT EQUAL SPACES
               MOVE WRK-INP-CODPROD-RHTM-N TO WRK-COMU-CPRODUTO
               MOVE WRK-INP-DESPROD-RHTM   TO WRK-COMU-IPRODUTO
           END-IF.

           IF  WRK-INP-CODSUBP-RHTM    NOT EQUAL SPACES
               MOVE WRK-INP-CODSUBP-RHTM-N TO WRK-COMU-CSUBPROD
               MOVE WRK-INP-DESSUBP-RHTM   TO WRK-COMU-ISUBPROD
           END-IF.


           MOVE WRK-INP-PAG-RHTM-N     TO WRK-COMU-PAGINA-1.


           IF WRK-INP-OPCAO-RHTM  EQUAL '1'
              MOVE 1                      TO WRK-COMU-TP-SELECAO
              MOVE 'DCOM1802'             TO WRK-TELA
           ELSE
              IF WRK-INP-OPCAO-RHTM  EQUAL '2'
                 MOVE 2                   TO WRK-COMU-TP-SELECAO
                 MOVE 'DCOM1804'          TO WRK-TELA
              ELSE
                 IF WRK-INP-OPCAO-RHTM  EQUAL '3'
                    MOVE 3                TO WRK-COMU-TP-SELECAO
                    MOVE 'DCOM1806'       TO WRK-TELA
                 END-IF
              END-IF
           END-IF

           MOVE 'DCOM1808'             TO WRK-COMU-TRANSACAO
           MOVE WRK-CHNG               TO WRK-FUNCAO

           MOVE WRK-COMU-AREA          TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2275-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2300-PROCESSAR-BRAD0660         SECTION.
      *----------------------------------------------------------------*


           CALL 'BRAD0660'             USING WRK-OUTPUT-RHTM
                                             WRK-660-DCOMRHTM.


           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE RETURN-CODE        TO WRK-RETURN-CODE
               MOVE 10                 TO WRK-LOCAL-ERRO
               MOVE WRK-ERRO-BRAD0660  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO

           END-IF.

           MOVE WRK-ISRT               TO WRK-FUNCAO.
           MOVE WRK-OUTPUT-RHTM        TO WRK-MENSAGEM.

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
               MOVE 'DCOM1808'         TO ERR-PGM
           ELSE
               MOVE 'SENHAS02'         TO ERR-PGM
               MOVE 'DCOM1808'         TO ERR-MODULO
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
