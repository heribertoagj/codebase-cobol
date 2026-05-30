      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID.     DCOM1809.
       AUTHOR.         FABRICA - ALTRAN.
      *================================================================*
      *                          A L T R A N                           *
      *----------------------------------------------------------------*
      *     PROGRAMA....: DCOM1809                                     *
      *     PROGRAMADOR.: FABRICA - ALTRAN.                            *
      *     DATA........: 24/08/2007                                   *
      *----------------------------------------------------------------*
      *     OBJETIVO....: INCLUIR PRORROGACAO DE VECTO PARCELAS - LISTA*
      *       OPERACOES POR CLIENTE.                                   *
      *----------------------------------------------------------------*
      *     TELAS.......:                                              *
      *     DCOMRITM....: LISTA DE OPERACAO POR CLIENTE.               *
      *----------------------------------------------------------------*
      *     INC'S.......:                                              *

      *     I#BRAD7C - AREA DE ERRO DA BRAD7100                        *
      *     I#DCOMX3 - AREA DE COMUNICAO DCOM6408                      *
      *----------------------------------------------------------------*
      *     MODULOS.....:                                              *
      *     BRAD0431 - OBTER DIGITO DA CONTA                           *
      *     BRAD0660 - INSERIR CARACTERES DE NULL NA MENSAGEM DE SAIDA *
      *     BRAD7100 - TRATAMENTO DE ERROS QUANDO PROGRAMA INVALIDO    *
      *     DCOM4732 - OBTER LISTA DE OPERACOES POR AGENCIA E CONTA    *
      *     DCOM4733 - OBTER LISTA DE OPERACOES POR CNPJ/CPF           *
      *     DCOM6408 - OBTER NOME DO CLIENTE                           *
      *     DCOM6416 - OBTER NOME DA AGENCIA                           *
      *     POOL5005 - CONTROLE DO TRANSITO DE MENSAGENS NO IMS/DC     *
      *----------------------------------------------------------------*
      *     NAVEGACAO...:                                              *
      *     CHAMADO POR.: DCOM0901           CHAMA: DCOM0905           *
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
           '*** DCOM1809 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

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

           'ERRO NO ACESSO A BRAD0431'.
       77  WRK-MSG011                  PIC  X(079)         VALUE
           'NAO HA OPERACAO(OES) PARA O(S) PARAMETRO(S) INFORMADO(S)'.

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** AREA DE ATRIBUTOS ***'.
      *---------------------------------------------------------------*

      *--- ALPHA    - NORMAL    - PROTEGIDO    - NAO POS CURSOR ---*
       77  WRK-225                     PIC S9(08) COMP     VALUE +225.

      *--- ALPHA    - BRILHANTE - DESPROTEGIDO - POS. CURSOR    ---*
       77  WRK-49353                   PIC S9(08) COMP     VALUE +49353.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*

       77  IND-1                       PIC  9(003) COMP-3  VALUE ZEROS.


       77  WRK-RESTART                 PIC  9(005) COMP-3  VALUE ZEROS.
       77  WRK-SELECAO                 PIC  9(002)         VALUE ZEROS.
       77  WRK-SEL-X                   PIC  9(001) COMP-3  VALUE ZEROS.
       77  WRK-SQLCA                   PIC  X(136)         VALUE SPACES.
       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-INCONSIS                PIC  X(001)         VALUE SPACES.
       77  WRK-ERRO-MENSAGEM           PIC  X(079)         VALUE SPACES.

       01  WRK-AGENCIA-AUX.
           05 WRK-AGENCIA-AUX-N        PIC  9(005)         VALUE ZEROS.

       01  WRK-AREA-BRAD0431.
           05 WRK-CONTA-0431           PIC  9(013) COMP-3  VALUE ZEROS.
           05 WRK-TAMANHO-0431         PIC  9(002)         VALUE ZEROS.
           05 WRK-DIGITO-0431          PIC  X(001)         VALUE SPACES.

       01  WRK-AGCTA.
           05 FILLER                   PIC  X(009)         VALUE
              'BANCO..: '.
           05 WRK-CBANCO               PIC  9(003)         VALUE ZEROS.

           05 FILLER                   PIC  X(010)         VALUE
              ' AGENCIA: '.
           05 WRK-CAGENCIA             PIC  9(005)         VALUE ZEROS.
           05 FILLER                   PIC  X(001)         VALUE '-'.
           05 WRK-IAGENCIA             PIC  X(021)         VALUE SPACES.
           05 FILLER                   PIC  X(010)         VALUE
              ' CONTA..: '.
           05 WRK-CONTA                PIC  9(013)         VALUE ZEROS.
           05 FILLER                   PIC  X(001)         VALUE '-'.
           05 WRK-DIGITO               PIC  9(001)         VALUE ZEROS.
BSI001     05 FILLER                   PIC  X(014)         VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREAS AUXILIARES PARA POOL5005 ***'.
      *----------------------------------------------------------------*

       01  WRK-GU                      PIC  X(004)         VALUE 'GU  '.
       01  WRK-CHNG                    PIC  X(004)         VALUE 'CHNG'.
       01  WRK-ISRT                    PIC  X(004)         VALUE 'ISRT'.
       01  WRK-FUNCAO                  PIC  X(004)         VALUE SPACES.

       01  WRK-TELA                    PIC  X(008)         VALUE SPACES.
       01  WRK-VERSAO                  PIC  X(006)         VALUE
           'VRS002'.

       01  WRK-COD-USER                PIC  X(007)         VALUE SPACES.
       01  WRK-COD-USER-R              REDEFINES
           WRK-COD-USER                PIC  9(007).

       01  WRK-COD-DEPTO               PIC  9(006)         VALUE ZEROS.
       01  FILLER REDEFINES            WRK-COD-DEPTO.
           05  FILLER                  PIC  9(002).
           05  WRK-COD-DEPTO-N         PIC  9(004).

       01  WRK-MENSAGEM.
           05  FILLER                  PIC  X(004)         VALUE SPACES.
           05  WRK-TRANSACAO           PIC  X(008)         VALUE SPACES.
           05  FILLER                  PIC  X(1988)        VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE WRK-COMUNICACAO DCOM0901/DCOM0905 ***'.

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
             10 WRK-COMU-BCAGCT.
               15 WRK-COMU-BANCO       PIC  9(003)         VALUE ZEROS.

               15 WRK-COMU-AGENCIA     PIC  9(005)         VALUE ZEROS.
             15 WRK-COMU-CONTA       PIC  9(013)         VALUE ZEROS.
4S2511       10 WRK-COMU-CCNPJCPF      PIC  X(09)          VALUE SPACES.
4S2511*      10 WRK-COMU-CFLIAL        PIC  9(004)         VALUE ZEROS.
4S2511       10 WRK-COMU-CFLIAL        PIC  X(04)          VALUE SPACES.
             10 WRK-COMU-CCTRL         PIC  9(002)         VALUE ZEROS.
             10 WRK-COMU-CPRODUTO      PIC  9(003)         VALUE ZEROS.
             10 WRK-COMU-IPRODUTO      PIC  X(025)         VALUE SPACES.
             10 WRK-COMU-CSUBPROD      PIC  9(003)         VALUE ZEROS.
             10 WRK-COMU-ISUBPROD      PIC  X(025)         VALUE SPACES.
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
           '*** AREA DA TELA DCOMRITM - INPUT ***'.
      *----------------------------------------------------------------*

       01  WRK-INPUT-RITM.
           05 FILLER                    PIC  X(018)        VALUE SPACES.
           05 WRK-INP-PFK-RITM          PIC  X(001)        VALUE SPACES.
           05 WRK-INP-SENHAS-RITM       PIC  X(037)        VALUE SPACES.
           05 WRK-INP-COMANDO-RITM      PIC  X(068)        VALUE SPACES.
           05 WRK-INP-LITE-RITM         PIC  X(009)        VALUE SPACES.
           05 WRK-INP-DADOS-RITM.

             10 WRK-INP-PAG-RITM.
               15 WRK-INP-PAG-RITM-N    PIC  9(004)        VALUE ZEROS.
             10 WRK-INP-OCULTOS-RITM.
               15 WRK-INP-OPCAO-RITM    PIC  X(001)        VALUE SPACES.
               15 WRK-INP-TMSTAMP-RITM  PIC  X(026)        VALUE SPACES.
               15 WRK-INP-FIMAMOS-RITM  PIC  X(001)        VALUE SPACES.
               15 WRK-INP-TPLISTA-RITM  PIC  X(001)        VALUE SPACES.
4S2511*        15 WRK-INP-CNPJOCUL-RITM PIC  9(009)        VALUE ZEROS.
4S2511         15 WRK-INP-CNPJOCUL-RITM PIC  X(09)         VALUE SPACES.
               15 WRK-INP-RESERVA-RITM  PIC  X(041)        VALUE SPACES.
             10 WRK-INP-NOMCLIE-RITM    PIC  X(039)        VALUE SPACES.
             10 WRK-INP-LITCNPJ-RITM    PIC  X(009)        VALUE SPACES.
             10 WRK-INP-CPFCNPJ-RITM.
4S2511*        15 WRK-INP-NRCNPJCPF-RITM
4S2511*                                 PIC  999.999.999   VALUE ZEROS.
4S2511         15 WRK-INP-NRCNPJCPF-RITM
4S2511                                  PIC  X(11)         VALUE SPACES.
               15 WRK-INP-BARRA-RITM    PIC  X(001)        VALUE SPACES.
4S2511*        15 WRK-INP-FILIAL-RITM   PIC  9(004)        VALUE ZEROS.
4S2511         15 WRK-INP-FILIAL-RITM   PIC  X(04)         VALUE SPACES.
               15 WRK-INP-HIFEN-RITM    PIC  X(001)        VALUE SPACES.
               15 WRK-INP-CONTROLE-RITM PIC  9(002)        VALUE ZEROS.
             10 WRK-INP-AGECTA-RITM     PIC  X(079)        VALUE SPACES.
             10 WRK-INP-LITPROD-RITM    PIC  X(008)        VALUE SPACES.
             10 WRK-INP-CODPROD-RITM.

              15 WRK-INP-CODPROD-RITM-N PIC  9(003)        VALUE ZEROS.
             10 WRK-INP-HIFEN1-RITM     PIC  X(001)        VALUE SPACES.
             10 WRK-INP-DESPROD-RITM    PIC  X(020)        VALUE SPACES.
             10 WRK-INP-LITSUBP-RITM    PIC  X(011)        VALUE SPACES.
             10 WRK-INP-CODSUBP-RITM.
              15 WRK-INP-CODSUBP-RITM-N PIC  9(003)        VALUE ZEROS.
             10 WRK-INP-HIFEN2-RITM     PIC  X(001)        VALUE SPACES.
             10 WRK-INP-DESSUBP-RITM    PIC  X(014)        VALUE SPACES.
             10 WRK-INP-LINHAS-RITM     OCCURS 010         TIMES.
               15 WRK-INP-SELEC-RITM    PIC  X(001)        VALUE SPACES.
               15 WRK-INP-OPERAC-RITM.
                 20 WRK-INP-OPERAC-RITM-N
                                        PIC  9(013)        VALUE ZEROS.
               15 WRK-INP-QTPARC-RITM   PIC  X(005)        VALUE SPACES.
               15 WRK-INP-NOME-RITM     PIC  X(022)        VALUE SPACES.
               15 WRK-INP-VALOR-RITM    PIC  X(014)        VALUE SPACES.
               15 WRK-INP-DTVCTO-RITM   PIC  X(010)        VALUE SPACES.
               15 WRK-INP-SITUAC-RITM   PIC  X(007)        VALUE SPACES.
             10 WRK-INP-MENSA-RITM      PIC  X(079)        VALUE SPACES.

      *----------------------------------------------------------------*

       01  FILLER                       PIC  X(050)        VALUE
           '*** AREA DA TELA DCOMRITM  - OUTPUT ***'.
      *----------------------------------------------------------------*

       01  WRK-OUTPUT-RITM.
           05 WRK-OUT-LL-RITM           PIC S9(004) COMP  VALUE +1227.
           05 WRK-OUT-ZZ-RITM           PIC S9(004) COMP  VALUE ZEROS.
           05 WRK-OUT-SENHAS-RITM       PIC  X(037)       VALUE SPACES.
           05 WRK-OUT-DADOS-RITM.
             10 WRK-OUT-COMANDO-RITM    PIC  X(068)        VALUE SPACES.
             10 WRK-OUT-LITE-RITM       PIC  X(009)        VALUE SPACES.
             10 WRK-OUT-PAG-RITM.
               15 WRK-OUT-PAG-RITM-N    PIC  9(004)        VALUE ZEROS.
             10 WRK-OUT-OCULTOS-RITM.
               15 WRK-OUT-OPCAO-RITM    PIC  X(001)        VALUE SPACES.
               15 WRK-OUT-TMSTAMP-RITM  PIC  X(026)        VALUE SPACES.
               15 WRK-OUT-FIMAMOS-RITM  PIC  X(001)        VALUE SPACES.
               15 WRK-OUT-TPLISTA-RITM  PIC  X(001)        VALUE SPACES.
4S2511*        15 WRK-OUT-CNPJOCUL-RITM PIC  9(009)        VALUE ZEROS.
4S2511         15 WRK-OUT-CNPJOCUL-RITM PIC  X(09)         VALUE SPACES.
               15 WRK-OUT-RESERVA-RITM  PIC  X(041)        VALUE SPACES.
             10 WRK-OUT-NOMCLIE-RITM    PIC  X(039)        VALUE SPACES.

             10 WRK-OUT-LITCNPJ-RITM    PIC  X(009)        VALUE SPACES.
             10 WRK-OUT-CPFCNPJ-RITM.
4S2511*        15 WRK-OUT-NRCNPJCPF-RITM
4S2511*                                 PIC  999.999.999   VALUE ZEROS.
4S2511         15 WRK-OUT-NRCNPJCPF-RITM
4S2511                                  PIC  X(11)         VALUE SPACES.
               15 WRK-OUT-BARRA-RITM    PIC  X(001)        VALUE SPACES.
4S2511*        15 WRK-OUT-FILIAL-RITM   PIC  9(004)        VALUE ZEROS.
4S2511         15 WRK-OUT-FILIAL-RITM   PIC  X(04)         VALUE SPACES.
               15 WRK-OUT-HIFEN-RITM    PIC  X(001)        VALUE SPACES.
               15 WRK-OUT-CONTROLE-RITM PIC  9(002)        VALUE ZEROS.
             10 WRK-OUT-AGECTA-RITM     PIC  X(079)        VALUE SPACES.
             10 WRK-OUT-LITPROD-RITM    PIC  X(008)        VALUE SPACES.
             10 WRK-OUT-CODPROD-RITM.
              15 WRK-OUT-CODPROD-RITM-N PIC  9(003)        VALUE ZEROS.
             10 WRK-OUT-HIFEN1-RITM     PIC  X(001)        VALUE SPACES.
             10 WRK-OUT-DESPROD-RITM    PIC  X(020)        VALUE SPACES.
             10 WRK-OUT-LITSUBP-RITM    PIC  X(011)        VALUE SPACES.
             10 WRK-OUT-CODSUBP-RITM.
              15 WRK-OUT-CODSUBP-RITM-N PIC  9(003)        VALUE ZEROS.
             10 WRK-OUT-HIFEN2-RITM     PIC  X(001)        VALUE SPACES.
             10 WRK-OUT-DESSUBP-RITM    PIC  X(014)        VALUE SPACES.
             10 WRK-OUT-LINHAS-RITM     OCCURS 010 TIMES.
               15 WRK-OUT-SELEC-ATTR-RITM

                                        PIC S9(004) COMP   VALUE ZEROS.
               15 WRK-OUT-SELEC-RITM    PIC  X(001)        VALUE SPACES.
               15 WRK-OUT-OPERAC-RITM.
                 20 WRK-OUT-OPERAC-RITM-N
                                        PIC  9(013)        VALUE ZEROS.
               15 WRK-OUT-QTPARC-RITM.
                 20 WRK-OUT-QTPARC-RITM-N
                                        PIC  X(005)        VALUE SPACES.
               15 WRK-OUT-NOME-RITM     PIC  X(022)        VALUE SPACES.
               15 WRK-OUT-VALOR-RITM.
                 20 WRK-OUT-VALOR-RITM-N
                                        PIC ZZZ.ZZZ.ZZ9,99 VALUE SPACES.
               15 WRK-OUT-DTVCTO-RITM   PIC  X(010)        VALUE SPACES.
               15 WRK-OUT-SITUAC-RITM   PIC  X(007)        VALUE SPACES.
             10 WRK-OUT-MENSA-RITM      PIC  X(079)        VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA TELA DCOMRITM - BRAD0660 ***'.
      *----------------------------------------------------------------*


       01  WRK-660-DCOMRITM.
           05 WRK-660-LL-AREA-RITM     PIC  9(004) COMP    VALUE 0180.
           05 WRK-660-LL-MENSAGEM-RITM PIC  9(004) COMP    VALUE 1227.
           05 WRK-660-SENHAS-RITM      PIC  9(004) COMP    VALUE 0037.
           05 WRK-660-COMANDO-RITM     PIC  9(004) COMP    VALUE 0068.
           05 WRK-660-LITE-RITM        PIC  9(004) COMP    VALUE 0009.
           05 WRK-660-PAG-RITM         PIC  9(004) COMP    VALUE 0004.
           05 WRK-660-OCULTOS-RITM     PIC  9(004) COMP    VALUE 0079.
           05 WRK-660-NOMCLIE-RITM     PIC  9(004) COMP    VALUE 0039.
           05 WRK-660-LITCNPJ-RITM     PIC  9(004) COMP    VALUE 0009.
           05 WRK-660-CPFCNPJ-RITM     PIC  9(004) COMP    VALUE 0019.
           05 WRK-660-AGECTA-RITM      PIC  9(004) COMP    VALUE 0079.
           05 WRK-660-LITPROD-RITM     PIC  9(004) COMP    VALUE 0008.
           05 WRK-660-CODPROD-RITM     PIC  9(004) COMP    VALUE 0003.
           05 WRK-660-HIFEN1-RITM      PIC  9(004) COMP    VALUE 0001.
           05 WRK-660-DESPROD-RITM     PIC  9(004) COMP    VALUE 0020.
           05 WRK-660-LITSUBP-RITM     PIC  9(004) COMP    VALUE 0011.
           05 WRK-660-CODSUBP-RITM     PIC  9(004) COMP    VALUE 0003.
           05 WRK-660-HIFEN2-RITM      PIC  9(004) COMP    VALUE 0001.
           05 WRK-660-DESSUBP-RITM     PIC  9(004) COMP    VALUE 0014.
           05 WRK-600-LISTA            OCCURS 010  TIMES.

             10 WRK-660-SELEC-RITM     PIC  9(004) COMP    VALUE 2003.
             10 WRK-660-OPERAC-RITM    PIC  9(004) COMP    VALUE 0013.
             10 WRK-660-QTPARC-RITM    PIC  9(004) COMP    VALUE 0005.
             10 WRK-660-NOME-RITM      PIC  9(004) COMP    VALUE 0022.
             10 WRK-660-VALOR-RITM     PIC  9(004) COMP    VALUE 0014.
             10 WRK-660-DTVCTO-RITM    PIC  9(004) COMP    VALUE 0010.
             10 WRK-660-SITUAC-RITM    PIC  9(004) COMP    VALUE 0007.
           05 WRK-660-MENSA-RITM       PIC  9(004) COMP    VALUE 0079.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DO MODULO DCOM4732 / 4733 ***'.
      *----------------------------------------------------------------*

       01  WRK-ENTRADA.
           05 WRK-ENT-COD-RETORNO      PIC  X(004)         VALUE SPACES.
           05 WRK-ENT-MSG-RETORNO      PIC  X(079)         VALUE SPACES.
           05 WRK-ENT-RESTART          PIC  9(005)         VALUE ZEROS.
           05 WRK-ENT-FLAG             PIC  9(001)         VALUE ZEROS.
           05 WRK-ENT-START-COUNT      PIC  X(001)         VALUE SPACES.
           05 WRK-ENT-CFUNC-BDSCO      PIC  9(009)         VALUE ZEROS.

           05 WRK-ENT-CTERM            PIC  X(008)         VALUE SPACES.
           05 WRK-ENT-FLAG-FILTRO      PIC  X(002)         VALUE SPACES.
           05 WRK-ENT-CBCO             PIC  9(003)         VALUE ZEROS.
           05 WRK-ENT-CAG-BCRIA        PIC  9(005)         VALUE ZEROS.
           05 WRK-ENT-CCTA-BCRIA-CLI   PIC  9(013)         VALUE ZEROS.
4S2511*    05 WRK-ENT-CCNPJ-CPF        PIC  9(009)         VALUE ZEROS.
4S2511     05 WRK-ENT-CCNPJ-CPF        PIC  X(09)          VALUE SPACES.
4S2511*    05 WRK-ENT-CFLIAL-CNPJ      PIC  9(005)         VALUE ZEROS.
4S2511     05 WRK-ENT-CFLIAL-CNPJ      PIC  X(04)          VALUE SPACES.
           05 WRK-ENT-CCTRL-CNPJ-CPF   PIC  9(002)         VALUE ZEROS.
           05 WRK-ENT-CPRODT           PIC  9(003)         VALUE ZEROS.
           05 WRK-ENT-CSPROD-DESC      PIC  9(003)         VALUE ZEROS.

       01  WRK-SAIDA.
           05 WRK-SAI-COD-RETORNO      PIC  X(004)         VALUE SPACES.
           05 WRK-SAI-MSG-RETORNO      PIC  X(079)         VALUE SPACES.
           05 WRK-SAI-RESTART          PIC  9(005)         VALUE ZEROS.
           05 WRK-SAI-FLAG             PIC  X(001)         VALUE SPACES.
           05 WRK-SAI-QTDE-RETORNADA   PIC  9(003)         VALUE ZEROS.
           05 WRK-SAI-COUNT            PIC  9(009)         VALUE ZEROS.
           05 WRK-SAI-DADOS.
             10 WRK-SAI-TABELA-SAIDA   OCCURS 010 TIMES.
               15 WRK-SAI-OPERACAO     PIC  9(013)         VALUE ZEROS.

               15 WRK-SAI-QTDE-PARCELA PIC  9(005)         VALUE ZEROS.
               15 WRK-SAI-IPSSOA-DESC  PIC  X(030)         VALUE SPACES.
               15 FILLER               PIC  X(030)         VALUE SPACES.
               15 WRK-SAI-VOPER-DESC   PIC  9(015)V9(002)  VALUE ZEROS.
               15 WRK-SAI-DVCTO-FNAL   PIC  X(010)         VALUE SPACES.
               15 WRK-SAI-SITUAC-OPER  PIC  X(010)         VALUE SPACES.
               15 WRK-SAI-CPRODT       PIC  9(003)         VALUE ZEROS.
               15 WRK-SAI-CSPROD-DESC  PIC  9(003)         VALUE ZEROS.

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
             10 WRK-6416S-NOME-DEPDC   PIC  X(040)         VALUE SPACES.
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
           '*** AREA DE COMUNICACAO DCOM6408 ***'.
      *----------------------------------------------------------------*

       COPY 'I#DCOMX3'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA BRAD7100 ***'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE

           '*** DCOM1809 - FIM DA AREA DE WORKING ***'.
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

           MOVE SPACES                 TO WRK-OUT-DADOS-RITM
                                          WRK-INCONSIS.

           PERFORM 2000-PROCESSAR-MENSAGEM.

           GO TO 0000-INICIAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000-PROCESSAR-MENSAGEM         SECTION.
      *----------------------------------------------------------------*



           MOVE 'DCOMRITM'             TO WRK-TELA.

           IF  WRK-TRANSACAO           NOT EQUAL 'DCOM1809'
               PERFORM 2100-RECEBER-CONTROLE
           ELSE
               PERFORM 2200-PROCESSAR-DCOMRITM
           END-IF.

           IF  WRK-TELA                EQUAL 'DCOMRITM'
               PERFORM 2300-PROCESSAR-BRAD0660
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2100-RECEBER-CONTROLE           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM           TO WRK-COMU-AREA.

           MOVE WRK-COMU-OPCAO         TO WRK-OUT-OPCAO-RITM
                                          WRK-INP-OPCAO-RITM.
           MOVE WRK-COMU-TIMESTAMP     TO WRK-OUT-TMSTAMP-RITM
                                          WRK-INP-TMSTAMP-RITM.

           IF  WRK-COMU-CPRODUTO       NOT EQUAL ZEROS
               MOVE 'PRODUTO:'         TO WRK-OUT-LITPROD-RITM
               MOVE WRK-COMU-CPRODUTO  TO WRK-INP-CODPROD-RITM-N
                                          WRK-OUT-CODPROD-RITM-N
               MOVE '-'                TO WRK-OUT-HIFEN1-RITM
               MOVE WRK-COMU-IPRODUTO  TO WRK-INP-DESPROD-RITM
                                          WRK-OUT-DESPROD-RITM
           END-IF.

           IF  WRK-COMU-CSUBPROD       NOT EQUAL ZEROS
               MOVE 'SUBPRODUTO:'      TO WRK-OUT-LITSUBP-RITM
               MOVE WRK-COMU-CSUBPROD  TO WRK-INP-CODSUBP-RITM-N
                                          WRK-OUT-CODSUBP-RITM-N
               MOVE '-'                TO WRK-OUT-HIFEN2-RITM
               MOVE WRK-COMU-ISUBPROD  TO WRK-INP-DESSUBP-RITM
                                          WRK-OUT-DESSUBP-RITM

           END-IF.

           IF  WRK-COMU-BCAGCT         NOT EQUAL ZEROS
               PERFORM 2110-OBTER-DIGITO-CONTA
               PERFORM 2120-ACESSAR-DCOM6416
               MOVE WRK-COMU-BANCO     TO WRK-CBANCO
               MOVE WRK-COMU-AGENCIA   TO WRK-CAGENCIA
               MOVE WRK-6416S-NOME-DEPDC
                                       TO WRK-IAGENCIA
               MOVE WRK-COMU-CONTA     TO WRK-CONTA
               MOVE WRK-DIGITO-0431    TO WRK-DIGITO
               MOVE WRK-AGCTA          TO WRK-INP-AGECTA-RITM
                                          WRK-OUT-AGECTA-RITM
               MOVE '1'                TO WRK-INP-TPLISTA-RITM
                                          WRK-OUT-TPLISTA-RITM
           ELSE
               MOVE 'CNPJ/CPF:'        TO WRK-OUT-LITCNPJ-RITM
               MOVE WRK-COMU-CCNPJCPF  TO WRK-OUT-NRCNPJCPF-RITM
                                          WRK-INP-CNPJOCUL-RITM
                                          WRK-OUT-CNPJOCUL-RITM
               MOVE WRK-COMU-CFLIAL    TO WRK-INP-FILIAL-RITM

                                          WRK-OUT-FILIAL-RITM
               MOVE WRK-COMU-CCTRL     TO WRK-INP-CONTROLE-RITM
                                          WRK-OUT-CONTROLE-RITM
               MOVE '/'                TO WRK-OUT-BARRA-RITM
               MOVE '-'                TO WRK-OUT-HIFEN-RITM
               MOVE '2'                TO WRK-INP-TPLISTA-RITM
                                          WRK-OUT-TPLISTA-RITM
           END-IF.

           IF  WRK-COMU-TRANSACAO      EQUAL 'DCOM1801'
               MOVE 1                  TO WRK-OUT-PAG-RITM-N
                                          WRK-INP-PAG-RITM-N
               MOVE ZEROS              TO WRK-RESTART
           ELSE
               MOVE WRK-COMU-PAGINA-1  TO WRK-OUT-PAG-RITM-N
                                          WRK-INP-PAG-RITM-N
               COMPUTE WRK-RESTART     = (WRK-INP-PAG-RITM-N - 1) * 10
           END-IF.

           IF  WRK-INCONSIS                    EQUAL SPACES
               PERFORM 2130-ACESSAR-DCOM6408


               IF  WRK-INCONSIS                EQUAL SPACES
                   PERFORM 2140-ACESSAR-LISTA

                   IF WRK-SAI-COD-RETORNO      EQUAL '0003'
                      MOVE WRK-MSG011          TO WRK-SAI-MSG-RETORNO
                      MOVE '*'                 TO WRK-INCONSIS
                   END-IF
               END-IF

               IF  WRK-INCONSIS              EQUAL '*'
                   MOVE WRK-SAI-MSG-RETORNO  TO WRK-ERRO-MENSAGEM
                   PERFORM 2220-RETORNAR-CHAMADOR
               ELSE
                   IF WRK-COMU-MENSAGEM  NOT EQUAL SPACES AND LOW-VALUES
                      MOVE WRK-COMU-MENSAGEM TO WRK-OUT-MENSA-RITM
                   ELSE
                      MOVE WRK-MSG001        TO WRK-OUT-MENSA-RITM
                   END-IF
               END-IF
           END-IF.


      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2110-OBTER-DIGITO-CONTA         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-COMU-CONTA         TO WRK-CONTA-0431
           MOVE 07                     TO WRK-TAMANHO-0431.
           MOVE SPACES                 TO WRK-DIGITO-0431.

           CALL 'BRAD0431'             USING WRK-CONTA-0431
                                             WRK-DIGITO-0431
                                             WRK-TAMANHO-0431.

           IF  WRK-DIGITO-0431         EQUAL '.'
               MOVE  WRK-MSG010        TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO

           ELSE
               IF  WRK-DIGITO-0431     EQUAL 'P'
                   MOVE '0'            TO WRK-DIGITO-0431
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2120-ACESSAR-DCOM6416           SECTION.
      *----------------------------------------------------------------*


           MOVE SPACES                 TO WRK-SQLCA ERRO-AREA.

           INITIALIZE WRK-6416-ENTRADA
                      WRK-6416-SAIDA
                      ERRO-AREA.


           MOVE WRK-COD-USER           TO WRK-6416E-CFUNC-BDSCO.
           MOVE LNK-IO-LTERM           TO WRK-6416E-CTERM.
           MOVE WRK-COMU-BANCO         TO WRK-6416E-BANCO.
           MOVE WRK-COMU-AGENCIA       TO WRK-6416E-DEPDC.
           MOVE 'DCOM6416'             TO WRK-MODULO.

           CALL WRK-MODULO             USING WRK-6416-ENTRADA
                                             WRK-6416-SAIDA
                                             ERRO-AREA
                                             WRK-SQLCA.


           EVALUATE WRK-6416S-COD-RETORNO
               WHEN '0000'
                    CONTINUE

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
       2120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2130-ACESSAR-DCOM6408           SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO WRK-SQLCA
                                          ERRO-AREA.

           INITIALIZE ROTENT-6408-AREA
                      ROTSAI-6408-RETORNO
                      ERRO-AREA.


           IF  WRK-INP-TPLISTA-RITM    EQUAL '1'
               MOVE WRK-COMU-CONTA     TO ROTENT-6408-CCTAC-ENV
               MOVE WRK-COMU-AGENCIA   TO ROTENT-6408-CJUNC-ENV
               MOVE WRK-DIGITO-0431    TO ROTENT-6408-DCTAC-ENV
           ELSE
               MOVE WRK-COMU-CCNPJCPF  TO ROTENT-6408-NUM-CNPJ-ENV
               MOVE WRK-COMU-CFLIAL    TO ROTENT-6408-FIL-CNPJ-ENV
               MOVE WRK-COMU-CCTRL     TO ROTENT-6408-CTR-CNPJ-ENV
           END-IF.

           MOVE 'DCOM6408'             TO WRK-MODULO.

           CALL WRK-MODULO             USING ROTENT-6408-AREA
                                             ROTSAI-6408-RETORNO
                                             ERRO-AREA
                                             WRK-SQLCA.


           EVALUATE ROTSAI-6408-COD-RET
               WHEN '0000'
                    MOVE ROTSAI-6408-NM-RZSOC-RET(1)

                                       TO WRK-OUT-NOMCLIE-RITM

               WHEN '0099'
                    MOVE 'DB2'         TO ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                    MOVE ROTSAI-6408-MENS-RET
                                       TO WRK-ERRO-MENSAGEM
                    MOVE '*'           TO WRK-INCONSIS
                    PERFORM 2220-RETORNAR-CHAMADOR
           END-EVALUATE.

      *----------------------------------------------------------------*
       2130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2140-ACESSAR-LISTA              SECTION.
      *----------------------------------------------------------------*



           MOVE SPACES                 TO WRK-SQLCA ERRO-AREA.

           INITIALIZE  WRK-ENTRADA
                       WRK-SAIDA
                       ERRO-AREA.

           MOVE WRK-COD-USER           TO WRK-ENT-CFUNC-BDSCO.
           MOVE LNK-IO-LTERM           TO WRK-ENT-CTERM.
           MOVE WRK-COD-DEPTO-N        TO WRK-AGENCIA-AUX-N.
           MOVE WRK-AGENCIA-AUX        TO WRK-ENT-MSG-RETORNO(1:5).
           MOVE WRK-RESTART            TO WRK-ENT-RESTART.
           MOVE 'S'                    TO WRK-ENT-START-COUNT.

           IF  WRK-INP-OPCAO-RITM         EQUAL '1'
               MOVE '  INCLUIR'           TO WRK-OUT-LITE-RITM
               MOVE 'IP'                  TO WRK-ENT-FLAG-FILTRO
           ELSE
               IF  WRK-INP-OPCAO-RITM     EQUAL '2'
                   MOVE '  EXCLUIR'       TO WRK-OUT-LITE-RITM
                   MOVE 'EP'              TO WRK-ENT-FLAG-FILTRO

               ELSE
                   IF  WRK-INP-OPCAO-RITM EQUAL '3'
                       MOVE 'CONSULTAR'   TO WRK-OUT-LITE-RITM
                       MOVE 'CP'          TO WRK-ENT-FLAG-FILTRO
                   END-IF
               END-IF
           END-IF

           IF  WRK-INP-CODPROD-RITM    NOT EQUAL SPACES
               MOVE WRK-INP-CODPROD-RITM-N
                                       TO WRK-ENT-CPRODT
           END-IF.

           IF  WRK-INP-CODSUBP-RITM    NOT EQUAL SPACES
               MOVE WRK-INP-CODSUBP-RITM-N
                                       TO WRK-ENT-CSPROD-DESC
           END-IF.

           IF  WRK-INP-TPLISTA-RITM    EQUAL '1'
               MOVE WRK-INP-AGECTA-RITM TO WRK-AGCTA
               MOVE WRK-CBANCO         TO WRK-ENT-CBCO

               MOVE WRK-CAGENCIA       TO WRK-ENT-CAG-BCRIA
               MOVE WRK-CONTA          TO WRK-ENT-CCTA-BCRIA-CLI
               MOVE 'DCOM4732'         TO WRK-MODULO
           ELSE
               MOVE WRK-INP-CNPJOCUL-RITM TO WRK-ENT-CCNPJ-CPF
               MOVE WRK-INP-FILIAL-RITM   TO WRK-ENT-CFLIAL-CNPJ
               MOVE WRK-INP-CONTROLE-RITM TO WRK-ENT-CCTRL-CNPJ-CPF
               MOVE 'DCOM4733'            TO WRK-MODULO
           END-IF.

           CALL WRK-MODULO             USING WRK-ENTRADA
                                             WRK-SAIDA
                                             ERRO-AREA
                                             WRK-SQLCA.

           EVALUATE  WRK-SAI-COD-RETORNO
               WHEN '0000'
                     PERFORM 2150-MONTAR-TELA

               WHEN '0003'
                     CONTINUE


               WHEN '0099'
                     MOVE 'DB2'        TO ERR-TIPO-ACESSO
                     PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN  OTHER
                     MOVE WRK-SAI-MSG-RETORNO
                                       TO WRK-OUT-MENSA-RITM
                     MOVE '*'          TO WRK-INCONSIS
           END-EVALUATE.

      *----------------------------------------------------------------*
       2140-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     FORMATACAO DAS OCORRENCIAS DA TELA                         *
      *----------------------------------------------------------------*
       2150-MONTAR-TELA                SECTION.
      *----------------------------------------------------------------*


           PERFORM VARYING IND-1                FROM 1 BY 1
                     UNTIL IND-1                GREATER 10
                IF WRK-SAI-OPERACAO(IND-1)      EQUAL ZEROS
                   MOVE WRK-225
                     TO WRK-OUT-SELEC-ATTR-RITM(IND-1)
                ELSE
                   MOVE WRK-SAI-OPERACAO       (IND-1)
                     TO WRK-OUT-OPERAC-RITM-N  (IND-1)
                   MOVE WRK-SAI-QTDE-PARCELA   (IND-1)
                     TO WRK-OUT-QTPARC-RITM    (IND-1)
                   MOVE WRK-SAI-IPSSOA-DESC    (IND-1)
                     TO WRK-OUT-NOME-RITM      (IND-1)
                   MOVE WRK-SAI-VOPER-DESC     (IND-1)
                     TO WRK-OUT-VALOR-RITM-N   (IND-1)
                   MOVE WRK-SAI-DVCTO-FNAL     (IND-1)
                     TO WRK-OUT-DTVCTO-RITM    (IND-1)
                   MOVE WRK-SAI-SITUAC-OPER    (IND-1)
                     TO WRK-OUT-SITUAC-RITM    (IND-1)
                   INSPECT WRK-OUT-DTVCTO-RITM (IND-1)
                           REPLACING ALL '.' BY '/'
                END-IF

           END-PERFORM.


           IF  WRK-SAI-FLAG                   EQUAL 'S'
               MOVE '*'                       TO WRK-OUT-FIMAMOS-RITM
               MOVE WRK-MSG002                TO WRK-OUT-MENSA-RITM
           ELSE
               MOVE SPACES                    TO WRK-OUT-FIMAMOS-RITM
               MOVE WRK-MSG003                TO WRK-OUT-MENSA-RITM
           END-IF.

      *----------------------------------------------------------------*
       2150-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2200-PROCESSAR-DCOMRITM         SECTION.
      *----------------------------------------------------------------*


           MOVE WRK-MENSAGEM           TO WRK-INPUT-RITM.


           EVALUATE TRUE
               WHEN WRK-INP-PFK-RITM     EQUAL 'H' OR
                   (WRK-INP-PFK-RITM     EQUAL '.' AND
                    WRK-INP-COMANDO-RITM EQUAL 'PFK01')
                    PERFORM 2210-DEVOLVER-TELA
                    MOVE WRK-INP-MENSA-RITM TO WRK-OUT-MENSA-RITM

               WHEN WRK-INP-PFK-RITM     EQUAL '3' OR
                   (WRK-INP-PFK-RITM     EQUAL '.' AND
                    WRK-INP-COMANDO-RITM EQUAL 'PFK03')
                    PERFORM 2220-RETORNAR-CHAMADOR

               WHEN WRK-INP-PFK-RITM     EQUAL '5' OR
                   (WRK-INP-PFK-RITM     EQUAL '.' AND
                    WRK-INP-COMANDO-RITM EQUAL 'PFK05')
                    PERFORM 2230-RETORNAR-MENU-ROTINAS

               WHEN WRK-INP-PFK-RITM      EQUAL '7' OR
                   (WRK-INP-PFK-RITM      EQUAL '.' AND
                    WRK-INP-COMANDO-RITM  EQUAL 'PFK07')

                    PERFORM 2240-RETORNAR-PAGINA

               WHEN WRK-INP-PFK-RITM      EQUAL '8' OR
                   (WRK-INP-PFK-RITM      EQUAL '.' AND
                    WRK-INP-COMANDO-RITM  EQUAL 'PFK08')
                    PERFORM 2250-AVANCAR-PAGINA

               WHEN WRK-INP-PFK-RITM     EQUAL 'A' OR
                   (WRK-INP-PFK-RITM     EQUAL '.' AND
                    WRK-INP-COMANDO-RITM EQUAL 'PFK10')
                    PERFORM 2260-RETORNAR-MENU-DCOM

               WHEN WRK-INP-PFK-RITM     EQUAL '.' AND
                    WRK-INP-COMANDO-RITM EQUAL SPACES
                    PERFORM 2270-TRATAR-ENTER

               WHEN OTHER
                    PERFORM 2210-DEVOLVER-TELA
                    MOVE WRK-MSG004      TO WRK-OUT-MENSA-RITM

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
                IF WRK-INP-LINHAS-RITM(IND-1) EQUAL SPACES
                   MOVE WRK-225        TO WRK-OUT-SELEC-ATTR-RITM(IND-1)
                ELSE
                   MOVE WRK-INP-SELEC-RITM  (IND-1)
                     TO WRK-OUT-SELEC-RITM  (IND-1)
                   MOVE WRK-INP-OPERAC-RITM (IND-1)
                     TO WRK-OUT-OPERAC-RITM (IND-1)

                   MOVE WRK-INP-QTPARC-RITM (IND-1)
                     TO WRK-OUT-QTPARC-RITM (IND-1)
                   MOVE WRK-INP-NOME-RITM   (IND-1)
                     TO WRK-OUT-NOME-RITM   (IND-1)
                   MOVE WRK-INP-VALOR-RITM  (IND-1)
                     TO WRK-OUT-VALOR-RITM  (IND-1)
                   MOVE WRK-INP-DTVCTO-RITM (IND-1)
                     TO WRK-OUT-DTVCTO-RITM (IND-1)
                   MOVE WRK-INP-SITUAC-RITM (IND-1)
                     TO WRK-OUT-SITUAC-RITM (IND-1)
                END-IF
           END-PERFORM.

      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2215-DEVOLVER-TELA-FIXA         SECTION.
      *----------------------------------------------------------------*



           MOVE WRK-INP-PAG-RITM       TO WRK-OUT-PAG-RITM.
           MOVE WRK-INP-LITE-RITM      TO WRK-OUT-LITE-RITM.
           MOVE WRK-INP-OCULTOS-RITM   TO WRK-OUT-OCULTOS-RITM.
           MOVE WRK-INP-NOMCLIE-RITM   TO WRK-OUT-NOMCLIE-RITM.
           MOVE WRK-INP-LITCNPJ-RITM   TO WRK-OUT-LITCNPJ-RITM.
           MOVE WRK-INP-CPFCNPJ-RITM   TO WRK-OUT-CPFCNPJ-RITM.
           MOVE WRK-INP-AGECTA-RITM    TO WRK-OUT-AGECTA-RITM.
           MOVE WRK-INP-LITPROD-RITM   TO WRK-OUT-LITPROD-RITM.
           MOVE WRK-INP-CODPROD-RITM   TO WRK-OUT-CODPROD-RITM.
           MOVE WRK-INP-HIFEN1-RITM    TO WRK-OUT-HIFEN1-RITM.
           MOVE WRK-INP-DESPROD-RITM   TO WRK-OUT-DESPROD-RITM.
           MOVE WRK-INP-LITSUBP-RITM   TO WRK-OUT-LITSUBP-RITM.
           MOVE WRK-INP-CODSUBP-RITM   TO WRK-OUT-CODSUBP-RITM.
           MOVE WRK-INP-HIFEN2-RITM    TO WRK-OUT-HIFEN2-RITM.
           MOVE WRK-INP-DESSUBP-RITM   TO WRK-OUT-DESSUBP-RITM.

      *----------------------------------------------------------------*
       2215-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      *----------------------------------------------------------------*
       2220-RETORNAR-CHAMADOR          SECTION.
      *----------------------------------------------------------------*


           INITIALIZE WRK-COMU-AREA.

           MOVE LENGTH                 OF WRK-COMU-AREA
                                       TO WRK-COMU-LL.
           MOVE WRK-INP-OPCAO-RITM     TO WRK-COMU-OPCAO
           MOVE WRK-INP-TMSTAMP-RITM   TO WRK-COMU-TIMESTAMP
           MOVE WRK-ERRO-MENSAGEM      TO WRK-COMU-MENSAGEM.

           IF  WRK-INP-TPLISTA-RITM       EQUAL '2'
               MOVE WRK-INP-CNPJOCUL-RITM TO WRK-COMU-CCNPJCPF
               MOVE WRK-INP-FILIAL-RITM   TO WRK-COMU-CFLIAL
               MOVE WRK-INP-CONTROLE-RITM TO WRK-COMU-CCTRL
           ELSE
               MOVE WRK-INP-AGECTA-RITM   TO WRK-AGCTA
               MOVE WRK-CAGENCIA          TO WRK-COMU-AGENCIA
               MOVE WRK-CONTA             TO WRK-COMU-CONTA

           END-IF

           MOVE 'DCOM1809'                TO WRK-COMU-TRANSACAO
           MOVE 'DCOM1801'                TO WRK-TELA
           MOVE WRK-CHNG                  TO WRK-FUNCAO
           MOVE WRK-COMU-AREA             TO WRK-MENSAGEM.

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

           MOVE 'DCOM1809'             TO WRK-TELA
                                          WRK-COMU-TRANSACAO.
           MOVE '5'                    TO WRK-COMU-PFK.
           MOVE WRK-COMU-AREA          TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2240-RETORNAR-PAGINA            SECTION.
      *----------------------------------------------------------------*


           IF  WRK-INP-PAG-RITM-N      EQUAL 1
               PERFORM 2210-DEVOLVER-TELA
               MOVE WRK-MSG006         TO WRK-OUT-MENSA-RITM
               GO TO 2240-99-FIM
           END-IF.

           PERFORM 2215-DEVOLVER-TELA-FIXA.


           COMPUTE WRK-RESTART      = (WRK-INP-PAG-RITM-N - 2) * 10.

           PERFORM 2140-ACESSAR-LISTA.

           IF WRK-SAI-COD-RETORNO      NOT EQUAL '0000'
              PERFORM 2210-DEVOLVER-TELA
              MOVE WRK-SAI-MSG-RETORNO TO WRK-OUT-MENSA-RITM
           ELSE
               SUBTRACT 1              FROM WRK-OUT-PAG-RITM-N
           END-IF.

      *----------------------------------------------------------------*
       2240-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2250-AVANCAR-PAGINA             SECTION.
      *----------------------------------------------------------------*



           IF  WRK-INP-FIMAMOS-RITM    EQUAL '*'
               PERFORM 2210-DEVOLVER-TELA
               MOVE WRK-MSG007         TO WRK-OUT-MENSA-RITM
               GO TO 2250-99-FIM
           END-IF.

           PERFORM 2215-DEVOLVER-TELA-FIXA.

           COMPUTE WRK-RESTART      =  WRK-INP-PAG-RITM-N * 10.

           PERFORM 2140-ACESSAR-LISTA.

           IF WRK-SAI-COD-RETORNO      NOT EQUAL '0000'
              PERFORM 2210-DEVOLVER-TELA
              MOVE WRK-SAI-MSG-RETORNO TO WRK-OUT-MENSA-RITM
           ELSE
              ADD 1                    TO WRK-OUT-PAG-RITM-N
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
           MOVE 'DCOM1809'             TO WRK-TELA
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
                   WRK-INP-LINHAS-RITM(IND-1) EQUAL SPACES

                IF WRK-INP-SELEC-RITM (IND-1) NOT EQUAL 'X' AND SPACES
                   MOVE '*'            TO WRK-INCONSIS
                   MOVE WRK-49353      TO WRK-OUT-SELEC-ATTR-RITM(IND-1)
                END-IF
                IF WRK-INP-SELEC-RITM (IND-1) EQUAL 'X'
                   ADD 1               TO WRK-SEL-X

                   MOVE IND-1          TO WRK-SELECAO


                END-IF
           END-PERFORM.

           IF WRK-INCONSIS             EQUAL '*'
              MOVE WRK-MSG009          TO WRK-OUT-MENSA-RITM
           ELSE
              IF WRK-SEL-X             EQUAL ZEROS
                 MOVE WRK-MSG001       TO WRK-OUT-MENSA-RITM
              ELSE
                 IF WRK-SEL-X          GREATER 1
                    MOVE WRK-MSG008    TO WRK-OUT-MENSA-RITM
                    PERFORM VARYING    IND-1 FROM 1 BY 1
                      UNTIL IND-1      GREATER 10 OR
                            WRK-INP-LINHAS-RITM(IND-1) EQUAL SPACES
                         IF WRK-INP-SELEC-RITM (IND-1) EQUAL 'X'
                            MOVE WRK-49353
                              TO WRK-OUT-SELEC-ATTR-RITM(IND-1)
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
           MOVE WRK-INP-OPERAC-RITM-N (WRK-SELECAO)

                                       TO WRK-COMU-OPERACAO.
           MOVE WRK-INP-OPCAO-RITM     TO WRK-COMU-OPCAO.
           MOVE WRK-INP-TMSTAMP-RITM   TO WRK-COMU-TIMESTAMP.

           IF  WRK-INP-CODPROD-RITM    NOT EQUAL SPACES
               MOVE WRK-INP-CODPROD-RITM-N TO WRK-COMU-CPRODUTO
               MOVE WRK-INP-DESPROD-RITM   TO WRK-COMU-IPRODUTO
           END-IF.

           IF  WRK-INP-CODSUBP-RITM    NOT EQUAL SPACES
               MOVE WRK-INP-CODSUBP-RITM-N TO WRK-COMU-CSUBPROD
               MOVE WRK-INP-DESSUBP-RITM   TO WRK-COMU-ISUBPROD
           END-IF.

           IF  WRK-INP-TPLISTA-RITM       EQUAL '1'
               MOVE WRK-INP-AGECTA-RITM   TO WRK-AGCTA
               MOVE WRK-CBANCO            TO WRK-COMU-BANCO
               MOVE WRK-CAGENCIA          TO WRK-COMU-AGENCIA
               MOVE WRK-CONTA             TO WRK-COMU-CONTA
               MOVE 4                     TO WRK-COMU-TP-SELECAO
           ELSE

               MOVE WRK-INP-CNPJOCUL-RITM TO WRK-COMU-CCNPJCPF
               MOVE WRK-INP-FILIAL-RITM   TO WRK-COMU-CFLIAL
               MOVE WRK-INP-CONTROLE-RITM TO WRK-COMU-CCTRL
               MOVE 5                     TO WRK-COMU-TP-SELECAO
           END-IF.

           IF WRK-INP-OPCAO-RITM  EQUAL '1'
              MOVE 1                      TO WRK-COMU-TP-SELECAO
              MOVE 'DCOM1802'             TO WRK-TELA
           ELSE
              IF WRK-INP-OPCAO-RITM  EQUAL '2'
                 MOVE WRK-INP-AGECTA-RITM   TO WRK-AGCTA
                 MOVE WRK-CBANCO            TO WRK-COMU-BANCO
                 MOVE WRK-CAGENCIA          TO WRK-COMU-AGENCIA
                 MOVE WRK-CONTA             TO WRK-COMU-CONTA
                 MOVE WRK-INP-CNPJOCUL-RITM TO WRK-COMU-CCNPJCPF
                 MOVE WRK-INP-FILIAL-RITM   TO WRK-COMU-CFLIAL
                 MOVE WRK-INP-CONTROLE-RITM TO WRK-COMU-CCTRL

                 MOVE 2                   TO WRK-COMU-TP-SELECAO
                 MOVE 'DCOM1804'          TO WRK-TELA

              ELSE
                 IF WRK-INP-OPCAO-RITM  EQUAL '3'
                    MOVE 3                TO WRK-COMU-TP-SELECAO
                    MOVE 'DCOM1806'       TO WRK-TELA
                 END-IF
              END-IF
           END-IF

           MOVE WRK-INP-PAG-RITM-N        TO WRK-COMU-PAGINA-1.
           MOVE 'DCOM1809'                TO WRK-COMU-TRANSACAO.
           MOVE WRK-CHNG                  TO WRK-FUNCAO.
           MOVE WRK-COMU-AREA             TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2270-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2300-PROCESSAR-BRAD0660         SECTION.
      *----------------------------------------------------------------*



           CALL 'BRAD0660'             USING WRK-OUTPUT-RITM
                                             WRK-660-DCOMRITM.


           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE RETURN-CODE        TO WRK-RETURN-CODE
               MOVE 10                 TO WRK-LOCAL-ERRO
               MOVE WRK-ERRO-BRAD0660  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-ISRT               TO WRK-FUNCAO.
           MOVE WRK-OUTPUT-RITM        TO WRK-MENSAGEM.

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
               MOVE 'DCOM1809'         TO ERR-PGM
           ELSE
               MOVE 'SENHAS02'         TO ERR-PGM
               MOVE 'DCOM1809'         TO ERR-MODULO
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
