      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. DCOM1804.
       AUTHOR.     ALTRAN.
      *================================================================*
      *                          A L T R A N                           *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   DCOM1804                                    *
      *    DATA........:   25/08/2007                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   EXCLUIR PRORROGACAO VENCTO    - SELECAO DA  *
      *                    PARCELA.                                    *
      *----------------------------------------------------------------*
      *    TELAS.......:                                               *
      *    DCOMRDTM - EXCLUIR PRORROGACAO VENCTO    - SELECAO PARCELA  *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    I#BRAD7C - AREA DE ERRO DA BRAD7100.                        *

      *    I#DCOMXJ - AREA DE COMUNICACAO DO MODULO DCOM4174.          *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    BRAD0660 - INSERIR CARACTERES DE NULL NA MENSAGEM DE SAIDA  *
      *    BRAD7100 - TRATAMENTO DE ERROS QUANDO PROGRAMA INVALIDO     *
      *    DCOM4174 - OBTER DADOS DA OPERACAO TRANS/OFICIAL/HISTORICO  *
      *    DCOM5474 - LISTAR AS OPERACOES DE UMA AGENCIA/CONTA         *
      *    POOL5005 - CONTROLE DO TRANSITO DE MENSAGENS NO IMS/DC      *
      *----------------------------------------------------------------*
      *    NAVEGACAO...:                                               *
      *    CHAMADO POR.: DCOM0801           CHAMA: DCOM0803            *
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
           '*** DCOM1804 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** AREA DE ATRIBUTOS ***'.
      *---------------------------------------------------------------*


      *--- ALPHA    - NORMAL    - DESPROTEGIDO - NAO POS CURSOR ---*
       77  WRK-193                     PIC S9(008) COMP    VALUE +193.

      *--- ALPHA    - NORMAL    - PROTEGIDO    - NAO POS CURSOR ---*
       77  WRK-225                     PIC S9(008) COMP    VALUE +225.

      *--- ALPHA    - NORMAL    - DESPROTEGIDO -     POS CURSOR ---*
       77  WRK-49345                   PIC S9(008) COMP    VALUE +49345.

      *--- ALPHA    - BRILHANTE - DESPROTEGIDO - NAO POS CURSOR ---*
       77  WRK-49353                   PIC S9(008) COMP    VALUE +49353.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA MENSAGENS ***'.
      *----------------------------------------------------------------*

       77  WRK-MSG001                  PIC  X(079)         VALUE
           'SELECIONE UMA PARCELA COM "D" PARA DETALHAR OU "E" PARA EXCL
      -    'UIR E TECLE <ENTER>'.
       77  WRK-MSG002                  PIC  X(079)         VALUE

           'PF INVALIDA'.
       77  WRK-MSG004                  PIC  X(079)         VALUE
           'FIM DA AMOSTRAGEM'.
       77  WRK-MSG005                  PIC  X(079)         VALUE
           'AMOSTRAGEM CONTINUA'.
       77  WRK-MSG006                  PIC  X(079)         VALUE
           'NAO EXISTE PAGINA PARA RETORNAR'.
       77  WRK-MSG007                  PIC  X(079)         VALUE
           'NAO EXISTE PAGINA PARA AVANCAR'.
       77  WRK-MSG009                  PIC  X(079)         VALUE
           'SELECIONE PARCELA APENAS COM "X"'.
       77  WRK-MSG011              PIC  X(79)          VALUE
               'SELECAO INVALIDA, SELECIONE "D" P/ DETALHAR OU "E" P/ EX
      -        'CLUIR E TECLE <ENTER>'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*

       77  IND-1                       PIC  9(002) COMP-3  VALUE ZEROS.


       77  WRK-SELECAO                 PIC  9(002) COMP-3  VALUE ZEROS.
       77  WRK-RESTART                 PIC  9(005) COMP-3  VALUE ZEROS.
       77  WRK-SQLCA                   PIC  X(136)         VALUE SPACES.
       77  WRK-CHAMADOR-AUX            PIC  X(008)         VALUE SPACES.
       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-INCONSIS                PIC  X(001)         VALUE SPACES.
       77  WRK-QTD-OPCAO-D             PIC  9(001)         VALUE ZEROS.
       77  WRK-SELEC-E                 PIC  X(001)         VALUE SPACES.
       77  WRK-SELEC-D                 PIC  X(001)         VALUE SPACES.
       01  WRK-AGENCIA-AUX.
           05 WRK-AGENCIA-AUX-N        PIC  9(005)         VALUE ZEROS.

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

       01  WRK-COD-DEPTO.
           05 FILLER                   PIC  X(002)         VALUE SPACES.
           05 WRK-COD-DEPTO-N          PIC  9(004)         VALUE ZEROS.

       01  WRK-MENSAGEM.
           05 FILLER                   PIC  X(004)         VALUE SPACES.
           05 WRK-TRANSACAO            PIC  X(008)         VALUE SPACES.
           05 FILLER                   PIC  X(1988)        VALUE SPACES.

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
           '*** AREA DA TELA DCOMRDTM  - INPUT ***'.
      *----------------------------------------------------------------*

       01  WRK-INPUT-RDTM.
           05 FILLER                   PIC  X(018)         VALUE SPACES.
           05 WRK-INP-PFK-RDTM         PIC  X(001)         VALUE SPACES.

           05 WRK-INP-DADOS-RDTM.
             10 WRK-INP-SENHAS-RDTM    PIC  X(037)         VALUE SPACES.
             10 WRK-INP-COMANDO-RDTM   PIC  X(068)         VALUE SPACES.
             10 WRK-INP-PAGINA-RDTM    PIC  9(004)         VALUE ZEROS.
             10 WRK-INP-OCULTOS-RDTM.
               15 WRK-INP-OPCAO-RDTM    PIC  X(001)        VALUE SPACES.
               15 WRK-INP-TMSTAMP-RDTM  PIC  X(026)        VALUE SPACES.
               15 WRK-INP-FIMAMOS-RDTM  PIC  X(001)        VALUE SPACES.
             15 WRK-INP-PAGOCUL-RDTM  PIC  9(004)        VALUE ZEROS.
4S2511*        15 WRK-INP-CNPJOC-RDTM   PIC  9(009)        VALUE ZEROS.
4S2511         15 WRK-INP-CNPJOC-RDTM   PIC  X(09)         VALUE SPACES.
               15 WRK-INP-PRODOC-RDTM   PIC  9(003)        VALUE ZEROS.
               15 WRK-INP-SBPROC-RDTM   PIC  9(003)        VALUE ZEROS.
               15 WRK-INP-TPSELECAO-RDTM
                                        PIC  9(001)        VALUE ZEROS.
               15 WRK-INP-AGENOC-RDTM   PIC  9(005)        VALUE ZEROS.
               15 WRK-INP-CONTOC-RDTM   PIC  9(013)        VALUE ZEROS.
               15 WRK-INP-SELECP-RDTM   PIC  X(001)        VALUE SPACES.
               15 WRK-INP-SELECT-RDTM   PIC  X(001)        VALUE SPACES.
               15 WRK-INP-CHAMADO-RDTM  PIC  X(008)        VALUE SPACES.
               15 WRK-INP-FASE-RDTM     PIC  X(001)        VALUE SPACES.
               15 FILLER                PIC  X(002)        VALUE SPACES.

             10 WRK-INP-NOMCLIE-RDTM    PIC  X(040)        VALUE SPACES.
             10 WRK-INP-CNPJCPF-RDTM.
4S2511*       15 WRK-INP-CCNPJCPF-RDTM PIC ZZZ.ZZZ.ZZ9    VALUE ZEROS.
4S2511        15 WRK-INP-CCNPJCPF-RDTM PIC X(011)         VALUE SPACES.
              15 FILLER                PIC  X(001)        VALUE SPACES.
4S2511*       15 WRK-INP-FILIAL-RDTM   PIC  9(004)        VALUE ZEROS.
4S2511        15 WRK-INP-FILIAL-RDTM   PIC  X(04)         VALUE SPACES.
4S2511*       15 WRK-INP-FILIAL-RDTM-ST PIC  X(04)        VALUE SPACES.
              15 FILLER                PIC  X(001)        VALUE SPACES.
              15 WRK-INP-CONTROLE-RDTM PIC  9(002)        VALUE ZEROS.
             10 WRK-INP-BCAGCTA-RDTM.
               15 WRK-INP-BANCO-RDTM    PIC  9(003)        VALUE ZEROS.
               15 FILLER                PIC  X(001)        VALUE SPACES.
               15 WRK-INP-AGENCIA-RDTM  PIC  9(005)        VALUE ZEROS.
               15 FILLER                PIC  X(001)        VALUE SPACES.
               15 WRK-INP-CONTA-RDTM    PIC  9(013)        VALUE ZEROS.
             10 WRK-INP-PRODUTO-RDTM.
               15 WRK-INP-CODPROD-RDTM  PIC  9(003)        VALUE ZEROS.
               15 WRK-INP-DESPROD-RDTM  PIC  X(012)        VALUE SPACES.
             10 WRK-INP-SUBPRODUTO-RDTM.
               15 WRK-INP-CODSUBP-RDTM  PIC  9(003)        VALUE ZEROS.
               15 WRK-INP-DESSUBP-RDTM  PIC  X(012)        VALUE SPACES.
             10 WRK-INP-OPERAC-RDTM     PIC  9(013)        VALUE ZEROS.
             10 FILLER REDEFINES        WRK-INP-OPERAC-RDTM.

               15 WRK-INP-DANO-RDTM     PIC  9(004).
               15 WRK-INP-NSEQ-RDTM     PIC  9(009).
             10 WRK-INP-DESSIT-RDTM     PIC  X(015)        VALUE SPACES.
             10 WRK-INP-SIT1-RDTM       PIC  X(013)        VALUE SPACES.
             10 WRK-INP-SIT2-RDTM.
               15 WRK-INP-SIT2-RDTM-N   PIC  9(009)        VALUE ZEROS.
             10 WRK-INP-SIT3-RDTM       PIC  X(013)        VALUE SPACES.
             10 WRK-INP-SIT4-RDTM.
               15 WRK-INP-SIT4-RDTM-N   PIC  9(009)        VALUE ZEROS.
             10 WRK-INP-TABELA-RDTM     OCCURS 009 TIMES.
               15 WRK-INP-SELEC-RDTM    PIC  X(001)        VALUE SPACES.
               15 WRK-INP-PARC-RDTM.
                 20 WRK-INP-PARC-RDTM-N PIC  9(003)        VALUE ZEROS.
               15 WRK-INP-NOME-RDTM     PIC  X(013)        VALUE SPACES.
               15 WRK-INP-DTVCTO-RDTM   PIC  X(010)        VALUE SPACES.
               15 WRK-INP-DTPROR-RDTM   PIC  X(010)        VALUE SPACES.
               15 WRK-INP-TXORIG-RDTM-X.
                 20 WRK-INP-TXORIG-RDTM   PIC  ZZ9,99     VALUE ZEROS.
               15 WRK-INP-TXPROR-RDTM-X.
                 20 WRK-INP-TXPROR-RDTM   PIC  ZZ9,99      VALUE ZEROS.
               15 WRK-INP-VALOR-RDTM.

                 20 WRK-INP-VALOR-RDTM-N
                                        PIC Z.ZZZ.ZZ9,99   VALUE ZEROS.
               15 WRK-INP-VLENC-RDTM.
                 20 WRK-INP-VLENC-RDTM-N
                                        PIC ZZZ.ZZ9,99     VALUE ZEROS.
             10 WRK-INP-MENSA-RDTM      PIC  X(079)        VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA TELA DCOMRDTM  - OUTPUT ***'.
      *----------------------------------------------------------------*

       01  WRK-OUTPUT-RDTM.
           05 WRK-OUT-LL-RDTM           PIC S9(004) COMP   VALUE +1114.
           05 WRK-OUT-ZZ-RDTM           PIC  9(004) COMP   VALUE ZEROS.
           05 WRK-OUT-DADOS-RDTM.
             10 WRK-OUT-SENHAS-RDTM     PIC  X(037)        VALUE SPACES.
             10 WRK-OUT-COMANDO-ATTR-RDTM
                                        PIC  9(004) COMP   VALUE ZEROS.
             10 WRK-OUT-COMANDO-RDTM    PIC  X(068)        VALUE SPACES.
             10 WRK-OUT-PAGINA-RDTM     PIC  9(004)        VALUE ZEROS.

             10 WRK-OUT-OCULTOS-RDTM.
               15 WRK-OUT-OPCAO-RDTM    PIC  X(001)        VALUE SPACES.
               15 WRK-OUT-TMSTAMP-RDTM  PIC  X(026)        VALUE SPACES.
               15 WRK-OUT-FIMAMOS-RDTM  PIC  X(001)        VALUE SPACES.
               15 WRK-OUT-PAGOCUL-RDTM  PIC  9(004)        VALUE ZEROS.
4S2511*        15 WRK-OUT-CNPJOC-RDTM   PIC  9(009)        VALUE ZEROS.
4S2511         15 WRK-OUT-CNPJOC-RDTM   PIC  X(09)         VALUE SPACES.
               15 WRK-OUT-PRODOC-RDTM   PIC  9(003)        VALUE ZEROS.
               15 WRK-OUT-SBPROC-RDTM   PIC  9(003)        VALUE ZEROS.
               15 WRK-OUT-TPSELECAO-RDTM
                                        PIC  9(001)        VALUE ZEROS.
               15 WRK-OUT-AGENOC-RDTM   PIC  9(005)        VALUE ZEROS.
               15 WRK-OUT-CONTOC-RDTM   PIC  9(013)        VALUE ZEROS.
               15 WRK-OUT-SELECP-RDTM   PIC  X(001)        VALUE SPACES.
               15 WRK-OUT-SELECT-RDTM   PIC  X(001)        VALUE SPACES.
               15 WRK-OUT-CHAMADO-RDTM  PIC  X(008)        VALUE SPACES.
               15 WRK-OUT-FASE-RDTM     PIC  X(001)        VALUE SPACES.
               15 FILLER                PIC  X(002)        VALUE SPACES.
             10 WRK-OUT-NOMCLIE-RDTM    PIC  X(040)        VALUE SPACES.
             10 WRK-OUT-CNPJCPF-RDTM.
4S2511*       15 WRK-OUT-CCNPJCPF-RDTM PIC ZZZ.ZZZ.ZZ9    VALUE ZEROS.
4S2511        15 WRK-OUT-CCNPJCPF-RDTM PIC X(011)         VALUE SPACES.
              15 FILLER                PIC  X(001)        VALUE SPACES.
4S2511*       15 WRK-OUT-FILIAL-RDTM   PIC  9(004)        VALUE ZEROS.
4S2511        15 WRK-OUT-FILIAL-RDTM   PIC  X(04)         VALUE SPACES.
              15 FILLER                PIC  X(001)        VALUE SPACES.
              15 WRK-OUT-CONTROLE-RDTM PIC  9(002)        VALUE ZEROS.
             10 WRK-OUT-BCAGCTA-RDTM.
               15 WRK-OUT-BANCO-RDTM    PIC  9(003)        VALUE ZEROS.
               15 FILLER                PIC  X(001)        VALUE SPACES.
               15 WRK-OUT-AGENCIA-RDTM  PIC  9(005)        VALUE ZEROS.
               15 FILLER                PIC  X(001)        VALUE SPACES.
               15 WRK-OUT-CONTA-RDTM    PIC  9(013)        VALUE ZEROS.
             10 WRK-OUT-PRODUTO-RDTM.
               15 WRK-OUT-CODPROD-RDTM  PIC  9(003)        VALUE ZEROS.
               15 WRK-OUT-DESPROD-RDTM  PIC  X(012)        VALUE SPACES.
             10 WRK-OUT-SUBPRODUTO-RDTM.
               15 WRK-OUT-CODSUBP-RDTM  PIC  9(003)        VALUE ZEROS.
               15 WRK-OUT-DESSUBP-RDTM  PIC  X(012)        VALUE SPACES.
             10 WRK-OUT-OPERAC-RDTM     PIC  9(013)        VALUE ZEROS.
             10 FILLER REDEFINES        WRK-OUT-OPERAC-RDTM.
               15 WRK-OUT-DANO-RDTM     PIC  9(004).
               15 WRK-OUT-NSEQ-RDTM     PIC  9(009).
             10 WRK-OUT-DESSIT-RDTM     PIC  X(015)        VALUE SPACES.
             10 WRK-OUT-SIT1-RDTM       PIC  X(013)        VALUE SPACES.

             10 WRK-OUT-SIT2-RDTM.
               15 WRK-OUT-SIT2-RDTM-N   PIC  9(009)        VALUE ZEROS.
             10 WRK-OUT-SIT3-RDTM       PIC  X(013)        VALUE SPACES.
             10 WRK-OUT-SIT4-RDTM.
               15 WRK-OUT-SIT4-RDTM-N   PIC  9(009)        VALUE ZEROS.
             10 WRK-OUT-TABELA-RDTM     OCCURS 009 TIMES.
               15 WRK-OUT-SELEC-ATTR-RDTM
                                        PIC S9(004) COMP   VALUE ZEROS.
               15 WRK-OUT-SELEC-RDTM    PIC  X(001)        VALUE SPACES.
               15 WRK-OUT-PARC-RDTM.
                 20 WRK-OUT-PARC-RDTM-N PIC  9(003)        VALUE ZEROS.
               15 WRK-OUT-NOME-RDTM     PIC  X(013)        VALUE SPACES.
               15 WRK-OUT-DTVCTO-RDTM   PIC  X(010)        VALUE SPACES.
               15 WRK-OUT-DTPROR-RDTM   PIC  X(010)        VALUE SPACES.
               15 WRK-OUT-TXORIG-RDTM-X.
                 20 WRK-OUT-TXORIG-RDTM   PIC  ZZ9,99      VALUE ZEROS.
               15 WRK-OUT-TXPROR-RDTM-X.
                 20 WRK-OUT-TXPROR-RDTM   PIC  ZZ9,99      VALUE ZEROS.
               15 WRK-OUT-VALOR-RDTM.
                 20 WRK-OUT-VALOR-RDTM-N
                                        PIC Z.ZZZ.ZZ9,99   VALUE ZEROS.

               15 WRK-OUT-VLENC-RDTM.
                 20 WRK-OUT-VLENC-RDTM-N
                                        PIC ZZZ.ZZ9,99     VALUE ZEROS.
             10 WRK-OUT-MENSA-RDTM      PIC  X(079)        VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA TELA DCOMRDTM  - BRAD0660 ***'.
      *----------------------------------------------------------------*

       01  WRK-660-DCOMRDTM.
           05 WRK-660-LL-AREA-RDTM     PIC  9(004) COMP    VALUE 0202.
           05 WRK-660-LL-MENSA-RDTM    PIC  9(004) COMP    VALUE 1114.
           05 WRK-660-SENHAS-RDTM      PIC  9(004) COMP    VALUE 0037.
           05 WRK-660-COMANDO-RDTM     PIC  9(004) COMP    VALUE 2070.
           05 WRK-660-PAGINA-RDTM      PIC  9(004) COMP    VALUE 0004.
           05 WRK-660-OCULTO1-RDTM     PIC  9(004) COMP    VALUE 0079.
           05 WRK-660-NOMCLIE-RDTM     PIC  9(004) COMP    VALUE 0040.
           05 WRK-660-CNPJCPF-RDTM     PIC  9(004) COMP    VALUE 0019.
           05 WRK-660-BCAGCTA-RDTM     PIC  9(004) COMP    VALUE 0023.
           05 WRK-660-CODPROD-RDTM     PIC  9(004) COMP    VALUE 0003.

           05 WRK-660-DESPROD-RDTM     PIC  9(004) COMP    VALUE 0012.
           05 WRK-660-CODSUBP-RDTM     PIC  9(004) COMP    VALUE 0003.
           05 WRK-660-DESSUBP-RDTM     PIC  9(004) COMP    VALUE 0012.
           05 WRK-660-OPERAC-RDTM      PIC  9(004) COMP    VALUE 0013.
           05 WRK-660-DESSIT-RDTM      PIC  9(004) COMP    VALUE 0015.
           05 WRK-660-SIT1-RDTM        PIC  9(004) COMP    VALUE 0013.
           05 WRK-660-SIT2-RDTM        PIC  9(004) COMP    VALUE 0009.
           05 WRK-660-SIT3-RDTM        PIC  9(004) COMP    VALUE 0013.
           05 WRK-660-SIT4-RDTM        PIC  9(004) COMP    VALUE 0009.
           05 WRK-660-TABELA-RDTM      OCCURS 009  TIMES.
             10 WRK-660-SELEC-RDTM     PIC  9(004) COMP    VALUE 2003.
             10 WRK-660-PARC-RDTM      PIC  9(004) COMP    VALUE 0003.
             10 WRK-660-NOME-RDTM      PIC  9(004) COMP    VALUE 0013.
             10 WRK-660-DTVCTO-RDTM    PIC  9(004) COMP    VALUE 0010.
             10 WRK-660-DTPROR-RDTM    PIC  9(004) COMP    VALUE 0010.
             10 WRK-660-TXORIG-RDTM    PIC  9(004) COMP    VALUE 0006.
             10 WRK-660-TXPROR-RDTM    PIC  9(004) COMP    VALUE 0006.
             10 WRK-660-VALOR-RDTM     PIC  9(004) COMP    VALUE 0012.
             10 WRK-660-VLENC-RDTM     PIC  9(004) COMP    VALUE 0010.
           05 WRK-660-MENSAGE-RDTM     PIC  9(004) COMP    VALUE 0079.


      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO ***'.
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
             10 WRK-COMU-CNPJCPF.
4S2511         15 WRK-COMU-CCNPJ-CPF   PIC  X(09)          VALUE SPACES.
4S2511         15 WRK-COMU-CFLIAL-CNPJ PIC  X(04)          VALUE SPACES.
               15 WRK-COMU-CCTRL-CNPJCPF
                                       PIC  9(002)         VALUE ZEROS.
             10 WRK-COMU-CPRODUTO      PIC  9(003)         VALUE ZEROS.
             10 WRK-COMU-IPRODUTO      PIC  X(025)         VALUE SPACES.
             10 WRK-COMU-CSUBPROD      PIC  9(003)         VALUE ZEROS.
             10 WRK-COMU-ISUBPROD      PIC  X(025)         VALUE SPACES.
             10 WRK-COMU-PAGINA-1      PIC  9(004)         VALUE ZEROS.
             10 WRK-COMU-PAGINA-2      PIC  9(004)         VALUE ZEROS.
             10 WRK-COMU-TP-SELECAO    PIC  9(001)         VALUE ZEROS.
      *-------- - 3 = SELECAO POR AGENCIA         (DCOM0903)
      *-------- - 4 = SELECAO POR AGENCIA E CONTA (DCOM0904)
      *-------- - 5 = SELECAO POR CNPJ/CPF        (DCOM0904)
             10 WRK-COMU-MENSAGEM      PIC  X(079)         VALUE SPACES.

             10 WRK-COMU-CHAMADOR      PIC  X(008)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
          '*** AREA DE COMUNICACAO MODULO DCOM5335 ***'.
      *----------------------------------------------------------------*

       01  5335-ENTRADA.
           05 5335E-COD-RETORNO        PIC  X(004)         VALUE SPACES.
           05 5335E-MSG-RETORNO        PIC  X(079)         VALUE SPACES.
           05 5335E-RESTART            PIC  9(005)         VALUE ZEROS.
           05 5335E-FLAG               PIC  9(001)         VALUE ZEROS.
           05 5335E-CFUNC-BDSCO        PIC  9(009)         VALUE ZEROS.
           05 5335E-CTERM              PIC  X(008)         VALUE SPACES.
           05 5335E-OPERACAO.
              10 5335E-DANO-OPER-DESC  PIC  9(004)         VALUE ZEROS.
              10 5335E-NSEQ-OPER-DESC  PIC  9(009)         VALUE ZEROS.
           05 5335E-NPCELA-DESC-COML   PIC  9(005)         VALUE ZEROS.
           05 5335E-CELMTO-DESC-COML   PIC  9(003)         VALUE ZEROS.
           05 5335E-CEVNTO-DESC-COML   PIC  9(003)         VALUE ZEROS.
           05 5335E-ACAO               PIC  X(001)         VALUE SPACES.


       01  5335-SAIDA.
           05 5335S-COD-RETORNO        PIC  9(004)         VALUE ZEROS.
           05 5335S-MSG-RETORNO        PIC  X(079)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
          '*** AREA DE COMUNICACAO MODULO DCOM5474 ***'.
      *----------------------------------------------------------------*

       01  WRK-5474-ENTRADA.
           05 WRK-5474E-COD-RETORNO    PIC  X(004)         VALUE SPACES.
           05 WRK-5474E-MSG-RETORNO    PIC  X(079)         VALUE SPACES.
           05 WRK-5474E-RESTART        PIC  9(005)         VALUE ZEROS.
           05 WRK-5474E-FLAG           PIC  X(001)         VALUE SPACES.
           05 WRK-5474E-START-COUNT    PIC  X(001)         VALUE SPACES.
           05 WRK-5474E-CFUNC-BDSCO    PIC  9(009)         VALUE ZEROS.
           05 WRK-5474E-CTERM          PIC  X(008)         VALUE SPACES.
           05 WRK-5474E-FLAG-FILTRO    PIC  X(002)         VALUE SPACES.
           05 WRK-5474E-OPERACAO       PIC  9(013)         VALUE ZEROS.
           05 WRK-5474E-FLAG-TIPO      PIC  X(001)         VALUE SPACES.


       01  WRK-5474-SAIDA.
         03  5474-ROTSAI-HEADER.
           05 WRK-5474S-COD-RETORNO    PIC  X(004)         VALUE SPACES.
           05 WRK-5474S-MSG-RETORNO    PIC  X(079)         VALUE SPACES.
           05 WRK-5474S-RESTART        PIC  9(005)         VALUE ZEROS.
           05 WRK-5474S-FLAG           PIC  X(001)         VALUE SPACES.
           05 WRK-5474S-QTDE-RETORNADA PIC  9(003)         VALUE ZEROS.
           05 WRK-5474S-COUNT          PIC  9(009)         VALUE ZEROS.
           05 WRK-5474S-VTOTAL-PARCELAS  PIC  9(015)V9(02) VALUE ZEROS.
           05 WRK-5474S-VTOTAL-ENCARGOS  PIC  9(015)V9(02) VALUE ZEROS.
         03  5474-ROTSAI-DADOS.
           05 WRK-5474S-TABELA-SAIDA   OCCURS 010 TIMES.
             10 WRK-5474S-NPCELA-DESC  PIC  9(005)         VALUE ZEROS.
             10 WRK-5474S-IPSSOA-SACDO PIC  X(013)         VALUE SPACES.
             10 FILLER                 PIC  X(047)         VALUE SPACES.
             10 WRK-5474S-VPRINC-PCELA PIC  9(015)V99      VALUE ZEROS.
             10 WRK-5474S-DTVCTO-PCELA PIC  X(010)         VALUE SPACES.
             10 WRK-5474S-CSEU-NRO     PIC  X(015)         VALUE SPACES.
             10 WRK-5474S-CSIT-DESC-COML
                                       PIC  9(003)         VALUE ZEROS.

             10 WRK-5474S-DSIT-DESC-COML
                                       PIC  X(020)         VALUE SPACES.
             10 WRK-5474S-DVCTO-PCELA-PARA   PIC  X(010)   VALUE SPACES.
             10 WRK-5474S-PTX-JURO-MES-DE    PIC  9(003)V9(05).
             10 WRK-5474S-PTX-JURO-MES-PARA  PIC  9(003)V9(05).
             10 WRK-5474S-VENCAR-PCELA-PARA  PIC  9(015)V9(2).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DE COMUNICACAO P/ DCOM5472 ***'.
      *----------------------------------------------------------------*
       01  5472-ENTRADA.
           03  5472-HEADER.
               05  5472-ENT-COD-RETORNO              PIC  X(004).
               05  5472-ENT-MSG-RETORNO              PIC  X(079).
               05  5472-ENT-RESTART                  PIC  9(005).
               05  5472-ENT-FLAG                     PIC  X(001).
               05  5472-ENT-CFUNC-BDSCO              PIC  9(009).
               05  5472-ENT-CTERM                    PIC  X(008).
               05  5472-ENT-DANO-OPER-DESC           PIC  9(004).
               05  5472-ENT-NSEQ-OPER-DESC           PIC  9(009).

               05  5472-ENT-NR-PARCELA                PIC 9(005).

       01  5472-SAIDA.
           03  5472-SAI-HEADER.
               05  5472-SAI-COD-RETORNO              PIC  X(004).
               05  5472-SAI-MSG-RETORNO              PIC  X(079).
               05  5472-SAI-RESTART                  PIC  9(005).
               05  5472-SAI-FLAG                     PIC  X(001).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
          '*** AREA DE COMUNICACAO MODULO DCOM4174 ***'.
      *----------------------------------------------------------------*

       COPY 'I#DCOMXJ'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA BRAD7100 ***'.
      *----------------------------------------------------------------*


       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** DCOM1804 - FIM DA AREA DE WORKING ***'.
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

           IF  RETURN-CODE             EQUAL 04 OR 12
               PERFORM 2000-FINALIZAR
           END-IF.

           MOVE SPACES                 TO WRK-OUT-DADOS-RDTM
                                          WRK-INCONSIS.

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

           MOVE 'DCOMRDTM'             TO  WRK-TELA.

           IF  WRK-TRANSACAO           NOT EQUAL 'DCOM1804'
               PERFORM 1100-RECEBER-CONTROLE
           ELSE
               PERFORM 1200-PROCESSAR-DCOMRDTM
           END-IF.

           IF  WRK-TELA                EQUAL 'DCOMRDTM'
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


           MOVE WRK-MENSAGEM           TO WRK-COMU-AREA.

           MOVE WRK-COMU-OPCAO         TO WRK-INP-OPCAO-RDTM
                                          WRK-OUT-OPCAO-RDTM.

           MOVE WRK-COMU-TRANSACAO     TO WRK-CHAMADOR-AUX.

           IF  WRK-COMU-TRANSACAO       EQUAL 'DCOM1805'
               MOVE WRK-COMU-CHAMADOR   TO WRK-INP-CHAMADO-RDTM
                                           WRK-OUT-CHAMADO-RDTM
           ELSE
               MOVE WRK-COMU-TRANSACAO  TO WRK-INP-CHAMADO-RDTM

                                           WRK-OUT-CHAMADO-RDTM
           END-IF

           MOVE WRK-COMU-TIMESTAMP     TO WRK-INP-TMSTAMP-RDTM
                                          WRK-OUT-TMSTAMP-RDTM.
           MOVE WRK-COMU-PAGINA-1      TO WRK-INP-PAGOCUL-RDTM
                                          WRK-OUT-PAGOCUL-RDTM.
           MOVE WRK-COMU-OPERACAO      TO WRK-INP-OPERAC-RDTM
                                          WRK-OUT-OPERAC-RDTM.
           MOVE WRK-COMU-TP-SELECAO    TO WRK-INP-TPSELECAO-RDTM
                                          WRK-OUT-TPSELECAO-RDTM.
           MOVE WRK-COMU-CPRODUTO      TO WRK-INP-PRODOC-RDTM
                                          WRK-OUT-PRODOC-RDTM.
           MOVE WRK-COMU-IPRODUTO      TO WRK-INP-DESPROD-RDTM
                                          WRK-OUT-DESPROD-RDTM.
           MOVE WRK-COMU-CSUBPROD      TO WRK-INP-SBPROC-RDTM
                                          WRK-OUT-SBPROC-RDTM.
           MOVE WRK-COMU-ISUBPROD      TO WRK-INP-DESSUBP-RDTM
                                          WRK-OUT-DESSUBP-RDTM.

                    MOVE WRK-COMU-BANCO   TO WRK-INP-BANCO-RDTM

                                             WRK-OUT-BANCO-RDTM
                    MOVE WRK-COMU-AGENCIA TO WRK-INP-AGENOC-RDTM
                                             WRK-OUT-AGENOC-RDTM
                    MOVE WRK-COMU-CONTA   TO WRK-INP-CONTOC-RDTM
                                             WRK-OUT-CONTOC-RDTM
                    MOVE WRK-COMU-CCNPJ-CPF
                                          TO WRK-INP-CNPJOC-RDTM
                                             WRK-OUT-CNPJOC-RDTM
                    MOVE WRK-COMU-CFLIAL-CNPJ
                                          TO WRK-INP-FILIAL-RDTM
                                             WRK-OUT-FILIAL-RDTM
                    MOVE WRK-COMU-CCTRL-CNPJCPF
                                          TO WRK-INP-CONTROLE-RDTM
                                             WRK-OUT-CONTROLE-RDTM

           PERFORM 1110-OBTER-DADOS-OPERACAO.

           IF  WRK-INCONSIS            EQUAL SPACES
               IF  WRK-COMU-TRANSACAO  EQUAL 'DCOM1805'
                   MOVE WRK-COMU-PAGINA-2 TO WRK-OUT-PAGINA-RDTM
                                             WRK-INP-PAGINA-RDTM

                   COMPUTE WRK-RESTART    = (WRK-COMU-PAGINA-2 - 1) * 9
               ELSE
                   MOVE 1              TO WRK-OUT-PAGINA-RDTM
                                          WRK-INP-PAGINA-RDTM
                   MOVE ZEROS          TO WRK-RESTART
               END-IF

               INITIALIZE WRK-5474-ENTRADA
                          WRK-5474-SAIDA
                          ERRO-AREA

               PERFORM 1140-OBTER-LISTA-PARCELAS

               IF  WRK-5474S-COD-RETORNO  EQUAL '0003' OR
                   WRK-INCONSIS           EQUAL '*'
                   PERFORM 1220-RETORNAR-CHAMADOR
                   IF  WRK-CHAMADOR-AUX   EQUAL 'DCOM1805'
                       MOVE 'EXCLUSAO DA PRORROGACAO EFETUADA COM SUCESS
      -                     'O'           TO WRK-COMU-MENSAGEM
                   ELSE
                       MOVE WRK-5474S-MSG-RETORNO

                                          TO WRK-COMU-MENSAGEM
                   END-IF
                   MOVE WRK-COMU-AREA     TO WRK-MENSAGEM
               ELSE
                   IF  WRK-COMU-MENSAGEM  EQUAL SPACES
                       MOVE WRK-MSG001    TO WRK-OUT-MENSA-RDTM
                   ELSE
                       MOVE WRK-COMU-MENSAGEM
                                          TO WRK-OUT-MENSA-RDTM
                   END-IF
               END-IF
           END-IF.

           MOVE '1'                 TO WRK-OUT-FASE-RDTM.

      *----------------------------------------------------------------*
       1100-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * OBTEM DADOS DA OPERACAO ATRAVES DE ACESSO AO MODULO DCOM4174   *

      *----------------------------------------------------------------*
       1110-OBTER-DADOS-OPERACAO       SECTION.
      *----------------------------------------------------------------*


           MOVE SPACES                 TO WRK-SQLCA
                                          ERRO-AREA.

           INITIALIZE 4174-ENTRADA-ROTEADOR
                      4174-SAIDA-ROTEADOR
                      ERRO-AREA.

           MOVE WRK-COD-USER           TO 4174-ENT-CFUNC-BDSCO.
           MOVE LNK-IO-LTERM           TO 4174-ENT-CTERM.
           MOVE WRK-COMU-DANO-OPER     TO 4174-ENT-DANO-OPER-DESC.
           MOVE WRK-COMU-NSEQ-OPER     TO 4174-ENT-NSEQ-OPER-DESC.
           MOVE 'DCOM4174'             TO WRK-MODULO


           CALL WRK-MODULO             USING 4174-ENTRADA-ROTEADOR
                                             4174-SAIDA-ROTEADOR

                                             ERRO-AREA
                                             WRK-SQLCA.


           EVALUATE 4174-SAI-COD-RETORNO
              WHEN '0000'
                    PERFORM 1120-FORMATAR-CABECALHO

              WHEN '0099'
                    MOVE 'DB2'         TO ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO

              WHEN OTHER
                   MOVE 4174-SAI-MSG-RETORNO
                                       TO WRK-COMU-MENSAGEM
                   MOVE WRK-COMU-AREA  TO WRK-MENSAGEM
                   MOVE '*'            TO WRK-INCONSIS
                   PERFORM 1220-RETORNAR-CHAMADOR
           END-EVALUATE.

      *----------------------------------------------------------------*

       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     FORMATA CABECALHO COM DADOS RETORNADOS NO MODULO DCOM4174  *
      *----------------------------------------------------------------*
       1120-FORMATAR-CABECALHO         SECTION.
      *----------------------------------------------------------------*


           MOVE 4174-SAI-IPSSOA-DESC-COML      TO WRK-OUT-NOMCLIE-RDTM.


           STRING 4174-SAI-CBCO      '/'
                  4174-SAI-CAG-BCRIA '/'
                  4174-SAI-CCTA-BCRIA-CLI
           DELIMITED BY SIZE                 INTO WRK-INP-BCAGCTA-RDTM

           MOVE WRK-INP-BCAGCTA-RDTM           TO WRK-OUT-BCAGCTA-RDTM
           MOVE 4174-SAI-CCNPJ-CPF             TO WRK-INP-CNPJOC-RDTM
                                                  WRK-OUT-CNPJOC-RDTM.


           STRING 4174-SAI-CCNPJ-CPF(1:3) '.'
                  4174-SAI-CCNPJ-CPF(4:3) '.'
                  4174-SAI-CCNPJ-CPF(7:3) '/'
                  4174-SAI-CFLIAL-CNPJ    '-'
                  4174-SAI-CCTRL-CNPJ-CPF
           DELIMITED BY SIZE                 INTO WRK-INP-CNPJCPF-RDTM.

           MOVE WRK-INP-CNPJCPF-RDTM           TO WRK-OUT-CNPJCPF-RDTM
           MOVE 4174-SAI-CPRODT                TO WRK-OUT-CODPROD-RDTM.
           MOVE 4174-SAI-IABREV-PRODT          TO WRK-OUT-DESPROD-RDTM.
           MOVE 4174-SAI-CSPROD-DESC-COML      TO WRK-OUT-CODSUBP-RDTM.
           MOVE 4174-SAI-IRSUMO-SPROD-DESC     TO WRK-OUT-DESSUBP-RDTM.
           MOVE 4174-SAI-IRSUMO-SIT-DESC       TO WRK-OUT-DESSIT-RDTM.

           EVALUATE TRUE
               WHEN 4174-SAI-CCONTR-LIM-DESC      GREATER ZEROS
                    MOVE 'CONTR LIMITE:'          TO WRK-OUT-SIT1-RDTM
                    MOVE 4174-SAI-CCONTR-LIM-DESC TO WRK-OUT-SIT2-RDTM-N

               WHEN 4174-SAI-CCONTR-CONVE-DESC    GREATER ZEROS AND

                    4174-SAI-CELMTO-DESC-COML     EQUAL 8
                    MOVE 'CONV GERAL..:'          TO WRK-OUT-SIT1-RDTM
                    MOVE 4174-SAI-CCONTR-CONVE-DESC
                                                  TO WRK-OUT-SIT2-RDTM-N

               WHEN 4174-SAI-CCONTR-CONVE-DESC    GREATER ZEROS AND
                    4174-SAI-CELMTO-DESC-COML     EQUAL 9
                    MOVE 'CONV CLIENTE:'          TO WRK-OUT-SIT1-RDTM
                    MOVE 4174-SAI-CCONTR-CONVE-DESC
                                                  TO WRK-OUT-SIT2-RDTM-N

           END-EVALUATE.

           IF  4174-SAI-NSMULA-OPER-DESC    GREATER ZEROS
               IF  WRK-OUT-SIT1-RDTM        EQUAL SPACES
                   MOVE 'SIMULACAO...:'     TO WRK-OUT-SIT1-RDTM
                   MOVE 4174-SAI-NSMULA-OPER-DESC
                                            TO WRK-OUT-SIT2-RDTM-N
               ELSE
                   MOVE 'SIMULACAO...:'     TO WRK-OUT-SIT3-RDTM
                   MOVE 4174-SAI-NSMULA-OPER-DESC

                                            TO WRK-OUT-SIT4-RDTM-N
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  VERIFICA CONVENIO                                             *
      *----------------------------------------------------------------*
      *     OBTEM LISTA DE PARCELAS DA OPERACAO ATRAVES DE ACESSO AO   *
      *     MODULO DCOM5474.                                           *
      *----------------------------------------------------------------*
       1140-OBTER-LISTA-PARCELAS       SECTION.
      *----------------------------------------------------------------*


           MOVE SPACES                 TO WRK-SQLCA ERRO-AREA.



           MOVE WRK-COD-USER           TO WRK-5474E-CFUNC-BDSCO.
           MOVE LNK-IO-LTERM           TO WRK-5474E-CTERM.
           MOVE WRK-COD-DEPTO-N        TO WRK-AGENCIA-AUX-N.
           MOVE WRK-AGENCIA-AUX        TO WRK-5474E-MSG-RETORNO(1:5).
           MOVE WRK-INP-OPERAC-RDTM    TO WRK-5474E-OPERACAO.
           MOVE WRK-RESTART            TO WRK-5474E-RESTART.

           EVALUATE WRK-INP-OPCAO-RDTM
               WHEN 1
                    MOVE 'IP'          TO WRK-5474E-FLAG-FILTRO
                    MOVE 'I'           TO WRK-5474E-FLAG-TIPO

               WHEN 2
                    MOVE 'EP'          TO WRK-5474E-FLAG-FILTRO
                    MOVE 'E'           TO WRK-5474E-FLAG-TIPO

               WHEN 3
                    MOVE 'CP'          TO WRK-5474E-FLAG-FILTRO
                    MOVE 'C'           TO WRK-5474E-FLAG-TIPO
           END-EVALUATE.


           MOVE 'DCOM5474'             TO WRK-MODULO.


           CALL WRK-MODULO             USING WRK-5474-ENTRADA
                                             WRK-5474-SAIDA
                                             ERRO-AREA
                                             WRK-SQLCA.


           EVALUATE  WRK-5474S-COD-RETORNO
               WHEN '0000'
                     PERFORM 1150-MONTAR-OCORRENCIAS

               WHEN '0003'
                     MOVE '*'          TO WRK-INCONSIS

               WHEN '0099'
                     MOVE 'DB2'        TO ERR-TIPO-ACESSO
                     PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN  OTHER

                     MOVE WRK-5474S-MSG-RETORNO
                                       TO WRK-OUT-MENSA-RDTM
                     MOVE '*'          TO WRK-INCONSIS
           END-EVALUATE.

      *----------------------------------------------------------------*
       1140-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     FORMATA OCORRENCIAS DA TELA COM DADOS RETORNADOS DO MODULO *
      *     DCOM5474                                                   *
      *----------------------------------------------------------------*
       1150-MONTAR-OCORRENCIAS         SECTION.
      *----------------------------------------------------------------*


           PERFORM VARYING IND-1       FROM 1 BY 1
                     UNTIL IND-1       GREATER 09
                IF WRK-5474S-NPCELA-DESC(IND-1) EQUAL ZEROS
                   MOVE WRK-225

                     TO WRK-OUT-SELEC-ATTR-RDTM     (IND-1)
                ELSE
                   MOVE WRK-49345
                     TO WRK-OUT-SELEC-ATTR-RDTM     (IND-1)
                   MOVE WRK-5474S-NPCELA-DESC       (IND-1)
                     TO WRK-OUT-PARC-RDTM-N         (IND-1)
                   MOVE WRK-5474S-IPSSOA-SACDO      (IND-1)
                     TO WRK-OUT-NOME-RDTM           (IND-1)
                   MOVE WRK-5474S-DTVCTO-PCELA      (IND-1)
                     TO WRK-OUT-DTVCTO-RDTM         (IND-1)
                   INSPECT WRK-OUT-DTVCTO-RDTM      (IND-1)
                            REPLACING  ALL '.'  BY '/'
                   MOVE WRK-5474S-DVCTO-PCELA-PARA  (IND-1)
                     TO WRK-OUT-DTPROR-RDTM         (IND-1)
                   INSPECT WRK-OUT-DTPROR-RDTM      (IND-1)
                            REPLACING  ALL '.'  BY '/'
                   MOVE WRK-5474S-PTX-JURO-MES-DE   (IND-1)
                     TO WRK-OUT-TXORIG-RDTM         (IND-1)
                   MOVE WRK-5474S-PTX-JURO-MES-PARA (IND-1)
                     TO WRK-OUT-TXPROR-RDTM         (IND-1)
                   MOVE WRK-5474S-VPRINC-PCELA      (IND-1)

                     TO WRK-OUT-VALOR-RDTM-N        (IND-1)
                   MOVE WRK-5474S-VENCAR-PCELA-PARA (IND-1)
                     TO WRK-OUT-VLENC-RDTM-N        (IND-1)
                END-IF
           END-PERFORM.

           IF  WRK-5474S-FLAG          EQUAL 'S'
               MOVE '*'                TO WRK-OUT-FIMAMOS-RDTM
               MOVE WRK-MSG004         TO WRK-OUT-MENSA-RDTM
           ELSE
               MOVE SPACES             TO WRK-OUT-FIMAMOS-RDTM
               MOVE WRK-MSG005         TO WRK-OUT-MENSA-RDTM
           END-IF.

      *----------------------------------------------------------------*
       1150-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA PROCESSA A MENSAGEM VINDA DA TELA DCOMDCTM        *
      *----------------------------------------------------------------*

       1200-PROCESSAR-DCOMRDTM         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM             TO WRK-INPUT-RDTM.

           PERFORM 1210-DEVOLVER-TELA

           EVALUATE TRUE
               WHEN WRK-INP-PFK-RDTM     EQUAL 'H' OR
                   (WRK-INP-PFK-RDTM     EQUAL '.' AND
                    WRK-INP-COMANDO-RDTM EQUAL 'PFK01')
                    MOVE WRK-49345       TO WRK-OUT-SELEC-ATTR-RDTM(1)
                    MOVE WRK-INP-MENSA-RDTM
                                         TO WRK-OUT-MENSA-RDTM
                    IF  WRK-INP-FASE-RDTM EQUAL '2'
                        PERFORM 1277-PROTEGE-TELA
                    END-IF

               WHEN WRK-INP-PFK-RDTM     EQUAL '3' OR
                   (WRK-INP-PFK-RDTM     EQUAL '.' AND
                    WRK-INP-COMANDO-RDTM EQUAL 'PFK03')

                    PERFORM 1220-RETORNAR-CHAMADOR

               WHEN WRK-INP-PFK-RDTM     EQUAL '5' OR
                   (WRK-INP-PFK-RDTM     EQUAL '.' AND
                    WRK-INP-COMANDO-RDTM EQUAL 'PFK05')
                    PERFORM 1230-RETORNAR-MENU-ROTINAS

               WHEN (WRK-INP-PFK-RDTM     EQUAL '6' OR
                    (WRK-INP-PFK-RDTM     EQUAL '.' AND
                     WRK-INP-COMANDO-RDTM EQUAL 'PFK06')) AND
                     WRK-INP-FASE-RDTM    EQUAL '2'
                     PERFORM 1235-TRATAR-CONFIRMACAO


               WHEN (WRK-INP-PFK-RDTM     EQUAL '7' OR
                    (WRK-INP-PFK-RDTM     EQUAL '.' AND
                     WRK-INP-COMANDO-RDTM EQUAL 'PFK07')) AND
                     WRK-INP-FASE-RDTM    EQUAL '1'
                     PERFORM 1240-TRATAR-PFK7

               WHEN (WRK-INP-PFK-RDTM     EQUAL '8' OR

                    (WRK-INP-PFK-RDTM     EQUAL '.' AND
                     WRK-INP-COMANDO-RDTM EQUAL 'PFK08')) AND
                     WRK-INP-FASE-RDTM    EQUAL '1'
                     PERFORM 1250-TRATAR-PFK8

               WHEN WRK-INP-PFK-RDTM     EQUAL 'A' OR
                   (WRK-INP-PFK-RDTM     EQUAL '.' AND
                    WRK-INP-COMANDO-RDTM EQUAL 'PFK10')
                    PERFORM 1260-RETORNAR-MENU-DCOM

               WHEN WRK-INP-PFK-RDTM     EQUAL '.' AND
                    WRK-INP-COMANDO-RDTM EQUAL  SPACES OR LOW-VALUES
                    PERFORM 1270-TRATAR-ENTER
                    IF  WRK-INP-SELECP-RDTM   EQUAL  'D'   AND
                        WRK-INCONSIS     EQUAL SPACES
                        PERFORM 2285-DETALHAR-OPERACAO
                    END-IF

               WHEN OTHER
                    MOVE WRK-49345     TO WRK-OUT-SELEC-ATTR-RDTM(1)
                    MOVE WRK-MSG002    TO WRK-OUT-MENSA-RDTM

                    IF  WRK-INP-FASE-RDTM    EQUAL '2'
                        PERFORM 1277-PROTEGE-TELA
                    END-IF
           END-EVALUATE.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA DEVOLVE OS DADOS DA TELA SEM ALTERACAO            *
      *----------------------------------------------------------------*
       1210-DEVOLVER-TELA              SECTION.
      *----------------------------------------------------------------*

           PERFORM 1215-DEVOLVER-TELA-FIXA.

           PERFORM VARYING IND-1       FROM 1 BY 1
                     UNTIL IND-1       GREATER 9
                IF WRK-INP-TABELA-RDTM(IND-1) EQUAL SPACES
                   MOVE WRK-225        TO WRK-OUT-SELEC-ATTR-RDTM(IND-1)

                ELSE
                   MOVE WRK-193
                     TO WRK-OUT-SELEC-ATTR-RDTM(IND-1)
                   MOVE WRK-INP-SELEC-RDTM (IND-1)
                     TO WRK-OUT-SELEC-RDTM (IND-1)
                   MOVE WRK-INP-PARC-RDTM  (IND-1)
                     TO WRK-OUT-PARC-RDTM  (IND-1)
                   MOVE WRK-INP-NOME-RDTM  (IND-1)
                     TO WRK-OUT-NOME-RDTM  (IND-1)
                   MOVE WRK-INP-DTVCTO-RDTM(IND-1)
                     TO WRK-OUT-DTVCTO-RDTM(IND-1)
                   MOVE WRK-INP-DTPROR-RDTM(IND-1)
                     TO WRK-OUT-DTPROR-RDTM(IND-1)
                   MOVE WRK-INP-TXORIG-RDTM(IND-1)
                     TO WRK-OUT-TXORIG-RDTM(IND-1)
                   MOVE WRK-INP-TXPROR-RDTM(IND-1)
                     TO WRK-OUT-TXPROR-RDTM(IND-1)
                   MOVE WRK-INP-VALOR-RDTM(IND-1)
                     TO WRK-OUT-VALOR-RDTM(IND-1)
                   MOVE WRK-INP-VLENC-RDTM(IND-1)
                     TO WRK-OUT-VLENC-RDTM(IND-1)

                END-IF
           END-PERFORM.

      *----------------------------------------------------------------*
       1210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  LIMPA A TELA                                                  *
      *----------------------------------------------------------------*
       1210-1-LIMPA-TELA               SECTION.
      *----------------------------------------------------------------*

           PERFORM VARYING IND-1       FROM 1 BY 1
                     UNTIL IND-1       GREATER 9
             MOVE WRK-193
                     TO WRK-OUT-SELEC-ATTR-RDTM(IND-1)
             MOVE SPACES TO WRK-INP-SELEC-RDTM (IND-1)
                            WRK-OUT-SELEC-RDTM (IND-1)
                            WRK-INP-PARC-RDTM  (IND-1)
                            WRK-OUT-PARC-RDTM  (IND-1)

                            WRK-INP-NOME-RDTM  (IND-1)
                            WRK-OUT-NOME-RDTM  (IND-1)
                            WRK-INP-DTVCTO-RDTM(IND-1)
                            WRK-OUT-DTVCTO-RDTM(IND-1)
                            WRK-INP-DTPROR-RDTM(IND-1)
                            WRK-OUT-DTPROR-RDTM(IND-1)
                            WRK-INP-TXORIG-RDTM-X(IND-1)
                            WRK-OUT-TXORIG-RDTM-X(IND-1)
                            WRK-INP-TXPROR-RDTM-X(IND-1)
                            WRK-OUT-TXPROR-RDTM-X(IND-1)
                            WRK-INP-VALOR-RDTM(IND-1)
                            WRK-OUT-VALOR-RDTM(IND-1)
                            WRK-INP-VLENC-RDTM(IND-1)
                            WRK-OUT-VLENC-RDTM(IND-1)
           END-PERFORM.

      *----------------------------------------------------------------*
       1210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*

      *  ESSA ROTINA DEVOLVE OS DADOS DA TELA SEM ALTERACAO            *
      *----------------------------------------------------------------*
       1215-DEVOLVER-TELA-FIXA         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-PAGINA-RDTM     TO WRK-OUT-PAGINA-RDTM.
           MOVE WRK-INP-OCULTOS-RDTM    TO WRK-OUT-OCULTOS-RDTM.
           MOVE WRK-INP-NOMCLIE-RDTM    TO WRK-OUT-NOMCLIE-RDTM.
           MOVE WRK-INP-CNPJCPF-RDTM    TO WRK-OUT-CNPJCPF-RDTM.
           MOVE WRK-INP-BCAGCTA-RDTM    TO WRK-OUT-BCAGCTA-RDTM.
           MOVE WRK-INP-PRODUTO-RDTM    TO WRK-OUT-PRODUTO-RDTM.
           MOVE WRK-INP-SUBPRODUTO-RDTM TO WRK-OUT-SUBPRODUTO-RDTM.
           MOVE WRK-INP-OPERAC-RDTM     TO WRK-OUT-OPERAC-RDTM.
           MOVE WRK-INP-DESSIT-RDTM     TO WRK-OUT-DESSIT-RDTM.
           MOVE WRK-INP-SIT1-RDTM       TO WRK-OUT-SIT1-RDTM.
           MOVE WRK-INP-SIT2-RDTM       TO WRK-OUT-SIT2-RDTM.
           MOVE WRK-INP-SIT3-RDTM       TO WRK-OUT-SIT3-RDTM.
           MOVE WRK-INP-SIT4-RDTM       TO WRK-OUT-SIT4-RDTM.

      *----------------------------------------------------------------*
       1215-99-FIM.                    EXIT.

      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA RETORNA AO PROGRAMA CHAMADOR                      *
      *----------------------------------------------------------------*
       1220-RETORNAR-CHAMADOR          SECTION.
      *----------------------------------------------------------------*

           PERFORM 1225-FORMATAR-COMU-AREA.

           MOVE WRK-INP-CHAMADO-RDTM   TO WRK-TELA
           MOVE WRK-INP-PFK-RDTM       TO WRK-COMU-PFK

           MOVE WRK-CHNG               TO WRK-FUNCAO.
           MOVE WRK-COMU-AREA          TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*

      *  FORMATA AREA DE COMUNICACAO                                   *
      *----------------------------------------------------------------*
       1225-FORMATAR-COMU-AREA         SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-COMU-AREA.

           MOVE LENGTH                 OF WRK-COMU-AREA
                                       TO WRK-COMU-LL.
           MOVE WRK-INP-OPCAO-RDTM     TO WRK-COMU-OPCAO.
           MOVE WRK-INP-TMSTAMP-RDTM   TO WRK-COMU-TIMESTAMP.
           MOVE WRK-INP-PAGOCUL-RDTM   TO WRK-COMU-PAGINA-1.
           MOVE WRK-INP-PAGINA-RDTM    TO WRK-COMU-PAGINA-2.
           MOVE WRK-INP-TPSELECAO-RDTM TO WRK-COMU-TP-SELECAO.
BSI001
BSI001              MOVE WRK-INP-BANCO-RDTM  TO WRK-COMU-BANCO
BSI001              MOVE WRK-INP-AGENOC-RDTM TO WRK-COMU-AGENCIA
BSI001              MOVE WRK-INP-CONTOC-RDTM TO WRK-COMU-CONTA
BSI001              MOVE WRK-INP-CNPJOC-RDTM TO WRK-COMU-CCNPJ-CPF
BSI001              MOVE WRK-INP-FILIAL-RDTM TO WRK-COMU-CFLIAL-CNPJ
BSI001              MOVE WRK-INP-CONTROLE-RDTM

BSI001                                       TO WRK-COMU-CCTRL-CNPJCPF
BSI001
           MOVE WRK-INP-OPERAC-RDTM    TO WRK-COMU-OPERACAO.
           MOVE 237                    TO WRK-COMU-BANCO.
           MOVE WRK-INP-AGENCIA-RDTM   TO WRK-COMU-AGENCIA.

           MOVE WRK-INP-PRODOC-RDTM    TO WRK-COMU-CPRODUTO.
           MOVE WRK-INP-SBPROC-RDTM    TO WRK-COMU-CSUBPROD.
           MOVE WRK-INP-DESPROD-RDTM   TO WRK-COMU-IPRODUTO.
           MOVE WRK-INP-DESSUBP-RDTM   TO WRK-COMU-ISUBPROD.
           MOVE 'DCOM1804'             TO WRK-COMU-TRANSACAO.
           MOVE WRK-INP-CHAMADO-RDTM   TO WRK-COMU-CHAMADOR.

      *----------------------------------------------------------------*
       1225-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRATAMENTO DE RETORNO AO MENU ROTINAS    *
      *----------------------------------------------------------------*
       1230-RETORNAR-MENU-ROTINAS      SECTION.

      *----------------------------------------------------------------*


           INITIALIZE WRK-COMU-AREA.

           MOVE LENGTH                 OF WRK-COMU-AREA
                                       TO WRK-COMU-LL.
           MOVE WRK-CHNG               TO WRK-FUNCAO.
           MOVE 'DCOM1804'             TO WRK-TELA
                                          WRK-COMU-TRANSACAO.
           MOVE '5'                    TO WRK-COMU-PFK.
           MOVE WRK-COMU-AREA          TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1235-TRATAR-CONFIRMACAO         SECTION.
      *----------------------------------------------------------------*


           MOVE SPACES                 TO WRK-INCONSIS.

           PERFORM
           VARYING IND-1               FROM 1 BY 1
             UNTIL IND-1               GREATER 9
                OR WRK-INCONSIS        EQUAL '*'
                IF WRK-INP-SELEC-RDTM (IND-1)
                                       EQUAL 'E'
                   PERFORM 1271-ACESSAR-DCOM5335
                   MOVE '1'            TO WRK-INP-FASE-RDTM
                END-IF
           END-PERFORM.

           IF  WRK-INCONSIS            EQUAL SPACES
               PERFORM
               VARYING IND-1           FROM 1 BY 1
                 UNTIL IND-1           GREATER 9
                    IF WRK-INP-SELEC-RDTM(IND-1) EQUAL 'E'
                       PERFORM 1280-TRATAR-EXCLUIR-PAG
                    END-IF
               END-PERFORM


               IF  WRK-INCONSIS        NOT EQUAL SPACES
                   GO TO 1235-99-FIM
               END-IF

               PERFORM 1210-1-LIMPA-TELA

               MOVE '1'                TO WRK-INP-FASE-RDTM

               COMPUTE WRK-RESTART     = (WRK-INP-PAGINA-RDTM - 1) * 9

               PERFORM 1140-OBTER-LISTA-PARCELAS

               IF  WRK-INCONSIS        EQUAL SPACES
                   MOVE 'EXCLUSAO EFETUADA COM SUCESSO'
                                       TO WRK-OUT-MENSA-RDTM
               ELSE
                   PERFORM 1220-RETORNAR-CHAMADOR
                   MOVE  'EXCLUSAO EFETUADA COM SUCESSO'
                                       TO WRK-COMU-MENSAGEM
                   MOVE WRK-COMU-AREA  TO WRK-MENSAGEM

               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1235-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     TRATAMENTO DE RETORNO DE PAGINA                            *
      *----------------------------------------------------------------*
       1240-TRATAR-PFK7                SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INP-PAGINA-RDTM     EQUAL 1
               PERFORM 1210-DEVOLVER-TELA
               MOVE WRK-49345          TO WRK-OUT-SELEC-ATTR-RDTM(1)
               MOVE WRK-MSG006         TO WRK-OUT-MENSA-RDTM
               GO TO 1240-99-FIM
           END-IF.

           PERFORM 1215-DEVOLVER-TELA-FIXA.


           MOVE WRK-49345              TO WRK-OUT-SELEC-ATTR-RDTM(1).

           COMPUTE WRK-RESTART         = (WRK-INP-PAGINA-RDTM - 2) * 9.

           PERFORM 1140-OBTER-LISTA-PARCELAS.

           IF  WRK-5474S-COD-RETORNO   NOT EQUAL '0000'
               PERFORM 1210-DEVOLVER-TELA
               MOVE WRK-5474S-MSG-RETORNO
                                       TO WRK-OUT-MENSA-RDTM
           ELSE
               SUBTRACT 1              FROM WRK-OUT-PAGINA-RDTM
           END-IF.

           MOVE WRK-49345              TO WRK-OUT-SELEC-ATTR-RDTM(1).

      *----------------------------------------------------------------*
       1240-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      *----------------------------------------------------------------*
      *     TRATAMENTO DE AVANCAR PAGINA                               *
      *----------------------------------------------------------------*
       1250-TRATAR-PFK8                SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INP-FIMAMOS-RDTM    EQUAL '*'
               PERFORM 1210-DEVOLVER-TELA
               MOVE WRK-49345          TO WRK-OUT-SELEC-ATTR-RDTM(1)
               MOVE WRK-MSG007         TO WRK-OUT-MENSA-RDTM
               GO TO 1250-99-FIM
           END-IF.

           PERFORM 1215-DEVOLVER-TELA-FIXA.

           COMPUTE WRK-RESTART      =  WRK-INP-PAGINA-RDTM * 9.

           PERFORM 1140-OBTER-LISTA-PARCELAS.

           IF  WRK-5474S-COD-RETORNO   NOT EQUAL '0000'
               PERFORM 1210-DEVOLVER-TELA

               MOVE WRK-5474S-MSG-RETORNO
                                       TO WRK-OUT-MENSA-RDTM
           ELSE
               ADD 1                   TO WRK-OUT-PAGINA-RDTM
           END-IF.

           MOVE WRK-49345              TO WRK-OUT-SELEC-ATTR-RDTM(1).

      *----------------------------------------------------------------*
       1250-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRATAMENTO DE RETORNO AO MENU DCOM       *
      *----------------------------------------------------------------*
       1260-RETORNAR-MENU-DCOM         SECTION.
      *----------------------------------------------------------------*


           INITIALIZE WRK-COMU-AREA.


           MOVE LENGTH                 OF WRK-COMU-AREA
                                       TO WRK-COMU-LL.
           MOVE WRK-CHNG               TO WRK-FUNCAO.
           MOVE 'DCOM1804'             TO WRK-TELA
                                          WRK-COMU-TRANSACAO.
           MOVE 'A'                    TO WRK-COMU-PFK.
           MOVE WRK-COMU-AREA          TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1260-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1270-TRATAR-ENTER               SECTION.
      *----------------------------------------------------------------*

           PERFORM 1210-DEVOLVER-TELA.

           IF  WRK-INP-FASE-RDTM        EQUAL '2'
               MOVE '1'                 TO  WRK-OUT-FASE-RDTM
               MOVE WRK-MSG001          TO  WRK-OUT-MENSA-RDTM

               GO TO 1270-99-FIM
           END-IF.

           PERFORM VARYING IND-1 FROM 1 BY 1 UNTIL IND-1 GREATER 9
             IF  WRK-INP-SELEC-RDTM(IND-1) NOT EQUAL 'D' AND 'E'
                                                 AND ' ' AND LOW-VALUES
                 MOVE '*'        TO WRK-INCONSIS
                 MOVE WRK-49353  TO WRK-OUT-SELEC-ATTR-RDTM(IND-1)
                 MOVE WRK-MSG011
                                 TO  WRK-OUT-MENSA-RDTM
             END-IF
           END-PERFORM.

           IF WRK-INCONSIS        EQUAL  '*'
              GO TO 1270-99-FIM
           END-IF.

           MOVE    ZEROS         TO WRK-QTD-OPCAO-D
                                    WRK-SELECAO
           MOVE    SPACES        TO WRK-SELEC-D
           MOVE    SPACES        TO WRK-SELEC-E


           PERFORM VARYING IND-1 FROM 1 BY 1 UNTIL IND-1 GREATER 9

                IF WRK-INP-SELEC-RDTM (IND-1) EQUAL 'D'
                   MOVE IND-1                     TO WRK-SELECAO
                   MOVE WRK-INP-SELEC-RDTM(IND-1) TO WRK-SELEC-D
                   IF  WRK-SELEC-E  EQUAL 'E'
                       MOVE '*'         TO WRK-INCONSIS
                       MOVE WRK-49353  TO WRK-OUT-SELEC-ATTR-RDTM(IND-1)
                   END-IF
                END-IF
                IF WRK-INP-SELEC-RDTM (IND-1) EQUAL 'E'
                   MOVE  WRK-INP-SELEC-RDTM(IND-1)   TO WRK-SELEC-E
                   IF  WRK-SELEC-D  EQUAL 'D'
                       MOVE '*'         TO WRK-INCONSIS
                       MOVE WRK-49353  TO WRK-OUT-SELEC-ATTR-RDTM(IND-1)
                   END-IF
                END-IF
           END-PERFORM.

           IF WRK-INCONSIS             EQUAL '*'

              MOVE WRK-MSG009          TO WRK-OUT-MENSA-RDTM
              GO TO 1270-99-FIM
           END-IF

           PERFORM VARYING IND-1 FROM 1 BY 1 UNTIL IND-1 GREATER 9
             IF  WRK-INP-SELEC-RDTM(IND-1) NOT EQUAL SPACES
                 ADD  1             TO WRK-QTD-OPCAO-D
                 IF WRK-QTD-OPCAO-D   GREATER 1 AND
                    WRK-SELEC-D       EQUAL  'D'
                    MOVE WRK-49353  TO WRK-OUT-SELEC-ATTR-RDTM(IND-1)
                    MOVE '*'        TO WRK-INCONSIS
                    MOVE 'SELECIONAR APENAS UMA OCORRENCIA COM "D"'
                                    TO WRK-OUT-MENSA-RDTM
                    GO TO 1270-99-FIM
                 END-IF
             END-IF
           END-PERFORM.

           IF WRK-QTD-OPCAO-D   EQUAL  0
              MOVE '*'        TO WRK-INCONSIS
              MOVE WRK-MSG001       TO  WRK-OUT-MENSA-RDTM

              GO TO 1270-99-FIM
           END-IF.

           IF  WRK-SELEC-D     EQUAL   'D'
               MOVE WRK-SELEC-D     TO  WRK-INP-SELECP-RDTM
                                        WRK-OUT-SELECP-RDTM
           END-IF.

           IF  WRK-SELEC-E             EQUAL  'E'
               PERFORM
               VARYING IND-1           FROM 1 BY 1
                 UNTIL IND-1           GREATER 9
                    IF WRK-INP-SELEC-RDTM(IND-1) EQUAL 'E'
                       PERFORM 1271-ACESSAR-DCOM5335
                       IF  WRK-INCONSIS          EQUAL '*'
                           GO TO 1270-99-FIM
                       END-IF
                    END-IF
               END-PERFORM
           END-IF.


           IF  WRK-SELEC-E     EQUAL   'E'
               PERFORM VARYING IND-1 FROM 1 BY 1 UNTIL IND-1
                 GREATER 9
                 IF  WRK-INP-SELEC-RDTM(IND-1) NOT EQUAL SPACES
                     MOVE WRK-225   TO WRK-OUT-SELEC-ATTR-RDTM(IND-1)
                 END-IF

               END-PERFORM
               MOVE WRK-SELEC-E     TO  WRK-INP-SELECT-RDTM
                                        WRK-OUT-SELECT-RDTM
               MOVE 'TECLE <PF6> P/ CONFIRMAR OU <ENTER> P/ ALTERAR'
                                    TO  WRK-OUT-MENSA-RDTM
           END-IF.

           PERFORM 1277-PROTEGE-TELA.

           MOVE '2'                    TO WRK-INP-FASE-RDTM
                                          WRK-OUT-FASE-RDTM.

      *----------------------------------------------------------------*
       1270-99-FIM.                    EXIT.

      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1271-ACESSAR-DCOM5335               SECTION.
      *----------------------------------------------------------------*

           INITIALIZE 5335-ENTRADA
                      5335-SAIDA.

           MOVE WRK-INP-OPERAC-RDTM    TO 5335E-OPERACAO.
           MOVE WRK-INP-PARC-RDTM-N   (IND-1)
                                       TO 5335E-NPCELA-DESC-COML.
           MOVE 007                    TO 5335E-CELMTO-DESC-COML.
           MOVE 008                    TO 5335E-CEVNTO-DESC-COML
           MOVE 'E'                    TO 5335E-ACAO.
           MOVE 'DCOM5335'             TO WRK-MODULO.

           CALL WRK-MODULO             USING 5335-ENTRADA
                                             5335-SAIDA
                                             ERRO-AREA
                                             WRK-SQLCA



           EVALUATE  5335S-COD-RETORNO
               WHEN '0000'
                     CONTINUE

               WHEN '0099'
                     MOVE 'DB2'        TO ERR-TIPO-ACESSO
                     PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN  OTHER
                     MOVE 5335S-MSG-RETORNO
                                       TO WRK-OUT-MENSA-RDTM
                     MOVE WRK-49353    TO WRK-OUT-SELEC-ATTR-RDTM(IND-1)
                     MOVE '*'          TO WRK-INCONSIS
           END-EVALUATE.

      *----------------------------------------------------------------*
       1271-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      *----------------------------------------------------------------*
       1277-PROTEGE-TELA                   SECTION.
      *----------------------------------------------------------------*

           PERFORM VARYING IND-1     FROM 1 BY 1
                     UNTIL IND-1     GREATER 9
                      MOVE WRK-225     TO WRK-OUT-SELEC-ATTR-RDTM
                                         (IND-1)
           END-PERFORM.

           MOVE WRK-49345              TO WRK-OUT-COMANDO-ATTR-RDTM.

      *----------------------------------------------------------------*
       1277-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1280-TRATAR-EXCLUIR-PAG            SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-COD-USER               TO 5472-ENT-CFUNC-BDSCO

           MOVE LNK-IO-LTERM               TO 5472-ENT-CTERM

           MOVE WRK-INP-OPERAC-RDTM(1:4)   TO 5472-ENT-DANO-OPER-DESC
           MOVE WRK-INP-OPERAC-RDTM(5:9)   TO 5472-ENT-NSEQ-OPER-DESC

           IF WRK-INP-SELEC-RDTM(IND-1) NOT EQUAL SPACES AND
                                                    LOW-VALUES
              MOVE ZEROS      TO  WRK-OUT-SELEC-ATTR-RDTM(IND-1)
              MOVE WRK-INP-PARC-RDTM-N(IND-1)
                              TO  5472-ENT-NR-PARCELA
           ELSE
              GO TO 1280-99-FIM
           END-IF.

           MOVE 'DCOM5472'             TO WRK-MODULO

           CALL WRK-MODULO             USING 5472-ENTRADA
                                             5472-SAIDA
                                             ERRO-AREA
                                             WRK-SQLCA.


           EVALUATE 5472-SAI-COD-RETORNO
             WHEN '0000'
                MOVE SPACES            TO WRK-INCONSIS

             WHEN '0099'
                   MOVE 'DB2'         TO ERR-TIPO-ACESSO
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO

             WHEN OTHER
                   MOVE '1'               TO WRK-OUT-FASE-RDTM
                   MOVE 5472-SAI-MSG-RETORNO
                                      TO WRK-OUT-MENSA-RDTM
                   MOVE '*'           TO WRK-INCONSIS

           END-EVALUATE.

      *----------------------------------------------------------------*
       1280-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*

      *     PASSA CONTROLE AO PROGRAMA DE ADITAMENTO DE PARCELA        *
      *----------------------------------------------------------------*
       2285-DETALHAR-OPERACAO          SECTION.
      *----------------------------------------------------------------*

           PERFORM 1225-FORMATAR-COMU-AREA.

           MOVE WRK-INP-PARC-RDTM-N(WRK-SELECAO)
                                       TO WRK-COMU-PARCELA.
           MOVE 'DCOM1805'             TO WRK-TELA.
           MOVE WRK-CHNG               TO WRK-FUNCAO.
           MOVE WRK-COMU-AREA          TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1285-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA CARREGA A TELA QUE SERA DEMONSTRADA PELO IMS.     *
      *----------------------------------------------------------------*
       1300-PROCESSAR-BRAD0660         SECTION.

      *----------------------------------------------------------------*

           CALL 'BRAD0660'             USING WRK-OUTPUT-RDTM
                                             WRK-660-DCOMRDTM.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE RETURN-CODE        TO WRK-RETURN-CODE
               MOVE 10                 TO WRK-LOCAL-ERRO
               MOVE WRK-ERRO-BRAD0660  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-ISRT               TO WRK-FUNCAO.
           MOVE WRK-OUTPUT-RDTM        TO WRK-MENSAGEM.

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

           IF  ERR-MODULO              NOT EQUAL SPACES AND LOW-VALUES
               MOVE 'DCOM1804'         TO ERR-PGM
           ELSE
               MOVE 'SENHAS02'         TO ERR-PGM

               MOVE 'DCOM1804'         TO ERR-MODULO
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

           PERFORM 2000-FINALIZAR.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.

      *----------------------------------------------------------------*
