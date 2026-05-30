      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. DCOM1806.
       AUTHOR.     ALTRAN.
      *================================================================*
      *                          A L T R A N                           *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   DCOM1806                                    *
      *    DATA........:   25/08/2007                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   CONSULTAR PRORROGACAO VENCTO  - SELECAO DA  *
      *                    PARCELA.                                    *
      *----------------------------------------------------------------*
      *    TELAS.......:                                               *
      *    DCOMRFTM - CONSULTAR PRORROGACAO VENCTO  - SELECAO PARCELA  *
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
           '*** DCOM1806 - INICIO DA AREA DE WORKING ***'.
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
           'SELECIONE UMA PARCELA COM "X" PARA DETALHAR E TECLE <ENTER>'
           .
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
       77  WRK-MSG008                  PIC  X(079)         VALUE
           'SELECIONE APENAS UMA PARCELA COM "X" E TECLE <ENTER>'.
       77  WRK-MSG009                  PIC  X(079)         VALUE
           'SELECIONE PARCELA APENAS COM "X"'.
       77  WRK-MSG010                  PIC  X(079)         VALUE
           'NAO HA PARCELA(S) PARA O(S) PARAMETRO(S) INFORMADO(S)'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*


       77  IND-1                       PIC  9(002) COMP-3  VALUE ZEROS.

       77  WRK-SEL-X                   PIC  9(002) COMP-3  VALUE ZEROS.
       77  WRK-SELECAO                 PIC  9(002) COMP-3  VALUE ZEROS.
       77  WRK-RESTART                 PIC  9(005) COMP-3  VALUE ZEROS.
       77  WRK-SQLCA                   PIC  X(136)         VALUE SPACES.
       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-INCONSIS                PIC  X(001)         VALUE SPACES.

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
           '*** AREA DA TELA DCOMRFTM  - INPUT ***'.
      *----------------------------------------------------------------*

       01  WRK-INPUT-RFTM.
           05 FILLER                   PIC  X(018)         VALUE SPACES.
           05 WRK-INP-PFK-RFTM         PIC  X(001)         VALUE SPACES.
           05 WRK-INP-DADOS-RFTM.

             10 WRK-INP-SENHAS-RFTM    PIC  X(037)         VALUE SPACES.
             10 WRK-INP-COMANDO-RFTM   PIC  X(068)         VALUE SPACES.
             10 WRK-INP-PAGINA-RFTM    PIC  9(004)         VALUE ZEROS.
             10 WRK-INP-OCULTOS-RFTM.
               15 WRK-INP-OPCAO-RFTM    PIC  X(001)        VALUE SPACES.
               15 WRK-INP-TMSTAMP-RFTM  PIC  X(026)        VALUE SPACES.
               15 WRK-INP-FIMAMOS-RFTM  PIC  X(001)        VALUE SPACES.
               15 WRK-INP-PAGOCUL-RFTM  PIC  9(004)        VALUE ZEROS.
4S2511*        15 WRK-INP-CNPJOC-RFTM   PIC  9(009)        VALUE ZEROS.
4S2511         15 WRK-INP-CNPJOC-RFTM   PIC  X(09)         VALUE SPACES.
               15 WRK-INP-PRODOC-RFTM   PIC  9(003)        VALUE ZEROS.
               15 WRK-INP-SBPROC-RFTM   PIC  9(003)        VALUE ZEROS.
               15 WRK-INP-TPSELECAO-RFTM
                                        PIC  9(001)        VALUE ZEROS.
               15 WRK-INP-AGENOC-RFTM   PIC  9(005)        VALUE ZEROS.
               15 WRK-INP-CONTOC-RFTM   PIC  9(013)        VALUE ZEROS.
               15 WRK-INP-CHAMADO-RFTM  PIC  X(008)        VALUE SPACES.
             10 WRK-INP-NOMCLIE-RFTM    PIC  X(040)        VALUE SPACES.
             10 WRK-INP-CNPJCPF-RFTM.
4S2511*        15 WRK-INP-CCNPJCPF-RFTM PIC  ZZZ.ZZZ.ZZ9   VALUE ZEROS.
4S2511         15 WRK-INP-CCNPJCPF-RFTM PIC  X(11)         VALUE SPACES.
               15 FILLER                PIC  X(001)        VALUE SPACES.
4S2511*        15 WRK-INP-FILIAL-RFTM   PIC  9(004)        VALUE ZEROS.
4S2511         15 WRK-INP-FILIAL-RFTM   PIC  X(04)         VALUE SPACES.
4S2511         15 WRK-INP-FILIAL-RFTM-ST PIC  X(04)        VALUE SPACES.

               15 FILLER                PIC  X(001)        VALUE SPACES.
               15 WRK-INP-CONTROLE-RFTM PIC  9(002)        VALUE ZEROS.
             10 WRK-INP-BCAGCTA-RFTM.
               15 WRK-INP-BANCO-RFTM    PIC  9(003)        VALUE ZEROS.
               15 FILLER                PIC  X(001)        VALUE SPACES.
               15 WRK-INP-AGENCIA-RFTM  PIC  9(005)        VALUE ZEROS.
               15 FILLER                PIC  X(001)        VALUE SPACES.
               15 WRK-INP-CONTA-RFTM    PIC  9(013)        VALUE ZEROS.
             10 WRK-INP-PRODUTO-RFTM.
               15 WRK-INP-CODPROD-RFTM  PIC  9(003)        VALUE ZEROS.
               15 WRK-INP-DESPROD-RFTM  PIC  X(012)        VALUE SPACES.
             10 WRK-INP-SUBPRODUTO-RFTM.
               15 WRK-INP-CODSUBP-RFTM  PIC  9(003)        VALUE ZEROS.
               15 WRK-INP-DESSUBP-RFTM  PIC  X(012)        VALUE SPACES.
             10 WRK-INP-OPERAC-RFTM     PIC  9(013)        VALUE ZEROS.
             10 FILLER REDEFINES        WRK-INP-OPERAC-RFTM.
               15 WRK-INP-DANO-RFTM     PIC  9(004).
               15 WRK-INP-NSEQ-RFTM     PIC  9(009).
             10 WRK-INP-DESSIT-RFTM     PIC  X(015)        VALUE SPACES.
             10 WRK-INP-SIT1-RFTM       PIC  X(013)        VALUE SPACES.
             10 WRK-INP-SIT2-RFTM.

               15 WRK-INP-SIT2-RFTM-N   PIC  9(009)        VALUE ZEROS.
             10 WRK-INP-SIT3-RFTM       PIC  X(013)        VALUE SPACES.
             10 WRK-INP-SIT4-RFTM.
               15 WRK-INP-SIT4-RFTM-N   PIC  9(009)        VALUE ZEROS.
             10 WRK-INP-TABELA-RFTM     OCCURS 009 TIMES.
               15 WRK-INP-SELEC-RFTM    PIC  X(001)        VALUE SPACES.
               15 WRK-INP-PARC-RFTM.
                 20 WRK-INP-PARC-RFTM-N PIC  9(003)        VALUE ZEROS.
               15 WRK-INP-NOME-RFTM     PIC  X(013)        VALUE SPACES.
               15 WRK-INP-DTVCTO-RFTM   PIC  X(010)        VALUE SPACES.
               15 WRK-INP-DTPROR-RFTM   PIC  X(010)        VALUE SPACES.
               15 WRK-INP-TXORIG-RFTM   PIC  ZZ9,99        VALUE ZEROS.
               15 WRK-INP-TXPROR-RFTM   PIC  ZZ9,99        VALUE ZEROS.
               15 WRK-INP-VALOR-RFTM.
                 20 WRK-INP-VALOR-RFTM-N
                                        PIC Z.ZZZ.ZZ9,99   VALUE ZEROS.
               15 WRK-INP-VLENC-RFTM.
                 20 WRK-INP-VLENC-RFTM-N
                                        PIC ZZZ.ZZ9,99     VALUE ZEROS.
             10 WRK-INP-MENSA-RFTM      PIC  X(079)        VALUE SPACES.


      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA TELA DCOMRFTM  - OUTPUT ***'.
      *----------------------------------------------------------------*

       01  WRK-OUTPUT-RFTM.
           05 WRK-OUT-LL-RFTM           PIC S9(004) COMP   VALUE +1109.
           05 WRK-OUT-ZZ-RFTM           PIC  9(004) COMP   VALUE ZEROS.
           05 WRK-OUT-DADOS-RFTM.
             10 WRK-OUT-SENHAS-RFTM     PIC  X(037)        VALUE SPACES.
             10 WRK-OUT-COMANDO-ATTR-RFTM
                                        PIC  9(004) COMP   VALUE ZEROS.
             10 WRK-OUT-COMANDO-RFTM    PIC  X(068)        VALUE SPACES.
             10 WRK-OUT-PAGINA-RFTM     PIC  9(004)        VALUE ZEROS.
             10 WRK-OUT-OCULTOS-RFTM.
               15 WRK-OUT-OPCAO-RFTM    PIC  X(001)        VALUE SPACES.
               15 WRK-OUT-TMSTAMP-RFTM  PIC  X(026)        VALUE SPACES.
               15 WRK-OUT-FIMAMOS-RFTM  PIC  X(001)        VALUE SPACES.
               15 WRK-OUT-PAGOCUL-RFTM  PIC  9(004)        VALUE ZEROS.
4S2511*        15 WRK-OUT-CNPJOC-RFTM   PIC  9(009)        VALUE ZEROS.
4S2511         15 WRK-OUT-CNPJOC-RFTM   PIC  X(09)         VALUE SPACES.
               15 WRK-OUT-PRODOC-RFTM   PIC  9(003)        VALUE ZEROS.

               15 WRK-OUT-SBPROC-RFTM   PIC  9(003)        VALUE ZEROS.
               15 WRK-OUT-TPSELECAO-RFTM
                                        PIC  9(001)        VALUE ZEROS.
               15 WRK-OUT-AGENOC-RFTM   PIC  9(005)        VALUE ZEROS.
               15 WRK-OUT-CONTOC-RFTM   PIC  9(013)        VALUE ZEROS.
               15 WRK-OUT-CHAMADO-RFTM  PIC  X(008)        VALUE SPACES.
             10 WRK-OUT-NOMCLIE-RFTM    PIC  X(040)        VALUE SPACES.
             10 WRK-OUT-CNPJCPF-RFTM.
4S2511*        15 WRK-OUT-CCNPJCPF-RFTM PIC  ZZZ.ZZZ.ZZ9   VALUE ZEROS.
4S2511         15 WRK-OUT-CCNPJCPF-RFTM PIC  X(11)         VALUE SPACES.
               15 FILLER                PIC  X(001)        VALUE SPACES.
4S2511*        15 WRK-OUT-FILIAL-RFTM   PIC  9(004)        VALUE ZEROS.
4S2511         15 WRK-OUT-FILIAL-RFTM   PIC  X(04)         VALUE SPACES.
               15 FILLER                PIC  X(001)        VALUE SPACES.
               15 WRK-OUT-CONTROLE-RFTM PIC  9(002)        VALUE ZEROS.
             10 WRK-OUT-BCAGCTA-RFTM.
               15 WRK-OUT-BANCO-RFTM    PIC  9(003)        VALUE ZEROS.
               15 FILLER                PIC  X(001)        VALUE SPACES.
               15 WRK-OUT-AGENCIA-RFTM  PIC  9(005)        VALUE ZEROS.
               15 FILLER                PIC  X(001)        VALUE SPACES.
               15 WRK-OUT-CONTA-RFTM    PIC  9(013)        VALUE ZEROS.
             10 WRK-OUT-PRODUTO-RFTM.
               15 WRK-OUT-CODPROD-RFTM  PIC  9(003)        VALUE ZEROS.

               15 WRK-OUT-DESPROD-RFTM  PIC  X(012)        VALUE SPACES.
             10 WRK-OUT-SUBPRODUTO-RFTM.
               15 WRK-OUT-CODSUBP-RFTM  PIC  9(003)        VALUE ZEROS.
               15 WRK-OUT-DESSUBP-RFTM  PIC  X(012)        VALUE SPACES.
             10 WRK-OUT-OPERAC-RFTM     PIC  9(013)        VALUE ZEROS.
             10 FILLER REDEFINES        WRK-OUT-OPERAC-RFTM.
               15 WRK-OUT-DANO-RFTM     PIC  9(004).
               15 WRK-OUT-NSEQ-RFTM     PIC  9(009).
             10 WRK-OUT-DESSIT-RFTM     PIC  X(015)        VALUE SPACES.
             10 WRK-OUT-SIT1-RFTM       PIC  X(013)        VALUE SPACES.
             10 WRK-OUT-SIT2-RFTM.
               15 WRK-OUT-SIT2-RFTM-N   PIC  9(009)        VALUE ZEROS.
             10 WRK-OUT-SIT3-RFTM       PIC  X(013)        VALUE SPACES.
             10 WRK-OUT-SIT4-RFTM.
               15 WRK-OUT-SIT4-RFTM-N   PIC  9(009)        VALUE ZEROS.
             10 WRK-OUT-TABELA-RFTM     OCCURS 009 TIMES.
               15 WRK-OUT-SELEC-ATTR-RFTM
                                        PIC S9(004) COMP   VALUE ZEROS.
               15 WRK-OUT-SELEC-RFTM    PIC  X(001)        VALUE SPACES.
               15 WRK-OUT-PARC-RFTM.
                 20 WRK-OUT-PARC-RFTM-N PIC  9(003)        VALUE ZEROS.

               15 WRK-OUT-NOME-RFTM     PIC  X(013)        VALUE SPACES.
               15 WRK-OUT-DTVCTO-RFTM   PIC  X(010)        VALUE SPACES.
               15 WRK-OUT-DTPROR-RFTM   PIC  X(010)        VALUE SPACES.
               15 WRK-OUT-TXORIG-RFTM   PIC  ZZ9,99        VALUE ZEROS.
               15 WRK-OUT-TXPROR-RFTM   PIC  ZZ9,99        VALUE ZEROS.
               15 WRK-OUT-VALOR-RFTM.
                 20 WRK-OUT-VALOR-RFTM-N
                                        PIC Z.ZZZ.ZZ9,99   VALUE ZEROS.
               15 WRK-OUT-VLENC-RFTM.
                 20 WRK-OUT-VLENC-RFTM-N
                                        PIC ZZZ.ZZ9,99     VALUE ZEROS.
             10 WRK-OUT-MENSA-RFTM      PIC  X(079)        VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA TELA DCOMRFTM  - BRAD0660 ***'.
      *----------------------------------------------------------------*

       01  WRK-660-DCOMRFTM.
           05 WRK-660-LL-AREA-RFTM     PIC  9(004) COMP    VALUE 0202.
           05 WRK-660-LL-MENSA-RFTM    PIC  9(004) COMP    VALUE 1109.

           05 WRK-660-SENHAS-RFTM      PIC  9(004) COMP    VALUE 0037.
           05 WRK-660-COMANDO-RFTM     PIC  9(004) COMP    VALUE 2070.
           05 WRK-660-PAGINA-RFTM      PIC  9(004) COMP    VALUE 0004.
           05 WRK-660-OCULTO1-RFTM     PIC  9(004) COMP    VALUE 0074.
           05 WRK-660-NOMCLIE-RFTM     PIC  9(004) COMP    VALUE 0040.
           05 WRK-660-CNPJCPF-RFTM     PIC  9(004) COMP    VALUE 0019.
           05 WRK-660-BCAGCTA-RFTM     PIC  9(004) COMP    VALUE 0023.
           05 WRK-660-CODPROD-RFTM     PIC  9(004) COMP    VALUE 0003.
           05 WRK-660-DESPROD-RFTM     PIC  9(004) COMP    VALUE 0012.
           05 WRK-660-CODSUBP-RFTM     PIC  9(004) COMP    VALUE 0003.
           05 WRK-660-DESSUBP-RFTM     PIC  9(004) COMP    VALUE 0012.
           05 WRK-660-OPERAC-RFTM      PIC  9(004) COMP    VALUE 0013.
           05 WRK-660-DESSIT-RFTM      PIC  9(004) COMP    VALUE 0015.
           05 WRK-660-SIT1-RFTM        PIC  9(004) COMP    VALUE 0013.
           05 WRK-660-SIT2-RFTM        PIC  9(004) COMP    VALUE 0009.
           05 WRK-660-SIT3-RFTM        PIC  9(004) COMP    VALUE 0013.
           05 WRK-660-SIT4-RFTM        PIC  9(004) COMP    VALUE 0009.
           05 WRK-660-TABELA-RFTM      OCCURS 009  TIMES.
             10 WRK-660-SELEC-RFTM     PIC  9(004) COMP    VALUE 0003.
             10 WRK-660-PARC-RFTM      PIC  9(004) COMP    VALUE 0003.
             10 WRK-660-NOME-RFTM      PIC  9(004) COMP    VALUE 0013.

             10 WRK-660-DTVCTO-RFTM    PIC  9(004) COMP    VALUE 0010.
             10 WRK-660-DTPROR-RFTM    PIC  9(004) COMP    VALUE 0010.
             10 WRK-660-TXORIG-RFTM    PIC  9(004) COMP    VALUE 0006.
             10 WRK-660-TXPROR-RFTM    PIC  9(004) COMP    VALUE 0006.
             10 WRK-660-VALOR-RFTM     PIC  9(004) COMP    VALUE 0012.
             10 WRK-660-VLENC-RFTM     PIC  9(004) COMP    VALUE 0010.
           05 WRK-660-MENSAGE-RFTM     PIC  9(004) COMP    VALUE 0079.

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
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO COM O MODULO DCOM5355 **'.
      *----------------------------------------------------------------*

       01  WRK-5355E-ENTRADA.
           05 WRK-5355E-COD-RETORNO      PIC  X(004)       VALUE SPACES.
           05 WRK-5355E-MSG-RETORNO      PIC  X(079)       VALUE SPACES.
           05 WRK-5355E-RESTART          PIC  9(005)       VALUE ZEROS.

           05 WRK-5355E-FLAG             PIC  X(001)       VALUE SPACES.
           05 WRK-5355E-CFUNC-BDSCO      PIC  9(009)       VALUE ZEROS.
           05 WRK-5355E-CTERM            PIC  X(008)       VALUE SPACES.
           05 WRK-5355E-TIPO-CONVENIO    PIC  X(001)       VALUE SPACES.
           05 WRK-5355E-CCONVE-GRAL-DESC PIC  9(009)       VALUE ZEROS.
           05 WRK-5355E-CPRODT           PIC  9(003)       VALUE ZEROS.
           05 WRK-5355E-CSPROD-DESC-COML PIC  9(003)       VALUE ZEROS.
4S2511     05 WRK-5355E-CCNPJ-CPF        PIC  X(009)       VALUE SPACES.
4S2511     05 WRK-5355E-CFLIAL-CNPJ      PIC  X(04)        VALUE SPACES.
           05 WRK-5355E-CCTRL-CNPJ-CPF   PIC  9(002)       VALUE ZEROS.
           05 WRK-5355E-BANCO            PIC  9(003)       VALUE ZEROS.
           05 WRK-5355E-AGENCIA          PIC  9(005)       VALUE ZEROS.
           05 WRK-5355E-CCTA-CLI         PIC  9(007)        VALUE ZEROS.
           05 WRK-5355E-FLAG-FORMAL      PIC  X(001)       VALUE SPACES.

       01  WRK-5355S-SAIDA.
           05 WRK-5355S-COD-RETORNO      PIC  X(004)       VALUE SPACES.
           05 WRK-5355S-MSG-RETORNO      PIC  X(079)       VALUE SPACES.
           05 WRK-5355S-RESTART          PIC  9(005)       VALUE ZEROS.
           05 WRK-5355S-FLAG             PIC  X(001)       VALUE SPACES.
           05 WRK-5355S-DADOS.

             10 WRK-5355S-CCONVE         PIC  9(009)       VALUE ZEROS.
             10 WRK-5355S-CVRSAO         PIC  9(003)       VALUE ZEROS.

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
           '*** DCOM1806 - FIM DA AREA DE WORKING ***'.
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

           MOVE SPACES                 TO WRK-OUT-DADOS-RFTM
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


           MOVE 'DCOMRFTM'             TO  WRK-TELA.

           IF  WRK-TRANSACAO           NOT EQUAL 'DCOM1806'
               PERFORM 1100-RECEBER-CONTROLE
           ELSE
               PERFORM 1200-PROCESSAR-DCOMRFTM
           END-IF.

           IF  WRK-TELA                EQUAL 'DCOMRFTM'
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

           MOVE WRK-COMU-OPCAO         TO WRK-INP-OPCAO-RFTM
                                          WRK-OUT-OPCAO-RFTM.

           IF  WRK-COMU-TRANSACAO       EQUAL 'DCOM1807'
               MOVE WRK-COMU-CHAMADOR   TO WRK-INP-CHAMADO-RFTM
                                           WRK-OUT-CHAMADO-RFTM
           ELSE
               MOVE WRK-COMU-TRANSACAO  TO WRK-INP-CHAMADO-RFTM
                                           WRK-OUT-CHAMADO-RFTM
           END-IF

           MOVE WRK-COMU-TIMESTAMP     TO WRK-INP-TMSTAMP-RFTM
                                          WRK-OUT-TMSTAMP-RFTM.
           MOVE WRK-COMU-PAGINA-1      TO WRK-INP-PAGOCUL-RFTM
                                          WRK-OUT-PAGOCUL-RFTM.
           MOVE WRK-COMU-OPERACAO      TO WRK-INP-OPERAC-RFTM

                                          WRK-OUT-OPERAC-RFTM.
           MOVE WRK-COMU-TP-SELECAO    TO WRK-INP-TPSELECAO-RFTM
                                          WRK-OUT-TPSELECAO-RFTM.
           MOVE WRK-COMU-CPRODUTO      TO WRK-INP-PRODOC-RFTM
                                          WRK-OUT-PRODOC-RFTM.
           MOVE WRK-COMU-IPRODUTO      TO WRK-INP-DESPROD-RFTM
                                          WRK-OUT-DESPROD-RFTM.
           MOVE WRK-COMU-CSUBPROD      TO WRK-INP-SBPROC-RFTM
                                          WRK-OUT-SBPROC-RFTM.
           MOVE WRK-COMU-ISUBPROD      TO WRK-INP-DESSUBP-RFTM
                                          WRK-OUT-DESSUBP-RFTM.

           EVALUATE WRK-COMU-TP-SELECAO
               WHEN 3
                    MOVE WRK-COMU-BANCO   TO WRK-INP-BANCO-RFTM
                                             WRK-OUT-BANCO-RFTM
                    MOVE WRK-COMU-AGENCIA TO WRK-INP-AGENOC-RFTM
                                             WRK-OUT-AGENOC-RFTM
               WHEN 4
                    MOVE WRK-COMU-BANCO   TO WRK-INP-BANCO-RFTM
                                             WRK-OUT-BANCO-RFTM

                    MOVE WRK-COMU-AGENCIA TO WRK-INP-AGENOC-RFTM
                                             WRK-OUT-AGENOC-RFTM
                    MOVE WRK-COMU-CONTA   TO WRK-INP-CONTOC-RFTM
                                             WRK-OUT-CONTOC-RFTM
               WHEN 5
                    MOVE WRK-COMU-CCNPJ-CPF
                                          TO WRK-INP-CNPJOC-RFTM
                                             WRK-OUT-CNPJOC-RFTM
                    MOVE WRK-COMU-CFLIAL-CNPJ
                                          TO WRK-INP-FILIAL-RFTM
                                             WRK-OUT-FILIAL-RFTM
                    MOVE WRK-COMU-CCTRL-CNPJCPF
                                          TO WRK-INP-CONTROLE-RFTM
                                             WRK-OUT-CONTROLE-RFTM
           END-EVALUATE.

           PERFORM 1110-OBTER-DADOS-OPERACAO.

           IF  WRK-INCONSIS            EQUAL SPACES
               IF  WRK-COMU-TRANSACAO  EQUAL 'DCOM1807'
                   MOVE WRK-COMU-PAGINA-2 TO WRK-OUT-PAGINA-RFTM

                                             WRK-INP-PAGINA-RFTM
                   COMPUTE WRK-RESTART    = (WRK-COMU-PAGINA-2 - 1) * 9
               ELSE
                   MOVE 1              TO WRK-OUT-PAGINA-RFTM
                                          WRK-INP-PAGINA-RFTM
                   MOVE ZEROS          TO WRK-RESTART
               END-IF

               PERFORM 1140-OBTER-LISTA-PARCELAS

               IF  WRK-5474S-COD-RETORNO  EQUAL '0003' OR
                   WRK-INCONSIS           EQUAL '*'
                   PERFORM 1220-RETORNAR-CHAMADOR
                   MOVE WRK-5474S-MSG-RETORNO
                                          TO WRK-COMU-MENSAGEM
                   MOVE WRK-COMU-AREA     TO WRK-MENSAGEM
               ELSE
                   IF  WRK-COMU-MENSAGEM  EQUAL SPACES
                       MOVE WRK-MSG001    TO WRK-OUT-MENSA-RFTM
                   ELSE
                       MOVE WRK-COMU-MENSAGEM

                                          TO WRK-OUT-MENSA-RFTM
                   END-IF
               END-IF
           END-IF.

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



           MOVE 4174-SAI-IPSSOA-DESC-COML      TO WRK-OUT-NOMCLIE-RFTM.


           STRING 4174-SAI-CBCO      '/'
                  4174-SAI-CAG-BCRIA '/'
                  4174-SAI-CCTA-BCRIA-CLI
           DELIMITED BY SIZE                 INTO WRK-INP-BCAGCTA-RFTM

           MOVE WRK-INP-BCAGCTA-RFTM           TO WRK-OUT-BCAGCTA-RFTM
           MOVE 4174-SAI-CCNPJ-CPF             TO WRK-INP-CNPJOC-RFTM
                                                  WRK-OUT-CNPJOC-RFTM.

           STRING 4174-SAI-CCNPJ-CPF(1:3) '.'
                  4174-SAI-CCNPJ-CPF(4:3) '.'
                  4174-SAI-CCNPJ-CPF(7:3) '/'
                  4174-SAI-CFLIAL-CNPJ    '-'
                  4174-SAI-CCTRL-CNPJ-CPF
           DELIMITED BY SIZE                 INTO WRK-INP-CNPJCPF-RFTM.

           MOVE WRK-INP-CNPJCPF-RFTM           TO WRK-OUT-CNPJCPF-RFTM
           MOVE 4174-SAI-CPRODT                TO WRK-OUT-CODPROD-RFTM.

           MOVE 4174-SAI-IABREV-PRODT          TO WRK-OUT-DESPROD-RFTM.
           MOVE 4174-SAI-CSPROD-DESC-COML      TO WRK-OUT-CODSUBP-RFTM.
           MOVE 4174-SAI-IRSUMO-SPROD-DESC     TO WRK-OUT-DESSUBP-RFTM.
           MOVE 4174-SAI-IRSUMO-SIT-DESC       TO WRK-OUT-DESSIT-RFTM.

           EVALUATE TRUE
               WHEN 4174-SAI-CCONTR-LIM-DESC      GREATER ZEROS
                    MOVE 'CONTR LIMITE:'          TO WRK-OUT-SIT1-RFTM
                    MOVE 4174-SAI-CCONTR-LIM-DESC TO WRK-OUT-SIT2-RFTM-N

               WHEN 4174-SAI-CCONTR-CONVE-DESC    GREATER ZEROS AND
                    4174-SAI-CELMTO-DESC-COML     EQUAL 8
                    MOVE 'CONV GERAL..:'          TO WRK-OUT-SIT1-RFTM
                    MOVE 4174-SAI-CCONTR-CONVE-DESC
                                                  TO WRK-OUT-SIT2-RFTM-N

               WHEN 4174-SAI-CCONTR-CONVE-DESC    GREATER ZEROS AND
                    4174-SAI-CELMTO-DESC-COML     EQUAL 9
                    MOVE 'CONV CLIENTE:'          TO WRK-OUT-SIT1-RFTM
                    MOVE 4174-SAI-CCONTR-CONVE-DESC
                                                  TO WRK-OUT-SIT2-RFTM-N


           END-EVALUATE.

           IF  4174-SAI-NSMULA-OPER-DESC    GREATER ZEROS
               IF  WRK-OUT-SIT1-RFTM        EQUAL SPACES
                   MOVE 'SIMULACAO...:'     TO WRK-OUT-SIT1-RFTM
                   MOVE 4174-SAI-NSMULA-OPER-DESC
                                            TO WRK-OUT-SIT2-RFTM-N
               ELSE
                   MOVE 'SIMULACAO...:'     TO WRK-OUT-SIT3-RFTM
                   MOVE 4174-SAI-NSMULA-OPER-DESC
                                            TO WRK-OUT-SIT4-RFTM-N
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  VERIFICA CONVENIO                                             *

      *----------------------------------------------------------------*
       1130-VERIFICAR-CONVENIO         SECTION.
      *----------------------------------------------------------------*


           MOVE SPACES                     TO WRK-SQLCA
                                              ERRO-AREA.

           INITIALIZE WRK-5355E-ENTRADA
                      WRK-5355S-SAIDA
                      ERRO-AREA.

           MOVE WRK-COD-USER-R             TO WRK-5355E-CFUNC-BDSCO.
           MOVE LNK-IO-LTERM               TO WRK-5355E-CTERM.
           MOVE 4174-SAI-CCONTR-CONVE-DESC TO WRK-5355E-CCONVE-GRAL-DESC
           MOVE 4174-SAI-CPRODT            TO WRK-5355E-CPRODT.
           MOVE 4174-SAI-CSPROD-DESC-COML  TO WRK-5355E-CSPROD-DESC-COML
           MOVE 4174-SAI-CCNPJ-CPF         TO WRK-5355E-CCNPJ-CPF.
           MOVE 4174-SAI-CFLIAL-CNPJ       TO WRK-5355E-CFLIAL-CNPJ.
           MOVE 4174-SAI-CCTRL-CNPJ-CPF    TO WRK-5355E-CCTRL-CNPJ-CPF.
           MOVE 4174-SAI-CBCO              TO WRK-5355E-BANCO.

           MOVE 4174-SAI-CAG-BCRIA         TO WRK-5355E-AGENCIA.
           MOVE 4174-SAI-CCTA-BCRIA-CLI    TO WRK-5355E-CCTA-CLI.

           IF   4174-SAI-CELMTO-DESC-COML  EQUAL 8
                MOVE 'G'                   TO WRK-5355E-TIPO-CONVENIO
           ELSE
                MOVE 'C'                   TO WRK-5355E-TIPO-CONVENIO
           END-IF

           MOVE 'DCOM5355'                 TO WRK-MODULO.


           CALL WRK-MODULO                 USING WRK-5355E-ENTRADA
                                                 WRK-5355S-SAIDA
                                                 ERRO-AREA
                                                 WRK-SQLCA.


           EVALUATE  WRK-5355S-COD-RETORNO
               WHEN '0000'
                     MOVE 4174-SAI-CCONTR-CONVE-DESC

                                                  TO WRK-OUT-SIT2-RFTM-N
                     IF 4174-SAI-CELMTO-DESC-COML EQUAL 8
                        MOVE 'CONV GERAL..:'      TO WRK-OUT-SIT1-RFTM
                     ELSE
                        MOVE 'CONV CLIENTE:'      TO WRK-OUT-SIT1-RFTM
                     END-IF

               WHEN '0003'
                     CONTINUE

               WHEN '1075'
               WHEN '1125'
                     MOVE '0003'        TO WRK-5355S-COD-RETORNO

               WHEN '0099'
                     MOVE 'DB2'         TO ERR-TIPO-ACESSO
                     PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN  OTHER
                     PERFORM 1220-RETORNAR-CHAMADOR
                     MOVE WRK-5355S-MSG-RETORNO

                                        TO WRK-COMU-MENSAGEM
                     MOVE WRK-COMU-AREA TO WRK-MENSAGEM
                     MOVE '*'           TO WRK-INCONSIS
           END-EVALUATE.

      *----------------------------------------------------------------*
       1130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     OBTEM LISTA DE PARCELAS DA OPERACAO ATRAVES DE ACESSO AO   *
      *     MODULO DCOM5474.                                           *
      *----------------------------------------------------------------*
       1140-OBTER-LISTA-PARCELAS       SECTION.
      *----------------------------------------------------------------*


           MOVE SPACES                 TO WRK-SQLCA ERRO-AREA.

           INITIALIZE WRK-5474-ENTRADA
                      WRK-5474-SAIDA

                      ERRO-AREA.

           MOVE WRK-COD-USER           TO WRK-5474E-CFUNC-BDSCO.
           MOVE LNK-IO-LTERM           TO WRK-5474E-CTERM.
           MOVE WRK-COD-DEPTO-N        TO WRK-AGENCIA-AUX-N.
           MOVE WRK-AGENCIA-AUX        TO WRK-5474E-MSG-RETORNO(1:5).
           MOVE WRK-INP-OPERAC-RFTM    TO WRK-5474E-OPERACAO.
           MOVE WRK-RESTART            TO WRK-5474E-RESTART.

           EVALUATE WRK-INP-OPCAO-RFTM
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
                     CONTINUE

               WHEN '0099'
                     MOVE 'DB2'        TO ERR-TIPO-ACESSO
                     PERFORM 9999-PROCESSAR-ROTINA-ERRO


               WHEN  OTHER
                     MOVE WRK-5474S-MSG-RETORNO
                                       TO WRK-OUT-MENSA-RFTM
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
                     TO WRK-OUT-SELEC-ATTR-RFTM     (IND-1)
                ELSE
                   MOVE WRK-49345
                     TO WRK-OUT-SELEC-ATTR-RFTM     (IND-1)
                   MOVE WRK-5474S-NPCELA-DESC       (IND-1)
                     TO WRK-OUT-PARC-RFTM-N         (IND-1)
                   MOVE WRK-5474S-IPSSOA-SACDO      (IND-1)
                     TO WRK-OUT-NOME-RFTM           (IND-1)
                   MOVE WRK-5474S-DTVCTO-PCELA      (IND-1)
                     TO WRK-OUT-DTVCTO-RFTM         (IND-1)
                   INSPECT WRK-OUT-DTVCTO-RFTM      (IND-1)
                            REPLACING  ALL '.'  BY '/'
                   MOVE WRK-5474S-DVCTO-PCELA-PARA  (IND-1)
                     TO WRK-OUT-DTPROR-RFTM         (IND-1)
                   INSPECT WRK-OUT-DTPROR-RFTM      (IND-1)
                            REPLACING  ALL '.'  BY '/'
                   MOVE WRK-5474S-PTX-JURO-MES-DE   (IND-1)
                     TO WRK-OUT-TXORIG-RFTM         (IND-1)
                   MOVE WRK-5474S-PTX-JURO-MES-PARA (IND-1)
                     TO WRK-OUT-TXPROR-RFTM         (IND-1)

                   MOVE WRK-5474S-VPRINC-PCELA      (IND-1)
                     TO WRK-OUT-VALOR-RFTM-N        (IND-1)
                   MOVE WRK-5474S-VENCAR-PCELA-PARA (IND-1)
                     TO WRK-OUT-VLENC-RFTM-N        (IND-1)
                END-IF
           END-PERFORM.

           IF  WRK-5474S-FLAG          EQUAL 'S'
               MOVE '*'                TO WRK-OUT-FIMAMOS-RFTM
               MOVE WRK-MSG004         TO WRK-OUT-MENSA-RFTM
           ELSE
               MOVE SPACES             TO WRK-OUT-FIMAMOS-RFTM
               MOVE WRK-MSG005         TO WRK-OUT-MENSA-RFTM
           END-IF.

      *----------------------------------------------------------------*
       1150-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA PROCESSA A MENSAGEM VINDA DA TELA DCOMDCTM        *

      *----------------------------------------------------------------*
       1200-PROCESSAR-DCOMRFTM         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM             TO WRK-INPUT-RFTM.

           EVALUATE TRUE
               WHEN WRK-INP-PFK-RFTM     EQUAL 'H' OR
                   (WRK-INP-PFK-RFTM     EQUAL '.' AND
                    WRK-INP-COMANDO-RFTM EQUAL 'PFK01')
                    PERFORM 1210-DEVOLVER-TELA
                    MOVE WRK-49345       TO WRK-OUT-SELEC-ATTR-RFTM(1)
                    MOVE WRK-INP-MENSA-RFTM
                                         TO WRK-OUT-MENSA-RFTM

               WHEN WRK-INP-PFK-RFTM     EQUAL '3' OR
                   (WRK-INP-PFK-RFTM     EQUAL '.' AND
                    WRK-INP-COMANDO-RFTM EQUAL 'PFK03')
                    PERFORM 1220-RETORNAR-CHAMADOR

               WHEN WRK-INP-PFK-RFTM     EQUAL '5' OR

                   (WRK-INP-PFK-RFTM     EQUAL '.' AND
                    WRK-INP-COMANDO-RFTM EQUAL 'PFK05')
                    PERFORM 1230-RETORNAR-MENU-ROTINAS

               WHEN WRK-INP-PFK-RFTM     EQUAL '7' OR
                   (WRK-INP-PFK-RFTM     EQUAL '.' AND
                    WRK-INP-COMANDO-RFTM EQUAL 'PFK07')
                    PERFORM 1240-TRATAR-PFK7

               WHEN WRK-INP-PFK-RFTM     EQUAL '8' OR
                   (WRK-INP-PFK-RFTM     EQUAL '.' AND
                    WRK-INP-COMANDO-RFTM EQUAL 'PFK08')
                    PERFORM 1250-TRATAR-PFK8

               WHEN WRK-INP-PFK-RFTM     EQUAL 'A' OR
                   (WRK-INP-PFK-RFTM     EQUAL '.' AND
                    WRK-INP-COMANDO-RFTM EQUAL 'PFK10')
                    PERFORM 1260-RETORNAR-MENU-DCOM

               WHEN WRK-INP-PFK-RFTM     EQUAL '.' AND
                    WRK-INP-COMANDO-RFTM EQUAL  SPACES OR LOW-VALUES

                    PERFORM 1270-TRATAR-ENTER

               WHEN OTHER
                    PERFORM 1210-DEVOLVER-TELA
                    MOVE WRK-49345     TO WRK-OUT-SELEC-ATTR-RFTM(1)
                    MOVE WRK-MSG002    TO WRK-OUT-MENSA-RFTM
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
                IF WRK-INP-TABELA-RFTM(IND-1) EQUAL SPACES
                   MOVE WRK-225        TO WRK-OUT-SELEC-ATTR-RFTM(IND-1)
                ELSE
                   MOVE WRK-193
                     TO WRK-OUT-SELEC-ATTR-RFTM(IND-1)
                   MOVE WRK-INP-SELEC-RFTM  (IND-1)
                     TO WRK-OUT-SELEC-RFTM  (IND-1)
                   MOVE WRK-INP-PARC-RFTM   (IND-1)
                     TO WRK-OUT-PARC-RFTM   (IND-1)
                   MOVE WRK-INP-NOME-RFTM   (IND-1)
                     TO WRK-OUT-NOME-RFTM   (IND-1)
                   MOVE WRK-INP-VALOR-RFTM  (IND-1)
                     TO WRK-OUT-VALOR-RFTM  (IND-1)
                   MOVE WRK-INP-DTVCTO-RFTM (IND-1)
                     TO WRK-OUT-DTVCTO-RFTM (IND-1)
                   MOVE WRK-INP-DTPROR-RFTM (IND-1)
                     TO WRK-OUT-DTPROR-RFTM (IND-1)
                   MOVE WRK-INP-TXORIG-RFTM (IND-1)
                     TO WRK-OUT-TXORIG-RFTM (IND-1)
                   MOVE WRK-INP-TXPROR-RFTM (IND-1)

                     TO WRK-OUT-TXPROR-RFTM (IND-1)
                   MOVE WRK-INP-VALOR-RFTM-N(IND-1)
                     TO WRK-OUT-VALOR-RFTM-N(IND-1)
                   MOVE WRK-INP-VLENC-RFTM-N(IND-1)
                     TO WRK-OUT-VLENC-RFTM-N(IND-1)

                END-IF
           END-PERFORM.

      *----------------------------------------------------------------*
       1210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA DEVOLVE OS DADOS DA TELA SEM ALTERACAO            *
      *----------------------------------------------------------------*
       1215-DEVOLVER-TELA-FIXA         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-PAGINA-RFTM     TO WRK-OUT-PAGINA-RFTM.
           MOVE WRK-INP-OCULTOS-RFTM    TO WRK-OUT-OCULTOS-RFTM.

           MOVE WRK-INP-NOMCLIE-RFTM    TO WRK-OUT-NOMCLIE-RFTM.
           MOVE WRK-INP-CNPJCPF-RFTM    TO WRK-OUT-CNPJCPF-RFTM.
           MOVE WRK-INP-BCAGCTA-RFTM    TO WRK-OUT-BCAGCTA-RFTM.
           MOVE WRK-INP-PRODUTO-RFTM    TO WRK-OUT-PRODUTO-RFTM.
           MOVE WRK-INP-SUBPRODUTO-RFTM TO WRK-OUT-SUBPRODUTO-RFTM.
           MOVE WRK-INP-OPERAC-RFTM     TO WRK-OUT-OPERAC-RFTM.
           MOVE WRK-INP-DESSIT-RFTM     TO WRK-OUT-DESSIT-RFTM.
           MOVE WRK-INP-SIT1-RFTM       TO WRK-OUT-SIT1-RFTM.
           MOVE WRK-INP-SIT2-RFTM       TO WRK-OUT-SIT2-RFTM.
           MOVE WRK-INP-SIT3-RFTM       TO WRK-OUT-SIT3-RFTM.
           MOVE WRK-INP-SIT4-RFTM       TO WRK-OUT-SIT4-RFTM.

      *----------------------------------------------------------------*
       1215-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA RETORNA AO PROGRAMA CHAMADOR                      *
      *----------------------------------------------------------------*
       1220-RETORNAR-CHAMADOR          SECTION.
      *----------------------------------------------------------------*


           PERFORM 1225-FORMATAR-COMU-AREA.

           MOVE WRK-INP-CHAMADO-RFTM   TO WRK-TELA


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
           MOVE WRK-INP-OPCAO-RFTM     TO WRK-COMU-OPCAO.
           MOVE WRK-INP-TMSTAMP-RFTM   TO WRK-COMU-TIMESTAMP.
           MOVE WRK-INP-PAGOCUL-RFTM   TO WRK-COMU-PAGINA-1.
           MOVE WRK-INP-PAGINA-RFTM    TO WRK-COMU-PAGINA-2.
           MOVE WRK-INP-TPSELECAO-RFTM TO WRK-COMU-TP-SELECAO.
           MOVE WRK-INP-OPERAC-RFTM    TO WRK-COMU-OPERACAO.
           MOVE 237                    TO WRK-COMU-BANCO.
           MOVE WRK-INP-AGENCIA-RFTM   TO WRK-COMU-AGENCIA.
           MOVE WRK-INP-PRODOC-RFTM    TO WRK-COMU-CPRODUTO.
           MOVE WRK-INP-SBPROC-RFTM    TO WRK-COMU-CSUBPROD.
           MOVE WRK-INP-DESPROD-RFTM   TO WRK-COMU-IPRODUTO.
           MOVE WRK-INP-DESSUBP-RFTM   TO WRK-COMU-ISUBPROD.
           MOVE 'DCOM1806'             TO WRK-COMU-TRANSACAO.
           MOVE WRK-INP-CHAMADO-RFTM   TO WRK-COMU-CHAMADOR.

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
           MOVE 'DCOM1806'             TO WRK-TELA
                                          WRK-COMU-TRANSACAO.
           MOVE '5'                    TO WRK-COMU-PFK.
           MOVE WRK-COMU-AREA          TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*

      *     TRATAMENTO DE RETORNO DE PAGINA                            *
      *----------------------------------------------------------------*
       1240-TRATAR-PFK7                SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INP-PAGINA-RFTM     EQUAL 1
               PERFORM 1210-DEVOLVER-TELA
               MOVE WRK-49345          TO WRK-OUT-SELEC-ATTR-RFTM(1)
               MOVE WRK-MSG006         TO WRK-OUT-MENSA-RFTM
               GO TO 1240-99-FIM
           END-IF.

           PERFORM 1215-DEVOLVER-TELA-FIXA.

           MOVE WRK-49345              TO WRK-OUT-SELEC-ATTR-RFTM(1).

           COMPUTE WRK-RESTART         = (WRK-INP-PAGINA-RFTM - 2) * 9.

           PERFORM 1140-OBTER-LISTA-PARCELAS.

           IF  WRK-5474S-COD-RETORNO   NOT EQUAL '0000'

               PERFORM 1210-DEVOLVER-TELA
               MOVE WRK-5474S-MSG-RETORNO
                                       TO WRK-OUT-MENSA-RFTM
           ELSE
               SUBTRACT 1              FROM WRK-OUT-PAGINA-RFTM
           END-IF.

           MOVE WRK-49345              TO WRK-OUT-SELEC-ATTR-RFTM(1).

      *----------------------------------------------------------------*
       1240-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     TRATAMENTO DE AVANCAR PAGINA                               *
      *----------------------------------------------------------------*
       1250-TRATAR-PFK8                SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INP-FIMAMOS-RFTM    EQUAL '*'
               PERFORM 1210-DEVOLVER-TELA

               MOVE WRK-49345          TO WRK-OUT-SELEC-ATTR-RFTM(1)
               MOVE WRK-MSG007         TO WRK-OUT-MENSA-RFTM
               GO TO 1250-99-FIM
           END-IF.

           PERFORM 1215-DEVOLVER-TELA-FIXA.

           COMPUTE WRK-RESTART      =  WRK-INP-PAGINA-RFTM * 9.

           PERFORM 1140-OBTER-LISTA-PARCELAS.

           IF  WRK-5474S-COD-RETORNO   NOT EQUAL '0000'
               PERFORM 1210-DEVOLVER-TELA
               MOVE WRK-5474S-MSG-RETORNO
                                       TO WRK-OUT-MENSA-RFTM
           ELSE
               ADD 1                   TO WRK-OUT-PAGINA-RFTM
           END-IF.

           MOVE WRK-49345              TO WRK-OUT-SELEC-ATTR-RFTM(1).


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
           MOVE 'DCOM1806'             TO WRK-TELA
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

           MOVE ZEROS                  TO WRK-SELECAO
                                          WRK-SEL-X.

           PERFORM VARYING IND-1       FROM 1 BY 1
             UNTIL IND-1               GREATER 9 OR
                   WRK-INP-TABELA-RFTM(IND-1) EQUAL SPACES
                IF WRK-INP-SELEC-RFTM (IND-1) NOT EQUAL 'X' AND SPACES
                   MOVE '*'            TO WRK-INCONSIS
                   MOVE WRK-49353      TO WRK-OUT-SELEC-ATTR-RFTM(IND-1)
                END-IF
                IF WRK-INP-SELEC-RFTM (IND-1) EQUAL 'X'

                   ADD 1               TO WRK-SEL-X
                   MOVE IND-1          TO WRK-SELECAO
                END-IF
           END-PERFORM.

           IF WRK-INCONSIS             EQUAL '*'
              MOVE WRK-MSG009          TO WRK-OUT-MENSA-RFTM
           ELSE
              IF WRK-SEL-X             EQUAL ZEROS
                 MOVE WRK-MSG001       TO WRK-OUT-MENSA-RFTM
                 MOVE WRK-49345        TO WRK-OUT-SELEC-ATTR-RFTM(1)
              ELSE
                 IF WRK-SEL-X          GREATER 1
                    MOVE WRK-MSG008    TO WRK-OUT-MENSA-RFTM
                    PERFORM VARYING    IND-1 FROM 1 BY 1
                      UNTIL IND-1      GREATER 9 OR
                            WRK-INP-TABELA-RFTM(IND-1) EQUAL SPACES
                         IF WRK-INP-SELEC-RFTM (IND-1) EQUAL 'X'
                            MOVE WRK-49353
                              TO WRK-OUT-SELEC-ATTR-RFTM(IND-1)
                         END-IF

                    END-PERFORM
                 ELSE
                    PERFORM 2285-DETALHAR-OPERACAO
                 END-IF
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       1280-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     PASSA CONTROLE AO PROGRAMA DE ADITAMENTO DE PARCELA        *
      *----------------------------------------------------------------*
       2285-DETALHAR-OPERACAO          SECTION.
      *----------------------------------------------------------------*

           PERFORM 1225-FORMATAR-COMU-AREA.

           MOVE WRK-INP-PARC-RFTM-N   (WRK-SELECAO)
                                       TO WRK-COMU-PARCELA.

           MOVE 'DCOM1807'             TO WRK-TELA.
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

           CALL 'BRAD0660'             USING WRK-OUTPUT-RFTM
                                             WRK-660-DCOMRFTM.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE RETURN-CODE        TO WRK-RETURN-CODE
               MOVE 10                 TO WRK-LOCAL-ERRO

               MOVE WRK-ERRO-BRAD0660  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-ISRT               TO WRK-FUNCAO.
           MOVE WRK-OUTPUT-RFTM        TO WRK-MENSAGEM.

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
               MOVE 'DCOM1806'         TO ERR-PGM
           ELSE
               MOVE 'SENHAS02'         TO ERR-PGM
               MOVE 'DCOM1806'         TO ERR-MODULO
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
