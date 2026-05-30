
***************************** Top of Data ******************************
      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID.     DCOM1807.
       AUTHOR.         FABRICA - ALTRAN.
      *================================================================*
      *                          A L T R A N                           *
      *----------------------------------------------------------------*
      *     PROGRAMA....: DCOM1807                                     *
      *     PROGRAMADOR.: FABRICA - ALTRAN.                            *
      *     ANALISTA....: EDGARD RODRIGUES/AQUIRA H.                   *
      *     DATA........: 17/04/2007                                   *
      *----------------------------------------------------------------*
      *     OBJETIVO....: CONSULTAR PRORROGACAO  VENCIMENTO - DETALHES *
      *----------------------------------------------------------------*
      *     TELAS.......:                                              *
      *     DCOMRGTM....: CONSULTAR PRORROGACAO  VENCIMENTO - DETALHES *
      *----------------------------------------------------------------*
      *     INC'S.......:                                              *
      *     I#BRAD7C - AREA DE ERRO DA BRAD7100                        *

      *     I#DCOMSG - TABELA DE MENSAGENS DE RETORNO DO               *
      *                SISTEMA DCOM (ON-LINE)                          *
      *----------------------------------------------------------------*
      *     MODULOS.....:                                              *
      *     BRAD0660 - INSERIR CARACTERES DE NULL NA MENSAGEM DE SAIDA *
      *     BRAD5010 - CONTROLE DO TRANSITO DE MENSAGENS NO IMS/DC     *
      *     BRAD7100 - TRATAMENTO DE ERROS QUANDO PROGRAMA INVALIDO    *
      *     BRAD7600  FORNECE DATA E HORA CORRENTE DO SISTEMA          *
      *----------------------------------------------------------------*
      *     NAVEGACAO...:                                              *
      *     CHAMADO POR.: DCOM1801/DCOM1804                            *
      *================================================================*
BSI001*                    ALTERACAO  -  BSI TECNOLOGIA                *
BSI001*----------------------------------------------------------------*
BSI001*    PROGRAMADOR.:   FABRICA BSI             -  BSI TECNOLOGIA   *
BSI001*    ANALISTA DDS:   EDGARD/RICARDO          -  DDS / GRUPO 70   *
BSI001*    DATA........:   14/02/2013                                  *
BSI001*----------------------------------------------------------------*
BSI001*    OBJETIVO....: - CORRIGIR FORMATACAO DO FILTRO DE CONSULTA   *
BSI001*                    NO MODULO DCIN5473. PROJETO ITENS DINAMICOS *
BSI001*================================================================*

BRQ001*                    ALTERACAO - BRQ                             *
BRQ001*----------------------------------------------------------------*
BRQ001*  ANALISTA BRQ..:   FABIO AUGUSTO FINK                          *
BRQ001*  ANALISTA DS...:   RICARDO JAMMAL     - BRADESCO - GRUPO 39    *
BRQ001*  DATA..........:   24/11/2017                                  *
BRQ001*----------------------------------------------------------------*
BRQ001*  OBJETIVO......: - INCLUIR CAMPO BASE LEGAL PRORROGACAO PARA   *
BRQ001*                    ADEQUAR FUNCIONALIDADE AO SICOR 4.10.       *
BRQ001*================================================================*

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
       01  FILLER                      PIC  X(50)          VALUE
           '*** DCOM1807 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DE ATRIBUTOS ***'.
      *----------------------------------------------------------------*
      *--- ALPHA    - NORMAL    - PROTEGIDO    - NAO POS CURSOR ---*
       77  WRK-225                     PIC S9(08) COMP     VALUE +241.
      *--- ALPHA    - NODISP    - PROTEGIDO    - NAO POS CURSOR ---*
       77  WRK-229                     PIC S9(08) COMP     VALUE +229.
      *--- NUMERICO - NORMAL    - PROTEGIDO    - NAO POS CURSOR ---*
       01  WRK-241                     PIC S9(08) COMP     VALUE +241.
      *--- NUMERIC  - NORMAL    - DESPROTEGIO  - NAO POS CURSOR ---*

       77  WRK-209                     PIC S9(08) COMP     VALUE +209.
      *--- ALPHA    - NORMAL    - DESPROTEGIO  - NAO POS CURSOR ---*
       77  WRK-193                     PIC S9(08) COMP     VALUE +193.
      *--- BRILHANTE, NORMAL    - DESPROTEGIDO  - POS CURSOR - ALFA ---*
       01  WRK-49353                   PIC S9(08) COMP     VALUE +49353.
      *--- NUMERICO - NORMAL    - PROTEGIDO    - POS. CURSOR    ---*
       01  WRK-49361                   PIC S9(08) COMP     VALUE +49361.
      *--- NUMERICO - BRILHANTE - DESPROTEGIDO - POS. CURSOR    ---*
       01  WRK-49369                   PIC S9(08) COMP     VALUE +49369.
      *

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*
       01  WRK-AUXILIARES.
           05  WRK-MODULO              PIC  X(08)          VALUE SPACES.
           05  WRK-CHAMADOR            PIC  X(08)          VALUE SPACES.
           05  WRK-INCONSIS            PIC  X(01)          VALUE SPACES.

           05  WRK-BCOAGECTA-AUX.

               10  WRK-BANCO-AUX       PIC  9(03)          VALUE ZEROS.
               10  FILLER              PIC  X(01)          VALUE '/'.
               10  WRK-AGENC-AUX       PIC  9(05)          VALUE ZEROS.
               10  FILLER              PIC  X(01)          VALUE '/'.
               10  WRK-CONTA-AUX       PIC  9(13)          VALUE ZEROS.

          05  WRK-TAXA-MINMAX.
              10 FILLER               PIC  X(06)          VALUE
                 '(MIN: '.
              10 WRK-TAX-MIN          PIC  99,999         VALUE ZEROS.
              10 FILLER               PIC  X(06)          VALUE
                 ' MAX: '.
              10 WRK-TAX-MAX          PIC  99,999         VALUE ZEROS.
              10 FILLER               PIC  X(01)          VALUE ')'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DE COMUNICACAO DCOM1801 ***'.
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
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DE COMUNICACAO PFK ***'.
      *----------------------------------------------------------------*

       01  COMU-AREA-PFK.
           05  COMU-LL-PFK             PIC S9(04) COMP     VALUE +124.
           05  COMU-ZZ-PFK             PIC S9(04) COMP     VALUE ZEROS.
           05  COMU-TRANCODE.
               10  COMU-TRANSACAO-PFK  PIC  X(08)          VALUE SPACES.
               10  FILLER              PIC  X(06)          VALUE SPACES.
               10  COMU-PFK            PIC  X(01)          VALUE SPACES.
           05  COMU-SENHAS-PFK         PIC  X(37)          VALUE SPACES.
           05  COMU-COMANDO-PFK        PIC  X(68)          VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DE COMUNICACAO DCOM4174 ***'.
      *----------------------------------------------------------------*

       COPY 'I#DCOMXJ'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DE COMUNICACAO P/ DCOM5473 ***'.
      *----------------------------------------------------------------*


       01  5473-ENTRADA.
           03 5473-ENT-COD-RETORNO        PIC  X(004).
           03 5473-ENT-MSG-RETORNO        PIC  X(079).
           03 5473-ENT-CFUNC-BDSCO        PIC  9(009).
           03 5473-ENT-CTERM              PIC  X(008).
      ******************DADOS GERAIS ***********************************
           03 5473-ENT-FLAG-FILTRO        PIC  X(002).
           03 5473-ENT-OPERACAO           PIC  9(013).
           03 FILLER REDEFINES 5473-ENT-OPERACAO.
              05 5473-ENT-DANO-OPER-DESC  PIC  9(004).
              05 5473-ENT-NSEQ-OPER-DESC  PIC  9(009).
           03 5473-ENT-NPCELA-DESC-COML   PIC  9(005).

       01  5473-SAIDA.
           03  5473-SAI-COD-RETORNO              PIC  X(004).
           03  5473-SAI-MSG-RETORNO              PIC  X(079).
           03  5473-SAI-TAMANHO-ERRO             PIC  9(001).
           03  5473-SAI-TABELA-ERROS  OCCURS  03  TIMES.
               05  5473-SAI-CAMPOS-ERRO          PIC  9(001).
           03  5473-SAI-DVCTO-PCELA-DESC         PIC  X(010).

           03  5473-SAI-VPRINC-PCELA-DESC        PIC  9(015)V9(02).
           03  5473-SAI-PTX-JURO-MES             PIC  9(003)V9(05).
           03  5473-SAI-TFREQ-TAXA-PCELA         PIC  X(004).
           03  5473-SAI-DVCTO-PCELA-PRROG        PIC  X(010).
           03  5473-SAI-PTX-JURO-MES-PRROG       PIC  9(003)V9(05).
           03  5473-SAI-TFREQ-TX-PRROG           PIC  X(004).
           03  5473-SAI-ITPO-TX-JURO             PIC  X(015).
           03  5473-SAI-PMIN-TAXA-JURO           PIC  S9(003)V9(3).
           03  5473-SAI-PMAX-TAXA-JURO           PIC  S9(003)V9(3).
           03  5473-SAI-CAUTRZ-ESPCL             PIC  9(009).
           03  5473-SAI-IAUTRZ-ESPCL             PIC  X(040).
           03  5473-SAI-VJURO-PCELA-DESC         PIC  9(015)V9(02).
           03  5473-SAI-VIOF-PRROG-PCELA         PIC  9(015)V9(02).
           03  5473-SAI-VTARIF-PRROG             PIC  9(015)V9(02).
           03  5473-SAI-VENCAR-PCELA-DESC        PIC  9(015)V9(02).
BRQ001     03  WRK-5473S-NBASE-LEGAL-RENEG       PIC 9(005) VALUE ZEROS.
BRQ001     03  WRK-5473S-IBASE-LEGAL-RENEG       PIC X(100) VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DE COMUNICACAO P/ DCOM5151 ***'.

      *----------------------------------------------------------------*
       01  5151-ENTRADA.
           03  5151-HEADER.
               05  5151-ENT-COD-RETORNO              PIC  X(004).
               05  5151-ENT-MSG-RETORNO              PIC  X(079).
               05  5151-ENT-RESTART                  PIC  9(005).
               05  5151-ENT-FLAG                     PIC  X(001).
               05  5151-ENT-CFUNC-BDSCO              PIC  9(009).
               05  5151-ENT-CTERM                    PIC  X(008).
               05  5151-ENT-DANO-OPER-DESC           PIC  9(004).
               05  5151-ENT-NSEQ-OPER-DESC           PIC  9(009).
4S2511*        05  5151-ENT-CCNPJ-CPF                PIC  9(009).
4S2511         05  5151-ENT-CCNPJ-CPF                PIC  X(09).
4S2511*        05  5151-ENT-CFLIAL-CNPJ              PIC  9(004).
4S2511         05  5151-ENT-CFLIAL-CNPJ              PIC  X(04).
               05  5151-ENT-DVCTO-ANTE               PIC  X(010).
               05  5151-ENT-DVCTO-NOVA               PIC  X(010).
               05  5151-ENT-VPRINC-PCELA             PIC  9(015)V99.

       01  5151-SAIDA.
           03  5151-SAI-HEADER.
               05  5151-SAI-COD-RETORNO              PIC  X(004).
               05  5151-SAI-MSG-RETORNO              PIC  X(079).

               05  5151-SAI-RESTART                  PIC  9(005).
               05  5151-SAI-FLAG                     PIC  X(001).
           03  5151-SAI-DADOS.
               05 5151-SAI-CINDCD-TX-REDE            PIC X(01).
               05 5151-SAI-CTPO-TX-JURO-DESC         PIC 9(03).
               05 5151-SAI-NPCELA-DESC               PIC 9(05).
               05 5151-SAI-CSGL-TPO-TX-JURO          PIC X(06).
               05 5151-SAI-PMIN-TX-JURO-REGRA        PIC 9(03)V9(06).
               05 5151-SAI-PMAX-TX-JURO-REGRA        PIC 9(03)V9(06).
               05 5151-SAI-CINDCD-FREQ-TX            PIC X(02).
           03  5151-SAI-CONSISTENCIA REDEFINES 5151-SAI-DADOS.
               05  5151-SAI-TAMANHO-ERRO             PIC  9(001).
               05  5151-SAI-TABELA-ERROS             OCCURS 2 TIMES.
                   07  5151-SAI-CAMPOS-ERRO          PIC  9(001).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)      VALUE
           '*** AREAS AUXILIARES PARA BRAD5010 ***'.
      *----------------------------------------------------------------*

       01  WRK-BRAD5010.

           05  WRK-SQLCA               PIC  X(136)         VALUE SPACES.
           05  WRK-GU                  PIC  X(04)          VALUE 'GU  '.
           05  WRK-CHNG                PIC  X(04)          VALUE 'CHNG'.
           05  WRK-ISRT                PIC  X(04)          VALUE 'ISRT'.
           05  WRK-FUNCAO              PIC  X(04)          VALUE SPACES.
           05  WRK-TELA                PIC  X(08)          VALUE SPACES.
           05  WRK-VERSAO              PIC  X(06)          VALUE
                                                           'VRS002'.
           05  WRK-COD-USER            PIC  X(07)          VALUE SPACES.
           05  WRK-COD-USER-R          REDEFINES
               WRK-COD-USER            PIC  9(07).
           05  WRK-COD-DEPTO           PIC  9(06)          VALUE ZEROS.

           05  WRK-MENSAGEM.
               10  FILLER              PIC  X(04)          VALUE SPACES.
               10  WRK-TRANSACAO       PIC  X(08)          VALUE SPACES.
               10  FILLER              PIC  X(1988)        VALUE SPACES.
      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DA TELA DCOMRGTM - INPUT ***'.

      *----------------------------------------------------------------*

       01  WRK-INPUT-RGTM.
           05  FILLER                  PIC  X(18)          VALUE SPACES.
           05  WRK-INP-PFK-RGTM        PIC  X(01)          VALUE SPACES.
           05  WRK-INP-SENHAS-RGTM     PIC  X(37)          VALUE SPACES.
           05  WRK-INP-COMANDO-RGTM    PIC  X(68)          VALUE SPACES.
           05  WRK-INP-DADOS-RGTM.
               10  WRK-INP-TIPOOPC-RGTM
                                       PIC  X(01)          VALUE SPACES.
               10  WRK-INP-TMSTAMP-RGTM
                                       PIC  X(26)          VALUE SPACES.
               10  WRK-INP-PASSO-RGTM  PIC  X(01)          VALUE SPACES.
               10  WRK-INP-CHAMADO-RGTM
                                       PIC  X(08)          VALUE SPACES.
               10  WRK-INP-PAGINA-RGTM PIC  X(04)          VALUE SPACES.
               10  WRK-INP-TPSEL-RGTM  PIC  X(01)          VALUE SPACES.
               10  WRK-INP-CODDESC-RGTM PIC 9(03)          VALUE ZEROS.
               10  WRK-INP-PAGINA1-RGTM PIC 9(04)          VALUE ZEROS.
               10  WRK-INP-NOMECL-RGTM PIC  X(40)          VALUE SPACES.
               10  WRK-INP-CGCCPF-RGTM PIC  X(19)          VALUE SPACES.

               10  WRK-INP-XCONTA-RGTM PIC  X(23)          VALUE SPACES.
               10  WRK-INP-CODPRD-RGTM PIC  9(03)          VALUE ZEROS.
               10  WRK-INP-DESPRD-RGTM PIC  X(06)          VALUE SPACES.
               10  WRK-INP-CODSUB-RGTM PIC  9(03)          VALUE ZEROS.
               10  WRK-INP-DESSUB-RGTM PIC  X(10)          VALUE SPACES.
               10  WRK-INP-OPERAC-RGTM PIC  9(13)          VALUE ZEROS.
               10  WRK-INP-SITUAC-RGTM PIC  X(15)          VALUE SPACES.
               10  WRK-INP-DESIDE-RGTM PIC  X(10)          VALUE SPACES.
               10  WRK-INP-NUMERO-RGTM PIC  X(09)          VALUE SPACES.
               10  WRK-INP-PARCEL-RGTM PIC  9999           VALUE ZEROS.
               10  WRK-INP-DTVENC-RGTM PIC  X(10)          VALUE SPACES.
               10  WRK-INP-VALOR-RGTM  PIC  ZZ.ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
               10  WRK-INP-TAXA-RGTM   PIC  ZZ9,99         VALUE ZEROS.
               10  WRK-INP-FREQ-RGTM   PIC  X(04)          VALUE SPACES.
               10  WRK-INP-TPJUR-RGTM  PIC  9(03)          VALUE ZEROS.
               10  WRK-INP-TSMIN-RGTM.
                   15 WRK-INP-TSMIN-RGTM-N
                                       PIC  9(16)V99        VALUE ZEROS.
               10  WRK-INP-TSMAX-RGTM.
                   15 WRK-INP-TSMAX-RGTM-N

                                      PIC  9(16)V99        VALUE ZEROS.
               10  WRK-INP-FLAG-RGTM   PIC  X(01)          VALUE SPACES.
               10  WRK-INP-CHAMA-RGTM  PIC  X(08)          VALUE SPACES.
               10  WRK-INP-DATA-PRORROGADA  PIC X(10)     VALUE SPACES.
               10  WRK-INP-TXJURO-RGTM.
                  15  WRK-INP-TXJURO-RGTM-N PIC  ZZ9,99999 VALUE ZEROS.
               10  WRK-INP-FREQTX-RGTM PIC  X(04)          VALUE SPACES.
BRQ001         10  WRK-INP-TPJURO-RGTM PIC  X(13)          VALUE SPACES.
               10 WRK-INP-MINMAX-RGTM.
                 15 FILLER              PIC  X(06)         VALUE SPACES.
                 15 WRK-INP-TXMIN-RGTM  PIC  99,999  VALUE ZEROS.
                 15 FILLER              PIC  X(06)    VALUE SPACES.
                 15 WRK-INP-TXMAX-RGTM  PIC  99,999   VALUE ZEROS.
                 15 FILLER              PIC  X(01)         VALUE SPACES.
BRQ001         10 WRK-INP-LTBSPR-RGTM   PIC  X(17)         VALUE SPACES.
BRQ001         10 WRK-INP-CDBSPR-RGTM.
BRQ001           15 WRK-INP-CDBSPR-RGTM-N
BRQ001                                  PIC  9(05)         VALUE ZEROS.
BRQ001         10 WRK-INP-DSBSPR-RGTM   PIC  X(55)         VALUE SPACES.
               10 WRK-INP-LITAUT-RGTM   PIC  X(12)         VALUE SPACES.
               10 WRK-INP-CODAUT-RGTM.

                 15 WRK-INP-CODAUT-RGTM-N
                                        PIC  9(09).
               10 WRK-INP-DESAUT-RGTM   PIC  X(22)        VALUE SPACES.
               10 WRK-INP-JAUTOR-RGTM   PIC  ZZZ.ZZZ.ZZ9,99 VALUE ZEROS.
               10 WRK-INP-IAUTOR-RGTM   PIC  ZZZ.ZZZ.ZZ9,99 VALUE ZEROS.
               10 WRK-INP-TACAUT-RGTM   PIC  ZZZ.ZZZ.ZZ9,99 VALUE ZEROS.
               10 WRK-INP-TOTAUT-RGTM   PIC  ZZZ.ZZZ.ZZ9,99 VALUE ZEROS.
           05  WRK-INP-MENSA-RGTM       PIC  X(79)          VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(50)      VALUE
           '*** AREA DA TELA DCOMRGTM  - OUTPUT ***'.
      *----------------------------------------------------------------*
       01  WRK-OUTPUT-RGTM.
           05  WRK-OUT-LL-RGTM         PIC S9(04) COMP     VALUE +0637.
           05  WRK-OUT-ZZ-RGTM         PIC S9(04) COMP     VALUE ZEROS.
           05  WRK-OUT-SENHAS-RGTM     PIC  X(37)          VALUE SPACES.
           05  WRK-OUT-COMANDO-RGTM    PIC  X(68)          VALUE SPACES.
           05  WRK-OUT-DADOS-RGTM.
               10  WRK-OUT-TIPOOPC-RGTM
                                       PIC  X(01)          VALUE SPACES.

               10  WRK-OUT-TMSTAMP-RGTM
                                       PIC  X(26)          VALUE SPACES.
               10  WRK-OUT-PASSO-RGTM  PIC  X(01)          VALUE SPACES.
               10  WRK-OUT-CHAMADO-RGTM
                                       PIC  X(08)          VALUE SPACES.
               10  WRK-OUT-PAGINA-RGTM PIC  X(04)          VALUE SPACES.
               10  WRK-OUT-TPSEL-RGTM  PIC  X(01)          VALUE SPACES.
               10  WRK-OUT-CODDESC-RGTM PIC  9(03)         VALUE ZEROS.
               10  WRK-OUT-PAGINA1-RGTM PIC 9(04)          VALUE ZEROS.
               10  WRK-OUT-NOMECL-RGTM PIC  X(40)          VALUE SPACES.
               10  WRK-OUT-CGCCPF-RGTM PIC  X(19)          VALUE SPACES.
               10  WRK-OUT-XCONTA-RGTM PIC  X(23)          VALUE SPACES.
               10  WRK-OUT-CODPRD-RGTM PIC  9(03)          VALUE ZEROS.
               10  WRK-OUT-DESPRD-RGTM PIC  X(06)          VALUE SPACES.
               10  WRK-OUT-CODSUB-RGTM PIC  9(03)          VALUE ZEROS.
               10  WRK-OUT-DESSUB-RGTM PIC  X(10)          VALUE SPACES.
               10  WRK-OUT-OPERAC-RGTM PIC  9(13)          VALUE ZEROS.
               10  WRK-OUT-SITUAC-RGTM PIC  X(15)          VALUE SPACES.
               10  WRK-OUT-DESIDE-RGTM PIC  X(10)          VALUE SPACES.
               10  WRK-OUT-NUMERO-RGTM PIC  X(09)          VALUE SPACES.
               10  WRK-OUT-PARCEL-RGTM PIC  9999           VALUE ZEROS.

               10  WRK-OUT-DTVENC-RGTM PIC  X(10)          VALUE SPACES.
               10  WRK-OUT-VALOR-RGTM  PIC  ZZ.ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
               10  WRK-OUT-TAXA-RGTM   PIC  ZZ9,99         VALUE ZEROS.
               10  WRK-OUT-FREQ-RGTM   PIC  X(04)          VALUE SPACES.
               10  WRK-OUT-TPJUR-RGTM  PIC  9(03)          VALUE ZEROS.
               10  WRK-OUT-TSMIN-RGTM.
                   15 WRK-OUT-TSMIN-RGTM-N
                                       PIC  9(16)V99       VALUE ZEROS.
               10  WRK-OUT-TSMAX-RGTM.
                   15 WRK-OUT-TSMAX-RGTM-N
                                       PIC  9(16)V99       VALUE ZEROS.
               10  WRK-OUT-FLAG-RGTM   PIC  X(01)          VALUE SPACES.
               10  WRK-OUT-CHAMA-RGTM  PIC  X(08)          VALUE SPACES.
               10  WRK-OUT-DATA-PRORROGADA   PIC X(10) VALUE SPACES.
               10  WRK-OUT-TXJURO-RGTM.
                   15  WRK-OUT-TXJURO-RGTM-N PIC  ZZ9,99999 VALUE ZEROS.
               10  WRK-OUT-FREQTX-RGTM PIC  X(04)          VALUE SPACES.
BRQ001         10  WRK-OUT-TPJURO-RGTM PIC  X(13)          VALUE SPACE.
               10 WRK-OUT-MINMAX-RGTM.
                 15 FILLER              PIC  X(06)         VALUE SPACES.

                 15 WRK-OUT-TXMIN-RGTM  PIC  99,999        VALUE ZEROS.
                 15 FILLER              PIC  X(06)         VALUE SPACES.
                 15 WRK-OUT-TXMAX-RGTM  PIC  99,999        VALUE ZEROS.
                 15 FILLER              PIC  X(01)         VALUE SPACES.
BRQ001         10 WRK-OUT-LTBSPR-RGTM   PIC  X(17)         VALUE SPACES.
BRQ001         10 WRK-OUT-CDBSPR-RGTM.
BRQ001           15 WRK-OUT-CDBSPR-RGTM-N
BRQ001                                  PIC  9(05)         VALUE ZEROS.
BRQ001         10 WRK-OUT-DSBSPR-RGTM   PIC  X(55)         VALUE SPACES.
               10  WRK-OUT-LITAUT-RGTM  PIC  X(12)         VALUE SPACES.
               10  WRK-OUT-CODAUT-RGTM.
                   15  WRK-OUT-CODAUT-RGTM-N
                                        PIC  ZZZZZZZZZ.
               10  WRK-OUT-DESAUT-RGTM PIC  X(22)      VALUE SPACES.
               10  WRK-OUT-JAUTOR-RGTM PIC  ZZZ.ZZZ.ZZ9,99 VALUE ZEROS.
               10  WRK-OUT-IAUTOR-RGTM PIC  ZZZ.ZZZ.ZZ9,99 VALUE ZEROS.
               10  WRK-OUT-TACAUT-RGTM PIC  ZZZ.ZZZ.ZZ9,99 VALUE ZEROS.
               10  WRK-OUT-TOTAUT-RGTM PIC  ZZZ.ZZZ.ZZ9,99 VALUE ZEROS.
           05  WRK-OUT-MENSA-RGTM      PIC  X(79)          VALUE SPACES.

      *----------------------------------------------------------------*

       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DE ERRO DA BRAD0660 ***'.
      *----------------------------------------------------------------*
       01  WRK-ERRO-BRAD0660.
           05  FILLER                  PIC  X(45)          VALUE
               '* RETURN-CODE DIFERENTE DE ZEROS NA BRAD0660'.
           05  FILLER                  PIC  X(12)          VALUE
               '- RET.COD ='.
           05  WRK-RETURN-CODE         PIC  9(02)          VALUE ZEROS.
           05  FILLER                  PIC  X(11)          VALUE
               ' - LOCAL ='.
           05  WRK-LOCAL-ERRO          PIC  9(02)          VALUE ZEROS.
           05  FILLER                  PIC  X(02)          VALUE ' *'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DA TELA DCOMRGTM - BRAD0660 ***'.
      *----------------------------------------------------------------*

       01  WRK-660-DCOMRGTM.
           05  WRK-660-LL-AREA-RGTM    PIC S9(004) COMP    VALUE +0092.

           05  WRK-660-LL-MENSAGEM-RGTM
                                       PIC S9(004) COMP    VALUE +0637.
           05  WRK-660-SENHAS-RGTM     PIC  9(004) COMP    VALUE 0037.
           05  WRK-660-COMANDO-RGTM    PIC  9(004) COMP    VALUE 0068.
           05  WRK-660-TIPOOPC-RGTM    PIC  9(004) COMP    VALUE 0001.
           05  WRK-660-TMSTAMP-RGTM    PIC  9(004) COMP    VALUE 0026.
           05  WRK-660-PASSO-RGTM      PIC  9(004) COMP    VALUE 0001.
           05  WRK-660-CHAMADO-RGTM    PIC  9(004) COMP    VALUE 0008.
           05  WRK-660-PAGINA-RGTM     PIC  9(004) COMP    VALUE 0004.
           05  WRK-660-TPSEL-RGTM      PIC  9(004) COMP    VALUE 0001.
           05  WRK-660-CODDESC-RGTM    PIC  9(004) COMP    VALUE 0003.
           05  WRK-660-PAGINA1-RGTM    PIC  9(004) COMP    VALUE 0004.
           05  WRK-660-NOMECL-RGTM     PIC  9(004) COMP    VALUE 0040.
           05  WRK-660-CGCCPF-RGTM     PIC  9(004) COMP    VALUE 0019.
           05  WRK-660-XCONTA-RGTM     PIC  9(004) COMP    VALUE 0023.
           05  WRK-660-NUPROD-RGTM     PIC  9(004) COMP    VALUE 0003.
           05  WRK-660-NMPROD-RGTM     PIC  9(004) COMP    VALUE 0006.
           05  WRK-660-NUSPRD-RGTM     PIC  9(004) COMP    VALUE 0003.
           05  WRK-660-NMSPRD-RGTM     PIC  9(004) COMP    VALUE 0010.
           05  WRK-660-OPERAC-RGTM     PIC  9(004) COMP    VALUE 0013.
           05  WRK-660-SITUAC-RGTM     PIC  9(004) COMP    VALUE 0015.

           05  WRK-660-TITSIT-RGTM     PIC  9(004) COMP    VALUE 0010.
           05  WRK-660-DESSIT-RGTM     PIC  9(004) COMP    VALUE 0009.
           05  WRK-660-PARCEL-RGTM     PIC  9(004) COMP    VALUE 0004.
           05  WRK-660-DTVENC-RGTM     PIC  9(004) COMP    VALUE 0010.
           05  WRK-660-VALOR-RGTM      PIC  9(004) COMP    VALUE 0017.
           05  WRK-660-TAXA-RGTM       PIC  9(004) COMP    VALUE 0006.
           05  WRK-660-FREQ-RGTM       PIC  9(004) COMP    VALUE 0004.
           05  WRK-660-TPJUR-RGTM      PIC  9(004) COMP    VALUE 0003.
           05  WRK-660-TSMIN-RGTM      PIC  9(004) COMP    VALUE 0018.
           05  WRK-660-TSMAX-RGTM      PIC  9(004) COMP    VALUE 0018.
           05  WRK-660-FLAG-RGTM       PIC  9(004) COMP    VALUE 0001.
           05  WRK-660-CHAMA-RGTM      PIC  9(004) COMP    VALUE 0008.
           05  WRK-660-DTPROR-RGTM     PIC  9(004) COMP    VALUE 0010.
           05  WRK-660-TXJURO-RGTM     PIC  9(004) COMP    VALUE 0009.
           05  WRK-660-FREQTX-RGTM     PIC  9(004) COMP    VALUE 0004.
BRQ001     05  WRK-660-TPJURO-RGTM     PIC  9(004) COMP    VALUE 0013.
           05  WRK-660-MINMAX-RGTM     PIC  9(004) COMP    VALUE 0025.
BRQ001     05  WRK-660-LTBSPR-RGTM     PIC  9(004) COMP    VALUE 0017.
BRQ001     05  WRK-660-CDBSPR-RGTM     PIC  9(004) COMP    VALUE 0005.
BRQ001     05  WRK-660-DSBSPR-RGTM     PIC  9(004) COMP    VALUE 0055.
           05  WRK-660-LITAUT-RGTM     PIC  9(004) COMP    VALUE 0012.

           05  WRK-660-CODAUT-RGTM     PIC  9(004) COMP    VALUE 0009.
           05  WRK-660-DESAUT-RGTM     PIC  9(004) COMP    VALUE 0022.
           05  WRK-660-JAUTOR-RGTM     PIC  9(004) COMP    VALUE 0014.
           05  WRK-660-IAUTOR-RGTM     PIC  9(004) COMP    VALUE 0014.
           05  WRK-660-TACAUT-RGTM     PIC  9(004) COMP    VALUE 0014.
           05  WRK-660-TOTAUT-RGTM     PIC  9(004) COMP    VALUE 0014.
           05  WRK-660-MENSA-RGTM      PIC  9(004) COMP    VALUE 0079.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA PARA MENSAGENS ***'.
      *----------------------------------------------------------------*
       COPY 'I#DCOMSY'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DA BRAD7100 ***'.
      *----------------------------------------------------------------*
       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*

       01  FILLER                      PIC  X(50)          VALUE
           '*** DCOM1807 - FIM DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  LNK-IO-PCB.
           05 LNK-IO-LTERM             PIC  X(08).
           05 FILLER                   PIC  X(02).
           05 LNK-IO-STATUS            PIC  X(02).
           05 FILLER                   PIC  X(12).
           05 LNK-IO-MODNAME           PIC  X(08).

       01  LNK-ALT-PCB.
           05 LNK-ALT-LTERM            PIC  X(08).
           05 FILLER                   PIC  X(02).
           05 LNK-ALT-STATUS           PIC  X(02).
           05 FILLER                   PIC  X(12).
           05 LNK-ALT-MODNAME          PIC  X(08).


      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

           ENTRY 'DLITCBL'             USING LNK-IO-PCB
                                             LNK-ALT-PCB.

           MOVE WRK-GU                 TO WRK-FUNCAO.

      *----------------------------------------------------------------*
      *     PROCEDIMENTOS INICIAIS                                     *
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

           MOVE SPACES                 TO WRK-OUT-DADOS-RGTM
                                          WRK-OUT-SENHAS-RGTM
                                          WRK-OUT-COMANDO-RGTM.

           PERFORM 2000-PROCESSAR-MENSAGEM.

           GO TO 0000-INICIAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*

      *     PROCESSAR MENSAGEM                                         *
      *----------------------------------------------------------------*
       2000-PROCESSAR-MENSAGEM         SECTION.
      *----------------------------------------------------------------*

           MOVE 'DCOMRGTM'             TO WRK-TELA

           IF  WRK-TRANSACAO           NOT EQUAL 'DCOM1807'
               PERFORM 2100-RECEBER-CONTROLE
           ELSE
               PERFORM 2200-PROCESSAR-DCOMRGTM
           END-IF.

           IF  WRK-TELA                EQUAL 'DCOMRGTM'
               PERFORM 2300-PROCESSAR-BRAD0660
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      *----------------------------------------------------------------*
      *     RECEBER CONTROLE                                           *
      *----------------------------------------------------------------*
       2100-RECEBER-CONTROLE           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM             TO WRK-COMU-AREA.
           MOVE WRK-COMU-TRANSACAO       TO WRK-INP-CHAMADO-RGTM
                                            WRK-OUT-CHAMADO-RGTM

           MOVE WRK-COMU-CHAMADOR        TO WRK-INP-CHAMA-RGTM
                                            WRK-OUT-CHAMA-RGTM

           MOVE WRK-COMU-DANO-OPER       TO WRK-INP-OPERAC-RGTM(1:4)
                                            WRK-OUT-OPERAC-RGTM(1:4)
           MOVE WRK-COMU-NSEQ-OPER       TO WRK-INP-OPERAC-RGTM(5:9)
                                            WRK-OUT-OPERAC-RGTM(5:9)
           MOVE WRK-COMU-OPCAO           TO WRK-INP-TIPOOPC-RGTM
                                            WRK-OUT-TIPOOPC-RGTM
           MOVE WRK-COMU-TP-SELECAO      TO WRK-INP-TPSEL-RGTM
                                            WRK-OUT-TPSEL-RGTM


           MOVE WRK-COMU-PAGINA-2        TO WRK-INP-PAGINA-RGTM
                                            WRK-OUT-PAGINA-RGTM

           MOVE WRK-COMU-PAGINA-1        TO WRK-INP-PAGINA1-RGTM
                                            WRK-OUT-PAGINA1-RGTM

           PERFORM 2105-CARREGA-CABECALHO

           INITIALIZE                  5473-ENTRADA
                                       5473-SAIDA.

           MOVE WRK-COD-USER-R         TO 5473-ENT-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO 5473-ENT-CTERM
BSI001     MOVE 'CP'                   TO 5473-ENT-FLAG-FILTRO
           MOVE WRK-COMU-OPERACAO      TO 5473-ENT-OPERACAO
           MOVE WRK-COMU-PARCELA       TO 5473-ENT-NPCELA-DESC-COML

           MOVE 'DCOM5473'             TO WRK-MODULO

           CALL WRK-MODULO             USING 5473-ENT-COD-RETORNO

                                             5473-SAIDA
                                             ERRO-AREA
                                             WRK-SQLCA.

           EVALUATE 5473-SAI-COD-RETORNO

              WHEN '0000'
                    PERFORM 2130-FORMATAR-TELA
                    MOVE 1                      TO WRK-OUT-PASSO-RGTM
                    MOVE 'CONSULTA EFETUADA COM SUCESSO TECLE <PF3> PARA
      -                  ' RETORNAR'        TO WRK-OUT-MENSA-RGTM

              WHEN '0099'
                    MOVE 'DB2'         TO ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO

              WHEN OTHER
                   MOVE 5473-SAI-MSG-RETORNO
                                       TO WRK-COMU-MENSAGEM
                   MOVE '*'            TO WRK-INCONSIS
                   PERFORM 2220-RETORNAR-CHAMADOR


           END-EVALUATE.


      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2105-CARREGA-CABECALHO          SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                     4174-ENTRADA-ROTEADOR
                                          4174-SAIDA-ROTEADOR

           MOVE '0000'                 TO 4174-ENT-COD-RETORNO
           MOVE SPACES                 TO 4174-ENT-MSG-RETORNO
           MOVE ZEROS                  TO 4174-ENT-RESTART
                                          4174-ENT-FLAG
           MOVE WRK-COD-USER           TO 4174-ENT-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO 4174-ENT-CTERM

           MOVE WRK-COMU-DANO-OPER     TO 4174-ENT-DANO-OPER-DESC
           MOVE WRK-COMU-NSEQ-OPER     TO 4174-ENT-NSEQ-OPER-DESC

           MOVE 'DCOM4174'             TO WRK-MODULO

           CALL WRK-MODULO          USING 4174-ENTRADA-ROTEADOR
                                          4174-SAIDA-ROTEADOR
                                          ERRO-AREA
                                          WRK-SQLCA.

           EVALUATE 4174-SAI-COD-RETORNO

              WHEN '0000'
               PERFORM 2107-FORMATA-CABECALHO

              WHEN '0099'
                    MOVE 'DB2'         TO ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO

              WHEN OTHER
                   MOVE 4174-SAI-MSG-RETORNO

                                       TO WRK-COMU-MENSAGEM
                   MOVE '*'            TO WRK-INCONSIS
                   PERFORM 2220-RETORNAR-CHAMADOR

           END-EVALUATE.


      *----------------------------------------------------------------*
       2105-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2107-FORMATA-CABECALHO          SECTION.
      *----------------------------------------------------------------*

           MOVE 4174-SAI-IPSSOA-DESC-COML  TO WRK-OUT-NOMECL-RGTM
           MOVE 4174-SAI-CPRODT            TO WRK-OUT-CODPRD-RGTM
           MOVE 4174-SAI-IABREV-PRODT      TO WRK-OUT-DESPRD-RGTM
           MOVE 4174-SAI-CSPROD-DESC-COML  TO WRK-OUT-CODSUB-RGTM
           MOVE 4174-SAI-CTPO-DESC-COML    TO WRK-OUT-CODDESC-RGTM
           MOVE 4174-SAI-IRSUMO-SPROD-DESC TO WRK-OUT-DESSUB-RGTM

           MOVE 4174-SAI-CSIT-DESC-COML    TO WRK-OUT-SITUAC-RGTM
           STRING 4174-SAI-CCNPJ-CPF(1:3)
           '.'    4174-SAI-CCNPJ-CPF(4:3)
           '.'    4174-SAI-CCNPJ-CPF(7:3)
           '/'    4174-SAI-CFLIAL-CNPJ
           '-'    4174-SAI-CCTRL-CNPJ-CPF
           DELIMITED BY SIZE             INTO WRK-OUT-CGCCPF-RGTM
           MOVE 4174-SAI-CBCO              TO WRK-BANCO-AUX
           MOVE 4174-SAI-CAG-BCRIA         TO WRK-AGENC-AUX
           MOVE 4174-SAI-CCTA-BCRIA-CLI    TO WRK-CONTA-AUX
           MOVE WRK-BCOAGECTA-AUX          TO WRK-OUT-XCONTA-RGTM
           MOVE 4174-SAI-IRSUMO-SIT-DESC   TO WRK-OUT-SITUAC-RGTM
           MOVE 4174-SAI-CSGL-TPO-TX-JURO  TO WRK-INP-TPJURO-RGTM
                                              WRK-OUT-TPJURO-RGTM
           MOVE 4174-SAI-CTPO-TX-JURO-DESC  TO WRK-OUT-TPJUR-RGTM.

           MOVE 4174-SAI-CSGL-TPO-TX-JURO   TO WRK-INP-TPJURO-RGTM
                                               WRK-OUT-TPJURO-RGTM.

           IF  4174-SAI-CCONTR-CONVE-DESC  GREATER ZEROS
               MOVE 'CONVENIO'             TO WRK-OUT-DESIDE-RGTM

               MOVE 4174-SAI-CCONTR-CONVE-DESC
                                           TO WRK-OUT-NUMERO-RGTM
           ELSE
              IF 4174-SAI-CCONTR-LIM-DESC   GREATER ZEROS
                 MOVE 'CONTRATO'           TO WRK-OUT-DESIDE-RGTM
                 MOVE 4174-SAI-CCONTR-LIM-DESC
                                           TO WRK-OUT-NUMERO-RGTM
              ELSE
                 IF 4174-SAI-NSMULA-OPER-DESC GREATER ZEROS
                      MOVE 'SIMULACAO'     TO WRK-OUT-DESIDE-RGTM
                      MOVE 4174-SAI-NSMULA-OPER-DESC
                                           TO WRK-OUT-NUMERO-RGTM
                 END-IF
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       2107-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*

      *     FORMATAR A TELA                                            *
      *----------------------------------------------------------------*
       2130-FORMATAR-TELA              SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-COMU-PARCELA            TO WRK-OUT-PARCEL-RGTM
           INSPECT 5473-SAI-DVCTO-PCELA-DESC
           REPLACING ALL '.' BY '/'
           MOVE 5473-SAI-DVCTO-PCELA-DESC   TO WRK-OUT-DTVENC-RGTM
           MOVE 5473-SAI-VPRINC-PCELA-DESC  TO WRK-OUT-VALOR-RGTM
           MOVE 5473-SAI-PTX-JURO-MES       TO WRK-OUT-TAXA-RGTM
           MOVE 5473-SAI-TFREQ-TAXA-PCELA   TO WRK-OUT-FREQ-RGTM
           INSPECT 5473-SAI-DVCTO-PCELA-PRROG
           REPLACING ALL '.' BY '/'
           MOVE 5473-SAI-DVCTO-PCELA-PRROG  TO WRK-INP-DATA-PRORROGADA
                                               WRK-OUT-DATA-PRORROGADA
           MOVE 5473-SAI-PTX-JURO-MES-PRROG TO WRK-OUT-TXJURO-RGTM-N
           MOVE 5473-SAI-TFREQ-TX-PRROG     TO WRK-INP-FREQTX-RGTM
                                               WRK-OUT-FREQTX-RGTM
           MOVE 5473-SAI-ITPO-TX-JURO       TO WRK-OUT-TPJURO-RGTM
           MOVE 5473-SAI-PMIN-TAXA-JURO     TO WRK-TAX-MIN

           MOVE 5473-SAI-PMAX-TAXA-JURO     TO WRK-TAX-MAX
           MOVE WRK-TAXA-MINMAX             TO WRK-OUT-MINMAX-RGTM
           MOVE 5473-SAI-CAUTRZ-ESPCL       TO WRK-OUT-CODAUT-RGTM-N
           IF   WRK-OUT-CODAUT-RGTM-N       EQUAL ZEROS
                MOVE SPACES                 TO WRK-OUT-CODAUT-RGTM
           END-IF

           IF  5473-SAI-CAUTRZ-ESPCL   GREATER ZEROS
               MOVE 'COD.AUTORIZ:'          TO WRK-OUT-LITAUT-RGTM
           END-IF

           MOVE 5473-SAI-IAUTRZ-ESPCL       TO WRK-OUT-DESAUT-RGTM
           MOVE 5473-SAI-VJURO-PCELA-DESC   TO WRK-OUT-JAUTOR-RGTM
           MOVE 5473-SAI-VIOF-PRROG-PCELA   TO WRK-OUT-IAUTOR-RGTM
           MOVE 5473-SAI-VTARIF-PRROG       TO WRK-OUT-TACAUT-RGTM
           MOVE 5473-SAI-VENCAR-PCELA-DESC  TO WRK-OUT-TOTAUT-RGTM.

BRQ001     IF  WRK-5473S-NBASE-LEGAL-RENEG  NOT EQUAL ZEROS
BRQ001         MOVE 'BASE PRORROGACAO:'     TO WRK-OUT-LTBSPR-RGTM
BRQ001         MOVE WRK-5473S-NBASE-LEGAL-RENEG
BRQ001                                      TO WRK-OUT-CDBSPR-RGTM-N

BRQ001         MOVE WRK-5473S-IBASE-LEGAL-RENEG(1:55)
BRQ001                                      TO WRK-OUT-DSBSPR-RGTM
BRQ001     ELSE
BRQ001         MOVE SPACES                  TO WRK-OUT-LTBSPR-RGTM
BRQ001                                         WRK-OUT-CDBSPR-RGTM
BRQ001                                         WRK-OUT-DSBSPR-RGTM
BRQ001     END-IF.
BRQ001
      *----------------------------------------------------------------*
       2130-99-FIM.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2200-PROCESSAR-DCOMRGTM         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM           TO WRK-INPUT-RGTM.

           PERFORM 2210-DEVOLVER-TELA

           EVALUATE TRUE

               WHEN WRK-INP-PFK-RGTM        EQUAL 'H' OR
                   (WRK-INP-PFK-RGTM        EQUAL '.' AND
                    WRK-INP-COMANDO-RGTM    EQUAL 'PFK01')
                    MOVE WRK-INP-MENSA-RGTM TO WRK-OUT-MENSA-RGTM

               WHEN WRK-INP-PFK-RGTM      EQUAL '3' OR
                   (WRK-INP-PFK-RGTM      EQUAL '.' AND
                    WRK-INP-COMANDO-RGTM  EQUAL 'PFK03')
                    PERFORM 2220-RETORNAR-CHAMADOR

               WHEN WRK-INP-PFK-RGTM      EQUAL 'X' OR
                   (WRK-INP-PFK-RGTM      EQUAL '.' AND
                    WRK-INP-COMANDO-RGTM  EQUAL 'PFK05')
                    PERFORM 2230-MENU-ROTINAS

               WHEN WRK-INP-PFK-RGTM      EQUAL 'Z' OR
                   (WRK-INP-PFK-RGTM      EQUAL '.' AND
                    WRK-INP-COMANDO-RGTM  EQUAL 'PFK10')
                    PERFORM 2270-MENU-DCOM

               WHEN (WRK-INP-PFK-RGTM     EQUAL '.' AND

                     WRK-INP-COMANDO-RGTM EQUAL SPACES)
                     MOVE WRK-INP-MENSA-RGTM TO WRK-OUT-MENSA-RGTM

               WHEN  OTHER
                     PERFORM 2210-DEVOLVER-TELA
                     MOVE 'PF INVALIDA'
                                       TO  WRK-OUT-MENSA-RGTM

           END-EVALUATE.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     DEVOLVER TELA                                              *
      *----------------------------------------------------------------*
       2210-DEVOLVER-TELA              SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-SENHAS-RGTM    TO WRK-OUT-SENHAS-RGTM

           MOVE WRK-INP-COMANDO-RGTM   TO WRK-OUT-COMANDO-RGTM
           MOVE WRK-INP-TIPOOPC-RGTM   TO WRK-OUT-TIPOOPC-RGTM
           MOVE WRK-INP-TPSEL-RGTM     TO WRK-OUT-TPSEL-RGTM
           MOVE WRK-INP-CODDESC-RGTM   TO WRK-OUT-CODDESC-RGTM
           MOVE WRK-INP-TMSTAMP-RGTM   TO WRK-OUT-TMSTAMP-RGTM
           MOVE WRK-INP-FREQTX-RGTM    TO WRK-OUT-FREQTX-RGTM
           MOVE WRK-INP-DATA-PRORROGADA  TO WRK-OUT-DATA-PRORROGADA
           MOVE WRK-INP-PASSO-RGTM     TO WRK-OUT-PASSO-RGTM
           MOVE WRK-INP-CHAMADO-RGTM   TO WRK-OUT-CHAMADO-RGTM
           MOVE WRK-INP-CHAMA-RGTM     TO WRK-OUT-CHAMA-RGTM
           MOVE WRK-INP-NOMECL-RGTM    TO WRK-OUT-NOMECL-RGTM
           MOVE WRK-INP-CGCCPF-RGTM    TO WRK-OUT-CGCCPF-RGTM
           MOVE WRK-INP-XCONTA-RGTM    TO WRK-OUT-XCONTA-RGTM
           MOVE WRK-INP-CODPRD-RGTM    TO WRK-OUT-CODPRD-RGTM
           MOVE WRK-INP-DESPRD-RGTM    TO WRK-OUT-DESPRD-RGTM
           MOVE WRK-INP-CODSUB-RGTM    TO WRK-OUT-CODSUB-RGTM
           MOVE WRK-INP-DESSUB-RGTM    TO WRK-OUT-DESSUB-RGTM
           MOVE WRK-INP-OPERAC-RGTM    TO WRK-OUT-OPERAC-RGTM
           MOVE WRK-INP-PAGINA-RGTM    TO WRK-OUT-PAGINA-RGTM
           MOVE WRK-INP-PAGINA1-RGTM   TO WRK-OUT-PAGINA1-RGTM
           MOVE WRK-INP-SITUAC-RGTM    TO WRK-OUT-SITUAC-RGTM

           MOVE WRK-INP-DESIDE-RGTM    TO WRK-OUT-DESIDE-RGTM
           MOVE WRK-INP-NUMERO-RGTM    TO WRK-OUT-NUMERO-RGTM
           MOVE WRK-INP-DTVENC-RGTM    TO WRK-OUT-DTVENC-RGTM
           MOVE WRK-INP-TPJURO-RGTM    TO WRK-OUT-TPJURO-RGTM
           MOVE WRK-INP-TPJUR-RGTM     TO WRK-OUT-TPJUR-RGTM
           MOVE WRK-INP-PARCEL-RGTM    TO WRK-OUT-PARCEL-RGTM
           MOVE WRK-INP-VALOR-RGTM     TO WRK-OUT-VALOR-RGTM
           MOVE WRK-INP-TAXA-RGTM      TO WRK-OUT-TAXA-RGTM
           MOVE WRK-INP-FREQ-RGTM      TO WRK-OUT-FREQ-RGTM
           MOVE WRK-INP-TSMIN-RGTM     TO WRK-OUT-TSMIN-RGTM
           MOVE WRK-INP-TSMAX-RGTM     TO WRK-OUT-TSMAX-RGTM
           MOVE WRK-INP-TXJURO-RGTM    TO WRK-OUT-TXJURO-RGTM
           MOVE WRK-INP-MINMAX-RGTM    TO WRK-OUT-MINMAX-RGTM
           MOVE WRK-INP-LITAUT-RGTM    TO WRK-OUT-LITAUT-RGTM
           MOVE WRK-INP-CODAUT-RGTM    TO WRK-OUT-CODAUT-RGTM
           MOVE WRK-INP-DESAUT-RGTM    TO WRK-OUT-DESAUT-RGTM
           MOVE WRK-INP-JAUTOR-RGTM    TO WRK-OUT-JAUTOR-RGTM
           MOVE WRK-INP-IAUTOR-RGTM    TO WRK-OUT-IAUTOR-RGTM
           MOVE WRK-INP-TACAUT-RGTM    TO WRK-OUT-TACAUT-RGTM
           MOVE WRK-INP-TOTAUT-RGTM    TO WRK-OUT-TOTAUT-RGTM.
BRQ001     MOVE WRK-INP-LTBSPR-RGTM    TO WRK-OUT-LTBSPR-RGTM.

BRQ001     MOVE WRK-INP-CDBSPR-RGTM    TO WRK-OUT-CDBSPR-RGTM.
BRQ001     MOVE WRK-INP-DSBSPR-RGTM    TO WRK-OUT-DSBSPR-RGTM.

      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     RETORNAR AO PROGRAMA CHAMADOR - DCOM1802/DCOM1801          *
      *----------------------------------------------------------------*
       2220-RETORNAR-CHAMADOR          SECTION.
      *----------------------------------------------------------------*

           MOVE LENGTH OF WRK-COMU-AREA
                                           TO WRK-COMU-LL
           MOVE 'DCOM1807'                 TO WRK-COMU-TRANSACAO
           MOVE WRK-INP-SENHAS-RGTM        TO WRK-COMU-SENHAS
           MOVE WRK-INP-COMANDO-RGTM       TO WRK-COMU-COMANDO
           MOVE WRK-INP-TMSTAMP-RGTM       TO WRK-COMU-TIMESTAMP
           MOVE WRK-INP-TIPOOPC-RGTM       TO WRK-COMU-OPCAO
           MOVE WRK-INP-TPSEL-RGTM         TO WRK-COMU-TP-SELECAO

           MOVE WRK-INP-PAGINA-RGTM        TO WRK-COMU-PAGINA-2
           MOVE WRK-INP-PAGINA1-RGTM       TO WRK-COMU-PAGINA-1
           MOVE WRK-INP-OPERAC-RGTM(1:4)   TO WRK-COMU-DANO-OPER
           MOVE WRK-INP-OPERAC-RGTM(5:9)   TO WRK-COMU-NSEQ-OPER
           MOVE 237                        TO WRK-COMU-BANCO
           MOVE WRK-INP-XCONTA-RGTM(5:5)   TO WRK-COMU-AGENCIA
           MOVE WRK-INP-XCONTA-RGTM(11:13) TO WRK-COMU-CONTA
           MOVE WRK-INP-CGCCPF-RGTM(1:3)   TO WRK-COMU-CCNPJ-CPF(1:3)
           MOVE WRK-INP-CGCCPF-RGTM(5:3)   TO WRK-COMU-CCNPJ-CPF(4:3)
           MOVE WRK-INP-CGCCPF-RGTM(9:3)   TO WRK-COMU-CCNPJ-CPF(7:3)
           MOVE WRK-INP-CGCCPF-RGTM(13:4)  TO WRK-COMU-CFLIAL-CNPJ
           MOVE WRK-INP-CGCCPF-RGTM(18:2)  TO WRK-COMU-CCTRL-CNPJCPF
           MOVE WRK-INP-CHAMA-RGTM         TO WRK-COMU-CHAMADOR
           MOVE WRK-INP-CHAMADO-RGTM       TO WRK-TELA
           MOVE WRK-CHNG                   TO WRK-FUNCAO
           MOVE WRK-COMU-AREA              TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      *----------------------------------------------------------------*
      *     RETORNAR AO MENU ROTINAS                                   *
      *----------------------------------------------------------------*
       2230-MENU-ROTINAS               SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                     WRK-COMU-AREA
           MOVE LENGTH                 OF WRK-COMU-AREA
                                       TO WRK-COMU-LL.
           MOVE WRK-CHNG               TO WRK-FUNCAO
           MOVE 'DCOM1807'             TO WRK-TELA
                                          WRK-COMU-TRANSACAO.
           MOVE WRK-INP-SENHAS-RGTM    TO WRK-COMU-SENHAS
           MOVE WRK-INP-COMANDO-RGTM   TO WRK-COMU-COMANDO
           MOVE '5'                    TO WRK-COMU-PFK
           MOVE WRK-COMU-AREA          TO WRK-MENSAGEM.


      *----------------------------------------------------------------*
       2230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      *----------------------------------------------------------------*
      *     RETORNAR AO MENU DCOM                                      *
      *----------------------------------------------------------------*
       2270-MENU-DCOM                  SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                     WRK-COMU-AREA
           MOVE LENGTH OF WRK-COMU-AREA
                                       TO WRK-COMU-LL
           MOVE WRK-CHNG               TO WRK-FUNCAO
           MOVE 'DCOM1807'             TO WRK-TELA
                                          WRK-COMU-TRANSACAO
           MOVE WRK-OUT-SENHAS-RGTM    TO WRK-COMU-SENHAS
           MOVE WRK-OUT-COMANDO-RGTM   TO WRK-COMU-COMANDO
           MOVE 'A'                    TO WRK-COMU-PFK
           MOVE WRK-COMU-AREA          TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2270-99-FIM.                    EXIT.
      *----------------------------------------------------------------*



      *----------------------------------------------------------------*
       2300-PROCESSAR-BRAD0660         SECTION.
      *----------------------------------------------------------------*

           MOVE LENGTH OF WRK-660-DCOMRGTM TO WRK-660-LL-AREA-RGTM
           MOVE LENGTH OF WRK-OUTPUT-RGTM  TO WRK-OUT-LL-RGTM
                                              WRK-660-LL-MENSAGEM-RGTM

           CALL 'BRAD0660'             USING WRK-OUTPUT-RGTM
                                             WRK-660-DCOMRGTM.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE RETURN-CODE        TO WRK-RETURN-CODE
               MOVE 10                 TO WRK-LOCAL-ERRO
               MOVE WRK-ERRO-BRAD0660  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.


           MOVE WRK-ISRT               TO WRK-FUNCAO
           MOVE WRK-OUTPUT-RGTM        TO WRK-MENSAGEM.

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
               MOVE 'DCOM1807'         TO ERR-PGM
           ELSE
               MOVE 'SENHAS02'         TO ERR-PGM
               MOVE 'DCOM1807'         TO ERR-MODULO
           END-IF.

           MOVE WRK-COD-USER           TO ERR-COD-USER
           MOVE WRK-COD-DEPTO          TO ERR-COD-DEPTO

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
