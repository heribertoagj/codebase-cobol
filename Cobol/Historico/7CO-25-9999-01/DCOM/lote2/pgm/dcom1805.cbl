      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID.     DCOM1805.
       AUTHOR.         FABRICA - ALTRAN.
      *================================================================*
      *                          A L T R A N                           *
      *----------------------------------------------------------------*
      *     PROGRAMA....: DCOM1805                                     *
      *     PROGRAMADOR.: FABRICA - ALTRAN.                            *
      *     ANALISTA....: EDGARD RODRIGUES/AQUIRA H.                   *
      *     DATA........: 17/04/2007                                   *
      *----------------------------------------------------------------*
      *     OBJETIVO....: EXCLUIR PRORROGACAO DE VENCIMENTO - DETALHES *
      *----------------------------------------------------------------*
      *     TELAS.......:                                              *
      *     DCOMRETM....: EXCLUIR PRORROGACAO DE VENCIMENTO - DETALHES *
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
BRQ001*                    ALTERACAO - BRQ                             *
BRQ001*----------------------------------------------------------------*
BRQ001*  ANALISTA BRQ..:   FABIO AUGUSTO FINK                          *
BRQ001*  ANALISTA DS...:   RICARDO JAMMAL     - BRADESCO - GRUPO 39    *
BRQ001*  DATA..........:   24/11/2017                                  *
BRQ001*----------------------------------------------------------------*
BRQ001*  OBJETIVO......: - INCLUIR CAMPO BASE LEGAL PRORROGACAO PARA   *
BRQ001*                    ADEQUAR FUNCIONALIDADE AO SICOR 4.10.       *
BRQ001*----------------------------------------------------------------*

BRQ001*  MODULO........:                                               *
BRQ001*         DCOM5218 - OBTER E CONSISTIR CODIGO BASE LEGAL PRORRO- *
BRQ001*                    GACAO BACEN.                                *
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
           '*** DCOM1805 - INICIO DA AREA DE WORKING ***'.
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
BRQ001     03  WRK-5473S-NBASE-LEGAL-RENEG PIC  9(005)     VALUE ZEROS.
BRQ001     03  WRK-5473S-IBASE-LEGAL-RENEG PIC  X(100)     VALUE SPACES.

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
           '*** AREA DA TELA DCOMRETM - INPUT ***'.

      *----------------------------------------------------------------*
       01  WRK-INPUT-RETM.
           05  FILLER                  PIC  X(18)          VALUE SPACES.
           05  WRK-INP-PFK-RETM        PIC  X(01)          VALUE SPACES.
           05  WRK-INP-SENHAS-RETM     PIC  X(37)          VALUE SPACES.
           05  WRK-INP-COMANDO-RETM    PIC  X(68)          VALUE SPACES.
           05  WRK-INP-DADOS-RETM.
               10  WRK-INP-TIPOOPC-RETM
                                       PIC  X(01)          VALUE SPACES.
               10  WRK-INP-TMSTAMP-RETM
                                       PIC  X(26)          VALUE SPACES.
               10  WRK-INP-PASSO-RETM  PIC  X(01)          VALUE SPACES.
               10  WRK-INP-CHAMADO-RETM
                                       PIC  X(08)          VALUE SPACES.
               10  WRK-INP-PAGINA-RETM PIC  X(04)          VALUE SPACES.
               10  WRK-INP-TPSEL-RETM  PIC  X(01)          VALUE SPACES.
               10  WRK-INP-CODDESC-RETM PIC 9(03)          VALUE ZEROS.
               10  WRK-INP-PAGINA1-RETM PIC 9(04)          VALUE ZEROS.
               10  WRK-INP-NOMECL-RETM PIC  X(40)          VALUE SPACES.
               10  WRK-INP-CGCCPF-RETM PIC  X(19)          VALUE SPACES.
               10  WRK-INP-XCONTA-RETM PIC  X(23)          VALUE SPACES.

               10  WRK-INP-CODPRD-RETM PIC  9(03)          VALUE ZEROS.
               10  WRK-INP-DESPRD-RETM PIC  X(06)          VALUE SPACES.
               10  WRK-INP-CODSUB-RETM PIC  9(03)          VALUE ZEROS.
               10  WRK-INP-DESSUB-RETM PIC  X(10)          VALUE SPACES.
               10  WRK-INP-OPERAC-RETM PIC  9(13)          VALUE ZEROS.
               10  WRK-INP-SITUAC-RETM PIC  X(15)          VALUE SPACES.
               10  WRK-INP-DESIDE-RETM PIC  X(10)          VALUE SPACES.
               10  WRK-INP-NUMERO-RETM PIC  X(09)          VALUE SPACES.
               10  WRK-INP-PARCEL-RETM PIC  9999           VALUE ZEROS.
               10  WRK-INP-DTVENC-RETM PIC  X(10)          VALUE SPACES.
               10  WRK-INP-VALOR-RETM  PIC  ZZ.ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
               10  WRK-INP-TAXA-RETM   PIC  ZZ9,99         VALUE ZEROS.
               10  WRK-INP-FREQ-RETM   PIC  X(04)          VALUE SPACES.
               10  WRK-INP-TPJUR-RETM  PIC  9(03)          VALUE ZEROS.
               10  WRK-INP-TSMIN-RETM.
                   15 WRK-INP-TSMIN-RETM-N
                                       PIC  9(16)V99        VALUE ZEROS.
               10  WRK-INP-TSMAX-RETM.
                   15 WRK-INP-TSMAX-RETM-N
                                      PIC  9(16)V99        VALUE ZEROS.

               10  WRK-INP-FLAG-RETM   PIC  X(01)          VALUE SPACES.
               10  WRK-INP-CHAMA-RETM    PIC  X(08)      VALUE SPACES.
               10  WRK-INP-DTPROR-RETM   PIC X(10)     VALUE SPACES.
               10  WRK-INP-TXJURO-RETM.
                  15  WRK-INP-TXJURO-RETM-N PIC  ZZ9,99999 VALUE ZEROS.
               10  WRK-INP-FREQTX-RETM PIC  X(04)          VALUE SPACES.
BRQ001         10  WRK-INP-TPJURO-RETM PIC  X(13)          VALUE SPACES.
               10 WRK-INP-MINMAX-RETM.
                 15 FILLER              PIC  X(06)         VALUE SPACES.
                 15 WRK-INP-TXMIN-RETM  PIC  99,999  VALUE ZEROS.
                 15 FILLER              PIC  X(06)    VALUE SPACES.
                 15 WRK-INP-TXMAX-RETM  PIC  99,999   VALUE ZEROS.
                 15 FILLER              PIC  X(01)         VALUE SPACES.
BRQ001         10 WRK-INP-LTBSPR-RETM   PIC  X(17)         VALUE SPACES.
BRQ001         10 WRK-INP-CDBSPR-RETM.
BRQ001           15 WRK-INP-CDBSPR-RETM-N
BRQ001                                  PIC  9(05)         VALUE ZEROS.
BRQ001         10 WRK-INP-DSBSPR-RETM   PIC  X(55)         VALUE SPACES.
               10 WRK-INP-LITAUT-RETM   PIC  X(12)         VALUE SPACES.
               10 WRK-INP-CODAUT-RETM.
                 15 WRK-INP-CODAUT-RETM-N

                                        PIC  9(09).
               10 WRK-INP-DESAUT-RETM   PIC  X(22)        VALUE SPACES.
               10 WRK-INP-JAUTOR-RETM   PIC  ZZZ.ZZZ.ZZ9,99 VALUE ZEROS.
               10 WRK-INP-IAUTOR-RETM   PIC  ZZZ.ZZZ.ZZ9,99 VALUE ZEROS.
               10 WRK-INP-TACAUT-RETM   PIC  ZZZ.ZZZ.ZZ9,99 VALUE ZEROS.
               10 WRK-INP-TOTAUT-RETM   PIC  ZZZ.ZZZ.ZZ9,99 VALUE ZEROS.
           05  WRK-INP-MENSA-RETM       PIC  X(79)          VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(50)      VALUE
           '*** AREA DA TELA DCOMRETM  - OUTPUT ***'.
      *----------------------------------------------------------------*
       01  WRK-OUTPUT-RETM.
           05  WRK-OUT-LL-RETM         PIC S9(04) COMP     VALUE +0637.
           05  WRK-OUT-ZZ-RETM         PIC S9(04) COMP     VALUE ZEROS.
           05  WRK-OUT-SENHAS-RETM     PIC  X(37)          VALUE SPACES.
           05  WRK-OUT-COMANDO-RETM    PIC  X(68)          VALUE SPACES.
           05  WRK-OUT-DADOS-RETM.
               10  WRK-OUT-TIPOOPC-RETM
                                       PIC  X(01)          VALUE SPACES.
               10  WRK-OUT-TMSTAMP-RETM

                                       PIC  X(26)          VALUE SPACES.
               10  WRK-OUT-PASSO-RETM  PIC  X(01)          VALUE SPACES.
               10  WRK-OUT-CHAMADO-RETM
                                       PIC  X(08)          VALUE SPACES.
               10  WRK-OUT-PAGINA-RETM PIC  X(04)          VALUE SPACES.
               10  WRK-OUT-TPSEL-RETM  PIC  X(01)          VALUE SPACES.
               10  WRK-OUT-CODDESC-RETM PIC  9(03)         VALUE ZEROS.
               10  WRK-OUT-PAGINA1-RETM PIC  9(04)         VALUE ZEROS.
               10  WRK-OUT-NOMECL-RETM PIC  X(40)          VALUE SPACES.
               10  WRK-OUT-CGCCPF-RETM PIC  X(19)          VALUE SPACES.
               10  WRK-OUT-XCONTA-RETM PIC  X(23)          VALUE SPACES.
               10  WRK-OUT-CODPRD-RETM PIC  9(03)          VALUE ZEROS.
               10  WRK-OUT-DESPRD-RETM PIC  X(06)          VALUE SPACES.
               10  WRK-OUT-CODSUB-RETM PIC  9(03)          VALUE ZEROS.
               10  WRK-OUT-DESSUB-RETM PIC  X(10)          VALUE SPACES.
               10  WRK-OUT-OPERAC-RETM PIC  9(13)          VALUE ZEROS.
               10  WRK-OUT-SITUAC-RETM PIC  X(15)          VALUE SPACES.
               10  WRK-OUT-DESIDE-RETM PIC  X(10)          VALUE SPACES.
               10  WRK-OUT-NUMERO-RETM PIC  X(09)          VALUE SPACES.
               10  WRK-OUT-PARCEL-RETM PIC  9999           VALUE ZEROS.
               10  WRK-OUT-DTVENC-RETM PIC  X(10)          VALUE SPACES.

               10  WRK-OUT-VALOR-RETM  PIC  ZZ.ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
               10  WRK-OUT-TAXA-RETM   PIC  ZZ9,99         VALUE ZEROS.
               10  WRK-OUT-FREQ-RETM   PIC  X(04)          VALUE SPACES.
               10  WRK-OUT-TPJUR-RETM  PIC  9(03)          VALUE ZEROS.
               10  WRK-OUT-TSMIN-RETM.
                   15 WRK-OUT-TSMIN-RETM-N
                                       PIC  9(16)V99       VALUE ZEROS.
               10  WRK-OUT-TSMAX-RETM.
                   15 WRK-OUT-TSMAX-RETM-N
                                       PIC  9(16)V99       VALUE ZEROS.
               10  WRK-OUT-FLAG-RETM   PIC  X(01)          VALUE SPACES.
               10  WRK-OUT-CHAMA-RETM  PIC  X(08)          VALUE SPACES.
               10  WRK-OUT-DTPROR-RETM       PIC X(10) VALUE SPACES.
               10  WRK-OUT-TXJURO-RETM.
                   15  WRK-OUT-TXJURO-RETM-N PIC  ZZ9,99999 VALUE ZEROS.
               10  WRK-OUT-FREQTX-RETM PIC  X(04)          VALUE SPACES.
BRQ001         10  WRK-OUT-TPJURO-RETM PIC  X(13)          VALUE SPACE.
               10 WRK-OUT-MINMAX-RETM.
                 15 FILLER              PIC  X(06)         VALUE SPACES.
                 15 WRK-OUT-TXMIN-RETM  PIC  99,999        VALUE ZEROS.

                 15 FILLER              PIC  X(06)         VALUE SPACES.
                 15 WRK-OUT-TXMAX-RETM  PIC  99,999        VALUE ZEROS.
                 15 FILLER              PIC  X(01)         VALUE SPACES.
BRQ001         10 WRK-OUT-LTBSPR-RETM   PIC  X(17)         VALUE SPACES.
BRQ001         10 WRK-OUT-CDBSPR-RETM.
BRQ001           15 WRK-OUT-CDBSPR-RETM-N
BRQ001                                  PIC  9(05)         VALUE ZEROS.
BRQ001         10  WRK-OUT-DSBSPR-RETM  PIC  X(55)         VALUE SPACES.
               10  WRK-OUT-LITAUT-RETM  PIC  X(12)         VALUE SPACES.
               10  WRK-OUT-CODAUT-RETM.
                   15  WRK-OUT-CODAUT-RETM-N
                                        PIC  ZZZZZZZZZ.
               10  WRK-OUT-DESAUT-RETM PIC  X(22)      VALUE SPACES.
               10  WRK-OUT-JAUTOR-RETM PIC  ZZZ.ZZZ.ZZ9,99 VALUE ZEROS.
               10  WRK-OUT-IAUTOR-RETM PIC  ZZZ.ZZZ.ZZ9,99 VALUE ZEROS.
               10  WRK-OUT-TACAUT-RETM PIC  ZZZ.ZZZ.ZZ9,99 VALUE ZEROS.
               10  WRK-OUT-TOTAUT-RETM PIC  ZZZ.ZZZ.ZZ9,99 VALUE ZEROS.
           05  WRK-OUT-MENSA-RETM      PIC  X(79)          VALUE SPACES.

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
           '*** AREA DA TELA DCOMRETM - BRAD0660 ***'.
      *----------------------------------------------------------------*

       01  WRK-660-DCOMRETM.
           05  WRK-660-LL-AREA-RETM    PIC S9(004) COMP    VALUE +0092.
           05  WRK-660-LL-MENSAGEM-RETM

                                       PIC S9(004) COMP    VALUE +0637.
           05  WRK-660-SENHAS-RETM     PIC  9(004) COMP    VALUE 0037.
           05  WRK-660-COMANDO-RETM    PIC  9(004) COMP    VALUE 0068.
           05  WRK-660-TIPOOPC-RETM    PIC  9(004) COMP    VALUE 0001.
           05  WRK-660-TMSTAMP-RETM    PIC  9(004) COMP    VALUE 0026.
           05  WRK-660-PASSO-RETM      PIC  9(004) COMP    VALUE 0001.
           05  WRK-660-CHAMADO-RETM    PIC  9(004) COMP    VALUE 0008.
           05  WRK-660-PAGINA-RETM     PIC  9(004) COMP    VALUE 0004.
           05  WRK-660-TPSEL-RETM      PIC  9(004) COMP    VALUE 0001.
           05  WRK-660-CODDESC-RETM    PIC  9(004) COMP    VALUE 0003.
           05  WRK-660-PAGINA1-RETM    PIC  9(004) COMP    VALUE 0004.
           05  WRK-660-NOMECL-RETM     PIC  9(004) COMP    VALUE 0040.
           05  WRK-660-CGCCPF-RETM     PIC  9(004) COMP    VALUE 0019.
           05  WRK-660-XCONTA-RETM     PIC  9(004) COMP    VALUE 0023.
           05  WRK-660-NUPROD-RETM     PIC  9(004) COMP    VALUE 0003.
           05  WRK-660-NMPROD-RETM     PIC  9(004) COMP    VALUE 0006.
           05  WRK-660-NUSPRD-RETM     PIC  9(004) COMP    VALUE 0003.
           05  WRK-660-NMSPRD-RETM     PIC  9(004) COMP    VALUE 0010.
           05  WRK-660-OPERAC-RETM     PIC  9(004) COMP    VALUE 0013.
           05  WRK-660-SITUAC-RETM     PIC  9(004) COMP    VALUE 0015.
           05  WRK-660-TITSIT-RETM     PIC  9(004) COMP    VALUE 0010.

           05  WRK-660-DESSIT-RETM     PIC  9(004) COMP    VALUE 0009.
           05  WRK-660-PARCEL-RETM     PIC  9(004) COMP    VALUE 0004.
           05  WRK-660-DTVENC-RETM     PIC  9(004) COMP    VALUE 0010.
           05  WRK-660-VALOR-RETM      PIC  9(004) COMP    VALUE 0017.
           05  WRK-660-TAXA-RETM       PIC  9(004) COMP    VALUE 0006.
           05  WRK-660-FREQ-RETM       PIC  9(004) COMP    VALUE 0004.
           05  WRK-660-TPJUR-RETM      PIC  9(004) COMP    VALUE 0003.
           05  WRK-660-TSMIN-RETM      PIC  9(004) COMP    VALUE 0018.
           05  WRK-660-TSMAX-RETM      PIC  9(004) COMP    VALUE 0018.
           05  WRK-660-FLAG-RETM       PIC  9(004) COMP    VALUE 0001.
           05  WRK-660-CHAMA-RETM      PIC  9(004) COMP    VALUE 0008.
           05  WRK-660-DTPROR-RETM     PIC  9(004) COMP    VALUE 0010.
           05  WRK-660-TXJURO-RETM     PIC  9(004) COMP    VALUE 0009.
           05  WRK-660-FREQTX-RETM     PIC  9(004) COMP    VALUE 0004.
BRQ001     05  WRK-660-TPJURO-RETM     PIC  9(004) COMP    VALUE 0013.
           05  WRK-660-MINMAX-RETM     PIC  9(004) COMP    VALUE 0025.
BRQ001     05  WRK-660-LTBSPR-RETM     PIC  9(004) COMP    VALUE 0017.
BRQ001     05  WRK-660-CDBSPR-RETM     PIC  9(004) COMP    VALUE 0005.
BRQ001     05  WRK-660-DSBSPR-RETM     PIC  9(004) COMP    VALUE 0055.
           05  WRK-660-LITAUT-RETM     PIC  9(004) COMP    VALUE 0012.
           05  WRK-660-CODAUT-RETM     PIC  9(004) COMP    VALUE 0009.

           05  WRK-660-DESAUT-RETM     PIC  9(004) COMP    VALUE 0022.
           05  WRK-660-JAUTOR-RETM     PIC  9(004) COMP    VALUE 0014.
           05  WRK-660-IAUTOR-RETM     PIC  9(004) COMP    VALUE 0014.
           05  WRK-660-TACAUT-RETM     PIC  9(004) COMP    VALUE 0014.
           05  WRK-660-TOTAUT-RETM     PIC  9(004) COMP    VALUE 0014.
           05  WRK-660-MENSA-RETM      PIC  9(004) COMP    VALUE 0079.

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

           '*** DCOM1805 - FIM DA AREA DE WORKING ***'.
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

           MOVE SPACES                 TO WRK-OUT-DADOS-RETM
                                          WRK-OUT-SENHAS-RETM
                                          WRK-OUT-COMANDO-RETM.

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

           MOVE 'DCOMRETM'             TO WRK-TELA

           IF  WRK-TRANSACAO           NOT EQUAL 'DCOM1805'
               PERFORM 2100-RECEBER-CONTROLE
           ELSE
               PERFORM 2200-PROCESSAR-DCOMRETM
           END-IF.

           IF  WRK-TELA                EQUAL 'DCOMRETM'
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
           MOVE WRK-COMU-TRANSACAO       TO WRK-INP-CHAMADO-RETM
                                            WRK-OUT-CHAMADO-RETM

           MOVE WRK-COMU-CHAMADOR        TO WRK-INP-CHAMA-RETM
                                            WRK-OUT-CHAMA-RETM

           MOVE WRK-COMU-DANO-OPER       TO WRK-INP-OPERAC-RETM(1:4)
                                            WRK-OUT-OPERAC-RETM(1:4)
           MOVE WRK-COMU-NSEQ-OPER       TO WRK-INP-OPERAC-RETM(5:9)
                                            WRK-OUT-OPERAC-RETM(5:9)
           MOVE WRK-COMU-OPCAO           TO WRK-INP-TIPOOPC-RETM
                                            WRK-OUT-TIPOOPC-RETM
           MOVE WRK-COMU-TP-SELECAO      TO WRK-INP-TPSEL-RETM
                                            WRK-OUT-TPSEL-RETM


           MOVE WRK-COMU-PAGINA-2        TO WRK-INP-PAGINA-RETM
                                            WRK-OUT-PAGINA-RETM

           MOVE WRK-COMU-PAGINA-1        TO WRK-INP-PAGINA1-RETM
                                            WRK-OUT-PAGINA1-RETM

           PERFORM 2105-CARREGA-CABECALHO

           INITIALIZE                  5473-ENTRADA
                                       5473-SAIDA.

           MOVE WRK-COD-USER-R         TO 5473-ENT-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO 5473-ENT-CTERM
           MOVE 'EP'                   TO 5473-ENT-FLAG-FILTRO
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
                    MOVE 'TECLE <PF6> PARA EXCLUIR OU <PF3> PARA RETORNA
      -                  'R'                    TO WRK-OUT-MENSA-RETM

              WHEN '0099'
                    MOVE 'DB2'         TO ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO

              WHEN OTHER
                   MOVE 5473-SAI-MSG-RETORNO
                                       TO WRK-COMU-MENSAGEM
                   MOVE '*'            TO WRK-INCONSIS
                   PERFORM 2220-RETORNAR-CHAMADOR

           END-EVALUATE.


BRQ001     IF  4174-SAI-CTPO-DESC-COML EQUAL 007 OR 008
BRQ001         IF  WRK-5473S-NBASE-LEGAL-RENEG   GREATER ZEROS
BRQ001             MOVE 'BASE PRORROGACAO:'
BRQ001                                 TO WRK-OUT-LTBSPR-RETM
BRQ001             MOVE WRK-5473S-NBASE-LEGAL-RENEG
BRQ001                                 TO WRK-OUT-CDBSPR-RETM-N
BRQ001             MOVE WRK-5473S-IBASE-LEGAL-RENEG
BRQ001                                 TO WRK-OUT-DSBSPR-RETM
BRQ001         END-IF
BRQ001     END-IF.
BRQ001
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

           MOVE 4174-SAI-IPSSOA-DESC-COML  TO WRK-OUT-NOMECL-RETM
           MOVE 4174-SAI-CPRODT            TO WRK-OUT-CODPRD-RETM
           MOVE 4174-SAI-IABREV-PRODT      TO WRK-OUT-DESPRD-RETM
           MOVE 4174-SAI-CSPROD-DESC-COML  TO WRK-OUT-CODSUB-RETM
           MOVE 4174-SAI-CTPO-DESC-COML    TO WRK-OUT-CODDESC-RETM
           MOVE 4174-SAI-IRSUMO-SPROD-DESC TO WRK-OUT-DESSUB-RETM
           MOVE 4174-SAI-CSIT-DESC-COML    TO WRK-OUT-SITUAC-RETM
      *    MOVE 4174-SAI-IRSUMO-SIT-DESC   TO WRK-OUT-DESIDE-RETM
           STRING 4174-SAI-CCNPJ-CPF(1:3)
           '.'    4174-SAI-CCNPJ-CPF(4:3)
           '.'    4174-SAI-CCNPJ-CPF(7:3)
           '/'    4174-SAI-CFLIAL-CNPJ
           '-'    4174-SAI-CCTRL-CNPJ-CPF
           DELIMITED BY SIZE             INTO WRK-OUT-CGCCPF-RETM
           MOVE 4174-SAI-CBCO              TO WRK-BANCO-AUX
           MOVE 4174-SAI-CAG-BCRIA         TO WRK-AGENC-AUX
           MOVE 4174-SAI-CCTA-BCRIA-CLI    TO WRK-CONTA-AUX
           MOVE WRK-BCOAGECTA-AUX          TO WRK-OUT-XCONTA-RETM
           MOVE 4174-SAI-IRSUMO-SIT-DESC   TO WRK-OUT-SITUAC-RETM

           MOVE 4174-SAI-CSGL-TPO-TX-JURO  TO WRK-INP-TPJURO-RETM
                                              WRK-OUT-TPJURO-RETM
           MOVE 4174-SAI-CTPO-TX-JURO-DESC  TO WRK-OUT-TPJUR-RETM.

           MOVE 4174-SAI-CSGL-TPO-TX-JURO   TO WRK-INP-TPJURO-RETM
                                               WRK-OUT-TPJURO-RETM.




           IF  4174-SAI-CCONTR-CONVE-DESC  GREATER ZEROS
               MOVE 'CONVENIO'             TO WRK-OUT-DESIDE-RETM
               MOVE 4174-SAI-CCONTR-CONVE-DESC
                                           TO WRK-OUT-NUMERO-RETM
           ELSE
              IF 4174-SAI-CCONTR-LIM-DESC   GREATER ZEROS
                 MOVE 'CONTRATO'           TO WRK-OUT-DESIDE-RETM
                 MOVE 4174-SAI-CCONTR-LIM-DESC
                                           TO WRK-OUT-NUMERO-RETM
              ELSE
                 IF 4174-SAI-NSMULA-OPER-DESC GREATER ZEROS

                      MOVE 'SIMULACAO'     TO WRK-OUT-DESIDE-RETM
                      MOVE 4174-SAI-NSMULA-OPER-DESC
                                           TO WRK-OUT-NUMERO-RETM
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

           MOVE WRK-COMU-PARCELA            TO WRK-OUT-PARCEL-RETM
           INSPECT 5473-SAI-DVCTO-PCELA-DESC
           REPLACING ALL '.' BY '/'
           MOVE 5473-SAI-DVCTO-PCELA-DESC   TO WRK-OUT-DTVENC-RETM

           MOVE 5473-SAI-VPRINC-PCELA-DESC  TO WRK-OUT-VALOR-RETM
           MOVE 5473-SAI-PTX-JURO-MES       TO WRK-OUT-TAXA-RETM
           MOVE 5473-SAI-TFREQ-TAXA-PCELA   TO WRK-OUT-FREQ-RETM
           INSPECT 5473-SAI-DVCTO-PCELA-PRROG
           REPLACING ALL '.' BY '/'
           MOVE 5473-SAI-DVCTO-PCELA-PRROG  TO WRK-INP-DTPROR-RETM
                                               WRK-OUT-DTPROR-RETM
           MOVE 5473-SAI-PTX-JURO-MES-PRROG TO WRK-OUT-TXJURO-RETM-N
           MOVE 5473-SAI-TFREQ-TX-PRROG     TO WRK-INP-FREQTX-RETM
                                               WRK-OUT-FREQTX-RETM
           MOVE 5473-SAI-ITPO-TX-JURO       TO WRK-OUT-TPJURO-RETM
           MOVE 5473-SAI-PMIN-TAXA-JURO     TO WRK-TAX-MIN
           MOVE 5473-SAI-PMAX-TAXA-JURO     TO WRK-TAX-MAX
           MOVE WRK-TAXA-MINMAX             TO WRK-OUT-MINMAX-RETM
           MOVE 5473-SAI-CAUTRZ-ESPCL       TO WRK-OUT-CODAUT-RETM-N

           IF  5473-SAI-CAUTRZ-ESPCL   GREATER ZEROS
               MOVE 'COD.AUTORIZ:'          TO WRK-OUT-LITAUT-RETM
           END-IF

           MOVE 5473-SAI-IAUTRZ-ESPCL       TO WRK-OUT-DESAUT-RETM

           MOVE 5473-SAI-VJURO-PCELA-DESC   TO WRK-OUT-JAUTOR-RETM
           MOVE 5473-SAI-VIOF-PRROG-PCELA   TO WRK-OUT-IAUTOR-RETM
           MOVE 5473-SAI-VTARIF-PRROG       TO WRK-OUT-TACAUT-RETM
           MOVE 5473-SAI-VENCAR-PCELA-DESC  TO WRK-OUT-TOTAUT-RETM.


      *----------------------------------------------------------------*
       2130-99-FIM.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2200-PROCESSAR-DCOMRETM         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM           TO WRK-INPUT-RETM.

           PERFORM 2210-DEVOLVER-TELA

           EVALUATE TRUE
               WHEN WRK-INP-PFK-RETM        EQUAL 'H' OR
                   (WRK-INP-PFK-RETM        EQUAL '.' AND

                    WRK-INP-COMANDO-RETM    EQUAL 'PFK01')
                    MOVE WRK-INP-MENSA-RETM TO WRK-OUT-MENSA-RETM

               WHEN WRK-INP-PFK-RETM      EQUAL '3' OR
                   (WRK-INP-PFK-RETM      EQUAL '.' AND
                    WRK-INP-COMANDO-RETM  EQUAL 'PFK03')
                    PERFORM 2220-RETORNAR-CHAMADOR

               WHEN WRK-INP-PFK-RETM      EQUAL 'X' OR
                   (WRK-INP-PFK-RETM      EQUAL '.' AND
                    WRK-INP-COMANDO-RETM  EQUAL 'PFK05')
                    PERFORM 2230-MENU-ROTINAS

               WHEN WRK-INP-PFK-RETM      EQUAL '6' OR
                   (WRK-INP-PFK-RETM      EQUAL '.' AND
                    WRK-INP-COMANDO-RETM  EQUAL 'PFK06')
                    PERFORM 2250-CONFIRMA

               WHEN WRK-INP-PFK-RETM      EQUAL 'Z' OR
                   (WRK-INP-PFK-RETM      EQUAL '.' AND
                    WRK-INP-COMANDO-RETM  EQUAL 'PFK10')

                    PERFORM 2270-MENU-DCOM

               WHEN (WRK-INP-PFK-RETM     EQUAL '.' AND
                     WRK-INP-COMANDO-RETM EQUAL SPACES)

                     PERFORM 2280-TRATAR-ENTER

               WHEN  OTHER
                     PERFORM 2210-DEVOLVER-TELA
                     MOVE 'PF INVALIDA'
                                       TO  WRK-OUT-MENSA-RETM

           END-EVALUATE.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     DEVOLVER TELA                                              *
      *----------------------------------------------------------------*

       2210-DEVOLVER-TELA              SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-SENHAS-RETM    TO WRK-OUT-SENHAS-RETM
           MOVE WRK-INP-COMANDO-RETM   TO WRK-OUT-COMANDO-RETM
           MOVE WRK-INP-TIPOOPC-RETM   TO WRK-OUT-TIPOOPC-RETM
           MOVE WRK-INP-TPSEL-RETM     TO WRK-OUT-TPSEL-RETM
           MOVE WRK-INP-CODDESC-RETM   TO WRK-OUT-CODDESC-RETM
           MOVE WRK-INP-TMSTAMP-RETM   TO WRK-OUT-TMSTAMP-RETM
           MOVE WRK-INP-DTPROR-RETM    TO WRK-OUT-DTPROR-RETM
           MOVE WRK-INP-FREQTX-RETM    TO WRK-OUT-FREQTX-RETM
           MOVE WRK-INP-CHAMADO-RETM   TO WRK-OUT-CHAMADO-RETM
           MOVE WRK-INP-CHAMA-RETM     TO WRK-OUT-CHAMA-RETM
           MOVE WRK-INP-NOMECL-RETM    TO WRK-OUT-NOMECL-RETM
           MOVE WRK-INP-CGCCPF-RETM    TO WRK-OUT-CGCCPF-RETM
           MOVE WRK-INP-XCONTA-RETM    TO WRK-OUT-XCONTA-RETM
           MOVE WRK-INP-CODPRD-RETM    TO WRK-OUT-CODPRD-RETM
           MOVE WRK-INP-DESPRD-RETM    TO WRK-OUT-DESPRD-RETM
           MOVE WRK-INP-CODSUB-RETM    TO WRK-OUT-CODSUB-RETM
           MOVE WRK-INP-DESSUB-RETM    TO WRK-OUT-DESSUB-RETM
           MOVE WRK-INP-OPERAC-RETM    TO WRK-OUT-OPERAC-RETM

           MOVE WRK-INP-PAGINA-RETM    TO WRK-OUT-PAGINA-RETM
           MOVE WRK-INP-PAGINA1-RETM   TO WRK-OUT-PAGINA1-RETM
           MOVE WRK-INP-SITUAC-RETM    TO WRK-OUT-SITUAC-RETM
           MOVE WRK-INP-DESIDE-RETM    TO WRK-OUT-DESIDE-RETM
           MOVE WRK-INP-NUMERO-RETM    TO WRK-OUT-NUMERO-RETM
           MOVE WRK-INP-DTVENC-RETM    TO WRK-OUT-DTVENC-RETM
           MOVE WRK-INP-TPJURO-RETM    TO WRK-OUT-TPJURO-RETM
           MOVE WRK-INP-TPJUR-RETM     TO WRK-OUT-TPJUR-RETM
           MOVE WRK-INP-PARCEL-RETM    TO WRK-OUT-PARCEL-RETM
           MOVE WRK-INP-VALOR-RETM     TO WRK-OUT-VALOR-RETM
           MOVE WRK-INP-TAXA-RETM      TO WRK-OUT-TAXA-RETM
           MOVE WRK-INP-FREQ-RETM      TO WRK-OUT-FREQ-RETM
           MOVE WRK-INP-TSMIN-RETM     TO WRK-OUT-TSMIN-RETM
           MOVE WRK-INP-TSMAX-RETM     TO WRK-OUT-TSMAX-RETM
           MOVE WRK-INP-TXJURO-RETM    TO WRK-OUT-TXJURO-RETM
           MOVE WRK-INP-MINMAX-RETM    TO WRK-OUT-MINMAX-RETM
BRQ001     MOVE WRK-INP-LTBSPR-RETM    TO WRK-OUT-LTBSPR-RETM.
BRQ001     MOVE WRK-INP-CDBSPR-RETM    TO WRK-OUT-CDBSPR-RETM.
BRQ001     MOVE WRK-INP-DSBSPR-RETM    TO WRK-OUT-DSBSPR-RETM.
           MOVE WRK-INP-LITAUT-RETM    TO WRK-OUT-LITAUT-RETM
           MOVE WRK-INP-CODAUT-RETM    TO WRK-OUT-CODAUT-RETM

           MOVE WRK-INP-DESAUT-RETM    TO WRK-OUT-DESAUT-RETM
           MOVE WRK-INP-JAUTOR-RETM    TO WRK-OUT-JAUTOR-RETM
           MOVE WRK-INP-IAUTOR-RETM    TO WRK-OUT-IAUTOR-RETM
           MOVE WRK-INP-TACAUT-RETM    TO WRK-OUT-TACAUT-RETM
           MOVE WRK-INP-TOTAUT-RETM    TO WRK-OUT-TOTAUT-RETM.

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
           MOVE 'DCOM1805'                 TO WRK-COMU-TRANSACAO
           MOVE WRK-INP-SENHAS-RETM        TO WRK-COMU-SENHAS
           MOVE WRK-INP-COMANDO-RETM       TO WRK-COMU-COMANDO

           MOVE WRK-INP-TMSTAMP-RETM       TO WRK-COMU-TIMESTAMP
           MOVE WRK-INP-TIPOOPC-RETM       TO WRK-COMU-OPCAO
           MOVE WRK-INP-TPSEL-RETM         TO WRK-COMU-TP-SELECAO
           MOVE WRK-INP-PAGINA-RETM        TO WRK-COMU-PAGINA-2
           MOVE WRK-INP-PAGINA1-RETM       TO WRK-COMU-PAGINA-1
           MOVE WRK-INP-OPERAC-RETM(1:4)   TO WRK-COMU-DANO-OPER
           MOVE WRK-INP-OPERAC-RETM(5:9)   TO WRK-COMU-NSEQ-OPER
           MOVE 237                        TO WRK-COMU-BANCO
           MOVE WRK-INP-XCONTA-RETM(5:5)   TO WRK-COMU-AGENCIA
           MOVE WRK-INP-XCONTA-RETM(11:13) TO WRK-COMU-CONTA
           MOVE WRK-INP-CGCCPF-RETM(1:3)   TO WRK-COMU-CCNPJ-CPF(1:3)
           MOVE WRK-INP-CGCCPF-RETM(5:3)   TO WRK-COMU-CCNPJ-CPF(4:3)
           MOVE WRK-INP-CGCCPF-RETM(9:3)   TO WRK-COMU-CCNPJ-CPF(7:3)
           MOVE WRK-INP-CGCCPF-RETM(13:4)  TO WRK-COMU-CFLIAL-CNPJ
           MOVE WRK-INP-CGCCPF-RETM(18:2)  TO WRK-COMU-CCTRL-CNPJCPF
           MOVE WRK-INP-CHAMA-RETM         TO WRK-COMU-CHAMADOR
           MOVE WRK-INP-CHAMADO-RETM       TO WRK-TELA
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
           MOVE 'DCOM1805'             TO WRK-TELA
                                          WRK-COMU-TRANSACAO.
           MOVE WRK-INP-SENHAS-RETM    TO WRK-COMU-SENHAS
           MOVE WRK-INP-COMANDO-RETM   TO WRK-COMU-COMANDO
           MOVE '5'                    TO WRK-COMU-PFK
           MOVE WRK-COMU-AREA          TO WRK-MENSAGEM.



      *----------------------------------------------------------------*
       2230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     CONFIRMAR VALORES DA TELA                                  *
      *----------------------------------------------------------------*
       2250-CONFIRMA                   SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-COD-USER               TO 5472-ENT-CFUNC-BDSCO
           MOVE LNK-IO-LTERM               TO 5472-ENT-CTERM

           MOVE WRK-INP-OPERAC-RETM(1:4)   TO 5472-ENT-DANO-OPER-DESC
           MOVE WRK-INP-OPERAC-RETM(5:9)   TO 5472-ENT-NSEQ-OPER-DESC
           MOVE WRK-INP-PARCEL-RETM        TO 5472-ENT-NR-PARCELA

           PERFORM 2251-EXCLUSAO

           .


      *----------------------------------------------------------------*
       2250-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     EXCLUSAO VIA O MODULO DCOM5472                             *
      *----------------------------------------------------------------*
       2251-EXCLUSAO                   SECTION.
      *----------------------------------------------------------------*

           MOVE 'DCOM5472'             TO WRK-MODULO

           CALL WRK-MODULO             USING 5472-ENTRADA
                                             5472-SAIDA
                                             ERRO-AREA
                                             WRK-SQLCA.

           EVALUATE 5472-SAI-COD-RETORNO

             WHEN '0000'
                   MOVE '6'                TO WRK-COMU-PFK

                   MOVE 'EXCLUSAO EFETUADA COM SUCESSO'
                                           TO WRK-COMU-MENSAGEM
                   PERFORM 2220-RETORNAR-CHAMADOR

             WHEN '0099'
                   MOVE 'DB2'         TO ERR-TIPO-ACESSO
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO

             WHEN OTHER
                   MOVE 5472-SAI-MSG-RETORNO
                                      TO WRK-OUT-MENSA-RETM
                   MOVE '*'           TO WRK-INCONSIS

           END-EVALUATE.

      *----------------------------------------------------------------*
       2251-99-FIM.                    EXIT.
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
           MOVE 'DCOM1805'             TO WRK-TELA
                                          WRK-COMU-TRANSACAO
           MOVE WRK-OUT-SENHAS-RETM    TO WRK-COMU-SENHAS
           MOVE WRK-OUT-COMANDO-RETM   TO WRK-COMU-COMANDO
           MOVE 'A'                    TO WRK-COMU-PFK
           MOVE WRK-COMU-AREA          TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2270-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2280-TRATAR-ENTER               SECTION.

      *----------------------------------------------------------------*

           MOVE SPACES                     TO  WRK-INCONSIS

           MOVE 'TECLE <PF6> PARA CONFIRMAR EXCLUSAO OU <PF3> PARA RETOR
      -         'NAR'                      TO  WRK-OUT-MENSA-RETM.

      *----------------------------------------------------------------*
       2280-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2300-PROCESSAR-BRAD0660         SECTION.
      *----------------------------------------------------------------*

           MOVE LENGTH OF WRK-660-DCOMRETM TO WRK-660-LL-AREA-RETM
           MOVE LENGTH OF WRK-OUTPUT-RETM  TO WRK-OUT-LL-RETM
                                              WRK-660-LL-MENSAGEM-RETM

           CALL 'BRAD0660'             USING WRK-OUTPUT-RETM
                                             WRK-660-DCOMRETM.


           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE RETURN-CODE        TO WRK-RETURN-CODE
               MOVE 10                 TO WRK-LOCAL-ERRO
               MOVE WRK-ERRO-BRAD0660  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-ISRT               TO WRK-FUNCAO
           MOVE WRK-OUTPUT-RETM        TO WRK-MENSAGEM.

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
               MOVE 'DCOM1805'         TO ERR-PGM
           ELSE
               MOVE 'SENHAS02'         TO ERR-PGM
               MOVE 'DCOM1805'         TO ERR-MODULO
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
